(ns comportexviz.viz-canvas
  (:require [comportexviz.viz-layouts :as lay
             :refer [layout-bounds
                     n-onscreen
                     top-id-onscreen
                     element-xy
                     fill-element-group
                     fill-elements
                     centred-rect
                     make-layout]]
            [reagent.core :as reagent :refer [atom]]
            [goog.dom :as dom]
            [goog.style :as style]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [goog.dom.ViewportSizeMonitor]
            [monet.canvas :as c]
            [monet.core]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.cells :as cells]
            [clojure.set :as set]
            [cljs.core.async :as async :refer [<!]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-cache]]))

(def model-steps (atom []))
(def viz-layouts (atom {:inputs {}
                        :regions {}}))
(def selection (atom {:region nil
                      :layer nil
                      :dt 0
                      :col nil}))

;;; ## Colours

(defn hsl
  ([h s l] (hsl h s l 1.0))
  ([h s l a]
   (let [h2 (if (keyword? h)
              (case h
                :red 0
                :orange 30
                :yellow 60
                :yellow-green 90
                :green 120
                :blue 210
                :purple 270
                :pink 300)
              ;; otherwise angle
              h)]
     (str "hsla(" h2 ","
          (long (* s 100)) "%,"
          (long (* l 100)) "%,"
          a ")"))))

(defn grey
  [z]
  (let [v (long (* z 255))]
    (str "rgb(" v "," v "," v ")")))

(def state-colors
  {:background "#eee"
   :inactive "white"
   :inactive-syn "black"
   :disconnected "#ddd"
   :active (hsl :red 1.0 0.5)
   :predicted (hsl :blue 1.0 0.5 0.5)
   :active-predicted (hsl :purple 1.0 0.4)
   :highlight (hsl :yellow 1 0.65 0.6)
   :temporal-pooling (hsl :green 1 0.5 0.4)
   })

(def viz-options
  (atom {:input {:active true
                 :predicted true
                 :scroll-counter 0}
         :columns {:active true
                   :overlaps nil
                   :boosts nil
                   :active-freq nil
                   :n-segments nil
                   :predictive true
                   :temporal-pooling true
                   :scroll-counter 0}
         :ff-synapses {:to :selected ;; :selected, :all, :none
                       :active true
                       :inactive nil
                       :disconnected nil
                       :permanences true}
         :distal-synapses {:from :learning ;; :learning, :all, :none
                           :active true
                           :inactive nil
                           :disconnected nil
                           :permanences true}
         :keep-steps 30
         :drawing {:display-mode :one-d ;; :one-d, :two-d
                   :draw-steps 15
                   :height-px nil ;; set on init / resize
                   :width-px nil ;; set on init / resize
                   :top-px 30
                   :bit-w-px 3
                   :bit-h-px 3
                   :bit-shrink 0.85
                   :col-d-px 5
                   :col-shrink 0.85
                   :cell-r-px 10
                   :seg-w-px 30
                   :seg-h-px 10
                   :seg-h-space-px 50
                   :h-space-px 60
                   :highlight-color (:highlight state-colors)}
         }))

(defn draw-image-dt
  [ctx lay dt img]
  (let [[x y] (lay/origin-px-topleft lay dt)]
    (c/draw-image ctx img x y)))

(defn all-layout-paths
  [m]
  (for [k [:inputs :regions]
        subk (if (= k :regions)
               (keys (k m))
               [nil])
        :let [path0 (if subk [k subk] [k])]
        id (keys (get-in m path0))]
    (conj path0 id)))

(defn reset-layout-caches
  [m]
  (reduce (fn [m path]
            (update-in m path vary-meta
                       (fn [mm]
                         (assoc mm ::cache (atom {})))))
          m
          (all-layout-paths m)))

(defn rebuild-layouts
  [model opts]
  (let [inputs (:inputs model)
        regions (:regions model)
        layerseq (mapcat (fn [rgn-id]
                           (map vector (repeat rgn-id)
                                (core/layers (regions rgn-id))))
                         (core/region-keys model))
        d-opts (:drawing opts)
        display-mode (:display-mode d-opts)
        spacer (:h-space-px d-opts)
        height-px (:height-px d-opts)
        top-px (:top-px d-opts)
        ;; for now draw inputs and layers in a horizontal stack
        [i-lays i-right]
        (reduce (fn [[lays left] inp-id]
                  (let [topo (p/topology (inputs inp-id))
                        lay (make-layout topo top-px left height-px d-opts
                                         true display-mode)]
                    [(assoc lays inp-id lay)
                     (+ (lay/right-px lay) spacer)]))
                [{} 0]
                (core/input-keys model))
        [r-lays r-right]
        (reduce (fn [[lays left] [rgn-id lyr-id]]
                  (let [topo (p/topology (get-in regions [rgn-id lyr-id]))
                        lay (make-layout topo top-px left height-px d-opts
                                         false display-mode)]
                    [(assoc-in lays [rgn-id lyr-id] lay)
                     (+ (lay/right-px lay) spacer)]))
                [{} i-right]
                layerseq)]
    (reset-layout-caches
     {:inputs i-lays
      :regions r-lays})))

(add-watch viz-options :rebuild-layouts
           (fn [_ _ old-opts opts]
             (when (not= (:drawing opts)
                         (:drawing old-opts))
               (reset! viz-layouts (rebuild-layouts (first @model-steps) opts)))))

(defn update-dt-offsets!
  []
  (swap! viz-layouts
         (fn [m]
           (let [sel-dt (:dt @selection)
                 draw-steps (get-in @viz-options [:drawing :draw-steps])
                 dt0 (max 0 (- sel-dt (quot draw-steps 2)))]
             (-> (reduce (fn [m path]
                           (update-in m path
                                      (fn [lay] (assoc-in lay [:dt-offset] dt0))))
                         m
                         (all-layout-paths m))
                 (reset-layout-caches))))))

(add-watch selection :update-dt-offsets
           (fn [_ _ _ _]
             (update-dt-offsets!)))

(defn scroll-layout
  [lay down?]
  (let [n (n-onscreen lay)
        ncol (p/size-of lay)]
    (update-in lay [:scroll-top]
               (fn [x]
                 (if down?
                   (-> (+ x n) (min (- ncol n)))
                   (-> (- x n) (max 0)))))))

(defn scroll!
  [down?]
  (swap! viz-layouts
         (fn [m]
           (reduce (fn [m path]
                     (update-in m path scroll-layout down?))
                   m
                   (all-layout-paths m))))
  ;; need this to invalidate the drawing cache
  (swap! viz-options
         (fn [m]
           (-> m
               (update-in [:columns :scroll-counter]
                          #(if down? (inc %) (dec %)))
               (update-in [:input :scroll-counter]
                          #(if down? (inc %) (dec %)))))))

(defn draw-ff-synapses
  [ctx state r-lays i-lays selection opts]
  (c/save ctx)
  (c/stroke-width ctx 1)
  (c/alpha ctx 1)
  (let [{dt :dt, sel-rgn :region, sel-lyr :layer, sel-col :col} selection
        do-inactive? (get-in opts [:ff-synapses :inactive])
        do-disconn? (get-in opts [:ff-synapses :disconnected])
        do-perm? (get-in opts [:ff-synapses :permanences])
        syn-states (concat (when do-disconn? [:disconnected])
                           (when do-inactive? [:inactive-syn])
                           [:active :active-predicted])
        regions (:regions state)
        ;; need to know which layers have input across regions
        input-layer? (into #{} (map (fn [[rgn-id rgn]]
                                      [rgn-id (first (core/layers rgn))])
                                    regions))
        ;; need to know the output layer of each region
        output-layer (into {} (map (fn [[rgn-id rgn]]
                                     [rgn-id (last (core/layers rgn))])
                                   regions))
        this-rgn (get regions sel-rgn)
        this-lyr (get this-rgn sel-lyr)
        to-cols (case (get-in opts [:ff-synapses :to])
                    :all (p/active-columns this-lyr)
                    :selected [sel-col])
        this-paths (map #(vector sel-rgn sel-lyr %) to-cols)]
    ;; trace ff connections downwards
    (loop [path (first this-paths)
           more (rest this-paths)
           done #{}]
      (if (and path (not (done path)))
        (let [[rgn-id lyr-id col] path
              lyr (get-in regions [rgn-id lyr-id])
              in-bits (:in-ff-bits (:state lyr))
              in-sbits (:in-signal-ff-bits (:state lyr))
              sg (:proximal-sg lyr)
              all-syns (p/in-synapses sg col)
              syns (select-keys all-syns (p/sources-connected-to sg col))
              this-lay (get-in r-lays [rgn-id lyr-id])
              [this-x this-y] (element-xy this-lay col dt)]
          (recur
           (first more)
           (into (next more)
                 (for [syn-state syn-states
                       :let [sub-syns (case syn-state
                                        :active (select-keys syns in-bits)
                                        :active-predicted (select-keys syns in-sbits)
                                        :inactive-syn (apply dissoc syns in-bits)
                                        :disconnected (apply dissoc all-syns (keys syns)))
                             _ (c/stroke-style ctx (state-colors syn-state))]
                       [i perm] sub-syns]
                   (let [[src-id src-lyr src-i]
                         (if (input-layer? [rgn-id lyr-id])
                           ;; input from another region
                           (let [[src-id src-i]
                                 (core/source-of-incoming-bit state rgn-id i)]
                             [src-id (output-layer src-id) src-i])
                           ;; input from another layer in same region (hardcoded)
                           [rgn-id :layer-4 i])
                         src-lay (or (get i-lays src-id)
                                     (get-in r-lays [src-id src-lyr]))
                         src-col (if src-lyr
                                   (first (p/source-of-bit
                                           (get-in regions [src-id src-lyr])
                                           src-i))
                                   src-i)
                         [src-x src-y] (element-xy src-lay src-col dt)]
                     (doto ctx
                       (c/alpha (if do-perm? perm 1))
                       (c/begin-path)
                       (c/move-to (- this-x 1) this-y) ;; -1 avoid obscuring colour
                       (c/line-to (+ src-x 1) src-y)
                       (c/stroke))
                     (when src-lyr
                       ;; source is a cell not an input bit, so continue tracing
                       [src-id src-lyr src-col]))))
           (conj done path)))
        ;; go on to next
        (when (seq more)
          (recur (first more) (next more) done)))))
  (c/restore ctx)
  ctx)

(defn group-synapses
  [syns ac pcon]
  (group-by (fn [[id p]]
                   [(if (>= p pcon)
                      :connected :disconnected)
                    (if (ac id)
                      :active :inactive)])
                 syns))

(defn natural-curve
  [ctx x0 y0 x1 y1]
  (let [x-third (/ (- x1 x0) 3)]
    (c/bezier-curve-to ctx
                       (- x1 x-third) y0
                       (+ x0 x-third) y1
                       x1 y1)))

(defprotocol PCellsSegmentsLayout
  (seg-xy [this ci si])
  (cell-xy [this ci])
  (col-cell-line [this ctx ci])
  (cell-seg-line [this ctx ci si]))

(defn all-cell-segments
  [col depth distal-sg]
  (let [cell-ids (map vector (repeat col) (range depth))]
    (mapv (fn [cell-id]
            (->> (p/cell-segments distal-sg cell-id)
                 (reverse)
                 (drop-while empty?)
                 (reverse)))
          cell-ids)))

(defn cells-segments-layout
  [col segs-by-cell cols-lay dt cells-left opts]
  (let [nsegbycell (map count segs-by-cell)
        nsegbycell-pad (map (partial max 1) nsegbycell)
        nseg-pad (apply + nsegbycell-pad)
        d-opts (:drawing opts)
        segs-left (+ cells-left (:seg-h-space-px d-opts))
        col-d-px (:col-d-px d-opts)
        col-r-px (* col-d-px 0.5)
        cell-r-px (:cell-r-px d-opts)
        seg-h-px (:seg-h-px d-opts)
        seg-w-px (:seg-w-px d-opts)
        seg-r-px (* seg-w-px 0.5)
        our-height (:height-px d-opts)
        our-top (+ (:top-px d-opts) cell-r-px)
        [col-x col-y] (element-xy cols-lay col dt)]
    (reify PCellsSegmentsLayout
      (seg-xy
        [_ ci si]
        (let [i-all (apply + si (take ci nsegbycell-pad))
              frac (/ i-all nseg-pad)]
          [(+ segs-left seg-r-px)
           (+ our-top (* frac our-height))]))
      (cell-xy
        [this ci]
        (let [[_ sy] (seg-xy this ci 0)]
          [cells-left sy]))
      (col-cell-line
        [this ctx ci]
        (let [[cell-x cell-y] (cell-xy this ci)]
          (doto ctx
            (c/begin-path)
            (c/move-to (+ col-x col-r-px 1) col-y) ;; avoid obscuring colour
            (natural-curve col-x col-y cell-x cell-y)
            (c/stroke))))
      (cell-seg-line
        [this ctx ci si]
        (let [[cell-x cell-y] (cell-xy this ci)
              [sx sy] (seg-xy this ci si)]
          (doto ctx
            (c/begin-path)
            (c/move-to sx sy)
            (c/line-to (+ cell-x cell-r-px) cell-y)
            (c/stroke)))))))

(defn draw-cell-segments
  [ctx state r-lays i-lays selection opts cells-left]
  (c/save ctx)
  (let [{dt :dt, sel-rgn :region, sel-lyr :layer, col :col} selection
        regions (:regions state)
        lyr (get-in regions [sel-rgn sel-lyr])
        lay (get-in r-lays [sel-rgn sel-lyr])
        spec (p/params lyr)
        threshold (:seg-stimulus-threshold spec)
        pcon (:distal-perm-connected spec)
        ac (p/active-cells lyr)
        prev-ac (:active-cells (:prior-state lyr))
        prev-pc (:pred-cells (:prior-distal-state lyr))
        prev-aci (:distal-bits (:prior-distal-state lyr))
        learning (:learn-segments (:state lyr))
        active? (get (p/active-columns lyr) col)
        bursting? (get (p/bursting-columns lyr) col)
        distal-sg (:distal-sg lyr)
        segs-by-cell (all-cell-segments col (p/layer-depth lyr) distal-sg)
        cslay (cells-segments-layout col segs-by-cell lay dt cells-left opts)
        col-d-px (get-in opts [:drawing :col-d-px])
        cell-r-px (get-in opts [:drawing :cell-r-px])
        seg-h-px (get-in opts [:drawing :seg-h-px])
        seg-w-px (get-in opts [:drawing :seg-w-px])
        seg-r-px (* seg-w-px 0.5)]
    ;; draw background lines to cell from column and from segments
    (c/stroke-width ctx col-d-px)
    (c/stroke-style ctx (:background state-colors))
    (doseq [[ci segs] (map-indexed vector segs-by-cell)]
      (col-cell-line cslay ctx ci)
      (doseq [si (range (count segs))]
        (cell-seg-line cslay ctx ci si)))
    ;; draw each cell
    (doseq [[ci segs] (map-indexed vector segs-by-cell)
            :let [[cell-x cell-y] (cell-xy cslay ci)
                  cell-id [col ci]
                  cell-active? (ac cell-id)
                  cell-predictive? (prev-pc cell-id)
                  learn-cell? (find learning cell-id)
                  learn-seg-idx (when learn-cell? (val learn-cell?))
                  cell-state (cond
                              (and cell-active? cell-predictive?) :active-predicted
                              cell-predictive? :predicted
                              cell-active? :active
                              :else :inactive)]]
      (when cell-active?
        (doto ctx
          (c/stroke-style (:active state-colors))
          (c/stroke-width 2))
        (col-cell-line cslay ctx ci))
      ;; draw the cell itself
      (when learn-cell?
        (doto ctx
          (c/fill-style (:highlight state-colors))
          (c/circle {:x cell-x :y cell-y :r (+ cell-r-px 8)})
          (c/fill)))
      (doto ctx
        (c/fill-style (state-colors cell-state))
        (c/stroke-style "black")
        (c/stroke-width 1)
        (c/circle {:x cell-x :y cell-y :r cell-r-px})
        (c/stroke)
        (c/fill))
      (c/fill-style ctx "black")
      (c/text ctx {:text (str "cell " ci
                              (when learn-cell?
                                (str "   (learning on "
                                     (if learn-seg-idx
                                       (str "segment " learn-seg-idx)
                                       "new segment")
                                     ")")))
                   :x cell-x :y (- cell-y cell-r-px 5)})
      ;; draw each segment
      (doseq [[si seg] (map-indexed vector segs)
              :let [[sx sy] (seg-xy cslay ci si)
                    grouped-syns (group-synapses seg prev-aci pcon)
                    conn-act (count (grouped-syns [:connected :active]))
                    conn-tot (+ (count (grouped-syns [:connected :inactive]))
                                conn-act)
                    disc-act (count (grouped-syns [:disconnected :active]))
                    disc-tot (+ (count (grouped-syns [:disconnected :inactive]))
                                disc-act)
                    z (-> (/ conn-act threshold)
                          (min 1.0))
                    learn-seg? (and learn-cell? (= si learn-seg-idx))]]
        ;; draw segment as a rectangle
        (let [s (centred-rect sx sy seg-w-px seg-h-px)
              hs (centred-rect sx sy (+ seg-w-px 8) (+ seg-h-px 8))]
          (when learn-seg?
            (doto ctx
              (c/fill-style (:highlight state-colors))
              (c/fill-rect hs)))
          (doto ctx
            (c/alpha 1.0)
            (c/stroke-style "black")
            (c/stroke-width 1)
            (c/stroke-rect s)
            (c/fill-style "white")
            (c/fill-rect s)
            (c/alpha z)
            (c/fill-style (:active state-colors))
            (c/fill-rect s)
            (c/alpha 1.0)))
        (when (>= conn-act threshold)
          (doto ctx
            (c/stroke-style (:active state-colors))
            (c/stroke-width 2))
          (cell-seg-line cslay ctx ci si))
        (c/fill-style ctx "black")
        (c/text ctx {:text (str "[" si "],  active " conn-act
                                " / " conn-tot " conn."
                                " (" disc-act " / " disc-tot " disconn.)")
                     :x (+ sx 5 seg-r-px) :y sy})
        ;; draw distal synapses
        (c/stroke-width ctx 1)
        (let [do-perm? (get-in opts [:distal-synapses :permanences])
              do-act? (get-in opts [:distal-synapses :active])
              do-ina? (get-in opts [:distal-synapses :inactive])
              do-disc? (get-in opts [:distal-synapses :disconnected])
              do-from (get-in opts [:distal-synapses :from])
              grouped-sourced-syns
              (util/remap (fn [syns]
                            (map (fn [[i p]]
                                   [(core/source-of-distal-bit state sel-rgn sel-lyr i)
                                    p])
                                 syns))
                          grouped-syns)]
          (when (or (= do-from :all)
                    (and (= do-from :learning) learn-seg?))
            (doseq [syn-state (concat (when do-ina? [:inactive])
                                      (when do-act? [:active]))
                    syn-conn (concat (when do-disc? [:disconnected])
                                     [:connected])
                    :let [sourced-syns (grouped-sourced-syns [syn-conn syn-state])]]
              (c/stroke-style ctx (state-colors syn-state))
              (doseq [[[src-id src-lyr src-i] p] sourced-syns
                      :let [src-lay (or (get i-lays src-id)
                                        (get-in r-lays [src-id src-lyr]))
                            src-col (if src-lyr
                                      (first (p/source-of-bit
                                              (get-in regions [src-id src-lyr])
                                              src-i))
                                      src-i)
                            [src-x src-y] (element-xy src-lay src-col (inc dt))]]
                (when do-perm? (c/alpha ctx p))
                (doto ctx
                  (c/begin-path)
                  (c/move-to sx sy)
                  (c/line-to (+ src-x 1) src-y) ;; +1 avoid obscuring colour
                  (c/stroke))))))))
    (c/restore ctx))
  ctx)

(defn image-buffer
  [{:keys [w h]}]
  (let [el (dom/createElement "canvas")]
    (set! (.-width el) w)
    (set! (.-height el) h)
    el))

(defn bg-image
  [lay]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        j (top-id-onscreen lay)]
    (c/fill-style ctx (:background state-colors))
    (fill-element-group ctx lay (range j (+ j (n-onscreen lay))))
    el))

(defn active-bits-image
  [lay inp]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        inbits (if (:encoder inp)
                 (p/bits-value inp)
                 (p/motor-bits-value inp))]
    (c/fill-style ctx (:active state-colors))
    (fill-element-group ctx lay inbits)
    el))

(defn pred-bits-image
  [lay prev-rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        bit-votes (core/predicted-bit-votes prev-rgn)
        bit-alpha (util/remap #(min 1.0 (/ % 8)) bit-votes)]
    (c/fill-style ctx (:predicted state-colors))
    (fill-elements ctx lay bit-alpha c/alpha)
    el))

(defn active-columns-image
  [lay lyr]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        cols (p/active-columns lyr)]
    (c/fill-style ctx (:active state-colors))
    (fill-element-group ctx lay cols)
    el))

(defn pred-columns-image
  [lay lyr]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        cols (->> (p/prior-predictive-cells lyr)
                  (map first)
                  (distinct))]
    (c/fill-style ctx (:predicted state-colors))
    (fill-element-group ctx lay cols)
    el))

(defn tp-columns-image
  [lay lyr]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        cols (->> (p/temporal-pooling-cells lyr)
                  (map first))]
    (c/fill-style ctx (:temporal-pooling state-colors))
    (fill-element-group ctx lay cols)
    el))

(defn overlaps-columns-image
  [lay lyr]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        col-m (->> (p/column-excitation lyr)
                   (util/remap #(min 1.0 (/ % 16))))]
    (c/fill-style ctx "black")
    (fill-elements ctx lay col-m c/alpha)
    el))

(defn boost-columns-image
  [lay lyr]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        spec (p/params lyr)
        maxb (:max-boost spec)
        col-m (->> (:boosts lyr)
                   (map #(/ (dec %) (dec maxb)))
                   (zipmap (range)))]
    (c/fill-style ctx "black")
    (fill-elements ctx lay col-m c/alpha)
    el))

(defn active-freq-columns-image
  [lay lyr]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        spec (p/params lyr)
        col-m (->> (:active-duty-cycles lyr)
                   (map #(min 1.0 (* 2 %)))
                   (zipmap (range)))]
    (c/fill-style ctx "black")
    (fill-elements ctx lay col-m c/alpha)
    el))

(defn count-segs-in-column
  [distal-sg depth col]
  (reduce (fn [n ci]
            (+ n (util/count-filter seq
                                    (p/cell-segments distal-sg [col ci]))))
          0
          (range depth)))

(defn n-segments-columns-image
  [lay lyr]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        sg (:distal-sg lyr)
        n-cols (p/size-of lyr)
        depth (p/layer-depth lyr)
        n-start (top-id-onscreen lay)
        cols (range n-start (+ n-start (n-onscreen lay)))
        col-m (->> cols
                   (map #(count-segs-in-column sg depth %))
                   (map #(min 1.0 (/ % 16.0)))
                   (zipmap cols))]
    (c/fill-style ctx "black")
    (fill-elements ctx lay col-m c/alpha)
    el))

(defn scroll-status-str
  [lay]
  (str (top-id-onscreen lay)
       "--" (+ (top-id-onscreen lay)
               (n-onscreen lay) -1)
       " of " (p/size-of lay)))

(defn draw-timeline!
  [ctx steps sel-dt opts]
  (let [current-t (p/timestep (first steps))
        keep-steps (:keep-steps opts)
        width-px (.-width (.-canvas ctx))
        height-px (.-height (.-canvas ctx))
        t-width (/ width-px keep-steps)
        y-px (/ height-px 2)
        r-px (min y-px (* t-width 0.5))
        sel-r-px y-px]
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    (c/text-align ctx :center)
    (c/text-baseline ctx :middle)
    (c/font-style ctx "bold 10px sans-serif")
    (doseq [dt (reverse (range keep-steps))
            :let [t (- current-t dt)
                  kept? (< dt (count steps))
                  x-px (- (dec width-px) r-px (* dt t-width))]]
      (c/fill-style ctx "black")
      (c/alpha ctx (cond (== dt sel-dt) 1.0 kept? 0.3 :else 0.1))
      (c/circle ctx {:x x-px :y y-px :r (if (== dt sel-dt) sel-r-px r-px)})
      (c/fill ctx)
      (when (or (== dt sel-dt)
                (and kept? (< keep-steps 100)))
        (c/fill-style ctx "white")
        (c/text ctx {:x x-px :y y-px :text (str t)})))
    (c/alpha ctx 1.0)))

(defn timeline-click
  [e*]
  (let [e (.-nativeEvent e*)
        x (.-offsetX e)
        steps @model-steps
        opts @viz-options
        keep-steps (:keep-steps opts)
        width-px (.-width (.-target e))
        t-width (/ width-px keep-steps)
        click-dt (quot (- (dec width-px) x) t-width)]
    (when (< click-dt (count @model-steps))
      (swap! selection assoc :dt click-dt))
    ))

(defn draw-viz!
  [ctx steps layouts sel opts]
  (let [{sel-dt :dt
         sel-rgn :region
         sel-lyr :layer
         sel-col :col} sel
        i-lays (:inputs layouts)
        r-lays (:regions layouts)
        draw-steps (get-in opts [:drawing :draw-steps])
        ;; in case scrolled back in history
        dt0 (max 0 (- sel-dt (quot draw-steps 2)))
        sel-state (nth steps sel-dt)
        sel-prev-state (nth steps (inc sel-dt) nil)
        cells-left (->> (mapcat vals (vals r-lays))
                        (map lay/right-px)
                        (apply max)
                        (+ (get-in opts [:drawing :h-space-px])))
        label-top-px 0
        top-px (get-in opts [:drawing :top-px])
        width-px (.-width (.-canvas ctx))
        height-px (.-height (.-canvas ctx))]
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    ;; draw labels
    (c/text-align ctx :start)
    (c/text-baseline ctx :top)
    (c/font-style ctx "10px sans-serif")
    (c/fill-style ctx "black")
    (doseq [[inp-id lay] i-lays]
      (c/text ctx {:text (str (name inp-id) " encoded bits.")
                   :x (:x (layout-bounds lay))
                   :y label-top-px})
      (c/text ctx {:text (scroll-status-str lay)
                   :x (:x (layout-bounds lay))
                   :y (+ label-top-px 10)}))
    (doseq [[rgn-id lyr-lays] r-lays
            [lyr-id lay] lyr-lays]
      (c/text ctx {:text (str (name rgn-id) " " (name lyr-id) " columns.")
                   :x (:x (layout-bounds lay))
                   :y label-top-px})
      (c/text ctx {:text (scroll-status-str lay)
                   :x (:x (layout-bounds lay))
                   :y (+ label-top-px 10)}))
    (c/text ctx {:text "Cells and distal dendrite segments."
                 :x cells-left :y label-top-px})
    (doseq [dt (range dt0 (min (+ dt0 draw-steps)
                               (count steps)))
            :let [state (nth steps dt)
                  prev-state (nth steps (inc dt) nil)
                  dt-cache (::cache (meta state))]]
      ;; draw encoded inbits
      (doseq [[inp-id lay] i-lays
              :when (or (== 1 (count (p/dims-of lay)))
                        (== dt sel-dt))
              :let [inp (get-in state [:inputs inp-id])
                    ;; region this input feeds to, for predictions
                    ff-rgn-id (first (get-in state [:fb-deps inp-id]))
                    ;; TODO offset if multiple inputs feeding to region
                    prev-ff-rgn (when (pos? (p/size (p/ff-topology inp)))
                                  (get-in prev-state [:regions ff-rgn-id]))
                    lay-cache (::cache (meta lay))]]
        (->> (bg-image lay)
             (with-cache lay-cache [::bg inp-id] opts #{:drawing})
             (draw-image-dt ctx lay dt))
        (when (get-in opts [:input :active])
          (->> (active-bits-image lay inp)
               (with-cache dt-cache [::abits inp-id] opts #{:input :drawing})
               (draw-image-dt ctx lay dt)))
        (when (and (get-in opts [:input :predicted])
                   prev-ff-rgn)
          (->> (pred-bits-image lay prev-ff-rgn)
               (with-cache dt-cache [::pbits inp-id] opts #{:input :drawing})
               (draw-image-dt ctx lay dt))))
      ;; draw regions / layers
      (doseq [[rgn-id lyr-lays] r-lays
              [lyr-id lay] lyr-lays
              :when (or (== 1 (count (p/dims-of lay)))
                        (== dt sel-dt))
              :let [lyr (get-in state [:regions rgn-id lyr-id])
                    uniqix (str (name rgn-id) (name lyr-id))
                    lay-cache (::cache (meta lay))]]
        (->> (bg-image lay)
             (with-cache lay-cache [::bg uniqix] opts #{:drawing})
             (draw-image-dt ctx lay dt))
        (when (get-in opts [:columns :overlaps])
          (->> (overlaps-columns-image lay lyr)
               (with-cache dt-cache [::ocols uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when (get-in opts [:columns :boosts])
          (->> (boost-columns-image lay lyr)
               (with-cache dt-cache [::boosts uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when (get-in opts [:columns :active-freq])
          (->> (active-freq-columns-image lay lyr)
               (with-cache dt-cache [::afreq uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when (get-in opts [:columns :n-segments])
          (->> (n-segments-columns-image lay lyr)
               (with-cache dt-cache [::nsegcols uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when (get-in opts [:columns :active])
          (->> (active-columns-image lay lyr)
               (with-cache dt-cache [::acols uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when (get-in opts [:columns :predictive])
          (->> (pred-columns-image lay lyr)
               (with-cache dt-cache [::pcols uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when (get-in opts [:columns :temporal-pooling])
          (->> (tp-columns-image lay lyr)
               (with-cache dt-cache [::tpcols uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))))
    ;; highlight selection
    (doseq [lay (vals i-lays)]
      (lay/highlight-dt lay ctx sel-dt))
    (doseq [lay (mapcat vals (vals r-lays))]
      (lay/highlight-dt lay ctx sel-dt))
    (when sel-col
      (let [lay (get-in r-lays [sel-rgn sel-lyr])]
        (lay/highlight-element lay ctx sel-dt sel-col)))
    ;; draw ff synapses
    (let [to (get-in opts [:ff-synapses :to])]
      (when (or (= to :all)
                (and (= to :selected )
                     sel-col))
        (draw-ff-synapses ctx sel-state r-lays i-lays sel opts)))
    ;; draw selected cells and segments
    (when sel-col
      (draw-cell-segments ctx sel-state r-lays i-lays sel opts cells-left)))
  nil)

(defn set-canvas-pixels-from-element-size!
  [el min-px-width]
  (let [size-px (style/getSize el)
        width-px (-> (.-width size-px)
                     (max min-px-width))
        height-px (.-height size-px)]
    (set! (.-width el) width-px)
    (set! (.-height el) height-px)))

(defn on-resize
  [_]
  (let [viz-el (dom/getElement "comportex-viz")
        tl-el (dom/getElement "comportex-timeline")
        size-px (style/getSize viz-el)
        width-px (.-width size-px)
        height-px (.-height size-px)]
    (set-canvas-pixels-from-element-size! viz-el 300)
    (set-canvas-pixels-from-element-size! tl-el 300)
    (swap! viz-options (fn [opts]
                         (-> opts
                             (assoc-in [:drawing :height-px] height-px)
                             (assoc-in [:drawing :width-px] width-px))))))

(defn selected-model-state
  []
  (nth @model-steps (:dt @selection) nil))

(defn draw!
  []
  (if (nil? (get-in @viz-options [:drawing :height-px]))
    (on-resize nil) ;; triggers another redraw
    (when (seq @model-steps)
      (let [viz-el (dom/getElement "comportex-viz")
            viz-ctx (c/get-context viz-el "2d")
            tl-el (dom/getElement "comportex-timeline")
            tl-ctx (c/get-context tl-el "2d")]
        (draw-viz! viz-ctx
                   @model-steps
                   @viz-layouts
                   @selection
                   @viz-options)
        (draw-timeline! tl-ctx
                        @model-steps
                        (:dt @selection)
                        @viz-options)))))

(defn step-forward!
  [sim-step!]
  (if (zero? (:dt @selection))
    (sim-step!)
    (swap! selection update-in [:dt]
           (fn [x] (max (dec x) 0)))))

(defn step-backward!
  []
  (let [;; we need to assume there is a previous step, so:
        max-dt (max 0 (- (count @model-steps) 2))]
    (swap! selection update-in [:dt]
          (fn [x] (min (inc x) max-dt)))))

(defn viz-click
  [e*]
  (let [e (.-nativeEvent e*)
        x (.-offsetX e)
        y (.-offsetY e)
        i-lays (:inputs @viz-layouts)
        r-lays (:regions @viz-layouts)
        ;; we need to assume there is a previous step, so:
        max-dt (max 0 (- (count @model-steps) 2))
        hit? (atom false)]
    ;; check inputs
    (doseq [[k lay] i-lays
            :let [[dt id] (lay/clicked-id lay x y)]
            :when dt]
      (reset! hit? true)
      (when (== 1 (count (p/dims-of lay)))
        (swap! selection assoc :dt (min dt max-dt))))
    ;; check regions
    (doseq [[rgn-id lyr-lays] r-lays
            [lyr-id lay] lyr-lays
            :let [[dt col] (lay/clicked-id lay x y)]
            :when dt]
      (reset! hit? true)
      (if (== 1 (count (p/dims-of lay)))
        (swap! selection assoc :region rgn-id :layer lyr-id :col col
               :dt (min dt max-dt))
        (swap! selection assoc :region rgn-id :layer lyr-id :col col)))
    (when-not @hit?
      ;; checked all, nothing clicked
      (swap! selection assoc :col nil))))

(defn oh-look-the-model-changed!
  [htm]
  (reset! viz-layouts (rebuild-layouts htm @viz-options))
  (let [region-key (first (core/region-keys htm))
        layer-id (first (core/layers (get-in htm [:regions region-key])))]
    (swap! selection assoc :region region-key :layer layer-id
           :dt 0 :col nil)))

(defn init!
  [steps-c]
  (events/listen (goog.dom.ViewportSizeMonitor.) event-type/RESIZE
                 on-resize)
  ;; stream the simulation steps into the sliding history buffer
  (go (loop []
        (when-let [x* (<! steps-c)]
          (let [x (vary-meta x* assoc ::cache (atom {}))
                keep-steps (:keep-steps @viz-options)]
            (swap! model-steps (fn [xs]
                                 (take keep-steps (cons x xs)))))
          (recur)))))
