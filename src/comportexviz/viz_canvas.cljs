(ns comportexviz.viz-canvas
  (:require [comportexviz.viz-layouts :as lay
             :refer [layout-bounds
                     n-onscreen
                     top-id-onscreen
                     element-xy
                     fill-element-group
                     fill-elements
                     circle
                     centred-rect
                     make-layout]]
            [c2.dom :as dom :refer [->dom]]
            [c2.event]
            [goog.events.EventType]
            [goog.events :as gevents]
            [goog.string :as gstring]
            [goog.string.format]
            [monet.canvas :as c]
            [monet.core]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.cells :as cells]
            [clojure.set :as set]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-cache]]))

(def height-px 900)
(def top-px 30)

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
  {:background "white"
   :inactive "white"
   :inactive-syn "black"
   :disconnected "white"
   :active (hsl :red 1.0 0.5)
   :predicted (hsl :blue 1.0 0.5 0.5)
   :active-predicted (hsl :purple 1.0 0.4)
   :highlight (hsl :yellow 1 0.75 0.5)
   :temporal-pooling (hsl :green 1 0.5 0.4)
   })

(def viz-options
  (atom {:input {:active true
                 :predicted true
                 :scroll-counter 0}
         :columns {:active true
                   :overlaps nil
                   :n-segments nil
                   :predictive true
                   :temporal-pooling true
                   :scroll-counter 0}
         :ff-synapses {:active nil
                       :inactive nil
                       :disconnected nil
                       :permanences nil}
         :lat-synapses {:from :learning ;; :learning, :all, :none
                        :active true
                        :inactive nil
                        :disconnected nil
                        :permanences nil}
         :drawing {:draw-steps 25
                   :world-w-px 150
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

(def keep-steps (atom 25))
(def steps (atom []))
(def layouts (atom {:inputs {}
                    :regions {}}))

(defn draw-image-dt
  [ctx lay dt img]
  (let [[x y] (lay/origin-px-topleft lay dt)]
    (c/draw-image ctx img x y)))

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
  (swap! layouts
         (fn [m]
           (-> m
               (update-in [:regions]
                          (fn [lays]
                            (util/remap #(scroll-layout % down?) lays)))
               (update-in [:inputs]
                          (fn [lays]
                            (util/remap #(scroll-layout % down?) lays))))))
  ;; need this to invalidate the drawing cache
  (swap! viz-options
         (fn [m]
           (-> m
               (update-in [:columns :scroll-counter]
                          #(if down? (inc %) (dec %)))
               (update-in [:input :scroll-counter]
                          #(if down? (inc %) (dec %)))))))

(defn draw-ff-synapses
  [ctx region-key state r-lays i-lays sel-col sel-dt opts]
  (c/save ctx)
  (c/stroke-width ctx 1)
  (c/alpha ctx 1)
  (let [rgn (get-in state [:regions region-key])
        do-inactive? (get-in opts [:ff-synapses :inactive])
        do-disconn? (get-in opts [:ff-synapses :disconnected])
        do-perm? (get-in opts [:ff-synapses :permanences])
        this-lay (r-lays region-key)
        layer-ids (core/layers rgn)]
    (doseq [[layer-idx layer-id] (map-indexed vector layer-ids)
            :let [lyr (get rgn layer-id)]
            col (if sel-col [sel-col]
                    (p/active-columns lyr))
            syn-state (concat (when do-disconn? [:disconnected])
                              (when do-inactive? [:inactive-syn])
                              [:active :active-predicted])
            :let [in-bits (:in-ff-bits (:state lyr))
                  in-sbits (:in-signal-ff-bits (:state lyr))
                  sg (:proximal-sg lyr)
                  all-syns (p/in-synapses sg col)
                  syns (select-keys all-syns (p/sources-connected-to sg col))
                  sub-syns (case syn-state
                             :active (select-keys syns in-bits)
                             :active-predicted (select-keys syns in-sbits)
                             :inactive-syn (apply dissoc syns in-bits)
                             :disconnected (apply dissoc all-syns (keys syns)))
                  [this-x this-y] (element-xy this-lay col sel-dt)]]
      (c/stroke-style ctx (state-colors syn-state))
      (doseq [[i perm] sub-syns
              :let [[src-key src-i]
                    (if (zero? layer-idx)
                      ;; input from another region
                      (core/source-of-incoming-bit state region-key i)
                      ;; input from another layer in same region
                      ;; TODO layer layouts (nth layer-ids (dec layer-idx))
                      [region-key i])
                    src-lay (or (r-lays src-key)
                                (i-lays src-key))
                    [src-x src-y] (element-xy src-lay src-i sel-dt)]]
        (doto ctx
          (c/alpha (if do-perm? perm 1))
          (c/begin-path)
          (c/move-to (- this-x 1) this-y) ;; -1 avoid obscuring colour
          (c/line-to (+ src-x 1) src-y)
          (c/stroke)))))
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
        segs-left (+ cells-left (get-in opts [:drawing :seg-h-space-px]))
        col-d-px (get-in opts [:drawing :col-d-px])
        col-r-px (* col-d-px 0.5)
        cell-r-px (get-in opts [:drawing :cell-r-px])
        seg-h-px (get-in opts [:drawing :seg-h-px])
        seg-w-px (get-in opts [:drawing :seg-w-px])
        seg-r-px (* seg-w-px 0.5)
        our-height (* 0.95 (.-innerHeight js/window))
        our-top (+ (.-pageYOffset js/window) (* 2 cell-r-px))
        [col-x col-y] (element-xy cols-lay col dt)]
    (reify PCellsSegmentsLayout
      (seg-xy
        [_ ci si]
        (let [i-all (apply + si (take ci nsegbycell-pad))
              frac (/ i-all nseg-pad)]
          [(+ segs-left seg-r-px)
           (+ our-top top-px (* frac our-height))]))
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
  [ctx layer lay col dt cells-left opts]
  (c/save ctx)
  (let [spec (p/params layer)
        threshold (:seg-stimulus-threshold spec)
        pcon (:distal-perm-connected spec)
        ac (p/active-cells layer)
        prev-ac (:active-cells (:prior-state layer))
        prev-pc (:pred-cells (:prior-distal-state layer))
        prev-aci (:distal-bits (:prior-distal-state layer))
        learning (:learn-segments (:state layer))
        active? (get (p/active-columns layer) col)
        bursting? (get (p/bursting-columns layer) col)
        distal-sg (:distal-sg layer)
        segs-by-cell (all-cell-segments col (p/layer-depth layer) distal-sg)
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
          (c/begin-path)
          (circle cell-x cell-y (+ cell-r-px 8))
          (c/fill)))
      (doto ctx
        (c/fill-style (state-colors cell-state))
        (c/stroke-style "black")
        (c/stroke-width 1)
        (c/begin-path)
        (circle cell-x cell-y cell-r-px)
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
        (let [do-perm? (get-in opts [:lat-synapses :permanences])
              do-act? (get-in opts [:lat-synapses :active])
              do-ina? (get-in opts [:lat-synapses :inactive])
              do-disc? (get-in opts [:lat-synapses :disconnected])
              do-from (get-in opts [:lat-synapses :from])
              grouped-sourced-syns (util/remap
                                    (fn [syns]
                                      (map (fn [[id p]]
                                             [(cells/id->source spec id) p])
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
              (doseq [[[source-k source-v] p] sourced-syns
                      :let [[tx ty] (case source-k
                                      :this (let [[to-col] source-v]
                                              (element-xy lay to-col (inc dt)))
                                      :ff [0 height-px]
                                      :fb [1000 height-px])]]
                (when do-perm? (c/alpha ctx p))
                (doto ctx
                  (c/alpha (if do-perm? p 1))
                  (c/begin-path)
                  (c/move-to sx sy)
                  (c/line-to (+ tx 1) ty) ;; +1 avoid obscuring colour
                  (c/stroke)))
              (c/alpha ctx 1))))))
    (c/restore ctx))
  ctx)

(defn detail-text
  [{dt :dt
    region-key :region
    col :col
    :as selection}]
  (let [state (nth @steps dt)
        rgn (get-in state [:regions region-key])
        layer (:layer-3 rgn)
        depth (p/layer-depth layer)
        inp (first (core/input-seq state))
        in (:value inp)
        bits (p/bits-value inp)]
    (->>
     ["__Selection__"
      (str "* timestep " (p/timestep rgn)
           " (delay " dt ")")
      (str "* column " (or col "nil"))
      ""
      "__Input__"
      (str in " (" (count bits) " bits)")
      ""
      "__Input bits__"
      (str (sort bits))
      ""
      "__Active columns__"
      (str (sort (p/active-columns layer)))
      ""
      "__Active cells__"
      (str (sort (p/active-cells layer)))
      ""
      "__Learnable cells__"
      (str (sort (p/learnable-cells layer)))
      ""
      "__Learning segments__"
      (str (sort (:learn-segments (:state layer))))
      ""
      "__Signal cells__"
      (str (sort (p/signal-cells layer)))
      ""
      "__Predicted cells__"
      (str (sort (p/predictive-cells layer)))
      ""
      (if col
        (let [dtp (inc dt)
              p-state (nth @steps dtp)
              p-rgn (get-in p-state [:regions region-key])
              p-layer (:layer-3 p-rgn)
              p-prox-sg (:proximal-sg p-layer)
              p-distal-sg (:distal-sg p-layer)
              ac (p/active-cells p-layer)
              lc (or (p/learnable-cells p-layer) #{})
              pcon (:distal-perm-connected (p/params p-rgn))
              ;; TODO
              bits #{}
              sig-bits #{}
              ]
          ["__Active cells prev__"
           (str (sort ac))
           ""
           "__Learn cells prev__"
           (str (sort lc))
           ""
           "__Distal LC bits prev__"
           (str (:distal-lc-bits (:prior-distal-state layer)))
           ""
           "__Distal LC bits__"
           (str (:distal-lc-bits (:distal-state layer)))
           ""
           "__Distal bits__"
           (str (:distal-bits (:distal-state layer)))
           ""
           "__Predicted cells prev__"
           (str (sort (p/predictive-cells p-layer)))
           ""
           "__Selected column__"
           "__Connected ff-synapses__"
           (let [syns (p/in-synapses p-prox-sg col)]
             (for [[id p] (sort syns)]
               (str "  " id " :=> "
                    (gstring/format "%.2f" p)
                    (if (sig-bits id) " S")
                    (if (bits id) (str " A "
                                       ;(p/source-of-incoming-bit)
                                       )))))
           "__Cells and their Dendrite segments__"
           (for [ci (range (p/layer-depth layer))
                 :let [segs (p/cell-segments p-distal-sg [col ci])]]
             [(str "CELL " ci)
              (str (count segs) " = " (map count segs))
              (str "Lateral excitation from this cell: "
                   (p/targets-connected-from p-distal-sg (+ ci (* depth col)))) ;; TODO cell->id
              (for [[si syns] (map-indexed vector segs)]
                [(str "  SEGMENT " si)
                 (for [[id p] (sort syns)]
                   (str "  " id
                        (if (>= p pcon) " :=> " " :.: ")
                        (gstring/format "%.2f" p)
                        (if (lc id) " L"
                            (if (ac id) " A"))))])
              ])
           ]))
      ""
      "__spec__"
      (map str (sort (p/params rgn)))]
     (flatten)
     (interpose \newline)
     (apply str))))

(defn image-buffer
  [{:keys [w h]}]
  (let [el (->dom [:canvas])]
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
        pcols (->> (p/predictive-cells (:layer-3 prev-rgn))
                   (map first)
                   (distinct)) ;; ?
        bit-votes (core/predicted-bit-votes prev-rgn pcols)
        bit-alpha (util/remap #(min 1.0 (/ % 8)) bit-votes)]
    (c/fill-style ctx (:predicted state-colors))
    (fill-elements ctx lay bit-alpha c/alpha)
    el))

(defn active-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        cols (p/active-columns (:layer-3 rgn))]
    (c/fill-style ctx (:active state-colors))
    (fill-element-group ctx lay cols)
    el))

(defn pred-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        cols (->> (p/prior-predictive-cells (:layer-3 rgn))
                  (map first)
                  (distinct))]
    (c/fill-style ctx (:predicted state-colors))
    (fill-element-group ctx lay cols)
    el))

(defn tp-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        cols (->> (p/temporal-pooling-cells (:layer-3 rgn))
                  (map first))]
    (c/fill-style ctx (:temporal-pooling state-colors))
    (fill-element-group ctx lay cols)
    el))

(defn overlaps-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        col-m (->> (p/column-excitation (:layer-3 rgn))
                   (util/remap #(min 1.0 (/ % 16))))]
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
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        layer (:layer-3 rgn)
        sg (:distal-sg layer)
        n-cols (p/size-of layer)
        depth (p/layer-depth layer)
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
  (str "Showing " (top-id-onscreen lay)
       "--" (+ (top-id-onscreen lay)
               (n-onscreen lay) -1)
       " of " (p/size-of lay)))

(defn do-draw!
  [{sel-dt :dt
    sel-region :region
    sel-col :col
    :as selection}]
  (dom/val "#detail-text"
           (if sel-col (detail-text selection)
               "Select a column (by clicking on it) to see details."))
  (let [opts @viz-options
        i-lays (:inputs @layouts)
        r-lays (:regions @layouts)
        draw-steps (get-in opts [:drawing :draw-steps])
        sel-state (nth @steps sel-dt)
        sel-prev-state (nth @steps (inc sel-dt) nil)
        canvas-el (->dom "#comportex-viz")
        ctx (c/get-context canvas-el "2d")
        i-bgs (util/remap bg-image i-lays)
        r-bgs (util/remap bg-image r-lays)
        cells-left (+ (apply max (map lay/right-px (vals r-lays)))
                      (get-in opts [:drawing :h-space-px]))
        width-px (.-width canvas-el)]
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    (c/text-baseline ctx :top)
    (c/text ctx {:text "Input on selected timestep."
                 :x 2
                 :y 0})
    (doseq [[k lay] i-lays]
      (c/text ctx {:text (str (name k) " encoded bits.")
                   :x (:x (layout-bounds lay))
                   :y 0})
      (c/text ctx {:text (scroll-status-str lay)
                   :x (:x (layout-bounds lay))
                   :y 10}))
    (doseq [[k lay] r-lays]
      (c/text ctx {:text (str "Region " (name k) " columns.")
                   :x (:x (layout-bounds lay))
                   :y 0})
      (c/text ctx {:text (scroll-status-str lay)
                   :x (:x (layout-bounds lay))
                   :y 10}))
    (let [segs-left (+ cells-left (get-in opts [:drawing :seg-h-space-px]))]
          (c/text ctx {:text (str "Segments. "
                             (if sel-col "(arrows keys to move)"
                                 "(click on a column)")
                             " Page up / page down to scroll columns.")
                       :x segs-left :y 0}))
    (let [world-w-px (get-in opts [:drawing :world-w-px])
          in-value (:value (first (core/input-seq sel-state)))]
      (when-let [draw-world (:comportexviz/draw-world (meta in-value))]
        (draw-world in-value ctx 0 top-px world-w-px (- height-px top-px) sel-state)))
    (doseq [dt (range (min draw-steps (count @steps)))
            :let [state (nth @steps dt)
                  prev-state (nth @steps (inc dt) nil)
                  cache (::cache (meta state))]]
      ;; draw encoded inbits
      (doseq [[k lay] i-lays
              :when (or (== 1 (count (p/dims-of lay)))
                        (== dt sel-dt))
              :let [inp (get-in state [:inputs k])
                    ;; region this input feeds to, for predictions
                    ff-rgn-k (first (get-in state [:fb-deps k]))
                    prev-ff-rgn (get-in prev-state [:regions k])]]
        (->> (i-bgs k)
             (draw-image-dt ctx lay dt))
        (when (get-in opts [:input :active])
          (->> (active-bits-image lay inp)
               (with-cache cache [::abits k] opts :input)
               (draw-image-dt ctx lay dt)))
        (when (and (get-in opts [:input :predicted])
                   prev-ff-rgn)
          (->> (pred-bits-image lay prev-ff-rgn)
               (with-cache cache [::pbits k] opts :input)
               (draw-image-dt ctx lay dt))))
      ;; draw regions
      (doseq [[k lay] r-lays
              :when (or (== 1 (count (p/dims-of lay)))
                        (== dt sel-dt))
              :let [rgn (get-in state [:regions k])]]
        (->> (r-bgs k)
             (draw-image-dt ctx lay dt))
        (when (get-in opts [:columns :overlaps])
          (->> (overlaps-columns-image lay rgn)
               (with-cache cache [::ocols k] opts :columns)
               (draw-image-dt ctx lay dt)))
        (when (get-in opts [:columns :n-segments])
          (->> (n-segments-columns-image lay rgn)
               (with-cache cache [::nsegcols k] opts :columns)
               (draw-image-dt ctx lay dt)))
        (when (get-in opts [:columns :active])
          (->> (active-columns-image lay rgn)
               (with-cache cache [::acols k] opts :columns)
               (draw-image-dt ctx lay dt)))
        (when (get-in opts [:columns :predictive])
          (->> (pred-columns-image lay rgn)
               (with-cache cache [::pcols k] opts :columns)
               (draw-image-dt ctx lay dt)))
        (when (get-in opts [:columns :temporal-pooling])
          (->> (tp-columns-image lay rgn)
               (with-cache cache [::tpcols k] opts :columns)
               (draw-image-dt ctx lay dt))))
      (when (not= opts (:opts @cache))
        (swap! cache assoc :opts opts)))
    ;; highlight selection
    (doseq [lay (vals i-lays)]
      (lay/highlight-dt lay ctx sel-dt))
    (doseq [lay (vals r-lays)]
      (lay/highlight-dt lay ctx sel-dt))
    (when sel-col
      (let [lay (r-lays sel-region)]
        (lay/highlight-element lay ctx sel-dt sel-col)))
    ;; draw ff synapses
    (when (get-in opts [:ff-synapses :active])
      (doseq [k (keys r-lays)
              :when (or (not sel-col)
                        (= sel-region k))]
        (draw-ff-synapses ctx k sel-state r-lays i-lays sel-col sel-dt opts)))
    ;; draw selected cells and segments
    (when (and sel-col
               (< (inc sel-dt) (count @steps)))
      (let [rgn (get-in sel-state [:regions sel-region])
            lay (r-lays sel-region)]
        (draw-cell-segments ctx (:layer-3 rgn) lay sel-col sel-dt cells-left
                            opts))))
  nil)

(defn draw!
  [selection]
  (when (seq @steps)
    (do-draw! selection)))

;; ## Event stream processing

(defn listen [el type capture-fn]
  (let [out (chan)]
    (gevents/listen el type
                    (fn [e] (put! out e)
                      (when (capture-fn e)
                        (.preventDefault e)
                        false)))
    out))

(defn handle-canvas-clicks
  [el selection]
  (let [clicks (listen el "click" (fn [_] false))]
    (go
     (while true
       (let [e (<! clicks)
             x (.-offsetX e)
             y (.-offsetY e)
             i-lays (:inputs @layouts)
             r-lays (:regions @layouts)
             ;; we need to assume there is a previous step, so:
             max-dt (max 0 (- (count @steps) 2))
             hit? (atom false)]
         ;; check inputs
         (doseq [[k lay] i-lays
                 :let [[dt id] (lay/clicked-id lay x y)]
                 :when dt]
           (reset! hit? true)
           (when (== 1 (count (p/dims-of lay)))
             (swap! selection assoc :dt (min dt max-dt))))
         ;; check regions
         (doseq [[k lay] r-lays
                 :let [[dt id] (lay/clicked-id lay x y)]
                 :when dt]
           (reset! hit? true)
           (if (== 1 (count (p/dims-of lay)))
             (swap! selection assoc :region k :col id :dt (min dt max-dt))
             (swap! selection assoc :region k :col id)))
         (when-not @hit?
           ;; checked all, nothing clicked
           (swap! selection assoc :col nil)))))))

(def code-key
  {33 :page-up
   34 :page-down
   37 :left
   38 :up
   39 :right
   40 :down})

(defn handle-canvas-keys
  [el selection sim-step!]
  (let [presses (listen el goog.events.EventType.KEYDOWN
                        (fn [e] (code-key (.-keyCode e))))]
    (go
     (while true
       (let [e (<! presses)
             k (code-key (.-keyCode e))
             ;; we need to assume there is a previous step, so:
             max-dt (max 0 (- (count @steps) 2))]
         (when k
           (case k
             :left (swap! selection update-in [:dt]
                          (fn [x] (min (inc x) max-dt)))
             :right (if (zero? (:dt @selection))
                      (do (sim-step!)
                          (swap! selection identity)) ;; redraw
                      (swap! selection update-in [:dt]
                             (fn [x] (max (dec x) 0))))
             :up (swap! selection update-in [:col]
                        (fn [x] (when x (dec x))))
             :down (swap! selection update-in [:col]
                          (fn [x] (when x (inc x))))
             :page-up (scroll! false)
             :page-down (scroll! true)
             )))))))

(defn init!
  [init-model steps-c selection sim-step!]
  (let [inp-keys (p/input-keys init-model)
        rgn-keys (p/region-keys init-model)
        inputs (:inputs init-model)
        regions (:regions init-model)
        d-opts (:drawing @viz-options)
        force-d (:force-d d-opts)
        spacer (:h-space-px d-opts)
        world-w-px (:world-w-px d-opts)
        ;; for now draw inputs in a horizontal stack
        [i-lays i-right]
        (reduce (fn [[lays left] k]
                  (let [topo (p/topology (inputs k))
                        lay (make-layout topo top-px left height-px d-opts
                                         true :force-d force-d)]
                    [(assoc lays k lay)
                     (+ (lay/right-px lay) spacer)]))
                [{} (+ world-w-px 10)]
                inp-keys)
        [r-lays r-right]
        (reduce (fn [[lays left] k]
                  ;; TODO layers!
                  (let [topo (p/topology (regions k))
                        lay (make-layout topo top-px left height-px d-opts
                                         false :force-d force-d)]
                    [(assoc lays k lay)
                     (+ (lay/right-px lay) spacer)]))
                [{} i-right]
                rgn-keys)]
    (reset! layouts {:inputs i-lays
                     :regions r-lays}))
  ;; stream the simulation steps into the sliding history buffer
  (go (loop []
        (when-let [x* (<! steps-c)]
          (let [x (vary-meta x* assoc ::cache (atom {}))]
            (swap! steps (fn [xs]
                           (take @keep-steps (cons x xs)))))
          (recur))))
  (let [el (->dom "#comportex-viz")]
    (set! (.-width el) (* 0.70 (- (.-innerWidth js/window) 20)))
    (set! (.-height el) height-px)
    (handle-canvas-clicks el selection)
    (handle-canvas-keys js/document selection sim-step!)))
