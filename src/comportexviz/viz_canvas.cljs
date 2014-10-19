(ns comportexviz.viz-canvas
  (:require [comportexviz.viz-layouts :as lay
             :refer [layout-bounds
                     n-onscreen
                     top-id-onscreen
                     element-xy
                     draw-element-group
                     fill-elements
                     circle
                     centred-rect
                     inbits-1d-layout
                     columns-1d-layout
                     inbits-2d-layout
                     columns-2d-layout]]
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
   :alternative (hsl 40 1 0.5 0.75)
   })

(def viz-options
  (atom {:input {:active true
                 :predicted true}
         :columns {:active true
                   :overlaps nil
                   :n-segments nil
                   :predictive true
                   :temporal-pooling true
                   :alternative false
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
                   :input-w-px 150
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
(def layouts (atom nil))

(defn draw-image-dt
  [ctx lay dt img]
  (let [[x y] (lay/origin-px-topleft lay dt)]
    (c/draw-image ctx img x y)))

(defn scroll-columns!
  [down?]
  (swap! layouts update-in [:regions]
         (fn [r-lays]
           (mapv (fn [lay]
                   (let [n (n-onscreen lay)
                         ncol (:elements-per-dt lay)]
                     (update-in lay [:scroll-top]
                               (fn [x]
                                 (if down?
                                   (-> (+ x n) (min (- ncol n)))
                                   (-> (- x n) (max 0)))))))
                 r-lays)))
  ;; need this to invalidate the drawing cache
  (swap! viz-options update-in [:columns :scroll-counter]
         #(if down? (inc %) (dec %))))

(defn draw-ff-synapses
  [ctx rgn prev-rgn r-lay src src-lay sel-col sel-dt opts]
  (c/save ctx)
  (c/stroke-width ctx 1)
  (c/alpha ctx 1)
  (let [cf (:column-field rgn)
        ff-sg (:ff-sg cf)
        cols (if sel-col [sel-col]
                 (p/active-columns (:layer-3 rgn)))
        src-bits (p/bits-value src)
        src-sbits (p/signal-bits-value src)
        do-inactive? (get-in opts [:ff-synapses :inactive])
        do-disconn? (get-in opts [:ff-synapses :disconnected])
        do-perm? (get-in opts [:ff-synapses :permanences])]
    (doseq [col cols
            syn-state (concat (when do-disconn? [:disconnected])
                              (when do-inactive? [:inactive-syn])
                              [:active :active-predicted])
            :let [all-syns (p/in-synapses ff-sg col)
                  syns (select-keys all-syns (p/sources-connected-to ff-sg col))
                  sub-syns (case syn-state
                             :active (select-keys syns src-bits)
                             :active-predicted (select-keys syns src-sbits)
                             :inactive-syn (apply dissoc syns src-bits)
                             :disconnected (apply dissoc all-syns (keys syns)))
                  [this-x this-y] (element-xy r-lay col sel-dt)]]
      (c/stroke-style ctx (state-colors syn-state))
      (doseq [[in-id perm] sub-syns
              :let [[src-id _] (p/source-of-bit src in-id)
                    [src-x src-y] (element-xy src-lay src-id sel-dt)]]
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
                      :active :inactive-syn)])
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
  [ctx rgn prev-rgn lay col dt cells-left opts]
  (c/save ctx)
  (let [spec (p/params prev-rgn)
        th (:seg-stimulus-threshold spec)
        pcon (:distal-perm-connected spec)
        cf (:column-field rgn)
        layer (:layer-3 rgn)
        prev-cf (:column-field prev-rgn)
        prev-layer (:layer-3 prev-rgn)
        ac (p/active-cells layer)
        prev-ac (p/active-cells prev-layer)
        prev-pc (p/predictive-cells prev-layer)
        learning (:learn-segments layer)
        alt-learning (:alternative-segments layer)
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
                  alt-learn-cell? (find alt-learning cell-id)
                  learn-cell? (or (find learning cell-id) alt-learn-cell?)
                  learn-seg-idx (when learn-cell? (val learn-cell?))
                  seg-sg (mapv #(group-synapses % prev-ac pcon) segs)
                  on? (fn [sg] (>= (count (sg [:connected :active])) th))
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
                                     (if alt-learn-cell? " alternatively")
                                     ")")))
                   :x cell-x :y (- cell-y cell-r-px 5)})
      (doseq [[si sg] (map-indexed vector seg-sg)
              :let [[sx sy] (seg-xy cslay ci si)
                    conn-act (count (sg [:connected :active]))
                    conn-tot (+ conn-act (count (sg [:connected :inactive])))
                    disc-act (count (sg [:disconnected :active]))
                    disc-tot (+ disc-act (count (sg [:disconnected :inactive])))
                    z (-> (/ conn-act th)
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
        (when (on? sg)
          (doto ctx
            (c/stroke-style (:active state-colors))
            (c/stroke-width 2))
          (cell-seg-line cslay ctx ci si))
        (c/fill-style ctx "black")
        (c/text ctx {:text (str "[" si "],  active " conn-act
                                " / " conn-tot " conn."
                                " (" disc-act " / " disc-tot " disconn.)")
                     :x (+ sx 5 seg-r-px) :y sy})
        ;; synapses
        (c/stroke-width ctx 1)
        (let [do-perm? (get-in opts [:lat-synapses :permanences])
              do-act? (get-in opts [:lat-synapses :active])
              do-ina? (get-in opts [:lat-synapses :inactive])
              do-disc? (get-in opts [:lat-synapses :disconnected])
              do-from (get-in opts [:lat-synapses :from])]
          (when (or (= do-from :all)
                    (and (= do-from :learning) learn-seg?))
            (doseq [syn-state (concat (when do-ina? [:inactive-syn])
                                      (when do-act? [:active]))
                    syn-conn (concat (when do-disc? [:disconnected])
                                     [:connected])
                    :let [syns (sg [syn-conn syn-state])]]
              (c/stroke-style ctx (state-colors syn-state))
              (doseq [[[to-col _] perm] syns
                      :let [[cx cy] (element-xy lay to-col (inc dt))]]
                (doto ctx
                  (c/alpha (if do-perm? perm 1))
                  (c/begin-path)
                  (c/move-to sx sy)
                  (c/line-to (+ cx 1) cy) ;; +1 avoid obscuring colour
                  (c/stroke)))
              (c/alpha ctx 1))))))
    (c/restore ctx))
  ctx)

(defn detail-text
  [{dt :dt
    rid :region
    col :col
    :as selection}]
  (let [state (nth @steps dt)
        rgns (core/region-seq state)
        rgn (nth rgns rid)
        cf (:column-field rgn)
        layer (:layer-3 rgn)
        inp (first (core/inputs-seq state))
        in (p/domain-value inp)
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
      (str (sort (:learn-segments layer)))
      ""
      "__Signal cells__"
      (str (sort (p/signal-cells layer)))
      ""
      "__Alternative cells / segs__"
      (str (sort (:alternative-segments layer)))
      ""
      "__TP scores__"
      (str (sort (:tp-scores cf)))
      ""
      "__Predicted cells__"
      (str (sort (p/predictive-cells layer)))
      ""
      (if col
        (let [dtp (inc dt)
              p-state (nth @steps dtp)
              p-rgn (nth (core/region-seq p-state) rid)
              p-cf (:column-field p-rgn)
              p-layer (:layer-3 p-rgn)
              p-ff-sg (:ff-sg p-cf)
              p-distal-sg (:distal-sg p-layer)
              ac (p/active-cells p-layer)
              lc (or (p/learnable-cells p-layer) #{})
              pcon (:distal-perm-connected (p/params p-rgn))
              rgn-tree (nth (core/region-tree-seq state) rid)
              bits (core/incoming-bits-value rgn-tree p/bits-value)
              sig-bits (core/incoming-bits-value rgn-tree p/signal-bits-value)]
          ["__Active cells prev__"
           (str (sort ac))
           ""
           "__Learn cells prev__"
           (str (sort lc))
           ""
           "__Predicted cells prev__"
           (str (sort (p/predictive-cells p-layer)))
           ""
           "__Selected column__"
           "__Connected ff-synapses__"
           (let [syns (p/in-synapses p-ff-sg col)]
             (for [[id p] (sort syns)]
               (str "  " id " :=> "
                    (gstring/format "%.2f" p)
                    (if (sig-bits id) " S")
                    (if (bits id) (str " A "
                                       (p/source-of-incoming-bit rgn-tree id))))))
           "__Cells and their Dendrite segments__"
           (for [ci (range (p/layer-depth layer))
                 :let [segs (p/cell-segments p-distal-sg [col ci])]]
             [(str "CELL " ci)
              (str (count segs) " = " (map count segs))
              (str "Lateral excitation from this cell: "
                   (p/targets-connected-from p-distal-sg [col ci]))
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
      (sort (p/params rgn))]
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
    (draw-element-group ctx lay (range j (+ j (n-onscreen lay))))
    (c/fill ctx)
    el))

(defn active-bits-image
  [lay inp]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        inbits (p/bits-value inp)]
    (c/fill-style ctx (:active state-colors))
    (draw-element-group ctx lay inbits)
    (c/fill ctx)
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
    (draw-element-group ctx lay cols)
    (c/fill ctx)
    el))

(defn pred-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        cols (->> (p/prior-predictive-cells (:layer-3 rgn))
                  (map first)
                  (distinct))]
    (c/fill-style ctx (:predicted state-colors))
    (draw-element-group ctx lay cols)
    (c/fill ctx)
    el))

(defn tp-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        cols (->> (p/temporal-pooling-cells (:layer-3 rgn))
                  (map first))]
    (c/fill-style ctx (:temporal-pooling state-colors))
    (draw-element-group ctx lay cols)
    (c/fill ctx)
    el))

(defn alternative-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        cols (->> (:alternative-cells (:layer-3 rgn))
                  (map first))]
    (c/fill-style ctx (:alternative state-colors))
    (draw-element-group ctx lay cols)
    (c/fill ctx)
    el))

(defn overlaps-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        col-m (->> (p/column-overlaps (:column-field rgn))
                   (util/remap #(min 1.0 (/ % 16))))]
    (c/fill-style ctx "black")
    (fill-elements ctx lay col-m c/alpha)
    (c/fill ctx)
    el))

(defn count-segs-in-column
  [distal-sg depth col]
  (reduce (fn [n ci]
            (+ n (util/count-filter seq
                                    (p/cell-segments distal-sg [col ci]))))
          (range depth)))

(defn n-segments-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        layer (:layer-3 rgn)
        sg (:distal-sg layer)
        n-cols (p/size-of layer)
        depth (p/layer-depth layer)
        col-m (->> (map #(count-segs-in-column sg depth %) (range n-cols))
                   (zipmap (range n-cols))
                   (util/remap #(min 1.0 (/ % 16))))]
    (c/fill-style ctx "black")
    (fill-elements ctx lay col-m c/alpha)
    (c/fill ctx)
    el))

(defn scroll-status-str
  [lay]
  (str "Showing " (top-id-onscreen lay)
       "--" (+ (top-id-onscreen lay)
               (n-onscreen lay) -1)
       " of " (p/size (:topo lay))))

(defn draw!
  [{sel-dt :dt
    sel-rid :region
    sel-col :col
    :as selection}]
  (dom/val "#detail-text"
           (if sel-col (detail-text selection)
               "Select a column (by clicking on it) to see details."))
  (let [opts @viz-options
        i-lay (:input @layouts)
        r-lays (:regions @layouts)
        draw-steps (get-in opts [:drawing :draw-steps])
        sel-state (nth @steps sel-dt)
        sel-prev-state (nth @steps (inc sel-dt) {})
        canvas-el (->dom "#comportex-viz")
        ctx (c/get-context canvas-el "2d")
        inbits-bg (bg-image i-lay)
        columns-bgs (map bg-image r-lays)
        cells-left (+ (lay/right-px (last r-lays))
                      (get-in opts [:drawing :h-space-px]))
        width-px (.-width canvas-el)]
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    (c/text-baseline ctx :top)
    (c/text ctx {:text "Input on selected timestep."
                 :x 2
                 :y 0})
    (c/text ctx {:text "Encoded bits.    => time =>"
                 :x (:x (layout-bounds i-lay))
                 :y 0})
    (c/text ctx {:text (scroll-status-str i-lay)
                 :x (:x (layout-bounds i-lay))
                 :y 10})
    (doseq [[rid r-lay] (map-indexed vector r-lays)]
      (c/text ctx {:text (str "Region " rid " columns.   => time =>")
                   :x (:x (layout-bounds r-lay))
                   :y 0})
      (c/text ctx {:text (scroll-status-str r-lay)
                   :x (:x (layout-bounds r-lay))
                   :y 10}))
    (let [segs-left (+ cells-left (get-in opts [:drawing :seg-h-space-px]))]
          (c/text ctx {:text (str "Segments. "
                             (if sel-col "(arrows keys to move)"
                                 "(click on a column)")
                             " Page up / page down to scroll columns.")
                       :x segs-left :y 0}))
    (let [inp-w-px (get-in opts [:drawing :input-w-px])
          inp (first (core/inputs-seq sel-state))
          rgn (first (core/region-seq sel-state))]
      (when-let [draw-input (:comportexviz/draw-input inp)]
        (c/save ctx)
        (c/translate ctx 0 top-px)
        (draw-input inp ctx inp-w-px (- height-px top-px) rgn)
        (c/restore ctx)))
    (doseq [dt (range (min draw-steps (count @steps)))
            :let [state (nth @steps dt)
                  prev-state (nth @steps (inc dt) {})
                  prev-first-region (first (core/region-seq prev-state))
                  rgns (core/region-seq state)
                  inp (first (core/inputs-seq state))
                  cache (::cache (meta state))]]
      ;; draw encoded inbits
      (when (or (== 1 (count (p/dims-of inp)))
                (== dt sel-dt))
        (->> inbits-bg
             (draw-image-dt ctx i-lay dt))
        (when (get-in opts [:input :active])
          (->> (active-bits-image i-lay inp)
               (with-cache cache ::abits opts :input)
               (draw-image-dt ctx i-lay dt)))
        (when (and (get-in opts [:input :predicted])
                   prev-first-region)
          (->> (pred-bits-image i-lay prev-first-region)
               (with-cache cache ::pbits opts :input)
               (draw-image-dt ctx i-lay dt))))
      ;; draw regions
      (doseq [[rid r-lay rgn bg-i]
              (map vector (range) r-lays rgns columns-bgs)
              :when (or (== 1 (count (p/dims-of rgn)))
                        (== dt sel-dt))]
        (->> bg-i
             (draw-image-dt ctx r-lay dt))
        (when (get-in opts [:columns :overlaps])
          (->> (overlaps-columns-image r-lay rgn)
               (with-cache cache [::ocols rid] opts :columns)
               (draw-image-dt ctx r-lay dt)))
        (when (get-in opts [:columns :n-segments])
          (->> (n-segments-columns-image r-lay rgn)
               (with-cache cache [::nsegcols rid] opts :columns)
               (draw-image-dt ctx r-lay dt)))
        (when (get-in opts [:columns :active])
          (->> (active-columns-image r-lay rgn)
               (with-cache cache [::acols rid] opts :columns)
               (draw-image-dt ctx r-lay dt)))
        (when (get-in opts [:columns :predictive])
          (->> (pred-columns-image r-lay rgn)
               (with-cache cache [::pcols rid] opts :columns)
               (draw-image-dt ctx r-lay dt)))
        (when (get-in opts [:columns :temporal-pooling])
          (->> (tp-columns-image r-lay rgn)
               (with-cache cache [::tpcols rid] opts :columns)
               (draw-image-dt ctx r-lay dt)))
        (when (get-in opts [:columns :alternative])
          (->> (alternative-columns-image r-lay rgn)
               (with-cache cache [::vcols rid] opts :columns)
               (draw-image-dt ctx r-lay dt))))
      (when (not= opts (:opts @cache))
        (swap! cache assoc :opts opts)))
    ;; highlight selection
    (lay/highlight-dt i-lay ctx sel-dt)
    (doseq [r-lay r-lays]
      (lay/highlight-dt r-lay ctx sel-dt))
    (when sel-col
      (let [r-lay (nth r-lays sel-rid)]
        (lay/highlight-element r-lay ctx sel-dt sel-col)))
    ;; draw feed-forward synapses
    ;; TODO mapping multiple input sources
    (when (get-in opts [:ff-synapses :active])
      (doseq [[rid rgn prev-rgn src r-lay src-lay]
              (map vector
                   (range)
                   (core/region-seq sel-state)
                   (core/region-seq sel-prev-state)
                   (list* (first (core/inputs-seq sel-state))
                          (core/region-tree-seq sel-state))
                   r-lays
                   (list* i-lay r-lays))]
        (when (or (not sel-col)
                  (= sel-rid rid))
          (draw-ff-synapses ctx rgn prev-rgn r-lay src src-lay
                            sel-col sel-dt opts))))
    ;; draw selected cells and segments
    (when (and sel-col
               (< (inc sel-dt) (count @steps)))
      (let [rgn (-> (core/region-seq sel-state)
                    (nth sel-rid))
            prev-rgn (-> (core/region-seq sel-prev-state)
                         (nth sel-rid))
            lay (nth r-lays sel-rid)]
        (draw-cell-segments ctx rgn prev-rgn lay
                            sel-col sel-dt cells-left opts))))
  nil)

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
             i-lay (:input @layouts)
             r-lays (:regions @layouts)
             ;; we need to assume there is a previous step, so:
             max-dt (- (count @steps) 2)]
         (if-let [[dt _] (lay/clicked-id i-lay x y)]
           ;; in-bit clicked
           (swap! selection assoc :dt (min dt max-dt))
           ;; otherwise, check regions
           (loop [rid 0]
             (if (< rid (count r-lays))
               (if-let [[dt id] (lay/clicked-id (nth r-lays rid) x y)]
                 (reset! selection {:region rid :col id
                                    :dt (min dt max-dt)})
                 (recur (inc rid)))
               ;; checked all, nothing clicked
               (swap! selection assoc :col nil)))))))))

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
             max-dt (- (count @steps) 2)]
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
             :page-up (scroll-columns! false)
             :page-down (scroll-columns! true)
             )))))))

(defn init!
  [init-model steps-c selection sim-step!]
  (let [rgns (core/region-seq init-model)
        d-opts (:drawing @viz-options)
        ;; for now assume only one input
        inp (first (core/inputs-seq init-model))
        inp-w-px (:input-w-px d-opts)
        ;; check dimensionality of input
        itopo (p/topology inp)
        enc-lay (if (== 1 (count (p/dimensions itopo)))
                  (inbits-1d-layout itopo top-px (+ inp-w-px 10) height-px d-opts)
                  (inbits-2d-layout itopo top-px (+ inp-w-px 10) height-px d-opts))
        ;; for now draw regions in a horizontal stack
        spacer (:h-space-px d-opts)
        r-lays (reduce (fn [lays rgn]
                         (let [topo (p/topology rgn)
                               left (-> (or (peek lays) enc-lay)
                                        (lay/right-px)
                                        (+ spacer))
                               r-lay (if (== 1 (count (p/dimensions topo)))
                                       (columns-1d-layout
                                        topo top-px left height-px d-opts)
                                       (columns-2d-layout
                                        topo top-px left height-px d-opts))]
                           (conj lays r-lay)))
                       []
                       rgns)]
    (reset! layouts {:input enc-lay
                     :regions r-lays}))
  ;; stream the simulation steps into the sliding history buffer
  (go (loop []
        (when-let [x (<! steps-c)]
          (let [x* (vary-meta x assoc ::cache (atom {}))]
            (swap! steps #(->> (cons x* %)
                               (take @keep-steps)
                               (vec))))
          (recur))))
  (let [el (->dom "#comportex-viz")]
    (set! (.-width el) (* 0.70 (- (.-innerWidth js/window) 20)))
    (set! (.-height el) height-px)
    (handle-canvas-clicks el selection)
    (handle-canvas-keys js/document selection sim-step!)))
