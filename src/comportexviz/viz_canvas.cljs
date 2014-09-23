(ns comportexviz.viz-canvas
  (:require [c2.dom :as dom :refer [->dom]]
            [c2.event]
            [goog.events.EventType]
            [goog.events :as gevents]
            [goog.string :as gstring]
            [goog.string.format]
            [monet.canvas :as c]
            [monet.core]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.sequence-memory :as sm]
            [org.nfrac.comportex.util :as util]
            [clojure.set :as set]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-cache]]))

(def height-px 900)
(def top-px 20)

(def viz-options
  (atom {:input {:active true
                 :predicted true}
         :columns {:active true
                   :overlaps nil
                   :predictive true
                   :temporal-pooling true
                   :scroll-counter 0}
         :ff-synapses {:active nil
                       :inactive nil
                       :permanences nil}
         :lat-synapses {:from :learning ;; :learning, :all, :none
                        :active true
                        :inactive nil
                        :permanences nil}
         :drawing {:draw-steps 25
                   :bit-w-px 5
                   :bit-h-px 2
                   :bit-shrink 0.85
                   :col-d-px 5
                   :col-shrink 0.85
                   :cell-r-px 10
                   :seg-w-px 30
                   :seg-h-px 10
                   :seg-h-space-px 50
                   :h-space-px 80}
         }))

(def keep-steps (atom 25))
(def steps (atom []))
(def layouts (atom nil))

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
  {:inactive "white"
   :active (hsl :red 1.0 0.5)
   :predicted (hsl :blue 1.0 0.5 0.5)
   :active-predicted (hsl :purple 1.0 0.4)
   :highlight (hsl :yellow 1 0.75 0.5)
   :temporal-pooling (hsl :green 1 0.5 0.4)
   })

;;; ## Layouts

(defprotocol PArrayLayout
  (layout-bounds [this]
    "Returns `{:x :y :w :h}` defining bounding box from top left.")
  (origin-px-topleft [this dt]
    "Returns [x y] pixel coordinates for top left of time offset dt.")
  (local-px-topleft [this id]
    "Returns [x y] pixel coordinates for id relative to the dt origin.")
  (element-size-px [this]
    "The size [w h] in pixels of each drawn array element.")
  (n-onscreen [this]
    "Number of elements per timestep visible on the screen, for pagination.")
  (top-id-onscreen [this]
    "Current scroll position, giving the first element id visible on screen.")
  (clicked-id [this x y]
    "Returns [dt id] for the [x y] pixel coordinates, or null.")
  (draw-element [this ctx id]
    "Draws the element id in local coordinates. Does not stroke or fill.")
  (highlight-dt [this ctx dt]
    "Draws highlight on the time offset dt in global coordinates.")
  (highlight-element [this ctx dt id]
    "Draws highlight on the element id in global coordinates."))

(defn right-px
  [this]
  (let [b (layout-bounds this)]
    (+ (:x b) (:w b))))

(defn element-xy
  "Returns pixel coordinates on the canvas `[x y]` for the
   center of an input element `id` at time delay `dt`."
  [lay id dt]
  (let [[w h] (element-size-px lay)
        [x y] (origin-px-topleft lay dt)
        [lx ly] (local-px-topleft lay id)]
    [(+ x lx (* w 0.5))
     (+ y ly (* h 0.5))]))

(defn draw-element-group
  "Draws all elements with the given ids in a single path, without
   filling or stroking it. For efficiency, skips any ids which are
   currently offscreen."
  [ctx lay ids]
  (let [j0 (top-id-onscreen lay)
        j1 (+ j0 (n-onscreen lay) -1)]
    (c/begin-path ctx)
    (doseq [i ids
           :when (<= j0 i j1)]
      (draw-element lay ctx i))
    ctx))

(defn fill-elements
  [ctx lay id-styles set-style]
  (c/save ctx)
  (doseq [[style ids] (group-by id-styles (keys id-styles))]
    (draw-element-group ctx lay ids)
    (set-style ctx style)
    (c/fill ctx))
  (c/restore ctx)
  ctx)

(defn draw-image-dt
  [ctx lay dt img]
  (let [[x y] (origin-px-topleft lay dt)]
    (c/draw-image ctx img x y)))

;;; ## Grid Layouts support

(defn circle
  [ctx x y r]
  (.arc ctx x y r 0 (* (.-PI js/Math) 2) true))

(defn circle-from-bounds
  [ctx x y w]
  (let [r (* w 0.5)]
    (circle ctx (+ x r) (+ y r) r)))

(defn centred-rect
  [cx cy w h]
  {:x (- cx (/ w 2))
   :y (- cy (/ h 2))
   :w w
   :h h})

(defn highlight-rect
  [ctx rect]
  (doto ctx
    (c/stroke-style (:highlight state-colors))
    (c/stroke-width 3)
    (c/stroke-rect rect)
    (c/stroke-style "black")
    (c/stroke-width 1)
    (c/stroke-rect rect)))

;;; ## Grid Layouts

(defrecord Grid1dLayout
    [elements-per-dt
     scroll-top
     draw-steps
     element-w
     element-h
     shrink
     left-px
     circles?]
  PArrayLayout
  (layout-bounds [_]
    {:x left-px :y top-px
     :w (* draw-steps element-w)
     :h (* elements-per-dt element-h)})
  
  (origin-px-topleft [_ dt]
    (let [right (+ left-px (* draw-steps element-w))
          off-x-px (* (+ dt 1) element-w)
          x-px (- right off-x-px)]
      [x-px top-px]))
  
  (local-px-topleft [_ id]
    [0 (* (- id scroll-top) element-h)])
  
  (element-size-px [_]
    [element-w element-h])
  
  (n-onscreen [_]
    (min elements-per-dt
         (/ (- height-px top-px) element-h)))
  
  (top-id-onscreen [_]
    scroll-top)
  
  (clicked-id [this x y]
    (let [right (+ left-px (* draw-steps element-w))
          dt (Math/floor (/ (- right x) element-w))
          id* (Math/floor (/ (- y top-px) element-h))
          id (+ id* scroll-top)]
      (when (and (<= 0 dt (count @steps))
                 (<= 0 id* (n-onscreen this)))
        [dt id])))
  
  (draw-element [this ctx id]
    (let [[x y] (local-px-topleft this id)]
      (if circles?
        (circle-from-bounds ctx x y
                            (* element-w shrink))
        (.rect ctx x y
               (* element-w shrink)
               (* element-h shrink)))))
  
  (highlight-dt [this ctx dt]
    ;; draw vertical axis on selected dt
    (let [[x y] (origin-px-topleft this dt)
          bb (layout-bounds this)]
      (highlight-rect ctx {:x x
                           :y (- y 5)
                           :w (+ element-w 1)
                           :h (+ 10 (:h bb))})))
  
  (highlight-element [this ctx dt id]
    ;; draw horizontal axis on selected id
    (let [[x y] (origin-px-topleft this dt)
          [lx ly] (local-px-topleft this id)
          bb (layout-bounds this)]
      (highlight-rect ctx {:x (- (:x bb) 5)
                           :y (+ y ly)
                           :w (+ (:w bb) 10)
                           :h (+ element-h 1)}))))

(defn inbits-1d-layout
  [nbits left opts]
  (let [{:keys [draw-steps
                bit-w-px
                bit-h-px
                bit-shrink]} opts]
    (map->Grid1dLayout
     {:elements-per-dt nbits
      :scroll-top 0
      :draw-steps draw-steps
      :element-w bit-w-px
      :element-h bit-h-px
      :shrink bit-shrink
      :left-px left
      :circles? false})))

(defn columns-1d-layout
  [ncol left opts]
  (let [{:keys [draw-steps
                col-d-px
                col-shrink]} opts]
    (map->Grid1dLayout
     {:elements-per-dt ncol
      :scroll-top 0
      :draw-steps draw-steps
      :element-w col-d-px
      :element-h col-d-px
      :shrink col-shrink
      :left-px left
      :circles? true})))

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
  [ctx rgn prev-rgn r-lay src src-lay sel-cid sel-dt opts]
  (c/save ctx)
  (c/stroke-width ctx 1)
  (c/alpha ctx 1)
  (let [cids (if sel-cid [sel-cid]
                 (:active-columns rgn))
        src-bits (core/bits-value src)
        do-inactive? (get-in opts [:ff-synapses :inactive])
        do-perm? (get-in opts [:ff-synapses :permanences])]
    (doseq [cid cids
            active? [false true]
            :when (or active? do-inactive?)
            :let [col (get-in prev-rgn [:columns cid])
                  syns (-> col :ff-synapses :connected)
                  sub-syns (if active?
                             (select-keys syns src-bits)
                             (apply dissoc syns src-bits))
                  [this-x this-y] (element-xy r-lay cid sel-dt)]]
      (c/stroke-style ctx (if active? (state-colors :active) "black"))
      (doseq [[in-id perm] sub-syns
              :let [[src-id _] (core/source-of-bit src in-id)
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
                      :active :inactive)])
                 syns))

(defn natural-curve
  [ctx x0 y0 x1 y1]
  (let [x-third (/ (- x1 x0) 3)]
    (c/bezier-curve-to ctx
                       (- x1 x-third) y0
                       (+ x0 x-third) y1
                       x1 y1)))

(defn draw-cell-segments
  [ctx rgn prev-rgn lay
   cid dt cells-left opts]
  (c/save ctx)
  (let [col (get-in prev-rgn [:columns cid])
        spec (:spec prev-rgn)
        th (:activation-threshold (:spec rgn))
        pcon (:connected-perm spec)
        ac (:active-cells prev-rgn)
        learning (:learn-segments rgn)
        active? (get-in rgn [:active-columns cid])
        bursting? (get-in rgn [:bursting-columns cid])
        current-ac (:active-cells rgn)
        cells (:cells col)
        ncells (count cells)
        nsegbycell (map (comp count :segments) cells)
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
        [col-x col-y] (element-xy lay cid dt)]
    (letfn [(seg-xy
              [ci si]
              (let [i-all (apply + si (take ci nsegbycell-pad))
                    frac (/ i-all nseg-pad)]
                [(+ segs-left seg-r-px)
                 (+ our-top top-px (* frac our-height))]))
            (cell-xy
              [ci]
              (let [[_ sy] (seg-xy ci 0)]
                [cells-left sy]))
            (col-cell-line
              [ctx ci]
              (let [[cell-x cell-y] (cell-xy ci)]
                (doto ctx
                  (c/begin-path)
                  (c/move-to (+ col-x col-r-px 1) col-y) ;; avoid obscuring colour
                  (natural-curve col-x col-y cell-x cell-y)
                  (c/stroke))))
            (cell-seg-line
              [ctx ci si]
              (let [[cell-x cell-y] (cell-xy ci)
                    [sx sy] (seg-xy ci si)]
                (doto ctx
                  (c/begin-path)
                  (c/move-to sx sy)
                  (c/line-to (+ cell-x cell-r-px) cell-y)
                  (c/stroke))))]
      ;; draw background lines to cell from column and from segments
      (c/stroke-width ctx col-d-px)
      (c/stroke-style ctx (:inactive state-colors))
      (doseq [[ci {segs :segments}] (map-indexed vector cells)]
        (col-cell-line ctx ci)
        (doseq [si (range (count segs))]
          (cell-seg-line ctx ci si)))
      ;; draw the rest
      (doseq [[ci {segs :segments}] (map-indexed vector cells)
              :let [[cell-x cell-y] (cell-xy ci)
                    cell-id [cid ci]
                    cell-active? (current-ac cell-id)
                    learn-cell? (find learning cell-id)
                    learn-seg-idx (when learn-cell? (val learn-cell?))
                    seg-sg (mapv #(group-synapses (:synapses %) ac pcon) segs)
                    on? (fn [sg] (>= (count (sg [:connected :active])) th))
                    cell-state (cond
                                (and cell-active? (some on? seg-sg)) :active-predicted
                                (some on? seg-sg) :predicted
                                cell-active? :active
                                :else :inactive)]]
        (when cell-active?
          (doto ctx
            (c/stroke-style (:active state-colors))
            (c/stroke-width 2)
            (col-cell-line ci)))
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
        (doseq [[si sg] (map-indexed vector seg-sg)
                :let [[sx sy] (seg-xy ci si)
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
              (c/stroke-width 2)
              (cell-seg-line ci si)))
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
                do-from (get-in opts [:lat-synapses :from])]
            (when (or (= do-from :all)
                      (and (= do-from :learning) learn-seg?))
              (doseq [syn-state (concat (when do-ina? [:inactive])
                                        (when do-act? [:active]))
                      :let [syns (sg [:connected syn-state])]]
                (c/stroke-style ctx (state-colors syn-state))
                (doseq [[[to-cid _] perm] syns
                        :let [[cx cy] (element-xy lay to-cid (inc dt))]]
                         (doto ctx
                           (c/alpha (if do-perm? perm 1))
                           (c/begin-path)
                           (c/move-to sx sy)
                           (c/line-to (+ cx 1) cy) ;; +1 avoid obscuring colour
                           (c/stroke)))
                (c/alpha ctx 1)))))))
    (c/restore ctx))
  ctx)

(defn detail-text
  [{dt :dt
    rid :region
    cid :cid
    :as selection}]
  (let [state (nth @steps dt)
        rgns (core/region-seq state)
        rgn (nth rgns rid)
        inp (first (core/inputs-seq state))
        in (core/domain-value inp)
        bits (core/bits-value inp)]
    (->>
     ["__Selection__"
      (str "* timestep " (:timestep rgn)
           " (delay " dt ")")
      (str "* column " (or cid "nil"))
      ""
      "__Input__"
      (str in " (" (count bits) " bits)")
      ""
      "__Active columns__"
      (str (sort (:active-columns rgn)))
      ""
      "__Active cells__"
      (str (sort (:active-cells rgn)))
      ""
      "__Learn cells__"
      (str (sort (:learn-cells rgn)))
      ""
      "__Signal cells__"
      (str (sort (:signal-cells rgn)))
      ""
      "__TP scores__"
      (str (sort (:temporal-pooling-scores rgn)))
      ""
      "__Predicted cells__"
      (str (sort (:predictive-cells rgn)))
      ""
      (if cid
        (let [dtp (inc dt)
              pstate (nth @steps dtp)
              prgn (nth (core/region-seq pstate) rid)
              pcbc (:predictive-cells-by-column prgn)
              pcells (:active-cells prgn)
              pcol (get-in prgn [:columns cid])
              col (get-in rgn [:columns cid])
              pcon (:connected-perm (:spec prgn))
              rgn-tree (nth (core/region-tree-seq state) rid)
              bits (core/incoming-bits-value rgn-tree)
              sig-bits (if (pos? rid)
                         (core/signal-bits-value
                          (nth (core/region-tree-seq state) (dec rid)))
                         {})]
          ["__Active cells prev__"
           (str (sort (:active-cells prgn)))
           ""
           "__Learn cells prev__"
           (str (sort (:learn-cells prgn)))
           ""
           "__Predicted cells prev__"
           (str (sort (:predictive-cells prgn)))
           ""
           "__Selected column__"
           "__Connected ff-synapses__"
           (let [syns (:connected (:ff-synapses pcol))]
             (for [[id p] (sort syns)]
               (str "  " id " :=> "
                    (gstring/format "%.2f" p)
                    (if (sig-bits id) " S")
                    (if (bits id) (str " A "
                                       (core/source-of-incoming-bit rgn-tree id))))))
           "__Cells and their Dendrite segments__"
           (->> (:cells pcol)
                (map-indexed
                 (fn [i cell]
                   (let [ds (:segments cell)
                         ac (:active-cells prgn)
                         lc (:learn-cells prgn)]
                     [(str "CELL " i)
                      (str (count ds) " = "
                           (sort (map (comp count :synapses) ds)))
                      (str "Lateral excitation from this cell: "
                           (sm/lateral-excitation-from prgn (:depth (:spec prgn)) (:id cell)))
                      (for [i (range (count ds))
                            :let [syns (:synapses (ds i))]]
                        [(str "  SEGMENT " i)
                         (for [[id p] (sort syns)]
                           (str "  " id
                                (if (>= p pcon) " :=> " " :.: ")
                                (gstring/format "%.2f" p)
                                (if (lc id) " L"
                                    (if (ac id) " A"))))])
                      ]))))
           ]))]
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
    (c/fill-style ctx (:inactive state-colors))
    (draw-element-group ctx lay (range j (+ j (n-onscreen lay))))
    (c/fill ctx)
    el))

(defn active-bits-image
  [lay inp]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        inbits (core/bits-value inp)]
    (c/fill-style ctx (:active state-colors))
    (draw-element-group ctx lay inbits)
    (c/fill ctx)
    el))

(defn pred-bits-image
  [lay prev-rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        pcbc (:predictive-cells-by-column prev-rgn)
        bit-votes (sm/predicted-bit-votes prev-rgn pcbc)
        bit-alpha (util/remap #(min 1.0 (/ % 8)) bit-votes)]
    (c/fill-style ctx (:predicted state-colors))
    (fill-elements ctx lay bit-alpha c/alpha)
    el))

(defn active-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        ids (:active-columns rgn)]
    (c/fill-style ctx (:active state-colors))
    (draw-element-group ctx lay ids)
    (c/fill ctx)
    el))

(defn pred-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        ids (keys (:prev-predictive-cells-by-column rgn))]
    (c/fill-style ctx (:predicted state-colors))
    (draw-element-group ctx lay ids)
    (c/fill ctx)
    el))

(defn tp-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        ids (keys (:temporal-pooling-scores rgn))]
    (c/fill-style ctx (:temporal-pooling state-colors))
    (draw-element-group ctx lay ids)
    (c/fill ctx)
    el))

(defn overlaps-columns-image
  [lay rgn]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        idm (->> (:overlaps rgn)
                 (util/remap #(min 1.0 (/ % 16))))]
    (c/fill-style ctx "black")
    (fill-elements ctx lay idm c/alpha)
    (c/fill ctx)
    el))

(defn scroll-status-str
  [lay]
  (str "Showing " (top-id-onscreen lay)
       "--" (+ (top-id-onscreen lay)
               (n-onscreen lay) -1)
       " of " (:elements-per-dt lay)))

(defn draw!
  [{sel-dt :dt
    sel-rid :region
    sel-cid :cid
    :as selection}]
  (dom/val "#detail-text"
           (if sel-cid (detail-text selection)
               "Select a column (by clicking on it) to see details."))
  (let [opts @viz-options
        i-lay (:input @layouts)
        r-lays (:regions @layouts)
        sel-state (nth @steps sel-dt)
        sel-prev-state (nth @steps (inc sel-dt) {})
        canvas-el (->dom "#comportex-viz")
        ctx (c/get-context canvas-el "2d")
        inbits-bg (bg-image i-lay)
        columns-bgs (map bg-image r-lays)
        cells-left (+ (right-px (last r-lays))
                      (get-in opts [:drawing :h-space-px]))
        width-px (.-width canvas-el)]
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    (c/text ctx {:text "Input bits.    Time -->"
                 :x (:x (layout-bounds i-lay))
                 :y (- top-px 13)})
    (c/text ctx {:text (scroll-status-str i-lay)
                 :x (:x (layout-bounds i-lay))
                 :y (- top-px 3)})
    (doseq [[rid r-lay] (map-indexed vector r-lays)
            :let []]
      (c/text ctx {:text (str "Region " rid " columns.")
                   :x (:x (layout-bounds r-lay))
                   :y (- top-px 13)})
      (c/text ctx {:text (scroll-status-str r-lay)
                   :x (:x (layout-bounds r-lay))
                   :y (- top-px 3)}))
    (let [segs-left (+ cells-left (get-in opts [:drawing :seg-h-space-px]))]
          (c/text ctx {:text (str "Segments. "
                             (if sel-cid "(arrows keys to move)"
                                 "(click on a column)"))
                       :x segs-left :y (- top-px 13)}))
    (doseq [dt (range (count @steps))
            :let [state (nth @steps dt)
                  prev-state (nth @steps (inc dt) {})
                  prev-first-region (first (core/region-seq prev-state))
                  rgns (core/region-seq state)
                  cache (::cache (meta state))]]
      (->> inbits-bg
           (draw-image-dt ctx i-lay dt))
      (when (get-in opts [:input :active])
        (->> (active-bits-image i-lay (first (core/inputs-seq state)))
             (with-cache cache ::abits opts :input)
             (draw-image-dt ctx i-lay dt)))
      (when (get-in opts [:input :predicted])
        (->> (pred-bits-image i-lay prev-first-region)
             (with-cache cache ::pbits opts :input)
             (draw-image-dt ctx i-lay dt)))
      (doseq [[rid r-lay rgn bg-i]
              (map vector (range) r-lays rgns columns-bgs)]
        (->> bg-i
             (draw-image-dt ctx r-lay dt))
        (when (get-in opts [:columns :overlaps])
          (->> (overlaps-columns-image r-lay rgn)
               (with-cache cache [::ocols rid] opts :columns)
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
               (draw-image-dt ctx r-lay dt))))
      (when (not= opts (:opts @cache))
        (swap! cache assoc :opts opts)))
    ;; highlight selection
    (highlight-dt i-lay ctx sel-dt)
    (doseq [r-lay r-lays]
      (highlight-dt r-lay ctx sel-dt))
    (when sel-cid
      (let [r-lay (nth r-lays sel-rid)]
        (highlight-element r-lay ctx sel-dt sel-cid)))
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
        (when (or (not sel-cid)
                  (= sel-rid rid))
          (draw-ff-synapses ctx rgn prev-rgn r-lay src src-lay
                            sel-cid sel-dt opts))))
    ;; draw selected cells and segments
    (when (and sel-cid
               (< (inc sel-dt) (count @steps)))
      (let [rgn (-> (core/region-seq sel-state)
                    (nth sel-rid))
            prev-rgn (-> (core/region-seq sel-prev-state)
                         (nth sel-rid))
            lay (nth r-lays sel-rid)]
        (draw-cell-segments ctx rgn prev-rgn lay
                            sel-cid sel-dt cells-left opts))))
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
         (if-let [[dt _] (clicked-id i-lay x y)]
           ;; in-bit clicked
           (swap! selection assoc :dt (min dt max-dt))
           ;; otherwise, check regions
           (loop [rid 0]
             (if (< rid (count r-lays))
               (if-let [[dt id] (clicked-id (nth r-lays rid) x y)]
                 (reset! selection {:region rid :cid id
                                    :dt (min dt max-dt)})
                 (recur (inc rid)))
               ;; checked all, nothing clicked
               (swap! selection assoc :cid nil)))))))))

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
             :up (swap! selection update-in [:cid]
                        (fn [x] (when x (dec x))))
             :down (swap! selection update-in [:cid]
                          (fn [x] (when x (inc x))))
             :page-up (scroll-columns! false)
             :page-down (scroll-columns! true)
             )))))))

(defn init!
  [init-model steps-c selection sim-step!]
  (let [rgns (core/region-seq init-model)
        ;; for now assume only one input
        inp (first (core/inputs-seq init-model))
        d-opts (:drawing @viz-options)
        nbits (core/bit-width inp)
        i-lay (inbits-1d-layout nbits 0 d-opts)
        ;; for now draw regions in a horizontal stack
        spacer (:h-space-px d-opts)
        r-lays (reduce (fn [lays rgn]
                         (let [ncol (:ncol (:spec rgn))
                               left (-> (or (peek lays) i-lay)
                                        (right-px)
                                        (+ spacer))]
                           (conj lays (columns-1d-layout
                                       ncol left d-opts))))
                       []
                       rgns)]
    (reset! layouts {:input i-lay
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
