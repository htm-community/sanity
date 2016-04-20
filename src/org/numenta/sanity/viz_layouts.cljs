(ns org.numenta.sanity.viz-layouts
  (:require [monet.canvas :as c]
            [tailrecursion.priority-map :refer [priority-map]]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]))

(defprotocol PBox
  (layout-bounds [this]
    "Returns `{:x :y :w :h}` defining bounding box from top left."))

(defprotocol PArrayLayout
  (origin-px-topleft [this dt]
    "Returns [x y] pixel coordinates for top left of time offset dt.")
  (local-dt-bounds [this dt]
    "Returns `{:x :y :w :h}` defining local bounding box relative to dt origin.")
  (local-px-topleft [this id]
    "Returns [x y] pixel coordinates for id relative to the dt origin.")
  (element-size-px [this]
    "The size [w h] in pixels of each drawn array element.")
  (scroll-position [this]
    "Current scroll position, giving the first element index visible on screen.")
  (scroll [this down?]
    "Updates the layout with scroll position adjusted up or down one page.")
  (ids-count [_]
    "Returns the total number of ids")
  (ids-onscreen-count [this]
    "Returns the number of ids onscreen in constant time")
  (ids-onscreen [this]
    "Sequence of element ids per timestep currently drawn in the layout.")
  (id-onscreen? [this id]
    "Checks whether the element id is currently drawn in the layout.")
  (clicked-id [this x y]
    "Returns [dt id] for the [x y] pixel coordinates, or null.")
  (draw-element [this ctx id]
    "Draws the element in dt-local coordinates. Does not stroke or fill."))

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

(defn fill-elements
  "Fills all elements with the given ids (in a single path if
  possible). For efficiency, skips any ids which are currently
  offscreen."
  [lay ctx ids]
  (let [one-d? (== 1 (count (p/dims-of lay)))]
    (c/begin-path ctx)
    (doseq [id ids
            :when (id-onscreen? lay id)]
      (draw-element lay ctx id)
      (when-not one-d? ;; single path only works in 1D
        (c/fill ctx)
        (c/begin-path ctx)))
    (when one-d?
      (c/fill ctx))
    ctx))

(defn group-and-fill-elements
  "Groups the map `id-styles` by key, each key being a style value.
   For each such group, calls `set-style` with the value and then
   fills the group of elements."
  [lay ctx id-styles set-style]
  (c/save ctx)
  (doseq [[style ids] (group-by id-styles (keys id-styles))]
    (set-style ctx style)
    (fill-elements lay ctx ids)
    (c/fill ctx))
  (c/restore ctx)
  ctx)

(defn circle
  [ctx x y r]
  (.arc ctx x y r 0 (* (.-PI js/Math) 2) true))

(defn circle-from-bounds
  [ctx x y w]
  (let [r (* w 0.5)]
    (circle ctx (+ x r) (+ y r) r)))

(def extra-px-for-highlight 4)

(defn highlight-rect
  [ctx rect color]
  (doto ctx
    (c/stroke-style color)
    (c/stroke-width 3)
    (c/stroke-rect rect)
    (c/stroke-style "black")
    (c/stroke-width 0.75)
    (c/stroke-rect rect)))

(defn highlight-layer
  "Draws highlight around the whole layout in global coordinates."
  [lay ctx color]
  (let [bb (layout-bounds lay)
        scroll-off (if (pos? (scroll-position lay)) 50 0)]
    (highlight-rect ctx {:x (- (:x bb) 1)
                         :y (- (:y bb) 1 scroll-off)
                         :w (+ (:w bb) 2)
                         :h (+ (:h bb) 2 scroll-off)}
                    color)))

(defn highlight-dt
  "Draws highlight on the time offset dt in global coordinates."
  [lay ctx dt color]
  (let [[x y] (origin-px-topleft lay dt)
        bb (local-dt-bounds lay dt)
        scroll-off (if (pos? (scroll-position lay)) 50 0)]
    (highlight-rect ctx {:x (- x 0)
                         :y (- y 1 scroll-off)
                         :w (+ (:w bb) 0)
                         :h (+ (:h bb) 2 scroll-off)}
                    color)))

(defn highlight-element
  "Draw highlight bar horizontally to left axis from element."
  [lay ctx dt id label color]
  (let [[x y] (origin-px-topleft lay dt)
        [lx ly] (local-px-topleft lay id)
        bb (layout-bounds lay)
        [element-w element-h] (element-size-px lay)
        rect {:x (- (:x bb) 1)
              :y (- (+ y ly) 1)
              :w (+ (- x (:x bb)) lx element-w 2)
              :h (+ element-h 2)}]
    (highlight-rect ctx rect color)
    (when label
      (c/save ctx)
      (c/text-align ctx :right)
      (c/text-baseline ctx :middle)
      (c/fill-style ctx "black")
      (c/text ctx {:x (- (:x rect) 1)
                   :y (+ (:y rect) (/ element-h 2))
                   :text label})
      (c/restore ctx))))

(defrecord Grid1dLayout
    [topo
     scroll-top
     dt-offset
     draw-steps
     element-w
     element-h
     shrink
     left-px
     top-px
     max-bottom-px
     circles?]
  p/PTopological
  (topology [_]
    topo)
  PBox
  (layout-bounds [_]
    {:x left-px :y top-px
     :w (* draw-steps element-w)
     :h (cond-> (* (p/size topo) element-h)
          max-bottom-px (min (- max-bottom-px top-px)))})
  PArrayLayout
  (origin-px-topleft [_ dt]
    (let [right (+ left-px (* draw-steps element-w))
          off-x-px (* (- (inc dt) dt-offset) element-w)
          x-px (- right off-x-px)]
      [x-px top-px]))

  (local-dt-bounds [this dt]
    (assoc (layout-bounds this)
           :x 0 :y 0
           :w element-w))

  (local-px-topleft [_ id]
    [0 (* (- id scroll-top) element-h)])

  (element-size-px [_]
    [element-w element-h])

  (scroll-position [_]
    scroll-top)

  (scroll [this down?]
    (let [page-n (ids-onscreen-count this)
          n-ids (p/size topo)]
      (assoc this :scroll-top
             (if down?
               (if (< scroll-top (- n-ids page-n))
                 (+ scroll-top page-n)
                 scroll-top)
               (max 0 (- scroll-top page-n))))))

  (ids-count [_]
    (p/size topo))

  (ids-onscreen-count [_]
    (cond-> (p/size topo)
      max-bottom-px (min (quot (- max-bottom-px top-px)
                               element-h))))

  (ids-onscreen [this]
    (let [n0 scroll-top]
      (range n0 (+ n0 (ids-onscreen-count this)))))

  (id-onscreen? [this id]
    (let [n (ids-onscreen-count this)
          n0 scroll-top]
      (and (<= n0 id)
           (< id (+ n0 n)))))

  (clicked-id [this x y]
    (let [right (+ left-px (* draw-steps element-w))
          dt* (Math/floor (/ (- right x) element-w))
          id* (Math/floor (/ (- y top-px) element-h))
          id (+ id* scroll-top)
          dt (+ dt* dt-offset)]
      (when (<= 0 dt* draw-steps)
        (if (id-onscreen? this id)
          [dt id]
          ;; check for click on header
          (when (<= y top-px)
            [dt nil])))))

  (draw-element [this ctx id]
    (let [[x y] (local-px-topleft this id)]
      (if circles?
        (circle-from-bounds ctx x y
                            (* element-w shrink))
        (.rect ctx x y
               (* element-w shrink)
               (* element-h shrink))))))

(defn grid-1d-layout
  [topo top left opts inbits?]
  (let [{:keys [draw-steps
                max-height-px
                col-d-px
                col-shrink
                bit-w-px
                bit-h-px
                bit-shrink]} opts]
    (map->Grid1dLayout
     {:topo topo
      :scroll-top 0
      :dt-offset 0
      :draw-steps draw-steps
      :element-w (if inbits? bit-w-px col-d-px)
      :element-h (if inbits? bit-h-px col-d-px)
      :shrink (if inbits? bit-shrink col-shrink)
      :top-px top
      :left-px left
      :max-bottom-px (when max-height-px
                       (- max-height-px extra-px-for-highlight))
      :circles? (if inbits? false true)})))

(defrecord Grid2dLayout
    [n-elements
     topo
     scroll-top
     element-w
     element-h
     shrink
     left-px
     top-px
     max-bottom-px
     circles?]
  p/PTopological
  (topology [_]
    topo)
  PBox
  (layout-bounds [_]
    (let [[w h] (p/dimensions topo)]
      {:x left-px :y top-px
       :w (* w element-w)
       :h (cond-> (* h element-h)
            max-bottom-px (min (- max-bottom-px top-px)))}))
  PArrayLayout
  (origin-px-topleft [_ dt]
    [left-px top-px])

  (local-dt-bounds [this dt]
    (assoc (layout-bounds this)
           :x 0 :y 0))

  (local-px-topleft [_ id]
    (let [[x y] (p/coordinates-of-index topo (+ id scroll-top))]
      [(* x element-w)
       (* y element-h)]))

  (element-size-px [_]
    [element-w element-h])

  (scroll-position [_]
    scroll-top)

  (scroll [this down?]
    (let [page-n (ids-onscreen-count this)
          n-ids (p/size topo)]
      (assoc this :scroll-top
             (if down?
               (if (< scroll-top (- n-ids page-n))
                 (+ scroll-top page-n)
                 scroll-top)
               (max 0 (- scroll-top page-n))))))

  (ids-count [_]
    ;; might not fill the w*h rectangle
    n-elements)

  (ids-onscreen-count [_]
    (let [[w h] (p/dimensions topo)]
      (min n-elements
           (* w
              (cond-> h
                max-bottom-px (min (quot (- max-bottom-px top-px)
                                         element-h)))))))

  (ids-onscreen [this]
    (let [n0 scroll-top]
      (range n0 (+ n0 (ids-onscreen-count this)))))

  (id-onscreen? [this id]
    (let [n0 scroll-top]
      (and (<= n0 id)
           (< id (+ n0 (ids-onscreen-count this))))))

  (clicked-id [_ x y]
    (let [[w h] (p/dimensions topo)
          xi (Math/floor (/ (- x left-px) element-w))
          yi (Math/floor (/ (- y top-px) element-h))]
      (when (and (<= 0 xi (dec w))
                 (<= yi (dec h)))
        (if (>= y 0)
          (let [id* (p/index-of-coordinates topo [xi yi])
                id (- id* scroll-top)]
            (when (< id n-elements)
              [0 id]))
          ;; check for click on header
          [0 nil]))))

  (draw-element [this ctx id]
    (let [[x y] (local-px-topleft this id)]
      (if circles?
        (circle-from-bounds ctx x y
                            (* element-w shrink))
        (.rect ctx x y
               (* element-w shrink)
               (* element-h shrink))))))

(defn grid-2d-layout
  [n-elements topo top left opts inbits?]
  (let [{:keys [max-height-px
                col-d-px
                col-shrink
                bit-w-px
                bit-h-px
                bit-shrink]} opts]
    (map->Grid2dLayout
     {:n-elements n-elements
      :topo topo
      :scroll-top 0
      :element-w (if inbits? bit-w-px col-d-px)
      :element-h (if inbits? bit-h-px col-d-px)
      :shrink (if inbits? bit-shrink col-shrink)
      :top-px top
      :left-px left
      :max-bottom-px (when max-height-px
                       (- max-height-px extra-px-for-highlight))
      :circles? (if inbits? false true)})))

(defn grid-layout
  [dims top left opts inbits? display-mode]
  (let [n-elements (reduce * dims)]
    (case display-mode
      :one-d (grid-1d-layout (topology/one-d-topology n-elements)
                             top left opts inbits?)
      :two-d (let [[width height] (case (count dims)
                                    2 dims ;; keep actual topology if possible
                                    1 (let [w (-> (Math/sqrt n-elements)
                                                  Math/ceil
                                                  (min 20))]
                                        [w (-> n-elements
                                               (/ w)
                                               Math/ceil)])
                                    3 (let [[w h d] dims]
                                        [w (* h d)]))]
               (grid-2d-layout n-elements (topology/two-d-topology width height)
                               top left opts inbits?)))))


;;; # Orderable layouts

(defprotocol POrderable
  (reorder [this ordered-ids]))

(defprotocol PTemporalSortable
  (sort-by-recent-activity [this ids-ts])
  (clear-sort [this])
  (add-facet [this ids label])
  (clear-facets [this])
  (draw-facets [this ctx]))

(defrecord OrderableLayout
    ;; `order` is a priority-map. keys are column ids, vals are layout indices.
    ;; `facets` is a vector of [label length] tuples.
    [layout order facets]
  p/PTopological
  (topology [_]
    (p/topology layout))
  PBox
  (layout-bounds [_]
    (layout-bounds layout))
  PArrayLayout
  (local-px-topleft [_ id]
    (let [idx (order id)]
      (local-px-topleft layout idx)))

  (scroll [this down?]
    (update this :layout scroll down?))

  (ids-count [_]
    (ids-count layout))

  (ids-onscreen-count [_]
    (ids-onscreen-count layout))

  (ids-onscreen [_]
    (let [n0 (scroll-position layout)]
      (->> (subseq order >= n0 < (+ n0 (ids-onscreen-count layout)))
           (map key))))

  (id-onscreen? [_ id]
    (let [idx (order id)]
      (id-onscreen? layout idx)))

  (clicked-id [this x y]
    (when-let [[dt idx] (clicked-id layout x y)]
      (if idx
        (let [id (key (first (subseq order >= idx <= idx)))]
          [dt id])
        [dt nil])))

  (draw-element [this ctx id]
    (let [idx (order id)]
      (draw-element layout ctx idx)))

  ;; the rest are identical to underlying layout methods
  (origin-px-topleft [_ dt]
    (origin-px-topleft layout dt))
  (local-dt-bounds [_ dt]
    (local-dt-bounds layout dt))
  (element-size-px [_]
    (element-size-px layout))
  (scroll-position [_]
    (scroll-position layout))

  POrderable
  (reorder [this ordered-ids]
    (assert (= (count ordered-ids) (count order)))
    (assoc this :order
           (apply priority-map (interleave ordered-ids (range)))))

  PTemporalSortable
  (sort-by-recent-activity [this ids-ts]
    (assert (every? set? ids-ts))
    (let [ftotal (reduce + (map second facets))
          faceted (take ftotal (keys order))
          ord-ids (loop [ids-ts ids-ts
                         ord (transient (vec faceted))
                         ord-set (transient (set faceted))]
                    (if-let [ids (first ids-ts)]
                      (let [new-ids (->> (remove ord-set ids)
                                         (sort-by (fn [id]
                                                    (mapv #(boolean (% id))
                                                          ids-ts))))]
                        (recur (next ids-ts)
                               (reduce conj! ord new-ids)
                               (reduce conj! ord-set new-ids)))
                      ;; finished -- complete the order with all column ids
                      (-> (reduce conj! ord
                                  (remove ord-set (range (count order))))
                          (persistent!))))]
      (reorder this ord-ids)))

  (clear-sort [this]
    (let [ftotal (reduce + (map second facets))
          faceted (take ftotal (keys order))
          ord-ids (concat faceted
                          (remove (set faceted)
                                  (range (count order))))]
      (reorder this ord-ids)))

  (add-facet [this ids label]
    (let [ftotal (reduce + (map second facets))
          old-faceted (take ftotal (keys order))
          new-faceted (distinct (concat old-faceted ids))
          new-length (- (count new-faceted) (count old-faceted))]
      (if (zero? new-length)
        this
        (let [ord-ids (concat new-faceted
                              (remove (set ids)
                                      (drop ftotal (keys order))))]
          (-> (reorder this ord-ids)
              (assoc :facets (conj facets [label new-length])))))))

  (clear-facets [this]
    (assoc this :facets []))

  (draw-facets [this ctx]
    (when (seq facets)
      (let [bb (layout-bounds this)
            [x y] (origin-px-topleft this 0)]
        (c/stroke-style ctx "black")
        (c/fill-style ctx "black")
        (c/text-baseline ctx :bottom)
        (reduce (fn [offset [label length]]
                  (let [idx (+ offset length)
                        [lx ly] (local-px-topleft layout idx)
                        y-px (+ y ly)]
                    (c/begin-path ctx)
                    (c/move-to ctx (:x bb) y-px)
                    (c/line-to ctx (+ (:x bb) (:w bb) 16) y-px)
                    (c/stroke ctx)
                    (c/text ctx {:x (+ (:x bb) (:w bb) 3)
                                 :y y-px
                                 :text label})
                    (+ offset length)))
                0
                facets)))))

(defn orderable-layout
  [lay n-ids]
  (let [order (apply priority-map (interleave (range n-ids) (range)))]
    (map->OrderableLayout
     {:layout lay
      :order order
      :facets []})))
