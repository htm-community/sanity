(ns comportexviz.viz-layouts
  (:require [monet.canvas :as c]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]))

(defprotocol PArrayLayout
  (layout-bounds [this]
    "Returns `{:x :y :w :h}` defining bounding box from top left.")
  (origin-px-topleft [this dt]
    "Returns [x y] pixel coordinates for top left of time offset dt.")
  (local-dt-bounds [this dt]
    "Returns `{:x :y :w :h}` defining local bounding box relative to dt origin.")
  (local-px-topleft [this id]
    "Returns [x y] pixel coordinates for id relative to the dt origin.")
  (element-size-px [this]
    "The size [w h] in pixels of each drawn array element.")
  (ids-onscreen [this]
    "Sequence of element ids per timestep currently drawn in the layout.")
  (id-onscreen? [this id]
    "Checks whether the element id is currently drawn in the layout.")
  (scroll-position [this]
    "Current scroll position, giving the first element index visible on screen.")
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

(defn fill-element-group
  "Fills all elements with the given ids (in a single path if
  possible). For efficiency, skips any ids which are currently
  offscreen."
  [ctx lay ids]
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

(defn fill-elements
  "Groups the map `id-styles` by key, each key being a style value.
   For each such group, calls `set-style` with the value and then
   fills the group of elements."
  [ctx lay id-styles set-style]
  (c/save ctx)
  (doseq [[style ids] (group-by id-styles (keys id-styles))]
    (set-style ctx style)
    (fill-element-group ctx lay ids)
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
  [lay ctx]
  (let [bb (layout-bounds lay)
        color (:highlight-color lay)]
    (highlight-rect ctx {:x (- (:x bb) 1)
                         :y (- (:y bb) 1)
                         :w (+ (:w bb) 2)
                         :h (+ (:h bb) 2)}
                    color)))

(defn highlight-dt
  "Draws highlight on the time offset dt in global coordinates. Only
  draws if multiple time steps are shown."
  [lay ctx dt]
  (when (> (:draw-steps lay 1) 1)
    (let [[x y] (origin-px-topleft lay dt)
          bb (local-dt-bounds lay dt)
          color (:highlight-color lay)]
      (highlight-rect ctx {:x (- x 1)
                           :y (- y 1)
                           :w (+ (:w bb) 2)
                           :h (+ (:h bb) 2)}
                      color))))

(defn highlight-element
  "Draw highlight bar horizontally to left axis from element."
  [lay ctx dt id label]
  (let [[x y] (origin-px-topleft lay dt)
        [lx ly] (local-px-topleft lay id)
        bb (layout-bounds lay)
        [element-w element-h] (element-size-px lay)
        color (:highlight-color lay)
        rect {:x (- (:x bb) 1)
              :y (- (+ y ly) 1)
              :w (+ (- x (:x bb)) lx element-w 2)
              :h (+ element-h 2)}]
    (highlight-rect ctx rect
                    color)
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
     height-px
     circles?
     highlight-color]
  p/PTopological
  (topology [_]
    topo)
  PArrayLayout
  (layout-bounds [_]
    {:x left-px :y top-px
     :w (* draw-steps element-w)
     :h (min height-px (* (p/size topo) element-h))})

  (origin-px-topleft [_ dt]
    (let [right (+ left-px (* draw-steps element-w))
          off-x-px (* (- (inc dt) dt-offset) element-w)
          x-px (- right off-x-px)]
      [x-px top-px]))

  (local-dt-bounds [lay dt]
    (assoc (layout-bounds lay)
           :x 0 :y 0
           :w element-w))

  (local-px-topleft [_ id]
    [0 (* (- id scroll-top) element-h)])

  (element-size-px [_]
    [element-w element-h])

  (scroll-position [_]
    scroll-top)

  (ids-onscreen [this]
    (let [n (min (p/size topo)
                 (quot height-px element-h))
          n0 scroll-top]
      (range n0 (+ n0 n))))

  (id-onscreen? [this id]
    (let [n (min (p/size topo)
                 (quot height-px element-h))
          n0 scroll-top]
      (<= n0 id (+ n0 n -1))))

  (clicked-id [this x y]
    (let [right (+ left-px (* draw-steps element-w))
          dt* (Math/floor (/ (- right x) element-w))
          id* (Math/floor (/ (- y top-px) element-h))
          id (+ id* scroll-top)
          dt (+ dt* dt-offset)]
      (when (and (<= 0 dt* draw-steps)
                 (id-onscreen? this id))
        [dt id])))

  (draw-element [this ctx id]
    (let [[x y] (local-px-topleft this id)]
      (if circles?
        (circle-from-bounds ctx x y
                            (* element-w shrink))
        (.rect ctx x y
               (* element-w shrink)
               (* element-h shrink))))))

(defn grid-1d-layout
  [topo top left height opts inbits?]
  (let [{:keys [draw-steps
                col-d-px
                col-shrink
                bit-w-px
                bit-h-px
                bit-shrink
                highlight-color]} opts]
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
      :height-px height
      :circles? (if inbits? false true)
      :highlight-color highlight-color})))

(defrecord Grid2dLayout
    [topo
     scroll-top
     element-w
     element-h
     shrink
     left-px
     top-px
     height-px
     circles?
     highlight-color]
  p/PTopological
  (topology [_]
    topo)
  PArrayLayout
  (layout-bounds [_]
    (let [[w h] (p/dimensions topo)]
      {:x left-px :y top-px
       :w (* w element-w)
       :h (min height-px (* h element-h))}))

  (origin-px-topleft [_ dt]
    [left-px top-px])

  (local-dt-bounds [lay dt]
    (assoc (layout-bounds lay)
           :x 0 :y 0))

  (local-px-topleft [_ id]
    (let [[x y] (p/coordinates-of-index topo (+ id scroll-top))]
      [(* x element-w)
       (* y element-h)]))

  (element-size-px [_]
    [element-w element-h])

  (scroll-position [_]
    scroll-top)

  (ids-onscreen [this]
    (let [[w h] (p/dimensions topo)
          n (* w (min h (quot height-px element-h)))
          n0 scroll-top]
      (range n0 (+ n0 n -1))))

  (id-onscreen? [this id]
    (let [[w h] (p/dimensions topo)
          n (* w (min h (quot height-px element-h)))
          n0 scroll-top]
      (<= n0 id (+ n0 n -1))))

  (clicked-id [this x y]
    (let [[w h] (p/dimensions topo)
          xi (Math/floor (/ (- x left-px) element-w))
          yi (Math/floor (/ (- y top-px) element-h))]
      (when (and (<= 0 xi (dec w))
                 (<= 0 yi (dec h)))
        (let [id* (p/index-of-coordinates topo [xi yi])
              id (- id* scroll-top)]
          [0 id]))))

  (draw-element [this ctx id]
    (let [[x y] (local-px-topleft this id)]
      (if circles?
        (circle-from-bounds ctx x y
                            (* element-w shrink))
        (.rect ctx x y
               (* element-w shrink)
               (* element-h shrink))))))

(defn grid-2d-layout
  [topo top left height opts inbits?]
  (let [{:keys [col-d-px
                col-shrink
                bit-w-px
                bit-h-px
                bit-shrink
                highlight-color]} opts]
    (map->Grid2dLayout
     {:topo topo
      :scroll-top 0
      :element-w (if inbits? bit-w-px col-d-px)
      :element-h (if inbits? bit-h-px col-d-px)
      :shrink (if inbits? bit-shrink col-shrink)
      :top-px top
      :left-px left
      :height-px height
      :circles? (if inbits? false true)
      :highlight-color highlight-color})))

(defn grid-layout
  [topo top left height opts inbits? display-mode]
  (let [ndim (count (p/dimensions topo))
        lay-topo (case display-mode
                   :one-d (topology/one-d-topology (p/size topo))
                   :two-d (if (== 2 ndim)
                            topo ;; keep actual topology if possible
                            (topology/two-d-topology 20 (quot (p/size topo) 20))))
        lay-ndim (count (p/dimensions lay-topo))]
    (case lay-ndim
      1 (grid-1d-layout lay-topo top left height opts inbits?)
      2 (grid-2d-layout lay-topo top left height opts inbits?))))
