(ns comportexviz.viz-layouts
  (:require [monet.canvas :as c]
            [org.nfrac.comportex.protocols :as p]))

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

(defn fill-element-group
  "Fills all elements with the given ids (in a single path if
   possible). For efficiency, skips any ids which are currently
   offscreen."
  [ctx lay ids]
  (let [j0 (top-id-onscreen lay)
        j1 (+ j0 (n-onscreen lay) -1)
        one-d? (== 1 (count (p/dims-of lay)))]
    (c/begin-path ctx)
    (doseq [i ids
           :when (<= j0 i j1)]
      (draw-element lay ctx i)
      (when-not one-d?
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
  [ctx rect color]
  (doto ctx
    (c/stroke-style color)
    (c/stroke-width 3)
    (c/stroke-rect rect)
    (c/stroke-style "black")
    (c/stroke-width 1)
    (c/stroke-rect rect)))

(defrecord Grid1dLayout
    [topo
     scroll-top
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
          off-x-px (* (+ dt 1) element-w)
          x-px (- right off-x-px)]
      [x-px top-px]))
  
  (local-px-topleft [_ id]
    [0 (* (- id scroll-top) element-h)])
  
  (element-size-px [_]
    [element-w element-h])
  
  (n-onscreen [_]
    (min (p/size topo)
         (/ (- height-px top-px) element-h)))
  
  (top-id-onscreen [_]
    scroll-top)
  
  (clicked-id [this x y]
    (let [right (+ left-px (* draw-steps element-w))
          dt (Math/floor (/ (- right x) element-w))
          id* (Math/floor (/ (- y top-px) element-h))
          id (+ id* scroll-top)]
      (when (and (<= 0 dt draw-steps)
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
                           :h (+ 10 (:h bb))}
                      highlight-color)))
  
  (highlight-element [this ctx dt id]
    ;; draw horizontal axis on selected id
    (let [[x y] (origin-px-topleft this dt)
          [lx ly] (local-px-topleft this id)
          bb (layout-bounds this)]
      (highlight-rect ctx {:x (- (:x bb) 5)
                           :y (+ y ly)
                           :w (+ (:w bb) 10)
                           :h (+ element-h 1)}
                      highlight-color))))

(defn inbits-1d-layout
  [topo top left height opts]
  (let [{:keys [draw-steps
                bit-w-px
                bit-h-px
                bit-shrink
                highlight-color]} opts]
    (map->Grid1dLayout
     {:topo topo
      :scroll-top 0
      :draw-steps draw-steps
      :element-w bit-w-px
      :element-h bit-h-px
      :shrink bit-shrink
      :left-px left
      :top-px top
      :height-px height
      :circles? false
      :highlight-color highlight-color})))

(defn columns-1d-layout
  [topo top left height opts]
  (let [{:keys [draw-steps
                col-d-px
                col-shrink
                highlight-color]} opts]
    (map->Grid1dLayout
     {:topo topo
      :scroll-top 0
      :draw-steps draw-steps
      :element-w col-d-px
      :element-h col-d-px
      :shrink col-shrink
      :top-px top
      :left-px left
      :height-px height
      :circles? true
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
  
  (local-px-topleft [_ id]
    (let [[x y] (p/coordinates-of-index topo (+ id scroll-top))]
      [(* x element-w)
       (* y element-h)]))
  
  (element-size-px [_]
    [element-w element-h])
  
  (n-onscreen [_]
    (let [[w h] (p/dimensions topo)]
      (* w (min h
                (/ (- height-px top-px) element-h)))))
  
  (top-id-onscreen [_]
    scroll-top)
  
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
               (* element-h shrink)))))
  
  (highlight-dt [this ctx dt]
    ;; draw vertical axis on selected dt
    (let [[x y] (origin-px-topleft this dt)
          bb (layout-bounds this)]
      (highlight-rect ctx {:x (- x 5)
                           :y (- y 5)
                           :w (+ 10 (:w bb))
                           :h (+ 10 (:h bb))}
                      highlight-color)))
  
  (highlight-element [this ctx dt id]
    ;; draw horizontal axis on selected id
    (let [[x y] (origin-px-topleft this dt)
          [lx ly] (local-px-topleft this id)
          bb (layout-bounds this)]
      (highlight-rect ctx {:x (+ x lx)
                           :y (+ y ly)
                           :w (+ element-w 1)
                           :h (+ element-h 1)}
                      highlight-color))))

(defn inbits-2d-layout
  [topo top left height opts]
  (let [{:keys [bit-w-px
                bit-h-px
                bit-shrink
                highlight-color]} opts]
    (map->Grid2dLayout
     {:topo topo
      :scroll-top 0
      :element-w bit-w-px
      :element-h bit-h-px
      :shrink bit-shrink
      :left-px left
      :top-px top
      :height-px height
      :circles? false
      :highlight-color highlight-color})))

(defn columns-2d-layout
  [topo top left height opts]
  (let [{:keys [col-d-px
                col-shrink
                highlight-color]} opts]
    (map->Grid2dLayout
     {:topo topo
      :scroll-top 0
      :element-w col-d-px
      :element-h col-d-px
      :shrink col-shrink
      :top-px top
      :left-px left
      :height-px height
      :circles? true
      :highlight-color highlight-color})))

(defn make-layout
  [topo top left height opts inbits? & {:keys [force-d]}]
  (let [ndim (or force-d (count (p/dimensions topo)))]
    (if inbits?
      (case ndim
        1 (inbits-1d-layout topo top left height opts)
        2 (inbits-2d-layout topo top left height opts))
      (case ndim
        1 (columns-1d-layout topo top left height opts)
        2 (columns-2d-layout topo top left height opts)))))
