(ns comportexviz.plots-canvas
  (:require [monet.canvas :as c]))

(defn indexed
  [ys]
  (vec (map-indexed vector ys)))

(defprotocol PPlot
  (bg! [this])
  (frame! [this])
  (grid! [this opts])
  (point! [this x y radius-px])
  (rect! [this x y w h])
  (line! [this xys])
  (->px [this x y]))

(defn draw-grid
  [ctx [x-lo x-hi] [y-lo y-hi] xs ys]
  (c/begin-path ctx)
  (doseq [x xs]
    (c/move-to ctx x y-lo)
    (c/line-to ctx x y-hi))
  (doseq [y ys]
    (c/move-to ctx x-lo y)
    (c/line-to ctx x-hi y))
  (c/stroke ctx))

(defn scale-fn
  [[lo hi] size-px]
  (fn [x]
    (* (- x lo)
       (/ size-px
          (- hi lo)))))

(defrecord XYPlot
    [ctx plot-size x-lim y-lim x-scale y-scale]
  PPlot
  (bg! [_]
    (let [plot-rect (assoc plot-size :x 0 :y 0)]
      (doto ctx
        (c/fill-style "white")
        (c/fill-rect plot-rect))))

  (frame! [_]
    (let [plot-rect (assoc plot-size :x 0 :y 0)]
      (doto ctx
        (c/stroke-style "black")
        (c/stroke-rect plot-rect))))

  (grid! [_ {:keys [grid-every]
             :or {grid-every 1}}]
    (c/save ctx)
    (let [[x-lo x-hi] x-lim
          [y-lo y-hi] y-lim]
      (draw-grid ctx [0 (:w plot-size)] [0 (:h plot-size)]
                 (map x-scale (range (long x-lo) (inc (long x-hi)) grid-every))
                 (map y-scale (range (long y-lo) (inc (long y-hi)) grid-every))))
    (c/restore ctx))

  (point! [_ x y radius-px]
    (doto ctx
      (c/circle {:x (x-scale x)
                 :y (y-scale y)
                 :r radius-px})
      (c/fill)
      (c/stroke)))

  (rect! [_ x y w h]
    (let [xpx (x-scale x)
          ypx (y-scale y)]
      (doto ctx
        (c/fill-rect {:x xpx
                      :y ypx
                      :w (- (x-scale (+ x w)) xpx)
                      :h (- (y-scale (+ y h)) ypx)})
        ;; TODO kill
        (c/stroke))))

  (line! [_ xys]
    (c/begin-path ctx)
    (doseq [[i [x y]] (indexed xys)]
      (let [f (if (zero? i) c/move-to c/line-to)]
        (f ctx (x-scale x) (y-scale y))))
    (c/stroke ctx))

  (->px [_ x y]
    [(x-scale x) (y-scale y)]))

(defn xy-plot
  "Assumes ctx is already translated."
  [ctx {:keys [w h] :as plot-size} x-lim y-lim]
  (let [x-scale (scale-fn x-lim (:w plot-size))
        y-scale (scale-fn y-lim (:h plot-size))]
    (map->XYPlot
     {:ctx ctx
      :plot-size plot-size
      :x-lim x-lim
      :y-lim y-lim
      :x-scale x-scale
      :y-scale y-scale})))
