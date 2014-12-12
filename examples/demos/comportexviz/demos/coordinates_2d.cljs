(ns comportexviz.demos.coordinates-2d
  (:require [org.nfrac.comportex.demos.coordinates-2d :as demo]
            [comportexviz.main :as main]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [goog.ui.TabPane]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(defn draw-arrow
  [ctx {:keys [x y angle]}]
  (c/save ctx)
  (c/translate ctx x y)
  (c/rotate ctx angle)
  (c/begin-path ctx)
  (c/move-to ctx 5 0)
  (c/line-to ctx -5 3)
  (c/line-to ctx -5 -3)
  (c/line-to ctx 5 0)
  (c/fill ctx)
  (c/stroke ctx)
  (c/restore ctx))

(defn centred-rect
  [cx cy w h]
  {:x (- cx (/ w 2))
   :y (- cy (/ h 2))
   :w w
   :h h})

(defn draw-coords-fn
  [max-pos]
  (let [x-lim [(- max-pos) max-pos]
        y-lim [(- max-pos) max-pos]]
    (fn [this ctx left-px top-px w-px h-px state]
      (let [[plot-x plot-y] [10 100]
            plot-size {:w (- w-px 20)
                       :h (- w-px 20)}
            plot (plt/xy-plot ctx plot-size x-lim y-lim)
            x-scale (plt/scale-fn x-lim (:w plot-size))
            y-scale (plt/scale-fn y-lim (:h plot-size))
            {:keys [x y vx vy]} this
            r-px (- (x-scale demo/radius) (x-scale 0))]
        (c/save ctx)
        (c/translate ctx left-px top-px)
        ;; draw coordinates text
        (doto ctx
          (c/fill-style "black")
          (c/font-style "14px monospace")
          (c/text {:x plot-x :y (quot plot-y 2)
                   :text (str "(" x "," y ")")}))
        ;; draw the plot
        (c/translate ctx plot-x plot-y)
        (plt/frame! plot)
        (plt/grid! plot {:grid-every 2})
        (doto ctx
          ;; draw the radius
          (c/fill-style "rgba(255,0,0,0.25)")
          (c/fill-rect (centred-rect (x-scale x) (y-scale y)
                                     (* 2 r-px) (* 2 r-px)))
          ;; draw the current coordinate
          (c/stroke-style "black")
          (c/fill-style "yellow")
          (draw-arrow {:x (x-scale x) :y (y-scale y)
                       :angle (Math/atan2 vy vx)})))
      (c/restore ctx))))

(defn ^:export reset-world
  []
  (let [draw (draw-coords-fn demo/max-pos)]
    (main/set-world (->> (demo/world)
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))))

(defn ^:export set-n-region-model
  [n]
  (with-ui-loading-message
    (main/set-model (demo/n-region-model n))))

(defn ^:export init
  []
  (goog.ui.TabPane. (.getElementById js/document "comportex-tabs"))
  (reset-world))
