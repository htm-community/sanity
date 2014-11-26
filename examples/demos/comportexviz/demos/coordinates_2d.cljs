(ns comportexviz.demos.coordinates-2d
  (:require [org.nfrac.comportex.demos.coordinates-2d :as demo]
            [comportexviz.main :as main]
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
  (fn [this ctx left-px top-px w-px h-px state]
    (let [{:keys [x y vx vy]} this
          px-scale (/ (- w-px 4)
                      (* 2 max-pos))
          x-px (* x px-scale)
          y-px (* y px-scale)
          max-px (* max-pos px-scale)]
      (c/save ctx)
      (c/translate ctx
                   (+ left-px (* max-pos px-scale) 2)
                   (+ top-px (* max-pos px-scale)))
      (c/stroke-style ctx "black")
      (c/stroke-rect ctx (centred-rect 0 0
                                       (inc (* 2 max-px))
                                       (inc (* 2 max-px))))
      (c/stroke-style ctx "grey")
      (c/begin-path ctx)
      (c/move-to ctx (- max-px) 0)
      (c/line-to ctx max-px 0)
      (c/line-to ctx 0 0)
      (c/line-to ctx 0 (- max-px))
      (c/line-to ctx 0 max-px)
      (c/stroke ctx)
      ;; draw the radius
      (c/fill-style ctx "green")
      (c/alpha ctx 0.25)
      (c/fill-rect ctx (centred-rect x-px y-px
                                     (inc (* 2 demo/radius px-scale))
                                     (inc (* 2 demo/radius px-scale))))
      ;; draw the actual coordinate
      (c/alpha ctx 1.0)
      (c/stroke-style ctx "black")
      (c/fill-style ctx "yellow")
      (draw-arrow ctx {:x x-px :y y-px :angle (Math/atan2 vy vx)})
      (c/restore ctx))))

(defn ^:export reset-world
  []
  (let [draw-fn (draw-coords-fn demo/max-pos)]
    (main/set-world (->> (demo/world)
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw-fn))))))

(defn ^:export set-n-region-model
  [n]
  (with-ui-loading-message
    (main/set-model (demo/n-region-model n))))

(defn ^:export init
  []
  (goog.ui.TabPane. (.getElementById js/document "comportex-tabs"))
  (reset-world))
