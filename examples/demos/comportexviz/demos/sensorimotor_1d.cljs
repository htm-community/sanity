(ns comportexviz.demos.sensorimotor-1d
  (:require [org.nfrac.comportex.demos.sensorimotor-1d :as demo]
            [comportexviz.main :as main]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [goog.ui.TabPane]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(def item-colors
  (zipmap demo/items
          (for [hue (range 0 360 70)
                lig [70 30]]
            (str "hsl(" hue ",100%," lig "%)"))))

(defn draw-eye
  [ctx {:keys [x y angle radius]}]
  (c/save ctx)
  (let [pi2 (/ Math/PI 2)]
    (c/begin-path ctx)
    (c/arc ctx {:x x :y y :r radius
                :start-angle (- pi2) :end-angle pi2
                :counter-clockwise? true})
    (c/close-path ctx)
    (c/fill-style ctx "white")
    (c/fill ctx)
    (c/stroke-style ctx "black")
    (c/stroke ctx)
    (c/clip ctx)
    (let [pupil-x (+ x (* radius (Math/cos angle)))
          pupil-y (+ y (* radius (Math/sin angle)))]
      (c/circle ctx {:x pupil-x :y pupil-y
                     :r (quot radius 2)})
      (c/fill-style ctx "rgb(128,128,255)")
      (c/fill ctx)
      (c/circle ctx {:x pupil-x :y pupil-y
                     :r (quot radius 5)})
      (c/fill-style ctx "black")
      (c/fill ctx)))
  (c/restore ctx))

(defn draw-coords-fn
  []
  (let [x-lim [0 1]
        y-lim [0 demo/world-size]]
    (fn [this ctx left-px top-px w-px h-px state]
      (let [[plot-x plot-y] [10 100]
            plot-size {:w (- w-px 20)
                       :h 200}
            plot (plt/xy-plot ctx plot-size x-lim y-lim)
            x-scale (plt/scale-fn x-lim (:w plot-size))
            y-scale (plt/scale-fn y-lim (:h plot-size))
            {:keys [field position next-saccade]} this]
        (c/save ctx)
        (c/translate ctx left-px top-px)
        ;; draw coordinates text
        (doto ctx
          (c/fill-style "black")
          (c/font-style "14px monospace")
          (c/text {:x plot-x :y (quot plot-y 2)
                   :text (str "pos=" position
                              ", next" (if (neg? next-saccade) "" "+")
                              next-saccade)}))
        ;; draw the plot
        (c/translate ctx plot-x plot-y)
        (plt/frame! plot)
        (c/stroke-style ctx "black")
        (doseq [[y item] (map-indexed vector field)
                :let [rect {:x 0 :y (y-scale y)
                            :w 20 :h (y-scale 1)}]]
          (c/fill-style ctx (item-colors item))
          (c/fill-rect ctx rect)
          (c/stroke-rect ctx rect))
        (let [focus-x 10
              focus-y (y-scale (+ 0.5 position))
              next-focus-y (y-scale (+ 0.5 position next-saccade))
              eye-x (:w plot-size)
              eye-y (quot (:h plot-size) 2)]
          ;; draw line of next position (after next saccade)
          (c/begin-path ctx)
          (c/move-to ctx eye-x eye-y)
          (c/line-to ctx focus-x next-focus-y)
          (c/stroke-style ctx "lightgrey")
          (c/stroke ctx)
          ;; draw line of current position
          (c/begin-path ctx)
          (c/move-to ctx eye-x eye-y)
          (c/line-to ctx focus-x focus-y)
          (c/stroke-style ctx "black")
          (c/stroke ctx)
          ;; draw eye
          (draw-eye ctx {:x eye-x
                         :y eye-y
                         :angle (Math/atan2 (- focus-y eye-y)
                                            (- focus-x eye-x))
                         :radius 30})))
      (c/restore ctx))))

(defn ^:export reset-world
  []
  (let [draw (draw-coords-fn)]
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
