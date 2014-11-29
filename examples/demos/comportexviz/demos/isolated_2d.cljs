(ns comportexviz.demos.isolated-2d
  (:require [org.nfrac.comportex.demos.isolated-2d :as demo]
            [comportexviz.main :as main]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [cljs.core.async :as async]
            [goog.ui.TabPane])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(defn draw-pattern-fn
  [patterns]
  (let [x-max (reduce max (map first (mapcat val patterns)))
        y-max (reduce max (map second (mapcat val patterns)))
        x-lim [(- 0 1) (+ x-max 1)]
        y-lim [(- 0 1) (+ y-max 1)]]
    (fn [this ctx left-px top-px w-px h-px state]
      (let [[plot-x plot-y] [10 120]
            plot-size {:w (- w-px 20)
                       :h (- w-px 20)}
            plot (plt/xy-plot ctx plot-size x-lim y-lim)
            {:keys [id index]} this]
        (c/save ctx)
        ;; draw pattern name and current value
        (doto ctx
          (c/fill-style "black")
          (c/font-style "14px monospace")
          (c/text {:x plot-x :y (quot plot-y 2)
                   :text (if id (name id) "_")})
          (c/text {:x plot-x :y (+ (quot plot-y 2) 20)
                   :text (if id (get-in patterns [id index]) "")}))
        ;; draw the plot
        (c/translate ctx plot-x plot-y)
        (plt/frame! plot)
        (plt/grid! plot {})
        (when id
          (plt/line! plot (patterns id))
          (doseq [[i [x y]] (plt/indexed (patterns id))]
            (c/fill-style ctx (if (== i index) "red" "lightgrey"))
            (plt/point! plot x y 4)))
        (c/restore ctx)))))

(defn ^:export reset-world
  []
  (let [draw (draw-pattern-fn demo/patterns)]
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
