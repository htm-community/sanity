(ns comportexviz.demos.mixed-gaps-1d
  (:require [org.nfrac.comportex.demos.mixed-gaps-1d :as demo]
            [comportexviz.main :as main]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [cljs.core.async :as async]
            [goog.ui.TabPane])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(defn draw-patterns-fn
  [patterns]
  (let [x-max (reduce max (map count (vals patterns)))
        y-max (reduce max (mapcat val patterns))
        x-lim [(- 0 1) (+ x-max 1)]
        y-lim [(- 0 1) (+ y-max 1)]]
    (fn [this ctx left-px top-px w-px h-px state]
      (let [[plot-x plot-y] [10 120]
            plot-size {:w (- w-px 20)
                       :h 200}
            plot (plt/xy-plot ctx plot-size x-lim y-lim)]
        (c/save ctx)
        (c/translate ctx left-px top-px)
        ;; draw pattern names and current values
        (doto ctx
          (c/fill-style "black")
          (c/font-style "14px monospace")
          (c/text {:x plot-x :y (quot plot-y 2)
                   :text (or (keys this) "_")})
          (c/text {:x plot-x :y (+ (quot plot-y 2) 20)
                   :text (demo/current-values this)}))
        ;; draw the plot
        (c/translate ctx plot-x plot-y)
        (plt/frame! plot)
        (plt/grid! plot {})
        (doseq [[id index] this]
          (plt/line! plot (plt/indexed (patterns id)))
          (doseq [[i y] (plt/indexed (patterns id))]
            (c/fill-style ctx (if (== i index) "red" "lightgrey"))
            (plt/point! plot i y 4)))
        (c/restore ctx)))))

(defn ^:export reset-world
  []
  (let [draw (draw-patterns-fn demo/patterns)]
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
