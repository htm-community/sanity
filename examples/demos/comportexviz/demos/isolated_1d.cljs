(ns comportexviz.demos.isolated-1d
  (:require [org.nfrac.comportex.demos.isolated-1d :as demo]
            [org.nfrac.comportex.core :as core]
            [comportexviz.main :as main]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [cljs.reader]
            [cljs.core.async :as async]
            [goog.ui.TabPane])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(defn draw-pattern-fn
  [patterns]
  (let [x-max (reduce max (map count (vals patterns)))
        y-max (reduce max (mapcat val patterns))
        x-lim [(- 0 1) (+ x-max 1)]
        y-lim [(- 0 1) (+ y-max 1)]]
    (fn [this ctx left-px top-px w-px h-px state]
      (let [[plot-x plot-y] [10 120]
            plot-size {:w (- w-px 20)
                       :h 200}
            plot (plt/xy-plot ctx plot-size x-lim y-lim)
            {:keys [id index]} this]
        (c/save ctx)
        (c/translate ctx left-px top-px)
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
        (c/stroke-style ctx "lightgray")
        (plt/grid! plot {})
        (c/stroke-style ctx "black")
        (when id
          (plt/line! plot (plt/indexed (patterns id)))
          (doseq [[i y] (plt/indexed (patterns id))]
            (c/fill-style ctx (if (== i index) "red" "lightgrey"))
            (plt/point! plot i y 4)))
        (c/restore ctx)))))

(def world-c (async/chan))

(defn set-world
  []
  (let [draw (draw-pattern-fn demo/patterns)]
    (main/set-world (->> world-c
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))
    (async/onto-chan world-c (demo/world-seq) false)))

(defn restart-from-ui
  []
  (let [enc-choice (dom/val (->dom "#comportex-encoder"))
        n-regions (cljs.reader/read-string
                   (dom/val (->dom "#comportex-n-regions")))
        encoder (case enc-choice
                  "block" demo/block-encoder
                  "coord" demo/coord-encoder)]
    (with-ui-loading-message
      (main/set-model
       (core/regions-in-series core/sensory-region (core/sensory-input encoder)
                               n-regions demo/spec)))))

(defn ^:export init
  []
  (goog.ui.TabPane. (.getElementById js/document "comportex-tabs"))
  (let [form-el (->dom "#comportex-input-text-form")]
    (event/on-raw form-el :submit
                  (fn [e]
                    (restart-from-ui)
                    (.preventDefault e)
                    false)))
  (set-world))
