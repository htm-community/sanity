(ns comportexviz.demos.fixed-seqs
  (:require [org.nfrac.comportex.demos.directional-steps-1d :as demo-dir]
            [org.nfrac.comportex.demos.isolated-1d :as demo-i1d]
            [org.nfrac.comportex.demos.mixed-gaps-1d :as demo-mix]
            [org.nfrac.comportex.demos.isolated-2d :as demo-i2d]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [comportexviz.main :as main]
            [comportexviz.viz-canvas :as viz]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [cljs.reader]
            [cljs.core.async :as async]
            [goog.ui.TabPane])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(defn current-values
  [currs patterns]
  (map (fn [[id index]]
         (get-in patterns [id index]))
       currs))

(defn draw-pattern-fn
  [patterns mixed? xy?]
  (let [patterns-xy (if xy?
                      patterns
                      (util/remap plt/indexed patterns))
        x-max (reduce max (map first (mapcat val patterns-xy)))
        y-max (reduce max (map second (mapcat val patterns-xy)))
        x-lim [(- 0 1) (+ x-max 1)]
        y-lim [(- 0 1) (+ y-max 1)]]
    (fn [this ctx left-px top-px w-px h-px state]
      (let [[plot-x plot-y] [10 120]
            plot-size {:w (- w-px 20)
                       :h 200}
            plot (plt/xy-plot ctx plot-size x-lim y-lim)
            curr-patts (if mixed?
                         this
                         (when (:id this)
                           [[(:id this) (:index this)]]))
            label (if mixed?
                    (keys curr-patts)
                    (if (seq curr-patts) (name (ffirst curr-patts))))]
        (c/save ctx)
        (c/translate ctx left-px top-px)
        ;; draw pattern name and current value
        (doto ctx
          (c/fill-style "black")
          (c/font-style "14px monospace")
          (c/text {:x plot-x :y (quot plot-y 2)
                   :text (or label "_")})
          (c/text {:x plot-x :y (+ (quot plot-y 2) 20)
                   :text (current-values curr-patts patterns)}))
        ;; draw the plot
        (c/translate ctx plot-x plot-y)
        (plt/frame! plot)
        (c/stroke-style ctx "lightgray")
        (plt/grid! plot {})
        (c/stroke-style ctx "black")
        (doseq [[id index] curr-patts]
          (plt/line! plot (patterns-xy id))
          (doseq [[i [x y]] (plt/indexed (patterns-xy id))]
            (c/fill-style ctx (if (== i index) "red" "lightgrey"))
            (plt/point! plot x y 4)))
        (c/restore ctx)))))

(defn draw-directional-steps-fn
  [numb-max]
  (let [patterns {:dir [[0 1] [0 -1]]
                  :pos (plt/indexed (range (inc numb-max)))}
        draw (draw-pattern-fn patterns true true)]
    (fn [this ctx left-px top-px w-px h-px state]
      (let [[dir i] this]
        (draw {:dir (case dir :up 0 :down 1)
               :pos i}
              ctx left-px top-px w-px h-px state)))))

(defn set-world
  [world-seq-fn patterns mixed? xy?]
  (let [draw (if patterns
               (draw-pattern-fn patterns mixed? xy?)
               (draw-directional-steps-fn demo-dir/numb-max))
        world-c (async/chan)]
    (main/set-world (->> world-c
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))
    (async/onto-chan world-c (world-seq-fn) false)))

(defn restart-from-ui
  []
  (let [inp-choice (dom/val (->dom "#comportex-input-source"))
        enc-choice (dom/val (->dom "#comportex-encoder"))
        ;; TODO
        n-regions (cljs.reader/read-string
                   (dom/val (->dom "#comportex-n-regions")))
        [model-fn world-fn patterns mixed? xy?]
        (case inp-choice
          "directional-steps-1d"
          [demo-dir/n-region-model demo-dir/world-seq nil false false]
          "isolated-1d"
          [demo-i1d/n-region-model demo-i1d/world-seq demo-i1d/patterns false false]
          "mixed-gaps-1d"
          [demo-mix/n-region-model demo-mix/world-seq demo-mix/patterns true false]
          "isolated-2d"
          [demo-i2d/n-region-model demo-i2d/world-seq demo-i2d/patterns false true])]
    (set-world world-fn patterns mixed? xy?)
    (swap! viz/viz-options assoc-in [:drawing :force-d] (if xy? 2 1))
    (with-ui-loading-message
      (main/set-model
       (model-fn n-regions)))))

(defn ^:export init
  []
  (goog.ui.TabPane. (.getElementById js/document "comportex-tabs"))
  (let [form-el (->dom "#comportex-input-form")]
    (event/on-raw form-el :submit
                  (fn [e]
                    (restart-from-ui)
                    (.preventDefault e)
                    false))))
