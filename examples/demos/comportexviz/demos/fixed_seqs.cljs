(ns comportexviz.demos.fixed-seqs
  (:require [org.nfrac.comportex.demos.directional-steps-1d :as demo-dir]
            [org.nfrac.comportex.demos.isolated-1d :as demo-i1d]
            [org.nfrac.comportex.demos.mixed-gaps-1d :as demo-mix]
            [org.nfrac.comportex.demos.isolated-2d :as demo-i2d]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [comportexviz.main :as main]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [comportexviz.viz-canvas :as viz]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(def config
  (atom {:input-stream :directional-steps-1d
         :encoder :block
         :n-regions 1}))

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

(defn make-world-chan
  [world-seq-fn patterns mixed? xy?]
  (let [draw (if patterns
               (draw-pattern-fn patterns mixed? xy?)
               (draw-directional-steps-fn demo-dir/numb-max))
        world-c (async/chan (async/buffer 1)
                            (map #(vary-meta % assoc
                                             :comportexviz/draw-world
                                             draw)))]
    (async/onto-chan world-c (world-seq-fn) false)
    world-c))

(defn set-model!
  []
  (let [n-regions (:n-regions @config)
        [model-fn world-fn patterns mixed? xy?]
        (case (:input-stream @config)
          :directional-steps-1d
          [demo-dir/n-region-model demo-dir/world-seq nil false false]
          :isolated-1d
          [demo-i1d/n-region-model demo-i1d/world-seq demo-i1d/patterns false false]
          :mixed-gaps-1d
          [demo-mix/n-region-model demo-mix/world-seq demo-mix/patterns true false]
          :isolated-2d
          [demo-i2d/n-region-model demo-i2d/world-seq demo-i2d/patterns false true])]
    (async/close! @main/world)
    (swap! viz/viz-options assoc-in [:drawing :display-mode] (if xy? :two-d :one-d))
    (with-ui-loading-message
      (main/set-model! (model-fn n-regions))
      (reset! main/world (make-world-chan world-fn patterns mixed? xy?))
      )))

(def config-template
  [:div.form-horizontal
   [:div.form-group
    [:label.col-sm-5 "Input stream:"]
    [:div.col-sm-7
     [:select.form-control {:field :list
                            :id :input-stream}
      [:option {:key :directional-steps-1d} "directional-steps-1d"]
      [:option {:key :isolated-1d} "isolated-1d"]
      [:option {:key :mixed-gaps-1d} "mixed-gaps-1d"]
      [:option {:key :isolated-2d} "isolated-2d"]]]]
   [:div.form-group
    [:label.col-sm-5 "Encoder:"]
    [:div.col-sm-7
     [:select.form-control {:field :list
                            :id :encoder
                            :disabled "disabled"}
      [:option {:key :block} "block"]
      [:option {:key :random} "random"]]]]
   [:div.form-group
    [:label.col-sm-5 "Number of regions:"]
    [:div.col-sm-7
     [:input.form-control {:field :numeric
                           :id :n-regions}]]]
   [:div.form-group
    [:div.col-sm-offset-5.col-sm-7
     [:button.btn.btn-primary
      {:on-click (fn [e]
                   (set-model!)
                   (.preventDefault e))}
      "Restart with new model"]
     [:p.text-danger "This resets all parameters."]]]

   [:dl
    [:dt [:code "directional-steps-1d"]]
    [:dd "The input stream steps through integers [0--8] either
              upwards or downwards, chosen randomly. An indicator of
              the next step direction is included as part of the
              input."]
    [:dt [:code "isolated-1d"]]
    [:dd "The following fixed sequences are presented one at a
              time with a gap of 5 time steps. Each new pattern is
              chosen randomly. This example is designed for testing
              temporal pooling, as each fixed sequence should give
              rise to a stable representation."
     [:pre
":run-0-5   [0 1 2 3 4 5]
:rev-5-1   [5 4 3 2 1]
:run-6-10  [6 7 8 9 10]
:jump-6-12 [6 7 8 11 12]
:twos      [0 2 4 6 8 10 12 14]
:saw-10-15 [10 12 11 13 12 14 13 15]"
      ]]
    [:dt [:code "mixed-gaps-1d"]]
    [:dd "The same fixed sequences as above are each repeated
                with random-length gaps and mixed together."]
    [:dt [:code "isolated-2d"]]
    [:dd "The following fixed sequences are presented one at a
              time with a gap of 5 time steps. Each new pattern is
              chosen randomly."
     [:pre
":down-1     [[1 0] [1 1] [1 2] [1 3] [1 4]
             [1 5] [1 6] [1 7] [1 8] [1 9]]
:down-right [[1 0] [1 1] [1 2] [1 3] [1 4]
             [1 5] [3 5] [5 5] [7 5] [9 5]]
:diag-tl-br [[0 0] [1 1] [2 2] [3 3] [4 4]
             [5 5] [6 6] [7 7] [8 8] [9 9]]
:rand-10    [ten random coordinates]"]]
    ]
   ])

(defn model-tab
  []
  [:div
   [:p "These demos are all kinds of fixed sequences."]
   [bind-fields config-template config]
   ])

(defn ^:export init
  []
  (reagent/render (main/comportexviz-app model-tab)
                  (dom/getElement "comportexviz-app"))
  (swap! main/main-options assoc :sim-go? true))
