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

(def model-info
  {:directional-steps-1d {:model-fn demo-dir/n-region-model
                          :world-fn demo-dir/world-seq
                          :patterns {:dir [[0 1] [0 -1]]
                                     :pos (plt/indexed (range (inc demo-dir/numb-max)))}
                          :mixed? true
                          :xy? true}
   :isolated-1d {:model-fn demo-i1d/n-region-model
                 :world-fn demo-i1d/world-seq
                 :patterns demo-i1d/patterns
                 :mixed? false
                 :xy? false}
   :mixed-gaps-1d {:model-fn demo-mix/n-region-model
                   :world-fn demo-mix/world-seq
                   :patterns demo-mix/patterns
                   :mixed? true
                   :xy? false}
   :isolated-2d {:model-fn demo-i2d/n-region-model
                 :world-fn demo-i2d/world-seq
                 :patterns demo-i2d/patterns
                 :mixed? false
                 :xy? true}})

(defn draw-world
  "Currs should be a map from patt-id to index (current position in the pattern)."
  [ctx currs patterns xy?]
  (let [patterns-xy (if xy?
                      patterns
                      (util/remap plt/indexed patterns))
        x-max (reduce max (map first (mapcat val patterns-xy)))
        y-max (reduce max (map second (mapcat val patterns-xy)))
        x-lim [(- 0 1) (+ x-max 1)]
        y-lim [(- 0 1) (+ y-max 1)]
        width-px (.-width (.-canvas ctx))
        height-px (.-height (.-canvas ctx))
        plot-size {:w width-px
                   :h 200}
        plot (plt/xy-plot ctx plot-size x-lim y-lim)]
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    (plt/frame! plot)
    (c/stroke-style ctx "lightgray")
    (plt/grid! plot {})
    (c/stroke-style ctx "black")
    (doseq [[patt-id index] currs]
      (plt/line! plot (patterns-xy patt-id))
      (doseq [[i [x y]] (plt/indexed (patterns-xy patt-id))]
        (c/fill-style ctx (if (== i index) "red" "lightgrey"))
        (plt/point! plot x y 4)))))

;; fluid canvas - resizes drawing area as element stretches

(def trigger-redraw (atom 0))

(defn on-resize
  [_]
  (let [el (dom/getElement "comportex-world")]
    (viz/set-canvas-pixels-from-element-size! el 160)
    (swap! trigger-redraw inc)))

(defn pattern-index-map
  [in-value mixed? model-id]
  (case model-id
    :directional-steps-1d
    (let [[dir i] in-value]
      {:dir (case dir :up 0 :down 1)
       :pos i})
    ;; default case
    (if mixed?
      in-value
      (when (:id in-value)
        {(:id in-value) (:index in-value)}))
    ))

(defn world-pane
  []
  (when-let [htm (viz/selected-model-step)]
    (let [in-value (:value (first (core/input-seq htm)))
          model-id (::model-id (meta in-value))
          {:keys [patterns mixed? xy?]} (model-info model-id)
          currs (pattern-index-map in-value mixed? model-id)
          canvas (dom/getElement "comportex-world")]
      (when canvas
        (when (zero? @trigger-redraw)
          (on-resize nil))
        (let [ctx (c/get-context canvas "2d")]
          (draw-world ctx currs patterns xy?)
          ))
      [:div
       [:p.muted [:small "Input on selected timestep."]]
       [:table.table
        [:tbody
         [:tr
          [:th "pattern"]
          [:th "value"]]
         (for [[patt-id v] currs]
           ^{:key (str patt-id v)}
           [:tr
            [:td (str patt-id)]
            [:td v
             (when (and (= model-id :directional-steps-1d)
                        (= patt-id :dir))
               (str " (" (first in-value) ")"))]])]]
       [:canvas#comportex-world {:style {:width "100%"
                                         :height "300px"}}]])))

(defn make-world-chan
  [world-seq-fn model-id]
  (let [world-c (async/chan (async/buffer 1)
                            (map #(vary-meta % assoc ::model-id model-id)))]
    (async/onto-chan world-c (world-seq-fn) false)
    world-c))

(defn set-model!
  []
  (let [n-regions (:n-regions @config)
        model-id (:input-stream @config)
        {:keys [model-fn world-fn xy?]} (model-info model-id)]
    (async/close! @main/world)
    (swap! viz/viz-options assoc-in [:drawing :display-mode]
           (if (= model-id :isolated-2d) :two-d :one-d))
    (with-ui-loading-message
      (main/set-model! (model-fn n-regions))
      (reset! main/world (make-world-chan world-fn (:input-stream @config)))
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
  (reagent/render (main/comportexviz-app model-tab world-pane)
                  (dom/getElement "comportexviz-app"))
  (.addEventListener js/window "resize" on-resize)
  (swap! main/main-options assoc :sim-go? true))
