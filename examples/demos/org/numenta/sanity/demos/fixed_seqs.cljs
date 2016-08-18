(ns org.numenta.sanity.demos.fixed-seqs
  (:require [org.nfrac.comportex.demos.isolated-1d :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [org.numenta.sanity.demos.comportex-common :refer [all-features]]
            [org.numenta.sanity.main :as main]
            [org.numenta.sanity.helpers :as helpers :refer [resizing-canvas]]
            [org.numenta.sanity.plots-canvas :as plt]
            [org.numenta.sanity.bridge.browser :as server]
            [org.numenta.sanity.comportex.data :as data]
            [org.numenta.sanity.util :refer [translate-network-shape]]
            [monet.canvas :as c]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [cljs.core.async :as async :refer [put!]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [org.numenta.sanity.macros :refer [with-ui-loading-message]]))

(def config
  (atom {:n-regions 1}))

(def world-c (async/chan (async/buffer 1)
                         (map #(assoc % :label (:id %)))))

(def model (atom nil))

(def into-sim (async/chan))

(defn draw-world
  [ctx inval patterns]
  (let [patterns-xy (util/remap plt/indexed patterns)
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
    (when-let [id (:id inval)]
      (plt/line! plot (patterns-xy id))
      (doseq [[i [x y]] (plt/indexed (patterns-xy id))]
        (c/fill-style ctx (if (== i (:index inval)) "red" "lightgrey"))
        (plt/point! plot x y 4)))))

(defn world-pane
  []
  (when-let [step (main/selected-step)]
    (let [inval (:input-value step)]
      [:div
       [:p.muted [:small "Input on selected timestep."]]
       [:table.table
        [:tbody
         [:tr
          [:th "pattern"]
          [:th "value"]]
         [:tr
          [:td (str (or (:id inval) "-"))]
          [:td (:value inval)]]]]
       [resizing-canvas {:style {:width "100%"
                                 :height "300px"}}
        [main/selection]
        (fn [ctx]
          (let [step (main/selected-step)
                inval (:input-value step)]
            (draw-world ctx inval demo/patterns)))
        nil]])))

(defn set-model!
  []
  (with-ui-loading-message
    (let [init? (nil? @model)]
      (reset! model (demo/n-region-model (:n-regions @config)))
      (if init?
        (server/init model world-c main/into-journal into-sim)
        (reset! main/network-shape (translate-network-shape
                                    (data/network-shape @model))))
      (when init?
        (async/onto-chan world-c (demo/input-seq) false)))))

(def config-template
  [:div.form-horizontal
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
     [:button.btn.btn-default
      {:on-click (fn [e]
                   (set-model!)
                   (.preventDefault e))}
      "Restart with new model"]
     [:p.text-danger "This resets all parameters."]]]

   [:p "The following fixed sequences are presented one at a time with
   a gap of 5 time steps. Each new pattern is chosen randomly. This
   example is designed for testing temporal pooling, as each fixed
   sequence should give rise to a stable representation."
    [:pre
     ":run-0-5   [0 1 2 3 4 5]
:rev-5-1   [5 4 3 2 1]
:run-6-10  [6 7 8 9 10]
:jump-6-12 [6 7 8 11 12]
:twos      [0 2 4 6 8 10 12 14]
:saw-10-15 [10 12 11 13 12 14 13 15]"
     ]]
   ])

(defn model-tab
  []
  [:div
   [:p "Fixed integer patterns repeating in random order."]
   [bind-fields config-template config]
   ])

(defn ^:export init
  []
  (reagent/render [main/sanity-app "Comportex" [model-tab] [world-pane]
                   (atom :model) all-features into-sim]
                  (dom/getElement "sanity-app"))
  (set-model!))
