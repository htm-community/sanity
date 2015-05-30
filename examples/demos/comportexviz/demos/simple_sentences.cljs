(ns comportexviz.demos.simple-sentences
  (:require [org.nfrac.comportex.demos.simple-sentences :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [comportexviz.main :as main]
            [comportexviz.helpers :as helpers]
            [comportexviz.viz-canvas :as viz]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(def config
  (atom {:n-regions 1
         :encoder :block
         :repeats 1
         :text demo/input-text
         :world-buffer-count 0}))

(def world-buffer (async/buffer 5000))
(def world-c
  (async/chan world-buffer
              (map (util/keep-history-middleware 100 :word :history))))

(add-watch main/model ::count-world-buffer
           (fn [_ _ _ _]
             (swap! config assoc :world-buffer-count (count world-buffer))))

(def max-shown 100)
(def scroll-every 50)

(defn world-pane
  []
  (let [show-predictions (atom false)]
    (fn []
      (when-let [htm (main/selected-model-step)]
        (let [in-value (:value (first (core/input-seq htm)))]
          [:div
           [:p.muted [:small "Input on selected timestep."]]
           [:div {:style {:min-height "40vh"}}
            (helpers/text-world-input-component in-value htm max-shown scroll-every " ")]
           [:div
            [:button.btn.btn-default {:class (if @show-predictions "active")
                                      :on-click (fn [e]
                                                  (swap! show-predictions not)
                                                  (.preventDefault e))}
             "Compute predictions"]]
           (when @show-predictions
             (helpers/text-world-predictions-component in-value htm 8))])))))

(defn set-model!
  []
  (let [n-regions (:n-regions @config)
        encoder (case (:encoder @config)
                  :block (demo/make-block-encoder (:text @config))
                  :random demo/random-encoder)]
    (with-ui-loading-message
      (main/set-model!
       (demo/n-region-model n-regions demo/spec encoder)))))

(defn send-text!
  []
  (when-let [xs (seq (demo/word-item-seq (:repeats @config)
                                         (:text @config)))]
    (async/onto-chan world-c xs false)
    (swap! config assoc :world-buffer-count (count world-buffer))))

(def config-template
  [:div
   [:h3 "Input " [:small "Word sequences"]]
   [:p.text-info {:field :label
                  :id :world-buffer-count
                  :postamble " queued input values."}]
   [:div.form-horizontal
    [:div.form-group
     [:label.col-sm-5 "Repeats of each sentence:"]
     [:div.col-sm-7
      [:input.form-control {:field :numeric
                            :id :repeats}]]]
    [:div.form-group
     [:div.col-sm-12
      [:textarea.form-control {:field :textarea
                               :id :text
                               :rows 10}]]]
    [:div.form-group
     [:div.col-sm-8
      [:button.btn.btn-primary
       {:on-click (fn [e]
                    (send-text!)
                    (.preventDefault e))}
       "Send text block input"]]]
    ]
   [:h3 "HTM model"]
   [:div.form-horizontal
    [:div.form-group
     [:label.col-sm-5 "Number of regions:"]
     [:div.col-sm-7
      [:input.form-control {:field :numeric
                            :id :n-regions}]]]
    [:div.form-group
     [:label.col-sm-5 "Word encoder:"]
     [:div.col-sm-7
      [:select.form-control {:field :list
                             :id :encoder}
       [:option {:key :block} "block"]
       [:option {:key :random} "random"]]]]
    [:div.form-group
     [:div.col-sm-offset-5.col-sm-7
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (set-model!)
                    (.preventDefault e))}
       "Restart with new model"]
      [:p.text-danger "This resets all parameters."]]]
    ]])

(defn model-tab
  []
  [:div
   [:p "For looking at generalisation in sequence
        learning. The text is split into sentences at each period (.)
        and each sentence into words. The words are presented through
        a category encoder, i.e. with non-overlapping input bits."]
   [bind-fields config-template config]
   ]
  )

(defn ^:export init
  []
  (reagent/render [main/comportexviz-app model-tab world-pane]
                  (dom/getElement "comportexviz-app"))
  (reset! main/world world-c)
  (set-model!))
