(ns org.numenta.sanity.demos.simple-sentences
  (:require [org.nfrac.comportex.demos.simple-sentences :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [org.numenta.sanity.demos.comportex-common :refer [all-features]]
            [org.numenta.sanity.main :as main]
            [org.numenta.sanity.helpers :as helpers]
            [org.numenta.sanity.bridge.browser :as server]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.comportex.data :as data]
            [org.numenta.sanity.util :refer [translate-network-shape]]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [cljs.core.async :as async :refer [put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [org.numenta.sanity.macros :refer [with-ui-loading-message]]))

(def config
  (atom {:n-regions 1
         :repeats 1
         :text demo/input-text
         :world-buffer-count 0}))

(def world-buffer (async/buffer 5000))
(def world-c
  (async/chan world-buffer
              (comp (map (util/keep-history-middleware 100 :word :history))
                    (map #(assoc % :label (:word %))))))

(def into-sim (async/chan))

(def model (atom nil))

(add-watch model ::count-world-buffer
           (fn [_ _ _ _]
             (swap! config assoc :world-buffer-count (count world-buffer))))

(def max-shown 100)
(def scroll-every 50)

(defn world-pane
  []
  (let [show-predictions (atom false)
        selected-htm (atom nil)]

    (add-watch main/selection ::fetch-selected-htm
               (fn [_ _ _ [sel1]]
                 (when-let [snapshot-id (get-in sel1 [:step :snapshot-id])]
                   (let [out-c (async/chan)]
                     (put! main/into-journal ["get-model" snapshot-id
                                              (marshal/channel out-c true)])
                     (go
                       (reset! selected-htm (<! out-c)))))))

    (fn []
      (when-let [htm @selected-htm]
        (let [inval (:input-value htm)]
          [:div
           [:p.muted [:small "Input on selected timestep."]]
           [:div {:style {:min-height "40vh"}}
            (helpers/text-world-input-component inval htm max-shown
                                                scroll-every " ")]
           [:div.checkbox
            [:label [:input {:type :checkbox
                             :checked (when @show-predictions true)
                             :on-change (fn [e]
                                          (swap! show-predictions not)
                                          (.preventDefault e))}]
             "Compute predictions"]]
           (when @show-predictions
             (helpers/text-world-predictions-component htm 8))])))))

(defn set-model!
  []
  (with-ui-loading-message
    (let [n-regions (:n-regions @config)
          init? (nil? @model)]
      (reset! model (demo/n-region-model n-regions demo/spec))
      (if init?
        (server/init model world-c main/into-journal into-sim)
        (reset! main/network-shape (translate-network-shape
                                    (data/network-shape @model)))))))

(defn send-text!
  []
  (when-let [xs (seq (demo/word-item-seq (:repeats @config)
                                         (:text @config)))]
    (go
      (<! (async/onto-chan world-c xs false))
      (swap! config assoc :world-buffer-count (count world-buffer)))))

(def config-template
  [:div
   [:h3 "Input " [:small "Word sequences"]]
   [:p.text-info
    [:span {:field :label
            :id :world-buffer-count
            :postamble " queued input values."}]
    " "
    [:span {:field :container
            :visible? #(pos? (:world-buffer-count %))}
     [:button.btn.btn-warning.btn-xs
      {:on-click (fn [e]
                   (go-loop []
                     (when (and (pos? (count world-buffer))
                                (<! world-c))
                       (swap! config assoc :world-buffer-count (count world-buffer))
                       (recur)))
                   (.preventDefault e))}
      "Clear"]]]
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
       {:field :container
        :visible? #(zero? (:world-buffer-count %))
        :on-click (fn [e]
                    (send-text!)
                    (.preventDefault e))}
       "Queue text input"]
      [:button.btn.btn-default
       {:field :container
        :visible? #(pos? (:world-buffer-count %))
        :on-click (fn [e]
                    (send-text!)
                    (.preventDefault e))}
       "Queue more text input"]]]
    ]
   [:h3 "HTM model"]
   [:div.form-horizontal
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
    ]])

(defn model-tab
  []
  [:div
   [:p "In this example, text is presented as a sequence of words,
        with independent unique encodings. The text is split into
        sentences at each period (.) and each sentence into
        words."]
   [bind-fields config-template config]
   ]
  )

(defn ^:export init
  []
  (reagent/render [main/sanity-app "Comportex" [model-tab] [world-pane]
                   (atom :model) all-features into-sim]
                  (dom/getElement "sanity-app"))
  (send-text!)
  (set-model!))
