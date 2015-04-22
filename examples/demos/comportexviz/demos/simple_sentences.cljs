(ns comportexviz.demos.simple-sentences
  (:require [org.nfrac.comportex.demos.simple-sentences :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [comportexviz.sentence-drawing :refer [draw-sentence-fn]]
            [comportexviz.main :as main]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(def n-predictions 8)

(def model-config
  (atom {:n-regions 1
         :encoder :block}))

(def input-config
  (atom {:repeats 1
         :text demo/input-text}))

(def world-buffer (async/buffer 5000))
(def world-c (async/chan world-buffer))

;; used to force Reagent to re-render world-buffer count
(def world-buffer-trigger (atom true))
(add-watch main/model ::world-buffer-trigger (fn [_ _ _ _]
                                               (swap! world-buffer-trigger not)))

(defn set-world!
  []
  (let [draw (draw-sentence-fn n-predictions)]
    (main/set-world (->> world-c
                         (async/map< (util/keep-history-middleware 100 :word :history))
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))))

(defn set-model!
  []
  (let [n-regions (:n-regions @model-config)
        encoder (case (:encoder @model-config)
                  :block (demo/make-block-encoder (:text @input-config))
                  :random demo/random-encoder)]
    (with-ui-loading-message
      (main/set-model
       (core/regions-in-series core/sensory-region (core/sensory-input encoder)
                               n-regions demo/spec)))))

(defn send-text!
  []
  (when-let [xs (seq (demo/word-item-seq (:repeats @input-config)
                                         (:text @input-config)))]
    (async/onto-chan world-c xs false)
    (swap! world-buffer-trigger not)))

(def model-config-template
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
   ])

(def input-config-template
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
   ])

(defn model-tab
  []
  [:div
   [:p "For looking at generalisation in sequence
        learning. The text is split into sentences at each period (.)
        and each sentence into words. The words are presented through
        a category encoder, i.e. with non-overlapping input bits."]

   [:h3 "Input " [:small "Word sequences"]]
   ^{:key (str "reagent-refresh-key-" @world-buffer-trigger)}
   [:p.text-info
    (str (count world-buffer) " queued input values.")]
   [bind-fields input-config-template input-config]

   [:h3 "HTM model"]
   [bind-fields model-config-template model-config]
   ]
  )

(defn ^:export init
  []
  (reagent/render (main/comportexviz-app model-tab)
                  (dom/getElement "comportexviz-app"))
  (set-world!)
  (set-model!)
  (swap! main/main-options assoc :sim-go? true))
