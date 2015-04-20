(ns comportexviz.demos.letters
  (:require [org.nfrac.comportex.demos.letters :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.protocols :as p]
            [comportexviz.sentence-drawing :refer [draw-text-fn]]
            [comportexviz.main :as main]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-ui-loading-message]]))

(def n-predictions 8)

(def config
  (atom {:n-regions 1
         :encoder :block}))

(def text-to-send
  (atom
"Jane has eyes.
Jane has a head.
Jane has a mouth.
Jane has a brain.
Jane has a book.
Jane has no friend.

Chifung has eyes.
Chifung has a head.
Chifung has a mouth.
Chifung has a brain.
Chifung has no book.
Chifung has a friend."))

(def world-buffer (async/buffer 1000))
(def world-c (async/chan world-buffer))

(defn touch!
  "Alters the atom (metadata) just to trigger Regent to re-render dependent components."
  ;; (here `model`, to display world-buffer size)
  [atom]
  (swap! atom vary-meta update ::touch-flag not))

(defn set-world
  []
  (let [draw (draw-text-fn n-predictions)]
    (main/set-world (->> world-c
                         (async/map< (util/keep-history-middleware 400 :value :history))
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))))

(defn reset-model-from-ui
  []
  (let [n-regions (:n-regions @config)
        encoder (case (:encoder @config)
                  :block demo/block-encoder
                  :random demo/random-encoder)]
    (with-ui-loading-message
      (main/set-model
       (core/regions-in-series core/sensory-region (core/sensory-input encoder)
                               n-regions demo/spec)))))

(defn immediate-key-down
  [e]
  (when-let [[x] (->> (or (.-keyCode e) (.-charCode e))
                      (.fromCharCode js/String)
                      (demo/clean-text)
                      (seq))]
    (async/put! world-c {:value (str x)}))
  (touch! main/model)
  (.preventDefault e))

(defn do-send-text
  [e]
  (when-let [xs (seq (demo/clean-text @text-to-send))]
    (async/onto-chan world-c (for [x xs] {:value (str x)})
                     false)
    (touch! main/model)))

(def config-template
  [:div.form-horizontal
   [:div.form-group
    [:label.col-sm-5 "Number of regions:"]
    [:div.col-sm-7
     [:input.form-control {:field :numeric
                           :id :n-regions}]]]
   [:div.form-group
    [:label.col-sm-5 "Letter encoder:"]
    [:div.col-sm-7
     [:select.form-control {:field :list
                            :id :encoder}
      [:option {:key :block} "block"]
      [:option {:key :random} "random"]]]]
   [:div.form-group
    [:div.col-sm-offset-5.col-sm-7
     [:button.btn.btn-default
      {:on-click #(reset-model-from-ui)}
      "Restart with new model"]
     [:p.text-danger "Resets all parameter values."]]]
   ])

(defn model-tab
  []
  [:div
   [:p "In this example, text input is presented as a sequence of letters.
        Allowed characters are letters, numbers, space, period and question
        mark."]

   [:h3 "HTM model"]
   [bind-fields config-template config]

   [:h3 "Input " [:small "Letter sequences"]]
   [:p.text-info
    ;; force re-render when model atom changes:
    {:to-force-reagent-refresh (count @main/model)}
    (str (count world-buffer) " queued input values.")]
   [:div.well
    "Immediate input as you type: "
    [:input {:size 2 :maxLength 1
             :on-key-press immediate-key-down}]]
   [:div.well
    [:textarea {:style {:width "90%" :height "10em"}
                :value @text-to-send
                :on-change #(reset! text-to-send
                                    (-> % .-target forms/getValue))}]
    [:button.btn.btn-primary {:on-click do-send-text}
     "Send text block input"]]
   ]
  )

(defn ^:export init
  []
  (reagent/render (main/comportexviz-app model-tab)
                  (dom/getElement "comportexviz-app"))
  (set-world)
  (reset-model-from-ui)
  (swap! main/main-options assoc :sim-go? true))
