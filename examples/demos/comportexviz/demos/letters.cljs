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

(def model-config
  (atom {:n-regions 1
         :encoder :block}))

(def draw (draw-text-fn 8))

(def world-buffer (async/buffer 5000))
(def world-c
  (async/chan world-buffer
              (comp (map (util/keep-history-middleware 400 :value :history))
                    (map #(vary-meta % assoc
                                     :comportexviz/draw-world
                                     draw)))))

;; used to force Reagent to re-render world-buffer count
(def world-buffer-trigger (atom true))
(add-watch main/model ::world-buffer-trigger (fn [_ _ _ _]
                                               (swap! world-buffer-trigger not)))

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

(defn set-model!
  []
  (let [n-regions (:n-regions @model-config)
        encoder (case (:encoder @model-config)
                  :block demo/block-encoder
                  :random demo/random-encoder)]
    (with-ui-loading-message
      (main/set-model!
        (core/regions-in-series core/sensory-region (core/sensory-input encoder)
                                n-regions demo/spec)))))

(defn immediate-key-down!
  [e]
  (when-let [[x] (->> (or (.-keyCode e) (.-charCode e))
                      (.fromCharCode js/String)
                      (demo/clean-text)
                      (seq))]
    (async/put! world-c {:value (str x)}))
  (swap! world-buffer-trigger not))

(defn send-text!
  []
  (when-let [xs (seq (demo/clean-text @text-to-send))]
    (async/onto-chan world-c (for [x xs] {:value (str x)})
                     false)
    (swap! world-buffer-trigger not)))

(def model-config-template
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
      {:on-click (fn [e]
                   (set-model!)
                   (.preventDefault e))}
      "Restart with new model"]
     [:p.text-danger "This resets all parameters."]]]
   ])

(defn model-tab
  []
  [:div
   [:p "In this example, text input is presented as a sequence of letters.
        Allowed characters are letters, numbers, space, period and question
        mark."]

   [:h3 "Input " [:small "Letter sequences"]]
   ^{:key (str "reagent-refresh-key-" @world-buffer-trigger)}
   [:p.text-info
    (str (count world-buffer) " queued input values.")]
   [:div.well
    "Immediate input as you type: "
    [:input {:size 2 :maxLength 1
             :on-key-press (fn [e]
                             (immediate-key-down! e)
                             (.preventDefault e))}]]
   [:div.well
    [:textarea {:style {:width "90%" :height "10em"}
                :value @text-to-send
                :on-change (fn [e]
                             (reset! text-to-send
                                     (-> e .-target forms/getValue))
                             (.preventDefault e))}]
    [:button.btn.btn-primary {:on-click (fn [e]
                                          (send-text!)
                                          (.preventDefault e))}
     "Send text block input"]]

   [:h3 "HTM model"]
   [bind-fields model-config-template model-config]
   ]
  )

(defn ^:export init
  []
  (reagent/render (main/comportexviz-app model-tab)
                  (dom/getElement "comportexviz-app"))
  (reset! main/world world-c)
  (set-model!)
  (swap! main/main-options assoc :sim-go? true))
