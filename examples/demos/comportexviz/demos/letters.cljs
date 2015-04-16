(ns comportexviz.demos.letters
  (:require [org.nfrac.comportex.demos.letters :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
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

(def world-c (async/chan 1000))

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
  (.preventDefault e))

(defn do-send-text
  [e]
  (when-let [xs (seq (demo/clean-text @text-to-send))]
    (async/onto-chan world-c (for [x xs] {:value (str x)})
                     false)))

(defn setup-tab
  []
  [:div
   [:h3 "Letters"]
   [:p "Text input is presented as a sequence of letters. Allowed
        characters are letters, numbers, space, period and question
        mark."]
   [:fieldset
    [:legend "HTM model"]

    ;; TODO - use reagent.forms

    "Number of regions:"
    [:input {:size 2 :value (:n-regions @config)}]
    [:br]
    "Letter encoder:"
    [:select
     [:option {:value "block"} "block"]
     [:option {:value "random"} "random"]]
    [:br]
    [:button {:on-click #(reset-model-from-ui)}
     "Restart with this model"]
    "(this will also reset all parameter values)"
    ]
   [:hr]
   [:fieldset
    [:legend "Input"]
    "Immediate input as you type:"
    [:input {:size 1 :maxLength 1
             :on-key-down immediate-key-down}]
    [:br]
    [:textarea {:style {:width "90%" :height "10em"}
                :value @text-to-send
                :on-change #(reset! text-to-send
                                   (-> % .-target forms/getValue))}]
    [:br]
    [:button {:on-click do-send-text}
     "Send text block input"]
    ]
  ]
  )

(defn drawing-tab []
  [:div#comportex-drawing])
(defn parameters-tab []
  [:div#comportex-parameters])
(defn plots-tab []
  [:div#comportex-plots])
(defn details-tab []
  [:div#comportex-details
   [:textarea#detail-text]])

(defn comportex-controls []
  [:div#comportex-controls])

(def tab-cmps
  [[:setup setup-tab]
   [:drawing drawing-tab]
   [:parameters parameters-tab]
   [:plots plots-tab]
   [:details details-tab]])

(defn tab-bar
  [current-tab]
  (into [:div.btn-group]
        (for [[k _] tab-cmps]
          [:button.btn.btn-default {:key k
                                    :class (if (= @current-tab k) "active")
                                    :on-click #(reset! current-tab k)}
           (name k)])))

(defn comportex-app
  []
  (let [current-tab (atom :setup)]
    (fn []
      [:div
       [:div
        [:canvas#comportex-viz]
        [:div#comportex-sidebar
         [comportex-controls]
         [tab-bar current-tab]
         (into [:div#comportex-tabs]
               (for [[k cmp] tab-cmps]
                 [:div {:style (if (not= k @current-tab) {:display "none"})}
                  [:h2 (name k)]
                  [cmp]]))
         ]]
       [:div#comportex-loading]])))

(defn ^:export init
  []
  (reagent/render [comportex-app] (dom/getElement "comportex-app"))
  (set-world)
  (reset-model-from-ui)
  (reset! main/sim-go? true))
