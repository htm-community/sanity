(ns org.numenta.sanity.demos.letters
  (:require [org.nfrac.comportex.demos.letters :as demo]
            [org.nfrac.comportex.core :as core]
            [org.numenta.sanity.demos.comportex-common :refer [all-features]]
            [org.nfrac.comportex.util :as util]
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
         :world-buffer-count 0}))

(def world-buffer (async/buffer 5000))
(def world-c
  (async/chan world-buffer
              (comp (map (util/keep-history-middleware 300 :value :history))
                    (map #(assoc % :label (:value %))))))

(def model (atom nil))

(def into-sim (async/chan))

(add-watch model ::count-world-buffer
           (fn [_ _ _ _]
             (swap! config assoc :world-buffer-count (count world-buffer))))

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

(def max-shown 300)
(def scroll-every 150)

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
            (helpers/text-world-input-component inval htm max-shown scroll-every "")]
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

(defn immediate-key-down!
  [e]
  (when-let [[x] (->> (.-charCode e)
                      (.fromCharCode js/String)
                      (demo/clean-text)
                      (seq))]
    (async/put! world-c {:value (str x)}))
  (swap! config assoc :world-buffer-count (count world-buffer)))

(defn send-text!
  []
  (when-let [xs (seq (demo/clean-text @text-to-send))]
    (go
      (<! (async/onto-chan world-c (for [x xs] {:value (str x)})
                           false))
      (swap! config assoc :world-buffer-count (count world-buffer)))))

(def config-template
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
   ])

(defn model-tab
  []
  [:div
   [:p "In this example, text input is presented as a sequence of
        letters, with independent unique encodings. It is transformed
        to lower case, and all whitespace is squashed into single
        spaces."]

   [:h3 "Input " [:small "Letter sequences"]]
   [:p.text-info
    (str (:world-buffer-count @config) " queued input values.")
    " "
    (when (pos? (:world-buffer-count @config))
      [:button.btn.btn-warning.btn-xs
       {:on-click (fn [e]
                    (go-loop []
                      (when (and (pos? (count world-buffer))
                                 (<! world-c))
                        (swap! config assoc :world-buffer-count (count world-buffer))
                        (recur)))
                    (.preventDefault e))}
       "Clear"])]
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
    [:button.btn {:class (if (pos? (count world-buffer))
                           "btn-default"
                           "btn-primary")
                  :on-click (fn [e]
                              (send-text!)
                              (.preventDefault e))}
     (if (pos? (count world-buffer))
       "Queue more text input"
       "Queue text input")]]

   [:h3 "HTM model"]
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
