(ns comportexviz.controls-ui
  (:require [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [goog.events :as gevents]
            [goog.ui.Slider]
            [goog.ui.Component.EventType]
            [cljs.core.async :as async :refer [chan put! <! >! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def simulation-gate (chan))
(def animation-gate (chan))

(def display-options
  (atom {:active-columns true
         :bursting-columns true
         :predicted-bits true
         :predictive-columns nil
         :overlap-columns nil
         :active-insyns nil
         :inactive-insyns nil
         :insyns-permanences nil
         :active-dendrites nil
         :inactive-dendrites nil
         :dendrite-permanences nil
         }))

(def sim-go? (atom false))
(def sim-step-ms (atom 500))
(def anim-go? (atom false))
(def anim-step-ms (atom 500))

(defn run-sim
  []
  (go
   (while @sim-go?
     (>! simulation-gate true)
     (<! (timeout @sim-step-ms)))))

(defn run-anim
  []
  (go
   (while @anim-go?
     (>! animation-gate true)
     (<! (timeout @anim-step-ms)))))

(add-watch sim-go? :run-sim
           (fn [_ _ _ v] (when v (run-sim))))

(add-watch anim-go? :run-anim
           (fn [_ _ _ v] (when v (run-anim))))

(add-watch sim-go? :buttons
           (fn [_ _ _ v]
             (-> (->dom "#sim-start")
                 (dom/style :display (when v "none")))
             (-> (->dom "#sim-stop")
                 (dom/style :display (when-not v "none")))))

(add-watch anim-go? :buttons
           (fn [_ _ _ v]
             (-> (->dom "#anim-start")
                 (dom/style :display (when v "none")))
             (-> (->dom "#anim-stop")
                 (dom/style :display (when-not v "none")))))

(add-watch sim-step-ms :ui-text
           (fn [_ _ _ v]
             (-> (->dom "#sim-ms-text")
                 (dom/text (str v "ms")))))

(add-watch anim-step-ms :ui-text
           (fn [_ _ _ v]
             (-> (->dom "#anim-ms-text")
                 (dom/text (str v "ms")))))

(add-watch display-options :redraw
           (fn [_ _ _ _]
             (put! animation-gate true)))

(defn slider
  [id min-val max-val step unit]
  (doto (goog.ui.Slider.)
    (.setId id)
    (.setMinimum min-val)
    (.setMaximum max-val)
    (.setStep step)
    (.setUnitIncrement unit)
    (.createDom)
    (.render (.-body js/document))))

(defn bind-slider
  [s atom]
  (gevents/listen s goog.ui.Component.EventType/CHANGE
                  (fn [_] (reset! atom (.getValue s))))
  (.setValue s @atom))

(defn controls
  []
  [:div#controls

   [:fieldset#sim-controls
    [:legend "Simulation"]
    [:label "Timestep:" [:span#sim-timestep]]
    [:br]
    [:label "Step ms:"
     [:div#sim-ms-slider-box {:class "slider-box"}]]
    [:span#sim-ms-text]
    [:button#sim-start "Start"]
    [:button#sim-stop {:style {:display "none"}} "Stop"]
    [:button#sim-step "Step"]]

   [:fieldset#anim-controls

    [:legend "Animation"]
    [:label "Step ms:"
     [:div#anim-ms-slider-box {:class "slider-box"}]]
    [:span#anim-ms-text]
    [:button#anim-start "Start"]
    [:button#anim-stop {:style {:display "none"}} "Stop"]
    [:button#anim-step "Draw now"]]

   [:fieldset#display-options
    [:legend "Display"]
    [:div
     [:label [:input#overlap-columns {:type "checkbox"}] "Overlap scores"] [:br]
     [:label [:input#active-columns {:type "checkbox"}] "Active columns"] [:br]
     [:label [:input#bursting-columns {:type "checkbox"}] "Bursting columns"] [:br]
     [:label [:input#predictive-columns {:type "checkbox"}] "Predictive columns"] [:br]
     [:label [:input#predicted-bits {:type "checkbox"}] "Predicted bits"]]
    [:div
     [:label [:input#active-insyns {:type "checkbox"}] "Active in-synapses"] [:br]
     [:label [:input#inactive-insyns {:type "checkbox"}] "Inactive in-synapses"] [:br]
     [:label [:input#insyns-permanences {:type "checkbox"}] "Permanences"]]
    [:div
     [:label [:input#active-dendrites {:type "checkbox"}] "Active dendrites"] [:br]
     [:label [:input#inactive-dendrites {:type "checkbox"}] "Inactive synapses"] [:br]
     [:label [:input#dendrite-permanences {:type "checkbox"}] "Permanences"]]]])

(defn handle-display-options
  []
  (doseq [k (keys @display-options)
          :let [el (->dom (str "#" (name k)))]]
    (event/on-raw el :click
                  (fn [_] (swap! display-options
                                assoc k (dom/val el))))))

(defn show-display-options
  []
  (doseq [[k v] @display-options
          :let [el (->dom (str "#" (name k)))]]
    (dom/val el (boolean v))))

(defn init-ui
  []
  (dom/replace! "#controls" (controls))
  (let [sim-s (slider "sim-ms-slider" 0 1000 10 50)
        anim-s (slider "anim-ms-slider" 0 1000 10 50)]
    (dom/append! "#sim-ms-slider-box" (.getElement sim-s))
    (dom/append! "#anim-ms-slider-box" (.getElement anim-s))
    (bind-slider sim-s sim-step-ms)
    (bind-slider anim-s anim-step-ms))

  (event/on-raw "#sim-start" :click
                (fn [_] (reset! sim-go? true)))
  (event/on-raw "#sim-stop" :click
                (fn [_] (reset! sim-go? false)))
  (event/on-raw "#sim-step" :click
                (fn [_] (put! simulation-gate true)))

  (event/on-raw "#anim-start" :click
                (fn [_] (reset! anim-go? true)))
  (event/on-raw "#anim-stop" :click
                (fn [_] (reset! anim-go? false)))
  (event/on-raw "#anim-step" :click
                (fn [_] (put! animation-gate true)))

  (handle-display-options)
  (show-display-options))








