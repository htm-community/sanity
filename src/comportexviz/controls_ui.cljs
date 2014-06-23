(ns comportexviz.controls-ui
  (:require [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [goog.events :as gevents]
            [goog.ui.Slider]
            [goog.ui.Component.EventType])
  (:require-macros [c2.util :refer [bind!]]))

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

(defn checkbox
  [m key txt]
  [:label [:input {:id (name key)
                   :type "checkbox"
                   :checked (when (m key) "checked")}]
   txt])

(defn init!
  [model sim-go? main-options keep-steps viz-options sim-step! draw-now!]
  (bind! "#controls"
         [:div#controls

          [:fieldset#sim-controls
           [:legend "Simulation"]
           [:label "Timestep:" [:span#sim-timestep
                                (:timestep (:region @model))]]
           [:br]
           [:button#sim-start
            {:style {:display (when @sim-go? "none")}} "Start"]
           [:button#sim-stop
            {:style {:display (when-not @sim-go? "none")}} "Stop"]
           [:button#sim-step "Step"]
           [:label "Step every:"
            [:span#sim-ms-text (str (:sim-step-ms @main-options) " ms")]
            [:span [:a#sim-slower {:href "#"} "slower"]]
            [:span [:a#sim-faster {:href "#"} "faster"]]]]

          [:fieldset#anim-controls
           [:legend "Animation"]
           [:button#anim-start
            {:style {:display (when (:anim-go? @main-options) "none")}} "Start"]
           [:button#anim-stop
            {:style {:display (when-not (:anim-go? @main-options) "none")}} "Stop"]
           [:button#anim-step "Draw now"]
           [:label "Draw every:"
            [:span#anim-every-text (str (:anim-every @main-options) " steps")]
            [:span [:a#anim-slower {:href "#"} "slower"]]
            [:span [:a#anim-faster {:href "#"} "faster"]]]]

          (let [viz @viz-options]
            [:fieldset#viz-options
             [:legend "Visualisation"]
             [:div
              (checkbox viz :overlap-columns "Overlap scores") [:br]
              (checkbox viz :active-columns "Active columns") [:br]
              (checkbox viz :bursting-columns "Bursting columns") [:br]
              (checkbox viz :predictive-columns "Predictive columns") [:br]
              (checkbox viz :predicted-bits "Predicted bits")]
             [:div
              (checkbox viz :active-insyns "Active in-synapses") [:br]
              (checkbox viz :inactive-insyns "Inactive in-synapses") [:br]
              (checkbox viz :insyns-permanences "Permanences")]
             [:div
              (checkbox viz :active-dendrites "Active dendrites") [:br]
              (checkbox viz :inactive-dendrites "Inactive synapses") [:br]
              (checkbox viz :dendrite-permanences "Permanences")]])])

  (event/on-raw "#sim-start" :click
                (fn [_] (reset! sim-go? true)))
  (event/on-raw "#sim-stop" :click
                (fn [_] (reset! sim-go? false)))
  (event/on-raw "#sim-step" :click
                (fn [_] (sim-step!)))
  (event/on-raw "#sim-faster" :click
                (fn [_] (swap! main-options update-in [:sim-step-ms]
                              #(-> (- % 100) (max 0)))))
  (event/on-raw "#sim-slower" :click
                (fn [_] (swap! main-options update-in [:sim-step-ms]
                              #(+ % 100))))

  (event/on-raw "#anim-start" :click
                (fn [_] (swap! main-options assoc :anim-go? true)))
  (event/on-raw "#anim-stop" :click
                (fn [_] (swap! main-options assoc :anim-go? false)))
  (event/on-raw "#anim-step" :click
                (fn [_] (draw-now!)))
  (event/on-raw "#anim-faster" :click
                (fn [_] (swap! main-options update-in [:anim-every]
                              #(-> (dec %) (max 1)))))
  (event/on-raw "#anim-slower" :click
                (fn [_] (swap! main-options update-in [:anim-every]
                              #(inc %))))

  (doseq [k (keys @viz-options)
          :let [el (->dom (str "#" (name k)))]]
    (event/on-raw el :click
                  (fn [_] (swap! viz-options assoc k (dom/val el))))))

