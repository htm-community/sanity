(ns comportexviz.controls-ui
  (:require [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [goog.events :as gevents]
            [goog.string :as gstring]
            [goog.string.format]
            [goog.ui.Slider]
            [goog.ui.Component.EventType])
  (:require-macros [c2.util :refer [bind!]]))

(defn now [] (.getTime (js/Date.)))

(defn sim-rate
  "Returns the simulation rate in timesteps per second for current
   run."
  [model]
  (when (:time (:run-start model))
    (let [m (:run-start model)
          dur-ms (- (now)
                    (:time m))
          steps (- (:timestep (:region model))
                   (:timestep m))]
      (-> (/ steps dur-ms)
          (* 1000)))))

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

(defn keys->id
  [keys]
  (apply str "display-" (interpose "-" (map name keys))))

(defn checkbox
  [m keys txt]
  [:label [:input {:id (keys->id keys)
                   :type "checkbox"
                   :checked (when (get-in m keys) "checked")}]
   txt])

(defn combobox
  [m keys optvals txt]
  [:label txt
   [:select {:id (keys->id keys)}
    (for [optval optvals]
      [:option {:value (name optval)
                :selected (when (= (get-in m keys) optval) "selected")}
       (name optval)])]])

(defn init!
  [model sim-go? main-options keep-steps viz-options sim-step! draw-now!]
  (bind! "#controls"
         [:div#controls

          [:fieldset#sim-controls
           [:legend "Simulation"]
           [:label "Timestep:" [:span#sim-timestep
                                (:timestep (:region @model))]]
           [:span#sim-rate {:class "detail"}
            (when @sim-go?
              (gstring/format "%.1f steps/sec."
                              (sim-rate @model)))]
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
              (checkbox viz [:input :active] "Active bits") [:br]
              (checkbox viz [:input :predicted] "Predicted bits")]
             [:div
              (checkbox viz [:columns :overlaps] "Overlap scores") [:br]
              (checkbox viz [:columns :predictive] "Predictive columns")]
             [:div
              (checkbox viz [:ff-synapses :active] "Active in-synapses") [:br]
              (checkbox viz [:ff-synapses :inactive] "Inactive in-synapses") [:br]
              (checkbox viz [:ff-synapses :permanences] "Permanences")]
             [:div
              (combobox viz [:lat-synapses :from] [:learning :all :none]
                        "Synapses from ") [:br]
              (checkbox viz [:lat-synapses :active] "Active synapses") [:br]
              (checkbox viz [:lat-synapses :inactive] "Inactive synapses") [:br]
              (checkbox viz [:lat-synapses :permanences] "Permanences")]])])

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

  (doseq [[k km] @viz-options
          [subk v] km
          :let [id (keys->id [k subk])
                el (->dom (str "#" id))]]
    (event/on-raw el :change
                  (fn [_]
                    (let [v (when-let [s (dom/val el)] (keyword s))]
                      (swap! viz-options assoc-in [k subk] v))))))
