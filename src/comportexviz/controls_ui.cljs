(ns comportexviz.controls-ui
  (:require [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [goog.events :as gevents]
            [goog.string :as gstring]
            [goog.string.format]
            [clojure.string :as str]
            [goog.ui.Slider]
            [goog.ui.Component.EventType]
            [cljs.reader]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p])
  (:require-macros [c2.util :refer [bind!]]
                   [comportexviz.macros :refer [with-ui-loading-message]]))

(defn now [] (.getTime (js/Date.)))

(defn sim-rate
  "Returns the simulation rate in timesteps per second for current
   run."
  [model]
  (if (:time (:run-start model))
    (let [m (:run-start model)
          dur-ms (- (now)
                    (:time m))
          steps (- (p/timestep model)
                   (:timestep m))]
      (-> (/ steps dur-ms)
          (* 1000)))
    0))

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
  (->> (map (fn [x] (if (number? x) (str x) (name x))) keys)
       (map #(str/replace % \? "_QMARK_"))
       (interpose "_")
       (apply str "comportex-")))

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

(defn parameter-input
  [prefix [k v]]
  [:label
   [:span.parameter-label (name k)]
   [:input {:id (keys->id [prefix k])
            :value (str v)}]])

(defn handle-controls!
  [model sim-go? main-options sim-step! draw-now!]
  (bind! "#comportex-controls"
         [:div#comportex-controls

          [:fieldset#sim-controls
           [:legend "Simulation"]
           [:label "Timestep:" [:span#sim-timestep
                                (p/timestep @model)]]
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
            [:span [:a#sim-faster {:href "#"} "faster"]]]
           [:button#sim-reset "Reset model"] " (not input)"]

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
          ])

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
  (event/on-raw "#sim-reset" :click
                (fn [_]
                  (with-ui-loading-message
                    (swap! model p/reset))))

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
                              #(inc %)))))

(defn handle-options!
  [model keep-steps viz-options]
  (bind! "#comportex-drawing"
         [:div#comportex-drawing
          (let [viz @viz-options]
            [:fieldset#viz-options
             [:legend "Visualisation"]
             [:fieldset
              [:legend "Input"]
              (checkbox viz [:input :active] "Active bits") [:br]
              (checkbox viz [:input :predicted] "Predicted bits")]
             [:fieldset
              [:legend "Columns"]
              (checkbox viz [:columns :overlaps] "Overlap scores") [:br]
              (checkbox viz [:columns :n-segments] "Num segments") [:br]
              (checkbox viz [:columns :active] "Active columns") [:br]
              (checkbox viz [:columns :predictive] "Predictive columns") [:br]
              (checkbox viz [:columns :temporal-pooling] "TP columns")]
             [:fieldset
              [:legend "Feed-forward synapses"]
              (checkbox viz [:ff-synapses :active] "Active ff-synapses") [:br]
              (checkbox viz [:ff-synapses :inactive] "Inactive ff-synapses") [:br]
              (checkbox viz [:ff-synapses :disconnected] "Disconnected ff-synapses") [:br]
              (checkbox viz [:ff-synapses :permanences] "Permanences")]
             [:fieldset
              [:legend "Lateral dendrite segments"]
              (combobox viz [:lat-synapses :from] [:learning :all :none]
                        "Synapses from ") [:br]
              (checkbox viz [:lat-synapses :active] "Active synapses") [:br]
              (checkbox viz [:lat-synapses :inactive] "Inactive synapses") [:br]
              (checkbox viz [:lat-synapses :disconnected] "Disconnected synapses") [:br]
              (checkbox viz [:lat-synapses :permanences] "Permanences")]])
          ])
  (doseq [[k km] @viz-options
          [subk v] km
          :let [id (keys->id [k subk])
                el (->dom (str "#" id))]
          :when el]
    (event/on-raw el :change
                  (fn [_]
                    (let [v (when-let [s (dom/val el)] (keyword s))]
                      (swap! viz-options assoc-in [k subk] v))))))

(defn handle-parameters!
  [model selection]
  (bind! "#comportex-parameters"
         (let [sel-rid (:region @selection)]
           [:div#comportex-parameters
            (let [rgns (p/region-seq @model)]
              (for [rid (range (count rgns))
                    :let [rgn (nth rgns rid)
                          spec (p/params rgn)]]
                [:div {:style {:display (if (not= rid sel-rid) "none")}}
                 [:form {:id (str "region-spec-form-" rid)}
                  [:p (str "Region " rid) [:br]
                   [:span.detail "(click on a region to select it)"]]
                  [:fieldset.region-spec
                   [:legend "Parameters"]
                   (map (partial parameter-input rid) (sort spec))
                   [:p.detail [:input {:type "submit" :value "Set!"}]
                    (str " (will be set immediately, but then use Reset above for any"
                         " parameters that apply only in initialisation)")]]]]))]
           ))

  (doseq [rid (range (count (p/region-seq @model)))
          :let [form-el (->dom (str "#region-spec-form-" rid))]]
    (event/on-raw form-el :submit
                  (fn [e]
                    (let [rgn (nth (p/region-seq @model) rid)
                          ospec (p/params rgn)
                          s (reduce (fn [s k]
                                      (let [id (keys->id [rid k])
                                            el (->dom (str "#" id))
                                            v (cljs.reader/read-string (dom/val el))]
                                        (assoc s k v)))
                                    {} (keys ospec))]
                      (swap! model p/update-by-uuid (:uuid rgn)
                             #(-> %
                                  (assoc-in [:column-field :spec] s)
                                  (assoc-in [:layer-3 :spec] s)))
                      (.preventDefault e)
                      false)))))
