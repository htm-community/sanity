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
  (->> (map (fn [x] (if (keyword? x) (name x) (str x))) keys)
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
    (for [optval optvals
          :let [optstr (if (keyword? optval) (name optval) (str optval))]]
      [:option {:value optstr
                :selected (when (= (get-in m keys) optval) "selected")}
       optstr])]])

(defn radio
  [m keys value]
  [:input {:name (keys->id keys)
           :id (keys->id (conj keys value))
           :type "radio"
           :value value
           :checked (when (= value (get-in m keys)) "checked")}])

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
             [:p
              [:label
               "Keep "
               (combobox {:keep-steps @keep-steps}
                         [:keep-steps] [2 5 10 20 30 40 50 100 200]
                         "")
               " steps of history"]]
             [:p
              [:label
               (radio viz [:drawing :force-d] 1)
               "Draw "
               (combobox viz [:drawing :draw-steps] [1 5 10 15 20 25 30 40 50]
                         "")
               " steps in 1D"]
              [:br]
              [:label
               (radio viz [:drawing :force-d] 2)
               "Draw one step in 2D"]]
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
              [:legend "Distal dendrite segments"]
              (combobox viz [:distal-synapses :from] [:learning :all :none]
                        "Synapses from ") [:br]
              (checkbox viz [:distal-synapses :active] "Active synapses") [:br]
              (checkbox viz [:distal-synapses :inactive] "Inactive synapses") [:br]
              (checkbox viz [:distal-synapses :disconnected] "Disconnected synapses") [:br]
              (checkbox viz [:distal-synapses :permanences] "Permanences")]])
          ])
  (doseq [[k km] @viz-options
          [subk v] km
          :let [id (keys->id [k subk])
                el (->dom (str "#" id))]
          :when el]
    (event/on-raw el :change
                  (fn [_]
                    (let [v (when-let [s (dom/val el)]
                              (if (#{:force-d :draw-steps} subk)
                                (cljs.reader/read-string s)
                                (keyword s)))]
                      (swap! viz-options assoc-in [k subk] v)))))
  ;; radio buttons are annoyingly different
  (let [[k subk] [:drawing :force-d]]
    (doseq [value [1 2]
            :let [id (keys->id [k subk value])
                  el (->dom (str "#" id))]]
      (event/on-raw el :change
                    (fn [_]
                      (when (dom/val el)
                        (swap! viz-options assoc-in [k subk] value))))))
  ;; keep-steps is in a separate atom
  (let [id (keys->id [:keep-steps])
        el (->dom (str "#" id))]
    (event/on-raw el :change
                  (fn [_]
                    (let [s (dom/val el)
                          v (cljs.reader/read-string s)]
                      (reset! keep-steps v))))))

(defn handle-parameters!
  [model selection]
  (bind! "#comportex-parameters"
         (let [sel-region (:region @selection)
               sel-layer (:layer @selection)]
           [:div#comportex-parameters
            (for [[region-key rgn] (:regions @model)
                  layer-id (core/layers rgn)
                  :let [spec (p/params (get rgn layer-id))
                        uniqix (str (name region-key) (name layer-id))]]
              [:div {:style {:display (if (not= [region-key layer-id]
                                                [sel-region sel-layer])
                                        "none")}}
               [:form {:id (str "region-spec-form-" uniqix)}
                [:p (str "Currently selected: " (name region-key) " "
                         (name layer-id)) [:br]
                 [:span.detail "(click on a region layer to select it)"]]
                [:fieldset.region-spec
                 [:legend "Parameters"]
                 (map (partial parameter-input uniqix)
                      (sort spec))
                 [:p.detail [:input {:type "submit" :value "Set!"}]
                  (str " (will be set immediately, but then use Reset above for any"
                       " parameters that apply only in initialisation)")]]]])]
           ))
  ;; once only
  (doseq [[region-key init-rgn] (:regions @model)
          layer-id (core/layers init-rgn)
          :let [uniqix (str (name region-key) (name layer-id))
                form-el (->dom (str "#region-spec-form-" uniqix))]]
    (event/on-raw form-el :submit
                  (fn [e]
                    (let [rgn (get-in @model [:region region-key])
                          ospec (p/params (get rgn layer-id))
                          s (reduce (fn [s k]
                                      (let [id (keys->id [uniqix k])
                                            el (->dom (str "#" id))
                                            v (cljs.reader/read-string (dom/val el))]
                                        (assoc s k v)))
                                    {} (keys ospec))]
                      (swap! model assoc-in [:regions region-key layer-id :spec]
                             s)
                      (.preventDefault e)
                      false)))))
