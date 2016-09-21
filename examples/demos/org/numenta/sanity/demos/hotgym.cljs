(ns org.numenta.sanity.demos.hotgym
  (:require [cljs.core.async :as async :refer [put! <!]]
            [cljs.reader :as edn]
            [org.numenta.sanity.bridge.browser :as server]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.demos.comportex-common :refer [all-features]]
            [org.numenta.sanity.main :as main]
            [org.numenta.sanity.comportex.data :as data]
            [org.numenta.sanity.util :refer [translate-network-shape]]
            [org.numenta.sanity.viz-canvas :as viz]
            [goog.dom :as dom]
            [goog.net.XhrIo :as XhrIo]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.layer.params :as params]
            [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.encoders :as e]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            [reagent-forms.core :refer [bind-fields]]
            [reagent.core :as reagent :refer [atom]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [org.numenta.sanity.macros :refer [with-ui-loading-message]]))

(def world-c
  (async/chan))

(def into-sim (async/chan))

(def model (atom nil))

(defn anomaly-score
  [{:keys [active active-predicted]}]
  (let [total (+ active active-predicted)]
    (if (pos? total)
      (/ active total)
      1)))

(defn consider-consumption!
  [step->scores step consumption]
  (let [;; this is sufficient because it's the only proximal input
        candidate {:consumption consumption}
        out-c (async/chan)
        snapshot-id (:snapshot-id step)]
    (put! main/into-journal
          ["consider-future" snapshot-id candidate (marshal/channel out-c true)])
    (go
      (let [[[_ col-state-freqs]] (seq (<! out-c))]
        (swap! step->scores assoc-in [step consumption]
               (anomaly-score col-state-freqs))))))

(def try-boundaries?
  (atom true))

(def try-last-value?
  (atom true))

(def n-predictions
  (atom 3))

(def unit-width 8)
(def cx (/ unit-width 2))
(def max-r cx)

(defn actual-svg
  [step top unit-height]
  (let [actual-consumption (get-in step
                                   [:input-value :consumption])
        y-actual (* (- top actual-consumption)
                    unit-height)]
    [:rect {:x 0 :y (- y-actual 1.5)
            :width unit-width :height 3
            :fill "black"}]))

(defn prediction-svg
  [y-scores]
  (let [min-score (->> y-scores
                       (map second)
                       (apply min))
        candidates (->> y-scores
                        (filter (fn [[consumption score]]
                                  (= score min-score)))
                        (sort-by first))
        [y] (nth candidates (quot (count candidates) 2))]
    [:rect {:x 0 :y (- y 1)
            :width unit-width :height 2
            :fill "#78B4FB"}]))

(defn anomaly-gradient-svg
  [y-scores]
  (into [:g]
        (for [[[y1 score1] [y2 score2]] (->> y-scores
                                             (sort-by first)
                                             (partition 2 1))
              :let [grad-id (str (random-uuid))]]
          [:g
           [:defs
            [:linearGradient {:id grad-id
                              :x1 0 :y1 0
                              :x2 0 :y2 1}
             [:stop {:offset "0%"
                     :stop-color "red"
                     :stop-opacity score1}]
             [:stop {:offset "100%"
                     :stop-color "red"
                     :stop-opacity score2}]]]
           [:rect {:x (- cx max-r) :y y1
                   :width (* 2 max-r) :height (- y2 y1)
                   :fill (str "url(#" grad-id ")")}]])))

(defn anomaly-samples-svg
  [ys]
  (into [:g]
        (for [y ys]
          [:circle {:fill "brown"
                    :cx cx :cy y
                    :r 1.5}])))

(defn consumption-axis-svg
  [h bottom top]
  (let [label-every 10]
    (into [:g]
          (for [i (range bottom top label-every)]
            [:text {:x -5 :y (* h (- 1 (/ (- i bottom)
                                          (- top bottom))))
                    :dy "0.35em"
                    :font-family "sans-serif"
                    :font-size "9px"
                    :fill "rgb(104, 104, 104)"
                    :style {:font-weight "bold"}
                    :text-anchor "end"}
             (str i)]))))

(def extend-past-px 30)

(defn horizontal-label
  [x y w transition? contents-above contents-below]
  (into [:div {:style {:position "relative"}}
         [:div {:style {:position "absolute"
                        :left x
                        :top (- y 0.5)
                        :width (+ (- w x)
                                  extend-past-px)
                        :transition-property (if transition?
                                               "top"
                                               "none")
                        :transition-duration "0.15s"
                        :height 1
                        :background-color "black"}}]]
        (for [[contents top] [[contents-above "-2.7em"]
                              [contents-below "0.2em"]]
              :when contents]
          [:div {:style {:position "absolute"
                         :top y
                         :transition-property (if transition?
                                                "top"
                                                "none")
                         :transition-duration "0.15s"
                         :left w
                         :font-family "sans-serif"
                         :font-size "9px"
                         :font-weight "bold"}}
           [:div {:style {:position "absolute"
                          :top top
                          :transition-property "top"
                          :transition-duration "0.15s"
                          :left 4}}
            contents]])))

(defn y->consumption
  [y h top bottom]
  (+ (* (- 1 (/ y h))
        (- top bottom))
     bottom))

(defn consumption->y
  [consumption top unit-height]
  (* (- top consumption)
     unit-height))

(defn anomaly-radar-pane
  []
  (let [step->scores (atom {})
        hover-i (atom nil)
        hover-y (atom nil)]
    (add-watch main/steps ::fetch-anomaly-radar
               (fn [_ _ _ steps-v]
                 ;; remove the old
                 (swap! step->scores select-keys steps-v)
                 ;; populate the new
                 (doseq [step (->> steps-v
                                   (remove (partial contains?
                                                    @step->scores)))
                         :let [out-c (async/chan)
                               snapshot-id (:snapshot-id step)]]
                   (put! main/into-journal
                         ["decode-predictive-columns" snapshot-id
                          :power-consumption @n-predictions
                          (marshal/channel out-c true)])
                   (when @try-boundaries?
                     (consider-consumption! step->scores step -10)
                     (consider-consumption! step->scores step 110))
                   (when @try-last-value?
                     (consider-consumption! step->scores step
                                            (get-in step [:input-value
                                                          :consumption])))
                   (go
                     (doseq [consumption (map :value (<! out-c))]
                       (consider-consumption! step->scores step consumption))))))
    (fn []
      (let [h 400
            draw-steps (get-in @main/viz-options [:drawing :draw-steps])
            w (* unit-width draw-steps)
            h-pad-top 15
            h-pad-bottom 8
            w-pad-left 20
            w-pad-right 42 ;; for text
            top 110
            bottom -10
            unit-height (/ h (- top bottom))
            label-every 10
            center-dt (:dt (peek @main/selection))
            dt0 (max -1 (- center-dt (quot draw-steps 2)))
            center-i (- center-dt dt0)
            draw-dts (range dt0 (min (+ dt0 draw-steps)
                                     (count @main/steps)))]
        [:div {:style {:position "relative"
                       :width (+ w-pad-left w w-pad-right)}}
         [:div {:style {:position "absolute"
                        :top 0
                        :left 0
                        :font-family "sans-serif"
                        :font-size "9px"
                        :font-weight "bold"}}
          "power-consumption"]
         [:svg {:height (+ h h-pad-top h-pad-bottom)
                :width (+ w w-pad-left w-pad-right)}
          [:g {:transform (str "translate(" w-pad-left "," h-pad-top ")")}
           [consumption-axis-svg h bottom top]
           (into [:g]
                 (for [i (range (count draw-dts))
                       :let [dt (nth draw-dts i)
                             from-step (nth @main/steps (inc dt) nil)
                             y-scores (when from-step
                                        (for [[consumption score]
                                              (->> (get @step->scores from-step)
                                                   (sort-by first))]
                                          [(consumption->y consumption top
                                                           unit-height)
                                           score]))]]
                   [:g {:transform (str "translate(" (* unit-width
                                                        (- (dec draw-steps) i))
                                        ",0)")}
                    [:rect (cond-> {:x 0
                                    :y 0
                                    :width unit-width :height h
                                    :fill "white"}
                             from-step
                             (assoc
                              :on-click
                              (fn [e]
                                (let [y (- (.-clientY e)
                                           (-> e
                                               .-target
                                               .getBoundingClientRect
                                               .-top))]
                                  (consider-consumption!
                                   step->scores from-step (y->consumption
                                                           y h top bottom))))
                              :on-mouse-move
                              (fn [e]
                                (let [y (- (.-clientY e)
                                           (-> e
                                               .-target
                                               .getBoundingClientRect
                                               .-top))]
                                  (reset! hover-i i)
                                  (reset! hover-y y)))
                              :on-mouse-leave (fn [e]
                                                (reset! hover-i nil)
                                                (reset! hover-y nil))))]
                    [:g {:style {:pointer-events "none"}}
                     (when (not-empty y-scores)
                       [anomaly-gradient-svg y-scores])
                     (when (not-empty y-scores)
                       [anomaly-samples-svg (->> y-scores
                                                 (map first))])
                     (when (and (<= 0 dt)
                                (< dt (count @main/steps)))
                       [actual-svg (nth @main/steps dt) top unit-height])
                     (when (not-empty y-scores)
                       [prediction-svg y-scores])]]))
           (let [x (* unit-width
                      (- (dec draw-steps) center-i))
                 points (str x "," 0 " "
                             x "," -1 " "
                             (+ x unit-width) "," -1 " "
                             (+ x unit-width) "," 0)]
             [:g
              (let [points (str x "," 0 " "
                                x "," -1 " "
                                (+ x unit-width) "," -1 " "
                                (+ x unit-width) "," 0)]
                [:g
                 [:polyline {:stroke (:highlight viz/state-colors)
                             :stroke-width 3
                             :fill "none"
                             :points points}]
                 [:polyline {:stroke "black"
                             :stroke-width 0.75
                             :fill "none"
                             :points points}]])
              (let [points (str x "," h " "
                                x "," (+ h 6) " "
                                (+ x unit-width) "," (+ h 6) " "
                                (+ x unit-width) "," h)]
                [:g
                 [:polyline {:stroke (:highlight viz/state-colors)
                             :stroke-width 3
                             :fill "none"
                             :points points}]
                 [:polyline {:stroke "black"
                             :stroke-width 0.75
                             :fill "none"
                             :points points}]])])]]
         (when @hover-y
           (let [i @hover-i
                 dt (nth draw-dts i)
                 from-step (nth @main/steps (inc dt))
                 y @hover-y
                 consumption (y->consumption y h top bottom)
                 [[lower-consumption lower-score]
                  [upper-consumption upper-score]] (->> (get @step->scores
                                                             from-step)
                                                        (sort-by first)
                                                        (partition 2 1)
                                                        (filter
                                                         (fn [[[c1 s1] [c2 s2]]]
                                                           (<= c1
                                                               consumption
                                                               c2)))
                                                        first)
                 lower-y (consumption->y lower-consumption
                                         top unit-height)
                 upper-y (consumption->y upper-consumption
                                         top unit-height)
                 dt-left (+ w-pad-left (* unit-width
                                          (+ (- (dec draw-steps) i)
                                             0.5)))]
             [:div {:style {:position "absolute"
                            :left 0
                            :top h-pad-top
                            :pointer-events "none"}}
              [horizontal-label dt-left lower-y (+ w w-pad-left) true
               nil [:div (str (.toFixed lower-consumption 1) "kW") [:br]
                    (str (.toFixed lower-score 3))]]
              (let [contents [:div
                              (str (.toFixed consumption 1) "kW") [:br]
                              "click"]
                    [above below] (cond (> (- y
                                              upper-y)
                                           30)
                                        [contents nil]

                                        (> (- lower-y
                                              y)
                                           30)
                                        [nil contents]

                                        :else
                                        [nil nil])]
                [horizontal-label dt-left y (+ w w-pad-left) false
                 above below])
              [horizontal-label dt-left upper-y (+ w w-pad-left) true
               [:div
                (str (.toFixed upper-consumption 1) "kW") [:br]
                (str (.toFixed upper-score 3))] nil]]))]))))

(defn world-pane
  []
  (when (not-empty @main/steps)
    [:div
     [:div {:style {:margin-top 10}}
      [:p [anomaly-radar-pane]]]
     (into [:div {:margin-top 30}]
           (for [[sense-id v] (dissoc (:sensed-values (main/selected-step))
                                      :power-consumption)]
             [:div {:style {:margin-bottom 20}}
              [:p
               [:span {:style {:font-family "sans-serif"
                               :font-size "9px"
                               :font-weight "bold"}} (name sense-id)]
               [:br]
               [:strong (str v)]]]))]))

(defn set-model!
  []
  (with-ui-loading-message
    (let [init? (nil? @model)
          params (util/deep-merge
                   params/better-parameter-defaults
                   {:depth 1
                    :distal {:max-segments 128
                             :perm-connected 0.20
                             :perm-init 0.20}})]
      (reset! model
              (cx/network
               {:layer-a (layer/layer-of-cells params)}
               {:power-consumption [:consumption
                                    (e/sampling-linear-encoder
                                     [(+ 1024 256)] 17 [-12.8 112.8] 12.8)]
                :is-weekend? [:is-weekend?
                              (e/category-encoder [10] [true false])]
                :hour-of-day [:hour-of-day
                              (e/category-encoder [(* 40 24)] (range 24))]}
               {:ff-deps {:layer-a [:power-consumption]}
                :lat-deps {:layer-a [:is-weekend? :hour-of-day]}}))
      (if init?
        (do
          (XhrIo/send "../data/hotgym.consumption_weekend_hour.edn"
                      (fn [e]
                        (if (.. e -target isSuccess)
                          (let [response (.. e -target getResponseText)
                                inputs (map (partial zipmap [:consumption
                                                             :is-weekend?
                                                             :hour-of-day])
                                            (edn/read-string response))]
                            (async/onto-chan world-c inputs false))
                          (js/log.error
                           (str "Request to " (.. e -target getLastUri)
                                " failed. " (.. e -target getStatus) " - "
                                (.. e -target getStatusText))))))
          (server/init model world-c main/into-journal into-sim))
        (reset! main/network-shape (translate-network-shape
                                    (data/network-shape @model)))))))

(defn model-tab
  []
  [:div
   [:p "Numenta's \"hotgym\" dataset."]
   [:p "Uses the solution from:" [:br]
    [:a {:href "http://mrcslws.com/gorilla/?path=hotgym.clj"}
     "Predicting power consumptions with HTM"]]
   [:p "This demo highlights the Anomaly Radar display on the left. The anomaly
   scores for possible next inputs are sampled, and the sample points are shown
   as dots. The prediction is a blue dash, and the actual value is a black
   dash. The red->white scale represents the anomaly score. The anomaly score is
   correct wherever there's a dot, and it's estimated elsewhere."]
   [:p "Inspect the numbers by hovering your mouse over the Anomaly Radar. Click
   to add your own samples. You might want to pause the simulation first."]
   [:p "This demo chooses samples by decoding the predictive columns, as
   explained in the essay above."]
   [:p "It's fun to click the black dashes and see if it changes the
   prediction. When this happens, it shows that the HTM actually predicted
   something better than we thought, we just didn't sample the right points. You
   could expand on this demo to try different strategies for choosing a clever
   set of samples, finding the right balance between results and code
   performance."]])

(defn ^:export init
  []
  (reagent/render [main/sanity-app "Comportex" [model-tab] [world-pane]
                   (atom :model) all-features into-sim]
                  (dom/getElement "sanity-app"))
  (set-model!))
