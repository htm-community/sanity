(ns comportexviz.demos.hotgym
  (:require [cljs.core.async :as async :refer [put! <!]]
            [cljs.reader :as edn]
            [comportexviz.bridge.browser :as server]
            [comportexviz.main :as main]
            [comportexviz.server.data :as data]
            [goog.dom :as dom]
            [goog.net.XhrIo :as XhrIo]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as e]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            [reagent-forms.core :refer [bind-fields]]
            [reagent.core :as reagent :refer [atom]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [comportexviz.macros :refer [with-ui-loading-message]]))

(def world-c
  (async/chan))

(def into-sim (async/chan))

(def model (atom nil))

(defn middle-out-range
  "By example:
  Given 7.2, returns (7, 8, 6, 9, 5, 10, ...),
  Given 7.7, returns (8, 7, 9, 6, 10, 5, ...)"
  [v]
  (let [start (-> v float Math/round long)
        rounded-down? (> v start)
        up (iterate inc start)
        down (iterate dec start)]
    (if rounded-down?
      (interleave down (drop 1 up))
      (interleave up (drop 1 down)))))

(defn multiples-within-radius
  [center radius multiples-of]
  (let [lower-bound (- center radius)
        upper-bound (+ center radius)]
    (->> (middle-out-range (/ center multiples-of))
         (map (partial * multiples-of))
         (take-while #(<= lower-bound
                          %
                          upper-bound)))))

(defn into-bounded
  "Move items from `from` to `coll` until its size reaches `max-size`
  or we run out of items. Specifically supports sets and maps, which don't
  always grow when an item is added."
  [coll max-size from]
  (loop [coll coll
         from from]
    (let [n-remaining (- max-size (count coll))]
      (if (and (pos? n-remaining)
               (not-empty from))
        (let [[taken untaken] (split-at n-remaining from)]
          (recur (into coll taken)
                 untaken))
        coll))))

(defn sampled-window
  "Place a bit in the center.
  Distribute bits around the center until we've used half of the remainder.
  Double the density. Distribute again until we've used half of the remainder.
  Double the density. ...
  Continue until all active bits are distributed or all bits are active.

  Strategically choose bit positions so that the intersections between
  various ranges will select the same bits."
  [center n-bits target-n-active bit-radius]
  (loop [chosen #{center}
         density (/ (- target-n-active (count chosen))
                    (* 2 bit-radius)
                    2)]
    (let [remaining (- target-n-active (count chosen))
          multiples-of (long (/ 1 density))]
      (if (and (pos? remaining)
               (pos? multiples-of))
        (let [half-remaining (quot remaining 2)
              n-take (if (or (odd? remaining)
                             (odd? half-remaining))
                       remaining
                       half-remaining)]
          (recur (->> (multiples-within-radius center bit-radius multiples-of)
                      (filter #(<= 0 % (dec n-bits)))
                      (into-bounded chosen (+ n-take (count chosen))))
                 (* density 2)))
        chosen))))

(defrecord SamplingLinearEncoder
    [topo n-active lower upper radius]
  p/PTopological
  (topology
    [_]
    topo)
  p/PEncoder
  (encode
    [_ x]
    (if x
      (let [n-bits (p/size topo)
            domain-width (- upper lower)
            center (-> x
                       (max lower)
                       (min upper)
                       (- lower)
                       (/ domain-width)
                       (* n-bits)
                       long)
            bit-radius (* radius
                          (/ (p/size topo) domain-width))]
        (sampled-window center n-bits n-active bit-radius))
      (sequence nil)))
  (decode
    [this bit-votes n]
    (let [span (double (- upper lower))
          values (range lower upper (if (< 5 span 250)
                                      1
                                      (/ span 50)))]
      (->> (e/decode-by-brute-force this values bit-votes)
           (take n)))))

(defn sampling-linear-encoder
  "A linear encoder that samples the surrounding radius, rather than
  activating all of it. Sampling density decreases as distance increases.

  * `dimensions` is the size of the encoder in bits along one or more
    dimensions, a vector e.g. [500].

  * `n-active` is the number of bits to be active.

  * `[lower upper]` gives the numeric range to cover. The input number
    will be clamped to this range.

  * `radius` describes the range to sample.

  Recommendations:

  * `lower` and `upper` should be `radius` below and above the actual
    lower and upper bounds. Otherwise the radius will extend off the
    number line, creating representations that behave a bit differently
    from the rest."
  [dimensions n-active [lower upper] radius]
  (let [topo (topology/make-topology dimensions)]
    (map->SamplingLinearEncoder {:topo topo
                                 :n-active n-active
                                 :lower lower
                                 :upper upper
                                 :radius radius})))

(defn world-pane
  []
  (into [:div
         [:div {:style {:font "10px sans-serif"}} "sensed values"]]
        (for [[sense-id v] (:sensed-values (main/selected-step))]
          [:div {:style {:margin-top 30}}
           [:p (name sense-id) [:br]
            [:strong (str v)]]])))

(defn set-model!
  []
  (with-ui-loading-message
    (let [init? (nil? @model)]
      (reset! model
              (core/region-network
               {:rgn-0 [:power-consumption :is-weekend? :hour-of-day]}
               (constantly core/sensory-region)
               {:rgn-0 (assoc cells/better-parameter-defaults
                              :depth 1
                              :max-segments 128
                              :distal-perm-connected 0.20
                              :distal-perm-init 0.20)}
               {:power-consumption [:consumption
                                    (sampling-linear-encoder
                                     [(+ 1024 256)] 17 [-12.8 112.8] 12.8)]}
               {:is-weekend? [:is-weekend?
                              (e/category-encoder [10] [true false])]
                :hour-of-day [:hour-of-day
                              (e/category-encoder [(* 40 24)] (range 24))]}))
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
        (reset! main/step-template (data/step-template-data @model))))))

(defn model-tab
  []
  [:div
   [:p "Numenta's \"hotgym\" dataset."]
   [:p "Uses the solution from "
    [:a {:href "http://mrcslws.com/gorilla/?path=hotgym.clj"}
     "Predicting power consumptions with HTM"]]])

(defn ^:export init
  []
  (reagent/render [main/comportexviz-app [model-tab] [world-pane] into-sim]
                  (dom/getElement "comportexviz-app"))
  (set-model!))
