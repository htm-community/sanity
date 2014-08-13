(ns comportexviz.demos.mixed-gaps-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [comportexviz.parameters]))

(def bit-width 400)
(def on-bits 25)
(def numb-max 15)
(def numb-domain [0 numb-max])

(def patterns
  {:run-0-5 [0 1 2 3 4 5]
   :rev-5-1 [5 4 3 2 1]
   :run-6-10 [6 7 8 9 10]
   :twos [0 2 4 6 8 10 12 14]
   :reps-0-5 [0 0 1 1 2 2 3 3 4 4 5 5]
   :jump-7-11 [5 6 7 11 12]
   :saw-10-15 [10 12 11 13 12 14 13 15]})

(def gap-range
  (->> (vals patterns) (map count) (reduce +) (long)))

(defn initial-input
  []
  (mapv (fn [[k xs]]
          {:name k, :seq xs, :index nil,
           :gap-countdown (util/rand-int 0 gap-range)})
        patterns))

(defn input-transform
  [ms]
  (mapv (fn [m]
          (cond
           ;; reached end of sequence; begin gap
           (= (:index m) (dec (count (:seq m))))
           (assoc m :index nil
                  :gap-countdown (util/rand-int 0 gap-range))
           ;; in gap
           (and (not (:index m))
                (pos? (:gap-countdown m)))
           (update-in m [:gap-countdown] dec)
           ;; reached end of gap; restart sequence
           (and (not (:index m))
                (zero? (:gap-countdown m)))
           (assoc m :index 0)
           ;; in sequence
           :else
           (update-in m [:index] inc)))
        ms))

(defn current-value
  [m]
  (when (:index m)
    (get (:seq m) (:index m))))

(def efn
  (let [f (enc/superpose-encoder
           (enc/linear-number-encoder bit-width on-bits numb-domain))]
    (fn [ms]
      (f (map current-value ms)))))

(defn ^:export model
  []
  (let [gen (core/generator (initial-input) input-transform efn
                            {:bit-width bit-width})
        spec (assoc comportexviz.parameters/small
               :input-size bit-width
               :potential-radius (quot bit-width 4)
               :ncol 1000
               :depth 8
               :duty-cycle-period 100000
               :global-inhibition false
               :permanence-dec 0.01
               :permanence-inc 0.05
               :sp-perm-dec 0.03
               :sp-perm-inc 0.03
               )]
    (core/cla-model gen spec)))
