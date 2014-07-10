(ns comportexviz.demos.mixed-fixed-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [comportexviz.parameters]))

(def bit-width 300)
(def on-bits 30)
(def numb-max 10)
(def numb-domain [0 numb-max])

(def patterns
  [{:first 0, :last 3, :step inc, :gap 3}
   {:first 3, :last 8, :step inc, :gap 4}
   {:first 10, :last 0, :step dec, :gap 1}])

(def initial-input
  (mapv (fn [m]
          (assoc m :value (:first m)
                 :gap-countdown 0))
        patterns))

(defn input-transform
  [ms]
  (mapv (fn [m]
          (cond
           ;; reached end of sequence; begin gap
           (= (:value m) (:last m))
           (assoc m :value nil
                  :gap-countdown (dec (:gap m)))
           ;; in gap
           (and (not (:value m))
                (pos? (:gap-countdown m)))
           (update-in m [:gap-countdown] dec)
           ;; reached end of gap; restart sequence
           (and (not (:value m))
                (zero? (:gap-countdown m)))
           (assoc m :value (:first m))
           ;; in sequence
           :else
           (update-in m [:value] (:step m))))
        ms))

(def efn
  (let [f (enc/superpose-encoder
           (enc/linear-number-encoder bit-width on-bits numb-domain))]
    (fn [ms]
      (f (map :value ms)))))

(defn ^:export model
  []
  (let [gen (core/generator initial-input input-transform efn
                            {:bit-width bit-width})
        spec (assoc comportexviz.parameters/small
               :input-size bit-width
               :potential-radius (quot bit-width 2))]
    (core/cla-model gen spec)))
