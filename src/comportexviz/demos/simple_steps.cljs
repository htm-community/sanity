(ns comportexviz.demos.simple-steps
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [comportexviz.parameters]))

(def bit-width 300)
(def on-bits 30)
(def numb-max 9)
(def numb-domain [0 numb-max])

(def initial-input [0 :up])

(defn input-transform
  [[i dir]]
  (if (= dir :up)
    (if (< i numb-max)
      [(inc i) :up]
      [(dec i) :down])
    ;; going down
    (if (> i 0)
      [(dec i) :down]
      [(inc i) :up])))

(def efn
  (let [f (enc/linear-number-encoder bit-width on-bits numb-domain)]
    (fn [[i dir]]
      (f i))))

(defn ^:export model
  []
  (let [spec (assoc comportexviz.parameters/small
               :input-size bit-width
               :potential-radius (quot bit-width 4))
        gen (core/generator initial-input input-transform efn
                            {:bit-width bit-width})]
    (core/cla-model gen spec)))
