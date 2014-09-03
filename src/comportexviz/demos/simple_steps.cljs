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

(def encoder
  (enc/pre-transform first
                     (enc/linear-encoder bit-width on-bits numb-domain)))

(defn ^:export model
  []
  (let [gen (core/input-generator initial-input input-transform encoder)
        spec comportexviz.parameters/small]
    (core/tree core/cla-region spec [gen])))
