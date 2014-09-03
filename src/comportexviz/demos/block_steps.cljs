(ns comportexviz.demos.block-steps
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [comportexviz.parameters]))

(def bit-width 300)
(def on-bits 30)
(def numb-max 9)
(def numb-domain [0 numb-max])

(def initial-input {:value 0, :dir :up, :counter 0})

(def step-every 3)

(defn input-transform
  [{:keys [value dir counter] :as m}]
  (if (< counter step-every)
    ;; pass through value
    (update-in m [:counter] inc)
    ;; update value
    (let [newval (if (= dir :up)
                   (inc value)
                   (dec value))
          newdir (cond
                  (zero? newval) :up
                  (= numb-max newval) :down
                  :else dir)]
      {:value newval
       :dir newdir
       :counter 1})))

(def encoder
  (enc/pre-transform :value
                     (enc/linear-encoder bit-width on-bits numb-domain)))

(defn ^:export model
  []
  (let [gen (core/input-generator initial-input input-transform encoder)
        spec comportexviz.parameters/small]
    (core/tree core/cla-region spec [gen])))
