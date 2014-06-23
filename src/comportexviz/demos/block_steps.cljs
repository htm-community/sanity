(ns comportexviz.demos.block-steps
  (:require [org.nfrac.comportex.encoders :as enc]
            [comportexviz.cla-model :as cla-model]
            [comportexviz.parameters]))

;; inputs
(def bit-width 300)
(def on-bits 30)
(def numb-max 9)
(def numb-domain [0 numb-max])

(def initial-input {:value 0, :dir :up, :counter 0})

(def alter-every 4)

(defn input-transform
  [{:keys [value dir counter] :as m}]
  (if (< counter alter-every)
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
       :counter 0})))

(def efn
  (let [f (enc/linear-number-encoder bit-width on-bits numb-domain)]
    (fn [m]
      (f (:value m)))))

(def spec
  (assoc comportexviz.parameters/small
    :input-size bit-width
    :potential-radius (quot bit-width 4)))

(def generator
  (cla-model/generator initial-input input-transform efn
                       {:bit-width bit-width}))

(def ^:export model
  (cla-model/cla-model generator spec))
