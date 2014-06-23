(ns comportexviz.demos.signal-steps
  (:require [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [comportexviz.cla-model :as cla-model]
            [comportexviz.parameters]))

;; inputs
(def bit-width 300)
(def cat-bit-width 60)
(def numb-bit-width (- bit-width cat-bit-width))
(def numb-max 7)
(def numb-domain [0 numb-max])
(def on-bits 30)

(def initial-input [0 :up])

(defn input-transform
  [[i dir]]
  (let [new-i (-> (case dir
                    :up (inc i)
                    :down (dec i))
                  (min numb-max)
                  (max 0))
        new-dir (util/rand-nth [:up :down])]
    [new-i new-dir]))

(def efn
  (enc/juxtapose-encoder
   (enc/linear-number-encoder numb-bit-width on-bits numb-domain)
   (enc/category-encoder cat-bit-width [:down :up])))

(def spec
  (assoc comportexviz.parameters/small
    :input-size bit-width
    :potential-radius (quot bit-width 4)))

(def generator
  (cla-model/generator initial-input input-transform efn
                       {:bit-width bit-width}))

(def ^:export model
  (cla-model/cla-model generator spec))
