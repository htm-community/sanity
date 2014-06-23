(ns comportexviz.demos.shuffled-fixed-1d
  (:require [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [comportexviz.cla-model :as cla-model]
            [comportexviz.parameters]))

(def bit-width 400)
(def on-bits 25)
(def numb-max 15)
(def numb-domain [0 numb-max])

(def patterns
  {:run0 (range 5)
   :twos (range 0 10 2)
   :reps (mapcat #(repeat 2 %) (range 5))
   :run10 (range 10 16)
   :rev10 (reverse (range 10 16))
   :threes (range 0 16 3)
   :bounce (map (partial + 10)
                (concat (range 5) (range 3) (range 2) (range 1)))
   })

(defn input-seq
  []
  ;; infinite lazy sequence
  (let [ps (vec patterns)]
    (->> (fn []
           (let [[k xs] (util/rand-nth ps)]
             (map (fn [x]
                    {:pattern k :values [x]})
                  xs)))
         (repeatedly)
         (interpose {:pattern nil :values []})
         (apply concat))))

;; a function; do not hold on to the head of an infinite seq.
(defn initial-input-fn
  []
  ;; infinite lazy sequence, store in metadata to avoid printing
  (let [inseq (input-seq)]
    (with-meta (first inseq) {::inseq inseq})))

(defn input-transform
  [v]
  (let [inseq (next (::inseq (meta v)))]
    (with-meta (first inseq) {::inseq inseq})))

(def efn
  (let [f (enc/superpose-encoder
           (enc/linear-number-encoder bit-width on-bits numb-domain))]
    (fn [v]
      (f (:values v)))))

(def spec
  (assoc comportexviz.parameters/small
    :input-size bit-width
    :potential-radius (quot bit-width 4)))

(def generator
  (cla-model/generator (initial-input-fn) input-transform efn
                       {:bit-width bit-width}))

(def ^:export model
  (cla-model/cla-model generator spec))

