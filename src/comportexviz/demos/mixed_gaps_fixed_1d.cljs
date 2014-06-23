(ns comportexviz.demos.mixed-gaps-fixed-1d
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

(defn repeat-with-gaps
  [xs [lower upper]]
  (let [gap #(repeat (util/rand-int lower upper) nil)]
    (apply concat (interpose xs (repeatedly gap)))))

(defn mix-patterns-with-gaps
  "Returns an infinite sequence of maps, each with keys `:patterns` (a
   set of keywords) and `:values` (a set of numbers). "
  [patterns gap-range]
  (let [tagseqs (map (fn [[k xs]]
                       (->> (repeat-with-gaps xs gap-range)
                            (map (fn [x] {:id (if x k) :val x}))))
                     patterns)]
    (apply map (fn [& ms] {:patterns (set (keep :id ms))
                          :values (set (keep :val ms))})
           tagseqs)))

(defn crouching-head-hidden-tail
  "Returns the first element, with the rest in metadata to avoid
   printing an infinite sequence."
  [xs]
  (-> (first xs)
      (with-meta {::next (next xs)})))

;; a function not a value; do not hold on to the head of an infinite seq.
(defn initial-input
  []
  (let [inseq (mix-patterns-with-gaps patterns [1 50])]
    (crouching-head-hidden-tail inseq)))

(defn input-transform
  [v]
  (crouching-head-hidden-tail (::next (meta v))))

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
  (cla-model/generator (initial-input) input-transform efn
                       {:bit-width bit-width}))

(def ^:export model
  (cla-model/cla-model generator spec))
