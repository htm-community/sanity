(ns comportexviz.demos.mixed-gaps-fixed-1d
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [comportexviz.parameters]
            [clojure.set :as set]
            [cljs.core.async :refer [chan <! >!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;; inputs
(def bit-width 400)
(def numb-max 15)
(def numb-domain [0 numb-max])
(def on-bits 25)

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
  [patterns gap-range]
  (let [tagseqs (map (fn [[k xs]]
                       (->> (repeat-with-gaps xs gap-range)
                            (map (fn [x] {:id (if x k) :val x}))))
                     patterns)]
    (apply map (fn [& ms] {:patterns (set (keep :id ms))
                          :values (set (keep :val ms))})
           tagseqs)))

(defn input-init
  []
  ;; infinite lazy sequence
  (mix-patterns-with-gaps patterns [1 50]))

(defn input-transform
  [xs]
  (next xs))

(def efn
  (enc/superpose-encoder
   (enc/linear-number-encoder bit-width on-bits numb-domain)))

(def r-init
  (core/cla-region
   (assoc comportexviz.parameters/small
     :input-size bit-width
     :potential-radius (quot bit-width 5))))

(defn ^:export run-sim
  []
  (let [c (chan)]
    (go
     (loop [inseq (input-init)
            rgn r-init]
       (let [in (first inseq)
             in-bits (efn (:values in))
             new-rgn (core/cla-step rgn in-bits)]
         (>! c
             {:input in :inbits in-bits :region new-rgn})
         (recur (input-transform inseq)
                new-rgn))))
    c))
