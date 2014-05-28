(ns comportexviz.mixed-fixed-1d
  (:require [org.nfrac.comportex.pooling :as p]
            [org.nfrac.comportex.sequence-memory :as sm]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [clojure.set :as set]
            [comportexviz.mq :as mq]
            [cljs.core.async :refer [<! >!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;; inputs
(def bit-width 200)
(def numb-max 15)
(def numb-domain [0 numb-max])
(def on-bits 15)

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

;; initial CLA region
(def ncol 200)
(def depth 5)

(def r-init
  (-> (p/region (assoc p/spatial-pooler-defaults
                  :ncol ncol
                  :input-size bit-width
                  :potential-radius (quot bit-width 5)
                  :global-inhibition false
                  :stimulus-threshold 2
                  :duty-cycle-period 500))
      (sm/with-sequence-memory (assoc sm/sequence-memory-defaults
                                 :depth depth))))

(defn cla-step
  [r in-bits]
  (let [r-sp (p/pooling-step r in-bits)]
    (sm/sequence-memory-step r-sp (:active-columns r-sp))))

(defn run-sim!
  []
  (go
   (loop [inseq (input-init)
          rgn r-init]
     (let [in (first inseq)
           in-bits (efn (:values in))
           new-rgn (cla-step rgn in-bits)]
       (>! mq/sim-channel
           {:input in :inbits in-bits :region new-rgn})
       (recur (input-transform inseq)
              new-rgn)))))

(run-sim!)
