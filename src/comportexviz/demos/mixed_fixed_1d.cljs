(ns comportexviz.demos.mixed-fixed-1d
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
  [(range 0 5)
   (range 3 9)
   (range 8 12)
   (reverse (range 0 15))])

(defn mix-patterns
  "Returns an infinite sequence of sets of values."
  [patterns]
  (->> patterns
       (map #(apply concat (repeat %)))
       (apply map (fn [& xs] (set xs)))))

(defn input-init
  []
  (mix-patterns patterns))

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
     :potential-radius (quot bit-width 4))))

(defn ^:export run-sim
  []
  (let [c (chan)]
    (go
     (loop [inseq (input-init)
            rgn r-init]
       (let [in (first inseq)
             in-bits (efn in)
             new-rgn (core/cla-step rgn in-bits)]
         (>! c
             {:input in :inbits in-bits :region new-rgn})
         (recur (input-transform inseq)
                new-rgn))))
    c))
