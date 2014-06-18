(ns comportexviz.demos.block-steps
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util :refer [round]]
            [comportexviz.parameters]
            [clojure.set :as set]
            [cljs.core.async :refer [chan <! >!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;; inputs
(def bit-width 200)
(def item-radius-bits 25)
(def item-overlap 0.0)
(def input-bases
  (let [step-bits (* item-radius-bits 2 (- 1 item-overlap))]
    (range item-radius-bits bit-width step-bits)))

(def initial-input [0 :up])

(def alter-every 4)

(defn input-transform
  [[i dir] t]
  (if (zero? (mod t alter-every))
    (if (= dir :up)
      (if (< i (dec (count input-bases)))
        [(inc i) :up]
        [(dec i) :down])
      ;; going down
      (if (> i 0)
        [(dec i) :down]
        [(inc i) :up]))
    ;; pass through unchanged
    [i dir]))

(defn efn
  [[i _]]
  (let [x (nth input-bases i)
        lo (max 0 (- x item-radius-bits))
        hi (min bit-width (+ x item-radius-bits))]
    (set (range lo hi))))

(defn rgn-init
  []
  (core/cla-region
   (assoc comportexviz.parameters/small
     :input-size bit-width
     :potential-radius (quot bit-width 2))))

(defn ^:export run-sim
  []
  (let [c (chan)]
    (go
     (loop [in initial-input
            rgn (rgn-init)]
       (let [in-bits (efn in)
             new-rgn (core/cla-step rgn in-bits)
             t (:timestep new-rgn)]
         (>! c
             {:input in :inbits in-bits :region new-rgn})
         (recur (input-transform in t)
                new-rgn))))
    c))
