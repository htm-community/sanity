(ns comportexviz.block-steps
  (:require [org.nfrac.comportex.pooling :as p]
            [org.nfrac.comportex.sequence-memory :as sm]
            [org.nfrac.comportex.util :as util :refer [round]]
            [clojure.set :as set]
            [comportexviz.mq :as mq]
            [cljs.core.async :refer [<! >!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;; inputs
(def bit-width 128)
(def item-radius-bits 16)
(def item-overlap 0.0)
(def input-bases
  (let [step-bits (* item-radius-bits 2 (- 1 item-overlap))]
    (range item-radius-bits bit-width step-bits)))

(defn input-init
  []
  [0 :up])

(def alter-every 5)

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

;; initial CLA region
(def ncol 128)
(def depth 5)

(defn rgn-init
  []
  (-> (p/region (assoc p/spatial-pooler-defaults
                  :ncol ncol
                  :input-size bit-width
                  :potential-radius (quot bit-width 2)
                  :global-inhibition false
                  :stimulus-threshold 2
                  :duty-cycle-period 100))
      (sm/with-sequence-memory (assoc sm/sequence-memory-defaults
                                 :depth depth))))

(defn cla-step
  [r in-bits]
  (let [r-sp (p/pooling-step r in-bits)]
    (sm/sequence-memory-step r-sp (:active-columns r-sp))))

(defn run-sim!
  []
  (go
   (loop [in (input-init)
          rgn (rgn-init)]
     (let [in-bits (efn in)
           new-rgn (cla-step rgn in-bits)
           t (:timestep new-rgn)]
       (>! mq/sim-channel
           {:input in :inbits in-bits :region new-rgn})
       (recur (input-transform in t)
              new-rgn)))))

(run-sim!)
