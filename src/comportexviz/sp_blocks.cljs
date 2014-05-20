(ns comportexviz.sp-blocks
  (:require [org.nfrac.comportex.pooling :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util :refer [round]]
            [comportexviz.viz-canvas :as viz]
            [cljs.core.async :refer [chan <! >! timeout]]
            [clojure.set :as set])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;; initial CLA region
(def ncol 128)
(def bit-width 128)
(def item-radius-bits 16)
(def item-overlap 0.0)
(def input-bases
  (let [step-bits (* item-radius-bits 2 (- 1 item-overlap))]
    (range item-radius-bits bit-width step-bits)))

(defn rgn-init
  []
  (p/region (assoc p/spatial-pooler-defaults
              :ncol ncol
              :input-size bit-width
              :potential-radius (quot bit-width 2)
              :global-inhibition false
              :stimulus-threshold 2
              :duty-cycle-period 100)))

;; inputs

(defn input-init
  []
  (set (take 1 input-bases)))

(def alter-every 10)

(defn input-transform
  [xs t]
  (if (zero? (mod t alter-every))
    (let [one-or-nil #(when (seq %) (util/rand-nth %))]
      (-> xs
          (disj (one-or-nil (seq xs)))
          (conj (util/rand-nth input-bases))))
    ;; pass through unchanged
    xs))

(defn efn
  [xs]
  (->>
   (for [x xs
         :let [lo (max 0 (- x item-radius-bits))
               hi (min bit-width (+ x item-radius-bits))]]
     (set (range lo hi)))
   (apply set/union)))

(defn run-sim!
  []
  (go
   (loop [in (input-init)
          rgn (rgn-init)]
     (let [in-bits (efn in)
           new-rgn (p/pooling-step rgn in-bits)
           t (:timestep new-rgn)]
       (>! viz/sim-chan
           {:input in :inbits in-bits :region new-rgn})
       (recur (input-transform in t)
              new-rgn)))))

(run-sim!)
