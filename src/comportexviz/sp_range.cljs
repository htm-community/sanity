(ns comportexviz.sp-range
  (:require [org.nfrac.comportex.pooling :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [comportexviz.viz-canvas :as viz]
            [cljs.core.async :refer [chan <! >! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;; initial CLA region
(def ncol 200)
(def numb-bits 64)
(def numb-on-bits 11)
(def numb-max 100)
(def numb-min 0)
(def numb-domain [numb-min numb-max])
(def n-in-items 3)
(def bit-width (* numb-bits n-in-items))

(def r-init
  (p/region (assoc p/spatial-pooler-defaults
              :ncol ncol
              :input-size bit-width
              :potential-radius (quot bit-width 5)
              :global-inhibition false
              :stimulus-threshold 2
              :duty-cycle-period 100)))

;; inputs

(def input-init (vec (repeat n-in-items (/ numb-max 2))))

(defn input-transform
  [xs]
  (mapv (fn [x]
          (mod (+ x 2) numb-max))
        xs))

(def efn
  (enc/juxtapose-encoder
   (enc/linear-number-encoder numb-bits numb-on-bits numb-domain)))

(defn run-sim!
  []
  (go
   (loop [in input-init
          rgn r-init]
     (let [in-bits (efn in)
           new-rgn (p/pooling-step rgn in-bits)]
       (>! viz/sim-chan
           {:input in :inbits in-bits :region new-rgn})
       (recur (input-transform in)
              new-rgn)))))
(run-sim!)
