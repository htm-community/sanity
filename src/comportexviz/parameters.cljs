(ns comportexviz.parameters)

(def small
  {:ncol 300
   :input-size 200
   :potential-radius 100
   :activation-level 0.06
   :global-inhibition false
   :stimulus-threshold 2
   :duty-cycle-period 1000
   ;; sequence memory:
   :depth 5
   :new-synapse-count 10
   :activation-threshold 7
   :min-threshold 5
   :connected-perm 0.20
   :initial-perm 0.16
   :permanence-inc 0.05
   :permanence-dec 0.05
   })
