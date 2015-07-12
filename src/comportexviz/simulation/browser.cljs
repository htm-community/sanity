(ns comportexviz.simulation.browser
  (:require [cljs.core.async :as async :refer [chan put!]]
            [comportexviz.proxies :as proxy]
            [comportexviz.simulation.common :as common]))

(defn browser-instance-proxy
  [m]
  (reify
    proxy/PHasRemoteResources
    (release! [_] nil)

    proxy/PProxy
    (fetch [_ route]
      (let [c (chan)]
        (put! c (common/extract-data m route))
        c))))

(defn simulate-raw-models-onto-chan!
  [steps-c model world-c options commands-c]
  (let [model-atom (if (satisfies? IDeref model)
                     model
                     (atom model))
        sim-closed? (atom false)]
    (common/handle-commands commands-c model-atom sim-closed?)
    (common/simulation-loop model-atom world-c steps-c options sim-closed?))
  nil)

;; To end the simulation, close `world-c` and/or `commands-c`. If only one is
;; closed, the simulation may consume another value from the other before
;; closing.
(defn simulate-onto-chan!
  [steps-c model world-c options commands-c]
  (let [c (chan)]
    (async/pipeline 1 steps-c (map browser-instance-proxy) c)
    (simulate-raw-models-onto-chan! c model world-c options commands-c)))
