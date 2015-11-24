(ns org.numenta.sanity.bridge.browser
  (:require [cljs.core.async :as async]
            [org.numenta.sanity.comportex.simulation :as simulation]
            [org.numenta.sanity.comportex.journal :as journal]
            [org.numenta.sanity.util :as utilv]
            [org.nfrac.comportex.protocols :as p]))

(defn init
  ([model world-c into-journal into-sim]
   (init
    model world-c into-journal into-sim p/htm-step))
  ([model world-c into-journal into-sim htm-step]
   (init
    model world-c into-journal into-sim htm-step nil))
  ([model world-c into-journal into-sim htm-step models-out]
   (assert (satisfies? IDeref model))
   (let [models-in (async/chan)
         models-mult (async/mult models-in)
         into-journal* (async/chan)
         into-sim* (async/chan)
         client-info (atom {}) ;; one client
         client-info-xf (map (fn [v] [v client-info]))]
     (async/pipeline 1 into-sim* client-info-xf into-sim)
     (async/pipeline 1 into-journal* client-info-xf into-journal)
     (when models-out
       (async/tap models-mult models-out))
     (simulation/start models-in model world-c into-sim* htm-step)
     (journal/init (utilv/tap-c models-mult) into-journal* model 50))))
