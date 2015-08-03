(ns comportexviz.bridge.browser
  (:require [cljs.core.async :as async]
            [comportexviz.server.simulation :as simulation]
            [comportexviz.server.journal :as journal]
            [comportexviz.util :as utilv]))

(defn init
  ([model world-c into-journal into-sim]
   (init model world-c into-journal into-sim nil))
  ([model world-c into-journal into-sim models-out]
   (let [model-atom (if (satisfies? IDeref model)
                      model
                      (atom model))
         models-in (async/chan)
         models-mult (async/mult models-in)
         into-journal* (async/chan)
         into-sim* (async/chan)
         client-info (atom {}) ;; one client
         client-info-xf (map (fn [v] [v client-info]))]
     (async/pipeline 1 into-sim* client-info-xf into-sim)
     (async/pipeline 1 into-journal* client-info-xf into-journal)
     (when models-out
       (async/tap models-mult models-out))
     (simulation/start models-in model-atom world-c into-sim*)
     (journal/init (utilv/tap-c models-mult) into-journal* model-atom))))
