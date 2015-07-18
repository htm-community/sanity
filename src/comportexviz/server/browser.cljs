(ns comportexviz.server.browser
  (:require [cljs.core.async :as async]
            [comportexviz.helpers :as helpers]
            [comportexviz.server.simulation :as simulation]
            [comportexviz.server.journal :as journal]))

(defn init
  ([model world-c into-journal into-sim sim-options]
   (init model world-c into-journal into-sim sim-options nil))
  ([model world-c into-journal into-sim sim-options models-out]
   (let [model-atom (if (satisfies? IDeref model)
                      model
                      (atom model))
         models-in (async/chan)
         models-mult (async/mult models-in)]
     (when models-out
       (async/tap models-mult models-out))
     (simulation/start models-in model-atom world-c sim-options into-sim)
     (journal/init (helpers/tap-c models-mult) into-journal model-atom))))
