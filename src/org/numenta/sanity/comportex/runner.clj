(ns org.numenta.sanity.comportex.runner
  (:require [clojure.core.async :as async :refer [put! <! go go-loop close!]]
            [compojure.route :as route]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.comportex.simulation :as simulation]
            [org.numenta.sanity.comportex.journal :as journal]
            [org.numenta.sanity.comportex.websocket :as server-ws]
            [org.numenta.sanity.util :as utilv]))

(defprotocol PStoppable
  (stop [_]))

(defn start
  ([model-atom input-c htm-step models-out-c opts]
   (let [into-journal (async/chan)
         into-sim (async/chan)
         models-in (async/chan)
         models-mult (async/mult models-in)
         connection-changes-c (async/chan)
         connection-changes-mult (async/mult connection-changes-c)
         ch->mchannel (atom {"simulation" (marshal/channel into-sim)
                             "journal" (marshal/channel into-journal)})
         server (server-ws/start ch->mchannel connection-changes-c
                                 (assoc opts
                                        :http-handler (route/files "/")))]
     (when models-out-c
       (async/tap models-mult models-out-c))
     (async/tap connection-changes-mult into-journal)
     (async/tap connection-changes-mult into-sim)
     (simulation/start models-in model-atom input-c into-sim htm-step)
     (journal/init (utilv/tap-c models-mult) into-journal model-atom 50)
     (reify
       PStoppable
       (stop [_]
         (.stop server)
         (close! into-journal)
         (close! into-sim)
         (close! models-in))))))
