(ns comportexviz.server.runner
  (:require [clojure.core.async :as async :refer [put! <! go go-loop close!]]
            [compojure.route :as route]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [comportexviz.server.simulation :as simulation]
            [comportexviz.server.journal :as journal]
            [comportexviz.server.websocket :as server-ws]
            [comportexviz.util :as utilv]))

(defprotocol PStoppable
  (stop [_]))

(defn start
  ([model-atom input-c models-out-c opts]
   (let [into-journal (async/chan)
         into-sim (async/chan)
         models-in (async/chan)
         models-mult (async/mult models-in)
         channel-proxies (channel-proxy/registry
                          {:into-sim into-sim
                           :into-journal into-journal})
         server (server-ws/start channel-proxies
                                 (assoc opts
                                        :http-handler (route/files "/")))]
     (when models-out-c
       (async/tap models-mult models-out-c))
     (simulation/start models-in model-atom input-c into-sim)
     (journal/init (utilv/tap-c models-mult) into-journal model-atom)
     (reify
       PStoppable
       (stop [_]
         (.stop server)
         (close! into-journal)
         (close! into-sim)
         (close! models-in))))))
