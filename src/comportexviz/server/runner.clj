(ns comportexviz.server.runner
  (:require [clojure.core.async :as async :refer [put! <! go go-loop close!]]
            [cognitect.transit :as transit]
            [ring.adapter.jetty9 :as jetty :refer [run-jetty]]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [org.nfrac.comportex.core]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [comportexviz.server.simulation :as simulation]
            [comportexviz.server.journal :as journal]
            [comportexviz.util :as utilv])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]))

(def write-handlers
  (transit/record-write-handlers
   org.nfrac.comportex.topology.OneDTopology
   org.nfrac.comportex.topology.TwoDTopology))

(defn transit-str
  [m]
  (let [out (ByteArrayOutputStream.)
        writer (transit/writer out :json
                               {:handlers (merge write-handlers
                                                 channel-proxy/write-handler)})]
    (transit/write writer m)
    (.toString out)))

(defn read-transit-str
  [text extra-handlers]
  (let [in (-> text (.getBytes "UTF-8") ByteArrayInputStream.)
        reader (transit/reader in :json {:handlers extra-handlers})]
    (transit/read reader)))

(defn ws-handler
  [channel-proxies]
  (let [clients (atom {})]
   {:on-connect
    (fn [ws]
      (let [to-network-c (async/chan)]
        (go-loop []
          (let [msg (<! to-network-c)]
            (when (not (nil? msg))
              (jetty/send! ws msg)
              (recur))))
        (swap! clients assoc ws to-network-c)))

    :on-error
    (fn [ws e] (println e))

    :on-close
    (fn [ws status-code reason]
      (swap! clients dissoc ws))

    :on-text
    (fn [ws text]
      (let [to-network-c (get @clients ws)
            [target op msg] (read-transit-str
                             text
                             (channel-proxy/read-handler
                              (fn [t v]
                                (put! to-network-c (transit-str
                                                    [t :put! v])))
                              (fn [t]
                                (put! to-network-c (transit-str
                                                    [t :close!])))))]
        (let [ch (channel-proxy/from-target channel-proxies target)]
          (case op
            :put! (put! ch msg)
            :close! (close! ch)))))

    :on-bytes
    (fn [ws bytes offset len])}))

(defprotocol PStoppable
  (stop [_]))

(defn start
  ([model-atom input-c models-out-c {:keys [port block?]}]
   (let [into-journal (async/chan)
         into-sim (async/chan)
         models-in (async/chan)
         models-mult (async/mult models-in)

         channel-proxies (channel-proxy/registry {:into-sim into-sim
                                                  :into-journal into-journal})
         server (run-jetty (route/files "/")
                           {:port port
                            :websockets {"/ws" (ws-handler channel-proxies)}
                            :join? block?})]
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
