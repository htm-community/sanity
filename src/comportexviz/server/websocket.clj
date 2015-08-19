(ns comportexviz.server.websocket
  (:require [clojure.core.async :as async :refer [put! <! go go-loop close!]]
            [cognitect.transit :as transit]
            [compojure.core :refer [routes GET]]
            [compojure.route :as route]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [ring.adapter.jetty9 :as jetty :refer [run-jetty]])
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

(defn connection-persistor
  [reconnect-blob connection-changes-c channel-proxies]
  (let [connection-persist-c (async/chan)
        reconnect-blob-subscribers (atom [])]
    (channel-proxy/register-chan channel-proxies :connection-persistor
                                 connection-persist-c)
    (go-loop []
      (let [[command & xs] (<! connection-persist-c)]
        (when (not (nil? command))
          (case command
            :ping
            nil

            :subscribe-reconnect-blob
            (let [[subscriber-c] xs]
              (swap! reconnect-blob-subscribers conj subscriber-c))

            :restore-connection
            (let [[recovered-reconnect-blob] xs]
              (put! connection-changes-c [[:client-reconnected]
                                          recovered-reconnect-blob])))
          (recur))))
    (add-watch reconnect-blob [(hash connection-persist-c) ::push-to-clients]
               (fn [_ _ _ v]
                 (doseq [subscriber-c @reconnect-blob-subscribers]
                   (put! subscriber-c v))))
    connection-persist-c))

(defn ws-output-chan
  [ws]
  (let [to-network-c (async/chan)]
    (go-loop []
      (let [msg (<! to-network-c)]
        (when (not (nil? msg))
          (jetty/send! ws msg)
          (recur))))
    to-network-c))

(defn ws-handler
  [channel-proxies connection-changes-c]
  (let [clients (atom {})]
   {:on-connect
    (fn [ws]
      (let [client-info (atom {})]
        (swap! clients assoc ws
               [client-info
                (ws-output-chan ws)
                (connection-persistor client-info connection-changes-c
                                      channel-proxies)])))

    :on-error
    (fn [ws e] (println e))

    :on-close
    (fn [ws status-code reason]
      (let [[client-info to-network-c connection-persist-c] (get @clients ws)]
        (close! to-network-c)
        (close! connection-persist-c)
        (put! connection-changes-c [[:client-disconnect] client-info]))
      (swap! clients dissoc ws))

    :on-text
    (fn [ws text]
      (let [[client-info to-network-c] (get @clients ws)
            [target op msg] (read-transit-str
                             text
                             (channel-proxy/read-handler
                              (fn [t v]
                                (put! to-network-c (transit-str
                                                    [t :put! v])))
                              (fn [t]
                                (put! to-network-c (transit-str
                                                    [t :close!])))))]
        (if-let [ch (channel-proxy/from-target channel-proxies target)]
          (case op
            :put! (put! ch [msg client-info])
            :close! (close! ch))
          (do
            (println "ERROR: Unrecognized target" target)
            (println "KNOWN TARGETS:" (channel-proxy/known-targets
                                       channel-proxies))))))

    :on-bytes
    (fn [ws bytes offset len])}))

(defn start
  ([channel-proxies connection-changes-c {:keys [http-handler port block?]}]
   (run-jetty (or http-handler
                  (routes (GET "/*" [] (str "Use WebSocket on port " port))))
              {:port port
               :websockets {"/ws" (ws-handler channel-proxies
                                              connection-changes-c)}
               :join? block?})))
