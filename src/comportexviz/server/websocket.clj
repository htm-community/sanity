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

(defn ws-handler
  [channel-proxies]
  (let [clients (atom {})]
   {:on-connect
    (fn [ws]
      (let [to-network-c (async/chan)
            client-info (atom {})]
        (go-loop []
          (let [msg (<! to-network-c)]
            (when (not (nil? msg))
              (jetty/send! ws msg)
              (recur))))
        (add-watch client-info ::push-to-client
                   (fn [_ _ _ v]
                     (put! to-network-c (transit-str
                                         [:connection-admin :put!
                                          [:reset-reconnect-blob v]]))))
        (swap! clients assoc ws
               [to-network-c client-info])))

    :on-error
    (fn [ws e] (println e))

    :on-close
    (fn [ws status-code reason]
      (let [[_ client-info] (get @clients ws)]
        (doseq [target (channel-proxy/known-targets channel-proxies)
                :let [ch (channel-proxy/from-target channel-proxies target)]]
          (put! ch [[:client-disconnect] client-info])))
      (swap! clients dissoc ws))

    :on-text
    (fn [ws text]
      (let [[to-network-c client-info] (get @clients ws)
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
  ([channel-proxies {:keys [http-handler port block?]}]
   (run-jetty (or http-handler
                  (routes (GET "/*" [] (str "Use WebSocket on port " port))))
              {:port port
               :websockets {"/ws" (ws-handler channel-proxies)}
               :join? block?})))
