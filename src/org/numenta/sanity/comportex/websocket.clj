(ns org.numenta.sanity.comportex.websocket
  (:require [clojure.core.async :as async :refer [put! <! go go-loop close!]]
            [cognitect.transit :as transit]
            [compojure.core :refer [routes GET]]
            [compojure.route :as route]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [ring.adapter.jetty9 :as jetty :refer [run-jetty]])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]))

(defn transit-str
  [m extra-handlers]
  (let [out (ByteArrayOutputStream.)
        writer (transit/writer out :json
                               {:handlers extra-handlers})]
    (transit/write writer m)
    (.toString out)))

(defn read-transit-str
  [text extra-handlers]
  (let [in (-> text (.getBytes "UTF-8") ByteArrayInputStream.)
        reader (transit/reader in :json {:handlers extra-handlers})]
    (transit/read reader)))

(defn ws-output-chan
  [ws]
  (let [to-network-c (async/chan)]
    (go-loop []
      (let [msg (<! to-network-c)]
        (when (not (nil? msg))
          (jetty/send! ws msg)
          (recur))))
    to-network-c))

(def all-websockets-for-testing (atom #{}))
(defn sever-all-connections-for-testing! []
  (let [all-ws @all-websockets-for-testing]
    (reset! all-websockets-for-testing (empty all-ws))
    (doseq [ws all-ws]
      (jetty/close! ws))))

(defn ws-handler
  [target->mchannel connection-changes-c]
  (let [clients (atom {})]
   {:on-connect
    (fn [ws]
      (swap! all-websockets-for-testing conj ws)
      (swap! clients assoc ws
             [(java.util.UUID/randomUUID)
              (ws-output-chan ws)
              (atom {})
              (atom {})]))

    :on-error
    (fn [ws e] (println e))

    :on-close
    (fn [ws status-code reason]
      (swap! all-websockets-for-testing disj ws)
      (let [[client-id to-network-c] (get @clients ws)]
        (close! to-network-c)
        (put! connection-changes-c [[:client-disconnect] client-id]))
      (swap! clients dissoc ws))

    :on-text
    (fn [ws text]
      (let [[client-id to-network-c
             local-resources remote-resources] (get @clients ws)
            [target op msg] (read-transit-str
                             text
                             (marshal/read-handlers
                              target->mchannel
                              (fn [t v]
                                (put! to-network-c (transit-str
                                                    [t :put! v]
                                                    (marshal/write-handlers
                                                     target->mchannel
                                                     local-resources))))
                              (fn [t]
                                (put! to-network-c (transit-str
                                                    [t :close!]
                                                    (marshal/write-handlers
                                                     target->mchannel))))
                              remote-resources))]
        (if-let [{:keys [ch single-use?] :as mchannel} (@target->mchannel
                                                        target)]
          (do
            (when single-use?
              (marshal/release! mchannel))
            (case op
              :put! (put! ch [msg client-id])
              :close! (close! ch)))
          (do
            (println "ERROR: Unrecognized target" target)
            (println "KNOWN TARGETS:" @target->mchannel)))))

    :on-bytes
    (fn [ws bytes offset len])}))

(defn start
  ([target->mchannel connection-changes-c {:keys [http-handler port block?]}]
   (run-jetty (or http-handler
                  (routes (GET "/*" [] (str "Use WebSocket on port " port))))
              {:port port
               :websockets {"/ws" (ws-handler target->mchannel
                                              connection-changes-c)}
               :join? block?})))
