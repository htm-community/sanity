(ns comportexviz.bridge.remote
  (:require [cljs.core.async :as async :refer [put! close!]]
            [cognitect.transit :as transit]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [org.nfrac.comportex.topology :refer [map->OneDTopology
                                                  map->TwoDTopology
                                                  map->ThreeDTopology]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def handlers
  {"org.nfrac.comportex.topology.OneDTopology" map->OneDTopology
   "org.nfrac.comportex.topology.TwoDTopology" map->TwoDTopology
   "org.nfrac.comportex.topology.ThreeDTopology" map->ThreeDTopology})

(defn transit-str
  [m]
  (-> (transit/writer :json {:handlers channel-proxy/write-handler})
      (transit/write m)))

(defn read-transit-str
  [s extra-handlers]
  (-> (transit/reader :json {:handlers (merge handlers
                                              extra-handlers)})
      (transit/read s)))

(defn target-put
  [target v]
  [target :put! v])

(defn target-close
  [target]
  [target :close!])

(defn init
  [ws-url local-targets]
  (let [to-network-c (async/chan)
        connection-persistor-c (async/chan)
        reconnect-blob (atom nil)
        ;; Remote targets are often declared inside of messages, but you have to
        ;; jumpstart the process somehow. One machine needs to know about
        ;; targets on another machine before communication can start.
        pipe-to-remote-target! (fn pipe-to-remote-target [t ch]
                                 (go-loop []
                                   (let [v (<! ch)]
                                     (if-not (nil? v)
                                       (do
                                         (put! to-network-c (target-put t v))
                                         (recur))
                                       (put! to-network-c (target-close t))))))
        fconn (fn fconn []
                (let [ws (js/WebSocket. ws-url)]
                  (doto ws
                    (aset "onopen"
                          (fn [evt]
                            (println "WebSocket connected.")
                            (when @reconnect-blob
                              (println "Establishing reconnection."
                                       @reconnect-blob)
                              (put! connection-persistor-c [:restore-connection
                                                            @reconnect-blob]))
                            (go-loop []
                              (let [msg (<! to-network-c)]
                                (when (not (nil? msg))
                                  (if (= (.-readyState ws) js/WebSocket.OPEN)
                                    (do (let [out (transit-str msg)]
                                          (.send ws out))
                                        (recur))
                                    ;; put it back, stop listening
                                    (put! to-network-c msg)))))))
                    (aset "onerror"
                          (fn [evt]
                            (println "WebSocket error:")
                            (js/console.error evt)))
                    (aset "onclose"
                          (fn [evt]
                            (println "WebSocket closed. Reconnecting...")
                            (fconn)))
                    (aset "onmessage"
                          (fn [evt]
                            (let [[target op msg] (read-transit-str
                                                   (.-data evt)
                                                   (channel-proxy/read-handler
                                                    (fn [t v]
                                                      (put! to-network-c
                                                            (target-put t v)))
                                                    (fn [t]
                                                      (put! to-network-c
                                                            (target-close t)))))
                                  ch (get (channel-proxy/as-map local-targets)
                                          target)]
                              (case op
                                :put! (do
                                        ;; enumerate lazy tree
                                        ;; (dorun (tree-seq coll? seq msg))
                                        (put! ch msg))
                                :close! (close! ch))))))))]
    (pipe-to-remote-target! :connection-persistor connection-persistor-c)
    (let [reconnect-blobs-c (async/chan)]
      (put! connection-persistor-c [:subscribe-reconnect-blob
                                    (channel-proxy/register! local-targets
                                                             reconnect-blobs-c)])
      (go-loop []
        (let [new-blob (<! reconnect-blobs-c)]
          (when-not (nil? new-blob)
            (reset! reconnect-blob new-blob)
            (recur)))))
    (fconn)
    pipe-to-remote-target!))
