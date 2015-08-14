(ns comportexviz.bridge.remote
  (:require [cljs.core.async :as async :refer [put! close!]]
            [cognitect.transit :as transit]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [org.nfrac.comportex.topology :refer [map->OneDTopology
                                                  map->TwoDTopology]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def handlers
  {"org.nfrac.comportex.topology.OneDTopology" map->OneDTopology
   "org.nfrac.comportex.topology.TwoDTopology" map->TwoDTopology})

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
  [ws-url channel-proxies]
  (let [to-network-c (async/chan)
        connection-admin-c (async/chan)
        reconnect-blob (atom nil)
        remote-targets (atom {})
        fconn (fn fconn []
                (let [ws (js/WebSocket. ws-url)]
                  (doto ws
                    (aset "onopen"
                          (fn [evt]
                            (println "WebSocket connected.")
                            (when @reconnect-blob
                              (println "Establishing reconnection."
                                       @reconnect-blob)
                              (doseq [[t ch] @remote-targets]
                                (put! ch [:client-reconnect @reconnect-blob])))
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
                                                            (target-close t)))
                                                    ))
                                  ch (channel-proxy/from-target
                                      channel-proxies target)]
                              (case op
                                :put! (do
                                        ;; enumerate lazy tree
                                        ;; (dorun (tree-seq coll? seq msg))
                                        (put! ch msg))
                                :close! (close! ch))))))))]
    (go-loop []
      (let [v (<! connection-admin-c)]
        (when-not (nil? v)
          (let [[command & xs] v]
            (case command
              :reset-reconnect-blob (let [[blob] xs]
                                      (reset! reconnect-blob blob))))
          (recur))))
    (channel-proxy/register-chan channel-proxies :connection-admin
                                 connection-admin-c)
    (fconn)

    (fn register-remote-target! [t ch]
      (swap! remote-targets assoc t ch)
      (async/pipeline 1 to-network-c (map (partial target-put t)) ch false))))
