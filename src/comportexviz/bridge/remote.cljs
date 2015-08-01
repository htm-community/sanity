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

(defn init
  [ws-url into-journal into-sim channel-proxies]
  (let [id-max (atom 0)
        ws (js/WebSocket. ws-url)
        to-network-c (async/chan) ]
    (go-loop []
      (let [msg (<! to-network-c)]
        (when (not (nil? msg))
          (.send ws (transit-str msg))
          (recur))))

    (async/pipeline 10 to-network-c (map (fn [v] [:into-sim :put! v])) into-sim
                    false)
    (async/pipeline 10 to-network-c (map (fn [v] [:into-journal :put! v]))
                    into-journal false)

    (set! (.-onmessage ws)
          (fn [evt]
            (let [[target op msg] (read-transit-str
                                   (.-data evt)
                                   (channel-proxy/read-handler
                                    (fn [t v]
                                      (put! to-network-c
                                            [t :put! v]))
                                    (fn [t]
                                      (put! to-network-c
                                            [t :close!]))))]
              (let [ch (channel-proxy/from-target channel-proxies target)]
                (case op
                  :put! (put! ch msg)
                  :close! (close! ch))))))))
