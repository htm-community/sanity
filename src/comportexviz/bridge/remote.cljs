(ns comportexviz.bridge.remote
  (:require [cljs.core.async :as async :refer [put! close!]]
            [cognitect.transit :as transit]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [org.nfrac.comportex.topology :refer [map->OneDTopology
                                                  map->TwoDTopology
                                                  map->ThreeDTopology]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

;; Jetty's maxTextMessageBufferSize
(def max-message-size
  (* 64 1024))

(defn transit-str
  [m]
  (-> (transit/writer :json {:handlers channel-proxy/write-handler})
      (transit/write m)))

(defn read-transit-str
  [s extra-handlers]
  (-> (transit/reader :json {:handlers extra-handlers})
      (transit/read s)))

(defn target-put
  [target v]
  [target :put! v])

(defn target-close
  [target]
  [target :close!])

(defn connect!
  [connection-id to-network-c on-connect-c ws-url connecting? local-targets]
  (let [ws (js/WebSocket. ws-url)
        teardown-c (async/chan)
        id (random-uuid)]
    (doto ws
      (aset "onopen"
            (fn [evt]
              (println "WebSocket connected.")
              (reset! connection-id id)
              (reset! connecting? false)
              (go-loop []
                (alt! teardown-c nil
                      on-connect-c ([on-connect-f]
                                    (on-connect-f)
                                    (recur))
                      :priority true))
              (go-loop []
                (let [msg (alt! teardown-c nil
                                to-network-c ([v] v)
                                :priority true)]
                  (when-not (nil? msg)
                    (let [out (transit-str msg)
                          c (count out)]
                      (when (> c max-message-size)
                        ;; No recovery. Either the max is too low or something
                        ;; has gone wrong, e.g. leaking onto the reconnect-blob.
                        (js/alert (str "Message too large! Size: " c
                                       "Max-size: " max-message-size))
                        (println "Message too large!" out))
                      (.send ws out)
                      (recur)))))))
      (aset "onerror"
            (fn [evt]
              (println "WebSocket error:")
              (js/console.error evt)))
      (aset "onclose"
            (fn [evt]
              (reset! connection-id nil)
              (reset! connecting? false)
              (close! teardown-c)
              (println "WebSocket closed.")))
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
                  :close! (close! ch))))))))

(defn init
  [ws-url local-targets]
  (let [to-network-c (async/chan)
        connection-id (atom nil)
        on-connect-c (async/chan)
        connecting? (atom false)]
    ;; Remote targets are often declared inside of messages, but you have to
    ;; jumpstart the process somehow. One machine needs to know about targets on
    ;; another machine before communication can start.
    (fn pipe-to-remote-target [t ch]
      (let [last-seen-connection-id (atom nil)
            ;; The server stores state about this client for two reasons:
            ;; - Necessary to support subscribe functionality
            ;; - Decreases the size of many messages, making the whole
            ;;   experience faster
            ;;
            ;; The server throws this state away when the websocket closes.
            ;; It doesn't know if this client plans to reconnect.
            ;;
            ;; So the client subscribes to its own server-side data, and sends
            ;; it back on reconnect.
            reconnect-blob (atom nil)
            blob-resets-c (async/chan)
            blob-resets-cproxy (channel-proxy/register! local-targets
                                                        blob-resets-c)]
        (go-loop []
          (let [v (<! blob-resets-c)]
            (when-not (nil? v)
              (reset! reconnect-blob v)
              (recur))))
        (go-loop []
          (let [v (<! ch)]
            (let [action (fn []
                           ;; Use a callback because we want a connection-id
                           ;; immediately available so that we don't put
                           ;; multiple connects.
                           (when (or (nil? @last-seen-connection-id)
                                     (not= @connection-id
                                           @last-seen-connection-id))
                             (put! to-network-c
                                   (target-put t [:connect @reconnect-blob
                                                  blob-resets-cproxy]))
                             (reset! last-seen-connection-id @connection-id))
                           (if-not (nil? v)
                             (put! to-network-c (target-put t v))
                             (put! to-network-c (target-close t))))]
              (if @connection-id
                (action)
                (do
                  (put! on-connect-c action)
                  (when-not @connecting?
                    (reset! connecting? true)
                    (connect! connection-id to-network-c on-connect-c ws-url
                              connecting? local-targets))))
              (when-not (nil? v)
                (recur)))))))))
