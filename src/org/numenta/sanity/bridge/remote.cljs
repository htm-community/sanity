(ns org.numenta.sanity.bridge.remote
  (:require [cljs.core.async :as async :refer [put! close!]]
            [cljs.pprint :refer [pprint]]
            [cognitect.transit :as transit]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.nfrac.comportex.topology :refer [map->OneDTopology
                                                  map->TwoDTopology
                                                  map->ThreeDTopology]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

;; Jetty's maxTextMessageBufferSize
(def max-message-size
  (* 64 1024))

(defn transit-str
  [m extra-handlers]
  (-> (transit/writer marshal/encoding {:handlers extra-handlers})
      (transit/write m)))

(defn read-transit-str
  [s extra-handlers]
  (-> (transit/reader marshal/encoding {:handlers extra-handlers})
      (transit/read s)))

(defn target-put
  [target v]
  ["put!" target v])

(defn target-close
  [target]
  ["close!" target])

(def log-messages? (atom false))
(def log-raw-messages? (atom false))
(def log-pretty? (atom true))

(defn log
  [v prefix]
  (pr prefix)
  ((if @log-pretty?
     pprint
     println) v)
  v)

(defn connect!
  [connection-id to-network-c on-connect-c ws-url connecting? target->mchannel]
  (let [ws (js/WebSocket. ws-url)
        teardown-c (async/chan)
        connection-id* (random-uuid)
        local-resources (atom {})
        remote-resources (atom {})]
    (doto ws
      (aset "onopen"
            (fn [evt]
              (println "WebSocket connected.")
              (reset! connection-id connection-id*)
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
                    (let [out (cond-> msg
                                @log-messages? (log "SENDING:")
                                true (transit-str (marshal/write-handlers
                                                   target->mchannel local-resources))
                                @log-raw-messages? (log "SENDING TEXT:"))
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
              (let [[op target msg] (cond-> (.-data evt)
                                      @log-raw-messages? (log "RECEIVED TEXT:")
                                      true (read-transit-str
                                            (marshal/read-handlers
                                             target->mchannel
                                             (fn [t v]
                                               (put! to-network-c
                                                     (target-put t v)))
                                             (fn [t]
                                               (put! to-network-c
                                                     (target-close t)))
                                             remote-resources))
                                      @log-messages? (log "RECEIVED:"))
                    {:keys [ch single-use?] :as mchannel} (@target->mchannel
                                                           target)]
                (if ch
                  (do (when single-use?
                        (marshal/release! mchannel))
                      (case op
                        "put!" (do
                                ;; enumerate lazy tree
                                ;; (dorun (tree-seq coll? seq msg))
                                (put! ch msg))
                        "close!" (close! ch)))
                  (do
                    (println "UNRECOGNIZED TARGET" target)
                    (println "Known targets:" @target->mchannel)))))))))

(defn init
  [ws-url]
  (let [to-network-c (async/chan (async/sliding-buffer 1024))
        connection-id (atom nil)
        on-connect-c (async/chan (async/sliding-buffer 1024))
        connecting? (atom false)
        target->mchannel (atom {})]
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
            blob-resets-cproxy (marshal/channel blob-resets-c)]
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
                                   (target-put t ["connect" @reconnect-blob
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
                              connecting? target->mchannel))))
              (when-not (nil? v)
                (recur)))))))))

(aset js/window "sanityLogMessages" #(swap! log-messages? not))
(aset js/window "sanityLogRawMessages" #(swap! log-raw-messages? not))
(aset js/window "sanityLogUgly" #(swap! log-pretty? not))

(js/console.log
 (str "Call sanityLogMessages() or sanityLogRawMessages() to display websocket "
      "traffic. Call sanityLogUgly() to condense the output."))
