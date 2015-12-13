(ns org.numenta.sanity.bridge.marshalling
  (:require #?(:clj [clojure.core.async :as async :refer [<! put! go go-loop]]
                    :cljs [cljs.core.async :as async :refer [<! put!]])
            #?(:clj
               [clojure.core.async.impl.protocols :as p]
               :cljs
               [cljs.core.async.impl.protocols :as p])
            [cognitect.transit :as transit])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

#?(:clj
   (defn random-uuid []
     (java.util.UUID/randomUUID)))

;;; ## For message senders / receivers

(defprotocol PMarshalled
  (release! [this]))

(defrecord ChannelMarshal [ch single-use? released?]
  PMarshalled
  (release! [this]
    (reset! released? true)))

(defn channel
  "Allows a channel to be marshalled across the network.

  When Client A serializes a ChannelMarshal, Client A will assign it a target-id
  and send the target-id across the network. When decoding the message, Client B
  will set the decoded ChannelMarshal's :ch field to a ChannelProxy. Any `put!`
  or `close!` on a ChannelProxy will be delivered across the network back to
  Client A.

  Client B should not send a ChannelMarshal back to Client A. It will create a
  silly chain of proxies. Use `channel-weak`.

  All of this assumes that the network code on both clients is using
  write-handlers and read-handlers that follow this protocol."
  ([ch]
   (channel ch false))
  ([ch single-use?]
   (ChannelMarshal. ch single-use? (atom false))))

(defrecord ChannelWeakMarshal [target-id])

(defn channel-weak
  "Allows a marshalled channel to be referred to without creating chains of
  proxies.

  When a client decodes a message containing a ChannelWeakMarshal, it will check
  if this target-id belongs to this client. If it does, this ChannelWeakMarshal
  will be 'decoded' into its original ChannelMarshal. This allows Client B to
  tell Client A 'If we get disconnected, send me this data blob on reconnect,
  and I'll remember you'. This allows the data blob to contain channels that are
  usable again on second send without causing a chain of proxies.

  All of this assumes that the network code on both clients is using
  write-handlers and read-handlers that follow this protocol."
  [target-id]
  (ChannelWeakMarshal. target-id))

(defrecord BigValueMarshal [resource-id value pushed? released?]
  PMarshalled
  (release! [_]
    (reset! released? true)))

(defn big-value
  "Put a value in a box labelled 'recipients should cache this so that I don't
  have to send it every time.'

  When Client B decodes a message containing a BigValueMarshal, it will save the
  value and tell Client A that it has saved the value. Later, when Client A
  serializes the same BigValueMarshal to send it to Client B, it will only
  include the resource-id, and Client B will reinsert the value when it decodes
  the message.

  Call `release!` on a BigValueMarshal to tell other machines that they can
  release it.

  All of this assumes that the network code on both clients is using
  write-handlers and read-handlers that follow this protocol."
  [value]
  (BigValueMarshal. (random-uuid) value false (atom false)))

;;; ## For networking

#?(:cljs
   (defn future [val]
     (reify cljs.core/IDeref
       (-deref [_] val))))

(deftype ImpersonateChannel [fput fclose ftake]
  p/ReadPort
  (take! [_ _]
    (future
      (ftake)))

  p/WritePort
  (put! [_ v _]
    (assert v)
    (future
      (fput v)))

  p/Channel
  (close! [_]
    (fclose)))

(defrecord ChannelProxy [target-id ch]
  p/ReadPort
  (take! [_ handler]
    (p/take! ch handler))

  p/WritePort
  (put! [_ v handler]
    (p/put! ch v handler))

  p/Channel
  (close! [_]
    (p/close! ch)))

;; Use `:json-verbose` to avoid Transit's caching, which has bugs.
;; This issue is tracked in https://github.com/cognitect/transit-cljs/issues/22
;; But there are other issues with `:json-verbose` in Python.
;; This issue is tracked in https://github.com/cognitect/transit-python/issues/23
;; So, for the moment, the transit-cljs bug isn't causing any known issues.
(def encoding :json)

(defn write-handlers
  [target->mchannel local-resources]
  {ChannelMarshal (transit/write-handler
                   (fn [_]
                     "ChannelMarshal")
                   (fn [mchannel]
                     (let [target-id (random-uuid)
                           released? (:released? mchannel)]
                       (if-not @released?
                         (do
                           (add-watch released? (random-uuid)
                                      (fn [_ _ _ r?]
                                        (when r?
                                          (swap! target->mchannel
                                                 dissoc target-id))))
                           (swap! target->mchannel assoc
                                  target-id mchannel)
                           target-id)
                         (println "Serializing a released channel!"
                                  mchannel)))))
   ChannelWeakMarshal (transit/write-handler
                       (fn [_]
                         "ChannelWeakMarshal")
                       (fn [wmchannel]
                         (:target-id wmchannel)))

   BigValueMarshal
   (transit/write-handler
    (fn [_]
      "BigValueMarshal")
    (fn [marshal]
      (let [{:keys [released? resource-id]} marshal]
        (when-not @released?
          (when-not (contains? @local-resources resource-id)
            (swap! local-resources assoc resource-id marshal)
            (add-watch released? local-resources
                       (fn [_ _ _ r?]
                         (when r?
                           (remove-watch released? local-resources)
                           (when-let [ch (get-in @local-resources
                                                 [resource-id :on-released-c])]
                             (put! ch [:release]))
                           (swap! local-resources dissoc resource-id))))))
        (if (get-in @local-resources [resource-id :pushed?])
          {"resource-id" resource-id}
          (let [saved-c (async/chan)]
            (go
              (when-let [[_ on-released-c-marshal] (<! saved-c)]
                (swap! local-resources update resource-id
                       (fn [marshal]
                         (cond-> (assoc marshal
                                        :pushed? true)
                           on-released-c-marshal
                           (assoc :on-released-c
                                  (:ch on-released-c-marshal)))))))
            {"resource-id" resource-id
             "value" (:value marshal)
             "on-saved-c-marshal" (channel saved-c true)})))))})

(defn read-handlers
  [target->mchannel fput fclose remote-resources]
  {"ChannelMarshal" (transit/read-handler
                     (fn [target-id]
                       (channel
                        (ChannelProxy. target-id
                                       (ImpersonateChannel.
                                        (fn [v]
                                          (fput target-id v))
                                        (fn []
                                          (fclose target-id))
                                        nil)))))
   "ChannelWeakMarshal" (transit/read-handler
                         (fn [target-id]
                           (if-let [mchannel (get @target->mchannel target-id)]
                             mchannel
                             (channel-weak target-id))))

   "BigValueMarshal"
   (transit/read-handler
    (fn [m]
      (let [{resource-id "resource-id"
             on-saved-c-marshal "on-saved-c-marshal"} m
            new? (not (contains? @remote-resources resource-id))]
        (when new?
          (swap! remote-resources assoc resource-id
                 (BigValueMarshal. resource-id (get m "value") nil nil)))
        (when on-saved-c-marshal
          ;; Depending on timing, this may happen multiple
          ;; times for a single resource. The other machine
          ;; wants to free up this channel, so always respond.
          ;; But only give it an on-released channel once.
          (let [on-released-c-marshal (when new?
                                        (let [ch (async/chan)]
                                          (go
                                            (when-not (nil? (<! ch))
                                              (swap! remote-resources
                                                     dissoc resource-id)))
                                          (channel ch)))]
            (put! (:ch on-saved-c-marshal) [:saved on-released-c-marshal])))
        (get @remote-resources resource-id))))})
