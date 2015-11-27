(ns org.numenta.sanity.bridge.marshalling
  (:require #?(:clj [clojure.core.async :as async]
                    :cljs [cljs.core.async :as async])
            #?(:clj
               [clojure.core.async.impl.protocols :as p]
               :cljs
               [cljs.core.async.impl.protocols :as p])
            [cognitect.transit :as transit])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

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

;;; ## For networking

#?(:cljs
   (defn future [val]
     (reify cljs.core/IDeref
       (-deref [_] val))))

#?(:clj
   (defn random-uuid []
     (java.util.UUID/randomUUID)))

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

(defn write-handlers [target->mchannel]
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
                         (:target-id wmchannel)))})

(defn read-handlers
  [target->mchannel fput fclose]
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
                             (channel-weak target-id))))})
