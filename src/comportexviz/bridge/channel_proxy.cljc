(ns comportexviz.bridge.channel-proxy
  (:require #?(:clj [clojure.core.async :as async]
               :cljs [cljs.core.async :as async])
            #?(:clj
               [clojure.core.async.impl.protocols :as p]
               :cljs
               [cljs.core.async.impl.protocols :as p])
            [cognitect.transit :as transit])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

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

(defprotocol PTargeted
  (target-id [_]))

(deftype ChannelProxy [target-id ch]
  p/ReadPort
  (take! [_ handler]
    (p/take! ch handler))

  p/WritePort
  (put! [_ v handler]
    (p/put! ch v handler))

  p/Channel
  (close! [_]
    (p/close! ch))

  PTargeted
  (target-id [_]
    target-id))

(defprotocol PChannelProxyFactory
  (from-chan [_ ch])
  (from-target [_ target-id]))

(defprotocol PChannelProxyRegistry
  (register-chan [_ target-id ch])
  (known-targets [_]))

(deftype ChannelProxyRegistry [target->proxy]
  PChannelProxyFactory
  (from-chan [this ch]
    (register-chan this (random-uuid) ch))
  (from-target [_ target-id]
    (get @target->proxy target-id))

  PChannelProxyRegistry
  (register-chan [_ target-id ch]
    (let [r (ChannelProxy.
             target-id
             (ImpersonateChannel. (fn [v]
                                    (assert v)
                                    (async/put! ch v))
                                  (fn []
                                    (async/close! ch)
                                    (swap! target->proxy
                                           dissoc target-id))
                                  nil))]
      (swap! target->proxy assoc target-id r)
      r))
  (known-targets [_]
    (keys @target->proxy)))

(defn registry
  "A mechanism for getting serializable channels. Allows you to
  specify network behavior to the serialized channels at decode time.

  The idea: the parts of a program that know about the network are
  often far-separated from the parts that know what's in a
  message. With this approach, the decode/deserialize code now knows
  when it sees a channel and can specify the proper behavior.

  The registry keeps a mapping of IDs to channels, so that when
  messages arrive from the network (generally coming from a remote
  channel-proxy) the message can be dispatched to the proper channel."
  ([]
   (ChannelProxyRegistry. (atom {})))
  ([target->chan]
   (let [r (ChannelProxyRegistry. (atom {}))]
     (doseq [[t c] target->chan]
       (register-chan r t c))
     r)))

;;; ## Transit helpers

(def channel-proxy-tag "ChannelProxy")

(def write-handler
  {ChannelProxy (transit/write-handler (fn [_]
                                         channel-proxy-tag)
                                       target-id)})

(defn read-handler [fput fclose]
  {channel-proxy-tag (transit/read-handler #(ChannelProxy.
                                             %
                                             (ImpersonateChannel.
                                              (partial fput %)
                                              (partial fclose %)
                                              nil)))})
