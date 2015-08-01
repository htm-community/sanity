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

(def last-target-id (atom 24601))

(defprotocol PChannelProxyFactory
  (from-chan [_ ch])
  (from-target [_ target-id]))

(defprotocol PChannelProxyRegistry
  (register-chan [_ target-id ch]))

(deftype ChannelProxyRegistry [target->proxy]
  PChannelProxyFactory
  (from-chan [this ch]
    (register-chan this (swap! last-target-id inc) ch))
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
      r)))

(defn registry
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
  {channel-proxy-tag (transit/read-handler #(ImpersonateChannel.
                                             (partial fput %)
                                             (partial fclose %)
                                             nil))})
