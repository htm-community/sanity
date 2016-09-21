(ns org.numenta.sanity.util
  (:require [clojure.core.async :as async]
            [clojure.walk :refer [keywordize-keys]]))

(defn tap-c
  [mult]
  (let [c (async/chan)]
    (async/tap mult c)
    c))

(defn index-of
  [coll pred]
  (first (->> coll
              (keep-indexed (fn [i v]
                              (when (pred v)
                                i))))))

(defn translate-network-shape
  [n-shape-from-server]
  ;; keywordize the network-shape, but don't mangle layer / sense IDs
  (let [{layers "layers" senses "senses"} n-shape-from-server]
    {:layers (into {}
                   (for [[lyr-id lyr] layers]
                      [lyr-id (keywordize-keys lyr)]))
     :senses (into {}
                   (for [[sense-id sense] senses]
                     [sense-id (keywordize-keys sense)]))}))
