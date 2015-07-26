(ns comportexviz.util
  (:require #?(:clj [clojure.core.async :as async]
               :cljs [cljs.core.async :as async])))

(defn tap-c
  [mult]
  (let [c (async/chan)]
    (async/tap mult c)
    c))

(defn close-and-reset! [chan-atom v]
  (swap! chan-atom (fn [c]
                     (when c
                       (async/close! c))
                     v)))

(defn index-of
  [coll pred]
  (first (->> coll
              (keep-indexed (fn [i v]
                              (when (pred v)
                                i))))))
