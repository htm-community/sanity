(ns org.numenta.sanity.util
  (:require #?(:clj [clojure.core.async :as async]
               :cljs [cljs.core.async :as async])
            [clojure.walk :refer [postwalk]]))

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
