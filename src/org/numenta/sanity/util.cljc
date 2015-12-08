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

(defn keywordize-keys*
  "Recursively transforms all map keys from strings to keywords.
  Like `clojure.walk/keywordize-keys`, but does not transform records."
  [m]
  (let [f (fn [[k v]] (if (string? k) [(keyword k) v] [k v]))]
    ;; only apply to maps, and don't apply to records
    (postwalk (fn [x] (if (and (map? x) (not (record? x)))
                        (into {} (map f x))
                        x))
              m)))

(defn stringify-keys*
  "Recursively transforms all map keys from keywords to strings.
  Like `clojure.walk/stringify-keys`, but does not transform records."
  [m]
  (let [f (fn [[k v]] (if (keyword? k) [(name k) v] [k v]))]
    ;; only apply to maps, and don't apply to records
    (postwalk (fn [x] (if (and (map? x) (not (record? x)))
                        (into {} (map f x))
                        x))
              m)))
