(ns comportexviz.proxies
  (:require [cljs.core.async :as async :refer [chan put! <!]]
            [comportexviz.helpers :as helpers]
            [org.nfrac.comportex.protocols :as p])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;; Contents:
;; - A proxy interface for fetching data from remote data structures
;; - The proxy types, and the functions that create/populate them
;;
;;
;; Stage 1:
;; Just prefetch the entire model so that rendering can stay synchronous.
;;
;; TODO:
;; Make prefetching more selective, rendering more asynchronous.

(defprotocol PHasRemoteResources
  (release! [_]))

(defprotocol PProxy
  (fetch [_ route]))

(defn- get-method-result
  [m method & args]
  (let [{:keys [method-results]} m
        lookup (into [method] args)]
    (assert (contains? method-results lookup))
    (get method-results lookup)))

;;; ## layer

;; TODO

;;; ## model

(defrecord ModelProxy [original-proxy]
  PHasRemoteResources
  (release! [_]
    (release! original-proxy))

  PProxy
  (fetch [_ route]
    (fetch original-proxy route))

  p/PTemporal
  (timestep [this]
    (get-method-result this :timestep)))

(def model-prefetch
  {:values [:ff-deps :fb-deps :strata :inputs :regions :run-start]
   :methods [[:timestep]]})

(def model-transforms
  {:f map->ModelProxy})

(defn model-proxy
  [proxy]
  (let [out (chan)]
    (go
      (put! out
            (-> (<! (fetch proxy model-prefetch))
                (assoc :original-proxy proxy)
                (helpers/update-routed model-transforms
                                       (fn [m r]
                                         (if-let [f (:f r)]
                                           (f m)
                                           m))))))
    out))
