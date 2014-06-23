(ns comportexviz.cla-model
  (:require [org.nfrac.comportex.core :as core]))

(defprotocol PInputGenerator
  "Maintains an input stream and its encoding into bit sets."
  (bit-width [this])
  (bits-value [this])
  (domain-value [this])
  (input-step [this]))

(defrecord InputGenerator [value transform encode options]
  PInputGenerator
  (bit-width [_] (:bit-width options))
  (bits-value [_] (encode value))
  (domain-value [_] value)
  (input-step [this] (assoc this :value (transform value))))

(defn generator
  [value transform encode options]
  (->InputGenerator value transform encode options))

(defn cla-model
  [ingen spec]
  (let [r (core/cla-region spec)]
    {:region r
     :in ingen}))

(defn step
  [model]
  (let [nin (input-step (:in model))
        ppc (:predictive-cells-by-column (:region model))]
    (-> model
        (assoc :in nin)
        (update-in [:region] core/cla-step (bits-value nin))
        (assoc-in [:region :prev-predictive-cells-by-column] ppc))))

(defn column-state-freqs
  [rgn]
  (let [pm (zipmap (keys (:prev-predictive-cells-by-column rgn))
                   (repeat :predicted))
        am (zipmap (:active-columns rgn) (repeat :active))
        bm (zipmap (:bursting-columns rgn) (repeat :unpredicted))
        m (merge pm am bm)]
    (-> (frequencies (vals m))
        (assoc :timestep (:timestep rgn)
               :ncol (count (:columns rgn))))))

