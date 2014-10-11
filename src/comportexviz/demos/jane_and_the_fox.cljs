(ns comportexviz.demos.jane-and-the-fox
  (:require [org.nfrac.comportex.demos.jane-and-the-fox :as demo]
            [org.nfrac.comportex.core :as core]
            [comportexviz.sentence-drawing :refer [draw-sentence-fn]]))

(def n-predictions 8)
(def n-ff-votes 5)

(def draw-input
  (draw-sentence-fn demo/split-sentences demo/terminator n-predictions
                    n-ff-votes))

(defn input-gen
  []
  (let [input (demo/input-gen)]
    (assoc input :comportexviz/draw-input draw-input)))

(defn ^:export n-region-model
  [n]
  (core/regions-in-series core/sensory-region (input-gen) n demo/spec))
