;; gorilla-repl.fileformat = 1

;; **
;;; # Comportex
;;;
;; **

;; **
;;; Start pressing \[shift\]+\[enter\]
;; **

;; @@
(ns my-notebook
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [org.numenta.sanity.comportex.launchpad :refer [start-runner
                                                            stop-all-runners]]
            [org.numenta.sanity.comportex.notebook :refer [viz]]
            [gorilla-plot.core :as plot]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as e]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.repl])
  (:use [clojure.pprint]
        [clojure.stacktrace]))

(org.nfrac.comportex.repl/truncate-large-data-structures)
;; @@

;; **
;;; ## Configure
;; **

;; @@
(def params
  {:column-dimensions [800]
   :spatial-pooling :local-inhibition
   :depth 5
   :distal {:max-segments 5
            :stimulus-threshold 5
            :learn-threshold 3}})

(def encoder (e/linear-encoder [200] 10 [0 24]))

(def model (core/regions-in-series
            1 core/sensory-region
            [params] {:input [[] encoder]}))

(def inputs (flatten (repeat (range 0 12))))
;; @@

;; **
;;; ## Interface 1: Notebook
;; **

;; @@
(viz encoder 2)
;; @@

;; @@
(for [v (range 12)]
  [v (viz encoder v)])
;; @@

;; @@
(viz model)
;; @@

;; @@
(viz model {:ff-synapses {:inactive true}})

;; command-click input bits and columns to multi-select.
;; (ctrl-click on Windows)
;; @@

;; @@
(def timeline
  (reductions p/htm-step model inputs))
;; @@

;; @@
(viz (take 50 timeline))
;; @@

;; @@
(viz (->> timeline
          (drop 1000)
          (take 50))
     {:ff-synapses {:to :all}})
;; @@

;; @@
(defn t->input [t]
  (-> (nth timeline t)
      :input-value))

(defn t->prediction [t]
  (when (> t 0)
    (-> (nth timeline (dec t))
        (core/predictions :input 1)
        first
        :value)))

(defn plot-predictions [start n]
  (let [r (range start (+ start n) 1)]
    (plot/compose
     (plot/list-plot (->> r
                          (map (fn [i]
                                 [i (t->input i)])))
                     :plot-size 700
                     :joined true
                     :color "red"
                     :opacity 0.8)
     (plot/list-plot (->> r
                          (map (fn [i]
                                 [i (t->prediction i)])))
                     :plot-size 700
                     :joined false
                     :color "blue"
                     :opacity 0.8))))
;; @@

;; @@
(plot-predictions 0 50)
;; @@

;; @@
(plot-predictions 1000 50)
;; @@

;; **
;;; ## Interface 2: Runner
;; **

;; @@
(start-runner model inputs)
;; @@

;; **
;;; ## Debug
;; **

;; @@
(stop-all-runners)
;; @@
