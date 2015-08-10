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
            [comportexviz.server.launchpad :refer [start-runner]]
            [comportexviz.server.notebook :refer [viz]]
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
(def spec
  {:column-dimensions [800]
   :ff-potential-radius 0.2
   :ff-perm-inc 0.05
   :ff-perm-dec 0.01
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 3
   :global-inhibition? false
   :activation-level 0.04
   :boost-active-every 10000
   :depth 5
   :max-segments 5
   :seg-max-synapse-count 18
   :seg-new-synapse-count 12
   :seg-stimulus-threshold 5
   :seg-learn-threshold 3
   :distal-perm-connected 0.20
   :distal-perm-inc 0.05
   :distal-perm-dec 0.01
   :distal-perm-init 0.16})

(def encoder (e/linear-encoder 200 10 [0 24]))

(def model (core/regions-in-series
                  core/sensory-region
                  (core/sensory-input encoder) 1
                  (repeat spec)))

(viz [model])

(def inputs (flatten (repeat (range 0 12))))
;; @@

;; **
;;; ## Interface 1: Notebook
;; **

;; @@
(def timeline
  (reductions p/htm-step model inputs))

(defn steps* [s l]
  (->> timeline
       (drop s)
       (take l)))

(def steps (comp viz steps*))
;; @@

;; @@
(steps 0 50)
;; @@

;; @@
(steps 1000 50)
;; @@

;; @@
(def predicted-inputs (->> timeline
                           (map #(-> %
                                     (core/predictions 1)
                                     first
                                     :value))))

(defn plot-predictions [range-start length]
  (let [range-start (max 1 range-start) ;; need a previous step for predictions
        r (range range-start (+ range-start length) 1)]
    (plot/compose
     (plot/list-plot (->> r
                          (map (fn [i]
                                 [i (nth inputs i)])))
                     :plot-size 800
                     :joined true
                     :color "red"
                     :opacity 0.8)
     (plot/list-plot (->> r
                          (map (fn [i]
                                 [i (nth predicted-inputs (dec i))])))
                     :plot-size 800
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
