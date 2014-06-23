(ns comportexviz.main
  (:require [c2.dom :as dom :refer [->dom]]
            [comportexviz.cla-model :as cla-model]
            [comportexviz.controls-ui]
            [comportexviz.viz-canvas :as viz]
            [comportexviz.plots :as plots]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defn- tap-c
  [mult]
  (let [c (chan)]
    (async/tap mult c)
    c))

;; ## DATA

(def model (atom nil))

(def steps-c (chan))
(def steps-mult (async/mult steps-c))

(def freqs-c (async/map< (comp cla-model/column-state-freqs :region)
                         (tap-c steps-mult)))
(def freqs-mult (async/mult freqs-c))
(def agg-freqs-ts (plots/aggregated-ts-ref (tap-c freqs-mult) 200))

(def sim-go? (atom false))
(def main-options
  (atom {:sim-step-ms 500
         :anim-go? true
         :anim-every 1}))

(def selection (atom {:cid nil :dt 0}))

;; ## ENTRY POINTS

(defn sim-step!
  []
  (->>
   (swap! model cla-model/step)
   (put! steps-c)))

(defn ^:export set-model
  [x]
  (->>
   (reset! model x)
   (put! steps-c)))

(defn draw!
  []
  (dom/request-animation-frame
   #(viz/draw! @selection)))

;; ## HELPERS

(defn update-ts-plot
  [agg-ts]
  (plots/bind-ts-plot "#plots" agg-ts 400 240
                      [:unpredicted :active :predicted]
                      viz/state-colors))

(defn init-ui!
  []
  (comportexviz.viz-canvas/init! (tap-c steps-mult) selection sim-step!)
  (comportexviz.controls-ui/init! model sim-go? main-options viz/keep-steps
                                  viz/viz-options sim-step! draw!))

(c2.event/on-load init-ui!)

(defn run-sim
  []
  (go
   (while @sim-go?
     (let [tc (async/timeout (:sim-step-ms @main-options))]
       (sim-step!)
       (<! tc)))))

;; ## TRIGGERS

(add-watch sim-go? :run-sim
           (fn [_ _ _ v]
             (when v (run-sim))))

(add-watch agg-freqs-ts :ts-plot
           (fn [_ _ _ v]
             (update-ts-plot v)))

(add-watch viz/viz-options :redraw
           (fn [_ _ _ _]
             (draw!)))

(add-watch selection :redraw
           (fn [_ _ _ _]
             (draw!)))

;; animation loop
(go (loop [c (tap-c steps-mult)]
      (when-let [state (<! c)]
        (let [t (:timestep (:region state))
              n (:anim-every @main-options)]
          (when (and (:anim-go? @main-options)
                     (zero? (mod t n)))
            (draw!)))
        (recur c))))
