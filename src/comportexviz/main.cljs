(ns comportexviz.main
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [comportexviz.controls-ui :as cui]
            [comportexviz.viz-canvas :as viz]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defn- tap-c
  [mult]
  (let [c (chan)]
    (async/tap mult c)
    c))

;;; ## Data

(def model (atom nil))
(def world (atom (chan)))

(def steps-c (chan))
(def steps-mult (async/mult steps-c))

(def main-options
  (atom {:sim-go? false
         :sim-step-ms 200}))

;;; ## Simulation

(defn sim-step!
  []
  (go
   (when-let [in-value (<! @world)]
     (->> (swap! model p/htm-step in-value)
          (put! steps-c)))))

(defn now [] (.getTime (js/Date.)))

(defn run-sim
  []
  (when @model
    (swap! model assoc
           :run-start {:time (now)
                       :timestep (p/timestep @model)}))
  (go
   (while (:sim-go? @main-options)
     (let [tc (async/timeout (:sim-step-ms @main-options))]
       (<! (sim-step!))
       (<! tc)))))

(add-watch main-options :run-sim
           (fn [_ _ old v]
             (when (and (:sim-go? v)
                        (not (:sim-go? old)))
               (run-sim))))

(def controls
  {:step-backward viz/step-backward!
   :step-forward #(viz/step-forward! sim-step!)
   :column-up #(swap! viz/selection update-in [:col]
                      (fn [x] (when (and x (pos? x)) (dec x))))
   :column-down #(swap! viz/selection update-in [:col]
                        (fn [x] (if x (inc x) 0)))
   :scroll-up #(viz/scroll! false)
   :scroll-down #(viz/scroll! true)
   :toggle-run #(swap! main-options update-in [:sim-go?] not)
   })

;;; ## Entry point

(defn main-pane [world-pane]
  (fn []
    [:div
     [viz/viz-timeline]
     [:div.row
      [:div.col-sm-3.col-lg-2
       [world-pane]]
      [:div.col-sm-9.col-lg-10
       [viz/viz-canvas {:tabIndex 1} controls]]]]))

(defn comportexviz-app
  [model-tab world-pane]
  (viz/init! (tap-c steps-mult))
  (cui/comportexviz-app model-tab (main-pane world-pane) model main-options
                        viz/viz-options viz/selection viz/model-steps controls
                        viz/state-colors))

(defn set-model!
  [x]
  (reset! model x)
  (viz/oh-look-the-model-changed! x))
