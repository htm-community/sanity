(ns comportexviz.main
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [comportexviz.controls-ui :as cui]
            [comportexviz.viz-canvas :as viz]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defn- tap-c
  [mult]
  (let [c (chan)]
    (async/tap mult c)
    c))

;;; ## Simulation data

(def model (atom nil))
(def world (atom (chan)))

(def steps-c (chan))
(def steps-mult (async/mult steps-c))

(def main-options
  (atom {:sim-go? false
         :sim-step-ms 20}))

;;; ## Viz data

(def model-steps (atom []))
(def selection (atom viz/blank-selection))
(def viz-options (atom viz/default-viz-options))
(def into-viz (chan))
(def from-viz (chan))

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

;;; ## Entry point

(go-loop []
  (when-let [command (<! from-viz)]
    (case command
      :toggle-run (swap! main-options update-in [:sim-go?] not)
      :sim-step (sim-step!))
    (recur)))

;; stream the simulation steps into the sliding history buffer
(let [steps-out (tap-c steps-mult)]
  (go-loop []
    (when-let [x* (<! steps-out)]
      (let [x (-> x*
                  viz/init-caches)
            keep-steps (:keep-steps @viz-options)]
        (swap! model-steps (fn [xs]
                             (take keep-steps (cons x xs)))))
      (recur))))

(defn main-pane [world-pane model-steps selection viz-options into-viz from-viz]
  [:div
   [viz/viz-timeline model-steps selection viz-options]
   [:div.row
    [:div.col-sm-3.col-lg-2
     [world-pane]]
    [:div.col-sm-9.col-lg-10
     [viz/viz-canvas {:tabIndex 1} model-steps selection viz-options
      into-viz from-viz]]]])

(defn comportexviz-app
  [model-tab world-pane]
  (let [m (fn [] [main-pane world-pane model-steps selection viz-options
                  into-viz from-viz])]
    (cui/comportexviz-app model-tab m model main-options viz-options
                          selection model-steps viz/state-colors into-viz)))

(defn set-model!
  [x]
  (reset! model x)
  (put! into-viz [:on-model-changed x]))

(defn selected-model-step
  []
  (nth @model-steps (:dt @selection) nil))
