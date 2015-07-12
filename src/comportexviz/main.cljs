(ns comportexviz.main
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [comportexviz.controls-ui :as cui]
            [comportexviz.helpers :refer [tap-c]]
            [comportexviz.proxies :as proxy]
            [comportexviz.viz-canvas :as viz]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

;;; ## Simulation data

(def steps-c (atom nil))

(def sim-options
  (atom {:go? false
         :step-ms 20
         :force-n-steps 0}))

;;; ## Viz data

(def model-steps (atom []))
(def selection (atom viz/blank-selection))
(def viz-options (atom viz/default-viz-options))
(def into-viz (chan))
(def into-viz-mult (async/mult into-viz))
(def from-viz (chan))

;;; ## Entry point

(go-loop []
  (when-let [command (<! from-viz)]
    (case command
      :toggle-run (swap! sim-options update :go? not)
      :sim-step (swap! sim-options update :force-n-steps inc))
    (recur)))

(add-watch steps-c :simulation-change
           (fn [_ _ _ ch] ;; stream the simulation steps into the sliding history buffer
             (put! into-viz [:on-model-changed])
             (go-loop []
               (when-let [x* (<! ch)]
                 (let [x (-> (<! (proxy/model-proxy x*))
                             viz/init-caches)
                       keep-steps (:keep-steps @viz-options)
                       [kept dropped] (split-at keep-steps (cons x @model-steps))]
                   (reset! model-steps kept)
                   (mapv proxy/release! dropped))
                 (recur)))))

(defn main-pane [world-pane model-steps selection viz-options into-viz-mult
                 from-viz]
  [:div
   [viz/viz-timeline model-steps selection viz-options]
   [:div.row
    [:div.col-sm-3.col-lg-2
     [world-pane]]
    [:div.col-sm-9.col-lg-10
     [viz/viz-canvas {:tabIndex 1} model-steps selection viz-options
      into-viz-mult from-viz]]]])

(defn comportexviz-app
  [model-tab world-pane into-sim]
  (let [m (fn [] [main-pane world-pane model-steps selection viz-options
                  into-viz-mult from-viz])]
    [cui/comportexviz-app model-tab m sim-options viz-options selection
     model-steps viz/state-colors into-viz into-sim]))

(defn selected-model-step
  []
  (nth @model-steps (:dt @selection) nil))
