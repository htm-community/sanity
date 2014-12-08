(ns comportexviz.main
  (:require [c2.dom :as dom :refer [->dom]]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [comportexviz.controls-ui :as cui]
            [comportexviz.viz-canvas :as viz]
            [comportexviz.plots :as plots]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [c2.util :refer [bind!]]))

(enable-console-print!)

(defn- tap-c
  [mult]
  (let [c (chan)]
    (async/tap mult c)
    c))

;;; ## Data

(def model (atom nil))
(def world (atom (chan)))

(def selection (atom {:region nil, :layer nil, :dt 0, :col nil}))

(def steps-c (chan))
(def steps-mult (async/mult steps-c))

(def sim-go? (atom false))
(def main-options
  (atom {:sim-step-ms 200
         :anim-go? true
         :anim-every 1}))

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
  (swap! model assoc
         :run-start {:time (now)
                     :timestep (p/timestep @model)})
  (go
   (while @sim-go?
     (let [tc (async/timeout (:sim-step-ms @main-options))]
       (<! (sim-step!))
       (<! tc)))))

(add-watch sim-go? :run-sim
           (fn [_ _ _ v]
             (when v (run-sim))))

;;; ## Plots

(defn update-ts-plot
  [el agg-ts]
  (plots/bind-ts-plot el agg-ts 400 180
                      [:active :active-predicted :predicted]
                      viz/state-colors))

(defn init-plots!
  [init-model el]
  (bind! el
         [:div
          (for [[region-key rgn] (:regions init-model)
                layer-id (core/layers rgn)
                :let [uniqix (str (name region-key) (name layer-id))
                      el-id (str "comportex-plot-" uniqix)]]
            [:fieldset
             [:legend (str "Region " region-key " " layer-id)]
             [:div {:id el-id}]])])
  (doseq [[region-key rgn] (:regions init-model)
          layer-id (core/layers rgn)
          :let [uniqix (str (name region-key) (name layer-id))
                el-id (str "comportex-plot-" uniqix)]]
    (let [this-el (->dom (str "#" el-id))
          freqs-c (async/map< (comp #(core/column-state-freqs % layer-id)
                                    region-key
                                    :regions)
                              (tap-c steps-mult))
          agg-freqs-ts (plots/aggregated-ts-ref freqs-c 200)]
      (add-watch agg-freqs-ts :ts-plot
                 (fn [_ _ _ v]
                   (update-ts-plot this-el v))))))

;;; ## Visualisation

(defn draw!
  []
  (dom/request-animation-frame
   #(viz/draw! @selection)))

(add-watch viz/viz-options :redraw
           (fn [_ _ _ _]
             (draw!)))

(add-watch selection :redraw
           (fn [_ _ _ _]
             (draw!)))

;;; # Animation loop

(go (loop [c (tap-c steps-mult)]
      (when-let [state (<! c)]
        (let [t (p/timestep state)
              n (:anim-every @main-options)]
          (when (and (:anim-go? @main-options)
                     (zero? (mod t n)))
            (draw!)))
        (recur c))))

;;; ## Entry point

(defn- init-ui!
  [init-model]
  (init-plots! init-model (->dom "#comportex-plots"))
  (viz/init! init-model (tap-c steps-mult) selection sim-step!)
  (cui/handle-controls! model sim-go? main-options sim-step! draw!)
  (cui/handle-options! model viz/keep-steps viz/viz-options)
  (cui/handle-parameters! model selection))

(defn set-model
  [x]
  (let [init? (nil? @model)]
    (reset! model x)
    (if init?
      (init-ui! x))
    (let [region-key (first (p/region-keys x))
          layer-id (first (core/layers (get-in x [:regions region-key])))]
      (swap! selection assoc :region region-key :layer layer-id
             :dt 0 :col nil))))

(defn set-world
  [c]
  (when-let [old-c @world]
    (async/close! old-c))
  (reset! world c))
