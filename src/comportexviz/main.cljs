(ns comportexviz.main
  (:require [comportexviz.controls-ui :as cui]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [comportexviz.viz-canvas :as viz]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

;;; ## Journal data

(def into-journal (atom nil))
(def local-targets (channel-proxy/registry))

;;; ## Viz data

(def steps (atom []))
(def step-template (atom nil))
(def selection (atom viz/blank-selection))
(def viz-options (atom viz/default-viz-options))
(def into-viz (chan))

;;; ## Connect journal to viz

(defn subscribe-to-steps! [into-j]
  (let [steps-c (async/chan)
        response-c (async/chan)]
    (put! into-j [:subscribe (:keep-steps @viz-options)
                  (channel-proxy/register! local-targets steps-c)
                  (channel-proxy/register! local-targets response-c)])
    (go
      ;; Get the template before getting any steps.
      (reset! step-template (<! response-c))
      (let [[region-key rgn] (-> @step-template :regions seq first)
            layer-id (-> rgn keys first)]
        (swap! selection assoc
               :dt 0
               :region region-key
               :layer layer-id))

      (add-watch viz-options ::keep-steps
                 (fn [_ _ prev-opts opts]
                   (let [n (:keep-steps opts)]
                     (when (not= n (:keep-steps prev-opts))
                       (put! into-j [:set-keep-steps n])))))

      (loop []
        (when-let [step (<! steps-c)]
          (let [keep-steps (:keep-steps @viz-options)
                [kept dropped] (split-at keep-steps
                                         (cons step @steps))]
            (reset! steps kept))
          (recur))))
    steps-c))

(defn unsubscribe! [subscription-data]
  (let [steps-c subscription-data]
    (async/close! steps-c))
  (remove-watch viz-options ::keep-steps))

;;; ## Entry point

(add-watch steps ::recalculate-selection
           (fn [_ _ _ steps]
             (swap! selection
                    (fn [sel]
                      (assoc sel :model-id
                             (:model-id
                              (nth steps (:dt sel))))))))

(let [subscription-data (atom nil)]
  (add-watch into-journal ::subscribe-to-steps
             (fn [_ _ _ into-j]
               (swap! subscription-data
                      (fn [sd]
                        (when sd
                          (unsubscribe! sd))
                        (subscribe-to-steps! into-j))))))

;;; ## Components

(defn main-pane [world-pane into-sim]
  [:div
   [viz/viz-timeline steps selection viz-options]
   [:div.row
    [:div.col-sm-3.col-lg-2
     [world-pane]]
    [:div.col-sm-9.col-lg-10
     [viz/viz-canvas {:style {:width "100%" :height "100vh"} :tabIndex 1} steps
      selection step-template viz-options into-viz into-sim into-journal
      local-targets]]]])

(defn comportexviz-app
  [model-tab world-pane into-sim]
  (let [m (fn [] [main-pane world-pane into-sim])]
    [cui/comportexviz-app model-tab m viz-options selection
     steps step-template viz/state-colors into-viz into-sim into-journal
     local-targets]))

;;; ## Exported helpers

(defn selected-step
  []
  (when-let [dt (:dt @selection)]
    (nth @steps dt nil)))
