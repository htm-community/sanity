(ns comportexviz.main
  (:require [comportexviz.controls-ui :as cui]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [comportexviz.helpers :as helpers :refer [window-resize-listener]]
            [comportexviz.selection :as sel]
            [comportexviz.viz-canvas :as viz]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

;;; ## Journal data

(def into-journal (async/chan))
(def local-targets (channel-proxy/registry))

;;; ## Viz data

(def steps (atom []))
(def step-template (atom nil))
(def selection (atom sel/blank-selection))
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
        (reset! selection
                [{:dt 0
                  :path [:regions region-key layer-id]}]))
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

;; not sure why this would be used, but for completeness...
(def subscription-data (subscribe-to-steps! into-journal))

(defn unsubscribe! [subscription-data]
  (let [steps-c subscription-data]
    (async/close! steps-c))
  (remove-watch viz-options ::keep-steps))

;;; ## Entry point

(add-watch steps ::recalculate-selection
           (fn [_ _ _ steps]
             (swap! selection #(mapv (fn [sel]
                                       (assoc sel :model-id
                                              (:model-id
                                               (nth steps (:dt sel)))))
                                     %))))

;;; ## Components

(defn main-pane [_ _]
  (let [size-invalidates-c (async/chan)]
    (go-loop []
      (swap! viz-options assoc-in [:drawing :max-height-px]
             js/window.innerHeight)
      (when-not (nil? (<! size-invalidates-c))
        (recur)))
    (fn [world-pane into-sim]
      [:div {:on-click #(put! into-viz [:background-clicked])
             :on-key-down #(viz/viz-key-down % into-viz)
             :tabIndex 1}
       [:div.row
        [viz/viz-timeline steps selection viz-options]]
       [:div.row
        [:div.col-sm-3.col-lg-2
         world-pane]
        [:div.col-sm-9.col-lg-10
         [window-resize-listener size-invalidates-c]
         [viz/viz-canvas nil steps selection step-template viz-options
          into-viz into-sim into-journal local-targets]]]])))

(defn comportexviz-app
  [model-tab world-pane into-sim]
  [cui/comportexviz-app model-tab [main-pane world-pane into-sim] viz-options
   selection steps step-template viz/state-colors into-viz into-sim into-journal
   local-targets])

;;; ## Exported helpers

(defn selected-step
  ([]
   (selected-step steps selection))
  ([steps selection]
   (when-let [dt (:dt (first @selection))]
     (nth @steps dt nil))))
