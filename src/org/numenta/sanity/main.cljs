(ns org.numenta.sanity.main
  (:require [org.numenta.sanity.controls-ui :as cui]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.helpers :as helpers :refer [window-resize-listener]]
            [org.numenta.sanity.selection :as sel]
            [org.numenta.sanity.viz-canvas :as viz]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

;;; ## Journal data

(def into-journal (async/chan 65536))

;;; ## Viz data

(def steps (atom []))
(def step-template (atom nil))
(def selection (atom sel/blank-selection))
(def capture-options (atom nil))
(def viz-options (atom viz/default-viz-options))
(def into-viz (chan))

;;; ## Connect journal to viz

(defn subscribe-to-steps! []
  (let [steps-c (async/chan)
        response-c (async/chan)]
    (put! into-journal ["subscribe" (marshal/channel steps-c)
                        (marshal/channel response-c true)])
    (go
      ;; Get the template before getting any steps.
      (let [[st co] (<! response-c)]
        (reset! step-template st)
        (reset! capture-options co))
      (let [[region-key rgn] (-> @step-template :regions seq first)
            layer-id (-> rgn keys first)]
        (reset! selection
                [{:dt 0
                  :path [:regions region-key layer-id]}]))
      (add-watch capture-options ::push-to-server
                 (fn [_ _ _ opts]
                   (put! into-journal ["set-capture-options" opts])))

      (loop []
        (when-let [step (<! steps-c)]
          (let [keep-steps (:keep-steps @capture-options)
                [kept dropped] (split-at keep-steps
                                         (cons step @steps))]
            (reset! steps kept))
          (recur))))
    steps-c))

;; not sure why this would be used, but for completeness...
(def subscription-data (subscribe-to-steps!))

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
      [:div.container-fluid {:on-click #(put! into-viz [:background-clicked])
                             :on-key-down #(viz/viz-key-down % into-viz)
                             :tabIndex 1}
       [:div.row
        [viz/viz-timeline steps selection capture-options]]
       [:div.row
        [:div.col-sm-3.col-lg-2
         world-pane]
        [:div.col-sm-9.col-lg-10 {:style {:overflow "auto"}}
         [window-resize-listener size-invalidates-c]
         [viz/viz-canvas nil steps selection step-template viz-options
          into-viz into-sim into-journal]]]])))

(defn sanity-app
  [title model-tab world-pane features into-sim]
  [cui/sanity-app title model-tab [main-pane world-pane into-sim]
   features capture-options viz-options selection steps step-template
   viz/state-colors into-viz into-sim into-journal])

;;; ## Exported helpers

(defn selected-step
  ([]
   (selected-step steps selection))
  ([steps selection]
   (when-let [dt (:dt (first @selection))]
     (nth @steps dt nil))))
