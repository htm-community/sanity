(ns comportexviz.demos.runner
  (:require [comportexviz.main :as main]
            [comportexviz.bridge.remote :as bridge]
            [comportexviz.util :refer [tap-c]]
            [reagent.core :as reagent :refer [atom]]
            [goog.dom :as dom]
            [cljs.core.async :as async :refer [put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn world-pane
  []
  (when-let [step (main/selected-step)]
    (into [:div
           [:div {:style {:font "10px sans-serif"}} "sensed values"]]
          (for [[sense-id v] (:sensed-values step)]
            [:div {:style {:margin-top 30}}
             [:p (name sense-id) [:br]
              [:strong (str v)]]]))))

(defn ^:export init
  []
  (let [into-sim-in (async/chan)
        into-sim-mult (async/mult into-sim-in)
        into-sim-eavesdrop (tap-c into-sim-mult)
        into-journal main/into-journal
        pipe-to-remote-target! (bridge/init
                                (str "ws://" js/location.host "/ws/")
                                main/local-targets)]
    (pipe-to-remote-target! :into-journal into-journal)
    (pipe-to-remote-target! :into-sim (tap-c into-sim-mult))

    (go-loop []
      (when-not (nil? (<! into-sim-eavesdrop))
        ;; Ensure the journal is still connected, resubscribing if needed.
        (put! into-journal [:ping])
        (recur)))

    (reagent/render [main/comportexviz-app nil [world-pane] into-sim-in]
                    (dom/getElement "comportexviz-app"))))
