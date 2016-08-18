(ns org.numenta.sanity.demos.runner
  (:require [org.numenta.sanity.main :as main]
            [org.numenta.sanity.bridge.remote :as remote]
            [org.numenta.sanity.util :refer [tap-c]]
            [reagent.core :as reagent :refer [atom]]
            [goog.dom :as dom]
            [cljs.core.async :as async :refer [put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn key-value-display
  [k v]
  [:div {:style {:margin-top 20}}
   [:p
    [:span {:style {:font-family "sans-serif"
                    :font-size "9px"
                    :font-weight "bold"}} k]
    [:br]
    [:strong v]]])

(defn world-pane
  [steps selection]
  (when (not-empty @steps)
    (let [step (main/selected-step steps selection)
          kvs (if-let [display-value (:display-value step)]
                (seq display-value)
                (when (:input-value step)
                  (for [[sense-id v] (:sensed-values step)]
                    [(name sense-id) (str v)])))]
      (into [:div]
            (for [[k v] kvs]
              [key-value-display k v])))))

(defn ^:export init
  [title ws-url selected-tab & feature-list]
  (let [into-sim-in (async/chan)
        into-sim-mult (async/mult into-sim-in)
        into-sim-eavesdrop (tap-c into-sim-mult)
        into-journal main/into-journal
        pipe-to-remote-target! (remote/init ws-url)
        features (into #{} (map keyword) feature-list)
        current-tab (atom (keyword selected-tab))]
    (pipe-to-remote-target! "journal" into-journal)
    (pipe-to-remote-target! "simulation" (tap-c into-sim-mult))

    (go-loop []
      (when-not (nil? (<! into-sim-eavesdrop))
        ;; Ensure the journal is still connected, resubscribing if needed.
        (put! into-journal ["ping"])
        (recur)))

    (reagent/render [main/sanity-app title nil
                     [world-pane main/steps main/selection] current-tab features
                     into-sim-in]
                    (dom/getElement "sanity-app"))))
