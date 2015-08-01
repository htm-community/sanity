(ns comportexviz.demos.runner
  (:require [comportexviz.main :as main]
            [comportexviz.bridge.remote :as bridge]
            [reagent.core :as reagent :refer [atom]]
            [goog.dom :as dom]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(defn world-pane
  [])

(defn model-tab
  [])

(defn ^:export init
  []
  (reset! main/into-journal (async/chan))

  (let [into-sim (atom (async/chan))]
    (bridge/init (str "ws://" js/location.host "/ws/")
                 @main/into-journal
                 @into-sim
                 main/channel-proxies)

    (reagent/render [main/comportexviz-app model-tab world-pane into-sim]
                    (dom/getElement "comportexviz-app"))))
