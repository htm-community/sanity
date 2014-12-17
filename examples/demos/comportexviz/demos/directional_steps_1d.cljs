(ns comportexviz.demos.directional-steps-1d
  (:require [org.nfrac.comportex.demos.directional-steps-1d :as demo]
            [comportexviz.main :as main]
            [goog.ui.TabPane]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(defn ^:export set-n-region-model
  [n]
  (with-ui-loading-message
    (main/set-model (demo/n-region-model n))))

(def world-c (async/chan))

(defn ^:export reset-world
  []
  (main/set-world world-c)
  (async/onto-chan world-c (demo/world-seq) false))

(defn ^:export init
  []
  (goog.ui.TabPane. (.getElementById js/document "comportex-tabs"))
  (reset-world))
