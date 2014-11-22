(ns comportexviz.demos.isolated-1d
  (:require [org.nfrac.comportex.demos.isolated-1d :as demo]
            [comportexviz.main :as main]
            [goog.ui.TabPane])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(defn ^:export set-n-region-model
  [n]
  (with-ui-loading-message
    (main/set-model (demo/n-region-model n))))

(defn ^:export reset-world
  []
  (main/set-world (demo/world)))

(defn ^:export init
  []
  (goog.ui.TabPane. (.getElementById js/document "comportex-tabs"))
  (reset-world))
