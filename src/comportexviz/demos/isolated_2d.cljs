(ns comportexviz.demos.isolated-2d
  (:require [org.nfrac.comportex.demos.isolated-2d :as demo]
            [comportexviz.main :as main])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(defn ^:export set-n-region-model
  [n]
  (with-ui-loading-message
    (main/set-model
     (demo/n-region-model n))))
