(ns comportexviz.demos.notebook
  (:require [cljs.core.async :as async :refer [chan put! <!]]
            [cognitect.transit :as transit]
            [comportexviz.viz-canvas :as viz]
            [org.nfrac.comportex.protocols :as p]
            [comportexviz.bridge.remote :as remote]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [reagent.core :as reagent :refer [atom]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(def local-targets (channel-proxy/registry))
(def pipe-to-remote-target!
  (atom nil))

(defn ^:export connect
  [url]
  (reset! pipe-to-remote-target!
          (remote/init url local-targets)))

(defn read-transit-str
  [s]
  (-> (transit/reader :json)
      (transit/read s)))

(defn ^:export add-viz [el serialized]
  (let [journal-target (read-transit-str serialized)
        into-journal (async/chan)
        response-c (async/chan)]
    (@pipe-to-remote-target! journal-target into-journal)
    (put! into-journal [:get-steps (channel-proxy/register! local-targets
                                                            response-c)])
    (go
      (let [[step-template all-steps :as r] (<! response-c)
            step-template (atom step-template)
            steps (->> all-steps
                       reverse
                       vec
                       atom)
            selection (atom viz/blank-selection)
            viz-options (atom
                         (-> viz/default-viz-options
                             (assoc-in [:ff-synapses :to] :all)
                             (assoc-in [:distal-synapses :to] :all)
                             (cond->
                                 (= 1 (count @steps))
                               (assoc-in [:drawing :display-mode]
                                         :two-d))))]
        (let [[region-key rgn] (-> @step-template :regions seq first)
              layer-id (-> rgn keys first)]
          (swap! selection assoc
                 :dt 0
                 :region region-key
                 :layer layer-id
                 :model-id (:model-id (first @steps))))
        (reagent/render [viz/viz-canvas {:style {:width "100%"
                                                 :height "100vh"}
                                         :tabIndex 0} steps
                         selection step-template viz-options nil nil
                         (atom into-journal) local-targets]
                        el)))))

(defn ^:export exported-viz [el]
  (let [cnv (-> el (.getElementsByTagName "canvas") (aget 0))]
    (assert (= "CANVAS" (.-nodeName cnv)))
    (str "<img src='" (.toDataURL cnv "image/png") "' />")))
