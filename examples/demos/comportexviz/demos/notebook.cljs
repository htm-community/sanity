(ns comportexviz.demos.notebook
  (:require [cognitect.transit :as transit]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [comportexviz.bridge.remote :as remote]
            [comportexviz.selection :as sel]
            [comportexviz.viz-canvas :as viz]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.topology :refer [map->OneDTopology
                                                  map->TwoDTopology
                                                  map->ThreeDTopology]]
            [org.nfrac.comportex.util :as util]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(def local-targets (channel-proxy/registry))
(def pipe-to-remote-target!
  (atom nil))
(def target->chan (atom {}))

(defn ^:export connect
  [url]
  (reset! pipe-to-remote-target!
          (remote/init url local-targets)))

(def handlers
  {"org.nfrac.comportex.topology.OneDTopology" map->OneDTopology
   "org.nfrac.comportex.topology.TwoDTopology" map->TwoDTopology
   "org.nfrac.comportex.topology.ThreeDTopology" map->ThreeDTopology})

(defn read-transit-str
  [s]
  (-> (transit/reader :json {:handlers handlers})
      (transit/read s)))

(defn ^:export display-inbits [el serialized]
  (let [[topo state->bits] (read-transit-str serialized)]
    (reagent/render [viz/inbits-display topo state->bits
                     (:drawing viz/default-viz-options)]
                    el)))

(defn ^:export release-inbits [el]
  (reagent/unmount-component-at-node el))

(defn ^:export add-viz [el serialized]
  (let [[journal-target opts] (read-transit-str serialized)
        into-journal (async/chan)
        response-c (async/chan)]
    (swap! target->chan assoc journal-target into-journal)
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
            selection (atom sel/blank-selection)
            base-opts (cond-> viz/default-viz-options
                        (= 1 (count @steps))
                        (assoc-in [:drawing :display-mode]
                                  :two-d))
            viz-options (atom
                         (util/deep-merge-with
                          (fn [& xs]
                            (let [last-non-nil (->> (reverse xs)
                                                    (filter (complement nil?))
                                                    first)]
                              (if (coll? last-non-nil)
                                last-non-nil
                                (last xs))))
                          base-opts
                          opts))]
        (let [[region-key rgn] (-> @step-template :regions seq first)
              layer-id (-> rgn keys first)]
          (swap! selection
                 #(conj (empty %)
                        {:dt 0
                         :region region-key
                         :layer layer-id
                         :model-id (:model-id (first @steps))})))
        (reagent/render [:div
                         (when (> (count @steps) 1)
                           [viz/viz-timeline steps selection viz-options])
                         [viz/viz-canvas {:tabIndex 0} steps
                          selection step-template viz-options nil nil
                          (atom into-journal) local-targets]]
                        el)))))

(defn ^:export release-viz [el serialized]
  (reagent/unmount-component-at-node el)
  (let [journal-target (read-transit-str serialized)]
    (async/close! (get @target->chan journal-target))
    (swap! target->chan dissoc journal-target)))

(defn ^:export exported-viz [el]
  (apply str
         (for [cnv (-> el (.getElementsByTagName "canvas") array-seq)]
           (str "<img src='" (.toDataURL cnv "image/png") "' />"))))
