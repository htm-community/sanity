(ns org.numenta.sanity.demos.notebook
  (:require [cognitect.transit :as transit]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.bridge.remote :as remote]
            [org.numenta.sanity.demos.runner :as runner]
            [org.numenta.sanity.selection :as sel]
            [org.numenta.sanity.util :refer [translate-network-shape]]
            [org.numenta.sanity.viz-canvas :as viz]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util]
            [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer [chan put! <!]]
            [clojure.walk :refer [keywordize-keys]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(def pipe-to-remote-target!
  (atom nil))

;; So that we can close the channel via its target-id.
(def remote-target->chan (atom {}))

(defn ^:export connect
  [url]
  (reset! pipe-to-remote-target! (remote/init url)))

(defn read-transit-str
  [s]
  (-> (transit/reader :json)
      (transit/read s)))

(defn ^:export display-inbits [el serialized]
  (let [[dims state->bits d-opts] (read-transit-str serialized)]
    (reagent/render [viz/inbits-display dims state->bits
                     (merge (:drawing viz/default-viz-options)
                            d-opts)]
                    el)))

(defn ^:export release-inbits [el]
  (reagent/unmount-component-at-node el))

(defn ^:export add-viz [el serialized]
  (let [[journal-target opts] (read-transit-str serialized)
        into-journal (async/chan)
        into-viz (async/chan)
        response-c (async/chan)]
    (swap! remote-target->chan assoc journal-target into-journal)
    (@pipe-to-remote-target! journal-target into-journal)
    (put! into-journal ["get-network-shape" (marshal/channel response-c true)])
    (go
      (let [network-shape (atom
                           (translate-network-shape (<! response-c)))
            response-c (async/chan)]
        (put! into-journal ["get-steps" (marshal/channel response-c true)])
        (let [all-steps (<! response-c)
              steps (->> all-steps
                         keywordize-keys
                         (map (fn [step]
                                (assoc step :network-shape @network-shape)))
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
                            opts))
              [region-key rgn] (-> @network-shape :regions seq first)
              layer-id (-> rgn keys first)]
          (let [[region-key rgn] (-> @network-shape :regions seq first)
                layer-id (-> rgn keys first)]
            (swap! selection
                   #(conj (empty %)
                          {:dt 0
                           :region region-key
                           :layer layer-id
                           :snapshot-id (:snapshot-id (first @steps))})))
          (reagent/render [:div
                           {:on-click #(put! into-viz [:background-clicked])
                            :on-key-down #(viz/viz-key-down % into-viz)
                            :tabIndex 1}
                           (when (> (count @steps) 1)
                             [viz/viz-timeline steps selection viz-options])
                           [:table
                            [:tr
                             [:td {:style {:border "none"
                                           :vertical-align "top"}}
                              [runner/world-pane steps selection]]
                             [:td {:style {:border "none"
                                           :vertical-align "top"}}
                              [viz/viz-canvas {:tabIndex 0} steps
                               selection network-shape viz-options into-viz nil
                               into-journal]]]]]
                          el))))))

(defn ^:export release-viz [el serialized]
  (reagent/unmount-component-at-node el)
  (let [journal-target (read-transit-str serialized)]
    (async/close! (get @remote-target->chan journal-target))
    (swap! remote-target->chan dissoc journal-target)))

(defn ^:export exported-viz [el]
  (let [cnvs (array-seq (.getElementsByTagName el "canvas"))
        copy-el (js/document.createElement "div")]
    (set! (.-innerHTML copy-el) (.-innerHTML el))
    (doseq [cnv cnvs
            :let [;; Each mutation invalidates the list.
                  victim-el (aget (.getElementsByTagName copy-el "canvas") 0)
                  img-el (js/document.createElement "img")]]
      (.setAttribute img-el "src" (.toDataURL cnv "image/png"))
      (when-let [style (.getAttribute victim-el "style")]
        (.setAttribute img-el "style" style))
      (.replaceChild (.-parentNode victim-el) img-el victim-el))
    (.-innerHTML copy-el)))
