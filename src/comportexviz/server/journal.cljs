(ns comportexviz.server.journal
  (:require [cljs.core.async :as async :refer [chan put! <!]]
            [comportexviz.details]
            [comportexviz.helpers :as helpers]
            [comportexviz.server.data :as data]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn make-step
  [model id]
  {:model-id id
   :timestep (p/timestep model)
   :input-values (->> model core/input-seq (map :value))})

(defn init
  [steps-c commands-c current-model]
  (let [steps-offset (atom 0)
        model-steps (atom [])
        keep-steps (atom 0)
        subscriber-c (atom nil)
        find-model (fn [id]
                     (nth @model-steps (- id @steps-offset) nil))
        find-model-pair (fn [id]
                          (let [i (- id @steps-offset)]
                            (if (zero? i)
                              [nil (nth @model-steps i nil)]
                              (subvec @model-steps (dec i) (inc i)))))]
    (go-loop []
      (when-let [model (<! steps-c)]
        (let [model-id (+ @steps-offset (count @model-steps))
              added (conj @model-steps model)
              to-drop (max 0 (- (count added) @keep-steps))]
          (reset! model-steps (subvec added to-drop))
          (swap! steps-offset + to-drop)
          (when @subscriber-c
            (put! @subscriber-c (make-step model model-id))))
        (recur)))

    (go-loop []
      (when-let [[command & xs :as v] (<! commands-c)]
        (do
          (case command
            :subscribe
            (let [[keep-n-steps steps-c response-c] xs]
              (reset! keep-steps keep-n-steps)
              (reset! subscriber-c steps-c)
              (put! response-c (data/step-template-data @current-model)))

            :set-keep-steps
            (let [[keep-n-steps] xs]
              (reset! keep-steps keep-n-steps))

            :get-inbits-cols
            (let [[id opts out-c] xs
                  [prev-htm htm] (find-model-pair id)]
              (put! out-c (data/inbits-cols-data htm prev-htm opts)))

            :get-inbits-cols-subset
            (let [[id opts path->ids out-c] xs]
              (put! out-c (-> (find-model id)
                              (data/inbits-cols-data-subset path->ids opts))))

            :get-ff-synapses
            (let [[sel opts out-c] xs
                  id (:model-id sel)
                  to (get-in opts [:ff-synapses :to])]
              (put! out-c (or (when (or (= to :all)
                                        (and (= to :selected)
                                             (:col sel)))
                                (data/ff-synapses-data (find-model id) sel
                                                       opts))
                              {})))

            :get-cell-segments
            (let [[sel opts out-c] xs
                  id (:model-id sel)
                  [prev-htm htm] (find-model-pair id)]
              (put! out-c (or (when (:col sel)
                                (data/cell-segments-data htm prev-htm sel opts))
                              {})))

            :get-details-text
            (let [[sel out-c] xs
                  id (:model-id sel)
                  [prev-htm htm] (find-model-pair id)]
              (put! out-c (comportexviz.details/detail-text htm prev-htm sel)))

            :get-model
            (let [[model-id out-c] xs]
              (put! out-c (find-model model-id)))

            :get-column-state-freqs
            (let [[id region-key layer-id
                   response-c] xs]
              (put! response-c (-> (find-model id)
                                   (get-in [:regions region-key])
                                   (core/column-state-freqs
                                    layer-id))))

            :get-cell-excitation-data
            (let [[id region-key layer-id sel-col
                   response-c] xs
                   [prev-htm htm] (find-model-pair id)]
              (put! response-c (data/cell-excitation-data
                                htm prev-htm region-key layer-id sel-col))))
          (recur))))))
