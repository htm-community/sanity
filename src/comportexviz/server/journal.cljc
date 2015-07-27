(ns comportexviz.server.journal
  (:require #?(:clj [clojure.core.async :as async :refer [put! <! go go-loop]]
               :cljs [cljs.core.async :as async :refer [put! <!]])
            [comportexviz.details]
            [comportexviz.server.data :as data]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

(defn make-step
  [model id]
  {:model-id id
   :timestep (p/timestep model)
   :input-values (->> model core/input-seq (map :value))})

(defn id-missing-response
  [id steps-offset]
  (let [offset @steps-offset]
    (assert (< id offset))
    (println (str "Can't fetch model " id
                  ". We've dropped all models below id " offset))
    {}))

(defn init
  [steps-c commands-c current-model]
  (let [steps-offset (atom 0)
        model-steps (atom [])
        keep-steps (atom 0)
        subscriber-c (atom nil)
        find-model (fn [id]
                     (when (number? id)
                       (let [i (- id @steps-offset)]
                         (when-not (neg? i)
                           (nth @model-steps i nil)))))
        find-model-pair (fn [id]
                          (when (number? id)
                            (let [i (- id @steps-offset)]
                              (cond
                                (pos? i) (subvec @model-steps (dec i) (inc i))
                                (zero? i) [nil (nth @model-steps i nil)]))))]
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
      (when-let [c (<! commands-c)]
        (let [[command & xs] c]
          (case command
            :subscribe
            (let [[keep-n-steps steps-c response-c] xs]
              (reset! keep-steps keep-n-steps)
              (reset! subscriber-c steps-c)
              (->> (data/step-template-data @current-model)
                   (put! response-c)))

            :set-keep-steps
            (let [[keep-n-steps] xs]
              (reset! keep-steps keep-n-steps))

            :get-inbits-cols
            (let [[id opts response-c] xs]
              (put! response-c
                    (if-let [[prev-htm htm] (find-model-pair id)]
                      (data/inbits-cols-data htm prev-htm opts)
                      (id-missing-response id steps-offset))))

            :get-inbits-cols-subset
            (let [[id opts path->ids response-c] xs]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (data/inbits-cols-data-subset htm path->ids opts)
                      (id-missing-response id steps-offset))))

            :get-ff-synapses
            (let [[sel opts response-c] xs
                  id (:model-id sel)
                  to (get-in opts [:ff-synapses :to])]
              (put! response-c
                    (or (when (or (= to :all)
                                  (and (= to :selected)
                                       (:col sel)))
                          (if-let [htm (find-model id)]
                            (data/ff-synapses-data (find-model id) sel opts)
                            (id-missing-response id steps-offset)))
                        {})))

            :get-cell-segments
            (let [[sel opts response-c] xs
                  id (:model-id sel)]
              (put! response-c
                    (if (:col sel)
                      (if-let [[prev-htm htm] (find-model-pair id)]
                        (data/cell-segments-data htm prev-htm sel opts)
                        (id-missing-response id steps-offset))
                      {})))

            :get-details-text
            (let [[sel response-c] xs
                  id (:model-id sel)]
              (put! response-c
                    (if-let [[prev-htm htm] (find-model-pair id)]
                      (comportexviz.details/detail-text htm prev-htm sel)
                      (id-missing-response id steps-offset))))

            :get-model
            (let [[id response-c as-str?] xs]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (cond-> htm
                        as-str? pr-str)
                      (id-missing-response id steps-offset))))

            :get-column-state-freqs
            (let [[id region-key layer-id response-c] xs]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (-> (get-in htm [:regions region-key])
                          (core/column-state-freqs layer-id))
                      (id-missing-response id steps-offset))))

            :get-cell-excitation-data
            (let [[id region-key layer-id sel-col response-c] xs]
              (put! response-c
                    (if-let [[prev-htm htm] (find-model-pair id)]
                      (data/cell-excitation-data htm prev-htm region-key layer-id
                                                 sel-col)
                      (id-missing-response id steps-offset)))))
          (recur))))))
