(ns comportexviz.server.journal
  (:require #?(:clj [clojure.core.async :as async :refer [put! <! go go-loop]]
               :cljs [cljs.core.async :as async :refer [put! <!]])
            [comportexviz.server.details]
            [comportexviz.server.data :as data]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

#?(:clj
   (defn random-uuid []
     (java.util.UUID/randomUUID)))

(defn make-step
  [htm id]
  (let [input-value (:input-value htm)]
    {:model-id id
     :timestep (p/timestep htm)
     :input-value input-value
     :sensed-values (into {}
                          (for [sense-id (core/sense-keys htm)
                                :let [[selector _] (get-in htm [:sensors
                                                                sense-id])
                                      v (p/extract selector input-value)]]
                            [sense-id v]))}))

(defn id-missing-response
  [id steps-offset]
  (let [offset @steps-offset]
    (when (pos? offset)
      (assert (< id offset))
      (println (str "Can't fetch model " id
                    ". We've dropped all models below id " offset)))
    {}))

(defn init
  [steps-c commands-c current-model n-keep]
  (let [steps-offset (atom 0)
        model-steps (atom [])
        steps-in (async/chan)
        steps-mult (async/mult steps-in)
        client-infos (atom {})
        capture-options (atom {:keep-steps n-keep
                               :ff-synapses {:capture? true
                                             :min-perm 0.0
                                             :only-active? false}
                               :distal-synapses {:capture? true
                                                 :min-perm 0.0
                                                 :only-active? false
                                                 :only-noteworthy-columns? false}
                               :apical-synapses {:capture? true
                                                 :min-perm 0.0
                                                 :only-active? false
                                                 :only-noteworthy-columns? false}})
        find-model (fn [id]
                     (when (number? id)
                       (let [i (- id @steps-offset)]
                         (when-not (neg? i)
                           (nth @model-steps i nil)))))
        find-model-pair (fn [id]
                          (when (number? id)
                            (let [i (- id @steps-offset)]
                              (cond
                                (pos? i) (let [[prev-step step]
                                               (subvec @model-steps (dec i)
                                                       (inc i))]
                                           ;; might be a sampling of steps
                                           (if (= (inc (p/timestep prev-step))
                                                  (p/timestep step))
                                             [prev-step step]
                                             [nil step]))
                                (zero? i) [nil (nth @model-steps i nil)]))))]
    (go-loop []
      (when-let [model (<! steps-c)]
        (let [model-id (+ @steps-offset (count @model-steps))
              added (conj @model-steps model)
              keep-steps (:keep-steps @capture-options)
              to-drop (if (not (neg? keep-steps))
                        (max 0 (- (count added) keep-steps))
                        0)]
          (reset! model-steps (cond-> added
                                (pos? to-drop)
                                (subvec to-drop)))
          (swap! steps-offset + to-drop)
          (put! steps-in (make-step model model-id)))
        (recur)))

    (go-loop []
      (if-let [c (<! commands-c)]
        (let [[[command & xs] client-id] c
              client-info (or (get @client-infos client-id)
                              (let [v (atom {})]
                                (swap! client-infos assoc client-id v)
                                v))]
          (case command
            :ping
            nil

            :client-disconnect
            (do
              (println "JOURNAL: Client disconnected.")
              (async/untap steps-mult (::steps-subscriber @client-info)))

            :connect
            (let [[old-client-info subscriber-c] xs]
              (add-watch client-info ::push-to-client
                         (fn [_ _ _ v]
                           (put! subscriber-c v)))
              (when-let [{viewports ::viewports
                          steps-subscriber ::steps-subscriber} old-client-info]
                (println "JOURNAL: Client reconnected.")
                (when steps-subscriber
                  (println "JOURNAL: Client resubscribed to steps.")
                  (async/tap steps-mult steps-subscriber))
                (swap! client-info
                       #(cond-> %
                          steps-subscriber (assoc ::steps-subscriber
                                                  steps-subscriber)
                          viewports (assoc ::viewports viewports)))))

            :consider-future
            (let [[id input response-c] xs]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (zipmap (core/region-keys htm)
                              (->> (-> htm
                                       (p/htm-sense input nil)
                                       p/htm-activate
                                       core/region-seq)
                                   (map core/column-state-freqs)))
                      (id-missing-response id steps-offset))))

            :decode-predictive-columns
            (let [[id sense-id n response-c] xs]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (core/predictions htm sense-id n)
                      (id-missing-response id steps-offset))))

            :get-steps
            (let [[response-c] xs]
              (put! response-c
                    [(data/step-template-data @current-model)
                     (->> (map make-step @model-steps (drop @steps-offset (range)))
                          vec)]))

            :subscribe
            (let [[steps-c response-c] xs]
              (async/tap steps-mult steps-c)
              (swap! client-info assoc ::steps-subscriber steps-c)
              (println "JOURNAL: Client subscribed to steps.")
              (put! response-c
                    [(data/step-template-data @current-model)
                     @capture-options]))

            :register-viewport
            (let [[viewport response-c] xs]
              (let [token (random-uuid)]
                (swap! client-info update ::viewports assoc token viewport)
                (put! response-c token)))

            :unregister-viewport
            (let [[token] xs]
              (swap! client-info update ::viewports dissoc token))

            :set-capture-options
            (let [[co] xs]
              (reset! capture-options co))

            :get-inbits-cols
            (let [[id token response-c] xs
                  [opts path->ids] (get-in @client-info [::viewports token])]
              (put! response-c
                    (if-let [[prev-htm htm] (find-model-pair id)]
                      (data/inbits-cols-data htm prev-htm path->ids opts)
                      (id-missing-response id steps-offset))))

            :get-ff-in-synapses
            (let [[id rgn-id lyr-id only-ids trace-back? token response-c] xs
                  [opts] (get-in @client-info [::viewports token])]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (data/ff-in-synapses-data htm rgn-id lyr-id only-ids
                                                trace-back? opts)
                      (id-missing-response id steps-offset))))

            :get-ff-out-synapses
            (let [[id sense-id bit token response-c] xs
                  [opts] (get-in @client-info [::viewports token])]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (data/ff-out-synapses-data htm sense-id bit opts)
                      (id-missing-response id steps-offset))))

            :get-cells-segments
            (let [[id rgn-id lyr-id col ci-si token response-c] xs
                  [opts] (get-in @client-info [::viewports token])]
              (put! response-c
                    (if-let [[prev-htm htm] (find-model-pair id)]
                      (let [types [:distal :apical]]
                        (zipmap
                         types
                         (for [type types]
                           (data/cells-segments-data htm prev-htm rgn-id lyr-id
                                                     col ci-si type opts))))
                      (id-missing-response id steps-offset))))

            :get-details-text
            (let [[id rgn-id lyr-id col response-c] xs]
              (put! response-c
                    (if-let [[prev-htm htm] (find-model-pair id)]
                      (comportexviz.server.details/detail-text htm prev-htm
                                                               rgn-id lyr-id
                                                               col)
                      (id-missing-response id steps-offset))))

            :get-model
            (let [[id response-c as-str?] xs]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (cond-> htm
                        as-str? pr-str)
                      (id-missing-response id steps-offset))))

            :get-column-state-freqs
            (let [[id response-c] xs]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (into {}
                            (for [[region-key rgn] (:regions htm)
                                  layer-id (core/layers rgn)]
                              [[region-key layer-id]
                               (-> (get-in htm [:regions region-key])
                                   (core/column-state-freqs layer-id))]))
                      (id-missing-response id steps-offset))))

            :get-cell-excitation-data
            (let [[id region-key layer-id sel-col response-c] xs]
              (put! response-c
                    (let [[prev-htm htm] (find-model-pair id)]
                      (if prev-htm
                        (data/cell-excitation-data htm prev-htm region-key layer-id
                                                   sel-col)
                        (id-missing-response id steps-offset)))))

            :get-cells-by-state
            (let [[id region-key layer-id response-c] xs]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (let [layer (get-in htm [:regions region-key layer-id])]
                        {:winner-cells (p/winner-cells layer)
                         :active-cells (p/active-cells layer)
                         :pred-cells (p/predictive-cells layer)
                         :engaged? (get-in layer [:state :engaged?])})
                      (id-missing-response id steps-offset))))

            :get-transitions-data
            (let [[id region-key layer-id cell-sdr-fracs response-c] xs]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (data/transitions-data htm region-key layer-id cell-sdr-fracs)
                      (id-missing-response id steps-offset)))))
          (recur))
        (println "CLOSING JOURNAL")))))
