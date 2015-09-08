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
  [model id]
  {:model-id id
   :timestep (p/timestep model)
   :input-value (:input-value model)})

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
        keep-steps (atom n-keep)
        steps-in (async/chan)
        steps-mult (async/mult steps-in)
        client-infos (atom {})
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
              to-drop (if (not (neg? @keep-steps))
                        (max 0 (- (count added) @keep-steps))
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

            :get-steps
            (let [[response-c] xs]
              (put! response-c
                    [(data/step-template-data @current-model)
                     (->> (map make-step @model-steps (drop @steps-offset (range)))
                          vec)]))

            :subscribe
            (let [[keep-n-steps steps-c response-c] xs]
              (reset! keep-steps keep-n-steps)
              (async/tap steps-mult steps-c)
              (swap! client-info assoc ::steps-subscriber steps-c)
              (println "JOURNAL: Client subscribed to steps.")
              (->> (data/step-template-data @current-model)
                   (put! response-c)))

            :register-viewport
            (let [[viewport response-c] xs]
              (let [token (random-uuid)]
                (swap! client-info update ::viewports assoc token viewport)
                (put! response-c token)))

            :unregister-viewport
            (let [[token] xs]
              (swap! client-info update ::viewports dissoc token))

            :set-keep-steps
            (let [[keep-n-steps] xs]
              (reset! keep-steps keep-n-steps))

            :get-inbits-cols
            (let [[id token response-c] xs
                  [opts path->ids] (get-in @client-info [::viewports token])]
              (put! response-c
                    (if-let [[prev-htm htm] (find-model-pair id)]
                      (data/inbits-cols-data htm prev-htm path->ids opts)
                      (id-missing-response id steps-offset))))

            :get-ff-in-synapses
            (let [[id rgn-id lyr-id only-ids token response-c] xs
                  [opts] (get-in @client-info [::viewports token])]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (data/ff-in-synapses-data htm rgn-id lyr-id only-ids opts)
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
                      (data/cells-segments-data htm prev-htm rgn-id lyr-id col
                                                ci-si opts)
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
                        {:learn-cells (p/learnable-cells layer)
                         :active-cells (p/active-cells layer)
                         :pred-cells (p/predictive-cells layer)})
                      (id-missing-response id steps-offset))))

            :get-transitions-data
            (let [[id region-key layer-id cell-sdr-fracs response-c] xs]
              (put! response-c
                    (if-let [htm (find-model id)]
                      (data/transitions-data htm region-key layer-id cell-sdr-fracs)
                      (id-missing-response id steps-offset)))))
          (recur))
        (println "CLOSING JOURNAL")))))
