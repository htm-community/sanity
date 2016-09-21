(ns org.numenta.sanity.comportex.journal
  (:require [clojure.core.async :as async :refer [put! <! #?(:clj go-loop)]]
            [clojure.set :as set]
            [clojure.walk :refer [keywordize-keys stringify-keys]]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.comportex.details]
            [org.numenta.sanity.comportex.data :as data]
            [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.util :as util])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go-loop]])))

#?(:clj
   (defn random-uuid []
     (java.util.UUID/randomUUID)))

(defn make-step
  [htm id]
  (let [input-value (:input-value htm)]
    {"snapshot-id" id
     "timestep" (cx/timestep htm)
     "input-value" input-value
     "sensed-values" (into {}
                           (for [sense-id (cx/sense-keys htm)
                                 :let [[selector _] (get-in htm [:sensors
                                                                 sense-id])
                                       v (cx/extract selector input-value)]]
                             [sense-id v]))}))

(defn id-missing-response
  [id steps-offset]
  (let [offset @steps-offset]
    (when (pos? offset)
      (assert (< id offset))
      (println (str "Can't fetch model " id
                    ". We've dropped all models below id " offset)))
    {}))

(defn command-handler
  [current-model steps-offset model-steps steps-mult client-infos
   capture-options]
  (letfn [(find-model [id]
            (when (number? id)
              (let [i (- id @steps-offset)]
                (when-not (neg? i)
                  (nth @model-steps i nil)))))
          (find-model-pair [id]
            (when (number? id)
              (let [i (- id @steps-offset)]
                (cond
                  (pos? i) (let [[prev-step step]
                                 (subvec @model-steps (dec i)
                                         (inc i))]
                             ;; might be a sampling of steps
                             (if (= (inc (cx/timestep prev-step))
                                    (cx/timestep step))
                               [prev-step step]
                               [nil step]))
                  (zero? i) [nil (nth @model-steps i nil)]))))]
    (fn handle-command [[[command & xs] client-id]]
      (let [client-info (or (get @client-infos client-id)
                            (let [v (atom {})]
                              (swap! client-infos assoc client-id v)
                              v))]
        (case command
          "ping"
          nil

          "client-disconnect"
          (do
            (println "JOURNAL: Client disconnected.")
            (async/untap steps-mult (:ch (:steps-mchannel @client-info))))

          "connect"
          (let [[old-client-info {subscriber-c :ch}] xs]
            (add-watch client-info ::push-to-client
                       (fn [_ _ _ v]
                         (put! subscriber-c
                               (update v :steps-mchannel
                                       (fn [steps-mchannel]
                                         (marshal/channel-weak
                                          (get-in steps-mchannel
                                                  [:ch :target-id])))))))
            (when-let [{steps-mchannel :steps-mchannel} old-client-info]
              (println "JOURNAL: Client reconnected.")
              (when steps-mchannel
                (println "JOURNAL: Client resubscribed to steps.")
                (async/tap steps-mult (:ch steps-mchannel)))
              (swap! client-info
                     #(cond-> %
                        steps-mchannel (assoc :steps-mchannel steps-mchannel)))))

          "consider-future"
          (let [[id input {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (zipmap (cx/layer-keys htm)
                            (->> (-> htm
                                     (cx/htm-sense input nil)
                                     cx/htm-activate
                                     cx/layer-seq)
                                 (map layer/column-state-freqs)))
                    (id-missing-response id steps-offset))))

          "decode-predictive-columns"
          (let [[id sense-id n {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (cx/predictions htm sense-id n)
                    (id-missing-response id steps-offset))))

          "get-steps"
          (let [[{response-c :ch}] xs]
            (put! response-c
                  (->> (map make-step @model-steps (drop @steps-offset
                                                         (range)))
                       vec)))

          "subscribe"
          (let [[steps-mchannel] xs]
            (async/tap steps-mult (:ch steps-mchannel))
            (swap! client-info assoc :steps-mchannel steps-mchannel)
            (println "JOURNAL: Client subscribed to steps."))

          "get-network-shape"
          (let [[{response-c :ch}] xs]
            (put! response-c (data/network-shape @current-model)))

          "get-capture-options"
          (let [[{response-c :ch}] xs]
            (put! response-c (stringify-keys @capture-options)))

          "set-capture-options"
          (let [[co] xs]
            (reset! capture-options (keywordize-keys co)))

          "get-layer-bits"
          (let [[id lyr-id fetches {cols-subset :value}
                 {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (let [lyr (get-in htm [:layers lyr-id])]
                      (cond-> {}
                        (contains? fetches "active-columns")
                        (assoc "active-columns" (:active-columns (cx/layer-state lyr)))

                        (contains? fetches "pred-columns")
                        (assoc "pred-columns" (->> (cx/layer-state lyr)
                                                   (:prior-predictive-cells)
                                                   (map first)
                                                   (distinct)))

                        (contains? fetches "overlaps-columns-alpha")
                        (assoc "overlaps-columns-alpha"
                               (->> (:col-overlaps (:active-state lyr))
                                    (reduce-kv (fn [m [col _ _] v]
                                                 (assoc!
                                                  m col
                                                  (max v (get m col
                                                              0))))
                                               (transient {}))
                                    (persistent!)
                                    (util/remap #(min 1.0
                                                      (float (/ % 16))))))

                        (contains? fetches "boost-columns-alpha")
                        (assoc "boost-columns-alpha"
                               (let [{:keys [max-boost]} (cx/params lyr)]
                                (->> (:boosts lyr)
                                     (map
                                      #(/ (dec %)
                                          (dec max-boost)))
                                     (map float)
                                     (zipmap (range)))))

                        (contains? fetches "active-freq-columns-alpha")
                        (assoc "active-freq-columns-alpha"
                               (->> (:active-duty-cycles lyr)
                                    (map #(min 1.0 (* 2 %)))
                                    (zipmap (range))))

                        (contains? fetches "n-segments-columns-alpha")
                        (assoc "n-segments-columns-alpha"
                               (->> cols-subset
                                    (map #(data/count-segs-in-column
                                           (:distal-sg lyr)
                                           (:depth (cx/params lyr)) %))
                                    (map #(min 1.0
                                               (float (/ % 16.0))))
                                    (zipmap cols-subset)))

                        true
                        (assoc "break?"
                               (-> lyr
                                   (get-in [:prior-distal-state
                                            :active-bits])
                                   empty?))))
                    (id-missing-response id steps-offset))))

          "get-sense-bits"
          (let [[id sense-id fetches {bits-subset :value}
                 {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (let [sense (get-in htm [:senses sense-id])
                          ;; layer this sense feeds to, for predictions
                          {:keys [fb-deps]} (cx/add-feedback-deps htm)
                          lyr-id (first (get fb-deps sense-id))
                          prev-lyr (get-in prev-htm [:layers lyr-id])]
                      (cond-> {}
                        (contains? fetches "active-bits")
                        (assoc "active-bits" (set (:bits sense)))

                        (and (contains? fetches "pred-bits-alpha")
                             prev-lyr)
                        (assoc "pred-bits-alpha"
                               (let [start (cx/ff-base htm lyr-id sense-id)
                                     end (+ start (cx/size-of sense))]
                                 (->> (cx/layer-decode-to-ff-bits prev-lyr {})
                                      (keep (fn [[id votes]]
                                              (when (and (<= start id)
                                                         (< id end))
                                                [(- id start) votes])))
                                      (into {})
                                      (util/remap
                                       #(min 1.0 (float (/ % 8)))))))))
                    (id-missing-response id steps-offset))))

          "get-proximal-synapses-by-source-bit"
          (let [[id sense-id bit syn-states {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (data/syns-from-source-bit htm sense-id bit syn-states)
                    (id-missing-response id steps-offset))))

          "get-column-cells"
          (let [[id lyr-id col fetches {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (let [lyr (get-in htm [:layers lyr-id])
                          info (cx/layer-state lyr)
                          extract-cells #(->> %
                                              (keep (fn [[column ci]]
                                                      (when (= col column)
                                                        ci)))
                                              (into #{}))]
                      (cond-> {}
                        (contains? fetches "active-cells")
                        (assoc "active-cells"
                               (extract-cells (:active-cells info)))

                        (contains? fetches "prior-predicted-cells")
                        (assoc "prior-predicted-cells"
                               (extract-cells (:prior-predictive-cells info)))

                        (contains? fetches "winner-cells")
                        (assoc "winner-cells"
                               (extract-cells (:winner-cells info)))))
                    (id-missing-response id steps-offset))))

          "get-apical-segments"
          (let [[id lyr-id seg-selector {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/query-segs htm prev-htm lyr-id seg-selector :apical)
                    (id-missing-response id steps-offset))))

          "get-distal-segments"
          (let [[id lyr-id seg-selector {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/query-segs htm prev-htm lyr-id seg-selector :distal)
                    (id-missing-response id steps-offset))))

          "get-proximal-segments"
          (let [[id lyr-id seg-selector {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/query-segs htm prev-htm lyr-id seg-selector :proximal)
                    (id-missing-response id steps-offset))))

          "get-apical-synapses"
          (let [[id lyr-id seg-selector syn-states {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/query-syns htm prev-htm lyr-id seg-selector
                                     syn-states :apical)
                    (id-missing-response id steps-offset))))

          "get-distal-synapses"
          (let [[id lyr-id seg-selector syn-states {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/query-syns htm prev-htm lyr-id seg-selector
                                     syn-states :distal)
                    (id-missing-response id steps-offset))))

          "get-proximal-synapses"
          (let [[id lyr-id seg-selector syn-states {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/query-syns htm prev-htm lyr-id seg-selector
                                     syn-states :proximal)
                    (id-missing-response id steps-offset))))

          "get-details-text"
          (let [[id lyr-id col {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (org.numenta.sanity.comportex.details/detail-text
                     htm prev-htm lyr-id col)
                    (id-missing-response id steps-offset))))

          "get-model"
          (let [[id {response-c :ch} as-str?] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (cond-> htm
                      as-str? pr-str)
                    (id-missing-response id steps-offset))))

          "get-layer-stats"
          (let [[id lyr-id fetches {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (let [lyr (get-in htm [:layers lyr-id])
                          info (cx/layer-state lyr)
                          a-cols (:active-columns info)
                          ppc (:prior-predictive-cells info)
                          pp-cols (into #{} (map first ppc))
                          ap-cols (set/intersection pp-cols a-cols)
                          col-states (merge
                                      (zipmap pp-cols (repeat :predicted))
                                      (zipmap a-cols (repeat :active))
                                      (zipmap ap-cols
                                              (repeat :active-predicted)))
                          freqs (frequencies (vals col-states))]
                      (cond-> {}
                        (contains? fetches "n-unpredicted-active-columns")
                        (assoc "n-unpredicted-active-columns"
                               (get freqs :active 0))

                        (contains? fetches "n-predicted-inactive-columns")
                        (assoc "n-predicted-inactive-columns"
                               (get freqs :predicted 0))

                        (contains? fetches "n-predicted-active-columns")
                        (assoc "n-predicted-active-columns"
                               (get freqs :active-predicted 0))))
                    (id-missing-response id steps-offset))))

          "get-cell-excitation-data"
          (let [[id lyr-id sel-col {response-c :ch}] xs]
            (put! response-c
                  (let [[prev-htm htm] (find-model-pair id)]
                    (if prev-htm
                      (data/cell-excitation-data htm prev-htm lyr-id sel-col)
                      (id-missing-response id steps-offset)))))

          "get-cells-by-state"
          (let [[id lyr-id {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (let [layer (get-in htm [:layers lyr-id])
                          info (cx/layer-state layer)]
                      {:winner-cells (:winner-cells info)
                       :active-cells (:active-cells info)
                       :pred-cells (:predictive-cells info)
                       :engaged? true})
                    (id-missing-response id steps-offset))))

          "get-transitions-data"
          (let [[id lyr-id cell-sdr-fracs {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (data/transitions-data htm lyr-id cell-sdr-fracs)
                    (id-missing-response id steps-offset)))))))))

(defn init
  [steps-c commands-c current-model n-keep]
  (let [steps-offset (atom 0)
        model-steps (atom [])
        steps-in (async/chan)
        steps-mult (async/mult steps-in)
        client-infos (atom {})
        capture-options (atom {:keep-steps n-keep
                               :ff-synapses {:capture? true
                                             :only-active? false
                                             :only-connected? false}
                               :distal-synapses {:capture? true
                                                 :only-active? false
                                                 :only-connected? false
                                                 :only-noteworthy-columns? false}
                               :apical-synapses {:capture? true
                                                 :only-active? false
                                                 :only-connected? false
                                                 :only-noteworthy-columns? false}})
        handle-command (command-handler current-model steps-offset model-steps
                                        steps-mult client-infos
                                        capture-options)]
    (go-loop []
      (when-let [model (<! steps-c)]
        (let [snapshot-id (+ @steps-offset (count @model-steps))
              added (conj @model-steps model)
              keep-steps (:keep-steps @capture-options)
              to-drop (if (not (neg? keep-steps))
                        (max 0 (- (count added) keep-steps))
                        0)]
          (reset! model-steps (cond-> added
                                (pos? to-drop)
                                (subvec to-drop)))
          (swap! steps-offset + to-drop)
          (put! steps-in (make-step model snapshot-id)))
        (recur)))
    (go-loop []
      (let [c (<! commands-c)]
        (if-not (nil? c)
          (do
            (handle-command c)
            (recur))
          (println "CLOSING JOURNAL"))))))
