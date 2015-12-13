(ns org.numenta.sanity.comportex.journal
  (:require #?(:clj [clojure.core.async :as async :refer [put! <! go go-loop]]
               :cljs [cljs.core.async :as async :refer [put! <!]])
            [clojure.walk :refer [keywordize-keys stringify-keys]]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.comportex.details]
            [org.numenta.sanity.comportex.data :as data]
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
    {"model-id" id
     "timestep" (p/timestep htm)
     "input-value" input-value
     "sensed-values" (into {}
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
                             (if (= (inc (p/timestep prev-step))
                                    (p/timestep step))
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
                    (zipmap (core/region-keys htm)
                            (->> (-> htm
                                     (p/htm-sense input nil)
                                     p/htm-activate
                                     core/region-seq)
                                 (map core/column-state-freqs)))
                    (id-missing-response id steps-offset))))

          "decode-predictive-columns"
          (let [[id sense-id n {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (core/predictions htm sense-id n)
                    (id-missing-response id steps-offset))))

          "get-steps"
          (let [[{response-c :ch}] xs]
            (put! response-c
                  [(data/step-template-data @current-model)
                   (->> (map make-step @model-steps (drop @steps-offset
                                                          (range)))
                        vec)]))

          "subscribe"
          (let [[steps-mchannel {response-c :ch}] xs]
            (async/tap steps-mult (:ch steps-mchannel))
            (swap! client-info assoc :steps-mchannel steps-mchannel)
            (println "JOURNAL: Client subscribed to steps.")
            (put! response-c
                  [(data/step-template-data @current-model)
                   (stringify-keys @capture-options)]))

          "set-capture-options"
          (let [[co] xs]
            (reset! capture-options (keywordize-keys co)))

          "get-layer-bits"
          (let [[id rgn-id lyr-id fetches {cols-subset :value}
                 {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (let [lyr (get-in htm [:regions rgn-id lyr-id])]
                      (cond-> {}
                        (contains? fetches "active-columns")
                        (assoc "active-columns" (p/active-columns lyr))

                        (contains? fetches "pred-columns")
                        (assoc "pred-columns" (->> (p/prior-predictive-cells lyr)
                                                   (map first)
                                                   (distinct)))

                        (contains? fetches "overlaps-columns-alpha")
                        (assoc "overlaps-columns-alpha"
                               (->> (:col-overlaps (:state lyr))
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
                               (let [{:keys [max-boost]} (p/params lyr)]
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
                                           (p/layer-depth lyr) %))
                                    (map #(min 1.0
                                               (float (/ % 16.0))))
                                    (zipmap cols-subset)))

                        (contains? fetches "tp-columns")
                        (assoc "tp-columns"
                               (->> (p/temporal-pooling-cells lyr)
                                    (map first)))

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
                          ;; region this sense feeds to, for predictions
                          ff-rgn-id (first (get-in htm [:fb-deps sense-id]))
                          prev-ff-rgn (when (pos? (p/size (p/ff-topology
                                                           sense)))
                                        (get-in prev-htm [:regions ff-rgn-id]))]
                      (cond-> {}
                        (contains? fetches "active-bits")
                        (assoc "active-bits" (set (data/active-bits sense)))

                        (and (contains? fetches "pred-bits-alpha")
                             prev-ff-rgn)
                        (assoc "pred-bits-alpha"
                               (let [start (core/ff-base htm ff-rgn-id
                                                         sense-id)
                                     end (+ start
                                            (-> sense p/ff-topology p/size))]
                                 (->> (core/predicted-bit-votes prev-ff-rgn)
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
          (let [[id rgn-id lyr-id col {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (let [lyr (get-in htm [:regions rgn-id lyr-id])
                          extract-cells #(->> %
                                              (keep (fn [[column ci]]
                                                      (when (= col column)
                                                        ci)))
                                              (into #{}))]
                      {"cells-per-column" (p/layer-depth lyr)
                       "active-cells" (extract-cells
                                       (p/active-cells lyr))
                       "prior-predicted-cells" (extract-cells
                                                (p/prior-predictive-cells lyr))
                       "winner-cells" (extract-cells
                                       (p/winner-cells lyr))})
                    (id-missing-response id steps-offset))))

          "get-column-apical-segments"
          (let [[id rgn-id lyr-id col {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/column-segs htm prev-htm rgn-id lyr-id col :apical)
                    (id-missing-response id steps-offset))))

          "get-column-distal-segments"
          (let [[id rgn-id lyr-id col {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/column-segs htm prev-htm rgn-id lyr-id col :distal)
                    (id-missing-response id steps-offset))))

          "get-column-proximal-segments"
          (let [[id rgn-id lyr-id col {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/column-segs htm prev-htm rgn-id lyr-id col :proximal)
                    (id-missing-response id steps-offset))))

          "get-apical-segment-synapses"
          (let [[id rgn-id lyr-id col ci si syn-states {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/segment-syns htm prev-htm rgn-id lyr-id col ci si
                                       syn-states :apical)
                    (id-missing-response id steps-offset))))

          "get-distal-segment-synapses"
          (let [[id rgn-id lyr-id col ci si syn-states {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/segment-syns htm prev-htm rgn-id lyr-id col ci si
                                       syn-states :distal)
                    (id-missing-response id steps-offset))))

          "get-proximal-segment-synapses"
          (let [[id rgn-id lyr-id col ci si syn-states {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/segment-syns htm prev-htm rgn-id lyr-id col ci si
                                       syn-states :proximal)
                    (id-missing-response id steps-offset))))

          "get-details-text"
          (let [[id rgn-id lyr-id col {response-c :ch}] xs]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (org.numenta.sanity.comportex.details/detail-text
                     htm prev-htm rgn-id lyr-id col)
                    (id-missing-response id steps-offset))))

          "get-model"
          (let [[id {response-c :ch} as-str?] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (cond-> htm
                      as-str? pr-str)
                    (id-missing-response id steps-offset))))

          "get-column-state-freqs"
          (let [[id {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (into {}
                          (for [[region-key rgn] (:regions htm)
                                layer-id (core/layers rgn)]
                            [[region-key layer-id]
                             (-> (get-in htm [:regions region-key])
                                 (core/column-state-freqs layer-id)
                                 stringify-keys)]))
                    (id-missing-response id steps-offset))))

          "get-cell-excitation-data"
          (let [[id rgn-id lyr-id sel-col {response-c :ch}] xs]
            (put! response-c
                  (let [[prev-htm htm] (find-model-pair id)]
                    (if prev-htm
                      (data/cell-excitation-data htm prev-htm rgn-id lyr-id
                                                 sel-col)
                      (id-missing-response id steps-offset)))))

          "get-cells-by-state"
          (let [[id rgn-id lyr-id {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (let [layer (get-in htm [:regions rgn-id lyr-id])]
                      {:winner-cells (p/winner-cells layer)
                       :active-cells (p/active-cells layer)
                       :pred-cells (p/predictive-cells layer)
                       :engaged? (get-in layer [:state :engaged?])})
                    (id-missing-response id steps-offset))))

          "get-transitions-data"
          (let [[id rgn-id lyr-id cell-sdr-fracs {response-c :ch}] xs]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (data/transitions-data htm rgn-id lyr-id cell-sdr-fracs)
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
      (let [c (<! commands-c)]
        (if-not (nil? c)
          (do
            (handle-command c)
            (recur))
          (println "CLOSING JOURNAL"))))))
