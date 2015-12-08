(ns org.numenta.sanity.comportex.journal
  (:require #?(:clj [clojure.core.async :as async :refer [put! <! go go-loop]]
               :cljs [cljs.core.async :as async :refer [put! <!]])
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
    {:model-id id
     :timestep (p/timestep htm)
     :input-value input-value
     :sensed-values (into {}
                          (for [sense-id (core/sense-keys htm)
                                :let [[selector _] (get-in htm [:sensors
                                                                sense-id])
                                      v (p/extract selector input-value)]]
                            [sense-id v]))
     :senses (into
              {}
              (for [sense-id (core/sense-keys htm)
                    :let [sense (get-in htm [:senses sense-id])]]
                [sense-id {:active-bits (set (data/active-bits sense))}]))
     :regions (into
               {}
               (for [rgn-id (core/region-keys htm)
                     :let [rgn (get-in htm [:regions rgn-id])]]
                 [rgn-id
                  (into
                   {}
                   (for [lyr-id (core/layers rgn)
                         :let [lyr (get rgn lyr-id)]]
                     [lyr-id {:active-columns (p/active-columns lyr)
                              :pred-columns (->> (p/prior-predictive-cells lyr)
                                                 (map first)
                                                 (distinct))}]))]))}))

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
                  [(data/step-template-data @current-model) @capture-options]))

          "set-capture-options"
          (let [[co] xs]
            (reset! capture-options co))

          "get-inbits-cols"
          (let [[id fetches onscreen-bits-marshal {response-c :ch}] xs
                path->ids (:value onscreen-bits-marshal)]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/inbits-cols-data htm prev-htm path->ids fetches)
                    (id-missing-response id steps-offset))))

          "get-proximal-synapses-by-source-bit"
          (let [[id sense-id bit syn-states {response-c :ch}] xs
                sense-id (keyword sense-id)]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (data/syns-from-source-bit htm sense-id bit syn-states)
                    (id-missing-response id steps-offset))))

          "get-column-cells"
          (let [[id rgn-id lyr-id col {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (let [lyr (get-in htm [:regions rgn-id lyr-id])
                          extract-cells #(->> %
                                              (keep (fn [[column ci]]
                                                      (when (= col column)
                                                        ci)))
                                              (into #{}))]
                      {:cells-per-column (p/layer-depth lyr)
                       :active-cells (extract-cells
                                      (p/active-cells lyr))
                       :prior-predicted-cells (extract-cells
                                               (p/prior-predictive-cells lyr))
                       :winner-cells (extract-cells
                                      (p/winner-cells lyr))})
                    (id-missing-response id steps-offset))))

          "get-column-apical-segments"
          (let [[id rgn-id lyr-id col {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/column-segs htm prev-htm rgn-id lyr-id col :apical)
                    (id-missing-response id steps-offset))))

          "get-column-distal-segments"
          (let [[id rgn-id lyr-id col {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/column-segs htm prev-htm rgn-id lyr-id col :distal)
                    (id-missing-response id steps-offset))))

          "get-column-proximal-segments"
          (let [[id rgn-id lyr-id col {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/column-segs htm prev-htm rgn-id lyr-id col :proximal)
                    (id-missing-response id steps-offset))))

          "get-apical-segment-synapses"
          (let [[id rgn-id lyr-id col ci si syn-states {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/segment-syns htm prev-htm rgn-id lyr-id col ci si
                                       syn-states :apical)
                    (id-missing-response id steps-offset))))

          "get-distal-segment-synapses"
          (let [[id rgn-id lyr-id col ci si syn-states {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/segment-syns htm prev-htm rgn-id lyr-id col ci si
                                       syn-states :distal)
                    (id-missing-response id steps-offset))))

          "get-proximal-segment-synapses"
          (let [[id rgn-id lyr-id col ci si syn-states {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
            (put! response-c
                  (if-let [[prev-htm htm] (find-model-pair id)]
                    (data/segment-syns htm prev-htm rgn-id lyr-id col ci si
                                       syn-states :proximal)
                    (id-missing-response id steps-offset))))

          "get-details-text"
          (let [[id rgn-id lyr-id col {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
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
                                 (core/column-state-freqs layer-id))]))
                    (id-missing-response id steps-offset))))

          "get-cell-excitation-data"
          (let [[id rgn-id lyr-id sel-col {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
            (put! response-c
                  (let [[prev-htm htm] (find-model-pair id)]
                    (if prev-htm
                      (data/cell-excitation-data htm prev-htm rgn-id lyr-id
                                                 sel-col)
                      (id-missing-response id steps-offset)))))

          "get-cells-by-state"
          (let [[id rgn-id lyr-id {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
            (put! response-c
                  (if-let [htm (find-model id)]
                    (let [layer (get-in htm [:regions rgn-id lyr-id])]
                      {:winner-cells (p/winner-cells layer)
                       :active-cells (p/active-cells layer)
                       :pred-cells (p/predictive-cells layer)
                       :engaged? (get-in layer [:state :engaged?])})
                    (id-missing-response id steps-offset))))

          "get-transitions-data"
          (let [[id rgn-id lyr-id cell-sdr-fracs
                 {response-c :ch}] xs
                rgn-id (keyword rgn-id)
                lyr-id (keyword lyr-id)]
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
