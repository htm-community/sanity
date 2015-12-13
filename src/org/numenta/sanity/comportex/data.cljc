(ns org.numenta.sanity.comportex.data
  (:require #?(:clj [clojure.core.async :as async :refer [put! <! go go-loop]]
               :cljs [cljs.core.async :as async :refer [put! <!]])
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

(defn all-cell-segments
  [cell-ids sg]
  (mapv (fn [cell-id]
          (->> (p/cell-segments sg cell-id)
               (reverse)
               (drop-while empty?)
               (reverse)))
        cell-ids))

(defn group-synapses
  [syns ac pcon]
  (group-by (fn [[id p]]
              [(if (>= p pcon)
                 :connected :disconnected)
               (if (ac id)
                 :active :inactive)])
            syns))

(defn active-bits
  [sense-node]
  (or (seq (p/bits-value sense-node))
      (p/motor-bits-value sense-node)))

(defn count-segs-in-column
  [distal-sg depth col]
  (reduce (fn [n ci]
            (+ n (util/count-filter seq
                                    (p/cell-segments distal-sg [col ci]))))
          0
          (range depth)))

(defn syns-from-source-bit
  [htm sense-id bit syn-states]
  (let [active-bit? (->> (active-bits (get-in htm [:senses sense-id]))
                         (some (partial = bit))
                         boolean)]
    (for [rgn-id (get-in htm [:fb-deps sense-id])
          :let [rgn (get-in htm [:regions rgn-id])
                [lyr-id] (core/layers rgn)
                lyr (get rgn lyr-id)
                sg (:proximal-sg lyr)
                adjusted-bit (+ (core/ff-base htm rgn-id sense-id)
                                bit)
                to-segs (p/targets-connected-from sg adjusted-bit)
                predictive-columns (->> (p/prior-predictive-cells lyr)
                                        (map first)
                                        (into #{}))]
          [col _ _ :as seg-path] to-segs
          :let [predictive-col? (contains? predictive-columns col)]
          :when (or (contains? syn-states "inactive")
                    (and (contains? syn-states "predicted")
                         predictive-col?)
                    active-bit?)
          :let [perm (get (p/in-synapses sg seg-path) adjusted-bit)]]
      {"target-id" rgn-id
       "target-lyr" lyr-id
       "target-col" col
       "target-dt" 0
       "syn-state" (if active-bit?
                     (if predictive-col?
                       "active-predicted"
                       "active")
                     (if predictive-col?
                       "predicted"
                       "inactive-syn"))
       "perm" perm})))

(defn column-segs
  [htm prev-htm rgn-id lyr-id col seg-type]
  (let [lyr (get-in htm [:regions rgn-id lyr-id])
        spec (p/params lyr)
        dspec (get spec seg-type)
        stimulus-th (:stimulus-threshold dspec)
        learning-th (:learn-threshold dspec)
        pcon (:perm-connected dspec)
        on-bits (case seg-type
                  :apical (get-in lyr [:prior-apical-state :active-bits])
                  :distal (get-in lyr [:prior-distal-state :active-bits])
                  :proximal (get-in lyr [:state :in-ff-bits]))
        learning (get-in lyr [:learn-state :learning seg-type])
        cell-ids (if (= seg-type :proximal)
                   [[col 0]]
                   (for [ci (range (p/layer-depth lyr))]
                     [col ci]))
        seg-up (first
                (vals (select-keys learning cell-ids)))
        {[_ learn-ci learn-si] :target-id} seg-up
        sg-key (case seg-type
                 :apical :apical-sg
                 :distal :distal-sg
                 :proximal :proximal-sg)
        segs-by-cell (all-cell-segments cell-ids (get lyr sg-key))
        ;; get synapse info from before learning -- so from prev step:
        p-segs-by-cell (when prev-htm
                         (->> (get-in prev-htm [:regions rgn-id lyr-id sg-key])
                              (all-cell-segments cell-ids)))]
    (into
     {}
     (for [[ci _] (map-indexed vector segs-by-cell)
           :let [cell-learning? (= ci learn-ci)
                 ;; need to add an entry for a new segment if just grown
                 p-segs (nth p-segs-by-cell ci)
                 use-segs (if (and cell-learning? (>= learn-si (count p-segs)))
                            (take (inc learn-si) (concat p-segs (repeat {})))
                            p-segs)]]
       [ci
        (into
         {}
         (for [[si seg] (map-indexed vector use-segs)
               :let [grouped-syns (group-synapses seg on-bits pcon)
                     conn-act (count (grouped-syns [:connected :active]))
                     conn-tot (+ (count (grouped-syns [:connected :inactive]))
                                 conn-act)
                     disc-act (count (grouped-syns [:disconnected :active]))
                     disc-tot (+ (count (grouped-syns [:disconnected :inactive]))
                                 disc-act)]]
           [si
            {"learn-seg?" (and (= ci learn-ci)
                              (= si learn-si))
             "n-conn-act" conn-act
             "n-conn-tot" conn-tot
             "n-disc-act" disc-act
             "n-disc-tot" disc-tot
             "stimulus-th" stimulus-th
             "learning-th" learning-th}]))]))))

(defn segment-syns
  [htm prev-htm rgn-id lyr-id col ci si syn-states seg-type]
  (let [regions (:regions htm)
        lyr (get-in regions [rgn-id lyr-id])
        spec (p/params lyr)
        dspec (get spec seg-type)
        pcon (:perm-connected dspec)
        pinit (:perm-init dspec)
        on-bits (case seg-type
                  :apical (get-in lyr [:prior-apical-state :active-bits])
                  :distal (get-in lyr [:prior-distal-state :active-bits])
                  :proximal (get-in lyr [:state :in-ff-bits]))
        depth (p/layer-depth lyr)
        learning (get-in lyr [:learn-state :learning seg-type])
        seg-up (first
                (vals (select-keys learning
                                   (for [ci (range depth)]
                                     [col ci]))))
        {[_ learn-ci learn-si] :target-id
         grow-sources :grow-sources} seg-up
        learn-seg? (and (= ci learn-ci)
                        (= si learn-si))
        p-segs (-> prev-htm
                   (get-in [:regions rgn-id lyr-id])
                   (get (case seg-type
                          :apical :apical-sg
                          :distal :distal-sg
                          :proximal :proximal-sg))
                   (p/cell-segments [col ci]))
        seg (if (< si (count p-segs))
              (nth p-segs si)
              ;; need to add an entry for a new segment if just grown
              {})
        ;; need to know which layers have input across regions
        input-layer? (into #{} (map (fn [[rgn-id rgn]]
                                      [rgn-id (first (core/layers rgn))])
                                    regions))
        ;; need to know the output layer of each region
        output-layer (into {} (map (fn [[rgn-id rgn]]
                                     [rgn-id (last (core/layers rgn))])
                                   regions))
        grouped-syns (group-synapses seg on-bits pcon)
        source-of-bit (case seg-type
                        :apical core/source-of-apical-bit
                        :distal core/source-of-distal-bit
                        :proximal
                        (fn [htm rgn-id lyr-id i]
                          (if (input-layer? [rgn-id lyr-id])
                            ;; input from another region
                            (let [[src-id src-i]
                                  (core/source-of-incoming-bit htm rgn-id i
                                                               :ff-deps)]
                              [src-id (output-layer src-id) src-i])
                            ;; input from another layer in same region
                            ;; (hardcoded)
                            [rgn-id :layer-4 i])))
        grouped-sourced-syns (util/remap
                              (fn [syns]
                                (map (fn [[i p]]
                                       [i
                                        (source-of-bit
                                         htm rgn-id lyr-id i)
                                        p])
                                     syns))
                              (assoc grouped-syns
                                     :growing (when learn-seg?
                                                (map vector grow-sources
                                                     (repeat pinit)))))
        syn-sources (cond-> {}
                      (contains? syn-states "active")
                      (assoc "active"
                             (grouped-sourced-syns
                              [:connected :active]))

                      (contains? syn-states "inactive-syn")
                      (assoc "inactive-syn"
                             (concat (grouped-sourced-syns
                                      [:connected :inactive])
                                     (if (:disconnected syn-states)
                                       (grouped-sourced-syns
                                        [:disconnected
                                         :inactive]))))

                      (contains? syn-states "disconnected")
                      (assoc "disconnected"
                             (grouped-sourced-syns
                              [:disconnected :active]))

                      (contains? syn-states "growing")
                      (assoc "growing"
                             (grouped-sourced-syns :growing)))
        dt (case seg-type
             :apical 1
             :distal 1
             :proximal 0)]
    (->> syn-sources
         (util/remap (fn [source-info]
                       (for [[i [src-id src-lyr src-i] p] source-info]
                         {"src-col"
                          (if src-lyr
                            (first (p/source-of-bit
                                    (get-in regions
                                            [src-id src-lyr])
                                    src-i))
                            src-i)

                          "src-id" src-id
                          "src-lyr" (when src-lyr src-lyr)
                          "src-dt" dt
                          "perm" p}))))))

(defn cell-excitation-data
  [htm prior-htm rgn-id lyr-id sel-col]
  (let [wc (p/winner-cells (get-in htm [:regions rgn-id lyr-id]))
        wc+ (if sel-col
              (let [prior-wc (p/winner-cells (get-in prior-htm [:regions rgn-id lyr-id]))
                    sel-cell (or (first (filter (fn [[col _]]
                                                  (= col sel-col))
                                                (concat prior-wc wc)))
                                 [sel-col 0])]
                (conj wc sel-cell))
              wc)]
    (core/cell-excitation-breakdowns htm prior-htm rgn-id lyr-id wc+)))

(defn step-template-data
  [htm]
  (let [sense-keys (core/sense-keys htm)]
    {"senses" (->> (map vector (range) sense-keys)
                   (reduce (fn [st [ordinal sense-id]]
                             (let [sense (get-in htm [:senses sense-id])]
                               (assoc st sense-id
                                      {"dimensions" (p/dims-of sense)
                                       "ordinal" ordinal})))
                           {}))
     "regions" (->> (map vector (range)
                         (for [rgn-id (core/region-keys htm)
                               :let [rgn (get-in htm [:regions rgn-id])]
                               lyr-id (core/layers rgn)]
                           [rgn-id lyr-id]))
                    (reduce (fn [rt [ordinal [rgn-id lyr-id]]]
                              (let [lyr (get-in htm [:regions rgn-id lyr-id])]
                                (assoc-in rt [rgn-id lyr-id]
                                          {"spec" (p/params lyr)
                                           "dimensions" (p/dims-of lyr)
                                           "ordinal" (+ ordinal
                                                        (count sense-keys))})))
                            {}))}))

(defn- cell->id
  [depth [col ci]]
  (+ (* col depth) ci))

(defn cell-cells-transitions
  [distal-sg depth n-cols]
  (let [all-cell-ids (for [col (range n-cols)
                           ci (range depth)]
                       [col ci])]
    (->> all-cell-ids
         (reduce (fn [m from-cell]
                   (let [source-id (cell->id depth from-cell)
                         to-segs (p/targets-connected-from distal-sg source-id)
                         to-cells (map pop to-segs)]
                     (if (seq to-cells)
                       (assoc! m from-cell
                               (into (get m from-cell #{})
                                     to-cells))
                       m)))
                 (transient {}))
         (persistent!))))

(defn- cell-sdr-transitions
  [cell-cells-xns cell-sdr-fracs]
  (->> cell-cells-xns
       (reduce-kv (fn [m from-cell to-cells]
                    (assoc! m from-cell
                            (apply merge-with max
                                   (map cell-sdr-fracs to-cells))))
                  (transient {}))
       (persistent!)))

(defn- sdr-sdr-transitions
  [cell-sdrs-xns cell-sdr-fracs]
  (->> cell-sdrs-xns
       ;; count SDR transitions
       (reduce-kv (fn [m from-cell to-sdrs-fracs]
                    (->>
                     (cell-sdr-fracs from-cell)
                     (reduce-kv (fn [m from-sdr from-frac]
                                  (assoc! m from-sdr
                                          (merge-with
                                           +
                                           (get m from-sdr {})
                                           (util/remap #(* % from-frac) to-sdrs-fracs))))
                                m)))
                  (transient {}))
       (persistent!)
       ;; filter transitions to those with >= 1 fully-specific-cell-equivalent
       (util/remap (fn [to-sdr-frac-sums]
                     (into {} (filter (fn [[_ n]] (>= n 1)))
                           to-sdr-frac-sums)))))

(defn- freqs->fracs
  [freqs]
  (let [total (reduce + (vals freqs))]
    (util/remap #(/ % total) freqs)))

(defn transitions-data
  "Argument cell-sdr-counts is a map from cell id to the SDRs it
  participates in. Each value gives the frequencies map by SDR id
  for that cell.

  Returns the SDR to SDR transitions, derived from the distal synapse
  graph. It is a map from an SDR id to any subsequent SDRs, each
  mapped to the number of connected synapses, weighted by the
  specificity of both the source and target cells to those SDRs."
  [htm rgn-id lyr-id cell-sdr-counts]
  (let [lyr (get-in htm [:regions rgn-id lyr-id])
        depth (p/layer-depth lyr)
        distal-sg (:distal-sg lyr)
        cell-sdr-fracs (util/remap freqs->fracs cell-sdr-counts)]
    (-> (cell-cells-transitions distal-sg depth (p/size-of lyr))
        (cell-sdr-transitions cell-sdr-fracs)
        (sdr-sdr-transitions cell-sdr-fracs))))
