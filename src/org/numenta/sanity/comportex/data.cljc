(ns org.numenta.sanity.comportex.data
  (:require #?(:clj [clojure.core.async :as async :refer [put! <! go go-loop]]
               :cljs [cljs.core.async :as async :refer [put! <!]])
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

(defn all-cell-segments
  [sg cell-id]
  (->> (p/cell-segments sg cell-id)
       (reverse)
       (drop-while empty?)
       (reverse)))

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
                       "inactive"))
       "perm" perm})))


(defn expand-seg-selector
  "Lazily convert a seg-selector into a tree of indices.

  Some example seg-selectors:
  [] ;; none
  [1 7] ;; all segments for columns 1, 7
  {1 [2]} ;; all segments for column 1, cell 2
  {1 {2 [3 4]}} ;; the third and fourth segments on column 1, cell 2"
  [seg-selector layer-depth sg seg-type]
  (let [specific-cells? (map? seg-selector)
        cols (if specific-cells?
               (keys seg-selector)
               seg-selector)]
    (for [col cols
          :let [selector-within-col (when specific-cells?
                                      (get seg-selector col))
                specific-segs? (map? selector-within-col)
                cell-indices (if specific-cells?
                               (if specific-segs?
                                 (keys selector-within-col)
                                 selector-within-col)
                               (if (= seg-type :proximal)
                                 [0]
                                 (range layer-depth)))]]
      [col
       (for [ci cell-indices
             :let [seg-indices (if specific-segs?
                                 (get selector-within-col ci)
                                 (range (count
                                         (all-cell-segments sg [col ci]))))]]
         [ci
          (for [si seg-indices]
            si)])])))

(defn query-segs
  [htm prev-htm rgn-id lyr-id seg-selector seg-type]
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
        sg-key (case seg-type
                 :apical :apical-sg
                 :distal :distal-sg
                 :proximal :proximal-sg)
        sg (get lyr sg-key)
        prev-sg (get-in prev-htm [:regions rgn-id lyr-id sg-key])
        depth (p/layer-depth lyr)]
    (into
     {}
     (for [[col cells] (expand-seg-selector seg-selector depth sg seg-type)]
       [col
        (into
         {}
         (for [[ci seg-indices] cells
               :let [cell-id [col ci]
                     learn-seg-paths (->> (get learning cell-id)
                                          (map :target-id))
                     learn-seg-indices (into #{} (map (fn [[_ _ si]] si))
                                             learn-seg-paths)
                     ;; get synapse info from before learning, so from prev step
                     p-segs (when prev-htm
                              (all-cell-segments prev-sg [col ci]))]]
           [ci
            (into
             {}
             (for [si seg-indices
                   :let [seg (nth p-segs si {}) ;; segment may have just grown
                         grouped-syns (group-synapses seg on-bits pcon)
                         conn-act (count (grouped-syns [:connected :active]))
                         conn-tot (+ (count (grouped-syns [:connected
                                                           :inactive]))
                                     conn-act)
                         disc-act (count (grouped-syns [:disconnected :active]))
                         disc-tot (+ (count (grouped-syns [:disconnected
                                                           :inactive]))
                                     disc-act)]]
               [si
                {"learn-seg?" (contains? learn-seg-indices si)
                 "n-conn-act" conn-act
                 "n-conn-tot" conn-tot
                 "n-disc-act" disc-act
                 "n-disc-tot" disc-tot
                 "stimulus-th" stimulus-th
                 "learning-th" learning-th}]))]))]))))

(defn query-syns
  [htm prev-htm rgn-id lyr-id seg-selector syn-states seg-type]
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
        sg-key (case seg-type
                 :apical :apical-sg
                 :distal :distal-sg
                 :proximal :proximal-sg)
        sg (get lyr sg-key)
        prev-sg (get-in prev-htm [:regions rgn-id lyr-id sg-key])
        ;; need to know which layers have input across regions
        input-layer? (into #{} (map (fn [[rgn-id rgn]]
                                      [rgn-id (first (core/layers rgn))])
                                    regions))
        ;; need to know the output layer of each region
        output-layer (into {} (map (fn [[rgn-id rgn]]
                                     [rgn-id (last (core/layers rgn))])
                                   regions))
        dt (case seg-type
             :apical -1
             :distal -1
             :proximal 0)
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
        source-id-and-dt-of-bit
        (fn [htm rgn-id lyr-id i]
          (let [[src-id src-lyr-id src-i] (source-of-bit htm rgn-id lyr-id i)]
            (if (and (= :proximal seg-type)
                     (contains? (:regions htm) src-id))
              (let [src-state (get-in htm [:regions src-id src-lyr-id :state])
                    src-depth (get-in htm [:regions src-id src-lyr-id :spec :depth])
                    prev-ss (get-in prev-htm [:regions src-id src-lyr-id :state])
                    src-cell-id [(quot src-i src-depth)
                                 (rem src-i src-depth)]
                    src-dt (loop [dt 0
                                  csets (cons (:active-cells src-state)
                                              (reverse (:stable-cells-buffer prev-ss)))]
                             (if-let [cset (first csets)]
                               (if (contains? cset src-cell-id)
                                 dt
                                 (recur (dec dt) (next csets)))
                               ;; ran out of cell sets; must be inactive syn
                               0))]
                [src-id src-lyr-id src-i src-dt])
              [src-id src-lyr-id src-i dt])))]
    (for [[col cells] (expand-seg-selector seg-selector depth sg seg-type)]
      [col
       (into
        {}
        (for [[ci seg-indices] cells
              :let [cell-id [col ci]
                    si->seg-update (let [upd (get learning cell-id)
                                         [_ _ si] (:target-id upd)]
                                     {si upd})
                    p-segs (when prev-htm
                             (all-cell-segments prev-sg [col ci]))]]
          [ci
           (into
            {}
            (for [si seg-indices
                  :let [seg (nth p-segs si {}) ;; segment may have just grown
                        grow-sources (-> (si->seg-update si) :grow-sources)
                        grouped-syns (group-synapses seg on-bits pcon)
                        grouped-sourced-syns (util/remap
                                              (fn [syns]
                                                (map (fn [[i p]]
                                                       [i
                                                        (source-id-and-dt-of-bit
                                                         htm rgn-id lyr-id i)
                                                        p])
                                                     syns))
                                              (assoc grouped-syns
                                                     :growing
                                                     (map vector grow-sources
                                                          (repeat pinit))))
                        syn-sources (cond-> {}
                                      (contains? syn-states "active")
                                      (assoc "active"
                                             (grouped-sourced-syns
                                              [:connected :active]))

                                      (contains? syn-states "inactive")
                                      (assoc "inactive"
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
                                             (grouped-sourced-syns :growing)))]]
              [si
               (->> syn-sources
                    (util/remap (fn [source-info]
                                  (for [[i [src-id src-lyr src-i src-dt] p] source-info]
                                    {"src-i" src-i
                                     "src-id" src-id
                                     "src-lyr" (when src-lyr src-lyr)
                                     "src-dt" src-dt
                                     "perm" p}))))]))]))])))

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

(defn network-shape
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
                                           "cells-per-column" (p/layer-depth
                                                               lyr)
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
