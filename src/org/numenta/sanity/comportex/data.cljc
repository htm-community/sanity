(ns org.numenta.sanity.comportex.data
  (:require [org.nfrac.comportex.core :as cx]
            [org.nfrac.comportex.synapses :as syn]
            [org.nfrac.comportex.layer :as layer]
            [org.nfrac.comportex.layer.tools :as layertools]
            [org.nfrac.comportex.util :as util]))

(defn all-cell-segments
  [sg cell-id]
  (->> (syn/cell-segments sg cell-id)
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

(defn count-segs-in-column
  [distal-sg depth col]
  (reduce (fn [n ci]
            (+ n (util/count-filter seq
                                    (syn/cell-segments distal-sg [col ci]))))
          0
          (range depth)))

(defn syns-from-source-bit
  [htm sense-id bit syn-states]
  (let [active-bit? (->> (:bits (get-in htm [:senses sense-id]))
                         (some (partial = bit))
                         boolean)
        {:keys [fb-deps]} (cx/add-feedback-deps htm)]
    (for [lyr-id (get fb-deps sense-id)
          :let [lyr (get-in htm [:layers lyr-id])
                sg (:proximal-sg lyr)
                adjusted-bit (+ (cx/ff-base htm lyr-id sense-id)
                                bit)
                to-segs (syn/targets-connected-from sg adjusted-bit)
                predictive-columns (->> (cx/layer-state lyr)
                                        (:prior-predictive-cells)
                                        (map first)
                                        (into #{}))]
          [col _ _ :as seg-path] to-segs
          :let [predictive-col? (contains? predictive-columns col)]
          :when (or (contains? syn-states "inactive")
                    (and (contains? syn-states "predicted")
                         predictive-col?)
                    active-bit?)
          :let [perm (get (syn/in-synapses sg seg-path) adjusted-bit)]]
      {"target-lyr" lyr-id
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
  [htm prev-htm lyr-id seg-selector seg-type]
  (let [lyr (get-in htm [:layers lyr-id])
        params (cx/params lyr)
        dparams (get params seg-type)
        stimulus-th (:stimulus-threshold dparams)
        learning-th (:learn-threshold dparams)
        pcon (:perm-connected dparams)
        on-bits (set
                 (case seg-type
                  :apical (get-in lyr [:prior-apical-state :active-bits])
                  :distal (get-in lyr [:prior-distal-state :active-bits])
                  :proximal (get-in lyr [:active-state :in-ff-signal :bits])))
        learning (get-in lyr [:learn-state :learning seg-type])
        sg-key (case seg-type
                 :apical :apical-sg
                 :distal :distal-sg
                 :proximal :proximal-sg)
        sg (get lyr sg-key)
        prev-sg (get-in prev-htm [:layers lyr-id sg-key])
        depth (:depth params)]
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
  [htm prev-htm lyr-id seg-selector syn-states seg-type]
  (let [lyr (get-in htm [:layers lyr-id])
        params (cx/params lyr)
        dparams (get params seg-type)
        pcon (:perm-connected dparams)
        pinit (:perm-init dparams)
        on-bits (set
                 (case seg-type
                  :apical (get-in lyr [:prior-apical-state :active-bits])
                  :distal (get-in lyr [:prior-distal-state :active-bits])
                  :proximal (get-in lyr [:active-state :in-ff-signal :bits])))

        learning (get-in lyr [:learn-state :learning seg-type])
        sg-key (case seg-type
                 :apical :apical-sg
                 :distal :distal-sg
                 :proximal :proximal-sg)
        sg (get lyr sg-key)
        prev-sg (get-in prev-htm [:layers lyr-id sg-key])
        dt (case seg-type
             :apical -1
             :distal -1
             :proximal 0)
        source-of-bit (case seg-type
                        :distal #(layertools/source-of-distal-bit htm lyr-id %)
                        :apical #(cx/source-of-incoming-bit htm lyr-id % :fb-deps)
                        :proximal #(cx/source-of-incoming-bit htm lyr-id % :ff-deps))
        source-id-and-dt-of-bit
        (fn [i]
          (let [[src-id src-i] (source-of-bit i)]
            (if (and (= :proximal seg-type)
                     (contains? (:layers htm) src-id))
              (let [src-state (get-in htm [:layers src-id :active-state])
                    src-depth (get-in htm [:layers src-id :params :depth])
                    prev-ss (get-in prev-htm [:layers src-id :active-state])
                    src-cell-id (layer/id->cell src-depth src-i)
                    src-dt (loop [dt 0
                                  csets (cons (:active-cells src-state)
                                              (reverse (:stable-cells-buffer prev-ss)))]
                             (if-let [cset (first csets)]
                               (if (contains? cset src-cell-id)
                                 dt
                                 (recur (dec dt) (next csets)))
                               ;; ran out of cell sets; must be inactive syn
                               0))]
                [src-id src-i src-dt])
              [src-id src-i dt])))
        depth (:depth params)]
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
                                                        (source-id-and-dt-of-bit i)
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
                                  (for [[i [src-id src-i src-dt] p] source-info]
                                    {"src-i" src-i
                                     "src-id" src-id
                                     "src-dt" src-dt
                                     "perm" p}))))]))]))])))

(defn cell-excitation-data
  [htm prior-htm lyr-id sel-col]
  (let [wc (-> (get-in htm [:layers lyr-id]) (cx/layer-state) :winner-cells)
        wc+ (if sel-col
              (let [prior-wc (-> (get-in prior-htm [:layers lyr-id]) (cx/layer-state) :winner-cells)
                    sel-cell (or (first (filter (fn [[col _]]
                                                  (= col sel-col))
                                                (concat prior-wc wc)))
                                 [sel-col 0])]
                (conj wc sel-cell))
              wc)]
    (layertools/cell-excitation-breakdowns htm prior-htm lyr-id wc+)))

(defn network-shape
  [htm]
  (let [sense-keys (cx/sense-keys htm)]
    {"senses" (->> (map vector (range) sense-keys)
                   (reduce (fn [m [ordinal sense-id]]
                             (let [sense (get-in htm [:senses sense-id])]
                               (assoc m sense-id
                                      {"dimensions" (cx/dims-of sense)
                                       "ordinal" ordinal})))
                           {}))
     "layers" (->> (map vector (range) (cx/layer-keys htm))
                   (reduce (fn [m [ordinal lyr-id]]
                             (let [lyr (get-in htm [:layers lyr-id])]
                               (assoc m lyr-id
                                      {"params" (cx/params lyr)
                                       "dimensions" (pop (cx/dims-of lyr))
                                       "cells-per-column" (peek (cx/dims-of lyr))
                                       "ordinal" (+ ordinal
                                                    (count sense-keys))})))
                           {}))}))

(defn cell-cells-transitions
  [distal-sg depth n-cols]
  (let [all-cell-ids (for [col (range n-cols)
                           ci (range depth)]
                       [col ci])]
    (->> all-cell-ids
         (reduce (fn [m from-cell]
                   (let [source-id (layer/cell->id depth from-cell)
                         to-segs (syn/targets-connected-from distal-sg source-id)
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
  [htm lyr-id cell-sdr-counts]
  (let [lyr (get-in htm [:layers lyr-id])
        params (cx/params lyr)
        depth (:depth params)
        distal-sg (:distal-sg lyr)
        cell-sdr-fracs (util/remap freqs->fracs cell-sdr-counts)]
    (-> (cell-cells-transitions distal-sg depth (:n-columns lyr))
        (cell-sdr-transitions cell-sdr-fracs)
        (sdr-sdr-transitions cell-sdr-fracs))))
