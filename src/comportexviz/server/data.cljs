(ns comportexviz.server.data
  (:require [cljs.core.async :as async :refer [chan put! <!]]
            [comportexviz.details]
            [comportexviz.helpers :as helpers]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn all-cell-segments
  [col depth distal-sg]
  (let [cell-ids (map vector (repeat col) (range depth))]
    (mapv (fn [cell-id]
            (->> (p/cell-segments distal-sg cell-id)
                 (reverse)
                 (drop-while empty?)
                 (reverse)))
          cell-ids)))

(defn group-synapses
  [syns ac pcon]
  (group-by (fn [[id p]]
              [(if (>= p pcon)
                 :connected :disconnected)
               (if (ac id)
                 :active :inactive)])
            syns))

(defn active-bits
  [inp]
  (if (:encoder inp)
    (p/bits-value inp)
    (p/motor-bits-value inp)))

(defn count-segs-in-column
  [distal-sg depth col]
  (reduce (fn [n ci]
            (+ n (util/count-filter seq
                                    (p/cell-segments distal-sg [col ci]))))
          0
          (range depth)))

(defn ff-synapses-data
  [htm selection opts]
  (let [{sel-rgn :region, sel-lyr :layer, sel-col :col} selection
        do-growing? (get-in opts [:ff-synapses :growing])
        do-inactive? (get-in opts [:ff-synapses :inactive])
        do-disconn? (get-in opts [:ff-synapses :disconnected])
        do-perm? (get-in opts [:ff-synapses :permanences])
        syn-states (concat (when do-disconn? [:disconnected])
                           (when do-inactive? [:inactive-syn])
                           [:active :active-predicted]
                           (when do-growing? [:growing]))
        regions (:regions htm)
        ;; need to know which layers have input across regions
        input-layer? (into #{} (map (fn [[rgn-id rgn]]
                                      [rgn-id (first (core/layers rgn))])
                                    regions))
        ;; need to know the output layer of each region
        output-layer (into {} (map (fn [[rgn-id rgn]]
                                     [rgn-id (last (core/layers rgn))])
                                   regions))
        this-rgn (get regions sel-rgn)
        this-lyr (get this-rgn sel-lyr)
        to-cols (case (get-in opts [:ff-synapses :to])
                  :all (p/active-columns this-lyr)
                  :selected [sel-col])
        this-paths (map #(vector sel-rgn sel-lyr %) to-cols)]

    ;; trace ff connections downwards
    (loop [path (first this-paths)
           more (rest this-paths)
           path->synapses {}]
      (if (and path (not (contains? path->synapses path)))
        (let [[rgn-id lyr-id col] path
              lyr (get-in regions [rgn-id lyr-id])
              in-bits (:in-ff-bits (:state lyr))
              in-sbits (:in-stable-ff-bits (:state lyr))
              sg (:proximal-sg lyr)
              prox-learning (:proximal-learning (:state lyr))
              seg-up (get prox-learning [col 0])
              {learn-seg-path :target-id, grow-sources :grow-sources} seg-up
              this-seg-path (or learn-seg-path [col 0 0])
              all-syns (p/in-synapses sg this-seg-path)
              conn-syns (select-keys all-syns
                                     (p/sources-connected-to sg this-seg-path))

              syns-to-draw
              (for [syn-state syn-states
                    :let [sub-syns
                          (case syn-state
                            :active (select-keys conn-syns in-bits)
                            :active-predicted (select-keys conn-syns in-sbits)
                            :inactive-syn (if do-disconn?
                                            (apply dissoc all-syns in-bits)
                                            (apply dissoc conn-syns in-bits))
                            :disconnected (-> (apply dissoc all-syns
                                                     (keys conn-syns))
                                              (select-keys in-bits))
                            :growing (select-keys conn-syns grow-sources))]
                    [i perm] sub-syns
                    :let [[src-id src-lyr src-i]
                          (if (input-layer? [rgn-id lyr-id])
                            ;; input from another region
                            (let [[src-id src-i]
                                  (core/source-of-incoming-bit htm rgn-id i)]
                              [src-id (output-layer src-id) src-i])
                            ;; input from another layer in same region (hardcoded)
                            [rgn-id :layer-4 i])

                          src-col (if src-lyr
                                    (first (p/source-of-bit
                                            (get-in regions [src-id src-lyr])
                                            src-i))
                                    src-i)]]
                (cond-> {:src-id src-id
                         :src-col src-col
                         :syn-state syn-state}
                  do-perm? (assoc :perm perm)
                  src-lyr (assoc :src-lyr src-lyr)))]
          (recur (first more)
                 (->> syns-to-draw
                      (map (fn [{:keys [src-id src-lyr src-col]}]
                             (when src-lyr
                               ;; source is a cell not an input bit, so continue
                               ;; tracing
                               [src-id src-lyr src-col])))
                      (remove nil?)
                      (into (next more)))
                 (assoc path->synapses
                        path syns-to-draw)))
        ;; go on to next
        (if (not-empty more)
          (recur (first more) (next more) path->synapses)
          path->synapses)))))

(defn cell-segments-data
  [htm prev-htm selection opts]
  (let [{sel-rgn :region, sel-lyr :layer, col :col, sel-ci-si :cell-seg} selection
        regions (:regions htm)
        lyr (get-in regions [sel-rgn sel-lyr])
        spec (p/params lyr)
        stimulus-th (:seg-stimulus-threshold spec)
        learning-th (:seg-learn-threshold spec)
        pcon (:distal-perm-connected spec)
        pinit (:distal-perm-init spec)
        ac (p/active-cells lyr)
        prev-lyr (get-in prev-htm [:regions sel-rgn sel-lyr])
        prev-pc (:pred-cells (:prior-distal-state lyr))
        prev-aci (:distal-bits (:prior-distal-state lyr))
        depth (p/layer-depth lyr)
        learning (:distal-learning (:state lyr))
        seg-up (first (vals (select-keys learning (for [ci (range depth)] [col ci]))))
        {[_ learn-ci learn-si] :target-id, grow-sources :grow-sources} seg-up
        segs-by-cell (->> (:distal-sg lyr)
                          (all-cell-segments col depth))
        p-segs-by-cell (when prev-htm
                         (->> (get-in prev-htm [:regions sel-rgn sel-lyr :distal-sg])
                              (all-cell-segments col depth)))]
    (into
     {}
     (for [[ci segs] (map-indexed vector segs-by-cell)
           :let [p-segs (nth p-segs-by-cell ci)
                 cell-id [col ci]
                 cell-active? (boolean (ac cell-id))
                 cell-predictive? (boolean (get prev-pc cell-id))
                 cell-learning? (= ci learn-ci)
                 ;; need to add an entry for a new segment if just grown
                 use-segs (if (and cell-learning? (>= learn-si (count p-segs)))
                            (take (inc learn-si) (concat p-segs (repeat {})))
                            p-segs)]]

       [ci {:cell-active? cell-active?
            :cell-predictive? cell-predictive?
            :cell-state (cond
                          (and cell-active? cell-predictive?) :active-predicted
                          cell-predictive? :predicted
                          cell-active? :active
                          :else :inactive)
            :selected-cell? (if sel-ci-si
                              (== ci (first sel-ci-si))
                              cell-learning?)
            :segments (into
                       {}
                       (for [[si seg] (map-indexed vector use-segs)
                             :let [grouped-syns (group-synapses seg prev-aci pcon)
                                   conn-act (count (grouped-syns [:connected :active]))
                                   conn-tot (+ (count (grouped-syns [:connected :inactive]))
                                               conn-act)
                                   disc-act (count (grouped-syns [:disconnected :active]))
                                   disc-tot (+ (count (grouped-syns [:disconnected :inactive]))
                                               disc-act)
                                   learn-seg? (and cell-learning? (= si learn-si))
                                   selected-seg? (if sel-ci-si
                                                   (= [ci si] sel-ci-si)
                                                   learn-seg?)

                                   do-from (get-in opts [:distal-synapses :from])
                                   do-disconn? (get-in opts [:distal-synapses :disconnected])
                                   grouped-sourced-syns
                                   (util/remap (fn [syns]
                                                 (map (fn [[i p]]
                                                        [i
                                                         (core/source-of-distal-bit
                                                          htm sel-rgn sel-lyr i)
                                                         p])
                                                      syns))
                                               (assoc grouped-syns
                                                      :growing (when learn-seg?
                                                                 (map vector grow-sources
                                                                      (repeat pinit)))))]]
                         [si (cond-> {:learn-seg? learn-seg?
                                      :selected-seg? selected-seg?
                                      :n-conn-act conn-act
                                      :n-conn-tot conn-tot
                                      :n-disc-act disc-act
                                      :n-disc-tot disc-tot
                                      :stimulus-th stimulus-th
                                      :learning-th learning-th}
                               (or (= do-from :all)
                                   (and (= do-from :selected) selected-seg?))
                               (assoc :syns-by-state
                                      (->> (cond-> {}
                                             true
                                             (assoc :active
                                                    (grouped-sourced-syns
                                                     [:connected :active]))

                                             (get-in opts [:distal-synapses :inactive])
                                             (assoc :inactive-syn
                                                    (concat (grouped-sourced-syns
                                                             [:connected :inactive])
                                                            (if do-disconn?
                                                              (grouped-sourced-syns
                                                               [:disconnected
                                                                :inactive]))))

                                             do-disconn?
                                             (assoc :disconnected
                                                    (grouped-sourced-syns
                                                     [:disconnected :active]))

                                             (get-in opts [:distal-synapses :growing])
                                             (assoc :growing
                                                    (grouped-sourced-syns :growing)))
                                           (util/remap (fn [source-info]
                                                         (for [[i [src-id src-lyr
                                                                   src-i] p] source-info]
                                                           (cond-> {:src-col
                                                                    (if src-lyr
                                                                      (first (p/source-of-bit
                                                                              (get-in regions
                                                                                      [src-id src-lyr])
                                                                              src-i))
                                                                      src-i)

                                                                    :src-id src-id
                                                                    :src-lyr src-lyr}

                                                             (get-in opts [:distal-synapses :permanences])
                                                             (assoc :perm p))))))))]))}]))))

(defn inbits-cols-data [htm prev-htm opts]
  {:inputs (into
            {}
            (for [inp-id (core/input-keys htm)
                  :let [inp (get-in htm [:inputs inp-id])
                        ;; region this input feeds to, for predictions
                        ff-rgn-id (first (get-in htm [:fb-deps inp-id]))
                        ;; TODO offset if multiple inputs feeding to region
                        prev-ff-rgn (when (pos? (p/size (p/ff-topology inp)))
                                      (get-in prev-htm [:regions ff-rgn-id]))]]
              [inp-id (cond-> {}
                        (get-in opts [:input :active])
                        (assoc :active-bits (active-bits inp))

                        (and (get-in opts [:input :predicted]) prev-ff-rgn)
                        (assoc :pred-bits-alpha
                               (->> (core/predicted-bit-votes prev-ff-rgn)
                                    (util/remap #(min 1.0 (/ % 8))))))]))

   :regions (into
             {}
             (for [rgn-id (core/region-keys htm)
                   :let [rgn (get-in htm [:regions rgn-id])]]
               [rgn-id (into
                        {}
                        (for [lyr-id (core/layers rgn)
                              :let [lyr (get rgn lyr-id)
                                    spec (p/params lyr)]]
                          [lyr-id (cond-> {}
                                    (get-in opts [:columns :overlaps])
                                    (assoc :overlaps-columns-alpha
                                           (->> (:col-overlaps (:state lyr))
                                                (reduce-kv (fn [m [col _ _] v]
                                                             (assoc!
                                                              m col
                                                              (max v (get m col
                                                                          0))))
                                                           (transient {}))
                                                (persistent!)
                                                (util/remap #(min 1.0
                                                                  (/ % 16)))))

                                    (get-in opts [:columns :boosts])
                                    (assoc :boost-columns-alpha
                                           (->> (:boosts lyr)
                                                (map
                                                 #(/ (dec %)
                                                     (dec (:max-boost spec))))
                                                (zipmap (range))))

                                    (get-in opts [:columns :active-freq])
                                    (assoc :active-freq-columns-alpha
                                           (->> (:active-duty-cycles lyr)
                                                (map #(min 1.0 (* 2 %)))
                                                (zipmap (range))))

                                    ;; Always include, needed for sorting.
                                    ;; Check [:columns :active] before drawing.
                                    true
                                    (assoc :active-columns
                                           (p/active-columns lyr))

                                    (get-in opts [:columns :predictive])
                                    (assoc :pred-columns
                                           (->> (p/prior-predictive-cells lyr)
                                                (map first)
                                                (distinct)))

                                    (get-in opts [:columns :temporal-pooling])
                                    (assoc :tp-columns
                                           (->> (p/temporal-pooling-cells lyr)
                                                (map first)))

                                    true
                                    (assoc :break?
                                           (-> lyr
                                               (get-in [:prior-distal-state
                                                        :distal-bits])
                                               empty?)))]))]))})

(defn inbits-cols-data-subset
  [htm path->ids opts]
  {:inputs (into
            {}
            (for [inp-id (core/input-keys htm)
                  :let [bits-subset (path->ids [:inputs inp-id])]]
              [inp-id {}]))
   :regions (into
             {}
             (for [rgn-id (core/region-keys htm)
                   :let [rgn (get-in htm [:regions rgn-id])]]
               [rgn-id (into
                        {}
                        (for [lyr-id (core/layers rgn)
                              :let [cols-subset (path->ids [:regions rgn-id
                                                            lyr-id])
                                    lyr (get rgn lyr-id)
                                    spec (p/params lyr)]]
                          [lyr-id (cond-> {}
                                    (get-in opts [:columns :n-segments])
                                    (assoc :n-segments-columns-alpha
                                           (->> cols-subset
                                                (map #(count-segs-in-column
                                                       (:distal-sg lyr)
                                                       (p/layer-depth lyr) %))
                                                (map #(min 1.0 (/ % 16.0)))
                                                (zipmap cols-subset))))]))]))})

(defn cell-excitation-data
  [htm prior-htm rgn-id lyr-id sel-col]
  (let [lc (get-in htm [:regions rgn-id lyr-id :state :learn-cells])
        lc+ (if sel-col
              (let [prior-lc (get-in prior-htm [:regions rgn-id lyr-id :state
                                                :learn-cells])
                    sel-cell (or (first (filter (fn [[col _]]
                                                  (= col sel-col))
                                                (concat prior-lc lc)))
                                 [sel-col 0])]
                (conj lc sel-cell))
              lc)]
    (core/cell-excitation-breakdowns htm prior-htm rgn-id lyr-id lc+)))

(defn step-template-data
  [htm]
  {:inputs (into
            {}
            (for [inp-id (core/input-keys htm)
                  :let [inp (get-in htm [:inputs inp-id])]]
              [inp-id {:topology (p/topology inp)}]))

   :regions (into
             {}
             (for [rgn-id (core/region-keys htm)
                   :let [rgn (get-in htm [:regions rgn-id])]]
               [rgn-id (into
                        {}
                        (for [lyr-id (core/layers rgn)
                              :let [lyr (get rgn lyr-id)
                                    spec (p/params lyr)]]
                          [lyr-id {:spec (p/params lyr)
                                   :topology (p/topology lyr)}]))]))})
