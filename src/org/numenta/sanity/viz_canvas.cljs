(ns org.numenta.sanity.viz-canvas
  (:require [org.numenta.sanity.viz-layouts :as lay
             :refer [layout-bounds
                     element-xy
                     fill-elements
                     group-and-fill-elements]]
            [reagent.core :as reagent :refer [atom]]
            [goog.dom :as dom]
            [goog.style :as style]
            [org.numenta.sanity.dom :refer [offset-from-target]]
            [org.numenta.sanity.helpers :as helpers :refer [canvas
                                                            resizing-canvas
                                                            window-resize-listener]]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.util :as utilv :refer [tap-c index-of]]
            [org.numenta.sanity.selection :as sel]
            [monet.canvas :as c]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util]
            [cljs.core.async :as async :refer [<! put!]]
            [clojure.walk :refer [keywordize-keys]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]
                   [org.numenta.sanity.macros :refer [with-cache]]))

;;; ## Colours

(defn hsl
  ([h s l] (hsl h s l 1.0))
  ([h s l a]
   (let [h2 (if (keyword? h)
              (case h
                :red 0
                :orange 30
                :yellow 60
                :yellow-green 90
                :green 120
                :blue 210
                :purple 270
                :pink 300)
              ;; otherwise angle
              h)]
     (str "hsla(" h2 ","
          (long (* s 100)) "%,"
          (long (* l 100)) "%,"
          a ")"))))

(defn grey
  [z]
  (let [v (long (* z 255))]
    (str "rgb(" v "," v "," v ")")))

(def state-colors
  {:background "#eee"
   :inactive "white"
   :active (hsl :red 1.0 0.5)
   :predicted (hsl :blue 1.0 0.5 0.5)
   :active-predicted (hsl :purple 1.0 0.4)
   :highlight (hsl :yellow 1 0.65 0.6)
   })

(def syn-colors
  {:active (hsl :red 1.0 0.5)
   :active-predicted (hsl :purple 1.0 0.4)
   :disconnected (hsl :red 1.0 0.5)
   :growing (hsl :green 1.0 0.5)
   :inactive "black"
   :predicted (hsl :blue 1.0 0.5 0.5)})

(def default-viz-options
  {:inbits {:active true
            :predicted true
            :refresh-index 0}
   :columns {:active true
             :overlaps nil
             :boosts nil
             :active-freq nil
             :n-segments nil
             :predictive true
             :refresh-index 0}
   :ff-synapses {:to :selected ;; :selected, :all, :none
                 :trace-back? true
                 :growing true
                 :predicted true
                 :inactive nil
                 :disconnected nil
                 :permanences true}
   :distal-synapses {:to :selected ;; :selected, :all, :none
                     :growing true
                     :inactive nil
                     :disconnected nil
                     :permanences true}
   ;; triggers a rebuild & redraw of the layouts when changed:
   :drawing {:display-mode :one-d ;; :one-d, :two-d
             :draw-steps 16
             :max-height-px 900
             :max-width-px nil
             :top-px 5
             :bit-w-px 4
             :bit-h-px 3
             :bit-shrink 0.85
             :col-d-px 5
             :col-shrink 0.85
             :cell-label-h-px 16
             :cell-r-px 10
             :min-cells-segs-w-px 100
             :seg-w-px 30
             :seg-h-px 8
             :seg-h-space-px 55
             :h-space-px 45
             :h-space-for-text-px 25
             :anim-go? true
             :anim-every 1}})

(defn draw-image-dt
  [ctx lay dt img]
  (let [[x y] (lay/origin-px-topleft lay dt)]
    (c/draw-image ctx img x y)))

(defn sense-layout-paths
  [m]
  (for [sense-id (keys (:senses m))]
    [:senses sense-id]))

(defn layer-layout-paths
  [m]
  (for [[rgn-id rgn] (:regions m)
        lyr-id (keys rgn)]
    [:regions rgn-id lyr-id]))

(defn grid-layout-paths
  [m]
  (concat (sense-layout-paths m) (layer-layout-paths m)))

(defn non-grid-layout-paths
  [m]
  (for [k [:cells-segments :apical-segments]
        :when (get m k)]
    [k]))

(defn all-layout-paths
  [m]
  (concat (grid-layout-paths m)
          (non-grid-layout-paths m)))

(defn grid-layout-vals
  ([m]
   (map (partial get-in m) (grid-layout-paths m))))

(defn reset-layout-caches
  [m]
  (reduce (fn [m path]
            (update-in m path vary-meta
                       (fn [mm]
                         (assoc mm ::cache (atom {})))))
          m
          (grid-layout-paths m)))

(defn stack-layouts
  "`layoutfns` is a sequence of functions that return:

  - A sequence of [path layout] pairs
  - The right edge of this sequence of layouts, i.e. the start point of the next
    layout 'up' the visual stack"
  [layoutfns]
  (let [[layouts _] (->> layoutfns
                         (reduce (fn [[lays left] f]
                                   (let [[changes right] (f left)]
                                     [(reduce (fn [lays [path lay]]
                                                (assoc-in lays path lay))
                                              lays changes)
                                      right]))
                                 [{} 20]))]
    layouts))

(defn create-layouts
  [network-shape opts insertions]
  (let [senses (:senses network-shape)
        regions (:regions network-shape)
        d-opts (:drawing opts)
        display-mode (:display-mode d-opts)
        spacer (:h-space-px d-opts)
        top-px (:top-px d-opts)
        paths-layfns (->>
                      (concat (for [[sense-id
                                     {:keys [ordinal dimensions]}] senses
                                     :let [path [:senses sense-id]]]
                                [ordinal
                                 [path (fn [left-px]
                                         (let [lay (lay/grid-layout
                                                    dimensions top-px
                                                    (+ spacer left-px)
                                                    d-opts true
                                                    display-mode)]
                                           [{path lay}
                                            (lay/right-px lay)]))]])
                              (for [[rgn-id rgn] regions
                                    [lyr-id {:keys [ordinal dimensions]}] rgn
                                    :let [path [:regions rgn-id lyr-id]]]
                                [ordinal
                                 [path (fn [left-px]
                                         (let [lay (lay/grid-layout
                                                    dimensions top-px
                                                    (+ spacer left-px)
                                                    d-opts false
                                                    display-mode)]
                                           [{path lay}
                                            (lay/right-px lay)]))]]))
                      (sort-by (fn [[ordinal _]]
                                 ordinal))
                      (map (fn [[_ path-layfn]]
                             path-layfn)))
        paths-layfns (->> insertions
                          (reduce (fn [plfns [after-path layfn]]
                                    (let [idx (if (nil? after-path)
                                                0
                                                (-> plfns
                                                    (index-of (fn [[path _]]
                                                                (= path
                                                                   after-path)))
                                                    inc))
                                          [before after] (split-at idx plfns)
                                          dummy [after-path :after]]
                                      (concat before [[dummy layfn]] after)))
                                  paths-layfns))]
    (stack-layouts (->> paths-layfns
                        (map (fn [[_ layfn]]
                               layfn))))))

(defn rebuild-layouts
  "Used when the model remains the same but the display has
  changed. Maintains any sorting, facets, and dt/scroll position on
  each layer/sense layout. I.e. replaces the GridLayout within each
  OrderableLayout."
  [viz-layouts network-shape opts insertions]
  (let [new-layouts (create-layouts network-shape opts insertions)
        sorted-layouts (reduce (fn [m path]
                                 (let [prev-lay (get-in viz-layouts path)
                                       scraps (select-keys (:layout prev-lay)
                                                           [:dt-offset
                                                            :scroll-top])]
                                   (update-in m path
                                            (fn [lay]
                                              (assoc prev-lay
                                                     :layout (merge lay
                                                                    scraps))))))
                               new-layouts
                               (grid-layout-paths new-layouts))]
    (reset-layout-caches sorted-layouts)))

(defn init-layouts
  [network-shape opts]
  (let [layouts (create-layouts network-shape opts nil)]
    (->
     (reduce (fn [m path]
               (update-in m path
                          (fn [lay]
                            (lay/orderable-layout lay (lay/ids-count lay)))))
             layouts (grid-layout-paths layouts))
     (reset-layout-caches))))

(defn update-dt-offsets!
  [viz-layouts sel-dt opts]
  (swap! viz-layouts
         (fn [m]
           (let [draw-steps (get-in opts [:drawing :draw-steps])
                 dt0 (max 0 (- sel-dt (quot draw-steps 2)))]
             (-> (reduce (fn [m path]
                           (update-in m path assoc-in [:layout :dt-offset] dt0))
                         m
                         (grid-layout-paths m))
                 (reset-layout-caches))))))

(def path->invalidate-path
  {:regions :columns
   :senses :inbits})

(defn invalidate!
  [viz-options paths]
  (swap! viz-options
         #(reduce (fn [m [t]]
                    (update-in m [(path->invalidate-path t) :refresh-index]
                               inc))
                  % paths)))

(defn scroll!
  [viz-layouts viz-options paths down?]
  (swap! viz-layouts
         (fn [m]
           (reduce (fn [m path]
                     (update-in m path lay/scroll down?))
                   m
                   paths)))
  (invalidate! viz-options paths))

(defn active-ids
  "Returns the set of active columns or bits for the sense/layer."
  [m-bits step path]
  (let [[lyr-type & _] path
        {:keys [snapshot-id]} step]
    (get-in m-bits [snapshot-id path (case lyr-type
                                    :regions :active-columns
                                    :senses :active-bits)])))

(defn add-facets!
  [viz-layouts viz-options paths step m-bits]
  (swap! viz-layouts
         (fn [m]
           (reduce (fn [m path]
                     (update-in m path
                                (fn [lay]
                                  (let [bits (active-ids m-bits step path)
                                        ;; record bits in current sort order
                                        ord-bits (->> (select-keys (:order lay) bits)
                                                      (sort-by second)
                                                      (map first))]
                                    (lay/add-facet lay ord-bits (:timestep step))))))
                   m
                   paths)))
  (invalidate! viz-options paths))

(defn clear-facets!
  [viz-layouts viz-options paths]
  (swap! viz-layouts
         (fn [m]
           (reduce (fn [m path]
                     (update-in m path lay/clear-facets))
                   m
                   paths)))
  (invalidate! viz-options paths))

(defn sort!
  [viz-layouts viz-options paths steps sel-dt m-bits]
  (let [use-steps (max 2 (get-in @viz-options [:drawing :draw-steps]))
        steps (->> steps (drop sel-dt) (take use-steps))]
    (swap! viz-layouts
           (fn [m]
             (reduce (fn [m path]
                       (update-in m path
                                  (fn [lay]
                                    (->> (map (partial active-ids m-bits)
                                              steps (repeat path))
                                         (lay/sort-by-recent-activity lay)))))
                     m
                     paths))))
  (invalidate! viz-options paths))

(defn clear-sort!
  [viz-layouts viz-options paths]
  (swap! viz-layouts
         (fn [m]
           (reduce (fn [m path]
                     (update-in m path lay/clear-sort))
                   m
                   paths)))
  (invalidate! viz-options paths))

(defn draw-ff-synapses
  [ctx p-syns p-syns-by-source steps r-lays s-lays sel opts]
  (c/save ctx)
  (c/stroke-width ctx 1)
  (c/alpha ctx 1)

  (let [do-perm? (get-in opts [:ff-synapses :permanences])
        draw-to (get-in opts [:ff-synapses :to])]
    ;; Selected layers: traceback
    (loop [seen-col-paths #{}
           rest-col-paths (for [{:keys [dt step path bit]} sel
                                :when (= (first path) :regions)
                                :let [{:keys [snapshot-id]} step
                                      [_ rgn-id lyr-id] path
                                      cols-with-data (keys (get-in p-syns
                                                                   [snapshot-id
                                                                    path]))
                                      cols (case draw-to
                                             :all (concat cols-with-data
                                                          (when bit
                                                            [bit]))
                                             :selected (when bit
                                                         [bit])
                                             :none nil)]
                                col cols]
                            [step path col])]
      (when (not-empty rest-col-paths)
        (let [[col-path & rest-col-paths*] rest-col-paths
              [step target-path target-col] col-path
              [_ target-rgn-id target-lyr-id] target-path
              {:keys [snapshot-id]} step
              syns-by-cell (get-in p-syns [snapshot-id target-path target-col])
              new-col-paths (distinct
                             (for [[_ syns-by-seg] syns-by-cell
                                   [_ syns-by-state] syns-by-seg
                                   :let [active-syns (:active syns-by-state)]
                                   {:keys [src-id src-lyr src-i]} active-syns
                                   :when src-lyr
                                   :let [new-path [:regions src-id src-lyr]
                                         src-col (quot
                                                  src-i
                                                  (get-in step
                                                          [:network-shape
                                                           :regions
                                                           target-rgn-id
                                                           target-lyr-id
                                                           :cells-per-column]))
                                         new-col-path [snapshot-id
                                                       new-path src-col]]
                                   :when (not (contains? seen-col-paths
                                                         new-col-path))]
                               new-col-path))
              target-lay (get-in r-lays [target-rgn-id target-lyr-id])
              dt (utilv/index-of steps #(= snapshot-id (:snapshot-id %)))
              [target-x target-y] (element-xy target-lay target-col dt)]
          (doseq [[_ syns-by-seg] syns-by-cell
                  [_ syns-by-state] syns-by-seg
                  [syn-state syns] syns-by-state
                  {:keys [src-id src-i src-lyr src-dt perm]} syns
                  :let [src-lay (if src-lyr
                                  (get-in r-lays [src-id src-lyr])
                                  (get s-lays src-id))
                        src-col (if src-lyr
                                  (quot src-i
                                        (get-in step [:network-shape :regions
                                                      src-id src-lyr
                                                      :cells-per-column]))
                                  src-i)
                        [src-x src-y] (element-xy src-lay src-col
                                                  (+ dt (- src-dt)))]]
            (doto ctx
              (c/stroke-style (syn-colors syn-state))
              (c/alpha (if do-perm? perm 1))
              (c/begin-path)
              (c/move-to (- target-x 1) target-y) ;; -1 avoid obscuring colour
              (c/line-to (+ src-x 1) src-y)
              (c/stroke)))
          (recur (into seen-col-paths new-col-paths)
                 (concat rest-col-paths* new-col-paths)))))
    ;; Selected senses
    (doseq [{:keys [dt step path bit]} sel
            :when (= (first path) :senses)
            :let [[_ sense-id] path
                  lay (get s-lays sense-id)
                  syns-by-bit (get-in p-syns-by-source [(:snapshot-id step)
                                                        path])
                  syns-by-bit (case draw-to
                                :all syns-by-bit
                                :selected (select-keys syns-by-bit [bit])
                                :none {})]
            [src-bit syns] syns-by-bit
            :let [[src-x src-y] (element-xy lay src-bit dt)]
            {:keys [target-id target-col target-lyr target-dt perm
                    syn-state]} syns
                    :let [target-lay (or (get s-lays target-id)
                                         (get-in r-lays
                                                 [target-id target-lyr]))
                          [target-x target-y] (element-xy
                                               target-lay target-col
                                               (+ dt target-dt))]]
      (doto ctx
        (c/stroke-style (syn-colors (keyword syn-state)))
        (c/alpha (if do-perm? perm 1))
        (c/begin-path)
        (c/move-to (- target-x 1) target-y) ;; -1 avoid obscuring colour
        (c/line-to (+ src-x 1) src-y)
        (c/stroke))))

  (c/restore ctx)
  ctx)

(defn natural-curve
  [ctx x0 y0 x1 y1]
  (let [x-third (/ (- x1 x0) 3)]
    (c/bezier-curve-to ctx
                       (- x1 x-third) y0
                       (+ x0 x-third) y1
                       x1 y1)))

(defprotocol PCellsSegmentsLayout
  (seg-xy [this ci si])
  (cell-xy [this ci])
  (col-cell-line [this ctx ci src-lay col dt])
  (cell-seg-line [this ctx ci si])
  (clicked-seg [this x y]))

(defn cells-segments-layout
  [segs-by-cell nsegbycell cells-left h-offset-px v-offset-px opts]
  (let [nsegbycell-pad (map (partial max 1) (vals (sort nsegbycell)))
        nseg-pad (apply + nsegbycell-pad)
        d-opts (:drawing opts)
        segs-left (+ cells-left h-offset-px)
        col-d-px (:col-d-px d-opts)
        col-r-px (* col-d-px 0.5)
        cell-r-px (:cell-r-px d-opts)
        seg-h-px (:seg-h-px d-opts)
        seg-w-px (:seg-w-px d-opts)
        longest-seg-w-px (apply max
                                0
                                (for [[_ segs] segs-by-cell
                                      [_ seg] segs
                                      :let [{:keys [n-conn-act n-conn-tot
                                                    n-disc-act n-disc-tot
                                                    stimulus-th]} seg
                                            scale-factor (/ seg-w-px stimulus-th)]]
                                  (* scale-factor
                                     (max n-conn-act n-conn-tot
                                          n-disc-act n-disc-tot
                                          stimulus-th))))
        cells-segs-w-px (max (int (+ h-offset-px longest-seg-w-px))
                             (:min-cells-segs-w-px d-opts))
        max-height-px (:max-height-px d-opts)
        our-top (+ (:top-px d-opts) cell-r-px (:cell-label-h-px d-opts))
        our-height (* nseg-pad 4 cell-r-px)]
    (reify
      lay/PBox
      (layout-bounds [_]
        {:x cells-left :y our-top
         :w cells-segs-w-px :h our-height})
      PCellsSegmentsLayout
      (seg-xy
        [_ ci si]
        (let [i-all (apply + si (take ci nsegbycell-pad))
              frac (/ i-all nseg-pad)]
          [segs-left
           (+ our-top (* frac our-height) v-offset-px)]))
      (cell-xy
        [this ci]
        (let [i-all (apply + (take ci nsegbycell-pad))
              frac (/ i-all nseg-pad)]
          [cells-left (+ our-top (* frac our-height))]))
      (col-cell-line
        [this ctx ci src-lay col dt]
        (let [[col-x col-y] (element-xy src-lay col dt)
              [cell-x cell-y] (cell-xy this ci)]
          (doto ctx
            (c/begin-path)
            (c/move-to (+ col-x col-r-px 1) col-y) ;; avoid obscuring colour
            (natural-curve col-x col-y cell-x cell-y)
            (c/stroke))))
      (cell-seg-line
        [this ctx ci si]
        (let [[cell-x cell-y] (cell-xy this ci)
              [sx sy] (seg-xy this ci si)]
          (doto ctx
            (c/begin-path)
            (c/move-to sx sy)
            (c/line-to (+ cell-x cell-r-px) cell-y)
            (c/stroke))))
      (clicked-seg
        [this x y]
        (when (<= (- cells-left cell-r-px) x
                  (+ segs-left seg-w-px 5))
          (first (for [[ci nsegs] nsegbycell
                       si (range nsegs)
                       :let [[_ seg-y] (seg-xy this ci si)]
                       :when (<= (- seg-y seg-h-px) y
                                 (+ seg-y seg-h-px 5))]
                   [ci si])))))))

(defn cells-segments-insertions
  [sel1 c-states d-segs a-segs opts]
  (let [{:keys [step path bit]} sel1
        {:keys [snapshot-id]} step
        {:keys [cells-per-column]} (-> step :network-shape (get-in path))
        d-segs-by-cell (get-in d-segs [snapshot-id path bit])
        a-segs-by-cell (get-in a-segs [snapshot-id path bit])]
    (when d-segs-by-cell
      {path (fn [left-px]
              (let [n-segs-by-cell (merge-with
                                    max
                                    (util/remap count d-segs-by-cell)
                                    (util/remap count a-segs-by-cell)
                                    (zipmap (range cells-per-column)
                                            (repeat 0)))
                    space-px (get-in opts
                                     [:drawing
                                      :seg-h-space-px])
                    seg-w-px (get-in opts
                                     [:drawing
                                      :seg-w-px])
                    cells-left (+ left-px space-px)
                    distal-lay (cells-segments-layout d-segs-by-cell
                                                      n-segs-by-cell
                                                      cells-left
                                                      space-px
                                                      0
                                                      opts)
                    apical-lay (when (first
                                      (for [[_ segs] a-segs-by-cell
                                            [_ seg] segs]
                                        seg))
                                 (cells-segments-layout a-segs-by-cell
                                                        n-segs-by-cell
                                                        cells-left
                                                        (+ space-px
                                                           seg-w-px
                                                           space-px)
                                                        -16
                                                        opts))]
                [(cond-> {[:cells-segments] distal-lay}
                   apical-lay (assoc [:apical-segments] apical-lay))
                 (cond-> (lay/right-px distal-lay)
                   apical-lay (max (lay/right-px apical-lay)))]))})))

(defn choose-selected-seg
  [segs-by-cell sel1 seg-type]
  (let [{:keys [path bit]} sel1
        [sel-ci sel-si] (get sel1 (case seg-type
                                    :apical :apical-seg
                                    :distal :distal-seg))]
    (if (and sel-ci sel-si)
      [sel-ci sel-si]
      (let [cands (for [[ci segs] segs-by-cell
                        [si seg] segs]
                    [[ci si] seg])]
        (if-let [learning (first (keep (fn [[ci-si seg]]
                                         (when (:learn-seg? seg)
                                           ci-si))
                                       cands))]
          learning
          (when-let [[most-active _] (when (not-empty cands)
                                       (apply max-key
                                              (fn [[_ seg]]
                                                (:n-conn-act seg))
                                              cands))]
            most-active))))))

(defn draw-cells-segments
  [ctx c-states d-segs d-syns a-segs a-syns steps sel1 layouts opts]
  (c/save ctx)
  (let [{path :path col :bit step :step} sel1
        {:keys [snapshot-id]} step
        {:keys [cells-per-column]} (-> step :network-shape (get-in path))
        dt (utilv/index-of steps (partial = step))
        [d-sel-ci d-sel-si] (choose-selected-seg (get-in d-segs
                                                         [snapshot-id path col])
                                                 sel1 :distal)
        [a-sel-ci a-sel-si] (choose-selected-seg (get-in a-segs
                                                         [snapshot-id path col])
                                                 sel1 :apical)]
    (when dt
      (let [d-segs-by-cell (get-in d-segs [snapshot-id path col])
            a-segs-by-cell (get-in a-segs [snapshot-id path col])
            cells-in-col (get-in c-states [snapshot-id path col])
            sel-lay (get-in layouts (:path sel1))
            col-d-px (get-in opts [:drawing :col-d-px])
            cell-r-px (get-in opts [:drawing :cell-r-px])
            seg-h-px (get-in opts [:drawing :seg-h-px])
            seg-w-px (get-in opts [:drawing :seg-w-px])
            draw-to (get-in opts [:distal-synapses :to])
            draw-perm? (get-in opts [:distal-synapses :permanences])
            seg-r-px (* seg-w-px 0.5)]
        ;; background pass
        (doseq [layout-key [:cells-segments :apical-segments]
                :let [cslay (get layouts layout-key)]
                :when cslay
                :let [[data-key segs-by-cell]
                      (case layout-key
                        :cells-segments [:distal d-segs-by-cell]
                        :apical-segments [:apical a-segs-by-cell])]
                ci (range cells-per-column)
                :let [segs (get segs-by-cell ci)
                      cell-active? (contains? (:active-cells cells-in-col)
                                              ci)]]
          ;; draw background lines to cell from column and from segments
          (doseq [[si seg] segs
                  :let [{:keys [n-conn-act stimulus-th]} seg]]
            (c/stroke-style ctx (:background state-colors))
            (c/stroke-width ctx col-d-px)
            (cell-seg-line cslay ctx ci si)
            (when (>= n-conn-act stimulus-th)
              (doto ctx
                (c/stroke-style (:active state-colors))
                (c/stroke-width 2))
              (cell-seg-line cslay ctx ci si)))
          ;; cell-specific stuff - don't duplicate (in apical case)
          (when (= data-key :distal)
            (c/stroke-style ctx (:background state-colors))
            (c/stroke-width ctx col-d-px)
            (col-cell-line cslay ctx ci sel-lay col dt)
            (when cell-active?
              (doto ctx
                (c/stroke-style (:active state-colors))
                (c/stroke-width 2))
              (col-cell-line cslay ctx ci sel-lay col dt))
            ))
        ;; foreground pass
        (doseq [layout-key [:cells-segments :apical-segments]
                :let [cslay (get layouts layout-key)]
                :when cslay
                :let [[data-key segs-by-cell syns-by-model]
                      (case layout-key
                        :cells-segments [:distal d-segs-by-cell d-syns]
                        :apical-segments [:apical a-segs-by-cell a-syns])]
                ci (range cells-per-column)
                :let [segs (get segs-by-cell ci)
                      [cell-x cell-y] (cell-xy cslay ci)
                      selected-cell? (or (= ci d-sel-ci)
                                         (= ci a-sel-ci))
                      winner-cell? (contains? (:winner-cells cells-in-col) ci)
                      active-cell? (contains? (:active-cells cells-in-col) ci)
                      predicted-cell? (contains? (:prior-predicted-cells
                                                  cells-in-col) ci)
                      cell-state (cond (and active-cell? predicted-cell?)
                                       :active-predicted

                                       active-cell? :active
                                       predicted-cell? :predicted
                                       :else :inactive)]]
          ;; draw the cell itself
          ;; don't duplicate (in apical case)
          (c/text-align ctx :start)
          (when (= data-key :distal)
            (when selected-cell?
              (doto ctx
                (c/fill-style (:highlight state-colors))
                (c/circle {:x cell-x :y cell-y :r (+ cell-r-px 8)})
                (c/fill)))
            (doto ctx
              (c/fill-style (state-colors cell-state))
              (c/stroke-style "black")
              (c/stroke-width 1)
              (c/circle {:x cell-x :y cell-y :r cell-r-px})
              (c/stroke)
              (c/fill))
            (c/fill-style ctx "black")
            (c/text ctx {:text (str "cell " ci
                                    (if winner-cell? " - winner" ""))
                         :x (+ cell-x 10) :y (- cell-y cell-r-px)}))
          (when (and (= data-key :apical)
                     (pos? (count segs)))
            (let [[sx sy] (seg-xy cslay ci 0)
                  h2 (int (/ seg-h-px 2))]
              (c/fill-style ctx "black")
              (c/text ctx {:text "apical"
                           :x sx :y (- sy h2 5)})))
          ;; draw segments
          (c/text-align ctx :right)
          (doseq [[si seg] segs
                  :let [[sx sy] (seg-xy cslay ci si)
                        {:keys [learn-seg? n-conn-act n-conn-tot
                                n-disc-act n-disc-tot
                                stimulus-th learning-th]} seg
                        [sel-ci sel-si] (if (= data-key :distal)
                                          [d-sel-ci d-sel-si]
                                          [a-sel-ci a-sel-si])
                        selected-seg? (and (= ci sel-ci)
                                           (= si sel-si))
                        scale-factor (/ seg-w-px stimulus-th)
                        scale #(-> % (* scale-factor) int)

                        h2 (int (/ seg-h-px 2))
                        conn-th-r {:x sx :y (- sy h2)
                                   :w seg-w-px :h seg-h-px}
                        conn-tot-r (assoc conn-th-r :w (scale n-conn-tot))
                        conn-act-r (assoc conn-th-r :w (scale n-conn-act))
                        disc-th-r {:x sx :y (+ sy h2)
                                   :w (scale learning-th) :h seg-h-px}
                        disc-tot-r (assoc disc-th-r :w (scale n-disc-tot))
                        disc-act-r (assoc disc-th-r :w (scale n-disc-act))]]
            ;; draw segment as a rectangle
            (when selected-seg?
              (doto ctx
                (c/fill-style (:highlight state-colors))
                (c/fill-rect {:x (- sx 5) :y (- sy h2 5)
                              :w (+ seg-w-px 5 5) :h (+ (* 2 seg-h-px) 5 5)})))
            (doto ctx
              (c/fill-style "white") ;; overlay on highlight rect
              (c/fill-rect conn-th-r)
              (c/fill-rect disc-th-r)
              (c/fill-style (:background state-colors))
              (c/fill-rect conn-tot-r)
              (c/fill-rect disc-tot-r)
              (c/stroke-style "black")
              (c/stroke-width 1)
              (c/fill-style (:active state-colors))
              (c/fill-rect conn-act-r)
              (c/stroke-rect conn-th-r)
              (c/alpha 0.5)
              (c/fill-rect disc-act-r)
              (c/stroke-rect disc-th-r)
              (c/alpha 1.0))
            (c/fill-style ctx "black")
            (c/text ctx {:text (str "seg " si "") :x (- sx 3) :y sy})
            (when learn-seg?
              (c/text ctx {:text (str "learning") :x (- sx 3) :y (+ sy 10)}))
            ;; draw distal synapses
            (c/stroke-width ctx 1)
            (when (or (and (= draw-to :selected)
                           selected-seg?)
                      (= draw-to :all))
              (doseq [[syn-state syns] (get-in syns-by-model [snapshot-id path
                                                              col ci si])]
                (c/stroke-style ctx (syn-colors syn-state))
                (doseq [{:keys [src-i src-id src-lyr src-dt perm]} syns
                        :let [src-lay (get-in layouts
                                              (if src-lyr
                                                [:regions src-id src-lyr]
                                                [:senses src-id]))
                              src-col (if src-lyr
                                        (quot src-i
                                              (get-in step [:network-shape
                                                            :regions
                                                            src-id src-lyr
                                                            :cells-per-column]))
                                        src-i)
                              [src-x src-y] (element-xy src-lay src-col
                                                        (+ dt (- src-dt)))]]
                  (when draw-perm?
                    (c/alpha ctx perm))
                  (doto ctx
                    (c/begin-path)
                    (c/move-to sx sy)
                    (c/line-to (+ src-x 1) src-y) ;; +1 avoid obscuring colour
                    (c/stroke)))))
            (c/alpha ctx 1.0)))
        (c/restore ctx))))
  ctx)

(defn image-buffer
  [{:keys [w h]}]
  (let [el (dom/createElement "canvas")]
    (set! (.-width el) w)
    (set! (.-height el) h)
    el))

(defn bg-image
  [lay]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")]
    (c/fill-style ctx (:background state-colors))
    (fill-elements lay ctx (lay/ids-onscreen lay))
    el))

(defn fill-ids-image
  [lay fill-style ids]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")]
    (c/fill-style ctx fill-style)
    (fill-elements lay ctx ids)
    el))

(defn fill-ids-alpha-image
  [lay fill-style id->alpha]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")]
    (c/fill-style ctx fill-style)
    (group-and-fill-elements lay ctx id->alpha c/alpha)
    el))

(defn break-image
  [lay]
  (let [el (image-buffer (layout-bounds lay))]
    (doto (c/get-context el "2d")
      (c/stroke-style "black")
      (c/stroke-width 1)
      (c/begin-path)
      (c/move-to 0.5 0)
      (c/line-to 0.5 (.-height el))
      (c/stroke))
    el))

(defn scroll-status-str
  [lay inbits?]
  (let [idx (lay/scroll-position lay)
        page-n (lay/ids-onscreen-count lay)
        n-ids (lay/ids-count lay)]
    (str page-n
         " of "
         n-ids
         (if inbits? " bits" " cols")
         (if (pos? idx)
           (str " @ "
                (long (* 100 (/ idx (- n-ids page-n))))
                "%")
           ""))))

(defn should-draw? [steps opts]
  (let [{:keys [anim-go? anim-every]} (:drawing opts)
        most-recent (first steps)]
    (when (and anim-go? most-recent)
      (let [t (:timestep most-recent)]
        (zero? (mod t anim-every))))))

(defn timeline-click
  [e click-dt steps selection]
  (.stopPropagation e)
  (let [append? (.-metaKey e)
        sel1 {:dt click-dt
              :step (nth steps click-dt)}]
    (if append?
      (let [same-dt? #(= click-dt (:dt %))]
        (if (some same-dt? @selection)
          (if (> (count @selection) 1)
            (swap! selection #(into (empty %) (remove same-dt? %)))
            (swap! selection sel/clear))
          (swap! selection conj (merge (peek @selection) sel1))))
      (swap! selection #(conj (empty %) (merge (peek @selection) sel1))))))

(defn viz-timeline [_ _ _]
  (let [size-invalidates-c (async/chan)
        container-width-px (atom 0)]
    (reagent/create-class
     {:component-did-mount
      (fn [component]
        (go-loop []
          (let [size-px (-> component reagent/dom-node style/getSize)]
            (reset! container-width-px (.-width size-px))
            (when (not (nil? (<! size-invalidates-c)))
              (recur)))))

      :reagent-render
      (fn [steps selection capture-options]
        (let [steps @steps
              sel @selection
              sel-dts (into #{} (map :dt sel))
              keep-steps (or (:keep-steps @capture-options)
                             50)
              min-t-width (* (cond-> 2
                               (pos? (count steps))
                               (max (count (str (:timestep (first steps))))))
                             12)
              t-width (max (/ @container-width-px keep-steps)
                           min-t-width)
              width-px (* t-width keep-steps)
              height-px 28
              y-px (/ height-px 2)
              ry-max y-px
              rx (* t-width 0.5)
              ry (min ry-max rx)
              sel-rx (max rx ry-max)
              sel-ry ry-max
              dt-render-order (concat (->> (range keep-steps)
                                           (remove sel-dts))
                                      sel-dts)]
          [:div {:style {:width "100%"
                         :direction "rtl"
                         :cursor "default"
                         :overflow-x "auto"
                         :overflow-y "hidden"}}
           [window-resize-listener size-invalidates-c]
           (into [:svg {:width width-px
                        :height height-px}]
                 (for [dt dt-render-order
                       :let [kept? (< dt (count steps))
                             sel? (and kept? (contains? sel-dts dt))
                             x-px (- (dec width-px) rx (* dt t-width))]]
                   [:g (cond-> {:text-anchor "middle"
                                :font-family "sans-serif"
                                :font-weight "bold"
                                :font-size "11px"}
                         kept?
                         (assoc :on-click
                                #(timeline-click % dt steps selection)))
                    [:ellipse {:cx x-px :cy y-px
                               :rx (if sel? sel-rx rx)
                               :ry (if sel? sel-ry ry)
                               :fill "black"
                               :fill-opacity (cond sel? 1.0 kept? 0.5 :else 0.1)}]
                    (when kept?
                      [:text {:x x-px :y y-px
                              :dy "0.35em"
                              :fill "white"}
                       (str (:timestep (nth steps dt)))])]))]))})))

(defn draw-viz!
  [ctx steps s-caches m-bits p-syns p-syns-by-source c-states d-segs d-syns
   a-segs a-syns layouts sel opts]
  (let [d-opts (:drawing opts)

        draw-steps (case (:display-mode d-opts)
                     :one-d (:draw-steps d-opts)
                     :two-d 1)

        center-dt (:dt (peek sel))
        draw-dts (if (== 1 draw-steps)
                   [center-dt]
                   ;; in case scrolled back in history
                   (let [dt0 (max 0 (- center-dt (quot draw-steps 2)))]
                     (range dt0 (min (+ dt0 draw-steps)
                                     (count steps)))))]
    (c/clear-rect ctx {:x 0 :y 0
                       :w (.-width (.-canvas ctx))
                       :h (.-height (.-canvas ctx))})

    (doseq [dt draw-dts
            :let [{:keys [snapshot-id]} (nth steps dt)
                  sc (get s-caches snapshot-id)
                  path->bits (get m-bits snapshot-id)]]
      ;; draw encoded inbits
      (doseq [[_ sense-id :as path] (sense-layout-paths layouts)
              :let [{:keys [active-bits
                            pred-bits-alpha]} (get path->bits path)
                    lay (get-in layouts path)
                    lay-cache (::cache (meta lay))]]
        (->> (bg-image lay)
             (with-cache lay-cache [::bg sense-id] opts #{:drawing})
             (draw-image-dt ctx lay dt))
        (when active-bits
          (->> active-bits
               (fill-ids-image lay (:active state-colors))
               (with-cache sc [::abits sense-id] opts #{:inbits :drawing})
               (draw-image-dt ctx lay dt)))
        (when pred-bits-alpha
          (->> pred-bits-alpha
               (fill-ids-alpha-image lay (:predicted state-colors))
               (with-cache sc [::pbits sense-id] opts #{:inbits :drawing})
               (draw-image-dt ctx lay dt))))
      ;; draw regions / layers
      (doseq [[_ lyr-id rgn-id :as path] (layer-layout-paths layouts)
              :let [{:keys [active-columns
                            pred-columns
                            overlaps-columns-alpha
                            boost-columns-alpha
                            active-freq-columns-alpha
                            n-segments-columns-alpha
                            break?]} (get path->bits path)
                    uniqix (str (name rgn-id) (name lyr-id))
                    lay (get-in layouts path)
                    lay-cache (::cache (meta lay))]
              :when lay]
        (->> (bg-image lay)
             (with-cache lay-cache [::bg uniqix] opts #{:drawing})
             (draw-image-dt ctx lay dt))
        (when overlaps-columns-alpha
          (->> overlaps-columns-alpha
               (fill-ids-alpha-image lay "black")
               (with-cache sc [::ocols uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when boost-columns-alpha
          (->> boost-columns-alpha
               (fill-ids-alpha-image lay "black")
               (with-cache sc [::boosts uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when active-freq-columns-alpha
          (->> active-freq-columns-alpha
               (fill-ids-alpha-image lay "black")
               (with-cache sc [::afreq uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when n-segments-columns-alpha
          (->> n-segments-columns-alpha
               (fill-ids-alpha-image lay "black")
               (with-cache sc [::nsegcols uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when (and active-columns
                   (get-in opts [:columns :active]))
          (->> active-columns
               (fill-ids-image lay (:active state-colors))
               (with-cache sc [::acols uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when (and pred-columns
                   (get-in opts [:columns :predictive]))
          (->> pred-columns
               (fill-ids-image lay (:predicted state-colors))
               (with-cache sc [::pcols uniqix] opts #{:columns :drawing})
               (draw-image-dt ctx lay dt)))
        (when (and break? (= :one-d (:display-mode d-opts)))
          (->> (break-image lay)
               (draw-image-dt ctx lay dt)))))

    ;; mark facets
    (doseq [lay (grid-layout-vals layouts)]
      (lay/draw-facets lay ctx))
    ;; highlight selection
    (when (and (> draw-steps 1)
               (= 1 (count sel)))
      (doseq [dt (map :dt sel)]
        (doseq [lay (grid-layout-vals layouts)]
          (lay/highlight-dt lay ctx dt (:highlight state-colors)))))
    (doseq [{:keys [path bit dt]} sel
            :when path
            :let [lay (get-in layouts path)]]
      (lay/highlight-layer lay ctx (:highlight state-colors))
      (when bit
        (lay/highlight-element lay ctx dt bit bit (:highlight state-colors))))

    ;; draw ff synapses
    (draw-ff-synapses ctx p-syns p-syns-by-source steps (:regions layouts)
                      (:senses layouts) sel opts)

    (when-let [cslay (:cells-segments layouts)]
      ;; draw selected cells and segments
      (draw-cells-segments ctx c-states d-segs d-syns a-segs a-syns steps
                           (peek sel) layouts opts))))

(def code-key
  {32 :space
   33 :page-up
   34 :page-down
   37 :left
   38 :up
   39 :right
   40 :down
   191 :slash
   220 :backslash
   187 :equals
   45 :minus
   173 :minus
   189 :minus
   })

(defn viz-key-down
  [e commands-in]
  (if-let [k (code-key (.-keyCode e))]
    (do
      (put! commands-in
            (case k
              :left [:step-backward]
              :right [:step-forward]
              :up [:bit-up]
              :down [:bit-down]
              :page-up [:scroll-up]
              :page-down [:scroll-down]
              :space [:toggle-run]
              :slash [:sort (.-shiftKey e)]
              :backslash [:clear-sort (.-shiftKey e)]
              :equals [:add-facet (.-shiftKey e)]
              :minus [:clear-facets (.-shiftKey e)]
              ))
      (.preventDefault e))
    true))

(defn viz-click
  [e steps selection layouts]
  (.stopPropagation e)
  (let [{:keys [x y]} (offset-from-target e)
        append? (.-metaKey e)
        s-lays (:senses layouts)
        r-lays (:regions layouts)
        max-dt (max 0 (dec (count steps)))
        hit? (atom false)]
    ;; check senses and regions
    (doseq [path (grid-layout-paths layouts)
            :let [lay (get-in layouts path)
                  [dt id] (lay/clicked-id lay x y)]
            :when dt
            :let [dt (if (== 1 (count (p/dims-of lay)))
                       (min dt max-dt)
                       (:dt (peek @selection)))
                  sel1 {:path path :bit id :dt dt :step (nth steps dt nil)}]]
      (reset! hit? true)
      (if append?
        (let [same-bit? #(and (= dt (:dt %))
                              (= id (:bit %))
                              (= path (:path %)))]
          (if (some same-bit? @selection)
            (if (> (count @selection) 1)
              (swap! selection #(into (empty %) (remove same-bit? %)))
              (swap! selection sel/clear))
            (swap! selection conj sel1)))
        (swap! selection #(conj (empty %) sel1))))
    ;; check cells
    (when ((every-pred sel/layer #(pos? (:bit %))) (peek @selection))
      (doseq [[layout-key sel-key] [[:cells-segments :distal-seg]
                                    [:apical-segments :apical-seg]]
              :let [cslay (get layouts layout-key)]
              :when cslay]
        (when-let [[ci si] (clicked-seg cslay x y)]
          (reset! hit? true)
          (swap! selection #(conj (pop %)
                                  (assoc (peek %)
                                         sel-key [ci si]))))))
    (when-not (or append? @hit?)
      ;; checked all, nothing clicked
      (swap! selection sel/clear))))

(defn on-onscreen-bits-changed!
  [cached-onscreen-bits layouts]
  (assert (not-empty layouts))
  (doseq [[path big-value] @cached-onscreen-bits]
    (marshal/release! big-value))
  (reset! cached-onscreen-bits
          (into {}
                (for [path (grid-layout-paths layouts)]
                  [path (marshal/big-value
                         (into #{}
                               (lay/ids-onscreen (get-in layouts path))))]))))

(defn absorb-network-shape
  [network-shape viz-options viz-layouts cached-onscreen-bits]
  (reset! viz-layouts
          (init-layouts network-shape @viz-options))
  (on-onscreen-bits-changed! cached-onscreen-bits @viz-layouts))

(defn fetch-ff-syns-by-source!
  [into-journal proximal-syns-by-source m-bits sel opts]
  (doseq [{:keys [step path dt bit] :as sel1} sel
          :when (= :senses (first path))
          :let [{:keys [snapshot-id]} step
                [_ sense-id] path
                src-bits (case (get-in opts [:ff-synapses :to])
                           :all (concat (get-in m-bits
                                                [snapshot-id path :active-bits])
                                        (when bit
                                          [bit]))
                           :selected (when bit
                                       [bit])
                           :none [])]
          src-bit src-bits
          :let [response-c (async/chan)
                {do-disconn? :disconnected do-inactive? :inactive
                 do-growing? :growing} (:ff-synapses opts)
                syn-states (into #{"active" "active-predicted" "predicted"}
                                 (concat (when do-disconn? ["disconnected"])
                                         (when do-inactive? ["inactive"])
                                         (when do-growing? ["growing"])))]]
    (put! into-journal ["get-proximal-synapses-by-source-bit" snapshot-id
                        sense-id src-bit syn-states
                        (marshal/channel response-c true)])
    (go
      (let [syns (<! response-c)]
        (swap! proximal-syns-by-source assoc-in [snapshot-id path src-bit]
               (keywordize-keys syns))))))

;; keywordize-keys is slow if the data contains sequences of bits, since it has
;; to crawl them all to check for maps.
(defn keywordize1 [m] (into {} (for [[k v] m] [(keyword k) v])))

(defn fetch-model-bits!
  [into-journal model-bits steps opts path->onscreen-bits]
  (doseq [:let [sense-fetches (into #{} (remove nil?)
                                    [;; active bits needed even if not displayed
                                     "active-bits"
                                     (when (get-in opts [:inbits :predicted])
                                       "pred-bits-alpha")])
                layer-fetches (into #{} (remove nil?)
                                    [;; active cols needed even if not displayed
                                     "active-columns"
                                     (when (get-in opts [:columns :predictive])
                                       "pred-columns")
                                     (when (get-in opts [:columns :overlaps])
                                       "overlaps-columns-alpha")
                                     (when (get-in opts [:columns :boosts])
                                       "boost-columns-alpha")
                                     (when (get-in opts [:columns :active-freq])
                                       "active-freq-columns-alpha")
                                     (when (get-in opts [:columns :n-segments])
                                       "n-segments-columns-alpha")])]
          step steps
          :let [{:keys [senses regions]} (:network-shape step)
                sense-ids (keys senses)
                rgn-lyr-ids (for [[rgn-id rgn] regions
                                  lyr-id (keys rgn)]
                              [rgn-id lyr-id])
                snapshot-id (:snapshot-id step)]]
    (doseq [sense-id sense-ids
            :let [path [:senses sense-id]
                  onscreen-bits-marshal (path->onscreen-bits path)
                  response-c (async/chan)]]
      (put! into-journal ["get-sense-bits" snapshot-id sense-id sense-fetches
                          onscreen-bits-marshal
                          (marshal/channel response-c true)])
      (go
        (let [fetched-bits (<! response-c)]
          (swap! model-bits assoc-in [snapshot-id path]
                 (keywordize1 fetched-bits)))))
    (doseq [[rgn-id lyr-id] rgn-lyr-ids
            :let [path [:regions rgn-id lyr-id]
                  onscreen-bits-marshal (path->onscreen-bits path)
                  response-c (async/chan)]]
      (put! into-journal ["get-layer-bits" snapshot-id rgn-id lyr-id
                          layer-fetches onscreen-bits-marshal
                          (marshal/channel response-c true)])
      (go
        (let [fetched-bits (<! response-c)]
          (swap! model-bits assoc-in [snapshot-id path]
                 (keywordize1 fetched-bits)))))))

;; `seen-cols` is an atom shared between the parallel recursive go blocks
;; snapshot-id -> path -> #{}
(defn fetch-segs-traceback!
  [into-journal segs-atom syns-atom seen-cols cols-to-fetch seg-type syn-states
   segs-decider trace-back?]
  (doseq [[step cols-by-path] cols-to-fetch
          [path cols] cols-by-path
          :let [seen (get-in @seen-cols [step path])
                cols (remove #(contains? seen %) cols)]
          :when (not-empty cols)
          :let [[_ rgn-id lyr-id] path
                {:keys [snapshot-id]} step
                response-c (async/chan)]]
    (swap! seen-cols update-in [step path]
           (fn [seen]
             (reduce conj (or seen #{}) cols)))
    (put! into-journal
          [(case seg-type
             :apical "get-apical-segments"
             :distal "get-distal-segments"
             :proximal "get-proximal-segments")
           snapshot-id rgn-id lyr-id cols (marshal/channel response-c true)])
    (go
      (let [segs-by-col (keywordize-keys (<! response-c))
            _ (swap! segs-atom update-in [snapshot-id path] merge segs-by-col)
            stf-seq (for [[col segs-by-cell] segs-by-col
                          [ci segs] (segs-decider segs-by-cell)
                          [si seg] segs]
                      [col ci si])
            syns-to-fetch (reduce (fn [stf [col ci si]]
                                    (update-in stf [col ci] conj si))
                                  {} stf-seq)
            response-c (async/chan)]
        (put! into-journal
              [(case seg-type
                 :apical "get-apical-synapses"
                 :distal "get-distal-synapses"
                 :proximal "get-proximal-synapses")
               snapshot-id rgn-id lyr-id syns-to-fetch syn-states
               (marshal/channel response-c true)])
        (let [syns-by-col (keywordize-keys (<! response-c))]
          (swap! syns-atom update-in [snapshot-id path] merge syns-by-col)
          (when trace-back?
            (let [ctf-seq (for [[col syns-by-cell] syns-by-col
                                [ci syns-by-seg] syns-by-cell
                                [si syns-by-state] syns-by-seg
                                :let [syns (:active syns-by-state)]
                                {:keys [src-id src-lyr src-i]} syns
                                :when src-lyr
                                :let [path [:regions src-id src-lyr]
                                      src-col (quot
                                               src-i
                                               (get-in step
                                                       [:network-shape :regions
                                                        src-id src-lyr
                                                        :cells-per-column]))]]
                            [step path src-col])
                  new-ctf (reduce (fn [ctf [step path col]]
                                    (update-in ctf [step path]
                                               conj col))
                                  {} ctf-seq)]
              (fetch-segs-traceback! into-journal segs-atom syns-atom
                                     seen-cols new-ctf seg-type
                                     syn-states segs-decider
                                     trace-back?))))))))

(defn fetch-segs!
  [into-journal segs-atom syns-atom m-bits sel opts seg-type]
  (let [{draw-to :to
         get-inactive? :inactive
         get-disconnected? :disconnected
         get-growing? :growing
         trace-back? :trace-back?} (get opts (case seg-type
                                               :apical :distal-synapses
                                               :distal :distal-synapses
                                               :proximal :ff-synapses))
        syn-states (cond-> #{"active"}
                     get-inactive? (conj "inactive")
                     get-disconnected? (conj "disconnected")
                     get-growing? (conj "growing"))
        ctf-seq (for [{:keys [step bit path]} sel
                      :when (= (first path) :regions)
                      :let [{:keys [snapshot-id]} step
                            cols (case seg-type
                                   :apical (when bit
                                             [bit])
                                   :distal (when bit
                                             [bit])
                                   :proximal
                                   (case draw-to
                                     :all (concat
                                           (get-in m-bits
                                                   [snapshot-id path
                                                    :active-columns])
                                           (when bit
                                             [bit]))
                                     :selected (when bit
                                                 [bit])
                                     :none nil))]]
                  [step path cols])
        cols-to-fetch (reduce (fn [ctf [step path cols]]
                                (assoc-in ctf [step path] cols))
                              {} ctf-seq)
        segs-decider (cond (or (= seg-type :proximal)
                               (and (or (= seg-type :apical)
                                        (= seg-type :distal))
                                    (= draw-to :all)))
                           identity

                           (and (or (= seg-type :apical)
                                    (= seg-type :distal))
                                (= draw-to :selected))
                           (fn [segs-by-cell]
                             (when-let [[ci si] (choose-selected-seg
                                                 segs-by-cell
                                                 (peek sel)
                                                 seg-type)]
                               {ci {si (get-in segs-by-cell [ci si])}}))

                           (= draw-to :none)
                           (fn [_]
                             nil))]
    (fetch-segs-traceback! into-journal segs-atom syns-atom (atom {})
                           cols-to-fetch seg-type syn-states segs-decider
                           trace-back?)))

(defn fetch-cells!
  [into-journal cell-states snapshot-id path col]
  (let [response-c (async/chan)
        [_ rgn-id lyr-id] path
        fetches #{"active-cells" "prior-predicted-cells" "winner-cells"}]
    (put! into-journal
          ["get-column-cells" snapshot-id rgn-id lyr-id col fetches
           (marshal/channel response-c true)])
    (go
      (let [column-cells (<! response-c)]
        (swap! cell-states assoc-in [snapshot-id path col]
               (keywordize-keys column-cells))))))

(defn absorb-new-steps!
  [steps-v step-caches model-bits into-journal opts
   path->onscreen-bits]
  ;; Assumption: all new steps are at the beginning
  (let [new-steps (loop [xs (seq steps-v)
                         new-steps []]
                    (if-let [step (first xs)]
                      (if-not (contains? @step-caches (:snapshot-id step))
                        (recur (next xs)
                               (conj new-steps step))
                        new-steps)
                      new-steps))]
    (swap! step-caches into (for [step new-steps]
                              [(:snapshot-id step) (atom {})]))
    (fetch-model-bits! into-journal model-bits new-steps opts
                       path->onscreen-bits)))

(defn ids-onscreen-changed?
  [before after]
  (let [;; TODO should also consider height
        extractor (juxt lay/scroll-position :order)]
    (not= (map extractor (grid-layout-vals before))
          (map extractor (grid-layout-vals after)))))

(defn viz-canvas
  [_ steps selection network-shape viz-options into-viz into-sim into-journal]
  (let [;; ## Fetched remote data
        ;; snapshot-id -> path-vector -> {:active-columns [] :active-cells [] ...}
        model-bits (atom {})
        ;; snapshot-id -> path-vector -> col -> data
        cell-states (atom {})
        ;; snapshot-id -> path-vector -> col -> cell-i -> segs
        apical-segs (atom {})
        distal-segs (atom {})
        proximal-segs (atom {})
        ;; snapshot-id -> path-vector -> col -> cell-i -> seg-i -> syns-by-state
        apical-syns (atom {})
        distal-syns (atom {})
        proximal-syns (atom {})
        ;; snapshot-id -> path-vector -> bit -> syns
        proximal-syns-by-source (atom {})

        ;; ## Saved local data
        ;; path-vector -> BigValueMarshal
        cached-onscreen-bits (atom nil)
        ;; path -> layout
        ;; (use `get-in m path-vector`, not `get m path-vector`)
        viz-layouts (atom nil)
        ;; snapshot-id -> cache-atom
        step-caches (atom {})

        into-viz (or into-viz (async/chan))
        teardown-c (async/chan)]
    (go-loop []
      (when-let [[command & xs] (alt! teardown-c nil
                                      into-viz ([v] v)
                                      :priority true)]
        (case command
          :drop-steps-data (let [[dropped] xs
                                 ids (map :snapshot-id dropped)]
                             (doseq [m-atom [model-bits step-caches]]
                               (apply swap! m-atom dissoc ids)))
          :background-clicked (swap! selection sel/clear)
          :sort (let [[apply-to-all?] xs]
                  (sort! viz-layouts viz-options
                         (if apply-to-all?
                           (grid-layout-paths @viz-layouts)
                           (map :path @selection))
                         @steps
                         (:dt (peek @selection))
                         @model-bits))
          :clear-sort (let [[apply-to-all?] xs]
                        (clear-sort! viz-layouts viz-options
                                     (if apply-to-all?
                                       (grid-layout-paths @viz-layouts)
                                       (map :path @selection))))
          :add-facet (let [[apply-to-all?] xs]
                       (add-facets! viz-layouts viz-options
                                    (if apply-to-all?
                                      (grid-layout-paths @viz-layouts)
                                      (map :path @selection))
                                    (nth @steps
                                         (:dt (peek @selection)))
                                    @model-bits))
          :clear-facets (let [[apply-to-all?] xs]
                          (clear-facets! viz-layouts viz-options
                                         (if apply-to-all?
                                           (grid-layout-paths @viz-layouts)
                                           (map :path @selection))))
          :step-backward (let [max-dt (max 0 (dec (count @steps)))]
                           (swap! selection
                                  #(conj (pop %)
                                         (let [sel1 (peek %)
                                               dt (min (inc (:dt sel1)) max-dt)]
                                           (assoc sel1
                                                  :dt dt
                                                  :step (nth @steps dt))))))
          :step-forward (if (some #(zero? (:dt %)) @selection)
                          (when (and @network-shape into-sim)
                            (put! into-sim ["step"]))
                          (swap! selection
                                 #(conj (pop %)
                                        (let [sel1 (peek %)
                                              dt (dec (:dt (peek %)))]
                                          (assoc sel1
                                                 :dt dt
                                                 :step (nth @steps dt))))))
          :bit-up (let [{:keys [path bit]} (peek @selection)]
                    (when bit
                      (let [lay (get-in @viz-layouts path)
                            order (:order lay)
                            idx (order bit)
                            next-bit (if (zero? idx)
                                       nil
                                       (let [next-idx (dec idx)]
                                         (key (first (subseq order >= next-idx
                                                             <= next-idx)))))]
                        (swap! selection #(conj (pop %)
                                                (assoc (peek %)
                                                       :bit next-bit
                                                       :distal-seg nil
                                                       :apical-seg nil))))))
          :bit-down (let [{:keys [path bit]} (peek @selection)
                          lay (get-in @viz-layouts path)
                          order (:order lay)
                          next-idx (if bit
                                     (inc (order bit))
                                     0)
                          next-bit (if (< next-idx (lay/ids-count lay))
                                     (key (first (subseq order >= next-idx <=
                                                         next-idx)))
                                     nil)]
                      (swap! selection #(conj (pop %)
                                              (assoc (peek %)
                                                     :bit next-bit
                                                     :distal-seg nil
                                                     :apical-seg nil))))
          :scroll-down (let [[apply-to-all?] xs]
                         (scroll! viz-layouts viz-options
                                  (if apply-to-all?
                                    (grid-layout-paths @viz-layouts)
                                    (map :path @selection))
                                  true))
          :scroll-up (let [[apply-to-all?] xs]
                       (scroll! viz-layouts viz-options
                                (if apply-to-all?
                                  (grid-layout-paths @viz-layouts)
                                  (map :path @selection))
                                false))
          :toggle-run (when (and @network-shape into-sim)
                        (put! into-sim ["toggle"])))
        (recur)))

    (reagent/create-class
     {:component-will-mount
      (fn [_]
        (when @network-shape
          (absorb-network-shape @network-shape viz-options viz-layouts
                                cached-onscreen-bits)
          (when (not-empty @steps)
            (absorb-new-steps! @steps step-caches model-bits into-journal
                               @viz-options @cached-onscreen-bits)))

        (add-watch steps ::init-caches-and-request-data
                   (fn init-caches-and-request-data [_ _ _ xs]
                     (absorb-new-steps! xs step-caches model-bits
                                        into-journal @viz-options
                                        @cached-onscreen-bits)))
        (add-watch viz-layouts ::onscreen-bits
                   (fn onscreen-bits<-layouts [_ _ prev layouts]
                     (when (and prev
                                (ids-onscreen-changed? prev layouts))
                       (on-onscreen-bits-changed! cached-onscreen-bits
                                                  layouts))))
        (add-watch cached-onscreen-bits ::fetch-bits
                   (fn on-onscreen-bits-change [_ _ _ cob]
                     (fetch-model-bits! into-journal model-bits @steps
                                        @viz-options cob)))
        (add-watch network-shape ::absorb-network-shape
                   (fn network-shape-changed [_ _ _ template]
                     (absorb-network-shape template viz-options viz-layouts
                                           cached-onscreen-bits)))
        (add-watch viz-options ::rebuild-layouts
                   (fn layouts<-viz-options [_ _ old-opts opts]
                     (when (not= (:drawing opts)
                                 (:drawing old-opts))
                       (when-let [st @network-shape]
                         (swap! viz-layouts rebuild-layouts st opts
                                (cells-segments-insertions
                                 (peek @selection) @cell-states @distal-segs
                                 @apical-segs opts))))))
        (add-watch selection ::update-dt-offsets
                   (fn dt-offsets<-selection [_ _ old-sel sel]
                     (let [dt-sel (:dt (peek sel))]
                       (when (not= dt-sel (:dt (peek old-sel)))
                         (update-dt-offsets! viz-layouts dt-sel @viz-options)))))
        (add-watch selection ::fetch-selection-change
                   (fn fetch-selection-change [_ _ old-sel sel]
                     (let [opts @viz-options]
                       ;; Proximal synapses with selection as target
                       (when-not (->> sel
                                      (every?
                                       (fn [{:keys [step path bit]}]
                                         (or (= :senses (first path))
                                             (get-in @proximal-syns
                                                     [(:snapshot-id step) path
                                                      bit])))))
                         (swap! proximal-segs empty)
                         (swap! proximal-syns empty)
                         (fetch-segs! into-journal proximal-segs proximal-syns
                                      @model-bits sel opts :proximal))
                       ;; Proximal synapses with selection as source
                       (when-not (->> sel
                                      (every?
                                       (fn [{:keys [snapshot-id path bit]}]
                                         (or (= :regions (first path))
                                             (get-in @proximal-syns-by-source
                                                     [snapshot-id path bit])))))
                         (swap! proximal-syns-by-source empty)
                         (fetch-ff-syns-by-source! into-journal
                                                   proximal-syns-by-source
                                                   @model-bits sel opts))
                       (let [{:keys [step path bit] :as sel1} (peek sel)
                             {:keys [snapshot-id]} step
                             old-sel1 (peek old-sel)]
                         ;; Cells
                         (when-not (get-in @cell-states [snapshot-id path bit])
                           (swap! cell-states empty)
                           (when (and (= (first path) :regions)
                                      bit)
                             (fetch-cells! into-journal cell-states snapshot-id
                                           path bit)))
                         ;; Distal segments / synapses
                         (when (or (not (get-in @distal-segs
                                                [snapshot-id path bit]))
                                   (not= (:distal-seg sel1)
                                         (:distal-seg old-sel1)))
                           (when (or bit (:bit old-sel1))
                             (swap! distal-segs empty)
                             (swap! distal-syns empty))
                           (when bit
                             (fetch-segs! into-journal distal-segs distal-syns
                                          @model-bits [sel1] opts :distal)))
                         ;; Apical segments / synapses
                         (when (or (not (get-in @apical-segs
                                                [snapshot-id path bit]))
                                   (not= (:apical-seg sel1)
                                         (:apical-seg old-sel1)))
                           (when (or bit (:bit old-sel1))
                             (swap! apical-segs empty)
                             (swap! distal-syns empty))
                           (when bit
                             (fetch-segs! into-journal apical-segs apical-syns
                                          @model-bits [sel1] opts :apical)))))))
        (add-watch viz-options ::fetches
                   (fn [_ _ old-opts opts]
                     (when (or (not= (:inbits old-opts)
                                     (:inbits opts))
                               (not= (:columns old-opts)
                                     (:columns opts)))
                       (fetch-model-bits! into-journal model-bits @steps
                                          opts @cached-onscreen-bits))
                     (when (not= (:ff-synapses old-opts)
                                 (:ff-synapses opts))
                       (swap! proximal-segs empty)
                       (swap! proximal-syns empty)
                       (fetch-segs! into-journal proximal-segs proximal-syns
                                    @model-bits @selection opts :proximal))
                     (when (not= (:distal-synapses old-opts)
                                 (:distal-synapses opts))
                       (swap! distal-segs empty)
                       (swap! distal-syns empty)
                       (fetch-segs! into-journal distal-segs distal-syns
                                    @model-bits @selection opts :distal))
                     (when (not= (:apical-synapses old-opts)
                                 (:apical-synapses opts))
                       (swap! apical-segs empty)
                       (swap! apical-syns empty)
                       (fetch-segs! into-journal apical-segs apical-syns
                                    @model-bits @selection opts :apical))))
        (add-watch model-bits ::synapses-waiting-on-active-bits
                   (fn [_ _ old-m-bits m-bits]
                     (when (= :all (get-in @viz-options [:ff-synapses :to]))
                       (doseq [{:keys [snapshot-id path]} @selection
                               :when (not (contains?
                                           (get-in old-m-bits [snapshot-id
                                                               path])
                                           :active-columns))]
                         (when (and (= :regions (first path))
                                    (not (contains?
                                          (get-in old-m-bits [snapshot-id path])
                                          :active-columns))
                                    (contains? (get-in m-bits [snapshot-id
                                                               path])
                                               :active-columns))
                           (fetch-segs! into-journal proximal-segs proximal-syns
                                        m-bits @selection @viz-options
                                        :proximal))
                         (when (and (= :senses (first path))
                                    (not (contains? (get-in old-m-bits
                                                            [snapshot-id path])
                                                    :active-bits))
                                    (contains? (get-in m-bits [snapshot-id
                                                               path])
                                               :active-bits))
                           (fetch-ff-syns-by-source! into-journal
                                                     proximal-syns-by-source
                                                     m-bits @selection
                                                     @viz-options))))))
        (add-watch distal-segs ::cells-segments-layout
                   (fn [_ _ _ d-s]
                     (let [opts @viz-options]
                       (swap! viz-layouts rebuild-layouts @network-shape opts
                              (cells-segments-insertions
                               (peek @selection) @cell-states d-s @apical-segs
                               opts)))))

        (add-watch apical-segs ::cells-segments-layout
                   (fn [_ _ _ ap-s]
                     (let [opts @viz-options]
                       (swap! viz-layouts rebuild-layouts @network-shape opts
                              (cells-segments-insertions
                               (peek @selection) @cell-states @distal-segs ap-s
                               opts))))))

      :component-will-unmount
      (fn [_]
        (remove-watch steps ::init-caches-and-request-data)
        (remove-watch network-shape ::absorb-network-shape)
        (remove-watch viz-options ::rebuild-layouts)
        (remove-watch selection ::update-dt-offsets)
        (remove-watch selection ::fetch-selection-change)
        (remove-watch viz-options ::fetches)
        (async/close! teardown-c))

      :display-name "viz-canvas"

      :reagent-render
      (fn [props _ _ _ _ _ _ _]
        (let [layouts @viz-layouts
              [right bottom] (->> (all-layout-paths layouts)
                                  (map (partial get-in layouts))
                                  (map layout-bounds)
                                  (map (fn [{:keys [x y w h]}]
                                         [(+ x w) (+ y h)]))
                                  (reduce (fn [result r-and-b]
                                            (map max result r-and-b))
                                          [0 0]))
              d-opts (:drawing @viz-options)
              max-width (:max-width-px d-opts)
              text-px (:h-space-for-text-px d-opts)
              width (or (when right
                          (cond-> (+ right lay/extra-px-for-highlight)
                            max-width (min max-width)))
                        0)
              height (or (when bottom
                           (+ bottom lay/extra-px-for-highlight))
                         0)]
          [:div
           [:div {:style {:height 20
                          :font "9px sans-serif"
                          :font-weight "bold"
                          :position "relative"}}
            (when (not-empty @steps)
              (into [:div
                     (when-let [cslay (:cells-segments layouts)]
                       (let [cell-r (:cell-r-px d-opts)
                             {:keys [x w]} (layout-bounds cslay)]
                         [:div {:style {:position "absolute"
                                        :left (- x cell-r)
                                        :width (+ w cell-r)
                                        :top 0}}
                          "cells and distal / apical dendrite segments"]))]
                    (for [path (grid-layout-paths layouts)
                          :let [lay (get-in layouts path)
                                {:keys [x w]} (layout-bounds lay)
                                ids (subvec path 1)
                                sense? (= (first path) :senses)]]
                      [:div {:style {:position "absolute"
                                     :left x
                                     :width (+ w text-px)
                                     :top 0}}
                       (->> ids (map name) (interpose " ") (apply str))
                       [:br]
                       (scroll-status-str lay sense?)])))]
           ^{:key "main-canvas"}
           [canvas
            (assoc props
                   :on-click #(viz-click % @steps selection
                                         @viz-layouts))
            width height
            [selection steps model-bits proximal-syns proximal-syns-by-source
             cell-states distal-segs distal-syns apical-segs apical-syns
             viz-layouts viz-options]
            (fn [ctx]
              (let [opts @viz-options]
                (when (should-draw? @steps opts)
                  (draw-viz! ctx @steps @step-caches @model-bits @proximal-syns
                             @proximal-syns-by-source @cell-states @distal-segs
                             @distal-syns @apical-segs @apical-syns @viz-layouts
                             @selection opts))))]]))})))

(defn inbits-display [dims state->bits d-opts]
  (let [d-opts (assoc d-opts :draw-steps 1)
        lay (lay/grid-layout dims 0 0 d-opts true (:display-mode d-opts))
        {:keys [x y w h]} (layout-bounds lay)]
    [canvas nil w h [dims state->bits]
     (fn [ctx]
       (c/clear-rect ctx {:x 0 :y 0 :w w :h h})
       (c/fill-style ctx (:background state-colors))
       (fill-elements lay ctx (lay/ids-onscreen lay))
       (doseq [[state bits] state->bits]
         (c/fill-style ctx (get state-colors state))
         (fill-elements lay ctx bits)))]))
