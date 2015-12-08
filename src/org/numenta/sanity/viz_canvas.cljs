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
            [cljs.core.async :as async :refer [<! put!]])
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
   :inactive-syn "black"
   :growing (hsl :green 1.0 0.5)
   :disconnected (hsl :red 1.0 0.5)
   :active (hsl :red 1.0 0.5)
   :predicted (hsl :blue 1.0 0.5 0.5)
   :active-predicted (hsl :purple 1.0 0.4)
   :highlight (hsl :yellow 1 0.65 0.6)
   :temporal-pooling (hsl :green 1 0.5 0.4)
   })

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
             :temporal-pooling true
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
  [step-template opts insertions]
  (let [senses (:senses step-template)
        regions (:regions step-template)
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
  [viz-layouts step-template opts insertions]
  (let [new-layouts (create-layouts step-template opts insertions)
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
  [step-template opts]
  (let [layouts (create-layouts step-template opts nil)]
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
  [step path]
  (let [[lyr-type & _] path]
    (-> (get-in step path)
        (get (case lyr-type
               :regions :active-columns
               :senses :active-bits)))))

(defn add-facets!
  [viz-layouts viz-options paths step]
  (swap! viz-layouts
         (fn [m]
           (reduce (fn [m path]
                     (update-in m path
                                (fn [lay]
                                  (lay/add-facet lay
                                                 (sort (active-ids step path))
                                                 (:timestep step)))))
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
  [viz-layouts viz-options paths steps sel-dt]
  (let [use-steps (max 2 (get-in @viz-options [:drawing :draw-steps]))
        steps (->> steps (drop sel-dt) (take use-steps))]
    (swap! viz-layouts
           (fn [m]
             (reduce (fn [m path]
                       (update-in m path
                                  (fn [lay]
                                    (->> (map active-ids steps (repeat path))
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
           rest-col-paths (for [{:keys [dt model-id path bit]} sel
                                :when (= (first path) :regions)
                                :let [[_ rgn-id lyr-id] path
                                      cols-with-data (keys (get-in p-syns
                                                                   [model-id
                                                                    path]))
                                      cols (case draw-to
                                             :all (concat cols-with-data
                                                          (when bit
                                                            [bit]))
                                             :selected (when bit
                                                         [bit])
                                             :none nil)]
                                col cols]
                            [model-id path col])]
      (when (not-empty rest-col-paths)
        (let [[col-path & rest-col-paths*] rest-col-paths
              syns-by-cell (get-in p-syns col-path)
              [target-model-id [_ target-rgn-id target-lyr-id]
               target-col] col-path
              new-col-paths (distinct
                             (for [[_ syns-by-seg] syns-by-cell
                                   [_ syns-by-state] syns-by-seg
                                   :let [active-syns (:active syns-by-state)]
                                   {:keys [src-id src-lyr src-col]} active-syns
                                   :when src-lyr
                                   :let [new-path [:regions (keyword src-id)
                                                   (keyword src-lyr)]
                                         new-col-path [target-model-id new-path
                                                       src-col]]
                                   :when (not (contains? seen-col-paths
                                                         new-col-path))]
                               new-col-path))
              target-lay (get-in r-lays [(keyword target-rgn-id)
                                         (keyword target-lyr-id)])
              dt (utilv/index-of steps #(= target-model-id (:model-id %)))
              [target-x target-y] (element-xy target-lay target-col dt)]
          (doseq [[_ syns-by-seg] syns-by-cell
                  [_ syns-by-state] syns-by-seg
                  [syn-state syns] syns-by-state
                  {:keys [src-id src-col src-lyr src-dt perm]} syns
                  :let [src-lay (or (get s-lays (keyword src-id))
                                    (get-in r-lays [(keyword src-id)
                                                    (keyword src-lyr)]))
                        [src-x src-y] (element-xy src-lay src-col
                                                  (+ dt src-dt))]]
            (doto ctx
              (c/stroke-style (state-colors syn-state))
              (c/alpha (if do-perm? perm 1))
              (c/begin-path)
              (c/move-to (- target-x 1) target-y) ;; -1 avoid obscuring colour
              (c/line-to (+ src-x 1) src-y)
              (c/stroke)))
          (recur (into seen-col-paths new-col-paths)
                 (concat rest-col-paths* new-col-paths)))))
    ;; Selected senses
    (doseq [{:keys [dt model-id path bit]} sel
            :when (= (first path) :senses)
            :let [[_ sense-id] path
                  lay (get s-lays sense-id)
                  syns-by-bit (get-in p-syns-by-source [model-id path])
                  syns-by-bit (case draw-to
                                :all syns-by-bit
                                :selected (select-keys syns-by-bit [bit])
                                :none {})]
            [src-bit syns] syns-by-bit
            :let [[src-x src-y] (element-xy lay src-bit dt)]
            {:keys [target-id target-col target-lyr target-dt perm
                    syn-state]} syns
                    :let [target-lay (or (get s-lays (keyword target-id))
                                         (get-in r-lays
                                                 [(keyword target-id)
                                                  (keyword target-lyr)]))
                          [target-x target-y] (element-xy
                                               target-lay target-col
                                               (+ dt target-dt))]]
      (doto ctx
        (c/stroke-style (state-colors (keyword syn-state)))
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
        our-height (cond-> (* nseg-pad (* 8 cell-r-px))
                     max-height-px (min (- max-height-px our-top)))]
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
  (let [{:keys [model-id path bit]} sel1
        cells-per-column (get-in c-states [model-id path bit :cells-per-column])
        d-segs-by-cell (get-in d-segs [model-id path bit])
        a-segs-by-cell (get-in a-segs [model-id path bit])]
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
  (let [{:keys [model-id path bit]} sel1
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
  (let [{path :path col :bit model-id :model-id} sel1
        dt (utilv/index-of steps #(= model-id (:model-id %)))
        [d-sel-ci d-sel-si] (choose-selected-seg (get-in d-segs
                                                         [model-id path col])
                                                 sel1 :distal)
        [a-sel-ci a-sel-si] (choose-selected-seg (get-in a-segs
                                                         [model-id path col])
                                                 sel1 :apical)]
    (when dt
      (let [d-segs-by-cell (get-in d-segs [model-id path col])
            a-segs-by-cell (get-in a-segs [model-id path col])
            cells-in-col (get-in c-states [model-id path col])
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
                ci (range (:cells-per-column cells-in-col))
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
                ci (range (:cells-per-column cells-in-col))
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
              (doseq [[syn-state syns] (get-in syns-by-model [model-id path col
                                                              ci si])]
                (c/stroke-style ctx (state-colors syn-state))
                (doseq [{:keys [src-col src-id src-lyr src-dt perm]} syns
                        :let [src-lay (get-in layouts
                                              (if src-lyr
                                                [:regions (keyword src-id)
                                                 (keyword src-lyr)]
                                                [:senses (keyword src-id)]))
                              [src-x src-y] (element-xy src-lay src-col
                                                        (+ dt src-dt))]]
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
              :model-id (:model-id (nth steps click-dt))}]
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
      (fn [viz-steps selection capture-options]
        (let [steps @viz-steps
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
                    (when (and (pos? (count steps))
                               (or sel?
                                   (and kept? (< keep-steps 100))))
                      [:text {:x x-px :y y-px
                              :dy "0.35em"
                              :fill "white"}
                       (str (:timestep (nth steps dt)))])]))]))})))

(defn draw-viz!
  [ctx viz-steps p-syns p-syns-by-source c-states d-segs d-syns a-segs a-syns
   layouts sel opts]
  (let [s-lays (:senses layouts)
        r-lays (:regions layouts)

        d-opts (:drawing opts)

        draw-steps (case (:display-mode d-opts)
                     :one-d (:draw-steps d-opts)
                     :two-d 1)

        center-dt (:dt (peek sel))
        draw-dts (if (== 1 draw-steps)
                   [center-dt]
                   ;; in case scrolled back in history
                   (let [dt0 (max 0 (- center-dt (quot draw-steps 2)))]
                     (range dt0 (min (+ dt0 draw-steps)
                                     (count viz-steps)))))]
    (c/clear-rect ctx {:x 0 :y 0
                       :w (.-width (.-canvas ctx))
                       :h (.-height (.-canvas ctx))})

    (doseq [dt draw-dts
            :let [{sc :cache inbits-cols :inbits-cols
                   senses :senses regions :regions
                   :as step} (nth viz-steps dt)]]
      ;; draw encoded inbits
      (doseq [[sense-id {:keys [active-bits]}] senses
              :let [{:keys [pred-bits-alpha]} (get-in inbits-cols [:senses
                                                                   sense-id])
                    lay (s-lays sense-id)
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
      (doseq [[rgn-id rgn-data] regions]
        (doseq [[lyr-id {:keys [active-columns
                                pred-columns]}] rgn-data
                :let [{:keys [overlaps-columns-alpha
                              boost-columns-alpha
                              active-freq-columns-alpha
                              n-segments-columns-alpha
                              tp-columns
                              break?]} (get-in inbits-cols
                                               [:regions rgn-id lyr-id])
                      uniqix (str (name rgn-id) (name lyr-id))
                      lay (get-in r-lays [rgn-id lyr-id])
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
          (when tp-columns
            (->> tp-columns
                 (fill-ids-image lay (:temporal-pooling state-colors))
                 (with-cache sc [::tpcols uniqix] opts #{:columns :drawing})
                 (draw-image-dt ctx lay dt)))
          (when (and break? (= :one-d (:display-mode d-opts)))
            (->> (break-image lay)
                 (draw-image-dt ctx lay dt))))))

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
    (draw-ff-synapses ctx p-syns p-syns-by-source viz-steps r-lays s-lays sel
                      opts)

    (when-let [cslay (:cells-segments layouts)]
      ;; draw selected cells and segments
      (draw-cells-segments ctx c-states d-segs d-syns a-segs a-syns viz-steps
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
                  sel1 {:path path :bit id :dt dt
                        :model-id (:model-id (nth steps dt nil))}]]
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
  (let [path->ids-onscreen (into
                            {}
                            (concat
                             (for [[_ rgn-id lyr-id
                                    :as path] (layer-layout-paths layouts)]
                               [[rgn-id lyr-id]
                                (lay/ids-onscreen (get-in layouts path))])
                             (for [[_ sense-id
                                    :as path] (sense-layout-paths layouts)]
                               [sense-id
                                (lay/ids-onscreen (get-in layouts path))])))]
    (swap! cached-onscreen-bits
           (fn [ob]
             (when ob
               (marshal/release! ob))
             (marshal/big-value path->ids-onscreen)))))

(defn absorb-step-template
  [step-template viz-options viz-layouts cached-onscreen-bits]
  (reset! viz-layouts
          (init-layouts step-template @viz-options))
  (on-onscreen-bits-changed! cached-onscreen-bits @viz-layouts))

(defn fetch-ff-syns-by-source!
  [into-journal proximal-syns-by-source steps sel opts]
  (doseq [{:keys [model-id path dt bit] :as sel1} sel
          :when (= :senses (first path))
          :let [[_ sense-id] path
                src-bits (case (get-in opts [:ff-synapses :to])
                           :all (concat (get-in (nth steps dt)
                                                [:senses sense-id :active-bits])
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
                                         (when do-inactive? ["inactive-syn"])
                                         (when do-growing? ["growing"])))]]
    (put! into-journal ["get-proximal-synapses-by-source-bit" model-id
                        (name sense-id) src-bit syn-states
                        (marshal/channel response-c true)])
    (go
      (let [syns (<! response-c)]
        (swap! proximal-syns-by-source assoc-in [model-id path src-bit] syns)))))

(defn fetch-inbits-cols!
  [into-journal steps-data steps opts c-onscreen-bits]
  (let [fetches (into #{} (remove nil?)
                      [(when (get-in opts [:inbits :predicted])
                         "pred-bits-alpha")
                       (when (get-in opts [:columns :overlaps])
                         "overlaps-columns-alpha")
                       (when (get-in opts [:columns :boosts])
                         "boost-columns-alpha")
                       (when (get-in opts [:columns :active-freq])
                         "active-freq-columns-alpha")
                       (when (get-in opts [:columns :n-segments])
                         "n-segments-columns-alpha")
                       (when (get-in opts [:columns :temporal-pooling])
                         "tp-columns")])]
   (doseq [step steps
           :let [model-id (:model-id step)
                 response-c (async/chan)]]
     (put! into-journal ["get-inbits-cols" model-id fetches c-onscreen-bits
                         (marshal/channel response-c true)])
     (go
       (swap! steps-data assoc-in [step :inbits-cols] (<! response-c))))))

;; `seen-col-paths` is an atom shared between the parallel recursive go blocks
(defn fetch-segs-traceback!
  [into-journal segs-atom syns-atom seen-col-paths col-paths seg-type syn-states
   segs-decider trace-back?]
  (doseq [col-path col-paths
          :when (not (contains? @seen-col-paths col-path))
          :let [[model-id [_ rgn-id lyr-id] col] col-path
                response-c (async/chan)]]
    (swap! seen-col-paths conj col-path)
    (put! into-journal
          [(case seg-type
             :apical "get-column-apical-segments"
             :distal "get-column-distal-segments"
             :proximal "get-column-proximal-segments")
           model-id (name rgn-id) (name lyr-id) col
           (marshal/channel response-c true)])
    (go
      (let [segs-by-cell (<! response-c)]
        (swap! segs-atom assoc-in col-path segs-by-cell)
        (doseq [[ci segs] (segs-decider segs-by-cell)
                [si seg] segs
                :let [response2-c (async/chan)]]
          (put! into-journal
                [(case seg-type
                   :apical "get-apical-segment-synapses"
                   :distal "get-distal-segment-synapses"
                   :proximal "get-proximal-segment-synapses")
                 model-id (name rgn-id) (name lyr-id) col ci si syn-states
                 (marshal/channel response-c true)])
          (go
            (let [syns-by-state (<! response-c)]
              (swap! syns-atom assoc-in (into col-path [ci si])
                     syns-by-state)
              (when trace-back?
                (let [syns (:active syns-by-state)
                      new-cps (for [{:keys [src-id src-lyr src-col]} syns
                                    ;; continue tracing till we reach a sense
                                    :when src-lyr]
                                [model-id [:regions (keyword src-id)
                                           (keyword src-lyr)] src-col])]
                  (fetch-segs-traceback! into-journal segs-atom syns-atom
                                         seen-col-paths new-cps seg-type
                                         syn-states segs-decider
                                         trace-back?))))))))))

(defn fetch-segs!
  [into-journal segs-atom syns-atom steps sel opts seg-type]
  (let [{draw-to :to
         get-inactive? :inactive
         get-disconnected? :disconnected
         get-growing? :growing
         trace-back? :trace-back?} (get opts (case seg-type
                                               :apical :distal-synapses
                                               :distal :distal-synapses
                                               :proximal :ff-synapses))
        syn-states (cond-> #{"active"}
                     get-inactive? (conj "inactive-syn")
                     get-disconnected? (conj "disconnected")
                     get-growing? (conj "growing"))
        col-paths (for [{:keys [model-id bit path dt]} sel
                        :when (= (first path) :regions)
                        :let [[_ rgn-id lyr-id] path
                              cols (case seg-type
                                     :apical (when bit
                                               [bit])
                                     :distal (when bit
                                               [bit])
                                     :proximal
                                     (case draw-to
                                       :all (concat
                                             (get-in (nth steps dt)
                                                     [:regions rgn-id lyr-id
                                                      :active-columns])
                                             (when bit
                                               [bit]))
                                       :selected (when bit
                                                   [bit])
                                       :none nil))]
                        col cols]
                    [model-id path col])
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
    (fetch-segs-traceback! into-journal segs-atom syns-atom (atom #{}) col-paths
                           seg-type syn-states segs-decider trace-back?)))

;; A "viz-step" is a step with viz-canvas-specific data added.
(defn make-viz-step
  [step steps-data]
  (merge step (get steps-data step)))

(defn make-viz-steps
  [steps steps-data]
  (map make-viz-step steps (repeat steps-data)))

(defn absorb-new-steps!
  [steps-v steps-data into-journal opts c-onscreen-bits]
  (let [new-steps (->> steps-v
                       (remove (partial contains?
                                        @steps-data)))]
    (swap! steps-data
           #(into (select-keys % steps-v) ;; remove old steps
                  (for [step new-steps] ;; insert new caches
                    [step {:cache (atom {})}])))
    (fetch-inbits-cols! into-journal steps-data new-steps opts
                        c-onscreen-bits)))

(defn ids-onscreen-changed?
  [before after]
  (let [;; TODO should also consider height
        extractor (juxt lay/scroll-position :order)]
    (not= (map extractor (grid-layout-vals before))
          (map extractor (grid-layout-vals after)))))

(defn viz-canvas
  [_ steps selection step-template viz-options into-viz into-sim into-journal]
  (let [steps-data (atom {})

        ;; model-id -> path -> col -> data
        cell-states (atom {})
        ;; model-id -> path -> col -> cell-index -> segs
        apical-segs (atom {})
        distal-segs (atom {})
        proximal-segs (atom {})
        ;; model-id -> path -> col -> cell-index -> seg-index -> syns-by-state
        apical-syns (atom {})
        distal-syns (atom {})
        proximal-syns (atom {})
        ;; model-id -> path -> bit -> syns
        proximal-syns-by-source (atom {})

        viz-layouts (atom nil)
        cached-onscreen-bits (atom nil)
        into-viz (or into-viz (async/chan))
        teardown-c (async/chan)]
    (go-loop []
      (when-let [[command & xs] (alt! teardown-c nil
                                      into-viz ([v] v)
                                      :priority true)]
        (case command
          :background-clicked (swap! selection sel/clear)
          :sort (let [[apply-to-all?] xs]
                  (sort! viz-layouts viz-options
                         (if apply-to-all?
                           (grid-layout-paths @viz-layouts)
                           (map :path @selection))
                         @steps
                         (:dt (peek @selection))))
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
                                         (:dt (peek @selection)))))
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
                                                  :model-id (:model-id
                                                             (nth @steps dt)))))))
          :step-forward (if (some #(zero? (:dt %)) @selection)
                          (when (and @step-template into-sim)
                            (put! into-sim ["step"]))
                          (swap! selection
                                 #(conj (pop %)
                                        (let [sel1 (peek %)
                                              dt (dec (:dt (peek %)))]
                                          (assoc sel1
                                                 :dt dt
                                                 :model-id (:model-id
                                                            (nth @steps dt)))))))
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
          :toggle-run (when (and @step-template into-sim)
                        (put! into-sim ["toggle"])))
        (recur)))

    (reagent/create-class
     {:component-will-mount
      (fn [_]
        (when @step-template
          (absorb-step-template @step-template viz-options viz-layouts
                                cached-onscreen-bits)
          (when (not-empty @steps)
            (absorb-new-steps! @steps steps-data into-journal @viz-options
                               @cached-onscreen-bits)))

        (add-watch steps ::init-caches-and-request-data
                   (fn init-caches-and-request-data [_ _ _ xs]
                     (absorb-new-steps! xs steps-data into-journal @viz-options
                                        @cached-onscreen-bits)))
        (add-watch viz-layouts ::onscreen-bits
                   (fn onscreen-bits<-layouts [_ _ prev layouts]
                     (when (and prev
                                (ids-onscreen-changed? prev layouts))
                       (on-onscreen-bits-changed! cached-onscreen-bits
                                                  layouts))))
        (add-watch cached-onscreen-bits ::fetch-bits
                   (fn on-onscreen-bits-change [_ _ _ cob]
                     (fetch-inbits-cols! into-journal steps-data @steps
                                         @viz-options cob)))
        (add-watch step-template ::absorb-step-template
                   (fn step-template-changed [_ _ _ template]
                     (absorb-step-template template viz-options viz-layouts
                                           cached-onscreen-bits)))
        (add-watch viz-options ::rebuild-layouts
                   (fn layouts<-viz-options [_ _ old-opts opts]
                     (when (not= (:drawing opts)
                                 (:drawing old-opts))
                       (when-let [st @step-template]
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
                     (let [opts @viz-options
                           {:keys [bit model-id path cell
                                   distal-seg apical-seg]
                            :as sel1} (peek sel)
                           old-sel1 (peek old-sel)]
                       ;; Proximal synapses: check all of selection
                       (when-not (->> sel
                                      (every?
                                       (fn [{:keys [model-id path bit]}]
                                         (or (= :senses (first path))
                                             (get-in @proximal-syns
                                                     [model-id path bit])))))
                         (swap! proximal-segs empty)
                         (swap! proximal-syns empty)
                         (fetch-segs! into-journal proximal-segs proximal-syns
                                      @steps sel opts :proximal))
                       (when-not (->> sel
                                      (every?
                                       (fn [{:keys [model-id path bit]}]
                                         (or (= :regions (first path))
                                             (get-in @proximal-syns-by-source
                                                     [model-id path bit])))))
                         (swap! proximal-syns-by-source empty)
                         (fetch-ff-syns-by-source! into-journal
                                                   proximal-syns-by-source
                                                   steps sel opts))
                       ;; Cells and distal/apical synapses: just check sel1
                       (when-not (get-in @cell-states [model-id path bit])
                         (swap! cell-states empty)
                         (when (and (= (first path) :regions)
                                    bit)
                           (let [response-c (async/chan)
                                 [_ rgn-id lyr-id] path]
                             (put! into-journal
                                   ["get-column-cells" model-id (name rgn-id)
                                    (name lyr-id) bit
                                    (marshal/channel response-c true)])
                             (go
                               (let [{:keys [cells-per-column winner-cells
                                             active-cells
                                             prior-predicted-cells]}
                                     (<! response-c)]
                                 (swap! cell-states assoc-in [model-id path bit]
                                        {:active-cells active-cells
                                         :prior-predicted-cells prior-predicted-cells
                                         :winner-cells winner-cells
                                         :cells-per-column cells-per-column}))))))
                       (when (or (not (get-in @distal-segs [model-id path bit]))
                                 (not= distal-seg (:distal-seg old-sel1)))
                         (swap! distal-segs empty)
                         (swap! distal-syns empty)
                         (fetch-segs! into-journal distal-segs distal-syns
                                      @steps [sel1] opts :distal))
                       (when (or (not (get-in @apical-segs [model-id path bit]))
                                 (not= apical-seg (:apical-seg old-sel1)))
                         (swap! apical-segs empty)
                         (swap! distal-syns empty)
                         (fetch-segs! into-journal apical-segs apical-syns
                                      @steps [sel1] opts :apical)))))
        (add-watch viz-options ::fetches
                   (fn [_ _ old-opts opts]
                     (when (or (not= (:inbits old-opts)
                                     (:inbits opts))
                               (not= (:columns old-opts)
                                     (:columns opts)))
                       (fetch-inbits-cols! into-journal steps-data @steps opts
                                           @cached-onscreen-bits))
                     (when (not= (:ff-synapses old-opts)
                                 (:ff-synapses opts))
                       (swap! proximal-segs empty)
                       (swap! proximal-syns empty)
                       (fetch-segs! into-journal proximal-segs proximal-syns
                                    @steps @selection opts :proximal))
                     (when (not= (:distal-synapses old-opts)
                                 (:distal-synapses opts))
                       (swap! distal-segs empty)
                       (swap! distal-syns empty)
                       (fetch-segs! into-journal distal-segs distal-syns @steps
                                    @selection opts :distal))
                     (when (not= (:apical-synapses old-opts)
                                 (:apical-synapses opts))
                       (swap! apical-segs empty)
                       (swap! apical-syns empty)
                       (fetch-segs! into-journal apical-segs apical-syns @steps
                                    @selection opts :apical))))
        (add-watch distal-segs ::cells-segments-layout
                   (fn [_ _ _ d-s]
                     (let [opts @viz-options]
                       (swap! viz-layouts rebuild-layouts @step-template opts
                              (cells-segments-insertions
                               (peek @selection) @cell-states d-s @apical-segs
                               opts)))))

        (add-watch apical-segs ::cells-segments-layout
                   (fn [_ _ _ ap-s]
                     (let [opts @viz-options]
                       (swap! viz-layouts rebuild-layouts @step-template opts
                              (cells-segments-insertions
                               (peek @selection) @cell-states @distal-segs ap-s
                               opts))))))

      :component-will-unmount
      (fn [_]
        (remove-watch steps ::init-caches-and-request-data)
        (remove-watch step-template ::absorb-step-template)
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
            [selection steps steps-data proximal-syns proximal-syns-by-source
             cell-states distal-segs distal-syns apical-segs apical-syns
             viz-layouts viz-options]
            (fn [ctx]
              (let [viz-steps (make-viz-steps @steps @steps-data)
                    opts @viz-options]
                (when (should-draw? viz-steps opts)
                  (draw-viz! ctx viz-steps @proximal-syns
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
