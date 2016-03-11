(ns org.numenta.sanity.plots
  (:require [reagent.core :as reagent :refer [atom]]
            [monet.canvas :as c]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.plots-canvas :as plt]
            [org.numenta.sanity.helpers :refer [canvas resizing-canvas
                                          window-resize-listener]]
            [org.numenta.sanity.selection :as sel]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util :refer [round]]
            [tailrecursion.priority-map :refer [priority-map]]
            [clojure.string :as str]
            [goog.dom :as dom]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defprotocol PCompressible
  (compress [this factor]))

(defprotocol PBucketed
  (buckets [this])
  (bucket-size [this]))

(defprotocol PCapped
  (max-count [this]))

(deftype SequenceCompressor [bucket-size* fcompress xs unfilled-bucket]
  PBucketed
  (bucket-size [_]
    bucket-size*)
  (buckets [_]
    (vec xs))

  PCompressible
  (compress [_ factor]
    (SequenceCompressor. (* factor bucket-size*) fcompress
                         (->> xs
                              (partition factor)
                              (mapv (partial apply fcompress)))
                         unfilled-bucket))

  ICollection
  (-conj [_ x]
    (let [bucket (conj unfilled-bucket x)]
      (if (< (count bucket) bucket-size*)
        (SequenceCompressor. bucket-size* fcompress xs bucket)
        (SequenceCompressor. bucket-size* fcompress
                             (conj xs (apply fcompress bucket)) (empty xs)))))

  ICounted
  (-count [_]
    (count xs))

  ISeqable
  (-seq [_]
    (seq xs)))

(deftype SequenceCompressorCapped [max-bucket-count fcompress seq-compressor]
  PBucketed
  (bucket-size [_]
    (bucket-size seq-compressor))
  (buckets [_]
    (buckets seq-compressor))

  PCompressible
  (compress [_ factor]
    (SequenceCompressorCapped. max-bucket-count fcompress
                               (compress seq-compressor factor)))

  PCapped
  (max-count [_]
    max-bucket-count)

  ICollection
  (-conj [_ x]
    (let [r (SequenceCompressorCapped. max-bucket-count fcompress
                                       (conj seq-compressor x))]
      (if (< (count r) max-bucket-count)
        r
        (compress r 2))))

  ICounted
  (-count [_]
    (count seq-compressor))

  ISeqable
  (-seq [_]
    (seq seq-compressor)))

(defn sequence-compressor
  "A sequence builder that does not necessarily grow on `conj`.
  Individual items are the result of compressing buckets of values
  into a single value. Compress the sequence or check its bucket size
  via methods. Specify a `max-bucket-count` to make it automatically
  recompress as it grows.

  Extract the compressed sequence via `seq` or `buckets`.

  `fcompress` must take an arbitrary number of args and must return a
  value of the same format, i.e. a value that will later be used as a
  fcompress arg."
  ([fcompress]
   (SequenceCompressor. 1 fcompress [] []))
  ([max-bucket-count fcompress]
   (SequenceCompressorCapped. max-bucket-count fcompress
                              (sequence-compressor fcompress))))

(defn mean
  [xs]
  (/ (apply + xs) (count xs)))

(defn aggregate-by
  [f maps]
  (let [ks (keys (first maps))]
    (->>
     (for [k ks]
       [k (f (map #(get % k) maps))])
     (into {}))))

(defn stacked-ts-plot
  [ctx col-state-freqs-log series-keys series-colors]
  (let [bucket-size (bucket-size col-state-freqs-log)
        buckets (buckets col-state-freqs-log)
        n-timesteps (* bucket-size (max-count col-state-freqs-log))
        ncol (:size (peek buckets))
        v-max (* ncol 0.06)
        cnv (.-canvas ctx)
        plot-size {:w (- (.-width cnv) 25)
                   :h (- (.-height cnv) 18)}
        plot (plt/xy-plot ctx plot-size
                          [0 n-timesteps]
                          [v-max 0])
        ]

    (c/clear-rect ctx {:x 0 :y 0 :w (.-width cnv) :h (.-height cnv)})
    (c/stroke-width ctx 0)
    (doseq [[i x] (plt/indexed buckets)]
      (reduce (fn [from-y k]
                (let [val (get x k)]
                  (c/fill-style ctx (series-colors k))
                  (plt/rect! plot (* i bucket-size) from-y
                             bucket-size val)
                  (+ from-y val)))
              0 series-keys))
    (plt/frame! plot)
    (c/fill-style ctx "black")
    (c/stroke-style ctx "black")
    (c/stroke-width ctx 1)
    ;; draw x labels
    (c/text-baseline ctx :top)
    (doseq [x (range 0 (inc n-timesteps)
                     (/ n-timesteps 8))
            :let [[xpx ypx] (plt/->px plot x 0)]]
      (doto ctx
        (c/begin-path)
        (c/move-to xpx ypx)
        (c/line-to xpx (+ ypx 5))
        (c/stroke))
      (c/text ctx {:x xpx
                   :y (+ ypx 5)
                   :text x}))
    ;; draw y labels
    (c/text-baseline ctx :middle)
    (let [labx n-timesteps]
      (doseq [f [0 0.02 0.04]
              :let [y (* ncol f)
                    [xpx ypx] (plt/->px plot labx y)]]
        (doto ctx
          (c/begin-path)
          (c/move-to xpx ypx)
          (c/line-to (+ xpx 5) ypx)
          (c/stroke))
        (c/text ctx {:x (+ xpx 10)
                     :y ypx
                     :text y})))
    ))

(defn empty-col-state-freqs-log
  []
  (sequence-compressor 200
                       (fn [& col-state-freqs-seq]
                         (aggregate-by mean col-state-freqs-seq))))

(defn ts-freqs-plot-cmp
  [_ _]
  (let [size-invalidates-c (async/chan)]
    (fn [col-state-freqs-log series-colors]
      [:div nil
       [window-resize-listener size-invalidates-c]
       [resizing-canvas
        {:style {:width "100%"
                 :height 180}}
        []
        (fn [ctx]
          (stacked-ts-plot ctx col-state-freqs-log
                           [:active :active-predicted :predicted]
                           series-colors))
        size-invalidates-c]])))

(def excitation-colors
  {:proximal-unstable :active
   :proximal-stable :active-predicted
   :boost :highlight
   :distal :predicted
   })

(def excitation-order
  [:proximal-unstable
   :proximal-stable
   :boost
   :distal])

(defn viz-rgn-shades
  [network-shape]
  (let [srcs (concat (keys (:senses @network-shape))
                     (keys (:regions @network-shape)))]
    (zipmap srcs (range -0.3 0.31 (/ 1.0 (count srcs))))))

(defn- abs [x] (if (neg? x) (- x) x))

(defn draw-cell-excitation-plot!
  [ctx breakdowns network-shape sel-col series-colors]
  (let [width-px (.-width (.-canvas ctx))
        height-px (.-height (.-canvas ctx))
        plot-size {:w width-px
                   :h 200}

        src-shades (viz-rgn-shades network-shape)
        y-max (* 1.1 (apply max (map :total (vals breakdowns))))
        x-lim [-0.5 (+ (count breakdowns) 3)] ;; space for legend
        y-lim [y-max 0]
        bot-lab-y (- (* y-max 0.02))
        draw-cell-bar
        (fn [plot x-coord bd labels?]
          (let [series (for [k excitation-order
                             :let [v (get bd k)]
                             [src z] (if (map? v)
                                       (sort-by (comp src-shades key) v)
                                       {nil v})
                             :when (and z (pos? z))]
                         [k src z])]
            (c/stroke-style ctx "black")
            (reduce (fn [offset [k src z]]
                      (let [color (excitation-colors k)
                            shade (if src (src-shades src) 0.0)]
                        (c/fill-style ctx (get series-colors color))
                        (plt/rect! plot x-coord offset 0.5 z)
                        (when-not (zero? shade)
                          (c/fill-style ctx (if (pos? shade) "white" "black"))
                          (c/alpha ctx (abs shade))
                          (plt/rect! plot x-coord offset 0.25 z)
                          (c/alpha ctx 1.0))
                        (let [y-top (+ offset z)]
                          (plt/line! plot [[(+ x-coord 0.05) y-top]
                                           [(+ x-coord 0.45) y-top]]))
                        (when labels?
                          (c/fill-style ctx "black")
                          (let [labs (concat (str/split (name k) #"-")
                                             (if src [(str "(" (name src) ")")]))]
                            (plt/texts! plot (+ x-coord 0.5) (+ offset (* 0.5 z))
                                        labs 10)))
                        (+ offset (or z 0))))
                    0.0
                    series)))]
    (c/save ctx)
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    (c/stroke-width ctx 0.5)
    (let [plot (plt/xy-plot ctx plot-size x-lim y-lim)]
      (doseq [[i [cell-id bd]] (->> breakdowns
                                    (sort-by key)
                                    (sort-by (comp :total val) >)
                                    (map-indexed vector))
              :let [x-coord i
                    [col _] cell-id
                    total-exc (:total bd)]]
        (draw-cell-bar plot x-coord bd false)
        (when (= col sel-col)
          (c/fill-style ctx (:highlight series-colors))
          (plt/rect! plot (- x-coord 0.25) -100 1.0 100))
        (c/fill-style ctx "black")
        (plt/text! plot x-coord (+ total-exc 0.5) total-exc)
        (plt/text-rotated! plot x-coord bot-lab-y (if (= col sel-col) cell-id col)))
      ;; draw legend
      (let [sep-x (count breakdowns)
            leg-x (inc sep-x)
            key-bd* (->
                     (apply util/deep-merge-with + (vals breakdowns))
                     (core/update-excitation-breakdown #(if (pos? %) 1.0 0.0)))
            key-bd (core/update-excitation-breakdown key-bd* #(* % (/ y-max (:total key-bd*))))]
        (c/fill-style ctx (:background series-colors))
        (plt/rect! plot sep-x 0 (- (second x-lim) sep-x) y-max)
        (c/text-align ctx :center)
        (draw-cell-bar plot leg-x key-bd true)
        (c/fill-style ctx "black")
        (c/text-align ctx :left)
        (plt/text-rotated! plot leg-x bot-lab-y "KEY")
        (c/stroke-width ctx 1)
        (plt/frame! plot)))
    (c/restore ctx)))

(defn fetch-excitation-data!
  [excitation-data region-key layer-id sels into-journal]
  (let [sel1 (first (filter sel/layer sels))
        bit (when (= (sel/layer sel1) [region-key layer-id])
              (:bit sel1))]
    (if-let [snapshot-id (get-in sel1 [:step :snapshot-id])]
      (let [response-c (async/chan)]
        (put! into-journal
              ["get-cell-excitation-data" snapshot-id region-key
               layer-id bit (marshal/channel response-c true)])
        (go
          (reset! excitation-data (<! response-c))))
      (reset! excitation-data {}))))

(defn cell-excitation-plot-cmp
  [_ selection _ region-key layer-id into-journal]
  (let [excitation-data (atom {})]
    (reagent/create-class
     {:component-will-mount
      (fn [_]
        (add-watch selection [::fetch-excitation-data region-key layer-id]
                   (fn [_ _ _ sels]
                     (fetch-excitation-data! excitation-data region-key layer-id
                                             sels into-journal)))

        (fetch-excitation-data! excitation-data region-key layer-id @selection
                                into-journal))

      :component-will-unmount
      (fn [_]
        (remove-watch selection [::fetch-excitation-data region-key layer-id]))

      :reagent-render
      (let [size-invalidates-c (async/chan)]
        (fn [network-shape _ series-colors region-key layer-id _ _]
          [:div nil
           [window-resize-listener size-invalidates-c]
           [resizing-canvas
            {:style {:width "100%"
                     :height "240px"}}
            [excitation-data]
            (fn [ctx]
              (let [sel (first (filter sel/layer @selection))
                    bit (when (= (sel/layer sel) [region-key layer-id]) (:bit sel))]
                (draw-cell-excitation-plot! ctx @excitation-data network-shape
                                            bit series-colors)))
            size-invalidates-c]]))
      })))

(defn draw-cell-sdrs-plot!*
  [ctx
   {:as plot-data
    :keys [sdr-transitions sdr-label-counts sdr-votes sdr-sizes sdr-growth
           kept-sdrs hit-counts timestep threshold title]}
   {:as plot-opts
    :keys [group-contexts? ordering hide-conns-smaller]}]
  (let [wc-sdrv (:winners sdr-votes)
        ac-sdrv (:active sdr-votes)
        pc-sdrv (:pred sdr-votes)
        kept-sdr? (set kept-sdrs)
        sdr-label-counts* (select-keys sdr-label-counts kept-sdrs)
        gap threshold
        [sdr-ordinate y-max] (loop [sdrs (case ordering
                                           :first-appearance (sort kept-sdrs)
                                           :last-appearance kept-sdrs)
                                    m {}
                                    offset 0]
                               (if-let [sdr (first sdrs)]
                                 (let [dy (sdr-sizes sdr)
                                       gy (sdr-growth sdr 0)
                                       ord (+ offset gap (/ dy 2))]
                                   (recur (rest sdrs)
                                          (assoc m sdr ord)
                                          (+ offset gap dy gy)))
                                 [m offset]))
        title-px 16
        hits-w-px 30
        full-width-px (.-width (.-canvas ctx))
        width-px (- full-width-px hits-w-px)
        full-height-px (.-height (.-canvas ctx))
        height-px (- full-height-px title-px)
        y-scale (/ height-px (max (* 50 gap) (+ y-max gap)))
        sdr-max-count (->> (vals sdr-label-counts*)
                           (map (fn [label-counts]
                                  (reduce + (vals label-counts))))
                           (reduce max)
                           (max 5))
        x-scale (/ width-px sdr-max-count)
        mid-x (quot width-px 2)
        label-width 50]
    (c/save ctx)
    (c/clear-rect ctx {:x 0 :y 0 :w full-width-px :h full-height-px})
    (c/translate ctx (+ mid-x hits-w-px) title-px)
    ;; draw title
    (let [title-y 0]
      (c/text-align ctx :center)
      (c/text-baseline ctx :bottom)
      (c/text ctx {:x 0
                   :y title-y
                   :text title})
      (c/text ctx {:x (* mid-x 0.8)
                   :y title-y
                   :text "forward"})
      (c/text ctx {:x (- (* mid-x 0.8))
                   :y title-y
                   :text "back"}))
    ;; draw transitions
    (c/stroke-style ctx "hsl(210,50%,50%)")
    (doseq [[from-sdr to-sdrs-counts] sdr-transitions
            :when (kept-sdr? from-sdr)
            :let [from-y (* y-scale (sdr-ordinate from-sdr))]
            [to-sdr to-sdr-count] to-sdrs-counts
            :when (kept-sdr? to-sdr)
            :when (>= to-sdr-count hide-conns-smaller)
            :let [to-y (* y-scale (sdr-ordinate to-sdr))
                  mid-y (/ (+ to-y from-y) 2)
                  off-x (* 1.0 width-px
                           (/ (- to-y from-y)
                              height-px))]]
      (doto ctx
        (c/stroke-width (-> (/ to-sdr-count threshold) (* 4) (min 6)))
        (c/begin-path)
        (c/move-to 0 from-y)
        (c/quadratic-curve-to off-x mid-y
                              0 to-y)
        (c/stroke)))
    (c/stroke-width ctx 1)
    ;; draw states
    (c/text-baseline ctx :middle)
    (doseq [[sdr label-counts] sdr-label-counts*
            :let [y-mid (* y-scale (sdr-ordinate sdr))
                  sdr-tot-count (reduce + (vals label-counts))
                  sdr-width (* x-scale sdr-tot-count)
                  sdr-height (* y-scale (sdr-sizes sdr))
                  y-top (- y-mid (quot sdr-height 2))
                  active-color (cond
                                (ac-sdrv sdr) "#fbb"
                                (pc-sdrv sdr) "#bbf"
                                :else nil)
                  active-size (or (ac-sdrv sdr)
                                  (pc-sdrv sdr))
                  sdr-rect {:x (- (quot sdr-width 2))
                            :y y-top
                            :w sdr-width
                            :h sdr-height
                            :r 5}]]
      ;; outline/background of state
      (doto ctx
        (c/stroke-style "#aaa")
        (c/rounded-rect (update sdr-rect :r min (/ sdr-height 2)))
        (c/alpha 0.8)
        (c/fill-style "#ddd")
        (c/fill)
        (c/alpha 1.0))
      ;; activation / prediction level
      (when active-size
        (let [h (* y-scale active-size)]
          (doto ctx
            (c/alpha 0.4)
            (c/fill-style active-color)
            (c/rounded-rect (-> (assoc sdr-rect :h h)
                                (update :r min (/ h 2))))
            (c/fill)
            (c/alpha 1.0))))
      ;; growth
      (when-let [growth (sdr-growth sdr)]
        (let [h (* y-scale growth)]
          (doto ctx
            (c/alpha 0.8)
            (c/fill-style "#bfb")
            (c/rounded-rect (-> (assoc sdr-rect :h h)
                                (update :r min (/ h 2))
                                (update :y + sdr-height)))
            (c/fill)
            (c/alpha 1.0))))
      ;; outline of exact matching cells (winner cells)
      (when-let [wc-size (wc-sdrv sdr)]
        (let [h (* y-scale wc-size)]
          (doto ctx
            (c/stroke-style "#000")
            (c/rounded-rect (-> (assoc sdr-rect :h h)
                                (update :r min (/ h 2)))))))
      ;; draw labels; center label on new growth if that's all there is
      (let [gy (* y-scale (sdr-growth sdr))
            lab-y (if (pos? sdr-height)
                    y-mid
                    (+ y-mid (/ gy 2)))]
        (c/fill-style ctx "#000")
        (reduce (fn [offset [label n]]
                  (c/text ctx {:x (* x-scale (+ offset (/ n 2)))
                               :y lab-y
                               :text (str label)})
                  (+ offset n))
                (- (/ sdr-tot-count 2))
                label-counts)))
    ;; hit counts from spreading activation
    (let [max-hits (apply max 5 (vals hit-counts))
          x-left (- 0 mid-x hits-w-px)]
      (c/text-align ctx :right)
      (c/font-style ctx (str (round (* hits-w-px 0.5)) "px sans"))
      (doseq [[sdr n-hits] hit-counts
              :let [z (min 1.0 (/ n-hits max-hits))
                    y-mid (* y-scale (sdr-ordinate sdr))
                    sdr-height (* y-scale (sdr-sizes sdr))
                    y-top (- y-mid (quot sdr-height 2))]]
        (doto ctx
          (c/alpha z)
          (c/fill-style "black")
          (c/fill-rect {:x x-left
                        :y y-top
                        :w hits-w-px
                        :h sdr-height})
          (c/alpha 1.0)
          (c/fill-style (if (> z 0.5) "white" "black"))
          (c/text {:x (+ x-left hits-w-px -5)
                   :y y-mid
                   :text n-hits}))))
    (c/restore ctx)))

(defn cell-sdrs-plot-data-group-contexts
  [{:as plot-data
    :keys [sdr-transitions sdr-label-counts sdr-votes sdr-sizes sdr-growth
           sdr->gid gid-sizes gid-growth kept-sdrs current-sdrs
           ]}]
  (let [kept-gids (-> (map sdr->gid kept-sdrs) ;; keep order by last appearance:
                      (reverse) (distinct) (reverse))]
    ;; TODO an abstraction for this kind of aggregation
    (-> plot-data
        (assoc :kept-sdrs kept-gids
               :current-sdrs (distinct (map sdr->gid current-sdrs))
               :sdr-sizes gid-sizes
               :sdr-growth gid-growth
               :grouped? true)
        (update :sdr-label-counts
                (fn [m]
                  (persistent!
                   (reduce-kv
                    (fn [m sdr label-counts]
                      (let [gid (sdr->gid sdr)]
                        (assoc! m gid (merge-with + (get m gid {})
                                                  label-counts))))
                    (transient {})
                    m))))
        (update :sdr-transitions
                (fn [m]
                  (persistent!
                   (reduce-kv
                    (fn [m from-sdr to-sdrs-counts]
                      (let [from-gid (sdr->gid from-sdr)
                            to-gids-counts (persistent!
                                            (reduce-kv
                                             (fn [mm to-sdr n]
                                               (let [to-gid (sdr->gid to-sdr)]
                                                 (assoc! mm to-gid
                                                         (max (get mm to-gid 0)
                                                              n))))
                                             (transient {})
                                             to-sdrs-counts))]
                        (assoc! m from-gid (merge-with + (get m from-gid {})
                                                       to-gids-counts))))
                    (transient {})
                    m))))
        (update :sdr-votes (fn [vm]
                             (util/remap
                              (fn [m]
                                (reduce-kv (fn [mm sdr vote]
                                             (let [gid (sdr->gid sdr)]
                                               (assoc mm gid
                                                      (max (get mm gid 0)
                                                           vote))))
                                           {}
                                           m))
                              vm)))
        )))

(defn cell-sdrs-plot-spread-activation
  [{:as plot-data
    :keys [sdr-transitions sdr-last-matches timestep kept-sdrs current-sdrs]}
   {:as plot-opts
    :keys [spreading-activation-steps hide-conns-smaller]}]
  (let [ok-transitions (->> (select-keys sdr-transitions kept-sdrs)
                            (into {}
                                  (map (fn [[from-sdr to-sdrs-counts]]
                                         [from-sdr
                                          (keep (fn [[id n]]
                                                  (when (and (not= from-sdr id)
                                                             (>= n hide-conns-smaller))
                                                    id))
                                                to-sdrs-counts)]))))
        hit-counts
        (loop [hit-counts (zipmap current-sdrs (repeat 0))
               current-ids current-sdrs
               n spreading-activation-steps]
          (if (pos? n)
            (let [next-ids (mapcat ok-transitions current-ids)
                  next-counts (merge-with + hit-counts
                                          (frequencies next-ids))]
              (recur next-counts
                     next-ids
                     (dec n)))
            ;; finished
            hit-counts))]
    (assoc plot-data
           :hit-counts hit-counts)))

(defn kept-sdrs-by-last-appearance
  "Applies options hide-states-older, hide-states-rarer to filter the
  list of sdrs, and returns them ordered by last appearance."
  [{:keys [sdr-label-counts sdr-last-matches sdr-votes timestep]}
   {:keys [hide-states-older hide-states-rarer]}]
  (let [wc-sdrv (:winners sdr-votes)]
    (->> (subseq sdr-last-matches >= (- timestep hide-states-older))
         (into []
               (comp (map key)
                     (if (> hide-states-rarer 1)
                       (filter (fn [sdr]
                                 (or (wc-sdrv sdr)
                                     (let [label-counts (sdr-label-counts sdr)]
                                       (>= (reduce + (vals label-counts))
                                           hide-states-rarer)))))
                       identity))))))

(defn draw-cell-sdrs-plot!
  [ctx
   {:as plot-data*
    :keys [sdr-last-matches timestep]}
   plot-opts]
  (let [kept-sdrs (kept-sdrs-by-last-appearance plot-data* plot-opts)
        current-sdrs (map key (subseq sdr-last-matches >= timestep))
        plot-data (cond->
                      (assoc plot-data* :kept-sdrs kept-sdrs
                             :current-sdrs current-sdrs)
                    (:group-contexts? plot-opts)
                    (cell-sdrs-plot-data-group-contexts)
                    (pos? (:spreading-activation-steps plot-opts))
                    (cell-sdrs-plot-spread-activation plot-opts))]
    (draw-cell-sdrs-plot!* ctx plot-data plot-opts)))

(defn cell-sdrs-context-analysis-cmp
  [{:as plot-data
    :keys [sdr-history sdr-transitions sdr->gid sdr-label-counts timestep]}
   {:as plot-opts
    :keys [hide-states-older hide-conns-smaller]}]
  (let [sdrseq (reverse (take hide-states-older sdr-history))
        sdr->label (util/remap (fn [labm] (key (apply max-key val labm)))
                               sdr-label-counts)
        sdr->count (util/remap (fn [labm] (apply + (vals labm)))
                               sdr-label-counts)
        gid->sdrs (group-by sdr->gid (keys sdr->gid))
        kept-sdrs (kept-sdrs-by-last-appearance plot-data plot-opts)
        ok-transitions (->> (select-keys sdr-transitions kept-sdrs)
                            (into {}
                                  (map (fn [[from-sdr to-sdrs-counts]]
                                         [from-sdr
                                          (keep (fn [[id n]]
                                                  (when (and (not= from-sdr id)
                                                             (>= n hide-conns-smaller))
                                                    id))
                                                to-sdrs-counts)]))))
        column-diversity (fn [gid]
                           (let [instances (gid->sdrs gid)
                                 counts (map sdr->count instances)
                                 total (reduce + counts)]
                             (- (reduce + (map (fn [n]
                                                 (let [p (/ n total)]
                                                   (* p (Math/log p))))
                                               counts)))))
        context-alternatives (fn [ctx-sdr]
                               (->> (ok-transitions ctx-sdr)
                                    (map sdr->gid)
                                    (distinct)))
        curr-sdr (first sdr-history)]
    [:table.table.table-condensed
     [:thead
      [:tr
       [:th]
       [:th.text-right "diversity of contexts input appears in"]
       [:th.text-right "number of inputs appearing in this context"]]]
     (into [:tbody]
           (for [[prior-sdr sdr] (partition 2 1 (cons nil sdrseq))
                 :let [col-d (column-diversity (sdr->gid sdr))
                       ctx-n (if prior-sdr
                               (count (context-alternatives prior-sdr))
                               0)]]
             [:tr
              [:th (if (== sdr curr-sdr) {:class :active})
               (str (sdr->label sdr))]
              [:td.text-right
               (cond (>= col-d 1.5) {:class :success}
                     (>= col-d 1.2) {:class :info}
                     (>= col-d 0.68) {:class :warning}
                     :else {})
               (.toFixed col-d 2)]
              [:td.text-right
               (cond (>= ctx-n 4) {:class :success}
                     (>= ctx-n 3) {:class :info}
                     (>= ctx-n 2) {:class :warning})
               ctx-n]]))
     (let [sdr (first sdr-history)
           prev-sdr (second sdr-history)
           alternatives (context-alternatives prev-sdr)
           instances (gid->sdrs (sdr->gid sdr))
           transitions-to (fn [sdr]
                            (keep (fn [[from-sdr to-sdrs]]
                                    (when (some #(= sdr %) to-sdrs)
                                      from-sdr))
                                  ok-transitions))]
       [:tfoot
        [:tr
         [:th]
         [:td.text-right
          ;; contexts input appears in
          (into [:ol]
                (for [i-sdr instances]
                  [:li (->> (transitions-to i-sdr)
                            (map sdr->label)
                            (str/join " / "))
                   " (...)"]))]
         [:td.text-right
          ;; inputs appearing in this context
          (into [:ol]
                (for [a-sdr alternatives]
                  [:li (sdr->label a-sdr)]))]]])]))

(defn fetch-transitions-data
  [sel1 cell-sdr-counts into-journal]
  (when-let [[region layer] (sel/layer sel1)]
    (let [{:keys [snapshot-id]} (:step sel1)
          response-c (async/chan)]
      (put! into-journal ["get-transitions-data"
                          snapshot-id region layer cell-sdr-counts
                          (marshal/channel response-c true)])
      response-c)))

(defn calc-sdr-sizes
  [cell-sdr-fracs]
  (persistent!
   (reduce (fn [m sdr-fracs]
             (reduce-kv (fn [m sdr frac]
                          (assoc! m sdr
                                  (+ (get m sdr 0)
                                     frac)))
                        m
                        sdr-fracs))
           (transient {})
           (vals cell-sdr-fracs))))

(defn update-cell-sdrs-plot-data!
  [plot-data states sel1 into-journal]
  (when-let [[region layer] (sel/layer sel1)]
    (let [snapshot-id (get-in sel1 [:step :snapshot-id])]
      (when-let [state* (get-in @states [snapshot-id [region layer]])]
        (let [state (assoc state* :title (str (name region) " " (name layer)))]
          (if-not (:sdr-transitions state)
           ;; request transitions new data
           (go
             ;; immediate update from local state while waiting:
             (reset! plot-data state)
             ;; another update when receive server data
             (let [x (<! (fetch-transitions-data sel1 (:cell-sdr-counts state)
                                                 into-journal))]
               (swap! states assoc-in [snapshot-id [region layer] :sdr-transitions] x)
               (swap! plot-data assoc :sdr-transitions x)))
           ;; otherwise - we have the data
           (reset! plot-data state)))))))

(defn sdr-votes
  [cells cell-sdr-fracs]
  (->> (map cell-sdr-fracs cells)
       (apply merge-with + {})))

(defn- freqs->fracs
  [freqs]
  (let [total (reduce + (vals freqs))]
    (util/remap #(/ % total) freqs)))

(def empty-cell-sdrs-state
  {:cell-sdr-counts {}
   :col-gid-counts {}
   :sdr->gid {}
   :sdr-label-counts {}
   :sdr-sizes {}
   :gid-sizes {}
   :sdr-growth {}
   :sdr-votes {:winners {}
               :active {}
               :pred {}}
   :sdr-transitions nil
   :sdr-last-matches (priority-map)
   :sdr-history ()
   :timestep 0
   :threshold 0})

(defn update-cell-sdrs-states!
  [states network-shape step prev-step into-journal]
  (for [[region layer-map] (:regions @network-shape)
        layer (keys layer-map)
        :let [response-c (async/chan)
              snapshot-id (:snapshot-id step)]]
    (go
      (put! into-journal ["get-cells-by-state" snapshot-id
                          region layer
                          (marshal/channel response-c true)])
      (let [cells-by-state (<! response-c)
            state (or (get-in @states [(:snapshot-id prev-step) [region layer]])
                      empty-cell-sdrs-state)
            wc (:winner-cells cells-by-state)
            ac (:active-cells cells-by-state)
            pc (:pred-cells cells-by-state)
            on? (:engaged? cells-by-state)
            threshold (get-in @network-shape [:regions region layer
                                              :spec :distal :learn-threshold])
            ;; for each cell, represents its specificity to each SDR
            cell-sdr-fracs (->> (:cell-sdr-counts state)
                                (util/remap freqs->fracs))
            ac-sdrv (sdr-votes ac cell-sdr-fracs)
            pc-sdrv (sdr-votes pc cell-sdr-fracs)
            wc-sdrv (sdr-votes wc cell-sdr-fracs)
            [win-sdrs new-sdr] (let [hits (keep (fn [[id vote]]
                                                  (when (>= vote threshold) id))
                                                wc-sdrv)
                                     n (count (:sdr-label-counts state))]
                                 (if (seq hits) [hits nil] ;; otherwise, new state:
                                     [[n] n]))
            ;; grouping by columns
            col-gid-fracs (->> (:col-gid-counts state)
                               (util/remap freqs->fracs))
            a-cols (map first wc)
            gidv (sdr-votes a-cols col-gid-fracs)
            [win-gids new-gid] (let [hits (keep (fn [[id vote]]
                                                  (when (>= vote threshold) id))
                                                gidv)
                                     n (count (:sdr-label-counts state))]
                                 (if (seq hits) [hits nil] ;; otherwise, new state:
                                     [[n] n]))
            ;; etc
            sdr-sizes (or (:updated-sdr-sizes state)
                          (calc-sdr-sizes cell-sdr-fracs))
            gid-sizes (or (:updated-gid-sizes state)
                          (calc-sdr-sizes col-gid-fracs))
            inc-win-sdrs (partial merge-with + (zipmap win-sdrs (repeat 1)))
            inc-win-gids (partial merge-with + (zipmap win-gids (repeat 1)))
            label (let [v (:input-value step)] (get v :label v))
            inc-label (partial merge-with + {label (/ 1 (count win-sdrs))})
            t (:timestep step)
            new-state* (cond-> state
                         on? (update :cell-sdr-counts
                                     (fn [m]
                                       (util/update-each m wc inc-win-sdrs)))
                         on? (update :col-gid-counts
                                     (fn [m]
                                       (util/update-each m a-cols inc-win-gids)))
                         on? (update :sdr->gid
                                     (fn [m]
                                       (if new-sdr (assoc m new-sdr (first win-gids))
                                           m)))
                         on? (update :sdr-label-counts
                                     (fn [m]
                                       (util/update-each m win-sdrs inc-label)))
                         on? (update :sdr-last-matches
                                     (fn [m]
                                       (merge m (zipmap win-sdrs (repeat t)))))
                         on? (update :sdr-history conj (first win-sdrs))
                         true
                         (assoc :sdr-transitions nil ;; updated lazily
                                :sdr-votes {:winners wc-sdrv
                                            :active ac-sdrv
                                            :pred pc-sdrv}
                                :sdr-sizes sdr-sizes
                                :gid-sizes gid-sizes
                                :timestep t
                                :threshold threshold))
            new-sdr-sizes (calc-sdr-sizes (->> (:cell-sdr-counts new-state*)
                                               (util/remap freqs->fracs)))
            sdr-growth (merge-with - (select-keys new-sdr-sizes win-sdrs)
                                   (select-keys sdr-sizes win-sdrs))
            new-gid-sizes (calc-sdr-sizes (->> (:col-gid-counts new-state*)
                                               (util/remap freqs->fracs)))
            gid-growth (merge-with - (select-keys new-gid-sizes win-gids)
                                   (select-keys gid-sizes win-gids))
            new-state (cond-> new-state*
                        on? (assoc :updated-sdr-sizes new-sdr-sizes
                                   :sdr-growth sdr-growth
                                   :updated-gid-sizes new-gid-sizes
                                   :gid-growth gid-growth)
                        (and on? new-sdr) (assoc-in [:sdr-votes :winners new-sdr]
                                                    (sdr-growth new-sdr)))]
        (swap! states assoc-in [snapshot-id [region layer]] new-state)))))

(defn cell-sdrs-plot-builder
  [steps network-shape selection into-journal plot-opts]
  (let [states (atom {})
        plot-data (atom (assoc empty-cell-sdrs-state :title ""))]
    (add-watch steps ::cell-sdrs-plot
               (fn [_ _ _ [step prev-step]]
                 (when (:snapshot-id step)
                   (let [procs (update-cell-sdrs-states! states network-shape
                                                         step prev-step
                                                         into-journal)]
                     (go
                       ;; await update processes for all layers...
                       (doseq [c procs] (<! c))
                       ;; ...before refreshing plot
                       (let [[sel] @selection]
                         (update-cell-sdrs-plot-data! plot-data states sel
                                                      into-journal)))))))
    (add-watch selection ::cell-sdrs-plot
               (fn [_ _ _ [sel]]
                 (update-cell-sdrs-plot-data! plot-data states sel
                                              into-journal)))
    ;; at build time, pull in existing steps starting from current selection
    (let [[sel1] @selection]
      (when-let [snapshot-id (get-in sel1 [:step :snapshot-id])]
        (let [to-ingest (->> (reverse @steps)
                             (drop-while #(not= snapshot-id (:snapshot-id %))))]
          (println "cell-sdrs ingesting from" (:snapshot-id (first to-ingest)) "to"
                   (:snapshot-id (last to-ingest)))
          (go
            (doseq [[prev-step step] (partition 2 1 (cons nil to-ingest))]
              (let [procs (update-cell-sdrs-states! states network-shape
                                                    step prev-step
                                                    into-journal)]
                ;; await update processes before going on to next time step
                (doseq [c procs] (<! c))))
            ;; finally, redraw
            (update-cell-sdrs-plot-data! plot-data states sel1 into-journal)))))
    {:content
     (let [size-invalidates-c (async/chan)]
       (fn []
         [:div nil
          [window-resize-listener size-invalidates-c]
          [resizing-canvas
           {:style {:width "100%"
                    :height "100vh"}}
           [plot-data
            plot-opts]
           (fn [ctx]
             (draw-cell-sdrs-plot! ctx @plot-data @plot-opts))
           size-invalidates-c]
          (cell-sdrs-context-analysis-cmp @plot-data @plot-opts)]))
     :teardown
     (fn []
       (remove-watch steps ::cell-sdrs-plot)
       (remove-watch selection ::cell-sdrs-plot))}))
