(ns comportexviz.plots
  (:require [reagent.core :as reagent :refer [atom]]
            [monet.canvas :as c]
            [comportexviz.plots-canvas :as plt]
            [comportexviz.helpers :refer [canvas resizing-canvas]]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
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
  [col-state-freqs-log series-colors]
  [resizing-canvas
    {:style {:width "100%"
             :height 180}}
    []
    (fn [ctx]
      (stacked-ts-plot ctx col-state-freqs-log
                       [:active :active-predicted :predicted]
                       series-colors))])

(def excitation-colors
  {:proximal-unstable :active
   :proximal-stable :active-predicted
   :boost :highlight
   :temporal-pooling :temporal-pooling
   :distal :predicted
   })

(def excitation-order
  [:proximal-unstable
   :proximal-stable
   :boost
   :temporal-pooling
   :distal])

(defn viz-rgn-shades
  [step-template]
  (let [srcs (concat (keys (:inputs @step-template))
                     (keys (:regions @step-template)))]
    (zipmap srcs (range -0.3 0.31 (/ 1.0 (count srcs))))))

(defn- abs [x] (if (neg? x) (- x) x))

(defn draw-cell-excitation-plot!
  [ctx breakdowns step-template sel-col series-colors]
  (let [width-px (.-width (.-canvas ctx))
        height-px (.-height (.-canvas ctx))
        plot-size {:w width-px
                   :h 200}

        src-shades (viz-rgn-shades step-template)
        y-max (* 1.1 (apply max (map :total (vals breakdowns))))
        x-lim [-0.5 (+ (count breakdowns) 3)] ;; space for legend
        y-lim [y-max 0]
        bot-lab-y (- (* y-max 0.02))
        draw-cell-bar
        (fn [plot x-coord bd labels?]
          (let [series (for [k excitation-order
                             :let [v (get bd k)]
                             [src x] (if (map? v)
                                       (sort-by (comp src-shades key) v)
                                       {nil v})
                             :when (and x (pos? x))]
                         [k src x])]
            (c/stroke-style ctx "black")
            (reduce (fn [offset [k src x]]
                      (let [color (excitation-colors k)
                            shade (if src (src-shades src) 0.0)]
                        (c/fill-style ctx (get series-colors color))
                        (plt/rect! plot x-coord offset 0.5 x)
                        (when-not (zero? shade)
                          (c/fill-style ctx (if (pos? shade) "white" "black"))
                          (c/alpha ctx (abs shade))
                          (plt/rect! plot x-coord offset 0.25 x)
                          (c/alpha ctx 1.0))
                        (when labels?
                          (c/fill-style ctx "black")
                          (let [labs (concat (str/split (name k) #"-")
                                             (if src [(str "(" (name src) ")")]))]
                            (plt/texts! plot (+ x-coord 0.5) (+ offset (* 0.5 x))
                                        labs 10)))
                        (+ offset (or x 0))))
                    0.0
                    series)))]
    (c/save ctx)
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
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
        (plt/frame! plot)))
    (c/restore ctx)))

(defn fetch-excitation-data!
  [excitation-data sel into-journal channel-proxies]
  (let [{:keys [model-id region layer col]} sel
        response-c (async/chan)]
    (put! @into-journal
          [:get-cell-excitation-data model-id region layer col
           (channel-proxy/from-chan channel-proxies
                                    response-c)])
    (go
      (reset! excitation-data (<! response-c)))))

(defn cell-excitation-plot-cmp
  [_ selection _ _ _ into-journal channel-proxies]
  (let [excitation-data (atom {})]
    (add-watch selection :fetch-excitation-data
               (fn [_ _ _ sel]
                 (fetch-excitation-data! excitation-data sel into-journal
                                         channel-proxies)))

    (fetch-excitation-data! excitation-data @selection into-journal
                            channel-proxies)

    (fn [step-template _ series-colors region-key layer-id _ _]
      [canvas
       {}
       300
       240
       [excitation-data]
       (fn [ctx]
         (let [dt (:dt @selection)
               sel-col (when (and (= region-key (:region @selection))
                                  (= layer-id (:layer @selection)))
                         (:col @selection))]
           (draw-cell-excitation-plot! ctx @excitation-data step-template
                                       sel-col series-colors)))
       nil])))
