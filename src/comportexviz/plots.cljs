(ns comportexviz.plots
  (:require [reagent.core :as reagent :refer [atom]]
            [monet.canvas :as c]
            [comportexviz.plots-canvas :as plt]
            [comportexviz.helpers :refer [canvas]]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [clojure.string :as str]
            [goog.dom :as dom]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn mean
  [xs]
  (/ (apply + xs) (count xs)))

(defn aggregate-by
  [f maps]
  (let [ks (keys (first maps))]
    (->>
     (for [k ks]
       [k (f (map k maps))])
     (into {}))))

(defn double-aggregation-bucket
  [{:as m :keys [bucket ts]}]
  (let [b (* 2 bucket)]
    (assoc m
      :bucket b
      :ts (mapv (partial aggregate-by mean)
                (partition 2 ts)))))

(defn update-aggregated-ts
  [agg-val raw-ts keep-n]
  ;; append to aggregated series
  (let [new-agg (update-in agg-val [:ts] conj
                           (aggregate-by mean raw-ts))
        n (count (:ts new-agg))]
    (if (and (> n keep-n)
             (even? n))
      (double-aggregation-bucket new-agg)
      new-agg)))

(defn aggregated-ts-ref
  [c keep-n]
  (let [agg (atom {:bucket 1 :ts [] :keep-n keep-n})]
    (go (loop [pxs []]
          (let [xs (conj pxs (<! c))]
            (if (< (count xs) (:bucket @agg))
              (recur xs)
              (do
                (swap! agg update-aggregated-ts xs keep-n)
                (recur (empty xs)))))))
    agg))

(defn aggregating-ts
  [step-atom keep-n]
  (let [agg (atom {:bucket 1 :ts [] :keep-n keep-n})
        accumulator (atom [])]
    (add-watch step-atom :aggregate
               (fn [_ _ _ x]
                 (let [pxs @accumulator
                       xs (conj pxs x)]
                   (if (< (count xs) (:bucket @agg))
                     (reset! accumulator xs)
                     (do
                       (swap! agg update-aggregated-ts xs keep-n)
                       (reset! accumulator (empty xs))))
                   )))
    agg))

(defn stacked-ts-plot
  [el agg series-keys series-colors]
  (let [{:keys [ts keep-n bucket]} @agg
        n-timesteps (* bucket keep-n)
        ncol (:size (peek ts))
        v-max (* ncol 0.06)
        ctx (c/get-context el "2d")
        plot-size {:w (- (.-width el) 50)
                   :h (- (.-height el) 50)}
        plot (plt/xy-plot ctx plot-size
                          [0 n-timesteps]
                          [v-max 0])
        ]
    (c/clear-rect ctx {:x 0 :y 0 :w (:w plot-size) :h (:h plot-size)})
    (c/stroke-width ctx 0)
    (doseq [[i x] (plt/indexed ts)]
      (reduce (fn [from-y k]
                (let [val (get x k)]
                  (c/fill-style ctx (series-colors k))
                  (plt/rect! plot (* i bucket) from-y
                             bucket val)
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

(defn ts-freqs-plot-cmp
  [steps region-key layer-id series-colors into-journal channel-proxies]
  (let [series-keys [:active :active-predicted :predicted]
        step-freqs (atom nil)
        agg-freqs-ts (aggregating-ts step-freqs 200)]
    (add-watch steps [:calc-freqs region-key layer-id] ;; unique key per layer
               (fn [_ _ _ v]
                 (when-let [model-id (:model-id (first v))]
                   (let [response-c (async/chan)]
                     (put! @into-journal
                           [:get-column-state-freqs model-id
                            region-key layer-id
                            (channel-proxy/from-chan channel-proxies
                                                     response-c)])
                     (go
                       (reset! step-freqs (<! response-c)))))))
    (fn [_ _ _ _]
      (let [el-id (str "comportexviz-tsplot-" (name region-key) (name layer-id))
            el (dom/getElement el-id)]
        ;; draw!
        (when el
          (set! (.-width el) (* 0.28 (- (.-innerWidth js/window) 20)))
          (set! (.-height el) 180)
          (stacked-ts-plot el agg-freqs-ts series-keys series-colors))
        (when-not el
          ;; create data dependency for re-rendering
          @agg-freqs-ts)
        [:div
         [:canvas {:id el-id}]])
      )))

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

(defn cell-excitation-plot-cmp
  [_ selection _ _ _ into-journal channel-proxies]
  (let [excitation-data (atom {})]
    (add-watch selection :fetch-excitation-data
               (fn [_ _ _ sel]
                 (let [{:keys [model-id region layer col]} sel
                       response-c (async/chan)]
                   (put! @into-journal
                         [:get-cell-excitation-data model-id region layer col
                          (channel-proxy/from-chan channel-proxies
                                                   response-c)])
                   (go
                     (reset! excitation-data (<! response-c))))))

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

(defn draw-transitions-plot!
  [ctx {:keys [sdr-transitions sdr-label-fracs curr-sdr]}]
  (let [width-px (.-width (.-canvas ctx))
        height-px (.-height (.-canvas ctx))
        y-scale (/ height-px (max 24 (inc (count sdr-label-fracs))))
        mid-x (quot width-px 2)
        label-width 50
        label-height 14]
    (c/save ctx)
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    (c/translate ctx mid-x (quot label-height 2))
    (c/stroke-style ctx "hsl(210,50%,50%)")
    (c/stroke-width ctx 4)
    (doseq [[from-sdr to-sdrs] sdr-transitions
            :let [from-y (* y-scale from-sdr)]
            to-sdr to-sdrs
            :let [to-y (* y-scale to-sdr)
                  mid-y (/ (+ to-y from-y) 2)
                  off-x (* 1.0 width-px
                           (/ (- to-y from-y)
                              height-px))]]
      (if (= from-sdr curr-sdr)
        (c/alpha ctx 1.0)
        (c/alpha ctx 0.3))
      (doto ctx
        (c/begin-path)
        (c/move-to 0 from-y)
        (c/quadratic-curve-to off-x mid-y
                              0 to-y)
        (c/stroke)))
    (c/alpha ctx 1.0)
    (c/stroke-width ctx 1)
    (c/stroke-style ctx "#ccc")
    (c/text-align ctx :center)
    (c/text-baseline ctx :middle)
    (doseq [[sdr label-fracs] sdr-label-fracs
            :let [y (* y-scale sdr)]]
      (doto ctx
        (c/fill-style (if (= sdr curr-sdr)
                        "#fdd"
                        "#eee"))
        (c/rounded-rect {:x (- (quot label-width 2))
                         :y (- y (quot label-height 2))
                         :w label-width
                         :h label-height
                         :r (quot label-height 2)})
        (c/fill)
        (c/fill-style "#000")
        (c/text {:x 0 :y y :text (->> (for [[label frac] label-fracs]
                                        (if (== frac 1.0)
                                          label
                                          (str label "(" frac ")")))
                                      (interpose " ")
                                      (apply str))})))
    (c/text ctx {:x (quot mid-x 2)
                 :y 0
                 :text "forward"})
    (c/text ctx {:x (- (quot mid-x 2))
                 :y 0
                 :text "back"})
    (c/restore ctx)))

(defn matching-sdr
  [cells cell-sdr-fracs threshold]
  (let [votes (->> (map cell-sdr-fracs cells)
                   (apply merge-with +)
                   (seq))
        [sdr-idx vote] (when votes (apply max-key val votes))]
    (println "votes" votes "so sdr-idx " sdr-idx "(" vote ">=" threshold ")")
    (if (and vote (>= vote threshold))
      sdr-idx
      nil)))

(defn- freqs->fracs
  [freqs]
  (let [total (reduce + (vals freqs))]
    (util/remap #(/ % total) freqs)))

(defn transitions-plot-cmp
  [steps step-template selection into-journal channel-proxies]
  (let [cell-sdr-counts (atom {})
        sdr-label-counts (atom {})
        curr-sdr (atom {})
        plot-data (atom nil)]
    (add-watch steps ::count-sdrs-and-labels
               (fn [_ _ _ step]
                 (when-let [model-id (:model-id (first step))]
                   (doseq [[region-key rgn] (:regions @step-template)
                           layer-id (keys rgn)
                           :let [response-c (async/chan)]]
                     (put! @into-journal [:get-learn-cells model-id
                                          region-key layer-id
                                          (channel-proxy/from-chan channel-proxies
                                                                   response-c)])
                     (go
                       (let [lc (<! response-c)
                             threshold (get-in @step-template [:regions
                                                               region-key
                                                               layer-id
                                                               :spec
                                                               :seg-learn-threshold])
                             cell-sdr-fracs (->> (get @cell-sdr-counts [region-key layer-id])
                                                 (util/remap freqs->fracs))
                             _ (println "count-sdrs" region-key layer-id)
                             which-sdr* (matching-sdr lc cell-sdr-fracs threshold)
                             which-sdr (or which-sdr*
                                           (count (get @sdr-label-counts
                                                       [region-key layer-id])))]
                         (swap! curr-sdr
                                assoc [region-key layer-id]
                                which-sdr)
                         (swap! cell-sdr-counts
                                update [region-key layer-id]
                                (fn [m]
                                  (util/update-each (or m {}) lc
                                                    #(merge-with + % {which-sdr 1}))))
                         (let [label (:label (first (:input-values (first step))))]
                           (swap! sdr-label-counts
                                  update [region-key layer-id]
                                  (fn [m]
                                    (if-not (get m which-sdr)
                                      (assoc m which-sdr {label 1})
                                      (update m which-sdr
                                              #(update % label (fnil inc 0)))))))
                         ))))))
    ;; TODO: keep history of values by dt. watch selection and/or sdr-label-counts?
    ;; or maybe should do all this accounting on the server side.
    ;; (which would avoid continually sending cell-sdr-fracs to server)
    (add-watch sdr-label-counts ::fetch-transitions-data
               (fn [_ _ old AT-sdr-label-counts]
                 (let [{:keys [model-id region layer]} @selection]
                   (when (not= (get old [region layer])
                               (get AT-sdr-label-counts [region layer]))
                     (let [cell-sdr-fracs (->> (get @cell-sdr-counts [region layer])
                                               (util/remap freqs->fracs))
                           sdr-label-fracs (->> (get AT-sdr-label-counts [region layer])
                                                (util/remap freqs->fracs))
                           response-c (async/chan)]
                       (put! @into-journal [:get-transitions-data
                                            model-id region layer cell-sdr-fracs response-c])
                       (go
                         (let [sdr-transitions (<! response-c)]
                           (reset! plot-data {:sdr-transitions sdr-transitions
                                              :sdr-label-fracs sdr-label-fracs
                                              :curr-sdr (get @curr-sdr [region layer])}))))))))
    (fn [_ _ _ _ _]
      [canvas
       {}
       300
       800
       [plot-data]
       (fn [ctx]
         (draw-transitions-plot! ctx @plot-data))
       nil]
      )))
