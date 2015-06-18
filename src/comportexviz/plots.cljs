(ns comportexviz.plots
  (:require [reagent.core :as reagent :refer [atom]]
            [monet.canvas :as c]
            [comportexviz.plots-canvas :as plt]
            [comportexviz.helpers :refer [canvas]]
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
  [steps region-key layer-id series-colors]
  (let [series-keys [:active :active-predicted :predicted]
        step-freqs (atom nil)
        agg-freqs-ts (aggregating-ts step-freqs 200)]
    (add-watch steps [:calc-freqs region-key layer-id] ;; unique key per layer
               (fn [_ _ _ v]
                 (let [htm (first v)
                       freqs (-> htm :regions region-key
                                 (core/column-state-freqs layer-id))]
                   (reset! step-freqs freqs))))
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
  [htm]
  (let [srcs (concat (core/input-keys htm)
                     (core/region-keys htm))]
    (zipmap srcs (range -0.3 0.31 (/ 1.0 (count srcs))))))

(defn- abs [x] (if (neg? x) (- x) x))

(defn draw-cell-excitation-plot!
  [ctx htm prior-htm rgn-id lyr-id series-colors]
  (let [width-px (.-width (.-canvas ctx))
        height-px (.-height (.-canvas ctx))
        plot-size {:w width-px
                   :h 200}
        lc (get-in htm [:regions rgn-id lyr-id :state :learn-cells])
        breakdowns (core/cell-excitation-breakdowns htm prior-htm rgn-id lyr-id
                                                    lc)
        src-shades (viz-rgn-shades htm)
        y-max (* 1.1 (apply max (map :total (vals breakdowns))))
        x-lim [-0.5 (+ (count lc) 3)] ;; space for legend
        y-lim [y-max 0]
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
                                    (sort-by (comp :total val) >)
                                    (map-indexed vector))
              :let [x-coord i
                    [col _] cell-id
                    total-exc (:total bd)]]
        (draw-cell-bar plot x-coord bd false)
        (c/fill-style ctx "black")
        (plt/text! plot x-coord (+ total-exc 0.5) total-exc)
        (plt/text-rotated! plot x-coord -1 col))
      ;; draw legend
      (let [leg-x (+ 1 (count lc))
            sep-x (count lc)
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
        (plt/text-rotated! plot leg-x -1 "KEY")
        (plt/frame! plot)))
    (c/restore ctx)))

(defn cell-excitation-plot-cmp
  [steps selection series-colors region-key layer-id]
  [canvas
   {}
   300
   240
   [steps selection]
   (fn [ctx]
     (let [dt (:dt @selection)
           htm (nth @steps dt)
           prior-htm (nth @steps (inc dt))]
       (draw-cell-excitation-plot! ctx htm prior-htm region-key layer-id
                                   series-colors)))
   nil])
