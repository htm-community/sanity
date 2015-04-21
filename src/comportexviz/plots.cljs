(ns comportexviz.plots
  (:require [reagent.core :as reagent :refer [atom]]
            [monet.canvas :as c]
            [comportexviz.plots-canvas :as plt]

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
