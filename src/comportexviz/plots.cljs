(ns comportexviz.plots
  (:require [c2.core :as c2]
            [c2.scale :as scale]
            [c2.svg :as svg]
            [c2.ticks :as ticks]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [c2.util :refer [bind!]]))

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

(defn bind-ts-plot
  [el agg width height series-keys series-colors]
  (let [margin {:left 50 :right 30 :bottom 50 :top 20}]
    (bind! el
     (let [{:keys [ts keep-n bucket]} agg
           n-timesteps (* bucket keep-n)
           ncol (:ncol (peek ts))
           v-max (* ncol 0.10)
           h-scale (scale/linear :domain [0 n-timesteps]
                                 :range [0 (dec width)])
           h-extent h-scale
           h-ticks (:ticks (ticks/search (:domain h-scale)
                                         :length width))
           v-scale (scale/linear :domain [0 v-max]
                                 :range [(dec height) 0])
           v-extent (scale/linear :domain [0 v-max]
                                  :range [0 (dec height)])
           v-ticks (:ticks (ticks/search (:domain v-scale)
                                         :length (* height 2.5) ; more ticks
                                         ))]
       (when (pos? ncol)
         [:div
          [:style {:type "text/css"}
           (apply str
                  "g.timestep rect { stroke-width: 0px; }"
                  ".plot-space line { stroke: black; }"
                  ".plot-space text { font-size: 80%; }"
                  (for [k series-keys]
                    (str "." (name k) " { fill: " (series-colors k) "}")))]
          ;; the containing SVG element
          [:svg {:style {:display "block", :margin "auto"
                         :width (+ width (:left margin) (:right margin))
                         :height (+ height (:bottom margin) (:top margin))}}
           [:g.plot-space {:transform (svg/translate [(:left margin) (:top margin)])}
            (svg/axis v-scale v-ticks :orientation :left :text-margin 28
                      :label "n. columns" :label-margin 35)
            [:g {:transform (svg/translate [0 height])}
             (svg/axis h-scale h-ticks :orientation :bottom :text-margin 18
                       :label "timestep" :label-margin 35)]
            [:g
             (c2/unify
              (map-indexed vector ts)
              (fn [[i x]]
                (let [vals (map x series-keys)
                      cums (reductions + vals)]
                  (into [:g.timestep]
                        (for [[k val cum] (map vector series-keys vals cums)]
                          [:rect {:class k
                                  :x (h-scale (* i bucket))
                                  :y (v-scale cum)
                                  :width (h-extent bucket)
                                  :height (v-extent val)}]))))
              :key-fn (comp :timestep second))]]]]))))
)
