(ns comportexviz.plots
  (:require [c2.core :as c2]
            [c2.scale :as scale]
            [c2.svg :as svg]
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
  (let [margin {:left 20 :bottom 20}]
    (bind! el
     (let [{:keys [ts keep-n]} agg
           step-width (/ width keep-n)
           ncol (:ncol (peek ts))
           scale (scale/linear :domain [0 (* ncol 0.15)]
                               :range [0 (dec height)])]
       [:div
        [:style {:type "text/css"}
         (apply str "g.timestep rect { stroke-width: 0px; }"
                (for [k series-keys]
                  (str "." (name k) " { fill: " (series-colors k) "}")))]
        ;; the containing SVG element
        [:svg#main {:style {:display "block", :margin "auto"
                            :width (+ width (:left margin))
                            :height (+ height (:bottom margin))}}
         [:g.plot-space {:transform (str (svg/translate [(:left margin) height])
                                         ;; flip y coordinates to
                                         ;; have origin at bottom
                                         " " (svg/scale [1 -1]))}
          (c2/unify
           (map-indexed vector ts)
           (fn [[i x]]
             (let [vals (map x series-keys)
                   cums (reductions + 0 vals)]
               (into
                [:g.timestep {:transform (svg/translate [(inc (* i step-width)) 0])}]
                (for [[k val cum] (map vector series-keys vals cums)]
                  [:rect {:class k
                          :x 0, :y (scale cum)
                          :width step-width
                          :height (scale val)}]))))
           :key-fn (comp :timestep second))]]]))))
