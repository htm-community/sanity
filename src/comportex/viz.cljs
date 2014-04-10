(ns comportex.viz
  (:require [org.nfrac.comportex.pooling :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [strokes :refer [d3]]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(strokes/bootstrap)

;; initial CLA region
(def numb-bit-width 64)
(def numb-max 100)
(def numb-min 0)
(def numb-domain [numb-min numb-max])
(def numb-span 8)
(def n-in-items 3)
(def bit-width (* numb-bit-width n-in-items))
(def ncol 200)
(def r-init (p/region (assoc p/spatial-pooler-defaults
                        :ncol ncol
                        :input-size bit-width
                        :potential-radius (quot bit-width 5)
                        :global-inhibition false
                        :stimulus-threshold 2
                        :duty-cycle-period 100)))

(defn add-noise
  [delta xs]
  (mapv (fn [x]
          (-> (+ x (util/rand-int (- delta) (inc delta)))
              (min numb-max)
              (max numb-min)))
        xs))

(defn gen-ins
  []
  (repeatedly n-in-items #(util/rand-int numb-min numb-max)))

(def efn
  (enc/map-encoder numb-bit-width
                   (enc/number-linear numb-bit-width numb-domain numb-span)))


;; graphic display
(def width 960)
(def bitpx 5)
(def inbits-height (* bitpx bit-width))
(def rgn-height (* bitpx ncol))
(def height (max inbits-height rgn-height))

(def inbits-svg
  (-> d3 (.select "#viz-node") (.append "svg")
      (.attr {:width width :height height})
      (.append "g")
      (.attr {:id "inbits"
              :transform (str "translate(32, 32)")})))

(def in-syn-svg
  (-> d3 (.select "#viz-node") (.select "svg")
      (.append "g")
      (.attr {:id "in-syn"
              :transform (str "translate(40, 32)")})))

(def rgn-svg
  (-> d3 (.select "#viz-node") (.select "svg")
      (.append "g")
      (.attr {:id "rgn"
              :transform (str "translate(240, 32)")})))

(def input-state (atom (vec (repeat n-in-items (/ numb-max 2)))))
(def r-state (atom r-init))


(defn update-inbits [data]
  ;; DATA JOIN
  (let [bits (-> inbits-svg (.selectAll "g.inbit")
                 (.data data identity))]
    ;; UPDATE
    (-> bits
        (.select "rect")
        (.style {:fill "yellow"}))

    ;; ENTER
    (-> bits (.enter)
        (.append "g")
        (.attr {:class "inbit"
                :transform #(str "translate(0," (* % bitpx) ")")})
        (.append "rect")
        (.attr {:width bitpx
                :height bitpx})
        (.style {:fill "yellow"
                 :stroke "black"}))

    ;; EXIT
    (-> bits (.exit)
        (.select "rect")
        (.style {:fill "black"}))))

(defn overlap-frac
  [x]
  (-> (/ (- x (:stimulus-threshold @r-state))
         10) ;; arbitrary scale
      (min 0.95)
      (max 0.05)))

(defn update-rgn [data]
  ;; DATA JOIN
  (let [cols (-> rgn-svg (.selectAll "g.rgn")
                 (.data data first))]
    ;; UPDATE
    (-> cols
        (.select "circle")
        (.attr {:r (inc (/ bitpx 2))})
        (.style {:fill (fn [[_ x]]
                         (if (= :active x) "yellow"
                             (-> d3 (.hsl 0 1 (overlap-frac x)))))}))

    ;; ENTER
    (-> cols (.enter)
        (.append "g")
        (.attr {:class "rgn"
                :transform #(str "translate(0," (* (first %) bitpx) ")")})
        (.append "circle")
        (.attr {:r (inc (/ bitpx 2))})
        (.style {:fill "yellow"
                 :stroke "black"}))

    ;; EXIT
    (-> cols (.exit)
        (.select "circle")
        (.transition)
        (.duration 750)
        (.attr {:r 1})
        (.style {:fill "black"}))))

(defn update-in-synapses [data]
  ;; DATA JOIN
  (let [elems (-> in-syn-svg (.selectAll "line.in-syn")
                  (.data data (comp str first)))]
    ;; UPDATE
    (-> elems
        (.select "line")
        (.style {:stroke (fn [[[idx col] state]]
                         (if (= :active state) "yellow"
                             (-> d3 (.hsl 0.5 1 state))))}))

    ;; ENTER
    (-> elems (.enter)
        (.append "line")
        (.attr {:class "in-syn"
                :transform (fn [[[idx col] state]]
                             (str "translate(0," (* idx bitpx) ")"))
                :x2 0
                :y2 0})
        (.style {:stroke (fn [[[idx col] state]]
                         (if (= :active state) "yellow"
                             (-> d3 (.hsl 0.5 1 state))))})
        (.transition)
        (.duration 750)
        (.attr {:x2 200
                :y2 (fn [[[idx col] state]]
                      (- (* idx bitpx)
                         (* col bitpx)))
                }))

    ;; EXIT
    (-> elems (.exit)
        (.select "line")
        (.transition)
        (.duration 750)
        (.attr {:x2 0})
        (.attr {:y2 0})
        (.remove))))

(defn sim-step!
  []
  (let [newin (swap! input-state (partial add-noise 5))
        newbits (efn newin)]
    (println (str "inputs " newin))
    (println (str "bits " newbits))
    (let [newr (swap! r-state p/pooling-step newbits)
          newom (:overlaps newr)
          newac (:active-columns newr)]
      (println (str "ac " newac)))))

;; use core.async to run simulation separately from animation

(defn listen [el type]
  (let [out (chan)]
    (events/listen el type
                   (fn [e] (put! out e)))
    out))


(def sim-go? (atom false))
(def sim-step-ms (atom 1000))
(def anim-step-ms (atom 1000))

(defn run-sim
  []
  (go
   (while @sim-go?
     (sim-step!)
     (<! (timeout @sim-step-ms)))))

(defn handle-sim-control
  []
  (let [btn (dom/getElement "sim-control")
        clicks (listen btn "click")]
    (go (while true
          (<! clicks)
          (let [newval (swap! sim-go? not)]
            (set! (.-innerHTML btn)
                  (if newval "Stop sim" "Start sim"))
            (when newval (run-sim)))))))

(defn handle-sim-step
  []
  (let [btn (dom/getElement "sim-step")
        clicks (listen btn "click")]
    (go (while true
          (<! clicks)
          (sim-step!)))))

(defn anim-step
  []
  (let [newbits (efn @input-state)
        newr @r-state]
    (update-inbits (vec newbits))
    (let [ac (:active-columns newr)
          om (:overlaps newr)
          am (zipmap ac (repeat :active))
          st (merge om am)]
      (update-rgn (vec st))
      (let [syns (->> (select-keys (:columns newr) (keys om))
                      (mapcat (fn [[id col]]
                                (map (fn [[inp perm]]
                                       [[inp id] perm])
                                     (-> col :in-synapses :connected
                                         (select-keys newbits))))))]
        (update-in-synapses syns)))))

(defn run-animation
  []
  (go
   (while true
     (anim-step)
     (<! (timeout @anim-step-ms)))))

(handle-sim-control)
(handle-sim-step)
(run-animation)

