(ns comportex.viz
  (:require [org.nfrac.comportex.pooling :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [strokes :refer [d3]]
            [goog.dom :as dom]
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <! alts! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

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

(def input-state (atom (vec (repeat n-in-items (/ numb-max 2)))))
(def r-state (atom r-init))
(def sim-go? (atom false))
(def sim-step-ms (atom 1000))
(def animation-go? (atom false))
(def animation-step-ms (atom 1000))
(def display-options (atom {}))

(defn sim-step!
  []
  (let [newin (swap! input-state (partial add-noise 5))
        newbits (efn newin)]
    (println (str "inputs " newin))
    (println (str "bits " (sort newbits)))
    (let [newr (swap! r-state p/pooling-step newbits)
          newom (:overlaps newr)
          newac (:active-columns newr)]
      (println "ac " (sort newac))
      (println "om " (sort newom)))))

(defn run-sim
  []
  (go
   (while @sim-go?
     (sim-step!)
     (<! (timeout @sim-step-ms)))))

;; GRAPHIC DISPLAY

(enable-console-print!)

(strokes/bootstrap)

(def width 960)
(def bitpx 5)
(def inbits-height (* bitpx bit-width))
(def rgn-height (* bitpx ncol))
(def height (max inbits-height rgn-height))

(def inbits-svg
  (-> d3 (.select "#viz") (.append "svg")
      (.attr {:width width :height height})
      (.append "g")
      (.attr {:id "inbits"
              :transform (str "translate(32, 32)")})))

(def in-syn-svg
  (-> d3 (.select "#viz") (.select "svg")
      (.append "g")
      (.attr {:id "in-syn"
              :transform (str "translate(40, 32)")})))

(def rgn-svg
  (-> d3 (.select "#viz") (.select "svg")
      (.append "g")
      (.attr {:id "rgn"
              :transform (str "translate(240, 32)")})))


(defn d3-draw-inbits [data]
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
        (.style {:fill "grey"
                 :stroke "black"}))

    ;; EXIT
    (-> bits (.exit)
        (.select "rect")
        (.style {:fill "grey"}))))

(defn overlap-frac
  [x]
  (-> (/ (- x (:stimulus-threshold @r-state))
         10) ;; arbitrary scale
      (min 0.95)
      (max 0.05)))

(defn d3-draw-rgn [data]
  ;; DATA JOIN
  (let [cols (-> rgn-svg (.selectAll "g.rgn")
                 (.data data first))]
    ;; UPDATE
    (-> cols
        (.select "circle")
        (.attr {:r (/ bitpx 2)})
        (.style {:fill (fn [[_ x]]
                         (cond
                          (= x :active) "yellow"
                          (= x :inactive) "white"
                          :else (-> d3 (.hsl 0 1 (- 1.0 (overlap-frac x))))))}))

    ;; ENTER
    (-> cols (.enter)
        (.append "g")
        (.attr {:class "rgn"
                :transform #(str "translate(0," (* (first %) bitpx) ")")})
        (.append "circle")
        (.attr {:r 1})
        (.style {:fill "white"
                 :stroke "black"}))

    ;; EXIT
    (-> cols (.exit)
        (.select "circle")
        (.transition)
        (.duration 500)
        (.attr {:r 1})
        (.style {:fill "white"}))))

(defn d3-draw-insynapses [data]
  ;; DATA JOIN
  (let [elems (-> in-syn-svg (.selectAll "line.in-syn")
                  (.data data (fn [[[idx col] _]] (str idx "," col))))
        syn-stroke (fn [[[idx col] x]]
                     (cond
                      (= :active x) "yellow"
                      :else (-> d3 (.hsl 0.5 1 (- 1.0 x)))))
        syn-opacity (fn [[_ x]]
                      (if (= :active x) 1.0
                          0.5))]
    ;; UPDATE
    (-> elems
        (.style {:stroke syn-stroke
                 :stroke-opacity syn-opacity}))

    ;; ENTER
    (-> elems (.enter)
        (.append "line")
        (.attr {:class "in-syn"
                :transform (fn [[[idx col] x]]
                             (str "translate(0," (* idx bitpx) ")"))
                :x2 0
                :y2 0})
        (.style {:stroke syn-stroke
                 :stroke-opacity syn-opacity})
        (.transition)
        (.duration 500)
        (.attr {:x2 200
                :y2 (fn [[[idx col] x]]
                      (- (* col bitpx)
                         (* idx bitpx)))
                }))

    ;; EXIT
    (-> elems (.exit)
        (.transition)
        (.duration 500)
        (.attr {:x2 0})
        (.attr {:y2 0})
        (.remove))))

;; use core.async to run simulation separately from animation

(defn listen [el type]
  (let [out (chan)]
    (events/listen el type
                   (fn [e] (put! out e)))
    out))

(defn handle-sim-control
  []
  (let [btn (dom/getElement "sim-control")
        clicks (listen btn "click")]
    (go (while true
          (let [e (<! clicks)
                newval (swap! sim-go? not)]
            (set! (.-innerHTML (.-currentTarget e))
                  (if newval "Stop" "Start"))
            (when newval (run-sim)))))))

(defn handle-sim-step
  []
  (let [btn (dom/getElement "sim-step")
        clicks (listen btn "click")]
    (go (while true
          (<! clicks)
          (sim-step!)))))

(defn animation-step!
  []
  (let [newbits (efn @input-state)
        newr @r-state
        o @display-options]
    (d3-draw-inbits (vec newbits))
    (let [ac (:active-columns newr)
          om (:overlaps newr)
          am (zipmap ac (repeat :active))
          em (zipmap (map :id (:columns newr)) (repeat :inactive))
          m (cond-> {}
                    (:display-inactive-columns o) (merge em)
                    (:display-overlap-columns o) (merge om)
                    (:display-active-columns o) (merge am))]
      (d3-draw-rgn (vec m))
      (let [syn-cols (select-keys (:columns newr) (keys m))
            syn-data (->> syn-cols
                          (mapcat (fn [[col-id col]]
                                    (let [syns (cond-> {}
                                                       (:display-connected-insyns o)
                                                       (merge (-> col :in-synapses :connected))
                                                       (:display-disconnected-insyns o)
                                                       (merge (-> col :in-synapses :disconnected))
                                                       true
                                                       (select-keys newbits))]
                                      (map (fn [[in-id perm]]
                                             [[in-id col-id] perm])
                                           syns)))))]
        (d3-draw-insynapses syn-data)))))

(defn init-display
  []
  (d3-draw-inbits (vec (range bit-width)))
  (d3-draw-rgn (mapv vector (range ncol) (repeat :inactive))))

(defn run-animation
  []
  (go
   (while @animation-go?
     (animation-step!)
     (<! (timeout @animation-step-ms)))))

(defn handle-animation-control
  []
  (let [btn (dom/getElement "animation-control")
        clicks (listen btn "click")]
    (go (while true
          (let [e (<! clicks)
                newval (swap! animation-go? not)]
            (set! (.-innerHTML (.-currentTarget e))
                  (if newval "Stop" "Start"))
            (when newval (run-animation)))))))

(defn handle-animation-step
  []
  (let [btn (dom/getElement "animation-step")
        clicks (listen btn "click")]
    (go (while true
          (<! clicks)
          (animation-step!)))))

(defn handle-display-options
  []
  (let [ids ["display-connected-insyns"
             "display-disconnected-insyns"
             "display-active-columns"
             "display-overlap-columns"
             "display-inactive-columns"]
        btns (map dom/getElement ids)
        cs (map listen btns (repeat "click"))
        cm (zipmap cs ids)]
    (go (while true
          (let [[e c] (alts! (keys cm))
                id (cm c)
                on? (.-checked (.-currentTarget e))]
            (swap! display-options assoc (keyword id) on?))))))

;; TODO click on columns to filter display
;; TODO control to turn on all inputs (to see all synapses)
;; TODO allow stepping back in time
;; TODO text info output

(handle-sim-control)
(handle-sim-step)
(handle-animation-control)
(handle-animation-step)
(handle-display-options)
(init-display)
