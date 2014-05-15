(ns comportex.viz
  (:require [org.nfrac.comportex.pooling :as p]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.util :as util]
            [strokes :refer [d3]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            goog.ui.Slider
            goog.ui.Component.EventType
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

(defn inputs-transform
  [xs]
  (mapv (fn [x]
          (mod (+ x 2) numb-max))
        xs))

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

(defn dense
  [is bits]
  (loop [bs (transient (vec (repeat bits false)))
         is is]
    (if (seq is)
      (recur (assoc! bs (first is) true)
             (rest is))
      (persistent! bs))))

(def input-state (atom (vec (repeat n-in-items (/ numb-max 2)))))
(def r-state (atom r-init))
(def sim-go? (atom false))
(def sim-step-ms (atom 1000))
(def animation-go? (atom false))
(def animation-step-ms (atom 1000))
(def display-options (atom {:display-active-columns true}))

(defn sim-step!
  []
  (let [newin (swap! input-state inputs-transform)
        newbits (efn newin)]
    (swap! r-state p/pooling-step newbits)))

(defn run-sim
  []
  (go
   (while @sim-go?
     (sim-step!)
     (<! (timeout @sim-step-ms)))))

;; GRAPHIC DISPLAY

(enable-console-print!)

(strokes/bootstrap)

(def width 800)
(def bitpx 5)
(def inbits-height (* bitpx bit-width))
(def rgn-height (* bitpx ncol))
(def height (max inbits-height rgn-height))

(def keep-steps 10)

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


(defn d3-draw-inbits [data t-now]
  ;; DATA JOIN
  (let [bits (-> inbits-svg (.selectAll "g.inbit")
                 (.data data (fn [[i t _]]
                               (str i "t" t))))
        inp-fill (fn [[_ _ on?]]
                   (if on? "grey" "white"))
        xform (fn [[i t _]]
                (let [tdiff (- t-now t)]
                  (str "translate(" (* tdiff bitpx) ","
                       (* i bitpx) ")")))]

    ;; UPDATE
    (-> bits
        (.attr {:transform xform})
        (.select "rect")
        (.style {:fill inp-fill
                 :stroke "black"}))

    ;; ENTER
    (-> bits (.enter)
        (.append "g")
        (.attr {:class "inbit"
                :transform xform})
        (.append "rect")
        (.attr {:width bitpx
                :height bitpx})
        (.style {:fill inp-fill
                 :stroke "black"}))

    ;; EXIT
    (-> bits (.exit)
        (.remove))))

(defn overlap-frac
  [x]
  (-> (/ (- x (:stimulus-threshold @r-state))
         10) ;; arbitrary scale
      (min 0.90)
      (max 0.10)))

(defn d3-draw-rgn
  [data t-now]
  ;; DATA JOIN
  (let [cols (-> rgn-svg (.selectAll "g.rgn")
                 (.data data (fn [[id t _]]
                               (str id "t" t))))
        col-fill (fn [[_ _ x]]
                   (cond
                    (= x :active) "red"
                    (= x :inactive) "white"
                    :else (-> d3 (.hsl 270 1 (- 1.0 (overlap-frac x))))))
        col-stroke (fn [[_ _ x]]
                     (cond
                      (= x :active) "black"
                      :else "grey"))
        xform (fn [[i t _]]
                (let [tdiff (- t-now t)]
                  (str "translate(" (* tdiff bitpx) ","
                       (* i bitpx) ")")))]
    ;; UPDATE
    (-> cols
        (.attr {:transform xform})
        (.select "circle")
        (.style {:fill col-fill
                 :stroke "black"}))

    ;; ENTER
    (-> cols (.enter)
        (.append "g")
        (.attr {:class "rgn"
                :transform xform})
        (.append "circle")
        (.attr {:r (/ bitpx 2)})
        (.style {:fill col-fill
                 :stroke "black"
                 :stroke-width 1}))

    ;; EXIT
    (-> cols (.exit)
        (.remove))))

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

(defn detail-text
  []
  (let [newin @input-state
        newbits (efn newin)
        newr @r-state
        newom (:overlaps newr)
        newac (:active-columns newr)]
    (apply str
           (interpose \newline
                      ["# Input"
                       newin
                       "# Input bits"
                       (sort newbits)
                       "# Active columns"
                       (sort newac)
                       "# Overlaps map"
                       (sort newom)]))))

;; use core.async to run simulation separately from animation

(defn listen [el type]
  (let [out (chan)]
    (events/listen el type
                   (fn [e] (put! out e)))
    out))

(defn handle-sim-ms
  [s]
  (let [changes (listen s goog.ui.Component.EventType/CHANGE)
        txt (dom/getElement "sim-ms-text")]
    (go (while true
          (let [e (<! changes)
                newval (reset! sim-step-ms (.getValue s))]
            (set! (.-innerHTML txt) newval))))))

(defn handle-animation-ms
  [s]
  (let [changes (listen s goog.ui.Component.EventType/CHANGE)
        txt (dom/getElement "animation-ms-text")]
    (go (while true
          (let [e (<! changes)
                newval (reset! animation-step-ms (.getValue s))]
            (set! (.-innerHTML txt) newval))))))

(defn init-ui!
  []
  (let [s-sim (doto (goog.ui.Slider.)
                (.setId "sim-ms-slider")
                (.setMaximum 2000)
                (.createDom)
                (.render (dom/getElement "sim-ms-slider-box")))
        s-anim (doto (goog.ui.Slider.)
                 (.setId "animation-ms-slider")
                 (.setMaximum 2000)
                 (.createDom)
                 (.render (dom/getElement "animation-ms-slider-box")))]
    (handle-sim-ms s-sim)
    (handle-animation-ms s-anim)
    (.setValue s-sim @sim-step-ms)
    (.setValue s-anim @animation-step-ms)))

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

(defn update-text-display
  []
  (let [ts-el (dom/getElement "sim-timestep")
        info-el (dom/getElement "detail-text")]
    (set! (.-innerHTML ts-el) (:timestep @r-state))
    (forms/setValue info-el (detail-text))))

(def d3-inbits-data-q (atom (vec (repeat keep-steps nil))))
(def d3-rgn-data-q (atom (vec (repeat keep-steps nil))))

(defn animation-step!
  []
  (let [newr @r-state
        t (:timestep newr)
        newbits (efn @input-state)
        newarray (dense newbits bit-width)
        newin-data (map list (range bit-width) (repeat t) newarray)
        in-data (swap! d3-inbits-data-q (fn [q]
                                          (conj (subvec q 1) newin-data)))
        o @display-options]
    (update-text-display)
    (d3-draw-inbits (apply concat in-data) t)
    (let [ac (:active-columns newr)
          om (:overlaps newr)
          am (zipmap ac (repeat :active))
          em (zipmap (map :id (:columns newr)) (repeat :inactive))
          m (cond-> em
                    (:display-overlap-columns o) (merge om)
                    (:display-active-columns o) (merge am))
          new-rgn-data (map list (keys m) (repeat t) (vals m))
          rgn-data (swap! d3-rgn-data-q (fn [q]
                                          (conj (subvec q 1) new-rgn-data)))]
      (d3-draw-rgn (apply concat rgn-data) t)
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
             "display-overlap-columns"]
        btns (map dom/getElement ids)
        cs (map listen btns (repeat "click"))
        cm (zipmap cs ids)]
    (doseq [[el id] (map vector btns ids)]
      (forms/setValue el (get @display-options (keyword id))))
    (go (while true
          (let [[e c] (alts! (keys cm))
                id (cm c)
                on? (forms/getValue (.-currentTarget e))]
            (swap! display-options assoc (keyword id) on?))))))

(init-ui!)
(handle-sim-control)
(handle-sim-step)
(handle-animation-control)
(handle-animation-step)
(handle-display-options)
