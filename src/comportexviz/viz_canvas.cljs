(ns comportexviz.viz-canvas
  (:require [goog.dom :as dom]
            [goog.dom.forms :as forms]
            goog.ui.Slider
            goog.ui.Component.EventType
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <! alts! timeout]]
            [monet.canvas :as c]
            [monet.core])
  (:require-macros [cljs.core.async.macros :refer [go]]))


(def sim-chan (chan))

(def sim-go? (atom false))
(def sim-step-ms (atom 1000))
(def animation-go? (atom false))
(def animation-step-ms (atom 1000))
(def display-options (atom {:display-active-columns true}))

;; keep recent time steps
(def keep-steps (atom 50))
(def steps (atom []))

(defn- take-last-subvec
  [v n]
  (subvec v (max 0 (- (count v) n))))

(defn take-step!
  []
  (go
   (let [x (<! sim-chan)]
     (swap! steps (fn [q]
                     (-> (conj q x)
                         (take-last-subvec @keep-steps)))))))

(defn run-sim
  []
  (go
   (while @sim-go?
     (take-step!)
     (<! (timeout @sim-step-ms)))))

;; GRAPHIC DISPLAY

(enable-console-print!)

(def width 800)
(def bitpx 6)
(def fill% 0.9)

(def canvas-dom (dom/getElement "viz"))
;; need to set canvas size in js not CSS, the latter delayed so
;; get-context would see the wrong resolution here.
(set! (.-width canvas-dom) width)
(set! (.-height canvas-dom) 1000)

(def canvas-ctx (c/get-context canvas-dom "2d"))

(defn hexit
  [z]
  (let [i (.floor js/Math (* z 16))]
    (if (< i 10)
      (str i)
      (case i
        10 "a"
        11 "b"
        12 "c"
        13 "d"
        14 "e"
        "f"))))

(defn rgbhex
  [r g b]
  (str "#" (hexit r) (hexit g) (hexit b)))

(defn greyhex
  [z]
  (let [x (hexit z)]
    (str "#" x x x)))

(defn draw-inbits
  [ctx data bit-width]
  (c/save ctx)
  (c/scale ctx bitpx bitpx)
  (c/stroke-width ctx 0.05)
  (c/stroke-style ctx "#000")
  (doseq [dt (range (count data))
          :let [bits (data dt)]]
    (c/alpha ctx (/ dt (* (count data) 1.2)))
    (c/clear-rect ctx {:x dt :y 0 :w 1 :h bit-width})
    (c/fill-style ctx "#f00")
    (doseq [b (range bit-width)]
      (c/stroke-rect ctx {:x dt :y b :w fill% :h fill%}))
    (doseq [b bits]
      (c/fill-rect ctx {:x dt :y b :w fill% :h fill%})
      (c/stroke-rect ctx {:x dt :y b :w fill% :h fill%})))
  (c/restore ctx)
  ctx)

(defn rgn-column-states
  [rgn]
  (let [ac (:active-columns rgn)
        om (:overlaps rgn)
        am (zipmap ac (repeat :active))]
    (merge om am)))

(defn draw-rgn
  [ctx data ncol]
  (c/save ctx)
  (c/translate ctx (* @keep-steps bitpx 1.5) 0)
  (c/scale ctx bitpx bitpx)
  (c/stroke-width ctx 0.05)
  (c/stroke-style ctx "#000")
  (doseq [dt (range (count data))
          :let [m (data dt)]]
    (c/alpha ctx (/ dt (* (count data) 1.2)))
    (c/clear-rect ctx {:x dt :y 0 :w 1 :h (count m)})
    (c/fill-style ctx "#fff")
    (doseq [cid (range (count m))]
      (c/circle ctx {:x (+ 0.5 dt) :y (+ 0.5 cid) :r (* fill% 0.5)})
      (c/stroke ctx))
    (doseq [[cid cval] m]
      (let [color (case cval
                    :inactive "#fff"
                    :active "#f00"
                    (->> (/ cval 10)
                         (min 1.0)
                         (- 1)
                         (greyhex)))]
        (c/fill-style ctx color)
        (c/circle ctx {:x (+ 0.5 dt) :y (+ 0.5 cid) :r (* fill% 0.5)})
        (c/stroke ctx))))
  (c/restore ctx)
  ctx)

(defn draw-insynapses
  [data]
  )

(defn detail-text
  []
  (let [x (peek @steps)
        newin (:input x)
        newbits (:inbits x)
        newr (:region x)
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

(defn handle-sliders
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
          (take-step!)))))

(defn update-text-display
  []
  (let [rgn (:region (peek @steps))
        ts-el (dom/getElement "sim-timestep")
        info-el (dom/getElement "detail-text")]
    (set! (.-innerHTML ts-el) (:timestep rgn))
    (forms/setValue info-el (detail-text))))

(defn animation-step!
  []
  (let [x (peek @steps)
        newin (:input x)
        newbits (:inbits x)
        newr (:region x)
        t (:timestep newr)
        bit-width (:input-size (:spec newr))
        o @display-options]
    (update-text-display)
    (draw-inbits canvas-ctx (mapv :inbits @steps) bit-width)
    (let [rgn-data (mapv (comp rgn-column-states :region) @steps)]
      (draw-rgn canvas-ctx rgn-data t)
      (let [ac (:active-columns newr)
            syn-cols (select-keys (:columns newr) ac)
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
        (draw-insynapses syn-data)))))

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

(defn init-ui!
  []
  (handle-sliders)
  (handle-sim-control)
  (handle-sim-step)
  (handle-animation-control)
  (handle-animation-step)
  (handle-display-options))

(init-ui!)
