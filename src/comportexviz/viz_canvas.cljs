(ns comportexviz.viz-canvas
  (:require [goog.dom :as dom]
            [goog.dom.forms :as forms]
            goog.ui.Slider
            goog.ui.Component.EventType
            [goog.events :as events]
            [cljs.core.async :refer [put! chan <! alts! timeout]]
            [clojure.core.rrb-vector :as fv]
            [monet.canvas :as c]
            [monet.core])
  (:require-macros [cljs.core.async.macros :refer [go]]))


(def sim-chan (chan))

(def sim-go? (atom false))
(def sim-step-ms (atom 500))
(def animation-go? (atom false))
(def animation-step-ms (atom 500))
(def display-options (atom {:display-active-columns true}))

;; keep recent time steps
(def keep-steps (atom 25))
(def steps (atom []))

(defn i->dt
  [i]
  (- (count @steps) i 1))

(def dt->i i->dt)

(defn- take-last-v
  [n v]
  (fv/subvec v (max 0 (- (count v) n))))

(defn take-step!
  []
  (go
   (let [x (<! sim-chan)]
     (swap! steps
            (fn [q]
              (->> (conj q x)
                   (take-last-v @keep-steps)))))))

(def selected-cid (atom nil))
(def selected-dt (atom 0))

(defn run-sim
  []
  (go
   (while @sim-go?
     (take-step!)
     (<! (timeout @sim-step-ms)))))

;; GRAPHIC DISPLAY

(enable-console-print!)

(def width-px 800)
(def height-px 1100)
(def bit-grid-px 5)
(def col-grid-px 5)
(def fill% 1)
(def bit-w-px (dec (* fill% bit-grid-px)))
(def bit-r-px (* bit-w-px 0.5))
(def col-r-px (* fill% col-grid-px 0.5))

(def canvas-dom (dom/getElement "viz"))
;; need to set canvas size in js not CSS, the latter delayed so
;; get-context would see the wrong resolution here.
(set! (.-width canvas-dom) width-px)
(set! (.-height canvas-dom) height-px)

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

(defn rgn-px-offset
  []
  (* @keep-steps bit-grid-px 1.3))

(defn column->px
  "Returns pixel coordinates on the canvas `[x-px y-px]` for the
   center of a column `cid` at time delay `dt`."
  [cid dt]
  (let [left (rgn-px-offset)
        width (* @keep-steps col-grid-px)
        right (+ left width)
        off-x-px (* (+ dt 0.5) col-grid-px)
        x-px (- right off-x-px)
        y-px (* (+ cid 0.5) col-grid-px)]
    [x-px y-px]))

(defn px->column
  "Returns column id and time delay `[cid dt]` located by given pixel
   coordinates on the canvas. Otherwise nil."
  [x-px y-px]
  (let [left (rgn-px-offset)
        width (* @keep-steps col-grid-px)
        right (+ left width)
        cid (Math/floor (/ y-px col-grid-px))
        dt (Math/floor (/ (- right x-px) col-grid-px))]
    (when (and (<= 0 dt (count @steps))
               (<= 0 cid))
      [cid dt])))

(defn inbit->px
  "Returns pixel coordinates on the canvas `[x-px y-px]` for the
   center of an input bit `id` at time delay `dt`."
  [id dt]
  (let [width (* @keep-steps bit-grid-px)
        right width
        off-x-px (* (+ dt 0.5) bit-grid-px)
        x-px (- right off-x-px)
        y-px (* (+ id 0.5) bit-grid-px)]
    [x-px y-px]))

(defn px->inbit
  "Returns input bit id and time delay `[id dt]` located by given
   pixel coordinates on the canvas. Otherwise nil."
  [x-px y-px]
  (let [width (* @keep-steps bit-grid-px)
        right width
        id (Math/floor (/ y-px bit-grid-px))
        dt (Math/floor (/ (- right x-px) bit-grid-px))]
    (when (and (<= 0 dt (count @steps))
               (<= 0 id))
      [id dt])))

(defn draw-inbits
  [ctx data bit-width sel-dt]
  (c/save ctx)
  (c/stroke-width ctx 1)
  (c/stroke-style ctx "#000")
  (doseq [dt (range (count data))
          :let [bits (data (dt->i dt))
                alph (if (= sel-dt dt) 1 0.5)]]
    (c/alpha ctx alph)
    (c/fill-style ctx "#f00")
    (doseq [b bits
            :let [[x-px y-px] (inbit->px b dt)]]
      (c/fill-rect ctx {:x (- x-px bit-r-px)
                        :y (- y-px bit-r-px)
                        :w bit-w-px
                        :h bit-w-px}))
    (doseq [b (range bit-width)
            :let [[x-px y-px] (inbit->px b dt)]]
      (c/stroke-rect ctx {:x (- x-px bit-r-px)
                          :y (- y-px bit-r-px)
                          :w bit-w-px
                          :h bit-w-px})))
  (c/restore ctx)
  ctx)

(defn draw-rgn
  [ctx data ncol sel-cid sel-dt]
  ;; Originally this used translate and scale to draw grid on integer
  ;; coordinates. But better to use px lookup functions so that we can
  ;; draw between frames of reference: inbits & columns.
  (c/save ctx)
  (c/stroke-width ctx 1)
  (c/stroke-style ctx "#000")
  (doseq [dt (range (count data))
          :let [m (data (dt->i dt))
                alph (if (and (= sel-dt dt)
                              (not sel-cid)) 1 0.5)]]
    (c/alpha ctx alph)
    (doseq [cid (range ncol)
            :let [[x-px y-px] (column->px cid dt)
                  cval (m cid :inactive)
                  sel? (and (= sel-dt dt)
                            (= sel-cid cid))
                  color (case cval
                          :inactive "#fff"
                          :active "#f00"
                          (->> (/ cval 10)
                               (min 1.0)
                               (- 1)
                               (greyhex)))]]
      (when sel?
        (c/alpha ctx 1))
      (c/fill-style ctx color)
      (c/circle ctx {:x x-px :y y-px :r col-r-px})
      (c/stroke ctx)
      (when sel?
        (c/alpha ctx alph))))
  (c/restore ctx)
  ctx)

(defn draw-insynapses
  [ctx data]
  (c/save ctx)
  (c/stroke-width ctx 1)
  (doseq [[cid dt m] data
          :let [[cx cy] (column->px cid dt)
                perms? (:display-insyns-permanences @display-options)
                draw (fn [syns active?]
                       (c/stroke-style ctx (if active? "#f00" "#000"))
                       (doseq [[id perm] syns
                               :let [[ix iy] (inbit->px id dt)]]
                         (doto ctx
                           (c/alpha (if perms? perm 1))
                           (c/begin-path)
                           (c/move-to cx cy)
                           (c/line-to ix iy)
                           (c/stroke)))
                       )]]
    (draw (:inactive m) false)
    (draw (:active m) true))
  (c/restore ctx)
  ctx)

(defn detail-text
  [state dt cid]
  (let [in (:input state)
        bits (:inbits state)
        r (:region state)
        ac (:active-columns r)]
    (apply str
           (interpose \newline
                      ["__Selection__"
                       (str "  * timestep " (:timestep r))
                       (str "  * delay " dt)
                       (str "  * column " (or cid "nil"))
                       ""
                       "__Input__"
                       in
                       ""
                       "__Input bits__"
                       (sort bits)
                       ""
                       "__Active columns__"
                       (sort ac)]))))

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
                (.setMaximum 1000)
                (.createDom)
                (.render (dom/getElement "sim-ms-slider-box")))
        s-anim (doto (goog.ui.Slider.)
                 (.setId "animation-ms-slider")
                 (.setMaximum 1000)
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
  (let [curr-t (:timestep (:region (peek @steps)))
        view-state (nth @steps (dt->i @selected-dt))
        ts-el (dom/getElement "sim-timestep")
        info-el (dom/getElement "detail-text")]
    (set! (.-innerHTML ts-el) curr-t)
    (forms/setValue info-el (detail-text view-state @selected-dt @selected-cid))))

(defn rgn-column-states
  [rgn opts]
  (let [om (:overlaps rgn)
        am (zipmap (:active-columns rgn) (repeat :active))
        pm (delay nil)] ;; TODO
    (cond-> (if (:display-overlap-columns opts) om {})
            (:display-predictive-columns opts) (merge @pm)
            (:display-active-columns opts) (merge am))))

(defn in-synapse-display-data
  [cid dt opts]
  (when (:display-active-insyns opts)
    (let [x (@steps (dt->i dt))
          col (get-in x [:region :columns cid])
          on-bits (:inbits x)
         syns (-> col :in-synapses :connected)
         m {:active (select-keys syns on-bits)
            :inactive (when (:display-inactive-insyns opts)
                        (apply dissoc syns on-bits))}]
     [cid dt m])))

(defn animation-step!
  []
  (let [x (peek @steps)
        curr (:region x)
        bit-width (:input-size (:spec curr))
        ncol (count (:columns curr))
        dt @selected-dt
        view-r (get-in @steps [(dt->i dt) :region])
        rgn-data (mapv (comp rgn-column-states :region) @steps)
        view-cids (if @selected-cid
                    [@selected-cid]
                    (:active-columns view-r))
        view-syn-data (map in-synapse-display-data view-cids (repeat dt))]
    (update-text-display)
    (c/clear-rect canvas-ctx {:x 0 :y 0 :w width-px :h height-px})
    (draw-inbits canvas-ctx (mapv :inbits @steps) bit-width dt)
    (draw-rgn canvas-ctx rgn-data ncol @selected-cid dt)
    (draw-insynapses canvas-ctx view-syn-data)))

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
  (let [ids ["display-active-columns"
             "display-overlap-columns"
             "display-predictive-columns"
             "display-active-insyns"
             "display-inactive-insyns"
             "display-insyns-permanences"
             "display-active-dendrites"]
        btns (map dom/getElement ids)
        cs (map listen btns (repeat "click"))
        cm (zipmap cs ids)]
    (doseq [[el id] (map vector btns ids)]
      (forms/setValue el (get @display-options (keyword id))))
    (go (while true
          (let [[e c] (alts! (keys cm))
                id (cm c)
                on? (forms/getValue (.-currentTarget e))]
            (swap! display-options assoc (keyword id) on?)
            (animation-step!))))))

(defn handle-canvas-clicks
  []
  (let [clicks (listen canvas-dom "click")]
    (go
     (while true
       (let [e (<! clicks)
             x (.-offsetX e)
             y (.-offsetY e)]
         (if-let [[cid dt] (px->column x y)]
           ;; column clicked
           (do
             (reset! selected-cid cid)
             (reset! selected-dt dt))
           (if-let [[id dt] (px->inbit x y)]
             ;; in-bit clicked
             (do
               (reset! selected-cid nil)
               (reset! selected-dt dt))
             ;; nothing clicked
             (do
               (reset! selected-cid nil)
               (reset! selected-dt 0))))
         (animation-step!))))))

(defn init-ui!
  []
  (handle-sliders)
  (handle-sim-control)
  (handle-sim-step)
  (handle-animation-control)
  (handle-animation-step)
  (handle-display-options)
  (handle-canvas-clicks))

(init-ui!)
