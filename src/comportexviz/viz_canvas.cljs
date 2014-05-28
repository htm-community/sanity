(ns comportexviz.viz-canvas
  (:require [goog.dom :as dom]
            [goog.dom.forms :as forms]
            goog.ui.Slider
            goog.ui.Component.EventType
            [goog.events :as events]
            [goog.string :as gstring]
            [goog.string.format]
            [clojure.core.rrb-vector :as fv]
            [monet.canvas :as c]
            [monet.core]
            [org.nfrac.comportex.sequence-memory :as sm]
            [comportexviz.mq :as mq]
            [cljs.core.async :refer [chan put! <! alts! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def sim-go? (atom false))
(def sim-step-ms (atom 500))
(def animation-go? (atom false))
(def animation-step-ms (atom 500))
(def display-options (atom {:display-active-columns true}))

(defn wrap
  [x]
  (let [pcr (delay (sm/predictive-cells (:region x)))]
    (assoc-in x [:region :predictive-cells-ref] pcr)))

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
   (let [x (<! mq/sim-channel)
         xw (wrap x)]
     (swap! steps
            (fn [q]
              (->> (conj q xw)
                   (take-last-v @keep-steps)))))))

(def selected-cid (atom nil))
(def selected-dt (atom 0))

(defn run-sim
  []
  (go
   (while @sim-go?
     (take-step!)
     (<! (timeout @sim-step-ms)))))

;; ## Graphic Display

(enable-console-print!)

(def width-px 800)
(def height-px 1100)
(def bit-grid-px 5)
(def col-grid-px 5)
(def bit-w-px (- bit-grid-px 1))
(def bit-r-px (* bit-w-px 0.5))
(def col-r-px (* col-grid-px 0.5))
(def head-px 10)

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
        off-y-px (* (+ cid 0.5) col-grid-px)
        y-px (+ head-px off-y-px)]
    [x-px y-px]))

(defn px->column
  "Returns column id and time delay `[cid dt]` located by given pixel
   coordinates on the canvas. Otherwise nil."
  [x-px y-px]
  (let [left (rgn-px-offset)
        width (* @keep-steps col-grid-px)
        right (+ left width)
        cid (Math/floor (/ (- y-px head-px) col-grid-px))
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
        off-y-px (* (+ id 0.5) bit-grid-px)
        y-px (+ head-px off-y-px)]
    [x-px y-px]))

(defn px->inbit
  "Returns input bit id and time delay `[id dt]` located by given
   pixel coordinates on the canvas. Otherwise nil."
  [x-px y-px]
  (let [width (* @keep-steps bit-grid-px)
        right width
        id (Math/floor (/ (- y-px head-px) bit-grid-px))
        dt (Math/floor (/ (- right x-px) bit-grid-px))]
    (when (and (<= 0 dt (count @steps))
               (<= 0 id))
      [id dt])))

(defn square
  [cx cy r]
  {:x (- cx r)
   :y (- cy r)
   :w (* 2 r)
   :h (* 2 r)})

(defn draw-inbits
  [ctx data bit-width sel-dt]
  (c/save ctx)
  (c/stroke-width ctx 1)
  (c/stroke-style ctx "#000")
  (doseq [dt (range (count data))
          :let [bits (data (dt->i dt))
                alph (if (= sel-dt dt) 1 0.5)]]
    (c/alpha ctx alph)
    (doseq [b (range bit-width)
            :let [[x-px y-px] (inbit->px b dt)
                  color (if (bits b)
                          "#f00"
                          "#fff")]]
      (c/fill-style ctx color)
      (c/fill-rect ctx (square x-px y-px bit-r-px))
      (c/stroke-rect ctx (square x-px y-px bit-r-px))))
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
                          :active "#0a4"
                          :predictive "#88f"
                          :bursting "#f00"
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
  ;; draw axis on selection: vertical dt and horizonal cid
  (c/fill-style ctx "#000")
  (c/alpha ctx 1.0)
  (let [pts [(column->px -1 sel-dt)
             (column->px ncol sel-dt)]]
    (doseq [[x-px y-px] pts]
      (c/fill-rect ctx (square x-px y-px col-r-px))))
  (when sel-cid
    (let [pts [(column->px sel-cid -1)
               (column->px sel-cid (count data))]]
      (doseq [[x-px y-px] pts]
        (c/fill-rect ctx (square x-px y-px col-r-px)))))
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
                           (c/move-to (- cx 1) cy) ;; -1 avoid obscuring colour
                           (c/line-to ix iy)
                           (c/stroke)))
                       )]]
    (draw (:inactive m) false)
    (draw (:active m) true))
  (c/restore ctx)
  ctx)

(defn draw-dendrites
  [ctx data]
  (c/save ctx)
  (c/stroke-width ctx 1)
  (doseq [[cid dt m] data
          :let [[cx cy] (column->px cid dt)
                perms? (:display-dendrite-permanences @display-options)
                draw (fn [syns active?]
                       (c/stroke-style ctx (if active? "#f00" "#000"))
                       (doseq [[[id _] perm] syns
                               :let [[dcx dcy] (column->px id (inc dt))
                                     mid-y (quot (+ cy dcy) 2)
                                     diff-y (Math/abs (- cy dcy))]]
                         (doto ctx
                           (c/alpha (if perms? perm 1))
                           (c/begin-path)
                           (c/move-to (+ cx 1) cy) ;; avoid obscuring colour
                           (c/line-to (+ cx 10) cy)
                           (c/quadratic-curve-to (+ cx (quot diff-y 2))
                                                 mid-y
                                                 dcx
                                                 dcy)
                           (c/stroke)))
                       )]]
    (draw (:inactive m) false)
    (draw (:active m) true))
  (c/restore ctx)
  ctx)

(declare dendrite-display-data)

(defn detail-text
  []
  (let [dt @selected-dt
        cid @selected-cid
        state (@steps (dt->i dt))
        in (:input state)
        bits (:inbits state)
        rgn (:region state)
        ac (:active-columns rgn)]
    (->>
     ["__Selection__"
      (str "* timestep " (:timestep rgn)
           " (delay " dt ")")
      (str "* column " (or cid "nil"))
      ""
      "__Input__"
      (str in)
      ""
      "__Input bits__"
      (str (sort bits))
      ""
      "__Active columns__"
      (str (sort ac))
      ""
      "__Active cells__"
      (str (sort (:active-cells rgn)))
      ""
      (if cid
        (let [dtp (inc dt)
              x (@steps (dt->i dtp))
              pcol (get-in x [:region :columns cid])
              prgn (:region x)
              pcells (:active-cells prgn)
              best (sm/best-matching-segment-and-cell pcol pcells (:spec rgn))
              col (get-in rgn [:columns cid])]
          ["__Selected column__"
           "__Best matching segment and cell_"
           (str best)
           "__Cells and their Dendrite segments__"
           (->> (:cells pcol)
                (map-indexed
                 (fn [i cell]
                   (let [ds (:segments cell)]
                     [(str "CELL " i)
                      (str (count ds) " = "
                           (sort (map (comp count :synapses) ds)))
                      (for [i (range (count ds))
                            :let [syns (:synapses (ds i))]]
                        [(str "  SEGMENT " i)
                         (for [s (sort syns)]
                           (str "  " (key s) " :=> "
                                (gstring/format "%.2f" (val s))))])
                      ]))))
           ]))]
     (flatten)
     (interpose \newline)
     (apply str))))

(defn update-text-display
  []
  (let [info-el (dom/getElement "detail-text")]
    (forms/setValue info-el (detail-text))))

(defn update-timestep
  []
  (let [ts-el (dom/getElement "sim-timestep")
        curr-t (:timestep (:region (peek @steps)))]
    (set! (.-innerHTML ts-el) curr-t)))

(defn rgn-column-states
  [rgn prev-rgn]
  (let [opts @display-options
        om (:overlaps rgn)
        pm (delay (when prev-rgn
                    (zipmap (keys (deref (:predictive-cells-ref prev-rgn)))
                            (repeat :predictive))))
        am (zipmap (:active-columns rgn) (repeat :active))
        bm (zipmap (:bursting-columns rgn) (repeat :bursting))]
    (cond-> (if (:display-overlap-columns opts) om {})
            (:display-predictive-columns opts) (merge @pm)
            (:display-active-columns opts) (merge am)
            (:display-bursting-columns opts) (merge bm))))

(defn in-synapse-display-data
  [cid dt]
  (let [opts @display-options]
    (when (:display-active-insyns opts)
      (let [x (@steps (dt->i dt))
            col (get-in x [:region :columns cid])
            on-bits (:inbits x)
            syns (-> col :in-synapses :connected)
            m {:active (select-keys syns on-bits)
               :inactive (when (:display-inactive-insyns opts)
                           (apply dissoc syns on-bits))}]
        [cid dt m]))))

(defn dendrite-display-data
  [cid dt]
  (let [opts @display-options]
    (when (:display-active-dendrites opts)
      ;; logically need to use prev region, since it is prior to
      ;; learning that occurred at time dt.
      ;; also need its current active cells that are the
      ;; PREVIOUSLY active cells at dt.
      (let [dtp (inc dt)
            x (@steps (dt->i dtp))
            col (get-in x [:region :columns cid])
            rgn (:region x)
            pcon (-> rgn :spec :connected-perm)
            on-cells (:active-cells rgn)
            best (sm/best-matching-segment-and-cell col on-cells (:spec rgn))
            m (if-let [sid (:segment-idx best)]
                (let [[_ cell-idx] (:cell-id best)
                      allsyns (get-in col [:cells cell-idx :segments sid :synapses])
                      syns (into {} (filter (fn [[id p]] (>= pcon p)) allsyns))]
                  {:active (select-keys syns on-cells)
                   :inactive (when (:display-dendrites-inactive opts)
                               (apply dissoc syns on-cells))})
                {})]
        [cid dt m]))))

(defn animation-step!
  []
  (let [x (peek @steps)
        curr (:region x)
        bit-width (:input-size (:spec curr))
        ncol (count (:columns curr))
        dt @selected-dt
        view-r (get-in @steps [(dt->i dt) :region])
        rs (map :region @steps)
        rgn-data (mapv rgn-column-states rs (cons nil rs))
        view-cids (if @selected-cid
                    [@selected-cid]
                    (:active-columns view-r))
        view-syn-data (map in-synapse-display-data view-cids (repeat dt))
        view-dendrite-data (map dendrite-display-data view-cids (repeat dt))]
    (update-text-display)
    (c/clear-rect canvas-ctx {:x 0 :y 0 :w width-px :h height-px})
    (draw-inbits canvas-ctx (mapv :inbits @steps) bit-width dt)
    (draw-rgn canvas-ctx rgn-data ncol @selected-cid dt)
    (draw-insynapses canvas-ctx view-syn-data)
    (draw-dendrites canvas-ctx view-dendrite-data)))

(defn run-animation
  []
  (go
   (while @animation-go?
     (monet.core/animation-frame animation-step!)
     (<! (timeout @animation-step-ms)))))


;; ## Event stream processing

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
             "display-bursting-columns"
             "display-active-insyns"
             "display-inactive-insyns"
             "display-insyns-permanences"
             "display-active-dendrites"
             "display-dendrites-inactive"
             "display-dendrite-permanences"]
        ids (filter dom/getElement ids)
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
  (handle-canvas-clicks)
  (add-watch steps :timestep (fn [_ _ _ _]
                               (update-timestep))))

(init-ui!)
