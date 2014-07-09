(ns comportexviz.viz-canvas
  (:require [c2.dom :as dom :refer [->dom]]
            [c2.event]
            [goog.events.EventType]
            [goog.events :as gevents]
            [goog.string :as gstring]
            [goog.string.format]
            [monet.canvas :as c]
            [monet.core]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.sequence-memory :as sm]
            [clojure.set :as set]
            [cljs.core.async :as async :refer [chan put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def steps (atom []))
(def keep-steps (atom 25))

(def viz-options
  (atom {:input {:active true
                 :predicted true}
         :columns {:overlaps nil
                   :predictive true}
         :ff-synapses {:active nil
                       :inactive nil
                       :permanences nil}
         :lat-synapses {:from :learning ;; :learning, :all, :none
                        :active true
                        :inactive nil
                        :permanences nil}}))

(def height-px 1400)
(def pad 0.85)
(def bit-w-px 5)
(def bit-h-px 2)
(def bit-h-px-pad (* bit-h-px pad))
(def bit-w-px-pad (* bit-w-px pad))
(def col-grid-px 5)
(def col-r-px (* col-grid-px 0.5))
(def col-r-px-pad (* col-r-px pad))
(def cell-r-px 10)
(def seg-r-px 15)
(def head-px 10)
(def h-spacing-px 80)

(defn hsl
  ([h s l] (hsl h s l 1.0))
  ([h s l a]
   (let [h2 (if (keyword? h)
              (case h
                :red 0
                :orange 30
                :yellow 60
                :yellow-green 90
                :green 120
                :blue 210
                :purple 270
                :pink 300)
              ;; otherwise angle
              h)]
     (str "hsla(" h2 ","
          (long (* s 100)) "%,"
          (long (* l 100)) "%,"
          a ")"))))

(defn sixteenths
  [z]
  (-> z (* 16) (long) (/ 16.0)))

(defn grey
  [z]
  (let [v (long (* z 255))]
    (str "rgb(" v "," v "," v ")")))

(def state-colors
  {:inactive "white"
   :active (hsl :red 1.0 0.5)
   :predicted (hsl :blue 1.0 0.5)
   :active-predicted (hsl :purple 1.0 0.4)
   :highlight (hsl :yellow 1 0.75 0.5)
   })

(defn rgns-x-offset
  []
  (+ (* @keep-steps bit-w-px)
     h-spacing-px))

(defn cells-x-offset
  []
  (+ (rgns-x-offset)
     (* @keep-steps col-grid-px)
     h-spacing-px))

(defn region-dt-offset
  "Returns the pixel offsets `[x-px y-px]` from the canvas origin for
   drawing the region columns at time delay `dt`."
  [dt]
  (let [left (rgns-x-offset)
        width (* @keep-steps col-grid-px)
        right (+ left width)
        off-x-px (* (+ dt 1) col-grid-px)
        x-px (- right off-x-px)
        y-px head-px]
    [x-px y-px]))

(defn column->px
  "Returns pixel coordinates `[x-px y-px]` for the centre of a
   region column `cid`, in local coordinates, or in absolute canvas
   coordinates if time delay `dt` is given."
  ([cid]
     (let [x-px col-r-px
           y-px (* (+ cid 0.5) col-grid-px)]
       [x-px y-px]))
  ([cid dt]
     (mapv + (region-dt-offset dt)
           (column->px cid))))

(defn px->column
  "Returns column id and time delay `[cid dt]` located by given pixel
   coordinates on the canvas. Otherwise nil."
  [x-px y-px]
  (let [left (rgns-x-offset)
        width (* @keep-steps col-grid-px)
        right (+ left width)
        cid (Math/floor (/ (- y-px head-px) col-grid-px))
        dt (Math/floor (/ (- right x-px) col-grid-px))]
    (when (and (<= 0 dt (count @steps))
               (<= 0 cid))
      [cid dt])))

(defn inbits-dt-offset
  "Returns the pixel offsets `[x-px y-px]` from the canvas origin for
   drawing the input bits at time delay `dt`."
  [dt]
  (let [width (* @keep-steps bit-w-px)
        right width
        off-x-px (* (+ dt 1) bit-w-px)
        x-px (- right off-x-px)
        y-px head-px]
    [x-px y-px]))

(defn inbit->px-top-left
  "Returns pixel coordinates `[x-px y-px]` for the top-left of an
   input bit `id`, in local coordinates, or in absolute canvas
   coordinates if time delay `dt` is given."
  ([id]
     (let [x-px 0
           y-px (* id bit-h-px)]
       [x-px y-px]))
  ([id dt]
     (mapv + (inbits-dt-offset dt)
           (inbit->px-top-left id))))

(defn inbit->px
  "Returns pixel coordinates on the canvas `[x-px y-px]` for the
   center of an input bit `id` at time delay `dt`."
  [id dt]
  (let [[x-px y-px] (inbit->px-top-left id dt)]
    [(+ x-px (/ bit-w-px 2))
     (+ y-px (/ bit-h-px 2))]))

(defn px->inbit
  "Returns input bit id and time delay `[id dt]` located by given
   pixel coordinates on the canvas. Otherwise nil."
  [x-px y-px]
  (let [width (* @keep-steps bit-w-px)
        right width
        id (Math/floor (/ (- y-px head-px) bit-h-px))
        dt (Math/floor (/ (- right x-px) bit-w-px))]
    (when (and (<= 0 dt (count @steps))
               (<= 0 id))
      [id dt])))

(defn centred-rect
  [cx cy w h]
  {:x (- cx (/ w 2))
   :y (- cy (/ h 2))
   :w w
   :h h})

(defn highlight-rect
  [ctx rect]
  (doto ctx
    (c/stroke-style (:highlight state-colors))
    (c/stroke-width 3)
    (c/stroke-rect rect)
    (c/stroke-style "black")
    (c/stroke-width 1)
    (c/stroke-rect rect)))

(defn draw-bits-1
  "Fills the input bits in given color.
   Draws with relative pixel positions, assuming the context has been
   transformed already."
  [ctx bits color]
  (let [w bit-w-px-pad
        h bit-h-px-pad]
    (c/begin-path ctx)
    (doseq [i bits
            :let [[x y] (inbit->px-top-left i)]]
      (.rect ctx x y w h))
    (c/fill-style ctx color)
    (c/fill ctx))
  ctx)

(defn draw-predbits-1
  "Draws with relative pixel positions, assuming the context has been
   transformed already."
  [ctx bit-votes]
  (let [w bit-w-px-pad
        h bit-h-px-pad]
    (c/begin-path ctx)
    (c/fill-style ctx (:predicted state-colors))
    (doseq [[i v] bit-votes
            :let [[x-px y-px] (inbit->px-top-left i)]]
      (c/alpha ctx (->> (/ v 8)
                        (min 1.0)
                        (* 0.5)))
      (.fillRect ctx x-px y-px w h)))
  ctx)

(defn draw-inbits-selection
  [ctx sel-dt bit-width]
  ;; draw axis on selection: vertical dt
  (let [[x y1] (inbit->px 0 sel-dt)
        [_ y2] (inbit->px (dec bit-width) sel-dt)
        y (/ (+ y1 y2) 2)
        w (+ 1 bit-w-px)
        h (+ 10 bit-h-px (- y2 y1))]
    (highlight-rect ctx (centred-rect x y w h))))

(defn circle
  [ctx x y r]
  (.arc ctx x y r 0 (* (.-PI js/Math) 2) true))

(defn column-color
  [col-states cid]
  (let [col-state (col-states cid :inactive)]
    (or (state-colors col-state)
        (->> (/ col-state 10)
             (min 1.0)
             (- 1)
             (sixteenths)
             (grey)))))

(defn draw-region-1
  "Draws with relative pixel positions, assuming the context has been
   transformed already."
  [ctx col-states ncol]
  (c/save ctx)
  (let [color-groups (group-by (partial column-color col-states)
                               (range ncol))]
    (doseq [[color cids] color-groups]
      (c/begin-path ctx)
      (doseq [cid cids
              :let [[x-px y-px] (column->px cid)]]
        (circle ctx x-px y-px col-r-px-pad))
      (c/fill-style ctx color)
      (c/fill ctx)))
  (c/restore ctx)
  ctx)

(defn draw-region-selection
  [ctx sel-dt sel-cid ncol]
  ;; draw axis on selection: vertical dt and horizonal cid
  (let [[x y1] (column->px 0 sel-dt)
        [_ y2] (column->px (dec ncol) sel-dt)
        y (/ (+ y1 y2) 2)
        w (+ 1 col-grid-px)
        h (+ 10 col-grid-px (- y2 y1))]
    (highlight-rect ctx (centred-rect x y w h)))
  (when sel-cid
    (let [[x1 y] (column->px sel-cid 0)
          [x2 _] (column->px sel-cid (dec @keep-steps))
          x (/ (+ x1 x2) 2)
          w (+ 10 col-grid-px (Math/abs (- x2 x1)))
          h (+ 1 col-grid-px)]
      (highlight-rect ctx (centred-rect x y w h))))
  ctx)

(defn draw-insynapses
  [ctx data dt opts]
  (c/save ctx)
  (c/stroke-width ctx 1)
  (doseq [[cid m] data
          :let [[cx cy] (column->px cid dt)
                perms? (get-in opts [:ff-synapses :permanences])
                draw (fn [syns active?]
                       (c/stroke-style ctx (if active? "red" "black"))
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

(defn group-synapses
  [syns ac pcon]
  (group-by (fn [[id p]]
                   [(if (>= p pcon)
                      :connected :disconnected)
                    (if (ac id)
                      :active :inactive)])
                 syns))

(defn column-learning-segment-and-cell
  [prev-rgn current-ac cid]
  (let [col-ac (set (filter (fn [[i _]] (= i cid))
                            current-ac))]
    (when (seq col-ac)
      (let [col (get-in prev-rgn [:columns cid])
            ac (:active-cells prev-rgn)
            lc (:learn-cells prev-rgn)
            bursting? (= (count col-ac)
                         (count (:cells col)))]
        (sm/column-learning-segment-and-cell col bursting? col-ac lc ac
                                             (:spec prev-rgn))))))

(defn natural-curve
  [ctx x0 y0 x1 y1]
  (let [x-third (/ (- x1 x0) 3)]
    (c/bezier-curve-to ctx
                       (- x1 x-third) y0
                       (+ x0 x-third) y1
                       x1 y1)))

(defn draw-all-segments
  [ctx cid dt prev-rgn rgn ncol opts]
  (c/save ctx)
  (let [col (get-in prev-rgn [:columns cid])
        spec (:spec prev-rgn)
        th (:activation-threshold (:spec rgn))
        pcon (:connected-perm spec)
        ac (:active-cells prev-rgn)
        lc (:learn-cells prev-rgn)
        active? (get-in rgn [:active-columns cid])
        bursting? (get-in rgn [:bursting-columns cid])
        current-ac (:active-cells rgn)
        learning (column-learning-segment-and-cell prev-rgn current-ac cid)
        cells (:cells col)
        ncells (count cells)
        nsegbycell (map (comp count :segments) cells)
        nsegbycell-pad (map (partial max 1) nsegbycell)
        nseg-pad (apply + nsegbycell-pad)
        our-left (cells-x-offset)
        segs-left (+ our-left seg-r-px (* cell-r-px 2))
        our-height (* 0.95 (.-innerHeight js/window))
        our-top (+ (.-pageYOffset js/window) cell-r-px)
        seg->px (fn [cell-idx idx]
                  (let [i-all (apply + idx (take cell-idx nsegbycell-pad))
                        frac (/ i-all nseg-pad)]
                    [(+ segs-left seg-r-px)
                     (+ our-top head-px (* frac our-height))]))
        cell->px (fn [cell-idx]
                   (let [[sx sy] (seg->px cell-idx 0)]
                     [our-left sy]))
        [colx coly] (column->px cid 0)
        stroke-col-to-cell (fn [ctx cell-idx]
                             (let [[cellx celly] (cell->px cell-idx)]
                               (doto ctx
                                 (c/begin-path)
                                 (c/move-to (+ colx col-r-px 1) coly) ;; avoid obscuring colour
                                 (natural-curve colx coly cellx celly)
                                 (c/stroke))))
        stroke-seg-to-cell (fn [ctx cell-idx idx]
                             (let [[cellx celly] (cell->px cell-idx)
                                   [sx sy] (seg->px cell-idx idx)]
                               (doto ctx
                                 (c/begin-path)
                                 (c/move-to sx sy)
                                 (c/line-to (+ cellx cell-r-px) celly)
                                 (c/stroke))))]
    ;; draw background channels to cell from column and from segments
    (c/stroke-width ctx col-grid-px)
    (c/stroke-style ctx (:inactive state-colors))
    (doseq [[ci {segs :segments}] (map-indexed vector cells)]
      (stroke-col-to-cell ctx ci)
      (doseq [si (range (count segs))]
        (stroke-seg-to-cell ctx ci si)))
    (doseq [[ci {segs :segments}] (map-indexed vector cells)
            :let [[cellx celly] (cell->px ci)
                  cell-id [cid ci]
                  cell-active? (current-ac cell-id)
                  learn-cell? (= cell-id (:cell-id learning))
                  seg-sg (mapv #(group-synapses (:synapses %) ac pcon) segs)
                  on? (fn [sg] (>= (count (sg [:connected :active])) th))
                  cell-state (cond
                              (and cell-active? (some on? seg-sg)) :active-predicted
                              (some on? seg-sg) :predicted
                              cell-active? :active
                              :else :inactive)]]
      (when cell-active?
        (doto ctx
          (c/stroke-style (:active state-colors))
          (c/stroke-width 2)
          (stroke-col-to-cell ci)))
      ;; draw the cell itself
      (when learn-cell?
        (doto ctx
          (c/fill-style (:highlight state-colors))
          (c/begin-path)
          (circle cellx celly (+ cell-r-px 8))
          (c/fill)))
      (doto ctx
        (c/fill-style (state-colors cell-state))
        (c/stroke-style "black")
        (c/stroke-width 1)
        (c/begin-path)
        (circle cellx celly cell-r-px)
        (c/stroke)
        (c/fill))
      (c/fill-style ctx "black")
      (c/text ctx {:text (str "cell " ci
                              (when learn-cell?
                                (str "   (learning on "
                                     (if-let [si (:segment-idx learning)]
                                       (str "segment " si)
                                       "new segment")
                                     ")")))
                   :x cellx :y (- celly cell-r-px 5)})
      (doseq [[si sg] (map-indexed vector seg-sg)
              :let [[sx sy] (seg->px ci si)
                    conn-act (count (sg [:connected :active]))
                    conn-tot (+ conn-act (count (sg [:connected :inactive])))
                    disc-act (count (sg [:disconnected :active]))
                    disc-tot (+ disc-act (count (sg [:disconnected :inactive])))
                    z (-> (/ conn-act th)
                          (min 1.0))
                    learn-seg? (and learn-cell?
                                    (= si (:segment-idx learning)))]]
        ;; draw segment as a rectangle
        (let [seg-w (* 2 seg-r-px)
              seg-cx (+ sx seg-r-px)
              s (centred-rect seg-cx sy seg-w 10)
              hs (centred-rect seg-cx sy (+ seg-w 8) (+ 10 8))]
          (when learn-seg?
            (doto ctx
              (c/fill-style (:highlight state-colors))
              (c/fill-rect hs)))
          (doto ctx
            (c/alpha 1.0)
            (c/stroke-style "black")
            (c/stroke-width 1)
            (c/stroke-rect s)
            (c/fill-style "white")
            (c/fill-rect s)
            (c/alpha z)
            (c/fill-style (:active state-colors))
            (c/fill-rect s)
            (c/alpha 1.0)))
        (when (on? sg)
          (doto ctx
            (c/stroke-style (:active state-colors))
            (c/stroke-width 2)
            (stroke-seg-to-cell ci si)))
        (c/fill-style ctx "black")
        (c/text ctx {:text (str "[" si "],  active / conn. = " conn-act
                                " / " conn-tot)
                     :x (+ sx 5 (* 2 seg-r-px)) :y (- sy 5)})
        (c/text ctx {:text (str "   active / disconn. = " disc-act
                                " / " disc-tot)
                     :x (+ sx 5 (* 2 seg-r-px)) :y (+ sy 5)})
        ;; synapses
        (c/stroke-width ctx 1)
        (let [perms? (get-in opts [:lat-synapses :permanences])
              draw (fn [syns conn? active?]
                     (c/stroke-style ctx
                                     (cond
                                      (and conn? active?) "red"
                                      (and conn? (not active?)) "black"
                                      active? (hsl :red 0.5 0.5)
                                      (not active?) (grey 0.5)))
                     (doseq [[[to-cid _] perm] syns
                             :let [[cx cy] (column->px to-cid (inc dt))]]
                       (doto ctx
                         (c/alpha (if perms? perm 1))
                         (c/begin-path)
                         (c/move-to sx sy)
                         (c/line-to (+ cx 1) cy) ;; +1 avoid obscuring colour
                         (c/stroke)))
                     (c/alpha ctx 1))
              from (get-in opts [:lat-synapses :from])]
          (when (or (= from :all)
                    (and (= from :learning) learn-seg?))
                                        ;(draw (sg [:disconnected :inactive]) false false)
                                        ;(draw (sg [:disconnected :active]) false true)
            (when (get-in opts [:lat-synapses :inactive])
              (draw (sg [:connected :inactive]) true false))
            (when (get-in opts [:lat-synapses :active])
              (draw (sg [:connected :active]) true true))))))
    (c/restore ctx))
  ctx)

(defn detail-text
  [{:keys [dt cid] :as selection}]
  (let [state (nth @steps dt)
        rgn (:region state)
        ingen (:in state)
        in (core/domain-value ingen)
        bits (core/bits-value ingen)]
    (->>
     ["__Selection__"
      (str "* timestep " (:timestep rgn)
           " (delay " dt ")")
      (str "* column " (or cid "nil"))
      ""
      "__Input__"
      (str in " (" (count bits) " bits)")
      ""
      "__Active columns__"
      (str (sort (:active-columns rgn)))
      ""
      "__Active cells__"
      (str (sort (:active-cells rgn)))
      ""
      "__Learn cells__"
      (str (sort (:learn-cells rgn)))
      ""
      "__Predicted cells__"
      (str (sort (:predictive-cells rgn)))
      ""
      (if cid
        (let [dtp (inc dt)
              pstate (nth @steps dtp)
              pcol (get-in pstate [:region :columns cid])
              prgn (:region pstate)
              pcells (:active-cells prgn)
              col (get-in rgn [:columns cid])]
          ["__Active cells prev__"
           (str (sort (:active-cells prgn)))
           ""
           "__Learn cells prev__"
           (str (sort (:learn-cells prgn)))
           ""
           "__Selected column__"
           "__Cells and their Dendrite segments__"
           (->> (:cells pcol)
                (map-indexed
                 (fn [i cell]
                   (let [ds (:segments cell)
                         ac (:active-cells prgn)
                         lc (:learn-cells prgn)]
                     [(str "CELL " i)
                      (str (count ds) " = "
                           (sort (map (comp count :synapses) ds)))
                      (for [i (range (count ds))
                            :let [syns (:synapses (ds i))]]
                        [(str "  SEGMENT " i)
                         (for [[id p] (sort syns)]
                           (str "  " id " :=> "
                                (gstring/format "%.2f" p)
                                (if (lc id) " L"
                                    (if (ac id) " A"))))])
                      ]))))
           ]))]
     (flatten)
     (interpose \newline)
     (apply str))))

(defn rgn-column-states
  [rgn opts]
  (if (get-in opts [:columns :predictive])
    (let [pred-cids (set (keys (:prev-predictive-cells-by-column rgn)))
          active-cids (:active-columns rgn)
          hit-cids (set/intersection pred-cids active-cids)]
      (merge (zipmap pred-cids (repeat :predicted))
             (zipmap active-cids (repeat :active))
             (zipmap hit-cids (repeat :active-predicted))))
    (zipmap (:active-columns rgn) (repeat :active))))

(defn in-synapse-display-data
  [state cid opts]
  (when (get-in opts [:ff-synapses :active])
    (let [col (get-in state [:region :columns cid])
          on-bits (core/bits-value (:in state))
          syns (-> col :in-synapses :connected)]
      {:active (select-keys syns on-bits)
       :inactive (when (get-in opts [:ff-synapses :inactive])
                   (apply dissoc syns on-bits))})))

(defn group-synapses
  [syns ac pcon]
  (group-by (fn [[id p]]
              [(if (>= p pcon)
                 :connected :disconnected)
               (if (ac id)
                 :active :inactive)])
            syns))

(defn get-column-learning-segment
  [prev-rgn current-ac cid]
  (when (current-ac cid)
    (let [col (get-in prev-rgn [:columns cid])
          ac (:active-cells prev-rgn)
          lc (:learn-cells prev-rgn)
          col-ac (set (filter (fn [[_ i]] (= i cid))
                              current-ac))
          bursting? (= (count col-ac)
                       (count (:cells col)))
          spec (:spec prev-rgn)]
      (sm/column-learning-segment-and-cell col bursting? col-ac lc ac spec))))

(defn image-buffer
  [w h]
  (let [el (->dom [:canvas])]
    (set! (.-width el) w)
    (set! (.-height el) h)
    el))

(defn inbits-grid-image
  [bit-width opts]
  (let [el (image-buffer bit-w-px (* bit-h-px bit-width))
        ctx (c/get-context el "2d")]
    (draw-bits-1 ctx (range bit-width) (:inactive state-colors))
    el))

(defn inbits-image
  [in bit-width opts]
  (let [inbits (core/bits-value in)
        el (image-buffer bit-w-px (* bit-h-px bit-width))
        ctx (c/get-context el "2d")]
    (draw-bits-1 ctx inbits (:active state-colors))
    el))

(defn predbits-image
  [prev-rgn bit-width opts]
  (let [pcbc (:predictive-cells-by-column prev-rgn)
        bit-votes (sm/predicted-bit-votes prev-rgn pcbc)
        el (image-buffer bit-w-px (* bit-h-px bit-width))
        ctx (c/get-context el "2d")]
    (draw-predbits-1 ctx bit-votes)
    el))

(defn region-image
  [rgn opts]
  (let [col-states (rgn-column-states rgn opts)
        col-vals (if (get-in opts [:columns :overlaps])
                   (merge (:overlaps rgn) col-states)
                   col-states)
        ncol (count (:columns rgn))
        el (image-buffer col-grid-px (* col-grid-px ncol))
        ctx (c/get-context el "2d")]
    (draw-region-1 ctx col-vals ncol)
    el))

(defn draw!
  [{sel-dt :dt
    sel-cid :cid
    :as selection}]
  (dom/val "#detail-text"
           (if sel-cid (detail-text selection) ""))
  (let [opts @viz-options
        sel-state (nth @steps sel-dt)
        bit-width (core/bit-width (:in sel-state))
        ncol (count (:columns (:region sel-state)))
        canvas-el (->dom "#viz")
        ctx (c/get-context canvas-el "2d")
        bg-img (inbits-grid-image bit-width opts)
        width-px (.-width canvas-el)]
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    (doseq [dt (range (count @steps))
            :let [state (nth @steps dt)
                  prev-state (nth @steps (inc dt) {})
                  rgn (:region state)
                  cache (::cache (meta state))
                  cached-opts (:viz-options @cache)]]
      ;; refresh cache if necessary
      (when (not= (:input opts) (:input cached-opts))
        (let [img (inbits-image (:in state) bit-width opts)]
          (swap! cache assoc
                 :viz-options opts
                 :inbits-image img))
        (let [img (predbits-image (:region prev-state) bit-width opts)]
          (swap! cache assoc
                 :viz-options opts
                 :predbits-image img)))
      ;; refresh cache if necessary
      (when (not= (:columns opts) (:columns cached-opts))
        (let [img (region-image (:region state) opts)]
          (swap! cache assoc
                 :viz-options opts
                 :region-image img)))
      ;; draw from the cached off-screen buffers
      (let [[xo yo] (inbits-dt-offset dt)]
        (c/draw-image ctx bg-img xo yo)
        (when (get-in opts [:input :active])
          (c/draw-image ctx (:inbits-image @cache) xo yo))
        (when (get-in opts [:input :predicted])
          (c/draw-image ctx (:predbits-image @cache) xo yo)))
      (let [[xo yo] (region-dt-offset dt)]
        (c/draw-image ctx (:region-image @cache) xo yo)))
    (draw-inbits-selection ctx sel-dt bit-width)
    (draw-region-selection ctx sel-dt sel-cid ncol)
    ;; extra visuals
    (let [rgn (:region sel-state)
          prev-rgn (:region (nth @steps (inc sel-dt) {}))
          sel-cids (if sel-cid [sel-cid] (:active-columns rgn))
          insyn-data (for [cid sel-cids]
                       [cid (in-synapse-display-data sel-state cid opts)])]
      (draw-insynapses ctx insyn-data sel-dt opts)
      (when (and prev-rgn sel-cid)
        (draw-all-segments ctx sel-cid sel-dt prev-rgn rgn ncol opts))))
  nil)

;; ## Event stream processing

(defn listen [el type capture-fn]
  (let [out (chan)]
    (gevents/listen el type
                    (fn [e] (put! out e)
                      (when (capture-fn e)
                        (.preventDefault e)
                        false)))
    out))

(defn handle-canvas-clicks
  [el selection]
  (let [clicks (listen el "click" (fn [_] false))]
    (go
     (while true
       (let [e (<! clicks)
             x (.-offsetX e)
             y (.-offsetY e)
             ;; we need to assume there is a previous step, so:
             max-dt (- (count @steps) 2)]
         (if-let [[cid dt] (px->column x y)]
           ;; column clicked
           (reset! selection {:cid cid :dt (min dt max-dt)})
           (if-let [[id dt] (px->inbit x y)]
             ;; in-bit clicked
             (reset! selection {:cid nil :dt (min dt max-dt)})
             ;; nothing clicked
             (reset! selection {:cid nil :dt 0}))))))))

(def code-key
  {;32 :space
   37 :left
   38 :up
   39 :right
   40 :down})

(defn handle-canvas-keys
  [selection sim-step!]
  (let [presses (listen js/document goog.events.EventType.KEYDOWN
                        (fn [e] (code-key (.-keyCode e))))]
    (go
     (while true
       (let [e (<! presses)
             k (code-key (.-keyCode e))
             ;; we need to assume there is a previous step, so:
             max-dt (- (count @steps) 2)]
         (when k
           (case k
             :left (swap! selection update-in [:dt]
                          (fn [x] (min (inc x) max-dt)))
             :right (if (zero? (:dt @selection))
                      (do (sim-step!)
                          (swap! selection identity)) ;; redraw
                      (swap! selection update-in [:dt]
                             (fn [x] (max (dec x) 0))))
             :up (swap! selection update-in [:cid]
                        (fn [x] (if x (dec x) 0)))
             :down (swap! selection update-in [:cid]
                          (fn [x] (if x (inc x) 0)))
             )))))))

(defn init!
  [steps-c selection sim-step!]
  (go (loop []
        (when-let [x (<! steps-c)]
          (let [x* (vary-meta x assoc ::cache (atom {}))]
            (swap! steps #(->> (cons x* %)
                               (take @keep-steps)
                               (vec))))
          (recur))))
  (let [el (->dom "#viz")]
    (set! (.-width el) (* 0.70 (- (.-innerWidth js/window) 20)))
    (set! (.-height el) height-px)
    (handle-canvas-clicks el selection)
    (handle-canvas-keys selection sim-step!)))
