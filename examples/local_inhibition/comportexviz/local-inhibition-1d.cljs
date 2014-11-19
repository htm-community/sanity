(ns comportexviz.local-inhibition-1d
  (:require [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.util :as util :refer [abs]]
            [monet.canvas :as c]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]))

(def width-px 600)
(def height-px 300)
(def plot-height-px (quot height-px 2))

(def nx 200)
(def dim [nx])
(def topo (topology/make-topology dim))
(def size nx)
(def inh-radius 15)

(def !max-exc (atom 0))
(def !exc (atom {}))
(def !act (atom #{}))
(def !global-act (atom #{}))
(def !spec (atom {:activation-level 0.03
                  :global-inhibition? false
                  :inhibition-max-distance inh-radius
                  :inhibition-base-distance 2
                  :ff-stimulus-threshold 5.0
                  }))

(defn zapsmall
  [x d]
  (if (< x d) 0.0 x))

(defn gen-exc
  [spec]
  (let [focus-r (* inh-radius 2.0)
        focus-1 focus-r
        focus-2 (+ nx (* inh-radius 0.7))]
    (->> (for [i (range size)]
           (let [x (p/coordinates-of-index topo i)]
             ;; triangular peaks at focus-1 and focus-2
             (-> (max 0
                      (- focus-r (abs (- x focus-1)))
                      (- focus-r (abs (- x focus-2))))
                 ;; multiplicative noise on triangular peaks
                 (* (rand))
                 ;; add (positive half of) sine wave, with multipicative noise
                 (+ (rand (-> (* (/ x nx)
                                 3.14 4)
                              (Math/sin)
                              (max 0)
                              (* 4.0))))
                 ;; additive noise - skewed distribution
                 (+ (* 5.0 (Math/pow (rand) 3.0)))
                 ;; can't have fewer than 1 active synapse
                 (zapsmall 1.0))))
         (zipmap (range size)))))

(defn local-active-columns
  [exc-raw topo inh-radius spec]
  (let [threshold (:ff-stimulus-threshold spec)
        exc (into {} (filter #(>= (val %) threshold) exc-raw))]
   (set
    (inh/inhibit-locally exc topo inh-radius
                         (:inhibition-base-distance spec)))))

(defn global-active-columns
  [exc topo spec]
  (set
   (inh/inhibit-globally exc (:activation-level spec) (p/size topo))))

(defn set-ui!
  [spec next-spec actual-level]
  (dom/text (->dom "#inh-base-dist") (:inhibition-base-distance spec))
  (dom/text (->dom "#inh-radius")
            (util/round inh-radius 2))
  (dom/text (->dom "#inh-stimulus")
            (util/round (:ff-stimulus-threshold spec) 2))
  (dom/text (->dom "#inh-stimulus-next")
            (util/round (:ff-stimulus-threshold next-spec) 2))
  (dom/text (->dom "#inh-actual-level")
            (util/round actual-level 2))
  (dom/text (->dom "#inh-target-level") (:activation-level spec))
  (let [too-high? (> actual-level (:activation-level spec))]
    (dom/text (->dom "#inh-high-or-low")
              (if too-high? "high" "low"))
    (dom/text (->dom "#inh-up-or-down")
              (if too-high? "up" "down"))))

(defn draw-exc-bars
  [ctx exc act ->x ->y ->h]
  (doseq [[col o] exc]
      (c/fill-style ctx (if (act col) "red" "black"))
      (c/fill-rect ctx {:x (- (->x col) 1)
                        :y (->y o)
                        :h (->h o)
                        :w 2.5})))

(defn draw-inh-radii
  [ctx exc act ->x ->y spec]
  (let [base (:inhibition-base-distance spec)]
   (doseq [col act
           :let [o (exc col)]]
     (doto ctx
       (c/begin-path)
       (c/move-to (->x (- col inh-radius)) (->y 0))
       (c/line-to (->x (- col base)) (->y o))
       (c/line-to (->x (+ col base)) (->y o))
       (c/line-to (->x (+ col inh-radius)) (->y 0))
       (c/stroke)))))

(defn draw!
  [exc act global-act spec]
  (let [ctx (c/get-context (->dom "#inh-viz") "2d")
        this-max-o (apply max (vals exc))
        max-o (swap! !max-exc #(max % this-max-o))
        ->x (fn [col] (* (/ col nx) width-px))
        ->h (fn [o]  (* (/ o max-o) plot-height-px))
        ->y (fn [o]  (- plot-height-px (->h o)))
        threshold (:ff-stimulus-threshold spec)]
    (c/save ctx)
    (doto ctx
      (c/clear-rect {:x 0 :y 0 :w width-px :h height-px})
      (draw-exc-bars exc act ->x ->y ->h)
      (c/fill-style "black")
      (c/text {:text "local inhibition" :x (quot width-px 2)
               :y (quot plot-height-px 3)})
      (c/stroke-style "#888888")
      (draw-inh-radii exc act ->x ->y spec)
      ;; zero line
      (c/stroke-width 2)
      (c/stroke-style "black")
      (c/begin-path)
      (c/move-to 0 (->y 0))
      (c/line-to width-px (->y 0))
      (c/stroke)
      ;; stimulus threshold line
      (c/stroke-style "#00bb44")
      (c/fill-style "#00bb44")
      (c/begin-path)
      (c/move-to 0 (->y threshold))
      (c/line-to width-px (->y threshold))
      (c/stroke)
      (c/text {:text "stimulus threshold" :x (quot width-px 2)
               :y (- (->y threshold) 2)}))
    ;; draw global inhibition version mirrored below
    (let [->y (fn [o] plot-height-px)]
      (doto ctx
        (draw-exc-bars exc global-act ->x ->y ->h)
        (c/fill-style "black")
        (c/text {:text "global inhibition" :x (quot width-px 2)
                 :y (- height-px (quot plot-height-px 3))})))
    (c/restore ctx)))

(defn do-step!
  []
  (let [prev-actual-level (/ (count @!act) size)
        spec (swap! !spec cells/tune-spec prev-actual-level (count @!exc))
        exc (reset! !exc (gen-exc spec))
        act (reset! !act (local-active-columns exc topo inh-radius spec))
        actual-level (/ (count act) size)
        global-act (reset! !global-act
                           (global-active-columns exc topo spec))
        next-spec (cells/tune-spec spec actual-level (count exc))]
    (draw! exc act global-act spec)
    (set-ui! spec next-spec actual-level)))

(defn ^:export init
  []
  (let [viz-el (->dom "#inh-viz")]
    (set! (.-height viz-el) height-px)
    (set! (.-width viz-el) width-px)
    (do-step!)
    (let [step-el (->dom "#inh-step")]
      (event/on-raw step-el :click
                    (fn [e]
                      (try
                        (do-step!)
                        (catch js/Error e
                          (println e)))
                      false)))))
