(ns comportexviz.local-inhibition-2d
  (:require [org.nfrac.comportex.inhibition :as inh]
            [org.nfrac.comportex.topology :as topology]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cells :as cells]
            [org.nfrac.comportex.util :as util :refer [abs]]
            [monet.canvas :as c]
            [comportexviz.viz-canvas :as viz :refer [image-buffer]]
            [comportexviz.viz-layouts :as lay
             :refer [layout-bounds
                     fill-element-group
                     fill-elements
                     columns-2d-layout]]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]))

(def width-px 500)
(def height-px 380)
(def plot-height-px 170)
(def plot-spacing 10)

(def nx 80)
(def ny 30)
(def dim [nx ny])
(def topo (topology/make-topology dim))
(def size (* nx ny))
(def inh-radius 15)

(def !exc (atom {}))
(def !act (atom ()))
(def !global-act (atom #{}))
(def spec {:activation-level 0.02
           :global-inhibition? false
           :inhibition-base-distance 1
           :ff-stimulus-threshold 2.0
           })

(defn zapsmall
  [x d]
  (if (< x d) 0.0 x))

(defn gen-exc
  [spec]
  (let [focus-r 20
        focus-1-x (* focus-r 2)
        focus-1-y 20
        focus-2-x (+ nx (* inh-radius 0.7))
        focus-2-y 0]
    (->> (for [i (range size)]
           (let [[x y] (p/coordinates-of-index topo i)]
             ;; triangular peaks at focus-1 and focus-2
             (-> (+ (max 0 (- focus-r (max (abs (- x focus-1-x))
                                           (abs (- y focus-1-y)))))
                    (max 0 (- focus-r (max (abs (- x focus-2-x))
                                           (abs (- y focus-2-y))))))
                 ;; multiplicative noise on triangular peaks
                 (* (rand))
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
                         (:inhibition-base-distance spec)
                         (* (p/size topo) (:activation-level spec))))))

(defn global-active-columns
  [exc topo spec]
  (set
   (inh/inhibit-globally exc (* (p/size topo) (:activation-level spec)))))

(defn set-ui!
  [spec actual-level]
  (dom/text (->dom "#inh2d-base-dist") (:inhibition-base-distance spec))
  (dom/text (->dom "#inh2d-radius")
            (util/round inh-radius 2))
  (dom/text (->dom "#inh2d-stimulus")
            (util/round (:ff-stimulus-threshold spec) 2))
  (dom/text (->dom "#inh2d-actual-level")
            (util/round actual-level 2))
  (dom/text (->dom "#inh2d-target-level") (:activation-level spec)))

(defn excitation-image
  [lay exc]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")
        maxval (apply max 0 (vals exc))
        m (->> exc
               (util/remap #(util/round
                             (min 1.0 (/ % maxval))
                             2)))]
    (c/fill-style ctx "black")
    (fill-elements ctx lay m c/alpha)
    el))

(defn activation-image
  [lay act]
  (let [el (image-buffer (layout-bounds lay))
        ctx (c/get-context el "2d")]
    (c/fill-style ctx "red")
    (fill-element-group ctx lay act)
    el))

(defn draw!
  [exc act global-act spec]
  (let [ctx (c/get-context (->dom "#inh2d-viz") "2d")
        d-opts (:drawing @viz/viz-options)
        lay (columns-2d-layout topo 0 0 plot-height-px d-opts)
        exc-img (excitation-image lay exc)
        act-img (activation-image lay act)
        global-act-img (activation-image lay global-act)]
    (c/save ctx)
    (doto ctx
      (c/clear-rect {:x 0 :y 0 :w width-px :h height-px})
      (c/draw-image exc-img 0 0)
      (c/draw-image act-img 0 0)
      (c/fill-style "black")
      (c/text {:text "global inhibition:" :x 100
               :y (+ plot-height-px (quot plot-spacing 2))})
      (c/draw-image exc-img 0 (+ plot-height-px plot-spacing))
      (c/draw-image global-act-img 0 (+ plot-height-px plot-spacing)))
    (c/restore ctx)))

(defn do-step!
  []
  (let [prev-actual-level (/ (count @!act) size)
        exc (reset! !exc (gen-exc spec))
        act (reset! !act (local-active-columns exc topo inh-radius spec))
        actual-level (/ (count act) size)
        global-act (reset! !global-act
                           (global-active-columns exc topo spec))]
    (draw! exc act global-act spec)
    (set-ui! spec actual-level)))

(defn ^:export init
  []
  (let [viz-el (->dom "#inh2d-viz")]
    (set! (.-height viz-el) height-px)
    (set! (.-width viz-el) width-px)
    (do-step!)
    (let [step-el (->dom "#inh2d-step")
          glob-el (->dom "#inh2d-global")]
      (event/on-raw step-el :click
                    (fn [e]
                      (try
                        (do-step!)
                        (catch js/Error e
                          (println e)))
                      false)))))
