(ns org.numenta.sanity.demos.coordinates-2d
  (:require [org.nfrac.comportex.demos.coordinates-2d :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util :refer [round]]
            [org.numenta.sanity.demos.comportex-common :refer [all-features]]
            [org.numenta.sanity.main :as main]
            [org.numenta.sanity.helpers :as helpers :refer [resizing-canvas]]
            [org.numenta.sanity.plots-canvas :as plt]
            [org.numenta.sanity.bridge.browser :as server]
            [org.numenta.sanity.comportex.data :as data]
            [org.numenta.sanity.util :refer [translate-network-shape]]
            [monet.canvas :as c]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [org.numenta.sanity.macros :refer [with-ui-loading-message]]))

(def config
  (atom {:n-regions 1}))

(defn quadrant
  [inval]
  (str (if (pos? (:y inval)) "S" "N")
       (if (pos? (:x inval)) "E" "W")))

(def world-c
  (async/chan (async/buffer 1)
              (comp (map (util/keep-history-middleware
                          50 #(select-keys % [:x :y :vx :vy])
                          :history))
                    (map #(assoc % :label (quadrant %))))))

(def model (atom nil))

(def into-sim (async/chan))

(def control-c (async/chan))

(defn feed-world!
  "Feed the world channel continuously, reacting to UI settings."
  []
  (go
    (loop [x demo/initial-input-val]
      (>! world-c x)
      (let [tc (async/timeout 50)
            ;; collect and apply all control messages until timeout
            xx (loop [x x]
                 (let [[f c] (async/alts! [control-c tc])]
                   (if (= c control-c)
                     (recur (f x))
                     x)))]
        (recur (demo/input-transform xx))))))

(defn draw-arrow
  [ctx {:keys [x y angle]}]
  (c/save ctx)
  (c/translate ctx x y)
  (c/rotate ctx angle)
  (c/begin-path ctx)
  (c/move-to ctx 5 0)
  (c/line-to ctx -5 3)
  (c/line-to ctx -5 -3)
  (c/line-to ctx 5 0)
  (c/fill ctx)
  (c/stroke ctx)
  (c/restore ctx))

(defn centred-rect
  [cx cy w h]
  {:x (- cx (/ w 2))
   :y (- cy (/ h 2))
   :w w
   :h h})

(defn draw-world
  [ctx in-value]
  (let [max-pos demo/max-pos
        radius demo/radius
        x-lim [(- max-pos) max-pos]
        y-lim [(- max-pos) max-pos]
        width-px (.-width (.-canvas ctx))
        height-px (.-height (.-canvas ctx))
        edge-px (min width-px height-px)
        plot-size {:w edge-px
                   :h edge-px}
        plot (plt/xy-plot ctx plot-size x-lim y-lim)
        x-scale (plt/scale-fn x-lim (:w plot-size))
        y-scale (plt/scale-fn y-lim (:h plot-size))
        {:keys [x y vx vy]} in-value
        history (:history (meta in-value))
        r-px (- (x-scale radius) (x-scale 0))]
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    (plt/frame! plot)
    (c/stroke-style ctx "lightgray")
    (plt/grid! plot {:grid-every 2})
    ;; draw the axes
    (c/stroke-style ctx "black")
    (plt/draw-grid ctx (map x-scale x-lim) (map y-scale y-lim)
                   [(round (x-scale 0))] [(round (y-scale 0))])
    ;; draw the radius
    (c/fill-style ctx "rgba(255,0,0,0.25)")
    (c/fill-rect ctx (centred-rect (x-scale x) (y-scale y)
                                   (* 2 r-px) (* 2 r-px)))
    ;; draw the locations and headings
    (c/stroke-style ctx "black")
    (c/fill-style ctx "yellow")
    (doseq [[i {:keys [x y vx vy]}] (map-indexed vector history)]
      (if (== (inc i) (count history))
        (c/alpha ctx 1)
        (c/alpha ctx (/ (inc i) (count history) 2)))
      (draw-arrow ctx {:x (x-scale x) :y (y-scale y)
                       :angle (Math/atan2 vy vx)}))
    ))

(defn world-pane
  []
  (when-let [step (main/selected-step)]
    (let [in-value (:input-value step)
          {:keys [x y vx vy]} in-value]
      [:div
       [:p.muted [:small "Input on selected timestep."]]
       [:table.table
        [:tr [:th "x"]
         [:td x]]
        [:tr [:th "y"]
         [:td y]]]
       [resizing-canvas {:style {:width "100%"
                                 :height "300px"}}
        [main/selection]
        (fn [ctx]
          (let [step (main/selected-step)
                in-value (:input-value step)]
            (draw-world ctx in-value)))
        nil]])))

(defn set-model!
  []
  (with-ui-loading-message
    (let [init? (nil? @model)]
      (reset! model (demo/n-region-model (:n-regions @config)))
      (if init?
        (server/init model world-c main/into-journal into-sim)
        (reset! main/network-shape (translate-network-shape
                                    (data/network-shape @model))))
      (when init?
        (feed-world!)))))

(def config-template
  [:div.form-horizontal
   [:div.form-group
    [:label.col-sm-5 "Number of regions:"]
    [:div.col-sm-7
     [:input.form-control {:field :numeric
                           :id :n-regions}]]]
   [:div.form-group
    [:div.col-sm-offset-5.col-sm-7
     [:button.btn.btn-default
      {:on-click (fn [e]
                   (set-model!)
                   (.preventDefault e))}
      "Restart with new model"]
     [:p.text-danger "This resets all parameters."]]]
   ])

(defn model-tab
  []
  [:div
   [:p "A simple example of the coordinate encoder in 2
    dimensions, on a repeating path."]
   [:p "The coordinate is on a 90x90 integer grid and has a
    locality radius of 15 units. It maintains position, velocity
    and acceleration. Velocity is limited to 5 units per timestep.
    When the point crosses the horizontal axis, its vertical
    acceleration is reversed; when it crosses the vertical axis,
    its horizontal acceleration is reversed."]

   [:h3 "HTM model"]
   [bind-fields config-template config]

   [:h3 "Input"]
   [:div
    [:div.form-group
     [:label "Interference with the movement path"]
     [:div
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (async/put! control-c #(update-in % [:ay] dec))
                    (.preventDefault e))}
       "Turn up"]
      [:button.btn.btn-default
       {:on-click (fn [e]
                    (async/put! control-c #(update-in % [:ay] inc))
                    (.preventDefault e))}
       "Turn down"]]]
    ]
   ])

(defn ^:export init
  []
  (reagent/render [main/sanity-app "Comportex" [model-tab] [world-pane]
                   (atom :model) all-features into-sim]
                  (dom/getElement "sanity-app"))
  (swap! main/viz-options assoc-in [:drawing :display-mode] :two-d)
  (set-model!))
