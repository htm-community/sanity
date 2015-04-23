(ns comportexviz.demos.coordinates-2d
  (:require [org.nfrac.comportex.demos.coordinates-2d :as demo]
            [org.nfrac.comportex.util :as util :refer [round]]
            [comportexviz.main :as main]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [comportexviz.viz-canvas :as viz]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-ui-loading-message]]))

(def model-config
  (atom {:n-regions 1}))

(declare draw-coords-fn)

(def world-c
  (async/chan (async/buffer 1)
              (comp (map (util/keep-history-middleware
                          50 #(select-keys % [:x :y :vx :vy])
                          :history))
                    (map #(vary-meta % assoc
                                     :comportexviz/draw-world
                                     (draw-coords-fn demo/max-pos))))))

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

(defn draw-coords-fn
  [max-pos]
  (let [x-lim [(- max-pos) max-pos]
        y-lim [(- max-pos) max-pos]]
    (fn [this ctx left-px top-px w-px h-px state]
      (let [[plot-x plot-y] [10 100]
            plot-size {:w (- w-px 20)
                       :h (- w-px 20)}
            plot (plt/xy-plot ctx plot-size x-lim y-lim)
            x-scale (plt/scale-fn x-lim (:w plot-size))
            y-scale (plt/scale-fn y-lim (:h plot-size))
            {:keys [x y vx vy]} this
            history (:history (meta this))
            r-px (- (x-scale demo/radius) (x-scale 0))]
        (c/save ctx)
        (c/translate ctx left-px top-px)
        ;; draw coordinates text
        (doto ctx
          (c/fill-style "black")
          (c/font-style "14px monospace")
          (c/text {:x plot-x :y (quot plot-y 2)
                   :text (str "(" x "," y ")")}))
        ;; draw the plot
        (c/translate ctx plot-x plot-y)
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
                           :angle (Math/atan2 vy vx)})))
      (c/restore ctx))))

(defn set-model!
  []
  (let [n-regions (:n-regions @model-config)]
    (with-ui-loading-message
      (main/set-model!
       (demo/n-region-model n-regions)))))

(def model-config-template
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
   [bind-fields model-config-template model-config]

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
  (reagent/render (main/comportexviz-app model-tab)
                  (dom/getElement "comportexviz-app"))
  (swap! viz/viz-options assoc-in [:drawing :display-mode] :two-d)
  (reset! main/world world-c)
  (feed-world!)
  (set-model!))
