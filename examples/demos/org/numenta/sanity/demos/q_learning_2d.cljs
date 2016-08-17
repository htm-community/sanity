(ns org.numenta.sanity.demos.q-learning-2d
  (:require [org.nfrac.comportex.demos.q-learning-2d :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            [org.numenta.sanity.demos.q-learning-1d :refer [q-learning-sub-pane]]
            [org.numenta.sanity.demos.comportex-common :refer [all-features]]
            [org.numenta.sanity.main :as main]
            [org.numenta.sanity.helpers :as helpers :refer [resizing-canvas]]
            [org.numenta.sanity.plots-canvas :as plt]
            [org.numenta.sanity.bridge.browser :as server]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.comportex.data :as data]
            [org.numenta.sanity.util :refer [translate-network-shape]]
            [monet.canvas :as c]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.string :as gstr]
            [goog.string.format]
            [cljs.core.async :as async :refer [put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]
                   [org.numenta.sanity.macros :refer [with-ui-loading-message]]))

(def config
  (atom {:n-regions 1}))

(def world-c
  (async/chan (async/buffer 1)
              (map #(assoc % :label [(:x %) (:y %)]))))

(def into-sim (async/chan))

(def model (atom nil))

(defn draw-world
  [ctx inval htm]
  (let [surface demo/surface
        x-max (count surface)
        y-max (count (first surface))
        x-lim [0 x-max]
        y-lim [0 y-max]
        width-px (.-width (.-canvas ctx))
        height-px (.-height (.-canvas ctx))
        edge-px (min width-px height-px)
        plot-size {:w edge-px
                   :h edge-px}
        plot (plt/xy-plot ctx plot-size x-lim y-lim)]
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    (plt/frame! plot)
    ;; draw grid with terminal positions
    (doseq [y (range (count surface))
            x (range (count (first surface)))
            :let [v (get-in surface [x y])]]
      (cond
        (>= v 10)
        (do (c/fill-style ctx "#66ff66")
            (plt/rect! plot x y 1 1))
        (<= v -10)
        (do (c/fill-style ctx "black")
            (plt/rect! plot x y 1 1))
        ))
    ;; show Q values for transitions as colour-coded edges
    (doseq [[state-action q] (:Q-map inval)
            :let [{:keys [x y action]} state-action
                  {:keys [dx dy]} action]]
      (c/fill-style ctx (if (pos? q) "green" "red"))
      (c/alpha ctx (abs q))
      (cond
        ;; from left
        (pos? dx)
        (plt/rect! plot (- x 0.25) y 0.25 1)
        ;; from right
        (neg? dx)
        (plt/rect! plot (+ x 1) y 0.25 1)
        ;; from above
        (pos? dy)
        (plt/rect! plot x (- y 0.25) 1 0.25)
        ;; from below
        (neg? dy)
        (plt/rect! plot x (+ y 1) 1 0.25)
        ))
    (c/alpha ctx 1)
    ;; draw position and action
    (let [x= (+ 0.5 (:x inval))
          y= (+ 0.5 (:y inval))
          dx-1 (:dx (:prev-action inval))
          dy-1 (:dy (:prev-action inval))
          dx (:dx (:action inval))
          dy (:dy (:action inval))]
      ;; previous action
      (c/stroke-style ctx "black")
      (plt/line! plot [[(- x= dx-1) (- y= dy-1)]
                       [x= y=]])
      ;; next action
      (c/stroke-style ctx "#888")
      (plt/line! plot [[x= y=]
                       [(+ x= dx) (+ y= dy)]])
      ;; previous position
      (c/stroke-style ctx "#888")
      (c/fill-style ctx "white")
      (plt/point! plot (- x= dx-1) (- y= dy-1) 3)
      ;; current position
      (c/stroke-style ctx "black")
      (c/fill-style ctx "yellow")
      (plt/point! plot x= y= 4))
    (c/stroke-style ctx "black")
    (plt/grid! plot {})))

(defn signed-str [x] (str (if (neg? x) "" "+") x))

(defn world-pane
  []
  (let [selected-htm (atom nil)]
    (add-watch main/selection ::fetch-selected-htm
               (fn [_ _ _ [sel1]]
                 (when-let [snapshot-id (get-in sel1 [:step :snapshot-id])]
                   (let [out-c (async/chan)]
                     (put! main/into-journal ["get-model" snapshot-id
                                              (marshal/channel out-c true)])
                     (go
                       (reset! selected-htm (<! out-c)))))))
    (fn []
      (when-let [htm @selected-htm]
        (let [inval (:input-value htm)
              DELTA (gstr/unescapeEntities "&Delta;")
              TIMES (gstr/unescapeEntities "&times;")]
          [:div
           [:p.muted [:small "Input on selected timestep."]]
           [:p.muted [:small "Reward " [:var "R"] " = z " TIMES " 0.01"]]
           [:table.table.table-condensed
            [:tr
             [:th "x,y"]
             [:td [:small "position"]]
             [:td (:x inval) "," (:y inval)]]
            [:tr
             [:th (str DELTA "x," DELTA "y")]
             [:td [:small "action"]]
             [:td (str (signed-str (:dx (:prev-action inval)))
                       ","
                       (signed-str (:dy (:prev-action inval))))]]
            [:tr
             [:th [:var "z"]]
             [:td [:small "~reward"]]
             [:td (signed-str (:z inval))]]
            [:tr
             [:th (str DELTA "x," DELTA "y") [:sub "t+1"]]
             [:td [:small "action"]]
             [:td (str (signed-str (:dx (:action inval)))
                       ","
                       (signed-str (:dy (:action inval))))]]]
           (q-learning-sub-pane htm)
           ;; plot
           [resizing-canvas {:style {:width "100%"
                                     :height "240px"}}
            [main/selection selected-htm]
            (fn [ctx]
              (let [step (main/selected-step)
                    inval (:input-value step)]
                (draw-world ctx inval @selected-htm)))
            nil]
           [:small
            [:p
             "Current position on the objective function surface. "
             "Also shows approx Q values for each position/action combination,
            where green is positive and red is negative.
            These are the last seen Q values including last adjustments."]
            ]])))))

(defn set-model!
  []
  (with-ui-loading-message
    (let [init? (nil? @model)]
      (go
        (when-not init?
          ;; break cycle to reset input
          (<! world-c))
        (reset! model (demo/make-model))
        (if init?
          (server/init model world-c main/into-journal into-sim
                       (demo/htm-step-with-action-selection world-c))
          (reset! main/network-shape (translate-network-shape
                                      (data/network-shape @model))))
        (put! world-c demo/initial-inval)))))

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
   [:p "Highly experimental attempt at integrating "
    [:a {:href "http://en.wikipedia.org/wiki/Q-learning"} "Q learning"]
    " (reinforcement learning)."]
   [:h4 "General approach"]
   [:p "A Q value indicates the goodness of taking an action from some
        state. We represent a Q value by the average permanence of
        synapses activating the action from that state, minus the
        initial permanence value."]
   [:p "The action region columns are activated just like any other
        region, but are then interpreted to produce an action."]
   [:p "Adjustments to a Q value, based on reward and expected future
        reward, are applied to the permanence of synapses which
        directly activated the action (columns). This adjustment
        applies in the action layer only, where it replaces the usual
        learning of proximal synapses (spatial pooling)."]
   [:p "Exploration arises from the usual boosting of neglected
        columns, primarily in the action layer."]

   [:h4 "This example"]
   [:p "The agent can move up, down, left or right on a surface.
        The reward is -3 on normal squares, -200 on hazard squares
        and +200 on the goal square. These are divided by 100 for
        comparison to Q values on the synaptic permanence scale."]
   [:p "The action layer columns are interpreted to produce an
        action. 10 columns are allocated to each of the four
        directions of movement, and the direction with most active
        columns is used to move the agent."]
   [:p "The input is the location of the agent via coordinate
        encoder, plus the last movement as distal input."]
   [:p "This example is episodic: when the agent reaches either the
        goal or a hazard it is returned to the starting point. Success
        is indicated by the agent following a direct path to the goal
        square."]

   [:h3 "HTM model"]
   [bind-fields config-template config]
   ]
  )

(defn ^:export init
  []
  (reagent/render [main/sanity-app "Comportex" [model-tab] [world-pane]
                   (atom :model) all-features into-sim]
                  (dom/getElement "sanity-app"))
  (swap! main/viz-options assoc-in [:drawing :display-mode] :two-d)
  (set-model!))
