(ns comportexviz.demos.q-learning-1d
  (:require [org.nfrac.comportex.demos.q-learning-1d :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            [comportexviz.main :as main]
            [comportexviz.helpers :refer [resizing-canvas]]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.string :as gstr]
            [goog.string.format]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(def config
  (atom {:n-regions 1}))

(def world-c
  (async/chan (async/buffer 1)
              (map (util/frequencies-middleware :x :freqs))))

(defn feed-world!
  "Feed the world input channel continuously, selecting actions from
  state of model itself."
  []
  (let [step-c (main/tap-c main/steps-mult)]
    (demo/feed-world-c-with-actions! step-c world-c main/model)))

(defn draw-world
  [ctx in-value htm]
  (let [surface demo/surface
        surface-xy (mapv vector (range) surface)
        x-max (count surface)
        y-max (reduce max surface)
        x-lim [(- 0 1) (+ x-max 1)]
        y-lim [(+ y-max 1) 0]
        width-px (.-width (.-canvas ctx))
        height-px (.-height (.-canvas ctx))
        plot-size {:w width-px
                   :h 100}]
    (c/save ctx)
    (c/clear-rect ctx {:x 0 :y 0 :w width-px :h height-px})
    ;; draw Q values for right/left at each position
    (let [qplot-size {:w (:w plot-size) :h 40}
          qplot-lim [0 2]
          qplot (plt/xy-plot ctx qplot-size x-lim qplot-lim)]
      (plt/frame! qplot)
      (doseq [[state-action q] (:Q-map in-value)
              :let [{:keys [x dx]} state-action]]
        (c/fill-style ctx (if (pos? q) "green" "red"))
        (c/alpha ctx (abs q))
        (cond
          ;; from left
          (pos? dx)
          (plt/rect! qplot (- x 0.6) 0 0.6 1)
          ;; from right
          (neg? dx)
          (plt/rect! qplot x 1 0.6 1))
        )
      ;; draw ticks to separate left/right indicators
      (c/alpha ctx 0.25)
      (c/fill-style ctx "black")
      (doseq [x (range (inc (count surface)))]
        (plt/line! qplot [[x 0] [x 2]]))
      )
    (c/alpha ctx 1)
    ;; draw surface and current position
    (c/translate ctx 0 40)
    (let [plot (plt/xy-plot ctx plot-size x-lim y-lim)]
      (plt/frame! plot)
      (c/stroke-style ctx "lightgray")
      (plt/grid! plot {})
      (c/stroke-style ctx "black")
      (plt/line! plot surface-xy)
      (c/stroke-style ctx "yellow")
      (c/fill-style ctx "#6666ff")
      (plt/point! plot (:x in-value) (:y in-value) 4))
    ;; histogram
    (c/translate ctx 0 (:h plot-size))
    (let [freqs (:freqs (meta in-value))
          hist-lim [0 (inc (apply max (vals freqs)))]
          histogram (plt/xy-plot ctx plot-size x-lim hist-lim)]
      ;; draw the plot
      (c/stroke-style ctx "black")
      (doseq [[x f] freqs]
        (plt/line! histogram [[x 0] [x f]])))
    (c/restore ctx)))

(defn signed-str [x] (str (if (neg? x) "" "+") x))

(defn q-learning-sub-pane
  [htm]
  (let [alyr (get-in htm [:regions :action :layer-3])
        qinfo (:Q-info alyr)
        {:keys [q-alpha q-discount]} (:spec alyr)
        Q_T [:var "Q" [:sub "t"]]
        Q_T+1 [:var.text-nowrap "Q" [:sub "t+1"]]
        R_T+1 [:var.text-nowrap "R" [:sub "t+1"]]
        ]
    [:div
     [:h4 "Q learning"]
     [:small.text-muted
      "This is from 2 time steps back "
      [:abbr {:title
              (str "1. We wait one step (+1) to see the reward and Q
              value resulting from an action. "
                   "2. This display shows on the following step (+2)
                   ... because the Q-learning algorithm is applied
                   only after the time step has already been recorded
                   for display.")}
       "(why?)"]]
     [:table.table.table-condensed
      [:tr
       [:th R_T+1]
       [:td [:small "reward"]]
       [:td (-> (:reward qinfo 0) (.toFixed 2))]]
      [:tr
       [:th Q_T+1]
       [:td [:small "goodness"]]
       [:td (-> (:Q-prev qinfo 0) (.toFixed 3))]]
      [:tr
       [:th Q_T]
       [:td [:small "current"]]
       [:td (-> (:Q-val qinfo 0) (.toFixed 3))]]
      [:tr
       [:th [:var "n"]]
       [:td [:small "active synapses"]]
       [:td (:perms qinfo 0)]]
      ]
     [:p.text-right
      [:b "adjustment: "] [:br]
      [:abbr {:title (str "learning rate, alpha")} q-alpha]
      "("
      R_T+1
      " + "
      [:abbr {:title "discount factor"} q-discount]
      Q_T+1
      " - "
      Q_T
      ") = "
      [:mark
       (->> (:adj qinfo 0)
            (gstr/format "%+.3f"))]
      ]]))

(defn world-pane
  []
  (when-let [htm (main/selected-model-step)]
    (let [in-value (:value (first (core/input-seq htm)))
          DELTA (gstr/unescapeEntities "&Delta;")
          TIMES (gstr/unescapeEntities "&times;")]
      [:div
       [:p.muted [:small "Input on selected timestep."]]
       [:table.table.table-condensed
        [:tr
         [:th "x"]
         [:td [:small "position"]]
         [:td (:x in-value)]]
        [:tr
         [:th "y"]
         [:td [:small "objective"]]
         [:td (-> (:y in-value) (.toFixed 1))]]
        [:tr
         [:th (str DELTA "x")]
         [:td [:small "action"]]
         [:td (signed-str (:dx in-value))]]
        [:tr
         [:th (str DELTA "y")]
         [:td [:small "~reward"]]
         [:td (signed-str (:dy in-value))]]
        [:tr
         [:td {:colSpan 3}
          [:small DELTA "y " TIMES " 0.5 = " [:var "R"]]]]]
       (q-learning-sub-pane htm)
       ;; plot
       [resizing-canvas {:style {:width "100%"
                                 :height "240px"}}
        [main/model-steps main/selection]
        (fn [ctx]
          (let [htm (main/selected-model-step)
                in-value (:value (first (core/input-seq htm)))]
            (draw-world ctx in-value htm)))
        nil]
       [:small
        [:p [:b "top: "]
         "Approx Q values for each position/action combination,
            where green is positive and red is negative.
            These are the last seen Q values including last adjustments."]
        [:p [:b "middle: "]
         "Current position on the objective function surface."]
        [:p [:b "bottom: "]
         "Frequencies of being at each position."]]])))

(defn set-model!
  []
  (let [n-regions (:n-regions @config)]
    (with-ui-loading-message
      (main/set-model! (demo/make-model)))))

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
   [:p "The agent can move left or right on a reward surface. The
        reward is proportional to the change in y value after
        moving (dy)."]
   [:p "The action layer columns are interpreted to produce an
        action. 120 columns are allocated to each of the two
        directions of movement, where 40 are inhibitory and 80 are
        excitatory, and the direction with most overall excitation is
        used to move the agent."]
   [:p "The input is the location of the agent via coordinate
        encoder, plus the last movement as distal input."]
   [:p "This example is continuous, not episodic. Success is
        presumably indicated by the agent finding the optimum position
        and staying there."]

   [:h3 "HTM model"]
   [bind-fields config-template config]
   ]
  )

(defn ^:export init
  []
  (reagent/render [main/comportexviz-app model-tab world-pane]
                  (dom/getElement "comportexviz-app"))
  (reset! main/world world-c)
  (set-model!)
  (feed-world!))
