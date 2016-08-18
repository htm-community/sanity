(ns org.numenta.sanity.demos.q-learning-1d
  (:require [org.nfrac.comportex.demos.q-learning-1d :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util :refer [round abs]]
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
              (comp (map (util/frequencies-middleware :x :freqs))
                    (map #(assoc % :label (:x %))))))

(def into-sim (async/chan))

(def model (atom nil))

(defn draw-world
  [ctx inval]
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
      (doseq [[state-action q] (:Q-map inval)
              :let [{:keys [x action]} state-action
                    dx (:dx action)]]
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
    ;; draw surface
    (c/translate ctx 0 40)
    (let [plot (plt/xy-plot ctx plot-size x-lim y-lim)]
      (plt/frame! plot)
      (c/stroke-style ctx "lightgray")
      (plt/grid! plot {})
      (c/stroke-style ctx "black")
      (plt/line! plot surface-xy)
      ;; current position
      (let [dx-1 (:dx (:prev-action inval))
            x (:x inval)
            y (:y inval)
            x-1 (- x dx-1)
            y-1 (surface x-1)]
        (c/stroke-style ctx "#888")
        (c/fill-style ctx "white")
        (plt/point! plot x-1 y-1 3)
        (c/stroke-style ctx "black")
        (c/fill-style ctx "yellow")
        (plt/point! plot x y 4)))
    ;; histogram
    (c/translate ctx 0 (:h plot-size))
    (let [freqs (:freqs (meta inval))
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
        Q_T-1 [:var.text-nowrap "Q" [:sub "t-1"]]
        R_T [:var.text-nowrap "R" [:sub "t"]]
        ]
    [:div
     [:h4 "Q learning"]
     [:table.table.table-condensed
      [:tr
       [:th R_T]
       [:td [:small "reward"]]
       [:td (-> (:reward qinfo 0) (.toFixed 2))]]
      [:tr
       [:th Q_T]
       [:td [:small "goodness"]]
       [:td (-> (:Q-val qinfo 0) (.toFixed 3))]]
      [:tr
       [:th Q_T-1]
       [:td [:small "previous"]]
       [:td (-> (:Q-old qinfo 0) (.toFixed 3))]]
      [:tr
       [:th [:var "n"]]
       [:td [:small "active synapses"]]
       [:td (:perms qinfo 0)]]
      ]
     [:p.text-right
      [:b "adjustment: "] [:br]
      [:abbr {:title (str "learning rate, alpha")} q-alpha]
      "("
      R_T
      " + "
      [:abbr {:title "discount factor"} q-discount]
      Q_T
      " - "
      Q_T-1
      ") = "
      [:mark
       (->> (:adj qinfo 0)
            (gstr/format "%+.3f"))]
      ]]))

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
           [:p.muted [:small "Reward " [:var "R"] " = " DELTA "y " TIMES " 0.5"]]
           [:table.table.table-condensed
            [:tr
             [:th "x"]
             [:td [:small "position"]]
             [:td (:x inval)]]
            [:tr
             [:th (str DELTA "x")]
             [:td [:small "action"]]
             [:td (signed-str (:dx (:prev-action inval)))]]
            [:tr
             [:th (str DELTA "y")]
             [:td [:small "~reward"]]
             [:td (signed-str (:dy inval))]]
            [:tr
             [:th (str DELTA "x") [:sub "t+1"]]
             [:td [:small "action"]]
             [:td (signed-str (:dx (:action inval)))]]]
           (q-learning-sub-pane htm)
           ;; plot
           [resizing-canvas {:style {:width "100%"
                                     :height "240px"}}
            [main/selection]
            (fn [ctx]
              (let [step (main/selected-step)
                    inval (:input-value step)]
                (draw-world ctx inval)))
            nil]
           [:small
            [:p [:b "top: "]
             "Approx Q values for each position/action combination,
            where green is positive and red is negative.
            These are the last seen Q values including last adjustments."]
            [:p [:b "middle: "]
             "Current position on the objective function surface."]
            [:p [:b "bottom: "]
             "Frequencies of being at each position."]]])))))

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
        ;; seed input
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
   [:p "The agent can move left or right on a reward surface. The
        reward is proportional to the change in y value after
        moving (dy)."]
   [:p "The action layer columns are interpreted to produce an
        action. 15 columns are allocated to each of the two directions
        of movement, and the direction with most active columns is
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
  (reagent/render [main/sanity-app "Comportex" [model-tab] [world-pane]
                   (atom :model) all-features into-sim]
                  (dom/getElement "sanity-app"))
  (set-model!))
