(ns comportexviz.demos.q-learning-2d
  (:require [org.nfrac.comportex.demos.q-learning-2d :as demo]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            [comportexviz.main :as main]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [goog.string :as gstr]
            [goog.string.format]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(def model-config
  (atom {:n-regions 1}))

(def world-c (async/chan))

(defn draw-surface-fn
  [surface]
  (let [x-max (count surface)
        y-max (count (first surface))
        x-lim [0 x-max]
        y-lim [0 y-max]]
    (fn [this ctx left-px top-px w-px h-px state]
      (let [[plot-x plot-y] [10 160]
            plot-size {:w 120
                       :h 120}
            plot (plt/xy-plot ctx plot-size x-lim y-lim)
            label-y 10
            alyr (get-in state [:regions :action :layer-3])
            qinfo (get-in alyr [:prior-state :Q-info])
            {:keys [q-alpha q-discount]} (:spec alyr)]
        (c/save ctx)
        (c/translate ctx left-px top-px)
        ;; draw current value
        (doto ctx
          (c/fill-style "black")
          (c/font-style "14px monospace")
          (c/text {:x plot-x :y label-y
                   :text (str "x " (:x this)
                              " dx " (if (neg? (:dx this)) "" "+") (:dx this))})
          (c/text {:x plot-x :y (+ label-y 20)
                   :text (str "y " (:y this)
                              " dy "(if (neg? (:dy this)) "" "+") (:dy this))})
          (c/font-style "12px sans-serif")
          (c/text {:x plot-x :y (+ label-y 40)
                   :text (str "prior: reward " (gstr/format "%.2f" (:reward qinfo 0)))})
          (c/text {:x plot-x :y (+ label-y 60)
                   :text (str "Q=" (gstr/format "%.3f" (:Qt qinfo 0))
                              " Qn=" (gstr/format "%.3f" (:Q-val (:prior-state alyr) 0))
                              )})
          (c/text {:x plot-x :y (+ label-y 80)
                   :text "adjustment:"})
          (c/text {:x plot-x :y (+ label-y 100)
                   :text (str (gstr/format "%.2f" q-alpha)
                              "(R + " (gstr/format "%.2f" q-discount)
                              "[Qn] - Q)")})
          (c/text {:x plot-x :y (+ label-y 120)
                   :text (str " = " (gstr/format "%.3f" (:adj qinfo 0)))}))
        ;; draw the plot
        (c/translate ctx plot-x plot-y)
        (plt/frame! plot)
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
        (doseq [[state-action q] (:Q-map this)
                :let [{:keys [x y dx dy]} state-action]]
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
        (c/stroke-style ctx "yellow")
        (c/fill-style ctx "#6666ff")
        (plt/point! plot (+ 0.5 (:x this)) (+ 0.5 (:y this)) 4)
        (c/stroke-style ctx "black")
        (plt/grid! plot {})
        (c/restore ctx)
        ))))

(defn set-world!
  []
  (let [draw (draw-surface-fn demo/surface)]
    (main/set-world (->> world-c
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))
    ;; feed the world input channel continuously, selecting actions
    ;; from state of model itself
    (let [step-c (main/tap-c main/steps-mult)]
      (demo/feed-world-c-with-actions! step-c world-c main/model))))

(defn set-model!
  []
  (let [n-regions (:n-regions @model-config)]
    (with-ui-loading-message
      (main/set-model
       (demo/make-model)))))

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
   [:p "Highly experimental attempt at integrating Q learning
        (reinforcement learning). The Q value of an action from some state
        is the average permanence of synapses activating the action
        from that state, minus the initial permanence value."]
   [:p "The action layer columns are interpreted to produce an
        action: 100 columns are allocated to each of the four
        directions of movement, and the direction with most active
        columns is used to move the agent."]
   [:p "Adjustments to the Q value, based on reward and expected
        future reward, are applied to the permanence of synapses which
        directly activated the action (columns). This adjustment
        applies in the action layer only, and it does this instead of
        the usual spatial pooling."]
   [:p "Exploration arises from boosting neglected columns,
        primarily in the action layer."]
   [:p "The reward is -3 on normal squares, -200 on hazard squares
        and +200 on the goal square. These are divided by 100 for
        comparison to Q values on the synaptic permanence scale."]
   [:p "This example is episodic: when the agent reaches either the
        goal or a hazard it is returned to the starting point."]
   [:p "The input is the location of the agent via coordinate
        encoder, plus the last movement as distal input."]

   [:h3 "HTM model"]
   [bind-fields model-config-template model-config]
   ]
  )

(defn ^:export init
  []
  (reagent/render (main/comportexviz-app model-tab)
                  (dom/getElement "comportexviz-app"))
  (set-model!)
  (set-world!))
