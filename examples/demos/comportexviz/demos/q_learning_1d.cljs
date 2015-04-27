(ns comportexviz.demos.q-learning-1d
  (:require [org.nfrac.comportex.demos.q-learning-1d :as demo]
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

(def config
  (atom {:n-regions 1}))

(declare draw-surface-fn)

(def world-c
  (async/chan (async/buffer 1)
              (comp (map (util/frequencies-middleware :x :freqs))
                    (map #(vary-meta % assoc
                                     :comportexviz/draw-world
                                     (draw-surface-fn demo/surface))))))

(defn feed-world!
  "Feed the world input channel continuously, selecting actions from
  state of model itself."
  []
  (let [step-c (main/tap-c main/steps-mult)]
    (demo/feed-world-c-with-actions! step-c world-c main/model)))

(defn draw-surface-fn
  [surface]
  (let [x-max (count surface)
        y-max (reduce max surface)
        x-lim [(- 0 1) (+ x-max 1)]
        y-lim [(+ y-max 1) 0]
        surface-xy (mapv vector (range) surface)]
    (fn [this ctx left-px top-px w-px h-px state]
      (let [[plot-x plot-y] [0 160]
            plot-size {:w (- w-px 0)
                       :h 100}
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
        ;; draw the plots
        (c/translate ctx plot-x plot-y)
        ;; draw Q values for right/left at each position
        (let [qplot-size {:w (:w plot-size) :h 40}
              qplot-lim [0 2]
              qplot (plt/xy-plot ctx qplot-size x-lim qplot-lim)]
          (plt/frame! qplot)
          (doseq [[state-action q] (:Q-map this)
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
          (plt/point! plot (:x this) (:y this) 4))
        ;; histogram
        (c/translate ctx 0 (:h plot-size))
        (let [freqs (:freqs (meta this))
              hist-lim [0 (inc (apply max (vals freqs)))]
              histogram (plt/xy-plot ctx plot-size x-lim hist-lim)]
          ;; draw the plot
          ;(plt/frame! histogram)
          (c/stroke-style ctx "black")
          (doseq [[x f] freqs]
            (plt/line! histogram [[x 0] [x f]])))
        (c/restore ctx)
        ))))

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
   [:p "Highly experimental attempt at integrating Q learning
        (reinforcement learning). The Q value of an action from some state
        is the average permanence of synapses activating the action
        from that state, minus the initial permanence value."]
   [:p "The action layer columns are interpreted to produce an
        action: 120 columns are allocated to each of the two
        directions of movement, where 40 are inhibitory and 80 are
        excitatory, and the direction with most overall excitation is
        used to move the agent."]
   [:p "Adjustments to the Q value, based on reward and expected
        future reward, are applied to the permanence of synapses which
        directly activated the action (columns). This adjustment
        applies in the action layer only, and it does this instead of
        the usual spatial pooling."]
   [:p "Exploration arises from boosting neglected columns,
        primarily in the action layer."]
   [:p "The reward is the increase in y value after moving (dy)."]
   [:p "This example is continuous, not episodic."]
   [:p "The input is the location of the agent via coordinate
        encoder, plus the last movement as distal input."]

   [:h3 "HTM model"]
   [bind-fields config-template config]
   ]
  )

(defn ^:export init
  []
  (reagent/render (main/comportexviz-app model-tab)
                  (dom/getElement "comportexviz-app"))
  (reset! main/world world-c)
  (set-model!)
  (feed-world!))
