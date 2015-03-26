(ns comportexviz.demos.q-learning-1d
  (:require [org.nfrac.comportex.demos.q-learning-1d :as demo]
            [org.nfrac.comportex.util :as util :refer [round abs]]
            [comportexviz.main :as main]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [cljs.reader]
            [goog.string :as gstr]
            [goog.string.format]
            [goog.ui.TabPane]
            [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-ui-loading-message]]))

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

(def world-c (async/chan))

(defn set-world
  []
  (let [draw (draw-surface-fn demo/surface)]
    (main/set-world (->> world-c
                         (async/map< (util/frequencies-middleware :x :freqs))
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))
    ;; feed the world input channel continuously, selecting actions
    ;; from state of model itself
    (let [step-c (main/tap-c main/steps-mult)]
      (demo/feed-world-c-with-actions! step-c world-c main/model))))

(defn set-model-from-ui
  []
  (let [n-regions (cljs.reader/read-string
                   (dom/val (->dom "#comportex-n-regions")))]
    (with-ui-loading-message
      (main/set-model
       (demo/make-model)))))

(defn ^:export init
  []
  (goog.ui.TabPane. (.getElementById js/document "comportex-tabs"))
  (event/on-raw (->dom "#comportex-model-form") :submit
                (fn [e]
                  (set-model-from-ui)
                  (.preventDefault e)
                  false))
  (set-model-from-ui)
  (set-world))
