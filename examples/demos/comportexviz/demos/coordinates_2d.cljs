(ns comportexviz.demos.coordinates-2d
  (:require [org.nfrac.comportex.demos.coordinates-2d :as demo]
            [org.nfrac.comportex.util :as util :refer [round]]
            [comportexviz.main :as main]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [cljs.reader]
            [goog.ui.TabPane]
            [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-ui-loading-message]]))

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

(def world-c (async/chan))

(def control-c (async/chan))

(defn set-world
  []
  (let [draw (draw-coords-fn demo/max-pos)]
    (main/set-world (->> world-c
                         (async/map< (util/keep-history-middleware
                                      50 #(select-keys % [:x :y :vx :vy])
                                      :history))
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))
    ;; feed the world channel continuously, reacting to UI settings
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
         (recur (demo/input-transform xx)))))))

(defn set-model-from-ui
  []
  (let [n-regions (cljs.reader/read-string
                   (dom/val (->dom "#comportex-n-regions")))]
    (with-ui-loading-message
      (main/set-model
       (demo/n-region-model n-regions)))))

(defn ^:export init
  []
  (goog.ui.TabPane. (.getElementById js/document "comportex-tabs"))
  (event/on-raw (->dom "#comportex-model-form") :submit
                (fn [e]
                  (set-model-from-ui)
                  (.preventDefault e)
                  false))
  (event/on-raw (->dom "#comportex-input-nudge-up") :click
                (fn [e]
                  (async/put! control-c #(update-in % [:ay] dec))
                  (.preventDefault e)
                  false))
  (event/on-raw (->dom "#comportex-input-nudge-down") :click
                (fn [e]
                  (async/put! control-c #(update-in % [:ay] inc))
                  (.preventDefault e)
                  false))
  (set-world)
  (set-model-from-ui))
