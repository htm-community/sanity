(ns comportexviz.demos.sensorimotor-1d
  (:require [org.nfrac.comportex.demos.sensorimotor-1d :as demo]
            [comportexviz.main :as main]
            [comportexviz.plots-canvas :as plt]
            [monet.canvas :as c]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [cljs.reader]
            [goog.ui.TabPane]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(def item-colors
  (zipmap demo/items
          (for [i (range 10)
                :let [hue (* i 36)
                      lig (if (even? i) 70 30)]]
            (str "hsl(" hue ",100%," lig "%)"))))

(def item-text-colors
  (zipmap demo/items
          (for [i (range 10)]
            (if (even? i) "black" "white"))))

(defn draw-eye
  [ctx {:keys [x y angle radius]}]
  (c/save ctx)
  (let [pi2 (/ Math/PI 2)]
    (c/begin-path ctx)
    (c/arc ctx {:x x :y y :r radius
                :start-angle (- pi2) :end-angle pi2
                :counter-clockwise? true})
    (c/close-path ctx)
    (c/fill-style ctx "white")
    (c/fill ctx)
    (c/stroke-style ctx "black")
    (c/stroke ctx)
    (c/clip ctx)
    (let [pupil-x (+ x (* radius (Math/cos angle)))
          pupil-y (+ y (* radius (Math/sin angle)))]
      (c/circle ctx {:x pupil-x :y pupil-y
                     :r (quot radius 2)})
      (c/fill-style ctx "rgb(128,128,255)")
      (c/fill ctx)
      (c/circle ctx {:x pupil-x :y pupil-y
                     :r (quot radius 5)})
      (c/fill-style ctx "black")
      (c/fill ctx)))
  (c/restore ctx))

(defn draw-world
  [this ctx left-px top-px w-px h-px state]
  (let [[plot-x plot-y] [10 100]
        {:keys [field position next-saccade]} this
        item-w 20
        x-lim [0 1]
        y-lim [0 (count field)]
        plot-size {:w (- w-px 20)
                   :h (* (count field) item-w)}
        plot (plt/xy-plot ctx plot-size x-lim y-lim)
        x-scale (plt/scale-fn x-lim (:w plot-size))
        y-scale (plt/scale-fn y-lim (:h plot-size))]
    (c/save ctx)
    (c/translate ctx left-px top-px)
    ;; draw coordinates text
    (doto ctx
      (c/fill-style "black")
      (c/font-style "14px monospace")
      (c/text {:x plot-x :y (quot plot-y 2)
               :text (str "val=" (get field position)
                          ", next" (if (neg? next-saccade) "" "+")
                          next-saccade)}))
    ;; draw the plot
    (c/translate ctx plot-x plot-y)
    (plt/frame! plot)
    (c/stroke-style ctx "black")
    (c/font-style ctx "bold 14px monospace")
    (c/text-baseline ctx :middle)
    (doseq [[y item] (map-indexed vector field)
            :let [rect {:x 0 :y (y-scale y)
                        :w item-w :h (y-scale 1)}]]
      (doto ctx
        (c/fill-style (item-colors item))
        (c/fill-rect rect)
        (c/stroke-rect rect)
        (c/fill-style (item-text-colors item))
        (c/text {:x 5 :y (y-scale (+ y 0.5)) :text (name item)})))
    (let [focus-x 10
          focus-y (y-scale (+ 0.5 position))
          next-focus-y (y-scale (+ 0.5 position next-saccade))
          eye-x (:w plot-size)
          eye-y (quot (:h plot-size) 2)]
      (doto ctx
        ;; draw line of next position (after next saccade)
        (c/begin-path)
        (c/move-to eye-x eye-y)
        (c/line-to focus-x next-focus-y)
        (c/stroke-style "lightgrey")
        (c/stroke)
        ;; draw line of current position
        (c/begin-path)
        (c/move-to eye-x eye-y)
        (c/line-to focus-x focus-y)
        (c/stroke-style "black")
        (c/stroke)
        ;; draw eye
        (draw-eye {:x eye-x
                   :y eye-y
                   :angle (Math/atan2 (- focus-y eye-y)
                                      (- focus-x eye-x))
                   :radius 30}))))
  (c/restore ctx))

(def world-c (async/chan 100))

(defn put-input-steps
  [field-key n-steps]
  (let [field (demo/fields field-key)]
    (async/onto-chan world-c
                     (take n-steps (iterate demo/input-transform
                                            {:field field
                                             :position (quot (count field) 2)
                                             :next-saccade 1}))
                     false)))

(defn set-world
  []
  (main/set-world (->> world-c
                       (async/map< #(vary-meta % assoc
                                               :comportexviz/draw-world
                                               draw-world)))))

(defn ^:export set-n-region-model
  [n]
  (with-ui-loading-message
    (main/set-model (demo/n-region-model n))))

(defn ^:export init
  []
  (goog.ui.TabPane. (.getElementById js/document "comportex-tabs"))
  (let [form-el (->dom "#comportex-input-form")
        field-el (->dom "#comportex-input-field")]
    (doseq [[i k] (map-indexed vector (keys demo/fields))]
      (dom/append! field-el
                   [:option {:value (name k)
                             :selected (when (zero? i) "selected")}
                    (name k)]))
    (event/on-raw form-el :submit
                  (fn [e]
                    (let [field-key (keyword (dom/val field-el))
                          n-steps (cljs.reader/read-string
                                   (dom/val (->dom "#comportex-input-n-steps")))]
                      (put-input-steps field-key n-steps))
                    (.preventDefault e)
                    false)))
  (set-world)
  (set-n-region-model 1)
  (reset! main/sim-go? true))
