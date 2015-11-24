(ns org.numenta.sanity.helpers
  (:require [org.numenta.sanity.util :refer [tap-c]]
            [goog.dom]
            [goog.dom.classes]
            [goog.style :as style]
            [goog.events :as events]
            [reagent.core :as reagent :refer [atom]]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util :refer [round]]
            [cljs.core.async :as async :refer [put! <!]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))

(defn- loading-message-element []
  (goog.dom/getElement "loading-message"))

(defn- show [el]
  (goog.dom.classes/add el "show"))

(defn- hide [el]
  (goog.dom.classes/remove el "show"))

(defn ui-loading-message-until
  [finished-c]
  (let [el (loading-message-element)]
    (show el)
    (go (let [x (<! finished-c)]
          (hide el)
          x))))

(defn with-ui-loading-message
  [f]
  (ui-loading-message-until
   (go
     ;; need a timeout to allow redraw to show loading message
     (<! (async/timeout 100))
     (f))))

(defn text-world-input-component
  [inval htm max-shown scroll-every separator]
  (let [time (p/timestep htm)
        show-n (- max-shown (mod (- max-shown time) scroll-every))
        history (->> (:history (meta inval))
                     (take-last show-n))]
    [:p
     (for [[i word] (map-indexed vector history)
           :let [t (+ i (- time (dec (count history))))
                 curr? (== time t)]]
       ^{:key (str word t)}
       [(if curr? :ins :span) (str word separator)]
       )]
    ))

(defn predictions-table
  [predictions]
  [:div
   [:table.table
    [:tbody
     [:tr
      [:th "prediction"]
      [:th "votes %"]
      [:th "votes per bit"]]
     (for [[j {:keys [value votes-frac votes-per-bit]
               }] (map-indexed vector predictions)]
       (let [txt value]
         ^{:key (str txt j)}
         [:tr
          [:td txt]
          [:td.text-right (str (round (* votes-frac 100)))]
          [:td.text-right (str (round votes-per-bit))]]
         ))
     ]]])

(defn text-world-predictions-component
  [htm n-predictions]
  (let [[_ e] (first (vals (:sensors htm)))
        rgn (first (core/region-seq htm))
        pr-votes (core/predicted-bit-votes rgn)
        predictions (p/decode e pr-votes n-predictions)]
    (predictions-table predictions)))

;;; canvas

(defn- canvas$call-draw-fn
  [component]
  ;; argv contains entire hiccup form, so it's shifted one to the right.
  (let [[_ _ _ _ _ draw] (reagent/argv component)]
    (draw (-> component
              reagent/dom-node
              (.getContext "2d")))))

(defn canvas [_ _ _ _ _]
  (reagent/create-class
   {:component-did-mount #(canvas$call-draw-fn %)

    :component-did-update #(canvas$call-draw-fn %)

    :display-name "canvas"
    :reagent-render (fn [props width height canaries _]
                      ;; Need to deref all atoms consumed by draw function to
                      ;; subscribe to changes.
                      (doseq [v canaries]
                        (when (satisfies? IDeref v)
                          @v))
                      [:canvas (assoc props
                                      :width width
                                      :height height)])}))

(defn window-resize-listener [resizes-c]
  "An empty component whose sole role is to pipe window resize events into a
   channel."
  (let [resize-key (atom nil)]
    (reagent/create-class
     {:component-did-mount (fn [component]
                             (reset! resize-key
                                     (events/listen js/window "resize"
                                                    #(put! resizes-c
                                                           :window-resized))))

      :component-will-unmount #(when @resize-key
                                 (events/unlistenByKey @resize-key))

      :display-name "window-resize-listener"
      :reagent-render (fn [_]
                        nil)})))

(defn- resizing-canvas$call-draw-fn
  [component]
  ;; argv contains entire hiccup form, so it's shifted one to the right.
  (let [[_ _ _ draw _] (reagent/argv component)]
    (draw (-> component
              reagent/dom-node
              (.getContext "2d")))))

(defn resizing-canvas [_ _ _ invalidates-c resizes]
  "A canvas that syncs its internal dimensions with its layout dimensions.
  This sync happens immediately after the canvas is mounted in the DOM, and
  subsequently whenever a value comes through the `invalidates-mult`."
  (let [width-px (atom nil)
        height-px (atom nil)
        invalidates-c (or invalidates-c (async/chan))
        teardown-c (async/chan)]
    (reagent/create-class
     {:component-did-mount (fn [component]
                             (go-loop []
                               (when (not (nil? (alt! teardown-c nil
                                                      invalidates-c true
                                                      :priority true)))
                                 (let [size-px (-> component
                                                   reagent/dom-node
                                                   style/getSize)
                                       w (.-width size-px)
                                       h (.-height size-px)]
                                   (reset! width-px w)
                                   (reset! height-px h)
                                   (when resizes
                                     (put! resizes [w h])))
                                 (recur)))
                             (put! invalidates-c :initial-mount))

      :component-did-update #(resizing-canvas$call-draw-fn %)

      :component-will-unmount #(async/close! teardown-c)

      :display-name "resizing-canvas"
      :reagent-render (fn [props canaries _ _]
                        ;; Need to deref all atoms consumed by draw function to
                        ;; subscribe to changes.
                        (doseq [v canaries]
                          (when (satisfies? IDeref v)
                            @v))
                        (let [w @width-px
                              h @height-px]
                          (when (or (zero? w) (zero? h))
                            ;; The "right" way around this is to not use
                            ;; display:none, and instead simply exclude
                            ;; undisplayed nodes from the component's render fn.
                            (js/console.warn
                             (str "The resizing canvas is size " w " " h
                                  ". If it's 'display:none`, it won't detect "
                                  "its own visibility change.")))
                          [:canvas (assoc props
                                          :width w
                                          :height h)]))})))
