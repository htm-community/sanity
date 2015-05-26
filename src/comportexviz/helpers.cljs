(ns comportexviz.helpers
  (:require [goog.dom]
            [goog.dom.classes]
            [goog.style :as style]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :refer [round]]))

(defn with-ui-loading-message
  [f]
  (let [el (goog.dom/getElement "loading-message")]
     (goog.dom.classes/add el "show")
     ;; need a timeout to allow redraw to show loading message
     (js/setTimeout (fn []
                      (try
                        (f)
                        (finally
                          (goog.dom.classes/remove el "show"))))
                    100)))

(defn text-world-input-component
  [in-value htm max-shown scroll-every separator]
  (let [time (p/timestep htm)
        rgn (first (core/region-seq htm))
        show-n (- max-shown (mod (- max-shown time) scroll-every))
        history (->> (:history (meta in-value))
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
  [in-value htm n-predictions]
  (let [inp (first (core/input-seq htm))
        rgn (first (core/region-seq htm))
        pr-votes (core/predicted-bit-votes rgn)
        predictions (p/decode (:encoder inp) pr-votes n-predictions)]
    (predictions-table predictions)))

(defn set-canvas-pixels-from-element-size!
  [el min-px-width]
  (let [size-px (style/getSize el)
        width-px (-> (.-width size-px)
                     (max min-px-width))
        height-px (.-height size-px)]
    (set! (.-width el) width-px)
    (set! (.-height el) height-px)))
