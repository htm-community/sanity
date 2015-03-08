(ns comportexviz.sentence-drawing
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :refer [round]]
            [monet.canvas :as c]
            [clojure.string :as str]
            [cljs.core.async :refer [<!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn text-rotated
  [ctx {:keys [x y text]}]
  (c/save ctx)
  (c/translate ctx x y)
  (c/rotate ctx (/ Math/PI 2))
  (c/text ctx {:x 0 :y 0 :text text})
  (c/restore ctx))

(defn draw-sentence-fn
  "For sensory input of a sequence of sentences, each of which is a
   sequence of words. Assumes input value has a key :index containing
   [i j], indices for the current sentence and word. Returns a
   function used by viz-canvas to draw the world (input)."
  [split-sentences n-predictions]
  (fn [this ctx left-px top-px w-px h-px state]
    (let [inp (first (core/input-seq state))
          rgn (first (core/region-seq state))
          [curr-sen-i curr-word-j] (:index this)
          curr-sen (get split-sentences curr-sen-i)
          pr-votes (core/predicted-bit-votes rgn)
          left-x 5
          vf-x (- w-px 30)
          vpb-x (- w-px 5)
          sents-top 5
          curr-top (quot h-px 3)
          pr-top (* 2 (quot h-px 3))
          spacing 24]
      (c/save ctx)
      (c/translate ctx left-px top-px)
      (c/font-style ctx "small-caps 14px sans-serif")
      (c/text-align ctx :right)
      (text-rotated ctx {:text "votes %" :x vf-x :y (+ pr-top 14)})
      (text-rotated ctx {:text "votes per bit" :x vpb-x :y (+ pr-top 14)})
      (c/font-style ctx "small-caps bold 14px sans-serif")
      (c/text-baseline ctx :top)
      (c/text-align ctx :left)
      (c/text ctx {:text "Sentences" :x left-x :y sents-top})
      (c/text ctx {:text "Current sentence" :x left-x :y curr-top})
      (c/text ctx {:text "Predictions" :x left-x :y pr-top})
      (c/font-style ctx "12px sans-serif")
      ;; previous and next sentences
      (doseq [[k [i sen]] (->> split-sentences
                               (map-indexed vector)
                               (drop (- curr-sen-i 4))
                               (take 9)
                               (map-indexed vector))
              :let [y (+ sents-top (* spacing (inc k)))]]
        (when (== i curr-sen-i)
          (c/fill-style ctx "#eee")
          (c/fill-rect ctx {:x 0 :y (- y (quot spacing 4))
                            :w w-px :h spacing})
          (c/fill-style ctx "#000"))
        (c/text ctx {:text (str/join " " sen) :x left-x :y y}))
      ;; current sentence words
      (doseq [[j word] (map-indexed vector curr-sen)
              :let [y (+ curr-top (* spacing (inc j)))]]
        (when (== j curr-word-j)
          (c/fill-style ctx "#eee")
          (c/fill-rect ctx {:x 0 :y (- y (quot spacing 4))
                            :w w-px :h spacing})
          (c/fill-style ctx "#000"))
        (c/text ctx {:text word :x left-x :y y}))
      (c/restore ctx)
      ;; predictions for next word - asynchronously
      (go
       (let [pr-words (p/decode (:encoder inp) pr-votes n-predictions)
             ;; decode may return a channel for async calls
             pr-words (if-let [c (:channel pr-words)]
                        (<! c)
                        pr-words)]
         (c/save ctx)
         (c/translate ctx left-px top-px)
         (c/clear-rect ctx {:x 0 :y (+ pr-top spacing)
                            :w w-px :h h-px})
         (c/text-baseline ctx :top)
         (doseq [[j {:keys [value votes-frac votes-per-bit]
                     }] (map-indexed vector pr-words)]
           (let [jy (+ pr-top (* spacing (inc j)))
                 txt value]
             (c/text-align ctx :left)
             (c/text ctx {:text txt :x left-x :y jy})
             (c/text-align ctx :right)
             (c/text ctx {:text (str (round (* votes-frac 100)))
                          :x vf-x :y jy})
             (c/text ctx {:text (str (round votes-per-bit))
                          :x vpb-x :y jy})))
         (c/restore ctx))))))

(defn draw-text-fn
  "For sensory input of a sequence of letters. Assumes input value has
   metadata key :history with a queue of recent and current letters.
   Returns a function used by viz-canvas to draw the world (input)."
  [n-predictions]
  (fn [this ctx left-px top-px w-px h-px state]
    (let [t (p/timestep state)
          inp-id (first (core/input-keys state))
          inp (get-in state [:inputs inp-id])
          ;; region this input feeds to, for predictions
          ff-rgn-id (first (get-in state [:fb-deps inp-id]))
          rgn (get-in state [:regions ff-rgn-id])
          pr-votes (core/predicted-bit-votes rgn)
          char-px 12
          line-px 20
          per-line (quot w-px char-px)
          n-lines (quot (quot h-px 3) line-px)
          max-n (* per-line n-lines)
          this-n (- max-n (mod (- max-n t) per-line)) ;; accumulate last line
          history (->> (:history (meta this))
                       (take-last this-n))
          left-x 5
          vf-x (- w-px 30)
          vpb-x (- w-px 5)
          input-top 5
          pred-top (quot h-px 2)
          spacing 24]
      (c/save ctx)
      (c/translate ctx left-px top-px)
      (c/fill-style ctx "black")
      (c/font-style ctx "small-caps 14px sans-serif")
      (c/text-align ctx :right)
      (text-rotated ctx {:text "votes %" :x vf-x :y (+ pred-top 14)})
      (text-rotated ctx {:text "votes per bit" :x vpb-x :y (+ pred-top 14)})
      (c/font-style ctx "small-caps bold 14px sans-serif")
      (c/text-baseline ctx :top)
      (c/text-align ctx :left)
      (c/text ctx {:text "Predictions" :x left-x :y pred-top})
      (c/font-style ctx "12px monospace")
      (doseq [[i s] (map-indexed vector history)
              :let [y-px (+ input-top (* line-px (quot i per-line)))
                    x-px (+ left-x (* char-px (rem i per-line)))
                    curr? (== i (dec (count history)))]]
        (c/text ctx {:text s :x x-px :y y-px})
        (when curr?
          (c/fill-style ctx "darkgreen")
          (c/text ctx {:text " |" :x x-px :y y-px})))
      (c/restore ctx)
      ;; predictions for next word - asynchronously
      (go
       (let [pr-words (p/decode (:encoder inp) pr-votes n-predictions)
             ;; decode may return a channel for async calls
             pr-words (if-let [c (:channel pr-words)]
                        (<! c)
                        pr-words)]
         (c/save ctx)
         (c/translate ctx left-px top-px)
         (c/clear-rect ctx {:x 0 :y (+ pred-top spacing)
                            :w w-px :h h-px})
         (c/text-baseline ctx :top)
         (doseq [[j {:keys [value votes-frac votes-per-bit]
                     }] (map-indexed vector pr-words)]
           (let [jy (+ pred-top (* spacing (inc j)))
                 txt value]
             (c/text-align ctx :left)
             (c/text ctx {:text txt :x left-x :y jy})
             (c/text-align ctx :right)
             (c/text ctx {:text (str (round (* votes-frac 100)))
                          :x vf-x :y jy})
             (c/text ctx {:text (str (round votes-per-bit))
                          :x vpb-x :y jy})))
         (c/restore ctx))))))
