(ns comportexviz.sentence-drawing
  (:require [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :refer [round]]
            [monet.canvas :as c]
            [clojure.string :as str]))

(defn text-rotated
  [ctx {:keys [x y text]}]
  (c/save ctx)
  (c/translate ctx x y)
  (c/rotate ctx (/ Math/PI 2))
  (c/text ctx {:x 0 :y 0 :text text})
  (c/restore ctx))

(defn draw-sentence-fn
  "For sensory input of a sequence of sentences, each of which is a
   sequence of words. Returns a function used by viz-canvas to draw
   the input."
  [split-sentences n-predictions]
  (fn [this ctx w-px h-px rgn]
    (let [[curr-sen-i curr-word-j _] (p/domain-value this)
          curr-sen (get split-sentences curr-sen-i)
          pr-cols (->> (p/predictive-cells (:layer-3 rgn))
                       (map first))
          pr-votes (core/predicted-bit-votes rgn pr-cols)
          pr-words (p/decode (:encoder this) pr-votes n-predictions)
          left-x 5
          vf-x (- w-px 30)
          vpb-x (- w-px 5)
          sents-top 5
          curr-top (quot h-px 3)
          pr-top (* 2 (quot h-px 3))
          spacing 24]
      (c/save ctx)
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
      ;; predictions for next word
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
      (c/restore ctx))))
