(ns comportexviz.demos.cortical-io-demo
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [comportexviz.cortical-io :refer [cortical-io-encoder]]
            [comportexviz.sentence-drawing :refer [draw-sentence-fn]]
            ;; ui
            [comportexviz.main]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [clojure.string :as str]
            [cljs.reader]
            [cljs.core.async :refer [>! <! chan put!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def spec
  {:column-dimensions [40 40]
   :ff-init-frac 0.30
   :ff-potential-radius 0.15
   :ff-perm-inc 0.05
   :ff-perm-dec 0.005
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 3
   :global-inhibition false
   :activation-level 0.03
   :duty-cycle-period 100000
   :max-boost 2.0
   ;; sequence memory:
   :depth 8
   :max-segments 5
   :seg-max-synapse-count 18
   :seg-new-synapse-count 12
   :seg-stimulus-threshold 9
   :seg-learn-threshold 6
   :distal-perm-connected 0.20
   :distal-perm-inc 0.05
   :distal-perm-dec 0.01
   :distal-perm-init 0.16
   :distal-punish? false
   :inhibition-base-distance 2
   :inhibition-speed 0.25
   })

(def n-predictions 8)

(def min-votes 2)

(defn split-sentences
  [text]
  (->> (str/split (str/trim text) #"[^\w]*[\.\!\?]+[^\w]*")
       (mapv #(str/split % #"[^\w']+"))
       (mapv #(conj % "."))))

(defn input-transform-fn
  "Returns an input transform function of [[i j rep]]
   [sentence index, word index, repeat number]"
  [split-sentences n-repeats]
  (fn [[i j rep]]
    (let [sen (get split-sentences i)
          n-sen (count split-sentences)]
      ;; check end of a sentence
      (if (== j (dec (count sen)))
        ;; reached the end of a sentence
        (if (== rep (dec n-repeats))
          ;; finished repeating this sentence, move on
          [(mod (inc i) n-sen)
           0
           0]
          ;; next repeat
          [i
           0
           (inc rep)])
        ;; continuing this sentence
        [i (inc j) rep]))))

(defn sensory-input-from-text
  [api-key text n-repeats]
  (let [split-sens (split-sentences text)
        terms (distinct (apply concat split-sens))
        encoder (enc/pre-transform (fn [[i j _]]
                                     (get-in split-sens [i j]))
                                   (cortical-io-encoder api-key min-votes
                                                        terms))
        xform (input-transform-fn split-sens n-repeats)]
    ;; [sentence index, word index, repeat number]
    (core/sensory-input [0 0 0] xform encoder)))

(defn ^:export input-gen
  [api-key text n-repeats]
  (let [inp (sensory-input-from-text api-key text n-repeats)
        split-sens (split-sentences text)
        draw-inp (draw-sentence-fn split-sens n-predictions)]
    (assoc inp :comportexviz/draw-input draw-inp)))

(defn ^:export n-region-model
  [api-key text n-repeats n]
  (core/regions-in-series core/sensory-region
                          (input-gen api-key text n-repeats) n spec))

;; handle UI for input stream

(defn ^:export restart-from-ui
  []
  (let [api-key (dom/val (->dom "#comportex-api-key"))
        n-reps (cljs.reader/read-string
                (dom/val (->dom "#comportex-input-repeats")))
        text (dom/val (->dom "#comportex-input-text"))]
    (comportexviz.main.set-model (n-region-model api-key text n-reps 1))))

(defn ^:export handle-user-input-form
  []
  (let [form-el (->dom "#comportex-input-text-form")]
    (event/on-raw form-el :submit
                  (fn [e]
                    (restart-from-ui)
                    (.preventDefault e)
                    false))))


