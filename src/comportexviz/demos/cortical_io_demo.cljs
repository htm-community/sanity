(ns comportexviz.demos.cortical-io-demo
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.cortical-io :refer [cortical-io-encoder
                                                     cache-fingerprint!]]
            [comportexviz.sentence-drawing :refer [draw-sentence-fn]]
            ;; ui
            [comportexviz.main]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [clojure.string :as str]
            [cljs.reader]
            [cljs.core.async :refer [<! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def spec
  {:column-dimensions [40 40]
   :ff-init-frac 0.15
   :ff-potential-radius 1.0
   :ff-perm-inc 0.05
   :ff-perm-dec 0.005
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 3
   :global-inhibition true
   :activation-level 0.015
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
   :distal-punish? true
   :proximal-vs-distal-weight 20
   :inhibition-base-distance 2
   :inhibition-speed 0.25
   })

(def n-predictions 5)

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

(defn ^:export input-gen
  [api-key text n-repeats decode-locally? spatial-scramble?]
  (let [cache (atom {})
        split-sens (split-sentences text)
        encoder (enc/pre-transform
                 (fn [[i j _]]
                   (get-in split-sens [i j]))
                 (cortical-io-encoder api-key cache
                                      :decode-locally? decode-locally?
                                      :spatial-scramble? spatial-scramble?))
        xform (input-transform-fn split-sens n-repeats)
        ;; [sentence index, word index, repeat number]
        inp (core/sensory-input [0 0 0] xform encoder)]
    ;; kick off the process to load the fingerprints
    (go
     (doseq [term (->> (apply concat split-sens)
                       (distinct)
                       (map str/lower-case))]
       (println "requesting fingerprint for:" term)
       ;; one request at a time (just has to keep ahead of sim)
       (<! (cache-fingerprint! api-key cache term))
       (<! (timeout 100))))
    (assoc inp :comportexviz/draw-input
           (draw-sentence-fn split-sens n-predictions))))

(defn ^:export n-region-model
  [input n]
  (core/regions-in-series core/sensory-region input n spec))

;; handle UI for input stream

(defn ^:export restart-from-ui
  []
  (let [api-key (dom/val (->dom "#comportex-api-key"))
        n-reps (cljs.reader/read-string
                (dom/val (->dom "#comportex-input-repeats")))
        decode-locally? (dom/val (->dom "#comportex-decode-local"))
        spatial-scramble? (dom/val (->dom "#comportex-scramble"))
        text (dom/val (->dom "#comportex-input-text"))]
    (go
     (let [input (input-gen api-key text n-reps decode-locally? spatial-scramble?)
           model (n-region-model input 1)]
       ;; allow some time for the first fingerprint request to cortical.io
       (<! (timeout 3000))
       (comportexviz.main.set-model model)))))

(defn ^:export handle-user-input-form
  []
  (let [form-el (->dom "#comportex-input-text-form")]
    (event/on-raw form-el :submit
                  (fn [e]
                    (restart-from-ui)
                    (.preventDefault e)
                    false))))
