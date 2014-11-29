(ns comportexviz.cortical-io-demo
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.cortical-io :as cio
             :refer [cortical-io-encoder
                     cache-fingerprint!]]
            [comportexviz.sentence-drawing :refer [draw-sentence-fn]]
            ;; ui
            [comportexviz.main :as main]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [clojure.string :as str]
            [cljs.reader]
            [cljs.core.async :as async :refer [<! timeout]]
            [goog.ui.TabPane])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-ui-loading-message]]))

(def spec-global
  {:column-dimensions [40 40]
   :ff-init-frac 0.20
   :ff-potential-radius 1.0
   :ff-perm-inc 0.05
   :ff-perm-dec 0.005
   :ff-perm-connected 0.20
   :ff-stimulus-threshold 1
   :global-inhibition? true
   :activation-level 0.02
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
   :distal-vs-proximal-weight 0
   })

(def spec-local
  (assoc spec-global
    :ff-init-frac 0.30
    :ff-potential-radius 0.20
    :global-inhibition? false
    :inhibition-base-distance 1))

(def n-predictions 5)

(defn split-sentences
  [text]
  (->> (str/split (str/trim text) #"[^\w]*[\.\!\?]+[^\w]*")
       (mapv #(str/split % #"[^\w']+"))
       (mapv #(conj % "."))))

(defn word-item-seq
  "An input sequence consisting of words from the given text, with
   periods separating sentences also included as distinct words. Each
   sequence element has the form `{:word _, :index [i j]}`, where i is
   the sentence index and j is the word index into sentence j."
  [n-repeats text]
  (for [[i sen] (map-indexed vector (split-sentences text))
        rep (range n-repeats)
        [j word] (map-indexed vector sen)]
    {:word word :index [i j]}))

(defn cio-start-requests!
  "Kicks off the process to load the fingerprints."
  [api-key text cache]
  (go
   (doseq [term (distinct (apply concat (split-sentences (str/lower-case text))))]
     (println "requesting fingerprint for:" term)
     ;; one request at a time (just has to keep ahead of sim)
     (<! (cache-fingerprint! api-key cache term)))))

(defn ^:export reset-world
  [text n-reps]
  (let [draw-fn (draw-sentence-fn (split-sentences text) n-predictions)
        world-seq (->> (word-item-seq n-reps text)
                       (map #(vary-meta % assoc
                                        :comportexviz/draw-world draw-fn)))
        world (doto (async/chan)
                (async/onto-chan world-seq))]
    (main/set-world world)))

;; handle UI for input stream

(defn ^:export restart-from-ui
  []
  (let [api-key (dom/val (->dom "#comportex-api-key"))
        n-reps (cljs.reader/read-string
                (dom/val (->dom "#comportex-input-repeats")))
        enc-choice (dom/val (->dom "#comportex-encoder"))
        decode-locally? (dom/val (->dom "#comportex-decode-local"))
        spatial-scramble? (dom/val (->dom "#comportex-scramble"))
        text (dom/val (->dom "#comportex-input-text"))
        spec-choice (dom/val (->dom "#comportex-starting-parameters"))
        spec (if (= spec-choice "a")
               spec-global
               spec-local)
        cache (atom {})
        inp (->>
             (case enc-choice
               "cortical_io"
               (cortical-io-encoder api-key cache
                                    :decode-locally? decode-locally?
                                    :spatial-scramble? spatial-scramble?)
               "random"
               (enc/unique-encoder cio/retina-dim
                                   (apply * 0.02 cio/retina-dim)))
             (enc/pre-transform :word)
             (core/sensory-input))]
    (go
     (when (= enc-choice "cortical_io")
       (cio-start-requests! api-key text cache)
       ;; allow some time for the first fingerprint request to cortical.io
       (<! (timeout 1000)))
     (with-ui-loading-message
       (reset-world text n-reps)
       (main/set-model
        (core/regions-in-series core/sensory-region inp 1 spec))))))

(defn ^:export init
  []
  (goog.ui.TabPane. (->dom "#comportex-tabs"))
  (let [form-el (->dom "#comportex-input-text-form")]
    (event/on-raw form-el :submit
                  (fn [e]
                    (restart-from-ui)
                    (.preventDefault e)
                    false))))
