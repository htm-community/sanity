(ns comportexviz.demos.simple-sentences
  (:require [org.nfrac.comportex.demos.simple-sentences :as demo]
            [org.nfrac.comportex.core :as core]
            [comportexviz.sentence-drawing :refer [draw-sentence-fn]]
            ;; ui
            [comportexviz.main :as main]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [cljs.reader]
            [goog.ui.TabPane])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(def n-predictions 8)

(def spec demo/spec)

(defn input-gen
  [text n-repeats]
  (let [inp (demo/sensory-input-from-text text n-repeats demo/bits-per-word)
        split-sens (demo/split-sentences text)
        draw-inp (draw-sentence-fn split-sens n-predictions)]
    (assoc inp :comportexviz/draw-input draw-inp)))

(defn ^:export set-n-region-model
  [text n-repeats n]
  (with-ui-loading-message
    (let [inp (input-gen text n-repeats)]
      (main/set-model
       (core/regions-in-series core/sensory-region inp n spec)))))

;; handle UI for input stream

(defn ^:export restart-from-ui
  []
  (let [n-reps (cljs.reader/read-string
                (dom/val (->dom "#comportex-input-repeats")))
        text (dom/val (->dom "#comportex-input-text"))]
    (set-n-region-model text n-reps 1)))

(defn ^:export init
  []
  (goog.ui.TabPane. (->dom "#comportex-tabs"))
  (let [form-el (->dom "#comportex-input-text-form")]
    (event/on-raw form-el :submit
                  (fn [e]
                    (restart-from-ui)
                    (.preventDefault e)
                    false))))
