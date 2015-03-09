(ns comportexviz.demos.simple-sentences
  (:require [org.nfrac.comportex.demos.simple-sentences :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [comportexviz.sentence-drawing :refer [draw-sentence-fn]]
            [comportexviz.main :as main]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [cljs.reader]
            [goog.ui.TabPane]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(def n-predictions 8)

(def world-c (async/chan 1000))

(defn set-world
  []
  (let [draw (draw-sentence-fn n-predictions)]
    (main/set-world (->> world-c
                         (async/map< (util/keep-history-middleware 100 :word :history))
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))))

(defn set-model-from-ui
  [text]
  (let [enc-choice (dom/val (->dom "#comportex-encoder"))
        n-regions (cljs.reader/read-string
                   (dom/val (->dom "#comportex-n-regions")))
        encoder (case enc-choice
                  "block" (demo/make-block-encoder text)
                  "random" demo/random-encoder)]
    (with-ui-loading-message
      (main/set-model
       (core/regions-in-series core/sensory-region (core/sensory-input encoder)
                               n-regions demo/spec)))))

(defn ^:export init
  []
  (goog.ui.TabPane. (->dom "#comportex-tabs"))
  (let [form-el (->dom "#comportex-model-form")
        text (dom/val (->dom "#comportex-input-text"))]
    (event/on-raw form-el :submit
                  (fn [e]
                    (set-model-from-ui text)
                    (.preventDefault e)
                    false)))
  (let [text (dom/val (->dom "#comportex-input-text"))
        n-reps (cljs.reader/read-string
                (dom/val (->dom "#comportex-input-repeats")))
        but-el (->dom "#comportex-text-send")]
      (event/on-raw but-el :click
                    (fn [e]
                      (async/onto-chan world-c
                                       (demo/word-item-seq n-reps text)
                                       false)
                      (.preventDefault e)
                      false)))
  (set-world)
  (reset! main/sim-go? true))
