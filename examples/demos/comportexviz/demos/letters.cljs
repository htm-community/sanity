(ns comportexviz.demos.letters
  (:require [org.nfrac.comportex.demos.letters :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [comportexviz.sentence-drawing :refer [draw-text-fn]]
            [comportexviz.main :as main]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [cljs.reader]
            [goog.ui.TabPane]
            [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-ui-loading-message]]))

(def n-predictions 8)

(def world-c (async/chan 10))

(defn set-world
  []
  (let [draw (draw-text-fn n-predictions)]
    (main/set-world (->> world-c
                         (async/map< (util/keep-history-middleware 400 :value :history))
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))))

(defn set-model-from-ui
  []
  (let [enc-choice (dom/val (->dom "#comportex-encoder"))
        n-regions (cljs.reader/read-string
                   (dom/val (->dom "#comportex-n-regions")))
        encoder (case enc-choice
                  "block" demo/block-encoder
                  "random" demo/random-encoder)]
    (with-ui-loading-message
      (main/set-model
       (core/regions-in-series core/sensory-region (core/sensory-input encoder)
                               n-regions demo/spec)))))

(defn ^:export init
  []
  (goog.ui.TabPane. (->dom "#comportex-tabs"))
  (let [form-el (->dom "#comportex-model-form")]
    (event/on-raw form-el :submit
                  (fn [e]
                    (set-model-from-ui)
                    (.preventDefault e)
                    false)))
  (let [el (->dom "#comportex-input-immediate")]
    (event/on-raw el :keypress
                  (fn [e]
                    (when-let [[x] (->> (or (.-keyCode e) (.-charCode e))
                                        (.fromCharCode js/String)
                                        (demo/clean-text)
                                        (seq))]
                      (dom/val el "")
                      (async/put! world-c {:value (str x)}))
                    false)))
  (let [text-el (->dom "#comportex-input-text")
        but-el (->dom "#comportex-text-send")]
      (event/on-raw but-el :click
                    (fn [e]
                      (when-let [xs (seq (demo/clean-text (dom/val text-el)))]
                        (async/onto-chan world-c (for [x xs] {:value (str x)})
                                         false))
                      (.preventDefault e)
                      false)))
  (set-world)
  (set-model-from-ui)
  (reset! main/sim-go? true))
