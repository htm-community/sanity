(ns comportexviz.demos.simple-sentences
  (:require [org.nfrac.comportex.demos.simple-sentences :as demo]
            [org.nfrac.comportex.core :as core]
            [comportexviz.sentence-drawing :refer [draw-sentence-fn]]
            ;; ui
            [comportexviz.main :as main]
            [c2.dom :as dom :refer [->dom]]
            [c2.event :as event]
            [cljs.reader]
            [goog.ui.TabPane]
            [cljs.core.async :as async])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(def n-predictions 8)

(def spec demo/spec)

(defn ^:export reset-world
  [text n-repeats]
  (let [draw-fn (draw-sentence-fn (demo/split-sentences text) n-predictions)]
    (main/set-world (->> (demo/world text n-repeats)
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw-fn))))))

(defn ^:export set-n-region-model
  [text n]
  (with-ui-loading-message
    (main/set-model
     (demo/n-region-model text n spec))))

;; handle UI for input stream

(defn ^:export restart-from-ui
  []
  (let [n-reps (cljs.reader/read-string
                (dom/val (->dom "#comportex-input-repeats")))
        text (dom/val (->dom "#comportex-input-text"))]
    (reset-world text n-reps)
    (set-n-region-model text 1)))

(defn ^:export init
  []
  (goog.ui.TabPane. (->dom "#comportex-tabs"))
  (let [form-el (->dom "#comportex-input-text-form")]
    (event/on-raw form-el :submit
                  (fn [e]
                    (restart-from-ui)
                    (.preventDefault e)
                    false))))
