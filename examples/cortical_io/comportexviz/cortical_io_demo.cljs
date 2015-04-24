(ns comportexviz.cortical-io-demo
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.cortical-io :as cio
             :refer [cortical-io-encoder
                     cache-fingerprint!]]
            [clojure.string :as str]
            [comportexviz.sentence-drawing :refer [draw-sentence-fn]]
            [comportexviz.main :as main]
            [comportexviz.viz-canvas :as viz]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [cljs.core.async :as async :refer [<! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-ui-loading-message]]))

(def fox-eats-what
  "
frog 	eat 	flies.
cow 	eat 	grain.
elephant 	eat 	leaves.
goat 	eat 	grass.
wolf 	eat 	rabbit.
cat 	likes 	ball.
elephant 	likes 	water.
sheep 	eat 	grass.
cat 	eat 	salmon.
wolf 	eat 	mice.
lion 	eat 	cow.
dog 	likes 	sleep.
coyote 	eat 	mice.
coyote 	eat 	rodent.
coyote 	eat 	rabbit.
wolf 	eat 	squirrel.
cow 	eat 	grass.
frog 	eat 	flies.
cow 	eat 	grain.
elephant 	eat 	leaves.
goat 	eat 	grass.
wolf 	eat 	rabbit.
sheep 	eat 	grass.
cat 	eat 	salmon.
wolf 	eat 	mice.
lion 	eat 	cow.
coyote 	eat 	mice.
elephant 	likes 	water.
cat 	likes 	ball.
coyote 	eat 	rodent.
coyote 	eat 	rabbit.
wolf 	eat 	squirrel.
dog 	likes 	sleep.
cat 	eat 	salmon.
cat 	likes 	ball.
cow 	eat 	grass.
fox 	eat 	something.
")

(def fingerprint-cache (atom {}))

(def model-config
  (atom {:n-regions 1
         :encoder :cortical-io
         :api-key nil
         :decode-locally? false
         :spatial-scramble? false
         :spec-choice :a
         }))

(def input-config
  (atom {:repeats 3
         :text fox-eats-what}))

(def draw (draw-sentence-fn 8))

(def world-buffer (async/buffer 5000))
(def world-c
  (async/chan world-buffer
              (comp (map (util/keep-history-middleware 100 :word :history))
                    (map #(vary-meta % assoc :comportexviz/draw-world draw)))))

;; used to force Reagent to re-render world-buffer count
(def world-buffer-trigger (atom true))
(add-watch main/model ::world-buffer-trigger (fn [_ _ _ _]
                                               (swap! world-buffer-trigger not)))

(def spec-global
  {:column-dimensions [30 40]
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
  [api-key text]
  (go
   (doseq [term (distinct (apply concat (split-sentences (str/lower-case text))))]
     (println "requesting fingerprint for:" term)
     ;; one request at a time (just has to keep ahead of sim)
     (<! (cache-fingerprint! api-key fingerprint-cache term)))))

(defn send-text!
  []
  (when-let [xs (seq (word-item-seq (:repeats @input-config)
                                    (:text @input-config)))]
    (go
      (when (= :cortical-io (:encoder @model-config))
        (cio-start-requests! (:api-key @model-config)
                             (:text @input-config))
          ;; allow some time for the first fingerprint request to cortical.io
          (<! (timeout 1000)))
      (async/onto-chan world-c xs false)
      (swap! world-buffer-trigger not))))

(defn set-model!
  []
  (let [config @model-config
        n-regions (:n-regions config)
        spec (case (:spec-choice config)
               :a spec-global
               :b spec-local)
        inp (->>
             (case (:encoder config)
               :cortical-io
               (cortical-io-encoder (:api-key config) fingerprint-cache
                                    :decode-locally? (:decode-locally? config)
                                    :spatial-scramble? (:spatial-scramble? config))
               :random
               (enc/unique-encoder cio/retina-dim
                                   (apply * 0.02 cio/retina-dim)))
             (enc/pre-transform :word)
             (core/sensory-input))]
    (with-ui-loading-message
      (main/set-model!
       (core/regions-in-series core/sensory-region inp
                               n-regions spec)))))

(def model-config-template
  [:div.form-horizontal
   [:div.form-group
    [:label.col-sm-5 "Word encoder:"]
    [:div.col-sm-7
     [:select.form-control {:field :list
                            :id :encoder}
      [:option {:key :cortical-io} "cortical.io"]
      [:option {:key :random} "random"]]]]
   [:div.form-group
    {:field :container
     :visible? #(= :cortical-io (:encoder %))}
    [:label.col-sm-5 "Cortical.io API key:"]
    [:div.col-sm-7
     [:input.form-control {:field :text
                           :id :api-key}]]]
   [:div.form-group
    [:label.col-sm-5 "Decode locally?"]
    [:div.col-sm-7
     [:input.form-control {:field :checkbox
                           :id :decode-locally?}]]]
   [:div.form-group
    [:label.col-sm-5 "Spatial scramble?"]
    [:div.col-sm-7
     [:input.form-control {:field :checkbox
                           :id :spatial-scramble?}]]]
   [:div.form-group
    [:label.col-sm-5 "Starting parameter set:"]
    [:div.col-sm-7
     [:select.form-control {:field :list
                            :id :spec-choice}
      [:option {:key :a} "20% potential, no topology"]
      [:option {:key :b} "30% * local 16% area = 5% potential"]]]]
   [:div.form-group
    [:label.col-sm-5 "Number of regions:"]
    [:div.col-sm-7
     [:input.form-control {:field :numeric
                           :id :n-regions}]]]
   [:div.form-group
    [:div.col-sm-offset-5.col-sm-7
     [:button.btn.btn-primary
      {:on-click (fn [e]
                   (set-model!)
                   (.preventDefault e))}
      "Restart with new model"]
     [:p.text-danger "This resets all parameters."]]]
   ])

(def input-config-template
  [:div.form-horizontal
   [:div.form-group
    [:label.col-sm-5 "Repeats of each sentence:"]
    [:div.col-sm-7
     [:input.form-control {:field :numeric
                           :id :repeats}]]]
   [:div.form-group
    [:div.col-sm-12
     [:textarea.form-control {:field :textarea
                              :id :text
                              :rows 10}]]]
   [:div.form-group
    [:div.col-sm-8
     [:button.btn.btn-primary
      {:on-click (fn [e]
                   (send-text!)
                   (.preventDefault e))}
      "Send text block input"]]]
   ])

(defn model-tab
  []
  [:div
   [:p "This demo looks up the "
    [:a {:href "http://cortical.io/"} "cortical.io"]
    " fingerprint for each word. Enter your API key below to start. The
     pre-loaded text below is the famous "
    [:a {:href "https://github.com/numenta/nupic.nlp-examples/blob/master/resources/associations/foxeat.csv"}
     "'fox eats what?' example"]
    " but you can enter whatever text you like. Words that are not
      found in the cortical.io 'associative_en' retina are assigned a
      random SDR."]

   [:h3 "Input " [:small "Word sequences"]]
   ^{:key (str "reagent-refresh-key-" @world-buffer-trigger)}
   [:p.text-info
    (str (count world-buffer) " queued input values.")]
   [:p.text-info
    (str (count @fingerprint-cache) " cached word fingerprints.")]
   [bind-fields input-config-template input-config]

   [:h3 "HTM model"]
   [bind-fields model-config-template model-config]
   ]
  )

(defn ^:export init
  []
  (reagent/render (main/comportexviz-app model-tab)
                  (dom/getElement "comportexviz-app"))
  (swap! viz/viz-options assoc-in [:drawing :display-mode] :two-d)
  (reset! main/world world-c)
  (swap! main/main-options assoc :sim-go? true))
