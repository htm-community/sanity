(ns org.numenta.sanity.demos.cortical-io
  (:require [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.encoders :as enc]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.cortical-io :as cio
             :refer [cortical-io-encoder
                     cache-fingerprint!]]
            [clojure.string :as str]
            [org.numenta.sanity.demos.comportex-common :refer [all-features]]
            [org.numenta.sanity.main :as main]
            [org.numenta.sanity.helpers :as helpers]
            [org.numenta.sanity.bridge.browser :as server]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.comportex.data :as data]
            [org.numenta.sanity.util :refer [translate-network-shape]]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [cljs.core.async :as async :refer [<! timeout put!]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [org.numenta.sanity.macros :refer [with-ui-loading-message]]))

(def fox-eats-what
  "
frog eat flies.
cow eat grain.
elephant eat leaves.
goat eat grass.
wolf eat rabbit.
cat likes ball.
elephant likes water.
sheep eat grass.
cat eat salmon.
wolf eat mice.
lion eat cow.
dog likes sleep.
coyote eat mice.
coyote eat rodent.
coyote eat rabbit.
wolf eat squirrel.
cow eat grass.
frog eat flies.
cow eat grain.
elephant eat leaves.
goat eat grass.
wolf eat rabbit.
sheep eat grass.
cat eat salmon.
wolf eat mice.
lion eat cow.
coyote eat mice.
elephant likes water.
cat likes ball.
coyote eat rodent.
coyote eat rabbit.
wolf eat squirrel.
dog likes sleep.
cat eat salmon.
cat likes ball.
cow eat grass.
fox eat something.
")

(def fingerprint-cache (atom {}))

(def config
  (atom {:n-regions 1
         :encoder :cortical-io
         :api-key nil
         :decode-locally? true
         :spatial-scramble? false
         :spec-choice :a
         :repeats 3
         :text fox-eats-what
         :world-buffer-count 0
         :cache-count 0
         :have-model? false
         }))

(def world-buffer (async/buffer 5000))
(def world-c
  (async/chan world-buffer
              (comp (map (util/keep-history-middleware 100 :word :history))
                    (map #(assoc % :label (:word %))))))

(def into-sim (async/chan))

(def model (atom nil))

(add-watch model ::count-world-buffer
           (fn [_ _ _ _]
             (swap! config assoc :world-buffer-count (count world-buffer))))

(add-watch fingerprint-cache :count
           (fn [_ _ _ v]
             (swap! config assoc :cache-count (count v))))

(def spec-global
  {:column-dimensions [30 40]
   :ff-init-frac 0.20
   :ff-potential-radius 1.0
   :proximal {:perm-inc 0.05
              :perm-dec 0.005
              :perm-connected 0.20
              :stimulus-threshold 1}
   :activation-level 0.02
   :duty-cycle-period 100000
   :max-boost 2.0
   ;; sequence memory:
   :depth 5
   :distal {:max-segments 5
            :max-synapse-count 18
            :new-synapse-count 12
            :stimulus-threshold 9
            :learn-threshold 6
            :perm-connected 0.20
            :perm-init 0.16
            :perm-inc 0.05
            :perm-dec 0.01
            :punish? true}
   :distal-vs-proximal-weight 0
   })

(def spec-local
  (assoc spec-global
    :ff-init-frac 0.30
    :ff-potential-radius 0.20
    :spatial-pooling :local-inhibition
    :inhibition-base-distance 1))

(def higher-level-spec-diff
  {:column-dimensions [300]})

(defn load-predictions
  [htm n-predictions predictions-cache]
  (let [[_ e] (first (vals (:sensors htm)))
        rgn (first (core/region-seq htm))
        pr-votes (core/predicted-bit-votes rgn)
        predictions (p/decode e pr-votes n-predictions)]
    (if-let [c (:channel predictions)]
      ;; async call, return nil and await cache (like a promise)
      (do
        (go (let [predictions (<! c)]
              (swap! predictions-cache assoc htm predictions)))
        nil)
      ;; sync call, return predictions without caching
      predictions)))

(def max-shown 100)
(def scroll-every 50)

(defn world-pane
  []
  (let [show-predictions (atom false)
        predictions-cache (atom {})
        selected-htm (atom nil)]
    (add-watch main/selection ::fetch-selected-htm
               (fn [_ _ _ [sel1]]
                 (when-let [snapshot-id (get-in sel1 [:step :snapshot-id])]
                   (let [out-c (async/chan)]
                     (put! main/into-journal ["get-model" snapshot-id
                                              (marshal/channel out-c)])
                     (go
                       (reset! selected-htm (<! out-c)))))))
    (fn []
      (when-let [htm @selected-htm]
        (let [inval (:input-value htm)]
          [:div
           [:p.muted [:small "Input on selected timestep."]]
           [:div {:style {:min-height "40vh"}}
            (helpers/text-world-input-component inval htm max-shown
                                                scroll-every " ")]
           [:div
            [:button.btn.btn-default.btn-block {:class (if @show-predictions "active")
                                                :on-click (fn [e]
                                                            (swap! show-predictions not)
                                                            (.preventDefault e))}
             "Compute predictions"]]
           (when @show-predictions
             (if-let [predictions (or (get @predictions-cache htm)
                                      (load-predictions htm 8 predictions-cache))]
               (helpers/predictions-table predictions)
               ;; not cached and not returned immediately
               [:p.text-info "Loading predictions..."]))])))))

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
  (when-let [xs (seq (word-item-seq (:repeats @config)
                                    (:text @config)))]
    (go
      (when (= :cortical-io (:encoder @config))
        (cio-start-requests! (:api-key @config)
                             (:text @config))
          ;; allow some time for the first fingerprint request to cortical.io
          (<! (timeout 2500)))
      (async/onto-chan world-c xs false)
      (swap! config assoc :world-buffer-count (count world-buffer)))))

(defn set-model!
  []
  (with-ui-loading-message
    (let [n-regions (:n-regions @config)
          spec (case (:spec-choice @config)
                 :a spec-global
                 :b spec-local)
          e (case (:encoder @config)
              :cortical-io
              (cortical-io-encoder (:api-key @config) fingerprint-cache
                                   :decode-locally? (:decode-locally? @config)
                                   :spatial-scramble? (:spatial-scramble? @config))
              :random
              (enc/unique-encoder cio/retina-dim
                                  (apply * 0.02 cio/retina-dim)))
          sensor [:word e]
          init? (nil? @model)]
      (reset! model (core/regions-in-series
                     n-regions core/sensory-region
                     (list* spec (repeat (merge spec higher-level-spec-diff)))
                     {:input sensor}))
      (if init?
        (server/init model world-c main/into-journal into-sim)
        (reset! main/network-shape (translate-network-shape
                                    (data/network-shape @model))))
      (swap! config assoc :have-model? true))))

(def config-template
  [:div
   [:h3 "Input " [:small "Word sequences"]]
   [:p.text-info {:field :label
                  :id :world-buffer-count
                  :postamble " queued input values."}]
   [:p.text-info {:field :label
                  :id :cache-count
                  :postamble " cached word fingerprints."}]
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
      ;; active state
      [:div {:field :container
             :visible? #(:have-model? %)}
       [:button.btn.btn-primary
        {:on-click (fn [e]
                     (send-text!)
                     (.preventDefault e))}
        "Send text block input"]
       ]
      ;; disabled state
      [:div {:field :container
             :visible? #(not (:have-model? %))}
       [:button.btn.btn-primary.disabled
        "Send text block input"]
       [:p.text-info
        "Create a model first (below)."]
       ]]]
    ]
   [:h3 "HTM model"]
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
    ]])

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
   [bind-fields config-template config]
   ]
  )

(defn ^:export init
  []
  (reagent/render [main/sanity-app "Comportex" [model-tab] [world-pane]
                   (atom :model) all-features into-sim]
                  (dom/getElement "sanity-app"))
  (swap! main/viz-options assoc-in [:drawing :display-mode] :two-d)
  (put! into-sim ["run"]))
