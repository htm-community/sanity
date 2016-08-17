(ns org.numenta.sanity.controls-ui
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [goog.dom.classes :as classes]
            [goog.string :as gstr]
            [clojure.browser.repl :as browser-repl]
            [clojure.string :as str]
            [clojure.walk :refer [keywordize-keys]]
            [cljs.core.async :as async :refer [put! <!]]
            [cljs.reader]
            [org.numenta.sanity.helpers :as helpers]
            [org.numenta.sanity.plots :as plots]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.selection :as sel]
            [org.nfrac.comportex.protocols :as p])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(defn now [] (.getTime (js/Date.)))

(defn sim-rate
  "Returns the simulation rate in timesteps per second for current
   run."
  [step run-start]
  (if-let [time-start (:time run-start)]
    (let [dur-ms (- (now) time-start)]
      (-> (- (:timestep step)
             (:timestep run-start))
          (/ dur-ms)
          (* 1000)))
    0))

(defn param-type [v]
  (cond
    (or (true? v) (false? v)) :boolean
    (vector? v) :vector
    :else :number))

(defn- spec-form
  [network-shape partypes spec spec-path skip-set]
  (for [[k v] (sort spec)
        :when (not (skip-set k))
        :let [typ (or (get @partypes k)
                      (get (swap! partypes assoc k (param-type v))
                           k))
              setv! #(swap! network-shape assoc-in spec-path
                            (assoc spec k %))]]
    [:div.row {:class (when (or (nil? v) (string? v))
                        "has-error")}
     [:div.col-sm-8
      [:label.control-label.text-left (name k)]]
     [:div.col-sm-4
      (case typ
        ;; boolean
        :boolean
        [:input.form-control.input-sm
         {:type :checkbox
          :checked (if v true)
          :on-change #(setv! (not v))}]
        ;; vector
        :vector
        [:input.form-control.input-sm
         {:value (str v)
          :on-change (fn [e]
                       (let [s (-> e .-target forms/getValue)
                             x (try (cljs.reader/read-string s)
                                    (catch :default _ s))
                             newval (if (and (vector? x)
                                             (every? integer? x))
                                      x s)]
                         (setv! newval)))}]
        ;; number
        :number
        [:input.form-control.input-sm
         {:value v
          :on-change (fn [e]
                       (let [s (-> e .-target forms/getValue)
                             parsed (js/parseFloat s)
                             newval (if (or (empty? s)
                                            (js/isNaN parsed))
                                      nil
                                      (->> (if (not= s (str parsed))
                                             s
                                             parsed)))]
                         (setv! newval)))}])]]))

(defn parameters-tab
  [network-shape _ into-sim _]

  (add-watch network-shape ::push-to-server
             (fn [_ _ prev-st st]
               (when-not (nil? prev-st) ;; don't push when getting initial template
                 (doseq [path (for [[r-id rgn] (:regions st)
                                    l-id (keys rgn)]
                                [:regions r-id l-id :spec])
                         :let [old-spec (get-in prev-st path)
                               new-spec (get-in st path)]]
                   (when (not= old-spec new-spec)
                     (put! into-sim ["set-spec" path new-spec]))))))

  (let [partypes (cljs.core/atom {})] ;; write-once cache
    (fn [network-shape selection into-sim]
      (let [[sel-region sel-layer] (some sel/layer @selection)]
        [:div
         [:p.text-muted "Read/write model parameters of the selected region layer,
                    with immediate effect. Click a layer to select it."]
         [:p.text-info.text-center
          (when sel-layer
            (str (name sel-region) " " (name sel-layer)))]
         (into
         [:div.form-horizontal]
         (when @network-shape
           (let [spec-path [:regions sel-region sel-layer :spec]
                 spec (get-in @network-shape spec-path)]
             (concat
              (spec-form network-shape partypes spec spec-path
                         #{:proximal :distal :apical})
              (for [[sub-k title] [[:proximal "Proximal dendrites"]
                                   [:distal "Distal (lateral) dendrites"]
                                   [:apical "Apical dendrites"]]]
                [:div.panel.panel-default
                 [:div.panel-heading [:h4.panel-title title]]
                 [:div.panel-body
                  (into
                   [:div.form-horizontal]
                   (spec-form network-shape partypes
                              (get spec sub-k) (conj spec-path sub-k) #{}))]])
              [
               [:div.panel.panel-default
                [:div.panel-heading [:h4.panel-title "Note"]]
                [:div.panel-body
                 [:p "Parameter values can be altered above, but some parameters
                     must be in effect when the HTM regions are created.
                     Notable examples are "
                  [:code "column-dimensions"] " and " [:code "depth"]
                  ". After setting such parameter values, rebuild all regions
                 (obviously losing any learned connections in the process):"
                  ]
                 [:button.btn.btn-warning.btn-block
                  {:on-click #(helpers/ui-loading-message-until
                               (go
                                 (<! (async/timeout 100))
                                 (let [finished (async/chan)]
                                   (put! into-sim
                                         ["restart" (marshal/channel finished
                                                                     true)])
                                   (<! finished))))}
                  "Rebuild model"]
                 [:p.small "This will not reset, or otherwise alter, the input stream."]]
                ]
               [:h4 "Current spec value"]
               [:pre (str spec)]
               ])
             )))])))
  )

(defn gather-col-state-history!
  [col-state-history step into-journal]
  (let [{:keys [snapshot-id network-shape timestep]} step
        fetches #{"n-unpredicted-active-columns"
                  "n-predicted-inactive-columns"
                  "n-predicted-active-columns"}]
    (doseq [[rgn-id rgn] (:regions network-shape)
            [lyr-id lyr] rgn
            :let [response-c (async/chan)]]
      (put! into-journal ["get-layer-stats" snapshot-id rgn-id lyr-id fetches
                          (marshal/channel response-c true)])
      (go
        (let [r (<! response-c)
              {active "n-unpredicted-active-columns"
               predicted "n-predicted-inactive-columns"
               active-predicted "n-predicted-active-columns"} r
              size (reduce * (:dimensions lyr))]
          (swap! col-state-history update-in [rgn-id lyr-id]
                 (fn [csf-log]
                   (conj (or csf-log
                             (plots/empty-col-state-freqs-log))
                         {:active active
                          :predicted predicted
                          :active-predicted active-predicted
                          :timestep timestep
                          :size size}))))))))

(defn time-plots-tab-builder
  [steps into-journal]
  (let [;; rgn-id -> lyr-id -> sequence-compressor
        col-state-history (atom {})]
    (add-watch steps ::ts-plot-data
               (fn [_ _ _ xs]
                 (gather-col-state-history! col-state-history (first xs)
                                            into-journal)))
    (fn time-plots-tab [series-colors]
      [:div
       [:p.text-muted "Time series of cortical column activity."]
       [:div
        (for [[rgn-id rgn] @col-state-history
              [lyr-id csf-log] rgn]
          ^{:key [rgn-id lyr-id]}
          [:fieldset
           [:legend (str (name rgn-id) " " (name lyr-id))]
           [plots/ts-freqs-plot-cmp csf-log series-colors]])]])))

(defn sources-tab
  [network-shape selection series-colors into-journal]
  [:div
   [:p.text-muted "Plots of cell excitation broken down by source."]
   [:div
    (when @network-shape
      (for [[region-key rgn] (:regions @network-shape)
            layer-id (keys rgn)]
        ^{:key [region-key layer-id]}
        [:fieldset
         [:legend (str (name region-key) " " (name layer-id))]
         [plots/cell-excitation-plot-cmp network-shape selection series-colors
          region-key layer-id into-journal]]))]])

(def default-cell-sdrs-plot-options
  {:group-contexts? false
   :spreading-activation-steps 0
   :ordering :first-appearance
   :hide-states-older 100
   :hide-states-rarer 1
   :hide-conns-smaller 5})

(def cell-sdrs-plot-dt-limit 300)

(def cell-sdrs-plot-options-template
  [:div
   [:div.row
    [:div.col-sm-6
     [:label.small "Group contexts?"]]
    [:div.col-sm-6
     [:input {:field :checkbox
              :id :group-contexts?}]
     [:small " (column-level SDRs)"]]
    ]
   [:div.row
    [:div.col-sm-6
     [:label.small "Order by"]]
    [:div.col-sm-6
     [:select.form-control.input-sm {:field :list
                                     :id :ordering}
      [:option {:key :first-appearance} "first appearance"]
      [:option {:key :last-appearance} "last appearance"]]]
    ]
   [:div.row
    [:div.col-sm-6
     [:label.small {:field :label
                    :id :hide-states-older
                    :preamble (gstr/unescapeEntities "Seen &le; ")
                    :postamble " steps ago"}]]
    [:div.col-sm-6
     [:input {:field :range
              :min 5
              :max cell-sdrs-plot-dt-limit
              :step 5
              :id :hide-states-older}]]
    ]
   [:div.row
    [:div.col-sm-6
     [:label.small {:field :label
                    :id :hide-states-rarer
                    :preamble (gstr/unescapeEntities "Seen &ge; ")
                    :postamble " times"}]]
    [:div.col-sm-6
     [:input {:field :range
              :min 1
              :max 16
              :id :hide-states-rarer}]]
    ]
   [:div.row
    [:div.col-sm-6
     [:label.small {:field :label
                    :id :hide-conns-smaller
                    :preamble (gstr/unescapeEntities "&ge; ")
                    :postamble "-cell connections"}]
     ]
    [:div.col-sm-6
     [:input {:field :range
              :min 1
              :max 16
              :id :hide-conns-smaller}]]
    ]
   [:div.row
    [:div.col-sm-6
     [:label.small {:field :label
                    :id :spreading-activation-steps
                    :preamble "spreading "
                    :postamble " steps"}]
     ]
    [:div.col-sm-6
     [:input {:field :range
              :min 0
              :max 12
              :id :spreading-activation-steps}]]
    ]
   ])

(defn cell-sdrs-tab-builder
  [steps network-shape selection into-journal]
  (let [plot-opts (atom default-cell-sdrs-plot-options)
        component (atom nil)
        enable! (fn []
                  (reset!
                   component
                   (plots/cell-sdrs-plot-builder steps network-shape selection
                                                 into-journal
                                                 plot-opts)))
        disable! (fn []
                   (let [teardown! (:teardown @component)]
                     (teardown!))
                   (reset! component nil))
        ]
    (fn cell-sdrs-tab []
      [:div
       (if-not @component
         ;; placeholder
         [:div
          [:p.text-muted "Cell "
           [:abbr {:title "Sparse Distributed Representations"} "SDRs"]
           " on a state transition diagram. Labels are corresponding inputs."]
          [:button.btn.btn-primary.btn-block
           {:on-click (fn [e]
                        (enable!)
                        (.preventDefault e))}
           "Start from selected timestep"]
          [:p.small.text-warning "So to start from the beginning, select timestep 1 first."]
          [:p.small "Not enabled by default because it can be slow."]]
         ;; enabled content
         [:div
          [bind-fields cell-sdrs-plot-options-template plot-opts]
          [(:content @component)]
          [:button.btn.btn-warning.btn-block
           {:on-click (fn [e]
                        (disable!)
                        (.preventDefault e))}
           "Disable and reset"]])
       [:p "This shows the dynamics of a layer of cells as a state
        transition diagram. The \"states\" are in fact cell SDRs,
        i.e. sets of cells active together. They are fuzzy: cells may
        participate in multiple states. And they are evolving: the
        membership of a state may change over time."]
       [:p "To be precise, a state is defined as a set of cells
         weighted by their specificity to that state. So if a cell
         participates in states A and B an equal number of times, it
         will count only half as much to A as a cell fully specific to
         A."]
       [:p "If the active winner cells match a known state
        sufficiently well (meeting " [:code "seg-learn-threshold"]
        ") then the state is extended to include all current
        cells. Otherwise, a new state is created."]
       [:p "Input labels (key :label) are recorded on matching
        states, but this is only for display, it is not used to define
        states."]
       [:p "The display shows one layer at one point in
         time. Select other layers to switch the display to them. Step
         back and forward in time as you wish."]
       [:h4 "Reading the diagram"]
       [:ul
        [:li "States are drawn in order of appearance."]
        [:li "If any of a state's cells are currently active that
         fraction will be shaded red (whether active due to bursting
         or not)."]
        [:li "Similarly, any predictive cells (predicting activation for the "
         [:strong "next"] " time step) will be shaded blue."]
        [:li "If any of a state's cells are the
         current " [:i "winner cells"] " that fraction will be
         outlined in black."]
        [:li "When a matching state will be extended to include new
         cells, those are shown in green."]
        [:li "Transitions are drawn as blue curves. Thickness
         corresponds to the number of connected synapses, weighted by
         specificity of both the source and target cells."]
        [:li "The height of a state corresponds to the (weighted)
         number of cells it represents."]
        [:li "The width of a state corresponds to the number of times
         it has matched."]
        [:li "Labels are drawn with horizonal spacing by frequency."]]
       ])))

(defn fetch-details-text!
  [into-journal text-response sel]
  (let [{:keys [step bit] :as sel1} (first (filter sel/layer sel))
        {:keys [snapshot-id]} step
        [rgn-id lyr-id] (sel/layer sel1)]
    (when lyr-id
      (let [response-c (async/chan)]
        (put! into-journal ["get-details-text" snapshot-id rgn-id lyr-id bit
                            (marshal/channel response-c true)])
        (go
          (reset! text-response (<! response-c)))))))

(defn details-tab
  [selection into-journal]
  (let [text-response (atom "")]
    (reagent/create-class
     {:component-will-mount
      (fn [_]
        (add-watch selection :fetch-details-text
                   (fn [_ _ _ sel]
                     (reset! text-response "")
                     (fetch-details-text! into-journal text-response sel)))
        (fetch-details-text! into-journal text-response @selection))

      :component-will-unmount
      (fn [_]
        (remove-watch selection :fetch-details-text))

      :reagent-render
      (fn [_ _]
        [:div
         [:p.text-muted "The details of model state on the selected time step, selected column."]
         [:pre.pre-scrollable {:style {:height "90vh" :resize "both"}}
          @text-response]
         [:p.text-muted [:small "(scrollable)"]]
         [:hr]
         [:p.text-muted "If you're brave:"]
         (let [{:keys [step]} (first (filter sel/layer @selection))
               {:keys [snapshot-id]} step]
           [:button.btn.btn-warning.btn-block
            (cond-> {:on-click (fn [e]
                                 (let [response-c (async/chan)]
                                   (put! into-journal
                                         [:get-model snapshot-id
                                          (marshal/channel response-c true)
                                          true])
                                   (go
                                     (println (<! response-c))))
                                 (.preventDefault e))}
              (not snapshot-id) (assoc :disabled "disabled"))
            "Dump entire model to console"])
         ])})))

(defn t-chbox
  [id label]
  [:div.checkbox
   [:label
    [:input {:field :checkbox :id id}]
    " "
    label]])

(def keep-steps-template
  [:div.row
   [:div.col-xs-12
    [:div.panel.panel-default
     [:div.panel-body
      [:label {:style {:font-weight "normal"}}
       "Keep "
       [:input {:field :numeric
                :id :keep-steps
                :style {:text-align "center"}
                :size 4}]
       " steps of history"]]]]])

(defn capture-tab [capture-options]
  [:div
   [:p.text-muted {:style {:margin-top 15
                           :margin-bottom 15}}
    "Choose data the server should capture."]
   [bind-fields keep-steps-template capture-options]
   [:div.row
    [:div.col-lg-6.col-sm-12
     [:div.panel.panel-default
      [:div.panel-heading
       [:h4.panel-title "Feed-forward synapses"]]
      [:div.panel-body
       [bind-fields [:div
                     (t-chbox :ff-synapses.capture?
                              "Save")
                     [:div {:field :container
                            ;; for consistent checkbox layout
                            :style {:margin-top -5}
                            :visible? #(get-in % [:ff-synapses :capture?])}
                      (t-chbox :ff-synapses.only-active?
                               "Only if active")
                      (t-chbox :ff-synapses.only-connected?
                               "Only if connected")]]
        capture-options]]]]
    [:div.col-lg-6.col-sm-12
     [:div.panel.panel-default
      [:div.panel-heading
       [:h4.panel-title "Distal synapses"]]
      [:div.panel-body
       [bind-fields [:div
                     (t-chbox :distal-synapses.capture?
                              "Save")
                     [:div {:field :container
                            :style {:margin-top -5}
                            :visible? #(get-in % [:distal-synapses :capture?])}
                      (t-chbox :distal-synapses.only-active?
                               "Only if active")
                      (t-chbox :distal-synapses.only-connected?
                               "Only if connected")
                      (t-chbox :distal-synapses.only-noteworthy-columns?
                               "Only active / predicted columns")]]
        capture-options]]]]]
   [:div.row
    [:div.col-lg-6.col-sm-12
     [:div.panel.panel-default
      [:div.panel-heading
       [:h4.panel-title "Apical synapses"]]
      [:div.panel-body
       [bind-fields [:div
                     (t-chbox :apical-synapses.capture?
                              "Save")
                     [:div {:field :container
                            :style {:margin-top -5}
                            :visible? #(get-in % [:apical-synapses :capture?])}
                      (t-chbox :apical-synapses.only-active?
                               "Only if active")
                      (t-chbox :apical-synapses.only-connected?
                               "Only if connected")
                      (t-chbox :apical-synapses.only-noteworthy-columns?
                               "Only active / predicted columns")]]
        capture-options]]]]]])

(def viz-options-template
  (let [chbox (fn [id label]
                [:div.checkbox
                 [:label
                  [:input {:field :checkbox :id id}]
                  (str " " label)]])
        group (fn [title content]
                [:div.col-lg-6.col-sm-12
                 [:div.panel.panel-default
                  [:div.panel-heading
                   [:h4.panel-title title]]
                  content]])]
    [:div
     [:div.panel.panel-default
      [:div.panel-body
       [:div.radio
        [:label
         [:input {:field :radio
                  :name :drawing.display-mode
                  :value :one-d}]
         " Draw "
         [:input {:field :numeric
                  :id :drawing.draw-steps
                  :size 4}]
         " steps in 1D"]]
       [:div.radio
        [:label
         [:input {:field :radio
                  :name :drawing.display-mode
                  :value :two-d}]
         " Draw one step in 2D"]]
       ]]
     [:div.row
      (group "Inbits"
             [:div.panel-body
              (chbox :inbits.active "Active bits")
              (chbox :inbits.predicted "Predicted bits")
              ])
      (group "Columns"
             [:div.panel-body
              (chbox :columns.overlaps "Overlaps")
              (chbox :columns.active-freq "Active-duty")
              (chbox :columns.boosts "Boosts")
              (chbox :columns.n-segments "N.segments")
              (chbox :columns.active "Active")
              (chbox :columns.predictive "Predictive")
              ])
      (group "Feed-forward synapses"
             [:div.panel-body
              [:div "To "
               [:select {:field :list
                         :id :ff-synapses.to}
                [:option {:key :all} "all active columns"]
                [:option {:key :selected} "selected column"]
                [:option {:key :none} "none"]
                ]]
              (chbox :ff-synapses.trace-back? "Trace back")
              (chbox :ff-synapses.disconnected "Disconnected")
              (chbox :ff-synapses.inactive "Inactive")
              (chbox :ff-synapses.predicted "Predictive")
              (chbox :ff-synapses.permanences "Permanences")
              ])
      (group "Distal synapses"
             [:div.panel-body
              [:div "(selected column) "
               [:select {:field :list
                         :id :distal-synapses.to}
                [:option {:key :all} "all cell segments"]
                [:option {:key :selected} "selected segment"]
                [:option {:key :none} "none"]
                ]]
              (chbox :distal-synapses.disconnected "Disconnected")
              (chbox :distal-synapses.inactive "Inactive")
              (chbox :distal-synapses.permanences "Permanences")
              ])]
     ]))

(defn drawing-tab
  [features viz-options capture-options]
  [:div
   [:p.text-muted {:style {:margin-top 15
                           :margin-bottom 15}}
    "Select drawing options, with immediate effect."]
   (when-not (features :capture)
     [bind-fields keep-steps-template capture-options])
   [bind-fields viz-options-template viz-options]])

(def default-debug-data
  {:repl-url "http://localhost:9000/repl"
   :started? false
   :conn nil})

(defn debug-tab
  [debug-data]
  [:div
   [:p.text-muted {:style {:margin-top 15
                           :margin-bottom 15}}
    "Inspect the inspector."]
   [:div.col-sm-12
    [:div.panel.panel-default
     [:div.panel-heading
      [:h4.panel-title "REPL"]]
     [:div.panel-body
      [:p "ClojureScript REPL URL:"]
      (if (:started? @debug-data)
        [:p (:repl-url @debug-data)]
        [:div
         [:p [bind-fields [:input {:style {:width "100%"}
                                   :field :text
                                   :id :repl-url}]
              debug-data]]
         [:p [:button.btn.btn-primary.btn-block
              {:on-click (fn [_]
                           (let [conn (browser-repl/connect (:repl-url
                                                             @debug-data))]
                             (swap! debug-data assoc
                                    :started? true
                                    :conn conn)))}
              "Connect to Browser REPL"]]])
      [:p "Go start a ClojureScript REPL, then connect to it from here."]
      [:p [:a {:href "https://github.com/nupic-community/sanity"} "Sanity"]
       " has a browser_repl.clj that you can run."]
      [:p "Pro-tip: in Emacs, do 'M-x shell', run "
       "'lein run -m clojure.main browser_repl.clj', "
       "and maybe do 'M-x paredit-mode'."]]]]])

(defn send-command [ch command & xs]
  (fn [e]
    (put! ch (into [command] xs))
    (.preventDefault e)))

(defn gather-start-data!
  [run-start steps]
  (reset! run-start
          {:time (now)
           :timestep (:timestep (first steps))}))

(defn navbar
  [_ _ steps show-help viz-options viz-expanded network-shape into-viz into-sim]
  ;; Ideally we would only show unscroll/unsort/unwatch when they are relevant...
  ;; but that is tricky. An easier option is to hide those until the
  ;; first time they are possible, then always show them. We keep track here:
  (let [has-scrolled? (atom false)
        has-sorted? (atom false)
        has-watched? (atom false)
        apply-to-all? (atom true)
        run-start (atom {})
        going? (atom false)
        subscriber-c (async/chan)]

    (put! into-sim ["subscribe-to-status" (marshal/channel subscriber-c)])

    (go-loop []
      (when-let [[g?] (<! subscriber-c)]
        ;; Avoid passing false directly through a channel.
        ;; It foils every `when-let` on its way.
        (reset! going? g?)
        (recur)))

    ;; initial start data
    (add-watch steps ::gather-start-data
               (fn [_ _ _ xs]
                 (remove-watch steps ::gather-start-data)
                 (gather-start-data! run-start xs)))

    ;; subsequent start data
    (add-watch going? ::gather-start-data
               (fn [_ _ oldv go?]
                 (when (and (not oldv)
                            go?)
                   (when (first @steps)
                     (gather-start-data! run-start @steps)))))

    (fn [title features _ _ _ _ _ _]
      [:nav.navbar.navbar-default
       [:div.container-fluid
        [:div.navbar-header
         [:button.navbar-toggle.collapsed {:data-toggle "collapse"
                                           :data-target "#comportex-navbar-collapse"}
          [:span.icon-bar] [:span.icon-bar] [:span.icon-bar]]
         [:a.navbar-brand {:href "https://github.com/nupic-community/sanity"}
          title]]
        [:div.collapse.navbar-collapse {:id "comportex-navbar-collapse"}
         [:ul.nav.navbar-nav
          ;; step back
          [:li
           [:button.btn.btn-default.navbar-btn
            (cond-> {:type :button
                     :on-click (send-command into-viz :step-backward)
                     :title "Step backward in time"}
              (not @network-shape) (assoc :disabled "disabled"))
            [:span.glyphicon.glyphicon-step-backward {:aria-hidden "true"}]
            [:span.visible-xs-inline " Step backward"]]]
          ;; step forward
          [:li
           [:button.btn.btn-default.navbar-btn
            (cond-> {:type :button
                     :on-click (send-command into-viz :step-forward)
                     :title "Step forward in time"}
              (not @network-shape) (assoc :disabled "disabled"))
            [:span.glyphicon.glyphicon-step-forward {:aria-hidden "true"}]
            [:span.visible-xs-inline " Step forward"]]]
          ;; pause button
          [:li (when-not @going? {:class "hidden"})
           [:button.btn.btn-default.navbar-btn
            (cond-> {:type :button
                     :on-click #(put! into-sim ["pause"])
                     :style {:width "5em"}}
              (not @network-shape) (assoc :disabled "disabled"))
            "Pause"]]
          ;; run button
          [:li (when @going? {:class "hidden"})
           [:button.btn.btn-primary.navbar-btn
            (cond-> {:type :button
                     :on-click #(put! into-sim ["run"])
                     :style {:width "5em"}}
              (not @network-shape) (assoc :disabled "disabled"))
            "Run"]]
          ;; display mode
          [:li.dropdown
           [:a.dropdown-toggle {:data-toggle "dropdown"
                                :role "button"
                                :href "#"}
            "Display" [:span.caret]]
           [:ul.dropdown-menu {:role "menu"}
            [:li [:a {:href "#"
                      :on-click #(swap! viz-options assoc-in [:drawing :display-mode]
                                        :one-d)}
                  "column states over time (1D)"]]
            [:li [:a {:href "#"
                      :on-click #(swap! viz-options assoc-in [:drawing :display-mode]
                                        :two-d)}
                  "column states over space (2D)"]]
            ]]
          ;; expand canvas
          (when-not @viz-expanded
            [:li.hidden-xs
             [:button.btn.btn-default.navbar-btn
              {:type :button
               :on-click (fn [e]
                           (doseq [el (prim-seq (dom/getElementsByClass "viz-expandable"))]
                             (classes/swap el "col-sm-9" "col-sm-12"))
                           (reset! viz-expanded true)
                           (.dispatchEvent js/window (js/Event. "resize"))
                           (.preventDefault e))
               :title "Expand visualisation"}
              [:span.glyphicon.glyphicon-resize-full {:aria-hidden "true"}]
              [:span.sr-only "Expand"]]])
          ;; un-expand canvas
          (when @viz-expanded
            [:li.hidden-xs
             [:button.btn.btn-default.navbar-btn
              {:type :button
               :on-click (fn [e]
                           (doseq [el (prim-seq (dom/getElementsByClass "viz-expandable"))]
                             (classes/swap el "col-sm-12" "col-sm-9"))
                           (reset! viz-expanded false)
                           (.dispatchEvent js/window (js/Event. "resize"))
                           (.preventDefault e))
               :title "Un-expand visualisation"}
              [:span.glyphicon.glyphicon-resize-small {:aria-hidden "true"}]
              [:span.sr-only "Un-expand"]]])
          ;; sort/watch/scroll and all-layers option
          [:li
           [:p.navbar-text "Sort/Watch/Scroll:"]]
          ;; sort selected layer
          [:li
           [:button.btn.btn-default.navbar-btn
            {:type :button
             :on-click (comp (fn [_] (reset! has-sorted? true))
                             (send-command into-viz :sort @apply-to-all?))
             :title "Sort the columns by order of recent activity"}
            [:span.glyphicon.glyphicon-sort-by-attributes-alt {:aria-hidden "true"}]
            [:span.visible-xs-inline " Sort by recent active columns"]]]
          ;; clear sorting
          (when @has-sorted?
            [:li
             [:button.btn.btn-default.navbar-btn
              {:type :button
               :on-click (comp (fn [_] (when @apply-to-all? (reset! has-sorted? false)))
                               (send-command into-viz :clear-sort @apply-to-all?))
               :title "Clear all sorting - revert to actual column order"}
              [:span.glyphicon.glyphicon-sort-by-order {:aria-hidden "true"}]
              [:span.visible-xs-inline " Clear sorting"]]])
          ;; watch - add facet
          [:li {:style {:margin-left "1ex"}}
           [:button.btn.btn-default.navbar-btn
            {:type :button
             :on-click (comp (fn [_] (reset! has-watched? true))
                             (send-command into-viz :add-facet @apply-to-all?))
             :title "Add a facet to watch the current active set of columns"}
            [:span.glyphicon.glyphicon-eye-open {:aria-hidden "true"}]
            [:span.visible-xs-inline " Add facet, watching active set"]]]
          ;; unwatch - clear facets
          (when @has-watched?
            [:li
             [:button.btn.btn-default.navbar-btn
              {:type :button
               :on-click (comp (fn [_] (when @apply-to-all? (reset! has-watched? false)))
                               (send-command into-viz :clear-facets @apply-to-all?))
               :title "Clear all facets (watching sets of columns)"}
              [:span.glyphicon.glyphicon-eye-close {:aria-hidden "true"}]
              [:span.visible-xs-inline " Clear all facets"]]])
          ;; scroll down
          [:li {:style {:margin-left "1ex"}}
           [:button.btn.btn-default.navbar-btn
            {:type :button
             :on-click (comp (fn [_] (reset! has-scrolled? true))
                             (send-command into-viz :scroll-down @apply-to-all?))
             :title "Scroll down visible columns"}
            [:span.glyphicon.glyphicon-arrow-down {:aria-hidden "true"}]
            [:span.visible-xs-inline " Scroll down"]]]
          ;; scroll up
          (when @has-scrolled?
            [:li
             [:button.btn.btn-default.navbar-btn
              {:type :button
               :on-click (send-command into-viz :scroll-up @apply-to-all?)
               :title "Scroll up visible columns"}
              [:span.glyphicon.glyphicon-arrow-up {:aria-hidden "true"}]
              [:span.visible-xs-inline " Scroll up"]]])
          [:li
           [:div.navbar-form
            [:div.form-group
             [:div.checkbox
              [:label.small
               {:title (str "Apply scroll/sort/watch actions to all layers; "
                            "otherwise only the selected layer.")}
               [:input
                {:type :checkbox
                 :checked (when @apply-to-all? true)
                 :on-change (fn [e]
                              (swap! apply-to-all? not)
                              (.preventDefault e))}]
               " all layers"]]]]]
          ]
         ;; right-aligned items
         [:ul.nav.navbar-nav.navbar-right
          ;; sim rate
          [:li (if-not @going? {:class "hidden"})
           [:p.navbar-text (str (.toFixed (sim-rate (first @steps) @run-start)
                                          1) "/sec.")]]
          ;; sim / anim options
          (when (features :speed)
            [:li.dropdown
             [:a.dropdown-toggle {:data-toggle "dropdown"
                                  :role "button"
                                  :href "#"}
              "Speed" [:span.caret]]
             [:ul.dropdown-menu {:role "menu"}
              [:li [:a {:href "#"
                        :on-click (fn []
                                    (put! into-sim ["set-step-ms" 0])
                                    (swap! viz-options assoc-in
                                           [:drawing :anim-every] 1))}
                    "max sim speed"]]
              [:li [:a {:href "#"
                        :on-click (fn []
                                    (put! into-sim ["set-step-ms" 0])
                                    (swap! viz-options assoc-in
                                           [:drawing :anim-every] 100))}
                    "max sim speed, draw every 100 steps"]]
              [:li [:a {:href "#"
                        :on-click (fn []
                                    (put! into-sim ["set-step-ms" 250])
                                    (swap! viz-options assoc-in
                                           [:drawing :anim-every] 1))}
                    "limit to 4 steps/sec."]]
              [:li [:a {:href "#"
                        :on-click (fn []
                                    (put! into-sim ["set-step-ms" 500])
                                    (swap! viz-options assoc-in
                                           [:drawing :anim-every] 1))}
                    "limit to 2 steps/sec."]]
              [:li [:a {:href "#"
                        :on-click (fn []
                                    (put! into-sim ["set-step-ms" 1000])
                                    (swap! viz-options assoc-in
                                           [:drawing :anim-every] 1))}
                    "limit to 1 step/sec."]]]])
          [:li
           [:button.btn.btn-default.navbar-btn
            (cond-> {:type :button
                     :on-click (fn [e]
                                 (swap! show-help not)
                                 (.preventDefault e))
                     :title "Help"}
              @show-help (assoc :class "active"))
            [:span.glyphicon.glyphicon-question-sign {:aria-hidden "true"}]
            [:span.visible-xs-inline " Help"]]]
          ]
         ]
        ]])))

(defn tabs
  [current-tab tab-cmps]
  [:div
   [:nav
    (into [:ul.nav.nav-tabs]
          (for [[k _] tab-cmps]
            [:li {:role "presentation"
                  :class (if (= @current-tab k) "active")}
             [:a {:href "#"
                  :on-click (fn [e]
                              (reset! current-tab k)
                              (.preventDefault e))}
              (name k)]]))]
   [:div.tabs
    (let [[_ cmp] (->> tab-cmps
                       (filter (fn [[k _]]
                                 (= @current-tab k)))
                       first)]
      cmp)]])

(defn help-block
  [show-help]
  (if @show-help
    [:div.container-fluid
     [:div.row
      [:div.col-lg-3.col-md-4.col-sm-6
       [:h4 "Overview"]
       [:p [:a {:href "https://github.com/nupic-community/sanity"}
            "Sanity"]
        " runs HTM models in the browser with interactive
       controls. The model state from recent timesteps is kept, so you can step
       back in time. You can inspect input values, encoded sense bits, and the
       columns that make up cortical region layers. Within a column you can inspect
       cells and their distal dendrite segments. Feed-forward and distal synapses
       can be shown."]]
      [:div.col-lg-3.col-md-4.col-sm-6
       [:h4 "Display"]
       [:p "Kept timesteps are shown in a row at the top of the display.
      Click one to jump to it.
      Below that, the blocks represent sensory fields (squares) and
      layers of cortical columns (circles). Depending on the display mode,
      these may be shown in 2D grids from a single time step, or as one
      vertical line per timestep, allowing several time steps to be shown
      in series."]
       [:p "Don't miss the various complementary displays in the other tabs."]]
      [:div.col-lg-3.col-md-4.col-sm-6
       [:h4 "Selection"]
       [:p "Click on the main canvas to select one column of cells,
      within some region layer. The individual cells
      and their distal dendrite segments will be shown.
      If you click off the layer, the column will be de-selected, but
      the layer will remain selected. Its parameters can be seen and edited in
      the 'params' tab."]
       [:p "Input bits can also be selected. For multiple selections,
       hold Command / Ctrl key while clicking (on Mac / Windows,
       respectively)."]]
      [:div.col-lg-3.col-md-4.col-sm-6
       [:h4 "Key controls"]
       [:p "When the main canvas is in focus, "
        [:ul
         [:li [:kbd "up"] "/" [:kbd "down"]
          " select columns; "]
         [:li
          [:kbd "page up"] "/" [:kbd "page down"]
          " scroll the visible field; "]
         [:li
          [:kbd "right"] "/" [:kbd "left"]
          " step forward / back in time; "]
         [:li
          [:kbd "space"]
          " starts or stops running. "]
         [:li
          [:kbd "/"] " sorts the selected layer (Shift for all);"]
         [:li
          [:kbd "\\"] " clears sorting (Shift for all);"]
         [:li
          [:kbd "+"] " adds a facet (Shift for all);"]
         [:li
          [:kbd "-"] " clears facets (Shift for all);"]]
        ]]
      [:div.col-lg-3.col-md-4.col-sm-6
       [:h4 "Colour legend"]
       [:ul
        [:li [:b {:style {:color "red"}} "Red"]
         ": active"]
        [:li [:b {:style {:color "blue"}} "Blue"]
         ": predicted"]
        [:li [:b {:style {:color "purple"}} "Purple"]
         ": active+predicted (i.e. recognised)"]
        [:li [:b {:style {:color "green"}} "Green"]
         ": growing (new synapses)"]]]
      [:div.col-lg-3.col-md-4.col-sm-6
       [:h4 "Segment diagrams"]
       (let [conn-act-px 115
             disc-act-px 25
             stimulus-th-px 100
             learning-th-px 60
             conn-tot-px 130
             disc-tot-px 40
             width-label (fn [y-transform width label-above? text]
                           [:g {:transform (str "translate(0," y-transform ")")}
                            [:line {:x1 0 :y1 -3 :x2 0 :y2 3
                                    :stroke "black" :stroke-width 1}]
                            [:line {:x1 width :y1 -3 :x2 width :y2 3
                                    :stroke "black" :stroke-width 1}]
                            [:line {:x1 0 :y1 0 :x2 width :y2 0
                                    :stroke "black" :stroke-width 1}]
                            [:text {:x 0 :y (if label-above? -5 15)
                                    :font-family "sans-serif"
                                    :font-size "13px"}
                             text]])]
         [:div
          [:p "Segments are displayed as a pair of progress bars.
              The meaning of each of the widths is shown below."]
          [:div {:style {:margin-left 20}}
           [:svg {:width 200 :height 220}
            ;; Hand-coded adjustment to avoid cutoff.
            [:g {:transform (str "translate(1,0)")}
             [width-label 15 conn-tot-px true "connected synapses"]
             [width-label 45 conn-act-px true "active connected synapses"]
             [width-label 75 stimulus-th-px true "stimulus threshold"]
             [:g {:transform (str "translate(0," 90 ")")}
              ;; Connected totals
              [:rect {:x 0 :y 1 :width conn-tot-px :height 8
                      :stroke "none"
                      :fill "black"
                      :fill-opacity "0.1"}]
              [:rect {:x 0 :y 11 :width disc-tot-px :height 8
                      :stroke "none"
                      :fill "black"
                      :fill-opacity "0.1"}]
              ;; Actives
              [:rect {:x 0 :y 1 :width conn-act-px :height 8
                      :stroke "none"
                      :fill "red"}]
              [:rect {:x 0 :y 11 :width disc-act-px :height 8
                      :stroke "none"
                      :fill "red"}]
              ;; Thresholds
              [:rect {:x 0 :y 0 :width stimulus-th-px :height 10
                      :stroke "black"
                      :fill-opacity 0}]
              [:rect {:x 0 :y 10 :width learning-th-px :height 10
                      :stroke "black"
                      :fill-opacity 0}]]
             [width-label 125 learning-th-px false "learning threshold"]
             [width-label 155 disc-tot-px false "disconnected synapses"]
             [width-label 185 disc-act-px false "active disconnected synapses"]]]]])]]
     [:hr]]))

(defn sanity-app
  [_ _ _ features _ _ _ selection steps network-shape _ _ _ into-journal _]
  (let [show-help (atom false)
        viz-expanded (atom false)
        time-plots-tab (when (features :time-plots)
                         (time-plots-tab-builder steps into-journal))
        cell-sdrs-tab (when (features :cell-SDRs)
                        (cell-sdrs-tab-builder steps network-shape selection
                                               into-journal))]
    (fn [title model-tab main-pane _ capture-options viz-options current-tab
         selection steps network-shape series-colors into-viz into-sim
         into-journal debug-data]
      [:div
       [navbar title features steps show-help viz-options viz-expanded
        network-shape into-viz into-sim]
       [help-block show-help]
       [:div.container-fluid
        [:div.row
         [:div.col-sm-9.viz-expandable
          main-pane]
         [:div.col-sm-3
          [tabs
           current-tab
           (remove nil?
                   [(when model-tab
                      [:model model-tab])
                    (when (features :capture)
                      [:capture [capture-tab capture-options]])
                    (when (features :drawing)
                      [:drawing [drawing-tab features viz-options
                                 capture-options]])
                    (when (features :params)
                      [:params [parameters-tab network-shape selection
                                into-sim]])
                    (when time-plots-tab
                      [:time-plots [time-plots-tab series-colors]])
                    (when cell-sdrs-tab
                      [:cell-SDRs [cell-sdrs-tab]])
                    (when (features :sources)
                      [:sources [sources-tab network-shape selection
                                 series-colors into-journal]])
                    (when (features :details)
                      [:details [details-tab selection into-journal]])
                    [:debug [debug-tab debug-data]]])]]]
        [:div#loading-message "loading"]]])))
