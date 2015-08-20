(ns comportexviz.controls-ui
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [goog.dom.classes :as classes]
            [clojure.string :as str]
            [cljs.core.async :as async :refer [put! <!]]
            [cljs.reader]
            [comportexviz.helpers :as helpers]
            [comportexviz.plots :as plots]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
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

(defn parameters-tab
  [step-template _ into-sim _]

  (add-watch step-template ::push-to-server
             (fn [_ _ oldv newv]
               (when-not (nil? oldv) ;; don't push when getting initial template
                 (fn [_ _ prev-st st]
                   (assert @into-sim)
                   (doseq [path (for [[r-id rgn] (:regions st)
                                      l-id (keys rgn)]
                                  [:regions r-id l-id :spec])
                           :let [old-spec (get-in prev-st path)
                                 new-spec (get-in st path)]]
                     (when (not= old-spec new-spec)
                       (put! @into-sim [:set-spec path new-spec])))))))

  (let [partypes (cljs.core/atom {})] ;; immutable cache
    (fn [step-template selection into-sim local-targets]
      [:div
       [:p.text-muted "Read/write model parameters of the selected region layer,
                    with immediate effect. Click a layer to select it."]
       [:p.text-info.text-center (str (some-> (:region @selection) name) " "
                                      (some-> (:layer @selection) name))]
       (into
        [:div.form-horizontal]
        (when @step-template
          (let [sel-region (:region @selection)
                sel-layer (:layer @selection)
                spec-path [:regions sel-region sel-layer :spec]
                spec (get-in @step-template spec-path)]
            (concat
             (for [[k v] (sort spec)
                   :let [typ (or (get @partypes k)
                                 (get (swap! partypes assoc k (param-type v))
                                      k))
                         setv! (if @into-sim
                                 #(swap! step-template assoc-in spec-path
                                         (assoc spec k %))
                                 identity)]]
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
                                        newval (if (and (vector? x) (every? integer? x))
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
                                    (setv! newval)))}])]])
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

                 (if @into-sim
                   {:on-click #(let [finished (async/chan)]
                                 (put! @into-sim
                                       [:restart (channel-proxy/register!
                                                  local-targets finished)])
                                 (helpers/ui-loading-message-until finished))}
                   {:disabled "disabled"})
                 "Rebuild model"]
                [:p.small "This will not reset, or otherwise alter, the input stream."]]
               ]
              [:h4 "Current spec value"]
              [:pre (str spec)]
              ])
            )))]))
  )

(defn gather-col-state-history!
  [col-state-history step into-journal local-targets]
  (let [response-c (async/chan)]
    (put! @into-journal [:get-column-state-freqs
                         (:model-id step)
                         (channel-proxy/register! local-targets response-c)])
    (go
      (let [r (<! response-c)]
        (swap! col-state-history
               #(reduce
                 (fn [csh [layer-path col-state-freqs]]
                   (update csh layer-path
                           (fn [csf-log]
                             (conj (or csf-log
                                       (plots/empty-col-state-freqs-log))
                                   col-state-freqs))))
                 % r))))))

(defn ts-plots-tab-builder
  [steps into-journal local-targets]
  (let [col-state-history (atom {})]
    (add-watch steps ::ts-plot-data
               (fn [_ _ _ xs]
                 (gather-col-state-history! col-state-history (first xs)
                                            into-journal local-targets)))
    (fn ts-plots-tab [series-colors]
      [:div
       [:p.text-muted "Time series of cortical column activity."]
       [:div
        (for [[path csf-log] @col-state-history
              :let [[region-key layer-id] path]]
          ^{:key [region-key layer-id]}
          [:fieldset
           [:legend (str (name region-key) " " (name layer-id))]
           [plots/ts-freqs-plot-cmp csf-log series-colors]])]])))

(defn cell-plots-tab
  [step-template selection series-colors into-journal local-targets]
  [:div
   [:p.text-muted "Plots of cell excitation broken down by source."]
   [:div
    (when @step-template
      (for [[region-key rgn] (:regions @step-template)
            layer-id (keys rgn)]
        ^{:key [region-key layer-id]}
        [:fieldset
         [:legend (str (name region-key) " " (name layer-id))]
         [plots/cell-excitation-plot-cmp step-template selection series-colors
          region-key layer-id into-journal local-targets]]))]])

(defn transitions-tab-builder
  [steps step-template selection into-journal local-targets]
  (let [enable (async/chan)
        transitions-plot (atom
                          (fn []
                            [:button.btn.btn-warning.btn-block
                             {:on-click #(put! enable :enable)}
                             "Enable"]))]
    (go
      (when (<! enable)
        (reset! transitions-plot
                (plots/transitions-plot-builder steps step-template selection
                                                into-journal local-targets))))
    (fn transitions-tab []
      [:div
       [:p.text-muted "Cell SDRs and their transitions meeting
   seg-learn-threshold. Labelled by inputs for interpretability."]
       [:div
        [@transitions-plot]]])))

(defn fetch-details-text!
  [into-journal text-response sel local-targets]
  (when (:col sel)
    (let [{:keys [model-id region layer col]} sel
          response-c (async/chan)]
      (put! @into-journal [:get-details-text model-id region layer col
                           (channel-proxy/register! local-targets
                                                    response-c)])
      (go
        (reset! text-response [sel (<! response-c)])))))

(defn details-tab
  [selection into-journal local-targets]
  (let [text-response (atom [nil ""])]
    (reagent/create-class
     {:component-will-mount
      (fn [_]
        (add-watch selection :fetch-details-text
                   (fn [_ _ _ sel]
                     (reset! text-response [sel ""])
                     (fetch-details-text! into-journal text-response sel
                                          local-targets)))

        (fetch-details-text! into-journal text-response @selection
                             local-targets))

      :component-will-unmount
      (fn [_]
        (remove-watch selection :fetch-details-text))

      :reagent-render
      (fn [_ _]
        [:div
         [:p.text-muted "The details of model state on the selected time step, selected column."]
         [:pre.pre-scrollable {:style {:height "90vh" :resize "both"}}
          (let [[sel-for-text text] @text-response]
            (when (= sel-for-text @selection)
              text))]
         [:p.text-muted [:small "(scrollable)"]]
         [:hr]
         [:p.text-muted "If you're brave:"]
         [:button.btn.btn-warning.btn-block
          {:on-click (fn [e]
                       (let [response-c (async/chan)]
                         (put! @into-journal
                               [:get-model
                                (:model-id @selection)
                                (channel-proxy/register! local-targets
                                                         response-c)
                                true])
                         (go
                           (println (<! response-c))))
                       (.preventDefault e))}
          "Dump entire model to console"]
         ])})))

(def viz-options-template
  (let [chbox (fn [id label]
                [:div.checkbox
                 [:label
                  [:input {:field :checkbox :id id}]
                  (str " " label)]])
        group (fn [title content]
                [:div.col-sm-6
                 [:div.panel.panel-default
                  [:div.panel-heading
                   [:h4.panel-title title]]
                  content]])]
    [:div
     [:p.text-muted "Select drawing options, with immediate effect."]
     [:div.panel.panel-default
      [:div.panel-body
       [:div
        [:label " Keep "
         [:input {:field :numeric
                  :id :keep-steps
                  :size 4}]
         " steps of history"]]
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
      (group "Inputs"
             [:div.panel-body
              (chbox :input.active "Active bits")
              (chbox :input.predicted "Predicted bits")
              ])
      (group "Columns"
             [:div.panel-body
              (chbox :columns.overlaps "Overlaps")
              (chbox :columns.active-freq "Active-duty")
              (chbox :columns.boosts "Boosts")
              (chbox :columns.n-segments "N.segments")
              (chbox :columns.active "Active")
              (chbox :columns.predictive "Predictive")
              (chbox :columns.temporal-pooling "Temporal Pooling")
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
              (chbox :ff-synapses.disconnected "Disconnected")
              (chbox :ff-synapses.inactive "Inactive")
              (chbox :ff-synapses.permanences "Permanences")
              ])
      (group "Distal synapses"
             [:div.panel-body
              [:div "(selected column) "
               [:select {:field :list
                         :id :distal-synapses.from}
                [:option {:key :all} "all cell segments"]
                [:option {:key :selected} "selected segment"]
                [:option {:key :none} "none"]
                ]]
              (chbox :distal-synapses.disconnected "Disconnected")
              (chbox :distal-synapses.inactive "Inactive")
              (chbox :distal-synapses.permanences "Permanences")
              ])]
     ]))

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
  [steps show-help viz-options viz-expanded into-viz into-sim local-targets]
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

    (let [m [:subscribe-to-status (channel-proxy/register! local-targets
                                                           subscriber-c)]]
      (add-watch into-sim ::subscribe-to-status
                 (fn [_ _ _ c]
                   (when c
                     (put! c m))))
      (when @into-sim
        (put! @into-sim m)))

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

    (fn [_ _ _ _ _ _]
      [:nav.navbar.navbar-default
       [:div.container-fluid
        [:div.navbar-header
         [:button.navbar-toggle.collapsed {:data-toggle "collapse"
                                           :data-target "#comportex-navbar-collapse"}
          [:span.icon-bar] [:span.icon-bar] [:span.icon-bar]]
         [:a.navbar-brand {:href "https://github.com/nupic-community/comportexviz"}
          "ComportexViz"]]
        [:div.collapse.navbar-collapse {:id "comportex-navbar-collapse"}
         [:ul.nav.navbar-nav
          ;; step back
          [:li
           [:button.btn.btn-default.navbar-btn
            (cond-> {:type :button
                     :on-click (send-command into-viz :step-backward)
                     :title "Step backward in time"}
              (not @into-sim) (assoc :disabled "disabled"))
            [:span.glyphicon.glyphicon-step-backward {:aria-hidden "true"}]
            [:span.visible-xs-inline " Step backward"]]]
          ;; step forward
          [:li
           [:button.btn.btn-default.navbar-btn
            (cond-> {:type :button
                     :on-click (send-command into-viz :step-forward)
                     :title "Step forward in time"}
              (not @into-sim) (assoc :disabled "disabled"))
            [:span.glyphicon.glyphicon-step-forward {:aria-hidden "true"}]
            [:span.visible-xs-inline " Step forward"]]]
          ;; pause button
          [:li (when-not @going? {:class "hidden"})
           [:button.btn.btn-default.navbar-btn
            (cond-> {:type :button
                     :on-click #(put! @into-sim [:pause])
                     :style {:width "5em"}}
              (not @into-sim) (assoc :disabled "disabled"))
            "Pause"]]
          ;; run button
          [:li (when @going? {:class "hidden"})
           [:button.btn.btn-primary.navbar-btn
            (cond-> {:type :button
                     :on-click #(put! @into-sim [:run])
                     :style {:width "5em"}}
              (not @into-sim) (assoc :disabled "disabled"))
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
          [:li.dropdown
           [:a.dropdown-toggle {:data-toggle "dropdown"
                                :role "button"
                                :href "#"}
            "Speed" [:span.caret]]
           [:ul.dropdown-menu {:role "menu"}
            [:li [:a {:href "#"
                      :on-click (fn []
                                  (put! @into-sim [:set-step-ms 0])
                                  (swap! viz-options assoc-in
                                         [:drawing :anim-every] 1))}
                  "max sim speed"]]
            [:li [:a {:href "#"
                      :on-click (fn []
                                  (put! @into-sim [:set-step-ms 0])
                                  (swap! viz-options assoc-in
                                         [:drawing :anim-every] 100))}
                  "max sim speed, draw every 100 steps"]]
            [:li [:a {:href "#"
                      :on-click (fn []
                                  (put! @into-sim [:set-step-ms 250])
                                  (swap! viz-options assoc-in
                                         [:drawing :anim-every] 1))}
                  "limit to 4 steps/sec."]]
            [:li [:a {:href "#"
                      :on-click (fn []
                                  (put! @into-sim [:set-step-ms 500])
                                  (swap! viz-options assoc-in
                                         [:drawing :anim-every] 1))}
                  "limit to 2 steps/sec."]]
            [:li [:a {:href "#"
                      :on-click (fn []
                                  (put! @into-sim [:set-step-ms 1000])
                                  (swap! viz-options assoc-in
                                         [:drawing :anim-every] 1))}
                  "limit to 1 step/sec."]]]]
          [:li (if @show-help {:class "active"})
           [:a {:href "#"
                :on-click (fn [e]
                            (swap! show-help not)
                            (.preventDefault e))}
            "Help"]]
          ]
         ]
        ]])))

(defn tabs
  [tab-cmps]
  (let [current-tab (atom (ffirst tab-cmps))]
    (fn [tab-cmps]
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
          cmp)]])))

(defn help-block
  [show-help]
  (if @show-help
    [:div.container-fluid
     [:div.row
      [:div.col-lg-3.col-md-4.col-sm-6
       [:h4 "Overview"]
       [:p [:a {:href "https://github.com/nupic-community/comportexviz"}
            "ComportexViz"]
        " runs HTM models in the browser with interactive
       controls. The model state from recent timesteps is kept, so you can step
       back in time. You can inspect input values, encoded input bits, and the
       columns that make up cortical region layers. Within a column you can inspect
       cells and their distal dendrite segments. Feed-forward and distal synapses
       can be shown."]]
      [:div.col-lg-3.col-md-4.col-sm-6
       [:h4 "Display"]
       [:p "Kept timesteps are shown in a row at the top of the display.
      Click one to jump to it.
      Below that, the blocks represent input fields (squares) and
      layers of cortical columns (circles). Depending on the display mode,
      these may be shown in 2D grids from a single time step, or as one
      vertical line per timestep, allowing several time steps to be shown
      in series. Also, summarised time series are shown in the 'plots' tab."]]
      [:div.col-lg-3.col-md-4.col-sm-6
       [:h4 "Selection"]
       [:p "Click on the main canvas to select one column of cells,
      within some region layer. The individual cells
      and their distal dendrite segments will be shown.
      If you click off the layer, the column will be de-selected, but
      the layer will remain selected. Its parameters can be seen and edited in
      the 'params' tab."]]
      [:div.col-lg-3.col-md-4.col-sm-6
       [:h4 "Key controls"]
       [:p "When the main canvas is in focus, "
        [:kbd "up"] "/" [:kbd "down"]
        " select columns; "
        [:kbd "page up"] "/" [:kbd "page down"]
        " scroll the visible field; "
        [:kbd "right"] "/" [:kbd "left"]
        " step forward / back in time; "
        [:kbd "space"]
        " starts or stops running. "
        ]]]
     [:hr]]))

(defn comportexviz-app
  [_ _ _ selection steps step-template _ _ _ into-journal local-targets]
  (let [show-help (atom false)
        viz-expanded (atom false)
        ts-plots-tab (ts-plots-tab-builder steps into-journal local-targets)
        transitions-tab (transitions-tab-builder steps step-template selection
                                                 into-journal local-targets)]
    (fn [model-tab main-pane viz-options selection steps step-template
         series-colors into-viz into-sim into-journal local-targets]
     [:div
      [navbar steps show-help viz-options viz-expanded into-viz into-sim
       local-targets]
      [help-block show-help]
      [:div.container-fluid
       [:div.row
        [:div.col-sm-9.viz-expandable
         [main-pane]]
        [:div.col-sm-3
         [tabs
          [[:model [model-tab]]
           [:drawing [bind-fields viz-options-template viz-options]]
           [:params [parameters-tab step-template selection into-sim
                     local-targets]]
           [:ts-plots [ts-plots-tab series-colors]]
           [:cell-plots [cell-plots-tab step-template selection series-colors
                         into-journal local-targets]]
           [:transitions [transitions-tab]]
           [:details [details-tab selection into-journal local-targets]]]]
         ]]
       [:div#loading-message "loading"]]])))
