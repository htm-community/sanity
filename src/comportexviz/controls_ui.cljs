(ns comportexviz.controls-ui
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [goog.string :as gstring]
            [goog.string.format]
            [clojure.string :as str]
            [cljs.reader]
            [comportexviz.plots :as plots]
            [comportexviz.details]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p])
  (:require-macros [comportexviz.macros :refer [with-ui-loading-message]]))

(defn now [] (.getTime (js/Date.)))

(defn sim-rate
  "Returns the simulation rate in timesteps per second for current
   run."
  [model]
  (if (:time (:run-start model))
    (let [m (:run-start model)
          dur-ms (- (now)
                    (:time m))
          steps (- (p/timestep model)
                   (:timestep m))]
      (-> (/ steps dur-ms)
          (* 1000)))
    0))

(defn param-type [v]
  (cond
    (or (true? v) (false? v)) :boolean
    (vector? v) :vector
    :else :number))

(defn parameters-tab
  [model selection]
  (let [partypes (cljs.core/atom {})] ;; immutable cache
    (fn []
      [:div
       [:p.text-muted "Read/write model parameters of the selected region layer,
                    with immediate effect. Click a layer to select it."]
       [:p.text-info.text-center (str (some-> (:region @selection) name) " "
                                      (some-> (:layer @selection) name))]
       (into
        [:div.form-horizontal]
        (when @model
          (let [sel-region (:region @selection)
                sel-layer (:layer @selection)
                rgn (get-in @model [:regions sel-region])
                lyr (get rgn sel-layer)
                spec (p/params lyr)]
            (concat
             (for [[k v] (sort spec)
                   :let [path [:regions sel-region sel-layer :spec k]
                         typ (or (get @partypes k)
                                 (get (swap! partypes assoc k (param-type v))
                                      k))]]
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
                     :on-change #(swap! model update-in path not)}]
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
                                    (swap! model assoc-in path newval)))}]
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
                                    (swap! model assoc-in path newval))
                                  )}])]
                ])
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
                 {:on-click #(with-ui-loading-message
                               (swap! model p/reset))}
                 "Rebuild model"]
                [:p.small "This will not reset, or otherwise alter, the input stream."]]
               ]
              [:h4 "Current spec value"]
              [:pre (str spec)]
              ])
            )))]))
  )

(defn ts-freqs-plot-cmp
  [steps region-key layer-id series-colors]
  (let [series-keys [:active :active-predicted :predicted]
        step-freqs (atom nil)
        agg-freqs-ts (plots/aggregating-ts step-freqs 200)]
    (add-watch steps [:calc-freqs region-key layer-id] ;; unique key per layer
               (fn [_ _ _ v]
                 (let [htm (first v)
                       freqs (-> htm :regions region-key
                                 (core/column-state-freqs layer-id))]
                   (reset! step-freqs freqs))))
    (fn [_ _ _ _]
      (let [el-id (str "comportexviz-plot-" (name region-key) (name layer-id))
            el (dom/getElement el-id)]
        ;; draw!
        (when el
          (set! (.-width el) (* 0.33 (- (.-innerWidth js/window) 20)))
          (set! (.-height el) 180)
          (plots/stacked-ts-plot el agg-freqs-ts series-keys series-colors))
        (when-not el
          ;; create data dependency for re-rendering
          @agg-freqs-ts)
        [:div
         [:canvas {:id el-id}]])
      )))

(defn plots-tab
  [steps series-colors]
  [:div
   [:p.text-muted "Time series of cortical column activity."]
   [:div
    (when (first @steps)
      (for [[region-key rgn] (:regions (first @steps))
            layer-id (core/layers rgn)]
        ^{:key [region-key layer-id]}
        [:fieldset
         [:legend (str (name region-key) " " (name layer-id))]
         [ts-freqs-plot-cmp steps region-key layer-id series-colors]
         ]))]
   ])

(defn details-tab
  [steps selection]
  [:div
   [:p.text-muted "The details of current state on the selected time step, selected column."]
   [:textarea.form-control
    {:id "detail-text"
     :rows 30
     :readOnly true
     :value (if (:col @selection)
              (let [dt (:dt @selection)]
                (comportexviz.details/detail-text (nth @steps dt)
                                                  (nth @steps (inc dt))
                                                  @selection)))}
    ]])

(def viz-options-template
  (let [item (fn [id label]
               [:li [:label [:input {:field :checkbox :id id}]
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
       [:ul.list-unstyled
        [:li [:label " Keep "
              [:input {:field :numeric
                       :id :keep-steps
                       :size 4}]
              " steps of history"]]
        [:li [:label
              [:input {:field :radio
                       :name :drawing.display-mode
                       :value :one-d}]
              " Draw "
              [:input {:field :numeric
                       :id :drawing.draw-steps
                       :size 4}]
              " steps in 1D"]]
        [:li [:label
              [:input {:field :radio
                       :name :drawing.display-mode
                       :value :two-d}]
              " Draw one step in 2D"]]
        ]]]
     [:div.row
      (group "Inputs"
             [:div.panel-body
              [:ul.list-unstyled
               (item :input.active "Active bits")
               (item :input.predicted "Predicted bits")
               ]])
      (group "Columns"
             [:div.panel-body
              [:ul.list-unstyled
               (item :columns.overlaps "Overlap scores")
               (item :columns.active-freq "Activation freq")
               (item :columns.boosts "Boost factors")
               (item :columns.n-segments "Num segments")
               (item :columns.active "Active columns")
               (item :columns.predictive "Predictive columns")
               ]])
      ]
     [:div.row
      (group "Feed-forward synapses"
             [:div.panel-body
              [:p.help-block "To all active columns, or the selected column."]
              [:ul.list-unstyled
               (item :ff-synapses.active "Active")
               (item :ff-synapses.inactive "Inactive")
               (item :ff-synapses.disconnected "Disconnected")
               (item :ff-synapses.permanences "Permanences")
               ]])
      (group "Distal synapses"
             [:div.panel-body
              [:p.help-block "To distal dendrite segments of cells in the selected column."]
              [:ul.list-unstyled
               (item :distal-synapses.active "Active")
               (item :distal-synapses.inactive "Inactive")
               (item :distal-synapses.disconnected "Disconnected")
               (item :distal-synapses.permanences "Permanences")
               ]])
      ]
     ]))

(defn navbar
  [main-options model show-help controls viz-options]
  [:nav.navbar.navbar-default
   [:div.container-fluid
    [:div.navbar-header
     [:button.navbar-toggle.collpased {:data-toggle "collapse"
                                       :data-target "#comportex-navbar-collapse"}
      [:span.icon-bar] [:span.icon-bar] [:span.icon-bar]]
     [:a.navbar-brand {:href "https://github.com/nupic-community/comportexviz"}
      "ComportexViz"]]
    [:div.collapse.navbar-collapse {:id "comportex-navbar-collapse"}
     [:ul.nav.navbar-nav
      ;; step back
      [:li
       [:button.btn.btn-default.navbar-btn
        {:type :button
         :on-click (fn [e]
                     (let [f (:step-backward controls)]
                       (f))
                     (.preventDefault e))
         :title "Step backward in time"}
        [:span.glyphicon.glyphicon-step-backward {:aria-hidden "true"}]
        [:span.sr-only "Step backward"]]]
      ;; step forward
      [:li
       [:button.btn.btn-default.navbar-btn
        {:type :button
         :on-click (fn [e]
                     (let [f (:step-forward controls)]
                       (f))
                     (.preventDefault e))
         :title "Step forward in time"}
        [:span.glyphicon.glyphicon-step-forward {:aria-hidden "true"}]
        [:span.sr-only "Step forward"]]]
      ;; pause button
      [:li (if-not (:sim-go? @main-options) {:class "hidden"})
       [:button.btn.btn-default.navbar-btn
        {:type :button
         :on-click #(swap! main-options assoc :sim-go? false)
         :style {:width "5em"}}
        "Pause"]]
      ;; run button
      [:li (if (:sim-go? @main-options) {:class "hidden"})
       [:button.btn.btn-primary.navbar-btn
        {:type :button
         :on-click #(swap! main-options assoc :sim-go? true)
         :style {:width "5em"}}
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
      ;; scroll down
      [:li
       [:button.btn.btn-default.navbar-btn
        {:type :button
         :on-click (fn [e]
                     (let [f (:scroll-down controls)]
                       (f))
                     (.preventDefault e))
         :title "Scroll down visible columns"}
        [:span.glyphicon.glyphicon-arrow-down {:aria-hidden "true"}]
        [:span.sr-only "Scroll down"]]]
      ;; scroll up
      [:li
       [:button.btn.btn-default.navbar-btn
        {:type :button
         :on-click (fn [e]
                     (let [f (:scroll-up controls)]
                       (f))
                     (.preventDefault e))
         :title "Scroll down visible columns"}
        [:span.glyphicon.glyphicon-arrow-up {:aria-hidden "true"}]
        [:span.sr-only "Scroll up"]]]]
     ;; right-aligned items
     [:ul.nav.navbar-nav.navbar-right
      ;; sim rate
      [:li (if-not (:sim-go? @main-options) {:class "hidden"})
       [:p.navbar-text (gstring/format "%.1f/sec."
                                       (sim-rate @model))]]
      ;; sim / anim options
      [:li.dropdown
       [:a.dropdown-toggle {:data-toggle "dropdown"
                            :role "button"
                            :href "#"}
        "Speed" [:span.caret]]
       [:ul.dropdown-menu {:role "menu"}
        [:li [:a {:href "#"
                  :on-click #(swap! main-options assoc :sim-step-ms 0
                                    :anim-every 1)}
              "max sim speed"]]
        [:li [:a {:href "#"
                  :on-click #(swap! main-options assoc :sim-step-ms 0
                                    :anim-every 100)}
              "max sim speed, draw every 100 steps"]]
        [:li [:a {:href "#"
                  :on-click #(swap! main-options assoc :sim-step-ms 250
                                    :anim-every 1)}
              "limit to 4 steps/sec."]]
        [:li [:a {:href "#"
                  :on-click #(swap! main-options assoc :sim-step-ms 500
                                    :anim-every 1)}
              "limit to 2 steps/sec."]]
        [:li [:a {:href "#"
                  :on-click #(swap! main-options assoc :sim-step-ms 1000
                                    :anim-every 1)}
              "limit to 1 step/sec."]]
        ]]
      [:li (if @show-help {:class "active"})
       [:a {:href "#"
            :on-click #(swap! show-help not)}
        "Help"]]
      ]
     ]
    ]])

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
       (into [:div.tabs]
             (for [[k cmp] tab-cmps]
               [:div {:style (if (not= k @current-tab) {:display "none"})}
                cmp]))
       ])))

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
       [:p "When the main canvas is selected, "
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

(def code-key
  {32 :space
   33 :page-up
   34 :page-down
   37 :left
   38 :up
   39 :right
   40 :down})

(def key->control-k
  {:left :step-backward
   :right :step-forward
   :up :column-up
   :down :column-down
   :page-up :scroll-up
   :page-down :scroll-down
   :space :toggle-run})

(defn canvas-key-down
  [e controls]
  (if-let [k (code-key (.-keyCode e))]
    (let [control-fn (-> k key->control-k controls)]
      (control-fn)
      (.preventDefault e))
    true))

(defn comportexviz-app
  [model-tab model main-options viz-options selection canvas-click controls steps
   series-colors]
  (let [show-help (atom false)]
    [:div
     [navbar main-options model show-help controls viz-options]
     [help-block show-help]
     [:div.container-fluid
      [:div.row
       [:div.col-sm-8
        [:canvas#comportex-viz {:on-click canvas-click
                                :on-key-down (fn [e] (canvas-key-down e controls))
                                :tabIndex 1}]
        ]
       [:div.col-sm-4
        [tabs
         [[:model [model-tab]]
          [:drawing [bind-fields viz-options-template viz-options]]
          [:params [parameters-tab model selection]]
          [:plots [plots-tab steps series-colors]]
          [:details [details-tab steps selection]]]]
        ]]
      [:div#loading-message "loading"]]]))
