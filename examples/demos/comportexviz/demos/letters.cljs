(ns comportexviz.demos.letters
  (:require [org.nfrac.comportex.demos.letters :as demo]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.util :as util]
            [org.nfrac.comportex.protocols :as p]
            [comportexviz.sentence-drawing :refer [draw-text-fn]]
            [comportexviz.main :as main :refer [main-options model sim-step!]]
            [reagent.core :as reagent :refer [atom]]
            [reagent-forms.core :refer [bind-fields]]
            [goog.string :as gstring]
            [goog.string.format]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [cljs.core.async :as async])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [comportexviz.macros :refer [with-ui-loading-message]]))

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

(def n-predictions 8)

(def config
  (atom {:n-regions 1
         :encoder :block}))

(def text-to-send
  (atom
"Jane has eyes.
Jane has a head.
Jane has a mouth.
Jane has a brain.
Jane has a book.
Jane has no friend.

Chifung has eyes.
Chifung has a head.
Chifung has a mouth.
Chifung has a brain.
Chifung has no book.
Chifung has a friend."))

(def world-buffer (async/buffer 1000))
(def world-c (async/chan world-buffer))

(defn touch!
  "Alters the atom (metadata) just to trigger Regent to re-render dependent components."
  ;; (here `model`, to display world-buffer size)
  [atom]
  (swap! atom vary-meta update ::touch-flag not))

(defn set-world
  []
  (let [draw (draw-text-fn n-predictions)]
    (main/set-world (->> world-c
                         (async/map< (util/keep-history-middleware 400 :value :history))
                         (async/map< #(vary-meta % assoc
                                                 :comportexviz/draw-world
                                                 draw))))))

(defn reset-model-from-ui
  []
  (let [n-regions (:n-regions @config)
        encoder (case (:encoder @config)
                  :block demo/block-encoder
                  :random demo/random-encoder)]
    (with-ui-loading-message
      (main/set-model
       (core/regions-in-series core/sensory-region (core/sensory-input encoder)
                               n-regions demo/spec)))))

(defn immediate-key-down
  [e]
  (when-let [[x] (->> (or (.-keyCode e) (.-charCode e))
                      (.fromCharCode js/String)
                      (demo/clean-text)
                      (seq))]
    (async/put! world-c {:value (str x)}))
  (touch! model)
  (.preventDefault e))

(defn do-send-text
  [e]
  (when-let [xs (seq (demo/clean-text @text-to-send))]
    (async/onto-chan world-c (for [x xs] {:value (str x)})
                     false)
    (touch! model)))

(def config-template
  [:div
   [:div.row
    [:div.col-sm-6
     [:label "Number of regions:"]]
    [:div.col-sm-6
     [:input {:field :numeric
              :id :n-regions
              :size 3}]]]
   [:div.row
    [:div.col-sm-6
     [:label "Letter encoder:"]]
    [:div.col-sm-6
     [:select {:field :list
               :id :encoder}
      [:option {:key :block} "block"]
      [:option {:key :random} "random"]]]]
   [:div.row
    [:div.col-sm-6
     [:button.btn.btn-default
      {:on-click #(reset-model-from-ui)}
      "Restart with new model"]]]
   [:p.text-danger "This will reset all parameter values."]
   ])

(defn model-tab
  []
  [:div
   [:p "In this example, text input is presented as a sequence of letters.
        Allowed characters are letters, numbers, space, period and question
        mark."]

   [:h3 "HTM model"]
   [bind-fields config-template config]

   [:h3 "Input " [:small "Letter sequences"]]
   [:p.text-info
    ;; force re-render when model atom changes:
    {:to-force-reagent-refresh (count @model)}
    (str (count world-buffer) " queued input values.")]
   [:div.well
    "Immediate input as you type: "
    [:input {:size 2 :maxLength 1
             :on-key-down immediate-key-down}]]
   [:div.well
    [:textarea {:style {:width "90%" :height "10em"}
                :value @text-to-send
                :on-change #(reset! text-to-send
                                    (-> % .-target forms/getValue))}]
    [:button.btn.btn-default {:on-click do-send-text}
     "Send text block input"]]
   ]
  )

(defn drawing-tab []
  [:div#comportex-drawing])
(defn parameters-tab []
  [:div#comportex-parameters])
(defn plots-tab []
  [:div#comportex-plots])
(defn details-tab []
  [:div#comportex-details
   [:textarea#detail-text]])

(defn tabs
  [tab-cmps current-tab]
  [:div
   (into [:ul.nav.nav-tabs]
         (for [[k _] tab-cmps]
           [:li {:role "presentation"
                 :class (if (= @current-tab k) "active")}
            [:a {:href "#"
                 :on-click #(reset! current-tab k)}
             (name k)]]))
   (into [:div.tabs]
         (for [[k cmp] tab-cmps]
           [:div {:style (if (not= k @current-tab) {:display "none"})}
            [cmp]]))
   ])

(def tab-cmps
  [[:model model-tab]
   [:drawing drawing-tab]
   [:params parameters-tab]
   [:plots plots-tab]
   [:details details-tab]])

(defn navbar
  [main-options model show-help]
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
      ;; pause button
      [:li (if-not (:sim-go? @main-options) {:class "hidden"})
       [:button.btn.btn-default.navbar-btn
        {:type :button
         :on-click #(swap! main-options assoc :sim-go? false)}
        "Pause"]]
      ;; sim rate
      [:li (if-not (:sim-go? @main-options) {:class "hidden"})
       [:p.navbar-text (gstring/format "%.1f/sec."
                                       (sim-rate @model))]]
      ;; run button
      [:li (if (:sim-go? @main-options) {:class "hidden"})
       [:button.btn.btn-primary.navbar-btn
        {:type :button
         :on-click #(swap! main-options assoc :sim-go? true)}
        "Run"]]
      ;; step back
      [:li (if (:sim-go? @main-options) {:class "hidden"})
       [:button.btn.btn-default.navbar-btn
        {:type :button
         :on-click #(sim-step!)}
        [:span.glyphicon.glyphicon-step-backward {:aria-hidden "true"}]
        [:span.sr-only "Step backward"]]]
      ;; step forward
      [:li (if (:sim-go? @main-options) {:class "hidden"})
       [:button.btn.btn-default.navbar-btn
        {:type :button
         :on-click #(sim-step!)}
        [:span.glyphicon.glyphicon-step-forward {:aria-hidden "true"}]
        [:span.sr-only "Step forward"]]]
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
      ]
     [:ul.nav.navbar-nav.navbar-right
      [:li.dropdown
       [:a.dropdown-toggle {:data-toggle "dropdown"
                            :role "button"
                            :href "#"}
        "Draw" [:span.caret]]
       [:ul.dropdown-menu {:role "menu"}
        ;; TODO
        ]
       ]
      [:li (if @show-help {:class "active"})
       [:a {:href "#"
            :on-click #(swap! show-help not)}
        "Help"]]
      ]
     ]
    ]])

(defn help-bar
  [show-help]
  (if @show-help
    [:div.container-fluid
     [:p "Right / left arrow keys move forward / back in time.
      Up / down arrow keys select columns.
      Click on a column to show its cells.
      Page up / page down to scroll display.
      TODO: improve this text!"]]))

(defn comportex-app
  []
  (let [current-tab (atom (ffirst tab-cmps))
        show-help (atom false)]
    (fn []
      [:div
       [navbar main-options model show-help]
       [help-bar show-help]
       [:div.container-fluid
        [:div.row
         [:div.col-sm-8
          [:canvas#comportex-viz]
          ]
         [:div.col-sm-4
          [tabs tab-cmps current-tab]
          ]]
        [:div#loading-message "loading"]]])))

(defn ^:export init
  []
  (reagent/render [comportex-app] (dom/getElement "comportex-app"))
  (set-world)
  (reset-model-from-ui)
  (swap! main-options assoc :sim-go? true))
