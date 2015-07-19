(ns comportexviz.server.simulation
  (:require #?(:clj [clojure.core.async :as async :refer [put! <! go go-loop]]
               :cljs [cljs.core.async :as async :refer [put! <!]])
            [comportexviz.details]
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

(def default-sim-options
  {:go? false
   :step-ms 20
   :force-n-steps 0})

(defn- sim-step! [model in-value out]
  (->> (swap! model p/htm-step in-value)
       (put! out)))

(defn should-go?! [options]
  (let [{:keys [go? force-n-steps step-ms]} @options]
    (cond
      go? step-ms
      (pos? force-n-steps) (do
                             (swap! options update :force-n-steps dec)
                             0)
      :else false)))

(defn simulation-loop [model world out options sim-closed?]
  (go
    (if (loop []
          (when (not @sim-closed?)
            (if-let [t (should-go?! options)]
              (when-let [in-value (<! world)]
                (sim-step! model in-value out)
                (<! (async/timeout t))
                (recur))
              true)))
      (add-watch options :run-sim
                 (fn [_ _ _ _]
                   (remove-watch options :run-sim)
                   (simulation-loop model world out options sim-closed?)))
      (reset! sim-closed? true))))

(defn handle-commands [commands model sim-closed?]
  (go-loop []
    (if-not @sim-closed?
      (when-let [[command & xs] (<! commands)]
        (case command
          :set-spec (let [[path v] xs]
                      (swap! model assoc-in path v))
          :restart (let [[result] xs]
                     (swap! model p/restart)
                     (put! result :done)))
        (recur))
      (reset! sim-closed? true))))

;; To end the simulation, close `world-c` and/or `commands-c`. If only one is
;; closed, the simulation may consume another value from the other before
;; closing.
(defn start
  [steps-c model-atom world-c options commands-c]
  (let [sim-closed? (atom false)]
    (when commands-c
      (handle-commands commands-c model-atom sim-closed?))
    (simulation-loop model-atom world-c steps-c options sim-closed?))
  nil)
