(ns comportexviz.simulation.common
  (:require [cljs.core.async :as async :refer [chan put! <!]]
            [comportexviz.helpers :as helpers]
            [org.nfrac.comportex.protocols :as p])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

;;; Shared code for various servers

(def whitelisted
  {:timestep p/timestep})

(defn- extract-data [m route]
  (helpers/update-routed
   m route
   (fn [m route]
     (let [{:keys [values methods]} route]
       (-> (select-keys m values)
           (assoc :method-results
                  (zipmap methods
                          (->> methods
                               (map (fn [[method & args]]
                                      (let [f (whitelisted method)]
                                        (assert f)
                                        (apply f m args))))))))))))

(defn- sim-step! [model in-value out]
  (->> (swap! model p/htm-step in-value)
       (put! out)))

(defn now [] (.getTime (js/Date.)))

(defn should-go?! [options]
  (let [{:keys [go? force-n-steps step-ms]} @options]
    (cond
      go? step-ms
      (pos? force-n-steps) (do
                             (swap! options update :force-n-steps dec)
                             0)
      :else false)))

(defn- simulation-loop [model world out options sim-closed?]
  (go
    (swap! model assoc
           :run-start {:time (now)
                       :timestep (p/timestep @model)})

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
    (when-not @sim-closed?
      (when-let [[command & xs] (<! commands)]
        (do (case command
              :set-spec (let [[path v] xs]
                          (swap! model assoc-in path v))
              :restart (let [[result] xs]
                         (swap! model p/restart)
                         (put! result :done)))
            (recur))))
    (reset! sim-closed? true)))
