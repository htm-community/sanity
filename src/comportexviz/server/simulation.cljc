(ns comportexviz.server.simulation
  (:require #?(:clj [clojure.core.async :as async :refer [put! <! go go-loop]]
               :cljs [cljs.core.async :as async :refer [put! <!]])
            [org.nfrac.comportex.core :as core]
            [org.nfrac.comportex.protocols :as p]
            [org.nfrac.comportex.util :as util])
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

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

(defn handle-commands [commands model options sim-closed?]
  (let [status-in (async/chan)
        status-mult (async/mult status-in)]
    (add-watch options ::push-to-subscribers
               (fn [_ _ oldv newv]
                 (let [{:keys [go?]} newv]
                   (when (not= go? (:go? oldv))
                     (put! status-in [go?])))))
    (go-loop []
      (if-not @sim-closed?
        (when-let [c (<! commands)]
          (let [[[command & xs] client-info] c]
            (case command
              :client-disconnect (do
                                   (println "SIMULATION: Client disconnected.")
                                   (async/untap status-mult (::status-subscriber
                                                             @client-info)))
              :client-reconnect (let [[old-client-info] xs
                                      {status-subscriber
                                       ::status-subscriber} old-client-info]
                                  (println "SIMULATION: Client reconnected.")
                                  (when status-subscriber
                                    (println (str "SIMULATION: Client "
                                                  "resubscribed to status."))
                                    (async/tap status-mult status-subscriber))
                                  (swap! client-info
                                         #(cond-> %
                                            status-subscriber
                                            (assoc ::status-subscriber
                                                   status-subscriber))))
              :step (swap! options update :force-n-steps inc)
              :set-spec (let [[path v] xs]
                          (swap! model assoc-in path v))
              :restart (let [[result response-c] xs]
                         (swap! model p/restart)
                         (put! response-c :done))
              :toggle (->> (swap! options update :go? not)
                           (println "SIMULATION TOGGLE. Current timestep:"
                                    (p/timestep @model)))
              :pause (do
                       (println "SIMULATION PAUSE. Current timestep:"
                                (p/timestep @model))
                       (swap! options assoc :go? false))
              :run (do
                     (println "SIMULATION RUN. Current timestep:"
                              (p/timestep @model))
                     (swap! options assoc :go? true))
              :set-step-ms (let [[t] xs]
                             (swap! options assoc :step-ms t))
              :subscribe-to-status (let [[ch] xs]
                                     (println (str "SIMULATION: Client "
                                                   "subscribed to status."))
                                     (async/tap status-mult ch)
                                     (swap! client-info assoc
                                            ::status-subscriber ch))))
          (recur))
        (reset! sim-closed? true)))))

;; To end the simulation, close `world-c` and/or `commands-c`. If only one is
;; closed, the simulation may consume another value from the other before
;; closing.
(defn start
  [steps-c model-atom world-c commands-c]
  (let [options (atom {:go? false
                       :step-ms 20
                       :force-n-steps 0})
        sim-closed? (atom false)]
    (when commands-c
      (handle-commands commands-c model-atom options sim-closed?))
    (simulation-loop model-atom world-c steps-c options sim-closed?))
  nil)
