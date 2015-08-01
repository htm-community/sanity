(ns comportexviz.server.launchpad
  (:require [clojure.core.async :as async]
            [comportexviz.server.runner :as runner]))

(def runners (atom {}))
(def port-choice (atom 24600))

(defn stop
  [port]
  (runner/stop (get @runners port))
  (swap! runners dissoc port))

(defn start-runner
  ([model inputs]
   (start-runner model inputs nil))
  ([model inputs opts]
   (let [input-c (async/chan)]
     (async/onto-chan input-c inputs)
     (start-runner model input-c nil opts)))
  ([model input-c models-out-c {:keys [port]}]
   (let [model-atom (if (instance? clojure.lang.Ref model)
                      model
                      (atom model))
         port (or port
                  (swap! port-choice inc))
         runner (runner/start model-atom input-c models-out-c {:port port
                                                               :block? false})]
     (swap! runners assoc port runner)
     (println (str "Started server on port " port))
     (println (str "Navigate to http://localhost:" port
                   "/demos/runner.html")))))
