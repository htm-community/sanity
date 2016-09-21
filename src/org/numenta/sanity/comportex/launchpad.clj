(ns org.numenta.sanity.comportex.launchpad
  (:require [clojure.core.async :as async]
            [compojure.route :as route]
            [org.numenta.sanity.comportex.runner :as runner]
            [org.numenta.sanity.comportex.notebook :as notebook]
            [org.numenta.sanity.comportex.websocket :as server-ws]
            [org.nfrac.comportex.core :as cx]))

(defonce runners (atom {}))

(defn random-port []
  (-> (rand-int 30000)
      (+ 10000)))

(defn stop
  [port]
  (runner/stop (get @runners port))
  (swap! runners dissoc port))

(defn stop-all-runners
  []
  (doseq [[_ r] @runners]
    (runner/stop r))
  (swap! runners empty))

(defn start-runner
  ([model inputs]
   (start-runner model inputs nil))
  ([model inputs opts]
   (let [input-c (async/chan)]
     (async/onto-chan input-c inputs)
     (start-runner
      model input-c cx/htm-step nil opts)))
  ([model input-c htm-step models-out-c {:keys [port]}]
   (let [model-atom (if (instance? clojure.lang.Ref model)
                      model
                      (atom model))
         port (or port
                  (random-port))
         runner (runner/start model-atom input-c htm-step models-out-c
                              {:port port
                               :block? false})]
     (swap! runners assoc port runner)
     (println (str "Started server on port " port))
     (println (str "Navigate to http://localhost:" port
                   "/demos/runner.html"))
     runner)))

(defn start-notebook
  []
  (let [comportex-port (random-port)]
    (server-ws/start notebook/target->mchannel notebook/connection-changes-c
                     {:port comportex-port
                      :block? false
                      :http-handler (route/files "/")})
    (let [gorilla-port (notebook/start comportex-port)
          hostname (str "localhost:" gorilla-port)]
      (println
       (format "Navigate to http://%s/worksheet.html?filename=examples/worksheets/hello.clj"
               hostname)))))
