(ns comportexviz.server.notebook
  (:require [clojure.core.async :as async]
            [cognitect.transit :as transit]
            [compojure.route :as route]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [comportexviz.server.journal :as journal]
            [comportexviz.server.runner :as runner]
            [gorilla-renderable.core :as renderable]
            [gorilla-repl.core :as g])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]))

;; If there are multiple notebooks this is used for all of them.
;; Currently not an issue. This keeps the `viz` API easier.
(def channel-proxies (channel-proxy/registry))

(def id-count (atom 0))
(defn next-id! []
  (str "ComportexNotebookOutput" (swap! id-count inc)))

;; Elegant.
(defn run-script-on-el [manipulate-el]
  (let [id (next-id!)]
    {:type :html
     :content (str "<div id='" id "'></div>"
                   "<script type='text/javascript'>"
                   "var el = document.getElementById('" id "');"
                   manipulate-el
                   "</script>")}))

(defn transit-str
  [m]
  (let [out (ByteArrayOutputStream.)
        writer (transit/writer out :json)]
    (transit/write writer m)
    (.toString out)))

(defn viz
  [models]
  (reify
    renderable/Renderable
    (render [_]
      (let [models-c (async/chan)
            into-j (async/chan)]
        (journal/init models-c into-j (atom (last models)))
        (async/onto-chan models-c models)
        (let [ij (channel-proxy/from-chan channel-proxies into-j)
              ;; No need to send a channel-proxy. The client knows exactly what
              ;; it's receiving -- it's not piping opaque messages to some other
              ;; corner of its code.
              target-id (channel-proxy/target-id ij)]
          (run-script-on-el
           (format "comportexviz.demos.notebook.add_viz(el,%s);"
                   (pr-str (transit-str target-id)))))))))

(defn head-html
  [comportex-port]
  (let [host (str "localhost:" comportex-port)
        script-url (fn [s]
                     (str "http://" host "/demos/out/" s))
        ws-url (str "ws://" host "/ws/")]
    (format
     "<script type='text/javascript' src='%s'></script>
      <script type='text/javascript' src='%s'></script>
      <script type='text/javascript'>
        goog.require('comportexviz.demos.notebook');
      </script>
      <script type='text/javascript'>
        comportexviz.demos.notebook.connect('%s');
      </script>"
     (script-url "goog/base.js")
     (script-url "comportexviz.js")
      ws-url)))

(defn start
  [comportex-port]
  (let [server (g/run-gorilla-server {:port 0
                                      :version "0.3.5"
                                      :project "comportexviz"
                                      :extra-head-html (head-html
                                                        comportex-port)})]
    (:local-port (meta server))))
