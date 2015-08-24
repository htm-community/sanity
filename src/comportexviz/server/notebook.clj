(ns comportexviz.server.notebook
  (:require [clojure.core.async :as async]
            [cognitect.transit :as transit]
            [compojure.route :as route]
            [comportexviz.bridge.channel-proxy :as channel-proxy]
            [comportexviz.server.journal :as journal]
            [comportexviz.server.runner :as runner]
            [gorilla-renderable.core :as renderable]
            [gorilla-repl.core :as g]
            [org.nfrac.comportex.protocols :as p])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]))

;; If there are multiple notebooks these are used for all of them.
;; Currently not an issue. This keeps the `viz` API easier.
(def local-targets (channel-proxy/registry))
(def connection-changes-c (async/chan))
(def connection-changes-mult (async/mult connection-changes-c))

(def write-handlers
  (transit/record-write-handlers
   org.nfrac.comportex.topology.OneDTopology
   org.nfrac.comportex.topology.TwoDTopology
   org.nfrac.comportex.topology.ThreeDTopology))

(defn transit-str
  [m]
  (let [out (ByteArrayOutputStream.)
        writer (transit/writer out :json {:handlers write-handlers})]
    (transit/write writer m)
    (.toString out)))

(defmulti viz
  (fn [arg1 & xs]
    (cond (satisfies? p/PHTM arg1) p/PHTM
          (and (sequential? arg1)
               (satisfies? p/PHTM (first arg1))) p/PHTM
          (satisfies? p/PEncodable arg1) p/PEncodable
          (empty? arg1) :empty)))

(defmethod viz :empty
  [arg1 & xs]
  arg1)

(def save-canvases
  "(function(el) {
     return {
       'type': 'html',
       'content': comportexviz.demos.notebook.exported_viz(el)
     };
   })")

(defmethod viz p/PHTM
  [models & [viz-options]]
  (reify
    renderable/Renderable
    (render [_]
      (let [models (if (sequential? models)
                     models
                     [models])
            models-c (async/chan)
            into-j (async/chan)]
        (async/tap connection-changes-mult into-j)
        (journal/init models-c into-j (atom (last models)) -1)
        (async/onto-chan models-c models)
        (let [ij (channel-proxy/register! local-targets into-j)
              ;; No need to send a channel-proxy. The client knows exactly what
              ;; it's receiving -- it's not piping opaque messages to some other
              ;; corner of its code.
              target-id (channel-proxy/target-id ij)]
          {:type :html
           :content ""
           :didMount (format
                      "(function(el) {
                         comportexviz.demos.notebook.add_viz(el, %s);
                       })"
                      (pr-str (transit-str [target-id viz-options])))
           :willUnmount (format
                         "(function(el) {
                            comportexviz.demos.notebook.release_viz(el, %s);
                          })"
                         (pr-str (transit-str target-id)))
           :saveHook save-canvases})))))

(defmethod viz p/PEncodable
  [enc & [input]]
  (reify
    renderable/Renderable
    (render [_]
      (let [topo (p/topology enc)
            state->bits {:active (p/encode enc input)}]
        {:type :html
         :content ""
         :didMount (format
                    "(function(el) {
                       comportexviz.demos.notebook.display_inbits(el, %s);
                     })"
                    (pr-str (transit-str [topo state->bits])))
         :willUnmount "(function(el) {
                         comportexviz.demos.notebook.release_inbits(el);
                       })"
         :saveHook save-canvases}))))

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
