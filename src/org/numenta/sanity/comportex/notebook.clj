(ns org.numenta.sanity.comportex.notebook
  (:require [clojure.core.async :as async]
            [cognitect.transit :as transit]
            [compojure.route :as route]
            [org.numenta.sanity.bridge.marshalling :as marshal]
            [org.numenta.sanity.comportex.journal :as journal]
            [org.numenta.sanity.comportex.runner :as runner]
            [gorilla-renderable.core :as renderable]
            [gorilla-repl.core :as g]
            [org.nfrac.comportex.protocols :as p])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]))

;; If there are multiple notebooks these are used for all of them.
;; Currently not an issue. This keeps the `viz` API easier.
(def target->mchannel (atom {}))
(def connection-changes-c (async/chan))
(def connection-changes-mult (async/mult connection-changes-c))

(defn transit-str
  [m]
  (let [out (ByteArrayOutputStream.)
        writer (transit/writer out marshal/encoding)]
    (transit/write writer m)
    (.toString out)))

(defmulti viz
  (fn [arg1 & xs]
    (cond (satisfies? p/PHTM arg1) p/PHTM
          (and (sequential? arg1)
               (satisfies? p/PHTM (first arg1))) p/PHTM
          (satisfies? p/PEncoder arg1) p/PEncoder
          (empty? arg1) :empty)))

(defmethod viz :empty
  [arg1 & xs]
  arg1)

(def save-canvases
  "(function(el) {
     return {
       'type': 'html',
       'content': org.numenta.sanity.demos.notebook.exported_viz(el)
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
            into-journal (async/chan)
            journal-target (java.util.UUID/randomUUID)]
        (swap! target->mchannel assoc journal-target {:ch into-journal})
        (async/tap connection-changes-mult into-journal)
        (journal/init models-c into-journal (atom (last models)) -1)
        (async/onto-chan models-c models)
        {:type :html
         :content ""
         :didMount (format
                    "(function(el) {
                         org.numenta.sanity.demos.notebook.add_viz(el, %s);
                       })"
                    (pr-str (transit-str [journal-target viz-options])))
         :willUnmount (format
                       "(function(el) {
                            org.numenta.sanity.demos.notebook.release_viz(el, %s);
                          })"
                       (pr-str (transit-str journal-target)))
         :saveHook save-canvases}))))

(defmethod viz p/PEncoder
  [enc & [input d-opts]]
  (reify
    renderable/Renderable
    (render [_]
      (let [dims (p/dims-of enc)
            state->bits {:active (p/encode enc input)}
            d-opts (merge {:display-mode :two-d}
                          d-opts)]
        {:type :html
         :content ""
         :didMount (format
                    "(function(el) {
                       org.numenta.sanity.demos.notebook.display_inbits(el, %s);
                     })"
                    (pr-str (transit-str [dims state->bits d-opts])))
         :willUnmount "(function(el) {
                         org.numenta.sanity.demos.notebook.release_inbits(el);
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
        goog.require('org.numenta.sanity.demos.notebook');
      </script>
      <script type='text/javascript'>
        org.numenta.sanity.demos.notebook.connect('%s');
      </script>"
     (script-url "goog/base.js")
     (script-url "sanity.js")
     ws-url)))

(defn start
  [comportex-port]
  (let [server (g/run-gorilla-server {:port 0
                                      :version "0.3.5"
                                      :project "sanity"
                                      :extra-head-html (head-html
                                                        comportex-port)})]
    (:local-port (meta server))))
