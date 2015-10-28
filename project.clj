(defproject comportexviz "0.0.12-SNAPSHOT"
  :description "Web visualisation of HTM algorithm as implemented in comportex"
  :url "https://github.com/nupic-community/comportexviz"

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.107"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [tailrecursion/cljs-priority-map "1.1.0"]
                 [org.nfrac/comportex "0.0.12-SNAPSHOT"]
                 [rm-hull/monet "0.2.1"]
                 [reagent "0.5.0"]
                 [reagent-forms "0.5.6"]
                 [ring/ring-core "1.4.0"]
                 [compojure "1.4.0"]
                 [info.sunng/ring-jetty9-adapter "0.9.1"]
                 [com.cognitect/transit-clj "0.8.281"]
                 [com.cognitect/transit-cljs "0.8.220"]
                 [com.mrcslws/gorilla-repl "0.3.5-009"
                  :exclusions [javax.servlet/servlet-api
                               org.slf4j/slf4j-api]]]

  :plugins [[lein-cljsbuild "1.1.0"]
            [com.cemerick/austin "0.1.6"]
            [org.clojure/tools.nrepl "0.2.10"]]
  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                 :init-ns comportexviz.server.launchpad}

  :source-paths ["src"]

  :clean-targets ["public/demos/out"
                  "public/cortical_io/out"
                  "public/local_inhibition/out"]

  :profiles {:dev {:dependencies [[org.clojure/data.csv "0.1.3"]
                                  [org.clojure/data.codec "0.1.0"]]}
             :repl
             {;; for Cursive debugging
              ;; :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"]
              }}

  :cljsbuild {:builds [{:id "demos"
                        :source-paths ["src" "examples/demos"]
                        :compiler {:optimizations :whitespace
                                   :output-dir "public/demos/out"
                                   :source-map "public/demos/out/comportexviz.js.map"
                                   :output-to "public/demos/out/comportexviz.js"}}
                       {:id "cortical-io"
                        :source-paths ["src" "examples/cortical_io"]
                        :compiler {:optimizations :whitespace
                                   :output-dir "public/cortical_io/out"
                                   :source-map "public/cortical_io/out/comportexviz_cio.js.map"
                                   :output-to "public/cortical_io/out/comportexviz_cio.js"}}
                       {:id "inh"
                        :source-paths ["src" "examples/local_inhibition"]
                        :compiler {:optimizations :whitespace
                                   :output-dir "public/local_inhibition/out"
                                   :source-map "public/local_inhibition/out/comportexviz_inh.js.map"
                                   :output-to "public/local_inhibition/out/comportexviz_inh.js"}}
                       ]})
