(defproject org.numenta/sanity "0.0.14-SNAPSHOT"
  :description "Web visualisation of the HTM algorithm"
  :url "https://github.com/nupic-community/sanity"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [org.clojure/core.async "0.2.374"]
                 [tailrecursion/cljs-priority-map "1.1.0"]
                 [org.nfrac/comportex "0.0.14-SNAPSHOT"]
                 [rm-hull/monet "0.2.1"]
                 [reagent "0.5.0"]
                 [reagent-forms "0.5.6"]
                 [ring/ring-core "1.4.0"]
                 [compojure "1.4.0"]
                 [info.sunng/ring-jetty9-adapter "0.9.1"]
                 [com.cognitect/transit-clj "0.8.285"]
                 [com.cognitect/transit-cljs "0.8.232"]
                 [com.mrcslws/gorilla-repl "0.3.5-010"
                  :exclusions [javax.servlet/servlet-api
                               org.slf4j/slf4j-api]]]

  :plugins [[lein-cljsbuild "1.1.1"]
            [com.cemerick/austin "0.1.6"]
            [org.clojure/tools.nrepl "0.2.10"]]
  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                 :init-ns org.numenta.sanity.comportex.launchpad}

  :source-paths ["src"]

  :clean-targets ["public/demos/out"
                  "public/local_inhibition/out"]

  :profiles {:dev {:dependencies [[org.clojure/data.csv "0.1.3"]
                                  [org.clojure/data.codec "0.1.0"]]
                   :cljsbuild {:builds
                               {:demos {:compiler
                                        {:optimizations :none}}}}}
             ;; Use: "lein with-profile +prod [your command]"
             :prod
             {:cljsbuild {:builds
                          {:demos {:compiler
                                   {:optimizations :advanced}}}}}
             :debug
             {;; for Cursive debugging
              :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"]
              }}

  :cljsbuild {:builds
              {:demos {:source-paths ["src" "examples/demos"]
                       :compiler {:output-dir "public/demos/out"
                                  :source-map "public/demos/out/sanity.js.map"
                                  :output-to "public/demos/out/sanity.js"}}
               ;; :inh {:source-paths ["src" "examples/local_inhibition"]
               ;;       :compiler {:optimizations :whitespace
               ;;                  :output-dir "public/local_inhibition/out"
               ;;                  :source-map "public/local_inhibition/out/sanity_inh.js.map"
               ;;                  :output-to "public/local_inhibition/out/sanity_inh.js"}}
               }})
