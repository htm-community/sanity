(defproject org.numenta/sanity "0.0.16-SNAPSHOT"
  :description "Web visualisation of the HTM algorithm"
  :url "https://github.com/htm-community/sanity"

  :dependencies [[org.clojure/clojure "1.9.0-alpha12"]
                 [org.clojure/clojurescript "1.9.229"]
                 [org.clojure/core.async "0.2.391"]
                 [tailrecursion/cljs-priority-map "1.2.0"]
                 [org.nfrac/comportex "0.0.16-SNAPSHOT"]
                 [rm-hull/monet "0.3.0"]
                 [reagent "0.5.1"]
                 [reagent-forms "0.5.25"]
                 [ring/ring-core "1.5.0"]
                 [compojure "1.5.1"]
                 [info.sunng/ring-jetty9-adapter "0.9.5"]
                 [com.cognitect/transit-clj "0.8.288"]
                 [com.cognitect/transit-cljs "0.8.239"]
                 [com.mrcslws/gorilla-repl "0.3.5-010"
                  :exclusions [javax.servlet/servlet-api
                               org.slf4j/slf4j-api]]]

  :plugins [[lein-cljsbuild "1.1.4"]
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
                                        {:optimizations :none
                                         :source-map true}}}}}
             ;; Use: "lein with-profile +prod [your command]"
             :prod
             {:cljsbuild {:builds
                          {:demos {:compiler
                                   {:optimizations :advanced
                                    :source-map "public/demos/out/sanity.js.map"}}}}}
             :debug
             {;; for Cursive debugging
              :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"]}}


  :cljsbuild {:builds
              {:demos {:source-paths ["src" "examples/demos"]
                       :compiler {:output-dir "public/demos/out"
                                  :output-to "public/demos/out/sanity.js"}}}})
               ;; :inh {:source-paths ["src" "examples/local_inhibition"]
               ;;       :compiler {:optimizations :whitespace
               ;;                  :output-dir "public/local_inhibition/out"
               ;;                  :source-map "public/local_inhibition/out/sanity_inh.js.map"
               ;;                  :output-to "public/local_inhibition/out/sanity_inh.js"}}})
