(defproject comportexviz "0.0.8-SNAPSHOT"
  :description "Web visualisation of HTM algorithm as implemented in comportex"
  :url "https://github.com/nupic-community/comportexviz"

  :dependencies [[org.clojure/clojure "1.7.0-beta2"]
                 [org.clojure/clojurescript "0.0-3211"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.nfrac/comportex "0.0.8-SNAPSHOT"]
                 [rm-hull/monet "0.2.1"]
                 [reagent "0.5.0"]
                 [reagent-forms "0.5.0"]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [com.cemerick/austin "0.1.6"]]
  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :source-paths ["src"]

  :clean-targets ["public/demos/out"
                  "public/cortical_io/out"
                  "public/local_inhibition/out"]

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
