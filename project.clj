(defproject comportexviz "0.0.1-SNAPSHOT"
  :description "Web visualisation of HTM algorithm as implemented in comportex"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2227"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [com.cemerick/pprng "0.0.2"]
                 [org.nfrac/comportex "0.1.0-SNAPSHOT"]
                 [rm-hull/monet "0.1.12"]
                 [com.keminglabs/c2 "0.2.4-SNAPSHOT"]]
  
  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild {:builds [{:id "viz"
                        :source-paths ["src"]
                        :compiler  {:optimizations :none
                                    :libs [""]
                                    :source-map true
                                    :output-dir "public/out/deps"
                                    :output-to "public/out/comportex_viz.js"}}
                       ]})
