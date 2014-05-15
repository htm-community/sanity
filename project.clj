(defproject comportexviz "0.0.1-SNAPSHOT"
  :description "comportex dev viz"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.cemerick/pprng "0.0.2"]
                 [org.clojure/clojurescript "0.0-2197"]
                 [org.nfrac/comportex "0.1.0-SNAPSHOT"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]

                 [rm-hull/monet "0.1.12"]
                 
                 [net.drib/strokes "0.5.1"]
                 ]
  
  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild {:builds [{:id "viz1"
                        :source-paths ["src"]
                        :compiler  {:optimizations :none
                                    :libs [""]
                                    :source-map true
                                    :output-dir "public/out/deps"
                                    :output-to "public/out/viz1.js"}}
                       ]})