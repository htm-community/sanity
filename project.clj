(defproject comportexviz "0.0.4-SNAPSHOT"
  :description "Web visualisation of HTM algorithm as implemented in comportex"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2356"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [com.cemerick/pprng "0.0.2"]
                 [org.nfrac/comportex "0.0.4-SNAPSHOT"]
                 [rm-hull/monet "0.2.1"]
                 [com.keminglabs/c2 "0.2.4-SNAPSHOT"]]
  
  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild {:builds [{:id "viz"
                        :source-paths ["src"]
                        :compiler {:optimizations :advanced
                                   :output-dir "public/out"
                                   :source-map "public/comportexviz.js.map"
                                   :output-to "public/comportexviz.js"}}
                       ]})
