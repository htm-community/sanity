(defproject comportexviz "0.0.6-SNAPSHOT"
  :description "Web visualisation of HTM algorithm as implemented in comportex"
  :url "http://github.com/nupic-community/comportexviz/"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2371"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.nfrac/comportex "0.0.6-SNAPSHOT"]
                 [rm-hull/monet "0.2.1"]
                 [com.keminglabs/c2 "0.2.4-SNAPSHOT"]]
  
  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild {:builds [{:id "comportexviz"
                        :source-paths ["src"]
                        :compiler {:optimizations :advanced
                                   :output-dir "public/out"
                                   :source-map "public/comportexviz.js.map"
                                   :output-to "public/comportexviz.js"}}
                       {:id "cortical-io"
                        :source-paths ["src" "examples/cortical_io"]
                        :compiler {:optimizations :advanced
                                   :output-dir "public/cortical_io/out"
                                   :source-map "public/cortical_io/comportexviz.js.map"
                                   :output-to "public/cortical_io/comportexviz.js"}}
                       ]})
