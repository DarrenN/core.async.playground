(defproject buffers "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2080"]
                 [org.clojure/core.async "0.1.242.0-44b1e3-alpha"]
                 [domina "1.0.2"]
                 [hiccups "0.2.0"]]

  :plugins [[lein-cljsbuild "1.0.0"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "buffers"
              :source-paths ["src"]
              :compiler {
                :output-to "buffers.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}
             {:id "googtest"
              :source-paths ["src"]
              :compiler {
                :output-to "googtest.js"
                :output-dir "gout"
                :optimizations :none
                :source-map true}}]})
