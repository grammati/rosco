(defproject ca.grammati/rosco "0.1.0-SNAPSHOT"

  :description "Tracing for Clojure"
  :license "MIT License"
  :url "http://github.com/grammati/rosco"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [robert/hooke "1.3.0"]
                 ]

  :plugins [[lein-figwheel "0.5.2"]
            [lein-cljsbuild "1.1.3" :exclusions [[org.clojure/clojure]]]]

  :profiles {:dev   {:dependencies [[ring "1.3.1"]
                                    [ring-mock "0.1.5"]
                                    [org.clojure/clojurescript "1.7.228"]
                                    [reagent "0.5.1"]
                                    [figwheel-sidecar "0.5.2"]
                                    [com.cemerick/piggieback "0.2.1"]]}

             :debug {:jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5007"]}}

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {:builds
              [{:id           "dev"
                :source-paths ["src"]

                :figwheel     {:on-jsload "rosco.ui/on-js-reload"}

                :compiler     {:main                 rosco.ui
                               :asset-path           "js/compiled/out"
                               :output-to            "resources/public/js/compiled/rosco/ui.js"
                               :output-dir           "resources/public/js/compiled/out"
                               :source-map-timestamp true}}

               {:id           "min"
                :source-paths ["src"]
                :compiler     {:output-to     "resources/public/js/compiled/rosco/ui.js"
                               :main          rosco.ui
                               :optimizations :advanced
                               :pretty-print  false}}]})
