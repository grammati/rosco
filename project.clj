(defproject ca.grammati/rosco "0.1.0-SNAPSHOT"

  :description "Tracing for Clojure"
  :license "MIT License"
  :url "http://github.com/grammati/rosco"
  :min-lein-version "2.5.3"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [robert/hooke "1.3.0"]]

  :figwheel {:css-dirs     ["resources/public/css"]
             :ring-handler rosco.handler/dev-handler}

  :garden {:builds [{:id           "screen"
                     :source-paths ["src/clj"]
                     :stylesheet   rosco.css/screen
                     :compiler     {:output-to     "resources/public/css/screen.css"
                                    :pretty-print? true}}]}

  :profiles
  {:cljs {:dependencies [[org.clojure/clojurescript "1.9.229"]
                         [reagent "0.6.0"]
                         [re-frame "0.9.2"]
                         [re-frisk "0.3.2"]
                         [secretary "1.2.3"]
                         [garden "1.3.2"]

                         [figwheel-sidecar "0.5.10"]
                         [com.cemerick/piggieback "0.2.1"]
                         [binaryage/devtools "0.8.2"]
                         [figwheel-sidecar "0.5.9"]]
          :plugins      [[lein-cljsbuild "1.1.4"]
                         [lein-figwheel "0.5.10"]
                         [lein-doo "0.1.7"]
                         [lein-garden "0.2.8"]]
          :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
          :prep-tasks   [["cljsbuild" "once" "min"]["garden" "once"] "compile"]}

   :dev [:cljs
         {:dependencies [[ring "1.6.1"]
                         [ring-mock "0.1.5"]]}]

   :debug {:jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5007"]}}

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild
  {:builds
   [{:id           "dev"
     :source-paths ["src/cljs"]
     :figwheel     {:on-jsload "rosco.ui/mount-root"}
     :compiler     {:main                 rosco.ui
                    :output-to            "resources/public/js/compiled/ui.js"
                    :output-dir           "resources/public/js/compiled/out"
                    :asset-path           "js/compiled/out"
                    :source-map-timestamp true
                    :preloads             [devtools.preload]
                    :external-config      {:devtools/config {:features-to-install :all}}
                    }}

    {:id           "min"
     :source-paths ["src/cljs"]
     :jar          true
     :compiler     {:main            rosco.ui
                    :output-to       "resources/public/js/compiled/ui.js"
                    :optimizations   :advanced
                    :closure-defines {goog.DEBUG false}
                    :pretty-print    false}}

    {:id           "test"
     :source-paths ["src/cljs" "test/cljs"]
     :compiler     {:main          rosco.runner
                    :output-to     "resources/public/js/compiled/test.js"
                    :output-dir    "resources/public/js/compiled/test/out"
                    :optimizations :none}}
    ]})
