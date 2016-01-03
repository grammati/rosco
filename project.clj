(defproject ca.grammati/rosco "0.1.0-SNAPSHOT"
  :description "Tracing for Clojure"
  :license "MIT License"
  :url "http://github.com/grammati/rosco"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [robert/hooke "1.3.0"]]
  :profiles {:dev {:dependencies [[ring "1.3.1"]
                                  [ring-mock "0.1.5"]]
                   :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5007"]}})
