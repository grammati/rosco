(defproject rosco "0.1.0-SNAPSHOT"
  :description "Clojure stuff"
  :url "http://github.com/grammati/rosco"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [cheshire "5.3.1"]
                 [robert/hooke "1.3.0"]]
  :profiles {:dev {:dependencies [[ring "1.3.1"]
                                  [ring-mock "0.1.5"]]}})
