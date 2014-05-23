(ns rosco.trace-test
  (:require [rosco.trace :as trace]
            [clojure.test :refer :all]))


(defn foo1 []
  (Thread/sleep 100)
  "x")

(defn foo2 [& [throw?]]
  (if throw?
    (throw (Exception. "arghh!"))
    (str (foo1) (foo1))))

(defn foo3 [& [x]]
  (str (foo1) (foo2 x)))

(defn foo4 [& [x]]
  (str (foo1) (foo2 x) (foo3 x)))

(defn foo5 []
  (str (foo4 false) (foo4 false)))



(deftest test-tracing
  (testing "trace-namespace with pattern"
    (try
      (trace/trace-namespace 'rosco.trace-test #"foo.*" )
      (trace/trace-root #'foo5)
      (foo5)
      (is (= 42 (-> @trace/traces peek :trace-data count)))
      (finally
        (trace/untrace-namespace 'rosco.trace-test)
        (trace/clear-traces!))))
  (testing "using trace macro"
    (let [[v trace] (trace/trace (foo5))]
      (is (= "xxxxxxxxxxxx" v))
      ;; NOTE: Only the entry and exit from the trace block are
      ;; recorded, since none of the functions are currently traced.
      (is (= 2 (count (:trace-data trace))))))
  (testing "trace macro inside with-tracing macro"
   (let [[v trace] (trace/with-tracing [#'foo1]
                     (trace/trace (foo5)))]
     (is (= "xxxxxxxxxxxx" v))
     (is (= 26 (count (:trace-data trace))))
     (trace/print-trace! trace))))



