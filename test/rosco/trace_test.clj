(ns rosco.trace-test
  (:require [rosco.trace :as trace]
            [clojure.test :refer :all]))


(defn foo1 []
  (Thread/sleep 10)
  "x")

(defn foo2 []
  (str (foo1) (foo1)))

(defn foo3 []
  (str (foo1) (foo2)))

(defn foo4 []
  (str (foo1) (foo2) (foo3)))




(deftest test-tracing
  
  (testing "trace-namespace with pattern"
    (try
      (trace/trace-namespace 'rosco.trace-test #"foo.*" )
      (trace/trace-root #'foo4)
      (foo4)
      ;; calls: 1 to foo4, 1 to foo3, 2 to foo2, 6 to foo1 = 10
      ;; enter/leave for each = 20 data entries
      (is (= 20 (-> @trace/traces peek :trace-data count)))
      (trace/print-trace)
      (finally
        (trace/untrace-namespace 'rosco.trace-test)
        (trace/clear-traces!))))
  
  (testing "using trace macro"
    (let [[v trace] (trace/trace (foo4))]
      (is (= "xxxxxx" v))
      ;; NOTE: Only the entry and exit from the trace block are
      ;; recorded, since none of the functions are currently traced.
      (trace/print-trace trace)
      (is (= 2 (count (:trace-data trace))))))
  
  (testing "trace macro inside with-tracing macro"
    (let [[v trace]
          (trace/with-tracing [#'foo1]
            (trace/trace (foo4)))]
      (is (= "xxxxxx" v))
      ;; enter/leave with-tracing = 2, enter/leave trace = 2, plus 6 enter/leave pairs for foo1
      (is (= 16 (count (:trace-data trace))))
      )))


(defn- select-keys+ [m keys]
  (cond
   (map? m)
   (let [keys (set keys)]
     (reduce-kv (fn [m k v]
                  (if (keys k)
                    (assoc m k (select-keys+ v keys))
                    m))
                nil
                (select-keys m keys)))
   
   (sequential? m)
   (mapv #(select-keys+ % keys) m)
   
   :else
   m))

(deftest test->trace-info
  (let [t {:trace-data [{:id 1 :var :a :type :enter :t 0 :depth 1}
                        {:id 2 :var :b :type :enter :t 5 :depth 2}
                        {:id 3 :var :c :type :enter :t 10 :depth 3}
                        {:id 3 :var :c :type :leave :t 20 :depth 3}
                        {:id 4 :var :c :type :enter :t 20 :depth 3}
                        {:id 4 :var :c :type :leave :t 40 :depth 3}
                        {:id 2 :var :b :type :leave :t 95 :depth 2}
                        {:id 1 :var :a :type :leave :t 99 :depth 1}]}]
    (let [tree     (trace/->trace-info t)
          expected [{:var :a
                     :duration 99,
                     :children
                     [{:var :b
                       :duration 90,
                       :children
                       [{:var :c :duration 10 :children [] :id 3 :t 10}
                        {:var :c :duration 20 :children [] :id 4 :t 20}]
                       :id 2
                       :t 5}]
                     :id 1
                     :t 0}]
          actual   (select-keys+ tree [:var :duration :children :id :t])]
      (is (= expected actual)))))


