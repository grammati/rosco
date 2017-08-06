(ns rosco.trace-test
  (:require [clojure.test :refer :all]
            [rosco.trace :as trace]
            [rosco.test.ns1 :as ns1]))

(deftest test-trace-namespace-with-pattern
  (try
    (trace/trace-namespace 'rosco.test.ns1 #"foo.*")
    (ns1/foo4)
    ;; calls: 1 to foo4, 1 to foo3, 2 to foo2, 6 to foo1 = 10
    ;; enter/leave for each = 20 data entries
    (is (= 20 (-> @trace/traces peek :trace-data count)))
    (trace/print-last-trace)
    (finally
      (trace/untrace-namespace 'rosco.test.ns1)
      (trace/clear-traces!))))

(deftest test-with-tracing-macro
  (let [[v trace] (trace/with-tracing [#"rosco.test.ns1"]
                    (ns1/foo4))]
    (is (= "xxxxxx" v))
    (trace/print-trace trace)
    (is (= 22 (count (:trace-data trace))))))

(deftest trace-macro-inside-with-tracing-macro
  (let [[v trace]
        (trace/with-tracing [#'ns1/foo1]
          (ns1/foo4))]
    (is (= "xxxxxx" v))
    ;; enter/leave with-tracing = 2, plus 6 enter/leave pairs for foo1
    (trace/print-trace trace)
    (is (= 14 (count (:trace-data trace))))))

;; (deftest "with an exception thrown")

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
