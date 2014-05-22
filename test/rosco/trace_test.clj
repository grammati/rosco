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





