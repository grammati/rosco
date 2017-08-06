(ns rosco.test.ns1)


(defn foo1 []
  (Thread/sleep 10)
  "x")

(defn foo2 []
  (str (foo1) (foo1)))

(defn foo3 []
  (str (foo1) (foo2)))

(defn foo4 []
  (str (foo1) (foo2) (foo3)))
