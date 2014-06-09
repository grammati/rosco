(ns rosco.ring
  (:require [rosco.trace :as trace])
  (:import [java.util.concurrent.locks Lock ReadWriteLock ReentrantReadWriteLock]))


(defonce trace-lock (ReentrantReadWriteLock.))

(defmacro with-read-lock [^ReadWriteLock lock & body]
  `(let [lock# (.readLock ~lock)]
     (.lock lock#)
     (try
       ~@body
       (finally
         (.unlock lock#)))))

(defmacro with-write-lock [^ReadWriteLock lock & body]
  `(let [lock# (.writeLock ~lock)]
     (.lock lock#)
     (try
       ~@body
       (finally
         (.unlock lock#)))))


(defn wrap-tracing [handler]
  (fn [request]
    (if (get-in request [:params :trace])
      (with-read-lock trace-lock
        (handler request))
      (with-write-lock trace-lock
        (trace/with-tracing
          (handler request))))))
