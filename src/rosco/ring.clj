(ns rosco.ring
  (:require [rosco.trace :as trace]
            [cheshire.core :as json])
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


(defn wrap-tracing
  "Ring middleware to cature a trace of the request.

  Does not inject tracing into any vars, but just initiates a trace
  capture. Only vars that already have tracing injected will be
  traced.

  Only captures a trace if the should-trace? function returns
  true. Defaults to (get-in request [:params :trace]).

  Adds a header, x-rosco-trace, to the response, containing the id of
  the captured trace."
  ([handler]
     (wrap-tracing handler #(get-in % [:params "trace"])))
  ([handler should-trace?]
     (fn [request]
       (if (should-trace? request)
         (let [[response trace] (trace/trace
                                 (handler request))]
           (assoc-in response [:headers "x-rosco-trace"] (:trace-id trace)))
         (handler request)))))

(defn wrap-exclusive-tracing
  "Ring middleware to inject tracing into vars, capture a trace of the
  request handler, and remove the tracing. Locks out all other
  requests while capturing the trace.

  Takes the next handler and a vector of regular expressions to be
  passed to trace-namespaces. Will lock out all other requests while
  capturing the trace.

  Only captures a trace if the should-trace? function returns
  true. Defaults to (get-in request [:params :trace]).

  Adds a header, x-rosco-trace, to the response, containing the id of
  the captured trace."
  ([handler regexes]
     (wrap-exclusive-tracing handler regexes #(get-in % [:params "trace"])))
  
  ([handler regexes should-trace?]
     
     (assert (vector? regexes))
     (assert (pos? (count regexes)))
     (assert (every? #(or (#'trace/pattern? %) (var? %)) regexes))
     
     (fn [request]
       (if (should-trace? request)
         (with-write-lock trace-lock
           (let [[response trace] (trace/with-tracing regexes
                                    (handler request))]
             (assoc-in response [:headers "x-rosco-trace"] (:trace-id trace))))
         (with-read-lock trace-lock
           (handler request))))))


(defn get-trace
  "Ring handler to return a JSON representation of a previously
  captured trace."
  [request]
  (let [trace-id (get-in request [:params "trace-id"])
        trace    (trace/get-trace trace-id)]
    {:status  (if trace 200 404)
     :headers {"content-type" "application/json"}
     :body    (json/encode
               (or trace {:error "trace not found"}))}))

