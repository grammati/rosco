(ns rosco.trace
  (:require [clojure.edn :as edn]
            [robert.hooke :refer [add-hook remove-hook]]))


(def ^:dynamic *trace-depth*
  "Thread-local stack-depth into traced functions"
  0)

(def ^:dynamic *trace-data*
  "Thread-local containing either:
    - an atom containing a sequence of trace-data maps, if a
      trace-collection is in-progress, or
    - nil, if no trace-collection is in progress"
  nil)

(def traces
  "Atom containing collected traces."
  (atom []))


;;; Utilities and stuff

(defn ->ns
  "DWIM to get a namespace. Takes a string, namespace, or symbol."
  [ns]
  (cond
   (string? ns) (->ns (edn/read-string ns))
   :else        (the-ns ns)))

(defn ns-vars
  "Returns a sequence of the vars in a namespace."
  ([ns]
     (ns-vars ns (constantly true)))
  ([ns filter-fn]
     (->> ns ->ns ns-interns vals (filter filter-fn))))

(defn find-namespaces
  "Returns a sequence of loaded namespaces whose names match the given
  regular expression."
  [pattern]
  (filter #(re-matches pattern (-> % .name name)) (all-ns)))

(defn traceable?
  "This is the default predictae for determining whether to add
  tracing to a var when calling `trace-namespace`."
  [v]
  (and (var? v)
       (fn? @v)
       (not (:macro (meta v)))
       (not (:no-trace (meta v)))))


(defn trace-enter
  "Called when entering a traced function"
  [id v args]
  (swap! *trace-data* conj!
         {:id    id
          :var   v
          :args  args
          :type  :enter
          :depth *trace-depth*
          :t     (System/nanoTime)}))

(defn trace-leave
  "Called when leaving a traced function"
  [id v ret]
  (swap! *trace-data* conj!
         {:id    id
          :var   v
          :ret   ref
          :type  :leave
          :depth *trace-depth*
          :t     (System/nanoTime)}))


(defn- call-with-tracing [v f args]
  (let [call-id  (gensym "call")]
    (trace-enter call-id v args)
    (let [ret (binding [*trace-depth* (inc *trace-depth*)]
                (apply f args))]
      (trace-leave call-id v ret)
      ret)))

(defn wrap-tracing
  "Wrap a var such that a call to it will collect trace data if and
  only if a trace is already in-progress."
  [v]
  (fn [f & args]
    (if *trace-data*
      (call-with-tracing v f args)
      (apply f args))))

(defn wrap-trace-root
  "Wrap a var such that calling it begins a trace collection, if one
  is not already in progress."
  [v]
  (fn [f & args]
    (if (nil? *trace-data*)
      
      ;; Set up a new trace collection
      (let [trace-id (gensym "trace")]
        (binding [*trace-data* (atom (transient []))]
          (try
            (call-with-tracing v f args)
            (finally
              (swap! traces conj {:trace-id   trace-id
                                  :trace-data (persistent! @*trace-data*)
                                  :thread     (Thread/currentThread)})))))

      ;; Else: a trace collection is already in progress
      (call-with-tracing v f args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defn trace-var
  "Inject tracing into the var."
  [v]
  (println "Tracing" v)
  (add-hook v ::trace (wrap-tracing v)))

(defn untrace-var
  "Remove tracing from a var."
  [v]
  (println "Untracing" v)
  (remove-hook v ::trace))

(defn trace-root
  "Wrap a var so that calling it begins a trace collection."
  [v]
  (add-hook v ::trace (wrap-trace-root v)))

(defn trace-namespace
  "Add tracing to all vars in a namspace."
  ([]
     (trace-namespace *ns*))
  ([ns]
     (trace-namespace ns traceable?))
  ([ns filter-fn]
     (doseq [v (ns-vars ns filter-fn)]
       (trace-var v))))

(defn untrace-namespace
  "Remove tracing from all vars in a namespace."
  ([]
     (untrace-namespace *ns*))
  ([ns]
     (doseq [v (ns-vars ns)]
       (untrace-var v))))

(defn trace-namespaces
  "Traces all loaded namespaces whose names match the given regular expression."
  ([ns-pattern]
     (trace-namespaces ns-pattern traceable?))
  ([ns-pattern var-filter]
     (doseq [ns (find-namespaces ns-pattern)]
       (trace-namespace ns var-filter))))

(defn untrace-namespaces
  "Untrace namespaces whose names match the given pattern."
  ([ns-pattern]
     (doseq [ns (find-namespaces ns-pattern)]
       (untrace-namespace ns))))


(defn print-trace!
  "Prints out the last collected trace."
  ([]
     (print-trace! (:trace-data (last @traces)) {}))
  ([[d & data] start-times]
     (when d
       (println (apply str (repeat (:depth d) "  "))
                ({:enter "=>" :leave "<="} (:type d) "??")
                (:var d)
                (when (= :leave (:type d))
                  (str (/ (- (:t d) (get start-times (:id d))) 1e6) " ms")))
       (recur data (if (= :enter (:type d))
                     (assoc start-times (:id d) (:t d))
                     start-times)))))

(defn clear-traces! []
  (reset! traces []))
