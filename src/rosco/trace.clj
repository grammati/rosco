(ns rosco.trace
  (:require [clojure.edn :as edn])
  (:import clojure.lang.Namespace
           java.util.concurrent.atomic.AtomicLong))

(def ^:dynamic *trace-depth*
  "Thread-local stack-depth into traced functions"
  0)

(def ^:dynamic *trace-data*
  "Thread-local containing either:
    - an atom containing a sequence of trace-data maps, if a
      trace-collection is in-progress, or
    - nil, if no trace-collection is in progress"
  nil)

(def ^:dynamic *verbose*
  "If true, print when vars are traced and untraced."
  nil)

(defn- dbg [& msgs]
  (when *verbose*
    (apply println msgs)))

(defonce traces
  ;; Atom containing a list of collected traces.
  (atom nil))

;;; Utilities and stuff

(defn- pattern? [x]
  (instance? java.util.regex.Pattern x))

(defn named? [x]
  (instance? clojure.lang.Named x))

(defn namespace? [x]
  (instance? clojure.lang.Namespace x))

(defn- ->name [x]
  (cond
    (named? x)     (name x)
    (var? x)       (-> x meta :name name)
    (namespace? x) (-> ^Namespace x .name name)))

(defn ->pred [f]
  (cond
    (pattern? f) (partial re-matches f)
    :else        f))

(defn ->var [x]
  (if (var? x)
    x
    (ns-resolve *ns* x)))

(defn ->ns
  "DWIM to get a namespace. Takes a string, namespace, or symbol."
  [ns]
  (cond
    (string? ns) (->ns (edn/read-string ns))
    :else        (the-ns ns)))

(defn ns-vars
  "Returns a sequence of the vars in a namespace, optionally fitlered
  by the predicate, which can be a function taking a Var, or a pattern
  to match the Var's name."
  ([ns]
   (ns-vars ns (constantly true)))
  ([ns pred]
   (let [pred (if (pattern? pred)
                #(re-matches pred (->name %))
                pred)]
     (->> ns ->ns ns-interns vals (filter pred)))))

(defn find-namespaces
  "Returns a sequence of loaded namespaces whose names match the given
  predicate or regular expression."
  [pred]
  (let [pred (->pred pred)]
    (filter #(pred (->name %)) (all-ns))))

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
  ([id args]
   (trace-enter id args nil))
  ([id args v]
   (swap! *trace-data* conj!
          {:id     id
           :var    v
           :args   args
           :type   :enter
           :depth  *trace-depth*
           :t      (System/nanoTime)})))

(defn trace-leave
  "Called when leaving a traced function normally"
  ([id ret]
   (trace-leave id ret nil))
  ([id ret v]
   (swap! *trace-data* conj!
          {:id    id
           :var   v
           :ret   ret
           :type  :leave
           :depth *trace-depth*
           :t     (System/nanoTime)})))

(defn trace-exception
  "Called when unwinding through a traced function due to an exception"
  ([id e]
   (trace-exception id e nil))
  ([id e v]
   (swap! *trace-data* conj!
          {:id        id
           :var       v
           :exception e
           :type      :exception
           :depth     *trace-depth*
           :t         (System/nanoTime)})))

(defn original
  "Returns the original, untraced function for the given var"
  [v]
  (::original (meta @v)))

(defn traced?
  "Returns true if the given var is currently traced."
  [v]
  (some? (original v)))

(defonce ^:private ^AtomicLong call-id (AtomicLong.))

(defmacro next-id []
  `(.incrementAndGet call-id))

;;; TODO - would this be faster as a macro? I want tracing to have the
;;; minimum possible impact on runtime (both time and stack-depth)
;;; TODO - do I really need the call-id?
(defn- call-with-tracing
  "Capture trace data around execution of the given function, applied to the
  given args. A trace collection needs to already be in progress on this
  thread."
  [f args v]
  (let [call-id (next-id)]
    (trace-enter call-id args v)
    (try
      (let [ret (binding [*trace-depth* (inc *trace-depth*)]
                  (apply f args))]
        (trace-leave call-id ret v)
        ret)
      (catch Throwable e
        (trace-exception call-id e v)
        (throw e)))))

(defn with-trace-capture
  "Begin capturing a new trace."
  [f args v]
  (let [trace-id (str "trace-" (next-id))]
    (binding [*trace-data* (atom (transient []))]
      (try
        (call-with-tracing f args v)
        (finally
          (swap! traces conj {:trace-id   trace-id
                              :trace-data (persistent! @*trace-data*)}))))))

(defn- wrap-tracing
  "Given a var, return the function in the var wrapped such that calls to it
  will capture tracing data.."
  [v]
  (let [f (or (original v) @v)]
    (with-meta
      (fn [& args]
        (if *trace-data*
          (call-with-tracing f args v)
          (with-trace-capture f args v)))
      {::original f})))

(defn- reset-var-root! [v val]
  (alter-var-root v (constantly val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defn trace-var
  "Inject tracing into the var."
  [v]
  {:pre [(var? v)]}
  (if (traced? v)
    (dbg "Already traced" v)
    (do
      (dbg "Tracing" v)
      (reset-var-root! v (wrap-tracing v)))))

(defn untrace-var
  "Remove tracing from a var."
  [v]
  {:pre [(var? v)]}
  (when (traced? v)
    (dbg "Untracing" v)
    (reset-var-root! v (original v))))

(defn trace-namespace
  "Add tracing to all vars in a namspace (optinally filtered by
  predicate or pattern)"
  ([]
   (trace-namespace *ns*))
  ([ns]
   (trace-namespace ns traceable?))
  ([ns var-pred]
   (doseq [v (ns-vars ns var-pred)]
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
  ([ns-pred]
   (trace-namespaces ns-pred traceable?))
  ([ns-pred var-pred]
   (doseq [ns (find-namespaces ns-pred)]
     (trace-namespace ns var-pred))))

(defn untrace-namespaces
  "Untrace namespaces whose names match the given predicate or pattern"
  ([pred]
   (doseq [ns (find-namespaces pred)]
     (untrace-namespace ns))))

(defn trace-dwim
  "Traces things. The argument specifies what to trace, and can be
  regex, a var, or a qualified symbol."
  [spec]
  (if (pattern? spec)
    (trace-namespaces spec)
    (trace-var (->var spec))))

(defn untrace-dwim
  "Untraces things. See trace-dwim."
  [spec]
  (if (pattern? spec)
    (untrace-namespaces spec)
    (untrace-var (->var spec))))

(defn get-trace
  "Return a captured trace, by id."
  [trace-id]
  (->> @traces
       (filter #(= trace-id (:trace-id %)))
       first))

(defn get-last-trace []
  (peek @traces))

(defn with-tracing* [trace-specs f]
  (try
    (doseq [spec trace-specs]
      (trace-dwim spec))
    (let [ret (with-trace-capture f nil nil)]
      ;; FIXME: race condition here - (get-last-trace) may not return the one we just captured
      [ret (get-last-trace)])
    (finally
      (doseq [spec trace-specs]
        (untrace-dwim spec)))))

(defmacro with-tracing
  "Excutes the body while capturing a trace.
  Takes a vector of regexes or vars, indicating what to trace.
  Returns a 2-tupe of the return-value and the captured trace."
  [trace-specs & body]
  `(with-tracing* ~trace-specs (fn [] ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analysis and display of traces

(defn- trace-data->tree
  ([data]
   (trace-data->tree data []))
  ([[d & data] siblings]
   {:pre [(or (nil? d) (= :enter (:type d)))]}
   (if d
     (let [[children [end & rest]]
           (split-with #(> (:depth %) (:depth d)) data)

           this-call (-> d
                         (assoc
                          :children  (trace-data->tree children)
                          :duration  (- (:t end) (:t d))
                          :exception (:exception end))
                         (dissoc :type))]
       (concat siblings
               [this-call]
               (trace-data->tree rest)))
     siblings)))

(defn ->trace-info
  "Munge a collected trace into a nicer shape."
  [trace]
  (trace-data->tree (:trace-data trace)))

;; (defn- print-call-tree!
;;   "Prints the call tree from a trace."
;;   [[d & data] start-times]
;;   (when d
;;     (println (apply str (repeat (:depth d) "  "))
;;              ({:enter "=>" :leave "<="} (:type d) "??")
;;              (:var d)
;;              (if (= :leave (:type d))
;;                (str (/ (- (:t d) (get start-times (:id d))) 1e6) " ms")
;;                ""))
;;     (recur data (if (= :enter (:type d))
;;                   (assoc start-times (:id d) (:t d))
;;                   start-times))))

(defn- print-call-tree
  "Prints the call tree from a trace."
  [calls]
  (doseq [call calls]
    (let [indent   (apply str (repeat (:depth call) "  "))
          children (:children call)
          name     (str (:var call))
          time     (str (/ (:duration call) 1e6) "ms")]
      (if (empty? children)
        (println indent "==" name time)
        (do
          (println indent "=>" name)
          (print-call-tree children)
          (when-not (:exception call)
            (println indent "<=" name time)))))))

(defn print-trace
  "Prints the given trace."
  [trace]
  (print-call-tree (->trace-info trace)))

(defn print-last-trace
  "Prints the most recently captured trace."
  []
  (print-trace (peek @traces)))

(defn clear-traces! []
  (reset! traces nil))
