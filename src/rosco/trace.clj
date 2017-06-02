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

(def ^:dynamic *verbose*
  "If true, print when vars are traced and untraced."
  nil)

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
    (namespace? x) (-> x .name name)))

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
  [id v args]
  (swap! *trace-data* conj!
         {:id     id
          :var    v
          :args   args
          :type   :enter
          :depth  *trace-depth*
          :t      (System/nanoTime)}))

(defn trace-leave
  "Called when leaving a traced function normally"
  [id v ret]
  (swap! *trace-data* conj!
         {:id    id
          :var   v
          :ret   ret
          :type  :leave
          :depth *trace-depth*
          :t     (System/nanoTime)}))

(defn trace-exception
  "Called when unwinding through a traced function due to an exception"
  [id v e]
  (swap! *trace-data* conj!
         {:id        id
          :var       v
          :exception e
          :type      :exception
          :depth     *trace-depth*
          :t         (System/nanoTime)}))

;;; TODO - would this be faster as a macro? I want tracing to have the
;;; minimum possible impact on runtime (both time and stack-depth)
;;; TODO - do I really need the call-id?
(defn- call-with-tracing [v f args]
  (let [call-id  (str (gensym "call"))]
    (trace-enter call-id v args)
    (try
      (let [ret (binding [*trace-depth* (inc *trace-depth*)]
                  (apply f args))]
        (trace-leave call-id v ret)
        ret)
      (catch Throwable e
        (trace-exception call-id v e)
        (throw e)))))

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
  ([v]
   (wrap-trace-root v (str (gensym "trace"))))
  ([v trace-id]
   (fn [f & args]
     (if (nil? *trace-data*)
       ;; Set up a new trace collection
       (binding [*trace-data* (atom (transient []))]
         (try
           (call-with-tracing v f args)
           (finally
             (swap! traces conj {:trace-id   trace-id
                                 :trace-data (persistent! @*trace-data*)}))))

       ;; Else: a trace collection is already in progress
       (call-with-tracing v f args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defn traced?
  "Returns true if the given var is currently traced."
  [v]
  {:pre [(var? v)]}
  (when-let [hooks (#'robert.hooke/hooks v)]
    (::trace @hooks)))

(defn trace-var
  "Inject tracing into the var."
  [v]
  {:pre [(var? v)]}
  (when *verbose* (println "Tracing" v))
  (add-hook v ::trace (wrap-tracing v)))

(defn trace-root
  "Wrap a var so that calling it begins a trace collection."
  [v]
  {:pre [(var? v)]}
  (when *verbose* (println "Tracing root" v))
  (add-hook v ::trace (wrap-trace-root v)))

(defn untrace-var
  "Remove tracing from a var."
  [v]
  {:pre [(var? v)]}
  (when (and *verbose* (traced? v))
    (println "Untracing" v))
  (remove-hook v ::trace))

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

(defn trace*
  "Calls the function while capturing a trace.
  Returns a 2-tuple of the return-value and the captured trace."
  [f & args]
  (let [trace-id (str (gensym "trace"))
        root-fn  (wrap-trace-root trace-id trace-id)
        ret      (root-fn f)
        ;; Unwrap nested traces
        ret      (if (::trace-result (meta ret))
                   (first ret)
                   ret)]
    ^::trace-result [ret (get-trace trace-id)]))

(defmacro trace
  "Execute the body while capturing a trace.
  Returns a 2-tuple of the return-value and the captured trace."
  [& body]
  `(trace* (fn [] ~@body)))

(defmacro with-tracing
  "Excutes the body while capturing a trace.
  Takes a vector of regexes or vars, indicating what to trace.
  Returns a 2-tupe of the return-value and the captured trace."
  [trace-spec & body]
  `(let [specs# ~trace-spec]
     (try
       (doseq [spec# specs#]
         (trace-dwim spec#))
       (trace ~@body)
       (finally
         (doseq [spec# specs#]
           (untrace-dwim spec#))))))

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
