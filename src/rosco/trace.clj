(ns rosco.trace
  (:require [clojure.edn :as edn]
            [robert.hooke :refer [add-hook clear-hooks]]))


(def ^:dynamic *trace-depth* 0)

(def trace-data (atom nil))

(defn ->ns
  "DWIM to get a namespace. Takes a string, namespace, or symbol."
  [ns]
  (cond
   (string? ns) (->ns (edn/read-string ns))
   :else        (the-ns ns)))

(defn ns-vars [ns filter-fn]
  (->> ns ->ns ns-interns vals (filter filter-fn)))

(defn find-namespaces [pattern]
  (filter #(re-matches pattern (-> % .name name)) (all-ns)))

(defn traceable? [v]
  (and (var? v)
       (fn? @v)
       (not (:macro (meta v)))
       (not (:no-trace (meta v)))))

(defn trace-start
  "Starts a new trace collection."
  []
  (reset! trace-data []))

(defn- trace-indent []
  (apply str (repeat *trace-depth* "  ")))

(defn trace-enter
  "Called when entering a traced function"
  [id v args]
  (swap! trace-data conj
         {:id    id
          :var   v
          :args  args
          :type  :enter
          :depth *trace-depth*
          :t     (System/nanoTime)}))

(defn trace-leave
  "Called when leaving a traced function"
  [id v ret]
  (swap! trace-data conj
         {:id    id
          :var   v
          :ret   ref
          :type  :leave
          :depth *trace-depth*
          :t     (System/nanoTime)}))

(defn wrap-tracing [v]
  (fn [f & args]
    (let [id (gensym "trace")]
      (when (zero? *trace-depth*)
        (trace-start))
      (trace-enter id v args)
      (let [ret (binding [*trace-depth* (inc *trace-depth*)]
                  (apply f args))]
        (trace-leave id v ret)
        ret))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defn trace-var
  "Inject tracing into the var."
  [v]
  (add-hook v ::trace (wrap-tracing v)))

(defn trace-namespace
  "Given a namespace object, add tracing to all vars that contain a function."
  ([]
     (trace-namespace *ns*))
  ([ns]
     (trace-namespace ns traceable?))
  ([ns filter-fn]
     (doseq [v (ns-vars ns filter-fn)]
       (trace-var v))))

(defn trace-namespaces
  "Traces all loaded namespaces whose names match the given regular expression."
  ([ns-pattern]
     (trace-namespaces ns-pattern traceable?))
  ([ns-pattern var-filter]
     (doseq [ns (find-namespaces ns-pattern)]
       (trace-namespace ns var-filter))))

(defn print-trace!
  "Prints out the last collected trace."
  ([]
     (print-trace! @trace-data {}))
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
