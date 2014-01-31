(ns chlorine.prelude
  (:require [chlorine.js
             :refer [*temp-sym-count* *macros* tojs' js]]
            [clojure.stacktrace :refer [print-cause-trace]])
  (:import java.util.Date))

(defn now
  "Gets current time in miliseconds"
  [] (.getTime (Date.)))

(defn gen-states
  "Compiles a pre-defined Chlorine strategy, returns compiler's states."
  [strategy]
  (binding [*temp-sym-count* (ref 999)
            *macros*         (ref {})]
    (let [inclusion (eval `(js (load-file
                                ~(str "r:/strategies/" strategy ".cl2"))))]
      {:temp-sym-count @*temp-sym-count*
       :macros @*macros*
       :inclusion inclusion})))

(def ^{:doc "Compiles Chlorine strategies once
  and saves compiler's states to this var."}
  prelude
  (let [strategies ["bare" "prod" "prod-compat" "dev"]]
    (zipmap strategies (map gen-states strategies))))

(defn compile-with-states
  "Compiles expressions using states from prelude."
  [state-name & exprs]
  (let [state (get prelude state-name)]
    (binding [*temp-sym-count*  (ref (:temp-sym-count state))
              *macros*          (ref (:macros state))]
      (str
       (:inclusion state) "\n\n"
       (eval `(js ~@exprs))))))

(defn compile-file-with-states
  "Compiles a file using states from prelude."
  [state-name f]
  (let [state (get prelude state-name)]
    (binding [*temp-sym-count*  (ref (:temp-sym-count state))
              *macros*          (ref (:macros state))]
      (str
       (:inclusion state) "\n\n"
       (tojs' f)))))

(defn exception->msg [e]
  (with-out-str (print-cause-trace e 3)))

(defn exception+->msg [e]
  (with-out-str
    (println "Compilation Error: ")
    (println (:msg e))
    (doseq [i (range (count (:causes e)))
            :let [cause (nth (:causes e) i)]]
      (print (apply str (repeat (inc i) "  ")))
      (println "caused by " cause))
    (when-let [trace (:trace e)]
      (print-cause-trace trace 3))))

(defn msg->alert [msg]
  (->> msg
       pr-str
       (format "alert(%s)")))
