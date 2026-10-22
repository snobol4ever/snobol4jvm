(ns SNOBOL4clojure.dump
  "The dump a program asks for with &DUMP: at its end, the natural variables it holds and then the keywords, in SPITBOL's layout (measured against sbl -bf).
   &DUMP = 1 lists the variables and the keywords; 2 also lists the elements of the arrays and tables the variables hold."
  (:require [clojure.string :as string]
            [SNOBOL4clojure.env :as env :refer [<VARS> <LASTOUT>]])
  (:import  [SNOBOL4clojure.env NAME DataObj]))

(defn- subscript-text [ks] (string/join "," (map (fn [k] (if (string? k) (str "'" k "'") (env/sstr k))) ks)))

(defn- name-text [^NAME v]
  (let [n (.n v)]
    (if (and (seq? n) (next n))
      (str "." (first n) "<" (subscript-text (rest n)) ">")
      (str "." n))))

(defn- dims-text [dims] (string/join "," (map (fn [[lo hi]] (if (clojure.core/= lo 1) (str hi) (str lo ":" hi))) dims)))

(defn- value-text [v]
  (cond
    (string? v)  (str "'" v "'")
    (integer? v) (str v)
    (double? v)  (env/sstr v)
    (instance? NAME v)    (name-text v)
    (env/array? v)        (str "ARRAY(" (dims-text (:dims v)) ") #" (:serial v))
    (env/table? v)        (str "TABLE(" (:size (meta v) 11) ") #" (:serial (meta v)))
    (instance? DataObj v) (str (.type-name ^DataObj v) " #" (.serial ^DataObj v))
    (symbol? (:defer-var (meta v))) (str "*" (:defer-var (meta v)))   ; *NAME for a variable, EXPRESSION for anything larger
    :else (env/DATATYPE v)))

(defn- variables []
  (let [vars (cond-> (merge (into {} (for [s @env/<PRIMS-READ>] [s (deref (ns-resolve (the-ns 'SNOBOL4clojure.core) s))])) @<VARS>)
               true (merge @env/<ASSOC-LAST>))]   ; an associated variable holds the last record read or written through it
    (sort-by (comp name key) (remove (fn [[_ v]] (or (nil? v) (clojure.core/= "" v) (fn? v))) vars))))

(def ^:private keyword-order
  '[ANCHOR CASE CODE DUMP ERRLIMIT ERRTEXT ERRTYPE FILE FNCLEVEL FTRACE FULLSCAN INPUT LASTFILE LASTLINE LASTNO LINE MAXLNGTH OUTPUT PROFILE RTNTYPE STCOUNT STLIMIT STNO TRACE TRIM])

(defn- keywords []
  (for [k keyword-order]
    (let [v @@(ns-resolve (the-ns 'SNOBOL4clojure.env) (symbol (str "&" k)))]
      (str "&" k " = " (if (string? v) (str "'" v "'") (env/sstr v))))))

;; The objects a dump at level 2 lists: every array, table and DATA object the variables reach, through elements and fields too, in the order they were made (their
;; numbers), each named by the variable that holds it, or by its own display (NODE #2) when none does. Its lines are holder<subscripts> = value for an element and
;; field(holder) = value for a field, the null ones left out (manual ch. 19 DUMP: "non-null array and table elements, and non-null field values of program-defined data
;; types"), as sbl -bf writes them.
(defn- serial-of [v] (cond (env/array? v) (:serial v) (env/table? v) (:serial (meta v)) (instance? DataObj v) (.serial ^DataObj v)))

(defn- parts [v]   ; [[kind label value] ...] of an object, in order
  (cond
    (env/array? v)        (for [[k x] (sort-by key @(:data v))] [:sub k x])
    (env/table? v)        (for [[k x] (env/table-entries v)] [:sub [k] x])
    (instance? DataObj v) (map (fn [f x] [:field f x]) (.field-names ^DataObj v) (seq (.fields ^DataObj v)))))

(defn- objects [vars]
  (loop [todo (map val vars) seen (java.util.IdentityHashMap.) out []]
    (if-let [[v & more] (seq todo)]
      (if (and (serial-of v) (not (.containsKey seen v)))
        (do (.put seen v true) (recur (concat more (map #(nth % 2) (parts v))) seen (conj out v)))
        (recur more seen out))
      (sort-by serial-of out))))

(defn- object-lines [vars v]
  (let [holder (or (some (fn [[k x]] (when (identical? x v) (str k))) vars) (value-text v))]
    (concat ["" (value-text v)]
            (for [[kind label x] (parts v) :when (and (some? x) (not= "" x))]
              (case kind
                :sub   (str holder "<" (subscript-text label) "> = " (value-text x))
                :field (str label "(" holder ") = " (value-text x)))))))

(defn dump-text
  "The text of a dump at level lvl (1 or more): after the last line, SPITBOL writes its newline and three blank lines."
  [lvl]
  (let [vars (variables)
        sec  (fn [header lines] (concat ["" "" header ""] lines))]
    (str (string/join "\n"
           (concat [""]
                   (sec "dump of natural variables" (map (fn [[k v]] (str k " = " (value-text v))) vars))
                   (sec "dump of keyword values" (keywords))
                   (when (>= lvl 2) (mapcat #(object-lines vars %) (objects vars)))))
         "\n\n\n\n")))
