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
    (:defer-var (meta v)) (str "*" (:defer-var (meta v)))
    :else (env/DATATYPE v)))

(defn- variables []
  (let [vars (cond-> @<VARS> (some? @<LASTOUT>) (assoc 'OUTPUT @<LASTOUT>))]
    (sort-by (comp name key) (remove (fn [[_ v]] (or (nil? v) (clojure.core/= "" v) (fn? v))) vars))))

(def ^:private keyword-order
  '[ANCHOR CASE CODE DUMP ERRLIMIT ERRTEXT ERRTYPE FILE FNCLEVEL FTRACE FULLSCAN INPUT LASTFILE LASTLINE LASTNO LINE MAXLNGTH OUTPUT PROFILE RTNTYPE STCOUNT STLIMIT STNO TRACE TRIM])

(defn- keywords []
  (for [k keyword-order]
    (let [v @@(ns-resolve (the-ns 'SNOBOL4clojure.env) (symbol (str "&" k)))]
      (str "&" k " = " (if (string? v) (str "'" v "'") (env/sstr v))))))

(defn- elements [v]
  (cond
    (env/array? v) (for [[k x] (sort-by key @(:data v)) :when (and (some? x) (not= "" x))] (str "<" (subscript-text k) "> = " (value-text x)))
    (env/table? v) (for [[k x] (env/table-entries v) :when (and (some? x) (not= "" x))] (str "<" (subscript-text [k]) "> = " (value-text x)))))

(defn dump-text
  "The text of a dump at level lvl (1 or more): after the last line, SPITBOL writes its newline and three blank lines."
  [lvl]
  (let [vars (variables)
        sec  (fn [header lines] (concat ["" "" header ""] lines))]
    (str (string/join "\n"
           (concat [""]
                   (sec "dump of natural variables" (map (fn [[k v]] (str k " = " (value-text v))) vars))
                   (sec "dump of keyword values" (keywords))
                   (when (>= lvl 2)
                     (mapcat (fn [[k v]]
                               (when (or (env/array? v) (env/table? v))
                                 (concat ["" (value-text v)] (map (fn [e] (str k e)) (elements v)))))
                             vars))))
         "\n\n\n\n")))
