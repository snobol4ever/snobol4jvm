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

(defn value-text
  "A value as a dump shows it, and a trace: an array, a table or a DATA object by its kind and number (ARRAY(2:4) #2, TABLE(11) #4, NODE #6)."
  [v]
  (cond
    (string? v)  (str "'" v "'")
    (integer? v) (str v)
    (double? v)  (env/sstr v)
    (instance? NAME v)    (name-text v)
    ;; an array by its prototype as ARRAY was given it (MEASURED on sbl -bf: ARRAY('1:3') dumps ARRAY(1:3), ARRAY(' 2 : 4 , 3 ') ARRAY( 2 : 4 , 3 ), ARRAY(2.9)
    ;; ARRAY(2); corpus gate_cases_snobol4 dump_matches_spitbol__dump2b)
    (env/array? v)        (str "ARRAY(" (or (:proto v) (dims-text (:dims v))) ") #" (:serial v))
    (env/table? v)        (str "TABLE(" (:size (meta v) 11) ") #" (:serial (meta v)))
    (instance? DataObj v) (str (.type-name ^DataObj v) " #" (.serial ^DataObj v))
    (symbol? (:defer-var (meta v))) (str "*" (:defer-var (meta v)))   ; *NAME for a variable, EXPRESSION for anything larger
    :else (env/DATATYPE v)))

;; The seven pattern variables SPITBOL predefines are listed when the program's text names one, bare or called, whether or not the statement runs (MEASURED on
;; sbl -bf: "Y = BAL ARB REM FAIL FENCE ABORT SUCCEED" skipped by a goto lists all seven = PATTERN, and Z = FENCE('a') lists FENCE; ARBNO is none; corpus
;; decus_vax84a diag1, whose BAL lines never run, where this engine listed only the ones a run read)
(def ^:private pattern-variables '#{ABORT ARB BAL FAIL FENCE REM SUCCEED})
(defn- named-patterns []
  (into {} (for [n @env/<NAMES> :let [s (symbol n)] :when (pattern-variables s)]
             [s (if (clojure.core/= s 'FENCE) @(ns-resolve (the-ns 'SNOBOL4clojure.operators) 'bare-fence) (env/primitive-pattern s))])))

(defn- variables [lvl]
  (let [vars (cond-> (merge (named-patterns) (into {} (for [s @env/<PRIMS-READ>] [s (deref (ns-resolve (the-ns 'SNOBOL4clojure.core) s))])) @<VARS>)
               true (merge @env/<ASSOC-LAST>))]   ; an associated variable holds the last record read or written through it
    (if (>= (long lvl) 3)
      ;; &DUMP = 3 "adds statement labels and all null-valued variables" (manual ch. 7): every name the program spoke, null ones as '' (MEASURED on sbl -bf: corpus
      ;; setl4_spitbol sbl-sonnets, &DUMP = 3, lists ANCHOR = '', BREAK = '', END = '', RETURN = '', a DEFINE's locals and a DATA's fields, where this engine
      ;; listed the non-null ones alone)
      (sort-by (comp name key)
               ;; INPUT, OUTPUT and TERMINAL are SPITBOL's own, there in every program (MEASURED: "        &DUMP = 3" / "        X = 1" / "END" lists them)
               (for [[k v] (merge (into {} (for [n (into @env/<NAMES> ["INPUT" "OUTPUT" "TERMINAL"])] [(symbol n) ""])) vars)]
                 (clojure.lang.MapEntry/create k (if (or (nil? v) (fn? v)) "" v))))
      (sort-by (comp name key) (remove (fn [[_ v]] (or (nil? v) (clojure.core/= "" v) (fn? v))) vars)))))

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

;; SPITBOL's hash of a table key, the bucket's order in a dump (sbl.min: tfind and hashs; a dump walks the buckets in order and each chain in the order entries were
;; made). A string: its length, exclusive-or'd with its first three 8-byte words of characters (little-endian, zero past the end), kept to 63 bits. An integer: its
;; magnitude; a real: the magnitude of its bit pattern; the most negative of either hashes to 0. Another key has no hash here, and its entries come last in their order.
(defn- key-hash [k]
  (let [mag (fn [^long x] (cond (clojure.core/= x Long/MIN_VALUE) 0 (neg? x) (- x) :else x))]
    (cond
      (string? k)  (let [b ^bytes (.getBytes ^String k "ISO-8859-1") n (alength b)]
                     (if (zero? n)
                       0
                       (bit-and Long/MAX_VALUE
                                (reduce (fn [h w] (bit-xor h (reduce (fn [acc j] (let [i (+ (* 8 w) j)]
                                                                                   (if (< i n) (bit-or acc (bit-shift-left (bit-and 0xff (long (aget b i))) (* 8 j))) acc)))
                                                                     0 (range 8))))
                                        (long n) (range (min 3 (quot (+ n 7) 8)))))))
      (integer? k) (mag (long k))
      (double? k)  (mag (Double/doubleToRawLongBits k))
      :else nil)))

(defn- parts [v]   ; [[kind label value] ...] of an object, in order
  (cond
    (env/array? v)        (for [[k x] (sort-by key @(:data v))] [:sub k x])
    (env/table? v)        (let [n (long (:size (meta v) 11))]
                            (for [[k x] (sort-by (fn [[k _]] (if-some [h (key-hash k)] (mod h n) Long/MAX_VALUE)) (env/table-entries v))] [:sub [k] x]))
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
  (let [vars (variables lvl)
        sec  (fn [header lines] (concat ["" "" header ""] lines))]
    (str (string/join "\n"
           (concat [""]
                   (sec "dump of natural variables" (map (fn [[k v]] (str k " = " (value-text v))) vars))
                   (sec "dump of keyword values" (keywords))
                   (when (>= lvl 2) (mapcat #(object-lines vars %) (objects vars)))))
         "\n\n\n\n")))
