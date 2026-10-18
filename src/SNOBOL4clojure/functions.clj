(ns SNOBOL4clojure.functions
  ;; Built-in SNOBOL4 functions.
  ;; Stubs are marked clearly; implemented ones are complete.
  (:require [SNOBOL4clojure.env :as env :refer [ε equal subtract sstr
                                        DATATYPE table? array? table-get table-set
                                        ARRAY ->SnobolArray array-get array-set
                                        <CHANNELS>]])
  (:refer-clojure :exclude [= + - * / num]))

;; ── String functions ──────────────────────────────────────────────────────────
(defn SIZE    [s]         (count (str s)))
(defn REPLACE [s1 s2 s3]
  ;; Replace chars in s1: each char of s2 mapped to corresponding char of s3
  (apply str (map #(let [i (.indexOf ^String s2 (str %))]
                     (if (>= i 0) (.charAt ^String s3 i) %))
                  s1)))
(defn DUPL    [x i]   (apply str (repeat i x)))  ; string only for now
(defn TRIM    [s]     (clojure.string/trimr (str s)))
(defn REVERSE [s]     (apply str (clojure.string/reverse s)))
(defn LPAD
  ([s n]        (LPAD s n " "))
  ([s n fill]   (let [s-str  (str s)
                      pad    (clojure.core/- (long n) (count s-str))
                      fill-c (str (first (str fill)))]
                  (if (clojure.core/<= pad 0) s-str
                    (str (apply str (repeat pad fill-c)) s-str)))))
(defn RPAD
  ([s n]        (RPAD s n " "))
  ([s n fill]   (let [s-str  (str s)
                      pad    (clojure.core/- (long n) (count s-str))
                      fill-c (str (first (str fill)))]
                  (if (clojure.core/<= pad 0) s-str
                    (str s-str (apply str (repeat pad fill-c)))))))
(defn SUBSTR  [s i j] (subs (str s) (dec (long i)) (clojure.core/+ (dec (long i)) (long j))))
(defn CONVERT [x t]
  "CONVERT(x, type) — coerce x to the named SNOBOL4 type.
   Returns nil (failure) if the conversion is not defined."
  (let [from (clojure.string/upper-case (DATATYPE x))
        to   (clojure.string/upper-case (str t))]
    (cond
      ;; Same type → identity (but always a 'new' value logically)
      (clojure.core/= from to) x

      ;; STRING conversions
      (and (clojure.core/= from "STRING") (clojure.core/= to "INTEGER"))
      (let [v (try (Long/parseLong (clojure.string/trim (str x)))
                   (catch Exception _ nil))]
        v)   ; nil propagates as failure

      (and (clojure.core/= from "STRING") (clojure.core/= to "REAL"))
      (let [v (try (Double/parseDouble (clojure.string/trim (str x)))
                   (catch Exception _ nil))]
        v)

      (and (clojure.core/= from "STRING") (clojure.core/= to "PATTERN"))
      ;; A string converts to a literal LIT$ pattern node
      (list 'SEQ (list 'LIT$ (str x)))

      (and (clojure.core/= from "STRING") (clojure.core/= to "NAME"))
      ;; A string becomes a NAME (symbol pointing to a variable of that name)
      (symbol (str x))

      ;; INTEGER conversions
      (and (clojure.core/= from "INTEGER") (clojure.core/= to "STRING"))
      (str x)

      (and (clojure.core/= from "INTEGER") (clojure.core/= to "REAL"))
      (double x)

      (and (clojure.core/= from "INTEGER") (clojure.core/= to "PATTERN"))
      (list 'SEQ (list 'LIT$ (str x)))

      (and (clojure.core/= from "INTEGER") (clojure.core/= to "NAME"))
      (symbol (str x))

      ;; REAL conversions
      (and (clojure.core/= from "REAL") (clojure.core/= to "STRING"))
      (str x)

      (and (clojure.core/= from "REAL") (clojure.core/= to "INTEGER"))
      ;; Fails if value doesn't fit in a long
      (let [v (try (let [lv (long x)]
                     (when (clojure.core/<= (double Long/MIN_VALUE) x (double Long/MAX_VALUE))
                       lv))
                   (catch Exception _ nil))]
        v)

      (and (clojure.core/= from "REAL") (clojure.core/= to "PATTERN"))
      (list 'SEQ (list 'LIT$ (str x)))

      (and (clojure.core/= from "REAL") (clojure.core/= to "NAME"))
      (symbol (str x))

      ;; ARRAY conversions
      (and (clojure.core/= from "ARRAY") (clojure.core/= to "TABLE"))
      ;; ARRAY must be Nx2; column 1 = key, column 2 = value.
      ;; Only succeeds if exactly 2 dimensions and second dim size is 2.
      (let [dims (:dims x)]
        (if (and (clojure.core/= (count dims) 2)
                 (clojure.core/= (second (second dims)) 2)
                 (clojure.core/= (first (second dims)) 1))
          (let [t (atom {})
                [lo1 hi1] (first dims)]
            (doseq [i (range lo1 (inc hi1))]
              (let [k (array-get x [i 1])
                    v (array-get x [i 2])]
                (when-not (nil? k)
                  (swap! t assoc k (or v ε)))))
            t)
          nil))  ; wrong shape → fail

      ;; TABLE conversions
      (and (clojure.core/= from "TABLE") (clojure.core/= to "ARRAY"))
      ;; Convert TABLE to Nx2 ARRAY sorted by key string representation
      (let [entries (sort-by #(str (first %)) (seq @x))
            n       (count entries)
            arr     (ARRAY (str n ",2"))]
        (doseq [[i [k v]] (map-indexed vector entries)]
          (array-set arr [(inc i) 1] k)
          (array-set arr [(inc i) 2] v))
        arr)

      ;; PATTERN stays PATTERN
      (and (clojure.core/= from "PATTERN") (clojure.core/= to "PATTERN")) x

      ;; NAME conversions (NAME is a symbol in our representation)
      (and (clojure.core/= from "NAME") (clojure.core/= to "STRING"))
      (str x)

      (and (clojure.core/= from "NAME") (clojure.core/= to "INTEGER"))
      (let [v (try (Long/parseLong (str x)) (catch Exception _ nil))] v)

      (and (clojure.core/= from "NAME") (clojure.core/= to "REAL"))
      (let [v (try (Double/parseDouble (str x)) (catch Exception _ nil))] v)

      (and (clojure.core/= from "NAME") (clojure.core/= to "PATTERN"))
      (list 'SEQ (list 'LIT$ (str x)))

      (and (clojure.core/= from "NAME") (clojure.core/= to "NAME")) x

      ;; EXPRESSION stays EXPRESSION
      (and (clojure.core/= from "EXPRESSION") (clojure.core/= to "EXPRESSION")) x

      ;; CODE stays CODE
      (and (clojure.core/= from "CODE") (clojure.core/= to "CODE")) x

      ;; All other combinations fail
      :else nil)))
(defn COPY    [x]
  (cond
    (table? x) (atom @x)
    (array? x) (SNOBOL4clojure.env/->SnobolArray
                  (:dims x) (:dflt x) (atom @(:data x)))
    :else x))

;; ── Type conversion & char functions ─────────────────────────────────────────
(defn ASCII   [s]  (int (first (str s))))
(defn CHAR    [n]  (str (char n)))
(defn REMDR   [x y] (clojure.core/rem (long x) (long y)))

;; ── Real functions (SPITBOL) ─────────────────────────────────────────────────
;; Each takes an integer, a real or a numeric string and returns a real. The
;; error codes are SPITBOL's, measured with sbl -bf: a non-numeric argument is
;; 301 ATAN, 302 CHOP, 303 COS, 304 EXP, 306 LN, 308 SIN, 309 TAN, 313 SQRT;
;; overflow is 305 EXP, 307 LN (of 0), 310 TAN; 315 is LN of a negative and 314
;; SQRT of a negative.
(defn- real-error [code msg] (throw (ex-info msg {:snobol/error code})))

(defn- real-arg
  "x as a double, or ERROR code when it is not numeric."
  [x code fname]
  (let [n (env/num x)]
    (if (and (double? n) (Double/isNaN ^double n))
      (real-error code (str fname " argument not numeric"))
      (double n))))

(defn- finite-or [^double d code msg]
  (if (Double/isInfinite d) (real-error code msg) d))

(defn ATAN [x] (Math/atan (real-arg x 301 "ATAN")))
(defn CHOP
  "The real x truncated toward zero."
  [x]
  (let [d (real-arg x 302 "CHOP")]
    (clojure.core/+ 0.0 (if (neg? d) (Math/ceil d) (Math/floor d)))))
(defn COS  [x] (Math/cos (real-arg x 303 "COS")))
(defn EXP  [x] (finite-or (Math/exp (real-arg x 304 "EXP")) 305 "EXP produced real overflow"))
(defn LN
  [x]
  (let [d (real-arg x 306 "LN")]
    (cond (neg? d)  (real-error 315 "LN argument negative")
          (zero? d) (real-error 307 "LN produced real overflow")
          :else     (Math/log d))))
(defn SIN  [x] (Math/sin (real-arg x 308 "SIN")))
(defn TAN  [x] (finite-or (Math/tan (real-arg x 309 "TAN")) 310 "TAN produced real overflow"))
(defn SQRT
  [x]
  (let [d (real-arg x 313 "SQRT")]
    (if (neg? d) (real-error 314 "SQRT argument negative") (Math/sqrt d))))

(defn INTEGER [x]
  "Convert x to integer, or return ε (fail) if not convertible."
  (cond
    (integer? x) x
    (number?  x) (long x)
    :else (try (Long/parseLong (clojure.string/trim (str x)))
               (catch Exception _ ε))))

(defn REAL    [x]
  "Convert x to real (double), or return ε (fail) if not convertible."
  (cond
    (float? x)   x
    (number? x)  (double x)
    :else (try (Double/parseDouble (clojure.string/trim (str x)))
               (catch Exception _ ε))))

(defn STRING  [x]
  "Convert any value to its string representation (a real as SPITBOL prints it)."
  (sstr x))

;; ── Program-defined datatype ──────────────────────────────────────────────────
;; PDD instances are represented as Clojure maps:
;;   {:__type__ "COMPLEX", "REAL" 3.14, "IMAG" -2.0}
;; Constructor and accessor functions are stored as Clojure fns in the SNOBOL
;; namespace so INVOKE can find them via the normal ($$ op) fallthrough.

;; Global registry: type-name-str → vector of field name strings (upper-case)
(def data-type-registry (atom {}))

(defn- parse-data-spec
  "Parse 'NAME(F1,F2,...)' into [name-str [field-str ...]].
   NAME and fields may contain any chars except ( ) , whitespace."
  [S]
  (let [s (clojure.string/trim S)
        lp (.indexOf ^String s "(")
        rp (.lastIndexOf ^String s ")")]
    (when (and (> lp 0) (>= rp (inc lp)))
      (let [type-name  (.substring ^String s 0 lp)
            fields-str (.substring ^String s (inc lp) rp)
            fields     (if (clojure.string/blank? fields-str)
                         []
                         (mapv (comp clojure.string/trim)
                               (clojure.string/split fields-str #",")))]
        [type-name fields]))))

(defn DATA!
  "Parse and register a DATA type definition.
   Returns a do-form that installs constructor + accessor fns in the active ns."
  [S]
  (when-let [[type-name fields] (parse-data-spec S)]
    ;; Upcase for case-insensitive SNOBOL4 compatibility
    (let [tname  (clojure.string/upper-case type-name)
          fnames (mapv clojure.string/upper-case fields)]
      ;; Register in the global registry
      (swap! data-type-registry assoc tname fnames)
      ;; Build constructor: (TYPE f1 f2 ...) → {:__type__ "TYPE" "F1" f1 ...}
      ;; Build accessors:   (F inst) → val   and   (F inst new-val) → updated inst
      ;; We emit a form suitable for eval in the user namespace.
      (let [constructor-body
              `(fn [& args#]
                 (let [field-names# ~fnames
                       pairs# (map vector field-names# args#)]
                   (into {:__type__ ~tname} pairs#)))
            accessor-forms
              (for [f fnames]
                [f
                 `(fn
                    ([inst#]       (get inst# ~f ε))
                    ([inst# val#]  (assoc inst# ~f val#)))])]
        `(do
           (SNOBOL4clojure.env/snobol-set! '~(symbol tname) ~constructor-body)
           ~@(for [[f form] accessor-forms]
               `(SNOBOL4clojure.env/snobol-set! '~(symbol f) ~form)))))))

(defn DATA [S]
  (when-let [form (DATA! S)]
    (eval form)
    ε))

(defn FIELD
  "FIELD(type-name, n) — return the nth field name of the program-defined type.
   n is 1-based. Returns nil on out-of-range."
  [type-name n]
  (let [tname  (clojure.string/upper-case (str type-name))
        fields (get @data-type-registry tname)]
    (when (and fields (>= (long n) 1) (<= (long n) (count fields)))
      (nth fields (dec (long n))))))

;; ── Date / time ───────────────────────────────────────────────────────────────
(defn DATE   []    (.toString (java.util.Date.)))
(defn TIME   []    (System/currentTimeMillis))

;; ── I/O ───────────────────────────────────────────────────────────────────────
;; Bare INPUT() call (no args): read one line from stdin.
(defn READ-LINE! []
  (let [line (try (read-line) (catch Exception _ nil))]
    (if (nil? line) ε (str line))))

;; ── Named I/O channel management (Sprint 25D) ────────────────────────────────
;; SNOBOL4 syntax:
;;   INPUT(.VAR, unit)              — associate VAR with stdin on unit
;;   INPUT(.VAR, unit,, 'file')     — open file for input on unit
;;   OUTPUT(.VAR, unit)             — associate VAR with stdout on unit
;;   OUTPUT(.VAR, unit,, 'file')    — open file for output on unit
;;
;; After association:  reading  $VAR  reads a line from the channel.
;;                     writing  VAR = val  writes val to the channel.
;; ENDFILE(unit)  — signal EOF / close channel
;; DETACH(var)    — disassociate var from its channel

(defn- channel-file
  "The java.io.File a channel's file name names: relative to env/*file-dir*
   when that is bound, else to the working directory."
  ^java.io.File [^String file-str]
  (let [f (java.io.File. file-str)]
    (if (and env/*file-dir* (not (.isAbsolute f)))
      (java.io.File. (str env/*file-dir*) file-str)
      f)))

(defn open-input-channel!
  "Register var-sym as an input channel on unit.
   filename nil or empty → stdin sentinel (reads via read-line, :file stored as nil).
   filename provided     → open file as BufferedReader.
   A pipe spec (\"|cmd\") or a file that cannot be opened is ERROR 116, as
   SPITBOL raises it. Returns ε on success."
  [var-sym unit filename]
  (let [file-str (when (and filename (not (clojure.string/blank? (str filename))))
                   (str filename))
        rdr (if file-str
              (or (when-not (clojure.string/starts-with? file-str "|")
                    (try (java.io.BufferedReader. (java.io.FileReader. (channel-file file-str)))
                         (catch Exception _ nil)))
                  (throw (ex-info "inappropriate file specification for input"
                                  {:snobol/error 116})))
              ;; stdin: wrap *in* but mark :file nil so close-channel! won't close it
              (java.io.BufferedReader. *in*))]
    (when rdr
      (let [ch {:type :input :unit unit :var var-sym :reader rdr :file file-str}]
        (swap! <CHANNELS> assoc var-sym ch)
        (swap! <CHANNELS> assoc unit    ch)
        ε))))

(defn open-output-channel!
  "Register var-sym as an output channel on unit.
   filename nil or empty → stdout (writes via PrintWriter, :file stored as nil).
   filename provided     → open file as PrintWriter.
   A pipe spec (\"|cmd\") or a file that cannot be opened is ERROR 160, as
   SPITBOL raises it. Returns ε on success."
  [var-sym unit filename]
  (let [file-str (when (and filename (not (clojure.string/blank? (str filename))))
                   (str filename))
        wtr (if file-str
              (or (when-not (clojure.string/starts-with? file-str "|")
                    (try (java.io.PrintWriter. (java.io.FileWriter. (channel-file file-str)))
                         (catch Exception _ nil)))
                  (throw (ex-info "inappropriate file specification for output"
                                  {:snobol/error 160})))
              ;; stdout: wrap *out* but mark :file nil so close-channel! won't close it
              (java.io.PrintWriter. *out* true))]
    (when wtr
      (let [ch {:type :output :unit unit :var var-sym :writer wtr :file file-str}]
        (swap! <CHANNELS> assoc var-sym ch)
        (swap! <CHANNELS> assoc unit    ch)
        ε))))

(defn close-channel!
  "Close and deregister a channel by unit number or var symbol."
  [key]
  (when-let [ch (get @<CHANNELS> key)]
    (try
      (when-let [rdr (:reader ch)] (.close ^java.io.BufferedReader rdr))
      (when-let [wtr (:writer ch)] (.close ^java.io.PrintWriter    wtr))
      (catch Exception _ nil))
    (swap! <CHANNELS> dissoc (:var ch))
    (swap! <CHANNELS> dissoc (:unit ch))
    ε))

(defn write-to-channel!
  "Write val to the output channel registered for var-sym.
   Returns val on success, nil if no channel registered."
  [var-sym val]
  (when-let [ch (get @<CHANNELS> var-sym)]
    (when (clojure.core/= :output (:type ch))
      (let [wtr ^java.io.PrintWriter (:writer ch)]
        (.println wtr (sstr val))
        (.flush   wtr)
        val))))

(defn BACKSPACE [] ε)
(defn DETACH
  "DETACH(var) — disassociate var from its I/O channel."
  ([] ε)
  ([var-sym]
   (close-channel! (symbol (str var-sym)))))

(defn EJECT     [] ε)
(defn ENDFILE
  "ENDFILE(unit) — signal EOF and close the channel on unit."
  ([] ε)
  ([unit] (close-channel! unit)))

(defn INPUT     [] (READ-LINE!))   ; bare INPUT() — reads stdin
(defn OUTPUT    [] ε)
(defn REWIND
  "REWIND(unit) — rewind file channel to beginning."
  ([] ε)
  ([unit]
   (when-let [ch (get @<CHANNELS> unit)]
     (when-let [rdr ^java.io.BufferedReader (:reader ch)]
       (try (.reset rdr) (catch Exception _ nil)))
     ε)))

;; ── Memory stubs ──────────────────────────────────────────────────────────────
(defn CLEAR   [] ε)
(defn COLLECT [] ε)
(defn DUMP    [] ε)

;; ── Program control stubs ─────────────────────────────────────────────────────
(defn EXIT    [] nil)
(defn HOST    [] nil)
(defn SETEXIT [] nil)
(defn STOPTR  [] nil)
(defn TRACE   [] nil)

;; ── Function definition stubs ─────────────────────────────────────────────────
(defn APPLY
  "APPLY(fname, arg1, ...) — call a named function by string/symbol."
  [fname & fargs]
  (let [f-sym (symbol (str fname))
        f-fn  (SNOBOL4clojure.env/$$ f-sym)]
    (when (fn? f-fn)
      (apply f-fn fargs))))
(defn ARG
  "ARG(fname, n) — return the name of the nth parameter of function fname (1-based).
   Returns nil (statement failure) if fname unknown or n out of range."
  [fname n]
  (when-let [meta (get @SNOBOL4clojure.env/<FDEFS>
                       (clojure.string/upper-case (str fname)))]
    (nth (:params meta) (dec (long n)) nil)))

(defn LOCAL
  "LOCAL(fname, n) — return the name of the nth local variable of function fname (1-based).
   Returns nil (statement failure) if fname unknown or n out of range."
  [fname n]
  (when-let [meta (get @SNOBOL4clojure.env/<FDEFS>
                       (clojure.string/upper-case (str fname)))]
    (nth (:locals meta) (dec (long n)) nil)))

;; ── Table / array functions ───────────────────────────────────────────────────
(defn ITEM
  "ITEM(table, key) — subscript read/lvalue for TABLE.
   Returns the value at key, or ε if absent."
  [t k]
  (SNOBOL4clojure.env/table-get t k))

(defn PROTOTYPE
  "PROTOTYPE(array-or-table) — return the dimension spec string.
   For TABLE returns ε.  For ARRAY returns normalized 'lo:hi,...' string."
  [x]
  (cond
    (SNOBOL4clojure.env/table? x) ""
    (SNOBOL4clojure.env/array? x) (SNOBOL4clojure.env/array-prototype x)
    :else ""))

(defn- sort-key-compare
  "SPITBOL's sort order: two numbers compare as numbers, anything else as strings."
  [a b]
  (if (and (number? a) (number? b)) (compare a b) (compare (str a) (str b))))

(defn- table->sorted-array
  "Convert TABLE to an Nx2 ARRAY (keys in column 1, values in column 2)
   sorted on column col (1 by default, the keys). SORT sorts ascending,
   RSORT descending."
  [t col descending?]
  (let [pick    (if (clojure.core/= 2 col) second first)
        cmp     (if descending? #(sort-key-compare %2 %1) sort-key-compare)
        entries (sort-by pick cmp (seq @t))
        n       (count entries)
        arr     (ARRAY (str n ",2"))]
    (doseq [[i [k v]] (map-indexed vector entries)]
      (array-set arr [(inc i) 1] k)
      (array-set arr [(inc i) 2] v))
    arr))

(defn SORT
  ([A] (SORT A 1))
  ([A col]
   (cond
     (table? A) (table->sorted-array A (long (env/num col)) false)
     (array? A) A   ; ARRAY sort not yet defined — return as-is
     :else nil)))

(defn RSORT
  ([A] (RSORT A 1))
  ([A col]
   (cond
     (table? A) (table->sorted-array A (long (env/num col)) true)
     (array? A) A
     :else nil)))
