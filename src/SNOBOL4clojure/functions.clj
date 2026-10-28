(ns SNOBOL4clojure.functions
  ;; Built-in SNOBOL4 functions.
  ;; Stubs are marked clearly; implemented ones are complete.
  (:require [SNOBOL4clojure.env :as env :refer [ε equal subtract sstr
                                        DATATYPE table? array? table-get table-set
                                        ARRAY ->SnobolArray array-get array-set
                                        <CHANNELS>]])
  (:refer-clojure :exclude [= + - * / num]))

;; ── String functions ──────────────────────────────────────────────────────────

(defn- string->real
  "The real a string denotes, or nil (failure). SNOBOL4's syntax, measured
   with sbl -bf: blanks at the ends ignored, the null string is 0., then an
   optional sign, digits with an optional point, and an optional exponent
   marked e, E, d or D. A value out of range fails."
  [x]
  (let [t (clojure.string/trim (str x))]
    (cond
      (clojure.core/= t "") 0.0
      ;; an integer's digits are an integer first (one too large for 64 bits is no number), anything else a real as gtnum reads it
      (re-matches #"[+-]?\d+" t)
      (try (double (Long/parseLong t)) (catch NumberFormatException _ nil))
      (re-matches #"[+-]?(\d+\.?\d*|\.\d+)([eEdD][+-]?\d+)?" t)
      (env/parse-real t)
      :else nil)))

(defn- real->integer
  "The integer a real becomes: truncated toward zero. A real outside the
   integer range is the least integer, as sbl -bf gives it (the x86-64
   conversion), not a failure."
  [x]
  (let [lo (double Long/MIN_VALUE)]
    (if (and (clojure.core/<= lo x) (clojure.core/< x (clojure.core/- lo)))
      (long x)
      Long/MIN_VALUE)))

;; An integer argument as sbl -bf reads it: an integer, a real (truncated toward zero) or a numeric string (read as a number, so a point or an exponent makes it a
;; real, which is truncated). Anything else is the calling function's own error, which the caller names.
(defn integer-of
  "The integer x is as an integer argument, or nil when it is none."
  [x]
  (cond
    (integer? x) x
    (double? x)  (real->integer x)
    :else        (let [t (clojure.string/trim (str x))]
                   (if (re-matches #"[+-]?\d+" t)
                     (try (Long/parseLong t) (catch NumberFormatException _ nil))
                     (when-some [d (string->real t)] (real->integer d))))))

(defn- int-arg [x err]
  (if-some [n (integer-of x)] n (throw (ex-info "argument is not integer" {:snobol/error err}))))


;; A string argument as SPITBOL converts it: an integer as its digits and a real "converted to its standard representation" (manual ch. 17, Real to string: 21. for 21.0,
;; 2.71828182845905 for e), which sstr writes; the null string for nil.
(defn- str-arg ^String [x] (sstr x))

(defn SIZE    [s]         (count (str-arg s)))
(defn REPLACE [s1 s2 s3]
  ;; Replace chars in s1: each char of s2 mapped to corresponding char of s3
  (let [s1 (str-arg s1) s2 (str-arg s2) s3 (str-arg s3)]
    (apply str (map #(let [i (.indexOf ^String s2 (str %))]
                       (if (>= i 0) (.charAt ^String s3 i) %))
                    s1))))
(defn DUPL    [x i]   (apply str (repeat (int-arg i 90) (if (string? x) x (if (number? x) (str-arg x) x)))))
(defn TRIM    [s]     (clojure.string/trimr (str-arg s)))
(defn REVERSE [s]     (apply str (clojure.string/reverse (str-arg s))))
(defn LPAD
  ([s n]        (LPAD s n " "))
  ([s n fill]   (let [s-str  (str-arg s)
                      pad    (clojure.core/- (int-arg n 145) (count s-str))
                      fill-c (let [f (str fill)] (if (clojure.core/= "" f) " " (subs f 0 1)))]   ; a null fill pads with blanks
                  (if (clojure.core/<= pad 0) s-str
                    (str (apply str (repeat pad fill-c)) s-str)))))
(defn RPAD
  ([s n]        (RPAD s n " "))
  ([s n fill]   (let [s-str  (str-arg s)
                      pad    (clojure.core/- (int-arg n 179) (count s-str))
                      fill-c (let [f (str fill)] (if (clojure.core/= "" f) " " (subs f 0 1)))]   ; a null fill pads with blanks
                  (if (clojure.core/<= pad 0) s-str
                    (str s-str (apply str (repeat pad fill-c)))))))
(defn SUBSTR
  "SUBSTR(s, i, n): the n characters of s from the 1-based position i, where n = 0 means all the rest of s from i. It fails when i is not from 1 to one past
   the end of s, when n is negative, or when the n characters are not all there, as sbl -bf does."
  [s i n]
  (let [s (str-arg s) i (int-arg i 193) n (int-arg n 192) len (count s)]
    (when (and (clojure.core/<= 1 i (inc len)) (clojure.core/>= n 0))
      (if (zero? n)
        (subs s (dec i))
        (when (clojure.core/<= (clojure.core/+ (dec i) n) len)
          (subs s (dec i) (clojure.core/+ (dec i) n)))))))

;; A table as an array: "The table is converted to a two-dimensional array ... The table must have at least one non-null element. If the table is empty, CONVERT,
;; RSORT and SORT will fail" (manual ch. 17, Table to array). As sbl -bf has it, an entry whose value is null is no element: N rows of key and value, in the order
;; the keys were first assigned, for the N entries with a value; nil (failure) when there are none.
(defn table->array [t]
  (let [entries (remove (fn [[_ v]] (or (nil? v) (clojure.core/= "" v))) (env/table-entries t))
        n       (count entries)]
    (when (pos? n)
      (let [arr (ARRAY (str n ",2"))]
        (doseq [[i [k v]] (map-indexed vector entries)]
          (array-set arr [(inc i) 1] k)
          (array-set arr [(inc i) 2] v))
        arr))))

(defn CONVERT [x t]
  "CONVERT(x, type) — coerce x to the named SNOBOL4 type.
   Returns nil (failure) if the conversion is not defined."
  ;; "Data type names may be lower case if case-folding is active" (manual ch. 19, CONVERT): while &CASE is 0, as sbl -bf runs, only the capitals name a type
  (let [from (clojure.string/upper-case (DATATYPE x))
        to   (if (zero? (long @env/&CASE)) (str t) (clojure.string/upper-case (str t)))]
    (cond
      ;; Same type → identity (but always a 'new' value logically)
      (clojure.core/= from to) x

      ;; "s can take an additional data type, 'NUMERIC', which converts the argument to integer or real, as appropriate" (manual ch. 19, CONVERT): a number is itself, a
      ;; string of a number's form its number (blanks around it ignored, the null string 0), anything else fails
      (clojure.core/= to "NUMERIC")
      (cond (number? x) x
            (string? x) (let [n (env/num x)] (when-not (and (double? n) (Double/isNaN n)) n))
            :else nil)

      ;; STRING conversions
      ;; Integer syntax is an integer (out of range fails); anything else is read
      ;; as a real and converted as one. nil propagates as failure.
      (and (clojure.core/= from "STRING") (clojure.core/= to "INTEGER"))
      (let [t (clojure.string/trim (str x))]
        (if (re-matches #"[+-]?\d+" t)
          (try (Long/parseLong t) (catch NumberFormatException _ nil))
          (when-some [d (string->real t)] (real->integer d))))

      (and (clojure.core/= from "STRING") (clojure.core/= to "REAL"))
      (string->real x)

      (and (clojure.core/= from "STRING") (clojure.core/= to "PATTERN"))
      ;; A string converts to a literal LIT$ pattern node
      (list 'SEQ (list 'LIT$ (str x)))

      (and (clojure.core/= from "STRING") (clojure.core/= to "NAME"))
      ;; A string becomes the name of the variable it spells, the NAME .X gives ("a STRING to NAME conversion ... A subsequent reference to $X would access
      ;; a variable with name '1'", manual ch. 17, Integer to name), so IDENT(.X, CONVERT('X','NAME')) succeeds and both are one table key, as in sbl -bf
      (SNOBOL4clojure.env.NAME. (symbol (str x)))

      ;; INTEGER conversions
      (and (clojure.core/= from "INTEGER") (clojure.core/= to "STRING"))
      (str x)

      (and (clojure.core/= from "INTEGER") (clojure.core/= to "REAL"))
      (double x)

      (and (clojure.core/= from "INTEGER") (clojure.core/= to "PATTERN"))
      (list 'SEQ (list 'LIT$ (str x)))

      (and (clojure.core/= from "INTEGER") (clojure.core/= to "NAME"))
      (SNOBOL4clojure.env.NAME. (symbol (str x)))

      ;; REAL conversions: "The real number is converted to its standard representation" (manual ch. 17, Real to string), as sstr writes it
      (and (clojure.core/= from "REAL") (clojure.core/= to "STRING"))
      (str-arg x)

      (and (clojure.core/= from "REAL") (clojure.core/= to "INTEGER"))
      (real->integer x)

      (and (clojure.core/= from "REAL") (clojure.core/= to "PATTERN"))
      (list 'SEQ (list 'LIT$ (str-arg x)))

      (and (clojure.core/= from "REAL") (clojure.core/= to "NAME"))
      (SNOBOL4clojure.env.NAME. (symbol (str-arg x)))

      ;; ARRAY conversions
      (and (clojure.core/= from "ARRAY") (clojure.core/= to "TABLE"))
      ;; ARRAY must be Nx2; column 1 = key, column 2 = value.
      ;; Only succeeds if exactly 2 dimensions and second dim size is 2.
      (let [dims (:dims x)]
        (if (and (clojure.core/= (count dims) 2)
                 (clojure.core/= (second (second dims)) 2)
                 (clojure.core/= (first (second dims)) 1))
          (let [[lo1 hi1] (first dims)
                t (env/TABLE (inc (clojure.core/- hi1 lo1)))]
            (doseq [i (range lo1 (inc hi1))]
              (let [k (array-get x [i 1])
                    v (array-get x [i 2])]
                (when-not (nil? k)
                  (env/table-set t k (or v ε)))))
            t)
          nil))  ; wrong shape → fail

      ;; TABLE conversions
      (and (clojure.core/= from "TABLE") (clojure.core/= to "ARRAY"))
      (table->array x)

      ;; PATTERN stays PATTERN
      (and (clojure.core/= from "PATTERN") (clojure.core/= to "PATTERN")) x

      ;; NAME conversions (a NAME, or a symbol where the engine keeps one, reads as its variable's name)
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
    (table? x) (let [t (atom @x :meta {:order (atom (if-let [o (:order (meta x))] @o {})) :size (:size (meta x) 11) :serial (SNOBOL4clojure.env/next-serial!)})] t)
    (array? x) (assoc (SNOBOL4clojure.env/->SnobolArray (:dims x) (:dflt x) (atom @(:data x))) :proto (:proto x) :serial (SNOBOL4clojure.env/next-serial!))
    ;; a DATA object: a new object with the same field values
    (instance? SNOBOL4clojure.env.DataObj x)
    (SNOBOL4clojure.env/->DataObj (.type-name ^SNOBOL4clojure.env.DataObj x) (.field-names ^SNOBOL4clojure.env.DataObj x)
                                  (aclone ^objects (.fields ^SNOBOL4clojure.env.DataObj x)) (SNOBOL4clojure.env/next-serial!))
    :else x))

;; ── Type conversion & char functions ─────────────────────────────────────────
(defn ASCII   [s]  (int (first (str s))))
(defn CHAR
  "CHAR(n): the one-character string whose code is n, 0 to 255."
  [n]
  (let [n (int-arg n 281)]
    (if (clojure.core/<= 0 n 255)
      (str (char n))
      (throw (ex-info "char argument not in range" {:snobol/error 282})))))
(defn ORD
  "ORD(s): the code of the first character of s; it fails for the null string. A number is read as its text."
  [s]
  (let [t (sstr s)] (when (seq t) (long (int (first t))))))
(declare finite-or)
;; REMDR(n1, n2) as sbl.min s_rmd computes it: the arguments convert as an arithmetic operator's do; two integers give the integer remainder, its sign the
;; first's ("167 remdr caused integer overflow" for a zero divisor), and otherwise both are reals and the result is chop(n1/n2) * n2 - n1 given the sign of n1,
;; each step rounded ("312 remdr caused real overflow" when the quotient overflows). The errors' texts are sbl -bf's.
(defn REMDR [x y]
  (let [a (env/num x)
        b (env/num y)
        nan? #(and (double? %) (Double/isNaN ^double %))]
    (when (nan? a) (throw (ex-info "remdr first argument is not numeric" {:snobol/error 166})))
    (when (nan? b) (throw (ex-info "remdr second argument is not numeric" {:snobol/error 165})))
    (if (and (integer? a) (integer? b))
      (if (zero? b) (throw (ex-info "remdr caused integer overflow" {:snobol/error 167})) (clojure.core/rem (long a) (long b)))
      (let [a (double a)
            q (clojure.core// a (double b))
            _ (when (or (Double/isInfinite q) (Double/isNaN q)) (throw (ex-info "remdr caused real overflow" {:snobol/error 312})))
            q (finite-or q 312 "remdr caused real overflow")
            d (finite-or (clojure.core/- (finite-or (clojure.core/* (if (neg? q) (Math/ceil q) (Math/floor q)) (double b)) 312 "") a) 312 "")]
        (if (neg? a) (if (clojure.core/<= d 0.0) d (clojure.core/- d)) (if (neg? d) (clojure.core/- d) d))))))

;; ── Real functions (SPITBOL) ─────────────────────────────────────────────────
;; Each takes an integer, a real or a numeric string and returns a real. The
;; error codes are SPITBOL's, measured with sbl -bf: a non-numeric argument is
;; 301 ATAN, 302 CHOP, 303 COS, 304 EXP, 306 LN, 308 SIN, 309 TAN, 313 SQRT;
;; overflow is 305 EXP, 307 LN (of 0), 310 TAN; 315 is LN of a negative and 314
;; SQRT of a negative.
;; the text is SPITBOL's, in lower case as sbl -bf writes it ("ln produced real overflow")
(defn- real-error [code msg] (throw (ex-info (or (env/error-message code) msg) {:snobol/error code})))

(defn- real-arg
  "x as a double, or ERROR code when it is not numeric."
  [x code fname]
  (let [n (env/num x)]
    (if (and (double? n) (Double/isNaN ^double n))
      (real-error code (str fname " argument not numeric"))
      (double n))))

;; a result too large is the function's error, one below the least normal real zero (the oracle treats a denormal as zero, MXCSR 0x9fc0)
(defn- finite-or [^double d code msg]
  (cond (Double/isInfinite d)              (real-error code msg)
        (< (Math/abs d) Double/MIN_NORMAL) (clojure.core/* d 0.0)
        :else                              d))

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

(defn integer-predicate
  "INTEGER(x) as a program sees it: the null string when x is an integer or a
   string that reads as one (blanks and a sign allowed; the null string is 0),
   nil (failure) otherwise."
  [x]
  (cond
    (integer? x) ε
    (number? x)  nil
    :else (when (re-matches #"|[+-]?[0-9]+" (clojure.string/trim (str x))) ε)))

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

;; Global registry: type-name-str → vector of field name strings, as written (sbl -bf folds no name)
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

(defn- field-slot
  "Where field f lives in inst: its index in inst's type. A field function applied to an object whose type has no such field, or to anything that is no DATA object,
   is \"41 FIELD function argument is wrong datatype\" (manual, Appendix D), as sbl -bf raises it."
  [f inst]
  (or (when (instance? SNOBOL4clojure.env.DataObj inst)
        (let [fields (.field-names ^SNOBOL4clojure.env.DataObj inst) i (.indexOf ^java.util.List (vec fields) f)]
          (when (>= i 0) i)))
      (throw (ex-info "field function argument is wrong datatype" {:snobol/error 41}))))

(defn field-fn
  "The field function named f: (F inst) reads the field and (F inst v) assigns it in place. One function serves every DATA type that has a field named f, as in
   SPITBOL (DATA('NODE(VAL,LSON)') and DATA('CLUNK(VALUE,LSON)') share LSON)."
  [f]
  (fn
    ([inst]   (let [i (int (field-slot f inst))] (aget ^objects (.fields ^SNOBOL4clojure.env.DataObj inst) i)))
    ([inst v] (let [i (int (field-slot f inst))] (aset ^objects (.fields ^SNOBOL4clojure.env.DataObj inst) i v) v))))

(defn DATA!
  "Parse and register a DATA type definition, and return a form that installs its creation function and field functions."
  [S]
  (when-let [[type-name fields] (parse-data-spec S)]
    ;; The names are kept as written: sbl -bf folds none, so DATA('node(val,next)') defines node, val and next.
    (let [tname  type-name
          fnames fields]
      (swap! data-type-registry assoc tname fnames)
      ;; "The DATA function implicitly defines a creation function and n new field reference functions" (manual ch. 19, DATA): they are functions, kept with the
      ;; DEFINEd ones in <FUNS> and marked :data, not values of variables, so a variable or a parameter of the same name (DEFINE('INSERT(LIST,CARD)') beside
      ;; DATA('CARD(NUM,NEXT)')) leaves them alone. The creation function makes a new DataObj, a field not given being the null string.
      `(do
         (swap! SNOBOL4clojure.env/<FUNS> assoc ~tname
                (with-meta (fn [& args#]
                             (SNOBOL4clojure.env/->DataObj ~tname ~fnames (object-array (map-indexed (fn [i# _#] (nth args# i# ε)) ~fnames))
                                                           (SNOBOL4clojure.env/next-serial!)))
                           {:data true}))
         ~@(for [f fnames]
             `(swap! SNOBOL4clojure.env/<FUNS> assoc ~f (with-meta (field-fn ~f) {:data true :field true})))))))

(defn DATA [S]
  (when-let [form (DATA! S)]
    (eval form)
    ε))

(defn FIELD
  "FIELD(type-name, n) — return the nth field name of the program-defined type.
   n is 1-based. Returns nil on out-of-range."
  [type-name n]
  (let [tname  (str type-name)
        fields (get @data-type-registry tname)]
    (when (and fields (>= (long n) 1) (<= (long n) (count fields)))
      (nth fields (dec (long n))))))

;; ── Date / time ───────────────────────────────────────────────────────────────
;; DATE(i) "produce date and time" (manual ch. 19). As sbl -bf answers it: mm/dd/yy hh:mm:ss, mm/dd/yyyy hh:mm:ss for 1, yyyy-mm-dd hh:mm:ss for 2, the first
;; for any other integer; an argument that is not an integer is "330 date argument is not integer".
(defn DATE
  ([] (DATE 0))
  ([i]
   (let [n (if (or (nil? i) (clojure.core/= "" i)) 0 (SNOBOL4clojure.env/num i))]
     (when-not (integer? n) (throw (ex-info "date argument is not integer" {:snobol/error 330})))
     (.format (java.time.LocalDateTime/now)
              (java.time.format.DateTimeFormatter/ofPattern (case (long n) 1 "MM/dd/yyyy HH:mm:ss" 2 "yyyy-MM-dd HH:mm:ss" "MM/dd/yy HH:mm:ss"))))))
(defn TIME   []    (System/currentTimeMillis))

;; ── I/O ───────────────────────────────────────────────────────────────────────
;; Bare INPUT() call (no args): read one line from stdin.
(defn READ-LINE! []
  (let [line (try (.readLine (env/stdin-reader)) (catch Exception _ nil))]
    (if (nil? line) ε (env/input-record line))))

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

;; A SPITBOL file argument is a name and options, as sbl x64 reads it (osint lenfnm and sioarg): a trailing [options] is set aside, the name runs to the first blank,
;; and everything after it is options, separated by blanks, tabs, commas, brackets and single dashes. -f<n> names an open file descriptor (0 standard input, 1
;; standard output, 2 standard error), -r<n> (and -q<n>) is raw mode (a read takes up to n characters, end-of-line characters among them, and fails only when it gets
;; none), -c is raw mode by one character, -l<n> line mode with records of at most n characters (the rest of a longer line is dropped), -a appends to the file; B, I,
;; S, U, W, X and Y are accepted and change nothing here. Returns {:name "" :opts {\f 0 ...} :bad b}; an options-only argument has the null string for its name.
(defn file-spec [s]
  (let [s   (str s)
        n   (count s)
        ;; lenfnm: a trailing [options] is set aside, and the name runs to the first blank of what is left
        len (or (when (and (pos? n) (clojure.core/= \] (.charAt s (dec n))))
                  (loop [i (clojure.core/- n 2)]
                    (cond (neg? i) nil
                          (clojure.core/= \] (.charAt s i)) nil
                          (clojure.core/= \[ (.charAt s i)) i
                          :else (recur (dec i)))))
                n)
        b   (.indexOf (subs s 0 len) " ")
        nm  (if (neg? b) (subs s 0 len) (subs s 0 b))
        ;; sioarg: every character after the name is option text
        [opts bad]
        (loop [i (count nm) dash false opts {}]
          (if (>= i n)
            [opts false]
            (let [ch  (Character/toUpperCase (.charAt s i))
                  num (fn [] (let [j (loop [j (inc i)] (if (and (< j n) (Character/isDigit (.charAt s j))) (recur (inc j)) j))]
                               [(if (> j (inc i)) (Long/parseLong (subs s (inc i) j)) 0) j]))]
              (case ch
                (\space \tab \, \[ \]) (recur (inc i) false opts)
                \- (if dash [opts true] (recur (inc i) true opts))
                (\A \C \I \U \W \X \Y) (recur (inc i) false (assoc opts (Character/toLowerCase ch) true))
                \F (let [[v j] (num)] (recur j false (assoc opts \f v)))
                (\B \L \Q \R) (let [[v j] (num)]
                                  (if (pos? v) (recur j false (assoc opts (if (clojure.core/= ch \Q) \r (Character/toLowerCase ch)) v)) [opts true]))
                \S (let [d  (when (< (inc i) n) (Character/toUpperCase (.charAt s (inc i))))
                         e  (when (< (clojure.core/+ i 2) n) (Character/toUpperCase (.charAt s (clojure.core/+ i 2))))
                         rw (and (clojure.core/= e \R) (< (clojure.core/+ i 3) n) (clojure.core/= \W (Character/toUpperCase (.charAt s (clojure.core/+ i 3)))))]
                     (if (and (clojure.core/= d \D) (#{\N \R \W} e) (< (clojure.core/+ i (if rw 4 3)) (inc n)))
                       (recur (clojure.core/+ i (if rw 4 3)) false opts)
                       [opts true]))
                [opts true]))))]
    ;; an option the scan does not know, or a name with -f ("CANNOT specify BOTH filename and -f option!", sysfc.c), makes the argument :bad (ERROR 116, 160)
    {:name nm :opts opts :bad (boolean (or bad (and (seq nm) (contains? opts \f))))}))

(defn channel-key [u]
  (if (integer? u)
    u
    (let [t (clojure.string/trim (if (nil? u) "" (sstr u)))]
      (cond (clojure.core/= "" t)        0
            (re-matches #"[+-]?\d+" t)  (Long/parseLong t)
            :else                        t))))

(defn- null-arg? [x] (or (nil? x) (clojure.core/= "" x)))

;; "If the third argument, s, is omitted, the file name can be provided by setting a system environment variable with the same name as the channel ... If s is
;; omitted, and no channel association appears on the command line or as an environment variable, then the standard system input and output channels are assumed"
;; (manual ch. 19, INPUT). sbl -bf takes the standard channel only when the channel is omitted as well: a channel with no file is the stream that channel already
;; has, else the file an environment variable of its name names, else ERROR 116 (160 for OUTPUT), and so is a file with no channel. A fourth argument, CSNOBOL4's
;; file name, is not read (sbl -bf: INPUT(.X, 8, , 'f') is ERROR 116). Returns [key file-argument channel-to-share], or calls bad.
(defn- association [type unit filename bad]
  (let [{nm :name opts :opts} (file-spec (or filename ""))]
    (cond
      (null-arg? unit)                                       (if (null-arg? filename) [0 nil nil] (bad))
      (or (not (clojure.string/blank? nm)) (contains? opts \f)) [(channel-key unit) filename nil]
      :else
      (let [k  (channel-key unit)
            ch (get @<CHANNELS> k)
            e  (when (string? unit) (not-empty (System/getenv (clojure.string/trim unit))))]
        (cond
          (and ch (clojure.core/= type (:type ch))) [k nil ch]
          e                                         [k e nil]
          :else                                     (bad))))))

(defn open-input-channel!
  "Register var-sym as an input channel: INPUT(name, unit, file) as sbl -bf takes it (association above).
   No file and no unit → stdin (reads via read-line, :file stored as nil); a file → a BufferedReader. A pipe spec (\"|cmd\") is ERROR 116, as SPITBOL raises it.
   A file that cannot be opened fails, as in sbl -bf. Returns ε on success, nil on failure."
  [var-sym unit filename]
  (let [bad      #(throw (ex-info "inappropriate file specification for input" {:snobol/error 116}))
        [unit filename shared] (association :input unit filename bad)
        {nm :name opts :opts blank-in-name :bad} (file-spec (or filename ""))
        file-str (when-not (clojure.string/blank? nm) nm)
        _        (when blank-in-name (bad))
        rdr (cond
              shared   nil
              file-str (if (clojure.string/starts-with? file-str "|")
                         (bad)
                         (try (java.io.BufferedReader. (java.io.InputStreamReader. (java.io.FileInputStream. (channel-file file-str)) "ISO-8859-1"))
                              (catch Exception _ nil)))
              ;; stdin: wrap *in* but mark :file nil so close-channel! won't close it
              :else    (env/stdin-reader))]
    (cond
      shared (do (swap! <CHANNELS> assoc var-sym shared) ε)
      rdr
      ;; :at is the position of the next read, which SET answers and moves
      (let [ch (cond-> {:type :input :unit unit :var var-sym :reader rdr :file file-str :at (atom 0)}
                 (get opts \r) (assoc :raw (get opts \r))
                 (get opts \c) (assoc :raw 1)
                 (get opts \l) (assoc :maxlen (get opts \l)))]
        (swap! <CHANNELS> assoc var-sym ch)
        (swap! <CHANNELS> assoc unit    ch)
        ε))))

;; REWIND(unit): an input file starts again from its first record. A unit no INPUT or OUTPUT opened is ERROR 174, as sbl -bf raises it (standard input among them).
(defn rewind-channel! [unit]
  (let [u  (channel-key unit)
        ch (get @<CHANNELS> u)]
    (cond
      (nil? ch) (throw (ex-info "rewind file does not exist" {:snobol/error 174}))
      (and (clojure.core/= :input (:type ch)) (:file ch))
      (let [rdr (java.io.BufferedReader. (java.io.InputStreamReader. (java.io.FileInputStream. (channel-file (:file ch))) "ISO-8859-1"))
            ch' (assoc ch :reader rdr :at (atom 0))]
        (try (.close ^java.io.BufferedReader (:reader ch)) (catch Exception _ nil))
        (swap! <CHANNELS> assoc (:var ch) ch' (:unit ch) ch')
        ε)
      :else ε)))

;; "SET(channel, offset, whence) ... SET repositions the file attached to this channel and returns the new position as the result. The next read or write operation will
;; take place at that position in the file. ... If whence is 0, the file pointer is set to offset bytes from the beginning of the file. If whence is 1, the file pointer is
;; set to its current position plus offset ... SET(channel, 0, 1) simply returns the current file position without changing it. If whence is 2, the pointer is set to the
;; current size of the file plus offset" (manual ch. 19, SET). As sbl -bf has it: the channel is a unit number or a numeric string; a null one is "292 SET first argument
;; is null", one no INPUT or OUTPUT opened (a real names none) "295 SET file does not exist"; an offset or a whence that is not an integer (a numeric string is one, the
;; null string 0) is "293 Inappropriate second argument to SET", for the whence too; a whence other than 0, 1 and 2 is "297 SET caused non-recoverable I/O error". The
;; position stays within the file. A channel on standard input or output answers where it is and moves nowhere, "296 SET file does not permit setting file pointer".
(defn SET
  ([] (SET nil nil nil))
  ([unit] (SET unit nil nil))
  ([unit offset] (SET unit offset nil))
  ([unit offset whence]
   (let [fail   (fn [n] (throw (ex-info (env/error-message n) {:snobol/error n})))
         int-of (fn [x] (cond (or (nil? x) (clojure.core/= "" x)) 0
                              (integer? x)                        x
                              (string? x)                         (let [t (clojure.string/trim x)] (when (re-matches #"[+-]?\d+" t) (Long/parseLong t)))))
         _      (when (or (nil? unit) (clojure.core/= "" unit)) (fail 292))
         ch     (get @<CHANNELS> (when (or (integer? unit) (string? unit)) (int-of unit)))
         _      (when-not ch (fail 295))
         off    (or (int-of offset) (fail 293))
         wh     (or (int-of whence) (fail 293))
         _      (when-not (#{0 1 2} wh) (fail 297))
         f      (when (:file ch) (channel-file (:file ch)))
         target (fn [cur size] (max 0 (min size (case (long wh) 0 off 1 (clojure.core/+ cur off) 2 (clojure.core/+ size off)))))]
     (cond
       (and (clojure.core/= :input (:type ch)) f)
       (let [cur @(:at ch)]
         (if (and (zero? off) (clojure.core/= 1 wh))
           cur
           (let [to  (target cur (.length ^java.io.File f))
                 in  (java.io.FileInputStream. f)
                 _   (.skip in to)
                 ch' (assoc ch :reader (java.io.BufferedReader. (java.io.InputStreamReader. in "ISO-8859-1")) :at (atom to))]
             (try (.close ^java.io.BufferedReader (:reader ch)) (catch Exception _ nil))
             (swap! <CHANNELS> assoc (:var ch) ch' (:unit ch) ch')
             to)))
       (:stream ch)
       (let [fc (.getChannel ^java.io.FileOutputStream (:stream ch))]
         (.flush ^java.io.PrintWriter (:writer ch))
         (let [to (target (.position fc) (.size fc))] (.position fc (long to)) to))
       (and (zero? off) (clojure.core/= 1 wh))
       (if-let [at (:at ch)] @at 0)
       :else (fail 296)))))

(defn open-output-channel!
  "Register var-sym as an output channel: OUTPUT(name, unit, file) as sbl -bf takes it (association above).
   No file and no unit → stdout (writes via PrintWriter, :file stored as nil); a file → a PrintWriter. A pipe spec (\"|cmd\") is ERROR 160, as SPITBOL raises it.
   A file that cannot be opened fails, as in sbl -bf. Returns ε on success, nil on failure."
  [var-sym unit filename]
  (let [bad      #(throw (ex-info "inappropriate file specification for output" {:snobol/error 160}))
        [unit filename shared] (association :output unit filename bad)
        {nm :name opts :opts blank-in-name :bad} (file-spec (or filename ""))
        file-str (when-not (clojure.string/blank? nm) nm)
        _        (when blank-in-name (bad))
        ;; a file's stream is kept, for SET to move
        out (when (and file-str (not shared) (not (clojure.string/starts-with? file-str "|")))
              (try (java.io.FileOutputStream. (channel-file file-str) (contains? opts \a)) (catch Exception _ nil)))
        wtr (cond
              shared   nil
              file-str (if (clojure.string/starts-with? file-str "|")
                         (bad)
                         (when out (java.io.PrintWriter. (java.io.OutputStreamWriter. ^java.io.OutputStream out "ISO-8859-1"))))
              ;; stdout (or, with -f2, stderr): wrap the stream but mark :file nil so close-channel! won't close it
              :else    (java.io.PrintWriter. ^java.io.Writer (if (clojure.core/= 2 (get opts \f)) *err* *out*) true))]
    (cond
      shared (do (swap! <CHANNELS> assoc var-sym shared) ε)
      wtr
      (let [ch (cond-> {:type :output :unit unit :var var-sym :writer wtr :file file-str} out (assoc :stream out))]
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

;; "The file associated with the channel is positioned backwards one record. If the file is a binary (raw-mode) file, the file position is decremented by the record
;; size ... If an end-of-line character is found immediately preceding the current file position, it is ignored. The file is then scanned in reverse until another
;; end-of-line character is found. The file is then positioned just forward of the end-of-line character. BACKSPACE stops at beginning-of-file" (manual ch. 19,
;; BACKSPACE). As sbl -bf has it, an output file is positioned the same way and the next record written replaces the last, nothing cut off after it; a null
;; argument is "316 backspace argument is not a suitable name", a unit no INPUT or OUTPUT opened "317 backspace file does not exist", and a channel on standard
;; input or output "318 backspace file does not permit backspace".
(defn BACKSPACE
  ([] (BACKSPACE nil))
  ([unit]
   (let [fail (fn [n m] (throw (ex-info m {:snobol/error n})))
         _    (when (or (nil? unit) (clojure.core/= "" unit)) (fail 316 "backspace argument is not a suitable name"))
         ch   (get @<CHANNELS> (channel-key unit))
         _    (when-not ch (fail 317 "backspace file does not exist"))
         f    (when (:file ch) (channel-file (:file ch)))
         back (fn [p]
                (if-let [n (:raw ch)]
                  (max 0 (clojure.core/- p n))
                  (let [^bytes b (java.nio.file.Files/readAllBytes (.toPath ^java.io.File f))
                        eol?     (fn [i] (clojure.core/== 10 (aget b (dec i))))]
                    (loop [i (if (and (pos? p) (eol? p)) (dec p) p)] (if (and (pos? i) (not (eol? i))) (recur (dec i)) i)))))]
     (cond
       (and (clojure.core/= :input (:type ch)) f)
       (let [to  (back @(:at ch))
             in  (java.io.FileInputStream. ^java.io.File f)
             _   (.skip in to)
             ch' (assoc ch :reader (java.io.BufferedReader. (java.io.InputStreamReader. in "ISO-8859-1")) :at (atom to))]
         (try (.close ^java.io.BufferedReader (:reader ch)) (catch Exception _ nil))
         (swap! <CHANNELS> assoc (:var ch) ch' (:unit ch) ch')
         ε)
       (:stream ch)
       (let [fc (.getChannel ^java.io.FileOutputStream (:stream ch))]
         (.flush ^java.io.PrintWriter (:writer ch))
         (.position fc (long (back (.position fc))))
         ε)
       :else (fail 318 "backspace file does not permit backspace")))))
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
;; HOST() is "a 'host-information' string describing the system on which SPITBOL is executing", of the form computer type:operating system:version; HOST(0) is the
;; text of the -u option, "If -u was not specified, it returns the concatenation of all command line arguments" (manual ch. 19, HOST): sbl -bf joins the arguments after the
;; program file with blanks. HOST(-1, k) answers sbl x64's memory facts (osint/syshs.c): 0 the heap increment and 1 the heap's limit (sbl's defaults, -i1m and
;; -d128m, which the harness declares), 4 the stack's size less 400 (-s4m), 6 the word size; 2 and 3, the heap's base and top addresses, and 5 and 7, the stack in
;; use and its high-water mark, are what a JVM can say of them: no address (0), the heap's current size, and the frames on the thread's stack. k omitted is 0.
;; Another selector is ERROR 254, erroneous argument for host, as sbl -bf raises it for the sub-functions this engine does not have.
(defn HOST
  ([] (str "jvm:" (System/getProperty "os.name") ":SNOBOL4clojure 0.2.0"))
  ([i & more]
   (let [n   (SNOBOL4clojure.env/num i)
         bad #(throw (ex-info "erroneous argument for host" {:snobol/error 254}))]
     ;; As sbl -bf (x64) answers them: HOST(1, cmd) runs cmd in the shell, its output going where the program's goes, and returns the shell's wait status (an
     ;; exit code times 256, or the signal that ended it); HOST(2, n) the nth word of the command line, 0 the engine (here its jar), failing past the last;
     ;; HOST(3) the index of the program's first own argument; HOST(4, name) the environment variable name, failing when it is unset. A null or non-integer
     ;; n and a null name are 254.
     (cond
       (and (integer? n) (zero? n)) (str @SNOBOL4clojure.env/<HOST0>)
       (and (integer? n) (clojure.core/= 1 n))
       (let [cmd (str (or (first more) ""))]
         (if (clojure.core/= "" cmd)
           0
           (do (flush)
               (let [p    (-> (ProcessBuilder. ^java.util.List ["/bin/sh" "-c" cmd]) (.inheritIO) (.start))
                     code (long (.waitFor p))]
                 (if (clojure.core/> code 128) (clojure.core/- code 128) (clojure.core/* code 256))))))
       (and (integer? n) (clojure.core/= 2 n))
       (let [k (if (or (nil? (first more)) (clojure.core/= "" (first more))) 0 (SNOBOL4clojure.env/num (first more)))]
         (if (integer? k) (get @SNOBOL4clojure.env/<ARGV> k) (bad)))
       (and (integer? n) (clojure.core/= 3 n)) (long @SNOBOL4clojure.env/<ARGI>)
       (and (integer? n) (clojure.core/= 4 n))
       (let [nm (str (or (first more) ""))]
         (if (clojure.core/= "" nm) (bad) (System/getenv nm)))
       (and (integer? n) (clojure.core/= -1 n))
       (let [k (SNOBOL4clojure.env/num (first more))
             frames #(long (count (.getStackTrace (Thread/currentThread))))]
         (if-not (integer? k)
           (bad)
           (case (long k)
             0 1048576
             1 134217728
             2 0
             3 (.totalMemory (Runtime/getRuntime))
             4 (clojure.core/- 4194304 400)
             5 (frames)
             6 8
             7 (frames)
             (bad))))
       :else (bad)))))
;; SETEXIT(label) arms the label (a string or a NAME) as the error handler and returns the handler it replaces, as a NAME, or the null string when there was none.
;; The null string, or no argument, disarms. Anything else names a natural variable whose label is defined, else "187 setexit argument is not label name or null"
;; (sbl.min s_stx, gtnvr): a string folds as &CASE says at that moment (sbl -bf, under -f: with &CASE 1 SETEXIT('lab1') looks for LAB1), the name of a
;; variable does not, and a pattern or the name of an element is no variable. The system labels are always defined, in capitals (sbl -bf: SETEXIT(.CONTINUE)
;; and SETEXIT('RETURN') arm, SETEXIT('continue') is 187; END is END or end).
(def ^:private system-labels #{"ABORT" "CONTINUE" "SCONTINUE" "RETURN" "FRETURN" "NRETURN" "END" "end"})
(defn SETEXIT
  ([] (SETEXIT ε))
  ([label]
   (let [prev @env/<SETEXIT>
         s    (env/gtstg label 187)
         s    (if (and (string? label) (not (zero? (long @env/&CASE)))) (env/fold-case s) s)]
     (when-not (or (clojure.core/= "" s) (system-labels s) (@env/<STNO> (keyword s)))
       (throw (ex-info (env/error-message 187) {:snobol/error 187})))
     (reset! env/<SETEXIT> (when-not (clojure.core/= "" s) (keyword s)))
     (if prev (SNOBOL4clojure.env.NAME. (symbol (name prev))) ε))))
(defn STOPTR  [] nil)
(defn TRACE   [] nil)

;; ── Function definition stubs ─────────────────────────────────────────────────
(defn APPLY
  "APPLY(fname, arg1, ...) — call a named function by string/symbol."
  [fname & fargs]
  (let [f-sym (symbol (str fname))
        f-fn  (SNOBOL4clojure.env/$$fn f-sym)]
    (when (fn? f-fn)
      (apply f-fn fargs))))
(defn ARG
  "ARG(fname, n) — return the nth parameter of function fname (1-based) as a NAME.
   Returns nil (statement failure) if fname unknown or n out of range."
  [fname n]
  (when-let [meta (get @SNOBOL4clojure.env/<FDEFS>
                       (str fname))]
    (when-some [p (nth (:params meta) (dec (long n)) nil)]
      (SNOBOL4clojure.env.NAME. (symbol p)))))

(defn LOCAL
  "LOCAL(fname, n) — return the nth local variable of function fname (1-based) as a NAME.
   Returns nil (statement failure) if fname unknown or n out of range."
  [fname n]
  (when-let [meta (get @SNOBOL4clojure.env/<FDEFS>
                       (str fname))]
    (when-some [p (nth (:locals meta) (dec (long n)) nil)]
      (SNOBOL4clojure.env.NAME. (symbol p)))))

;; ── Table / array functions ───────────────────────────────────────────────────
(defn ITEM
  "ITEM(container, key...) — the element of a TABLE or an ARRAY, as container<key...> reads it: the value at the key (ε if absent from a table), failure for
   a subscript outside an array."
  [c & ks]
  (cond
    (SNOBOL4clojure.env/table? c) (SNOBOL4clojure.env/table-get c (first ks))
    (SNOBOL4clojure.env/array? c) (SNOBOL4clojure.env/array-get c (vec ks))
    :else nil))

(defn PROTOTYPE
  "PROTOTYPE(array): the prototype the array was made from (manual ch. 19). sbl -bf also answers for a table, with the number of its hash headers, an INTEGER (11 for
   TABLE()), and ignores a second argument."
  [x & _]
  (cond
    (SNOBOL4clojure.env/table? x) (:size (meta x) 11)
    (SNOBOL4clojure.env/array? x) (SNOBOL4clojure.env/array-prototype x)
    :else ""))

(defn- sort-key-compare
  "SPITBOL's sort order: two numbers compare as numbers, anything else as strings."
  [a b]
  (if (and (number? a) (number? b)) (compare a b) (compare (str a) (str b))))

;; SORT and RSORT take a table or an array. A table sorts as the two-column array of its entries (keys in column 1, values in column 2, in the order the keys were
;; first assigned); an array sorts its rows on column col (1 by default), a one-dimensional array being its own column. The sort is stable, ascending for SORT and
;; descending for RSORT, so entries that tie keep the order they had. The result is a new array.


(defn- sort-array
  "A new array with the rows of arr stably sorted on column col."
  [arr col descending?]
  (let [dims   (:dims arr)
        lo1    (first (first dims))
        hi1    (second (first dims))
        two-d? (clojure.core/= 2 (count dims))
        cols   (when two-d? (let [[lo hi] (second dims)] (range lo (inc hi))))
        rows   (for [i (range lo1 (inc hi1))]
                 (if two-d? (vec (for [j cols] (array-get arr [i j]))) [(array-get arr [i])]))
        pick   (fn [row] (nth row (min (dec (long col)) (dec (count row)))))
        cmp    (if descending? #(sort-key-compare %2 %1) sort-key-compare)
        sorted (sort-by pick cmp rows)
        out    (ARRAY (:proto arr))]
    (doseq [[i row] (map vector (range lo1 (inc hi1)) sorted)]
      (if two-d?
        (doseq [[j v] (map vector cols row)] (array-set out [i j] v))
        (array-set out [i] (first row))))
    out))

(defn- sort-any [A col descending?]
  (cond
    (table? A) (when-some [arr (table->array A)] (sort-array arr col descending?))
    (array? A) (sort-array A col descending?)
    :else nil))

(defn SORT
  ([A]     (SORT A 1))
  ([A col] (sort-any A (long (env/num col)) false)))

(defn RSORT
  ([A]     (RSORT A 1))
  ([A col] (sort-any A (long (env/num col)) true)))
