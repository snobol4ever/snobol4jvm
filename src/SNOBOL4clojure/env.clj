(ns SNOBOL4clojure.env
  ;; Global SNOBOL4 environment: keywords, constants, data types,
  ;; utility arithmetic aliases, and the DATATYPE dispatch table.
  ;; No dependencies on other SNOBOL4clojure namespaces.
  (:require [clojure.pprint  :as pp]
            [SNOBOL4clojure.errors :as errors])
  (:refer-clojure :exclude [= + - * / num]))

;; ── Utility arithmetic (avoid shadowing clojure.core in this ns) ─────────────
(defn Σ+       ([x y] (clojure.core/+' x y)) ([x] (clojure.core/+' x)))
(defn subtract ([x y] (clojure.core/-' x y)) ([x] (clojure.core/-' x)))
(defn multiply ([x y] (clojure.core/*' x y)) ([x] (clojure.core/*' x)))
(defn divide    [x y]
  (if (and (integer? x) (integer? y))
    (quot x y)                        ; integer / integer → truncate toward zero
    (clojure.core// x y)))            ; real arithmetic otherwise
(defn equal     [x y] (clojure.core/= x y))
(defn not-equal [x y] (clojure.core/not= x y))
(defn out      [item] (binding [pp/*print-right-margin* 2048, pp/*print-miser-width* 2000]
                        (pp/pprint item)) item)

;; ── SNOBOL4 constants ─────────────────────────────────────────────────────────
(def  ε          "")        ; null / empty string
(def  η          ##NaN)     ; undefined numeric

;; ── SNOBOL4 keywords (& variables) ───────────────────────────────────────────
(def  &ALPHABET  (atom (apply str (map char (range 256)))))   ; the 256-character string, codes 0..255 in order
(def  &ABEND     (atom 0))
(def  &ANCHOR    (atom 0))
(def  &CASE      (atom 0))   ; not 0: CODE and EVAL fold lower case names to upper case (a program's own -CASE card governs its source)
(def  &DIGITS    "0123456789")
(def  &DUMP      (atom 0))  ; 1, 2, and 3 levels
(def  &CODE       (atom 0))   ; the end-of-job code
(def  &COMPARE    (atom 0))
(def  &ERRLIMIT  (atom 0))
(def  &ERRTEXT   (atom ε))
(def  &ERRTYPE   (atom 0))
(def  &FTRACE    (atom 0))
(def  &FULLSCAN  (atom 1))
(def  &LASTNO    (atom 0))   ; the number of the statement executed before the current one
(def  &STNO      (atom 0))   ; the number of the statement being executed
(def  &FNCLEVEL  (atom 0))   ; how many user functions are active
(def  &FILE      (atom ε))   ; the file and line of the statement being executed, and of the one executed before it
(def  &LASTFILE  (atom ε))
(def  &LINE      (atom 0))
(def  &LASTLINE  (atom 0))
(def  &LCASE     "abcdefghijklmnopqrstuvwxyz")
(def  &INPUT      (atom 1))   ; 0 switches the INPUT variable off: a read gives the null string
(def  &MAXLNGTH  (atom 16777216))
(def  &OUTPUT     (atom 1))   ; 0 suppresses what is assigned to OUTPUT
(def  &PROFILE   (atom 0))
(def  &RTNTYPE   (atom ε))   ; how the last function returned: RETURN, FRETURN or NRETURN
(def  &TRACE     (atom 0))
(def  &TRIM      (atom 1))   ; 1 when a program starts, as in SPITBOL: input records lose their trailing blanks and tabs
(def  &STCOUNT   (atom 0))
(def  &STLIMIT   (atom 2147483647))
;; &IOCOMPAT: I/O argument convention selector (Sprint 25D).
;;   :catspaw  — INPUT('name', unit, length, 'file') — Catspaw/CSNOBOL4/SPITBOL (default)
;;   :sitbol   — INPUT('name', 'device', 'format')   — SITBOL/PDP-10 (no unit number)
(def  &IOCOMPAT  (atom :catspaw))
(def  &UCASE     "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;; The value each settable keyword holds when a program starts.
(def ^:private keyword-defaults
  {#'&ABEND 0, #'&ANCHOR 0, #'&CASE 0, #'&CODE 0, #'&COMPARE 0, #'&DUMP 0, #'&ERRLIMIT 0, #'&FTRACE 0, #'&FULLSCAN 1, #'&INPUT 1,
   #'&MAXLNGTH 16777216, #'&OUTPUT 1, #'&PROFILE 0, #'&TRIM 1, #'&TRACE 0,
   #'&STLIMIT 2147483647, #'&IOCOMPAT :catspaw})

(defn reset-keywords!
  "Give every settable keyword its starting value, as a fresh program has it."
  []
  (doseq [[v dflt] keyword-defaults] (reset! @v dflt)))

;; ── I/O channel atoms ────────────────────────────────────────────────────────
(def  INPUT$     (atom ε))
(def  OUTPUT$    (atom ε))
(def  TERMINAL$  (atom ε))

;; ── Named I/O channel registry (Sprint 25D) ──────────────────────────────────
;; Maps variable symbol → channel descriptor map:
;;   {:type    :input | :output
;;    :unit    integer unit number
;;    :reader  java.io.BufferedReader  (input channels)
;;    :writer  java.io.PrintWriter     (output channels)
;;    :file    string filename or nil (nil = stdin/stdout)}
;;
;; Also maps unit integer → same descriptor (for ENDFILE/REWIND by unit).
;;
;; Cleared by reset-runtime! between test runs.
(def <CHANNELS> (atom {}))

;; *file-dir*: the directory a relative file name in INPUT()/OUTPUT() resolves
;; against. nil = the process's working directory. A JVM cannot change its own
;; working directory, so a caller that must keep a program's files out of the
;; current one (the test suite) binds this to a scratch directory instead.
(def ^:dynamic *file-dir* nil)

;; <OPSYN>: operator/function synonym table (Sprint 25E).
;; Maps new-sym → {:target old-sym :arity n}
;;   :arity 0 = function synonym (no-op on arity, just alias)
;;   :arity 1 = unary operator redefinition
;;   :arity 2 = binary operator redefinition
(def <POS> (atom {}))   ; statement number -> [file line]: where the statement stands in the source, which &FILE and &LINE read
(def <OBJNO> (atom 0))   ; how many arrays, tables and DATA objects the program has made: the number a dump gives each, #n
(defn next-serial! [] (swap! <OBJNO> inc))
(def <PRIMS-READ> (atom #{}))   ; the primitive pattern variables (BAL, ARB, ...) the program has read, which a dump lists as SPITBOL lists them
(def <DIAGNOSTICS> (atom []))   ; the compile-time errors of the last source compiled, as [line code] pairs
(def <SOURCE> (atom nil))   ; the last source compiled, its -INCLUDEs spliced in, as {:text t :stmts its [text line] statements :cards the -INCLUDE lines}, for the listing
;; True while a pattern match is in progress, in the functions it calls too: an execution error is then not fatal (match.clj matching, runtime.clj trap)
(def ^:dynamic *matching* false)
;; Where an execution error goes (operators.clj error-decided, after sbl.min's error section, which keeps them as flptr, flprt, stage and gtcef): the depth of the
;; innermost failure point (the statement, a negation's operand, a selection's alternative but its last, the expression EVAL evaluates, a function's body); the
;; depth of the innermost function's body, below every failure point in the main program; the stage, :ee while EVAL evaluates an expression and :xt otherwise;
;; and the depth the last EVAL was called at
(def ^:dynamic *fp* 0)
(def ^:dynamic *flprt* -1)
(def <STAGE> (atom :xt))
(def <GTCEF> (atom 0))
(def <ERRFT> (atom 0))   ; the fatal errors so far (sbl.min errft): at the third the program ends whatever &ERRLIMIT is
(def <HOST0> (atom ""))   ; what HOST(0) returns: the -u text, or else the program's own arguments joined by blanks
(def <ARGV>  (atom []))   ; the command line HOST(2, n) answers from: the engine's jar, then -main's arguments
(def <ARGI>  (atom 0))    ; what HOST(3) returns: the index in <ARGV> of the program's first own argument
(def <LASTOUT> (atom nil))   ; the last value assigned to OUTPUT, which a dump lists as the variable OUTPUT
(def <ASSOC-LAST> (atom {}))   ; an associated variable (INPUT, OUTPUT, a channel's) -> the last record read or written through it, which a dump lists as its value
(def <SETEXIT> (atom nil))   ; the label SETEXIT named (a keyword), or nil when no handler is armed
(def <DISPNO> (atom {}))   ; statement key (the number a statement is stored under) -> the number SPITBOL gives it, which counts blank lines
(def BLANKS   (atom 0))    ; the blank lines of the code loaded so far: they take numbers and are never run
(def <TRACES> (atom {}))   ; the TRACE registrations: [name kind] -> options; a program's own state, reset with the rest
(def <OPSYN> (atom {}))

;; ── Runtime statement table (used by compiler + runtime) ─────────────────────
(def  STNO   (atom 0))
(def <STNO>  (atom {}))
(def <LABL>  (atom {}))
(def <CODE>  (atom {}))
;; the statement that closes a block CODE() compiles at run time: reached, it is counted and numbered as any statement is, and it ends the program
(def block-end [::block-end])

;; ── User-defined function registry ───────────────────────────────────────────
;; Stores DEFINE'd closures keyed by uppercased function name (string).
;; This is separate from the user-namespace variable of the same name, which
;; serves as the result slot.  Keeping them separate allows recursive calls to
;; find the function even while the result slot holds ε or an intermediate value.
(def <FUNS>  (atom {}))

;; SPITBOL's own functions (manual ch. 19, Function summary): sbl -bf answers "248 attempted redefinition of system function" when a program would UNLOAD one.
(def system-functions
  #{"ANY" "APPLY" "ARBNO" "ARG" "ARRAY" "ATAN" "BACKSPACE" "BREAK" "BREAKX" "CHAR" "CHOP" "CLEAR" "CODE" "COLLECT" "CONVERT" "COPY" "COS" "DATA" "DATATYPE"
    "DATE" "DEFINE" "DETACH" "DIFFER" "DUMP" "DUPL" "EJECT" "ENDFILE" "EQ" "EVAL" "EXIT" "EXP" "FENCE" "FIELD" "GE" "GT" "HOST" "IDENT" "INPUT" "INTEGER"
    "ITEM" "LE" "LEN" "LEQ" "LGE" "LGT" "LLE" "LLT" "LN" "LNE" "LOAD" "LOCAL" "LPAD" "LT" "NE" "NOTANY" "OPSYN" "OUTPUT" "POS" "PROTOTYPE" "REMDR"
    "REPLACE" "REVERSE" "REWIND" "RPAD" "RPOS" "RSORT" "RTAB" "SET" "SETEXIT" "SIN" "SIZE" "SORT" "SPAN" "SQRT" "STOPTR" "SUBSTR" "TAB" "TABLE" "TAN"
    "TIME" "TRACE" "TRIM" "UNLOAD"})

;; Registry of function parameter and local-variable metadata.
;; Populated by DEFINE; read by ARG() and LOCAL().
;; Shape: {"FNAME" {:params ["P1" "P2"] :locals ["L1"]}}
(def <FDEFS> (atom {}))

;; ── NAME type: mutable named reference (SNOBOL4 . operator) ──────────────────
(definterface &NAME (n []) (n [_]))
;; Two NAMEs are equal when they name the same variable or element, as IDENT
;; and DIFFER read them: .A is .A and is not .B or the string "A". As text a
;; NAME is the name of its variable, which is what OUTPUT, SIZE and a
;; concatenation read.
(deftype NAME [^:unsynchronized-mutable n] &NAME
  (n [this]  n)
  (n [this _] (set! n _))
  Object
  (equals   [this o] (and (instance? &NAME o) (clojure.core/= n (.n ^&NAME o))))
  (hashCode [this]   (hash n))
  (toString [this]   (str n)))

;; The name of an expression, .*E for an E that is no variable: a pseudo-variable, a new one each time, whose value is E evaluated when it is read and which is
;; assigned through E evaluated by name (sbl.min evblk: "an expression can be used in any position which would normally expect a name ... this corresponds to
;; the creation of a pseudo-variable"). Its identity is its own: two are never IDENT, and it keys a table by itself.
(deftype ExprVar [ir])

(defrecord NameResult [target])   ; what a call that returned with NRETURN gives: the variable (a symbol) or element (T k...) it names; used as a value it is that variable's value

;; Upper-case S outside its quoted strings: the names of source compiled with case folding on (-CASE 1, or &CASE not 0 for CODE and EVAL). Only ASCII letters change.
(defn fold-case [^String s]
  (let [sb (StringBuilder.)]
    (loop [i 0 q nil]
      (when (clojure.core/< i (count s))
        (let [c (.charAt s i)]
          (cond
            q                      (do (.append sb c) (recur (inc i) (if (clojure.core/= c q) nil q)))
            (or (clojure.core/= c \') (clojure.core/= c \")) (do (.append sb c) (recur (inc i) c))
            :else                  (do (.append sb (if (and (clojure.core/>= (int c) 97) (clojure.core/<= (int c) 122)) (Character/toUpperCase c) c)) (recur (inc i) nil))))))
    (str sb)))

(defn name-string
  "The text of a name argument: a NAME (.F) is the name of its variable,
   anything else its string."
  [x]
  (if (instance? NAME x) (str (.n ^NAME x)) (str x)))

;; ── SPITBOL's real conversion ─────────────────────────────────────────────────
;; The oracle reads and writes reals with its own arithmetic (sbl.min gtnum and gtstg; the x64 build computes in SSE doubles, rounding to nearest, with results
;; below the least normal flushed to zero, MXCSR 0x9fc0), and about one real in twenty then reads or prints otherwise than correct rounding gives. These two
;; follow it step for step; on 8,944 random literals and products they print what sbl -bf prints.
(def ^:private ten-powers (double-array [1.0 1e1 1e2 1e3 1e4 1e5 1e6 1e7 1e8 1e9 1e10]))   ; reav1 .. reatt, the powers of ten table
(def ^:private gts-scale (reduce (fn [a _] (clojure.core/* a 10.0)) 1.0 (range 15)))                  ; gtssc, 10**cfp_s, cfp_s = 15 significant digits
(def ^:private gts-round (clojure.core// 0.5 gts-scale))                                             ; gtsrn, the rounding bias
(defn- ftz ^double [^double x] (if (< (Math/abs x) Double/MIN_NORMAL) (clojure.core/* x 0.0) x))

(defn parse-real
  "The real sbl.min gtnum makes of s (an optional sign, digits, a point or an exponent marked e, E, d or D or both, blanks at the ends), or nil when s has
   no digits, neither a point nor an exponent, or a step overflows. The digits before the point are an integer while it fits in 64 bits, the rest go in as r*10 + digit, and the
   value is scaled by steps of 10**10 and then a powers-of-ten table entry, each step rounded."
  [s]
  (when-let [[_ sg ^String ip point ex] (re-matches #"\s*([+-]?)(\d*)(?:\.(\d*))?(?:[eEdD]([+-]?\d+))?\s*" (str s))]
    (let [^String fp (or point "")
          ;; the integer part, gathered negative as cvm does, until a digit would overflow it
          [n i] (loop [i 0 n 0]
                  (if (< i (.length ip))
                    (if-some [m (try (Math/subtractExact (Math/multiplyExact (long n) 10) (long (clojure.core/- (int (.charAt ip i)) 48))) (catch ArithmeticException _ nil))]
                      (recur (inc i) m)
                      [n i])
                    [n i]))
          digit (fn [^double r ^Character c] (clojure.core/+ (clojure.core/* r 10.0) (double (clojure.core/- (int c) 48))))
          r     (reduce digit (clojure.core/- (double n)) (concat (subs ip i) fp))
          sc    (try (clojure.core/- (.length fp) (Long/parseLong (or ex "0"))) (catch NumberFormatException _ nil))]
      (when (and sc (pos? (clojure.core/+ (.length ip) (.length fp))) (or point ex))
        (let [r (loop [r (double r) sc (long sc)]
                  (cond
                    (or (Double/isInfinite r) (zero? r)) r
                    (> sc 10)  (recur (ftz (clojure.core// r 1e10)) (clojure.core/- sc 10))
                    (< sc -10) (recur (clojure.core/* r 1e10) (clojure.core/+ sc 10))
                    (pos? sc)  (ftz (clojure.core// r (aget ^doubles ten-powers sc)))
                    (neg? sc)  (clojure.core/* r (aget ^doubles ten-powers (clojure.core/- sc)))
                    :else      r))]
          (when-not (Double/isInfinite r) (if (clojure.core/= "-" sg) (clojure.core/- r) r)))))))

;; ── Numeric conversion ────────────────────────────────────────────────────────
(defn num [x]
  (cond
    (nil? x)     0                        ; unset variable = 0 in arithmetic context
    (double? x)  x
    (integer? x) x                        ; preserve integer type
    true (let [s (clojure.string/trim (str x))]
           (if (clojure.core/= s "")
             0                            ; empty string = 0 in arithmetic context
             ;; an integer that fits, else a real as gtnum reads one; anything else is not numeric (##NaN, the operator's error), as sbl -bf has it
             (or (try (Long/parseLong s)
                   (catch NumberFormatException _ nil))
                 (parse-real s)
                 ##NaN)))))

(defn  ncvt [x] (list 'num x))
(defn  scvt [x] (list 'str x))

;; ── SNOBOL4 string conversion ────────────────────────────────────────────────
(defn real-str
  "A real as sbl.min gtstg prints it: its absolute value scaled into [0.1, 1) by steps of 10**10 and a powers-of-ten table entry, 0.5e-15 added, and 15 digits
   truncated from it, trailing zeros dropped; fixed point when the exponent is 0 to 15, with a trailing \".\" when integral (2.0 -> \"2.\"), otherwise
   0.<digits>e<sign><exponent> (1e20 -> \"0.1e+21\", 0.001 -> \"0.1e-2\"); zero is \"0.\"."
  [x]
  (let [x (double x)]
    (cond
      (or (Double/isNaN x) (Double/isInfinite x)) (str x)
      (zero? x) "0."
      :else
      (let [[a e] (loop [a (Math/abs x) e 0] (if (< a 0.1) (recur (clojure.core/* a 1e10) (clojure.core/- e 10)) [a e]))
            [a e] (if (< (double a) 1.0)
                    [a e]
                    (let [[a e] (loop [a (double a) e (long e)] (if (>= a 1e10) (recur (clojure.core// a 1e10) (clojure.core/+ e 10)) [a e]))
                          k     (loop [k 1] (if (>= (double a) (aget ^doubles ten-powers k)) (recur (inc k)) k))]
                      [(clojure.core// (double a) (aget ^doubles ten-powers k)) (clojure.core/+ (long e) k)]))
            a     (clojure.core/+ (double a) (double gts-round))
            [a e] (if (< a 1.0) [a e] [(clojure.core// a 10.0) (inc (long e))])
            n     (long (clojure.core/* (double a) (double gts-scale)))
            e     (long e)
            fixed (clojure.core/<= 0 e 15)
            nd    (if fixed (clojure.core/- 15 e) 15)
            p     (long (Math/pow 10.0 nd))
            frac  (if (zero? nd) "" (clojure.string/replace (format (str "%0" nd "d") (rem n p)) #"0+$" ""))]
        (str (when (neg? x) "-") (quot n p) "." frac (when-not fixed (str "e" (if (neg? e) "-" "+") (Math/abs e))))))))

(defn sstr
  "A SNOBOL4 value as the text it becomes in output and concatenation:
   nil is the null string, a real is formatted by real-str, anything else is str."
  [x]
  (cond (nil? x) "" (double? x) (real-str x) :else (str x)))

;; ── User variable store — Scope 1: SNOBOL4 program variables ─────────────────
;;
;; SNOBOL4 has four distinct namespaces:
;;   1. Variables  — stored here in <VARS>   (atom: symbol → value)
;;   2. Functions  — stored in <FUNS>        (atom: string → fn)
;;   3. Labels     — stored in <LABL>/<STNO> (atom: keyword/string → int)
;;   4. &-Keywords — defs in env.clj         (looked up directly by symbol)
;;
;; Using a plain Clojure map atom means user variables like INTEGER or REAL
;; can NEVER collide with Clojure or engine symbols — they live in their own
;; scope, not in any Clojure namespace.  This is the natural Lisp approach.
;;
;; GLOBALS now only resets the runtime state atoms — no namespace arg needed.
;; The ns argument is accepted for backwards compatibility but ignored.
(def <VARS> (atom {}))

(defn GLOBALS
  "Reset ALL SNOBOL4 runtime state for a fresh program run.
   Clears variables, functions, code/label tables, I/O channels, and operator synonyms.
   The ns argument is accepted for API compatibility but ignored."
  ([]
   (reset! <VARS>    {})
   (reset! <FUNS>    {})
   (reset! <FDEFS>   {})
   ;; Close open file channels before discarding them
   (doseq [[_ ch] @<CHANNELS>]
     (try
       (when-let [rdr (:reader ch)] (.close ^java.io.BufferedReader rdr))
       (when-let [wtr (:writer ch)] (.close ^java.io.PrintWriter    wtr))
       (catch Exception _ nil)))
   (reset! <CHANNELS> {})
   (reset! <OPSYN>    {})
   (reset! <TRACES>   {})
   (reset! <DISPNO>   {})
   (reset! <SETEXIT>  nil)
   (reset! <STAGE>    :xt)
   (reset! <GTCEF>    0)
   (reset! <ERRFT>    0)
   (reset! <OBJNO>    0)
   (reset! <LASTOUT>  nil)
   (reset! <ASSOC-LAST> {})
   (reset! <HOST0>    "")
   (reset! <ARGV>     [])
   (reset! <ARGI>     0)
   (reset! <PRIMS-READ> #{})
   (reset! &RTNTYPE   ε)
   (reset! &ERRTYPE   0)
   (reset! &ERRTEXT   ε)
   (reset! <POS>      {})
   (reset! &FILE ε)
   (reset! &LASTFILE ε)
   (reset! &LINE 0)
   (reset! &LASTLINE 0)
   (reset! BLANKS     0)
   (reset! STNO    0)
   (reset! <STNO>  {})
   (reset! <LABL>  {})
   (reset! <CODE>  {})
   (reset! &STNO 0)
   (reset! &FNCLEVEL 0)
   (reset! &LASTNO 0)
   (reset-keywords!))
  ([_ns] (GLOBALS)))

;; ── Keyword assignment ───────────────────────────────────────────────────────
;; The text of SPITBOL's own error n, as sbl -bf writes it (Appendix D in lower case), or nil for a number outside SPITBOL's. The manual's table lacks 274 to 280 (its
;; extraction took the page number 274 and two more for entries), so the one of those sbl was seen to write is given here.
(declare error-text)
(defn error-message "SPITBOL's text for its error n, in lower case as sbl -bf writes it, or nil." [n] (error-text n))
(def ^:private error-text
  (merge (into {} (for [[n m] errors/error-messages :when (not (#{274 276 278} n))] [n (clojure.string/lower-case m)]))
         {274 "value assigned to keyword fullscan is zero"}))

;; A value as SPITBOL's gtstg converts it to a string: a string, an integer, a real, or the name of a natural variable, which is its name ("convert a name
;; (only possible if natural var)", sbl.min gtstg); any other value is SPITBOL's error n
(defn gtstg ^String [x n]
  (cond (string? x)  x
        (nil? x)     ""
        (char? x)    (str x)
        (integer? x) (str x)
        (double? x)  (real-str x)
        (and (instance? NAME x) (symbol? (.n ^NAME x))) (name (.n ^NAME x))
        :else (throw (ex-info (error-message n) {:snobol/error n}))))

;; &K = V, with SPITBOL's errors: an unknown keyword is 251, a value that is not
;; an integer is 208, a protected keyword is 209. Assigning &ERRTYPE raises the
;; error it names.
(defn assign-keyword! [k v]
  (let [kw (clojure.string/upper-case (name k))
        vr (ns-resolve (the-ns 'SNOBOL4clojure.env) (symbol (str "&" kw)))]
    (when-not vr
      (throw (ex-info "keyword operand is not name of defined keyword" {:snobol/error 251})))
    ;; &ERRTEXT is the one keyword that takes a string (manual ch. 14, Replacement field; ch. 16): what is assigned is kept as a string
    (if (clojure.core/= kw "ERRTEXT")
      (reset! &ERRTEXT (sstr v))
    (let [n (if (integer? v) v (num v))]
      (when (and (double? n) (Double/isNaN n))
        (throw (ex-info "keyword value assigned is not integer" {:snobol/error 208})))
      (let [n (long n)]
        (cond
          ;; "&ERRTYPE may be assigned a value; in this case, an immediate error is signaled", with SPITBOL's text for its own numbers and, for another number, the
          ;; string the program assigned to &ERRTEXT (manual ch. 16)
          (clojure.core/= kw "ERRTYPE")
          (throw (ex-info (or (error-text n) (str @&ERRTEXT)) {:snobol/error n}))
          ;; every settable keyword but &STLIMIT takes 0 to 16777216 (sbl -bf: "210 keyword value assigned is negative or too large"); &PROFILE takes 0 and 1, and 2
          ;; while it is 1 is "268 inconsistent value assigned to keyword profile"; &MAXLNGTH is at least 1024 ("287 value assigned to keyword maxlngth is too small")
          (and (clojure.core/= kw "PROFILE") (clojure.core/= n 2) (clojure.core/= 1 (long @&PROFILE)))
          (throw (ex-info "inconsistent value assigned to keyword profile" {:snobol/error 268}))
          (and (not (clojure.core/= kw "STLIMIT")) (or (contains? keyword-defaults vr) (clojure.core/= vr #'&TRACE))
               (or (neg? n) (clojure.core/> n 16777216) (and (clojure.core/= kw "PROFILE") (clojure.core/> n 1))))
          (throw (ex-info "keyword value assigned is negative or too large" {:snobol/error 210}))
          (and (clojure.core/= kw "MAXLNGTH") (clojure.core/< n 1024))
          (throw (ex-info "value assigned to keyword maxlngth is too small" {:snobol/error 287}))
          ;; "Setting &FULLSCAN to 0 is an error" (manual ch. 16): the Quickscan heuristics are not supported
          (and (clojure.core/= kw "FULLSCAN") (zero? n))
          (throw (ex-info "value assigned to keyword fullscan is zero" {:snobol/error 274}))
          (or (contains? keyword-defaults vr) (clojure.core/= vr #'&TRACE))
          (do (reset! @vr n) n)
          :else
          (throw (ex-info "keyword in assignment is protected" {:snobol/error 209}))))))))

;; ── Variable write / read ─────────────────────────────────────────────────────
;; "These variables contain the primitive patterns of the same name" (manual ch. 18), and SPITBOL protects them: an assignment to one is "042 attempt to change
;; value of protected variable" (sbl -bf: ARB = 'Q', BAL = P and FENCE = 'f' are 42), so the compiler may take their values as constants.
(def protected-variables '#{ABORT ARB BAL FAIL FENCE REM SUCCEED})

(defn snobol-set!
  "Assign val to the SNOBOL4 user variable named by sym."
  [sym val]
  (let [s (symbol (name sym))]
    (when (protected-variables s) (throw (ex-info (error-message 42) {:snobol/error 42})))
    (swap! <VARS> assoc s val))
  val)

(defn reference
  "Return the current value of SNOBOL4 user variable N, or nil if unbound."
  [N]
  (get @<VARS> (symbol (name N))))

;; "If s is omitted, then the null string is assigned to all user variables in the system. When s is present, it should be a list of variable names separated by
;; commas. These variables will not be affected by the CLEAR function" (manual ch. 19, CLEAR). An output-associated variable is null too, and nothing is written;
;; the names are trimmed and keep their case, and s is converted to a string, so .X names X (sbl -bf).
(defn clear-vars! [s]
  (let [kept (into #{} (comp (map clojure.string/trim) (remove empty?) (map symbol)) (clojure.string/split (sstr s) #","))
        null (fn [m] (reduce-kv (fn [m k _] (if (kept k) m (assoc m k ""))) m m))]
    (swap! <VARS> null)
    (swap! <ASSOC-LAST> null)
    ""))

;; Standard input, one reader for the INPUT variable and every channel opened on it ([-f0], or no file), so a record read through one is not read again through
;; another and none reads ahead of the others; it follows the *in* of the run (a test binds its own).
(def ^:private stdin-cache (atom nil))
(defn stdin-reader ^java.io.BufferedReader []
  (let [in *in* [k r] @stdin-cache]
    (if (identical? k in)
      r
      (let [r (if (instance? java.io.BufferedReader in) in (java.io.BufferedReader. ^java.io.Reader in))]
        (reset! stdin-cache [in r])
        r))))

;; An input record as the program reads it: at most 1024 characters, a longer line losing the rest of its length (it is not read as the next record), and
;; while &TRIM is not 0 the blanks and tabs at its end dropped.
(def ^:private record-length 1024)

(defn input-record
  ([line] (input-record line nil))
  ([line maxlen]
  (let [s (str line)
        n (or maxlen record-length)
        s (if (clojure.core/> (count s) n) (subs s 0 n) s)]
    (if (zero? (long @&TRIM)) s (clojure.string/replace s #"[ \t]+$" "")))))

;; A raw-mode read (-r<n>): up to n characters as they come, end-of-line characters among them; nil (failure) only when none are left.
(defn- read-raw [^java.io.Reader rdr n]
  (let [buf (char-array n)]
    (loop [k 0]
      (if (clojure.core/< k n)
        (let [r (.read rdr buf k (clojure.core/- n k))]
          (if (neg? r) (when (pos? k) (String. buf 0 k)) (recur (clojure.core/+ k r))))
        (String. buf 0 k)))))

;; A line as BufferedReader.readLine reads it (ended by \n, \r or \r\n, none of them part of it; nil at the end of the input), adding to the channel's position the
;; characters it takes, its end among them. A file is read one byte to a character, so the count is the byte position SET answers.
(defn- read-line-counted [^java.io.BufferedReader rdr at]
  (let [sb (StringBuilder.)]
    (loop [n 0]
      (let [c (.read rdr)]
        (cond
          (neg? c)               (do (swap! at clojure.core/+ n) (when (pos? n) (str sb)))
          (clojure.core/== c 10) (do (swap! at clojure.core/+ n 1) (str sb))
          (clojure.core/== c 13) (do (.mark rdr 1)
                                     (if (clojure.core/== (.read rdr) 10) (swap! at clojure.core/+ n 2) (do (.reset rdr) (swap! at clojure.core/+ n 1)))
                                     (str sb))
          :else                  (do (.append sb (char c)) (recur (inc n))))))))

(defn channel-read
  "The next record of input channel ch, as it comes (nil at its end), counted into the channel's position (:at) when it keeps one."
  [ch]
  (let [rdr ^java.io.BufferedReader (:reader ch)
        at  (:at ch)]
    (if-let [n (:raw ch)]
      (let [s (read-raw rdr n)] (when (and s at) (swap! at clojure.core/+ (count s))) s)
      (if at (read-line-counted rdr at) (.readLine rdr)))))

;; What a name calls: the program's own variable or, failing that, the engine's built-in of that name (a function among them). INVOKE resolves a call with it.
(declare $$)
(defn $$fn [N]
  (let [sym (symbol (name N))]
    (if-let [v (find @<VARS> sym)]
      (val v)
      (let [resolved (ns-resolve (find-ns 'SNOBOL4clojure.core) sym)]
        (if (instance? clojure.lang.Var resolved) (var-get resolved) ($$ N))))))

(defn $$ [N]
  (let [sym (symbol (name N))]
    (cond
      ;; Named input channel: var registered → read from its BufferedReader
      (clojure.core/= :input (:type (get @<CHANNELS> sym)))
      (let [ch  (get @<CHANNELS> sym)
            line (try (channel-read ch) (catch Exception _ nil))
            v    (cond (nil? line) nil                       ; nil = EOF = statement fails (:F branch)
                       (:raw ch)   line                      ; a raw read is the data as it is
                       :else       (input-record line (:maxlen ch)))]
        (when (some? v) (swap! <ASSOC-LAST> assoc sym v))
        v)

      ;; Plain INPUT variable (no channel): read from *in* (stdin)
      (clojure.core/= sym 'INPUT)
      (if (zero? (long @&INPUT))
        ε                                                   ; &INPUT = 0 switches the variable off: a read gives the null string
        (let [line (try (.readLine (stdin-reader)) (catch Exception _ nil))
              v    (when-not (nil? line) (input-record line))]
          (when (some? v) (swap! <ASSOC-LAST> assoc 'INPUT v))
          v))   ; nil = EOF = statement fails, as SPITBOL does

      ;; An output-associated variable, OUTPUT among them, reads the value last assigned to it ("the string assigned to the output variable is written", manual
      ;; ch. 19, INPUT/OUTPUT), as sbl -bf has it; the value is the one <ASSOC-LAST> keeps for the dump. OUTPUT is not set as a variable, which would hide the function.
      (or (clojure.core/= sym 'OUTPUT) (clojure.core/= :output (:type (get @<CHANNELS> sym))))
      (get @<ASSOC-LAST> sym ε)

      :else
      ;; Scope chain: user <VARS> first, then engine builtins (read-only).
      ;; A user variable named INTEGER shadows the built-in INTEGER predicate —
      ;; correct SNOBOL4 semantics.  Engine builtins are never written here.
      (if-let [v (find @<VARS> sym)]
        (val v)
        ;; a built-in function's name is no variable: an unset TRIM is the null string. The variables that hold a value of their own are the primitive patterns
        ;; ("These variables contain the primitive patterns of the same name", manual ch. 18: ABORT, ARB, BAL, FAIL, FENCE, REM, SUCCEED). No other unset name
        ;; reads the engine's own definitions: $('&ANCHOR') is the variable of that name, null until assigned, and not the keyword (sbl -bf)
        (if (#{'ABORT 'ARB 'BAL 'FAIL 'FENCE 'REM 'SUCCEED} sym)
          (let [resolved (ns-resolve (find-ns 'SNOBOL4clojure.core) sym)
                v        (when (instance? clojure.lang.Var resolved) (var-get resolved))]
            (if (nil? v)
              ε
              (do (when-not (clojure.core/= sym 'FENCE) (swap! <PRIMS-READ> conj sym)) v)))
          ε)))))

;; ── Variable snapshot ────────────────────────────────────────────────────────
(defn snapshot!
  "Return a sorted map of all currently-set SNOBOL4 user variables → values.
   Call after a :step-limit stop to inspect full program state.
   Also useful for post-mortem debugging: (snapshot!) after any RUN."
  []
  (into (sorted-map) @<VARS>))

;; ── Arrays and Tables ─────────────────────────────────────────────────────────

;; SnobolArray: multi-dimensional, integer-subscripted, bounds-checked.
;; dims  = vector of [lo hi] pairs, one per dimension
;; dflt  = default value for unset slots
;; data  = atom wrapping a map from index-tuple (vector of ints) → value
(defrecord SnobolArray [dims dflt data])

(defn- parse-array-proto
  "Parse a SNOBOL4 array prototype string into a vector of [lo hi] pairs.
   Accepts integer N (meaning [1,N]) or string '[-]m:n,...'.
   Returns nil on any parse error."
  [proto]
  (let [s (str proto)]
    (try
      (let [parts (clojure.string/split s #",")]
        (mapv (fn [part]
                (let [part (clojure.string/trim part)]
                  (if (re-find #":" part)
                    (let [[lo hi] (clojure.string/split part #":" 2)
                          lo (Long/parseLong (clojure.string/trim lo))
                          hi (Long/parseLong (clojure.string/trim hi))]
                      (when (> lo hi) (throw (ex-info "lo>hi" {})))
                      [lo hi])
                    (let [n (Long/parseLong part)]
                      (when (<= n 0) (throw (ex-info "size<=0" {})))
                      [1 n]))))
              parts))
      (catch Exception _ nil))))

(defn ARRAY
  "ARRAY(proto) or ARRAY(proto, default).
   proto is an integer N → dimension [1..N], or a string like '-5:10,3:5,20'.
   Returns a SnobolArray, or throws on invalid proto."
  ([proto] (ARRAY proto ε))
  ([proto dflt]
   (let [dims (parse-array-proto proto)]
     (when (nil? dims) (throw (ex-info "ARRAY: invalid prototype" {:proto proto})))
     ;; :proto keeps the prototype as written, for PROTOTYPE
     (assoc (->SnobolArray dims dflt (atom {})) :proto (clojure.string/trim (str proto)) :serial (next-serial!)))))

;; A TABLE is an atom holding a map, with the order its keys were first assigned kept beside it (in the atom's metadata), because SPITBOL lists a table's
;; entries in that order (CONVERT to an ARRAY, SORT, RSORT) and a plain map does not keep it.
;; TABLE(i, x, arg): i hash headers (11 when omitted), x ignored, and arg "a value which is to be returned instead of the default null string when table look-up is performed
;; using a key which has not been entered into the table" (manual ch. 19, TABLE). A look-up does not enter the key.
(defn- new-table ([size] (new-table size ε)) ([size dflt] (atom {} :meta {:order (atom {}) :size size :serial (next-serial!) :dflt dflt})))
(defn- table-size [n] (let [k (num n)] (cond (and (integer? k) (pos? k)) k (and (double? k) (pos? k)) (long k) :else 11)))   ; TABLE() and TABLE(,..) are TABLE(11)   ; TABLE() is TABLE(11), the size a dump shows
(defn TABLE     ([]      (new-table 11))
                ([n]     (new-table (table-size n)))
                ([n _x]  (new-table (table-size n)))
                ([n _x d] (new-table (table-size n) (if (nil? d) ε d))))
(defn SET       [] (hash-set))

(defn table?    [x] (and (instance? clojure.lang.IDeref x)
                         (map? @x)))
(declare object-identity?)
;; A key that is the object itself: a pattern, an expression or the name of a keyword keys a table by its identity (object-identity?)
(deftype ObjKey [v]
  Object
  (equals   [_ o] (and (instance? ObjKey o) (identical? v (.v ^ObjKey o))))
  (hashCode [_]   (System/identityHashCode v)))
(defn- table-key [k] (if (object-identity? k) (ObjKey. k) k))

(defn table-get [t k]        (get @t (table-key k) (:dflt (meta t) ε)))
(defn table-set [t k v]
  (let [k (table-key k)]
    (when-let [o (:order (meta t))]
      (swap! o (fn [m] (if (contains? m k) m (assoc m k (count m))))))
    (swap! t assoc k v))
  v)

(defn table-entries
  "The [key value] entries of a table in the order the keys were first assigned."
  [t]
  (let [order (if-let [o (:order (meta t))] @o {})]
    (map (fn [[k v]] [(if (instance? ObjKey k) (.v ^ObjKey k) k) v])
         (sort-by (fn [[k _]] (get order k Long/MAX_VALUE)) (seq @t)))))

(defn array?    [x] (instance? SnobolArray x))

(defn- array-subscript
  "A subscript as the integer SPITBOL converts it to: a numeric string reads as
   its number, a real is truncated, the null string is 0. Anything else is
   ERROR 238."
  [i]
  (if (integer? i)
    i
    (let [n (num i)]
      (if (and (double? n) (Double/isNaN n))
        (throw (ex-info "array subscript is not integer" {:snobol/error 238}))
        (long n)))))

(defn- array-check-bounds
  "Returns nil (failure) if idx-vec is out of bounds, else the normalised key."
  [^SnobolArray arr idx-vec]
  (let [dims    (:dims arr)
        idx-vec (mapv array-subscript idx-vec)]
    (when (clojure.core/= (count idx-vec) (count dims))
      (when (every? true?
                    (map (fn [[lo hi] i]
                           (and (integer? i) (>= i lo) (<= i hi)))
                         dims idx-vec))
        idx-vec))))

(defn array-in-bounds? "True when idx-vec names an element of arr." [arr idx-vec] (some? (array-check-bounds arr idx-vec)))

(defn array-get
  "Read arr at idx-vec (vector of ints). Returns nil on bounds failure."
  [^SnobolArray arr idx-vec]
  (when-let [k (array-check-bounds arr idx-vec)]
    (get @(:data arr) k (:dflt arr))))

(defn array-set
  "Write arr at idx-vec. Returns nil on bounds failure, value on success."
  [^SnobolArray arr idx-vec v]
  (when-let [k (array-check-bounds arr idx-vec)]
    (swap! (:data arr) assoc k v)
    v))

(defn array-prototype
  "Return the PROTOTYPE of a SnobolArray: the count, an integer, for an array
   made from a bare count, as SPITBOL returns it; otherwise the prototype string."
  [^SnobolArray arr]
  (let [proto (or (:proto arr)
                  (clojure.string/join "," (map (fn [[lo hi]] (str lo ":" hi)) (:dims arr))))]
    (if (re-matches #"[+]?[0-9]+" proto)
      (second (first (:dims arr)))
      proto)))


;; ── SNOBOL4 control-flow signals ────────────────────────────────────────────
;; Used to unwind the call stack from :(RETURN), :(FRETURN), :(NRETURN), :(END)
;; These are ex-info maps thrown as exceptions and caught by DEFINE's wrapper.
(defn snobol-return!  [] (throw (ex-info "RETURN"  {:snobol/signal :return})))
(defn snobol-freturn! [] (throw (ex-info "FRETURN" {:snobol/signal :freturn})))
(defn snobol-nreturn! [] (throw (ex-info "NRETURN" {:snobol/signal :nreturn})))
(defn snobol-end!     [] (throw (ex-info "END"     {:snobol/signal :end})))
(defn snobol-steplimit! [n]
  (throw (ex-info (str "Step limit exceeded at step " n)
                  {:snobol/signal :step-limit :steps n})))
;; Maps JVM class names to SNOBOL4 type names.
(defmulti  DATATYPE (fn [X] (str (class X))))
(defmethod DATATYPE "class java.lang.Character"                   [_] "STRING")
(defmethod DATATYPE "class java.lang.String"                      [_] "STRING")
(defmethod DATATYPE "class java.lang.Long"                        [_] "INTEGER")
(defmethod DATATYPE "class java.lang.Double"                      [_] "REAL")
(defmethod DATATYPE "class [Ljava.lang.Object;"                   [_] "ARRAY")
(defmethod DATATYPE "class [LLjava.lang.Object;"                  [_] "ARRAY")
(defmethod DATATYPE "class SNOBOL4clojure.env.SnobolArray"        [_] "ARRAY")
(defmethod DATATYPE "class clojure.lang.Atom"                     [_] "TABLE")
;; A program-defined (DATA) object has an identity, as in SPITBOL: its fields are assigned in place, whoever refers to the object sees the change, two constructions
;; are two objects, and IDENT compares identity. It holds its type name, its field names and a mutable array of field values.
(deftype DataObj [type-name field-names ^objects fields serial])
;; What CODE(s) returns: "a pointer to the object code compiled from statements in the argument string ... executed with a direct Goto (:<CEDAR>)"
;; (manual ch. 19, CODE); its datatype is CODE. entry is the statement the code begins with, as the loader returns it.
(deftype CodeBlock [entry])
(defmethod DATATYPE "class SNOBOL4clojure.env.CodeBlock"          [_] "CODE")
(defmethod DATATYPE "class SNOBOL4clojure.env.DataObj"            [X] (.type-name ^DataObj X))
(defmethod DATATYPE "class clojure.lang.PersistentArrayMap"       [X] (get X :__type__ "TABLE"))
(defmethod DATATYPE "class clojure.lang.PersistentHashMap"        [X] (get X :__type__ "TABLE"))
(defmethod DATATYPE "class clojure.lang.PersistentVector"         [_] "PATTERN")
(defmethod DATATYPE "class clojure.lang.Symbol"                   [_] "NAME")
(defmethod DATATYPE "class clojure.lang.PersistentList"           [X]
  (if ('#{SEQ ALT CAPTURE CAPTURE-IMM CAPTURE-COND CURSOR-IMM! CONJ! COLLECT!
          ANY$ NOTANY$ SPAN$ NSPAN$ BREAK$ BREAKX$
          LEN# POS# RPOS# TAB# RTAB# BOL# EOL#
          FENCE! ARBNO! ARB! BAL! REM! ABORT!
          SUCCEED! FAIL! X} (first X))
    "PATTERN"
    "EXPRESSION"))
(defmethod DATATYPE "class clojure.lang.PersistentList$EmptyList" [_] "EXPRESSION")

;; The values SPITBOL compares by identity, as IDENT, DIFFER and a table's key do: a pattern, an expression, and the name of a keyword, a new one each time it is
;; made (code is a CodeBlock, whose identity is its own); a string, a number and the name of a variable or an element compare by value (sbl -bf:
;; IDENT(LEN(1), LEN(1)) and IDENT(.&TRACE, .&TRACE) fail, IDENT(P, Q) of Q = P and IDENT(.X, .X) succeed; T[LEN(1)] after T[P] = 'p' is the null string).
(defn object-identity? [x]
  (or (and (or (seq? x) (vector? x)) (contains? #{"PATTERN" "EXPRESSION"} (DATATYPE x)))
      (and (instance? NAME x) (seq? (.n ^NAME x)) (clojure.core/= '& (first (.n ^NAME x))))))
(defn snobol-ident? [x y] (if (or (object-identity? x) (object-identity? y)) (identical? x y) (clojure.core/= x y)))
(defmethod DATATYPE "class clojure.lang.PersistentTreeMap"        [_] "CODE")
(defmethod DATATYPE "class clojure.lang.Keyword"                  [_] "CODE")
(defmethod DATATYPE "class clojure.lang.PersistentHashSet"        [_] "SET")
(defmethod DATATYPE "class clojure.lang.PersistentTreeSet"        [_] "SET")
(defmethod DATATYPE "class java.util.regex.Pattern"               [_] "REGEX")
(defmethod DATATYPE "class java.lang.Class"                       [_] "DATA")
(defmethod DATATYPE :default [X]
  (let [cn (str (class X))]
    (or
      ;; Program-defined data types (map with :__type__ key)
      (when (and (map? X) (:__type__ X)) (:__type__ X))
      ;; Custom SNOBOL4 data types defined via DATA (legacy deftype path)
      (when-let [m (re-find #"class SNOBOL4clojure\.core\.(.*)" cn)]  (m 1))
      (when-let [m (re-find #"class SNOBOL4clojure\.env\.(.*)"  cn)]  (m 1))
      ;; NAME type specifically
      (when (re-find #"NAME" cn) "NAME"))))

;; ── Storage, as COLLECT counts it ──────────────────────────────────────────────
;; The words of SPITBOL storage the program's data occupies, by the sizes the manual gives (ch. 20, Space Considerations), in sbl x64's words of 8 bytes, 8
;; characters to a word: a variable's block 8 words and its name; a string 2 and its characters, the null string nothing; an integer or a real 2; a name 2; an array
;; 3 and its elements when it has one dimension from 1, else 8 + 2 per dimension and its elements; a table 4 and its hash headers, then 4 for each non-null element;
;; an object of a DATA type 3 and its fields; a pattern 3 for each of its nodes. Every value the variables reach is counted once, through elements and fields.
(defn- words-of-text [s] (quot (clojure.core/+ (count s) 7) 8))

(defn storage-words []
  ;; a worklist, not recursion: a list of a hundred thousand links is a chain that deep (spitbol_x64_tests gcbuster)
  (let [seen  (java.util.IdentityHashMap.)
        work  (java.util.ArrayDeque.)
        words (fn [v]   ; v's own words; what it holds goes on the worklist
                (cond
                  (nil? v)                         0
                  (string? v)                      (if (clojure.core/= "" v) 0 (clojure.core/+ 2 (words-of-text v)))
                  (number? v)                      2
                  (or (keyword? v) (symbol? v))    0
                  (.containsKey seen v)            0
                  :else
                  (do (.put seen v true)
                      (cond
                        (instance? NAME v)         2
                        (instance? SnobolArray v)
                        (let [dims (:dims v)
                              n    (reduce clojure.core/* 1 (map (fn [[lo hi]] (max 0 (inc (clojure.core/- hi lo)))) dims))]
                          (when (some? (:dflt v)) (.add work (:dflt v)))
                          (doseq [x (vals @(:data v))] (when (some? x) (.add work x)))
                          (clojure.core/+ (if (and (clojure.core/= 1 (count dims)) (clojure.core/= 1 (ffirst dims))) 3 (clojure.core/+ 8 (clojure.core/* 2 (count dims))))
                                          n))
                        (table? v)
                        (let [live (for [[k x] @v :when (not (or (nil? x) (clojure.core/= "" x)))] [k x])]
                          (doseq [[k x] live] (when (some? k) (.add work k)) (.add work x))
                          (clojure.core/+ 4 (long (:size (meta v) 11)) (clojure.core/* 4 (count live))))
                        (instance? DataObj v)
                        (let [fs (.fields ^DataObj v)] (doseq [x fs] (when (some? x) (.add work x))) (clojure.core/+ 3 (alength fs)))
                        (or (seq? v) (vector? v))  (do (doseq [x v] (when (some? x) (.add work x))) 3)
                        :else                      2))))]
    (loop [total (reduce clojure.core/+ 0 (for [[sym v] @<VARS>] (do (when (some? v) (.add work v)) (clojure.core/+ 8 (words-of-text (name sym))))))]
      (if-let [v (.poll work)]
        (recur (clojure.core/+ total (long (words v))))
        total))))
