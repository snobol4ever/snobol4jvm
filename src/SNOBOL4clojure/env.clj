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
(def <HOST0> (atom ""))   ; what HOST(0) returns: the -u text, or else the program's own arguments joined by blanks
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

;; ── Numeric conversion ────────────────────────────────────────────────────────
(defn num [x]
  (cond
    (nil? x)     0                        ; unset variable = 0 in arithmetic context
    (double? x)  x
    (integer? x) x                        ; preserve integer type
    true (let [s (clojure.string/trim (str x))]
           (if (clojure.core/= s "")
             0                            ; empty string = 0 in arithmetic context
             (or (try (Long/parseLong s)  ; try integer parse first
                   (catch NumberFormatException _ nil))
                 (try (Double/parseDouble s)
                   (catch NumberFormatException _ ##NaN)))))))

(defn  ncvt [x] (list 'num x))
(defn  scvt [x] (list 'str x))

;; ── SNOBOL4 string conversion ────────────────────────────────────────────────
(defn real-str
  "A real as SPITBOL prints it: rounded to 15 significant digits with trailing
   zeros stripped; fixed point for 0.1 <= |x| < 1e15, with a trailing \".\" when
   integral (2.0 -> \"2.\"); otherwise 0.<digits>e<sign><exponent> (1e20 ->
   \"0.1e+21\", 0.001 -> \"0.1e-2\"); zero is \"0.\"."
  [x]
  (let [x (double x)]
    (cond
      (or (Double/isNaN x) (Double/isInfinite x)) (str x)
      (zero? x) "0."
      :else
      (let [bd (.stripTrailingZeros
                 (.round (java.math.BigDecimal. x)
                         (java.math.MathContext. 15 java.math.RoundingMode/HALF_EVEN)))
            e  (clojure.core/- (.precision bd) (.scale bd))]
        (if (clojure.core/<= 0 e 15)
          (let [p (.toPlainString bd)]
            (if (clojure.string/includes? p ".") p (str p ".")))
          (str (when (neg? (.signum bd)) "-") "0." (.abs (.unscaledValue bd))
               "e" (if (neg? e) "-" "+") (Math/abs (long e))))))))

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
   (reset! <OBJNO>    0)
   (reset! <LASTOUT>  nil)
   (reset! <ASSOC-LAST> {})
   (reset! <HOST0>    "")
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
(defn snobol-set!
  "Assign val to the SNOBOL4 user variable named by sym."
  [sym val]
  (swap! <VARS> assoc (symbol (name sym)) val)
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
        ;; ("These variables contain the primitive patterns of the same name", manual ch. 18: ABORT, ARB, BAL, FAIL, FENCE, REM, SUCCEED)
        (let [resolved (ns-resolve (find-ns 'SNOBOL4clojure.core) sym)
              v        (when (instance? clojure.lang.Var resolved) (var-get resolved))]
          (if (or (nil? v) (and (fn? v) (not= sym 'FENCE)))
            ε
            (do (when (#{'ABORT 'ARB 'BAL 'FAIL 'REM 'SUCCEED} sym) (swap! <PRIMS-READ> conj sym)) v)))))))

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
(defn table-get [t k]        (get @t k (:dflt (meta t) ε)))
(defn table-set [t k v]
  (when-let [o (:order (meta t))]
    (swap! o (fn [m] (if (contains? m k) m (assoc m k (count m))))))
  (swap! t assoc k v)
  v)

(defn table-entries
  "The [key value] entries of a table in the order the keys were first assigned."
  [t]
  (let [order (if-let [o (:order (meta t))] @o {})]
    (sort-by (fn [[k _]] (get order k Long/MAX_VALUE)) (seq @t))))

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
