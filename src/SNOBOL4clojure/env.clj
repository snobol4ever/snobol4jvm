(ns SNOBOL4clojure.env
    (:refer-clojure :exclude [num])
    (:require [clojure.pprint :as pp] [clojure.string :as string])
    (:import [java.time LocalDateTime] [java.time.format DateTimeFormatter] [java.util Locale] [java.lang.management ManagementFactory GarbageCollectorMXBean]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def error-messages
    {
     1 "Addition left operand is not numeric"
     2 "Addition right operand is not numeric"
     3 "Addition caused integer overflow"
     4 "Affirmation operand is not numeric"
     5 "Alternation right operand is not pattern"
     6 "Alternation left operand is not pattern"
     7 "Compilation error encountered during execution"
     8 "Concatenation left operand is not a string or pattern"
     9 "Concatenation right operand is not a string or pattern"
     10 "Negation operand is not numeric"
     11 "Negation caused integer overflow"
     12 "Division left operand is not numeric"
     13 "Division right operand is not numeric"
     14 "Division caused integer overflow"
     15 "Exponentiation right operand is not numeric"
     16 "Exponentiation left operand is not numeric"
     17 "Exponentiation caused integer overflow"
     18 "Exponentiation result is undefined"
     20 "Goto evaluation failure"
     21 "Function called by name returned a value"
     22 "Undefined function called"
     23 "Goto operand is not a natural variable"
     24 "Goto operand in direct goto is not code"
     25 "Immediate assignment left operand is not pattern"
     26 "Multiplication left operand is not numeric"
     27 "Multiplication right operand is not numeric"
     28 "Multiplication caused integer overflow"
     29 "Undefined operator referenced"
     30 "Pattern assignment left operand is not pattern"
     31 "Pattern replacement right operand is not a string"
     32 "Subtraction left operand is not numeric"
     33 "Subtraction right operand is not numeric"
     34 "Subtraction caused integer overflow"
     35 "Unexpected failure in -NOFAIL mode"
     36 "Goto ABORT with no preceding error"
     37 "Goto CONTINUE with no preceding error"
     38 "Goto undefined label"
     39 "External function argument is not a string"
     40 "External function argument is not integer"
     41 "FIELD function argument is wrong datatype"
     42 "Attempt to change value of protected variable"
     43 "ANY evaluated argument is not a string"
     44 "BREAK evaluated argument is not a string"
     45 "BREAKX evaluated argument is not a string"
     46 "Expression does not evaluate to pattern"
     47 "LEN evaluated argument is not integer"
     48 "LEN evaluated argument is negative or too large"
     49 "NOTANY evaluated argument is not a string"
     50 "POS evaluated argument is not integer"
     51 "POS evaluated argument is negative or too large"
     52 "RPOS evaluated argument is not integer"
     53 "RPOS evaluated argument is negative or too large"
     54 "RTAB evaluated argument is not integer"
     55 "RTAB evaluated argument is negative or too large"
     56 "SPAN evaluated argument is not a string"
     57 "TAB evaluated argument is not integer"
     58 "TAB evaluated argument is negative or too large"
     59 "ANY argument is not a string or expression"
     60 "APPLY first arg is not natural variable name"
     61 "ARBNO argument is not pattern"
     62 "ARG second argument is not integer"
     63 "ARG first argument is not program function name"
     64 "ARRAY first argument is not integer or string"
     65 "ARRAY first argument lower bound is not integer"
     66 "ARRAY first argument upper bound is not integer"
     67 "ARRAY dimension is zero,negative or out of range"
     68 "ARRAY size exceeds maximum permitted"
     69 "BREAK argument is not a string or expression"
     70 "BREAKX argument is not a string or expression"
     71 "CLEAR argument is not a string"
     72 "CLEAR argument has null variable name"
     73 "COLLECT argument is not integer"
     74 "CONVERT second argument is not a string"
     75 "DATA argument is not a string"
     76 "DATA argument is null"
     77 "DATA argument is missing a left paren"
     78 "DATA argument has null datatype name"
     79 "DATA argument is missing a right paren"
     80 "DATA argument has null field name"
     81 "DEFINE first argument is not a string"
     82 "DEFINE first argument is null"
     83 "DEFINE first argument is missing a left paren"
     84 "DEFINE first argument has null function name"
     85 "Null arg name or missing ) in DEFINE first arg."
     86 "DEFINE function entry point is not defined label"
     87 "DETACH argument is not appropriate name"
     88 "DUMP argument is not integer"
     89 "DUMP argument is negative or too large"
     90 "DUPL second argument is not integer"
     91 "DUPL first argument is not a string or pattern"
     92 "EJECT argument is not a suitable name"
     93 "EJECT file does not exist"
     94 "EJECT file does not permit page eject"
     95 "EJECT caused non-recoverable output error"
     274 "Appendices"
     96 "ENDFILE argument is not a suitable name"
     97 "ENDFILE argument is null"
     98 "ENDFILE file does not exist"
     99 "ENDFILE file does not permit endfile"
     100 "ENDFILE caused non-recoverable output error"
     101 "EQ first argument is not numeric"
     102 "EQ second argument is not numeric"
     103 "EVAL argument is not expression"
     104 "EXIT first argument is not suitable integer or string"
     105 "EXIT action not available in this implementation"
     106 "EXIT action caused irrecoverable error"
     107 "FIELD second argument is not integer"
     108 "FIELD first argument is not datatype name"
     109 "GE first argument is not numeric"
     110 "GE second argument is not numeric"
     111 "GT first argument is not numeric"
     112 "GT second argument is not numeric"
     113 "INPUT third argument is not a string"
     114 "Inappropriate second argument for INPUT"
     115 "Inappropriate first argument for INPUT"
     116 "Inappropriate file specification for INPUT"
     117 "INPUT file cannot be read"
     118 "LE first argument is not numeric"
     119 "LE second argument is not numeric"
     120 "LEN argument is not integer or expression"
     121 "LEN argument is negative or too large"
     122 "LEQ first argument is not a string"
     123 "LEQ second argument is not a string"
     124 "LGE first argument is not a string"
     125 "LGE second argument is not a string"
     126 "LGT first argument is not a string"
     127 "LGT second argument is not a string"
     128 "LLE first argument is not a string"
     129 "LLE second argument is not a string"
     130 "LLT first argument is not a string"
     131 "LLT second argument is not a string"
     132 "LNE first argument is not a string"
     133 "LNE second argument is not a string"
     134 "LOCAL second argument is not integer"
     135 "LOCAL first arg is not a program function name"
     136 "LOAD second argument is not a string"
     137 "LOAD first argument is not a string"
     138 "LOAD first argument is null"
     139 "LOAD first argument is missing a left paren"
     140 "LOAD first argument has null function name"
     141 "LOAD first argument is missing a right paren"
     142 "LOAD function does not exist"
     143 "LOAD function caused input error during load"
     144 "LPAD third argument not a string"
     145 "LPAD second argument is not integer"
     146 "LPAD first argument is not a string"
     147 "LT first argument is not numeric"
     148 "LT second argument is not numeric"
     149 "NE first argument is not numeric"
     150 "NE second argument is not numeric"
     151 "NOTANY argument is not a string or expression"
     152 "OPSYN third argument is not integer"
     153 "OPSYN third argument is negative or too large"
     154 "OPSYN second arg is not natural variable name"
     155 "OPSYN first arg is not natural variable name"
     156 "OPSYN first arg is not correct operator name"
     157 "OUTPUT third argument is not a string"
     158 "Inappropriate second argument for OUTPUT"
     159 "Inappropriate first argument for OUTPUT"
     160 "Inappropriate file specification for OUTPUT"
     161 "OUTPUT file cannot be written to"
     162 "POS argument is not integer or expression"
     163 "POS argument is negative or too large"
     164 "PROTOTYPE argument is not valid object"
     165 "REMDR second argument is not numeric"
     166 "REMDR first argument is not integer"
     167 "REMDR caused integer overflow"
     168 "REPLACE third argument is not a string"
     169 "REPLACE second argument is not a string"
     170 "REPLACE first argument is not a string"
     171 "Null or unequally long 2nd, 3rd args to REPLACE"
     172 "REWIND argument is not a suitable name"
     173 "REWIND argument is null"
     174 "REWIND file does not exist"
     175 "REWIND file does not permit rewind"
     176 "REWIND caused non-recoverable error"
     177 "REVERSE argument is not a string"
     178 "RPAD third argument is not a string"
     179 "RPAD second argument is not integer"
     180 "RPAD first argument is not a string"
     181 "RTAB argument is not integer or expression"
     182 "RTAB argument is negative or too large"
     183 "TAB argument is not integer or expression"
     184 "TAB argument is negative or too large"
     185 "RPOS argument is not integer or expression"
     186 "RPOS argument is negative or too large"
     187 "SETEXIT argument is not label name or null"
     188 "SPAN argument is not a string or expression"
     189 "SIZE argument is not a string"
     190 "STOPTR first argument is not appropriate name"
     191 "STOPTR second argument is not trace type"
     192 "SUBSTR third argument is not integer"
     193 "SUBSTR second argument is not integer"
     276 "Appendices"
     194 "SUBSTR first argument is not a string"
     195 "TABLE argument is not integer"
     196 "TABLE argument is out of range"
     197 "TRACE fourth arg is not function name or null"
     198 "TRACE first argument is not appropriate name"
     199 "TRACE second argument is not trace type"
     200 "TRIM argument is not a string"
     201 "UNLOAD argument is not natural variable name"
     202 "Input from file caused non-recoverable error"
     203 "Input file record has incorrect format"
     204 "Memory overflow"
     205 "String length exceeds value of MAXLNGTH keyword"
     206 "Output caused file overflow"
     207 "Output caused non-recoverable error"
     208 "Keyword value assigned is not integer"
     209 "Keyword in assignment is protected"
     210 "Keyword value assigned is negative or too large"
     211 "Value assigned to keyword ERRTEXT not a string"
     212 "Syntax error: Value used where name is required"
     213 "Syntax error: Statement is too complicated."
     214 "Bad label or misplaced continuation line"
     215 "Syntax error: Undefined or erroneous entry label"
     216 "Syntax error: Missing END line"
     217 "Syntax error: Duplicate label"
     218 "Syntax error: Duplicated goto field"
     219 "Syntax error: Empty goto field"
     220 "Syntax error: Missing operator"
     221 "Syntax error: Missing operand"
     222 "Syntax error: Invalid use of left bracket"
     223 "Syntax error: Invalid use of comma"
     224 "Syntax error: Unbalanced right parenthesis"
     225 "Syntax error: Unbalanced right bracket"
     226 "Syntax error: Missing right paren"
     227 "Syntax error: Right paren missing from goto"
     228 "Syntax error: Right bracket missing from goto"
     229 "Syntax error: Missing right array bracket"
     230 "Syntax error: Illegal character"
     231 "Syntax error: Invalid numeric item"
     232 "Syntax error: Unmatched string quote"
     233 "Syntax error: Invalid use of operator"
     234 "Syntax error: Goto field incorrect"
     235 "Subscripted operand is not table or array"
     236 "Array referenced with wrong number of subscripts"
     237 "Table referenced with more than one subscript"
     238 "Array subscript is not integer"
     239 "Indirection operand is not name"
     240 "Pattern match right operand is not pattern"
     241 "Pattern match left operand is not a string"
     242 "Function return from level zero"
     243 "Function result in NRETURN is not name"
     244 "Statement count exceeds value of STLIMIT keyword"
     246 "Stack overflow"
     247 "Invalid control statement"
     248 "Attempted redefinition of system function"
     249 "Expression evaluated by name returned value"
     250 "Insufficient memory to complete dump"
     251 "Keyword operand is not name of defined keyword"
     252 "Error on printing to interactive channel"
     254 "Erroneous argument for HOST"
     255 "Error during execution of HOST"
     256 "SORT/RSORT 1st arg not suitable ARRAY or TABLE"
     257 "Erroneous 2nd arg in SORT/RSORT of vector"
     258 "SORT/RSORT 2nd arg out of range or non-integer"
     259 "FENCE argument is not pattern"
     260 "Conversion array size exceeds maximum permitted"
     261 "Addition caused real overflow"
     262 "Division caused real overflow"
     263 "Multiplication caused real overflow"
     264 "Subtraction caused real overflow"
     265 "External function argument is not real"
     266 "Exponentiation caused real overflow"
     268 "Inconsistent value assigned to keyword PROFILE"
     270 "BACKSPACE argument is not a suitable name"
     271 "BACKSPACE file does not exist"
     272 "BACKSPACE file does not permit backspace"
     273 "BACKSPACE caused non-recoverable error"
     281 "Char argument not integer"
     282 "Char argument not in range"
     284 "Excessively nested INCLUDE files"
     285 "INCLUDE file cannot be opened"
     286 "Function call to undefined entry label"
     287 "Value assigned to keyword MAXLNGTH is too small"
     288 "EXIT second argument is not a string"
     289 "Input channel currently in use"
     290 "Output channel currently in use"
     291 "SET first argument is not a suitable name"
     292 "SET first argument is null"
     293 "Inappropriate second argument to SET"
     294 "Inappropriate third argument to SET"
     295 "SET file does not exist"
     296 "SET file does not permit setting file pointer"
     297 "SET caused non-recoverable I/O error"
     298 "External function argument is not file"
     299 "Internal logic error: Unexpected PPM branch"
     301 "ATAN argument not numeric"
     302 "CHOP argument not numeric"
     303 "COS argument not numeric"
     304 "EXP argument not numeric"
     305 "EXP produced real overflow"
     306 "LN argument not numeric"
     278 "Appendices"
     307 "LN produced real overflow"
     308 "SIN argument not numeric"
     309 "TAN argument not numeric"
     310 "TAN produced real overflow"
     311 "Exponentiation of negative base to non-integral power"
     312 "REMDR caused real overflow"
     313 "SQRT argument not numeric"
     314 "SQRT argument negative"
     315 "LN argument negative"
     320 "User interrupt"
     321 "Goto SCONTINUE with no preceding error"
     326 "Calling external function – bad argument type"
     327 "Calling external function – not found"
     328 "LOAD function – insufficient memory"
     329 "Requested MAXLNGTH too large"
     331 "Goto SCONTINUE with no user interrupt"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn Σ+ ([x y] (clojure.core/+' x y)) ([x] (clojure.core/+' x)))
(defn subtract ([x y] (clojure.core/-' x y)) ([x] (clojure.core/-' x)))
(defn multiply ([x y] (clojure.core/*' x y)) ([x] (clojure.core/*' x)))
(defn divide [x y] (if (and (integer? x) (integer? y)) (quot x y) (clojure.core// x y)))
(defn equal [x y] (clojure.core/= x y))
(defn not-equal [x y] (clojure.core/not= x y))
(defn out [item] (binding [pp/*print-right-margin* 2048 pp/*print-miser-width* 2000] (pp/pprint item)) item)
(def ε "")
(def η ##NaN)
(def &ALPHABET (atom (apply str (map char (range 256)))))
(def &ABEND (atom 0))
(def &ANCHOR (atom 0))
(def &CASE (atom 0))
(def &DIGITS "0123456789")
(def &DUMP (atom 0))
(def &CODE (atom 0))
(def &COMPARE (atom 0))
(def &ERRLIMIT (atom 0))
(def &ERRTEXT (atom ε))
(def &ERRTYPE (atom 0))
(def &FTRACE (atom 0))
(def &FULLSCAN (atom 1))
(def &LASTNO (atom 0))
(def &STNO (atom 0))
(def &FNCLEVEL (atom 0))
(def &FILE (atom ε))
(def &LASTFILE (atom ε))
(def &LINE (atom 0))
(def &LASTLINE (atom 0))
(def &LCASE "abcdefghijklmnopqrstuvwxyz")
(def &INPUT (atom 1))
(def &MAXLNGTH (atom 16777216))
(def &OUTPUT (atom 1))
(def &PROFILE (atom 0))
(def &RTNTYPE (atom ε))
(def &TRACE (atom 0))
(def &TRIM (atom 1))
(def &STCOUNT (atom 0))
(def &STLIMIT (atom 2147483647))
(def &IOCOMPAT (atom :catspaw))
(def &UCASE "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private keyword-defaults
    {
     #'&ABEND 0
     #'&ANCHOR 0
     #'&CASE 0
     #'&CODE 0
     #'&COMPARE 0
     #'&DUMP 0
     #'&ERRLIMIT 0
     #'&FTRACE 0
     #'&FULLSCAN 1
     #'&INPUT 1
     #'&MAXLNGTH 16777216
     #'&OUTPUT 1
     #'&PROFILE 0
     #'&TRIM 1
     #'&TRACE 0
     #'&STLIMIT 2147483647
     #'&IOCOMPAT :catspaw})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn reset-keywords! [] (doseq [[v dflt] keyword-defaults] (reset! @v dflt)))
(def INPUT$ (atom ε))
(def OUTPUT$ (atom ε))
(def TERMINAL$ (atom ε))
(def <CHANNELS> (atom {}))
(def ^:dynamic *file-dir* nil)
(def <POS> (atom {}))
(def ^:dynamic *eval-block* nil)
(def <CODELINE> (atom ["" 0]))
(def <OBJNO> (atom 0))
(defn next-serial! [] (swap! <OBJNO> inc))
(def <NAMES> (atom #{}))
(defn note-names! [names] (when (seq names) (swap! <NAMES> into names)))
(def <PRIMS-READ> (atom #{}))
(def <DIAGNOSTICS> (atom []))
(def <SOURCE> (atom nil))
(def ^:dynamic *matching* false)
(def ^:dynamic *fp* 0)
(def ^:dynamic *flprt* -1)
(def ^:dynamic *handlers* 0)
(def <STAGE> (atom :xt))
(def <GTCEF> (atom 0))
(def <XC> (atom false))
(def <ERRFT> (atom 0))
(def <ERRSERIAL> (atom 0))
(def <WIDTH> (atom 120))
(def <STMT-LINE> (atom 0))
(def <HOST0> (atom ""))
(def <ARGV> (atom []))
(def <ARGI> (atom 0))
(def <LASTOUT> (atom nil))
(def <ASSOC-LAST> (atom {}))
(def <SETEXIT> (atom nil))
(def <DISPNO> (atom {}))
(def BLANKS (atom 0))
(def <TRACES> (atom {}))
(def <ETRACES> (atom []))
(def <PRNMV> (atom nil))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- var-slot
    [^String s]
    (let [
          b ^bytes (.getBytes s "ISO-8859-1")
          n (alength b)
          h
              (reduce
                  (fn
                      [h w]
                      (bit-xor h
                          (reduce
                              (fn [acc j] (let [i (clojure.core/+ (clojure.core/* 8 w) j)] (if (< i n) (bit-or acc (bit-shift-left (bit-and 0xff (long (aget b i))) (clojure.core/* 8 j))) acc)))
                              0
                              (range 8))))
                  (long n)
                  (range (min 3 (quot (clojure.core/+ n 7) 8))))]
        (mod (bit-and Long/MAX_VALUE h) 257)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def <OPSYN> (atom {}))
(def STNO (atom 0))
(def <STNO> (atom {}))
(def <LABL> (atom {}))
(def <CODE> (atom {}))
(def block-end [::block-end])
(def compile-error [::compile-error])
(def <FUNS> (atom {}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def system-functions
    #{
      "ANY" "APPLY"
      "ARBNO" "ARG"
      "ARRAY" "ATAN"
      "BACKSPACE" "BREAK"
      "BREAKX" "CHAR"
      "CHOP" "CLEAR"
      "CODE" "COLLECT"
      "CONVERT" "COPY"
      "COS" "DATA"
      "DATATYPE" "DATE"
      "DEFINE" "DETACH"
      "DIFFER" "DUMP"
      "DUPL" "EJECT"
      "ENDFILE" "EQ"
      "EVAL" "EXIT"
      "EXP" "FENCE"
      "FIELD" "GE"
      "GT" "HOST"
      "IDENT" "INPUT"
      "INTEGER" "ITEM"
      "LE" "LEN"
      "LEQ" "LGE"
      "LGT" "LLE"
      "LLT" "LN"
      "LNE" "LOAD"
      "LOCAL" "LPAD"
      "LT" "NE"
      "NOTANY" "OPSYN"
      "OUTPUT" "POS"
      "PROTOTYPE" "REMDR"
      "REPLACE" "REVERSE"
      "REWIND" "RPAD"
      "RPOS" "RSORT"
      "RTAB" "SET"
      "SETEXIT" "SIN"
      "SIZE" "SORT"
      "SPAN" "SQRT"
      "STOPTR" "SUBSTR"
      "TAB" "TABLE"
      "TAN" "TIME"
      "TRACE" "TRIM"
      "UNLOAD"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def <FDEFS> (atom {}))
(definterface &NAME (n []) (n [_]))
(defn- name-identity [n] (if-let [c (and (seq? n) (:container (meta n)))] (cons c (rest n)) n))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(deftype NAME
    [^:unsynchronized-mutable n]
    &NAME
    (n [this] n)
    (n [this _] (set! n _))
    Object
    (equals [this o] (and (instance? &NAME o) (clojure.core/= (name-identity n) (name-identity (.n ^&NAME o)))))
    (hashCode [this] (hash (name-identity n)))
    (toString [this] (str n)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(deftype ExprVar [ir])
(defrecord NameResult [target])
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fold-case
    [^String s]
    (let [sb (StringBuilder.)]
        (loop [i 0 q nil]
            (when
                (clojure.core/< i (count s))
                (let [c (.charAt s i)]
                    (cond
                        q (do (.append sb c) (recur (inc i) (if (clojure.core/= c q) nil q)))
                        (or (clojure.core/= c \') (clojure.core/= c \")) (do (.append sb c) (recur (inc i) c))
                        :else (do (.append sb (if (and (clojure.core/>= (int c) 97) (clojure.core/<= (int c) 122)) (Character/toUpperCase c) c)) (recur (inc i) nil))))))
        (str sb)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn name-string [x] (if (instance? NAME x) (str (.n ^NAME x)) (str x)))
(def ^:private ten-powers (double-array [1.0 1e1 1e2 1e3 1e4 1e5 1e6 1e7 1e8 1e9 1e10]))
(def ^:private gts-scale (reduce (fn [a _] (clojure.core/* a 10.0)) 1.0 (range 15)))
(def ^:private gts-round (clojure.core// 0.5 gts-scale))
(defn- ftz ^double [^double x] (if (< (Math/abs x) Double/MIN_NORMAL) (clojure.core/* x 0.0) x))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn parse-real
    [s]
    (when-let [[_ sg ^String ip point ex] (re-matches #"\s*([+-]?)(\d*)(?:\.(\d*))?(?:[eEdD]([+-]?\d+|[+-]?(?=\s)))?\s*" (str s))]
        (let [
              ^String fp (or point "")
              [n i]
                  (loop [i 0 n 0]
                      (if
                          (< i (.length ip))
                          (if-some [m (try (Math/subtractExact (Math/multiplyExact (long n) 10) (long (clojure.core/- (int (.charAt ip i)) 48))) (catch ArithmeticException _ nil))]
                              (recur (inc i) m)
                              [n i])
                          [n i]))
              digit (fn [^double r ^Character c] (clojure.core/+ (clojure.core/* r 10.0) (double (clojure.core/- (int c) 48))))
              r (reduce digit (clojure.core/- (double n)) (concat (subs ip i) fp))
              sc (try (clojure.core/- (.length fp) (Long/parseLong (if (or (nil? ex) (re-matches #"[+-]?" ex)) "0" ex))) (catch NumberFormatException _ nil))]
            (when
                (and sc (pos? (clojure.core/+ (.length ip) (.length fp))) (or point ex))
                (let [r
                          (loop [r (double r) sc (long sc)]
                              (cond
                                  (or (Double/isInfinite r) (zero? r)) r
                                  (> sc 10) (recur (ftz (clojure.core// r 1e10)) (clojure.core/- sc 10))
                                  (< sc -10) (recur (clojure.core/* r 1e10) (clojure.core/+ sc 10))
                                  (pos? sc) (ftz (clojure.core// r (aget ^doubles ten-powers sc)))
                                  (neg? sc) (clojure.core/* r (aget ^doubles ten-powers (clojure.core/- sc)))
                                  :else r))]
                    (when-not (Double/isInfinite r) (if (clojure.core/= "-" sg) (clojure.core/- r) r)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn sign-then-blanks? [x] (and (string? x) (boolean (re-matches #"[ \t]*[+-][ \t]+" x))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn num
    [x]
    (cond
        (sign-then-blanks? x) 0
        (nil? x) 0
        (double? x) x
        (integer? x) x
        true (let [s (clojure.string/trim (str x))] (if (clojure.core/= s "") 0 (or (try (Long/parseLong s) (catch NumberFormatException _ nil)) (parse-real (str x)) ##NaN)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ncvt [x] (list 'num x))
(defn scvt [x] (list 'str x))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn real-str
    [x]
    (let [x (double x)]
        (cond
            (or (Double/isNaN x) (Double/isInfinite x)) (str x)
            (zero? x) "0."
            :else
                (let [
                      [a e] (loop [a (Math/abs x) e 0] (if (< a 0.1) (recur (clojure.core/* a 1e10) (clojure.core/- e 10)) [a e]))
                      [a e]
                          (if
                              (< (double a) 1.0)
                              [a e]
                              (let [
                                    [a e] (loop [a (double a) e (long e)] (if (>= a 1e10) (recur (clojure.core// a 1e10) (clojure.core/+ e 10)) [a e]))
                                    k (loop [k 1] (if (>= (double a) (aget ^doubles ten-powers k)) (recur (inc k)) k))]
                                  [(clojure.core// (double a) (aget ^doubles ten-powers k)) (clojure.core/+ (long e) k)]))
                      a (clojure.core/+ (double a) (double gts-round))
                      [a e] (if (< a 1.0) [a e] [(clojure.core// a 10.0) (inc (long e))])
                      n (long (clojure.core/* (double a) (double gts-scale)))
                      e (long e)
                      fixed (clojure.core/<= 0 e 15)
                      nd (if fixed (clojure.core/- 15 e) 15)
                      p (long (Math/pow 10.0 nd))
                      frac (if (zero? nd) "" (clojure.string/replace (format (str "%0" nd "d") (rem n p)) #"0+$" ""))]
                    (str (when (neg? x) "-") (quot n p) "." frac (when-not fixed (str "e" (if (neg? e) "-" "+") (Math/abs e))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn sstr [x] (cond (nil? x) "" (double? x) (real-str x) :else (str x)))
(def <VARS> (atom {}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn GLOBALS
    ([]
        (reset! <VARS> {})
        (reset! <FUNS> {})
        (reset! <FDEFS> {})
        (doseq [[_ ch] @<CHANNELS>] (try (when-let [rdr (:reader ch)] (.close ^java.io.BufferedReader rdr)) (when-let [wtr (:writer ch)] (.close ^java.io.PrintWriter wtr)) (catch Exception _ nil)))
        (reset! <CHANNELS> {})
        (reset! <OPSYN> {})
        (reset! <TRACES> {})
        (reset! <DISPNO> {})
        (reset! <SETEXIT> nil)
        (reset! <STAGE> :xt)
        (reset! <GTCEF> 0)
        (reset! <XC> false)
        (reset! <ERRFT> 0)
        (reset! <ERRSERIAL> 0)
        (reset! <OBJNO> 0)
        (reset! <LASTOUT> nil)
        (reset! <ASSOC-LAST> {})
        (reset! <HOST0> "")
        (reset! <ARGV> [])
        (reset! <ARGI> 0)
        (reset! <PRIMS-READ> #{})
        (reset! <NAMES> #{})
        (reset! <ETRACES> [])
        (reset! <PRNMV> nil)
        (reset! &RTNTYPE ε)
        (reset! &ERRTYPE 0)
        (reset! &ERRTEXT ε)
        (reset! <POS> {})
        (reset! <CODELINE> ["" 0])
        (reset! &FILE ε)
        (reset! &LASTFILE ε)
        (reset! &LINE 0)
        (reset! &LASTLINE 0)
        (reset! BLANKS 0)
        (reset! STNO 0)
        (reset! <STNO> {})
        (reset! <LABL> {})
        (reset! <CODE> {})
        (reset! &STNO 0)
        (reset! &FNCLEVEL 0)
        (reset! &LASTNO 0)
        (reset-keywords!))
    ([_ns] (GLOBALS)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare error-text)
(defn error-message [n] (error-text n))
(def ^:private error-text (merge (into {} (for [[n m] error-messages :when (not (#{274 276 278} n))] [n (clojure.string/lower-case m)])) {274 "value assigned to keyword fullscan is zero"}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn gtstg
    ^String [x n]
    (cond
        (string? x) x
        (nil? x) ""
        (char? x) (str x)
        (integer? x) (str x)
        (double? x) (real-str x)
        (and (instance? NAME x) (symbol? (.n ^NAME x))) (name (.n ^NAME x))
        :else (throw (ex-info (error-message n) {:snobol/error n}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn keyword-var [k] (ns-resolve (the-ns 'SNOBOL4clojure.env) (symbol (str "&" (name k)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn assign-keyword!
    [k v]
    (let [kw (name k) vr (keyword-var kw)]
        (when-not vr (throw (ex-info "keyword operand is not name of defined keyword" {:snobol/error 251})))
        (if
            (clojure.core/= kw "ERRTEXT")
            (reset! &ERRTEXT (sstr v))
            (let [n (if (integer? v) v (num v))]
                (when (and (double? n) (Double/isNaN n)) (throw (ex-info "keyword value assigned is not integer" {:snobol/error 208})))
                (let [n (long n)]
                    (cond
                        (clojure.core/= kw "ERRTYPE") (throw (ex-info (or (error-text n) (str @&ERRTEXT)) {:snobol/error n}))
                        (and (clojure.core/= kw "PROFILE") (clojure.core/= n 2) (clojure.core/= 1 (long @&PROFILE)))
                            (throw (ex-info "inconsistent value assigned to keyword profile" {:snobol/error 268}))
                        (and
                            (not (clojure.core/= kw "STLIMIT"))
                            (or (contains? keyword-defaults vr) (clojure.core/= vr #'&TRACE))
                            (or (neg? n) (clojure.core/> n 16777216) (and (clojure.core/= kw "PROFILE") (clojure.core/> n 1))))
                            (throw (ex-info "keyword value assigned is negative or too large" {:snobol/error 210}))
                        (and (clojure.core/= kw "MAXLNGTH") (clojure.core/< n 1024)) (throw (ex-info "value assigned to keyword maxlngth is too small" {:snobol/error 287}))
                        (and (clojure.core/= kw "FULLSCAN") (zero? n)) (throw (ex-info "value assigned to keyword fullscan is zero" {:snobol/error 274}))
                        (or (contains? keyword-defaults vr) (clojure.core/= vr #'&TRACE)) (do (reset! @vr n) n)
                        :else (throw (ex-info "keyword in assignment is protected" {:snobol/error 209}))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def protected-variables '#{ABORT ARB BAL FAIL FENCE REM SUCCEED})
(defn output-associated? [sym] (clojure.core/= :output (:type (get @<CHANNELS> sym))))
(defn snobol-set! [sym val] (let [s (symbol (name sym))] (swap! <VARS> assoc s val) (when (output-associated? s) (swap! <ASSOC-LAST> assoc s val))) val)
(defn protected-store! [sym val] (when (protected-variables (symbol (name sym))) (throw (ex-info (error-message 42) {:snobol/error 42}))) (snobol-set! sym val))
(defn reference [N] (get @<VARS> (symbol (name N))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn clear-vars!
    [s]
    (let [kept (into #{} (comp (map clojure.string/trim) (remove empty?) (map symbol)) (clojure.string/split (sstr s) #",")) null (fn [m] (reduce-kv (fn [m k _] (if (kept k) m (assoc m k ""))) m m))]
        (swap! <VARS> null)
        (swap! <ASSOC-LAST> null)
        ""))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private stdin-cache (atom nil))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn stdin-reader
    ^java.io.BufferedReader []
    (let [in *in* [k r] @stdin-cache] (if (identical? k in) r (let [r (if (instance? java.io.BufferedReader in) in (java.io.BufferedReader. ^java.io.Reader in))] (reset! stdin-cache [in r]) r))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:dynamic *terminal-device* "/dev/tty")
(defonce ^:private terminal-cache (atom nil))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn terminal-reader
    ^java.io.BufferedReader []
    (let [dev *terminal-device* [k r] @terminal-cache]
        (if
            (clojure.core/= k dev)
            r
            (let [r (try (java.io.BufferedReader. (java.io.InputStreamReader. (java.io.FileInputStream. ^String dev) "ISO-8859-1")) (catch Exception _ nil))] (reset! terminal-cache [dev r]) r))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private record-length 1024)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn input-record
    ([line] (input-record line nil))
    ([line maxlen] (let [s (str line) n (or maxlen record-length) s (if (clojure.core/> (count s) n) (subs s 0 n) s)] (if (zero? (long @&TRIM)) s (clojure.string/replace s #"[ \t]+$" "")))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- read-raw
    [^java.io.Reader rdr n]
    (let [buf (char-array n)]
        (loop [k 0] (if (clojure.core/< k n) (let [r (.read rdr buf k (clojure.core/- n k))] (if (neg? r) (when (pos? k) (String. buf 0 k)) (recur (clojure.core/+ k r)))) (String. buf 0 k)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- read-line-counted
    [^java.io.BufferedReader rdr at]
    (let [sb (StringBuilder.)]
        (loop [n 0]
            (let [c (.read rdr)]
                (cond
                    (neg? c) (do (swap! at clojure.core/+ n) (when (pos? n) (str sb)))
                    (clojure.core/== c 10) (do (swap! at clojure.core/+ n 1) (str sb))
                    (clojure.core/== c 13) (do (.mark rdr 1) (if (clojure.core/== (.read rdr) 10) (swap! at clojure.core/+ n 2) (do (.reset rdr) (swap! at clojure.core/+ n 1))) (str sb))
                    :else (do (.append sb (char c)) (recur (inc n))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn channel-read
    [ch]
    (let [rdr ^java.io.BufferedReader (:reader ch) at (:at ch)]
        (if-let [n (:raw ch)] (let [s (read-raw rdr n)] (when (and s at) (swap! at clojure.core/+ (count s))) s) (if at (read-line-counted rdr at) (.readLine rdr)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private no-function '#{ARB BAL FAIL REM SUCCEED BOL EOL NAME NSPAN REAL FULLMATCH GLOBALS INVOKE MATCH RUN SEARCH})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn primitive-pattern
    [sym]
    (let [resolved (ns-resolve (find-ns 'SNOBOL4clojure.core) sym) v (when (instance? clojure.lang.Var resolved) (var-get resolved))]
        (if (nil? v) ε (do (when-not (clojure.core/= sym 'FENCE) (swap! <PRIMS-READ> conj sym)) v))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare $$)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn $$fn
    [N]
    (let [sym (symbol (name N))]
        (if-let [v (find @<VARS> sym)]
            (val v)
            (let [resolved (when (and (re-matches #"[A-Z][A-Z0-9]*" (name sym)) (not (no-function sym))) (ns-resolve (find-ns 'SNOBOL4clojure.core) sym))]
                (if (instance? clojure.lang.Var resolved) (var-get resolved) ($$ N))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn holder-name
    [v]
    (let [last @<PRNMV>]
        (if
            (and last (identical? v (get @<VARS> last)))
            (name last)
            (when-let [k (first (sort-by (fn [k] [(var-slot (name k)) (name k)]) (keep (fn [[k x]] (when (identical? x v) k)) @<VARS>)))] (reset! <PRNMV> k) (name k)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn $$
    [N]
    (let [sym (symbol (name N))]
        (cond
            (clojure.core/= :input (:type (get @<CHANNELS> sym)))
                (let [ch (get @<CHANNELS> sym) line (try (channel-read ch) (catch Exception _ nil)) v (cond (nil? line) nil (:raw ch) line :else (input-record line (:maxlen ch)))]
                    (when (some? v) (swap! <ASSOC-LAST> assoc sym v))
                    v)
            (clojure.core/= sym 'INPUT)
                (if
                    (zero? (long @&INPUT))
                    ε
                    (let [line (try (.readLine (stdin-reader)) (catch Exception _ nil)) v (when-not (nil? line) (input-record line))] (when (some? v) (swap! <ASSOC-LAST> assoc 'INPUT v)) v))
            (clojure.core/= sym 'TERMINAL)
                (let [line (try (.readLine (terminal-reader)) (catch Exception _ nil)) v (when-not (nil? line) (input-record line))] (when (some? v) (swap! <ASSOC-LAST> assoc 'TERMINAL v)) v)
            (or (clojure.core/= sym 'OUTPUT) (clojure.core/= :output (:type (get @<CHANNELS> sym)))) (get @<ASSOC-LAST> sym ε)
            :else (if-let [v (find @<VARS> sym)] (val v) (if (protected-variables sym) (primitive-pattern sym) ε)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn snapshot! [] (into (sorted-map) @<VARS>))
(defrecord SnobolArray [dims dflt data])
(defn- array-bound [x err] (let [n (num x)] (cond (integer? n) (long n) (and (double? n) (not (Double/isNaN n))) (long n) :else (throw (ex-info (error-message err) {:snobol/error err})))))
(defn- dimension [lo hi] (when (> (long lo) (long hi)) (throw (ex-info (error-message 67) {:snobol/error 67}))) [lo hi])
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- parse-array-proto
    [proto]
    (let [
          proto (if (and (instance? NAME proto) (symbol? (.n ^NAME proto))) (str (.n ^NAME proto)) proto)
          whole (when (or (number? proto) (string? proto)) (let [n (num proto)] (when (or (integer? n) (and (double? n) (not (Double/isNaN n)))) (long n))))]
        (cond
            whole [[(dimension 1 whole)] (str whole)]
            (not (string? proto)) (throw (ex-info (error-message 64) {:snobol/error 64}))
            :else
                [(mapv
                     (fn [part] (if-some [i (clojure.string/index-of part ":")] (dimension (array-bound (subs part 0 i) 65) (array-bound (subs part (inc i)) 66)) (dimension 1 (array-bound part 66))))
                     (clojure.string/split proto #"," -1))
                    proto])))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ARRAY ([proto] (ARRAY proto ε)) ([proto dflt] (let [[dims text] (parse-array-proto proto)] (assoc (->SnobolArray dims dflt (atom {})) :proto text :serial (next-serial!)))))
(defn- new-table ([size] (new-table size ε)) ([size dflt] (atom {} :meta {:order (atom {}) :size size :serial (next-serial!) :dflt dflt})))
(defn- table-size [n] (let [k (num n)] (cond (and (integer? k) (pos? k)) k (and (double? k) (pos? k)) (long k) :else 11)))
(defn TABLE ([] (new-table 11)) ([n] (new-table (table-size n))) ([n _x] (new-table (table-size n))) ([n _x d] (new-table (table-size n) (if (nil? d) ε d))))
(defn empty-set [] (hash-set))
(defn table? [x] (and (instance? clojure.lang.IDeref x) (map? @x)))
(declare object-identity?)
(deftype ObjKey [v] Object (equals [_ o] (and (instance? ObjKey o) (identical? v (.v ^ObjKey o)))) (hashCode [_] (System/identityHashCode v)))
(defn- table-key [k] (if (object-identity? k) (ObjKey. k) k))
(defn table-subscript [ks] (if (next ks) (throw (ex-info (error-message 237) {:snobol/error 237})) (first ks)))
(defn table-get [t k] (get @t (table-key k) (:dflt (meta t) ε)))
(defn table-set [t k v] (let [k (table-key k)] (when-let [o (:order (meta t))] (swap! o (fn [m] (if (contains? m k) m (assoc m k (count m)))))) (swap! t assoc k v)) v)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn table-entries
    [t]
    (let [order (if-let [o (:order (meta t))] @o {})] (map (fn [[k v]] [(if (instance? ObjKey k) (.v ^ObjKey k) k) v]) (sort-by (fn [[k _]] (get order k Long/MAX_VALUE)) (seq @t)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn array? [x] (instance? SnobolArray x))
(defn- array-subscript [i] (if (integer? i) i (let [n (num i)] (if (and (double? n) (Double/isNaN n)) (throw (ex-info "array subscript is not integer" {:snobol/error 238})) (long n)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- array-check-bounds
    [^SnobolArray arr idx-vec]
    (let [dims (:dims arr) _ (when-not (clojure.core/= (count idx-vec) (count dims)) (throw (ex-info (error-message 236) {:snobol/error 236}))) idx-vec (mapv array-subscript idx-vec)]
        (when (clojure.core/= (count idx-vec) (count dims)) (when (every? true? (map (fn [[lo hi] i] (and (integer? i) (>= i lo) (<= i hi))) dims idx-vec)) idx-vec))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn array-in-bounds? [arr idx-vec] (some? (array-check-bounds arr idx-vec)))
(defn array-get [^SnobolArray arr idx-vec] (when-let [k (array-check-bounds arr idx-vec)] (get @(:data arr) k (:dflt arr))))
(defn array-set [^SnobolArray arr idx-vec v] (when-let [k (array-check-bounds arr idx-vec)] (swap! (:data arr) assoc k v) v))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn array-prototype
    [^SnobolArray arr]
    (let [proto (or (:proto arr) (clojure.string/join "," (map (fn [[lo hi]] (str lo ":" hi)) (:dims arr))))] (if (re-matches #"[+]?[0-9]+" proto) (second (first (:dims arr))) proto)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private return-signal (clojure.lang.ExceptionInfo. "RETURN" {:snobol/signal :return}))
(def ^:private freturn-signal (clojure.lang.ExceptionInfo. "FRETURN" {:snobol/signal :freturn}))
(def ^:private nreturn-signal (clojure.lang.ExceptionInfo. "NRETURN" {:snobol/signal :nreturn}))
(def ^:private end-signal (clojure.lang.ExceptionInfo. "END" {:snobol/signal :end}))
(defn snobol-return! [] (throw return-signal))
(defn snobol-freturn! [] (throw freturn-signal))
(defn snobol-nreturn! [] (throw nreturn-signal))
(defn snobol-end! [] (throw end-signal))
(defn snobol-steplimit! [n] (throw (ex-info (str "Step limit exceeded at step " n) {:snobol/signal :step-limit :steps n})))
(defmulti DATATYPE (fn [X] (str (class X))))
(defmethod DATATYPE "class java.lang.Character" [_] "STRING")
(defmethod DATATYPE "class java.lang.String" [_] "STRING")
(defmethod DATATYPE "class java.lang.Long" [_] "INTEGER")
(defmethod DATATYPE "class java.lang.Integer" [_] "INTEGER")
(defmethod DATATYPE "class java.lang.Double" [_] "REAL")
(defmethod DATATYPE "class [Ljava.lang.Object;" [_] "ARRAY")
(defmethod DATATYPE "class [LLjava.lang.Object;" [_] "ARRAY")
(defmethod DATATYPE "class SNOBOL4clojure.env.SnobolArray" [_] "ARRAY")
(defmethod DATATYPE "class clojure.lang.Atom" [_] "TABLE")
(deftype DataObj [type-name field-names ^objects fields serial])
(deftype CodeBlock [entry])
(defmethod DATATYPE "class SNOBOL4clojure.env.CodeBlock" [_] "CODE")
(defmethod DATATYPE "class SNOBOL4clojure.env.DataObj" [X] (.type-name ^DataObj X))
(defmethod DATATYPE "class clojure.lang.PersistentArrayMap" [X] (get X :__type__ "TABLE"))
(defmethod DATATYPE "class clojure.lang.PersistentHashMap" [X] (get X :__type__ "TABLE"))
(defmethod DATATYPE "class clojure.lang.PersistentVector" [_] "PATTERN")
(defmethod DATATYPE "class clojure.lang.Symbol" [_] "NAME")
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defmethod DATATYPE "class clojure.lang.PersistentList"
    [X]
    (cond
        ('#{
            SEQ ALT
            CAPTURE CAPTURE-IMM
            CAPTURE-COND CURSOR-IMM!
            COLLECT! ANY$
            NOTANY$ SPAN$
            NSPAN$ BREAK$
            BREAKX$ BREAKX#
            LEN# POS#
            RPOS# TAB#
            RTAB# BOL#
            EOL# FENCE!
            ARBNO! ARB!
            BAL! REM!
            ABORT! SUCCEED!
            FAIL! X}
            (first X))
            "PATTERN"
        (:pattern (meta X)) "PATTERN"
        :else "EXPRESSION"))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defmethod DATATYPE "class clojure.lang.PersistentList$EmptyList" [_] "EXPRESSION")
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn object-identity?
    [x]
    (or (and (or (seq? x) (vector? x)) (contains? #{"PATTERN" "EXPRESSION"} (DATATYPE x))) (and (instance? NAME x) (seq? (.n ^NAME x)) (clojure.core/= '& (first (.n ^NAME x))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn snobol-ident? [x y] (if (or (object-identity? x) (object-identity? y)) (identical? x y) (clojure.core/= x y)))
(defmethod DATATYPE "class clojure.lang.PersistentTreeMap" [_] "CODE")
(defmethod DATATYPE "class clojure.lang.Keyword" [_] "CODE")
(defmethod DATATYPE "class clojure.lang.PersistentHashSet" [_] "SET")
(defmethod DATATYPE "class clojure.lang.PersistentTreeSet" [_] "SET")
(defmethod DATATYPE "class java.util.regex.Pattern" [_] "REGEX")
(defmethod DATATYPE "class java.lang.Class" [_] "DATA")
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defmethod DATATYPE :default
    [X]
    (let [cn (str (class X))]
        (or
            (when (and (map? X) (:__type__ X)) (:__type__ X))
            (when-let [m (re-find #"class SNOBOL4clojure\.core\.(.*)" cn)] (m 1))
            (when-let [m (re-find #"class SNOBOL4clojure\.env\.(.*)" cn)] (m 1))
            (when (re-find #"NAME" cn) "NAME"))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- words-of-text [s] (quot (clojure.core/+ (count s) 7) 8))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn storage-words
    []
    (let [
          seen (java.util.IdentityHashMap.)
          work (java.util.ArrayDeque.)
          words
              (fn
                  [v]
                  (cond
                      (nil? v) 0
                      (string? v) (if (clojure.core/= "" v) 0 (clojure.core/+ 2 (words-of-text v)))
                      (number? v) 2
                      (or (keyword? v) (symbol? v)) 0
                      (.containsKey seen v) 0
                      :else
                          (do
                              (.put seen v true)
                              (cond
                                  (instance? NAME v) 2
                                  (instance? SnobolArray v)
                                      (let [dims (:dims v) n (reduce clojure.core/* 1 (map (fn [[lo hi]] (max 0 (inc (clojure.core/- hi lo)))) dims))]
                                          (when (some? (:dflt v)) (.add work (:dflt v)))
                                          (doseq [x (vals @(:data v))] (when (some? x) (.add work x)))
                                          (clojure.core/+ (if (and (clojure.core/= 1 (count dims)) (clojure.core/= 1 (ffirst dims))) 3 (clojure.core/+ 8 (clojure.core/* 2 (count dims)))) n))
                                  (table? v)
                                      (let [live (for [[k x] @v :when (not (or (nil? x) (clojure.core/= "" x)))] [k x])]
                                          (doseq [[k x] live] (when (some? k) (.add work k)) (.add work x))
                                          (clojure.core/+ 4 (long (:size (meta v) 11)) (clojure.core/* 4 (count live))))
                                  (instance? DataObj v) (let [fs (.fields ^DataObj v)] (doseq [x fs] (when (some? x) (.add work x))) (clojure.core/+ 3 (alength fs)))
                                  (or (seq? v) (vector? v)) (do (doseq [x v] (when (some? x) (.add work x))) 3)
                                  :else 2))))]
        (loop [total (reduce clojure.core/+ 0 (for [[sym v] @<VARS>] (do (when (some? v) (.add work v)) (clojure.core/+ 8 (words-of-text (name sym))))))]
            (if-let [v (.poll work)] (recur (clojure.core/+ total (long (words v)))) total))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- subscript-text [ks] (string/join "," (map (fn [k] (if (string? k) (str "'" k "'") (sstr k))) ks)))
(defn- name-text [^NAME v] (let [n (.n v)] (if (and (seq? n) (next n)) (str "." (first n) "<" (subscript-text (rest n)) ">") (str "." n))))
(defn- dims-text [dims] (string/join "," (map (fn [[lo hi]] (if (clojure.core/= lo 1) (str hi) (str lo ":" hi))) dims)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn value-text
    [v]
    (cond
        (string? v) (str "'" v "'")
        (integer? v) (str v)
        (double? v) (sstr v)
        (instance? NAME v) (name-text v)
        (array? v) (str "ARRAY(" (or (:proto v) (dims-text (:dims v))) ") #" (:serial v))
        (table? v) (str "TABLE(" (:size (meta v) 11) ") #" (:serial (meta v)))
        (instance? DataObj v) (str (.type-name ^DataObj v) " #" (.serial ^DataObj v))
        (symbol? (:defer-var (meta v))) (str "*" (:defer-var (meta v)))
        :else (DATATYPE v)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private pattern-variables '#{ABORT ARB BAL FAIL FENCE REM SUCCEED})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- named-patterns
    []
    (into {} (for [n @<NAMES> :let [s (symbol n)] :when (pattern-variables s)] [s (if (clojure.core/= s 'FENCE) @(ns-resolve (the-ns 'SNOBOL4clojure.operators) 'bare-fence) (primitive-pattern s))])))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- variables
    [lvl]
    (let [vars (cond-> (merge (named-patterns) (into {} (for [s @<PRIMS-READ>] [s (deref (ns-resolve (the-ns 'SNOBOL4clojure.core) s))])) @<VARS>) true (merge @<ASSOC-LAST>))]
        (if
            (>= (long lvl) 3)
            (sort-by
                (comp name key)
                (for [[k v] (merge (into {} (for [n (into @<NAMES> ["INPUT" "OUTPUT" "TERMINAL"])] [(symbol n) ""])) vars)] (clojure.lang.MapEntry/create k (if (or (nil? v) (fn? v)) "" v))))
            (sort-by (comp name key) (remove (fn [[_ v]] (or (nil? v) (clojure.core/= "" v) (fn? v))) vars)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private keyword-order
    '[ANCHOR CASE CODE DUMP ERRLIMIT ERRTEXT ERRTYPE FILE FNCLEVEL FTRACE FULLSCAN INPUT LASTFILE LASTLINE LASTNO LINE MAXLNGTH OUTPUT PROFILE RTNTYPE STCOUNT STLIMIT STNO TRACE TRIM])
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- keywords [] (for [k keyword-order] (let [v @@(ns-resolve (the-ns 'SNOBOL4clojure.env) (symbol (str "&" k)))] (str "&" k " = " (if (string? v) (str "'" v "'") (sstr v))))))
(defn- serial-of [v] (cond (array? v) (:serial v) (table? v) (:serial (meta v)) (instance? DataObj v) (.serial ^DataObj v)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- key-hash
    [k]
    (let [mag (fn [^long x] (cond (clojure.core/= x Long/MIN_VALUE) 0 (neg? x) (- x) :else x))]
        (cond
            (string? k)
                (let [b ^bytes (.getBytes ^String k "ISO-8859-1") n (alength b)]
                    (if
                        (zero? n)
                        0
                        (bit-and Long/MAX_VALUE
                            (reduce
                                (fn [h w] (bit-xor h (reduce (fn [acc j] (let [i (+ (* 8 w) j)] (if (< i n) (bit-or acc (bit-shift-left (bit-and 0xff (long (aget b i))) (* 8 j))) acc))) 0 (range 8))))
                                (long n)
                                (range (min 3 (quot (+ n 7) 8)))))))
            (integer? k) (mag (long k))
            (double? k) (mag (Double/doubleToRawLongBits k))
            :else nil)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- parts
    [v]
    (cond
        (array? v) (for [[k x] (sort-by key @(:data v))] [:sub k x])
        (table? v) (let [n (long (:size (meta v) 11))] (for [[k x] (sort-by (fn [[k _]] (if-some [h (key-hash k)] (mod h n) Long/MAX_VALUE)) (table-entries v))] [:sub [k] x]))
        (instance? DataObj v) (map (fn [f x] [:field f x]) (.field-names ^DataObj v) (seq (.fields ^DataObj v)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- objects
    [vars]
    (loop [todo (map val vars) seen (java.util.IdentityHashMap.) out []]
        (if-let [[v & more] (seq todo)]
            (if (and (serial-of v) (not (.containsKey seen v))) (do (.put seen v true) (recur (concat more (map #(nth % 2) (parts v))) seen (conj out v))) (recur more seen out))
            (sort-by serial-of out))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- object-lines
    [vars v]
    (let [holder (or (some (fn [[k x]] (when (identical? x v) (str k))) vars) (value-text v))]
        (concat
            ["" (value-text v)]
            (for [[kind label x] (parts v) :when (and (some? x) (not= "" x))]
                (case kind :sub (str holder "<" (subscript-text label) "> = " (value-text x)) :field (str label "(" holder ") = " (value-text x)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn dump-text
    [lvl]
    (let [vars (variables lvl) sec (fn [header lines] (concat ["" "" header ""] lines))]
        (str
            (string/join "\n"
                (mapcat
                    (fn [^String l] (let [w (long @<WIDTH>)] (if (<= (count l) w) [l] (map #(apply str %) (partition-all w l)))))
                    (concat
                        [""]
                        (sec "dump of natural variables" (map (fn [[k v]] (str k " = " (value-text v))) vars))
                        (sec "dump of keyword values" (keywords))
                        (when (>= lvl 2) (mapcat #(object-lines vars %) (objects vars))))))
            "\n\n\n\n")))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private page-lines 55)
(def ^:private identity-line "snobol4jvm version 0.2.0")
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- clock-line
    []
    (let [arch (System/getProperty "os.arch")]
        (str (if (#{"amd64" "x86_64"} arch) "x86-64" arch) "  " (.format (DateTimeFormatter/ofPattern "EEE MMM ppd HH:mm:ss yyyy" Locale/US) (LocalDateTime/now)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- cut [^String l] (let [width (long @<WIDTH>)] (if (<= (count l) width) [l] (map #(apply str %) (partition-all width l)))))
(defn- write! [l] (doseq [p (cut l)] (println p)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- page-line
    [^String title n]
    (let [pn (str "page " n) c (- (long @<WIDTH>) 8) t (if (< (count title) c) (str title (apply str (repeat (- c (count title)) \space))) title)]
        (str (subs t 0 c) pn (when (> (count t) (+ c (count pn))) (subs t (+ c (count pn)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- start-page!
    [st used]
    (swap! st update :page inc)
    (write! (page-line (:title @st) (:page @st)))
    (swap! st assoc :on-page used)
    (when-some [s (:subtitle @st)] (write! s) (swap! st update :on-page inc))
    (println))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- new-page! [st] (print "\n\n\n") (start-page! st 0))
(defn- open! [st] (when (zero? (long (:page @st))) (print "\n\n\n") (println identity-line) (println (clock-line)) (print "\n\n\n") (start-page! st 4) true))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- list-line!
    [st l]
    (when (:pending @st) (swap! st assoc :pending false) (when (pos? (long (:page @st))) (new-page! st)))
    (when-not (open! st) (when (>= (long (:on-page @st)) page-lines) (new-page! st)))
    (write! l)
    (swap! st update :on-page inc)
    (when (:double @st) (println) (swap! st update :on-page inc)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- numbered [n depth line] (let [line (string/replace line #"[ \t]+\z" "")] (if (pos? (long depth)) (format "%-5s%-3s%s" (or n "") depth line) (format "%-8s%s" (or n "") line))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- statement-numbers
    [stmts]
    (loop [ss stmts n 0 starts (sorted-map)]
        (if-let [[s l] (first ss)]
            (if (or (= :blank s) (not (#{\* \-} (first s)))) (recur (rest ss) (inc n) (if (contains? starts l) starts (assoc starts l (inc n)))) (recur (rest ss) n starts))
            starts)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn write-listing!
    [file]
    (let [
          {:keys [text stmts cards]} @<SOURCE>
          lines (string/split (str text) #"\n")
          starts (statement-numbers stmts)
          errs (group-by first @<DIAGNOSTICS>)
          fold false
          card (fn [^String l nm] (re-find (re-pattern (str (when fold "(?i)") "^-" nm "(?:\\s|,|$)")) l))
          st (atom {:page 0 :on-page 0 :title "" :subtitle nil :pending false :on false :print true :double false})]
        (loop [i 1 ls lines depth 0 places [] file (str file) fline 0 cards cards]
            (when-let [raw (first ls)]
                (let [l raw n (get starts i) ctl (string/starts-with? l "-") shown (fn [line] (when (and (:on @st) (or (not ctl) (:print @st))) (list-line! st (numbered n depth line))))]
                    (cond
                        (string/starts-with? l "-DEPTH-LEAVE") (recur (inc i) (rest ls) (max 0 (dec depth)) places file (inc fline) cards)
                        (string/starts-with? l "-FILE-LEAVE") (let [[f fl] (peek places)] (recur (inc i) (rest ls) (dec depth) (pop places) f fl cards))
                        (string/starts-with? l "-FILE-ENTER")
                            (let [[_ nm] (re-find #"^-FILE-ENTER \"([^\"]*)\"" l)] (shown (first cards)) (recur (inc i) (rest ls) (inc depth) (conj places [file (inc fline)]) nm 0 (rest cards)))
                        :else
                            (let [fline (inc fline)]
                                (if ctl
                                    (let [segs (if (or (card l "TITLE") (card l "STITL")) [l] (map #(if (string/starts-with? % "-") % (str "-" %)) (string/split l #",-")))]
                                        (doseq [c segs]
                                            (cond
                                                (card c "LIST") (swap! st assoc :on true)
                                                (card c "NOLIST") (swap! st assoc :on false)
                                                (card c "PRINT") (swap! st assoc :print true)
                                                (card c "NOPRINT") (swap! st assoc :print false)
                                                (card c "SINGLE") (swap! st assoc :double false)
                                                (card c "DOUBLE") (swap! st assoc :double true)
                                                (card c "TITLE") (swap! st assoc :title (string/replace (subs c 6) #"\A[ \t]+|[ \t]+\z" "") :subtitle "" :pending true)
                                                (card c "STITL") (swap! st assoc :subtitle (string/replace (subs c 6) #"\A[ \t]+|[ \t]+\z" "") :pending true)
                                                (card c "EJECT") (when (:on @st) (when-not (open! st) (new-page! st)))
                                                (card c "SPACE")
                                                    (when
                                                        (and (:on @st) (not (open! st)))
                                                        (let [k (if-let [[_ d] (re-find #"^-\S+\s+(\d+)" c)] (Long/parseLong d) 1)]
                                                            (if (< (+ (long (:on-page @st)) k) page-lines) (do (print (apply str (repeat k \newline))) (swap! st update :on-page + k)) (new-page! st))))
                                                :else nil))
                                        (shown l))
                                    (shown l))
                                (doseq [[k [_ code col]] (map-indexed vector (get errs i))]
                                    (let [
                                          stmt (numbered (when-not ctl (some-> (first (rsubseq starts <= i)) val)) depth l)
                                          mark (if col (str (numbered nil 0 "") (apply str (map #(if (= \tab %) \tab \space) (take (dec (long col)) l))) "!") "")
                                          msg
                                              (if col
                                                  (format "%s(%d,%d) : ERROR %03d -- %s" file fline col code (error-message code))
                                                  (format "%s(%d) : ERROR %03d -- %s" file fline code (error-message code)))]
                                        (when (and (zero? (long k)) (not (:on @st))) (list-line! st stmt))
                                        (when (pos? (long k)) (write! ""))
                                        (doseq [x [mark msg ""]] (write! x))
                                        (swap! st update :on-page + 3)
                                        (binding [*out* *err*] (doseq [x [(if (zero? (long k)) stmt "") mark msg ""]] (write! x)))))
                                (when-not
                                    (and n (re-find (if fold #"(?i)^END([ \t]|\z)" #"^(END|end)([ \t]|\z)") l))
                                    (recur (inc i) (rest ls) (if (and ctl (re-find #"^-INCLUDE\s+['\"]" l)) (inc depth) depth) places file fline cards)))))))
        (flush)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- row [label v] (str (format "%-21s" label) v))
(defn- regenerations [] (reduce + 0 (map #(max 0 (.getCollectionCount ^GarbageCollectorMXBean %)) (ManagementFactory/getGarbageCollectorMXBeans))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- report-lines
    [code text stmts start-ns]
    (let [rt (Runtime/getRuntime) used (- (.totalMemory rt) (.freeMemory rt)) file @&FILE]
        (concat
            [["" false]
                ["" false]
                ["" false]
                [(format "%s(%d) : ERROR %03d -- %s" file @<STMT-LINE> code text) true]
                ["" true]
                ["" true]
                ["" false]
                ["" false]
                ["" false]
                [(row "in file" file) true]
                [(row "in line" @<STMT-LINE>) true]
                [(row "in statement" @&STNO) true]]
            (when-not (neg? (long @&STLIMIT)) [[(row "stmts executed" stmts) true] [(row "execution time msec" (quot (- (System/nanoTime) start-ns) 1000000)) true]])
            [[(row "REGENERATIONS" (regenerations)) true] [(row "memory used (bytes)" used) false] [(row "memory left (bytes)" (- (.maxMemory rt) used)) false] ["" true]])))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn report!
    [code text stmts start-ns]
    (doseq [[l both?] (report-lines code text stmts start-ns)]
        (let [width (long @<WIDTH>) pieces (if (<= (count l) width) [l] (map #(apply str %) (partition-all width l)))]
            (doseq [p pieces] (println p))
            (when both? (binding [*out* *err*] (println (last pieces)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private registry <TRACES>)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- trace-kinds
    [type]
    (case
        (first (clojure.string/upper-case (str type)))
        \V [:value]
        \A [:access]
        \L [:label]
        \C [:call]
        \R [:return]
        \F [:call :return]
        \K [:keyword]
        (throw (ex-info "trace second argument is not trace type" {:snobol/error 199}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- reg-key [name kind] [(str name) kind])
(def ^:private elements SNOBOL4clojure.env/<ETRACES>)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- element-of
    [name]
    (when
        (instance? NAME name)
        (let [n (.n ^NAME name)]
            (when-let [c (and (seq? n) (:container (meta n)))] [c (vec (rest n)) (str (first n) "<" (clojure.string/join "," (map #(if (string? %) (str "'" % "'") (str %)) (rest n))) ">")]))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn trace!
    ([name type] (trace! name type nil nil))
    ([name type label] (trace! name type label nil))
    ([name type label user-fn]
        (let [kinds (trace-kinds type)]
            (when (and (some #{:keyword} kinds) (not (#{"ERRTYPE" "FNCLEVEL" "STCOUNT"} (str name)))) (throw (ex-info "trace first argument is not appropriate name" {:snobol/error 198})))
            (if-let [[c ks text] (element-of name)]
                (doseq [kind kinds]
                    (swap! elements (fn [es] (conj (vec (remove #(and (identical? c (:c %)) (= ks (:ks %)) (= kind (:kind %))) es)) {:c c :ks ks :kind kind :text text :label label :fn user-fn}))))
                (doseq [kind kinds] (swap! registry assoc (reg-key name kind) {:label label :fn user-fn}))))
        nil))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn stoptr!
    [name type]
    (if-let [[c ks _] (element-of name)]
        (let [kinds (set (trace-kinds type))] (swap! elements (fn [es] (vec (remove #(and (identical? c (:c %)) (= ks (:ks %)) (kinds (:kind %))) es)))))
        (doseq [kind (trace-kinds type)] (swap! registry dissoc (reg-key name kind))))
    nil)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn clear-all-traces! [] (reset! registry {}) (reset! elements []) (reset! &TRACE 0) nil)
(defn active? [name type] (boolean (some #(contains? @registry (reg-key name %)) (trace-kinds type))))
(defn any-active? [] (pos? (count @registry)))
(def ^:dynamic *trace-output* nil)
(defn- trace-println [& args] (let [w (or *trace-output* *out*) l (apply str args)] (binding [*out* w] (if (<= (count l) 120) (println l) (doseq [p (partition-all 120 l)] (println (apply str p)))))))
(defn- star-field [] (let [s (str "****" @&STNO)] (str s (apply str (repeat (max 0 (- 12 (count s))) \*)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- trace-val
    [v]
    (cond
        (or (SNOBOL4clojure.env/array? v) (SNOBOL4clojure.env/table? v) (instance? DataObj v)) (value-text v)
        (nil? v) "''"
        (string? v) (str "'" v "'")
        (instance? NAME v) (str "." (.n ^NAME v))
        (integer? v) (str v)
        (double? v) (real-str v)
        (and (or (list? v) (vector? v)) (= "PATTERN" (SNOBOL4clojure.env/DATATYPE v))) "PATTERN"
        :else (str v)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- emit-line! [text] (when (pos? @&TRACE) (swap! &TRACE dec) (trace-println (star-field) " " text)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- trace-fn
    [entry]
    (when-some [f (:fn entry)]
        (let [n (if (instance? NAME f) (str (.n ^NAME f)) (str f))]
            (or
                (get @<FUNS> n)
                (when (contains? SNOBOL4clojure.env/system-functions n) (let [invoke (ns-resolve (the-ns 'SNOBOL4clojure.operators) 'INVOKE)] (fn [& args] (apply @invoke (symbol n) args))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- call-trace-fn!
    [f item-name entry]
    (swap! &TRACE dec)
    (let [tr @&TRACE] (reset! &TRACE 0) (let [r (f (NAME. (symbol (str item-name))) (:label entry))] (reset! &TRACE tr) (reset! SNOBOL4clojure.env/&LINE 2) r)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-value!
    [sym new-val]
    (when-let [entry (@registry (reg-key sym :value))]
        (if-let [f (and (pos? @&TRACE) (trace-fn entry))] (call-trace-fn! f sym entry) (emit-line! (str (apply str (repeat @&FNCLEVEL \i)) " " sym " = " (trace-val new-val))))
        (:label entry)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-element!
    [c ks new-val]
    (when-let [e (and (seq @elements) (first (filter #(and (= :value (:kind %)) (identical? c (:c %)) (= (vec ks) (:ks %))) @elements)))]
        (if-let [f (and (pos? @&TRACE) (trace-fn e))]
            (call-trace-fn! f (:text e) e)
            (emit-line!
                (str
                    (apply str (repeat @&FNCLEVEL \i))
                    " "
                    (if-let [h (SNOBOL4clojure.env/holder-name c)] (str h (subs (:text e) (.indexOf ^String (:text e) "<"))) (:text e))
                    " = "
                    (trace-val new-val))))
        (:label e)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-access!
    [sym v]
    (when-let [entry (@registry (reg-key sym :access))]
        (if-let [f (trace-fn entry)] (call-trace-fn! f sym entry) (do (swap! &TRACE dec) (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " " sym " = " (trace-val v))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-keyword!
    [kw new-val]
    (when
        (pos? @&TRACE)
        (when-let [entry (@registry (reg-key kw :keyword))]
            (if-let [f (trace-fn entry)] (call-trace-fn! f kw entry) (do (swap! &TRACE dec) (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " &" kw " = " (trace-val new-val)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-label!
    [label-key]
    (when
        (pos? @&TRACE)
        (let [lname (if (keyword? label-key) (name label-key) (str label-key))]
            (when-let [entry (@registry (reg-key lname :label))]
                (if-let [f (trace-fn entry)] (call-trace-fn! f lname entry) (do (swap! &TRACE dec) (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " :(" lname ")")))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- function-line!
    [fname kind text & [caller-stno]]
    (let [line #(trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " " (text)) entry (@registry (reg-key fname kind))]
        (let [f (when (and entry (pos? @&TRACE)) (trace-fn entry))]
            (cond
                f (do (call-trace-fn! f fname entry) (when caller-stno (reset! &STNO caller-stno)) (when (pos? @&FTRACE) (swap! &FTRACE dec) (line)))
                (and entry (pos? @&TRACE)) (do (swap! &TRACE dec) (line))
                (pos? @&FTRACE) (do (swap! &FTRACE dec) (line))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-call! [fname args] (function-line! fname :call #(str fname "(" (clojure.string/join "," (map trace-val args)) ")")))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-return!
    [fname val signal & [caller-stno]]
    (function-line! fname :return #(case signal :freturn (str "FRETURN " fname) :nreturn (str "NRETURN " fname " = " (trace-val val)) (str "RETURN " fname " = " (trace-val val))) caller-stno))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn pattern-trace-active? [subject-var] (and (pos? @&TRACE) (or (@registry (reg-key subject-var :pattern)) (@registry (reg-key "*" :pattern)))))
(defn stcount-traced? [] (boolean (@registry (reg-key "STCOUNT" :keyword))))
(defn statement-trace-active? [] (and (pos? @&TRACE) (@registry (reg-key "*" :label))))
(defn enable-full-trace! [] (trace! "*" "LABEL") (trace! "*" "VALUE") (trace! "*" "CALL") (trace! "*" "RETURN"))
(defn disable-full-trace! [] (stoptr! "*" "LABEL") (stoptr! "*" "VALUE") (stoptr! "*" "CALL") (stoptr! "*" "RETURN"))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- string->real
    [x]
    (let [t (clojure.string/trim (str x))]
        (cond (or (clojure.core/= t "") (sign-then-blanks? x)) 0.0 (re-matches #"[+-]?\d+" t) (try (double (Long/parseLong t)) (catch NumberFormatException _ nil)) :else (parse-real (str x)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- real->integer
    [x]
    (let [lo (double Long/MIN_VALUE)] (if (and (clojure.core/<= lo x) (clojure.core/< x (clojure.core/- lo))) (let [i (long x)] (when-not (clojure.core/= i 2147483648) i)) Long/MIN_VALUE)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn integer-of
    [x]
    (cond
        (integer? x) x
        (double? x) (real->integer x)
        :else (let [t (clojure.string/trim (str x))] (if (re-matches #"[+-]?\d+" t) (try (Long/parseLong t) (catch NumberFormatException _ nil)) (when-some [d (string->real x)] (real->integer d))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- int-arg [x err] (if-some [n (integer-of x)] n (throw (ex-info "argument is not integer" {:snobol/error err}))))
(defn- str-arg ^String [x n] (gtstg x n))
(defn SIZE [s] (long (count (str-arg s 189))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn REPLACE
    [s1 s2 s3]
    (let [
          s3 (str-arg s3 168)
          s2 (str-arg s2 169)
          _ (when (or (zero? (count s2)) (not= (count s2) (count s3))) (throw (ex-info "null or unequally long 2nd, 3rd args to replace" {:snobol/error 171})))
          s1 (str-arg s1 170)]
        (apply str (map #(let [i (.lastIndexOf ^String s2 (str %))] (if (>= i 0) (.charAt ^String s3 i) %)) s1))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn DUPL
    [x i]
    (let [n (int-arg i 90)]
        (when-not
            (neg? n)
            (if
                (#{"PATTERN" "EXPRESSION"} (DATATYPE x))
                (if (zero? n) (list 'LEN# 0) (apply list 'SEQ (repeat n x)))
                (let [s (str-arg x 91) m (long @&MAXLNGTH)]
                    (cond
                        (clojure.core/> n 16777216) nil
                        (clojure.core/> (clojure.core/* n (count s)) m) (throw (ex-info "string length exceeds value of maxlngth keyword" {:snobol/error 205}))
                        :else (apply str (repeat n s))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn TRIM [s] (let [^String t (str-arg s 200)] (subs t 0 (loop [i (count t)] (if (and (pos? i) (#{\space \tab} (.charAt t (dec i)))) (recur (dec i)) i)))))
(defn REVERSE [s] (apply str (clojure.string/reverse (str-arg s 177))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn LPAD
    ([s n] (LPAD s n " "))
    ([s n fill]
        (let [f (str-arg fill 144) w (int-arg n 145) s-str (str-arg s 146) pad (clojure.core/- w (count s-str)) fill-c (if (clojure.core/= "" f) " " (subs f 0 1))]
            (if (clojure.core/<= pad 0) s-str (str (apply str (repeat pad fill-c)) s-str)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn RPAD
    ([s n] (RPAD s n " "))
    ([s n fill]
        (let [f (str-arg fill 178) w (int-arg n 179) s-str (str-arg s 180) pad (clojure.core/- w (count s-str)) fill-c (if (clojure.core/= "" f) " " (subs f 0 1))]
            (if (clojure.core/<= pad 0) s-str (str s-str (apply str (repeat pad fill-c)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SUBSTR
    [s i n]
    (let [n (int-arg n 192) i (int-arg i 193) s (str-arg s 194) len (count s)]
        (when
            (and (clojure.core/<= 1 i (inc len)) (clojure.core/>= n 0))
            (if (zero? n) (subs s (dec i)) (when (clojure.core/<= (clojure.core/+ (dec i) n) len) (subs s (dec i) (clojure.core/+ (dec i) n)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn table->array
    ([t] (table->array t false))
    ([t sorting?]
        (let [
              entries (remove (fn [[_ v]] (or (nil? v) (clojure.core/= "" v))) (table-entries t))
              entries
                  (if sorting?
                      (let [kh (deref (ns-resolve (the-ns 'SNOBOL4clojure.env) 'key-hash)) n (long (:size (meta t) 11))]
                          (sort-by (fn [[k _]] (if-some [h (kh k)] (clojure.core/- (mod h n)) 1)) entries))
                      entries)
              n (count entries)]
            (when (pos? n) (let [arr (ARRAY (str n ",2"))] (doseq [[i [k v]] (map-indexed vector entries)] (array-set arr [(inc i) 1] k) (array-set arr [(inc i) 2] v)) arr)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CONVERT
    [x t]
    "CONVERT(x, type) — coerce x to the named SNOBOL4 type.
   Returns nil (failure) if the conversion is not defined."
    (when-not (or (and (string? t) (seq t)) (number? t) (instance? SNOBOL4clojure.env.NAME t)) (throw (ex-info (error-message 74) {:snobol/error 74})))
    (let [from (clojure.string/upper-case (DATATYPE x)) to (if (zero? (long @&CASE)) (str t) (clojure.string/upper-case (str t)))]
        (cond
            (clojure.core/= from to) x
            (clojure.core/= to "NUMERIC") (cond (number? x) x (string? x) (let [n (num x)] (when-not (and (double? n) (Double/isNaN n)) n)) :else nil)
            (and (clojure.core/= from "STRING") (clojure.core/= to "INTEGER"))
                (let [t (clojure.string/trim (str x))]
                    (if (sign-then-blanks? x) 0 (if (re-matches #"[+-]?\d+" t) (try (Long/parseLong t) (catch NumberFormatException _ nil)) (when-some [d (string->real x)] (real->integer d)))))
            (and (clojure.core/= from "STRING") (clojure.core/= to "REAL")) (string->real x)
            (and (clojure.core/= from "STRING") (clojure.core/= to "PATTERN")) (list 'SEQ (list 'LIT$ (str x)))
            (and (clojure.core/= from "STRING") (clojure.core/= to "NAME")) (SNOBOL4clojure.env.NAME. (symbol (str x)))
            (and (clojure.core/= from "INTEGER") (clojure.core/= to "STRING")) (str x)
            (and (clojure.core/= from "INTEGER") (clojure.core/= to "REAL")) (double x)
            (and (clojure.core/= from "INTEGER") (clojure.core/= to "PATTERN")) (list 'SEQ (list 'LIT$ (str x)))
            (and (clojure.core/= from "INTEGER") (clojure.core/= to "NAME")) (SNOBOL4clojure.env.NAME. (symbol (str x)))
            (and (clojure.core/= from "REAL") (clojure.core/= to "STRING")) (sstr x)
            (and (clojure.core/= from "REAL") (clojure.core/= to "INTEGER")) (real->integer x)
            (and (clojure.core/= from "REAL") (clojure.core/= to "PATTERN")) (list 'SEQ (list 'LIT$ (sstr x)))
            (and (clojure.core/= from "REAL") (clojure.core/= to "NAME")) (SNOBOL4clojure.env.NAME. (symbol (sstr x)))
            (and (clojure.core/= from "ARRAY") (clojure.core/= to "TABLE"))
                (let [dims (:dims x)]
                    (if
                        (and (clojure.core/= (count dims) 2) (clojure.core/= (second (second dims)) 2) (clojure.core/= (first (second dims)) 1))
                        (let [[lo1 hi1] (first dims) t (TABLE (inc (clojure.core/- hi1 lo1)))]
                            (doseq [i (range lo1 (inc hi1))] (let [k (array-get x [i 1]) v (array-get x [i 2])] (when-not (nil? k) (table-set t k (or v ε)))))
                            t)
                        nil))
            (and (clojure.core/= from "TABLE") (clojure.core/= to "ARRAY")) (table->array x)
            (and (clojure.core/= from "PATTERN") (clojure.core/= to "PATTERN")) x
            (and (clojure.core/= from "NAME") (#{"STRING" "INTEGER" "REAL" "PATTERN"} to) (instance? SNOBOL4clojure.env.NAME x) (not (symbol? (.n ^SNOBOL4clojure.env.NAME x)))) nil
            (and (clojure.core/= from "NAME") (clojure.core/= to "STRING")) (str x)
            (and (clojure.core/= from "NAME") (clojure.core/= to "INTEGER")) (let [v (try (Long/parseLong (str x)) (catch Exception _ nil))] v)
            (and (clojure.core/= from "NAME") (clojure.core/= to "REAL")) (let [v (try (Double/parseDouble (str x)) (catch Exception _ nil))] v)
            (and (clojure.core/= from "NAME") (clojure.core/= to "PATTERN")) (list 'SEQ (list 'LIT$ (str x)))
            (and (clojure.core/= from "NAME") (clojure.core/= to "NAME")) x
            (and (clojure.core/= from "EXPRESSION") (clojure.core/= to "EXPRESSION")) x
            (and (clojure.core/= from "EXPRESSION") (clojure.core/= to "PATTERN")) (list 'SEQ x)
            (and (clojure.core/= from "CODE") (clojure.core/= to "CODE")) x
            :else nil)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn COPY
    [x]
    (cond
        (table? x) (let [t (atom @x :meta {:order (atom (if-let [o (:order (meta x))] @o {})) :size (:size (meta x) 11) :serial (SNOBOL4clojure.env/next-serial!)})] t)
        (array? x) (assoc (SNOBOL4clojure.env/->SnobolArray (:dims x) (:dflt x) (atom @(:data x))) :proto (:proto x) :serial (SNOBOL4clojure.env/next-serial!))
        (instance? SNOBOL4clojure.env.DataObj x)
            (SNOBOL4clojure.env/->DataObj
                (.type-name ^SNOBOL4clojure.env.DataObj x)
                (.field-names ^SNOBOL4clojure.env.DataObj x)
                (aclone ^objects (.fields ^SNOBOL4clojure.env.DataObj x))
                (SNOBOL4clojure.env/next-serial!))
        (instance? SNOBOL4clojure.env.CodeBlock x) (SNOBOL4clojure.env/->CodeBlock (.entry ^SNOBOL4clojure.env.CodeBlock x))
        (and (seq? x) (#{"PATTERN" "EXPRESSION"} (DATATYPE x))) (with-meta (apply list x) (meta x))
        (and (vector? x) (clojure.core/= "PATTERN" (DATATYPE x))) (with-meta (into [] x) (meta x))
        :else x))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CHAR [n] (let [n (int-arg n 281)] (if (clojure.core/<= 0 n 255) (str (char n)) (throw (ex-info "char argument not in range" {:snobol/error 282})))))
(defn ORD [s] (let [t (sstr s)] (when (seq t) (long (int (first t))))))
(declare finite-or)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn REMDR
    [x y]
    (let [a (num x) b (num y) nan? #(and (double? %) (Double/isNaN ^double %))]
        (when (nan? b) (throw (ex-info "remdr second argument is not numeric" {:snobol/error 165})))
        (when (nan? a) (throw (ex-info "remdr first argument is not numeric" {:snobol/error 166})))
        (if
            (and (integer? a) (integer? b))
            (if (zero? b) (throw (ex-info "remdr caused integer overflow" {:snobol/error 167})) (clojure.core/rem (long a) (long b)))
            (let [
                  a (double a)
                  q (clojure.core// a (double b))
                  _ (when (or (Double/isInfinite q) (Double/isNaN q)) (throw (ex-info "remdr caused real overflow" {:snobol/error 312})))
                  q (finite-or q 312 "remdr caused real overflow")
                  d (finite-or (clojure.core/- (finite-or (clojure.core/* (if (neg? q) (Math/ceil q) (Math/floor q)) (double b)) 312 "") a) 312 "")]
                (if (neg? a) (if (clojure.core/<= d 0.0) d (clojure.core/- d)) (if (neg? d) (clojure.core/- d) d))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- real-error [code msg] (throw (ex-info (or (error-message code) msg) {:snobol/error code})))
(defn- real-arg [x code fname] (let [n (num x)] (if (and (double? n) (Double/isNaN ^double n)) (real-error code (str fname " argument not numeric")) (double n))))
(defn- finite-or [^double d code msg] (cond (Double/isInfinite d) (real-error code msg) (< (Math/abs d) Double/MIN_NORMAL) (clojure.core/* d 0.0) :else d))
(defn ATAN [x] (Math/atan (real-arg x 301 "ATAN")))
(defn CHOP [x] (let [d (real-arg x 302 "CHOP")] (clojure.core/+ 0.0 (if (neg? d) (Math/ceil d) (Math/floor d)))))
(defn COS [x] (Math/cos (real-arg x 303 "COS")))
(defn EXP [x] (finite-or (Math/exp (real-arg x 304 "EXP")) 305 "EXP produced real overflow"))
(defn LN [x] (let [d (real-arg x 306 "LN")] (cond (neg? d) (real-error 315 "LN argument negative") (zero? d) (real-error 307 "LN produced real overflow") :else (Math/log d))))
(defn SIN [x] (Math/sin (real-arg x 308 "SIN")))
(defn TAN [x] (finite-or (Math/tan (real-arg x 309 "TAN")) 310 "TAN produced real overflow"))
(defn SQRT [x] (let [d (real-arg x 313 "SQRT")] (if (neg? d) (real-error 314 "SQRT argument negative") (Math/sqrt d))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn INTEGER
    [x]
    "Convert x to integer, or return ε (fail) if not convertible."
    (cond (integer? x) x (number? x) (long x) :else (try (Long/parseLong (clojure.string/trim (str x))) (catch Exception _ ε))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn integer-predicate [x] (cond (integer? x) ε (number? x) nil :else (when (re-matches #"|[+-]?[0-9]+" (clojure.string/trim (str x))) ε)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn REAL
    [x]
    "Convert x to real (double), or return ε (fail) if not convertible."
    (cond (float? x) x (number? x) (double x) :else (try (Double/parseDouble (clojure.string/trim (str x))) (catch Exception _ ε))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def data-type-registry (atom {}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- parse-data-spec
    [S]
    (let [s (clojure.string/trim S) lp (.indexOf ^String s "(") rp (.lastIndexOf ^String s ")")]
        (when
            (and (> lp 0) (>= rp (inc lp)))
            (let [
                  type-name (.substring ^String s 0 lp)
                  fields-str (.substring ^String s (inc lp) rp)
                  fields (if (clojure.string/blank? fields-str) [] (mapv (comp clojure.string/trim) (clojure.string/split fields-str #",")))]
                [type-name fields]))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- field-slot
    [f inst]
    (or
        (when (instance? SNOBOL4clojure.env.DataObj inst) (let [fields (.field-names ^SNOBOL4clojure.env.DataObj inst) i (.indexOf ^java.util.List (vec fields) f)] (when (>= i 0) i)))
        (throw (ex-info "field function argument is wrong datatype" {:snobol/error 41}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn field-fn
    [f]
    (fn
        ([inst] (let [i (int (field-slot f inst))] (aget ^objects (.fields ^SNOBOL4clojure.env.DataObj inst) i)))
        ([inst v] (let [i (int (field-slot f inst))] (aset ^objects (.fields ^SNOBOL4clojure.env.DataObj inst) i v) v))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn DATA!
    [S]
    (when-let [[type-name fields] (parse-data-spec S)]
        (let [tname type-name fnames fields]
            (swap! data-type-registry assoc tname fnames)
            `(do
                 (swap! SNOBOL4clojure.env/<FUNS> assoc ~tname
                     (with-meta
                         (fn [& args#] (SNOBOL4clojure.env/->DataObj ~tname ~fnames (object-array (map-indexed (fn [i# _#] (nth args# i# ε)) ~fnames)) (SNOBOL4clojure.env/next-serial!)))
                         {:data true}))
                 ~@(for [f fnames] `(swap! SNOBOL4clojure.env/<FUNS> assoc ~f (with-meta (field-fn ~f) {:data true :field true})))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn DATA [S] (when-let [form (DATA! S)] (when-let [[t fs] (parse-data-spec S)] (note-names! (cons t fs))) (eval form) ε))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn FIELD
    [type-name n]
    (let [n (if (clojure.core/= "" (str n)) 0 (int-arg n 107)) tname (str type-name) fields (get @data-type-registry tname)]
        (when-not fields (throw (ex-info "field first argument is not datatype name" {:snobol/error 108})))
        (when (and (>= (long n) 1) (<= (long n) (count fields))) (nth fields (dec (long n))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn DATE
    ([] (DATE 0))
    ([i]
        (let [n (if (or (nil? i) (clojure.core/= "" i)) 0 (SNOBOL4clojure.env/num i))]
            (when-not (integer? n) (throw (ex-info "date argument is not integer" {:snobol/error 330})))
            (.format (java.time.LocalDateTime/now) (java.time.format.DateTimeFormatter/ofPattern (case (long n) 1 "MM/dd/yyyy HH:mm:ss" 2 "yyyy-MM-dd HH:mm:ss" "MM/dd/yy HH:mm:ss"))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn TIME [] (System/currentTimeMillis))
(defn READ-LINE! [] (let [line (try (.readLine (stdin-reader)) (catch Exception _ nil))] (if (nil? line) ε (input-record line))))
(defn- channel-file ^java.io.File [^String file-str] (let [f (java.io.File. file-str)] (if (and *file-dir* (not (.isAbsolute f))) (java.io.File. (str *file-dir*) file-str) f)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn file-spec
    [s]
    (let [
          s (str s)
          n (count s)
          len
              (or
                  (when
                      (and (pos? n) (clojure.core/= \] (.charAt s (dec n))))
                      (loop [i (clojure.core/- n 2)] (cond (neg? i) nil (clojure.core/= \] (.charAt s i)) nil (clojure.core/= \[ (.charAt s i)) i :else (recur (dec i)))))
                  n)
          b (.indexOf (subs s 0 len) " ")
          nm (if (neg? b) (subs s 0 len) (subs s 0 b))
          [opts bad]
              (loop [i (count nm) dash false opts {}]
                  (if
                      (>= i n)
                      [opts false]
                      (let [
                            ch (Character/toUpperCase (.charAt s i))
                            num
                                (fn
                                    []
                                    (let [j (loop [j (inc i)] (if (and (< j n) (Character/isDigit (.charAt s j))) (recur (inc j)) j))] [(if (> j (inc i)) (Long/parseLong (subs s (inc i) j)) 0) j]))]
                          (case ch
                              (\space \tab \, \[ \]) (recur (inc i) false opts)
                              \- (if dash [opts true] (recur (inc i) true opts))
                              (\A \C \I \U \W \X \Y) (recur (inc i) false (assoc opts (Character/toLowerCase ch) true))
                              \F (let [[v j] (num)] (recur j false (assoc opts \f v)))
                              (\B \L \Q \R) (let [[v j] (num)] (if (pos? v) (recur j false (assoc opts (if (clojure.core/= ch \Q) \r (Character/toLowerCase ch)) v)) [opts true]))
                              \S
                                  (let [
                                        d (when (< (inc i) n) (Character/toUpperCase (.charAt s (inc i))))
                                        e (when (< (clojure.core/+ i 2) n) (Character/toUpperCase (.charAt s (clojure.core/+ i 2))))
                                        rw (and (clojure.core/= e \R) (< (clojure.core/+ i 3) n) (clojure.core/= \W (Character/toUpperCase (.charAt s (clojure.core/+ i 3)))))]
                                      (if (and (clojure.core/= d \D) (#{\N \R \W} e) (< (clojure.core/+ i (if rw 4 3)) (inc n))) (recur (clojure.core/+ i (if rw 4 3)) false opts) [opts true]))
                              [opts true]))))]
        {:name nm :opts opts :bad (boolean (or bad (and (seq nm) (contains? opts \f))))}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn channel-key [u] (if (integer? u) u (let [t (clojure.string/trim (if (nil? u) "" (sstr u)))] (cond (clojure.core/= "" t) 0 (re-matches #"[+-]?\d+" t) (Long/parseLong t) :else t))))
(defn- null-arg? [x] (or (nil? x) (clojure.core/= "" x)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- association
    [type unit filename bad]
    (let [{nm :name opts :opts} (file-spec (or filename "")) held (when-not (null-arg? unit) (get @<CHANNELS> (channel-key unit)))]
        (when
            (and held (not (clojure.core/= type (:type held))))
            (if (clojure.core/= type :input) (throw (ex-info "input channel currently in use" {:snobol/error 289})) (throw (ex-info "output file cannot be written to" {:snobol/error 161}))))
        (when
            (and held (not (null-arg? filename)))
            (if (clojure.core/= type :input) (throw (ex-info "input channel currently in use" {:snobol/error 289})) (throw (ex-info "output channel currently in use" {:snobol/error 290}))))
        (cond
            (null-arg? unit) (if (null-arg? filename) [0 nil nil] (bad))
            (or (not (clojure.string/blank? nm)) (contains? opts \f)) [(channel-key unit) filename nil]
            :else
                (let [k (channel-key unit) ch (get @<CHANNELS> k) e (when (string? unit) (not-empty (System/getenv (clojure.string/trim unit))))]
                    (cond (and ch (clojure.core/= type (:type ch))) [k nil ch] e [k e nil] :else (bad))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn open-input-channel!
    [var-sym unit filename]
    (let [
          bad #(throw (ex-info "inappropriate file specification for input" {:snobol/error 116}))
          [unit filename shared] (association :input unit filename bad)
          {nm :name opts :opts blank-in-name :bad} (file-spec (or filename ""))
          file-str (when-not (clojure.string/blank? nm) nm)
          _ (when blank-in-name (bad))
          rdr
              (cond
                  shared nil
                  file-str
                      (if
                          (clojure.string/starts-with? file-str "|")
                          (bad)
                          (try (java.io.BufferedReader. (java.io.InputStreamReader. (java.io.FileInputStream. (channel-file file-str)) "ISO-8859-1")) (catch Exception _ nil)))
                  :else (stdin-reader))]
        (cond
            shared (do (swap! <CHANNELS> assoc var-sym shared) ε)
            rdr
                (let [ch
                          (cond->
                              {:type :input :unit unit :var var-sym :reader rdr :file file-str :at (atom 0)}
                              (get opts \r) (assoc :raw (get opts \r))
                              (get opts \c) (assoc :raw 1)
                              (get opts \l) (assoc :maxlen (get opts \l)))]
                    (swap! <CHANNELS> assoc var-sym ch)
                    (swap! <CHANNELS> assoc unit ch)
                    ε))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn rewind-channel!
    [unit]
    (let [u (channel-key unit) ch (get @<CHANNELS> u)]
        (cond
            (nil? ch) (throw (ex-info "rewind file does not exist" {:snobol/error 174}))
            (and (clojure.core/= :input (:type ch)) (:file ch))
                (let [rdr (java.io.BufferedReader. (java.io.InputStreamReader. (java.io.FileInputStream. (channel-file (:file ch))) "ISO-8859-1")) ch' (assoc ch :reader rdr :at (atom 0))]
                    (try (.close ^java.io.BufferedReader (:reader ch)) (catch Exception _ nil))
                    (swap! <CHANNELS> assoc (:var ch) ch' (:unit ch) ch')
                    ε)
            :else ε)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SET
    ([] (SET nil nil nil))
    ([unit] (SET unit nil nil))
    ([unit offset] (SET unit offset nil))
    ([unit offset whence]
        (let [
              fail (fn [n] (throw (ex-info (error-message n) {:snobol/error n})))
              int-of (fn [x] (cond (or (nil? x) (clojure.core/= "" x)) 0 (integer? x) x (string? x) (let [t (clojure.string/trim x)] (when (re-matches #"[+-]?\d+" t) (Long/parseLong t)))))
              _ (when (or (nil? unit) (clojure.core/= "" unit)) (fail 292))
              ch (get @<CHANNELS> (when (or (integer? unit) (string? unit)) (int-of unit)))
              _ (when-not ch (fail 295))
              off (or (int-of offset) (fail 293))
              wh (or (int-of whence) (fail 293))
              _ (when-not (#{0 1 2} wh) (fail 297))
              f (when (:file ch) (channel-file (:file ch)))
              target (fn [cur size] (max 0 (min size (case (long wh) 0 off 1 (clojure.core/+ cur off) 2 (clojure.core/+ size off)))))]
            (cond
                (and (clojure.core/= :input (:type ch)) f)
                    (let [cur @(:at ch)]
                        (if
                            (and (zero? off) (clojure.core/= 1 wh))
                            cur
                            (let [
                                  to (target cur (.length ^java.io.File f))
                                  in (java.io.FileInputStream. f)
                                  _ (.skip in to)
                                  ch' (assoc ch :reader (java.io.BufferedReader. (java.io.InputStreamReader. in "ISO-8859-1")) :at (atom to))]
                                (try (.close ^java.io.BufferedReader (:reader ch)) (catch Exception _ nil))
                                (swap! <CHANNELS> assoc (:var ch) ch' (:unit ch) ch')
                                to)))
                (:stream ch)
                    (let [fc (.getChannel ^java.io.FileOutputStream (:stream ch))] (.flush ^java.io.PrintWriter (:writer ch)) (let [to (target (.position fc) (.size fc))] (.position fc (long to)) to))
                (and (zero? off) (clojure.core/= 1 wh)) (if-let [at (:at ch)] @at 0)
                :else (fail 296)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn open-output-channel!
    [var-sym unit filename]
    (let [
          bad #(throw (ex-info "inappropriate file specification for output" {:snobol/error 160}))
          [unit filename shared] (association :output unit filename bad)
          {nm :name opts :opts blank-in-name :bad} (file-spec (or filename ""))
          file-str (when-not (clojure.string/blank? nm) nm)
          _ (when blank-in-name (bad))
          out (when (and file-str (not shared) (not (clojure.string/starts-with? file-str "|"))) (try (java.io.FileOutputStream. (channel-file file-str) (contains? opts \a)) (catch Exception _ nil)))
          wtr
              (cond
                  shared nil
                  file-str (if (clojure.string/starts-with? file-str "|") (bad) (when out (java.io.PrintWriter. (java.io.OutputStreamWriter. ^java.io.OutputStream out "ISO-8859-1"))))
                  :else (java.io.PrintWriter. ^java.io.Writer (if (clojure.core/= 2 (get opts \f)) *err* *out*) true))]
        (cond
            shared (do (swap! <CHANNELS> assoc var-sym shared) ε)
            wtr (let [ch (cond-> {:type :output :unit unit :var var-sym :writer wtr :file file-str} out (assoc :stream out))] (swap! <CHANNELS> assoc var-sym ch) (swap! <CHANNELS> assoc unit ch) ε))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn close-channel!
    [key]
    (when-let [ch (get @<CHANNELS> key)]
        (try (when-let [rdr (:reader ch)] (.close ^java.io.BufferedReader rdr)) (when-let [wtr (:writer ch)] (.close ^java.io.PrintWriter wtr)) (catch Exception _ nil))
        (swap! <CHANNELS> (fn [m] (into {} (remove (fn [[_ v]] (and (map? v) (some? (:unit ch)) (clojure.core/= (:unit v) (:unit ch)))) (dissoc m (:var ch) (:unit ch))))))
        ε))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn write-to-channel!
    [var-sym val]
    (when-let [ch (get @<CHANNELS> var-sym)] (when (clojure.core/= :output (:type ch)) (let [wtr ^java.io.PrintWriter (:writer ch)] (.println wtr (sstr val)) (.flush wtr) val))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn BACKSPACE
    ([] (BACKSPACE nil))
    ([unit]
        (let [
              fail (fn [n m] (throw (ex-info m {:snobol/error n})))
              _ (when (or (nil? unit) (clojure.core/= "" unit)) (fail 316 "backspace argument is not a suitable name"))
              ch (get @<CHANNELS> (channel-key unit))
              _ (when-not ch (fail 317 "backspace file does not exist"))
              f (when (:file ch) (channel-file (:file ch)))
              back
                  (fn
                      [p]
                      (if-let [n (:raw ch)]
                          (max 0 (clojure.core/- p n))
                          (let [^bytes b (java.nio.file.Files/readAllBytes (.toPath ^java.io.File f)) eol? (fn [i] (clojure.core/== 10 (aget b (dec i))))]
                              (loop [i (if (and (pos? p) (eol? p)) (dec p) p)] (if (and (pos? i) (not (eol? i))) (recur (dec i)) i)))))]
            (cond
                (and (clojure.core/= :input (:type ch)) f)
                    (let [
                          to (back @(:at ch))
                          in (java.io.FileInputStream. ^java.io.File f)
                          _ (.skip in to)
                          ch' (assoc ch :reader (java.io.BufferedReader. (java.io.InputStreamReader. in "ISO-8859-1")) :at (atom to))]
                        (try (.close ^java.io.BufferedReader (:reader ch)) (catch Exception _ nil))
                        (swap! <CHANNELS> assoc (:var ch) ch' (:unit ch) ch')
                        ε)
                (:stream ch) (let [fc (.getChannel ^java.io.FileOutputStream (:stream ch))] (.flush ^java.io.PrintWriter (:writer ch)) (.position fc (long (back (.position fc)))) ε)
                :else (fail 318 "backspace file does not permit backspace")))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn DETACH ([] ε) ([var-sym] (close-channel! (symbol (str var-sym)))))
(defn EJECT [] ε)
(defn ENDFILE ([] ε) ([unit] (close-channel! unit)))
(defn INPUT [] (READ-LINE!))
(defn OUTPUT [] ε)
(defn REWIND ([] ε) ([unit] (when-let [ch (get @<CHANNELS> unit)] (when-let [rdr ^java.io.BufferedReader (:reader ch)] (try (.reset rdr) (catch Exception _ nil))) ε)))
(defn CLEAR [] ε)
(defn COLLECT [] ε)
(defn DUMP [] ε)
(defn EXIT [] nil)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn EXIT
    ([] (EXIT nil nil))
    ([a] (EXIT a nil))
    ([a b & _]
        (let [n (when-not (string? a) (SNOBOL4clojure.env/num a))]
            (cond
                (and (string? a) (seq a))
                    (do
                        (flush)
                        (let [p (-> (ProcessBuilder. ^java.util.List ["/bin/bash" "-c" a]) (.inheritIO) (.start)) code (long (.waitFor p))]
                            (throw (ex-info "exit" {:snobol/signal :exit :snobol/rc code}))))
                (and (integer? n) (clojure.core/= -3 n)) (throw (ex-info "exit" {:snobol/signal :exit :snobol/rc 0}))
                :else (throw (ex-info (error-message 105) {:snobol/error 105}))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn HOST
    ([] (str "jvm:" (System/getProperty "os.name") ":SNOBOL4clojure 0.2.0"))
    ([i & more]
        (if
            (or (nil? i) (clojure.core/= "" i))
            (HOST)
            (let [n (SNOBOL4clojure.env/num i) bad #(throw (ex-info "erroneous argument for host" {:snobol/error 254}))]
                (cond
                    (and (integer? n) (zero? n)) (str @SNOBOL4clojure.env/<HOST0>)
                    (and (integer? n) (clojure.core/= 1 n))
                        (let [cmd (str (or (first more) ""))]
                            (if
                                (clojure.core/= "" cmd)
                                0
                                (do
                                    (flush)
                                    (let [p (-> (ProcessBuilder. ^java.util.List ["/bin/sh" "-c" cmd]) (.inheritIO) (.start)) code (long (.waitFor p))]
                                        (if (clojure.core/> code 128) (clojure.core/- code 128) (clojure.core/* code 256))))))
                    (and (integer? n) (clojure.core/= 2 n))
                        (let [k (if (or (nil? (first more)) (clojure.core/= "" (first more))) 0 (SNOBOL4clojure.env/num (first more)))] (if (integer? k) (get @SNOBOL4clojure.env/<ARGV> k) (bad)))
                    (and (integer? n) (clojure.core/= 3 n)) (long @SNOBOL4clojure.env/<ARGI>)
                    (and (integer? n) (clojure.core/= 4 n)) (let [nm (str (or (first more) ""))] (if (clojure.core/= "" nm) (bad) (System/getenv nm)))
                    (and (integer? n) (clojure.core/= -1 n))
                        (let [k (SNOBOL4clojure.env/num (first more)) frames #(long (count (.getStackTrace (Thread/currentThread))))]
                            (if-not
                                (integer? k)
                                (bad)
                                (case
                                    (long k)
                                    0 1048576
                                    1 134217728
                                    2 4096
                                    3 (clojure.core/+ 4096 (.totalMemory (Runtime/getRuntime)))
                                    4 (clojure.core/- 4194304 400)
                                    5 (frames)
                                    6 8
                                    7 (frames)
                                    (bad))))
                    :else (bad))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private system-labels #{"ABORT" "CONTINUE" "SCONTINUE" "RETURN" "FRETURN" "NRETURN" "END" "end"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SETEXIT
    ([] (SETEXIT ε))
    ([label]
        (let [prev @<SETEXIT> s (gtstg label 187) s s]
            (when-not (or (clojure.core/= "" s) (system-labels s) (@<STNO> (keyword s))) (throw (ex-info (error-message 187) {:snobol/error 187})))
            (reset! <SETEXIT> (when-not (clojure.core/= "" s) (keyword s)))
            (if prev (SNOBOL4clojure.env.NAME. (symbol (name prev))) ε))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn STOPTR [] nil)
(defn TRACE [] nil)
(defn APPLY [fname & fargs] (let [f-sym (symbol (str fname)) f-fn (SNOBOL4clojure.env/$$fn f-sym)] (when (fn? f-fn) (apply f-fn fargs))))
(defn ARG [fname n] (when-let [meta (get @SNOBOL4clojure.env/<FDEFS> (str fname))] (when-some [p (nth (:params meta) (dec (long n)) nil)] (SNOBOL4clojure.env.NAME. (symbol p)))))
(defn LOCAL [fname n] (when-let [meta (get @SNOBOL4clojure.env/<FDEFS> (str fname))] (when-some [p (nth (:locals meta) (dec (long n)) nil)] (SNOBOL4clojure.env.NAME. (symbol p)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ITEM
    [c & ks]
    (cond (SNOBOL4clojure.env/table? c) (SNOBOL4clojure.env/table-get c (SNOBOL4clojure.env/table-subscript ks)) (SNOBOL4clojure.env/array? c) (SNOBOL4clojure.env/array-get c (vec ks)) :else nil))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn PROTOTYPE
    [x & _]
    (cond
        (SNOBOL4clojure.env/table? x) (:size (meta x) 11)
        (SNOBOL4clojure.env/array? x) (SNOBOL4clojure.env/array-prototype x)
        :else (throw (ex-info "prototype argument is not valid object" {:snobol/error 164}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private sort-type-rank {"INTEGER" 0 "NAME" 1 "PATTERN" 2 "REAL" 3 "CODE" 4 "STRING" 5 "EXPRESSION" 6 "TABLE" 7 "ARRAY" 8})
(defn- sort-serial [v] (cond (array? v) (:serial v) (table? v) (:serial (meta v)) (instance? SNOBOL4clojure.env.DataObj v) (.serial ^SNOBOL4clojure.env.DataObj v)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- sort-key-compare
    [a b]
    (cond
        (and (string? a) (string? b)) (compare a b)
        (and (number? a) (number? b)) (compare a b)
        :else
            (let [ta (str (DATATYPE a)) tb (str (DATATYPE b))]
                (if (clojure.core/= ta tb) (let [sa (sort-serial a) sb (sort-serial b)] (if (and sa sb) (compare sa sb) 0)) (compare (sort-type-rank ta 9) (sort-type-rank tb 9))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- sort-array
    [arr col descending?]
    (let [
          dims (:dims arr)
          lo1 (first (first dims))
          hi1 (second (first dims))
          two-d? (clojure.core/= 2 (count dims))
          _ (when (clojure.core/< 2 (count dims)) (throw (ex-info (error-message 256) {:snobol/error 256})))
          [lo2 hi2] (second dims)
          k
              (when two-d?
                  (let [k (if (or (nil? col) (clojure.core/= "" col)) lo2 (integer-of col))]
                      (if (and k (clojure.core/<= lo2 k hi2)) (clojure.core/- k lo2) (throw (ex-info (error-message 258) {:snobol/error 258})))))
          cols (when two-d? (range lo2 (inc hi2)))
          rows (for [i (range lo1 (inc hi1))] (if two-d? (vec (for [j cols] (array-get arr [i j]))) [(array-get arr [i])]))
          pick (fn [row] (if two-d? (nth row k) (first row)))
          cmp (if descending? #(sort-key-compare %2 %1) sort-key-compare)
          sorted (sort-by pick cmp rows)
          out (ARRAY (:proto arr))]
        (doseq [[i row] (map vector (range lo1 (inc hi1)) sorted)] (if two-d? (doseq [[j v] (map vector cols row)] (array-set out [i j] v)) (array-set out [i] (first row))))
        out))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- sort-any
    [A col descending?]
    (cond
        (table? A) (when-some [arr (table->array A true)] (sort-array arr col descending?))
        (array? A) (sort-array A col descending?)
        :else (throw (ex-info (error-message 256) {:snobol/error 256}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SORT ([A] (SORT A nil)) ([A col] (sort-any A col false)))
(defn RSORT ([A] (RSORT A nil)) ([A col] (sort-any A col true)))
(defn err [Σ Δ] [Σ (clojure.core/- -1 Δ)])
(defn SUCCEED! [Σ Δ _Π] [Σ Δ])
(defn FAIL! [Σ Δ _Π] (err Σ Δ))
(defn ABORT! [Σ Δ _Π] (err nil Δ))
(defn ARB! [Σ Δ _Π] (err nil Δ))
(defn BAL! [Σ Δ _Π] (err nil Δ))
(defn ARBNO! [Σ Δ _Π] (err nil Δ))
(defn FENCE! [Σ Δ _Π] (err nil Δ))
(defn FENCE!! [Σ Δ _Π] (err nil Δ))
(defn BREAKX$ [Σ Δ Π] (loop [σ Σ δ Δ] (if (not (seq σ)) (err σ δ) (if (contains? Π (first σ)) [σ δ] (recur (rest σ) (inc δ))))))
(defn POS# [Σ Δ Π] (if (equal Δ Π) [Σ Δ] (err Σ Δ)))
(defn RPOS# [Σ Δ Π] (if (equal (count Σ) Π) [Σ Δ] (err Σ Δ)))
(defn ANY$ [Σ Δ Π] (if (not (seq Σ)) (err Σ Δ) (if (contains? Π (first Σ)) [(rest Σ) (inc Δ)] (err Σ Δ))))
(defn NOTANY$ [Σ Δ Π] (if (not (seq Σ)) (err Σ Δ) (if-not (contains? Π (first Σ)) [(rest Σ) (inc Δ)] (err Σ Δ))))
(defn REM! [Σ Δ _Π] (loop [σ Σ δ Δ] (if (not (seq σ)) [σ δ] (recur (rest σ) (inc δ)))))
(defn LIT$ [Σ Δ Π] (loop [σ Σ δ Δ π Π] (if (not (seq π)) [σ δ] (if (not (seq σ)) (err σ δ) (if (not-equal (first σ) (first π)) (err σ δ) (recur (rest σ) (inc δ) (rest π)))))))
(defn LEN# [Σ Δ Π] (loop [σ Σ δ Δ] (if (>= δ (Σ+ Δ Π)) [σ δ] (if (not (seq σ)) (err σ δ) (recur (rest σ) (inc δ))))))
(defn TAB# [Σ Δ Π] (if (> Δ Π) (err Σ Δ) (loop [σ Σ δ Δ] (if (>= δ Π) [σ δ] (if (not (seq σ)) (err σ δ) (recur (rest σ) (inc δ)))))))
(defn RTAB# [Σ Δ Π] (loop [σ Σ δ Δ] (let [rem (count σ)] (cond (== rem Π) [σ δ] (< rem Π) (err Σ Δ) :else (recur (rest σ) (inc δ))))))
(defn BOL# [Σ Δ _Π] (if (clojure.core/= Δ 0) [Σ Δ] (err Σ Δ)))
(defn EOL# [Σ Δ _Π] (if (not (seq Σ)) [Σ Δ] (err Σ Δ)))
(defn NSPAN$ [Σ Δ Π] (loop [σ Σ δ Δ] (if (not (contains? Π (first σ))) [σ δ] (recur (rest σ) (inc δ)))))
(defn SPAN$ [Σ Δ Π] (loop [σ Σ δ Δ] (if (not (contains? Π (first σ))) (if (not-equal δ Δ) [σ δ] (err σ δ)) (recur (rest σ) (inc δ)))))
(defn BREAK$ [Σ Δ Π] (loop [σ Σ δ Δ] (if (not (seq σ)) (err σ δ) (if (contains? Π (first σ)) [σ δ] (recur (rest σ) (inc δ))))))
(defn charset [S] (reduce #(conj %1 %2) #{} S))
(defn- deferred? [S] (and (list? S) (clojure.core/= (first S) 'DEFER!)))
(defn- deferred-pattern [f] (with-meta (list 'DEFER! f) {:pattern true}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- set-of
    [S err]
    (let [s (if (or (string? S) (number? S) (and (instance? SNOBOL4clojure.env.NAME S) (symbol? (.n ^SNOBOL4clojure.env.NAME S)))) (sstr S) "")]
        (if (seq s) (charset s) (throw (ex-info (error-message err) {:snobol/error err})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- set-pattern-of [op S bad ebad] (if (deferred? S) (deferred-pattern (fn [] (when-some [v ((second S))] (list op (set-of v ebad))))) (list op (set-of S bad))))
(defn ANY [S] (set-pattern-of 'ANY$ S 59 43))
(defn BREAK [S] (set-pattern-of 'BREAK$ S 69 44))
(defn BREAKX [S] (set-pattern-of 'BREAKX# S 70 45))
(defn NOTANY [S] (set-pattern-of 'NOTANY$ S 151 49))
(defn NSPAN [S] (if (deferred? S) (deferred-pattern (fn [] (NSPAN (str ((second S)))))) (list 'NSPAN$ (charset S))))
(defn SPAN [S] (set-pattern-of 'SPAN$ S 188 56))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- int-pattern
    [op I bad big]
    (let [n (integer-of I)]
        (cond
            (nil? n) (throw (ex-info (error-message bad) {:snobol/error bad}))
            (or (neg? n) (clojure.core/> n 16777216)) (throw (ex-info (error-message big) {:snobol/error big}))
            :else (list op n))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- int-pattern-of [op I [bad big ebad ebig]] (if (deferred? I) (deferred-pattern (fn [] (when-some [v ((second I))] (int-pattern op v ebad ebig)))) (int-pattern op I bad big)))
(defn LEN [I] (int-pattern-of 'LEN# I [120 121 47 48]))
(defn POS [I] (int-pattern-of 'POS# I [162 163 50 51]))
(defn RPOS [I] (int-pattern-of 'RPOS# I [185 186 52 53]))
(defn RTAB [I] (int-pattern-of 'RTAB# I [181 182 54 55]))
(defn TAB [I] (int-pattern-of 'TAB# I [183 184 57 58]))
(def BOL (list 'BOL#))
(def EOL (list 'EOL#))
(defn ARBNO [P] (list 'ARBNO! P))
(defn FENCE ([] (list 'FENCE!)) ([P] (list 'FENCE! P)))
(defn CURSOR [N] (list 'CURSOR-IMM! N))
(defn DEFER [thunk] (list 'DEFER! thunk))
(def ARB (list 'ARB!))
(def BAL (list 'BAL!))
(def REM (list 'REM!))
(def ABORT (list 'ABORT!))
(def FAIL (list 'FAIL!))
(def SUCCEED (list 'SUCCEED!))
