(ns SNOBOL4clojure.core
    (:gen-class :main true :prefix "jar-")
    (:refer-clojure :exclude [= + - * / num])
    (:require [clojure.pprint :as pp] [clojure.string :as string] [instaparse.core :as insta :refer [defparser]] [clojure.edn :as edn] [clojure.string :as str])
    (:import
        [java.time LocalDateTime]
        [java.time.format DateTimeFormatter]
        [java.util Locale]
        [java.lang.management ManagementFactory GarbageCollectorMXBean]
        [clojure.asm ClassWriter MethodVisitor Label Opcodes]
        [clojure.lang DynamicClassLoader IFn RT]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def &ALPHABET (atom (apply str (map char (range 256)))))
(def &DIGITS "0123456789")
(def &LCASE "abcdefghijklmnopqrstuvwxyz")
(def &UCASE "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defmulti DATATYPE (fn [X] (str (class X))))
(defmethod DATATYPE "class java.lang.Character" [_] "STRING")
(defmethod DATATYPE "class java.lang.String" [_] "STRING")
(defmethod DATATYPE "class java.lang.Long" [_] "INTEGER")
(defmethod DATATYPE "class java.lang.Integer" [_] "INTEGER")
(defmethod DATATYPE "class java.lang.Double" [_] "REAL")
(defmethod DATATYPE "class [Ljava.lang.Object;" [_] "ARRAY")
(defmethod DATATYPE "class [LLjava.lang.Object;" [_] "ARRAY")
(defmethod DATATYPE "class SNOBOL4clojure.core.SnobolArray" [_] "ARRAY")
(defmethod DATATYPE "class clojure.lang.Atom" [_] "TABLE")
(defmethod DATATYPE "class SNOBOL4clojure.core.CodeBlock" [_] "CODE")
(deftype DataObj [type-name field-names ^objects fields serial])
(defmethod DATATYPE "class SNOBOL4clojure.core.DataObj" [X] (.type-name ^DataObj X))
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
(definterface &NAME (n []) (n [_]))
(declare name-identity)
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
(defn object-identity?
    [x]
    (or (and (or (seq? x) (vector? x)) (contains? #{"PATTERN" "EXPRESSION"} (DATATYPE x))) (and (instance? NAME x) (seq? (.n ^NAME x)) (clojure.core/= '& (first (.n ^NAME x))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn snobol-ident? [x y] (if (or (object-identity? x) (object-identity? y)) (identical? x y) (clojure.core/= x y)))
(def ε "")
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn REAL
    [x]
    "Convert x to real (double), or return ε (fail) if not convertible."
    (cond (float? x) x (number? x) (double x) :else (try (Double/parseDouble (clojure.string/trim (str x))) (catch Exception _ ε))))
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
(def ^:private error-text (merge (into {} (for [[n m] error-messages :when (not (#{274 276 278} n))] [n (clojure.string/lower-case m)])) {274 "value assigned to keyword fullscan is zero"}))
(defn error-message [n] (error-text n))
(def <CHANNELS> (atom {}))
(def ^:dynamic *file-dir* nil)
(defn- channel-file ^java.io.File [^String file-str] (let [f (java.io.File. file-str)] (if (and *file-dir* (not (.isAbsolute f))) (java.io.File. (str *file-dir*) file-str) f)))
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
(defn- deferred-arg? [S] (and (list? S) (clojure.core/= (first S) 'DEFER!)))
(defn- deferred-pattern [f] (with-meta (list 'DEFER! f) {:pattern true}))
(defn charset [S] (reduce #(conj %1 %2) #{} S))
(defn NSPAN [S] (if (deferred-arg? S) (deferred-pattern (fn [] (NSPAN (str ((second S)))))) (list 'NSPAN$ (charset S))))
(def BOL (list 'BOL#))
(def EOL (list 'EOL#))
(def ^:dynamic *pending-cond* nil)
(def ^:dynamic *matching* false)
(def &ERRLIMIT (atom 0))
(def &ERRTYPE (atom 0))
(def &TRACE (atom 0))
(def <TRACES> (atom {}))
(def registry <TRACES>)
(defn reg-key [name kind] [(str name) kind])
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
(def η ##NaN)
(defn & ([_n] nil) ([_x _y] η))
(def <VARS> (atom {}))
(defn reference [N] (get @<VARS> (symbol (name N))))
(defn- program-fn [op] (when (and (symbol? op) (Character/isLetter (.charAt (name op) 0))) (let [v (reference op)] (if (fn? v) v (get @<FUNS> (name op))))))
(defn x-2 [op x y] (list op x y))
(defn x-n [op x y Ω] (apply list (conj Ω y x op)))
(defn | ([_x] η) ([x y] (x-2 'ALT x y)) ([x y & zs] (x-n 'ALT x y zs)))
(defrecord SnobolArray [dims dflt data])
(defn array? [x] (instance? SnobolArray x))
(defn table? [x] (and (instance? clojure.lang.IDeref x) (map? @x)))
(deftype CodeBlock [entry])
(defn- textless? [v] (or (and (instance? NAME v) (not (symbol? (.n ^NAME v)))) (array? v) (table? v) (instance? DataObj v) (instance? CodeBlock v)))
(defn- pattern-argument [p err] (if (textless? p) (throw (ex-info (error-message err) {:snobol/error err})) p))
(defn- pattern-operands! [args] (let [v (vec args) n (count v)] (pattern-argument (v (dec n)) 5) (doseq [i (range (clojure.core/- n 2) -1 -1)] (pattern-argument (v i) 6))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- read-raw
    [^java.io.Reader rdr n]
    (let [buf (char-array n)]
        (loop [k 0] (if (< k n) (let [r (.read rdr buf k (clojure.core/- n k))] (if (neg? r) (when (pos? k) (String. buf 0 k)) (recur (clojure.core/+ k r)))) (String. buf 0 k)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- read-line-counted
    [^java.io.BufferedReader rdr at]
    (let [sb (StringBuilder.)]
        (loop [n 0]
            (let [c (.read rdr)]
                (cond
                    (neg? c) (do (swap! at clojure.core/+ n) (when (pos? n) (str sb)))
                    (== c 10) (do (swap! at clojure.core/+ n 1) (str sb))
                    (== c 13) (do (.mark rdr 1) (if (== (.read rdr) 10) (swap! at clojure.core/+ n 2) (do (.reset rdr) (swap! at clojure.core/+ n 1))) (str sb))
                    :else (do (.append sb (char c)) (recur (inc n))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn channel-read
    [ch]
    (let [rdr ^java.io.BufferedReader (:reader ch) at (:at ch)]
        (if-let [n (:raw ch)] (let [s (read-raw rdr n)] (when (and s at) (swap! at clojure.core/+ (count s))) s) (if at (read-line-counted rdr at) (.readLine rdr)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private record-length 1024)
(def &TRIM (atom 1))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn input-record
    ([line] (input-record line nil))
    ([line maxlen] (let [s (str line) n (or maxlen record-length) s (if (> (count s) n) (subs s 0 n) s)] (if (zero? (long @&TRIM)) s (clojure.string/replace s #"[ \t]+$" "")))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def <ASSOC-LAST> (atom {}))
(def &INPUT (atom 1))
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
(def protected-variables '#{ABORT ARB BAL FAIL FENCE REM SUCCEED})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private builtin-names
    '#{
       ABORT ANY
       APPLY ARB
       ARBNO ARRAY
       ATAN BAL
       BOL BREAK
       BREAKX CHAR
       CHOP CODE
       CONVERT COPY
       COS DATA
       DATATYPE DIFFER
       DUPL EOL
       EQ EVAL
       EXP FAIL
       FENCE FIELD
       FULLMATCH GE
       GLOBALS GT
       IDENT INTEGER
       INVOKE ITEM
       LE LEN
       LEQ LGE
       LGT LLE
       LLT LN
       LNE LT
       MATCH NE
       NOTANY NSPAN
       ORD POS
       PROTOTYPE REAL
       REM REMDR
       REPLACE REVERSE
       RPOS RSORT
       RTAB RUN
       SEARCH SET
       SIN SIZE
       SORT SPAN
       SQRT SUCCEED
       TAB TABLE
       TAN TRIM})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def <PRIMS-READ> (atom #{}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn primitive-pattern
    [sym]
    (let [resolved (when (builtin-names sym) (ns-resolve (find-ns 'SNOBOL4clojure.core) sym)) v (when (instance? clojure.lang.Var resolved) (var-get resolved))]
        (if (nil? v) ε (do (when-not (clojure.core/= sym 'FENCE) (swap! <PRIMS-READ> conj sym)) v))))
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
(defn $ ([n] ($$ n)) ([x y] (x-2 'CAPTURE-IMM x y)) ([x y & zs] (x-n 'CAPTURE-IMM x y zs)))
(defn . ([x] (NAME. x)) ([x y] (x-2 'CAPTURE-COND x y)) ([x y & zs] (x-n 'CAPTURE-COND x y zs)))
(defn- real->integer [x] (let [lo (double Long/MIN_VALUE)] (if (and (<= lo x) (< x (clojure.core/- lo))) (let [i (long x)] (when-not (clojure.core/= i 2147483648) i)) Long/MIN_VALUE)))
(defn sign-then-blanks? [x] (and (string? x) (boolean (re-matches #"[ \t]*[+-][ \t]+" x))))
(defn- ftz ^double [^double x] (if (< (Math/abs x) Double/MIN_NORMAL) (clojure.core/* x 0.0) x))
(def ^:private ten-powers (double-array [1.0 1e1 1e2 1e3 1e4 1e5 1e6 1e7 1e8 1e9 1e10]))
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
(defn- string->real
    [x]
    (let [t (clojure.string/trim (str x))]
        (cond (or (clojure.core/= t "") (sign-then-blanks? x)) 0.0 (re-matches #"[+-]?\d+" t) (try (double (Long/parseLong t)) (catch NumberFormatException _ nil)) :else (parse-real (str x)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn integer-of
    [x]
    (cond
        (integer? x) x
        (double? x) (real->integer x)
        :else (let [t (clojure.string/trim (str x))] (if (re-matches #"[+-]?\d+" t) (try (Long/parseLong t) (catch NumberFormatException _ nil)) (when-some [d (string->real x)] (real->integer d))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- int-pattern
    [op I bad big]
    (let [n (integer-of I)]
        (cond (nil? n) (throw (ex-info (error-message bad) {:snobol/error bad})) (or (neg? n) (> n 16777216)) (throw (ex-info (error-message big) {:snobol/error big})) :else (list op n))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- int-pattern-of [op I [bad big ebad ebig]] (if (deferred-arg? I) (deferred-pattern (fn [] (when-some [v ((second I))] (int-pattern op v ebad ebig)))) (int-pattern op I bad big)))
(defn LEN [I] (int-pattern-of 'LEN# I [120 121 47 48]))
(defn POS [I] (int-pattern-of 'POS# I [162 163 50 51]))
(defn RPOS [I] (int-pattern-of 'RPOS# I [185 186 52 53]))
(defn TAB [I] (int-pattern-of 'TAB# I [183 184 57 58]))
(defn RTAB [I] (int-pattern-of 'RTAB# I [181 182 54 55]))
(def ^:private gts-scale (reduce (fn [a _] (clojure.core/* a 10.0)) 1.0 (range 15)))
(def ^:private gts-round (clojure.core// 0.5 gts-scale))
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
                      fixed (<= 0 e 15)
                      nd (if fixed (clojure.core/- 15 e) 15)
                      p (long (Math/pow 10.0 nd))
                      frac (if (zero? nd) "" (clojure.string/replace (format (str "%0" nd "d") (rem n p)) #"0+$" ""))]
                    (str (when (neg? x) "-") (quot n p) "." frac (when-not fixed (str "e" (if (neg? e) "-" "+") (Math/abs e))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn sstr [x] (cond (nil? x) "" (double? x) (real-str x) :else (str x)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- set-of
    [S err]
    (let [s (if (or (string? S) (number? S) (and (instance? NAME S) (symbol? (.n ^NAME S)))) (sstr S) "")] (if (seq s) (charset s) (throw (ex-info (error-message err) {:snobol/error err})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- set-pattern-of [op S bad ebad] (if (deferred-arg? S) (deferred-pattern (fn [] (when-some [v ((second S))] (list op (set-of v ebad))))) (list op (set-of S bad))))
(defn ANY [S] (set-pattern-of 'ANY$ S 59 43))
(defn BREAK [S] (set-pattern-of 'BREAK$ S 69 44))
(defn BREAKX [S] (set-pattern-of 'BREAKX# S 70 45))
(defn NOTANY [S] (set-pattern-of 'NOTANY$ S 151 49))
(defn SPAN [S] (set-pattern-of 'SPAN$ S 188 56))
(defn FENCE ([] (list 'FENCE!)) ([P] (list 'FENCE! P)))
(defn ARBNO [P] (list 'ARBNO! P))
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
(defn- operand [x code msg] (let [n (num x)] (if (and (double? n) (Double/isNaN n) (not (double? x))) (throw (ex-info msg {:snobol/error code})) n)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defmacro ^:private numeric-predicate
    [f cmp first-code second-code]
    (let [nm (clojure.string/lower-case (name f))]
        `(defn ~f
             [& args#]
             (let [b# (operand (second args#) ~second-code ~(str nm " second argument is not numeric")) a# (operand (first args#) ~first-code ~(str nm " first argument is not numeric"))]
                 (when (~cmp a# b#) ε)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(numeric-predicate EQ == 101 102)
(numeric-predicate NE (fn [a b] (not (== a b))) 149 150)
(numeric-predicate LE <= 118 119)
(numeric-predicate LT < 147 148)
(numeric-predicate GE >= 109 110)
(numeric-predicate GT > 111 112)
(defn ncvt [x] (list 'num x))
(defmacro numcvt [x] `(ncvt ~x))
(defn n-1 [op x] (list op (numcvt x)))
(defn Σ+ ([x y] (+' x y)) ([x] (+' x)))
(defn n-2 [op x y] (list op (numcvt x) (numcvt y)))
(defn n-n [op x y Ω] (apply list op (map ncvt (conj Ω y x))))
(defn + ([x] (n-1 Σ+ x)) ([x y] (n-2 Σ+ x y)) ([x y & zs] (n-n Σ+ x y zs)))
(defn- exact [exact-op a b code msg] (try (exact-op (long a) (long b)) (catch ArithmeticException _ (throw (ex-info msg {:snobol/error code})))))
(defn- finite [r code msg] (let [r (double r)] (cond (Double/isInfinite r) (throw (ex-info msg {:snobol/error code})) (< (Math/abs r) Double/MIN_NORMAL) (clojure.core/* r 0.0) :else r)))
(defn subtract ([x y] (-' x y)) ([x] (-' x)))
(defn - ([x] (n-1 subtract x)) ([x y] (n-2 subtract x y)) ([x y & zs] (n-n subtract x y zs)))
(defn multiply ([x y] (*' x y)) ([x] (*' x)))
(defn * ([x] (list 'DEFER! (fn [] x))) ([x y] (n-2 multiply x y)) ([x y & zs] (n-n multiply x y zs)))
(defn divide [x y] (if (and (integer? x) (integer? y)) (quot x y) (clojure.core// x y)))
(defn / ([_x] η) ([x y] (n-2 divide x y)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn pow-fn
    [x y]
    (let [
          y (operand y 15 "exponentiation right operand is not numeric")
          x (operand x 16 "exponentiation left operand is not numeric")
          ovf "exponentiation caused real overflow"
          undefined #(throw (ex-info "exponentiation result is undefined" {:snobol/error 18}))]
        (cond
            (and (integer? x) (integer? y) (not (neg? y)))
                (cond
                    (and (zero? x) (zero? y)) (undefined)
                    (zero? x) 0
                    (clojure.core/= x 1) 1
                    (clojure.core/= x -1) (if (even? y) 1 -1)
                    (> y 64) (throw (ex-info "exponentiation caused integer overflow" {:snobol/error 17}))
                    :else (let [r (.pow (biginteger x) (int y))] (if (< (.bitLength r) 64) (long r) (throw (ex-info "exponentiation caused integer overflow" {:snobol/error 17})))))
            (and (integer? y) (not (neg? y)))
                (let [b (double x)] (if (zero? y) (if (zero? b) (undefined) 1.0) (loop [r b k (dec (long y))] (if (or (zero? k) (zero? r)) r (recur (finite (clojure.core/* r b) 266 ovf) (dec k))))))
            :else
                (let [b (double x) e (double y)]
                    (if
                        (zero? b)
                        (if (zero? e) (undefined) 0.0)
                        (let [c
                                  (when
                                      (neg? b)
                                      (let [t (if (neg? e) (Math/ceil e) (Math/floor e)) c (if (< (Math/abs t) 9.223372036854775807E18) (long t) Long/MIN_VALUE)]
                                          (when (== c 2147483648) (throw (ex-info ovf {:snobol/error 266})))
                                          (when-not (== t e) (throw (ex-info "exponentiation of negative base to non-integral power" {:snobol/error 311})))
                                          c))
                              r (finite (Math/exp (finite (clojure.core/* (finite (Math/log (Math/abs b)) 266 ovf) e) 266 ovf)) 266 ovf)]
                            (if (and c (odd? c)) (clojure.core/- r) r)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ** ([x y] (pow-fn x y)))
(defn annihilate [_x] nil)
(def ^:dynamic *trace* false)
(defn pattern-trace-active? [subject-var] (and (pos? @&TRACE) (or (@registry (reg-key subject-var :pattern)) (@registry (reg-key "*" :pattern)))))
(def &ANCHOR (atom 0))
(defn ζΠ [ζ] (if ζ (ζ 4)))
(defn out [item] (binding [pp/*print-right-margin* 2048 pp/*print-miser-width* 2000] (pp/pprint item)) item)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ζλ
    [ζ]
    (cond
        (nil? ζ) nil
        (list? ζ) (first ζ)
        (nil? (ζΠ ζ)) nil
        (string? (ζΠ ζ)) 'LIT$
        (number? (ζΠ ζ)) 'LIT$
        (instance? NAME (ζΠ ζ)) 'LIT$
        (symbol? (ζΠ ζ)) (ζΠ ζ)
        (vector? (ζΠ ζ)) 'SEQ
        (list? (ζΠ ζ)) (first (ζΠ ζ))
        (seq? (ζΠ ζ)) (first (ζΠ ζ))
        true (out ["ζλ? " (type (ζΠ ζ)) (ζΠ ζ)])))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ζΨ [ζ] (if ζ (ζ 6)))
(defn ζφ [ζ] (if ζ (ζ 5)))
(defn ζΔ [ζ] (if ζ (ζ 1)))
(defn ζΣ [ζ] (if ζ (ζ 0)))
(defn equal [x y] (clojure.core/= x y))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- preview
    ([action X φ] (preview action X 0 0 φ))
    ([action X pos depth φ]
        (str
            (if (> pos 0) " " "")
            (cond
                (nil? X) "nil"
                (char? X) (str "\\" X)
                (string? X) (str "\"" X "\"")
                (integer? X) (str X)
                (symbol? X) (str X)
                (float? X) (str X)
                (>= depth 3) "_"
                (vector? X) (str "[" (reduce str (map #(preview action %1 %2 (inc depth) 0) X (range))) "]")
                (list? X)
                    (str "("
                        (reduce str
                            (map
                                #(cond
                                     (equal %2 0) (str %1 " ")
                                     (> φ 0)
                                         (cond
                                             (< %2 φ) "."
                                             (> %2 (clojure.core/+ φ 2)) "?"
                                             (>= %2 (clojure.core/+ φ 2)) " ?"
                                             (and (equal %2 φ) (identical? action :succeed)) "."
                                             true (preview action %1 (dec %2) (inc depth) 0))
                                     true (preview action %1 (dec %2) (inc depth) 0))
                                X
                                (range)))
                        ")")
                (set? X) (str "\"" (apply str X) "\"")
                true (str " Yikes!!! " (type X))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- animate
    [action full-subject ζ]
    (when
        (and *trace* full-subject ζ (vector? ζ))
        (println
            (format "%2s %-8s %16s %3d %16s %-9s %s"
                (count (ζΨ ζ))
                (str (ζλ ζ) "/" (ζφ ζ))
                (str "\"" (apply str (take (ζΔ ζ) full-subject)) "\"")
                (ζΔ ζ)
                (str "\"" (apply str (reverse (ζΣ ζ))) "\"")
                (str " " action)
                (preview action (ζΠ ζ) (ζφ ζ))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ζδ [ζ] (if ζ (ζ 3)))
(defn- ζω [ζ] (>= (ζφ ζ) (count (ζΠ ζ))))
(defn- push [Ψ ζ] (if Ψ (conj Ψ ζ)))
(defn- 🡥 [Ω ζ] (push Ω ζ))
(defn- ζ↓ ([ζ Π] (let [[Σ Δ _ _ _ _ Ψ] ζ] [Σ Δ Σ Δ Π 1 Ψ])) ([ζ] (let [[Σ Δ _ _ Π φ Ψ] ζ] [Σ Δ Σ Δ (nth Π φ) 1 (🡥 Ψ ζ)])))
(defn- top [Ψ] (peek Ψ))
(defn- 🡡 [Ω] (top Ω))
(defn- pull [Ψ] (if Ψ (if-not (empty? Ψ) (pop Ψ))))
(defn- 🡧 [Ω] (pull Ω))
(defn- ζ← [ζ] (let [[Σ Δ _ _ Π φ Ψ] ζ] [Σ Δ Σ Δ Π (inc φ) Ψ]))
(defn- ζ↑ ([ζ σ δ] (let [[Σ Δ _ _ _ _ Ψ] ζ] [Σ Δ σ δ (ζΠ (🡡 Ψ)) (ζφ (🡡 Ψ)) (🡧 Ψ)])) ([ζ] (let [[Σ Δ σ δ _ _ Ψ] ζ] [Σ Δ σ δ (ζΠ (🡡 Ψ)) (ζφ (🡡 Ψ)) (🡧 Ψ)])))
(defn- ζ→ [ζ] (let [[_ _ σ δ Π φ Ψ] ζ] [σ δ σ δ Π (inc φ) Ψ]))
(defn err [Σ Δ] [Σ (clojure.core/- -1 Δ)])
(defn not-equal [x y] (not= x y))
(defn LIT$ [Σ Δ Π] (loop [σ Σ δ Δ π Π] (if (not (seq π)) [σ δ] (if (not (seq σ)) (err σ δ) (if (not-equal (first σ) (first π)) (err σ δ) (recur (rest σ) (inc δ) (rest π)))))))
(defn SUCCEED! [Σ Δ _Π] [Σ Δ])
(defn FAIL! [Σ Δ _Π] (err Σ Δ))
(defn ANY$ [Σ Δ Π] (if (not (seq Σ)) (err Σ Δ) (if (contains? Π (first Σ)) [(rest Σ) (inc Δ)] (err Σ Δ))))
(defn NOTANY$ [Σ Δ Π] (if (not (seq Σ)) (err Σ Δ) (if-not (contains? Π (first Σ)) [(rest Σ) (inc Δ)] (err Σ Δ))))
(defn SPAN$ [Σ Δ Π] (loop [σ Σ δ Δ] (if (not (contains? Π (first σ))) (if (not-equal δ Δ) [σ δ] (err σ δ)) (recur (rest σ) (inc δ)))))
(defn NSPAN$ [Σ Δ Π] (loop [σ Σ δ Δ] (if (not (contains? Π (first σ))) [σ δ] (recur (rest σ) (inc δ)))))
(defn BREAK$ [Σ Δ Π] (loop [σ Σ δ Δ] (if (not (seq σ)) (err σ δ) (if (contains? Π (first σ)) [σ δ] (recur (rest σ) (inc δ))))))
(defn BREAKX$ [Σ Δ Π] (loop [σ Σ δ Δ] (if (not (seq σ)) (err σ δ) (if (contains? Π (first σ)) [σ δ] (recur (rest σ) (inc δ))))))
(defn POS# [Σ Δ Π] (if (equal Δ Π) [Σ Δ] (err Σ Δ)))
(defn RPOS# [Σ Δ Π] (if (equal (count Σ) Π) [Σ Δ] (err Σ Δ)))
(defn LEN# [Σ Δ Π] (loop [σ Σ δ Δ] (if (>= δ (Σ+ Δ Π)) [σ δ] (if (not (seq σ)) (err σ δ) (recur (rest σ) (inc δ))))))
(defn TAB# [Σ Δ Π] (if (> Δ Π) (err Σ Δ) (loop [σ Σ δ Δ] (if (>= δ Π) [σ δ] (if (not (seq σ)) (err σ δ) (recur (rest σ) (inc δ)))))))
(defn RTAB# [Σ Δ Π] (loop [σ Σ δ Δ] (let [rem (count σ)] (cond (== rem Π) [σ δ] (< rem Π) (err Σ Δ) :else (recur (rest σ) (inc δ))))))
(defn BOL# [Σ Δ _Π] (if (clojure.core/= Δ 0) [Σ Δ] (err Σ Δ)))
(defn EOL# [Σ Δ _Π] (if (not (seq Σ)) [Σ Δ] (err Σ Δ)))
(def ^:private prim-fns {'ANY$ ANY$ 'NOTANY$ NOTANY$ 'SPAN$ SPAN$ 'NSPAN$ NSPAN$ 'BREAK$ BREAK$ 'BREAKX$ BREAKX$ 'POS# POS# 'RPOS# RPOS# 'LEN# LEN# 'TAB# TAB# 'RTAB# RTAB# 'BOL# BOL# 'EOL# EOL#})
(defn output-associated? [sym] (clojure.core/= :output (:type (get @<CHANNELS> sym))))
(defn snobol-set! [sym val] (let [s (symbol (name sym))] (swap! <VARS> assoc s val) (when (output-associated? s) (swap! <ASSOC-LAST> assoc s val))) val)
(defn- assign! [target v] (if (fn? target) (some? (target v)) (do (snobol-set! target v) true)))
(defn FENCE! [Σ Δ _Π] (err nil Δ))
(defn % ([_x] η) ([_x _y] η))
(defn ARB! [Σ Δ _Π] (err nil Δ))
(defn ARBNO! [Σ Δ _Π] (err nil Δ))
(defn ABORT! [Σ Δ _Π] (err nil Δ))
(defn REM! [Σ Δ _Π] (loop [σ Σ δ Δ] (if (not (seq σ)) [σ δ] (recur (rest σ) (inc δ)))))
(defn BAL! [Σ Δ _Π] (err nil Δ))
(declare engine)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn COLLECT!
    [S P]
    (let [chars (seq S) n (count S) bag (atom []) node (list 'COLLECT! bag P)]
        (loop [i 0] (when (and (<= i n) (not (clojure.core/= :abort (engine (drop i chars) i node i S)))) (recur (inc i))))
        @bag))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn engine
    [Σ Δ Π start-Δ full-subject]
    (loop [action :proceed ζ [Σ Δ Σ Δ Π 1 []] Ω []]
        (let [λ (ζλ ζ)]
            (animate action full-subject ζ)
            (case λ
                nil (case action (:proceed :succeed) [start-Δ (ζδ ζ)] (:recede :fail) (when (clojure.core/= :ABORT ζ) :abort))
                ALT
                    (case action
                        :proceed (if (not (ζω ζ)) (recur :proceed (ζ↓ ζ) (🡥 Ω ζ)) (recur :recede (🡡 Ω) (🡧 Ω)))
                        :recede (recur :proceed (ζ← ζ) Ω)
                        :succeed (recur :succeed (ζ↑ ζ) Ω)
                        :fail (recur :proceed (ζ← ζ) (🡧 Ω)))
                SEQ (case action :proceed (if (not (ζω ζ)) (recur :proceed (ζ↓ ζ) Ω) (recur :succeed (ζ↑ ζ) Ω)) :succeed (recur :proceed (ζ→ ζ) Ω) :fail (recur :recede (🡡 Ω) (🡧 Ω)))
                LIT$
                    (case action
                        :proceed
                            (let [[Σ Δ _ _ Π] ζ [σ δ] (LIT$ Σ Δ (cond (string? Π) Π (seq? Π) (str (second Π)) :else (sstr Π)))] (if (>= δ 0) (recur :succeed (ζ↑ ζ σ δ) Ω) (recur :fail (ζ↑ ζ Σ Δ) Ω))))
                SUCCEED! (let [[Σ Δ] ζ] (recur :succeed (ζ↑ ζ Σ Δ) Ω))
                FAIL! (let [[Σ Δ] ζ] (recur :fail (ζ↑ ζ Σ Δ) Ω))
                (ANY$ NOTANY$ SPAN$ NSPAN$ BREAK$ BREAKX$ POS# RPOS# LEN# TAB# RTAB# BOL# EOL#)
                    (case action :proceed (let [[Σ Δ _ _ Π] ζ [σ δ] ((prim-fns λ) Σ Δ (second Π))] (if (>= δ 0) (recur :succeed (ζ↑ ζ σ δ) Ω) (recur :fail (ζ↑ ζ Σ Δ) Ω))))
                X (case action :proceed (let [Π ($$ 'X)] (recur :proceed (ζ↓ ζ Π) Ω)))
                BREAKX#
                    (case action
                        :proceed (let [[Σ Δ _ _ Π] ζ cs (second Π) [σ δ] (BREAKX$ Σ Δ cs)] (if (>= δ 0) (recur :succeed (ζ↑ ζ σ δ) (🡥 Ω (assoc ζ 5 (inc δ)))) (recur :fail (ζ↑ ζ Σ Δ) Ω)))
                        :recede
                            (let [retry-Δ (ζφ ζ) full-len (count full-subject)]
                                (if
                                    (> retry-Δ full-len)
                                    (recur :recede (🡡 Ω) (🡧 Ω))
                                    (let [Σ-retry (drop retry-Δ (seq full-subject)) cs (second (ζΠ ζ)) [σ δ] (BREAKX$ Σ-retry retry-Δ cs)]
                                        (if (>= δ 0) (recur :succeed (ζ↑ ζ σ δ) (🡥 Ω (assoc ζ 5 (inc δ)))) (recur :recede (🡡 Ω) (🡧 Ω))))))
                        :succeed (recur :succeed (ζ↑ ζ) Ω)
                        :fail (recur :recede (🡡 Ω) (🡧 Ω)))
                CAPTURE-IMM
                    (case action
                        :proceed (let [[_ child-P var-sym] (ζΠ ζ) ζ (assoc ζ 4 (list 'CAPTURE-IMM child-P var-sym (ζΔ ζ)))] (recur :proceed (ζ↓ ζ) (🡥 Ω ζ)))
                        :succeed
                            (let [[_ _ var-sym start-pos] (ζΠ ζ) matched-text (subs full-subject start-pos (ζδ ζ))]
                                (if (assign! var-sym matched-text) (recur :succeed (ζ↑ ζ) Ω) (recur :recede (🡡 Ω) (🡧 Ω))))
                        (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))
                CAPTURE
                    (case action
                        :proceed (recur :proceed (ζ↓ ζ) (🡥 Ω ζ))
                        :succeed (let [[_ _ var-sym] (ζΠ ζ) matched-text (subs full-subject (ζΔ ζ) (ζδ ζ))] (assign! var-sym matched-text) (recur :succeed (ζ↑ ζ) Ω))
                        (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))
                CAPTURE-COND
                    (case action
                        :proceed
                            (let [[_ child-P var-sym] (ζΠ ζ) start-pos (ζΔ ζ) expanded (vector (list 'COND-MARK! start-pos) child-P (list 'COND-ASSIGN! var-sym start-pos))]
                                (recur :proceed (ζ↓ ζ expanded) (🡥 Ω ζ)))
                        :succeed (recur :succeed (ζ↑ ζ) Ω)
                        (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))
                COND-MARK! (case action :proceed (recur :succeed (ζ↑ ζ) Ω) :succeed (recur :succeed (ζ↑ ζ) Ω) (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))
                COND-ASSIGN!
                    (case action
                        :proceed
                            (let [[_ var-sym start-pos] (ζΠ ζ) end-pos (ζΔ ζ) matched-text (subs full-subject start-pos end-pos)]
                                (when *pending-cond* (vswap! *pending-cond* conj [var-sym matched-text]))
                                (recur :succeed (ζ↑ ζ) (🡥 Ω (list 'COND-UNDO! var-sym))))
                        :succeed (recur :succeed (ζ↑ ζ) Ω)
                        (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))
                COND-UNDO!
                    (let [[_ var-sym] ζ]
                        (when *pending-cond*
                            (vswap! *pending-cond*
                                (fn [lst] (if-let [i (last (keep-indexed (fn [i [vs _]] (when (clojure.core/= vs var-sym) i)) lst))] (into (subvec lst 0 i) (subvec lst (inc i))) lst))))
                        (recur :recede (🡡 Ω) (🡧 Ω)))
                FENCE!
                    (case action
                        :proceed
                            (let [[Σ Δ _ _ _ _ Ψ] ζ child (second (ζΠ ζ))]
                                (if child (let [ζ (assoc ζ 4 (list 'FENCE! child (count Ω)))] (recur :proceed [Σ Δ Σ Δ child 1 (🡥 Ψ ζ)] (🡥 Ω ζ))) (recur :succeed (ζ↑ ζ Σ Δ) (🡥 Ω :ABORT))))
                        :succeed (let [d (long (nth (ζΠ ζ) 2))] (recur :succeed (ζ↑ ζ) (into (subvec Ω 0 d) (filter #(and (list? %) (clojure.core/= 'COND-UNDO! (first %))) (subvec Ω (inc d))))))
                        (:recede :fail) (recur :recede (🡡 Ω) (🡧 Ω)))
                ARB!
                    (case action
                        :proceed (recur :succeed (ζ↑ ζ (ζΣ ζ) (ζΔ ζ)) (🡥 Ω (assoc ζ 5 1)))
                        :recede
                            (let [len (ζφ ζ) remaining (count (ζΣ ζ))]
                                (if (<= len remaining) (recur :succeed (ζ↑ ζ (drop len (ζΣ ζ)) (clojure.core/+ (ζΔ ζ) len)) (🡥 Ω (assoc ζ 5 (inc len)))) (recur :recede (🡡 Ω) (🡧 Ω)))))
                ARBNO!
                    (case action
                        :proceed (let [[Σ Δ] ζ] (recur :succeed (ζ↑ ζ Σ Δ) (🡥 Ω ζ)))
                        :recede (let [[Σ Δ _ _ _ _ Ψ] ζ] (recur :proceed [Σ Δ Σ Δ (second (ζΠ ζ)) 1 (🡥 Ψ (assoc ζ 5 Δ))] Ω))
                        :succeed (let [[_ _ σ δ Π start Ψ] ζ] (if (clojure.core/= δ start) (recur :succeed (ζ↑ ζ) Ω) (recur :succeed (ζ↑ ζ) (🡥 Ω [σ δ σ δ Π 1 Ψ]))))
                        :fail (recur :recede (🡡 Ω) (🡧 Ω)))
                ABORT! :abort
                REM! (case action :proceed (recur :succeed (ζ↑ ζ [] (count full-subject)) Ω) (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))
                BAL!
                    (let [
                          sublen (count full-subject)
                          pos0 (ζΔ ζ)
                          scan-bal
                              (fn
                                  [pos nest]
                                  (loop [p pos n nest]
                                      (cond
                                          (>= p sublen) nil
                                          :else
                                              (let [ch (nth full-subject p) n2 (case ch \( (inc n) \) (dec n) n)]
                                                  (cond (< n2 0) nil (clojure.core/= n2 0) [(inc p) (inc p) 0] :else (recur (inc p) n2))))))]
                        (case action
                            :proceed (if-let [[end np nn] (scan-bal pos0 0)] (recur :succeed (ζ↑ ζ (drop end (seq full-subject)) end) (🡥 Ω (assoc ζ 5 [np nn]))) (recur :fail (ζ↑ ζ (ζΣ ζ) pos0) Ω))
                            :recede
                                (let [[rp rn] (ζφ ζ)]
                                    (if-let [[end np nn] (scan-bal rp rn)] (recur :succeed (ζ↑ ζ (drop end (seq full-subject)) end) (🡥 Ω (assoc ζ 5 [np nn]))) (recur :recede (🡡 Ω) (🡧 Ω))))
                            :succeed (recur :succeed (ζ↑ ζ) Ω)
                            :fail (recur :recede (🡡 Ω) (🡧 Ω))))
                COLLECT!
                    (case action
                        :proceed (let [[S D _ _ Pi _ Psi] ζ P (nth Pi 2) chi [S D S D P 1 (🡥 Psi ζ)]] (recur :proceed chi (🡥 Ω ζ)))
                        :succeed (let [bag (nth (ζΠ ζ) 1)] (swap! bag conj [(ζΔ ζ) (ζδ ζ)]) (recur :recede (🡡 Ω) (🡧 Ω)))
                        (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))
                CURSOR-IMM!
                    (case action
                        (:proceed :succeed) (if (assign! (second (ζΠ ζ)) (ζδ ζ)) (recur :succeed (ζ↑ ζ (ζΣ ζ) (ζδ ζ)) Ω) (recur :recede (🡡 Ω) (🡧 Ω)))
                        (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))
                DEFER!
                    (case action
                        :proceed
                            (let [
                                  thunk (second (ζΠ ζ))
                                  P (if (fn? thunk) (thunk) thunk)
                                  _ (when (or (array? P) (table? P) (instance? DataObj P)) (throw (ex-info (error-message 46) {:snobol/error 46})))
                                  _ (when (> (count (nth ζ 6)) 200000) (throw (ex-info (error-message 246) {:snobol/error 246 :snobol/abort true})))]
                                (if (nil? P) (let [[Σ Δ] ζ] (recur :fail (ζ↑ ζ Σ Δ) Ω)) (recur :proceed (ζ↓ ζ P) (🡥 Ω ζ))))
                        :succeed (recur :succeed (ζ↑ ζ) Ω)
                        (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- commit-pending! [] (every? (fn [[var-sym text]] (assign! var-sym text)) @*pending-cond*))
(declare matching)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SEARCH
    [S P]
    (binding [*trace* (or *trace* (pattern-trace-active? S)) *pending-cond* (volatile! [])]
        (matching
            #(let [chars (seq S) n (if (zero? @&ANCHOR) (count S) 0)]
                 (loop [i 0]
                     (when
                         (<= i n)
                         (vswap! *pending-cond* (constantly []))
                         (let [result (engine (drop i chars) i P i S)] (cond (clojure.core/= :abort result) nil result (when (commit-pending!) result) :else (recur (inc i))))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn match-replace [_n _s _p] nil)
(defn ? ([x] (annihilate x)) ([s p] (let [s (sstr s)] (when-let [[a b] (SEARCH s p)] (subs s a b)))) ([n s p] (match-replace n s p)))
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
(defn = ([_x] η) ([n x] (list '= n x)))
(deftype ExprVar [ir])
(def &LINE (atom 0))
(defn- call-trace-fn! [f item-name entry] (swap! &TRACE dec) (let [tr @&TRACE] (reset! &TRACE 0) (let [r (f (NAME. (symbol (str item-name))) (:label entry))] (reset! &TRACE tr) (reset! &LINE 2) r)))
(def ^:dynamic *trace-output* nil)
(defn- trace-println [& args] (let [w (or *trace-output* *out*) l (apply str args)] (binding [*out* w] (if (<= (count l) 120) (println l) (doseq [p (partition-all 120 l)] (println (apply str p)))))))
(def &STNO (atom 0))
(defn- star-field [] (let [s (str "****" @&STNO)] (str s (apply str (repeat (max 0 (clojure.core/- 12 (count s))) \*)))))
(def &FNCLEVEL (atom 0))
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
(defn- trace-val
    [v]
    (cond
        (or (array? v) (table? v) (instance? DataObj v)) (value-text v)
        (nil? v) "''"
        (string? v) (str "'" v "'")
        (instance? NAME v) (str "." (.n ^NAME v))
        (integer? v) (str v)
        (double? v) (real-str v)
        (and (or (list? v) (vector? v)) (clojure.core/= "PATTERN" (DATATYPE v))) "PATTERN"
        :else (str v)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare trace-fn)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-access!
    [sym v]
    (when-let [entry (@registry (reg-key sym :access))]
        (if-let [f (trace-fn entry)] (call-trace-fn! f sym entry) (do (swap! &TRACE dec) (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " " sym " = " (trace-val v))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private bare-fence (FENCE))
(def ^:dynamic *fp* 0)
(def ^:dynamic *handlers* 0)
(def <STNO> (atom {}))
(def <LABL> (atom {}))
(def <CODE> (atom {}))
(defrecord NameResult [target])
(def ^:private special-targets #{"RETURN" "FRETURN" "NRETURN" "END" "end" :RETURN :FRETURN :NRETURN :END :end})
(def <SETEXIT> (atom nil))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- level-zero-return!
    []
    (when (zero? (long @&FNCLEVEL)) (throw (ex-info (error-message 242) (if (and (pos? (long @&ERRLIMIT)) @<SETEXIT>) {:snobol/error 242} {:snobol/error 242 :snobol/abort true})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private return-signal (clojure.lang.ExceptionInfo. "RETURN" {:snobol/signal :return}))
(defn snobol-return! [] (throw return-signal))
(def ^:private freturn-signal (clojure.lang.ExceptionInfo. "FRETURN" {:snobol/signal :freturn}))
(defn snobol-freturn! [] (throw freturn-signal))
(def ^:private nreturn-signal (clojure.lang.ExceptionInfo. "NRETURN" {:snobol/signal :nreturn}))
(defn snobol-nreturn! [] (throw nreturn-signal))
(def ^:private end-signal (clojure.lang.ExceptionInfo. "END" {:snobol/signal :end}))
(defn snobol-end! [] (throw end-signal))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- dispatch-special!
    [target]
    (when (#{"RETURN" "FRETURN" "NRETURN"} (clojure.string/upper-case (name target))) (level-zero-return!))
    (case
        (clojure.string/upper-case (name target))
        "RETURN" (snobol-return!)
        "FRETURN" (snobol-freturn!)
        "NRETURN" (snobol-nreturn!)
        "END" (if (@<STNO> (keyword (name target))) (snobol-end!) (throw (ex-info "END" {:snobol/signal :end :snobol/closing true})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- label-name [tgt] (when (or (keyword? tgt) (string? tgt) (symbol? tgt)) (name tgt)))
(def <ERRSERIAL> (atom 0))
(def <ERRFT> (atom 0))
(defn- fatal-error [n] (let [msg (error-message n)] (if (< (long (swap! <ERRFT> inc)) 3) (ex-info msg {:snobol/error n}) (ex-info msg {:snobol/error n :snobol/abort true}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-label!
    [label-key]
    (when
        (pos? @&TRACE)
        (let [lname (if (keyword? label-key) (name label-key) (str label-key))]
            (when-let [entry (@registry (reg-key lname :label))]
                (if-let [f (trace-fn entry)] (call-trace-fn! f lname entry) (do (swap! &TRACE dec) (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " :(" lname ")")))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defrecord Transfer [at err])
(def <STAGE> (atom :xt))
(def ^:dynamic *flprt* -1)
(def <GTCEF> (atom 0))
(def <XC> (atom false))
(def &ERRTEXT (atom ε))
(declare fire-keyword!)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn error-decided
    [^clojure.lang.ExceptionInfo e d]
    (let [{n :snobol/error abort :snobol/abort :as data} (ex-data e)]
        (if
            (or (nil? n) abort (contains? data :snobol/to) (contains? data :snobol/label))
            e
            (let [ee? (clojure.core/= :ee @<STAGE>) gtcef? (and ee? (< (long *flprt*) (long @<GTCEF>))) lim (long @&ERRLIMIT)]
                (if
                    (and (zero? lim) (not ee?) (not *matching*) (not (and @<XC> (pos? (long @&FNCLEVEL)))))
                    e
                    (let [h (when (pos? lim) @<SETEXIT>) k (swap! <ERRSERIAL> inc) to (if gtcef? @<GTCEF> d)]
                        (when gtcef? (reset! <STAGE> :xt))
                        (when (pos? lim) (swap! &ERRLIMIT dec))
                        (reset! &ERRTYPE n)
                        (fire-keyword! "ERRTYPE" n)
                        (reset! &ERRTEXT (ex-message e))
                        (when h (reset! <SETEXIT> nil))
                        (ex-info (ex-message e) (if h (assoc data :snobol/label h :snobol/serial k :snobol/fp d :snobol/resume to) (assoc data :snobol/to to)) e)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defrecord Trapped [label ex])
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- trap
    [^clojure.lang.ExceptionInfo e]
    (if
        (clojure.core/= :transfer (:snobol/signal (ex-data e)))
        (->Transfer (:snobol/at (ex-data e)) (:snobol/err (ex-data e)))
        (let [e (error-decided e *fp*) {to :snobol/to h :snobol/label} (ex-data e)] (cond h (->Trapped h e) to false :else (throw e)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private system-labels #{"CONTINUE" "ABORT" "RETURN" "FRETURN" "NRETURN" "END" "end"})
(def &STCOUNT (atom 0))
(def <ST-DEAD> (atom nil))
(def &LASTNO (atom 0))
(def &LASTFILE (atom ε))
(def &FILE (atom ε))
(def &LASTLINE (atom 0))
(def <POS> (atom {}))
(def <STMT-LINE> (atom 0))
(def <DISPNO> (atom {}))
(def &STLIMIT (atom 2147483647))
(defn stcount-traced? [] (boolean (@registry (reg-key "STCOUNT" :keyword))))
(def &PROFILE (atom 0))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- stlimit-over?
    [n step]
    (let [L (long @&STLIMIT) d @<ST-DEAD>] (and (pos? L) (not= d :never) (if (or (pos? (long step)) (nil? d)) (> (clojure.core/+ (long n) (long step)) L) (>= (long n) (long d))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn snobol-steplimit! [n] (throw (ex-info (str "Step limit exceeded at step " n) {:snobol/signal :step-limit :steps n})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- overrun
    [n]
    (let [lim (long @&STLIMIT)]
        (reset! &STCOUNT lim)
        (swap! &STLIMIT clojure.core/+ 10)
        (when (number? @<ST-DEAD>) (reset! <ST-DEAD> (clojure.core/+ lim (long @&STLIMIT))))
        (if (and (pos? (long @&ERRLIMIT)) (< (long (swap! <ERRFT> inc)) 3)) (ex-info (error-message 244) {:snobol/error 244}) (snobol-steplimit! n))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn statement-trace-active? [] (and (pos? @&TRACE) (@registry (reg-key "*" :label))))
(def block-end [::block-end])
(def compile-error [::compile-error])
(declare EVAL!)
(defn- exec-body [body] (try (or (nil? body) (EVAL! body)) (catch clojure.lang.ExceptionInfo e (trap e))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- end-statement!
    []
    (when-let [n (or (@<STNO> :END) (@<STNO> :end))]
        (let [counted (when-not (neg? (long @&STLIMIT)) (swap! &STCOUNT inc))]
            (reset! &LASTNO @&STNO)
            (reset! &STNO (get @<DISPNO> n n))
            (reset! &LASTFILE @&FILE)
            (reset! &LASTLINE @&LINE)
            (let [[f l] (get @<POS> n ["" 0])] (reset! &FILE f) (reset! &LINE l) (reset! <STMT-LINE> l))
            (when (and counted (pos? (long @&TRACE))) (fire-keyword! "STCOUNT" counted)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare INVOKE)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- run*
    [at err0 k]
    (letfn
        [(skey [address] (let [[no label] address] (if label label no)))
            (saddr
                [at]
                (cond
                    (keyword? at) (when-let [n (@<STNO> at)] [n at])
                    (string? at)
                        (or (when-let [n (@<STNO> (keyword at))] [n (keyword at)]) (when (re-matches #"[0-9]+" at) (let [n (Long/parseLong at)] [n (@<LABL> n)])) (when (@<CODE> at) [(@<STNO> at) at]))
                    (integer? at) [at (@<LABL> at)]))
            (computed-target
                [tgt]
                (cond
                    (and (list? tgt) (clojure.core/= '$ (first tgt)) (clojure.core/= 2 (count tgt))) (when-some [v (EVAL! (second tgt))] (if (integer? v) v (keyword (str v))))
                    (and (list? tgt) (clojure.core/= 'DIRECT (first tgt)))
                        (when-some [v (EVAL! (second tgt))] (if (instance? CodeBlock v) (.entry ^CodeBlock v) (throw (ex-info "goto operand in direct goto is not code" {:snobol/error 24}))))
                    (and (list? tgt) (symbol? (first tgt)) (not (:subscript (meta tgt))))
                        (let [args (mapv EVAL! (rest tgt)) _ (when (some nil? args) (throw (ex-info "goto evaluation failure" {:snobol/error 20}))) r (apply INVOKE (first tgt) args)]
                            (cond
                                (instance? NameResult r) (let [t (:target r)] (if (symbol? t) (keyword (name t)) t))
                                (nil? r) (throw (ex-info "goto evaluation failure" {:snobol/error 20}))
                                :else (throw (ex-info "function called by name returned a value" {:snobol/error 21}))))
                    :else tgt))
            (goto! [tgt] (if (special-targets tgt) (dispatch-special! tgt) tgt))
            (label-addr [tgt] (or (saddr tgt) (throw (ex-info "goto undefined label" {:snobol/error 38}))))
            (step
                [r goto err current]
                (let [g (when (map? goto) goto) tgt (computed-target (if r (or (:G g) (:S g)) (or (:G g) (:F g)))) nm (label-name tgt)]
                    (cond
                        (and (clojure.core/= nm "CONTINUE") (:resume err) (clojure.core/= (:resume err) @<ERRSERIAL>)) [::resume nil]
                        (clojure.core/= nm "CONTINUE")
                            (if
                                (and err (clojure.core/= (:snobol/serial (ex-data (:ex err))) @<ERRSERIAL>))
                                (let [g2 (:goto err) f2 (when (map? g2) (or (:G g2) (:F g2)))]
                                    (if
                                        (and (:in-goto err) f2 (not (keyword? f2)))
                                        (throw (let [e (fatal-error 332)] (ex-info (ex-message e) (assoc (ex-data e) ::continued err))))
                                        (let [etgt (computed-target f2)] (if etgt (do (goto! etgt) [(label-addr etgt) nil]) [(saddr (inc ((:at err) 0))) nil]))))
                                (throw (fatal-error 37)))
                        (and err (clojure.core/= nm "ABORT"))
                            (let [e ^clojure.lang.ExceptionInfo (:ex err)]
                                (throw (ex-info (ex-message e) (-> (ex-data e) (dissoc :snobol/label :snobol/serial :snobol/fp :snobol/resume) (assoc :snobol/abort true)))))
                        (clojure.core/= nm "ABORT")
                            (let [n (long @&ERRTYPE)] (throw (if (zero? n) (ex-info (error-message 36) {:snobol/error 36}) (ex-info (error-message n) {:snobol/error n :snobol/abort true}))))
                        (clojure.core/= nm "SCONTINUE")
                            (throw
                                (fatal-error
                                    (if (and err (or (and (:resume err) (clojure.core/= (:resume err) @<ERRSERIAL>)) (clojure.core/= (:snobol/serial (ex-data (:ex err))) @<ERRSERIAL>))) 331 321)))
                        tgt (do (goto! tgt) (let [a (label-addr tgt)] (when (pos? (long @&TRACE)) (fire-label! (or (second a) tgt))) [a err]))
                        :else [(saddr (inc (current 0))) err])))
            (follow
                [r goto err current]
                (loop [r r]
                    (let [res (try (step r goto err current) (catch clojure.lang.ExceptionInfo e (let [t (trap e)] (if t t ::fail))))]
                        (cond
                            (instance? Transfer res) [(:at res) (:err res)]
                            (instance? Trapped res)
                                (enter (:label res) (if-let [e0 (::continued (ex-data (:ex res)))] (assoc e0 :ex (:ex res)) {:at current :goto goto :ex (:ex res) :in-goto true}) current)
                            (clojure.core/= ::fail res) (recur false)
                            :else res))))
            (enter [label err current] (if (system-labels (name label)) (follow true {:G label} err current) [(saddr label) err]))]
        (when (and (nil? k) (zero? (long @&FNCLEVEL))) (reset! &STCOUNT 0) (reset! <ST-DEAD> nil))
        (try
            (loop [[current err] (if k (enter at err0 nil) [(saddr at) err0])]
                (cond
                    (identical? ::resume current) ::resume
                    (and k (or (nil? current) (not (clojure.core/= k @<ERRSERIAL>)))) (throw (ex-info "transfer" {:snobol/signal :transfer :snobol/at current :snobol/err err}))
                    :else
                        (when-let [key (skey current)]
                            (when (special-targets key) (dispatch-special! key))
                            (when-let [stmt (@<CODE> key)]
                                (reset! &LASTNO @&STNO)
                                (reset! &LASTFILE @&FILE)
                                (reset! &LASTLINE @&LINE)
                                (when-some [[f l] (get @<POS> (first current))] (reset! &FILE f) (reset! &LINE l) (reset! <STMT-LINE> l))
                                (reset! &STNO (get @<DISPNO> (first current) (first current)))
                                (let [over
                                          (let [n (if (neg? (long @&STLIMIT)) @&STCOUNT (swap! &STCOUNT inc))]
                                              (let [
                                                    step (if (or (stcount-traced?) (not (zero? (long @&PROFILE)))) 1 0)
                                                    over (when (stlimit-over? n step) (overrun n))
                                                    _ (when (and (not over) (pos? (long @&TRACE)) (not (neg? (long @&STLIMIT)))) (fire-keyword! "STCOUNT" n))]
                                                  (when (pos? @&TRACE) (when (statement-trace-active?) (binding [*out* *err*] (println (format ">>> stmt %5d  %s" (dec n) (str key))))))
                                                  over))]
                                    (when (identical? stmt block-end) (throw (ex-info "END" {:snobol/signal :end :snobol/closing true})))
                                    (let [ferst (first stmt) seqond (second stmt) body (if (map? ferst) seqond ferst) goto (if (map? ferst) ferst seqond)]
                                        (let [r (cond over (trap over) (identical? stmt compile-error) (trap (ex-info (error-message 7) {:snobol/error 7})) :else (exec-body body))]
                                            (cond
                                                (instance? Transfer r) (recur [(:at r) (:err r)])
                                                (instance? Trapped r) (recur (enter (:label r) {:at current :goto goto :ex (:ex r)} current))
                                                :else (recur (follow r goto err current))))))))))
            (catch clojure.lang.ExceptionInfo e
                (if k
                    (throw e)
                    (case
                        (get (ex-data e) :snobol/signal)
                        :end (if (pos? (long @&FNCLEVEL)) (throw e) (do (when-some [l (:fnclevel (ex-data e))] (reset! &FNCLEVEL l)) (when-not (:snobol/closing (ex-data e)) (end-statement!)) nil))
                        :return nil
                        :step-limit (throw e)
                        :freturn (throw e)
                        :nreturn (throw e)
                        (throw e)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn RUN-AT-ERROR [h k e] (run* h {:resume k :ex e} k))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- at-error
    [^clojure.lang.ExceptionInfo e d]
    (let [{h :snobol/label k :snobol/serial fp :snobol/fp to :snobol/resume :as data} (ex-data e)]
        (if
            (and h (clojure.core/= d fp) (< (long *handlers*) 100))
            (do
                (binding [*handlers* (inc (long *handlers*))] ((var-get (ns-resolve 'SNOBOL4clojure.core 'RUN-AT-ERROR)) h k e))
                (ex-info (ex-message e) (-> data (dissoc :snobol/label :snobol/serial :snobol/fp :snobol/resume) (assoc :snobol/to to)) e))
            e)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- failure-point
    [f]
    (let [d (inc (long *fp*))]
        (binding [*fp* d] (try (f) (catch clojure.lang.ExceptionInfo e (let [e (at-error (error-decided e d) d) to (:snobol/to (ex-data e))] (if (and to (>= (long to) d)) nil (throw e))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- eval-operands [operands] (loop [ops (seq operands) vals []] (if ops (let [v (EVAL! (first ops))] (when-not (nil? v) (recur (next ops) (conj vals v)))) vals)))
(def &MAXLNGTH (atom 16777216))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- concat-value
    [r]
    (if
        (and (list? r) (clojure.core/= (first r) 'SEQ))
        (let [
              parts (remove #(clojure.core/= ε %) (rest r))
              _
                  (when
                      (next parts)
                      (let [v (vec parts) n (count v) e8 #(throw (ex-info "concatenation left operand is not a string or pattern" {:snobol/error 8}))]
                          (cond
                              (textless? (v (clojure.core/- n 2))) (e8)
                              (textless? (v (dec n))) (throw (ex-info "concatenation right operand is not a string or pattern" {:snobol/error 9}))
                              (some textless? (subvec v 0 (clojure.core/- n 2))) (e8))))]
            (cond
                (every? #(or (string? %) (number? %) (instance? NAME %)) parts)
                    (if
                        (and (clojure.core/= 1 (count parts)) (< 1 (count (rest r))))
                        (first parts)
                        (let [t (apply str (map sstr parts))] (if (> (count t) (long @&MAXLNGTH)) (throw (ex-info "string length exceeds value of maxlngth keyword" {:snobol/error 205})) t)))
                (clojure.core/= (count parts) (count (rest r))) r
                (empty? (rest parts)) (first parts)
                :else (apply list 'SEQ parts)))
        r))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(deftype ObjKey [v] Object (equals [_ o] (and (instance? ObjKey o) (identical? v (.v ^ObjKey o)))) (hashCode [_] (System/identityHashCode v)))
(defn- table-key [k] (if (object-identity? k) (ObjKey. k) k))
(defn table-get [t k] (get @t (table-key k) (:dflt (meta t) ε)))
(defn table-subscript [ks] (if (next ks) (throw (ex-info (error-message 237) {:snobol/error 237})) (first ks)))
(defn- array-subscript [i] (if (integer? i) i (let [n (num i)] (if (and (double? n) (Double/isNaN n)) (throw (ex-info "array subscript is not integer" {:snobol/error 238})) (long n)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- array-check-bounds
    [^SnobolArray arr idx-vec]
    (let [dims (:dims arr) _ (when-not (clojure.core/= (count idx-vec) (count dims)) (throw (ex-info (error-message 236) {:snobol/error 236}))) idx-vec (mapv array-subscript idx-vec)]
        (when (clojure.core/= (count idx-vec) (count dims)) (when (every? true? (map (fn [[lo hi] i] (and (integer? i) (>= i lo) (<= i hi))) dims idx-vec)) idx-vec))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn array-get [^SnobolArray arr idx-vec] (when-let [k (array-check-bounds arr idx-vec)] (get @(:data arr) k (:dflt arr))))
(def ^:private unused-unary-ops {'! "!" '% "%" '/ "/" 'sharp-op "#" '= "=" '| "|" (symbol "^") "^"})
(defn- opsyn-key [spelling arity] (str spelling "/" arity))
(def ^:private unused-binary-ops {'& "&" 'at-op "@" 'sharp-op "#" '% "%" 'tilde-op "~"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- name-value
    [target]
    (cond
        (symbol? target) ($$ target)
        (and (list? target) (equal (first target) '&) (clojure.core/= 2 (count target))) (EVAL! target)
        :else (apply INVOKE (or (:container (meta target)) (first target)) (rest target))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- deref-result [r] (if (instance? NameResult r) (name-value (:target r)) r))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- call-form?
    [X]
    (and (list? X) (symbol? (first X)) (contains? @<FUNS> (name (first X))) (not (:data (meta (get @<FUNS> (name (first X)))))) (let [v (reference (first X))] (not (or (table? v) (array? v))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn name-string [x] (if (instance? NAME x) (str (.n ^NAME x)) (str x)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- indirect-target
    [v]
    (cond
        (and (instance? NAME v) (list? (.n ^NAME v))) (.n ^NAME v)
        (and (instance? NAME v) (instance? ExprVar (.n ^NAME v))) (.n ^NAME v)
        (instance? NAME v) (symbol (name-string v))
        (or (and (string? v) (not= "" v)) (number? v)) (symbol (sstr v))
        :else (throw (ex-info "indirection operand is not name" {:snobol/error 239}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- applied-function [fname] (let [s (gtstg fname 60)] (when (clojure.core/= "" s) (throw (ex-info (error-message 60) {:snobol/error 60}))) (symbol s)))
(defn- deferred? [x] (and (seq? x) (clojure.core/= 'DEFER! (first x)) (fn? (second x))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- evaluated-or-fail
    [f]
    (let [d0 (long *fp*)]
        (when (clojure.core/= :xt @<STAGE>) (reset! <STAGE> :ee))
        (reset! <GTCEF> d0)
        (let [v
                  (binding [*fp* (inc d0)]
                      (try
                          (f)
                          (catch clojure.lang.ExceptionInfo e (let [e (at-error (error-decided e (inc d0)) (inc d0)) to (:snobol/to (ex-data e))] (if (and to (>= (long to) d0)) nil (throw e))))))]
            (reset! <STAGE> :xt)
            v)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- returned-value! [] (throw (ex-info "expression evaluated by name returned value" {:snobol/error 249})))
(defn ends-in-mark? [^String s] (let [n (.length s)] (and (pos? n) (contains? #{\: \;} (.charAt s (dec n))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def grammar
    " stmt      ::=  label? body? goto? <__> <eos?>
  label     ::=  #'[0-9A-Za-z][^ \\t\\n]*'
  body      ::=  <_> (invoking | matching | replacing | assigning)
  goto      ::=  <__ ':' __> (jmp  |  sjmp (<__> fjmp)?  |  fjmp (<__> sjmp)?)
  invoking  ::=  subject | xand | xalt
  matching  ::=  subject <_> pattern | cat <_ '?' _> and qmatch*
  qmatch    ::=  <_ '?' _> and
  replacing ::=  subject <_> pattern <_ '='> replace
  assigning ::=  subject <_ '='> replace
 <subject>  ::=  at
 <pattern>  ::=  (<'?' _>)? and
 <replace>  ::=  (<_> expr)?
  jmp       ::=  target
  sjmp      ::=  <'S' | 's'> target
  fjmp      ::=  <'F' | 'f'> target
 <target>   ::=  <'('> expr <')'> | dgo
  dgo       ::=  <'<'> expr <'>'> | <'['> expr <']'>
  comment   ::=  <'*'> #'.*' <eol>
  control   ::=  <'-'> #'[^;\\n]*' <eos>
  eos       ::=  '\\n' | ';'
  eol       ::=  '\\n'
 <expr>     ::=  <__> asn <__>
  asn       ::=  mch | mch  <_  '='  _>  asn | mch <_> nulasn
  nulasn    ::=  <'='> !','
  mch       ::=  and | mch  <_  '?'  _>  and (<_ '=' _> and)?
  and       ::=  alt | and  <_  '&'  _>  alt
  alt       ::=  cat | cat (<_  '|'  _>  cat)+
  xalt      ::=  cat (<_  '|'  _>  cat)+
  xand      ::=  and <_  '&'  _>  alt
  cat       ::=  at  | at  (<_>          at)+
  at        ::=  sum | sum  <_  '@'  _>  at
  sum       ::=  hsh | sum  <_> '+' <_>  hsh
                     | sum  <_> '-' <_>  hsh
  hsh       ::=  div | hsh  <_  '#'  _>  div
  div       ::=  mul | div  <_  '/'  _>  mul
  mul       ::=  pct | mul  <_  '*'  _>  pct
  pct       ::=  xp  | pct  <_  '%'  _>  xp
  xp        ::=  cap | cap  <_> '^' <_>  xp
                     | cap  <_> '!' <_>  xp
                     | cap  <_> '**' <_> xp
  cap       ::=  ttl | cap  <_> '$' <_>  ttl
                     | cap  <_> '.' <_>  ttl
  ttl       ::=  uop | uop  <_  '~'  _>  ttl
  uop       ::=  ndx | '@' uop | '~' uop | '?' uop | '&' uop | '+' uop
                     | '-' uop | '*' uop | '$' uop | '.' uop | '!' uop
                     | '%' uop | '/' uop | '#' uop | '=' uop | '|' uop | '^' uop
  ndx       ::=  itm | ndx <'<'> (lst | nul) <'>'> | ndx <'['> (lst | nul) <']'>
  <itm>     ::=  I | R | S | N | grp | cnd | inv
  grp       ::=  <'('> expr <')'>
  cnd       ::=  <'('> expr <','> lst <')'>
  inv       ::=  N <'()'>  |  N <'('> lst <')'>
  <lst>     ::=  expr | arg (<','> arg)+
  <arg>     ::=  expr | nul
  nul       ::=  <__>
  <_>       ::=  <white+>
  <__>      ::=  <white*>
  I         ::=  #'[0-9]+'
  R         ::=  #'[0-9]+(\\.[0-9]*([eEdD][+-]?[0-9]+)?|[eEdD][+-]?[0-9]+)'
  S         ::=  #'\"([^\"]|\\x3B)*\"'  |  #'\\'([^\\']|\\x3B)*\\''
  N         ::=  #'[A-Za-z][A-Z_a-z0-9\\.]*'
  white     ::=  #'[ \\t]'
")
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defmacro ^:private compiled-rules [] (list 'quote (:grammar (insta/parser grammar))))
(def ^:private rules (compiled-rules))
(def parse-expression (insta/parser rules :start :expr))
(defn- compiled! [ok?] (reset! <XC> (and (not ok?) (pos? (long @&FNCLEVEL)))))
(defn- blank? [c] (or (clojure.core/= c \space) (clojure.core/= c \tab)))
(defn- char-at [^String s i] (when (< i (count s)) (.charAt s i)))
(defn- skip-blanks [s i] (if (blank? (char-at s i)) (recur s (inc i)) i))
(defn- fail! [n i] (throw (ex-info "syntax" {::error n ::at i})))
(defn- stray! [c i] (fail! (case c \) 224 \, 223 225) i))
(def ^:private operator-char? #{\+ \- \* \/ \! \^ \$ \. \& \@ \~ \? \% \# \| \=})
(defn- letter? [c] (and c (or (<= (int \A) (int c) (int \Z)) (<= (int \a) (int c) (int \z)))))
(declare parse-operand)
(defn- operand-after-unary [s i] (let [c (char-at s i)] (if (and (operator-char? c) (blank? (char-at s (inc i)))) (fail! 233 i) (parse-operand s i))))
(defn- digit? [c] (and c (<= (int \0) (int c) (int \9))))
(defn- name-char? [c] (or (letter? c) (digit? c) (clojure.core/= c \_) (clojure.core/= c \.)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- number-end
    [s i]
    (let [
          digits (fn [j] (if (digit? (char-at s j)) (recur (inc j)) j))
          j (digits i)
          _ (when (and (not (#{\. \e \E \d \D} (char-at s j))) (> (clojure.core/- j i) 18) (try (Long/parseLong (subs s i j)) false (catch NumberFormatException _ true))) (fail! 231 i))
          j (if (clojure.core/= \. (char-at s j)) (digits (inc j)) j)
          j (if (#{\e \E \d \D} (char-at s j)) (let [k (if (#{\+ \-} (char-at s (inc j))) (clojure.core/+ j 2) (inc j))] (if (digit? (char-at s k)) (digits k) (fail! 231 i))) j)]
        (if (or (name-char? (char-at s j)) (#{\- \+} (char-at s j))) (fail! 231 i) j)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- last-token-start
    [^String s i]
    (let [
          j (loop [j (dec i)] (if (and (>= j 0) (blank? (char-at s j))) (recur (dec j)) j))
          c (char-at s j)
          back (fn [k] (loop [k k] (if (and (pos? k) (name-char? (char-at s (dec k)))) (recur (dec k)) k)))]
        (cond
            (neg? j) i
            (#{\' \"} c) (let [k (.lastIndexOf s (str c) (int (dec j)))] (if (neg? k) j k))
            (and (#{\( \[ \<} c) (pos? j) (name-char? (char-at s (dec j)))) (back (dec j))
            (name-char? c) (back j)
            :else j)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare expression)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- element-list
    [s i closer]
    (loop [i i]
        (let [i (expression s i {:closer closer}) c (char-at s i)]
            (cond
                (clojure.core/= c \,) (recur (inc i))
                (clojure.core/= c closer) (inc i)
                (nil? c) (fail! (if (clojure.core/= closer \)) 226 229) (last-token-start s i))
                (#{\: \;} c) (fail! (if (clojure.core/= closer \)) 226 229) i)
                (clojure.core/= c \)) (fail! 224 i)
                (#{\] \>} c) (fail! 225 i)
                :else (fail! 220 i)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- parse-operand
    [s i]
    (let [c (char-at s i)]
        (cond
            (nil? c) (fail! 221 i)
            (and (operator-char? c) (let [d (char-at s (inc i))] (or (nil? d) (blank? d) (#{\) \] \> \, \: \;} d)))) (fail! 221 (if (blank? (char-at s (inc i))) i (inc i)))
            (#{\. \&} c)
                (let [d (char-at s (inc i))]
                    (cond
                        (or (letter? d) (and (clojure.core/= c \.) (#{\$ \( \*} d))) [(first (parse-operand s (inc i))) :value]
                        (and (operator-char? d) (blank? (char-at s (clojure.core/+ i 2)))) (fail! 233 (inc i))
                        :else (fail! 212 (inc i))))
            (operator-char? c) [(first (operand-after-unary s (inc i))) :value]
            :else
                (let [[j kind]
                          (cond
                              (#{\' \"} c) (let [k (.indexOf ^String s (str c) (int (inc i)))] (if (neg? k) (fail! 232 i) [(inc k) :literal]))
                              (digit? c) [(number-end s i) :literal]
                              (letter? c)
                                  (let [j (loop [j (inc i)] (if (name-char? (char-at s j)) (recur (inc j)) j))] (if (clojure.core/= \( (char-at s j)) [(element-list s (inc j) \)) :value] [j :name]))
                              (clojure.core/= c \() [(element-list s (inc i) \)) :value]
                              (#{\< \[} c) (fail! 222 i)
                              (clojure.core/= c \)) (fail! 224 i)
                              (#{\] \>} c) (fail! 225 i)
                              (clojure.core/= c \,) (fail! 223 i)
                              (#{\: \;} c) (fail! 221 i)
                              :else (fail! 230 i))]
                    (loop [j j kind kind] (let [k (skip-blanks s j)] (case (char-at s k) \< (recur (element-list s (inc k) \>) :name) \[ (recur (element-list s (inc k) \]) :name) [j kind])))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- unclosed-quote? [^String s j] (let [q (char-at s j)] (and (#{\' \"} q) (neg? (.indexOf s (str q) (int (inc (long j))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- sign-mark
    [s j]
    (let [nc? (fn [k] (and (>= k 0) (let [c (char-at s k)] (or (letter? c) (digit? c) (#{\. \_} c)))))]
        (if (nc? (dec j)) (loop [k (loop [k (dec j)] (if (nc? (dec k)) (recur (dec k)) k))] (if (and (< k j) (not (letter? (char-at s k)))) (recur (inc k)) k)) j)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- expression
    [s i ctx]
    (let [stop? (fn [c] (or (nil? c) (#{\: \;} c) (and (not (:top ctx)) (#{\) \] \> \,} c)))) i (skip-blanks s i) c (char-at s i)]
        (cond
            (and (:top ctx) (#{\) \] \> \,} c)) (stray! c i)
            (stop? c) i
            :else
                (loop [[j kind] (parse-operand s i) n 1 q 0 pre 0 st i]
                    (let [c (char-at s j)]
                        (cond
                            (and (#{\) \] \> \,} c) (:top ctx)) (stray! c j)
                            (stop? c) j
                            (or (letter? c) (digit? c) (#{\' \" \(} c)) (fail! (if (unclosed-quote? s j) 232 220) j)
                            (operator-char? c) (fail! 233 (if (#{\+ \-} c) (sign-mark s j) j))
                            (not (blank? c)) (fail! 230 j)
                            :else
                                (let [k (skip-blanks s j) d (char-at s k) k (if (and (clojure.core/= d \*) (clojure.core/= \* (char-at s (inc k)))) (inc k) k) e (char-at s (inc k))]
                                    (cond
                                        (and (#{\) \] \> \,} d) (:top ctx)) (stray! d k)
                                        (stop? d) k
                                        (and (operator-char? d) (or (nil? e) (blank? e)))
                                            (if
                                                (clojure.core/= d \=)
                                                (cond
                                                    (and (clojure.core/= kind :literal) (clojure.core/= n 1)) (fail! 212 k)
                                                    (and (:top ctx) (pos? q) (or (:stop-at-equals ctx) (> q 1) (> pre 1))) (fail! 212 :none)
                                                    (:stop-at-equals ctx) k
                                                    :else (let [m (skip-blanks s (inc k))] (if (char-at s m) (expression s m (dissoc ctx :stop-at-equals)) m)))
                                                (let [m (skip-blanks s (inc k))]
                                                    (cond
                                                        (or (nil? (char-at s m)) (#{\) \] \> \, \: \;} (char-at s m))) (fail! 221 m)
                                                        (:goto ctx) (fail! 212 m)
                                                        :else (recur (parse-operand s m) (inc n) (if (clojure.core/= d \?) (inc q) q) (if (and (clojure.core/= d \?) (zero? q)) n pre) m))))
                                        (:goto ctx) (fail! 212 k)
                                        :else (recur (parse-operand s k) (inc n) q pre k)))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn expression-end [^String s] (try {:end (expression s 0 {:top true})} (catch clojure.lang.ExceptionInfo e (when-let [n (::error (ex-data e))] {:error n}))))
(def ^:private ^:dynamic *raise-compile-errors* false)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- compile-failed!
    [n text]
    (compiled! false)
    (when *raise-compile-errors* (throw (ex-info text {:snobol/error n})))
    (reset! &ERRTYPE n)
    (fire-keyword! "ERRTYPE" n)
    (reset! &ERRTEXT text)
    (when (pos? (long @&ERRLIMIT)) (swap! &ERRLIMIT dec))
    nil)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn top-replacement
    [r]
    (if
        (and (seq? r) (clojure.core/= '= (first r)) (clojure.core/= 3 (count r)) (not (:paren (meta r))))
        (let [[_ t v] r v (top-replacement v)] (if (and (vector? t) (not (:paren (meta t)))) (list '?= (first t) (if (clojure.core/= 2 (count t)) (second t) (vec (rest t))) v) (list '= t v)))
        r))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn emitter
    [ast]
    (insta/transform
        {
         :comment (fn [cmt] [:comment cmt])
         :control (fn [ctl] [:control ctl])
         :stmt (fn [& ss] (apply conj ss))
         :label (fn [L] {:label (keyword L)})
         :body (fn [B] {:body B})
         :invoking (fn [S] S)
         :matching (fn [S P & qs] (reduce (fn [m q] (list '? m q)) (list '? S P) qs))
         :qmatch (fn [P] P)
         :replacing (fn ([S P] (list '?= S P 'epsilon)) ([S P R] (list '?= S P (top-replacement R))))
         :assigning (fn ([S] (list '= S 'epsilon)) ([S R] (list '= S (top-replacement R))))
         :grp (fn [x] (if (instance? clojure.lang.IObj x) (vary-meta x assoc :paren true) x))
         :goto (fn [& gs] {:goto (reduce (fn [bs b] (let [key (first b) tgt (second b)] (assoc bs key (if (symbol? tgt) (keyword tgt) tgt)))) {} gs)})
         :jmp (fn [L] [:G L])
         :sjmp (fn [L] [:S L])
         :fjmp (fn [L] [:F L])
         :expr (fn [x] x)
         :nulasn (fn [] "")
         :asn (fn ([x] x) ([x y] (if (and (seq? x) (clojure.core/= '? (first x)) (clojure.core/= 3 (count x))) (list '?= (second x) (nth x 2) y) (list '= x y))))
         :mch (fn ([x] x) ([x y] (list '? x y)) ([x y z] (list '?= x y z)))
         :and (fn ([x] x) ([x y] (list '& x y)))
         :alt (fn ([x] x) ([x & ys] (apply list '| x ys)))
         :xalt (fn [x & ys] (apply list '| x ys))
         :xand (fn [x y] (list '& x y))
         :cat (fn ([x] x) ([x & ys] (apply vector x ys)))
         :at (fn ([x] x) ([x y] (list 'at-op x y)))
         :sum (fn ([x] x) ([x op y] (list (symbol op) x y)))
         :hsh (fn ([x] x) ([x y] (list 'sharp-op x y)))
         :div (fn ([x] x) ([x y] (list '/ x y)))
         :mul (fn ([x] x) ([x y] (list '* x y)))
         :pct (fn ([x] x) ([x y] (list '% x y)))
         :xp (fn ([x] x) ([x op y] (list (symbol (if (#{"^" "!"} op) "**" op)) x y)))
         :cap (fn ([x] x) ([x op y] (list (symbol op) x y)))
         :ttl (fn ([x] x) ([x y] (list 'tilde-op x y)))
         :uop (fn ([x] x) ([op y] (case op "@" (list 'at-op y) "#" (list 'sharp-op y) "~" (list 'tilde-op y) (list (symbol op) y))))
         :ndx (fn ([n] n) ([n & xs] (with-meta (apply list n xs) {:subscript true})))
         :cnd (fn ([x] x) ([x & ys] (apply vector :comma x ys)))
         :inv (fn [f & xs] (apply list f xs))
         :nul (fn [] "")
         :dgo (fn [x] (list 'DIRECT x))
         :N (fn [n] (symbol n))
         :S (fn [s] (subs s 1 (subtract (count s) 1)))
         :I (fn [s] (let [v (edn/read-string s)] (if (instance? Long v) v (throw (ex-info "syntax error: invalid numeric item" {:snobol/error 231 :snobol/compile true})))))
         :R (fn [s] (or (parse-real s) (throw (ex-info "syntax error: invalid numeric item" {:snobol/error 231 :snobol/compile true}))))}
        ast))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- pattern-variable [sym] (some-> (requiring-resolve (symbol "SNOBOL4clojure.core" (name sym))) var-get))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- name-positions
    [op args]
    (let [n (count args)]
        (cond
            (and (clojure.core/= 1 n) ('#{. at-op &} op)) #{0}
            (and (clojure.core/= 2 n) ('#{. $} op)) #{1}
            (and (clojure.core/= 2 n) (clojure.core/= '= op)) #{0}
            (and (clojure.core/= 3 n) (clojure.core/= '?= op)) #{0}
            :else #{})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private unary-names '#{$ * & ! % / sharp-op = |})
(def ^:private binary-names '#{sharp-op % & tilde-op at-op})
(def ^:private operators '#{+ - * / ** sharp-op % at-op & tilde-op | ? ?= = . $ !})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- name-form?
    [x]
    (cond
        (or (symbol? x) (keyword? x)) true
        (list? x)
            (let [[op & args] x n (count args)]
                (cond
                    (:subscript (meta x)) true
                    (not (symbol? op)) true
                    (and (clojure.core/= 1 n) (unary-names op)) true
                    (and (clojure.core/= 2 n) (binary-names op)) true
                    (operators op) false
                    :else true))
        :else false))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- name-required! [x] (when-not (name-form? x) (throw (ex-info "syntax error: value used where name is required" {:snobol/error 212}))))
(def ^:private constant-keywords '#{ABORT ALPHABET ARB BAL FAIL FENCE LCASE REM SUCCEED UCASE})
(declare fold)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- constant-name?
    [x]
    (or
        (symbol? x)
        (and (list? x) (clojure.core/= 2 (count x)) (or (clojure.core/= '& (first x)) (clojure.core/= '* (first x))))
        (and (list? x) (clojure.core/= 2 (count x)) (clojure.core/= '$ (first x)) (first (fold (second x))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private folded-operators '#{+ - * / **})
(def ^:private folded-functions '#{SIZE LEN ANY NOTANY SPAN BREAK BREAKX POS RPOS TAB RTAB ARBNO FENCE REMDR CHAR REPLACE LPAD RPAD REVERSE DATATYPE SQRT EXP LN SIN COS TAN ATAN CHOP})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- fold
    [n]
    (let [eval! (requiring-resolve 'SNOBOL4clojure.core/EVAL!)]
        (cond
            (or (string? n) (number? n)) [true n]
            (and (symbol? n) (protected-variables n)) [true (pattern-variable n)]
            (vector? n) (let [comma? (clojure.core/= :comma (first n)) parts (mapv fold (if comma? (rest n) n))] (if (and (not comma?) (every? first parts)) [true (eval! n)] [false nil]))
            (list? n)
                (let [[op & args] n names (name-positions op args)]
                    (cond
                        (and (clojure.core/= '& op) (clojure.core/= 1 (count args)))
                            (do (when-not (symbol? (first args)) (name-required! (first args))) (if (constant-keywords (first args)) [true (eval! n)] [false nil]))
                        (and (clojure.core/= '* op) (clojure.core/= 1 (count args))) (do (fold (first args)) [true (eval! n)])
                        (and
                            ('#{. at-op} op)
                            (clojure.core/= 1 (count args))
                            (do (name-required! (first args)) true)
                            (constant-name? (first args))
                            (or (clojure.core/= '. op) (symbol? (first args)) (clojure.core/= '$ (first (first args)))))
                            (do (fold (first args)) [true (eval! n)])
                        (and ('#{. $} op) (clojure.core/= 2 (count args)))
                            (let [p (fold (first args))] (name-required! (second args)) (let [t (fold (second args))] (if (and (first p) (constant-name? (second args))) [true (eval! n)] [false nil])))
                        :else
                            (let [parts (vec (map-indexed (fn [i a] (when (names i) (name-required! a)) (fold a)) args))]
                                (if
                                    (and
                                        (every? first parts)
                                        (or
                                            (and (folded-operators op) (<= 1 (count args) 2) (not (and (clojure.core/= '/ op) (clojure.core/= 1 (count args)))))
                                            (and (clojure.core/= '| op) (<= 2 (count args)))
                                            (and (folded-functions op) (not (:subscript (meta n))))))
                                    (let [v (eval! n)] (if (nil? v) [false nil] [true v]))
                                    [false nil]))))
            :else [false nil])))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fold-error
    ([body] (fold-error body nil))
    ([body goto]
        (try
            (fold body)
            (doseq [k [:S :F :G] :let [t (get goto k)] :when (and (some? t) (not (keyword? t)) (not (and (list? t) (clojure.core/= 'DIRECT (first t)))))] (name-required! t) (fold t))
            nil
            (catch clojure.lang.ExceptionInfo e (:snobol/error (ex-data e))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn freeze
    [n]
    (letfn
        [(mark
             [n]
             (cond
                 (and (symbol? n) (protected-variables n)) (list 'pattern-constant n)
                 (vector? n) (with-meta (mapv mark n) (meta n))
                 (list? n) (let [[op & args] n names (name-positions op args)] (with-meta (apply list op (map-indexed (fn [i a] (if (names i) a (mark a))) args)) (meta n)))
                 :else n))
            (walk
                [n]
                (cond
                    (and (list? n) (clojure.core/= '* (first n)) (clojure.core/= 2 (count n))) (with-meta (list '* (walk (second n))) (meta n))
                    (and (or (list? n) (vector? n)) (first (try (fold n) (catch clojure.lang.ExceptionInfo _ [false nil])))) (mark n)
                    (vector? n) (with-meta (mapv walk n) (meta n))
                    (list? n) (with-meta (apply list (first n) (map walk (rest n))) (meta n))
                    :else n))]
        (if (some #(and (symbol? %) (protected-variables %)) (tree-seq coll? seq n)) (walk n) n)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- compile-expression
    ([s] (compile-expression s nil))
    ([^String s early]
        (let [p (parse-expression s)]
            (cond
                (re-matches #"[ \t]*" s) (do (compiled! true) "")
                (instance? instaparse.gll.Failure p) (let [{n :error i :end} (expression-end s) n (cond n n (< (long i) (count s)) early :else 220)] (when n (compile-failed! n (error-message n))))
                :else
                    (let [ir (top-replacement (first (emitter p))) n ((requiring-resolve 'SNOBOL4clojure.core/fold-error) ir)]
                        (if n (compile-failed! n (error-message n)) (do (compiled! true) ((requiring-resolve 'SNOBOL4clojure.core/freeze) ir))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- compile-or-fail
    [^String s]
    (try (compile-expression s 103) (catch clojure.lang.ExceptionInfo e (let [{n :snobol/error c :snobol/compile} (ex-data e)] (when-not c (throw e)) (compile-failed! n (ex-message e))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare name-of)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- call-by-name
    [fsym args]
    (let [fname (name fsym) f (get @<FUNS> fname)]
        (cond
            (and f (:field (meta f))) (apply list fsym args)
            (and (nil? f) (clojure.core/= fname "ITEM") (seq args)) (let [c (first args)] (with-meta (apply list c (rest args)) {:subscript true :container c}))
            (and (nil? f) (clojure.core/= fname "APPLY") (seq args)) (call-by-name (applied-function (first args)) (rest args))
            (and (nil? f) (clojure.core/= fname "EVAL"))
                (let [x (first args)]
                    (cond
                        (deferred? x) (if (symbol? (:defer-var (meta x))) (:defer-var (meta x)) (when-not (nil? (evaluated-or-fail (second x))) (returned-value!)))
                        (string? x)
                            (do
                                (when (ends-in-mark? x) (throw (ex-info "eval argument is not expression" {:snobol/error 103})))
                                (let [ir (compile-or-fail x)] (when-not (nil? ir) (evaluated-or-fail #(name-of ir)))))
                        :else (returned-value!)))
            :else (let [r (apply INVOKE fsym args)] (cond (nil? r) nil (instance? NameResult r) (:target r) :else (throw (ex-info "function called by name returned a value" {:snobol/error 21})))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- assign-to [target v] (INVOKE '= target v))
(def ^:private value-operators '#{= ?= ? | at-op + - sharp-op / * % ** ^! . tilde-op & SEQ})
(defn- element-target [N] (when-some [c (if (seq? (first N)) (EVAL! (first N)) (first N))] (when-some [ks (eval-operands (rest N))] (with-meta (apply list c ks) (meta N)))))
(defn array-in-bounds? [arr idx-vec] (some? (array-check-bounds arr idx-vec)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- capture-target
    [N]
    (cond
        (symbol? N) (fn [x] (assign-to N x))
        (and (list? N) (equal (first N) '$) (clojure.core/= 2 (count N))) (when-some [v (EVAL! (second N))] (let [t (indirect-target v)] (fn [x] (assign-to t x))))
        (and (list? N) (equal (first N) '&) (clojure.core/= 2 (count N))) (fn [x] (assign-to N x))
        (and (list? N) (equal (first N) '*) (clojure.core/= 2 (count N))) (let [X (second N)] (fn [x] (when-some [t (name-of X)] (assign-to t x))))
        (and (list? N) (symbol? (first N)) (not (:subscript (meta N))) (not (contains? value-operators (first N))) (let [v (reference (first N))] (not (or (table? v) (array? v)))))
            (when-some [args (eval-operands (rest N))] (when-some [t (call-by-name (first N) args)] (fn [x] (assign-to t x))))
        (list? N) (let [t (element-target N) c (when (symbol? (first t)) ($$ (first t)))] (when-not (and (array? c) (not (array-in-bounds? c (vec (rest t))))) (fn [x] (assign-to t x))))
        true N))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- name-of-call [X] (when-some [args (eval-operands (rest X))] (let [r (apply INVOKE (first X) args)] (when (instance? NameResult r) r))))
(defn protected-store! [sym val] (when (protected-variables (symbol (name sym))) (throw (ex-info (error-message 42) {:snobol/error 42}))) (snobol-set! sym val))
(def ABORT (list 'ABORT!))
(def ARB (list 'ARB!))
(def BAL (list 'BAL!))
(def FAIL (list 'FAIL!))
(def REM (list 'REM!))
(def SUCCEED (list 'SUCCEED!))
(def ^:private pattern-keywords {"ABORT" (fn [] ABORT) "ARB" (fn [] ARB) "BAL" (fn [] BAL) "FAIL" (fn [] FAIL) "FENCE" (fn [] bare-fence) "REM" (fn [] REM) "SUCCEED" (fn [] SUCCEED)})
(def ^:private op-spelling {'at-op "@" 'sharp-op "#" 'tilde-op "~"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn EVAL!
    [E]
    (when E
        (cond
            (nil? E) E
            (char? E) E
            (float? E) E
            (string? E) E
            (integer? E) E
            (symbol? E) (let [v ($$ E)] (when (pos? (long @&TRACE)) (fire-access! E v)) (if (identical? v FENCE) bare-fence v))
            (instance? ExprVar E) (EVAL! (.ir ^ExprVar E))
            (and (vector? E) (clojure.core/= :comma (first E))) (loop [[a & more] (rest E)] (if more (if-some [v (failure-point #(EVAL! a))] v (recur more)) (EVAL! a)))
            (vector? E) (let [evaled (eval-operands E)] (when evaled (concat-value (apply list 'SEQ evaled))))
            (list? E)
                (let [[op & parms] E]
                    (cond
                        (equal op 'pattern-constant) (let [v (primitive-pattern (first parms))] (if (identical? v FENCE) bare-fence v))
                        (:subscript (meta E))
                            (let [c (if (symbol? op) ($$ op) (EVAL! op)) c (if (instance? NAME c) ($$ (symbol (str (.n ^NAME c)))) c)]
                                (when-some [ks (eval-operands parms)]
                                    (cond
                                        (table? c) (table-get c (table-subscript ks))
                                        (array? c) (array-get c (vec ks))
                                        :else (throw (ex-info "subscripted operand is not table or array" {:snobol/error 235})))))
                        (or
                            (and (clojure.core/= 1 (count parms)) (contains? unused-unary-ops op) (nil? (get @<FUNS> (opsyn-key (unused-unary-ops op) 1))))
                            (and (clojure.core/= 2 (count parms)) (contains? unused-binary-ops op) (nil? (get @<FUNS> (opsyn-key (unused-binary-ops op) 2)))))
                            (throw (ex-info "undefined operator referenced" {:snobol/error 29}))
                        (and (equal op '?) (clojure.core/= 1 (count parms)) (nil? (get @<FUNS> (opsyn-key "?" 1)))) (when-not (nil? (EVAL! (first parms))) ε)
                        (and
                            (or (and (clojure.core/= 2 (count parms)) (or (equal op '.) (equal op '$))) (and (clojure.core/= 1 (count parms)) (or (equal op '*) (equal op '?))))
                            (get @<FUNS> (opsyn-key (str op) (count parms))))
                            (when-some [args (eval-operands parms)] (deref-result (apply (get @<FUNS> (opsyn-key (str op) (count parms))) args)))
                        (equal op '.)
                            (let [n (count parms)]
                                (if
                                    (clojure.core/= n 1)
                                    (let [x (first parms)]
                                        (cond
                                            (call-form? x)
                                                (when-some [args (eval-operands (rest x))]
                                                    (let [r (apply INVOKE (first x) args)]
                                                        (cond
                                                            (instance? NameResult r) (NAME. (:target r))
                                                            (nil? r) nil
                                                            :else (throw (ex-info "function called by name returned a value" {:snobol/error 21})))))
                                            (and (list? x) (equal (first x) '&) (clojure.core/= 2 (count x))) (NAME. x)
                                            (and (list? x) (equal (first x) '$) (clojure.core/= 2 (count x))) (when-some [v (EVAL! (second x))] (NAME. (indirect-target v)))
                                            (and (list? x) (equal (first x) '*) (clojure.core/= 2 (count x))) (let [E (second x)] (NAME. (if (symbol? E) E (->ExprVar E))))
                                            (and (list? x) (>= (count x) 2))
                                                (when-some [ks (eval-operands (rest x))]
                                                    (let [v (when (symbol? (first x)) (reference (first x)))]
                                                        (if
                                                            (or (table? v) (array? v) (:subscript (meta x)) (not (symbol? (first x))))
                                                            (NAME. (cond-> (apply list (first x) ks) (or (table? v) (array? v)) (with-meta {:container v})))
                                                            (when-some [t (call-by-name (first x) ks)] (NAME. t)))))
                                            :else (NAME. x)))
                                    (let [[P N] parms] (when-some [p (EVAL! P)] (when-some [t (capture-target N)] (INVOKE '. p t))))))
                        (and (equal op 'at-op) (clojure.core/= 1 (count parms))) (when-some [t (capture-target (first parms))] (list 'CURSOR-IMM! t))
                        (equal op '$)
                            (if
                                (clojure.core/= 1 (count parms))
                                (when-some [v (EVAL! (first parms))] (EVAL! (indirect-target v)))
                                (let [[P N] parms] (when-some [p (EVAL! P)] (when-some [t (capture-target N)] (INVOKE '$ p t)))))
                        (and (equal op '=) (list? (first parms)) (clojure.core/= 2 (count (first parms))) (equal (first (first parms)) '$))
                            (let [[[_ X] R] parms] (when-some [v (EVAL! X)] (INVOKE '= (indirect-target v) (EVAL! R))))
                        (and (equal op '=) (list? (first parms)) (equal (first (first parms)) 'ITEM) (<= 3 (count (first parms))))
                            (let [[_ container-expr & key-exprs] (first parms)]
                                (when-some [c (EVAL! container-expr)] (when-some [ks (eval-operands key-exprs)] (when-some [v (EVAL! (second parms))] (INVOKE '= (apply list c ks) v)))))
                        (and (equal op '=) (list? (first parms)) (equal (first (first parms)) 'APPLY) (<= 2 (count (first parms))))
                            (when-some [[f & as] (eval-operands (rest (first parms)))]
                                (let [fsym (symbol (name-string f)) df (get @<FUNS> (name fsym))]
                                    (if
                                        (and df (not (:data (meta df))))
                                        (let [r (apply INVOKE fsym as)] (when (instance? NameResult r) (when-some [v (EVAL! (second parms))] (INVOKE '= (:target r) v))))
                                        (when-some [v (EVAL! (second parms))] (INVOKE '= (apply list fsym as) v)))))
                        (and
                            (equal op '=)
                            (list? (first parms))
                            (or
                                (and (clojure.core/= 2 (count (first parms))) (contains? unused-unary-ops (first (first parms))) (get @<FUNS> (opsyn-key (unused-unary-ops (first (first parms))) 1)))
                                (and
                                    (clojure.core/= 3 (count (first parms)))
                                    (contains? unused-binary-ops (first (first parms)))
                                    (get @<FUNS> (opsyn-key (unused-binary-ops (first (first parms))) 2)))))
                            (let [
                                  N (first parms)
                                  f (if (clojure.core/= 2 (count N)) (get @<FUNS> (opsyn-key (unused-unary-ops (first N)) 1)) (get @<FUNS> (opsyn-key (unused-binary-ops (first N)) 2)))]
                                (when-some [args (eval-operands (rest N))] (let [r (apply f args)] (when (instance? NameResult r) (when-some [v (EVAL! (second parms))] (INVOKE '= (:target r) v))))))
                        (and (equal op '=) (call-form? (first parms))) (when-some [r (name-of-call (first parms))] (when-some [v (EVAL! (second parms))] (INVOKE '= (:target r) v)))
                        (equal op '=)
                            (let [
                                  [N R] parms
                                  N'
                                      (if
                                          (and (list? N) (>= (count N) 2) (not (clojure.core/= (first N) '&)))
                                          (let [container-sym (first N) container-val (if (list? container-sym) (EVAL! container-sym) ($$ container-sym))]
                                              (cond
                                                  (nil? container-val) nil
                                                  :else (when-some [ks (eval-operands (rest N))] (with-meta (apply list (if (list? container-sym) container-val container-sym) ks) (meta N)))))
                                          N)]
                                (let [
                                      c (when (list? N') (if (symbol? (first N')) ($$ (first N')) (first N')))
                                      field? (and (list? N') (symbol? (first N')) (not (:subscript (meta N'))) (:data (meta (get @<FUNS> (name (first N'))))))]
                                    (when
                                        (and N' (not (and (array? c) (not field?) (not (array-in-bounds? c (vec (rest N')))))))
                                        (let [r (EVAL! R)] (when (and (some? r) (symbol? N') (protected-variables N')) (protected-store! N' r)) (INVOKE '= N' r)))))
                        (equal op '?=)
                            (let [
                                  [N P R] parms
                                  N'
                                      (cond
                                          (and (list? N) (equal (first N) '$) (clojure.core/= 2 (count N))) (when-some [v (EVAL! (second N))] (indirect-target v))
                                          (and (list? N) (not (equal (first N) '&))) (when-some [ks (eval-operands (rest N))] (with-meta (apply list (first N) ks) (meta N)))
                                          :else N)]
                                (when N' (when-some [p (EVAL! P)] (INVOKE '?= N' p R))))
                        (and (equal op '&) (clojure.core/= 1 (count parms)) (pattern-keywords (str (first parms)))) ((pattern-keywords (str (first parms))))
                        (and (equal op '&) (clojure.core/= 1 (count parms)))
                            (let [
                                  [N] parms
                                  kw-sym (symbol (str "&" N))
                                  vr (when-not (clojure.core/= kw-sym '&) (ns-resolve (find-ns 'SNOBOL4clojure.core) kw-sym))
                                  v (if vr (var-get vr) (throw (ex-info "keyword operand is not name of defined keyword" {:snobol/error 251})))]
                                (if (instance? clojure.lang.IDeref v) @v v))
                        (and (equal op 'SEQ) (not (program-fn op))) (apply str (map #(sstr (EVAL! %)) parms))
                        (and (equal op '*) (clojure.core/= (count parms) 1)) (let [sym (first parms)] (with-meta (list 'DEFER! (fn [] (EVAL! sym))) {:defer-var sym}))
                        (list? op) (when-some [container (EVAL! op)] (when-some [args (eval-operands parms)] (apply INVOKE container args)))
                        true
                            (let [
                                  args (if (and (equal op 'tilde-op) (clojure.core/= 1 (count parms))) [(failure-point #(EVAL! (first parms)))] (eval-operands parms))
                                  opsyn-fn (get @<FUNS> (opsyn-key (get op-spelling op (str op)) (count parms)))]
                                (when args (deref-result (if opsyn-fn (apply opsyn-fn args) (apply INVOKE op args)))))))
            true E)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- name-of
    [X]
    (cond
        (symbol? X) X
        (and (list? X) (equal (first X) '$) (clojure.core/= 2 (count X))) (when-some [v (EVAL! (second X))] (indirect-target v))
        (and (list? X) (equal (first X) '&) (clojure.core/= 2 (count X))) X
        (and (list? X) (or (:subscript (meta X)) (not (symbol? (first X))))) (element-target X)
        (and (list? X) (not (contains? value-operators (first X))))
            (let [v (reference (first X))] (if (or (table? v) (array? v)) (element-target X) (when-some [args (eval-operands (rest X))] (call-by-name (first X) args))))
        :else (when-not (nil? (EVAL! X)) (returned-value!))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn keyword-var [k] (when (seq (name k)) (ns-resolve (the-ns 'SNOBOL4clojure.core) (symbol (str "&" (name k))))))
(def &ABEND (atom 0))
(def &CASE (atom 0))
(def &CODE (atom 0))
(def &COMPARE (atom 0))
(def &DUMP (atom 0))
(def &FTRACE (atom 0))
(def &FULLSCAN (atom 1))
(def &OUTPUT (atom 1))
(def &IOCOMPAT (atom :catspaw))
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
                            (or (neg? n) (> n 16777216) (and (clojure.core/= kw "PROFILE") (> n 1))))
                            (throw (ex-info "keyword value assigned is negative or too large" {:snobol/error 210}))
                        (and (clojure.core/= kw "MAXLNGTH") (< n 1024)) (throw (ex-info "value assigned to keyword maxlngth is too small" {:snobol/error 287}))
                        (and (clojure.core/= kw "FULLSCAN") (zero? n)) (throw (ex-info "value assigned to keyword fullscan is zero" {:snobol/error 274}))
                        (clojure.core/= kw "STLIMIT") (let [a (long @&STCOUNT)] (reset! <ST-DEAD> (cond (neg? n) nil (< n a) :never :else (clojure.core/+ a n))) (reset! &STLIMIT n) n)
                        (or (contains? keyword-defaults vr) (clojure.core/= vr #'&TRACE)) (do (reset! @vr n) n)
                        :else (throw (ex-info "keyword in assignment is protected" {:snobol/error 209}))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn table-set [t k v] (let [k (table-key k)] (when-let [o (:order (meta t))] (swap! o (fn [m] (if (contains? m k) m (assoc m k (count m)))))) (swap! t assoc k v)) v)
(def <ETRACES> (atom []))
(def elements <ETRACES>)
(defn- emit-line! [text] (when (pos? @&TRACE) (swap! &TRACE dec) (trace-println (star-field) " " text)))
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
(defn holder-name
    [v]
    (let [last @<PRNMV>]
        (if
            (and last (identical? v (get @<VARS> last)))
            (name last)
            (when-let [k (first (sort-by (fn [k] [(var-slot (name k)) (name k)]) (keep (fn [[k x]] (when (identical? x v) k)) @<VARS>)))] (reset! <PRNMV> k) (name k)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-element!
    [c ks new-val]
    (when-let [e (and (seq @elements) (first (filter #(and (clojure.core/= :value (:kind %)) (identical? c (:c %)) (clojure.core/= (vec ks) (:ks %))) @elements)))]
        (if-let [f (and (pos? @&TRACE) (trace-fn e))]
            (call-trace-fn! f (:text e) e)
            (emit-line! (str (apply str (repeat @&FNCLEVEL \i)) " " (if-let [h (holder-name c)] (str h (subs (:text e) (.indexOf ^String (:text e) "<"))) (:text e)) " = " (trace-val new-val))))
        (:label e)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn array-set [^SnobolArray arr idx-vec v] (when-let [k (array-check-bounds arr idx-vec)] (swap! (:data arr) assoc k v) v))
(defn- output-text [v] (if (or (and (or (list? v) (vector? v)) (#{"PATTERN" "EXPRESSION"} (DATATYPE v))) (textless? v)) (DATATYPE v) (sstr v)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn write-to-channel!
    [var-sym val]
    (when-let [ch (get @<CHANNELS> var-sym)] (when (clojure.core/= :output (:type ch)) (let [wtr ^java.io.PrintWriter (:writer ch)] (.println wtr (sstr val)) (.flush wtr) val))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-value!
    [sym new-val]
    (when-let [entry (@registry (reg-key sym :value))]
        (if-let [f (and (pos? @&TRACE) (trace-fn entry))] (call-trace-fn! f sym entry) (emit-line! (str (apply str (repeat @&FNCLEVEL \i)) " " sym " = " (trace-val new-val))))
        (:label entry)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ?= ([n s p] (match-replace n s p)))
(defn- saved-value [sym] (if (output-associated? sym) (get @<ASSOC-LAST> sym) (reference sym)))
(def ^:dynamic *eval-block* nil)
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
(defn RUN [at] (run* at nil nil))
(def &RTNTYPE (atom ε))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-return!
    [fname val signal & [caller-stno]]
    (function-line! fname :return #(case signal :freturn (str "FRETURN " fname) :nreturn (str "NRETURN " fname " = " (trace-val val)) (str "RETURN " fname " = " (trace-val val))) caller-stno))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- restore-var! [sym v] (if (nil? v) (do (swap! <VARS> dissoc sym) (swap! <ASSOC-LAST> dissoc sym)) (snobol-set! sym v)))
(def <NAMES> (atom #{}))
(defn note-names! [names] (when (seq names) (swap! <NAMES> into names)))
(def <FDEFS> (atom {}))
(defn APPLY [fname & fargs] (apply INVOKE (applied-function fname) fargs))
(defn- str-arg ^String [x n] (gtstg x n))
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
(def <OBJNO> (atom 0))
(defn next-serial! [] (swap! <OBJNO> inc))
(defn- new-table ([size] (new-table size ε)) ([size dflt] (atom {} :meta {:order (atom {}) :size size :serial (next-serial!) :dflt dflt})))
(defn- table-size [n] (let [k (num n)] (cond (and (integer? k) (pos? k)) k (and (double? k) (pos? k)) (long k) :else 11)))
(defn TABLE ([] (new-table 11)) ([n] (new-table (table-size n))) ([n _x] (new-table (table-size n))) ([n _x d] (new-table (table-size n) (if (nil? d) ε d))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private fixed-arity
    (memoize
        (fn
            [^Class c]
            (let [ms (.getDeclaredMethods c)]
                (when-not
                    (some (fn [^java.lang.reflect.Method m] (clojure.core/= "doInvoke" (.getName m))) ms)
                    (reduce max -1 (keep (fn [^java.lang.reflect.Method m] (when (clojure.core/= "invoke" (.getName m)) (alength (.getParameterTypes m)))) ms)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- apply-builtin [f args] (let [n (fixed-arity (class f))] (if (and n (>= n 0) (> (count args) n)) (apply f (take n args)) (apply f args))))
(defn- dimension [lo hi] (when (> (long lo) (long hi)) (throw (ex-info (error-message 67) {:snobol/error 67}))) [lo hi])
(defn- array-bound [x err] (let [n (num x)] (cond (integer? n) (long n) (and (double? n) (not (Double/isNaN n))) (long n) :else (throw (ex-info (error-message err) {:snobol/error err})))))
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
(defn- real-error [code msg] (throw (ex-info (or (error-message code) msg) {:snobol/error code})))
(defn- finite-or [^double d code msg] (cond (Double/isInfinite d) (real-error code msg) (< (Math/abs d) Double/MIN_NORMAL) (clojure.core/* d 0.0) :else d))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn REMDR
    [x y]
    (let [a (num x) b (num y) nan? #(and (double? %) (Double/isNaN ^double %))]
        (when (nan? b) (throw (ex-info "remdr second argument is not numeric" {:snobol/error 165})))
        (when (nan? a) (throw (ex-info "remdr first argument is not numeric" {:snobol/error 166})))
        (if
            (and (integer? a) (integer? b))
            (if (zero? b) (throw (ex-info "remdr caused integer overflow" {:snobol/error 167})) (rem (long a) (long b)))
            (let [
                  a (double a)
                  q (clojure.core// a (double b))
                  _ (when (or (Double/isInfinite q) (Double/isNaN q)) (throw (ex-info "remdr caused real overflow" {:snobol/error 312})))
                  q (finite-or q 312 "remdr caused real overflow")
                  d (finite-or (clojure.core/- (finite-or (clojure.core/* (if (neg? q) (Math/ceil q) (Math/floor q)) (double b)) 312 "") a) 312 "")]
                (if (neg? a) (if (<= d 0.0) d (clojure.core/- d)) (if (neg? d) (clojure.core/- d) d))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn TRACE [] nil)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn trace-kinds
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
                    (swap! elements
                        (fn
                            [es]
                            (conj
                                (vec (remove #(and (identical? c (:c %)) (clojure.core/= ks (:ks %)) (clojure.core/= kind (:kind %))) es))
                                {:c c :ks ks :kind kind :text text :label label :fn user-fn}))))
                (doseq [kind kinds] (swap! registry assoc (reg-key name kind) {:label label :fn user-fn}))))
        nil))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn STOPTR [] nil)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn stoptr!
    [name type]
    (if-let [[c ks _] (element-of name)]
        (let [kinds (set (trace-kinds type))] (swap! elements (fn [es] (vec (remove #(and (identical? c (:c %)) (clojure.core/= ks (:ks %)) (kinds (:kind %))) es)))))
        (doseq [kind (trace-kinds type)] (swap! registry dissoc (reg-key name kind))))
    nil)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn COLLECT [] ε)
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
(defn CLEAR [] ε)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn clear-vars!
    [s]
    (let [kept (into #{} (comp (map clojure.string/trim) (remove empty?) (map symbol)) (clojure.string/split (sstr s) #",")) null (fn [m] (reduce-kv (fn [m k _] (if (kept k) m (assoc m k ""))) m m))]
        (swap! <VARS> null)
        (swap! <ASSOC-LAST> null)
        ""))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn channel-key [u] (if (integer? u) u (let [t (clojure.string/trim (if (nil? u) "" (sstr u)))] (cond (clojure.core/= "" t) 0 (re-matches #"[+-]?\d+" t) (Long/parseLong t) :else t))))
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
                          (let [^bytes b (java.nio.file.Files/readAllBytes (.toPath ^java.io.File f)) eol? (fn [i] (== 10 (aget b (dec i))))]
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
(defn EJECT [] ε)
(def ^:private goto-system-labels #{"ABORT" "CONTINUE" "SCONTINUE" "RETURN" "FRETURN" "NRETURN" "END" "end"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SETEXIT
    ([] (SETEXIT ε))
    ([label]
        (let [prev @<SETEXIT> s (gtstg label 187) s s]
            (when-not (or (clojure.core/= "" s) (goto-system-labels s) (@<STNO> (keyword s))) (throw (ex-info (error-message 187) {:snobol/error 187})))
            (reset! <SETEXIT> (when-not (clojure.core/= "" s) (keyword s)))
            (if prev (NAME. (symbol (name prev))) ε))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def <HOST0> (atom ""))
(def <ARGV> (atom []))
(def <ARGI> (atom 0))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn HOST
    ([] (str "jvm:" (System/getProperty "os.name") ":SNOBOL4clojure 0.2.0"))
    ([i & more]
        (if
            (or (nil? i) (clojure.core/= "" i))
            (HOST)
            (let [n (num i) bad #(throw (ex-info "erroneous argument for host" {:snobol/error 254}))]
                (cond
                    (and (integer? n) (zero? n)) (str @<HOST0>)
                    (and (integer? n) (clojure.core/= 1 n))
                        (let [cmd (str (or (first more) ""))]
                            (if
                                (clojure.core/= "" cmd)
                                0
                                (do
                                    (flush)
                                    (let [p (-> (ProcessBuilder. ^java.util.List ["/bin/sh" "-c" cmd]) (.inheritIO) (.start)) code (long (.waitFor p))]
                                        (if (> code 128) (clojure.core/- code 128) (clojure.core/* code 256))))))
                    (and (integer? n) (clojure.core/= 2 n)) (let [k (if (or (nil? (first more)) (clojure.core/= "" (first more))) 0 (num (first more)))] (if (integer? k) (get @<ARGV> k) (bad)))
                    (and (integer? n) (clojure.core/= 3 n)) (long @<ARGI>)
                    (and (integer? n) (clojure.core/= 4 n)) (let [nm (str (or (first more) ""))] (if (clojure.core/= "" nm) (bad) (System/getenv nm)))
                    (and (integer? n) (clojure.core/= -1 n))
                        (let [k (num (first more)) frames #(long (count (.getStackTrace (Thread/currentThread))))]
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
(defn DUMP [] ε)
(def ^:private pattern-variables '#{ABORT ARB BAL FAIL FENCE REM SUCCEED})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- named-patterns
    []
    (into {} (for [n @<NAMES> :let [s (symbol n)] :when (pattern-variables s)] [s (if (clojure.core/= s 'FENCE) @(ns-resolve (the-ns 'SNOBOL4clojure.core) 'bare-fence) (primitive-pattern s))])))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- variables
    [lvl]
    (let [vars (cond-> (merge (named-patterns) (into {} (for [s @<PRIMS-READ>] [s (deref (when (builtin-names s) (ns-resolve (the-ns 'SNOBOL4clojure.core) s)))])) @<VARS>) true (merge @<ASSOC-LAST>))]
        (if
            (>= (long lvl) 3)
            (sort-by
                (comp name key)
                (for [[k v] (merge (into {} (for [n (into @<NAMES> ["INPUT" "OUTPUT" "TERMINAL"])] [(symbol n) ""])) vars)] (clojure.lang.MapEntry/create k (if (or (nil? v) (fn? v)) "" v))))
            (sort-by (comp name key) (remove (fn [[_ v]] (or (nil? v) (clojure.core/= "" v) (fn? v))) vars)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def <WIDTH> (atom 120))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private keyword-order
    '[ANCHOR CASE CODE DUMP ERRLIMIT ERRTEXT ERRTYPE FILE FNCLEVEL FTRACE FULLSCAN INPUT LASTFILE LASTLINE LASTNO LINE MAXLNGTH OUTPUT PROFILE RTNTYPE STCOUNT STLIMIT STNO TRACE TRIM])
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- keywords [] (for [k keyword-order] (let [v @@(ns-resolve (the-ns 'SNOBOL4clojure.core) (symbol (str "&" k)))] (str "&" k " = " (if (string? v) (str "'" v "'") (sstr v))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- key-hash
    [k]
    (let [mag (fn [^long x] (cond (clojure.core/= x Long/MIN_VALUE) 0 (neg? x) (clojure.core/- x) :else x))]
        (cond
            (string? k)
                (let [b ^bytes (.getBytes ^String k "ISO-8859-1") n (alength b)]
                    (if
                        (zero? n)
                        0
                        (bit-and Long/MAX_VALUE
                            (reduce
                                (fn
                                    [h w]
                                    (bit-xor h
                                        (reduce
                                            (fn
                                                [acc j]
                                                (let [i (clojure.core/+ (clojure.core/* 8 w) j)] (if (< i n) (bit-or acc (bit-shift-left (bit-and 0xff (long (aget b i))) (clojure.core/* 8 j))) acc)))
                                            0
                                            (range 8))))
                                (long n)
                                (range (min 3 (quot (clojure.core/+ n 7) 8)))))))
            (integer? k) (mag (long k))
            (double? k) (mag (Double/doubleToRawLongBits k))
            :else nil)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn table-entries
    [t]
    (let [order (if-let [o (:order (meta t))] @o {})] (map (fn [[k v]] [(if (instance? ObjKey k) (.v ^ObjKey k) k) v]) (sort-by (fn [[k _]] (get order k Long/MAX_VALUE)) (seq @t)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- parts
    [v]
    (cond
        (array? v) (for [[k x] (sort-by key @(:data v))] [:sub k x])
        (table? v) (let [n (long (:size (meta v) 11))] (for [[k x] (sort-by (fn [[k _]] (if-some [h (key-hash k)] (mod h n) Long/MAX_VALUE)) (table-entries v))] [:sub [k] x]))
        (instance? DataObj v) (map (fn [f x] [:field f x]) (.field-names ^DataObj v) (seq (.fields ^DataObj v)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- object-lines
    [vars v]
    (let [holder (or (some (fn [[k x]] (when (identical? x v) (str k))) vars) (value-text v))]
        (concat
            ["" (value-text v)]
            (for [[kind label x] (parts v) :when (and (some? x) (not= "" x))]
                (case kind :sub (str holder "<" (subscript-text label) "> = " (value-text x)) :field (str label "(" holder ") = " (value-text x)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- serial-of [v] (cond (array? v) (:serial v) (table? v) (:serial (meta v)) (instance? DataObj v) (.serial ^DataObj v)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- objects
    [vars]
    (loop [todo (map val vars) seen (java.util.IdentityHashMap.) out []]
        (if-let [[v & more] (seq todo)]
            (if (and (serial-of v) (not (.containsKey seen v))) (do (.put seen v true) (recur (concat more (map #(nth % 2) (parts v))) seen (conj out v))) (recur more seen out))
            (sort-by serial-of out))))
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
(defn- real-arg [x code fname] (let [n (num x)] (if (and (double? n) (Double/isNaN ^double n)) (real-error code (str fname " argument not numeric")) (double n))))
(defn ATAN [x] (Math/atan (real-arg x 301 "ATAN")))
(defn CHOP [x] (let [d (real-arg x 302 "CHOP")] (clojure.core/+ 0.0 (if (neg? d) (Math/ceil d) (Math/floor d)))))
(defn COS [x] (Math/cos (real-arg x 303 "COS")))
(defn EXP [x] (finite-or (Math/exp (real-arg x 304 "EXP")) 305 "EXP produced real overflow"))
(defn LN [x] (let [d (real-arg x 306 "LN")] (cond (neg? d) (real-error 315 "LN argument negative") (zero? d) (real-error 307 "LN produced real overflow") :else (Math/log d))))
(defn SIN [x] (Math/sin (real-arg x 308 "SIN")))
(defn SQRT [x] (let [d (real-arg x 313 "SQRT")] (if (neg? d) (real-error 314 "SQRT argument negative") (Math/sqrt d))))
(defn TAN [x] (finite-or (Math/tan (real-arg x 309 "TAN")) 310 "TAN produced real overflow"))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn INTEGER
    [x]
    "Convert x to integer, or return ε (fail) if not convertible."
    (cond (integer? x) x (number? x) (long x) :else (try (Long/parseLong (clojure.string/trim (str x))) (catch Exception _ ε))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn integer-predicate [x] (cond (integer? x) ε (number? x) nil :else (when (re-matches #"|[+-]?[0-9]+" (clojure.string/trim (str x))) ε)))
(defn SIZE [s] (long (count (str-arg s 189))))
(defn TRIM [s] (let [^String t (str-arg s 200)] (subs t 0 (loop [i (count t)] (if (and (pos? i) (#{\space \tab} (.charAt t (dec i)))) (recur (dec i)) i)))))
(defn- int-arg [x err] (if-some [n (integer-of x)] n (throw (ex-info "argument is not integer" {:snobol/error err}))))
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
                        (> n 16777216) nil
                        (> (clojure.core/* n (count s)) m) (throw (ex-info "string length exceeds value of maxlngth keyword" {:snobol/error 205}))
                        :else (apply str (repeat n s))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn REVERSE [s] (apply str (clojure.string/reverse (str-arg s 177))))
(defn ITEM [c & ks] (cond (table? c) (table-get c (table-subscript ks)) (array? c) (array-get c (vec ks)) :else nil))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn array-prototype
    [^SnobolArray arr]
    (let [proto (or (:proto arr) (clojure.string/join "," (map (fn [[lo hi]] (str lo ":" hi)) (:dims arr))))] (if (re-matches #"[+]?[0-9]+" proto) (second (first (:dims arr))) proto)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn PROTOTYPE [x & _] (cond (table? x) (:size (meta x) 11) (array? x) (array-prototype x) :else (throw (ex-info "prototype argument is not valid object" {:snobol/error 164}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn table->array
    ([t] (table->array t false))
    ([t sorting?]
        (let [
              entries (remove (fn [[_ v]] (or (nil? v) (clojure.core/= "" v))) (table-entries t))
              entries
                  (if sorting?
                      (let [kh (deref (ns-resolve (the-ns 'SNOBOL4clojure.core) 'key-hash)) n (long (:size (meta t) 11))]
                          (sort-by (fn [[k _]] (if-some [h (kh k)] (clojure.core/- (mod h n)) 1)) entries))
                      entries)
              n (count entries)]
            (when (pos? n) (let [arr (ARRAY (str n ",2"))] (doseq [[i [k v]] (map-indexed vector entries)] (array-set arr [(inc i) 1] k) (array-set arr [(inc i) 2] v)) arr)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CONVERT
    [x t]
    "CONVERT(x, type) — coerce x to the named SNOBOL4 type.
   Returns nil (failure) if the conversion is not defined."
    (when-not (or (and (string? t) (seq t)) (number? t) (instance? NAME t)) (throw (ex-info (error-message 74) {:snobol/error 74})))
    (let [from (clojure.string/upper-case (DATATYPE x)) to (if (zero? (long @&CASE)) (str t) (clojure.string/upper-case (str t)))]
        (cond
            (clojure.core/= from to) x
            (clojure.core/= to "NUMERIC") (cond (number? x) x (string? x) (let [n (num x)] (when-not (and (double? n) (Double/isNaN n)) n)) :else nil)
            (and (clojure.core/= from "STRING") (clojure.core/= to "INTEGER"))
                (let [t (clojure.string/trim (str x))]
                    (if (sign-then-blanks? x) 0 (if (re-matches #"[+-]?\d+" t) (try (Long/parseLong t) (catch NumberFormatException _ nil)) (when-some [d (string->real x)] (real->integer d)))))
            (and (clojure.core/= from "STRING") (clojure.core/= to "REAL")) (string->real x)
            (and (clojure.core/= from "STRING") (clojure.core/= to "PATTERN")) (list 'SEQ (list 'LIT$ (str x)))
            (and (clojure.core/= from "STRING") (clojure.core/= to "NAME")) (NAME. (symbol (str x)))
            (and (clojure.core/= from "INTEGER") (clojure.core/= to "STRING")) (str x)
            (and (clojure.core/= from "INTEGER") (clojure.core/= to "REAL")) (double x)
            (and (clojure.core/= from "INTEGER") (clojure.core/= to "PATTERN")) (list 'SEQ (list 'LIT$ (str x)))
            (and (clojure.core/= from "INTEGER") (clojure.core/= to "NAME")) (NAME. (symbol (str x)))
            (and (clojure.core/= from "REAL") (clojure.core/= to "STRING")) (sstr x)
            (and (clojure.core/= from "REAL") (clojure.core/= to "INTEGER")) (real->integer x)
            (and (clojure.core/= from "REAL") (clojure.core/= to "PATTERN")) (list 'SEQ (list 'LIT$ (sstr x)))
            (and (clojure.core/= from "REAL") (clojure.core/= to "NAME")) (NAME. (symbol (sstr x)))
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
            (and (clojure.core/= from "NAME") (#{"STRING" "INTEGER" "REAL" "PATTERN"} to) (instance? NAME x) (not (symbol? (.n ^NAME x)))) nil
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
(defn- expression-of [ir] (list 'DEFER! (fn [] (EVAL! ir))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn COPY
    [x]
    (cond
        (table? x) (let [t (atom @x :meta {:order (atom (if-let [o (:order (meta x))] @o {})) :size (:size (meta x) 11) :serial (next-serial!)})] t)
        (array? x) (assoc (->SnobolArray (:dims x) (:dflt x) (atom @(:data x))) :proto (:proto x) :serial (next-serial!))
        (instance? DataObj x) (->DataObj (.type-name ^DataObj x) (.field-names ^DataObj x) (aclone ^objects (.fields ^DataObj x)) (next-serial!))
        (instance? CodeBlock x) (->CodeBlock (.entry ^CodeBlock x))
        (and (seq? x) (#{"PATTERN" "EXPRESSION"} (DATATYPE x))) (with-meta (apply list x) (meta x))
        (and (vector? x) (clojure.core/= "PATTERN" (DATATYPE x))) (with-meta (into [] x) (meta x))
        :else x))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def data-type-registry (atom {}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn FIELD
    [type-name n]
    (let [n (if (clojure.core/= "" (str n)) 0 (int-arg n 107)) tname (str type-name) fields (get @data-type-registry tname)]
        (when-not fields (throw (ex-info "field first argument is not datatype name" {:snobol/error 108})))
        (when (and (>= (long n) 1) (<= (long n) (count fields))) (NAME. (symbol (str (nth fields (dec (long n)))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- sort-serial [v] (cond (array? v) (:serial v) (table? v) (:serial (meta v)) (instance? DataObj v) (.serial ^DataObj v)))
(def ^:private sort-type-rank {"INTEGER" 0 "NAME" 1 "PATTERN" 2 "REAL" 3 "CODE" 4 "STRING" 5 "EXPRESSION" 6 "TABLE" 7 "ARRAY" 8})
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
    [arr col descending? fresh?]
    (let [
          dims (:dims arr)
          lo1 (first (first dims))
          hi1 (second (first dims))
          two-d? (clojure.core/= 2 (count dims))
          _ (when (< 2 (count dims)) (throw (ex-info (error-message 256) {:snobol/error 256})))
          [lo2 hi2] (second dims)
          k
              (when two-d?
                  (let [k (if (or (nil? col) (clojure.core/= "" col)) lo2 (integer-of col))]
                      (if (and k (<= lo2 k hi2)) (clojure.core/- k lo2) (throw (ex-info (error-message 258) {:snobol/error 258})))))
          cols (when two-d? (range lo2 (inc hi2)))
          rows (for [i (range lo1 (inc hi1))] (if two-d? (vec (for [j cols] (array-get arr [i j]))) [(array-get arr [i])]))
          pick (fn [row] (if two-d? (nth row k) (first row)))
          cmp (if descending? #(sort-key-compare %2 %1) sort-key-compare)
          sorted (sort-by pick cmp rows)
          out (if fresh? arr (ARRAY (:proto arr)))]
        (doseq [[i row] (map vector (range lo1 (inc hi1)) sorted)] (if two-d? (doseq [[j v] (map vector cols row)] (array-set out [i j] v)) (array-set out [i] (first row))))
        out))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- sort-any
    [A col descending?]
    (cond
        (table? A) (when-some [arr (table->array A true)] (sort-array arr col descending? true))
        (array? A) (sort-array A col descending? false)
        :else (throw (ex-info (error-message 256) {:snobol/error 256}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SORT ([A] (SORT A nil)) ([A col] (sort-any A col false)))
(defn RSORT ([A] (RSORT A nil)) ([A col] (sort-any A col true)))
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
        (when (instance? DataObj inst) (let [fields (.field-names ^DataObj inst) i (.indexOf ^java.util.List (vec fields) f)] (when (>= i 0) i)))
        (throw (ex-info "field function argument is wrong datatype" {:snobol/error 41}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn field-fn
    [f]
    (fn ([inst] (let [i (int (field-slot f inst))] (aget ^objects (.fields ^DataObj inst) i))) ([inst v] (let [i (int (field-slot f inst))] (aset ^objects (.fields ^DataObj inst) i v) v))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn DATA!
    [S]
    (when-let [[type-name fields] (parse-data-spec S)]
        (let [tname type-name fnames fields]
            (swap! data-type-registry assoc tname fnames)
            `(do
                 (swap! <FUNS> assoc ~tname (with-meta (fn [& args#] (->DataObj ~tname ~fnames (object-array (map-indexed (fn [i# _#] (nth args# i# ε)) ~fnames)) (next-serial!))) {:data true}))
                 ~@(for [f fnames] `(swap! <FUNS> assoc ~f (with-meta (field-fn ~f) {:data true :field true})))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn DATA [S] (when-let [form (DATA! S)] (when-let [[t fs] (parse-data-spec S)] (note-names! (cons t fs))) (eval form) ε))
(defn CHAR [n] (let [n (int-arg n 281)] (if (<= 0 n 255) (str (char n)) (throw (ex-info "char argument not in range" {:snobol/error 282})))))
(defn ORD [s] (let [t (sstr s)] (when (seq t) (long (int (first t))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SUBSTR
    [s i n]
    (let [n (int-arg n 192) i (int-arg i 193) s (str-arg s 194) len (count s)]
        (when (and (<= 1 i (inc len)) (>= n 0)) (if (zero? n) (subs s (dec i)) (when (<= (clojure.core/+ (dec i) n) len) (subs s (dec i) (clojure.core/+ (dec i) n)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn DATE
    ([] (DATE 0))
    ([i]
        (let [n (if (or (nil? i) (clojure.core/= "" i)) 0 (num i))]
            (when-not (integer? n) (throw (ex-info "date argument is not integer" {:snobol/error 330})))
            (.format (java.time.LocalDateTime/now) (java.time.format.DateTimeFormatter/ofPattern (case (long n) 1 "MM/dd/yyyy HH:mm:ss" 2 "yyyy-MM-dd HH:mm:ss" "MM/dd/yy HH:mm:ss"))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn TIME [] (System/currentTimeMillis))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn LPAD
    ([s n] (LPAD s n " "))
    ([s n fill]
        (let [f (str-arg fill 144) w (int-arg n 145) s-str (str-arg s 146) pad (clojure.core/- w (count s-str)) fill-c (if (clojure.core/= "" f) " " (subs f 0 1))]
            (if (<= pad 0) s-str (str (apply str (repeat pad fill-c)) s-str)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn RPAD
    ([s n] (RPAD s n " "))
    ([s n fill]
        (let [f (str-arg fill 178) w (int-arg n 179) s-str (str-arg s 180) pad (clojure.core/- w (count s-str)) fill-c (if (clojure.core/= "" f) " " (subs f 0 1))]
            (if (<= pad 0) s-str (str s-str (apply str (repeat pad fill-c)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- lexical [x y n1 test] (let [b (gtstg y (inc (long n1))) a (gtstg x n1)] (when (test (compare a b)) ε)))
(defn LEQ ([] (LEQ ε ε)) ([x] (LEQ x ε)) ([x y & _] (lexical x y 122 zero?)))
(defn LNE ([] (LNE ε ε)) ([x] (LNE x ε)) ([x y & _] (lexical x y 132 #(not (zero? %)))))
(defn LLE ([] (LLE ε ε)) ([x] (LLE x ε)) ([x y & _] (lexical x y 128 #(<= % 0))))
(defn LLT ([] (LLT ε ε)) ([x] (LLT x ε)) ([x y & _] (lexical x y 130 neg?)))
(defn LGE ([] (LGE ε ε)) ([x] (LGE x ε)) ([x y & _] (lexical x y 124 #(>= % 0))))
(defn LGT ([] (LGT ε ε)) ([x] (LGT x ε)) ([x y & _] (lexical x y 126 pos?)))
(defn ARG [fname n] (when-let [meta (get @<FDEFS> (str fname))] (when-some [p (nth (:params meta) (dec (long n)) nil)] (NAME. (symbol p)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- arg-or-local
    [[fname n] k e1 t1 e2 t2]
    (let [
          meta (get @<FDEFS> (name-string fname))
          _ (when-not meta (throw (ex-info t1 {:snobol/error e1})))
          i (let [v (num n)] (if (and (number? v) (not (and (double? v) (Double/isNaN v)))) (long v) (throw (ex-info t2 {:snobol/error e2}))))]
        (when-some [p (when (pos? i) (nth (k meta) (dec i) nil))] (NAME. (symbol p)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn LOCAL [fname n] (when-let [meta (get @<FDEFS> (str fname))] (when-some [p (nth (:locals meta) (dec (long n)) nil)] (NAME. (symbol p)))))
(defn READ-LINE! [] (let [line (try (.readLine (stdin-reader)) (catch Exception _ nil))] (if (nil? line) ε (input-record line))))
(defn INPUT [] (READ-LINE!))
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
(defn- channel-variable [name-arg] (cond (symbol? name-arg) name-arg (instance? NAME name-arg) (symbol (str (.n ^NAME name-arg))) :else (symbol (str name-arg))))
(defn OUTPUT [] ε)
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
(defn ENDFILE ([] ε) ([unit] (close-channel! unit)))
(defn DETACH ([] ε) ([var-sym] (close-channel! (symbol (str var-sym)))))
(defn REWIND ([] ε) ([unit] (when-let [ch (get @<CHANNELS> unit)] (when-let [rdr ^java.io.BufferedReader (:reader ch)] (try (.reset rdr) (catch Exception _ nil))) ε)))
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
(defn EXIT [] nil)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn EXIT
    ([] (EXIT nil nil))
    ([a] (EXIT a nil))
    ([a b & _]
        (let [n (when-not (string? a) (num a))]
            (cond
                (and (string? a) (seq a))
                    (do
                        (flush)
                        (let [p (-> (ProcessBuilder. ^java.util.List ["/bin/bash" "-c" a]) (.inheritIO) (.start)) code (long (.waitFor p))]
                            (throw (ex-info "exit" {:snobol/signal :exit :snobol/rc code}))))
                (and (integer? n) (clojure.core/= -3 n)) (throw (ex-info "exit" {:snobol/signal :exit :snobol/rc 0}))
                :else (throw (ex-info (error-message 105) {:snobol/error 105}))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn preprocess-includes*
    [src search-path seen]
    (let [include-re #"^-INCLUDE\s+['\"]([^'\"]+)['\"]"]
        (loop [lines (string/split src #"\n") out [] seen seen cards [] ph 0]
            (if-let [line (first lines)]
                (if-let [[_ fname] (re-find include-re line)]
                    (let [found (some (fn [dir] (let [f (clojure.java.io/file dir (string/trim fname))] (when (.exists f) f))) search-path)]
                        (cond
                            (seen fname) (recur (rest lines) (conj out (str "* -INCLUDE skipped (already included): " fname)) seen cards ph)
                            (nil? found) (recur (rest lines) (conj out line) (conj seen (string/trim fname)) cards (inc ph))
                            :else
                                (let [
                                      text (slurp found :encoding "ISO-8859-1")
                                      [body seen' inner] (preprocess-includes* text (distinct (cons (.getParent found) search-path)) (conj seen (string/trim fname)))
                                      last-line (let [ls (string/split text #"\r?\n" -1)] (last (if (and (next ls) (clojure.core/= "" (peek ls))) (pop ls) ls)))
                                      stale? (boolean (and last-line (or (string/starts-with? last-line "*") (re-find #";[ \t]*(\*[^'\"]*)?$" last-line))))]
                                    (recur
                                        (rest lines)
                                        (into out (concat [(str "-\uE000FILE-ENTER \"" (string/trim fname) "\"")] (string/split body #"\n") [(str "-\uE000FILE-LEAVE" (when stale? " STALE"))]))
                                        seen'
                                        (into (conj cards line) inner)
                                        ph))))
                    (if
                        (and (pos? ph) (re-find #"^(END|end)([ \t;]|$)" line) (empty? (rest lines)) (not (string/ends-with? src "\n\n")))
                        (recur (rest lines) (conj out "-\uE000DEPTH-LEAVE") seen cards (dec ph))
                        (recur (rest lines) (conj out line) seen cards ph)))
                [(string/join "\n" out) seen cards]))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- open-parens
    [^String s]
    (loop [i 0 d 0 q nil]
        (if
            (>= i (count s))
            d
            (let [c (.charAt s i)]
                (cond
                    q (recur (inc i) d (if (clojure.core/= c q) nil q))
                    (#{\' \"} c) (recur (inc i) d c)
                    (clojure.core/= c \() (recur (inc i) (inc d) q)
                    (clojure.core/= c \)) (recur (inc i) (dec d) q)
                    :else (recur (inc i) d q))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- split-source
    ([src] (split-source src false))
    ([src record?]
        (let [
              chars (if (or record? (clojure.core/= "" (str src)) (string/ends-with? (str src) "\n")) (str src) (str src "\n"))
              n (count chars)
              trimr (if record? #(string/replace % #"[ \t]+\z" "") #(string/replace % #"[ \t\n]+\z" ""))
              blank-line? (fn [from to] (every? #(and (Character/isWhitespace ^char %) (not= \return %)) (subs chars from to)))]
            (loop [i 0 cur (StringBuilder.) stmts [] in-sq false in-dq false ls 0 line 1 sl 1 diags (if (and (pos? n) (#{\+ \.} (.charAt chars 0))) [[1 214]] [])]
                (if
                    (>= i n)
                    (let [s (trimr (str cur))] (with-meta (cond (seq s) (conj stmts [s sl]) (and record? (pos? (.length cur))) (conj stmts [:blank sl]) :else stmts) {:diagnostics diags}))
                    (let [ch (.charAt chars i) sl (if (zero? (.length cur)) line sl)]
                        (cond
                            (and (#{\' \"} ch) (pos? (.length cur)) (not (Character/isWhitespace (.charAt cur 0))) (not (string/includes? (str cur) " ")) (not (string/includes? (str cur) "\t")))
                                (recur (inc i) (.append cur ch) stmts in-sq in-dq ls line sl diags)
                            (and (clojure.core/= ch \') (not in-dq)) (recur (inc i) (.append cur ch) stmts (not in-sq) in-dq ls line sl diags)
                            (and (clojure.core/= ch \") (not in-sq)) (recur (inc i) (.append cur ch) stmts in-sq (not in-dq) ls line sl diags)
                            (and
                                (clojure.core/= ch \;)
                                (not in-sq)
                                (not in-dq)
                                (not (and (pos? (.length cur)) (clojure.core/= \* (.charAt cur 0))))
                                (not (pos? (long (open-parens (let [t (str cur)] (if (and (seq t) (not (Character/isWhitespace (.charAt t 0)))) (subs t (or (string/index-of t " ") (count t))) t))))))
                                (not (and (pos? (.length cur)) (let [c (.charAt cur 0)] (not (or (Character/isLetterOrDigit c) (#{\space \tab \- \+ \. \( \[ \< \> \: \,} c)))))))
                                (let [s (trimr (str cur))] (recur (inc i) (StringBuilder.) (conj stmts (if (seq s) [s sl] [:blank sl])) false false ls line line diags))
                            (and (clojure.core/= ch \newline) (not record?))
                                (let [
                                      s (trimr (str cur))
                                      next-i (inc i)
                                      next-ch (when (< next-i n) (.charAt chars next-i))
                                      cont? (and next-ch (or (clojure.core/= next-ch \+) (clojure.core/= next-ch \.)))]
                                    (if
                                        (seq s)
                                        (cond
                                            (and cont? (clojure.core/= \* (.charAt s 0)))
                                                (recur (inc i) (StringBuilder.) (conj stmts [s sl]) false false (inc i) (inc line) (inc line) (conj diags [(inc line) 214]))
                                            cont? (recur (clojure.core/+ i 2) (.append cur \space) stmts false false (inc i) (inc line) sl diags)
                                            :else (recur (inc i) (StringBuilder.) (conj stmts [s sl]) false false (inc i) (inc line) (inc line) diags))
                                        (let [blank? (blank-line? ls i) tail? (and (pos? (.length cur)) (clojure.core/= i (dec n)) (not (string/ends-with? (str src) "\n")))]
                                            (recur
                                                (inc i)
                                                (StringBuilder.)
                                                (if (or blank? tail?) (conj stmts [:blank line]) stmts)
                                                false false
                                                (inc i)
                                                (inc line)
                                                (inc line)
                                                (if (and cont? (not blank?)) (conj diags [(inc line) 214]) diags)))))
                            :else (recur (inc i) (.append cur ch) stmts in-sq in-dq ls line sl diags))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def <DIAGNOSTICS> (atom []))
(def <SOURCE> (atom nil))
(defn- comment? [cmd] (re-find #"^\*" cmd))
(defn- control? [cmd] (re-find #"^\-" cmd))
(defn- case-card [fold command] (when-let [[_ n] (re-find (if fold #"(?i)^-CASE(?:\s+(\d+))?\s*$" #"^-CASE(?:\s+(\d+))?\s*$") command)] (boolean (and n (not (zero? (Long/parseLong n)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- position-card
    [fold command pline pos]
    (let [card (fn [re] (re-find (if fold (re-pattern (str "(?i)" re)) (re-pattern re)) command))]
        (cond
            (card "^-LINE\\s+\\d+")
                (let [[_ n f1 f2] (re-find #"^-\S+\s+(\d+)(?:\s+(?:\"([^\"]*)\"|'([^']*)'))?" command)] (assoc pos :file (or f1 f2 (:file pos)) :revert nil :bp (inc pline) :bl (Long/parseLong n)))
            (card "^-\uE000FILE-ENTER\\s+\"")
                (let [[_ f] (re-find #"\"([^\"]*)\"" command)]
                    (assoc pos :file f :bp (inc pline) :bl 1 :stack (conj (:stack pos) (assoc pos :card-line (clojure.core/+ (:bl pos) (clojure.core/- pline (:bp pos)))))))
            (card "^-\uE000FILE-LEAVE")
                (let [outer (peek (:stack pos))]
                    (let [stale (boolean (re-find #"STALE" command))]
                        (assoc outer :file (if stale (:file pos) (:file outer)) :revert (when stale (:file outer)) :bp (inc pline) :bl (inc (:card-line outer)) :stack (pop (:stack pos)))))
            :else pos)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- stuck-card
    [^String card stuck]
    (cond
        (clojure.core/= (char 0xE000) (char-at card 1)) stuck
        (re-find #"^-[Ii][Nn]\d" card) stuck
        (letter? (char-at card 1)) (boolean (#{\+ \-} (char-at card (loop [j 2] (if (name-char? (char-at card j)) (recur (inc j)) j)))))
        :else false))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fold-case
    [^String s]
    (let [sb (StringBuilder.)]
        (loop [i 0 q nil]
            (when
                (< i (count s))
                (let [c (.charAt s i)]
                    (cond
                        q (do (.append sb c) (recur (inc i) (if (clojure.core/= c q) nil q)))
                        (or (clojure.core/= c \') (clojure.core/= c \")) (do (.append sb c) (recur (inc i) c))
                        :else (do (.append sb (if (and (>= (int c) 97) (<= (int c) 122)) (Character/toUpperCase c) c)) (recur (inc i) nil))))))
        (str sb)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- source-names
    [^String s]
    (let [n (count s) name-char? (fn [c] (or (Character/isLetterOrDigit (char c)) (clojure.core/= c \_) (clojure.core/= c \.)))]
        (loop [i 0 goto? false out (transient (if (and (pos? n) (Character/isLetterOrDigit (.charAt s 0))) [(subs s 0 1)] []))]
            (if
                (>= i n)
                (persistent! out)
                (let [c (.charAt s i)]
                    (cond
                        (or (clojure.core/= c \') (clojure.core/= c \")) (let [k (.indexOf s (str c) (int (inc i)))] (recur (if (neg? k) n (inc k)) goto? out))
                        (clojure.core/= c \:) (recur (inc i) true out)
                        (Character/isDigit c) (recur (loop [j (inc i)] (if (and (< j n) (name-char? (.charAt s j))) (recur (inc j)) j)) goto? out)
                        (Character/isLetter c)
                            (let [j (loop [j (inc i)] (if (and (< j n) (name-char? (.charAt s j))) (recur (inc j)) j)) t (subs s i j)]
                                (recur j goto? (if (and goto? (#{"S" "F"} t) (< j n) (#{\( \<} (.charAt s j))) out (conj! out t))))
                        :else (recur (inc i) goto? out)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- stuck-token
    [^String s i prev goto? last?]
    (let [j (skip-blanks s i) blanks? (> j i) c (char-at s j) d (char-at s (inc j)) tok (fn [t e] {:type t :at j :end e :blanks blanks?})]
        (cond
            (and (nil? c) last?) (throw (ex-info "syntax" {::eof true}))
            (nil? c) (assoc (tok :smc j) :at (if blanks? j (:at prev 0)))
            (clojure.core/= c \;) (tok :smc j)
            (and goto? (#{\S \F \s \f} c) (#{\( \< \[} d)) (tok :sf (inc j))
            (letter? c)
                (let [e (loop [e (inc j)] (if (name-char? (char-at s e)) (recur (inc e)) e))]
                    (cond (#{\+ \-} (char-at s e)) (fail! 233 j) (clojure.core/= \( (char-at s e)) (tok :fnc (inc e)) :else (assoc (tok :uop e) :fake true)))
            (digit? c) (tok :con (number-end s j))
            (#{\' \"} c) (let [k (.indexOf s (str c) (int (inc j)))] (if (neg? k) (fail! 232 j) (tok :con (inc k))))
            (clojure.core/= c \() (tok :lpr (inc j))
            (clojure.core/= c \)) (tok :rpr (inc j))
            (#{\[ \<} c) (tok :lbr (inc j))
            (#{\] \>} c) (tok :rbr (inc j))
            (clojure.core/= c \,) (tok :cma (inc j))
            (clojure.core/= c \:) (tok :col (inc j))
            (operator-char? c)
                (let [e (if (and (clojure.core/= c \*) (clojure.core/= d \*)) (clojure.core/+ j 2) (inc j)) d (char-at s e)]
                    (cond
                        (or (nil? d) (blank? d) (#{\; \: \) \] \>} d)) (if blanks? (tok :bop e) (fail! 233 j))
                        (or blanks? (#{:uop :lpr :lbr :cma :fnc} (:type prev))) (tok :uop e)
                        :else (fail! 233 j)))
            :else (fail! 230 j))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- stuck-expression
    [^String s i level last?]
    (loop [i i state 0 levels (list level) prev {:type :smc :at i} again nil fake false]
        (let [
              t (or again (stuck-token s i prev false last?))
              kind (:type t)
              at (:at t)
              level (first levels)
              next-i (:end t)
              fake (or fake (boolean (:fake t)))
              done (fn [k] (if fake (throw (ex-info "syntax" {::crash true})) k))]
            (case state
                0
                    (case kind
                        :uop (recur next-i 1 levels t nil fake)
                        :con (recur next-i 2 levels t nil fake)
                        :lpr (recur next-i 0 (conj levels 4) t nil fake)
                        :fnc (recur next-i 0 (conj levels 5) t nil fake)
                        :lbr (fail! 222 at)
                        :bop (fail! 221 at)
                        (recur i 2 levels prev t fake))
                1
                    (case kind
                        :uop (recur next-i 1 levels t nil fake)
                        :con (recur next-i 2 levels t nil fake)
                        :lpr (recur next-i 0 (conj levels 4) t nil fake)
                        :fnc (recur next-i 0 (conj levels 5) t nil fake)
                        :lbr (fail! 222 at)
                        (fail! 221 at))
                2
                    (case kind
                        (:uop :con :lpr :fnc) (if (:blanks t) (recur i 1 levels prev t fake) (fail! 220 at))
                        :bop (recur next-i 1 levels t nil fake)
                        :lbr (recur next-i 0 (conj levels 3) t nil fake)
                        :cma (if (> (long level) 2) (recur next-i 0 levels t nil fake) (fail! 223 at))
                        :rpr (case (long level) 1 (done next-i) (4 5) (recur next-i 2 (rest levels) t nil fake) (fail! 224 at))
                        :rbr (case (long level) 2 (done next-i) 3 (recur next-i 2 (rest levels) t nil fake) (fail! 225 at))
                        (case (long level) 0 (done (when (clojure.core/= kind :col) next-i)) 1 (fail! 227 at) 2 (fail! 228 at) 3 (fail! 229 at) (fail! 226 at)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- stuck-syntax-error
    [^String s last?]
    (try
        (if
            (letter? (char-at s 0))
            {:error 214}
            (let [body (if (blank? (char-at s 0)) 0 (loop [i 0] (if (and (char-at s i) (not (blank? (char-at s i)))) (recur (inc i)) i))) end (stuck-expression s body 0 last?)]
                (when end
                    (loop [i (long end)]
                        (let [t (stuck-token s i {:type :col :at i} true last?) t (if (clojure.core/= :sf (:type t)) (stuck-token s (:end t) {:type :col :at i} false last?) t)]
                            (case (:type t) :lpr (recur (long (stuck-expression s (:end t) 1 last?))) :lbr (recur (long (stuck-expression s (:end t) 2 last?))) nil))))))
        (catch clojure.lang.ExceptionInfo e (let [d (ex-data e)] (cond (::crash d) {:crash true} (::eof d) nil (::error d) {:error (::error d) :column (inc (or (::at d) 0))} :else (throw e))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- line-end-outside-literals?
    [^String s]
    (loop [i 0 q nil]
        (when
            (< i (count s))
            (let [c (.charAt s i)] (cond (and q (clojure.core/= c q)) (recur (inc i) nil) q (recur (inc i) q) (#{\' \"} c) (recur (inc i) c) (#{\return \newline} c) true :else (recur (inc i) nil))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def parse-statement (insta/parser rules :start :stmt :total true))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- bad-goto!
    [s p]
    (fail! (cond (and (char-at s p) (< (int (char-at s p)) 32) (not= \tab (char-at s p))) 230 (and (operator-char? (char-at s p)) (pos? p) (not (blank? (char-at s (dec p))))) 233 :else 234) p))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- goto-field
    [s i]
    (let [i0 (skip-blanks s i)]
        (when (nil? (char-at s i0)) (fail! 219 i0))
        (loop [i i0 seen #{}]
            (let [i (skip-blanks s i) c (char-at s i)]
                (when c
                    (let [[kind j] (case c (\S \s) [:S (inc i)] (\F \f) [:F (inc i)] [:G i]) o (char-at s j)]
                        (when-not (#{\( \< \[} o) (bad-goto! s j))
                        (let [
                              dup? (or (seen kind) (and (clojure.core/= kind :G) (seq seen)) (seen :G))
                              closer (case o \( \) \< \> \[ \])
                              k (skip-blanks s (inc j))
                              _ (when (clojure.core/= (char-at s k) closer) (fail! (if dup? 218 212) k))
                              k
                                  (try
                                      (expression s k {:closer closer :goto true})
                                      (catch clojure.lang.ExceptionInfo e (if (and (clojure.core/= 226 (::error (ex-data e))) (clojure.core/= closer \))) (fail! 227 (::at (ex-data e))) (throw e))))]
                            (cond
                                (clojure.core/= (char-at s k) closer)
                                    (let [n (inc k) d (char-at s n)] (when dup? (fail! 218 k)) (when-not (or (nil? d) (blank? d) (#{\S \s \F \f \( \<} d)) (bad-goto! s n)) (recur n (conj seen kind)))
                                (#{\: \;} (char-at s k)) (fail! (if (clojure.core/= closer \)) 227 228) k)
                                (nil? (char-at s k)) (fail! (if (clojure.core/= closer \)) 227 228) (loop [b k] (if (and (pos? b) (name-char? (char-at s (dec b)))) (recur (dec b)) b)))
                                :else (fail! 212 k)))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn syntax-error
    [^String s]
    (try
        (when (and (char-at s 0) (not (blank? (char-at s 0))) (not (letter? (char-at s 0))) (not (digit? (char-at s 0)))) (fail! 214 0))
        (let [
              i (if (and (seq s) (not (blank? (char-at s 0)))) (loop [i 0] (if (and (char-at s i) (not (blank? (char-at s i)))) (recur (inc i)) i)) 0)
              i (skip-blanks s i)
              replacement (fn [k] (let [m (expression s (inc k) {:top true})] (when (char-at s m) (goto-field s (inc m)))))]
            (cond
                (nil? (char-at s i)) nil
                (clojure.core/= \: (char-at s i)) (goto-field s (inc i))
                :else
                    (let [[j kind] (parse-operand s i) k (skip-blanks s j) e (char-at s (inc k))]
                        (cond
                            (and (clojure.core/= \= (char-at s k)) (or (nil? e) (blank? e))) (do (when (clojure.core/= kind :literal) (fail! 212 k)) (replacement k))
                            (and (clojure.core/= j k) (char-at s j) (not= \: (char-at s j)))
                                (let [c (char-at s j)]
                                    (cond
                                        (#{\) \] \> \,} c) (stray! c j)
                                        (not (or (operator-char? c) (letter? c) (digit? c) (#{\' \" \( \< \[ \= \;} c))) (fail! 230 j)
                                        (operator-char? c) (fail! 233 (if (#{\+ \-} c) (sign-mark s j) j))
                                        :else (fail! (if (unclosed-quote? s j) 232 220) j)))
                            :else
                                (let [k (if (and (clojure.core/= \? (char-at s k)) (or (nil? e) (blank? e))) (inc k) k) m (expression s k {:top true :stop-at-equals true})]
                                    (cond (nil? (char-at s m)) nil (clojure.core/= \: (char-at s m)) (goto-field s (inc m)) (clojure.core/= \= (char-at s m)) (replacement m) :else (fail! 220 m)))))))
        (catch clojure.lang.ExceptionInfo e (when-let [n (::error (ex-data e))] (if (clojure.core/= :none (::at (ex-data e))) {:error n} {:error n :column (inc (or (::at (ex-data e)) 0))})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private compile-system-labels #{:ABORT :CONTINUE :SCONTINUE :RETURN :FRETURN :NRETURN})
(defn- error-node [info] (list 'ERROR (:line info) (:column info) (:text info)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CODE!
    ([S] (CODE! S ["."]))
    ([S include-path] (CODE! S include-path nil))
    ([S include-path file] (CODE! S include-path file false))
    ([S include-path file record?]
        (let [
              code-text? record?
              record? (and record? (boolean (re-find #"[\r\n]" (str S))))
              S (if (and (not record?) (string/starts-with? (str S) "#!")) (let [k (string/index-of (str S) "\n")] (if k (subs (str S) (inc k)) "")) S)
              [expanded _ cards] (if record? [(str S) #{} []] (preprocess-includes* (str S) include-path #{}))
              raw-stmts (split-source expanded record?)
              _ (reset! <DIAGNOSTICS> (:diagnostics (meta raw-stmts)))
              _ (reset! <SOURCE> {:text expanded :stmts raw-stmts :cards cards})]
            (let [
                  src-lines (string/split (str expanded) #"\n")
                  placed (atom {})
                  skip-line (atom nil)
                  offset
                      (fn
                          [pline ^String stmt]
                          (let [
                                line (get src-lines (dec (long pline)))
                                from (get @placed pline 0)
                                k (when (and line (string? stmt)) (or (string/index-of line stmt from) (first (filter #(string/starts-with? stmt (subs line %)) (range from (count line))))))]
                              (if k (do (swap! placed assoc pline (clojure.core/+ (long k) (count stmt))) k) 0)))]
                (loop [stmts raw-stmts NO 1 CODES {} NOS {} LABELS {} blanks 0 before [] fold false poss [] pos {:file (str file) :bp 1 :bl 1 :stack []} stuck false]
                    (if
                        (empty? stmts)
                        [CODES NOS LABELS {:before before :total blanks :pos poss}]
                        (let [[command pline] (first stmts)]
                            (cond
                                (clojure.core/= command :blank) (recur (rest stmts) NO CODES NOS LABELS (inc blanks) before fold poss pos stuck)
                                (comment? command) (recur (rest stmts) NO CODES NOS LABELS blanks before fold poss pos stuck)
                                (control? command)
                                    (recur
                                        (rest stmts)
                                        NO CODES NOS LABELS blanks before
                                        (do
                                            (case-card fold command)
                                            (when-let [q (and (re-find #"^-INCLUDE\s+['\"]" command) (re-find #"['\"]" command))]
                                                (swap! <DIAGNOSTICS> conj [pline 285 (inc (long (string/index-of command q))) NO]))
                                            fold)
                                        poss
                                        (position-card fold command pline pos)
                                        (let [now (stuck-card command stuck)] (when (and now (not (re-find #"^-(?:\uE000|[Ii][Nn]\d)" command))) (swap! <DIAGNOSTICS> conj [pline 233 2 NO])) now))
                                :else
                                    (if
                                        (clojure.core/= pline @skip-line)
                                        (do
                                            (reset! skip-line nil)
                                            (swap! <SOURCE> update :swallowed (fnil conj #{}) (clojure.core/- (count raw-stmts) (count stmts)))
                                            (recur (rest stmts) NO CODES NOS LABELS blanks before fold poss pos stuck))
                                        (let [
                                              stmt (cond-> command fold fold-case)
                                              _ (note-names! (source-names stmt))
                                              [_ e-kw e-sp e-op] (re-find #"^(END|end)([ \t]+)(\S.*?)[ \t]*$" stmt)
                                              stmt
                                                  (if
                                                      (and e-op (not (and (re-matches #"[A-Za-z0-9][A-Za-z0-9._]*" e-op) (contains? NOS (keyword e-op)))))
                                                      (do (swap! <DIAGNOSTICS> conj [pline 215 (inc (count (str e-kw e-sp))) NO]) e-kw)
                                                      stmt)
                                              jam (when stuck (stuck-syntax-error stmt (empty? (rest stmts))))
                                              ast (cond jam ::stuck (and record? (line-end-outside-literals? stmt)) ::line-end :else (parse-statement stmt))
                                              code
                                                  (cond
                                                      jam {:reason :stuck :line pline :column 1}
                                                      (clojure.core/= ast ::line-end) {:reason :line-end :line pline :column 1}
                                                      :else
                                                          (try
                                                              (emitter ast)
                                                              (catch clojure.lang.ExceptionInfo e
                                                                  (if (:snobol/compile (ex-data e)) {:reason :compile :line pline :column 1 :text (ex-message e)} (throw e)))))
                                              here [(:file pos) (clojure.core/+ (:bl pos) (clojure.core/- pline (:bp pos)))]
                                              pos'' (if (:revert pos) (assoc pos :file (:revert pos) :revert nil) pos)]
                                            (if
                                                (:crash jam)
                                                (do (swap! <SOURCE> assoc :crash pline) [CODES NOS LABELS {:before before :total blanks :pos poss}])
                                                (if
                                                    (and (map? code) (:reason code))
                                                    (let [{n :error col :column}
                                                              (or jam
                                                                  (when-not (clojure.core/= :compile (:reason code)) (syntax-error stmt))
                                                                  (when (clojure.core/= :compile (:reason code)) (let [se (syntax-error stmt)] (when (clojure.core/= 231 (:error se)) se)))
                                                                  {:error (case (:reason code) :compile 231 :line-end 230 220)})]
                                                        (let [
                                                              lbl (when-not (or (#{\+ \.} (first stmt)) (and jam (clojure.core/= 214 n))) (second (re-find #"^([A-Za-z0-9][^ \t;]*)" stmt)))
                                                              lkey (when lbl (keyword lbl))
                                                              dup? (and lkey (not code-text?) (or (contains? NOS lkey) (compile-system-labels lkey)))]
                                                            (when-not
                                                                (#{\+ \.} (first stmt))
                                                                (let [char-at
                                                                          (fn
                                                                              [c]
                                                                              (loop [l (long pline) c (long c)]
                                                                                  (let [len (count (get src-lines (dec l) "")) nxt (get src-lines l)]
                                                                                      (if (and (> c len) nxt (#{\+ \.} (first nxt))) (recur (inc l) (clojure.core/- c len)) [l c]))))
                                                                      [dl dc]
                                                                          (if
                                                                              (and col (not= 214 n) (not dup?))
                                                                              (char-at (clojure.core/+ (long (offset pline command)) (min (long col) (max 1 (count stmt)))))
                                                                              [pline nil])]
                                                                    (swap! <DIAGNOSTICS> conj (if dup? [pline 217 nil NO] [dl n dc NO]))))
                                                            (recur
                                                                (rest stmts)
                                                                (inc NO)
                                                                (assoc CODES NO (error-node code))
                                                                (if (and lkey (not dup?)) (assoc NOS lkey NO) NOS)
                                                                (if (and lkey (not dup?)) (assoc LABELS NO lkey) LABELS)
                                                                blanks
                                                                (conj before blanks)
                                                                fold
                                                                (conj poss here)
                                                                pos'' stuck)))
                                                    (let [
                                                          label (:label code)
                                                          body (:body code)
                                                          goto (:goto code)
                                                          dup? (and (not code-text?) (keyword? label) (or (contains? NOS label) (compile-system-labels label)))
                                                          ferr (if dup? 217 (fold-error body (when (map? goto) goto)))
                                                          _ (when (and ferr (not dup?)) (reset! skip-line pline))
                                                          _
                                                              (when ferr
                                                                  (let [last-line
                                                                            (loop [l (long pline) c (clojure.core/+ (long (offset pline command)) (count stmt))]
                                                                                (let [len (count (get src-lines (dec l) "")) nxt (get src-lines l)]
                                                                                    (if (and (> c len) nxt (#{\+ \.} (first nxt))) (recur (inc l) (clojure.core/- c len)) l)))]
                                                                      (swap! <DIAGNOSTICS> conj [(if (clojure.core/= 217 ferr) pline last-line) ferr nil NO])))
                                                          body (if ferr body (freeze body))
                                                          key (if (and label (not dup?)) label NO)
                                                          code (reduce #(conj %1 %2) [] [body goto])
                                                          nos (if (keyword? key) (assoc NOS key NO) NOS)
                                                          labels (if (keyword? key) (assoc LABELS NO key) LABELS)
                                                          codes (assoc CODES key code)]
                                                        (if
                                                            (#{:END :end} label)
                                                            [codes nos labels {:before (conj before blanks) :total blanks :pos (conj poss here)}]
                                                            (recur (rest stmts) (inc NO) codes nos labels blanks (conj before blanks) fold (conj poss here) pos'' stuck)))))))))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def <CODELINE> (atom ["" 0]))
(def STNO (atom 0))
(def BLANKS (atom 0))
(defn- end-entry [codes] (let [b (first (or (get codes :END) (get codes :end)))] (when (symbol? b) (let [k (keyword (name b))] (when (@<STNO> k) k)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- load-code
    [[codes nos labels disp]]
    (let [start (inc @STNO) base @BLANKS]
        (loop [NO 1]
            (if
                (> NO (count codes))
                (do (swap! BLANKS clojure.core/+ (:total disp 0)) (or (end-entry codes) (if (and (@<LABL> start) (@<CODE> (@<LABL> start))) (@<LABL> start) (when (@<CODE> start) start))))
                (do
                    (swap! STNO inc)
                    (swap! <DISPNO> assoc @STNO (clojure.core/+ @STNO base (get (:before disp) (dec NO) 0)))
                    (swap! <POS> assoc @STNO (get (:pos disp) (dec NO) ["" 0]))
                    (if-let [label (labels NO)]
                        (do (swap! <CODE> #(assoc % label (codes label))) (swap! <LABL> #(assoc % @STNO label)) (swap! <STNO> #(assoc % label @STNO)))
                        (swap! <CODE> #(assoc % @STNO (codes NO))))
                    (recur (inc NO)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CODE
    ([S] (CODE S ["."]))
    ([S include-path] (CODE S include-path nil))
    ([S include-path file] (let [[_ _ _ disp :as ir] (CODE! S include-path file)] (reset! <CODELINE> (or (last (:pos disp)) ["" 0])) (load-code ir))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CODE-block
    [S]
    (let [[f l] (swap! <CODELINE> (fn [[f l]] [f (inc (long l))])) [codes :as ir] (update-in (CODE! S ["."] nil true) [3 :pos] (fn [ps] (mapv #(if (clojure.core/= "" (first %)) [f l] %) ps)))]
        (if-let [[_ n _ bad] (first @<DIAGNOSTICS>)]
            (let [[codes nos labels disp] ir k (dec (long (or bad 1))) before (fn [m] (into {} (filter (fn [[no _]] (<= (long no) k)) m)))]
                (when
                    (pos? k)
                    (load-code
                        [(into {} (for [no (range 1 (inc k)) :let [key (or (labels no) no)]] [key (codes key)]))
                            (into {} (filter (fn [[_ no]] (<= (long no) k)) nos))
                            (before labels)
                            (-> disp (update :before #(vec (take k %))) (update :pos #(vec (take k %))) (assoc :total (get (:before disp) k (:total disp))))]))
                (swap! <CODE> assoc (inc (long @STNO)) compile-error)
                {:error n})
            (let [entry (load-code ir)]
                (if
                    (or (contains? codes :END) (contains? codes :end))
                    entry
                    (do (swap! STNO inc) (swap! <DISPNO> assoc @STNO (clojure.core/+ @STNO @BLANKS)) (swap! <CODE> assoc @STNO block-end) (or entry @STNO)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private no-function '#{ARB BAL FAIL REM SUCCEED BOL EOL NAME NSPAN REAL FULLMATCH GLOBALS INVOKE MATCH RUN SEARCH})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn $$fn
    [N]
    (let [sym (symbol (name N))]
        (if-let [v (find @<VARS> sym)]
            (val v)
            (let [resolved (when (and (re-matches #"[A-Z][A-Z0-9]*" (name sym)) (not (no-function sym))) (when (builtin-names sym) (ns-resolve (find-ns 'SNOBOL4clojure.core) sym)))]
                (if (instance? clojure.lang.Var resolved) (var-get resolved) ($$ N))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn INVOKE
    [op & args]
    (if-let [pf (program-fn op)]
        (apply pf args)
        (case op
            | (do (when (<= 2 (count args)) (pattern-operands! args)) (apply | args))
            $ (do (when (and (<= 2 (count args)) (textless? (first args))) (throw (ex-info (error-message 25) {:snobol/error 25}))) (apply $ args))
            . (do (when (and (<= 2 (count args)) (textless? (first args))) (throw (ex-info (error-message 30) {:snobol/error 30}))) (apply . args))
            LEN (LEN (first args))
            POS (POS (first args))
            RPOS (RPOS (first args))
            TAB (TAB (first args))
            RTAB (RTAB (first args))
            ANY (ANY (first args))
            BREAK (BREAK (first args))
            BREAKX (BREAKX (first args))
            NOTANY (NOTANY (first args))
            SPAN (SPAN (first args))
            FENCE (FENCE (pattern-argument (if (seq args) (first args) ε) 259))
            ARBNO (ARBNO (pattern-argument (first args) 61))
            EQ (EQ (first args) (second args))
            NE (NE (first args) (second args))
            LE (LE (first args) (second args))
            LT (LT (first args) (second args))
            GE (GE (first args) (second args))
            GT (GT (first args) (second args))
            +
                (if
                    (clojure.core/= 1 (count args))
                    (operand (first args) 4 "affirmation operand is not numeric")
                    (let [b (operand (second args) 2 "addition right operand is not numeric") a (operand (first args) 1 "addition left operand is not numeric")]
                        (if
                            (and (integer? a) (integer? b))
                            (exact #(Math/addExact ^long %1 ^long %2) a b 3 "addition caused integer overflow")
                            (finite (clojure.core/+ a b) 261 "addition caused real overflow"))))
            -
                (if
                    (clojure.core/= 1 (count args))
                    (let [a (operand (first args) 10 "negation operand is not numeric")]
                        (if (integer? a) (exact #(Math/subtractExact ^long %1 ^long %2) 0 a 11 "negation caused integer overflow") (clojure.core/- a)))
                    (let [b (operand (second args) 33 "subtraction right operand is not numeric") a (operand (first args) 32 "subtraction left operand is not numeric")]
                        (if
                            (and (integer? a) (integer? b))
                            (exact #(Math/subtractExact ^long %1 ^long %2) a b 34 "subtraction caused integer overflow")
                            (finite (clojure.core/- a b) 264 "subtraction caused real overflow"))))
            *
                (let [b (operand (second args) 27 "multiplication right operand is not numeric") a (operand (first args) 26 "multiplication left operand is not numeric")]
                    (if
                        (and (integer? a) (integer? b))
                        (exact #(Math/multiplyExact ^long %1 ^long %2) a b 28 "multiplication caused integer overflow")
                        (finite (clojure.core/* a b) 263 "multiplication caused real overflow")))
            tilde-op (let [v (first args)] (if (nil? v) ε nil))
            /
                (let [b (operand (second args) 13 "division right operand is not numeric") a (operand (first args) 12 "division left operand is not numeric")]
                    (cond
                        (and (integer? a) (integer? b))
                            (if (or (zero? b) (and (clojure.core/= b -1) (clojure.core/= a Long/MIN_VALUE))) (throw (ex-info "division caused integer overflow" {:snobol/error 14})) (quot a b))
                        (and (zero? (double b)) (zero? (double a))) 0.0
                        (zero? (double b)) (throw (ex-info "division caused real overflow" {:snobol/error 262}))
                        :else (finite (clojure.core// (double a) (double b)) 262 "division caused real overflow")))
            ** (let [[x y] args] (pow-fn x y))
            ? (let [[s p] args s (gtstg s 241)] (when-let [[a b] (SEARCH s (pattern-argument p 240))] (subs s a b)))
            =
                (let [[N r] args]
                    (when-not
                        (nil? r)
                        (cond
                            (instance? ExprVar N) (when-some [t (name-of (.ir ^ExprVar N))] (INVOKE '= t r))
                            (and (list? N) (clojure.core/= (first N) '&)) (do (assign-keyword! (second N) r) r)
                            (and (list? N) (>= (count N) 2))
                                (let [
                                      [container-sym & ks] N
                                      r (concat-value r)
                                      raw-container
                                          (cond
                                              (:container (meta N)) (:container (meta N))
                                              (symbol? container-sym)
                                                  (let [v ($$ container-sym) f (get @<FUNS> (name container-sym))]
                                                      (if (and (:data (meta f)) (or (not (:subscript (meta N))) (and (not (table? v)) (not (array? v))))) f v))
                                              :else container-sym)
                                      container (if (instance? NAME raw-container) ($$ (symbol (str (.n raw-container)))) raw-container)]
                                    (cond
                                        (table? container) (let [v (table-set container (table-subscript ks) r)] (fire-element! container ks r) v)
                                        (array? container) (when-some [v (array-set container (vec ks) r)] (fire-element! container ks r) v)
                                        (fn? container) (when (container (first ks) r) r)
                                        (and (symbol? container-sym) (not (:subscript (meta N)))) (throw (ex-info "undefined function called" {:snobol/error 22}))
                                        :else (throw (ex-info "subscripted operand is not table or array" {:snobol/error 235}))))
                            (contains? #{'OUTPUT 'TERMINAL 'INPUT} N)
                                (let [out-val (concat-value r)]
                                    (swap! <ASSOC-LAST> assoc N out-val)
                                    (when (and (clojure.core/= N 'OUTPUT) (not (zero? (long @&OUTPUT)))) (println (output-text out-val)))
                                    (when (clojure.core/= N 'TERMINAL) (binding [*out* *err*] (println (output-text out-val))))
                                    out-val)
                            (clojure.core/= :output (:type (get @<CHANNELS> N))) (let [out-val (concat-value r)] (write-to-channel! N out-val) (swap! <ASSOC-LAST> assoc N out-val) out-val)
                            :else (let [v (concat-value r)] (snobol-set! N v) (fire-value! N v) v))))
            ?=
                (let [
                      [n p R] args
                      subject (gtstg (if (list? n) (apply INVOKE (or (:container (meta n)) (first n)) (rest n)) (let [v ($$ n)] (when (pos? (long @&TRACE)) (fire-access! n v)) v)) 241)
                      pat (pattern-argument p 240)]
                    (when-let [[start end] (SEARCH subject pat)]
                        (let [repl (sstr (EVAL! R)) result (str (subs subject 0 start) repl (subs subject end))]
                            (when (> (count result) (long @&MAXLNGTH)) (throw (ex-info "string length exceeds value of maxlngth keyword" {:snobol/error 205})))
                            (INVOKE '= n result)
                            result)))
            DEFINE
                (let [
                      [proto entry-arg] args
                      lp (.indexOf ^String proto "(")
                      rp (.indexOf ^String proto ")")
                      fname (if (>= lp 0) (.substring ^String proto 0 lp) proto)
                      inner (if (and (>= lp 0) (>= rp 0)) (.substring ^String proto (inc lp) rp) "")
                      after-rp (if (>= rp 0) (.substring ^String proto (inc rp)) "")
                      params (if (clojure.string/blank? inner) [] (mapv clojure.string/trim (clojure.string/split inner #",")))
                      locals
                          (if
                              (clojure.string/blank? after-rp)
                              []
                              (let [s (-> after-rp clojure.string/trim (clojure.string/replace #"^," ""))]
                                  (filterv (complement clojure.string/blank?) (mapv clojure.string/trim (clojure.string/split s #",")))))
                      f-sym (symbol fname)
                      entry
                          (if
                              (and (>= (count args) 2) entry-arg (not (clojure.core/= "" (sstr entry-arg))))
                              (if (symbol? entry-arg) (keyword (str (name entry-arg))) (keyword (str entry-arg)))
                              (keyword fname))
                      all-saved (into params locals)]
                    (letfn
                        [(the-fn
                             [& call-args]
                             (let [
                                   saved (zipmap all-saved (map #(saved-value (symbol %)) all-saved))
                                   f-saved (saved-value f-sym)
                                   stno-saved @&STNO
                                   file-saved (if-some [[f _] *eval-block*] f @&FILE)
                                   line-saved (if-some [[_ l] *eval-block*] l @&LINE)]
                                 (snobol-set! f-sym ε)
                                 (doseq [i (range (count params))] (snobol-set! (symbol (params i)) (nth call-args i ε)))
                                 (doseq [l locals] (snobol-set! (symbol l) ε))
                                 (fire-call! fname (map #(reference (symbol %)) params))
                                 (fire-keyword! "FNCLEVEL" (swap! &FNCLEVEL inc))
                                 (let [
                                       run-fn (ns-resolve 'SNOBOL4clojure.core 'RUN)
                                       d (inc (long *fp*))
                                       outcome
                                           (try
                                               (when run-fn (binding [*fp* d *flprt* d *eval-block* nil] ((var-get run-fn) entry)))
                                               :return
                                               (catch clojure.lang.ExceptionInfo e
                                                   (let [sig (:snobol/signal (ex-data e))]
                                                       (if
                                                           (#{:return :freturn :nreturn} sig)
                                                           sig
                                                           (throw (if (contains? (ex-data e) :fnclevel) e (ex-info (ex-message e) (assoc (ex-data e) :fnclevel @&FNCLEVEL) e))))))
                                               (finally (fire-keyword! "FNCLEVEL" (swap! &FNCLEVEL dec))))]
                                     (let [
                                           result ($$ f-sym)
                                           _ (reset! &RTNTYPE (case outcome :freturn "FRETURN" :nreturn "NRETURN" "RETURN"))
                                           _ (fire-return! fname result outcome stno-saved)
                                           result ($$ f-sym)]
                                         (doseq [[k v] saved] (restore-var! (symbol k) v))
                                         (restore-var! f-sym f-saved)
                                         (reset! &LASTNO @&STNO)
                                         (reset! &LASTLINE @&LINE)
                                         (reset! &LASTFILE @&FILE)
                                         (reset! &STNO stno-saved)
                                         (reset! &FILE file-saved)
                                         (reset! &LINE line-saved)
                                         (reset! <STMT-LINE> line-saved)
                                         (reset! &RTNTYPE (case outcome :freturn "FRETURN" :nreturn "NRETURN" "RETURN"))
                                         (case outcome
                                             :return result
                                             :freturn nil
                                             :nreturn
                                                 (cond
                                                     (instance? NAME result) (->NameResult (.n ^NAME result))
                                                     (and (string? result) (seq result)) (->NameResult (symbol result))
                                                     (number? result) (->NameResult (symbol (sstr result)))
                                                     :else (throw (ex-info "function result in NRETURN is not name" {:snobol/error 243})))
                                             result)))))]
                        (swap! <FUNS> assoc fname the-fn)
                        (note-names! (concat [fname] params locals))
                        (swap! <FDEFS> assoc fname {:params params :locals locals})
                        ε))
            OPSYN
                (let [
                      [new-arg old-arg n-arg] args
                      new-name (name-string new-arg)
                      old-name (name-string old-arg)
                      old-sym (symbol (name-string old-arg))
                      fail! (fn [n text] (throw (ex-info text {:snobol/error n})))
                      n
                          (when
                              (and n-arg (not= n-arg ε))
                              (let [v (num n-arg)]
                                  (cond
                                      (or (not (number? v)) (and (double? v) (Double/isNaN v))) (fail! 152 "OPSYN third argument is not integer")
                                      (or (neg? v) (> v 16777216)) (fail! 153 "OPSYN third argument is negative or too large")
                                      :else (long v))))
                      _
                          (cond
                              (clojure.core/= "" new-name) (fail! 155 "OPSYN first arg is not natural variable name")
                              (clojure.core/= "" old-name) (fail! 154 "OPSYN second arg is not natural variable name")
                              (or (nil? n) (zero? n)) (when (contains? system-functions new-name) (fail! 248 "attempted redefinition of system function"))
                              (and (> n 2) (not (#{"&" "@" "#" "%" "~"} new-name))) (fail! 156 "OPSYN first arg is not correct operator name")
                              :else nil)
                      n (when n (min n 2))
                      old-fn
                          (or
                              (get @<FUNS> old-name)
                              (if (and n (pos? n)) (if (clojure.core/= n 1) (fn [a] (INVOKE old-sym a)) (fn [a b] (INVOKE old-sym a b))) (fn [& a] (apply INVOKE old-sym (vec a)))))]
                    (if (and n (pos? n)) (swap! <FUNS> assoc (opsyn-key (if (and (clojure.core/= 2 n) (#{"^" "!"} new-name)) "**" new-name) n) old-fn) (swap! <FUNS> assoc new-name old-fn))
                    ε)
            LOAD
                (let [p (sstr (nth args 0 "")) lp (clojure.string/index-of p "(") e #(throw (ex-info (error-message %) {:snobol/error %}))]
                    (cond (clojure.core/= "" p) (e 138) (nil? lp) (e 139) (zero? (long lp)) (e 140) (nil? (clojure.string/index-of p ")" lp)) (e 141) :else (e 142)))
            UNLOAD
                (let [nm (name-string (first args))]
                    (cond
                        (clojure.core/= "" nm) (throw (ex-info "UNLOAD argument is not natural variable name" {:snobol/error 201}))
                        (contains? system-functions nm) (throw (ex-info "attempted redefinition of system function" {:snobol/error 248}))
                        :else (do (swap! <FUNS> dissoc nm) (swap! <FDEFS> dissoc nm) ε)))
            APPLY (apply APPLY (first args) (rest args))
            REPLACE (let [[s1 s2 s3] args] (REPLACE s1 s2 s3))
            TABLE (apply-builtin TABLE args)
            ARRAY (apply-builtin ARRAY args)
            REMDR (REMDR (first args) (second args))
            TRACE (let [[nm ty lbl f] args] (trace! nm (or ty "VALUE") lbl f) ε)
            STOPTR (let [[nm ty] args] (stoptr! nm (or ty "VALUE")) ε)
            COLLECT
                (do (System/gc) (let [free (clojure.core/- (quot (.maxMemory (Runtime/getRuntime)) 8) (storage-words)) want (num (first args))] (when-not (and (integer? want) (> want free)) free)))
            CLEAR (clear-vars! (first args))
            BACKSPACE (apply-builtin BACKSPACE args)
            EJECT
                (let [u (first args)]
                    (if
                        (or (nil? u) (clojure.core/= ε u))
                        (do (print "\f") (flush) ε)
                        (let [ch (get @<CHANNELS> (channel-key u))]
                            (cond
                                (nil? ch) (throw (ex-info "EJECT file does not exist" {:snobol/error 93}))
                                (clojure.core/= :output (:type ch)) (let [w ^java.io.PrintWriter (:writer ch)] (.println w "\f") (.flush w) ε)
                                :else ε))))
            SETEXIT (apply-builtin SETEXIT args)
            HOST (apply HOST args)
            DUMP (let [n (num (first args))] (when (and (integer? n) (<= 1 n 3)) (print (dump-text n))) ε)
            ATAN (ATAN (first args))
            CHOP (CHOP (first args))
            COS (COS (first args))
            EXP (EXP (first args))
            LN (LN (first args))
            SIN (SIN (first args))
            SQRT (SQRT (first args))
            TAN (TAN (first args))
            INTEGER (integer-predicate (first args))
            SIZE (SIZE (first args))
            TRIM (TRIM (first args))
            DUPL (DUPL (first args) (second args))
            REVERSE (REVERSE (first args))
            ITEM (apply ITEM args)
            PROTOTYPE (apply-builtin PROTOTYPE args)
            CONVERT
                (let [[x t] args]
                    (if
                        (clojure.core/= "EXPRESSION" (clojure.string/upper-case (str t)))
                        (cond (deferred? x) x (string? x) (when-not (ends-in-mark? x) (when-some [ir (compile-expression x)] (expression-of ir))) (number? x) (expression-of x) :else (CONVERT x t))
                        (CONVERT x t)))
            COPY (COPY (first args))
            FIELD (FIELD (first args) (second args))
            SORT (apply-builtin SORT args)
            RSORT (apply-builtin RSORT args)
            DATA (DATA (first args))
            DATATYPE (DATATYPE (first args))
            CHAR (CHAR (first args))
            ORD (when (seq args) (ORD (first args)))
            SUBSTR (SUBSTR (first args) (second args) (nth args 2 ""))
            DATE (apply-builtin DATE args)
            TIME (TIME)
            LPAD (LPAD (nth args 0 "") (nth args 1 "") (nth args 2 ""))
            RPAD (RPAD (nth args 0 "") (nth args 1 "") (nth args 2 ""))
            LEQ (LEQ (first args) (second args))
            LNE (LNE (first args) (second args))
            LLE (LLE (first args) (second args))
            LLT (LLT (first args) (second args))
            LGE (LGE (first args) (second args))
            LGT (LGT (first args) (second args))
            ARG (arg-or-local args :params 63 "ARG first argument is not program function name" 62 "ARG second argument is not integer")
            LOCAL (arg-or-local args :locals 135 "LOCAL first arg is not a program function name" 134 "LOCAL second argument is not integer")
            INPUT (let [[name-arg unit filename] args] (open-input-channel! (channel-variable name-arg) unit filename))
            OUTPUT (let [[name-arg unit filename] args] (open-output-channel! (channel-variable name-arg) unit filename))
            ENDFILE
                (let [u (first args)]
                    (cond
                        (or (nil? u) (clojure.core/= "" u)) (throw (ex-info (error-message 97) {:snobol/error 97}))
                        (nil? (get @<CHANNELS> (channel-key u))) (throw (ex-info (error-message 98) {:snobol/error 98}))
                        :else (close-channel! (channel-key u))))
            DETACH (let [v (first args) sym (cond (symbol? v) v (instance? NAME v) (symbol (str (.n ^NAME v))) :else (symbol (str v)))] (do (swap! <CHANNELS> dissoc sym) ε))
            REWIND (rewind-channel! (first args))
            EXIT (apply-builtin EXIT args)
            CODE
                (let [src (str (first args)) compile-f (ns-resolve 'SNOBOL4clojure.core 'CODE-block)]
                    (when-not compile-f (throw (ex-info "CODE: compiler not loaded" {:snobol/signal :error})))
                    (let [entry (try (compile-f src) (catch Exception e (throw (ex-info (.getMessage e) {:snobol/signal :error}))))]
                        (if
                            (map? entry)
                            (if
                                (and (pos? (long @&ERRLIMIT)) @<SETEXIT>)
                                (do (compiled! false) (throw (ex-info (error-message (:error entry)) {:snobol/error (:error entry)})))
                                (compile-failed! (:error entry) (error-message (:error entry))))
                            (when (some? entry) (compiled! true) (->CodeBlock entry)))))
            (let [raw-f (if (symbol? op) ($$fn op) op) f (if (instance? NAME raw-f) ($$ (symbol (str (.n raw-f)))) raw-f) f (if (fn? f) f (get @<FUNS> (str op) f))]
                (cond
                    (table? f) (table-get f (table-subscript args))
                    (array? f) (array-get f (vec args))
                    (fn? f) (apply-builtin f args)
                    :else (throw (ex-info "undefined function called" {:snobol/error 22 :function op})))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- trace-fn
    [entry]
    (when-some [f (:fn entry)]
        (let [n (if (instance? NAME f) (str (.n ^NAME f)) (str f))]
            (or (get @<FUNS> n) (when (contains? system-functions n) (let [invoke (ns-resolve (the-ns 'SNOBOL4clojure.core) 'INVOKE)] (fn [& args] (apply @invoke (symbol n) args))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fire-keyword!
    [kw new-val]
    (when
        (pos? @&TRACE)
        (when-let [entry (@registry (reg-key kw :keyword))]
            (if-let [f (trace-fn entry)] (call-trace-fn! f kw entry) (do (swap! &TRACE dec) (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " &" kw " = " (trace-val new-val)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- matching
    [f]
    (binding [*matching* true]
        (try
            (f)
            (catch clojure.lang.ExceptionInfo e
                (let [{n :snobol/error abort :snobol/abort :as data} (ex-data e)]
                    (if
                        (and n (not abort) (zero? (long @&ERRLIMIT)) (not (contains? data :snobol/to)) (not (contains? data :snobol/label)) (not (<= 43 (long n) 58)))
                        (do (reset! &ERRTYPE n) (fire-keyword! "ERRTYPE" n) (reset! &ERRTEXT (ex-message e)) nil)
                        (throw e)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn MATCH
    [S P]
    (binding [*pending-cond* (volatile! [])]
        (matching #(let [result (engine (seq S) 0 (list 'SEQ (POS 0) P) 0 S) result (when-not (clojure.core/= :abort result) result)] (when (and result (commit-pending!)) result)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn FULLMATCH
    [S P]
    (binding [*pending-cond* (volatile! [])]
        (matching #(let [result (engine (seq S) 0 (list 'SEQ (POS 0) P (RPOS 0)) 0 S) result (when-not (clojure.core/= :abort result) result)] (when (and result (commit-pending!)) result)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- primitive
    [func default missing cvt condition]
    (list 'defn func (list [] missing) (list ['x] (list 'if (condition (cvt 'x) default) ε)) (list ['x 'y '& '_] (list 'if (condition (cvt 'x) (cvt 'y)) ε))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(eval (primitive 'IDENT ε ε identity #(list 'SNOBOL4clojure.core/snobol-ident? %1 %2)))
(eval (primitive 'DIFFER ε nil identity #(list 'not (list 'SNOBOL4clojure.core/snobol-ident? %1 %2))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn EVAL
    [X]
    (cond
        (and (deferred? X) (not (:pattern (meta X)))) (evaluated-or-fail (second X))
        (string? X)
            (if
                (ends-in-mark? X)
                (throw (ex-info "eval argument is not expression" {:snobol/error 103}))
                (evaluated-or-fail #(when-some [ir (binding [*raise-compile-errors* true] (compile-or-fail X))] (binding [*eval-block* @<CODELINE>] (EVAL! ir)))))
        (number? X) X
        (and (instance? NAME X) (symbol? (.n ^NAME X))) (EVAL (name (.n ^NAME X)))
        (or (textless? X) (and (list? X) (clojure.core/= "PATTERN" (DATATYPE X)))) (throw (ex-info "eval argument is not expression" {:snobol/error 103}))
        true (EVAL! X)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private ^:const ARETURN (int Opcodes/ARETURN))
(def ^:private ^:const ALOAD-0 (int Opcodes/ALOAD))
(def ^:private ^:const INVOKEVIRTUAL (int Opcodes/INVOKEVIRTUAL))
(def ^:private ^:const INVOKESPECIAL (int Opcodes/INVOKESPECIAL))
(def ^:private ^:const DUP (int Opcodes/DUP))
(def ^:private JC-CLASS "SNOBOL4clojure/jvm_codegen")
(def ^:private STR "Ljava/lang/String;")
(def ^:private THROW-SIG (str "(" STR ")Lclojure/lang/ExceptionInfo;"))
(declare EVAL_FN)
(defn ^clojure.lang.ExceptionInfo make-signal [^String sig-name] (ex-info sig-name {:snobol/signal (keyword sig-name)}))
(def ^:private class-cache (atom {}))
(defn clear-class-cache! [] (reset! class-cache {}) nil)
(def <OPSYN> (atom {}))
(def <LASTOUT> (atom nil))
(defn reset-keywords! [] (doseq [[v dflt] keyword-defaults] (reset! @v dflt)))
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
        (reset! <ST-DEAD> nil)
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
(defn- assign-pcs
    [[codes nos labels]]
    (let [all-int (sort (into (set (filter integer? (keys codes))) (vals nos))) kw-slots (filter keyword? (keys codes))]
        (loop [slots all-int pc 0 m {}]
            (if
                (empty? slots)
                (reduce (fn [m kw] (let [int-no (get nos kw) kw-pc (if int-no (get m int-no) pc)] (assoc m kw kw-pc))) m kw-slots)
                (recur (rest slots) (inc pc) (assoc m (first slots) pc))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private special-names #{"RETURN" "FRETURN" "NRETURN" "END"})
(defn- special? [tgt] (and tgt (not (integer? tgt)) (contains? special-names (str/upper-case (name tgt)))))
(defn- signal-for [tgt] (case (str/upper-case (name tgt)) "END" :end "RETURN" :return "FRETURN" :freturn "NRETURN" :nreturn))
(def ^:const OP-HALT 0)
(def ^:const OP-SIGNAL 6)
(def ^:const OP-JUMP 5)
(def ^:const OP-EXEC 1)
(def ^:const OP-EXEC-SF 4)
(def ^:const OP-EXEC-S 2)
(def ^:const OP-EXEC-F 3)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn compile-ir
    [[codes nos labels :as ir]]
    (let [
          pc-map (assign-pcs ir)
          all-int (sort (into (set (filter integer? (keys codes))) (vals nos)))
          halt-pc (count all-int)
          resolve-tgt (fn [tgt] (cond (nil? tgt) nil (special? tgt) :signal (integer? tgt) (get pc-map tgt) (keyword? tgt) (get pc-map tgt) :else (get pc-map tgt)))
          make-instr
              (fn
                  [key]
                  (let [
                        lbl (get labels key)
                        code (if lbl (get codes lbl) (get codes key))
                        [raw-body raw-goto] (if code code [nil nil])
                        body raw-body
                        goto raw-goto
                        g-tgt (:G goto)
                        s-tgt (:S goto)
                        f-tgt (:F goto)
                        fall (clojure.core/+ (get pc-map key) 1)
                        s-sig (when (special? s-tgt) (signal-for s-tgt))
                        f-sig (when (special? f-tgt) (signal-for f-tgt))
                        g-sig (when (special? g-tgt) (signal-for g-tgt))]
                      (cond
                          (and (nil? body) (nil? g-tgt) (nil? s-tgt) (nil? f-tgt) (or lbl (nil? (get codes key)))) {:op OP-HALT :body nil :s nil :f nil}
                          (and (nil? body) g-tgt) (if g-sig {:op OP-SIGNAL :body g-sig :s nil :f nil} {:op OP-JUMP :body nil :s (resolve-tgt g-tgt) :f nil})
                          (and body g-tgt) (if g-sig {:op OP-EXEC :body body :s fall :f fall} {:op OP-JUMP :body body :s (resolve-tgt g-tgt) :f nil})
                          (and body s-tgt f-tgt) {:op OP-EXEC-SF :body body :s (if s-sig :signal (resolve-tgt s-tgt)) :f (if f-sig :signal (resolve-tgt f-tgt)) :s-sig s-sig :f-sig f-sig}
                          (and body s-tgt) {:op OP-EXEC-S :body body :s (if s-sig :signal (resolve-tgt s-tgt)) :f fall :s-sig s-sig}
                          (and body f-tgt) {:op OP-EXEC-F :body body :s fall :f (if f-sig :signal (resolve-tgt f-tgt)) :f-sig f-sig}
                          body {:op OP-EXEC :body body :s fall :f fall}
                          :else {:op OP-EXEC :body nil :s fall :f fall})))]
        (let [instrs (mapv make-instr all-int)] (conj instrs {:op OP-HALT :body nil :s nil :f nil}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn compile-src [src] (compile-ir (CODE! src)))
(defn gen-class-name [src] (str "SNOBOL4clojure/SnobolProg" (format "%08x" (Math/abs (hash src)))))
(def ^:private ^:const V1-8 (int 52))
(def ^:private ^:const ACC-PUBLIC (int Opcodes/ACC_PUBLIC))
(def ^:private ^:const ACC-SUPER (int Opcodes/ACC_SUPER))
(def ^:private ^:const ACC-STATIC (int Opcodes/ACC_STATIC))
(def ^:private IFN-DESC "Lclojure/lang/IFn;")
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private JC-IFN-FIELD-SIGS
    {
     "$$_FN" "$$_FN"
     "SET_FN" "SET_FN"
     "PRINT_FN" "PRINT_FN"
     "ADD_FN" "ADD_FN"
     "SUB_FN" "SUB_FN"
     "MUL_FN" "MUL_FN"
     "DIV_FN" "DIV_FN"
     "LT_FN" "LT_FN"
     "GT_FN" "GT_FN"
     "LE_FN" "LE_FN"
     "GE_FN" "GE_FN"
     "EQ_FN" "EQ_FN"
     "NEQ_FN" "NEQ_FN"
     "SIGNAL_FN" "SIGNAL_FN"
     "CONCAT_FN" "CONCAT_FN"
     "SIZE_FN" "SIZE_FN"
     "STR_FN" "STR_FN"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private OBJ "Ljava/lang/Object;")
(def ^:private ^:const INVOKESTATIC (int Opcodes/INVOKESTATIC))
(def ^:private RT-CLASS "clojure/lang/RT")
(def ^:private ^:const PUTSTATIC (int Opcodes/PUTSTATIC))
(def ^:private ^:const RETURN (int Opcodes/RETURN))
(defn- sym-or-lit? [x] (or (symbol? x) (integer? x) (float? x) (string? x)))
(def ^:private ^:const GETSTATIC (int Opcodes/GETSTATIC))
(def ^:private ^:const INVOKEINTERFACE (int Opcodes/INVOKEINTERFACE))
(def ^:private IFN-CLASS "clojure/lang/IFn")
(def ^:private IFN-INVOKE-SIG (str "(" OBJ ")" OBJ))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- emit-val!
    [^MethodVisitor mv cls-name x]
    (cond
        (symbol? x) (do (.visitFieldInsn mv GETSTATIC cls-name "$$_FN" IFN-DESC) (.visitLdcInsn mv (str x)) (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true))
        (integer? x) (do (.visitLdcInsn mv (long x)) (.visitMethodInsn mv INVOKESTATIC "java/lang/Long" "valueOf" "(J)Ljava/lang/Long;" false))
        (float? x) (do (.visitLdcInsn mv (double x)) (.visitMethodInsn mv INVOKESTATIC "java/lang/Double" "valueOf" "(D)Ljava/lang/Double;" false))
        (string? x) (.visitLdcInsn mv ^String x)
        :else (throw (ex-info "emit-val!: not a sym-or-lit" {:x x}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private ^:const ASTORE (int Opcodes/ASTORE))
(def ^:private ^:const ALOAD (int Opcodes/ALOAD))
(def ^:private IFN-INVOKE-2-SIG (str "(" OBJ OBJ ")" OBJ))
(def ^:private ARITH-OP->FN {'+ "ADD_FN" '- "SUB_FN" '* "MUL_FN" '/ "DIV_FN"})
(def ^:private CMP-OP->FN {'LT "LT_FN" 'GT "GT_FN" 'LE "LE_FN" 'GE "GE_FN" 'EQ "EQ_FN" 'NE "NEQ_FN"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- inline-emit!
    [^MethodVisitor mv cls-name body]
    (cond
        (and (list? body) (clojure.core/= (first body) '=) (clojure.core/= (second body) 'OUTPUT) (sym-or-lit? (nth body 2)))
            (let [val (nth body 2)]
                (.visitFieldInsn mv GETSTATIC cls-name "PRINT_FN" IFN-DESC)
                (emit-val! mv cls-name val)
                (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
                true)
        (and (list? body) (clojure.core/= (first body) '=) (symbol? (second body)) (not (clojure.core/= (second body) 'OUTPUT)) (sym-or-lit? (nth body 2)))
            (let [sym (second body) val (nth body 2)]
                (emit-val! mv cls-name val)
                (.visitVarInsn mv ASTORE (int 0))
                (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
                (.visitLdcInsn mv (str sym))
                (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern" "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
                (.visitVarInsn mv ALOAD (int 0))
                (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                true)
        (and
            (list? body)
            (clojure.core/= (first body) '=)
            (symbol? (second body))
            (list? (nth body 2))
            (ARITH-OP->FN (first (nth body 2)))
            (sym-or-lit? (second (nth body 2)))
            (sym-or-lit? (nth (nth body 2) 2)))
            (let [sym (second body) [op a b] (nth body 2) fn-fld (ARITH-OP->FN op)]
                (.visitFieldInsn mv GETSTATIC cls-name fn-fld IFN-DESC)
                (emit-val! mv cls-name a)
                (emit-val! mv cls-name b)
                (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                (.visitVarInsn mv ASTORE (int 0))
                (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
                (.visitLdcInsn mv (str sym))
                (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern" "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
                (.visitVarInsn mv ALOAD (int 0))
                (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                true)
        (and (list? body) (CMP-OP->FN (first body)) (sym-or-lit? (second body)) (sym-or-lit? (nth body 2)))
            (let [[op a b] body fn-fld (CMP-OP->FN op)]
                (.visitFieldInsn mv GETSTATIC cls-name fn-fld IFN-DESC)
                (emit-val! mv cls-name a)
                (emit-val! mv cls-name b)
                (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                true)
        (and (list? body) (clojure.core/= (first body) '=) (symbol? (second body)) (not (clojure.core/= (second body) 'OUTPUT)) (vector? (nth body 2)) (every? sym-or-lit? (nth body 2)))
            (let [sym (second body) elems (nth body 2) n (count elems)]
                (if
                    (clojure.core/= n 2)
                    (let [[e1 e2] elems]
                        (.visitFieldInsn mv GETSTATIC cls-name "CONCAT_FN" IFN-DESC)
                        (emit-val! mv cls-name e1)
                        (emit-val! mv cls-name e2)
                        (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                        (.visitVarInsn mv ASTORE (int 0))
                        (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
                        (.visitLdcInsn mv (str sym))
                        (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern" "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
                        (.visitVarInsn mv ALOAD (int 0))
                        (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                        true)
                    false))
        (and (list? body) (clojure.core/= (first body) '=) (clojure.core/= (second body) 'OUTPUT) (vector? (nth body 2)) (every? sym-or-lit? (nth body 2)))
            (let [elems (nth body 2) n (count elems)]
                (if
                    (clojure.core/= n 2)
                    (let [[e1 e2] elems]
                        (.visitFieldInsn mv GETSTATIC cls-name "CONCAT_FN" IFN-DESC)
                        (emit-val! mv cls-name e1)
                        (emit-val! mv cls-name e2)
                        (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                        (.visitVarInsn mv ASTORE (int 0))
                        (.visitFieldInsn mv GETSTATIC cls-name "PRINT_FN" IFN-DESC)
                        (.visitVarInsn mv ALOAD (int 0))
                        (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
                        true)
                    false))
        (and
            (list? body)
            (clojure.core/= (first body) '=)
            (symbol? (second body))
            (not (clojure.core/= (second body) 'OUTPUT))
            (list? (nth body 2))
            (clojure.core/= (first (nth body 2)) 'SIZE)
            (sym-or-lit? (second (nth body 2))))
            (let [sym (second body) arg (second (nth body 2))]
                (.visitFieldInsn mv GETSTATIC cls-name "SIZE_FN" IFN-DESC)
                (emit-val! mv cls-name arg)
                (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
                (.visitVarInsn mv ASTORE (int 0))
                (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
                (.visitLdcInsn mv (str sym))
                (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern" "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
                (.visitVarInsn mv ALOAD (int 0))
                (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                true)
        (and (list? body) (clojure.core/= (first body) '=) (clojure.core/= (second body) 'OUTPUT) (list? (nth body 2)) (clojure.core/= (first (nth body 2)) 'SIZE) (sym-or-lit? (second (nth body 2))))
            (let [arg (second (nth body 2))]
                (.visitFieldInsn mv GETSTATIC cls-name "PRINT_FN" IFN-DESC)
                (.visitFieldInsn mv GETSTATIC cls-name "SIZE_FN" IFN-DESC)
                (emit-val! mv cls-name arg)
                (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
                (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
                true)
        :else false))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private ^:const ACONST-NULL (int Opcodes/ACONST_NULL))
(def ^:private ^:const POP (int Opcodes/POP))
(def ^:private ^:const ATHROW (int Opcodes/ATHROW))
(def ^:private ^:const GOTO (int Opcodes/GOTO))
(def ^:private ^:const IFNULL (int Opcodes/IFNULL))
(def ^:private ^:const IFNONNULL (int Opcodes/IFNONNULL))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn compile-to-class
    [src]
    (let [
          bc-vec (compile-src src)
          n-instrs (count bc-vec)
          cls-name (gen-class-name src)
          cw (ClassWriter. (int ClassWriter/COMPUTE_FRAMES))
          bodies (into [] (keep-indexed (fn [i {:keys [body]}] (when body [i body])) bc-vec))
          body-field (fn [i] (str "body_" i))]
        (.visit cw V1-8 (int (clojure.core/+ ACC-PUBLIC ACC-SUPER)) cls-name nil "java/lang/Object" nil)
        (.visitField cw (int (clojure.core/+ ACC-PUBLIC ACC-STATIC)) "EVAL_FN" IFN-DESC nil nil)
        (doseq [fld-name (keys JC-IFN-FIELD-SIGS)] (.visitField cw (int (clojure.core/+ ACC-PUBLIC ACC-STATIC)) fld-name IFN-DESC nil nil))
        (doseq [[i _] bodies] (.visitField cw (int (clojure.core/+ ACC-PUBLIC ACC-STATIC)) (body-field i) OBJ nil nil))
        (let [ci (.visitMethod cw (int (clojure.core/+ ACC-PUBLIC ACC-STATIC)) "<clinit>" "()V" nil nil)]
            (.visitCode ci)
            (doseq [[i body] bodies]
                (.visitLdcInsn ci (pr-str body))
                (.visitMethodInsn ci INVOKESTATIC RT-CLASS "readString" (str "(" STR ")" OBJ) false)
                (.visitFieldInsn ci PUTSTATIC cls-name (body-field i) OBJ))
            (.visitInsn ci RETURN)
            (.visitMaxs ci (int 2) (int 0))
            (.visitEnd ci))
        (let [mv (.visitMethod cw (int (clojure.core/+ ACC-PUBLIC ACC-STATIC)) "run" "()V" nil nil) lbls (mapv (fn [_] (Label.)) bc-vec) end-lbl (Label.)]
            (.visitCode mv)
            (doseq [i (range n-instrs)]
                (.visitLabel mv (lbls i))
                (let [
                      {:keys [op body s f s-sig f-sig]} (bc-vec i)
                      op (int op)
                      eval-body!
                          (fn
                              []
                              (if body
                                  (or
                                      (inline-emit! mv cls-name body)
                                      (do
                                          (.visitFieldInsn mv GETSTATIC cls-name "EVAL_FN" IFN-DESC)
                                          (.visitFieldInsn mv GETSTATIC cls-name (body-field i) OBJ)
                                          (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)))
                                  (.visitInsn mv ACONST-NULL)))
                      emit-signal!
                          (fn
                              [sig]
                              (.visitFieldInsn mv GETSTATIC cls-name "SIGNAL_FN" IFN-DESC)
                              (.visitLdcInsn mv (name sig))
                              (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
                              (.visitInsn mv POP)
                              (.visitInsn mv ACONST-NULL)
                              (.visitInsn mv ATHROW))]
                    (case op
                        0 (.visitInsn mv RETURN)
                        1 (do (when body (eval-body!) (.visitInsn mv POP)) (when s (.visitJumpInsn mv GOTO (lbls s))))
                        2 (do (eval-body!) (let [fail-lbl (lbls f)] (.visitJumpInsn mv IFNULL fail-lbl) (if s-sig (emit-signal! s-sig) (.visitJumpInsn mv GOTO (lbls s)))))
                        3
                            (do
                                (eval-body!)
                                (let [ok-lbl (if s (lbls s) end-lbl)] (.visitJumpInsn mv IFNONNULL ok-lbl) (if f-sig (emit-signal! f-sig) (.visitJumpInsn mv GOTO (lbls f))) (.visitLabel mv ok-lbl)))
                        4
                            (do
                                (eval-body!)
                                (let [fail-lbl (Label.)]
                                    (.visitJumpInsn mv IFNULL fail-lbl)
                                    (if s-sig (emit-signal! s-sig) (.visitJumpInsn mv GOTO (lbls s)))
                                    (.visitLabel mv fail-lbl)
                                    (if f-sig (emit-signal! f-sig) (.visitJumpInsn mv GOTO (lbls f)))))
                        5 (do (when body (eval-body!) (.visitInsn mv POP)) (.visitJumpInsn mv GOTO (lbls (int s))))
                        6 (emit-signal! body)
                        (.visitInsn mv RETURN))))
            (.visitLabel mv end-lbl)
            (.visitInsn mv RETURN)
            (.visitMaxs mv (int 4) (int 2))
            (.visitEnd mv))
        (.visitEnd cw)
        (.toByteArray cw)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare $$_FN SET_FN PRINT_FN ADD_FN SUB_FN MUL_FN DIV_FN LT_FN GT_FN LE_FN GE_FN EQ_FN NEQ_FN SIGNAL_FN CONCAT_FN SIZE_FN STR_FN)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn load-program
    [src]
    (or
        (get @class-cache src)
        (let [
              bc (compile-to-class src)
              cls-name (.replace (gen-class-name src) "/" ".")
              dcl (DynamicClassLoader.)
              cls (.defineClass dcl cls-name bc nil)
              eval-field (.getField cls "EVAL_FN")
              _ (.set eval-field nil EVAL!)
              inline-fn-vals
                  {
                   "$$_FN" $$_FN
                   "SET_FN" SET_FN
                   "PRINT_FN" PRINT_FN
                   "ADD_FN" ADD_FN
                   "SUB_FN" SUB_FN
                   "MUL_FN" MUL_FN
                   "DIV_FN" DIV_FN
                   "LT_FN" LT_FN
                   "GT_FN" GT_FN
                   "LE_FN" LE_FN
                   "GE_FN" GE_FN
                   "EQ_FN" EQ_FN
                   "NEQ_FN" NEQ_FN
                   "SIGNAL_FN" SIGNAL_FN
                   "CONCAT_FN" CONCAT_FN
                   "SIZE_FN" SIZE_FN
                   "STR_FN" STR_FN}
              _ (doseq [[fld-name fld-val] inline-fn-vals] (.set (.getField cls fld-name) nil fld-val))
              method (.getMethod cls "run" (into-array Class []))
              run-fn (fn [] (.set eval-field nil EVAL!) (.invoke method nil (into-array Object [])))]
            (swap! class-cache assoc src run-fn)
            run-fn)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-jvm!
    [src]
    (GLOBALS (find-ns 'SNOBOL4clojure.core))
    (reset! STNO 0)
    (reset! <STNO> {})
    (reset! <LABL> {})
    (reset! <CODE> {})
    (reset! <FUNS> {})
    (reset! &STCOUNT 0)
    (reset! <ST-DEAD> nil)
    (let [run-fn (load-program src)]
        (try
            (run-fn)
            (catch java.lang.reflect.InvocationTargetException e
                (let [cause (.getCause e)]
                    (if
                        (instance? clojure.lang.ExceptionInfo cause)
                        (when-not (contains? #{:end :return :freturn :nreturn} (get (ex-data cause) :snobol/signal)) (throw cause))
                        (throw (or cause e))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-jvm-captured! [src] (with-out-str (run-jvm! src)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- init-inline-fns!
    []
    (alter-var-root #'$$_FN (constantly $$))
    (alter-var-root #'SET_FN (constantly protected-store!))
    (alter-var-root #'PRINT_FN (constantly (fn [v] (println (if (nil? v) "" (str v))) v)))
    (alter-var-root #'ADD_FN (constantly Σ+))
    (alter-var-root #'SUB_FN (constantly subtract))
    (alter-var-root #'MUL_FN (constantly multiply))
    (alter-var-root #'DIV_FN (constantly divide))
    (alter-var-root #'LT_FN (constantly (fn [a b] (let [na (num a) nb (num b)] (when (< na nb) a)))))
    (alter-var-root #'GT_FN (constantly (fn [a b] (let [na (num a) nb (num b)] (when (> na nb) a)))))
    (alter-var-root #'LE_FN (constantly (fn [a b] (let [na (num a) nb (num b)] (when (<= na nb) a)))))
    (alter-var-root #'GE_FN (constantly (fn [a b] (let [na (num a) nb (num b)] (when (>= na nb) a)))))
    (alter-var-root #'EQ_FN (constantly (fn [a b] (let [na (num a) nb (num b)] (when (clojure.core/= na nb) a)))))
    (alter-var-root #'NEQ_FN (constantly (fn [a b] (let [na (num a) nb (num b)] (when (not (clojure.core/= na nb)) a)))))
    (alter-var-root #'SIGNAL_FN (constantly (fn [^String sig-name] (throw (ex-info sig-name {:snobol/signal (keyword sig-name)})))))
    (alter-var-root #'CONCAT_FN
        (constantly
            (fn
                ([a b] (when (and (some? a) (some? b)) (str (if (nil? a) "" (str a)) (if (nil? b) "" (str b)))))
                ([a b & more] (let [all (list* a b more)] (when (every? some? all) (apply str (map str all))))))))
    (alter-var-root #'SIZE_FN (constantly (fn [s] (count (str s)))))
    (alter-var-root #'STR_FN (constantly (fn [v] (if (nil? v) nil (str v))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(init-inline-fns!)
(defn- name-identity [n] (if-let [c (and (seq? n) (:container (meta n)))] (cons c (rest n)) n))
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
    (let [pn (str "page " n) c (clojure.core/- (long @<WIDTH>) 8) t (if (< (count title) c) (str title (apply str (repeat (clojure.core/- c (count title)) \space))) title)]
        (str (subs t 0 c) pn (when (> (count t) (clojure.core/+ c (count pn))) (subs t (clojure.core/+ c (count pn)))))))
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
    [stmts swallowed]
    (loop [ss stmts i 0 n 0 starts (sorted-map)]
        (if-let [[s l] (first ss)]
            (if
                (and (not (contains? swallowed i)) (or (clojure.core/= :blank s) (not (#{\* \-} (first s)))))
                (recur (rest ss) (inc i) (inc n) (if (contains? starts l) starts (assoc starts l (inc n))))
                (recur (rest ss) (inc i) n starts))
            starts)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn write-listing!
    [file]
    (let [
          {:keys [text stmts cards crash swallowed]} @<SOURCE>
          lines (cond->> (string/split (str text) #"\n") crash (take crash))
          starts (statement-numbers stmts (or swallowed #{}))
          errs (group-by first @<DIAGNOSTICS>)
          fold false
          card (fn [^String l nm] (re-find (re-pattern (str (when fold "(?i)") "^-" nm "(?:\\s|,|$)")) l))
          st (atom {:page 0 :on-page 0 :title "" :subtitle nil :pending false :on false :print true :double false})]
        (loop [i 1 ls lines depth 0 places [] file (str file) fline 0 cards cards]
            (when-let [raw (first ls)]
                (let [l raw n (get starts i) ctl (string/starts-with? l "-") shown (fn [line] (when (and (:on @st) (or (not ctl) (:print @st))) (list-line! st (numbered n depth line))))]
                    (cond
                        (string/starts-with? l "-\uE000DEPTH-LEAVE") (recur (inc i) (rest ls) (max 0 (dec depth)) places file (inc fline) cards)
                        (string/starts-with? l "-\uE000FILE-LEAVE") (let [[f fl] (peek places)] (recur (inc i) (rest ls) (dec depth) (pop places) f fl cards))
                        (string/starts-with? l "-\uE000FILE-ENTER")
                            (let [[_ nm] (re-find #"^-\uE000FILE-ENTER \"([^\"]*)\"" l)] (shown (first cards)) (recur (inc i) (rest ls) (inc depth) (conj places [file (inc fline)]) nm 0 (rest cards)))
                        :else
                            (let [
                                  fline (inc fline)
                                  [file fline]
                                      (if-let [[_ ln f1 f2] (when ctl (re-find #"^-LINE\s+(\d+)(?:\s+(?:\"([^\"]*)\"|'([^']*)'))?" l))] [(or f1 f2 file) (dec (Long/parseLong ln))] [file fline])]
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
                                                            (if
                                                                (< (clojure.core/+ (long (:on-page @st)) k) page-lines)
                                                                (do (print (apply str (repeat k \newline))) (swap! st update :on-page clojure.core/+ k))
                                                                (new-page! st))))
                                                :else nil))
                                        (shown l))
                                    (shown l))
                                (doseq [[k [_ code col]] (map-indexed vector (get errs i))]
                                    (let [
                                          stmt (numbered (when-not ctl (some-> (first (rsubseq starts <= i)) val)) depth l)
                                          mark (if col (str (numbered nil 0 "") (apply str (map #(if (clojure.core/= \tab %) \tab \space) (take (dec (long col)) l))) "!") "")
                                          msg
                                              (if col
                                                  (format "%s(%d,%d) : ERROR %03d -- %s" file fline col code (error-message code))
                                                  (format "%s(%d) : ERROR %03d -- %s" file fline code (error-message code)))]
                                        (when (and (zero? (long k)) (not (:on @st))) (list-line! st stmt))
                                        (when (pos? (long k)) (write! ""))
                                        (doseq [x [mark msg ""]] (write! x))
                                        (swap! st update :on-page clojure.core/+ 3)
                                        (binding [*out* *err*] (doseq [x [(if (zero? (long k)) stmt "") mark msg ""]] (write! x)))))
                                (when-not
                                    (and n (re-find (if fold #"(?i)^END([ \t]|\z)" #"^(END|end)([ \t]|\z)") l))
                                    (recur (inc i) (rest ls) (if (and ctl (re-find #"^-INCLUDE\s+['\"]" l)) (inc depth) depth) places file fline cards)))))))
        (flush)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- row [label v] (str (format "%-21s" label) v))
(defn- regenerations [] (reduce clojure.core/+ 0 (map #(max 0 (.getCollectionCount ^GarbageCollectorMXBean %)) (ManagementFactory/getGarbageCollectorMXBeans))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- report-lines
    [code text stmts start-ns]
    (let [rt (Runtime/getRuntime) used (clojure.core/- (.totalMemory rt) (.freeMemory rt)) file @&FILE]
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
            (when-not (neg? (long @&STLIMIT)) [[(row "stmts executed" stmts) true] [(row "execution time msec" (quot (clojure.core/- (System/nanoTime) start-ns) 1000000)) true]])
            [[(row "REGENERATIONS" (regenerations)) true] [(row "memory used (bytes)" used) false] [(row "memory left (bytes)" (clojure.core/- (.maxMemory rt) used)) false] ["" true]])))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn report!
    [code text stmts start-ns]
    (doseq [[l both?] (report-lines code text stmts start-ns)]
        (let [width (long @<WIDTH>) pieces (if (<= (count l) width) [l] (map #(apply str %) (partition-all width l)))]
            (doseq [p pieces] (println p))
            (when both? (binding [*out* *err*] (println (last pieces)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn include-dirs [program-dir sno-lib] (vec (distinct (concat [program-dir] (remove clojure.string/blank? (clojure.string/split (or sno-lib "") #":")) ["."]))))
(def ^:dynamic *list-sink* nil)
(def ^:private dump-switch (atom nil))
(def ^:private exit-code (atom nil))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- noexecute?
    [^String src]
    (reduce
        (fn
            [no line]
            (reduce
                (fn [no c] (cond (re-find #"^-NOEXECUTE(?:\s|$)" c) true (re-find #"^-EXECUTE(?:\s|$)" c) false :else no))
                no
                (when (clojure.string/starts-with? line "-") (map #(if (clojure.string/starts-with? % "-") % (str "-" %)) (clojure.string/split line #",-")))))
        false
        (clojure.string/split-lines src)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn -main
    [& args]
    (reset! <WIDTH> 120)
    (reset! dump-switch nil)
    (reset! exit-code nil)
    (let [argv
              (loop [as args v [(System/getProperty "java.class.path")] switches? true]
                  (cond
                      (empty? as) v
                      (and switches? (clojure.core/= "--engine" (first as))) (recur (nnext as) v true)
                      (and switches? (clojure.core/= "-u" (first as))) (recur (nnext as) (into v (take 2 as)) true)
                      :else (recur (next as) (conj v (first as)) (and switches? (.startsWith ^String (first as) "-")))))
          [engine u args]
              (loop [engine (or (not-empty (System/getenv "SNOBOL4JVM_ENGINE")) "interp") u nil as args]
                  (cond
                      (clojure.core/= "--engine" (first as)) (recur (second as) u (nnext as))
                      (clojure.core/= "-u" (first as)) (recur engine (second as) (nnext as))
                      (and (first as) (re-matches #"-t\d+" (first as))) (do (reset! <WIDTH> (Long/parseLong (subs (first as) 2))) (recur engine u (next as)))
                      (and (first as) (re-matches #"-D\d+" (first as))) (do (reset! dump-switch (Long/parseLong (subs (first as) 2))) (recur engine u (next as)))
                      (and (first as) (re-matches #"-(?:[bfnelsx]+|[dsimt]\d+[kmgKMG]?)" (first as))) (recur engine u (next as))
                      :else [engine u as]))
          [file & prog-args] args]
        (when (or (nil? file) (not (#{"interp" "compiled"} engine))) (binding [*out* *err*] (println "Usage: lein run [--engine interp|compiled] <file.sno>")) (System/exit 1))
        (let [src (slurp file :encoding "ISO-8859-1") dir (or (.getParent (java.io.File. ^String file)) ".")]
            (let [
                  sink (some-> (or *list-sink* (System/getenv "SCRIP_SNO_LIST_SINK")) not-empty (java.io.FileWriter. java.nio.charset.StandardCharsets/ISO_8859_1))
                  listed (fn [f] (if sink (binding [*out* sink] (f) (flush)) (f)))
                  run (if (clojure.core/= "compiled" engine) (requiring-resolve 'SNOBOL4clojure.codegen/run-loaded) RUN)
                  rc
                      (case engine
                          ("interp" "compiled")
                              (do
                                  (GLOBALS)
                                  (when-some [d @dump-switch] (reset! &DUMP d))
                                  (reset! <HOST0> (or u (clojure.string/join " " (remove #{"--"} prog-args))))
                                  (reset! <ARGV> argv)
                                  (reset! <ARGI> (clojure.core/- (count argv) (count prog-args)))
                                  (let [entry (CODE src (include-dirs dir (System/getenv "SNO_LIB")) file)]
                                      (listed #(write-listing! file))
                                      (cond
                                          (:crash @<SOURCE>) 139
                                          (and (nil? (@<STNO> :END)) (nil? (@<STNO> :end))) (do (binding [*out* *err*] (println "No END statement found in source file(s).") (flush)) 1)
                                          (seq @<DIAGNOSTICS>) 231
                                          (noexecute? src) 231
                                          :else
                                              (do
                                                  (let [t0 (System/nanoTime)]
                                                      (try
                                                          (run entry)
                                                          (catch clojure.lang.ExceptionInfo e
                                                              (let [
                                                                    {n :snobol/error sig :snobol/signal steps :steps lvl :fnclevel} (ex-data e)
                                                                    _ (when lvl (reset! &FNCLEVEL lvl))
                                                                    n (if (and n (zero? (long n))) 36 n)
                                                                    code (or n (when (clojure.core/= :step-limit sig) 244))]
                                                                  (when-not (or code (clojure.core/= :exit sig)) (throw e))
                                                                  (if
                                                                      (clojure.core/= :exit sig)
                                                                      (reset! exit-code (long (:snobol/rc (ex-data e) 0)))
                                                                      (let [text (or (error-message code) (ex-message e))]
                                                                          (reset! &ERRTYPE code)
                                                                          (reset! &ERRTEXT text)
                                                                          (listed #(report! code text @&STCOUNT t0))))))
                                                          (catch StackOverflowError _
                                                              (let [text (error-message 246)] (reset! &ERRTYPE 246) (reset! &ERRTEXT text) (listed #(report! 246 text @&STCOUNT t0))))
                                                          (finally (when (and (nil? @exit-code) (pos? (long @&DUMP))) (listed #(print (dump-text @&DUMP)))))))
                                                  (let [c (long (or @exit-code @&CODE)) c (if (neg? c) 0 (mod c 256))] (when (pos? c) c)))))))]
                (flush)
                (when sink (.close ^java.io.Writer sink))
                (when (integer? rc) rc)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private stack-factor 256)
(def ^:private default-stack-kb 4096)
(defn- declared-stack-kb [] (or (try (let [v (Long/parseLong (System/getProperty "snobol4jvm.stack.kb"))] (when (pos? v) v)) (catch Exception _ nil)) default-stack-kb))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- on-a-stack
    [kb f]
    (let [result (promise) body (fn [] (deliver result (try {:value (f)} (catch Throwable t {:thrown t})))) floor (clojure.core/* 16 1024 1024)]
        (loop [bytes (clojure.core/* (long kb) 1024 stack-factor)]
            (let [t (try (doto (Thread. nil body "snobol4" (long bytes)) (.start)) (catch OutOfMemoryError e (if (> (quot bytes 4) floor) nil (throw e))))]
                (if t (do (.join ^Thread t) (let [r @result] (if (contains? r :thrown) (throw (:thrown r)) (:value r)))) (recur (quot bytes 4)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- run-program
    [args]
    (let [
          out (java.io.PrintWriter. (java.io.OutputStreamWriter. (java.io.FileOutputStream. java.io.FileDescriptor/out) "ISO-8859-1") true)
          err (java.io.PrintWriter. (java.io.OutputStreamWriter. (java.io.FileOutputStream. java.io.FileDescriptor/err) "ISO-8859-1") true)
          in (clojure.lang.LineNumberingPushbackReader. (java.io.InputStreamReader. System/in "ISO-8859-1"))]
        (let [rc (try (binding [*out* out *err* err *in* in] (apply (resolve 'SNOBOL4clojure.core/-main) args)) (finally (.flush out) (.flush err)))]
            (when (and (integer? rc) (pos? rc)) (System/exit rc)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn jar-main [& args] (require 'SNOBOL4clojure.core) (on-a-stack (declared-stack-kb) #(run-program args)))
