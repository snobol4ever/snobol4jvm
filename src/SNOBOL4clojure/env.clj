(ns SNOBOL4clojure.env
  "The runtime: its state, the built-in functions, the pattern constructors and scanners, and what sbl -bf writes besides
  the program's output (the listing, an error's report, the &DUMP dump, the traces). Everything here depends on nothing but itself."
  (:refer-clojure :exclude [num])
  (:require [clojure.pprint :as pp]
            [clojure.string :as string])
  (:import  [java.time LocalDateTime]
            [java.time.format DateTimeFormatter]
            [java.util Locale]
            [java.lang.management ManagementFactory GarbageCollectorMXBean]))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; Error messages: the text of each SPITBOL error number
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

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
331 "Goto SCONTINUE with no user interrupt"
})


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; Runtime state: variables, keywords, channels, ARRAY, TABLE, DATATYPE and GLOBALS
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

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
;; the [file line] of the code block an EVAL string compiles into, while its expression runs: <CODELINE> as it stands, the program's END line until a CODE text
;; moves it; a function called from that expression is given it back as it returns (operators.clj EVAL, the DEFINE wrapper)
(def ^:dynamic *eval-block* nil)
(def <CODELINE> (atom ["" 0]))   ; [file line] of the last line the compiler read, the program's END: each CODE text is the next line
(def <OBJNO> (atom 0))   ; how many arrays, tables and DATA objects the program has made: the number a dump gives each, #n
(defn next-serial! [] (swap! <OBJNO> inc))
;; The names the program has spoken: every name its statements' text holds (a variable, a function, a label, a goto's target, a keyword's name) and each name a
;; DEFINE or a DATA prototype makes, as SPITBOL keeps a variable block for each; &DUMP = 3 lists them all, the null ones too (manual ch. 7, &DUMP: "&DUMP = 3 adds
;; statement labels and all null-valued variables")
(def <NAMES> (atom #{}))
(defn note-names! [names] (when (seq names) (swap! <NAMES> into names)))
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
(def ^:dynamic *handlers* 0)   ; the SETEXIT handlers running one inside another, each at its error in a failure point inside a statement (runtime.clj RUN-AT-ERROR)
(def <STAGE> (atom :xt))
(def <GTCEF> (atom 0))
;; The stage left at execute-time compile (sbl.min err04, errc4): a compile at run time (EVAL of a string, CODE, CONVERT to EXPRESSION) that fails in a function's
;; statement pops to the function's frame, found before the compile's own call, and leaves the stage at compile; every later error is then taken there, and at
;; &ERRLIMIT 0 an error in a function fails its innermost failure point instead of ending the program, until a compile at run time succeeds or one fails in the
;; main program (operators.clj compiled!, error-decided)
(def <XC> (atom false))
(def <ERRFT> (atom 0))   ; the fatal errors so far (sbl.min errft): at the third the program ends whatever &ERRLIMIT is
(def <ERRSERIAL> (atom 0))   ; the errors so far that went on (sbl.min err07 sets r_cnt for each): CONTINUE goes on from the latest only
;; the page width sbl's -tN switch sets, 120 when none is given: the listing's lines, the column of "page N" (width less 8) and the lines of a compile or an
;; execution error's report are cut to it (manual ch. 13, -t; MEASURED on sbl -bf: -t400 puts "page 1" at columns 393 to 398 and writes a 173-column error
;; line whole, -t80 puts it at 73 to 78 and cuts the error line at 80; corpus gate_cases_snobol4 test_gate_sno_any_notany_span_..._error__c1, run -t400)
(def <WIDTH> (atom 120))
;; the line of the statement running, which an error's report names: &LINE as a statement starts, but kept when a trace function leaves &LINE at 2 (trace.clj
;; call-trace-fn!; MEASURED on sbl -bf: csnobol4_suite setexit4, an STCOUNT trace armed, reports ERROR 331 "in line 16", its :(SCONTINUE)'s line)
(def <STMT-LINE> (atom 0))
(def <HOST0> (atom ""))   ; what HOST(0) returns: the -u text, or else the program's own arguments joined by blanks
(def <ARGV>  (atom []))   ; the command line HOST(2, n) answers from: the engine's jar, then -main's arguments
(def <ARGI>  (atom 0))    ; what HOST(3) returns: the index in <ARGV> of the program's first own argument
(def <LASTOUT> (atom nil))   ; the last value assigned to OUTPUT, which a dump lists as the variable OUTPUT
(def <ASSOC-LAST> (atom {}))   ; an associated variable (INPUT, OUTPUT, a channel's) -> the last record read or written through it, which a dump lists as its value
(def <SETEXIT> (atom nil))   ; the label SETEXIT named (a keyword), or nil when no handler is armed
(def <DISPNO> (atom {}))   ; statement key (the number a statement is stored under) -> the number SPITBOL gives it, which counts blank lines
(def BLANKS   (atom 0))    ; the blank lines of the code loaded so far: they take numbers and are never run
(def <TRACES> (atom {}))   ; the TRACE registrations: [name kind] -> options; a program's own state, reset with the rest
(def <ETRACES> (atom []))
;; The variable SPITBOL names an array, a table or a DATA object by (sbl.min prtnm): the first natural variable holding it in the order of its variable hash table,
;; slot by slot, after first trying the one it found last time. A name's slot is SPITBOL's string hash (hashs: the length exclusive-or'd with the first three
;; 8-byte words of characters, little-endian) modulo 257, this build's bucket count (MEASURED on sbl -bf: 40 pairs of names, each an array two variables hold, an
;; element of it traced and assigned; the name each line shows fits 257 alone). Names in one slot are taken in name order here, SPITBOL's being the order they
;; were made.
(def <PRNMV> (atom nil))
(defn- var-slot [^String s]
  (let [b ^bytes (.getBytes s "ISO-8859-1") n (alength b)
        h (reduce (fn [h w] (bit-xor h (reduce (fn [acc j] (let [i (clojure.core/+ (clojure.core/* 8 w) j)]
                                                            (if (< i n) (bit-or acc (bit-shift-left (bit-and 0xff (long (aget b i))) (clojure.core/* 8 j))) acc)))
                                               0 (range 8))))
                  (long n) (range (min 3 (quot (clojure.core/+ n 7) 8))))]
    (mod (bit-and Long/MAX_VALUE h) 257)))   ; the element traces, TRACE(.A[2], 'VALUE'): their container by identity, subscripts and text (trace.clj)
(def <OPSYN> (atom {}))

;; ── Runtime statement table (used by compiler + runtime) ─────────────────────
(def  STNO   (atom 0))
(def <STNO>  (atom {}))
(def <LABL>  (atom {}))
(def <CODE>  (atom {}))
;; the statement that closes a block CODE() compiles at run time: reached, it is counted and numbered as any statement is, and it ends the program
(def block-end [::block-end])
;; the statement in error that ends a block CODE() failed to compile: reached, it is "7 Compilation error encountered during execution"
(def compile-error [::compile-error])

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
;; The identity of a name of an element is the element, the table or array it was made from and its subscripts, never the variable that held it, which only
;; spells it: the name of t['k'] carries its table as :container, and ITEM(t,'k') names the table itself (MEASURED on sbl -bf: IDENT(.ITEM(t,'k'), .t['k'])
;; succeeds; corpus gate_cases_snobol4 test_gate_sno_ident_of_two_names_of_one_table_or_array_element_succeeds, which this engine failed)
(defn- name-identity [n]
  (if-let [c (and (seq? n) (:container (meta n)))] (cons c (rest n)) n))
(deftype NAME [^:unsynchronized-mutable n] &NAME
  (n [this]  n)
  (n [this _] (set! n _))
  Object
  (equals   [this o] (and (instance? &NAME o) (clojure.core/= (name-identity n) (name-identity (.n ^&NAME o)))))
  (hashCode [this]   (hash (name-identity n)))
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
  ;; an exponent marker with no digits is the exponent 0 when a blank follows it (MEASURED on sbl -bf: CONVERT('1e ', 'REAL'), '1e+ ', '1e- ', '1d ' and '1.e '
  ;; are 1., where '1e', '1e+' and '1e 2' fail; corpus gate_cases_snobol4 convert_reads_a_numeric_string_as_spitbol_gtnum_does__w)
  (when-let [[_ sg ^String ip point ex] (re-matches #"\s*([+-]?)(\d*)(?:\.(\d*))?(?:[eEdD]([+-]?\d+|[+-]?(?=\s)))?\s*" (str s))]
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
          sc    (try (clojure.core/- (.length fp) (Long/parseLong (if (or (nil? ex) (re-matches #"[+-]?" ex)) "0" ex))) (catch NumberFormatException _ nil))]
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
;; A sign that only blanks follow is the integer 0, a quirk of sbl.min gtnum, which ends the number at the blank with no digit read; a bare sign fails (MEASURED
;; on sbl -bf: CONVERT('- ', 'INTEGER'), CONVERT(' - ', ...) and CONVERT('+ ', ...) are 0, NUMERIC an INTEGER and REAL 0.; '-', '+', ' -' and '- 5' fail;
;; corpus gate_cases_snobol4 convert_reads_a_numeric_string_as_spitbol_gtnum_does__w)
(defn sign-then-blanks? [x] (and (string? x) (boolean (re-matches #"[ \t]*[+-][ \t]+" x))))

(defn num [x]
  (cond
    (sign-then-blanks? x) 0
    (nil? x)     0                        ; unset variable = 0 in arithmetic context
    (double? x)  x
    (integer? x) x                        ; preserve integer type
    true (let [s (clojure.string/trim (str x))]
           (if (clojure.core/= s "")
             0                            ; empty string = 0 in arithmetic context
             ;; an integer that fits, else a real as gtnum reads one; anything else is not numeric (##NaN, the operator's error), as sbl -bf has it
             (or (try (Long/parseLong s)
                   (catch NumberFormatException _ nil))
                 (parse-real (str x))   ; untrimmed: a blank after an exponent marker is what makes it the exponent 0 (parse-real)
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
   (reset! <XC>       false)
   (reset! <ERRFT>    0)
   (reset! <ERRSERIAL> 0)
   (reset! <OBJNO>    0)
   (reset! <LASTOUT>  nil)
   (reset! <ASSOC-LAST> {})
   (reset! <HOST0>    "")
   (reset! <ARGV>     [])
   (reset! <ARGI>     0)
   (reset! <PRIMS-READ> #{})
   (reset! <NAMES> #{})
   (reset! <ETRACES> [])
   (reset! <PRNMV> nil)
   (reset! &RTNTYPE   ε)
   (reset! &ERRTYPE   0)
   (reset! &ERRTEXT   ε)
   (reset! <POS>      {})
   (reset! <CODELINE> ["" 0])
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
  (merge (into {} (for [[n m] error-messages :when (not (#{274 276 278} n))] [n (clojure.string/lower-case m)]))
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

;; A keyword's name is upper case, as every built-in name is: snobol4jvm never folds case (Lon 2026-11-17 18:2x CST, in the JVM window, verbatim: "We WILL NEVER
;; support case-folding. We will always have upper-case built-in keywords and functions."). MEASURED on sbl -bf the same: &trim = 1 and X = &trim each end
;; "ERROR 251 -- keyword operand is not name of defined keyword"; this engine upper-cased the name of an assignment and took &trim = 1 as &TRIM = 1.
;; Every &K def of this file is a keyword, SPITBOL's and the extensions snobol4jvm keeps (&DIGITS, CSNOBOL4's; &IOCOMPAT, its own).
(defn keyword-var
  "The var of keyword &K (K a string or symbol, spelled as the program spells it), or nil when there is no such keyword."
  [k]
  (ns-resolve (the-ns 'SNOBOL4clojure.env) (symbol (str "&" (name k)))))

;; &K = V, with SPITBOL's errors: an unknown keyword is 251, a value that is not
;; an integer is 208, a protected keyword is 209. Assigning &ERRTYPE raises the
;; error it names.
(defn assign-keyword! [k v]
  (let [kw (name k)
        vr (keyword-var kw)]
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
;; "These variables contain the primitive patterns of the same name" (manual ch. 18), and SPITBOL protects them from a direct assignment: the compiled store of
;; X = V goes through vrsto, b_vre for these seven, "042 attempt to change value of protected variable" (sbl -bf: ARB = 'Q', BAL = P and FENCE = 'f' are 42),
;; so the compiler may take their values as constants. Every other write sets the value: a call sets and restores the function's name, arguments and locals
;; in vrval (sbl.min b_pfc), an indirect store and a pattern's capture go through asign (sbl -bf: a DEFINEd ABORT runs and returns the null string, an
;; argument ARB takes its argument, $'FENCE' = 'q' and 'x' ? LEN(1) . ARB assign).
(def protected-variables '#{ABORT ARB BAL FAIL FENCE REM SUCCEED})

;; An output-associated variable's value is the one a read of it finds, <ASSOC-LAST>: a call binds a parameter, a local or the result so, without I/O, and its
;; body reads what was bound (sbl.min b_pfc sets vrval; MEASURED on sbl -bf: OUTPUT(.V, 7, 'out.txt'), V = 'first', then F('arg') of DEFINE('F(V)') reads V as
;; 'arg' inside and writes nothing until V = 'last'; corpus gate_cases_snobol4 a_call_saves_binds_and_restores_an_io_associated_name_without_io__outparam)
(defn output-associated? [sym] (clojure.core/= :output (:type (get @<CHANNELS> sym))))

(defn snobol-set!
  "Assign val to the SNOBOL4 user variable named by sym."
  [sym val]
  (let [s (symbol (name sym))]
    (swap! <VARS> assoc s val)
    (when (output-associated? s) (swap! <ASSOC-LAST> assoc s val)))
  val)

(defn protected-store!
  "A direct assignment's store, X = V: 042 for a protected variable, else snobol-set!."
  [sym val]
  (when (protected-variables (symbol (name sym))) (throw (ex-info (error-message 42) {:snobol/error 42})))
  (snobol-set! sym val))

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

;; The keyboard TERMINAL reads (manual ch. 6): /dev/tty, opened once, ISO-8859-1 as main.clj reads a program's streams; nil where no terminal can be opened,
;; so a read of TERMINAL fails there as it does on sbl -bf. The failure to open is remembered, so a loop over TERMINAL tries the device once. *terminal-device*
;; names the device, so a test can name one that does not exist and never waits on the keyboard of whoever runs it.
(def ^:dynamic *terminal-device* "/dev/tty")
(defonce ^:private terminal-cache (atom nil))   ; [device reader-or-nil]
(defn terminal-reader ^java.io.BufferedReader []
  (let [dev *terminal-device* [k r] @terminal-cache]
    (if (clojure.core/= k dev)
      r
      (let [r (try (java.io.BufferedReader. (java.io.InputStreamReader. (java.io.FileInputStream. ^String dev) "ISO-8859-1")) (catch Exception _ nil))]
        (reset! terminal-cache [dev r])
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
;; The engine's own names that are no SPITBOL function are not built-ins here, so a call of one is "22 undefined function" (sbl -bf, APPLY of each: the pattern
;; variables ARB BAL FAIL REM SUCCEED; BOL EOL NAME NSPAN REAL; FULLMATCH GLOBALS INVOKE MATCH RUN SEARCH). A built-in's name is upper case only, so a lower-case
;; name (table, equal, transpile) reaches none and is the program's own or 22, as under sbl -bf (Lon: "snobol4jvm uses only uppercase built-in functions", CEO-2028).
(def ^:private no-function '#{ARB BAL FAIL REM SUCCEED BOL EOL NAME NSPAN REAL FULLMATCH GLOBALS INVOKE MATCH RUN SEARCH})
;; The pattern a pattern variable holds from the start, read as the program reads it: a dump then lists the variable (dump.clj).
(defn primitive-pattern [sym]
  (let [resolved (ns-resolve (find-ns 'SNOBOL4clojure.core) sym)
        v        (when (instance? clojure.lang.Var resolved) (var-get resolved))]
    (if (nil? v)
      ε
      (do (when-not (clojure.core/= sym 'FENCE) (swap! <PRIMS-READ> conj sym)) v))))

(declare $$)
(defn $$fn [N]
  (let [sym (symbol (name N))]
    (if-let [v (find @<VARS> sym)]
      (val v)
      (let [resolved (when (and (re-matches #"[A-Z][A-Z0-9]*" (name sym)) (not (no-function sym))) (ns-resolve (find-ns 'SNOBOL4clojure.core) sym))]
        (if (instance? clojure.lang.Var resolved) (var-get resolved) ($$ N))))))

(defn holder-name
  "The name of the natural variable SPITBOL names the object v by (<PRNMV> above), a string, or nil when no variable holds it."
  [v]
  (let [last @<PRNMV>]
    (if (and last (identical? v (get @<VARS> last)))
      (name last)
      (when-let [k (first (sort-by (fn [k] [(var-slot (name k)) (name k)]) (keep (fn [[k x]] (when (identical? x v) k)) @<VARS>)))]
        (reset! <PRNMV> k)
        (name k)))))

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

      ;; TERMINAL reads the keyboard, never standard input ("TERMINAL always refers to the same things—the keyboard for input and the screen for output", manual
      ;; ch. 6): a line of /dev/tty, and a statement that reads it fails where there is no terminal. MEASURED on sbl -bf with no terminal: X = TERMINAL fails with
      ;; stdin a pipe holding a line and with stdin /dev/null alike, so dotnet palin and temp end at once; this engine read the null string and looped for ever.
      (clojure.core/= sym 'TERMINAL)
      (let [line (try (.readLine (terminal-reader)) (catch Exception _ nil))
            v    (when-not (nil? line) (input-record line))]
        (when (some? v) (swap! <ASSOC-LAST> assoc 'TERMINAL v))
        v)

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
        (if (protected-variables sym) (primitive-pattern sym) ε)))))

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

;; ARRAY's prototype (manual ch. 19, ARRAY; sbl.min s_arr): an argument that converts whole to an integer is one dimension of that many elements, 1 to N, and
;; the prototype PROTOTYPE gives is that integer; any other string is dimensions split at commas, each "lo:hi" or "hi" (lo 1), every bound converted to an integer as
;; an argument is (blanks around it, a real truncated, the null string 0), and is kept as it was written. A value neither integer nor string is "064 array first
;; argument is not integer or string", a lower bound that will not convert 065, an upper 066, a dimension of no element 067 (MEASURED on sbl -bf: ARRAY(2.9),
;; ARRAY('2.9'), ARRAY(' 3') and ARRAY('1e1') have prototypes 2, 2, 3 and 10; ARRAY(' 2 : 4 , 3 ') keeps ' 2 : 4 , 3 '; ARRAY(':2') is 0:2; ARRAY('1.5,2')
;; is 1 by 2; LEN(1) and TABLE() are 64, 'x' and '2,x' 66, 'a:2' 65, '3:1', 0, '', '2,', -1 and '2:' 67; corpus gate_cases_snobol4
;; array_first_argument_converts_and_raises_as_spitbol_does__access, which this engine answered with an exception)
(defn- array-bound [x err]
  (let [n (num x)]
    (cond (integer? n) (long n)
          (and (double? n) (not (Double/isNaN n))) (long n)
          :else (throw (ex-info (error-message err) {:snobol/error err})))))

(defn- dimension [lo hi]
  (when (> (long lo) (long hi)) (throw (ex-info (error-message 67) {:snobol/error 67})))
  [lo hi])

(defn- parse-array-proto
  "[dims proto]: the [lo hi] pairs of ARRAY's first argument and the prototype PROTOTYPE answers."
  [proto]
  ;; The name of a natural variable is its text here, as where a number is wanted (sbl -bf: ARRAY(.$'3', .$'7') is ARRAY 3 of 7s, ARRAY(.$'2:4') has bounds 2:4,
  ;; ARRAY(.X) is 066 as ARRAY('X'); the name of an element, .A<1>, has no text and stays 064; corpus snobol4-rungs array_convert_dupl_replace_1)
  (let [proto (if (and (instance? NAME proto) (symbol? (.n ^NAME proto))) (str (.n ^NAME proto)) proto)
        whole (when (or (number? proto) (string? proto)) (let [n (num proto)] (when (or (integer? n) (and (double? n) (not (Double/isNaN n)))) (long n))))]
    (cond
      whole                [[(dimension 1 whole)] (str whole)]
      (not (string? proto)) (throw (ex-info (error-message 64) {:snobol/error 64}))
      :else
      [(mapv (fn [part]
               (if-some [i (clojure.string/index-of part ":")]
                 (dimension (array-bound (subs part 0 i) 65) (array-bound (subs part (inc i)) 66))
                 (dimension 1 (array-bound part 66))))
             (clojure.string/split proto #"," -1))
       proto])))

(defn ARRAY
  "ARRAY(proto) or ARRAY(proto, default): a SnobolArray of the dimensions proto gives, each element default."
  ([proto] (ARRAY proto ε))
  ([proto dflt]
   (let [[dims text] (parse-array-proto proto)]
     (assoc (->SnobolArray dims dflt (atom {})) :proto text :serial (next-serial!)))))

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
(defn empty-set [] (hash-set))

(defn table?    [x] (and (instance? clojure.lang.IDeref x)
                         (map? @x)))
(declare object-identity?)
;; A key that is the object itself: a pattern, an expression or the name of a keyword keys a table by its identity (object-identity?)
(deftype ObjKey [v]
  Object
  (equals   [_ o] (and (instance? ObjKey o) (identical? v (.v ^ObjKey o))))
  (hashCode [_]   (System/identityHashCode v)))
(defn- table-key [k] (if (object-identity? k) (ObjKey. k) k))

;; A TABLE takes one subscript: more is "237 table referenced with more than one subscript" (manual Appendix D), as sbl -bf raises it for T[1,2] read, T[1,2] = 3
;; and ITEM(T,1,2) alike (MEASURED). snobol4jvm read the first key and dropped the rest, so spitbol_x64_tests demos/kwic2, whose SORT of an empty table fails
;; and leaves CAPTURE a table, read CAPTURE[I = I + 1, 2] for ever where sbl -bf ends at line 103 with ERROR 237. -> the one key.
(defn table-subscript [ks]
  (if (next ks) (throw (ex-info (error-message 237) {:snobol/error 237})) (first ks)))
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

;; A count of subscripts that is not the array's dimension count is "236 array referenced with wrong number of subscripts", checked before the subscripts
;; are converted and before the bounds (MEASURED on sbl -bf: of ARRAY(2), A<1,'x'>, A<1,2>, A<3,1>, ITEM(A), ITEM(A,1,'x') and the capture target A<1,2> are 236,
;; A<'x'> 238 and A<3> fails; of ARRAY('2,2'), B<1> and ITEM(B,1) are 236, B<1,'x'> 238). snobol4jvm failed the statement for a wrong count.
(defn- array-check-bounds
  "Returns nil (failure) if idx-vec is out of bounds, else the normalised key."
  [^SnobolArray arr idx-vec]
  (let [dims    (:dims arr)
        _       (when-not (clojure.core/= (count idx-vec) (count dims))
                  (throw (ex-info (error-message 236) {:snobol/error 236})))
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
;; Each signal carries nothing that varies, so one exception of each is thrown every time: clojure.core/ex-info reads the whole Java stack trace of the thrower (elide-top-frames:
;; getStackTrace, a line number decoded for every frame) and a call of a SNOBOL4 function is dozens of Java frames deep per level, so a RETURN cost a stack trace of the whole
;; recursion (fib(24) took 22 s and 281 G instructions, 73 percent of them in the JVM's trace code); a thrown constant has neither that nor fillInStackTrace's walk.
(def ^:private return-signal  (clojure.lang.ExceptionInfo. "RETURN"  {:snobol/signal :return}))
(def ^:private freturn-signal (clojure.lang.ExceptionInfo. "FRETURN" {:snobol/signal :freturn}))
(def ^:private nreturn-signal (clojure.lang.ExceptionInfo. "NRETURN" {:snobol/signal :nreturn}))
(def ^:private end-signal     (clojure.lang.ExceptionInfo. "END"     {:snobol/signal :end}))
(defn snobol-return!  [] (throw return-signal))
(defn snobol-freturn! [] (throw freturn-signal))
(defn snobol-nreturn! [] (throw nreturn-signal))
(defn snobol-end!     [] (throw end-signal))
(defn snobol-steplimit! [n]
  (throw (ex-info (str "Step limit exceeded at step " n)
                  {:snobol/signal :step-limit :steps n})))
;; Maps JVM class names to SNOBOL4 type names.
(defmulti  DATATYPE (fn [X] (str (class X))))
(defmethod DATATYPE "class java.lang.Character"                   [_] "STRING")
(defmethod DATATYPE "class java.lang.String"                      [_] "STRING")
(defmethod DATATYPE "class java.lang.Long"                        [_] "INTEGER")
;; an int a Java call answers is an INTEGER too (SIZE's count was one: CONVERT(SIZE(s), 'STRING') threw where sbl -bf answers the digits; corpus
;; gate_cases_snobol4 substr_indexes_bytes_as_the_pattern_cursor_does__census)
(defmethod DATATYPE "class java.lang.Integer"                     [_] "INTEGER")
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
;; BREAKX builds its backtracking node BREAKX# (patterns.clj), a PATTERN as BREAK's is (sbl -bf: DATATYPE(BREAKX('a')) is PATTERN; it read EXPRESSION)
(defmethod DATATYPE "class clojure.lang.PersistentList"           [X]
  (cond
    ('#{SEQ ALT CAPTURE CAPTURE-IMM CAPTURE-COND CURSOR-IMM! COLLECT!
        ANY$ NOTANY$ SPAN$ NSPAN$ BREAK$ BREAKX$ BREAKX#
        LEN# POS# RPOS# TAB# RTAB# BOL# EOL#
        FENCE! ARBNO! ARB! BAL! REM! ABORT!
        SUCCEED! FAIL! X} (first X)) "PATTERN"
    (:pattern (meta X))             "PATTERN"   ; a pattern function's element deferred to the match (patterns.clj deferred-pattern)
    :else                           "EXPRESSION"))
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


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; The &DUMP dump, and the value text traces show
;;
;; The dump a program asks for with &DUMP: at its end, the natural variables it holds and then the keywords, in SPITBOL's layout (measured against sbl -bf).
;; &DUMP = 1 lists the variables and the keywords; 2 also lists the elements of the arrays and tables the variables hold.
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

(defn- subscript-text [ks] (string/join "," (map (fn [k] (if (string? k) (str "'" k "'") (sstr k))) ks)))

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
    (double? v)  (sstr v)
    (instance? NAME v)    (name-text v)
    ;; an array by its prototype as ARRAY was given it (MEASURED on sbl -bf: ARRAY('1:3') dumps ARRAY(1:3), ARRAY(' 2 : 4 , 3 ') ARRAY( 2 : 4 , 3 ), ARRAY(2.9)
    ;; ARRAY(2); corpus gate_cases_snobol4 dump_matches_spitbol__dump2b)
    (array? v)        (str "ARRAY(" (or (:proto v) (dims-text (:dims v))) ") #" (:serial v))
    (table? v)        (str "TABLE(" (:size (meta v) 11) ") #" (:serial (meta v)))
    (instance? DataObj v) (str (.type-name ^DataObj v) " #" (.serial ^DataObj v))
    (symbol? (:defer-var (meta v))) (str "*" (:defer-var (meta v)))   ; *NAME for a variable, EXPRESSION for anything larger
    :else (DATATYPE v)))

;; The seven pattern variables SPITBOL predefines are listed when the program's text names one, bare or called, whether or not the statement runs (MEASURED on
;; sbl -bf: "Y = BAL ARB REM FAIL FENCE ABORT SUCCEED" skipped by a goto lists all seven = PATTERN, and Z = FENCE('a') lists FENCE; ARBNO is none; corpus
;; decus_vax84a diag1, whose BAL lines never run, where this engine listed only the ones a run read)
(def ^:private pattern-variables '#{ABORT ARB BAL FAIL FENCE REM SUCCEED})
(defn- named-patterns []
  (into {} (for [n @<NAMES> :let [s (symbol n)] :when (pattern-variables s)]
             [s (if (clojure.core/= s 'FENCE) @(ns-resolve (the-ns 'SNOBOL4clojure.operators) 'bare-fence) (primitive-pattern s))])))

(defn- variables [lvl]
  (let [vars (cond-> (merge (named-patterns) (into {} (for [s @<PRIMS-READ>] [s (deref (ns-resolve (the-ns 'SNOBOL4clojure.core) s))])) @<VARS>)
               true (merge @<ASSOC-LAST>))]   ; an associated variable holds the last record read or written through it
    (if (>= (long lvl) 3)
      ;; &DUMP = 3 "adds statement labels and all null-valued variables" (manual ch. 7): every name the program spoke, null ones as '' (MEASURED on sbl -bf: corpus
      ;; setl4_spitbol sbl-sonnets, &DUMP = 3, lists ANCHOR = '', BREAK = '', END = '', RETURN = '', a DEFINE's locals and a DATA's fields, where this engine
      ;; listed the non-null ones alone)
      (sort-by (comp name key)
               ;; INPUT, OUTPUT and TERMINAL are SPITBOL's own, there in every program (MEASURED: "        &DUMP = 3" / "        X = 1" / "END" lists them)
               (for [[k v] (merge (into {} (for [n (into @<NAMES> ["INPUT" "OUTPUT" "TERMINAL"])] [(symbol n) ""])) vars)]
                 (clojure.lang.MapEntry/create k (if (or (nil? v) (fn? v)) "" v))))
      (sort-by (comp name key) (remove (fn [[_ v]] (or (nil? v) (clojure.core/= "" v) (fn? v))) vars)))))

(def ^:private keyword-order
  '[ANCHOR CASE CODE DUMP ERRLIMIT ERRTEXT ERRTYPE FILE FNCLEVEL FTRACE FULLSCAN INPUT LASTFILE LASTLINE LASTNO LINE MAXLNGTH OUTPUT PROFILE RTNTYPE STCOUNT STLIMIT STNO TRACE TRIM])

(defn- keywords []
  (for [k keyword-order]
    (let [v @@(ns-resolve (the-ns 'SNOBOL4clojure.env) (symbol (str "&" k)))]
      (str "&" k " = " (if (string? v) (str "'" v "'") (sstr v))))))

;; The objects a dump at level 2 lists: every array, table and DATA object the variables reach, through elements and fields too, in the order they were made (their
;; numbers), each named by the variable that holds it, or by its own display (NODE #2) when none does. Its lines are holder<subscripts> = value for an element and
;; field(holder) = value for a field, the null ones left out (manual ch. 19 DUMP: "non-null array and table elements, and non-null field values of program-defined data
;; types"), as sbl -bf writes them.
(defn- serial-of [v] (cond (array? v) (:serial v) (table? v) (:serial (meta v)) (instance? DataObj v) (.serial ^DataObj v)))

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
    (array? v)        (for [[k x] (sort-by key @(:data v))] [:sub k x])
    (table? v)        (let [n (long (:size (meta v) 11))]
                            (for [[k x] (sort-by (fn [[k _]] (if-some [h (key-hash k)] (mod h n) Long/MAX_VALUE)) (table-entries v))] [:sub [k] x]))
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
    ;; each line is cut at the page width, as every line SPITBOL writes to its listing is (MEASURED on sbl -bf: X = DUPL('b',200) dumps a 120-column line and
    ;; the 91 after it; corpus gate_cases_snobol4 dump_matches_spitbol__dump2b)
    (str (string/join "\n"
           (mapcat (fn [^String l] (let [w (long @<WIDTH>)] (if (<= (count l) w) [l] (map #(apply str %) (partition-all w l)))))
                   (concat [""]
                           (sec "dump of natural variables" (map (fn [[k v]] (str k " = " (value-text v))) vars))
                           (sec "dump of keyword values" (keywords))
                           (when (>= lvl 2) (mapcat #(object-lines vars %) (objects vars))))))
         "\n\n\n\n")))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; The -LIST listing and a compile error's page
;;
;; The program listing SPITBOL writes on standard output: the lines -LIST asks for, and the page a compile error is reported on (measured against sbl -bf).
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; "-LIST This statement turns on listing, thereby producing a program listing with statement numbers"; "-NOLIST This statement turns off listing (the default)";
;; -EJECT "Advance to a new page"; "-SPACE N This sends N blank lines to the listing. N defaults to one"; -TITLE and -STITL: "The list file is advanced to a new page
;; prior to printing the new title and any subtitle line"; -PRINT and -NOPRINT whether control statements are listed; -SINGLE and -DOUBLE its spacing (manual ch. 14,
;; Listing controls). As sbl -bf lays it out: the first line listed brings the head (three empty lines, the identity and clock lines, three empty lines) and page 1; a
;; page opens with its title line, "page N" at column 113, its subtitle line once a -TITLE or -STITL has given one, and an empty line; a page holds 55 listed lines
;; (the first 51) and the next one opens three empty lines on. A line on which a statement begins carries the statement's number in its first columns, an included
;; line the depth of its -INCLUDE in column 6 ("The include nest depth appears in the listing file between the statement number and the statement proper", -INCLUDE),
;; and every line is cut at 120 columns. A -SPACE's empty lines come before its own line, an -EJECT's or a title's new page before its own. Nothing after END is listed.
;; A compile error is written after the line it is found on: an empty line, file(line) : ERROR nnn -- text, an empty line; when the listing is off, the line is written
;; first, on the page the first error opens. The line, the empty line, the error and the empty line go to standard error as well.
(def ^:private page-lines 55)

;; the head names this engine (project.clj's version), as sbl's names its own build
(def ^:private identity-line "snobol4jvm version 0.2.0")

(defn- clock-line []
  (let [arch (System/getProperty "os.arch")]
    (str (if (#{"amd64" "x86_64"} arch) "x86-64" arch) "  "
         (.format (DateTimeFormatter/ofPattern "EEE MMM ppd HH:mm:ss yyyy" Locale/US) (LocalDateTime/now)))))

(defn- cut [^String l] (let [width (long @<WIDTH>)] (if (<= (count l) width) [l] (map #(apply str %) (partition-all width l)))))

(defn- write! [l] (doseq [p (cut l)] (println p)))

(defn- page-line [^String title n]
  (let [pn (str "page " n)
        c  (- (long @<WIDTH>) 8)
        t  (if (< (count title) c) (str title (apply str (repeat (- c (count title)) \space))) title)]
    (str (subs t 0 c) pn (when (> (count t) (+ c (count pn))) (subs t (+ c (count pn)))))))

(defn- start-page! [st used]
  (swap! st update :page inc)
  (write! (page-line (:title @st) (:page @st)))
  (swap! st assoc :on-page used)
  (when-some [s (:subtitle @st)] (write! s) (swap! st update :on-page inc))
  (println))

(defn- new-page! [st] (print "\n\n\n") (start-page! st 0))

;; the head and page 1, when nothing is listed yet: true when it opened them
(defn- open! [st]
  (when (zero? (long (:page @st)))
    (print "\n\n\n") (println identity-line) (println (clock-line)) (print "\n\n\n")
    (start-page! st 4)
    true))

(defn- list-line! [st l]
  (when (:pending @st)
    (swap! st assoc :pending false)
    (when (pos? (long (:page @st))) (new-page! st)))
  (when-not (open! st)
    (when (>= (long (:on-page @st)) page-lines) (new-page! st)))
  (write! l)
  (swap! st update :on-page inc)
  (when (:double @st) (println) (swap! st update :on-page inc)))

;; A listed source line loses the blanks and tabs at its end, as sbl -bf prints it (MEASURED: "        X = 1 ? " and "        X = 1 ?\t" each list as
;; "1               X = 1 ?"; corpus tests/snobol4 compile_diag__p35), while the number field keeps its width (a numbered line with no text is "4" and seven blanks)
(defn- numbered [n depth line]
  (let [line (string/replace line #"[ \t]+\z" "")]   ; a carriage return stays, listed as sbl -bf lists it
    (if (pos? (long depth))
      (format "%-5s%-3s%s" (or n "") depth line)
      (format "%-8s%s" (or n "") line))))

;; The number of the statement each physical line of the source begins, and of the statement in progress on each line: split-source's statements, [text line], are
;; numbered in order, a blank line too, and neither a comment nor a control line.
(defn- statement-numbers [stmts]
  (loop [ss stmts n 0 starts (sorted-map)]
    (if-let [[s l] (first ss)]
      (if (or (= :blank s) (not (#{\* \-} (first s))))
        (recur (rest ss) (inc n) (if (contains? starts l) starts (assoc starts l (inc n))))
        (recur (rest ss) n starts))
      starts)))

(defn write-listing!
  "Write the listing of the program last compiled, named file, as far as its -LIST cards ask for one, and its compile errors on their page."
  [file]
  (let [{:keys [text stmts cards]} @<SOURCE>
        lines  (string/split (str text) #"\n")   ; LF alone ends a line: a CR is a character of it (compiler split-source)
        starts (statement-numbers stmts)
        errs   (group-by first @<DIAGNOSTICS>)
        fold   false   ; snobol4jvm never folds case, so a card is upper case whatever &CASE holds (Lon 2026-11-17 18:2x CST, in the JVM window)
        card   (fn [^String l nm] (re-find (re-pattern (str (when fold "(?i)") "^-" nm "(?:\\s|,|$)")) l))
        st     (atom {:page 0 :on-page 0 :title "" :subtitle nil :pending false :on false :print true :double false})]
    ;; cards: the -INCLUDE lines as written, one for each -FILE-ENTER card the splice put in their place
    (loop [i 1 ls lines depth 0 places [] file (str file) fline 0 cards cards]
      (when-let [raw (first ls)]
        (let [l   raw
              n   (get starts i)
              ctl (string/starts-with? l "-")
              shown (fn [line] (when (and (:on @st) (or (not ctl) (:print @st))) (list-line! st (numbered n depth line))))]
          (cond
            ;; the END that closes an include that could not be opened (compiler, preprocess-includes*): the depth it opened ends
            (string/starts-with? l "-DEPTH-LEAVE")
            (recur (inc i) (rest ls) (max 0 (dec depth)) places file (inc fline) cards)
            (string/starts-with? l "-FILE-LEAVE")
            (let [[f fl] (peek places)] (recur (inc i) (rest ls) (dec depth) (pop places) f fl cards))
            (string/starts-with? l "-FILE-ENTER")
            (let [[_ nm] (re-find #"^-FILE-ENTER \"([^\"]*)\"" l)]
              (shown (first cards))
              (recur (inc i) (rest ls) (inc depth) (conj places [file (inc fline)]) nm 0 (rest cards)))
            :else
            (let [fline (inc fline)]
              ;; a card may be followed by a comma and another card, each taking effect, and the line is listed once (MEASURED on sbl -bf: "-LIST,-EJECT" turns
              ;; the listing on and is listed whole; -TITLE and -STITL take the rest of their line, so "-TITLE x,-LIST" titles the page "x,-LIST"; corpus
              ;; gate_cases_snobol4 test_gate_snobol4_list_listing_matches_sbl__p_chain, which this engine did not list)
              (if ctl
                (let [segs (if (or (card l "TITLE") (card l "STITL")) [l] (map #(if (string/starts-with? % "-") % (str "-" %)) (string/split l #",-")))]
                  (doseq [c segs]
                    (cond
                      (card c "LIST")    (swap! st assoc :on true)
                      (card c "NOLIST")  (swap! st assoc :on false)
                      (card c "PRINT")   (swap! st assoc :print true)
                      (card c "NOPRINT") (swap! st assoc :print false)
                      (card c "SINGLE")  (swap! st assoc :double false)
                      (card c "DOUBLE")  (swap! st assoc :double true)
                      (card c "TITLE")   (swap! st assoc :title (string/replace (subs c 6) #"\A[ \t]+|[ \t]+\z" "") :subtitle "" :pending true)
                      (card c "STITL")   (swap! st assoc :subtitle (string/replace (subs c 6) #"\A[ \t]+|[ \t]+\z" "") :pending true)
                      (card c "EJECT")   (when (:on @st) (when-not (open! st) (new-page! st)))
                      (card c "SPACE")   (when (and (:on @st) (not (open! st)))
                                           (let [k (if-let [[_ d] (re-find #"^-\S+\s+(\d+)" c)] (Long/parseLong d) 1)]
                                             (if (< (+ (long (:on-page @st)) k) page-lines)
                                               (do (print (apply str (repeat k \newline))) (swap! st update :on-page + k))
                                               (new-page! st))))
                      :else              nil))
                  (shown l))
                (shown l))
              ;; a syntax error names its column too, and a line under the statement marks it with a ! (sbl -bf: "t.sno(2,6) : ERROR 232 -- ...", the marker
              ;; keeping the tabs of the line before the column, a blank for any other character)
              ;; a line with several errors is listed once, before the first, and a later error has an empty line in its place, on both streams (MEASURED on sbl -bf:
              ;; "        X = ) ; Y = ( ; Z = 1 +" lists the line, then 224's mark and message, then 226's; corpus tests/snobol4 compile_diag__p22)
              (doseq [[k [_ code col]] (map-indexed vector (get errs i))]
                ;; a control card is no statement and carries no number (MEASURED on sbl -bf: a -INCLUDE that cannot be opened is listed with none)
                (let [stmt (numbered (when-not ctl (some-> (first (rsubseq starts <= i)) val)) depth l)
                      mark (if col
                             ;; the field, then the run to the column, never trimmed: numbered drops a line's trailing blanks, and the run is all blanks
                             ;; the field is blank, the include depth not shown on the marker's line (MEASURED on sbl -bf: corpus tests/snobol4 compile_diag__p37,
                             ;; an error in an included file, and p15, after one that could not be opened, mark "                    !" under "2    1          Z = )")
                             (str (numbered nil 0 "") (apply str (map #(if (= \tab %) \tab \space) (take (dec (long col)) l))) "!")
                             "")
                      msg  (if col
                             (format "%s(%d,%d) : ERROR %03d -- %s" file fline col code (error-message code))
                             (format "%s(%d) : ERROR %03d -- %s" file fline code (error-message code)))]
                  (when (and (zero? (long k)) (not (:on @st))) (list-line! st stmt))
                  ;; the empty line standing for a later error's listed line is no line of the page (MEASURED on sbl -bf: corpus gate_cases_snobol4
                  ;; test_gate_s4e_donewhen_runs_the_whole_criterion__w, whose page 3 holds two such lines and breaks one statement later than a count of them would)
                  (when (pos? (long k)) (write! ""))
                  (doseq [x [mark msg ""]] (write! x))
                  (swap! st update :on-page + 3)
                  (binding [*out* *err*] (doseq [x [(if (zero? (long k)) stmt "") mark msg ""]] (write! x)))))
              ;; the listing ends with the END line, END and a blank or nothing after it: END and a carriage return is a label of its own, and SPITBOL lists on
              ;; (MEASURED on sbl -bf: corpus spitbol_88 DEMOS/TREESORT, a CR LF file, lists its line 83 after "END\r" with 214; \z, since a regex $ stands before a CR)
              ;; an -INCLUDE card still standing names a file that could not be opened, and SPITBOL lists the lines after it one level deeper (compiler,
              ;; preprocess-includes*)
              (when-not (and n (re-find (if fold #"(?i)^END([ \t]|\z)" #"^(END|end)([ \t]|\z)") l))
                (recur (inc i) (rest ls) (if (and ctl (re-find #"^-INCLUDE\s+['\"]" l)) (inc depth) depth) places file fline cards)))))))
    (flush)))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; An execution error's report, in sbl -bf's layout
;;
;; The report of an execution error that ends the program, in SPITBOL's layout (measured against sbl -bf).
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; "With the first type of execution error, SPITBOL will send a message to the screen, much as it does with compilation errors ... program.spt(3) : Error 038 --
;; Goto undefined label" (manual ch. 13, Execution errors). sbl -bf writes it on standard output: three empty lines, file(line) : ERROR nnn -- text, five empty
;; lines, where the error was and the run's figures, each labelled to column 21 (sbl.min stopr), and an empty line. The error line and the two empty lines after
;; it, the place and the figures down to REGENERATIONS, and the last empty line go to standard error as well, line by line. Each line is cut at 120 columns, and
;; of a line cut in pieces only the last goes to standard error. A negative &STLIMIT leaves out the statement count and the time (stopr: "skip if negative"); the
;; rates sbl adds after a run of a millisecond or more (stmt / microsec and the rest) measure the engine's speed, not the program's, and are not written.

(defn- row [label v] (str (format "%-21s" label) v))

(defn- regenerations [] (reduce + 0 (map #(max 0 (.getCollectionCount ^GarbageCollectorMXBean %)) (ManagementFactory/getGarbageCollectorMXBeans))))

(defn- report-lines
  "[line also-to-standard-error?] pairs: the report of error code with text, after stmts statements executed in the run that started at start-ns."
  [code text stmts start-ns]
  (let [rt   (Runtime/getRuntime)
        used (- (.totalMemory rt) (.freeMemory rt))
        file @&FILE]
    (concat [["" false] ["" false] ["" false]
             [(format "%s(%d) : ERROR %03d -- %s" file @<STMT-LINE> code text) true]
             ["" true] ["" true] ["" false] ["" false] ["" false]
             [(row "in file" file) true]
             [(row "in line" @<STMT-LINE>) true]
             [(row "in statement" @&STNO) true]]
            (when-not (neg? (long @&STLIMIT))
              [[(row "stmts executed" stmts) true]
               [(row "execution time msec" (quot (- (System/nanoTime) start-ns) 1000000)) true]])
            [[(row "REGENERATIONS" (regenerations)) true]
             [(row "memory used (bytes)" used) false]
             [(row "memory left (bytes)" (- (.maxMemory rt) used)) false]
             ["" true]])))

(defn report!
  "Write the report of error code with text on *out*, and its share on *err*, as sbl -bf does."
  [code text stmts start-ns]
  (doseq [[l both?] (report-lines code text stmts start-ns)]
    (let [width  (long @<WIDTH>)
          pieces (if (<= (count l) width) [l] (map #(apply str %) (partition-all width l)))]
      (doseq [p pieces] (println p))
      (when both? (binding [*out* *err*] (println (last pieces)))))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; TRACE, STOPTR, &TRACE and &FTRACE
;;
;; SNOBOL4 TRACE / STOPTR / &TRACE as SPITBOL (sbl -bf) does them.
;;
;; TRACE(name, type [, label [, fn]])
;;   Register a trace trigger on `name`. The type is read in any case by its first letter:
;;   V(ALUE) fires when the named variable is assigned a value, A(CCESS) each time its value is referenced. A name is read in the case it is written.
;;   L(ABEL), C(ALL), R(ETURN), F(UNCTION = call and return), K(EYWORD) are registered too; a type that begins with
;;   another letter is ERROR 199.
;;
;; STOPTR(name, type)
;;   Remove the trace registration for (name, type).
;;
;; &TRACE  (env/&TRACE atom)
;;   A count the program sets. A trace line is written only while it is above 0, and each line costs 1. TRACE and
;;   STOPTR do not touch it.
;;
;; ## Output
;;
;; A trace line goes to the program's output (stdout):  four asterisks and the number of the statement, filled with
;; asterisks to 12 characters, two blanks, then the text. A VALUE trace's text is `name = value`, the value as a dump
;; shows it: a string in quotes exactly as it is, a NAME with a point, a real with its point, PATTERN for a pattern.
;;
;; Other trace types keep the form they had until they are measured against the oracle.
;;
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── Registry ───────────────────────────────────────────�
;; Maps [name kind] -> {:label lbl :fn fn-name}. A name is kept as written; a kind is :value :access :label :call :return :keyword.

(def ^:private registry <TRACES>)

(defn- trace-kinds
  "The kinds a trace type registers, by its first letter in any case: V value, A access, L label, C call, R return, F both of a function's, K keyword.
   Any other letter is ERROR 199, trace second argument is not trace type."
  [type]
  (case (first (clojure.string/upper-case (str type)))
    \V [:value]
    \A [:access]
    \L [:label]
    \C [:call]
    \R [:return]
    \F [:call :return]
    \K [:keyword]
    (throw (ex-info "trace second argument is not trace type" {:snobol/error 199}))))

(defn- reg-key [name kind] [(str name) kind])

;; A trace of an element, TRACE(.A[2], 'VALUE'), is of that element of the array or table A held when the name was made: its entries are kept apart, each with
;; its container (compared by identity), its subscripts and its text as SPITBOL writes it, A<2> or A<2,5> or T<'k'> (MEASURED on sbl -bf: corpus tests/snobol4
;; ladder__rung23_trace_element, A[2] = 'x' then A[1] = 'y' then A[2] = 'z' writes "****4*******  A<2> = 'x'" and "****6*******  A<2> = 'z'", each costing 1
;; of &TRACE; rung23_trace_element_two_dimensional writes "A<2,5> = 7"; this engine registered the name's text and never fired it)
(def ^:private elements SNOBOL4clojure.env/<ETRACES>)
(defn- element-of [name]
  (when (instance? NAME name)
    (let [n (.n ^NAME name)]
      (when-let [c (and (seq? n) (:container (meta n)))]
        [c (vec (rest n)) (str (first n) "<" (clojure.string/join "," (map #(if (string? %) (str "'" % "'") (str %)) (rest n))) ">")]))))

;; ── Public registration API ──────────────────────────────────────�

(defn trace!
  "Register a trace. Called by INVOKE's TRACE branch.
   name  — variable/label/function name (a string, a symbol or a NAME)
   type  — trace type: VALUE LABEL CALL RETURN FUNCTION KEYWORD, or by its first letter(s)
   label — optional goto label (string/keyword) when trace fires
   fn    — optional user function name to call when trace fires"
  ([name type]       (trace! name type nil nil))
  ([name type label] (trace! name type label nil))
  ([name type label user-fn]
   (let [kinds (trace-kinds type)]
     ;; a KEYWORD trace names one of the three keywords SPITBOL traces, in upper case, as a keyword is always written (snobol4jvm never folds case, Lon 2026-11-17
     ;; 18:2x CST): any other name is "198 trace first argument is not appropriate name" (MEASURED on sbl -bf: TRACE('ERRTYPE','K'), TRACE('FNCLEVEL','K') and
     ;; TRACE('STCOUNT','k') take; TRACE('stcount','K'), TRACE('stcount','KEYWORD'), TRACE('STLIMIT','K'), TRACE('TRIM','K') and TRACE('ZZZ','K') are 198), where
     ;; this engine registered the name and never fired it (corpus csnobol4_suite t: TRACE('stcount', 'k'))
     (when (and (some #{:keyword} kinds) (not (#{"ERRTYPE" "FNCLEVEL" "STCOUNT"} (str name))))
       (throw (ex-info "trace first argument is not appropriate name" {:snobol/error 198})))
     (if-let [[c ks text] (element-of name)]
       (doseq [kind kinds]
         (swap! elements (fn [es] (conj (vec (remove #(and (identical? c (:c %)) (= ks (:ks %)) (= kind (:kind %))) es))
                                        {:c c :ks ks :kind kind :text text :label label :fn user-fn}))))
       (doseq [kind kinds]
         (swap! registry assoc (reg-key name kind) {:label label :fn user-fn}))))
   nil))

(defn stoptr!
  "Remove a trace registration. Called by INVOKE's STOPTR branch."
  [name type]
  (if-let [[c ks _] (element-of name)]
    (let [kinds (set (trace-kinds type))]
      (swap! elements (fn [es] (vec (remove #(and (identical? c (:c %)) (= ks (:ks %)) (kinds (:kind %))) es)))))
    (doseq [kind (trace-kinds type)]
      (swap! registry dissoc (reg-key name kind))))
  nil)

(defn clear-all-traces!
  "Remove every trace registration. Resets &TRACE to 0."
  []
  (reset! registry {})
  (reset! elements [])
  (reset! &TRACE 0)
  nil)

(defn active?
  "True if a trace of the given type is registered for name."
  [name type]
  (boolean (some #(contains? @registry (reg-key name %)) (trace-kinds type))))

(defn any-active?
  "True if any trace of any type is currently registered."
  []
  (pos? (count @registry)))

;; ── Trace output ──────────────────────────────────────────────────────────────

(def ^:dynamic *trace-output*
  "Where trace output goes. Default: the program's output (*out*), as in SPITBOL.
   Rebind to a writer in tests to capture trace output."
  nil)

;; A trace line is cut into pieces of 120 columns, each on a line of its own, as sbl -bf writes a long one (the call of a function given a long string).
(defn- trace-println [& args]
  (let [w (or *trace-output* *out*)
        l (apply str args)]
    (binding [*out* w]
      (if (<= (count l) 120)
        (println l)
        (doseq [p (partition-all 120 l)] (println (apply str p)))))))

(defn- star-field
  "Four asterisks and the number of the statement being executed, filled with asterisks to 12 characters."
  []
  (let [s (str "****" @&STNO)]
    (str s (apply str (repeat (max 0 (- 12 (count s))) \*)))))

(defn- trace-val
  "A value as a dump shows it: a string in quotes exactly as it is, a NAME with a point, a real with its point, PATTERN for a pattern, an array, a table or a
   DATA object by its kind and number (sbl -bf: N = ARRAY(2:4) #2)."
  [v]
  (cond
    (or (SNOBOL4clojure.env/array? v) (SNOBOL4clojure.env/table? v) (instance? DataObj v)) (value-text v)
    (nil? v)                  "''"
    (string? v)               (str "'" v "'")
    (instance? NAME v)        (str "." (.n ^NAME v))
    (integer? v)              (str v)
    (double? v)               (real-str v)
    (and (or (list? v) (vector? v)) (= "PATTERN" (SNOBOL4clojure.env/DATATYPE v))) "PATTERN"
    :else                     (str v)))

(defn- emit-line!
  "Write one trace line if &TRACE is above 0, and charge it 1."
  [text]
  (when (pos? @&TRACE)
    (swap! &TRACE dec)
    (trace-println (star-field) " " text)))

;; "Name2 is the optional name of a program-defined function that will be called when a trace occurs. It is called with two arguments: the name of the item being
;; traced, and argument s2" (manual ch. 19, TRACE); "When a program-defined trace function is invoked, keywords &TRACE and &FTRACE are temporarily set to zero.
;; Their values are restored when the trace function returns" (ch. 10, Debugging). As sbl -bf has it: the function is called in place of the trace line and costs 1
;; of &TRACE as a line does; the item's name is a NAME. A function name that names no defined function leaves the line.
;; A system function is a trace function too, called as a program calls it (MEASURED on sbl -bf: TRACE('X','VALUE','','IDENT') writes no line for X = 1 or
;; X = 'two'; corpus gate_cases_snobol4 a_value_trace_callback_is_entered_by_the_assignment_box_and_not_by_c__e, where this engine wrote both lines).
(defn- trace-fn [entry]
  (when-some [f (:fn entry)]
    (let [n (if (instance? NAME f) (str (.n ^NAME f)) (str f))]
      (or (get @<FUNS> n)
          (when (contains? SNOBOL4clojure.env/system-functions n)
            (let [invoke (ns-resolve (the-ns 'SNOBOL4clojure.operators) 'INVOKE)] (fn [& args] (apply @invoke (symbol n) args))))))))

;; SPITBOL sets &TRACE alone to zero: the trace function is a function called like any other, so &FTRACE traces its call and return and is charged for them, and
;; is not given back (MEASURED on sbl -bf: &TRACE = 100, &FTRACE = 50 and a VALUE trace calling CB writes "****6*******  CB(.X,'tg')", CB reads &TRACE 0 and
;; &FTRACE 49, its RETURN line follows, and the statement after reads 99 and 48; corpus gate_cases_snobol4 a_trace_callback_is_ftraced_like_any_called_function)
;; and &TRACE is given back when the function returns, never when an error ends the run inside it (MEASURED on sbl -bf: the statement limit reached inside an
;; STCOUNT trace function dumps &TRACE = 0; corpus spitbol_88 DEMOS/GOTOS)
(defn- call-trace-fn! [f item-name entry]
  (swap! &TRACE dec)
  (let [tr @&TRACE]
    (reset! &TRACE 0)
    (let [r (f (NAME. (symbol (str item-name))) (:label entry))]
      (reset! &TRACE tr)
      ;; and SPITBOL does not give &LINE back after a trace function, as it does after a plain call: &LINE reads 2, wherever the function stands, while &STNO is
      ;; the traced statement's again and &LASTNO and &LASTLINE the trace function's last (MEASURED on sbl -bf: a trace function of one statement on line 2, of
      ;; two on lines 7 and 8, and of three on 9 to 11 each leave &LINE 2 in the statement traced, &LASTLINE 2, 8 and 11; HYPOTHESIS: the 2 is the count of the
      ;; two arguments every trace function is given; corpus spitbol_88 DEMOS/GOTOS, whose dump reads &LASTLINE = 2 after its last trace)
      (reset! SNOBOL4clojure.env/&LINE 2)
      r)))

;; ── Fire helpers — called from runtime / operators / functions ────────────────

(defn fire-value!
  "Fire VALUE trace for `sym` being assigned `new-val`. Called from operators.clj INVOKE '= after the assignment. The line is the statement's star field, an i
   for each function level, then name = value, as sbl -bf writes it (****4******* i G = 3 for an assignment in a function called from level 0)."
  [sym new-val]
  (when-let [entry (@registry (reg-key sym :value))]
    (if-let [f (and (pos? @&TRACE) (trace-fn entry))]
      (call-trace-fn! f sym entry)
      (emit-line! (str (apply str (repeat @&FNCLEVEL \i)) " " sym " = " (trace-val new-val))))
    (:label entry)))

(defn fire-element!
  "Fire the VALUE trace of the element of container c at subscripts ks, just assigned new-val (operators.clj, the element assignment), as fire-value! does for a
   variable; nothing when no element trace is registered."
  [c ks new-val]
  (when-let [e (and (seq @elements) (first (filter #(and (= :value (:kind %)) (identical? c (:c %)) (= (vec ks) (:ks %))) @elements)))]
    (if-let [f (and (pos? @&TRACE) (trace-fn e))]
      (call-trace-fn! f (:text e) e)
      ;; the element is named by the variable that holds its container now, as SPITBOL names it (env/holder-name: sbl -bf writes B<1> once B alone holds the
      ;; array traced as A[1]), else by the name it was traced by
      (emit-line! (str (apply str (repeat @&FNCLEVEL \i)) " "
                       (if-let [h (SNOBOL4clojure.env/holder-name c)] (str h (subs (:text e) (.indexOf ^String (:text e) "<"))) (:text e))
                       " = " (trace-val new-val))))
    (:label e)))

(defn fire-access!
  "Fire the ACCESS trace of the variable `sym`, whose value `v` a statement has just fetched: \"Produces a trace every time the named item is referenced. If you say
   X = N, that does not change the value of N, but it is a reference\" (manual ch. 10, Debugging). As sbl -bf writes it: the referencing statement's star field, an
   i for each function level, then name = value; or the trace function is called. Either costs 1 of &TRACE. Called only while &TRACE is above 0."
  [sym v]
  (when-let [entry (@registry (reg-key sym :access))]
    (if-let [f (trace-fn entry)]
      (call-trace-fn! f sym entry)
      (do (swap! &TRACE dec)
          (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " " sym " = " (trace-val v))))))

(defn fire-keyword!
  "Fire the KEYWORD trace of the keyword named kw (ERRTYPE, FNCLEVEL or STCOUNT, the ones SPITBOL traces) now holding new-val: the trace function is called, or the
   line written as sbl -bf writes it, the statement's star field, an i for each function level, then &NAME = value; either costs 1 of &TRACE."
  [kw new-val]
  (when (pos? @&TRACE)
    (when-let [entry (@registry (reg-key kw :keyword))]
      (if-let [f (trace-fn entry)]
        (call-trace-fn! f kw entry)
        (do (swap! &TRACE dec)
            (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " &" kw " = " (trace-val new-val)))))))

(defn fire-label!
  "Fire the LABEL trace of `label-key`, a Goto transferring to it: \"Produce a trace when a Goto transfer to statement name occurs. Flowing sequentially into the
   labeled statement ... does not produce a trace\" (manual ch. 10). Called from runtime.clj's Goto while &STNO is still the statement whose Goto it is; the line
   is its star field, an i for each function level, then :(L), as sbl -bf writes it, or the trace function is called; either costs 1 of &TRACE."
  [label-key]
  (when (pos? @&TRACE)
    (let [lname (if (keyword? label-key) (name label-key) (str label-key))]
      (when-let [entry (@registry (reg-key lname :label))]
        (if-let [f (trace-fn entry)]
          (call-trace-fn! f lname entry)
          (do (swap! &TRACE dec)
              (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " :(" lname ")")))))))

(defn- function-line!
  "A call or return line of the function `fname`. It is written when the function is registered for `kind` and &TRACE is above 0 (and costs 1 of &TRACE),
   or else when &FTRACE is above 0 (and costs 1 of &FTRACE). Four asterisks and the statement number, a blank, one i for each function level the caller is
   inside, a blank, then the text. The text is a thunk, built only when the line is written: a call's text quotes each argument whole, so building it on
   every call copied every string argument of every call (MEASURED: corpus mikebeer_snobol bench/recursion, whose PERM doubles its string argument each level,
   ran the JVM out of heap in trace-val, with no trace armed, where sbl -bf ends at ERROR 205)."
  [fname kind text & [caller-stno]]
  (let [line  #(trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " " (text))
        entry (@registry (reg-key fname kind))]
    ;; a trace function called in place of the line leaves &FTRACE its own line (MEASURED on sbl -bf: TRACE('F','CALL','tg',.CB) with &FTRACE on writes CB's
    ;; call, CB's lines and return, then "****8*******  F(1)"; a TRACE line alone is the only line, &FTRACE not charged; corpus gate_cases_snobol4
    ;; a_trace_callback_is_ftraced_like_any_called_function__call)
    (let [f (when (and entry (pos? @&TRACE)) (trace-fn entry))]
      (cond
        ;; and the line after a RETURN trace function is the caller's statement's (MEASURED on sbl -bf: F's return at statement 7, called from 9, writes
        ;; "****7******* i CB(.F,'tg')" then "****9******* i RETURN F = 2"; corpus gate_cases_snobol4
        ;; a_return_trace_callback_prints_the_deferred_banner_with_the_callers_statement_number__nest)
        f                          (do (call-trace-fn! f fname entry)
                                       (when caller-stno (reset! &STNO caller-stno))
                                       (when (pos? @&FTRACE) (swap! &FTRACE dec) (line)))
        (and entry (pos? @&TRACE)) (do (swap! &TRACE dec) (line))
        (pos? @&FTRACE)            (do (swap! &FTRACE dec) (line))))))

(defn fire-call!
  "Fire the call trace of user function `fname`, whose parameters hold `args`. Called from the DEFINE wrapper before the body runs, at the caller's
   function level and with the number of the calling statement."
  [fname args]
  (function-line! fname :call #(str fname "(" (clojure.string/join "," (map trace-val args)) ")")))

(defn fire-return!
  "Fire the return trace of user function `fname` returning `val`. signal is :return :freturn or :nreturn. Called from the DEFINE wrapper once the level is
   the caller's again and while the number of the returning statement is still &STNO."
  [fname val signal & [caller-stno]]
  (function-line! fname :return
                  #(case signal
                    :freturn  (str "FRETURN " fname)
                    :nreturn  (str "NRETURN " fname " = " (trace-val val))
                    (str "RETURN " fname " = " (trace-val val)))
                  caller-stno))

(defn pattern-trace-active?
  "True if PATTERN trace is registered for `subject-var` (or globally via \"*\").
   Used by match.clj to decide whether to bind *trace* true."
  [subject-var]
  (and (pos? @&TRACE)
       (or (@registry (reg-key subject-var :pattern))
           (@registry (reg-key "*" :pattern)))))

;; ── &TRACE as a simple on/off toggle (convenience, non-standard extension) ────
;; Setting &TRACE = 1 with no registrations enables a default statement trace.
;; Setting &TRACE = 0 suppresses all output even if registrations exist.

;; True while an STCOUNT trace is armed, whatever &TRACE holds (sbl.min stgcc: "jump if not stcount tracing" reads the trace block alone)
(defn stcount-traced? [] (boolean (@registry (reg-key "STCOUNT" :keyword))))

(defn statement-trace-active?
  "True if &TRACE > 0 AND a global statement trace (name \"*\" type \"LABEL\")
   is registered. This is our extension for full statement-by-statement tracing."
  []
  (and (pos? @&TRACE)
       (@registry (reg-key "*" :label))))

(defn enable-full-trace!
  "Convenience: register a global statement+value trace. Sets &TRACE > 0.
   Equivalent to the test helper calling:
     (trace! \"*\" \"LABEL\") (trace! \"*\" \"VALUE\") (trace! \"*\" \"CALL\")"
  []
  (trace! "*" "LABEL")
  (trace! "*" "VALUE")
  (trace! "*" "CALL")
  (trace! "*" "RETURN"))

(defn disable-full-trace!
  "Convenience: clear the global wildcard traces."
  []
  (stoptr! "*" "LABEL")
  (stoptr! "*" "VALUE")
  (stoptr! "*" "CALL")
  (stoptr! "*" "RETURN"))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; The built-in functions
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── String functions ──────────────────────────────────────────────────────────

(defn- string->real
  "The real a string denotes, or nil (failure). SNOBOL4's syntax, measured
   with sbl -bf: blanks at the ends ignored, the null string is 0., then an
   optional sign, digits with an optional point, and an optional exponent
   marked e, E, d or D. A value out of range fails."
  [x]
  (let [t (clojure.string/trim (str x))]
    (cond
      (or (clojure.core/= t "") (sign-then-blanks? x)) 0.0
      ;; an integer's digits are an integer first (one too large for 64 bits is no number), anything else a real as gtnum reads it
      (re-matches #"[+-]?\d+" t)
      (try (double (Long/parseLong t)) (catch NumberFormatException _ nil))
      ;; anything else is gtnum's to read, untrimmed: a blank after an exponent marker makes it the exponent 0 (env/parse-real)
      :else (parse-real (str x)))))

(defn- real->integer
  "The integer a real becomes: truncated toward zero. A real outside the
   integer range is the least integer, as sbl -bf gives it (the x86-64
   conversion), not a failure."
  ;; and a real that truncates to 2147483648, 2**31, converts to no integer: sbl's rti fails there and nowhere else (MEASURED on sbl -bf: CONVERT(2147483648.5,
  ;; 'INTEGER'), 2147483648.1 and 2147483648.0 fail, 2147483647.5 and 2147483649.5 convert, -2147483648.5 gives -2147483648, 1.5e19 the least integer;
  ;; corpus gate_cases_snobol4 convert_reads_a_numeric_string_as_spitbol_gtnum_does__w)
  [x]
  (let [lo (double Long/MIN_VALUE)]
    (if (and (clojure.core/<= lo x) (clojure.core/< x (clojure.core/- lo)))
      (let [i (long x)] (when-not (clojure.core/= i 2147483648) i))
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
                     (when-some [d (string->real x)] (real->integer d))))))

(defn- int-arg [x err]
  (if-some [n (integer-of x)] n (throw (ex-info "argument is not integer" {:snobol/error err}))))


;; A string argument as SPITBOL converts it (sbl.min gtstg): an integer as its digits, a real "converted to its standard representation" (manual ch. 17, Real to
;; string: 21. for 21.0, 2.71828182845905 for e), the name of a natural variable as its name, the null string for nil; anything else is the function's error n for
;; that argument. The arguments are converted in sbl.min's order, the last first (sbl -bf: REPLACE(P, T, N) of a pattern, a table and a keyword's name is 168,
;; LPAD(P, 3, T) 144, SIZE(LEN(1)) 189, TRIM(TABLE()) 200).
(defn- str-arg ^String [x n] (gtstg x n))

(defn SIZE    [s]         (long (count (str-arg s 189))))
(defn REPLACE [s1 s2 s3]
  ;; Replace chars in s1: each char of s2 mapped to corresponding char of s3. The third and second arguments convert first, then a second argument that is null or
  ;; not as long as the third is "171 null or unequally long 2nd, 3rd args to replace", before the first converts (sbl.min s_rpl: srpl6; sbl -bf: REPLACE('abc',
  ;; 'ab', 'x'), REPLACE('abc', '', '') and REPLACE(ARRAY(1), 'a', 'bc') are 171, REPLACE(ARRAY(1), 'a', 'b') is 170)
  (let [s3 (str-arg s3 168) s2 (str-arg s2 169)
        _  (when (or (zero? (count s2)) (not= (count s2) (count s3)))
             (throw (ex-info "null or unequally long 2nd, 3rd args to replace" {:snobol/error 171})))
        s1 (str-arg s1 170)]
    ;; a character the second argument holds twice is replaced as its last place says, SPITBOL's table being filled from the first to the last (MEASURED on sbl
    ;; -bf: with f = a..z and its 14th made z, REPLACE('lazy', f, 'ZYX...A') gives OZAB, z taking A, the 26th; corpus gate_cases_snobol4
    ;; replace_is_an_asm_leaf_that_agrees_with_sbl_and_its_c_twin__rep)
    (apply str (map #(let [i (.lastIndexOf ^String s2 (str %))]
                       (if (>= i 0) (.charAt ^String s3 i) %))
                    s1))))
;; DUPL's first argument is a string or a pattern, "091 dupl first argument is not a string or pattern": a pattern, or an expression, repeated is the pattern of
;; its copies in turn, and repeated no times the null pattern; a negative count fails (sbl -bf: 'abcd' ? DUPL(LEN(1), 3) . V assigns abc; DUPL(LEN(1), 0) is a
;; PATTERN; 'aab' ? DUPL(*X, 2) . V of X = 'a' assigns aa; DUPL('ab', -1) fails)
(defn DUPL    [x i]
  (let [n (int-arg i 90)]
    (when-not (neg? n)
      (if (#{"PATTERN" "EXPRESSION"} (DATATYPE x))
        (if (zero? n) (list 'LEN# 0) (apply list 'SEQ (repeat n x)))
        ;; a count past 2**24 fails, whatever &MAXLNGTH is, and a string longer than &MAXLNGTH is 205 (MEASURED on sbl -bf: DUPL('', 16777217) and
        ;; DUPL('a', 16777217) fail at &MAXLNGTH 1024 and at its default, DUPL('', 16777216) is null; DUPL('a', 1025) at &MAXLNGTH 1024 and DUPL('ab', 8388609)
        ;; at the default are "205 string length exceeds value of maxlngth keyword", DUPL('ab', 8388608) is 16777216 long)
        (let [s (str-arg x 91) m (long @&MAXLNGTH)]
          (cond (clojure.core/> n 16777216)                          nil
                (clojure.core/> (clojure.core/* n (count s)) m)      (throw (ex-info "string length exceeds value of maxlngth keyword" {:snobol/error 205}))
                :else                                                (apply str (repeat n s))))))))
;; TRIM drops the trailing blanks and tabs, and no other character (MEASURED on sbl -bf: SIZE(TRIM('abc' CHAR(9))) and of 'abc ' CHAR(9) ' ' are 3; 'abc' and a
;; CR, LF, VT or FF after it keep 4; corpus gate_cases_snobol4 trim_drops_a_trailing_tab_and_scrips_own_builtins_are_undefined__tr)
(defn TRIM    [s]     (let [^String t (str-arg s 200)] (subs t 0 (loop [i (count t)] (if (and (pos? i) (#{\space \tab} (.charAt t (dec i)))) (recur (dec i)) i)))))
(defn REVERSE [s]     (apply str (clojure.string/reverse (str-arg s 177))))
(defn LPAD
  ([s n]        (LPAD s n " "))
  ([s n fill]   (let [f      (str-arg fill 144)
                      w      (int-arg n 145)
                      s-str  (str-arg s 146)
                      pad    (clojure.core/- w (count s-str))
                      fill-c (if (clojure.core/= "" f) " " (subs f 0 1))]   ; a null fill pads with blanks
                  (if (clojure.core/<= pad 0) s-str
                    (str (apply str (repeat pad fill-c)) s-str)))))
(defn RPAD
  ([s n]        (RPAD s n " "))
  ([s n fill]   (let [f      (str-arg fill 178)
                      w      (int-arg n 179)
                      s-str  (str-arg s 180)
                      pad    (clojure.core/- w (count s-str))
                      fill-c (if (clojure.core/= "" f) " " (subs f 0 1))]   ; a null fill pads with blanks
                  (if (clojure.core/<= pad 0) s-str
                    (str s-str (apply str (repeat pad fill-c)))))))
(defn SUBSTR
  "SUBSTR(s, i, n): the n characters of s from the 1-based position i, where n = 0 means all the rest of s from i. It fails when i is not from 1 to one past
   the end of s, when n is negative, or when the n characters are not all there, as sbl -bf does."
  [s i n]
  (let [n (int-arg n 192) i (int-arg i 193) s (str-arg s 194) len (count s)]
    (when (and (clojure.core/<= 1 i (inc len)) (clojure.core/>= n 0))
      (if (zero? n)
        (subs s (dec i))
        (when (clojure.core/<= (clojure.core/+ (dec i) n) len)
          (subs s (dec i) (clojure.core/+ (dec i) n)))))))

;; A table as an array: "The table is converted to a two-dimensional array ... The table must have at least one non-null element. If the table is empty, CONVERT,
;; RSORT and SORT will fail" (manual ch. 17, Table to array). As sbl -bf has it, an entry whose value is null is no element: N rows of key and value, in the order
;; the keys were first assigned, for the N entries with a value; nil (failure) when there are none.
(defn table->array
  ([t] (table->array t false))
  ([t sorting?]
  (let [entries (remove (fn [[_ v]] (or (nil? v) (clojure.core/= "" v))) (table-entries t))
        ;; SORT and RSORT take a table's entries as SPITBOL's gtarr gives them to its sort, its buckets from the last to the first, each bucket's chain in the
        ;; order its keys were made, and the sort keeps equal keys in that order (sbl.min sorta, sortc: "if equal, compare key offsets to give stable sort";
        ;; MEASURED on sbl -bf: TABLE() with t B u a r = 4 and x = 1 sorts on the values as x a t u r B, RSORT as a t u r B x, where CONVERT gives t B u a r x;
        ;; TABLE(1) with p q r s = 1 keeps p q r s both ways; corpus slobol_tests test/cfreq, RSORT(freq,2) of a TABLE(1000), whose equal counts sbl orders
        ;; t u r n i a B and this engine in the order the keys came)
        entries (if sorting?
                  (let [kh (deref (ns-resolve (the-ns 'SNOBOL4clojure.env) 'key-hash)) n (long (:size (meta t) 11))]
                    (sort-by (fn [[k _]] (if-some [h (kh k)] (clojure.core/- (mod h n)) 1)) entries))
                  entries)
        n       (count entries)]
    (when (pos? n)
      (let [arr (ARRAY (str n ",2"))]
        (doseq [[i [k v]] (map-indexed vector entries)]
          (array-set arr [(inc i) 1] k)
          (array-set arr [(inc i) 2] v))
        arr)))))

(defn CONVERT [x t]
  "CONVERT(x, type) — coerce x to the named SNOBOL4 type.
   Returns nil (failure) if the conversion is not defined."
  ;; "Data type names may be lower case if case-folding is active" (manual ch. 19, CONVERT): while &CASE is 0, as sbl -bf runs, only the capitals name a type
  ;; The type is a string: the null string, a pattern, an array or any object that is no string, number or name is "074 convert second argument is not a string"
  ;; (Appendix D; MEASURED on sbl -bf: CONVERT('1', ''), CONVERT('1', LEN(1)) and CONVERT('1', ARRAY(2)) are 74, CONVERT('1', 'FOO') and CONVERT('1', .Q) fail with
  ;; no error; corpus gate_cases_snobol4 a_builtin_called_short_or_over_its_arity_pads_with_null_and_drops_extras__cnv, where this engine failed with none)
  (when-not (or (and (string? t) (seq t)) (number? t) (instance? SNOBOL4clojure.env.NAME t))
    (throw (ex-info (error-message 74) {:snobol/error 74})))
  (let [from (clojure.string/upper-case (DATATYPE x))
        to   (if (zero? (long @&CASE)) (str t) (clojure.string/upper-case (str t)))]
    (cond
      ;; Same type → identity (but always a 'new' value logically)
      (clojure.core/= from to) x

      ;; "s can take an additional data type, 'NUMERIC', which converts the argument to integer or real, as appropriate" (manual ch. 19, CONVERT): a number is itself, a
      ;; string of a number's form its number (blanks around it ignored, the null string 0), anything else fails
      (clojure.core/= to "NUMERIC")
      (cond (number? x) x
            (string? x) (let [n (num x)] (when-not (and (double? n) (Double/isNaN n)) n))
            :else nil)

      ;; STRING conversions
      ;; Integer syntax is an integer (out of range fails); anything else is read
      ;; as a real and converted as one. nil propagates as failure.
      (and (clojure.core/= from "STRING") (clojure.core/= to "INTEGER"))
      (let [t (clojure.string/trim (str x))]
        (if (sign-then-blanks? x) 0 (if (re-matches #"[+-]?\d+" t)
          (try (Long/parseLong t) (catch NumberFormatException _ nil))
          (when-some [d (string->real x)] (real->integer d)))))

      (and (clojure.core/= from "STRING") (clojure.core/= to "REAL"))
      (string->real x)

      (and (clojure.core/= from "STRING") (clojure.core/= to "PATTERN"))
      ;; A string converts to a literal LIT$ pattern node, a PATTERN that an assignment keeps as one (primitives.clj LIT$ matches its text)
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
      (sstr x)

      (and (clojure.core/= from "REAL") (clojure.core/= to "INTEGER"))
      (real->integer x)

      (and (clojure.core/= from "REAL") (clojure.core/= to "PATTERN"))
      (list 'SEQ (list 'LIT$ (sstr x)))

      (and (clojure.core/= from "REAL") (clojure.core/= to "NAME"))
      (SNOBOL4clojure.env.NAME. (symbol (sstr x)))

      ;; ARRAY conversions
      (and (clojure.core/= from "ARRAY") (clojure.core/= to "TABLE"))
      ;; ARRAY must be Nx2; column 1 = key, column 2 = value.
      ;; Only succeeds if exactly 2 dimensions and second dim size is 2.
      (let [dims (:dims x)]
        (if (and (clojure.core/= (count dims) 2)
                 (clojure.core/= (second (second dims)) 2)
                 (clojure.core/= (first (second dims)) 1))
          (let [[lo1 hi1] (first dims)
                t (TABLE (inc (clojure.core/- hi1 lo1)))]
            (doseq [i (range lo1 (inc hi1))]
              (let [k (array-get x [i 1])
                    v (array-get x [i 2])]
                (when-not (nil? k)
                  (table-set t k (or v ε)))))
            t)
          nil))  ; wrong shape → fail

      ;; TABLE conversions
      (and (clojure.core/= from "TABLE") (clojure.core/= to "ARRAY"))
      (table->array x)

      ;; PATTERN stays PATTERN
      (and (clojure.core/= from "PATTERN") (clojure.core/= to "PATTERN")) x

      ;; NAME conversions (a NAME, or a symbol where the engine keeps one, reads as its variable's name). Only the name of a natural variable has text: the name of a
      ;; keyword, an element or an expression converts to no string, integer, real or pattern, and CONVERT fails (sbl.min gtstg; sbl -bf: CONVERT(.A[1], 'STRING')
      ;; and CONVERT(.&ERRTEXT, 'PATTERN') fail, CONVERT(.Y, 'INTEGER') fails whatever Y holds)
      (and (clojure.core/= from "NAME") (#{"STRING" "INTEGER" "REAL" "PATTERN"} to) (instance? SNOBOL4clojure.env.NAME x) (not (symbol? (.n ^SNOBOL4clojure.env.NAME x))))
      nil

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

      ;; an EXPRESSION to PATTERN is a pattern that evaluates it when matched, as *X is in a pattern (sbl -bf: CONVERT(*X, 'PATTERN') is a PATTERN, and
      ;; 'aqb' ? CONVERT(*X, 'PATTERN') . R matches X's value at the match)
      (and (clojure.core/= from "EXPRESSION") (clojure.core/= to "PATTERN")) (list 'SEQ x)

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
    ;; a pattern, an expression or code: a new one that works as it does, not IDENT to it (sbl -bf: IDENT(P, COPY(P)) fails, 'abc' ? COPY(LEN(1)) . V assigns a,
    ;; :<COPY(C)> runs C's statements); a string, a number or a name is its own copy
    (instance? SNOBOL4clojure.env.CodeBlock x) (SNOBOL4clojure.env/->CodeBlock (.entry ^SNOBOL4clojure.env.CodeBlock x))
    (and (seq? x) (#{"PATTERN" "EXPRESSION"} (DATATYPE x))) (with-meta (apply list x) (meta x))
    (and (vector? x) (clojure.core/= "PATTERN" (DATATYPE x))) (with-meta (into [] x) (meta x))
    :else x))

;; ── Type conversion & char functions ─────────────────────────────────────────
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
  (let [a (num x)
        b (num y)
        nan? #(and (double? %) (Double/isNaN ^double %))]
    ;; the second argument converts first, as an arithmetic operator's right operand (sbl -bf: REMDR('a', ARB) is 165)
    (when (nan? b) (throw (ex-info "remdr second argument is not numeric" {:snobol/error 165})))
    (when (nan? a) (throw (ex-info "remdr first argument is not numeric" {:snobol/error 166})))
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
(defn- real-error [code msg] (throw (ex-info (or (error-message code) msg) {:snobol/error code})))

(defn- real-arg
  "x as a double, or ERROR code when it is not numeric."
  [x code fname]
  (let [n (num x)]
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
    ;; the type's name and its fields are names the program has made, which &DUMP = 3 lists (env/<NAMES>)
    (when-let [[t fs] (parse-data-spec S)] (note-names! (cons t fs)))
    (eval form)
    ε))

;; FIELD(type, n): the nth field name of a type DATA defined, failing when n is out of range. The arguments are converted last first: n not an integer is "107
;; field second argument is not integer" (the null string is 0, a real or a numeric string its integer), then a type that is no defined datatype's name, or the
;; null string, is "108 field first argument is not datatype name" (MEASURED on sbl -bf: of DATA('NODE(V,W)'), FIELD('NOPE', 1), FIELD(5, 1), FIELD('STRING', 1)
;; and FIELD('', 1) are 108, FIELD('NODE', 'x') and FIELD('NOPE', 'x') 107, FIELD('NODE', 3) and FIELD('NODE', '') fail with nothing set, FIELD('NODE', '2') and
;; FIELD('NODE', 2.0) are W, FIELD(.NODE, 1) is V). snobol4jvm failed every one of these errors with nothing set.
(defn FIELD
  "FIELD(type-name, n) — return the nth field name of the program-defined type.
   n is 1-based. Returns nil on out-of-range."
  [type-name n]
  (let [n      (if (clojure.core/= "" (str n)) 0 (int-arg n 107))
        tname  (str type-name)
        fields (get @data-type-registry tname)]
    (when-not fields (throw (ex-info "field first argument is not datatype name" {:snobol/error 108})))
    (when (and (>= (long n) 1) (<= (long n) (count fields)))
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
  (let [line (try (.readLine (stdin-reader)) (catch Exception _ nil))]
    (if (nil? line) ε (input-record line))))

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
    (if (and *file-dir* (not (.isAbsolute f)))
      (java.io.File. (str *file-dir*) file-str)
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
  (let [{nm :name opts :opts} (file-spec (or filename ""))
        held (when-not (null-arg? unit) (get @<CHANNELS> (channel-key unit)))]
    ;; A unit the other direction holds is that direction's error, whatever the file argument says (MEASURED on sbl -bf: OUTPUT(.A,10,'u1.txt') then INPUT(.B,10,5)
    ;; or INPUT(.B,10,'u1.txt') is "289 input channel currently in use"; INPUT(.A,10,'t.txt') then OUTPUT(.B,10) is "161 output file cannot be written to"; corpus
    ;; csnobol4_suite update, INPUT(.bin, 10, 5) on the unit OUTPUT opened, ends at that 289, where this engine opened a second channel and ran on)
    (when (and held (not (clojure.core/= type (:type held))))
      (if (clojure.core/= type :input)
        (throw (ex-info "input channel currently in use" {:snobol/error 289}))
        (throw (ex-info "output file cannot be written to" {:snobol/error 161}))))
    ;; and a unit already open in this direction takes no second file: a file argument that is not null is "289 input channel currently in use", "290 output
    ;; channel currently in use" (MEASURED on sbl -bf: INPUT(.X,5,'in5.txt') then INPUT(.Y,5,'[-f0]'), INPUT(.Y,5,'in5.txt') or INPUT(.X,5,'[-f0]') is 289,
    ;; INPUT(.Y,5) and INPUT(.Y,5,'') share the unit; OUTPUT(.Y,6,'o2.txt') on an open 6 is 290; corpus gate_cases_snobol4 input_honours_the_f_and_l_file_options__reassoc)
    (when (and held (not (null-arg? filename)))
      (if (clojure.core/= type :input)
        (throw (ex-info "input channel currently in use" {:snobol/error 289}))
        (throw (ex-info "output channel currently in use" {:snobol/error 290}))))
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
              :else    (stdin-reader))]
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
   (let [fail   (fn [n] (throw (ex-info (error-message n) {:snobol/error n})))
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
    ;; every variable associated with the unit loses it, the unit's own variable and those attached after it alike (manual ch. 8, attaching to a channel)
    (swap! <CHANNELS> (fn [m] (into {} (remove (fn [[_ v]] (and (map? v) (some? (:unit ch)) (clojure.core/= (:unit v) (:unit ch)))) (dissoc m (:var ch) (:unit ch))))))
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
;; use and its high-water mark, are what a JVM can say of them: the first page and the heap's size above it, and the frames on the thread's stack. k omitted is 0.
;; Another selector is ERROR 254, erroneous argument for host, as sbl -bf raises it for the sub-functions this engine does not have.
;; EXIT(s) "will terminate program execution and request that the operating system perform the system command s"; EXIT(n,s) "creates a file that captures the
;; current state of program execution ... Argument n is -3 to create a save file, and +3 to create a load module. Other values are not permitted" (manual ch. 19,
;; EXIT). MEASURED on sbl -bf: EXIT('echo hi; exit 3') prints the program's output, then hi, and exits 3, &CODE and &DUMP not read; EXIT('false') exits 1, an
;; unknown command 127 (bash's); EXIT(-3, f) writes the save file f (a.spx when f is omitted) and exits 0; EXIT(3, f), EXIT(5) and EXIT() are "105 exit action not
;; available in this implementation". A save file is SPITBOL's own memory image, which no JVM can write, so this engine writes none and ends as sbl does; corpus
;; tests/snobol4 test_probe_loose_conformance_f65_exit and f65b_exit_savefile, where this engine said 022.
(defn EXIT
  ([] (EXIT nil nil))
  ([a] (EXIT a nil))
  ([a b & _]
   (let [n (when-not (string? a) (SNOBOL4clojure.env/num a))]
     (cond
       (and (string? a) (seq a))
       (do (flush)
           (let [p (-> (ProcessBuilder. ^java.util.List ["/bin/bash" "-c" a]) (.inheritIO) (.start)) code (long (.waitFor p))]
             (throw (ex-info "exit" {:snobol/signal :exit :snobol/rc code}))))
       (and (integer? n) (clojure.core/= -3 n))
       (throw (ex-info "exit" {:snobol/signal :exit :snobol/rc 0}))
       :else (throw (ex-info (error-message 105) {:snobol/error 105}))))))

(defn HOST
  ([] (str "jvm:" (System/getProperty "os.name") ":SNOBOL4clojure 0.2.0"))
  ([i & more]
   ;; A null first argument is HOST() whatever follows it, never HOST(0), though the null string converts to 0 elsewhere (MEASURED on sbl -bf: HOST(''), HOST('',),
   ;; HOST('','') and HOST('','x') each answer the identity, "x86-64:unix :Macro SPITBOL 15.01 #"; HOST(Y), Y unset, the same, so csnobol4_suite float2's
   ;; BITS = HOST(HOST_REAL_BITS) is that string and EQ(BITS,64) ends ERROR 101, where this engine answered HOST(0), the null string, and EQ failed quietly)
   (if (or (nil? i) (clojure.core/= "" i))
     (HOST)
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
             ;; the heap's base: a JVM has no address to give, and sbl's is above 0 (MEASURED on sbl -bf: GT(HOST(-1,2),0) succeeds; corpus gate_cases_snobol4
             ;; host_minus_one_answers_the_memory_statistics__w), so the base is the first page and the top the heap's size above it
             2 4096
             3 (clojure.core/+ 4096 (.totalMemory (Runtime/getRuntime)))
             4 (clojure.core/- 4194304 400)
             5 (frames)
             6 8
             7 (frames)
             (bad))))
       :else (bad))))))
;; SETEXIT(label) arms the label (a string or a NAME) as the error handler and returns the handler it replaces, as a NAME, or the null string when there was none.
;; The null string, or no argument, disarms. Anything else names a natural variable whose label is defined, else "187 setexit argument is not label name or null"
;; (sbl.min s_stx, gtnvr): a string folds as &CASE says at that moment (sbl -bf, under -f: with &CASE 1 SETEXIT('lab1') looks for LAB1), the name of a
;; variable does not, and a pattern or the name of an element is no variable. The system labels are always defined, in capitals (sbl -bf: SETEXIT(.CONTINUE)
;; and SETEXIT('RETURN') arm, SETEXIT('continue') is 187; END is END or end).
(def ^:private system-labels #{"ABORT" "CONTINUE" "SCONTINUE" "RETURN" "FRETURN" "NRETURN" "END" "end"})
(defn SETEXIT
  ([] (SETEXIT ε))
  ([label]
   (let [prev @<SETEXIT>
         s    (gtstg label 187)
         ;; the label as written: snobol4jvm never folds case, whatever &CASE holds (Lon 2026-11-17 18:2x CST, in the JVM window)
         s    s]
     (when-not (or (clojure.core/= "" s) (system-labels s) (@<STNO> (keyword s)))
       (throw (ex-info (error-message 187) {:snobol/error 187})))
     (reset! <SETEXIT> (when-not (clojure.core/= "" s) (keyword s)))
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
    (SNOBOL4clojure.env/table? c) (SNOBOL4clojure.env/table-get c (SNOBOL4clojure.env/table-subscript ks))
    (SNOBOL4clojure.env/array? c) (SNOBOL4clojure.env/array-get c (vec ks))
    :else nil))

(defn PROTOTYPE
  "PROTOTYPE(array): the prototype the array was made from (manual ch. 19). sbl -bf also answers for a table, with the number of its hash headers, an INTEGER (11 for
   TABLE()), and ignores a second argument."
  [x & _]
  (cond
    (SNOBOL4clojure.env/table? x) (:size (meta x) 11)
    (SNOBOL4clojure.env/array? x) (SNOBOL4clojure.env/array-prototype x)
    ;; anything else, a string, a number, a pattern or a DATA object, is "164 prototype argument is not valid object" (MEASURED on sbl -bf: PROTOTYPE(''),
    ;; PROTOTYPE('abc'), PROTOTYPE(3), PROTOTYPE(LEN(1)) and PROTOTYPE of a DATA object; corpus tests/snobol4 scrip_test_1113_table, PROTOTYPE of what a failed
    ;; CONVERT left null, where this engine answered the null string)
    :else (throw (ex-info "prototype argument is not valid object" {:snobol/error 164}))))

;; SPITBOL's sort order (sbl.min sortc): two strings compare as strings, two numbers as numbers; two objects of one other type by creation, "item first created is
;; less"; anything else by its datatype's id. MEASURED on sbl -bf: SORT of '10' 5 '3' 20 'a' 2.0 is 2. 5 20 10 3 a; RSORT of one of each type orders them DATA,
;; ARRAY (the later made first), TABLE, EXPRESSION, STRING, CODE, REAL, PATTERN, NAME, INTEGER; corpus gate_cases_snobol4 aggregate_registry_keys_are_descrs__w3,
;; SORT of a table keyed by twelve arrays, which sbl gives in the order the arrays were made and this engine compared as the text of their atoms.
(def ^:private sort-type-rank {"INTEGER" 0 "NAME" 1 "PATTERN" 2 "REAL" 3 "CODE" 4 "STRING" 5 "EXPRESSION" 6 "TABLE" 7 "ARRAY" 8})

(defn- sort-serial [v] (cond (array? v) (:serial v) (table? v) (:serial (meta v)) (instance? SNOBOL4clojure.env.DataObj v) (.serial ^SNOBOL4clojure.env.DataObj v)))

(defn- sort-key-compare
  [a b]
  (cond
    (and (string? a) (string? b)) (compare a b)
    (and (number? a) (number? b)) (compare a b)
    :else (let [ta (str (DATATYPE a)) tb (str (DATATYPE b))]
            (if (clojure.core/= ta tb)
              (let [sa (sort-serial a) sb (sort-serial b)] (if (and sa sb) (compare sa sb) 0))
              (compare (sort-type-rank ta 9) (sort-type-rank tb 9))))))

;; SORT and RSORT take a table or an array. A table sorts as the two-column array of its entries (keys in column 1, values in column 2, in the order the keys were
;; first assigned); an array sorts its rows on column col (1 by default), a one-dimensional array being its own column. The sort is stable, ascending for SORT and
;; descending for RSORT, so entries that tie keep the order they had. The result is a new array.


;; The column of a two-dimensional array is an integer within the second dimension's bounds, its lower bound when omitted, else "258 sort/rsort 2nd arg out of
;; range or non-integer"; a one-dimensional array takes no column; anything but a table or such an array is "256 sort/rsort 1st arg not suitable array or table"
;; (sbl.min sorta; sbl -bf: RSORT(T, 3) of a table, whose array has two columns, is 258).
(defn- sort-array
  "A new array with the rows of arr stably sorted on column col."
  [arr col descending?]
  (let [dims   (:dims arr)
        lo1    (first (first dims))
        hi1    (second (first dims))
        two-d? (clojure.core/= 2 (count dims))
        _      (when (clojure.core/< 2 (count dims)) (throw (ex-info (error-message 256) {:snobol/error 256})))
        [lo2 hi2] (second dims)
        k      (when two-d?
                 (let [k (if (or (nil? col) (clojure.core/= "" col)) lo2 (integer-of col))]
                   (if (and k (clojure.core/<= lo2 k hi2)) (clojure.core/- k lo2) (throw (ex-info (error-message 258) {:snobol/error 258})))))
        cols   (when two-d? (range lo2 (inc hi2)))
        rows   (for [i (range lo1 (inc hi1))]
                 (if two-d? (vec (for [j cols] (array-get arr [i j]))) [(array-get arr [i])]))
        pick   (fn [row] (if two-d? (nth row k) (first row)))
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
    (table? A) (when-some [arr (table->array A true)] (sort-array arr col descending?))
    (array? A) (sort-array A col descending?)
    :else (throw (ex-info (error-message 256) {:snobol/error 256}))))

(defn SORT
  ([A]     (SORT A nil))
  ([A col] (sort-any A col false)))

(defn RSORT
  ([A]     (RSORT A nil))
  ([A col] (sort-any A col true)))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; Pattern primitives: the pure scanners, each [Σ Δ Π] to [σ δ]
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── Failure encoding ─────────────────────────────────────────────────────────
(defn err [Σ Δ] [Σ (clojure.core/- -1 Δ)])

;; ── Trivials ──────────────────────────────────────────────────────────────────
(defn SUCCEED! [Σ Δ _Π] [Σ Δ])
(defn FAIL!    [Σ Δ _Π] (err Σ Δ))
(defn ABORT!   [Σ Δ _Π] (err nil Δ))

;; ── Stub scanners (not yet implemented) ──────────────────────────────────────
(defn ARB!    [Σ Δ _Π] (err nil Δ))
(defn BAL!    [Σ Δ _Π] (err nil Δ))
(defn ARBNO!  [Σ Δ _Π] (err nil Δ))
(defn FENCE!  [Σ Δ _Π] (err nil Δ))
(defn FENCE!! [Σ Δ _Π] (err nil Δ))
;; BREAKX$ — like BREAK$ but participates in backtracking:
;; on first call matches up to (but not including) the first break char,
;; then on each retry advances ONE character past the break char.
;; Implemented as a stateful multi-yield: φ (ζ slot 5) stores retry count.
;; In the engine BREAKX$ is handled specially; the primitive here is the
;; single-step scanner used by the engine's BREAKX# node.
;; For the simple (non-backtracking) form we expose the same interface as BREAK$.
(defn BREAKX$ [Σ Δ Π]
  ;; Single-yield form: scan until a char in Π is found (same as BREAK$).
  ;; The engine's BREAKX# node handles the backtracking by calling this
  ;; at successive positions.
  (loop [σ Σ δ Δ]
    (if (not (seq σ))
      (err σ δ)                          ; no break char found → fail
      (if (contains? Π (first σ))
        [σ δ]                            ; stopped before the break char
        (recur (rest σ) (inc δ))))))

;; ── Positional ────────────────────────────────────────────────────────────────
(defn POS#  [Σ Δ Π] (if (equal Δ Π)         [Σ Δ] (err Σ Δ)))
(defn RPOS# [Σ Δ Π] (if (equal (count Σ) Π) [Σ Δ] (err Σ Δ)))

;; ── Character class ───────────────────────────────────────────────────────────
(defn ANY$    [Σ Δ Π] (if (not (seq Σ))
                        (err Σ Δ)
                        (if     (contains? Π (first Σ)) [(rest Σ) (inc Δ)] (err Σ Δ))))
(defn NOTANY$ [Σ Δ Π] (if (not (seq Σ))
                        (err Σ Δ)
                        (if-not (contains? Π (first Σ)) [(rest Σ) (inc Δ)] (err Σ Δ))))

;; ── String matching ───────────────────────────────────────────────────────────
(defn REM! [Σ Δ _Π]
  (loop [σ Σ δ Δ]
    (if (not (seq σ)) [σ δ] (recur (rest σ) (inc δ)))))

(defn LIT$ [Σ Δ Π]
  (loop [σ Σ δ Δ π Π]
    (if (not (seq π)) [σ δ]
      (if (not (seq σ)) (err σ δ)
        (if (not-equal (first σ) (first π)) (err σ δ)
          (recur (rest σ) (inc δ) (rest π)))))))

;; ── Length and tab ────────────────────────────────────────────────────────────
(defn LEN# [Σ Δ Π]
  (loop [σ Σ δ Δ]
    (if (>= δ (Σ+ Δ Π)) [σ δ]
      (if (not (seq σ)) (err σ δ) (recur (rest σ) (inc δ))))))

(defn TAB# [Σ Δ Π]
  (if (> Δ Π)
    (err Σ Δ)                                ; the cursor is already past the target: TAB fails
    (loop [σ Σ δ Δ]
      (if (>= δ Π) [σ δ]
        (if (not (seq σ)) (err σ δ) (recur (rest σ) (inc δ)))))))

(defn RTAB# [Σ Δ Π]
  (loop [σ Σ δ Δ]
    (let [rem (count σ)]
      (cond
        (== rem Π)  [σ δ]                ; exactly at target — succeed
        (< rem Π)   (err Σ Δ)            ; overshot — fail
        :else       (recur (rest σ) (inc δ))))))

;; ── Zero-width anchors ────────────────────────────────────────────────────────
(defn BOL# [Σ Δ _Π] (if (clojure.core/= Δ 0) [Σ Δ] (err Σ Δ)))
(defn EOL# [Σ Δ _Π] (if (not (seq Σ)) [Σ Δ] (err Σ Δ)))

;; ── Span and break ────────────────────────────────────────────────────────────
;; NSPAN$ — like SPAN$ but matches 0 or more chars (no minimum-1 requirement)
(defn NSPAN$ [Σ Δ Π]
  (loop [σ Σ δ Δ]
    (if (not (contains? Π (first σ)))
      [σ δ]                              ; always succeeds, even on zero chars
      (recur (rest σ) (inc δ)))))

(defn SPAN$ [Σ Δ Π]
  (loop [σ Σ δ Δ]
    (if (not (contains? Π (first σ)))
      (if (not-equal δ Δ) [σ δ] (err σ δ))
      (recur (rest σ) (inc δ)))))

(defn BREAK$ [Σ Δ Π]
  (loop [σ Σ δ Δ]
    (if (not (seq σ)) (err σ δ)
      (if (contains? Π (first σ)) [σ δ]
        (recur (rest σ) (inc δ))))))

;; ── Charset helper ────────────────────────────────────────────────────────────
(defn charset [S] (reduce #(conj %1 %2) #{} S))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; Pattern constructors: patterns are data
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── Character-class patterns ──────────────────────────────────────────────────
;; If the argument is a DEFER! thunk (produced by unary * on a variable),
;; we must NOT call charset now — the variable may change between uses.
;; Instead wrap the whole constructor in a DEFER so the engine re-evaluates
;; the pattern (and re-builds the charset) at match time from the live value.
(defn- deferred? [S] (and (list? S) (clojure.core/= (first S) 'DEFER!)))
;; A pattern function given a deferred argument builds its element as the match reaches it, and is a PATTERN all the same, where *X alone is an EXPRESSION
;; (MEASURED on sbl -bf: DATATYPE of BREAK(*X), ANY(*X), SPAN(*X), NOTANY(*X), BREAKX(*X), LEN(*X), POS(*X) and TAB(*X) is PATTERN, of *X EXPRESSION; env DATATYPE)
(defn- deferred-pattern [f] (with-meta (list 'DEFER! f) {:pattern true}))

;; The argument is a string: an integer converts to its digits, a real as SPITBOL writes it and the name of a natural variable to its name; a null one, or any
;; other value, the name of a keyword or an element among them, is the function's "argument is not a string or expression" (59 ANY, 151 NOTANY, 188 SPAN, 69
;; BREAK, 70 BREAKX), and when it came deferred, read as the match reaches it, its "evaluated argument is not a string" (43, 49, 56, 44, 45; Appendix D), as
;; sbl -bf raises them (ANY(.&ERRTEXT) is 59). A deferred expression that fails fails the element.
(defn- set-of [S err]
  (let [s (if (or (string? S) (number? S) (and (instance? SNOBOL4clojure.env.NAME S) (symbol? (.n ^SNOBOL4clojure.env.NAME S)))) (sstr S) "")]
    (if (seq s) (charset s) (throw (ex-info (error-message err) {:snobol/error err})))))

(defn- set-pattern-of [op S bad ebad]
  (if (deferred? S)
    (deferred-pattern (fn [] (when-some [v ((second S))] (list op (set-of v ebad)))))
    (list op (set-of S bad))))

(defn ANY    [S] (set-pattern-of 'ANY$    S  59 43))
(defn BREAK  [S] (set-pattern-of 'BREAK$  S  69 44))
(defn BREAKX [S] (set-pattern-of 'BREAKX# S  70 45))   ; backtracking form
(defn NOTANY [S] (set-pattern-of 'NOTANY$ S 151 49))
(defn NSPAN  [S] (if (deferred? S) (deferred-pattern (fn [] (NSPAN  (str ((second S))))))  (list 'NSPAN$  (charset S))))   ; 0-or-more span
(defn SPAN   [S] (set-pattern-of 'SPAN$   S 188 56))

;; ── Length / position patterns ────────────────────────────────────────────────
;; A deferred argument (*N) is read when the match reaches the pattern, as for
;; the charset patterns above.
;; "These functions return a pattern based on their integer argument" (manual ch. 4, Integer pattern functions); for LEN "I must be an integer greater than or equal
;; to zero". The argument converts as any integer argument (a numeric string, a real truncated); one that does not is "120 LEN argument is not integer or expression",
;; one below 0 or above 16777216 (SPITBOL's longest string) "121 LEN argument is negative or too large" (sbl.min patin, gtsmi). A deferred argument is checked when
;; the match reaches it: "47 LEN evaluated argument is not integer", "48 LEN evaluated argument is negative or too large" (Appendix D). POS, RPOS, TAB and RTAB the
;; same, each with its own numbers.
(defn- int-pattern [op I bad big]
  (let [n (integer-of I)]
    (cond
      (nil? n)                                  (throw (ex-info (error-message bad) {:snobol/error bad}))
      (or (neg? n) (clojure.core/> n 16777216)) (throw (ex-info (error-message big) {:snobol/error big}))
      :else                                     (list op n))))

;; A deferred argument that fails fails the pattern there, and the match backtracks, as a deferred argument of ANY does (sbl -bf: 'abc' ? (LEN(*EVAL('1 / N')) |
;; 'c') . R assigns c; the engine read the failure as the null string, 0).
(defn- int-pattern-of [op I [bad big ebad ebig]]
  (if (deferred? I) (deferred-pattern (fn [] (when-some [v ((second I))] (int-pattern op v ebad ebig)))) (int-pattern op I bad big)))

(defn LEN  [I] (int-pattern-of 'LEN#  I [120 121 47 48]))
(defn POS  [I] (int-pattern-of 'POS#  I [162 163 50 51]))
(defn RPOS [I] (int-pattern-of 'RPOS# I [185 186 52 53]))
(defn RTAB [I] (int-pattern-of 'RTAB# I [181 182 54 55]))
(defn TAB  [I] (int-pattern-of 'TAB#  I [183 184 57 58]))

;; ── Zero-width anchor patterns ────────────────────────────────────────────────
(def BOL (list 'BOL#))    ; beginning of subject (position 0)
(def EOL (list 'EOL#))    ; end of subject (no chars remaining)

;; ── Repetition / structural patterns ─────────────────────────────────────────
(defn ARBNO [P]  (list 'ARBNO! P))
(defn FENCE
  ([]  (list 'FENCE!))
  ([P] (list 'FENCE! P)))

;; ── Cursor assignment ─────────────────────────────────────────────────────────
(defn CURSOR [N] (list 'CURSOR-IMM! N))  ; @N — assign cursor position to N

;; ── Deferred pattern ──────────────────────────────────────────────────────────
(defn DEFER [thunk] (list 'DEFER! thunk)) ; *expr — evaluate thunk at match time

;; ── Constant pattern values ───────────────────────────────────────────────────
(def ARB     (list 'ARB!))
(def BAL     (list 'BAL!))
(def REM     (list 'REM!))
(def ABORT   (list 'ABORT!))
(def FAIL    (list 'FAIL!))
(def SUCCEED (list 'SUCCEED!))
