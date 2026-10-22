(ns SNOBOL4clojure.core
  "SNOBOL4/Clojure — public entry point.
   Loads all sub-namespaces and re-exports the public API via def aliases
   so that (require '[SNOBOL4clojure.core :refer :all]) works unchanged."
  (:gen-class)
  (:require [clojure.pprint            :as pp]
            [SNOBOL4clojure.env        :as env]
            [SNOBOL4clojure.primitives :as prim]
            [SNOBOL4clojure.match      :as match]
            [SNOBOL4clojure.patterns   :as pat]
            [SNOBOL4clojure.operators  :as ops]
            [SNOBOL4clojure.functions  :as fns]
            [SNOBOL4clojure.grammar    :as gram]
            [SNOBOL4clojure.emitter    :as emit]
            [SNOBOL4clojure.compiler   :as comp]
            [SNOBOL4clojure.runtime    :as rt]
            [SNOBOL4clojure.dump       :as dump]
            [SNOBOL4clojure.fatal      :as fatal]
            [SNOBOL4clojure.listing    :as listing]
            [SNOBOL4clojure.trace      :as trace]
            [SNOBOL4clojure.transpiler :as xpile]
            [SNOBOL4clojure.vm         :as vm]
            [SNOBOL4clojure.jvm-codegen :as jvmc])
  (:refer-clojure :exclude [= + - * / num]))

;; ── Re-export env ─────────────────────────────────────────────────────────────
(def  ε          env/ε)
(def  η          env/η)
(def  GLOBALS    env/GLOBALS)
(def  snobol-set! env/snobol-set!)
(def  &ALPHABET  env/&ALPHABET)
(def  &ANCHOR    env/&ANCHOR)
(def  &DIGITS    env/&DIGITS)
(def  &DUMP      env/&DUMP)
(def  &ERRLIMIT  env/&ERRLIMIT)
(def  &ERRTEXT   env/&ERRTEXT)
(def  &ERRTYPE   env/&ERRTYPE)
(def  &FTRACE    env/&FTRACE)
(def  &FULLSCAN  env/&FULLSCAN)
(def  &LASTNO    env/&LASTNO)
(def  &STNO      env/&STNO)
(def  &FNCLEVEL  env/&FNCLEVEL)
(def  &LCASE     env/&LCASE)
(def  &MAXLNGTH  env/&MAXLNGTH)
(def  &PROFILE   env/&PROFILE)
(def  &TRACE     env/&TRACE)
(def  &TRIM      env/&TRIM)
(def  &STCOUNT   env/&STCOUNT)
(def  &STLIMIT   env/&STLIMIT)
(def  &IOCOMPAT  env/&IOCOMPAT)
(def  &UCASE     env/&UCASE)
(def  INPUT$     env/INPUT$)
(def  OUTPUT$    env/OUTPUT$)
(def  TERMINAL$  env/TERMINAL$)
;; deftype — import directly
(import SNOBOL4clojure.env.NAME)
(def DATATYPE    env/DATATYPE)
(def ARRAY       env/ARRAY)
(def TABLE       env/TABLE)
(def SET         fns/SET)   ; SPITBOL's SET(channel, offset, whence); env/SET is the engine's own set constructor
(def $$          env/$$)
(def equal       env/equal)
(def not-equal   env/not-equal)
(def Σ+          env/Σ+)
(def num         env/num)
(def out         env/out)
(def reference   env/reference)

;; ── Re-export patterns ────────────────────────────────────────────────────────
(def ANY     pat/ANY)
(def BREAK   pat/BREAK)
(def BREAKX  pat/BREAKX)
(def NOTANY  pat/NOTANY)
(def NSPAN   pat/NSPAN)
(def SPAN    pat/SPAN)
(def ARBNO   pat/ARBNO)
(def FENCE   pat/FENCE)
(def LEN     pat/LEN)
(def POS     pat/POS)
(def RPOS    pat/RPOS)
(def RTAB    pat/RTAB)
(def TAB     pat/TAB)
(def ARB     pat/ARB)
(def BAL     pat/BAL)
(def BOL     pat/BOL)
(def EOL     pat/EOL)
(def REM     pat/REM)
(def ABORT   pat/ABORT)
(def FAIL    pat/FAIL)
(def SUCCEED pat/SUCCEED)

;; ── Re-export match engine ────────────────────────────────────────────────────
(def MATCH      match/MATCH)
(def SEARCH     match/SEARCH)
(def FULLMATCH  match/FULLMATCH)
(def REPLACE    match/REPLACE)
(def COLLECT!  match/COLLECT!)

;; ── Re-export operators / EVAL ────────────────────────────────────────────────
(def =     ops/=)
(def ?     ops/?)
(def ?=    ops/?=)
(def &     ops/&)
(def +     ops/+)
(def -     ops/-)
(def *     ops/*)
(def /     ops//)
(def $     ops/$)
(def .     ops/.)
(def |     ops/|)
(def at    ops/at)
(def tilde ops/tilde)
(def !     ops/!)
(def **    ops/**)
(def %     ops/%)
(def sharp ops/sharp)
(def EQ    ops/EQ)
(def NE    ops/NE)
(def LE    ops/LE)
(def LT    ops/LT)
(def GE    ops/GE)
(def GT    ops/GT)
(def LEQ   ops/LEQ)
(def LNE   ops/LNE)
(def LLE   ops/LLE)
(def LLT   ops/LLT)
(def LGE   ops/LGE)
(def LGT   ops/LGT)
(def IDENT  ops/IDENT)
(def DIFFER ops/DIFFER)
(def EVAL  ops/EVAL)
(def EVAL! ops/EVAL!)
(def INVOKE ops/INVOKE)
(def APPLY  ops/APPLY)

;; ── Re-export functions ───────────────────────────────────────────────────────
(def SIZE    fns/SIZE)
(def REPLACE fns/REPLACE)
(def DUPL    fns/DUPL)
(def TRIM    fns/TRIM)
(def REVERSE fns/REVERSE)
(def DATA    fns/DATA)
(def DATA!   fns/DATA!)
(def FIELD   fns/FIELD)
(def ASCII   fns/ASCII)
(def CHAR    fns/CHAR)
(def ORD     fns/ORD)
(def REMDR   fns/REMDR)
(def ATAN    fns/ATAN)
(def CHOP    fns/CHOP)
(def COS     fns/COS)
(def EXP     fns/EXP)
(def LN      fns/LN)
(def SIN     fns/SIN)
(def SQRT    fns/SQRT)
(def TAN     fns/TAN)
(def INTEGER fns/INTEGER)
(def REAL    fns/REAL)
(def STRING  fns/STRING)
(def CONVERT fns/CONVERT)
(def COPY    fns/COPY)
(def SORT    fns/SORT)
(def RSORT   fns/RSORT)
(def ITEM    fns/ITEM)
(def PROTOTYPE fns/PROTOTYPE)
(def data-type-registry fns/data-type-registry)

;; ── Re-export compiler / runtime ─────────────────────────────────────────────
(def CODE!             comp/CODE!)
(def CODE              comp/CODE)
(def RUN               rt/RUN)
;; Stage 23A — EDN cache
(def ir->edn           comp/ir->edn)
(def edn->ir           comp/edn->ir)
(def CODE-ir           comp/CODE-ir)
(def CODE-memo         comp/CODE-memo)
(def CODE-cached       comp/CODE-cached)
(def compile-to-file   comp/compile-to-file)
(def load-ir           comp/load-ir)
(def clear-memo!       comp/clear-memo!)
(def memo-stats        comp/memo-stats)
(def precompile-corpus! comp/precompile-corpus!)
;; Sprint 25A — -INCLUDE preprocessor
(def preprocess-includes comp/preprocess-includes)

;; ── -INCLUDE search path ─────────────────────────────────────────────────────
(defn include-dirs
  "Where -INCLUDE looks, in order: the program's own directory, then each
   directory of SNO_LIB (colon-separated, the fleet's include variable), then
   the current directory."
  [program-dir sno-lib]
  (vec (distinct (concat [program-dir]
                         (remove clojure.string/blank? (clojure.string/split (or sno-lib "") #":"))
                         ["."]))))

;; ── -main ─────────────────────────────────────────────────────────────────────
(defn -main
  "SNOBOL4/Clojure — run a .sno file: [--engine interp|vm] <file.sno>.
   The default engine is the Stage 1 interpreter, the one the test suite runs.
   -INCLUDE files resolve against the program's own directory, then SNO_LIB,
   then the current one. INPUT reads stdin."
  [& args]
  (let [[engine args] (if (clojure.core/= "--engine" (first args)) [(second args) (nnext args)] ["interp" args])
        ;; -u STR before the file is HOST(0)'s text; the arguments after the file are the program's own, which HOST(0) joins when there is no -u
        [u args]      (if (clojure.core/= "-u" (first args)) [(second args) (nnext args)] [nil args])
        [file & prog-args] args]
    (when (or (nil? file) (not (#{"interp" "vm"} engine)))
      (binding [*out* *err*]
        (println "Usage: lein run [--engine interp|vm] <file.sno>"))
      (System/exit 1))
    (let [src (slurp file :encoding "ISO-8859-1")   ; a program is bytes
          dir (or (.getParent (java.io.File. ^String file)) ".")]
      ;; -main answers the exit code the program answers: 231 for a refused compile, nil for a run
      (let [rc (case engine
        "interp" (do (env/GLOBALS)
                     (reset! env/<HOST0> (or u (clojure.string/join " " (remove #{"--"} prog-args))))
                     ;; "-NOERRORS is the default, and suppresses execution if there is a compilation error" (manual ch. 14): the listing -LIST asks for is
                     ;; written as the program is compiled, a compile error on its page in sbl -bf's shape, and a program with one is not run; -main answers 231,
                     ;; sbl's exit code for it
                     (let [entry (comp/CODE src (include-dirs dir (System/getenv "SNO_LIB")) file)]
                       (listing/write-listing! file)
                       (if (seq @env/<DIAGNOSTICS>)
                         231
                         ;; an execution error that ends the program (and the statement limit, "244 Statement count exceeds value of STLIMIT keyword") is
                         ;; reported as sbl -bf reports it, with &ERRTYPE and &ERRTEXT set, and the run ends there, exit code 0 as sbl's; the termination
                         ;; dump &DUMP asks for is written after it, however the program ends
                         (let [t0 (System/nanoTime)]
                           (try (rt/RUN entry)
                                (catch clojure.lang.ExceptionInfo e
                                  (let [{n :snobol/error sig :snobol/signal steps :steps} (ex-data e)
                                        code (or n (when (clojure.core/= :step-limit sig) 244))]
                                    (when-not code (throw e))
                                    (let [text (or (env/error-message code) (ex-message e))]
                                      (reset! env/&ERRTYPE code)
                                      (reset! env/&ERRTEXT text)
                                      ;; the statement the limit stops is not executed
                                      (fatal/report! code text (if n @env/&STCOUNT (dec (long steps))) t0))))
                                (finally (when (pos? (long @env/&DUMP)) (print (dump/dump-text @env/&DUMP)))))))))
        "vm"     (print (vm/run-program! src)))]
        (flush)
        (when (integer? rc) rc)))))

;; ── Re-export transpiler (Stage 23B) ─────────────────────────────────────────
(def transpile          xpile/transpile)
(def transpile-ir       xpile/transpile-ir)
(def load-transpiled!   xpile/load-transpiled!)
(def run-transpiled!    xpile/run-transpiled!)
(def bench-compare      xpile/bench-compare)

;; ── Re-export VM (Stage 23C) ──────────────────────────────────────────────────
(def compile-ir         vm/compile-ir)
(def compile-src        vm/compile-src)
(def run-vm!            vm/run-vm!)
(def run-program!       vm/run-program!)
(def bench-compare-vm   vm/bench-compare-vm)
;; Opcode constants
(def OP-HALT            vm/OP-HALT)
(def OP-EXEC            vm/OP-EXEC)
(def OP-EXEC-S          vm/OP-EXEC-S)
(def OP-EXEC-F          vm/OP-EXEC-F)
(def OP-EXEC-SF         vm/OP-EXEC-SF)
(def OP-JUMP            vm/OP-JUMP)
(def OP-SIGNAL          vm/OP-SIGNAL)

;; ── Re-export JVM codegen (Stage 23D) ───────────────────────────────────────
(def compile-to-class    jvmc/compile-to-class)
(def load-program        jvmc/load-program)
(def run-jvm!            jvmc/run-jvm!)
(def run-jvm-captured!   jvmc/run-jvm-captured!)
(def bench-compare-jvm   jvmc/bench-compare-jvm)
(def clear-class-cache!  jvmc/clear-class-cache!)
(def jvm-gen-class-name  jvmc/gen-class-name)

;; ── Re-export trace API ───────────────────────────────────────────────────────
;; These are available to SNOBOL4 programs via INVOKE and to Clojure tests.
(def trace-register!     trace/trace!)
(def trace-stop!         trace/stoptr!)
(def trace-clear!        trace/clear-all-traces!)
(def trace-enable-full!  trace/enable-full-trace!)
(def trace-disable-full! trace/disable-full-trace!)
(def trace-output        trace/*trace-output*)

;; ── Re-export debug/probe API ─────────────────────────────────────────────────
(def snapshot!  env/snapshot!)
(def <CHANNELS> env/<CHANNELS>)
(def <OPSYN>    env/<OPSYN>)
