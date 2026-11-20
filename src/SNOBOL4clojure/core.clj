(ns SNOBOL4clojure.core
    (:gen-class)
    (:refer-clojure :exclude [= + - * / num])
    (:require [clojure.pprint :as pp] [SNOBOL4clojure.env :as env] [SNOBOL4clojure.operators :as ops] [SNOBOL4clojure.compiler :as comp] [SNOBOL4clojure.runtime :as rt]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ε env/ε)
(def η env/η)
(def GLOBALS env/GLOBALS)
(def snobol-set! env/snobol-set!)
(def &ALPHABET env/&ALPHABET)
(def &ANCHOR env/&ANCHOR)
(def &DIGITS env/&DIGITS)
(def &DUMP env/&DUMP)
(def &ERRLIMIT env/&ERRLIMIT)
(def &ERRTEXT env/&ERRTEXT)
(def &ERRTYPE env/&ERRTYPE)
(def &FTRACE env/&FTRACE)
(def &FULLSCAN env/&FULLSCAN)
(def &LASTNO env/&LASTNO)
(def &STNO env/&STNO)
(def &FNCLEVEL env/&FNCLEVEL)
(def &LCASE env/&LCASE)
(def &MAXLNGTH env/&MAXLNGTH)
(def &PROFILE env/&PROFILE)
(def &TRACE env/&TRACE)
(def &TRIM env/&TRIM)
(def &STCOUNT env/&STCOUNT)
(def &STLIMIT env/&STLIMIT)
(def &IOCOMPAT env/&IOCOMPAT)
(def &UCASE env/&UCASE)
(def INPUT$ env/INPUT$)
(def OUTPUT$ env/OUTPUT$)
(def TERMINAL$ env/TERMINAL$)
(import SNOBOL4clojure.env.NAME)
(def DATATYPE env/DATATYPE)
(def ARRAY env/ARRAY)
(def TABLE env/TABLE)
(def SET env/SET)
(def $$ env/$$)
(def equal env/equal)
(def not-equal env/not-equal)
(def Σ+ env/Σ+)
(def num env/num)
(def out env/out)
(def reference env/reference)
(def ANY env/ANY)
(def BREAK env/BREAK)
(def BREAKX env/BREAKX)
(def NOTANY env/NOTANY)
(def NSPAN env/NSPAN)
(def SPAN env/SPAN)
(def ARBNO env/ARBNO)
(def FENCE env/FENCE)
(def LEN env/LEN)
(def POS env/POS)
(def RPOS env/RPOS)
(def RTAB env/RTAB)
(def TAB env/TAB)
(def ARB env/ARB)
(def BAL env/BAL)
(def BOL env/BOL)
(def EOL env/EOL)
(def REM env/REM)
(def ABORT env/ABORT)
(def FAIL env/FAIL)
(def SUCCEED env/SUCCEED)
(def MATCH ops/MATCH)
(def SEARCH ops/SEARCH)
(def FULLMATCH ops/FULLMATCH)
(def COLLECT! ops/COLLECT!)
(def = ops/=)
(def ? ops/?)
(def ?= ops/?=)
(def & ops/&)
(def + ops/+)
(def - ops/-)
(def * ops/*)
(def / ops//)
(def $ ops/$)
(def . ops/.)
(def | ops/|)
(def at ops/at)
(def tilde ops/tilde)
(def ! ops/!)
(def ** ops/**)
(def % ops/%)
(def sharp ops/sharp)
(def EQ ops/EQ)
(def NE ops/NE)
(def LE ops/LE)
(def LT ops/LT)
(def GE ops/GE)
(def GT ops/GT)
(def LEQ ops/LEQ)
(def LNE ops/LNE)
(def LLE ops/LLE)
(def LLT ops/LLT)
(def LGE ops/LGE)
(def LGT ops/LGT)
(def IDENT ops/IDENT)
(def DIFFER ops/DIFFER)
(def EVAL ops/EVAL)
(def EVAL! ops/EVAL!)
(def INVOKE ops/INVOKE)
(def APPLY ops/APPLY)
(def SIZE env/SIZE)
(def REPLACE env/REPLACE)
(def DUPL env/DUPL)
(def TRIM env/TRIM)
(def REVERSE env/REVERSE)
(def DATA env/DATA)
(def DATA! env/DATA!)
(def FIELD env/FIELD)
(def CHAR env/CHAR)
(def ORD env/ORD)
(def REMDR env/REMDR)
(def ATAN env/ATAN)
(def CHOP env/CHOP)
(def COS env/COS)
(def EXP env/EXP)
(def LN env/LN)
(def SIN env/SIN)
(def SQRT env/SQRT)
(def TAN env/TAN)
(def INTEGER env/INTEGER)
(def REAL env/REAL)
(def CONVERT env/CONVERT)
(def COPY env/COPY)
(def SORT env/SORT)
(def RSORT env/RSORT)
(def ITEM env/ITEM)
(def PROTOTYPE env/PROTOTYPE)
(def data-type-registry env/data-type-registry)
(def CODE! comp/CODE!)
(def CODE comp/CODE)
(def RUN rt/RUN)
(def CODE-ir comp/CODE-ir)
(def preprocess-includes comp/preprocess-includes)
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
    (reset! env/<WIDTH> 120)
    (reset! dump-switch nil)
    (reset! exit-code nil)
    (let
        [argv
            (into [(System/getProperty "java.class.path")] args)
            [engine u args]
            (loop
                [engine "interp" u nil as args]
                (cond
                    (clojure.core/= "--engine" (first as))
                    (recur (second as) u (nnext as))
                    (clojure.core/= "-u" (first as))
                    (recur engine (second as) (nnext as))
                    (and (first as) (re-matches #"-t\d+" (first as)))
                    (do (reset! env/<WIDTH> (Long/parseLong (subs (first as) 2))) (recur engine u (next as)))
                    (and (first as) (re-matches #"-D\d+" (first as)))
                    (do (reset! dump-switch (Long/parseLong (subs (first as) 2))) (recur engine u (next as)))
                    (and (first as) (re-matches #"-(?:[bfnelsx]+|[dsimt]\d+[kmgKMG]?)" (first as)))
                    (recur engine u (next as))
                    :else
                    [engine u as]))
            [file & prog-args]
            args]
        (when (or (nil? file) (not (#{"interp"} engine))) (binding [*out* *err*] (println "Usage: lein run [--engine interp] <file.sno>")) (System/exit 1))
        (let
            [src (slurp file :encoding "ISO-8859-1") dir (or (.getParent (java.io.File. ^String file)) ".")]
            (let
                [sink
                    (some-> (or *list-sink* (System/getenv "SCRIP_SNO_LIST_SINK")) not-empty (java.io.FileWriter. java.nio.charset.StandardCharsets/ISO_8859_1))
                    listed
                    (fn [f] (if sink (binding [*out* sink] (f) (flush)) (f)))
                    rc
                    (case engine "interp"
                        (do
                            (env/GLOBALS)
                            (when-some [d @dump-switch] (reset! env/&DUMP d))
                            (reset! env/<HOST0> (or u (clojure.string/join " " (remove #{"--"} prog-args))))
                            (reset! env/<ARGV> argv)
                            (reset! env/<ARGI> (clojure.core/- (count argv) (count prog-args)))
                            (let
                                [entry (comp/CODE src (include-dirs dir (System/getenv "SNO_LIB")) file)]
                                (listed #(env/write-listing! file))
                                (cond
                                    (and (nil? (@env/<STNO> :END)) (nil? (@env/<STNO> :end)))
                                    (do (binding [*out* *err*] (println "No END statement found in source file(s).") (flush)) 1)
                                    (seq @env/<DIAGNOSTICS>)
                                    231
                                    (noexecute? src)
                                    231 :else
                                    (do
                                        (let
                                            [t0 (System/nanoTime)]
                                            (try
                                                (rt/RUN entry)
                                                (catch clojure.lang.ExceptionInfo e
                                                    (let
                                                        [{n :snobol/error sig :snobol/signal steps :steps lvl :fnclevel}
                                                            (ex-data e)
                                                            _
                                                            (when lvl (reset! env/&FNCLEVEL lvl))
                                                            n
                                                            (if (and n (zero? (long n))) 36 n)
                                                            code
                                                            (or n (when (clojure.core/= :step-limit sig) 244))]
                                                        (when-not (or code (clojure.core/= :exit sig)) (throw e))
                                                        (if
                                                            (clojure.core/= :exit sig)
                                                            (reset! exit-code (long (:snobol/rc (ex-data e) 0)))
                                                            (let
                                                                [text (or (env/error-message code) (ex-message e))]
                                                                (reset! env/&ERRTYPE code)
                                                                (reset! env/&ERRTEXT text)
                                                                (listed #(env/report! code text @env/&STCOUNT t0))))))
                                                (catch StackOverflowError _
                                                    (let [text (env/error-message 246)] (reset! env/&ERRTYPE 246) (reset! env/&ERRTEXT text) (listed #(env/report! 246 text @env/&STCOUNT t0))))
                                                (finally (when (and (nil? @exit-code) (pos? (long @env/&DUMP))) (listed #(print (env/dump-text @env/&DUMP)))))))
                                        (let [c (long (or @exit-code @env/&CODE)) c (if (neg? c) 0 (mod c 256))] (when (pos? c) c)))))))]
                (flush)
                (when sink (.close ^java.io.Writer sink))
                (when (integer? rc) rc)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def compile-to-class rt/compile-to-class)
(def load-program rt/load-program)
(def run-jvm! rt/run-jvm!)
(def run-jvm-captured! rt/run-jvm-captured!)
(def bench-compare-jvm rt/bench-compare-jvm)
(def clear-class-cache! rt/clear-class-cache!)
(def jvm-gen-class-name rt/gen-class-name)
(def trace-register! env/trace!)
(def trace-stop! env/stoptr!)
(def trace-clear! env/clear-all-traces!)
(def trace-enable-full! env/enable-full-trace!)
(def trace-disable-full! env/disable-full-trace!)
(def trace-output env/*trace-output*)
(def snapshot! env/snapshot!)
(def <CHANNELS> env/<CHANNELS>)
(def <OPSYN> env/<OPSYN>)
