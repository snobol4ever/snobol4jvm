(ns SNOBOL4clojure.test-csnobol4-suite
  "Budne csnobol4-suite (116 programs) + FENCE tests (10) run through snobol4jvm CODE/RUN.
   Each test reads .sno from corpus on disk and compares stdout to .ref oracle.
   Set CORPUS_ROOT env var to override default /home/claude/corpus."
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [SNOBOL4clojure.compiler :refer [CODE]]
            [SNOBOL4clojure.runtime  :refer [RUN]]
            [SNOBOL4clojure.env      :as env]))

(def ^:private corpus-root
  (or (System/getenv "CORPUS_ROOT") "/home/claude/corpus"))

(def ^:private suite-dir
  (str corpus-root "/programs/csnobol4-suite"))

(def ^:private fence-dir
  (str corpus-root "/crosscheck/patterns"))

(defn- reset-rt! []
  (reset! env/STNO   0)
  (reset! env/<STNO> {})
  (reset! env/<LABL> {})
  (reset! env/<CODE> {}))

(use-fixtures :each (fn [t] (reset-rt!) (env/GLOBALS *ns*) (t)))

(defn- run-sno
  "Read sno-path, compile and run, return trimmed stdout."
  [sno-path]
  (let [src (slurp sno-path)
        out (with-out-str (RUN (CODE src)))]
    (str/trimr out)))

(defn- ref-expected
  "Read ref-path, return trimmed content."
  [ref-path]
  (str/trimr (slurp ref-path)))

(defmacro defsuite
  "Define a deftest that runs one .sno file and compares to its .ref."
  [test-name dir base]
  `(deftest ~test-name
     (let [sno# (str ~dir "/" ~base ".sno")
           ref# (str ~dir "/" ~base ".ref")]
       (if (and (.exists (java.io.File. sno#)) (.exists (java.io.File. ref#)))
         (is (= (ref-expected ref#) (run-sno sno#)))
         (println (str "SKIP (file not found): " sno#))))))

;; ── FENCE tests (058–067) ────────────────────────────────────────────────────

(defsuite fence-058-keyword          fence-dir "058_pat_fence_keyword")
(defsuite fence-059-fn-basic         fence-dir "059_pat_fence_fn_basic")
(defsuite fence-060-fn-fail          fence-dir "060_pat_fence_fn_fail")
(defsuite fence-061-fn-seal          fence-dir "061_pat_fence_fn_seal")
(defsuite fence-062-fn-outer         fence-dir "062_pat_fence_fn_outer")
(defsuite fence-063-fn-optional      fence-dir "063_pat_fence_fn_optional")
(defsuite fence-064-fn-capture       fence-dir "064_pat_fence_fn_capture")
(defsuite fence-065-fn-decimal       fence-dir "065_pat_fence_fn_decimal")
(defsuite fence-066-fn-nested        fence-dir "066_pat_fence_fn_nested")
(defsuite fence-067-fn-vs-kw         fence-dir "067_pat_fence_fn_vs_kw")

;; ── Budne csnobol4-suite (116 tests) ─────────────────────────────────────────
;; Excluded (8): bench, breakline, genc, k, ndbm, sleep, time, line2

(defsuite budne-100func    suite-dir "100func")
(defsuite budne-8bit       suite-dir "8bit")
(defsuite budne-8bit2      suite-dir "8bit2")
(defsuite budne-a          suite-dir "a")
(defsuite budne-alis       suite-dir "alis")
(defsuite budne-alph       suite-dir "alph")
(defsuite budne-alt1       suite-dir "alt1")
(defsuite budne-alt2       suite-dir "alt2")
(defsuite budne-any        suite-dir "any")
(defsuite budne-atn        suite-dir "atn")
(defsuite budne-bal        suite-dir "bal")
(defsuite budne-base       suite-dir "base")
(defsuite budne-breakx     suite-dir "breakx")
(defsuite budne-case1      suite-dir "case1")
(defsuite budne-case2      suite-dir "case2")
(defsuite budne-cat        suite-dir "cat")
(defsuite budne-char       suite-dir "char")
(defsuite budne-collect    suite-dir "collect")
(defsuite budne-collect2   suite-dir "collect2")
(defsuite budne-comment    suite-dir "comment")
(defsuite budne-contin     suite-dir "contin")
(defsuite budne-conv2      suite-dir "conv2")
(defsuite budne-convert    suite-dir "convert")
(defsuite budne-crlf       suite-dir "crlf")
(defsuite budne-diag1      suite-dir "diag1")
(defsuite budne-diag2      suite-dir "diag2")
(defsuite budne-digits     suite-dir "digits")
(defsuite budne-dump       suite-dir "dump")
(defsuite budne-end        suite-dir "end")
(defsuite budne-err        suite-dir "err")
(defsuite budne-fact       suite-dir "fact")
(defsuite budne-factor     suite-dir "factor")
(defsuite budne-file       suite-dir "file")
(defsuite budne-float      suite-dir "float")
(defsuite budne-float2     suite-dir "float2")
(defsuite budne-ftrace     suite-dir "ftrace")
(defsuite budne-fun1       suite-dir "fun1")
(defsuite budne-fun2       suite-dir "fun2")
(defsuite budne-func2      suite-dir "func2")
(defsuite budne-function   suite-dir "function")
(defsuite budne-hello      suite-dir "hello")
(defsuite budne-hide       suite-dir "hide")
(defsuite budne-include    suite-dir "include")
(defsuite budne-include2   suite-dir "include2")
(defsuite budne-include3   suite-dir "include3")
(defsuite budne-include4   suite-dir "include4")
(defsuite budne-ind        suite-dir "ind")
(defsuite budne-intval     suite-dir "intval")
(defsuite budne-json1      suite-dir "json1")
(defsuite budne-keytrace   suite-dir "keytrace")
(defsuite budne-label      suite-dir "label")
(defsuite budne-labelcode  suite-dir "labelcode")
(defsuite budne-len        suite-dir "len")
(defsuite budne-lexcmp     suite-dir "lexcmp")
(defsuite budne-lgt        suite-dir "lgt")
(defsuite budne-line       suite-dir "line")
(defsuite budne-loadErr    suite-dir "loaderr")
(defsuite budne-local      suite-dir "local")
(defsuite budne-longline   suite-dir "longline")
(defsuite budne-longrec    suite-dir "longrec")
(defsuite budne-loop       suite-dir "loop")
(defsuite budne-match      suite-dir "match")
(defsuite budne-match2     suite-dir "match2")
(defsuite budne-match3     suite-dir "match3")
(defsuite budne-match4     suite-dir "match4")
(defsuite budne-matchloop  suite-dir "matchloop")
(defsuite budne-maxint     suite-dir "maxint")
(defsuite budne-noexec     suite-dir "noexec")
(defsuite budne-nqueens    suite-dir "nqueens")
(defsuite budne-openi      suite-dir "openi")
(defsuite budne-openo      suite-dir "openo")
(defsuite budne-openo2     suite-dir "openo2")
(defsuite budne-ops        suite-dir "ops")
(defsuite budne-ord        suite-dir "ord")
(defsuite budne-pad        suite-dir "pad")
(defsuite budne-popen      suite-dir "popen")
(defsuite budne-popen2     suite-dir "popen2")
(defsuite budne-pow        suite-dir "pow")
(defsuite budne-preload1   suite-dir "preload1")
(defsuite budne-preload2   suite-dir "preload2")
(defsuite budne-preload3   suite-dir "preload3")
(defsuite budne-preload4   suite-dir "preload4")
(defsuite budne-punch      suite-dir "punch")
(defsuite budne-random     suite-dir "random")
(defsuite budne-repl       suite-dir "repl")
(defsuite budne-reverse    suite-dir "reverse")
(defsuite budne-rewind1    suite-dir "rewind1")
(defsuite budne-roman      suite-dir "roman")
(defsuite budne-scanerr    suite-dir "scanerr")
(defsuite budne-setexit    suite-dir "setexit")
(defsuite budne-setexit2   suite-dir "setexit2")
(defsuite budne-setexit3   suite-dir "setexit3")
(defsuite budne-setexit4   suite-dir "setexit4")
(defsuite budne-setexit5   suite-dir "setexit5")
(defsuite budne-setexit6   suite-dir "setexit6")
(defsuite budne-setexit7   suite-dir "setexit7")
(defsuite budne-space      suite-dir "space")
(defsuite budne-space2     suite-dir "space2")
(defsuite budne-spit       suite-dir "spit")
(defsuite budne-str        suite-dir "str")
(defsuite budne-substr     suite-dir "substr")
(defsuite budne-sudoku     suite-dir "sudoku")
(defsuite budne-t          suite-dir "t")
(defsuite budne-tab        suite-dir "tab")
(defsuite budne-trace1     suite-dir "trace1")
(defsuite budne-trace2     suite-dir "trace2")
(defsuite budne-trfunc     suite-dir "trfunc")
(defsuite budne-trim0      suite-dir "trim0")
(defsuite budne-trim1      suite-dir "trim1")
(defsuite budne-uneval     suite-dir "uneval")
(defsuite budne-uneval2    suite-dir "uneval2")
(defsuite budne-unsc       suite-dir "unsc")
(defsuite budne-update     suite-dir "update")
(defsuite budne-vdiffer    suite-dir "vdiffer")
(defsuite budne-words      suite-dir "words")
(defsuite budne-words1     suite-dir "words1")
