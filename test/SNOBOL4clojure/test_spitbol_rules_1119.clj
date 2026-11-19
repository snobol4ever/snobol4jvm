;; Rules of SPITBOL's measured on sbl -bf for the SNOBOL4 suites on snobol4jvm, 2026-11-19. Each deftest runs its programs through -main, as the jar runs them,
;; and compares with what sbl -bf wrote for the same program.
(ns SNOBOL4clojure.test-spitbol-rules-1119
  (:require [clojure.test :refer [deftest is]]
            [clojure.string]
            [SNOBOL4clojure.core :refer [-main]]))

;; [stdout rc] of src run by -main (rc nil when the program ends at END)
(defn- run-sno [src]
  (let [f (java.io.File/createTempFile "r1119" ".sno")]
    (try
      (spit f src :encoding "ISO-8859-1")
      (let [rc (atom nil) out (with-out-str (binding [*err* (java.io.StringWriter.)] (reset! rc (-main (.getPath f)))))]
        [out @rc])
      (finally (.delete f)))))

;; "In SPITBOL, extra arguments are ignored, and missing arguments are supplied as null strings" (manual ch. 19, APPLY): a built-in handed more arguments than
;; it takes is called with those it takes. MEASURED on sbl -bf, this program's whole output; corpus snobol4-rungs eval_line__a1_a_second_argument (EVAL("F()", 1)).
(deftest a-built-in-ignores-its-extra-arguments
  (is (= "3\nARRAY TABLE\nARRAY ARRAY\n17 ok\n317\n"
         (first (run-sno (str "        &ERRLIMIT = 10\n        TA = TABLE()\n        TA<1> = 2\n        OUTPUT = EVAL('1 + 2', 'x')\n"
                       "        OUTPUT = DATATYPE(ARRAY(2, 0, 'x')) ' ' DATATYPE(TABLE(1, 1, 'd', 'x'))\n"
                       "        OUTPUT = DATATYPE(SORT(TA, 1, 'x')) ' ' DATATYPE(RSORT(TA, 1, 'x'))\n"
                       "        OUTPUT = SIZE(DATE(0, 'x')) ' ' SETEXIT('', 'x') 'ok'\n        BACKSPACE(99, 'x')\n        OUTPUT = &ERRTYPE\nEND\n"))))))

;; The name of a natural variable is its text where ARRAY wants a prototype, as where a number is wanted; the name of an element has no text and stays 064.
;; MEASURED on sbl -bf, this program's whole output: ARRAY(.$'3', .$'7') is an ARRAY 3 of 7s, ARRAY(.$'2:4') has bounds 2:4, ARRAY(.X) of X = 'abc' is 066 as
;; ARRAY('X') is, ARRAY(.A<1>) 064; corpus snobol4-rungs array_convert_dupl_replace_1.
(deftest a-variables-name-is-its-text-as-an-array-prototype
  (is (= "3 7\n2:4\n66\n64\n"
         (first (run-sno (str "        &ERRLIMIT = 10\n        N3 = .$'3'\n        N7 = .$'7'\n        V = ARRAY(N3, N7)\n        OUTPUT = PROTOTYPE(V) ' ' V<3>\n"
                       "        OUTPUT = PROTOTYPE(ARRAY(.$'2:4'))\n        X = 'abc'\n        V = ARRAY(.X)\n        OUTPUT = &ERRTYPE\n"
                       "        A = ARRAY(3)\n        V = ARRAY(.A<1>)\n        OUTPUT = &ERRTYPE\nEND\n"))))))

;; A compile at run time that fails in a function's statement leaves the stage at compile (sbl.min err04, errc4): at &ERRLIMIT 0 a later error in a function then
;; fails its innermost failure point and the program goes on, until a compile at run time succeeds. MEASURED on sbl -bf, this program: after F's EVAL('X +'),
;; G's LN(0) takes :F with &ERRTYPE 307, ~LN(0) succeeds and (LN(0), 'b') is 'b', twice; after EVAL('1') the same error ends the run; corpus snolib LOG.tst.
(deftest a-failed-compile-in-a-function-leaves-a-functions-errors-failing
  (let [out (first (run-sno (str "         DEFINE('F()') :(F_E)\nF        EVAL('X +') :(RETURN)\nF_E      DEFINE('G(X)') :(G_E)\n"
                                 "G        G = LN(X) :S(RETURN)F(GF)\nGF       G = 'f' &ERRTYPE ~LN(X) (LN(X), 'b') :(RETURN)\n"
                                 "G_E      F()\n         OUTPUT = G(0) ' ' G(0)\n         EVAL('1')\n         OUTPUT = 'not ' G(0)\nEND\n")))]
    (is (clojure.string/starts-with? out "f307b f307b\n"))
    (is (clojure.string/includes? out "(4) : ERROR 307 -- ln produced real overflow"))
    (is (not (clojure.string/includes? out "not ")))))

;; A pattern function's value is a PATTERN, BREAKX's and one given a deferred argument too, where *X alone is an EXPRESSION, and EVAL refuses a pattern with 103.
;; MEASURED on sbl -bf, these programs' whole output: DATATYPE(BREAKX('a')) is PATTERN (snobol4jvm read EXPRESSION); BREAK(*X), ANY(*X), SPAN(*X), NOTANY(*X),
;; LEN(*X), POS(*X), TAB(*X), ARBNO(*X), *X | 'a' and FENCE(*X) are PATTERNs; EVAL(BREAK(*X)) and EVAL(LEN(*N)) are 103.
(deftest a-pattern-functions-value-is-a-pattern-deferred-or-not
  (is (= "PATTERN PATTERN PATTERN\nxx\n"
         (first (run-sno (str "        OUTPUT = DATATYPE(BREAKX('a')) ' ' DATATYPE(BREAK('a')) ' ' DATATYPE(BREAKX(*X))\n        X = 'a'\n"
                              "        'xxa' BREAKX('a') . Y\n        OUTPUT = Y\nEND\n")))))
  (is (= "EXPRESSION PATTERN PATTERN PATTERN PATTERN\nPATTERN PATTERN PATTERN PATTERN PATTERN PATTERN\n"
         (first (run-sno (str "        OUTPUT = DATATYPE(*X) ' ' DATATYPE(BREAK(*X)) ' ' DATATYPE(ANY(*X)) ' ' DATATYPE(SPAN(*X)) ' ' DATATYPE(NOTANY(*X))\n"
                              "        OUTPUT = DATATYPE(LEN(*X)) ' ' DATATYPE(POS(*X)) ' ' DATATYPE(TAB(*X)) ' ' DATATYPE(ARBNO(*X)) ' ' DATATYPE(*X | 'a') ' '"
                              " DATATYPE(FENCE(*X))\nEND\n")))))
  (is (= "brk 103 STRING\nlen 103\n"
         (first (run-sno (str "        &ERRLIMIT = 10\n        X = 'a'\n        Y = EVAL(BREAK(*X))\n        OUTPUT = 'brk ' &ERRTYPE ' ' DATATYPE(Y)\n        &ERRTYPE = 0\n"
                              "        Y = EVAL(LEN(*N))\n        OUTPUT = 'len ' &ERRTYPE\nEND\n"))))))
