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
