;; Rules of SPITBOL's measured on sbl -bf for the gate_cases_snobol4 suite on snobol4jvm, 2026-11-18. Each deftest runs its programs through -main, as the jar runs
;; them, and compares with what sbl -bf wrote: an exact test holds sbl's whole standard output, captured by the seat's generator, never typed by hand.
(ns SNOBOL4clojure.test-spitbol-rules-1118
  (:require [clojure.test :refer [deftest is]]
            [clojure.string]
            [SNOBOL4clojure.core :refer [-main]]))

;; [stdout rc] of src run by -main with args before the file; @OUTFILE@ in src names a temporary file
(defn- run-sno
  ([src] (run-sno src []))
  ([src args]
   (let [f (java.io.File/createTempFile "r1118" ".sno") o (java.io.File/createTempFile "r1118" ".out")]
     (try
       (spit f (clojure.string/replace src "@OUTFILE@" (.getPath o)) :encoding "ISO-8859-1")
       (let [rc (atom nil) out (with-out-str (binding [*err* (java.io.StringWriter.)] (reset! rc (apply -main (concat args [(.getPath f)])))))]
         [out @rc])
       (finally (.delete f) (.delete o))))))


;; A trace function is a function called at the trace as any other: &TRACE alone is zeroed while it runs, &FTRACE traces its call and return and is charged,
;; a system function serves, a RETURN trace function reads &RTNTYPE and the returning function's names still bound, and the &FTRACE line after it is the caller's
;; statement's (manual ch. 10, ch. 19 TRACE). MEASURED on sbl -bf, each program's whole output; corpus gate_cases_snobol4 a_trace_callback_is_ftraced_like_any_
;; called_function, a_return_trace_function_sees_the_returning_functions_names_still_bound, a_traced_call_keeps_its_procedure_and_its_value_across_a_collection,
;; a_value_trace_callback_is_entered_by_the_assignment_box_and_not_by_c, a_return_trace_callback_prints_the_deferred_banner_with_the_callers_statement_number
(deftest a-trace-function-is-called-as-any-function
  (is (= "****6*******  CB(.X,'tg')\ncb X tg T=0 F=49\n****4*******  RETURN CB = ''\ndone T=99 F=48\n"
         (first (run-sno (str "        &TRACE = 100\n        &FTRACE = 50\n        DEFINE('CB(NAME,TAG)')  :(S)\nCB      OUTPUT = 'cb ' NAME ' ' TAG ' T=' &TRACE ' F=' &FTRACE  :(R"
              "ETURN)\nS       TRACE('X', 'VALUE', 'tg', .CB)\n        X = 5\n        OUTPUT = 'done T=' &TRACE ' F=' &FTRACE\nEND\n")))))
  (is (= "****8*******  CB(.F,'tg')\ncb F tg\n****4*******  RETURN CB = ''\n****8*******  F(1)\n****6*******  RETURN F = 1\ndone\n"
         (first (run-sno (str "        &TRACE = 100\n        &FTRACE = 100000\n        DEFINE('CB(NAME,TAG)')  :(S)\nCB      OUTPUT = 'cb ' NAME ' ' TAG  :(RETURN)\n"
              "S       DEFINE('F(A)')  :(S2)\nF       F = A  :(RETURN)\nS2      TRACE('F', 'CALL', 'tg', .CB)\n        X = F(1)\n        OUTPUT = 'done'\n"
              "END\n")))))
  (is (= "fret X=[zz] K=[gl] type=FRETURN\nafter x=outer\n"
         (first (run-sno (str "        CODE('G K = \"gl\";  G = X :(FRETURN)')\n        DEFINE('G(X)K', 'G')\n        DEFINE('TRF()')                         :(TRF.END)\n"
              "TRF     OUTPUT = 'fret X=[' X '] K=[' K '] type=' &RTNTYPE :(RETURN)\nTRF.END X = 'outer'\n        TRACE('G', 'RETURN', , 'TRF')\n"
              "        &TRACE = 100\n        G('zz')\n        OUTPUT = 'after x=' X\nEND\n")))))
  (is (= "TF F F=abcdefabcdef\nR=changed-by-tf\nF after=\n"
         (first (run-sno (str " DEFINE('F(X,Y)A')\n DEFINE('TF(N,T)')  :(TFE)\nTF OUTPUT = 'TF ' N ' F=' F\n F = IDENT(T,'RET') 'changed-by-tf'  :(RETURN)\nTFE\n"
              " TRACE('F','RETURN','RET','TF')\n &TRACE = 10000\n OUTPUT = 'R=' F('abc', 'def')\n OUTPUT = 'F after=' F    :(END)\nF A = X Y\n F = A A              "
              "              :(RETURN)\nEND\n")))))
  (is (= "done two\n"
         (first (run-sno "        TRACE('X','VALUE','','IDENT')\n        &TRACE = 100\n        X = 1\n        X = 'two'\n        OUTPUT = 'done ' X\nEND\n"))))
  (is (= (str "****11******  G(1)\n****9******* i CB(.F,'tg')\ncb F tg\n****4******* i RETURN CB = ''\n****9******* i F(1)\n****7******* i CB(.F,'tg')\n"
              "cb F tg\n****4******* i RETURN CB = ''\n****9******* i RETURN F = 2\n****9*******  RETURN G = 3\n3\n****14******  G(2)\n****9******* i CB(.F,'tg')\n"
              "cb F tg\n****4******* i RETURN CB = ''\n****9******* i F(2)\n****6******* ii F = 2\n****7******* ii F = 3\n****7******* i CB(.F,'tg')\n"
              "cb F tg\n****4******* i RETURN CB = ''\n****9******* i RETURN F = 3\n****9*******  RETURN G = 4\n")
         (first (run-sno (str "        &TRACE = 100\n        &FTRACE = 100000\n        DEFINE('CB(NAME,TAG)')\t:(S)\nCB      OUTPUT = 'cb ' NAME ' ' TAG\t:(RETURN)\n"
              "S       DEFINE('F(A)')\t:(S1)\nF       F = A\n        F = F + 1\t:(RETURN)\nS1      DEFINE('G(A)')\t:(S2)\nG       G = F(A) + 1\t:(RETURN)\n"
              "S2      TRACE('F', 'FUNCTION', 'tg', .CB)\n        X = G(1)\n        OUTPUT = X\n        TRACE('F')\n        Y = G(2)\nEND\n"))))))
