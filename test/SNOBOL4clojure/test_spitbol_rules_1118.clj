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

;; SPITBOL's sort order (sbl.min sortc): strings as strings, numbers as numbers, two objects of one other type by creation, anything else by its type's id.
;; MEASURED on sbl -bf, the output; corpus gate_cases_snobol4 aggregate_registry_keys_are_descrs__w3, a table keyed by twelve arrays
(deftest sort-orders-mixed-keys-as-spitbols-sortc
  (is (= " 2. 5 20 10 3 a\n"
         (first (run-sno (str "        A = ARRAY(6)\n        A[1] = '10'\n        A[2] = 5\n        A[3] = '3'\n        A[4] = 20\n        A[5] = 'a'\n        A[6] = 2.0\n"
              "        B = SORT(A)\n        I = 1\nL       OUT = OUT ' ' B[I]\n        I = LT(I,6) I + 1 :S(L)\n        OUTPUT = OUT\nEND\n")))))
  (is (= "1 2 3 4 5 6 7 8 9 10 11 12 \n"
         (first (run-sno (str "        T = TABLE()\n        K = ARRAY(12)\n        I = 1\nL1      K[I] = ARRAY(1)\n        T[K[I]] = I\n        I = LT(I, 12) I + 1              :S("
              "L1)\n        B = SORT(T)\n        I = 1\nL2      OUT = OUT B[I,2] \" \"\n        I = LT(I, 12) I + 1              :S(L2)\n        OUTPUT = OUT\n"
              "END\n"))))))

;; CONVERT's second argument is a string: the null string, a pattern or an array is "074 convert second argument is not a string" (Appendix D). MEASURED on sbl -bf;
;; corpus gate_cases_snobol4 a_builtin_called_short_or_over_its_arity_pads_with_null_and_drops_extras__cnv
(deftest convert-takes-a-string-type-else-error-74
  (is (= "a 74\nb 74\nc 74\nd 1\ne 1\n"
         (first (run-sno (str "        &ERRLIMIT = 10\n        X = CONVERT('1', '')\n        OUTPUT = 'a ' &ERRTYPE\n        X = CONVERT('1', LEN(1))\n        OUTPUT = 'b ' &ERRTYP"
              "E\n        X = CONVERT('1', ARRAY(2))\n        OUTPUT = 'c ' &ERRTYPE\n        &ERRTYPE = 1\n        X = CONVERT('1', 'FOO')\n        OUTPUT = 'd ' &"
              "ERRTYPE\n        X = CONVERT('1', .Q)\n        OUTPUT = 'e ' &ERRTYPE\nEND\n"))))))

;; A call binds a parameter that is output-associated without I/O, and its body reads what was bound (sbl.min b_pfc). MEASURED on sbl -bf; corpus gate_cases_snobol4
;; a_call_saves_binds_and_restores_an_io_associated_name_without_io__outparam
(deftest a-call-binds-an-output-associated-name-without-io
  (is (= "got arg\n"
         (first (run-sno (str "        OUTPUT(.V, 7, '@OUTFILE@')\n        V = 'first'\n        DEFINE('F(V)')                          :(F_END)\nF       F = 'got ' V              "
              "              :(RETURN)\nF_END\n        OUTPUT = F('arg')\n        V = 'last'\n        ENDFILE(7)\nEND\n"))))))

;; At &ERRLIMIT 0 a pattern's own check of its deferred argument, 043 to 058, ends the program, where an error inside the unevaluated expression fails the match.
;; MEASURED on sbl -bf; corpus gate_cases_snobol4 a_deferred_cset_argument_that_evaluates_to_null_or_a_pattern_raises_spitbols_error__f
(deftest a-patterns-own-argument-error-is-fatal-in-a-match
  (let [[out] (run-sno "        S = ''\n        'abc' ? SPAN(*S)\n        OUTPUT = 'after'\nEND\n")]
    (is (re-find #"\(2\) : ERROR 056 -- span evaluated argument is not a string" out))
    (is (not (re-find #"(?m)^after$" out))))
  (is (= "n\n" (first (run-sno "        S2 = 'x'\n        'abc' ? *(1 + S2)  :S(Y)F(N)\nY       OUTPUT = 'y'  :(END)\nN       OUTPUT = 'n'\nEND\n")))))

;; Under &ERRLIMIT with a SETEXIT label armed, a compile error in CODE's text is raised as an error and the label takes it; with none armed CODE fails.
;; MEASURED on sbl -bf; corpus gate_cases_snobol4 a_goto_field_takes_a_blank_before_its_paren_and_refuses_an_empty_field__ce
(deftest a-code-compile-error-goes-to-the-setexit-label
  (is (= "trapped 219\nran 219 9\nno label 226 8\n"
         (first (run-sno (str "        &ERRLIMIT = 10\n        SETEXIT('h')\n        C = CODE(' X = 1 :')   :S(OK)\n        OUTPUT = 'ran ' &ERRTYPE ' ' &ERRLIMIT   :(N)\n"
              "OK      OUTPUT = 'fell'  :(END)\nh       OUTPUT = 'trapped ' &ERRTYPE   :(CONTINUE)\nN       C = CODE(' X = (')   :S(OK)\n        OUTPUT = 'no label "
              "' &ERRTYPE ' ' &ERRLIMIT\nEND\n"))))))

;; The run ends with the exit code &CODE holds, modulo 256 and 0 when negative, however it ends (manual ch. 7, &CODE). MEASURED on sbl -bf: 3 exits 3, after an
;; execution error too, 300 exits 44, -5 exits 0; corpus gate_cases_snobol4 test_gate_harness_road_lib_reads_the_per_process_verdict__code
(deftest the-exit-code-is-code-modulo-256
  (is (= 3 (second (run-sno "        &CODE = 3\nEND\n"))))
  (is (= 44 (second (run-sno "        &CODE = 300\nEND\n"))))
  (is (= 5 (second (run-sno "        &CODE = 5\n        A = 'a'\n        X = 1 + A\nEND\n"))))
  (is (nil? (second (run-sno "        &CODE = 256\nEND\n")))))

;; Unary / is no operator until OPSYN makes it one: it is not folded, and its 029 comes as it runs. MEASURED on sbl -bf; corpus gate_cases_snobol4
;; a_unary_and_a_binary_opsyn_of_one_operator_are_two_functions__ops
(deftest unary-slash-is-undefined-until-it-runs
  (is (= "slash u<a>\n"
         (first (run-sno (str "        DEFINE('U(X)')                          :(U.END)\nU       U = 'u<' X '>'                          :(RETURN)\nU.END   OPSYN('/', .U, 1)\n"
              "        OUTPUT = 'slash ' /'a'\nEND\n"))))))

;; A value with no text where a pattern is wanted is its operator's error: 240 the match, 005 and 006 alternation (right operand first), 061 ARBNO, 259 FENCE,
;; 046 *T, 030 the . capture, 025 the $ capture (Appendix D). MEASURED on sbl -bf; corpus gate_cases_snobol4
;; an_array_or_table_as_a_pattern_operand_raises_the_operand_errors_of_spitbol_in_both_modes__a
(deftest a-non-pattern-operand-is-its-operators-error
  (is (= (str "1 ERROR 240\n2 ERROR 240\n3 ERROR 5\n4 ERROR 6\n5 ERROR 8\n6 ERROR 5\n7 ERROR 6\n8 ERROR 6\n9 ERROR 6\n10 ERROR 5\n11 ERROR 8\n12 ERROR 8\n"
              "13 ERROR 9\n14 ERROR 240\n15 ERROR 6\n16 ERROR 9\n17 ok\n18 ok\n19 ok\n20 ok\n21 ERROR 61\n22 ERROR 259\n23 ERROR 46\n24 ERROR 30\n"
              "25 ERROR 25\n26 ERROR 30\n")
         (first (run-sno (str "        &ERRLIMIT = 1000\n        t = TABLE()\n        a = ARRAY(3)\n        p = LEN(1)\n        i = 0\nnxt     i = i + 1\n        SETEXIT(.errh)\n"
              "        :($('C' i))\nC1      '0' ? t                         :(ok)\nC2      'ab' ? a                        :(ok)\nC3      x = LEN(2) | a            "
              "      :(ok)\nC4      x = a | LEN(2)                  :(ok)\nC5      a[1] ? t 'a'                    :(ok)\nC6      'ab' ? 'a' | a                  :("
              "ok)\nC7      'ab' ? a | 'a'                  :(ok)\nC8      'ab' ? 'a' | a | 'b'            :(ok)\nC9      'ab' ? a | 'a' | 'b'            :(ok)\n"
              "C10     'ab' ? 'a' | 'b' | a            :(ok)\nC11     'ab' ? 'a' a 'b'                :(ok)\nC12     'ab' ? a 'a' 'b'                :(ok)\n"
              "C13     'ab' ? 'a' 'b' a                :(ok)\nC14     x = ('a' ? a)                   :(ok)\nC15     x = ('a' ? t | 'a')             :(ok)\n"
              "C16     'ab' ? p t                      :(ok)\nC17     'ab' ? p 'b'                    :(ok)\nC18     'ab' ? p | 'b'                  :(ok)\n"
              "C19     x = p | 'b'                     :(ok)\nC20     x = 'ab' ? p                    :(ok)\nC21     'ab' ? ARBNO(t)                 :(ok)\n"
              "C22     'ab' ? FENCE(t)                 :(ok)\nC23     'ab' ? *t                       :(ok)\nC24     'ab' ? t . x                    :(ok)\n"
              "C25     'ab' ? a $ x                    :(ok)\nC26     x = ('ab' ? t . y)              :(ok)\nC27     :(END)\nok      OUTPUT = i ' ok'               "
              " :(nxt)\nerrh    OUTPUT = i ' ERROR ' &ERRTYPE   :(nxt)\nEND\n"))))))

;; A function that returns a keyword's name by NRETURN gives the keyword's value. MEASURED on sbl -bf; corpus gate_cases_snobol4
;; an_nreturn_fix_up_is_emitted_by_the_box_and_agrees_with_sbl_in_a_value_and_a_name_context__n
(deftest an-nreturn-of-a-keywords-name-reads-the-keyword
  (is (= "K value 16777216\n"
         (first (run-sno (str "        DEFINE('K()')                           :(K.E)\nK       K = .&MAXLNGTH                          :(NRETURN)\nK.E     Y = K()\n"
              "        OUTPUT = 'K value ' Y\nEND\n"))))))

;; ARRAY's prototype as SPITBOL reads it (manual ch. 19, sbl.min s_arr): a whole integer is one dimension and the prototype, any other string its dimensions,
;; kept as written; 064, 065, 066 and 067 for what will not do. MEASURED on sbl -bf; corpus gate_cases_snobol4 array_first_argument_converts_and_raises_as_spitbol_does__access
(deftest array-reads-its-prototype-as-spitbol-does
  (is (= "dx| 2 : 4 , 3 |\nzero|:2|\n7|1.5,2\n2 10 3\n66\n65\n64\n67\n67\n"
         (first (run-sno (str "\tA = ARRAY(' 2 : 4 , 3 ', 'd')\n\tA[4,3] = 'x'\n\tOUTPUT = A[2,1] A[4,3] '|' PROTOTYPE(A) '|'\n\tB = ARRAY(':2')\n\tB[0] = 'zero'\n"
              "\tOUTPUT = B[0] '|' PROTOTYPE(B) '|'\n\tC = ARRAY('1.5,2', 7)\n\tOUTPUT = C[1,2] '|' PROTOTYPE(C)\n\tE = ARRAY(2.9)\n\tOUTPUT = PROTOTYPE(E) ' ' PROT"
              "OTYPE(ARRAY('1e1')) ' ' PROTOTYPE(ARRAY(' 3'))\n\t&ERRLIMIT = 20\n\tX = ARRAY('x')\n\tOUTPUT = &ERRTYPE\n\tX = ARRAY('a:2')\n\tOUTPUT = &ERRTYPE\n"
              "\tX = ARRAY(LEN(1))\n\tOUTPUT = &ERRTYPE\n\tX = ARRAY('3:1')\n\tOUTPUT = &ERRTYPE\n\tX = ARRAY('2,')\n\tOUTPUT = &ERRTYPE\nEND\n"))))))

;; A string converted to a PATTERN matches its text and stays a PATTERN when assigned. MEASURED on sbl -bf; corpus gate_cases_snobol4
;; convert_of_a_matched_substring_is_that_substring__conv
(deftest convert-to-pattern-matches-the-strings-text
  (is (= "PATTERN\n[QR]\n[QR]\n"
         (first (run-sno (str "        P = CONVERT('QR', 'PATTERN')\n        OUTPUT = DATATYPE(P)\n        'xxQRyy' P . M\n        OUTPUT = '[' M ']'\n        'QRST' LEN(2) . C6\n"
              "        'xxQRyy' CONVERT(C6, 'PATTERN') . M2\n        OUTPUT = '[' M2 ']'\nEND\n"))))))

;; A numeric string reads as sbl.min gtnum reads it: an exponent marker with no digits is the exponent 0 when a blank follows it, and a sign that only blanks
;; follow is 0. MEASURED on sbl -bf; corpus gate_cases_snobol4 convert_reads_a_numeric_string_as_spitbol_gtnum_does__w
(deftest a-numeric-string-reads-as-gtnum-reads-it
  (is (= (str "[1e ] I:1 R:1. N:REAL 1.\n[1e+ ] I:1 R:1. N:REAL 1.\n[1d ] I:1 R:1. N:REAL 1.\n[1e] I:FAIL R:FAIL N:FAIL\n[1e 2] I:FAIL R:FAIL N:FAIL\n"
              "[- ] I:0 R:0. N:INTEGER 0\n[ - ] I:0 R:0. N:INTEGER 0\n[+ ] I:0 R:0. N:INTEGER 0\n[-] I:FAIL R:FAIL N:FAIL\n[- 5] I:FAIL R:FAIL N:FAIL\n")
         (first (run-sno (str "        DEFINE('C(S)')  :(CE)\nC       OUT = '[' S ']'\n        OUT = OUT ' I:' (CONVERT(S, 'INTEGER') , 'FAIL')\n        OUT = OUT ' R:' (CONVERT(S,"
              " 'REAL') , 'FAIL')\n        X = CONVERT(S, 'NUMERIC')   :F(CN)\n        OUT = OUT ' N:' DATATYPE(X) ' ' X  :(CO)\nCN      OUT = OUT ' N:FAIL'\n"
              "CO      OUTPUT = OUT  :(RETURN)\nCE      C('1e ')\n        C('1e+ ')\n        C('1d ')\n        C('1e')\n        C('1e 2')\n        C('- ')\n"
              "        C(' - ')\n        C('+ ')\n        C('-')\n        C('- 5')\nEND\n"))))))

;; A real that truncates to 2147483648 converts to no integer, sbl's rti failing there alone. MEASURED on sbl -bf; corpus gate_cases_snobol4
;; convert_reads_a_numeric_string_as_spitbol_gtnum_does__w
(deftest a-real-of-two-to-the-31-converts-to-no-integer
  (is (= "2147483648.5 FAIL\n2147483648. FAIL\n2147483647.5 2147483647\n2147483649.5 2147483649\n-2147483648.5 -2147483648\n2147483648.5 FAIL\n"
         (first (run-sno (str "        DEFINE('C(R)')  :(CE)\nC       OUTPUT = R ' ' (CONVERT(R, 'INTEGER') , 'FAIL')  :(RETURN)\nCE      C(2147483648.5)\n        C(2147483648.0)\n"
              "        C(2147483647.5)\n        C(2147483649.5)\n        C(-2147483648.5)\n        C('2147483648.5')\nEND\n"))))))

;; The dump names an array by its prototype as ARRAY was given it (manual ch. 7, &DUMP). MEASURED on sbl -bf; corpus gate_cases_snobol4 dump_matches_spitbol__dump2b
(deftest a-dump-names-an-array-by-its-prototype
  (let [[out] (run-sno "        C = ARRAY('1:3')\n        A = ARRAY(' 2 : 4 , 3 ')\n        &DUMP = 1\nEND\n")]
    (is (re-find #"(?m)^C = ARRAY\(1:3\) #1$" out))
    (is (re-find #"(?m)^A = ARRAY\( 2 : 4 , 3 \) #2$" out))))

;; sbl's -tN sets the page width: "page N" ends two columns short of it and every listed line, error line and dump line is cut to it (manual ch. 13).
;; MEASURED on sbl -bf; corpus gate_cases_snobol4 test_gate_sno_any_notany_span_break_breakx_of_the_null_string_or_a_pattern_raise_spitbols_error__c1, run -t400,
;; and dump_matches_spitbol__dump2b, a 200-character value dumped on lines of 120
(deftest the-page-width-is-the-t-switchs
  (doseq [[args n] [[["-t80"] 78] [["-t400"] 398] [[] 118]]]
    (let [[out rc] (run-sno "        X = SPAN('')\nEND\n" args)]
      (is (= 231 rc))
      (is (= [n] (map count (filter #(re-find #"page 1$" %) (clojure.string/split-lines out)))) (str args))))
  (let [[out] (run-sno "        X = DUPL('b',200)\n        &DUMP = 1\nEND\n")
        ls (clojure.string/split-lines out)]
    (is (some #(= (str "X = '" (apply str (repeat 115 \b))) %) ls))
    (is (some #(= (str (apply str (repeat 85 \b)) "'") %) ls))))

;; A unit already open in a direction takes no second file: 289 for INPUT, 290 for OUTPUT; a null file argument shares it. MEASURED on sbl -bf; corpus gate_cases_snobol4
;; input_honours_the_f_and_l_file_options__reassoc
(deftest a-unit-open-in-one-direction-takes-no-second-file
  (is (= "fails 290\nshared\nfails3 289\n"
         (first (run-sno (str "        &ERRLIMIT = 5\n        OUTPUT(.X, 6, '@OUTFILE@')\n        OUTPUT(.Y, 6, '@OUTFILE@')  :F(F1)\n        OUTPUT = 'ok1'  :(N)\n"
              "F1      OUTPUT = 'fails ' &ERRTYPE\nN       OUTPUT(.Y, 6)  :F(F2)\n        OUTPUT = 'shared'  :(M)\nF2      OUTPUT = 'fails2 ' &ERRTYPE\n"
              "M       INPUT(.Z, 6, '@OUTFILE@')  :F(F3)\n        OUTPUT = 'ok3'  :(END)\nF3      OUTPUT = 'fails3 ' &ERRTYPE\nEND\n"))))))

;; HOST(-1,2), the heap's base, is above 0 and HOST(-1,3) not below it. MEASURED on sbl -bf; corpus gate_cases_snobol4 host_minus_one_answers_the_memory_statistics__w
(deftest host-minus-one-gives-a-base-above-zero
  (is (= "base positive\ntop not below base\n"
         (first (run-sno "        OUTPUT = GT(HOST(-1,2),0) 'base positive'\n        OUTPUT = GE(HOST(-1,3),HOST(-1,2)) 'top not below base'\nEND\n")))))

;; TRIM drops the trailing blanks and tabs and no other character (manual ch. 19, TRIM). MEASURED on sbl -bf; corpus gate_cases_snobol4
;; trim_drops_a_trailing_tab_and_scrips_own_builtins_are_undefined__tr
(deftest trim-drops-trailing-blanks-and-tabs-alone
  (is (= "3\n3\n4\n4\n4\n4\n"
         (first (run-sno (str "        OUTPUT = SIZE(TRIM('abc' CHAR(9)))\n        OUTPUT = SIZE(TRIM('abc ' CHAR(9) ' '))\n        OUTPUT = SIZE(TRIM('abc' CHAR(13)))\n"
              "        OUTPUT = SIZE(TRIM('abc' CHAR(10)))\n        OUTPUT = SIZE(TRIM('abc' CHAR(11)))\n        OUTPUT = SIZE(TRIM('abc' CHAR(12)))\n"
              "END\n"))))))

;; SIZE answers an INTEGER that converts as any integer does. MEASURED on sbl -bf; corpus gate_cases_snobol4 substr_indexes_bytes_as_the_pattern_cursor_does__census
(deftest size-answers-an-integer
  (is (= "INTEGER\n2\n"
         (first (run-sno "        OUTPUT = DATATYPE(SIZE('abc'))\n        OUTPUT = SIZE(CONVERT(SIZE('abcdefghijkl'), 'STRING'))\nEND\n")))))

;; REPLACE maps a character its second argument holds twice by its last place. MEASURED on sbl -bf; corpus gate_cases_snobol4
;; replace_is_an_asm_leaf_that_agrees_with_sbl_and_its_c_twin__rep
(deftest replace-maps-a-repeated-character-by-its-last-place
  (is (= "OZAB\nyyz\n"
         (first (run-sno (str "        OUTPUT = REPLACE('lazy', 'abcdefghijklmzopqrstuvwxyz', 'ZYXWVUTSRQPONMLKJIHGFEDCBA')\n        OUTPUT = REPLACE('aab', 'aab', 'xyz')\n"
              "END\n"))))))

;; A return at function level 0 is "242 function return from level zero": the label SETEXIT armed takes it under &ERRLIMIT, else it ends the program whatever
;; &ERRLIMIT holds. MEASURED on sbl -bf; corpus gate_cases_snobol4 test_gate_errlimit_survival_is_measured_per_code__e242
(deftest a-return-at-level-zero-is-error-242
  (let [[out] (run-sno "        &ERRLIMIT = 10\n        OUTPUT = 'B'  :(RETURN)\n        OUTPUT = 'C'\nEND\n")]
    (is (re-find #"\A[BR]?B\n" (str "R" out)))
    (is (re-find #"\(2\) : ERROR 242 -- function return from level zero" out))
    (is (not (re-find #"(?m)^C$" out))))
  (is (= "B\nh 242\n" (first (run-sno "        &ERRLIMIT = 10\n        SETEXIT('H')\n        OUTPUT = 'B'  :(RETURN)\n        OUTPUT = 'C'  :(END)\nH       OUTPUT = 'h ' &ERRTYPE\nEND\n")))))

;; A control card may be followed by a comma and another card, each taking effect, the line listed once; -TITLE takes the rest of its line (manual ch. 14).
;; MEASURED on sbl -bf; corpus gate_cases_snobol4 test_gate_snobol4_list_listing_matches_sbl__p_chain
(deftest control-cards-chain-with-commas
  (let [[out] (run-sno "-LIST,-EJECT\n A = 1\n-TITLE x,-LIST\n\tB = 2\n OUTPUT = A + B\nEND\n")
        ls (clojure.string/split-lines out)]
    (is (some #(= "        -LIST,-EJECT" %) ls))
    (is (some #(re-matches #"x,-LIST +page 2" %) ls))
    (is (some #(= "        -TITLE x,-LIST" %) ls))))

;; -NOEXECUTE compiles the program and does not run it, exit code 231; a later -EXECUTE runs it (manual ch. 14). MEASURED on sbl -bf; corpus gate_cases_snobol4
;; test_gate_snobol4_noexecute_matches_sbl__noex_list
(deftest noexecute-compiles-and-does-not-run
  (let [[out rc] (run-sno "-NOEXECUTE\n        OUTPUT = 'x'\nEND\n")]
    (is (= 231 rc))
    (is (not (re-find #"(?m)^x$" out))))
  (is (= ["x\n" nil] (run-sno "-NOEXECUTE\n-EXECUTE\n        OUTPUT = 'x'\nEND\n"))))

;; A statement's ? binds loosest, so a replacement after it needs the subject before it to be a name: a concatenation, or an earlier match's value, is 212,
;; with no column. MEASURED on sbl -bf; corpus gate_cases_snobol4 a_statement_level_query_takes_a_concatenated_subject_and_chains_left_to_right__refuse1 and 2
(deftest a-replacement-after-a-statement-query-needs-a-name
  (doseq [src ["        X = 'a'\n        X 'b' ? LEN(1) = 'Z'\nEND\n" "        Y = 'hello'\n        Y ? 'l' ? '' = 'L'\nEND\n"]]
    (let [[out rc] (run-sno src)]
      (is (= 231 rc))
      (is (re-find #"\(2\) : ERROR 212 -- syntax error: value used where name is required" out) src)))
  (is (= ["c\n" nil] (run-sno "        X = 'ab'\n        X ? 'a' 'b' = 'c'\n        OUTPUT = X\nEND\n"))))

;; A line whose label field begins with what no label may is one statement, its semicolons no ends; a quote in a label opens no literal; an operator touching the
;; subject is 233. MEASURED on sbl -bf; corpus gate_cases_snobol4 test_gate_s4e_donewhen_runs_the_whole_criterion__w, a shell script read as a program
(deftest a-bad-label-and-a-labels-quotes-read-as-spitbol-reads-them
  (let [errs (fn [src] (vec (re-seq #"\(\d+(?:,\d+)?\) : ERROR \d+" (first (run-sno src)))))]
    (is (= ["(1) : ERROR 214"] (errs "#   a; b\n        OUTPUT = 'n'\nEND\n")))
    (is (= ["(1,15) : ERROR 224"] (errs "HERE=\"$(cd \"x\")\"; M=\"a\"\nEND\n")))
    (is (= ["(1,10) : ERROR 233"] (errs "        M=1\nEND\n")))))

;; sbl's -D# sets &DUMP to # before the first statement, and a program that sets &DUMP itself wins. MEASURED on sbl -bf: -D1 dumps a program of X = 1, -D2
;; dumps &DUMP = 2, -D1 of a program whose first statement is &DUMP = 0 dumps nothing; corpus csnobol4_suite collect and loop, silent entries graded by -D1.
(deftest the-d-switch-sets-dump-before-the-first-statement
  (is (re-find #"(?m)^X = 1$" (first (run-sno "        X = 1\nEND\n" ["-D1"]))))
  (is (re-find #"(?m)^&DUMP = 2$" (first (run-sno "        X = 1\nEND\n" ["-D2"]))))
  (is (not (re-find #"dump of" (first (run-sno "        &DUMP = 0\n        X = 1\nEND\n" ["-D1"])))))
  (is (not (re-find #"dump of" (first (run-sno "        X = 1\nEND\n"))))))

;; An operator touching the operand before it is 233 there, a + or - at that operand's start. MEASURED on sbl -bf: Y = X+1 at 13, Y = X*1 at 14, Y = A<X+1> at 15;
;; corpus slobol_tests test/arith and jmu_cs430 snobol4p/test5.
(deftest a-touching-plus-or-minus-is-233-at-its-operands-start
  (let [errs (fn [src] (vec (re-seq #"\(\d+(?:,\d+)?\) : ERROR \d+" (first (run-sno src)))))]
    (is (= ["(1,13) : ERROR 233"] (errs "        Y = X+1\nEND\n")))
    (is (= ["(1,14) : ERROR 233"] (errs "        Y = X*1\nEND\n")))
    (is (= ["(1,15) : ERROR 233"] (errs "        Y = A<X+1>\nEND\n")))))

;; A carriage return is a character of its line, never a line end: in a statement it is 230, after a goto 230, a line of NULs and a CR 214, a literal keeps it,
;; and END followed by one is no END. MEASURED on sbl -bf; corpus snolib g360 and spitbol_88 DEMOS/TREESORT, CR LF files sbl refuses.
(deftest a-carriage-return-is-a-character-of-its-line
  (let [errs (fn [src] (vec (re-seq #"\(\d+(?:,\d+)?\) : ERROR \d+" (first (run-sno src)))))]
    (is (= ["(1,14) : ERROR 230" "(2,19) : ERROR 230"] (errs "        X = 1\r\n        OUTPUT = X\r\nEND\r\n")))
    (is (= 1 (second (run-sno "        X = 1\r\n        OUTPUT = X\r\nEND\r\n"))))
    (is (= ["(1,20) : ERROR 230" "(2,21) : ERROR 230"] (errs "        X = 1  :(L)\r\nL       OUTPUT = 'l'\r\nEND\n")))
    (is (= ["(3) : ERROR 214"] (errs "        OUTPUT = 'a'\nEND\r\n\u0000\u0000\r\n")))
    (is (= "3\n" (first (run-sno "        X = 'a\rb'\n        OUTPUT = SIZE(X)\nEND\n"))))))

;; A subscript whose container is itself an expression, ($V)[S], is a capture's target as V[S] is. MEASURED on sbl -bf: 'xyz' REM . ($VARS_)[S] of VARS_ = .V sets
;; V['VAR']; corpus snolib ROUTING.tst.
(deftest an-element-of-an-expression-container-takes-a-capture
  (is (= "xyz\n" (first (run-sno "        V = TABLE()\n        VARS_ = .V\n        S = 'VAR'\n        'xyz' REM . ($VARS_)[S]  :F(N)\n        OUTPUT = V['VAR']  :(END)\nN       OUTPUT = 'FAIL'\nEND\n")))))

;; A pattern that defers to itself before it reads a character recurses without end: "246 stack overflow". MEASURED on sbl -bf: P = *P 'Z' | 'Y' then 'Y' P . OUTPUT
;; ends at ERROR 246 on that line; corpus snobol_playground ch2/recursive and recursive1.
(deftest a-left-recursive-pattern-overflows-the-stack
  (let [[out] (run-sno "      P = *P 'Z' | 'Y'\n      'Y' P . OUTPUT\n      OUTPUT = 'after'\nEND\n")]
    (is (re-find #"\(2\) : ERROR 246 -- stack overflow" out))
    (is (not (re-find #"(?m)^after$" out)))))

;; EXIT(s) ends the run and has the system perform s, the run's exit code the command's, no dump and no &CODE after it; EXIT(-3) ends it as a save file is written;
;; any other action is ERROR 105 (manual ch. 19, EXIT). MEASURED on sbl -bf: EXIT('exit 3') exits 3 with &DUMP = 1 and &CODE = 7 set, EXIT(5) is 105, EXIT(-3)
;; exits 0; corpus tests/snobol4 test_probe_loose_conformance_f65_exit and f65b_exit_savefile.
(deftest exit-ends-the-run-with-the-commands-status
  (is (= ["b\n" 3] (run-sno "        &CODE = 7\n        &DUMP = 1\n        OUTPUT = 'b'\n        EXIT('exit 3')\n        OUTPUT = 'after'\nEND\n")))
  (is (= ["b\n" nil] (run-sno "        OUTPUT = 'b'\n        EXIT(-3)\n        OUTPUT = 'after'\nEND\n")))
  (is (re-find #"\(2\) : ERROR 105 -- exit action not available in this implementation" (first (run-sno "        OUTPUT = 'b'\n        EXIT(5)\nEND\n")))))

;; An operator that a blank follows where an operand should be is 221, and 233 when it stands right after another unary operator. MEASURED on sbl -bf: Y = - X 221
;; at 13, Y = 'a' && b 233 at 18, Y = .. X 233 at 14, Y = ..X 212; corpus gate_cases_snobol4 test_gate_s4e_donewhen_runs_the_whole_criterion__w.
(deftest a-second-unary-operator-before-a-blank-is-233
  (let [errs (fn [src] (vec (re-seq #"\(\d+(?:,\d+)?\) : ERROR \d+" (first (run-sno src)))))]
    (is (= ["(1,13) : ERROR 221"] (errs "        Y = - X\nEND\n")))
    (is (= ["(1,18) : ERROR 233"] (errs "        Y = 'a' && b\nEND\n")))
    (is (= ["(1,14) : ERROR 233"] (errs "        Y = .. X\nEND\n")))
    (is (= ["(1) : ERROR 212"] (errs "        Y = ..X\nEND\n")))))

;; After a bad label the scan reads on to a semicolon when the label begins with ( [ < > : or , and a parenthesis in the label field is no bracket; any other bad
;; first character ends the line's scan. MEASURED on sbl -bf: "[  a; }" and "(  a; }" are 214 and 230 at 7, "#  a; }" and "!  a; }" 214 alone; corpus gate_cases_snobol4
;; test_gate_s4e_donewhen_runs_the_whole_criterion__w.
(deftest a-bad-label-ends-the-scan-of-its-line-unless-it-is-a-bracket
  (let [errs (fn [src] (vec (re-seq #"\(\d+(?:,\d+)?\) : ERROR \d+" (first (run-sno src)))))]
    (is (= ["(1) : ERROR 214" "(1,7) : ERROR 230"] (errs "[  a; }\nEND\n")))
    (is (= ["(1) : ERROR 214" "(1,7) : ERROR 230"] (errs "(  a; }\nEND\n")))
    (is (= ["(1) : ERROR 214"] (errs "#  a; }\nEND\n")))
    (is (= ["(1) : ERROR 214"] (errs "!  a; }\nEND\n")))))

;; The statement limit is checked before the STCOUNT trace fires; with an STCOUNT trace armed SPITBOL stops at the statement whose count reaches the limit, and
;; otherwise at the one after; the overrun raises &STLIMIT by 10 and leaves &STCOUNT at the limit (sbl.min stgcc, stgo4, stcov). MEASURED on sbl -bf: &STLIMIT = 5
;; and a loop print &STCOUNT 2 to 5; traced, the trace writes 3 and 4 and the fifth statement ends the run; a trace function under &STLIMIT = 20 is stopped at its
;; own first statement on its sixth call, &STCOUNT 20 and &STLIMIT 30; corpus spitbol_88 DEMOS/GOTOS.
(deftest the-statement-limit-is-checked-before-the-stcount-trace
  (let [[out] (run-sno "        &STLIMIT = 5\nL       OUTPUT = &STCOUNT  :(L)\nEND\n")]
    (is (clojure.string/starts-with? out "2\n3\n4\n5\n")))
  (let [[out] (run-sno "        &TRACE = 100\n        TRACE('STCOUNT','KEYWORD')\n        &STLIMIT = 5\nL       X = 1  :(L)\nEND\n")]
    (is (re-find #"&STCOUNT = 4\n" out))
    (is (not (re-find #"&STCOUNT = 5\n" out)))
    (is (re-find #"\(4\) : ERROR 244" out)))
  (let [[out] (run-sno (str "        DEFINE('TF()')                     :(TFE)\nTF      N = N + 1\n        M = M + 1                          :(RETURN)\n"
                            "TFE     TRACE('STCOUNT', 'KEYWORD', , 'TF')\n        &TRACE = 1000\n        &DUMP = 1\n        &STLIMIT = 20\n"
                            "L       I = I + 1                          :(L)\nEND\n"))]
    (is (re-find #"\(2\) : ERROR 244" out))
    (is (re-find #"(?m)^N = 5$" out))
    (is (re-find #"(?m)^&STCOUNT = 20$" out))
    (is (re-find #"(?m)^&STLIMIT = 30$" out))))

;; After a trace function SPITBOL gives &STNO back and not &LINE, which reads 2, and &TRACE is given back only when the function returns: an error that ends the
;; run inside it leaves &TRACE 0. MEASURED on sbl -bf: a three-statement trace function on lines 9 to 11 leaves &LASTLINE 11, &LASTNO 9, &LINE 2 and &STNO 13 in
;; the statement traced; corpus spitbol_88 DEMOS/GOTOS, whose dump reads &LASTLINE = 2 and &TRACE = 0.
(deftest a-trace-function-leaves-line-two-and-keeps-trace-zero-at-an-error
  (is (= "ll 11 9 2 13\nl2 11 9 2 15\n"
         (first (run-sno (str "        X = 1\n        Y = 1\n        Z = 1\n        W = 1\n        V = 1\n        DEFINE('TF()')                     :(TFE)\n"
                              "*       a comment\n*       a comment\nTF      M = M + 1\n        M = M + 1\n        M = M + 1                          :(RETURN)\n"
                              "TFE     TRACE('STCOUNT', 'KEYWORD', , 'TF')\n        &TRACE = 1000\n        A = 1\n"
                              "        OUTPUT = 'll ' &LASTLINE ' ' &LASTNO ' ' &LINE ' ' &STNO\n        B = 1\n"
                              "        OUTPUT = 'l2 ' &LASTLINE ' ' &LASTNO ' ' &LINE ' ' &STNO\nEND\n")))))
  (let [[out] (run-sno (str "        DEFINE('TF()')                     :(TFE)\nTF      N = N + 1\n        M = M + 1                          :(RETURN)\n"
                            "TFE     TRACE('STCOUNT', 'KEYWORD', , 'TF')\n        &TRACE = 1000\n        &DUMP = 1\n        &STLIMIT = 20\n"
                            "L       I = I + 1                          :(L)\nEND\n"))]
    (is (re-find #"(?m)^&TRACE = 0$" out))))

;; The empty line standing for a later error's listed line is no line of the page. MEASURED on sbl -bf: fourteen lines of "X = ) ; Y = (", two errors each, put
;; the page 2 header on the listing's 78th line; corpus gate_cases_snobol4 test_gate_s4e_donewhen_runs_the_whole_criterion__w.
(deftest a-later-errors-empty-line-is-no-line-of-the-page
  (let [[out] (run-sno (str (apply str (repeat 14 "        X = ) ; Y = (\n")) "END\n"))
        ls (clojure.string/split-lines out)]
    (is (= [8 77] (keep-indexed (fn [i l] (when (re-find #"page [0-9]+$" l) i)) ls)))))

;; An error's report names the line of the statement in error, whatever &LINE reads after a trace function. MEASURED on sbl -bf: with an STCOUNT trace armed, a
;; :(SCONTINUE) on line 6 with no preceding error ends at "(6) : ERROR 321" and "in line 6"; corpus csnobol4_suite setexit4 (its 331, line 16).
(deftest an-errors-report-names-its-statements-line-after-a-trace
  (let [[out] (run-sno (str "        DEFINE('S()')  :(SE)\nS       X = 1  :(RETURN)\nSE      TRACE('STCOUNT','KEYWORD',,.S)\n        &TRACE = 1000\n"
                            "        OUTPUT = 'a'\n        :(SCONTINUE)\nEND\n"))]
    (is (re-find #"\(6\) : ERROR 321" out))
    (is (re-find #"in line +6\n" out))))
