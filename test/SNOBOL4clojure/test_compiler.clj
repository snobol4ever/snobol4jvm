(ns SNOBOL4clojure.test-compiler
  "The compiler from Clojure: IR, compile error numbers, -INCLUDE, and CODE and RUN called directly."
  (:require [SNOBOL4clojure.repl :refer [preprocess-includes]] [clojure.test :refer :all]
            [SNOBOL4clojure.core :refer [CODE! syntax-error CODE]]
            [SNOBOL4clojure.core :refer [GLOBALS $$ snobol-set! DATATYPE table? table-get array? array-prototype]]
            [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [SNOBOL4clojure.test-helpers :refer [prog prog-include]]
            [SNOBOL4clojure.core :refer [INVOKE EVAL!]]
            [SNOBOL4clojure.core :refer [RUN]]))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_compiler
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── CODE! basic parse ─────────────────────────────────────────────────────────
(deftest test-code-simple-assignment
  (let [[codes _ _] (CODE! "X = 1")]
    (is (map? codes))
    (is (pos? (count codes)))))

(deftest test-code-with-label
  (let [[_ nos labels] (CODE! "START X = 1")]
    (is (contains? nos :START))
    (is (contains? labels (nos :START)))))

(deftest test-code-multiple-statements
  (let [[codes _ _] (CODE! "A = 1\nB = 2\nC = 3")]
    (is (= 3 (count codes)))))

(deftest test-code-comment-skipped
  ;; Comments should not become statements
  (let [[codes _ _] (CODE! "* This is a comment\nX = 1")]
    (is (= 1 (count codes)))))

(deftest test-code-goto
  (let [[codes nos _] (CODE! "LOOP X = 1 :S(LOOP)")]
    (is (contains? nos :LOOP))
    ;; The LOOP label maps directly as a key in codes
    (is (contains? codes :LOOP))))

(deftest test-code-empty-program
  ;; A blank/whitespace input: the block regex may produce a single empty-line
  ;; entry. Acceptable — either empty or all-nil entries.
  (let [[codes _ _] (CODE! "")]
    (is (or (empty? codes)
            (every? (fn [[_ v]] (every? nil? v)) codes)))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_syntax
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

(def ^:private sbl-errors
  [[" X = (" 226]
   [" X = (1 + 2" 226]
   [" X = 1 + 2)" 224]
   [" X = A<1" 229]
   [" X = A<1>>" 225]
   [" X = A[1" 229]
   [" X = A[1]]" 225]
   [" X = 'abc" 232]
   [" X = \"abc" 232]
   [" X = 1 +" 221]
   [" X = +" 221]
   [" X = 1 2 +" 221]
   [" X = F(1," 226]
   [" X = F(1,2" 226]
   [" X = F(1,2))" 224]
   [" X =  :(L" 227]
   [" X = 1 :S(L" 227]
   [" X = 1 :S(L)S(M)" 218]
   [" X = 1 :()" 212]
   [" X = 1 :S()" 212]
   [" X = 1 :X" 234]
   [" X = 1 :<L" 228]
   [" X = 1 :S<L" 228]
   [" X = #" 221]
   [" X = 1 ~" 221]
   [" X = 1.2.3" 231]
   [" X = 1e" 231]
   [" X = 12abc" 231]
   [" X = ." 221]
   [" X = $" 221]
   [" X = *" 221]
   [" 1 = 2" 212]
   [" 'a' = 2" 212]
   [" X = A<>" nil]
   [" X = A< , >" nil]
   [" X = (1,2)" nil]
   [" X = (,)" nil]
   [" X = ()" nil]
   [" X = F(" 226]
   [" X = )" 224]
   [" X = ]" 225]
   [" X = >" 225]
   [" X = 1 ==2" nil]
   [" X = 1 = 2" 212]
   [" X Y = 2" nil]
   [" X = Y Z (" 226]
   [" X = Y ?" 221]
   [" X = ?" 221]
   [" X = F(1 2" 226]
   [" X = A<1," 229]
   [" X = 1 : S(L)" nil]
   [" X = 1 :F(L)F(M)" 218]
   [" X = 1 :(L)(M)" 218]
   [" X = 1 :S(L) junk" 234]
   [" X = @" 221]
   [" X = 1 @" 221]
   [" X = &" 221]
   [" X = &1" 212]
   [" X = -" 221]
   [" X = 1 -" 221]
   [" X = 1 **" 221]
   [" X = !" 221]
   [" X = 1 & 2" nil]
   [" X = 1 % 2" nil]
   [" X = 1 |" 221]
   [" X = | 2" 221]
   [" X = 1 ." 221]
   [" X = 1 $" 221]
   [" X = A [1]" nil]
   [" X = 3 .5" 212]
   [" X = .5." 212]
   [" X = 1. 2." nil]
   [" X = 0x1" 231]
   [" X = 1E5" nil]
   [" X = 1.5E" 231]
   [" X = ''abc" 220]
   [" X = 'abc'def'" 220]
   [" X = (1 +" 221]
   [" X = (1 + 'abc" 232]
   [" X = (1>" 225]
   [" X = A<1)" 224]
   [" X = <1>" 222]
   [" X = 1,2" 223]
   [" X = F(1,)" nil]
   [" X = F(,)" nil]
   [" X = {" 230]
   [" X = \\" 230]
   [" X = `" 230]
   [" X = 1 {" 230]
   [" X = A<1>B" 220]
   [" X = 'a'B" 220]
   [" X = F(1)G" 220]
   [" X = (1)(2)" 220]
   [" X = (1) 'a'" nil]
   [" (X) = 1" nil]
   [" $'X' = 1" nil]
   [" F(X) = 1" nil]
   [" *X = 1" nil]
   [" X = Y = 1" nil]
   [" X = 'a' = 1" 212]
   [" X = .X" nil]
   [" X = ..X" 212]
   [" X = &X" nil]
   [" X = &ANCHOR" nil]
   [" X = $1" nil]
   [" X = *1" nil]
   [" X = - 1" 221]
   [" X = -1" nil]
   [" X = 1 -1" nil]
   [" X = 1- 1" 231]
   [" X = 1 :S(A) F(B)" nil]
   [" X = 1 :F(B) S(A)" nil]
   [" X = 1 :S(A)F(B)" nil]
   [" X = 1 :(A" 227]
   [" X = 1 :S(A)(B)" 218]
   [" X = 1 :S(A) :F(B)" 234]
   [" X = 1 :S(A)X" 234]
   [" X = 1 :SA" 234]
   [" X = 1 :S" 234]
   [" X = 1 :" 219]
   [" X = 1 :S((A)" 227]
   [" X = 1 :S(A))" 234]
   [" X = 1 :S(A B)" 212]
   [" X = 1 :S(A + )" 221]
   [" X = 1 :S<A>F(B)" nil]
   [" X = 1:S(A)" nil]
   [" X = 'a' 'b" 232]
   [" X = 1." nil]
   [" X = 1.e5" nil]
   [" X = 1.5e+" 231]
   [" X = 1d5" nil]
   [" X = 1.5." 231]
   [" X = 1A" 231]
   [" X = A.B" nil]
   [" X = A_B" nil]
   [" X = _A" 230]
   [" X = 1 ?" 221]
   [" X = A ? B =" nil]
   [" X = A ? B = C" nil]
   [" A ? B = C = D" nil]
   [" X = F(1 2)" nil]
   [" X = F (1)" nil]
   [" X = F( 1 , 2 )" nil]
   [" X = (1 , 2)" nil]
   [" X = A<1, 2>" nil]
   [" X = A<1 2>" nil]
   [" X = 1 ** 2" nil]
   [" X = 1 *2" nil]
   [" X = 1* 2" 233]
   [" X = 1*2" 233]
   [" X = A+B" 233]
   [" X = A +B" nil]
   ;; a colon ends the scan inside brackets too, and the bracket lacks its closer (generated 2026-10-29)
   [" X = (4 :(L))" 226]
   [" X = (4 :" 226]
   [" X = F(4 :(L))" 226]
   [" X = A<4 :(L)>" 229]
   [" X = A[4 :(L)]" 229]
   [" X = (4, 5 :(L))" 226]
   [" X = (4 + :(L))" 221]
   [" X = 1 :(L:X)" 227]
   [" X = 1 :S(L :F(L))" 227]
   [" X = 1 :<L :>" 228]
   ;; a label begins with a letter or a digit (generated 2026-10-29)
   ["#L X = 1" 214]
   ["(L X = 1" 214]
   ["_L X = 1" 214]
   ["'L' X = 1" 214]
   ["$L X = 1" 214]
   ["\u000b X = 1" 214]
   ["\f X = 1" 214]])

(deftest the-scan-names-the-error-sbl-names
  (doseq [[stmt n] sbl-errors]
    (is (= n (:error (syntax-error stmt))) stmt)))

;; An operator with no operand after it is 221 at the operator itself when a blank follows it, and at the next column before a closer or the statement's end.
;; MEASURED on sbl -bf: "L       = 'X'" (1,9), "L  =  'X'" (1,4), "OUTPUT = 'X'" (1,8): a label, then a body beginning with =; and
;; "        X = (1 +)" (1,17) (the compiled program also reports "        X = *" at (1,13), as sbl -bf does). snobol4jvm said 10, 5 and 9 for the first three (corpus tests/snobol4 compile_diag__p01 and kin, whose listing caret moved with it).
(deftest a-missing-operand-before-a-blank-is-221-at-the-operator
  (doseq [[src col] [["L       = 'X'" 9] ["L  =  'X'" 4] ["OUTPUT = 'X'" 8] ["        X = (1 +)" 17]]]
    (is (= {:error 221 :column col} (select-keys (syntax-error src) [:error :column])) src)))

;; An invalid numeric item is 231 at the item's first character. MEASURED on sbl -bf: X = 12ABC, 1.2.3, 1E, 1.5E+, 1- 1 and 3.14X, each after eight blanks, are
;; 231 at column 13; snobol4jvm said 15, 16, 14, 16, 14 and 17, the character that spoiled the item (corpus tests/snobol4 compile_diag__p13, p13b).
(deftest an-invalid-numeric-item-is-231-at-its-first-character
  (doseq [n ["12ABC" "1.2.3" "1E" "1.5E+" "1- 1" "3.14X"]]
    (is (= {:error 231 :column 13} (select-keys (syntax-error (str "        X = " n)) [:error :column])) n)))

;; A goto whose right paren is missing at the statement's end is 227 at the first character of the name it ends with. MEASURED on sbl -bf: "        :(END" and
;; "        :(LONGLABEL" are 227 at column 11; "        X = 1 :S(A" at 18 and "        X = 1 :F($V" at 19; a colon inside stops it there, "        X = 1 :(L:X)" at
;; 18. snobol4jvm said 13 and 19 for the first two (corpus tests/snobol4 compile_diag__p29).
(deftest a-goto-unclosed-at-the-end-is-227-at-its-last-name
  (doseq [[src col] [["        :(END" 11] ["        :(LONGLABEL" 11] ["        X = 1 :S(A" 18] ["        X = 1 :F($V" 19] ["        X = 1 :(L:X)" 18]]]
    (is (= {:error 227 :column col} (select-keys (syntax-error src) [:error :column])) src)))

;; A character no element may hold right after the subject is 230, not the missing operator 220. MEASURED on sbl -bf: "A B\C = 1" is 230 at column 4 (the
;; backslash); "        X = Y\Z" is 230 at 14. snobol4jvm said 220 for the first (corpus tests/snobol4 compile_diag__p33).
(deftest an-illegal-character-after-the-subject-is-230
  (is (= {:error 230 :column 4} (select-keys (syntax-error "A B\\C = 1") [:error :column])))
  (is (= {:error 230 :column 14} (select-keys (syntax-error "        X = Y\\Z") [:error :column]))))

;; A duplicated goto element is 218 at its closer, once it is scanned (manual Appendix D 218). MEASURED on sbl -bf: "X = 1 :S(L)S(L)" at column 23,
;; ":S(LL)S(LLL)" at 26, ":(L)S(L)" at 22, ":S<L>S<L>" at 23, ":S(L)S()" at 22; snobol4jvm said the opener, 21 and so on (corpus tests/snobol4 compile_diag__p11c).
(deftest a-duplicated-goto-is-218-at-its-closer
  (doseq [[src col] [["        X = 1 :S(L)S(L)" 23] ["        X = 1 :S(LL)S(LLL)" 26] ["        X = 1 :(L)S(L)" 22] ["        X = 1 :S<L>S<L>" 23]
                     ["        X = 1 :S(L)S()" 22] ["        X = 1 :S(L)F(L)S(L)" 27]]]
    (is (clojure.core/= {:error 218 :column col} (syntax-error src)) src)))

;; An operator touching what is before it where the goto field wants an element is 233, anything else there 234 (manual Appendix D 233, 234). MEASURED on
;; sbl -bf: ":$" and ":S$(L)" are 233 at 16 and 17, ":S(L)$" at 20; ":  $" and ":S(L) +" are 234 at 18 and 21, ":)" 234 at 16 (corpus compile_diag__p45).
(deftest an-operator-in-the-goto-field-is-233
  (doseq [[src want] [["        X = 1 :$" [233 16]] ["        X = 1 :S$(L)" [233 17]] ["        X = 1 :S(L)$" [233 20]] ["        X = 1 :  $" [234 18]]
                      ["        X = 1 :S(L) +" [234 21]] ["        X = 1 :)" [234 16]]]]
    (is (clojure.core/= {:error (first want) :column (second want)} (syntax-error src)) src)))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; t_include
;;
;; Sprint 25A — -INCLUDE preprocessor tests.
;;
;; Tests:
;; include_preprocess_basic     — preprocess-includes expands a -INCLUDE line
;; include_preprocess_not_found — missing file becomes a comment, does not crash
;; include_preprocess_cycle     — cyclic includes are detected and skipped
;; include_smoke_inline         — CODE! with include-path compiles included code
;; include_two_file             — two-file include via temp directory
;; include_bcd_ebcd_compiles    — BCD_EBCD.SNO (no includes) compiles cleanly
;;
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── preprocess-includes unit tests ───────────────────────────────────────────

(deftest include_preprocess_basic
  "preprocess-includes expands -INCLUDE 'file' to file contents"
  (let [dir  (doto (io/file (System/getProperty "java.io.tmpdir")
                            (str "snobol4test_" (System/currentTimeMillis)))
               .mkdirs)
        inc  (io/file dir "greet.inc")]
    (try
      (spit inc "        OUTPUT = 'hello from include'\n")
      (let [src      (str "-INCLUDE 'greet.inc'\nEND\n")
            expanded (preprocess-includes src [(.getAbsolutePath dir)])]
        (is (.contains expanded "hello from include")
            "Expanded source should contain included file content")
        (is (not (.contains expanded "-INCLUDE"))
            "Expanded source should not contain raw -INCLUDE directive"))
      (finally (.delete inc) (.delete dir)))))

;; A missing include file leaves its card standing, which CODE! reads as ERROR 285 (MEASURED on sbl -bf: -INCLUDE 'nosuch.inc' is "(1,10) : ERROR 285 --
;; include file cannot be opened" and nothing runs; the engine had made it a comment and run on).
(deftest include_preprocess_not_found
  "preprocess-includes leaves a missing -INCLUDE card standing, no exception"
  (let [expanded (preprocess-includes "-INCLUDE 'does_not_exist.inc'\nEND\n" ["/nonexistent"])]
    (is (.contains expanded "-INCLUDE 'does_not_exist.inc'")
        "The card stands for CODE! to read as 285")))

;; snobol4jvm never folds case (Lon 2026-11-17 18:2x CST, in the JVM window, verbatim: "We WILL NEVER support case-folding. We will always have upper-case
;; built-in keywords and functions."): a control card is upper case, so -include is no card and includes nothing, as sbl -bf reads it (MEASURED).
(deftest include_preprocess_lower_case_is_no_card
  "-include (lower case) is no card: the file is not spliced in"
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir")
                           (str "snobol4test_" (System/currentTimeMillis)))
              .mkdirs)
        inc (io/file dir "low.inc")]
    (try
      (spit inc "        OUTPUT = 'lower case include'\n")
      (let [expanded (preprocess-includes
                       (str "-include 'low.inc'\nEND\n")
                       [(.getAbsolutePath dir)])]
        (is (not (.contains expanded "lower case include")))
        (is (.contains expanded "-include 'low.inc'")))
      (finally (.delete inc) (.delete dir)))))

;; ── Two-file include smoke test ───────────────────────────────────────────────

;; ── BCD_EBCD.SNO compiles without includes ────────────────────────────────────


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_sprint11
;;
;; Sprint 11 — TABLE: atom-backed mutable hash, subscript read/write, ITEM, PROTOTYPE, DATATYPE.
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

(defn run-prog [src]
  (let [start (CODE src)] (RUN start)))

;; ── TABLE construction ────────────────────────────────────────────────────────

(deftest table-001-construction
  (testing "TABLE() creates an atom-backed table"
    (let [t (INVOKE 'TABLE)]
      (is (table? t))
      (is (= "TABLE" (DATATYPE t))))))

;; ── Subscript write / read — angle-bracket ───────────────────────────────────

;; ── Subscript write / read — square-bracket ──────────────────────────────────

;; ── Missing key returns ε ─────────────────────────────────────────────────────

;; ── Variable key ─────────────────────────────────────────────────────────────

;; ── Reference semantics ───────────────────────────────────────────────────────

;; ── Multiple keys ────────────────────────────────────────────────────────────

;; ── Overwrite ────────────────────────────────────────────────────────────────

;; ── ITEM function ─────────────────────────────────────────────────────────────

;; ── PROTOTYPE of a table is its hash headers ─────────────────────────────────
;; sbl -bf: PROTOTYPE(TABLE()) is 11, an INTEGER, the number of hash headers TABLE() makes (the manual, ch. 19, documents PROTOTYPE for arrays; sbl answers for a table).

;; ── Integer keys ─────────────────────────────────────────────────────────────


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_sprint11_array
;;
;; Sprint 11 — ARRAY: multi-dimensional, bounds-checked, integer subscripts,
;; default value, PROTOTYPE, DATATYPE.
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── Construction ─────────────────────────────────────────────────────────────

(deftest array-001-simple-construction
  (testing "array(N) creates a SnobolArray with dims [1..N]"
    (let [a (INVOKE 'ARRAY 10)]
      (is (array? a))
      (is (= "ARRAY" (DATATYPE a)))
      ;; sbl -bf: PROTOTYPE(ARRAY(10)) is the integer 10 (DATATYPE INTEGER)
      (is (= 10 (array-prototype a))))))

(deftest array-002-proto-string
  (testing "array('lo:hi,...') parses dimension specs"
    (let [a (INVOKE 'ARRAY "-5:10,3:5,20")]
      (is (array? a))
      ;; sbl -bf and CSNOBOL4 print -5:10,3:5,20, the prototype as written
      (is (= "-5:10,3:5,20" (array-prototype a))))))

;; ── Read / Write ──────────────────────────────────────────────────────────────

;; ── Bounds checking ───────────────────────────────────────────────────────────

;; ── PROTOTYPE ────────────────────────────────────────────────────────────────

;; ── Overwrite ────────────────────────────────────────────────────────────────

;; ── Multi-dimensional ────────────────────────────────────────────────────────


(use-fixtures :each (fn [t] (SNOBOL4clojure.core/GLOBALS) (t)))
