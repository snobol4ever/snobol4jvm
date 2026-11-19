;; The compile error a statement that does not compile reports, as sbl -bf names it: each row is CODE(statement) under sbl -bf, &ERRTYPE when CODE failed and nil
;; when it compiled (generated 2026-10-23 from /home/resources/x64/bin/sbl -bf, macro spitbol 4.0f). The scan in syntax.clj names the error of a statement the
;; parser refuses; a statement the parser takes is not its business, so a nil row only asks that the scan find no error there. The scan is given one statement,
;; so no row holds the semicolon that separates two.
(ns SNOBOL4clojure.test-syntax
  (:require [clojure.test :refer :all]
            [SNOBOL4clojure.compiler :refer [syntax-error]]
            [SNOBOL4clojure.env]
            [SNOBOL4clojure.compiler]))

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

;; Where a statement ends inside an open list, 226 or 229 stands at the start of the last token, and a column past the statement's first line falls on the
;; continuation line it is in (manual Appendix D; MEASURED on sbl -bf: "X = F('a'" 226 at (1,15); "X = F(" at (1,13); "X = A['ab'" 229 at (1,15); "X = F(G(1)"
;; at (1,18); "        X = F(" then "+ 'a'" and "+ 'b'" at (3,3); "        X = 'a' ;  Y = (" then "+ 'b'" at (2,3); corpus setl4_spitbol sonnets-1, a 226
;; at (229,3) that snobol4jvm put at (225,303)).
(deftest an-open-list-at-the-end-is-named-at-its-last-token
  (doseq [[src want] [["        X = F('a'\nEND\n" [1 226 15]] ["        X = F(\nEND\n" [1 226 13]] ["        X = A['ab'\nEND\n" [1 229 15]]
                      ["        X = F(G(1)\nEND\n" [1 226 18]] ["        X = F(\n+ 'a'\n+ 'b'\nEND\n" [3 226 3]]
                      ["        X = 'a' ;  Y = (\n+ 'b'\nEND\n" [2 226 3]]]]
    (SNOBOL4clojure.env/GLOBALS)
    (SNOBOL4clojure.compiler/CODE src)
    (is (clojure.core/= want (vec (take 3 (first @SNOBOL4clojure.env/<DIAGNOSTICS>)))) src)))

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
