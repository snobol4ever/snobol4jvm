(ns SNOBOL4clojure.test-snocone-parser
  "Tests for sc->snobol4 — Snocone expression → SNOBOL4 source text.

  Reference: bconv table in the language specification.
  Each test verifies that a Snocone expression compiles to the correct
  SNOBOL4 source string."
  (:require [clojure.test :refer :all]
            [SNOBOL4clojure.snocone-emitter :refer [sc->snobol4]]))

;; ===========================================================================
;; 1. Literals pass through unchanged
;; ===========================================================================

(deftest test-integer-literal
  (is (= "42" (sc->snobol4 "42"))))

(deftest test-real-literal
  (is (= "3.14" (sc->snobol4 "3.14"))))

(deftest test-string-literal
  (is (= "'hello'" (sc->snobol4 "'hello'"))))

(deftest test-identifier-literal
  (is (= "x" (sc->snobol4 "x"))))

;; ===========================================================================
;; 2. dotck — leading-dot float gets 0 prepended
;; ===========================================================================

(deftest test-dotck-leading-dot
  (is (= "0.5" (sc->snobol4 ".5"))))

(deftest test-dotck-normal-float-unchanged
  (is (= "3.14" (sc->snobol4 "3.14"))))

;; ===========================================================================
;; 3. Infix operators — pass through as-is
;; ===========================================================================

(deftest test-assign    (is (= "x = y"   (sc->snobol4 "x = y"))))
(deftest test-question  (is (= "x ? y"   (sc->snobol4 "x ? y"))))
(deftest test-pipe      (is (= "x | y"   (sc->snobol4 "x | y"))))
(deftest test-plus      (is (= "a + b"   (sc->snobol4 "a + b"))))
(deftest test-minus     (is (= "a - b"   (sc->snobol4 "a - b"))))
(deftest test-slash     (is (= "a / b"   (sc->snobol4 "a / b"))))
(deftest test-star      (is (= "a * b"   (sc->snobol4 "a * b"))))
(deftest test-period    (is (= "a . b"   (sc->snobol4 "a . b"))))
(deftest test-dollar    (is (= "a $ b"   (sc->snobol4 "a $ b"))))

;; ===========================================================================
;; 4. Function operators — emit as FN(a,b)
;; ===========================================================================

(deftest test-eq        (is (= "EQ(a,b)"     (sc->snobol4 "a == b"))))
(deftest test-ne        (is (= "NE(a,b)"     (sc->snobol4 "a != b"))))
(deftest test-lt        (is (= "LT(a,b)"     (sc->snobol4 "a < b"))))
(deftest test-gt        (is (= "GT(a,b)"     (sc->snobol4 "a > b"))))
(deftest test-le        (is (= "LE(a,b)"     (sc->snobol4 "a <= b"))))
(deftest test-ge        (is (= "GE(a,b)"     (sc->snobol4 "a >= b"))))
(deftest test-ident     (is (= "IDENT(a,b)"  (sc->snobol4 "a :: b"))))
(deftest test-differ    (is (= "DIFFER(a,b)" (sc->snobol4 "a :!: b"))))
(deftest test-lgt       (is (= "LGT(a,b)"   (sc->snobol4 "a :>: b"))))
(deftest test-llt       (is (= "LLT(a,b)"   (sc->snobol4 "a :<: b"))))
(deftest test-lge       (is (= "LGE(a,b)"   (sc->snobol4 "a :>=: b"))))
(deftest test-lle       (is (= "LLE(a,b)"   (sc->snobol4 "a :<=: b"))))
(deftest test-leq       (is (= "LEQ(a,b)"   (sc->snobol4 "a :==: b"))))
(deftest test-lne       (is (= "LNE(a,b)"   (sc->snobol4 "a :!=: b"))))
(deftest test-remdr     (is (= "REMDR(a,b)" (sc->snobol4 "a % b"))))

;; ===========================================================================
;; 5. Special operators
;; ===========================================================================

(deftest test-caret-to-starstar
  (is (= "a ** b" (sc->snobol4 "a ^ b"))))

(deftest test-or-to-alternation
  ;; || → pattern alternation (a,b)
  (is (= "(a,b)" (sc->snobol4 "a || b"))))

(deftest test-concat-to-blank
  ;; && → blank concatenation
  (is (= "a b" (sc->snobol4 "a && b"))))

(deftest test-juxtaposition-concat
  ;; adjacent terms → blank concat
  (is (= "a b" (sc->snobol4 "a b"))))

;; ===========================================================================
;; 6. Unary operators
;; ===========================================================================

(deftest test-unary-minus  (is (= "-x"  (sc->snobol4 "-x"))))
(deftest test-unary-plus   (is (= "+x"  (sc->snobol4 "+x"))))
(deftest test-unary-star   (is (= "*p"  (sc->snobol4 "*p"))))
(deftest test-unary-dot    (is (= ".v"  (sc->snobol4 ".v"))))
(deftest test-unary-dollar (is (= "$v"  (sc->snobol4 "$v"))))
(deftest test-unary-tilde  (is (= "~x"  (sc->snobol4 "~x"))))
(deftest test-unary-at     (is (= "@x"  (sc->snobol4 "@x"))))

;; ===========================================================================
;; 7. Function calls and array refs
;; ===========================================================================

(deftest test-call-no-args
  (is (= "f()" (sc->snobol4 "f()"))))

(deftest test-call-one-arg
  (is (= "f(x)" (sc->snobol4 "f(x)"))))

(deftest test-call-two-args
  (is (= "f(x,y)" (sc->snobol4 "f(x,y)"))))

(deftest test-call-expr-arg
  (is (= "f(EQ(a,b))" (sc->snobol4 "f(a == b)"))))

(deftest test-array-ref
  (is (= "arr[i]" (sc->snobol4 "arr[i]"))))

;; ===========================================================================
;; 8. Precedence and associativity preserved in output
;; ===========================================================================

(deftest test-precedence-mul-before-add
  (is (= "a + b * c" (sc->snobol4 "a + b * c"))))

(deftest test-precedence-compare-fn
  ;; a == b + c → EQ(a,b + c)
  (is (= "EQ(a,b + c)" (sc->snobol4 "a == b + c"))))

(deftest test-caret-right-assoc
  ;; a ^ b ^ c → a ** b ** c  (right-assoc, no parens needed)
  (is (= "a ** b ** c" (sc->snobol4 "a ^ b ^ c"))))
