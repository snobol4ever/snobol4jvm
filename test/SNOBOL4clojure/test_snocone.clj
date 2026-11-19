(ns SNOBOL4clojure.test-snocone
  "The Snocone front end: its lexer, its expression grammar and its emitter of SNOBOL4 source."
  (:require [clojure.test :refer :all]
            [SNOBOL4clojure.snocone :as sc]
            [SNOBOL4clojure.snocone :refer [sc->snobol4]]))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_snocone
;;
;; Tests for the Snocone lexer (Step 1) in snocone.clj.
;;
;; Covers:
;; 1. Helpers — strip-comment, continuation?, split-semicolon
;; 2. Literals — integers, reals (incl. leading-dot), strings
;; 3. Keywords — all 12 reserved words classified correctly
;; 4. Operators — every operator in the bconv table, longest-match
;; 5. Punctuation — () {} [] , ; :
;; 6. Statement boundaries — :sc/newline tokens, continuation joining
;; 7. Semicolon splitting
;; 8. Line numbers
;; 9. End-to-end snippets drawn from the language specification
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- kinds
  "Tokenize src and return the :kind sequence, excluding :sc/eof and :sc/newline."
  [src]
  (->> (sc/tokenize src)
       (remove #(#{:sc/eof :sc/newline} (:kind %)))
       (mapv :kind)))

(defn- texts
  "Tokenize src and return the :text sequence, excluding :sc/eof and :sc/newline."
  [src]
  (->> (sc/tokenize src)
       (remove #(#{:sc/eof :sc/newline} (:kind %)))
       (mapv :text)))

(defn- tokens [src] (sc/tokenize src))

(defn- newline-count [src]
  (->> (sc/tokenize src) (filter #(= :sc/newline (:kind %))) count))

;; ===========================================================================
;; 1. Helpers
;; ===========================================================================

(deftest test-strip-comment-no-comment
  (is (= "x = 1" (#'sc/strip-comment "x = 1"))))

(deftest test-strip-comment-trailing-hash
  (is (= "x = 1 " (#'sc/strip-comment "x = 1 # comment"))))

(deftest test-strip-comment-hash-inside-single-quote
  (is (= "x = 'a#b'" (#'sc/strip-comment "x = 'a#b'"))))

(deftest test-strip-comment-hash-inside-double-quote
  (is (= "x = \"a#b\"" (#'sc/strip-comment "x = \"a#b\""))))

(deftest test-strip-comment-leading-hash
  (is (= "" (#'sc/strip-comment "# full comment line"))))

(deftest test-continuation-ends-with-plus
  (is (#'sc/continuation? "x = y +")))

(deftest test-continuation-ends-with-colon
  (is (#'sc/continuation? "x :")))

(deftest test-continuation-ends-with-comma
  (is (#'sc/continuation? "f(a,")))

(deftest test-continuation-ends-with-identifier
  (is (not (#'sc/continuation? "x = y"))))

(deftest test-continuation-empty
  (is (not (#'sc/continuation? ""))))

(deftest test-continuation-whitespace-after-cont-char
  ;; Whitespace is trimmed before the check
  (is (#'sc/continuation? "x + ")))

(deftest test-split-semicolon-no-semicolon
  (let [r (#'sc/split-semicolon "x = 1")]
    (is (= 1 (count r)))
    (is (= "x = 1" (first r)))))

(deftest test-split-semicolon-two-segments
  (let [r (#'sc/split-semicolon "x = 1; y = 2")]
    (is (= 2 (count r)))
    (is (= "x = 1" (first r)))
    (is (= " y = 2" (second r)))))

(deftest test-split-semicolon-inside-string
  (let [r (#'sc/split-semicolon "x = 'a;b'")]
    (is (= 1 (count r)))
    (is (= "x = 'a;b'" (first r)))))

;; ===========================================================================
;; 2. Literals
;; ===========================================================================

(deftest test-literal-integer
  (let [t (first (tokens "42"))]
    (is (= :sc/integer (:kind t)))
    (is (= "42" (:text t)))))

(deftest test-literal-real-decimal
  (let [t (first (tokens "3.14"))]
    (is (= :sc/real (:kind t)))
    (is (= "3.14" (:text t)))))

(deftest test-literal-real-exponent-lower-e
  (let [t (first (tokens "1e10"))]
    (is (= :sc/real (:kind t)))))

(deftest test-literal-real-exponent-upper-e
  (let [t (first (tokens "2.5E-3"))]
    (is (= :sc/real (:kind t)))
    (is (= "2.5E-3" (:text t)))))

(deftest test-literal-real-exponent-d
  (let [t (first (tokens "1D2"))]
    (is (= :sc/real (:kind t)))))

(deftest test-literal-real-leading-dot
  ;; .5 is valid Snocone; parser rewrites to 0.5 (dotck)
  (let [t (first (tokens ".5"))]
    (is (= :sc/real (:kind t)))
    (is (= ".5" (:text t)))))

(deftest test-literal-string-single-quote
  (let [t (first (tokens "'hello'"))]
    (is (= :sc/string (:kind t)))
    (is (= "'hello'" (:text t)))))

(deftest test-literal-string-double-quote
  (let [t (first (tokens "\"world\""))]
    (is (= :sc/string (:kind t)))
    (is (= "\"world\"" (:text t)))))

(deftest test-literal-string-contains-semicolon
  (let [t (first (tokens "'a;b'"))]
    (is (= :sc/string (:kind t)))
    (is (= "'a;b'" (:text t)))))

(deftest test-literal-string-contains-hash
  (let [t (first (tokens "'a#b'"))]
    (is (= :sc/string (:kind t)))
    (is (= "'a#b'" (:text t)))))

;; ===========================================================================
;; 3. Keywords
;; ===========================================================================

(deftest test-keywords-all
  (are [src kw] (= kw (first (kinds src)))
    "if"        :sc/kw-if
    "else"      :sc/kw-else
    "while"     :sc/kw-while
    "do"        :sc/kw-do
    "for"       :sc/kw-for
    "return"    :sc/kw-return
    "freturn"   :sc/kw-freturn
    "nreturn"   :sc/kw-nreturn
    "go"        :sc/kw-go
    "to"        :sc/kw-to
    "procedure" :sc/kw-procedure
    "struct"    :sc/kw-struct))

(deftest test-keyword-not-reserved-longer-ident
  ;; "iffy" must be an identifier, not keyword "if" + "fy"
  (is (= :sc/identifier (first (kinds "iffy")))))

(deftest test-keyword-case-sensitive-upper-not-keyword
  ;; Snocone is case-sensitive; "IF" is an identifier
  (is (= :sc/identifier (first (kinds "IF")))))

;; ===========================================================================
;; 4. Operators — every bconv entry, longest-match
;; ===========================================================================

(deftest test-operators-all-single
  (are [src kw] (= kw (first (kinds src)))
    "="    :sc/op-assign
    "?"    :sc/op-question
    "|"    :sc/op-pipe
    "||"   :sc/op-or
    "&&"   :sc/op-concat
    "=="   :sc/op-eq
    "!="   :sc/op-ne
    "<"    :sc/op-lt
    ">"    :sc/op-gt
    "<="   :sc/op-le
    ">="   :sc/op-ge
    "::"   :sc/op-str-ident
    ":!:"  :sc/op-str-differ
    ":<:"  :sc/op-str-lt
    ":>:"  :sc/op-str-gt
    ":<=:" :sc/op-str-le
    ":>=:" :sc/op-str-ge
    ":==:" :sc/op-str-eq
    ":!=:" :sc/op-str-ne
    "+"    :sc/op-plus
    "-"    :sc/op-minus
    "/"    :sc/op-slash
    "*"    :sc/op-star
    "%"    :sc/op-percent
    "^"    :sc/op-caret
    "."    :sc/op-period
    "$"    :sc/op-dollar
    "~"    :sc/op-tilde
    "@"    :sc/op-at
    "&"    :sc/op-ampersand))

(deftest test-op-longest-match-str-ne
  ;; :!=: must be one token, not : then != then :
  (let [toks (remove #(= :sc/newline (:kind %)) (tokens ":!=:"))]
    (is (= 1 (count (remove #(= :sc/eof (:kind %)) toks))))
    (is (= :sc/op-str-ne (:kind (first toks))))))

(deftest test-op-longest-match-or-not-two-pipes
  (let [toks (remove #(#{:sc/newline :sc/eof} (:kind %)) (tokens "||"))]
    (is (= 1 (count toks)))
    (is (= :sc/op-or (:kind (first toks))))))

(deftest test-op-longest-match-concat-not-two-amps
  (let [toks (remove #(#{:sc/newline :sc/eof} (:kind %)) (tokens "&&"))]
    (is (= 1 (count toks)))
    (is (= :sc/op-concat (:kind (first toks))))))

(deftest test-op-double-star-is-caret
  (is (= :sc/op-caret (first (kinds "**")))))

;; ===========================================================================
;; 5. Punctuation
;; ===========================================================================

(deftest test-punctuation-all
  (are [src kw] (= kw (first (kinds src)))
    "("  :sc/lparen
    ")"  :sc/rparen
    "{"  :sc/lbrace
    "}"  :sc/rbrace
    "["  :sc/lbracket
    "]"  :sc/rbracket
    ","  :sc/comma
    ":"  :sc/colon))

;; ===========================================================================
;; 6. Statement boundaries
;; ===========================================================================

(deftest test-boundary-newline-after-statement
  (is (pos? (newline-count "x = 1"))))

(deftest test-boundary-two-statements-two-newlines
  (is (= 2 (newline-count "x = 1\ny = 2"))))

(deftest test-boundary-continuation-joined-one-statement
  ;; "x = 1 +" continues; "2" completes it → one Newline
  (is (= 1 (newline-count "x = 1 +\n2")))
  (is (= ["x" "=" "1" "+" "2"] (texts "x = 1 +\n2"))))

(deftest test-boundary-continuation-on-comma
  (is (= 1 (newline-count "f(a,\nb)"))))

(deftest test-boundary-comment-stripped-before-continuation-check
  ;; "x + # note" → after stripping → "x + ", last real char is '+'
  (is (= 1 (newline-count "x +  # note\n2"))))

(deftest test-boundary-blank-line-skipped
  (is (= 2 (newline-count "x = 1\n\ny = 2"))))

(deftest test-boundary-comment-only-line-skipped
  (is (= 2 (newline-count "x = 1\n# comment\ny = 2"))))

;; ===========================================================================
;; 7. Semicolons
;; ===========================================================================

(deftest test-semicolon-splits-two-statements
  (is (= 2 (newline-count "x = 1; y = 2"))))

(deftest test-semicolon-inside-string-not-split
  (is (= 1 (newline-count "x = 'a;b'"))))

;; ===========================================================================
;; 8. Line numbers
;; ===========================================================================

(deftest test-line-numbers-first-token-line-1
  (let [t (first (remove #(#{:sc/newline :sc/eof} (:kind %)) (tokens "x = 1")))]
    (is (= 1 (:line t)))))

(deftest test-line-numbers-second-statement-line-2
  (let [toks (remove #(#{:sc/newline :sc/eof} (:kind %)) (tokens "x = 1\ny = 2"))
        y    (first (filter #(= "y" (:text %)) toks))]
    (is (= 2 (:line y)))))

(deftest test-line-numbers-continued-statement-uses-first-line
  (let [toks (remove #(#{:sc/newline :sc/eof} (:kind %)) (tokens "x = 1 +\n2"))]
    (is (every? #(= 1 (:line %)) toks))))

;; ===========================================================================
;; 9. End-to-end snippets
;; ===========================================================================

(deftest test-e2e-if-statement
  (is (= [:sc/kw-if :sc/lparen :sc/identifier :sc/op-eq :sc/integer :sc/rparen
          :sc/lbrace :sc/identifier :sc/op-assign :sc/integer :sc/rbrace]
         (kinds "if (x == 0) { y = 1 }"))))

(deftest test-e2e-procedure-header
  (let [k (kinds "procedure foo(a, b) {")]
    (is (= :sc/kw-procedure (nth k 0)))
    (is (= :sc/identifier   (nth k 1)))
    (is (= :sc/lparen       (nth k 2)))
    (is (= :sc/identifier   (nth k 3)))
    (is (= :sc/comma        (nth k 4)))
    (is (= :sc/identifier   (nth k 5)))
    (is (= :sc/rparen       (nth k 6)))
    (is (= :sc/lbrace       (nth k 7)))))

(deftest test-e2e-while-loop
  (let [k (kinds "while (i > 0) {")]
    (is (= :sc/kw-while    (nth k 0)))
    (is (= :sc/lparen      (nth k 1)))
    (is (= :sc/identifier  (nth k 2)))
    (is (= :sc/op-gt       (nth k 3)))
    (is (= :sc/integer     (nth k 4)))
    (is (= :sc/rparen      (nth k 5)))
    (is (= :sc/lbrace      (nth k 6)))))

(deftest test-e2e-for-loop-two-commas
  (let [k (kinds "for (i = 1, i <= n, i = i + 1)")]
    (is (= :sc/kw-for (first k)))
    (is (= 2 (count (filter #(= :sc/comma %) k))))))

(deftest test-e2e-string-comparison-ops
  (is (= :sc/op-str-eq (second (kinds "a :==: b"))))
  (is (= :sc/op-str-ne (second (kinds "a :!=: b"))))
  (is (= :sc/op-str-lt (second (kinds "a :<: b"))))
  (is (= :sc/op-str-gt (second (kinds "a :>: b")))))

(deftest test-e2e-return-with-value
  (is (= [:sc/kw-return :sc/identifier :sc/op-plus :sc/integer]
         (kinds "return x + 1"))))

(deftest test-e2e-goto-statement
  (is (= [:sc/kw-go :sc/kw-to :sc/identifier]
         (kinds "go to END"))))

(deftest test-e2e-struct-decl
  (is (= [:sc/kw-struct :sc/identifier :sc/lbrace
          :sc/identifier :sc/comma :sc/identifier :sc/rbrace]
         (kinds "struct point { x, y }"))))

(deftest test-e2e-array-subscript
  (is (= [:sc/identifier :sc/lbracket :sc/identifier :sc/rbracket]
         (kinds "arr[i]"))))

(deftest test-e2e-concat-operator
  (is (= [:sc/identifier :sc/op-concat :sc/identifier]
         (kinds "x && y"))))

(deftest test-e2e-multiline-continuation
  (let [src "x = a +\nb +\nc\n"]
    (is (= 1 (newline-count src)))
    (is (= ["x" "=" "a" "+" "b" "+" "c"] (texts src)))))

(deftest test-e2e-eof-token
  (let [toks (tokens "x = 1")]
    (is (= :sc/eof (:kind (last toks))))))

(deftest test-e2e-empty-source-only-eof
  (let [toks (tokens "")]
    (is (= 1 (count toks)))
    (is (= :sc/eof (:kind (first toks))))))

(deftest test-e2e-comment-only-source-only-eof
  (let [toks (tokens "# nothing here\n# more nothing")]
    (is (= 1 (count toks)))
    (is (= :sc/eof (:kind (first toks))))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_snocone_parser
;;
;; Tests for sc->snobol4 — Snocone expression → SNOBOL4 source text.
;;
;; Reference: bconv table in the language specification.
;; Each test verifies that a Snocone expression compiles to the correct
;; SNOBOL4 source string.
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

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
