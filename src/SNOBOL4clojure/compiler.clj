(ns SNOBOL4clojure.compiler
  "The front end: SNOBOL4 source to IR. Statements are split, -INCLUDE expanded, each statement parsed by the grammar, its
  constant parts folded and its tree emitted as IR; a statement that does not compile is an error in <DIAGNOSTICS>."
  (:require [instaparse.core :as insta :refer [defparser]]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [SNOBOL4clojure.env :as env :refer [subtract parse-real STNO <STNO> <LABL> <CODE> <DISPNO> <POS> <CODELINE> <DIAGNOSTICS> <SOURCE> BLANKS &CASE fold-case block-end compile-error]]))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; The statement grammar (instaparse)
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; The subject of a statement is what binds tighter than concatenation ("If the expression comprising the subject contains the concatenation operator, the subject
;; should be enclosed in parenthesis", manual ch. 14), so N + 1 '4' matches '4' against N + 1; a body whose top is an alternation or a binary & (xalt, xand) is an
;; expression the statement evaluates, as sbl -bf takes A | B 'D'. ? binds loosest of all, so A B ? 'B' matches 'B' against A B, and S ? P1 ? P2 is (S ? P1) ? P2.
(def grammar
" stmt      ::=  label? body? goto? <__> <eos?>
  label     ::=  #'[0-9A-Za-z][^ \\t\\n]*'
  body      ::=  <_> (invoking | matching | replacing | assigning)
  goto      ::=  <__ ':' __> (jmp  |  sjmp (<__> fjmp)?  |  fjmp (<__> sjmp)?)
  invoking  ::=  subject | xand | xalt
  matching  ::=  subject <_> pattern | cat <_ '?' _> and qmatch*
  qmatch    ::=  <_ '?' _> and
  replacing ::=  subject <_> pattern <_ '='> replace
  assigning ::=  subject <_ '='> replace
 <subject>  ::=  at
 <pattern>  ::=  (<'?' _>)? and
 <replace>  ::=  (<_> expr)?
  jmp       ::=  target
  sjmp      ::=  <'S' | 's'> target
  fjmp      ::=  <'F' | 'f'> target
 <target>   ::=  <'('> expr <')'> | dgo
  dgo       ::=  <'<'> expr <'>'> | <'['> expr <']'>
  comment   ::=  <'*'> #'.*' <eol>
  control   ::=  <'-'> #'[^;\\n]*' <eos>
  eos       ::=  '\\n' | ';'
  eol       ::=  '\\n'
 <expr>     ::=  <__> asn <__>
  asn       ::=  mch | mch  <_  '='  _>  asn | mch <_> nulasn
  nulasn    ::=  <'='>
  mch       ::=  and | mch  <_  '?'  _>  and (<_ '=' _> and)?
  and       ::=  alt | and  <_  '&'  _>  alt
  alt       ::=  cat | cat (<_  '|'  _>  cat)+
  xalt      ::=  cat (<_  '|'  _>  cat)+
  xand      ::=  and <_  '&'  _>  alt
  cat       ::=  at  | at  (<_>          at)+
  at        ::=  sum | sum  <_  '@'  _>  at
  sum       ::=  hsh | sum  <_> '+' <_>  hsh
                     | sum  <_> '-' <_>  hsh
  hsh       ::=  div | hsh  <_  '#'  _>  div
  div       ::=  mul | div  <_  '/'  _>  mul
  mul       ::=  pct | mul  <_  '*'  _>  pct
  pct       ::=  xp  | pct  <_  '%'  _>  xp
  xp        ::=  cap | cap  <_> '^' <_>  xp
                     | cap  <_> '!' <_>  xp
                     | cap  <_> '**' <_> xp
  cap       ::=  ttl | cap  <_> '$' <_>  ttl
                     | cap  <_> '.' <_>  ttl
  ttl       ::=  uop | uop  <_  '~'  _>  ttl
  uop       ::=  ndx | '@' uop | '~' uop | '?' uop | '&' uop | '+' uop
                     | '-' uop | '*' uop | '$' uop | '.' uop | '!' uop
                     | '%' uop | '/' uop | '#' uop | '=' uop | '|' uop | '^' uop
  ndx       ::=  itm | ndx <'<'> (lst | nul) <'>'> | ndx <'['> (lst | nul) <']'>
  <itm>     ::=  I | R | S | N | grp | cnd | inv
  grp       ::=  <'('> expr <')'>
  cnd       ::=  <'('> expr <','> lst <')'>
  inv       ::=  N <'()'>  |  N <'('> lst <')'>
  <lst>     ::=  expr | arg (<','> arg)+
  <arg>     ::=  expr | nul
  nul       ::=  <__>
  <_>       ::=  <white+>
  <__>      ::=  <white*>
  I         ::=  #'[0-9]+'
  R         ::=  #'[0-9]+(\\.[0-9]*([eEdD][+-]?[0-9]+)?|[eEdD][+-]?[0-9]+)'
  S         ::=  #'\"([^\"]|\\x3B)*\"'  |  #'\\'([^\\']|\\x3B)*\\''
  N         ::=  #'[A-Za-z][A-Z_a-z0-9\\.]*'
  white     ::=  #'[ \\t]'
")

;; The EBNF above is parsed once, when this namespace is compiled, and the combinator map it gives is a constant of the class: a run no longer parses the grammar text
;; (two parsers built from the text took 380 ms and 7 G instructions of every start, of the 17 G an empty program costs). The text stays the one source of the grammar;
;; both parsers start from the same map, which instaparse takes in place of the text, so a rule changed above is compiled in again with the next build.
(defmacro ^:private compiled-rules [] (list 'quote (:grammar (insta/parser grammar))))
(def ^:private rules (compiled-rules))
(def parse-statement  (insta/parser rules :start :stmt  :total true))
(def parse-expression (insta/parser rules :start :expr))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; Compile error numbers: a rejected statement rescanned as SPITBOL's scanner reads it
;;
;; The compile error SPITBOL reports for a statement it cannot compile (manual Appendix D, errors 212 to 234), found by scanning the statement left to right as
;; SPITBOL's own scanner does: the first error met is the one reported. The parser (grammar.clj) decides whether a statement compiles; this namespace names the
;; error of one that does not, as sbl -bf names it.
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; The characters that are operators, unary or binary (manual ch. 15, Operators). A binary operator has a blank on each side; a unary one stands before its operand.
(def ^:private operator-char? #{\+ \- \* \/ \! \^ \$ \. \& \@ \~ \? \% \# \| \=})

(defn- blank? [c] (or (= c \space) (= c \tab)))
(defn- letter? [c] (and c (or (<= (int \A) (int c) (int \Z)) (<= (int \a) (int c) (int \z)))))
(defn- digit? [c] (and c (<= (int \0) (int c) (int \9))))
(defn- name-char? [c] (or (letter? c) (digit? c) (= c \_) (= c \.)))

(defn- fail! [n i] (throw (ex-info "syntax" {::error n ::at i})))

;; A closer or a comma where nothing is open: ")" is 224, "]" or ">" 225 and "," 223.
(defn- stray! [c i] (fail! (case c \) 224 \, 223 225) i))

(defn- at [^String s i] (when (< i (count s)) (.charAt s i)))
(defn- skip-blanks [s i] (if (blank? (at s i)) (recur s (inc i)) i))

(declare expression)

;; A number: digits, then an optional fraction and exponent ("1.", "1.5", "1E5", "1.e5", "1d5"). A letter, a digit, a period or a minus sign right after it is
;; "231 invalid numeric item" (sbl -bf: 12ABC, 1.2.3, 1E, 1.5E+, 1- 1).
(defn- number-end [s i]
  (let [digits (fn [j] (if (digit? (at s j)) (recur (inc j)) j))
        j      (digits i)
        ;; an integer past 64 bits is invalid too, at the item's start (sbl -bf: X = 9223372036854775808 is 231 at column 13)
        _      (when (and (not (#{\. \e \E \d \D} (at s j))) (> (- j i) 18)
                          (try (Long/parseLong (subs s i j)) false (catch NumberFormatException _ true)))
                 (fail! 231 i))
        j      (if (= \. (at s j)) (digits (inc j)) j)
        j      (if (#{\e \E \d \D} (at s j))
                 (let [k (if (#{\+ \-} (at s (inc j))) (+ j 2) (inc j))]
                   (if (digit? (at s k)) (digits k) (fail! 231 i)))
                 j)]
    ;; the error stands at the numeric item's first character (MEASURED on sbl -bf: X = 12ABC, 1.2.3, 1E, 1.5E+, 1- 1 and 3.14X each 231 at column 13, the item's
    ;; start, where this engine said 15, 16, 14, 16, 14 and 17, the character that spoiled it; corpus tests/snobol4 compile_diag__p13, p13b)
    (if (or (name-char? (at s j)) (= \- (at s j))) (fail! 231 i) j)))

;; The list inside ( ) or < > or [ ], its elements separated by commas, any of them empty. Returns the index after the closer. A colon or a semicolon ends the
;; scan inside it as the end does (sbl -bf: (4 :(L)) and F(4 :(L)) are "226 missing right paren", A<4 :(L)> and A[4 :(L)] "229 missing right array bracket").
;; Where the statement ends inside an open list, the error stands at the start of the last token scanned: a literal at its opening quote, a name or a number at
;; its first character, a name with the ( or [ that opens its call or subscript at the name, any other character at itself (MEASURED on sbl -bf: "X = F('a'" is 226
;; at the quote, column 15; "X = F(YYY" and "X = F(12345" at 15; "X = F('ab'   " at 15; "X = F(" at 13, the F; "X = A['ab'" 229 at 15; "X = F(G(1)" at the
;; closing paren, 18; "X = (" at 13; where this engine said the last character, 17, 17, 19, 18, 14, 18; and "F(" then "+ 'a'" and "+ 'b'" is 226 at (3,3), the
;; quote; corpus setl4_spitbol sonnets-1). A colon or a semicolon that ends the scan is the error's place itself.
(defn- last-token-start [^String s i]
  (let [j (loop [j (dec i)] (if (and (>= j 0) (blank? (at s j))) (recur (dec j)) j))
        c (at s j)
        back (fn [k] (loop [k k] (if (and (pos? k) (name-char? (at s (dec k)))) (recur (dec k)) k)))]
    (cond
      (neg? j)                                         i
      (#{\' \"} c)                                    (let [k (.lastIndexOf s (str c) (int (dec j)))] (if (neg? k) j k))
      (and (#{\( \[ \<} c) (pos? j) (name-char? (at s (dec j)))) (back (dec j))
      (name-char? c)                                   (back j)
      :else                                            j)))

(defn- element-list [s i closer]
  (loop [i i]
    (let [i (expression s i {:closer closer})
          c (at s i)]
      (cond
        (= c \,)                              (recur (inc i))
        (= c closer)                          (inc i)
        (nil? c)                              (fail! (if (= closer \)) 226 229) (last-token-start s i))
        (#{\: \;} c)                          (fail! (if (= closer \)) 226 229) i)
        (= c \))                              (fail! 224 i)
        (#{\] \>} c)                          (fail! 225 i)
        :else                                 (fail! 220 i)))))

;; An operand: unary operators, then a primary, then its subscripts. Returns [index-after kind], kind :literal for a string or number standing alone, :name for a
;; variable, and :value for anything else.
(declare operand)
;; an operator that a blank follows where an operand should be is 221, and 233 when it stands right after another unary operator (MEASURED on sbl -bf: "Y = - X"
;; and "Y = & X" are 221 at the operator, "Y = 'a' && b", "Y = .. X" and "Y = 'a' -- b" 233 at the second; corpus gate_cases_snobol4
;; test_gate_s4e_donewhen_runs_the_whole_criterion__w, a shell script's "cd "$HERE/../.." && pwd", where this engine said 212)
(defn- operand-after-unary [s i]
  (let [c (at s i)]
    (if (and (operator-char? c) (blank? (at s (inc i)))) (fail! 233 i) (operand s i))))

(defn- operand [s i]
  (let [c (at s i)]
    (cond
      (nil? c)                         (fail! 221 i)
      ;; an operator with no operand after it: before a blank the error stands at the operator itself, before a closer or the end at the character after it
      ;; (MEASURED on sbl -bf: "L       = 'X'" and "L  =  'X'", a label then a body that begins with =, are 221 at the = (columns 9 and 4), where this engine
      ;; said 10 and 5; "        X = *" and "        X = (1 +)" are 221 after the operator, columns 13 and 17, as here; the rungs compile_diag__p01, p30 and kin)
      (and (operator-char? c) (let [d (at s (inc i))] (or (nil? d) (blank? d) (#{\) \] \> \, \: \;} d))))
      (fail! 221 (if (blank? (at s (inc i))) i (inc i)))
      ;; .X names X and &K a keyword: what follows is a name, never a literal or another name (sbl -bf: .5, ..X and &1 are "212 value used where name is required")
      (#{\. \&} c)
      (let [d (at s (inc i))]
        (cond (or (letter? d) (and (= c \.) (#{\$ \( \*} d)))   [(first (operand s (inc i))) :value]
              (and (operator-char? d) (blank? (at s (+ i 2))))       (fail! 233 (inc i))
              :else                                                  (fail! 212 (inc i))))
      (operator-char? c)               [(first (operand-after-unary s (inc i))) :value]
      :else
      (let [[j kind]
            (cond
              (#{\' \"} c)  (let [k (.indexOf ^String s (str c) (int (inc i)))] (if (neg? k) (fail! 232 i) [(inc k) :literal]))
              (digit? c)    [(number-end s i) :literal]
              (letter? c)   (let [j (loop [j (inc i)] (if (name-char? (at s j)) (recur (inc j)) j))]
                              (if (= \( (at s j)) [(element-list s (inc j) \)) :value] [j :name]))
              (= c \()      [(element-list s (inc i) \)) :value]
              (#{\< \[} c)  (fail! 222 i)
              (= c \))      (fail! 224 i)
              (#{\] \>} c)  (fail! 225 i)
              (= c \,)      (fail! 223 i)
              (#{\: \;} c)  (fail! 221 i)
              :else         (fail! 230 i))]
        ;; subscripts follow, with or without blanks before them (sbl -bf: A [1] is A<1>)
        (loop [j j kind kind]
          (let [k (skip-blanks s j)]
            (case (at s k)
              \< (recur (element-list s (inc k) \>) :name)
              \[ (recur (element-list s (inc k) \]) :name)
              [j kind])))))))

;; An expression, up to the end of the statement, a closer, a comma, a colon or a semicolon (at any depth: SPITBOL's scan ends there), or (at the top of the body)
;; the = of an assignment or a replacement. Elements are concatenated by blanks; a binary operator has a blank on each side. ctx is {:closer c} inside brackets
;; and {:top true :stop-at-equals b} in the body. Returns the index where it stopped.
(defn- expression [s i ctx]
  (let [stop? (fn [c] (or (nil? c) (#{\: \;} c) (and (not (:top ctx)) (#{\) \] \> \,} c))))
        i     (skip-blanks s i)
        c     (at s i)]
    (cond
      (and (:top ctx) (#{\) \] \> \,} c)) (stray! c i)
      (stop? c) i
      :else
      ;; q counts the binary ?s at this level and pre the operands before the first: a statement's ? binds loosest, so what stands before it is the subject
      ;; st is where the operand just read begins
      (loop [[j kind] (operand s i) n 1 q 0 pre 0 st i]
        (let [c (at s j)]
          ;; right after an operand comes a blank, the end, a closer, a comma or a colon: another operand there lacks its operator, and an operator there lacks
          ;; the blank a binary operator needs
          (cond
            (and (#{\) \] \> \,} c) (:top ctx))                       (stray! c j)
            (stop? c)                                                   j
            (or (letter? c) (digit? c) (#{\' \" \(} c))                (fail! 220 j)
            ;; an operator touching the operand before it is 233 there, a + or - at that operand's start (MEASURED on sbl -bf: Y = X+1 and Y = X-a are 233 at
            ;; X, 13; Y = X*1 at the *, 14; Y = A<X+1> and F(X,X+1) at the X; corpus slobol_tests test/arith "junk = junk-adsfsd", 233 at column 9, and jmu_cs430
            ;; snobol4p/test5 "anArray[i+1]" at 29, where this engine named the operator)
            (operator-char? c)                                          (fail! 233 (if (#{\+ \-} c) st j))
            (not (blank? c))                                            (fail! 230 j)
            :else
            (let [k (skip-blanks s j)
                  d (at s k)
                  ;; ** is the one operator of two characters
                  k (if (and (= d \*) (= \* (at s (inc k)))) (inc k) k)
                  e (at s (inc k))]
              (cond
                (and (#{\) \] \> \,} d) (:top ctx))                   (stray! d k)
                (stop? d)                                               k
                (and (operator-char? d) (or (nil? e) (blank? e)))
                (if (= d \=)
                  ;; an assignment's object is a name: a literal standing alone is "212 value used where name is required"
                  ;; and so is the subject of a replacement after a statement's ?: a concatenation before it, or the value of an earlier match, is 212 (MEASURED on
                  ;; sbl -bf: X 'b' ? LEN(1) = 'Z' and Y ? 'l' ? '' = 'L' are 212, X ? 'a' 'b' = 'c', (X) ? 'a' = 'b' and $X ? 'a' = 'c' compile; corpus
                  ;; gate_cases_snobol4 a_statement_level_query_takes_a_concatenated_subject_and_chains_left_to_right__refuse1 and 2, which this engine named 220)
                  (cond (and (= kind :literal) (= n 1))  (fail! 212 k)
                        ;; in the statement's pattern (:stop-at-equals, the subject read before it) any ? is one more than the subject's own
                        ;; and it names no column (sbl -bf: "(3) : ERROR 212")
                        (and (:top ctx) (pos? q) (or (:stop-at-equals ctx) (> q 1) (> pre 1))) (fail! 212 :none)
                        (:stop-at-equals ctx)            k
                        :else                            (let [m (skip-blanks s (inc k))] (if (at s m) (expression s m (dissoc ctx :stop-at-equals)) m)))
                  (let [m (skip-blanks s (inc k))]
                    (cond
                      (or (nil? (at s m)) (#{\) \] \> \, \: \;} (at s m)))      (fail! 221 m)
                      ;; a goto's target is a name: an expression of more than one element is "212 value used where name is required"
                      (:goto ctx)                                                 (fail! 212 m)
                      :else                                                       (recur (operand s m) (inc n) (if (= d \?) (inc q) q) (if (and (= d \?) (zero? q)) n pre) m))))
                (:goto ctx) (fail! 212 k)
                :else (recur (operand s k) (inc n) q pre k)))))))))

;; The goto field after the colon: S(label), F(label), (label) or the same with < >, each at most once (sbl -bf: 218 duplicated, 219 empty, 227 and 228 unclosed,
;; 234 anything else).
;; What spoils the goto field is 234, unless it is an operator touching what is before it, which SPITBOL's scanner reads as an operator where none may stand,
;; "233 invalid use of operator" (MEASURED on sbl -bf: "X = 1 :$", ":*", ":+", ":=", ":#", ":.", ":S$(L)", ":S(L)$" and ":S(L)F$" are 233, while ":  $" and
;; ":S(L) +", the operator after a blank, are 234, as ":)" and ":," are; corpus tests/snobol4 compile_diag__p45, where this engine said 234)
;; and a control character there is "230 illegal character", as in any field (MEASURED on sbl -bf: "        X = 1  :(L)" ended by a carriage return is 230 at
;; column 20; corpus snolib g360, a CR LF file)
(defn- bad-goto! [s p] (fail! (cond (and (at s p) (< (int (at s p)) 32) (not= \tab (at s p))) 230
                                    (and (operator-char? (at s p)) (pos? p) (not (blank? (at s (dec p))))) 233
                                    :else 234) p))

(defn- goto-field [s i]
  (let [i0 (skip-blanks s i)]
    (when (nil? (at s i0)) (fail! 219 i0))
    (loop [i i0 seen #{}]
      (let [i (skip-blanks s i)
            c (at s i)]
        (when c
          (let [[kind j] (case c (\S \s) [:S (inc i)] (\F \f) [:F (inc i)] [:G i])
                o        (at s j)]
            ;; a direct goto is :<C> or :[C] (MEASURED on sbl -bf: :[C], :S[C] and :F[C]S(L) compile and run as :<C> does; corpus exercism_snobol4's test
            ;; harnesses go :[compiled], where this engine said 234)
            (when-not (#{\( \< \[} o) (bad-goto! s j))
            ;; a duplicated element is 218 at its closer, once it is scanned (MEASURED on sbl -bf: "X = 1 :S(L)S(L)" is 218 at column 23, ":S(LL)S(LLL)" at 26,
            ;; ":(L)S(L)" at 22, ":S<L>S<L>" at 23, where this engine said 21, 22, 20 and 21, the opener; corpus tests/snobol4 compile_diag__p11c)
            (let [dup?   (or (seen kind) (and (= kind :G) (seq seen)) (seen :G))
                  closer (case o \( \) \< \> \[ \])
                  k      (skip-blanks s (inc j))
                  _      (when (= (at s k) closer) (fail! (if dup? 218 212) k))   ; an empty duplicate is 218 too (sbl -bf: :S(L)S() at 22)
                  k      (try (expression s k {:closer closer :goto true})
                              (catch clojure.lang.ExceptionInfo e
                                (if (and (= 226 (::error (ex-data e))) (= closer \))) (fail! 227 (::at (ex-data e))) (throw e))))]
              (cond
                (= (at s k) closer) (let [n (inc k) d (at s n)]
                                      (when dup? (fail! 218 k))
                                      (when-not (or (nil? d) (blank? d) (#{\S \s \F \f \( \<} d)) (bad-goto! s n))
                                      (recur n (conj seen kind)))
                ;; a colon or a semicolon ends the scan inside the goto too (sbl -bf: :(L:X) and :S(L :F(L)) are 227, :<L :> 228)
                (#{\: \;} (at s k)) (fail! (if (= closer \)) 227 228) k)
                ;; at the statement's end the error stands at the first character of the name the goto ends with (MEASURED on sbl -bf: "        :(END" is 227 at
                ;; column 11, the E, and "        :(LONGLABEL" at 11, where this engine said 13 and 19; "        X = 1 :S(A", :F($V) and :(A + B stand at their last name,
                ;; 18, 19 and 21; corpus tests/snobol4 compile_diag__p29)
                (nil? (at s k))
                (fail! (if (= closer \)) 227 228) (loop [b k] (if (and (pos? b) (name-char? (at s (dec b)))) (recur (dec b)) b)))
                :else               (fail! 212 k)))))))))

(defn syntax-error
  "The error SPITBOL reports for statement s, a line of source without its line end, as {:error n :column c} (c counted from 1), or nil when the scan finds none."
  [^String s]
  (try
    ;; a label begins with a letter or a digit and runs to a blank; anything else in the first column is "214 bad label or misplaced continuation line" (sbl -bf,
    ;; CODE: #L X = 1, (L X = 1, _L X = 1 and 'L' X = 1 are 214, and so is a line end or a form feed there)
    (when (and (at s 0) (not (blank? (at s 0))) (not (letter? (at s 0))) (not (digit? (at s 0)))) (fail! 214 0))
    (let [i (if (and (seq s) (not (blank? (at s 0)))) (loop [i 0] (if (and (at s i) (not (blank? (at s i)))) (recur (inc i)) i)) 0)
          i (skip-blanks s i)
          ;; the replacement after an =, then the goto field
          replacement (fn [k] (let [m (expression s (inc k) {:top true})] (when (at s m) (goto-field s (inc m)))))]
      (cond
        (nil? (at s i))   nil
        (= \: (at s i))   (goto-field s (inc i))
        :else
        (let [[j kind] (operand s i)
              k        (skip-blanks s j)
              e        (at s (inc k))]
          (cond
            ;; the subject is an element; an assignment's subject is a name
            (and (= \= (at s k)) (or (nil? e) (blank? e)))
            (do (when (= kind :literal) (fail! 212 k)) (replacement k))
            (and (= j k) (at s j) (not= \: (at s j)))
            ;; a character no element may hold is 230 here as anywhere, before the missing operator (MEASURED on sbl -bf: "A B\\C = 1", a label and then B and a
            ;; backslash, is 230 at column 4, where this engine said 220; corpus tests/snobol4 compile_diag__p33); an operator, a quote or a bracket is 220
            (let [c (at s j)]
              (cond (#{\) \] \> \,} c) (stray! c j)
                    (not (or (operator-char? c) (letter? c) (digit? c) (#{\' \" \( \< \[ \= \;} c))) (fail! 230 j)
                    ;; an operator touching the subject is "233 invalid use of operator", at the operator, or for + and - at the subject's first column
                    ;; (MEASURED on sbl -bf: "        M=1", X$Y, X*Y, X?Y, X|Y and X/Y are 233 at column 10, X+1, X-1, XY+1 and X1- at 9; X'a' and X"a" stay
                    ;; 220; corpus gate_cases_snobol4 test_gate_s4e_donewhen_runs_the_whole_criterion__w, a shell script's MSG="$HERE/s4e_msg.sh")
                    (operator-char? c) (fail! 233 (if (#{\+ \-} c) i j))
                    :else (fail! 220 j)))
            :else
            ;; the pattern, after a blank or the binary ? that makes the match explicit
            (let [k (if (and (= \? (at s k)) (or (nil? e) (blank? e))) (inc k) k)
                  m (expression s k {:top true :stop-at-equals true})]
              (cond
                (nil? (at s m))  nil
                (= \: (at s m))  (goto-field s (inc m))
                (= \= (at s m))  (replacement m)
                :else            (fail! 220 m)))))))
    (catch clojure.lang.ExceptionInfo e
      (when-let [n (::error (ex-data e))] (if (= :none (::at (ex-data e))) {:error n} {:error n :column (inc (or (::at (ex-data e)) 0))})))))

;; The text EVAL or CONVERT compiles is one expression, scanned to its end (sbl.min gtexp: expan builds the expression, then "error if not end of image").
;; {:error n} for the syntax error the scan meets first, else {:end i}, the index where the expression ends: the end of s, or a colon or a semicolon outside every
;; bracket before it.
(defn expression-end [^String s]
  (try {:end (expression s 0 {:top true})}
       (catch clojure.lang.ExceptionInfo e (when-let [n (::error (ex-data e))] {:error n}))))

;; A colon or a semicolon can end an expression in a statement, but "they are invalid as terminators for a string that is being converted to expression form", so
;; a string whose last character is one is refused before it is compiled (sbl.min gtexp).
(defn ends-in-mark? [^String s]
  (let [n (.length s)] (and (pos? n) (contains? #{\: \;} (.charAt s (dec n))))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; The emitter: parse tree to IR
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; An = whose target is a concatenation outside every bracket, at the top of a statement's replacement or of the text EVAL compiles, is a replacement of its own:
;; the first element is the subject and the rest the pattern, and its value is the subject's new value; inside brackets it is an assignment, whose target must be
;; a name (sbl -bf: X = A 'y' = 1 of A = 'xyz' makes X and A x1z; X = A 'y' = C 'b' = 3 replaces in C first; X = Y = A 'y' = 2 makes all three x2z; X = A 'y' 'z'
;; = 1 matches 'y' 'z'; EVAL("A 'y' = 1") replaces; X = (A 'y' = 1) is 212). A parenthesized value carries :paren in its metadata (the grammar's grp).
(defn top-replacement [r]
  (if (and (seq? r) (= '= (first r)) (= 3 (count r)) (not (:paren (meta r))))
    (let [[_ t v] r
          v       (top-replacement v)]
      (if (and (vector? t) (not (:paren (meta t))))
        (list '?= (first t) (if (= 2 (count t)) (second t) (vec (rest t))) v)
        (list '= t v)))
    r))

(defn emitter [ast]
  (insta/transform
    {:comment   (fn [cmt]    [:comment cmt])
     :control   (fn [ctl]    [:control ctl])
     :stmt      (fn [& ss]   (apply conj ss))
     ;; "Labels must begin with a letter or digit, optionally followed by an arbitrary string of characters" (manual ch. 14, Label field): L+, X-, Q# and HSORT.END
     ;; are labels, each a keyword, as the goto target naming it is
     :label     (fn [L]      {:label (keyword L)})
     :body      (fn [B]      {:body B})
     :invoking  (fn [S]      S)
     ;; "It is left associative" (manual, Appendix, SPITBOL extensions): S ? P1 ? P2 matches P2 against the text P1 matched
     :matching  (fn [S P & qs] (reduce (fn [m q] (list '? m q)) (list '? S P) qs))
     :qmatch    (fn [P]      P)
     :replacing (fn
                  ([S P]     (list '?= S P 'epsilon))
                  ([S P R]   (list '?= S P (top-replacement R))))
     :assigning (fn
                  ([S]       (list '= S 'epsilon))
                  ([S R]     (list '= S (top-replacement R))))
     :grp       (fn [x]      (if (instance? clojure.lang.IObj x) (vary-meta x assoc :paren true) x))
     :goto      (fn [& gs]   {:goto (reduce
                                      (fn [bs b]
                                        (let [key (first b) tgt (second b)]
                                          (assoc bs key
                                            (if (symbol? tgt) (keyword tgt) tgt))))
                                      {} gs)})
     :jmp       (fn [L]      [:G L])
     :sjmp      (fn [L]      [:S L])
     :fjmp      (fn [L]      [:F L])
     :expr      (fn [x]      x)
     ;; an = with nothing after it assigns the null string, the last of a chain too (MEASURED on sbl -bf: A = B = C = leaves A, B and C null, STRING;
     ;; corpus rosetta-snobol4 iban's country = checkdigits = line2 =, which this engine refused as 220)
     :nulasn    (fn []       "")
     :asn       (fn
                  ([x]       x)
                  ;; (S ? P) = R is S ? P = R, a match with a replacement: the grammar reads it this way inside parentheses
                  ([x y]     (if (and (seq? x) (= '? (first x)) (= 3 (count x)))
                               (list '?= (second x) (nth x 2) y)
                               (list '= x y))))
     :mch       (fn
                  ([x]       x)
                  ([x y]     (list '? x y))
                  ([x y z]   (list '?= x y z)))
     :and       (fn
                  ([x]       x)
                  ([x y]     (list '& x y)))
     :alt       (fn
                  ([x]       x)
                  ([x & ys]  (apply list '| x ys)))
     :xalt      (fn [x & ys] (apply list '| x ys))
     :xand      (fn [x y]    (list '& x y))
     :cat       (fn
                  ([x]       x)
                  ([x & ys]  (apply vector x ys)))
     :at        (fn
                  ([x]       x)
                  ([x y]     (list 'at-op x y)))
     :sum       (fn
                  ([x]       x)
                  ([x op y]  (list (symbol op) x y)))
     :hsh       (fn
                  ([x]       x)
                  ([x y]     (list 'sharp-op x y)))
     :div       (fn
                  ([x]       x)
                  ([x y]     (list '/ x y)))
     :mul       (fn
                  ([x]       x)
                  ([x y]     (list '* x y)))
     :pct       (fn
                  ([x]       x)
                  ([x y]     (list '% x y)))
     :xp        (fn
                  ([x]       x)
                  ;; ^ and ! are SPITBOL's synonyms for **; one operator reaches INVOKE
                  ([x op y]  (list (symbol (if (#{"^" "!"} op) "**" op)) x y)))
     :cap       (fn
                  ([x]       x)
                  ([x op y]  (list (symbol op) x y)))
     :ttl       (fn
                  ([x]       x)
                  ([x y]     (list 'tilde-op x y)))
     :uop       (fn
                  ([x]       x)
                  ([op y]    (case op
                               "@" (list 'at-op y)
                               "#" (list 'sharp-op y)
                               "~" (list 'tilde-op y)
                                   (list (symbol op) y))))
     ;; A<k> and A[k] are subscripts, F(k) a call (the inv node): the subscript form carries :subscript, so DATA[1] reads the array a variable DATA holds while
     ;; DATA('...') still calls the function, as sbl -bf tells them apart
     :ndx       (fn
                  ([n]       n)
                  ([n & xs]  (with-meta (apply list n xs) {:subscript true})))
     :cnd       (fn
                  ([x]       x)
                  ;; (e1, e2, ...): the first expression that succeeds; the keyword marks the group, which no concatenation can begin with
                  ([x & ys]  (apply vector :comma x ys)))
     :inv       (fn [f & xs] (apply list f xs))
     ;; an argument left out, F(A,,C) or F(,B), is the null string in its place
     :nul       (fn []       "")
     ;; a direct Goto, :<V>, branches to the code V holds, not to a label (manual ch. 14, Goto field)
     :dgo       (fn [x]      (list 'DIRECT x))
     :N         (fn [n]      (symbol n))
     :S         (fn [s]      (subs s 1 (subtract (count s) 1)))
     ;; an integer literal is a 64-bit integer; one past 9223372036854775807 is "231 Syntax error: Invalid numeric item", a compile error (MEASURED on sbl -bf:
     ;; X = 9223372036854775807 prints it, X = 9223372036854775808 and X = 99999999999999999999999 are 231 at the item; corpus tests/snobol4 compile_diag__p13c)
     :I         (fn [s] (let [v (edn/read-string s)]
                          (if (instance? Long v) v (throw (ex-info "syntax error: invalid numeric item" {:snobol/error 231 :snobol/compile true})))))
     ;; a real literal reads as SPITBOL reads it (env/parse-real); one too large for a real is "231 Syntax error: Invalid numeric item" (Appendix D), a compile error
     :R         (fn [s] (or (parse-real s) (throw (ex-info "syntax error: invalid numeric item" {:snobol/error 231 :snobol/compile true}))))}
    ast))

(defn parse+emit
  "Parse a SNOBOL4 expression string and emit the Clojure IR."
  [s]
  (first (emitter (parse-expression s))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; The compiler: source to statements, -INCLUDE, constant folding, CODE
;;
;; CODE! / CODE: compile SNOBOL4 source text into a labeled statement table
;; and load it into the runtime's global statement store.
;;
;; ## Stage 23A — Pre-compiled EDN cache
;;
;; The IR produced by CODE! is pure EDN: persistent maps, lists, keywords,
;; primitives.  Serialising with pr-str and reloading with edn/read-string
;; is lossless — the grammar and instaparse pipeline never run again.
;;
;; Benchmark (session 13): CODE! costs ~10.93ms per program — 95% of the
;; total CODE+RUN cost of ~11.45ms for short programs.  Caching eliminates
;; this entirely for repeated runs of the same source.
;;
;; ### Disk cache API
;;   (compile-to-file src path)   — compile src, write EDN to path
;;   (load-ir path)               — load [codes nos labels] from EDN file
;;   (CODE-ir ir)                 — like CODE but takes pre-compiled IR triple
;;   (CODE-cached src path)       — load from path if exists, else compile+save
;;
;; ### In-memory (memoised) cache API — fastest for test suites
;;   (CODE-memo src)              — compile once per unique source string
;;   (clear-memo!)                — evict all entries
;;   (memo-stats)                 — {:size N :hits N :misses N}
;;
;; ### Low-level serialisation
;;   (ir->edn ir)                 — [codes nos labels] -> EDN string
;;   (edn->ir s)                  — EDN string -> [codes nos labels]
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; -- Source tokenizer ---------------------------------------------------------
;; The names a statement's text holds, as SPITBOL's scanner interns them: every name outside a literal (a variable, a function, a label, a goto's target, the name
;; of a keyword after its &), a number never one (1.5E3 holds no E3), nor a goto's S or F; and a label's first character, which SPITBOL's label scan interns as a
;; name of its own (MEASURED on sbl -bf, &DUMP = 3: labels foo, bar.baz, QUX and zed list f, b, Q and z beside them, and END lists E). &DUMP = 3 lists them
;; (env/<NAMES>).
(defn- source-names [^String s]
  (let [n (count s) name-char? (fn [c] (or (Character/isLetterOrDigit (char c)) (= c \_) (= c \.)))]
    (loop [i 0 goto? false out (transient (if (and (pos? n) (Character/isLetterOrDigit (.charAt s 0))) [(subs s 0 1)] []))]
      (if (>= i n)
        (persistent! out)
        (let [c (.charAt s i)]
          (cond
            (or (= c \') (= c \")) (let [k (.indexOf s (str c) (int (inc i)))] (recur (if (neg? k) n (inc k)) goto? out))
            (= c \:)                (recur (inc i) true out)
            (Character/isDigit c)   (recur (loop [j (inc i)] (if (and (< j n) (name-char? (.charAt s j))) (recur (inc j)) j)) goto? out)
            (Character/isLetter c)  (let [j (loop [j (inc i)] (if (and (< j n) (name-char? (.charAt s j))) (recur (inc j)) j))
                                          t (subs s i j)]
                                      (recur j goto? (if (and goto? (#{"S" "F"} t) (< j n) (#{\( \<} (.charAt s j))) out (conj! out t))))
            :else                   (recur (inc i) goto? out)))))))
(defn- comment? [cmd] (re-find #"^\*" cmd))
(defn- control? [cmd] (re-find #"^\-" cmd))
;; A -CASE card sets whether the statements after it are compiled with lower case names folded to upper case: -CASE N folds when N is not 0, -CASE alone or -CASE 0 does not.
;; The card's own name follows the mode: with folding on it may be written in any case, with folding off only as -CASE.
(defn- case-card [fold command]
  (when-let [[_ n] (re-find (if fold #"(?i)^-CASE(?:\s+(\d+))?\s*$" #"^-CASE(?:\s+(\d+))?\s*$") command)]
    (boolean (and n (not (zero? (Long/parseLong n)))))))

(defn- error-node [info] (list 'ERROR (:line info) (:column info) (:text info)))

(defn- open-parens
  "The parentheses s leaves open, quoted text aside."
  [^String s]
  (loop [i 0 d 0 q nil]
    (if (>= i (count s))
      d
      (let [c (.charAt s i)]
        (cond q                    (recur (inc i) d (if (= c q) nil q))
              (#{\' \"} c)         (recur (inc i) d c)
              (= c \()             (recur (inc i) (inc d) q)
              (= c \))             (recur (inc i) (dec d) q)
              :else                (recur (inc i) d q))))))

(defn- split-source
  "Split SNOBOL4 source into raw statement strings, each with the physical line it begins on, as [statement line]. A blank physical line is a statement of its own as
   SPITBOL numbers them, and is [:blank line]. ls is where the physical line being read begins, line is its number and sl the line the statement being read began on.
   A record, the text CODE compiles, is one line: a line end in it is a character as any other, and only a semicolon ends a statement (record? true)."
  ([src] (split-source src false))
  ([src record?]
  ;; the null string is no line at all, so CODE('') compiles no statement (sbl -bf), not a blank one
  (let [chars (if (or record? (= "" (str src)) (string/ends-with? (str src) "\n")) (str src) (str src "\n"))
        n     (count chars)
        ;; a record keeps the line ends at the end of a statement, which are no blanks
        ;; a carriage return is a character of its line, never a line end or a blank: in a statement it is "230 illegal character", a line of one is 214, and
        ;; END followed by one is no END (MEASURED on sbl -bf: "        X = 1\r" is 230 at column 14, "\r" alone 214, a file whose lines all end CR LF has "No END
        ;; statement found" and exits 1; corpus snolib g360, a CR LF file sbl refuses, which this engine ran)
        trimr (if record? #(string/replace % #"[ \t]+\z" "") #(string/replace % #"[ \t\n]+\z" ""))
        blank-line? (fn [from to] (every? #(and (Character/isWhitespace ^char %) (not= \return %)) (subs chars from to)))]
    ;; "A continuation mark ... is placed in the first column of the additional lines" (manual ch. 14): a + or . line that continues nothing -- the first line, a
    ;; line after a comment line, or after a statement closed by a semicolon -- is "214 bad label or misplaced continuation line" at compile time, as sbl -bf reports it.
    ;; The diagnostics ride on the result as metadata, {:diagnostics [[line 214] ..]}.
    (loop [i 0 cur (StringBuilder.) stmts [] in-sq false in-dq false ls 0 line 1 sl 1
           diags (if (and (pos? n) (#{\+ \.} (.charAt chars 0))) [[1 214]] [])]
      (if (>= i n)
        (let [s (trimr (str cur))]
          ;; the blanks after a record's last semicolon are a statement (sbl -bf: CODE(' X = 1 ; ') is two statements and the block's closing one)
          (with-meta (cond (seq s) (conj stmts [s sl]) (and record? (pos? (.length cur))) (conj stmts [:blank sl]) :else stmts) {:diagnostics diags}))
        (let [ch (.charAt chars i)
              sl (if (zero? (.length cur)) line sl)]
          (cond
            ;; a quote in the label field opens no literal: the label runs to the first blank, whatever it holds (MEASURED on sbl -bf: the line
            ;; HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"; MSG="$HERE/s4e_msg.sh" is one statement, its body's literals reading the semicolon as
            ;; text, so only its 233 at column 24 is named; corpus gate_cases_snobol4 test_gate_s4e_donewhen_runs_the_whole_criterion__w)
            (and (#{\' \"} ch) (pos? (.length cur)) (not (Character/isWhitespace (.charAt cur 0)))
                 (not (string/includes? (str cur) " ")) (not (string/includes? (str cur) "\t")))
            (recur (inc i) (.append cur ch) stmts in-sq in-dq ls line sl diags)
            (and (= ch \') (not in-dq))
            (recur (inc i) (.append cur ch) stmts (not in-sq) in-dq ls line sl diags)
            (and (= ch \") (not in-sq))
            (recur (inc i) (.append cur ch) stmts in-sq (not in-dq) ls line sl diags)
            ;; a comment line (* in column 1) is one comment: a semicolon in it separates nothing. Every statement a semicolon ends is one, an empty or blank one a
            ;; statement SPITBOL numbers and never runs, as a blank line is (sbl -bf: X = 1 ;; Y = 2 is three statements, two executed)
            ;; a semicolon inside a parenthesis the statement leaves open separates nothing: the statement goes on (MEASURED on sbl -bf: "        Y = ( ; Z = 1 +" is
            ;; one statement, 226 at the semicolon, column 15, and no 221 for Z; corpus tests/snobol4 compile_diag__p22); a parenthesis in the label field is no
            ;; bracket, the label running to the first blank ("(  a; }" is 214 and 230 at the })
            ;; a statement whose label field begins with what no label may begin with is 214 at its first column, and SPITBOL reads no further on that line, so
            ;; a semicolon later in it ends nothing (MEASURED on sbl -bf: "#   not ambiguous ... that; refusing ..." is one statement, the next line the next
            ;; number; corpus gate_cases_snobol4 test_gate_s4e_donewhen_runs_the_whole_criterion__w, a shell script, where this engine counted two and named 223)
            (and (= ch \;) (not in-sq) (not in-dq) (not (and (pos? (.length cur)) (= \* (.charAt cur 0)))) (not (pos? (long (open-parens (let [t (str cur)] (if (and (seq t) (not (Character/isWhitespace (.charAt t 0)))) (subs t (or (string/index-of t " ") (count t))) t))))))
                 ;; but after ( [ < > : or , the scan reads on and the semicolon ends the statement (MEASURED on sbl -bf: "[  a; }" and "(  a; }" are 214 and
                 ;; 230 at the }, "#  a; }", "!  a; }" and "_  a; }" 214 alone; corpus gate_cases_snobol4 test_gate_s4e_donewhen_runs_the_whole_criterion__w, whose
                 ;; [ -f ... ] || { ...; exit 2; } lines sbl reads as three statements)
                 (not (and (pos? (.length cur)) (let [c (.charAt cur 0)] (not (or (Character/isLetterOrDigit c) (#{\space \tab \- \+ \. \( \[ \< \> \: \,} c)))))))
            (let [s (trimr (str cur))]
              (recur (inc i) (StringBuilder.) (conj stmts (if (seq s) [s sl] [:blank sl])) false false ls line line diags))
            (and (= ch \newline) (not record?))
            (let [s (trimr (str cur))
                  next-i (inc i)
                  next-ch (when (< next-i n) (.charAt chars next-i))
                  cont? (and next-ch (or (= next-ch \+) (= next-ch \.)))]
              (if (seq s)
                (cond
                  ;; a comment is one line long: a continuation after it is misplaced
                  (and cont? (= \* (.charAt s 0)))
                  (recur (inc i) (StringBuilder.) (conj stmts [s sl]) false false (inc i) (inc line) (inc line) (conj diags [(inc line) 214]))
                  cont?
                  (recur (+ i 2) (.append cur \space) stmts false false (inc i) (inc line) sl diags)
                  :else
                  (recur (inc i) (StringBuilder.) (conj stmts [s sl]) false false (inc i) (inc line) (inc line) diags))
                (let [blank? (blank-line? ls i)
                      ;; a source line is read with its trailing blanks gone, but a CODE string, one record with no newline, is not: the blanks after its last
                      ;; semicolon are a statement (sbl -bf: CODE(' X = 1 ; ') is two statements and the block's closing one)
                      tail?  (and (pos? (.length cur)) (= i (dec n)) (not (string/ends-with? (str src) "\n")))]
                  (recur (inc i) (StringBuilder.) (if (or blank? tail?) (conj stmts [:blank line]) stmts) false false (inc i) (inc line) (inc line)
                         ;; the line ended in a semicolon, which closed its last statement: a continuation after it is misplaced
                         (if (and cont? (not blank?)) (conj diags [(inc line) 214]) diags)))))
            :else
            (recur (inc i) (.append cur ch) stmts in-sq in-dq ls line sl diags))))))))

;; A line end in a statement, outside its literals: in a record it is a character SPITBOL's scan takes in a label and refuses anywhere else (syntax.clj).
(defn- line-end-outside-literals? [^String s]
  (loop [i 0 q nil]
    (when (< i (count s))
      (let [c (.charAt s i)]
        (cond (and q (= c q))                    (recur (inc i) nil)
              q                                  (recur (inc i) q)
              (#{\' \"} c)                       (recur (inc i) c)
              (#{\return \newline} c)            true
              :else                              (recur (inc i) nil))))))

;; -- -INCLUDE preprocessor ----------------------------------------------------

(declare preprocess-includes*)

(defn preprocess-includes
  "Recursively expand -INCLUDE 'file' (or -INCLUDE \"file\") directives.

   Scans source line-by-line.  When a line matches (case-insensitive)
     -INCLUDE 'filename'   or   -INCLUDE \"filename\"
   it reads that file relative to each directory in search-path (first match
   wins) and splices its content inline.  The include file is itself
   preprocessed recursively.

   As SPITBOL has it, a file name is included once: a second -INCLUDE of the same name is skipped, and so is a cycle; the names kept are trimmed, so a name
   written with a blank after it is included every time. The splice is bracketed by
   the cards -FILE-ENTER \"name\" and -FILE-LEAVE (STALE), which CODE! reads to keep &FILE and &LINE. STALE is written when the included file's last line is a comment,
   the case in which SPITBOL leaves the included file's name as &FILE for the statement that follows.

   Parameters:
     src          – SNOBOL4 source string
     search-path  – seq of directory path strings; default [\".\"]
     seen         – (internal) the set of names already included

   Returns the fully expanded source string."
  ([src] (preprocess-includes src ["."]))
  ([src search-path] (preprocess-includes src search-path #{}))
  ([src search-path seen] (first (preprocess-includes* src search-path seen))))

(defn- preprocess-includes*
  "preprocess-includes, returning [expanded-source seen cards], cards the -INCLUDE lines as written, one for each -FILE-ENTER card in order, which the listing prints."
  [src search-path seen]
  ;; A control card is a card in column 1, its name in upper case: snobol4jvm never folds case (Lon 2026-11-17 18:2x CST, in the JVM window, verbatim: "We WILL
  ;; NEVER support case-folding. We will always have upper-case built-in keywords and functions."). MEASURED on sbl -bf the same: -include 'f', -Include 'f' and
  ;; " -INCLUDE 'f'" include nothing and raise nothing, -INCLUDE 'f' includes f; so csnobol4_suite json1, whose first line is -include 'json.sno', runs without it.
  (let [include-re #"^-INCLUDE\s+['\"]([^'\"]+)['\"]"]
    ;; ph counts the includes that could not be opened: SPITBOL reads the lines after such a card as inside the file it never opened, its listing at depth 1, and
    ;; the first END after it closes that file, ending no program, so a later END is the program's or there is none (MEASURED on sbl -bf: a statement, a missing
    ;; -INCLUDE, a statement, END is the 285 page, then "No END statement found in source file(s).", exit 1; with a second END it is rc 231; corpus tests/snobol4
    ;; compile_diag__p15, which lists "2    1          Z = )"). That END becomes the card -DEPTH-LEAVE, on its own line, so no line moves.
    (loop [lines (string/split src #"\n") out [] seen seen cards [] ph 0]
      (if-let [line (first lines)]
        (if-let [[_ fname] (re-find include-re line)]
          (let [found (some (fn [dir]
                              (let [f (clojure.java.io/file dir (string/trim fname))]
                                (when (.exists f) f)))
                            search-path)]
            (cond
              ;; a file that cannot be opened leaves its card where it stands, which CODE! reads as "285 include file cannot be opened" (MEASURED on sbl -bf:
              ;; -INCLUDE 'nosuch.inc' is "(1,10) : ERROR 285 -- include file cannot be opened", the column the quote's, and nothing runs; corpus rosetta-snobol4
              ;; call-a-function-in-a-shared-library and include-a-file, which this engine ran on)
              (nil? found)
              (recur (rest lines) (conj out line) seen cards (inc ph))
              ;; "SPITBOL remembers only the 'trimmed' names of files included so far, so the comparison with the blank-padded name will fail, and the file will be
              ;; included again" (manual ch. 14, -INCLUDE): the name as written is looked up among the trimmed names
              (seen fname)
              (recur (rest lines) (conj out (str "* -INCLUDE skipped (already included): " fname)) seen cards ph)
              :else
              (let [text                (slurp found :encoding "ISO-8859-1")
                    ;; the included file's own directory joins the search path
                    [body seen' inner]  (preprocess-includes* text (distinct (cons (.getParent found) search-path)) (conj seen (string/trim fname)))
                    ;; the last line as the file has it, a blank line after a comment included: split-lines drops trailing empty lines, so a file whose comment is
                    ;; followed by a blank line read stale (MEASURED on sbl -bf: an include ending "* c" leaves its name as &FILE for the next statement, one
                    ;; ending "* c" and a blank line does not; corpus gimpel asm_driver, whose rpad.inc ends so, where this engine named rpad.inc for the
                    ;; statements of asm.spt after it)
                    last-line           (let [ls (string/split text #"\r?\n" -1)] (last (if (and (next ls) (= "" (peek ls))) (pop ls) ls)))
                    ;; and so does one whose last line ends in a semicolon, alone or before a comment (MEASURED on sbl -bf: an include ending "X = 1 ;" or
                    ;; "X = 1 ;* c" leaves its name as &FILE for the END after it, one ending "X = 1 ; Y = 2" or "X = 1" does not; corpus rosetta-snobol4
                    ;; substring-top-and-tail, whose last line ends ";* strip both first and last characters")
                    stale?              (boolean (and last-line (or (string/starts-with? last-line "*") (re-find #";[ \t]*(\*[^'\"]*)?$" last-line))))]
                (recur (rest lines)
                       (into out (concat [(str "-FILE-ENTER \"" (string/trim fname) "\"")] (string/split body #"\n") [(str "-FILE-LEAVE" (when stale? " STALE"))]))
                       seen'
                       (into (conj cards line) inner)
                       ph))))
          (if (and (pos? ph) (re-find #"^(END|end)([ \t;]|$)" line))
            (recur (rest lines) (conj out "-DEPTH-LEAVE") seen cards (dec ph))
            (recur (rest lines) (conj out line) seen cards ph)))
        [(string/join "\n" out) seen cards]))))

;; The source position a card changes. A -LINE card sets the number and file of the line that follows it; a -FILE-ENTER card (written by preprocess-includes in place of the
;; -INCLUDE card) starts the included file at line 1 and remembers where the including file resumes, which is after the line the -INCLUDE card stood on; the matching
;; -FILE-LEAVE card resumes it, in the including file's name unless it says STALE, which keeps the included file's name for the one statement that
;; follows. A position is {:file f :bp physical-line-of-the-next-line :bl its number :stack [..]}.
(defn- position-card [fold command pline pos]
  (let [card (fn [re] (re-find (if fold (re-pattern (str "(?i)" re)) (re-pattern re)) command))]
    (cond
      (card "^-LINE\\s+\\d+")
      ;; "-LINE line_number 'filename' ... The file name is optional, but must be quoted if it appears" (manual ch. 14): in single or double quotes
      (let [[_ n f1 f2] (re-find #"^-\S+\s+(\d+)(?:\s+(?:\"([^\"]*)\"|'([^']*)'))?" command)]
        (assoc pos :file (or f1 f2 (:file pos)) :revert nil :bp (inc pline) :bl (Long/parseLong n)))
      (card "^-FILE-ENTER\\s+\"")
      (let [[_ f] (re-find #"\"([^\"]*)\"" command)]
        (assoc pos :file f :bp (inc pline) :bl 1 :stack (conj (:stack pos) (assoc pos :card-line (+ (:bl pos) (- pline (:bp pos)))))))
      (card "^-FILE-LEAVE")
      (let [outer (peek (:stack pos))]
        (let [stale (boolean (re-find #"STALE" command))]
          (assoc outer :file (if stale (:file pos) (:file outer)) :revert (when stale (:file outer)) :bp (inc pline) :bl (inc (:card-line outer)) :stack (pop (:stack pos)))))
      :else pos)))

;; -- Constant folding -----------------------------------------------------------
;; SPITBOL evaluates as a statement is compiled what is made of constants alone, and an error raised there is the statement's compile error (sbl -bf, measured:
;; CODE(' X = 1 / 0') is refused with 14 and a program holding it with rc 231; ' V = &UCASE * 2' with 26; ' X = LEN(-1)' with 121). The constants are literals and
;; the keywords whose values never change; what folds is arithmetic, concatenation, and the built-in functions sbl was seen to fold, given constants (EQ, LT, DUPL,
;; SUBSTR, ARRAY and CONVERT were seen not to; within a deferred *X the constants fold, and *X itself is no constant: *(1 / 0) is 14).
(def ^:private constant-keywords '#{ABORT ALPHABET ARB BAL FAIL FENCE LCASE REM SUCCEED UCASE})
(def ^:private folded-operators '#{+ - * / **})
;; The functions are those whose svblk carries svpre, "the compiler [may] preevaluate a call to the associated system function if all the arguments are themselves
;; constants. functions in this category must have no side effects and must never cause failure. the call may generate an error condition" (sbl.min svpre): the
;; mathematical ones among them too (sbl -bf, CODE: SQRT(-0.5) is 314, EXP(1000000) 305, LN(0) 307, LN(-1) 315, SIN('a') 308, COS('a') 303, TAN('a') 309,
;; ATAN('a') 301, CHOP('a') 302). TRIM and the predicates carry none, so a call of one is no constant (sbl -bf, CODE: TRIM(ARB) and LGT(ARB, 1) compile, and so
;; do IDENT(1, 1) + ARB, DIFFER(1, 2) + ARB, INTEGER(1) + ARB, LGT(2, 1) + ARB and TRIM('a ') + ARB, where SIZE('a') + ARB is 2).
(def ^:private folded-functions
  '#{SIZE LEN ANY NOTANY SPAN BREAK BREAKX POS RPOS TAB RTAB ARBNO FENCE REMDR CHAR REPLACE LPAD RPAD REVERSE DATATYPE SQRT EXP LN SIN COS TAN ATAN CHOP})

;; -- Names ----------------------------------------------------------------------
;; Where a name is wanted -- the operand of unary . @ and &, the right of binary . and $, the subject of an assignment or a replacement, a goto's operand -- an
;; expression that gives a value is "212 syntax error: value used where name is required" as the statement is compiled, found before an error folding what is inside it, and
;; operands left to right (sbl -bf: A . (1 / 0) is 212 and (1 / 0) . A is 14; F(.('a' + 1), 1 / 0) is 212 and F(1 / 0, .('a' + 1)) 14; :(1 + 2), :('L'),
;; :(X ? 'L') and (1 / 0) = 5 are 212). A name is a variable, an element, a call, $X, *X, &K, or an operator SPITBOL leaves for OPSYN, which is called as a
;; function is (unary ! % / # = |, binary # % & ~ @: :(!X) and :(X @ Y) compile); the operators SPITBOL defines give values, and so do a literal, a
;; concatenation and a selection (:(+X), :(.X), :(X ** Y), :((X, Y)) are 212).
(def ^:private unary-names  '#{$ * & ! % / sharp-op = |})
(def ^:private binary-names '#{sharp-op % & tilde-op at-op})
(def ^:private operators    '#{+ - * / ** sharp-op % at-op & tilde-op | ? ?= = . $ !})

(defn- name-form? [x]
  (cond
    (or (symbol? x) (keyword? x)) true
    (list? x) (let [[op & args] x n (count args)]
                (cond (:subscript (meta x))                   true
                      (not (symbol? op))                      true
                      (and (= 1 n) (unary-names op))          true
                      (and (= 2 n) (binary-names op))         true
                      (operators op)                          false
                      :else                                   true))
    :else false))

(defn- name-required! [x]
  (when-not (name-form? x) (throw (ex-info "syntax error: value used where name is required" {:snobol/error 212}))))

;; the operands of op given args that are name positions, by their index
(defn- name-positions [op args]
  (let [n (count args)]
    (cond (and (= 1 n) ('#{. at-op &} op)) #{0}
          (and (= 2 n) ('#{. $} op))    #{1}
          (and (= 2 n) (= '= op))       #{0}
          (and (= 3 n) (= '?= op))      #{0}
          :else                          #{})))

;; The constants beyond literals and keywords, as sbl -bf's compiler takes them (sbl.min svkvc; CODE refuses each): the seven protected pattern variables
;; (SIZE(ARB) is 189, ARB + 1 is 1; env/protected-variables); a deferred expression, *E, folded within too (*X + 1 is 1, *(1 / 0) 14); an alternation of
;; constants (('a' | 'b') + 1 is 1); the name of a variable, a keyword or an expression, .X .&K .*E (.X + 1 is 1, .&ERRTEXT 'z' is 8, LEN(.X) 120), and .$C of
;; a constant C, whose value names a variable or is "239 indirection operand is not name" (.$ARB, .$LEN(1), .$*X and .$&FENCE are 239, .$'X' and .$1 compile);
;; a cursor assignment @X or @$C (SIZE(@X) is 189, @$ARB 239); and a capture into such a name of a constant pattern (LEN(1) . X + 1 is 1, LEN(1) . $ARB 239).
;; A name that holds an element or a call's is found as the program runs (.A[1] + 1, .F() + 1, LEN(1) . A[1] + 1 and Q . $ARB compile).
(defn- pattern-variable [sym]
  (some-> (requiring-resolve (symbol "SNOBOL4clojure.env" (name sym))) var-get))

(declare fold)

(defn- constant-name?
  "True for a name that is a constant: a variable, a keyword, an expression, or $C of a constant C."
  [x]
  (or (symbol? x)
      (and (list? x) (= 2 (count x)) (or (= '& (first x)) (= '* (first x))))
      (and (list? x) (= 2 (count x)) (= '$ (first x)) (first (fold (second x))))))

(defn- fold
  "[constant? value] of the IR n, its constant parts evaluated as SPITBOL's compiler evaluates them; an error raised there is thrown."
  [n]
  (let [eval! (requiring-resolve 'SNOBOL4clojure.operators/EVAL!)]
    (cond
      (or (string? n) (number? n)) [true n]
      (and (symbol? n) (SNOBOL4clojure.env/protected-variables n)) [true (pattern-variable n)]
      (vector? n)
      (let [comma? (= :comma (first n))
            parts  (mapv fold (if comma? (rest n) n))]
        (if (and (not comma?) (every? first parts)) [true (eval! n)] [false nil]))
      (list? n)
      (let [[op & args] n
            names (name-positions op args)]
        (cond
          (and (= '& op) (= 1 (count args)))  (do (when-not (symbol? (first args)) (name-required! (first args)))
                                                  (if (constant-keywords (first args)) [true (eval! n)] [false nil]))
          (and (= '* op) (= 1 (count args)))  (do (fold (first args)) [true (eval! n)])   ; folded within, and itself a constant expression
          ;; .X .&K .*E .$C and @X @$C, each checked for a name first
          (and ('#{. at-op} op) (= 1 (count args)) (do (name-required! (first args)) true) (constant-name? (first args))
               (or (= '. op) (symbol? (first args)) (= '$ (first (first args)))))
          (do (fold (first args)) [true (eval! n)])
          ;; a constant pattern captured into a constant name
          (and ('#{. $} op) (= 2 (count args)))
          (let [p (fold (first args))]
            (name-required! (second args))
            (let [t (fold (second args))]
              (if (and (first p) (constant-name? (second args))) [true (eval! n)] [false nil])))
          :else
          (let [parts (vec (map-indexed (fn [i a] (when (names i) (name-required! a)) (fold a)) args))]
            (if (and (every? first parts)
                     ;; unary / is no operator of SPITBOL's until OPSYN makes it one, so it is not folded and its 029 comes as it runs (MEASURED on sbl -bf:
                     ;; X = /'a' writes the line before it and ends at ERROR 029; OPSYN('/', .U, 1) then /'a' calls U; corpus gate_cases_snobol4
                     ;; a_unary_and_a_binary_opsyn_of_one_operator_are_two_functions__ops, which this engine refused at compile time)
                     (or (and (folded-operators op) (<= 1 (count args) 2) (not (and (= '/ op) (= 1 (count args)))))
                         (and (= '| op) (<= 2 (count args)))
                         (and (folded-functions op) (not (:subscript (meta n))))))
              (let [v (eval! n)] (if (nil? v) [false nil] [true v]))
              [false nil]))))
      :else [false nil])))

(defn fold-error
  "The number of the error folding the statement body raises, then its goto's operands, or nil."
  ([body] (fold-error body nil))
  ([body goto]
   (try (fold body)
        (doseq [k [:S :F :G] :let [t (get goto k)] :when (and (some? t) (not (keyword? t)) (not (and (list? t) (= 'DIRECT (first t)))))]
          (name-required! t)
          (fold t))
        nil
        (catch clojure.lang.ExceptionInfo e (:snobol/error (ex-data e))))))

;; A pattern variable that an expression of constants reads is the pattern it holds from the start, whatever the program has put in it since: SPITBOL's compiler
;; evaluates such an expression as it compiles (svpre, above), while a variable read alone is read as the program runs (sbl -bf: after 'abc' ? LEN(1) . REM,
;; DATATYPE(REM) is PATTERN and X = REM gives a STRING; in a program's DEFINEd ABORT, whose call sets ABORT to the null string, DATATYPE(ABORT) is PATTERN and
;; $'ABORT' the null string). Each such read becomes (pattern-constant V), which EVAL! reads as the pattern; a name position keeps its name (.ARB), and a
;; deferred *E is no constant here, only what folds within it.
(defn freeze
  "The IR n with each pattern variable read by an expression of constants marked (pattern-constant V)."
  [n]
  (letfn [(mark [n]
            (cond (and (symbol? n) (SNOBOL4clojure.env/protected-variables n)) (list 'pattern-constant n)
                  (vector? n) (with-meta (mapv mark n) (meta n))
                  (list? n)   (let [[op & args] n names (name-positions op args)]
                                (with-meta (apply list op (map-indexed (fn [i a] (if (names i) a (mark a))) args)) (meta n)))
                  :else n))
          (walk [n]
            (cond (and (list? n) (= '* (first n)) (= 2 (count n))) (with-meta (list '* (walk (second n))) (meta n))
                  (and (or (list? n) (vector? n)) (first (try (fold n) (catch clojure.lang.ExceptionInfo _ [false nil])))) (mark n)
                  (vector? n) (with-meta (mapv walk n) (meta n))
                  (list? n)   (with-meta (apply list (first n) (map walk (rest n))) (meta n))
                  :else n))]
    ;; most statements read no pattern variable, and are left as they are
    (if (some #(and (symbol? %) (SNOBOL4clojure.env/protected-variables %)) (tree-seq coll? seq n)) (walk n) n)))

;; The labels SPITBOL defines before the program's first statement (manual ch. 16, Special names).
(def ^:private system-labels #{:ABORT :CONTINUE :SCONTINUE :RETURN :FRETURN :NRETURN})

;; -- CODE! — parse source into {CODES NOS LABELS} ----------------------------
(defn CODE!
  "Compile SNOBOL4 source S into a [codes nos labels disp] vector. disp holds, for each statement in order, how many blank lines (statements SPITBOL
   numbers but never runs) came before it, and the total, as {:before [..] :total n}, and where it stands in its file, as {:pos [[file line] ..]}.
   Optional include-path is a seq of directory strings used to resolve
   -INCLUDE directives (default [\".\"]). file is the name the program was given, which &FILE reads."
  ([S] (CODE! S ["."]))
  ([S include-path] (CODE! S include-path nil))
  ([S include-path file] (CODE! S include-path file false))
  ([S include-path file record?]
   (let [;; a CODE text (CODE-block passes record? true) may define a label again; the program's own compile may not (217, below)
         code-text? record?
         ;; a record with a line end in it is read as it stands: the -INCLUDE preprocessor reads lines, and would end them
         record?   (and record? (boolean (re-find #"[\r\n]" (str S))))
         ;; a first line that begins #! is no line of the program: it is not listed, and the lines and statements count from the one after it; a #! line
         ;; after the first is an error (MEASURED on sbl -bf: after "#! /usr/local/bin/snobol4 -b", OUTPUT = &LINE prints 1 and an error on the file's third
         ;; line is named (2), the listing starting at the line after; corpus tkslide_examples take_note, whose 022 sbl names at line 8, the file's 9)
         S         (if (and (not record?) (string/starts-with? (str S) "#!")) (let [k (string/index-of (str S) "\n")] (if k (subs (str S) (inc k)) "")) S)
         [expanded _ cards] (if record? [(str S) #{} []] (preprocess-includes* (str S) include-path #{}))
         raw-stmts (split-source expanded record?)
         _         (reset! <DIAGNOSTICS> (:diagnostics (meta raw-stmts)))
         _         (reset! <SOURCE> {:text expanded :stmts raw-stmts :cards cards})]
     ;; snobol4jvm never folds case (Lon 2026-11-17 18:2x CST, in the JVM window, verbatim: "We WILL NEVER support case-folding. We will always have upper-case built-in keywords and functions."): fold stays false, so a -CASE card is read as a card and folds nothing, and a name is
     ;; compiled as the program spells it whatever &CASE holds (sbl -bf folds after -CASE 1; such an entry is declared for this engine in EXCLUDED.jvm.tsv)
     ;; where each statement begins in its line, so a diagnostic's column counts from the line's start as sbl -bf counts it (MEASURED: "        X = ) ; Y = ( ; Z = 1 +"
     ;; is 224 at column 13 and 226 at 23; this engine said 6 for the second, counted from its statement); the line's text read past the statements already placed
     (let [src-lines (string/split (str expanded) #"\n") placed (atom {})
           offset (fn [pline ^String stmt]
                    (let [line (get src-lines (dec (long pline))) from (get @placed pline 0)
                          ;; a statement continued past its line stands in it as far as the line goes: the line's tail from k begins the statement
                          k    (when (and line (string? stmt))
                                 (or (string/index-of line stmt from)
                                     (first (filter #(string/starts-with? stmt (subs line %)) (range from (count line))))))]
                      (if k (do (swap! placed assoc pline (+ (long k) (count stmt))) k) 0)))]
     (loop [stmts raw-stmts NO 1 CODES {} NOS {} LABELS {} blanks 0 before [] fold false
            poss [] pos {:file (str file) :bp 1 :bl 1 :stack []}]
       (if (empty? stmts)
         [CODES NOS LABELS {:before before :total blanks :pos poss}]
         (let [[command pline] (first stmts)]
           (cond
             (= command :blank) (recur (rest stmts) NO CODES NOS LABELS (inc blanks) before fold poss pos)
             (comment? command) (recur (rest stmts) NO CODES NOS LABELS blanks before fold poss pos)
             (control? command) (recur (rest stmts) NO CODES NOS LABELS blanks before
                                       (do (case-card fold command)
                                           ;; an -INCLUDE card the preprocessor left standing names a file it could not open (preprocess-includes*)
                                           (when-let [q (and (re-find #"^-INCLUDE\s+['\"]" command) (re-find #"['\"]" command))]
                                             (swap! <DIAGNOSTICS> conj [pline 285 (inc (long (string/index-of command q))) NO]))
                                           fold) poss
                                       (position-card fold command pline pos))
             :else
             (let [stmt   (cond-> command fold fold-case)   ; a carriage return stays, a character of its statement (split-source)
                   _      (SNOBOL4clojure.env/note-names! (source-names stmt))
                   ;; the END statement's operand is its entry label: nothing, or a label the program defines; anything else is "215 syntax error: undefined or
                   ;; erroneous entry label" at the operand, and the END stands (manual ch. 14, The END Statement; MEASURED on sbl -bf: "END     NOPE",
                   ;; "END     :(NOWHERE)" and "END     X = (" are 215 at column 9, where this engine ran the first two and read the third as 226 with no END;
                   ;; corpus tests/snobol4 compile_diag__p30, p31)
                   [_ e-kw e-sp e-op] (re-find #"^(END|end)([ \t]+)(\S.*?)[ \t]*$" stmt)
                   stmt   (if (and e-op (not (and (re-matches #"[A-Za-z0-9][A-Za-z0-9._]*" e-op) (contains? NOS (keyword e-op)))))
                            (do (swap! <DIAGNOSTICS> conj [pline 215 (inc (count (str e-kw e-sp))) NO]) e-kw)
                            stmt)
                   ;; a line end outside a literal in a record does not compile: 214 where it begins the label field, 230 elsewhere (sbl -bf: CODE(' OUTPUT = 1'
                   ;; CHAR(10) ' OUTPUT = 2') and CODE(' OUTPUT = 3' CHAR(13) ' OUTPUT = 4') are 230, CODE(' OUTPUT = 6;' CHAR(10)) 214)
                   ast    (if (and record? (line-end-outside-literals? stmt)) ::line-end (parse-statement stmt))
                   ;; a compile error the emitter finds (an invalid numeric item) is the statement's error, as a parse failure is
                   code   (if (= ast ::line-end)
                            {:reason :line-end :line pline :column 1}
                            (try (emitter ast)
                                 (catch clojure.lang.ExceptionInfo e
                                   (if (:snobol/compile (ex-data e)) {:reason :compile :line pline :column 1 :text (ex-message e)} (throw e)))))
                   here   [(:file pos) (+ (:bl pos) (- pline (:bp pos)))]
                   pos''  (if (:revert pos) (assoc pos :file (:revert pos) :revert nil) pos)]
               (if (and (map? code) (:reason code))
                 ;; a statement that does not compile is the error SPITBOL's scan names, at its line and column (syntax.clj); 220 when the scan finds none. A line
                 ;; that starts with a continuation mark is split-source's to judge (214, or a continuation sbl takes after a blank line)
                 (let [{n :error col :column} (or (when-not (= :compile (:reason code)) (syntax-error stmt))
                                                  ;; a 231 the emitter finds stands where the scan puts it, the numeric item's start
                                                  (when (= :compile (:reason code)) (let [se (syntax-error stmt)] (when (= 231 (:error se)) se)))
                                                  {:error (case (:reason code) :compile 231 :line-end 230 220)})]
                   ;; the statement's label is read before its body, so a statement that does not compile still defines its label, and a duplicate one is 217
                   ;; first (MEASURED on sbl -bf: "A B\\C = 1" then "A" is 230 at (1,4) and "(2) : ERROR 217 -- syntax error: duplicate label"; corpus
                   ;; tests/snobol4 compile_diag__p33, where this engine defined no A at line 1 and took line 2)
                   (let [lbl  (when-not (#{\+ \.} (first stmt)) (second (re-find #"^([A-Za-z0-9][^ \t;]*)" stmt)))
                         lkey (when lbl (keyword lbl))
                         dup? (and lkey (not code-text?) (or (contains? NOS lkey) (system-labels lkey)))]
                     (when-not (#{\+ \.} (first stmt))
                       ;; 214, a bad label or a misplaced continuation, names no column (MEASURED on sbl -bf: "X = 1 ;# fails" is "(5) : ERROR 214" with an empty line
                       ;; for its mark, where this engine said (5,60); corpus rosetta-snobol4 string-matching)
                       ;; a column past the end of the statement's first line falls on a continuation line, which the statement joins one for one (its mark a blank):
                       ;; the error names that line and the column within it, as sbl -bf names them (MEASURED: "        X = F(" then "+ 'a'" and "+ 'b'" is 226 at
                       ;; (3,3), and "+ 'b' )" as the second line 224 at (2,7); corpus setl4_spitbol sonnets-1, a 226 at (229,3) that this engine put at (225,303))
                       (let [at (fn [c] (loop [l (long pline) c (long c)]
                                          (let [len (count (get src-lines (dec l) "")) nxt (get src-lines l)]
                                            (if (and (> c len) nxt (#{\+ \.} (first nxt))) (recur (inc l) (- c len)) [l c]))))
                             [dl dc] (if (and col (not= 214 n) (not dup?))
                                       (at (+ (long (offset pline command)) (min (long col) (max 1 (count stmt)))))
                                       [pline nil])]
                         (swap! <DIAGNOSTICS> conj (if dup? [pline 217 nil NO] [dl n dc NO]))))
                     (recur (rest stmts) (inc NO) (assoc CODES NO (error-node code)) (if (and lkey (not dup?)) (assoc NOS lkey NO) NOS)
                            (if (and lkey (not dup?)) (assoc LABELS NO lkey) LABELS) blanks (conj before blanks) fold (conj poss here) pos'')))
                 (let [label  (:label code)
                       body   (:body  code)
                       goto   (:goto  code)
                       ;; a label the program's compile has defined already, or a system label, is "217 syntax error: duplicate label"; the first definition
                       ;; stands (sbl.min cmp11, vrlbl not stndl outside a CODE call; sbl -bf: a second L, or a label ABORT, CONTINUE, SCONTINUE, RETURN, FRETURN
                       ;; or NRETURN is 217, and a CODE text may define L again)
                       dup?   (and (not code-text?) (keyword? label) (or (contains? NOS label) (system-labels label)))
                       ferr   (if dup? 217 (fold-error body (when (map? goto) goto)))
                       ;; an error the folding finds names no column and stands at the statement's last line, the one SPITBOL's compiler has read when it folds,
                       ;; and that line is the one listed (MEASURED on sbl -bf: "        X = (1 / 0)" then "+ 'abc'" and "+ 'def'" is "(3) : ERROR 014", listed
                       ;; "1       + 'def'"; "X = 'a'" then "+ 'c' (1 / 0)" is (2); corpus decus_vax84a atn, a 015 folded from 'prep ' ! 'adj ' on its line 35, which this
                       ;; engine put at 34); a duplicate label is the first line's
                       _      (when ferr
                                (let [last-line (loop [l (long pline) c (+ (long (offset pline command)) (count stmt))]
                                                  (let [len (count (get src-lines (dec l) "")) nxt (get src-lines l)]
                                                    (if (and (> c len) nxt (#{\+ \.} (first nxt))) (recur (inc l) (- c len)) l)))]
                                  (swap! <DIAGNOSTICS> conj [(if (= 217 ferr) pline last-line) ferr nil NO])))
                       body   (if ferr body (freeze body))
                       key    (if (and label (not dup?)) label NO)
                       code   (reduce #(conj %1 %2) [] [body goto])
                       nos    (if (keyword? key) (assoc NOS   key NO) NOS)
                       labels (if (keyword? key) (assoc LABELS NO key) LABELS)
                       codes  (assoc CODES key code)]
                   ;; "After reading the END statement, compilation ends" (manual ch. 14, The END Statement): nothing after it is compiled, so the data a program keeps there
                   ;; is no statement, and no error (sbl -bf; snoflake eliza-duquet-original keeps its script after END)
                   (if (#{:END :end} label)
                     [codes nos labels {:before (conj before blanks) :total blanks :pos (conj poss here)}]
                     (recur (rest stmts) (inc NO) codes nos labels blanks (conj before blanks) fold (conj poss here) pos'')))))))))))))

;; "If we place a statement label in the subject field of the END statement, execution will begin there" (manual ch. 14, The END Statement): END START starts the
;; program at START. The label END is recognised in either case.
(defn- end-entry [codes]
  (let [b (first (or (get codes :END) (get codes :end)))]
    (when (symbol? b)
      (let [k (keyword (name b))] (when (@<STNO> k) k)))))

;; -- CODE — compile and load into the global statement store ------------------
(declare load-code)
(defn CODE
  "Compile S and load into the global statement store.
   Optional include-path is a seq of directory strings (default [\".\"])."
  ([S] (CODE S ["."]))
  ([S include-path] (CODE S include-path nil))
  ([S include-path file]
   (let [[_ _ _ disp :as ir] (CODE! S include-path file)]
     ;; the program's last line read, its END's (sbl -bf reads no further: data after END is no line), is where CODE's texts number on from
     (reset! <CODELINE> (or (last (:pos disp)) ["" 0]))
     (load-code ir))))

(defn- load-code
  "Load a CODE! result into the global statement store; returns the entry, as CODE does."
  [[codes nos labels disp]]
   (let [start              (inc @STNO)
         base               @BLANKS]
     (loop [NO 1]
       (if (> NO (count codes))
         (do
           (swap! BLANKS + (:total disp 0))
           (or (end-entry codes)
               (if (and (@<LABL> start) (@<CODE> (@<LABL> start)))
                 (@<LABL> start)
                 (when (@<CODE> start) start))))
         (do
           (swap! STNO inc)
           (swap! <DISPNO> assoc @STNO (+ @STNO base (get (:before disp) (dec NO) 0)))
           (swap! <POS> assoc @STNO (get (:pos disp) (dec NO) ["" 0]))
           (if-let [label (labels NO)]
             (do
               (swap! <CODE> #(assoc % label (codes label)))
               (swap! <LABL> #(assoc % @STNO label))
               (swap! <STNO> #(assoc % label @STNO)))
             (swap! <CODE> #(assoc % @STNO (codes NO))))
           (recur (inc NO)))))))

;; "If we had not done this, SPITBOL would terminate when execution 'fell out of the bottom' of the new code block" (manual ch. 9, The CODE function): the block
;; CODE() compiles at run time ends in a statement of its own, numbered after its last, which ends the program when control reaches it (env/block-end). A block
;; with no statements, CODE('') or a comment alone, is that statement, and starts there. A block that ends in its own END has none: its END is its last statement,
;; and the next block numbers on from it (sbl -bf: after a seven-statement program CODE('END') is statement 8 and the next block's first 9; CODE(' OUTPUT = 1;END')
;; is 11 and 12, the next block's first 13).
;; "The string s must contain valid SPITBOL program statements complete with labels and using semicolons to separate statements" (manual ch. 19, CODE): the text
;; is one record, split-source's record?.
;; "The call fails if syntactical errors are found. In this case, it is possible to inspect &ERRTEXT to find the error message" (manual ch. 19, CODE): a block with
;; a statement that does not compile answers its first error as {:error n}. What SPITBOL compiled before that statement stays: those statements are loaded, their
;; labels defined, and the statement in error after them is error 7 when control reaches it; it takes no number of its own, so the next block numbers on from the
;; last statement loaded (sbl -bf: CODE(' X = 1;L1 OUTPUT = "in L1 " &STNO;L2 Y = SQRT(-0.5);L3 OUTPUT = "L3"') fails with 314, and :(L1) prints in L1 10, the
;; program having eight statements, then reaches error 7; L2 and L3 are undefined, or keep the statements an earlier block gave them; the next block's first is 11).
;; Not followed: the next block's first statement takes the place of the one in error, so control falling from the last statement loaded reaches it.
;; Each text CODE compiles is one more line of the program's file, after the last line the compiler read, whether it compiles or not; &FILE there is the program's
;; (sbl -bf: an eleven-line program's CODE texts are lines 12, 13 and 14, a failed one among them, and a text of two statements is one line; EVAL takes none). A
;; statement an -INCLUDE in the text brings keeps its own file and line.
(defn CODE-block [S]
  (let [[f l]          (swap! <CODELINE> (fn [[f l]] [f (inc (long l))]))
        [codes :as ir] (update-in (CODE! S ["."] nil true) [3 :pos] (fn [ps] (mapv #(if (= "" (first %)) [f l] %) ps)))]
    (if-let [[_ n _ bad] (first @<DIAGNOSTICS>)]
      (let [[codes nos labels disp] ir
            k      (dec (long (or bad 1)))
            before (fn [m] (into {} (filter (fn [[no _]] (<= (long no) k)) m)))]
        (when (pos? k)
          (load-code [(into {} (for [no (range 1 (inc k)) :let [key (or (labels no) no)]] [key (codes key)]))
                      (into {} (filter (fn [[_ no]] (<= (long no) k)) nos))
                      (before labels)
                      (-> disp (update :before #(vec (take k %))) (update :pos #(vec (take k %))) (assoc :total (get (:before disp) k (:total disp))))]))
        (swap! <CODE> assoc (inc (long @STNO)) compile-error)
        {:error n})
      (let [entry (load-code ir)]
        (if (or (contains? codes :END) (contains? codes :end))
          entry
          (do (swap! STNO inc)
              (swap! <DISPNO> assoc @STNO (+ @STNO @BLANKS))
              (swap! <CODE> assoc @STNO block-end)
              (or entry @STNO)))))))

;; -- Stage 23A: EDN serialisation --------------------------------------------

(defn ir->edn
  "Serialise a CODE! result triple [codes nos labels] to a compact EDN string.
   The IR is pure Clojure data so pr-str is lossless."
  [[codes nos labels disp]]
  (pr-str {:codes codes :nos nos :labels labels :disp disp}))

(defn edn->ir
  "Deserialise an EDN string produced by ir->edn back to [codes nos labels].
   Uses clojure.edn/read-string — safe, no eval."
  [s]
  (let [{:keys [codes nos labels disp]} (edn/read-string s)]
    [codes nos labels disp]))

;; -- Stage 23A: CODE-ir — load pre-compiled IR into statement store -----------

(defn CODE-ir
  "Like CODE, but takes a pre-compiled [codes nos labels] triple.
   Skips grammar and emitter entirely. ~10x faster than CODE for short programs."
  [[codes nos labels disp]]
  (let [start (inc @STNO)
        base  @BLANKS]
    (loop [NO 1]
      (if (> NO (count codes))
        (do
          (swap! BLANKS + (:total disp 0))
          (if (and (@<LABL> start) (@<CODE> (@<LABL> start)))
            (@<LABL> start)
            (when (@<CODE> start) start)))
        (do
          (swap! STNO inc)
          (swap! <DISPNO> assoc @STNO (+ @STNO base (get (:before disp) (dec NO) 0)))
          (swap! <POS> assoc @STNO (get (:pos disp) (dec NO) ["" 0]))
          (if-let [label (labels NO)]
            (do
              (swap! <CODE> #(assoc % label (codes label)))
              (swap! <LABL> #(assoc % @STNO label))
              (swap! <STNO> #(assoc % label @STNO)))
            (swap! <CODE> #(assoc % @STNO (codes NO))))
          (recur (inc NO)))))))

;; -- Stage 23A: Disk cache ---------------------------------------------------

(defn compile-to-file
  "Compile SNOBOL4 source src and write the IR as EDN to path.
   Optional include-path is a seq of directory strings (default [\".\"])
   Returns the [codes nos labels] triple."
  ([src path] (compile-to-file src path ["."]))
  ([src path include-path]
   (let [ir (CODE! src include-path)]
     (clojure.java.io/make-parents path)
     (spit path (ir->edn ir))
     ir)))

(defn load-ir
  "Load a pre-compiled [codes nos labels] triple from an EDN file at path."
  [path]
  (edn->ir (slurp path)))

(defn CODE-cached
  "Compile src and load into the statement store, using a disk EDN cache.
   If path exists: load from disk (skip grammar + emitter).
   If path absent: compile src, save to path, then load.
   Optional include-path is a seq of directory strings (default [\".\"])."
  ([src path] (CODE-cached src path ["."]))
  ([src path include-path]
   (let [ir (if (.exists (clojure.java.io/file path))
               (load-ir path)
               (compile-to-file src path include-path))]
     (CODE-ir ir))))

;; -- Stage 23A: In-memory memoised cache -------------------------------------
;;
;; Fastest for test suites: compile each unique source string once per JVM
;; session.  The lein test suite compiles the same programs on every run;
;; memoisation skips the grammar after the first pass.

(def ^:private memo-cache
  (atom {:cache {} :hits 0 :misses 0}))

(defn CODE-memo
  "Memoised CODE: compile src exactly once per JVM session, then reuse the IR.
   Thread-safe. Drop-in replacement for CODE in test fixtures and harness.
   Optional include-path is a seq of directory strings (default [\".\"])

   Side effects (loading into <CODE>/<STNO>/<LABL>) still happen on every call
   so that reset-runtime! between tests works correctly."
  ([src] (CODE-memo src ["."]))
  ([src include-path]
   ;; Cache key includes include-path so different paths compile separately
   (let [cache-key [src include-path]
         cached    (get-in @memo-cache [:cache cache-key])]
     (if cached
       (do (swap! memo-cache update :hits inc)
           (CODE-ir cached))
       (do (swap! memo-cache update :misses inc)
           (let [ir (CODE! src include-path)]
             (swap! memo-cache update :cache assoc cache-key ir)
             (CODE-ir ir)))))))

(defn clear-memo!
  "Evict all entries from the in-memory IR cache."
  []
  (swap! memo-cache assoc :cache {} :hits 0 :misses 0)
  nil)

(defn memo-stats
  "Return cache statistics: {:size N :hits N :misses N :hit-rate pct}."
  []
  (let [{:keys [cache hits misses]} @memo-cache
        total (+ hits misses)]
    {:size     (count cache)
     :hits     hits
     :misses   misses
     :hit-rate (if (pos? total)
                 (format "%.1f%%" (* 100.0 (/ hits total)))
                 "n/a")}))

;; -- Stage 23A: Corpus pre-compiler ------------------------------------------

(defn precompile-corpus!
  "Pre-compile a sequence of source strings to EDN files under cache-dir.
   Each file is named by the hash of the source string.
   Skips files that already exist (idempotent).
   Returns {:compiled N :skipped N :errors N}.

   Example:
     (require '[SNOBOL4clojure.harness :as gen])
     (precompile-corpus! (gen/gen-by-length) \"resources/ir-cache\")"
  [sources cache-dir]
  (clojure.java.io/make-parents (str cache-dir "/x"))
  (reduce
    (fn [acc src]
      (let [path   (str cache-dir "/" (format "%x" (Math/abs (hash src))) ".ir.edn")
            exists (.exists (clojure.java.io/file path))]
        (if exists
          (update acc :skipped inc)
          (try
            (compile-to-file src path)
            (update acc :compiled inc)
            (catch Exception _
              (update acc :errors inc))))))
    {:compiled 0 :skipped 0 :errors 0}
    sources))
