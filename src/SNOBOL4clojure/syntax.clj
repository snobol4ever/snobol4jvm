(ns SNOBOL4clojure.syntax
  "The compile error SPITBOL reports for a statement it cannot compile (manual Appendix D, errors 212 to 234), found by scanning the statement left to right as
   SPITBOL's own scanner does: the first error met is the one reported. The parser (grammar.clj) decides whether a statement compiles; this namespace names the
   error of one that does not, as sbl -bf names it.")

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
        (if (or (letter? d) (and (= c \.) (#{\$ \( \*} d)))
          [(first (operand s (inc i))) :value]
          (fail! 212 (inc i))))
      (operator-char? c)               [(first (operand s (inc i))) :value]
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
