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
        j      (if (= \. (at s j)) (digits (inc j)) j)
        j      (if (#{\e \E \d \D} (at s j))
                 (let [k (if (#{\+ \-} (at s (inc j))) (+ j 2) (inc j))]
                   (if (digit? (at s k)) (digits k) (fail! 231 j)))
                 j)]
    (if (or (name-char? (at s j)) (= \- (at s j))) (fail! 231 j) j)))

;; The list inside ( ) or < > or [ ], its elements separated by commas, any of them empty. Returns the index after the closer. A colon or a semicolon ends the
;; scan inside it as the end does (sbl -bf: (4 :(L)) and F(4 :(L)) are "226 missing right paren", A<4 :(L)> and A[4 :(L)] "229 missing right array bracket").
(defn- element-list [s i closer]
  (loop [i i]
    (let [i (expression s i {:closer closer})
          c (at s i)]
      (cond
        (= c \,)                              (recur (inc i))
        (= c closer)                          (inc i)
        (or (nil? c) (#{\: \;} c))            (fail! (if (= closer \)) 226 229) i)
        (= c \))                              (fail! 224 i)
        (#{\] \>} c)                          (fail! 225 i)
        :else                                 (fail! 220 i)))))

;; An operand: unary operators, then a primary, then its subscripts. Returns [index-after kind], kind :literal for a string or number standing alone, :name for a
;; variable, and :value for anything else.
(defn- operand [s i]
  (let [c (at s i)]
    (cond
      (nil? c)                         (fail! 221 i)
      (and (operator-char? c) (let [d (at s (inc i))] (or (nil? d) (blank? d) (#{\) \] \> \, \: \;} d))))
      (fail! 221 (inc i))
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
      (loop [[j kind] (operand s i) n 1]
        (let [c (at s j)]
          ;; right after an operand comes a blank, the end, a closer, a comma or a colon: another operand there lacks its operator, and an operator there lacks
          ;; the blank a binary operator needs
          (cond
            (and (#{\) \] \> \,} c) (:top ctx))                       (stray! c j)
            (stop? c)                                                   j
            (or (letter? c) (digit? c) (#{\' \" \(} c))                (fail! 220 j)
            (operator-char? c)                                          (fail! 233 j)
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
                  (cond (and (= kind :literal) (= n 1))  (fail! 212 k)
                        (:stop-at-equals ctx)            k
                        :else                            (let [m (skip-blanks s (inc k))] (if (at s m) (expression s m (dissoc ctx :stop-at-equals)) m)))
                  (let [m (skip-blanks s (inc k))]
                    (cond
                      (or (nil? (at s m)) (#{\) \] \> \, \: \;} (at s m)))      (fail! 221 m)
                      ;; a goto's target is a name: an expression of more than one element is "212 value used where name is required"
                      (:goto ctx)                                                 (fail! 212 m)
                      :else                                                       (recur (operand s m) (inc n)))))
                (:goto ctx) (fail! 212 k)
                :else (recur (operand s k) (inc n))))))))))

;; The goto field after the colon: S(label), F(label), (label) or the same with < >, each at most once (sbl -bf: 218 duplicated, 219 empty, 227 and 228 unclosed,
;; 234 anything else).
(defn- goto-field [s i]
  (let [i0 (skip-blanks s i)]
    (when (nil? (at s i0)) (fail! 219 i0))
    (loop [i i0 seen #{}]
      (let [i (skip-blanks s i)
            c (at s i)]
        (when c
          (let [[kind j] (case c (\S \s) [:S (inc i)] (\F \f) [:F (inc i)] [:G i])
                o        (at s j)]
            (when-not (#{\( \<} o) (fail! 234 j))
            (when (or (seen kind) (and (= kind :G) (seq seen)) (seen :G)) (fail! 218 j))
            (let [closer (if (= o \() \) \>)
                  k      (skip-blanks s (inc j))
                  _      (when (= (at s k) closer) (fail! 212 k))
                  k      (try (expression s k {:closer closer :goto true})
                              (catch clojure.lang.ExceptionInfo e
                                (if (and (= 226 (::error (ex-data e))) (= closer \))) (fail! 227 (::at (ex-data e))) (throw e))))]
              (cond
                (= (at s k) closer) (let [n (inc k) d (at s n)]
                                      (when-not (or (nil? d) (blank? d) (#{\S \s \F \f \( \<} d)) (fail! 234 n))
                                      (recur n (conj seen kind)))
                ;; a colon or a semicolon ends the scan inside the goto too (sbl -bf: :(L:X) and :S(L :F(L)) are 227, :<L :> 228)
                (or (nil? (at s k)) (#{\: \;} (at s k))) (fail! (if (= closer \)) 227 228) k)
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
            (if (#{\) \] \> \,} (at s j)) (stray! (at s j) j) (fail! 220 j))
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
      (when-let [n (::error (ex-data e))] {:error n :column (inc (or (::at (ex-data e)) 0))}))))

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
