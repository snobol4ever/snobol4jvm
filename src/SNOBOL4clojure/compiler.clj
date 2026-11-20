(ns SNOBOL4clojure.compiler
    (:require
        [instaparse.core :as insta :refer [defparser]]
        [clojure.edn :as edn]
        [clojure.string :as string]
        [SNOBOL4clojure.env :as env :refer [subtract parse-real STNO <STNO> <LABL> <CODE> <DISPNO> <POS> <CODELINE> <DIAGNOSTICS> <SOURCE> BLANKS &CASE fold-case block-end compile-error]]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defmacro ^:private compiled-rules [] (list 'quote (:grammar (insta/parser grammar))))
(def ^:private rules (compiled-rules))
(def parse-statement (insta/parser rules :start :stmt :total true))
(def parse-expression (insta/parser rules :start :expr))
(def ^:private operator-char? #{\+ \- \* \/ \! \^ \$ \. \& \@ \~ \? \% \# \| \=})
(defn- blank? [c] (or (= c \space) (= c \tab)))
(defn- letter? [c] (and c (or (<= (int \A) (int c) (int \Z)) (<= (int \a) (int c) (int \z)))))
(defn- digit? [c] (and c (<= (int \0) (int c) (int \9))))
(defn- name-char? [c] (or (letter? c) (digit? c) (= c \_) (= c \.)))
(defn- fail! [n i] (throw (ex-info "syntax" {::error n ::at i})))
(defn- stray! [c i] (fail! (case c \) 224 \, 223 225) i))
(defn- at [^String s i] (when (< i (count s)) (.charAt s i)))
(defn- skip-blanks [s i] (if (blank? (at s i)) (recur s (inc i)) i))
(declare expression)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- number-end
    [s i]
    (let
        [digits
            (fn [j] (if (digit? (at s j)) (recur (inc j)) j))
            j
            (digits i)
            _
            (when (and (not (#{\. \e \E \d \D} (at s j))) (> (- j i) 18) (try (Long/parseLong (subs s i j)) false (catch NumberFormatException _ true))) (fail! 231 i))
            j
            (if (= \. (at s j)) (digits (inc j)) j)
            j
            (if (#{\e \E \d \D} (at s j)) (let [k (if (#{\+ \-} (at s (inc j))) (+ j 2) (inc j))] (if (digit? (at s k)) (digits k) (fail! 231 i))) j)]
        (if (or (name-char? (at s j)) (= \- (at s j))) (fail! 231 i) j)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- last-token-start
    [^String s i]
    (let
        [j (loop [j (dec i)] (if (and (>= j 0) (blank? (at s j))) (recur (dec j)) j)) c (at s j) back (fn [k] (loop [k k] (if (and (pos? k) (name-char? (at s (dec k)))) (recur (dec k)) k)))]
        (cond
            (neg? j)
            i
            (#{\' \"} c)
            (let [k (.lastIndexOf s (str c) (int (dec j)))] (if (neg? k) j k))
            (and (#{\( \[ \<} c) (pos? j) (name-char? (at s (dec j))))
            (back (dec j))
            (name-char? c)
            (back j)
            :else j)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- element-list
    [s i closer]
    (loop
        [i i]
        (let
            [i (expression s i {:closer closer}) c (at s i)]
            (cond
                (= c \,)
                (recur (inc i))
                (= c closer)
                (inc i)
                (nil? c)
                (fail! (if (= closer \)) 226 229) (last-token-start s i))
                (#{\: \;} c)
                (fail! (if (= closer \)) 226 229) i)
                (= c \))
                (fail! 224 i)
                (#{\] \>} c)
                (fail! 225 i)
                :else
                (fail! 220 i)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare operand)
(defn- operand-after-unary [s i] (let [c (at s i)] (if (and (operator-char? c) (blank? (at s (inc i)))) (fail! 233 i) (operand s i))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- operand
    [s i]
    (let
        [c (at s i)]
        (cond
            (nil? c)
            (fail! 221 i)
            (and (operator-char? c) (let [d (at s (inc i))] (or (nil? d) (blank? d) (#{\) \] \> \, \: \;} d))))
            (fail! 221 (if (blank? (at s (inc i))) i (inc i)))
            (#{\. \&} c)
            (let
                [d (at s (inc i))]
                (cond
                    (or (letter? d) (and (= c \.) (#{\$ \( \*} d)))
                    [(first (operand s (inc i))) :value]
                    (and (operator-char? d) (blank? (at s (+ i 2))))
                    (fail! 233 (inc i))
                    :else
                    (fail! 212 (inc i))))
            (operator-char? c)
            [(first (operand-after-unary s (inc i))) :value]
            :else
            (let
                [[j kind]
                    (cond
                        (#{\' \"} c)
                        (let [k (.indexOf ^String s (str c) (int (inc i)))] (if (neg? k) (fail! 232 i) [(inc k) :literal]))
                        (digit? c)
                        [(number-end s i) :literal]
                        (letter? c)
                        (let [j (loop [j (inc i)] (if (name-char? (at s j)) (recur (inc j)) j))] (if (= \( (at s j)) [(element-list s (inc j) \)) :value] [j :name]))
                        (= c \()
                        [(element-list s (inc i) \)) :value]
                        (#{\< \[} c)
                        (fail! 222 i)
                        (= c \))
                        (fail! 224 i)
                        (#{\] \>} c)
                        (fail! 225 i)
                        (= c \,)
                        (fail! 223 i)
                        (#{\: \;} c)
                        (fail! 221 i)
                        :else
                        (fail! 230 i))]
                (loop [j j kind kind] (let [k (skip-blanks s j)] (case (at s k) \< (recur (element-list s (inc k) \>) :name) \[ (recur (element-list s (inc k) \]) :name) [j kind])))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- expression
    [s i ctx]
    (let
        [stop? (fn [c] (or (nil? c) (#{\: \;} c) (and (not (:top ctx)) (#{\) \] \> \,} c)))) i (skip-blanks s i) c (at s i)]
        (cond
            (and (:top ctx) (#{\) \] \> \,} c))
            (stray! c i)
            (stop? c)
            i :else
            (loop
                [[j kind] (operand s i) n 1 q 0 pre 0 st i]
                (let
                    [c (at s j)]
                    (cond
                        (and (#{\) \] \> \,} c) (:top ctx))
                        (stray! c j)
                        (stop? c)
                        j
                        (or (letter? c) (digit? c) (#{\' \" \(} c))
                        (fail! 220 j)
                        (operator-char? c)
                        (fail! 233 (if (#{\+ \-} c) st j))
                        (not (blank? c))
                        (fail! 230 j)
                        :else
                        (let
                            [k (skip-blanks s j) d (at s k) k (if (and (= d \*) (= \* (at s (inc k)))) (inc k) k) e (at s (inc k))]
                            (cond
                                (and (#{\) \] \> \,} d) (:top ctx))
                                (stray! d k)
                                (stop? d)
                                k
                                (and (operator-char? d) (or (nil? e) (blank? e)))
                                (if
                                    (= d \=)
                                    (cond
                                        (and (= kind :literal) (= n 1))
                                        (fail! 212 k)
                                        (and (:top ctx) (pos? q) (or (:stop-at-equals ctx) (> q 1) (> pre 1)))
                                        (fail! 212 :none)
                                        (:stop-at-equals ctx)
                                        k :else
                                        (let [m (skip-blanks s (inc k))] (if (at s m) (expression s m (dissoc ctx :stop-at-equals)) m)))
                                    (let
                                        [m (skip-blanks s (inc k))]
                                        (cond
                                            (or (nil? (at s m)) (#{\) \] \> \, \: \;} (at s m)))
                                            (fail! 221 m)
                                            (:goto ctx)
                                            (fail! 212 m)
                                            :else
                                            (recur (operand s m) (inc n) (if (= d \?) (inc q) q) (if (and (= d \?) (zero? q)) n pre) m))))
                                (:goto ctx)
                                (fail! 212 k)
                                :else
                                (recur (operand s k) (inc n) q pre k)))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- bad-goto! [s p] (fail! (cond (and (at s p) (< (int (at s p)) 32) (not= \tab (at s p))) 230 (and (operator-char? (at s p)) (pos? p) (not (blank? (at s (dec p))))) 233 :else 234) p))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- goto-field
    [s i]
    (let
        [i0 (skip-blanks s i)]
        (when (nil? (at s i0)) (fail! 219 i0))
        (loop
            [i i0 seen #{}]
            (let
                [i (skip-blanks s i) c (at s i)]
                (when c
                    (let
                        [[kind j] (case c (\S \s) [:S (inc i)] (\F \f) [:F (inc i)] [:G i]) o (at s j)]
                        (when-not (#{\( \< \[} o) (bad-goto! s j))
                        (let
                            [dup?
                                (or (seen kind) (and (= kind :G) (seq seen)) (seen :G))
                                closer
                                (case o \( \) \< \> \[ \])
                                k
                                (skip-blanks s (inc j))
                                _
                                (when (= (at s k) closer) (fail! (if dup? 218 212) k))
                                k
                                (try
                                    (expression s k {:closer closer :goto true})
                                    (catch clojure.lang.ExceptionInfo e (if (and (= 226 (::error (ex-data e))) (= closer \))) (fail! 227 (::at (ex-data e))) (throw e))))]
                            (cond
                                (= (at s k) closer)
                                (let [n (inc k) d (at s n)] (when dup? (fail! 218 k)) (when-not (or (nil? d) (blank? d) (#{\S \s \F \f \( \<} d)) (bad-goto! s n)) (recur n (conj seen kind)))
                                (#{\: \;} (at s k))
                                (fail! (if (= closer \)) 227 228) k)
                                (nil? (at s k))
                                (fail! (if (= closer \)) 227 228) (loop [b k] (if (and (pos? b) (name-char? (at s (dec b)))) (recur (dec b)) b)))
                                :else
                                (fail! 212 k)))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn syntax-error
    [^String s]
    (try
        (when (and (at s 0) (not (blank? (at s 0))) (not (letter? (at s 0))) (not (digit? (at s 0)))) (fail! 214 0))
        (let
            [i
                (if (and (seq s) (not (blank? (at s 0)))) (loop [i 0] (if (and (at s i) (not (blank? (at s i)))) (recur (inc i)) i)) 0)
                i
                (skip-blanks s i)
                replacement
                (fn [k] (let [m (expression s (inc k) {:top true})] (when (at s m) (goto-field s (inc m)))))]
            (cond
                (nil? (at s i))
                nil
                (= \: (at s i))
                (goto-field s (inc i))
                :else
                (let
                    [[j kind] (operand s i) k (skip-blanks s j) e (at s (inc k))]
                    (cond
                        (and (= \= (at s k)) (or (nil? e) (blank? e)))
                        (do (when (= kind :literal) (fail! 212 k)) (replacement k))
                        (and (= j k) (at s j) (not= \: (at s j)))
                        (let
                            [c (at s j)]
                            (cond
                                (#{\) \] \> \,} c)
                                (stray! c j)
                                (not (or (operator-char? c) (letter? c) (digit? c) (#{\' \" \( \< \[ \= \;} c)))
                                (fail! 230 j)
                                (operator-char? c)
                                (fail! 233 (if (#{\+ \-} c) i j))
                                :else
                                (fail! 220 j)))
                        :else
                        (let
                            [k (if (and (= \? (at s k)) (or (nil? e) (blank? e))) (inc k) k) m (expression s k {:top true :stop-at-equals true})]
                            (cond (nil? (at s m)) nil (= \: (at s m)) (goto-field s (inc m)) (= \= (at s m)) (replacement m) :else (fail! 220 m)))))))
        (catch clojure.lang.ExceptionInfo e (when-let [n (::error (ex-data e))] (if (= :none (::at (ex-data e))) {:error n} {:error n :column (inc (or (::at (ex-data e)) 0))})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn expression-end [^String s] (try {:end (expression s 0 {:top true})} (catch clojure.lang.ExceptionInfo e (when-let [n (::error (ex-data e))] {:error n}))))
(defn ends-in-mark? [^String s] (let [n (.length s)] (and (pos? n) (contains? #{\: \;} (.charAt s (dec n))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn top-replacement
    [r]
    (if
        (and (seq? r) (= '= (first r)) (= 3 (count r)) (not (:paren (meta r))))
        (let [[_ t v] r v (top-replacement v)] (if (and (vector? t) (not (:paren (meta t)))) (list '?= (first t) (if (= 2 (count t)) (second t) (vec (rest t))) v) (list '= t v)))
        r))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn emitter
    [ast]
    (insta/transform
        {:comment
            (fn [cmt] [:comment cmt])
            :control
            (fn [ctl] [:control ctl])
            :stmt
            (fn [& ss] (apply conj ss))
            :label
            (fn [L] {:label (keyword L)})
            :body
            (fn [B] {:body B})
            :invoking
            (fn [S] S)
            :matching
            (fn [S P & qs] (reduce (fn [m q] (list '? m q)) (list '? S P) qs))
            :qmatch
            (fn [P] P)
            :replacing
            (fn ([S P] (list '?= S P 'epsilon)) ([S P R] (list '?= S P (top-replacement R))))
            :assigning
            (fn ([S] (list '= S 'epsilon)) ([S R] (list '= S (top-replacement R))))
            :grp
            (fn [x] (if (instance? clojure.lang.IObj x) (vary-meta x assoc :paren true) x))
            :goto
            (fn [& gs] {:goto (reduce (fn [bs b] (let [key (first b) tgt (second b)] (assoc bs key (if (symbol? tgt) (keyword tgt) tgt)))) {} gs)})
            :jmp
            (fn [L] [:G L])
            :sjmp
            (fn [L] [:S L])
            :fjmp
            (fn [L] [:F L])
            :expr
            (fn [x] x)
            :nulasn
            (fn [] "")
            :asn
            (fn ([x] x) ([x y] (if (and (seq? x) (= '? (first x)) (= 3 (count x))) (list '?= (second x) (nth x 2) y) (list '= x y))))
            :mch
            (fn ([x] x) ([x y] (list '? x y)) ([x y z] (list '?= x y z)))
            :and
            (fn ([x] x) ([x y] (list '& x y)))
            :alt
            (fn ([x] x) ([x & ys] (apply list '| x ys)))
            :xalt
            (fn [x & ys] (apply list '| x ys))
            :xand
            (fn [x y] (list '& x y))
            :cat
            (fn ([x] x) ([x & ys] (apply vector x ys)))
            :at
            (fn ([x] x) ([x y] (list 'at-op x y)))
            :sum
            (fn ([x] x) ([x op y] (list (symbol op) x y)))
            :hsh
            (fn ([x] x) ([x y] (list 'sharp-op x y)))
            :div
            (fn ([x] x) ([x y] (list '/ x y)))
            :mul
            (fn ([x] x) ([x y] (list '* x y)))
            :pct
            (fn ([x] x) ([x y] (list '% x y)))
            :xp
            (fn ([x] x) ([x op y] (list (symbol (if (#{"^" "!"} op) "**" op)) x y)))
            :cap
            (fn ([x] x) ([x op y] (list (symbol op) x y)))
            :ttl
            (fn ([x] x) ([x y] (list 'tilde-op x y)))
            :uop
            (fn ([x] x) ([op y] (case op "@" (list 'at-op y) "#" (list 'sharp-op y) "~" (list 'tilde-op y) (list (symbol op) y))))
            :ndx
            (fn ([n] n) ([n & xs] (with-meta (apply list n xs) {:subscript true})))
            :cnd
            (fn ([x] x) ([x & ys] (apply vector :comma x ys)))
            :inv
            (fn [f & xs] (apply list f xs))
            :nul
            (fn [] "")
            :dgo
            (fn [x] (list 'DIRECT x))
            :N
            (fn [n] (symbol n))
            :S
            (fn [s] (subs s 1 (subtract (count s) 1)))
            :I
            (fn [s] (let [v (edn/read-string s)] (if (instance? Long v) v (throw (ex-info "syntax error: invalid numeric item" {:snobol/error 231 :snobol/compile true})))))
            :R
            (fn [s] (or (parse-real s) (throw (ex-info "syntax error: invalid numeric item" {:snobol/error 231 :snobol/compile true}))))}
        ast))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn parse+emit [s] (first (emitter (parse-expression s))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- source-names
    [^String s]
    (let
        [n (count s) name-char? (fn [c] (or (Character/isLetterOrDigit (char c)) (= c \_) (= c \.)))]
        (loop
            [i 0 goto? false out (transient (if (and (pos? n) (Character/isLetterOrDigit (.charAt s 0))) [(subs s 0 1)] []))]
            (if
                (>= i n)
                (persistent! out)
                (let
                    [c (.charAt s i)]
                    (cond
                        (or (= c \') (= c \"))
                        (let [k (.indexOf s (str c) (int (inc i)))] (recur (if (neg? k) n (inc k)) goto? out))
                        (= c \:)
                        (recur (inc i) true out)
                        (Character/isDigit c)
                        (recur (loop [j (inc i)] (if (and (< j n) (name-char? (.charAt s j))) (recur (inc j)) j)) goto? out)
                        (Character/isLetter c)
                        (let
                            [j (loop [j (inc i)] (if (and (< j n) (name-char? (.charAt s j))) (recur (inc j)) j)) t (subs s i j)]
                            (recur j goto? (if (and goto? (#{"S" "F"} t) (< j n) (#{\( \<} (.charAt s j))) out (conj! out t))))
                        :else
                        (recur (inc i) goto? out)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- comment? [cmd] (re-find #"^\*" cmd))
(defn- control? [cmd] (re-find #"^\-" cmd))
(defn- case-card [fold command] (when-let [[_ n] (re-find (if fold #"(?i)^-CASE(?:\s+(\d+))?\s*$" #"^-CASE(?:\s+(\d+))?\s*$") command)] (boolean (and n (not (zero? (Long/parseLong n)))))))
(defn- error-node [info] (list 'ERROR (:line info) (:column info) (:text info)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- open-parens
    [^String s]
    (loop
        [i 0 d 0 q nil]
        (if
            (>= i (count s))
            d
            (let
                [c (.charAt s i)]
                (cond q (recur (inc i) d (if (= c q) nil q)) (#{\' \"} c) (recur (inc i) d c) (= c \() (recur (inc i) (inc d) q) (= c \)) (recur (inc i) (dec d) q) :else (recur (inc i) d q))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- split-source
    ([src] (split-source src false))
    ([src record?]
        (let
            [chars
                (if (or record? (= "" (str src)) (string/ends-with? (str src) "\n")) (str src) (str src "\n"))
                n
                (count chars)
                trimr
                (if record? #(string/replace % #"[ \t]+\z" "") #(string/replace % #"[ \t\n]+\z" ""))
                blank-line?
                (fn [from to] (every? #(and (Character/isWhitespace ^char %) (not= \return %)) (subs chars from to)))]
            (loop
                [i 0 cur (StringBuilder.) stmts [] in-sq false in-dq false ls 0 line 1 sl 1 diags (if (and (pos? n) (#{\+ \.} (.charAt chars 0))) [[1 214]] [])]
                (if
                    (>= i n)
                    (let [s (trimr (str cur))] (with-meta (cond (seq s) (conj stmts [s sl]) (and record? (pos? (.length cur))) (conj stmts [:blank sl]) :else stmts) {:diagnostics diags}))
                    (let
                        [ch (.charAt chars i) sl (if (zero? (.length cur)) line sl)]
                        (cond
                            (and (#{\' \"} ch) (pos? (.length cur)) (not (Character/isWhitespace (.charAt cur 0))) (not (string/includes? (str cur) " ")) (not (string/includes? (str cur) "\t")))
                            (recur (inc i) (.append cur ch) stmts in-sq in-dq ls line sl diags)
                            (and (= ch \') (not in-dq))
                            (recur (inc i) (.append cur ch) stmts (not in-sq) in-dq ls line sl diags)
                            (and (= ch \") (not in-sq))
                            (recur (inc i) (.append cur ch) stmts in-sq (not in-dq) ls line sl diags)
                            (and
                                (= ch \;)
                                (not in-sq)
                                (not in-dq)
                                (not (and (pos? (.length cur)) (= \* (.charAt cur 0))))
                                (not (pos? (long (open-parens (let [t (str cur)] (if (and (seq t) (not (Character/isWhitespace (.charAt t 0)))) (subs t (or (string/index-of t " ") (count t))) t))))))
                                (not (and (pos? (.length cur)) (let [c (.charAt cur 0)] (not (or (Character/isLetterOrDigit c) (#{\space \tab \- \+ \. \( \[ \< \> \: \,} c)))))))
                            (let [s (trimr (str cur))] (recur (inc i) (StringBuilder.) (conj stmts (if (seq s) [s sl] [:blank sl])) false false ls line line diags))
                            (and (= ch \newline) (not record?))
                            (let
                                [s (trimr (str cur)) next-i (inc i) next-ch (when (< next-i n) (.charAt chars next-i)) cont? (and next-ch (or (= next-ch \+) (= next-ch \.)))]
                                (if
                                    (seq s)
                                    (cond
                                        (and cont? (= \* (.charAt s 0)))
                                        (recur (inc i) (StringBuilder.) (conj stmts [s sl]) false false (inc i) (inc line) (inc line) (conj diags [(inc line) 214]))
                                        cont?
                                        (recur (+ i 2) (.append cur \space) stmts false false (inc i) (inc line) sl diags)
                                        :else
                                        (recur (inc i) (StringBuilder.) (conj stmts [s sl]) false false (inc i) (inc line) (inc line) diags))
                                    (let
                                        [blank? (blank-line? ls i) tail? (and (pos? (.length cur)) (= i (dec n)) (not (string/ends-with? (str src) "\n")))]
                                        (recur
                                            (inc i)
                                            (StringBuilder.)
                                            (if (or blank? tail?) (conj stmts [:blank line]) stmts)
                                            false false
                                            (inc i)
                                            (inc line)
                                            (inc line)
                                            (if (and cont? (not blank?)) (conj diags [(inc line) 214]) diags)))))
                            :else
                            (recur (inc i) (.append cur ch) stmts in-sq in-dq ls line sl diags))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- line-end-outside-literals?
    [^String s]
    (loop
        [i 0 q nil]
        (when
            (< i (count s))
            (let [c (.charAt s i)] (cond (and q (= c q)) (recur (inc i) nil) q (recur (inc i) q) (#{\' \"} c) (recur (inc i) c) (#{\return \newline} c) true :else (recur (inc i) nil))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare preprocess-includes*)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn preprocess-includes
    ([src] (preprocess-includes src ["."]))
    ([src search-path] (preprocess-includes src search-path #{}))
    ([src search-path seen] (first (preprocess-includes* src search-path seen))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- preprocess-includes*
    [src search-path seen]
    (let
        [include-re #"^-INCLUDE\s+['\"]([^'\"]+)['\"]"]
        (loop
            [lines (string/split src #"\n") out [] seen seen cards [] ph 0]
            (if-let
                [line (first lines)]
                (if-let
                    [[_ fname] (re-find include-re line)]
                    (let
                        [found (some (fn [dir] (let [f (clojure.java.io/file dir (string/trim fname))] (when (.exists f) f))) search-path)]
                        (cond
                            (nil? found)
                            (recur (rest lines) (conj out line) seen cards (inc ph))
                            (seen fname)
                            (recur (rest lines) (conj out (str "* -INCLUDE skipped (already included): " fname)) seen cards ph)
                            :else
                            (let
                                [text
                                    (slurp found :encoding "ISO-8859-1")
                                    [body seen' inner]
                                    (preprocess-includes* text (distinct (cons (.getParent found) search-path)) (conj seen (string/trim fname)))
                                    last-line
                                    (let [ls (string/split text #"\r?\n" -1)] (last (if (and (next ls) (= "" (peek ls))) (pop ls) ls)))
                                    stale?
                                    (boolean (and last-line (or (string/starts-with? last-line "*") (re-find #";[ \t]*(\*[^'\"]*)?$" last-line))))]
                                (recur
                                    (rest lines)
                                    (into out (concat [(str "-FILE-ENTER \"" (string/trim fname) "\"")] (string/split body #"\n") [(str "-FILE-LEAVE" (when stale? " STALE"))]))
                                    seen'
                                    (into (conj cards line) inner)
                                    ph))))
                    (if (and (pos? ph) (re-find #"^(END|end)([ \t;]|$)" line)) (recur (rest lines) (conj out "-DEPTH-LEAVE") seen cards (dec ph)) (recur (rest lines) (conj out line) seen cards ph)))
                [(string/join "\n" out) seen cards]))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- position-card
    [fold command pline pos]
    (let
        [card (fn [re] (re-find (if fold (re-pattern (str "(?i)" re)) (re-pattern re)) command))]
        (cond
            (card "^-LINE\\s+\\d+")
            (let [[_ n f1 f2] (re-find #"^-\S+\s+(\d+)(?:\s+(?:\"([^\"]*)\"|'([^']*)'))?" command)] (assoc pos :file (or f1 f2 (:file pos)) :revert nil :bp (inc pline) :bl (Long/parseLong n)))
            (card "^-FILE-ENTER\\s+\"")
            (let [[_ f] (re-find #"\"([^\"]*)\"" command)] (assoc pos :file f :bp (inc pline) :bl 1 :stack (conj (:stack pos) (assoc pos :card-line (+ (:bl pos) (- pline (:bp pos)))))))
            (card "^-FILE-LEAVE")
            (let
                [outer (peek (:stack pos))]
                (let
                    [stale (boolean (re-find #"STALE" command))]
                    (assoc outer :file (if stale (:file pos) (:file outer)) :revert (when stale (:file outer)) :bp (inc pline) :bl (inc (:card-line outer)) :stack (pop (:stack pos)))))
            :else pos)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private constant-keywords '#{ABORT ALPHABET ARB BAL FAIL FENCE LCASE REM SUCCEED UCASE})
(def ^:private folded-operators '#{+ - * / **})
(def ^:private folded-functions '#{SIZE LEN ANY NOTANY SPAN BREAK BREAKX POS RPOS TAB RTAB ARBNO FENCE REMDR CHAR REPLACE LPAD RPAD REVERSE DATATYPE SQRT EXP LN SIN COS TAN ATAN CHOP})
(def ^:private unary-names '#{$ * & ! % / sharp-op = |})
(def ^:private binary-names '#{sharp-op % & tilde-op at-op})
(def ^:private operators '#{+ - * / ** sharp-op % at-op & tilde-op | ? ?= = . $ !})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- name-form?
    [x]
    (cond
        (or (symbol? x) (keyword? x))
        true
        (list? x)
        (let
            [[op & args] x n (count args)]
            (cond (:subscript (meta x)) true (not (symbol? op)) true (and (= 1 n) (unary-names op)) true (and (= 2 n) (binary-names op)) true (operators op) false :else true))
        :else false))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- name-required! [x] (when-not (name-form? x) (throw (ex-info "syntax error: value used where name is required" {:snobol/error 212}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- name-positions
    [op args]
    (let [n (count args)] (cond (and (= 1 n) ('#{. at-op &} op)) #{0} (and (= 2 n) ('#{. $} op)) #{1} (and (= 2 n) (= '= op)) #{0} (and (= 3 n) (= '?= op)) #{0} :else #{})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- pattern-variable [sym] (some-> (requiring-resolve (symbol "SNOBOL4clojure.env" (name sym))) var-get))
(declare fold)
(defn- constant-name? [x] (or (symbol? x) (and (list? x) (= 2 (count x)) (or (= '& (first x)) (= '* (first x)))) (and (list? x) (= 2 (count x)) (= '$ (first x)) (first (fold (second x))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- fold
    [n]
    (let
        [eval! (requiring-resolve 'SNOBOL4clojure.operators/EVAL!)]
        (cond
            (or (string? n) (number? n))
            [true n]
            (and (symbol? n) (SNOBOL4clojure.env/protected-variables n))
            [true (pattern-variable n)]
            (vector? n)
            (let [comma? (= :comma (first n)) parts (mapv fold (if comma? (rest n) n))] (if (and (not comma?) (every? first parts)) [true (eval! n)] [false nil]))
            (list? n)
            (let
                [[op & args] n names (name-positions op args)]
                (cond
                    (and (= '& op) (= 1 (count args)))
                    (do (when-not (symbol? (first args)) (name-required! (first args))) (if (constant-keywords (first args)) [true (eval! n)] [false nil]))
                    (and (= '* op) (= 1 (count args)))
                    (do (fold (first args)) [true (eval! n)])
                    (and ('#{. at-op} op) (= 1 (count args)) (do (name-required! (first args)) true) (constant-name? (first args)) (or (= '. op) (symbol? (first args)) (= '$ (first (first args)))))
                    (do (fold (first args)) [true (eval! n)])
                    (and ('#{. $} op) (= 2 (count args)))
                    (let [p (fold (first args))] (name-required! (second args)) (let [t (fold (second args))] (if (and (first p) (constant-name? (second args))) [true (eval! n)] [false nil])))
                    :else
                    (let
                        [parts (vec (map-indexed (fn [i a] (when (names i) (name-required! a)) (fold a)) args))]
                        (if
                            (and
                                (every? first parts)
                                (or
                                    (and (folded-operators op) (<= 1 (count args) 2) (not (and (= '/ op) (= 1 (count args)))))
                                    (and (= '| op) (<= 2 (count args)))
                                    (and (folded-functions op) (not (:subscript (meta n))))))
                            (let [v (eval! n)] (if (nil? v) [false nil] [true v]))
                            [false nil]))))
            :else
            [false nil])))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn fold-error
    ([body] (fold-error body nil))
    ([body goto]
        (try
            (fold body)
            (doseq [k [:S :F :G] :let [t (get goto k)] :when (and (some? t) (not (keyword? t)) (not (and (list? t) (= 'DIRECT (first t)))))] (name-required! t) (fold t))
            nil
            (catch clojure.lang.ExceptionInfo e (:snobol/error (ex-data e))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn freeze
    [n]
    (letfn
        [(mark
             [n]
             (cond
                 (and (symbol? n) (SNOBOL4clojure.env/protected-variables n))
                 (list 'pattern-constant n)
                 (vector? n)
                 (with-meta (mapv mark n) (meta n))
                 (list? n)
                 (let [[op & args] n names (name-positions op args)] (with-meta (apply list op (map-indexed (fn [i a] (if (names i) a (mark a))) args)) (meta n)))
                 :else n))
            (walk
                [n]
                (cond
                    (and (list? n) (= '* (first n)) (= 2 (count n)))
                    (with-meta (list '* (walk (second n))) (meta n))
                    (and (or (list? n) (vector? n)) (first (try (fold n) (catch clojure.lang.ExceptionInfo _ [false nil]))))
                    (mark n)
                    (vector? n)
                    (with-meta (mapv walk n) (meta n))
                    (list? n)
                    (with-meta (apply list (first n) (map walk (rest n))) (meta n))
                    :else n))]
        (if (some #(and (symbol? %) (SNOBOL4clojure.env/protected-variables %)) (tree-seq coll? seq n)) (walk n) n)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private system-labels #{:ABORT :CONTINUE :SCONTINUE :RETURN :FRETURN :NRETURN})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CODE!
    ([S] (CODE! S ["."]))
    ([S include-path] (CODE! S include-path nil))
    ([S include-path file] (CODE! S include-path file false))
    ([S include-path file record?]
        (let
            [code-text? record? record?
                (and record? (boolean (re-find #"[\r\n]" (str S))))
                S
                (if (and (not record?) (string/starts-with? (str S) "#!")) (let [k (string/index-of (str S) "\n")] (if k (subs (str S) (inc k)) "")) S)
                [expanded _ cards]
                (if record? [(str S) #{} []] (preprocess-includes* (str S) include-path #{}))
                raw-stmts
                (split-source expanded record?)
                _
                (reset! <DIAGNOSTICS> (:diagnostics (meta raw-stmts)))
                _
                (reset! <SOURCE> {:text expanded :stmts raw-stmts :cards cards})]
            (let
                [src-lines
                    (string/split (str expanded) #"\n")
                    placed
                    (atom {})
                    offset
                    (fn
                        [pline ^String stmt]
                        (let
                            [line
                                (get src-lines (dec (long pline)))
                                from
                                (get @placed pline 0)
                                k
                                (when (and line (string? stmt)) (or (string/index-of line stmt from) (first (filter #(string/starts-with? stmt (subs line %)) (range from (count line))))))]
                            (if k (do (swap! placed assoc pline (+ (long k) (count stmt))) k) 0)))]
                (loop
                    [stmts raw-stmts NO 1 CODES {} NOS {} LABELS {} blanks 0 before [] fold false poss [] pos {:file (str file) :bp 1 :bl 1 :stack []}]
                    (if
                        (empty? stmts)
                        [CODES NOS LABELS {:before before :total blanks :pos poss}]
                        (let
                            [[command pline] (first stmts)]
                            (cond
                                (= command :blank)
                                (recur (rest stmts) NO CODES NOS LABELS (inc blanks) before fold poss pos)
                                (comment? command)
                                (recur (rest stmts) NO CODES NOS LABELS blanks before fold poss pos)
                                (control? command)
                                (recur
                                    (rest stmts)
                                    NO CODES NOS LABELS blanks before
                                    (do
                                        (case-card fold command)
                                        (when-let
                                            [q (and (re-find #"^-INCLUDE\s+['\"]" command) (re-find #"['\"]" command))]
                                            (swap! <DIAGNOSTICS> conj [pline 285 (inc (long (string/index-of command q))) NO]))
                                        fold)
                                    poss
                                    (position-card fold command pline pos))
                                :else
                                (let
                                    [stmt
                                        (cond-> command fold fold-case)
                                        _
                                        (SNOBOL4clojure.env/note-names! (source-names stmt))
                                        [_ e-kw e-sp e-op]
                                        (re-find #"^(END|end)([ \t]+)(\S.*?)[ \t]*$" stmt)
                                        stmt
                                        (if
                                            (and e-op (not (and (re-matches #"[A-Za-z0-9][A-Za-z0-9._]*" e-op) (contains? NOS (keyword e-op)))))
                                            (do (swap! <DIAGNOSTICS> conj [pline 215 (inc (count (str e-kw e-sp))) NO]) e-kw)
                                            stmt)
                                        ast
                                        (if (and record? (line-end-outside-literals? stmt)) ::line-end (parse-statement stmt))
                                        code
                                        (if
                                            (= ast ::line-end)
                                            {:reason :line-end :line pline :column 1}
                                            (try
                                                (emitter ast)
                                                (catch clojure.lang.ExceptionInfo e (if (:snobol/compile (ex-data e)) {:reason :compile :line pline :column 1 :text (ex-message e)} (throw e)))))
                                        here
                                        [(:file pos) (+ (:bl pos) (- pline (:bp pos)))]
                                        pos''
                                        (if (:revert pos) (assoc pos :file (:revert pos) :revert nil) pos)]
                                    (if
                                        (and (map? code) (:reason code))
                                        (let
                                            [{n :error col :column}
                                                (or
                                                    (when-not (= :compile (:reason code)) (syntax-error stmt))
                                                    (when (= :compile (:reason code)) (let [se (syntax-error stmt)] (when (= 231 (:error se)) se)))
                                                    {:error (case (:reason code) :compile 231 :line-end 230 220)})]
                                            (let
                                                [lbl
                                                    (when-not (#{\+ \.} (first stmt)) (second (re-find #"^([A-Za-z0-9][^ \t;]*)" stmt)))
                                                    lkey
                                                    (when lbl (keyword lbl))
                                                    dup?
                                                    (and lkey (not code-text?) (or (contains? NOS lkey) (system-labels lkey)))]
                                                (when-not
                                                    (#{\+ \.} (first stmt))
                                                    (let
                                                        [at
                                                            (fn
                                                                [c]
                                                                (loop
                                                                    [l (long pline) c (long c)]
                                                                    (let
                                                                        [len (count (get src-lines (dec l) "")) nxt (get src-lines l)]
                                                                        (if (and (> c len) nxt (#{\+ \.} (first nxt))) (recur (inc l) (- c len)) [l c]))))
                                                            [dl dc]
                                                            (if (and col (not= 214 n) (not dup?)) (at (+ (long (offset pline command)) (min (long col) (max 1 (count stmt))))) [pline nil])]
                                                        (swap! <DIAGNOSTICS> conj (if dup? [pline 217 nil NO] [dl n dc NO]))))
                                                (recur
                                                    (rest stmts)
                                                    (inc NO)
                                                    (assoc CODES NO (error-node code))
                                                    (if (and lkey (not dup?)) (assoc NOS lkey NO) NOS)
                                                    (if (and lkey (not dup?)) (assoc LABELS NO lkey) LABELS)
                                                    blanks
                                                    (conj before blanks)
                                                    fold
                                                    (conj poss here)
                                                    pos'')))
                                        (let
                                            [label
                                                (:label code)
                                                body
                                                (:body code)
                                                goto
                                                (:goto code)
                                                dup?
                                                (and (not code-text?) (keyword? label) (or (contains? NOS label) (system-labels label)))
                                                ferr
                                                (if dup? 217 (fold-error body (when (map? goto) goto)))
                                                _
                                                (when ferr
                                                    (let
                                                        [last-line
                                                            (loop
                                                                [l (long pline) c (+ (long (offset pline command)) (count stmt))]
                                                                (let
                                                                    [len (count (get src-lines (dec l) "")) nxt (get src-lines l)]
                                                                    (if (and (> c len) nxt (#{\+ \.} (first nxt))) (recur (inc l) (- c len)) l)))]
                                                        (swap! <DIAGNOSTICS> conj [(if (= 217 ferr) pline last-line) ferr nil NO])))
                                                body
                                                (if ferr body (freeze body))
                                                key
                                                (if (and label (not dup?)) label NO)
                                                code
                                                (reduce #(conj %1 %2) [] [body goto])
                                                nos
                                                (if (keyword? key) (assoc NOS key NO) NOS)
                                                labels
                                                (if (keyword? key) (assoc LABELS NO key) LABELS)
                                                codes
                                                (assoc CODES key code)]
                                            (if
                                                (#{:END :end} label)
                                                [codes nos labels {:before (conj before blanks) :total blanks :pos (conj poss here)}]
                                                (recur (rest stmts) (inc NO) codes nos labels blanks (conj before blanks) fold (conj poss here) pos'')))))))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- end-entry [codes] (let [b (first (or (get codes :END) (get codes :end)))] (when (symbol? b) (let [k (keyword (name b))] (when (@<STNO> k) k)))))
(declare load-code)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CODE
    ([S] (CODE S ["."]))
    ([S include-path] (CODE S include-path nil))
    ([S include-path file] (let [[_ _ _ disp :as ir] (CODE! S include-path file)] (reset! <CODELINE> (or (last (:pos disp)) ["" 0])) (load-code ir))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- load-code
    [[codes nos labels disp]]
    (let
        [start (inc @STNO) base @BLANKS]
        (loop
            [NO 1]
            (if
                (> NO (count codes))
                (do (swap! BLANKS + (:total disp 0)) (or (end-entry codes) (if (and (@<LABL> start) (@<CODE> (@<LABL> start))) (@<LABL> start) (when (@<CODE> start) start))))
                (do
                    (swap! STNO inc)
                    (swap! <DISPNO> assoc @STNO (+ @STNO base (get (:before disp) (dec NO) 0)))
                    (swap! <POS> assoc @STNO (get (:pos disp) (dec NO) ["" 0]))
                    (if-let
                        [label (labels NO)]
                        (do (swap! <CODE> #(assoc % label (codes label))) (swap! <LABL> #(assoc % @STNO label)) (swap! <STNO> #(assoc % label @STNO)))
                        (swap! <CODE> #(assoc % @STNO (codes NO))))
                    (recur (inc NO)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CODE-block
    [S]
    (let
        [[f l] (swap! <CODELINE> (fn [[f l]] [f (inc (long l))])) [codes :as ir] (update-in (CODE! S ["."] nil true) [3 :pos] (fn [ps] (mapv #(if (= "" (first %)) [f l] %) ps)))]
        (if-let
            [[_ n _ bad] (first @<DIAGNOSTICS>)]
            (let
                [[codes nos labels disp] ir k (dec (long (or bad 1))) before (fn [m] (into {} (filter (fn [[no _]] (<= (long no) k)) m)))]
                (when
                    (pos? k)
                    (load-code
                        [(into {} (for [no (range 1 (inc k)) :let [key (or (labels no) no)]] [key (codes key)]))
                            (into {} (filter (fn [[_ no]] (<= (long no) k)) nos))
                            (before labels)
                            (-> disp (update :before #(vec (take k %))) (update :pos #(vec (take k %))) (assoc :total (get (:before disp) k (:total disp))))]))
                (swap! <CODE> assoc (inc (long @STNO)) compile-error)
                {:error n})
            (let
                [entry (load-code ir)]
                (if
                    (or (contains? codes :END) (contains? codes :end))
                    entry
                    (do (swap! STNO inc) (swap! <DISPNO> assoc @STNO (+ @STNO @BLANKS)) (swap! <CODE> assoc @STNO block-end) (or entry @STNO)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CODE-ir
    [[codes nos labels disp]]
    (let
        [start (inc @STNO) base @BLANKS]
        (loop
            [NO 1]
            (if
                (> NO (count codes))
                (do (swap! BLANKS + (:total disp 0)) (if (and (@<LABL> start) (@<CODE> (@<LABL> start))) (@<LABL> start) (when (@<CODE> start) start)))
                (do
                    (swap! STNO inc)
                    (swap! <DISPNO> assoc @STNO (+ @STNO base (get (:before disp) (dec NO) 0)))
                    (swap! <POS> assoc @STNO (get (:pos disp) (dec NO) ["" 0]))
                    (if-let
                        [label (labels NO)]
                        (do (swap! <CODE> #(assoc % label (codes label))) (swap! <LABL> #(assoc % @STNO label)) (swap! <STNO> #(assoc % label @STNO)))
                        (swap! <CODE> #(assoc % @STNO (codes NO))))
                    (recur (inc NO)))))))
