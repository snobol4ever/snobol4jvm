(ns SNOBOL4clojure.operators
  ;; SNOBOL4 operator definitions and the EVAL/EVAL!/INVOKE evaluator.
  ;; Operators shadow clojure.core: ?, =, |, $, ., +, -, *, /, %, !
  (:import  [SNOBOL4clojure.env NAME])
  (:require [clojure.tools.trace      :refer :all]
            [SNOBOL4clojure.env       :refer
             [ε η equal not-equal Σ+ subtract multiply divide
              ncvt scvt sstr num $$ $$fn out reference snobol-set!
              table? table-get table-set
              array? array-get array-set array-prototype assign-keyword! name-string
              snobol-return! snobol-freturn! snobol-nreturn!
              <FUNS> <FDEFS> <CHANNELS> <OPSYN> &STNO &FNCLEVEL &CASE &OUTPUT &FILE &LINE &RTNTYPE <LASTOUT> fold-case]]
            [SNOBOL4clojure.functions :refer
             [ASCII CHAR ORD DATE TIME REMDR ATAN CHOP COS EXP LN SIN SQRT TAN INTEGER integer-predicate REAL SETEXIT HOST STRING SIZE TRIM DUPL REVERSE LPAD RPAD REPLACE SUBSTR
              ITEM PROTOTYPE CONVERT COPY FIELD SORT RSORT DATA
              open-input-channel! open-output-channel! close-channel! write-to-channel! rewind-channel!]]
            [SNOBOL4clojure.match     :refer [MATCH SEARCH FULLMATCH]]
            [SNOBOL4clojure.patterns  :refer
             [ANY BREAK BREAKX NOTANY SPAN ARBNO FENCE
              ABORT BAL CONJ
              LEN POS RPOS RTAB TAB FAIL]]
            [SNOBOL4clojure.emitter   :refer [emitter]]
            [SNOBOL4clojure.dump      :refer [dump-text]]
            [SNOBOL4clojure.grammar   :refer [parse-expression]]
            [SNOBOL4clojure.trace     :refer [fire-value! fire-call! fire-return! trace! stoptr!]])
  (:refer-clojure :exclude [= + - * / num]))

;; ── Numeric conversion macros ─────────────────────────────────────────────────
(defmacro numcvt [x] `(ncvt ~x))
(defmacro uneval [x] `(if (list? ~x) ~x (list 'identity ~x)))

(declare APPLY)

;; ── Operator helpers ──────────────────────────────────────────────────────────
(defn annihilate    [_x]        nil)
(defn match-replace [_n _s _p]  nil)
(defn keyword-value [_n]        nil)
(defn $=            [_p _n])
(defn .=            [_p _n])
(defn lie           [p]        `(if (nil? ~p) ε nil))
(defn x-2           [op x y]    (list op x y))
(defn x-n           [op x y Ω]  (apply list (conj Ω y x op)))
(defn n-1           [op x]      (list op (numcvt x)))
(defn n-2           [op x y]    (list op (numcvt x) (numcvt y)))
(defn n-n           [op x y Ω]  (apply list op (map ncvt (conj Ω y x))))

;; ── Comparison primitives (generated via eval) ────────────────────────────────
(defn- primitive [func default missing cvt condition]
  (list 'defn func
    (list []            missing)
    (list ['x]          (list 'if (condition (cvt 'x) default) ε))
    (list ['x 'y '& '_] (list 'if (condition (cvt 'x) (cvt 'y)) ε))))

(eval (primitive 'LEQ    ε   ε scvt     #(list 'equal %1 %2)))
(eval (primitive 'LNE    ε nil scvt     #(list 'not=  %1 %2)))
(eval (primitive 'LLE    ε   ε scvt     #(list '<=   (list 'compare %1 %2) 0)))
(eval (primitive 'LLT    ε nil scvt     #(list '<    (list 'compare %1 %2) 0)))
(eval (primitive 'LGE    ε   ε scvt     #(list '>=   (list 'compare %1 %2) 0)))
(eval (primitive 'LGT    ε nil scvt     #(list '>    (list 'compare %1 %2) 0)))
(eval (primitive 'IDENT  ε   ε identity #(list 'equal %1 %2)))
(eval (primitive 'DIFFER ε nil identity #(list 'not= %1 %2)))

;; ── SNOBOL4 operators ─────────────────────────────────────────────────────────
(defn =     ([_x]       η)                         ; unary  — programmable
            ([n x]      (list '= n x)))             ; binary — assignment
(defn ?     ([x]        (annihilate x))             ; unary  — interrogation
            ([s p]      (SEARCH (str s) p))         ; binary — match, returns [start end] or nil
            ([n s p]    (match-replace n s p)))     ; ternary — match+replace
(defn ?=    ([n s p]    (match-replace n s p)))
(defn &     ([_n]       nil)                     ; unary  — keyword
            ([_x _y]    η))                      ; binary — programmable
(defn at    ([n]        (list 'CURSOR-IMM! n))   ; unary  — cursor assign
            ([_x _y]    η))
(defn +     ([x]        (n-1 Σ+ x))
            ([x y]      (n-2 Σ+ x y))
            ([x y & zs] (n-n Σ+ x y zs)))
(defn -     ([x]        (n-1 subtract x))
            ([x y]      (n-2 subtract x y))
            ([x y & zs] (n-n subtract x y zs)))
(defn sharp ([_x]       η)
            ([_x _y]    η))
(defn /     ([_x]       η)
            ([x y]      (n-2 divide x y)))
(defn *     ([x]        (list 'DEFER! (fn [] x)))    ; unary  — defer eval
            ([x y]      (n-2 multiply x y))
            ([x y & zs] (n-n multiply x y zs)))
(defn %     ([_x]       η)
            ([_x _y]    η))
;; An arithmetic operand as a number. A string converts when it has the form of a number, blanks around it ignored, and a null or blank string is 0 (manual ch. 17,
;; String to integer and String to real); anything else is the operator's error, "1 Addition left operand is not numeric" and its kin (manual, Appendix D).
(defn- operand [x code msg]
  (let [n (num x)]
    (if (and (double? n) (Double/isNaN n) (not (double? x)))
      (throw (ex-info msg {:snobol/error code}))
      n)))

;; An integer result past 64 bits is the operator's overflow error (3 addition, 34 subtraction, 28 multiplication); exact is Math/addExact and its kin.
(defn- exact [exact-op a b code msg]
  (try (exact-op (long a) (long b)) (catch ArithmeticException _ (throw (ex-info msg {:snobol/error code})))))

;; The numeric comparisons: "N1 and n2 must evaluate to integer or real values" (manual ch. 19, EQ and its kin), compared as numbers whatever their types, so EQ(2.0,2)
;; succeeds; an argument that is not numeric is the function's error (Appendix D: EQ 101/102, NE 149/150, LT 147/148, LE 118/119, GT 111/112, GE 109/110). An argument left
;; out is the null string, 0.
(defmacro ^:private numeric-predicate [f cmp first-code second-code]
  (let [nm (clojure.string/lower-case (name f))]
    `(defn ~f [& args#]
       (let [a# (operand (first args#) ~first-code ~(str nm " first argument is not numeric"))
             b# (operand (second args#) ~second-code ~(str nm " second argument is not numeric"))]
         (when (~cmp a# b#) ε)))))
(numeric-predicate EQ clojure.core/== 101 102)
(numeric-predicate NE (fn [a b] (not (clojure.core/== a b))) 149 150)
(numeric-predicate LT clojure.core/< 147 148)
(numeric-predicate LE clojure.core/<= 118 119)
(numeric-predicate GT clojure.core/> 111 112)
(numeric-predicate GE clojure.core/>= 109 110)

(defn pow-fn
  "SNOBOL4 exponentiation (**, and its synonyms ^ and !). Integer ** non-negative
   integer is an exact integer: 0 ** 0 is error 18 and a result past 64 bits is
   error 17, as SPITBOL reports them. A negative exponent or a real operand gives
   a real; 0 ** -n is 0.0, as SPITBOL prints it."
  [x y]
  (let [x (operand x 16 "exponentiation left operand is not numeric")
        y (operand y 15 "exponentiation right operand is not numeric")]
    (if (and (integer? x) (integer? y) (not (neg? y)))
      (cond
        (and (zero? x) (zero? y))
        (throw (ex-info "exponentiation result is undefined" {:snobol/error 18}))
        (zero? x) 0
        (clojure.core/= x 1) 1
        (clojure.core/= x -1) (if (even? y) 1 -1)
        (clojure.core/> y 64)
        (throw (ex-info "exponentiation caused integer overflow" {:snobol/error 17}))
        :else
        (let [r (.pow (biginteger x) (int y))]
          (if (clojure.core/< (.bitLength r) 64)
            (long r)
            (throw (ex-info "exponentiation caused integer overflow" {:snobol/error 17})))))
      (if (and (integer? x) (zero? x) (neg? y))
        0.0
        (Math/pow (double x) (double y))))))
(defn !     ([_x]       η)
            ([x y]      (pow-fn x y)))
(defn **    ([x y]      (pow-fn x y)))
(defn $     ([n]        ($$ n))                       ; unary  — indirection
            ([x y]      (x-2 'CAPTURE-IMM x y))       ; binary — immediate capture
            ([x y & zs] (x-n 'CAPTURE-IMM x y zs)))
(defn .     ([x]        (NAME. x))                    ; unary  — name
            ([x y]      (x-2 'CAPTURE-COND x y))      ; binary — conditional capture
            ([x y & zs] (x-n 'CAPTURE-COND x y zs)))
(defn tilde ([x]        (list 'ALT x ε))              ; unary  — optional (~P = P | ε)
            ([_x _y]    η))
(defn |     ([_x]       η)                       ; unary  — programmable
            ([x y]      (x-2 'ALT x y))          ; binary — alternation
            ([x y & zs] (x-n 'ALT x y zs)))

;; OPSYN files an operator's definition under its spelling and arity, so unary
;; and binary | are separate, and a function synonym under its name alone.
(defn- opsyn-key [spelling arity] (str spelling "/" arity))

;; The source spelling of an operator the emitter renames.
(def ^:private op-spelling {'at "@", 'sharp "#", 'tilde "~"})

;; ── INVOKE ────────────────────────────────────────────────────────────────────
(declare EVAL! compile-expression deferred? expression-of)

;; A concatenation evaluates to (SEQ v ...). It is a string when every part is
;; a string, a number or a NAME (text: its variable's name), and otherwise a
;; pattern, which the matcher reads as a SEQ list and which must not be
;; flattened into text. Null parts drop out, and a part they leave alone is
;; the value itself: P '' is P, NE(1,2) 5 is the INTEGER 5. (A one-part SEQ that had no null parts, as
;; CONVERT(x,'PATTERN') builds it, stays as it is.)
(defn- concat-value [r]
  (if (and (list? r) (clojure.core/= (first r) 'SEQ))
    (let [parts (remove #(clojure.core/= ε %) (rest r))]
      (cond
        (every? #(or (string? %) (number? %) (instance? NAME %)) parts)
        (if (and (clojure.core/= 1 (count parts)) (< 1 (count (rest r))))
          (first parts)                      ; one part left after the nulls: it, as it was
          (apply str (map sstr parts)))
        (clojure.core/= (count parts) (count (rest r))) r
        (empty? (rest parts))                        (first parts)
        :else                                        (apply list 'SEQ parts)))
    r))

;; What OUTPUT writes for a value: an object that is not a string, a number or a name prints as its datatype name, as SPITBOL prints it (PATTERN, EXPRESSION, ARRAY,
;; TABLE, the type of a DATA object); everything else as sstr.
(defn- output-text [v]
  (if (or (and (or (list? v) (vector? v)) (#{"PATTERN" "EXPRESSION"} (SNOBOL4clojure.env/DATATYPE v)))
          (array? v) (table? v) (instance? SNOBOL4clojure.env.DataObj v) (instance? SNOBOL4clojure.env.CodeBlock v))
    (SNOBOL4clojure.env/DATATYPE v)
    (sstr v)))

(defn- program-fn
  "The function a program gave the name op, or nil: a DATA constructor or field (held as the value of the name), or a DEFINEd or OPSYN function (held in
   <FUNS>). Only names that start with a letter: operators are not looked up."
  [op]
  (when (and (symbol? op) (Character/isLetter (.charAt (name op) 0)))
    (let [v (reference op)]
      (if (fn? v) v (get @<FUNS> (name op))))))

;; A variable put back as its caller had it: set to the value it had, or unset again when it had none.
(defn- restore-var! [sym v]
  (if (nil? v) (swap! SNOBOL4clojure.env/<VARS> dissoc sym) (snobol-set! sym v)))

(defn INVOKE [op & args]
  ;; A program's own DATA function wins over the engine's arm of the same
  ;; name: SPITBOL refuses to redefine its own functions (ERROR 248), so a
  ;; program can only own a name SPITBOL does not, such as REAL or STRING.
  (if-let [pf (program-fn op)]
    (apply pf args)
  (case op
    |       (apply | args)
    $       (apply $ args)
    .       (apply . args)
    LEN     (LEN    (first args))
    POS     (POS    (first args))
    RPOS    (RPOS   (first args))
    TAB     (TAB    (first args))
    RTAB    (RTAB   (first args))
    ANY     (ANY    (first args))
    BREAK   (BREAK  (first args))
    BREAKX  (BREAKX (first args))
    NOTANY  (NOTANY (first args))
    SPAN    (SPAN   (first args))
    FENCE   (if (seq args) (FENCE (first args)) (FENCE))
    ARBNO   (ARBNO  (first args))
    ABORT   ABORT
    BAL     BAL
    CONJ    (CONJ   (first args) (second args))
    EQ      (EQ     (first args) (second args))
    NE      (NE     (first args) (second args))
    LE      (LE     (first args) (second args))
    LT      (LT     (first args) (second args))
    GE      (GE     (first args) (second args))
    GT      (GT     (first args) (second args))
    FAIL    FAIL
    ;; Arithmetic — coerce to SNOBOL numeric type (integer if both integer, else real)
    ;; unary + is affirmation (error 4), unary - negation (error 10)
    +       (if (clojure.core/= 1 (count args))
              (operand (first args) 4 "affirmation operand is not numeric")
              (let [a (operand (first args) 1 "addition left operand is not numeric")
                    b (operand (second args) 2 "addition right operand is not numeric")]
                (if (and (integer? a) (integer? b))
                  (exact #(Math/addExact ^long %1 ^long %2) a b 3 "addition caused integer overflow")
                  (clojure.core/+ a b))))
    -       (if (clojure.core/= 1 (count args))
              (let [a (operand (first args) 10 "negation operand is not numeric")]
                (if (integer? a) (exact #(Math/subtractExact ^long %1 ^long %2) 0 a 11 "negation caused integer overflow") (clojure.core/- a)))
              (let [a (operand (first args) 32 "subtraction left operand is not numeric")
                    b (operand (second args) 33 "subtraction right operand is not numeric")]
                (if (and (integer? a) (integer? b))
                  (exact #(Math/subtractExact ^long %1 ^long %2) a b 34 "subtraction caused integer overflow")
                  (clojure.core/- a b))))
    *       (let [a (operand (first args) 26 "multiplication left operand is not numeric")
                  b (operand (second args) 27 "multiplication right operand is not numeric")]
              (if (and (integer? a) (integer? b))
                (exact #(Math/multiplyExact ^long %1 ^long %2) a b 28 "multiplication caused integer overflow")
                (clojure.core/* a b)))
    tilde   (let [v (first args)]       ; ~ negates success/failure
              (if (nil? v) ε nil))
    ;; a zero divisor is "14 Division caused integer overflow" between integers and "262 Division caused real overflow" otherwise, as sbl -bf reports them
    /       (let [a (operand (first args) 12 "division left operand is not numeric")
                  b (operand (second args) 13 "division right operand is not numeric")]
              (cond
                (and (integer? a) (integer? b))
                (if (or (zero? b) (and (clojure.core/= b -1) (clojure.core/= a Long/MIN_VALUE)))
                  (throw (ex-info "division caused integer overflow" {:snobol/error 14}))
                  (quot a b))
                (zero? (double b)) (throw (ex-info "division caused real overflow" {:snobol/error 262}))
                :else (clojure.core// (double a) (double b))))
    **      (let [[x y] args] (pow-fn x y))
    ?       (let [[s p] args] (SEARCH (str s) p))
    =       (let [[N r] args]
              ;; nil replacement = sub-expression failure → statement fails
              (when-not (nil? r)
              (cond
                ;; keyword assignment: (= (& K) val)
                (and (list? N) (clojure.core/= (first N) '&))
                (assign-keyword! (second N) r)
                ;; subscript assignment: (= (A k...) val) — mutate TABLE, ARRAY, or PDD
                (and (list? N) (>= (count N) 2))
                (let [[container-sym & ks] N
                      r (concat-value r)
                      ;; the container is a variable's name, or the table or array itself when it came from a subscript (tt['blue']['green'])
                      ;; a field of a DATA type, F(x) = v, when the name holds no table or array (A<k> and F(k) are one form in the IR)
                      raw-container (if (symbol? container-sym)
                                      (let [v ($$ container-sym) f (get @<FUNS> (name container-sym))]
                                        (if (and (not (table? v)) (not (array? v)) (:data (meta f))) f v))
                                      container-sym)
                      ;; NAME indirect reference — dereference to the actual array/table
                      container (if (instance? NAME raw-container)
                                  ($$ (symbol (str (.n raw-container))))
                                  raw-container)]
                  (cond
                    (table? container)
                    (table-set container (first ks) r)
                    ;; a subscript out of bounds fails the statement, as a read of one does (manual ch. 14, -FAIL: "...especially when array subscripts are out of bounds")
                    (array? container)
                    (array-set container (vec ks) r)
                    ;; DATA field setter: accessor(instance) = val assigns the field of the object in place (the instance is already evaluated)
                    (fn? container)
                    (when (container (first ks) r) r)
                    ;; a variable that holds no table, array or object cannot be subscripted: ERROR 235, as SPITBOL raises it
                    :else (throw (ex-info "subscripted operand is not table or array" {:snobol/error 235}))))
                ;; normal variable assignment
                (clojure.core/contains? #{'OUTPUT 'TERMINAL 'INPUT} N)
                (let [out-val (concat-value r)]
                  (swap! SNOBOL4clojure.env/<ASSOC-LAST> assoc N out-val)
                  (when (and (clojure.core/= N 'OUTPUT) (not (zero? (long @&OUTPUT)))) (println (output-text out-val)))
                  (when (clojure.core/= N 'TERMINAL) (binding [*out* *err*] (println (output-text out-val))))
                  out-val)
                ;; Named output channel: var is registered as :output → write to channel writer
                (clojure.core/= :output (:type (get @<CHANNELS> N)))
                (let [out-val (concat-value r)]
                  (write-to-channel! N out-val)
                  (swap! SNOBOL4clojure.env/<ASSOC-LAST> assoc N out-val)
                  out-val)
                :else
                (let [v (concat-value r)]
                  (snobol-set! N v)
                  (fire-value! N v)
                  v))))
    ?=      (let [[n p R] args
                  subject (str (or ($$ n) ""))
                  pat     p]                      ; already evaluated by EVAL! dispatch
              (when-let [[start end] (SEARCH subject pat)]
                ;; The replacement is evaluated after the match, so it sees the
                ;; variables the match assigned (S RTAB(N) . S REM . S1 = S1 S).
                (let [repl   (str (or (EVAL! R) ""))
                      result (str (subs subject 0 start)
                                  repl
                                  (subs subject end))]
                  (INVOKE '= n result)       ; assigned as any assignment is: a value trace fires, OUTPUT writes
                  result)))
    DEFINE  (let [[proto entry-arg] args
                  ;; Parse: 'fname(p1,p2,...)l1,l2,...'  or  'fname(p1,...)'
                  lp        (.indexOf ^String proto "(")
                  rp        (.indexOf ^String proto ")")
                  fname     (if (>= lp 0) (.substring ^String proto 0 lp) proto)
                  inner     (if (and (>= lp 0) (>= rp 0))
                              (.substring ^String proto (inc lp) rp) "")
                  after-rp  (if (>= rp 0) (.substring ^String proto (inc rp)) "")
                  params    (if (clojure.string/blank? inner) []
                              (mapv clojure.string/trim
                                    (clojure.string/split inner #",")))
                  locals    (if (clojure.string/blank? after-rp) []
                              (let [s (-> after-rp clojure.string/trim
                                          (clojure.string/replace #"^," ""))]
                                (filterv (complement clojure.string/blank?)
                                         (mapv clojure.string/trim
                                               (clojure.string/split s #",")))))
                  f-sym     (symbol fname)
                  ;; Entry point: explicit .label arg, or the keyword of fname, in the case it is written (sbl -bf folds no name)
                  entry     (if (and (>= (count args) 2) entry-arg)
                              ;; entry-arg may be a NAME (symbol) from (.label) notation
                              (if (symbol? entry-arg)
                                (keyword (str (name entry-arg)))
                                (keyword (str entry-arg)))
                              (keyword fname))
                  all-saved (into params locals)]
              (letfn [(the-fn [& call-args]
                        ;; Save current values of params + locals
                        ;; The result variable is saved with the params and locals: it belongs to one activation, so a recursive call must not
                        ;; leave the function object, or its own result, in the caller's.
                        ;; What is saved is the program's own variable (nil: it was not set), never the engine's built-in of that name that $$ would
                        ;; find in the core namespace, so the restore leaves the variable as the caller had it.
                        (let [saved   (zipmap all-saved (map #(reference (symbol %)) all-saved))
                              f-saved (reference f-sym)
                              stno-saved @&STNO
                              file-saved @&FILE
                              line-saved @&LINE]
                          ;; Clear the result slot, then bind the parameters: a parameter that has the function's name starts the result with its argument, as sbl -bf
                          ;; has it (mx(9,2) of DEFINE('mx(mx,x)') returns 9 when the body assigns nothing)
                          (snobol-set! f-sym ε)
                          (doseq [i (range (count params))]
                            (snobol-set! (symbol (params i))
                                         (nth call-args i ε)))
                          ;; Clear locals
                          (doseq [l locals]
                            (snobol-set! (symbol l) ε))
                          ;; CALL trace
                          (fire-call! fname (map #(reference (symbol %)) params))
                          ;; Run from entry label; catch RETURN/FRETURN/NRETURN. The function level is one deeper while the body runs.
                          (swap! &FNCLEVEL inc)
                          (let [run-fn  (ns-resolve 'SNOBOL4clojure.runtime 'RUN)
                                outcome (try
                                          (when run-fn ((var-get run-fn) entry))
                                          :return   ; normal fall-through = return
                                          ;; only the three returns end a call; END, the step limit and an error go on up (END in a function ends the program)
                                          (catch clojure.lang.ExceptionInfo e
                                            (let [sig (:snobol/signal (ex-data e))]
                                              (if (#{:return :freturn :nreturn} sig) sig (throw e))))
                                          (finally (swap! &FNCLEVEL dec)))]
                            ;; Collect result before restoring
                            (let [result ($$ f-sym)]
                              ;; RETURN trace
                              (fire-return! fname result outcome)
                              ;; Restore saved values
                              (doseq [[k v] saved]
                                (restore-var! (symbol k) v))
                              ;; Restore the caller's result variable
                              (restore-var! f-sym f-saved)
                              ;; the statement that called the function is again the one last executed
                              (reset! &STNO stno-saved)
                              (reset! &FILE file-saved)
                              (reset! &LINE line-saved)
                              (reset! &RTNTYPE (case outcome :freturn "FRETURN" :nreturn "NRETURN" "RETURN"))
                              ;; Dispatch on outcome
                              (case outcome
                                :return  result
                                :freturn nil    ; nil → statement failure → :F branch
                                ;; NRETURN returns a name: the NAME the function variable holds, or the variable a string spells
                                :nreturn (cond (instance? NAME result) (SNOBOL4clojure.env/->NameResult (.n ^NAME result))
                                               (and (string? result) (seq result)) (SNOBOL4clojure.env/->NameResult (symbol result))
                                               :else nil)
                                result)))))]   ; fall-through = return
                ;; The function lives in <FUNS>, not in the variable of its name: a function and a variable are two things (sbl -bf).
                (swap! <FUNS>  assoc fname the-fn)
                (swap! <FDEFS> assoc fname
                               {:params params :locals locals})
                ε))
    define  (apply INVOKE 'DEFINE args)
    ;; ── OPSYN(new, old, n) ── Sprint 25E ─────────────────────────────────────────────
    ;; Makes 'new' a synonym for 'old'.  n omitted/0=function, 1=unary op, 2=binary op.
    ;; Stores a wrapper fn in <FUNS> under new-name, as it is written, so the INVOKE fallthrough
    ;; arm dispatches to it automatically (it already checks <FUNS> by that key).
    ;; For operator synonyms (n=1/2), also index under the raw symbol string.
    ;; Always succeeds (even if old is unknown — wraps to no-op fn).
    OPSYN   (let [[new-arg old-arg n-arg] args
                  new-name (name-string new-arg)
                  old-name (name-string old-arg)
                  old-sym  (symbol (name-string old-arg))
                  n        (when (and n-arg (not= n-arg ε)) (long n-arg))
                  old-fn   (or (get @<FUNS> old-name)
                               (if (and n (pos? n))
                                 (if (clojure.core/= n 1)
                                   (fn [a]   (INVOKE old-sym a))
                                   (fn [a b] (INVOKE old-sym a b)))
                                 (fn [& a] (apply INVOKE old-sym (vec a)))))]
              (if (and n (pos? n))
                (swap! <FUNS> assoc (opsyn-key (name-string new-arg) n) old-fn)
                (swap! <FUNS> assoc new-name old-fn))
              ε)
    opsyn   (apply INVOKE 'OPSYN args)
    ;; ── LOAD(fnname, externalname) — stub ─────────────────────────────────────
    ;; Dynamic library loading is out of scope; return nil (failure) gracefully.
    LOAD    nil
    load    nil
    APPLY   (apply APPLY (first args) (rest args))
    apply   (apply APPLY (first args) (rest args))
    REPLACE (let [[s1 s2 s3] args] (REPLACE s1 s2 s3))
    TABLE   (apply SNOBOL4clojure.env/TABLE args)
    table   (apply SNOBOL4clojure.env/TABLE args)
    ARRAY   (apply SNOBOL4clojure.env/ARRAY args)
    array   (apply SNOBOL4clojure.env/ARRAY args)
    ASCII   (ASCII  (first args))
    REMDR   (REMDR  (first args) (second args))
    ;; TRACE / STOPTR register and remove traces in trace.clj.
    TRACE   (let [[nm ty lbl f] args] (trace! nm (or ty "VALUE") lbl f) ε)
    STOPTR  (let [[nm ty] args] (stoptr! nm (or ty "VALUE")) ε)
    ;; COLLECT returns an integer, the store left, as SPITBOL does (DATATYPE INTEGER).
    COLLECT (quot (.freeMemory (Runtime/getRuntime)) 8)
    CLEAR     (SNOBOL4clojure.env/clear-vars! (first args))
    BACKSPACE (apply SNOBOL4clojure.functions/BACKSPACE args)
    ;; Not yet implemented: these keep the null result they had before an
    ;; undefined name became ERROR 022, so no program that calls them regresses.
    EJECT     ε
    SETEXIT (apply SETEXIT args)
    HOST    (apply HOST args)
    ;; DUMP(i) writes a dump now: 1 the variables and keywords, 2 and 3 the elements and fields too; any other value writes nothing (manual ch. 19, DUMP)
    DUMP    (let [n (num (first args))] (when (and (integer? n) (clojure.core/<= 1 n 3)) (print (dump-text n))) ε)
    ATAN    (ATAN   (first args))
    CHOP    (CHOP   (first args))
    COS     (COS    (first args))
    EXP     (EXP    (first args))
    LN      (LN     (first args))
    SIN     (SIN    (first args))
    SQRT    (SQRT   (first args))
    TAN     (TAN    (first args))
    INTEGER (integer-predicate (first args))
    REAL    (REAL    (first args))
    STRING  (STRING  (first args))
    SIZE    (SIZE   (first args))
    TRIM    (TRIM   (first args))
    DUPL    (DUPL   (first args) (second args))
    REVERSE (REVERSE (first args))
    ITEM      (apply ITEM args)
    item      (ITEM      (first args) (second args))
    PROTOTYPE (apply PROTOTYPE args)
    prototype (PROTOTYPE (first args))
    ;; "String to expression: The string must represent a legal SPITBOL expression. The compiler is used to convert the string into its equivalent expression"; an integer
    ;; or a real becomes "an EXPRESSION which when evaluated yields the original" value (manual ch. 17)
    CONVERT   (let [[x t] args]
                (if (clojure.core/= "EXPRESSION" (clojure.string/upper-case (str t)))
                  (cond (deferred? x) x
                        (string? x)   (when-some [ir (compile-expression x)] (expression-of ir))
                        (number? x)   (expression-of x)
                        :else         (CONVERT x t))
                  (CONVERT x t)))
    convert   (CONVERT   (first args) (second args))
    COPY      (COPY      (first args))
    copy      (COPY      (first args))
    FIELD     (FIELD     (first args) (second args))
    field     (FIELD     (first args) (second args))
    SORT      (apply SORT  args)
    sort      (apply SORT  args)
    RSORT     (apply RSORT args)
    rsort     (apply RSORT args)
    DATA      (DATA      (first args))
    data      (DATA      (first args))
    DATATYPE  (SNOBOL4clojure.env/DATATYPE (first args))
    datatype  (SNOBOL4clojure.env/DATATYPE (first args))
    ;; String functions missing from original dispatch
    CHAR      (CHAR      (first args))
    ORD       (when (seq args) (ORD (first args)))
    char      (CHAR      (first args))
    ;; "If i2 is omitted or zero, the remaining characters of the argument string are extracted" (manual ch. 19, SUBSTR)
    SUBSTR    (SUBSTR    (first args) (second args) (nth args 2 ""))
    substr    (SUBSTR    (first args) (second args) (nth args 2 ""))
    DATE      (DATE)
    date      (DATE)
    TIME      (TIME)
    time      (TIME)
    ;; LPAD/RPAD with optional 3rd fill-char arg (functions.clj handles both arities)
    LPAD      (LPAD (nth args 0 "") (nth args 1 "") (nth args 2 ""))   ; "If trailing arguments are omitted, SPITBOL will supply null strings" (manual ch. 4)
    lpad      (LPAD (nth args 0 "") (nth args 1 "") (nth args 2 ""))   ; "If trailing arguments are omitted, SPITBOL will supply null strings" (manual ch. 4)
    RPAD      (RPAD (nth args 0 "") (nth args 1 "") (nth args 2 ""))   ; "If trailing arguments are omitted, SPITBOL will supply null strings" (manual ch. 4)
    rpad      (RPAD (nth args 0 "") (nth args 1 "") (nth args 2 ""))   ; "If trailing arguments are omitted, SPITBOL will supply null strings" (manual ch. 4)
    ;; String comparisons (lexicographic) — were defined by primitive macro but missing here
    LEQ       (LEQ    (first args) (second args))
    leq       (LEQ    (first args) (second args))
    LNE       (LNE    (first args) (second args))
    lne       (LNE    (first args) (second args))
    LLE       (LLE    (first args) (second args))
    lle       (LLE    (first args) (second args))
    LLT       (LLT    (first args) (second args))
    llt       (LLT    (first args) (second args))
    LGE       (LGE    (first args) (second args))
    lge       (LGE    (first args) (second args))
    LGT       (LGT    (first args) (second args))
    lgt       (LGT    (first args) (second args))
    ;; ARG(fname, n) — nth parameter name of function fname (1-based), as a NAME
    ;; (CEO-1861: sbl -bf returns one; the manual's string is not followed)
    ARG       (let [[fname n] args
                    meta (get @<FDEFS> (name-string fname))]
                (when-some [p (when meta (nth (:params meta) (dec (long n)) nil))]
                  (NAME. (symbol p))))
    arg       (apply INVOKE 'ARG args)
    ;; LOCAL(fname, n) — nth local variable name of function fname (1-based), as a NAME
    LOCAL     (let [[fname n] args
                    meta (get @<FDEFS> (name-string fname))]
                (when-some [p (when meta (nth (:locals meta) (dec (long n)) nil))]
                  (NAME. (symbol p))))
    local     (apply INVOKE 'LOCAL args)
    ;; ── Named I/O channel registration (Sprint 25D) ──────────────────────────
    ;; Argument conventions across SNOBOL4 implementations (researched 2026-03-08):
    ;;
    ;;  Original SNOBOL4 (Green Book / mainframe):
    ;;    INPUT('name', unit, length)      — 3 args; length=max record len; filename via JCL
    ;;
    ;;  Catspaw SNOBOL4+ / Minnesota SNOBOL4 / Burks tutorial (de facto modern standard):
    ;;    INPUT('name', unit, length, 'file') — 4 args; length=record len (default 80)
    ;;    INPUT('name', unit,, 'file')        — 4 args, empty length = use default
    ;;
    ;;  CSNOBOL4 / SPITBOL (our primary target — Gimpel/AI-SNOBOL corpora):
    ;;    INPUT('name', unit, options, 'file') — 4 args; arg3 = I/O option string
    ;;    INPUT(.name, unit,, 'file')          — NAME indirect first arg
    ;;    INPUT(.name, unit, 'file')           — 3 args (grammar drops empty slot)
    ;;
    ;;  SITBOL20 / PDP-10:
    ;;    INPUT('name', 'device/file', 'format') — 3 args; NO unit number; arg2 = device
    ;;
    ;; Our strategy: Catspaw/CSNOBOL4/SPITBOL 4-arg form is the primary target.
    ;; arg3 (length/options) is stored but currently ignored (safe for Gimpel corpus).
    ;; Grammar collapses INPUT(.VAR, 5,, 'file') → 3 args by dropping empty arg3 slot.
    ;; We detect file position by arg count: 2→stdin, 3→file=args[2], 4→file=args[3].
    ;; A future &IOCOMPAT keyword can switch arg2=device (SITBOL) vs arg2=unit (modern).
    INPUT     (let [name-arg (first args)
                    unit     (second args)
                    filename (case (count args)
                               2 nil          ; INPUT(.VAR, unit) — stdin
                               3 (nth args 2) ; INPUT(.VAR, unit, 'file')
                               4 (nth args 3) ; INPUT(.VAR, unit, reclen, 'file')
                               nil)
                    var-sym  (cond
                               (symbol? name-arg) name-arg
                               (instance? SNOBOL4clojure.env.NAME name-arg)
                               (symbol (str (.n ^SNOBOL4clojure.env.NAME name-arg)))
                               :else (symbol (str name-arg)))]
                (open-input-channel! var-sym (long (or unit 0)) filename (not= 4 (count args))))
    input     (apply INVOKE 'INPUT args)
    OUTPUT    (let [name-arg (first args)
                    unit     (second args)
                    filename (case (count args)
                               2 nil
                               3 (nth args 2)
                               4 (nth args 3)
                               nil)
                    var-sym  (cond
                               (symbol? name-arg) name-arg
                               (instance? SNOBOL4clojure.env.NAME name-arg)
                               (symbol (str (.n ^SNOBOL4clojure.env.NAME name-arg)))
                               :else (symbol (str name-arg)))]
                (open-output-channel! var-sym (long (or unit 0)) filename (not= 4 (count args))))
    output    (apply INVOKE 'OUTPUT args)
    ENDFILE   (close-channel! (first args))
    endfile   (apply INVOKE 'ENDFILE args)
    DETACH    (let [v (first args)
                    sym (cond
                          (symbol? v) v
                          (instance? SNOBOL4clojure.env.NAME v) (symbol (str (.n ^SNOBOL4clojure.env.NAME v)))
                          :else (symbol (str v)))]
                (close-channel! sym))
    detach    (apply INVOKE 'DETACH args)
    REWIND    (rewind-channel! (first args))
    rewind    (apply INVOKE 'REWIND args)
    ;; CODE(src) — compile and execute a SNOBOL4 source fragment in the current env.
    ;; Returns nil on success; :F branch taken on compile/runtime error.
    ;; Uses resolve to avoid circular dependency (runtime → operators → compiler).
    ;; Sprint 25F.
    CODE      (let [src       (str (first args))
                    compile-f (ns-resolve 'SNOBOL4clojure.compiler 'CODE-block)]
                (when-not compile-f
                  (throw (ex-info "CODE: compiler not loaded"
                                  {:snobol/signal :error})))
                ;; Compile src and load statements into the running label/code tables.
                ;; Returns the entry-point label string (success) or nil (failure).
                ;; Does NOT execute — caller uses a computed goto to invoke it.
                (try
                  (when-some [entry (compile-f src)] (SNOBOL4clojure.env/->CodeBlock entry))
                  (catch Exception e
                    (throw (ex-info (.getMessage e) {:snobol/signal :error})))))
    code      (apply INVOKE 'CODE args)
    quote   ($$ (second op))
            (let [raw-f (if (symbol? op) ($$fn op) op)   ; op is the container itself when it came from a subscript
                  ;; NAME indirect reference — dereference to actual array/table
                  f (if (instance? NAME raw-f)
                      ($$ (symbol (str (.n raw-f))))
                      raw-f)
                  ;; When f is not callable (e.g. result-slot cleared to ε during
                  ;; recursive execution), fall back to the functions registry.
                  f (if (fn? f) f
                      (get @<FUNS> (str op) f))]
              (cond
                (table? f) (table-get f (first args))   ; TABLE subscript read
                (array? f) (array-get f (vec args))     ; ARRAY subscript read
                (fn? f)    (apply f args)
                ;; Nothing callable by this name: ERROR 022, as SPITBOL raises it.
                ;; The IR spells a call F(x) and a subscript A<x> alike, so a
                ;; subscripted non-array reads 022 here where SPITBOL says 235.
                :else      (throw (ex-info "undefined function called"
                                           {:snobol/error 22 :function op})))))))

;; ── APPLY ─────────────────────────────────────────────────────────────────────
(defn APPLY
  "APPLY(fname, arg1, ...) — call the function fname, a string or a NAME, as a
   call written in the program calls it: through INVOKE, built-ins included."
  [fname & fargs]
  (apply INVOKE (symbol (name-string fname)) fargs))

;; ── EVAL! / EVAL ─────────────────────────────────────────────────────────────
;; Unary operators SPITBOL leaves unused, each with the name OPSYN files it
;; under. The emitter spells unary # as sharp.
(def ^:private unused-unary-ops
  {'! "!", '% "%", '/ "/", 'sharp "#", '= "=", '| "|"})

;; What $v refers to: the element a NAME of an element names, as its subscript
;; form (A k ...) with k already evaluated, or else the variable v names.
(defn- indirect-target [v]
  (cond
    (and (instance? NAME v) (list? (.n ^NAME v))) (.n ^NAME v)
    (instance? NAME v)                             (symbol (name-string v))
    ;; "The indirect reference operator ($) converts a string to a variable name" (manual ch. 15); a number converts by its string (ch. 17, Integer to name);
    ;; "The null string cannot be converted to a name" (ch. 17), and an object is no name: "239 Indirection operand is not name" (Appendix D)
    (or (and (string? v) (not= "" v)) (number? v)) (symbol (sstr v))
    :else (throw (ex-info "indirection operand is not name" {:snobol/error 239}))))

(def ^:private pattern-keywords
  {"ABORT" (fn [] ABORT) "ARB" (fn [] SNOBOL4clojure.patterns/ARB) "BAL" (fn [] BAL) "FAIL" (fn [] FAIL) "FENCE" (fn [] (FENCE))
   "REM" (fn [] SNOBOL4clojure.patterns/REM) "SUCCEED" (fn [] SNOBOL4clojure.patterns/SUCCEED)})

(defn- eval-operands
  "Evaluate the operands left to right. The first one that fails (nil) fails the
   whole expression, and the operands after it are not evaluated, so a call among
   them has no effect. Returns the vector of values, or nil."
  [operands]
  (loop [ops (seq operands) vals []]
    (if ops
      (let [v (EVAL! (first ops))]
        (when-not (nil? v) (recur (next ops) (conj vals v))))
      vals)))

;; A call that returned a name (NRETURN) is a NameResult. Used as a value it is the value of the variable or element it names; used as the target of an assignment
;; or a capture it is that variable or element.
(defn- name-value [target]
  (if (symbol? target) ($$ target) (apply INVOKE (first target) (rest target))))

(defn- deref-result [r]
  (if (instance? SNOBOL4clojure.env.NameResult r) (name-value (:target r)) r))

(defn- call-form?
  "True for a form F(args) that calls a function the program DEFINEd (and not a subscript of a table or an array of that name)."
  [X]
  (and (list? X) (symbol? (first X)) (contains? @<FUNS> (name (first X)))
       (not (:data (meta (get @<FUNS> (name (first X))))))   ; a DATA constructor or field is not a DEFINEd function
       (let [v (reference (first X))] (not (or (table? v) (array? v))))))

(defn- name-of-call
  "Call the DEFINEd function of the form F(args) and return the name it returned (a NameResult), or nil."
  [X]
  (when-some [args (eval-operands (rest X))]
    (let [r (apply INVOKE (first X) args)]
      (when (instance? SNOBOL4clojure.env.NameResult r) r))))

;; The target of . $ and @ says where the matcher puts a value, as a function of the value. A plain variable is assigned as a statement assigns it. An element
;; T<I> takes its subscripts now, a variable named by a string ($V) takes its name now, and a deferred expression (*T<I>) takes its subscripts when the assignment
;; is made, so a loop that moves I fills a different element each time.
(defn- assign-to [target v] (INVOKE '= target v))

(defn- element-target [N] (apply list (first N) (map EVAL! (rest N))))

(defn- capture-target [N]
  (cond
    ;; A plain variable is assigned as a statement assigns it: OUTPUT writes the line, an output channel its record, a traced variable fires its trace.
    (symbol? N)
    (fn [x] (assign-to N x))
    (and (list? N) (equal (first N) '$) (clojure.core/= 2 (count N)))
    (when-some [v (EVAL! (second N))]
      (let [t (indirect-target v)] (fn [x] (assign-to t x))))
    ;; *F(args), F a function that returns a name: called when the assignment is made, the value goes to the name it returns
    (and (list? N) (equal (first N) '*) (clojure.core/= 2 (count N)) (call-form? (second N)))
    (let [X (second N)]
      (fn [x] (when-some [r (name-of-call X)] (assign-to (:target r) x))))
    (and (list? N) (equal (first N) '*) (clojure.core/= 2 (count N)))
    (let [X (second N)]
      (fn [x] (assign-to (if (list? X) (element-target X) X) x)))
    (list? N)
    (let [t (element-target N)] (fn [x] (assign-to t x)))
    true
    N))

(defn EVAL! [E]
  (when E
    (cond
      (nil? E)     E
      (char? E)    E
      (float? E)   E
      (string? E)  E
      (integer? E) E
      (symbol? E)  (let [v ($$ E)] (if (identical? v FENCE) (FENCE) v))   ; a bare FENCE is the pattern, not the function that builds one
      ;; (e1, e2, ...): the value of the first expression that succeeds, the later ones not evaluated; failure if none does
      (and (vector? E) (clojure.core/= :comma (first E)))
      (some EVAL! (rest E))
      (vector? E)  (let [evaled (eval-operands E)]
                    ;; nil in any SEQ element = sub-expression failed → whole SEQ fails
                    (when evaled
                      (concat-value (apply list 'SEQ evaled))))
      (list? E)
      (let [[op & parms] E]
        (cond
          ;; A<k> or A[k]: an element of the table or array the name holds; a name that holds neither is "235 Subscripted operand is not table or array" (manual,
          ;; Appendix D), whatever function has that name
          (:subscript (meta E))
          (let [c (if (symbol? op) ($$ op) (EVAL! op))
                c (if (instance? NAME c) ($$ (symbol (str (.n ^NAME c)))) c)]
            (when-some [ks (eval-operands parms)]
              (cond (table? c) (table-get c (first ks))
                    (array? c) (array-get c (vec ks))
                    :else (throw (ex-info "subscripted operand is not table or array" {:snobol/error 235})))))
          ;; The unused unary operators are ERROR 029 until OPSYN defines one.
          (and (clojure.core/= 1 (count parms))
               (contains? unused-unary-ops op)
               (nil? (get @<FUNS> (opsyn-key (unused-unary-ops op) 1))))
          (throw (ex-info "undefined operator referenced" {:snobol/error 29}))
          ;; Unary ?X — interrogation: the null string if X succeeds, failure if not
          (and (equal op '?) (clojure.core/= 1 (count parms)))
          (when-not (nil? (EVAL! (first parms))) ε)
          (equal op '.)
          (let [n (count parms)]
            (if (clojure.core/= n 1)
              ;; Unary .VAR — NAME reference (indirect). Return a NAME wrapping the symbol.
              ;; Do NOT evaluate the arg — we want the symbol itself, not its current value.
              ;; The name of an element, .A<k>, takes its subscripts' values now.
              (let [x (first parms)]
                (NAME. (if (and (list? x) (>= (count x) 2))
                         (apply list (first x) (map EVAL! (rest x)))
                         x)))
              ;; Binary P . V — CAPTURE-COND (conditional assignment on match success).
              (let [[P N] parms] (INVOKE '. (EVAL! P) (capture-target N)))))
          ;; Unary @V — cursor assignment. The matcher assigns the cursor to V by
          ;; name, so the operand is passed as written and not read as a value.
          (and (equal op 'at) (clojure.core/= 1 (count parms)))
          (list 'CURSOR-IMM! (capture-target (first parms)))
          (equal op '$)     (if (clojure.core/= 1 (count parms))
                              ;; Unary $X — the variable X's value names (a string or a NAME)
                              (when-some [v (EVAL! (first parms))]
                                (EVAL! (indirect-target v)))
                              (let [[P N] parms] (INVOKE '$ (EVAL! P) (capture-target N))))
          ;; $X = V — assign the variable X's value names
          (and (equal op '=) (list? (first parms)) (clojure.core/= 2 (count (first parms)))
               (equal (first (first parms)) '$))
          (let [[[_ X] R] parms]
            (when-some [v (EVAL! X)]
              (INVOKE '= (indirect-target v) (EVAL! R))))
          ;; ITEM(container, key...) = v: the element, as container<key...> = v
          (and (equal op '=) (list? (first parms)) (equal (first (first parms)) 'ITEM) (clojure.core/<= 3 (count (first parms))))
          (let [[_ container-expr & key-exprs] (first parms)]
            (when-some [c (EVAL! container-expr)]
              (when-some [ks (eval-operands key-exprs)]
                (when-some [v (EVAL! (second parms))] (INVOKE '= (apply list c ks) v)))))
          ;; F(args) = v, F a function that returns a name: the value goes to the name it returns
          (and (equal op '=) (call-form? (first parms)))
          (when-some [r (name-of-call (first parms))]
            (when-some [v (EVAL! (second parms))] (INVOKE '= (:target r) v)))
          (equal op '=)     (let [[N R]   parms
                                  ;; If N is a subscript call (container key...),
                                  ;; evaluate the keys — EXCEPT for PDD field setters
                                  ;; where the first key must remain a symbol so
                                  ;; snobol-set! can rebind the instance variable.
                                  N' (if (and (list? N) (>= (count N) 2)
                                              (not (clojure.core/= (first N) '&)))
                                       (let [container-sym (first N)
                                             ;; a subscript on a subscript: the container is whatever the inner one names
                                             container-val (if (list? container-sym) (EVAL! container-sym) ($$ container-sym))]
                                         (cond
                                           (nil? container-val) nil
                                           ;; TABLE/ARRAY: evaluate keys
                                           :else (apply list (if (list? container-sym) container-val container-sym) (map EVAL! (rest N)))))
                                       N)]
                              ;; the subject is evaluated before the replacement: an element out of bounds fails the statement and the replacement is never
                              ;; evaluated (sbl -bf: A[3] = F() on a two-element array does not call F)
                              (when (and N' (not (and (list? N') (array? (if (symbol? (first N')) ($$ (first N')) (first N')))
                                                      (not (SNOBOL4clojure.env/array-in-bounds? (if (symbol? (first N')) ($$ (first N')) (first N')) (vec (rest N')))))))
                                (INVOKE '= N' (EVAL! R))))
          ;; "Replacement occurs only if evaluation of the subject, pattern, and replacement succeeded" (manual ch. 14): a pattern that fails to evaluate fails the statement
          (equal op '?=)    (let [[N P R] parms] (when-some [p (EVAL! P)] (INVOKE '?= N p R)))
          ;; the protected keywords &ABORT, &ARB, &BAL, &FAIL, &FENCE, &REM and &SUCCEED hold "the primitive pattern" of their name (manual ch. 16)
          (and (equal op '&) (pattern-keywords (str (first parms))))
          ((pattern-keywords (str (first parms))))
          (equal op '&)     (let [[N]     parms
                                  kw-sym  (symbol (str "&" N))
                                  ;; &-keywords are defs in env.clj; fall back to user ns
                                  v       (or (when-let [vr (ns-resolve (find-ns 'SNOBOL4clojure.env) kw-sym)]
                                                (var-get vr))
                                              ($$ kw-sym))]
                              (if (instance? clojure.lang.IDeref v) @v v))
          (equal op 'SEQ)   (apply str (map #(sstr (EVAL! %)) parms))
          (equal op 'quote) (first parms)
          ;; Unary * = deferred pattern: do NOT pre-evaluate the arg.
          ;; The thunk must re-read the variable at match time so that
          ;; SPAN(*B) / ANY(*B) / NOTANY(*B) see the current value of B,
          ;; not the value B held when the pattern was first assigned.
          (and (equal op '*) (clojure.core/= (count parms) 1))
          (let [sym (first parms)]
            ;; the expression keeps its source in its metadata when that is a variable, which a dump spells *NAME
            (with-meta (list 'DEFER! (fn [] (EVAL! sym))) {:defer-var sym}))
          ;; a subscript on a subscript, tt['blue']['green']: the operator is the inner subscript, whose value is the container
          (list? op) (when-some [container (EVAL! op)] (when-some [args (eval-operands parms)] (apply INVOKE container args)))
          true (let [;; ~X negates success and failure, so it takes a failed operand
                     ;; as its input; any other operator or function fails with it, binary ~ among them, an operator "available for user definition
                     ;; using OPSYN()" (manual ch. 15)
                     args     (if (and (equal op 'tilde) (clojure.core/= 1 (count parms)))
                                (apply vector (map EVAL! parms))
                                (eval-operands parms))
                     ;; OPSYN: an operator may be redefined, under its spelling
                     ;; and arity (opsyn-key). Check before the built-in INVOKE table.
                     opsyn-fn (get @<FUNS> (opsyn-key (get op-spelling op (str op)) (count parms)))]
                 (when args
                   (deref-result
                     (if opsyn-fn
                       (apply opsyn-fn args)
                       (apply INVOKE op args)))))))
      ;; anything else is a value already (a DATA object, an array, a table, a NAME, code) that an IR form carries, as the name of a field .COUNT(A) does
      true E)))

;; The IR of an expression in a string, or nil when the string is not a legal expression (folded to upper case while &CASE is not 0).
(defn- compile-expression [^String s]
  (let [p (parse-expression (if (zero? (long @&CASE)) s (fold-case s)))]
    (when-not (instance? instaparse.gll.Failure p) (first (emitter p)))))

;; An unevaluated expression, the value *X makes: DATATYPE EXPRESSION, evaluated by EVAL.
(defn- deferred? [x] (and (seq? x) (clojure.core/= 'DEFER! (first x)) (fn? (second x))))
(defn- expression-of [ir] (list 'DEFER! (fn [] (EVAL! ir))))

;; EVAL(s) compiles the expression s and evaluates it, failing when s is not a legal expression; EVAL(n) of a number is n; "If the argument is an unevaluated expression,
;; it is evaluated, and the result returned" (manual ch. 19, EVAL). sbl -bf also sets &ERRTEXT and &ERRTYPE (221 and kin) for the syntax error; this engine does not yet.
(defn EVAL [X]
  (cond
    (deferred? X) ((second X))
    (string? X)   (let [ir (compile-expression X)] (when-not (nil? ir) (EVAL! ir)))
    (number? X)   X
    true          (EVAL! X)))
