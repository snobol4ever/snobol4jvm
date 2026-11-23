(ns SNOBOL4clojure.codegen (:require [SNOBOL4clojure.core :as c]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(doseq [s
            '[$$ &TRACE fire-access! bare-fence concat-value deref-result <FUNS> opsyn-key op-spelling INVOKE EVAL! gtstg sstr &MAXLNGTH &ANCHOR <POS> <DISPNO> <STMT-LINE> &LASTNO &STNO &LASTFILE
                 &FILE &LASTLINE &LINE &STCOUNT &STLIMIT &FNCLEVEL &PROFILE &ERRTYPE end-statement! dispatch-special! special-targets protected-variables <CODE> <STNO> <LABL> <DIAGNOSTICS> <ERRSERIAL>
                 block-end compile-error dropped capture-target pattern-argument snobol-set! prim-fns BREAKX$ error-message array? table? trap matching overrun stlimit-over? <ST-DEAD> fire-keyword!
                 fire-label! statement-trace-active? stcount-traced? program-fn fatal-error STNO textless? &ERRLIMIT system-labels failure-point table-get table-subscript array-get array-in-bounds?
                 call-form? reference indirect-target call-by-name name-string unused-unary-ops unused-binary-ops protected-store! pattern-keywords primitive-pattern]]
    (intern *ns* (with-meta s {:private true}) (var-get (ns-resolve 'SNOBOL4clojure.core s))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private core-fence (var-get (ns-resolve 'SNOBOL4clojure.core 'FENCE)))
(deftype Pat [f] Object (toString [_] "PATTERN"))
(defmethod c/DATATYPE "class SNOBOL4clojure.codegen.Pat" [_] "PATTERN")
(def failure (ex-info "failure" {::failure true}))
(defn fail [] (throw failure))
(defn- value [thunk] (try (thunk) (catch clojure.lang.ExceptionInfo x (if (identical? x failure) nil (throw x)))))
(defn- some! [v] (if (nil? v) (fail) v))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- check-concat!
    [vs]
    (let [v (vec (remove #(= "" %) vs)) n (count v)]
        (when
            (> n 1)
            (cond
                (textless? (v (- n 2))) (throw (ex-info "concatenation left operand is not a string or pattern" {:snobol/error 8}))
                (textless? (v (dec n))) (throw (ex-info "concatenation right operand is not a string or pattern" {:snobol/error 9}))
                (some textless? (subvec v 0 (- n 2))) (throw (ex-info "concatenation left operand is not a string or pattern" {:snobol/error 8}))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn constant [n] (let [v (primitive-pattern n)] (if (identical? v core-fence) bare-fence v)))
(defn rd [s] (let [v ($$ s)] (when (pos? (long @&TRACE)) (fire-access! s v)) (some! (if (identical? v core-fence) bare-fence v))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn call
    [op & args]
    (let [p (first args)]
        (if
            (and (instance? Pat p) (= op 'COPY) (nil? (next args)))
            (Pat. (.f ^Pat p))
            (do
                (when (and (= op 'EVAL) (instance? Pat p)) (throw (ex-info "eval argument is not expression" {:snobol/error 103})))
                (when (and (= op 'INPUT) (instance? Pat (nth args 2 nil))) (throw (ex-info "inappropriate file specification for input" {:snobol/error 116})))
                (when (and (= op 'OUTPUT) (instance? Pat (nth args 2 nil))) (throw (ex-info "inappropriate file specification for output" {:snobol/error 160})))
                (let [f (get @<FUNS> (opsyn-key (get op-spelling op (str op)) (count args)))] (some! (deref-result (if f (apply f args) (apply INVOKE op args)))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ev [form] (some! (EVAL! form)))
(defn assign [n r] (some! (INVOKE '= n r)))
(defn protected [n r] (protected-store! n r))
(defn held [s] ($$ s))
(defn invoke [c & args] (some! (apply INVOKE c args)))
(defn operator! [k] (when (nil? (get @<FUNS> k)) (throw (ex-info "undefined operator referenced" {:snobol/error 29}))))
(defn TEXT [& thunks] (if (program-fn 'SEQ) (apply call 'SEQ (map (fn [t] (t)) thunks)) (apply str (map (fn [t] (sstr (value t))) thunks))))
(defn INTERROGATE [v] (if-some [f (get @<FUNS> "?/1")] (some! (deref-result (f v))) ""))
(defn NOT [thunk] (let [v (failure-point (fn [] (value thunk))) f (get @<FUNS> "~/1")] (some! (deref-result (if f (f v) (INVOKE 'tilde-op v))))))
(defn CHOICE [& thunks] (loop [[t & more] thunks] (if more (if-some [v (failure-point (fn [] (value t)))] v (recur more)) (t))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn keyword-value
    [n]
    (some!
        (if-some [p (pattern-keywords n)]
            (p)
            (let [
                  vr (when (seq n) (ns-resolve (the-ns 'SNOBOL4clojure.core) (symbol (str "&" n))))
                  v (if vr (var-get vr) (throw (ex-info "keyword operand is not name of defined keyword" {:snobol/error 251})))]
                (if (instance? clojure.lang.IDeref v) @v v)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn item
    [c & ks]
    (let [c (if (instance? SNOBOL4clojure.core.NAME c) ($$ (symbol (str (.n ^SNOBOL4clojure.core.NAME c)))) c)]
        (some! (cond (table? c) (table-get c (table-subscript ks)) (array? c) (array-get c (vec ks)) :else (throw (ex-info "subscripted operand is not table or array" {:snobol/error 235}))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn indirect [v] (indirect-target v))
(defn INDIRECT [v] (let [t (indirect-target v)] (if (symbol? t) (rd t) (ev t))))
(defn- by-name [r] (if (instance? SNOBOL4clojure.core.NameResult r) (:target r) (fail)))
(defn element [c & ks] (apply list c ks))
(defn element-of [h m & ks] (with-meta (apply list h ks) m))
(defn applied [f & as] (let [fsym (symbol (name-string f)) df (get @<FUNS> (name fsym))] (if (and df (not (:data (meta df)))) (by-name (apply INVOKE fsym as)) (apply list fsym as))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn place
    [c m & ks]
    (let [opf (when (symbol? c) (case (count ks) 1 (when-some [s (unused-unary-ops c)] (get @<FUNS> (opsyn-key s 1))) 2 (when-some [s (unused-binary-ops c)] (get @<FUNS> (opsyn-key s 2))) nil))]
        (cond
            opf (by-name (apply opf ks))
            (and (symbol? c) (not (:subscript m)) (let [v (reference c)] (not (or (table? v) (array? v))))) (let [t (call-by-name c ks)] (if (nil? t) (fail) t))
            :else
                (let [v (if (symbol? c) ($$ c) c) field? (and (symbol? c) (not (:subscript m)) (:data (meta (get @<FUNS> (name c)))))]
                    (if (and (array? v) (not field?) (not (array-in-bounds? v (vec ks)))) (fail) (with-meta (apply list c ks) m))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn named [x] (SNOBOL4clojure.core.NAME. x))
(defn expression [ir] (SNOBOL4clojure.core.ExprVar. ir))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn name-of
    [x & ks]
    (cond
        (call-form? x)
            (let [r (apply INVOKE (first x) ks)]
                (cond (instance? SNOBOL4clojure.core.NameResult r) (named (:target r)) (nil? r) (fail) :else (throw (ex-info "function called by name returned a value" {:snobol/error 21}))))
        (seq ks)
            (let [h (first x) v (when (symbol? h) (reference h))]
                (if
                    (or (table? v) (array? v) (:subscript (meta x)) (not (symbol? h)))
                    (named (cond-> (apply list h ks) (or (table? v) (array? v)) (with-meta {:container v})))
                    (named (some! (call-by-name h ks)))))
        :else (named x)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- pair? [x op] (and (list? x) (= op (first x)) (= 2 (count x))))
(defn pattern? [v] (or (instance? Pat v) (seq? v) (vector? v)))
(defn- store [t v] (if (fn? t) (some? (t v)) (do (snobol-set! t v) true)))
(def ^:private depth (long-array 1))
(declare matcher)
(defn- lit [^String t] (let [n (.length t)] (fn [^String s i caps k] (when (.startsWith s t (int i)) (k (+ (long i) n) caps)))))
(defn- seq-of [ms] (if (empty? ms) (lit "") (reduce (fn [tail p] (fn [s i caps k] (p s i caps (fn [j caps] (tail s j caps k))))) (peek ms) (reverse (pop ms)))))
(defn- alt-of [ms] (fn [s i caps k] (loop [ms (seq ms)] (when ms (or ((first ms) s i caps k) (recur (next ms)))))))
(defn- fence-of [m] (fn [s i caps k] (let [r (m s i caps (fn [j caps] [j caps]))] (cond (nil? r) nil (identical? r ::abort) ::abort :else (k (r 0) (r 1))))))
(defn- arbno-of [m] (fn arbno [s i caps k] (or (k i caps) (m s i caps (fn [j caps] (if (> (long j) (long i)) (arbno s j caps k) (k j caps)))))))
(defn- capture-of [m t] (fn [s i caps k] (m s i caps (fn [j caps] (k j (conj caps [t i j]))))))
(defn- capture-now-of [m t] (fn [^String s i caps k] (m s i caps (fn [j caps] (when (store t (subs s i j)) (k j caps))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- defer-of
    [thunk]
    (let [^longs depth depth]
        (fn
            [s i caps k]
            (let [p (if (fn? thunk) (value thunk) thunk)]
                (when (or (array? p) (table? p) (instance? SNOBOL4clojure.core.DataObj p)) (throw (ex-info (error-message 46) {:snobol/error 46})))
                (when
                    (some? p)
                    (let [d (aget depth 0)]
                        (when (> d 200000) (throw (ex-info (error-message 246) {:snobol/error 246 :snobol/abort true})))
                        (aset depth 0 (inc d))
                        (let [r ((matcher p) s i caps (fn [j caps] (aset depth 0 d) (let [r (k j caps)] (aset depth 0 (inc d)) r)))] (aset depth 0 d) r)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private arb (fn [^String s i caps k] (let [len (.length s)] (loop [j (long i)] (when (<= j len) (or (k j caps) (recur (inc j))))))))
(def ^:private rem-all (fn [^String s i caps k] (k (.length s) caps)))
(def ^:private succeed (fn [s i caps k] (loop [] (or (k i caps) (recur)))))
(def ^:private bare (fn [s i caps k] (or (k i caps) ::abort)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private bal
    (fn
        [^String s i caps k]
        (let [len (.length s)]
            (loop [p (long i) n 0]
                (when
                    (< p len)
                    (let [ch (.charAt s (int p)) n2 (long (case ch \( (inc n) \) (dec n) n))] (cond (neg? n2) nil (zero? n2) (or (k (inc p) caps) (recur (inc p) 0)) :else (recur (inc p) n2))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- breakx-of
    [cs]
    (fn
        [^String s i caps k]
        (let [len (.length s)] (loop [from (long i)] (when (<= from len) (let [[_ d] (BREAKX$ (seq (subs s from)) from cs)] (when (>= (long d) 0) (or (k d caps) (recur (inc (long d)))))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- node
    [p]
    (let [head (cond (vector? p) 'SEQ (seq? p) (first p) :else p) args (cond (vector? p) (seq p) (seq? p) (rest p))]
        (case head
            SEQ (seq-of (mapv matcher args))
            ALT (alt-of (mapv matcher args))
            LIT$ (lit (let [x (first args)] (if (string? x) x (sstr x))))
            (ANY$ NOTANY$ SPAN$ NSPAN$ BREAK$ BREAKX$ POS# RPOS# LEN# TAB# RTAB# BOL# EOL#)
                (let [f (prim-fns head) a (first args)] (fn [^String s i caps k] (let [[_ d] (f (seq (subs s i)) i a)] (when (>= (long d) 0) (k d caps)))))
            SUCCEED! succeed
            FAIL! (fn [s i caps k] nil)
            ABORT! (fn [s i caps k] ::abort)
            REM! rem-all
            ARB! arb
            BAL! bal
            FENCE! (if (some? (first args)) (fence-of (matcher (first args))) bare)
            ARBNO! (arbno-of (matcher (first args)))
            BREAKX# (breakx-of (first args))
            CAPTURE-COND (capture-of (matcher (first args)) (second args))
            (CAPTURE-IMM CAPTURE) (capture-now-of (matcher (first args)) (second args))
            CURSOR-IMM! (let [t (first args)] (fn [s i caps k] (when (store t i) (k i caps))))
            DEFER! (defer-of (first args))
            X (fn [s i caps k] ((matcher ($$ 'X)) s i caps k))
            (throw (ex-info "a pattern node the compiled matcher does not know" {::unsupported (str head)})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- matcher
    [v]
    (cond
        (instance? Pat v) (.f ^Pat v)
        (string? v) (lit v)
        (or (seq? v) (vector? v) (symbol? v)) (node v)
        (or (number? v) (and (instance? SNOBOL4clojure.core.NAME v) (not (textless? v)))) (lit (sstr v))
        :else (let [p (pattern-argument v 240)] (if (identical? p v) (throw (ex-info "a value the compiled matcher does not know" {::unsupported (str (type v))})) (matcher p)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SEQ
    [& vs]
    (if (not-any? pattern? vs) (some! (concat-value (apply list 'SEQ vs))) (let [parts (remove #(= "" %) vs)] (check-concat! vs) (if (next parts) (Pat. (seq-of (mapv matcher parts))) (first parts)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ALT
    [& vs]
    (let [v (vec vs) n (count v)]
        (if-some [f (when (= n 2) (get @<FUNS> "|/2"))]
            (some! (deref-result (apply f v)))
            (do
                (when-not (pattern? (v (dec n))) (pattern-argument (v (dec n)) 5))
                (doseq [i (range (- n 2) -1 -1)] (when-not (pattern? (v i)) (pattern-argument (v i) 6)))
                (Pat. (alt-of (mapv matcher v)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn DEFER
    [x]
    (if-some [f (get @<FUNS> "*/1")]
        (some! (deref-result (f (if (symbol? x) (rd x) (x)))))
        (if (symbol? x) (with-meta (list 'DEFER! (fn [] (value #(rd x)))) {:defer-var x}) (list 'DEFER! (fn [] (value x))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn target [n] (some! (capture-target n)))
(defn CAPTURE [p n] (if-some [f (get @<FUNS> "./2")] (some! (deref-result (f p (ev n)))) (Pat. (capture-of (matcher (pattern-argument p 30)) (target n)))))
(defn CAPTURE-NOW [p n] (if-some [f (get @<FUNS> "$/2")] (some! (deref-result (f p (ev n)))) (Pat. (capture-now-of (matcher (pattern-argument p 25)) (target n)))))
(defn CURSOR [t] (Pat. (fn [s i caps k] (when (store t i) (k i caps)))))
(defn- redefinable [op f] (let [k (str op "/1")] (fn [x] (if (or (get @<FUNS> k) (program-fn op)) (call op x) (f x)))))
(def FENCE (redefinable 'FENCE (fn [p] (Pat. (fence-of (matcher (pattern-argument p 259)))))))
(def ARBNO (redefinable 'ARBNO (fn [p] (Pat. (arbno-of (matcher (pattern-argument p 61)))))))
(defn- runtime-built [op arg] (some! (INVOKE op arg)))
(defn- positional [op f] (redefinable op (fn [n] (if (and (integer? n) (<= 0 (long n) 16777216)) (Pat. (f (long n))) (runtime-built op n)))))
(def POS (positional 'POS (fn [n] (fn [s i caps k] (when (== (long i) n) (k i caps))))))
(def RPOS (positional 'RPOS (fn [n] (fn [^String s i caps k] (when (== (- (.length s) (long i)) n) (k i caps))))))
(def LEN (positional 'LEN (fn [n] (fn [^String s i caps k] (let [j (+ (long i) n)] (when (<= j (.length s)) (k j caps)))))))
(def TAB (positional 'TAB (fn [n] (fn [^String s i caps k] (when (and (<= (long i) n) (<= n (.length s))) (k n caps))))))
(def RTAB (positional 'RTAB (fn [n] (fn [^String s i caps k] (let [j (- (.length s) n)] (when (<= (long i) j) (k j caps)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- char-class
    [op f]
    (redefinable op (fn [cs] (if (and (string? cs) (pos? (count cs))) (Pat. (f (fn [^String s j] (>= (.indexOf ^String cs (int (.charAt s (int j)))) 0)))) (runtime-built op cs)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ANY (char-class 'ANY (fn [in?] (fn [^String s i caps k] (when (and (< (long i) (.length s)) (in? s i)) (k (inc (long i)) caps))))))
(def NOTANY (char-class 'NOTANY (fn [in?] (fn [^String s i caps k] (when (and (< (long i) (.length s)) (not (in? s i))) (k (inc (long i)) caps))))))
(def SPAN (char-class 'SPAN (fn [in?] (fn [^String s i caps k] (let [len (.length s) j (loop [j (long i)] (if (and (< j len) (in? s j)) (recur (inc j)) j))] (when (> j (long i)) (k j caps)))))))
(def BREAK (char-class 'BREAK (fn [in?] (fn [^String s i caps k] (let [len (.length s) j (loop [j (long i)] (if (and (< j len) (not (in? s j))) (recur (inc j)) j))] (when (< j len) (k j caps)))))))
(def BREAKX (redefinable 'BREAKX (fn [cs] (runtime-built 'BREAKX cs))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- find-match
    [^String s p]
    (let [^longs depth depth d0 (aget depth 0)]
        (aset depth 0 0)
        (try
            (matching
                (fn
                    []
                    (let [m (matcher p) len (.length s) n (if (zero? (long @&ANCHOR)) len 0)]
                        (loop [st 0]
                            (when
                                (<= st n)
                                (let [r (m s st [] (fn [j caps] [j caps]))]
                                    (cond (identical? r ::abort) nil r (when (every? (fn [[t a b]] (store t (subs s a b))) (r 1)) [st (r 0)]) :else (recur (inc st)))))))))
            (catch StackOverflowError _ (throw (ex-info (error-message 246) {:snobol/error 246 :snobol/abort true})))
            (finally (aset depth 0 d0)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn MATCH [sv p] (if-some [f (get @<FUNS> "?/2")] (some! (deref-result (f sv p))) (let [s (gtstg sv 241) r (find-match s p)] (if r (subs s (r 0) (r 1)) (fail)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn REPLACE
    [n p replacement]
    (let [s (gtstg (if (list? n) (apply INVOKE (or (:container (meta n)) (first n)) (rest n)) (rd n)) 241) r (find-match s p)]
        (if r
            (let [result (str (subs s 0 (r 0)) (sstr (value replacement)) (subs s (r 1)))]
                (when (> (count result) (long @&MAXLNGTH)) (throw (ex-info "string length exceeds value of maxlngth keyword" {:snobol/error 205})))
                (assign n result))
            (fail))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn step!
    [n]
    (reset! &LASTNO @&STNO)
    (reset! &LASTFILE @&FILE)
    (reset! &LASTLINE @&LINE)
    (when-some [[f l] (get @<POS> n)] (reset! &FILE f) (reset! &LINE l) (reset! <STMT-LINE> l))
    (reset! &STNO (get @<DISPNO> n n))
    (let [cnt (if (neg? (long @&STLIMIT)) @&STCOUNT (swap! &STCOUNT inc)) bump (if (or (stcount-traced?) (not (zero? (long @&PROFILE)))) 1 0) over (when (stlimit-over? cnt bump) (overrun cnt))]
        (when
            (pos? (long @&TRACE))
            (when (and (not over) (not (neg? (long @&STLIMIT)))) (fire-keyword! "STCOUNT" cnt))
            (when (statement-trace-active?) (binding [*out* *err*] (println (format ">>> stmt %5d  %s" (dec (long cnt)) (str (or (@<LABL> n) n)))))))
        over))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- goto-of [n] (let [[x y] (@<CODE> (or (@<LABL> n) n))] (if (map? x) x y)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- pc-of
    [at]
    (cond
        (vector? at) (first at)
        (integer? at) at
        (keyword? at) (@<STNO> at)
        (symbol? at) (@<STNO> (keyword (name at)))
        (string? at) (or (@<STNO> (keyword at)) (when (re-matches #"[0-9]+" at) (Long/parseLong at)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- current-error? [err] (and err (= (:snobol/serial (ex-data (:ex err))) @<ERRSERIAL>)))
(defn goto ([label] (goto label (@<STNO> label))) ([label pc] (when (pos? (long @&TRACE)) (fire-label! label)) pc))
(defn- land [pc err] (ex-info "jump" {::land [pc err]}))
(defn- end-pc [] (or (@<STNO> :END) (@<STNO> :end) (inc (long @STNO))))
(defn computed [v] (keyword (str v)))
(defn direct [v] (if (instance? SNOBOL4clojure.core.CodeBlock v) (.entry ^SNOBOL4clojure.core.CodeBlock v) (throw (ex-info "goto operand in direct goto is not code" {:snobol/error 24}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- reached
    [r]
    (cond
        (instance? SNOBOL4clojure.core.NameResult r) (let [t (:target r)] (if (symbol? t) (keyword (name t)) t))
        (nil? r) (throw (ex-info "goto evaluation failure" {:snobol/error 20}))
        :else (throw (ex-info "function called by name returned a value" {:snobol/error 21}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- reached-by [f args] (when (some nil? args) (throw (ex-info "goto evaluation failure" {:snobol/error 20}))) (reached (apply INVOKE f args)))
(defn called [f & thunks] (reached-by f (mapv value thunks)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- computed-target
    [t]
    (cond
        (pair? t '$) (when-some [v (EVAL! (second t))] (computed v))
        (and (list? t) (= 'DIRECT (first t))) (when-some [v (EVAL! (second t))] (direct v))
        (and (list? t) (symbol? (first t)) (not (:subscript (meta t)))) (reached-by (first t) (mapv EVAL! (rest t)))
        :else t))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- exit
    [n tgt err]
    (let [thunk? (fn? tgt) tgt (if thunk? (value tgt) tgt) nm (when (or (keyword? tgt) (string? tgt) (symbol? tgt)) (name tgt))]
        (cond
            (and thunk? (nil? tgt)) [(inc (long n)) err]
            (and thunk? (integer? tgt)) [(goto (or (@<LABL> tgt) tgt) tgt) err]
            (integer? tgt) [tgt err]
            (= nm "CONTINUE")
                (if
                    (current-error? err)
                    (let [g (:goto err) f (when (map? g) (or (:G g) (:F g)))]
                        (if
                            (and (:in-goto err) f (not (keyword? f)))
                            (throw (let [e (fatal-error 332)] (ex-info (ex-message e) (assoc (ex-data e) :SNOBOL4clojure.core/continued err))))
                            (let [t (computed-target f)]
                                (if t
                                    (do (when (special-targets t) (dispatch-special! t)) [(or (pc-of t) (throw (ex-info "goto undefined label" {:snobol/error 38}))) nil])
                                    [(inc (long (pc-of (:at err)))) nil]))))
                    (throw (fatal-error 37)))
            (= nm "ABORT")
                (if err
                    (let [e ^clojure.lang.ExceptionInfo (:ex err)]
                        (throw (ex-info (ex-message e) (-> (ex-data e) (dissoc :snobol/label :snobol/serial :snobol/fp :snobol/resume) (assoc :snobol/abort true)))))
                    (let [k (long @&ERRTYPE)] (throw (if (zero? k) (ex-info (error-message 36) {:snobol/error 36}) (ex-info (error-message k) {:snobol/error k :snobol/abort true})))))
            (= nm "SCONTINUE") (throw (fatal-error (if (and err (or (and (:resume err) (= (:resume err) @<ERRSERIAL>)) (current-error? err))) 331 321)))
            (#{"END" "end"} nm) (if (@<STNO> (keyword nm)) [(end-pc) err] (throw (ex-info "END" {:snobol/signal :end :snobol/closing true})))
            (special-targets tgt) (dispatch-special! tgt)
            :else (let [pc (or (pc-of tgt) (throw (ex-info "goto undefined label" {:snobol/error 38})))] [(goto tgt pc) err]))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare follow)
(defn- enter [label err n] (if (system-labels (name label)) (let [t (keyword (name label))] (follow n true err t t)) [(pc-of label) err]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- follow
    [n ok err s f]
    (loop [ok ok]
        (let [r
                  (try
                      (exit n (if ok s f) err)
                      (catch clojure.lang.ExceptionInfo e
                          (let [t (trap e)]
                              (cond
                                  (false? t) ::fail
                                  (instance? SNOBOL4clojure.core.Transfer t) [(pc-of (:at t)) (:err t)]
                                  :else
                                      (enter
                                          (:label t)
                                          (if-let [e0 (:SNOBOL4clojure.core/continued (ex-data (:ex t)))] (assoc e0 :ex (:ex t)) {:at n :goto (goto-of n) :ex (:ex t) :in-goto true})
                                          n)))))]
            (if (identical? ::fail r) (recur false) r))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn leave ([n ok err t] (leave n ok err t t)) ([n ok err s f] (let [[pc e] (follow n ok err s f)] (if (and pc (identical? e err)) pc (throw (land pc e))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn trapped
    [n x]
    (let [t (trap x)]
        (cond
            (false? t) false
            (instance? SNOBOL4clojure.core.Transfer t) (throw (land (pc-of (:at t)) (:err t)))
            :else (let [[pc e] (enter (:label t) {:at n :goto (goto-of n) :ex (:ex t)} n)] (throw (land pc e))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defmacro stmt ([n] `(stmt ~n true)) ([n e] `(if-some [over# (step! ~n)] (trapped ~n over#) (try ~e true (catch clojure.lang.ExceptionInfo x# (if (identical? x# failure) false (trapped ~n x#)))))))
(defn landing [^clojure.lang.ExceptionInfo x] (if-some [l (::land (ex-data x))] (when (first l) l) (throw x)))
(defn- start! [] (when (zero? (long @&FNCLEVEL)) (reset! &STCOUNT 0) (reset! <ST-DEAD> nil)))
(defn end! [] (if (pos? (long @&FNCLEVEL)) (dispatch-special! :END) (do (end-statement!) nil)))
(defn closing! [] (throw (ex-info "END" {:snobol/signal :end :snobol/closing true})))
(def ^:private fallbacks (atom 0))
(def ^:private fns (atom []))
(def ^:private tally (atom {:statements 0 :native 0 :code []}))
(def ^:private chunk-size 50)
(defn- lit? [x] (or (string? x) (integer? x) (float? x)))
(def ^:private pattern-functions '#{POS RPOS LEN TAB RTAB ANY NOTANY SPAN BREAK BREAKX ARBNO FENCE})
(def ^:private special-heads '#{. $ = ?= & SEQ pattern-constant})
(defn- fallback [e] (swap! fallbacks inc) (list 'ev (list 'quote e)))
(defn- q [x] (list 'quote x))
(defn- unused? [op n] (or (and (= n 1) (unused-unary-ops op)) (and (= n 2) (unused-binary-ops op))))
(declare gen)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen-place
    [t]
    (cond
        (not (list? t)) (q t)
        (pair? t '$) (list 'indirect (gen (second t)))
        (and (= 'ITEM (first t)) (<= 3 (count t))) (cons 'element (map gen (rest t)))
        (and (= 'APPLY (first t)) (<= 2 (count t))) (cons 'applied (map gen (rest t)))
        (pair? t '&) (q t)
        :else (list* 'place (if (symbol? (first t)) (q (first t)) (gen (first t))) (meta t) (map gen (rest t)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen-subject [t] (cond (pair? t '$) (list 'indirect (gen (second t))) (and (list? t) (not= '& (first t))) (list* 'element-of (q (first t)) (meta t) (map gen (rest t))) :else (q t)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen-name
    [x]
    (cond
        (pair? x '&) (list 'named (q x))
        (pair? x '$) (list 'named (list 'indirect (gen (second x))))
        (pair? x '*) (list 'named (if (symbol? (second x)) (q (second x)) (list 'expression (q (second x)))))
        (list? x) (list* 'name-of (q x) (map gen (rest x)))
        :else (list 'named (q x))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen
    [e]
    (cond
        (lit? e) e
        (symbol? e) (list 'rd (q e))
        (and (vector? e) (= :comma (first e))) (cons 'CHOICE (map (fn [a] (list 'fn [] (gen a))) (rest e)))
        (vector? e) (cons 'SEQ (map gen e))
        (pair? e 'pattern-constant) (list 'constant (q (second e)))
        (and (list? e) (:subscript (meta e))) (list* 'item (if (symbol? (first e)) (list 'held (q (first e))) (gen (first e))) (map gen (rest e)))
        (and (list? e) (symbol? (first e)))
            (let [[op & args] e n (count args)]
                (cond
                    (unused? op n) (list 'do (list 'operator! (opsyn-key (unused? op n) n)) (list* 'call (q op) (map gen args)))
                    (and (= op '?) (= n 1)) (list 'INTERROGATE (gen (first args)))
                    (and (= op '.) (= n 1)) (gen-name (first args))
                    (and (= op '.) (= n 2)) (list 'CAPTURE (gen (first args)) (q (second args)))
                    (and (= op 'at-op) (= n 1)) (list 'CURSOR (list 'target (q (first args))))
                    (and (= op '$) (= n 1)) (list 'INDIRECT (gen (first args)))
                    (and (= op '$) (= n 2)) (list 'CAPTURE-NOW (gen (first args)) (q (second args)))
                    (and (= op '=) (= n 2) (symbol? (first args)) (protected-variables (first args))) (list 'protected (q (first args)) (gen (second args)))
                    (and (= op '=) (= n 2)) (list 'assign (gen-place (first args)) (gen (second args)))
                    (and (= op '?=) (= n 3)) (let [[t p r] args] (list 'REPLACE (gen-subject t) (gen p) (list 'fn [] (if (some? r) (gen r) ""))))
                    (and (= op '&) (= n 1)) (list 'keyword-value (str (first args)))
                    (and (= op '*) (= n 1)) (list 'DEFER (if (symbol? (first args)) (q (first args)) (list 'fn [] (gen (first args)))))
                    (and (= op 'tilde-op) (= n 1)) (list 'NOT (list 'fn [] (gen (first args))))
                    (and (= op '|) (>= n 2)) (cons 'ALT (map gen args))
                    (and (pattern-functions op) (= n 1)) (list op (gen (first args)))
                    (and (= op '?) (= n 2)) (list 'MATCH (gen (first args)) (gen (second args)))
                    (= op 'SEQ) (cons 'TEXT (map (fn [a] (list 'fn [] (gen a))) args))
                    (special-heads op) (fallback e)
                    :else (list* 'call (q op) (map gen args))))
        (and (list? e) (list? (first e))) (list* 'invoke (gen (first e)) (map gen (rest e)))
        :else (fallback e)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen-goto
    [t]
    (cond
        (pair? t '$) (list 'fn [] (list 'computed (gen (second t))))
        (and (list? t) (= 'DIRECT (first t))) (list 'fn [] (list 'direct (gen (second t))))
        (and (list? t) (symbol? (first t)) (not (:subscript (meta t)))) (list 'fn [] (list* 'called (q (first t)) (map (fn [a] (list 'fn [] (gen a))) (rest t))))
        :else (q t)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- generate
    [a b plain?]
    (let [
          end (end-pc)
          ending? (fn [t] (and (#{:END :end} t) (some? (@<STNO> t))))
          system? (fn [t] (and (keyword? t) (or (special-targets t) (#{"CONTINUE" "ABORT" "SCONTINUE"} (name t)))))
          static? (fn [t] (or (nil? t) (ending? t) (and (keyword? t) (not (system? t)) (some? (@<STNO> t)))))
          jump (fn [t n] (cond (nil? t) (list 'recur (inc n)) (ending? t) (list 'recur end) :else (list 'recur (list 'goto t))))
          exit-of (fn [t n] (cond (nil? t) (inc n) (ending? t) end (keyword? t) t :else (gen-goto t)))
          native (atom [0 0])
          clauses
              (doall
                  (mapcat
                      (fn
                          [n]
                          (let [stmt (@<CODE> (or (@<LABL> n) n))]
                              (cond
                                  (= n end) [n '(end!)]
                                  (nil? stmt) [n nil]
                                  (identical? stmt block-end) [n (list 'do (list 'step! n) '(closing!))]
                                  (identical? stmt dropped) [n (list 'recur (inc n))]
                                  (identical? stmt compile-error) [n (list 'do (list 'stmt n (list 'throw (list 'ex-info (error-message 7) {:snobol/error 7}))) (list 'recur (inc n)))]
                                  :else
                                      (let [
                                            [x y] stmt
                                            body (if (map? x) y x)
                                            goto (if (map? x) x y)
                                            g (when (map? goto) goto)
                                            before @fallbacks
                                            code (when (some? body) (if plain? (fallback body) (gen body)))
                                            s (or (:G g) (:S g))
                                            f (or (:G g) (:F g))
                                            head (if (nil? code) (list 'stmt n) (list 'stmt n code))
                                            arm
                                                (cond
                                                    (and (static? s) (static? f)) (if (= s f) (list 'do head (jump s n)) (list 'if head (jump s n) (jump f n)))
                                                    (= s f) (list 'recur (list 'leave n head 'err (exit-of s n)))
                                                    :else (list 'recur (list 'leave n head 'err (exit-of s n) (exit-of f n))))]
                                          (swap! native (fn [[k m]] [(if (= before @fallbacks) (inc (long k)) k) (inc (long m))]))
                                          [n arm]))))
                      (range a (inc b))))]
        (with-meta
            (list 'fn '[pc err] (list 'try (list 'loop '[pc pc] (concat (list 'case '(int pc)) clauses ['[pc err]])) '(catch clojure.lang.ExceptionInfo x (landing x))))
            {:native (@native 0) :statements (@native 1)})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- install!
    [a b]
    (let [made
              (fn
                  [plain?]
                  (try
                      (let [code (generate a b plain?)] [code (binding [*ns* (the-ns 'SNOBOL4clojure.codegen)] (eval code))])
                      (catch clojure.lang.Compiler$CompilerException e (when plain? (throw e)))))]
        (if-some [[code f] (or (made false) (when (= a b) (made true)))]
            (do
                (swap! fns (fn [v] (reduce (fn [v n] (assoc v n f)) (into v (repeat (max 0 (- (inc (long b)) (count v))) nil)) (range a (inc (long b))))))
                (swap! tally (fn [t] (-> t (update :statements + (:statements (meta code))) (update :native + (:native (meta code))) (update :code conj code)))))
            (let [m (quot (+ (long a) (long b)) 2)] (install! a m) (install! (inc m) b)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- top [] (max (long @STNO) (long (end-pc))))
(defn- phantom [pc err] (try (stmt pc (throw (ex-info (error-message 7) {:snobol/error 7}))) [(inc (long pc)) err] (catch clojure.lang.ExceptionInfo x (landing x))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- code-at
    [pc]
    (when
        (integer? pc)
        (or
            (get @fns pc)
            (cond
                (<= 1 (long pc) (top))
                    (let [a0 (inc (* (long chunk-size) (quot (dec (long pc)) (long chunk-size))))]
                        (install! (first (remove #(get @fns %) (range a0 (inc (long pc))))) (min (top) (+ a0 (dec (long chunk-size)))))
                        (get @fns pc))
                (identical? compile-error (@<CODE> pc)) phantom))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- drive [pc] (start!) (loop [pc pc err nil] (when-some [f (code-at pc)] (when-some [[pc err] (f pc err)] (recur pc err)))))
(defn- fresh! [] (reset! fallbacks 0) (reset! fns []) (reset! tally {:statements 0 :native 0 :code []}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- run-from
    [entry]
    (let [interpret c/RUN]
        (with-redefs [c/RUN (fn [at] (if-some [pc (pc-of at)] (drive pc) (interpret at)))]
            (try
                (drive (or (pc-of entry) 1))
                (catch clojure.lang.ExceptionInfo e
                    (if
                        (= :end (:snobol/signal (ex-data e)))
                        (do (when-some [l (:fnclevel (ex-data e))] (reset! &FNCLEVEL l)) (when-not (:snobol/closing (ex-data e)) (end-statement!)) nil)
                        (throw e)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-loaded [entry] (fresh!) (run-from entry))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn compile-source
    [src]
    (c/GLOBALS)
    (fresh!)
    (let [entry (c/CODE src)]
        (if
            (seq @<DIAGNOSTICS>)
            {:refused "compile errors"}
            (do
                (doseq [a (range 1 (inc (top)) chunk-size)] (install! a (min (top) (+ (long a) (dec (long chunk-size))))))
                (let [t @tally] {:entry entry :code (:code t) :statements (:statements t) :native (:native t) :fallback (- (long (:statements t)) (long (:native t)))})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-compiled [{:keys [entry]}] (run-from entry))
