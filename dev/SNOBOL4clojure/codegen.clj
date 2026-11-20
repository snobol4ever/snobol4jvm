(ns SNOBOL4clojure.codegen (:require [SNOBOL4clojure.core :as c] [clojure.pprint :as pp]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(doseq [s
            '[$$ &TRACE fire-access! bare-fence concat-value deref-result <FUNS> opsyn-key op-spelling INVOKE EVAL! gtstg sstr &MAXLNGTH &ANCHOR <POS> <DISPNO> <STMT-LINE> &LASTNO &STNO &LASTFILE
                 &FILE &LASTLINE &LINE &STCOUNT &STLIMIT &FNCLEVEL &PROFILE &ERRTYPE end-statement! dispatch-special! special-targets protected-variables <CODE> <STNO> <LABL> <DIAGNOSTICS> <ERRSERIAL>
                 block-end compile-error capture-target pattern-argument snobol-set! prim-fns BREAKX$ error-message array? table? trap matching overrun fire-keyword! fire-label!
                 statement-trace-active? stcount-traced? fatal-error STNO]]
    (intern *ns* (with-meta s {:private true}) (var-get (ns-resolve 'SNOBOL4clojure.core s))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private core-fence (var-get (ns-resolve 'SNOBOL4clojure.core 'FENCE)))
(deftype Pat [f])
(deftype Expr [thunk])
(defmethod c/DATATYPE "class SNOBOL4clojure.codegen.Pat" [_] "PATTERN")
(defmethod c/DATATYPE "class SNOBOL4clojure.codegen.Expr" [_] "EXPRESSION")
(def failure (ex-info "failure" {::failure true}))
(defn fail [] (throw failure))
(defn- value [thunk] (try (thunk) (catch clojure.lang.ExceptionInfo x (if (identical? x failure) nil (throw x)))))
(defn- some! [v] (if (nil? v) (fail) v))
(defn rd [s] (let [v ($$ s)] (when (pos? (long @&TRACE)) (fire-access! s v)) (some! (if (identical? v core-fence) bare-fence v))))
(defn call [op & args] (let [f (get @<FUNS> (opsyn-key (get op-spelling op (str op)) (count args)))] (some! (deref-result (if f (apply f args) (apply INVOKE op args))))))
(defn ev [form] (some! (EVAL! form)))
(defn assign [n r] (some! (INVOKE '= n r)))
(defn pattern? [v] (or (instance? Pat v) (instance? Expr v)))
(defn- store [t v] (if (fn? t) (some? (t v)) (do (snobol-set! t v) true)))
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
    (fn
        [s i caps k]
        (let [p (if (fn? thunk) (value thunk) thunk)] (when (or (array? p) (table? p)) (throw (ex-info (error-message 46) {:snobol/error 46}))) (when (some? p) ((matcher p) s i caps k)))))
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
        (instance? Expr v) (defer-of (.thunk ^Expr v))
        (or (seq? v) (vector? v) (symbol? v)) (node v)
        (or (number? v) (instance? SNOBOL4clojure.core.NAME v)) (lit (sstr v))
        :else (let [p (pattern-argument v 240)] (if (identical? p v) (throw (ex-info "a value the compiled matcher does not know" {::unsupported (str (type v))})) (matcher p)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SEQ [& vs] (if (not-any? pattern? vs) (some! (concat-value (apply list 'SEQ vs))) (Pat. (seq-of (mapv matcher (remove #(= "" %) vs))))))
(defn ALT [& vs] (Pat. (alt-of (mapv matcher vs))))
(defn DEFER [thunk] (Expr. thunk))
(defn target [n] (some! (capture-target n)))
(defn CAPTURE [p t] (Pat. (capture-of (matcher p) t)))
(defn CAPTURE-NOW [p t] (Pat. (capture-now-of (matcher p) t)))
(defn CURSOR [t] (Pat. (fn [s i caps k] (when (store t i) (k i caps)))))
(def ARB (Pat. arb))
(def REM (Pat. rem-all))
(def BAL (Pat. bal))
(def FAIL (Pat. (fn [s i caps k] nil)))
(def SUCCEED (Pat. succeed))
(def ABORT (Pat. (fn [s i caps k] ::abort)))
(defn FENCE ([] (Pat. bare)) ([p] (Pat. (fence-of (matcher p)))))
(defn ARBNO [p] (Pat. (arbno-of (matcher p))))
(defn- late [build thunk] (Pat. (fn [s i caps k] (when-some [v (value thunk)] ((matcher (build v)) s i caps k)))))
(defn- positional [op f] (fn build [n] (cond (instance? Expr n) (late build (.thunk ^Expr n)) (and (integer? n) (>= (long n) 0)) (Pat. (f (long n))) :else (some! (INVOKE op n)))))
(def POS (positional 'POS (fn [n] (fn [s i caps k] (when (== (long i) n) (k i caps))))))
(def RPOS (positional 'RPOS (fn [n] (fn [^String s i caps k] (when (== (- (.length s) (long i)) n) (k i caps))))))
(def LEN (positional 'LEN (fn [n] (fn [^String s i caps k] (let [j (+ (long i) n)] (when (<= j (.length s)) (k j caps)))))))
(def TAB (positional 'TAB (fn [n] (fn [^String s i caps k] (when (and (<= (long i) n) (<= n (.length s))) (k n caps))))))
(def RTAB (positional 'RTAB (fn [n] (fn [^String s i caps k] (let [j (- (.length s) n)] (when (<= (long i) j) (k j caps)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- char-class
    [op f]
    (fn build
        [cs]
        (cond
            (instance? Expr cs) (late build (.thunk ^Expr cs))
            (and (string? cs) (pos? (count cs))) (Pat. (f (fn [^String s j] (>= (.indexOf ^String cs (int (.charAt s (int j)))) 0))))
            :else (some! (INVOKE op cs)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ANY (char-class 'ANY (fn [in?] (fn [^String s i caps k] (when (and (< (long i) (.length s)) (in? s i)) (k (inc (long i)) caps))))))
(def NOTANY (char-class 'NOTANY (fn [in?] (fn [^String s i caps k] (when (and (< (long i) (.length s)) (not (in? s i))) (k (inc (long i)) caps))))))
(def SPAN (char-class 'SPAN (fn [in?] (fn [^String s i caps k] (let [len (.length s) j (loop [j (long i)] (if (and (< j len) (in? s j)) (recur (inc j)) j))] (when (> j (long i)) (k j caps)))))))
(def BREAK (char-class 'BREAK (fn [in?] (fn [^String s i caps k] (let [len (.length s) j (loop [j (long i)] (if (and (< j len) (not (in? s j))) (recur (inc j)) j))] (when (< j len) (k j caps)))))))
(def BREAKX (fn build [cs] (if (instance? Expr cs) (late build (.thunk ^Expr cs)) (some! (INVOKE 'BREAKX cs)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- find-match
    [^String s p]
    (matching
        (fn
            []
            (let [m (matcher p) len (.length s) n (if (zero? (long @&ANCHOR)) len 0)]
                (loop [st 0]
                    (when
                        (<= st n)
                        (let [r (m s st [] (fn [j caps] [j caps]))]
                            (cond (identical? r ::abort) nil r (when (every? (fn [[t a b]] (store t (subs s a b))) (r 1)) [st (r 0)]) :else (recur (inc st))))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn MATCH [sv p] (let [s (gtstg sv 241) r (find-match s p)] (if r (subs s (r 0) (r 1)) (fail))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn REPLACE
    [n p replacement]
    (let [s (gtstg (rd n) 241) r (find-match s p)]
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
    (let [
          cnt (if (neg? (long @&STLIMIT)) @&STCOUNT (swap! &STCOUNT inc))
          bump (if (or (stcount-traced?) (not (zero? (long @&PROFILE)))) 1 0)
          over (when (and (pos? (long @&STLIMIT)) (> (+ (long cnt) bump) (long @&STLIMIT))) (overrun cnt))]
        (when
            (pos? (long @&TRACE))
            (when (and (not over) (not (neg? (long @&STLIMIT)))) (fire-keyword! "STCOUNT" cnt))
            (when (statement-trace-active?) (binding [*out* *err*] (println (format ">>> stmt %5d  %s" (dec (long cnt)) (str (or (@<LABL> n) n)))))))
        over))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- goto-of [n] (let [[x y] (@<CODE> (or (@<LABL> n) n))] (if (map? x) x y)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn trapped
    [n x]
    (let [t (trap x)]
        (cond
            (false? t) false
            (instance? SNOBOL4clojure.core.Transfer t) (throw (ex-info "jump" {::jump [(:at t) (:err t)]}))
            :else (throw (ex-info "jump" {::jump [(:label t) {:at n :goto (goto-of n) :ex (:ex t)}]})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defmacro stmt ([n] `(stmt ~n true)) ([n e] `(if-some [over# (step! ~n)] (trapped ~n over#) (try ~e true (catch clojure.lang.ExceptionInfo x# (if (identical? x# failure) false (trapped ~n x#)))))))
(defn- pc-of [at] (cond (vector? at) (first at) (integer? at) at (keyword? at) (@<STNO> at) (string? at) (or (@<STNO> (keyword at)) (when (re-matches #"[0-9]+" at) (Long/parseLong at)))))
(defn- current-error? [err] (and err (= (:snobol/serial (ex-data (:ex err))) @<ERRSERIAL>)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn system!
    [nm err]
    (case nm
        "CONTINUE"
            (if
                (current-error? err)
                (let [g (:goto err) f (when (map? g) (or (:G g) (:F g)))]
                    (cond (nil? f) (inc (long (pc-of (:at err)))) (special-targets f) (dispatch-special! f) :else (or (pc-of f) (throw (ex-info "goto undefined label" {:snobol/error 38})))))
                (throw (fatal-error 37)))
        "ABORT"
            (if err
                (let [e ^clojure.lang.ExceptionInfo (:ex err)]
                    (throw (ex-info (ex-message e) (-> (ex-data e) (dissoc :snobol/label :snobol/serial :snobol/fp :snobol/resume) (assoc :snobol/abort true)))))
                (let [k (long @&ERRTYPE)] (throw (if (zero? k) (ex-info (error-message 36) {:snobol/error 36}) (ex-info (error-message k) {:snobol/error k :snobol/abort true})))))
        "SCONTINUE" (throw (fatal-error (if (current-error? err) 331 321)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn landing
    [^clojure.lang.ExceptionInfo x]
    (if-some [[tgt err] (::jump (ex-data x))]
        (let [nm (when (or (keyword? tgt) (string? tgt)) (name tgt))]
            (cond
                (#{"CONTINUE" "ABORT" "SCONTINUE"} nm) [(system! nm err) err]
                (and nm (special-targets tgt)) (dispatch-special! tgt)
                :else [(or (pc-of tgt) (throw (ex-info "goto undefined label" {:snobol/error 38}))) err]))
        (throw x)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn goto [label pc] (when (pos? (long @&TRACE)) (fire-label! label)) pc)
(defn start! [] (when (zero? (long @&FNCLEVEL)) (reset! &STCOUNT 0)))
(defn end! [] (if (pos? (long @&FNCLEVEL)) (dispatch-special! :END) (do (end-statement!) nil)))
(defn closing! [] (throw (ex-info "END" {:snobol/signal :end :snobol/closing true})))
(defn special! [t] (dispatch-special! t))
(def ^:private ^:dynamic *st* nil)
(defn- refuse [why] (throw (ex-info why {::refused why})))
(defn- lit? [x] (or (string? x) (integer? x) (float? x)))
(def ^:private pattern-functions '#{POS RPOS LEN TAB RTAB ANY NOTANY SPAN BREAK BREAKX ARBNO FENCE})
(def ^:private pattern-constants '{ARB ARB REM REM BAL BAL FAIL FAIL SUCCEED SUCCEED ABORT ABORT FENCE (FENCE)})
(defn- plain-op? [[op & args]] (let [n (count args)] (cond ('#{+ -} op) (<= 1 n 2) ('#{* /} op) (= n 2) :else (boolean (and (re-matches #"[A-Z][A-Z0-9]*" (name op)) (not= op 'SEQ))))))
(defn- fallback [e] (swap! (:fallbacks *st*) inc) (list 'ev (list 'quote e)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen
    [e]
    (cond
        (lit? e) e
        (symbol? e) (list 'rd (list 'quote e))
        (and (vector? e) (not= :comma (first e))) (cons 'SEQ (map gen e))
        (and (list? e) (symbol? (first e)) (not (:subscript (meta e))))
            (let [[op & args] e n (count args)]
                (cond
                    (and (= op '|) (>= n 2)) (cons 'ALT (map gen args))
                    (and (= op '*) (= n 1)) (list 'DEFER (list 'fn [] (gen (first args))))
                    (and (= op '.) (= n 2)) (list 'CAPTURE (gen (first args)) (list 'target (list 'quote (second args))))
                    (and (= op '$) (= n 2)) (list 'CAPTURE-NOW (gen (first args)) (list 'target (list 'quote (second args))))
                    (and (= op 'at-op) (= n 1)) (list 'CURSOR (list 'target (list 'quote (first args))))
                    (and (= op 'pattern-constant) (= n 1) (pattern-constants (first args))) (pattern-constants (first args))
                    (and (pattern-functions op) (= n 1)) (list op (gen (first args)))
                    (and (= op '?) (= n 2)) (list 'MATCH (gen (first args)) (gen (second args)))
                    (plain-op? e) (list* 'call (list 'quote op) (map gen args))
                    :else (fallback e)))
        :else (fallback e)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- target? [n] (and (symbol? n) (not (protected-variables n))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen-body
    [b]
    (cond
        (and (list? b) (= '= (first b)) (= 3 (count b)) (target? (second b))) (list 'assign (list 'quote (second b)) (gen (nth b 2)))
        (and (list? b) (= '?= (first b)) (= 4 (count b)) (target? (second b))) (let [[_ n p r] b] (list 'REPLACE (list 'quote n) (gen p) (list 'fn [] (if (some? r) (gen r) ""))))
        :else (gen b)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private unsupported '#{OPSYN})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- generate
    []
    (let [
          end (or (@<STNO> :END) (@<STNO> :end) (inc (long @STNO)))
          stmts (vec (for [n (range 1 end)] (or (@<CODE> (or (@<LABL> n) n)) (refuse (str "statement " n " is not in the store")))))
          atoms (set (filter #(or (symbol? %) (string? %)) (mapcat #(tree-seq coll? seq %) (remove #(or (identical? % block-end) (identical? % compile-error)) stmts))))
          _ (when-some [u (some unsupported atoms)] (refuse (str "uses " u)))
          _ (when (some #(and (string? %) (re-find #"^\s*(POS|RPOS|LEN|TAB|RTAB|ANY|NOTANY|SPAN|BREAK|BREAKX|ARBNO|FENCE)\s*\(" %)) atoms) (refuse "defines a pattern function's name"))
          jump
              (fn
                  [t n]
                  (cond
                      (nil? t) (list 'recur (inc n))
                      (#{:END :end} t) (list 'recur end)
                      (and (keyword? t) (#{"CONTINUE" "ABORT" "SCONTINUE"} (name t))) (list 'recur (list 'system! (name t) 'err))
                      (special-targets t) (list 'special! t)
                      (and (keyword? t) (@<STNO> t)) (list 'recur (list 'goto t (@<STNO> t)))
                      :else (refuse (str "statement " n " has a goto the compiler does not handle: " (pr-str t)))))
          clauses
              (doall
                  (mapcat
                      (fn
                          [n stmt]
                          (cond
                              (identical? stmt block-end) [n (list 'do (list 'step! n) '(closing!))]
                              (identical? stmt compile-error) [n (list 'do (list 'stmt n (list 'throw (list 'ex-info (error-message 7) {:snobol/error 7}))) (list 'recur (inc n)))]
                              :else
                                  (let [
                                        [x y] stmt
                                        body (if (map? x) y x)
                                        goto (if (map? x) x y)
                                        g (when (map? goto) goto)
                                        before @(:fallbacks *st*)
                                        code (when (some? body) (gen-body body))
                                        s (jump (or (:G g) (:S g)) n)
                                        f (jump (or (:G g) (:F g)) n)
                                        head (if (nil? code) (list 'stmt n) (list 'stmt n code))]
                                      (swap! (:stmts *st*) conj (if (= before @(:fallbacks *st*)) :native :fallback))
                                      [n (if (= s f) (list 'do head s) (list 'if head s f))])))
                      (range 1 end)
                      stmts))
          body (list 'loop '[pc pc] (concat (list 'case '(int pc)) clauses (list end '(end!) nil)))]
        (list 'fn '[entry] '(start!) (list 'loop '[pc entry err nil] (list 'when-some ['[pc err] (list 'try body '(catch clojure.lang.ExceptionInfo x (landing x)))] '(recur pc err))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn compile-source
    [src]
    (c/GLOBALS)
    (binding [*st* {:fallbacks (atom 0) :stmts (atom [])}]
        (try
            (let [
                  entry (c/CODE src)
                  _ (when (seq @<DIAGNOSTICS>) (refuse "compile errors"))
                  code (generate)
                  f (binding [*ns* (the-ns 'SNOBOL4clojure.codegen)] (eval code))
                  fq (frequencies @(:stmts *st*))
                  pcs @<STNO>]
                {
                 :code code
                 :f f
                 :entry (if (keyword? entry) (or (pcs entry) (refuse "entry label")) (or entry 1))
                 :pcs pcs
                 :last (long @STNO)
                 :statements (count @(:stmts *st*))
                 :native (:native fq 0)
                 :fallback (:fallback fq 0)})
            (catch clojure.lang.ExceptionInfo e (if-some [why (::refused (ex-data e))] {:refused why} (throw e)))
            (catch clojure.lang.Compiler$CompilerException e {:refused (str "generated code did not compile: " (.getMessage (or (.getCause e) e)))}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-compiled
    [{:keys [f entry pcs last]}]
    (let [interpret c/RUN]
        (with-redefs [c/RUN (fn [at] (let [pc (cond (keyword? at) (pcs at) (and (integer? at) (<= 1 at (long last))) at)] (if pc (f pc) (interpret at))))]
            (try
                (f entry)
                (catch clojure.lang.ExceptionInfo e
                    (if
                        (= :end (:snobol/signal (ex-data e)))
                        (do (when-some [l (:fnclevel (ex-data e))] (reset! &FNCLEVEL l)) (when-not (:snobol/closing (ex-data e)) (end-statement!)) nil)
                        (throw e)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn show [code] (binding [pp/*print-right-margin* 150 pp/*print-miser-width* 60] (pp/with-pprint-dispatch pp/code-dispatch (pp/pprint code))))
