(ns SNOBOL4clojure.codegen (:require [SNOBOL4clojure.core :as c] [clojure.pprint :as pp]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(doseq [s
            '[$$ &TRACE fire-access! bare-fence concat-value deref-result <FUNS> opsyn-key op-spelling INVOKE EVAL! gtstg sstr &MAXLNGTH &ANCHOR <POS> <DISPNO> <STMT-LINE> &LASTNO &STNO &LASTFILE
                 &FILE &LASTLINE &LINE &STCOUNT &STLIMIT &FNCLEVEL end-statement! dispatch-special! special-targets protected-variables <CODE> <STNO> <LABL> <DIAGNOSTICS> block-end compile-error
                 capture-target pattern-argument SEARCH snobol-set!]]
    (intern *ns* (with-meta s {:private true}) (var-get (ns-resolve 'SNOBOL4clojure.core s))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private core-fence (var-get (ns-resolve 'SNOBOL4clojure.core 'FENCE)))
(deftype Pat [f])
(deftype Expr [thunk])
(defmethod c/DATATYPE "class SNOBOL4clojure.codegen.Pat" [_] "PATTERN")
(defmethod c/DATATYPE "class SNOBOL4clojure.codegen.Expr" [_] "EXPRESSION")
(def failure (ex-info "failure" {::failure true}))
(defn fail [] (throw failure))
(defmacro attempt [e] `(try ~e true (catch clojure.lang.ExceptionInfo x# (if (identical? x# failure) false (throw x#)))))
(defn- value [thunk] (try (thunk) (catch clojure.lang.ExceptionInfo x (if (identical? x failure) nil (throw x)))))
(defn- some! [v] (if (nil? v) (fail) v))
(defn rd [s] (let [v ($$ s)] (when (pos? (long @&TRACE)) (fire-access! s v)) (some! (if (identical? v core-fence) bare-fence v))))
(defn call [op & args] (let [f (get @<FUNS> (opsyn-key (get op-spelling op (str op)) (count args)))] (some! (deref-result (if f (apply f args) (apply INVOKE op args))))))
(defn ev [form] (some! (EVAL! form)))
(defn assign [n r] (some! (INVOKE '= n r)))
(defn pattern? [v] (or (instance? Pat v) (instance? Expr v)))
(defn- lit [^String t] (let [n (.length t)] (fn [^String s i caps k] (when (.startsWith s t (int i)) (k (+ (long i) n) caps)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- matcher
    [v]
    (cond
        (instance? Pat v) (.f ^Pat v)
        (string? v) (lit v)
        (instance? Expr v) (let [thunk (.thunk ^Expr v)] (fn [s i caps k] (when-some [x (value thunk)] ((matcher x) s i caps k))))
        (number? v) (lit (sstr v))
        :else (throw (ex-info "a pattern value the prototype cannot match beside its own" {::unsupported (str (type v))}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- data? [vs] (and (some list? vs) (not-any? pattern? vs)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn SEQ
    [& vs]
    (if
        (or (not-any? pattern? vs) (data? vs))
        (some! (concat-value (apply list 'SEQ vs)))
        (let [ms (mapv matcher (remove #(= "" %) vs))] (Pat. (reduce (fn [tail p] (fn [s i caps k] (p s i caps (fn [j caps] (tail s j caps k))))) (peek ms) (reverse (pop ms)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ALT [& vs] (if (data? vs) (some! (apply INVOKE '| vs)) (let [ms (mapv matcher vs)] (Pat. (fn [s i caps k] (loop [ms (seq ms)] (when ms (or ((first ms) s i caps k) (recur (next ms))))))))))
(defn DEFER [thunk] (Expr. thunk))
(defn- store [t text] (if (fn? t) (some? (t text)) (do (snobol-set! t text) true)))
(defn target [n] (some! (capture-target n)))
(defn CAPTURE [p t] (let [m (matcher p)] (Pat. (fn [s i caps k] (m s i caps (fn [j caps] (k j (conj caps [t i j]))))))))
(defn CAPTURE-NOW [p t] (let [m (matcher p)] (Pat. (fn [^String s i caps k] (m s i caps (fn [j caps] (store t (subs s i j)) (k j caps)))))))
(def ARB (Pat. (fn [^String s i caps k] (let [len (.length s)] (loop [j (long i)] (when (<= j len) (or (k j caps) (recur (inc j)))))))))
(def REM (Pat. (fn [^String s i caps k] (k (.length s) caps))))
(def FAIL (Pat. (fn [s i caps k] nil)))
(def SUCCEED (Pat. (fn [s i caps k] (loop [] (or (k i caps) (recur))))))
(def ABORT (Pat. (fn [s i caps k] ::abort)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn FENCE
    ([] (Pat. (fn [s i caps k] (or (k i caps) ::abort))))
    ([p] (let [m (matcher p)] (Pat. (fn [s i caps k] (let [r (m s i caps (fn [j caps] [j caps]))] (cond (nil? r) nil (identical? r ::abort) ::abort :else (k (r 0) (r 1)))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ARBNO [p] (let [m (matcher p)] (Pat. (fn arbno [s i caps k] (or (k i caps) (m s i caps (fn [j caps] (when (> (long j) (long i)) (arbno s j caps k)))))))))
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
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- find-match
    [^String s p]
    (if
        (or (pattern? p) (string? p) (number? p))
        (let [m (matcher p) len (.length s) n (if (zero? (long @&ANCHOR)) len 0)]
            (loop [st 0]
                (when
                    (<= st n)
                    (let [r (m s st [] (fn [j caps] [j caps]))] (cond (identical? r ::abort) nil r (when (every? (fn [[t a b]] (store t (subs s a b))) (r 1)) [st (r 0)]) :else (recur (inc st)))))))
        (SEARCH s (pattern-argument p 240))))
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
(defn stmt!
    [n]
    (reset! &LASTNO @&STNO)
    (reset! &LASTFILE @&FILE)
    (reset! &LASTLINE @&LINE)
    (when-some [[f l] (get @<POS> n)] (reset! &FILE f) (reset! &LINE l) (reset! <STMT-LINE> l))
    (reset! &STNO (get @<DISPNO> n n))
    (let [cnt (if (neg? (long @&STLIMIT)) @&STCOUNT (swap! &STCOUNT inc))]
        (when (and (pos? (long @&STLIMIT)) (> (long cnt) (long @&STLIMIT))) (throw (ex-info "statement count exceeds value of stlimit keyword" {:snobol/error 244})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn start! [] (when (zero? (long @&FNCLEVEL)) (reset! &STCOUNT 0)))
(defn end! [] (if (pos? (long @&FNCLEVEL)) (dispatch-special! :END) (do (end-statement!) nil)))
(defn special! [t] (dispatch-special! t))
(def ^:private ^:dynamic *st* nil)
(defn- refuse [why] (throw (ex-info why {::refused why})))
(defn- lit? [x] (or (string? x) (integer? x) (float? x)))
(def ^:private pattern-functions '#{POS RPOS LEN TAB RTAB ANY NOTANY SPAN BREAK ARBNO FENCE})
(def ^:private pattern-constants '{ARB ARB REM REM FAIL FAIL SUCCEED SUCCEED ABORT ABORT FENCE (FENCE)})
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
(def ^:private unsupported '#{SETEXIT ERRLIMIT TRACE FTRACE STOPTR STLIMIT PROFILE OPSYN})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- generate
    []
    (let [
          end (or (@<STNO> :END) (@<STNO> :end) (refuse "no END statement"))
          stmts
              (vec
                  (for [n (range 1 end)]
                      (let [stmt (@<CODE> (or (@<LABL> n) n))]
                          (if (or (nil? stmt) (identical? stmt block-end) (identical? stmt compile-error)) (refuse (str "statement " n " is no plain statement")) stmt))))
          atoms (set (filter #(or (symbol? %) (string? %)) (mapcat #(tree-seq coll? seq %) stmts)))
          _ (when-some [u (some unsupported atoms)] (refuse (str "uses " u)))
          _ (when (some #(and (string? %) (re-find #"^\s*(POS|RPOS|LEN|TAB|RTAB|ANY|NOTANY|SPAN|BREAK|ARBNO|FENCE)\s*\(" %)) atoms) (refuse "defines a pattern function's name"))
          jump
              (fn
                  [t n]
                  (cond
                      (nil? t) (list 'recur (inc n))
                      (#{:END :end} t) (list 'recur end)
                      (special-targets t) (list 'special! t)
                      (and (keyword? t) (@<STNO> t)) (list 'recur (@<STNO> t))
                      :else (refuse (str "statement " n " has a goto the prototype does not compile: " (pr-str t)))))
          clauses
              (doall
                  (mapcat
                      (fn
                          [n stmt]
                          (let [
                                [x y] stmt
                                body (if (map? x) y x)
                                goto (if (map? x) x y)
                                g (when (map? goto) goto)
                                before @(:fallbacks *st*)
                                code (when (some? body) (gen-body body))
                                s (jump (or (:G g) (:S g)) n)
                                f (jump (or (:G g) (:F g)) n)]
                              (swap! (:stmts *st*) conj (if (= before @(:fallbacks *st*)) :native :fallback))
                              [n
                                  (cond
                                      (nil? code) (list 'do (list 'stmt! n) s)
                                      (= s f) (list 'do (list 'stmt! n) (list 'attempt code) s)
                                      :else (list 'do (list 'stmt! n) (list 'if (list 'attempt code) s f)))]))
                      (range 1 end)
                      stmts))]
        (list 'fn '[entry] '(start!) (list 'loop '[pc entry] (concat (list 'case '(int pc)) clauses (list end '(end!) nil))))))
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
                 :statements (count @(:stmts *st*))
                 :native (:native fq 0)
                 :fallback (:fallback fq 0)})
            (catch clojure.lang.ExceptionInfo e (if-some [why (::refused (ex-data e))] {:refused why} (throw e)))
            (catch clojure.lang.Compiler$CompilerException e {:refused (str "generated code did not compile: " (.getMessage (or (.getCause e) e)))}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-compiled
    [{:keys [f entry pcs]}]
    (let [interpret c/RUN last-pc (apply max 0 (vals pcs))]
        (with-redefs [c/RUN (fn [at] (let [pc (cond (keyword? at) (pcs at) (and (integer? at) (<= 1 at last-pc)) at)] (if pc (f pc) (interpret at))))]
            (try (f entry) (catch clojure.lang.ExceptionInfo e (if (= :end (:snobol/signal (ex-data e))) (do (when-not (:snobol/closing (ex-data e)) (end-statement!)) nil) (throw e)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn show [code] (binding [pp/*print-right-margin* 150 pp/*print-miser-width* 60] (pp/with-pprint-dispatch pp/code-dispatch (pp/pprint code))))
