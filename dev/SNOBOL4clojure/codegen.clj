(ns SNOBOL4clojure.codegen (:require [SNOBOL4clojure.core :as c] [clojure.pprint :as pp]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(doseq [s
            '[$$ &TRACE fire-access! FENCE bare-fence concat-value deref-result <FUNS> opsyn-key op-spelling INVOKE EVAL! gtstg sstr &MAXLNGTH assign-to &ANCHOR <POS> <DISPNO> <STMT-LINE> &LASTNO
                 &STNO &LASTFILE &FILE &LASTLINE &LINE &STCOUNT &STLIMIT &FNCLEVEL end-statement! dispatch-special! special-targets protected-variables <CODE> <STNO> <LABL> <DIAGNOSTICS> block-end
                 compile-error]]
    (intern *ns* (with-meta s {:private true}) (var-get (ns-resolve 'SNOBOL4clojure.core s))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn rd [s] (let [v ($$ s)] (when (pos? (long @&TRACE)) (fire-access! s v)) (if (identical? v FENCE) bare-fence v)))
(defn call [op & args] (let [f (get @<FUNS> (opsyn-key (get op-spelling op (str op)) (count args)))] (deref-result (if f (apply f args) (apply INVOKE op args)))))
(defn catenate [& vs] (concat-value (apply list 'SEQ vs)))
(defn assign [n r] (INVOKE '= n r))
(defn subject [v] (gtstg v 241))
(defn anchored? [] (not (zero? (long @&ANCHOR))))
(defn capture [v text] (assign-to v text) true)
(defn commit [caps ^String s] (every? (fn [[v a b]] (capture v (subs s a b))) caps))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn subst
    [n ^String s a b r]
    (let [result (str (subs s 0 a) (sstr r) (subs s b))]
        (when (> (count result) (long @&MAXLNGTH)) (throw (ex-info "string length exceeds value of maxlngth keyword" {:snobol/error 205})))
        (INVOKE '= n result)
        result))
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
(defn end! [] (end-statement!) nil)
(defn special! [t] (dispatch-special! t))
(def ^:private ^:dynamic *st* nil)
(defn- fresh [p] (symbol (str p (swap! (:n *st*) inc))))
(defn- refuse [why] (throw (ex-info why {::refused why})))
(defn- lit? [x] (or (string? x) (integer? x) (float? x)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- static-pattern?
    [p]
    (cond
        (string? p) true
        (vector? p) (and (seq p) (not= :comma (first p)) (every? static-pattern? p))
        (list? p)
            (let [[op a b] p n (dec (count p))]
                (cond
                    (:subscript (meta p)) false
                    (= op '|) (and (>= n 2) (every? static-pattern? (rest p)))
                    ('#{POS RPOS LEN TAB RTAB} op) (and (= n 1) (integer? a) (>= a 0))
                    ('#{ANY NOTANY SPAN BREAK} op) (and (= n 1) (string? a) (pos? (count a)))
                    (= op 'pattern-constant) (boolean (and (= n 1) ('#{ARB REM} a)))
                    ('#{. $} op) (boolean (and (= n 2) (static-pattern? a) (symbol? b) (not (protected-variables b))))
                    :else false))
        :else false))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- in-set [cs j] (list '>= (list '.indexOf cs (list 'int (list '.charAt 's j))) 0))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen-pat
    [p i caps k]
    (cond
        (string? p) (list 'when (list '.startsWith 's p i) (list k (list '+ i (count p)) caps))
        (vector? p)
            (if
                (= 1 (count p))
                (gen-pat (first p) i caps k)
                (let [
                      ks (vec (repeatedly (dec (count p)) #(fresh "k")))
                      fns (map (fn [n] (list (ks (dec n)) '[^long i caps] (gen-pat (nth p n) 'i 'caps (if (< n (dec (count p))) (ks n) k)))) (range 1 (count p)))]
                    (list 'letfn (vec fns) (gen-pat (first p) i caps (ks 0)))))
        :else
            (let [[op a b] p]
                (case op
                    | (if (symbol? k) (cons 'or (map #(gen-pat % i caps k) (rest p))) (let [kk (fresh "k")] (list 'let [kk k] (cons 'or (map #(gen-pat % i caps kk) (rest p))))))
                    POS (list 'when (list '== i a) (list k i caps))
                    RPOS (list 'when (list '== (list '- 'len i) a) (list k i caps))
                    LEN (list 'when (list '<= (list '+ i a) 'len) (list k (list '+ i a) caps))
                    TAB (list 'when (list 'and (list '<= i a) (list '<= a 'len)) (list k a caps))
                    RTAB (list 'when (list '<= i (list '- 'len a)) (list k (list '- 'len a) caps))
                    ANY (list 'when (list 'and (list '< i 'len) (in-set a i)) (list k (list 'inc i) caps))
                    NOTANY (list 'when (list 'and (list '< i 'len) (list 'not (in-set a i))) (list k (list 'inc i) caps))
                    SPAN
                        (let [j (fresh "j")]
                            (list 'let [j (list 'loop [j i] (list 'if (list 'and (list '< j 'len) (in-set a j)) (list 'recur (list 'inc j)) j))] (list 'when (list '> j i) (list k j caps))))
                    BREAK
                        (let [j (fresh "j")]
                            (list 'let
                                [j (list 'loop [j i] (list 'if (list 'and (list '< j 'len) (list 'not (in-set a j))) (list 'recur (list 'inc j)) j))]
                                (list 'when (list '< j 'len) (list k j caps))))
                    pattern-constant (case a ARB (let [j (fresh "j")] (list 'loop [j i] (list 'when (list '<= j 'len) (list 'or (list k j caps) (list 'recur (list 'inc j)))))) REM (list k 'len caps))
                    . (let [s0 (fresh "b")] (list 'let [s0 i] (gen-pat a s0 caps (list 'fn '[^long j caps] (list k 'j (list 'conj 'caps [(list 'quote b) s0 'j]))))))
                    $ (let [s0 (fresh "b")] (list 'let [s0 i] (gen-pat a s0 caps (list 'fn '[^long j caps] (list 'do (list 'capture (list 'quote b) (list 'subs 's s0 'j)) (list k 'j 'caps))))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen-matcher
    [p]
    (list 'fn
        [(with-meta 's {:tag 'String})]
        (list 'let
            '[len (.length s) n (if (anchored?) 0 len)]
            (list 'loop '[st 0] (list 'when '(<= st n) (list 'or (list 'letfn '[(done [^long j caps] (when (commit caps s) [st j]))] (gen-pat p 'st [] 'done)) '(recur (inc st))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- matcher! [p] (let [m (symbol (str "p" (inc (count @(:matchers *st*)))))] (swap! (:matchers *st*) conj [m (gen-matcher p)]) m))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- plain-op?
    [[op & args]]
    (let [n (count args)] (cond ('#{+ -} op) (<= 1 n 2) ('#{* /} op) (= n 2) (= op '|) (>= n 2) :else (boolean (and (re-matches #"[A-Z][A-Z0-9]*" (name op)) (not= op 'SEQ))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(declare gen-expr)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- operands
    [es body]
    (letfn
        [(go [es acc] (if (empty? es) (body acc) (let [e (first es)] (if (lit? e) (go (rest es) (conj acc e)) (let [v (fresh "v")] (list 'when-some [v (gen-expr e)] (go (rest es) (conj acc v))))))))]
        (go es [])))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- fallback [e] (swap! (:fallbacks *st*) inc) (list 'EVAL! (list 'quote e)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen-expr
    [e]
    (cond
        (lit? e) e
        (symbol? e) (list 'rd (list 'quote e))
        (and (vector? e) (not= :comma (first e))) (operands e #(cons 'catenate %))
        (and (list? e) (symbol? (first e)) (not (:subscript (meta e))) (plain-op? e)) (operands (rest e) #(list* 'call (list 'quote (first e)) %))
        :else (fallback e)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- target? [n] (and (symbol? n) (not (protected-variables n))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- gen-body
    [b]
    (cond
        (nil? b) true
        (and (list? b) (= '= (first b)) (= 3 (count b)) (target? (second b)))
            (let [[_ n r] b] (if (lit? r) (list 'assign (list 'quote n) r) (let [v (fresh "v")] (list 'when-some [v (gen-expr r)] (list 'assign (list 'quote n) v)))))
        (and (:static *st*) (list? b) (= '? (first b)) (= 3 (count b)) (static-pattern? (nth b 2)))
            (let [m (matcher! (nth b 2)) v (fresh "v")] (list 'when-some [v (gen-expr (second b))] (list 'let ['s (list 'subject v)] (list 'when-some ['[a b] (list m 's)] '(subs s a b)))))
        (and (:static *st*) (list? b) (= '?= (first b)) (= 4 (count b)) (target? (second b)) (static-pattern? (nth b 2)))
            (let [[_ n p r] b m (matcher! p)]
                (list 'let ['s (list 'subject (list 'rd (list 'quote n)))] (list 'when-some ['[a b] (list m 's)] (list 'subst (list 'quote n) 's 'a 'b (when (some? r) (gen-expr r))))))
        :else (gen-expr b)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private unsupported '#{SETEXIT ERRLIMIT TRACE FTRACE STOPTR STLIMIT PROFILE})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- generate
    [entry]
    (let [
          end (or (@<STNO> :END) (@<STNO> :end) (refuse "no END statement"))
          stmts
              (vec
                  (for [n (range 1 end)]
                      (let [stmt (@<CODE> (or (@<LABL> n) n))]
                          (if (or (nil? stmt) (identical? stmt block-end) (identical? stmt compile-error)) (refuse (str "statement " n " is no plain statement")) stmt))))
          atoms (set (filter #(or (symbol? %) (string? %)) (mapcat #(tree-seq coll? seq %) stmts)))
          _ (when-some [u (some unsupported atoms)] (refuse (str "uses " u)))
          static (not (or (atoms 'OPSYN) (some #(and (string? %) (re-find #"^\s*(POS|RPOS|LEN|TAB|RTAB|ANY|NOTANY|SPAN|BREAK|ARB|REM)\s*\(" %)) atoms)))
          jump
              (fn
                  [t n]
                  (cond
                      (nil? t) (list 'recur (inc n))
                      (#{:END :end} t) (list 'recur end)
                      (special-targets t) (list 'special! t)
                      (and (keyword? t) (@<STNO> t)) (list 'recur (@<STNO> t))
                      :else (refuse (str "statement " n " has a goto the prototype does not compile: " (pr-str t)))))]
        (binding [*st* (assoc *st* :static static)]
            (let [clauses
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
                                        code (gen-body body)
                                        s (jump (or (:G g) (:S g)) n)
                                        f (jump (or (:G g) (:F g)) n)]
                                      (swap! (:stmts *st*) conj (if (= before @(:fallbacks *st*)) :native :fallback))
                                      [n (if (= s f) (list 'do (list 'stmt! n) code s) (list 'do (list 'stmt! n) (list 'if code s f)))]))
                              (range 1 end)
                              stmts))]
                (list 'let
                    (vec (mapcat identity @(:matchers *st*)))
                    (list 'fn
                        []
                        '(start!)
                        (list 'loop ['pc (if (keyword? entry) (or (@<STNO> entry) (refuse "entry label")) (or entry 1))] (concat (list 'case '(int pc)) clauses (list end '(end!) nil)))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn compile-source
    [src]
    (c/GLOBALS)
    (binding [*st* {:n (atom 0) :matchers (atom []) :fallbacks (atom 0) :stmts (atom []) :entry (c/CODE src)}]
        (try
            (when (seq @<DIAGNOSTICS>) (refuse "compile errors"))
            (let [code (generate (:entry *st*)) f (binding [*ns* (the-ns 'SNOBOL4clojure.codegen)] (eval code)) fq (frequencies @(:stmts *st*))]
                {:code code :run f :statements (count @(:stmts *st*)) :native (:native fq 0) :fallback (:fallback fq 0) :matchers (count @(:matchers *st*))})
            (catch clojure.lang.ExceptionInfo e (if-some [why (::refused (ex-data e))] {:refused why} (throw e)))
            (catch clojure.lang.Compiler$CompilerException e {:refused (str "generated code did not compile: " (.getMessage (or (.getCause e) e)))}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-compiled [r] (try ((:run r)) (catch clojure.lang.ExceptionInfo e (when-not (= :end (:snobol/signal (ex-data e))) (throw e)))))
(defn show [code] (binding [pp/*print-right-margin* 150 pp/*print-miser-width* 60] (pp/with-pprint-dispatch pp/code-dispatch (pp/pprint code))))
