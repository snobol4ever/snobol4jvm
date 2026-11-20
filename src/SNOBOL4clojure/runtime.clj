(ns SNOBOL4clojure.runtime
    (:require
        [SNOBOL4clojure.env :as env :refer
            [<STNO> <LABL> <CODE> <DISPNO> block-end compile-error &STCOUNT &STLIMIT &TRACE &STNO &LASTNO &ERRLIMIT &ERRTYPE &ERRTEXT <POS> &FILE &LASTFILE &LINE &LASTLINE &FNCLEVEL snobol-return!
                snobol-freturn! snobol-nreturn! snobol-end! snobol-steplimit! fire-label! fire-keyword! statement-trace-active?]]
        [SNOBOL4clojure.operators :refer [EVAL! INVOKE error-decided]]
        [clojure.string :as str]
        [SNOBOL4clojure.compiler :as comp])
    (:import [clojure.asm ClassWriter MethodVisitor Label Opcodes] [clojure.lang DynamicClassLoader IFn RT]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private special-targets #{"RETURN" "FRETURN" "NRETURN" "END" "end" :RETURN :FRETURN :NRETURN :END :end})
(def ^:private system-labels #{"CONTINUE" "ABORT" "RETURN" "FRETURN" "NRETURN" "END" "end"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- level-zero-return!
    []
    (when
        (zero? (long @SNOBOL4clojure.env/&FNCLEVEL))
        (throw (ex-info (SNOBOL4clojure.env/error-message 242) (if (and (pos? (long @&ERRLIMIT)) @SNOBOL4clojure.env/<SETEXIT>) {:snobol/error 242} {:snobol/error 242 :snobol/abort true})))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- dispatch-special!
    [target]
    (when (#{"RETURN" "FRETURN" "NRETURN"} (clojure.string/upper-case (name target))) (level-zero-return!))
    (case (clojure.string/upper-case (name target)) "RETURN" (snobol-return!) "FRETURN" (snobol-freturn!) "NRETURN" (snobol-nreturn!) "END" (snobol-end!)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defrecord Trapped [label ex])
(defrecord Transfer [at err])
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- trap
    [^clojure.lang.ExceptionInfo e]
    (if
        (clojure.core/= :transfer (:snobol/signal (ex-data e)))
        (->Transfer (:snobol/at (ex-data e)) (:snobol/err (ex-data e)))
        (let [e (error-decided e SNOBOL4clojure.env/*fp*) {to :snobol/to h :snobol/label} (ex-data e)] (cond h (->Trapped h e) to false :else (throw e)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- overrun
    [n]
    (let
        [lim (long @&STLIMIT)]
        (reset! &STCOUNT lim)
        (swap! &STLIMIT clojure.core/+ 10)
        (if (and (pos? (long @&ERRLIMIT)) (< (long (swap! SNOBOL4clojure.env/<ERRFT> inc)) 3)) (ex-info (SNOBOL4clojure.env/error-message 244) {:snobol/error 244}) (snobol-steplimit! n))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- fatal-error
    [n]
    (let [msg (SNOBOL4clojure.env/error-message n)] (if (< (long (swap! SNOBOL4clojure.env/<ERRFT> inc)) 3) (ex-info msg {:snobol/error n}) (ex-info msg {:snobol/error n :snobol/abort true}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- exec-body [body] (try (or (nil? body) (EVAL! body)) (catch clojure.lang.ExceptionInfo e (trap e))))
(defn- label-name [tgt] (when (or (keyword? tgt) (string? tgt) (symbol? tgt)) (name tgt)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- end-statement!
    []
    (when-let
        [n (@<STNO> :END)]
        (let
            [counted (when-not (neg? (long @&STLIMIT)) (swap! &STCOUNT inc))]
            (reset! &LASTNO @&STNO)
            (reset! &STNO (get @<DISPNO> n n))
            (reset! &LASTFILE @&FILE)
            (reset! &LASTLINE @&LINE)
            (let [[f l] (get @<POS> n ["" 0])] (reset! &FILE f) (reset! &LINE l) (reset! SNOBOL4clojure.env/<STMT-LINE> l))
            (when (and counted (pos? (long @&TRACE))) (fire-keyword! "STCOUNT" counted)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- run*
    [at err0 k]
    (letfn
        [(skey [address] (let [[no label] address] (if label label no)))
            (saddr
                [at]
                (cond
                    (keyword? at)
                    (when-let [n (@<STNO> at)] [n at])
                    (string? at)
                    (or (when-let [n (@<STNO> (keyword at))] [n (keyword at)]) (when (re-matches #"[0-9]+" at) (let [n (Long/parseLong at)] [n (@<LABL> n)])) (when (@<CODE> at) [(@<STNO> at) at]))
                    (integer? at)
                    [at (@<LABL> at)]))
            (computed-target
                [tgt]
                (cond
                    (and (list? tgt) (clojure.core/= '$ (first tgt)) (clojure.core/= 2 (count tgt)))
                    (when-some [v (EVAL! (second tgt))] (if (integer? v) v (keyword (str v))))
                    (and (list? tgt) (clojure.core/= 'DIRECT (first tgt)))
                    (when-some
                        [v (EVAL! (second tgt))]
                        (if (instance? SNOBOL4clojure.env.CodeBlock v) (.entry ^SNOBOL4clojure.env.CodeBlock v) (throw (ex-info "goto operand in direct goto is not code" {:snobol/error 24}))))
                    (and (list? tgt) (symbol? (first tgt)) (not (:subscript (meta tgt))))
                    (let
                        [args (mapv EVAL! (rest tgt)) _ (when (some nil? args) (throw (ex-info "goto evaluation failure" {:snobol/error 20}))) r (apply INVOKE (first tgt) args)]
                        (cond
                            (instance? SNOBOL4clojure.env.NameResult r)
                            (let [t (:target r)] (if (symbol? t) (keyword (name t)) t))
                            (nil? r)
                            (throw (ex-info "goto evaluation failure" {:snobol/error 20}))
                            :else
                            (throw (ex-info "function called by name returned a value" {:snobol/error 21}))))
                    :else tgt))
            (goto! [tgt] (if (special-targets tgt) (dispatch-special! tgt) tgt))
            (label-addr [tgt] (or (saddr tgt) (throw (ex-info "goto undefined label" {:snobol/error 38}))))
            (step
                [r goto err current]
                (let
                    [g (when (map? goto) goto) tgt (computed-target (if r (or (:G g) (:S g)) (or (:G g) (:F g)))) nm (label-name tgt)]
                    (cond
                        (and (clojure.core/= nm "CONTINUE") (:resume err) (clojure.core/= (:resume err) @SNOBOL4clojure.env/<ERRSERIAL>))
                        [::resume nil]
                        (clojure.core/= nm "CONTINUE")
                        (if
                            (and err (clojure.core/= (:snobol/serial (ex-data (:ex err))) @SNOBOL4clojure.env/<ERRSERIAL>))
                            (let
                                [g2 (:goto err) f2 (when (map? g2) (or (:G g2) (:F g2)))]
                                (if
                                    (and (:in-goto err) f2 (not (keyword? f2)))
                                    (throw (let [e (fatal-error 332)] (ex-info (ex-message e) (assoc (ex-data e) ::continued err))))
                                    (let [etgt (computed-target f2)] (if etgt (do (goto! etgt) [(label-addr etgt) nil]) [(saddr (inc ((:at err) 0))) nil]))))
                            (throw (fatal-error 37)))
                        (and err (clojure.core/= nm "ABORT"))
                        (let
                            [e ^clojure.lang.ExceptionInfo (:ex err)]
                            (throw (ex-info (ex-message e) (-> (ex-data e) (dissoc :snobol/label :snobol/serial :snobol/fp :snobol/resume) (assoc :snobol/abort true)))))
                        (clojure.core/= nm "ABORT")
                        (let
                            [n (long @&ERRTYPE)]
                            (throw
                                (if (zero? n) (ex-info (SNOBOL4clojure.env/error-message 36) {:snobol/error 36}) (ex-info (SNOBOL4clojure.env/error-message n) {:snobol/error n :snobol/abort true}))))
                        (clojure.core/= nm "SCONTINUE")
                        (throw
                            (fatal-error
                                (if
                                    (and err
                                        (or
                                            (and (:resume err) (clojure.core/= (:resume err) @SNOBOL4clojure.env/<ERRSERIAL>))
                                            (clojure.core/= (:snobol/serial (ex-data (:ex err))) @SNOBOL4clojure.env/<ERRSERIAL>)))
                                    331 321)))
                        tgt
                        (do (goto! tgt) (let [a (label-addr tgt)] (when (pos? (long @&TRACE)) (fire-label! (or (second a) tgt))) [a err]))
                        :else
                        [(saddr (inc (current 0))) err])))
            (follow
                [r goto err current]
                (loop
                    [r r]
                    (let
                        [res (try (step r goto err current) (catch clojure.lang.ExceptionInfo e (let [t (trap e)] (if t t ::fail))))]
                        (cond
                            (instance? Transfer res)
                            [(:at res) (:err res)]
                            (instance? Trapped res)
                            (enter (:label res) (if-let [e0 (::continued (ex-data (:ex res)))] (assoc e0 :ex (:ex res)) {:at current :goto goto :ex (:ex res) :in-goto true}) current)
                            (clojure.core/= ::fail res)
                            (recur false)
                            :else res))))
            (enter [label err current] (if (system-labels (name label)) (follow true {:G label} err current) [(saddr label) err]))]
        (when (and (nil? k) (zero? (long @&FNCLEVEL))) (reset! &STCOUNT 0))
        (try
            (loop
                [[current err] (if k (enter at err0 nil) [(saddr at) err0])]
                (cond
                    (identical? ::resume current)
                    ::resume
                    (and k (or (nil? current) (not (clojure.core/= k @SNOBOL4clojure.env/<ERRSERIAL>))))
                    (throw (ex-info "transfer" {:snobol/signal :transfer :snobol/at current :snobol/err err}))
                    :else
                    (when-let
                        [key (skey current)]
                        (when (special-targets key) (dispatch-special! key))
                        (when-let
                            [stmt (@<CODE> key)]
                            (reset! &LASTNO @&STNO)
                            (reset! &LASTFILE @&FILE)
                            (reset! &LASTLINE @&LINE)
                            (when-some [[f l] (get @<POS> (first current))] (reset! &FILE f) (reset! &LINE l) (reset! SNOBOL4clojure.env/<STMT-LINE> l))
                            (reset! &STNO (get @<DISPNO> (first current) (first current)))
                            (let
                                [over
                                    (let
                                        [n (if (neg? (long @&STLIMIT)) @&STCOUNT (swap! &STCOUNT inc))]
                                        (let
                                            [step
                                                (if (or (SNOBOL4clojure.env/stcount-traced?) (not (zero? (long @SNOBOL4clojure.env/&PROFILE)))) 1 0)
                                                over
                                                (when (and (pos? (long @&STLIMIT)) (> (+ (long n) step) (long @&STLIMIT))) (overrun n))
                                                _
                                                (when (and (not over) (pos? (long @&TRACE)) (not (neg? (long @&STLIMIT)))) (fire-keyword! "STCOUNT" n))]
                                            (when (pos? @&TRACE) (when (statement-trace-active?) (binding [*out* *err*] (println (format ">>> stmt %5d  %s" (dec n) (str key))))))
                                            over))]
                                (when (identical? stmt block-end) (throw (ex-info "END" {:snobol/signal :end :snobol/closing true})))
                                (let
                                    [ferst (first stmt) seqond (second stmt) body (if (map? ferst) seqond ferst) goto (if (map? ferst) ferst seqond)]
                                    (let
                                        [r (cond over (trap over) (identical? stmt compile-error) (trap (ex-info (SNOBOL4clojure.env/error-message 7) {:snobol/error 7})) :else (exec-body body))]
                                        (cond
                                            (instance? Transfer r)
                                            (recur [(:at r) (:err r)])
                                            (instance? Trapped r)
                                            (recur (enter (:label r) {:at current :goto goto :ex (:ex r)} current))
                                            :else
                                            (recur (follow r goto err current))))))))))
            (catch clojure.lang.ExceptionInfo e
                (if k
                    (throw e)
                    (case
                        (get (ex-data e) :snobol/signal)
                        :end
                        (if (pos? (long @&FNCLEVEL)) (throw e) (do (when-some [l (:fnclevel (ex-data e))] (reset! &FNCLEVEL l)) (when-not (:snobol/closing (ex-data e)) (end-statement!)) nil))
                        :return nil :step-limit
                        (throw e)
                        :freturn
                        (throw e)
                        :nreturn
                        (throw e)
                        (throw e)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn RUN [at] (run* at nil nil))
(defn RUN-AT-ERROR [h k e] (run* h {:resume k :ex e} k))
(def ^:const OP-HALT 0)
(def ^:const OP-EXEC 1)
(def ^:const OP-EXEC-S 2)
(def ^:const OP-EXEC-F 3)
(def ^:const OP-EXEC-SF 4)
(def ^:const OP-JUMP 5)
(def ^:const OP-SIGNAL 6)
(def ^:private special-names #{"RETURN" "FRETURN" "NRETURN" "END"})
(defn- special? [tgt] (and tgt (not (integer? tgt)) (contains? special-names (str/upper-case (name tgt)))))
(defn- signal-for [tgt] (case (str/upper-case (name tgt)) "END" :end "RETURN" :return "FRETURN" :freturn "NRETURN" :nreturn))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- assign-pcs
    [[codes nos labels]]
    (let
        [all-int (sort (into (set (filter integer? (keys codes))) (vals nos))) kw-slots (filter keyword? (keys codes))]
        (loop
            [slots all-int pc 0 m {}]
            (if
                (empty? slots)
                (reduce (fn [m kw] (let [int-no (get nos kw) kw-pc (if int-no (get m int-no) pc)] (assoc m kw kw-pc))) m kw-slots)
                (recur (rest slots) (inc pc) (assoc m (first slots) pc))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn compile-ir
    [[codes nos labels :as ir]]
    (let
        [pc-map
            (assign-pcs ir)
            all-int
            (sort (into (set (filter integer? (keys codes))) (vals nos)))
            halt-pc
            (count all-int)
            resolve-tgt
            (fn [tgt] (cond (nil? tgt) nil (special? tgt) :signal (integer? tgt) (get pc-map tgt) (keyword? tgt) (get pc-map tgt) :else (get pc-map tgt)))
            make-instr
            (fn
                [key]
                (let
                    [lbl
                        (get labels key)
                        code
                        (if lbl (get codes lbl) (get codes key))
                        [raw-body raw-goto]
                        (if code code [nil nil])
                        body raw-body goto raw-goto g-tgt
                        (:G goto)
                        s-tgt
                        (:S goto)
                        f-tgt
                        (:F goto)
                        fall
                        (+ (get pc-map key) 1)
                        s-sig
                        (when (special? s-tgt) (signal-for s-tgt))
                        f-sig
                        (when (special? f-tgt) (signal-for f-tgt))
                        g-sig
                        (when (special? g-tgt) (signal-for g-tgt))]
                    (cond
                        (and (nil? body) (nil? g-tgt) (nil? s-tgt) (nil? f-tgt) (or lbl (nil? (get codes key))))
                        {:op OP-HALT :body nil :s nil :f nil}
                        (and (nil? body) g-tgt)
                        (if g-sig {:op OP-SIGNAL :body g-sig :s nil :f nil} {:op OP-JUMP :body nil :s (resolve-tgt g-tgt) :f nil})
                        (and body g-tgt)
                        (if g-sig {:op OP-EXEC :body body :s fall :f fall} {:op OP-JUMP :body body :s (resolve-tgt g-tgt) :f nil})
                        (and body s-tgt f-tgt)
                        {:op OP-EXEC-SF :body body :s (if s-sig :signal (resolve-tgt s-tgt)) :f (if f-sig :signal (resolve-tgt f-tgt)) :s-sig s-sig :f-sig f-sig}
                        (and body s-tgt)
                        {:op OP-EXEC-S :body body :s (if s-sig :signal (resolve-tgt s-tgt)) :f fall :s-sig s-sig}
                        (and body f-tgt)
                        {:op OP-EXEC-F :body body :s fall :f (if f-sig :signal (resolve-tgt f-tgt)) :f-sig f-sig}
                        body
                        {:op OP-EXEC :body body :s fall :f fall}
                        :else
                        {:op OP-EXEC :body nil :s fall :f fall})))]
        (let [instrs (mapv make-instr all-int)] (conj instrs {:op OP-HALT :body nil :s nil :f nil}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn compile-src [src] (compile-ir (comp/CODE! src)))
(def ^:private ^:const ACC-PUBLIC (int Opcodes/ACC_PUBLIC))
(def ^:private ^:const ACC-STATIC (int Opcodes/ACC_STATIC))
(def ^:private ^:const ACC-SUPER (int Opcodes/ACC_SUPER))
(def ^:private ^:const V1-8 (int 52))
(def ^:private ^:const RETURN (int Opcodes/RETURN))
(def ^:private ^:const ARETURN (int Opcodes/ARETURN))
(def ^:private ^:const ALOAD-0 (int Opcodes/ALOAD))
(def ^:private ^:const ALOAD (int Opcodes/ALOAD))
(def ^:private ^:const ASTORE (int Opcodes/ASTORE))
(def ^:private ^:const GETSTATIC (int Opcodes/GETSTATIC))
(def ^:private ^:const PUTSTATIC (int Opcodes/PUTSTATIC))
(def ^:private ^:const INVOKESTATIC (int Opcodes/INVOKESTATIC))
(def ^:private ^:const INVOKEINTERFACE (int Opcodes/INVOKEINTERFACE))
(def ^:private ^:const INVOKEVIRTUAL (int Opcodes/INVOKEVIRTUAL))
(def ^:private ^:const INVOKESPECIAL (int Opcodes/INVOKESPECIAL))
(def ^:private ^:const GOTO (int Opcodes/GOTO))
(def ^:private ^:const IFNULL (int Opcodes/IFNULL))
(def ^:private ^:const IFNONNULL (int Opcodes/IFNONNULL))
(def ^:private ^:const ATHROW (int Opcodes/ATHROW))
(def ^:private ^:const POP (int Opcodes/POP))
(def ^:private ^:const DUP (int Opcodes/DUP))
(def ^:private ^:const ACONST-NULL (int Opcodes/ACONST_NULL))
(def ^:private OBJ "Ljava/lang/Object;")
(def ^:private STR "Ljava/lang/String;")
(def ^:private IFN-DESC "Lclojure/lang/IFn;")
(def ^:private IFN-CLASS "clojure/lang/IFn")
(def ^:private RT-CLASS "clojure/lang/RT")
(def ^:private JC-CLASS "SNOBOL4clojure/jvm_codegen")
(def ^:private IFN-INVOKE-SIG (str "(" OBJ ")" OBJ))
(def ^:private IFN-INVOKE-2-SIG (str "(" OBJ OBJ ")" OBJ))
(def ^:private THROW-SIG (str "(" STR ")Lclojure/lang/ExceptionInfo;"))
(declare EVAL_FN)
(declare $$_FN SET_FN PRINT_FN ADD_FN SUB_FN MUL_FN DIV_FN LT_FN GT_FN LE_FN GE_FN EQ_FN NEQ_FN SIGNAL_FN CONCAT_FN SIZE_FN STR_FN)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- init-inline-fns!
    []
    (alter-var-root #'$$_FN (constantly env/$$))
    (alter-var-root #'SET_FN (constantly env/protected-store!))
    (alter-var-root #'PRINT_FN (constantly (fn [v] (println (if (nil? v) "" (str v))) v)))
    (alter-var-root #'ADD_FN (constantly env/Σ+))
    (alter-var-root #'SUB_FN (constantly env/subtract))
    (alter-var-root #'MUL_FN (constantly env/multiply))
    (alter-var-root #'DIV_FN (constantly env/divide))
    (alter-var-root #'LT_FN (constantly (fn [a b] (let [na (env/num a) nb (env/num b)] (when (< na nb) a)))))
    (alter-var-root #'GT_FN (constantly (fn [a b] (let [na (env/num a) nb (env/num b)] (when (> na nb) a)))))
    (alter-var-root #'LE_FN (constantly (fn [a b] (let [na (env/num a) nb (env/num b)] (when (<= na nb) a)))))
    (alter-var-root #'GE_FN (constantly (fn [a b] (let [na (env/num a) nb (env/num b)] (when (>= na nb) a)))))
    (alter-var-root #'EQ_FN (constantly (fn [a b] (let [na (env/num a) nb (env/num b)] (when (clojure.core/= na nb) a)))))
    (alter-var-root #'NEQ_FN (constantly (fn [a b] (let [na (env/num a) nb (env/num b)] (when (not (clojure.core/= na nb)) a)))))
    (alter-var-root #'SIGNAL_FN (constantly (fn [^String sig-name] (throw (ex-info sig-name {:snobol/signal (keyword sig-name)})))))
    (alter-var-root #'CONCAT_FN
        (constantly
            (fn
                ([a b] (when (and (some? a) (some? b)) (str (if (nil? a) "" (str a)) (if (nil? b) "" (str b)))))
                ([a b & more] (let [all (list* a b more)] (when (every? some? all) (apply str (map str all))))))))
    (alter-var-root #'SIZE_FN (constantly (fn [s] (count (str s)))))
    (alter-var-root #'STR_FN (constantly (fn [v] (if (nil? v) nil (str v))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private JC-IFN-FIELD-SIGS
    {"$$_FN" "$$_FN" "SET_FN" "SET_FN" "PRINT_FN" "PRINT_FN" "ADD_FN" "ADD_FN" "SUB_FN" "SUB_FN" "MUL_FN" "MUL_FN" "DIV_FN" "DIV_FN" "LT_FN" "LT_FN" "GT_FN" "GT_FN" "LE_FN" "LE_FN" "GE_FN" "GE_FN"
        "EQ_FN" "EQ_FN" "NEQ_FN" "NEQ_FN" "SIGNAL_FN" "SIGNAL_FN" "CONCAT_FN" "CONCAT_FN" "SIZE_FN" "SIZE_FN" "STR_FN" "STR_FN"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private ARITH-OP->FN {'+ "ADD_FN" '- "SUB_FN" '* "MUL_FN" '/ "DIV_FN"})
(def ^:private CMP-OP->FN {'LT "LT_FN" 'GT "GT_FN" 'LE "LE_FN" 'GE "GE_FN" 'EQ "EQ_FN" 'NE "NEQ_FN"})
(defn- sym-or-lit? [x] (or (symbol? x) (integer? x) (float? x) (string? x)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- emit-val!
    [^MethodVisitor mv cls-name x]
    (cond
        (symbol? x)
        (do (.visitFieldInsn mv GETSTATIC cls-name "$$_FN" IFN-DESC) (.visitLdcInsn mv (str x)) (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true))
        (integer? x)
        (do (.visitLdcInsn mv (long x)) (.visitMethodInsn mv INVOKESTATIC "java/lang/Long" "valueOf" "(J)Ljava/lang/Long;" false))
        (float? x)
        (do (.visitLdcInsn mv (double x)) (.visitMethodInsn mv INVOKESTATIC "java/lang/Double" "valueOf" "(D)Ljava/lang/Double;" false))
        (string? x)
        (.visitLdcInsn mv ^String x)
        :else
        (throw (ex-info "emit-val!: not a sym-or-lit" {:x x}))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- inline-emit!
    [^MethodVisitor mv cls-name body]
    (cond
        (and (list? body) (clojure.core/= (first body) '=) (clojure.core/= (second body) 'OUTPUT) (sym-or-lit? (nth body 2)))
        (let
            [val (nth body 2)]
            (.visitFieldInsn mv GETSTATIC cls-name "PRINT_FN" IFN-DESC)
            (emit-val! mv cls-name val)
            (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
            true)
        (and (list? body) (clojure.core/= (first body) '=) (symbol? (second body)) (not (clojure.core/= (second body) 'OUTPUT)) (sym-or-lit? (nth body 2)))
        (let
            [sym (second body) val (nth body 2)]
            (emit-val! mv cls-name val)
            (.visitVarInsn mv ASTORE (int 0))
            (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
            (.visitLdcInsn mv (str sym))
            (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern" "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
            (.visitVarInsn mv ALOAD (int 0))
            (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
            true)
        (and
            (list? body)
            (clojure.core/= (first body) '=)
            (symbol? (second body))
            (list? (nth body 2))
            (ARITH-OP->FN (first (nth body 2)))
            (sym-or-lit? (second (nth body 2)))
            (sym-or-lit? (nth (nth body 2) 2)))
        (let
            [sym (second body) [op a b] (nth body 2) fn-fld (ARITH-OP->FN op)]
            (.visitFieldInsn mv GETSTATIC cls-name fn-fld IFN-DESC)
            (emit-val! mv cls-name a)
            (emit-val! mv cls-name b)
            (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
            (.visitVarInsn mv ASTORE (int 0))
            (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
            (.visitLdcInsn mv (str sym))
            (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern" "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
            (.visitVarInsn mv ALOAD (int 0))
            (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
            true)
        (and (list? body) (CMP-OP->FN (first body)) (sym-or-lit? (second body)) (sym-or-lit? (nth body 2)))
        (let
            [[op a b] body fn-fld (CMP-OP->FN op)]
            (.visitFieldInsn mv GETSTATIC cls-name fn-fld IFN-DESC)
            (emit-val! mv cls-name a)
            (emit-val! mv cls-name b)
            (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
            true)
        (and (list? body) (clojure.core/= (first body) '=) (symbol? (second body)) (not (clojure.core/= (second body) 'OUTPUT)) (vector? (nth body 2)) (every? sym-or-lit? (nth body 2)))
        (let
            [sym (second body) elems (nth body 2) n (count elems)]
            (if
                (clojure.core/= n 2)
                (let
                    [[e1 e2] elems]
                    (.visitFieldInsn mv GETSTATIC cls-name "CONCAT_FN" IFN-DESC)
                    (emit-val! mv cls-name e1)
                    (emit-val! mv cls-name e2)
                    (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                    (.visitVarInsn mv ASTORE (int 0))
                    (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
                    (.visitLdcInsn mv (str sym))
                    (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern" "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
                    (.visitVarInsn mv ALOAD (int 0))
                    (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                    true)
                false))
        (and (list? body) (clojure.core/= (first body) '=) (clojure.core/= (second body) 'OUTPUT) (vector? (nth body 2)) (every? sym-or-lit? (nth body 2)))
        (let
            [elems (nth body 2) n (count elems)]
            (if
                (clojure.core/= n 2)
                (let
                    [[e1 e2] elems]
                    (.visitFieldInsn mv GETSTATIC cls-name "CONCAT_FN" IFN-DESC)
                    (emit-val! mv cls-name e1)
                    (emit-val! mv cls-name e2)
                    (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
                    (.visitVarInsn mv ASTORE (int 0))
                    (.visitFieldInsn mv GETSTATIC cls-name "PRINT_FN" IFN-DESC)
                    (.visitVarInsn mv ALOAD (int 0))
                    (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
                    true)
                false))
        (and
            (list? body)
            (clojure.core/= (first body) '=)
            (symbol? (second body))
            (not (clojure.core/= (second body) 'OUTPUT))
            (list? (nth body 2))
            (clojure.core/= (first (nth body 2)) 'SIZE)
            (sym-or-lit? (second (nth body 2))))
        (let
            [sym (second body) arg (second (nth body 2))]
            (.visitFieldInsn mv GETSTATIC cls-name "SIZE_FN" IFN-DESC)
            (emit-val! mv cls-name arg)
            (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
            (.visitVarInsn mv ASTORE (int 0))
            (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
            (.visitLdcInsn mv (str sym))
            (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern" "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
            (.visitVarInsn mv ALOAD (int 0))
            (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-2-SIG true)
            true)
        (and (list? body) (clojure.core/= (first body) '=) (clojure.core/= (second body) 'OUTPUT) (list? (nth body 2)) (clojure.core/= (first (nth body 2)) 'SIZE) (sym-or-lit? (second (nth body 2))))
        (let
            [arg (second (nth body 2))]
            (.visitFieldInsn mv GETSTATIC cls-name "PRINT_FN" IFN-DESC)
            (.visitFieldInsn mv GETSTATIC cls-name "SIZE_FN" IFN-DESC)
            (emit-val! mv cls-name arg)
            (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
            (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
            true)
        :else false))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ^clojure.lang.ExceptionInfo make-signal [^String sig-name] (ex-info sig-name {:snobol/signal (keyword sig-name)}))
(defn gen-class-name [src] (str "SNOBOL4clojure/SnobolProg" (format "%08x" (Math/abs (hash src)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn compile-to-class
    [src]
    (let
        [bc-vec
            (compile-src src)
            n-instrs
            (count bc-vec)
            cls-name
            (gen-class-name src)
            cw
            (ClassWriter. (int ClassWriter/COMPUTE_FRAMES))
            bodies
            (into [] (keep-indexed (fn [i {:keys [body]}] (when body [i body])) bc-vec))
            body-field
            (fn [i] (str "body_" i))]
        (.visit cw V1-8 (int (+ ACC-PUBLIC ACC-SUPER)) cls-name nil "java/lang/Object" nil)
        (.visitField cw (int (+ ACC-PUBLIC ACC-STATIC)) "EVAL_FN" IFN-DESC nil nil)
        (doseq [fld-name (keys JC-IFN-FIELD-SIGS)] (.visitField cw (int (+ ACC-PUBLIC ACC-STATIC)) fld-name IFN-DESC nil nil))
        (doseq [[i _] bodies] (.visitField cw (int (+ ACC-PUBLIC ACC-STATIC)) (body-field i) OBJ nil nil))
        (let
            [ci (.visitMethod cw (int (+ ACC-PUBLIC ACC-STATIC)) "<clinit>" "()V" nil nil)]
            (.visitCode ci)
            (doseq
                [[i body] bodies]
                (.visitLdcInsn ci (pr-str body))
                (.visitMethodInsn ci INVOKESTATIC RT-CLASS "readString" (str "(" STR ")" OBJ) false)
                (.visitFieldInsn ci PUTSTATIC cls-name (body-field i) OBJ))
            (.visitInsn ci RETURN)
            (.visitMaxs ci (int 2) (int 0))
            (.visitEnd ci))
        (let
            [mv (.visitMethod cw (int (+ ACC-PUBLIC ACC-STATIC)) "run" "()V" nil nil) lbls (mapv (fn [_] (Label.)) bc-vec) end-lbl (Label.)]
            (.visitCode mv)
            (doseq
                [i (range n-instrs)]
                (.visitLabel mv (lbls i))
                (let
                    [{:keys [op body s f s-sig f-sig]}
                        (bc-vec i)
                        op
                        (int op)
                        eval-body!
                        (fn
                            []
                            (if body
                                (or
                                    (inline-emit! mv cls-name body)
                                    (do
                                        (.visitFieldInsn mv GETSTATIC cls-name "EVAL_FN" IFN-DESC)
                                        (.visitFieldInsn mv GETSTATIC cls-name (body-field i) OBJ)
                                        (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)))
                                (.visitInsn mv ACONST-NULL)))
                        emit-signal!
                        (fn
                            [sig]
                            (.visitFieldInsn mv GETSTATIC cls-name "SIGNAL_FN" IFN-DESC)
                            (.visitLdcInsn mv (name sig))
                            (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke" IFN-INVOKE-SIG true)
                            (.visitInsn mv POP)
                            (.visitInsn mv ACONST-NULL)
                            (.visitInsn mv ATHROW))]
                    (case op 0
                        (.visitInsn mv RETURN)
                        1
                        (do (when body (eval-body!) (.visitInsn mv POP)) (when s (.visitJumpInsn mv GOTO (lbls s))))
                        2
                        (do (eval-body!) (let [fail-lbl (lbls f)] (.visitJumpInsn mv IFNULL fail-lbl) (if s-sig (emit-signal! s-sig) (.visitJumpInsn mv GOTO (lbls s)))))
                        3
                        (do
                            (eval-body!)
                            (let [ok-lbl (if s (lbls s) end-lbl)] (.visitJumpInsn mv IFNONNULL ok-lbl) (if f-sig (emit-signal! f-sig) (.visitJumpInsn mv GOTO (lbls f))) (.visitLabel mv ok-lbl)))
                        4
                        (do
                            (eval-body!)
                            (let
                                [fail-lbl (Label.)]
                                (.visitJumpInsn mv IFNULL fail-lbl)
                                (if s-sig (emit-signal! s-sig) (.visitJumpInsn mv GOTO (lbls s)))
                                (.visitLabel mv fail-lbl)
                                (if f-sig (emit-signal! f-sig) (.visitJumpInsn mv GOTO (lbls f)))))
                        5
                        (do (when body (eval-body!) (.visitInsn mv POP)) (.visitJumpInsn mv GOTO (lbls (int s))))
                        6
                        (emit-signal! body)
                        (.visitInsn mv RETURN))))
            (.visitLabel mv end-lbl)
            (.visitInsn mv RETURN)
            (.visitMaxs mv (int 4) (int 2))
            (.visitEnd mv))
        (.visitEnd cw)
        (.toByteArray cw)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private class-cache (atom {}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn load-program
    [src]
    (or
        (get @class-cache src)
        (let
            [bc
                (compile-to-class src)
                cls-name
                (.replace (gen-class-name src) "/" ".")
                dcl
                (DynamicClassLoader.)
                cls
                (.defineClass dcl cls-name bc nil)
                eval-field
                (.getField cls "EVAL_FN")
                _
                (.set eval-field nil SNOBOL4clojure.operators/EVAL!)
                inline-fn-vals
                {"$$_FN" $$_FN "SET_FN" SET_FN "PRINT_FN" PRINT_FN "ADD_FN" ADD_FN "SUB_FN" SUB_FN "MUL_FN" MUL_FN "DIV_FN" DIV_FN "LT_FN" LT_FN "GT_FN" GT_FN "LE_FN" LE_FN "GE_FN" GE_FN "EQ_FN" EQ_FN
                    "NEQ_FN" NEQ_FN "SIGNAL_FN" SIGNAL_FN "CONCAT_FN" CONCAT_FN "SIZE_FN" SIZE_FN "STR_FN" STR_FN}
                _
                (doseq [[fld-name fld-val] inline-fn-vals] (.set (.getField cls fld-name) nil fld-val))
                method
                (.getMethod cls "run" (into-array Class []))
                run-fn
                (fn [] (.set eval-field nil SNOBOL4clojure.operators/EVAL!) (.invoke method nil (into-array Object [])))]
            (swap! class-cache assoc src run-fn)
            run-fn)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn clear-class-cache! [] (reset! class-cache {}) nil)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-jvm!
    [src]
    (env/GLOBALS (find-ns 'SNOBOL4clojure.runtime))
    (reset! env/STNO 0)
    (reset! env/<STNO> {})
    (reset! env/<LABL> {})
    (reset! env/<CODE> {})
    (reset! env/<FUNS> {})
    (reset! &STCOUNT 0)
    (let
        [run-fn (load-program src)]
        (try
            (run-fn)
            (catch java.lang.reflect.InvocationTargetException e
                (let
                    [cause (.getCause e)]
                    (if
                        (instance? clojure.lang.ExceptionInfo cause)
                        (when-not (contains? #{:end :return :freturn :nreturn} (get (ex-data cause) :snobol/signal)) (throw cause))
                        (throw (or cause e))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-jvm-captured! [src] (with-out-str (run-jvm! src)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn bench-compare-jvm
    [src n]
    (let
        [reset-env!
            (fn [] (env/GLOBALS (find-ns 'SNOBOL4clojure.runtime)) (reset! env/STNO 0) (reset! env/<STNO> {}) (reset! env/<LABL> {}) (reset! env/<CODE> {}) (reset! env/<FUNS> {}))
            run-interp
            (fn
                []
                (reset-env!)
                (with-out-str (try (RUN (comp/CODE src)) (catch clojure.lang.ExceptionInfo e (when-not (contains? #{:end :return :freturn :nreturn} (get (ex-data e) :snobol/signal)) (throw e))))))
            run-fn
            (do (reset-env!) (load-program src))
            run-jvm
            (fn
                []
                (reset-env!)
                (with-out-str
                    (try
                        (run-fn)
                        (catch java.lang.reflect.InvocationTargetException e
                            (let
                                [cause (.getCause e)]
                                (when-not
                                    (and (instance? clojure.lang.ExceptionInfo cause) (contains? #{:end :return :freturn :nreturn} (get (ex-data cause) :snobol/signal)))
                                    (throw (or cause e))))))))]
        (dotimes [_ 30] (run-interp))
        (dotimes [_ 30] (run-jvm))
        (let
            [i-out
                (atom "")
                j-out
                (atom "")
                t0
                (System/nanoTime)
                _
                (dotimes [_ n] (reset! i-out (run-interp)))
                i-ms
                (/ (/ (- (System/nanoTime) t0) n) 1e6)
                t1
                (System/nanoTime)
                _
                (dotimes [_ n] (reset! j-out (run-jvm)))
                j-ms
                (/ (/ (- (System/nanoTime) t1) n) 1e6)]
            {:interpreter-ms (double i-ms) :jvm-ms (double j-ms) :ratio (double (/ i-ms j-ms)) :outputs-match (clojure.core/= @i-out @j-out)})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(init-inline-fns!)
