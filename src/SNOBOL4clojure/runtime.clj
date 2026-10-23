(ns SNOBOL4clojure.runtime
  "The SNOBOL4 GOTO-driven statement interpreter.

  ## &STCOUNT / &STLIMIT  (18B.1-18B.2)
  &STCOUNT is reset to 0 at RUN entry and incremented on every statement
  execution (the inner when-let [stmt ...] branch — i.e. every real statement
  body dispatch, not blank/comment lines).
  When &STCOUNT exceeds &STLIMIT a :step-limit ExceptionInfo is thrown.
  Default &STLIMIT is 2147483647 (effectively unlimited).
  Tests set it low to catch infinite loops without waiting for wall-clock timeout.

  ## &TRACE / TRACE hooks  (18B TRACE feature)
  LABEL traces fire at every statement whose key has a registration.
  Statement-level trace (wildcard '*') prints each statement number + label.
  Pattern traces are handled in match.clj via *trace* dynamic var."
  (:require [SNOBOL4clojure.env   :refer [<STNO> <LABL> <CODE> <DISPNO> block-end
                                           &STCOUNT &STLIMIT &TRACE &STNO &LASTNO &ERRLIMIT &ERRTYPE &ERRTEXT <SETEXIT> <POS> &FILE &LASTFILE &LINE &LASTLINE &FNCLEVEL
                                           snobol-return! snobol-freturn!
                                           snobol-nreturn! snobol-end!
                                           snobol-steplimit!]]
            [SNOBOL4clojure.trace :refer [fire-label! fire-keyword! statement-trace-active?]]
            [SNOBOL4clojure.operators :refer [EVAL! INVOKE]]))

;; Special goto targets that trigger control-flow signals rather than label jumps. "If case-folding is in effect, they may appear with any combination of upper- or
;; lower-case letters" (manual ch. 16, Special names); without folding, as sbl -bf runs, RETURN, FRETURN and NRETURN are special in capitals only (:(return) is a label, or
;; ERROR 038), and END as END or end. Folded source arrives in capitals.
(def ^:private special-targets
  #{"RETURN" "FRETURN" "NRETURN" "END" "end"
    :RETURN :FRETURN :NRETURN :END :end})

(defn- dispatch-special! [target]
  (case (clojure.string/upper-case (name target))
    "RETURN"  (snobol-return!)
    "FRETURN" (snobol-freturn!)
    "NRETURN" (snobol-nreturn!)
    "END"     (snobol-end!)))

;; An execution error under &ERRLIMIT not 0 is not fatal: &ERRLIMIT drops by one, &ERRTYPE and &ERRTEXT record the error, nothing is written, and control goes to the label
;; SETEXIT armed (which is then disarmed, so a handler that wants the next error arms it again), or else the statement fails.
;; With &ERRLIMIT at 0 the error goes on up and ends the program.
(defrecord Trapped [label ex])

;; e, an error raised in a statement's body or its Goto field: trapped under &ERRLIMIT not 0, it gives a Trapped (a handler was armed) or false (the statement fails).
;; An error with &ERRLIMIT at 0, one that ABORT passes on, and every other signal go on up.
(defn- trap [^clojure.lang.ExceptionInfo e]
  (let [{n :snobol/error abort :snobol/abort} (ex-data e)]
    (if (and n (not abort) (pos? (long @&ERRLIMIT)))
      (let [h @<SETEXIT>]
        (swap! &ERRLIMIT dec)
        (reset! &ERRTYPE n)
        (fire-keyword! "ERRTYPE" n)
        (reset! &ERRTEXT (ex-message e))
        (if h
          (do (reset! <SETEXIT> nil) (->Trapped h e))
          false))
      (throw e))))

(defn- exec-body [body]
  (try
    (or (nil? body) (EVAL! body))
    (catch clojure.lang.ExceptionInfo e (trap e))))

(defn- label-name [tgt] (when (or (keyword? tgt) (string? tgt) (symbol? tgt)) (name tgt)))

;; The END statement is executed like any other when control reaches it, by falling in or by a goto: it counts in &STCOUNT and is the statement &STNO and &LINE name afterwards.
(defn- end-statement! []
  (when-let [n (@<STNO> :END)]
    (when-not (neg? (long @&STLIMIT)) (swap! &STCOUNT inc))
    (reset! &LASTNO @&STNO)
    (reset! &STNO (get @<DISPNO> n n))
    (reset! &LASTFILE @&FILE)
    (reset! &LASTLINE @&LINE)
    (let [[f l] (get @<POS> n ["" 0])] (reset! &FILE f) (reset! &LINE l))))

(defn RUN [at]
  (letfn
    [(skey  [address] (let [[no label] address] (if label label no)))
     ;; Resolve a goto target to a [stmt-no label] address pair.
     ;; Target can be:
     ;;   keyword  :FOO  — label name (from emitter); may actually be a variable
     ;;                    holding a label name at runtime — try both
     ;;   string   "FOO" — label name as string (returned by CODE() at runtime)
     ;;   integer  N     — direct statement number
     (saddr [at]      (cond
                        ;; a label names a statement; a variable that holds a label's name is not one (sbl -bf: :(X) with X = 'LAB' is ERROR 038)
                        (keyword? at)
                          (when-let [n (@<STNO> at)] [n at])
                        (string?  at) (or (when-let [n (@<STNO> (keyword at))] [n (keyword at)])
                                          ;; Numeric string (e.g. CODE() returned stmt# as integer, stored as str)
                                          (when (re-matches #"[0-9]+" at)
                                            (let [n (Long/parseLong at)]
                                              [n (@<LABL> n)]))
                                          (when (@<CODE> at) [(@<STNO> at) at]))
                        (integer? at) [at (@<LABL> at)]))
     ;; A computed goto, :($X) or :S($('WANG.A.' OP)), names its label by the value of the expression: a string (a label, RETURN, END) or an integer.
     ;; It is resolved here, once, so that the jump and the special labels see the label itself. A target that is not a $ form is passed on as it was.
     ;; A direct Goto, :<V>, goes to the start of the code V holds; anything else there is "24 Goto operand in direct goto is not code" (manual, Appendix D).
     (computed-target [tgt]
       (cond
         (and (list? tgt) (clojure.core/= '$ (first tgt)) (clojure.core/= 2 (count tgt)))
         (when-some [v (EVAL! (second tgt))]
           (if (integer? v) v (keyword (str v))))
         (and (list? tgt) (clojure.core/= 'DIRECT (first tgt)))
         (when-some [v (EVAL! (second tgt))]
           (if (instance? SNOBOL4clojure.env.CodeBlock v)
             (.entry ^SNOBOL4clojure.env.CodeBlock v)
             (throw (ex-info "goto operand in direct goto is not code" {:snobol/error 24}))))
         ;; a function call is a call by name: the name it returns by NRETURN is the label (:(RET('C2')) goes to RETURN when RET returns .RETURN); a value returned
         ;; is "21 Function called by name returned a value", and a failure "20 Goto evaluation failure" (manual, Appendix D), as sbl -bf has them
         (and (list? tgt) (symbol? (first tgt)) (not (:subscript (meta tgt))))
         (let [args (mapv EVAL! (rest tgt))
               _    (when (some nil? args) (throw (ex-info "goto evaluation failure" {:snobol/error 20})))
               r    (apply INVOKE (first tgt) args)]
           (cond (instance? SNOBOL4clojure.env.NameResult r) (let [t (:target r)] (if (symbol? t) (keyword (name t)) t))
                 (nil? r) (throw (ex-info "goto evaluation failure" {:snobol/error 20}))
                 :else    (throw (ex-info "function called by name returned a value" {:snobol/error 21}))))
         :else tgt))
     (goto! [tgt]
       (if (special-targets tgt)
         (dispatch-special! tgt)
         tgt))
     ;; "38 Goto undefined label" (manual, Appendix D)
     (label-addr [tgt] (or (saddr tgt) (throw (ex-info "goto undefined label" {:snobol/error 38}))))
     ;; Where control goes after a statement that succeeded (r) or failed: [address err]. A Goto-field error is thrown, to be trapped as a body's is.
     ;; CONTINUE resumes at the failure exit of the statement in error; ABORT lets the error end the program, as though no intercept had been set.
     (step [r goto err current]
       (let [g   (when (map? goto) goto)
             tgt (computed-target (if r (or (:G g) (:S g)) (or (:G g) (:F g))))
             nm  (label-name tgt)]
         (cond
           (and err (clojure.core/= nm "CONTINUE"))
           (let [g2   (:goto err)
                 etgt (computed-target (when (map? g2) (or (:G g2) (:F g2))))]
             (if etgt
               (do (goto! etgt) [(label-addr etgt) nil])
               [(saddr (inc ((:at err) 0))) nil]))
           (and err (clojure.core/= nm "ABORT"))
           (let [e ^clojure.lang.ExceptionInfo (:ex err)] (throw (ex-info (ex-message e) (assoc (ex-data e) :snobol/abort true))))
           tgt
           (do (goto! tgt) (let [a (label-addr tgt)] (when (pos? (long @&TRACE)) (fire-label! (or (second a) tgt))) [a err]))
           :else
           [(saddr (inc (current 0))) err])))]
    ;; a program's run starts &STCOUNT at 0; the run of a function body, one level down, goes on counting
    (when (zero? (long @&FNCLEVEL)) (reset! &STCOUNT 0))
    (try
      (loop [current (saddr at) err nil]
        (when-let [key (skey current)]
          ;; Special-label check (e.g. labelled RETURN/END)
          (when (special-targets key)
            (dispatch-special! key))
          (when-let [stmt (@<CODE> key)]
            ;; ── &STCOUNT / &STLIMIT ──────────────────────────────────────────
            (reset! &LASTNO @&STNO)
            (reset! &LASTFILE @&FILE)
            (reset! &LASTLINE @&LINE)
            (let [[f l] (get @<POS> (first current) ["" 0])] (reset! &FILE f) (reset! &LINE l))
            (reset! &STNO (get @<DISPNO> (first current) (first current)))
            ;; a negative &STLIMIT is unlimited execution, and &STCOUNT stops counting; 0 is no limit too, as sbl -bf counts (its countdown starts at the limit,
            ;; sbl.min stgcc, so at 0 the check is never reached), and &STCOUNT goes on
            (let [n (if (neg? (long @&STLIMIT)) @&STCOUNT (swap! &STCOUNT inc))]
              ;; "tracing keyword &STCOUNT produces a trace after every SPITBOL statement" (manual ch. 16): as the count moves; a negative &STLIMIT moves none
              (when (and (pos? (long @&TRACE)) (not (neg? (long @&STLIMIT)))) (fire-keyword! "STCOUNT" n))
              (when (and (pos? (long @&STLIMIT)) (> n @&STLIMIT))
                (snobol-steplimit! n))
              ;; ── LABEL trace ────────────────────────────────────────────────
              (when (pos? @&TRACE)
                (when (statement-trace-active?)
                  (binding [*out* *err*]
                    (println (format ">>> stmt %5d  %s" (dec n) (str key)))))))
            ;; the statement closing a CODE block ends the program, itself the last statement executed
            (when (identical? stmt block-end) (throw (ex-info "END" {:snobol/signal :end :snobol/closing true})))
            ;; ── Execute statement ────────────────────────────────────────────
            (let [ferst  (first stmt)
                  seqond (second stmt)
                  body   (if (map? ferst) seqond ferst)
                  goto   (if (map? ferst) ferst  seqond)]
              (let [r (exec-body body)]
                (if (instance? Trapped r)
                  ;; control goes to the handler; CONTINUE and ABORT there refer back to the statement in error
                  (recur (saddr (:label r)) {:at current :goto goto :ex (:ex r)})
                  ;; an error in the Goto field is trapped like one in the body: to the handler, or the statement fails and its failure exit is taken,
                  ;; which may be the same Goto again until &ERRLIMIT runs out (manual ch. 16, &ERRLIMIT)
                  (let [[nxt err'] (loop [r r]
                                     (let [res (try (step r goto err current)
                                                    (catch clojure.lang.ExceptionInfo e (let [t (trap e)] (if t t ::fail))))]
                                       (cond
                                         (instance? Trapped res) [(saddr (:label res)) {:at current :goto goto :ex (:ex res)}]
                                         (clojure.core/= ::fail res) (recur false)
                                         :else res)))]
                    (recur nxt err'))))))))
      (catch clojure.lang.ExceptionInfo e
        (case (get (ex-data e) :snobol/signal)
          :end        (if (pos? (long @&FNCLEVEL)) (throw e) (do (when-not (:snobol/closing (ex-data e)) (end-statement!)) nil))   ; program end, from any level
          :return     nil          ; bubble up to DEFINE wrapper
          :step-limit (throw e)    ; propagate — caught by test-helpers/harness
          :freturn    (throw e)    ; bubble up to DEFINE wrapper
          :nreturn    (throw e)    ; bubble up to DEFINE wrapper
          (throw e))))))
