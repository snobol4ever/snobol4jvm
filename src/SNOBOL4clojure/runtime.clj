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
  (:require [SNOBOL4clojure.env   :refer [<STNO> <LABL> <CODE> <DISPNO> block-end compile-error
                                           &STCOUNT &STLIMIT &TRACE &STNO &LASTNO &ERRLIMIT &ERRTYPE &ERRTEXT <POS> &FILE &LASTFILE &LINE &LASTLINE &FNCLEVEL
                                           snobol-return! snobol-freturn!
                                           snobol-nreturn! snobol-end!
                                           snobol-steplimit!]]
            [SNOBOL4clojure.trace :refer [fire-label! fire-keyword! statement-trace-active?]]
            [SNOBOL4clojure.operators :refer [EVAL! INVOKE error-decided]]))

;; Special goto targets that trigger control-flow signals rather than label jumps. "If case-folding is in effect, they may appear with any combination of upper- or
;; lower-case letters" (manual ch. 16, Special names); without folding, as sbl -bf runs, RETURN, FRETURN and NRETURN are special in capitals only (:(return) is a label, or
;; ERROR 038), and END as END or end. Folded source arrives in capitals.
(def ^:private special-targets
  #{"RETURN" "FRETURN" "NRETURN" "END" "end"
    :RETURN :FRETURN :NRETURN :END :end})

;; The system labels a SETEXIT handler may be, which act at once (run* enter); SCONTINUE is not yet one a Goto takes
(def ^:private system-labels #{"CONTINUE" "ABORT" "RETURN" "FRETURN" "NRETURN" "END" "end"})

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

;; A handler running at its error (RUN-AT-ERROR) that can no longer continue it -- a later error took the continuation over or used it up, or control fell past
;; the last statement -- hands the statement it goes on at, and the error a CONTINUE there refers to, to the run of the statement in error, at that statement's end.
(defrecord Transfer [at err])

;; e, an error raised in a statement's body or its Goto field: trapped under &ERRLIMIT not 0, it gives a Trapped (a handler was armed) or false (the statement fails).
;; An error with &ERRLIMIT at 0, one that ABORT passes on, and every other signal go on up -- but in a function a pattern match called, while the match is in
;; progress, an error with &ERRLIMIT at 0 fails the statement, SETEXIT not consulted (sbl -bf, measured: a division by zero in a function called from
;; 'x' ? ('x' . *G()) takes the statement's :F branch with &ERRTYPE 14, and the program goes on; at &ERRLIMIT 5 it goes to the label SETEXIT armed).
;; The statement is the innermost failure point when no other in it caught the error (operators.clj error-decided). A handler's transfer gives a Transfer.
(defn- trap [^clojure.lang.ExceptionInfo e]
  (if (clojure.core/= :transfer (:snobol/signal (ex-data e)))
    (->Transfer (:snobol/at (ex-data e)) (:snobol/err (ex-data e)))
    (let [e (error-decided e SNOBOL4clojure.env/*fp*) {to :snobol/to h :snobol/label} (ex-data e)]
      (cond h    (->Trapped h e)
            to   false
            :else (throw e)))))

;; "if stlimit is exceeded, up to 10 more statements are obeyed so that a setexit trap can regain control" (sbl.min stcov): under &ERRLIMIT the overrun is
;; error 244, trapped as an error in the statement is, after &STLIMIT is raised by 10; it is a fatal error, and at the third the program ends whatever &ERRLIMIT
;; is (err07), as it ends with &ERRLIMIT at 0 (sbl -bf: SETEXIT(.OVER) at &ERRLIMIT 1 and &STLIMIT 20 reaches OVER with &ERRTYPE 244 and &STLIMIT 30).
(defn- overrun [n]
  (if (and (pos? (long @&ERRLIMIT)) (< (long (swap! SNOBOL4clojure.env/<ERRFT> inc)) 3))
    (do (swap! &STLIMIT clojure.core/+ 10)
        (ex-info (SNOBOL4clojure.env/error-message 244) {:snobol/error 244}))
    (snobol-steplimit! n)))

;; An error SPITBOL counts fatal (icv errft before erb) is trapped as any error is, but the third such ends the program whatever &ERRLIMIT is (sbl.min err07).
(defn- fatal-error [n]
  (let [msg (SNOBOL4clojure.env/error-message n)]
    (if (< (long (swap! SNOBOL4clojure.env/<ERRFT> inc)) 3)
      (ex-info msg {:snobol/error n})
      (ex-info msg {:snobol/error n :snobol/abort true}))))

(defn- exec-body [body]
  (try
    (or (nil? body) (EVAL! body))
    (catch clojure.lang.ExceptionInfo e (trap e))))

(defn- label-name [tgt] (when (or (keyword? tgt) (string? tgt) (symbol? tgt)) (name tgt)))

;; The END statement is executed like any other when control reaches it, by falling in or by a goto: it counts in &STCOUNT, fires the &STCOUNT trace as another
;; statement does (sbl -bf: ****6*******  &STCOUNT = 6 for the END of a five-statement program), and is the statement &STNO and &LINE name afterwards.
(defn- end-statement! []
  (when-let [n (@<STNO> :END)]
    (let [counted (when-not (neg? (long @&STLIMIT)) (swap! &STCOUNT inc))]
      (reset! &LASTNO @&STNO)
      (reset! &STNO (get @<DISPNO> n n))
      (reset! &LASTFILE @&FILE)
      (reset! &LASTLINE @&LINE)
      (let [[f l] (get @<POS> n ["" 0])] (reset! &FILE f) (reset! &LINE l))
      (when (and counted (pos? (long @&TRACE))) (fire-keyword! "STCOUNT" counted)))))

;; The statements from the one at names on, err0 the error a CONTINUE refers to; k, for a handler running at its error, that error's serial (RUN-AT-ERROR), else nil.
(defn- run* [at err0 k]
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
     ;; CONTINUE goes on from the latest error only: an error no handler takes goes on at once and uses the continuation up, so a CONTINUE after it, or with no
     ;; error before it, is "37 goto continue with no preceding error", a fatal error a handler armed again takes (sbl.min lcnt1, lcnt3; sbl -bf: SETEXIT('E')
     ;; :(CONTINUE) with no error reaches E with &ERRTYPE 37; a handler whose own error goes untrapped and that then continues reaches the next handler with 37).
     (step [r goto err current]
       (let [g   (when (map? goto) goto)
             tgt (computed-target (if r (or (:G g) (:S g)) (or (:G g) (:F g))))
             nm  (label-name tgt)]
         (cond
           ;; a handler running at its error continues it: the failure point it was raised in fails (operators.clj at-error)
           (and (clojure.core/= nm "CONTINUE") (:resume err) (clojure.core/= (:resume err) @SNOBOL4clojure.env/<ERRSERIAL>))
           [::resume nil]
           (clojure.core/= nm "CONTINUE")
           (if (and err (clojure.core/= (:snobol/serial (ex-data (:ex err))) @SNOBOL4clojure.env/<ERRSERIAL>))
             (let [g2 (:goto err)
                   f2 (when (map? g2) (or (:G g2) (:F g2)))]
               ;; an error in the Goto field of a statement whose failure goto is computed or direct cannot go on there: "332 goto continue with error in
               ;; failure goto", a fatal error, and the statement in error stays the one a CONTINUE goes on from (sbl.min lcnt4; sbl -bf: X = 1 :<Z> of a Z
               ;; holding no CODE is 24, then 332 at each CONTINUE; :($V), :F($V) and :S(UNDEF)F($V) likewise after 38; :S($V)F(L2) goes on to L2, :S<Z>
               ;; and :S($V) to the next statement, and :(UNDEF) is 38 again)
               (if (and (:in-goto err) f2 (not (keyword? f2)))
                 (throw (let [e (fatal-error 332)] (ex-info (ex-message e) (assoc (ex-data e) ::continued err))))
                 (let [etgt (computed-target f2)]
                   (if etgt
                     (do (goto! etgt) [(label-addr etgt) nil])
                     [(saddr (inc ((:at err) 0))) nil]))))
             (throw (fatal-error 37)))
           ;; "ABORT causes error processing to resume as though no error intercept had been set" (manual ch. 19, SETEXIT): the error goes on up without the
           ;; handler it was trapped to, which took it again and again (sbl -bf: a handler's :(ABORT) after 1 / Z prints ERROR 014 and the program ends)
           (and err (clojure.core/= nm "ABORT"))
           (let [e ^clojure.lang.ExceptionInfo (:ex err)]
             (throw (ex-info (ex-message e) (-> (ex-data e) (dissoc :snobol/label :snobol/serial :snobol/fp :snobol/resume) (assoc :snobol/abort true)))))
           ;; elsewhere ABORT ends the program with the last error, &ERRTYPE, however long ago it was trapped or continued; with none it is "036 goto abort with
           ;; no preceding error", which a handler takes as any error (sbl.min l_abo: kvert, labo3; sbl -bf: after 1 + Y was trapped, :(ABORT) prints ERROR 002
           ;; at the statement that took it; with no error the program ends with 036, or SETEXIT's handler runs with &ERRTYPE 36). It was 038, no such label.
           (clojure.core/= nm "ABORT")
           (let [n (long @&ERRTYPE)]
             (throw (if (zero? n)
                      (ex-info (SNOBOL4clojure.env/error-message 36) {:snobol/error 36})
                      (ex-info (SNOBOL4clojure.env/error-message n) {:snobol/error n :snobol/abort true}))))
           tgt
           (do (goto! tgt) (let [a (label-addr tgt)] (when (pos? (long @&TRACE)) (fire-label! (or (second a) tgt))) [a err]))
           :else
           [(saddr (inc (current 0))) err])))
     ;; Where control goes after the statement at current succeeded (r) or failed, by its Goto field goto: [address err]. An error in the Goto field is trapped
     ;; like one in the body: to the handler, or the statement fails and its failure exit is taken, which may be the same Goto again until &ERRLIMIT runs out
     ;; (manual ch. 16, &ERRLIMIT).
     (follow [r goto err current]
       (loop [r r]
         (let [res (try (step r goto err current)
                        (catch clojure.lang.ExceptionInfo e (let [t (trap e)] (if t t ::fail))))]
           (cond
             (instance? Transfer res) [(:at res) (:err res)]
             (instance? Trapped res) (enter (:label res) (if-let [e0 (::continued (ex-data (:ex res)))]
                                                           (assoc e0 :ex (:ex res))
                                                           {:at current :goto goto :ex (:ex res) :in-goto true})
                                            current)
             (clojure.core/= ::fail res) (recur false)
             :else res))))
     ;; The handler SETEXIT armed takes control: [address err], err the error a CONTINUE there refers to. A system label acts at once, as the handler's own
     ;; goto to it would: CONTINUE continues the error, ABORT ends the program with it, RETURN, FRETURN and NRETURN return from the function the error is in,
     ;; END ends the program (sbl.min err07 executes the label's code, r_sxc; sbl -bf: under SETEXIT(.CONTINUE), X = 1 / Z takes F and Y = ~(1 / Z) succeeds;
     ;; SETEXIT('RETURN') in F returns F's value; SETEXIT('ABORT') ends with the error's report). The engine looked for a statement with that label, found
     ;; none, and ended the program in silence.
     (enter [label err current]
       (if (system-labels (name label))
         (follow true {:G label} err current)
         [(saddr label) err]))]
    ;; a program's run starts &STCOUNT at 0; the run of a function body, one level down, or of a handler at its error goes on counting
    (when (and (nil? k) (zero? (long @&FNCLEVEL))) (reset! &STCOUNT 0))
    (try
      (loop [[current err] (if k (enter at err0 nil) [(saddr at) err0])]
        (cond
          (identical? ::resume current) ::resume
          (and k (or (nil? current) (not (clojure.core/= k @SNOBOL4clojure.env/<ERRSERIAL>))))
          (throw (ex-info "transfer" {:snobol/signal :transfer :snobol/at current :snobol/err err}))
          :else
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
            (let [over
            (let [n (if (neg? (long @&STLIMIT)) @&STCOUNT (swap! &STCOUNT inc))]
              ;; "tracing keyword &STCOUNT produces a trace after every SPITBOL statement" (manual ch. 16): as the count moves; a negative &STLIMIT moves none
              (when (and (pos? (long @&TRACE)) (not (neg? (long @&STLIMIT)))) (fire-keyword! "STCOUNT" n))
              (let [over (when (and (pos? (long @&STLIMIT)) (> n @&STLIMIT)) (overrun n))]
                ;; ── LABEL trace ────────────────────────────────────────────────
                (when (pos? @&TRACE)
                  (when (statement-trace-active?)
                    (binding [*out* *err*]
                      (println (format ">>> stmt %5d  %s" (dec n) (str key))))))
                over))]
            ;; the statement closing a CODE block ends the program, itself the last statement executed
            (when (identical? stmt block-end) (throw (ex-info "END" {:snobol/signal :end :snobol/closing true})))
            ;; ── Execute statement ────────────────────────────────────────────
            (let [ferst  (first stmt)
                  seqond (second stmt)
                  body   (if (map? ferst) seqond ferst)
                  goto   (if (map? ferst) ferst  seqond)]
              ;; a statement the limit stops is not executed: the overrun is trapped as an error in it is
              (let [r (cond over                           (trap over)
                            ;; the statement in error of a block CODE failed to compile is error 7 when control reaches it
                            (identical? stmt compile-error)  (trap (ex-info (SNOBOL4clojure.env/error-message 7) {:snobol/error 7}))
                            :else                            (exec-body body))]
                (cond
                  (instance? Transfer r) (recur [(:at r) (:err r)])
                  (instance? Trapped r)
                  ;; control goes to the handler; CONTINUE and ABORT there refer back to the statement in error
                  (recur (enter (:label r) {:at current :goto goto :ex (:ex r)} current))
                  :else
                  (recur (follow r goto err current))))))))))
      (catch clojure.lang.ExceptionInfo e
        ;; a handler at its error leaves every signal to the run of the statement in error
        (if k
          (throw e)
          (case (get (ex-data e) :snobol/signal)
            :end        (if (pos? (long @&FNCLEVEL)) (throw e) (do (when-not (:snobol/closing (ex-data e)) (end-statement!)) nil))   ; program end, from any level
            :return     nil          ; bubble up to DEFINE wrapper
            :step-limit (throw e)    ; propagate — caught by test-helpers/harness
            :freturn    (throw e)    ; bubble up to DEFINE wrapper
            :nreturn    (throw e)    ; bubble up to DEFINE wrapper
            (throw e)))))))

(defn RUN [at] (run* at nil nil))

;; The handler h of the error e, serial k, running at the error (operators.clj at-error): ::resume when it continues; it leaves any other way by a signal, a
;; Transfer among them.
(defn RUN-AT-ERROR [h k e] (run* h {:resume k :ex e} k))
