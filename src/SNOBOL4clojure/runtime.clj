(ns SNOBOL4clojure.runtime
  "The two execution engines over one IR: the interpreter, the reference, and JVM bytecode, the low-level one."
  (:require [SNOBOL4clojure.env :as env :refer [<STNO> <LABL> <CODE> <DISPNO> block-end compile-error &STCOUNT &STLIMIT &TRACE &STNO &LASTNO &ERRLIMIT &ERRTYPE &ERRTEXT <POS> &FILE &LASTFILE
                                                &LINE &LASTLINE &FNCLEVEL snobol-return! snobol-freturn! snobol-nreturn! snobol-end! snobol-steplimit! fire-label! fire-keyword!
                                                statement-trace-active?]]
            [SNOBOL4clojure.operators :refer [EVAL! INVOKE error-decided]]
            [clojure.string :as str]
            [SNOBOL4clojure.compiler :as comp])
  (:import  [clojure.asm ClassWriter MethodVisitor Label Opcodes]
            [clojure.lang DynamicClassLoader IFn RT]))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; The interpreter
;;
;; The SNOBOL4 GOTO-driven statement interpreter.
;;
;; ## &STCOUNT / &STLIMIT  (18B.1-18B.2)
;; &STCOUNT is reset to 0 at RUN entry and incremented on every statement
;; execution (the inner when-let [stmt ...] branch — i.e. every real statement
;; body dispatch, not blank/comment lines).
;; When &STCOUNT exceeds &STLIMIT a :step-limit ExceptionInfo is thrown.
;; Default &STLIMIT is 2147483647 (effectively unlimited).
;; Tests set it low to catch infinite loops without waiting for wall-clock timeout.
;;
;; ## &TRACE / TRACE hooks  (18B TRACE feature)
;; LABEL traces fire at every statement whose key has a registration.
;; Statement-level trace (wildcard '*') prints each statement number + label.
;; Pattern traces are handled in match.clj via *trace* dynamic var.
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; Special goto targets that trigger control-flow signals rather than label jumps. "If case-folding is in effect, they may appear with any combination of upper- or
;; lower-case letters" (manual ch. 16, Special names); without folding, as sbl -bf runs, RETURN, FRETURN and NRETURN are special in capitals only (:(return) is a label, or
;; ERROR 038), and END as END or end. Folded source arrives in capitals.
(def ^:private special-targets
  #{"RETURN" "FRETURN" "NRETURN" "END" "end"
    :RETURN :FRETURN :NRETURN :END :end})

;; The system labels a SETEXIT handler may be, which act at once (run* enter); SCONTINUE is not yet one a Goto takes
(def ^:private system-labels #{"CONTINUE" "ABORT" "RETURN" "FRETURN" "NRETURN" "END" "end"})

;; A return at function level 0 is "242 function return from level zero": the label SETEXIT armed takes it under &ERRLIMIT, and with none armed it ends the program
;; whatever &ERRLIMIT holds (MEASURED on sbl -bf: &ERRLIMIT = 10 then :(RETURN), :(FRETURN) or :(NRETURN) goes to SETEXIT's H with &ERRTYPE 242; with no SETEXIT
;; the run ends at ERROR 242; corpus gate_cases_snobol4 test_gate_errlimit_survival_is_measured_per_code__e242, which this engine ended quietly)
(defn- level-zero-return! []
  (when (zero? (long @SNOBOL4clojure.env/&FNCLEVEL))
    (throw (ex-info (SNOBOL4clojure.env/error-message 242)
                    (if (and (pos? (long @&ERRLIMIT)) @SNOBOL4clojure.env/<SETEXIT>) {:snobol/error 242} {:snobol/error 242 :snobol/abort true})))))

(defn- dispatch-special! [target]
  (when (#{"RETURN" "FRETURN" "NRETURN"} (clojure.string/upper-case (name target))) (level-zero-return!))
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
;; stcov raises &STLIMIT by 10 whatever &ERRLIMIT holds, and &STCOUNT reads the limit it reached (sbl.min stcov: "add to former limit", "set as new count" 10; MEASURED
;; on sbl -bf: &STLIMIT = 20 ends at ERROR 244 with &STCOUNT 20 and &STLIMIT 30 in the dump, at &ERRLIMIT 0; corpus spitbol_88 DEMOS/GOTOS)
(defn- overrun [n]
  (let [lim (long @&STLIMIT)]
    (reset! &STCOUNT lim)
    (swap! &STLIMIT clojure.core/+ 10)
    (if (and (pos? (long @&ERRLIMIT)) (< (long (swap! SNOBOL4clojure.env/<ERRFT> inc)) 3))
      (ex-info (SNOBOL4clojure.env/error-message 244) {:snobol/error 244})
      (snobol-steplimit! n))))

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
      (let [[f l] (get @<POS> n ["" 0])] (reset! &FILE f) (reset! &LINE l) (reset! SNOBOL4clojure.env/<STMT-LINE> l))
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
           ;; SCONTINUE resumes after a user interrupt, and this engine takes none, so a Goto to it is always a fatal error (sbl.min l_scn): "331 goto scontinue
           ;; with no user interrupt" while a continuation is pending (r_cnt, set by a trapped error and used up by a CONTINUE, as CONTINUE's own test above
           ;; reads it), else "321 goto scontinue with no preceding error"; each counts in errft, so the third such ends the program whatever &ERRLIMIT is (err07).
           ;; MEASURED on sbl -bf: a handler that arms SETEXIT again and goes :(SCONTINUE) is reached twice more with &ERRTYPE 331 and the third ends the program
           ;; with ERROR 331, &ERRLIMIT reset to 10 or not; with no handler armed the 331's own continuation runs on to ERROR 321; csnobol4_suite setexit4 reads
           ;; the 331s. It was 038, no such label.
           (clojure.core/= nm "SCONTINUE")
           (throw (fatal-error (if (and err (or (and (:resume err) (clojure.core/= (:resume err) @SNOBOL4clojure.env/<ERRSERIAL>))
                                                (clojure.core/= (:snobol/serial (ex-data (:ex err))) @SNOBOL4clojure.env/<ERRSERIAL>)))
                                 331 321)))
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
            ;; a statement with no place in a file, the one closing a CODE block, keeps the file and the line before it (MEASURED on sbl -bf: a program that ends
            ;; in its CODE block's closing statement dumps &FILE its own name and &LINE 3, the block's line; corpus rosetta-snobol4 runtime-evaluation-2)
            (when-some [[f l] (get @<POS> (first current))] (reset! &FILE f) (reset! &LINE l) (reset! SNOBOL4clojure.env/<STMT-LINE> l))
            (reset! &STNO (get @<DISPNO> (first current) (first current)))
            ;; a negative &STLIMIT is unlimited execution, and &STCOUNT stops counting; 0 is no limit too, as sbl -bf counts (its countdown starts at the limit,
            ;; sbl.min stgcc, so at 0 the check is never reached), and &STCOUNT goes on
            (let [over
            (let [n (if (neg? (long @&STLIMIT)) @&STCOUNT (swap! &STCOUNT inc))]
              ;; "tracing keyword &STCOUNT produces a trace after every SPITBOL statement" (manual ch. 16): as the count moves; a negative &STLIMIT moves none
              ;; the limit is checked first, and the trace fires only for a statement that will run (sbl.min stgo4: "ile stcov" before "execute keyword trace";
              ;; MEASURED on sbl -bf: an STCOUNT trace function TF under &STLIMIT = 20 is entered for the statement before the limit and ends at its own first
              ;; statement; corpus spitbol_88 DEMOS/GOTOS, which sbl ends at line 48, statement 21, and this engine ended inside the trace function)
              ;; and with an STCOUNT trace armed, or profiling on, SPITBOL counts every statement and stops at the one whose count reaches the limit, where it
              ;; otherwise counts in steps the limit's own size and stops at the one after (sbl.min stgcc, stgo4; MEASURED on sbl -bf: &STLIMIT = 5 and a loop
              ;; print &STCOUNT 2 to 5 and end at the sixth; with TRACE('STCOUNT','KEYWORD') the trace writes 3 and 4 and the fifth statement ends the run)
              (let [step (if (or (SNOBOL4clojure.env/stcount-traced?) (not (zero? (long @SNOBOL4clojure.env/&PROFILE)))) 1 0)
                    over (when (and (pos? (long @&STLIMIT)) (> (+ (long n) step) (long @&STLIMIT))) (overrun n))
                    _    (when (and (not over) (pos? (long @&TRACE)) (not (neg? (long @&STLIMIT)))) (fire-keyword! "STCOUNT" n))]
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
            :end        (if (pos? (long @&FNCLEVEL)) (throw e)
                          (do (when-some [l (:fnclevel (ex-data e))] (reset! &FNCLEVEL l))   ; END inside a function ends the program at that level (sbl -bf)
                              (when-not (:snobol/closing (ex-data e)) (end-statement!)) nil))   ; program end, from any level
            :return     nil          ; bubble up to DEFINE wrapper
            :step-limit (throw e)    ; propagate — caught by test-helpers/harness
            :freturn    (throw e)    ; bubble up to DEFINE wrapper
            :nreturn    (throw e)    ; bubble up to DEFINE wrapper
            (throw e)))))))

(defn RUN [at] (run* at nil nil))

;; The handler h of the error e, serial k, running at the error (operators.clj at-error): ::resume when it continues; it leaves any other way by a signal, a
;; Transfer among them.
(defn RUN-AT-ERROR [h k e] (run* h {:resume k :ex e} k))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; JVM bytecode, part 1: the instruction list
;;
;; The IR's statements flattened to one vector the class generator below walks, each a map {:op :body :s :f}: every label goto resolved to
;; its index, and each statement's goto shape one of seven opcodes (HALT, EXEC, EXEC-S, EXEC-F, EXEC-SF, JUMP, SIGNAL). It was the front end of
;; the VM engine, a loop that ran this vector; the VM was deleted 2026-11-19 (Lon: "delete the memoizer, EDN cache, VM and transpiler"),
;; the bytecode engine being the low-level one kept. To go: the generator reading the IR itself, an ASM Label a statement.
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── Opcode constants ─────────────────────────────────────────────────────────

(def ^:const OP-HALT    0)
(def ^:const OP-EXEC    1)   ; eval body, fall through
(def ^:const OP-EXEC-S  2)   ; eval; success->s, failure->pc+1
(def ^:const OP-EXEC-F  3)   ; eval; success->pc+1, failure->f
(def ^:const OP-EXEC-SF 4)   ; eval; success->s, failure->f
(def ^:const OP-JUMP    5)   ; unconditional goto s
(def ^:const OP-SIGNAL  6)   ; throw snobol/signal in body

;; ── Special-target detection ──────────────────────────────────────────────────

(def ^:private special-names
  #{"RETURN" "FRETURN" "NRETURN" "END"})

(defn- special? [tgt]
  (and tgt (not (integer? tgt))
       (contains? special-names (str/upper-case (name tgt)))))

(defn- signal-for [tgt]
  (case (str/upper-case (name tgt))
    "END"     :end
    "RETURN"  :return
    "FRETURN" :freturn
    "NRETURN" :nreturn))

;; ── IR → bytecode compiler ────────────────────────────────────────────────────
;;
;; We make two passes:
;;   Pass 1: assign a bytecode PC to every IR slot (integer or keyword)
;;   Pass 2: emit instructions with resolved integer jump targets

(defn- assign-pcs
  "Pass 1: build a map from IR key (integer or keyword) to bytecode PC.
   Integer slots that are label placeholders map to the same PC as the keyword."
  [[codes nos labels]]
  ;; All integer slots in order
  (let [all-int   (sort (into (set (filter integer? (keys codes)))
                              (vals nos)))
        kw-slots  (filter keyword? (keys codes))]
    ;; Assign each integer slot a unique PC, unless it's a label placeholder
    ;; (in which case it will redirect to the kw's PC — handled in pass 2)
    (loop [slots all-int
           pc    0
           m     {}]
      (if (empty? slots)
        ;; Assign keyword slots PCs equal to their integer slot PCs
        ;; (because the integer slot will redirect there)
        (reduce (fn [m kw]
                  (let [int-no (get nos kw)
                        kw-pc  (if int-no (get m int-no) pc)]
                    (assoc m kw kw-pc)))
                m kw-slots)
        (recur (rest slots) (inc pc) (assoc m (first slots) pc))))))

(defn compile-ir
  "Compile a [codes nos labels] triple to a flat bytecode vector.
   Returns a vector of instruction maps {:op :body :s :f}."
  [[codes nos labels :as ir]]
  (let [pc-map   (assign-pcs ir)
        ;; Total number of integer slots = total instructions before HALT
        all-int  (sort (into (set (filter integer? (keys codes)))
                             (vals nos)))
        halt-pc  (count all-int)

        ;; Resolve a goto target (keyword, integer, special) to a bytecode PC
        resolve-tgt (fn [tgt]
                      (cond
                        (nil? tgt)        nil
                        (special? tgt)    :signal
                        (integer? tgt)    (get pc-map tgt)
                        (keyword? tgt)    (get pc-map tgt)
                        :else             (get pc-map tgt)))

        ;; Build instruction for one IR slot
        make-instr (fn [key]
                     (let [lbl   (get labels key)    ; is this int slot a label placeholder?
                           code  (if lbl
                                   (get codes lbl)   ; get body from keyword slot
                                   (get codes key))  ; own body
                           [raw-body raw-goto] (if code code [nil nil])
                           body  raw-body
                           goto  raw-goto
                           g-tgt (:G goto)
                           s-tgt (:S goto)
                           f-tgt (:F goto)
                           fall  (+ (get pc-map key) 1)

                           ;; Check for END/RETURN signals
                           s-sig (when (special? s-tgt) (signal-for s-tgt))
                           f-sig (when (special? f-tgt) (signal-for f-tgt))
                           g-sig (when (special? g-tgt) (signal-for g-tgt))]
                       (cond
                         ;; Empty label (END etc) — HALT
                         (and (nil? body) (nil? g-tgt) (nil? s-tgt) (nil? f-tgt)
                              (or lbl (nil? (get codes key))))
                         {:op OP-HALT :body nil :s nil :f nil}

                         ;; Unconditional goto with no body
                         (and (nil? body) g-tgt)
                         (if g-sig
                           {:op OP-SIGNAL :body g-sig :s nil :f nil}
                           {:op OP-JUMP :body nil :s (resolve-tgt g-tgt) :f nil})

                         ;; Body with unconditional goto
                         (and body g-tgt)
                         (if g-sig
                           {:op OP-EXEC :body body :s fall :f fall}  ; signal after?
                           {:op OP-JUMP :body body :s (resolve-tgt g-tgt) :f nil})

                         ;; Body with both S and F
                         (and body s-tgt f-tgt)
                         {:op OP-EXEC-SF
                          :body body
                          :s    (if s-sig :signal (resolve-tgt s-tgt))
                          :f    (if f-sig :signal (resolve-tgt f-tgt))
                          :s-sig s-sig
                          :f-sig f-sig}

                         ;; Body with S only
                         (and body s-tgt)
                         {:op OP-EXEC-S
                          :body body
                          :s    (if s-sig :signal (resolve-tgt s-tgt))
                          :f    fall
                          :s-sig s-sig}

                         ;; Body with F only
                         (and body f-tgt)
                         {:op OP-EXEC-F
                          :body body
                          :s    fall
                          :f    (if f-sig :signal (resolve-tgt f-tgt))
                          :f-sig f-sig}

                         ;; Body, no goto
                         body
                         {:op OP-EXEC :body body :s fall :f fall}

                         ;; No body, no goto — skip
                         :else
                         {:op OP-EXEC :body nil :s fall :f fall})))]

    ;; Emit bytecode vector
    (let [instrs (mapv make-instr all-int)]
      (conj instrs {:op OP-HALT :body nil :s nil :f nil}))))

(defn compile-src
  "Compile SNOBOL4 source to the instruction list."
  [src]
  (compile-ir (comp/CODE! src)))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; JVM bytecode, part 2: the class, through clojure.asm
;;
;; Stage 23D — JVM Bytecode Generation
;;
;; Compiles SNOBOL4 IR all the way to JVM .class bytecode.
;; Each SNOBOL4 program becomes one Java class with a static run() method.
;; The JVM JIT compiles the resulting bytecode to native x64 machine code.
;;
;; ## Call convention
;;
;; Generated classes call EVAL! via INVOKEINTERFACE on clojure.lang.IFn,
;; using a static IFn field pre-populated in jvm_codegen itself.  This avoids
;; any reference to SNOBOL4clojure.operators in the generated class's constant
;; pool (which would fail to resolve at class-load time before the Clojure
;; DynamicClassLoader has loaded operators.clj).
;;
;; Generated class structure:
;;   static Object EVAL_FN;   // = jvm_codegen.EVAL_FN (set before run! is called)
;;   static Object body_N;    // one field per instruction that has a body
;;   static { body_N = RT.readString(pr-str(body)); ... }
;;   public static void run() { ... GETFIELD EVAL_FN, INVOKEINTERFACE IFn.invoke ... }
;;
;; The generated class only references:
;;   - java/lang/Object         (always available)
;;   - clojure/lang/IFn         (always available — part of Clojure runtime jar)
;;   - clojure/lang/RT          (always available)
;;   - clojure/lang/ExceptionInfo (always available)
;;   - SNOBOL4clojure/jvm_codegen (the class that compiled it — always loaded)
;;
;; ## Public API
;;
;;   (compile-to-class src)       → byte[] (JVM .class bytecode)
;;   (load-program src)           → zero-arg fn (wraps static run() via reflection)
;;   (run-jvm! src)               → nil (output via *out*)
;;   (run-jvm-captured! src)      → String (captured stdout)
;;   (bench-compare-jvm src n)    → {:interpreter-ms :jvm-ms :ratio :outputs-match}
;;   (clear-class-cache!)         → nil
;;
;; ## Design invariant
;;
;; Identical output to interpreter (runtime.clj RUN) on every program.
;; Validated against 1000+ worm corpus programs.
;;
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── ASM opcode integer constants ──────────────────────────────────────────────

(def ^:private ^:const ACC-PUBLIC   (int Opcodes/ACC_PUBLIC))
(def ^:private ^:const ACC-STATIC   (int Opcodes/ACC_STATIC))
(def ^:private ^:const ACC-SUPER    (int Opcodes/ACC_SUPER))
(def ^:private ^:const V1-8         (int 52))
(def ^:private ^:const RETURN       (int Opcodes/RETURN))
(def ^:private ^:const ARETURN      (int Opcodes/ARETURN))
(def ^:private ^:const ALOAD-0      (int Opcodes/ALOAD))
(def ^:private ^:const ALOAD        (int Opcodes/ALOAD))
(def ^:private ^:const ASTORE       (int Opcodes/ASTORE))
(def ^:private ^:const GETSTATIC    (int Opcodes/GETSTATIC))
(def ^:private ^:const PUTSTATIC    (int Opcodes/PUTSTATIC))
(def ^:private ^:const INVOKESTATIC (int Opcodes/INVOKESTATIC))
(def ^:private ^:const INVOKEINTERFACE (int Opcodes/INVOKEINTERFACE))
(def ^:private ^:const INVOKEVIRTUAL (int Opcodes/INVOKEVIRTUAL))
(def ^:private ^:const INVOKESPECIAL (int Opcodes/INVOKESPECIAL))
(def ^:private ^:const GOTO         (int Opcodes/GOTO))
(def ^:private ^:const IFNULL       (int Opcodes/IFNULL))
(def ^:private ^:const IFNONNULL    (int Opcodes/IFNONNULL))
(def ^:private ^:const ATHROW       (int Opcodes/ATHROW))
(def ^:private ^:const POP          (int Opcodes/POP))
(def ^:private ^:const DUP          (int Opcodes/DUP))
(def ^:private ^:const ACONST-NULL  (int Opcodes/ACONST_NULL))

;; ── Descriptor constants ──────────────────────────────────────────────────────

(def ^:private OBJ       "Ljava/lang/Object;")
(def ^:private STR       "Ljava/lang/String;")
(def ^:private IFN-DESC  "Lclojure/lang/IFn;")
(def ^:private IFN-CLASS "clojure/lang/IFn")
(def ^:private RT-CLASS  "clojure/lang/RT")
(def ^:private JC-CLASS  "SNOBOL4clojure/jvm_codegen")  ; THIS class
(def ^:private IFN-INVOKE-SIG   (str "(" OBJ ")" OBJ))           ; IFn.invoke(Object)->Object
(def ^:private IFN-INVOKE-2-SIG (str "(" OBJ OBJ ")" OBJ))       ; IFn.invoke(Object,Object)->Object
(def ^:private THROW-SIG  (str "(" STR ")Lclojure/lang/ExceptionInfo;"))

;; ── Static EVAL! IFn reference (set once, used by all generated classes) ─────
;;
;; The generated bytecode does:
;;   GETSTATIC SNOBOL4clojure/jvm_codegen EVAL_FN Lclojure/lang/IFn;
;;   <push body>
;;   INVOKEINTERFACE clojure/lang/IFn invoke (Ljava/lang/Object;)Ljava/lang/Object;
;;
;; This means jvm_codegen is always referenced — which is safe because it's the
;; class that called compile-to-class and is guaranteed already loaded.

(declare EVAL_FN)

;; ── Stage 23E: Inline helper IFn references ───────────────────────────────────
;; These are stored here and referenced by generated bytecode via GETSTATIC,
;; eliminating EVAL! dispatch for the most common IR patterns:
;;   (= VAR val), (= OUTPUT val), (= VAR (op A B)), (LT/GT/LE/GE/EQ A B)
;; Each is a clojure.lang.IFn (or Fn2/Fn1 shape) set at load time.

(declare $$_FN SET_FN PRINT_FN ADD_FN SUB_FN MUL_FN DIV_FN
         LT_FN GT_FN LE_FN GE_FN EQ_FN NEQ_FN SIGNAL_FN
         ;; 23E extension: string concat, SIZE, STR coerce
         CONCAT_FN SIZE_FN STR_FN)

(defn- init-inline-fns!
  "Bind all inline IFn references. Called once when jvm_codegen loads."
  []
  (alter-var-root #'$$_FN   (constantly env/$$))
  (alter-var-root #'SET_FN  (constantly env/protected-store!))
  (alter-var-root #'PRINT_FN (constantly (fn [v] (println (if (nil? v) "" (str v))) v)))
  (alter-var-root #'ADD_FN  (constantly env/Σ+))
  (alter-var-root #'SUB_FN  (constantly env/subtract))
  (alter-var-root #'MUL_FN  (constantly env/multiply))
  (alter-var-root #'DIV_FN  (constantly env/divide))
  ;; Comparison predicates: return v1 (truthy) on success, nil on failure.
  ;; Mirrors CSNOBOL4/SPITBOL semantics: success = first arg returned, fail = nil.
  (alter-var-root #'LT_FN  (constantly (fn [a b]
    (let [na (env/num a) nb (env/num b)] (when (< na nb) a)))))
  (alter-var-root #'GT_FN  (constantly (fn [a b]
    (let [na (env/num a) nb (env/num b)] (when (> na nb) a)))))
  (alter-var-root #'LE_FN  (constantly (fn [a b]
    (let [na (env/num a) nb (env/num b)] (when (<= na nb) a)))))
  (alter-var-root #'GE_FN  (constantly (fn [a b]
    (let [na (env/num a) nb (env/num b)] (when (>= na nb) a)))))
  (alter-var-root #'EQ_FN  (constantly (fn [a b]
    (let [na (env/num a) nb (env/num b)] (when (clojure.core/= na nb) a)))))
  (alter-var-root #'NEQ_FN (constantly (fn [a b]
    (let [na (env/num a) nb (env/num b)] (when (not (clojure.core/= na nb)) a)))))
  ;; SIGNAL_FN: creates and throws a SNOBOL4 ExceptionInfo signal.
  ;; Stored here so generated classes can call it via IFn.invoke(sig-name)
  ;; without needing to resolve SNOBOL4clojure.jvm_codegen by name in their
  ;; constant pool (which would fail under DynamicClassLoader isolation).
  (alter-var-root #'SIGNAL_FN (constantly
    (fn [^String sig-name]
      (throw (ex-info sig-name {:snobol/signal (keyword sig-name)})))))
  ;; CONCAT_FN: variadic SNOBOL4 string concatenation.
  ;; Mirrors SEQ semantics: if ANY element evaluates to nil, the whole concat
  ;; fails (returns nil). Otherwise coerces all to string and joins.
  ;; Called from generated bytecode as CONCAT_FN.invoke(v1, v2, ...) via IFn.
  ;; For 2-arg concat (most common case), IFn.invoke(Object,Object) is used.
  ;; For N-arg, we use IFn.applyTo via a varargs wrapper — but since most
  ;; real programs do 2-arg (S 'x'), we expose both arities.
  (alter-var-root #'CONCAT_FN (constantly
    (fn
      ;; 2-arg (most common: S = S 'x')
      ([a b]
       (when (and (some? a) (some? b))
         (str (if (nil? a) "" (str a))
              (if (nil? b) "" (str b)))))
      ;; N-arg for longer concat chains
      ([a b & more]
       (let [all (list* a b more)]
         (when (every? some? all)
           (apply str (map str all))))))))
  ;; SIZE_FN: SNOBOL4 SIZE — string length of coerced-to-string value.
  (alter-var-root #'SIZE_FN (constantly
    (fn [s] (count (str s)))))
  ;; STR_FN: coerce to string (used in concat sub-expressions).
  (alter-var-root #'STR_FN (constantly
    (fn [v] (if (nil? v) nil (str v))))))

;; ── 23E: Inline bytecode emitter ─────────────────────────────────────────────
;; Returns true if the body was inlined, false if EVAL_FN fallback needed.
;;
;; Inlinable IR patterns:
;;   (= SYM literal)          — assign integer/string literal to variable
;;   (= SYM SYM2)             — copy variable
;;   (= SYM (op A B))         — binary arithmetic (+,-,*,/) where A,B are sym/lit
;;   (= OUTPUT SYM-or-lit)    — print to stdout
;;   (LT|GT|LE|GE|EQ A B)    — numeric comparison, A/B sym or lit
;;
;; The inline path pushes IFn references stored in the jvm_codegen class as
;; static fields, then calls INVOKEINTERFACE IFn.invoke(Object,Object).
;; This is significantly faster than parsing the body EDN via RT.readString
;; and tree-walking through EVAL!.

(def ^:private JC-IFN-FIELD-SIGS
  "Map from inline-fn var-name string → static field name on jvm_codegen."
  {"$$_FN"    "$$_FN"
   "SET_FN"   "SET_FN"
   "PRINT_FN" "PRINT_FN"
   "ADD_FN"   "ADD_FN"
   "SUB_FN"   "SUB_FN"
   "MUL_FN"   "MUL_FN"
   "DIV_FN"   "DIV_FN"
   "LT_FN"    "LT_FN"
   "GT_FN"    "GT_FN"
   "LE_FN"    "LE_FN"
   "GE_FN"    "GE_FN"
   "EQ_FN"    "EQ_FN"
   "NEQ_FN"   "NEQ_FN"
   "SIGNAL_FN" "SIGNAL_FN"
   ;; 23E extensions
   "CONCAT_FN" "CONCAT_FN"
   "SIZE_FN"   "SIZE_FN"
   "STR_FN"    "STR_FN"})

(def ^:private ARITH-OP->FN
  "Map from IR arithmetic operator symbol → inline field name."
  {'+ "ADD_FN" '- "SUB_FN" '* "MUL_FN" '/ "DIV_FN"})

(def ^:private CMP-OP->FN
  "Map from IR comparison operator symbol → inline field name."
  {'LT "LT_FN" 'GT "GT_FN" 'LE "LE_FN" 'GE "GE_FN"
   'EQ "EQ_FN" 'NE "NEQ_FN"})

(defn- sym-or-lit?
  "True if x is a symbol (variable) or a number/string literal."
  [x]
  (or (symbol? x) (integer? x) (float? x) (string? x)))

(defn- emit-val!
  "Emit bytecode to push a SNOBOL4 value (sym or literal) onto the stack.
   Symbols → call $$_FN.invoke(sym-string) to read from <VARS>.
   Integer/real literals → LDC the boxed value.
   Strings → LDC the string."
  [^MethodVisitor mv cls-name x]
  (cond
    (symbol? x)
    (do (.visitFieldInsn mv GETSTATIC cls-name "$$_FN" IFN-DESC)
        (.visitLdcInsn mv (str x))
        (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                          IFN-INVOKE-SIG true))
    (integer? x)
    (do (.visitLdcInsn mv (long x))
        (.visitMethodInsn mv INVOKESTATIC "java/lang/Long" "valueOf"
                          "(J)Ljava/lang/Long;" false))
    (float? x)
    (do (.visitLdcInsn mv (double x))
        (.visitMethodInsn mv INVOKESTATIC "java/lang/Double" "valueOf"
                          "(D)Ljava/lang/Double;" false))
    (string? x)
    (.visitLdcInsn mv ^String x)
    :else
    (throw (ex-info "emit-val!: not a sym-or-lit" {:x x}))))

(defn- inline-emit!
  "Try to emit optimised JVM bytecode for IR body.
   Returns true on success (body inlined), false if EVAL_FN fallback needed.
   Leaves exactly one value on the stack (the result, possibly null)."
  [^MethodVisitor mv cls-name body]
  (cond
    ;; ── (= OUTPUT val) — print, return val ──────────────────────────────────
    (and (list? body)
         (clojure.core/= (first body) '=)
         (clojure.core/= (second body) 'OUTPUT)
         (sym-or-lit? (nth body 2)))
    (let [val (nth body 2)]
      ;; PRINT_FN.invoke(val) → prints and returns val
      (.visitFieldInsn mv GETSTATIC cls-name "PRINT_FN" IFN-DESC)
      (emit-val! mv cls-name val)
      (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                        IFN-INVOKE-SIG true)
      true)

    ;; ── (= SYM val) — assign literal or variable ────────────────────────────
    ;; Stack: compute val, store to local 0, call SET_FN(sym,val), push local 0 as result.
    (and (list? body)
         (clojure.core/= (first body) '=)
         (symbol? (second body))
         (not (clojure.core/= (second body) 'OUTPUT))
         (sym-or-lit? (nth body 2)))
    (let [sym (second body) val (nth body 2)]
      ;; 1. compute val, store in local slot 0
      (emit-val! mv cls-name val)
      (.visitVarInsn mv ASTORE (int 0))
      ;; 2. call SET_FN.invoke(sym, val)
      (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
      (.visitLdcInsn mv (str sym))
      (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern"
                        "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
      (.visitVarInsn mv ALOAD (int 0))
      (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                        IFN-INVOKE-2-SIG true)
      true)

    ;; ── (= SYM (op A B)) — binary arithmetic ────────────────────────────────
    ;; Compute op(A,B), store to local 0, call SET_FN(sym, local0), return local0.
    (and (list? body)
         (clojure.core/= (first body) '=)
         (symbol? (second body))
         (list? (nth body 2))
         (ARITH-OP->FN (first (nth body 2)))
         (sym-or-lit? (second (nth body 2)))
         (sym-or-lit? (nth (nth body 2) 2)))
    (let [sym    (second body)
          [op a b] (nth body 2)
          fn-fld (ARITH-OP->FN op)]
      ;; 1. compute op(a, b), store to local slot 0
      (.visitFieldInsn mv GETSTATIC cls-name fn-fld IFN-DESC)
      (emit-val! mv cls-name a)
      (emit-val! mv cls-name b)
      (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                        IFN-INVOKE-2-SIG true)
      (.visitVarInsn mv ASTORE (int 0))
      ;; 2. call SET_FN.invoke(sym, computed-val)
      (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
      (.visitLdcInsn mv (str sym))
      (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern"
                        "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
      (.visitVarInsn mv ALOAD (int 0))
      (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                        IFN-INVOKE-2-SIG true)
      true)

    ;; ── (LT|GT|LE|GE|EQ A B) — comparison predicate ─────────────────────────
    (and (list? body)
         (CMP-OP->FN (first body))
         (sym-or-lit? (second body))
         (sym-or-lit? (nth body 2)))
    (let [[op a b] body
          fn-fld   (CMP-OP->FN op)]
      (.visitFieldInsn mv GETSTATIC cls-name fn-fld IFN-DESC)
      (emit-val! mv cls-name a)
      (emit-val! mv cls-name b)
      (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                        IFN-INVOKE-2-SIG true)
      true)

    ;; ── (= SYM [e1 e2 ...]) — vector concat assign ─────────────────────────
    ;; All elements must be sym-or-lit. Calls CONCAT_FN.invoke(e1,e2) for 2-arg,
    ;; or falls back to EVAL_FN for 3+ args (uncommon).
    ;; Result stored to variable. nil in any element → nil result → stmt fails.
    (and (list? body)
         (clojure.core/= (first body) '=)
         (symbol? (second body))
         (not (clojure.core/= (second body) 'OUTPUT))
         (vector? (nth body 2))
         (every? sym-or-lit? (nth body 2)))
    (let [sym   (second body)
          elems (nth body 2)
          n     (count elems)]
      (if (clojure.core/= n 2)
        ;; Fast path: 2-element concat (most common: S = S 'x')
        (let [[e1 e2] elems]
          ;; 1. compute CONCAT_FN(e1, e2), store to local 0
          (.visitFieldInsn mv GETSTATIC cls-name "CONCAT_FN" IFN-DESC)
          (emit-val! mv cls-name e1)
          (emit-val! mv cls-name e2)
          (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                            IFN-INVOKE-2-SIG true)
          (.visitVarInsn mv ASTORE (int 0))
          ;; 2. call SET_FN(sym, result) — only if non-nil
          (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
          (.visitLdcInsn mv (str sym))
          (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern"
                            "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
          (.visitVarInsn mv ALOAD (int 0))
          (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                            IFN-INVOKE-2-SIG true)
          true)
        ;; 3+ element concat: fall through to EVAL_FN
        false))

    ;; ── (= OUTPUT [e1 e2 ...]) — vector concat to OUTPUT ────────────────────
    ;; 2-arg fast path: concat then print.
    (and (list? body)
         (clojure.core/= (first body) '=)
         (clojure.core/= (second body) 'OUTPUT)
         (vector? (nth body 2))
         (every? sym-or-lit? (nth body 2)))
    (let [elems (nth body 2)
          n     (count elems)]
      (if (clojure.core/= n 2)
        (let [[e1 e2] elems]
          ;; 1. concat
          (.visitFieldInsn mv GETSTATIC cls-name "CONCAT_FN" IFN-DESC)
          (emit-val! mv cls-name e1)
          (emit-val! mv cls-name e2)
          (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                            IFN-INVOKE-2-SIG true)
          (.visitVarInsn mv ASTORE (int 0))
          ;; 2. print via PRINT_FN
          (.visitFieldInsn mv GETSTATIC cls-name "PRINT_FN" IFN-DESC)
          (.visitVarInsn mv ALOAD (int 0))
          (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                            IFN-INVOKE-SIG true)
          true)
        false))

    ;; ── (= SYM (SIZE arg)) — assign SIZE result ──────────────────────────────
    (and (list? body)
         (clojure.core/= (first body) '=)
         (symbol? (second body))
         (not (clojure.core/= (second body) 'OUTPUT))
         (list? (nth body 2))
         (clojure.core/= (first (nth body 2)) 'SIZE)
         (sym-or-lit? (second (nth body 2))))
    (let [sym (second body)
          arg (second (nth body 2))]
      ;; 1. SIZE_FN.invoke(arg), store to local 0
      (.visitFieldInsn mv GETSTATIC cls-name "SIZE_FN" IFN-DESC)
      (emit-val! mv cls-name arg)
      (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                        IFN-INVOKE-SIG true)
      (.visitVarInsn mv ASTORE (int 0))
      ;; 2. SET_FN(sym, result)
      (.visitFieldInsn mv GETSTATIC cls-name "SET_FN" IFN-DESC)
      (.visitLdcInsn mv (str sym))
      (.visitMethodInsn mv INVOKESTATIC "clojure/lang/Symbol" "intern"
                        "(Ljava/lang/String;)Lclojure/lang/Symbol;" false)
      (.visitVarInsn mv ALOAD (int 0))
      (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                        IFN-INVOKE-2-SIG true)
      true)

    ;; ── (= OUTPUT (SIZE arg)) — print SIZE result ────────────────────────────
    (and (list? body)
         (clojure.core/= (first body) '=)
         (clojure.core/= (second body) 'OUTPUT)
         (list? (nth body 2))
         (clojure.core/= (first (nth body 2)) 'SIZE)
         (sym-or-lit? (second (nth body 2))))
    (let [arg (second (nth body 2))]
      (.visitFieldInsn mv GETSTATIC cls-name "PRINT_FN" IFN-DESC)
      (.visitFieldInsn mv GETSTATIC cls-name "SIZE_FN" IFN-DESC)
      (emit-val! mv cls-name arg)
      (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                        IFN-INVOKE-SIG true)
      (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                        IFN-INVOKE-SIG true)
      true)

    :else false))

;; ── Signal helpers — called from generated bytecode via INVOKESTATIC ─────────

(defn ^clojure.lang.ExceptionInfo make-signal
  "Create a SNOBOL4 ExceptionInfo. Called from JVM-compiled programs."
  [^String sig-name]
  (ex-info sig-name {:snobol/signal (keyword sig-name)}))

;; ── Class name generation ─────────────────────────────────────────────────────

(defn gen-class-name
  "Generate a stable JVM internal class name for a SNOBOL4 source string.
   The class is in the SNOBOL4clojure package to match jvm_codegen's package."
  [src]
  (str "SNOBOL4clojure/SnobolProg" (format "%08x" (Math/abs (hash src)))))

;; ── Bytecode emitter ─────────────────────────────────────────────────────────

(defn compile-to-class
  "Compile SNOBOL4 source to JVM bytecode.
   Returns byte[] — a valid .class file.

   Generated class layout:
     public static IFn EVAL_FN;     // set externally before run() is called
     public static Object body_N;   // one field per instruction body
     static { body_N = RT.readString(...); ... }
     public static void run() { ... }
  "
  [src]
  (let [bc-vec    (compile-src src)
        n-instrs  (count bc-vec)
        cls-name  (gen-class-name src)
        cw        (ClassWriter. (int ClassWriter/COMPUTE_FRAMES))

        ;; Collect (index, body) pairs for instructions that have a body
        bodies    (into [] (keep-indexed (fn [i {:keys [body]}]
                                           (when body [i body]))
                                         bc-vec))
        body-field (fn [i] (str "body_" i))]

    ;; ── Class header ──────────────────────────────────────────────────────────
    (.visit cw V1-8 (int (+ ACC-PUBLIC ACC-SUPER))
            cls-name nil "java/lang/Object" nil)

    ;; ── Static field: EVAL_FN (IFn, set from Clojure before run() is called) ─
    (.visitField cw (int (+ ACC-PUBLIC ACC-STATIC))
                 "EVAL_FN" IFN-DESC nil nil)

    ;; ── Static fields: 23E inline IFn helpers (set by load-program) ──────────
    (doseq [fld-name (keys JC-IFN-FIELD-SIGS)]
      (.visitField cw (int (+ ACC-PUBLIC ACC-STATIC))
                   fld-name IFN-DESC nil nil))

    ;; ── Static body fields (Object, one per instruction that has a body) ─────
    (doseq [[i _] bodies]
      (.visitField cw (int (+ ACC-PUBLIC ACC-STATIC))
                   (body-field i) OBJ nil nil))

    ;; ── <clinit>: initialise body fields via RT.readString ────────────────────
    (let [ci (.visitMethod cw (int (+ ACC-PUBLIC ACC-STATIC))
                           "<clinit>" "()V" nil nil)]
      (.visitCode ci)
      (doseq [[i body] bodies]
        ;; RT.readString(pr-str(body)) → Object → PUTSTATIC body_N
        (.visitLdcInsn ci (pr-str body))
        (.visitMethodInsn ci INVOKESTATIC RT-CLASS "readString"
                          (str "(" STR ")" OBJ) false)
        (.visitFieldInsn ci PUTSTATIC cls-name (body-field i) OBJ))
      (.visitInsn ci RETURN)
      (.visitMaxs ci (int 2) (int 0))
      (.visitEnd ci))

    ;; ── run() method ──────────────────────────────────────────────────────────
    (let [mv     (.visitMethod cw (int (+ ACC-PUBLIC ACC-STATIC))
                               "run" "()V" nil nil)
          lbls   (mapv (fn [_] (Label.)) bc-vec)
          end-lbl (Label.)]

      (.visitCode mv)

      (doseq [i (range n-instrs)]
        (.visitLabel mv (lbls i))
        (let [{:keys [op body s f s-sig f-sig]} (bc-vec i)
              op   (int op)

              ;; Helper: emit body evaluation, leave result on stack.
              ;; 23E: try inline-emit! first (no EVAL_FN call for common patterns).
              ;; Falls back to EVAL_FN for complex bodies.
              eval-body!
              (fn []
                (if body
                  (or (inline-emit! mv cls-name body)
                      ;; Fallback: call EVAL_FN.invoke(body-field)
                      (do (.visitFieldInsn mv GETSTATIC cls-name "EVAL_FN" IFN-DESC)
                          (.visitFieldInsn mv GETSTATIC cls-name (body-field i) OBJ)
                          (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                                            IFN-INVOKE-SIG true)))
                  (.visitInsn mv ACONST-NULL)))

              ;; Helper: emit a signal throw via SIGNAL_FN IFn field.
              ;; SIGNAL_FN.invoke(sig-name) always throws ExceptionInfo.
              ;; Using INVOKEINTERFACE avoids INVOKESTATIC to jvm_codegen which
              ;; fails under DynamicClassLoader class isolation.
              emit-signal!
              (fn [sig]
                (.visitFieldInsn mv GETSTATIC cls-name "SIGNAL_FN" IFN-DESC)
                (.visitLdcInsn mv (name sig))
                (.visitMethodInsn mv INVOKEINTERFACE IFN-CLASS "invoke"
                                  IFN-INVOKE-SIG true)
                ;; SIGNAL_FN always throws. Pop the nominally-returned value
                ;; (unreachable) so the JVM verifier sees an empty stack.
                (.visitInsn mv POP)
                (.visitInsn mv ACONST-NULL)
                (.visitInsn mv ATHROW))]

          (case op
            ;; 0 HALT
            0 (.visitInsn mv RETURN)

            ;; 1 EXEC — eval body, POP, GOTO fall
            1 (do (when body (eval-body!) (.visitInsn mv POP))
                  (when s (.visitJumpInsn mv GOTO (lbls s))))

            ;; 2 EXEC-S — eval; non-null → success, null → fall
            2 (do (eval-body!)
                  (let [fail-lbl (lbls f)]
                    (.visitJumpInsn mv IFNULL fail-lbl)
                    ;; success branch
                    (if s-sig
                      (emit-signal! s-sig)
                      (.visitJumpInsn mv GOTO (lbls s)))
                    ;; fall through to fail label (already placed)
                    ))

            ;; 3 EXEC-F — eval; null → failure, non-null → fall
            3 (do (eval-body!)
                  (let [ok-lbl (if s (lbls s) end-lbl)]
                    (.visitJumpInsn mv IFNONNULL ok-lbl)
                    ;; failure branch
                    (if f-sig
                      (emit-signal! f-sig)
                      (.visitJumpInsn mv GOTO (lbls f)))
                    (.visitLabel mv ok-lbl)))

            ;; 4 EXEC-SF — eval; non-null → s, null → f
            4 (do (eval-body!)
                  (let [fail-lbl (Label.)]
                    (.visitJumpInsn mv IFNULL fail-lbl)
                    ;; success
                    (if s-sig
                      (emit-signal! s-sig)
                      (.visitJumpInsn mv GOTO (lbls s)))
                    ;; failure
                    (.visitLabel mv fail-lbl)
                    (if f-sig
                      (emit-signal! f-sig)
                      (.visitJumpInsn mv GOTO (lbls f)))))

            ;; 5 JUMP — eval body (if any), GOTO s
            5 (do (when body (eval-body!) (.visitInsn mv POP))
                  (.visitJumpInsn mv GOTO (lbls (int s))))

            ;; 6 SIGNAL
            6 (emit-signal! body)

            ;; default
            (.visitInsn mv RETURN))))

      (.visitLabel mv end-lbl)
      (.visitInsn mv RETURN)
      (.visitMaxs mv (int 4) (int 2))
      (.visitEnd mv))

    (.visitEnd cw)
    (.toByteArray cw)))

;; ── Class loader cache ────────────────────────────────────────────────────────

(def ^:private class-cache (atom {}))

(defn load-program
  "Compile src to JVM bytecode and load the class.
   Returns a zero-arg fn that calls the generated static run() method.
   Results are cached by source string hash.

   Before invoking run(), this function sets EVAL_FN and all 23E inline
   IFn helper fields on the generated class."
  [src]
  (or (get @class-cache src)
      (let [bc       (compile-to-class src)
            cls-name (.replace (gen-class-name src) "/" ".")
            dcl      (DynamicClassLoader.)
            cls      (.defineClass dcl cls-name bc nil)
            ;; Set EVAL_FN field
            eval-field (.getField cls "EVAL_FN")
            _          (.set eval-field nil SNOBOL4clojure.operators/EVAL!)
            ;; Set all 23E inline IFn helper fields
            inline-fn-vals {"$$_FN"    $$_FN
                            "SET_FN"   SET_FN
                            "PRINT_FN" PRINT_FN
                            "ADD_FN"   ADD_FN
                            "SUB_FN"   SUB_FN
                            "MUL_FN"   MUL_FN
                            "DIV_FN"   DIV_FN
                            "LT_FN"    LT_FN
                            "GT_FN"    GT_FN
                            "LE_FN"    LE_FN
                            "GE_FN"    GE_FN
                            "EQ_FN"    EQ_FN
                            "NEQ_FN"   NEQ_FN
                            "SIGNAL_FN" SIGNAL_FN
                            ;; 23E extensions
                            "CONCAT_FN" CONCAT_FN
                            "SIZE_FN"   SIZE_FN
                            "STR_FN"    STR_FN}
            _  (doseq [[fld-name fld-val] inline-fn-vals]
                 (.set (.getField cls fld-name) nil fld-val))
            method   (.getMethod cls "run" (into-array Class []))
            run-fn   (fn []
                       ;; Refresh EVAL_FN in case of namespace reload
                       (.set eval-field nil SNOBOL4clojure.operators/EVAL!)
                       (.invoke method nil (into-array Object [])))]
        (swap! class-cache assoc src run-fn)
        run-fn)))

(defn clear-class-cache!
  "Evict all entries from the JVM class cache."
  []
  (reset! class-cache {})
  nil)

;; ── Execute ───────────────────────────────────────────────────────────────────

(defn run-jvm!
  "Run src through the JVM-compiled path. Output goes to *out*."
  [src]
  (env/GLOBALS (find-ns 'SNOBOL4clojure.runtime))
  (reset! env/STNO 0)
  (reset! env/<STNO> {})
  (reset! env/<LABL> {})
  (reset! env/<CODE> {})
  (reset! env/<FUNS> {})
  (reset! &STCOUNT 0)
  (let [run-fn (load-program src)]
    (try
      (run-fn)
      (catch java.lang.reflect.InvocationTargetException e
        (let [cause (.getCause e)]
          (if (instance? clojure.lang.ExceptionInfo cause)
            (when-not (contains? #{:end :return :freturn :nreturn}
                                 (get (ex-data cause) :snobol/signal))
              (throw cause))
            (throw (or cause e))))))))

(defn run-jvm-captured!
  "Run src through JVM-compiled path and return captured stdout as string."
  [src]
  (with-out-str (run-jvm! src)))

;; ── Benchmark ────────────────────────────────────────────────────────────────

(defn bench-compare-jvm
  "Compare interpreter (RUN+CODE) vs JVM-compiled (run-jvm!) on src.
   Warms both paths before timing.
   Returns {:interpreter-ms :jvm-ms :ratio :outputs-match}."
  [src n]
  (let [reset-env! (fn []
                     (env/GLOBALS (find-ns 'SNOBOL4clojure.runtime))
                     (reset! env/STNO 0) (reset! env/<STNO> {})
                     (reset! env/<LABL> {}) (reset! env/<CODE> {})
                     (reset! env/<FUNS> {}))

        run-interp (fn []
                     (reset-env!)
                     (with-out-str
                       (try (RUN (comp/CODE src))
                            (catch clojure.lang.ExceptionInfo e
                              (when-not (contains? #{:end :return :freturn :nreturn}
                                                   (get (ex-data e) :snobol/signal))
                                (throw e))))))

        run-fn     (do (reset-env!) (load-program src))
        run-jvm    (fn []
                     (reset-env!)
                     (with-out-str
                       (try (run-fn)
                            (catch java.lang.reflect.InvocationTargetException e
                              (let [cause (.getCause e)]
                                (when-not (and (instance? clojure.lang.ExceptionInfo cause)
                                              (contains? #{:end :return :freturn :nreturn}
                                                         (get (ex-data cause) :snobol/signal)))
                                  (throw (or cause e))))))))]

    (dotimes [_ 30] (run-interp))
    (dotimes [_ 30] (run-jvm))

    (let [i-out (atom "") j-out (atom "")
          t0 (System/nanoTime)
          _  (dotimes [_ n] (reset! i-out (run-interp)))
          i-ms (/ (/ (- (System/nanoTime) t0) n) 1e6)
          t1 (System/nanoTime)
          _  (dotimes [_ n] (reset! j-out (run-jvm)))
          j-ms (/ (/ (- (System/nanoTime) t1) n) 1e6)]
      {:interpreter-ms  (double i-ms)
       :jvm-ms          (double j-ms)
       :ratio           (double (/ i-ms j-ms))
       :outputs-match   (clojure.core/= @i-out @j-out)})))

;; ── Initialize 23E inline IFn references at namespace load time ─────────────
(init-inline-fns!)
