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
  (:require [SNOBOL4clojure.env   :refer [<STNO> <LABL> <CODE> <DISPNO>
                                           &STCOUNT &STLIMIT &TRACE &STNO &LASTNO &ERRLIMIT &ERRTYPE &ERRTEXT <SETEXIT>
                                           snobol-return! snobol-freturn!
                                           snobol-nreturn! snobol-end!
                                           snobol-steplimit!]]
            [SNOBOL4clojure.trace :refer [fire-label! statement-trace-active?]]
            [SNOBOL4clojure.operators :refer [EVAL!]]))

;; Special goto targets that trigger control-flow signals rather than label jumps.
(def ^:private special-targets
  #{"RETURN" "return" "FRETURN" "freturn" "NRETURN" "nreturn" "END" "end"
    :RETURN :return :FRETURN :freturn :NRETURN :nreturn :END :end})

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

(defn- exec-body [body]
  (try
    (or (nil? body) (EVAL! body))
    (catch clojure.lang.ExceptionInfo e
      (let [n (:snobol/error (ex-data e))]
        (if (and n (pos? (long @&ERRLIMIT)))
          (let [h @<SETEXIT>]
            (swap! &ERRLIMIT dec)
            (reset! &ERRTYPE n)
            (reset! &ERRTEXT (ex-message e))
            (if h
              (do (reset! <SETEXIT> nil) (->Trapped h e))
              false))
          (throw e))))))

(defn- label-name [tgt] (when (or (keyword? tgt) (string? tgt) (symbol? tgt)) (name tgt)))

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
                        (keyword? at)
                          (or (when-let [n (@<STNO> at)] [n at])
                              ;; Not a known label — treat as variable name
                              (let [varname  (name at)
                                    resolved (EVAL! (symbol varname))]
                                (cond
                                  (string?  resolved) (or (when-let [n (@<STNO> (keyword resolved))] [n (keyword resolved)])
                                                          (when (re-matches #"[0-9]+" resolved)
                                                            (let [n (Long/parseLong resolved)] [n (@<LABL> n)])))
                                  (keyword? resolved) [(@<STNO> resolved) resolved]
                                  (integer? resolved) [resolved (@<LABL> resolved)]
                                  :else               nil)))
                        (string?  at) (or (when-let [n (@<STNO> (keyword at))] [n (keyword at)])
                                          ;; Numeric string (e.g. CODE() returned stmt# as integer, stored as str)
                                          (when (re-matches #"[0-9]+" at)
                                            (let [n (Long/parseLong at)]
                                              [n (@<LABL> n)]))
                                          [(@<STNO> at) at])
                        (integer? at) [at (@<LABL> at)]))
     ;; A computed goto, :($X) or :S($('WANG.A.' OP)), names its label by the value of the expression: a string (a label, RETURN, END) or an integer.
     ;; It is resolved here, once, so that the jump and the special labels see the label itself. A target that is not a $ form is passed on as it was.
     (computed-target [tgt]
       (if (and (list? tgt) (clojure.core/= '$ (first tgt)) (clojure.core/= 2 (count tgt)))
         (when-some [v (EVAL! (second tgt))]
           (if (integer? v) v (keyword (str v))))
         tgt))
     (goto! [tgt]
       (if (special-targets tgt)
         (dispatch-special! tgt)
         tgt))]
    (reset! &STCOUNT 0)
    (try
      (loop [current (saddr at) err nil]
        (when-let [key (skey current)]
          ;; Special-label check (e.g. labelled RETURN/END)
          (when (special-targets key)
            (dispatch-special! key))
          (when-let [stmt (@<CODE> key)]
            ;; ── &STCOUNT / &STLIMIT ──────────────────────────────────────────
            (reset! &LASTNO @&STNO)
            (reset! &STNO (get @<DISPNO> (first current) (first current)))
            (let [n (swap! &STCOUNT inc)]
              (when (> n @&STLIMIT)
                (snobol-steplimit! n))
              ;; ── LABEL trace ────────────────────────────────────────────────
              (when (pos? @&TRACE)
                (fire-label! key)
                (when (statement-trace-active?)
                  (binding [*out* *err*]
                    (println (format ">>> stmt %5d  %s" (dec n) (str key)))))))
            ;; ── Execute statement ────────────────────────────────────────────
            (let [ferst  (first stmt)
                  seqond (second stmt)
                  body   (if (map? ferst) seqond ferst)
                  goto   (if (map? ferst) ferst  seqond)]
              (let [r (exec-body body)]
                (if (instance? Trapped r)
                  ;; control goes to the handler; CONTINUE and ABORT there refer back to the statement in error
                  (recur (saddr (:label r)) {:at current :goto goto :ex (:ex r)})
                  (let [g   (when (map? goto) goto)
                        tgt (computed-target (if r (or (:G g) (:S g)) (or (:G g) (:F g))))
                        nm  (label-name tgt)]
                    (cond
                      ;; CONTINUE resumes at the failure exit of the statement in error; ABORT lets the error end the program
                      (and err (clojure.core/= nm "CONTINUE"))
                      (let [g2   (:goto err)
                            etgt (computed-target (when (map? g2) (or (:G g2) (:F g2))))]
                        (if etgt
                          (do (goto! etgt) (recur (saddr etgt) nil))
                          (recur (saddr (inc ((:at err) 0))) nil)))
                      (and err (clojure.core/= nm "ABORT"))
                      (throw (:ex err))
                      tgt
                      (do (goto! tgt) (recur (saddr tgt) err))
                      :else
                      (recur (saddr (inc (current 0))) err)))))))))
      (catch clojure.lang.ExceptionInfo e
        (case (get (ex-data e) :snobol/signal)
          :end        nil          ; normal program end
          :return     nil          ; bubble up to DEFINE wrapper
          :step-limit (throw e)    ; propagate — caught by test-helpers/harness
          :freturn    (throw e)    ; bubble up to DEFINE wrapper
          :nreturn    (throw e)    ; bubble up to DEFINE wrapper
          (throw e))))))
