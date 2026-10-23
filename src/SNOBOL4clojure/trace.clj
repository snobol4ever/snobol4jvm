(ns SNOBOL4clojure.trace
  "SNOBOL4 TRACE / STOPTR / &TRACE as SPITBOL (sbl -bf) does them.

  TRACE(name, type [, label [, fn]])
    Register a trace trigger on `name`. The type is read in any case by its first letter:
    V(ALUE) fires when the named variable is assigned a value, A(CCESS) each time its value is referenced. A name is read in the case it is written.
    L(ABEL), C(ALL), R(ETURN), F(UNCTION = call and return), K(EYWORD) are registered too; a type that begins with
    another letter is ERROR 199.

  STOPTR(name, type)
    Remove the trace registration for (name, type).

  &TRACE  (env/&TRACE atom)
    A count the program sets. A trace line is written only while it is above 0, and each line costs 1. TRACE and
    STOPTR do not touch it.

  ## Output

  A trace line goes to the program's output (stdout):  four asterisks and the number of the statement, filled with
  asterisks to 12 characters, two blanks, then the text. A VALUE trace's text is `name = value`, the value as a dump
  shows it: a string in quotes exactly as it is, a NAME with a point, a real with its point, PATTERN for a pattern.

  Other trace types keep the form they had until they are measured against the oracle.
  "
  (:require [SNOBOL4clojure.env :refer [&TRACE &FTRACE &STNO &FNCLEVEL $$ real-str <TRACES> <FUNS>]])
  (:import  [SNOBOL4clojure.env NAME]))

;; ── Registry ───────────────────────────────────────────�
;; Maps [name kind] -> {:label lbl :fn fn-name}. A name is kept as written; a kind is :value :access :label :call :return :keyword.

(def ^:private registry <TRACES>)

(defn- trace-kinds
  "The kinds a trace type registers, by its first letter in any case: V value, A access, L label, C call, R return, F both of a function's, K keyword.
   Any other letter is ERROR 199, trace second argument is not trace type."
  [type]
  (case (first (clojure.string/upper-case (str type)))
    \V [:value]
    \A [:access]
    \L [:label]
    \C [:call]
    \R [:return]
    \F [:call :return]
    \K [:keyword]
    (throw (ex-info "trace second argument is not trace type" {:snobol/error 199}))))

(defn- reg-key [name kind] [(str name) kind])

;; ── Public registration API ──────────────────────────────────────�

(defn trace!
  "Register a trace. Called by INVOKE's TRACE branch.
   name  — variable/label/function name (a string, a symbol or a NAME)
   type  — trace type: VALUE LABEL CALL RETURN FUNCTION KEYWORD, or by its first letter(s)
   label — optional goto label (string/keyword) when trace fires
   fn    — optional user function name to call when trace fires"
  ([name type]       (trace! name type nil nil))
  ([name type label] (trace! name type label nil))
  ([name type label user-fn]
   (doseq [kind (trace-kinds type)]
     (swap! registry assoc (reg-key name kind) {:label label :fn user-fn}))
   nil))

(defn stoptr!
  "Remove a trace registration. Called by INVOKE's STOPTR branch."
  [name type]
  (doseq [kind (trace-kinds type)]
    (swap! registry dissoc (reg-key name kind)))
  nil)

(defn clear-all-traces!
  "Remove every trace registration. Resets &TRACE to 0."
  []
  (reset! registry {})
  (reset! &TRACE 0)
  nil)

(defn active?
  "True if a trace of the given type is registered for name."
  [name type]
  (boolean (some #(contains? @registry (reg-key name %)) (trace-kinds type))))

(defn any-active?
  "True if any trace of any type is currently registered."
  []
  (pos? (count @registry)))

;; ── Trace output ──────────────────────────────────────────────────────────────

(def ^:dynamic *trace-output*
  "Where trace output goes. Default: the program's output (*out*), as in SPITBOL.
   Rebind to a writer in tests to capture trace output."
  nil)

;; A trace line is cut into pieces of 120 columns, each on a line of its own, as sbl -bf writes a long one (the call of a function given a long string).
(defn- trace-println [& args]
  (let [w (or *trace-output* *out*)
        l (apply str args)]
    (binding [*out* w]
      (if (<= (count l) 120)
        (println l)
        (doseq [p (partition-all 120 l)] (println (apply str p)))))))

(defn- star-field
  "Four asterisks and the number of the statement being executed, filled with asterisks to 12 characters."
  []
  (let [s (str "****" @&STNO)]
    (str s (apply str (repeat (max 0 (- 12 (count s))) \*)))))

(defn- trace-val
  "A value as a dump shows it: a string in quotes exactly as it is, a NAME with a point, a real with its point, PATTERN for a pattern."
  [v]
  (cond
    (nil? v)                  "''"
    (string? v)               (str "'" v "'")
    (instance? NAME v)        (str "." (.n ^NAME v))
    (integer? v)              (str v)
    (double? v)               (real-str v)
    (and (or (list? v) (vector? v)) (= "PATTERN" (SNOBOL4clojure.env/DATATYPE v))) "PATTERN"
    :else                     (str v)))

(defn- emit-line!
  "Write one trace line if &TRACE is above 0, and charge it 1."
  [text]
  (when (pos? @&TRACE)
    (swap! &TRACE dec)
    (trace-println (star-field) " " text)))

;; "Name2 is the optional name of a program-defined function that will be called when a trace occurs. It is called with two arguments: the name of the item being
;; traced, and argument s2" (manual ch. 19, TRACE); "When a program-defined trace function is invoked, keywords &TRACE and &FTRACE are temporarily set to zero.
;; Their values are restored when the trace function returns" (ch. 10, Debugging). As sbl -bf has it: the function is called in place of the trace line and costs 1
;; of &TRACE as a line does; the item's name is a NAME. A function name that names no defined function leaves the line.
(defn- trace-fn [entry]
  (when-some [f (:fn entry)]
    (get @<FUNS> (if (instance? NAME f) (str (.n ^NAME f)) (str f)))))

(defn- call-trace-fn! [f item-name entry]
  (swap! &TRACE dec)
  (let [tr @&TRACE ft @&FTRACE]
    (reset! &TRACE 0)
    (reset! &FTRACE 0)
    (try (f (NAME. (symbol (str item-name))) (:label entry))
         (finally (reset! &TRACE tr) (reset! &FTRACE ft)))))

;; ── Fire helpers — called from runtime / operators / functions ────────────────

(defn fire-value!
  "Fire VALUE trace for `sym` being assigned `new-val`. Called from operators.clj INVOKE '= after the assignment. The line is the statement's star field, an i
   for each function level, then name = value, as sbl -bf writes it (****4******* i G = 3 for an assignment in a function called from level 0)."
  [sym new-val]
  (when-let [entry (@registry (reg-key sym :value))]
    (if-let [f (and (pos? @&TRACE) (trace-fn entry))]
      (call-trace-fn! f sym entry)
      (emit-line! (str (apply str (repeat @&FNCLEVEL \i)) " " sym " = " (trace-val new-val))))
    (:label entry)))

(defn fire-access!
  "Fire the ACCESS trace of the variable `sym`, whose value `v` a statement has just fetched: \"Produces a trace every time the named item is referenced. If you say
   X = N, that does not change the value of N, but it is a reference\" (manual ch. 10, Debugging). As sbl -bf writes it: the referencing statement's star field, an
   i for each function level, then name = value; or the trace function is called. Either costs 1 of &TRACE. Called only while &TRACE is above 0."
  [sym v]
  (when-let [entry (@registry (reg-key sym :access))]
    (if-let [f (trace-fn entry)]
      (call-trace-fn! f sym entry)
      (do (swap! &TRACE dec)
          (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " " sym " = " (trace-val v))))))

(defn fire-keyword!
  "Fire the KEYWORD trace of the keyword named kw (ERRTYPE, FNCLEVEL or STCOUNT, the ones SPITBOL traces) now holding new-val: the trace function is called, or the
   line written as sbl -bf writes it, the statement's star field, an i for each function level, then &NAME = value; either costs 1 of &TRACE."
  [kw new-val]
  (when (pos? @&TRACE)
    (when-let [entry (@registry (reg-key kw :keyword))]
      (if-let [f (trace-fn entry)]
        (call-trace-fn! f kw entry)
        (do (swap! &TRACE dec)
            (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " &" kw " = " (trace-val new-val)))))))

(defn fire-label!
  "Fire the LABEL trace of `label-key`, a Goto transferring to it: \"Produce a trace when a Goto transfer to statement name occurs. Flowing sequentially into the
   labeled statement ... does not produce a trace\" (manual ch. 10). Called from runtime.clj's Goto while &STNO is still the statement whose Goto it is; the line
   is its star field, an i for each function level, then :(L), as sbl -bf writes it, or the trace function is called; either costs 1 of &TRACE."
  [label-key]
  (when (pos? @&TRACE)
    (let [lname (if (keyword? label-key) (name label-key) (str label-key))]
      (when-let [entry (@registry (reg-key lname :label))]
        (if-let [f (trace-fn entry)]
          (call-trace-fn! f lname entry)
          (do (swap! &TRACE dec)
              (trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " :(" lname ")")))))))

(defn- function-line!
  "A call or return line of the function `fname`. It is written when the function is registered for `kind` and &TRACE is above 0 (and costs 1 of &TRACE),
   or else when &FTRACE is above 0 (and costs 1 of &FTRACE). Four asterisks and the statement number, a blank, one i for each function level the caller is
   inside, a blank, then the text."
  [fname kind text]
  (let [line  #(trace-println (star-field) " " (apply str (repeat @&FNCLEVEL \i)) " " text)
        entry (@registry (reg-key fname kind))]
    (cond
      (and entry (pos? @&TRACE)) (if-let [f (trace-fn entry)] (call-trace-fn! f fname entry) (do (swap! &TRACE dec) (line)))
      (pos? @&FTRACE)            (do (swap! &FTRACE dec) (line)))))

(defn fire-call!
  "Fire the call trace of user function `fname`, whose parameters hold `args`. Called from the DEFINE wrapper before the body runs, at the caller's
   function level and with the number of the calling statement."
  [fname args]
  (function-line! fname :call (str fname "(" (clojure.string/join "," (map trace-val args)) ")")))

(defn fire-return!
  "Fire the return trace of user function `fname` returning `val`. signal is :return :freturn or :nreturn. Called from the DEFINE wrapper once the level is
   the caller's again and while the number of the returning statement is still &STNO."
  [fname val signal]
  (function-line! fname :return
                  (case signal
                    :freturn  (str "FRETURN " fname)
                    :nreturn  (str "NRETURN " fname " = " (trace-val val))
                    (str "RETURN " fname " = " (trace-val val)))))

(defn pattern-trace-active?
  "True if PATTERN trace is registered for `subject-var` (or globally via \"*\").
   Used by match.clj to decide whether to bind *trace* true."
  [subject-var]
  (and (pos? @&TRACE)
       (or (@registry (reg-key subject-var :pattern))
           (@registry (reg-key "*" :pattern)))))

;; ── &TRACE as a simple on/off toggle (convenience, non-standard extension) ────
;; Setting &TRACE = 1 with no registrations enables a default statement trace.
;; Setting &TRACE = 0 suppresses all output even if registrations exist.

(defn statement-trace-active?
  "True if &TRACE > 0 AND a global statement trace (name \"*\" type \"LABEL\")
   is registered. This is our extension for full statement-by-statement tracing."
  []
  (and (pos? @&TRACE)
       (@registry (reg-key "*" :label))))

(defn enable-full-trace!
  "Convenience: register a global statement+value trace. Sets &TRACE > 0.
   Equivalent to the test helper calling:
     (trace! \"*\" \"LABEL\") (trace! \"*\" \"VALUE\") (trace! \"*\" \"CALL\")"
  []
  (trace! "*" "LABEL")
  (trace! "*" "VALUE")
  (trace! "*" "CALL")
  (trace! "*" "RETURN"))

(defn disable-full-trace!
  "Convenience: clear the global wildcard traces."
  []
  (stoptr! "*" "LABEL")
  (stoptr! "*" "VALUE")
  (stoptr! "*" "CALL")
  (stoptr! "*" "RETURN"))
