(ns SNOBOL4clojure.operators
  "The evaluator and the matcher. The one namespace that shadows clojure.core's = + - * /: inside it, comparison is
  written clojure.core/=."
  (:refer-clojure :exclude [= + - * / num])
  (:require [SNOBOL4clojure.env :as env :refer [ε equal out $$ snobol-set! sstr &ANCHOR LIT$ ANY$ NOTANY$ SPAN$ NSPAN$ BREAK$ BREAKX$ POS# RPOS# LEN# TAB# RTAB# BOL# EOL# SUCCEED! FAIL! ARB!
                                                BAL! ARBNO! ABORT! POS RPOS pattern-trace-active? fire-keyword! η not-equal Σ+ subtract multiply divide ncvt num $$fn reference table?
                                                table-get table-set array? array-get array-set array-prototype assign-keyword! name-string gtstg snobol-return! snobol-freturn!
                                                snobol-nreturn! <FUNS> <FDEFS> <CHANNELS> <OPSYN> &STNO &FNCLEVEL &CASE &OUTPUT &FILE &LINE &RTNTYPE <LASTOUT> fold-case CHAR ORD DATE TIME
                                                REMDR ATAN CHOP COS EXP LN SIN SQRT TAN INTEGER integer-predicate REAL SETEXIT HOST SIZE TRIM DUPL REVERSE LPAD RPAD REPLACE SUBSTR ITEM
                                                PROTOTYPE CONVERT COPY FIELD SORT RSORT DATA open-input-channel! open-output-channel! close-channel! write-to-channel! rewind-channel!
                                                channel-key ANY BREAK BREAKX NOTANY SPAN ARBNO FENCE ABORT BAL LEN RTAB TAB FAIL dump-text fire-value! fire-element! fire-access! fire-call!
                                                fire-return! trace! stoptr!]]
            [clojure.tools.trace :refer :all]
            [SNOBOL4clojure.compiler :as comp :refer [emitter top-replacement parse-expression expression-end ends-in-mark?]])
  (:import  [SNOBOL4clojure.env NAME]))
(defn- assign! [target v]
  (if (fn? target) (some? (target v)) (do (snobol-set! target v) true)))
(def ^:dynamic *trace* false)
(def ^:dynamic *pending-cond* nil)
(defn ζΣ [ζ] (if ζ (ζ 0)))
(defn ζΔ [ζ] (if ζ (ζ 1)))
(defn ζσ [ζ] (if ζ (ζ 2)))
(defn ζδ [ζ] (if ζ (ζ 3)))
(defn ζΠ [ζ] (if ζ (ζ 4)))
(defn ζφ [ζ] (if ζ (ζ 5)))
(defn ζΨ [ζ] (if ζ (ζ 6)))
(defn- ζα [ζ] (<= (ζφ ζ) 1))
(defn- ζω [ζ] (>= (ζφ ζ) (count (ζΠ ζ))))
(defn ζλ [ζ]
  (cond
    (nil?        ζ)  nil
    (list?       ζ)  (first ζ)
    (nil?    (ζΠ ζ)) nil
    (string? (ζΠ ζ)) 'LIT$
    (number? (ζΠ ζ)) 'LIT$
    (instance? SNOBOL4clojure.env.NAME (ζΠ ζ)) 'LIT$
    (symbol? (ζΠ ζ)) (ζΠ ζ)
    (vector? (ζΠ ζ)) 'SEQ
    (list?   (ζΠ ζ)) (first (ζΠ ζ))
    (seq?    (ζΠ ζ)) (first (ζΠ ζ))
    true     (out ["ζλ? " (type (ζΠ ζ)) (ζΠ ζ)])))
(defn- top  [Ψ]   (peek Ψ))
(defn- push [Ψ ζ] (if Ψ (conj Ψ ζ)))
(defn- pull [Ψ]   (if Ψ (if-not (empty? Ψ) (pop Ψ))))
(defn- 🡡 [Ω]     (top Ω))
(defn- 🡥 [Ω ζ]   (push Ω ζ))
(defn- 🡧 [Ω]     (pull Ω))
(defn- ζ↓
  ([ζ Π]  (let [[Σ Δ _ _ _ _ Ψ] ζ] [Σ Δ Σ Δ Π         1       Ψ]))
  ([ζ]    (let [[Σ Δ _ _ Π φ Ψ] ζ] [Σ Δ Σ Δ (nth Π φ) 1       (🡥 Ψ ζ)])))
(defn- ζ↑
  ([ζ σ δ] (let [[Σ Δ _ _ _ _ Ψ] ζ] [Σ Δ σ δ (ζΠ (🡡 Ψ)) (ζφ (🡡 Ψ)) (🡧 Ψ)]))
  ([ζ]     (let [[Σ Δ σ δ _ _ Ψ] ζ] [Σ Δ σ δ (ζΠ (🡡 Ψ)) (ζφ (🡡 Ψ)) (🡧 Ψ)])))
(defn- ζ→ [ζ] (let [[_ _ σ δ Π φ Ψ] ζ] [σ δ σ δ Π (inc φ) Ψ]))
(defn- ζ← [ζ] (let [[Σ Δ _ _ Π φ Ψ] ζ] [Σ Δ Σ Δ Π (inc φ) Ψ]))
(defn- preview
  ([action X φ] (preview action X 0 0 φ))
  ([action X pos depth φ]
   (str
     (if (> pos 0) " " "")
     (cond
       (nil? X)     "nil"
       (char? X)    (str "\\" X)
       (string? X)  (str "\"" X "\"")
       (integer? X) (str X)
       (symbol? X)  (str X)
       (float? X)   (str X)
       (>= depth 3) "_"
       (vector? X)  (str "[" (reduce str (map #(preview action %1 %2 (inc depth) 0) X (range))) "]")
       (list? X)    (str "("
                      (reduce str
                        (map #(cond
                                (equal %2 0) (str %1 " ")
                                (> φ 0)
                                (cond
                                  (< %2 φ)                            "."
                                  (> %2 (clojure.core/+ φ 2))        "?"
                                  (>= %2 (clojure.core/+ φ 2))       " ?"
                                  (and (equal %2 φ)
                                       (identical? action :succeed))  "."
                                  true (preview action %1 (dec %2) (inc depth) 0))
                                true (preview action %1 (dec %2) (inc depth) 0))
                          X (range)))
                      ")")
       (set? X)     (str "\"" (apply str X) "\"")
       true         (str " Yikes!!! " (type X))))))
(defn- animate [action full-subject ζ]
  (when (and *trace* full-subject ζ (vector? ζ))
    (println
      (format "%2s %-8s %16s %3d %16s %-9s %s"
        (count (ζΨ ζ))
        (str (ζλ ζ) "/" (ζφ ζ))
        (str "\"" (apply str (take (ζΔ ζ) full-subject)) "\"")
        (ζΔ ζ)
        (str "\"" (apply str (reverse (ζΣ ζ))) "\"")
        (str " " action)
        (preview action (ζΠ ζ) (ζφ ζ))))))
(def ^:private prim-fns
  {'ANY$  ANY$  'NOTANY$ NOTANY$ 'SPAN$ SPAN$ 'NSPAN$ NSPAN$ 'BREAK$ BREAK$ 'BREAKX$ BREAKX$ 'POS# POS# 'RPOS# RPOS#
   'LEN#  LEN#  'TAB#    TAB#    'RTAB# RTAB# 'BOL#   BOL#   'EOL#   EOL#})
(defn engine [Σ Δ Π start-Δ full-subject]
  (loop [action :proceed, ζ [Σ Δ Σ Δ Π 1 []], Ω []]
    (let [λ (ζλ ζ)]
      (animate action full-subject ζ)
      (case λ
        nil
        (case action
          (:proceed :succeed) [start-Δ (ζδ ζ)]
          (:recede :fail)     (when (clojure.core/= :ABORT ζ) :abort))
        ALT
        (case action
          :proceed (if (not (ζω ζ))
                     (recur :proceed (ζ↓ ζ) (🡥 Ω ζ))
                     (recur :recede  (🡡 Ω)  (🡧 Ω)))
          :recede  (recur :proceed (ζ← ζ) Ω)
          :succeed (recur :succeed (ζ↑ ζ) Ω)
          :fail    (recur :proceed (ζ← ζ) (🡧 Ω)))
        SEQ
        (case action
          :proceed (if (not (ζω ζ))
                     (recur :proceed (ζ↓ ζ) Ω)
                     (recur :succeed (ζ↑ ζ) Ω))
          :succeed (recur :proceed (ζ→ ζ) Ω)
          :fail    (recur :recede  (🡡 Ω) (🡧 Ω)))
        LIT$
        (case action
          :proceed
          (let [[Σ Δ _ _ Π] ζ
                [σ δ]        (LIT$ Σ Δ (cond (string? Π) Π (seq? Π) (str (second Π)) :else (sstr Π)))]
            (if (>= δ 0)
              (recur :succeed (ζ↑ ζ σ δ) Ω)
              (recur :fail    (ζ↑ ζ Σ Δ) Ω))))
        SUCCEED! (let [[Σ Δ] ζ] (recur :succeed (ζ↑ ζ Σ Δ) Ω))
        FAIL!    (let [[Σ Δ] ζ] (recur :fail    (ζ↑ ζ Σ Δ) Ω))
        (ANY$ NOTANY$ SPAN$ NSPAN$ BREAK$ BREAKX$ POS# RPOS# LEN# TAB# RTAB# BOL# EOL#)
        (case action
          :proceed
          (let [[Σ Δ _ _ Π] ζ
                [σ δ]        ((prim-fns λ) Σ Δ (second Π))]
            (if (>= δ 0)
              (recur :succeed (ζ↑ ζ σ δ) Ω)
              (recur :fail    (ζ↑ ζ Σ Δ) Ω))))
        X
        (case action
          :proceed
          (let [Π ($$ 'X)] (recur :proceed (ζ↓ ζ Π) Ω)))
        BREAKX#
        (case action
          :proceed
          (let [[Σ Δ _ _ Π] ζ
                cs           (second Π)
                [σ δ]        (BREAKX$ Σ Δ cs)]
            (if (>= δ 0)
              (recur :succeed (ζ↑ ζ σ δ) (🡥 Ω (assoc ζ 5 (inc δ))))
              (recur :fail (ζ↑ ζ Σ Δ) Ω)))
          :recede
          (let [retry-Δ (ζφ ζ)
                full-len (count full-subject)]
            (if (> retry-Δ full-len)
              (recur :recede (🡡 Ω) (🡧 Ω))
              (let [Σ-retry (drop retry-Δ (seq full-subject))
                    cs      (second (ζΠ ζ))
                    [σ δ]   (BREAKX$ Σ-retry retry-Δ cs)]
                (if (>= δ 0)
                  (recur :succeed (ζ↑ ζ σ δ) (🡥 Ω (assoc ζ 5 (inc δ))))
                  (recur :recede (🡡 Ω) (🡧 Ω))))))
          :succeed (recur :succeed (ζ↑ ζ) Ω)
          :fail    (recur :recede  (🡡 Ω) (🡧 Ω)))
        CAPTURE-IMM
        (case action
          :proceed
          (let [[_ child-P var-sym] (ζΠ ζ)
                ζ (assoc ζ 4 (list 'CAPTURE-IMM child-P var-sym (ζΔ ζ)))]
            (recur :proceed (ζ↓ ζ) (🡥 Ω ζ)))
          :succeed
          (let [[_ _ var-sym start-pos] (ζΠ ζ)
                matched-text  (subs full-subject start-pos (ζδ ζ))]
            (if (assign! var-sym matched-text)
              (recur :succeed (ζ↑ ζ) Ω)
              (recur :recede (🡡 Ω) (🡧 Ω))))
          (:fail :recede)
          (recur :recede (🡡 Ω) (🡧 Ω)))
        CAPTURE
        (case action
          :proceed
          (recur :proceed (ζ↓ ζ) (🡥 Ω ζ))
          :succeed
          (let [[_ _ var-sym] (ζΠ ζ)
                matched-text  (subs full-subject (ζΔ ζ) (ζδ ζ))]
            (assign! var-sym matched-text)
            (recur :succeed (ζ↑ ζ) Ω))
          (:fail :recede)
          (recur :recede (🡡 Ω) (🡧 Ω)))
        CAPTURE-COND
        (case action
          :proceed
          (let [[_ child-P var-sym] (ζΠ ζ)
                start-pos           (ζΔ ζ)
                expanded            (vector (list 'COND-MARK! start-pos)
                                            child-P
                                            (list 'COND-ASSIGN! var-sym start-pos))]
            (recur :proceed (ζ↓ ζ expanded) (🡥 Ω ζ)))
          :succeed (recur :succeed (ζ↑ ζ) Ω)
          (:fail :recede)
          (recur :recede (🡡 Ω) (🡧 Ω)))
        COND-MARK!
        (case action
          :proceed        (recur :succeed (ζ↑ ζ) Ω)
          :succeed        (recur :succeed (ζ↑ ζ) Ω)
          (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))
        COND-ASSIGN!
        (case action
          :proceed
          (let [[_ var-sym start-pos] (ζΠ ζ)
                end-pos              (ζΔ ζ)
                matched-text         (subs full-subject start-pos end-pos)]
            (when *pending-cond*
              (vswap! *pending-cond* conj [var-sym matched-text]))
            (recur :succeed (ζ↑ ζ) (🡥 Ω (list 'COND-UNDO! var-sym))))
          :succeed        (recur :succeed (ζ↑ ζ) Ω)
          (:fail :recede) (recur :recede (🡡 Ω) (🡧 Ω)))
        COND-UNDO!
        (let [[_ var-sym] ζ]
          (when *pending-cond*
            (vswap! *pending-cond*
                    (fn [lst]
                      (if-let [i (last (keep-indexed (fn [i [vs _]] (when (clojure.core/= vs var-sym) i)) lst))]
                        (into (subvec lst 0 i) (subvec lst (inc i)))
                        lst))))
          (recur :recede (🡡 Ω) (🡧 Ω)))
        FENCE!
        (case action
          :proceed
          (let [[Σ Δ _ _ _ _ Ψ] ζ
                child            (second (ζΠ ζ))]
            (if child
              (let [ζ (assoc ζ 4 (list 'FENCE! child (count Ω)))]
                (recur :proceed [Σ Δ Σ Δ child 1 (🡥 Ψ ζ)] (🡥 Ω ζ)))
              (recur :succeed (ζ↑ ζ Σ Δ) (🡥 Ω :ABORT))))
          :succeed
          (let [d (long (nth (ζΠ ζ) 2))]
            (recur :succeed (ζ↑ ζ) (into (subvec Ω 0 d) (filter #(and (list? %) (clojure.core/= 'COND-UNDO! (first %))) (subvec Ω (inc d))))))
          (:recede :fail)
          (recur :recede (🡡 Ω) (🡧 Ω)))
        ARB!
        (case action
          :proceed
          (recur :succeed (ζ↑ ζ (ζΣ ζ) (ζΔ ζ)) (🡥 Ω (assoc ζ 5 1)))
          :recede
          (let [len (ζφ ζ)
                remaining (count (ζΣ ζ))]
            (if (<= len remaining)
              (recur :succeed
                     (ζ↑ ζ (drop len (ζΣ ζ)) (clojure.core/+ (ζΔ ζ) len))
                     (🡥 Ω (assoc ζ 5 (inc len))))
              (recur :recede (🡡 Ω) (🡧 Ω)))))
        ARBNO!
        (case action
          :proceed
          (let [[Σ Δ] ζ]
            (recur :succeed (ζ↑ ζ Σ Δ) (🡥 Ω ζ)))
          :recede
          (let [[Σ Δ _ _ _ _ Ψ] ζ]
            (recur :proceed [Σ Δ Σ Δ (second (ζΠ ζ)) 1 (🡥 Ψ (assoc ζ 5 Δ))] Ω))
          :succeed
          (let [[_ _ σ δ Π start Ψ] ζ]
            (if (clojure.core/= δ start)
              (recur :succeed (ζ↑ ζ) Ω)
              (recur :succeed (ζ↑ ζ) (🡥 Ω [σ δ σ δ Π 1 Ψ]))))
          :fail
          (recur :recede (🡡 Ω) (🡧 Ω)))
        ABORT!
        :abort
        REM!
        (case action
          :proceed
          (recur :succeed (ζ↑ ζ [] (count full-subject)) Ω)
          (:fail :recede)
          (recur :recede (🡡 Ω) (🡧 Ω)))
        BAL!
        (let [sublen  (count full-subject)
              pos0    (ζΔ ζ)
              scan-bal (fn [pos nest]
                         (loop [p pos n nest]
                           (cond
                             (clojure.core/>= p sublen) nil
                             :else
                             (let [ch (nth full-subject p)
                                   n2 (case ch
                                        \( (inc n)
                                        \) (dec n)
                                        n)]
                               (cond
                                 (clojure.core/< n2 0) nil
                                 (clojure.core/= n2 0) [(inc p) (inc p) 0]
                                 :else (recur (inc p) n2))))))]
          (case action
            :proceed
            (if-let [[end np nn] (scan-bal pos0 0)]
              (recur :succeed
                     (ζ↑ ζ (drop end (seq full-subject)) end)
                     (🡥 Ω (assoc ζ 5 [np nn])))
              (recur :fail (ζ↑ ζ (ζΣ ζ) pos0) Ω))
            :recede
            (let [[rp rn] (ζφ ζ)]
              (if-let [[end np nn] (scan-bal rp rn)]
                (recur :succeed
                       (ζ↑ ζ (drop end (seq full-subject)) end)
                       (🡥 Ω (assoc ζ 5 [np nn])))
                (recur :recede (🡡 Ω) (🡧 Ω))))
            :succeed (recur :succeed (ζ↑ ζ) Ω)
            :fail    (recur :recede  (🡡 Ω) (🡧 Ω))))
        COLLECT!
        (case action
          :proceed
          (let [[S D _ _ Pi _ Psi] ζ
                P   (nth Pi 2)
                chi [S D S D P 1 (🡥 Psi ζ)]]
            (recur :proceed chi (🡥 Ω ζ)))
          :succeed
          (let [bag (nth (ζΠ ζ) 1)]
            (swap! bag conj [(ζΔ ζ) (ζδ ζ)])
            (recur :recede (🡡 Ω) (🡧 Ω)))
          (:fail :recede)
          (recur :recede (🡡 Ω) (🡧 Ω)))
        CURSOR-IMM!
        (case action
          (:proceed :succeed)
          (if (assign! (second (ζΠ ζ)) (ζδ ζ))
            (recur :succeed (ζ↑ ζ (ζΣ ζ) (ζδ ζ)) Ω)
            (recur :recede (🡡 Ω) (🡧 Ω)))
          (:fail :recede)
          (recur :recede (🡡 Ω) (🡧 Ω)))
        DEFER!
        (case action
          :proceed
          (let [thunk (second (ζΠ ζ))
                P     (if (fn? thunk) (thunk) thunk)
                _     (when (or (SNOBOL4clojure.env/array? P) (SNOBOL4clojure.env/table? P) (instance? SNOBOL4clojure.env.DataObj P))
                        (throw (ex-info (SNOBOL4clojure.env/error-message 46) {:snobol/error 46})))
                _     (when (> (count (nth ζ 6)) 200000) (throw (ex-info (SNOBOL4clojure.env/error-message 246) {:snobol/error 246 :snobol/abort true})))]
            (if (nil? P)
              (let [[Σ Δ] ζ] (recur :fail (ζ↑ ζ Σ Δ) Ω))
              (recur :proceed (ζ↓ ζ P) (🡥 Ω ζ))))
          :succeed (recur :succeed (ζ↑ ζ) Ω)
          (:fail :recede)
          (recur :recede (🡡 Ω) (🡧 Ω)))))))
(defn- commit-pending!
  "Commit all pending conditional (.) assignments to their variables.
   Called by SEARCH/MATCH/FULLMATCH when the engine returns success.
   They are made in the order the pattern matched them, so the last capture into a variable is the one that stays (sbl -bf).
   False when a target fails (*F() whose F fails, an element out of bounds): the statement fails, as sbl -bf fails it."
  []
  (every? (fn [[var-sym text]] (assign! var-sym text)) @*pending-cond*))
(defn- matching [f]
  (binding [SNOBOL4clojure.env/*matching* true]
    (try (f)
         (catch clojure.lang.ExceptionInfo e
           (let [{n :snobol/error abort :snobol/abort :as data} (ex-data e)]
             (if (and n (not abort) (zero? (long @SNOBOL4clojure.env/&ERRLIMIT)) (not (contains? data :snobol/to)) (not (contains? data :snobol/label))
                      (not (<= 43 (long n) 58)))
               (do (reset! SNOBOL4clojure.env/&ERRTYPE n)
                   (fire-keyword! "ERRTYPE" n)
                   (reset! SNOBOL4clojure.env/&ERRTEXT (ex-message e))
                   nil)
               (throw e)))))))
(defn SEARCH
  "Slide pattern P across string S, trying each start position 0..n, or
   only position 0 while &ANCHOR is non-zero.
   Returns [start end] for the first match found, nil if none.
   (subs S start end) extracts the matched substring.
   Binds *trace* true when a PATTERN trace is active (via trace.clj)."
  [S P]
  (binding [*trace*        (or *trace* (pattern-trace-active? S))
            *pending-cond* (volatile! [])]
    (matching
      #(let [chars (seq S)
             n     (if (zero? @&ANCHOR) (count S) 0)]
         (loop [i 0]
           (when (<= i n)
             (vswap! *pending-cond* (constantly []))
             (let [result (engine (drop i chars) i P i S)]
               (cond
                 (clojure.core/= :abort result) nil
                 result (when (commit-pending!) result)
                 :else  (recur (inc i))))))))))
(defn MATCH
  "Match pattern P against string S anchored at position 0.
   Returns [start end] on success, nil on failure.
   Equivalent to SEARCH with POS(0) prepended."
  [S P]
  (binding [*pending-cond* (volatile! [])]
    (matching
      #(let [result (engine (seq S) 0 (list 'SEQ (POS 0) P) 0 S)
             result (when-not (clojure.core/= :abort result) result)]
         (when (and result (commit-pending!)) result)))))
(defn FULLMATCH
  "Match pattern P against the entirety of string S (anchored both ends).
   Returns [0 (count S)] on success, nil on failure.
   Equivalent to SEARCH with POS(0) prepended and RPOS(0) appended."
  [S P]
  (binding [*pending-cond* (volatile! [])]
    (matching
      #(let [result (engine (seq S) 0 (list 'SEQ (POS 0) P (RPOS 0)) 0 S)
             result (when-not (clojure.core/= :abort result) result)]
         (when (and result (commit-pending!)) result)))))
(defn MATCH-REPLACE
  "Match pattern P against S; if matched, replace the matched span with R.
   Returns the new string on success, nil on failure.
   Example: (REPLACE \"hello world\" \"world\" \"Clojure\") => \"hello Clojure\""
  [S P R]
  (when-let [[start end] (SEARCH S P)]
    (str (subs S 0 start) R (subs S end))))
(defn COLLECT!
  "Exhaustively collect every match [start end] of pattern P across string S,
   including all multi-yield backtrack alternatives (e.g. BAL, ARB, ARBNO).
   Mirrors the SNOBOL4 idiom  P $ X FAIL  — each match is captured then the
   engine is forced to recede, driving P through all its Omega retry frames.
   Returns a vector of [start end] pairs in scan order."
  [S P]
  (let [chars (seq S)
        n     (count S)
        bag   (atom [])
        node  (list 'COLLECT! bag P)]
    (loop [i 0]
      (when (and (<= i n) (not (clojure.core/= :abort (engine (drop i chars) i node i S))))
        (recur (inc i))))
    @bag))
(defmacro numcvt [x] `(ncvt ~x))
(defmacro uneval [x] `(if (list? ~x) ~x (list 'identity ~x)))
(declare APPLY)
(defn- channel-variable [name-arg]
  (cond
    (symbol? name-arg)                            name-arg
    (instance? SNOBOL4clojure.env.NAME name-arg)  (symbol (str (.n ^SNOBOL4clojure.env.NAME name-arg)))
    :else                                         (symbol (str name-arg))))
(defn annihilate    [_x]        nil)
(defn match-replace [_n _s _p]  nil)
(defn keyword-value [_n]        nil)
(defn $=            [_p _n])
(defn .=            [_p _n])
(defn lie           [p]        `(if (nil? ~p) ε nil))
(defn x-2           [op x y]    (list op x y))
(defn x-n           [op x y Ω]  (apply list (conj Ω y x op)))
(defn n-1           [op x]      (list op (numcvt x)))
(defn n-2           [op x y]    (list op (numcvt x) (numcvt y)))
(defn n-n           [op x y Ω]  (apply list op (map ncvt (conj Ω y x))))
(defn- primitive [func default missing cvt condition]
  (list 'defn func
    (list []            missing)
    (list ['x]          (list 'if (condition (cvt 'x) default) ε))
    (list ['x 'y '& '_] (list 'if (condition (cvt 'x) (cvt 'y)) ε))))
(defn- lexical [x y n1 test] (let [b (gtstg y (inc (long n1))) a (gtstg x n1)] (when (test (compare a b)) ε)))
(defn LEQ ([] (LEQ ε ε)) ([x] (LEQ x ε)) ([x y & _] (lexical x y 122 zero?)))
(defn LNE ([] (LNE ε ε)) ([x] (LNE x ε)) ([x y & _] (lexical x y 132 #(not (zero? %)))))
(defn LLE ([] (LLE ε ε)) ([x] (LLE x ε)) ([x y & _] (lexical x y 128 #(<= % 0))))
(defn LLT ([] (LLT ε ε)) ([x] (LLT x ε)) ([x y & _] (lexical x y 130 neg?)))
(defn LGE ([] (LGE ε ε)) ([x] (LGE x ε)) ([x y & _] (lexical x y 124 #(>= % 0))))
(defn LGT ([] (LGT ε ε)) ([x] (LGT x ε)) ([x y & _] (lexical x y 126 pos?)))
(eval (primitive 'IDENT  ε   ε identity #(list 'SNOBOL4clojure.env/snobol-ident? %1 %2)))
(eval (primitive 'DIFFER ε nil identity #(list 'not (list 'SNOBOL4clojure.env/snobol-ident? %1 %2))))
(defn =     ([_x]       η)
            ([n x]      (list '= n x)))
(defn ?     ([x]        (annihilate x))
            ([s p]      (let [s (sstr s)] (when-let [[a b] (SEARCH s p)] (subs s a b))))
            ([n s p]    (match-replace n s p)))
(defn ?=    ([n s p]    (match-replace n s p)))
(defn &     ([_n]       nil)
            ([_x _y]    η))
(defn at    ([n]        (list 'CURSOR-IMM! n))
            ([_x _y]    η))
(defn +     ([x]        (n-1 Σ+ x))
            ([x y]      (n-2 Σ+ x y))
            ([x y & zs] (n-n Σ+ x y zs)))
(defn -     ([x]        (n-1 subtract x))
            ([x y]      (n-2 subtract x y))
            ([x y & zs] (n-n subtract x y zs)))
(defn sharp ([_x]       η)
            ([_x _y]    η))
(defn /     ([_x]       η)
            ([x y]      (n-2 divide x y)))
(defn *     ([x]        (list 'DEFER! (fn [] x)))
            ([x y]      (n-2 multiply x y))
            ([x y & zs] (n-n multiply x y zs)))
(defn %     ([_x]       η)
            ([_x _y]    η))
(defn- operand [x code msg]
  (let [n (num x)]
    (if (and (double? n) (Double/isNaN n) (not (double? x)))
      (throw (ex-info msg {:snobol/error code}))
      n)))
(defn- exact [exact-op a b code msg]
  (try (exact-op (long a) (long b)) (catch ArithmeticException _ (throw (ex-info msg {:snobol/error code})))))
(defn- finite [r code msg]
  (let [r (double r)]
    (cond (Double/isInfinite r)               (throw (ex-info msg {:snobol/error code}))
          (< (Math/abs r) Double/MIN_NORMAL)  (clojure.core/* r 0.0)
          :else                               r)))
(defmacro ^:private numeric-predicate [f cmp first-code second-code]
  (let [nm (clojure.string/lower-case (name f))]
    `(defn ~f [& args#]
       (let [b# (operand (second args#) ~second-code ~(str nm " second argument is not numeric"))
             a# (operand (first args#) ~first-code ~(str nm " first argument is not numeric"))]
         (when (~cmp a# b#) ε)))))
(numeric-predicate EQ clojure.core/== 101 102)
(numeric-predicate NE (fn [a b] (not (clojure.core/== a b))) 149 150)
(numeric-predicate LT clojure.core/< 147 148)
(numeric-predicate LE clojure.core/<= 118 119)
(numeric-predicate GT clojure.core/> 111 112)
(numeric-predicate GE clojure.core/>= 109 110)
(defn pow-fn
  "SNOBOL4 exponentiation (**, and its synonyms ^ and !) as sbl.min o_exp computes it. An integer exponent not below zero is repeated multiplication: of an
   integer base, exact, 0 ** 0 error 18 and a result past 64 bits error 17; of a real base, each product rounded, 0.0 ** 0 error 18 and an overflow error 266.
   A real exponent, or a negative integer one, gives e ** (exponent * ln(base)) (glibc's exp and log, which Math/exp and Math/log follow): a zero base gives
   0.0, or error 18 to the power 0.0; a negative base is allowed only to an integral power, whose parity is the sign, else error 311."
  [x y]
  (let [y   (operand y 15 "exponentiation right operand is not numeric")
        x   (operand x 16 "exponentiation left operand is not numeric")
        ovf "exponentiation caused real overflow"
        undefined #(throw (ex-info "exponentiation result is undefined" {:snobol/error 18}))]
    (cond
      (and (integer? x) (integer? y) (not (neg? y)))
      (cond
        (and (zero? x) (zero? y)) (undefined)
        (zero? x) 0
        (clojure.core/= x 1) 1
        (clojure.core/= x -1) (if (even? y) 1 -1)
        (clojure.core/> y 64)
        (throw (ex-info "exponentiation caused integer overflow" {:snobol/error 17}))
        :else
        (let [r (.pow (biginteger x) (int y))]
          (if (clojure.core/< (.bitLength r) 64)
            (long r)
            (throw (ex-info "exponentiation caused integer overflow" {:snobol/error 17})))))
      (and (integer? y) (not (neg? y)))
      (let [b (double x)]
        (if (zero? y)
          (if (zero? b) (undefined) 1.0)
          (loop [r b k (dec (long y))]
            (if (or (zero? k) (zero? r)) r (recur (finite (clojure.core/* r b) 266 ovf) (dec k))))))
      :else
      (let [b (double x) e (double y)]
        (if (zero? b)
          (if (zero? e) (undefined) 0.0)
          (let [c   (when (neg? b)
                      (let [t (if (neg? e) (Math/ceil e) (Math/floor e))
                            c (if (< (Math/abs t) 9.223372036854775807E18) (long t) Long/MIN_VALUE)]
                        (when (== c 2147483648) (throw (ex-info ovf {:snobol/error 266})))
                        (when-not (== t e)
                          (throw (ex-info "exponentiation of negative base to non-integral power" {:snobol/error 311})))
                        c))
                r   (finite (Math/exp (finite (clojure.core/* (finite (Math/log (Math/abs b)) 266 ovf) e) 266 ovf)) 266 ovf)]
            (if (and c (odd? c)) (clojure.core/- r) r)))))))
(defn !     ([_x]       η)
            ([x y]      (pow-fn x y)))
(defn **    ([x y]      (pow-fn x y)))
(defn $     ([n]        ($$ n))
            ([x y]      (x-2 'CAPTURE-IMM x y))
            ([x y & zs] (x-n 'CAPTURE-IMM x y zs)))
(defn .     ([x]        (NAME. x))
            ([x y]      (x-2 'CAPTURE-COND x y))
            ([x y & zs] (x-n 'CAPTURE-COND x y zs)))
(defn tilde ([x]        (list 'ALT x ε))
            ([_x _y]    η))
(defn |     ([_x]       η)
            ([x y]      (x-2 'ALT x y))
            ([x y & zs] (x-n 'ALT x y zs)))
(defn- opsyn-key [spelling arity] (str spelling "/" arity))
(def ^:private op-spelling {'at-op "@", 'sharp-op "#", 'tilde-op "~"})
(defn- arg-or-local [[fname n] k e1 t1 e2 t2]
  (let [meta (get @<FDEFS> (name-string fname))
        _    (when-not meta (throw (ex-info t1 {:snobol/error e1})))
        i    (let [v (SNOBOL4clojure.env/num n)] (if (and (number? v) (not (and (double? v) (Double/isNaN v)))) (long v) (throw (ex-info t2 {:snobol/error e2}))))]
    (when-some [p (when (pos? i) (nth (k meta) (dec i) nil))]
      (NAME. (symbol p)))))
(declare EVAL! compile-expression compile-failed! compiled! deferred? expression-of unused-unary-ops unused-binary-ops name-of)
(defn- textless? [v]
  (or (and (instance? NAME v) (not (symbol? (.n ^NAME v))))
      (array? v) (table? v) (instance? SNOBOL4clojure.env.DataObj v) (instance? SNOBOL4clojure.env.CodeBlock v)))
(defn- pattern-argument [p err] (if (textless? p) (throw (ex-info (SNOBOL4clojure.env/error-message err) {:snobol/error err})) p))
(defn- pattern-operands! [args]
  (let [v (vec args) n (count v)]
    (pattern-argument (v (dec n)) 5)
    (doseq [i (range (clojure.core/- n 2) -1 -1)] (pattern-argument (v i) 6))))
(defn- concat-value [r]
  (if (and (list? r) (clojure.core/= (first r) 'SEQ))
    (let [parts (remove #(clojure.core/= ε %) (rest r))
          _     (when (next parts)
                  (let [v (vec parts) n (count v) e8 #(throw (ex-info "concatenation left operand is not a string or pattern" {:snobol/error 8}))]
                    (cond (textless? (v (clojure.core/- n 2)))                (e8)
                          (textless? (v (dec n)))                               (throw (ex-info "concatenation right operand is not a string or pattern" {:snobol/error 9}))
                          (some textless? (subvec v 0 (clojure.core/- n 2)))    (e8))))]
      (cond
        (every? #(or (string? %) (number? %) (instance? NAME %)) parts)
        (if (and (clojure.core/= 1 (count parts)) (< 1 (count (rest r))))
          (first parts)
          (let [t (apply str (map sstr parts))]
            (if (clojure.core/> (count t) (long @SNOBOL4clojure.env/&MAXLNGTH))
              (throw (ex-info "string length exceeds value of maxlngth keyword" {:snobol/error 205}))
              t)))
        (clojure.core/= (count parts) (count (rest r))) r
        (empty? (rest parts))                        (first parts)
        :else                                        (apply list 'SEQ parts)))
    r))
(defn- output-text [v]
  (if (or (and (or (list? v) (vector? v)) (#{"PATTERN" "EXPRESSION"} (SNOBOL4clojure.env/DATATYPE v))) (textless? v))
    (SNOBOL4clojure.env/DATATYPE v)
    (sstr v)))
(defn- program-fn
  "The function a program gave the name op, or nil: a DATA constructor or field (held as the value of the name), or a DEFINEd or OPSYN function (held in
   <FUNS>). Only names that start with a letter: operators are not looked up."
  [op]
  (when (and (symbol? op) (Character/isLetter (.charAt (name op) 0)))
    (let [v (reference op)]
      (if (fn? v) v (get @<FUNS> (name op))))))
(defn- restore-var! [sym v]
  (if (nil? v)
    (do (swap! SNOBOL4clojure.env/<VARS> dissoc sym) (swap! SNOBOL4clojure.env/<ASSOC-LAST> dissoc sym))
    (snobol-set! sym v)))
(defn- saved-value [sym] (if (SNOBOL4clojure.env/output-associated? sym) (get @SNOBOL4clojure.env/<ASSOC-LAST> sym) (reference sym)))
(def ^:private fixed-arity
  (memoize (fn [^Class c]
             (let [ms (.getDeclaredMethods c)]
               (when-not (some (fn [^java.lang.reflect.Method m] (clojure.core/= "doInvoke" (.getName m))) ms)
                 (reduce max -1 (keep (fn [^java.lang.reflect.Method m] (when (clojure.core/= "invoke" (.getName m)) (alength (.getParameterTypes m)))) ms)))))))
(defn- apply-builtin [f args]
  (let [n (fixed-arity (class f))]
    (if (and n (clojure.core/>= n 0) (clojure.core/> (count args) n)) (apply f (take n args)) (apply f args))))
(defn INVOKE [op & args]
  (if-let [pf (program-fn op)]
    (apply pf args)
  (case op
    |       (do (when (<= 2 (count args)) (pattern-operands! args)) (apply | args))
    $       (do (when (and (<= 2 (count args)) (textless? (first args))) (throw (ex-info (SNOBOL4clojure.env/error-message 25) {:snobol/error 25})))
                (apply $ args))
    .       (do (when (and (<= 2 (count args)) (textless? (first args))) (throw (ex-info (SNOBOL4clojure.env/error-message 30) {:snobol/error 30})))
                (apply . args))
    LEN     (LEN    (first args))
    POS     (POS    (first args))
    RPOS    (RPOS   (first args))
    TAB     (TAB    (first args))
    RTAB    (RTAB   (first args))
    ANY     (ANY    (first args))
    BREAK   (BREAK  (first args))
    BREAKX  (BREAKX (first args))
    NOTANY  (NOTANY (first args))
    SPAN    (SPAN   (first args))
    FENCE   (FENCE (pattern-argument (if (seq args) (first args) ε) 259))
    ARBNO   (ARBNO  (pattern-argument (first args) 61))
    EQ      (EQ     (first args) (second args))
    NE      (NE     (first args) (second args))
    LE      (LE     (first args) (second args))
    LT      (LT     (first args) (second args))
    GE      (GE     (first args) (second args))
    GT      (GT     (first args) (second args))
    +       (if (clojure.core/= 1 (count args))
              (operand (first args) 4 "affirmation operand is not numeric")
              (let [b (operand (second args) 2 "addition right operand is not numeric")
                    a (operand (first args) 1 "addition left operand is not numeric")]
                (if (and (integer? a) (integer? b))
                  (exact #(Math/addExact ^long %1 ^long %2) a b 3 "addition caused integer overflow")
                  (finite (clojure.core/+ a b) 261 "addition caused real overflow"))))
    -       (if (clojure.core/= 1 (count args))
              (let [a (operand (first args) 10 "negation operand is not numeric")]
                (if (integer? a) (exact #(Math/subtractExact ^long %1 ^long %2) 0 a 11 "negation caused integer overflow") (clojure.core/- a)))
              (let [b (operand (second args) 33 "subtraction right operand is not numeric")
                    a (operand (first args) 32 "subtraction left operand is not numeric")]
                (if (and (integer? a) (integer? b))
                  (exact #(Math/subtractExact ^long %1 ^long %2) a b 34 "subtraction caused integer overflow")
                  (finite (clojure.core/- a b) 264 "subtraction caused real overflow"))))
    *       (let [b (operand (second args) 27 "multiplication right operand is not numeric")
                  a (operand (first args) 26 "multiplication left operand is not numeric")]
              (if (and (integer? a) (integer? b))
                (exact #(Math/multiplyExact ^long %1 ^long %2) a b 28 "multiplication caused integer overflow")
                (finite (clojure.core/* a b) 263 "multiplication caused real overflow")))
    tilde-op (let [v (first args)]
              (if (nil? v) ε nil))
    /       (let [b (operand (second args) 13 "division right operand is not numeric")
                  a (operand (first args) 12 "division left operand is not numeric")]
              (cond
                (and (integer? a) (integer? b))
                (if (or (zero? b) (and (clojure.core/= b -1) (clojure.core/= a Long/MIN_VALUE)))
                  (throw (ex-info "division caused integer overflow" {:snobol/error 14}))
                  (quot a b))
                (and (zero? (double b)) (zero? (double a))) 0.0
                (zero? (double b)) (throw (ex-info "division caused real overflow" {:snobol/error 262}))
                :else (finite (clojure.core// (double a) (double b)) 262 "division caused real overflow")))
    **      (let [[x y] args] (pow-fn x y))
    ?       (let [[s p] args s (gtstg s 241)] (when-let [[a b] (SEARCH s (pattern-argument p 240))] (subs s a b)))
    =       (let [[N r] args]
              (when-not (nil? r)
              (cond
                (instance? SNOBOL4clojure.env.ExprVar N)
                (when-some [t (name-of (.ir ^SNOBOL4clojure.env.ExprVar N))] (INVOKE '= t r))
                (and (list? N) (clojure.core/= (first N) '&))
                (do (assign-keyword! (second N) r) r)
                (and (list? N) (>= (count N) 2))
                (let [[container-sym & ks] N
                      r (concat-value r)
                      raw-container (cond
                                      (:container (meta N)) (:container (meta N))
                                      (symbol? container-sym)
                                      (let [v ($$ container-sym) f (get @<FUNS> (name container-sym))]
                                        (if (and (:data (meta f)) (or (not (:subscript (meta N))) (and (not (table? v)) (not (array? v))))) f v))
                                      :else container-sym)
                      container (if (instance? NAME raw-container)
                                  ($$ (symbol (str (.n raw-container))))
                                  raw-container)]
                  (cond
                    (table? container)
                    (let [v (table-set container (SNOBOL4clojure.env/table-subscript ks) r)] (fire-element! container ks r) v)
                    (array? container)
                    (when-some [v (array-set container (vec ks) r)] (fire-element! container ks r) v)
                    (fn? container)
                    (when (container (first ks) r) r)
                    (and (symbol? container-sym) (not (:subscript (meta N)))) (throw (ex-info "undefined function called" {:snobol/error 22}))
                    :else (throw (ex-info "subscripted operand is not table or array" {:snobol/error 235}))))
                (clojure.core/contains? #{'OUTPUT 'TERMINAL 'INPUT} N)
                (let [out-val (concat-value r)]
                  (swap! SNOBOL4clojure.env/<ASSOC-LAST> assoc N out-val)
                  (when (and (clojure.core/= N 'OUTPUT) (not (zero? (long @&OUTPUT)))) (println (output-text out-val)))
                  (when (clojure.core/= N 'TERMINAL) (binding [*out* *err*] (println (output-text out-val))))
                  out-val)
                (clojure.core/= :output (:type (get @<CHANNELS> N)))
                (let [out-val (concat-value r)]
                  (write-to-channel! N out-val)
                  (swap! SNOBOL4clojure.env/<ASSOC-LAST> assoc N out-val)
                  out-val)
                :else
                (let [v (concat-value r)]
                  (snobol-set! N v)
                  (fire-value! N v)
                  v))))
    ?=      (let [[n p R] args
                  subject (gtstg (if (list? n)
                                  (apply INVOKE (or (:container (meta n)) (first n)) (rest n))
                                  (let [v ($$ n)] (when (pos? (long @SNOBOL4clojure.env/&TRACE)) (fire-access! n v)) v))
                                241)
                  pat     (pattern-argument p 240)]
              (when-let [[start end] (SEARCH subject pat)]
                (let [repl   (sstr (EVAL! R))
                      result (str (subs subject 0 start)
                                  repl
                                  (subs subject end))]
                  (when (clojure.core/> (count result) (long @SNOBOL4clojure.env/&MAXLNGTH))
                    (throw (ex-info "string length exceeds value of maxlngth keyword" {:snobol/error 205})))
                  (INVOKE '= n result)
                  result)))
    DEFINE  (let [[proto entry-arg] args
                  lp        (.indexOf ^String proto "(")
                  rp        (.indexOf ^String proto ")")
                  fname     (if (>= lp 0) (.substring ^String proto 0 lp) proto)
                  inner     (if (and (>= lp 0) (>= rp 0))
                              (.substring ^String proto (inc lp) rp) "")
                  after-rp  (if (>= rp 0) (.substring ^String proto (inc rp)) "")
                  params    (if (clojure.string/blank? inner) []
                              (mapv clojure.string/trim
                                    (clojure.string/split inner #",")))
                  locals    (if (clojure.string/blank? after-rp) []
                              (let [s (-> after-rp clojure.string/trim
                                          (clojure.string/replace #"^," ""))]
                                (filterv (complement clojure.string/blank?)
                                         (mapv clojure.string/trim
                                               (clojure.string/split s #",")))))
                  f-sym     (symbol fname)
                  entry     (if (and (>= (count args) 2) entry-arg (not (clojure.core/= "" (sstr entry-arg))))
                              (if (symbol? entry-arg)
                                (keyword (str (name entry-arg)))
                                (keyword (str entry-arg)))
                              (keyword fname))
                  all-saved (into params locals)]
              (letfn [(the-fn [& call-args]
                        (let [saved   (zipmap all-saved (map #(saved-value (symbol %)) all-saved))
                              f-saved (saved-value f-sym)
                              stno-saved @&STNO
                              file-saved (if-some [[f _] SNOBOL4clojure.env/*eval-block*] f @&FILE)
                              line-saved (if-some [[_ l] SNOBOL4clojure.env/*eval-block*] l @&LINE)]
                          (snobol-set! f-sym ε)
                          (doseq [i (range (count params))]
                            (snobol-set! (symbol (params i))
                                         (nth call-args i ε)))
                          (doseq [l locals]
                            (snobol-set! (symbol l) ε))
                          (fire-call! fname (map #(reference (symbol %)) params))
                          (fire-keyword! "FNCLEVEL" (swap! &FNCLEVEL inc))
                          (let [run-fn  (ns-resolve 'SNOBOL4clojure.runtime 'RUN)
                                d       (inc (long SNOBOL4clojure.env/*fp*))
                                outcome (try
                                          (when run-fn (binding [SNOBOL4clojure.env/*fp* d SNOBOL4clojure.env/*flprt* d SNOBOL4clojure.env/*eval-block* nil] ((var-get run-fn) entry)))
                                          :return
                                          (catch clojure.lang.ExceptionInfo e
                                            (let [sig (:snobol/signal (ex-data e))]
                                              (if (#{:return :freturn :nreturn} sig)
                                                sig
                                                (throw (if (contains? (ex-data e) :fnclevel) e (ex-info (ex-message e) (assoc (ex-data e) :fnclevel @&FNCLEVEL) e))))))
                                          (finally (fire-keyword! "FNCLEVEL" (swap! &FNCLEVEL dec))))]
                            (let [result ($$ f-sym)
                                  _      (reset! &RTNTYPE (case outcome :freturn "FRETURN" :nreturn "NRETURN" "RETURN"))
                                  _      (fire-return! fname result outcome stno-saved)
                                  result ($$ f-sym)]
                              (doseq [[k v] saved]
                                (restore-var! (symbol k) v))
                              (restore-var! f-sym f-saved)
                              (reset! SNOBOL4clojure.env/&LASTNO @&STNO)
                              (reset! SNOBOL4clojure.env/&LASTLINE @&LINE)
                              (reset! SNOBOL4clojure.env/&LASTFILE @&FILE)
                              (reset! &STNO stno-saved)
                              (reset! &FILE file-saved)
                              (reset! &LINE line-saved)
                              (reset! SNOBOL4clojure.env/<STMT-LINE> line-saved)
                              (reset! &RTNTYPE (case outcome :freturn "FRETURN" :nreturn "NRETURN" "RETURN"))
                              (case outcome
                                :return  result
                                :freturn nil
                                :nreturn (cond (instance? NAME result) (SNOBOL4clojure.env/->NameResult (.n ^NAME result))
                                               (and (string? result) (seq result)) (SNOBOL4clojure.env/->NameResult (symbol result))
                                               (number? result) (SNOBOL4clojure.env/->NameResult (symbol (sstr result)))
                                               :else (throw (ex-info "function result in NRETURN is not name" {:snobol/error 243})))
                                result)))))]
                (swap! <FUNS>  assoc fname the-fn)
                (SNOBOL4clojure.env/note-names! (concat [fname] params locals))
                (swap! <FDEFS> assoc fname
                               {:params params :locals locals})
                ε))
    OPSYN   (let [[new-arg old-arg n-arg] args
                  new-name (name-string new-arg)
                  old-name (name-string old-arg)
                  old-sym  (symbol (name-string old-arg))
                  fail!    (fn [n text] (throw (ex-info text {:snobol/error n})))
                  n        (when (and n-arg (not= n-arg ε))
                             (let [v (SNOBOL4clojure.env/num n-arg)]
                               (cond (or (not (number? v)) (and (double? v) (Double/isNaN v))) (fail! 152 "OPSYN third argument is not integer")
                                     (or (neg? v) (clojure.core/> v 16777216))                     (fail! 153 "OPSYN third argument is negative or too large")
                                     :else (long v))))
                  _        (cond
                             (clojure.core/= "" new-name) (fail! 155 "OPSYN first arg is not natural variable name")
                             (clojure.core/= "" old-name) (fail! 154 "OPSYN second arg is not natural variable name")
                             (or (nil? n) (zero? n))
                             (when (contains? SNOBOL4clojure.env/system-functions new-name) (fail! 248 "attempted redefinition of system function"))
                             :else nil)
                  n        (when n (min n 2))
                  old-fn   (or (get @<FUNS> old-name)
                               (if (and n (pos? n))
                                 (if (clojure.core/= n 1)
                                   (fn [a]   (INVOKE old-sym a))
                                   (fn [a b] (INVOKE old-sym a b)))
                                 (fn [& a] (apply INVOKE old-sym (vec a)))))]
              (if (and n (pos? n))
                (swap! <FUNS> assoc (opsyn-key (if (and (clojure.core/= 2 n) (#{"^" "!"} new-name)) "**" new-name) n) old-fn)
                (swap! <FUNS> assoc new-name old-fn))
              ε)
    LOAD    (let [p  (sstr (nth args 0 ""))
                  lp (clojure.string/index-of p "(")
                  e  #(throw (ex-info (SNOBOL4clojure.env/error-message %) {:snobol/error %}))]
              (cond (clojure.core/= "" p)                             (e 138)
                    (nil? lp)                                         (e 139)
                    (zero? (long lp))                                 (e 140)
                    (nil? (clojure.string/index-of p ")" lp))        (e 141)
                    :else                                             (e 142)))
    UNLOAD  (let [nm (name-string (first args))]
              (cond
                (clojure.core/= "" nm)                          (throw (ex-info "UNLOAD argument is not natural variable name" {:snobol/error 201}))
                (contains? SNOBOL4clojure.env/system-functions nm) (throw (ex-info "attempted redefinition of system function" {:snobol/error 248}))
                :else (do (swap! <FUNS> dissoc nm) (swap! <FDEFS> dissoc nm) ε)))
    APPLY   (apply APPLY (first args) (rest args))
    REPLACE (let [[s1 s2 s3] args] (REPLACE s1 s2 s3))
    TABLE   (apply-builtin SNOBOL4clojure.env/TABLE args)
    ARRAY   (apply-builtin SNOBOL4clojure.env/ARRAY args)
    REMDR   (REMDR  (first args) (second args))
    TRACE   (let [[nm ty lbl f] args] (trace! nm (or ty "VALUE") lbl f) ε)
    STOPTR  (let [[nm ty] args] (stoptr! nm (or ty "VALUE")) ε)
    COLLECT (do (System/gc)
                (let [free (clojure.core/- (quot (.maxMemory (Runtime/getRuntime)) 8) (SNOBOL4clojure.env/storage-words))
                      want (SNOBOL4clojure.env/num (first args))]
                  (when-not (and (integer? want) (clojure.core/> want free)) free)))
    CLEAR     (SNOBOL4clojure.env/clear-vars! (first args))
    BACKSPACE (apply-builtin SNOBOL4clojure.env/BACKSPACE args)
    EJECT     (let [u (first args)]
                (if (or (nil? u) (clojure.core/= ε u))
                  (do (print "\f") (flush) ε)
                  (let [ch (get @<CHANNELS> (channel-key u))]
                    (cond
                      (nil? ch)                                (throw (ex-info "EJECT file does not exist" {:snobol/error 93}))
                      (clojure.core/= :output (:type ch))      (let [w ^java.io.PrintWriter (:writer ch)] (.println w "\f") (.flush w) ε)
                      :else                                    ε))))
    SETEXIT (apply-builtin SETEXIT args)
    HOST    (apply HOST args)
    DUMP    (let [n (num (first args))] (when (and (integer? n) (clojure.core/<= 1 n 3)) (print (dump-text n))) ε)
    ATAN    (ATAN   (first args))
    CHOP    (CHOP   (first args))
    COS     (COS    (first args))
    EXP     (EXP    (first args))
    LN      (LN     (first args))
    SIN     (SIN    (first args))
    SQRT    (SQRT   (first args))
    TAN     (TAN    (first args))
    INTEGER (integer-predicate (first args))
    SIZE    (SIZE   (first args))
    TRIM    (TRIM   (first args))
    DUPL    (DUPL   (first args) (second args))
    REVERSE (REVERSE (first args))
    ITEM      (apply ITEM args)
    PROTOTYPE (apply-builtin PROTOTYPE args)
    CONVERT   (let [[x t] args]
                (if (clojure.core/= "EXPRESSION" (clojure.string/upper-case (str t)))
                  (cond (deferred? x) x
                        (string? x)   (when-not (ends-in-mark? x) (when-some [ir (compile-expression x)] (expression-of ir)))
                        (number? x)   (expression-of x)
                        :else         (CONVERT x t))
                  (CONVERT x t)))
    COPY      (COPY      (first args))
    FIELD     (FIELD     (first args) (second args))
    SORT      (apply-builtin SORT args)
    RSORT     (apply-builtin RSORT args)
    DATA      (DATA      (first args))
    DATATYPE  (SNOBOL4clojure.env/DATATYPE (first args))
    CHAR      (CHAR      (first args))
    ORD       (when (seq args) (ORD (first args)))
    SUBSTR    (SUBSTR    (first args) (second args) (nth args 2 ""))
    DATE      (apply-builtin DATE args)
    TIME      (TIME)
    LPAD      (LPAD (nth args 0 "") (nth args 1 "") (nth args 2 ""))
    RPAD      (RPAD (nth args 0 "") (nth args 1 "") (nth args 2 ""))
    LEQ       (LEQ    (first args) (second args))
    LNE       (LNE    (first args) (second args))
    LLE       (LLE    (first args) (second args))
    LLT       (LLT    (first args) (second args))
    LGE       (LGE    (first args) (second args))
    LGT       (LGT    (first args) (second args))
    ARG       (arg-or-local args :params 63 "ARG first argument is not program function name" 62 "ARG second argument is not integer")
    LOCAL     (arg-or-local args :locals 135 "LOCAL first arg is not a program function name" 134 "LOCAL second argument is not integer")
    INPUT     (let [[name-arg unit filename] args]
                (open-input-channel! (channel-variable name-arg) unit filename))
    OUTPUT    (let [[name-arg unit filename] args]
                (open-output-channel! (channel-variable name-arg) unit filename))
    ENDFILE   (let [u (first args)]
                (cond (or (nil? u) (clojure.core/= "" u))           (throw (ex-info (SNOBOL4clojure.env/error-message 97) {:snobol/error 97}))
                      (nil? (get @<CHANNELS> (channel-key u)))       (throw (ex-info (SNOBOL4clojure.env/error-message 98) {:snobol/error 98}))
                      :else                                          (close-channel! (channel-key u))))
    DETACH    (let [v (first args)
                    sym (cond
                          (symbol? v) v
                          (instance? SNOBOL4clojure.env.NAME v) (symbol (str (.n ^SNOBOL4clojure.env.NAME v)))
                          :else (symbol (str v)))]
                (do (swap! <CHANNELS> dissoc sym) ε))
    REWIND    (rewind-channel! (first args))
    EXIT      (apply-builtin SNOBOL4clojure.env/EXIT args)
    CODE      (let [src       (str (first args))
                    compile-f (ns-resolve 'SNOBOL4clojure.compiler 'CODE-block)]
                (when-not compile-f
                  (throw (ex-info "CODE: compiler not loaded"
                                  {:snobol/signal :error})))
                (let [entry (try (compile-f src) (catch Exception e (throw (ex-info (.getMessage e) {:snobol/signal :error}))))]
                    (if (map? entry)
                      (if (and (pos? (long @SNOBOL4clojure.env/&ERRLIMIT)) @SNOBOL4clojure.env/<SETEXIT>)
                        (do (compiled! false) (throw (ex-info (SNOBOL4clojure.env/error-message (:error entry)) {:snobol/error (:error entry)})))
                        (compile-failed! (:error entry) (SNOBOL4clojure.env/error-message (:error entry))))
                      (when (some? entry) (compiled! true) (SNOBOL4clojure.env/->CodeBlock entry)))))
            (let [raw-f (if (symbol? op) ($$fn op) op)
                  f (if (instance? NAME raw-f)
                      ($$ (symbol (str (.n raw-f))))
                      raw-f)
                  f (if (fn? f) f
                      (get @<FUNS> (str op) f))]
              (cond
                (table? f) (table-get f (SNOBOL4clojure.env/table-subscript args))
                (array? f) (array-get f (vec args))
                (fn? f)    (apply-builtin f args)
                :else      (throw (ex-info "undefined function called"
                                           {:snobol/error 22 :function op})))))))
(defn- applied-function [fname]
  (let [s (SNOBOL4clojure.env/gtstg fname 60)]
    (when (clojure.core/= "" s) (throw (ex-info (SNOBOL4clojure.env/error-message 60) {:snobol/error 60})))
    (symbol s)))
(defn APPLY
  "APPLY(fname, arg1, ...) — call the function fname names as a call written in the program calls it: through INVOKE, built-ins included."
  [fname & fargs]
  (apply INVOKE (applied-function fname) fargs))
(def ^:private unused-unary-ops
  {'! "!", '% "%", '/ "/", 'sharp-op "#", '= "=", '| "|", (symbol "^") "^"})
(def ^:private unused-binary-ops
  {'& "&", 'at-op "@", 'sharp-op "#", '% "%", 'tilde-op "~"})
(defn- indirect-target [v]
  (cond
    (and (instance? NAME v) (list? (.n ^NAME v))) (.n ^NAME v)
    (and (instance? NAME v) (instance? SNOBOL4clojure.env.ExprVar (.n ^NAME v))) (.n ^NAME v)
    (instance? NAME v)                             (symbol (name-string v))
    (or (and (string? v) (not= "" v)) (number? v)) (symbol (sstr v))
    :else (throw (ex-info "indirection operand is not name" {:snobol/error 239}))))
(def ^:private bare-fence (FENCE))
(def ^:private pattern-keywords
  {"ABORT" (fn [] ABORT) "ARB" (fn [] SNOBOL4clojure.env/ARB) "BAL" (fn [] BAL) "FAIL" (fn [] FAIL) "FENCE" (fn [] bare-fence)
   "REM" (fn [] SNOBOL4clojure.env/REM) "SUCCEED" (fn [] SNOBOL4clojure.env/SUCCEED)})
(defn- eval-operands
  "Evaluate the operands left to right. The first one that fails (nil) fails the
   whole expression, and the operands after it are not evaluated, so a call among
   them has no effect. Returns the vector of values, or nil."
  [operands]
  (loop [ops (seq operands) vals []]
    (if ops
      (let [v (EVAL! (first ops))]
        (when-not (nil? v) (recur (next ops) (conj vals v))))
      vals)))
(defn- name-value [target]
  (cond (symbol? target)                                                           ($$ target)
        (and (list? target) (equal (first target) '&) (clojure.core/= 2 (count target))) (EVAL! target)
        :else (apply INVOKE (or (:container (meta target)) (first target)) (rest target))))
(defn- deref-result [r]
  (if (instance? SNOBOL4clojure.env.NameResult r) (name-value (:target r)) r))
(defn- call-form?
  "True for a form F(args) that calls a function the program DEFINEd (and not a subscript of a table or an array of that name)."
  [X]
  (and (list? X) (symbol? (first X)) (contains? @<FUNS> (name (first X)))
       (not (:data (meta (get @<FUNS> (name (first X))))))
       (let [v (reference (first X))] (not (or (table? v) (array? v))))))
(defn- name-of-call
  "Call the DEFINEd function of the form F(args) and return the name it returned (a NameResult), or nil."
  [X]
  (when-some [args (eval-operands (rest X))]
    (let [r (apply INVOKE (first X) args)]
      (when (instance? SNOBOL4clojure.env.NameResult r) r))))
(defn- assign-to [target v] (INVOKE '= target v))
(defn- element-target [N]
  (when-some [c (if (seq? (first N)) (EVAL! (first N)) (first N))]
    (when-some [ks (eval-operands (rest N))] (with-meta (apply list c ks) (meta N)))))
(def ^:private value-operators '#{= ?= ? | at-op + - sharp-op / * % ** ^ ! . tilde-op & SEQ})
(declare compile-or-fail evaluated-or-fail)
(defn- returned-value! [] (throw (ex-info "expression evaluated by name returned value" {:snobol/error 249})))
(defn- call-by-name [fsym args]
  (let [fname (name fsym) f (get @<FUNS> fname)]
    (cond
      (and f (:field (meta f)))                          (apply list fsym args)
      (and (nil? f) (clojure.core/= fname "ITEM") (seq args))
      (let [c (first args)] (with-meta (apply list c (rest args)) {:subscript true :container c}))
      (and (nil? f) (clojure.core/= fname "APPLY") (seq args)) (call-by-name (applied-function (first args)) (rest args))
      (and (nil? f) (clojure.core/= fname "EVAL"))
      (let [x (first args)]
        (cond
          (deferred? x)  (if (symbol? (:defer-var (meta x))) (:defer-var (meta x)) (when-not (nil? (evaluated-or-fail (second x))) (returned-value!)))
          (string? x)    (do (when (ends-in-mark? x) (throw (ex-info "eval argument is not expression" {:snobol/error 103})))
                             (let [ir (compile-or-fail x)] (when-not (nil? ir) (evaluated-or-fail #(name-of ir)))))
          :else          (returned-value!)))
      :else
      (let [r (apply INVOKE fsym args)]
        (cond (nil? r)                                      nil
              (instance? SNOBOL4clojure.env.NameResult r) (:target r)
              :else (throw (ex-info "function called by name returned a value" {:snobol/error 21})))))))
(defn- name-of [X]
  (cond
    (symbol? X)                                                          X
    (and (list? X) (equal (first X) '$) (clojure.core/= 2 (count X)))  (when-some [v (EVAL! (second X))] (indirect-target v))
    (and (list? X) (equal (first X) '&) (clojure.core/= 2 (count X)))  X
    (and (list? X) (or (:subscript (meta X)) (not (symbol? (first X))))) (element-target X)
    (and (list? X) (not (contains? value-operators (first X))))
    (let [v (reference (first X))]
      (if (or (table? v) (array? v))
        (element-target X)
        (when-some [args (eval-operands (rest X))] (call-by-name (first X) args))))
    :else (when-not (nil? (EVAL! X)) (returned-value!))))
(defn- capture-target [N]
  (cond
    (symbol? N)
    (fn [x] (assign-to N x))
    (and (list? N) (equal (first N) '$) (clojure.core/= 2 (count N)))
    (when-some [v (EVAL! (second N))]
      (let [t (indirect-target v)] (fn [x] (assign-to t x))))
    (and (list? N) (equal (first N) '&) (clojure.core/= 2 (count N)))
    (fn [x] (assign-to N x))
    (and (list? N) (equal (first N) '*) (clojure.core/= 2 (count N)))
    (let [X (second N)]
      (fn [x] (when-some [t (name-of X)] (assign-to t x))))
    (and (list? N) (symbol? (first N)) (not (:subscript (meta N))) (not (contains? value-operators (first N)))
         (let [v (reference (first N))] (not (or (table? v) (array? v)))))
    (when-some [args (eval-operands (rest N))]
      (when-some [t (call-by-name (first N) args)] (fn [x] (assign-to t x))))
    (list? N)
    (let [t (element-target N)
          c (when (symbol? (first t)) ($$ (first t)))]
      (when-not (and (array? c) (not (SNOBOL4clojure.env/array-in-bounds? c (vec (rest t)))))
        (fn [x] (assign-to t x))))
    true
    N))
(defn error-decided [^clojure.lang.ExceptionInfo e d]
  (let [{n :snobol/error abort :snobol/abort :as data} (ex-data e)]
    (if (or (nil? n) abort (contains? data :snobol/to) (contains? data :snobol/label))
      e
      (let [ee?    (clojure.core/= :ee @SNOBOL4clojure.env/<STAGE>)
            gtcef? (and ee? (< (long SNOBOL4clojure.env/*flprt*) (long @SNOBOL4clojure.env/<GTCEF>)))
            lim    (long @SNOBOL4clojure.env/&ERRLIMIT)]
        (if (and (zero? lim) (not ee?) (not SNOBOL4clojure.env/*matching*)
                 (not (and @SNOBOL4clojure.env/<XC> (pos? (long @SNOBOL4clojure.env/&FNCLEVEL)))))
          e
          (let [h  (when (pos? lim) @SNOBOL4clojure.env/<SETEXIT>)
                k  (swap! SNOBOL4clojure.env/<ERRSERIAL> inc)
                to (if gtcef? @SNOBOL4clojure.env/<GTCEF> d)]
            (when gtcef? (reset! SNOBOL4clojure.env/<STAGE> :xt))
            (when (pos? lim) (swap! SNOBOL4clojure.env/&ERRLIMIT dec))
            (reset! SNOBOL4clojure.env/&ERRTYPE n)
            (fire-keyword! "ERRTYPE" n)
            (reset! SNOBOL4clojure.env/&ERRTEXT (ex-message e))
            (when h (reset! SNOBOL4clojure.env/<SETEXIT> nil))
            (ex-info (ex-message e) (if h (assoc data :snobol/label h :snobol/serial k :snobol/fp d :snobol/resume to) (assoc data :snobol/to to)) e)))))))
(defn- at-error [^clojure.lang.ExceptionInfo e d]
  (let [{h :snobol/label k :snobol/serial fp :snobol/fp to :snobol/resume :as data} (ex-data e)]
    (if (and h (clojure.core/= d fp) (< (long SNOBOL4clojure.env/*handlers*) 100))
      (do (binding [SNOBOL4clojure.env/*handlers* (inc (long SNOBOL4clojure.env/*handlers*))]
            ((var-get (ns-resolve 'SNOBOL4clojure.runtime 'RUN-AT-ERROR)) h k e))
          (ex-info (ex-message e) (-> data (dissoc :snobol/label :snobol/serial :snobol/fp :snobol/resume) (assoc :snobol/to to)) e))
      e)))
(defn- failure-point [f]
  (let [d (inc (long SNOBOL4clojure.env/*fp*))]
    (binding [SNOBOL4clojure.env/*fp* d]
      (try (f)
           (catch clojure.lang.ExceptionInfo e
             (let [e (at-error (error-decided e d) d) to (:snobol/to (ex-data e))]
               (if (and to (>= (long to) d)) nil (throw e))))))))
(defn EVAL! [E]
  (when E
    (cond
      (nil? E)     E
      (char? E)    E
      (float? E)   E
      (string? E)  E
      (integer? E) E
      (symbol? E)  (let [v ($$ E)] (when (pos? (long @SNOBOL4clojure.env/&TRACE)) (fire-access! E v)) (if (identical? v FENCE) bare-fence v))
      (instance? SNOBOL4clojure.env.ExprVar E) (EVAL! (.ir ^SNOBOL4clojure.env.ExprVar E))
      (and (vector? E) (clojure.core/= :comma (first E)))
      (loop [[a & more] (rest E)] (if more (if-some [v (failure-point #(EVAL! a))] v (recur more)) (EVAL! a)))
      (vector? E)  (let [evaled (eval-operands E)]
                    (when evaled
                      (concat-value (apply list 'SEQ evaled))))
      (list? E)
      (let [[op & parms] E]
        (cond
          (equal op 'pattern-constant) (let [v (SNOBOL4clojure.env/primitive-pattern (first parms))] (if (identical? v FENCE) bare-fence v))
          (:subscript (meta E))
          (let [c (if (symbol? op) ($$ op) (EVAL! op))
                c (if (instance? NAME c) ($$ (symbol (str (.n ^NAME c)))) c)]
            (when-some [ks (eval-operands parms)]
              (cond (table? c) (table-get c (SNOBOL4clojure.env/table-subscript ks))
                    (array? c) (array-get c (vec ks))
                    :else (throw (ex-info "subscripted operand is not table or array" {:snobol/error 235})))))
          (or (and (clojure.core/= 1 (count parms))
                   (contains? unused-unary-ops op)
                   (nil? (get @<FUNS> (opsyn-key (unused-unary-ops op) 1))))
              (and (clojure.core/= 2 (count parms))
                   (contains? unused-binary-ops op)
                   (nil? (get @<FUNS> (opsyn-key (unused-binary-ops op) 2)))))
          (throw (ex-info "undefined operator referenced" {:snobol/error 29}))
          (and (equal op '?) (clojure.core/= 1 (count parms)) (nil? (get @<FUNS> (opsyn-key "?" 1))))
          (when-not (nil? (EVAL! (first parms))) ε)
          (and (or (and (clojure.core/= 2 (count parms)) (or (equal op '.) (equal op '$)))
                   (and (clojure.core/= 1 (count parms)) (or (equal op '*) (equal op '?))))
               (get @<FUNS> (opsyn-key (str op) (count parms))))
          (when-some [args (eval-operands parms)] (deref-result (apply (get @<FUNS> (opsyn-key (str op) (count parms))) args)))
          (equal op '.)
          (let [n (count parms)]
            (if (clojure.core/= n 1)
              (let [x (first parms)]
                (cond
                  (call-form? x)
                  (when-some [args (eval-operands (rest x))]
                    (let [r (apply INVOKE (first x) args)]
                      (cond (instance? SNOBOL4clojure.env.NameResult r) (NAME. (:target r))
                            (nil? r)                                    nil
                            :else (throw (ex-info "function called by name returned a value" {:snobol/error 21})))))
                  (and (list? x) (equal (first x) '&) (clojure.core/= 2 (count x)))
                  (NAME. x)
                  (and (list? x) (equal (first x) '$) (clojure.core/= 2 (count x)))
                  (when-some [v (EVAL! (second x))] (NAME. (indirect-target v)))
                  (and (list? x) (equal (first x) '*) (clojure.core/= 2 (count x)))
                  (let [E (second x)] (NAME. (if (symbol? E) E (SNOBOL4clojure.env/->ExprVar E))))
                  (and (list? x) (>= (count x) 2))
                  (when-some [ks (eval-operands (rest x))]
                    (let [v (when (symbol? (first x)) (reference (first x)))]
                      (if (or (table? v) (array? v) (:subscript (meta x)) (not (symbol? (first x))))
                        (NAME. (cond-> (apply list (first x) ks)
                                 (or (table? v) (array? v)) (with-meta {:container v})))
                        (when-some [t (call-by-name (first x) ks)] (NAME. t)))))
                  :else (NAME. x)))
              (let [[P N] parms] (when-some [p (EVAL! P)] (when-some [t (capture-target N)] (INVOKE '. p t))))))
          (and (equal op 'at-op) (clojure.core/= 1 (count parms)))
          (when-some [t (capture-target (first parms))] (list 'CURSOR-IMM! t))
          (equal op '$)     (if (clojure.core/= 1 (count parms))
                              (when-some [v (EVAL! (first parms))]
                                (EVAL! (indirect-target v)))
                              (let [[P N] parms] (when-some [p (EVAL! P)] (when-some [t (capture-target N)] (INVOKE '$ p t)))))
          (and (equal op '=) (list? (first parms)) (clojure.core/= 2 (count (first parms)))
               (equal (first (first parms)) '$))
          (let [[[_ X] R] parms]
            (when-some [v (EVAL! X)]
              (INVOKE '= (indirect-target v) (EVAL! R))))
          (and (equal op '=) (list? (first parms)) (equal (first (first parms)) 'ITEM) (clojure.core/<= 3 (count (first parms))))
          (let [[_ container-expr & key-exprs] (first parms)]
            (when-some [c (EVAL! container-expr)]
              (when-some [ks (eval-operands key-exprs)]
                (when-some [v (EVAL! (second parms))] (INVOKE '= (apply list c ks) v)))))
          (and (equal op '=) (list? (first parms)) (equal (first (first parms)) 'APPLY) (clojure.core/<= 2 (count (first parms))))
          (when-some [[f & as] (eval-operands (rest (first parms)))]
            (let [fsym (symbol (SNOBOL4clojure.env/name-string f))
                  df   (get @<FUNS> (name fsym))]
              (if (and df (not (:data (meta df))))
                (let [r (apply INVOKE fsym as)]
                  (when (instance? SNOBOL4clojure.env.NameResult r) (when-some [v (EVAL! (second parms))] (INVOKE '= (:target r) v))))
                (when-some [v (EVAL! (second parms))] (INVOKE '= (apply list fsym as) v)))))
          (and (equal op '=) (list? (first parms))
               (or (and (clojure.core/= 2 (count (first parms))) (contains? unused-unary-ops (first (first parms)))
                        (get @<FUNS> (opsyn-key (unused-unary-ops (first (first parms))) 1)))
                   (and (clojure.core/= 3 (count (first parms))) (contains? unused-binary-ops (first (first parms)))
                        (get @<FUNS> (opsyn-key (unused-binary-ops (first (first parms))) 2)))))
          (let [N  (first parms)
                f  (if (clojure.core/= 2 (count N)) (get @<FUNS> (opsyn-key (unused-unary-ops (first N)) 1)) (get @<FUNS> (opsyn-key (unused-binary-ops (first N)) 2)))]
            (when-some [args (eval-operands (rest N))]
              (let [r (apply f args)]
                (when (instance? SNOBOL4clojure.env.NameResult r) (when-some [v (EVAL! (second parms))] (INVOKE '= (:target r) v))))))
          (and (equal op '=) (call-form? (first parms)))
          (when-some [r (name-of-call (first parms))]
            (when-some [v (EVAL! (second parms))] (INVOKE '= (:target r) v)))
          (equal op '=)     (let [[N R]   parms
                                  N' (if (and (list? N) (>= (count N) 2)
                                              (not (clojure.core/= (first N) '&)))
                                       (let [container-sym (first N)
                                             container-val (if (list? container-sym) (EVAL! container-sym) ($$ container-sym))]
                                         (cond
                                           (nil? container-val) nil
                                           :else (when-some [ks (eval-operands (rest N))]
                                                   (with-meta (apply list (if (list? container-sym) container-val container-sym) ks) (meta N)))))
                                       N)]
                              (let [c      (when (list? N') (if (symbol? (first N')) ($$ (first N')) (first N')))
                                    field? (and (list? N') (symbol? (first N')) (not (:subscript (meta N'))) (:data (meta (get @<FUNS> (name (first N'))))))]
                                (when (and N' (not (and (array? c) (not field?) (not (SNOBOL4clojure.env/array-in-bounds? c (vec (rest N')))))))
                                  (let [r (EVAL! R)]
                                    (when (and (some? r) (symbol? N') (SNOBOL4clojure.env/protected-variables N')) (SNOBOL4clojure.env/protected-store! N' r))
                                    (INVOKE '= N' r)))))
          (equal op '?=)    (let [[N P R] parms
                                  N' (cond
                                       (and (list? N) (equal (first N) '$) (clojure.core/= 2 (count N)))
                                       (when-some [v (EVAL! (second N))] (indirect-target v))
                                       (and (list? N) (not (equal (first N) '&)))
                                       (when-some [ks (eval-operands (rest N))] (with-meta (apply list (first N) ks) (meta N)))
                                       :else N)]
                              (when N' (when-some [p (EVAL! P)] (INVOKE '?= N' p R))))
          (and (equal op '&) (clojure.core/= 1 (count parms)) (pattern-keywords (str (first parms))))
          ((pattern-keywords (str (first parms))))
          (and (equal op '&) (clojure.core/= 1 (count parms)))
                            (let [[N]     parms
                                  kw-sym  (symbol (str "&" N))
                                  vr      (ns-resolve (find-ns 'SNOBOL4clojure.env) kw-sym)
                                  v       (if vr (var-get vr) (throw (ex-info "keyword operand is not name of defined keyword" {:snobol/error 251})))]
                              (if (instance? clojure.lang.IDeref v) @v v))
          (and (equal op 'SEQ) (not (program-fn op)))   (apply str (map #(sstr (EVAL! %)) parms))
          (and (equal op '*) (clojure.core/= (count parms) 1))
          (let [sym (first parms)]
            (with-meta (list 'DEFER! (fn [] (EVAL! sym))) {:defer-var sym}))
          (list? op) (when-some [container (EVAL! op)] (when-some [args (eval-operands parms)] (apply INVOKE container args)))
          true (let [
                     args     (if (and (equal op 'tilde-op) (clojure.core/= 1 (count parms)))
                                [(failure-point #(EVAL! (first parms)))]
                                (eval-operands parms))
                     opsyn-fn (get @<FUNS> (opsyn-key (get op-spelling op (str op)) (count parms)))]
                 (when args
                   (deref-result
                     (if opsyn-fn
                       (apply opsyn-fn args)
                       (apply INVOKE op args)))))))
      true E)))
(def ^:private ^:dynamic *raise-compile-errors* false)
(defn- compiled! [ok?] (reset! SNOBOL4clojure.env/<XC> (and (not ok?) (pos? (long @SNOBOL4clojure.env/&FNCLEVEL)))))
(defn- compile-failed! [n text]
  (compiled! false)
  (when *raise-compile-errors* (throw (ex-info text {:snobol/error n})))
  (reset! SNOBOL4clojure.env/&ERRTYPE n)
  (fire-keyword! "ERRTYPE" n)
  (reset! SNOBOL4clojure.env/&ERRTEXT text)
  (when (pos? (long @SNOBOL4clojure.env/&ERRLIMIT)) (swap! SNOBOL4clojure.env/&ERRLIMIT dec))
  nil)
(defn- compile-expression
  ([s] (compile-expression s nil))
  ([^String s early]
   (let [p (parse-expression s)]
     (cond
       (re-matches #"[ \t]*" s) (do (compiled! true) "")
       (instance? instaparse.gll.Failure p)
       (let [{n :error i :end} (expression-end s)
             n (cond n n (< (long i) (count s)) early :else 220)]
         (when n (compile-failed! n (SNOBOL4clojure.env/error-message n))))
       :else
       (let [ir (top-replacement (first (emitter p)))
             n  ((requiring-resolve 'SNOBOL4clojure.compiler/fold-error) ir)]
         (if n (compile-failed! n (SNOBOL4clojure.env/error-message n)) (do (compiled! true) ((requiring-resolve 'SNOBOL4clojure.compiler/freeze) ir))))))))
(defn- compile-or-fail [^String s]
  (try (compile-expression s 103)
       (catch clojure.lang.ExceptionInfo e
         (let [{n :snobol/error c :snobol/compile} (ex-data e)]
           (when-not c (throw e))
           (compile-failed! n (ex-message e))))))
(defn- deferred? [x] (and (seq? x) (clojure.core/= 'DEFER! (first x)) (fn? (second x))))
(defn- expression-of [ir] (list 'DEFER! (fn [] (EVAL! ir))))
(defn- evaluated-or-fail [f]
  (let [d0 (long SNOBOL4clojure.env/*fp*)]
    (when (clojure.core/= :xt @SNOBOL4clojure.env/<STAGE>) (reset! SNOBOL4clojure.env/<STAGE> :ee))
    (reset! SNOBOL4clojure.env/<GTCEF> d0)
    (let [v (binding [SNOBOL4clojure.env/*fp* (inc d0)]
              (try (f)
                   (catch clojure.lang.ExceptionInfo e
                     (let [e (at-error (error-decided e (inc d0)) (inc d0)) to (:snobol/to (ex-data e))]
                       (if (and to (>= (long to) d0)) nil (throw e))))))]
      (reset! SNOBOL4clojure.env/<STAGE> :xt)
      v)))
(defn EVAL [X]
  (cond
    (and (deferred? X) (not (:pattern (meta X)))) (evaluated-or-fail (second X))
    (string? X)   (if (ends-in-mark? X)
                    (throw (ex-info "eval argument is not expression" {:snobol/error 103}))
                    (evaluated-or-fail #(when-some [ir (binding [*raise-compile-errors* true] (compile-or-fail X))]
                                          (binding [SNOBOL4clojure.env/*eval-block* @SNOBOL4clojure.env/<CODELINE>] (EVAL! ir)))))
    (number? X)   X
    (and (instance? NAME X) (symbol? (.n ^NAME X))) (EVAL (name (.n ^NAME X)))
    (or (textless? X) (and (list? X) (clojure.core/= "PATTERN" (SNOBOL4clojure.env/DATATYPE X))))
    (throw (ex-info "eval argument is not expression" {:snobol/error 103}))
    true          (EVAL! X)))
