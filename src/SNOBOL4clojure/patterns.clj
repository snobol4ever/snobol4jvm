(ns SNOBOL4clojure.patterns
  ;; Pattern constructors: build pattern representation (lists/symbols)
  ;; that the MATCH engine knows how to dispatch on.
  (:require [SNOBOL4clojure.primitives :refer [charset]]
            [SNOBOL4clojure.env        :as env]
            [SNOBOL4clojure.functions  :as fns])
  (:refer-clojure :exclude [= + - * / num]))

;; ── Character-class patterns ──────────────────────────────────────────────────
;; If the argument is a DEFER! thunk (produced by unary * on a variable),
;; we must NOT call charset now — the variable may change between uses.
;; Instead wrap the whole constructor in a DEFER so the engine re-evaluates
;; the pattern (and re-builds the charset) at match time from the live value.
(defn- deferred? [S] (and (list? S) (clojure.core/= (first S) 'DEFER!)))

(defn ANY    [S] (if (deferred? S) (list 'DEFER! (fn [] (ANY    (str ((second S))))))  (list 'ANY$    (charset S))))
(defn BREAK  [S] (if (deferred? S) (list 'DEFER! (fn [] (BREAK  (str ((second S))))))  (list 'BREAK$  (charset S))))
(defn BREAKX [S] (if (deferred? S) (list 'DEFER! (fn [] (BREAKX (str ((second S))))))  (list 'BREAKX# (charset S))))  ; backtracking form
(defn NOTANY [S] (if (deferred? S) (list 'DEFER! (fn [] (NOTANY (str ((second S))))))  (list 'NOTANY$ (charset S))))
(defn NSPAN  [S] (if (deferred? S) (list 'DEFER! (fn [] (NSPAN  (str ((second S))))))  (list 'NSPAN$  (charset S))))   ; 0-or-more span
(defn SPAN   [S] (if (deferred? S) (list 'DEFER! (fn [] (SPAN   (str ((second S))))))  (list 'SPAN$   (charset S))))

;; ── Length / position patterns ────────────────────────────────────────────────
;; A deferred argument (*N) is read when the match reaches the pattern, as for
;; the charset patterns above.
;; "These functions return a pattern based on their integer argument" (manual ch. 4, Integer pattern functions); for LEN "I must be an integer greater than or equal
;; to zero". The argument converts as any integer argument (a numeric string, a real truncated); one that does not is "120 LEN argument is not integer or expression",
;; one below 0 or above 16777216 (SPITBOL's longest string) "121 LEN argument is negative or too large" (sbl.min patin, gtsmi). A deferred argument is checked when
;; the match reaches it: "47 LEN evaluated argument is not integer", "48 LEN evaluated argument is negative or too large" (Appendix D). POS, RPOS, TAB and RTAB the
;; same, each with its own numbers.
(defn- int-pattern [op I bad big]
  (let [n (fns/integer-of I)]
    (cond
      (nil? n)                                  (throw (ex-info (env/error-message bad) {:snobol/error bad}))
      (or (neg? n) (clojure.core/> n 16777216)) (throw (ex-info (env/error-message big) {:snobol/error big}))
      :else                                     (list op n))))

(defn- int-pattern-of [op I [bad big ebad ebig]]
  (if (deferred? I) (list 'DEFER! (fn [] (int-pattern op ((second I)) ebad ebig))) (int-pattern op I bad big)))

(defn LEN  [I] (int-pattern-of 'LEN#  I [120 121 47 48]))
(defn POS  [I] (int-pattern-of 'POS#  I [162 163 50 51]))
(defn RPOS [I] (int-pattern-of 'RPOS# I [185 186 52 53]))
(defn RTAB [I] (int-pattern-of 'RTAB# I [181 182 54 55]))
(defn TAB  [I] (int-pattern-of 'TAB#  I [183 184 57 58]))

;; ── Zero-width anchor patterns ────────────────────────────────────────────────
(def BOL (list 'BOL#))    ; beginning of subject (position 0)
(def EOL (list 'EOL#))    ; end of subject (no chars remaining)

;; ── Repetition / structural patterns ─────────────────────────────────────────
(defn ARBNO [P]  (list 'ARBNO! P))
(defn FENCE
  ([]  (list 'FENCE!))
  ([P] (list 'FENCE! P)))

;; ── Cursor assignment ─────────────────────────────────────────────────────────
(defn CURSOR [N] (list 'CURSOR-IMM! N))  ; @N — assign cursor position to N

;; ── Conjunction ───────────────────────────────────────────────────────────────
(defn CONJ [P Q] (list 'CONJ! P Q))     ; P & Q — both must match same span

;; ── Deferred pattern ──────────────────────────────────────────────────────────
(defn DEFER [thunk] (list 'DEFER! thunk)) ; *expr — evaluate thunk at match time

;; ── Constant pattern values ───────────────────────────────────────────────────
(def ARB     (list 'ARB!))
(def BAL     (list 'BAL!))
(def REM     (list 'REM!))
(def ABORT   (list 'ABORT!))
(def FAIL    (list 'FAIL!))
(def SUCCEED (list 'SUCCEED!))
