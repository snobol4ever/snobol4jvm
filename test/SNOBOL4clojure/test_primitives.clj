(ns SNOBOL4clojure.test-primitives
  ;; Tests for the low-level scanners in primitives.clj.
  ;; Each scanner takes [Σ Δ Π] and returns [σ δ] on success
  ;; or [σ (- -1 δ)] (negative δ) on failure.
  (:require [clojure.test :refer :all]
            [SNOBOL4clojure.env :refer [ANY$ BREAK$ FAIL! LEN# LIT$ NOTANY$ POS# REM! RPOS# RTAB# SPAN$ SUCCEED! TAB# charset]]))

;; ── Helpers ───────────────────────────────────────────────────────────────────
(defn ok?   [[_ δ]] (>= δ 0))
(defn fail? [[_ δ]] (< δ 0))
(defn pos   [[_ δ]] δ)        ; ending position
(defn rem   [[σ _]] (apply str σ)) ; remaining chars as string

;; ── LIT$ ─────────────────────────────────────────────────────────────────────
(deftest test-lit$
  (is (ok?   (LIT$ (seq "hello") 0 (seq "hel"))))
  (is (= 3   (pos  (LIT$ (seq "hello") 0 (seq "hel")))))
  (is (= "lo"(rem  (LIT$ (seq "hello") 0 (seq "hel")))))
  (is (fail? (LIT$ (seq "hello") 0 (seq "xyz"))))
  (is (ok?   (LIT$ (seq "hello") 0 (seq ""))))   ; empty literal always matches
  (is (fail? (LIT$ (seq "hi")    0 (seq "hilo")))); subject shorter than pattern
  (is (ok?   (LIT$ (seq "abc")   0 (seq "abc")))); full match
  (is (= 3   (pos  (LIT$ (seq "abc") 0 (seq "abc"))))))

;; ── ANY$ ─────────────────────────────────────────────────────────────────────
(deftest test-any$
  (let [vowels (charset "aeiou")]
    (is (ok?   (ANY$ (seq "apple") 0 vowels)))
    (is (= 1   (pos  (ANY$ (seq "apple") 0 vowels))))
    (is (fail? (ANY$ (seq "xyz")   0 vowels)))
    (is (fail? (ANY$ (seq "")      0 vowels))) ; empty subject
    (is (ok?   (ANY$ (seq "egg")   0 vowels)))))

;; ── NOTANY$ ──────────────────────────────────────────────────────────────────
(deftest test-notany$
  (let [vowels (charset "aeiou")]
    (is (fail? (NOTANY$ (seq "apple") 0 vowels)))
    (is (ok?   (NOTANY$ (seq "xyz")   0 vowels)))
    (is (= 1   (pos     (NOTANY$ (seq "xyz") 0 vowels))))
    (is (fail? (NOTANY$ (seq "")      0 vowels)))))

;; ── SPAN$ ────────────────────────────────────────────────────────────────────
(deftest test-span$
  (let [digits (charset "0123456789")]
    (is (ok?   (SPAN$ (seq "123abc") 0 digits)))
    (is (= 3   (pos   (SPAN$ (seq "123abc") 0 digits))))
    (is (= "abc" (rem (SPAN$ (seq "123abc") 0 digits))))
    (is (fail? (SPAN$ (seq "abc")    0 digits))) ; must match at least one
    (is (ok?   (SPAN$ (seq "999")    0 digits))) ; entire subject
    (is (= 3   (pos   (SPAN$ (seq "999") 0 digits))))))

;; ── BREAK$ ───────────────────────────────────────────────────────────────────
;; BREAK$ scans until it hits a char in the stop set.
;; Returns the position BEFORE the stop char.
;; If the first char IS a stop char: returns pos=0 (zero-length match — ok).
;; If no stop char is found: returns failure.
(deftest test-break$
  (let [stops (charset ".!?")]
    (is (ok?   (BREAK$ (seq "hello.world") 0 stops)))
    (is (= 5   (pos    (BREAK$ (seq "hello.world") 0 stops))))
    ;; First char is the stop — zero-length match at position 0 (ok, not fail)
    (is (ok?   (BREAK$ (seq "!stop")       0 stops)))
    (is (= 0   (pos    (BREAK$ (seq "!stop") 0 stops))))
    (is (fail? (BREAK$ (seq "")            0 stops))) ; empty subject — fail
    ;; No stop found — failure (BREAK requires a stop char in subject)
    (is (fail? (BREAK$ (seq "abc")         0 stops)))))

;; ── POS# / RPOS# ─────────────────────────────────────────────────────────────
(deftest test-pos-rpos
  (is (ok?   (POS#  (seq "hello") 0 0)))
  (is (fail? (POS#  (seq "hello") 1 0)))
  (is (ok?   (POS#  (seq "hello") 3 3)))
  (is (ok?   (RPOS# (seq "lo")    3 2)))  ; 2 chars remain
  (is (fail? (RPOS# (seq "lo")    3 0)))) ; 0 remain wanted, but 2 left

;; ── LEN# ─────────────────────────────────────────────────────────────────────
(deftest test-len
  (is (ok?   (LEN# (seq "hello") 0 3)))
  (is (= 3   (pos  (LEN# (seq "hello") 0 3))))
  (is (fail? (LEN# (seq "hi")    0 5)))) ; subject too short

;; ── TAB# / RTAB# ─────────────────────────────────────────────────────────────
(deftest test-tab-rtab
  (is (ok?   (TAB#  (seq "hello") 0 3)))
  (is (= 3   (pos   (TAB#  (seq "hello") 0 3))))
  (is (ok?   (TAB#  (seq "hello") 3 3)))  ; already past target
  (is (ok?   (RTAB# (seq "lo")    3 2)))  ; 2 chars remain, want >= 2
  (is (fail? (RTAB# (seq "l")     4 2)))) ; only 1 char, want >= 2

;; ── REM! ─────────────────────────────────────────────────────────────────────
(deftest test-rem
  (is (ok?   (REM! (seq "abc") 0 nil)))
  (is (= 3   (pos  (REM! (seq "abc") 0 nil))))
  (is (ok?   (REM! (seq "")   0 nil)))
  (is (= 0   (pos  (REM! (seq "") 0 nil)))))

;; ── SUCCEED! / FAIL! ─────────────────────────────────────────────────────────
(deftest test-succeed-fail
  (is (ok?   (SUCCEED! (seq "x") 1 nil)))
  (is (= 1   (pos      (SUCCEED! (seq "x") 1 nil))))
  (is (fail? (FAIL!    (seq "x") 1 nil))))

;; ── charset helper ────────────────────────────────────────────────────────────
(deftest test-charset
  (is (= #{\a \e \i \o \u} (charset "aeiou")))
  (is (= #{}                (charset "")))
  (is (= #{\x}              (charset "xxx"))))  ; deduplicated
