(ns SNOBOL4clojure.test-engine
  "The matcher, the pattern constructors and scanners, the operators and the runtime state, tested from Clojure."
  (:refer-clojure :exclude [num])
  (:require [clojure.test :refer :all]
            [SNOBOL4clojure.operators :refer [SEARCH MATCH FULLMATCH MATCH-REPLACE INVOKE | . COLLECT! tilde]]
            [SNOBOL4clojure.env :refer [ANY SPAN POS RPOS LEN ABORT ARB ARBNO BAL BREAK FAIL FENCE NOTANY REM RTAB SUCCEED TAB ANY$ BREAK$ FAIL! LEN# LIT$ NOTANY$ POS# REM! RPOS# RTAB# SPAN$
                                        SUCCEED! TAB# charset GLOBALS ORD CHAR REMDR INTEGER REAL CONVERT ε NSPAN BREAKX BOL EOL snobol-set! $$ CURSOR DEFER &LCASE &UCASE &DIGITS ARRAY
                                        DATATYPE TABLE empty-set divide equal multiply not-equal num subtract Σ+ η]]
            [SNOBOL4clojure.operators :as m :refer [SEARCH FULLMATCH]]
            [SNOBOL4clojure.operators :as ops :refer :all :exclude [= + - * / num]])
  (:import [SNOBOL4clojure.env NAME]))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_match
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── SEARCH — slides pattern across subject ────────────────────────────────────
(deftest test-search-basic
  (is      (SEARCH "hello world" "world"))
  (is      (SEARCH "hello"       "hello"))
  (is (not (SEARCH "hello"       "xyz")))
  (is      (SEARCH "abcdef"      "cde")))  ; interior match

(deftest test-search-returns-span
  ;; SEARCH returns [start end] — a half-open span
  (is (= [6 11] (SEARCH "hello world" "world")))
  (is (= [0  5] (SEARCH "hello world" "hello")))
  (is (= [2  5] (SEARCH "abcde"       "cde"))))

(deftest test-search-empty-pattern
  ;; Empty string matches at position 0
  (is (= [0 0] (SEARCH "hello" ""))))

;; ── MATCH — anchored at position 0 ───────────────────────────────────────────
(deftest test-match-anchored
  (is      (MATCH "hello world" "hello"))   ; prefix match
  (is (not (MATCH "hello world" "world")))  ; not at position 0
  (is      (MATCH "abc" "")))               ; empty always matches at 0

(deftest test-match-returns-span
  (is (= [0 5] (MATCH "hello world" "hello")))
  (is (= [0 0] (MATCH "hello" ""))))

;; ── FULLMATCH — anchored at both ends ────────────────────────────────────────
(deftest test-fullmatch
  (is      (FULLMATCH "hello" "hello"))
  (is (not (FULLMATCH "hello world" "hello")))  ; trailing chars
  (is (not (FULLMATCH "hello" "hell")))          ; trailing chars
  (is      (FULLMATCH "" "")))

(deftest test-fullmatch-returns-span
  (is (= [0 5] (FULLMATCH "hello" "hello")))
  (is (= [0 0] (FULLMATCH "" ""))))

(deftest test-fullmatch-with-pattern
  (let [digits (SPAN "0123456789")]
    (is      (FULLMATCH "12345" digits))
    (is (not (FULLMATCH "123ab" digits)))))

;; ── REPLACE — match and substitute ───────────────────────────────────────────
(deftest test-replace-basic
  (is (= "hello Clojure" (MATCH-REPLACE "hello world"  "world"   "Clojure")))
  (is (= "Xello"         (MATCH-REPLACE "hello"        "h"       "X")))
  (is (= "hello world"   (MATCH-REPLACE "hello world"  "hello"   "hello"))))

(deftest test-replace-no-match
  (is (nil? (MATCH-REPLACE "hello" "xyz" "!"))))

(deftest test-replace-empty
  ;; Replacing first empty match inserts at position 0
  (is (= "Xhello" (MATCH-REPLACE "hello" "" "X"))))

;; ── Literal patterns (LIT$ dispatch) ─────────────────────────────────────────
(deftest test-literal
  (is (SEARCH "hello"       "hel"))
  (is (SEARCH "hello world" "hello"))
  (is (not (SEARCH "world" "hello")))
  (is (SEARCH "abc"   "")))

;; ── SEQ (vector → sequence) ───────────────────────────────────────────────────
(deftest test-seq
  (is      (SEARCH "hello"  (list 'SEQ "hel" "lo")))
  (is (not (SEARCH "hello"  (list 'SEQ "hel" "xx"))))
  (is      (SEARCH "abcdef" (list 'SEQ "abc" "def"))))

;; ── ALT (alternation) ─────────────────────────────────────────────────────────
(deftest test-alt
  (let [P (list 'ALT "cat" "dog" "bird")]
    (is      (SEARCH "cat"  P))
    (is      (SEARCH "dog"  P))
    (is      (SEARCH "bird" P))
    (is (not (SEARCH "fish" P)))))

;; ── Anchoring with POS / RPOS ─────────────────────────────────────────────────
(deftest test-fullmatch-anchoring
  (let [P (list 'SEQ (POS 0) "hello" (RPOS 0))]
    (is      (SEARCH "hello"    P))
    (is (not (SEARCH "hello!"   P)))
    (is (not (SEARCH "  hello"  P)))))

(deftest test-search-finds-interior
  ;; SEARCH slides — finds "hello" even with leading/trailing chars
  (is (= [4 9] (SEARCH "say hello there" "hello"))))

;; ── Nested ALT inside SEQ ─────────────────────────────────────────────────────
(deftest test-nested-alt-seq
  (let [P (list 'SEQ
             (POS 0)
             (list 'ALT "B" "F" "R")
             (list 'ALT "E" "EA")
             (list 'ALT "D" "DS")
             (RPOS 0))]
    (is (SEARCH "BED"   P))
    (is (SEARCH "BEDS"  P))
    (is (SEARCH "BEAD"  P))
    (is (SEARCH "BEADS" P))
    (is (SEARCH "RED"   P))
    (is (SEARCH "READS" P))
    (is (not (SEARCH "LEADER" P)))
    (is (not (SEARCH "ZED"    P)))))

;; ── Backtracking ──────────────────────────────────────────────────────────────
;; TODO: cross-SEQ backtracking not yet implemented.
(deftest test-backtracking
  (is (SEARCH "BAD" (list 'SEQ (list 'ALT "BE" "B") "AD" (RPOS 0))))
  #_(is (not (SEARCH "BEAD" (list 'SEQ (list 'ALT "BE" "B") "AD" (RPOS 0))))))

;; ── ANY / SPAN patterns ───────────────────────────────────────────────────────
(deftest test-primitive-dispatch
  (let [vowels (ANY "aeiou")
        digits (SPAN "0123456789")]
    (is (SEARCH "apple" vowels))
    (is (SEARCH "123"   digits))
    (is (not (FULLMATCH "xyz" vowels)))
    (is      (FULLMATCH "999" digits))))

;; ── LEN ──────────────────────────────────────────────────────────────────────
(deftest test-len-match
  (let [L3 (LEN 3)]
    (is      (SEARCH "abc"    L3))
    (is      (SEARCH "abcdef" L3))
    (is (not (FULLMATCH "ab"  L3)))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_patterns
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── Constructor representation ────────────────────────────────────────────────
(deftest test-constructors-produce-lists
  (is (= '(ANY$    #{\a \e \i \o \u}) (ANY    "aeiou")))
  (is (= '(SPAN$   #{\0 \1 \2})       (SPAN   "012")))
  (is (= '(BREAK$  #{\. \,})          (BREAK  ".,")))
  (is (= '(NOTANY$ #{\x})             (NOTANY "x")))
  (is (= '(LEN#    5)                 (LEN    5)))
  (is (= '(POS#    0)                 (POS    0)))
  (is (= '(RPOS#   0)                 (RPOS   0)))
  (is (= '(TAB#    4)                 (TAB    4)))
  (is (= '(RTAB#   2)                 (RTAB   2)))
  (is (= '(ARBNO!  "x")              (ARBNO  "x")))
  (is (= '(FENCE!)                   (FENCE)))
  (is (= '(FENCE!  "x")             (FENCE  "x"))))

;; ── Constant pattern values ───────────────────────────────────────────────────
(deftest test-constant-patterns
  (is (= '(ARB!)     ARB))
  (is (= '(BAL!)     BAL))
  (is (= '(REM!)     REM))
  (is (= '(ABORT!)   ABORT))
  (is (= '(FAIL!)    FAIL))
  (is (= '(SUCCEED!) SUCCEED)))

;; ── ANY in MATCH ──────────────────────────────────────────────────────────────
(deftest test-any-match
  (let [A (ANY "BFLR")]
    (is      (SEARCH "BED"  A))
    (is      (SEARCH "FAD"  A))
    (is (not (FULLMATCH "XYZ" A)))))

;; ── SPAN in MATCH ────────────────────────────────────────────────────────────
(deftest test-span-match
  (let [S (SPAN "0123456789")]
    (is      (SEARCH "123abc" S))
    (is (not (FULLMATCH "abc" S)))))

;; ── BREAK in MATCH ───────────────────────────────────────────────────────────
(deftest test-break-match
  (let [B (BREAK ".!?")]
    (is (SEARCH "hello.world" B))
    ;; BREAK stops before the delimiter
    (is (SEARCH "hello.world" (list 'SEQ B ".")))))

;; ── POS / RPOS anchoring ──────────────────────────────────────────────────────
(deftest test-pos-rpos-match
  (let [P (list 'SEQ (POS 0) "abc" (RPOS 0))]
    (is      (FULLMATCH "abc"  P))
    (is (not (FULLMATCH "xabc" P)))
    (is (not (FULLMATCH "abcx" P)))))

;; ── LEN ──────────────────────────────────────────────────────────────────────
(deftest test-len-search
  (is      (SEARCH "abcde" (LEN 3)))
  (is (not (FULLMATCH "ab" (LEN 3)))))

;; ── SUCCEED / FAIL ────────────────────────────────────────────────────────────
(deftest test-succeed-fail-patterns
  (is      (SEARCH "anything" SUCCEED))
  (is (not (SEARCH "anything" FAIL))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_primitives
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── Helpers ───────────────────────────────────────────────────────────────────
(defn ok?   [[_ δ]] (>= δ 0))
(defn fail? [[_ δ]] (< δ 0))
(defn pos   [[_ δ]] δ)        ; ending position
(defn remaining [[σ _]] (apply str σ)) ; remaining chars as string

;; ── LIT$ ─────────────────────────────────────────────────────────────────────
(deftest test-lit$
  (is (ok?   (LIT$ (seq "hello") 0 (seq "hel"))))
  (is (= 3   (pos  (LIT$ (seq "hello") 0 (seq "hel")))))
  (is (= "lo"(remaining (LIT$ (seq "hello") 0 (seq "hel")))))
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
    (is (= "abc" (remaining (SPAN$ (seq "123abc") 0 digits))))
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


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_arb
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

(GLOBALS *ns*)

;; ── ARB ──────────────────────────────────────────────────────────────────────

(deftest arb-basic
  ;; ARB matches any string, shortest first
  (let [arb ARB]
    ;; SEARCH finds first (shortest) match at pos 0
    (is (= [0 0] (SEARCH "abc" arb)))
    ;; FULLMATCH with ARB alone matches entire string
    (is (= [0 3] (FULLMATCH "abc" arb)))
    (is (= [0 0] (FULLMATCH "" arb)))))

(deftest arb-anchored
  ;; POS(0) ARB RPOS(0) = fullmatch any string
  (let [p (list 'SEQ (POS 0) ARB (RPOS 0))]
    (is (= [0 3] (SEARCH "abc" p)))
    (is (= [0 0] (SEARCH "" p)))))

(deftest arb-in-seq-backtrack
  ;; SEQ[ARB, "b"] — ARB starts at 0 chars, "b" fails; ARB grows to 1 char ("a"),
  ;; "b" matches -> [0,2] in "abc"
  (let [p (list 'SEQ ARB "b")]
    (is (= [0 2] (SEARCH "abc" p)))
    (is (= [0 2] (SEARCH "ab"  p)))))

(deftest arb-sandwich
  ;; "x" ARB "y" in "xhelloy" -> ARB matches "hello"
  (let [p (list 'SEQ "x" ARB "y")]
    (is (= [0 7] (SEARCH "xhelloy" p)))
    ;; shortest match: "xy" -> ARB=""
    (is (= [0 2] (SEARCH "xy" p)))))

;; ── ARBNO ────────────────────────────────────────────────────────────────────

(deftest arbno-basic
  ;; ARBNO("a") matches zero or more "a"s, shortest first
  (let [p (ARBNO "a")]
    (is (= [0 0] (SEARCH "aaa" p)))   ; shortest = 0 at pos 0
    (is (= [0 0] (FULLMATCH "" p))))) ; 0 reps of "a" = empty string

(deftest arbno-anchored
  ;; POS(0) ARBNO("a") RPOS(0) matches "aaa" entirely
  (let [p (list 'SEQ (POS 0) (ARBNO "a") (RPOS 0))]
    (is (= [0 3] (SEARCH "aaa" p)))
    (is (= [0 0] (SEARCH "" p)))))

(deftest arbno-with-span
  ;; ARBNO(SPAN(digits) ".") matches "1.22.333."
  (let [digits "0123456789"
        p      (list 'SEQ (POS 0) (ARBNO (list 'SEQ (SPAN digits) ".")) (RPOS 0))]
    (is (= [0 9] (SEARCH "1.22.333." p)))
    (is (= [0 0] (SEARCH "" p)))))

(deftest arbno-backtrack
  ;; SEQ[ARBNO("a"), "b"] on "aaab":
  ;; ARBNO first yields "" (0 reps), "b" fails; then "a" (1 rep), fails; then "aa" (2 reps), fails;
  ;; then "aaa" (3 reps), "b" matches -> [0,4]
  (let [p (list 'SEQ (ARBNO "a") "b")]
    (is (= [0 4] (SEARCH "aaab" p)))
    (is (= [0 1] (SEARCH "b"    p)))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_sprint8
;;
;; Sprint 8 — ABORT, bare FENCE(), REM, ORD, CHAR, REMDR, INTEGER, REAL, CONVERT to STRING (ASCII and STRING, from no SNOBOL4, are gone: CEO-2028).
;; Test cases derived from Snobol4.Net reference suite (jcooper0/Snobol4.Net).
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; Helper: extract matched substring from a SEARCH result
(defn- matched [s result]
  (when result (subs s (first result) (second result))))

;; Helper: build a SEQ pattern node
(defn- SEQ [& parts] (apply list 'SEQ parts))

;; ─── 8.1  ABORT ──────────────────────────────────────────────────────────────
;; Ref: Snobol4.Net Pattern/Abort.cs TEST_Abort_001 / _002
;;
;; TEST_001: subject '-ab-1-'; pattern ANY('ab') | '1' ABORT
;;   Unanchored scan: position 1 hits 'a' via ANY → success before ABORT branch
;;
;; TEST_002: subject '-1a-b-'; same pattern
;;   Anchored at 0: '-' is not ANY('ab') and '-1' is not '1' at pos 0 exactly.
;;   We use FULLMATCH with a pattern that forces the abort branch to fire.

(deftest abort-001-any-matches-before-abort
  (testing "ANY('ab') succeeds; ABORT branch never reached"
    (let [pat (| (INVOKE 'ANY "ab") (SEQ "1" ABORT))]
      (is (some? (SEARCH "-ab-1-" pat))))))

(deftest abort-002-abort-fires-when-its-branch-is-taken
  (testing "Pattern that forces ABORT branch: entire match returns nil"
    ;; FULLMATCH so there is only one attempt.
    ;; '1' matches at pos 0, then ABORT fires.
    (let [pat (SEQ "1" ABORT)]
      (is (nil? (FULLMATCH "1xyz" pat))))))

(deftest abort-terminates-regardless-of-remaining-alternatives
  (testing "ABORT inside SEQ kills match even when ALT has more alternatives"
    ;; pattern: ALT[ SEQ['a' ABORT], 'ax' ]
    ;; 'a' matches, ABORT fires — second alternative never tried
    (is (nil? (FULLMATCH "ax" (| (SEQ "a" ABORT) "ax"))))))

;; ─── 8.2  REM ────────────────────────────────────────────────────────────────
;; Ref: Snobol4.Net Pattern/Rem.cs TEST_Rem_001 / _002 / _003

(deftest rem-matches-rest-of-subject
  (testing "REM matches everything after preceding pattern — TEST_Rem_001"
    (let [s "programmer"
          r (SEARCH s (SEQ "gra" REM))]
      ;; full match is "grammer"; everything from 'gra' to end
      (is (some? r))
      (is (= "grammer" (subs s (first r) (second r))))
      ;; and the REM portion alone is "mmer"
      (is (= "mmer" (subs s (+ (first r) 3) (second r)))))))

(deftest rem-simple-capture
  (testing "REM captures to end of string"
    ;; The match starts at 'WIN', REM gets everything after
    (let [s   "THE WINTER WINDS"
          pat (SEQ "WIN" REM)
          r   (SEARCH s pat)]
      (is (some? r))
      ;; full match from first position of 'WIN' to end
      (is (= "TER WINDS" (subs s (+ (first r) 3) (second r)))))))

(deftest rem-at-end-matches-empty
  (testing "REM at end of string matches empty string — TEST_Rem_003"
    (let [s   "THE WINTER WINDS"
          pat (SEQ "WINDS" REM)
          r   (SEARCH s pat)]
      (is (some? r))
      ;; end of WINDS is end of string, REM consumes nothing extra
      (is (= (count s) (second r))))))

;; ─── 8.3  Bare FENCE() and FENCE(P) ─────────────────────────────────────────
;; Ref: Snobol4.Net Pattern/Fence.cs

(deftest fence-p-succeeds-when-child-matches
  (testing "FENCE(P) succeeds when child P matches"
    (is (some? (SEARCH "hello" (FENCE (SPAN "hel")))))))

(deftest fence-p-first-token
  (testing "FENCE(P) commits to first match, prevents retry — Fence.cs TEST_Abort_001"
    ;; 'ABC'     — REM matches whole string, result = 'ABC'
    ;; '123,,456' — BREAK(',') matches '123', FENCE prevents retry
    (let [tok (FENCE (| (BREAK ",") REM))]
      (is (= "ABC"  (let [r (SEARCH "ABC"      tok)] (when r (subs "ABC"      (first r) (second r))))))
      (is (= "123"  (let [r (SEARCH "123,,456" tok)] (when r (subs "123,,456" (first r) (second r)))))))))

(deftest fence-bare-aborts-match
  (testing "Bare FENCE() aborts entire match when backtrack crosses it — Fence.cs TEST_Abort_003/005"
    ;; ALT[ SEQ['a', FENCE(), 'z'],  SEQ['a', 'x'] ]
    ;; 'a' matches, FENCE() committed, 'z' fails → backtrack hits bare FENCE → abort
    ;; Second alternative never reached
    (is (nil? (FULLMATCH "ax"
                (| (SEQ "a" (FENCE) "z")
                   (SEQ "a" "x")))))))

;; ─── 8.4  ORD / CHAR ──────────────────────────────────────────────────────────

(deftest ord-returns-code
  (testing "ORD returns the integer code of the first character"
    (is (= 65 (ORD "A")))
    (is (= 97 (ORD "a")))
    (is (= 48 (ORD "0")))
    (is (= 32 (ORD " ")))))

(deftest char-returns-string
  (testing "CHAR converts integer to single-character string"
    (is (= "A" (CHAR 65)))
    (is (= "a" (CHAR 97)))
    (is (= " " (CHAR 32)))))

(deftest ord-char-roundtrip
  (testing "ORD and CHAR are mutual inverses for printable characters"
    (doseq [c "ABCDEFabcdef0123456789"]
      (is (= (str c) (CHAR (ORD (str c))))))))

;; ─── 8.5  REMDR ───────────────────────────────────────────────────────────────

(deftest remdr-basic
  (testing "REMDR returns integer remainder"
    (is (= 1 (REMDR 10 3)))
    (is (= 0 (REMDR  9 3)))
    (is (= 2 (REMDR 17 5)))
    (is (= 0 (REMDR 12 4)))))

;; ─── 8.6  INTEGER ─────────────────────────────────────────────────────────────

(deftest integer-from-string
  (testing "INTEGER parses numeric strings"
    (is (= 42 (INTEGER "42")))
    (is (= 0  (INTEGER "0")))
    (is (= -7 (INTEGER "-7")))))

(deftest integer-from-number
  (testing "INTEGER truncates reals"
    (is (= 42 (INTEGER 42)))
    (is (= 42 (INTEGER 42.9)))
    (is (= -3 (INTEGER -3.1)))))

(deftest integer-fail
  (testing "INTEGER returns ε on non-numeric input"
    (is (= ε (INTEGER "hello")))
    (is (= ε (INTEGER "")))))

;; ─── 8.7  REAL ────────────────────────────────────────────────────────────────

(deftest real-from-string
  (testing "REAL parses numeric strings to double"
    (is (= 3.14 (REAL "3.14")))
    (is (= 0.5  (REAL "0.5")))
    (is (= 1.0  (REAL "1")))))

(deftest real-from-number
  (testing "REAL promotes integers"
    (is (= 42.0 (REAL 42)))))

(deftest real-fail
  (testing "REAL returns ε on non-numeric input"
    (is (= ε (REAL "abc")))
    (is (= ε (REAL "")))))

;; ─── 8.8  CONVERT to STRING ──────────────────────────────────────────────────────────────

(deftest string-conversion
  (testing "CONVERT to STRING converts any value to string"
    (is (= "42"    (CONVERT 42 "STRING")))
    (is (= "3.14"  (CONVERT 3.14 "STRING")))
    (is (= "hello" (CONVERT "hello" "STRING")))
    (is (= ""      (CONVERT "" "STRING")))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_sprint9
;;
;; Sprint 9 — BREAKX backtracking, NSPAN, BOL/EOL, $ vs . capture semantics.
;; Test cases derived from Snobol4.Net reference suite (jcooper0/Snobol4.Net).
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

(defn- m [s pat] (when-let [[a b] (SEARCH s pat)] (subs s a b)))

;; ─── 9.1  BREAKX backtracking ────────────────────────────────────────────────
;; Ref: Snobol4.Net Pattern/BreakX.cs
;;
;; The key difference from BREAK:
;;   BREAK stops at the FIRST occurrence of a break char and does NOT retry.
;;   BREAKX stops at each break char in turn, retrying until something succeeds.
;;
;; BreakX_014 (the canonical discriminator):
;;   SUB = 'EXCEPTIONS-ARE-AS-TRUE-AS-RULES'
;;   BREAKX: P1 = POS(0) BREAKX('A') . R2 'AS'  → SUCCESS, R2='EXCEPTIONS-ARE-'
;;   BREAK:  P1 = POS(0) BREAK('A')  . R2 'AS'  → FAILURE

(deftest breakx-001-basic-match
  (testing "BREAKX matches up to first break char — same as BREAK on first attempt"
    (is (= ":= " (m ":= one,,, two" (SEQ (BREAKX "abcdefghijklmnopqrstuvwxyz")))))))

(deftest breakx-014-canonical-discriminator
  (testing "BREAKX retries past each break char until continuation matches — BreakX_014"
    (let [sub "EXCEPTIONS-ARE-AS-TRUE-AS-RULES"
          captured (atom nil)
          pat (SEQ (POS 0) (list 'CAPTURE-COND (BREAKX "A") 'R2) "AS")]
      (let [r (SEARCH sub pat)]
        (is (some? r))
        ;; The match should start at pos 0 and BREAKX should slide to 'EXCEPTIONS-ARE-'
        ;; before finding 'AS' succeeding
        (is (= "EXCEPTIONS-ARE-AS" (subs sub (first r) (second r))))))))

(deftest break-014-does-not-retry
  (testing "BREAK does NOT retry — fails where BREAKX succeeds — Break_014"
    ;; POS(0) BREAK('A') . R2 'AS' on 'EXCEPTIONS-ARE-AS-TRUE-AS-RULES'
    ;; BREAK gets 'EXCEPTIONS-' (stops at first A), then tries 'AS' against 'ARE...' → fails
    ;; No retry, so overall failure
    (let [sub "EXCEPTIONS-ARE-AS-TRUE-AS-RULES"
          pat (SEQ (POS 0) (BREAK "A") "AS")]
      (is (nil? (SEARCH sub pat))))))

(deftest breakx-002-non-backtracking-match
  (testing "BREAKX match with first occurrence works — BreakX_001"
    (is (= ":= " (m ":= one,,, two,..  three"
                    (BREAKX "abcdefghijklmnopqrstuvwxyz"))))))

(deftest breakx-empty-subject
  (testing "BREAKX on empty subject fails"
    (is (nil? (SEARCH "" (BREAKX "abc"))))))

(deftest breakx-no-break-char-in-subject
  (testing "BREAKX fails when break char never found — BreakX_009"
    (is (nil? (SEARCH "12345" (BREAKX "8"))))))

(deftest breakx-matches-at-start
  (testing "BREAKX can match zero chars when subject starts with break char — BreakX_003"
    (is (= "" (m "one,,, two,..  three"
                  (BREAKX "abcdefghijklmnopqrstuvwxyz"))))))

;; ─── 9.2  NSPAN (0-or-more span) ─────────────────────────────────────────────

(deftest nspan-matches-nonempty
  (testing "NSPAN matches one or more chars in the set"
    (is (= "abc" (m "abcDEF" (NSPAN "abcdefghijklmnopqrstuvwxyz"))))))

(deftest nspan-matches-empty
  (testing "NSPAN succeeds even when no chars match (returns empty string)"
    ;; At position 0 of '123', no lowercase letters → NSPAN matches ε
    (let [r (SEARCH "123" (NSPAN "abcdefghijklmnopqrstuvwxyz"))]
      (is (some? r))
      (is (= "" (subs "123" (first r) (second r)))))))

(deftest nspan-vs-span-on-failure
  (testing "SPAN fails where NSPAN succeeds on no-char-matches"
    (is (nil?   (SEARCH "123" (SPAN  "abcdefghijklmnopqrstuvwxyz"))))
    (is (some?  (SEARCH "123" (NSPAN "abcdefghijklmnopqrstuvwxyz"))))))

;; ─── 9.3  BOL / EOL ──────────────────────────────────────────────────────────

(deftest bol-matches-at-position-zero
  (testing "BOL anchors match to start of subject"
    (is (some? (SEARCH "hello" (SEQ BOL "hello"))))
    (is (nil?  (SEARCH "hello" (SEQ BOL "ello"))))))

(deftest eol-matches-at-end
  (testing "EOL anchors match to end of subject"
    (is (some? (SEARCH "hello" (SEQ "hello" EOL))))
    (is (nil?  (SEARCH "hello" (SEQ "hell"  EOL))))))

(deftest bol-eol-fullmatch-equivalent
  (testing "BOL + EOL together behave like FULLMATCH anchoring"
    (is (some? (SEARCH "hello" (SEQ BOL "hello" EOL))))
    (is (nil?  (SEARCH "hello" (SEQ BOL "hell"  EOL))))))

(deftest bol-only-at-zero
  (testing "BOL refuses to match in the middle of a string (unanchored search)"
    ;; SEARCH tries all positions; BOL fails at every position except 0
    ;; so pattern BOL + 'ello' should fail
    (is (nil? (SEARCH "hello" (SEQ BOL "ello"))))))

;; ─── 9.4  $ (CAPTURE-IMM) vs . (CAPTURE-COND) ───────────────────────────────
;; Build capture nodes directly to avoid Clojure . interop ambiguity.
;; CAPTURE-COND = conditional (. operator), CAPTURE-IMM = immediate ($ operator)

(deftest capture-cond-assigns-on-success
  (testing "CAPTURE-COND assigns variable when match succeeds"
    (snobol-set! 'V ε)
    (let [pat (list 'CAPTURE-COND (SPAN "abc") 'V)]
      (SEARCH "abcXYZ" pat)
      (is (= "abc" ($$ 'V))))))

(deftest capture-cond-basic-capture
  (testing "CAPTURE-COND captures the text matched by P"
    (snobol-set! 'W ε)
    (let [pat (list 'CAPTURE-COND (SPAN "0123456789") 'W)]
      (SEARCH "123abc" pat)
      (is (= "123" ($$ 'W))))))

(deftest capture-imm-assigns-immediately
  (testing "CAPTURE-IMM ($ operator) assigns as soon as inner P matches"
    (snobol-set! 'X ε)
    (let [pat (list 'CAPTURE-IMM (SPAN "abc") 'X)]
      (SEARCH "abcDEF" pat)
      (is (= "abc" ($$ 'X))))))

(deftest capture-imm-captures-first-word
  (testing "CAPTURE-IMM captures first word from subject"
    (snobol-set! 'Y ε)
    (let [letters "abcdefghijklmnopqrstuvwxyz"
          pat     (list 'CAPTURE-IMM (SPAN letters) 'Y)]
      (SEARCH "hello world" pat)
      (is (= "hello" ($$ 'Y))))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_sprint9_bal
;;
;; BAL tests derived from Snobol4.Net TestSnobol4/Function/Pattern/Bal.cs
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

(defn- bstr [sub [s e]] (subs sub s e))

;; ── Basic BAL ─────────────────────────────────────────────────────────────────

(deftest bal-single-char
  (is (= [0 1] (SEARCH "A" BAL))))

(deftest bal-balanced-parens
  (is (= [0 3] (SEARCH "(A)" BAL))))

(deftest bal-nested-parens
  (is (= [0 7] (SEARCH "((A+B))" BAL))))

(deftest bal-unclosed-anchored
  ;; Anchored to pos 0: "(A" has no balanced match from pos 0
  (is (nil? (SEARCH "(A" (list 'SEQ (list 'POS# 0) BAL)))))

(deftest bal-unmatched-close-anchored
  (is (nil? (SEARCH ")A" (list 'SEQ (list 'POS# 0) BAL)))))

(deftest bal-first-match-canonical
  (is (= [0 13] (SEARCH "((A+(B*C))+D)" BAL))))

;; ── Multi-yield via COLLECT! — Snobol4.Net Bal_001/002 ───────────────────────

(deftest bal-collect-canonical
  (testing "BAL yields the exact Snobol4.Net sequence on ((A+(B*C))+D)"
    (let [sub      "((A+(B*C))+D)"
          expected ["((A+(B*C))+D)" "(A+(B*C))" "(A+(B*C))+" "(A+(B*C))+D"
                    "A" "A+" "A+(B*C)" "+" "+(B*C)" "(B*C)"
                    "B" "B*" "B*C" "*" "*C" "C" "+" "+D" "D"]
          got      (map (partial bstr sub) (COLLECT! sub BAL))]
      (is (= 19 (count got)))
      (is (= expected got)))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_sprint10
;;
;; Sprint 10 — operator completeness: ~P optional, @N cursor, *expr deferred (CONJ, from no SNOBOL4, is gone: CEO-2028).
;; Tests derived from Snobol4.Net reference suite and SNOBOL4 language spec.
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── ~P optional (tilde = P | ε) ──────────────────────────────────────────────

(deftest test-tilde-basic
  ;; ~P matches P or nothing — so it always succeeds
  (is (= [0 1] (SEARCH "abc" (tilde (ANY "a")))))    ; matches "a"
  (is (= [0 0] (SEARCH "abc" (tilde (ANY "z")))))    ; P fails → matches empty at 0
  (is (= [0 3] (FULLMATCH "abc" (tilde (LEN 3))))))  ; matches full string

(deftest test-tilde-optional-segment
  ;; optional prefix: ~"Mr " followed by mandatory span of letters
  (let [P (list 'SEQ (tilde "Mr ") (SPAN "Smith"))]
    (is (SEARCH "Mr Smith" P))
    (is (SEARCH "Smith" P))))

(deftest test-tilde-in-seq
  ;; ~P inside a sequence: optional "s" suffix
  (let [P (list 'SEQ "colour" (tilde "s"))]
    (is (= [0 6] (SEARCH "colour"  P)))
    (is (= [0 7] (SEARCH "colours" P)))))

;; ── @N cursor assignment ──────────────────────────────────────────────────────

(deftest test-cursor-basic
  ;; After matching 3 chars, @N captures cursor position 3
  (let [P (list 'SEQ (LEN 3) (CURSOR 'C))]
    (GLOBALS (find-ns 'SNOBOL4clojure.test-engine))
    (SEARCH "hello" P)
    (is (= 3 ($$ 'C)))))

(deftest test-cursor-at-start
  ;; @N at start captures 0
  (let [P (list 'SEQ (CURSOR 'C) (LEN 2))]
    (GLOBALS (find-ns 'SNOBOL4clojure.test-engine))
    (MATCH "hello" P)
    (is (= 0 ($$ 'C)))))

(deftest test-cursor-mid-pattern
  ;; Capture cursor at two points in one pattern
  (let [P (list 'SEQ (LEN 2) (CURSOR 'A) (LEN 3) (CURSOR 'B))]
    (GLOBALS (find-ns 'SNOBOL4clojure.test-engine))
    (MATCH "hello!" P)
    (is (= 2 ($$ 'A)))
    (is (= 5 ($$ 'B)))))

;; ── *expr deferred guard ──────────────────────────────────────────────────────

(deftest test-defer-basic
  ;; Thunk returns a literal pattern evaluated at match time
  (let [P (DEFER (fn [] "bc"))]
    (is (= [1 3] (SEARCH "abcd" P)))))

(deftest test-defer-uses-runtime-var
  ;; Classic eager-eval bug: (EQ N 2) inside ALT is computed at build time,
  ;; so N's value at build time (ε) makes EQ fail, and the branch is never taken.
  ;; DEFER wraps it in a thunk so EQ runs during match when N is already set.
  (GLOBALS (find-ns 'SNOBOL4clojure.test-engine))
  (snobol-set! 'N 0)
  (let [counted-p (list 'SEQ
                        (ARBNO (list 'SEQ
                                     (ANY "abc")
                                     (DEFER (fn []
                                              (snobol-set! 'N (inc ($$ 'N)))
                                              ε)))))]
    (FULLMATCH "abc" counted-p)
    (is (= 3 ($$ 'N)))))

(deftest test-defer-lazy-pattern-selection
  ;; Thunk picks pattern based on a variable set before match
  (GLOBALS (find-ns 'SNOBOL4clojure.test-engine))
  (snobol-set! 'MODE "vowel")
  (let [P (DEFER (fn [] (if (= ($$ 'MODE) "vowel")
                          (ANY "aeiou")
                          (ANY "bcdfghjklmnpqrstvwxyz"))))]
    (is (= [0 1] (SEARCH "apple" P)))   ; vowel mode → matches "a" at 0
    (snobol-set! 'MODE "consonant")
    (is (= [1 2] (SEARCH "apple" P))))) ; consonant mode → matches "p" at 1


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_operators
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── ? (match operator) ───────────────────────────────────────────────────────
(deftest test-match-op
  (is (? "hello" "hel"))          ; substring match
  (is (? "hello" "hello"))
  (is (not (? "hello" "world"))))

;; ── | (alternation operator) ─────────────────────────────────────────────────
(deftest test-alt-op
  (let [P (| "cat" "dog")]
    (is (? "cat" P))
    (is (? "dog" P))
    (is (not (? "fish" P))))
  ;; Multi-arg
  (let [P (| "a" "b" "c")]
    (is (? "a" P))
    (is (? "b" P))
    (is (? "c" P))
    (is (not (? "d" P)))))

;; ── EQ / NE / LT / LE / GT / GE ──────────────────────────────────────────────
(deftest test-numeric-comparison
  (is      (EQ 1 1))
  (is (not (EQ 1 2)))
  (is      (NE 1 2))
  (is      (LT 1 2))
  (is (not (LT 2 1)))
  (is      (LE 1 1))
  (is      (LE 1 2))
  (is (not (LE 2 1)))
  (is      (GT 2 1))
  (is      (GE 2 2))
  (is (not (GE 1 2))))

;; ── LEQ / LNE (string comparison) ────────────────────────────────────────────
(deftest test-string-comparison
  (is      (LEQ "abc" "abc"))
  (is (not (LEQ "abc" "def")))
  (is      (LNE "abc" "xyz")))

;; ── IDENT / DIFFER ───────────────────────────────────────────────────────────
(deftest test-identity
  (let [x "hello"]
    ;; JVM interns string literals, so "hello" IS identical? to "hello"
    (is      (IDENT  x x))
    (is      (IDENT  x "hello"))    ; interned — same object
    (is (not (DIFFER x x)))
    (is (not (DIFFER x "hello"))))  ; interned — same object
  ;; IDENT uses value equality — (String. "hello") equals "hello"
  (let [x (str "hel" "lo")
        y (String. "hello")]
    (is      (IDENT x y))           ; same value → IDENT succeeds
    (is (not (DIFFER x y)))))       ; same value → DIFFER fails

;; ── EVAL with quoted expressions ──────────────────────────────────────────────
(deftest test-eval-quoted
  (let [P (EVAL '(ANY "aeiou"))]
    (is (? "apple" P))
    (is (not (? "xyz" P))))
  ;; Nested ALT
  (let [P (EVAL '(| "cat" "dog"))]
    (is (? "dog" P))))

;; ── EVAL with vector (SEQ) ───────────────────────────────────────────────────
(deftest test-eval-seq
  ;; Vector evaluates to a SEQ pattern
  (let [P (EVAL '["hel" "lo"])]
    (is (? "hello" P))
    (is (not (? "world" P)))))

;; ── EVAL with string (parses SNOBOL4 syntax) ─────────────────────────────────
(deftest test-eval-string
  (let [P (EVAL "ANY('aeiou')")]
    (is (? "apple" P))
    (is (not (? "xyz" P)))))

;; ── + and - operators ────────────────────────────────────────────────────────
(deftest test-arithmetic-ops
  ;; These produce Clojure list expressions (deferred arithmetic)
  ;; rather than immediately computing — that is by design.
  (is (list? (ops/+ 1 2)))
  (is (list? (ops/- 5 3)))
  (is (list? (ops/* 2 3)))
  (is (list? (ops// 6 2))))


;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
;; test_env
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

;; ── SNOBOL4 constants ─────────────────────────────────────────────────────────
(deftest test-constants
  (is (= "" ε))
  (is (Double/isNaN η))
  (is (= "0123456789"                  &DIGITS))
  (is (= "abcdefghijklmnopqrstuvwxyz"  &LCASE))
  (is (= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"  &UCASE)))

;; ── DATATYPE dispatch ─────────────────────────────────────────────────────────
(deftest test-datatype
  (is (= "STRING"     (DATATYPE "")))
  (is (= "STRING"     (DATATYPE "hello")))
  (is (= "INTEGER"    (DATATYPE 42)))
  (is (= "REAL"       (DATATYPE 3.14)))
  (is (= "ARRAY"      (DATATYPE (ARRAY "0:9"))))
  (is (= "TABLE"      (DATATYPE (TABLE))))
  (is (= "PATTERN"    (DATATYPE [:LEN 3])))
  (is (= "PATTERN"    (DATATYPE ["hello"])))
  (is (= "NAME"       (DATATYPE 'x)))
  (is (= "EXPRESSION" (DATATYPE '(+ 1 2))))
  (is (= "SET"        (DATATYPE (empty-set))))
  (is (= "SET"        (DATATYPE #{})))
  (is (= "REGEX"      (DATATYPE #"abc"))))

;; ── NAME deftype ──────────────────────────────────────────────────────────────
(deftest test-name-type
  (let [n (NAME. 'x)]
    (is (= 'x (.n n)))
    (is (= "NAME" (DATATYPE n)))
    (.n n 'y)
    (is (= 'y (.n n)))))

;; ── Arithmetic aliases ────────────────────────────────────────────────────────
(deftest test-arithmetic
  (is (= 5  (Σ+ 2 3)))
  (is (= 2  (subtract 5 3)))
  (is (= 6  (multiply 2 3)))
  (is (= 3  (divide 6 2)))
  (is      (equal     5 5))
  (is (not (equal     5 6)))
  (is      (not-equal 5 6))
  (is (not (not-equal 5 5))))

;; ── num conversion ────────────────────────────────────────────────────────────
(deftest test-num
  (is (= 3.14  (num 3.14)))
  (is (= 42    (num 42)))
  (is (= 1.5   (num "1.5")))
  (is (Double/isNaN (num "abc"))))

;; ── $$ lookup ────────────────────────────────────────────────────────────────
(GLOBALS)

(deftest test-$$
  (snobol-set! 'test-$$-sentinel "hello")
  (is (= "hello" ($$ 'test-$$-sentinel)))
  (is (= ""      ($$ 'no-such-var-xyz))))


(use-fixtures :each (fn [t] (GLOBALS) (t)))
