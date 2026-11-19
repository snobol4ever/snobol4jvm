(ns SNOBOL4clojure.catalog.t_define
  "Worm catalog — migrated from test_worm1000.clj.
   Each deftest encodes ONE semantic fact about SNOBOL4.
   Shape A: atomic (single assignment or match), budget 2000ms.
   Shape B: bounded loop, budget 2000ms, documented bound."
  (:require [clojure.test :refer :all]
            [SNOBOL4clojure.core :refer :all :exclude [= + - * / num]]
            [SNOBOL4clojure.test-helpers :refer [prog prog-timeout prog-infinite]]))

(GLOBALS *ns*)
(use-fixtures :each (fn [f] (GLOBALS (find-ns (quote SNOBOL4clojure.catalog.t_define))) (f)))

;; prog, prog-timeout, prog-infinite imported from test-helpers (Halting Problem defence).
;; All tests run under 2000ms wall-clock budget (default prog macro).

(deftest worm_define_double_3
  "DOUBLE(N) with arg 3 => 6"
  (prog
    "        DEFINE('DOUBLE(N)') :(DOUBLE_END)"
    "DOUBLE   DOUBLE = N * 2   :(RETURN)"
    "DOUBLE_END"
    "        R = DOUBLE(3)"
    "end"
  )
    (is (= 6 ($$ (quote R))))
)

(deftest worm_define_double_0
  "DOUBLE(N) with arg 0 => 0"
  (prog
    "        DEFINE('DOUBLE(N)') :(DOUBLE_END)"
    "DOUBLE   DOUBLE = N * 2   :(RETURN)"
    "DOUBLE_END"
    "        R = DOUBLE(0)"
    "end"
  )
    (is (= 0 ($$ (quote R))))
)

(deftest worm_define_double_7
  "DOUBLE(N) with arg 7 => 14"
  (prog
    "        DEFINE('DOUBLE(N)') :(DOUBLE_END)"
    "DOUBLE   DOUBLE = N * 2   :(RETURN)"
    "DOUBLE_END"
    "        R = DOUBLE(7)"
    "end"
  )
    (is (= 14 ($$ (quote R))))
)

(deftest worm_define_double_neg_1
  "DOUBLE(N) with arg -1 => -2"
  (prog
    "        DEFINE('DOUBLE(N)') :(DOUBLE_END)"
    "DOUBLE   DOUBLE = N * 2   :(RETURN)"
    "DOUBLE_END"
    "        R = DOUBLE(-1)"
    "end"
  )
    (is (= -2 ($$ (quote R))))
)

(deftest worm_define_triple_2
  "TRIPLE(N) with arg 2 => 6"
  (prog
    "        DEFINE('TRIPLE(N)') :(TRIPLE_END)"
    "TRIPLE   TRIPLE = N * 3   :(RETURN)"
    "TRIPLE_END"
    "        R = TRIPLE(2)"
    "end"
  )
    (is (= 6 ($$ (quote R))))
)

(deftest worm_define_triple_5
  "TRIPLE(N) with arg 5 => 15"
  (prog
    "        DEFINE('TRIPLE(N)') :(TRIPLE_END)"
    "TRIPLE   TRIPLE = N * 3   :(RETURN)"
    "TRIPLE_END"
    "        R = TRIPLE(5)"
    "end"
  )
    (is (= 15 ($$ (quote R))))
)

(deftest worm_define_square_4
  "SQUARE(...) calls a function never defined: DEFINE('SQ(N)') names SQ, and SQUARE is only its entry label, so the call is ERROR 022 and R is never
   assigned (manual ch. 19, DEFINE; MEASURED on sbl -bf: ERROR 022 -- undefined function called)"
  (let [r (prog
    "        DEFINE('SQ(N)') :(SQUARE_END)"
    "SQUARE   SQ = N * N   :(RETURN)"
    "SQUARE_END"
    "        R = SQUARE(4)"
    "end"
  )]
    (is (= "undefined function called" (:thrown r)))
    (is (= "" ($$ (quote R))))))

(deftest worm_define_square_3
  "SQUARE(...) calls a function never defined: DEFINE('SQ(N)') names SQ, and SQUARE is only its entry label, so the call is ERROR 022 and R is never
   assigned (manual ch. 19, DEFINE; MEASURED on sbl -bf: ERROR 022 -- undefined function called)"
  (let [r (prog
    "        DEFINE('SQ(N)') :(SQUARE_END)"
    "SQUARE   SQ = N * N   :(RETURN)"
    "SQUARE_END"
    "        R = SQUARE(3)"
    "end"
  )]
    (is (= "undefined function called" (:thrown r)))
    (is (= "" ($$ (quote R))))))

(deftest worm_define_square_0
  "SQUARE(...) calls a function never defined: DEFINE('SQ(N)') names SQ, and SQUARE is only its entry label, so the call is ERROR 022 and R is never
   assigned (manual ch. 19, DEFINE; MEASURED on sbl -bf: ERROR 022 -- undefined function called)"
  (let [r (prog
    "        DEFINE('SQ(N)') :(SQUARE_END)"
    "SQUARE   SQ = N * N   :(RETURN)"
    "SQUARE_END"
    "        R = SQUARE(0)"
    "end"
  )]
    (is (= "undefined function called" (:thrown r)))
    (is (= "" ($$ (quote R))))))

(deftest worm_define_negate_5
  "NEGATE(...) calls a function never defined: DEFINE('NEG(N)') names NEG, and NEGATE is only its entry label, so the call is ERROR 022 and R is never
   assigned (manual ch. 19, DEFINE; MEASURED on sbl -bf: ERROR 022 -- undefined function called)"
  (let [r (prog
    "        DEFINE('NEG(N)') :(NEGATE_END)"
    "NEGATE   NEG = 0 - N   :(RETURN)"
    "NEGATE_END"
    "        R = NEGATE(5)"
    "end"
  )]
    (is (= "undefined function called" (:thrown r)))
    (is (= "" ($$ (quote R))))))

(deftest worm_define_negate_neg_3
  "NEGATE(...) calls a function never defined: DEFINE('NEG(N)') names NEG, and NEGATE is only its entry label, so the call is ERROR 022 and R is never
   assigned (manual ch. 19, DEFINE; MEASURED on sbl -bf: ERROR 022 -- undefined function called)"
  (let [r (prog
    "        DEFINE('NEG(N)') :(NEGATE_END)"
    "NEGATE   NEG = 0 - N   :(RETURN)"
    "NEGATE_END"
    "        R = NEGATE(-3)"
    "end"
  )]
    (is (= "undefined function called" (:thrown r)))
    (is (= "" ($$ (quote R))))))

(deftest worm_define_addone_0
  "ADDONE(...) calls a function never defined: DEFINE('INC(N)') names INC, and ADDONE is only its entry label, so the call is ERROR 022 and R is never
   assigned (manual ch. 19, DEFINE; MEASURED on sbl -bf: ERROR 022 -- undefined function called)"
  (let [r (prog
    "        DEFINE('INC(N)') :(ADDONE_END)"
    "ADDONE   INC = N + 1   :(RETURN)"
    "ADDONE_END"
    "        R = ADDONE(0)"
    "end"
  )]
    (is (= "undefined function called" (:thrown r)))
    (is (= "" ($$ (quote R))))))

(deftest worm_define_addone_9
  "ADDONE(...) calls a function never defined: DEFINE('INC(N)') names INC, and ADDONE is only its entry label, so the call is ERROR 022 and R is never
   assigned (manual ch. 19, DEFINE; MEASURED on sbl -bf: ERROR 022 -- undefined function called)"
  (let [r (prog
    "        DEFINE('INC(N)') :(ADDONE_END)"
    "ADDONE   INC = N + 1   :(RETURN)"
    "ADDONE_END"
    "        R = ADDONE(9)"
    "end"
  )]
    (is (= "undefined function called" (:thrown r)))
    (is (= "" ($$ (quote R))))))

(deftest worm_define_addone_99
  "ADDONE(...) calls a function never defined: DEFINE('INC(N)') names INC, and ADDONE is only its entry label, so the call is ERROR 022 and R is never
   assigned (manual ch. 19, DEFINE; MEASURED on sbl -bf: ERROR 022 -- undefined function called)"
  (let [r (prog
    "        DEFINE('INC(N)') :(ADDONE_END)"
    "ADDONE   INC = N + 1   :(RETURN)"
    "ADDONE_END"
    "        R = ADDONE(99)"
    "end"
  )]
    (is (= "undefined function called" (:thrown r)))
    (is (= "" ($$ (quote R))))))

(deftest worm_freturn_causes_failure
  "FRETURN makes calling statement fail"
  (prog
    "        DEFINE('ALWAYSFAIL()') :(AF_END)"
    "ALWAYSFAIL                     :(FRETURN)"
    "AF_END"
    "        ALWAYSFAIL() :S(YES)F(NO)"
    "YES     R = 'yes'   :(END)"
    "NO      R = 'no'"
    "END"
  )
    (is (= "no" ($$ (quote R))))
)

(deftest worm_freturn_conditional
  "Function returns normally or FRETURNs based on arg"
  (prog
    "        DEFINE('POSCHECK(N)') :(PC_END)"
    "POSCHECK GT(N,0) :F(PC_FAIL)"
    "         POSCHECK = N         :(RETURN)"
    "PC_FAIL                       :(FRETURN)"
    "PC_END"
    "        R1 = POSCHECK(5)  :S(GOT)F(NO)"
    "GOT     R2 = 'ok'         :(END)"
    "NO      R2 = 'fail'"
    "END"
  )
    (is (= 5  ($$ (quote R1))))
    (is (= "ok" ($$ (quote R2))))
)

