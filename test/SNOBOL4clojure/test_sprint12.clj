(ns SNOBOL4clojure.test-sprint12
  "Sprint 12 tests: CONVERT, DATA/FIELD, SORT/RSORT."
  (:require [clojure.test :refer :all]
            [SNOBOL4clojure.core :refer :all :exclude [= + - * / num]]
            [SNOBOL4clojure.test-helpers :refer [run-with-timeout]]))

(use-fixtures :each (fn [f] (GLOBALS *ns*) (f)))

;; Run one program to its END under the suite's wall-clock budget, and fail the
;; test unless it finished normally. The programs spell their names in upper
;; case: the engine does not fold case, and neither does the oracle the suite
;; is judged by (sbl -bf).
(defn- run12 [src]
  (let [r (run-with-timeout (str src "END\n"))]
    (is (clojure.core/= :ok (:exit r)) (pr-str r))
    r))

;; ─────────────────────────────────────────────────────────────────────────────
;; 12.1  CONVERT — type coercion matrix
;; ─────────────────────────────────────────────────────────────────────────────

(deftest convert-string-string
  (run12 " A = 'hello'\n B = CONVERT(A,'STRING')\n")
  (is (= "hello" ($$ 'B))))

(deftest convert-string-integer
  (run12 " A = '42'\n B = CONVERT(A,'INTEGER')\n")
  (is (= 42 ($$ 'B))))

(deftest convert-string-integer-overflow
  (run12 " D = 'failure'\n A = '999999999999999999999999999999'\n B = CONVERT(A,'INTEGER') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-string-real
  (run12 " A = '3.14'\n B = CONVERT(A,'REAL')\n")
  (is (= 3.14 ($$ 'B))))

(deftest convert-string-real-overflow
  (run12 " D = 'failure'\n A = '1e9999'\n B = CONVERT(A,'REAL') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-string-pattern
  (run12 " A = 'abc'\n B = CONVERT(A,'PATTERN')\n")
  (is (= "PATTERN" (DATATYPE ($$ 'B)))))

(deftest convert-string-name
  (run12 " A = 'myvar'\n B = CONVERT(A,'NAME')\n")
  (is (= "NAME" (DATATYPE ($$ 'B)))))

(deftest convert-string-array-fails
  (run12 " D = 'failure'\n A = 'hello'\n B = CONVERT(A,'ARRAY') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-string-table-fails
  (run12 " D = 'failure'\n A = 'hello'\n B = CONVERT(A,'TABLE') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-integer-string
  (run12 " A = 32767\n B = CONVERT(A,'STRING')\n C = DATATYPE(B)\n")
  (is (= "32767" ($$ 'B)))
  (is (= "STRING" ($$ 'C))))

(deftest convert-integer-integer
  (run12 " A = 100\n B = CONVERT(A,'INTEGER')\n")
  (is (= 100 ($$ 'B))))

(deftest convert-integer-real
  (run12 " A = 10\n B = CONVERT(A,'REAL')\n C = DATATYPE(B)\n")
  (is (= 10.0 ($$ 'B)))
  (is (= "REAL" ($$ 'C))))

(deftest convert-integer-pattern
  (run12 " A = 99\n B = CONVERT(A,'PATTERN')\n")
  (is (= "PATTERN" (DATATYPE ($$ 'B)))))

(deftest convert-integer-name
  (run12 " A = 123\n B = CONVERT(A,'NAME')\n")
  (is (= "NAME" (DATATYPE ($$ 'B)))))

(deftest convert-integer-table-fails
  (run12 " D = 'failure'\n A = 1\n B = CONVERT(A,'TABLE') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-real-string
  (run12 " A = 3.14\n B = CONVERT(A,'STRING')\n")
  (is (= "3.14" ($$ 'B))))

(deftest convert-real-integer
  (run12 " A = 9.9\n B = CONVERT(A,'INTEGER')\n")
  (is (= 9 ($$ 'B))))

(deftest convert-real-integer-overflow
  (run12 " D = 'failure'\n A = 3.14E200\n B = CONVERT(A,'INTEGER') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-real-real
  (run12 " A = 2.5\n B = CONVERT(A,'REAL')\n")
  (is (= 2.5 ($$ 'B))))

(deftest convert-real-pattern
  (run12 " A = 1.5\n B = CONVERT(A,'PATTERN')\n")
  (is (= "PATTERN" (DATATYPE ($$ 'B)))))

(deftest convert-pattern-pattern
  (run12 " D = 'failure'\n A = ANY('abc')\n B = CONVERT(A,'PATTERN') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "success" ($$ 'D))))

(deftest convert-pattern-string-fails
  (run12 " D = 'failure'\n A = ANY('abc')\n B = CONVERT(A,'STRING') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-pattern-integer-fails
  (run12 " D = 'failure'\n A = ANY('abc')\n B = CONVERT(A,'INTEGER') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-array-to-table
  (run12 (str " B = ARRAY('3,2')\n"
            " B<1,1> = 'x'\n B<1,2> = 10\n"
            " B<2,1> = 'y'\n B<2,2> = 20\n"
            " B<3,1> = 'z'\n B<3,2> = 30\n"
            " A = CONVERT(B,'TABLE')\n"
            " VX = ITEM(A,'x')\n"
            " VY = ITEM(A,'y')\n"
            " VZ = ITEM(A,'z')\n"))
  (is (= "TABLE" (DATATYPE ($$ 'A))))
  (is (= 10 ($$ 'VX)))
  (is (= 20 ($$ 'VY)))
  (is (= 30 ($$ 'VZ))))

(deftest convert-array-to-table-wrong-shape-fails
  (run12 " D = 'failure'\n B = ARRAY('5')\n A = CONVERT(B,'TABLE') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-array-to-table-wrong-cols-fails
  (run12 " D = 'failure'\n B = ARRAY('5,3')\n A = CONVERT(B,'TABLE') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-table-to-array
  (run12 (str " T = TABLE()\n"
            " T<'a'> = 1\n"
            " T<'b'> = 2\n"
            " T<'c'> = 3\n"
            " B = CONVERT(T,'ARRAY')\n"
            " PROTO = PROTOTYPE(B)\n"))
  ;; A table converted to an array is N rows by 2 columns, and PROTOTYPE spells
  ;; that "N,2" (SPITBOL manual, PROTOTYPE); sbl -bf prints 3,2 here.
  (is (= "ARRAY" (DATATYPE ($$ 'B))))
  (is (= "3,2" ($$ 'PROTO))))

(deftest convert-table-table
  (run12 " D = 'failure'\n T = TABLE()\n T<1> = 2\n B = CONVERT(T,'TABLE') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "success" ($$ 'D))))

(deftest convert-table-string-fails
  (run12 " D = 'failure'\n T = TABLE()\n B = CONVERT(T,'STRING') :F(DONE)\n D = 'success'\nDONE\n")
  (is (= "failure" ($$ 'D))))

(deftest convert-name-string
  (run12 " A = 'quick'\n B = CONVERT(A,'NAME')\n C = DATATYPE(B)\n D = CONVERT(B,'STRING')\n")
  (is (= "NAME" ($$ 'C)))
  (is (= "quick" ($$ 'D))))

(deftest datatype-through-invoke
  (run12 " A = 'hello'\n B = DATATYPE(A)\n")
  (is (= "STRING" ($$ 'B)))
  (run12 " A = 42\n B = DATATYPE(A)\n")
  (is (= "INTEGER" ($$ 'B)))
  (run12 " A = 3.14\n B = DATATYPE(A)\n")
  (is (= "REAL" ($$ 'B)))
  (run12 " A = TABLE()\n B = DATATYPE(A)\n")
  (is (= "TABLE" ($$ 'B)))
  (run12 " A = ARRAY('3')\n B = DATATYPE(A)\n")
  (is (= "ARRAY" ($$ 'B))))

;; ─────────────────────────────────────────────────────────────────────────────
;; 12.2  DATA / FIELD — program-defined data types
;; ─────────────────────────────────────────────────────────────────────────────

(deftest data-basic-constructor-accessors
  (run12 (str " DATA('COMPLEX(REAL,IMAG)')\n"
            " X = COMPLEX(3.2,-2.0)\n"
            " I = IMAG(X)\n"
            " R = REAL(X)\n"))
  (is (= -2.0 ($$ 'I)))
  (is (= 3.2  ($$ 'R))))

(deftest data-string-fields
  (run12 (str " DATA('COMPLEX(REAL,IMAG)')\n"
            " X = COMPLEX('AAA','BBB')\n"
            " I = IMAG(X)\n"
            " R = REAL(X)\n"))
  (is (= "BBB" ($$ 'I)))
  (is (= "AAA" ($$ 'R))))

(deftest data-pattern-fields
  (run12 (str " DATA('COMPLEX(REAL,IMAG)')\n"
            " X = COMPLEX(ANY('ABC'),SPAN('123'))\n"
            " I = IMAG(X)\n"
            " R = REAL(X)\n"))
  (is (= "PATTERN" (DATATYPE ($$ 'I))))
  (is (= "PATTERN" (DATATYPE ($$ 'R)))))

(deftest data-datatype-returns-typename
  (run12 (str " DATA('POINT(X,Y)')\n"
            " P = POINT(10,20)\n"
            " T = DATATYPE(P)\n"))
  (is (= "POINT" ($$ 'T))))

(deftest data-field-write
  (run12 (str " DATA('COMPLEX(REAL,IMAG)')\n"
            " X = COMPLEX('AAA','BBB')\n"
            " IMAG(X) = 45\n"
            " REAL(X) = -5\n"
            " I = IMAG(X)\n"
            " R = REAL(X)\n"))
  (is (= 45 ($$ 'I)))
  (is (= -5 ($$ 'R))))

(deftest field-returns-field-name
  (run12 (str " DATA('COMPLEX(REAL,IMAG)')\n"
            " F1 = FIELD('COMPLEX',1)\n"
            " F2 = FIELD('COMPLEX',2)\n"))
  (is (= "REAL" ($$ 'F1)))
  (is (= "IMAG" ($$ 'F2))))

(deftest field-out-of-range-fails
  (run12 (str " D = 'failure'\n"
            " DATA('COMPLEX(REAL,IMAG)')\n"
            " F = FIELD('COMPLEX',3) :F(DONE)\n"
            " D = 'success'\nDONE\n"))
  (is (= "failure" ($$ 'D))))

;; ─────────────────────────────────────────────────────────────────────────────
;; 12.3  SORT / RSORT
;; ─────────────────────────────────────────────────────────────────────────────

(deftest sort-table-returns-array
  (run12 (str " T = TABLE()\n"
            " T<'a'> = 30\n"
            " T<'b'> = 10\n"
            " T<'c'> = 20\n"
            " S = SORT(T)\n"
            " V1 = S<1,2>\n"
            " V2 = S<2,2>\n"
            " V3 = S<3,2>\n"))
  ;; SORT of a table sorts on its first column, the keys (SPITBOL manual, SORT:
  ;; "it defaults to the smallest column number"): rows a, b, c, so the values
  ;; read 30 10 20. sbl -bf prints exactly these.
  (is (= "ARRAY" (DATATYPE ($$ 'S))))
  (is (= 30 ($$ 'V1)))
  (is (= 10 ($$ 'V2)))
  (is (= 20 ($$ 'V3))))

(deftest rsort-table-returns-array
  (run12 (str " T = TABLE()\n"
            " T<'a'> = 30\n"
            " T<'b'> = 10\n"
            " T<'c'> = 20\n"
            " S = RSORT(T)\n"
            " V1 = S<1,2>\n"
            " V2 = S<2,2>\n"
            " V3 = S<3,2>\n"))
  ;; RSORT sorts on the keys descending: rows c, b, a, so the values read
  ;; 20 10 30. sbl -bf prints exactly these.
  (is (= "ARRAY" (DATATYPE ($$ 'S))))
  (is (= 20 ($$ 'V1)))
  (is (= 10 ($$ 'V2)))
  (is (= 30 ($$ 'V3))))
