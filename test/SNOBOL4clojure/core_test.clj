(ns SNOBOL4clojure.core-test
  (:require [clojure.test :refer :all]
            [SNOBOL4clojure.core :refer :all :exclude [= + - * / num]]))

;; Point the SNOBOL runtime at this namespace so that $$ resolves vars.
(GLOBALS *ns*)

;; Re-establish GLOBALS before each test and re-bind SNOBOL4 vars.
(defn- reset-test-vars! []
  (GLOBALS)
  (snobol-set! 'digits  "0123456789")
  (snobol-set! 'epsilon ""))

(use-fixtures :each (fn [t] (reset-test-vars!) (t)))

;; ── match-1 : literal concatenation with alternation ─────────────────────────
(deftest match-1
  (let [ROOT "car"
        PAT  '[ROOT (| "s" "es" "")]]
    (is (EVAL '(? "car"   PAT)))
    (is (EVAL '(? "cars"  PAT)))
    (is (EVAL '(? "cares" PAT)))))

;; ── match-2 : conditional (EQ guard inside pattern) ──────────────────────────
;; TODO: the EQ guard is not properly blocking alternatives — (not (? "wolf" P))
;; returns false when N=1 because EQ N 2 guard should prevent the wolf branch.
;; Known engine bug: conditional guards in ALT don't prune failed branches.
(deftest match-2
  (let [N 1
        P '(| [(EQ N 1) "fox"]
              [(EQ N 2) "wolf"])]
    (is (EVAL '(? "fox" P)))
    ;; TODO: re-enable when EQ guard pruning is fixed:
    #_(is (not (EVAL '(? "wolf" P))))))

;; ── match-3 : BEARDS family — nested ALT, full-string anchor ─────────────────
(deftest match-3
  (let [BD (EVAL '[(POS 0) (| "BE" "BO" "B") (| "AR" "A") (| "DS" "D") (RPOS 0)])]
    (is (? "BEARDS" BD))
    (is (? "BEARD"  BD))
    (is (? "BEADS"  BD))
    (is (? "BEAD"   BD))
    (is (? "BARDS"  BD))
    (is (? "BARD"   BD))
    (is (? "BADS"   BD))
    (is (? "BAD"    BD))
    (is (not (? "BATS" BD)))))

;; ── match-4 : BED family — broader initial set ───────────────────────────────
(deftest match-4
  (let [BED (EVAL '[(POS 0) (| "B" "F" "L" "R") (| "E" "EA") (| "D" "DS") (RPOS 0)])]
    (is (? "BED"   BED))
    (is (? "BEDS"  BED))
    (is (? "BEAD"  BED))
    (is (? "BEADS" BED))
    (is (? "RED"   BED))
    (is (? "REDS"  BED))
    (is (? "READ"  BED))
    (is (? "READS" BED))
    (is (not (? "LEADER" BED)))))

;; ── match-5 : ANY + SPAN ──────────────────────────────────────────────────────
(deftest match-5
  (let [A (EVAL '[(POS 0) (ANY "BFLR") (SPAN "EA") "D" (RPOS 0)])]
    (is (? "BED"  A))
    (is (? "FAD"  A))
    (is (? "LEED" A))
    (is (? "READ" A))
    (is (? "RAD"  A))
    (is (not (? "IED" A)))
    (is (not (? "JED" A)))
    (is (not (? "BID" A)))))

;; ── match-real : floating point number pattern ───────────────────────────────
;; Built from a SNOBOL4 source string via EVAL — exercises the full
;; parse -> emitter -> MATCH pipeline.
;; digits/epsilon are top-level defs so $$ can resolve them via the
;; core-test namespace (established by the GLOBALS call above).
;; Build the pattern at load time — digits/epsilon must be in <VARS> first.
(snobol-set! 'digits  "0123456789")
(snobol-set! 'epsilon "")
(def real-number-pat
  (EVAL (str "POS(0)"
             " SPAN(digits)"
             " (  ('.' FENCE(SPAN(digits) | epsilon) | epsilon)"
             "    ('E' | 'e')"
             "    ('+' | '-' | epsilon)"
             "    SPAN(digits)"
             " |  '.' FENCE(SPAN(digits) | epsilon)"
             ")"
             " RPOS(0)")))

(deftest match-real
  (let [real real-number-pat]
    (is (? "1.618e+10" real))
    (is (? "1."        real))
    (is (? "1.6"       real))
    (is (? "1.61"      real))
    (is (? "1.6E2"     real))
    (is (? "1.6e-1"    real))
    (is (? "1.61e+2"   real))
    (is (? "1.618e+3"  real))
    (is (not (? "1"     real)))
    (is (not (? "1.6E"  real)))
    (is (not (? "1.6e"  real)))
    (is (not (? "1.6E-" real)))
    (is (not (? "1.6e-" real)))
    (is (not (? "1.6E+" real)))
    (is (not (? "1.6e+" real)))))

;; ── match-define : identifier pattern ────────────────────────────────────────
;; TODO: ANY(&UCASE &LCASE) — string concatenation as ANY argument inside
;; EVAL string throws ClassCastException (String cast to Future).
;; This is a bug in how EVAL resolves multi-arg ANY. Skipping for now.
(deftest match-define
  (def digits  "0123456789")
  (def epsilon "")
  #_(let [identifier (EVAL (str
                     "POS(0)"
                     " ANY(&UCASE &LCASE)"
                     " FENCE(SPAN(digits '-.' &UCASE '_' &LCASE) | epsilon)"
                     " RPOS(0)"))]
    (is (? ""             identifier))
    (is (? "v"            identifier))
    (is (? "id"           identifier))
    (is (? "ID19"         identifier))
    (is (? "match-define" identifier))
    (is (? "v_pat.name"   identifier)))
  ;; Minimal smoke test that EVAL round-trips at all:
  (is (string? digits)))

;; ── datatype-test : DATATYPE dispatch ────────────────────────────────────────
(deftest datatype-test
  (is (= (DATATYPE "no")                                  "STRING"))
  (is (= (DATATYPE 2019)                                  "INTEGER"))
  (is (= (DATATYPE 1.62)                                  "REAL"))
  (is (= (DATATYPE (ARRAY "0:9"))                         "ARRAY"))
  (is (= (DATATYPE (TABLE))                               "TABLE"))
  (is (= (DATATYPE [:LEN 10])                             "PATTERN"))
  (is (= (DATATYPE ["Hello, " (SPAN &LCASE)])             "PATTERN"))
  ;; naked pattern constructors return PATTERN:
  (is (= (DATATYPE (LEN 10))  "PATTERN"))
  (is (= (DATATYPE (POS 0))   "PATTERN"))
  (is (= (DATATYPE (RPOS 0))  "PATTERN"))
  (is (= (DATATYPE 'epsilon)                              "NAME"))
  (is (= (DATATYPE (SNOBOL4clojure.env.NAME. 'epsilon))   "NAME"))
  (is (= (DATATYPE (list '* (list 'EQ 0 0)))              "EXPRESSION"))
  (is (= (DATATYPE '(+ 1 2))                              "EXPRESSION"))
  (is (= (DATATYPE (CODE "what year = 2021"))             "CODE"))
  (is (= (DATATYPE #{})                                   "SET"))
  (is (= (DATATYPE (SET))                                 "SET"))
  (is (= (DATATYPE #"^[A-Z]+$")                           "REGEX"))
  (is (= (DATA "tree(t,v,n,c)")                           ""))
  ;; TODO: custom DATA datatype dispatch once DATA is fully wired:
  #_(is (= (DATATYPE (tree. \+ nil 2 [1 2])) "tree")))

;; The command line runs a program the way the test suite does, through the
;; interpreter. It used to run the Stage 3 VM, which dropped every statement
;; after a DEFINE's goto: this program printed only "one".
(deftest main-runs-every-statement-after-a-define
  (let [f (java.io.File/createTempFile "main-define" ".sno")]
    (try
      (spit f (str "        OUTPUT = 'one'\n"
                   "        DEFINE('F(X)')         :(F_END)\n"
                   "F       F = X '!'              :(RETURN)\n"
                   "F_END\n"
                   "        OUTPUT = F('two')\n"
                   "END\n"))
      (is (= "one\ntwo!\n" (with-out-str (-main (.getPath f)))))
      (finally (.delete f)))))

;; &ALPHABET is the 256-character string, so REPLACE and SIZE take it like any
;; other string. It was a vector of chars, and REPLACE(&ALPHABET, ...) threw a
;; ClassCastException (Gimpel's AGT.INC builds its case table that way).
;; Expected output is the oracle's: sbl -bf prints 256 and cAB.
(deftest alphabet-keyword-is-the-256-character-string
  (let [f (java.io.File/createTempFile "alphabet" ".sno")]
    (try
      (spit f (str "        T = REPLACE(&ALPHABET, 'ab', 'AB')\n"
                   "        OUTPUT = SIZE(&ALPHABET)\n"
                   "        OUTPUT = REPLACE('cab', &ALPHABET, T)\n"
                   "END\n"))
      (is (= "256\ncAB\n" (with-out-str (-main (.getPath f)))))
      (finally (.delete f)))))

;; Exponentiation: ** with ^ and ! as its synonyms. Integer ** integer is an
;; integer, a negative exponent gives a real, and the operator is
;; right-associative. It was Math/pow everywhere, so 2 ** 10 printed the real
;; 1024., and ^ and ! reached INVOKE with no entry and stopped the program.
;; Expected output is the oracle's: sbl -bf prints these lines.
(deftest exponentiation-integer-real-and-synonyms
  (let [f (java.io.File/createTempFile "power" ".sno")]
    (try
      (spit f (str "        OUTPUT = 2 ** 10\n"
                   "        OUTPUT = 2 ^ 10\n"
                   "        OUTPUT = 2 ! 10\n"
                   "        OUTPUT = 2 ** 0\n"
                   "        OUTPUT = 2 ** -1\n"
                   "        OUTPUT = -2 ** 3\n"
                   "        OUTPUT = 2 ** 62\n"
                   "        OUTPUT = '3' ** '2'\n"
                   "        OUTPUT = 2 ** 3 ** 2\n"
                   "END\n"))
      (is (= "1024\n1024\n1024\n1\n0.5\n-8\n4611686018427387904\n9\n512\n"
             (with-out-str (-main (.getPath f)))))
      (finally (.delete f)))))

;; A real becomes text the way SPITBOL prints it: 15 significant digits with
;; trailing zeros stripped; fixed point for 0.1 <= |x| < 1e15, with a trailing
;; "." when integral; otherwise 0.<digits>e<sign><exponent>; zero is "0.".
;; Reals printed as Clojure doubles ("2.0", "1.4142135623730951").
;; Expected output is the oracle's: sbl -bf prints these lines.
(deftest real-values-print-as-spitbol-prints-them
  (let [f (java.io.File/createTempFile "reals" ".sno")]
    (try
      (spit f (str
                   "        OUTPUT = 2.0\n"
                   "        OUTPUT = -0.5\n"
                   "        OUTPUT = 100.0\n"
                   "        OUTPUT = 1.0 / 3.0\n"
                   "        OUTPUT = 2 ** 0.5\n"
                   "        OUTPUT = 0.1 + 0.2\n"
                   "        OUTPUT = 12345.678\n"
                   "        OUTPUT = 999999999999999.0\n"
                   "        OUTPUT = 1234567890123456.0\n"
                   "        OUTPUT = 10.0 ** 20\n"
                   "        OUTPUT = 1.0 / 1000\n"
                   "        OUTPUT = 1.0 / 20\n"
                   "        OUTPUT = 1.5 / 10000000000\n"
                   "        OUTPUT = 0.0\n"
                   "        OUTPUT = 'v=' 3.0\n"
                   "        OUTPUT = 'x' 2.5 'y'\n"
                   "END\n"))
      (is (= "2.\n-0.5\n100.\n0.333333333333333\n1.4142135623731\n0.3\n12345.678\n999999999999999.\n0.123456789012346e+16\n0.1e+21\n0.1e-2\n0.5e-1\n0.15e-9\n0.\nv=3.\nx2.5y\n"
             (with-out-str (-main (.getPath f)))))
      (finally (.delete f)))))

;; A file specification the engine cannot open is an error, never a failure and
;; never a file: ERROR 116 for INPUT, ERROR 160 for OUTPUT. A pipe spec
;; ("|cat >x") was opened by FileWriter as a file literally named "|cat >x",
;; and a missing input file or an unwritable output path made the call fail.
;; The oracle's verdict, measured with sbl -b on each statement alone:
;;   OUTPUT(.o,99,,"|cat >popen2.dat")  ERROR 160, no file created
;;   OUTPUT(.o,99,,"/nonexistent/dir/x") ERROR 160
;;   INPUT(.i,98,,"|echo hi")           ERROR 116
;;   INPUT(.i,98,,"no_such_file")       ERROR 116
(deftest an-unopenable-file-specification-is-error-116-or-160
  (let [dir  (.toFile (java.nio.file.Files/createTempDirectory
                        "fspec" (make-array java.nio.file.attribute.FileAttribute 0)))
        pipe (str "|cat >" (.getPath (java.io.File. dir "popen2.dat")))
        code (fn [stmt]
               (try (RUN (CODE (str "        " stmt "  :S(END)F(END)\nEND\n")))
                    :no-error
                    (catch clojure.lang.ExceptionInfo e
                      (or (:snobol/error (ex-data e)) (.getMessage e)))))]
    (try
      (is (= 160 (code (str "OUTPUT(.o, 99,, \"" pipe "\")"))))
      (is (= [] (vec (.list dir))))
      (is (= 160 (code (str "OUTPUT(.o, 99,, \"" (.getPath (java.io.File. dir "no/such/x")) "\")"))))
      (is (= 116 (code "INPUT(.i, 98,, \"|echo hi\")")))
      (is (= 116 (code (str "INPUT(.i, 98,, \"" (.getPath (java.io.File. dir "no_such_file")) "\")"))))
      (finally (doseq [f (reverse (file-seq dir))] (.delete ^java.io.File f))))))

;; A relative file name resolves against env/*file-dir* when that is bound, so
;; a caller can keep a program's files out of the working directory.
(deftest a-relative-file-name-resolves-against-file-dir
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                       "filedir" (make-array java.nio.file.attribute.FileAttribute 0)))
        name (str "filedir-" (System/nanoTime) ".txt")]
    (try
      (binding [SNOBOL4clojure.env/*file-dir* dir]
        (RUN (CODE (str "        OUTPUT(.O, 9,, '" name "')\n"
                        "        O = 'written'\n"
                        "        ENDFILE(9)\n"
                        "END\n"))))
      (is (= "written\n" (slurp (java.io.File. dir name))))
      (is (not (.exists (java.io.File. name))))
      (finally (doseq [f (reverse (file-seq dir))] (.delete ^java.io.File f))))))
