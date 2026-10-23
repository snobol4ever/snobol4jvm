(ns SNOBOL4clojure.catalog.t-io-channels
  "Sprint 25D — Named I/O channels.
   Tests for INPUT(.VAR, unit, 'file') and OUTPUT(.VAR, unit, 'file'), as SPITBOL takes them (manual ch. 19, INPUT): standard input or output on a unit is the
   option -f0 or -f1, a unit with no file is ERROR 116 (160), and a fourth argument, CSNOBOL4's file name, is not read.
   All tests are Shape A (atomic) or Shape B (bounded loop).
   Budget: 1000ms per test."
  (:require [clojure.test  :refer :all]
            [clojure.java.io :as io]
            [SNOBOL4clojure.core :refer :all :exclude [= + - * / num]]))

(use-fixtures :each (fn [f]
                      (GLOBALS (find-ns 'SNOBOL4clojure.catalog.t-io-channels))
                      (reset! <CHANNELS> {})
                      (f)
                      ;; Close only file-backed channels (not stdin/stdout wrappers)
                      (doseq [[_k ch] @<CHANNELS>]
                        (when (:file ch)
                          (try (when-let [r (:reader ch)] (.close r)) (catch Exception _ nil))
                          (try (when-let [w (:writer ch)] (.close w)) (catch Exception _ nil))))
                      (reset! <CHANNELS> {})))

;; ── 25D.1: Channel registry atom exists and starts empty ─────────────────────
(deftest channel-registry-exists
  (testing "<CHANNELS> atom is initially empty"
    (is (map? @<CHANNELS>))
    (is (empty? @<CHANNELS>))))

;; ── 25D.2: INPUT(.VAR, unit) with stdin — channel registered ─────────────────
(deftest input-channel-registration-stdin
  (testing "INPUT(.VAR, unit, '[-f0]') registers var as input channel against stdin"
    ;; Use INVOKE directly (same path as SNOBOL4 program).
    ;; -f0 names standard input
    (INVOKE 'INPUT 'READER (long 5) "[-f0]")
    (let [ch (get @<CHANNELS> 'READER)]
      (is (some? ch)          "Channel entry created for var symbol")
      (is (= :input (:type ch)) "Channel type is :input")
      (is (= 5      (:unit ch)) "Unit number stored")
      (is (some? (:reader ch))  "Reader present"))))

;; ── 25D.3: INPUT(.VAR, unit) — also indexed by unit number ───────────────────
(deftest input-channel-indexed-by-unit
  (testing "Channel accessible by unit number after INPUT registration"
    (INVOKE 'INPUT 'READER2 (long 7) "[-f0]")
    (let [by-unit (get @<CHANNELS> 7)]
      (is (some? by-unit)             "Entry exists keyed by unit integer")
      (is (= :input (:type by-unit))  "Type is :input")
      (is (= 'READER2 (:var by-unit)) "Var symbol stored"))))

;; ── 25D.4: OUTPUT(.VAR, unit) registration ───────────────────────────────────
(deftest output-channel-registration
  (testing "OUTPUT(.VAR, unit, '[-f1]') registers var as output channel"
    (INVOKE 'OUTPUT 'WRITER (long 6) "[-f1]")
    (let [ch (get @<CHANNELS> 'WRITER)]
      (is (some? ch)           "Channel entry created")
      (is (= :output (:type ch)) "Channel type is :output")
      (is (= 6       (:unit ch)) "Unit number stored")
      (is (some? (:writer ch))   "Writer present"))))

;; ── 25D.5: INPUT from a real file ────────────────────────────────────────────
(deftest input-channel-reads-file
  (testing "INPUT(.VAR, unit, 'file') reads lines from a file"
    (let [tmp (java.io.File/createTempFile "snobol4_test_" ".txt")]
      (try
        (spit (.getPath tmp) "hello\nworld\n")
        ;; Register channel
        (INVOKE 'INPUT 'LINER (long 3) (.getPath tmp))
        ;; Read two lines via $$ — each call returns the next line
        (let [line1 ($$ 'LINER)
              line2 ($$ 'LINER)
              line3 ($$ 'LINER)]   ; EOF → nil
          (is (= "hello" line1) "First line read correctly")
          (is (= "world" line2) "Second line read correctly")
          (is (nil? line3)      "EOF returns nil (statement fails)"))
        (finally
          (.delete tmp))))))

;; ── 25D.6: OUTPUT to a real file ─────────────────────────────────────────────
(deftest output-channel-writes-file
  (testing "OUTPUT(.VAR, unit, 'file') writes lines to a file"
    (let [tmp (java.io.File/createTempFile "snobol4_out_" ".txt")]
      (try
        (.delete tmp)  ; ensure fresh
        ;; Register output channel
        (INVOKE 'OUTPUT 'LINER (long 4) (.getPath tmp))
        ;; Write via assignment (INVOKE '= path)
        (INVOKE '= 'LINER "line one")
        (INVOKE '= 'LINER "line two")
        ;; Flush by closing
        (let [ch (get @<CHANNELS> 'LINER)
              wtr ^java.io.PrintWriter (:writer ch)]
          (.flush wtr))
        (let [content (slurp (.getPath tmp))]
          (is (clojure.string/includes? content "line one") "First line written")
          (is (clojure.string/includes? content "line two") "Second line written"))
        (finally
          (.delete tmp))))))

;; ── 25D.7: ENDFILE closes channel ────────────────────────────────────────────
(deftest endfile-closes-channel
  (testing "ENDFILE(unit) removes channel from registry"
    (INVOKE 'INPUT 'CLOSEME (long 9) "[-f0]")
    (is (some? (get @<CHANNELS> 'CLOSEME)) "Channel exists before ENDFILE")
    (INVOKE 'ENDFILE (long 9))
    (is (nil? (get @<CHANNELS> 'CLOSEME))  "Var entry removed after ENDFILE")
    (is (nil? (get @<CHANNELS> 9))          "Unit entry removed after ENDFILE")))

;; ── 25D.8: DETACH removes channel by var ─────────────────────────────────────
(deftest detach-removes-channel
  (testing "DETACH removes channel registration by variable"
    (INVOKE 'INPUT 'DETACHME (long 11) "[-f0]")
    (is (some? (get @<CHANNELS> 'DETACHME)) "Channel registered")
    (INVOKE 'DETACH 'DETACHME)
    (is (nil? (get @<CHANNELS> 'DETACHME)) "Channel removed by DETACH")))

;; ── 25D.9: Full SNOBOL4 program — read file via named channel (direct RUN) ────
(deftest prog-reads-file-via-named-channel
  (testing "Full SNOBOL4 program reads a file line-by-line via INPUT(.VAR,...)"
    ;; Use direct RUN (not run-with-timeout) to avoid future/*in* closed-stream issue.
    ;; The channel opens a real FileReader independently of *in*.
    (let [tmp (java.io.File/createTempFile "snobol4_prog_" ".txt")]
      (try
        (spit (.getPath tmp) "alpha\nbeta\ngamma\n")
        (let [stdout (with-out-str
                       (try
                         (RUN (CODE-memo
                                (str "        INPUT(.LINE, 5, '" (.getPath tmp) "')\n"
                                     "LOOP    OUTPUT = LINE          :S(LOOP)\n"
                                     "END\n")))
                         (catch clojure.lang.ExceptionInfo e
                           (when-not (= :end (get (ex-data e) :snobol/signal))
                             (throw e)))))]
          (is (clojure.string/includes? stdout "alpha") "alpha printed")
          (is (clojure.string/includes? stdout "beta")  "beta printed")
          (is (clojure.string/includes? stdout "gamma") "gamma printed"))
        (finally
          (.delete tmp))))))

;; A unit with no file is ERROR 116 for INPUT and 160 for OUTPUT, and a fourth argument is not read (sbl -bf: INPUT(.X, 8, , 'f') is ERROR 116); a unit already
;; open is shared, and a channel named by a string takes the file an environment variable of that name holds (manual ch. 19, INPUT).
(deftest a-unit-with-no-file-is-an-error
  (let [code (fn [f & args] (try (apply INVOKE f args) :no-error (catch clojure.lang.ExceptionInfo e (:snobol/error (ex-data e)))))
        tmp  (java.io.File/createTempFile "snobol4_test_" ".txt")]
    (try
      (spit (.getPath tmp) "hello\n")
      (is (= 116 (code 'INPUT 'X (long 8))))
      (is (= 116 (code 'INPUT 'X (long 8) "" (.getPath tmp))))
      (is (= 116 (code 'INPUT 'X "" (.getPath tmp))))
      (is (= 160 (code 'OUTPUT 'X (long 8))))
      (is (= :no-error (code 'INPUT 'X (long 8) (.getPath tmp))))
      (is (= :no-error (code 'INPUT 'Y (long 8))))
      (is (= "hello" ($$ 'Y)))
      (finally (.delete tmp)))))
