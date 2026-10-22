(ns SNOBOL4clojure.catalog.t-code
  "Sprint 25F — CODE(src) compiles a SNOBOL4 source fragment and returns its object code, of datatype CODE; a direct Goto, :<ENTRY>, runs it, as the SPITBOL manual
   has it (ch. 19, CODE: \"The CODE function returns a pointer to the object code ... executed with a direct Goto (:<CEDAR>)\"). A normal Goto, :(ENTRY), names a label,
   and a variable that holds code is not one: sbl -bf raises ERROR 038. Until the direct Goto was its own form these tests jumped with :(ENTRY) and returned with :(RETURN)
   at level zero (sbl: ERROR 242), and read DATATYPE INTEGER; each program below is what sbl -bf runs, with its output as the expectation.
  "
  (:require [clojure.test :refer [deftest is]]
            [SNOBOL4clojure.test-helpers :refer [prog]]
            [SNOBOL4clojure.env :refer [$$]]))

(deftest code_output
  "CODE + direct goto produces output"
  (is (= "dynamic\n"
         (:stdout (prog "
        ENTRY = CODE(\"        OUTPUT = 'dynamic'  :(DONE)\")
        :<ENTRY>
DONE
END")))))

(deftest code_sees_outer_env
  "CODE fragment can read variables set before the CODE call"
  (is (= "hello\n"
         (:stdout (prog "
        MSG = 'hello'
        ENTRY = CODE(\"        OUTPUT = MSG  :(DONE)\")
        :<ENTRY>
DONE
END")))))

(deftest code_sets_outer_env
  "CODE fragment assignments are visible in the outer program after the call"
  (is (= "99\n"
         (:stdout (prog "
        ENTRY = CODE(\"        X = 99  :(AFTER)\")
        :<ENTRY>
AFTER   OUTPUT = X
END")))))

(deftest code_define_callable
  "CODE can install a DEFINE; the function is then callable in the outer program"
  (is (= "42\n"
         (:stdout (prog "
        ENTRY = CODE(\"        DEFINE('ANSWER()')  :(AFTER)\")
        :<ENTRY>
ANSWER  OUTPUT = 42
        :(RETURN)
AFTER   ANSWER()
END")))))

(deftest code_return_type
  "CODE() returns object code: DATATYPE CODE"
  (is (= "CODE\n"
         (:stdout (prog "
        ENTRY = CODE(\"        X = 1  :(DONE)\")
        OUTPUT = DATATYPE(ENTRY)
DONE
END")))))

(deftest code_is_not_a_label
  "A normal Goto to a variable that holds code is ERROR 038, goto undefined label (sbl -bf)"
  (let [r (SNOBOL4clojure.test-helpers/run-with-timeout "
        ENTRY = CODE(\"        OUTPUT = 'dynamic'  :(DONE)\")
        :(ENTRY)
DONE
END" 2000)]
    (is (= :error (:exit r)))
    (is (re-find #"goto undefined label" (str (:thrown r))))))
