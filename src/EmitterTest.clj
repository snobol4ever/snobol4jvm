(ns EmitterTest
  (:require [SNOBOL4clojure.snocone-grammar :refer [parse-sc-expr]]))
(defn -main [& _]
  (println "3.14:" (parse-sc-expr "3.14"))
  (println "x ? y:" (parse-sc-expr "x ? y"))
  (println "a b:" (parse-sc-expr "a b")))
