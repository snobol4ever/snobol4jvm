(ns SNOBOL4clojure.fatal
  "The report of an execution error that ends the program, in SPITBOL's layout (measured against sbl -bf)."
  (:require [SNOBOL4clojure.env :as env])
  (:import  [java.lang.management ManagementFactory GarbageCollectorMXBean]))

;; "With the first type of execution error, SPITBOL will send a message to the screen, much as it does with compilation errors ... program.spt(3) : Error 038 --
;; Goto undefined label" (manual ch. 13, Execution errors). sbl -bf writes it on standard output: three empty lines, file(line) : ERROR nnn -- text, five empty
;; lines, where the error was and the run's figures, each labelled to column 21 (sbl.min stopr), and an empty line. The error line and the two empty lines after
;; it, the place and the figures down to REGENERATIONS, and the last empty line go to standard error as well, line by line. Each line is cut at 120 columns, and
;; of a line cut in pieces only the last goes to standard error. A negative &STLIMIT leaves out the statement count and the time (stopr: "skip if negative"); the
;; rates sbl adds after a run of a millisecond or more (stmt / microsec and the rest) measure the engine's speed, not the program's, and are not written.
(def ^:private width 120)

(defn- row [label v] (str (format "%-21s" label) v))

(defn- regenerations [] (reduce + 0 (map #(max 0 (.getCollectionCount ^GarbageCollectorMXBean %)) (ManagementFactory/getGarbageCollectorMXBeans))))

(defn- report-lines
  "[line also-to-standard-error?] pairs: the report of error code with text, after stmts statements executed in the run that started at start-ns."
  [code text stmts start-ns]
  (let [rt   (Runtime/getRuntime)
        used (- (.totalMemory rt) (.freeMemory rt))
        file @env/&FILE]
    (concat [["" false] ["" false] ["" false]
             [(format "%s(%d) : ERROR %03d -- %s" file @env/&LINE code text) true]
             ["" true] ["" true] ["" false] ["" false] ["" false]
             [(row "in file" file) true]
             [(row "in line" @env/&LINE) true]
             [(row "in statement" @env/&STNO) true]]
            (when-not (neg? (long @env/&STLIMIT))
              [[(row "stmts executed" stmts) true]
               [(row "execution time msec" (quot (- (System/nanoTime) start-ns) 1000000)) true]])
            [[(row "REGENERATIONS" (regenerations)) true]
             [(row "memory used (bytes)" used) false]
             [(row "memory left (bytes)" (- (.maxMemory rt) used)) false]
             ["" true]])))

(defn report!
  "Write the report of error code with text on *out*, and its share on *err*, as sbl -bf does."
  [code text stmts start-ns]
  (doseq [[l both?] (report-lines code text stmts start-ns)]
    (let [pieces (if (<= (count l) width) [l] (map #(apply str %) (partition-all width l)))]
      (doseq [p pieces] (println p))
      (when both? (binding [*out* *err*] (println (last pieces)))))))
