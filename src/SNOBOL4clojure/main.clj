(ns SNOBOL4clojure.main
  "Thin AOT entry point. No requires here — keeps Greek-symbol namespaces
   (env, operators, engine_frame, match) out of AOT compilation entirely.
   Delegates to SNOBOL4clojure.core/-main at runtime after dynamic load."
  (:gen-class
   :main true))
(def ^:private stack-factor 256)
(def ^:private default-stack-kb 4096)
(defn- declared-stack-kb []
  (or (try (let [v (Long/parseLong (System/getProperty "snobol4jvm.stack.kb"))] (when (pos? v) v)) (catch Exception _ nil)) default-stack-kb))
(defn- on-a-stack
  "The value of (f), computed on a thread whose stack is stack-factor times kb kilobytes, or the largest quarter of it, down to 16 MB, that the host allows; what f
   throws is thrown here."
  [kb f]
  (let [result (promise)
        body   (fn [] (deliver result (try {:value (f)} (catch Throwable t {:thrown t}))))
        floor  (* 16 1024 1024)]
    (loop [bytes (* (long kb) 1024 stack-factor)]
      (let [t (try (doto (Thread. nil body "snobol4" (long bytes)) (.start))
                   (catch OutOfMemoryError e (if (> (quot bytes 4) floor) nil (throw e))))]
        (if t
          (do (.join ^Thread t)
              (let [r @result] (if (contains? r :thrown) (throw (:thrown r)) (:value r))))
          (recur (quot bytes 4)))))))
(defn- run-program
  "A program, its input and its output are bytes, so the launcher reads and writes them as ISO-8859-1, one byte to a character, whatever the locale. The
   streams are bound for the run, which leaves the core -main free to be called with *out* rebound (tests capture its output as characters)."
  [args]
  (let [out (java.io.PrintWriter. (java.io.OutputStreamWriter. (java.io.FileOutputStream. java.io.FileDescriptor/out) "ISO-8859-1") true)
        err (java.io.PrintWriter. (java.io.OutputStreamWriter. (java.io.FileOutputStream. java.io.FileDescriptor/err) "ISO-8859-1") true)
        in  (clojure.lang.LineNumberingPushbackReader. (java.io.InputStreamReader. System/in "ISO-8859-1"))]
    (let [rc (try
               (binding [*out* out *err* err *in* in]
                 (apply (resolve 'SNOBOL4clojure.core/-main) args))
               (finally (.flush out) (.flush err)))]
      (when (and (integer? rc) (pos? rc)) (System/exit rc)))))
(defn -main
  "Runs the program the arguments name, on a thread with a stack sized from the stack it declares."
  [& args]
  (require 'SNOBOL4clojure.core)
  (on-a-stack (declared-stack-kb) #(run-program args)))
