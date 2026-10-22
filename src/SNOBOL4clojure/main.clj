(ns SNOBOL4clojure.main
  "Thin AOT entry point. No requires here — keeps Greek-symbol namespaces
   (env, operators, engine_frame, match) out of AOT compilation entirely.
   Delegates to SNOBOL4clojure.core/-main at runtime after dynamic load."
  (:gen-class
   :main true))

(defn -main
  "A program, its input and its output are bytes, so the launcher reads and writes them as ISO-8859-1, one byte to a character, whatever the locale. The
   streams are bound for the run, which leaves the core -main free to be called with *out* rebound (tests capture its output as characters)."
  [& args]
  (require 'SNOBOL4clojure.core)
  (let [out (java.io.PrintWriter. (java.io.OutputStreamWriter. (java.io.FileOutputStream. java.io.FileDescriptor/out) "ISO-8859-1") true)
        err (java.io.PrintWriter. (java.io.OutputStreamWriter. (java.io.FileOutputStream. java.io.FileDescriptor/err) "ISO-8859-1") true)
        in  (clojure.lang.LineNumberingPushbackReader. (java.io.InputStreamReader. System/in "ISO-8859-1"))]
    (try
      (binding [*out* out *err* err *in* in]
        (apply (resolve 'SNOBOL4clojure.core/-main) args))
      (finally (.flush out) (.flush err)))))
