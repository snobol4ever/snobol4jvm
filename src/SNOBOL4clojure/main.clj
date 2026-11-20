(ns SNOBOL4clojure.main (:gen-class :main true))
(def ^:private stack-factor 256)
(def ^:private default-stack-kb 4096)
(defn- declared-stack-kb [] (or (try (let [v (Long/parseLong (System/getProperty "snobol4jvm.stack.kb"))] (when (pos? v) v)) (catch Exception _ nil)) default-stack-kb))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- on-a-stack
    [kb f]
    (let
        [result (promise) body (fn [] (deliver result (try {:value (f)} (catch Throwable t {:thrown t})))) floor (* 16 1024 1024)]
        (loop
            [bytes (* (long kb) 1024 stack-factor)]
            (let
                [t (try (doto (Thread. nil body "snobol4" (long bytes)) (.start)) (catch OutOfMemoryError e (if (> (quot bytes 4) floor) nil (throw e))))]
                (if t (do (.join ^Thread t) (let [r @result] (if (contains? r :thrown) (throw (:thrown r)) (:value r)))) (recur (quot bytes 4)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- run-program
    [args]
    (let
        [out
            (java.io.PrintWriter. (java.io.OutputStreamWriter. (java.io.FileOutputStream. java.io.FileDescriptor/out) "ISO-8859-1") true)
            err
            (java.io.PrintWriter. (java.io.OutputStreamWriter. (java.io.FileOutputStream. java.io.FileDescriptor/err) "ISO-8859-1") true)
            in
            (clojure.lang.LineNumberingPushbackReader. (java.io.InputStreamReader. System/in "ISO-8859-1"))]
        (let
            [rc (try (binding [*out* out *err* err *in* in] (apply (resolve 'SNOBOL4clojure.core/-main) args)) (finally (.flush out) (.flush err)))]
            (when (and (integer? rc) (pos? rc)) (System/exit rc)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn -main [& args] (require 'SNOBOL4clojure.core) (on-a-stack (declared-stack-kb) #(run-program args)))
