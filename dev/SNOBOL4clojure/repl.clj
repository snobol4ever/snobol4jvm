(ns SNOBOL4clojure.repl (:refer-clojure :exclude [= + - * / num]) (:require [SNOBOL4clojure.core :refer :all] [clojure.pprint :as pp]))
(defn snapshot! [] (into (sorted-map) @<VARS>))
(defn empty-set [] (hash-set))
(defn active? [name type] (boolean (some #(contains? @registry (reg-key name %)) (trace-kinds type))))
(defn any-active? [] (pos? (count @registry)))
(defn CURSOR [N] (list 'CURSOR-IMM! N))
(defn DEFER [thunk] (list 'DEFER! thunk))
(defn parse+emit [s] (first (emitter (parse-expression s))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn preprocess-includes
    ([src] (preprocess-includes src ["."]))
    ([src search-path] (preprocess-includes src search-path #{}))
    ([src search-path seen] (first (preprocess-includes* src search-path seen))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn CODE-ir
    [[codes nos labels disp]]
    (let [start (inc @STNO) base @BLANKS]
        (loop [NO 1]
            (if
                (> NO (count codes))
                (do (swap! BLANKS clojure.core/+ (:total disp 0)) (if (and (@<LABL> start) (@<CODE> (@<LABL> start))) (@<LABL> start) (when (@<CODE> start) start)))
                (do
                    (swap! STNO inc)
                    (swap! <DISPNO> assoc @STNO (clojure.core/+ @STNO base (get (:before disp) (dec NO) 0)))
                    (swap! <POS> assoc @STNO (get (:pos disp) (dec NO) ["" 0]))
                    (if-let [label (labels NO)]
                        (do (swap! <CODE> #(assoc % label (codes label))) (swap! <LABL> #(assoc % @STNO label)) (swap! <STNO> #(assoc % label @STNO)))
                        (swap! <CODE> #(assoc % @STNO (codes NO))))
                    (recur (inc NO)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn ζσ [ζ] (if ζ (ζ 2)))
(defn- ζα [ζ] (<= (ζφ ζ) 1))
(defn MATCH-REPLACE [S P R] (when-let [[start end] (SEARCH S P)] (str (subs S 0 start) R (subs S end))))
(defn at ([n] (list 'CURSOR-IMM! n)) ([_x _y] η))
(defn sharp ([_x] η) ([_x _y] η))
(defn ! ([_x] η) ([x y] (pow-fn x y)))
(defn tilde ([x] (list 'ALT x ε)) ([_x _y] η))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn bench-compare-jvm
    [src n]
    (let [
          reset-env! (fn [] (GLOBALS (find-ns 'SNOBOL4clojure.core)) (reset! STNO 0) (reset! <STNO> {}) (reset! <LABL> {}) (reset! <CODE> {}) (reset! <FUNS> {}))
          run-interp
              (fn
                  []
                  (reset-env!)
                  (with-out-str (try (RUN (CODE src)) (catch clojure.lang.ExceptionInfo e (when-not (contains? #{:end :return :freturn :nreturn} (get (ex-data e) :snobol/signal)) (throw e))))))
          run-fn (do (reset-env!) (load-program src))
          run-jvm
              (fn
                  []
                  (reset-env!)
                  (with-out-str
                      (try
                          (run-fn)
                          (catch java.lang.reflect.InvocationTargetException e
                              (let [cause (.getCause e)]
                                  (when-not
                                      (and (instance? clojure.lang.ExceptionInfo cause) (contains? #{:end :return :freturn :nreturn} (get (ex-data cause) :snobol/signal)))
                                      (throw (or cause e))))))))]
        (dotimes [_ 30] (run-interp))
        (dotimes [_ 30] (run-jvm))
        (let [
              i-out (atom "")
              j-out (atom "")
              t0 (System/nanoTime)
              _ (dotimes [_ n] (reset! i-out (run-interp)))
              i-ms (clojure.core// (clojure.core// (clojure.core/- (System/nanoTime) t0) n) 1e6)
              t1 (System/nanoTime)
              _ (dotimes [_ n] (reset! j-out (run-jvm)))
              j-ms (clojure.core// (clojure.core// (clojure.core/- (System/nanoTime) t1) n) 1e6)]
            {:interpreter-ms (double i-ms) :jvm-ms (double j-ms) :ratio (double (clojure.core// i-ms j-ms)) :outputs-match (clojure.core/= @i-out @j-out)})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def trace-register! trace!)
(def trace-stop! stoptr!)
(defn clear-all-traces! [] (reset! registry {}) (reset! elements []) (reset! &TRACE 0) nil)
(def trace-clear! clear-all-traces!)
(defn enable-full-trace! [] (trace! "*" "LABEL") (trace! "*" "VALUE") (trace! "*" "CALL") (trace! "*" "RETURN"))
(def trace-enable-full! enable-full-trace!)
(defn disable-full-trace! [] (stoptr! "*" "LABEL") (stoptr! "*" "VALUE") (stoptr! "*" "CALL") (stoptr! "*" "RETURN"))
(def trace-disable-full! disable-full-trace!)
(def trace-output *trace-output*)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn show
    [code]
    (doseq [code (if (and (seq? code) (clojure.core/= 'fn (first code))) [code] code)]
        (binding [pp/*print-right-margin* 150 pp/*print-miser-width* 60] (pp/with-pprint-dispatch pp/code-dispatch (pp/pprint code)))))
