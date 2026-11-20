(ns SNOBOL4clojure.harness
    (:require [clojure.java.shell :as sh] [clojure.string :as str] [SNOBOL4clojure.env :as env] [SNOBOL4clojure.core :as sno] [SNOBOL4clojure.compiler :as comp] [SNOBOL4clojure.runtime :as rt])
    (:import [java.io StringWriter]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:dynamic *spitbol-bin* "/usr/local/bin/spitbol")
(def ^:dynamic *csnobol4-bin* "/usr/local/bin/snobol4")
(def ^:dynamic *timeout-ms* 5000)
(def ^:dynamic *stlimit* 10000)
(defn- normalise [s] (when s (->> (str/split-lines s) (map str/trimr) (reverse) (drop-while str/blank?) (reverse) (str/join "\n"))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-spitbol
    [src]
    (try
        (let [result (sh/sh *spitbol-bin* "-b" "-" :in src :env {"PATH" "/usr/local/bin:/usr/bin:/bin"})]
            (if (zero? (:exit result)) {:stdout (normalise (:out result)) :stderr (normalise (:err result)) :exit 0} {:stdout "" :stderr (normalise (:out result)) :exit (:exit result)}))
        (catch Exception e {:stdout "" :stderr (.getMessage e) :exit :crashed})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-csnobol4
    [src]
    (try
        (let [result (sh/sh *csnobol4-bin* "-" :in src :env {"PATH" "/usr/local/bin:/usr/bin:/bin"})]
            (if
                (zero? (:exit result))
                {:stdout (normalise (:out result)) :stderr (normalise (:err result)) :exit 0}
                {:stdout "" :stderr (normalise (str (:out result) (:err result))) :exit (:exit result)}))
        (catch Exception e {:stdout "" :stderr (.getMessage e) :exit :crashed})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- reset-runtime! [] (env/GLOBALS) (reset! env/STNO 0) (reset! env/<STNO> {}) (reset! env/<LABL> {}) (reset! env/<CODE> {}) (reset! env/<CHANNELS> {}) (reset! env/<OPSYN> {}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-clojure
    [src]
    (try
        (reset-runtime!)
        (let [
              stdout-p (promise)
              f (future (deliver stdout-p (with-out-str (try (sno/RUN (sno/CODE src)) (catch clojure.lang.ExceptionInfo e (when-not (= (get (ex-data e) :snobol/signal) :end) (throw e)))))))]
            (if-let [stdout (deref stdout-p *timeout-ms* nil)] {:stdout (normalise stdout) :stderr "" :exit :ok} (do (future-cancel f) {:stdout "" :stderr "timeout" :exit :timeout})))
        (catch clojure.lang.ExceptionInfo e {:stdout "" :stderr (.getMessage e) :exit :error :thrown (str (class e) ": " (.getMessage e))})
        (catch Exception e {:stdout "" :stderr (.getMessage e) :exit :error :thrown (str (class e) ": " (.getMessage e))})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- oracle-stdout
    [sp cs]
    (let [sp-ok (zero? (:exit sp)) cs-ok (zero? (:exit cs))]
        (cond
            (and sp-ok cs-ok (= (:stdout sp) (:stdout cs))) {:stdout (:stdout sp) :oracle :both}
            (and sp-ok cs-ok (not= (:stdout sp) (:stdout cs))) {:stdout (:stdout sp) :oracle :disagree}
            sp-ok {:stdout (:stdout sp) :oracle :spitbol}
            cs-ok {:stdout (:stdout cs) :oracle :csnobol4}
            :else {:stdout "" :oracle :both-error})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- classify
    [oracle-out cl]
    (cond
        (#{:timeout :step-limit} (:exit cl)) :timeout
        (= :both-error (:oracle oracle-out)) (if (= (:exit cl) :error) :pass-class :skip)
        (= (:stdout oracle-out) (:stdout cl)) :pass
        (and (not= (:oracle oracle-out) :both) (not= (:oracle oracle-out) :spitbol) (not= (:oracle oracle-out) :csnobol4) (= (:exit cl) :error)) :pass-class
        (and (= :both-error (:oracle oracle-out)) (= (:exit cl) :error)) :pass-class
        :else :fail))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn diff-run
    ([src] (diff-run src nil))
    ([src depth]
        (let [sp (run-spitbol src) cs (run-csnobol4 src) cl (run-clojure src) oracle (oracle-stdout sp cs) status (classify oracle cl)]
            {:src src :spitbol sp :csnobol4 cs :clojure cl :oracle (:oracle oracle) :status status :length (count src) :depth depth})))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn save-corpus!
    [records]
    (let [path "resources/golden-corpus.edn"]
        (clojure.java.io/make-parents path)
        (with-open [w (clojure.java.io/writer path :append true)] (doseq [r records] (.write w (pr-str r)) (.newLine w)))
        (count records)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn load-corpus
    []
    (let [path "resources/golden-corpus.edn"]
        (when
            (.exists (clojure.java.io/file path))
            (with-open [r (java.io.PushbackReader. (clojure.java.io/reader path))]
                (loop [acc []] (let [form (try (read r false ::eof) (catch Exception _ ::eof))] (if (= form ::eof) acc (recur (conj acc form)))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-csnobol4-to-step [src n] (run-csnobol4 (str "        &STLIMIT = " n "\n" src)))
(defn run-spitbol-to-step [src n] (run-spitbol (str "        &STLIMIT = " n "\n" src)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-clojure-to-step
    [src n]
    (reset-runtime!)
    (reset! env/&STLIMIT n)
    (try
        (let [stdout (with-out-str (try (sno/RUN (sno/CODE src)) (catch clojure.lang.ExceptionInfo e (when-not (#{:end :step-limit} (get (ex-data e) :snobol/signal)) (throw e)))))]
            {:exit (if (> @env/&STCOUNT n) :step-limit :ok) :steps @env/&STCOUNT :stdout (normalise stdout) :vars (env/snapshot!)})
        (catch Exception e {:exit :error :thrown (.getMessage e) :vars (env/snapshot!)})
        (finally (reset! env/&STLIMIT 2147483647))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def int-vars '[I J K L M N])
(def real-vars '[A B C D E F])
(def str-vars '[S T X Y Z])
(def pat-vars '[P Q R])
(def int-lits [1 2 3 4 5 6 7 8 9 10 25 100])
(def real-lits [1.0 1.5 2.0 2.5 3.0 3.14 0.5 10.0])
(def str-lits ["'alpha'" "'beta'" "'gamma'" "'hello'" "'world'" "'foo'" "'bar'" "'baz'" "'SNOBOL'" "'test'"])
(def pat-lits ["'a'" "'e'" "'o'" "'l'" "'o'" "ANY('aeiou')" "SPAN('abcdefghijklmnopqrstuvwxyz')" "LEN(1)" "LEN(2)" "LEN(3)"])
(def arith-ops ["+" "-" "*"])
(def cmp-ops-int ["EQ" "NE" "GT" "LT" "GE" "LE"])
(def cmp-ops-str ["IDENT" "DIFFER"])
(defn- indent [body] (str "        " body))
(defn- labelled [lbl body] (let [pad (max 1 (- 8 (count (name lbl))))] (str (name lbl) (apply str (repeat pad " ")) body)))
(defn- goto-s [lbl] (str " :S(" (name lbl) ")"))
(defn- goto-f [lbl] (str " :F(" (name lbl) ")"))
(defn- goto-sf [sl fl] (str " :S(" (name sl) ")F(" (name fl) ")"))
(defn- fresh-state [] {:lines [] :live-int #{} :live-real #{} :live-str #{} :live-pat #{} :labels #{} :next-lbl 1})
(defn- next-label [st] (let [n (:next-lbl st) lbl (symbol (str "L" n))] [lbl (update st :next-lbl inc)]))
(defn- emit [st & lines] (update st :lines into lines))
(defn- live? [st pool] (seq (get st pool)))
(defn- move-assign-int [st rng] (let [v (rand-nth int-vars) lit (rand-nth int-lits)] (-> st (emit (indent (str v " = " lit))) (update :live-int conj v))))
(defn- move-assign-real [st rng] (let [v (rand-nth real-vars) lit (rand-nth real-lits)] (-> st (emit (indent (str v " = " lit))) (update :live-real conj v))))
(defn- move-assign-str [st rng] (let [v (rand-nth str-vars) lit (rand-nth str-lits)] (-> st (emit (indent (str v " = " lit))) (update :live-str conj v))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- move-arith
    [st rng]
    (let [
          op (rand-nth arith-ops)
          lhs (if (and (live? st :live-int) (< (rand) 0.6)) (str (rand-nth (vec (:live-int st)))) (str (rand-nth int-lits)))
          rhs (str (rand-nth int-lits))
          v (rand-nth int-vars)]
        (-> st (emit (indent (str v " = " lhs " " op " " rhs))) (update :live-int conj v))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- move-concat
    [st rng]
    (let [
          live (vec (:live-str st))
          lhs (if (and (seq live) (< (rand) 0.6)) (str (rand-nth live)) (rand-nth str-lits))
          rhs (if (and (seq live) (< (rand) 0.5)) (str (rand-nth live)) (rand-nth str-lits))
          v (rand-nth str-vars)]
        (-> st (emit (indent (str v " = " lhs " " rhs))) (update :live-str conj v))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- move-output-int [st rng] (when (live? st :live-int) (let [v (rand-nth (vec (:live-int st)))] (emit st (indent (str "OUTPUT = " v))))))
(defn- move-output-str [st rng] (let [src (if (and (live? st :live-str) (< (rand) 0.7)) (str (rand-nth (vec (:live-str st)))) (rand-nth str-lits))] (emit st (indent (str "OUTPUT = " src)))))
(defn- move-output-lit [st rng] (emit st (indent (str "OUTPUT = " (rand-nth str-lits)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- move-cmp-branch
    [st rng]
    (when
        (live? st :live-int)
        (let [v (rand-nth (vec (:live-int st))) op (rand-nth cmp-ops-int) lit (rand-nth int-lits) [ls st] (next-label st) [lf st] (next-label st) [lskip st] (next-label st)]
            (-> st
                (emit (indent (str op "(" v "," lit ")" (goto-sf ls lf))))
                (emit (labelled ls (str "OUTPUT = '" (name ls) " branch'")))
                (emit (indent (str ":(" (name lskip) ")")))
                (emit (labelled lf (str "OUTPUT = '" (name lf) " branch'")))
                (emit (labelled lskip ""))
                (update :labels conj ls lf lskip)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- move-pat-assign [st rng] (let [v (rand-nth pat-vars) lit (rand-nth pat-lits)] (-> st (emit (indent (str v " = " lit))) (update :live-pat conj v))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- move-pat-match
    [st rng]
    (let [
          subj (if (and (live? st :live-str) (< (rand) 0.7)) (str (rand-nth (vec (:live-str st)))) (rand-nth str-lits))
          pat (if (and (live? st :live-pat) (< (rand) 0.6)) (str (rand-nth (vec (:live-pat st)))) (rand-nth pat-lits))
          [ls st] (next-label st)
          [lf st] (next-label st)
          [lskip st] (next-label st)]
        (-> st
            (emit (indent (str subj " " pat (goto-sf ls lf))))
            (emit (labelled ls "OUTPUT = 'matched'"))
            (emit (indent (str ":(" (name lskip) ")")))
            (emit (labelled lf "OUTPUT = 'no match'"))
            (emit (labelled lskip ""))
            (update :labels conj ls lf lskip))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- move-pat-replace
    [st rng]
    (when
        (live? st :live-str)
        (let [v (rand-nth (vec (:live-str st))) pat (if (and (live? st :live-pat) (< (rand) 0.5)) (str (rand-nth (vec (:live-pat st)))) (rand-nth pat-lits)) repl (rand-nth str-lits)]
            (-> st (emit (indent (str v " " pat " = " repl))) (emit (indent (str "OUTPUT = " v)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- move-size [st rng] (when (live? st :live-str) (let [v (rand-nth (vec (:live-str st)))] (emit st (indent (str "OUTPUT = SIZE(" v ")"))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- move-loop
    [st rng]
    (let [limit (rand-nth [3 4 5]) counter 'I [lloop st] (next-label st) [lend st] (next-label st)]
        (-> st
            (emit (indent (str counter " = 1")))
            (emit (labelled lloop (str "OUTPUT = " counter)))
            (emit (indent (str counter " = " counter " + 1")))
            (emit (indent (str "LE(" counter "," limit ") :S(" (name lloop) ")")))
            (update :live-int conj counter)
            (update :labels conj lloop lend))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private all-moves
    [{:w 10 :needs nil :fn move-assign-int}
        {:w 8 :needs nil :fn move-assign-real}
        {:w 10 :needs nil :fn move-assign-str}
        {:w 6 :needs nil :fn move-output-lit}
        {:w 5 :needs nil :fn move-pat-assign}
        {:w 8 :needs :live-int :fn move-arith}
        {:w 8 :needs :live-str :fn move-concat}
        {:w 7 :needs :live-int :fn move-output-int}
        {:w 7 :needs :live-str :fn move-output-str}
        {:w 5 :needs :live-int :fn move-cmp-branch}
        {:w 4 :needs :live-str :fn move-pat-match}
        {:w 4 :needs :live-str :fn move-pat-replace}
        {:w 3 :needs :live-str :fn move-size}
        {:w 3 :needs nil :fn move-loop}])
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- eligible-moves [st] (filter (fn [{:keys [needs]}] (or (nil? needs) (live? st needs))) all-moves))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- weighted-rand
    [moves]
    (let [total (reduce + (map :w moves)) r (* (rand) total)]
        (loop [remaining moves acc 0] (let [{:keys [w fn]} (first remaining) acc (+ acc w)] (if (or (>= acc r) (= 1 (count remaining))) fn (recur (rest remaining) acc))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- finalise [st] (str/join "\n" (conj (:lines st) "end")))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn rand-program
    ([] (rand-program (+ 3 (rand-int 6))))
    ([n-moves] (loop [st (fresh-state) n n-moves] (if (zero? n) (finalise st) (let [moves (eligible-moves st) move-fn (weighted-rand moves) new-st (move-fn st nil)] (recur (or new-st st) (dec n)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn gen-assign-int [] (for [v int-vars lit int-lits] (str (indent (str v " = " lit)) "\n" (indent (str "OUTPUT = " v)) "\n" "end")))
(defn gen-assign-str [] (for [v str-vars lit str-lits] (str (indent (str v " = " lit)) "\n" (indent (str "OUTPUT = " v)) "\n" "end")))
(defn gen-arith [] (for [v int-vars op arith-ops lhs int-lits rhs (filter pos? int-lits)] (str (indent (str v " = " lhs " " op " " rhs)) "\n" (indent (str "OUTPUT = " v)) "\n" "end")))
(defn gen-concat [] (for [v str-vars s1 str-lits s2 str-lits :when (not= s1 s2)] (str (indent (str v " = " s1 " " s2)) "\n" (indent (str "OUTPUT = " v)) "\n" "end")))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn gen-pat-match
    []
    (for [s str-lits p pat-lits]
        (str (indent (str "S = " s)) "\n" (indent (str "S " p " :S(HIT)F(MISS)")) "\n" "HIT     OUTPUT = 'matched'\n" "        :(DONE)\n" "MISS    OUTPUT = 'no match'\n" "DONE\n" "end")))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn gen-cmp
    []
    (for [op cmp-ops-int lhs int-lits rhs int-lits :when (not= lhs rhs)]
        (str (indent (str op "(" lhs "," rhs ") :S(YES)F(NO)")) "\n" "YES     OUTPUT = 'yes'\n" "        :(DONE)\n" "NO      OUTPUT = 'no'\n" "DONE\n" "end")))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn rand-batch [n] (repeatedly n rand-program))
(defn systematic-batch [] (concat (gen-assign-int) (gen-assign-str) (gen-arith) (gen-concat) (gen-cmp) (gen-pat-match)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private canonical-fixtures
    (str/join "\n"
        ["        I = 0" "        J = 1" "        K = 2" "        L = 3" "        M = 4" "        N = 5" "        S = 'hello'" "        T = 'world'" "        X = 'foo'" "        Y = 'bar'"
            "        Z = 'baz'"]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- with-fixtures [& body-lines] (str canonical-fixtures "\n" (str/join "\n" body-lines) "\n" "END"))
(defn- band-assign-int [] (for [v int-vars lit int-lits] [(indent (str v " = " lit)) (indent (str "OUTPUT = " v))]))
(defn- band-assign-str [] (for [v str-vars lit str-lits] [(indent (str v " = " lit)) (indent (str "OUTPUT = " v))]))
(defn- band-arith [] (for [v int-vars op arith-ops lhs int-lits rhs (filter pos? int-lits)] [(indent (str v " = " lhs " " op " " rhs)) (indent (str "OUTPUT = " v))]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- band-cmp
    []
    (for [op cmp-ops-int lhs int-lits rhs int-lits :when (not= lhs rhs)]
        [(indent (str op "(" lhs "," rhs ") :S(YES)F(NO)")) "YES     OUTPUT = 'yes'" "        :(DONE)" "NO      OUTPUT = 'no'" "DONE"]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- band-concat [] (for [v str-vars s1 str-lits s2 str-lits :when (not= s1 s2)] [(indent (str v " = " s1 " " s2)) (indent (str "OUTPUT = " v))]))
(defn- band-size [] (for [v str-vars] [(indent (str "OUTPUT = SIZE(" v ")"))]))
(defn- band-pat-match [] (for [s str-vars p pat-lits] [(indent (str s " " p " :S(HIT)F(MISS)")) "HIT     OUTPUT = 'matched'" "        :(DONE)" "MISS    OUTPUT = 'no match'" "DONE"]))
(defn- band-pat-replace [] (for [v str-vars p pat-lits repl str-lits] [(indent (str v " " p " = " repl)) (indent (str "OUTPUT = " v))]))
(defn- band-loop [] (for [limit [3 4 5]] ["        I = 1" (str "LOOP    OUTPUT = I") "        I = I + 1" (str "        LE(I," limit ") :S(LOOP)")]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- band-ident
    []
    (for [s1 str-lits s2 str-lits :when (not= s1 s2)] [(indent (str "IDENT(" s1 "," s2 ") :S(YES)F(NO)")) "YES     OUTPUT = 'same'" "        :(DONE)" "NO      OUTPUT = 'different'" "DONE"]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- band-len-match [] (for [n [1 2 3 4 5] v str-vars] [(indent (str v " LEN(" n ") :S(HIT)F(MISS)")) "HIT     OUTPUT = 'matched'" "        :(DONE)" "MISS    OUTPUT = 'no match'" "DONE"]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- band-any-match
    []
    (for [cs ["'aeiou'" "'bcdfg'" "'0123456789'"] v str-vars] [(indent (str v " ANY(" cs ") :S(HIT)F(MISS)")) "HIT     OUTPUT = 'matched'" "        :(DONE)" "MISS    OUTPUT = 'no match'" "DONE"]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- band-span-match
    []
    (for [cs ["'abcdefghijklmnopqrstuvwxyz'" "'0123456789'" "'helo'"] v str-vars]
        [(indent (str v " SPAN(" cs ") :S(HIT)F(MISS)")) "HIT     OUTPUT = 'matched'" "        :(DONE)" "MISS    OUTPUT = 'no match'" "DONE"]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- band-break-match [] (for [cs ["'o'" "'l'" "'d'"] v str-vars] [(indent (str v " BREAK(" cs ") . T :S(HIT)F(MISS)")) "HIT     OUTPUT = T" "        :(DONE)" "MISS    OUTPUT = 'no match'" "DONE"]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- band-capture
    []
    (for [v str-vars p ["LEN(3)" "LEN(2)" "SPAN('abcdefghijklmnopqrstuvwxyz')" "ANY('aeiou')"]]
        [(indent (str v " " p " . T :S(HIT)F(MISS)")) "HIT     OUTPUT = T" "        :(DONE)" "MISS    OUTPUT = 'no match'" "DONE"]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- band-trim [] (for [v str-vars] [(indent (str "OUTPUT = TRIM(" v ")"))]))
(defn- band-substr-cap [] (for [v str-vars] [(indent (str v " LEN(3) $ T :S(HIT)F(MISS)")) "HIT     OUTPUT = T" "        :(DONE)" "MISS    OUTPUT = 'no match'" "DONE"]))
(defn- band-define-simple [] [["        DEFINE('DOUBLE(X)')" "        I = DOUBLE(5)" "        OUTPUT = I" "        :(END)" "DOUBLE  DOUBLE = X * 2 :S(RETURN)F(FRETURN)" "END"]])
(defn- bodies->programs [bodies] (map (fn [lines] (apply with-fixtures lines)) bodies))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn gen-by-length
    []
    (let [all-bodies
              (concat
                  (band-assign-int)
                  (band-assign-str)
                  (band-cmp)
                  (band-ident)
                  (band-arith)
                  (band-concat)
                  (band-size)
                  (band-trim)
                  (band-len-match)
                  (band-any-match)
                  (band-span-match)
                  (band-break-match)
                  (band-capture)
                  (band-substr-cap)
                  (band-pat-match)
                  (band-pat-replace)
                  (band-loop))
          programs (bodies->programs all-bodies)
          standalone (map first (band-define-simple))]
        (->> (concat programs standalone) (sort-by count) (dedupe))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn gen-by-length-annotated
    []
    (map
        (fn [src] {:src src :length (count src) :band (cond (<= (count src) 80) 0 (<= (count src) 140) 1 (<= (count src) 200) 2 (<= (count src) 300) 3 (<= (count src) 500) 4 :else 5)})
        (gen-by-length)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn rand-statement
    []
    (let [forms
              [(fn [] (indent (str (rand-nth int-vars) " = " (rand-nth int-lits))))
                  (fn [] (indent (str (rand-nth str-vars) " = " (rand-nth str-lits))))
                  (fn [] (let [op (rand-nth arith-ops) lhs (rand-nth int-lits) rhs (rand-nth (filter pos? int-lits))] (indent (str (rand-nth int-vars) " = " lhs " " op " " rhs))))
                  (fn [] (indent (str (rand-nth str-vars) " = " (rand-nth str-lits) " " (rand-nth str-lits))))
                  (fn [] (indent (str "OUTPUT = " (rand-nth str-vars))))
                  (fn [] (indent (str "OUTPUT = " (rand-nth int-vars))))
                  (fn
                      []
                      (let [op (rand-nth cmp-ops-int) a (rand-nth int-lits) b (rand-nth int-lits)]
                          (str (indent (str op "(" a "," b ") :S(YES)F(NO)")) "\n" "YES     OUTPUT = 'yes'\n" "        :(DONE)\n" "NO      OUTPUT = 'no'\n" "DONE")))
                  (fn
                      []
                      (let [a (rand-nth str-lits) b (rand-nth str-lits)]
                          (str (indent (str "IDENT(" a "," b ") :S(YES)F(NO)")) "\n" "YES     OUTPUT = 'same'\n" "        :(DONE)\n" "NO      OUTPUT = 'different'\n" "DONE")))
                  (fn [] (indent (str "OUTPUT = SIZE(" (rand-nth str-vars) ")")))
                  (fn
                      []
                      (let [v (rand-nth str-vars) p (rand-nth pat-lits)]
                          (str (indent (str v " " p " :S(HIT)F(MISS)")) "\n" "HIT     OUTPUT = 'matched'\n" "        :(DONE)\n" "MISS    OUTPUT = 'no match'\n" "DONE")))
                  (fn
                      []
                      (let [v (rand-nth str-vars) p (rand-nth ["LEN(2)" "LEN(3)" "ANY('aeiou')"])]
                          (str (indent (str v " " p " . T :S(HIT)F(MISS)")) "\n" "HIT     OUTPUT = T\n" "        :(DONE)\n" "MISS    OUTPUT = 'no match'\n" "DONE")))
                  (fn [] (let [v (rand-nth str-vars) p (rand-nth pat-lits) r (rand-nth str-lits)] (str (indent (str v " " p " = " r)) "\n" (indent (str "OUTPUT = " v)))))
                  (fn [] (indent (str "OUTPUT = TRIM(" (rand-nth str-vars) ")")))
                  (fn [] (let [n (rand-nth [3 4 5])] (str "        I = 1\n" "LOOP    OUTPUT = I\n" "        I = I + 1\n" (indent (str "LE(I," n ") :S(LOOP)")))))]
          body ((rand-nth forms))]
        (str canonical-fixtures "\n" body "\n" "END")))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn gen-error-class-programs
    []
    (concat
        (take 20 (gen-by-length))
        [(str canonical-fixtures "\n" "        I = 5 / 0\n" "        OUTPUT = I\n" "END")
            (str canonical-fixtures "\n" "        I = 10\n" "        J = 0\n" "        K = I / J\n" "        OUTPUT = K\n" "END")]
        [(str canonical-fixtures "\n" "        I = 1\n" "        I EQ(I,1) :S(NOWHERE)\n" "END") (str canonical-fixtures "\n" "        :(UNDEFINED_LABEL)\n" "END")]
        ["        = bad syntax\nEND" "NOLABEL\nEND"]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-worm-batch
    [n source-fn]
    (require '[SNOBOL4clojure.harness :as harness])
    (let [
          programs (repeatedly n source-fn)
          records (mapv (fn [src] (try ((resolve 'SNOBOL4clojure.harness/diff-run) src) (catch Exception e {:src src :status :error :thrown (.getMessage e)}))) programs)
          summary (reduce (fn [acc r] (update acc (get r :status :error) (fnil inc 0))) {} records)
          failures (filterv #(= :fail (:status %)) records)]
        ((resolve 'SNOBOL4clojure.harness/save-corpus!) records)
        {:records records :summary summary :failures failures}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-systematic-batch
    []
    (require '[SNOBOL4clojure.harness :as harness])
    (let [
          programs (vec (gen-by-length))
          _ (println (str "Running " (count programs) " systematic programs..."))
          records
              (vec
                  (map-indexed
                      (fn
                          [i src]
                          (when (zero? (mod i 50)) (println (str "  " i "/" (count programs) "...")))
                          (try ((resolve 'SNOBOL4clojure.harness/diff-run) src) (catch Exception e {:src src :status :error :thrown (.getMessage e)})))
                      programs))
          summary (reduce (fn [acc r] (update acc (get r :status :error) (fnil inc 0))) {} records)
          failures (filterv #(= :fail (:status %)) records)]
        ((resolve 'SNOBOL4clojure.harness/save-corpus!) records)
        {:records records :summary summary :failures failures :total (count programs)}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- safe-name [s] (-> s (str/replace #"[^a-zA-Z0-9_]" "_") (str/replace #"^[0-9]" "x")))
(defn- quote-lines [src] (map (fn [line] (str "    " (pr-str line))) (str/split-lines src)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn corpus-record->deftest
    [record index]
    (let [
          {:keys [src status oracle spitbol csnobol4]} record
          oracle-out (case oracle :both (:stdout spitbol) :spitbol (:stdout spitbol) :csnobol4 (:stdout csnobol4) :disagree (:stdout spitbol) :both-error nil nil)
          test-name (symbol (str "worm_auto_" (format "%04d" index)))
          lines (quote-lines src)]
        (case status
            (:pass :fail)
                (str "(deftest " test-name "\n" "  (let [r (run-with-timeout\n" "           (str/join \"\\n\"\n" "             ["
                    (str/join "\n              " lines)
                    "]))\n" "              2000)]\n" "    (is (= "
                    (pr-str (or oracle-out ""))
                    "\n" "           (str/trim-newline (or (:stdout r) \"\"))))))\n")
            (str ";; " (name status) " — skipped (oracle: " oracle ")\n" ";; (deftest " test-name " ...)\n"))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn emit-regression-tests
    [records ns-name]
    (str "(ns " ns-name "\n" "  \"Auto-generated regression tests from worm corpus.\n" "   DO NOT EDIT — regenerate with (emit-regression-tests ...)\"\n" "  (:require [clojure.test :refer :all]\n"
        "            [clojure.string :as str]\n" "            [SNOBOL4clojure.core :refer :all]\n" "            [SNOBOL4clojure.test-helpers :refer [run-with-timeout]]))\n\n" "(GLOBALS *ns*)\n"
        "(use-fixtures :each (fn [f] (GLOBALS (find-ns (quote " ns-name "))) (f)))\n\n"
        (str/join "\n" (map-indexed corpus-record->deftest records))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- mk-ns [sym] (or (find-ns sym) (create-ns sym)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- reset-to!
    [ns-sym]
    (env/GLOBALS (mk-ns ns-sym))
    (reset! env/STNO 0)
    (reset! env/<STNO> {})
    (reset! env/<LABL> {})
    (reset! env/<CODE> {})
    (reset! env/<FUNS> {})
    (reset! env/<CHANNELS> {})
    (reset! env/&STCOUNT 0))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-interpreter
    [src]
    (reset-to! 'snobol4.bench.interp)
    (with-out-str (try (rt/RUN (comp/CODE src)) (catch clojure.lang.ExceptionInfo e (when-not (#{:end :return :freturn :nreturn} (:snobol/signal (ex-data e))) (throw e))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn run-jvm [src] (rt/run-jvm-captured! src))
(def ^:private SDIR "bench")
(defn- include-src [filename] (comp/preprocess-includes (str "-INCLUDE '" filename "'") [SDIR]))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def PROGRAMS
    [{:id "arith-10k" :label "Arithmetic loop (I=0..10000, LT branch)" :src "        I = 0\nLOOP    I = I + 1\n        LT(I,10000)   :S(LOOP)\n        OUTPUT = I\nEND\n" :reps 30}
        {
         :id "strcat-500"
         :label "String concat (500 iters, S grows to 500 chars)"
         :src "        S = ''\n        I = 0\nLOOP    S = S 'x'\n        I = I + 1\n        LT(I,500)   :S(LOOP)\n        OUTPUT = SIZE(S)\nEND\n"
         :reps 30}
        {
         :id "pat-span"
         :label "Pattern: SPAN match loop (1000 iterations)"
         :src
             (str "        P = SPAN('aeiou')\n        S = 'hello world foo bar'\n" "        I = 0\nLOOP    S P                  :F(MISS)\n" "        I = I + 1\n        LT(I,1000) :S(LOOP)\n"
                 "MISS    OUTPUT = I\nEND\n")
         :reps 30}
        {:id "fact45" :label "Factorial 1..45 (big-number string arithmetic)" :src (include-src "testpgms-test3.spt") :reps 5}])
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn time-fn
    [f reps]
    (try
        (f)
        (let [t0 (System/nanoTime)] (dotimes [_ reps] (f)) (/ (double (- (System/nanoTime) t0)) (* reps 1e6)))
        (catch Exception e (binding [*out* *err*] (println "  BENCH-ERR:" (.getMessage e))) nil)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn bench-program
    [{:keys [id label src reps]}]
    (println (str "\n── " id ": " label " (n=" reps ")"))
    (let [
          sp (time-fn #(run-spitbol src) reps)
          cs (time-fn #(run-csnobol4 src) reps)
          i (time-fn #(run-interpreter src) reps)
          j (time-fn #(run-jvm src) reps)
          fmt (fn [ms ref] (if ms (str (format "%8.2f ms" ms) (if ref (format "  (%5.1fx)" (/ (double ref) ms)) "           ")) "      N/A           "))]
        (println (format "   %-30s %s" "SPITBOL v4.0f" (fmt sp nil)))
        (println (format "   %-30s %s" "CSNOBOL4 2.3.3" (fmt cs nil)))
        (println (format "   %-30s %s" "SNOBOL4clj interpreter" (fmt i i)))
        (println (format "   %-30s %s" "SNOBOL4clj JVM codegen" (fmt j i)))
        {:id id :spitbol sp :csnobol4 cs :interp i :jvm j}))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn print-grid
    [results]
    (let [f #(if % (format "%8.2f" %) "     N/A")]
        (println)
        (println "╔═══════════════════════╦══════════╦══════════╦══════════╦══════════╗")
        (println "║ Program               ║ SPITBOL  ║ CSNOBOL4 ║ Interp   ║ JVM code ║")
        (println "╠═══════════════════════╬══════════╬══════════╬══════════╬══════════╣")
        (doseq [{:keys [id spitbol csnobol4 interp jvm]} results] (println (format "║ %-21s ║%s ║%s ║%s ║%s ║" (subs id 0 (min 21 (count id))) (f spitbol) (f csnobol4) (f interp) (f jvm))))
        (println "╚═══════════════════════╩══════════╩══════════╩══════════╩══════════╝")
        (println)
        (println "Ratios vs Clojure interpreter shown per-program above.")
        (println "SPITBOL/CSNOBOL4 include process-spawn overhead (~15ms/run).")
        (println (str "Platform: " (System/getProperty "java.vm.name") " " (System/getProperty "java.version") "  Date: " (java.time.LocalDate/now)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn -main
    [& _]
    (println "\nSNOBOL4 Backend Benchmark")
    (println "=========================")
    (println "Warming up Clojure backends...")
    (let [w "        I = 0\nLOOP    I = I + 1\n        LT(I,100) :S(LOOP)\nEND\n"] (dotimes [_ 3] (run-interpreter w) (run-jvm w)))
    (println "Warmup done.\n")
    (let [results (mapv bench-program PROGRAMS)] (print-grid results))
    (println "\nDone.")
    (System/exit 0))
