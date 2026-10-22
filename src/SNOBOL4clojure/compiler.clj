(ns SNOBOL4clojure.compiler
  "CODE! / CODE: compile SNOBOL4 source text into a labeled statement table
   and load it into the runtime's global statement store.

   ## Stage 23A — Pre-compiled EDN cache

   The IR produced by CODE! is pure EDN: persistent maps, lists, keywords,
   primitives.  Serialising with pr-str and reloading with edn/read-string
   is lossless — the grammar and instaparse pipeline never run again.

   Benchmark (session 13): CODE! costs ~10.93ms per program — 95% of the
   total CODE+RUN cost of ~11.45ms for short programs.  Caching eliminates
   this entirely for repeated runs of the same source.

   ### Disk cache API
     (compile-to-file src path)   — compile src, write EDN to path
     (load-ir path)               — load [codes nos labels] from EDN file
     (CODE-ir ir)                 — like CODE but takes pre-compiled IR triple
     (CODE-cached src path)       — load from path if exists, else compile+save

   ### In-memory (memoised) cache API — fastest for test suites
     (CODE-memo src)              — compile once per unique source string
     (clear-memo!)                — evict all entries
     (memo-stats)                 — {:size N :hits N :misses N}

   ### Low-level serialisation
     (ir->edn ir)                 — [codes nos labels] -> EDN string
     (edn->ir s)                  — EDN string -> [codes nos labels]"
  (:require [clojure.string          :as string]
            [clojure.edn             :as edn]
            [SNOBOL4clojure.env      :refer [STNO <STNO> <LABL> <CODE> <DISPNO> <POS> BLANKS &CASE fold-case]]
            [SNOBOL4clojure.grammar  :refer [parse-statement]]
            [SNOBOL4clojure.emitter  :refer [emitter]]))

;; -- Source tokenizer ---------------------------------------------------------
(defn- comment? [cmd] (re-find #"^\*" cmd))
(defn- control? [cmd] (re-find #"^\-" cmd))
;; A -CASE card sets whether the statements after it are compiled with lower case names folded to upper case: -CASE N folds when N is not 0, -CASE alone or -CASE 0 does not.
;; The card's own name follows the mode: with folding on it may be written in any case, with folding off only as -CASE.
(defn- case-card [fold command]
  (when-let [[_ n] (re-find (if fold #"(?i)^-CASE(?:\s+(\d+))?\s*$" #"^-CASE(?:\s+(\d+))?\s*$") command)]
    (boolean (and n (not (zero? (Long/parseLong n)))))))

(defn- error-node [info] (list 'ERROR (:line info) (:column info) (:text info)))

(defn- split-source
  "Split SNOBOL4 source into raw statement strings, each with the physical line it begins on, as [statement line]. A blank physical line is a statement of its own as
   SPITBOL numbers them, and is [:blank line]. ls is where the physical line being read begins, line is its number and sl the line the statement being read began on."
  [src]
  (let [chars (if (string/ends-with? (str src) "\n") (str src) (str src "\n"))
        n     (count chars)
        blank-line? (fn [from to] (every? #(Character/isWhitespace ^char %) (subs chars from to)))]
    (loop [i 0 cur (StringBuilder.) stmts [] in-sq false in-dq false ls 0 line 1 sl 1]
      (if (>= i n)
        (let [s (string/trimr (str cur))]
          (if (seq s) (conj stmts [s sl]) stmts))
        (let [ch (.charAt chars i)
              sl (if (zero? (.length cur)) line sl)]
          (cond
            (and (= ch \') (not in-dq))
            (recur (inc i) (.append cur ch) stmts (not in-sq) in-dq ls line sl)
            (and (= ch \") (not in-sq))
            (recur (inc i) (.append cur ch) stmts in-sq (not in-dq) ls line sl)
            ;; a comment line (* in column 1) is one comment: a semicolon in it separates nothing
            (and (= ch \;) (not in-sq) (not in-dq) (not (and (pos? (.length cur)) (= \* (.charAt cur 0)))))
            (let [s (string/trimr (str cur))]
              (recur (inc i) (StringBuilder.) (if (seq s) (conj stmts [s sl]) stmts) false false ls line line))
            (= ch \newline)
            (let [s (string/trimr (str cur))]
              (if (seq s)
                (let [next-i (inc i)
                      next-ch (when (< next-i n) (.charAt chars next-i))]
                  (if (and next-ch (or (= next-ch \+) (= next-ch \.)))
                    (recur (+ i 2) (.append cur \space) stmts false false (inc i) (inc line) sl)
                    (recur (inc i) (StringBuilder.) (conj stmts [s sl]) false false (inc i) (inc line) (inc line))))
                (recur (inc i) (StringBuilder.) (if (blank-line? ls i) (conj stmts [:blank line]) stmts) false false (inc i) (inc line) (inc line))))
            :else
            (recur (inc i) (.append cur ch) stmts in-sq in-dq ls line sl)))))))

;; -- -INCLUDE preprocessor ----------------------------------------------------

(declare preprocess-includes*)

(defn preprocess-includes
  "Recursively expand -INCLUDE 'file' (or -INCLUDE \"file\") directives.

   Scans source line-by-line.  When a line matches (case-insensitive)
     -INCLUDE 'filename'   or   -INCLUDE \"filename\"
   it reads that file relative to each directory in search-path (first match
   wins) and splices its content inline.  The include file is itself
   preprocessed recursively.

   As SPITBOL has it, a file name is included once: a second -INCLUDE of the same name (as written) is skipped, and so is a cycle. The splice is bracketed by
   the cards -FILE-ENTER \"name\" and -FILE-LEAVE (STALE), which CODE! reads to keep &FILE and &LINE. STALE is written when the included file's last line is a comment,
   the case in which SPITBOL leaves the included file's name as &FILE for the statement that follows.

   Parameters:
     src          – SNOBOL4 source string
     search-path  – seq of directory path strings; default [\".\"]
     seen         – (internal) the set of names already included

   Returns the fully expanded source string."
  ([src] (preprocess-includes src ["."]))
  ([src search-path] (preprocess-includes src search-path #{}))
  ([src search-path seen] (first (preprocess-includes* src search-path seen))))

(defn- preprocess-includes*
  "preprocess-includes, returning [expanded-source seen]."
  [src search-path seen]
  (let [include-re #"(?i)^\s*-include\s+['\"]([^'\"]+)['\"]"]
    (loop [lines (string/split-lines src) out [] seen seen]
      (if-let [line (first lines)]
        (if-let [[_ fname] (re-find include-re line)]
          (let [found (some (fn [dir]
                              (let [f (clojure.java.io/file dir (string/trim fname))]
                                (when (.exists f) f)))
                            search-path)]
            (cond
              ;; File not found — keep the directive as a comment so the compiler sees a harmless *-line
              (nil? found)
              (recur (rest lines) (conj out (str "* -INCLUDE not found: " fname)) seen)
              (seen fname)
              (recur (rest lines) (conj out (str "* -INCLUDE skipped (already included): " fname)) seen)
              :else
              (let [text            (slurp found :encoding "ISO-8859-1")
                    ;; the included file's own directory joins the search path
                    [body seen']    (preprocess-includes* text (distinct (cons (.getParent found) search-path)) (conj seen fname))
                    last-line       (last (string/split-lines text))
                    stale?          (boolean (and last-line (string/starts-with? last-line "*")))]
                (recur (rest lines)
                       (into out (concat [(str "-FILE-ENTER \"" (string/trim fname) "\"")] (string/split-lines body) [(str "-FILE-LEAVE" (when stale? " STALE"))]))
                       seen'))))
          (recur (rest lines) (conj out line) seen))
        [(string/join "\n" out) seen]))))

;; The source position a card changes. A -LINE card sets the number and file of the line that follows it; a -FILE-ENTER card (written by preprocess-includes in place of the
;; -INCLUDE card) starts the included file at line 1 and remembers where the including file resumes, which is after the line the -INCLUDE card stood on; the matching
;; -FILE-LEAVE card resumes it, in the including file's name unless it says STALE, which keeps the included file's name for the one statement that
;; follows. A position is {:file f :bp physical-line-of-the-next-line :bl its number :stack [..]}.
(defn- position-card [fold command pline pos]
  (let [card (fn [re] (re-find (if fold (re-pattern (str "(?i)" re)) (re-pattern re)) command))]
    (cond
      (card "^-LINE\\s+\\d+")
      (let [[_ n f] (re-find #"^-\S+\s+(\d+)(?:\s+\"([^\"]*)\")?" command)]
        (assoc pos :file (or f (:file pos)) :revert nil :bp (inc pline) :bl (Long/parseLong n)))
      (card "^-FILE-ENTER\\s+\"")
      (let [[_ f] (re-find #"\"([^\"]*)\"" command)]
        (assoc pos :file f :bp (inc pline) :bl 1 :stack (conj (:stack pos) (assoc pos :card-line (+ (:bl pos) (- pline (:bp pos)))))))
      (card "^-FILE-LEAVE")
      (let [outer (peek (:stack pos))]
        (let [stale (boolean (re-find #"STALE" command))]
          (assoc outer :file (if stale (:file pos) (:file outer)) :revert (when stale (:file outer)) :bp (inc pline) :bl (inc (:card-line outer)) :stack (pop (:stack pos)))))
      :else pos)))

;; -- CODE! — parse source into {CODES NOS LABELS} ----------------------------
(defn CODE!
  "Compile SNOBOL4 source S into a [codes nos labels disp] vector. disp holds, for each statement in order, how many blank lines (statements SPITBOL
   numbers but never runs) came before it, and the total, as {:before [..] :total n}, and where it stands in its file, as {:pos [[file line] ..]}.
   Optional include-path is a seq of directory strings used to resolve
   -INCLUDE directives (default [\".\"]). file is the name the program was given, which &FILE reads."
  ([S] (CODE! S ["."]))
  ([S include-path] (CODE! S include-path nil))
  ([S include-path file]
   (let [expanded  (preprocess-includes (str S) include-path)
         raw-stmts (split-source expanded)]
     (loop [stmts raw-stmts NO 1 CODES {} NOS {} LABELS {} blanks 0 before [] fold (not (zero? (long @&CASE)))
            poss [] pos {:file (str file) :bp 1 :bl 1 :stack []}]
       (if (empty? stmts)
         [CODES NOS LABELS {:before before :total blanks :pos poss}]
         (let [[command pline] (first stmts)]
           (cond
             (= command :blank) (recur (rest stmts) NO CODES NOS LABELS (inc blanks) before fold poss pos)
             (comment? command) (recur (rest stmts) NO CODES NOS LABELS blanks before fold poss pos)
             (control? command) (recur (rest stmts) NO CODES NOS LABELS blanks before (let [f (case-card fold command)] (if (nil? f) fold f)) poss
                                       (position-card fold command pline pos))
             :else
             (let [stmt   (cond-> (string/replace command #"\r" "") fold fold-case)
                   ast    (parse-statement stmt)
                   code   (emitter ast)
                   here   [(:file pos) (+ (:bl pos) (- pline (:bp pos)))]
                   pos''  (if (:revert pos) (assoc pos :file (:revert pos) :revert nil) pos)]
               (if (and (map? code) (:reason code))
                 (recur (rest stmts) (inc NO) (assoc CODES NO (error-node code)) NOS LABELS blanks (conj before blanks) fold (conj poss here) pos'')
                 (let [label  (:label code)
                       body   (:body  code)
                       goto   (:goto  code)
                       key    (if label label NO)
                       code   (reduce #(conj %1 %2) [] [body goto])
                       nos    (if (keyword? key) (assoc NOS   key NO) NOS)
                       labels (if (keyword? key) (assoc LABELS NO key) LABELS)
                       codes  (assoc CODES key code)]
                   (recur (rest stmts) (inc NO) codes nos labels blanks (conj before blanks) fold (conj poss here) pos'')))))))))))

;; -- CODE — compile and load into the global statement store ------------------
(defn CODE
  "Compile S and load into the global statement store.
   Optional include-path is a seq of directory strings (default [\".\"])."
  ([S] (CODE S ["."]))
  ([S include-path] (CODE S include-path nil))
  ([S include-path file]
   (let [[codes nos labels disp] (CODE! S include-path file)
         start              (inc @STNO)
         base               @BLANKS]
     (loop [NO 1]
       (if (> NO (count codes))
         (do
           (swap! BLANKS + (:total disp 0))
           (if (and (@<LABL> start) (@<CODE> (@<LABL> start)))
             (@<LABL> start)
             (when (@<CODE> start) start)))
         (do
           (swap! STNO inc)
           (swap! <DISPNO> assoc @STNO (+ @STNO base (get (:before disp) (dec NO) 0)))
           (swap! <POS> assoc @STNO (get (:pos disp) (dec NO) ["" 0]))
           (if-let [label (labels NO)]
             (do
               (swap! <CODE> #(assoc % label (codes label)))
               (swap! <LABL> #(assoc % @STNO label))
               (swap! <STNO> #(assoc % label @STNO)))
             (swap! <CODE> #(assoc % @STNO (codes NO))))
           (recur (inc NO))))))))

;; -- Stage 23A: EDN serialisation --------------------------------------------

(defn ir->edn
  "Serialise a CODE! result triple [codes nos labels] to a compact EDN string.
   The IR is pure Clojure data so pr-str is lossless."
  [[codes nos labels disp]]
  (pr-str {:codes codes :nos nos :labels labels :disp disp}))

(defn edn->ir
  "Deserialise an EDN string produced by ir->edn back to [codes nos labels].
   Uses clojure.edn/read-string — safe, no eval."
  [s]
  (let [{:keys [codes nos labels disp]} (edn/read-string s)]
    [codes nos labels disp]))

;; -- Stage 23A: CODE-ir — load pre-compiled IR into statement store -----------

(defn CODE-ir
  "Like CODE, but takes a pre-compiled [codes nos labels] triple.
   Skips grammar and emitter entirely. ~10x faster than CODE for short programs."
  [[codes nos labels disp]]
  (let [start (inc @STNO)
        base  @BLANKS]
    (loop [NO 1]
      (if (> NO (count codes))
        (do
          (swap! BLANKS + (:total disp 0))
          (if (and (@<LABL> start) (@<CODE> (@<LABL> start)))
            (@<LABL> start)
            (when (@<CODE> start) start)))
        (do
          (swap! STNO inc)
          (swap! <DISPNO> assoc @STNO (+ @STNO base (get (:before disp) (dec NO) 0)))
          (swap! <POS> assoc @STNO (get (:pos disp) (dec NO) ["" 0]))
          (if-let [label (labels NO)]
            (do
              (swap! <CODE> #(assoc % label (codes label)))
              (swap! <LABL> #(assoc % @STNO label))
              (swap! <STNO> #(assoc % label @STNO)))
            (swap! <CODE> #(assoc % @STNO (codes NO))))
          (recur (inc NO)))))))

;; -- Stage 23A: Disk cache ---------------------------------------------------

(defn compile-to-file
  "Compile SNOBOL4 source src and write the IR as EDN to path.
   Optional include-path is a seq of directory strings (default [\".\"])
   Returns the [codes nos labels] triple."
  ([src path] (compile-to-file src path ["."]))
  ([src path include-path]
   (let [ir (CODE! src include-path)]
     (clojure.java.io/make-parents path)
     (spit path (ir->edn ir))
     ir)))

(defn load-ir
  "Load a pre-compiled [codes nos labels] triple from an EDN file at path."
  [path]
  (edn->ir (slurp path)))

(defn CODE-cached
  "Compile src and load into the statement store, using a disk EDN cache.
   If path exists: load from disk (skip grammar + emitter).
   If path absent: compile src, save to path, then load.
   Optional include-path is a seq of directory strings (default [\".\"])."
  ([src path] (CODE-cached src path ["."]))
  ([src path include-path]
   (let [ir (if (.exists (clojure.java.io/file path))
               (load-ir path)
               (compile-to-file src path include-path))]
     (CODE-ir ir))))

;; -- Stage 23A: In-memory memoised cache -------------------------------------
;;
;; Fastest for test suites: compile each unique source string once per JVM
;; session.  The lein test suite compiles the same programs on every run;
;; memoisation skips the grammar after the first pass.

(def ^:private memo-cache
  (atom {:cache {} :hits 0 :misses 0}))

(defn CODE-memo
  "Memoised CODE: compile src exactly once per JVM session, then reuse the IR.
   Thread-safe. Drop-in replacement for CODE in test fixtures and harness.
   Optional include-path is a seq of directory strings (default [\".\"])

   Side effects (loading into <CODE>/<STNO>/<LABL>) still happen on every call
   so that reset-runtime! between tests works correctly."
  ([src] (CODE-memo src ["."]))
  ([src include-path]
   ;; Cache key includes include-path so different paths compile separately
   (let [cache-key [src include-path]
         cached    (get-in @memo-cache [:cache cache-key])]
     (if cached
       (do (swap! memo-cache update :hits inc)
           (CODE-ir cached))
       (do (swap! memo-cache update :misses inc)
           (let [ir (CODE! src include-path)]
             (swap! memo-cache update :cache assoc cache-key ir)
             (CODE-ir ir)))))))

(defn clear-memo!
  "Evict all entries from the in-memory IR cache."
  []
  (swap! memo-cache assoc :cache {} :hits 0 :misses 0)
  nil)

(defn memo-stats
  "Return cache statistics: {:size N :hits N :misses N :hit-rate pct}."
  []
  (let [{:keys [cache hits misses]} @memo-cache
        total (+ hits misses)]
    {:size     (count cache)
     :hits     hits
     :misses   misses
     :hit-rate (if (pos? total)
                 (format "%.1f%%" (* 100.0 (/ hits total)))
                 "n/a")}))

;; -- Stage 23A: Corpus pre-compiler ------------------------------------------

(defn precompile-corpus!
  "Pre-compile a sequence of source strings to EDN files under cache-dir.
   Each file is named by the hash of the source string.
   Skips files that already exist (idempotent).
   Returns {:compiled N :skipped N :errors N}.

   Example:
     (require '[SNOBOL4clojure.generator :as gen])
     (precompile-corpus! (gen/gen-by-length) \"resources/ir-cache\")"
  [sources cache-dir]
  (clojure.java.io/make-parents (str cache-dir "/x"))
  (reduce
    (fn [acc src]
      (let [path   (str cache-dir "/" (format "%x" (Math/abs (hash src))) ".ir.edn")
            exists (.exists (clojure.java.io/file path))]
        (if exists
          (update acc :skipped inc)
          (try
            (compile-to-file src path)
            (update acc :compiled inc)
            (catch Exception _
              (update acc :errors inc))))))
    {:compiled 0 :skipped 0 :errors 0}
    sources))
