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
            [SNOBOL4clojure.env      :refer [STNO <STNO> <LABL> <CODE> <DISPNO> <POS> <CODELINE> <DIAGNOSTICS> <SOURCE> BLANKS &CASE fold-case block-end]]
            [SNOBOL4clojure.grammar  :refer [parse-statement]]
            [SNOBOL4clojure.syntax   :refer [syntax-error]]
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
   SPITBOL numbers them, and is [:blank line]. ls is where the physical line being read begins, line is its number and sl the line the statement being read began on.
   A record, the text CODE compiles, is one line: a line end in it is a character as any other, and only a semicolon ends a statement (record? true)."
  ([src] (split-source src false))
  ([src record?]
  ;; the null string is no line at all, so CODE('') compiles no statement (sbl -bf), not a blank one
  (let [chars (if (or record? (= "" (str src)) (string/ends-with? (str src) "\n")) (str src) (str src "\n"))
        n     (count chars)
        ;; a record keeps the line ends at the end of a statement, which are no blanks
        trimr (if record? #(string/replace % #"[ \t]+\z" "") string/trimr)
        blank-line? (fn [from to] (every? #(Character/isWhitespace ^char %) (subs chars from to)))]
    ;; "A continuation mark ... is placed in the first column of the additional lines" (manual ch. 14): a + or . line that continues nothing -- the first line, a
    ;; line after a comment line, or after a statement closed by a semicolon -- is "214 bad label or misplaced continuation line" at compile time, as sbl -bf reports it.
    ;; The diagnostics ride on the result as metadata, {:diagnostics [[line 214] ..]}.
    (loop [i 0 cur (StringBuilder.) stmts [] in-sq false in-dq false ls 0 line 1 sl 1
           diags (if (and (pos? n) (#{\+ \.} (.charAt chars 0))) [[1 214]] [])]
      (if (>= i n)
        (let [s (trimr (str cur))]
          ;; the blanks after a record's last semicolon are a statement (sbl -bf: CODE(' X = 1 ; ') is two statements and the block's closing one)
          (with-meta (cond (seq s) (conj stmts [s sl]) (and record? (pos? (.length cur))) (conj stmts [:blank sl]) :else stmts) {:diagnostics diags}))
        (let [ch (.charAt chars i)
              sl (if (zero? (.length cur)) line sl)]
          (cond
            (and (= ch \') (not in-dq))
            (recur (inc i) (.append cur ch) stmts (not in-sq) in-dq ls line sl diags)
            (and (= ch \") (not in-sq))
            (recur (inc i) (.append cur ch) stmts in-sq (not in-dq) ls line sl diags)
            ;; a comment line (* in column 1) is one comment: a semicolon in it separates nothing. Every statement a semicolon ends is one, an empty or blank one a
            ;; statement SPITBOL numbers and never runs, as a blank line is (sbl -bf: X = 1 ;; Y = 2 is three statements, two executed)
            (and (= ch \;) (not in-sq) (not in-dq) (not (and (pos? (.length cur)) (= \* (.charAt cur 0)))))
            (let [s (trimr (str cur))]
              (recur (inc i) (StringBuilder.) (conj stmts (if (seq s) [s sl] [:blank sl])) false false ls line line diags))
            (and (= ch \newline) (not record?))
            (let [s (string/trimr (str cur))
                  next-i (inc i)
                  next-ch (when (< next-i n) (.charAt chars next-i))
                  cont? (and next-ch (or (= next-ch \+) (= next-ch \.)))]
              (if (seq s)
                (cond
                  ;; a comment is one line long: a continuation after it is misplaced
                  (and cont? (= \* (.charAt s 0)))
                  (recur (inc i) (StringBuilder.) (conj stmts [s sl]) false false (inc i) (inc line) (inc line) (conj diags [(inc line) 214]))
                  cont?
                  (recur (+ i 2) (.append cur \space) stmts false false (inc i) (inc line) sl diags)
                  :else
                  (recur (inc i) (StringBuilder.) (conj stmts [s sl]) false false (inc i) (inc line) (inc line) diags))
                (let [blank? (blank-line? ls i)
                      ;; a source line is read with its trailing blanks gone, but a CODE string, one record with no newline, is not: the blanks after its last
                      ;; semicolon are a statement (sbl -bf: CODE(' X = 1 ; ') is two statements and the block's closing one)
                      tail?  (and (pos? (.length cur)) (= i (dec n)) (not (string/ends-with? (str src) "\n")))]
                  (recur (inc i) (StringBuilder.) (if (or blank? tail?) (conj stmts [:blank line]) stmts) false false (inc i) (inc line) (inc line)
                         ;; the line ended in a semicolon, which closed its last statement: a continuation after it is misplaced
                         (if (and cont? (not blank?)) (conj diags [(inc line) 214]) diags)))))
            :else
            (recur (inc i) (.append cur ch) stmts in-sq in-dq ls line sl diags))))))))

;; A line end in a statement, outside its literals: in a record it is a character SPITBOL's scan takes in a label and refuses anywhere else (syntax.clj).
(defn- line-end-outside-literals? [^String s]
  (loop [i 0 q nil]
    (when (< i (count s))
      (let [c (.charAt s i)]
        (cond (and q (= c q))                    (recur (inc i) nil)
              q                                  (recur (inc i) q)
              (#{\' \"} c)                       (recur (inc i) c)
              (#{\return \newline} c)            true
              :else                              (recur (inc i) nil))))))

;; -- -INCLUDE preprocessor ----------------------------------------------------

(declare preprocess-includes*)

(defn preprocess-includes
  "Recursively expand -INCLUDE 'file' (or -INCLUDE \"file\") directives.

   Scans source line-by-line.  When a line matches (case-insensitive)
     -INCLUDE 'filename'   or   -INCLUDE \"filename\"
   it reads that file relative to each directory in search-path (first match
   wins) and splices its content inline.  The include file is itself
   preprocessed recursively.

   As SPITBOL has it, a file name is included once: a second -INCLUDE of the same name is skipped, and so is a cycle; the names kept are trimmed, so a name
   written with a blank after it is included every time. The splice is bracketed by
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
  "preprocess-includes, returning [expanded-source seen cards], cards the -INCLUDE lines as written, one for each -FILE-ENTER card in order, which the listing prints."
  [src search-path seen]
  (let [include-re #"(?i)^\s*-include\s+['\"]([^'\"]+)['\"]"]
    (loop [lines (string/split-lines src) out [] seen seen cards []]
      (if-let [line (first lines)]
        (if-let [[_ fname] (re-find include-re line)]
          (let [found (some (fn [dir]
                              (let [f (clojure.java.io/file dir (string/trim fname))]
                                (when (.exists f) f)))
                            search-path)]
            (cond
              ;; File not found — keep the directive as a comment so the compiler sees a harmless *-line
              (nil? found)
              (recur (rest lines) (conj out (str "* -INCLUDE not found: " fname)) seen cards)
              ;; "SPITBOL remembers only the 'trimmed' names of files included so far, so the comparison with the blank-padded name will fail, and the file will be
              ;; included again" (manual ch. 14, -INCLUDE): the name as written is looked up among the trimmed names
              (seen fname)
              (recur (rest lines) (conj out (str "* -INCLUDE skipped (already included): " fname)) seen cards)
              :else
              (let [text                (slurp found :encoding "ISO-8859-1")
                    ;; the included file's own directory joins the search path
                    [body seen' inner]  (preprocess-includes* text (distinct (cons (.getParent found) search-path)) (conj seen (string/trim fname)))
                    last-line           (last (string/split-lines text))
                    stale?              (boolean (and last-line (string/starts-with? last-line "*")))]
                (recur (rest lines)
                       (into out (concat [(str "-FILE-ENTER \"" (string/trim fname) "\"")] (string/split-lines body) [(str "-FILE-LEAVE" (when stale? " STALE"))]))
                       seen'
                       (into (conj cards line) inner)))))
          (recur (rest lines) (conj out line) seen cards))
        [(string/join "\n" out) seen cards]))))

;; The source position a card changes. A -LINE card sets the number and file of the line that follows it; a -FILE-ENTER card (written by preprocess-includes in place of the
;; -INCLUDE card) starts the included file at line 1 and remembers where the including file resumes, which is after the line the -INCLUDE card stood on; the matching
;; -FILE-LEAVE card resumes it, in the including file's name unless it says STALE, which keeps the included file's name for the one statement that
;; follows. A position is {:file f :bp physical-line-of-the-next-line :bl its number :stack [..]}.
(defn- position-card [fold command pline pos]
  (let [card (fn [re] (re-find (if fold (re-pattern (str "(?i)" re)) (re-pattern re)) command))]
    (cond
      (card "^-LINE\\s+\\d+")
      ;; "-LINE line_number 'filename' ... The file name is optional, but must be quoted if it appears" (manual ch. 14): in single or double quotes
      (let [[_ n f1 f2] (re-find #"^-\S+\s+(\d+)(?:\s+(?:\"([^\"]*)\"|'([^']*)'))?" command)]
        (assoc pos :file (or f1 f2 (:file pos)) :revert nil :bp (inc pline) :bl (Long/parseLong n)))
      (card "^-FILE-ENTER\\s+\"")
      (let [[_ f] (re-find #"\"([^\"]*)\"" command)]
        (assoc pos :file f :bp (inc pline) :bl 1 :stack (conj (:stack pos) (assoc pos :card-line (+ (:bl pos) (- pline (:bp pos)))))))
      (card "^-FILE-LEAVE")
      (let [outer (peek (:stack pos))]
        (let [stale (boolean (re-find #"STALE" command))]
          (assoc outer :file (if stale (:file pos) (:file outer)) :revert (when stale (:file outer)) :bp (inc pline) :bl (inc (:card-line outer)) :stack (pop (:stack pos)))))
      :else pos)))

;; -- Constant folding -----------------------------------------------------------
;; SPITBOL evaluates as a statement is compiled what is made of constants alone, and an error raised there is the statement's compile error (sbl -bf, measured:
;; CODE(' X = 1 / 0') is refused with 14 and a program holding it with rc 231; ' V = &UCASE * 2' with 26; ' X = LEN(-1)' with 121). The constants are literals and
;; the keywords whose values never change; what folds is arithmetic, concatenation, and the built-in functions sbl was seen to fold, given constants (EQ, LT, DUPL,
;; SUBSTR, ARRAY and CONVERT were seen not to; within a deferred *X the constants fold, and *X itself is no constant: *(1 / 0) is 14).
(def ^:private constant-keywords '#{ABORT ALPHABET ARB BAL FAIL FENCE LCASE REM SUCCEED UCASE})
(def ^:private folded-operators '#{+ - * / **})
;; The functions are those whose svblk carries svpre, "the compiler [may] preevaluate a call to the associated system function if all the arguments are themselves
;; constants. functions in this category must have no side effects and must never cause failure. the call may generate an error condition" (sbl.min svpre): the
;; mathematical ones among them too (sbl -bf, CODE: SQRT(-0.5) is 314, EXP(1000000) 305, LN(0) 307, LN(-1) 315, SIN('a') 308, COS('a') 303, TAN('a') 309,
;; ATAN('a') 301, CHOP('a') 302).
(def ^:private folded-functions
  '#{SIZE LEN ANY NOTANY SPAN BREAK BREAKX POS RPOS TAB RTAB ARBNO FENCE REMDR CHAR TRIM REPLACE LPAD RPAD REVERSE DIFFER IDENT INTEGER DATATYPE LGT
     SQRT EXP LN SIN COS TAN ATAN CHOP})

;; -- Names ----------------------------------------------------------------------
;; Where a name is wanted -- the operand of unary . @ and &, the right of binary . and $, the subject of an assignment or a replacement, a goto's operand -- an
;; expression that gives a value is "212 syntax error: value used where name is required" as the statement is compiled, found before an error folding what is inside it, and
;; operands left to right (sbl -bf: A . (1 / 0) is 212 and (1 / 0) . A is 14; F(.('a' + 1), 1 / 0) is 212 and F(1 / 0, .('a' + 1)) 14; :(1 + 2), :('L'),
;; :(X ? 'L') and (1 / 0) = 5 are 212). A name is a variable, an element, a call, $X, *X, &K, or an operator SPITBOL leaves for OPSYN, which is called as a
;; function is (unary ! % / # = |, binary # % & ~ @: :(!X) and :(X @ Y) compile); the operators SPITBOL defines give values, and so do a literal, a
;; concatenation and a selection (:(+X), :(.X), :(X ** Y), :((X, Y)) are 212).
(def ^:private unary-names  '#{$ * & ! % / sharp = |})
(def ^:private binary-names '#{sharp % & tilde at})
(def ^:private operators    '#{+ - * / ** sharp % at & tilde | ? ?= = . $ !})

(defn- name-form? [x]
  (cond
    (or (symbol? x) (keyword? x)) true
    (list? x) (let [[op & args] x n (count args)]
                (cond (:subscript (meta x))                   true
                      (not (symbol? op))                      true
                      (and (= 1 n) (unary-names op))          true
                      (and (= 2 n) (binary-names op))         true
                      (operators op)                          false
                      :else                                   true))
    :else false))

(defn- name-required! [x]
  (when-not (name-form? x) (throw (ex-info "syntax error: value used where name is required" {:snobol/error 212}))))

;; the operands of op given args that are name positions, by their index
(defn- name-positions [op args]
  (let [n (count args)]
    (cond (and (= 1 n) ('#{. at &} op)) #{0}
          (and (= 2 n) ('#{. $} op))    #{1}
          (and (= 2 n) (= '= op))       #{0}
          (and (= 3 n) (= '?= op))      #{0}
          :else                          #{})))

;; The constants beyond literals and keywords, as sbl -bf's compiler takes them (sbl.min svkvc; CODE refuses each): the seven protected pattern variables
;; (SIZE(ARB) is 189, ARB + 1 is 1; env/protected-variables); a deferred expression, *E, folded within too (*X + 1 is 1, *(1 / 0) 14); an alternation of
;; constants (('a' | 'b') + 1 is 1); the name of a variable, a keyword or an expression, .X .&K .*E (.X + 1 is 1, .&ERRTEXT 'z' is 8, LEN(.X) 120), and .$C of
;; a constant C, whose value names a variable or is "239 indirection operand is not name" (.$ARB, .$LEN(1), .$*X and .$&FENCE are 239, .$'X' and .$1 compile);
;; a cursor assignment @X or @$C (SIZE(@X) is 189, @$ARB 239); and a capture into such a name of a constant pattern (LEN(1) . X + 1 is 1, LEN(1) . $ARB 239).
;; A name that holds an element or a call's is found as the program runs (.A[1] + 1, .F() + 1, LEN(1) . A[1] + 1 and Q . $ARB compile).
(defn- pattern-variable [sym]
  (some-> (requiring-resolve (symbol "SNOBOL4clojure.patterns" (name sym))) var-get))

(declare fold)

(defn- constant-name?
  "True for a name that is a constant: a variable, a keyword, an expression, or $C of a constant C."
  [x]
  (or (symbol? x)
      (and (list? x) (= 2 (count x)) (or (= '& (first x)) (= '* (first x))))
      (and (list? x) (= 2 (count x)) (= '$ (first x)) (first (fold (second x))))))

(defn- fold
  "[constant? value] of the IR n, its constant parts evaluated as SPITBOL's compiler evaluates them; an error raised there is thrown."
  [n]
  (let [eval! (requiring-resolve 'SNOBOL4clojure.operators/EVAL!)]
    (cond
      (or (string? n) (number? n)) [true n]
      (and (symbol? n) (SNOBOL4clojure.env/protected-variables n)) [true (pattern-variable n)]
      (vector? n)
      (let [comma? (= :comma (first n))
            parts  (mapv fold (if comma? (rest n) n))]
        (if (and (not comma?) (every? first parts)) [true (eval! n)] [false nil]))
      (list? n)
      (let [[op & args] n
            names (name-positions op args)]
        (cond
          (and (= '& op) (= 1 (count args)))  (do (when-not (symbol? (first args)) (name-required! (first args)))
                                                  (if (constant-keywords (first args)) [true (eval! n)] [false nil]))
          (and (= '* op) (= 1 (count args)))  (do (fold (first args)) [true (eval! n)])   ; folded within, and itself a constant expression
          ;; .X .&K .*E .$C and @X @$C, each checked for a name first
          (and ('#{. at} op) (= 1 (count args)) (do (name-required! (first args)) true) (constant-name? (first args))
               (or (= '. op) (symbol? (first args)) (= '$ (first (first args)))))
          (do (fold (first args)) [true (eval! n)])
          ;; a constant pattern captured into a constant name
          (and ('#{. $} op) (= 2 (count args)))
          (let [p (fold (first args))]
            (name-required! (second args))
            (let [t (fold (second args))]
              (if (and (first p) (constant-name? (second args))) [true (eval! n)] [false nil])))
          :else
          (let [parts (vec (map-indexed (fn [i a] (when (names i) (name-required! a)) (fold a)) args))]
            (if (and (every? first parts)
                     (or (and (folded-operators op) (<= 1 (count args) 2))
                         (and (= '| op) (<= 2 (count args)))
                         (and (folded-functions op) (not (:subscript (meta n))))))
              (let [v (eval! n)] (if (nil? v) [false nil] [true v]))
              [false nil]))))
      :else [false nil])))

(defn fold-error
  "The number of the error folding the statement body raises, then its goto's operands, or nil."
  ([body] (fold-error body nil))
  ([body goto]
   (try (fold body)
        (doseq [k [:S :F :G] :let [t (get goto k)] :when (and (some? t) (not (keyword? t)) (not (and (list? t) (= 'DIRECT (first t)))))]
          (name-required! t)
          (fold t))
        nil
        (catch clojure.lang.ExceptionInfo e (:snobol/error (ex-data e))))))

;; -- CODE! — parse source into {CODES NOS LABELS} ----------------------------
(defn CODE!
  "Compile SNOBOL4 source S into a [codes nos labels disp] vector. disp holds, for each statement in order, how many blank lines (statements SPITBOL
   numbers but never runs) came before it, and the total, as {:before [..] :total n}, and where it stands in its file, as {:pos [[file line] ..]}.
   Optional include-path is a seq of directory strings used to resolve
   -INCLUDE directives (default [\".\"]). file is the name the program was given, which &FILE reads."
  ([S] (CODE! S ["."]))
  ([S include-path] (CODE! S include-path nil))
  ([S include-path file] (CODE! S include-path file false))
  ([S include-path file record?]
   (let [;; a record with a line end in it is read as it stands: the -INCLUDE preprocessor reads lines, and would end them
         record?   (and record? (boolean (re-find #"[\r\n]" (str S))))
         [expanded _ cards] (if record? [(str S) #{} []] (preprocess-includes* (str S) include-path #{}))
         raw-stmts (split-source expanded record?)
         _         (reset! <DIAGNOSTICS> (:diagnostics (meta raw-stmts)))
         _         (reset! <SOURCE> {:text expanded :stmts raw-stmts :cards cards})]
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
             (let [stmt   (cond-> command (not record?) (string/replace #"\r" "") fold fold-case)
                   ;; a line end outside a literal in a record does not compile: 214 where it begins the label field, 230 elsewhere (sbl -bf: CODE(' OUTPUT = 1'
                   ;; CHAR(10) ' OUTPUT = 2') and CODE(' OUTPUT = 3' CHAR(13) ' OUTPUT = 4') are 230, CODE(' OUTPUT = 6;' CHAR(10)) 214)
                   ast    (if (and record? (line-end-outside-literals? stmt)) ::line-end (parse-statement stmt))
                   ;; a compile error the emitter finds (an invalid numeric item) is the statement's error, as a parse failure is
                   code   (if (= ast ::line-end)
                            {:reason :line-end :line pline :column 1}
                            (try (emitter ast)
                                 (catch clojure.lang.ExceptionInfo e
                                   (if (:snobol/compile (ex-data e)) {:reason :compile :line pline :column 1 :text (ex-message e)} (throw e)))))
                   here   [(:file pos) (+ (:bl pos) (- pline (:bp pos)))]
                   pos''  (if (:revert pos) (assoc pos :file (:revert pos) :revert nil) pos)]
               (if (and (map? code) (:reason code))
                 ;; a statement that does not compile is the error SPITBOL's scan names, at its line and column (syntax.clj); 220 when the scan finds none. A line
                 ;; that starts with a continuation mark is split-source's to judge (214, or a continuation sbl takes after a blank line)
                 (let [{n :error col :column} (or (when-not (= :compile (:reason code)) (syntax-error stmt))
                                                  {:error (case (:reason code) :compile 231 :line-end 230 220)})]
                   (when-not (#{\+ \.} (first stmt))
                     (swap! <DIAGNOSTICS> conj [pline n (when col (min (long col) (max 1 (count stmt))))]))
                   (recur (rest stmts) (inc NO) (assoc CODES NO (error-node code)) NOS LABELS blanks (conj before blanks) fold (conj poss here) pos''))
                 (let [label  (:label code)
                       body   (:body  code)
                       goto   (:goto  code)
                       _      (when-let [n (fold-error body (when (map? goto) goto))] (swap! <DIAGNOSTICS> conj [pline n nil]))
                       key    (if label label NO)
                       code   (reduce #(conj %1 %2) [] [body goto])
                       nos    (if (keyword? key) (assoc NOS   key NO) NOS)
                       labels (if (keyword? key) (assoc LABELS NO key) LABELS)
                       codes  (assoc CODES key code)]
                   ;; "After reading the END statement, compilation ends" (manual ch. 14, The END Statement): nothing after it is compiled, so the data a program keeps there
                   ;; is no statement, and no error (sbl -bf; snoflake eliza-duquet-original keeps its script after END)
                   (if (#{:END :end} label)
                     [codes nos labels {:before (conj before blanks) :total blanks :pos (conj poss here)}]
                     (recur (rest stmts) (inc NO) codes nos labels blanks (conj before blanks) fold (conj poss here) pos''))))))))))))

;; "If we place a statement label in the subject field of the END statement, execution will begin there" (manual ch. 14, The END Statement): END START starts the
;; program at START. The label END is recognised in either case.
(defn- end-entry [codes]
  (let [b (first (or (get codes :END) (get codes :end)))]
    (when (symbol? b)
      (let [k (keyword (name b))] (when (@<STNO> k) k)))))

;; -- CODE — compile and load into the global statement store ------------------
(declare load-code)
(defn CODE
  "Compile S and load into the global statement store.
   Optional include-path is a seq of directory strings (default [\".\"])."
  ([S] (CODE S ["."]))
  ([S include-path] (CODE S include-path nil))
  ([S include-path file]
   (let [[_ _ _ disp :as ir] (CODE! S include-path file)]
     ;; the program's last line read, its END's (sbl -bf reads no further: data after END is no line), is where CODE's texts number on from
     (reset! <CODELINE> (or (last (:pos disp)) ["" 0]))
     (load-code ir))))

(defn- load-code
  "Load a CODE! result into the global statement store; returns the entry, as CODE does."
  [[codes nos labels disp]]
   (let [start              (inc @STNO)
         base               @BLANKS]
     (loop [NO 1]
       (if (> NO (count codes))
         (do
           (swap! BLANKS + (:total disp 0))
           (or (end-entry codes)
               (if (and (@<LABL> start) (@<CODE> (@<LABL> start)))
                 (@<LABL> start)
                 (when (@<CODE> start) start))))
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

;; "If we had not done this, SPITBOL would terminate when execution 'fell out of the bottom' of the new code block" (manual ch. 9, The CODE function): the block
;; CODE() compiles at run time ends in a statement of its own, numbered after its last, which ends the program when control reaches it (env/block-end). A block
;; with no statements, CODE('') or a comment alone, is that statement, and starts there. A block that ends in its own END has none: its END is its last statement,
;; and the next block numbers on from it (sbl -bf: after a seven-statement program CODE('END') is statement 8 and the next block's first 9; CODE(' OUTPUT = 1;END')
;; is 11 and 12, the next block's first 13).
;; "The string s must contain valid SPITBOL program statements complete with labels and using semicolons to separate statements" (manual ch. 19, CODE): the text
;; is one record, split-source's record?.
;; "The call fails if syntactical errors are found. In this case, it is possible to inspect &ERRTEXT to find the error message" (manual ch. 19, CODE): a block with
;; a statement that does not compile loads nothing, and answers its first error as {:error n}.
;; Each text CODE compiles is one more line of the program's file, after the last line the compiler read, whether it compiles or not; &FILE there is the program's
;; (sbl -bf: an eleven-line program's CODE texts are lines 12, 13 and 14, a failed one among them, and a text of two statements is one line; EVAL takes none). A
;; statement an -INCLUDE in the text brings keeps its own file and line.
(defn CODE-block [S]
  (let [[f l]          (swap! <CODELINE> (fn [[f l]] [f (inc (long l))]))
        [codes :as ir] (update-in (CODE! S ["."] nil true) [3 :pos] (fn [ps] (mapv #(if (= "" (first %)) [f l] %) ps)))]
    (if-let [[_ n] (first @<DIAGNOSTICS>)]
      {:error n}
      (let [entry (load-code ir)]
        (if (or (contains? codes :END) (contains? codes :end))
          entry
          (do (swap! STNO inc)
              (swap! <DISPNO> assoc @STNO (+ @STNO @BLANKS))
              (swap! <CODE> assoc @STNO block-end)
              (or entry @STNO)))))))

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
