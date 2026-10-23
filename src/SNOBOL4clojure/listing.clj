(ns SNOBOL4clojure.listing
  "The program listing SPITBOL writes on standard output: the lines -LIST asks for, and the page a compile error is reported on (measured against sbl -bf)."
  (:require [clojure.string :as string]
            [SNOBOL4clojure.env :as env])
  (:import  [java.time LocalDateTime]
            [java.time.format DateTimeFormatter]
            [java.util Locale]))

;; "-LIST This statement turns on listing, thereby producing a program listing with statement numbers"; "-NOLIST This statement turns off listing (the default)";
;; -EJECT "Advance to a new page"; "-SPACE N This sends N blank lines to the listing. N defaults to one"; -TITLE and -STITL: "The list file is advanced to a new page
;; prior to printing the new title and any subtitle line"; -PRINT and -NOPRINT whether control statements are listed; -SINGLE and -DOUBLE its spacing (manual ch. 14,
;; Listing controls). As sbl -bf lays it out: the first line listed brings the head (three empty lines, the identity and clock lines, three empty lines) and page 1; a
;; page opens with its title line, "page N" at column 113, its subtitle line once a -TITLE or -STITL has given one, and an empty line; a page holds 55 listed lines
;; (the first 51) and the next one opens three empty lines on. A line on which a statement begins carries the statement's number in its first columns, an included
;; line the depth of its -INCLUDE in column 6 ("The include nest depth appears in the listing file between the statement number and the statement proper", -INCLUDE),
;; and every line is cut at 120 columns. A -SPACE's empty lines come before its own line, an -EJECT's or a title's new page before its own. Nothing after END is listed.
;; A compile error is written after the line it is found on: an empty line, file(line) : ERROR nnn -- text, an empty line; when the listing is off, the line is written
;; first, on the page the first error opens. The line, the empty line, the error and the empty line go to standard error as well.
(def ^:private width 120)
(def ^:private page-lines 55)

;; the head names this engine (project.clj's version), as sbl's names its own build
(def ^:private identity-line "snobol4jvm version 0.2.0")

(defn- clock-line []
  (let [arch (System/getProperty "os.arch")]
    (str (if (#{"amd64" "x86_64"} arch) "x86-64" arch) "  "
         (.format (DateTimeFormatter/ofPattern "EEE MMM ppd HH:mm:ss yyyy" Locale/US) (LocalDateTime/now)))))

(defn- cut [^String l] (if (<= (count l) width) [l] (map #(apply str %) (partition-all width l))))

(defn- write! [l] (doseq [p (cut l)] (println p)))

(defn- page-line [^String title n]
  (let [pn (str "page " n)
        t  (if (< (count title) 112) (str title (apply str (repeat (- 112 (count title)) \space))) title)]
    (str (subs t 0 112) pn (when (> (count t) (+ 112 (count pn))) (subs t (+ 112 (count pn)))))))

(defn- start-page! [st used]
  (swap! st update :page inc)
  (write! (page-line (:title @st) (:page @st)))
  (swap! st assoc :on-page used)
  (when-some [s (:subtitle @st)] (write! s) (swap! st update :on-page inc))
  (println))

(defn- new-page! [st] (print "\n\n\n") (start-page! st 0))

;; the head and page 1, when nothing is listed yet: true when it opened them
(defn- open! [st]
  (when (zero? (long (:page @st)))
    (print "\n\n\n") (println identity-line) (println (clock-line)) (print "\n\n\n")
    (start-page! st 4)
    true))

(defn- list-line! [st l]
  (when (:pending @st)
    (swap! st assoc :pending false)
    (when (pos? (long (:page @st))) (new-page! st)))
  (when-not (open! st)
    (when (>= (long (:on-page @st)) page-lines) (new-page! st)))
  (write! l)
  (swap! st update :on-page inc)
  (when (:double @st) (println) (swap! st update :on-page inc)))

(defn- numbered [n depth line]
  (if (pos? (long depth))
    (format "%-5s%-3s%s" (or n "") depth line)
    (format "%-8s%s" (or n "") line)))

;; The number of the statement each physical line of the source begins, and of the statement in progress on each line: split-source's statements, [text line], are
;; numbered in order, a blank line too, and neither a comment nor a control line.
(defn- statement-numbers [stmts]
  (loop [ss stmts n 0 starts (sorted-map)]
    (if-let [[s l] (first ss)]
      (if (or (= :blank s) (not (#{\* \-} (first s))))
        (recur (rest ss) (inc n) (if (contains? starts l) starts (assoc starts l (inc n))))
        (recur (rest ss) n starts))
      starts)))

(defn write-listing!
  "Write the listing of the program last compiled, named file, as far as its -LIST cards ask for one, and its compile errors on their page."
  [file]
  (let [{:keys [text stmts cards]} @env/<SOURCE>
        lines  (string/split-lines (str text))
        starts (statement-numbers stmts)
        errs   (group-by first @env/<DIAGNOSTICS>)
        fold   (not (zero? (long @env/&CASE)))
        card   (fn [^String l nm] (re-find (re-pattern (str (when fold "(?i)") "^-" nm "(?:\\s|$)")) l))
        st     (atom {:page 0 :on-page 0 :title "" :subtitle nil :pending false :on false :print true :double false})]
    ;; cards: the -INCLUDE lines as written, one for each -FILE-ENTER card the splice put in their place
    (loop [i 1 ls lines depth 0 places [] file (str file) fline 0 cards cards]
      (when-let [raw (first ls)]
        (let [l   (string/replace raw #"\r$" "")
              n   (get starts i)
              ctl (string/starts-with? l "-")
              shown (fn [line] (when (and (:on @st) (or (not ctl) (:print @st))) (list-line! st (numbered n depth line))))]
          (cond
            (string/starts-with? l "-FILE-LEAVE")
            (let [[f fl] (peek places)] (recur (inc i) (rest ls) (dec depth) (pop places) f fl cards))
            (string/starts-with? l "-FILE-ENTER")
            (let [[_ nm] (re-find #"^-FILE-ENTER \"([^\"]*)\"" l)]
              (shown (first cards))
              (recur (inc i) (rest ls) (inc depth) (conj places [file (inc fline)]) nm 0 (rest cards)))
            :else
            (let [fline (inc fline)]
              (cond
                (and ctl (card l "LIST"))    (do (swap! st assoc :on true) (shown l))
                (and ctl (card l "NOLIST"))  (swap! st assoc :on false)
                (and ctl (card l "PRINT"))   (do (swap! st assoc :print true) (shown l))
                (and ctl (card l "NOPRINT")) (swap! st assoc :print false)
                (and ctl (card l "SINGLE"))  (do (swap! st assoc :double false) (shown l))
                (and ctl (card l "DOUBLE"))  (do (swap! st assoc :double true) (shown l))
                (and ctl (card l "TITLE"))   (do (swap! st assoc :title (string/trim (subs l 6)) :subtitle "" :pending true) (shown l))
                (and ctl (card l "STITL"))   (do (swap! st assoc :subtitle (string/trim (subs l 6)) :pending true) (shown l))
                (and ctl (card l "EJECT"))   (do (when (:on @st) (when-not (open! st) (new-page! st))) (shown l))
                (and ctl (card l "SPACE"))   (do (when (and (:on @st) (not (open! st)))
                                                   (let [k (if-let [[_ d] (re-find #"^-\S+\s+(\d+)" l)] (Long/parseLong d) 1)]
                                                     (if (< (+ (long (:on-page @st)) k) page-lines)
                                                       (do (print (apply str (repeat k \newline))) (swap! st update :on-page + k))
                                                       (new-page! st))))
                                                 (shown l))
                :else                        (shown l))
              ;; a syntax error names its column too, and a line under the statement marks it with a ! (sbl -bf: "t.sno(2,6) : ERROR 232 -- ...", the marker
              ;; keeping the tabs of the line before the column, a blank for any other character)
              (doseq [[_ code col] (get errs i)]
                (let [stmt (numbered (some-> (first (rsubseq starts <= i)) val) depth l)
                      mark (if col
                             (str (numbered nil depth (apply str (map #(if (= \tab %) \tab \space) (take (dec (long col)) l)))) "!")
                             "")
                      msg  (if col
                             (format "%s(%d,%d) : ERROR %03d -- %s" file fline col code (env/error-message code))
                             (format "%s(%d) : ERROR %03d -- %s" file fline code (env/error-message code)))]
                  (when-not (:on @st) (list-line! st stmt))
                  (doseq [x [mark msg ""]] (write! x))
                  (swap! st update :on-page + 3)
                  (binding [*out* *err*] (doseq [x [stmt mark msg ""]] (write! x)))))
              ;; the listing ends with the END line
              (when-not (and n (re-find (if fold #"(?i)^END(\s|$)" #"^(END|end)(\s|$)") l))
                (recur (inc i) (rest ls) depth places file fline cards)))))))
    (flush)))
