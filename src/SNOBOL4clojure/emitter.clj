(ns SNOBOL4clojure.emitter
  ;; Transform an instaparse AST into Clojure data (the SNOBOL4 IR).
  (:require [clojure.edn :as edn]
            [clojure.string]
            [SNOBOL4clojure.env      :refer [subtract parse-real]]
            [SNOBOL4clojure.grammar  :refer [parse-expression]]
            [instaparse.core         :as insta]))

;; An = whose target is a concatenation outside every bracket, at the top of a statement's replacement or of the text EVAL compiles, is a replacement of its own:
;; the first element is the subject and the rest the pattern, and its value is the subject's new value; inside brackets it is an assignment, whose target must be
;; a name (sbl -bf: X = A 'y' = 1 of A = 'xyz' makes X and A x1z; X = A 'y' = C 'b' = 3 replaces in C first; X = Y = A 'y' = 2 makes all three x2z; X = A 'y' 'z'
;; = 1 matches 'y' 'z'; EVAL("A 'y' = 1") replaces; X = (A 'y' = 1) is 212). A parenthesized value carries :paren in its metadata (the grammar's grp).
(defn top-replacement [r]
  (if (and (seq? r) (= '= (first r)) (= 3 (count r)) (not (:paren (meta r))))
    (let [[_ t v] r
          v       (top-replacement v)]
      (if (and (vector? t) (not (:paren (meta t))))
        (list '?= (first t) (if (= 2 (count t)) (second t) (vec (rest t))) v)
        (list '= t v)))
    r))

(defn emitter [ast]
  (insta/transform
    {:comment   (fn [cmt]    [:comment cmt])
     :control   (fn [ctl]    [:control ctl])
     :stmt      (fn [& ss]   (apply conj ss))
     ;; "Labels must begin with a letter or digit, optionally followed by an arbitrary string of characters" (manual ch. 14, Label field): L+, X-, Q# and HSORT.END
     ;; are labels, each a keyword, as the goto target naming it is
     :label     (fn [L]      {:label (keyword L)})
     :body      (fn [B]      {:body B})
     :invoking  (fn [S]      S)
     ;; "It is left associative" (manual, Appendix, SPITBOL extensions): S ? P1 ? P2 matches P2 against the text P1 matched
     :matching  (fn [S P & qs] (reduce (fn [m q] (list '? m q)) (list '? S P) qs))
     :qmatch    (fn [P]      P)
     :replacing (fn
                  ([S P]     (list '?= S P 'epsilon))
                  ([S P R]   (list '?= S P (top-replacement R))))
     :assigning (fn
                  ([S]       (list '= S 'epsilon))
                  ([S R]     (list '= S (top-replacement R))))
     :grp       (fn [x]      (if (instance? clojure.lang.IObj x) (vary-meta x assoc :paren true) x))
     :goto      (fn [& gs]   {:goto (reduce
                                      (fn [bs b]
                                        (let [key (first b) tgt (second b)]
                                          (assoc bs key
                                            (if (symbol? tgt) (keyword tgt) tgt))))
                                      {} gs)})
     :jmp       (fn [L]      [:G L])
     :sjmp      (fn [L]      [:S L])
     :fjmp      (fn [L]      [:F L])
     :expr      (fn [x]      x)
     :asn       (fn
                  ([x]       x)
                  ;; (S ? P) = R is S ? P = R, a match with a replacement: the grammar reads it this way inside parentheses
                  ([x y]     (if (and (seq? x) (= '? (first x)) (= 3 (count x)))
                               (list '?= (second x) (nth x 2) y)
                               (list '= x y))))
     :mch       (fn
                  ([x]       x)
                  ([x y]     (list '? x y))
                  ([x y z]   (list '?= x y z)))
     :and       (fn
                  ([x]       x)
                  ([x y]     (list '& x y)))
     :alt       (fn
                  ([x]       x)
                  ([x & ys]  (apply list '| x ys)))
     :xalt      (fn [x & ys] (apply list '| x ys))
     :xand      (fn [x y]    (list '& x y))
     :cat       (fn
                  ([x]       x)
                  ([x & ys]  (apply vector x ys)))
     :at        (fn
                  ([x]       x)
                  ([x y]     (list 'at-op x y)))
     :sum       (fn
                  ([x]       x)
                  ([x op y]  (list (symbol op) x y)))
     :hsh       (fn
                  ([x]       x)
                  ([x y]     (list 'sharp-op x y)))
     :div       (fn
                  ([x]       x)
                  ([x y]     (list '/ x y)))
     :mul       (fn
                  ([x]       x)
                  ([x y]     (list '* x y)))
     :pct       (fn
                  ([x]       x)
                  ([x y]     (list '% x y)))
     :xp        (fn
                  ([x]       x)
                  ;; ^ and ! are SPITBOL's synonyms for **; one operator reaches INVOKE
                  ([x op y]  (list (symbol (if (#{"^" "!"} op) "**" op)) x y)))
     :cap       (fn
                  ([x]       x)
                  ([x op y]  (list (symbol op) x y)))
     :ttl       (fn
                  ([x]       x)
                  ([x y]     (list 'tilde-op x y)))
     :uop       (fn
                  ([x]       x)
                  ([op y]    (case op
                               "@" (list 'at-op y)
                               "#" (list 'sharp-op y)
                               "~" (list 'tilde-op y)
                                   (list (symbol op) y))))
     ;; A<k> and A[k] are subscripts, F(k) a call (the inv node): the subscript form carries :subscript, so DATA[1] reads the array a variable DATA holds while
     ;; DATA('...') still calls the function, as sbl -bf tells them apart
     :ndx       (fn
                  ([n]       n)
                  ([n & xs]  (with-meta (apply list n xs) {:subscript true})))
     :cnd       (fn
                  ([x]       x)
                  ;; (e1, e2, ...): the first expression that succeeds; the keyword marks the group, which no concatenation can begin with
                  ([x & ys]  (apply vector :comma x ys)))
     :inv       (fn [f & xs] (apply list f xs))
     ;; an argument left out, F(A,,C) or F(,B), is the null string in its place
     :nul       (fn []       "")
     ;; a direct Goto, :<V>, branches to the code V holds, not to a label (manual ch. 14, Goto field)
     :dgo       (fn [x]      (list 'DIRECT x))
     :N         (fn [n]      (symbol n))
     :S         (fn [s]      (subs s 1 (subtract (count s) 1)))
     :I         edn/read-string
     ;; a real literal reads as SPITBOL reads it (env/parse-real); one too large for a real is "231 Syntax error: Invalid numeric item" (Appendix D), a compile error
     :R         (fn [s] (or (parse-real s) (throw (ex-info "syntax error: invalid numeric item" {:snobol/error 231 :snobol/compile true}))))}
    ast))

(defn parse+emit
  "Parse a SNOBOL4 expression string and emit the Clojure IR."
  [s]
  (first (emitter (parse-expression s))))
