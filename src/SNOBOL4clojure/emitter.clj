(ns SNOBOL4clojure.emitter
  ;; Transform an instaparse AST into Clojure data (the SNOBOL4 IR).
  (:require [clojure.edn :as edn]
            [clojure.string]
            [SNOBOL4clojure.env      :refer [subtract parse-real]]
            [SNOBOL4clojure.grammar  :refer [parse-expression]]
            [instaparse.core         :as insta]))

(defn emitter [ast]
  (insta/transform
    {:comment   (fn [cmt]    [:comment cmt])
     :control   (fn [ctl]    [:control ctl])
     :stmt      (fn [& ss]   (apply conj ss))
     ;; a label of letters, digits, underscores and points is a keyword, as the goto target naming it is; the points are the N rule's (HSORT.END)
     :label     (fn [L]      {:label (if (re-find #"^[0-9A-Z_a-z.]+$" L)
                                       (keyword L) (str L))})
     :body      (fn [B]      {:body B})
     :invoking  (fn [S]      S)
     :matching  (fn [S P]    (list '? S P))
     :replacing (fn
                  ([S P]     (list '?= S P 'epsilon))
                  ([S P R]   (list '?= S P R)))
     :assigning (fn
                  ([S]       (list '= S 'epsilon))
                  ([S R]     (list '= S R)))
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
     :cat       (fn
                  ([x]       x)
                  ([x & ys]  (apply vector x ys)))
     :at        (fn
                  ([x]       x)
                  ([x y]     (list 'at x y)))
     :sum       (fn
                  ([x]       x)
                  ([x op y]  (list (symbol op) x y)))
     :hsh       (fn
                  ([x]       x)
                  ([x y]     (list 'sharp x y)))
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
                  ([x y]     (list 'tilde x y)))
     :uop       (fn
                  ([x]       x)
                  ([op y]    (case op
                               "@" (list 'at y)
                               "#" (list 'sharp y)
                               "~" (list 'tilde y)
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
     ;; a real literal reads as SPITBOL reads it (env/parse-real); one too large for a real stays the infinity it was
     :R         (fn [s] (or (parse-real s) (Double/parseDouble (clojure.string/replace s #"[dD]" "e"))))}
    ast))

(defn parse+emit
  "Parse a SNOBOL4 expression string and emit the Clojure IR."
  [s]
  (first (emitter (parse-expression s))))
