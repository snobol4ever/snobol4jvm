(ns SNOBOL4clojure.snocone (:require [instaparse.core :as insta :refer [defparser]]))
(def ^:const KIND-INTEGER :sc/integer)
(def ^:const KIND-REAL :sc/real)
(def ^:const KIND-STRING :sc/string)
(def ^:const KIND-IDENT :sc/identifier)
(def ^:const KIND-IF :sc/kw-if)
(def ^:const KIND-ELSE :sc/kw-else)
(def ^:const KIND-WHILE :sc/kw-while)
(def ^:const KIND-DO :sc/kw-do)
(def ^:const KIND-FOR :sc/kw-for)
(def ^:const KIND-RETURN :sc/kw-return)
(def ^:const KIND-FRETURN :sc/kw-freturn)
(def ^:const KIND-NRETURN :sc/kw-nreturn)
(def ^:const KIND-GO :sc/kw-go)
(def ^:const KIND-TO :sc/kw-to)
(def ^:const KIND-PROCEDURE :sc/kw-procedure)
(def ^:const KIND-STRUCT :sc/kw-struct)
(def ^:const KIND-LPAREN :sc/lparen)
(def ^:const KIND-RPAREN :sc/rparen)
(def ^:const KIND-LBRACE :sc/lbrace)
(def ^:const KIND-RBRACE :sc/rbrace)
(def ^:const KIND-LBRACKET :sc/lbracket)
(def ^:const KIND-RBRACKET :sc/rbracket)
(def ^:const KIND-COMMA :sc/comma)
(def ^:const KIND-SEMICOLON :sc/semicolon)
(def ^:const KIND-COLON :sc/colon)
(def ^:const KIND-ASSIGN :sc/op-assign)
(def ^:const KIND-QUESTION :sc/op-question)
(def ^:const KIND-PIPE :sc/op-pipe)
(def ^:const KIND-OR :sc/op-or)
(def ^:const KIND-CONCAT :sc/op-concat)
(def ^:const KIND-EQ :sc/op-eq)
(def ^:const KIND-NE :sc/op-ne)
(def ^:const KIND-LT :sc/op-lt)
(def ^:const KIND-GT :sc/op-gt)
(def ^:const KIND-LE :sc/op-le)
(def ^:const KIND-GE :sc/op-ge)
(def ^:const KIND-STR-IDENT :sc/op-str-ident)
(def ^:const KIND-STR-DIFF :sc/op-str-differ)
(def ^:const KIND-STR-LT :sc/op-str-lt)
(def ^:const KIND-STR-GT :sc/op-str-gt)
(def ^:const KIND-STR-LE :sc/op-str-le)
(def ^:const KIND-STR-GE :sc/op-str-ge)
(def ^:const KIND-STR-EQ :sc/op-str-eq)
(def ^:const KIND-STR-NE :sc/op-str-ne)
(def ^:const KIND-PLUS :sc/op-plus)
(def ^:const KIND-MINUS :sc/op-minus)
(def ^:const KIND-SLASH :sc/op-slash)
(def ^:const KIND-STAR :sc/op-star)
(def ^:const KIND-PERCENT :sc/op-percent)
(def ^:const KIND-CARET :sc/op-caret)
(def ^:const KIND-PERIOD :sc/op-period)
(def ^:const KIND-DOLLAR :sc/op-dollar)
(def ^:const KIND-AT :sc/op-at)
(def ^:const KIND-AMPERSAND :sc/op-ampersand)
(def ^:const KIND-TILDE :sc/op-tilde)
(def ^:const KIND-NEWLINE :sc/newline)
(def ^:const KIND-EOF :sc/eof)
(def ^:const KIND-UNKNOWN :sc/unknown)
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private keywords
    {
     "if" KIND-IF
     "else" KIND-ELSE
     "while" KIND-WHILE
     "do" KIND-DO
     "for" KIND-FOR
     "return" KIND-RETURN
     "freturn" KIND-FRETURN
     "nreturn" KIND-NRETURN
     "go" KIND-GO
     "to" KIND-TO
     "procedure" KIND-PROCEDURE
     "struct" KIND-STRUCT})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private continuation-chars #{\@ \$ \% \^ \& \* \( \- \+ \= \[ \< \> \| \~ \, \? \:})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private op-entries
    [[":!=:" KIND-STR-NE]
        [":<=:" KIND-STR-LE]
        [":>=:" KIND-STR-GE]
        [":==:" KIND-STR-EQ]
        [":!:" KIND-STR-DIFF]
        [":<:" KIND-STR-LT]
        [":>:" KIND-STR-GT]
        ["::" KIND-STR-IDENT]
        ["||" KIND-OR]
        ["&&" KIND-CONCAT]
        ["==" KIND-EQ]
        ["!=" KIND-NE]
        ["<=" KIND-LE]
        [">=" KIND-GE]
        ["**" KIND-CARET]
        ["=" KIND-ASSIGN]
        ["?" KIND-QUESTION]
        ["|" KIND-PIPE]
        ["+" KIND-PLUS]
        ["-" KIND-MINUS]
        ["/" KIND-SLASH]
        ["*" KIND-STAR]
        ["%" KIND-PERCENT]
        ["^" KIND-CARET]
        ["." KIND-PERIOD]
        ["$" KIND-DOLLAR]
        ["&" KIND-AMPERSAND]
        ["@" KIND-AT]
        ["~" KIND-TILDE]
        ["<" KIND-LT]
        [">" KIND-GT]
        ["(" KIND-LPAREN]
        [")" KIND-RPAREN]
        ["{" KIND-LBRACE]
        ["}" KIND-RBRACE]
        ["[" KIND-LBRACKET]
        ["]" KIND-RBRACKET]
        ["," KIND-COMMA]
        [";" KIND-SEMICOLON]
        [":" KIND-COLON]])
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private ops-by-length (sort-by (comp - count first) op-entries))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- strip-comment
    [^String line]
    (loop [i 0 in-single false in-double false]
        (if
            (>= i (count line))
            line
            (let [c (.charAt line i)]
                (cond
                    (and (= c \') (not in-double)) (recur (inc i) (not in-single) in-double)
                    (and (= c \") (not in-single)) (recur (inc i) in-single (not in-double))
                    (and (= c \#) (not in-single) (not in-double)) (subs line 0 i)
                    :else (recur (inc i) in-single in-double))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- continuation? [^String stripped] (let [trimmed (clojure.string/trimr stripped)] (and (pos? (count trimmed)) (continuation-chars (.charAt trimmed (dec (count trimmed)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- split-semicolon
    [^String line]
    (loop [i 0 in-single false in-double false buf (StringBuilder.) result []]
        (if
            (>= i (count line))
            (conj result (str buf))
            (let [c (.charAt line i)]
                (cond
                    (and (= c \') (not in-double)) (recur (inc i) (not in-single) in-double (.append buf c) result)
                    (and (= c \") (not in-single)) (recur (inc i) in-single (not in-double) (.append buf c) result)
                    (and (= c \;) (not in-single) (not in-double)) (recur (inc i) false false (StringBuilder.) (conj result (str buf)))
                    :else (recur (inc i) in-single in-double (.append buf c) result))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- scan-number
    [^String seg start]
    (let [len (count seg)]
        (let [leading-dot? (= \. (.charAt seg start))]
            (loop [pos (if leading-dot? (inc start) start) real? leading-dot?]
                (let [
                      pos (loop [p pos] (if (and (< p len) (Character/isDigit (.charAt seg p))) (recur (inc p)) p))
                      [pos real?]
                          (if
                              (and (not real?) (< pos len) (= \. (.charAt seg pos)) (< (inc pos) len) (Character/isDigit (.charAt seg (inc pos))))
                              [(loop [p (inc pos)] (if (and (< p len) (Character/isDigit (.charAt seg p))) (recur (inc p)) p)) true]
                              [pos real?])
                      [pos real?]
                          (if
                              (and (< pos len) (#{\e \E \d \D} (.charAt seg pos)))
                              (let [p (inc pos) p (if (and (< p len) (#{\+ \-} (.charAt seg p))) (inc p) p) p (loop [p p] (if (and (< p len) (Character/isDigit (.charAt seg p))) (recur (inc p)) p))]
                                  [p true])
                              [pos real?])]
                    [(if real? KIND-REAL KIND-INTEGER) (subs seg start pos) pos])))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- tokenize-segment
    [^String seg line-no acc]
    (let [len (count seg)]
        (loop [pos 0 acc acc]
            (if
                (>= pos len)
                acc
                (let [c (.charAt seg pos)]
                    (cond
                        (Character/isWhitespace c) (recur (inc pos) acc)
                        (or (= c \') (= c \"))
                            (let [quote c start pos end (loop [p (inc pos)] (cond (>= p len) p (= (.charAt seg p) quote) (inc p) :else (recur (inc p))))]
                                (recur end (conj! acc {:kind KIND-STRING :text (subs seg start end) :line line-no})))
                        (or (Character/isDigit c) (and (= c \.) (< (inc pos) len) (Character/isDigit (.charAt seg (inc pos)))))
                            (let [[kind text end] (scan-number seg pos)] (recur end (conj! acc {:kind kind :text text :line line-no})))
                        (or (Character/isLetter c) (= c \_))
                            (let [
                                  end (loop [p (inc pos)] (let [ch (when (< p len) (.charAt seg p))] (if (and ch (or (Character/isLetterOrDigit ch) (= ch \_))) (recur (inc p)) p)))
                                  word (subs seg pos end)
                                  kind (get keywords word KIND-IDENT)]
                                (recur end (conj! acc {:kind kind :text word :line line-no})))
                        :else
                            (let [[matched-text matched-kind]
                                      (first (for [[text kind] ops-by-length :let [end (+ pos (count text))] :when (and (<= end len) (= text (subs seg pos end)))] [text kind]))]
                                (if matched-text
                                    (recur (+ pos (count matched-text)) (conj! acc {:kind matched-kind :text matched-text :line line-no}))
                                    (recur (inc pos) (conj! acc {:kind KIND-UNKNOWN :text (subs seg pos (inc pos)) :line line-no}))))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn tokenize
    [^String source]
    (let [raw-lines (clojure.string/split (clojure.string/replace source #"\r\n|\r" "\n") #"\n" -1) n (count raw-lines)]
        (loop [i 0 logical [] acc (transient [])]
            (if
                (>= i n)
                (let [acc
                          (if
                              (seq logical)
                              (let [joined (apply str (map :text logical)) stmt-line (:line (first logical))]
                                  (reduce
                                      (fn
                                          [a seg]
                                          (let [s (clojure.string/trim seg)] (if (clojure.string/blank? s) a (conj! (tokenize-segment s stmt-line a) {:kind KIND-NEWLINE :text "\n" :line stmt-line}))))
                                      acc
                                      (split-semicolon joined)))
                              acc)]
                    (persistent! (conj! acc {:kind KIND-EOF :text "" :line n})))
                (let [raw (nth raw-lines i) line-no (inc i) stripped (strip-comment raw) logical (conj logical {:text stripped :line line-no})]
                    (if
                        (and (continuation? stripped) (< (inc i) n))
                        (recur (inc i) logical acc)
                        (let [
                              joined (apply str (map :text logical))
                              stmt-line (:line (first logical))
                              acc
                                  (reduce
                                      (fn
                                          [a seg]
                                          (let [s (clojure.string/trim seg)] (if (clojure.string/blank? s) a (conj! (tokenize-segment s stmt-line a) {:kind KIND-NEWLINE :text "\n" :line stmt-line}))))
                                      acc
                                      (split-semicolon joined))]
                            (recur (inc i) [] acc))))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private prec-table
    {
     :sc/op-assign [1 2]
     :sc/op-question [2 2]
     :sc/op-pipe [3 3]
     :sc/op-or [4 4]
     :sc/op-concat [5 5]
     :sc/op-eq [6 6]
     :sc/op-ne [6 6]
     :sc/op-lt [6 6]
     :sc/op-gt [6 6]
     :sc/op-le [6 6]
     :sc/op-ge [6 6]
     :sc/op-str-ident [6 6]
     :sc/op-str-differ [6 6]
     :sc/op-str-lt [6 6]
     :sc/op-str-gt [6 6]
     :sc/op-str-le [6 6]
     :sc/op-str-ge [6 6]
     :sc/op-str-eq [6 6]
     :sc/op-str-ne [6 6]
     :sc/op-plus [7 7]
     :sc/op-minus [7 7]
     :sc/op-slash [8 8]
     :sc/op-star [8 8]
     :sc/op-percent [8 8]
     :sc/op-caret [9 10]
     :sc/op-period [10 10]
     :sc/op-dollar [10 10]})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def ^:private unary-ops #{:sc/op-plus :sc/op-minus :sc/op-star :sc/op-ampersand :sc/op-at :sc/op-tilde :sc/op-question :sc/op-period :sc/op-dollar})
(def ^:private operand-kinds #{:sc/identifier :sc/integer :sc/real :sc/string})
(defn- sc-operand? [tok] (contains? operand-kinds (:kind tok)))
(defn- sc-binary? [tok] (contains? prec-table (:kind tok)))
(defn- sc-unary? [tok] (contains? unary-ops (:kind tok)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- sc-unary-pos?
    [tokens i]
    (if (zero? i) true (let [prev (:kind (nth tokens (dec i)))] (or (= prev :sc/lparen) (= prev :sc/lbracket) (= prev :sc/comma) (contains? prec-table prev) (contains? unary-ops prev)))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- sc-dotck [tok] (if (and (= (:kind tok) :sc/real) (string? (:text tok)) (clojure.string/starts-with? (:text tok) ".")) (assoc tok :text (str "0" (:text tok))) tok))
(defn- stk-push [s x] (conj s x))
(defn- stk-peek [s] (when (seq s) (last s)))
(defn- stk-pop [s] (when (seq s) (pop s)))
(defn- drain-ops-to [op-stack output stop-pred] (loop [stk op-stack out output] (let [top (stk-peek stk)] (if (or (nil? top) (stop-pred top)) [stk out] (recur (stk-pop stk) (conj out top))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn parse-expression
    [tokens]
    (let [tokens (mapv sc-dotck tokens) n (count tokens)]
        (loop [i 0 output [] op-stack [] call-stack []]
            (if
                (>= i n)
                (let [[_ out] (drain-ops-to op-stack output (constantly false))] out)
                (let [tok (nth tokens i) kind (:kind tok) next-k (when (< (inc i) n) (:kind (nth tokens (inc i))))]
                    (cond
                        (sc-operand? tok)
                            (cond
                                (and (= kind :sc/identifier) (= next-k :sc/lparen))
                                    (recur
                                        (+ i 2)
                                        (conj output tok)
                                        (stk-push op-stack {:kind :sc/lparen :text "(" :line (:line tok)})
                                        (stk-push call-stack {:kind :call :arg-count 0 :output-start (inc (count output))}))
                                (and (= kind :sc/identifier) (= next-k :sc/lbracket))
                                    (recur
                                        (+ i 2)
                                        (conj output tok)
                                        (stk-push op-stack {:kind :sc/lbracket :text "[" :line (:line tok)})
                                        (stk-push call-stack {:kind :array :arg-count 0 :output-start (inc (count output))}))
                                :else (recur (inc i) (conj output tok) op-stack call-stack))
                        (= kind :sc/lparen) (recur (inc i) output (stk-push op-stack tok) (stk-push call-stack {:kind :group :arg-count 0 :output-start (count output)}))
                        (= kind :sc/rparen)
                            (let [
                                  [stk2 out2] (drain-ops-to op-stack output #(= (:kind %) :sc/lparen))
                                  stk3 (stk-pop stk2)
                                  frame (stk-peek call-stack)
                                  calls2 (stk-pop call-stack)
                                  has? (> (count out2) (:output-start frame 0))
                                  argc (if has? (inc (:arg-count frame 0)) 0)
                                  out3 (if (= (:kind frame) :call) (conj out2 {:kind :sc/sc-call :text "()" :line (:line tok) :arg-count argc}) out2)]
                                (recur (inc i) out3 (or stk3 []) (or calls2 [])))
                        (= kind :sc/lbracket) (recur (inc i) output (stk-push op-stack tok) (stk-push call-stack {:kind :group :arg-count 0 :output-start (count output)}))
                        (= kind :sc/rbracket)
                            (let [
                                  [stk2 out2] (drain-ops-to op-stack output #(= (:kind %) :sc/lbracket))
                                  stk3 (stk-pop stk2)
                                  frame (stk-peek call-stack)
                                  calls2 (stk-pop call-stack)
                                  has? (> (count out2) (:output-start frame 0))
                                  argc (if has? (inc (:arg-count frame 0)) 0)
                                  out3 (if (= (:kind frame) :array) (conj out2 {:kind :sc/sc-array-ref :text "[]" :line (:line tok) :arg-count argc}) out2)]
                                (recur (inc i) out3 (or stk3 []) (or calls2 [])))
                        (= kind :sc/comma)
                            (let [
                                  [stk2 out2] (drain-ops-to op-stack output #(#{:sc/lparen :sc/lbracket} (:kind %)))
                                  frame (stk-peek call-stack)
                                  calls2 (if (and frame (#{:call :array} (:kind frame))) (stk-push (stk-pop call-stack) (update frame :arg-count inc)) call-stack)]
                                (recur (inc i) out2 stk2 calls2))
                        (and (sc-unary? tok) (sc-unary-pos? tokens i))
                            (let [[ops operand j]
                                      (loop [j (inc i) ops [(assoc tok :unary? true)]]
                                          (if
                                              (>= j n)
                                              [ops nil j]
                                              (let [t (sc-dotck (nth tokens j))]
                                                  (if (sc-operand? t) [ops t (inc j)] (if (sc-unary? t) (recur (inc j) (conj ops (assoc t :unary? true))) [ops nil j])))))]
                                (recur j (if operand (into output (cons operand (reverse ops))) output) op-stack call-stack))
                        (sc-binary? tok)
                            (let [[_ rp-new] (prec-table kind) [stk2 out2] (drain-ops-to op-stack output (fn [op] (let [[lp-top _] (prec-table (:kind op))] (not (and lp-top (>= lp-top rp-new))))))]
                                (recur (inc i) out2 (stk-push stk2 tok) call-stack))
                        :else (recur (inc i) output op-stack call-stack)))))))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(def sc-grammar
    " expr    ::= <ws*> asn <ws*>
    asn     ::= qst | qst  <ws* '='  ws*>  asn
    qst     ::= pip | pip  <ws* '?'  ws*>  pip
    pip     ::= or  | or   <ws* '|'  ws*>  or  (<ws* '|' ws*> or)*
    or      ::= cat | cat  <ws* '||' ws*>  cat (<ws* '||' ws*> cat)*
    cat     ::= cmp | cmp  <ws* '&&' ws*>  cmp (<ws* '&&' ws*> cmp)*
              | cmp  <ws+>  cmp  (<ws+>  cmp)*
    cmp     ::= sum | sum  <ws*> cmpop <ws*>   sum
   <cmpop>  ::= ':!=:' | ':>=:' | ':<=:' | ':==:' | ':>:' | ':<:' | '::' | ':!:'
              | '>=' | '<=' | '==' | '!=' | '<' | '>'
    sum     ::= mul | sum  <ws*> sumop <ws*>   mul
   <sumop>  ::= '+' | '-'
    mul     ::= xp  | mul  <ws*> mulop <ws*>   xp
   <mulop>  ::= '*' | '/' | '%'
    xp      ::= cap | cap  <ws*> '^'  <ws*>  xp
    cap     ::= uop | uop  <ws*> capop <ws*>  cap
   <capop>  ::= #'[.](?![0-9])' | '$'
    uop     ::= ndx | unaryop <ws*> uop
   <unaryop> ::= '+' | '-' | '*' | '&' | '@' | '~' | '$'
              | #'[.](?![0-9])'
    ndx     ::= atom | ndx <'('> arglist <')'> | ndx <'['> aref <']'>
    aref     ::= expr? | expr (<ws* ',' ws*> expr)*
    arglist  ::= expr? | expr (<ws* ',' ws*> expr)*
   <atom>   ::= real | integer | string | ident | <'('> expr <')'>
    ident   ::= #'[A-Za-z_][A-Za-z0-9_]*'
    integer ::= #'[0-9]+'
    real    ::= #'[0-9]+\\.[0-9]*([eEdD][+-]?[0-9]+)?'
              | #'[0-9]+[eEdD][+-]?[0-9]+'
              | #'\\.[0-9]+([eEdD][+-]?[0-9]+)?'
    string  ::= #'\"[^\"]*\"' | #'\\'[^\\']*\\''
   <ws>     ::= #'[ \\t]+'
  ")
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defparser parse-sc-expr sc-grammar :start :expr)
(def ^:private fn-ops {">" "GT" "<" "LT" ">=" "GE" "<=" "LE" "==" "EQ" "!=" "NE" "::" "IDENT" ":!:" "DIFFER" ":>:" "LGT" ":<:" "LLT" ":>=:" "LGE" ":<=:" "LLE" ":==:" "LEQ" ":!=:" "LNE" "%" "REMDR"})
(def ^:private infix-ops #{"=" "?" "|" "+" "-" "/" "*" "." "$"})
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn- emit-binary
    [op l r]
    (cond
        (= op "||") (str "(" l "," r ")")
        (= op "&&") (str l " " r)
        (= op "^") (str l " ** " r)
        (fn-ops op) (str (fn-ops op) "(" l "," r ")")
        (infix-ops op) (str l " " op " " r)
        :else (str l " " op " " r)))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn emit-expr
    [ast]
    (insta/transform
        {
         :expr (fn [x] x)
         :ident str
         :integer str
         :real (fn [s] (if (clojure.string/starts-with? s ".") (str "0" s) s))
         :string str
         :asn (fn ([x] x) ([l r] (emit-binary "=" l r)))
         :qst (fn ([x] x) ([l r] (emit-binary "?" l r)))
         :pip (fn ([x] x) ([l & rest] (reduce #(emit-binary "|" %1 %2) l rest)))
         :or (fn ([x] x) ([l & rest] (reduce #(emit-binary "||" %1 %2) l rest)))
         :cat (fn ([x] x) ([l & rest] (reduce #(emit-binary "&&" %1 %2) l rest)))
         :cmp (fn [& args] (if (= 1 (count args)) (first args) (let [[l op r] args] (emit-binary op l r))))
         :sum (fn [& args] (if (= 1 (count args)) (first args) (let [[l op r] args] (emit-binary op l r))))
         :mul (fn [& args] (if (= 1 (count args)) (first args) (let [[l op r] args] (emit-binary op l r))))
         :xp (fn [& args] (if (= 1 (count args)) (first args) (let [[l _ r] args] (emit-binary "^" l r))))
         :cap (fn [& args] (if (= 1 (count args)) (first args) (let [[l op r] args] (emit-binary op l r))))
         :uop (fn ([x] x) ([op x] (str op x)))
         :ndx (fn ([x] x) ([f arg] (if (and (string? arg) (clojure.string/starts-with? arg "[")) (str f arg) (str f "(" arg ")"))))
         :arglist (fn [& args] (clojure.string/join "," (remove nil? args)))
         :aref (fn [& args] (str "[" (clojure.string/join "," (remove nil? args)) "]"))}
        ast))
;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defn sc->snobol4 [sc-src] (let [ast (parse-sc-expr sc-src)] (if (insta/failure? ast) (throw (ex-info "Snocone parse error" {:failure ast :src sc-src})) (emit-expr ast))))
