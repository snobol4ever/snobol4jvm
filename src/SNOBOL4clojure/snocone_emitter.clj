(ns SNOBOL4clojure.snocone-emitter
  "Snocone expression emitter — Step 2.

  Transforms an instaparse AST from snocone-grammar into a SNOBOL4
  source string, mirroring dprint/sprint in the reference snocone.snobol4.

  Operator mapping (from the operator table):
    infix ops:  = ? | + - / * ^ . $   → same in SNOBOL4
    fn ops:     == != < > <= >= :: :!: :>: :<: :>=: :<=: :==: :!=:
                → EQ NE LT GT LE GE IDENT DIFFER LGT LLT LGE LLE LEQ LNE
    special:    ||  → (a,b)  pattern alternation
                &&  → blank concat (space between operands)
                ^   → **
                %   → REMDR(a,b)"
  (:require [instaparse.core :as insta]
            [SNOBOL4clojure.snocone-grammar :refer [parse-sc-expr]]))

;; ---------------------------------------------------------------------------
;; Operator tables (from the operator table)
;; ---------------------------------------------------------------------------

;; fn=1 ops → emit as FN(l,r)
(def ^:private fn-ops
  {">"   "GT"  "<"   "LT"  ">="  "GE"  "<="  "LE"
   "=="  "EQ"  "!="  "NE"
   "::"  "IDENT"  ":!:"  "DIFFER"
   ":>:" "LGT" ":<:" "LLT" ":>=:" "LGE" ":<=:" "LLE"
   ":==:" "LEQ" ":!=:" "LNE"
   "%"   "REMDR"})

;; infix ops → emit as  l OP r
(def ^:private infix-ops
  #{"=" "?" "|" "+" "-" "/" "*" "." "$"})

;; ---------------------------------------------------------------------------
;; Emitter — insta/transform over the AST
;; ---------------------------------------------------------------------------

(defn- emit-binary [op l r]
  (cond
    (= op "||") (str "(" l "," r ")")          ; pattern alternation
    (= op "&&") (str l " " r)                   ; blank concat
    (= op "^")  (str l " ** " r)               ; exponentiation
    (fn-ops op) (str (fn-ops op) "(" l "," r ")")
    (infix-ops op) (str l " " op " " r)
    :else (str l " " op " " r)))

(defn emit-expr
  "Transform a snocone-grammar AST node into a SNOBOL4 source string."
  [ast]
  (insta/transform
    {;; Terminals
     :ident   str
     :integer str
     :real    (fn [s] (if (clojure.string/starts-with? s ".")
                        (str "0" s)   ; dotck: .5 → 0.5
                        s))
     :string  str

     ;; Binary operators — each rule has shape (rule op? l r)
     ;; instaparse gives us the children after angle-bracket suppression
     :asn  (fn
             ([x] x)
             ([l r] (emit-binary "=" l r)))
     :qst  (fn
             ([x] x)
             ([l r] (emit-binary "?" l r)))
     :pip  (fn
             ([x] x)
             ([l & rest] (reduce #(emit-binary "|" %1 %2) l rest)))
     :or   (fn
             ([x] x)
             ([l & rest] (reduce #(emit-binary "||" %1 %2) l rest)))
     :cat  (fn
             ([x] x)
             ([l & rest] (reduce #(emit-binary "&&" %1 %2) l rest)))
     :cmp  (fn
             ([x] x)
             ([l op r] (emit-binary op l r)))
     :sum  (fn
             ([x] x)
             ([l op r] (emit-binary op l r)))
     :mul  (fn
             ([x] x)
             ([l op r] (emit-binary op l r)))
     :xp   (fn
             ([x] x)
             ([l r] (emit-binary "^" l r)))
     :cap  (fn
             ([x] x)
             ([l op r] (emit-binary op l r)))

     ;; Unary operators
     :uop  (fn
             ([x] x)
             ([op x] (str op x)))

     ;; Indexing — f(args) or f[args]
     ;; ndx children: base, then arglist children interleaved
     ;; instaparse gives (ndx base arglist) for f(...) and f[...]
     ;; We need to distinguish ( vs [ — captured via grammar tags
     :ndx  (fn
             ([x] x)
             ([f & arglists]
              ;; arglists is a seq of arglist results (comma-joined arg strings)
              (str f "(" (clojure.string/join "," arglists) ")")))

     :arglist (fn [& args] (clojure.string/join "," (remove nil? args)))}
    ast))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn sc->snobol4
  "Parse a Snocone expression string and return the SNOBOL4 source string."
  [sc-src]
  (let [ast (parse-sc-expr sc-src)]
    (if (insta/failure? ast)
      (throw (ex-info "Snocone parse error" {:failure ast :src sc-src}))
      (first (emit-expr ast)))))
