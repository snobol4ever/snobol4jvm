(defproject SNOBOL4clojure "0.2.0"
  :description "SNOBOL4 pattern matching."
  :url ""
  :license {:name "AGPL-3.0-or-later"
            :url "https://www.gnu.org/licenses/agpl-3.0.html"}
  :dependencies [
    [org.clojure/clojure "1.10.3"]
    [org.clojure/tools.trace "0.7.11"]
  ; [org.clojure/math.numeric-tower "0.0.4"]
  ; [aysylu/loom "1.0.2"]
    [instaparse "1.4.10"]
    [net.mikera/core.matrix "0.62.0"]
  ; [expresso "0.2.2"]
    [criterium "0.4.6"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :main ^:skip-aot SNOBOL4clojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:main SNOBOL4clojure.main
                       :aot [SNOBOL4clojure.main SNOBOL4clojure.core]
                       ;; core and every namespace it requires are AOT-compiled, so a
                       ;; run starts in about 1.4 s instead of 4-9 s compiling .clj
                       ;; source. Its classes carry no two names that differ
                       ;; only in case (Σ/σ and the like), so the build is safe on a
                       ;; case-insensitive filesystem too.
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
