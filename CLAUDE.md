# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

snobol4jvm is a SNOBOL4/SPITBOL implementation for the JVM in Clojure. The Leiningen project is `SNOBOL4clojure 0.2.0`, on Clojure 1.10.3 with instaparse. Every namespace lives under `SNOBOL4clojure.*`.

## Commands

Run every command from the repo root. Tests open corpus files by relative path (`corpus/gimpel/SNOBOL4`, `corpus/aisnobol`, `corpus/spitbol`).

```bash
lein test                                          # whole suite
lein test SNOBOL4clojure.test-match                # one namespace (file test/SNOBOL4clojure/test_match.clj)
lein test :only SNOBOL4clojure.test-match/test-search-basic   # one deftest
lein test SNOBOL4clojure.catalog.t_arith           # catalog namespaces keep the underscore
lein check                                         # compile every namespace, reflection warnings
lein run prog.sno < input.txt                      # run a program; INPUT reads stdin
lein uberjar && java -jar target/uberjar/SNOBOL4clojure-0.2.0-standalone.jar prog.sno
lein run -m SNOBOL4clojure.bench                   # engine × backend benchmark grid
```

- **`-main` takes the program as a file argument.** `SNOBOL4clojure.core/-main` slurps its first argument and runs it on the Stage 3 VM (`vm/run-program!`). With no argument it prints usage and exits 1. The README's `lein run < program.sno` therefore does not work.
- **The uberjar AOT-compiles only `SNOBOL4clojure.main`,** a launcher that requires no other namespace. At runtime it `require`s `core` from source. This keeps the Greek and symbol names (`Σ+`, `?=` and others) out of AOT. Do not add `:aot` for other namespaces.

## Architecture

### Front end (source to IR)

`compiler/CODE!` does the following, in order:

1. Splits the source into statements:
   - `;` separates statements;
   - a line starting with `+` or `.` continues the previous one;
   - `*` lines are comments.
2. Expands `-INCLUDE` (`preprocess-includes`).
3. Parses each statement with the instaparse grammar in `grammar.clj`.
4. Turns the parse tree into IR with `emitter.clj`.

The IR is plain homoiconic Clojure data: prefix lists such as `(= I (+ I 1))`, together with a goto map `{:S label :F label :G label}`. `compiler/CODE` loads that IR into **global atoms** in `env.clj`:

- `<CODE>`: label or statement number to statement;
- `<STNO>`: label to number;
- `<LABL>`: number to label;
- `STNO`: the running counter.

`CODE` appends to that store; it does not replace it. Runtime `CODE()` depends on this, and it is also why state leaks between runs. The IR is pure EDN, so `CODE-memo` (in memory) and `CODE-cached` / `compile-to-file` (on disk) cache it and skip parsing.

### Four execution engines

All four engines read the same IR, and each one evaluates statement bodies with `operators/EVAL!`:

| Stage | Namespace | Mechanism |
|-------|-----------|-----------|
| 1 | `runtime.clj` (`RUN`) | Interpreter. A `loop` over the statement store resolves gotos (labels, strings, numbers, and variables that hold labels) and counts `&STCOUNT` against `&STLIMIT`. Every feature is proven here first. |
| 2 | `transpiler.clj` | Emits a Clojure namespace with one `run!` function, a `loop`/`case` over statement indices. |
| 3 | `vm.clj` | Flat integer bytecode with 7 opcodes (`HALT`, `EXEC`, `EXEC-S`, `EXEC-F`, `EXEC-SF`, `JUMP`, `SIGNAL`). `-main` uses this engine. |
| 4 | `jvm_codegen.clj` | Writes a real `.class` file with `clojure.asm` (Clojure's bundled ASM, not an external dependency) and loads it with `DynamicClassLoader`. Generated code calls back through a static `IFn` field (`EVAL_FN`), so its constant pool never names `operators`. |

Control flow (`RETURN`, `FRETURN`, `NRETURN`, `END`, the step limit) travels as `ExceptionInfo` whose `ex-data` carries `:snobol/signal`. The `snobol-*!` helpers in `env.clj` throw these signals. Every engine and every test helper catches them by that key.

### Evaluator and pattern engine

- **`operators.clj`** is the live evaluator. `EVAL!` walks the IR, and `INVOKE` is the single dispatch point from operator or function name to its implementation (both cases are registered). It shadows `clojure.core` operators (`= ? | $ . + - * / % !`), so inside this file, comparison must be written `clojure.core/=`.
- **`invoke.clj`** is an older copy of `INVOKE`/`EVAL!` that nothing requires. **`errors.clj`** is likewise required by nothing. Edit `operators.clj`.
- **`functions.clj`** holds the built-ins (`SIZE`, `REPLACE`, `DATA`, `CONVERT`, `SORT` and others). Variables, keywords (`&ANCHOR`, `&TRIM` and the rest), I/O channels, `DATATYPE`, `TABLE`/`ARRAY` and `GLOBALS` (which resets all runtime state) live in **`env.clj`**. `env.clj` has no dependencies inside the project.
- **Patterns are data.** `patterns.clj` builds them as lists and symbols (`ANY`, `ARBNO`, `FENCE`, `BAL` and others). `primitives.clj` holds the pure scanners: each takes `[Σ Δ Π]` and returns `[σ δ]`, where a negative position means failure.
- **`match.clj` (`engine`) is the backtracking matcher.** It is one `loop`/`case` over a 7-element frame `[Σ Δ σ δ Π φ Ψ]` and the actions `:proceed`, `:recede`, `:succeed` and `:fail` (the Byrd-box model). `engine_frame.clj` holds the frame accessors. `MATCH`, `SEARCH`, `FULLMATCH` and `REPLACE` are defined in `match.clj`. `match_api.clj` is used only by the orphaned `invoke.clj`.
- **`trace.clj`** implements `TRACE`, `STOPTR`, `&TRACE` and `&FTRACE`. Label traces fire from `runtime.clj`; pattern traces fire from `match.clj` through a dynamic var.

### Other namespaces

- **Snocone front end:** `snocone.clj` (lexer), `snocone_grammar.clj` (expression grammar) and `snocone_emitter.clj` (emits SNOBOL4 source text, not IR).
- **`harness.clj`** shells out to SPITBOL and CSNOBOL4 and diffs their output against this engine's. The binaries default to `/usr/local/bin/spitbol` and `/usr/local/bin/snobol4` through `^:dynamic` vars (`*spitbol-bin*`, `*csnobol4-bin*`); rebind them to point at a real oracle.
- **`generator.clj`** is the "worm" program generator. Its `rand-*` functions are probabilistic and its `gen-*` functions are exhaustive lazy sequences.
- **`bench.clj`** and `bench/run_benchmarks.clj` drive the benchmarks through the harness.

## Rules the code depends on

These come from the README's "Design Laws", plus traps found in the code.

- **SNOBOL4 names are UPPERCASE everywhere,** for functions, keywords and pattern primitives alike.
- **`nil` means failure and `ε` (the empty string) means success with an empty value.** Never conflate the two.
- **`INVOKE` receives arguments already evaluated.** Never call `EVAL!` on them inside `INVOKE`.
- **`match.clj` stays a single `loop`/`case`.** Do not split the engine.
- **`TABLE` and `ARRAY` are mutable through atoms;** everything else is immutable.
- **`(is (= expected actual))` cannot fail in 33 test namespaces, so a green `lein test` does not yet mean the suite passes.**
  - **Why:** `(:require [SNOBOL4clojure.core :refer :all])` without `:exclude` replaces `clojure.core/=` by SNOBOL4's IR builder, and `lein test` prints a `WARNING: = already refers to …` line for each such namespace. There, `(= a b)` returns the list `(= a b)`. That list is always truthy, so `is` reports a pass whatever the values are. Measured: `(is (= 1 2))` returns `(= 1 2)` with no FAIL.
  - **How many (measured 2026-10-17 at `407cc01`):** `lein test` reads 2,022 tests, 0 failures. Rewriting the 1,654 `(is (= …))` lines in 31 files to `clojure.core/=` turns that into 366 failures in 285 tests. The largest groups are `test_csnobol4_suite` (101), `test_sprint12` (40), `test_cooper` (28), `catalog/t_spitbol` (28), `test_worm1000` (20), `catalog/t_gimpel` (16) and `catalog/t_aisnobol` (12).
  - **Which tests really compare:** files that write `clojure.core/=` (`test_worm_micro`, `catalog/t_patterns_ext`), files that refer core with `:exclude [= + - * / num]` (`core_test`), and files that don't refer core at all.
  - **For any test you write or fix:** use `clojure.core/=`, or exclude the operators the way `core_test.clj` does. `+`, `-`, `*` and `/` are shadowed in the same way.
- **Runtime state is global and is not reset between tests by default.** `run-with-timeout` and `prog` do not call `GLOBALS`. Namespaces that need a clean slate do it themselves with `(use-fixtures :each (fn [t] (GLOBALS) (t)))`, or reset `STNO`, `<STNO>`, `<LABL>` and `<CODE>` the way `run-to-step` does. Because of this shared state, tests cannot run in parallel.

## Tests

`test/SNOBOL4clojure/test_helpers.clj` is the harness. Every test that runs a program must use one of its wrappers, because a SNOBOL4 program can loop forever:

- `prog`: default budget, 2000 ms;
- `prog-timeout <ms>`: a set budget;
- `prog-steplimit`: a budget plus an in-engine step limit;
- `prog-infinite`: asserts that the program times out;
- `prog-include`: resolves `-INCLUDE`;
- `run-with-timeout`: returns `{:exit :ok|:error|:timeout|:step-limit :stdout :stderr}`.

A timeout is retried once. Programs run in a `future`, so `binding [*in* …]` reaches them; this is how stdin tests feed data, as in `test_csnobol4_suite.clj`. For debugging, `run-to-step`, `probe-at`, `bisect-divergence`, `probe-test` and `run-with-restart` stop a program after N statements and snapshot its variables.

Test groups:

- **`test_worm*` and `catalog/`:** hand-encoded semantic facts, one per `deftest`.
- **`test_cooper`:** ported from Cooper's Snobol4.Net tests.
- **`test_csnobol4_suite`:** Budne's CSNOBOL4 suite, with program text inlined.
- **`test_sprintN`:** feature tests by sprint.

The `:expected-fail` metadata that `test_csnobol4_suite`'s docstring mentions is not implemented anywhere.

## Repo notes

- **`corpus/lon` is a git submodule** (SNOBOL4-plus/SNOBOL4-corpus) and is uninitialised in a fresh clone. `corpus/gimpel`, `corpus/aisnobol`, `corpus/spitbol` and `corpus/beauty` are tracked directly.
- **Leftovers, not live code:** `src/probe` and `src/t4probe` are scratch probes. `t4probe/main.clj` declares `(ns t4probe)`, which does not match its path. `probe/main.clj` requires `test-helpers` from `test/`. Also leftover are `misc/` (editor syntax files and scratch), `src/SNOBOL4clojure/generator.clj.bak`, `CHANGELOG.md` and `doc/intro.md` (both still the Leiningen template).
- **The README's numbers are stale.** It claims "1,896 tests / 4,120 assertions". A full run at `407cc01` reads 2,022 tests and 4,246 assertions in about 70 s, but see the `=` trap above. The benchmark tables are likewise claims, not measurements.
