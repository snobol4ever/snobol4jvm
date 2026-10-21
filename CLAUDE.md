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

- `lein test` output holds control bytes from the CSNOBOL4 suite, so `grep` calls it binary and prints nothing. Read it with `grep -a` (in a shell where `grep` is a wrapper that adds `-I`, use `command grep -a`).
- **`-main` takes the program as a file argument:** `[--engine interp|vm] <file.sno>`. The default is the Stage 1 interpreter, the engine the test suite runs; `--engine vm` runs the Stage 3 VM, which drops every statement after a `DEFINE`'s goto. `-INCLUDE` resolves against the program's own directory, then `$SNO_LIB`, then the current one. With no file it prints usage and exits 1.
- **The `:uberjar` profile AOT-compiles `SNOBOL4clojure.main` and `core`,** and so every namespace `core` requires. A run then starts in about 1.4 s instead of 4–9 s. `project.clj` notes that no two generated classes differ only in case (`Σ`/`σ` and the like), so the build is safe on a case-insensitive filesystem. `main.clj` is still a thin launcher that `require`s `core` at run time, and its docstring (Greek-symbol namespaces stay out of AOT) is out of date.

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
| 3 | `vm.clj` | Flat integer bytecode with 7 opcodes (`HALT`, `EXEC`, `EXEC-S`, `EXEC-F`, `EXEC-SF`, `JUMP`, `SIGNAL`). `-main --engine vm` runs it; it drops every statement after a `DEFINE`'s goto. |
| 4 | `jvm_codegen.clj` | Writes a real `.class` file with `clojure.asm` (Clojure's bundled ASM, not an external dependency) and loads it with `DynamicClassLoader`. Generated code calls back through a static `IFn` field (`EVAL_FN`), so its constant pool never names `operators`. |

Control flow (`RETURN`, `FRETURN`, `NRETURN`, `END`, the step limit) travels as `ExceptionInfo` whose `ex-data` carries `:snobol/signal`. The `snobol-*!` helpers in `env.clj` throw these signals. Every engine and every test helper catches them by that key.

Errors travel the same way, as `ex-info` with `{:snobol/error N}`. `N` is SPITBOL's own error number (`22` undefined function, `29` undefined operator, `238` bad array subscript, and so on), so a divergence from the oracle's `ERROR nnn` is fixed by raising the same number. `core_test.clj` asserts on it.

### Evaluator and pattern engine

- **`operators.clj`** is the live evaluator. `EVAL!` walks the IR, and `INVOKE` is the single dispatch point from operator or function name to its implementation (both cases are registered). It shadows `clojure.core` operators (`= ? | $ . + - * / % !`), so inside this file, comparison must be written `clojure.core/=`.
- **`invoke.clj`** is an older copy of `INVOKE`/`EVAL!` that nothing requires. **`errors.clj`** is likewise required by nothing. Edit `operators.clj`.
- **`functions.clj`** holds the built-ins (`SIZE`, `REPLACE`, `DATA`, `CONVERT`, `SORT` and others). Variables, keywords (`&ANCHOR`, `&TRIM` and the rest), I/O channels, `DATATYPE`, `TABLE`/`ARRAY` and `GLOBALS` (which resets all runtime state) live in **`env.clj`**. `env.clj` has no dependencies inside the project.
- **Patterns are data.** `patterns.clj` builds them as lists and symbols (`ANY`, `ARBNO`, `FENCE`, `BAL` and others). `primitives.clj` holds the pure scanners: each takes `[Σ Δ Π]` and returns `[σ δ]`, where a negative position means failure.
- **`match.clj` (`engine`) is the backtracking matcher.** It is one `loop`/`case` over a 7-element frame `[Σ Δ σ δ Π φ Ψ]` and the actions `:proceed`, `:recede`, `:succeed` and `:fail` (the Byrd-box model). `engine_frame.clj` holds the frame accessors. `MATCH`, `SEARCH`, `FULLMATCH` and `REPLACE` are defined in `match.clj`. `match_api.clj` is used only by the orphaned `invoke.clj`.
- **`trace.clj`** implements `TRACE`, `STOPTR`, `&TRACE` and `&FTRACE`. Label traces fire from `runtime.clj`; pattern traces fire from `match.clj` through a dynamic var.

### Other namespaces

- **Snocone front end:** `snocone.clj` (lexer), `snocone_grammar.clj` (expression grammar) and `snocone_emitter.clj` (emits SNOBOL4 source text, not IR).
- **`harness.clj`** shells out to SPITBOL and CSNOBOL4 and diffs their output against this engine's. The binaries default to `/usr/local/bin/spitbol` and `/usr/local/bin/snobol4` through `^:dynamic` vars (`*spitbol-bin*`, `*csnobol4-bin*`); rebind them to point at a real oracle. On the fleet machines `/usr/local/bin/snobol4` is absent and `/usr/local/bin/spitbol` is not the graded oracle, so rebind both; the correctness oracle there is `/home/resources/x64/bin/sbl -bf prog.sno` (`-b` drops the signon, `-f` leaves source case alone, which is what this engine does and what the corpus refs were cut with). Where the oracle and this engine disagree, the oracle wins unless Lon rules otherwise.
- **`generator.clj`** is the "worm" program generator. Its `rand-*` functions are probabilistic and its `gen-*` functions are exhaustive lazy sequences.
- **`bench.clj`** and `bench/run_benchmarks.clj` drive the benchmarks through the harness.

## Rules the code depends on

These come from the README's "Design Laws", plus traps found in the code.

- **SNOBOL4 names are UPPERCASE everywhere,** for functions, keywords and pattern primitives alike.
- **`nil` means failure and `ε` (the empty string) means success with an empty value.** Never conflate the two.
- **`INVOKE` receives arguments already evaluated.** Never call `EVAL!` on them inside `INVOKE`.
- **`match.clj` stays a single `loop`/`case`.** Do not split the engine.
- **`TABLE` and `ARRAY` are mutable through atoms;** everything else is immutable.
- **A test namespace that refers `SNOBOL4clojure.core` or `operators` must exclude the shadowed operators:** `[SNOBOL4clojure.core :refer :all :exclude [= + - * / num]]`, as `core_test.clj` and every such namespace now does.
  - **Why:** without `:exclude`, `=` is SNOBOL4's IR builder, so `(= a b)` returns the list `(= a b)`. That list is always truthy, so `(is (= 1 2))` reports a pass. Until `1fe23d8` 35 of the 56 test namespaces were written that way, and `lein test` read 0 failures over a suite that really had about 366. The `WARNING: = already refers to …` line in `lein test` output is the sign of it. `test_operators` refers `operators :all` behind `:refer-clojure :exclude`, so it gave no warning.
  - **Predicates:** a lambda or predicate passed to a helper such as `bisect-divergence` follows the same rule; `test_helpers.clj`'s docstring has the example.
  - **`lein test` is red, and its reds are real.** The count is in the LIVE CURSOR of `.github/GOAL-HQ-JVM.md` (188 failures in 2,056 tests at `72da52d`, measured 2026-10-21; 98 of them in `test_csnobol4_suite`, 30 in `catalog/t_aisnobol`, 19 in `catalog/t_gimpel`), and it moves with every cure. Fix the engine, or set an expectation to what SPITBOL prints; never weaken a test to pass. `csnobol4-match4` flips between red and green across full runs, a shared-state flake (see the next bullet).
- **Runtime state is global and is not reset between tests by default.** `run-with-timeout` and `prog` do not call `GLOBALS`. Namespaces that need a clean slate do it themselves with `(use-fixtures :each (fn [t] (GLOBALS) (t)))`, or reset `STNO`, `<STNO>`, `<LABL>` and `<CODE>` the way `run-to-step` does. Because of this shared state, tests cannot run in parallel.

## Tests

`test/SNOBOL4clojure/test_helpers.clj` is the harness. Every test that runs a program must use one of its wrappers, because a SNOBOL4 program can loop forever:

- `prog`: default budget, 2000 ms;
- `prog-timeout <ms>`: a set budget;
- `prog-steplimit`: a budget plus an in-engine step limit;
- `prog-infinite`: asserts that the program times out;
- `prog-include`: resolves `-INCLUDE`;
- `run-with-timeout`: returns `{:exit :ok|:error|:timeout|:step-limit :stdout :stderr}`.

A timeout is retried once. Programs run in a `future`, so `binding [*in* …]` reaches them; this is how stdin tests feed data, as in `test_csnobol4_suite.clj`. A relative file name in `INPUT()`/`OUTPUT()` resolves against `env/*file-dir*` when it is bound, and against the process's working directory when it is `nil` (a JVM cannot change its own). `test_csnobol4_suite` binds it to a temp directory, so `lein test` writes nothing into the tree. For debugging, `run-to-step`, `probe-at`, `bisect-divergence`, `probe-test` and `run-with-restart` stop a program after N statements and snapshot its variables.

Test groups:

- **`test_worm*` and `catalog/`:** hand-encoded semantic facts, one per `deftest`.
- **`test_cooper`:** ported from Cooper's Snobol4.Net tests.
- **`test_csnobol4_suite`:** Budne's CSNOBOL4 suite, with program text inlined.
- **`test_sprintN`:** feature tests by sprint.

The `:expected-fail` metadata that `test_csnobol4_suite`'s docstring mentions is not implemented anywhere.

## Repo notes

- **`corpus/lon` is a git submodule** (SNOBOL4-plus/SNOBOL4-corpus) and is uninitialised in a fresh clone. `corpus/gimpel`, `corpus/aisnobol`, `corpus/spitbol` and `corpus/beauty` are tracked directly.
- **Leftovers, not live code:** `src/probe` and `src/t4probe` are scratch probes. `t4probe/main.clj` declares `(ns t4probe)`, which does not match its path. `probe/main.clj` requires `test-helpers` from `test/`. Also leftover are `misc/` (editor syntax files and scratch), `src/SNOBOL4clojure/generator.clj.bak`, `CHANGELOG.md` and `doc/intro.md` (both still the Leiningen template).
- **The README's numbers are stale.** Its badge and text claim "1,896 tests / 0 failures" and "4,120 assertions", which match no current run (see the `lein test` bullet above for the count). A full run takes about 70 s. The benchmark tables are likewise claims, not measurements.
