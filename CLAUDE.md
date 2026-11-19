# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

snobol4jvm is a SNOBOL4/SPITBOL implementation for the JVM in Clojure. The Leiningen project is `SNOBOL4clojure 0.2.0`, on Clojure 1.10.3 with instaparse. Every namespace lives under `SNOBOL4clojure.*`.

**It is SNOBOL4++: SPITBOL's semantics with every extension, and no SPITBOL restriction made for speed.** Lon ruled this on 2026-11-17 18:1x CST, in the JVM window: *"I think we should make snobol4jvm a bit more special that SPITBOL. Since SPITBOL restricts OPSYN for optimization. But Clojure will not see any of that optimization so let's do more of a SNOBOL4++ where it has all extensions and keeps the dynamic power of SNOBOL4. Have the INPUT and HOST be just like SPITBOL."* It amends his word of 2026-10-23 (*"The snobol4jvm will act like SPITBOL, not CSNOBOL4."*), which stands everywhere the ruling does not reach. Asked, he chose:

- **Extensions are allowed.** Where `sbl -bf` ends in an error caused only by a SPITBOL restriction that CSNOBOL4 or SNOBOL4 lifts, snobol4jvm runs on. Examples: OPSYN of any operator, unary `^` among them; CSNOBOL4's `&DIGITS`; `FUNCTION`, `LABEL` and `VDIFFER`.
- **Such an entry is declared per engine.** Its SPITBOL ref stands, and it leaves snobol4jvm's denominator alone through `EXCLUDED.jvm.tsv` beside its package, class `SNOBOL4PP_EXTENSION_KEPT_BY_LON`, with the oracle's answer. SCRIP's rows never read it.
- **I/O and the host are exactly SPITBOL's:** `INPUT`, `OUTPUT`, `TERMINAL`, `HOST`.
- **No case folding, ever.** Lon, the same hour: *"We WILL NEVER support case-folding. We will always have upper-case built-in keywords and functions."* Built-in keywords, functions and control cards are upper case only, and `&trim` or a lower-case `-include` is answered as `sbl -bf` answers it. `-CASE` and `&CASE` fold nothing (1e740ad).

The oracle is `/home/resources/x64/bin/sbl -bf prog.sno`:

- `-b` drops the signon;
- `-f` leaves source case alone, as this engine does and as the corpus refs were cut.

Where the engine and the oracle disagree, the oracle wins unless the difference is an extension the rulings above keep. Old code and tests still carry CSNOBOL4 or Catspaw dialect (for example `t_io_channels` and the Budne programs in `test_csnobol4_suite`). Where the two dialects differ, the test's expectation is what `sbl -bf` prints, unless snobol4jvm keeps the CSNOBOL4 extension. Before changing a construct, read its section of the SPITBOL manual (`/home/claude_ceo/.tools/docs/spitbol-manual-v3.7.txt`; Appendix D lists the errors).

## Commands

Run every command from the repo root. Tests open the repo's own `corpus/` by relative path (`corpus/gimpel/SNOBOL4`, `corpus/aisnobol`, `corpus/spitbol`). Do not confuse it with the fleet corpus `../corpus`, which the harness boards grade.

```bash
lein test                                          # whole suite, about a minute
lein test SNOBOL4clojure.test-match                # one namespace (file test/SNOBOL4clojure/test_match.clj)
lein test :only SNOBOL4clojure.test-match/test-search-basic   # one deftest
lein test SNOBOL4clojure.catalog.t_arith           # catalog namespaces keep the underscore
lein check                                         # compile every namespace, reflection warnings
lein run prog.sno < input.txt                      # run a program; INPUT reads stdin
lein run -m SNOBOL4clojure.harness                 # engine × backend benchmark grid

# what the harness boards run: an uberjar plus an AppCDS archive, then the jar in the boards' calling shape
bash ../harness/scripts/build_snobol4jvm.sh
timeout -k 2 30 bash ../harness/scripts/run_snobol4jvm.sh prog.sno < input.txt > jvm.out
timeout -k 2 30 /home/resources/x64/bin/sbl -bf prog.sno < input.txt > sbl.out; cmp jvm.out sbl.out
```

- **`lein test` output holds control bytes** from the CSNOBOL4 suite, so `grep` treats it as binary and prints nothing. Read it with `grep -a`. In a shell where `grep` is a wrapper that adds `-I`, use `command grep -a`.
- **Compare whole outputs with `cmp`.** A check that counts only the engine's extra lines (`diff | grep '^>'`) reads 0 for a program that crashed with empty output.
- **Compare against the oracle through the jar, not `lein run`.** `lein run` calls `core/-main` directly, without `main.clj`'s ISO-8859-1 streams, so bytes above 127 come out differently.
- **The machine is shared with the SCRIP fleet:**
  - start every engine run under `timeout -k 2 <secs>`;
  - run it in the foreground, one JVM at a time;
  - a hang left behind in the background burns a core until someone kills it.
- **`-main` arguments:** `[--engine interp|vm] [-u STR] <file.sno> [program args…]`.
  - The default engine is the Stage 1 interpreter, the one the test suite runs.
  - `--engine vm` runs the Stage 3 VM, which drops every statement after a `DEFINE`'s goto.
  - `-u STR` is what `HOST(0)` returns. Without it, `HOST(0)` joins the program arguments.
  - `-INCLUDE` resolves against the program's own directory, then `$SNO_LIB`, then the current directory.
  - A program with a compile error is not run, and `-main` returns 231, as `sbl -bf` exits.
  - Output the program did not write itself (the listing, a compile error's page, an execution error's report, the `&DUMP` dump) goes to the file `$SCRIP_SNO_LIST_SINK` names, if it is set. This is how the harness names a port's listing file. Standard error gets its share either way.
- **`main.clj` is the uberjar's entry point.** It reads and writes the streams as ISO-8859-1 (a program is bytes), `require`s `core` at run time and exits with `-main`'s code.
  - The `:uberjar` profile AOT-compiles `main` and `core`, and with them every namespace `core` requires. A run then starts in about 1.4 s, or about 1.0 s with the archive `build_snobol4jvm.sh` records.
  - `main.clj`'s docstring says the Greek-symbol namespaces stay out of AOT. That is out of date.

## Running the harness boards

This seat runs its own boards (CEO-1912). The engine arm is `../harness/scripts/run_snobol4jvm.sh`.

- **A board refuses a stale jar.** It refuses a jar older than any `src/**/*.clj` or `project.clj`, so rebuild with `build_snobol4jvm.sh` first.
- **A board refuses a moved or dirty tree.** It pins `/home/claude_jvm/snobol4jvm`'s tree at the start and writes nothing ("THE GROUND MOVED UNDER THIS RUN") if HEAD moves or the tree goes dirty before the end.
  - While a board runs, do not edit, commit or pull in the root clone.
  - Develop and run `lein test` in a worktree or clone under `/home/claude_jvm/.scratch/`, and push from there.
  - Fast-forward the root and rebuild its jar between runs.
- **Run one board at a time,** at nice 19 under `timeout -k`, calling the runner by its absolute path. Under a port engine the runner changes to the root, so a relative path breaks its row publish:
  `S4E_ENGINE=jvm nice -n 19 timeout -k 5 3600 bash /home/claude_jvm/harness/scripts/test_snobol4_gimpel_suite.sh`.
  - Add `S4E_CHUNK=chunk:NN` to run one part.
  - The other runners are `test_snobol4_{aisnobol,csnobol4,dotnet,spitbol_testpgms,spitbol_x32,spitbol_x64,bench}_suite.sh`, `test_snoflake_suite.sh` and `test_demos_suite.sh`.
  - SnoRungs (`../corpus/tests/snobol4/ALL.sno`) runs through `corpus_suite_harness.py run … --partition chunk:NN|entry:A,entry:B`. A whole pass takes about five hours.
  - If a row does not publish, run `python3 harness/scripts/util_score_row.py from-progress --suite <key>@jvm` from `/home/claude_jvm`.

## Architecture

### Source layout

`src/SNOBOL4clojure/` holds eight files, one namespace each. In load order, each requires only the files above it:

| File | Holds |
|------|-------|
| `env.clj` | The runtime: its state (variables, keywords, channels, `ARRAY`, `TABLE`, `DATATYPE`, `GLOBALS`), the error texts, the built-in functions, the pattern constructors and scanners, `TRACE`, and what `sbl -bf` writes besides the program's output (the listing, an error's report, the `&DUMP` dump). |
| `compiler.clj` | The front end: the grammar, the compile error numbers, the emitter and the compiler. |
| `operators.clj` | The matcher (`engine`) and the evaluator (`EVAL!`, `INVOKE` and the operators). |
| `runtime.clj` | The four execution engines. |
| `core.clj` | The public facade, which re-exports the API, and `-main`. |
| `main.clj` | The uberjar's entry point. |
| `snocone.clj` | The Snocone front end. |
| `harness.clj` | Development tools: the differential harness, the worm generator and the benchmark grid. |

Each file is divided into sections, and each section opens with a `;; ═══` banner that names it. The section names below (`grammar`, `trace`, `match` and so on) are the files these sections were before 2026-11-19.

### Front end (source to IR)

`compiler/CODE!` does the following, in order:

1. Splits the source into statements:
   - `;` separates statements;
   - a line starting with `+` or `.` continues the previous one;
   - `*` lines are comments;
   - control cards (`-LIST`, `-CASE` and the rest) are handled here.
2. Expands `-INCLUDE` (`preprocess-includes`).
3. Parses each statement with the instaparse grammar (`compiler.clj`, section grammar).
4. Turns the parse tree into IR (section emitter). Compilation stops at `END`, so data kept after it is never compiled.

**Compile errors are collected, not thrown.** They accumulate in `env/<DIAGNOSTICS>` as `[line error column]`, and a statement that does not compile becomes an error node.

- **The error number:** when the parser rejects a statement, `compiler.clj`'s section syntax rescans it the way SPITBOL's scanner does and names the error `sbl -bf` would report (212 to 234). If the scan finds none, the error is 220.
- **Constant folding:** `CODE!` evaluates the constant parts of each statement as SPITBOL's compiler does (`fold`), so `X = 1 / 0` is a compile error.
- **Who reads `<DIAGNOSTICS>`:** `-main` refuses to run the program, and runtime `CODE()` and `EVAL` fail with `&ERRTYPE` set.

The IR is plain homoiconic Clojure data: prefix lists such as `(= I (+ I 1))`, together with a goto map `{:S label :F label :G label}`. `compiler/CODE` loads that IR into **global atoms** in `env.clj`:

- `<CODE>`: label or statement number to statement;
- `<STNO>`: label to number;
- `<LABL>`: number to label;
- `STNO`: the running counter.

`CODE` appends to that store; it does not replace it. Runtime `CODE()` depends on this, and it is also why state leaks between runs. The IR is pure EDN, so `CODE-memo` (in memory) and `CODE-cached` / `compile-to-file` (on disk) cache it and skip parsing.

### Four execution engines

All four engines read the same IR, and each one evaluates statement bodies with `operators/EVAL!`:

All four live in `runtime.clj`, one section each.

| Stage | Section | Mechanism |
|-------|-----------|-----------|
| 1 | runtime (`RUN`) | Interpreter. A `loop` over the statement store resolves gotos (labels, strings, numbers, and variables that hold labels) and counts `&STCOUNT` against `&STLIMIT`. Every feature is proven here first. |
| 2 | transpiler | Emits a Clojure namespace with one `run!` function, a `loop`/`case` over statement indices. |
| 3 | vm | Flat integer bytecode with 7 opcodes (`HALT`, `EXEC`, `EXEC-S`, `EXEC-F`, `EXEC-SF`, `JUMP`, `SIGNAL`). |
| 4 | jvm_codegen | Writes a real `.class` file with `clojure.asm` (Clojure's bundled ASM, not an external dependency) and loads it with `DynamicClassLoader`. Generated code calls back through a static `IFn` field (`EVAL_FN`), so its constant pool never names `operators`. |

Control flow (`RETURN`, `FRETURN`, `NRETURN`, `END`, the step limit) travels as `ExceptionInfo` whose `ex-data` carries `:snobol/signal`. The `snobol-*!` helpers in `env.clj` throw these signals. Every engine and every test helper catches them by that key.

Errors travel the same way, as `ex-info` with `{:snobol/error N}`.

- `N` is SPITBOL's own error number (`22` undefined function, `29` undefined operator, `238` non-integer array subscript, and so on), so a divergence from the oracle's `ERROR nnn` is fixed by raising the same number. `core_test.clj` asserts on it.
- The texts come from `env.clj`'s section errors through `env/error-message`.
- An error that ends the program is reported by `env.clj`'s section fatal in `sbl -bf`'s layout, and exits 0, as `sbl` does. Error 0 is reported as 036.
- At `&ERRLIMIT` 0, an error inside `EVAL` fails `EVAL`, and an error during a pattern match fails the match.
- Above 0, every error is trapped, as `sbl -bf` traps it. The trap spends one of `&ERRLIMIT`, then takes the `SETEXIT` label if one is armed and otherwise fails the statement.

### Evaluator and pattern engine

- **`operators.clj`, section operators, is the evaluator.** `EVAL!` walks the IR, and `INVOKE` is the single dispatch point from operator or function name to its implementation (both cases are registered).
  - It defines the operators `= + - * /` (shadowing `clojure.core`) and `? | $ . % !`, so inside this file, comparison must be written `clojure.core/=`.
- **`env.clj`, section functions,** holds the built-ins (`SIZE`, `REPLACE`, `DATA`, `CONVERT`, `SORT` and others).
- **`env.clj`, section env,** holds the runtime state: variables, keywords (`&ANCHOR`, `&TRIM` and the rest), I/O channels, `DATATYPE`, `TABLE`/`ARRAY` and `GLOBALS`, which resets all runtime state. `env.clj` requires no other file of the project.
- **Patterns are data.** Section patterns builds them as lists and symbols (`ANY`, `ARBNO`, `FENCE`, `BAL` and others). Section primitives holds the pure scanners: each takes `[Σ Δ Π]` and returns `[σ δ]`, where a negative position means failure.
- **`operators.clj`, section match (`engine`), is the backtracking matcher.** It is one `loop`/`case` over a 7-element frame `[Σ Δ σ δ Π φ Ψ]` and the actions `:proceed`, `:recede`, `:succeed` and `:fail` (the Byrd-box model).
  - The frame accessors (`ζΣ`, `ζδ` and the rest) are defined there too.
  - `MATCH`, `SEARCH`, `FULLMATCH` and `MATCH-REPLACE` are defined there as well. `MATCH-REPLACE` is the Clojure API's match-and-replace; SPITBOL's `REPLACE` is the built-in.
- **`env.clj`, section trace,** implements `TRACE`, `STOPTR`, `&TRACE` and `&FTRACE`. Label traces fire from the interpreter; pattern traces fire from the matcher through a dynamic var.
- **What `sbl -bf` writes besides the program's output,** each section of `env.clj` reproducing it line for line:
  - listing: the `-LIST` listing and a compile error's page;
  - fatal: an execution error's report;
  - dump: the `&DUMP` dump, and the value text traces show.

### Other namespaces

- **`snocone.clj`, the Snocone front end:** its sections snocone (lexer), snocone_grammar (expression grammar) and snocone_emitter (emits SNOBOL4 source text, not IR). Only `test_snocone` and `test_snocone_parser` use it; `-main` runs SNOBOL4 source only.
- **`harness.clj`, section harness,** shells out to SPITBOL and CSNOBOL4 and diffs their output against this engine's.
  - The binaries are set by the `^:dynamic` vars `*spitbol-bin*` and `*csnobol4-bin*`, which default to `/usr/local/bin/spitbol` and `/usr/local/bin/snobol4`.
  - On the fleet machines `/usr/local/bin/snobol4` is absent and `/usr/local/bin/spitbol` is not the graded oracle, so rebind both.
- **Section generator** is the "worm" program generator. Its `rand-*` functions are probabilistic and its `gen-*` functions are exhaustive lazy sequences.
- **Section bench** and `bench/run_benchmarks.clj` drive the benchmarks; `lein run -m SNOBOL4clojure.harness` runs its `-main`.

## Rules the code depends on

These come from the README's "Design Laws", plus traps found in the code.

- **SNOBOL4 names are UPPERCASE everywhere,** for functions, keywords and pattern primitives alike.
- **`nil` means failure and `ε` (the empty string) means success with an empty value.** Never conflate the two.
- **`INVOKE` receives arguments already evaluated.** Never call `EVAL!` on them inside `INVOKE`.
- **The matcher (`engine` in `operators.clj`) stays a single `loop`/`case`.** Do not split the engine.
- **No code that uses `clojure.core`'s `=` joins `operators.clj`.** It is the one namespace that defines `= + - * /`, so a bare `(= a b)` there builds IR, which is always truthy. Every other namespace excludes only what it defines itself (`env.clj` excludes `num`).
- **`TABLE` and `ARRAY` are mutable through atoms;** everything else is immutable.
- **A test namespace that refers `SNOBOL4clojure.core` or `operators` must exclude the shadowed operators:** `[SNOBOL4clojure.core :refer :all :exclude [= + - * / num]]`, as `core_test.clj` and every such namespace now does.
  - **Why:** without `:exclude`, `=` is SNOBOL4's IR builder, so `(= a b)` returns the list `(= a b)`. That list is always truthy, so `(is (= 1 2))` reports a pass.
  - **What it cost:** until `1fe23d8`, 35 of the 56 test namespaces were written that way, and `lein test` read 0 failures over a suite that really had about 366.
  - **How to spot it:** a `WARNING: = already refers to …` line in `lein test` output. `test_operators` refers `operators :all` behind `:refer-clojure :exclude`, so it gave no warning.
  - **Predicates:** a lambda or predicate passed to a helper such as `bisect-divergence` follows the same rule; `test_helpers.clj`'s docstring has the example.
- **`lein test` is red, and its reds are real.** Measured on 2026-10-28 at `50caacd`: 2,249 tests, 4,854 assertions, 74 failures, 0 errors, in 57 s. The live count is in the LIVE CURSOR of `/home/claude_jvm/.github/GOAL-HQ-JVM.md` and in the last commit message.
  - Fix the engine, or set an expectation to what SPITBOL prints; never weaken a test to pass.
  - `csnobol4-match4` flips between red and green across full runs, a shared-state flake (see the next bullet).
- **Runtime state is global and is not reset between tests by default.** `run-with-timeout` and `prog` do not call `GLOBALS`.
  - Namespaces that need a clean slate do it themselves with `(use-fixtures :each (fn [t] (GLOBALS) (t)))`, or reset `STNO`, `<STNO>`, `<LABL>` and `<CODE>` the way `run-to-step` does.
  - Because of this shared state, tests cannot run in parallel.

## Tests

`test/SNOBOL4clojure/test_helpers.clj` is the harness. Every test that runs a program must use one of its wrappers, because a SNOBOL4 program can loop forever:

- `prog`: default budget, 2000 ms;
- `prog-timeout <ms>`: a set budget. Every `prog` macro evaluates its lines when the test runs, so a `(str ...)` form or a def'd program text works;
- `prog-steplimit`: a budget plus an in-engine step limit;
- `prog-infinite`: asserts that the program times out;
- `prog-include`: resolves `-INCLUDE`;
- `run-with-timeout`: returns `{:exit :ok|:error|:timeout|:step-limit :stdout :stderr}`.

**How a program runs under these wrappers:**

- **Retry:** a timeout is retried once.
- **Stdin:** programs run in a `future`, so `binding [*in* …]` reaches them. This is how stdin tests feed data, as in `test_csnobol4_suite.clj`.
- **Files:** a relative file name in `INPUT()`/`OUTPUT()` resolves against `env/*file-dir*` when it is bound. When it is `nil`, the name resolves against the process's working directory, since a JVM cannot change its own. `test_csnobol4_suite` binds it to a temp directory, so `lein test` writes nothing into the tree.
- **Debugging:** `run-to-step`, `probe-at`, `bisect-divergence`, `probe-test` and `run-with-restart` stop a program after N statements and snapshot its variables.

Test groups:

- **`test_worm*` and `catalog/`:** hand-encoded semantic facts, one per `deftest`.
- **`test_cooper`:** ported from Cooper's Snobol4.Net tests.
- **`test_csnobol4_suite`:** Budne's CSNOBOL4 suite, with program text inlined. The `:expected-fail` metadata its docstring mentions is not implemented anywhere.
- **`test_syntax`:** the compile error numbers `compiler.clj`'s section syntax names.
- **`test_sprintN`:** feature tests by sprint.

## Commits

Each commit cures one defect and carries the test that proves it.

- **Subject:** the subject line states SPITBOL's rule as a sentence, for example `Backing up into FENCE fails the whole match`.
- **Body:**
  - quotes the manual's rule and the `sbl -bf` probe that measured it;
  - names the board entry it cures;
  - ends with `Test: <deftest>` and the `lein test` count (tests, failures, and "none new").
- **Comments in the code** cite the same manual chapter and `sbl -bf` measurement.
- **Line width:** source lines run up to 200 characters, and a longer comment is broken onto further comment lines. Do not reflow lines that already fit.

## Repo notes

- **`corpus/lon` is a git submodule** (SNOBOL4-plus/SNOBOL4-corpus) and is uninitialised in a fresh clone. `corpus/gimpel`, `corpus/aisnobol`, `corpus/spitbol` and `corpus/beauty` are tracked directly.
- **Leftovers, not live code:**
  - `misc/` holds editor syntax files and scratch.
  - `CHANGELOG.md` and `doc/intro.md` are both still the Leiningen template.
- **The README's numbers are stale.** Its badge and text claim "1,896 tests / 0 failures" and "4,120 assertions", which match no current run. The benchmark tables are likewise claims, not measurements.
