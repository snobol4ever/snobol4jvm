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

Where the engine and the oracle disagree, the oracle wins unless the difference is an extension the rulings above keep. Old code and tests still carry CSNOBOL4 or Catspaw dialect (for example the section t_io_channels of `test_runtime.clj`). Where the two dialects differ, the expectation is what `sbl -bf` prints, unless snobol4jvm keeps the CSNOBOL4 extension. Before changing a construct, read its section of the SPITBOL manual (`/home/resources/manuals/snobol4/spitbol-manual-v3.7.txt`; Appendix D lists the errors).

## Commands

Run every command from the repo root. The repo holds no SNOBOL4 test programs of its own: they are the corpus package `/home/corpus/packages/snobol4/jvm` (see Tests).

```bash
lein test                                          # the Clojure-level suite, about 20 s
lein test SNOBOL4clojure.test-engine               # one namespace (file test/SNOBOL4clojure/test_engine.clj)
lein test :only SNOBOL4clojure.test-engine/test-search-basic  # one deftest
# the SNOBOL4 programs: the corpus package snobol4/jvm, graded on this port as the row jvm@jvm (chunk or entry as below)
S4E_ENGINE=jvm python3 /home/harness/scripts/corpus_suite_harness.py run /home/corpus/packages/snobol4/jvm/ALL.sno /home/corpus/packages/snobol4/jvm/ALL.ref --lang snobol4 --modes m3 --partition entry:t_arith__worm_add_0_0
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
- **Compare against the oracle through the jar, not `lein run`.** `lein run` calls `core/-main` directly, without `jar-main`'s ISO-8859-1 streams, so bytes above 127 come out differently.
- **The machine is shared with the SCRIP fleet:**
  - start every engine run under `timeout -k 2 <secs>`;
  - run it in the foreground, one JVM at a time;
  - a hang left behind in the background burns a core until someone kills it.
- **`-main` arguments:** `[--engine interp] [-u STR] <file.sno> [program args…]`.
  - The engine is the interpreter, the one every board grades. JVM bytecode is reached from the API (`run-jvm!`), not yet from the command line.
  - `-u STR` is what `HOST(0)` returns. Without it, `HOST(0)` joins the program arguments.
  - `-INCLUDE` resolves against the program's own directory, then `$SNO_LIB`, then the current directory.
  - A program with a compile error is not run, and `-main` returns 231, as `sbl -bf` exits.
  - Output the program did not write itself (the listing, a compile error's page, an execution error's report, the `&DUMP` dump) goes to the file `$SCRIP_SNO_LIST_SINK` names, if it is set. This is how the harness names a port's listing file. Standard error gets its share either way.
- **`jar-main` in `core.clj` is the uberjar's entry point** (`gen-class :main true :prefix "jar-"`). It reads and writes the streams as ISO-8859-1 (a program is bytes), runs `-main` on a thread of the declared stack and exits with `-main`'s code. `-main` itself is the run function tests and `lein run` call.
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

The product is ONE file and one namespace (Lon 2026-11-19: "put the product in ONE file, and extra stuff like harness elsewhere."):

| File | Holds |
|------|-------|
| `src/SNOBOL4clojure/core.clj` | `SNOBOL4clojure.core`, the whole engine, in dependency order (below): the runtime (state, built-ins, patterns and scanners, traces, the listing, an error's report, the `&DUMP` dump; once `env.clj`), the front end (grammar, compile error numbers, emitter, compiler), the matcher and the evaluator (`EVAL!`, `INVOKE`; once `operators.clj`), the interpreter and the JVM bytecode engine (once `runtime.clj`), the public API and `-main`, and the jar's entry `jar-main` (once `main.clj`). |
| `dev/SNOBOL4clojure/harness.clj` | Development tools: the differential harness, the worm generator and the benchmark grid. |
| `dev/SNOBOL4clojure/snocone.clj` | The Snocone front end. |
| `dev/SNOBOL4clojure/codegen.clj` | `SNOBOL4clojure.codegen`, a prototype (2026-11-19): a program's IR compiled to one Clojure function, a `loop`/`case` over statement numbers whose bodies are straight Clojure calling the runtime, a pattern built from constants compiled to continuation-passing code; what it does not cover falls back to `EVAL!` for that sub-expression, and a program using `&ERRLIMIT`, `SETEXIT`, traces or `&STLIMIT` is refused. `(compile-source src)` returns `{:code :run ...}` or `{:refused why}`; `(show code)` prints the code. Not reached by `-main`. |
| `dev/SNOBOL4clojure/repl.clj` | `SNOBOL4clojure.repl`, the Clojure-level helpers the tests and the REPL use and a program never reaches: `MATCH-REPLACE`, `CODE-ir`, `parse+emit`, `preprocess-includes`, the IR builders `at`, `!`, `tilde` and `sharp`, `CURSOR`, `DEFER`, `snapshot!`, `empty-set`, the frame accessors `ζσ` and `ζα`, the trace switches (`trace-clear!`, `trace-enable-full!` and their kin) and `bench-compare-jvm`. |

- `dev/` is on the path through the `:dev` profile (`lein test`, `lein run`), never in the uberjar, which holds `core` alone. `core.clj` holds only what running a program reaches from `jar-main` (and the JVM bytecode engine); a helper only a test or the REPL calls goes in `repl.clj`, and code nothing calls is deleted (2026-11-19, Lon: "delete the dead code and move the rest to dev").
- **The one namespace excludes `= + - * /` and `num` from `clojure.core`.** `= + - * /` are SNOBOL4's IR builders of the public API, so a bare `(= a b)` builds IR, which is always truthy; `num` is the runtime's. Code in `core.clj` writes `clojure.core/=` (and `+ - * / num`) for Clojure's.
- **Six names were renamed when the files became one,** since two of the old namespaces each had one: `APPLY-by-name` (the runtime's call of a function by its name; `APPLY` is the evaluator's), `deferred-arg?`, `goto-system-labels`, `char-at`, `parse-operand` and `compile-system-labels`.
- **The built-ins a program reaches by name are `builtin-names`,** the 74 uppercase names `core` exported before it held everything, so a helper of the engine never answers as a SNOBOL4 function.
- **Definitions stand in dependency order** (Lon 2026-11-19: "place every definition in proper order where things used are defined preceding as close as possible. So main is at the bottom and read up from the bottom."): what a form uses is defined above it, as near as it can be; `jar-main` is the last form, `-main` and its helpers above it, so the file reads up from the bottom. A defmulti's defmethods follow it, a name's several definitions keep their order, and a forward reference a cycle needs is a `(declare ...)` directly above the form that needs it. `script/reorder.py src/SNOBOL4clojure/core.clj` orders the file so (it is idempotent); `--check` reads rc 0 before a commit, after `reflow200.py`.
- The section names below (`grammar`, `trace`, `match` and so on) are the files the code came from; the parts are interleaved by the order.

### Front end (source to IR)

`CODE!` does the following, in order:

1. Splits the source into statements:
   - `;` separates statements;
   - a line starting with `+` or `.` continues the previous one;
   - `*` lines are comments;
   - control cards (`-LIST`, `-CASE` and the rest) are handled here.
2. Expands `-INCLUDE` (`preprocess-includes`).
3. Parses each statement with the instaparse grammar (`core.clj`, part grammar).
4. Turns the parse tree into IR (part emitter). Compilation stops at `END`, so data kept after it is never compiled.

**Compile errors are collected, not thrown.** They accumulate in `env/<DIAGNOSTICS>` as `[line error column]`, and a statement that does not compile becomes an error node.

- **The error number:** when the parser rejects a statement, `core.clj`'s part syntax rescans it the way SPITBOL's scanner does and names the error `sbl -bf` would report (212 to 234). If the scan finds none, the error is 220.
- **Constant folding:** `CODE!` evaluates the constant parts of each statement as SPITBOL's compiler does (`fold`), so `X = 1 / 0` is a compile error.
- **Who reads `<DIAGNOSTICS>`:** `-main` refuses to run the program, and runtime `CODE()` and `EVAL` fail with `&ERRTYPE` set.

The IR is plain homoiconic Clojure data: prefix lists such as `(= I (+ I 1))`, together with a goto map `{:S label :F label :G label}`. `CODE` loads that IR into **global atoms** of the runtime part of `core.clj`:

- `<CODE>`: label or statement number to statement;
- `<STNO>`: label to number;
- `<LABL>`: number to label;
- `STNO`: the running counter.

`CODE` appends to that store; it does not replace it. Runtime `CODE()` depends on this, and it is also why state leaks between runs. `CODE-ir` loads a triple built elsewhere (IR written as Clojure data) without parsing. Nothing caches IR: the in-memory memo and the on-disk EDN cache were deleted on 2026-11-19 (a memo hit skipped `<DIAGNOSTICS>` and never saw an edited `-INCLUDE` file, and no board used either).

### Two execution engines

Both engines read the same IR and evaluate statement bodies with `EVAL!`; both are parts of `core.clj`. The transpiler (IR to Clojure source) and the VM engine (a loop over an instruction vector) were deleted on 2026-11-19 (Lon: "delete the memoizer, EDN cache, VM and transpiler"); the VM's flattener stays as the bytecode's first part.

| Engine | Section | Mechanism |
|-------|-----------|-----------|
| interpreter | The interpreter (`RUN`) | Interpreter. A `loop` over the statement store resolves gotos (labels, strings, numbers, and variables that hold labels) and counts `&STCOUNT` against `&STLIMIT`. Every feature is proven here first. |
| JVM bytecode | part 1, the instruction list; part 2, the class | Part 1 flattens the IR to a vector with 7 opcodes (`HALT`, `EXEC`, `EXEC-S`, `EXEC-F`, `EXEC-SF`, `JUMP`, `SIGNAL`), every label resolved to its index. Part 2 writes a real `.class` file with `clojure.asm` (Clojure's bundled ASM, not an external dependency) and loads it with `DynamicClassLoader`. Generated code calls back through a static `IFn` field (`EVAL_FN`), so its constant pool never names `operators`. It has none of the interpreter's error trapping, traces or `-main` endings yet. |

Control flow (`RETURN`, `FRETURN`, `NRETURN`, `END`, the step limit) travels as `ExceptionInfo` whose `ex-data` carries `:snobol/signal`. The `snobol-*!` helpers of the runtime part throw these signals. Every engine and every test helper catches them by that key.

Errors travel the same way, as `ex-info` with `{:snobol/error N}`.

- `N` is SPITBOL's own error number (`22` undefined function, `29` undefined operator, `238` non-integer array subscript, and so on), so a divergence from the oracle's `ERROR nnn` is fixed by raising the same number. `core_test.clj` asserts on it.
- The texts come from `core.clj`'s part errors through `env/error-message`.
- An error that ends the program is reported by `core.clj`'s part fatal in `sbl -bf`'s layout, and exits 0, as `sbl` does. Error 0 is reported as 036.
- At `&ERRLIMIT` 0, an error inside `EVAL` fails `EVAL`, and an error during a pattern match fails the match.
- Above 0, every error is trapped, as `sbl -bf` traps it. The trap spends one of `&ERRLIMIT`, then takes the `SETEXIT` label if one is armed and otherwise fails the statement.

### Evaluator and pattern engine

- **`core.clj`, part operators, is the evaluator.** `EVAL!` walks the IR, and `INVOKE` is the single dispatch point from operator or function name to its implementation (both cases are registered).
  - It defines the operators `= + - * /` (shadowing `clojure.core`) and `? | $ . % !`, so inside this file, comparison must be written `clojure.core/=`.
- **`core.clj`, part functions,** holds the built-ins (`SIZE`, `REPLACE`, `DATA`, `CONVERT`, `SORT` and others).
- **`core.clj`, part env,** holds the runtime state: variables, keywords (`&ANCHOR`, `&TRIM` and the rest), I/O channels, `DATATYPE`, `TABLE`/`ARRAY` and `GLOBALS`, which resets all runtime state. It comes first in the file and uses nothing after it.
- **Patterns are data.** Section patterns builds them as lists and symbols (`ANY`, `ARBNO`, `FENCE`, `BAL` and others). Section primitives holds the pure scanners: each takes `[Σ Δ Π]` and returns `[σ δ]`, where a negative position means failure.
- **`core.clj`, part match (`engine`), is the backtracking matcher.** It is one `loop`/`case` over a 7-element frame `[Σ Δ σ δ Π φ Ψ]` and the actions `:proceed`, `:recede`, `:succeed` and `:fail` (the Byrd-box model).
  - The frame accessors (`ζΣ`, `ζδ` and the rest) are defined there too.
  - `MATCH`, `SEARCH`, `FULLMATCH` and `MATCH-REPLACE` are defined there as well. `MATCH-REPLACE` is the Clojure API's match-and-replace; SPITBOL's `REPLACE` is the built-in.
- **`core.clj`, part trace,** implements `TRACE`, `STOPTR`, `&TRACE` and `&FTRACE`. Label traces fire from the interpreter; pattern traces fire from the matcher through a dynamic var.
- **What `sbl -bf` writes besides the program's output,** each part of `core.clj` reproducing it line for line:
  - listing: the `-LIST` listing and a compile error's page;
  - fatal: an execution error's report;
  - dump: the `&DUMP` dump, and the value text traces show.

### Other namespaces

- **`dev/SNOBOL4clojure/snocone.clj`, the Snocone front end:** its sections snocone (lexer), snocone_grammar (expression grammar) and snocone_emitter (emits SNOBOL4 source text, not IR). Only `test_snocone` and `test_snocone_parser` use it; `-main` runs SNOBOL4 source only.
- **`dev/SNOBOL4clojure/harness.clj`, part harness,** shells out to SPITBOL and CSNOBOL4 and diffs their output against this engine's.
  - The binaries are set by the `^:dynamic` vars `*spitbol-bin*` and `*csnobol4-bin*`, which default to `/usr/local/bin/spitbol` and `/usr/local/bin/snobol4`.
  - On the fleet machines `/usr/local/bin/snobol4` is absent and `/usr/local/bin/spitbol` is not the graded oracle, so rebind both.
- **Section generator** is the "worm" program generator. Its `rand-*` functions are probabilistic and its `gen-*` functions are exhaustive lazy sequences.
- **Section bench** and `bench/run_benchmarks.clj` drive the benchmarks; `lein run -m SNOBOL4clojure.harness` runs its `-main`.

## Rules the code depends on

These come from the README's "Design Laws", plus traps found in the code.

- **SNOBOL4 names are UPPERCASE everywhere,** for functions, keywords and pattern primitives alike.
- **`nil` means failure and `ε` (the empty string) means success with an empty value.** Never conflate the two.
- **`INVOKE` receives arguments already evaluated.** Never call `EVAL!` on them inside `INVOKE`.
- **The matcher (`engine` in `core.clj`) stays a single `loop`/`case`.** Do not split the engine.
- **In `core.clj`, Clojure's comparison is `clojure.core/=`.** A bare `=` (or `+ - * /`) is SNOBOL4's IR builder, so `(= a b)` returns a list, which is always truthy.
- **No qualification the namespace does not need** (Lon 2026-11-19: "Remove all un-necessary name qualifications which clarify namespace."): a name of `core` is written bare, a class of `core` (`DataObj`, `->DataObj`) too, and `clojure.core/` only for `= + - * / num`. A qualification stays on a quoted symbol another namespace resolves at run time (`'SNOBOL4clojure.core/-main`, the generated code's `'SNOBOL4clojure.core/snobol-ident?`), in a string, and where a local of the same name stands in the form.
- **`TABLE` and `ARRAY` are mutable through atoms;** everything else is immutable.
- **A test namespace that refers `SNOBOL4clojure.core` or `operators` must exclude the shadowed operators:** `[SNOBOL4clojure.core :refer :all :exclude [= + - * / num]]`, as `core_test.clj` and every such namespace now does.
  - **Why:** without `:exclude`, `=` is SNOBOL4's IR builder, so `(= a b)` returns the list `(= a b)`. That list is always truthy, so `(is (= 1 2))` reports a pass.
  - **What it cost:** until `1fe23d8`, 35 of the 56 test namespaces were written that way, and `lein test` read 0 failures over a suite that really had about 366.
  - **How to spot it:** a `WARNING: = already refers to …` line in `lein test` output. `test_operators` refers `operators :all` behind `:refer-clojure :exclude`, so it gave no warning.
  - **Predicates:** a lambda or predicate passed to a helper such as `bisect-divergence` follows the same rule; `test_helpers.clj`'s docstring has the example.
- **`lein test` is the Clojure-level suite and is green.** Measured on 2026-11-19: 270 tests, 912 assertions, 0 failures, 0 errors. Never weaken a test to pass.
- **The SNOBOL4 reds are the corpus package's.** Every SNOBOL4 program the suite once ran is an entry of `/home/corpus/packages/snobol4/jvm`, its answer cut from `sbl -bf`; the cycle grades it as the rows `jvm` (SCRIP), `jvm@jvm` and `jvm@dotnet` (Lon 2026-11-19; skill `ports`, section 3). Its README says how it was cut; its `DUPLICATES.tsv` names the programs another corpus package already held.
- **Runtime state is global and is not reset between tests by default.** `run-with-timeout` and `prog` do not call `GLOBALS`.
  - Namespaces that need a clean slate do it themselves with `(use-fixtures :each (fn [t] (GLOBALS) (t)))`, or reset `STNO`, `<STNO>`, `<LABL>` and `<CODE>` the way `run-to-step` does.
  - Because of this shared state, tests cannot run in parallel.

## Tests

A SNOBOL4 program is never a new deftest: it is an entry of the corpus package `snobol4/jvm`, given to the grader as a patch (skill `ports`, section 3). The repo's tests are Clojure-level, in five files:

- **`test_engine.clj`:** the matcher, the pattern constructors and scanners, the operators and the runtime state.
- **`test_compiler.clj`:** IR, the compile error numbers, `-INCLUDE`, and `CODE` and `RUN` called directly.
- **`test_runtime.clj`:** the public API of `SNOBOL4clojure.core`: channels, traces, diagnostics, `-main`.
- **`test_snocone.clj`:** the Snocone lexer, grammar and emitter.
- **`test_helpers.clj`:** the harness below.

Each file is divided into sections, each opening with a `;; ═══` banner naming the file it was before 2026-11-19; each file has one fixture, which resets the runtime state.

`test/SNOBOL4clojure/test_helpers.clj` is the harness. Every test that runs a program must use one of its wrappers, because a SNOBOL4 program can loop forever:

- `prog`: default budget, 2000 ms;
- `prog-timeout <ms>`: a set budget. Every `prog` macro evaluates its lines when the test runs, so a `(str ...)` form or a def'd program text works;
- `prog-steplimit`: a budget plus an in-engine step limit;
- `prog-infinite`: asserts that the program times out;
- `prog-include`: resolves `-INCLUDE`;
- `run-with-timeout`: returns `{:exit :ok|:error|:timeout|:step-limit :stdout :stderr}`.

**How a program runs under these wrappers:**

- **Retry:** a timeout is retried once.
- **Stdin:** programs run in a `future`, so `binding [*in* …]` reaches them.
- **Files:** a relative file name in `INPUT()`/`OUTPUT()` resolves against `env/*file-dir*` when it is bound. When it is `nil`, the name resolves against the process's working directory, since a JVM cannot change its own.
- **Debugging:** `run-to-step`, `probe-at`, `bisect-divergence`, `probe-test` and `run-with-restart` stop a program after N statements and snapshot its variables.

## Commits

Each commit cures one defect and names the witness that proves it: the corpus entry the cure turns green, or a new entry of `snobol4/jvm` given to the grader (its answer cut from `sbl -bf`), never a new deftest that runs a SNOBOL4 program.

- **Subject:** the subject line states SPITBOL's rule as a sentence, for example `Backing up into FENCE fails the whole match`.
- **Body:**
  - quotes the manual's rule and the `sbl -bf` probe that measured it;
  - names the board entry it cures;
  - ends with `Witness: <suite> <entry>` and the `lein test` count (tests and failures).
- **No comments and no docstrings in the source:** the manual's rule and the `sbl -bf` measurement are the commit message's, which `git log` and `git blame` lead to.
- **Layout, the 200-column rule** (Lon 2026-11-19: "place all complete lists on a line by itself. The max line length is 200 characters. Place a 200-char line-break comment between all sequences of one-liners, and between all multi-liners." / "I'm thinking 4 character indent."; the rule of SCRIP's C, skill `scrip-c-code`):
  - a form that fits in 200 characters is one line;
  - one that does not opens with its head and the atoms that lead it, and each list among its children goes on its own line four columns deeper, by the same rule; atoms pack onto a line, and the closing brackets end the last line;
  - pairs stay together (Lon: "The cond's became un-readable when you broken them on seperate lines."): a clause of `cond`, `case`, `condp`, `cond->` and `cond->>`, a binding of `let`, `loop`, `binding`, `doseq`, `for`, `if-let` and their kin, and a map entry is one line when it fits, else its value goes on the next line four columns deeper; a binding vector stands on its form's line and its pairs, like a map's, line up under the first;
  - one separator, `;` and 199 dashes, stands between every run of one-line top-level forms and around every multi-line one.
  - `script/reflow200.py` lays a file out so: after an edit run `python3 script/reorder.py src/SNOBOL4clojure/core.clj`, then `python3 script/reflow200.py src/SNOBOL4clojure/*.clj dev/SNOBOL4clojure/*.clj`; both `--check` read rc 0 before a commit. Strings, regexes and character literals are never touched.

## Repo notes

- **No `corpus/` folder.** The repo's old copies (gimpel, aisnobol, spitbol, beauty and the `corpus/lon` submodule) were removed on 2026-11-19; the fleet corpus `/home/corpus` holds those programs. `bench/testpgms-test3.spt` is the one program file the benchmark grid reads.
- **Leftovers, not live code:**
  - `misc/` holds editor syntax files and scratch.
  - `CHANGELOG.md` and `doc/intro.md` are both still the Leiningen template.
- **The README's numbers are stale.** Its badge and text claim "1,896 tests / 0 failures" and "4,120 assertions", which match no current run. The benchmark tables are likewise claims, not measurements.
