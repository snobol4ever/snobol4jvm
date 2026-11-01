# snobol4jvm

[![License: AGPL v3](https://img.shields.io/badge/License-AGPL_v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![Fleet suites](https://img.shields.io/badge/fleet%20suites-11%20of%2011%20at%20100%25-brightgreen)](#test-suite-coverage)
[![lein test](https://img.shields.io/badge/lein%20test-2%2C295%20tests%20%2F%2074%20failures-orange)](#the-engines-own-unit-tests)

**Full SNOBOL4/SPITBOL implementation for the JVM, written in Clojure.**

SNOBOL4 was invented at Bell Labs in the 1960s by Ralph Griswold, Ivan Polonsky, and David
Farber. It introduced pattern matching as a first-class data type — patterns that compose,
backtrack, capture intermediate results, reference themselves recursively, and can express
BNF grammars directly. snobol4jvm runs the real language on any platform Java runs on,
with a four-stage acceleration pipeline that pushes toward native speed.

---

## What Is SNOBOL4?

Most string processing tools — regex engines, parser generators, PEG libraries — target one
tier of the Chomsky hierarchy and stop there. SNOBOL4 patterns span all four:

| Tier | Example | Tool that stops here |
|------|---------|---------------------|
| Type 3 — Regular | `(a\|b)*abb` | regex, PCRE |
| Type 2 — Context-free | `{a^n b^n}` | yacc, Bison, PEG |
| Type 1 — Context-sensitive | `{a^n b^n c^n}` | — |
| Type 0 — Unrestricted | `{w#w}` copy language | — |

All four tiers are expressible as SNOBOL4 patterns. No separate grammar formalism. No
external lexer. Patterns are first-class values: you build them, store them in variables,
pass them to functions, and compose them at runtime.

A SNOBOL4 program looks like this:

```snobol
*  Count vowels in a string
        SUBJECT = "Hello, World"
        VOWELS  = "AEIOUaeiou"
LOOP    SUBJECT ANY(VOWELS) =          :F(DONE)
        COUNT   = COUNT + 1            :(LOOP)
DONE    OUTPUT  = "Vowels: " COUNT
END
```

Each statement has a subject, an optional pattern, an optional replacement, and an optional
GOTO — conditional on success (`:S`) or failure (`:F`). The pattern `ANY(VOWELS)` matches
any single character in the set and deletes it (replacement is empty). This is not regex
sugar. This is a full backtracking engine.

---

## Architecture: The Four-Stage Pipeline

snobol4jvm runs SNOBOL4 through four increasingly optimized execution engines. Each stage
is a complete, correct implementation. The pipeline is additive — later stages delegate to
earlier ones for correctness and add speed on top.

```
SNOBOL4 source
      │
      ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Stage 1: Interpreter  (runtime.clj)                                │
│  GOTO-driven statement dispatch. Faithful to original semantics.    │
│  All features are proven here first.                                │
└──────────────────────────────┬──────────────────────────────────────┘
                               │  EDN cache (22× cold-start speedup)
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Stage 2: Transpiler  (transpiler.clj)                              │
│  SNOBOL4 IR → Clojure loop/case. The JVM JIT compiles it to        │
│  native code on first call. Hot loops benefit immediately.          │
│  Speedup: 3.5–6× vs interpreter.                                   │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Stage 3: Stack VM  (vm.clj)                                        │
│  Flat integer-indexed bytecode vector. Single integer dispatch.     │
│  Eliminates map lookups, keyword resolution, seq walking.           │
│  Speedup: 2–6× vs interpreter.                                      │
└──────────────────────────────┬──────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│  Stage 4: JVM Bytecode  (jvm_codegen.clj)                           │
│  SNOBOL4 → .class bytecode via ASM. DynamicClassLoader at runtime. │
│  The JVM JIT sees real bytecode and applies full optimization.      │
│  Speedup: 7.6× vs interpreter. Cold-start cumulative: ~190×.       │
└─────────────────────────────────────────────────────────────────────┘
```

### The Pattern Engine: match.clj

The pattern match engine is the core of the system. It is a single file and a single
`loop/case` — a design law that has never been violated. The state is a 7-element frame
vector:

```
[Σ  Δ  σ  δ  Π  φ  Ψ]
 ↑  ↑  ↑  ↑  ↑  ↑  ↑
 │  │  │  │  │  │  └─ parent frame stack
 │  │  │  │  │  └──── child index (for ALT/SEQ iteration)
 │  │  │  │  └─────── current pattern node
 │  │  │  └────────── current position
 │  │  └───────────── current subject chars
 │  └──────────────── position on entry to this frame
 └─────────────────── subject chars on entry to this frame
```

Actions: `:proceed` (enter) · `:recede` (backtrack) · `:succeed` (matched) · `:fail` (give up).

This is the **Byrd Box model** — described by Lawrence Byrd in 1980 for Prolog debugging
and generalized by Todd Proebsting in 1996 as a code generation strategy for goal-directed
languages. The same four-port model describes SNOBOL4 pattern matching, Icon generators,
and Prolog unification. In snobol4jvm the interpreter runs it explicitly; in the JVM
bytecode stage, each Byrd box becomes labeled JVM instructions wired at compile time.

### Stage 2: The Transpiler

The transpiler compiles SNOBOL4 IR to a Clojure namespace containing one `run!` function —
a `loop/case` over statement indices. Each SNOBOL4 statement becomes one case clause:

```clojure
; SNOBOL4:   LOOP  I = I + 1  :S(LOOP)
;
; Transpiled:
(case pc
  ...
  42 (let [r (ops/EVAL! '(= I (+ I 1)))]
       (if r (recur :LOOP) (recur 43)))
  ...)
```

This is a real Clojure function. The JVM JIT compiles it to native code on first invocation.

### Stage 3: The Stack VM

The VM flattens the IR into a dense integer-indexed bytecode vector with seven opcodes:

| Opcode | Meaning |
|--------|---------|
| `HALT`    | Program end |
| `EXEC`    | Eval body, always fall through |
| `EXEC-S`  | Eval; success → jump, failure → fall through |
| `EXEC-F`  | Eval; success → fall through, failure → jump |
| `EXEC-SF` | Eval; success → jump1, failure → jump2 |
| `JUMP`    | Unconditional goto |
| `SIGNAL`  | Throw SNOBOL4 signal |

A single integer dispatch per statement eliminates the map lookups, keyword-to-integer
resolution, and seq-walking overhead that the interpreter pays on every statement.

### Stage 4: JVM Bytecode

`jvm_codegen.clj` compiles SNOBOL4 IR to `.class` bytecode using the ASM library directly.
Each program becomes one Java class with a static `run()` method. `DynamicClassLoader`
loads it at runtime — the JVM JIT then applies full native compilation including inlining,
loop unrolling, and register allocation.

The generated class is intentionally minimal in its dependencies: it references only
`java/lang/Object`, `clojure/lang/IFn`, `clojure/lang/RT`, and the codegen class
itself. This makes class loading fast and reliable across all JVM versions.

---

## Performance

snobol4jvm is much slower than both SPITBOL and CSNOBOL4. This section says by how much, for the engine the command line runs: `java -jar` on the default `interp` engine,
started through the fleet's launcher `run_snobol4jvm.sh` (the AppCDS archive, `-XX:CompileThresholdScaling=4 -XX:-DontCompileHugeMethods`).

**How it is measured**

- The unit is **user instructions retired by the whole process**, JIT and collector threads included (`perf stat -e instructions:u`). G is a billion, M a million. An instruction
  count does not move with the load on the box; a wall clock does, so wall times appear only where stated.
- **SPITBOL** is the fleet's benchmark build of SPITBOL x64 (`/home/resources/spitbol-bench-oracle/sbl`): upstream with the minimal patches that make `-bf` accept SCRIP's
  upper/lower-case convention and the nanosecond clock, and **no monitor hooks**. The correctness oracle (`x64/bin/sbl`) carries the monitor hooks and is never timed: even idle, its
  hooks double the instruction count on the 23 kernels (348 M against 173 M). **CSNOBOL4** is the box's build of Phil Budne's CSNOBOL4. A figure is printed only when the engine's
  output equals the reference; otherwise the cell reads n/a.
- **Start-up is reported on its own, below, and is subtracted from every figure in the two benchmark tables:** each engine's number is its total less its own total on an empty program.
  A "×" is the jar's work divided by the other engine's work; larger is slower.

### Start-up (separate from the tables that follow)

An empty program (`END` alone), five runs, median instructions and the fastest wall clock:

- **SPITBOL:** 0.20 M, 0.00 s
- **CSNOBOL4:** 0.58 M, 0.01 s
- **snobol4jvm:** 6.55 G, 0.82 s on a loaded box (0.58 s on an idle one). A bare JVM printing one line costs 0.06 G and a bare Clojure runtime 4.2 G; the engine's namespaces, the source parser and the
  JIT's work on start-up code are the rest. It was 16.7 G before the grammar was compiled into the namespace and the launcher's JIT options were set (2026-10-31).

Every run of the jar pays this once, whatever the program does.

### General benchmarks: the 23 SnoBench kernels

Work above start-up, median of 3 runs. The kernels (`corpus/benchmarks/snobol4`) are millisecond-sized, so the jar's work is mostly one-time cost (parsing the source with instaparse,
compiling, class loading, the first run) and not loop iterations; the kernels that loop longest read lowest.

| Kernel | jar | SPITBOL | CSNOBOL4 | jar / SPITBOL | jar / CSNOBOL4 |
|---|---:|---:|---:|---:|---:|
| arith_loop | 1.81 G | 0.23 M | 1.33 M | 7,795× | 1,357× |
| arith_loop_twin | 21.42 G | 77.21 M | 365.42 M | 277× | 59× |
| array_sum | 7.42 G | 5.13 M | 32.11 M | 1,446× | 231× |
| eval_fixed | 7.80 G | 1.24 M | 4.14 M | 6,302× | 1,885× |
| fibonacci | 5.88 G | 2.60 M | 11.70 M | 2,261× | 503× |
| fib_recur | 17.35 G | 47.01 M | 208.15 M | 369× | 83× |
| func_call | 2.91 G | 0.42 M | 1.98 M | 6,910× | 1,474× |
| ident_call1 | 2.01 G | 0.29 M | 1.61 M | 6,996× | 1,245× |
| ident_call2 | 2.09 G | 0.28 M | 1.58 M | 7,370× | 1,324× |
| indirect_dispatch | 2.39 G | 0.32 M | 1.81 M | 7,473× | 1,323× |
| mixed_workload | 3.06 G | 0.38 M | 1.49 M | 8,154× | 2,052× |
| name_indirection | 6.31 G | 9.02 M | 50.65 M | 699× | 124× |
| op_dispatch | 2.12 G | 0.47 M | 2.71 M | 4,478× | 784× |
| pattern_bt | 5.00 G | 1.68 M | 7.70 M | 2,977× | 650× |
| roman | 4.29 G | 1.21 M | 8.46 M | 3,546× | 507× |
| string_concat | 2.02 G | 0.24 M | 1.81 M | 8,421× | 1,112× |
| string_concat_twin | 2.83 G | 1.12 M | 7.02 M | 2,532× | 404× |
| string_manip | 1.84 G | 0.52 M | 2.46 M | 3,556× | 748× |
| string_pattern | 3.46 G | 0.75 M | 3.87 M | 4,582× | 893× |
| table_access | 6.79 G | 7.12 M | 94.16 M | 954× | 72× |
| table_variety | 6.27 G | 4.41 M | 22.28 M | 1,420× | 281× |
| test_icon | 2.79 G | 0.25 M | 0.29 M | 11,091× | 9,545× |
| var_access | 3.05 G | 0.55 M | 2.95 M | 5,511× | 1,032× |
| **All 23** | **120.9 G** | **162 M** | **836 M** | **744×** | **145×** |

### Work-horse demos: the 24 demo programs

Each demo runs once on the full input its `.workhorse` sidecar declares (the sample input repeated by its scale, `DEMO-SCALE.tsv`); a demo has no iteration loop, so one run is the
measure. Work above start-up, one run each; the jar's output equals SPITBOL's on all 24. The last column is the wall clock net of each engine's start-up.

| Demo | Input (scale) | jar | SPITBOL | CSNOBOL4 | jar / SPITBOL | jar / CSNOBOL4 | Net wall: jar / SPITBOL / CSNOBOL4 |
|---|---:|---:|---:|---:|---:|---:|---:|
| arithmetic | — | start-up only | <1 M | 0.42 M | — | — | 0.0 s / 0.00 s / 0.00 s |
| beauty | 44 KB (×1) | 149.26 G | 248.8 M | n/a | 600× | — | 16.3 s / 0.05 s / n/a |
| calculator-1-match-fence | 33 KB (×1) | 42.08 G | 122.8 M | n/a | 343× | — | 2.1 s / 0.02 s / n/a |
| calculator-1-match | 33 KB (×1) | 104.35 G | 314.8 M | n/a | 331× | — | 5.1 s / 0.04 s / n/a |
| calculator-1 | 130 KB (×4) | 53.56 G | 138.8 M | n/a | 386× | — | 3.5 s / 0.04 s / n/a |
| calculator-2-match-fence | 130 KB (×4) | 11.86 G | 14.8 M | n/a | 801× | — | 0.4 s / 0.01 s / n/a |
| calculator-2-match | 130 KB (×4) | 12.26 G | 11.8 M | n/a | 1,039× | — | 0.6 s / 0.01 s / n/a |
| calculator-2 | 130 KB (×4) | 464.32 G | 155.8 M | n/a | 2,980× | — | 28.9 s / 0.04 s / n/a |
| claws5-match-fence | 2.1 MB (×32) | 21.95 G | 60.8 M | n/a | 361× | — | 0.9 s / 0.01 s / n/a |
| claws5-match | 2.1 MB (×32) | 25.64 G | 57.8 M | n/a | 443× | — | 1.4 s / 0.01 s / n/a |
| claws5 | 1.1 MB (×16) | 189.88 G | 671.8 M | n/a | 283× | — | 10.9 s / 0.08 s / n/a |
| counter | — | start-up only | <1 M | 0.42 M | — | — | 0.0 s / 0.00 s / 0.00 s |
| hello | — | start-up only | <1 M | 0.42 M | — | — | 0.0 s / 0.00 s / 0.00 s |
| json-match-fence | 1.7 MB (×1) | 24.08 G | 49.8 M | n/a | 484× | — | 1.5 s / 0.02 s / n/a |
| json-match | 1.7 MB (×1) | 24.15 G | 45.8 M | n/a | 527× | — | 1.8 s / 0.01 s / n/a |
| json | 1.7 MB (×1) | 73.76 G | 307.8 M | n/a | 240× | — | 5.8 s / 0.05 s / n/a |
| pattern_test | — | start-up only | <1 M | 0.42 M | — | — | 0.0 s / 0.00 s / 0.00 s |
| porter | 3.0 MB (×16) | 1060.37 G | 5.20 G | 29.54 G | 204× | 36× | 82.3 s / 1.01 s / 5.32 s |
| roman | — | 5.60 G | 1.8 M | 14.4 M | — | 388× | 0.1 s / 0.00 s / 0.00 s |
| treebank-alloc | 335 KB (×1024) | 59.93 G | 3.30 G | n/a | 18× | — | 4.0 s / 0.94 s / n/a |
| treebank-match-fence | 335 KB (×1024) | 12.60 G | 17.8 M | n/a | 708× | — | 0.6 s / 0.01 s / n/a |
| treebank-match | 335 KB (×1024) | 13.54 G | 15.8 M | n/a | 857× | — | 0.6 s / 0.02 s / n/a |
| treebank | 335 KB (×1024) | 4304.55 G | 1.71 G | n/a | 2,515× | — | 405.2 s / 0.21 s / n/a |
| wordcount | 5.8 MB (×65536) | 69.88 G | 589.8 M | 2.24 G | 118× | 31× | 6.7 s / 0.08 s / 0.33 s |

- *start-up only*: the jar's work is under 1 G, inside the spread of its start-up (about 0.3 G); SPITBOL's counts resolve to about 1 M.
- *n/a*: CSNOBOL4's output differs from SPITBOL's, so no figure is given. It agrees on 7 of the 24 demos; I did not look into why it differs on the other 17.
- The worst cases are `treebank` (about 400 s) and `calculator-2`; the closest to SPITBOL is `treebank-alloc`, where SPITBOL itself is slow.

**Reproduce:** `bash harness/scripts/bench_snobol4_jvm_table.sh` (table 2) and `bash harness/scripts/bench_snobol4_jvm_demos.sh` (start-up and table 3) from the fleet's `harness`
repository. The same numbers are kept in the org's `.github/findings/JVM-BENCH-TABLE.md`, with how each speed lever was found in `JVM-SPEED-NOTES.md`. Measured 2026-10-31 20:59 CDT by bench_snobol4_jvm_table.sh -n 3; snobol4jvm 4cd6529, load 7.92 5.08 5.41; corpus 0ffb9bd12.

<details>
<summary>Earlier figures (2026-10-17): in-process, after warm-up, on four tiny programs; not re-run and not about the default engine</summary>

Benchmarks on Linux / OpenJDK 21 / Leiningen 2.12.0. SPITBOL and CSNOBOL4 times include ~15 ms process-spawn overhead. The `Transpil`, `Stack VM` and `JVM code` columns are backends
reachable from the API, not from the command line; `--engine vm` is the only other engine the command line selects, and it counted within 4% of `interp` on the two programs tried.

```
╔══════════════════╦══════════╦══════════╦══════════╦══════════╦══════════╦══════════╗
║ Program          ║ SPITBOL  ║ CSNOBOL4 ║ Interp   ║ Transpil ║ Stack VM ║ JVM code ║
╠══════════════════╬══════════╬══════════╬══════════╬══════════╬══════════╬══════════╣
║ arith-10k        ║  15.83ms ║  27.25ms ║ 112.14ms ║  88.03ms ║ 107.02ms ║  85.55ms ║
║ strcat-500       ║  15.09ms ║  26.16ms ║   9.70ms ║   7.99ms ║  26.02ms ║   6.93ms ║
║ pat-span         ║  15.47ms ║  26.29ms ║  28.05ms ║  25.89ms ║  52.06ms ║  25.22ms ║
║ fact45           ║  16.59ms ║  24.73ms ║    N/A * ║   1.11ms ║ 167.46ms ║   0.89ms ║
╚══════════════════╩══════════╩══════════╩══════════╩══════════╩══════════╩══════════╝
* fact45 overflows the interpreter's step limit at default settings
```

**Notable results claimed then:** `strcat-500` interpreter 9.7 ms net against CSNOBOL4's 11 ms; `fact45` JVM bytecode 0.89 ms, 14-19x faster than SPITBOL; the EDN compilation cache a 22x
cold-start speedup.

| Stage | Speedup vs interpreter | Mechanism |
|-------|:----------------------:|-----------|
| EDN cache | **22x** cold-start | Memoized IR, skip reparse |
| Transpiler | **3.5-6x** | IR to Clojure `loop/case`, JVM JIT |
| Stack VM | **2.5-5.7x** | Dense integer bytecode, single dispatch |
| JVM bytecode | **7.6x** | ASM `.class`, full JIT optimization |
| Cold-start cumulative | **~190x** | EDN cache + JVM bytecode combined |

</details>

---

---

## What's Implemented

**Execution model**
- GOTO-driven statement execution with `:S()` / `:F()` conditional branches
- `DEFINE` / `RETURN` / `FRETURN` / `NRETURN` — user-defined functions with full recursion
- `APPLY` — first-class function calls by name at runtime
- `OPSYN` — operator aliasing
- `CODE()` / `EVAL()` — runtime compilation and evaluation
- `-INCLUDE` preprocessor directive

**Pattern engine — complete SNOBOL4 primitive set**

| Category | Primitives |
|----------|-----------|
| Character class | `ANY` `NOTANY` `SPAN` `BREAK` `BREAKX` |
| Positional | `POS` `RPOS` `TAB` `RTAB` `REM` `LEN` |
| Structural | `ARB` `ARBNO` `BAL` `FENCE` `ABORT` `FAIL` `SUCCEED` |
| Capture | `$` (immediate assign) · `.` (deferred, committed on match success) |
| Deferred ref | `*var` — pattern stored in a variable, evaluated at match time |
| Cursor | `@N` — captures current cursor position |

**Data structures**
- `TABLE` — hash map with arbitrary key/value types
- `ARRAY` — multi-dimensional arrays with `ITEM` accessor
- `DATA` — user-defined record types with `FIELD` accessor

**I/O and introspection**
- Named I/O channels (`INPUT` / `OUTPUT` with channel numbers)
- `TERMINAL` — direct terminal I/O
- `TRACE` / `STOPTR` — execution tracing hooks
- `DUMP` — runtime variable state inspection
- `&STCOUNT` / `&STLIMIT` — statement counter and execution budget

**Snocone frontend**
- Lexer and expression parser for Snocone — Andrew Koenig's structured C-like dialect
  that transpiles to SNOBOL4 (Bell Labs CSTR #124, 1986)

### Known Gaps

| # | Issue |
|---|-------|
| 1 | `CAPTURE-COND` (`.` operator) assigns immediately — deferred commit not yet built |
| 2 | `ANY(multi-arg)` inside an `EVAL` string causes `ClassCastException` |
| 3 | JVM bytecode stage calls back to interpreter for arithmetic — `EVAL!` inline pending |

---

## Testing

### Test-suite coverage

snobol4jvm is graded on the eleven suites of the snobol4ever fleet. Each program is run through the launcher (`run_snobol4jvm.sh`, the way `java -jar` is started) and its output
is compared byte for byte with a reference cut from the oracle: SPITBOL (`sbl -bf`), and CSNOBOL4 for Budne's suite. **Every suite reads 100% of the programs it grades**,
2,650 of 2,650, read on 2026-10-31 on snobol4jvm `4cd6529`.

| Suite | What it is | Passing / graded | Excluded | Wall |
|---|---|---:|---:|---:|
| SnoRungs | The construct ladder: one master of oracle-graded programs, one construct each | 2,192 / 2,192 (100%) | 1 | 30 m 30 s |
| Flake | Ori Livneh's snoflake fixture suite | 126 / 126 (100%) | 54 | 2 m 05 s |
| Gimpel | Gimpel's SNOBOL4 programs (the driver programs of the package) | 144 / 144 (100%) | 1 | 2 m 37 s |
| Budne | Phil Budne's CSNOBOL4 test suite, graded against CSNOBOL4 | 70 / 70 (100%) | 62 | 1 m 08 s |
| X64T | SPITBOL's x64 test programs | 33 / 33 (100%) | 3 | 1 m 42 s |
| X32T | SPITBOL's x32 test programs | 16 / 16 (100%) | 5 | 13 s |
| TPgm | SPITBOL's own test programs 1-4 | 8 / 8 (100%) | — | 8 s |
| AIS | The AIS SNOBOL4 package programs | 8 / 8 (100%) | 1 | 19 s |
| Dotnet | Programs of the snobol4dotnet package | 6 / 6 (100%) | 8 | 8 s |
| SnoBench | The 23 benchmark kernels below, output checked against the ref under three angles | 23 / 23 (100%) | — | 1 m 54 s |
| SnoDemo | The 24 demo programs below, on their sample input, output checked against the ref | 24 / 24 (100%) | — | 1 m 00 s |
| **All eleven** | | **2,650 / 2,650 (100%)** | **135** | |

*Excluded* programs are kept out of the denominator and are listed with their reasons in each package's `EXCLUDED.tsv` in the corpus (for example: the oracle produced no output, the source
cannot be carried by the harness, or a ruling excludes it; SnoRungs' one, `simple_program_11`, prints nothing under SPITBOL, so no output could witness it). *Wall* is the sum of each
program's fastest run. The fleet's harness and corpus are in the `harness` and `corpus` repositories of snobol4ever; the suite rows are in its `SUITES.tsv`.

### The engine's own unit tests

```bash
lein test
```

**2,295 tests, 4,972 assertions, 74 failures, 0 errors** (snobol4jvm `4cd6529`). The 74 are known and are the same 74 on every commit from `10ff562` to `4cd6529`; `lein test`
names each one. The suite is not clean, and the fleet table above is the measure of conformance, not this count. The unit tests cover:

- **Worm corpus** — automatically generated SNOBOL4 programs cross-validated against the interpreter oracle. Every program runs through both engines; output must match exactly.
- **Micro-worm** — short programs targeting specific language constructs
- **Bootstrap** — programs that compile and run SNOBOL4 at runtime via `CODE()` / `EVAL()`
- **Cooper suite** — cross-validation against snobol4dotnet semantics
- **Snocone** — parser and emitter tests for the structured frontend
- **Regression tests** for each defect cured, one `deftest` per commit

---

---

## beauty.sno — The Bootstrap Milestone

`beauty.sno` is an 801-line SNOBOL4 program written in SNOBOL4 that reads SNOBOL4 source
and outputs it canonically formatted. It exercises nearly every feature of the language
simultaneously: recursive pattern matching, user-defined data types, TABLE lookups,
DEFINE/RETURN, multiple I/O channels, -INCLUDE, and conditional execution at every level.

**M-JVM-BEAUTY ✅ `b67d0b1` J-212** — snobol4jvm self-beautifies `beauty.sno`, producing
output byte-for-byte identical to the CSNOBOL4 2.3.3 oracle. This is the gold standard
for SNOBOL4 implementation correctness.

The last bug before this milestone fired: a cross-scope GOTO routing error in the JVM
bytecode emitter — targets outside the current function body were emitting undefined label
references. One fix in `jvm_emit_goto()`: route out-of-scope targets to `Jfn%d_freturn`
instead. Zero Jasmin errors. Oracle match at runtime.

---

## The Five-Way Monitor

snobol4jvm participates in the five-way sync-step monitor — a parallel harness that runs
the same SNOBOL4 program through five implementations simultaneously and compares TRACE
streams event by event:

| Participant | Role |
|-------------|------|
| CSNOBOL4 2.3.3 | Primary oracle |
| SPITBOL x64 4.0f | Secondary oracle |
| snobol4x ASM backend | Native compiled target |
| **snobol4jvm** | **JVM compiled target** |
| snobol4dotnet | .NET compiled target |

Each participant writes trace events to its own named FIFO via `monitor_ipc.so` — a LOAD'd
C shared library that bypasses stdio entirely. The collector diffs all five streams in
parallel. The first diverging line identifies the exact bug in exactly one participant.
The agreeing participants become the living specification for the fix.

Infrastructure: M-MONITOR-IPC-CSN ✅ `6eebdc3`. Full five-way launch: M-MONITOR-IPC-5WAY.

---

## Build and Run

**Prerequisites:** Java 11+, [Leiningen](https://leiningen.github.io/)

```bash
git clone https://github.com/snobol4ever/snobol4jvm
cd snobol4jvm
```

**Run a SNOBOL4 program:**

```bash
lein run program.sno < input.txt     # INPUT reads stdin; --engine vm picks the Stage 3 VM
```

**Build a standalone jar:**

```bash
lein uberjar
java -jar target/uberjar/SNOBOL4clojure-0.2.0-standalone.jar program.sno < input.txt
```

**Run the test suite:**

```bash
lein test
```

### Project Structure

```
src/SNOBOL4clojure/
  match.clj           Pattern engine — one file, one loop/case. The core.
  runtime.clj         GOTO-driven statement interpreter (Stage 1)
  transpiler.clj      SNOBOL4 IR → Clojure loop/case (Stage 2)
  vm.clj              Flat bytecode stack machine (Stage 3)
  jvm_codegen.clj     ASM .class bytecode generation (Stage 4)
  grammar.clj         instaparse PEG grammar for SNOBOL4 source
  emitter.clj         Parse tree → Clojure IR
  operators.clj       EVAL / EVAL! / INVOKE — the central dispatch
  primitives.clj      Low-level pattern scanners (LIT$, ANY$, SPAN$, ...)
  patterns.clj        High-level pattern combinators (ARB, ARBNO, FENCE, BAL, ...)
  functions.clj       Built-in functions (SIZE, DUPL, REPLACE, SUBSTR, ...)
  env.clj             Variable tables, keywords, signal protocol
  trace.clj           TRACE / STOPTR execution hooks
  generator.clj       Test program generator (rand-* probabilistic, gen-* exhaustive)
  snocone.clj         Snocone frontend lexer and parser
  snocone_emitter.clj Snocone → SNOBOL4 IR
```

---

## Design Laws

Ten invariants that have governed the implementation from the first commit:

1. **ALL UPPERCASE keywords.** `ANY`, `SPAN`, `ARBNO` — not `any`, `span`, `arbno`.
2. **Single-file engine.** `match.clj` is one `loop/case`. It cannot be split.
3. **Immutable-by-default, mutable-by-atom.** `TABLE` and `ARRAY` use `atom`.
4. **Label/body whitespace contract.** Labels flush-left, bodies indented.
5. **INVOKE is the single dispatch point.** Both lowercase and uppercase entries.
6. **`nil` means failure; epsilon means empty string.** No ambiguity between them.
7. **`clojure.core/=` inside `operators.clj`.** Bare `=` builds IR lists.
8. **INVOKE args are pre-evaluated.** Never call `EVAL!` on args inside INVOKE.
9. **Two-tier generator discipline.** `rand-*` probabilistic. `gen-*` exhaustive lazy.
10. **Two-strategy debugging:** (a) run a probe; (b) read CSNOBOL4/SPITBOL source. Never speculate.

---

## Relationship to snobol4ever

snobol4jvm is part of the [snobol4ever](https://github.com/snobol4ever) organization — a
project to bring full SNOBOL4/SPITBOL to every modern runtime.

| Repo | Language | Target | Status |
|------|----------|--------|--------|
| [snobol4jvm](https://github.com/snobol4ever/snobol4jvm) | Clojure | JVM | **11 of 11 fleet suites at 100%; `lein test` 2,295 tests, 74 known failures** |
| [snobol4dotnet](https://github.com/snobol4ever/snobol4dotnet) | C# | .NET / Windows / Linux / macOS | 1,911 tests / 0 failures |
| [snobol4x](https://github.com/snobol4ever/snobol4x) | C | x86-64, JVM, .NET MSIL | Native compiler, all three backends |

snobol4jvm and snobol4dotnet are independent implementations validated against each other
and against CSNOBOL4 2.3.3 and SPITBOL x64 as reference oracles. Agreement between two
independently built implementations on any program is strong evidence of correctness.

snobol4x is the native compiler. Its JVM backend generates Jasmin bytecode from the same
SNOBOL4 source — a structurally different third path to the JVM, useful for cross-checking
both implementations.

---

## Acknowledgments

- **Philip L. Budne** — CSNOBOL4 2.3.3. Primary reference oracle, actively maintained,
  and the center of gravity of the SNOBOL4 community on groups.io.
- **Ralph Griswold, Ivan Polonsky, David Farber** — SNOBOL4, Bell Labs, 1962–1967.
- **Mark Emmer** — MACRO SPITBOL (Catspaw, 1987–2009). The definitive commercial implementation.
- **Cheyenne Wills** — SPITBOL x64. Active native compiler, our secondary oracle.
- **Lawrence Byrd** — the four-port box model (1980).
- **Todd Proebsting** — the Byrd Box as a *code generation strategy* (1996), and JCON:
  the Icon → JVM bytecode compiler that is the direct architectural blueprint for
  snobol4jvm's Stage 4. Source: https://github.com/proebsting/jcon
- **Andrew Koenig** — Snocone (Bell Labs CSTR #124, 1986).
- **Gregg Townsend** — JCON co-author; proof that this works on the JVM.

---

## License

AGPL-3.0-or-later. See [LICENSE](LICENSE).

---

## Source Volume (G-VOLUME · M-VOL-JVM ✅ · 2026-03-22)

> `wc -l` scan of `src/`. Categories are logical function — comparable across snobol4x, snobol4jvm, snobol4dotnet.

| Category | Files | Lines | Blank-stripped | % total |
|----------|------:|------:|:--------------:|--------:|
| Parser / lexer | 5 | 926 | 847 | 12.8% |
| Code emitter | 3 | 1,402 | 1,236 | 19.4% |
| Pattern engine | 5 | 921 | 833 | 12.8% |
| Runtime / builtins | 7 | 2,140 | 1,999 | 29.6% |
| Driver / CLI | 2 | 234 | 221 | 3.2% |
| Extensions / plugins | 2 | 29 | 29 | 0.4% |
| Tests | 55 | 19,352 | 17,159 | — |
| Benchmarks | 1 | 152 | 133 | — |
| Docs / Markdown | 2 | 456 | 353 | — |
| **Total (src)** | **28** | **7,220** | **6,562** | **100%** |

