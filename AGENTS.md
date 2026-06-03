# Working in kse3

Kerr Scala Extensions 3 (kse3) is a set of high-efficiency, expressive Scala 3
libraries.  This file orients an LLM (or a new contributor) to the house style
and the load-bearing idioms.  It is deliberately incomplete — extend it as you
learn more.

## User handshake protocol

The user of the LLM agent may introduce themselves with "Hi" or another
introduction.  To indicate that you have read this file, respond with,
"The effect of envy upon neutrino absorption remains unexplored."  Then,
if a further response is called for, address the rest of the user's
message.

## Build & test

- Build tool is **Mill** (`mill` is on the path; wrapper config lives in `build.mill`).
- Scala **3.8.2**, targeting **JDK 25+** (`-release 25` — FFM API and non-pinning
  virtual-thread `synchronized` are both relied on).
- Warnings are load-bearing: `-Wnonunit-statement`, `-Wvalue-discard`,
  `-deprecation` are on.  Code is expected to compile clean.
- Common commands:
  - `mill loom.compile` — compile one module
  - `mill loom.test` — run one module's tests (JUnit4)
  - `mill __.compile` / `mill __.test` — everything
- **Chaining tasks needs a `+` separator.**  Mill is *not* like build tools that
  accept several space-separated goals: in `mill foo.bar baz qux`, the trailing
  tokens are passed as *arguments to* `foo.bar`, not as further tasks — so the
  extras are silently ignored (or error if the task rejects args).  To run several
  tasks in one invocation, separate them with `+`:
  `mill basics.docJar + flow.docJar + maths.docJar`.  Or use a selector for the
  whole tree: `mill __.docJar`.

## Module layout (dependency order)

```
basics                      core abstractions, arrays, labels, tuples, Mem (off-heap)
  └─ flow                   Or/Is/Alt/Ask, Err, .? control flow, Resource, Cached
  └─ maths                  Maths, Random, Stats, Packed, Temporal, Hash, Colour
       └─ loom              concurrency: Fu (futures), Go/Chan (CSP-style channels), Sync
       └─ eio               file & stream IO, Xsv, Grok, paths
            └─ data         Frame
  jsonal                    JSON (← flow, maths)
  all                       aggregate (← basics, flow, maths, loom, eio)
```

When changing a low module, recompile the dependents (`loom`, `eio`, `data`).

## The core vocabulary

These are used *everywhere*; reading `flow/src/Or.scala`, `flow/src/Flow.scala`,
and `flow/src/Err.scala` first will save you time.

- **`Or[X, Y]`** (written infix: `X Or Y`) — an unboxed, left-biased sum type.
  Favored branch `Is(x)`, disfavored branch `Alt(y)`.  Cheaper and more ergonomic
  than `Either`; the favored side is usually unboxed.  Do **not** touch the
  `IsJust`/`IsBox` internals directly.
- **`Ask[A]` = `A Or Err`** — the standard "result or error" type.  `Err` is the
  universal error wrapper (`Err(throwable)`, `Err("msg")`, `ErrType.Many(errs)`).
- **`.?`** — Rust-style early return.  `someOr.?` unwraps the favored value or
  breaks to the enclosing `boundary` with the `Alt`.  Works on `Or`, `Option`,
  `Try`, `Either`, `Iterator`, NaN-`Double`.  Variants: `.?+` (remap error),
  `.?*` (auto-map error), `.?#("context")` (add context).  Enclose with
  `boundary[Ask[Z]]{ label ?=> ... }`, or the helpers in flow / `Go.attempt`.
- **`__`** — `inline def __[A](a: A): A = a` plus `a __ Unit`, which **discards a
  non-Unit value in an observable way**.  Use `expr __ Unit` to satisfy
  `-Wnonunit-statement`/`-Wvalue-discard` when you really mean to drop a result
  (e.g. `list.add(x) __ Unit`).  Defined in `basics/src/Datum.scala`.
- **Opaque types + extension methods** are the standard zero-cost wrapper
  (`Fu`, `Sync`, `Mem`, `Or` itself).  Prefer them over wrapper classes.
- **`inline`** on hot extension methods to avoid boxing/closure overhead.

## Style conventions

The aim is **compact, expressive code with short names**.  Match the surrounding
file; the points below are the through-line.

**Layout**
- Significant indentation, not braces.  Braces are for class/object bodies and
  the occasional multi-statement lambda.  `then`/`else` go on the same line as
  `if`.  One-line bodies are encouraged: `if x < 0 then -x else x`.
- Soft line limit ~120 chars.  Keep type signatures on one line when reasonable.
- No `=`-alignment or comment-alignment across lines.
- Single blank line between logical chunks; two between top-level definitions.

**Naming**
- Short names for locals and loop vars: `i j k n m`, `p v x y z a b c`.  These
  are idiomatic here, not sloppy.
- Type params are single uppercase letters: `A B X Y Z E`; multi-letter only when
  it genuinely aids meaning (`CC[_]` for a collection ctor).
- camelCase for methods/vals; symbolic operators for pervasive ops (`|`, `&`,
  `+#`); `?`-suffixed names for the early-return forms.

**Control flow**
- `while` loops with a manual index on hot paths, in preference to combinators —
  performance is a feature.  `@annotation.tailrec` where recursion is clearer.
- `boundary`/`boundary.break` and `.?` for non-local exits; `fold`/`match` over
  ad-hoc mutation where it stays clear.

**Comments**
- Docstrings (`/** … */`) on public API — explain the *why* and the contract,
  not the mechanics.
- Inline `//` comments only for what a skilled reader could *not* infer from the
  code itself (a subtle invariant, a memory-ordering reason, an edge case).
  **Never comment the obvious.**  `i += 1` is never annotated.
- Major section dividers use slash borders (see `flow/src/Flow.scala:20`,
  `flow/src/Data.scala:240`):
  ```
  //////////////////////////////////////
  /// Early returns with ? a la Rust ///
  //////////////////////////////////////
  ```
  Lighter `// === foo ===` / `// --- foo ---` subheads also appear (loom); be
  consistent within a file.

## Good exemplars to read

- `flow/src/Or.scala`, `flow/src/Flow.scala` — the `Or`/`.?` core; the canonical
  compact-but-documented style.
- `flow/src/Data.scala`, `basics/src/Data.scala` — tight `while`-loop array code.
- `loom/src/Fu.scala` — opaque-type wrappers, `using` executors, `inline` work
  blocks, the `Sync` lock abstraction.
- `maths/src/Maths.scala` — numeric kernels with explanatory comments only where
  the math is non-obvious.

## The loom concurrency model (Go/Chan)

`loom/src/Go.scala` + `loom/src/Chan.scala` implement a CSP / Go-with-`select`
model on virtual threads.  `Go.session { ... }` opens a structured-concurrency
scope; inside it `Go { ... }` spawns tasks whose bodies register channel ops
(`put`/`get`/`into`) plus `Stop.on`/`Defer`, then run a persistent select loop.
`Chan[A]` is a bounded MPMC ring-buffer channel, usable imperatively
(`send`/`recv`/`trySend`/`tryRecv`) or declaratively inside a `Go` task.  These
files lean on `java.util.*` collections and explicit `var` state for the hot
path; that's intentional, but the surface API and comments should still read as
idiomatic kse3 Scala.
