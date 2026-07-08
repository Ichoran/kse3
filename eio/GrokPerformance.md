# Grok performance: findings and the floating-point plan

Status as of 2026-07-03.  Grok.Str works and is tested; this documents what we measured while
tuning it, which architectural invariants came out of that, and the one big deferred item
(fast correctly-rounded Double parsing).  Nothing here blocks making Grok *usable*; it is the
record of why the code looks the way it does and what to do when we come back for speed.

Benchmark: parse a JSON array of 100 numbers, knowing it is a flat numeric array (direct-mode
cheating allowed).  `benchmarks/grok/GrokJsonBench.scala`, run per its header comment.
All numbers below are whole-array parses per microsecond on the dev box (14900HX, JDK 25,
pinned with `taskset -c 4`); ~4% run-to-run noise.

## Where things stand

Ints (`(digits)` = "full" is random Ints, ~10 digits; "1" is 0-9):

| parser                            | full  | 1-digit |
|-----------------------------------|-------|---------|
| jsoniter-scala (bytes)            | 1.28  | 3.25    |
| hand-rolled fused charAt loop     | 0.93  | 2.96    |
| **Grok.Bytes**                    | **0.84** | **1.24** |
| Jackson streaming (String)        | 0.77  | 1.16    |
| Jackson streaming/databind (bytes)| 0.65  | 1.16    |
| handRolledOps (see below)         | 0.60  | 1.26    |
| **Grok.Str**                      | 0.60  | 1.04    |

Grok.Bytes (direct Array[Byte] backing, ~30 lines thanks to the worker templates) parses digits at
0.42 ns each — cheaper per digit than the hand-rolled *String* loop (0.78) and than jsoniter (0.49).
It beats Jackson's best mode by ~9% and like-for-like Jackson-on-bytes by ~28%; the remaining gap to
jsoniter is entirely per-element (8.1 vs 3.1 ns: op decomposition vs their fused generated codec).
But Grok.Chars (Array[Char]) matches Bytes (0.85 vs 0.81-0.84), so at L1-resident sizes the win over
Str is `String.charAt` overhead (coder branch + double bounds checks), not byte-narrow loads; byte
width should matter once inputs stream through cache.  OPEN IDEA: `Grok(String)` could internally
`toCharArray` and use the Chars machinery (~+35% for one transient allocation).  Grok.MemBytes
(`Mem[Byte]`, MemorySegment-backed) runs at 0.70 — the FFM read path costs ~17% vs a direct array
(segment bounds/liveness checks C2 does not elide here); fine for off-heap data, use Bytes when an
array is available.

Two negative results worth remembering (both reproduced, then reverted): Int instead of Long for
cursor/indices was only ~5-7% (not worth losing >2G sources); and folding the Long checked tail
into `longWork` cost 2x per digit at 10 digits even though the identical structure with an Int
accumulator was fine — C2 codegen around the busier method body punishes the Long loop
specifically.  Also measured: inter-run (fresh-JVM) variance on these benchmarks is ~±5% from JIT
compilation decisions, notably worse than intra-run error bars — only within-run comparisons or
>10% effects are trustworthy.

Doubles in (-1, 1) (`dprec` = decimal digits):

| parser                            | 10 digits | full (~17 digits) |
|-----------------------------------|-----------|-------------------|
| jsoniter-scala (bytes)            | 0.89      | 0.57              |
| **Grok.Bytes**                    | **0.62**  | **0.089**         |
| **Grok.Str**                      | **0.53**  | **0.094**         |
| hand-rolled span + JDK parse      | 0.21      | 0.095             |
| Jackson streaming (String)        | 0.19      | 0.088             |

(Grok rows updated after the digit-kernel doubleImpl rebuild, 2026-07-03; before it, Grok-on-Str
measured 0.38 / 0.082.)

Two headlines:
1. **Doubles that fit our Clinger fast path (≤15 sig digits, |e10| ≤ 22): Grok beats Jackson 3x**,
   because Jackson delegates doubles to `Double.parseDouble` (its FastDoubleParser mode is off by
   default) and we do our own exact-arithmetic conversion.
2. **Full-precision doubles are a disaster for everyone who delegates to the JDK.**
   `Double.parseDouble` costs ~100 ns per 17-digit number.  Grok, Jackson, and a pragmatic
   hand-roller all land in the same 0.08-0.10 bucket, 6x behind jsoniter, and Grok pays extra
   because our scanner finds the span and then the JDK re-scans a substring of it.

## What we learned tuning the int path (chronology of experiments)

Grok went 0.451 → 0.585 (full) / 0.845 → 1.03 (1-digit).  The experiments, including the failures,
because the failures were the informative ones:

- **JIT inlining was NOT the problem.**  `longWork` compiled to 714 bytecodes, over HotSpot's
  325-byte hot-inline budget (`-XX:+PrintInlining` shows "hot method too big").  Forcing it inline
  (`-XX:FreqInlineSize=800`) made things *worse* (0.576 → 0.476): C2's refusal was protecting
  register allocation.  Manually specializing the worker (direct `charAt`, Int locals, 370 bytes)
  changed nothing either.  Conclusion: don't chase bytecode size for speed here; chase it only if
  it blocks something you've measured.
- **Scalac-`inline` bloat is real but mostly harmless at runtime.**  Template expansion produced
  accessor invokes for every `var` touch and a `Char$.char2int` per lambda read; the JIT collapses
  them all.  Keep scalac `inline` for what it buys: monomorphizing the `at` accessor per source
  type, and keeping `boundary.break` in tiny shims where it compiles to a jump.
- **The cost was redundant character reads across op boundaries.**  A `sp`/`peek`/`< ","`/`I`
  loop read the comma 3x and the first digit up to 5x.  Measured via `handRolledOps` (the same op
  decomposition with zero machinery): the decomposition itself costs ~4.5 ns/element over a fused
  loop, machinery another ~4.
- **Fix: the always-loaded lookahead invariant.**  `cc` always holds the character code at the
  cursor (-1 exactly when at end of view), `skipped` records that delimiters are already consumed.
  A loop's terminating read becomes the next op's lookahead; `peek`/`peekOr` are field reads with
  no call; ops need no bounds checks (cc == -1 encodes end).  Every cursor move must eagerly
  reload cc; `select` saves/restores it; bytes use unsigned 0-255 so -1 stays unambiguous.
  Worth ~20% at 1 digit.
- **Digit loops: no overflow checks for the first 18 digits** (a Long can't overflow), rare
  checked tail in a separate worker (`longTailWork`); loop exits by `boundary.break` when the
  18-digit window is exhausted so the in-loop load needs no bounds guard.  Per-digit marginal
  cost now ~0.72 ns, matching a hand-rolled Int loop.
- **Error handling lives entirely on the error branch.**  Workers do not clear `eCode` on entry;
  it is 0 except in flight between an error branch setting it and `failErr()` consuming-and-
  clearing it.  Perf-neutral (predicted-not-taken store was ~free) but the right invariant.
- Remaining gap to the fused loop (~6 ns/element) is spread thin: op-boundary field round-trips,
  the non-inlined `longWork` call, `eCode` checks, `ArrayBuilder` vs raw array, ~158 ns/parse of
  fixed setup (Str alloc + boundary Label + builder; Jackson keeps its equivalent at ~55 ns by
  pooling buffers in a ThreadLocal).  Individual trims now return less than measurement noise;
  next steps here need `perf` counters (`kernel.perf_event_paranoid=1`) or a structural change.
- CORRECTION 2026-07-07: measured directly (GrokInitBench: parse one small value, so setup
  dominates), per-parse fixed cost is far below that 158 ns estimate — the bulk of it was the
  benchmark's own `ArrayBuilder` and measurement residue.  `Grok(s)(g => g.I)` = 7.6 ns / 80 B
  vs `Integer.parseInt` 2.9 ns / 0 B, so the whole harness (source object + boundary Label +
  the boxed primitive that `Ask` itself costs) is ~5 ns and two small allocations; buffered
  adds the window array and fill closure (16.8 ns / 232 B).  `Grok(s)(g => g.D)` at 11.6 ns
  beats bare `Double.parseDouble` (17.4 ns) outright — the Eisel-Lemire kernel more than pays
  for the harness.  Conclusion: initialization needs no design change; the reusable/resettable
  Grok idea (below) is retired as not worth its API surface.  For many tiny records, the right
  tool is one Grok over the whole input with the delimiter hierarchy, not per-record instances.

## The deferred big item: Eisel-Lemire Double conversion — DONE 2026-07-07

The fix for full-precision doubles is the Eisel-Lemire algorithm (Lemire, "Number Parsing at a
Gigabyte per Second", 2021): convert (mantissa ≤ 19 digits, decimal exponent) to a correctly
rounded Double with one or two 64x64→128 multiplies against a precomputed powers-of-ten table.
This is what jsoniter-scala, Go, Rust, and fast_float all do.

Landed as `maths/src/EiselLemire.scala`, reimplemented from the algorithm's principles (error
bounds rederived in the comments, constants verified with Mathematica) rather than translated
from any existing implementation.  Measured effect (JMH, GrokJsonBench, full-precision
shortest-roundtrip doubles): Str 0.081→0.406, Bytes 0.086→0.423 ops/µs — 5x, from 14% ahead of
Jackson to 4.2x the JDK-fallback hand-rolled loop and within 25% of jsoniter-scala (0.548);
10-digit doubles unchanged (Clinger path, now also inside the kernel, gated on `w ≤ 2^53 &&
|e10| ≤ 22` instead of a digit count).

Deviations from the design sketch above, both discussed as options there:
- **Sentinel**: returns `Double` with *canonical* NaN as the punt signal, not bits-as-Long with
  a non-canonical payload — the scanner consumes NaN/Infinity literals before the digit path,
  so a real NaN can never emerge from digits and the simpler contract is unambiguous; it also
  makes the truncated-mantissa acceptance test just `toDouble(w,q) == toDouble(w+1,q)` (NaN
  punts compare unequal on their own).  Signature took `ULong` for the mantissa (house style
  for "bits are unsigned").
- **Table**: computed exactly from `BigInteger` at class load (sub-millisecond) instead of a
  generated 10.4 KB source literal; Mathematica-computed reference entries are asserted in
  `EiselLemireTest` instead.
- `EiselLemire.toFloat` narrows the correctly rounded Double with a midpoint-pattern guard
  (punt when the Double sits exactly on a Float rounding midpoint, or below normal Float
  range), replacing Grok's bespoke Float midpoint check; Grok's `doubleImpl` numeric tail is
  now just kernel-call + compare + JDK fallback, and `Grok.pow10` is gone.
- Subnormals, overflow edges, unresolved carry ambiguity, and possible round-to-even ties punt
  to the JDK: correctness never depends on the fast path succeeding.  Punt rate measured <0.1%
  on random mantissa/exponent pairs away from the subnormal band.

Verification: table spot-checks vs Mathematica; the `(q*108853)>>15 = floor(q log2 10)` shortcut
proven exhaustively over |q| ≤ 400 against exact integer log; 300k random (w, q) differential vs
the JDK in maths; in GrokTest, 10k shortest-roundtrip round-trips over random Double bit
patterns, the historic torture values (`2.2250738585072011e-308`, 2^53±1 midpoints, subnormal
and overflow boundaries, 19-nines, >19-digit truncations), plus the pre-existing 500 random
decimal strings with exponents in [-320, 320].

## Traversal (iterator) mode: measured cost of pull-based access

DECIDED 2026-07-03: the windowed design (next section) won and the Nx implementation was
deleted (it was never committed; this section is its record).  Deciding data: head-to-head on
ByteBuffer — the motivating use case — the window beat pull by +22% (64-byte window) to +37%
(512), because pull pays the source's per-char access cost (`get()` dispatch and checks) where
the window pays it once per bulk refill.

For sources that cannot (or should not) be indexed — ByteBuffer via relative `get()`, streams —
we prototyped parallel worker templates (`...ImplNx` in Grok.scala) over three inline
capabilities: `nx()` (next char or -1 at view end), `rw(p)` (reposition to an already-read
absolute position — bounded backtracking for failed probes, dangling exponents, and value
re-reads), and `cut(a,b)` (text of an already-read span, for tok strings and the JDK double
fallback).  That set is sufficient for every core op; a `has(n)` capability turned out to be
unnecessary (see below).  The global cursor `i` stays template-maintained, so error reporting is
unchanged.  `select` and `grok` sub-parses are NOT yet traversal-safe (they move the cursor
without informing the source); decision deferred until we choose between this and a buffering
design.

Same machinery, same `Array[Byte]`, only the access discipline differs (ints/µs, whole-array):

| workload            | index (Bytes) | traversal (NxBytes) | traversal (ByteBuffer get()) |
|---------------------|-------|-------|-------|
| ints, ~10 digits    | 0.83  | 0.60 (-28%) | 0.47 (-43%) |
| ints, 1 digit       | 1.14  | 1.02 (-10%) | 0.87 (-24%) |
| doubles, 10 digits  | 0.56  | 0.54 (-3%)  |       |
| doubles, ~17 digits | 0.091 | 0.091 (0%)  |       |

(Also new here: index-mode doubles over *bytes* run 0.56 at 10 digits vs the 0.38 recorded for
Str above.)

Findings:
- **Fixed per-element cost of traversal is small** (~1 ns/element: the 1-digit and double rows).
  Doubles are traversal-neutral because index-mode `doubleImpl` does guarded loads throughout
  anyway; `nx()`'s internal guard costs the same.
- **Hot digit loops pay 2x per digit** (0.36 → 0.77 ns/digit).  FALSIFIED as the cause: the
  end-of-view guard inside `nx()`.  Adding `has(n)` + unguarded `nxu()` to recover the index
  template's unguarded 18-digit window bought only ~3%, within noise of the guard theory being
  wrong.  The remaining structural difference is that the adapter's cursor is a *field* mutated
  once per character (a store the JIT cannot hoist across safepoint-bearing loop back edges),
  where index mode loops on a local and commits the cursor once per op.  This appears inherent
  to a pull interface: the source owns per-char mutable state.  (Inference from elimination, not
  from perf counters; check with `-prof perfasm` if it ever matters.)
- **ByteBuffer relative `get()` adds another ~15-20%** on top of traversal discipline: its own
  internal bounds/position bookkeeping.  An index-mode Grok over absolute `get(i)` would likely
  do better; measure if ByteBuffer parsing becomes a real use case.
- Where this lands: traversal-mode Grok on digit-heavy input ≈ Grok.Str ≈ 0.9x Jackson-on-bytes
  (vs index mode's 1.28x).  Fine as the *adapter* story for genuinely non-indexable sources; not
  a replacement for the index templates.
- The alternative if this is deemed too slow: a **chunked/buffering design** (adapter refills an
  internal array; index templates run over the chunk; token-straddles-boundary handled by
  compact-and-refill, as Jackson/jsoniter do).  That gets index speed for chunk-resident tokens
  at the cost of refill plumbing in the view-end paths, and it would obsolete `rw`/`cut` (the
  buffer itself provides both).

## Windowed (chunked-buffer) mode: measured, and why it is not free either

THIS IS THE SETTLED DESIGN (2026-07-03) for pull-based sources.  Final form: `Grok.Buffered`, a
sliding window with initial size 64 (constructor param) that DOUBLES whenever retention leaves
no room to advance — so oversized tokens/values/delimiter-runs work, they just cost memory.
`select` support: `pinWork(pos)`/`releaseWork(token)` hooks on the base class (no-ops for
indexed sources, LIFO-nesting via the returned previous pin); `select` pins its start so failed
alternatives can re-read arbitrarily far, and the window grows to hold everything since the
pin.  No selectImpl template was needed — the field-restore in shared `select` is already
correct because `at(j)` self-heals; the pin only governs retention.  Known caveat: select
releases its pin on its own exit paths, but a user-level boundary.break unwinding THROUGH
select skips the release — if the user catches it and keeps parsing, the window retains from
the stale pin (unbounded growth, not incorrectness).  Adding pin/growth was
verified perf-neutral (pinned is touched only in cold scoot; select's pin calls are per-select).

`Grok.Buffered`: a small sliding window (default 64 bytes) fed by a pull function, running the
ORDINARY index templates.  `at(j)` inline-translates `j - discard` into the window and calls an
out-of-line scoot-and-refill when the read falls off the loaded end; a new `inline advise(n)`
hook in `longImpl`/`zImpl`/`matchTokImpl`/`doubleImpl` (no-op lambda for plain sources)
pre-arranges the window at op start.  Scooting retains from the op-start cursor — which works
because workers only commit `i` on completion, so field `i` is exactly the earliest position an
op can re-read.  Bonus semantics vs traversal mode: `select` and `grok` sub-parses just work
(select via pin, above); error positions stay absolute (excerpts degrade outside the window).

Ints (ops/µs; per-parse costs INCLUDE allocating the window, since the benchmark parses ~1.3 KB
per Grok — long-lived streams would amortize that):

| source                                   | full  | 1-digit |
|------------------------------------------|-------|---------|
| index (Bytes)                            | 0.83  | 1.20    |
| window aliased to input (control: no alloc/copy/refill) | 0.65 | 1.06 |
| window 512                               | 0.63  | 1.05    |
| window 64                                | 0.58  | 1.03    |
| traversal Nx (for comparison)            | 0.59  | 0.97    |
| window 4096 (per-parse alloc dominates)  | 0.56  | 0.84    |
| window 64 fed from ByteBuffer            | 0.57  | 0.96    |

Doubles at 10 digits: index 0.56, Nx 0.54, window-64 0.40 — windowing is WORSE than pull mode
for doubles because it taxes every `at()` call and `doubleImpl` reads some chars 2-3x (NaN/Inf
probe, end checks), where `nx()` touches each char once; plus scoot+alloc on the longer text.

Cost decomposition (each verified against 2+ rows):
- **Window read discipline ≈ 0.3 ns/char** (the aliased-window control), ~2x what was predicted.
  FALSIFIED first theory: that scoot frequency at 64 bytes was the dominant cost (bigger windows
  barely helped, and 4096 was *worse* per-parse).  The discipline cost is `j - discard`
  translation plus two field loads that C2 must reload per read (the slow branch calls
  `fetched()`, which clobbers them) plus a second, non-elidable bounds check on the window array.
- **Refill/scoot ≈ 0.15 ns/char more at 64-byte windows**, shrinking with window size.
- **Per-parse window allocation** (`new Array[Byte](w)` zeroing): what sinks 4096 here; vanishes
  for reused Groks / long streams.  Same class of issue as the known ~158 ns fixed setup cost.

Bottom line so far: pull mode pays a per-char cursor store; window mode pays per-read
translation + field reloads.  Both land ~25-30% behind index on digit-heavy input.  The open
idea to reach index speed within window mode: after `advise(n)` *guarantees* residency of
[i, min(i+n, iZ)), a bounded op could legally use a fetch-free accessor (`buf((j-discard).toInt)`
with no slow branch), which C2 can fully hoist — i.e., two at-lambdas per windowed source, one
resident-unguarded for advised ops, one self-healing for unbounded scans.  Untested.

## The digit kernel (adopted 2026-07-03)

Number scanning now runs through one shared kernel, `digitsImpl`, instantiated per source as
the `digitsWork` worker and CALLED AS A PLAIN METHOD from `longImpl` and `doubleImpl`
rather than expanded inline.  Measured trio on full-digit ints — current longImpl 0.814,
kernel-inlined 0.804, kernel-as-method 0.816 (±0.004); at 1 digit the kernel variants swing
1.17-1.29 across forks (JIT layout noise) vs baseline 1.17, so: parity or slightly better,
never worse.  `digitsWork` is 167 bytecodes (half the 325 hot-inline budget) and every call
site is monomorphic (a final class's worker calling its own method), so C2 inlines it anyway;
the method form keeps the callers small (longWork went 628 → 511 bytecodes) and lets several
ops share one instantiation instead of re-expanding the template.

Design points:
- The kernel accumulates POSITIVELY and commits the cursor itself (i to end of run, cc to the
  guarded lookahead there) and returns the value; the caller recovers the digit count as
  i - j0.  No scratch fields: i and cc are the natural output channels, and the transient
  cc-invariant gap (cc ahead of i between kernel return and the caller's error checks) is
  unobservable inside a worker.
- Negative accumulation is GONE from the hot path.  It only ever mattered for Long.MinValue,
  which always has 19 digits and therefore goes through `longTailWork` — which still works in
  negative space, receiving `-x`.  (Negating ≤18 digits is always safe.)
- Positive accumulation is what a future 19-digit budget needs: 19 nines exceeds
  Long.MaxValue but fits unsigned, so Eisel-Lemire's mantissa can come straight out of the
  kernel with `budget = 19` and unsigned ops downstream.
- `doubleImpl` was rebuilt on digitsWork the same day (two kernel calls: integer part and
  fraction, spliced with `mant * pow10L(fd) + v`, which cannot overflow at ≤18 total digits;
  leading-zero skipping and dropped-digit skimming stay in the caller; each character is now
  read exactly once).  On windowed sources the kernel's eager commit advances the retention
  point mid-number, so `doubleImpl` pins vPos across its body (single-exit structure; no-op on
  indexed sources) to keep the slow-path `sub(vPos, j)` readable — tested with a slow-path
  double straddling the window edge.  Verified against 500 random decimal strings
  differentially with `Double.parseDouble`.  Measured (10-digit / full-precision doubles):
  Bytes 0.56→0.62 / 0.09 flat; Str 0.38→0.53 (cc-threading pays most where reads were dearest);
  Buffered-64 0.40→0.48 (every removed re-read also saved the window tax).  Full precision
  stays parseDouble-bound until Eisel-Lemire.

## Quoted strings (kernels settled 2026-07-05)

`str`/`strChars`/`strBytes`/`strSpan` with a `Quote` style spec (JSON backslash escapes or
CSV/SQL quote-doubling), on every source.

**Architecture.**  Scan to the closing quote ignoring delimiters; an escape-free string is
extracted in one piece (substring / `copyOfRange` — a clean `strBytes` never decodes).
Escaped strings flush clean segments in bulk (per-source `blit`: getChars / arraycopy) into
a doubling buffer in the source's native width — UTF-16 for char sources, UTF-8 for byte
sources (escaped surrogate pairs join there, since UTF-8 encodes the joint code point; lone
ones become U+FFFD) — with escapes decoded between flushes.  The `\u` hex decode is the
plain routine `Grok.hex4` (select-and-add normalization: no data-dependent branches, so the
worst case degrades gracefully), tested early in the escape dispatch since unicode is often
not rare.  UTF-8 emission is the plain routine `Grok.putUtf8`.  Cross-width outputs convert
once on the finished buffer (`utf8Bytes` / String's intrinsic decoder), never in the loop.
Windowed sources pin the string start so segments stay blit-able.

**Standing** (`GrokStringBench`, ops/µs whole-array parses: easy = 50 short clean words,
esc = simple-escape-dense, uni = `\uXXXX`-dense, raw = unescaped non-ASCII; reference rows
from same-box runs):

| benchmark                  | easy  | esc   | uni   | raw   |
|----------------------------|-------|-------|-------|-------|
| grokStr                    | 1.24  | 0.107 | 0.065 | 0.108 |
| grokStrBytes               | 1.18  | 0.122 | 0.048 | 0.114 |
| grokStrBytesRaw (stay-UTF-8)| 1.54 | 0.125 | 0.065 | 0.259 |
| grokBufStr (win 64)        | 1.00  | 0.094 | 0.038 | 0.081 |
| grokBufCharsStr (win 64)   | 1.09  | 0.091 | 0.043 | 0.167 |
| jacksonStreamBytes         | 1.51  | 0.20  | 0.11  | 0.14  |
| jacksonStreamString        | 1.60  | 0.21  | 0.11  | 0.28  |
| jsoniterScala              | 2.12  | 0.16  | 0.10  | 0.11  |

Clean strings are competitive everywhere (raw: grokStr beats Jackson-String; the stay-UTF-8
`strBytes` path is 2.3× jsoniter).  Escape-dense sits ~1.6-2× behind Jackson (down from the
3-4× of the first working version); the byte source now beats the String source on ASCII
escapes, and the windowed sources pay only ~10-25% over indexed.

**Tried and rejected — do not redo without new evidence:**

- Incremental builders (StringBuilder with substring-per-segment; a byte builder with a
  capacity check per byte): 2-2.5× slower than bulk-blit chunking on escape-dense input.
- Escape dispatch via lookup table (one or two cache lines): ~5% slower than the compare
  chain.  Escapes are homogeneous within a string, so the chain predicts nearly perfectly;
  the table puts a dependent L1 load on the critical path.  Table geometry was irrelevant.
- Char-at-a-time building after the first escape: wins in hand-rolled isolation on clean and
  simple-escape input, but loses 10-25% in template context and loses every mode on windowed
  sources — the template's per-iteration overhead (mode guards, doubled test, windowed `at`)
  lands per char instead of per segment.  Hand-rolled shape rankings do not transfer to
  template context; decide shapes on in-template measurements.
- Two-pass exact-span decode and whole-span preallocation: an extra scan or a large zeroed
  allocation costs more than the room checks they remove.  These loops run at IPC ~6,
  L1-resident: throughput is 1/instructions, so "L1-warm" re-reads are not free.
- Hex micro-variants: serial-fold (fewer live registers) — no effect; branch-chain digit
  normalization — equal in situ, but keeps data-dependent branches, so select-and-add wins
  on robustness; forcing `hex4` to stay a call (`dontinline`) — 20% worse.
- Explanations falsified by perfnorm along the way: branch prediction (miss deltas two
  orders too small) and cache/store effects (IPC flat throughout).

**The load-bearing lesson (transferable to all kse3 template code).**  Scalac `inline`
templating does not generalize like C++ templates: pasting a compact computation into a fat
loop body measurably degrades C2's output versus the same logic as a plain routine that C2
inlines on its own terms (+33% on `\u`-dense input from outlining `hex4` alone, with
identical source-level work).  Template-expand only what genuinely varies per source (the
`at` accessor, extraction and blit lambdas); fixed computations should be plain routines
taking already-loaded values (`hex4`, `putUtf8`, `digitsWork`).

**Open:**
- Residual ~1.6-2× to Jackson on escape-dense input: per-string op decomposition and
  bounds-check overhead vs Jackson's array-direct text buffer; not yet isolated.  (One
  loose end from the shape study: the per-char template build emitted ~100k instructions
  per parse beyond source-level accounting, PrintInlining symmetric — would need
  hsdis + perfasm to explain, but nothing rests on it now.)
- MemBytes blits with a per-element loop (no arraycopy across the FFM boundary): expect
  escaped-string lag on Mem until a bulk copy (MemorySegment.copy) or a parallel access
  pattern replaces it.  No Mem row in GrokStringBench yet.

## Other open performance items (all secondary)

- `Grok.Buffered` requires the total input length up front (`iZ` = totalLen drives every view
  guard).  Unknown-length streams need a template-level end-of-input story (discovering the end
  when `fill` returns 0 and materializing it as cc == -1 without a known iZ).

- `Grok.Bytes` / `Grok.Chars` over `MemorySegment.ofArray/ofBuffer`: same templates, byte loads
  instead of UTF-16 `charAt` — this is where the remaining gap to jsoniter mostly lives.
- Benchmark fairness: add a Jackson variant with `USE_FAST_DOUBLE_PARSER` enabled.
- Per-parse fixed cost (~158 ns): Str allocation + boundary + builder; matters for many small
  parses.  A reusable/resettable Grok is possible if it ever matters.
- `select` currently spawns a closure per alternative and uses a cross-frame break on total
  failure; inline 2-4-arity overloads would compile alternative misses to same-frame jumps.
- ~~`doubleImpl` cc-threading~~ DONE 2026-07-03 via the digit kernel (see above).

## Hex readers and the cost of width intent (2026-07-06)

`xB/xS/xI/xL` = `hexImpl` kernel (shift-or accumulation, `hex4`-style select-and-add
normalization, loop exit doubles as validity test) + base-class `smallHexWork` orchestration
(zero-skim, one budgeted `hexWork`, digit-follows check; hex budgets fit their types exactly so
the value needs no range check).

**GrokHexBench** (100 values, taskset -c 4): hex ≈ decimal on the same numbers, ACCEPTED as-is.
Int full-range: 0.79 vs 0.82 ops/µs (Bytes), parity on Str; Long full-range 0.43 vs 0.47;
1-digit 2-5% behind.  The predicted char-count win (8 vs ~10.5 for Int) did not materialize:
these loops are IPC-bound, and hex costs more µops per digit (or + two cmov-adds + two range
compares + shift-or) than decimal (two compares + mul-add), which cancels the length advantage;
the two virtual dispatches per number from base-class orchestration tip it slightly negative.
(The bench's handRolledHexStr is NOT a valid ceiling — unhoisted length checks — ignore it.)

**GrokWidthBench** — same 4-digit values (fit everything, hit no digit cap) via S, I, L:
L 1.214, I 1.124, S 1.057 ops/µs.  Each abstraction layer between digit kernel and caller costs
~6-7% at this token size: I's two range-check compares in the inline shim ≈ 0.66 ns/number;
S's `smallLongWork` virtual orchestration (skipDelimsWork + digitsWork dispatch) ≈ 0.56 ns/number
more.  CONFIRMS: keep `I` on `longWork` (moving it would stack both penalties); `S/B/uB/uS/uI`
stay on `smallLongWork` (0.6 ns buys correct out-of-range errors); promote per-source only if a
bulk-small-int workload ever appears.
