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
| **Grok**                          | **0.38**  | **0.082**         |
| hand-rolled span + JDK parse      | 0.21      | 0.095             |
| Jackson streaming (String)        | 0.19      | 0.088             |

Two headlines:
1. **Doubles that fit our Clinger fast path (≤15 sig digits, |e10| ≤ 22): Grok beats Jackson 2x**,
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

## The deferred big item: Eisel-Lemire Double conversion

The fix for full-precision doubles is the Eisel-Lemire algorithm (Lemire, "Number Parsing at a
Gigabyte per Second", 2021): convert (mantissa ≤ 19 digits, decimal exponent) to a correctly
rounded Double with one or two 64x64→128 multiplies against a precomputed powers-of-5 table.
This is what jsoniter-scala, Go, Rust, and fast_float all do.  Expected effect: full-precision
doubles go from 0.082 to ~0.35 (the scan barely lengthens; the conversion is a few ns), i.e.
~4x Jackson at full precision, with the JDK fallback retained only for genuinely ambiguous cases.

Design decisions already made:
- **Lives in maths**, not eio: it is a numeric kernel; jsonal will want it too.  Signature sketch:
  take `(mant: Long, e10: Int)` (positive mantissa, the scanner applies the sign), return the
  Double *bits* as a Long, with one reserved non-canonical NaN payload as the "cannot decide,
  take the slow path" sentinel (parsing never produces non-canonical NaNs, so the pattern is
  unambiguous and the API stays allocation-free).  Grok's `doubleImpl` scan already produces
  mant/e10/truncated; extend digit collection from 18 to 19 significant digits.
- **Table**: 128-bit truncated reciprocals/powers of 5 for q in [-342, 308]; two Longs per entry,
  ~10.4 KB, generated by a small script (or Mathematica) into Scala source.
- **Compute**: `Math.multiplyHigh` (intrinsic on JDK 9+); the usual checks — normalize mantissa
  with `numberOfLeadingZeros`, upper-bit product check for the truncated-table ambiguity, round-
  to-even tie detection, subnormal handling below 2^-1022, infinity overflow above ~1.8e308.
- **Truncated inputs** (>19 sig digits): try EL with mant and mant+1; if both round to the same
  Double, that's the answer; else slow path.
- **Fallback**: existing path (`Double.parseDouble` on the already-scanned span).  Rare: for
  shortest-roundtrip and human-written decimals, EL decides >99.9% of cases.
- **Licensing**: implement from the paper, credit Eisel and Lemire in the file header; fast_float
  (MIT/Apache-2.0) and jsoniter-scala (MIT) may be consulted as behavioral cross-checks but no
  code is to be copied.  BSD-3 compatible.
- **Verification**: differential against `Double.parseDouble` — millions of random bit patterns
  round-tripped through `toString`; random decimal strings of 1-25 digits with exponents across
  [-330, 320]; torture cases (exact halfway values, `2.2250738585072011e-308`, subnormal
  boundaries, `1.7976931348623157e308`, long all-9s mantissas); Mathematica available for
  independent reference values.

## Other open performance items (all secondary)

- `Grok.Bytes` / `Grok.Chars` over `MemorySegment.ofArray/ofBuffer`: same templates, byte loads
  instead of UTF-16 `charAt` — this is where the remaining gap to jsoniter mostly lives.
- Benchmark fairness: add a Jackson variant with `USE_FAST_DOUBLE_PARSER` enabled.
- Per-parse fixed cost (~158 ns): Str allocation + boundary + builder; matters for many small
  parses.  A reusable/resettable Grok is possible if it ever matters.
- `select` currently spawns a closure per alternative and uses a cross-frame break on total
  failure; inline 2-4-arity overloads would compile alternative misses to same-frame jumps.
- `doubleImpl` is 1838 bytecodes; harmless per the inlining findings, but it should get the same
  cc-threading the int path has (currently it re-reads a char the cache already holds and does
  its own guarded loads throughout).
