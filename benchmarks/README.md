# kse3 benchmarks

JMH microbenchmarks, run through [scala-cli](https://scala-cli.virtuslab.org/) against an
**assembled** build of the library — i.e. the same artifact you'd get from a release, so each
benchmark is exactly what an external user could write after adding kse3 as a dependency. The
only kse-specific thing in the sources is `import kse.loom.*`.

Each benchmark area is its own folder (a self-contained scala-cli project).

## Running

Build the assembled jar once (from the repo root):

```
mill all.assembly        # -> out/all/assembly.dest/out.jar
```

Then run a benchmark area (from the repo root):

```
scala-cli --power run benchmarks/loom --jmh                       # full suite
scala-cli --power run benchmarks/loom --jmh -- -f 1 -wi 3 -i 5    # custom JMH args
scala-cli --power run benchmarks/loom --jmh -- SpscBench          # one class
```

Requires a JDK 25+ on `PATH` (the library targets `-release 25`). The benchmark points at the
local assembly via `//> using jar ../../out/all/assembly.dest/out.jar`; against a real release
you'd swap that for `//> using dep com.github.ichoran::kse3-all:0.5.0`.

If scala-cli's incremental compiler ever errors with `FileAlreadyExistsException`, remove the
build cache for that area: `rm -rf benchmarks/<area>/.scala-build`.

## benchmarks/splitdeque — SplitDeque vs java.util(.concurrent)

All scores are **items/second** moved end-to-end. Items are references from a fixed
`String[]` pool walked in scrambled order (no per-item allocation, no prefetch-friendly
adjacency). No Blackhole: the consumer does a real trivial task — `sum` (string lengths)
or `copy` (into a pre-allocated output array) — and the result is returned to JMH.
Concurrent workers are **platform** threads; pin runs to one core complex
(`taskset -c 0-15` = P-cores on the 14900HX) or variance swamps the results.

| benchmark | what it measures | params |
|---|---|---|
| `SdPipeBench.sd` / `.sdInto` | m producers → one SplitDeque → n consumers; single (`pushRight`/`popLeft`) or block transfer — `sd` extracts via `splitLeft`+drain, `sdInto` via `popLeftInto` straight into an array | `pairs` 1/4/8, `in` 1/32, `out` 1/8/32, `task` |
| `BlockingPipeBench.abq` / `.lbd` | same pipe over `ArrayBlockingQueue` / `LinkedBlockingDeque`; block extraction via `drainTo`, no block insert (compare `in=1` rows) | `pairs`, `out`, `task`, `capacity` (1024) |
| `CldPipeBench.cld` | same pipe over lock-free unbounded `ConcurrentLinkedDeque`; no block ops (compare `in=1, out=1` rows) | `pairs`, `task` |
| `SoloBench` | single-threaded fill-1000/drain-1000 cycles: `batch` (raw engine, no atomics) vs `sd` (uncontended lock) vs `adq` (`ArrayDeque`) vs `abq` | — |
| `RebatchBench` | single-threaded chunked container-to-container moves: `splice` (split/splice, O(blockSize·log n) per chunk) vs `spliceSd` (+lock) vs `singles`/`adq` (element loops) | `chunk` 32/1024 |

### Findings (2026-06-10, JDK 25, taskset -c 0-15, default geometry lgCap=6/blockSize=24)

Items/second, `sum` task (`copy` was indistinguishable everywhere — the benchmark measures
coordination, not the task, as intended).

- **The rebatching thesis holds where it's supposed to.**  Batch-in/batch-out (`32/32`)
  moves 50M items/s at 1×1, 33.7M at 4×4, **29.6M at 8×8** — degrading only 1.7× from no
  contention to 16 threads, and 3–4× faster than ABQ/LBD in any shape at high contention
  (7× CLD).  One lock acquisition per 32 items is the whole story.
- **Per-item ops under contention are the weak spot.**  At `1/1`, 8×8: sd 4.9M ≈ CLD 4.3M,
  vs ABQ ~7–12M (blocking-queue runs are noisy, ±50%+ errors) — the single len-spinlock
  serializes everything where ABQ has separate put/take locks and parks.  Mixed shapes
  (`1/32`, `32/1`) land in between (~9–10M at 8×8): the single-item side is the bottleneck.
- **Uncontended single-op cost ≈ ABQ.**  Solo: sd 43M/s vs abq 48M/s; the raw engine
  (`batch`) does 163M/s, so the shell's two atomics + sentinel check cost ~3.8× the engine
  op; `ArrayDeque` is in another class (534M/s).
- **`popLeftInto` (2026-06-10 addition) makes small blocks pay.**  Direct array
  extraction — one lock acquisition, no Batch, sentinel swap fused into the copy
  (≤127 copies under the lock; bigger cuts detach via split and copy lock-free) — at
  `in=32`: out=8 goes from 16.6M (`splitLeft`) to **31.6M** items/s at 8×8, i.e. an
  8-element cut now buys what `splitLeft` needed 32 for; at out=32 it's 10–20% ahead
  (copy task more: 44.0M vs 33.1M at 4×4 — it pops straight into the final array).
  At out=1 it matches plain `popLeft`.  Ceiling ~50M at 1×1 regardless of mechanism.
- **Split/splice is nearly free at large chunks but has real per-call overhead.**
  Single-threaded chunk=1024: 4.4G items/s (±1.3G — ~0.25ns/item, pure pointer shuffling)
  vs 516M for the ArrayDeque element loop.  But chunk=32 splice is *slower* than the
  element loop (359M vs 516M; spliceSd 287M): the O(blockSize·log n) fixed cost plus Batch
  allocation needs hundreds of items — or lock amortization under contention — to pay off.
  Crossover is between 32 and 1024 single-threaded; under contention 32 already wins big.

## benchmarks/loom — Go/Chan concurrency

All scores are **items/second** (items moved end-to-end, normalized via
`@OperationsPerInvocation`). Items are references drawn from a fixed pre-allocated `String[]`
pool, so nothing is allocated per item — this measures coordination, not the garbage collector.
Every kse benchmark has a plain-`java.util.concurrent` control of the same shape:

| benchmark | what it measures | control |
|---|---|---|
| `spsc` | single producer → single consumer | `abqSpsc` (`ArrayBlockingQueue`) |
| `selectN` | one consumer selecting over N persistent recv handlers | `abqSelectN` (consumer round-robin-polling N queues — the "rebuild the select each iteration" approach) |
| `fanIn` | N producers contending on one shared channel | `abqFanIn` (`ArrayBlockingQueue`) |
| `ChanNSpscBench.spscN` / `.spscNArr` | chunked spsc through ChanN, `put(n)` generation / `putN` array drain, batch ∈ {1, 8, 32} | the `SpscBench` rows |
| `ChanNFanInBench.fanInN` | N batched producers on one shared ChanN — the headline rebatching claim | the `FanInBench` rows |
| `ChanNSelectBench.selectN` | one consumer over N chunked channels, ± per-item producer work | the `SelectBench` rows |
| `ChanNLatencyBench` / `ChanLatencyBench` | per-item delivery latency p50/p99/p999 (printed each iteration from a 2x-bucket histogram), trickle & bursty regimes | each other |
| `ChanNFullGrabBench.fullGrab` / `.asAvailable` | `getFull` vs `get` at independently chosen producer batch and consumer chunk (m-to-n rebatching through the channel) | each other |

Reading the results: `spsc` vs `abqSpsc` says how close a *blocking, select-capable, lifecycle-
managed* channel gets to a bare blocking queue. `selectN` vs `abqSelectN` tests the design's
thesis (registering handlers once instead of rebuilding selection per iteration) — note the
control *spins* (busy poll) to reach its throughput, while `selectN` blocks, so equal throughput
already favors the blocking design on CPU. `fanIn` is the case most sensitive to the single
per-channel lock.

## ChanN findings (2026-06-10, initial: single fork, taskset -c 0-15, JDK 25)

ChanN is the chunked channel built on the SplitDeque engine (`loom/src/ChanN.scala`); it exists
because of the verdict below — chunking divides the per-item coordination costs by the batch
size.  Initial pinned numbers (items/s):

- **Fan-in, 8 producers — the headline holds.**  ChanN batch=32: **10.5M**, vs Chan 1.79M
  (5.9×, prediction was 4–5×) and ABQ 3.61M (2.9× — the shape Chan *lost* at 49–82%).
  Batch=8: 5.5M, already 1.5× ABQ.
- **SPSC scales to the engine's ceiling.**  Batch=32 `put(n)`: 36.5M vs Chan 7.6M ≈ ABQ 7.4M
  (4.8×); `putN` array drain: 44.5M — near the SplitDeque `popLeftInto` pipe ceiling (~50M).
- **batch=1 degeneracy is approximate, not exact**: 0.8× Chan on spsc (6.1M), 0.6× on fan-in
  (1.07M).  The chunked producer pays a `shortcut` boundary + a local-buffer hop per item where
  Chan's `put` pays only an `attempt`; possible to optimize later, but the knob story (turn the
  batch up) is the point.
- **Saturated select: the old select tax is gone.**  At batch=32 the consumer holds
  **~33–42M items/s flat across 1/4/16 channels**, vs Chan 6.8/3.2/2.2M and the ABQ poll
  sweep 7.7/2.7/2.6M — ~13–16x at 4–16 channels, because the per-block `arm`/`disarm` O(N)
  cost (what made Chan's select cross over) is paid once per batch.  batch=1 is ~0.9x Chan
  at every channel count, the best degeneracy in the suite.
- **Work-bound (blocking) select is an open question.**  At work=256 ChanN *lags* Chan at low
  channel counts — ch=1: ~280k vs 594k; ch=4: 1.05M vs 1.65M (Chan's old 1.9x-over-ABQ win
  shrinks to 1.2x) — and passes it only at ch=16 (2.7M vs 2.1M; ABQ's poll sweep still wins
  that cell at 3.4M, as it did against Chan).  Batch size barely moves these numbers (266k
  vs 286k at ch=1), so it is *not* the batching: some per-item cost in ChanN's trickle path,
  unidentified — and the latency bench below shows the *opposite* ordering at similar work,
  so it is likely scheduler/JIT-sensitive.  Profile before drawing conclusions.
- **Full-grab consumption is throughput-free, and the bottleneck side's chunk is what
  matters.**  `getFull` (consume only whole chunks) matched `get` within error in every cell
  of a producers {1,8} x pbatch {8,32} x cbatch {8,32} sweep — saturated channels rarely sit
  below a chunkful, so the mode's value is semantic (whole-chunk processing) plus its trickle
  behavior.  The crossed sizes are the real finding: with 8 producers and one consumer the
  *consumer's* chunk dominates (cbatch 8 -> 32 takes 5M -> 12M at any pbatch), and producer
  batch 8 *beats* 32 there (12.2M vs ~10M) — the surplus side hogging the lock with big
  bursts starves the bottleneck side.  Size the chunk on the bottleneck; keep the surplus
  side's modest.
- **Latency is the price, and it is exactly the predicted one.**  Trickle regime (~11us/item
  production): batch=1 is latency-identical to Chan (p50 3.1us, p99 6.1us), batch=32 sits at
  p50 ~197us — the first item of a batch waits for its 31 batchmates, since `put(n)` fills the
  whole batch before flushing (there is no time-bound flush; the batch size *is* the latency
  knob).  Bursty arrivals (work clumped per 64 items): batch=32 p50 ~98us vs Chan's ~25us, with
  equal p99 (~197us) — items produced together chunk for free.

## Findings (2026-05-30) — why this design was shelved

Measured on JDK 25, zero-allocation String pool. The persistent-select model's *founding thesis
was a performance claim* (goroutines waste work rebuilding select every iteration). It does not
hold up as a differentiator:

- **SPSC: ~at parity with `ArrayBlockingQueue`** (~7.5M items/s). Matching a 20-year-old primitive
  with far more machinery is not a win.
- **Fan-in: 62–82% of ABQ.** Lock-contention bound — all producers + consumer serialize on one
  channel lock; the fast path can't help because under saturation the producers genuinely *are*
  parked, so the scan can't be skipped.
- **Select: no clean win.** Against a busy-poll baseline it *crosses over* — loses ~2.6× at 16
  channels (our blocking pays `arm/disarm` O(N) per block while the poll just spins), wins ~2.4× at
  64 channels (the poll's O(N) sweep finally dominates). The real alternative to N-channel select is
  usually fan-in to one channel, which is already at parity.
- **It's coordination-bound, not overhead-bound.** Pre-allocating the transient `Or`/`Woe` results
  and using a bare `Array[Handler]` moved nothing; a GC profile showed ~17 B/op and `gc.count ≈ 0`.
  The time is in the lock + cross-thread cache traffic + park/unpark, not in the ease-of-use
  abstractions. The JIT already handled `ArrayList`/boxing/virtual dispatch.
- **A per-scope readiness-bitmask hint was tried and reverted** — it *regressed* select, because a
  saturated select has no empty channels to skip; it only added per-push CAS contention.

The one axis where the design genuinely differs — it *blocks* efficiently instead of spinning —
is a CPU/latency property a throughput benchmark can't show, and isn't novel versus Go/Kotlin
selects. Conclusion: no differentiating performance boundary; the model's only remaining
justification would be ergonomic, which was not pursued. Left in history as a testbed result.

## benchmarks/thyme — Thyme vs JMH (validation, not a benchmark of the library)

This area exists to keep `kse.thyme.Thyme` honest: it measures identical workloads with **JMH
(ground truth)** and with **Thyme**, so we can tell whether Thyme is measuring properly-compiled
steady-state code. `Work.busywork(n)` is O(n) integer churn with a loop-carried dependency, called
verbatim by both. (This project pins an explicit `jmh-core` dep so the Thyme-side `main` compiles
without `--jmh`; the JMH benchmark still runs under `--jmh` as usual.)

```
mill all.assembly
taskset -c 0-7 scala-cli --power run benchmarks/thyme --jmh -- -f 1 -wi 5 -i 5 -tu ns   # ground truth
taskset -c 0-7 scala-cli --power run benchmarks/thyme                                   # Thyme's estimates
```

### Findings (2026-06-22, JDK 25, taskset -c 0-7)

| workload | JMH avgt (ns/op) | Thyme /call | Thyme/JMH |
|---|---|---|---|
| w10    | 0.192   | 0.418 ns | 2.2× |
| w100   | 43.59   | 43.02 ns | 0.99× |
| w1000  | 520.4   | 500.1 ns | 0.96× |
| w10000 | 5469    | 5.031 µs | 0.92× |

- **For ops ≥ ~40 ns, Thyme matches JMH within ~1–8%.** This is the load-bearing result: it shows
  Thyme's steady-state detection and OSR-based warmup are measuring C2 code, not interpreter/C1 code.
  If Thyme were failing to warm up, it would read *wildly high* here; it does not.
- **Sub-nanosecond ops have a sink-overhead floor.** At w10 both tools agree the op is sub-ns
  (JMH 0.19 ns); Thyme reads 0.42 ns because every measured iteration also pays for `consume`
  (the anti-dead-code sink), ~0.2 ns/call. That overhead is negligible above ~40 ns and only
  visible at the floor. This is the price of guaranteed dead-code protection, and is the honest
  limit of an in-process tool — for sub-ns work, or anything you'll stake a decision on, use JMH.
- Workloads too slow to reach the warmup-invocation floor within `tooMuchTime` are reported by
  Thyme as **NOT CONVERGED** rather than as a confident (and possibly C1) number.

### benchOff — head-to-head (2026-06-22)

`th.benchOff(a)(b)` runs scrambled block-interleaved mixtures of `a` and `b` and fits time vs
mixture ratio; the slope is the per-call cost difference (robust to drift, since both run in the
same window). Validated against the JMH absolute scores above:

| comparison | true ratio (JMH) | benchOff verdict |
|---|---|---|
| w1000 vs w2000  | ~2×    | First faster 2.01× |
| w1000 vs w10000 | ~10.5× | First faster 10.2× |
| w1000 vs w1000  | 1× (identical) | indistinguishable |

- **The close call is where it shines and where it's most accurate** (2.01× on a true 2×). That's
  the question benchOff exists for — "is A faster than B, here, now" — and the one heavier harnesses
  find awkward.
- **Identical code is reported as indistinguishable, not a spurious win.** A "winner" requires the
  difference to clear a practical-significance floor (`accuracyTarget`), not merely statistical
  significance — two separately-inlined copies of the same code differ by ~1–2% from layout/caches,
  and crowning a winner on that would be crying wolf.
- Per-call *absolute* costs are mixture-context-dependent (cache pressure differs when w1000 is run
  against w2000 vs w10000); the **difference/ratio** is the robust, reported answer.
- Dispatch is **block-interleaved** (`OffBlock` calls per branch) so the first/second selection
  branch doesn't add per-call misprediction cost or a spurious order-of-evaluation warning.

### RNG choice: dropped Thyme's hand-rolled pcg32 (2026-06-22)

Thyme originally carried its own 32-bit PCG (`pcg32`, XSH-RR) on the theory that a small generator
might be faster than the maths module's 64-bit `Pcg64` for the cheap int draws Thyme makes while
shuffling. We settled it by dogfooding — `benchOff` summing N int draws from each:

| N draws | typical verdict (4 of 5 runs) |
|---|---|
| 100   | Pcg64 faster ~1.2× |
| 1000  | Pcg64 faster ~1.38× |
| 10000 | Pcg64 faster ~1.37× |

`Pcg64` won — likely because it carries a single 64-bit state word, while the `pcg32` we had carried
*two* Longs (state + a selectable-stream increment) and added the increment every step. So the
hand-rolled generator was duplicated code that was also slower. **`Pcg32` was removed; Thyme now uses
`kse.maths.Pcg64`.**

One run out of five flipped the verdict (it measured pcg32 faster at N=1000/10000). That outlier is a
finding in itself: for two implementations this close (~within 40%) and this JIT-sensitive, *which one
wins can depend on how the JIT happened to compile them in that particular JVM invocation*. This is
exactly why JMH forks multiple JVMs — to average over per-invocation compilation variance — and why
`benchOff`, which runs in the one JVM you're in, honestly reports "faster **here, now**" rather than a
universal truth. Within any single run `benchOff` was tight and self-consistent; the variance is
across runs. A 1.2× `benchOff` win on JIT-sensitive code may not replicate in a fresh JVM.

## benchmarks/jsaun — jsaun JSON vs Jackson / jsoniter-scala / uPickle

Two files, one scala-cli project.  All parsers/serializers work over the **same** shared input
(a ~100-record array of mixed-field objects serialized once with jsoniter, so no library is fed
its own dialect).  The reference points span the implementation spectrum: **Jackson** (Java,
reflective tree), **jsoniter-scala** (macro-specialized, schema-aware), and **uPickle/uJson**
(pure Scala, both tree and derived codec).

`JsaunBench` — two planes:

| plane | what it measures | jsaun | references |
|---|---|---|---|
| **tree** parse | text/bytes/chars → dynamic tree | `jsaunParse{String,Bytes,Chars}`, `jsaunParseExact` | `jacksonTree{Bytes,String}`, `ujsonParse{Bytes,String}` |
| **tree** serialize | tree → text / UTF-8 bytes | `jsaunPrint`, `jsaunPrintBytes` | `jacksonWriteBytes`, `ujsonWriteBytes` |
| **typed** parse | bytes → `List[GeoRecord]` | `jsaunCodecDecode` (derived `FromJson`) | `jsoniterDecode`, `upickleDecode` |
| **typed** serialize | `List[GeoRecord]` → bytes | `jsaunCodecEncode` (derived `Jsonize`) | `jsoniterEncode`, `upickleEncode` |

The tree plane is apples-to-apples (no side knows the schema); the typed plane pits jsaun's
`derives` codec against jsoniter's compiled codec (the schema-aware ceiling) and uPickle's.

`JsaunFormatBench` — **format-preserving read-modify-write**, jsaun's headline feature: parse a
pretty-printed document, change one field, serialize.  `jsaunFmtEdit` keeps every untouched byte
verbatim and re-emits only the edited token; `jsaunPlainEdit` is the honest in-family baseline
(same trees, plain parse + canonical reprint, formatting *not* preserved), and `jacksonEdit` /
`ujsonEdit` reflow the whole document because they have no format memory.  The `jsaunFmt`→`jsaunPlain`
gap is the price of the span bookkeeping; the `jsaunPlain`→others gap is raw parser/printer speed.

`JsaunMatrixBench` — an **n×n double matrix**, to exercise jsaun's **packed array backing**.
When every element of an array is a Double, jsaun stores the row as a `Jarr.D` (a bare
`Array[Double]`), not a `Jarr.A` of boxed `Jnum`s — so a 10×10 matrix is ten primitive arrays
with zero per-number heap objects.  `@Setup` asserts the rows really packed (a silent fallback to
`Jarr.A` would fail loudly, not just slow down).  The `prec` param picks short decimals ("4sig",
fast path) vs shortest-round-trip doubles ("full", defeats it).  `jsaunSumDbls` pulls each row's
`Array[Double]` straight back out via `.dbls` and sums it — the packed-backing payoff.

`JsaunVisitorBench` — the **SAX-style visitor** (`Json.stream`) and its **skip gates**.  The
document is an array of records, each with two small wanted fields (`id`, `name`) and bulk we don't
(a 20-element `data` array, `tags`, `active`).  Declining a key makes the parser skip its value
structurally — match brackets/quotes, decode nothing, allocate nothing — so `jsaunVisitExtract`
(pull id+name) is compared against `jsaunTreeExtract` (build the whole tree, then navigate) and
`jacksonStreamExtract` (Jackson's streaming parser with `skipChildren`, the reference's "decline
this value").  The full plane (`jsaunVisitSumAll` / `jsaunParseTree` / `jacksonStreamSumAll`) touches
every number with no skipping.  `jsaunVisitExtract{,String,Chars,Mem}` also confirms `Mem[Byte]`
drives the visitor and compares the encodings.

Run:

```
mill all.assembly                                                   # jsaun now ships in `all`
scala-cli --power run benchmarks/jsaun --jmh -- -f 2 -wi 5 -i 5 -w 1 -r 1
scala-cli --power run benchmarks/jsaun --jmh -- JsaunFormatBench    # one class
scala-cli --power run benchmarks/jsaun --jmh -- JsaunMatrixBench    # packed Jarr.D matrix
scala-cli --power run benchmarks/jsaun --jmh -- JsaunVisitorBench   # visitor + skip gates
```

scala-cli's incremental compiler leaves stale JMH-generated sources when these files change;
`rm -rf benchmarks/jsaun/.scala-build` between edits.

### Findings (2026-07-10, JDK 25, i9-14900HX, `taskset -c 2,3`, `-f 2 -wi 5 -i 5 -w 1 -r 1`)

Throughput in **ops/µs** (higher = faster; ± is JMH's 99.9% CI over 2 forks × 5 iterations).
One machine, one run, GC/JIT sharing the two pinned cores — read these as **ratios**, not
absolutes; a 1.5× gap is real, 10% is noise.

**Records — 100-object mixed array (`JsaunBench`).**

| op | jsaun | Jackson | uJson | jsoniter | uPickle |
|---|---|---|---|---|---|
| tree parse (bytes / chars) | **0.044 / 0.052** | 0.021 | 0.018 | — | — |
| tree serialize (bytes) | 0.028 | 0.033 | 0.020 | — | — |
| typed decode | 0.035 | — | — | **0.077** | 0.018 |
| typed encode (tree / direct) | 0.023 / **0.029** | — | — | **0.069** | 0.027 |

- jsaun's **dynamic-tree parse beats both dynamic-tree references** — ~2× Jackson, ~2.4× uJson —
  and reads `Array[Char]` fastest of all its sources (0.052).
- On the **typed plane jsoniter's compiled codec is the ceiling** (~2.2× jsaun on decode, ~2.4× on
  encode); jsaun's Mirror-derived codec lands between uPickle and jsoniter decoding, above uPickle
  encoding.  (Typed rows re-pinned 2026-07-13 after a codec allocation pass: decode 0.031 → 0.035
  via positional field matching, a direct args-Product instead of `Tuple.fromArray`, and packed
  `Jarr.D` backings read without per-element `Jnum.D` wrappers; encode gained `jsonizeTo` direct
  serialization — `Json.printBytes(a)` at 0.029 skips the tree and beats even tree-serialize-only
  (0.028).  The boxing that remains in decode is one Double per record plus `List[Double]`'s own
  element boxes, so a constructor macro would buy little; the residual gaps to jsoniter are its
  no-tree parse on decode and its Ryū-class float printer on encode — JDK `Double.toString` is
  the bulk of our direct-encode cost.)  Exact mode is ~4× slower than default (0.010) — the
  dyadic-exactness check on every number.

**10×10 double matrix — the packed `Jarr.D` payoff (`JsaunMatrixBench`), 4sig / full precision.**

| op | jsaun | Jackson | uJson | jsoniter† |
|---|---|---|---|---|
| parse bytes → tree | **0.82 / 0.49** | 0.27 / 0.08 | 0.27 / 0.08 | 1.10 / 0.54 |
| serialize → bytes | 0.29 / 0.18 | 0.29 / 0.22 | 0.27 / 0.24 | 0.59 / 0.43 |
| typed decode | 0.76 / 0.47 | — | — | 1.10 / 0.54 |

† jsoniter is schema-typed (`Array[Double]`), *not* a dynamic tree — shown as the ceiling for scale.

- All-Double rows pack into `Jarr.D` (a bare `Array[Double]`, no boxed `Jnum`), so jsaun's
  **dynamic parse runs 3× (4sig) to 6× (full) faster than Jackson/uJson** and reaches **~75% of
  jsoniter's schema-specialized decode** — the dynamic tree is nearly as cheap as a typed one.
- The **full-precision penalty is smallest for jsaun** (1.7×: 0.82→0.49) vs ~3.3× for Jackson/uJson
  — the Eisel–Lemire number kernel digests hard doubles.
- `jsaunSumDbls` = **11.6 ops/µs**, ~14× the parse: with the row already an `Array[Double]`, pulling
  it back out via `.dbls` and summing is almost free.  Exact mode is ~13× slower here (0.065).

**Format-preserving read-modify-write (`JsaunFormatBench`)** — parse a pretty doc, change one field,
serialize:

| | ops/µs |
|---|---|
| `jsaunFmtEdit` (preserves format) | **0.088** |
| `jsaunPlainEdit` (jsaun, reflows) | 0.044 |
| `jacksonEdit` (reflows) | 0.044 |
| `ujsonEdit` (reflows) | 0.033 |

- The headline: **format preservation is not a tax here, it's a 2× speedup.** Emitting the untouched
  document as verbatim source spans (a bulk byte-copy) and re-serializing only the single edited
  token beats fully re-rendering every token — so `jsaunFmtEdit` outruns both a plain jsaun
  parse-and-reprint and Jackson, while being the only one whose output is byte-identical except for
  the edit.

**Visitor + skip gates (`JsaunVisitorBench`)** — records with two wanted fields and skipped bulk.
(Re-pinned 2026-07-15: numbers now arrive as raw `Long`/`Double` — no `Jnum` per visited
number — with the boolean skip gates unchanged.  Same-run Jackson references match the 07-10
pins, so these are comparable.)

| op | jsaun | Jackson stream | jsaun tree |
|---|---|---|---|
| extract id+name (skip the rest) | **0.044** (chars 0.048, str 0.041, Mem 0.037) | 0.022 (`skipChildren`) | 0.024 (build+navigate) |
| full visit / parse (touch every number) | 0.030 | 0.010 | 0.024 (`jsaunParseTree`) |

- **Skipping pays.** Declining a key skips its value structurally (match brackets/quotes, decode
  nothing, allocate nothing), so `jsaunVisitExtract` (0.044) is **~1.9× building the whole tree
  then navigating** (0.024) and **~2× Jackson's streaming `skipChildren`** (0.022).  (The 07-10
  pin read 0.049 on identical walker code — build-to-build JIT variance; ratios hold.)
- The unboxed number callbacks lift the **full** visit from 0.025 to **0.030** — now **3×**
  Jackson's streaming number-sum (0.010) — since a full traversal of number-heavy data
  allocates nothing at all.
- `Mem[Byte]` **works as a visitor source** (0.037, ~16% under on-heap bytes — the FFM
  segment-access cost); `Array[Char]` is fastest (0.048), as elsewhere.

**Builder (`Jbuilder[B, A]`, in `JsaunBench`)** — a hand-written no-tree `GeoRecord` decoder.
The builder is a *stateless recipe* (here an `object`, reused across calls): `zero()` makes the
per-walk state `B`, `key`/`index` answer `Jexpect` expectations the walker type-checks with
positioned errors, numbers arrive unboxed, and `build(b)` finishes.  Value callbacks answer
`Ask[Unit]`, so semantically bad values in well-formed JSON can be refused as they arrive with
the builder's own error, positioned by the walker — accepting with the prewrapped `Is.unit`
measures free (0.038 with the check vs 0.037–0.038 without).  Same-run numbers: builder decode
**0.038** vs derived-codec decode 0.035, tree parse alone 0.046, jsoniter 0.077.

- The builder only **edges out** parse-then-convert: builder event dispatch costs nearly what
  the tree→object conversion it avoids costs, because jsaun's tree building is already cheap.
  Its value is bounded memory over streaming sources, custom target shapes, walker-checked
  positioned errors, and zero per-parse setup — not raw speed on tree-friendly payloads.
- The remaining ~2× to jsoniter is **not** the tree: it's that `key(b, k: String)` materializes
  a decoded String per key per record, where jsoniter matches field names byte-wise in place.
  An allocation-free key-matching variant (trie/intern handshake with the walker) is the next
  lever if typed-decode speed ever matters enough.
