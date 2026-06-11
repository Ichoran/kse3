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
