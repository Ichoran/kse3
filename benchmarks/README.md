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

Reading the results: `spsc` vs `abqSpsc` says how close a *blocking, select-capable, lifecycle-
managed* channel gets to a bare blocking queue. `selectN` vs `abqSelectN` tests the design's
thesis (registering handlers once instead of rebuilding selection per iteration) — note the
control *spins* (busy poll) to reach its throughput, while `selectN` blocks, so equal throughput
already favors the blocking design on CPU. `fanIn` is the case most sensitive to the single
per-channel lock.

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
