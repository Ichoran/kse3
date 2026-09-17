# kse3 thyme

The thyme module is measurement: how long a piece of code takes.  `Thyme` is a nanobenchmarking helper for the REPL
or a scratch main.  It times a block once, or warms it up and repeats it until the estimate is stable, and it can
race two blocks head to head in interleaved mixtures so that whatever else the machine is doing cancels out of the
comparison.  `Parsley` is the in-program counterpart: it lives in a companion object, times call sites of the
running program with its real inputs, races two implementations at a site, and reports per site and per regime
when the program ends.  Neither replaces JMH for a number you'd publish; both answer the question you have now, in
the conditions you're in now.

`import kse.thyme.{given, *}` brings in both.  They lean on `kse.basics` and `kse.maths` (the statistics and the
random generator underneath).

<!-- guide examples: thyme/test/src/GuideExamples.scala -->
Every code example below is compiled and run from `thyme/test/src/GuideExamples.scala`; `mill guides.run` verifies
that the two copies are identical.

## What's here

- **Thyme**: `clock`, `time`, `bench`, and `benchOff` on a block; values come back so the JIT can't delete the work; reports carry confidence intervals.
- **Parsley**: `time{}` and `timeOff(a, b){}{}` at a call site inside a running program; per-site, per-regime medians and quantiles, reported at close or JVM exit.

## Thyme

`Thyme()` makes a timer; use one per thread.  `clock{ f }` is wall-clock seconds for one evaluation, with the value
discarded, so it is for order-of-magnitude checks; `clockPair` gives the value back too.  `timePair{ f }` also
watches the JVM and answers a `Report` saying whether garbage collection, class loading, or JIT compilation
happened during the measurement.  `bench{ f }` sizes a repetition count, warms the code to steady state, trims
outliers, and answers a `Benched`: `time` per evaluation, `lo` and `hi` of a 95% confidence interval, and
`converged`, which is false when steady state couldn't be confirmed within the time budget.  `benchOff{ a }{ b }`
runs scrambled mixtures of the two and fits time against the mixture ratio, so the slope is the per-call
difference and drift moves both alike; the `Comparison` says `winner` (-1 for the first, 1 for the second, 0 for
indistinguishable), `ratio`, and whether the difference is `significant`.  The `p` forms (`pclock`, `ptime`,
`pbench`, `pbenchOff`) print the report as well.  The one rule of microbenchmarking: work whose result the JIT can
see is unused gets deleted, so the single-shot forms hand the value back for you to use, and `bench` feeds every
result through a sink of its own.

**Reach for this when** you'd write `System.nanoTime` before and after a loop, or argue about which of two
implementations is faster.

Plain Scala:

<!-- guide: bench.plain -->
```scala
def howLongPlain(n: Int): Double =
  val t0 = System.nanoTime
  var i = 0
  while i < 1000 do { work(n): Unit; i += 1 }        // cold, and the JIT may drop the unused result
  (System.nanoTime - t0) * 1e-9 / 1000
```

kse3:

<!-- guide: bench.kse3 -->
```scala
val th = Thyme()                                     // one per thread; targetTime, tooMuchTime, accuracyTarget are its knobs
def howLong(n: Int): Thyme.Benched =
  th.bench(work(n))                                  // warmed to steady state, outlier-trimmed, with a 95% CI, in seconds
```

<!-- guide: benchoff.kse3 -->
```scala
def whichIsFaster(n: Int): Thyme.Comparison =
  th.benchOff(work(n))(work(2 * n))                  // interleaved in mixtures, so drift cancels; winner is -1, 0, or 1
```

<!-- guide: time.kse3 -->
```scala
def once(n: Int): (Long, Thyme.Report) =
  th.timePair(work(n))                               // one run: the value, and a report that flags GC, class loads, and JIT
```

`targetTime` is the wall time per sample, `accuracyTarget` the relative error `bench` stops at, and `tooMuchTime`
the budget after which it reports what it has; `tick`, measured when the `Thyme` is made, is the clock's own
resolution, folded into every interval so no claimed precision exceeds it.  What is measured is the steady-state
cost of a tight loop on one thread of a shared JVM: call-site context and inlining at a real use are out of reach,
and a `Comparison` with `historyEffect` set means the two blocks interact through caches or branch prediction.
For a number you'd stake a decision on, reach for JMH; `benchmarks/` holds the project's own.

Full API: `thyme/src/Thyme.scala`; `Report`, `Benched`, and `Comparison` in `object Thyme` document every field.

## Parsley

`Parsley()` in a companion object is a profiler for the program you're already running.  `parsley.time{ f }` times
one evaluation, records it under the call site (file and line, through `SourceLine`), and returns the value.
`parsley.timeOff("old", "new"){ a }{ b }` runs both in random order, records the pair, and returns whichever ran
first; with `mode = "pick"` it runs exactly one.  Timings at a site are summarized by median and quantiles per
regime: the stream is split where its behaviour changes (warmup, steady state, a load spike), and for a pair the
split is on the ratio of the two, so a shift that moves both alike is not a regime.  `results` is the snapshot,
`Parsley.formatReport` the text, and the report goes to `onClose` (print, by default) at `close()` or at JVM
shutdown, backstopped so it is not lost when nobody remembers to close.

**Reach for this when** you'd sprinkle `nanoTime` and log lines through production code to find the slow branch,
or need to compare implementations on real inputs rather than a synthetic loop.

<!-- guide: parsley.kse3 -->
```scala
object Prof { val parsley = Parsley() }              // one per program, in a companion; it reports at close or at JVM exit
def lookup(key: Long): Int =
  Prof.parsley.timeOff("linear", "hashed"){ linear(key) }{ hashed(key) }   // both run, in random order; one value returns
def load(n: Int): Long =
  Prof.parsley.time{ work(n) }                       // one call site, one track; regimes (warmup, steady) split by themselves
```

The `both` mode of `timeOff` needs the two blocks to be interchangeable and free of side effects, since both run;
`pick` is for when only one may.  `Parsley.onClose(sink)` routes the report to a function and `Parsley(_ => ())`
keeps it quiet; the regime-detection thresholds are constructor parameters.

Full API: `thyme/src/Parsley.scala`; the per-regime statistics are `kse.maths.MultiRadwin`.
