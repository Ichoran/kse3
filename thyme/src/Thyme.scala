// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2013, 2016, 2026 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.thyme


import java.lang.{Math => jm}
import java.lang.management.ManagementFactory

import scala.compiletime.{erasedValue, error}

import kse.basics.{given, _}
import kse.maths.{given, _}
import kse.maths.fitting.{given, _}


/** Thyme is a Timing Helper You Might Enjoy: a lightweight nanobenchmarking assistant for quick,
  * in-context timing — at the REPL or embedded in a running program.
  *
  * The JIT compiler will delete computations whose results are never used, so a microbenchmark
  * must make its work matter.  The single-shot methods here run your code exactly once and hand
  * its value back to you; that returned value is what keeps the work from being optimized away, so
  * use it (or pass it somewhere the compiler cannot see through).  Repeated measurement — where a
  * value must be accumulated across many iterations to stay alive — is the job of `bench`.
  *
  * Timing comes in increasing levels of sophistication:
  *   - `clock` simply measures wall-clock time and gets out of your way.
  *   - `time` additionally watches the JVM (garbage collection, class loading) so it can flag a
  *     measurement that was probably disturbed.
  *   - `bench` repeats the work, warms it to steady state, and produces a robust statistical
  *     estimate with an honest confidence interval.
  *
  * Thyme is for quick, in-context answers, not for rigorous publication-grade numbers.  It measures
  * the steady-state cost of `f` evaluated in a tight loop on the current thread of a single, shared
  * JVM; it cannot account for call-site context, inlining decisions at a real use site, or
  * cross-JVM variation.  When `bench` cannot confirm it warmed up within its time budget it says so
  * rather than reporting a confident-looking but wrong number.  For anything you would stake a
  * decision on, reach for JMH.
  */
final class Thyme(var targetTime: Double = 50e-3, val rng: Thyme.Pcg32 = Thyme.Pcg32()):
  import Thyme.{Report, Watch, Benched, Comparison, packLong}

  /** Target fractional accuracy (standard error / mean) that `bench` tries to reach before stopping. */
  var accuracyTarget: Double = 0.03

  /** Wall-clock budget, in seconds, after which `bench` stops refining and returns its best estimate. */
  var tooMuchTime: Double = 5.0

  /** Number of timed samples `bench` collects per round. */
  var sampleSize: Int = 20

  // Anti-dead-code sink: a field array, so it escapes via `this` (a live object) and HotSpot will not
  // eliminate stores to it.  `consume` additionally reads a data-dependent slot so every value stays
  // live.  Mutated without synchronization — use one Thyme per thread.
  private val playground = new Array[Long](Thyme.PlaygroundSize)
  private var pidx = 0

  /** Stores `value` into the anti-dead-code sink, reading a slot chosen by `value` itself so that no
    * stored value (hence no benchmarked computation) can be optimized away.  Public only because
    * `bench`'s inline expansion calls it; it is rarely useful directly.
    */
  def consume(value: Long): Unit =
    val i = pidx
    playground(i) = value ^ playground((value & Thyme.PlaygroundMask).toInt)
    pidx = (i + 1) & Thyme.PlaygroundMask

  /** A conservative estimate (seconds) of the timing error intrinsic to one `nanoTime`-bracketed
    * measurement on this machine.  `bench` folds it into its confidence interval so it never claims
    * precision the clock cannot deliver.  Computed once, when this Thyme is created.
    */
  val tick: Double = Thyme.calibrateTick()


  // ---- clock: bare wall-clock time -------------------------------------------------------------

  /** Times a single evaluation of `f` and returns the elapsed wall-clock time in seconds.  The
    * value of `f` is discarded; if `f` is pure the JIT may delete it, so this is for quick,
    * order-of-magnitude checks.  Use `clockPair` when you need the value (and its protection).
    */
  inline def clock[A](inline f: A): Double =
    val t0 = System.nanoTime
    val _ = f
    val t1 = System.nanoTime
    (t1 - t0) * 1e-9

  /** Times a single evaluation of `f`, returning both its value and the elapsed seconds. */
  inline def clockPair[A](inline f: A): (A, Double) =
    val t0 = System.nanoTime
    val a = f
    val t1 = System.nanoTime
    (a, (t1 - t0) * 1e-9)

  /** Times a single evaluation of `f`, prints the elapsed time, and returns the value of `f`. */
  inline def pclock[A](inline f: A): A =
    val t0 = System.nanoTime
    val a = f
    val t1 = System.nanoTime
    println("Wall clock: " + Thyme.humanTime((t1 - t0) * 1e-9))
    a


  // ---- time: JVM-aware single run --------------------------------------------------------------

  /** Times a single evaluation of `f`, watching the JVM for garbage collection and class loading
    * so that a disturbed measurement can be flagged.  Returns the value and a [[Thyme.Report]].
    */
  inline def timePair[A](inline f: A): (A, Report) =
    val w = Watch.start()
    val a = f
    (a, w.stop(1))

  /** Times a single evaluation of `f` with JVM watching, prints the report, and returns the value. */
  inline def ptime[A](inline f: A): A =
    val w = Watch.start()
    val a = f
    println(w.stop(1).toString)
    a


  // ---- bench: warmed, repeated, robust microbenchmark ------------------------------------------

  /** Benchmarks `f`: sizes a repetition count, warms the code to steady state, and returns a robust
    * estimate of the time per single evaluation with a confidence interval.  See [[Thyme.Benched]].
    */
  inline def bench[A](inline f: A): Benched =
    benchRunner: reps =>
      var i = 0
      while i < reps do
        consume(packLong(f))
        i += 1

  /** Benchmarks `f`, prints the report, and returns it. */
  inline def pbench[A](inline f: A): Benched =
    val b = bench(f)
    println(b.toString)
    b

  /** Benchmarks `first` against `second` head-to-head, *in the conditions you are running in right
    * now*, and reports which is faster and by how much.  Rather than timing each alone, it runs
    * scrambled mixtures of the two in varying proportions and fits time against the mixture ratio:
    * the slope is the per-call cost difference, and because both run interleaved in the same window,
    * any drift in the environment affects both equally and cancels out of the contrast.  This is the
    * question Thyme answers best — and one that is awkward even for heavier tools.  See [[Thyme.Comparison]].
    */
  inline def benchOff[A](inline first: A)(inline second: A): Comparison =
    benchOffRunner: (who, numBlocks, blockSize) =>
      var b = 0
      while b < numBlocks do
        if who(b) == 0 then
          var k = 0
          while k < blockSize do { consume(packLong(first)); k += 1 }
        else
          var k = 0
          while k < blockSize do { consume(packLong(second)); k += 1 }
        b += 1

  /** Runs [[benchOff]], prints the report, and returns it. */
  inline def pbenchOff[A](inline first: A)(inline second: A): Comparison =
    val c = benchOff(first)(second)
    println(c.toString)
    c

  /** The non-inline benchmarking engine.  `run(reps)` must evaluate the benchmarked code `reps`
    * times, feeding each result through [[consume]].  Public because `bench`'s inline expansion calls
    * it; a caller who wants to build a custom runner (and handle dead-code protection themselves) may
    * use it directly.
    */
  // One timed sample: run the work `reps` times bracketed by a JVM-status watch.
  private def sample(run: Int => Unit, reps: Int): Report =
    val w = Watch.start()
    run(reps)
    w.stop(reps)

  def benchRunner(run: Int => Unit): Benched =
    val tStart = System.nanoTime
    inline def elapsedS = (System.nanoTime - tStart) * 1e-9
    val ss = jm.max(8, sampleSize)
    val window = jm.max(ss, 24)
    val perSampleTarget = jm.max(targetTime / ss, tick * Thyme.MinTickRatio)

    // Phase 1: size `reps` so one sample reaches `perSampleTarget`.  No low cap: a tight loop must be
    // allowed to grow large enough that the JIT compiles it (via on-stack replacement) to C2 code.
    var reps = 1
    var dt = sample(run, reps).elapsed
    while dt < perSampleTarget && reps < Thyme.RepsCapHigh && elapsedS < tooMuchTime do
      val factor = if dt <= 0.0 then 8.0 else jm.min(16.0, jm.max(2.0, perSampleTarget / dt))
      val next = jm.min(Thyme.RepsCapHigh.toLong, jm.max((reps + 1).toLong, (reps * factor).toLong))
      reps = next.toInt
      dt = sample(run, reps).elapsed

    // Phase 2: collect samples until the timing series is warm and steady (or the budget runs out).
    var times = new Array[Double](64)
    var dirty = new Array[Boolean](64)
    var n = 0
    var totalCalls = 0L
    var gcTot = 0L
    var clTot = 0L
    var compiledEver = false
    var converged = false
    var stop = false
    while !stop do
      val r = sample(run, reps)
      if n >= times.length then
        times = java.util.Arrays.copyOf(times, times.length * 2)
        dirty = java.util.Arrays.copyOf(dirty, dirty.length * 2)
      times(n) = r.elapsed / reps
      dirty(n) = r.suspect
      n += 1
      totalCalls += reps
      gcTot += r.gcCount
      clTot += r.classLoads
      if r.compileTime > 0 then compiledEver = true
      if n >= window && totalCalls >= Thyme.MinWarmCalls && windowClean(dirty, n, window) && steady(times, n, window) then
        converged = true
        stop = true
      else if elapsedS >= tooMuchTime || n >= Thyme.MaxSamples then
        stop = true

    // Final estimate from the trailing (warm) window.
    val w0 = jm.max(0, n - window)
    val est = robustEstimate(times, w0, n, reps)
    new Benched(est.mean, est.error, est.lo, est.hi, est.used, totalCalls, elapsedS, reps, tick, converged, gcTot, clTot, compiledEver)

  // True if none of the last `window` samples (ending at index `n`) was disturbed by GC, class
  // loading, or JIT compilation.
  private def windowClean(dirty: Array[Boolean], n: Int, window: Int): Boolean =
    var i = n - window
    var ok = true
    while ok && i < n do
      if dirty(i) then ok = false
      i += 1
    ok

  // True if the trailing window shows no sign the code is still speeding up (warming): the Theil-Sen
  // trend is not significantly negative, and the second half is not significantly faster than the first.
  private def steady(times: Array[Double], n: Int, window: Int): Boolean =
    val w0 = n - window
    val xs = new Array[Double](window)
    val ys = new Array[Double](window)
    var i = 0
    while i < window do
      xs(i) = i.toDouble
      ys(i) = times(w0 + i)
      i += 1
    val fit = TheilSen.fit(xs, ys)
    val notTrendingDown = !(fit.slopeUpper < 0.0)   // CI on the slope reaches 0 or above
    // Half-split Welch test: is the later half significantly faster (smaller) than the earlier half?
    val h = window / 2
    val e1, e2 = Est.M()
    i = 0
    while i < h do { e1 += ys(i); i += 1 }
    while i < window do { e2 += ys(i); i += 1 }
    val se = (e1.semSq + e2.semSq).sqrt
    val halvesAgree =
      if !(se > 0.0) then true
      else
        val t = (e2.mean - e1.mean) / se
        val df = jm.max(1L, (jm.min(e1.n, e2.n) - 1).toLong)
        NumericFunctions.cdfStudentT(df, t) >= 0.05   // not significantly below the earlier half
    notTrendingDown && halvesAgree

  // Robust per-call estimate over times[i0, iN): drop outliers with a Tukey fence, take the mean and
  // standard error of the inliers, and form a 95% CI whose error is floored by the timer resolution.
  private def robustEstimate(times: Array[Double], i0: Int, iN: Int, reps: Int): Thyme.Estimate =
    val s = Quantile.finiteSorted(times, i0, iN)
    if s.length == 0 then return Thyme.Estimate(Double.NaN, Double.NaN, Double.NaN, Double.NaN, 0)
    val q1 = Quantile.ofSorted(s, 0, s.length)(0.25)
    val q3 = Quantile.ofSorted(s, 0, s.length)(0.75)
    val fence = 1.5 * (q3 - q1)
    val lo = q1 - fence
    val hi = q3 + fence
    val e = Est.M()
    var i = 0
    while i < s.length do
      val x = s(i)
      if x >= lo && x <= hi then e += x
      i += 1
    val used = e.n.toInt
    val mean = e.mean
    val sem = e.sem
    val floor = tick / jm.max(1, reps)   // timer error spread over the repetitions in a sample
    val error = ((if sem.finite then sem * sem else 0.0) + floor * floor).sqrt
    val (clo, chi) =
      if used >= 2 && error.finite then
        val t = NumericFunctions.icdfStudentT((used - 1).toLong, 0.975)
        (mean - t * error, mean + t * error)
      else (Double.NaN, Double.NaN)
    Thyme.Estimate(mean, error, clo, chi, used)


  // ---- benchOff: in-context head-to-head ------------------------------------------------------

  // One timed mixture: run the prepared who block-pattern bracketed by a JVM-status watch.  `who`
  // has one entry per block; each block runs `blockSize` calls of one side, so the dispatch branch
  // fires once per block instead of once per call.
  private def sampleMix(run: (Array[Byte], Int, Int) => Unit, who: Array[Byte], numBlocks: Int, blockSize: Int): Report =
    val w = Watch.start()
    run(who, numBlocks, blockSize)
    w.stop(numBlocks.toLong * blockSize)

  // Fill who[0, count) with `nf` zeros (first) and the rest ones (second), then scramble in place, so
  // first- and second-blocks are interleaved in a random order rather than run as two big runs.
  private def buildWho(who: Array[Byte], nf: Int, count: Int): Unit =
    var i = 0
    while i < count do
      who(i) = if i < nf then 0 else 1
      i += 1
    var j = count - 1
    while j > 0 do
      val k = rng.roll(j + 1)
      val t = who(j); who(j) = who(k); who(k) = t
      j -= 1

  // Number of adjacent first/second block transitions: the regressor for the order-of-evaluation check.
  private def countSwaps(who: Array[Byte], count: Int): Double =
    var c = 0
    var l = 1
    while l < count do
      if who(l) != who(l - 1) then c += 1
      l += 1
    c.toDouble

  def benchOffRunner(run: (Array[Byte], Int, Int) => Unit): Comparison =
    val tStart = System.nanoTime
    inline def elapsedS = (System.nanoTime - tStart) * 1e-9
    val ss = jm.max(8, sampleSize)
    val window = jm.max(ss, 24)
    val blockSize = Thyme.OffBlock
    val perSampleTarget = jm.max(targetTime / ss, tick * Thyme.MinTickRatio)

    // Phase 1: size the number of blocks so a balanced (half-and-half) mixture reaches the target time.
    var numBlocks = 2
    var who = new Array[Byte](numBlocks)
    buildWho(who, numBlocks / 2, numBlocks)
    var dt = sampleMix(run, who, numBlocks, blockSize).elapsed
    while dt < perSampleTarget && numBlocks < Thyme.OffBlocksCap && elapsedS < tooMuchTime do
      val factor = if dt <= 0.0 then 8.0 else jm.min(16.0, jm.max(2.0, perSampleTarget / dt))
      val grown = jm.min(Thyme.OffBlocksCap.toLong, jm.max((numBlocks + 2).toLong, (numBlocks * factor).toLong)).toInt
      numBlocks = grown & ~1   // keep it even so a balanced split is exact
      who = new Array[Byte](numBlocks)
      buildWho(who, numBlocks / 2, numBlocks)
      dt = sampleMix(run, who, numBlocks, blockSize).elapsed
    val reps = numBlocks.toLong * blockSize   // total calls per mixture

    // Phase 2: warm both code blocks together with balanced mixtures, until the series is steady.
    val wt = new Array[Double](jm.max(window, 1) * 4)
    val wd = new Array[Boolean](wt.length)
    var wn = 0
    var totalCalls = 0L
    var gcTot = 0L
    var clTot = 0L
    var compiled = false
    var warm = false
    while !warm && elapsedS < tooMuchTime do
      buildWho(who, numBlocks / 2, numBlocks)
      val r = sampleMix(run, who, numBlocks, blockSize)
      if wn >= wt.length then { System.arraycopy(wt, window, wt, 0, wt.length - window); wn -= window }   // slide
      wt(wn) = r.elapsed / reps
      wd(wn) = r.suspect
      wn += 1
      totalCalls += reps
      gcTot += r.gcCount; clTot += r.classLoads
      if r.compileTime > 0 then compiled = true
      if wn >= window && totalCalls >= Thyme.MinWarmCalls && windowClean(wd, wn, window) && steady(wt, wn, window) then warm = true

    // Phase 3: collect mixtures spanning the full ratio, executed in random order so any residual
    // drift is uncorrelated with the mixture weight, then fit time against weight.
    var weights = new Array[Double](ss * 4)
    var times = new Array[Double](ss * 4)
    var swaps = new Array[Double](ss * 4)
    var n = 0
    val order = new Array[Int](ss)
    var pass = 0
    var converged = false
    var stop = false
    while !stop do
      var i = 0
      while i < ss do { order(i) = i; i += 1 }
      i = ss - 1
      while i > 0 do { val k = rng.roll(i + 1); val t = order(i); order(i) = order(k); order(k) = t; i -= 1 }
      var oi = 0
      while oi < ss do
        val w = -1.0 + 2.0 * order(oi) / (ss - 1)
        val nf = jm.min(numBlocks, jm.max(0, jm.round(numBlocks * (1.0 - w) / 2.0).toInt))
        buildWho(who, nf, numBlocks)
        val r = sampleMix(run, who, numBlocks, blockSize)
        if !r.suspect then
          if n >= weights.length then
            weights = java.util.Arrays.copyOf(weights, weights.length * 2)
            times = java.util.Arrays.copyOf(times, times.length * 2)
            swaps = java.util.Arrays.copyOf(swaps, swaps.length * 2)
          weights(n) = (numBlocks - 2.0 * nf) / numBlocks   // actual achieved weight after rounding
          times(n) = r.elapsed / reps
          swaps(n) = countSwaps(who, numBlocks)
          n += 1
        totalCalls += reps
        gcTot += r.gcCount; clTot += r.classLoads
        if r.compileTime > 0 then compiled = true
        oi += 1
      pass += 1
      val fit = TheilSen.fit(weights, times, 0, n)
      val meanCost = fit.x2y(0.0)
      val diffSem = 2.0 * fit.slopeSem
      val precise = diffSem.finite && meanCost > 0 && diffSem < accuracyTarget * meanCost
      if pass >= 2 && (precise || elapsedS >= tooMuchTime || n >= Thyme.MaxSamples) then
        converged = precise
        stop = true
      else if elapsedS >= tooMuchTime || n >= Thyme.MaxSamples then
        stop = true

    // Build the report from the final fit.
    val fit = TheilSen.fit(weights, times, 0, n)
    val costF = fit.x2y(-1.0)
    val costH = fit.x2y(1.0)
    val diff = costH - costF
    val diffLo = 2.0 * fit.slopeLower
    val diffHi = 2.0 * fit.slopeUpper
    // Statistically distinguishable: the CI on the difference excludes zero.
    val significant = (diffLo > 0 && diffHi > 0) || (diffLo < 0 && diffHi < 0)
    // Practically distinguishable: the difference is larger than the precision we aimed for.  Two
    // separately-inlined copies of even identical code differ slightly (layout, caches), so we do not
    // crown a winner for a difference below the resolution target — that would be crying wolf.
    val meanCost = fit.x2y(0.0)
    val practicallyDifferent = meanCost > 0 && jm.abs(diff) > accuracyTarget * meanCost
    val winner = if significant && practicallyDifferent then (if diff > 0 then -1 else 1) else 0
    val ratio = if costF != 0 then costH / costF else Double.NaN
    // Order-of-evaluation check: does the number of first/second swaps explain residual time?  Only
    // a swap effect that is both statistically real and large enough to matter (its span across the
    // observed swap range exceeds the resolution target) is worth warning about.
    val resid = new Array[Double](n)
    var i = 0
    var swapLo = Double.PositiveInfinity
    var swapHi = Double.NegativeInfinity
    while i < n do
      resid(i) = times(i) - fit.x2y(weights(i))
      if swaps(i) < swapLo then swapLo = swaps(i)
      if swaps(i) > swapHi then swapHi = swaps(i)
      i += 1
    val swapFit = TheilSen.fit(swaps, resid, 0, n)
    val swapSignificant = (swapFit.slopeLower > 0 && swapFit.slopeUpper > 0) || (swapFit.slopeLower < 0 && swapFit.slopeUpper < 0)
    val swapImpact = jm.abs(swapFit.slope) * (swapHi - swapLo)
    val historyEffect = swapSignificant && meanCost > 0 && swapImpact > accuracyTarget * meanCost
    new Comparison(winner, costF, costH, diff, diffLo, diffHi, ratio, significant, historyEffect, n, totalCalls, elapsedS, converged, gcTot, clTot, compiled)


object Thyme:
  /** Size of the anti-dead-code sink used by `bench`; a power of two so the index is a cheap mask. */
  private inline val PlaygroundSize = 256
  private inline val PlaygroundMask = PlaygroundSize - 1

  // Each bench sample is sized to at least this many timer-error ticks, so timer quantization error
  // is a small fraction of the sample.
  private inline val MinTickRatio = 1024.0
  // Safety ceiling on the repetition count (well above any sane value); prevents overflow/runaway.
  private inline val RepsCapHigh = 1 << 28
  // benchOff runs each who[] entry as a block of this many calls, so the first/second dispatch branch
  // fires once per block instead of once per call — keeping dispatch overhead and branch-misprediction
  // noise out of the measured cost.
  private inline val OffBlock = 16
  // Ceiling on the number of blocks per mixture: bounds who[] while staying far above the on-stack-
  // replacement threshold (block count is the outer loop, so it is what triggers OSR for fast code).
  private inline val OffBlocksCap = 1 << 20
  // Minimum number of evaluations before bench will believe the code is warm (JIT to C2 takes ~this many).
  private inline val MinWarmCalls = 200000L
  // Hard ceiling on samples collected, as a backstop against pathological non-convergence.
  private inline val MaxSamples = 200000

  // Result of robustEstimate: central time, combined error, CI bounds, and number of inliers used.
  private[thyme] final case class Estimate(mean: Double, error: Double, lo: Double, hi: Double, used: Int)

  /** Conservative per-measurement timing error (seconds): a high, outlier-trimmed gap between
    * consecutive `System.nanoTime` reads.  Several warmed trials are run (the first are discarded so
    * the calibration loop itself is JIT-compiled), and the largest trimmed value is taken.  Falls
    * back to 1 ms when the clock is too coarse to be useful.
    */
  private def calibrateTick(): Double =
    val deltas = new Array[Double](16384)
    var worst = 0.0
    var trial = 0
    while trial < 12 do
      var last = System.nanoTime
      var i = 0
      while i < deltas.length do
        val t = System.nanoTime
        deltas(i) = (t - last).toDouble * 1e-9
        last = t
        i += 1
      // Largest gap that is not a gross outlier (above a 3·IQR Tukey fence): the worst normal jitter.
      val s = Quantile.finiteSorted(deltas, 0, deltas.length)
      val q1 = Quantile.ofSorted(s, 0, s.length)(0.25)
      val q3 = Quantile.ofSorted(s, 0, s.length)(0.75)
      val fence = q3 + 3.0 * (q3 - q1)
      var k = s.length - 1
      while k > 0 && s(k) > fence do k -= 1
      val tk = s(k)
      if trial >= 6 && tk > worst then worst = tk   // first 6 trials warm the loop; discard them
      trial += 1
    if !worst.finite || worst <= 0.0 then 1e-9
    else if worst > 1e-3 then 1e-3
    else worst

  /** Packs the result of a timed block into a `Long` at compile time, without boxing primitives, so
    * the value can be woven into `bench`'s sink.  `Unit` is rejected to force a real result.  This
    * is low-level machinery exposed for `bench`'s inline expansion; most users never call it.
    */
  transparent inline def packLong[A](inline a: A): Long =
    inline erasedValue[A] match
      case _: Long    => a.asInstanceOf[Long]
      case _: Int     => a.asInstanceOf[Int].toLong
      case _: Double  => java.lang.Double.doubleToRawLongBits(a.asInstanceOf[Double])
      case _: Float   => java.lang.Float.floatToRawIntBits(a.asInstanceOf[Float]).toLong
      case _: Boolean => if a.asInstanceOf[Boolean] then 1L else 0L
      case _: Char    => a.asInstanceOf[Char].toLong
      case _: Byte    => a.asInstanceOf[Byte].toLong
      case _: Short   => a.asInstanceOf[Short].toLong
      case _: Unit    => error("Timed code must return a value that depends on the work (so the JIT cannot delete it); a Unit result cannot be protected.")
      case _          => a.##.toLong

  /** A compact, significant-figure rendering of a duration in seconds, choosing s / ms / µs / ns. */
  def humanTime(seconds: Double): String =
    if !seconds.finite then seconds.toString
    else
      val a = jm.abs(seconds)
      val (scale, unit) =
        if a >= 1.0   then (1e0,  "s")
        else if a >= 1e-3 then (1e3, "ms")
        else if a >= 1e-6 then (1e6, "µs")
        else                   (1e9, "ns")
      f"${seconds * scale}%.4g $unit"


  // ---- JVM status watching (advisory) ----------------------------------------------------------

  // Shared management beans.  Reads are cheap relative to the work being timed.
  private val gcBeans = ManagementFactory.getGarbageCollectorMXBeans
  private val classBean = ManagementFactory.getClassLoadingMXBean
  private val compileBean = ManagementFactory.getCompilationMXBean   // null if the JVM has no JIT

  private def gcCount(): Long =
    var c = 0L
    var i = 0
    while i < gcBeans.size do
      val n = gcBeans.get(i).getCollectionCount
      if n > 0 then c += n
      i += 1
    c

  private def gcMillis(): Long =
    var t = 0L
    var i = 0
    while i < gcBeans.size do
      val m = gcBeans.get(i).getCollectionTime
      if m > 0 then t += m
      i += 1
    t

  // Cumulative JIT compilation time (ms), or 0 if the JVM cannot report it.  Used to detect that
  // the just-measured sample was disturbed by background compilation (so the code isn't warm yet).
  private def compileMillis(): Long =
    if compileBean != null && compileBean.isCompilationTimeMonitoringSupported then compileBean.getTotalCompilationTime
    else 0L

  /** A before-snapshot of JVM state.  Created by [[Watch.start]]; closed by [[Watch.stop]] to give
    * a [[Report]].  Beans are read first and `System.nanoTime` last, so bean-reading overhead is
    * excluded from the measured interval.
    */
  final class Watch private (
    private val gc0: Long, private val gt0: Long, private val cl0: Long, private val cp0: Long, private val t0: Long
  ):
    /** Closes the watch over `reps` represented iterations and produces a report. */
    def stop(reps: Long): Report =
      val t1 = System.nanoTime
      val gc1 = gcCount()
      val gt1 = gcMillis()
      val cl1 = classBean.getTotalLoadedClassCount.toLong
      val cp1 = compileMillis()
      new Report((t1 - t0) * 1e-9, gc1 - gc0, (gt1 - gt0) * 1e-3, cl1 - cl0, (cp1 - cp0) * 1e-3, jm.max(1L, reps))
  object Watch:
    /** Opens a watch: reads the JVM beans, then captures the start time last. */
    def start(): Watch =
      val gc = gcCount()
      val gt = gcMillis()
      val cl = classBean.getTotalLoadedClassCount.toLong
      val cp = compileMillis()
      new Watch(gc, gt, cl, cp, System.nanoTime)


  /** A report on a single JVM-aware timing.  In the advisory model, garbage collection and class
    * loading are reported and used to mark the timing `suspect`, but the elapsed time is the raw
    * wall-clock interval — it is not "corrected" by subtracting GC time.
    */
  final class Report(
    /** Raw wall-clock elapsed time, in seconds. */
    val elapsed: Double,
    /** Number of garbage-collection sweeps observed during the interval. */
    val gcCount: Long,
    /** Time spent in garbage collection during the interval, in seconds. */
    val gcTime: Double,
    /** Number of classes loaded during the interval. */
    val classLoads: Long,
    /** Time the JIT spent compiling during the interval, in seconds (0 if unsupported). */
    val compileTime: Double,
    /** Number of iterations the interval represents (1 for a single timing). */
    val effort: Long
  ):
    /** Elapsed time attributed to each represented iteration. */
    def perOp: Double = elapsed / jm.max(1L, effort)

    /** True if GC, class loading, or JIT compilation happened, so the timing is probably disturbed. */
    def suspect: Boolean = gcCount > 0 || classLoads > 0 || compileTime > 0

    override def toString =
      val sb = new StringBuilder
      sb ++= "Elapsed: " + humanTime(elapsed)
      if effort > 1 then sb ++= s"  ($effort iterations, ${humanTime(perOp)} each)"
      if suspect then sb ++= "  [suspect:"
      if gcCount > 0 then sb ++= s" ${gcCount} GC sweep${if gcCount == 1 then "" else "s"} (${humanTime(gcTime)})"
      if classLoads > 0 then sb ++= s" ${classLoads} classes loaded"
      if compileTime > 0 then sb ++= s" JIT compiled (${humanTime(compileTime)})"
      if suspect then sb ++= "]"
      sb.result()


  /** The result of a `bench` run: a robust estimate of the time per single evaluation, with a 95%
    * confidence interval.  If `converged` is false, `bench` could not confirm the code reached
    * steady state within its time budget, so the estimate should be treated with suspicion.
    */
  final class Benched(
    /** Best estimate of the time for one evaluation, in seconds. */
    val time: Double,
    /** Combined standard error of `time` (sampling error and timer resolution), in seconds. */
    val error: Double,
    /** Lower bound of the 95% confidence interval on `time`. */
    val lo: Double,
    /** Upper bound of the 95% confidence interval on `time`. */
    val hi: Double,
    /** Number of (outlier-trimmed) samples the estimate is based on. */
    val samples: Int,
    /** Total number of times the benchmarked code was evaluated. */
    val totalCalls: Long,
    /** Total wall-clock time spent benchmarking, in seconds. */
    val wallClock: Double,
    /** Repetitions per sample at the end of the run. */
    val reps: Int,
    /** Timer-resolution floor folded into the error, in seconds. */
    val tick: Double,
    /** True if the code was confirmed warm and steady before the estimate was taken. */
    val converged: Boolean,
    /** Garbage-collection sweeps seen across the whole run. */
    val gcCount: Long,
    /** Classes loaded across the whole run. */
    val classLoads: Long,
    /** Whether the JIT compiled at any point during the run. */
    val compiled: Boolean
  ):
    /** Relative standard error of the estimate (error / time). */
    def relativeError: Double = error / time

    override def toString =
      val sb = new StringBuilder
      sb ++= s"Benchmark: ${humanTime(time)} each   95% CI ${humanTime(lo)} – ${humanTime(hi)}"
      sb ++= s"   (n=$samples, $totalCalls calls in ${humanTime(wallClock)})"
      if !converged then
        sb ++= "\n  [NOT CONVERGED: could not confirm warmup within the time budget — raise tooMuchTime, or use JMH]"
      if gcCount > 0 then sb ++= s"\n  note: $gcCount GC sweep${if gcCount == 1 then "" else "s"} during the run"
      sb.result()


  /** The result of a `benchOff`: a head-to-head comparison of two code blocks, run interleaved so
    * the contrast is robust to drift.  `winner` is -1 if the first block is faster, +1 if the second
    * is faster, and 0 if they could not be told apart.  Per-call costs are read off the fitted line
    * at the all-first and all-second ends; `difference` (second − first) carries the confidence
    * interval that decides significance.
    */
  final class Comparison(
    /** -1 if the first block is faster, +1 if the second is faster, 0 if indistinguishable. */
    val winner: Int,
    /** Estimated time per call of the first block, in seconds. */
    val costFirst: Double,
    /** Estimated time per call of the second block, in seconds. */
    val costSecond: Double,
    /** Estimated per-call time difference (second − first), in seconds. */
    val difference: Double,
    /** Lower bound of the 95% CI on `difference`. */
    val differenceLo: Double,
    /** Upper bound of the 95% CI on `difference`. */
    val differenceHi: Double,
    /** Ratio of second to first per-call time. */
    val ratio: Double,
    /** True if the difference is statistically significant (its CI excludes zero). */
    val significant: Boolean,
    /** True if the order in which the two are interleaved measurably affects timing. */
    val historyEffect: Boolean,
    /** Number of mixtures the fit is based on. */
    val samples: Int,
    /** Total number of evaluations across both blocks. */
    val totalCalls: Long,
    /** Total wall-clock time spent, in seconds. */
    val wallClock: Double,
    /** True if the comparison reached its target precision before the time budget. */
    val converged: Boolean,
    /** Garbage-collection sweeps seen across the whole run. */
    val gcCount: Long,
    /** Classes loaded across the whole run. */
    val classLoads: Long,
    /** Whether the JIT compiled at any point during the run. */
    val compiled: Boolean
  ):
    override def toString =
      val sb = new StringBuilder
      val verdict =
        if winner != 0 then
          val (faster, slower, fast, slow) =
            if winner < 0 then ("First", "Second", costFirst, costSecond)
            else                ("Second", "First", costSecond, costFirst)
          val x = if fast > 0 then slow / fast else Double.NaN
          f"$faster is faster: ${x}%.3g× ($slower − $faster = ${humanTime(jm.abs(difference))} per call)"
        else if significant then
          s"indistinguishable in practice — a real but tiny ${humanTime(jm.abs(difference))} difference, " +
            "below the resolution target (likely code layout or caches, not the algorithm)"
        else "First and Second are indistinguishable"
      sb ++= s"Head-to-head: $verdict"
      sb ++= s"\n  First:  ${humanTime(costFirst)} each     Second: ${humanTime(costSecond)} each"
      sb ++= s"\n  Difference (Second − First): ${humanTime(difference)}   95% CI [${humanTime(differenceLo)}, ${humanTime(differenceHi)}]"
      sb ++= s"\n  (n=$samples mixtures, $totalCalls calls in ${humanTime(wallClock)})"
      if historyEffect then
        sb ++= "\n  [warning: order of evaluation affects timing — the two interact through shared state, caches, or branch prediction]"
      if !converged then
        sb ++= "\n  [NOT CONVERGED: target precision not reached within the time budget — raise tooMuchTime]"
      sb.result()


  // ---- PCG32 random number generator -----------------------------------------------------------

  /** A PCG generator, specifically `pcg32` = `pcg_setseq_64_xsh_rr_32` from the PCG family of
    * Melissa E. O'Neill ("PCG: A Family of Simple Fast Space-Efficient Statistically Good Algorithms
    * for Random Number Generation", Harvey Mudd College tech report HMC-CS-2014-0905, 2014;
    * https://www.pcg-random.org).  Concretely: a 64-bit linear congruential base (multiplier
    * 6364136223846793005, a selectable per-instance odd increment a.k.a. "stream") whose output is
    * the XSH-RR permutation — xorshift the high bits down, then rotate right by a random amount —
    * yielding 32 bits per step.  The seeding constants are PCG's canonical `PCG32_INITIALIZER`.
    *
    * NOTE ON ATTRIBUTION: "XSH-RR" is PCG nomenclature and is NOT the xoshiro/xoroshiro family of
    * Blackman & Vigna (whose scramblers are named `+`, `++`, `**`); the abbreviations look similar
    * but the algorithms are unrelated.  Recording the exact variant here matters: fast RNGs have
    * repeatedly been found to have structural weaknesses, and a fix or recall only applies to the
    * specific algorithm it was found in.  This is `pcg32` XSH-RR with 64-bit state.
    *
    * Kept as a lightweight alternative to the maths module's 64-bit generators for benchmark inner
    * loops, where only cheap `Int`-sized draws are needed.  Not cryptographically secure.
    */
  final class Pcg32 private ():
    import Pcg32.{Mult, DefaultSeed, DefaultStream}

    // Both are overwritten by `seed`, which every constructor in the companion calls.
    private var state: Long = 0L
    private var inc: Long = 0L

    /** Reseeds the generator from a starting value and a stream-selecting sequence. */
    def seed(start: Long, sequence: Long): this.type =
      state = 0L
      inc = (sequence << 1) | 1L
      nextInt() __ Unit
      state += start
      nextInt() __ Unit
      this

    /** The next pseudo-random 32-bit value. */
    def nextInt(): Int =
      val s = state
      state = s * Mult + inc
      val xorshifted = (((s >>> 18) ^ s) >>> 27).toInt
      val rot = (s >>> 59).toInt
      (xorshifted >>> rot) | (xorshifted << ((-rot) & 31))

    /** A pseudo-random value in `[0, bound)`, using rejection sampling to avoid modulo bias. */
    def roll(bound: Int): Int =
      if bound <= 0 then 0
      else
        val threshold = Integer.remainderUnsigned(-bound, bound)
        var x = nextInt()
        while Integer.compareUnsigned(x, threshold) < 0 do x = nextInt()
        Integer.remainderUnsigned(x, bound)
  object Pcg32:
    private inline val Mult          = 0x5851F42D4C957F2DL   // PCG LCG multiplier
    private inline val DefaultSeed   = 0x853C49E6748FEA9BL   // PCG default initial state
    private inline val DefaultStream = 0xDA3E39CB94B95BDBL   // PCG default stream selector

    /** A generator seeded from the system clock (distinct each call). */
    def apply(): Pcg32 = new Pcg32().seed(DefaultSeed, System.nanoTime)

    /** A generator with a chosen seed and the default stream. */
    def apply(seed: Long): Pcg32 = new Pcg32().seed(seed, DefaultStream)

    /** A generator with a chosen seed and stream. */
    def apply(seed: Long, sequence: Long): Pcg32 = new Pcg32().seed(seed, sequence)
