// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and Calico Life Sciences LLC.

package kse.maths


import java.lang.{Math => jm}
import java.util.Arrays

import kse.basics.{given, *}


/////////////////////////////////////////////////////////////////////////
/// UDDSketch: a uniform-collapse, relative-error quantile sketch        ///
/// over non-negative data, with a dense core + sparse tails.            ///
/////////////////////////////////////////////////////////////////////////

/** A quantile sketch for non-negative data with a *relative* error guarantee.
  *
  * This is a UDDSketch (Epicoco et al. 2020, "Uniform DDSketch") with three
  * deliberate departures that suit it to embedded latency profiling:
  *
  *  - '''Dense core + sparse tails.'''  Values map to logarithmic buckets
  *    `i = ⌊log_γ x⌋` (`γ = (1+α)/(1-α)`); the contiguous central mass lives in a
  *    dense `Array[Long]` of [[maxBuckets]] slots, while scattered outlying buckets
  *    live in a small sparse store of up to [[sparseBuckets]] entries.  A handful of
  *    huge (often spurious) values therefore occupy their own full-resolution buckets
  *    *without* widening the core's span — so they don't force a collapse that would
  *    coarsen the resolution of the bulk.
  *
  *  - '''Per-bucket sums.'''  Each bucket carries the exact sum of its members, so a
  *    bucket's point estimate is the true mean and [[quantile]] interpolates between
  *    adjacent bucket means.  [[quantileStrict]] returns the geometric bucket centre,
  *    carrying the clean worst-case `≤ α` relative-error guarantee.
  *
  *  - '''Exact moments.'''  `n`, [[mean]], and [[sd]] are tracked exactly via an
  *    [[Est]] accumulator, independent of bucket resolution.
  *
  * The sketch is a pure bag of bucket counts split across two containers, so
  * [[add]] / [[subtract]] / [[mergeWith]] are all exact at bucket resolution: removal
  * is structurally identical to insertion.  (Observed [[min]]/[[max]] are
  * insert-accurate only and may read wide after subtraction.)
  *
  * When the populated span will not fit, a '''uniform collapse''' merges every
  * adjacent bucket pair (`i → ⌊i/2⌋`), squaring `γ` and raising [[alpha]].
  *
  * Bucketing uses one `log2` per point (the JVM's is faster than any polynomial we
  * could substitute); construct with [[UDDSketch.apply]].  Only finite `x >= 0` may be
  * added; `NaN` is ignored, negatives and infinities throw.
  */
final class UDDSketch private (val alpha0: Double, val maxBuckets: Int, val sparseBuckets: Int) {
  private var gamma: Double = (1 + alpha0) / (1 - alpha0)
  private var invLog2Gamma: Double = 1.0 / gamma.log2     // i = ⌊log2(x) · (1/log2 γ)⌋
  private var nCollapse: Int = 0

  // Dense core: bucket index `base + k` is stored at slot `k`.
  private val dense = new Array[Long](maxBuckets)
  private val denseSum = new Array[Double](maxBuckets)
  private var base = 0
  private var started = false

  // Sparse tails: parallel arrays sorted ascending by index, with one slot of overflow slack.
  private val sIdx = new Array[Int](sparseBuckets + 1)
  private val sCnt = new Array[Long](sparseBuckets + 1)
  private val sSum = new Array[Double](sparseBuckets + 1)
  private var sLen = 0

  private var zeroCount = 0L
  private var total = 0L
  private val acc = new Est.M(0, 0, 0)
  private var loSeen = Double.PositiveInfinity
  private var hiSeen = Double.NegativeInfinity


  //////////////////////////
  /// Public observations ///
  //////////////////////////

  /** Total number of values represented (including exact zeros). */
  def count: Long = total

  /** Current worst-case relative error of a quantile estimate; grows as collapses coarsen `γ`. */
  def alpha: Double = (gamma - 1) / (gamma + 1)

  /** Number of uniform collapses performed so far (0 if the data fit at full resolution). */
  def collapses: Int = nCollapse

  /** Exact mean of all added values (independent of bucket resolution). */
  def mean: Double = acc.mean

  /** Exact standard deviation of all added values. */
  def sd: Double = acc.sd

  /** A snapshot of the exact moment accumulator (mean/sd/sem). */
  def moments: Est = acc.snapshot

  /** Smallest value observed.  Insert-accurate; may read low after [[subtract]]. */
  def min: Double = if zeroCount > 0 then 0.0 else loSeen

  /** Largest value observed.  Insert-accurate; may read high after [[subtract]]. */
  def max: Double = if total <= 0 then Double.NaN else hiSeen


  ////////////////////
  /// Construction ///
  ////////////////////

  /** Bucket index of `x > 0`: `⌊log_γ x⌋`. */
  private def bucketOf(x: Double): Int = jm.floor(x.log2 * invLog2Gamma).toInt

  /** Add a single non-negative value.  `NaN` is ignored; negatives/infinities throw. */
  def +=(x: Double): Unit = add(x, 1L)

  /** Add `c` copies of a non-negative value.  `NaN` is ignored; negatives/infinities throw. */
  def add(x: Double, c: Long): Unit =
    if x != x || c <= 0 then return
    if x < 0 || x == Double.PositiveInfinity then
      throw new IllegalArgumentException(s"UDDSketch accepts only finite x >= 0, not $x")
    acc.addWithWeight(c.toDouble)(x)
    total += c
    if x < loSeen then loSeen = x
    if x > hiSeen then hiSeen = x
    if x == 0.0 then zeroCount += c
    else putBucket(bucketOf(x), c, x * c)

  private def putBucket(i: Int, c: Long, s: Double): Unit =
    if !started then
      base = i - maxBuckets / 2
      started = true
    val off = i - base
    if off >= 0 && off < maxBuckets then
      dense(off) += c
      denseSum(off) += s
    else
      sparsePut(i, c, s)
      if sLen > sparseBuckets then rebalance()

  private def sparsePut(i: Int, c: Long, s: Double): Unit =
    var lo = 0
    var hi = sLen
    while lo < hi do
      val mid = (lo + hi) >>> 1
      if sIdx(mid) < i then lo = mid + 1 else hi = mid
    if lo < sLen && sIdx(lo) == i then
      sCnt(lo) += c
      sSum(lo) += s
    else
      var j = sLen
      while j > lo do
        sIdx(j) = sIdx(j - 1); sCnt(j) = sCnt(j - 1); sSum(j) = sSum(j - 1)
        j -= 1
      sIdx(lo) = i; sCnt(lo) = c; sSum(lo) = s
      sLen += 1


  ////////////////
  /// Removal  ///
  ////////////////

  /** Remove a single previously-added value (exact at bucket resolution). */
  def -=(x: Double): Unit = subtract(x, 1L)

  /** Remove `c` copies of a previously-added value.  Counts are clamped at zero. */
  def subtract(x: Double, c: Long): Unit =
    if x != x || c <= 0 then return
    if x < 0 || x == Double.PositiveInfinity then
      throw new IllegalArgumentException(s"UDDSketch accepts only finite x >= 0, not $x")
    acc.incorporate(-c.toDouble, x, 0.0)
    total -= c
    if total < 0 then total = 0
    if x == 0.0 then
      zeroCount -= c
      if zeroCount < 0 then zeroCount = 0
    else removeBucket(bucketOf(x), c, x * c)

  private def removeBucket(i: Int, c: Long, s: Double): Unit =
    val off = i - base
    if started && off >= 0 && off < maxBuckets then
      dense(off) -= c
      if dense(off) <= 0 then { dense(off) = 0; denseSum(off) = 0.0 }
      else denseSum(off) -= s
    else
      var lo = 0
      var hi = sLen
      while lo < hi do
        val mid = (lo + hi) >>> 1
        if sIdx(mid) < i then lo = mid + 1 else hi = mid
      if lo < sLen && sIdx(lo) == i then
        sCnt(lo) -= c
        if sCnt(lo) <= 0 then
          var j = lo
          while j < sLen - 1 do
            sIdx(j) = sIdx(j + 1); sCnt(j) = sCnt(j + 1); sSum(j) = sSum(j + 1)
            j += 1
          sLen -= 1
        else sSum(lo) -= s


  /////////////////////////
  /// Collapse / balance ///
  /////////////////////////

  /** Gather every populated bucket (zeros excluded) in ascending index order. */
  private def gather(): (Array[Int], Array[Long], Array[Double], Int) =
    var nz = sLen
    var off = 0
    while off < maxBuckets do
      if dense(off) > 0 then nz += 1
      off += 1
    val gi = new Array[Int](nz)
    val gc = new Array[Long](nz)
    val gs = new Array[Double](nz)
    var p = 0
    var w = 0
    while p < sLen && sIdx(p) < base do
      gi(w) = sIdx(p); gc(w) = sCnt(p); gs(w) = sSum(p); w += 1; p += 1
    off = 0
    while off < maxBuckets do
      if dense(off) > 0 then
        gi(w) = base + off; gc(w) = dense(off); gs(w) = denseSum(off); w += 1
      off += 1
    while p < sLen do
      gi(w) = sIdx(p); gc(w) = sCnt(p); gs(w) = sSum(p); w += 1; p += 1
    (gi, gc, gs, w)

  /** Collapse ascending bucket arrays in place by one uniform step (`i → ⌊i/2⌋`); returns new length. */
  private def collapseRuns(gi: Array[Int], gc: Array[Long], gs: Array[Double], n: Int): Int =
    var r = 0
    var w = -1
    while r < n do
      val j = gi(r) >> 1
      if w >= 0 && gi(w) == j then
        gc(w) += gc(r); gs(w) += gs(r)
      else
        w += 1
        gi(w) = j; gc(w) = gc(r); gs(w) = gs(r)
      r += 1
    w + 1

  private def bumpCollapse(): Unit =
    gamma = gamma * gamma
    invLog2Gamma *= 0.5      // log2(γ²) = 2·log2 γ
    nCollapse += 1

  /** Pick the best dense window over ascending entries and spill the rest to sparse,
    * collapsing uniformly until everything fits in `maxBuckets + sparseBuckets`.
    */
  private def rebalance(): Unit =
    var (gi, gc, gs, n) = gather()
    if n == 0 then { sLen = 0; clearDense(); return }
    var placed = false
    while !placed do
      val span = gi(n - 1) - gi(0) + 1
      if span <= maxBuckets then
        base = gi(0)
        redistribute(gi, gc, gs, n, 0, n)
        placed = true
      else
        var bestStart = 0
        var bestEnd = 0
        var bestCovered = -1L
        var lo = 0
        var covered = 0L
        var hi = 0
        while lo < n do
          while hi < n && gi(hi) - gi(lo) < maxBuckets do
            covered += gc(hi); hi += 1
          if covered > bestCovered then
            bestCovered = covered; bestStart = lo; bestEnd = hi
          covered -= gc(lo)
          lo += 1
        val outside = n - (bestEnd - bestStart)
        if outside <= sparseBuckets then
          base = gi(bestStart)
          redistribute(gi, gc, gs, n, bestStart, bestEnd)
          placed = true
        else
          n = collapseRuns(gi, gc, gs, n)
          bumpCollapse()

  private def clearDense(): Unit =
    Arrays.fill(dense, 0L)
    Arrays.fill(denseSum, 0.0)

  /** Write entries `[winStart, winEnd)` to the dense core (already positioned at `base`) and the rest to sparse. */
  private def redistribute(gi: Array[Int], gc: Array[Long], gs: Array[Double], n: Int, winStart: Int, winEnd: Int): Unit =
    clearDense()
    sLen = 0
    var k = 0
    while k < n do
      if k >= winStart && k < winEnd then
        val off = gi(k) - base
        dense(off) = gc(k); denseSum(off) = gs(k)
      else
        sIdx(sLen) = gi(k); sCnt(sLen) = gc(k); sSum(sLen) = gs(k); sLen += 1
      k += 1
    started = true


  ///////////////
  /// Merging ///
  ///////////////

  /** Merge another sketch into this one.  Both must share the same base accuracy [[alpha0]].
    * The finer sketch is uniformly collapsed to the coarser one's resolution first.
    */
  def mergeWith(that: UDDSketch): Unit =
    if that.alpha0 != alpha0 then
      throw new IllegalArgumentException(s"Cannot merge UDDSketches with different base accuracy: $alpha0 vs ${that.alpha0}")
    while nCollapse < that.nCollapse do
      val (gi, gc, gs, n) = gather()
      val m = collapseRuns(gi, gc, gs, n)
      bumpCollapse()
      redistributeFresh(gi, gc, gs, m)
    val dThat = nCollapse - that.nCollapse     // collapse `that` down to our (coarser) grid as we read it
    val (ti, tc, ts, tn) = that.gather()
    var k = 0
    while k < tn do
      putBucket(ti(k) >> dThat, tc(k), ts(k))
      k += 1
    zeroCount += that.zeroCount
    total += that.total          // putBucket doesn't touch total, so add the whole of `that` here
    acc += that.acc
    if that.loSeen < loSeen then loSeen = that.loSeen
    if that.hiSeen > hiSeen then hiSeen = that.hiSeen

  private def redistributeFresh(gi: Array[Int], gc: Array[Long], gs: Array[Double], n: Int): Unit =
    if n == 0 then { clearDense(); sLen = 0; return }
    val span = gi(n - 1) - gi(0) + 1
    if span <= maxBuckets then
      base = gi(0)
      redistribute(gi, gc, gs, n, 0, n)
    else
      clearDense(); sLen = 0
      var k = 0
      while k < n do { putBucket(gi(k), gc(k), gs(k)); k += 1 }
      if sLen > sparseBuckets then rebalance()


  /////////////////
  /// Quantiles ///
  /////////////////

  private inline def clampUnit(q: Double): Double = if q < 0 then 0.0 else if q > 1 then 1.0 else q

  /** Iterate populated buckets (zeros first) ascending, with each bucket's exact mean value and count. */
  private inline def foreachValue(inline f: (Double, Long) => Unit): Unit =
    if zeroCount > 0 then f(0.0, zeroCount)
    var p = 0
    while p < sLen && sIdx(p) < base do { f(sSum(p) / sCnt(p), sCnt(p)); p += 1 }
    var off = 0
    while off < maxBuckets do
      if dense(off) > 0 then f(denseSum(off) / dense(off), dense(off))
      off += 1
    while p < sLen do { f(sSum(p) / sCnt(p), sCnt(p)); p += 1 }

  /** Interpolated quantile `q` (mean-anchored): exact at the extremes, smoothly
    * interpolated between adjacent bucket means in between.  `q` is clamped to `[0,1]`.
    */
  def quantile(q: Double): Double =
    if total <= 0 then return Double.NaN
    if total == 1 then return hiSeen
    val r = clampUnit(q) * (total - 1)
    val nb = bucketEntries()
    val rs = new Array[Double](nb + 2)
    val vs = new Array[Double](nb + 2)
    rs(0) = 0.0; vs(0) = min
    var w = 1
    var cum = 0L
    foreachValue: (v, c) =>
      rs(w) = cum + (c - 1) / 2.0
      vs(w) = v
      w += 1
      cum += c
    rs(w) = (total - 1).toDouble; vs(w) = max
    w += 1
    var j = 0
    while j < w - 1 && rs(j + 1) < r do j += 1
    val r0 = rs(j); val r1 = rs(j + 1)
    if r1 <= r0 then vs(j)
    else
      val f = (r - r0) / (r1 - r0)
      vs(j) + f * (vs(j + 1) - vs(j))

  /** Strict quantile `q`: the geometric centre of the bucket holding rank `q`, carrying the
    * worst-case relative-error guarantee ([[alpha]]).  `q` is clamped to `[0,1]`.
    */
  def quantileStrict(q: Double): Double =
    if total <= 0 then return Double.NaN
    val target = jm.floor(clampUnit(q) * (total - 1)).toLong
    var cum = 0L
    var ans = Double.NaN
    var done = false
    if zeroCount > 0 then
      cum += zeroCount
      if target < cum then { ans = 0.0; done = true }
    inline def consider(i: Int, c: Long): Unit =
      if !done then
        cum += c
        if target < cum then { ans = jm.pow(gamma, i + 0.5); done = true }
    var p = 0
    while p < sLen && sIdx(p) < base do { consider(sIdx(p), sCnt(p)); p += 1 }
    var off = 0
    while off < maxBuckets do
      if dense(off) > 0 then consider(base + off, dense(off))
      off += 1
    while p < sLen do { consider(sIdx(p), sCnt(p)); p += 1 }
    if done then ans else max

  /** Median (interpolated). */
  def median: Double = quantile(0.5)

  /** Interquartile range Q3 − Q1 (interpolated). */
  def iqr: Double = quantile(0.75) - quantile(0.25)

  /** Approximate fraction of values `<= x` (the empirical CDF at `x`). */
  def fractionBelow(x: Double): Double =
    if total <= 0 then return Double.NaN
    if x < 0 then return 0.0
    var below = 0.0
    if x == 0.0 then below = zeroCount.toDouble * 0.5
    else
      if zeroCount > 0 then below += zeroCount.toDouble
      val ix = bucketOf(x)
      foreachValueIndexed: (i, _, c) =>
        if i < ix then below += c.toDouble
        else if i == ix then below += c.toDouble * 0.5
    below / total

  private inline def foreachValueIndexed(inline f: (Int, Double, Long) => Unit): Unit =
    var p = 0
    while p < sLen && sIdx(p) < base do { f(sIdx(p), sSum(p) / sCnt(p), sCnt(p)); p += 1 }
    var off = 0
    while off < maxBuckets do
      if dense(off) > 0 then f(base + off, denseSum(off) / dense(off), dense(off))
      off += 1
    while p < sLen do { f(sIdx(p), sSum(p) / sCnt(p), sCnt(p)); p += 1 }

  private def bucketEntries(): Int =
    var nz = sLen + (if zeroCount > 0 then 1 else 0)
    var off = 0
    while off < maxBuckets do
      if dense(off) > 0 then nz += 1
      off += 1
    nz


  ////////////////
  /// Plumbing ///
  ////////////////

  /** An independent deep copy. */
  def copy: UDDSketch =
    val s = new UDDSketch(alpha0, maxBuckets, sparseBuckets)
    s.gamma = gamma; s.invLog2Gamma = invLog2Gamma; s.nCollapse = nCollapse
    System.arraycopy(dense, 0, s.dense, 0, maxBuckets)
    System.arraycopy(denseSum, 0, s.denseSum, 0, maxBuckets)
    s.base = base; s.started = started
    System.arraycopy(sIdx, 0, s.sIdx, 0, sLen)
    System.arraycopy(sCnt, 0, s.sCnt, 0, sLen)
    System.arraycopy(sSum, 0, s.sSum, 0, sLen)
    s.sLen = sLen
    s.zeroCount = zeroCount; s.total = total
    s.acc.n = acc.n; s.acc.mean = acc.mean; s.acc.sse = acc.sse
    s.loSeen = loSeen; s.hiSeen = hiSeen
    s

  /** Remove all data, returning to the initial resolution. */
  def clear(): Unit =
    gamma = (1 + alpha0) / (1 - alpha0); invLog2Gamma = 1.0 / gamma.log2; nCollapse = 0
    clearDense(); base = 0; started = false; sLen = 0
    zeroCount = 0; total = 0; acc.reset()
    loSeen = Double.PositiveInfinity; hiSeen = Double.NegativeInfinity

  override def toString =
    if total <= 0 then s"UDDSketch(empty, α=$alpha0)"
    else f"UDDSketch(n=$total, α=$alpha%.4g${if nCollapse > 0 then s", $nCollapse collapses" else ""}, median≈${quantile(0.5)}%.4g)"
}
object UDDSketch {
  /** A sketch with base relative accuracy `alpha` (e.g. `0.01` for ~1% buckets).
    *
    * @param alpha         target relative error in `(0, 1)`; smaller is finer (and may collapse sooner)
    * @param maxBuckets    dense core size — the span, in buckets, kept at full resolution
    * @param sparseBuckets outlying buckets kept verbatim before a collapse is forced
    */
  def apply(alpha: Double = 0.01, maxBuckets: Int = 2048, sparseBuckets: Int = 64): UDDSketch =
    if !(alpha > 0 && alpha < 1) then
      throw new IllegalArgumentException(s"UDDSketch relative accuracy must be in (0, 1), not $alpha")
    if maxBuckets < 2 then
      throw new IllegalArgumentException(s"UDDSketch needs at least 2 dense buckets, not $maxBuckets")
    if sparseBuckets < 1 then
      throw new IllegalArgumentException(s"UDDSketch needs at least 1 sparse bucket, not $sparseBuckets")
    new UDDSketch(alpha, maxBuckets, sparseBuckets)
}
