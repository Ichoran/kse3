// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.maths


import java.lang.{Math => jm}
import java.util.Arrays

import scala.collection.mutable.ArrayBuffer

import kse.basics.{given, *}


/////////////////////////////////////////////////////////////////////////
/// UDDSketch: a uniform-collapse, relative-error quantile sketch        ///
/// over non-negative data, with a dense core + sparse tails.            ///
/////////////////////////////////////////////////////////////////////////

/** A quantile sketch for real-valued data with a *relative* error guarantee.
  *
  * This is a UDDSketch (Epicoco et al. 2020, "Uniform DDSketch") with four
  * deliberate departures that suit it to embedded latency profiling:
  *
  *  - '''Dense core + sparse tails.'''  Magnitudes map to logarithmic buckets
  *    `i = ⌊log_γ|x|⌋` (`γ = (1+α)/(1-α)`); the contiguous central mass lives in a
  *    dense `Array[Long]` of [[maxBuckets]] slots, while scattered outlying buckets
  *    live in a small sparse store of up to [[sparseBuckets]] entries.  A handful of
  *    huge (often spurious) values therefore occupy their own full-resolution buckets
  *    *without* widening the core's span — so they don't force a collapse that would
  *    coarsen the resolution of the bulk.
  *
  *  - '''Adaptive sign support.'''  Exact zeros are always counted separately.  Data is
  *    treated as non-negative ('''unsigned mode''') until enough distinct negative buckets
  *    accrue to overflow a small buffer; then the sketch flips to '''signed mode''', where the
  *    dense core is split symmetrically (half for negative magnitudes, half for positive),
  *    each side carries its own large-magnitude sparse tail, and the centre band the symmetric
  *    axis cannot split — `0 < |x| < γ^m` — is aggregated into per-sign "tiny" bins.  All-positive
  *    workloads never leave unsigned mode, so they keep full relative resolution down to zero.
  *
  *  - '''Per-bucket sums.'''  Each bucket carries the exact sum of its members' magnitudes, so a
  *    bucket's point estimate is the true mean and [[quantile]] interpolates between
  *    adjacent bucket means.  [[quantileStrict]] returns the (signed) geometric bucket centre,
  *    carrying the clean worst-case `≤ α` relative-error guarantee.
  *
  *  - '''Exact moments.'''  `n`, [[mean]], and [[sd]] are tracked exactly via an
  *    [[Est]] accumulator, independent of bucket resolution.
  *
  * The sketch is a pure bag of bucket counts, so [[add]] / [[subtract]] / [[mergeWith]] are
  * exact at bucket resolution: removal is structurally identical to insertion.  (Observed
  * [[min]]/[[max]] are insert-accurate only and may read wide after subtraction.)
  *
  * When the populated span will not fit, a '''uniform collapse''' merges every
  * adjacent bucket pair (`i → ⌊i/2⌋`), squaring `γ` and raising [[alpha]].
  *
  * Bucketing uses one `log2` per point (the JVM's is faster than any polynomial we
  * could substitute); construct with [[UDDSketch.apply]].  Any finite `x` may be added;
  * `NaN` is ignored, infinities throw.
  */
final class UDDSketch private (val alpha0: Double, val maxBuckets: Int, val sparseBuckets: Int) {
  private var gamma: Double = (1 + alpha0) / (1 - alpha0)
  private var invLog2Gamma: Double = 1.0 / gamma.log2     // i = ⌊log2(x) · (1/log2 γ)⌋
  private var nCollapse: Int = 0

  // Dense core.  Unsigned mode: abs-bucket `base + k` at slot `k`, spanning all `maxBuckets`.
  // Signed mode: value-ordered, slot 0 = most negative … slot maxBuckets-1 = most positive; the
  // negative half is `[0, half)`, the positive half `[half, maxBuckets)`, and `base` is the band
  // edge abs-bucket `m` shared by both signs (see `posSlot`/`negSlot`).  `denseSum` holds magnitudes.
  private val dense = new Array[Long](maxBuckets)
  private val denseSum = new Array[Double](maxBuckets)
  private var base = 0
  private var started = false
  private val half = maxBuckets / 2

  // Positive sparse tail.  Unsigned mode: every out-of-window positive bucket (low and high).
  // Signed mode: the positive large-magnitude tail only.
  private val sIdx = new Array[Int](sparseBuckets + 1)
  private val sCnt = new Array[Long](sparseBuckets + 1)
  private val sSum = new Array[Double](sparseBuckets + 1)
  private var sLen = 0

  // Negative store.  Unsigned mode: the small negative buffer (by abs-bucket) whose overflow flips
  // the sketch to signed mode.  Signed mode: the negative large-magnitude sparse tail.
  private val nIdx = new Array[Int](sparseBuckets + 1)
  private val nCnt = new Array[Long](sparseBuckets + 1)
  private val nSum = new Array[Double](sparseBuckets + 1)
  private var nLen = 0

  private var signed = false
  // Signed-mode centre band (|x| below the dense window), aggregated per sign.
  private var tinyPosCnt = 0L; private var tinyPosSum = 0.0
  private var tinyNegCnt = 0L; private var tinyNegSum = 0.0

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

  /** True once negative data has forced the symmetric signed representation. */
  def isSigned: Boolean = signed

  /** Exact mean of all added values (independent of bucket resolution). */
  def mean: Double = acc.mean

  /** Exact standard deviation of all added values. */
  def sd: Double = acc.sd

  /** A snapshot of the exact moment accumulator (mean/sd/sem). */
  def moments: Est = acc.snapshot

  /** Smallest value observed.  Insert-accurate; may read low after [[subtract]]. */
  def min: Double = if total <= 0 then Double.NaN else loSeen

  /** Largest value observed.  Insert-accurate; may read high after [[subtract]]. */
  def max: Double = if total <= 0 then Double.NaN else hiSeen


  ////////////////////
  /// Construction ///
  ////////////////////

  /** Abs-bucket index of `|x| > 0`: `⌊log_γ|x|⌋`. */
  private def bucketOf(ax: Double): Int = jm.floor(ax.log2 * invLog2Gamma).toInt

  /** Add a single value.  `NaN` is ignored; infinities throw. */
  def +=(x: Double): Unit = add(x, 1L)

  /** Add `c` copies of a value.  `NaN` is ignored; infinities throw. */
  def add(x: Double, c: Long): Unit =
    if x != x || c <= 0 then return
    if x == Double.PositiveInfinity || x == Double.NegativeInfinity then
      throw new IllegalArgumentException(s"UDDSketch accepts only finite values, not $x")
    acc.addWithWeight(c.toDouble)(x)
    total += c
    if x < loSeen then loSeen = x
    if x > hiSeen then hiSeen = x
    if x == 0.0 then zeroCount += c
    else place(x, c)

  /** Route a finite, nonzero value into the bucket structure (no moment/extreme/total bookkeeping). */
  private def place(x: Double, c: Long): Unit =
    val ax = jm.abs(x)
    val b = bucketOf(ax)
    val s = ax * c
    if signed then putSigned(x > 0, b, c, s)
    else if x > 0 then putBucket(b, c, s)
    else
      nLen = sIns(nIdx, nCnt, nSum, nLen, b, c, s)
      if nLen > sparseBuckets then flip()

  // --- Unsigned positive store (also the whole store before any flip) ---

  private def putBucket(i: Int, c: Long, s: Double): Unit =
    if !started then
      base = i - maxBuckets / 2
      started = true
    val off = i - base
    if off >= 0 && off < maxBuckets then
      dense(off) += c
      denseSum(off) += s
    else
      sLen = sIns(sIdx, sCnt, sSum, sLen, i, c, s)
      if sLen > sparseBuckets then rebalance()

  // --- Generic sorted sparse helpers (positive tail, negative buffer/tail) ---

  private def sIns(idx: Array[Int], cnt: Array[Long], sum: Array[Double], len: Int, i: Int, c: Long, s: Double): Int =
    var lo = 0
    var hi = len
    while lo < hi do
      val mid = (lo + hi) >>> 1
      if idx(mid) < i then lo = mid + 1 else hi = mid
    if lo < len && idx(lo) == i then
      cnt(lo) += c; sum(lo) += s; len
    else
      var j = len
      while j > lo do
        idx(j) = idx(j - 1); cnt(j) = cnt(j - 1); sum(j) = sum(j - 1)
        j -= 1
      idx(lo) = i; cnt(lo) = c; sum(lo) = s
      len + 1

  private def sDel(idx: Array[Int], cnt: Array[Long], sum: Array[Double], len: Int, i: Int, c: Long, s: Double): Int =
    var lo = 0
    var hi = len
    while lo < hi do
      val mid = (lo + hi) >>> 1
      if idx(mid) < i then lo = mid + 1 else hi = mid
    if lo < len && idx(lo) == i then
      cnt(lo) -= c
      if cnt(lo) <= 0 then
        var j = lo
        while j < len - 1 do
          idx(j) = idx(j + 1); cnt(j) = cnt(j + 1); sum(j) = sum(j + 1)
          j += 1
        len - 1
      else { sum(lo) -= s; len }
    else len


  // --- Signed store ---

  /** Dense slot for a positive magnitude bucket `b` inside the window `[base, base+half)`. */
  private inline def posSlot(b: Int): Int = half + (b - base)

  /** Dense slot for a negative magnitude bucket `b` inside the window `[base, base+half)`. */
  private inline def negSlot(b: Int): Int = (half - 1) - (b - base)

  private def putSigned(positive: Boolean, b: Int, c: Long, s: Double): Unit =
    val off = b - base
    if off >= 0 && off < half then
      val slot = if positive then half + off else (half - 1) - off
      dense(slot) += c; denseSum(slot) += s
    else if b < base then
      if positive then { tinyPosCnt += c; tinyPosSum += s }
      else { tinyNegCnt += c; tinyNegSum += s }
    else if positive then
      sLen = sIns(sIdx, sCnt, sSum, sLen, b, c, s)
      if sLen > sparseBuckets then rebalanceSigned()
    else
      nLen = sIns(nIdx, nCnt, nSum, nLen, b, c, s)
      if nLen > sparseBuckets then rebalanceSigned()


  ////////////////
  /// Removal  ///
  ////////////////

  /** Remove a single previously-added value (exact at bucket resolution). */
  def -=(x: Double): Unit = subtract(x, 1L)

  /** Remove `c` copies of a previously-added value.  Counts are clamped at zero. */
  def subtract(x: Double, c: Long): Unit =
    if x != x || c <= 0 then return
    if x == Double.PositiveInfinity || x == Double.NegativeInfinity then
      throw new IllegalArgumentException(s"UDDSketch accepts only finite values, not $x")
    acc.incorporate(-c.toDouble, x, 0.0)
    total -= c
    if total < 0 then total = 0
    if x == 0.0 then
      zeroCount -= c
      if zeroCount < 0 then zeroCount = 0
    else
      val ax = jm.abs(x)
      val b = bucketOf(ax)
      val s = ax * c
      if signed then removeSigned(x > 0, b, c, s)
      else if x > 0 then removeBucket(b, c, s)
      else nLen = sDel(nIdx, nCnt, nSum, nLen, b, c, s)

  private def removeBucket(i: Int, c: Long, s: Double): Unit =
    val off = i - base
    if started && off >= 0 && off < maxBuckets then
      dense(off) -= c
      if dense(off) <= 0 then { dense(off) = 0; denseSum(off) = 0.0 }
      else denseSum(off) -= s
    else sLen = sDel(sIdx, sCnt, sSum, sLen, i, c, s)

  private def removeSigned(positive: Boolean, b: Int, c: Long, s: Double): Unit =
    val off = b - base
    if off >= 0 && off < half then
      val slot = if positive then half + off else (half - 1) - off
      dense(slot) -= c
      if dense(slot) <= 0 then { dense(slot) = 0; denseSum(slot) = 0.0 }
      else denseSum(slot) -= s
    else if b < base then
      if positive then
        tinyPosCnt -= c; if tinyPosCnt <= 0 then { tinyPosCnt = 0; tinyPosSum = 0.0 } else tinyPosSum -= s
      else
        tinyNegCnt -= c; if tinyNegCnt <= 0 then { tinyNegCnt = 0; tinyNegSum = 0.0 } else tinyNegSum -= s
    else if positive then sLen = sDel(sIdx, sCnt, sSum, sLen, b, c, s)
    else nLen = sDel(nIdx, nCnt, nSum, nLen, b, c, s)


  /////////////////////////
  /// Collapse / balance ///
  /////////////////////////

  /** Gather every populated positive bucket (zeros excluded) in ascending index order. */
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

  // --- Signed mode: flip and symmetric rebalance ---

  /** Switch from unsigned mode to signed mode, redistributing the positive store and negative buffer
    * into the symmetric (value-ordered) dense core with two large-magnitude tails and tiny bands. */
  private def flip(): Unit =
    val (pi, pc, ps, pn) = gather()
    val ni = new Array[Int](nLen);    System.arraycopy(nIdx, 0, ni, 0, nLen)
    val nc = new Array[Long](nLen);   System.arraycopy(nCnt, 0, nc, 0, nLen)
    val ns = new Array[Double](nLen); System.arraycopy(nSum, 0, ns, 0, nLen)
    val nn = nLen
    clearDense(); sLen = 0; nLen = 0; base = 0; started = false
    signed = true
    placeSigned(pi, pc, ps, pn, ni, nc, ns, nn)

  /** Re-pack the current signed store after a sparse-tail overflow. */
  private def rebalanceSigned(): Unit =
    val (pi, pc, ps, pn) = gatherSigned(positive = true)
    val (ni, nc, ns, nn) = gatherSigned(positive = false)
    val tp = tinyPosCnt; val tps = tinyPosSum
    val tn = tinyNegCnt; val tns = tinyNegSum
    clearDense(); sLen = 0; nLen = 0
    tinyPosCnt = 0; tinyPosSum = 0.0; tinyNegCnt = 0; tinyNegSum = 0.0
    placeSigned(pi, pc, ps, pn, ni, nc, ns, nn)
    tinyPosCnt += tp; tinyPosSum += tps          // already-aggregated tiny mass stays tiny
    tinyNegCnt += tn; tinyNegSum += tns

  /** Gather one sign's populated abs-buckets (dense half + large tail), ascending. */
  private def gatherSigned(positive: Boolean): (Array[Int], Array[Long], Array[Double], Int) =
    val (idx, cnt, sum, len) = if positive then (sIdx, sCnt, sSum, sLen) else (nIdx, nCnt, nSum, nLen)
    var nz = len
    var slot = 0
    while slot < half do
      val ds = if positive then half + slot else (half - 1) - slot
      if dense(ds) > 0 then nz += 1
      slot += 1
    val gi = new Array[Int](nz)
    val gc = new Array[Long](nz)
    val gs = new Array[Double](nz)
    var w = 0
    // dense half in ascending abs-bucket order: positive slots half..maxBuckets-1; negative slots half-1..0
    var off = 0
    while off < half do
      val ds = if positive then half + off else (half - 1) - off
      if dense(ds) > 0 then { gi(w) = base + off; gc(w) = dense(ds); gs(w) = denseSum(ds); w += 1 }
      off += 1
    var p = 0
    while p < len do { gi(w) = idx(p); gc(w) = cnt(p); gs(w) = sum(p); w += 1; p += 1 }
    (gi, gc, gs, w)

  /** Choose a shared symmetric window over the two signs' ascending bucket arrays (collapsing as
    * needed), then scatter each side into dense / large tail / tiny band. */
  private def placeSigned(
    pi: Array[Int], pc: Array[Long], ps: Array[Double], pn0: Int,
    ni: Array[Int], nc: Array[Long], ns: Array[Double], nn0: Int
  ): Unit =
    var pn = pn0
    var nn = nn0
    if pn == 0 && nn == 0 then { started = true; return }
    var placed = false
    while !placed do
      var lo = Int.MaxValue
      if pn > 0 && pi(0) < lo then lo = pi(0)
      if nn > 0 && ni(0) < lo then lo = ni(0)
      // highest non-parkable bucket per side (top `sparseBuckets` go to the large tail)
      val pCore = if pn > sparseBuckets then pi(pn - 1 - sparseBuckets) else lo - 1
      val nCore = if nn > sparseBuckets then ni(nn - 1 - sparseBuckets) else lo - 1
      val core = jm.max(pCore, nCore)
      if core - lo + 1 <= half then
        base = lo
        scatterSigned(true,  pi, pc, ps, pn)
        scatterSigned(false, ni, nc, ns, nn)
        started = true
        placed = true
      else
        pn = collapseRuns(pi, pc, ps, pn)
        nn = collapseRuns(ni, nc, ns, nn)
        bumpCollapse()

  private def scatterSigned(positive: Boolean, gi: Array[Int], gc: Array[Long], gs: Array[Double], n: Int): Unit =
    var k = 0
    while k < n do
      val b = gi(k); val off = b - base
      if off >= 0 && off < half then
        val slot = if positive then half + off else (half - 1) - off
        dense(slot) = gc(k); denseSum(slot) = gs(k)
      else if b < base then
        if positive then { tinyPosCnt += gc(k); tinyPosSum += gs(k) }
        else { tinyNegCnt += gc(k); tinyNegSum += gs(k) }
      else if positive then { sIdx(sLen) = b; sCnt(sLen) = gc(k); sSum(sLen) = gs(k); sLen += 1 }
      else { nIdx(nLen) = b; nCnt(nLen) = gc(k); nSum(nLen) = gs(k); nLen += 1 }
      k += 1


  ///////////////
  /// Merging ///
  ///////////////

  /** Merge another sketch into this one.  Both must share the same base accuracy [[alpha0]].
    * Each of `that`'s bucket means is re-added at its own multiplicity, so the result is exact at
    * bucket resolution when the grids align and within bucket error otherwise; moments are exact.
    */
  def mergeWith(that: UDDSketch): Unit =
    if that.alpha0 != alpha0 then
      throw new IllegalArgumentException(s"Cannot merge UDDSketches with different base accuracy: $alpha0 vs ${that.alpha0}")
    val thatLo = that.loSeen
    val thatHi = that.hiSeen
    val thatAcc = that.acc.snapshot
    that.foreachAll: (mean, _, c) =>
      total += c
      if mean == 0.0 then zeroCount += c else place(mean, c)
    acc += thatAcc
    if thatLo < loSeen then loSeen = thatLo
    if thatHi > hiSeen then hiSeen = thatHi


  /////////////////
  /// Quantiles ///
  /////////////////

  private inline def clampUnit(q: Double): Double = if q < 0 then 0.0 else if q > 1 then 1.0 else q

  /** Iterate every populated entry in ascending value order, yielding `(meanValue, strictValue, count)`:
    * `strictValue` is the signed geometric bucket centre (or the mean, for zero / tiny bins). */
  private inline def foreachAll(inline f: (Double, Double, Long) => Unit): Unit =
    // Negatives, ascending value (most negative first): large tail (desc abs-bucket), dense half, tiny.
    var p = nLen - 1
    while p >= 0 do { val m = nSum(p) / nCnt(p); f(-m, -jm.pow(gamma, nIdx(p) + 0.5), nCnt(p)); p -= 1 }
    if signed then
      var slot = 0
      while slot < half do
        if dense(slot) > 0 then
          val b = base + (half - 1 - slot)
          f(-(denseSum(slot) / dense(slot)), -jm.pow(gamma, b + 0.5), dense(slot))
        slot += 1
      if tinyNegCnt > 0 then { val m = tinyNegSum / tinyNegCnt; f(-m, -m, tinyNegCnt) }
    if zeroCount > 0 then f(0.0, 0.0, zeroCount)
    if signed then
      if tinyPosCnt > 0 then { val m = tinyPosSum / tinyPosCnt; f(m, m, tinyPosCnt) }
      var slot = half
      while slot < maxBuckets do
        if dense(slot) > 0 then
          val b = base + (slot - half)
          f(denseSum(slot) / dense(slot), jm.pow(gamma, b + 0.5), dense(slot))
        slot += 1
      var q = 0
      while q < sLen do { f(sSum(q) / sCnt(q), jm.pow(gamma, sIdx(q) + 0.5), sCnt(q)); q += 1 }
    else
      var q = 0
      while q < sLen && sIdx(q) < base do { f(sSum(q) / sCnt(q), jm.pow(gamma, sIdx(q) + 0.5), sCnt(q)); q += 1 }
      var off = 0
      while off < maxBuckets do
        if dense(off) > 0 then f(denseSum(off) / dense(off), jm.pow(gamma, (base + off) + 0.5), dense(off))
        off += 1
      while q < sLen do { f(sSum(q) / sCnt(q), jm.pow(gamma, sIdx(q) + 0.5), sCnt(q)); q += 1 }

  private def entryCount(): Int =
    var n = 0
    foreachAll: (_, _, _) =>
      n += 1
    n

  /** Interpolated quantile `q` (mean-anchored): exact at the extremes, smoothly
    * interpolated between adjacent bucket means in between.  `q` is clamped to `[0,1]`.
    */
  def quantile(q: Double): Double =
    if total <= 0 then return Double.NaN
    if total == 1 then return hiSeen
    val r = clampUnit(q) * (total - 1)
    val nb = entryCount()
    val rs = new Array[Double](nb + 2)
    val vs = new Array[Double](nb + 2)
    rs(0) = 0.0; vs(0) = min
    var w = 1
    var cum = 0L
    foreachAll: (v, _, c) =>
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

  /** Strict quantile `q`: the (signed) geometric centre of the bucket holding rank `q`, carrying the
    * worst-case relative-error guarantee ([[alpha]]).  `q` is clamped to `[0,1]`.
    */
  def quantileStrict(q: Double): Double =
    if total <= 0 then return Double.NaN
    val target = jm.floor(clampUnit(q) * (total - 1)).toLong
    var cum = 0L
    var ans = Double.NaN
    var done = false
    foreachAll: (_, strict, c) =>
      if !done then
        cum += c
        if target < cum then { ans = strict; done = true }
    if done then ans else max

  /** Median (interpolated). */
  def median: Double = quantile(0.5)

  /** Interquartile range Q3 − Q1 (interpolated). */
  def iqr: Double = quantile(0.75) - quantile(0.25)

  /** Approximate fraction of values `<= x` (the empirical CDF at `x`). */
  def fractionBelow(x: Double): Double =
    if total <= 0 then return Double.NaN
    var below = 0.0
    foreachAll: (v, _, c) =>
      if v < x then below += c.toDouble
      else if v == x then below += c.toDouble * 0.5
    below / total


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
    System.arraycopy(nIdx, 0, s.nIdx, 0, nLen)
    System.arraycopy(nCnt, 0, s.nCnt, 0, nLen)
    System.arraycopy(nSum, 0, s.nSum, 0, nLen)
    s.nLen = nLen
    s.signed = signed
    s.tinyPosCnt = tinyPosCnt; s.tinyPosSum = tinyPosSum
    s.tinyNegCnt = tinyNegCnt; s.tinyNegSum = tinyNegSum
    s.zeroCount = zeroCount; s.total = total
    s.acc.n = acc.n; s.acc.mean = acc.mean; s.acc.sse = acc.sse
    s.loSeen = loSeen; s.hiSeen = hiSeen
    s

  /** Remove all data, returning to the initial resolution and unsigned mode. */
  def clear(): Unit =
    gamma = (1 + alpha0) / (1 - alpha0); invLog2Gamma = 1.0 / gamma.log2; nCollapse = 0
    clearDense(); base = 0; started = false; sLen = 0; nLen = 0
    signed = false
    tinyPosCnt = 0; tinyPosSum = 0.0; tinyNegCnt = 0; tinyNegSum = 0.0
    zeroCount = 0; total = 0; acc.reset()
    loSeen = Double.PositiveInfinity; hiSeen = Double.NegativeInfinity

  override def toString =
    if total <= 0 then s"UDDSketch(empty, α=$alpha0)"
    else f"UDDSketch(n=$total, α=$alpha%.4g${if signed then ", signed" else ""}${if nCollapse > 0 then s", $nCollapse collapses" else ""}, median≈${quantile(0.5)}%.4g)"
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


////////////////////////////////////////////////////////////////////////
/// ADWIN2: adaptive-windowing change detection over a streaming mean   ///
////////////////////////////////////////////////////////////////////////

/** ADWIN2 (Bifet & Gavaldà 2007) — an adaptive-window change detector for a stream of
  * real values, with a variance-aware cut.
  *
  * It keeps a window of the most recent values, compressed as an '''exponential
  * histogram''': a ladder of buckets whose sizes grow geometrically (1,1,…,2,2,…,4,…),
  * each holding the `(count, sum, SSE)` of a contiguous run.  Memory is `O(log width)`.
  *
  * On every [[add]] it inspects each split of the window into an older sub-window `W0`
  * and a newer `W1` (at bucket boundaries); if their means differ by more than a
  * variance-aware Hoeffding bound (confidence [[delta]]) it concludes the distribution
  * changed there and '''drops `W0`''', shrinking the window to the current regime.  So
  * the window mean tracks the present regime, and [[add]] returning `true` is a
  * changepoint signal — the natural trigger for closing a profiling segment.
  *
  * The sufficient statistics are exactly additive/subtractive, which is what makes the
  * bucket merges and the drop exact.  This detector watches the *mean*; pair it with a
  * separate quantile sketch (e.g. [[UDDSketch]]) per segment for distributional summaries.
  */
final class Adwin private (val delta: Double, maxBucketsPerRow: Int) {
  import Adwin.Row

  private val M = maxBucketsPerRow
  private val minWin = 5            // don't test cuts against sub-windows smaller than this

  private var wWidth = 0L           // number of values in the window
  private var wTotal = 0.0          // sum of values
  private var wSse = 0.0            // sum of squared deviations from the window mean

  private val rows = ArrayBuffer.empty[Row]   // rows(k) holds buckets of size 2^k; higher k = older


  //////////////////////////
  /// Public observations ///
  //////////////////////////

  /** Number of values currently in the adaptive window (shrinks when a change is detected). */
  def width: Long = wWidth

  /** Sum of the values in the window. */
  def total: Double = wTotal

  /** Mean of the window (the current-regime estimate). */
  def mean: Double = if wWidth > 0 then wTotal / wWidth else Double.NaN

  /** Sample variance of the window. */
  def variance: Double = if wWidth > 1 then wSse / (wWidth - 1) else 0.0

  /** Sample standard deviation of the window. */
  def sd: Double = jm.sqrt(variance)

  /** Number of histogram rows in use (`O(log width)`); exposed mainly for inspection. */
  def rowsInUse: Int = rows.length


  ////////////////
  /// Ingest   ///
  ////////////////

  /** Feed one value.  Returns `true` if a change was detected (and stale data dropped). */
  def add(x: Double): Boolean =
    if x != x then return false
    if wWidth > 0 then wSse += (wWidth.toDouble / (wWidth + 1)) * sq(wTotal / wWidth - x)
    wTotal += x
    wWidth += 1
    pushBucket(0, x, 0.0)
    compress()
    checkDrift()

  /** Drop all data, resetting the window. */
  def clear(): Unit =
    wWidth = 0; wTotal = 0.0; wSse = 0.0
    rows.clear()


  ////////////////
  /// Internals ///
  ////////////////

  private inline def sq(d: Double): Double = d * d

  /** Append a bucket of `2^level` items (newest position) to row `level`, growing the ladder. */
  private def pushBucket(level: Int, t: Double, v: Double): Unit =
    while rows.length <= level do rows += new Row(M + 1)
    val row = rows(level)
    row.total(row.n) = t
    row.variance(row.n) = v
    row.n += 1

  /** Merge the two oldest buckets of any over-full row into one of the next size up. */
  private def compress(): Unit =
    var level = 0
    while level < rows.length do
      val row = rows(level)
      if row.n > M then
        val size = (1L << level).toDouble
        val t = row.total(0) + row.total(1)
        val v = row.variance(0) + row.variance(1) + sq(row.total(0) - row.total(1)) / (2.0 * size)
        var k = 2
        while k < row.n do
          row.total(k - 2) = row.total(k); row.variance(k - 2) = row.variance(k)
          k += 1
        row.n -= 2
        pushBucket(level + 1, t, v)
      level += 1

  /** Remove the globally-oldest bucket (top non-empty row, oldest slot), updating the moments. */
  private def deleteOldest(): Unit =
    val level = rows.length - 1
    val row = rows(level)
    val size = (1L << level)
    val u = row.total(0)
    val v = row.variance(0)
    val wNew = wWidth - size
    if wNew <= 0 then
      wWidth = 0; wTotal = 0.0; wSse = 0.0
    else
      val uNew = wTotal - u
      val meanA = uNew / wNew
      val meanB = u / size
      wSse = wSse - v - (wNew.toDouble * size / wWidth) * sq(meanA - meanB)
      if wSse < 0 then wSse = 0.0
      wTotal = uNew
      wWidth = wNew
    var k = 1
    while k < row.n do
      row.total(k - 1) = row.total(k); row.variance(k - 1) = row.variance(k)
      k += 1
    row.n -= 1
    while rows.nonEmpty && rows(rows.length - 1).n == 0 do rows.remove(rows.length - 1): Unit

  /** Variance-aware Hoeffding cut: do the sub-window means differ beyond the bound? */
  private def cut(n0: Long, n1: Long, u0: Double, u1: Double): Boolean =
    val n = wWidth.toDouble
    val v = wSse / wWidth
    val m = 1.0 / (n0 - minWin + 1) + 1.0 / (n1 - minWin + 1)
    val dd = jm.log(2.0 * jm.log(n) / delta)
    val eps = jm.sqrt(2.0 * m * v * dd) + (2.0 / 3.0) * dd * m
    jm.abs(u0 / n0 - u1 / n1) > eps

  /** Peel stale buckets from the old end while any boundary cut signals a change. */
  private def checkDrift(): Boolean =
    var changed = false
    if wWidth >= 2 * minWin then
      var scanning = true
      while scanning do
        scanning = false
        var n0 = 0L; var u0 = 0.0
        var n1 = wWidth; var u1 = wTotal
        var level = rows.length - 1
        var triggered = false
        while level >= 0 && !triggered do
          val row = rows(level)
          val size = (1L << level)
          var idx = 0
          while idx < row.n && !triggered do
            val t = row.total(idx)
            n0 += size; u0 += t; n1 -= size; u1 -= t
            if n1 > 0 && n0 > minWin && n1 > minWin && cut(n0, n1, u0, u1) then
              deleteOldest()
              changed = true
              triggered = true
              scanning = true
            idx += 1
          level -= 1
    changed

  override def toString =
    if wWidth == 0 then s"Adwin(empty, δ=$delta)"
    else f"Adwin(width=$wWidth, mean=$mean%.4g, sd=$sd%.4g)"
}
object Adwin {
  private final class Row(cap: Int) {
    val total = new Array[Double](cap)
    val variance = new Array[Double](cap)
    var n = 0
  }

  /** An ADWIN2 detector.
    *
    * @param delta             confidence (false-positive budget per cut); smaller = fewer false alarms, slower to react
    * @param maxBucketsPerRow  exponential-histogram resolution `M` (memory/precision knob; 5 is the usual default)
    */
  def apply(delta: Double = 0.002, maxBucketsPerRow: Int = 5): Adwin =
    if !(delta > 0 && delta < 1) then
      throw new IllegalArgumentException(s"Adwin confidence delta must be in (0, 1), not $delta")
    if maxBucketsPerRow < 2 then
      throw new IllegalArgumentException(s"Adwin needs maxBucketsPerRow >= 2, not $maxBucketsPerRow")
    new Adwin(delta, maxBucketsPerRow)
}
////////////////////////////////////////////////////////////////////////
/// Radwin: robust, distribution-free changepoint location             ///
////////////////////////////////////////////////////////////////////////

/** Radwin (robust ADWIN) — retrospective changepoint localization over a buffered window
  * of a real-valued stream, built on '''rank-CUSUM''' statistics so it is distribution-free and
  * outlier-robust by construction.
  *
  * It keeps the most recent `capacity` values; [[locate]] pools them into ranks and scans for the
  * single split best explaining the window as two regimes, in two complementary channels (both via
  * [[kse.maths.Changepoint.bridge]], whose null is `sup|Brownian bridge|`):
  *
  *  - '''location''' (Wilcoxon — a CUSUM of the ranks): a shift in level, sensitive well below the
  *    noise given enough samples;
  *  - '''dispersion''' (Mood — a CUSUM of squared centered ranks): a change in spread/shape that
  *    leaves the median put.
  *
  * Because everything runs on ranks, a single huge outlier is merely "rank n" — it cannot move the
  * breakpoint or break calibration — and the per-channel p-values are correct for '''any''' noise
  * distribution (Gaussian, heavy-tailed, skewed) and for tied/quantized data alike.  The two
  * channels are combined by Bonferroni; [[locate]] returns the breakpoint, a localization interval,
  * and the combined p-value, leaving any across-stream false-discovery control to the caller.
  *
  * The Kolmogorov null is asymptotic, so p-values are mildly conservative for small windows
  * (`O(1/√n)`); the profiler's windows are large and `minSeg` keeps each side substantial.
  */
final class Radwin private (val capacity: Int, val minSeg: Int, val alpha: Double, val ciMargin: Double) {
  private val values = new Array[Double](capacity)
  private var head = 0
  private var count = 0
  private var seen = 0L

  /** Number of values currently buffered. */
  def size: Int = count

  /** Total number of values ever fed (the absolute stream length). */
  def total: Long = seen

  private inline def oldest: Int = if count == capacity then head else 0
  private inline def at(p: Int): Double = values((oldest + p) % capacity)

  /** Feed one value (NaN ignored).  O(1). */
  def add(x: Double): Unit =
    if x != x then return
    values(head) = x
    head = (head + 1) % capacity
    if count < capacity then count += 1
    seen += 1

  /** Drop all buffered data. */
  def clear(): Unit = { head = 0; count = 0; seen = 0 }

  /** Locate the most likely single breakpoint in the current window. */
  def locate(): Radwin.Change =
    val n = count
    val base = seen - n
    if n < 2 * minSeg then return Radwin.Change(base + n, base + n, base + n, 1.0, 1.0, 1.0, false)

    val r = new Array[Double](n)
    var i = 0
    while i < n do { r(i) = at(i); i += 1 }
    val rank = Ranks.of(r)

    val locB = Changepoint.bridge(rank, minSeg)         // Wilcoxon: location
    val center = (n + 1) / 2.0
    val disp = new Array[Double](n)
    i = 0
    while i < n do { val c = rank(i) - center; disp(i) = c * c; i += 1 }   // Mood: dispersion
    val dispB = Changepoint.bridge(disp, minSeg)

    val pLoc = locB.p
    val pScale = dispB.p
    val p = jm.min(1.0, 2.0 * jm.min(pLoc, pScale))     // Bonferroni across the two channels
    val sig = p < alpha
    val b = if pLoc <= pScale then locB else dispB
    val ci = b.interval(ciMargin)
    Radwin.Change(base + b.at, base + ci._1, base + ci._2, p, pLoc, pScale, sig)
}
object Radwin {
  /** A located breakpoint.  `at` is the absolute stream index of the first value of the new regime
    * and `[loCI, hiCI]` its localization interval; `p` is the combined (Bonferroni) p-value with
    * `pLoc`/`pScale` the per-channel values (location vs dispersion).  `significant` is the
    * convenience `p < alpha`; consumers should use `p` and apply their own across-stream false-
    * discovery control.
    */
  case class Change(at: Long, loCI: Long, hiCI: Long, p: Double, pLoc: Double, pScale: Double, significant: Boolean)

  /** A distribution-free rank-CUSUM changepoint localizer.
    *
    * @param capacity  how many recent values to retain (the localizable window)
    * @param minSeg    smallest segment allowed each side of a split (keeps the asymptotic null sound)
    * @param alpha     p-value cutoff for the convenience `significant` flag (consumers should use `p`)
    * @param ciMargin  margin (in Brownian-bridge units) defining the localization interval
    */
  def apply(capacity: Int = 4096, minSeg: Int = 20, alpha: Double = 0.05, ciMargin: Double = 0.5): Radwin =
    if capacity < 4 then throw new IllegalArgumentException(s"Radwin needs capacity >= 4, not $capacity")
    if minSeg < 2 then throw new IllegalArgumentException(s"Radwin minSeg must be >= 2, not $minSeg")
    if !(alpha > 0 && alpha < 1) then throw new IllegalArgumentException(s"Radwin alpha must be in (0, 1), not $alpha")
    new Radwin(capacity, minSeg, alpha, ciMargin)
}


////////////////////////////////////////////////////////////////////////
/// MultiRadwin: segmented multi-channel summary, detection driven by  ///
/// a synthesized score (composes Radwin + UDDSketch + Est).            ///
////////////////////////////////////////////////////////////////////////

/** MultiRadwin — accumulate several labeled input channels at once, summarizing each per regime,
  * where the regime boundaries come from a single '''synthesized''' score.
  *
  * {{{
  * val m = MultiRadwin(Array("first","second"))("ratio", xs => xs(1)/xs(0))
  * m.add(Array(tFirst, tSecond))   // one tick = one value per input channel
  * }}}
  * Each input channel keeps its own per-segment and overall [[Est]]/[[UDDSketch]] summary.  Every tick
  * the `synth` function maps the inputs to one score, which drives a composed [[Radwin]] detector; on a
  * significant changepoint that also clears a practical-effect gate, every channel's current segment is
  * closed at once (boundaries are shared, set by the score).  Detecting on a *ratio* makes the
  * segmentation immune to common-mode shifts (core migration, throttling, load) that move all inputs
  * together — only changes in the *relationship* split.  Inputs and score must be non-negative.
  */
final class MultiRadwin private (
  val inputs: Array[String],
  val scoreLabel: String,
  synth: Array[Double] => Double,
  cfg: MultiRadwin.Config
) {
  import MultiRadwin.Chan
  private val k = inputs.length
  private val chans = Array.tabulate(k)(i => new Chan(inputs(i)))
  private val score = new Chan(scoreLabel)
  private val radwin = Radwin(cfg.capacity, cfg.minSeg, cfg.alpha)
  private val recent = new Array[Double](jm.max(2 * cfg.minSeg, 8))   // recent scores ≈ the new regime
  private var rHead = 0
  private var rFill = 0
  private var sinceCheck = 0

  /** Feed one tick: one value per input channel (length must equal `inputs.length`). */
  def add(xs: Array[Double]): Unit = this.synchronized:
    if xs.length != k then throw new IllegalArgumentException(s"MultiRadwin expects $k inputs, got ${xs.length}")
    var i = 0
    while i < k do { chans(i).add(xs(i)); i += 1 }
    val s = synth(xs)
    score.add(s)
    radwin.add(s)
    recent(rHead) = s; rHead = (rHead + 1) % recent.length; if rFill < recent.length then rFill += 1
    sinceCheck += 1
    if sinceCheck >= cfg.cadence then
      sinceCheck = 0
      if radwin.size >= 2 * cfg.minSeg && radwin.locate().significant then
        val newLevel = recentMedian
        val oldLevel = score.segMedian                 // O(1) sketch query (mostly the old regime)
        radwin.clear()
        if practicallyDifferent(oldLevel, newLevel) then    // gate out tiny shifts
          i = 0
          while i < k do { chans(i).commit(); i += 1 }
          score.commit()

  private def recentMedian: Double =
    if rFill == 0 then Double.NaN else java.util.Arrays.copyOf(recent, rFill).median

  private def practicallyDifferent(a: Double, b: Double): Boolean =
    val scale = jm.max(jm.abs(a), jm.abs(b))
    scale > 0 && jm.abs(a - b) / scale > cfg.effect

  /** One [[MultiRadwin.Track]] per input channel, then the synthesized score channel. */
  def tracks: Vector[MultiRadwin.Track] = this.synchronized:
    val out = Vector.newBuilder[MultiRadwin.Track]
    var i = 0
    while i < k do { out += chans(i).track; i += 1 }
    out += score.track
    out.result()

  /** Drop all data. */
  def clear(): Unit = this.synchronized:
    var i = 0
    while i < k do { chans(i).reset(); i += 1 }
    score.reset()
    radwin.clear()
    rHead = 0; rFill = 0; sinceCheck = 0
}
object MultiRadwin {
  /** Summary statistics for one segment (or the overall) of one channel. */
  case class Stat(n: Long, mean: Double, sd: Double, median: Double, q90: Double, q99: Double)

  /** One channel's record: per-regime `segments` (the last is the still-open one) and `overall`. */
  case class Track(label: String, segments: Vector[Stat], overall: Stat)

  private case class Config(alpha: Double, effect: Double, cadence: Int, capacity: Int, minSeg: Int)

  private def newSketch = UDDSketch(0.01, maxBuckets = 512, sparseBuckets = 32)

  private def statOf(e: Est, s: UDDSketch): Stat =
    Stat(jm.round(e.n), e.mean, e.sd, s.median, s.quantile(0.9), s.quantile(0.99))

  private final class Chan(val label: String) {
    private var segEst = new Est.M(0, 0, 0)
    private var segSketch = newSketch
    private val allEst = new Est.M(0, 0, 0)
    private val allSketch = newSketch
    private val segs = scala.collection.mutable.ArrayBuffer.empty[Stat]
    def add(x: Double): Unit =
      if x == x && x >= 0 then
        segEst += x; segSketch += x; allEst += x; allSketch += x
    def segMedian: Double = segSketch.median
    def commit(): Unit =
      segs += statOf(segEst, segSketch)
      segEst = new Est.M(0, 0, 0); segSketch = newSketch
    def track: Track = Track(label, (segs.toVector :+ statOf(segEst, segSketch)), statOf(allEst, allSketch))
    def reset(): Unit =
      segEst = new Est.M(0, 0, 0); segSketch = newSketch
      allEst.reset(); allSketch.clear(); segs.clear()
  }

  /** A segmenting multi-channel summarizer whose regime boundaries come from `synth`.
    *
    * @param inputs   labels for the input channels (and the required `add` array length)
    * @param scoreLabel label for the synthesized score channel
    * @param synth    maps one tick's inputs to the changepoint score (non-negative; e.g. a ratio)
    * @param alpha    detector p-value cutoff (conservative default to limit over-segmenting)
    * @param effect   minimum relative level change for a real regime (smaller shifts are absorbed)
    * @param cadence  ticks between changepoint checks
    * @param capacity detector window size; minSeg the smallest segment each side
    */
  def apply(inputs: Array[String])(
    scoreLabel: String,
    synth: Array[Double] => Double,
    alpha: Double = 0.001,
    effect: Double = 0.03,
    cadence: Int = 128,
    capacity: Int = 1024,
    minSeg: Int = 20
  ): MultiRadwin =
    if inputs.isEmpty then throw new IllegalArgumentException("MultiRadwin needs at least one input channel")
    new MultiRadwin(inputs.clone, scoreLabel, synth, Config(alpha, jm.max(0.0, effect), jm.max(1, cadence), jm.max(8, capacity), jm.max(2, minSeg)))
}
