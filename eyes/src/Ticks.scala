// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.eyes


import java.lang.{Math => jm}


/** Decimal-exact ticks for a continuous axis: parallel arrays of positions (the correctly
  * rounded `Double` of each exact decimal tick value) and labels (the shortest decimal
  * spelling of that same exact value), plus unlabeled `minor` positions between and
  * beyond the majors.
  */
final class Ticks private[eyes] (val values: Array[Double], val labels: Array[String], val minor: Array[Double]):
  def length: Int = values.length


/** Tick generation.
  *
  * Ticks are the integer multiples, within the span, of a step m*10^k with m in {1, 2, 5}.
  * Restricting m to the divisors of 10 is the point: such a grid contains every multiple of
  * every coarser power of ten, so any value in span *rounder* than the step -- 1.02 amid
  * 1.012, 1.014, ... -- lands exactly on a tick.  Wilkinson-style step sets that also admit
  * 2.5, 3, or 4 break this (multiples of 0.003 skip 1.01, multiples of 0.04 skip 1.1, and
  * 0.0025 prints 1.0125 while skipping the shorter 1.012), which is why they are not used.
  *
  * Labels are built from the tick's decimal digits -- the integer multiple against the power
  * of ten -- never from its Double, so 1.02 prints as "1.02": no trailing zeros, no binary
  * float residue.  If the longest plain label would exceed [[plainLimit]] characters and
  * scientific notation (lowercase e) is strictly shorter, the whole axis switches together,
  * so mixed styles never appear; a zero tick is always "0".
  *
  * Minor ticks subdivide each major interval with the largest {1, 2, 5} step that divides
  * the major step at least four ways: major mantissa 1 -> fifths, 2 -> quarters, 5 ->
  * fifths.  So minors are nice decimals themselves, they coincide with majors only on the
  * exact integer grid (no float comparisons), and they run past the outermost majors to
  * the span edges.  Minors are never labeled -- a labeled minor would just be a major.
  */
object Ticks:
  /** Plain labels at most this long never trigger the switch to scientific notation. */
  inline val plainLimit = 6

  val none: Ticks = new Ticks(new Array[Double](0), new Array[String](0), new Array[Double](0))

  /** About `target` ticks spanning `lo` to `hi` (delivery is roughly 0.7x to 1.5x that,
    * depending on where nice multiples fall), and never fewer than two for a valid span:
    * a single labeled tick would give the reader a position but no sense of scale.
    */
  def linear(lo: Double, hi: Double, target: Int): Ticks =
    val raw = (hi - lo) / jm.max(1, target)
    if !(hi > lo) || !java.lang.Double.isFinite(raw) then none
    else
      var (m, e) = stepParts(raw)
      var res = make(lo, hi, m, e)
      // an axis with any labels needs at least two: one lone tick gives the reader a
      // position but no scale.  Refine down the ladder until a second tick lands in
      // span (at most 3 rungs suffice; the guard covers the degenerate-span bailouts).
      var guard = 8
      while res.length < 2 && guard > 0 do
        if m == 5 then m = 2
        else if m == 2 then m = 1
        else
          m = 5
          e -= 1
        res = make(lo, hi, m, e)
        guard -= 1
      res

  /** The nice step nearest `raw`: m*10^k with m in {1, 2, 5}.  Also the snap rule for
    * stats that want round widths (bin), so histogram edges obey the same criterion
    * tick marks do.
    */
  def step(raw: Double): Double =
    if !(raw > 0 && raw < Double.PositiveInfinity) then Double.NaN
    else
      val (m, e) = stepParts(raw)
      valueOf(m, e)

  // step mantissa (1, 2, or 5) and power of ten bracketing raw at pleasant density
  private def stepParts(raw: Double): (Int, Int) =
    var e = jm.floor(jm.log10(raw)).toInt
    val frac = raw / jm.pow(10, e)
    var m = if frac < 1.5 then 1 else if frac < 3 then 2 else if frac < 7 then 5 else 10
    if m == 10 then
      m = 1
      e += 1
    (m, e)

  // exact for |k| <= 22, which covers every axis where label exactness is observable
  private def pw(k: Int): Double = jm.pow(10.0, k)

  private def valueOf(mult: Long, e: Int): Double =
    if e >= 0 then mult.toDouble * pw(e) else mult.toDouble / pw(-e)

  private def make(lo: Double, hi: Double, m: Int, e: Int): Ticks =
    val step = valueOf(m, e)
    val q0 = lo / step
    val q1 = hi / step
    if !(jm.abs(q0) < 4e15 && jm.abs(q1) < 4e15) then none  // index math needs exact integers
    else
      // tolerance admits endpoints that are decimal ticks up to roundoff: absolute for
      // small quotients, ulp-scaled so ticks far from zero on a fine grid still qualify
      val kLo = jm.ceil(q0 - 4 * jm.ulp(q0) - 1e-9).toLong
      val kHi = jm.floor(q1 + 4 * jm.ulp(q1) + 1e-9).toLong
      val n = kHi - kLo + 1
      if n <= 0 || n > 4096 then none
      else
        val nn = n.toInt
        val mult = new Array[Long](nn)
        val vs = new Array[Double](nn)
        var i = 0
        while i < nn do
          mult(i) = (kLo + i) * m
          vs(i) = valueOf(mult(i), e)
          i += 1
        val plain = new Array[String](nn)
        var wide = 0
        i = 0
        while i < nn do
          plain(i) = plainLabel(mult(i), e)
          if plain(i).length > wide then wide = plain(i).length
          i += 1
        var labs = plain
        if wide > plainLimit then
          val sci = new Array[String](nn)
          var w = 0
          i = 0
          while i < nn do
            sci(i) = sciLabel(mult(i), e)
            if sci(i).length > w then w = sci(i).length
            i += 1
          if w < wide then labs = sci
        new Ticks(vs, labs, minorsIn(lo, hi, m, e))

  /** Minor positions across [lo, hi] for a major step m*10^e: the largest ladder step
    * dividing it at least four ways, with the exact major coincidences skipped by index.
    */
  private def minorsIn(lo: Double, hi: Double, m: Int, e: Int): Array[Double] =
    val (mm, me, ratio) =
      if m == 1 then (2, e - 1, 5)
      else if m == 2 then (5, e - 1, 4)
      else (1, e, 5)
    val step = valueOf(mm, me)
    val q0 = lo / step
    val q1 = hi / step
    if !(jm.abs(q0) < 4e15 && jm.abs(q1) < 4e15) then new Array[Double](0)
    else
      val kLo = jm.ceil(q0 - 4 * jm.ulp(q0) - 1e-9).toLong
      val kHi = jm.floor(q1 + 4 * jm.ulp(q1) + 1e-9).toLong
      val n = kHi - kLo + 1
      if n <= 0 || n > 4096 then new Array[Double](0)
      else
        val b = new Array[Double](n.toInt)
        var j = 0
        var k = kLo
        while k <= kHi do
          if k % ratio != 0 then
            b(j) = valueOf(k * mm, me)
            j += 1
          k += 1
        java.util.Arrays.copyOf(b, j)

  // digits with trailing zeros folded into the exponent, so labels come out shortest
  private def strip(mult: Long, e: Int): (String, Int) =
    val d = jm.abs(mult).toString
    var end = d.length
    while end > 1 && d.charAt(end - 1) == '0' do end -= 1
    (d.substring(0, end), e + d.length - end)

  private def plainLabel(mult: Long, e: Int): String =
    if mult == 0 then "0"
    else
      val (d, ee) = strip(mult, e)
      val s =
        if ee >= 0 then d + "0" * ee
        else if -ee < d.length then d.substring(0, d.length + ee) + "." + d.substring(d.length + ee)
        else "0." + "0" * (-ee - d.length) + d
      if mult < 0 then "-" + s else s

  private def sciLabel(mult: Long, e: Int): String =
    if mult == 0 then "0"
    else
      val (d, ee) = strip(mult, e)
      val mant = if d.length == 1 then d else d.substring(0, 1) + "." + d.substring(1)
      val s = mant + "e" + (ee + d.length - 1)
      if mult < 0 then "-" + s else s
