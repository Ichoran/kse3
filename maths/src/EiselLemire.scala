// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.maths


import scala.util.boundary


/** Correctly rounded decimal-to-binary floating point conversion: Clinger's exact-arithmetic
  * fast path backed by the Eisel-Lemire algorithm (devised by Michael Eisel and Daniel Lemire;
  * described in Lemire, "Number Parsing at a Gigabyte per Second", 2021), reimplemented from
  * the algorithm's principles rather than ported; the error bounds are derived in the comments,
  * and the constants were rederived with Mathematica.
  *
  * The contract is deliberately partial: `toDouble` and `toFloat` answer the correctly rounded
  * value of `w * 10^q10` for an unsigned mantissa `w`, or NaN for the rare inputs the fast paths
  * cannot decide, which the caller must send to an exact (slow) parser.  Sign, digit gathering,
  * and NaN/Infinity literals are the caller's business: this is the numeric core shared by text
  * parsers, not itself a parser.  A caller holding more than 19 significant digits passes the
  * 19-digit truncation `w` and accepts `toDouble(w, q)` only when it equals `toDouble(w+1, q)` —
  * the true value lies between the two, and NaN punts compare unequal automatically.
  */
object EiselLemire {

  // Powers of ten that are exactly representable as Double (the next, 1e23, is not).
  private val pow10d: Array[Double] = Array(
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11,
    1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22
  )

  /** The 128-bit truncated binary significands of 10^q for q in -342 to 308, interleaved
    * (hi, lo) at index 2*(q+342).  With E(q) = floor(q log2 10), the entry is P = floor(10^q *
    * 2^(127-E(q))), which lies in [2^127, 2^128) — hi's top bit is always set — and the truly
    * scaled power is P + d for some 0 <= d < 1.  That one-sided (truncated) error is what the
    * kernel's carry checks rely on.  Built exactly from BigInteger at load (well under a
    * millisecond); spot-checked against Mathematica-computed references in the test suite.
    *
    * The range is exactly what a 19-digit mantissa can need: below -342 even 10^19-1 scales to
    * under 2^-1075 (half the smallest subnormal, so it rounds to zero), and above 308 even 1
    * scales past (1 - 2^-54)*2^1024 (the rounding boundary to infinity).
    */
  private[kse] val sig128: Array[Long] =
    val a = new Array[Long](2 * (342 + 308 + 1))
    var q = -342
    while q <= 308 do
      val e = (q * 108853) >> 15   // floor(q log2 10); see below for the constant
      val p =
        if q >= 0 then java.math.BigInteger.TEN.pow(q).shiftLeft(127 - e)   // negative shift floors, per spec
        else java.math.BigInteger.ONE.shiftLeft(127 - e).divide(java.math.BigInteger.TEN.pow(-q))
      val k = 2 * (q + 342)
      a(k) = p.shiftRight(64).longValue()
      a(k + 1) = p.longValue()
      q += 1
    a

  /** The correctly rounded `Double` whose value is `w * 10^q10`, or NaN when undecided (see
    * the object docs; roughly one hard case per 10^7 random 17-digit inputs).
    */
  def toDouble(w: ULong, q10: Int): Double =
    val x = w.signed
    if x == 0L then 0.0
    else if x >= 0L && x <= (1L << 53) && q10 >= -22 && q10 <= 22 then
      // Clinger's fast path: the mantissa and the power of ten are both exactly representable,
      // so the one multiply or divide is correctly rounded by the hardware.
      if q10 >= 0 then x.toDouble * pow10d(q10) else x.toDouble / pow10d(-q10)
    else eiselLemire(x, q10)

  /** The correctly rounded `Float` whose value is `w * 10^q10`, or NaN when undecided.
    * Punts more often than `toDouble`: also when the value is in the `Float` subnormal range
    * (narrowing respaces the rounding midpoints there) or dead on a narrowing midpoint.
    */
  def toFloat(w: ULong, q10: Int): Float =
    val v = toDouble(w, q10)
    val b = java.lang.Double.doubleToRawLongBits(v)
    // Narrowing the correctly rounded Double is correct unless it lands exactly on a Float
    // rounding midpoint (bit 28 set, bits 27-0 clear: one half-ulp of a 24-bit mantissa), where
    // the dropped decimal tail decides a tie the Double can no longer see.  The pattern test is
    // only exact for results at or above the smallest normal Float, 2^-126 (raw exponent 0x381);
    // below that we punt.  Infinity narrows to infinity via the same test (its low bits are
    // clear), a toDouble punt narrows to a NaN punt, and the overflow-to-infinity midpoint
    // (2^128 - 2^103) is caught as an ordinary midpoint.
    if (b >>> 52) >= 0x381L && (b & 0x1FFFFFFFL) != 0x10000000L then v.toFloat
    else if v == 0.0 then 0.0f   // underflowed past half the smallest subnormal Double: 0 for Float too
    else Float.NaN

  /** The Eisel-Lemire kernel proper, for nonzero unsigned `x`.
    *
    * The engine is one wide multiply: normalize `x` to `v` in [2^63, 2^64), multiply by the
    * 128-bit table significand of 10^q, and the top bits of the product are the answer's
    * mantissa.  Only the top 55 bits matter — 1 for normalization slack (the product of two
    * normalized values fills 127 or 128 bits), 53 of mantissa, 1 round bit — so the 9 bits of
    * the product's high word below them are guards.  The table is truncated, so the error is
    * one-sided; it can only disturb the decisive bits by carrying through all-ones guards, and
    * a second multiply, against the table's next 64 bits, settles almost every such carry.
    * The genuinely undecidable leftovers punt as NaN: a carry unresolved after 128 table bits,
    * a value truncation can't distinguish from an exact round-to-even midpoint, and subnormal
    * or overflow-edge exponents where the bit budget changes.
    */
  private def eiselLemire(x: Long, q: Int): Double =
    if q < -342 then 0.0
    else if q > 308 then Double.PositiveInfinity
    else boundary[Double]:
      val l = java.lang.Long.numberOfLeadingZeros(x)
      val v = x << l
      val k = (q + 342) << 1
      var xh = Math.unsignedMultiplyHigh(v, sig128(k))
      var xl = v * sig128(k)
      // True product = (xh:xl) + v*(pl+d)/2^64 < (xh:xl) + v: refine only if that could carry
      // into the guards, i.e. they are all ones and xl + v wraps.
      if (xh & 0x1FF) == 0x1FF && java.lang.Long.compareUnsigned(xl + v, xl) < 0 then
        val pl = sig128(k + 1)
        val yl = v * pl
        val zl = xl + Math.unsignedMultiplyHigh(v, pl)
        if java.lang.Long.compareUnsigned(zl, xl) < 0 then xh += 1
        // Remaining error < 2 lo-word units (the dropped low word of v*pl, plus v*d/2^64, each
        // under 1): ambiguity now needs all-ones everywhere visible AND a wrap still in reach.
        if (xh & 0x1FF) == 0x1FF && zl == -1L && java.lang.Long.compareUnsigned(yl + v, yl) < 0 then
          boundary.break(Double.NaN)
        xl = zl
      val ub = (xh >>> 63).toInt   // normalization slack: product in [2^126, 2^127) or [2^127, 2^128)
      var m = xh >>> (ub + 9)      // 53 mantissa bits + round bit
      // A round bit of 1 over visible zeros may be an exact midpoint (round to even) or just
      // above one (round up): truncation hides the difference, so punt when evenness matters.
      // (For ub = 1 the guard mask misses one visible bit; that errs toward punting, which is safe.)
      if (m & 3L) == 1L && (xh & 0x1FF) == 0L && xl == 0L then boundary.break(Double.NaN)
      m = (m + (m & 1L)) >>> 1     // ambiguous ties punted above, so round-half-up here IS round-to-nearest
      // Exponent bookkeeping: value = (xh:xl) * 2^(E(q)-127-l+64) and m = (xh:xl) >> (73+ub+64),
      // so value = m * 2^(E(q)+ub-l+10); IEEE reads a mantissa in [2^52, 2^53) as m * 2^(eb-1075),
      // giving biased exponent eb = E(q) + ub - l + 1086.  E(q) = floor(q log2 10) = (q*108853)
      // >> 15, the smallest shift whose ceiling multiplier is exact over the whole table range
      // (Mathematica-checked on -350 to 350; >> is floor division, so negative q is correct too).
      var eb = ((q * 108853) >> 15) + ub - l + 1086
      if (m >>> 53) != 0L then     // rounding overflowed the mantissa to 2^53: renormalize
        m = m >>> 1
        eb += 1
      if eb <= 0 || eb >= 2047 then boundary.break(Double.NaN)   // subnormal or overflow edge
      java.lang.Double.longBitsToDouble((eb.toLong << 52) | (m & 0x000FFFFFFFFFFFFFL))
}
