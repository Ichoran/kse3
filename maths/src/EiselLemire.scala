// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.maths


import scala.annotation.targetName
import scala.util.boundary

import scala.collection.immutable.{Range => Rg}

import kse.basics.{given, _}
import kse.basics.intervals._
import kse.basics.Mem


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
  * parsers, not itself a full parser.  A caller holding more than 19 significant digits passes
  * the 19-digit truncation `w` and accepts `toDouble(w, q)` only when it equals `toDouble(w+1, q)`
  * — the true value lies between the two, and NaN punts compare unequal automatically.
  *
  * For callers who simply hold one complete numeric token in a buffer, however, whole-range
  * parsers are provided alongside: `parseDouble` and `parseFloat` read exactly one number
  * (Java/JSON-style: optional sign, digits with optional point, optional exponent, or the
  * `NaN`/`Infinity` literals -- no whitespace, no suffixes, no hex) spanning all of `[i0, iN)`
  * of a `String`, `Array[Byte]`, `Array[Char]`, `Mem[Byte]`, or `Mem[Char]`, with no allocation
  * on the fast path.  Failure -- bad syntax, or trailing junk within the range -- answers a
  * reserved quiet-NaN payload distinct from the canonical NaN that the literal `NaN` parses to;
  * test it with `failed` on the direct result (any arithmetic on a NaN, including passing it
  * through generic code, may recanonicalize it and lose the marker).
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


  //////////////////////////////////////
  /// Whole-range text parsing       ///
  //////////////////////////////////////

  private inline val failBitsD = 0x7FF800000000DEADL
  private inline val failBitsF = 0x7FC0DEAD

  /** The quiet NaN `parseDouble` answers on failure: same value as NaN, different payload. */
  val parseFailD: Double = java.lang.Double.longBitsToDouble(failBitsD)

  /** The quiet NaN `parseFloat` answers on failure: same value as NaN, different payload. */
  val parseFailF: Float = java.lang.Float.intBitsToFloat(failBitsF)

  /** Whether `d` is `parseDouble`'s failure marker (a parsed `"NaN"` is canonical and is not). */
  inline def failed(d: Double): Boolean = java.lang.Double.doubleToRawLongBits(d) == failBitsD

  /** Whether `f` is `parseFloat`'s failure marker (a parsed `"NaN"` is canonical and is not). */
  inline def failed(f: Float): Boolean = java.lang.Float.floatToRawIntBits(f) == failBitsF

  /** The parsing engine, templated over character access so each buffer type gets a monomorphic
    * instance: gather sign, literal, digits, and exponent; feed the (possibly 19-digit-truncated)
    * mantissa to the kernel; accept a truncated answer only when one ulp of mantissa slop agrees;
    * fall back to the JDK (via `sub`, the only allocation, roughly one case in 10^7) when the
    * kernel punts.  The whole of `[i0, iN)` must be exactly one number, which is what lets the
    * result stand alone -- no consumed-length side channel is needed.  In float mode the value
    * is answered exactly in a Double, with the Double failure marker; the public wrapper narrows.
    */
  private inline def parseImpl(inline at: Long => Int, inline sub: (Long, Long) => String)(i0: Long, iN: Long, asFloat: Boolean): Double =
    var j = i0
    var c = if j < iN then at(j) else -1
    var neg = false
    if c == '-' then
      neg = true
      j += 1
      c = if j < iN then at(j) else -1
    else if c == '+' then
      j += 1
      c = if j < iN then at(j) else -1
    if c == 'N' then
      if iN - j == 3 && at(j+1) == 'a' && at(j+2) == 'N' then Double.NaN
      else parseFailD
    else if c == 'I' then
      if iN - j == 8 && at(j+1)=='n' && at(j+2)=='f' && at(j+3)=='i' && at(j+4)=='n' && at(j+5)=='i' && at(j+6)=='t' && at(j+7)=='y' then
        if neg then Double.NegativeInfinity else Double.PositiveInfinity
      else parseFailD
    else
      var anyDigit = false
      while c == '0' do   // leading integer zeros are not significant
        anyDigit = true
        j += 1
        c = if j < iN then at(j) else -1
      var mant = 0L
      var nd = 0
      var droppedInt = 0
      var truncated = false
      while c >= '0' && c <= '9' && nd < 19 do
        mant = mant * 10 + (c - '0')   // 19 digits may wrap negative, but only into u64 space, which the kernel reads
        nd += 1
        anyDigit = true
        j += 1
        c = if j < iN then at(j) else -1
      while c >= '0' && c <= '9' do   // significance exhausted: count dropped integer digits
        droppedInt += 1
        if c != '0' then truncated = true
        j += 1
        c = if j < iN then at(j) else -1
      var fracScale = 0
      if c == '.' then
        j += 1
        c = if j < iN then at(j) else -1
        if nd == 0 then
          while c == '0' do   // leading fraction zeros scale the value but are not significant
            anyDigit = true
            fracScale += 1
            j += 1
            c = if j < iN then at(j) else -1
        while c >= '0' && c <= '9' && nd < 19 do
          mant = mant * 10 + (c - '0')
          nd += 1
          fracScale += 1
          anyDigit = true
          j += 1
          c = if j < iN then at(j) else -1
        while c >= '0' && c <= '9' do   // dropped fraction digits: only roundability matters
          if c != '0' then truncated = true
          j += 1
          c = if j < iN then at(j) else -1
      if !anyDigit then parseFailD
      else
        var e10 = droppedInt - fracScale
        if c == 'e' || c == 'E' then
          var k = j + 1
          var esign = false
          var cq = if k < iN then at(k) else -1
          if cq == '+' || cq == '-' then
            esign = cq == '-'
            k += 1
            cq = if k < iN then at(k) else -1
          if cq >= '0' && cq <= '9' then
            var ex = 0
            while cq >= '0' && cq <= '9' do
              if ex < 100000000 then ex = ex * 10 + (cq - '0')
              k += 1
              cq = if k < iN then at(k) else -1
            e10 += (if esign then -ex else ex)
            j = k
        if j != iN then parseFailD   // a malformed exponent strands its 'e' here and fails too
        else if mant == 0 then
          if neg then -0.0 else 0.0
        else if asFloat then
          val v = toFloat(ULong.wrap(mant), e10)
          val ok = if truncated then v == toFloat(ULong.wrap(mant + 1), e10) else v == v
          if ok then (if neg then -v else v).toDouble
          else java.lang.Float.parseFloat(sub(i0, iN)).toDouble
        else
          val v = toDouble(ULong.wrap(mant), e10)
          val ok = if truncated then v == toDouble(ULong.wrap(mant + 1), e10) else v == v
          if ok then (if neg then -v else v)
          else java.lang.Double.parseDouble(sub(i0, iN))

  private def parseStr(s: String, i0: Int, iN: Int, asFloat: Boolean): Double =
    parseImpl(j => s.charAt(j.toInt), (a, b) => s.substring(a.toInt, b.toInt))(i0, iN, asFloat)

  private def parseArrB(ab: Array[Byte], i0: Int, iN: Int, asFloat: Boolean): Double =
    parseImpl(j => ab(j.toInt) & 0xFF, (a, b) => new String(ab, a.toInt, (b - a).toInt, java.nio.charset.StandardCharsets.ISO_8859_1))(i0, iN, asFloat)

  private def parseArrC(ac: Array[Char], i0: Int, iN: Int, asFloat: Boolean): Double =
    parseImpl(j => ac(j.toInt), (a, b) => new String(ac, a.toInt, (b - a).toInt))(i0, iN, asFloat)

  private def parseMemB(mb: Mem[Byte], i0: Long, iN: Long, asFloat: Boolean): Double =
    parseImpl(
      j => mb.getB(j) & 0xFF,
      (a, b) =>
        val tmp = new Array[Byte]((b - a).toInt)
        var i = 0
        while i < tmp.length do
          tmp(i) = mb.getB(a + i)
          i += 1
        new String(tmp, java.nio.charset.StandardCharsets.ISO_8859_1)
    )(i0, iN, asFloat)

  private def parseMemC(mc: Mem[Char], i0: Long, iN: Long, asFloat: Boolean): Double =
    parseImpl(
      j => mc.getC(j),
      (a, b) =>
        val tmp = new Array[Char]((b - a).toInt)
        var i = 0
        while i < tmp.length do
          tmp(i) = mc.getC(a + i)
          i += 1
        new String(tmp)
    )(i0, iN, asFloat)

  private inline def narrow(d: Double): Float =
    if failed(d) then parseFailF else d.toFloat

  /** The `Double` whose decimal rendering occupies exactly `[i0, iN)` of `s`, correctly rounded;
    * `parseFailD` (test with `failed`) if that range is not exactly one number.
    */
  def parseDouble(s: String, i0: Int, iN: Int): Double = parseStr(s, i0, iN, false)

  /** The `Double` whose decimal rendering is exactly `s`, correctly rounded; `parseFailD` if not a number. */
  inline def parseDouble(s: String): Double = parseDouble(s, 0, s.length)
  /** As `parseDouble(s, i0, iN)`, with the range given as a literal or an `Iv.X` interval. */
  inline def parseDouble[R <: Iv.X | Rg](s: String, inline r: R): Double = Iv.dispatch(r, s)((i0, iN) => parseDouble(s, i0, iN))

  /** As the String `parseDouble`, over ASCII bytes. */
  def parseDouble(ab: Array[Byte], i0: Int, iN: Int): Double = parseArrB(ab, i0, iN, false)

  /** As the String `parseDouble`, over a whole ASCII byte array. */
  inline def parseDouble(ab: Array[Byte]): Double = parseDouble(ab, 0, ab.length)
  /** As `parseDouble(ab, i0, iN)`, with the range given as a literal or an `Iv.X` interval. */
  inline def parseDouble[R <: Iv.X | Rg](ab: Array[Byte], inline r: R): Double = Iv.dispatch(r, ab)((i0, iN) => parseDouble(ab, i0, iN))

  /** As the String `parseDouble`, over chars. */
  def parseDouble(ac: Array[Char], i0: Int, iN: Int): Double = parseArrC(ac, i0, iN, false)

  /** As the String `parseDouble`, over a whole char array. */
  inline def parseDouble(ac: Array[Char]): Double = parseDouble(ac, 0, ac.length)
  /** As `parseDouble(ac, i0, iN)`, with the range given as a literal or an `Iv.X` interval. */
  inline def parseDouble[R <: Iv.X | Rg](ac: Array[Char], inline r: R): Double = Iv.dispatch(r, ac)((i0, iN) => parseDouble(ac, i0, iN))

  /** As the String `parseDouble`, over ASCII bytes in memory. */
  @targetName("parseDoubleMemByte")
  def parseDouble(mb: Mem[Byte], i0: Long, iN: Long): Double = parseMemB(mb, i0, iN, false)

  /** As the String `parseDouble`, over all the ASCII bytes in memory. */
  @targetName("parseDoubleMemByteAll")
  inline def parseDouble(mb: Mem[Byte]): Double = parseDouble(mb, 0L, mb.length)

  /** As the String `parseDouble`, over chars in memory; positions index chars, not bytes. */
  @targetName("parseDoubleMemChar")
  def parseDouble(mc: Mem[Char], i0: Long, iN: Long): Double = parseMemC(mc, i0, iN, false)

  /** As the String `parseDouble`, over all the chars in memory. */
  @targetName("parseDoubleMemCharAll")
  inline def parseDouble(mc: Mem[Char]): Double = parseDouble(mc, 0L, mc.length)

  /** The `Float` whose decimal rendering occupies exactly `[i0, iN)` of `s`, correctly rounded
    * in one step (never double-rounded through a Double); `parseFailF` (test with `failed`) if
    * that range is not exactly one number.
    */
  def parseFloat(s: String, i0: Int, iN: Int): Float = narrow(parseStr(s, i0, iN, true))

  /** The `Float` whose decimal rendering is exactly `s`, correctly rounded; `parseFailF` if not a number. */
  inline def parseFloat(s: String): Float = parseFloat(s, 0, s.length)
  /** As `parseFloat(s, i0, iN)`, with the range given as a literal or an `Iv.X` interval. */
  inline def parseFloat[R <: Iv.X | Rg](s: String, inline r: R): Float = Iv.dispatch(r, s)((i0, iN) => parseFloat(s, i0, iN))

  /** As the String `parseFloat`, over ASCII bytes. */
  def parseFloat(ab: Array[Byte], i0: Int, iN: Int): Float = narrow(parseArrB(ab, i0, iN, true))

  /** As the String `parseFloat`, over a whole ASCII byte array. */
  inline def parseFloat(ab: Array[Byte]): Float = parseFloat(ab, 0, ab.length)
  /** As `parseFloat(ab, i0, iN)`, with the range given as a literal or an `Iv.X` interval. */
  inline def parseFloat[R <: Iv.X | Rg](ab: Array[Byte], inline r: R): Float = Iv.dispatch(r, ab)((i0, iN) => parseFloat(ab, i0, iN))

  /** As the String `parseFloat`, over chars. */
  def parseFloat(ac: Array[Char], i0: Int, iN: Int): Float = narrow(parseArrC(ac, i0, iN, true))

  /** As the String `parseFloat`, over a whole char array. */
  inline def parseFloat(ac: Array[Char]): Float = parseFloat(ac, 0, ac.length)
  /** As `parseFloat(ac, i0, iN)`, with the range given as a literal or an `Iv.X` interval. */
  inline def parseFloat[R <: Iv.X | Rg](ac: Array[Char], inline r: R): Float = Iv.dispatch(r, ac)((i0, iN) => parseFloat(ac, i0, iN))

  /** As the String `parseFloat`, over ASCII bytes in memory. */
  @targetName("parseFloatMemByte")
  def parseFloat(mb: Mem[Byte], i0: Long, iN: Long): Float = narrow(parseMemB(mb, i0, iN, true))

  /** As the String `parseFloat`, over all the ASCII bytes in memory. */
  @targetName("parseFloatMemByteAll")
  inline def parseFloat(mb: Mem[Byte]): Float = parseFloat(mb, 0L, mb.length)

  /** As the String `parseFloat`, over chars in memory; positions index chars, not bytes. */
  @targetName("parseFloatMemChar")
  def parseFloat(mc: Mem[Char], i0: Long, iN: Long): Float = narrow(parseMemC(mc, i0, iN, true))

  /** As the String `parseFloat`, over all the chars in memory. */
  @targetName("parseFloatMemCharAll")
  inline def parseFloat(mc: Mem[Char]): Float = parseFloat(mc, 0L, mc.length)
}
