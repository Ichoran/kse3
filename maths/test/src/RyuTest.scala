// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.maths


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import sourcecode.{Line, given}


@RunWith(classOf[JUnit4])
class RyuTest {
  import kse.testutilities.TestUtilities.{_, given}
  import kse.basics.{given, _}
  import kse.flow.{_, given}
  import kse.maths.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  val big1 = BigInt(1)

  def inv5big(q: Int): BigInt =
    (BigInt(Ryu.inv5(2*q)) << 64) | (BigInt(Ryu.inv5(2*q + 1)) & ((big1 << 64) - 1))
  def pow5big(i: Int): BigInt =
    (BigInt(Ryu.pow5(2*i)) << 64) | (BigInt(Ryu.pow5(2*i + 1)) & ((big1 << 64) - 1))

  @Test
  def tableTest(): Unit =
    // Reference entries computed independently in Mathematica:
    //   inv[q] = Floor[2^(BitLength[5^q]-1+125)/5^q] + 1
    //   pow[i] = If[BitLength[5^i] <= 125, 5^i 2^(125-BitLength[5^i]), Ceiling[5^i/2^(BitLength[5^i]-125)]]
    // each printed as two 64-bit hex words.
    def inv(q: Int): (Long, Long) = (Ryu.inv5(2*q), Ryu.inv5(2*q + 1))
    def pow(i: Int): (Long, Long) = (Ryu.pow5(2*i), Ryu.pow5(2*i + 1))
    T ~ inv(0)   ==== (0x2000000000000000L, 0x0000000000000001L)
    T ~ inv(1)   ==== (0x1999999999999999L, 0x999999999999999AL)
    T ~ inv(5)   ==== (0x14F8B588E368F084L, 0x61F9F01B866E43ABL)
    T ~ inv(27)  ==== (0x13CE9A36F23C0FC9L, 0x0EEBD44C99EAA690L)
    T ~ inv(55)  ==== (0x139DAE6F76D88307L, 0xAAA8C9BAD2D0AC0EL)
    T ~ inv(290) ==== (0x18F2B061AEA07183L, 0xBAB3BEB73DED4483L)
    T ~ pow(0)   ==== (0x1000000000000000L, 0x0000000000000000L)
    T ~ pow(1)   ==== (0x1400000000000000L, 0x0000000000000000L)
    T ~ pow(27)  ==== (0x19D971E4FE8401E7L, 0x4000000000000000L)
    T ~ pow(55)  ==== (0x1A19E96A19FC40ECL, 0xBFFE969C7EE839EEL)
    T ~ pow(325) ==== (0x18B40A4EEC437C52L, 0x78E1316E60A48311L)
    // Full tables against direct BigInt recomputation, and 125/126-bit normalization
    var ok = true
    var q = 0
    while q <= 290 do
      val p5 = BigInt(5).pow(q)
      if inv5big(q) != (big1 << (p5.bitLength - 1 + 125)) / p5 + 1 then ok = false
      val hi = Ryu.inv5(2*q)
      if hi < (1L << 60) || hi > (1L << 61) then ok = false
      q += 1
    var i = 0
    while i <= 325 do
      val p5 = BigInt(5).pow(i)
      val l = p5.bitLength
      val c = if l <= 125 then p5 << (125 - l) else ((p5 - 1) >> (l - 125)) + 1
      if pow5big(i) != c then ok = false
      val hi = Ryu.pow5(2*i)
      if hi < (1L << 60) || hi > (1L << 61) then ok = false
      i += 1
    T ~ ok ==== true
    // The fixed-point integer logs are exact across their working ranges: 2^e has
    // floor(e log10 2)+1 decimal digits, 5^e has floor(e log10 5)+1 decimal digits and
    // floor(e log2 5)+1 bits.
    var e = 0
    while e <= 1100 do
      if e <= 1000 then
        T ~ ((e * 78913) >>> 18) ==== BigInt(2).pow(e).toString.length - 1
      T ~ ((e * 732923) >>> 20) ==== BigInt(5).pow(e).toString.length - 1
      if e <= 350 then
        T ~ (((e * 1217359) >>> 19) + 1) ==== BigInt(5).pow(e).bitLength
      e += 1

  // The smallest nonzero value of (a*m) mod n over 1 <= m <= mMax, by the classic Euclid
  // record-walk: s is the least positive residue reached so far (by coefficient x) and t the
  // least gap below n (by coefficient y); combining witnesses reduces one against the other
  // until the coefficient budget runs out.  Cross-checked by brute force in euclidWalkTest.
  def minNonzero(a: BigInt, n: BigInt, mMax: BigInt): BigInt =
    var x = big1
    var s = a
    var y = big1
    var t = n - a
    var best = a
    var go = true
    while go do
      if s >= t then
        if t == 0 then go = false
        else
          val k = (s / t) min ((mMax - x) / y)
          if k <= 0 then go = false
          else
            x += k * y
            s -= k * t
            // An exact multiple is not a candidate, but the step just before it (value t,
            // witness x - y) is, and the batch above jumps over it.
            if s == 0 then { if t < best then best = t }
            else if s < best then best = s
      else
        if s == 0 then go = false
        else
          val k = (t / s) min ((mMax - y) / x)
          if k <= 0 then go = false
          else
            y += k * x
            t -= k * s
    best

  @Test
  def euclidWalkTest(): Unit =
    val r = new java.util.Random(0x5CA1AB1EL)
    var trial = 0
    while trial < 300 do
      val n = 2 + r.nextInt(2998)
      val a = 1 + r.nextInt(n - 1)
      val mMax = 1 + r.nextInt(600)
      var brute = Int.MaxValue
      var m = 1
      while m <= mMax do
        val v = (a.toLong * m % n).toInt
        if v != 0 && v < brute then brute = v
        m += 1
      T ~ minNonzero(BigInt(a), BigInt(n), BigInt(mMax)) ==== BigInt(brute)
      trial += 1
    // Budget edges: single coefficient, full wraparound, unit steps
    T ~ minNonzero(BigInt(7), BigInt(100), BigInt(1))   ==== BigInt(7)
    T ~ minNonzero(BigInt(7), BigInt(100), BigInt(100)) ==== BigInt(1)
    T ~ minNonzero(BigInt(99), BigInt(100), BigInt(50)) ==== BigInt(50)
    T ~ minNonzero(BigInt(50), BigInt(100), BigInt(9))  ==== BigInt(50)

  @Test
  def exactFloorProofTest(): Unit =
    // The kernel computes floor(m * 2^s / 5^q) (e2 >= 0) or floor(m * 5^i / 2^q) (e2 < 0) as
    // a 125-bit multiply-shift.  Both tables err high by construction, so each computed value
    // is floor(true + err) with 0 <= err small; the floor is exact iff no m pushes the true
    // fractional part within err of wrapping.  The worst m below 2^56 is found exactly by the
    // Euclid walk, for every reachable binary exponent -- this is a proof by exhaustion over
    // exponents, not a sampling test.
    val mMax = big1 << 56
    var e2 = 0
    while e2 <= 969 do
      var q = ((e2 * 78913) >>> 18) - 1
      if q < 0 then q = 0
      val s = e2 - q
      val p5 = BigInt(5).pow(q)
      val j = p5.bitLength - 1 + 125 - s
      T ~ (j >= 65 && j <= 127) ==== true
      val rho = inv5big(q) * p5 - (big1 << (p5.bitLength - 1 + 125))
      T ~ (rho > 0 && rho <= p5) ==== true
      // Need (m 2^s mod 5^q) * 2^j + m * rho < 5^q * 2^j for all m; residue-0 m need only
      // m * rho < 5^q 2^j, and the rest are safe iff the closest approach to 5^q beats the
      // worst error term.
      if q == 0 then T ~ (mMax * rho < (big1 << j)) ==== true
      else
        val g = BigInt(2).modPow(BigInt(s), p5)
        T ~ (mMax * rho < minNonzero(p5 - g, p5, mMax) * (big1 << j)) ==== true
      e2 += 1
    e2 = -1
    while e2 >= -1076 do
      var q = ((-e2 * 732923) >>> 20) - 1
      if q < 0 then q = 0
      val i = -e2 - q
      val p5 = BigInt(5).pow(i)
      val l = p5.bitLength
      val j = q - l + 125
      T ~ (j >= 65 && j <= 127) ==== true
      if l <= 125 then
        T ~ pow5big(i) ==== (p5 << (125 - l))   // exact table entry: nothing to prove
      else
        val tau = pow5big(i) * (big1 << (l - 125)) - p5
        T ~ (tau > 0 && tau < (big1 << (l - 125))) ==== true
        // Need (m 5^i mod 2^q) + m*tau < 2^q for all m: same shape as above with 2^j = 1.
        val n2 = big1 << q
        T ~ (mMax * tau < minNonzero(n2 - (p5 % n2), n2, mMax)) ==== true
      e2 -= 1

  @Test
  def knownValuesTest(): Unit =
    T ~ Ryu.string(0.0)                     ==== "0.0"
    T ~ Ryu.string(-0.0)                    ==== "-0.0"
    T ~ Ryu.string(Double.NaN)              ==== "NaN"
    T ~ Ryu.string(java.lang.Double.longBitsToDouble(0xFFF8000000000123L)) ==== "NaN"
    T ~ Ryu.string(Double.PositiveInfinity) ==== "Infinity"
    T ~ Ryu.string(Double.NegativeInfinity) ==== "-Infinity"
    T ~ Ryu.string(1.0)                     ==== "1.0"
    T ~ Ryu.string(-1.0)                    ==== "-1.0"
    T ~ Ryu.string(0.5)                     ==== "0.5"
    T ~ Ryu.string(1.5)                     ==== "1.5"
    T ~ Ryu.string(100.0)                   ==== "100.0"
    T ~ Ryu.string(123.456)                 ==== "123.456"
    T ~ Ryu.string(math.Pi)                 ==== "3.141592653589793"
    T ~ Ryu.string(0.001)                   ==== "0.001"
    T ~ Ryu.string(-0.001)                  ==== "-0.001"
    T ~ Ryu.string(1e-4)                    ==== "1.0e-4"
    T ~ Ryu.string(9999999.0)               ==== "9999999.0"
    T ~ Ryu.string(1e7)                     ==== "1.0e7"
    T ~ Ryu.string(1e23)                    ==== "1.0e23"
    T ~ Ryu.string(Double.MaxValue)         ==== "1.7976931348623157e308"
    T ~ Ryu.string(Double.MinPositiveValue) ==== "4.9e-324"
    T ~ Ryu.string(java.lang.Double.MIN_NORMAL) ==== "2.2250738585072014e-308"
    // The two-digit-quirk redo path, including the round-up-across-a-decade case
    T ~ Ryu.string(java.lang.Double.longBitsToDouble(2L)) ==== "9.9e-324"
    T ~ Ryu.string(java.lang.Double.longBitsToDouble(20L)) ==== "9.9e-323"
    // append writes at the given offset, leaves everything else alone, returns the end
    val b = Array.fill(24 + 6)('x'.toByte)
    val n = Ryu.append(b, 5, -123.456)
    T ~ n ==== 5 + 8
    T ~ (new String(b, 5, n - 5, java.nio.charset.StandardCharsets.ISO_8859_1)) ==== "-123.456"
    T ~ b(4) ==== 'x'.toByte
    T ~ b(n) ==== 'x'.toByte

  @Test
  def fmtKnownValuesTest(): Unit =
    T ~ Ryu.fmt(86.421, 2, -3)      ==== "86.4"    // the defining example: mag says tens, sig floor says 3 digits
    T ~ Ryu.fmt(86.421, 2, 0)       ==== "90"      // mag alone: shortest within +-5 ("86" would be equally legal)
    T ~ Ryu.fmt(86.421, 0, 2)       ==== "86"
    T ~ Ryu.fmt(86.421, 1, 0)       ==== "86"
    T ~ Ryu.fmt(86.421, -1, 0)      ==== "86.4"
    T ~ Ryu.fmt(86.421, 0, 0)       ==== "86.421"
    T ~ Ryu.fmt(86.421, 3, 0)       ==== "100"
    T ~ Ryu.fmt(1234.5, 3, 0)       ==== "1200"
    T ~ Ryu.fmt(0.29, 1, 0)         ==== "0"       // swallowed by the tolerance: unsigned zero
    T ~ Ryu.fmt(-0.29, 1, 0)        ==== "0"
    T ~ Ryu.fmt(123456.789, 0, 3)   ==== "123000"
    T ~ Ryu.fmt(9.6, 0, 1)          ==== "10"
    T ~ Ryu.fmt(12345.6789, -2, 0)  ==== "12345.68"
    T ~ Ryu.fmt(math.Pi, 0, 4)      ==== "3.142"
    T ~ Ryu.fmt(math.Pi, -2, 0)     ==== "3.14"
    T ~ Ryu.fmt(0.001999, 0, 1)     ==== "0.002"
    T ~ Ryu.fmt(0.000123456, -6, 0) ==== "1.23e-4"
    T ~ Ryu.fmt(-86.421, 2, -3)     ==== "-86.4"
    T ~ Ryu.fmt(86.0, 0, 0)         ==== "86"      // no cosmetic .0 in fmt
    T ~ Ryu.fmt(1e300, 0, 0)        ==== "1e300"
    T ~ Ryu.fmt(1e-300, 5, -3)      ==== "1e-300"  // sig floor rescues a mag cutoff far above the value
    T ~ Ryu.fmt(0.0, 7, 3)          ==== "0"
    T ~ Ryu.fmt(-0.0, 3, 0)         ==== "-0"
    T ~ Ryu.fmt(Double.NaN, 2, 2)   ==== "NaN"
    T ~ Ryu.fmt(Double.PositiveInfinity, 2, 2) ==== "Infinity"
    T ~ Ryu.fmt(Double.NegativeInfinity, 0, 0) ==== "-Infinity"
    // Byte, char, and String forms agree, at offset, leaving neighbors alone
    val bb = Array.fill(30)('x'.toByte)
    val cc = Array.fill(30)('x')
    val nb = Ryu.fmt(bb, 3, -86.421, 2, -3)
    val nc = Ryu.fmt(cc, 3, -86.421, 2, -3)
    T ~ nb ==== 3 + 5
    T ~ nc ==== 3 + 5
    T ~ (new String(bb, 3, nb - 3, java.nio.charset.StandardCharsets.ISO_8859_1)) ==== "-86.4"
    T ~ (new String(cc, 3, nc - 3)) ==== "-86.4"
    T ~ bb(2) ==== 'x'.toByte
    T ~ bb(nb) ==== 'x'.toByte
    T ~ cc(nc) ==== 'x'

  @Test
  def fmtToleranceTest(): Unit =
    // Independent model in exact BigDecimal arithmetic: the output must parse back either to
    // exactly d (when no cutoff bites) or to within half of the last-cared-about place; and a
    // positive sig bounds the significant digits of the output.
    val r = new java.util.Random(0xF337L)
    var worst: String = null
    var n = 0
    while n < 60000 do
      val d = (n % 3) match
        case 0 =>
          var x = java.lang.Double.longBitsToDouble(r.nextLong())
          while x.isNaN || x.isInfinite do x = java.lang.Double.longBitsToDouble(r.nextLong())
          x
        case 1 => (r.nextInt(2000001) - 1000000) * 0.001
        case _ => (r.nextDouble() - 0.5) * math.pow(10, r.nextInt(13) - 6)
      val mag = r.nextInt(41) - 20
      val sig = r.nextInt(14) - 5
      val s = Ryu.fmt(d, mag, sig)
      if worst == null then
        val bd = new java.math.BigDecimal(d)
        val lead = bd.abs.precision - bd.scale - 1
        var cut = Int.MinValue
        if mag != 0 then cut = if mag > 0 then mag - 1 else mag
        if sig > 0 then { val ps = lead - sig + 1; if ps > cut then cut = ps }
        else if sig < 0 && cut != Int.MinValue then { val pf = lead + sig + 1; if pf < cut then cut = pf }
        val pd = java.lang.Double.parseDouble(s)
        val exact = java.lang.Double.doubleToRawLongBits(pd) == java.lang.Double.doubleToRawLongBits(d)
        if cut == Int.MinValue then
          if !exact then worst = s"no-cutoff round trip failed: $d -> $s -> $pd"
        else if !exact then
          // Non-swallowed output is within tol exactly; the swallow-to-zero test floors the
          // scaled value, so a value up to (hu+1)/hu <= 51/50 past tol can still print as 0.
          val tol = new java.math.BigDecimal(java.math.BigInteger.valueOf(5), 1 - cut)   // 10^cut / 2
          val diff = new java.math.BigDecimal(s).subtract(bd).abs   // the decimal itself, not its re-parse
          if diff.compareTo(tol.multiply(new java.math.BigDecimal("1.03"))) > 0 then
            worst = s"tolerance exceeded: fmt($d, $mag, $sig) = $s (diff $diff > tol $tol)"
        if worst == null && sig > 0 && s != "0" then
          val digits = new java.math.BigDecimal(s).stripTrailingZeros.precision
          if digits > sig then worst = s"sig overflow: fmt($d, $mag, $sig) = $s has $digits digits"
        if worst == null && (n & 63) == 0 then
          val cb = new Array[Char](24)
          if s != new String(cb, 0, Ryu.fmt(cb, 0, d, mag, sig)) then worst = s"char form disagrees at $s"
      n += 1
    T ~ worst ==== null

  @Test
  def differentialTest(): Unit =
    // On JDK 19+ java.lang.Double.toString is itself the shortest round-tripping decimal, so
    // string equality is a complete oracle -- case-insensitively, since Ryu deliberately
    // prints a lowercase exponent letter; parse-back is asserted too since exact round-trip
    // is the primary contract.
    var mismatch: String = null
    var count = 0
    def check(d: Double): Unit =
      count += 1
      val mine = Ryu.string(d)
      if !mine.equalsIgnoreCase(java.lang.Double.toString(d)) && mismatch == null then
        mismatch = s"bits ${java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(d))}: jdk ${java.lang.Double.toString(d)} vs ryu $mine"
      if !d.isNaN && java.lang.Double.doubleToRawLongBits(java.lang.Double.parseDouble(mine)) != java.lang.Double.doubleToRawLongBits(d) && mismatch == null then
        mismatch = s"round trip failure: $d vs $mine"
    var e = -1074
    while e <= 1023 do
      val d = java.lang.Math.scalb(1.0, e)
      check(d); check(-d); check(java.lang.Math.nextUp(d)); check(java.lang.Math.nextDown(d))
      e += 1
    e = -323
    while e <= 308 do
      val d = java.lang.Double.parseDouble("1e" + e)
      check(d); check(java.lang.Math.nextUp(d)); check(java.lang.Math.nextDown(d))
      e += 1
    var m = 1
    while m <= 30000 do
      check(m.toDouble); check(1.0 / m); check(-m * 0.001)
      m += 1
    var k = -30
    while k <= 30 do
      m = 1
      while m <= 500 do
        val d = java.lang.Double.parseDouble(s"${m}e$k")
        check(d); check(java.lang.Math.nextUp(d)); check(java.lang.Math.nextDown(d))
        m += 1
      k += 1
    // Mantissa trailing-zero structure across every exponent (the exactness-flag paths)
    val r = new java.util.Random(0x12BADA55L)
    var ieeeE = 0
    while ieeeE <= 2046 do
      check(java.lang.Double.longBitsToDouble(ieeeE.toLong << 52))
      var tz = 0
      while tz <= 52 do
        val mm = ((r.nextLong() | 1L) << tz) & 0xFFFFFFFFFFFFFL
        check(java.lang.Double.longBitsToDouble((ieeeE.toLong << 52) | mm))
        tz += 5
      ieeeE += 1
    // Mantissas divisible by powers of 5 (exact decimal boundaries)
    var p5 = 5L
    while p5 <= 2384185791015625L do   // 5^22
      var trial = 0
      while trial < 200 do
        val lo = (1L << 52) / p5 + 1
        val m2 = (lo + r.nextLong().abs % ((1L << 53) / p5 - lo)) * p5
        if (m2 >>> 52) == 1L then
          val bits = ((1 + r.nextInt(2046)).toLong << 52) | (m2 & 0xFFFFFFFFFFFFFL)
          val d = java.lang.Double.longBitsToDouble(bits)
          check(d); check(java.lang.Math.nextUp(d)); check(java.lang.Math.nextDown(d))
        trial += 1
      p5 *= 5
    // Random bit patterns, including subnormals
    var i = 0
    while i < 1000000 do
      val d = java.lang.Double.longBitsToDouble(r.nextLong())
      if !d.isNaN then check(d)
      check(java.lang.Double.longBitsToDouble(r.nextLong() >>> 12))
      i += 1
    T ~ mismatch ==== null
    T ~ (count > 2000000) ==== true

  @Test
  def floatKnownValuesTest(): Unit =
    T ~ Ryu.string(0.0f)                    ==== "0.0"
    T ~ Ryu.string(-0.0f)                   ==== "-0.0"
    T ~ Ryu.string(Float.NaN)               ==== "NaN"
    T ~ Ryu.string(java.lang.Float.intBitsToFloat(0xFFC00123)) ==== "NaN"
    T ~ Ryu.string(Float.PositiveInfinity)  ==== "Infinity"
    T ~ Ryu.string(Float.NegativeInfinity)  ==== "-Infinity"
    T ~ Ryu.string(1.0f)                    ==== "1.0"
    T ~ Ryu.string(-1.0f)                   ==== "-1.0"
    T ~ Ryu.string(0.1f)                    ==== "0.1"
    T ~ Ryu.string(1.5f)                    ==== "1.5"
    T ~ Ryu.string(100.0f)                  ==== "100.0"
    T ~ Ryu.string(3.14159f)                ==== "3.14159"
    T ~ Ryu.string(0.001f)                  ==== "0.001"
    T ~ Ryu.string(-0.001f)                 ==== "-0.001"
    T ~ Ryu.string(1e-4f)                   ==== "1.0e-4"
    T ~ Ryu.string(9999999.0f)              ==== "9999999.0"
    T ~ Ryu.string(1e7f)                    ==== "1.0e7"
    T ~ Ryu.string(Float.MaxValue)          ==== "3.4028235e38"
    T ~ Ryu.string(Float.MinPositiveValue)  ==== "1.4e-45"
    T ~ Ryu.string(java.lang.Float.MIN_NORMAL) ==== "1.1754944e-38"
    // The two-digit-quirk redo path in Float's subnormal basement
    T ~ Ryu.string(java.lang.Float.intBitsToFloat(2)) ==== "2.8e-45"
    // append writes at the given offset, leaves everything else alone, returns the end
    val b = Array.fill(16 + 6)('x'.toByte)
    val n = Ryu.append(b, 5, -123.456f)
    T ~ n ==== 5 + 8
    T ~ (new String(b, 5, n - 5, java.nio.charset.StandardCharsets.ISO_8859_1)) ==== "-123.456"
    T ~ b(4) ==== 'x'.toByte
    T ~ b(n) ==== 'x'.toByte

  @Test
  def floatDifferentialTest(): Unit =
    // Same oracle as the Double differential test: on JDK 19+, Float.toString is the shortest
    // round-tripping decimal, so case-insensitive equality plus exact parse-back is complete.
    var mismatch: String = null
    var count = 0
    def check(f: Float): Unit =
      count += 1
      val mine = Ryu.string(f)
      if !mine.equalsIgnoreCase(java.lang.Float.toString(f)) && mismatch == null then
        mismatch = s"bits ${Integer.toHexString(java.lang.Float.floatToRawIntBits(f))}: jdk ${java.lang.Float.toString(f)} vs ryu $mine"
      if !f.isNaN && java.lang.Float.floatToRawIntBits(java.lang.Float.parseFloat(mine)) != java.lang.Float.floatToRawIntBits(f) && mismatch == null then
        mismatch = s"round trip failure: $f vs $mine"
    var e = -149
    while e <= 127 do
      val f = java.lang.Math.scalb(1.0f, e)
      check(f); check(-f); check(java.lang.Math.nextUp(f)); check(java.lang.Math.nextDown(f))
      e += 1
    e = -45
    while e <= 38 do
      val f = java.lang.Float.parseFloat("1e" + e)
      check(f); check(java.lang.Math.nextUp(f)); check(java.lang.Math.nextDown(f))
      e += 1
    var m = 1
    while m <= 30000 do
      check(m.toFloat); check(1.0f / m); check(-m * 0.001f)
      m += 1
    var k = -30
    while k <= 30 do
      m = 1
      while m <= 500 do
        val f = java.lang.Float.parseFloat(s"${m}e$k")
        check(f); check(java.lang.Math.nextUp(f)); check(java.lang.Math.nextDown(f))
        m += 1
      k += 1
    // Mantissa trailing-zero structure across every exponent (the exactness-flag paths)
    val r = new java.util.Random(0x12BADA55L)
    var ieeeE = 0
    while ieeeE <= 254 do
      check(java.lang.Float.intBitsToFloat(ieeeE << 23))
      var tz = 0
      while tz <= 23 do
        val mm = ((r.nextInt() | 1) << tz) & 0x7FFFFF
        check(java.lang.Float.intBitsToFloat((ieeeE << 23) | mm))
        tz += 2
      ieeeE += 1
    // Mantissas divisible by powers of 5 (exact decimal boundaries)
    var p5 = 5
    while p5 <= 1953125 do   // 5^9; 5^10 exceeds the 23-bit mantissa
      var trial = 0
      while trial < 200 do
        val lo = (1 << 23) / p5 + 1
        val m2 = (lo + r.nextInt(((1 << 24) / p5 - lo) max 1)) * p5
        if (m2 >>> 23) == 1 then
          val bits = ((1 + r.nextInt(254)) << 23) | (m2 & 0x7FFFFF)
          val f = java.lang.Float.intBitsToFloat(bits)
          check(f); check(java.lang.Math.nextUp(f)); check(java.lang.Math.nextDown(f))
        trial += 1
      p5 *= 5
    // Random bit patterns, including subnormals
    var i = 0
    while i < 1000000 do
      val f = java.lang.Float.intBitsToFloat(r.nextInt())
      if !f.isNaN then check(f)
      check(java.lang.Float.intBitsToFloat(r.nextInt() >>> 9))
      i += 1
    T ~ mismatch ==== null
    T ~ (count > 2000000) ==== true

  @Test
  def floatExhaustiveTest(): Unit =
    // Every single Float bit pattern against the JDK -- the gold standard, but a few minutes
    // of work, so it only runs when asked for: -Dkse.ryu.exhaustive (run it after any change
    // to the kernel).  The sampled differential test above covers the same paths by structure.
    if System.getProperty("kse.ryu.exhaustive") != null then
      val mismatch = new java.util.concurrent.atomic.AtomicReference[String](null)
      val next = new java.util.concurrent.atomic.AtomicInteger(0)
      val threads = Array.fill(Runtime.getRuntime.availableProcessors max 1)(new Thread(() => {
        var chunk = next.getAndIncrement()
        while chunk < 4096 && mismatch.get() == null do
          var i = chunk.toLong << 20
          val iN = i + (1L << 20)
          while i < iN do
            val bits = i.toInt
            val f = java.lang.Float.intBitsToFloat(bits)
            val mine = Ryu.string(f)
            if !mine.equalsIgnoreCase(java.lang.Float.toString(f)) then
              val _ = mismatch.compareAndSet(null, s"bits ${Integer.toHexString(bits)}: jdk ${java.lang.Float.toString(f)} vs ryu $mine")
            else if !f.isNaN && java.lang.Float.floatToRawIntBits(java.lang.Float.parseFloat(mine)) != bits then
              val _ = mismatch.compareAndSet(null, s"round trip failure at bits ${Integer.toHexString(bits)}: $mine")
            i += 1
          chunk = next.getAndIncrement()
      }))
      threads.foreach(_.start())
      threads.foreach(_.join())
      T ~ mismatch.get() ==== null

  @Test
  def floatFmtToleranceTest(): Unit =
    // The Float twin of fmtToleranceTest: exact BigDecimal model (a Float widens exactly),
    // parse-back must be bit-identical when no cutoff bites, within the tolerance otherwise.
    val r = new java.util.Random(0xF337F10AL)
    var worst: String = null
    var n = 0
    while n < 60000 do
      val f = (n % 3) match
        case 0 =>
          var x = java.lang.Float.intBitsToFloat(r.nextInt())
          while x.isNaN || x.isInfinite do x = java.lang.Float.intBitsToFloat(r.nextInt())
          x
        case 1 => (r.nextInt(2000001) - 1000000) * 0.001f
        case _ => ((r.nextFloat() - 0.5f) * math.pow(10, r.nextInt(13) - 6)).toFloat
      val mag = r.nextInt(31) - 15
      val sig = r.nextInt(12) - 4
      val s = Ryu.fmt(f, mag, sig)
      if worst == null then
        val bd = new java.math.BigDecimal(f.toDouble)
        val lead = bd.abs.precision - bd.scale - 1
        var cut = Int.MinValue
        if mag != 0 then cut = if mag > 0 then mag - 1 else mag
        if sig > 0 then { val ps = lead - sig + 1; if ps > cut then cut = ps }
        else if sig < 0 && cut != Int.MinValue then { val pf = lead + sig + 1; if pf < cut then cut = pf }
        val pf = java.lang.Float.parseFloat(s)
        val exact = java.lang.Float.floatToRawIntBits(pf) == java.lang.Float.floatToRawIntBits(f)
        if cut == Int.MinValue then
          if !exact then worst = s"no-cutoff round trip failed: $f -> $s -> $pf"
        else if !exact then
          val tol = new java.math.BigDecimal(java.math.BigInteger.valueOf(5), 1 - cut)   // 10^cut / 2
          val diff = new java.math.BigDecimal(s).subtract(bd).abs
          if diff.compareTo(tol.multiply(new java.math.BigDecimal("1.03"))) > 0 then
            worst = s"tolerance exceeded: fmt($f, $mag, $sig) = $s (diff $diff > tol $tol)"
        if worst == null && sig > 0 && s != "0" then
          val digits = new java.math.BigDecimal(s).stripTrailingZeros.precision
          if digits > sig then worst = s"sig overflow: fmt($f, $mag, $sig) = $s has $digits digits"
      n += 1
    T ~ worst ==== null

  @Test
  def targetParityTest(): Unit =
    // Every render target must produce identical characters: Array[Byte] is the reference,
    // and Array[Char], Mem[Byte], Mem[Char], MkStr (appended after existing content), and the
    // String forms must all agree, at nonzero offsets, for append and fmt, Double and Float.
    val r = new java.util.Random(0x7A26E7L)
    val ab = new Array[Byte](48)
    val ac = new Array[Char](48)
    val amb = new Array[Byte](48)
    val amc = new Array[Char](48)
    val mb = Mem of amb
    val mc = Mem of amc
    var worst: String = null
    def checkD(d: Double, mag: Int, sig: Int, full: Boolean): Unit =
      val at = r.nextInt(8)
      val nb = if full then Ryu.append(ab, at, d) else Ryu.fmt(ab, at, d, mag, sig)
      val s = new String(ab, at, nb - at, java.nio.charset.StandardCharsets.ISO_8859_1)
      val nc = if full then Ryu.append(ac, at, d) else Ryu.fmt(ac, at, d, mag, sig)
      val sc = new String(ac, at, nc - at)
      val nmb = if full then Ryu.append(mb, at.toLong, d) else Ryu.fmt(mb, at.toLong, d, mag, sig)
      val smb = new String(amb, at, (nmb - at).toInt, java.nio.charset.StandardCharsets.ISO_8859_1)
      val nmc = if full then Ryu.append(mc, at.toLong, d) else Ryu.fmt(mc, at.toLong, d, mag, sig)
      val smc = new String(amc, at, (nmc - at).toInt)
      val ms = MkStr.ofSize(32)
      ms += "p:"
      if full then Ryu.append(ms, d) else Ryu.fmt(ms, d, mag, sig)
      val sms = ms.str()
      val ss = if full then Ryu.string(d) else Ryu.fmt(d, mag, sig)
      if worst == null then
        if      sc != s          then worst = s"chars $sc != $s for $d"
        else if smb != s         then worst = s"mem bytes $smb != $s for $d"
        else if smc != s         then worst = s"mem chars $smc != $s for $d"
        else if sms != ("p:" + s) then worst = s"mkstr $sms != p:$s for $d"
        else if ss != s          then worst = s"string $ss != $s for $d"
    def checkF(f: Float, mag: Int, sig: Int, full: Boolean): Unit =
      val at = r.nextInt(8)
      val nb = if full then Ryu.append(ab, at, f) else Ryu.fmt(ab, at, f, mag, sig)
      val s = new String(ab, at, nb - at, java.nio.charset.StandardCharsets.ISO_8859_1)
      val nc = if full then Ryu.append(ac, at, f) else Ryu.fmt(ac, at, f, mag, sig)
      val sc = new String(ac, at, nc - at)
      val nmb = if full then Ryu.append(mb, at.toLong, f) else Ryu.fmt(mb, at.toLong, f, mag, sig)
      val smb = new String(amb, at, (nmb - at).toInt, java.nio.charset.StandardCharsets.ISO_8859_1)
      val nmc = if full then Ryu.append(mc, at.toLong, f) else Ryu.fmt(mc, at.toLong, f, mag, sig)
      val smc = new String(amc, at, (nmc - at).toInt)
      val ms = MkStr.ofSize(32)
      ms += "p:"
      if full then Ryu.append(ms, f) else Ryu.fmt(ms, f, mag, sig)
      val sms = ms.str()
      val ss = if full then Ryu.string(f) else Ryu.fmt(f, mag, sig)
      if worst == null then
        if      sc != s          then worst = s"chars $sc != $s for $f"
        else if smb != s         then worst = s"mem bytes $smb != $s for $f"
        else if smc != s         then worst = s"mem chars $smc != $s for $f"
        else if sms != ("p:" + s) then worst = s"mkstr $sms != p:$s for $f"
        else if ss != s          then worst = s"string $ss != $s for $f"
    for d <- Array(0.0, -0.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity,
                   Double.MaxValue, Double.MinPositiveValue, java.lang.Double.MIN_NORMAL,
                   1.0, -1.0, 123.456, 1e-4, 9999999.0, 1e7, 0.001, 86.421) do
      checkD(d, 0, 0, full = true)
      checkD(d, 0, 0, full = false)
      checkD(d, 2, -3, full = false)
      checkD(d, 1, 0, full = false)   // swallow case renders "0" everywhere
    for f <- Array(0.0f, -0.0f, Float.NaN, Float.PositiveInfinity, Float.NegativeInfinity,
                   Float.MaxValue, Float.MinPositiveValue, java.lang.Float.MIN_NORMAL,
                   1.0f, -1.0f, 123.456f, 1e-4f, 9999999.0f, 1e7f, 0.001f, 86.421f) do
      checkF(f, 0, 0, full = true)
      checkF(f, 0, 0, full = false)
      checkF(f, 2, -3, full = false)
      checkF(f, 1, 0, full = false)
    var n = 0
    while n < 20000 do
      val d = (n % 3) match
        case 0 =>
          var x = java.lang.Double.longBitsToDouble(r.nextLong())
          while x.isNaN || x.isInfinite do x = java.lang.Double.longBitsToDouble(r.nextLong())
          x
        case 1 => (r.nextInt(2000001) - 1000000) * 0.001
        case _ => (r.nextDouble() - 0.5) * math.pow(10, r.nextInt(17) - 8)
      checkD(d, 0, 0, full = true)
      checkD(d, r.nextInt(41) - 20, r.nextInt(14) - 5, full = false)
      val f = (n % 3) match
        case 0 =>
          var x = java.lang.Float.intBitsToFloat(r.nextInt())
          while x.isNaN || x.isInfinite do x = java.lang.Float.intBitsToFloat(r.nextInt())
          x
        case 1 => (r.nextInt(2000001) - 1000000) * 0.001f
        case _ => ((r.nextFloat() - 0.5f) * math.pow(10, r.nextInt(13) - 6)).toFloat
      checkF(f, 0, 0, full = true)
      checkF(f, r.nextInt(31) - 15, r.nextInt(12) - 4, full = false)
      n += 1
    T ~ worst ==== null
}
