// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.maths


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._



@RunWith(classOf[JUnit4])
class EiselLemireTest {
  import kse.basics.testutilities.TestUtilities.{_, given}
  import kse.basics.{given, _}
  import kse.flow.{_, given}
  import kse.maths.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  @Test
  def significandTableTest(): Unit =
    // Reference entries computed independently in Mathematica:
    //   E = Floor[q Log2[10]];  P = Floor[10^q 2^(127-E)]  printed as two 64-bit hex words
    def entry(q: Int): (Long, Long) = (EiselLemire.sig128(2*(q+342)), EiselLemire.sig128(2*(q+342)+1))
    T ~ entry(-342) ==== (0xEEF453D6923BD65AL, 0x113FAA2906A13B3FL)
    T ~ entry(-27)  ==== (0x9E74D1B791E07E48L, 0x775EA264CF55347DL)
    T ~ entry(-5)   ==== (0xA7C5AC471B478423L, 0x0FCF80DC33721D53L)
    T ~ entry(-1)   ==== (0xCCCCCCCCCCCCCCCCL, 0xCCCCCCCCCCCCCCCCL)   // truncated, not rounded up: the kernel needs floor
    T ~ entry(0)    ==== (0x8000000000000000L, 0x0000000000000000L)
    T ~ entry(1)    ==== (0xA000000000000000L, 0x0000000000000000L)
    T ~ entry(5)    ==== (0xC350000000000000L, 0x0000000000000000L)
    T ~ entry(27)   ==== (0xCECB8F27F4200F3AL, 0x0000000000000000L)
    T ~ entry(55)   ==== (0xD0CF4B50CFE20765L, 0xFFF4B4E3F741CF6DL)
    T ~ entry(308)  ==== (0x8E679C2F5E44FF8FL, 0x570F09EAA7EA7648L)
    // Every hi word is normalized (top bit set)
    var ok = true
    var k = 0
    while k < EiselLemire.sig128.length do
      if EiselLemire.sig128(k) >= 0 then ok = false
      k += 2
    T ~ ok ==== true
    // The exponent shortcut (q*108853) >> 15 == floor(q log2 10), checked against exact integer
    // math: bitLength(10^q) - 1 for q >= 0; 10^|q| is never a power of two, so floor of the
    // negative log is -bitLength(10^|q|)
    var q = -400
    while q <= 400 do
      val exact =
        if q >= 0 then java.math.BigInteger.TEN.pow(q).bitLength - 1
        else -java.math.BigInteger.TEN.pow(-q).bitLength
      T ~ ((q * 108853) >> 15) ==== exact
      q += 1

  @Test
  def knownValuesTest(): Unit =
    def d(w: Long, q: Int): Double = EiselLemire.toDouble(ULong.wrap(w), q)
    def f(w: Long, q: Int): Float = EiselLemire.toFloat(ULong.wrap(w), q)
    T ~ d(0L, 0)                       ==== 0.0
    T ~ d(0L, 5000)                    ==== 0.0
    T ~ d(1L, 0)                       ==== 1.0
    T ~ d(314159L, -5)                 ==== 3.14159                 // Clinger path
    T ~ d(31415926535897932L, -16)     ==== 3.141592653589793      // Eisel-Lemire path (17 digits)
    T ~ d(17976931348623157L, 292)     ==== Double.MaxValue
    T ~ d(22250738585072014L, -324)    ==== java.lang.Double.MIN_NORMAL
    T ~ d(1L, 309)                     ==== Double.PositiveInfinity // above the table: overflow clamp
    T ~ d(-1L, 309)                    ==== Double.PositiveInfinity // max u64 mantissa likewise
    T ~ d(1L, -343)                    ==== 0.0                     // below the table: underflow clamp
    T ~ d(9007199254740993L, 0).isNaN  ==== true    // 2^53 + 1: a true round-to-even midpoint punts
    T ~ d(9007199254740994L, 0)        ==== 9007199254740994.0      // 2^53 + 2 is exact
    T ~ d(5L, -324).isNaN              ==== true    // subnormal: punts
    T ~ d(-1L, 300).isNaN              ==== true    // (2^64-1)e300 ~ 1.8e319: overflow edge inside the table punts
    T ~ f(1L, 0)                       ==== 1.0f
    T ~ f(314159L, -5)                 ==== 3.14159f
    T ~ f(34028235L, 31)               ==== Float.MaxValue
    T ~ f(1L, 39)                      ==== Float.PositiveInfinity  // narrowing overflow rounds to infinity
    T ~ f(0L, 0)                       ==== 0.0f
    T ~ f(1L, -343)                    ==== 0.0f                    // Double underflow clamp maps to Float zero
    T ~ f(1L, -45).isNaN               ==== true    // Float subnormal range: punts
    // The classic narrowing trap: the correctly rounded Double of 7.038531e-26 sits exactly on
    // a Float midpoint, so blind narrowing is one ulp off; the kernel must punt instead.
    T ~ f(7038531L, -32).isNaN         ==== true
    T ~ java.lang.Float.parseFloat("7.038531e-26") ==== 7.038531e-26f

  @Test
  def differentialTest(): Unit =
    // Random mantissa/exponent pairs: whenever the kernel answers, it must agree exactly with
    // the JDK; in the exponent band with no subnormal or overflow edge, it must almost always answer.
    val r = new java.util.Random(0xE15E1L)
    var n = 0
    var punt = 0
    while n < 100000 do
      val w = r.nextLong() >>> r.nextInt(64)
      val q = -280 + r.nextInt(566)   // [-280, 285]: any w lands in normal Double territory
      val v = EiselLemire.toDouble(ULong.wrap(w), q)
      if v != v then punt += 1
      else T ~ v ==== java.lang.Double.parseDouble(java.lang.Long.toUnsignedString(w) + "e" + q)
      n += 1
    T ~ (punt < 100) ==== true   // midpoint/carry punts exist but are rare
    // Full range including the punting edges: answers must still be exact
    n = 0
    while n < 100000 do
      val w = r.nextLong() >>> r.nextInt(64)
      val q = -350 + r.nextInt(671)
      val v = EiselLemire.toDouble(ULong.wrap(w), q)
      if v == v && w != 0 then
        T ~ v ==== java.lang.Double.parseDouble(java.lang.Long.toUnsignedString(w) + "e" + q)
      n += 1
    // Float agreement over its own live range
    n = 0
    while n < 100000 do
      val w = r.nextLong() >>> r.nextInt(64)
      val q = -60 + r.nextInt(111)
      val v = EiselLemire.toFloat(ULong.wrap(w), q)
      if v == v && w != 0 then
        T ~ v ==== java.lang.Float.parseFloat(java.lang.Long.toUnsignedString(w) + "e" + q)
      n += 1

  // Parses `s` through every buffer form (String, Array[Char], Array[Byte], Mem[Byte],
  // Mem[Char], each embedded at an offset inside junk) and demands raw-bit agreement; the
  // shared answer comes back for value assertions.  Boxed-Double ==== canonicalizes NaN, so
  // failure-marker checks must go through EiselLemire.failed on this return value.
  var parseParityWorst: String = null
  def pd(s: String): Double =
    val d0 = EiselLemire.parseDouble(s)
    val emb = "x7" + s + "q"
    val ab = emb.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)
    val ac = emb.toCharArray
    val b0 = java.lang.Double.doubleToRawLongBits(d0)
    if parseParityWorst == null then
      val others = Array(
        EiselLemire.parseDouble(ac, 2, 2 + s.length),
        EiselLemire.parseDouble(ab, 2, 2 + s.length),
        EiselLemire.parseDouble(Mem of ab, 2L, (2 + s.length).toLong),
        EiselLemire.parseDouble(Mem of ac, 2L, (2 + s.length).toLong)
      )
      if others.exists(x => java.lang.Double.doubleToRawLongBits(x) != b0) then
        parseParityWorst = s"buffer forms disagree on \"$s\": ${others.mkString(", ")} vs $d0"
    d0
  def pf(s: String): Float =
    val f0 = EiselLemire.parseFloat(s)
    val emb = "x7" + s + "q"
    val ab = emb.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)
    val ac = emb.toCharArray
    val b0 = java.lang.Float.floatToRawIntBits(f0)
    if parseParityWorst == null then
      val others = Array(
        EiselLemire.parseFloat(ac, 2, 2 + s.length),
        EiselLemire.parseFloat(ab, 2, 2 + s.length),
        EiselLemire.parseFloat(Mem of ab, 2L, (2 + s.length).toLong),
        EiselLemire.parseFloat(Mem of ac, 2L, (2 + s.length).toLong)
      )
      if others.exists(x => java.lang.Float.floatToRawIntBits(x) != b0) then
        parseParityWorst = s"buffer forms disagree on \"$s\": ${others.mkString(", ")} vs $f0"
    f0

  @Test
  def parseKnownValuesTest(): Unit =
    T ~ pd("0")           ==== 0.0
    T ~ pd("-0")          ==== -0.0
    T ~ pd("+5")          ==== 5.0
    T ~ pd("5.")          ==== 5.0
    T ~ pd(".5")          ==== 0.5
    T ~ pd("-.5")         ==== -0.5
    T ~ pd("00.5e-3")     ==== 5e-4
    T ~ pd("123.456")     ==== 123.456
    T ~ pd("1e3")         ==== 1000.0
    T ~ pd("1E3")         ==== 1000.0
    T ~ pd("1e+3")        ==== 1000.0
    T ~ pd("1e-3")        ==== 0.001
    T ~ pd("5.e3")        ==== 5000.0
    T ~ pd("0.1")         ==== 0.1
    T ~ pd("3.141592653589793") ==== math.Pi
    T ~ pd("9007199254740993")  ==== 9007199254740992.0   // 2^53 + 1 rounds to even
    T ~ pd("1e400")       ==== Double.PositiveInfinity
    T ~ pd("-1e400")      ==== Double.NegativeInfinity
    T ~ pd("1e-400")      ==== 0.0
    T ~ pd("4.9e-324")    ==== Double.MinPositiveValue    // subnormal: kernel punts, fallback delivers
    T ~ pd("1.7976931348623157e308") ==== Double.MaxValue
    T ~ pd("123456789012345678901234567890") ==== 1.2345678901234568e29   // > 19 digits: truncation path
    T ~ pd("NaN").isNaN   ==== true
    T ~ pd("-NaN").isNaN  ==== true
    T ~ pd("Infinity")    ==== Double.PositiveInfinity
    T ~ pd("+Infinity")   ==== Double.PositiveInfinity
    T ~ pd("-Infinity")   ==== Double.NegativeInfinity
    // A parsed "NaN" is canonical, NOT the failure marker
    T ~ EiselLemire.failed(pd("NaN")) ==== false
    T ~ EiselLemire.parseFailD.isNaN  ==== true
    T ~ EiselLemire.failed(EiselLemire.parseFailD) ==== true
    // Whole-range enforcement and syntax failures
    for bad <- Array("", " ", "1 ", " 1", "1x", "x", "-", "+", ".", "-.", "e5", ".e5", "1e", "1e+",
                     "1..5", "1.2.3", "--1", "1,5", "NaNx", "-NaN2", "Infinit", "Infinityy", "0x1p3", "1f") do
      T ~ EiselLemire.failed(pd(bad)) ==== true
    // Range selection: exactly the number, junk on both sides
    val host = "abc123.456def"
    T ~ EiselLemire.parseDouble(host, 3, 10) ==== 123.456
    T ~ EiselLemire.failed(EiselLemire.parseDouble(host, 3, 11)) ==== true
    T ~ EiselLemire.failed(EiselLemire.parseDouble(host, 2, 10)) ==== true
    // Float versions
    T ~ pf("0")           ==== 0.0f
    T ~ pf("-0")          ==== -0.0f
    T ~ pf("0.1")         ==== 0.1f
    T ~ pf("3.14159")     ==== 3.14159f
    T ~ pf("1e40")        ==== Float.PositiveInfinity
    T ~ pf("-1e40")       ==== Float.NegativeInfinity
    T ~ pf("1e-50")       ==== 0.0f
    T ~ pf("1.4e-45")     ==== Float.MinPositiveValue     // float subnormal: punts to fallback
    T ~ pf("3.4028235e38") ==== Float.MaxValue
    T ~ pf("NaN").isNaN   ==== true
    T ~ EiselLemire.failed(pf("NaN")) ==== false
    T ~ EiselLemire.failed(pf("")) ==== true
    T ~ EiselLemire.failed(pf("1x")) ==== true
    T ~ EiselLemire.failed(EiselLemire.parseFailF) ==== true
    T ~ parseParityWorst ==== null

  @Test
  def parseFuzzTest(): Unit =
    val r = new java.util.Random(0x9A25EF00DL)
    var worst: String = null
    // Shortest renderings round-trip bit-identically through every buffer form
    var n = 0
    while n < 20000 do
      val d = (n % 3) match
        case 0 =>
          var x = java.lang.Double.longBitsToDouble(r.nextLong())
          while x.isNaN || x.isInfinite do x = java.lang.Double.longBitsToDouble(r.nextLong())
          x
        case 1 => (r.nextInt(2000001) - 1000000) * 0.001
        case _ => java.lang.Double.longBitsToDouble(r.nextLong() >>> 12)   // subnormals
      val s = kse.maths.Ryu.string(d)
      if worst == null && java.lang.Double.doubleToRawLongBits(pd(s)) != java.lang.Double.doubleToRawLongBits(d) then
        worst = s"double round trip failed: $d -> $s -> ${pd(s)}"
      val f = (n % 3) match
        case 0 =>
          var x = java.lang.Float.intBitsToFloat(r.nextInt())
          while x.isNaN || x.isInfinite do x = java.lang.Float.intBitsToFloat(r.nextInt())
          x
        case 1 => (r.nextInt(2000001) - 1000000) * 0.001f
        case _ => java.lang.Float.intBitsToFloat(r.nextInt() >>> 9)
      val t = kse.maths.Ryu.string(f)
      if worst == null && java.lang.Float.floatToRawIntBits(pf(t)) != java.lang.Float.floatToRawIntBits(f) then
        worst = s"float round trip failed: $f -> $t -> ${pf(t)}"
      n += 1
    // Long digit strings (deep truncation, midpoint stress, subnormal exponents): the JDK
    // parser is the oracle, and agreement must be exact whichever internal path answers
    n = 0
    while n < 20000 do
      val sb = new java.lang.StringBuilder
      if r.nextBoolean() then sb.append('-'): Unit
      val nd = 20 + r.nextInt(21)
      var i = 0
      while i < nd do
        sb.append(('0' + r.nextInt(10)).toChar): Unit
        i += 1
      sb.insert(sb.length - r.nextInt(nd), '.'): Unit
      val e = (n % 4) match
        case 0 => r.nextInt(61) - 30            // everyday
        case 1 => r.nextInt(41) - 330           // double subnormal / underflow edge
        case 2 => 290 + r.nextInt(40)           // overflow edge
        case _ => r.nextInt(31) - 55            // float subnormal territory
      sb.append('e'): Unit
      sb.append(e): Unit
      val s = sb.toString
      val jd = java.lang.Double.parseDouble(s)
      if worst == null && java.lang.Double.doubleToRawLongBits(pd(s)) != java.lang.Double.doubleToRawLongBits(jd) then
        worst = s"double oracle disagrees on $s: ${pd(s)} vs $jd"
      val jf = java.lang.Float.parseFloat(s)
      if worst == null && java.lang.Float.floatToRawIntBits(pf(s)) != java.lang.Float.floatToRawIntBits(jf) then
        worst = s"float oracle disagrees on $s: ${pf(s)} vs $jf"
      n += 1
    // Float narrowing midpoints: doubles exactly halfway between adjacent floats, printed
    // in full, must narrow by decimal (not double-rounded through the Double)
    n = 0
    while n < 20000 do
      var x = java.lang.Float.intBitsToFloat(r.nextInt())
      while x.isNaN || x.isInfinite || x == Float.MaxValue do x = java.lang.Float.intBitsToFloat(r.nextInt())
      val mid = (x.toDouble + java.lang.Math.nextUp(x).toDouble) / 2
      val s = new java.math.BigDecimal(mid).toPlainString
      val jf = java.lang.Float.parseFloat(s)
      if worst == null && java.lang.Float.floatToRawIntBits(pf(s)) != java.lang.Float.floatToRawIntBits(jf) then
        worst = s"float midpoint disagrees on $s: ${pf(s)} vs $jf"
      n += 1
    T ~ worst ==== null
    T ~ parseParityWorst ==== null
}
