// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.maths


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import sourcecode.{Line, given}


@RunWith(classOf[JUnit4])
class EiselLemireTest {
  import kse.testutilities.TestUtilities.{_, given}
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
}
