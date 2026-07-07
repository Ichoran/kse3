// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.eio


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import sourcecode.{Line, given}


@RunWith(classOf[JUnit4])
class GrokTest {
  import kse.testutilities.TestUtilities.{given, _}
  import kse.basics.{given, _}
  import kse.flow.{given, _}
  import kse.maths.{given, _}
  import kse.eio.{given, _}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  private def bad[A](ask: Ask[A]): Boolean = ask match
    case Alt(_) => true
    case _ => false

  private def errText[A](ask: Ask[A]): String = ask match
    case Alt(e) => e.toString
    case _ => "SUCCESS: " + ask.toString

  @Test
  def grokIntegerTest(): Unit =
    T ~ Grok("2025-06-25", partial = true)(g => (g.I, (g < '-').I, (g < '-').I)) ==== Is((2025, 6, 25))
    T ~ Grok("10\n20")(g => g.I + g.I)                                           ==== Is(30)
    T ~ Grok("10 20 -5", delim = Delim.white)(g => g.I + g.I + g.I)              ==== Is(25)
    T ~ Grok("9223372036854775807")(g => g.L)                                    ==== Is(Long.MaxValue)
    T ~ Grok("-9223372036854775808")(g => g.L)                                   ==== Is(Long.MinValue)
    T ~ Grok("+42")(g => g.I)                                                    ==== Is(42)
    T ~ bad(Grok("9223372036854775808")(g => g.L))                               ==== true
    T ~ bad(Grok("06b", partial = false)(g => g.I))                              ==== true
    T ~ Grok("06b", partial = true)(g => g.I)                                    ==== Is(6)
    T ~ errText(Grok("3000000000")(g => g.I)).contains("out of Int range")       ==== true
    T ~ errText(Grok("99999999999999999999")(g => g.L)).contains("out of Long range") ==== true

  @Test
  def grokErrorDataTest(): Unit =
    Grok("2025-06b-25", partial = true)(g => (g.I, (g < '-').I, (g < '-').I)) match
      case Alt(e) => e.underlying match
        case f: Grok.Failure =>
          T ~ f.position                              ==== 7L
          T ~ f.line                                  ==== 1
          T ~ f.column                                ==== 8
          T ~ f.description.contains("expected '-'")  ==== true
          T ~ f.description.contains("found 'b'")     ==== true
          T ~ f.message.contains("line 1, pos 8")     ==== true
        case u => assertTrue("not a Grok.Failure: " + u, false)
      case v => assertTrue("unexpected success: " + v, false)
    Grok("one\ntwo\nth?ee")(g => { g.skip(2); g.I }) match
      case Alt(e) => e.underlying match
        case f: Grok.Failure =>
          T ~ f.line   ==== 3
          T ~ f.column ==== 1
        case u => assertTrue("not a Grok.Failure: " + u, false)
      case v => assertTrue("unexpected success: " + v, false)

  @Test
  def grokDoubleTest(): Unit =
    T ~ Grok("3.14159")(g => g.D)                 ==== Is(3.14159)
    T ~ Grok("0.1")(g => g.D)                     ==== Is(0.1)
    T ~ Grok("-2.5e-4")(g => g.D)                 ==== Is(-2.5e-4)
    T ~ Grok("1e300")(g => g.D)                   ==== Is(1e300)
    T ~ Grok("9007199254740993")(g => g.D)        ==== Is("9007199254740993".toDouble)
    T ~ Grok("1.7976931348623157e308")(g => g.D)  ==== Is(Double.MaxValue)
    T ~ Grok("0.000001234")(g => g.D)             ==== Is(0.000001234)
    T ~ Grok("42")(g => g.D)                      ==== Is(42.0)
    T ~ Grok("NaN")(g => g.D.isNaN)               ==== Is(true)
    T ~ Grok("-Infinity")(g => g.D)               ==== Is(Double.NegativeInfinity)
    T ~ bad(Grok("1.5x")(g => g.D))               ==== true
    T ~ Grok("1.5x", partial = true)(g => g.D)    ==== Is(1.5)
    T ~ bad(Grok("eel")(g => g.D))                ==== true
    T ~ Grok("3.25e2 x", delim = Delim.white)(g => (g.Dspan, g.tok)) ==== Is((6, "x"))

  @Test
  def grokFloatTest(): Unit =
    T ~ Grok("1.5")(g => g.F)             ==== Is(1.5f)
    T ~ Grok("-2.25")(g => g.F)           ==== Is(-2.25f)
    T ~ Grok("3.14159265")(g => g.F)      ==== Is(3.14159265f)
    T ~ Grok("6.02214076e23")(g => g.F)   ==== Is(6.02214076e23f)
    T ~ Grok("7.038531e-26")(g => g.F)    ==== Is(7.038531e-26f)   // (double parse).toFloat is 1 ulp off: needs the true Float path
    T ~ Grok("16777217")(g => g.F)        ==== Is(16777217f)
    T ~ Grok("1e10")(g => g.F)            ==== Is(1e10f)
    T ~ Grok("1e-10")(g => g.F)           ==== Is(1e-10f)
    T ~ Grok("1e50")(g => g.F)            ==== Is(Float.PositiveInfinity)
    T ~ Grok("9.1093837015e-31")(g => g.F) ==== Is(9.1093837015e-31f)
    T ~ Grok("42")(g => g.F)              ==== Is(42.0f)
    T ~ Grok("NaN")(g => g.F.isNaN)       ==== Is(true)
    T ~ Grok("-Infinity")(g => g.F)       ==== Is(Float.NegativeInfinity)
    T ~ bad(Grok("1.5x")(g => g.F))       ==== true
    T ~ Grok("1.5x", partial = true)(g => g.F) ==== Is(1.5f)
    T ~ bad(Grok("eel")(g => g.F))        ==== true
    val checks = Array(
      "0.1", "123456.789", "0.000001234", "1234567890123456789.5", "3.4028235e38", "3.4028236e38",
      "1.17549435e-38", "1.4e-45", "7e-46", "-0.0", "8388608.5", "0.49999999999999999"
    )
    var k = 0
    while k < checks.length do
      T ~ Grok(checks(k))(g => g.F) ==== Is(java.lang.Float.parseFloat(checks(k)))
      k += 1
    T ~ Grok("1.5 -1.0", delim = Delim.white)(g => g.F_?(_ > 0f))       ==== Is(1.5f)
    T ~ bad(Grok("-1.0")(g => g.F_?(_ > 0f)))                           ==== true

  @Test
  def grokSizedIntegerTest(): Unit =
    T ~ Grok("100 -100", delim = Delim.white)(g => (g.B, g.B))          ==== Is((100.toByte, (-100).toByte))
    T ~ Grok("127 -128", delim = Delim.white)(g => (g.B, g.B))          ==== Is((Byte.MaxValue, Byte.MinValue))
    T ~ bad(Grok("128")(g => g.B))                                      ==== true
    T ~ bad(Grok("-129")(g => g.B))                                     ==== true
    T ~ Grok("32767 -32768", delim = Delim.white)(g => (g.S, g.S))      ==== Is((Short.MaxValue, Short.MinValue))
    T ~ bad(Grok("32768")(g => g.S))                                    ==== true
    T ~ bad(Grok("-32769")(g => g.S))                                   ==== true
    T ~ Grok("000127")(g => g.B)                                        ==== Is(127.toByte)
    T ~ bad(Grok("1234", partial = true)(g => g.B))                     ==== true
    T ~ errText(Grok("12345678901234567890123456789")(g => g.B)).contains("Byte range")  ==== true
    T ~ errText(Grok("12345678901234567890123456789")(g => g.S)).contains("Short range") ==== true
    T ~ errText(Grok("-999999999999999999999999")(g => g.B)).contains("Byte range")      ==== true
    T ~ Grok("7 8", delim = Delim.white)(g => g.B_?(_ % 2 == 1))        ==== Is(7.toByte)
    T ~ bad(Grok("8")(g => g.B_?(_ % 2 == 1)))                          ==== true
    T ~ Grok("300")(g => g.S_?(_ > 200))                                ==== Is(300.toShort)
    T ~ bad(Grok("100")(g => g.S_?(_ > 200)))                           ==== true

  @Test
  def grokUnsignedTest(): Unit =
    T ~ Grok("255")(g => g.uB)                          ==== Is(UByte.wrap(-1))
    T ~ Grok("0")(g => g.uB)                            ==== Is(UByte.wrap(0))
    T ~ bad(Grok("256")(g => g.uB))                     ==== true
    T ~ bad(Grok("-1")(g => g.uB))                      ==== true
    T ~ Grok("65535")(g => g.uS)                        ==== Is(UShort.wrap(-1))
    T ~ bad(Grok("65536")(g => g.uS))                   ==== true
    T ~ Grok("4294967295")(g => g.uI)                   ==== Is(UInt.wrap(-1))
    T ~ bad(Grok("4294967296")(g => g.uI))              ==== true
    T ~ Grok("18446744073709551615")(g => g.uL)         ==== Is(ULong.wrap(-1L))
    T ~ Grok("18446744073709551610")(g => g.uL)         ==== Is(ULong.wrap(-6L))
    T ~ Grok("9223372036854775808")(g => g.uL)          ==== Is(ULong.wrap(Long.MinValue))
    T ~ bad(Grok("18446744073709551616")(g => g.uL))    ==== true
    T ~ bad(Grok("99999999999999999999999")(g => g.uL)) ==== true
    T ~ Grok("+123")(g => g.uL)                         ==== Is(ULong.wrap(123L))
    T ~ bad(Grok("-0")(g => g.uL))                      ==== true
    T ~ bad(Grok("-5")(g => g.uL))                      ==== true
    T ~ Grok("0000000000000000000042")(g => g.uL)       ==== Is(ULong.wrap(42L))
    T ~ Grok("0000000000042")(g => g.uB)                ==== Is(UByte.wrap(42))
    T ~ errText(Grok("12345678901234567890123456789")(g => g.uB)).contains("UByte range")  ==== true
    T ~ errText(Grok("12345678901234567890123456789")(g => g.uI)).contains("UInt range")   ==== true
    T ~ errText(Grok("12345678901234567890123456789")(g => g.uL)).contains("ULong range")  ==== true
    T ~ Grok("12 34", delim = Delim.white)(g => (g.uI, g.uI)) ==== Is((UInt.wrap(12), UInt.wrap(34)))
    T ~ bad(Grok("12x")(g => g.uI))                     ==== true
    T ~ Grok("12x", partial = true)(g => g.uI)          ==== Is(UInt.wrap(12))
    T ~ bad(Grok("")(g => g.uL))                        ==== true
    T ~ bad(Grok("+")(g => g.uL))                       ==== true
    T ~ Grok("42")(g => g.uB_?(_.toInt < 100))          ==== Is(UByte.wrap(42))
    T ~ bad(Grok("42")(g => g.uB_?(_.toInt > 100)))     ==== true
    T ~ Grok("70000")(g => g.uI_?(_.toLong > 65535L))   ==== Is(UInt.wrap(70000))
    T ~ bad(Grok("50")(g => g.uS_?(_.toInt > 100)))     ==== true
    T ~ Grok("18446744073709551615")(g => g.uL_?(_ == ULong.MaxValue)) ==== Is(ULong.MaxValue)

  @Test
  def grokHexTest(): Unit =
    T ~ Grok("ff")(g => g.xB)                          ==== Is((-1).toByte)
    T ~ Grok("7f")(g => g.xB)                          ==== Is(127.toByte)
    T ~ Grok("FF")(g => g.xB)                          ==== Is((-1).toByte)
    T ~ Grok("0")(g => g.xB)                           ==== Is(0.toByte)
    T ~ Grok("00ff")(g => g.xB)                        ==== Is((-1).toByte)
    T ~ bad(Grok("100")(g => g.xB))                    ==== true
    T ~ errText(Grok("100")(g => g.xB)).contains("hex Byte range")  ==== true
    T ~ Grok("beef")(g => g.xS)                        ==== Is(0xBEEF.toShort)
    T ~ Grok("Beef")(g => g.xS)                        ==== Is(0xBEEF.toShort)
    T ~ bad(Grok("10000")(g => g.xS))                  ==== true
    T ~ Grok("DeadBeef")(g => g.xI)                    ==== Is(0xDEADBEEF)
    T ~ Grok("12345678")(g => g.xI)                    ==== Is(0x12345678)
    T ~ Grok("0000000012345678")(g => g.xI)            ==== Is(0x12345678)
    T ~ bad(Grok("123456789")(g => g.xI))              ==== true
    T ~ errText(Grok("123456789")(g => g.xI)).contains("hex Int range")  ==== true
    T ~ Grok("FFFFFFFFFFFFFFFF")(g => g.xL)            ==== Is(-1L)
    T ~ Grok("123456789abcdef0")(g => g.xL)            ==== Is(0x123456789ABCDEF0L)
    T ~ bad(Grok("12345678901234567")(g => g.xL))      ==== true
    T ~ bad(Grok("zz")(g => g.xB))                     ==== true
    T ~ errText(Grok("zz")(g => g.xB)).contains("hexadecimal")  ==== true
    T ~ bad(Grok("")(g => g.xB))                       ==== true
    T ~ bad(Grok("-1f")(g => g.xB))                    ==== true
    T ~ bad(Grok("fg")(g => g.xB))                     ==== true
    T ~ Grok("fg", partial = true)(g => g.xB)          ==== Is(15.toByte)
    T ~ Grok("ff 0a", delim = Delim.white)(g => (g.xB, g.xB)) ==== Is(((-1).toByte, 10.toByte))
    T ~ Grok("cafe babe", delim = Delim.white)(g => (g.xS, g.xS)) ==== Is((0xCAFE.toShort, 0xBABE.toShort))
    T ~ Grok("ff")(g => g.uxB)                         ==== Is(UByte.wrap(-1))
    T ~ Grok("ffff")(g => g.uxS)                       ==== Is(UShort.wrap(-1))
    T ~ Grok("ffffffff")(g => g.uxI)                   ==== Is(UInt.wrap(-1))
    T ~ Grok("ffffffffffffffff")(g => g.uxL)           ==== Is(ULong.wrap(-1L))
    T ~ Grok("0a")(g => g.uxB)                         ==== Is(UByte.wrap(10))
    T ~ bad(Grok("100")(g => g.uxB))                   ==== true
    T ~ errText(Grok("100")(g => g.uxB)).contains("hex UByte range")  ==== true
    T ~ bad(Grok("123456789")(g => g.uxI))             ==== true
    { // all sources agree on the new hexWork machinery (window 8 forces mid-number refills)
      val s = "deadbeef cafebabe 7fffffff 0 ffffffff"
      val expected = Array(0xDEADBEEF, 0xCAFEBABE, 0x7FFFFFFF, 0, 0xFFFFFFFF)
      T ~ Grok(s, delim = Delim.white)(g => Array.fill(5)(g.xI)).get                        =**= expected
      T ~ Grok(s.getBytes, Delim.white, false, false)(g => Array.fill(5)(g.xI)).get         =**= expected
      T ~ Grok(s.toCharArray, Delim.white, false, false)(g => Array.fill(5)(g.xI)).get      =**= expected
      T ~ Grok(Mem of s.getBytes, Delim.white, false, false)(g => Array.fill(5)(g.xI)).get  =**= expected
      T ~ Grok.buffered(s.getBytes, Delim.white, false, false, 8)(g => Array.fill(5)(g.xI)).get =**= expected
    }

  @Test
  def grokDigitsFieldTest(): Unit =
    T ~ Grok("20250706")(g => (g.digits(4), g.digits(2), g.digits(2)))  ==== Is((2025L, 7L, 6L))
    T ~ Grok("123456")(g => (g.digits(4), g.digits(2)))                 ==== Is((1234L, 56L))
    T ~ Grok("12ab")(g => (g.digits(2), g.C, g.C))                      ==== Is((12L, 'a', 'b'))
    T ~ Grok("0042x", partial = true)(g => g.digits(4))                 ==== Is(42L)
    T ~ Grok("12", partial = true)(g => g.digits(4))                    ==== Is(12L)
    T ~ Grok("999999999999999999")(g => g.digits(18))                   ==== Is(999999999999999999L)
    T ~ Grok("12 34", delim = Delim.white)(g => (g.digits(2), g.digits(2))) ==== Is((12L, 34L))
    T ~ Grok("x", partial = true)(g => (g.digits(0), g.C))              ==== Is((0L, 'x'))
    T ~ bad(Grok("x")(g => g.digits(4)))                                ==== true
    T ~ Grok("1234")(g => g.digits(4, exact = true))                    ==== Is(1234L)
    T ~ bad(Grok("123x")(g => g.digits(4, exact = true)))               ==== true
    T ~ errText(Grok("123x")(g => g.digits(4, exact = true))).contains("4 digits") ==== true
    T ~ bad(Grok("123")(g => g.digits(4, exact = true)))                ==== true
    T ~ Grok("cafebabe")(g => (g.hexDigits(4), g.hexDigits(4)))         ==== Is((0xCAFEL, 0xBABEL))
    T ~ Grok("CaFe")(g => g.hexDigits(4))                               ==== Is(0xCAFEL)
    T ~ Grok("ffffffffffffffff")(g => g.hexDigits(16))                  ==== Is(-1L)
    T ~ Grok("12", partial = true)(g => g.hexDigits(16))                ==== Is(0x12L)
    T ~ Grok("12zz", partial = true)(g => g.hexDigits(4))               ==== Is(0x12L)
    T ~ bad(Grok("12zz")(g => g.hexDigits(4, exact = true)))            ==== true
    T ~ errText(Grok("12zz")(g => g.hexDigits(4, exact = true))).contains("4 hex digits") ==== true
    T ~ bad(Grok("zz")(g => g.hexDigits(4)))                            ==== true
    // no token boundary needed: decimal field jammed against letters where I would fail
    T ~ bad(Grok("12ab")(g => g.I))                                     ==== true
    T ~ Grok("12ab")(g => (g.digits(2), g.tok))                         ==== Is((12L, "ab"))

  @Test
  def grokTokenTest(): Unit =
    T ~ Grok("My name is Eel", delim = Delim.white)(g => { (g < "My" < "name" < "is") __ Unit; g.tok }) ==== Is("Eel")
    T ~ Grok("line one\nline two")(g => (g.tok, g.tok))                    ==== Is(("line one", "line two"))
    T ~ Grok("a b c d", delim = Delim.white)(g => { g.skip(2); g.tok })    ==== Is("c")
    T ~ Grok("hello there", delim = Delim.white)(g => g.tokSpan)           ==== Is(5)
    T ~ Grok("true FALSE", delim = Delim.white)(g => (g.Z, g.Z))           ==== Is((true, false))
    T ~ bad(Grok("maybe")(g => g.Z))                                       ==== true
    T ~ bad(Grok("trueish")(g => g.Z))                                     ==== true
    T ~ Grok("trueish", partial = true)(g => g.Z)                          ==== Is(true)
    val e = errText(Grok("My name is Eel", delim = Delim.white)(g => (g < "My" < "nome").tok))
    T ~ e.contains("expected \"nome\"") ==== true
    T ~ e.contains("found \"name\"")    ==== true

  @Test
  def grokPeekTest(): Unit =
    T ~ Grok("ab", exact = true)(g => (g.peek, g.C, g.peek, g.C, g.peekOr('!'))) ==== Is(('a', 'a', 'b', 'b', '!'))
    T ~ bad(Grok("", exact = true)(g => g.peek))                           ==== true
    val jsonish = Grok("[3, -14, 159]", delim = Delim.white, partial = true): g =>
      val xsb = Array.newBuilder[Int]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ',') __ Unit
          case _   => xsb += g.I
      xsb.result()
    T ~ jsonish.get.toList ==== List(3, -14, 159)

  @Test
  def grokExactTest(): Unit =
    T ~ Grok("  x", exact = true)(g => (g.C, g.C, g.C))                    ==== Is((' ', ' ', 'x'))
    T ~ Grok(" 5", delim = Delim.white)(g => g.I)                          ==== Is(5)
    T ~ bad(Grok(" 5", delim = Delim.white, exact = true)(g => g.I))       ==== true
    T ~ Grok("5")(g => { val x = g.I; g.end; x })                          ==== Is(5)
    T ~ bad(Grok("5 ", delim = Delim.lines)(g => { val x = g.I; g.end; x })) ==== true
    T ~ errText(Grok("ab")(g => g.oops("gave up"))).contains("gave up")    ==== true

  @Test
  def grokSeparatorTest(): Unit =
    val c = Delim.of(",")
    def b(s: String): Array[Byte] = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    // In exact mode delimiters are separators: n separators delimit n+1 fields
    T ~ Grok(",", c, false, true)(g => (g.tok, g.tok))                        ==== Is(("", ""))
    T ~ bad(Grok(",", c, false, true)(g => (g.tok, g.tok, g.tok)))            ==== true
    T ~ Grok(",59,", c, false, true)(g => (g.tok, g.I, g.tok))                ==== Is(("", 59, ""))
    T ~ Grok("123456,true", c, false, true)(g => (g.digits(2) + g.I, g.Z))    ==== Is((3468L, true))
    T ~ Grok("a,,c", c, false, true)(g => (g.tok, g.tok, g.tok))              ==== Is(("a", "", "c"))
    T ~ Grok("a,", c, false, true)(g => (g.tok, g.tok))                       ==== Is(("a", ""))
    T ~ bad(Grok("a,", c, false, true)(g => { val t = g.tok; g.end; t }))     ==== true
    T ~ Grok("a,", c, false, true)(g => { val t = (g.tok, g.tok); g.end; t }) ==== Is(("a", ""))
    T ~ bad(Grok(",x", c, false, true)(g => g.I))                             ==== true
    // Iteration terminates: 3 separators = 4 fields, then hasMore is false
    T ~ Grok(",,,", c, false, true)(g => { val l = List.newBuilder[String]; while g.hasMore do l += g.tok; l.result() }) ==== Is(List("", "", "", ""))
    // Whitespace separators are not collapsed in exact mode
    T ~ Grok("a  b", Delim.white, false, true)(g => (g.tok, g.tok, g.tok))    ==== Is(("a", "", "b"))
    // skip counts empty fields too
    T ~ Grok("a,,c", c, false, true)(g => { g.skip(2); g.tok })               ==== Is("c")
    // Literal matching participates uniformly
    T ~ Grok("a -> b", Delim.white, false, true)(g => (g.tok, (g < "->").tok)) ==== Is(("a", "b"))
    // C and `< char` take manual control and revoke the grant
    T ~ Grok("a,,b", c, false, true)(g => (g.tok, g.C, g.tok, g.C, g.tok))    ==== Is(("a", ',', "", ',', "b"))
    // chars: fixed-width fields, token semantics for single characters
    T ~ Grok(",x,", c, false, true)(g => (g.tok, g.chars(1), g.tok))          ==== Is(("", "x", ""))
    T ~ Grok("20250706")(g => (g.chars(4), g.chars(2), g.chars(2)))           ==== Is(("2025", "07", "06"))
    T ~ Grok("abc")(g => g.chars(5))                                          ==== Is("abc")
    T ~ bad(Grok("abc")(g => g.chars(5, exact = true)))                       ==== true
    T ~ Grok("abc")(g => g.chars(0))                                          ==== Is("")
    T ~ bad(Grok("")(g => g.chars(1)))                                        ==== true
    T ~ Grok(b("πr"), Delim.white, false, false)(g => (g.chars(2), g.chars(1))) ==== Is(("π", "r"))
    // select saves and restores the separator grant
    T ~ Grok(",x", c, false, true)(g => { g.tok __ Unit; g.select(g.I.toString, g.tok) }) ==== Is("x")
    // Sub-parses start fresh and re-grant on completion
    T ~ Grok("x y,z", Delim.of(",", Delim.white), false, true)(g => (g.grok()((g.tok, g.tok)), g.tok)) ==== Is((("x", "y"), "z"))
    // Separator semantics on byte and windowed sources, known and unknown length
    T ~ Grok(b("a,,c"), c, false, true)(g => (g.tok, g.tok, g.tok))           ==== Is(("a", "", "c"))
    T ~ Grok.buffered(b("a,,c"), c, false, true, 8)(g => (g.tok, g.tok, g.tok)) ==== Is(("a", "", "c"))
    T ~ Grok.buffered(new java.io.ByteArrayInputStream(b(",")), c, false, true, 8)(g => (g.tok, g.tok)) ==== Is(("", ""))
    T ~ Grok.buffered(new java.io.ByteArrayInputStream(b("a,")), c, false, true, 8)(g => (g.tok, g.tok, g.hasMore)) ==== Is(("a", "", false))

  @Test
  def grokDelimTest(): Unit =
    // Set composition
    T ~ (Delim.white | Delim.of(","))(',')    ==== true
    T ~ (Delim.white | Delim.of(","))(' ')    ==== true
    T ~ (Delim.white & Delim.of(" ,"))(' ')   ==== true
    T ~ (Delim.white & Delim.of(" ,"))(',')   ==== false
    T ~ (Delim.white &~ Delim.of(" "))(' ')   ==== false
    T ~ (Delim.white &~ Delim.of(" "))('\t')  ==== true
    T ~ Delim.one(',')(',')                   ==== true
    T ~ Delim.one(',')('.')                   ==== false
    // over appends at the bottom of the sub chain
    T ~ (Delim.lines over Delim.of(",")).sub.sub(',') ==== true
    T ~ (Delim.lines over Delim.of(",")).sub(' ')     ==== true
    // The motivating case: a sub-grok delimited within the current token
    T ~ Grok("the 1,2,3 count", Delim.white){ g =>
          g < "the" __ Unit
          val t = g.grok(',', exact = true):
            (g.I, g.I, g.I)
          (t, g.tok)
        } ==== Is(((1, 2, 3), "count"))
    // Char and String delimiter specs at every level
    T ~ Grok("a,b", delim = ',')(g => (g.tok, g.tok))              ==== Is(("a", "b"))
    T ~ Grok("a,b;c", delim = ",;")(g => (g.tok, g.tok, g.tok))    ==== Is(("a", "b", "c"))
    T ~ Grok("a b")(g => (g.delimit(' ').tok, g.tok))              ==== Is(("a", "b"))
    // The sub-parse gets its own modes; the outer modes come back afterwards
    T ~ Grok("1,,3 x", Delim.white){ g =>
          val t = g.grok(',', exact = true)((g.tok, g.tok, g.tok))
          (t, g.tok)
        } ==== Is((("1", "", "3"), "x"))
    T ~ Grok("12ab x", Delim.white){ g =>
          val v = g.grok(Delim.none, partial = true)(g.I)
          (v, g.tok)
        } ==== Is((12, "x"))
    // Reading past the end of a sub-parse's token is an error, not a leak into the enclosing input
    val eOff = errText(Grok("1,2 rest", Delim.white)(g => g.grok(',')((g.I, g.I, g.I))))
    T ~ eOff.contains("in sub-parse")   ==== true
    T ~ eOff.contains("end of section") ==== true
    T ~ Grok("1,2 rest", Delim.white)(g => g.grok(',')((g.I, g.I, g.hasMore))) ==== Is((1, 2, false))
    // done() finishes a sub-parse early; the cursor still passes the whole token
    T ~ Grok("1,2,3,4,5 rest", Delim.white){ g =>
          val sum = g.grok(','):
            var s = 0
            while g.hasMore do
              val v = g.I
              if v >= 3 then g.done(s)
              s += v
            s
          (sum, g.tok)
        } ==== Is((3, "rest"))

  @Test
  def grokFactoryTest(): Unit =
    def b(s: String): Array[Byte] = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    def m(s: String): Mem[Byte] = Mem.of(b(s))
    // One transparent factory: every source flavor takes the same named defaults
    T ~ Grok(b("10 20"), delim = Delim.white)(g => g.I + g.I)                 ==== Is(30)
    T ~ Grok(b("06b"), partial = true)(g => g.I)                              ==== Is(6)
    T ~ Grok(m("a,,c"), Delim.of(","), exact = true)(g => (g.tok, g.tok, g.tok)) ==== Is(("a", "", "c"))
    T ~ Grok("x y".toCharArray, delim = Delim.white)(g => (g.tok, g.tok))     ==== Is(("x", "y"))
    T ~ Grok.buffered(b("10 20"), Delim.white, window = 4)(g => g.I + g.I)    ==== Is(30)
    T ~ Grok.buffered(new java.io.ByteArrayInputStream(b("1.5 x")), delim = Delim.white)(g => (g.D, g.tok)) ==== Is((1.5, "x"))
    T ~ Grok.buffered(java.nio.CharBuffer.wrap("7 8".toCharArray), delim = Delim.white)(g => g.I + g.I) ==== Is(15)

  @Test
  def grokSubTest(): Unit =
    T ~ Grok("My name is Eel\nsecond line"){ g =>
          val name = g.grok():
            (g < "My" < "name" < "is") __ Unit
            g.tok
          (name, g.tok)
        } ==== Is(("Eel", "second line"))
    val e = errText(Grok("My name is Eel\nmore"){ g =>
      g.grok()((g < "My" < "gnome").tok)
    })
    T ~ e.contains("in sub-parse starting at line 1, pos 1") ==== true
    T ~ e.contains("expected \"gnome\"")                     ==== true

  @Test
  def grokSelectTest(): Unit =
    def sel(s: String): Ask[String] = Grok(s, delim = Delim.white): g =>
      g.select(
        (g.I + g.I).toString,
        g.tok match
          case "yes" => "true"
          case "no"  => "false"
          case _     => g.continue(),
        { g.skip(1); g.D.toString }
      )
    T ~ sel("3 4")     ==== Is("7")
    T ~ sel("yes")     ==== Is("true")
    T ~ sel("no")      ==== Is("false")
    T ~ sel("x 2.5")   ==== Is("2.5")
    T ~ errText(sel("x y")).contains("no alternative matched (3 tried)") ==== true

  @Test
  def grokBytesTest(): Unit =
    def b(s: String): Array[Byte] = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    T ~ Grok(b("2025-06-25"), Delim.lines, true, false)(g => (g.I, (g < '-').I, (g < '-').I)) ==== Is((2025, 6, 25))
    T ~ Grok(b("10 20 -5"), Delim.white, false, false)(g => g.I + g.I + g.I)                  ==== Is(25)
    T ~ Grok(b("My name is Eel"), Delim.white, false, false)(g => { (g < "My" < "name" < "is") __ Unit; g.tok }) ==== Is("Eel")
    T ~ Grok(b("line one\nline two"))(g => (g.tok, g.tok))                                    ==== Is(("line one", "line two"))
    T ~ Grok(b("3.14159 true"), Delim.white, false, false)(g => (g.D, g.Z))                   ==== Is((3.14159, true))
    T ~ Grok(b("π = 3.25"), Delim.white, false, false)(g => (g.tok, (g < "=").D))        ==== Is(("π", 3.25))
    T ~ bad(Grok(b("06b"), Delim.lines, false, false)(g => g.I))                              ==== true
    Grok(b("2025-06b-25"), Delim.lines, true, false)(g => (g.I, (g < '-').I, (g < '-').I)) match
      case Alt(e) => e.underlying match
        case f: Grok.Failure =>
          T ~ f.position ==== 7L
          T ~ f.description.contains("expected '-'") ==== true
        case u => assertTrue("not a Grok.Failure: " + u, false)
      case v => assertTrue("unexpected success: " + v, false)

  @Test
  def grokDigitKernelTest(): Unit =
    // Edge cases at the kernel's 18-digit window boundary and the tail handoff (all longWork
    // now flows through the shared positive-accumulation digitsWork kernel)
    def b(s: String): Array[Byte] = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    T ~ Grok(b("999999999999999999"))(g => g.L)            ==== Is(999999999999999999L)    // 18 digits: no tail
    T ~ Grok(b("1000000000000000000"))(g => g.L)           ==== Is(1000000000000000000L)   // 19 digits: via tail
    T ~ Grok(b("-9223372036854775808"))(g => g.L)          ==== Is(Long.MinValue)          // reachable only in the tail's negative space
    T ~ Grok(b("000000000000000000123"))(g => g.L)         ==== Is(123L)                   // leading zeros exhaust the window harmlessly
    T ~ bad(Grok(b("-x"))(g => g.L))                       ==== true
    T ~ Grok("999999999999999999 1000000000000000000 x", delim = Delim.white)(g => (g.L, g.L, g.tok)) ==== Is((999999999999999999L, 1000000000000000000L, "x"))
    T ~ bad(Grok(Mem.of(b("18446744073709551616")))(g => g.L)) ==== true                  // 2^64: tail overflow on Mem too

  @Test
  def grokDoubleKernelTest(): Unit =
    // doubleImpl runs on the digitsWork kernel; these cover its significance/truncation edges
    def b(s: String): Array[Byte] = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    def d2(s: String): Ask[Double] = Grok(b(s))(g => g.D)
    T ~ d2("3.14159")                    ==== Is(3.14159)
    T ~ d2("0.1")                        ==== Is(0.1)
    T ~ d2("-2.5e-4")                    ==== Is(-2.5e-4)
    T ~ d2("1e300")                      ==== Is(1e300)
    T ~ d2("9007199254740993")           ==== Is("9007199254740993".toDouble)
    T ~ d2("1.7976931348623157e308")     ==== Is(Double.MaxValue)
    T ~ d2("0.000001234")                ==== Is(0.000001234)
    T ~ d2("42")                         ==== Is(42.0)
    T ~ d2("NaN").map(_.isNaN)           ==== Is(true)
    T ~ d2("-Infinity")                  ==== Is(Double.NegativeInfinity)
    T ~ d2("-0.0")                       ==== Is(-0.0)
    T ~ bad(d2("1.5x"))                  ==== true
    T ~ bad(d2("eel"))                   ==== true
    T ~ bad(d2("."))                     ==== true
    T ~ Grok(b("1.5x"), Delim.lines, true, false)(g => g.D)            ==== Is(1.5)
    T ~ Grok(b("1.5e+"), Delim.lines, true, false)(g => (g.D, g.C, g.C)) ==== Is((1.5, 'e', '+'))
    // Significance boundary and truncation paths
    T ~ d2("123456789012345678")         ==== Is("123456789012345678".toDouble)     // 18 sig digits exactly
    T ~ d2("12345678901234567890123")    ==== Is("12345678901234567890123".toDouble) // dropped integer digits
    T ~ d2("0.00000000000000000000123")  ==== Is("0.00000000000000000000123".toDouble) // long leading-zero fraction
    T ~ d2("1234567890123456789.012e-5") ==== Is("1234567890123456789.012e-5".toDouble)
    T ~ d2("0.99999999999999999999")     ==== Is("0.99999999999999999999".toDouble)  // dropped fraction digits
    T ~ d2("000000000000000000123.5")    ==== Is(123.5)                              // leading zeros don't eat significance
    // Random differential against the JDK
    val r = new java.util.Random(0x5EED2)
    var n = 0
    while n < 500 do
      val sb = new java.lang.StringBuilder
      if r.nextBoolean() then sb.append('-') __ Unit
      var ilen = r.nextInt(22)
      var flen = r.nextInt(22)
      if ilen + flen == 0 then ilen = 1
      var k = 0
      while k < ilen do
        sb.append(('0' + r.nextInt(10)).toChar) __ Unit
        k += 1
      if flen > 0 || (ilen > 0 && r.nextBoolean()) then
        sb.append('.') __ Unit
        k = 0
        while k < flen do
          sb.append(('0' + r.nextInt(10)).toChar) __ Unit
          k += 1
      if r.nextInt(3) == 0 then
        sb.append('e') __ Unit
        if r.nextBoolean() then sb.append('-') __ Unit
        sb.append(r.nextInt(320).toString) __ Unit
      val s = sb.toString
      T ~ d2(s) ==== Is(java.lang.Double.parseDouble(s))
      n += 1

  @Test
  def grokMemTest(): Unit =
    def m(s: String): Mem[Byte] = Mem.of(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    T ~ Grok(m("2025-06-25"), Delim.lines, true, false)(g => (g.I, (g < '-').I, (g < '-').I)) ==== Is((2025, 6, 25))
    T ~ Grok(m("My name is Salmon"), Delim.white, false, false)(g => { (g < "My" < "name" < "is") __ Unit; g.tok }) ==== Is("Salmon")
    T ~ Grok(m("3.25 true"), Delim.white, false, false)(g => (g.D, g.Z))                      ==== Is((3.25, true))
    T ~ Grok(m("π = -17"), Delim.white, false, false)(g => (g.tok, (g < "=").I))         ==== Is(("π", -17))
    T ~ bad(Grok(m("06b"))(g => g.I))                                                         ==== true

  @Test
  def grokCharsTest(): Unit =
    T ~ Grok("2025-06-25".toCharArray, Delim.lines, true, false)(g => (g.I, (g < '-').I, (g < '-').I)) ==== Is((2025, 6, 25))
    T ~ Grok("My name is Eel".toCharArray, Delim.white, false, false)(g => { (g < "My" < "name" < "is") __ Unit; g.tok }) ==== Is("Eel")
    T ~ Grok("3.14159 true".toCharArray, Delim.white, false, false)(g => (g.D, g.Z))          ==== Is((3.14159, true))
    T ~ bad(Grok("06b".toCharArray)(g => g.I))                                                ==== true

  @Test
  def grokBufferedTest(): Unit =
    def b(s: String): Array[Byte] = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    // Small inputs: fully preloaded, the window never moves
    T ~ Grok.buffered(b("2025-06-25"), partial = true)(g => (g.I, (g < '-').I, (g < '-').I)) ==== Is((2025, 6, 25))
    T ~ Grok.buffered(b("10 20 -5"), Delim.white)(g => g.I + g.I + g.I)                      ==== Is(25)
    T ~ Grok.buffered(b("9223372036854775807"))(g => g.L)                                    ==== Is(Long.MaxValue)
    T ~ bad(Grok.buffered(b("9223372036854775808"))(g => g.L))                               ==== true
    T ~ bad(Grok.buffered(b("06b"))(g => g.I))                                               ==== true
    T ~ Grok.buffered(b("π = 3.25"), Delim.white)(g => (g.tok, (g < "=").D))                 ==== Is(("π", 3.25))
    T ~ Grok.buffered(b("3.14159 true"), Delim.white)(g => (g.D, g.Z))                       ==== Is((3.14159, true))
    T ~ Grok.buffered(b("9007199254740993"))(g => g.D)                                       ==== Is("9007199254740993".toDouble)
    T ~ Grok.buffered(b("-Infinity"))(g => g.D)                                              ==== Is(Double.NegativeInfinity)
    T ~ Grok.buffered(b("1.5e+"), partial = true)(g => (g.D, g.C, g.C))                      ==== Is((1.5, 'e', '+'))
    T ~ Grok.buffered(b("trueish"), partial = true)(g => g.Z)                                ==== Is(true)
    // select pins its start, so alternatives can consume far past the window and still restart
    T ~ Grok.buffered(b("x 2.5"), Delim.white)(g => g.select((g.I + g.I).toString, { g.skip(1); g.D.toString })) ==== Is("2.5")
    val digits80 = "1" * 80
    T ~ Grok.buffered(b(digits80), Delim.white)(g => g.select(g.L.toString, g.tok)) ==== Is(digits80)
    T ~ Grok.buffered(b("abc 12"), Delim.white)(g => g.select(g.I.toString, g.select((g < "zzz").tok, g.tok) + " " + g.I.toString)) ==== Is("abc 12")
    // The window grows to hold a token or delimiter run larger than its initial size
    T ~ Grok.buffered(b("y" * 150 + " end"), Delim.white)(g => (g.tokSpan, g.tok)) ==== Is((150, "end"))
    T ~ Grok.buffered(b(" " * 100 + "7"), Delim.white)(g => g.I)                   ==== Is(7)
    // A slow-path double crossing the window edge: doubleImpl pins vPos so the re-read survives
    T ~ Grok.buffered(b(" " * 55 + "9007199254740993.5 x"), Delim.white)(g => (g.D, g.tok)) ==== Is(("9007199254740993.5".toDouble, "x"))
    T ~ Grok.buffered(b("My name is Eel\nsecond line")){ g =>
          val name = g.grok():
            (g < "My" < "name" < "is") __ Unit
            g.tok
          (name, g.tok)
        } ==== Is(("Eel", "second line"))
    // Long input: many scoots; sum of 60 ints crossing window edges at varied alignments
    val ns = Array.tabulate(60)(k => (k * 1000003) % 39916801 - 19958400)
    val nsText = ns.mkString(" ")
    T ~ Grok.buffered(b(nsText), Delim.white)(g => { var s = 0L; while g.hasMore do s += g.I; s }) ==== Is(ns.map(_.toLong).sum)
    // Token straddling the first window edge: skipDelims commits before the token scan pins retention
    T ~ Grok.buffered(b(" " * 60 + "abcdefghij rest"), Delim.white)(g => (g.tok, g.tok))     ==== Is(("abcdefghij", "rest"))
    // Differential vs the directly indexed Bytes source on a mixed workload
    val mixed = (0 until 25).map(k => s"w$k ${k * 77} ${k * 0.125} ${k % 2 == 0}").mkString(" ")
    val viaBytes    = Grok(b(mixed), Delim.white, false, false)(g => Array.fill(25)((g.tok, g.I, g.D, g.Z)).toSeq)
    val viaBuffered = Grok.buffered(b(mixed), Delim.white)(g => Array.fill(25)((g.tok, g.I, g.D, g.Z)).toSeq)
    T ~ viaBuffered ==== viaBytes
    // Error positions remain absolute even after scooting
    Grok.buffered(b(" " * 40 + "12x34"), Delim.white)(g => g.I) match
      case Alt(e) => e.underlying match
        case f: Grok.Failure => T ~ f.position ==== 42L
        case u => assertTrue("not a Grok.Failure: " + u, false)
      case v => assertTrue("unexpected success: " + v, false)
    // ByteBuffer-fed window, starting at a nonzero position
    val bb = java.nio.ByteBuffer.wrap(b("XX[1, 22, 333] true"))
    bb.position(2) __ Unit
    T ~ Grok.buffered(bb, Delim.white, true, false, 64){ g =>
          (g < '[') __ Unit
          val x = (g.I, (g < ",").I, (g < ",").I)
          (g < ']') __ Unit
          (x, g.Z)
        } ==== Is(((1, 22, 333), true))

  @Test
  def grokQuotedStringTest(): Unit =
    val utf8 = java.nio.charset.StandardCharsets.UTF_8
    // JSON style on a String source
    T ~ Grok("\"hello\"")(g => g.str)                                   ==== Is("hello")
    T ~ Grok("\"\"")(g => g.str)                                        ==== Is("")
    T ~ Grok("\"a\\n\\t\\\"b\\\\c\\/d\\be\\ff\\rg\"")(g => g.str)       ==== Is("a\n\t\"b\\c/d\be\ff\rg")
    T ~ Grok("\"snow \\u2603!\"")(g => g.str)                           ==== Is("snow ☃!")
    T ~ Grok("\"\\uD83D\\uDE00\"")(g => g.str)                          ==== Is("😀")
    T ~ Grok("  \"padded\"", delim = Delim.white)(g => g.str)           ==== Is("padded")
    T ~ Grok("\"a\",\"b\"")(g => (g.str, (g < ',').str))                ==== Is(("a", "b"))    // quote is self-delimiting
    T ~ Grok("\"del im\" 5", delim = Delim.white)(g => (g.str, g.I))    ==== Is(("del im", 5)) // delimiters are content
    // CSV style: doubling, and backslash/newline as plain content
    T ~ Grok("\"say \"\"hi\"\"\"")(g => g.str(Quote.csv))               ==== Is("say \"hi\"")
    T ~ Grok("\"\"\"\"")(g => g.str(Quote.csv))                         ==== Is("\"")
    T ~ Grok("\"a\nb\"")(g => g.str(Quote.csv))                         ==== Is("a\nb")
    T ~ Grok("\"back\\slash\"")(g => g.str(Quote.csv))                  ==== Is("back\\slash")
    T ~ Grok("\"a\",\"b \"\"x\"\"\",7")(g => (g.str(Quote.csv), (g < ',').str(Quote.csv), (g < ',').I)) ==== Is(("a", "b \"x\"", 7))
    // Other quote styles
    T ~ Grok("'it''s'")(g => g.str(Quote.sql))                          ==== Is("it's")
    T ~ Grok("'don\\'t'")(g => g.str(Quote('\'')))                      ==== Is("don't")
    // Output forms
    T ~ Grok("\"chars\"")(g => new String(g.strChars))                  ==== Is("chars")
    T ~ Grok("\"say \"\"hi\"\"\"")(g => new String(g.strChars(Quote.csv))) ==== Is("say \"hi\"")
    T ~ Grok("\"hi☃\"")(g => g.strBytes.toList)                    ==== Is("hi☃".getBytes(utf8).toList)
    T ~ Grok("\"\\uD83D\\uDE00\"")(g => g.strBytes.toList)              ==== Is("😀".getBytes(utf8).toList)
    T ~ Grok("\"abc\"def")(g => (g.strSpan, g.tok))                     ==== Is((5, "def"))
    T ~ Grok("\"a\\u0041b\" 9", delim = Delim.white)(g => (g.strSpan, g.I)) ==== Is((10, 9))
    // Lone surrogates: kept in UTF-16 output, U+FFFD in UTF-8 output
    T ~ Grok("\"lone\\uD800!\"")(g => g.str)                            ==== Is("lone\uD800!")
    T ~ Grok("\"lone\\uD800!\"")(g => g.strBytes.toList)                ==== Is("lone�!".getBytes(utf8).toList)
    // Failures: no quote, unclosed, bad escape, bad hex, trailing backslash
    T ~ bad(Grok("noquote")(g => g.str))                                ==== true
    T ~ bad(Grok("\"unclosed")(g => g.str))                             ==== true
    T ~ bad(Grok("\"bad\\q\"")(g => g.str))                             ==== true
    T ~ bad(Grok("\"bad\\u12G4\"")(g => g.str))                         ==== true
    T ~ bad(Grok("\"trail\\")(g => g.str))                              ==== true
    T ~ bad(Grok("\"a\"\"")(g => g.str(Quote.csv)))                     ==== true   // doubled quote then end: unclosed
    T ~ bad(Grok(" \"x\"", delim = Delim.white, exact = true)(g => g.str)) ==== true
    T ~ errText(Grok("\"unclosed")(g => g.str)).contains("unclosed quoted string") ==== true
    Grok("\"oops\\q\"")(g => g.str) match
      case Alt(e) => e.underlying match
        case f: Grok.Failure =>
          T ~ f.position                                    ==== 6L
          T ~ f.description.contains("expected a valid escape") ==== true
          T ~ f.description.contains("found 'q'")           ==== true
        case u => assertTrue("not a Grok.Failure: " + u, false)
      case v => assertTrue("unexpected success: " + v, false)
    // A failed str is an ordinary select alternative
    T ~ Grok("plain", delim = Delim.white)(g => g.select(g.str, g.tok)) ==== Is("plain")

  @Test
  def grokQuotedSourcesTest(): Unit =
    val utf8 = java.nio.charset.StandardCharsets.UTF_8
    def b(s: String): Array[Byte] = s.getBytes(utf8)
    // Byte source: escapes decode straight to UTF-8; clean strBytes never decodes
    T ~ Grok(b("\"snow \\u2603\""))(g => g.str)                         ==== Is("snow ☃")
    T ~ Grok(b("\"\\uD83D\\uDE00\""))(g => g.str)                       ==== Is("😀")
    T ~ Grok(b("\"\\uD83D\\uDE00\""))(g => g.strBytes.toList)           ==== Is("😀".getBytes(utf8).toList)
    T ~ Grok(b("\"π≈3\""))(g => g.str)                        ==== Is("π≈3")
    T ~ Grok(b("\"π≈3\""))(g => g.strBytes.toList)            ==== Is("π≈3".getBytes(utf8).toList)
    T ~ Grok(b("\"lone\\uD800!\""))(g => g.str)                         ==== Is("lone�!")
    T ~ Grok(b("\"say \"\"hi\"\"\" x"), Delim.white, false, false)(g => (g.str(Quote.csv), g.tok)) ==== Is(("say \"hi\"", "x"))
    T ~ Grok(b("\"a\\u0041b\" 9"), Delim.white, false, false)(g => (g.strSpan, g.I)) ==== Is((10, 9))
    T ~ Grok(b("\"chars \\u2603\""))(g => new String(g.strChars))       ==== Is("chars ☃")
    T ~ bad(Grok(b("\"unclosed"))(g => g.str))                          ==== true
    T ~ bad(Grok(b("\"bad\\q\""))(g => g.str))                          ==== true
    // Mem source
    def m(s: String): Mem[Byte] = Mem.of(b(s))
    T ~ Grok(m("\"snow \\u2603\""))(g => g.str)                         ==== Is("snow ☃")
    T ~ Grok(m("\"say \"\"hi\"\"\""))(g => g.str(Quote.csv))            ==== Is("say \"hi\"")
    T ~ Grok(m("\"π≈3\""))(g => g.strBytes.toList)            ==== Is("π≈3".getBytes(utf8).toList)
    // Chars source
    T ~ Grok("\"snow \\u2603\"".toCharArray)(g => g.str)                ==== Is("snow ☃")
    T ~ Grok("'it''s'".toCharArray)(g => g.str(Quote.sql))              ==== Is("it's")
    T ~ Grok("\"hi☃\"".toCharArray)(g => g.strBytes.toList)        ==== Is("hi☃".getBytes(utf8).toList)
    // Buffered source: strings larger than the window, clean and escaped, plus window-edge starts
    val longClean = "z" * 150
    T ~ Grok.buffered(b("\"" + longClean + "\" next"), Delim.white)(g => (g.str, g.tok)) ==== Is((longClean, "next"))
    val manyEsc = "ab\\\"" * 40
    T ~ Grok.buffered(b("\"" + manyEsc + "\""))(g => g.str)             ==== Is("ab\"" * 40)
    T ~ Grok.buffered(b(" " * 60 + "\"snow \\u2603\""), Delim.white)(g => g.str) ==== Is("snow ☃")
    T ~ Grok.buffered(b("\"π≈3\""))(g => g.strBytes.toList)   ==== Is("π≈3".getBytes(utf8).toList)
    T ~ Grok.buffered(b("\"" + longClean + "\"x"))(g => (g.strSpan, g.C)) ==== Is((152, 'x'))
    T ~ bad(Grok.buffered(b("\"" + longClean))(g => g.str))             ==== true
    // Differential vs the directly indexed Bytes source
    val csvish = (0 until 20).map(k => "\"f" + k + " \"\"q\"\" ☃\"").mkString(",")
    def viaB   = Grok(b(csvish), Delim.lines, false, false)(g => Array.fill(20){ val s = g.str(Quote.csv); if g.hasMore then (g < ',') __ Unit; s }.toSeq)
    def viaW   = Grok.buffered(b(csvish))(g => Array.fill(20){ val s = g.str(Quote.csv); if g.hasMore then (g < ',') __ Unit; s }.toSeq)
    T ~ viaW ==== viaB

  @Test
  def grokBufferedCharsTest(): Unit =
    def cb(s: String) = java.nio.CharBuffer.wrap(s.toCharArray)
    T ~ Grok.buffered(cb("2025-06-25"), Delim.lines, true, false, 64)(g => (g.I, (g < '-').I, (g < '-').I)) ==== Is((2025, 6, 25))
    T ~ Grok.buffered(cb("10 20 -5"), Delim.white, false, false, 64)(g => g.I + g.I + g.I)   ==== Is(25)
    T ~ Grok.buffered(cb("π = 3.25"), Delim.white, false, false, 64)(g => (g.tok, (g < "=").D)) ==== Is(("π", 3.25))
    T ~ Grok.buffered(cb("3.14159 true"), Delim.white, false, false, 64)(g => (g.D, g.Z))    ==== Is((3.14159, true))
    T ~ bad(Grok.buffered(cb("06b"))(g => g.I))                                              ==== true
    // Window growth and scooting on plain tokens
    T ~ Grok.buffered(cb("y" * 150 + " end"), Delim.white, false, false, 64)(g => (g.tokSpan, g.tok)) ==== Is((150, "end"))
    val ns = Array.tabulate(60)(k => (k * 1000003) % 39916801 - 19958400)
    T ~ Grok.buffered(cb(ns.mkString(" ")), Delim.white, false, false, 64)(g => { var s = 0L; while g.hasMore do s += g.I; s }) ==== Is(ns.map(_.toLong).sum)
    // Quoted strings through the window: clean and escaped strings larger than the window
    val longClean = "z" * 150
    T ~ Grok.buffered(cb("\"" + longClean + "\" next"), Delim.white, false, false, 64)(g => (g.str, g.tok)) ==== Is((longClean, "next"))
    val manyEsc = "ab\\\"" * 40
    T ~ Grok.buffered(cb("\"" + manyEsc + "\""))(g => g.str)                                 ==== Is("ab\"" * 40)
    T ~ Grok.buffered(cb(" " * 60 + "\"snow \\u2603\""), Delim.white, false, false, 64)(g => g.str) ==== Is("snow ☃")
    T ~ Grok.buffered(cb("\"say \"\"hi\"\"\""))(g => g.str(Quote.csv))                       ==== Is("say \"hi\"")
    T ~ Grok.buffered(cb("\"hi☃\""))(g => g.strBytes.toList) ==== Is("hi☃".getBytes(java.nio.charset.StandardCharsets.UTF_8).toList)
    T ~ Grok.buffered(cb("\"" + longClean + "\"x"))(g => (g.strSpan, g.C))                   ==== Is((152, 'x'))
    T ~ bad(Grok.buffered(cb("\"" + longClean))(g => g.str))                                 ==== true
    T ~ bad(Grok.buffered(cb("\"bad\\q\""))(g => g.str))                                     ==== true
    // select pins across the window, as for bytes
    T ~ Grok.buffered(cb("x 2.5"), Delim.white, false, false, 64)(g => g.select((g.I + g.I).toString, { g.skip(1); g.D.toString })) ==== Is("2.5")
    // Differential vs the String source on escape-dense content crossing many scoots
    val csvish = (0 until 20).map(k => "\"f" + k + " \"\"q\"\" ☃\"").mkString(",")
    def viaS = Grok(csvish)(g => Array.fill(20){ val s = g.str(Quote.csv); if g.hasMore then (g < ',') __ Unit; s }.toSeq)
    def viaC = Grok.buffered(cb(csvish))(g => Array.fill(20){ val s = g.str(Quote.csv); if g.hasMore then (g < ',') __ Unit; s }.toSeq)
    T ~ viaC ==== viaS

  @Test
  def grokValidateTest(): Unit =
    def date(s: String): Ask[(Int, Int, Int)] = Grok(s, partial = true): g =>
      val y = g.I
      val m = (g < '-').I_?(m => m >= 1 && m <= 12, "month out of range")
      val d = (g < '-').I_?(d => d >= 1 && d <= 31, "day out of range")
      g.end
      (y, m, d)
    T ~ date("2025-06-25")                                      ==== Is((2025, 6, 25))
    T ~ errText(date("2025-13-25")).contains("month out of range") ==== true
    T ~ errText(date("2025-06-99")).contains("day out of range")   ==== true
    T ~ bad(date("2025-06-25x"))                                ==== true
    T ~ Grok("wahoo")(g => g.tok_?(_.forall(_.isLetter), "letters only"))  ==== Is("wahoo")
    T ~ errText(Grok("wah00")(g => g.tok_?(_.forall(_.isLetter), "letters only"))).contains("letters only") ==== true

  @Test
  def grokUtf8LiteralTest(): Unit =
    def b(s: String): Array[Byte] = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    def m(s: String): Mem[Byte] = Mem.of(b(s))
    // Non-ASCII literals must match on every source flavor (byte flavors compare UTF-8 bytes)
    T ~ Grok("π ≈ 3.25", Delim.white)(g => ((g < "π") < "≈").D)                              ==== Is(3.25)
    T ~ Grok("π ≈ 3.25".toCharArray, Delim.white, false, false)(g => ((g < "π") < "≈").D)    ==== Is(3.25)
    T ~ Grok(b("π ≈ 3.25"), Delim.white, false, false)(g => ((g < "π") < "≈").D)             ==== Is(3.25)
    T ~ Grok(m("π ≈ 3.25"), Delim.white, false, false)(g => ((g < "π") < "≈").D)             ==== Is(3.25)
    T ~ Grok.buffered(b("π ≈ 3.25"), Delim.white)(g => ((g < "π") < "≈").D)                  ==== Is(3.25)
    // A long multi-byte literal straddling window refills
    T ~ Grok.buffered(b("héllo wörld héllo wörld 42"), Delim.white, false, false, 8)(g => ((g < "héllo" < "wörld" < "héllo" < "wörld")).I) ==== Is(42)
    // Mismatches still fail cleanly, and the report shows the literal as written
    T ~ bad(Grok(b("πx"), Delim.white, false, false)(g => g < "πy"))                          ==== true
    T ~ errText(Grok(b("crêpe"), Delim.white, false, false)(g => g < "crêpes")).contains("crêpes") ==== true
    T ~ bad(Grok.buffered(b("ab"), Delim.white, false, false, 8)(g => g < "abç"))             ==== true

  @Test
  def grokUnknownLengthTest(): Unit =
    import java.io.{ByteArrayInputStream, StringReader}
    def bais(s: String) = new ByteArrayInputStream(s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    T ~ Grok.buffered(bais("10\n20"))(g => g.I + g.I)                                         ==== Is(30)
    T ~ Grok.buffered(bais("10 20 -5"), Delim.white, false, false, 8)(g => g.I + g.I + g.I)   ==== Is(25)
    T ~ Grok.buffered(bais(""))(g => g.hasMore)                                               ==== Is(false)
    T ~ bad(Grok.buffered(bais(""))(g => g.I))                                                ==== true
    T ~ Grok.buffered(bais("3.25"))(g => (g.D, g.hasMore))                                    ==== Is((3.25, false))
    T ~ Grok.buffered(bais("12345678901"), Delim.white, false, false, 8)(g => g.L)            ==== Is(12345678901L)
    T ~ Grok.buffered(bais("42"), Delim.white, false, false, 8)(g => { val x = g.I; g.end; x }) ==== Is(42)
    T ~ bad(Grok.buffered(bais("42 43"), Delim.white, false, false, 8)(g => { val x = g.I; g.end; x })) ==== true
    T ~ bad(Grok.buffered(bais("ab"), Delim.white, false, false, 8)(g => g < "abc"))          ==== true
    T ~ Grok.buffered(bais("ff 18446744073709551615"), Delim.white, false, false, 8)(g => (g.xB, g.uL)) ==== Is((-1: Byte, ULong.wrap(-1L)))
    // The end discovered inside a failed select alternative must not break the restore
    T ~ Grok.buffered(bais("12"), Delim.white, false, false, 8)(g => g.select(g.I + g.I, g.I)) ==== Is(12)
    // Unclosed quoted string must error out, not spin on an unbounded view
    T ~ bad(Grok.buffered(bais("\"unclosed"), Delim.white, false, false, 8)(g => g.str))      ==== true
    T ~ Grok.buffered(bais("\"a\\tb\" done"), Delim.white, false, false, 8)(g => (g.str, g.tok)) ==== Is(("a\tb", "done"))
    // Window growth on a token longer than the initial window
    T ~ Grok.buffered(bais("supercalifragilisticexpialidocious!"), Delim.white, false, false, 8)(g => g.tok) ==== Is("supercalifragilisticexpialidocious!")
    // Sub-parses narrow and restore views correctly around a discovered end
    T ~ Grok.buffered(bais("a b\nc d"), Delim.lines, false, false, 8)(g => (g.grok()(g.tok + g.tok), g.grok()(g.tok + g.tok))) ==== Is(("ab", "cd"))
    // Reader flavor
    T ~ Grok.buffered(new StringReader("x 1.5e2 true"), Delim.white, false, false, 8)(g => (g.tok, g.D, g.Z)) ==== Is(("x", 150.0, true))
    T ~ bad(Grok.buffered(new StringReader("\"unclosed"), Delim.white, false, false, 8)(g => g.str)) ==== true
    T ~ Grok.buffered(new StringReader(""))(g => g.hasMore)                                   ==== Is(false)
}
