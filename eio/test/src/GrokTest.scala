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
  def grokSubTest(): Unit =
    T ~ Grok("My name is Eel\nsecond line"){ g =>
          val name = g.grok(): h =>
            (h < "My" < "name" < "is") __ Unit
            h.tok
          (name, g.tok)
        } ==== Is(("Eel", "second line"))
    val e = errText(Grok("My name is Eel\nmore"){ g =>
      g.grok(): h =>
        (h < "My" < "gnome").tok
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
          val name = g.grok(): h =>
            (h < "My" < "name" < "is") __ Unit
            h.tok
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
}
