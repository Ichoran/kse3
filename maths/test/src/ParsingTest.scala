// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.maths


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._



@RunWith(classOf[JUnit4])
class ParsingTest {
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
  def longKernelTest(): Unit =
    T ~ Parse.long("0")                     ==== 0L
    T ~ Parse.long("42")                    ==== 42L
    T ~ Parse.long("+42")                   ==== 42L
    T ~ Parse.long("-42")                   ==== -42L
    T ~ Parse.long("-0")                    ==== 0L
    T ~ Parse.long("0000000000000000000042")==== 42L
    T ~ Parse.long("-007")                  ==== -7L
    T ~ Parse.long("9223372036854775807")   ==== Long.MaxValue
    T ~ Parse.long("-9223372036854775808")  ==== Long.MinValue        // a boundary, not the sentinel
    T ~ Parse.long("9223372036854775808")   ==== Parse.failLong
    T ~ Parse.long("-9223372036854775809")  ==== Parse.failLong
    T ~ Parse.long("99999999999999999999")  ==== Parse.failLong
    T ~ Parse.long("")                      ==== Parse.failLong
    T ~ Parse.long("-")                     ==== Parse.failLong
    T ~ Parse.long("+")                     ==== Parse.failLong
    T ~ Parse.long("-+42")                  ==== Parse.failLong
    T ~ Parse.long("1x")                    ==== Parse.failLong
    T ~ Parse.long(" 1")                    ==== Parse.failLong
    T ~ Parse.long("1 ")                    ==== Parse.failLong
    T ~ Parse.long("xx-42yy", 2, 5)         ==== -42L
    T ~ Parse.long("xx-42yy", 2, 6)         ==== Parse.failLong

  @Test
  def uLongKernelTest(): Unit =
    T ~ Parse.uLong("0")                     ==== ULong.wrap(0L)
    T ~ Parse.uLong("+123")                  ==== ULong.wrap(123L)
    T ~ Parse.uLong("00000000000000000000042") ==== ULong.wrap(42L)
    T ~ Parse.uLong("9223372036854775808")   ==== ULong.wrap(Long.MinValue)
    T ~ Parse.uLong("18446744073709551615")  ==== ULong.MaxValue      // a boundary, not the sentinel
    T ~ Parse.uLong("18446744073709551616")  ==== Parse.failULong
    T ~ Parse.uLong("99999999999999999999999") ==== Parse.failULong
    T ~ Parse.uLong("-1")                    ==== Parse.failULong
    T ~ Parse.uLong("-0")                    ==== Parse.failULong
    T ~ Parse.uLong("")                      ==== Parse.failULong
    T ~ Parse.uLong("+")                     ==== Parse.failULong
    T ~ Parse.uLong("12x")                   ==== Parse.failULong
    T ~ Parse.uLong("q123q", 1, 4)           ==== ULong.wrap(123L)

  @Test
  def hexKernelTest(): Unit =
    T ~ Parse.hex("0")                  ==== 0L
    T ~ Parse.hex("ff")                 ==== 255L
    T ~ Parse.hex("FF")                 ==== 255L
    T ~ Parse.hex("DeadBeef")           ==== 0xDEADBEEFL
    T ~ Parse.hex("00ff")               ==== 255L
    T ~ Parse.hex("123456789abcdef0")   ==== 0x123456789ABCDEF0L
    T ~ Parse.hex("ffffffffffffffff")   ==== -1L                      // a boundary, not the sentinel
    T ~ Parse.hex("0ffffffffffffffff")  ==== -1L                      // leading zero is not significant
    T ~ Parse.hex("12345678901234567")  ==== Parse.failHex
    T ~ Parse.hex("+ff")                ==== Parse.failHex
    T ~ Parse.hex("-ff")                ==== Parse.failHex
    T ~ Parse.hex("0x2a")               ==== Parse.failHex
    T ~ Parse.hex("")                   ==== Parse.failHex
    T ~ Parse.hex("fg")                 ==== Parse.failHex
    T ~ Parse.hex("zzcafezz", 2, 6)     ==== 0xCAFEL

  @Test
  def spellsTest(): Unit =
    // The sentinels are nowhere points, so these are the only inputs that reach them
    T ~ Parse.long("-9170187325617826341")             ==== Parse.failLong
    T ~ Parse.uLong("18276556748091725275")            ==== Parse.failULong
    T ~ Parse.hex("c7a16d3e52b84f19")                  ==== Parse.failHex
    T ~ Parse.spellsFailLong("-9170187325617826341")   ==== true
    T ~ Parse.spellsFailLong("-0009170187325617826341")==== true
    T ~ Parse.spellsFailLong("9170187325617826341")    ==== false     // the positive value is not it
    T ~ Parse.spellsFailLong("-9170187325617826340")   ==== false
    T ~ Parse.spellsFailLong("-+9170187325617826341")  ==== false     // rejected by long, must not verify
    T ~ Parse.spellsFailLong("-91701873256178263410")  ==== false
    T ~ Parse.spellsFailLong("-9223372036854775808")   ==== false
    T ~ Parse.spellsFailLong("")                       ==== false
    T ~ Parse.spellsFailLong("-")                      ==== false
    T ~ Parse.spellsFailULong("18276556748091725275")  ==== true
    T ~ Parse.spellsFailULong("+0018276556748091725275") ==== true
    T ~ Parse.spellsFailULong("18276556748091725274")  ==== false
    T ~ Parse.spellsFailULong("8276556748091725275")   ==== false
    T ~ Parse.spellsFailULong("18446744073709551615")  ==== false
    T ~ Parse.spellsFailULong("")                      ==== false
    T ~ Parse.spellsFailHex("c7a16d3e52b84f19")        ==== true
    T ~ Parse.spellsFailHex("C7A16D3E52B84F19")        ==== true      // case-insensitive
    T ~ Parse.spellsFailHex("00c7a16d3e52b84f19")      ==== true
    T ~ Parse.spellsFailHex("c7a16d3e52b84f18")        ==== false
    T ~ Parse.spellsFailHex("c7a16d3e52b84f190")       ==== false
    T ~ Parse.spellsFailHex("ffffffffffffffff")        ==== false
    T ~ Parse.spellsFailHex("")                        ==== false
    // The full discrimination idiom: sentinel plus verifier separates failure from true parse
    def longOk(s: String): Boolean = Parse.long(s) != Parse.failLong || Parse.spellsFailLong(s)
    T ~ longOk("-9170187325617826341") ==== true
    T ~ longOk("-9223372036854775808") ==== true
    T ~ longOk("9223372036854775808")  ==== false
    T ~ longOk("123")                  ==== true
    T ~ longOk("botch")                ==== false

  @Test
  def sourcesAgreeTest(): Unit =
    val r: Prng = Pcg64(81571350892354L)
    def check(s: String): Unit =
      val ab = s.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1)
      val ac = s.toCharArray
      val mb = Mem of ab
      val mc = Mem of ac
      T ~ Parse.long(ab)  ==== Parse.long(s)
      T ~ Parse.long(ac)  ==== Parse.long(s)
      T ~ Parse.long(mb)  ==== Parse.long(s)
      T ~ Parse.long(mc)  ==== Parse.long(s)
      T ~ Parse.uLong(ab) ==== Parse.uLong(s)
      T ~ Parse.uLong(ac) ==== Parse.uLong(s)
      T ~ Parse.uLong(mb) ==== Parse.uLong(s)
      T ~ Parse.uLong(mc) ==== Parse.uLong(s)
      T ~ Parse.hex(ab)   ==== Parse.hex(s)
      T ~ Parse.hex(ac)   ==== Parse.hex(s)
      T ~ Parse.hex(mb)   ==== Parse.hex(s)
      T ~ Parse.hex(mc)   ==== Parse.hex(s)
      T ~ Parse.spellsFailLong(ab)  ==== Parse.spellsFailLong(s)
      T ~ Parse.spellsFailLong(mc)  ==== Parse.spellsFailLong(s)
      T ~ Parse.spellsFailULong(ac) ==== Parse.spellsFailULong(s)
      T ~ Parse.spellsFailULong(mb) ==== Parse.spellsFailULong(s)
      T ~ Parse.spellsFailHex(ab)   ==== Parse.spellsFailHex(s)
      T ~ Parse.spellsFailHex(mc)   ==== Parse.spellsFailHex(s)
    check("-9223372036854775808")
    check("18446744073709551615")
    check("ffffffffffffffff")
    check("-9170187325617826341")
    check("18276556748091725275")
    check("c7a16d3e52b84f19")
    check("0")
    check("")
    check("nope")
    nFor(200){ _ =>
      val l = r.L
      check(l.toString)
      check(java.lang.Long.toUnsignedString(l))
      check(l.loHexString)
      check(l.hexString)
    }
}
