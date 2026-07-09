// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.jsaun


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import sourcecode.{Line, given}


@RunWith(classOf[JUnit4])
class JsaunTest {
  import kse.testutilities.TestUtilities.{given, _}
  import kse.basics.{given, _}
  import kse.flow.{given, _}
  import kse.jsaun.{given, _}

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

  private def rootJerr(e: Err): Jerr =
    e.underlying match
      case j: Jerr => j
      case x: ErrType.Explained => rootJerr(x.error)
      case u => throw new AssertionError("no Jerr at the root of: " + u)

  @Test
  def leafParsingTest(): Unit =
    T ~ Json.parse("null").ask            ==== Is(Jnull)
    T ~ Json.parse("true").ask            ==== Is(Jbool.True)
    T ~ Json.parse("false").ask           ==== Is(Jbool.False)
    T ~ Json.parse("\"hi\"").ask          ==== Is(Jstr("hi"))
    T ~ Json.parse("\"\"").ask            ==== Is(Jstr(""))
    T ~ Json.parse("42").ask              ==== Is(Jnum(42))
    T ~ Json.parse("-17").ask             ==== Is(Jnum(-17))
    T ~ Json.parse("0").ask               ==== Is(Jnum(0))
    T ~ Json.parse("-0").ask              ==== Is(Jnum(0))
    T ~ Json.parse("3.25").ask            ==== Is(Jnum(3.25))
    T ~ Json.parse("-2.5e-4").ask         ==== Is(Jnum(-2.5e-4))
    T ~ Json.parse("1E+2").ask            ==== Is(Jnum(100.0))
    T ~ Json.parse("0.5").ask             ==== Is(Jnum(0.5))
    T ~ Json.parse("0.000001234").ask     ==== Is(Jnum(0.000001234))
    T ~ Json.parse(" \t\r\n 5 \n ").long  ==== Is(5L)

  @Test
  def numberEdgeTest(): Unit =
    T ~ Json.parse("9223372036854775807").long   ==== Is(Long.MaxValue)
    T ~ Json.parse("-9223372036854775808").long  ==== Is(Long.MinValue)
    T ~ Json.parse("9223372036854775808").dbl    ==== Is(9.223372036854776E18)
    T ~ Json.parse("9007199254740993").long      ==== Is(9007199254740993L)
    T ~ Json.parse("1.7976931348623157e308").dbl ==== Is(Double.MaxValue)
    T ~ Json.parse("4.9e-324").dbl               ==== Is(Double.MinPositiveValue)
    T ~ Json.parse("1e400").dbl                  ==== Is(Double.PositiveInfinity)
    T ~ Json.parse("1e-400").dbl                 ==== Is(0.0)
    T ~ Json.parse("123456789012345678901234567890").dbl ==== Is(1.2345678901234568E29)
    T ~ Json.parse("0.30000000000000004").dbl    ==== Is(0.30000000000000004)
    T ~ Json.parse("2.2250738585072011e-308").dbl ==== Is(2.2250738585072011e-308)
    T ~ Json.parse("1e2").ask.map(_.print)       ==== Is("100.0")
    T ~ Json.parse("20").ask.map(_.print)        ==== Is("20")
    T ~ Jnum(3)                                  ==== Jnum(3.0)
    T ~ (Jnum(9007199254740993L) == Jnum(9007199254740992.0)) ==== false
    T ~ Jnum(Double.NaN).print                   ==== "null"

  @Test
  def stringEscapeTest(): Unit =
    T ~ Json.parse("\"a\\nb\"").str            ==== Is("a\nb")
    T ~ Json.parse("\"\\\"\\\\\\/\"").str      ==== Is("\"\\/")
    T ~ Json.parse("\"\\b\\f\\r\\t\"").str     ==== Is("\b\f\r\t")
    T ~ Json.parse("\"\\u0041\\u00e9\"").str   ==== Is("Aé")
    T ~ Json.parse("\"\\ud83d\\ude00\"").str   ==== Is("😀")
    T ~ Json.parse("\"café\"").str        ==== Is("café")
    T ~ Jstr("a\"b\n\u0001").print             ==== "\"a\\\"b\\n\\u0001\""
    T ~ Json.parse(Jstr("x\\y\"z\t\u0007").print).str ==== Is("x\\y\"z\t\u0007")

  @Test
  def structureParsingTest(): Unit =
    T ~ Json.parse("[]").ask       ==== Is(Jarr())
    T ~ Json.parse("{}").ask       ==== Is(Jobj())
    T ~ Json.parse("[ ]").ask      ==== Is(Jarr())
    T ~ Json.parse("[1, 2, 3]").ask ==== Is(Jarr(Jnum(1), Jnum(2), Jnum(3)))
    T ~ Json.parse("[[[]]]").ask   ==== Is(Jarr(Jarr(Jarr())))
    T ~ Json.parse("""{"a": 1, "b": [true, null]}""").ask ==== Is(Jobj("a" -> Jnum(1), "b" -> Jarr(Jbool.True, Jnull)))
    T ~ Json.parse("""{ "x" : { "y" : "z" } }""")("x")("y").str ==== Is("z")
    T ~ Json.parse("[1,2,3,4,5,6,7,8,9,10,11,12]").size ==== 12
    T ~ Json.parse("""{"a":{},"b":[]}""").size ==== 2

  @Test
  def accessChainTest(): Unit =
    val j = Json.parse("""{"stations": [{"id": "a1"}, {"id": "b2"}], "n": 2}""")
    T ~ j("stations")(1)("id").str          ==== Is("b2")
    T ~ j("stations")(0)("id").strOr("?")   ==== "a1"
    T ~ j("n").long                         ==== Is(2L)
    T ~ j("n").dbl                          ==== Is(2.0)
    T ~ j.size                              ==== 2
    T ~ bad(j("missing").str)               ==== true
    T ~ bad(j("stations")(5)("id").str)     ==== true
    T ~ bad(j("stations")(-1).str)          ==== true
    T ~ bad(j("n")("x").str)                ==== true
    T ~ bad(j("stations").str)              ==== true
    T ~ j("missing").strOr("?")             ==== "?"
    T ~ j("missing").longOr(-1L)            ==== -1L
    T ~ errText(j("missing").str).contains("no key \"missing\"")     ==== true
    T ~ errText(j("n")("x").str).contains("found number")            ==== true
    T ~ errText(j("stations").str).contains("expected a string")     ==== true
    // A parse error flows through the whole access chain unchanged
    val e = Json.parse("[1, 2")
    T ~ bad(e("a")(3).str)                  ==== true
    T ~ errText(e("a")(3).str)              ==== errText(e.ask)
    // Boundary-style unwrap
    T ~ Ask{ j("stations")(1)("id").json.strOr("?") } ==== Is("b2")

  @Test
  def duplicateKeyTest(): Unit =
    T ~ Json.parse("""{"k": 1, "k": 2}""")("k").long ==== Is(2L)
    val big = Json.parse("""{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6,"g":7,"h":8,"k":1,"k":9}""")
    T ~ big("k").long ==== Is(9L)   // indexed path (>= 8 keys) must also be last-wins
    T ~ big("a").long ==== Is(1L)
    T ~ big.size      ==== 10

  @Test
  def strictnessTest(): Unit =
    T ~ bad(Json.parse("").ask)          ==== true
    T ~ bad(Json.parse("   ").ask)       ==== true
    T ~ bad(Json.parse("[1, 2").ask)     ==== true
    T ~ bad(Json.parse("[1 2]").ask)     ==== true
    T ~ bad(Json.parse("[1,]").ask)      ==== true
    T ~ bad(Json.parse("[,1]").ask)      ==== true
    T ~ bad(Json.parse("""{"a" 1}""").ask)   ==== true
    T ~ bad(Json.parse("""{"a":}""").ask)    ==== true
    T ~ bad(Json.parse("""{"a":1,}""").ask)  ==== true
    T ~ bad(Json.parse("""{a: 1}""").ask)    ==== true
    T ~ bad(Json.parse("01").ask)        ==== true
    T ~ bad(Json.parse("+1").ask)        ==== true
    T ~ bad(Json.parse("1.").ask)        ==== true
    T ~ bad(Json.parse(".5").ask)        ==== true
    T ~ bad(Json.parse("-").ask)         ==== true
    T ~ bad(Json.parse("1e").ask)        ==== true
    T ~ bad(Json.parse("1e+").ask)       ==== true
    T ~ bad(Json.parse("NaN").ask)       ==== true
    T ~ bad(Json.parse("truu").ask)      ==== true
    T ~ bad(Json.parse("tru").ask)       ==== true
    T ~ bad(Json.parse("\"ab").ask)      ==== true
    T ~ bad(Json.parse("\"a\\x\"").ask)  ==== true
    T ~ bad(Json.parse("\"a\\u12g4\"").ask) ==== true
    T ~ bad(Json.parse("\"a\\u12\"").ask)   ==== true
    T ~ bad(Json.parse("\"a\nb\"").ask)  ==== true
    T ~ bad(Json.parse("[1] x").ask)     ==== true
    T ~ bad(Json.parse("[1] [2]").ask)   ==== true
    T ~ Json.parse("2.5e2").dbl          ==== Is(250.0)   // and legal odd-looking things still work
    T ~ Json.parse("[1 ,\t2 ]").ask      ==== Is(Jarr(Jnum(1), Jnum(2)))

  @Test
  def errorReportingTest(): Unit =
    Json.parse("{\"a\": [1, 2, x]}").ask match
      case Alt(e) =>
        val text = e.toString
        T ~ text.contains("in element 2 of array")      ==== true
        T ~ text.contains("in value for key \"a\"")     ==== true
        T ~ text.contains("expected a JSON value")      ==== true
        val root = rootJerr(e)
        T ~ root.pos  ==== 13
        T ~ root.line ==== 1
        T ~ root.col  ==== 14
      case v => assertTrue("unexpected success: " + v, false)
    Json.parse("[\n  4,\n  ?\n]").ask match
      case Alt(e) =>
        val root = rootJerr(e)
        T ~ root.line ==== 3
        T ~ root.col  ==== 3
        T ~ e.toString.contains("^") ==== true   // caret-marked excerpt
      case v => assertTrue("unexpected success: " + v, false)
    T ~ errText(Json.parse("[" * 600).ask).contains("512 levels") ==== true
    T ~ errText(Json.parse("\u0007").ask).contains("control character 7") ==== true

  @Test
  def printRoundTripTest(): Unit =
    T ~ Json.parse("""{ "a" : [ 1 , 2.5 , "x" ] , "b" : null }""").ask.map(_.print) ==== Is("""{"a":[1,2.5,"x"],"b":null}""")
    T ~ Json.parse("[true,false,null]").ask.map(_.print) ==== Is("[true,false,null]")
    val srcs = List(
      """{"nested": {"deep": [[1], [2, [3, {"four": 4.5e-2}]]], "s": "a\nbc"}}""",
      """[0, -1, 9223372036854775807, 0.1, 1e300, "😀"]""",
      """{"empty": {}, "also": [], "dup": 1, "dup": 2}"""
    )
    for src <- srcs do
      val orig = Json.parse(src).jsonOr(Jnull)
      T ~ orig.isNull                  ==== false
      T ~ Json.parse(orig.print).ask   ==== Is(orig)

  @Test
  def bytesSourceTest(): Unit =
    val u8 = java.nio.charset.StandardCharsets.UTF_8
    val srcs = List(
      """{"a": [1, 2.5, "x"], "b": null, "c": true}""",
      """[0.1, -3e8, 9223372036854775807, 123456789012345678901234567890]""",
      "  [ [ ] , { } , \"\" ]  ",
      "{\"k\": \"caf\\u00e9 café\", \"emoji\": \"😀\", \"nl\": \"a\\nb\"}"
    )
    for s <- srcs do
      T ~ Json.parse(s.getBytes(u8)).ask ==== Json.parse(s).ask
    T ~ Json.parse("\"café\"".getBytes(u8)).str  ==== Is("café")
    T ~ Json.parse("\"caf\\u00e9\"".getBytes(u8)).str ==== Is("café")
    // for ASCII input, byte positions == char positions, so errors render identically
    val badIn = """{"a": [1, 2, x]}"""
    T ~ errText(Json.parse(badIn.getBytes(u8)).ask) ==== errText(Json.parse(badIn).ask)
    T ~ Json.parse("[1.5, 2.5]".getBytes(u8), exact = true).ask ==== Json.parse("[1.5, 2.5]").ask

  @Test
  def exactModeTest(): Unit =
    T ~ Json.parse("0.1", exact = true).ask.map(_.print)  ==== Is("0.1")
    T ~ Json.parse("0.1", exact = true).dbl                ==== Is(0.1)
    T ~ Json.parse("3.5", exact = true).ask                ==== Is(Jnum(3.5))
    T ~ Json.parse("3.5", exact = true).ask.map(_.isInstanceOf[Jnum.D])  ==== Is(true)
    T ~ Json.parse("1e22", exact = true).ask.map(_.isInstanceOf[Jnum.D]) ==== Is(true)   // 10^22 = 2^22 * 5^22 fits a Double
    T ~ Json.parse("42", exact = true).ask                 ==== Is(Jnum(42))
    T ~ Json.parse("0.30000000000000001", exact = true).ask.map(_.print) ==== Is("0.30000000000000001")
    T ~ Json.parse("123456789012345678901234567890", exact = true).ask.map(_.print) ==== Is("123456789012345678901234567890")
    T ~ Json.parse("123456789012345678901234567890", exact = true).dbl   ==== Is(1.2345678901234568E29)
    T ~ Json.parse("1e400", exact = true).ask.map(_.print) ==== Is("1e400")
    T ~ bad(Json.parse("10000000000000000001", exact = true).long)       ==== true
    T ~ Json.parse("[0.1, 2.5, 10000000000000000001, 1.0e-3]", exact = true).ask.map(_.print) ====
        Is("[0.1,2.5,10000000000000000001,1.0e-3]")   // 10^-3 is not dyadic, so 1.0e-3 stays textual too
    val big = Json.parse("0.1", exact = true).jsonOr(Jnull)
    T ~ big.isInstanceOf[Jnum.Big]  ==== true
    T ~ (big == Jnum(0.1))          ==== false   // decimal 0.1 is not the binary 0.1 Double
    T ~ (Jnum(BigDecimal("3")) == Jnum(3))         ==== true
    T ~ (Jnum(BigDecimal("3")).## == Jnum(3).##)   ==== true
    T ~ (Jnum(BigDecimal("3.5")) == Jnum(3.5))     ==== true
    T ~ (Jnum(BigDecimal("3.5")).## == Jnum(3.5).##) ==== true
    T ~ Jnum(BigDecimal("9007199254740993")).long  ==== Is(9007199254740993L)

  @Test
  def packedArrayTest(): Unit =
    val packed = Json.parse("[1.5, 2.5, 3.5]").jsonOr(Jnull)
    T ~ packed.isInstanceOf[Jarr.D]  ==== true
    T ~ (packed == Jarr(Jnum(1.5), Jnum(2.5), Jnum(3.5)))  ==== true
    T ~ (Jarr(Jnum(1.5), Jnum(2.5), Jnum(3.5)) == packed)  ==== true
    T ~ (packed.## == Jarr(Jnum(1.5), Jnum(2.5), Jnum(3.5)).##)  ==== true
    T ~ packed(1).dbl      ==== Is(2.5)
    T ~ bad(packed(3).dbl) ==== true
    T ~ packed.print       ==== "[1.5,2.5,3.5]"
    T ~ Json.parse(packed.print).ask ==== Is(packed)
    T ~ Json.parse("[1, 2.5]").ask.map(_.isInstanceOf[Jarr.D])  ==== Is(false)   // a Long element blocks packing
    T ~ Json.parse("[]").ask.map(_.isInstanceOf[Jarr.D])        ==== Is(false)
    T ~ Json.parse("[1.5, 2.5]").arr.flatMap(_.dbls).map(_.toList) ==== Is(List(1.5, 2.5))
    T ~ Json.parse("[1, 2, 3]").arr.flatMap(_.dbls).map(_.toList)  ==== Is(List(1.0, 2.0, 3.0))
    T ~ bad(Json.parse("[1, \"x\"]").arr.flatMap(_.dbls))          ==== true
    T ~ (Jarr(Array(1.5, 2.5)) == Jarr(Jnum(1.5), Jnum(2.5)))      ==== true

  @Test
  def printBytesTest(): Unit =
    val u8 = java.nio.charset.StandardCharsets.UTF_8
    val trees = List[Json](
      Json.parse("""{"a":[1,2.5,"x"],"b":null,"c":[true,false]}""").jsonOr(Jnull),
      Jstr("café 😀 plain"),
      Jstr("esc \t \"q\" \\ " + 1.toChar),
      Jobj("k€y" -> Jarr(Jnum(1.5), Jstr("😀")), "n" -> Jnum(-7)),
      Jnum(0.30000000000000004)
    )
    for j <- trees do
      T ~ new String(j.printBytes, u8) ==== j.print
    T ~ Json.parse(Jstr("café 😀").printBytes).str ==== Is("café 😀")

  @Test
  def mutableTreeTest(): Unit =
    val root = Json.M.parse("""{"a": [1, 2.5], "b": {"c": true}}""").jsonOr(Jnull)
    T ~ root.isInstanceOf[Jobj.M] ==== true
    T ~ root.isInstanceOf[Json.M] ==== true
    val m = root match
      case m: Jobj.M => m
      case _ => Jobj.M()
    m("a").jsonOr(Jnull) match
      case am: Jarr.A.M => am.add(Jstr("x")) __ Unit
      case _ => assertTrue("array did not parse as mutable", false)
    T ~ m("a").size     ==== 3
    T ~ m("a")(2).str   ==== Is("x")
    m("b").jsonOr(Jnull) match
      case bm: Jobj.M => bm("c") = Jbool.False
      case _ => assertTrue("object did not parse as mutable", false)
    T ~ m("b")("c").bool ==== Is(false)
    m.put("d", Jnum(4)) __ Unit
    T ~ m("d").long ==== Is(4L)
    T ~ m.size      ==== 3
    m("a") = Jnull
    T ~ m("a").isNull   ==== true
    T ~ m.remove("a")   ==== 1
    T ~ m.size          ==== 2
    // the upcast view is the same object: later edits show through (no copies, ever)
    val view: Jobj = m
    m.put("e", Jnum(5)) __ Unit
    T ~ view("e").long  ==== Is(5L)
    // matching in the mutable hierarchy
    T ~ (Json.M.parse("[1]").jsonOr(Jnull) match { case _: Jarr.M => "arr.m"; case _ => "?" }) ==== "arr.m"
    // mutable mode does not pack numeric arrays; immutable parse has no mutable types
    T ~ Json.M.parse("[1.5, 2.5]").ask.map(_.isInstanceOf[Jarr.D])   ==== Is(false)
    T ~ Json.M.parse("[1.5, 2.5]").ask.map(_.isInstanceOf[Jarr.A.M]) ==== Is(true)
    T ~ Json.parse("""{"a":[1]}""").ask.map(_.isInstanceOf[Json.M])  ==== Is(false)
    T ~ Json.M.parse("[]").ask.map(_.isInstanceOf[Jarr.A.M])         ==== Is(true)
    T ~ Json.M.parse("{}").ask.map(_.isInstanceOf[Jobj.M])           ==== Is(true)

  @Test
  def mutatorsTest(): Unit =
    val a = Jarr.A.M(Jnum(1), Jnum(2))
    a.insert(1, Jstr("mid")) __ Unit
    T ~ a.print      ==== """[1,"mid",2]"""
    T ~ a.remove(0)  ==== Jnum(1)
    T ~ a.print      ==== """["mid",2]"""
    a(0) = Jbool.True
    T ~ a.print      ==== "[true,2]"
    T ~ a.clear().size ==== 0
    val d = Jarr.D.M(1.5, 2.5)
    d.add(3.5) __ Unit
    d(0) = 0.5
    T ~ d.print      ==== "[0.5,2.5,3.5]"
    T ~ d.remove(1)  ==== 2.5
    T ~ ((d: Jarr) == Jarr(Array(0.5, 3.5))) ==== true
    val o = Jobj.M()
    for k <- 'a'.toInt to 'j'.toInt do o.add(k.toChar.toString, Jnum((k - 'a').toLong)) __ Unit
    T ~ o("j").long  ==== Is(9L)     // indexed lookup (10 keys)
    o.put("j", Jnum(99)) __ Unit     // must invalidate the index
    T ~ o("j").long  ==== Is(99L)
    o.add("j", Jnum(100)) __ Unit    // deliberate duplicate: last wins
    T ~ o("j").long  ==== Is(100L)
    T ~ o.remove("j") ==== 2
    T ~ o.contains("j") ==== false
    T ~ o.size       ==== 9

  @Test
  def equalityAndKindTest(): Unit =
    T ~ Jobj("a" -> Jnum(1), "b" -> Jnum(2))  ==== Jobj("b" -> Jnum(2), "a" -> Jnum(1))
    T ~ (Jobj("a" -> Jnum(1)) == Jobj("a" -> Jnum(2)))         ==== false
    T ~ (Jarr(Jnum(1), Jnum(2)) == Jarr(Jnum(2), Jnum(1)))     ==== false
    T ~ (Jobj("a" -> Jnum(1), "b" -> Jnum(2)).## == Jobj("b" -> Jnum(2), "a" -> Jnum(1)).##) ==== true
    T ~ (Jnum(3).## == Jnum(3.0).##)          ==== true
    T ~ Json.parse("null").ask.map(_.kind)    ==== Is("null")
    T ~ Json.parse("true").ask.map(_.kind)    ==== Is("boolean")
    T ~ Json.parse("1").ask.map(_.kind)       ==== Is("number")
    T ~ Json.parse("\"\"").ask.map(_.kind)    ==== Is("string")
    T ~ Json.parse("[]").ask.map(_.kind)      ==== Is("array")
    T ~ Json.parse("{}").ask.map(_.kind)      ==== Is("object")
    T ~ Json.parse("null").isNull             ==== true
    T ~ Json.parse("nul").isNull              ==== false
}
