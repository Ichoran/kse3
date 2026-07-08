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
