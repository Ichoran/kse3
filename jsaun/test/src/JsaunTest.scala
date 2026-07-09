// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.jsaun


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import sourcecode.{Line, given}

import kse.jsaun.{Jsonize, FromJson}


case class Pt(x: Double, y: Double) derives Jsonize, FromJson

case class WithOpt(a: Int, b: Option[String]) derives Jsonize, FromJson

case class Inner(z: Int)   // no derives: instances are derived on demand where needed

case class Auto(i: Inner, n: Int) derives Jsonize, FromJson

sealed trait Shape derives Jsonize, FromJson
case class Circle(r: Double) extends Shape
case class Sq(side: Double, label: String) extends Shape
case object Dot extends Shape

case class AB(a: Int, b: String) derives FromJson


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
  def formatPreservingTest(): Unit =
    val u8 = java.nio.charset.StandardCharsets.UTF_8
    val srcs = List(
      "{\"a\" : [ 1 , 2.50 , \"x\" ] ,\n\t\"b\":null,   \"c\"  :{ }}",
      "[1,2,3]",
      "[ ]",
      "{ }",
      "{\"weird\":[1e2,  0.30000000000000004],\"z\":[[],[ [] ]],\"esc\":\"a\\u0041b\"}",
      "[ 1.50, 2.5 ]"   // packed Jarr.D keeps its formatting too
    )
    for src <- srcs do
      T ~ Json.parseFmt(src).ask.map(_.print) ==== Is(src)
      T ~ java.util.Arrays.equals(Json.parseFmt(src.getBytes(u8)).jsonOr(Jnull).printBytes, src.getBytes(u8)) ==== true
      T ~ Json.M.parseFmt(src).ask.map(_.print) ==== Is(src)   // unedited mutable tree is verbatim too
    T ~ Json.parseFmt("  [ 1 ]  ").ask.map(_.print) ==== Is("[ 1 ]")   // outside the root value, whitespace is not kept
    T ~ Json.parseFmt("[1.50]").ask.map(j => Json.parse(j.print).ask == Is(j)) ==== Is(true)

  @Test
  def formatEditTest(): Unit =
    val src = """{
  "name": "widget",
  "count": 42,
  "tags": [ "a" , "b" , "c" ],
  "nested": { "deep": [1.5, 2.5] }
}"""
    def freshM(): Jobj.M = Json.M.parseFmt(src).jsonOr(Jnull) match
      case o: Jobj.M => o
      case _ => Jobj.M()
    // single value replacement => single-token diff
    val m1 = freshM()
    m1("count") = Jnum(43)
    T ~ m1.print ==== src.replace("42", "43")
    // deep edit: everything else verbatim
    val m2 = freshM()
    m2("nested")("deep").jsonOr(Jnull) match
      case a: Jarr.A.M => a(0) = Jnum(9.5)
      case _ => assertTrue("deep array not mutable", false)
    T ~ m2.print ==== src.replace("1.5,", "9.5,")
    // string replacement quotes/escapes fresh; neighbors verbatim
    val m3 = freshM()
    m3("name") = Jstr("gizmo")
    T ~ m3.print ==== src.replace("\"widget\"", "\"gizmo\"")
    // several edits in one pass
    val m4 = freshM()
    m4("count") = Jnum(43)
    m4("tags").jsonOr(Jnull) match
      case t: Jarr.A.M => t(1) = Jstr("B")
      case _ => assertTrue("tags not mutable", false)
    T ~ m4.print ==== src.replace("42", "43").replace("\"b\"", "\"B\"")
    // structural edit: the edited node regenerates its separators in its own inferred style
    val m5 = freshM()
    m5("tags").jsonOr(Jnull) match
      case t: Jarr.A.M => t.add(Jstr("d")) __ Unit
      case _ => assertTrue("tags not mutable", false)
    T ~ m5.print ==== src.replace("""[ "a" , "b" , "c" ]""", """[ "a" , "b" , "c" , "d" ]""")
    // cross-document splice: the guest subtree keeps ITS source's formatting
    val docA = """{ "keep" : [ 1 , 2 ] , "swap" : null }"""
    val docB = """[ 7 ,   8 ]"""
    val a = Json.M.parseFmt(docA).jsonOr(Jnull)
    val b = Json.parseFmt(docB).jsonOr(Jnull)
    a match
      case o: Jobj.M => o("swap") = b
      case _ => assertTrue("docA not mutable", false)
    T ~ a.print ==== docA.replace("null", docB)
    // edits still parse back to the right tree
    T ~ Json.parse(m1.print)("count").long ==== Is(43L)
    T ~ Json.parse(m5.print)("tags")(3).str ==== Is("d")

  @Test
  def styleTest(): Unit =
    T ~ Jarr(Jnum(1), Jnum(2)).print ==== "[1,2]"
    T ~ Jobj("a" -> Jnum(1), "b" -> Jarr(Jnum(1), Jnum(2))).print(using Jstyle.pretty) ====
        "{\n  \"a\": 1,\n  \"b\": [\n    1,\n    2\n  ]\n}"
    T ~ Jarr().print(using Jstyle.pretty)  ==== "[]"
    T ~ Jobj().print(using Jstyle.pretty)  ==== "{}"
    T ~ Jarr(Array(1.5, 2.5)).print(using Jstyle.pretty) ==== "[\n  1.5,\n  2.5\n]"
    // numeric policy: the shorter of rounded and exact wins, so 0.5 never grows
    T ~ Json.parse("[0.30000000000000004, 0.5]").jsonOr(Jnull).print(using Jstyle.compact.sig(4)) ==== "[0.3,0.5]"
    T ~ Jnum(0.5).print(using Jstyle.compact.sig(4))                  ==== "0.5"
    T ~ Jnum(0.30000000000000004).print(using Jstyle.compact.fixed(2)) ==== "0.3"
    T ~ Jnum(1.2345678901234568E29).print(using Jstyle.compact.sig(4)) ==== "1.235E+29"
    T ~ Jnum(7L).print(using Jstyle.compact.sig(2))                    ==== "7"   // Longs are already exact
    // verbatim beats style for untouched parsed tokens
    T ~ Json.parseFmt("[0.30000000000000004]").jsonOr(Jnull).print(using Jstyle.compact.sig(4)) ==== "[0.30000000000000004]"
    // ...but reprint restyles everything
    T ~ Json.parseFmt("[ 1 , 2 ]").jsonOr(Jnull).reprint(Jstyle.compact) ==== "[1,2]"
    T ~ Json.parseFmt("""{ "a" : 1 }""").jsonOr(Jnull).reprint(Jstyle.pretty) ==== "{\n  \"a\": 1\n}"
    T ~ Json.parseFmt("[0.30000000000000004]").jsonOr(Jnull).reprint(Jstyle.compact.sig(4)) ==== "[0.3]"

  @Test
  def styleInferenceTest(): Unit =
    // insertion into an indented array picks up the siblings' indentation
    val src = "{\n  \"tags\": [\n    \"a\",\n    \"b\"\n  ],\n  \"n\": 1\n}"
    val m = Json.M.parseFmt(src).jsonOr(Jnull)
    m("tags").jsonOr(Jnull) match
      case t: Jarr.A.M => t.add(Jstr("c")) __ Unit
      case _ => assertTrue("tags not mutable", false)
    T ~ m.print ==== src.replace("\"b\"\n  ]", "\"b\",\n    \"c\"\n  ]")
    // inline arrays keep their inline spacing
    val m2 = Json.M.parseFmt("""{ "xs" : [ 1 , 2 ] }""").jsonOr(Jnull)
    m2("xs").jsonOr(Jnull) match
      case t: Jarr.A.M => t.add(Jnum(3)) __ Unit
      case _ => assertTrue("xs not mutable", false)
    T ~ m2.print ==== """{ "xs" : [ 1 , 2 , 3 ] }"""
    // removal regenerates separators uniformly in the same style
    val m3 = Json.M.parseFmt("[ 1 , 2 , 3 ]").jsonOr(Jnull)
    m3 match
      case t: Jarr.A.M => t.remove(1) __ Unit
      case _ => assertTrue("array not mutable", false)
    T ~ m3.print ==== "[ 1 , 3 ]"
    // a new key matches the object's own spacing (separator synthesized from a singleton)
    val m4 = Json.M.parseFmt("""{ "a" : 1 }""").jsonOr(Jnull)
    m4 match
      case o: Jobj.M => o.put("b", Jnum(2)) __ Unit
      case _ => assertTrue("object not mutable", false)
    T ~ m4.print ==== """{ "a" : 1, "b" : 2 }"""
    // multiline object gains a key in its own layout
    val src5 = "{\n  \"a\": 1,\n  \"b\": 2\n}"
    val m5 = Json.M.parseFmt(src5).jsonOr(Jnull)
    m5 match
      case o: Jobj.M => o.put("c", Jnum(3)) __ Unit
      case _ => assertTrue("object not mutable", false)
    T ~ m5.print ==== "{\n  \"a\": 1,\n  \"b\": 2,\n  \"c\": 3\n}"
    // children of a structurally edited node still print verbatim from their source
    val src6 = "[ { \"deep\" : [ 1 , 2 ] } ]"
    val m6 = Json.M.parseFmt(src6).jsonOr(Jnull)
    m6 match
      case t: Jarr.A.M => t.add(Jnull) __ Unit
      case _ => assertTrue("array not mutable", false)
    T ~ m6.print ==== "[ { \"deep\" : [ 1 , 2 ] }, null ]"   // singleton: no separator to sample, ", " synthesized

  @Test
  def breadthTest(): Unit =
    // packed Float/Int backings interoperate with everything else
    T ~ Jarr(Array(1.5f, 2.5f)).print   ==== "[1.5,2.5]"
    T ~ (Jarr(Array(1.5f, 2.5f)) == Jarr(Jnum(1.5), Jnum(2.5)))  ==== true
    T ~ (Jarr(Array(1, 2)) == Jarr(Jnum(1), Jnum(2)))            ==== true
    T ~ (Jarr(Array(1, 2)) == Jarr(Array(1.0, 2.0)))             ==== true
    T ~ (Jarr(Array(1, 2)).## == Jarr(Jnum(1), Jnum(2)).##)      ==== true
    T ~ (Jarr(Array(1.5f)).## == Jarr(Array(1.5)).##)            ==== true
    T ~ Jarr(Array(7, 8))(1).long                 ==== Is(8L)
    T ~ Jarr(Array(1.5f)).dbls.map(_.toList)      ==== Is(List(1.5))
    T ~ Jarr(Array(3, 4)).print(using Jstyle.pretty) ==== "[\n  3,\n  4\n]"
    // Array[Char] source, plain and format-preserving
    val src = """{ "a" : [ 1 , 2.5 ] , "b" : "café" }"""
    T ~ Json.parse(src.toCharArray).ask                 ==== Json.parse(src).ask
    T ~ Json.parseFmt(src.toCharArray).ask.map(_.print) ==== Is(src)
    // compactFormat keeps regular layouts, normalizes irregular ones, and releases the source
    val uni = "{\n  \"a\": 1,\n  \"b\": 2\n}"
    T ~ Json.parseFmt(uni).jsonOr(Jnull).compactFormat().print   ==== uni
    T ~ Json.M.parseFmt(uni).jsonOr(Jnull).compactFormat().print ==== uni
    T ~ Json.parseFmt("[1,   2, 3]").jsonOr(Jnull).compactFormat().print ==== "[1,   2,   3]"

  @Test
  def codecTest(): Unit =
    T ~ Json(Pt(1.5, 2.5)).print                   ==== """{"x":1.5,"y":2.5}"""
    T ~ Json.parse("""{"x":1.5,"y":2.5}""").to[Pt] ==== Is(Pt(1.5, 2.5))
    T ~ Json(List(1, 2, 3)).print                  ==== "[1,2,3]"
    T ~ Json(Vector("a", "b")).print               ==== """["a","b"]"""
    T ~ Json.parse("[1,2,3]").to[List[Int]]        ==== Is(List(1, 2, 3))
    T ~ Json.parse("[1,2,3]").to[Vector[Long]]     ==== Is(Vector(1L, 2L, 3L))
    T ~ Json(Map("a" -> 1, "b" -> 2)).print        ==== """{"a":1,"b":2}"""
    T ~ Json.parse("""{"a":1,"b":2}""").to[Map[String, Int]] ==== Is(Map("a" -> 1, "b" -> 2))
    T ~ Json(Option(5)).print                      ==== "5"
    T ~ Json(None: Option[Int]).print              ==== "null"
    T ~ Json(Array(1.5, 2.5)).print                ==== "[1.5,2.5]"
    T ~ Json.parse("[1.5,2.5]").to[Array[Double]].map(_.toList) ==== Is(List(1.5, 2.5))
    T ~ Json.parse("[1.5,2.5]").to[List[Double]]   ==== Is(List(1.5, 2.5))   // packed Jarr.D path
    T ~ Json(WithOpt(1, None)).print               ==== """{"a":1,"b":null}"""
    T ~ Json(WithOpt(1, Some("x"))).print          ==== """{"a":1,"b":"x"}"""
    T ~ Json.parse("""{"a":1}""").to[WithOpt]          ==== Is(WithOpt(1, None))   // absent Option field
    T ~ Json.parse("""{"a":1,"b":null}""").to[WithOpt] ==== Is(WithOpt(1, None))
    T ~ Json.parse("""{"a":1,"b":"x"}""").to[WithOpt]  ==== Is(WithOpt(1, Some("x")))
    T ~ Json(Auto(Inner(3), 7)).print              ==== """{"i":{"z":3},"n":7}"""
    T ~ Json.parse("""{"i":{"z":3},"n":7}""").to[Auto] ==== Is(Auto(Inner(3), 7))
    val shapes: List[Shape] = List(Circle(2.0), Sq(1.0, "s"), Dot)
    for s <- shapes do
      T ~ Json.parse(Json(s).print).to[Shape]      ==== Is(s)
    T ~ Json(Circle(2.0): Shape).print             ==== """{"type":"Circle","r":2.0}"""
    T ~ Json(Dot: Shape).print                     ==== """{"type":"Dot"}"""
    T ~ bad(Json.parse("""{"type":"Tri"}""").to[Shape]) ==== true
    T ~ bad(Json.parse("""{"r":2.0}""").to[Shape])      ==== true   // no discriminator
    val e = errText(Json.parse("""{"a":"x","b":5}""").to[AB])
    T ~ e.contains("2 fields")   ==== true
    T ~ e.contains("\"a\"")      ==== true
    T ~ e.contains("\"b\"")      ==== true
    T ~ errText(Json.parse("""{"a":1}""").to[AB]).contains("missing key \"b\"") ==== true
    T ~ bad(Json.parse("[1, 2").to[List[Int]])     ==== true   // parse errors flow into decoding
    T ~ errText(Json.parse("""[1,"x",3]""").to[List[Int]]).contains("in element 1") ==== true

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
