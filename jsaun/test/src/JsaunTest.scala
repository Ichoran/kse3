// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.jsaun


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._


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

case class Empty() derives Jsonize, FromJson


@RunWith(classOf[JUnit4])
class JsaunTest {
  import kse.basics.testutilities.TestUtilities.{given, _}
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
    T ~ Jnum(Double.NaN).print                   ==== "NaN"   // non-finite policy: see nonFiniteTest

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
    T ~ Json.parse("NaN").dbl.map(_.isNaN) ==== Is(true)   // the ONE deliberate extension; see nonFiniteTest
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
        T ~ root.pos  ==== 13L
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
  def memSourceTest(): Unit =
    val u8 = java.nio.charset.StandardCharsets.UTF_8
    val srcs = List(
      """{"a": [1, 2.5, "x"], "b": null, "c": true}""",
      """[0.1, -3e8, 9223372036854775807, 123456789012345678901234567890]""",
      "  [ [ ] , { } , \"\" ]  ",
      "{\"k\": \"caf\\u00e9 café\", \"emoji\": \"😀\", \"nl\": \"a\\nb\"}"
    )
    for s <- srcs do
      // off-heap Mem parses identically to the on-heap Array of the same encoding
      T ~ Json.parse(Mem.of(s.getBytes(u8))).ask ==== Json.parse(s).ask
      T ~ Json.parse(Mem.of(s.toCharArray)).ask  ==== Json.parse(s).ask
    // strings decode identically from UTF-8 bytes and UTF-16 chars
    T ~ Json.parse(Mem.of("\"café 😀\"".getBytes(u8))).str ==== Is("café 😀")
    T ~ Json.parse(Mem.of("\"café 😀\"".toCharArray)).str  ==== Is("café 😀")
    // exact mode works from Mem (random-access, so revisiting number text is fine)
    T ~ Json.parse(Mem.of("0.1".getBytes(u8)), exact = true).ask.map(_.print) ==== Is("0.1")
    T ~ Json.parse(Mem.of("0.1".toCharArray),  exact = true).ask.map(_.print) ==== Is("0.1")
    // mutable-tree parse from Mem
    T ~ Json.M.parse(Mem.of("[1,2,3]".getBytes(u8))).ask.map(_.isInstanceOf[Jarr.A.M]) ==== Is(true)
    // format-preserving parse from Mem (snapshots to the heap) round-trips verbatim
    val fmtSrc = "{\"a\" : [ 1 , 2.50 ] , \"b\":null}"
    T ~ Json.parseFmt(Mem.of(fmtSrc.getBytes(u8))).ask.map(_.print)   ==== Is(fmtSrc)
    T ~ Json.parseFmt(Mem.of(fmtSrc.toCharArray)).ask.map(_.print)    ==== Is(fmtSrc)
    T ~ Json.M.parseFmt(Mem.of(fmtSrc.getBytes(u8))).ask.map(_.print) ==== Is(fmtSrc)
    // parse errors still flow through as errors
    T ~ bad(Json.parse(Mem.of("[1, 2".getBytes(u8))).ask) ==== true

  @Test
  def chunkedSourceTest(): Unit =
    val u8 = java.nio.charset.StandardCharsets.UTF_8
    val srcs = List(
      """{"a": [1, 2.5, "x"], "b": null, "c": true}""",
      """[0.1, -3e8, 9223372036854775807, 123456789012345678901234567890]""",
      "  [ [ ] , { } , \"\" ]  ",
      "{\"k\": \"caf\\u00e9 café\", \"emoji\": \"😀\", \"nl\": \"a\\nb\"}",
      "[" + ("\"" + "x" * 100 + "\",") * 20 + "0]"   // long tokens: forces window slides and regrowth
    )
    for s <- srcs do
      val ref = Json.parse(s).ask
      // InputStream and Reader through a deliberately tiny window
      T ~ Json.parse(new java.io.ByteArrayInputStream(s.getBytes(u8)), 16).ask ==== ref
      T ~ Json.parse(new java.io.StringReader(s), 16).ask                      ==== ref
      // and the default-sized window
      T ~ Json.parse(new java.io.ByteArrayInputStream(s.getBytes(u8))).ask     ==== ref
      // chunk iterators with awkward sizes, so tokens straddle chunk boundaries
      T ~ Json.parse(s.getBytes(u8).grouped(3), 16).ask ==== ref
      T ~ Json.parse(s.getBytes(u8).grouped(1), 16).ask ==== ref
      T ~ Json.parse(s.toCharArray.grouped(3), 16).ask  ==== ref
    // whole-window errors render exactly like the in-memory parse of the same bytes
    val badIn = """{"a": [1, 2, x]}"""
    T ~ errText(Json.parse(new java.io.ByteArrayInputStream(badIn.getBytes(u8))).ask) ==== errText(Json.parse(badIn).ask)
    T ~ errText(Json.parse(new java.io.StringReader(badIn)).ask)                      ==== errText(Json.parse(badIn).ask)
    T ~ errText(Json.parse(new java.io.StringReader("[1, 2")).ask)  ==== errText(Json.parse("[1, 2").ask)
    T ~ errText(Json.parse(new java.io.StringReader("[\"ab")).ask)  ==== errText(Json.parse("[\"ab").ask)
    // line/col stay exact even once newlines have slid out of a tiny window
    val ml = "[" + " " * 100 + "\n 4,\n ?\n]"
    Json.parse(new java.io.ByteArrayInputStream(ml.getBytes(u8)), 16).ask match
      case Alt(e) =>
        val root = rootJerr(e)
        T ~ root.line ==== 3
        T ~ root.col  ==== 2
      case v => assertTrue("unexpected success: " + v, false)
    // an IOException mid-stream comes back as an Err, not a throw
    class Boom extends java.io.InputStream {
      private var n = 0
      def read(): Int = throw new java.io.IOException("boom")
      override def read(dst: Array[Byte], off: Int, len: Int): Int =
        if n == 0 then { dst(off) = '['.toByte; n = 1; 1 } else throw new java.io.IOException("boom")
    }
    T ~ Json.parse(new Boom).isErr ==== true
    // mutable-tree parse from a stream
    Json.M.parse(new java.io.ByteArrayInputStream("""{"a": 1}""".getBytes(u8))).jsonOr(Jnull) match
      case o: Jobj.M =>
        o("b") = Jnum(2)
        T ~ o.size ==== 2
      case v => assertTrue("not a Jobj.M: " + v, false)
    // visitor over a stream: declined values skip through window slides without decoding
    val doc = """{"skip": [1, {"deep": [true, "no"]}, 2], "take": 7}"""
    class PickTake extends Jvisitor:
      var want = false
      var got = -1L
      override def key(k: String) = { want = k == "take"; want }
      override def num(l: Long) = if want then got = l
    val pt = new PickTake
    T ~ bad(Json.stream(new java.io.ByteArrayInputStream(doc.getBytes(u8)), 16)(pt)) ==== false
    T ~ pt.got ==== 7L
    T ~ bad(Json.stream(new java.io.StringReader(doc), 16)(new Jvisitor { override def objStart() = false })) ==== false
    // deep-nesting refusal still guards streaming input
    T ~ errText(Json.parse(new java.io.StringReader("[" * 600)).ask).contains("512 levels") ==== true

  @Test
  def linedSourceTest(): Unit =
    // lines are joined by an implied newline: same values as the joined text
    val lines = List("{", "  \"a\": [1, 2.5, \"x\\n\"],", "  \"b\": null", "}")
    val joined = lines.mkString("\n")
    T ~ Json.parse(lines).ask               ==== Json.parse(joined).ask
    T ~ Json.parse(lines.iterator).ask      ==== Json.parse(joined).ask
    T ~ Json.parse(Vector("[1,", "2]")).ask ==== Is(Jarr(Jnum(1), Jnum(2)))
    // lines carried inside other values, via an extractor
    case class Row(id: Int, text: String)
    val rows = List(Row(1, "["), Row(2, " 42,"), Row(3, " true"), Row(4, "]"))
    T ~ Json.parse(rows.iterator)(_.text).ask ==== Is(Jarr(Jnum(42), Jbool.True))
    // errors report exact (line, char within line); pos packs the pair
    Json.parse(List("[", "  4,", "  ?", "]")).ask match
      case Alt(e) =>
        val root = rootJerr(e)
        T ~ root.line ==== 3
        T ~ root.col  ==== 3
        T ~ root.pos  ==== ((2L << 32) | 2L)
        T ~ e.toString.contains("^") ==== true
      case v => assertTrue("unexpected success: " + v, false)
    // no token can span lines: the implied newline splits it
    T ~ Json.parse(List("[tr", "ue]")).isErr     ==== true
    T ~ Json.parse(List("[12", "34]")).isErr     ==== true
    T ~ Json.parse(List("[\"ab", "cd\"]")).isErr ==== true
    // but whitespace and structure cross lines freely, and blank/empty lines are fine
    T ~ Json.parse(List("", "  ", "[", "", "]", "  ", "")).ask ==== Is(Jarr())
    T ~ Json.parse(List("{}")).ask           ==== Is(Jobj())
    T ~ Json.parse(List.empty[String]).isErr ==== true
    T ~ Json.parse(List("[1]", "x")).isErr   ==== true
    T ~ errText(Json.parse(Iterator.fill(600)("[")).ask).contains("512 levels") ==== true
    // visitor mode: full visits and skips both track structure across lines
    T ~ bad(Json.stream(lines)(new Jvisitor {}))  ==== false
    T ~ bad(Json.stream(lines)(new Jvisitor { override def objStart() = false })) ==== false
    T ~ bad(Json.stream(rows.iterator)(_.text)(new Jvisitor {})) ==== false
    class PickB extends Jvisitor:
      var got = "?"
      override def key(k: String) = k == "b"
      override def str(s: String) = got = s
    val pb = new PickB
    T ~ bad(Json.stream(List("{\"a\": [1, {\"x\": 2}],", "\"b\": \"yes\",", "\"c\": 3}"))(pb)) ==== false
    T ~ pb.got ==== "yes"
    // mutable parse from lines
    Json.M.parse(List("{", "\"a\": 1", "}")).jsonOr(Jnull) match
      case o: Jobj.M =>
        o("b") = Jnum(2)
        T ~ o.size ==== 2
      case v => assertTrue("not a Jobj.M: " + v, false)

  @Test
  def streamVisitorTest(): Unit =
    val u8 = java.nio.charset.StandardCharsets.UTF_8
    // a visitor that records the whole event stream
    class Rec extends Jvisitor:
      val log = scala.collection.mutable.ArrayBuffer.empty[String]
      override def objStart() = { log += "{"; true }
      override def objEnd() = log += "}"
      override def arrStart() = { log += "["; true }
      override def arrEnd() = log += "]"
      override def key(k: String) = { log += s"k:$k"; true }
      override def index(i: Int) = { log += s"i:$i"; true }
      override def str(s: String) = log += s"s:$s"
      override def num(l: Long) = log += s"n:$l"
      override def num(d: Double) = log += s"n:$d"
      override def bool(b: Boolean) = log += s"b:$b"
      override def nul() = log += "z"
    val doc = """{"a":1,"b":[true,null,"x"]}"""
    val rec = new Rec
    T ~ bad(Json.stream(doc)(rec))                    ==== false
    T ~ rec.log.mkString(" ") ==== "{ k:a n:1 k:b [ i:0 b:true i:1 z i:2 s:x ] }"
    // all four in-memory sources drive the visitor identically
    def run(f: Rec => Any): Seq[String] = { val r = new Rec; f(r); r.log.toSeq }
    T ~ run(r => Json.stream(doc.getBytes(u8))(r))      ==== rec.log.toSeq
    T ~ run(r => Json.stream(doc.toCharArray)(r))       ==== rec.log.toSeq
    T ~ run(r => Json.stream(Mem.of(doc.getBytes(u8)))(r)) ==== rec.log.toSeq
    T ~ run(r => Json.stream(Mem.of(doc.toCharArray))(r))  ==== rec.log.toSeq

    // key skip: decline every key but "b"; the declined values are never decoded
    class PickB extends Jvisitor:
      var got = "?"
      var strs = 0
      override def key(k: String) = k == "b"
      override def str(s: String) = { got = s; strs += 1 }
    val pb = new PickB
    T ~ bad(Json.stream("""{"a":"nope","b":"want","c":"also"}""")(pb)) ==== false
    T ~ pb.got  ==== "want"
    T ~ pb.strs ==== 1

    // declined values are skipped structurally: braces/brackets inside skipped strings don't miscount
    class CountKeep extends Jvisitor:
      var strs = 0
      override def key(k: String) = k == "keep"
      override def str(s: String) = strs += 1
    val ck = new CountKeep
    val tricky = """{"drop":{"x":[1,2,{"y":"a]b}c"}],"z":"q\"}"},"keep":"yes","also":[1,2,3]}"""
    T ~ bad(Json.stream(tricky)(ck)) ==== false
    T ~ ck.strs ==== 1   // only "keep":"yes" reached; "drop" object and "also" array skipped whole

    // index skip: only element 1
    class Mid extends Jvisitor:
      val ns = scala.collection.mutable.ArrayBuffer.empty[String]
      override def index(i: Int) = i == 1
      override def num(l: Long) = ns += l.toString
    val mid = new Mid
    Json.stream("[10,20,30]")(mid) __ Unit
    T ~ mid.ns.mkString ==== "20"

    // whole-container skip via arrStart/objStart
    class NoArr extends Jvisitor:
      var strs = 0
      override def arrStart() = false
      override def str(s: String) = strs += 1
    val na = new NoArr
    T ~ bad(Json.stream("""["a","b","c"]""")(na)) ==== false
    T ~ na.strs ==== 0

    // malformed input still surfaces as an error
    T ~ bad(Json.stream("[1, 2")(new Jvisitor {}))        ==== true
    T ~ bad(Json.stream("""{"a" 1}""")(new Jvisitor {}))  ==== true
    T ~ bad(Json.stream("nope")(new Jvisitor {}))         ==== true

  @Test
  def builderTest(): Unit =
    val u8 = java.nio.charset.StandardCharsets.UTF_8
    // a no-tree, no-boxing custom decoder: the builder itself is a stateless recipe (here an
    // object, freely reused); zero() makes the per-walk state, expectations route and
    // type-check, build(b) assembles
    class PtState:
      var x = Double.NaN
      var y = Double.NaN
      var onY = false
    object PtBuilder extends Jbuilder[PtState, Pt]:
      def zero() = new PtState
      override def key(b: PtState, k: String) = k match
        case "x" => b.onY = false; Jexpect.D
        case "y" => b.onY = true; Jexpect.D
        case _ => Jexpect.Skip
      override def num(b: PtState, d: Double) = { if b.onY then b.y = d else b.x = d; Is.unit }
      def build(b: PtState): Ask[Pt] =
        if b.x.isNaN || b.y.isNaN then Alt(Err("missing x or y")) else Is(Pt(b.x, b.y))
    T ~ Json.build("""{"x":1.5,"y":2.5}""")(PtBuilder)                       ==== Is(Pt(1.5, 2.5))
    T ~ Json.build("""{"y":2.5,"junk":[{"deep":"]"}],"x":1.5}""")(PtBuilder) ==== Is(Pt(1.5, 2.5))
    T ~ Json.build("""{"x":1,"y":2}""")(PtBuilder)                           ==== Is(Pt(1.0, 2.0))   // D widens integers
    T ~ Json.build("""{"x":9.5,"y":8.5}""")(PtBuilder)                       ==== Is(Pt(9.5, 8.5))   // same instance, fresh state
    // build(b) decides success: missing fields are its call, reported through the same Ask
    T ~ errText(Json.build("""{"x":1.5}""")(PtBuilder)).contains("missing") ==== true
    // expectation mismatches fail the walk, positioned, with key context
    val e1 = errText(Json.build("""{"x":"oops","y":2.5}""")(PtBuilder))
    T ~ e1.contains("expected a number")         ==== true
    T ~ e1.contains("in value for key \"x\"")    ==== true
    // integer expectation: whole doubles convert (like Json.long), fractions and non-numbers fail
    class LongBox:
      var value = -1L
    object IdBuilder extends Jbuilder[LongBox, Long]:
      def zero() = new LongBox
      override def key(b: LongBox, k: String) = if k == "id" then Jexpect.L else Jexpect.Skip
      override def num(b: LongBox, l: Long) = { b.value = l; Is.unit }
      def build(b: LongBox): Ask[Long] = Is(b.value)
    T ~ Json.build("""{"id": 42}""")(IdBuilder)   ==== Is(42L)
    T ~ Json.build("""{"id": 42.0}""")(IdBuilder) ==== Is(42L)
    T ~ errText(Json.build("""{"id": 4.5}""")(IdBuilder)).contains("expected an integer") ==== true
    T ~ errText(Json.build("""{"id": "x"}""")(IdBuilder)).contains("expected an integer") ==== true
    // Str/Bool/Obj/Arr expectations enforce form; null satisfies none of them
    object Strict extends Jbuilder[Array[String], String]:
      def zero() = Array("")
      override def key(b: Array[String], k: String) = k match
        case "s" => Jexpect.Str
        case "b" => Jexpect.Bool
        case "o" => Jexpect.Obj
        case "a" => Jexpect.Arr
        case _ => Jexpect.Skip
      override def str(b: Array[String], v: String) = { b(0) = v; Is.unit }
      def build(b: Array[String]): Ask[String] = Is(b(0))
    T ~ Json.build("""{"s":"ok","b":true,"o":{"x":1},"a":[1]}""")(Strict) ==== Is("ok")
    T ~ bad(Json.build("""{"b":1}""")(Strict))    ==== true
    T ~ bad(Json.build("""{"o":[1]}""")(Strict))  ==== true
    T ~ bad(Json.build("""{"a":{}}""")(Strict))   ==== true
    T ~ bad(Json.build("""{"s":null}""")(Strict)) ==== true
    // B = A: accumulate straight into the result, build is just Is(b) -- no further allocation
    object Gather extends Jbuilder[scala.collection.mutable.ArrayBuffer[String], scala.collection.mutable.ArrayBuffer[String]]:
      def zero() = scala.collection.mutable.ArrayBuffer.empty[String]
      override def index(b: scala.collection.mutable.ArrayBuffer[String], i: Int) = Jexpect.Str
      override def str(b: scala.collection.mutable.ArrayBuffer[String], s: String) =
        b.addOne(s) __ Unit
        Is.unit
      def build(b: scala.collection.mutable.ArrayBuffer[String]) = Is(b)
    T ~ Json.build("""["a","b","c"]""")(Gather).map(_.toList) ==== Is(List("a", "b", "c"))
    // unboxed array elements via index expectations
    class DSum:
      var sum = 0.0
    object SumD extends Jbuilder[DSum, Double]:
      def zero() = new DSum
      override def index(b: DSum, i: Int) = Jexpect.D
      override def num(b: DSum, d: Double) = { b.sum += d; Is.unit }
      def build(b: DSum): Ask[Double] = Is(b.sum)
    T ~ Json.build("[1.5, 2, 0.5]")(SumD) ==== Is(4.0)
    // builders can refuse semantically bad values in well-formed JSON, as they arrive:
    // the walk fails right there, with the value's position and whose-key context
    object PickyId extends Jbuilder[LongBox, Long]:
      def zero() = new LongBox
      override def key(b: LongBox, k: String) = if k == "id" then Jexpect.L else Jexpect.Skip
      override def num(b: LongBox, l: Long) =
        if l < 0 then Alt(Err(s"id must be nonnegative: $l"))
        else { b.value = l; Is.unit }
      def build(b: LongBox): Ask[Long] = Is(b.value)
    T ~ Json.build("""{"id": 7}""")(PickyId) ==== Is(7L)
    val e2 = errText(Json.build("""{"junk": true, "id": -3}""")(PickyId))
    T ~ e2.contains("id must be nonnegative: -3")  ==== true
    T ~ e2.contains("at line 1, char 22")          ==== true
    T ~ e2.contains("in value for key \"id\"")     ==== true
    // ...and can refuse a whole container from its end callback, anchored at its start
    object Pair extends Jbuilder[DSum, Double]:
      def zero() = new DSum
      override def index(b: DSum, i: Int) = Jexpect.D
      override def num(b: DSum, d: Double) = { b.sum += d; Is.unit }
      override def arrEnd(b: DSum) = if b.sum == 0 then Alt(Err("elements sum to zero")) else Is.unit
      def build(b: DSum): Ask[Double] = Is(b.sum)
    T ~ Json.build("[1.0, 2.0]")(Pair)                                    ==== Is(3.0)
    T ~ errText(Json.build("[1.0, -1.0]")(Pair)).contains("sum to zero")  ==== true
    T ~ errText(Json.build("[1.0, -1.0]")(Pair)).contains("at line 1, char 1") ==== true
    // the builder runs over every source kind
    val doc = """{"x": -0.5, "y": 3.25}"""
    T ~ Json.build(doc.getBytes(u8))(PtBuilder)          ==== Is(Pt(-0.5, 3.25))
    T ~ Json.build(doc.toCharArray)(PtBuilder)           ==== Is(Pt(-0.5, 3.25))
    T ~ Json.build(Mem.of(doc.getBytes(u8)))(PtBuilder)  ==== Is(Pt(-0.5, 3.25))
    T ~ Json.build(new java.io.ByteArrayInputStream(doc.getBytes(u8)), 16)(PtBuilder) ==== Is(Pt(-0.5, 3.25))
    T ~ Json.build(new java.io.StringReader(doc))(PtBuilder)     ==== Is(Pt(-0.5, 3.25))
    T ~ Json.build(doc.getBytes(u8).grouped(3), 16)(PtBuilder)   ==== Is(Pt(-0.5, 3.25))
    T ~ Json.build(List("{\"x\": -0.5,", "\"y\": 3.25}"))(PtBuilder) ==== Is(Pt(-0.5, 3.25))
    T ~ Json.build(List(1, 2).iterator)(i => if i == 1 then "{\"x\": -0.5," else "\"y\": 3.25}")(PtBuilder) ==== Is(Pt(-0.5, 3.25))

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
  def objectIndexEditTest(): Unit =
    // interleaved keyed edits and lookups on a big object: the index is maintained across
    // edits, and removal punches holes that printing, iteration, and equality all skip
    val o = Jobj.M()
    var i = 0
    while i < 26 do
      o.add(('a' + i).toChar.toString, Jnum(i.toLong)) __ Unit
      i += 1
    T ~ o("z").long ==== Is(25L)
    i = 0
    while i < 26 do
      if (i & 1) == 1 then o.remove(('a' + i).toChar.toString) __ Unit
      i += 1
    T ~ o.size          ==== 13
    T ~ o("a").long     ==== Is(0L)
    T ~ o.contains("b") ==== false
    T ~ o.iterator.map(_._1).mkString ==== "acegikmoqsuwy"
    o.put("c", Jnum(99)) __ Unit     // in-place replacement keeps position
    T ~ o("c").long ==== Is(99L)
    o.put("b", Jnum(1)) __ Unit      // absent again, so it appends
    T ~ o.iterator.map(_._1).mkString ==== "acegikmoqsuwyb"
    // a hole-y object equals (and hashes and prints like) its freshly built equivalent
    val fresh = Jobj(o.iterator.toSeq*)
    T ~ ((o: Jobj) == fresh)     ==== true
    T ~ ((o: Jobj).## == fresh.##) ==== true
    T ~ o.print                  ==== fresh.print
    // removal-heavy objects print correctly in every style
    val p = Jobj.M()
    p.add("a", Jnum(1)).add("b", Jnum(2)).add("c", Jnum(3)) __ Unit
    p.remove("b") __ Unit
    T ~ p.print ==== """{"a":1,"c":3}"""
    T ~ p.print(using Jstyle.pretty) ==== "{\"a\": 1, \"c\": 3}"
    T ~ p.print(using Jstyle.pretty.fitTo(0)) ==== "{\n  \"a\": 1,\n  \"c\": 3\n}"
    p.remove("a") __ Unit
    p.remove("c") __ Unit
    T ~ p.print ==== "{}"
    T ~ p.print(using Jstyle.pretty) ==== "{}"
    // when a quarter of the capacity is holes, adding at the frontier compacts instead of growing
    val q = Jobj.M()
    i = 0
    while i < 8 do
      q.add(i.toString, Jnum(i.toLong)) __ Unit
      i += 1
    q.remove("0") __ Unit
    q.remove("1") __ Unit
    q.add("8", Jnum(8)) __ Unit
    T ~ q.size ==== 7
    T ~ q.iterator.map(_._1).mkString ==== "2345678"
    T ~ q("8").long ==== Is(8L)
    // sortKeys: lexicographic, stable for duplicates, last-wins lookup unaffected
    val s = Jobj.M()
    s.add("b", Jnum(1)).add("a", Jnum(2)).add("b", Jnum(3)).add("c", Jnum(4)) __ Unit
    T ~ s("b").long ==== Is(3L)
    s.sortKeys() __ Unit
    T ~ s.print ==== """{"a":2,"b":1,"b":3,"c":4}"""
    T ~ s("b").long ==== Is(3L)
    // sorting a hole-y indexed object updates the index positions
    val t = Jobj.M()
    i = 0
    while i < 12 do
      t.add(('a' + (11 - i)).toChar.toString, Jnum(i.toLong)) __ Unit
      i += 1
    T ~ t("a").long ==== Is(11L)
    t.remove("f") __ Unit
    t.sortKeys() __ Unit
    T ~ t.iterator.map(_._1).mkString ==== "abcdeghijkl"
    T ~ t("a").long ==== Is(11L)
    T ~ t("l").long ==== Is(0L)
    // removing from a format-preserved object demotes to the inferred separator style
    val f = Json.M.parseFmt("""{ "a" : 1 , "b" : 2 , "c" : 3 }""").jsonOr(Jnull)
    f match
      case fo: Jobj.M => fo.remove("b") __ Unit
      case _ => assertTrue("did not parse as a mutable object", false)
    T ~ f.print ==== """{ "a" : 1 , "c" : 3 }"""

  @Test
  def nonFiniteTest(): Unit =
    val u8 = java.nio.charset.StandardCharsets.UTF_8
    def no[A](a: Ask[A]): Boolean = a match
      case Alt(_) => true
      case _ => false
    // canonical scalar writing: the Double.toString / Python json / JSON5 spellings
    // (packed double ARRAYS quote non-finite instead -- see packModesTest)
    T ~ Jnum(Double.NaN).print              ==== "NaN"
    T ~ Jnum(Double.PositiveInfinity).print ==== "Infinity"
    T ~ Jnum(Double.NegativeInfinity).print ==== "-Infinity"
    T ~ Jarr(Array(Double.NaN, Double.PositiveInfinity, 1.5)).print ==== "[\"NaN\",\"Infinity\",1.5]"
    T ~ (new String(Jarr(Array(Double.NegativeInfinity)).printBytes, u8)) ==== "[\"-Infinity\"]"
    T ~ Jnum(Double.NaN).print(using Jstyle.compact.sig(4))                ==== "NaN"   // precision limits leave specials alone
    T ~ Jnum(Double.PositiveInfinity).print(using Jstyle.compact.fixed(2)) ==== "Infinity"
    // reading is liberal: optional sign, any case, inf or infinity
    T ~ Json.parse("NaN").dbl.map(_.isNaN)  ==== Is(true)
    T ~ Json.parse("nan").dbl.map(_.isNaN)  ==== Is(true)
    T ~ Json.parse("-NaN").dbl.map(_.isNaN) ==== Is(true)   // NaN flavors are not preserved
    T ~ Json.parse("Infinity").dbl  ==== Is(Double.PositiveInfinity)
    T ~ Json.parse("+Infinity").dbl ==== Is(Double.PositiveInfinity)
    T ~ Json.parse("-Infinity").dbl ==== Is(Double.NegativeInfinity)
    T ~ Json.parse("inf").dbl       ==== Is(Double.PositiveInfinity)
    T ~ Json.parse("-INF").dbl      ==== Is(Double.NegativeInfinity)
    T ~ Json.parse("iNfInItY").dbl  ==== Is(Double.PositiveInfinity)
    T ~ Json.parse("NaN", exact = true).dbl.map(_.isNaN) ==== Is(true)   // exact mode: a plain Jnum.D, never Big
    T ~ Json.parse("""{"x": Infinity}""").jsonOr(Jnull)("x").dbl ==== Is(Double.PositiveInfinity)
    T ~ Json.parse("[NaN, -inf, 2.5]").jsonOr(Jnull).isInstanceOf[Jarr.D] ==== false   // numbers, but not our array format: boxed (see packModesTest)
    // near misses are still errors, and wholly so
    T ~ no(Json.parse("Infinit").ask)  ==== true
    T ~ no(Json.parse("Infinite").ask) ==== true
    T ~ no(Json.parse("nana").ask)     ==== true
    T ~ no(Json.parse("infx").ask)     ==== true
    T ~ no(Json.parse("+1").ask)       ==== true   // the sign extension is for non-finite names only
    T ~ no(Json.parse("-").ask)        ==== true
    // round-trip, with one NaN equal to another
    T ~ (Jnum(Double.NaN) == Jnum(Double.NaN)) ==== true
    T ~ (Jnum(Double.NaN) == Jnum(1.0))        ==== false
    val t = Jarr(Array(Double.NaN, Double.NegativeInfinity, 0.5))
    T ~ Json.parse(t.print).ask ==== Is(t: Json)
    // typed encode/decode, including protobuf-style quoted names (but not quoted numbers)
    T ~ Json.print(Double.NaN)                         ==== "NaN"
    T ~ Json.print(List(1.5, Double.PositiveInfinity)) ==== "[1.5,Infinity]"
    T ~ Json.parse("NaN").to[Double].map(_.isNaN)      ==== Is(true)
    T ~ Json.parse("[1.5, Infinity]").to[Array[Double]].map(_(1)) ==== Is(Double.PositiveInfinity)
    T ~ Json.parse("\"NaN\"").to[Double].map(_.isNaN)  ==== Is(true)
    T ~ Json.parse("\"-Infinity\"").to[Double]         ==== Is(Double.NegativeInfinity)
    T ~ Json.parse("\"inf\"").to[Float]                ==== Is(Float.PositiveInfinity)
    T ~ no(Json.parse("\"1.5\"").to[Double])           ==== true
    T ~ no(Json.parse("\"nana\"").to[Double])          ==== true
    // format preservation keeps the original spelling; reprint canonicalizes
    T ~ Json.parseFmt("[ inf , 2.0 ]").jsonOr(Jnull).print ==== "[ inf , 2.0 ]"
    T ~ Json.parseFmt("[ inf ]").jsonOr(Jnull).reprint(Jstyle.compact) ==== "[Infinity]"
    // streaming and line-fed sources take the same tokens
    T ~ Json.parse(new java.io.StringReader("[-inf, NaN, 1.5]")).ask ==== Json.parse("[-inf, NaN, 1.5]").ask
    T ~ Json.parse(List("[NaN,", " inf]")).ask ==== Json.parse("[NaN, inf]").ask
    // the visitor descent delivers non-finite through the unboxed Double callback
    var seen = 0.0
    val vis = new Jvisitor {
      override def num(value: Double): Unit = seen += (if value.isNaN then 1.0 else if value.isInfinite then 2.0 else 0.5)
    }
    T ~ no(Json.stream("[NaN, inf, -Infinity, 1.5]")(vis)) ==== false
    T ~ seen ==== 5.5
    // a double-EXPECTING context reads null as NaN (JS's JSON.stringify writes null for
    // every non-finite); the tree itself keeps the null, and Option[Double] still sees None
    T ~ Json.parse("null").to[Double].map(_.isNaN)  ==== Is(true)
    T ~ Json.parse("null").to[Float].map(_.isNaN)   ==== Is(true)
    T ~ Json.parse("null").to[Option[Double]]       ==== Is(None)
    T ~ Json.parse("[1.5, null]").to[Array[Double]].map(_(1).isNaN) ==== Is(true)
    T ~ Json.parse("[1.5, null]").to[List[Double]].map(_(1).isNaN)  ==== Is(true)
    T ~ Json.parse("[1.5, null]").jsonOr(Jnull).arr.flatMap(_.dbls).map(_(1).isNaN) ==== Is(true)
    T ~ Json.parse("[1.5, null]").jsonOr(Jnull).isInstanceOf[Jarr.D] ==== false   // null is not our array format: boxed, so it prints as itself
    T ~ Json.parse("[1.5, null]").jsonOr(Jnull).print ==== "[1.5,null]"
    T ~ no(Json.parse("null").to[Long])             ==== true   // integers have no NaN analogue
    class DSum:
      var sum = 0.0
    object SumD extends Jbuilder[DSum, Double]:
      def zero() = new DSum
      override def index(b: DSum, i: Int) = Jexpect.D
      override def num(b: DSum, d: Double) = { b.sum += d; Is.unit }
      def build(b: DSum): Ask[Double] = Is(b.sum)
    T ~ Json.build("[1.5, null, inf]")(SumD).map(_.isNaN) ==== Is(true)   // Jexpect.D takes both
    // quoted non-finite names work in every doubles context too, arrays included --
    // but quoted finite numbers stay errors everywhere
    T ~ Json.parse("[1.5, \"Infinity\"]").to[Array[Double]].map(_(1)) ==== Is(Double.PositiveInfinity)
    T ~ Json.parse("[1.5, \"NaN\"]").to[List[Double]].map(_(1).isNaN) ==== Is(true)
    T ~ Json.parse("[1.5, \"-inf\"]").jsonOr(Jnull).arr.flatMap(_.dbls).map(_(1)) ==== Is(Double.NegativeInfinity)
    T ~ no(Json.parse("[1.5, \"2.5\"]").to[Array[Double]]) ==== true
    T ~ no(Json.parse("[1.5, \"true\"]").to[List[Double]]) ==== true
    T ~ Json.build("[1.5, \"inf\", \"NaN\"]")(SumD).map(_.isNaN) ==== Is(true)
    T ~ no(Json.build("[1.5, \"2.5\"]")(SumD)) ==== true
    T ~ Json.parse("[1.5, \"Infinity\"]").jsonOr(Jnull).print ==== "[1.5,\"Infinity\"]"   // packs (our array format) and reprints as it arrived

  @Test
  def packModesTest(): Unit =
    def kindOf(text: String)(using Jarr.Pack): String = Json.parse(text).jsonOr(Jnull) match
      case _: Jarr.D => "D"
      case _: Jarr.A => "A"
      case _ => "?"
    def rt(text: String)(using Jarr.Pack): String = Json.parse(text).jsonOr(Jnull).print
    // our array-of-doubles format: finite numbers bare, non-finite as the quoted exact names
    T ~ Jarr(Array(1.5, Double.NaN, Double.PositiveInfinity)).print ==== "[1.5,\"NaN\",\"Infinity\"]"
    // Standard (the default): pack exactly what we would have written, so it reprints as it arrived
    T ~ kindOf("[1.5, 2.5]")                   ==== "D"
    T ~ kindOf("""[1.5, "NaN"]""")             ==== "D"
    T ~ rt("""[1.5, "NaN"]""")                 ==== "[1.5,\"NaN\"]"
    T ~ kindOf("""["Infinity", "Infinity"]""") ==== "D"   // packs even if they were meant as strings...
    T ~ rt("""["Infinity", "Infinity"]""")     ==== "[\"Infinity\",\"Infinity\"]"   // ...because it reprints faithfully
    T ~ Json.parse("""["Infinity", "Infinity"]""").jsonOr(Jnull).arr.flatMap(_.dbls).map(_.toList) ====
        Is(List(Double.PositiveInfinity, Double.PositiveInfinity))
    // the round trip that motivates the default: our own serialized Array[Double] comes back packed
    T ~ Json.parse(Jarr(Array(1.5, Double.NaN)).print).jsonOr(Jnull).isInstanceOf[Jarr.D] ==== true
    // everything else stays unpacked, and so prints as itself: bare non-finite tokens,
    // nulls, lenient spellings, plain strings, exact integers
    T ~ kindOf("[1.5, NaN]")         ==== "A"
    T ~ rt("[1.5, NaN]")             ==== "[1.5,NaN]"
    T ~ kindOf("[1.5, null]")        ==== "A"
    T ~ rt("[1.5, null]")            ==== "[1.5,null]"
    T ~ kindOf("""["inf", "inf"]""") ==== "A"
    T ~ rt("""["inf", "inf"]""")     ==== "[\"inf\",\"inf\"]"
    T ~ kindOf("""["NaN", "x"]""")   ==== "A"
    T ~ kindOf("[1, 2]")             ==== "A"   // integers keep their exact Jnum.L, as always
    // packed-with-quotes equals its boxed twin: equality is value-level either way
    T ~ (Json.parse("""[1.5, "NaN"]""").jsonOr(Jnull) == Jarr(Jnum(1.5), Jnum(Double.NaN))) ==== true
    // Faithful: JSON-as-JSON; only plain finite numbers pack
    locally {
      given Jarr.Pack = Jarr.Pack.Faithful
      T ~ kindOf("[1.5, 2.5]")       ==== "D"
      T ~ kindOf("""[1.5, "NaN"]""") ==== "A"
      T ~ rt("""[1.5, "NaN"]""")     ==== "[1.5,\"NaN\"]"   // the string stays a string
    }
    // IfPossible: any workable Double interpretation packs (same rules as typed decode);
    // output canonicalizes to our format
    locally {
      given Jarr.Pack = Jarr.Pack.IfPossible
      T ~ kindOf("""[NaN, null, "inf", 1.5]""") ==== "D"
      T ~ rt("""[NaN, null, "inf", 1.5]""")     ==== "[\"NaN\",\"NaN\",\"Infinity\",1.5]"
      T ~ kindOf("""["hello"]""")               ==== "A"   // still only numbers-in-disguise
    }
    // format preservation is orthogonal: verbatim reprint regardless of packing
    T ~ Json.parseFmt("""[ 1.5 , "NaN" ]""").jsonOr(Jnull).isInstanceOf[Jarr.D] ==== true
    T ~ Json.parseFmt("""[ 1.5 , "NaN" ]""").jsonOr(Jnull).print ==== """[ 1.5 , "NaN" ]"""
    T ~ Json.parseFmt("""[ 1.5 , "NaN" ]""").jsonOr(Jnull).reprint(Jstyle.compact) ==== "[1.5,\"NaN\"]"
    // the fit-aware pretty printer speaks the format too
    T ~ Json.parse("""[1.5, "NaN", 2.5]""").jsonOr(Jnull).print(using Jstyle.pretty) ==== "[1.5, \"NaN\", 2.5]"
    // typed decode reads any of it, boxed or packed
    T ~ Json.parse("[1.5, null, 2.5]").to[Array[Double]].map(_(1).isNaN) ==== Is(true)
    T ~ Json.parse("""[1.5, "NaN"]""").to[Array[Double]].map(_(1).isNaN) ==== Is(true)

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
        "{\"a\": 1, \"b\": [1, 2]}"   // fits at width 100, so one line it is
    T ~ Jobj("a" -> Jnum(1), "b" -> Jarr(Jnum(1), Jnum(2))).print(using Jstyle.pretty.fitTo(0)) ====
        "{\n  \"a\": 1,\n  \"b\": [\n    1,\n    2\n  ]\n}"   // fitTo(0) is the classic one-per-line layout
    T ~ Jarr().print(using Jstyle.pretty)  ==== "[]"
    T ~ Jobj().print(using Jstyle.pretty)  ==== "{}"
    T ~ Jarr(Array(1.5, 2.5)).print(using Jstyle.pretty) ==== "[1.5, 2.5]"
    T ~ Jarr(Array(1.5, 2.5)).print(using Jstyle.pretty.fitTo(0)) ==== "[\n  1.5,\n  2.5\n]"
    // numeric policy: shortest within the don't-care tolerance (Ryu.fmt), so 0.5 never grows
    T ~ Json.parse("[0.30000000000000004, 0.5]").jsonOr(Jnull).print(using Jstyle.compact.sig(4)) ==== "[0.3,0.5]"
    T ~ Jnum(0.5).print(using Jstyle.compact.sig(4))                  ==== "0.5"
    T ~ Jnum(0.30000000000000004).print(using Jstyle.compact.fixed(2)) ==== "0.3"
    T ~ Jnum(1.2345678901234568E29).print(using Jstyle.compact.sig(4)) ==== "1.235e29"
    T ~ Jnum(7L).print(using Jstyle.compact.sig(2))                    ==== "7"   // Longs are already exact
    T ~ Jnum(86.421).print(using Jstyle.compact.limit(2, -3))          ==== "86.4"  // mag cutoff, sig floor
    T ~ Jnum(86.421).print(using Jstyle.compact.limit(2, 0))           ==== "90"
    T ~ Jnum(0.049).print(using Jstyle.compact.fixed(1))               ==== "0"     // swallowed by the tolerance
    T ~ Jnum(86.0).print(using Jstyle.compact.sig(4))                  ==== "86"    // no cosmetic .0 under a limit
    T ~ Jarr(Array(0.30000000000000004, 12345.6789)).print(using Jstyle.compact.limit(-2, 0)) ==== "[0.3,12345.68]"
    // verbatim beats style for untouched parsed tokens
    T ~ Json.parseFmt("[0.30000000000000004]").jsonOr(Jnull).print(using Jstyle.compact.sig(4)) ==== "[0.30000000000000004]"
    // ...but reprint restyles everything
    T ~ Json.parseFmt("[ 1 , 2 ]").jsonOr(Jnull).reprint(Jstyle.compact) ==== "[1,2]"
    T ~ Json.parseFmt("""{ "a" : 1 }""").jsonOr(Jnull).reprint(Jstyle.pretty) ==== "{\"a\": 1}"
    T ~ Json.parseFmt("""{ "a" : 1 }""").jsonOr(Jnull).reprint(Jstyle.pretty.fitTo(0)) ==== "{\n  \"a\": 1\n}"
    T ~ Json.parseFmt("[0.30000000000000004]").jsonOr(Jnull).reprint(Jstyle.compact.sig(4)) ==== "[0.3]"

  @Test
  def fitTest(): Unit =
    val u8 = java.nio.charset.StandardCharsets.UTF_8
    // collections that fit the page width stay on one line; those that don't break one child
    // per line, and children that fit stay inline
    val mat = Json.parse("[[1,2,3],[4,5,6],[7,8,9]]").jsonOr(Jnull)
    T ~ mat.print(using Jstyle.pretty)          ==== "[[1, 2, 3], [4, 5, 6], [7, 8, 9]]"
    T ~ mat.print(using Jstyle.pretty.fitTo(12)) ==== "[\n  [1, 2, 3],\n  [4, 5, 6],\n  [7, 8, 9]\n]"
    val doc = Json.parse("""{"name":"x","data":[1,2,3],"note":"hello"}""").jsonOr(Jnull)
    T ~ doc.print(using Jstyle.pretty)          ==== "{\"name\": \"x\", \"data\": [1, 2, 3], \"note\": \"hello\"}"
    T ~ doc.print(using Jstyle.pretty.fitTo(24)) ====
        "{\n  \"name\": \"x\",\n  \"data\": [1, 2, 3],\n  \"note\": \"hello\"\n}"
    // a value that cannot fit after its key breaks beneath it
    T ~ Json.parse("""{"k":[100000,200000,300000]}""").jsonOr(Jnull).print(using Jstyle.pretty.fitTo(16)) ====
        "{\n  \"k\": [\n    100000,\n    200000,\n    300000\n  ]\n}"
    // all-scalar arrays too long for one line wrap into columns, numbers right-aligned
    T ~ Jarr(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)).print(using Jstyle.pretty.fitTo(16)) ====
        "[\n   1,  2,  3,  4,\n   5,  6,  7,  8,\n   9, 10, 11, 12\n]"
    // ...strings left-aligned
    T ~ Jarr(Jstr("a"), Jstr("bb"), Jstr("ccc"), Jstr("d")).print(using Jstyle.pretty.fitTo(17)) ====
        "[\n  \"a\",   \"bb\",\n  \"ccc\", \"d\"\n]"
    // numeric policy applies inside reflow just like anywhere else
    T ~ Jarr(Array(0.30000000000000004, 0.5)).print(using Jstyle.pretty.sig(4)) ==== "[0.3, 0.5]"
    // preserved format still wins for print; reprint restyles through the reflow
    T ~ Json.parseFmt("[ 1 ,\n2 ]").jsonOr(Jnull).print(using Jstyle.pretty)   ==== "[ 1 ,\n2 ]"
    T ~ Json.parseFmt("[ 1 ,\n2 ]").jsonOr(Jnull).reprint(Jstyle.pretty)       ==== "[1, 2]"
    // a preserved multi-line guest inside a fresh tree passes through verbatim, never "fits"
    val guest = Json.parseFmt("[ 7 ,\n  8 ]").jsonOr(Jnull)
    T ~ Jarr(Jnum(1), guest, Jnum(2)).print(using Jstyle.pretty) ====
        "[\n  1,\n  [ 7 ,\n  8 ],\n  2\n]"
    // Str and Bytes targets produce identical text
    val big = Json.parse("""{"a":[1,2,3,4,5,6,7,8,9,10],"b":"x"}""").jsonOr(Jnull)
    T ~ (new String(big.printBytes(using Jstyle.pretty.fitTo(20)), u8)) ==== big.print(using Jstyle.pretty.fitTo(20))
    // the typed fast path routes through the tree when reflowing
    val jzm = summon[Jsonize[Array[Array[Int]]]]
    T ~ Json.print(Array(Array(1, 2, 3), Array(4, 5, 6)))(using jzm, Jstyle.pretty.fitTo(12)) ====
        "[\n  [1, 2, 3],\n  [4, 5, 6]\n]"

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
    T ~ Jarr(Array(3, 4)).print(using Jstyle.pretty) ==== "[3, 4]"
    T ~ Jarr(Array(3, 4)).print(using Jstyle.pretty.fitTo(0)) ==== "[\n  3,\n  4\n]"
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
  def directCodecTest(): Unit =
    val u8 = java.nio.charset.StandardCharsets.UTF_8
    // Json.print/printBytes serialize through jsonizeTo with no tree; output must match the
    // tree route byte-for-byte in every style
    def check[A](a: A)(using jz: Jsonize[A]): Unit =
      val tree = Json(a)
      T ~ Json.print(a)                                            ==== tree.print
      T ~ (new String(Json.printBytes(a), u8))                     ==== tree.print
      T ~ Json.print(a)(using jz, Jstyle.pretty)                   ==== tree.print(using Jstyle.pretty)
      T ~ (new String(Json.printBytes(a)(using jz, Jstyle.pretty), u8)) ==== tree.print(using Jstyle.pretty)
      // fitTo(0) keeps the direct emitter on the hook (fit-aware styles route through the tree)
      T ~ Json.print(a)(using jz, Jstyle.pretty.fitTo(0))          ==== tree.print(using Jstyle.pretty.fitTo(0))
      T ~ (new String(Json.printBytes(a)(using jz, Jstyle.pretty.fitTo(0)), u8)) ==== tree.print(using Jstyle.pretty.fitTo(0))
    check(Pt(1.5, 2.5))
    check(WithOpt(1, None))
    check(WithOpt(1, Some("x\n\"y é😀")))
    check(Auto(Inner(3), 7))
    check(Empty())
    check(List(1, 2, 3))
    check(Nil: List[Int])
    check(Vector("a", "é😀", ""))
    check(Array(1.5, -2.5e300, 0.0))
    check(Array(1.5, Double.NaN, Double.NegativeInfinity))   // packed-array format: quoted non-finite
    check(Array.empty[Double])
    check(Map("k" -> List(1.5, 2.5), "empty" -> Nil))
    check(Map.empty[String, Int])
    check(List(Pt(1.0, 2.0), Pt(-0.5, 3e-9)))
    check(Option(Pt(1.0, 2.0)))
    check(Circle(2.0): Shape)   // sums route through the tree; output still identical
    check(Dot: Shape)
    check(List[Shape](Circle(1.0), Sq(2.0, "s"), Dot))
    // and the round trip through the direct printer decodes back
    val v = Auto(Inner(3), 7)
    T ~ Json.parse(Json.print(v)).to[Auto] ==== Is(v)
    // decoding: field order does not matter, and the positional fast path keeps exact
    // last-wins duplicate-key semantics
    T ~ Json.parse("""{"y":2.5,"x":1.5}""").to[Pt]           ==== Is(Pt(1.5, 2.5))
    T ~ Json.parse("""{"x":1.0,"x":9.0,"y":2.5}""").to[Pt]   ==== Is(Pt(9.0, 2.5))
    T ~ bad(Json.parse("""{"x":1.0,"x":9.0}""").to[Pt])      ==== true   // dup hides a missing field
    T ~ Json.parse("""{"x":1.5,"y":2.5,"z":0}""").to[Pt]     ==== Is(Pt(1.5, 2.5))   // extras ignored
    T ~ Json.parse("{}").to[Empty]                           ==== Is(Empty())
    // packed-array decode: Jarr.D reads its backing directly for Double, per-element otherwise
    T ~ Json.parse("[1.5,2.5]").to[Vector[Double]]           ==== Is(Vector(1.5, 2.5))
    T ~ Json.parse("[1.5,2.5]").to[List[Float]]              ==== Is(List(1.5f, 2.5f))

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
