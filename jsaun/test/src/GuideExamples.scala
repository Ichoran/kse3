// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.jsaun

// Every example in GUIDE-jsaun.md lives here between a `// guide: name` and `// guide: end` pair,
// so that the guide's code is compiled and run.  `check-guides.py` verifies the two stay identical.

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics.{given, *}
import kse.flow.{given, *}
import kse.jsaun.{given, *}

// guide: codec.kse3
case class Station(id: String, depth: Double, tags: List[String]) derives Jsonize, FromJson
case class Survey(name: String, stations: Vector[Station], note: Option[String]) derives Jsonize, FromJson
sealed trait Gear derives Jsonize, FromJson         // a sum prints its case's object with a "type" field added
case class Net(mesh: Double) extends Gear
case class Trap(count: Int) extends Gear
// guide: end

object GuideExamples {

  def firstStation(text: String): Ask[String] =
    // guide: parse.kse3
    Json.parse(text)("stations")(0)("id").str       // Ask[String]; the first failure wins, parse or access
    // guide: end

  def whereItBroke(text: String): String =
    // guide: parseerr.kse3
    Json.parse(text).ask.fold(_ => "parsed")(_.toString)   // "expected ',' or ']' in array, found 'x' (line 1, char 9)" and a caret
    // guide: end

  def summary(text: String): Ask[(String, Int, Double)] = Ask:
    // guide: access.kse3
    val j = Json.parse(text).json_?                 // the Json, or leave with the parse error
    val name = j("name").strOr("unnamed")           // the Or forms answer a default instead
    val n = j("stations").size                      // elements or keys; 0 for a scalar, -1 for an error
    val depth = j("stations")(0)("depth").dbl.?     // a Double, or leave with "expected a number, found string"
    // guide: end
    (name, n, depth)


  def build(): (String, String) =
    // guide: build.kse3
    val j = Jobj(
      "name" -> Jstr("eel survey"),
      "stations" -> Jarr(Jobj("id" -> Jstr("a1"), "depth" -> Jnum(12.5)), Jobj("id" -> Jstr("b2"), "depth" -> Jnum(3))),
      "done" -> Jbool(true),
      "note" -> Jnull
    )
    val compact = j.print                           // {"name":"eel survey",...}: the default style, and toString
    val pretty = j.print(using Jstyle.pretty)       // indented, with whatever fits on a 78-column line kept on one
    // guide: end
    (compact, pretty)

  def styles(xs: Array[Double]): (String, String, String) =
    // guide: styles.kse3
    val a = Jarr(xs)                                // a packed array of Doubles
    val exact = a.print                             // [0.30000000000000004,0.5,86.0]
    val tidy = a.print(using Jstyle.compact.sig(4)) // [0.3,0.5,86]: the shortest decimal within 4 significant figures
    val spaced = a.print(using Jstyle.pretty)       // [0.30000000000000004, 0.5, 86.0]: fits the line, so stays on it
    // guide: end
    (exact, tidy, spaced)


  def roundTrip(s: Survey): Ask[Survey] =
    // guide: codecuse.kse3
    val text = Json.print(s)                        // straight to text with no tree; Json(s) builds the tree
    Json.parse(text).to[Survey]                     // Ask[Survey]; every field that fails is reported, with its key
    // guide: end

  // guide: codecgiven.kse3
  given Jsonize[java.time.Instant] = t => Jstr(t.toString)                                   // ISO-8601 text
  given FromJson[java.time.Instant] = j => j.str.flatMap(s => nice{ java.time.Instant.parse(s) })   // a bad string is an Err
  // guide: end


  def bump(text: String): Ask[String] = Ask:
    // guide: edit.kse3
    val o = Json.M.parseFmt(text).json_? match      // a mutable tree that remembers where every token sat
      case o: Jobj.M => o
      case _ => Err ?# "expected an object at the top"
    o("count") = Jnum(o("count").longOr(0) + 1)     // a value edit: only this token changes on output
    o("checked") = Jbool(true)                      // absent, so appended, with separators copied from the object's own layout
    o.print                                         // everything untouched is byte for byte the input
    // guide: end


  def deepestStation(in: java.io.InputStream): Ask[Double] =
    // guide: stream.kse3
    var deepest = Double.NaN
    val v = new Jvisitor {
      override def key(k: String) = k == "stations" || k == "depth"   // false skips the value without decoding it
      override def num(d: Double) = if !(d <= deepest) then deepest = d
      override def num(l: Long) = num(l.toDouble)
    }
    Json.stream(in)(v).map(_ => deepest)            // one pass through the stream, no tree, numbers unboxed
    // guide: end

  // guide: builder.kse3
  class Depths { var sum = 0.0; var n = 0 }
  object MeanDepth extends Jbuilder[Depths, Double] {   // a stateless recipe; zero() makes the state for one walk
    def zero() = new Depths
    override def key(b: Depths, k: String) = k match
      case "stations" => Jexpect.Arr                // must be an array, which is then visited
      case "depth" => Jexpect.D                     // must be a number, delivered to num(b, d)
      case _ => Jexpect.Skip                        // stepped over, undecoded
    override def num(b: Depths, d: Double) = { b.sum += d; b.n += 1; Is.unit }
    def build(b: Depths): Ask[Double] = if b.n == 0 then Err.or("no stations") else Is(b.sum / b.n)
  }
  // guide: end

  def meanDepth(text: String): Ask[Double] =
    // guide: builduse.kse3
    Json.build(text)(MeanDepth)                     // a wrong form under "depth" fails with its position and key
    // guide: end


  def numbers(): (Ask[Long], Ask[Double], String, String, Ask[Double], Ask[Array[Double]]) =
    // guide: numbers.kse3
    val n = Json.parse("42").long                   // Ask[Long]: a whole number a Long can hold is a Jnum.L
    val d = Json.parse("42").dbl                    // Ask[Double]: every number reads as a Double
    val kept = Json.parse("0.30000000000000001", exact = true).jsonOr(Jnull).print   // exact keeps what a Double can't
    val lost = Json.parse("0.30000000000000001").jsonOr(Jnull).print                 // "0.3": the nearest Double, shortest
    val nan = Json.parse("NaN").dbl                 // the one extension to the standard: NaN and the infinities round-trip
    val xs = Json.parse("[1.5, 2.5, 3.5]").arr.flatMap(_.dbls)   // an Array[Double] straight out of a packed Jarr.D
    // guide: end
    (n, d, kept, lost, nan, xs)
}


@RunWith(classOf[JUnit4])
class GuideTest {
  import kse.basics.testutilities.TestUtilities.{_, given}
  import GuideExamples as G

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  val text = """{"name": "eel survey", "stations": [{"id": "a1", "depth": 12.5, "tags": ["deep"]}, {"id": "b2", "depth": 3, "tags": []}], "note": null}"""

  @Test
  def parsingTest(): Unit =
    T ~ G.firstStation(text) ==== "a1"
    T ~ G.firstStation("[1, 2, 3x]").isAlt ==== true
    T ~ G.whereItBroke("[1, 2, 3x]").contains("found 'x' (line 1, char 9)") ==== true
    T ~ G.whereItBroke(text) ==== "parsed"
    T ~ G.summary(text) ==== ("eel survey", 2, 12.5)
    T ~ G.summary("""{"stations": [{"depth": "x"}]}""").isAlt ==== true

  @Test
  def buildingTest(): Unit =
    val (compact, pretty) = G.build()
    T ~ compact ==== """{"name":"eel survey","stations":[{"id":"a1","depth":12.5},{"id":"b2","depth":3}],"done":true,"note":null}"""
    T ~ pretty.contains("\n") ==== true
    T ~ Json.parse(pretty).ask ==== Json.parse(compact).ask
    T ~ G.styles(Array(0.30000000000000004, 0.5, 86.0)) ==== ("[0.30000000000000004,0.5,86.0]", "[0.3,0.5,86]", "[0.30000000000000004, 0.5, 86.0]")

  @Test
  def codecTest(): Unit =
    val s = Survey("eel survey", Vector(Station("a1", 12.5, List("deep")), Station("b2", 3, Nil)), None)
    T ~ G.roundTrip(s) ==== s
    T ~ Json.print(s) ==== """{"name":"eel survey","stations":[{"id":"a1","depth":12.5,"tags":["deep"]},{"id":"b2","depth":3.0,"tags":[]}],"note":null}"""
    T ~ Json.parse(text).to[Survey] ==== s
    T ~ Json.parse("""{"name": 1}""").to[Survey].isAlt ==== true
    T ~ Json.print(Net(2.5): Gear) ==== """{"type":"Net","mesh":2.5}"""
    T ~ Json.parse("""{"type":"Trap","count":3}""").to[Gear] ==== Trap(3)
    import G.given
    T ~ Json.print(java.time.Instant.EPOCH) ==== "\"1970-01-01T00:00:00Z\""
    T ~ Json.parse("\"1970-01-01T00:00:00Z\"").to[java.time.Instant] ==== java.time.Instant.EPOCH
    T ~ Json.parse("\"eel\"").to[java.time.Instant].isAlt ==== true

  @Test
  def editingTest(): Unit =
    T ~ G.bump("{\n  \"name\": \"eel\",\n  \"count\": 4\n}") ==== "{\n  \"name\": \"eel\",\n  \"count\": 5,\n  \"checked\": true\n}"
    T ~ G.bump("[1]").isAlt ==== true

  @Test
  def streamingTest(): Unit =
    T ~ G.deepestStation(new java.io.ByteArrayInputStream(text.getBytes("UTF-8"))) ==== 12.5
    T ~ G.meanDepth(text) ==== 7.75
    T ~ G.meanDepth("""{"stations": []}""").isAlt ==== true
    T ~ G.meanDepth("""{"stations": [{"depth": "deep"}]}""").isAlt ==== true

  @Test
  def numbersTest(): Unit =
    val (n, d, kept, lost, nan, xs) = G.numbers()
    T ~ n ==== 42L
    T ~ d ==== 42.0
    T ~ kept ==== "0.30000000000000001"
    T ~ lost ==== "0.3"
    T ~ nan.map(_.isNaN) ==== true
    T ~ xs.map(_.toList) ==== List(1.5, 2.5, 3.5)
}
