package kse.flow.test

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable

import sourcecode.{Line, given}

@RunWith(classOf[JUnit4])
class FlowTest {
  import kse.testutilities.TestUtilities.{_, given}
  import kse.flow.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue _
  )


  @Test
  def exceptionsTest(): Unit =
    val basic = new Exception("basic-exception")
    val caused = new Exception("caused-exception", basic)
    val stackless = new scala.util.control.ControlThrowable("stackless-exception") {}
    val circular = new Exception("circular-exception-1", stackless);
    val circulas = new Exception("circular-exception-2", stackless);
    circular.addSuppressed(circulas)
    circulas.addSuppressed(circular)

    for (thing, who) <- List(basic, caused, stackless, circular, circulas).zip(List("basic", "caused", "stackless", "circular", "circulas")) do
      val msg = s"while testing $who"
      msg \ thing.explainAsArray()           =**= ExceptionExplainer.explainAsArray(thing)
      msg \ thing.explainAsArray()           =**= thing.explainAsVector()
      msg \ thing.explain()                  ==== thing.explainAsArray().mkString("\n")
      msg \ thing.explainSuppressedAsArray() =**= ExceptionExplainer.explainAsArray(thing, showSuppressed = true)
      msg \ thing.explainSuppressedAsArray() =**= thing.explainSuppressedAsVector()
      msg \ thing.explainSuppressed()        ==== thing.explainSuppressedAsArray().mkString("\n")

    "" \ stackless.explainAsArray().length ==== 1
    "" \ caused.explainAsArray()               exists { x => x.contains("CAUSE") }
    "" \ circular.explainSuppressedAsArray()   exists { x => x.contains("circular-exception-2") }
    "" \ caused.explainAsArray()               exists { x => x startsWith "| " }
    "" \ caused.explainAsArray(childLines = 3) exists { x => x startsWith "| . . ." }

    val short = caused.explainAsArray(lines = 10)
    val full  = caused.explainAsArray()
    "" \ short.take(9) =**= full.take(9)
    val lines = full.drop(9).count(s => !s.startsWith("| "))
    "" \ short.last ==== s". . . (+$lines lines and 1 more exception)"


  @Test
  def repeatTest(): Unit =
    var bit = 1
    cFor(0)(_ < 10)(_ + 1){ _ => bit *= 2 }
    "int cFor" \ bit ==== (1 << 10)

    var sum = 0L
    cFor(Int.MaxValue + 10L)(_ >= Int.MaxValue - 10L)(_ - 1){ sum += _ }
    "long cFor" \ sum ==== 21L * Int.MaxValue

    val babble = new StringBuilder()
    cFor("hi")(_.length < 20)(s => s + " " + s){ s => if babble.nonEmpty then babble ++= ", "; babble ++= s }
    "generic cFor" \ babble.toString ==== "hi, hi hi, hi hi hi hi"

    var sumi = 0
    nFor(1000)(i => sumi += i*i + 1)
    "int nFor" \ sumi ==== 332834500

    var suml = 0L
    nFor(10000L)(l => suml += l*l + 1)
    "long nFor" \ suml ==== 333283345000L

    val ab = Array[Byte](1, 3, 5, 7, 9)
    var sumab = 0L
    aFor(ab)((b, i) => sumab += b*(i+1))
    "byte aFor" \ sumab ==== 95L

    val ac = Array[Char]('s', 'a', 'l', 'm', 'o', 'n')
    val sumac = new StringBuilder
    aFor(ac)((c, i) => sumac ++= c.toString*(i+1))
    "char aFor" \ sumac.result ==== "saalllmmmmooooonnnnnn"

    val s  = ac.mkString
    val sums = new StringBuilder
    aFor(s)((c, i) => sums ++= c.toString*(i+1))
    "string aFor" \ sums.result ==== "saalllmmmmooooonnnnnn"


    val as = Array[Short](1, 3, 5, 7, 9)
    var sumas = 0L
    aFor(as)((s, i) => sumas += s*(i+1))
    "short aFor" \ sumas ==== 95L

    val aj = Array[Int](1, 3, 5, 7 ,9)
    var sumaj = 0L
    aFor(aj)((j, i) => sumaj += j*(i+1))
    "int aFor" \ sumaj ==== 95L

    val al = Array[Long](1, 3, 5, 7, 9)
    var sumal = 0L
    aFor(al)((l, i) => sumal += l*(i+1))
    "long aFor" \ sumal ==== 95L

    val af = Array[Float](0.1f, 0.3f, 0.5f, 0.7f, 0.9f)
    var sumaf = 0f
    aFor(af)((f, i) => sumaf += f*(i+1))
    "float aFor" \ sumaf =~~= 9.5f

    val ad = Array[Double](0.01, 0.03, 0.05, 0.07, 0.09)
    var sumad = 0.0
    aFor(ad)((d, i) => sumad += d*(i+1))
    "double aFor" \ sumad =~~= 9.5

    val aa = Array[Option[String]](None, Some("cod"), Some("salmon"))
    val sumaa = new StringBuilder
    aFor(aa){ (o, i) => o match
      case Some(s) =>
        if sumaa.nonEmpty then sumaa ++= ", "
        sumaa ++= s*(i+1)
      case _ =>
    }
    "generic aFor" \ sumaa.result ==== "codcod, salmonsalmonsalmon"

    val xs = "cod" :: "bass" :: "perch" :: "salmon" :: Nil
    val fish = new StringBuilder
    iFor(xs.iterator){ (s, i) =>
      if fish.nonEmpty then fish ++= ", "
      fish ++= s*(i+1)
    }
    "iFor" \ fish.result ==== "cod, bassbass, perchperchperch, salmonsalmonsalmonsalmon"



  @Test
  def orTest(): Unit =
    "altnum" \ Alt(5) ==== Alt(5L)

    val aoc = Alt(Option("cod"))
    val asc = Alt(Some("cod"))
    "" \ aoc          ==== asc
    "" \ aoc.##       ==== asc.##
    "" \ aoc          =!!= Alt("cod")
    "" \ aoc.toString ==== "Alt(Some(cod))"

    "" \ aoc        ==== Alt.wrap(Some("cod"))
    "" \ aoc.unwrap ==== Some("cod")

    "" \ Alt.unit ==== Alt(())

    aoc.favor[Int] match
      case Alt(y) => "" \ y ==== Some("cod")
      case y      => assertTrue(s"Took disfavored branch with $y", false)

    (aoc: Any) match
      case Alt(y) => "" \ y ==== Some("cod")
      case y      => assertTrue(s"Took disfavored branch with $y", false)

    "isnum" \ Is(5) ==== Is(5L)

    "" \ Is("salmon") ==== Is("salmon")
    "" \ Is(aoc)      ==== Is(aoc: Any)

    val ioc = Is(Option("cod"))
    val isc = Is(Some("cod"))
    "" \ ioc              ==== isc
    "" \ ioc.##           ==== isc.##
    "" \ ioc              ==== Some("cod")
    "" \ ioc.toString     ==== "Some(cod)"
    "" \ Is(aoc).##       ==== Is(aoc: Any).##
    "" \ Is(aoc).toString ==== "Is(Alt(Some(cod)))"
    "" \ Is(aoc)          =!!= aoc
    "" \ Is(Is(aoc))      =!!= Is(aoc)

    "" \ ioc                ==== Is.wrap(Some("cod"))
    "" \ ioc.unwrap         ==== Some("cod")
    "" \ Is(aoc).unwrap     ==== aoc
    "" \ Is(Is(aoc)).unwrap ==== Is(aoc)
    "" \ Is.unit            ==== Is(())

    ioc.disfavor[Int] match
      case Is(x) => "" \ x ==== Some("cod")
      case x     => assertTrue(s"Took favored branch with $x", false)

    (ioc: Any) match
      case Is(x) => "" \ x ==== Some("cod")
      case x     => assertTrue(s"Took favored branch with $x", false)

    Is(aoc).disfavor[Int] match
      case Is(x) => "" \ x ==== aoc
      case x     => assertTrue(s"Took favored branch with $x", false)

    (Is(aoc): Any) match
      case Is(x) => "" \ x ==== aoc
      case x     => assertTrue(s"Took favored branch with $x", false)




  @Test
  def valueTest(): Unit =
    ()
    /*
    "" \ Ok[Int]()("salmon") ==== Yes("salmon")
    "" \ Ok[Int]()("salmon") =??= typed[Ok[Int, String]]
    "" \ Ok(7)[String]()     ==== No(7)
    "" \ Ok(7)[String]()     =??= typed[Ok[Int, String]]

    "" \ Yes(0) ==== Yes(0)
    "" \ Yes(0) =!!= No(0)
    "" \ No(0)  =!!= Yes(0)
    "" \ No(0)  ==== No(0)

    "" \ Yes(0).isYes ==== true
    "" \ Yes(0).isNo  ==== false
    "" \ No(0).isYes  ==== false
    "" \ No(0).isNo   ==== true

    "" \ Yes("salmon").yes ==== "salmon"
    "" \ Yes("salmon").no  ==== thrown[NoSuchElementException]
    "" \ No("herring").yes ==== thrown[NoSuchElementException]
    "" \ No("herring").no  ==== "herring"

    "" \ Yes("salmon").value ==== "salmon"
    "" \ No("herring").value ==== "herring"

    "" \ Yes("salmon").typeNo[Int]    .fold(_.toString, identity) ==== "salmon"
    "" \ No(47)       .typeYes[String].fold(identity, _.length)   ==== 47
    "" \ Yes("salmon").yesOr(_ => "minnow") ==== "salmon"
    "" \ No("herring").yesOr(_ => "minnow") ==== "minnow"
    "" \ Yes("salmon").noOr( _ => "minnow") ==== "minnow"
    "" \ No("herring").noOr( _ => "minnow") ==== "herring"
    */

  @Test
  def mapTest(): Unit =
    ()
    /*
    "" \ Ok[String]()("salmon").map(_.length)    ==== Yes(6)
    "" \ Ok("herring")[String]().map(_.length)   ==== No("herring")
    "" \ Ok[String]()("salmon").mapNo(_.length)  ==== Yes("salmon")
    "" \ Ok("herring")[String]().mapNo(_.length) ==== No(7)
    */
}
object FlowTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
