package kse.flow.test

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable

@RunWith(classOf[JUnit4])
class FlowTest {
  import kse.testutilities.TestUtilities.{_, given}
  import kse.flow.{_, given}

  given Asserter(assertEquals _, assertNotEquals _, assertTrue _)

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
