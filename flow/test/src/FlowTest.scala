package kse.flow.test

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable

import kse.flow._


object Test {
  case class Thrown(tag: ClassTag[_])(val classname: String) extends ControlThrowable(classname) {}
  def thrown[A](using tag: ClassTag[A]): Thrown = Thrown(tag)(tag.runtimeClass.getName)

  case class Typed[A](unit: Unit = ()) {}
  def typed[A]: Typed[A] = new Typed[A]()

  case class Labeled[A](message: String, value: () => A) {
    def ====[B](b: => B): Unit =
      val ta = Try{ value() }
      b match
        case t @ Thrown(tag) => ta match
          case Failure(x) =>
            if !tag.runtimeClass.isAssignableFrom(x.getClass) then
              assertEquals(message, ta, Failure(t))
          case _ => assertEquals(message, ta, Failure(t))
        case _ => assertEquals(message, ta.get, b)

    def =!!=[B](b: => B): Unit = Try{ value() } match
      case Success(v) => assertNotEquals(message, v, b)
      case Failure(x) => b match
        case t @ Thrown(tag) =>
          if tag.runtimeClass.isAssignableFrom(x.getClass) then
            assertTrue(s"$message\nDid not expect $x\nto be a ${t.classname}", false)
        case _ =>

    def =??=[B](t: Typed[B])(using B =:= A): Unit = {}
  }

  extension (message: String)
    def \[A](a: => A): Labeled[A] = Labeled(message, () => a)
}



@RunWith(classOf[JUnit4])
class OkTest {
  import Test._

  @Test
  def valueTest(): Unit =
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

  @Test
  def mapTest(): Unit =
    "" \ Ok[String]()("salmon").map(_.length)    ==== Yes(6)
    "" \ Ok("herring")[String]().map(_.length)   ==== No("herring")
    "" \ Ok[String]()("salmon").mapNo(_.length)  ==== Yes("salmon")
    "" \ Ok("herring")[String]().mapNo(_.length) ==== No(7)
}


@RunWith(classOf[JUnit4])
class FlowTest {
  import Test._

  @Test
  def canTest(): Unit = {
    "toOk" \ Yes("hi") ==== Option("hi").toOk
  }
}
object FlowTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
