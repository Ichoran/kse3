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

  def nlen(s: String) = if s eq null then -1 else s.length
  def nullone[N >: Null](n: N) = if n == null then 1 else -1


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
  def orCreationConversionTest: Unit =
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
    val iaoc = Is(aoc)
    val iiaoc = Is(iaoc)
    "" \ ioc           ==== isc
    "" \ ioc.##        ==== isc.##
    "" \ ioc           ==== Some("cod")
    "" \ ioc.toString  ==== "Some(cod)"
    "" \ iaoc.##       ==== Is(aoc: Any).##
    "" \ iaoc.toString ==== "Is(Alt(Some(cod)))"
    "" \ iaoc          =!!= aoc
    "" \ iiaoc         =!!= Is(aoc)

    "" \ ioc          ==== Is.wrap(Some("cod"))
    "" \ ioc.unwrap   ==== Some("cod")
    "" \ iaoc.unwrap  ==== aoc
    "" \ iiaoc.unwrap ==== iaoc
    "" \ Is.unit      ==== Is(())

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

    "" \ Or.from(Right[Int, String]("herring")) ==== Is("herring")
    "" \ Or.from(Left[Int, String](5))          ==== Alt(5)
    "" \ Or.swapFrom(Right[Int, String]("cod")) ==== Alt("cod")
    "" \ Or.swapFrom(Left[Int, String](9))      ==== Is(9)

    "" \ Or.from(Yes("herring").typeNo[Int]) ==== Is("herring")
    "" \ Or.from(No(5).typeYes[String])      ==== Alt(5)
    "" \ Or.swapFrom(Yes("cod").typeNo[Int]) ==== Alt("cod")
    "" \ Or.swapFrom(No(9).typeYes[String])  ==== Is(9)

    "" \ Or.from(Option("herring"))                  ==== Is("herring")
    "" \ Or.from(Some("herring"))                    ==== Is("herring")
    "" \ Or.from(None: Option[String])               ==== Alt.unit
    "" \ Or.swapFrom(Option(5))                      ==== Alt(5)
    "" \ Or.swapFrom(Some(9))                        ==== Alt(9)
    "" \ Or.swapFrom(None: Option[Int])              ==== Is.unit
    "" \ Or.fromOrElse(Option("herring"), 2)         ==== Is("herring")
    "" \ Or.fromOrElse(Some("herring"), 2)           ==== Is("herring")
    "" \ Or.fromOrElse(None: Option[String], 2)      ==== Alt(2)
    "" \ Or.swapFromOrElse(Option(5), "cod")         ==== Alt(5)
    "" \ Or.swapFromOrElse(Some(9), "cod")           ==== Alt(9)
    "" \ Or.swapFromOrElse(None: Option[Int], "cod") ==== Is("cod")

    val e = new Exception("test-exception")
    val trys = Try{ "herring" }
    val trye = Try{ throw e; 5 }
    "" \ Or.from(trys)                   ==== Is("herring")
    "" \ Or.from(trye)                   ==== Alt(e)
    "" \ Or.from(trye, _.getMessage)     ==== Alt("test-exception")
    "" \ Or.swapFrom(trys)               ==== Alt("herring")
    "" \ Or.swapFrom(trye)               ==== Is(e)
    "" \ Or.swapFrom(trye, _.getMessage) ==== Is("test-exception")

    val dis = ioc.disfavor[Int]
    val fav = aoc.favor[String]
    "" \ dis                                         ==== ioc
    "" \ fav                                         ==== aoc
    "" \ Option("cod").isOr[Int]                     ==== dis   // Have to use `isOr` because of `or` on `Option`
    "" \ Option("cod").isnt[String]                  ==== fav
    "" \ Option("cod").isnt[String].or[Int]          ==== iaoc
    "" \ Option("cod").isnt[String].or[Int].or[Char] ==== iiaoc

    "" \ 5.isIf(_ > 0)                                      =&&= 5            -> typed[Int Or Int]
    "" \ 5.isIf(_ < 0)                                      =&&= Alt(5)       -> typed[Int Or Int]
    "" \ 5.altIf(_ > 0)                                     =&&= Alt(5)       -> typed[Int Or Int]
    "" \ 5.altIf(_ < 0)                                     =&&= 5            -> typed[Int Or Int]
    "" \ 5.isCase{ case x if x > 0 => "!"*x }               =&&= "!!!!!"      -> typed[String Or Int]
    "" \ 5.isCase{ case x if x < 0 => x.toChar }            =&&= Alt(5)       -> typed[Char Or Int]
    "" \ 5.altCase{ case x if x > 0 => "!"*x }              =&&= Alt("!!!!!") -> typed[Int Or String]
    "" \ 5.altCase{ case x if x < 0 => x.toChar }           =&&= 5            -> typed[Int Or Char]
    "" \ 5.isCaseOrAlt{ case x if x > 0 => "!"*x }{_ < -3}  =&&= "!!!!!"      -> typed[String Or Boolean]
    "" \ 5.isCaseOrAlt{ case x if x < 0 => x.toChar}{_ > 3} =&&= Alt(true)    -> typed[Char Or Boolean]
    "" \ 5.altCaseOrIs{ case x if x > 0 => "!"*x }{_ < -3}  =&&= Alt("!!!!!") -> typed[Boolean Or String]
    "" \ 5.altCaseOrIs{ case x if x < 0 => x.toChar}{_ > 3} =&&= true         -> typed[Boolean Or Char]
    "" \ 5.unfoldToOr(_ > 0)("!" * _)(_ < -3)               =&&= "!!!!!"      -> typed[String Or Boolean]
    "" \ 5.unfoldToOr(_ < 0)(_.toChar)(_ > 3)               =&&= Alt(true)    -> typed[Char Or Boolean]
    "" \ 5.isLike(true.isnt[Int])                           =&&= 5            -> typed[Int Or Boolean]
    "" \ 5.altLike('e'.or[Int])                             =&&= Alt(5)       -> typed[Char Or Int]
    "" \ 5.altAlso(true.or[Char])                           =&&= Alt(5)       -> typed[Boolean Or (Int Or Char)]

    // Extra, probably superfluous tests
    "" \ List(5, -5).map(_.isIf( _ > 0)) ==== List(5, Alt(-5))
    "" \ List(5, -5).map(_.altIf(_ > 0)) ==== List(Alt(5), -5)
    "" \ List(5, -5).map(_.isCase{  case 5 => "five" }) ==== List("five", Alt(-5))
    "" \ List(5, -5).map(_.altCase{ case 5 => "five" }) ==== List(Alt("five"), -5)

    "" \ List("cod", null).map(_.isIf( _ ne null)) ==== List(Is("cod"), Alt(null))
    "" \ List("cod", null).map(_.altIf(_ ne null)) ==== List(Alt("cod"), null)
    "" \ List("cod", null).map(_.isCase{  case "cod" => true}) ==== List(true, Alt(null))
    "" \ List("cod", null).map(_.altCase{ case "cod" => true}) ==== List(Alt(true), null)


  class ProvideVariousOrValues() {
    val i = Is(5)
    val a = Alt("cod")
    val oi  = 5.or[String]     // *.
    val oa  = "cod".isnt[Int]  // .*
    val oii = oi.or[Char]      // (*.).
    val oia = oa.or[Char]      // (.*).
    val oai = oi.isnt[Char]    // .(*.)
    val oaa = oa.isnt[Char]    // .(.*)
    val oiii = oii.or[Long]    // ((*.).).
    val oiia = oia.or[Long]    // ((.*).).
    val oiai = oai.or[Long]    // (.(*.)).
    val oiaa = oaa.or[Long]    // (.(.*)).
    val oaii = oii.isnt[Long]  // .((*.).)
    val oaia = oia.isnt[Long]  // .((.*).)
    val oaai = oai.isnt[Long]  // .(.(*.))
    val oaaa = oaa.isnt[Long]  // .(.(.*))
    val n = Is(null)
    val m = Alt(null)
    val on = null.isOr[Int]
    val om = null.isnt[Int]
    val oin = on.or[Char]
    val oim = om.or[Char]
    val oan = on.isnt[Char]
    val oam = om.isnt[Char]
    val p = Is(null: String)
    val q = Alt(null: String)
    val op = (null: String).isOr[Int]
    val oq = (null: String).isnt[Int]
    val oip = op.or[Char]
    val oiq = oq.or[Char]
    val oap = op.isnt[Char]
    val oaq = oq.isnt[Char]
  }


  @Test
  def orAccessTest(): Unit =
    val valueProvider = new ProvideVariousOrValues()
    import valueProvider._

    // Tests both identity and expected level of boxing
    "" \ i    ==== 5
    "" \ a    ==== Alt("cod")
    "" \ oi   =&&= 5                    -> typed[Int Or String]
    "" \ oa   =&&= Alt("cod")           -> typed[Int Or String]
    "" \ oii  =&&= 5                    -> typed[(Int Or String) Or Char]
    "" \ oia  =&&= Is(Alt("cod"))       -> typed[(Int Or String) Or Char]
    "" \ oai  =&&= Alt(5)               -> typed[Char Or (Int Or String)]
    "" \ oaa  =&&= Alt(Alt("cod"))      -> typed[Char Or (Int Or String)]
    "" \ oiii =&&= 5                    -> typed[((Int Or String) Or Char) Or Long]
    "" \ oiia =&&= Is(Is(Alt("cod")))   -> typed[((Int Or String) Or Char) Or Long]
    "" \ oiai =&&= Is(Alt(5))           -> typed[(Char Or (Int Or String)) Or Long]
    "" \ oiaa =&&= Is(Alt(Alt("cod")))  -> typed[(Char Or (Int Or String)) Or Long]
    "" \ oaii =&&= Alt(5)               -> typed[Long Or ((Int Or String) Or Char)]
    "" \ oaia =&&= Alt(Is(Alt("cod")))  -> typed[Long Or ((Int Or String) Or Char)]
    "" \ oaai =&&= Alt(Alt(5))          -> typed[Long Or (Char Or (Int Or String))]
    "" \ oaaa =&&= Alt(Alt(Alt("cod"))) -> typed[Long Or (Char Or (Int Or String))]
    "" \ n    ==== null
    "" \ m    ==== Alt(null)
    "" \ on   =&&= null               :->: typed[Null Or Int]
    "" \ om   =&&= Alt(null)            -> typed[Int Or Null]
    "" \ oin  =&&= null               :->: typed[(Null Or Int) Or Char]
    "" \ oim  =&&= Is(Alt(null))        -> typed[(Int Or Null) Or Char]
    "" \ oan  =&&= Alt(null)            -> typed[Char Or (Null Or Int)]
    "" \ oam  =&&= Alt(Alt(null))       -> typed[Char Or (Int Or Null)]
    "" \ p    ==== null
    "" \ q    ==== Alt(null)
    "" \ op   =&&= null               :->: typed[String Or Int]
    "" \ oq   =&&= Alt(null)            -> typed[Int Or String]
    "" \ oip  =&&= null               :->: typed[(String Or Int) Or Char]
    "" \ oiq  =&&= Is(Alt(null))        -> typed[(Int Or String) Or Char]
    "" \ oap  =&&= Alt(null)            -> typed[Char Or (String Or Int)]
    "" \ oaq  =&&= Alt(Alt(null))       -> typed[Char Or (Int Or String)]

    "" \ i.get   ==== 5
    "" \ a.get   ==== thrown[NoSuchElementException]
    "" \ i.alt   ==== thrown[NoSuchElementException]
    "" \ a.alt   ==== "cod"
    "" \ oi.get  ==== 5
    "" \ oi.alt  ==== thrown[NoSuchElementException]
    "" \ oia.get ==== Alt("cod")
    "" \ oia.alt ==== thrown[NoSuchElementException]
    "" \ oa.get  ==== thrown[NoSuchElementException]
    "" \ oa.alt  ==== "cod"
    "" \ oai.get ==== thrown[NoSuchElementException]
    "" \ oai.alt ==== Is(5)
    "" \ n.get   ==== null
    "" \ n.alt   ==== thrown[NoSuchElementException] 
    "" \ m.get   ==== thrown[NoSuchElementException]
    "" \ m.alt   ==== null    
    "" \ on.get  ==== null
    "" \ on.alt  ==== thrown[NoSuchElementException]    
    "" \ om.get  ==== thrown[NoSuchElementException]
    "" \ om.alt  ==== null    
    "" \ oin.get ==== null
    "" \ oin.alt ==== thrown[NoSuchElementException]    
    "" \ oim.get ==== Alt(null)
    "" \ oim.alt ==== thrown[NoSuchElementException]   
    "" \ oan.get ==== thrown[NoSuchElementException]
    "" \ oan.alt ==== null    
    "" \ oam.get ==== thrown[NoSuchElementException]
    "" \ oam.alt ==== Alt(null)   
    "" \ p.get   ==== null
    "" \ p.alt   ==== thrown[NoSuchElementException] 
    "" \ q.get   ==== thrown[NoSuchElementException]
    "" \ q.alt   ==== null    
    "" \ op.get  ==== null
    "" \ op.alt  ==== thrown[NoSuchElementException]    
    "" \ oq.get  ==== thrown[NoSuchElementException]
    "" \ oq.alt  ==== null    
    "" \ oip.get ==== null
    "" \ oip.alt ==== thrown[NoSuchElementException]    
    "" \ oiq.get ==== Alt(null)
    "" \ oiq.alt ==== thrown[NoSuchElementException]   
    "" \ oap.get ==== thrown[NoSuchElementException]
    "" \ oap.alt ==== null    
    "" \ oaq.get ==== thrown[NoSuchElementException]
    "" \ oaq.alt ==== Alt(null)   

    "" \ i.value      ==== 5
    "" \ a.value      ==== "cod"
    "" \ oi.value     ==== 5
    "" \ oa.value     ==== "cod"
    "" \ oii.value    ==== Is(5)
    "" \ oia.value    ==== Alt("cod")
    "" \ oai.value    ==== Is(5)
    "" \ oaa.value    ==== Alt("cod")
    "" \ oa.value     =??= typed[Int | String]
    "" \ oi.value     =??= typed[Int | String]
    "" \ n.value      ==== null
    "" \ m.value      ==== null
    "" \ on.value     ==== null
    "" \ om.value     ==== null
    "" \ oin.value    ==== null
    "" \ oim.value    ==== Alt(null)
    "" \ oan.value    ==== null
    "" \ oam.value    ==== Alt(null)
    "" \ p.value      ==== null
    "" \ q.value      ==== null
    "" \ op.value     ==== null
    "" \ oq.value     ==== null
    "" \ oip.value    ==== null
    "" \ oiq.value    ==== Alt(null)
    "" \ oap.value    ==== null
    "" \ oaq.value    ==== Alt(null)

    "" \ i.isBoxed      ==== false
    "" \ a.isBoxed      ==== true
    "" \ oi.isBoxed     ==== false
    "" \ oa.isBoxed     ==== true
    "" \ oii.isBoxed    ==== false
    "" \ oia.isBoxed    ==== true
    "" \ oai.isBoxed    ==== true
    "" \ oaa.isBoxed    ==== true
    "" \ Is(a).isBoxed  ==== true
    "" \ Alt(a).isBoxed ==== true
    "" \ n.isBoxed      ==== false
    "" \ m.isBoxed      ==== true
    "" \ on.isBoxed     ==== false
    "" \ om.isBoxed     ==== true
    "" \ oin.isBoxed    ==== false
    "" \ oim.isBoxed    ==== true
    "" \ oan.isBoxed    ==== true
    "" \ oam.isBoxed    ==== true
    "" \ p.isBoxed      ==== false
    "" \ q.isBoxed      ==== true
    "" \ op.isBoxed     ==== false
    "" \ oq.isBoxed     ==== true
    "" \ oip.isBoxed    ==== false
    "" \ oiq.isBoxed    ==== true
    "" \ oap.isBoxed    ==== true
    "" \ oaq.isBoxed    ==== true

    "" \ { var x = 0; i.foreach(x = _)                 ; x } ==== 5
    "" \ { var x = 0; a.foreach(_ => x = 1)            ; x } ==== 0
    "" \ { var x = 0; oi.foreach(x = _)                ; x } ==== 5
    "" \ { var x = 0; oa.foreach(x = _)                ; x } ==== 0
    "" \ { var x = 0; oii.foreach(_.foreach(x = _))    ; x } ==== 5
    "" \ { var x = 0; oia.foreach(_.foreach(x = _))    ; x } ==== 0
    "" \ { var x = 0; oai.foreach(c => x = c.toInt)    ; x } ==== 0
    "" \ { var x = 0; oaa.foreach(c => x = c.toInt)    ; x } ==== 0
    "" \ { var x = 0; n.foreach(y => x = nullone(y))   ; x } ==== 1
    "" \ { var x = 0; m.foreach(_ => x = 1)            ; x } ==== 0
    "" \ { var x = 0; on.foreach(y => x = nullone(y))  ; x } ==== 1
    "" \ { var x = 0; om.foreach(x = _)                ; x } ==== 0
    "" \ { var x = 0; oin.foreach(y => x = nullone(y)) ; x } ==== 1
    "" \ { var x = 0; oim.foreach(y => x = nullone(y)) ; x } ==== -1
    "" \ { var x = 0; oan.foreach(c => x = c.toInt)    ; x } ==== 0
    "" \ { var x = 0; oam.foreach(c => x = c.toInt)    ; x } ==== 0
    "" \ { var x = 0; p.foreach(y => x = nlen(y))      ; x } ==== -1
    "" \ { var x = 0; q.foreach(_ => x = 1)            ; x } ==== 0
    "" \ { var x = 0; op.foreach(y => x = nlen(y))     ; x } ==== -1
    "" \ { var x = 0; oq.foreach(x = _)                ; x } ==== 0
    "" \ { var x = 0; oip.foreach(y => x = nlen(y.get)); x } ==== -1
    "" \ { var x = 0; oiq.foreach(y => x = nlen(y.alt)); x } ==== -1
    "" \ { var x = 0; oap.foreach(c => x = c.toInt)    ; x } ==== 0
    "" \ { var x = 0; oaq.foreach(c => x = c.toInt)    ; x } ==== 0

    "" \ { var x = 0; i.foreachAlt(_ => x = 1);             ; x } ==== 0
    "" \ { var x = 0; a.foreachAlt(s => x = s.length)       ; x } ==== 3
    "" \ { var x = 0; oi.foreachAlt(s => x = s.length)      ; x } ==== 0
    "" \ { var x = 0; oa.foreachAlt(s => x = s.length)      ; x } ==== 3
    "" \ { var x = 0; oii.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    "" \ { var x = 0; oia.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    "" \ { var x = 0; oai.foreachAlt(y => x = y.get)        ; x } ==== 5
    "" \ { var x = 0; oaa.foreachAlt(y => x = y.alt.length) ; x } ==== 3
    "" \ { var x = 0; n.foreachAlt(_ => x = 1)              ; x } ==== 0
    "" \ { var x = 0; m.foreachAlt(y => x = nullone(y))     ; x } ==== 1
    "" \ { var x = 0; on.foreachAlt(x = _)                  ; x } ==== 0
    "" \ { var x = 0; om.foreachAlt(y => x = nullone(y))    ; x } ==== 1
    "" \ { var x = 0; oin.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    "" \ { var x = 0; oim.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    "" \ { var x = 0; oan.foreachAlt(y => x = nullone(y))   ; x } ==== 1
    "" \ { var x = 0; oam.foreachAlt(y => x = nullone(y))   ; x } ==== -1
    "" \ { var x = 0; p.foreachAlt(_ => x = 1)              ; x } ==== 0
    "" \ { var x = 0; q.foreachAlt(y => x = nlen(y))        ; x } ==== -1
    "" \ { var x = 0; op.foreachAlt(x = _)                  ; x } ==== 0
    "" \ { var x = 0; oq.foreachAlt(y => x = nlen(y))       ; x } ==== -1
    "" \ { var x = 0; oip.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    "" \ { var x = 0; oiq.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    "" \ { var x = 0; oap.foreachAlt(y => x = nlen(y.get))  ; x } ==== -1
    "" \ { var x = 0; oaq.foreachAlt(y => x = nlen(y.alt))  ; x } ==== -1

    "" \ { var x = 0; i.foreachThem(x = _)(_ => x = 4)                                  ; x } ==== 5
    "" \ { var x = 0; a.foreachThem(_ => x = 4)(s => x = s.length)                      ; x } ==== 3
    "" \ { var x = 0; oi.foreachThem(x = _)(s => x = s.length)                          ; x } ==== 5
    "" \ { var x = 0; oa.foreachThem(x = _)(s => x = s.length)                          ; x } ==== 3
    "" \ { var x = 0; oii.foreachThem(y => x = y.fold(_+1)(_.length))(c => x = c.toInt) ; x } ==== 6
    "" \ { var x = 0; oia.foreachThem(y => x = y.fold(_+1)(_.length))(c => x = c.toInt) ; x } ==== 3
    "" \ { var x = 0; oai.foreachThem(c => x = c.toInt)(y => x = y.fold(_+1)(_.length)) ; x } ==== 6
    "" \ { var x = 0; oaa.foreachThem(c => x = c.toInt)(y => x = y.fold(_+1)(_.length)) ; x } ==== 3
    "" \ { var x = 0; n.foreachThem(y => x = nullone(y))(_ => x = 4)                    ; x } ==== 1
    "" \ { var x = 0; m.foreachThem(_ => x = 4)(y => x = nullone(y))                    ; x } ==== 1
    "" \ { var x = 0; on.foreachThem(y => x = nullone(y))(x = _)                        ; x } ==== 1
    "" \ { var x = 0; om.foreachThem(x = _)(y => x = nullone(y))                        ; x } ==== 1
    "" \ { var x = 0; oin.foreachThem(y => x = nullone(y))(c => x = c.toInt)            ; x } ==== 1
    "" \ { var x = 0; oim.foreachThem(y => x = nullone(y))(c => x = c.toInt)            ; x } ==== -1
    "" \ { var x = 0; oan.foreachThem(c => x = c.toInt)(y => x = nullone(y))            ; x } ==== 1
    "" \ { var x = 0; oam.foreachThem(c => x = c.toInt)(y => x = nullone(y))            ; x } ==== -1
    "" \ { var x = 0; p.foreachThem(y => x = nlen(y))(_ => x = 4)                       ; x } ==== -1
    "" \ { var x = 0; q.foreachThem(_ => x = 4)(y => x = nlen(y))                       ; x } ==== -1
    "" \ { var x = 0; op.foreachThem(y => x = nlen(y))(x = _)                           ; x } ==== -1
    "" \ { var x = 0; oq.foreachThem(x = _)(y => x = nlen(y))                           ; x } ==== -1
    "" \ { var x = 0; oip.foreachThem(y => x = nlen(y.get))(c => x = c.toInt)           ; x } ==== -1
    "" \ { var x = 0; oiq.foreachThem(y => x = nlen(y.alt))(c => x = c.toInt)           ; x } ==== -1
    "" \ { var x = 0; oap.foreachThem(c => x = c.toInt)(y => x = nlen(y.get))           ; x } ==== -1
    "" \ { var x = 0; oaq.foreachThem(c => x = c.toInt)(y => x = nlen(y.alt))           ; x } ==== -1

    "" \ { var x = 0; i.use(x = _)                  :==: typedLike(i)  ; x } ==== 5
    "" \ { var x = 0; a.use(_ => x = 1)             :==: typedLike(a)  ; x } ==== 0
    "" \ { var x = 0; oi.use(x = _)                 :==: typedLike(oi) ; x } ==== 5
    "" \ { var x = 0; oa.use(x = _)                 :==: typedLike(oa) ; x } ==== 0
    "" \ { var x = 0; oii.use(_.use(x = _))         :==: typedLike(oii); x } ==== 5
    "" \ { var x = 0; oia.use(_.use(x = _))         :==: typedLike(oia); x } ==== 0
    "" \ { var x = 0; oai.use(c => x = c.toInt)     :==: typedLike(oai); x } ==== 0
    "" \ { var x = 0; oaa.use(c => x = c.toInt)     :==: typedLike(oaa); x } ==== 0
    "" \ { var x = 0; n.use(y => x = nullone(y))    :==: typedLike(n)  ; x } ==== 1
    "" \ { var x = 0; m.use(_ => x = 1)             :==: typedLike(m)  ; x } ==== 0
    "" \ { var x = 0; on.use(y => x = nullone(y))   :==: typedLike(on) ; x } ==== 1
    "" \ { var x = 0; om.use(x = _)                 :==: typedLike(om) ; x } ==== 0
    "" \ { var x = 0; oin.use(y => x = nullone(y))  :==: typedLike(oin); x } ==== 1
    "" \ { var x = 0; oim.use(y => x = nullone(y))  :==: typedLike(oim); x } ==== -1
    "" \ { var x = 0; oan.use(c => x = c.toInt)     :==: typedLike(oan); x } ==== 0
    "" \ { var x = 0; oam.use(c => x = c.toInt)     :==: typedLike(oam); x } ==== 0
    "" \ { var x = 0; p.use(y => x = nlen(y))       :==: typedLike(p)  ; x } ==== -1
    "" \ { var x = 0; q.use(_ => x = 1)             :==: typedLike(q)  ; x } ==== 0
    "" \ { var x = 0; op.use(y => x = nlen(y))      :==: typedLike(op) ; x } ==== -1
    "" \ { var x = 0; oq.use(x = _)                 :==: typedLike(oq) ; x } ==== 0
    "" \ { var x = 0; oip.use(y => x = nlen(y.get)) :==: typedLike(oip); x } ==== -1
    "" \ { var x = 0; oiq.use(y => x = nlen(y.alt)) :==: typedLike(oiq); x } ==== -1
    "" \ { var x = 0; oap.use(c => x = c.toInt)     :==: typedLike(oap); x } ==== 0
    "" \ { var x = 0; oaq.use(c => x = c.toInt)     :==: typedLike(oaq); x } ==== 0

    "" \ { var x = 0; i.useAlt(_ => x = 1)              :==: typedLike(i)  ; x } ==== 0
    "" \ { var x = 0; a.useAlt(s => x = s.length)       :==: typedLike(a)  ; x } ==== 3
    "" \ { var x = 0; oi.useAlt(s => x = s.length)      :==: typedLike(oi) ; x } ==== 0
    "" \ { var x = 0; oa.useAlt(s => x = s.length)      :==: typedLike(oa) ; x } ==== 3
    "" \ { var x = 0; oii.useAlt(c => x = c.toInt)      :==: typedLike(oii); x } ==== 0
    "" \ { var x = 0; oia.useAlt(c => x = c.toInt)      :==: typedLike(oia); x } ==== 0
    "" \ { var x = 0; oai.useAlt(y => x = y.get)        :==: typedLike(oai); x } ==== 5
    "" \ { var x = 0; oaa.useAlt(y => x = y.alt.length) :==: typedLike(oaa); x } ==== 3
    "" \ { var x = 0; n.useAlt(_ => x = 1)              :==: typedLike(n)  ; x } ==== 0
    "" \ { var x = 0; m.useAlt(y => x = nullone(y))     :==: typedLike(m)  ; x } ==== 1
    "" \ { var x = 0; on.useAlt(x = _)                  :==: typedLike(on) ; x } ==== 0
    "" \ { var x = 0; om.useAlt(y => x = nullone(y))    :==: typedLike(om) ; x } ==== 1
    "" \ { var x = 0; oin.useAlt(c => x = c.toInt)      :==: typedLike(oin); x } ==== 0
    "" \ { var x = 0; oim.useAlt(c => x = c.toInt)      :==: typedLike(oim); x } ==== 0
    "" \ { var x = 0; oan.useAlt(y => x = nullone(y))   :==: typedLike(oan); x } ==== 1
    "" \ { var x = 0; oam.useAlt(y => x = nullone(y))   :==: typedLike(oam); x } ==== -1
    "" \ { var x = 0; p.useAlt(_ => x = 1)              :==: typedLike(p)  ; x } ==== 0
    "" \ { var x = 0; q.useAlt(y => x = nlen(y))        :==: typedLike(q)  ; x } ==== -1
    "" \ { var x = 0; op.useAlt(x = _)                  :==: typedLike(op) ; x } ==== 0
    "" \ { var x = 0; oq.useAlt(y => x = nlen(y))       :==: typedLike(oq) ; x } ==== -1
    "" \ { var x = 0; oip.useAlt(c => x = c.toInt)      :==: typedLike(oip); x } ==== 0
    "" \ { var x = 0; oiq.useAlt(c => x = c.toInt)      :==: typedLike(oiq); x } ==== 0
    "" \ { var x = 0; oap.useAlt(y => x = nlen(y.get))  :==: typedLike(oap); x } ==== -1
    "" \ { var x = 0; oaq.useAlt(y => x = nlen(y.alt))  :==: typedLike(oaq); x } ==== -1

    "" \ { var x = 0; i.useThem(x = _)(_ => x = 4)                                  :==: typedLike(i)  ; x } ==== 5
    "" \ { var x = 0; a.useThem(_ => x = 4)(s => x = s.length)                      :==: typedLike(a)  ; x } ==== 3
    "" \ { var x = 0; oi.useThem(x = _)(s => x = s.length)                          :==: typedLike(oi) ; x } ==== 5
    "" \ { var x = 0; oa.useThem(x = _)(s => x = s.length)                          :==: typedLike(oa) ; x } ==== 3
    "" \ { var x = 0; oii.useThem(y => x = y.fold(_+1)(_.length))(c => x = c.toInt) :==: typedLike(oii); x } ==== 6
    "" \ { var x = 0; oia.useThem(y => x = y.fold(_+1)(_.length))(c => x = c.toInt) :==: typedLike(oia); x } ==== 3
    "" \ { var x = 0; oai.useThem(c => x = c.toInt)(y => x = y.fold(_+1)(_.length)) :==: typedLike(oai); x } ==== 6
    "" \ { var x = 0; oaa.useThem(c => x = c.toInt)(y => x = y.fold(_+1)(_.length)) :==: typedLike(oaa); x } ==== 3
    "" \ { var x = 0; n.useThem(y => x = nullone(y))(_ => x = 4)                    :==: typedLike(n)  ; x } ==== 1
    "" \ { var x = 0; m.useThem(_ => x = 4)(y => x = nullone(y))                    :==: typedLike(m)  ; x } ==== 1
    "" \ { var x = 0; on.useThem(y => x = nullone(y))(x = _)                        :==: typedLike(on) ; x } ==== 1
    "" \ { var x = 0; om.useThem(x = _)(y => x = nullone(y))                        :==: typedLike(om) ; x } ==== 1
    "" \ { var x = 0; oin.useThem(y => x = nullone(y))(c => x = c.toInt)            :==: typedLike(oin); x } ==== 1
    "" \ { var x = 0; oim.useThem(y => x = nullone(y))(c => x = c.toInt)            :==: typedLike(oim); x } ==== -1
    "" \ { var x = 0; oan.useThem(c => x = c.toInt)(y => x = nullone(y))            :==: typedLike(oan); x } ==== 1
    "" \ { var x = 0; oam.useThem(c => x = c.toInt)(y => x = nullone(y))            :==: typedLike(oam); x } ==== -1
    "" \ { var x = 0; p.useThem(y => x = nlen(y))(_ => x = 4)                       :==: typedLike(p)  ; x } ==== -1
    "" \ { var x = 0; q.useThem(_ => x = 4)(y => x = nlen(y))                       :==: typedLike(q)  ; x } ==== -1
    "" \ { var x = 0; op.useThem(y => x = nlen(y))(x = _)                           :==: typedLike(op) ; x } ==== -1
    "" \ { var x = 0; oq.useThem(x = _)(y => x = nlen(y))                           :==: typedLike(oq) ; x } ==== -1
    "" \ { var x = 0; oip.useThem(y => x = nlen(y.get))(c => x = c.toInt)           :==: typedLike(oip); x } ==== -1
    "" \ { var x = 0; oiq.useThem(y => x = nlen(y.alt))(c => x = c.toInt)           :==: typedLike(oiq); x } ==== -1
    "" \ { var x = 0; oap.useThem(c => x = c.toInt)(y => x = nlen(y.get))           :==: typedLike(oap); x } ==== -1
    "" \ { var x = 0; oaq.useThem(c => x = c.toInt)(y => x = nlen(y.alt))           :==: typedLike(oaq); x } ==== -1

    "" \ i.exists(_ == 5)     ==== true
    "" \ i.exists(_ == 4)     ==== false
    "" \ a.exists(_ == 5)     ==== false
    "" \ oi.exists(_ == 5)    ==== true
    "" \ oi.exists(_ == 4)    ==== false
    "" \ oa.exists(_ == 5)    ==== false
    "" \ on.exists(_ eq null) ==== true
    "" \ on.exists(_ ne null) ==== false
    "" \ om.exists(_ == 5)    ==== false

    "" \ i.existsAlt(_ == "eel")   ==== false
    "" \ a.existsAlt(_ == "cod")   ==== true
    "" \ a.existsAlt(_ == "eel")   ==== false
    "" \ oi.existsAlt(_ == "eel")  ==== false
    "" \ oa.existsAlt(_ == "cod")  ==== true
    "" \ oa.existsAlt(_ == "eel")  ==== false
    "" \ on.existsAlt(_ == 5)      ==== false
    "" \ om.existsAlt(_ eq null)   ==== true
    "" \ om.existsAlt(_ ne null)   ==== false

    "" \ i.existsThem(_ == 5)(_ => false)     ==== true
    "" \ i.existsThem(_ == 4)(_ => true)      ==== false
    "" \ a.existsThem(_ => false)(_ == "cod") ==== true
    "" \ a.existsThem(_ => true)(_ == "eel")  ==== false
    "" \ oi.existsThem(_ == 5)(_ == "eel")    ==== true
    "" \ oi.existsThem(_ == 4)(_ == "eel")    ==== false
    "" \ oa.existsThem(_ == 5)(_ == "cod")    ==== true
    "" \ oa.existsThem(_ == 5)(_ == "eel")    ==== false
    "" \ on.existsThem(_ eq null)(_ == 4)     ==== true
    "" \ on.existsThem(_ ne null)(_ == 4)     ==== false
    "" \ om.existsThem(_ == 5)(_ eq null)     ==== true
    "" \ om.existsThem(_ == 5)(_ ne null)     ==== false

    "" \ i.forall(_ == 5)     ==== true
    "" \ i.forall(_ == 4)     ==== false
    "" \ a.forall(_ == 5)     ==== true
    "" \ oi.forall(_ == 5)    ==== true
    "" \ oi.forall(_ == 4)    ==== false
    "" \ oa.forall(_ == 5)    ==== true
    "" \ on.forall(_ eq null) ==== true
    "" \ on.forall(_ ne null) ==== false
    "" \ om.forall(_ == 5)    ==== true

    "" \ i.forallAlt(_ == "eel")   ==== true
    "" \ a.forallAlt(_ == "cod")   ==== true
    "" \ a.forallAlt(_ == "eel")   ==== false
    "" \ oi.forallAlt(_ == "eel")  ==== true
    "" \ oa.forallAlt(_ == "cod")  ==== true
    "" \ oa.forallAlt(_ == "eel")  ==== false
    "" \ on.forallAlt(_ == 5)      ==== true
    "" \ om.forallAlt(_ eq null)   ==== true
    "" \ om.forallAlt(_ ne null)   ==== false

    "" \ i.forallThem(_ == 5)(_ => false)     ==== true
    "" \ i.forallThem(_ == 4)(_ => true)      ==== false
    "" \ a.forallThem(_ => false)(_ == "cod") ==== true
    "" \ a.forallThem(_ => true)(_ == "eel")  ==== false
    "" \ oi.forallThem(_ == 5)(_ == "eel")    ==== true
    "" \ oi.forallThem(_ == 4)(_ == "eel")    ==== false
    "" \ oa.forallThem(_ == 5)(_ == "cod")    ==== true
    "" \ oa.forallThem(_ == 5)(_ == "eel")    ==== false
    "" \ on.forallThem(_ eq null)(_ == 4)     ==== true
    "" \ on.forallThem(_ ne null)(_ == 4)     ==== false
    "" \ om.forallThem(_ == 5)(_ eq null)     ==== true
    "" \ om.forallThem(_ == 5)(_ ne null)     ==== false



  @Test
  def orAlterationTest: Unit =
    val valueProvider = new ProvideVariousOrValues()
    import valueProvider._

    "" \ i.fold(_+1)(_ => 0)            ==== 6
    "" \ a.fold(_ => 0)(_.length)       ==== 3
    "" \ oi.fold(_+1)(_.length)         ==== 6
    "" \ oa.fold(_+1)(_.length)         ==== 3
    "" \ oai.fold(_.isDigit)(_.isBoxed) ==== false
    "" \ oia.fold(_.isBoxed)(_.isDigit) ==== true
    "" \ n.fold(_ eq null)(_ => false)  ==== true
    "" \ m.fold(_ => false)(_ eq null)  ==== true
    "" \ on.fold(_ eq null)(_ == 0)     ==== true
    "" \ om.fold(_ == 0)(_ eq null)     ==== true
    "" \ oin.fold(_.isBoxed)(_ == ' ')  ==== false
    "" \ oim.fold(_.isBoxed)(_ == ' ')  ==== true
    "" \ oan.fold(_ == ' ')(_.isBoxed)  ==== false
    "" \ oam.fold(_ == ' ')(_.isBoxed)  ==== true
    "" \ p.fold(_ eq null)(_ => false)  ==== true
    "" \ q.fold(_ => false)(_ eq null)  ==== true
    "" \ op.fold(_ eq null)(_ == 0)     ==== true
    "" \ oq.fold(_ == 0)(_ eq null)     ==== true
    "" \ oip.fold(_.isBoxed)(_ == ' ')  ==== false
    "" \ oiq.fold(_.isBoxed)(_ == ' ')  ==== true
    "" \ oap.fold(_ == ' ')(_.isBoxed)  ==== false
    "" \ oaq.fold(_ == ' ')(_.isBoxed)  ==== true

    "" \ i.getOrElse(_ => 0)                    ==== 5
    "" \ a.getOrElse(_.length)                  ==== 3
    "" \ oi.getOrElse(_.length)                 ==== 5
    "" \ oa.getOrElse(_.length)                 ==== 3
    "" \ oai.getOrElse(_.toString.head)         ==== '5'
    "" \ oia.getOrElse(_.toString.isnt[Int])    ==== Alt("cod")
    "" \ n.getOrElse(_ => throw new Exception)  ==== null
    "" \ m.getOrElse(_ => 0)                    ==== 0
    "" \ on.getOrElse(_ => throw new Exception) ==== null
    "" \ om.getOrElse(_ => 0)                   ==== 0
    "" \ oin.getOrElse(c => Alt(c.toInt))       ==== null
    "" \ oim.getOrElse(c => Is(c.toInt))        ==== Alt(null)
    "" \ oan.getOrElse(_.isBoxed.toString.head) ==== 'f'
    "" \ oam.getOrElse(_.isBoxed.toString.head) ==== 't'
    "" \ p.getOrElse(_.toString)                ==== null
    "" \ q.getOrElse(_ => 0)                    ==== 0
    "" \ op.getOrElse(_.toString)               ==== null
    "" \ oq.getOrElse(_ => 0)                   ==== 0
    "" \ oip.getOrElse(c => Alt(c.toInt))       ==== null
    "" \ oiq.getOrElse(c => Is(c.toInt))        ==== Alt(null)
    "" \ oap.getOrElse(_.isBoxed.toString.head) ==== 'f'
    "" \ oaq.getOrElse(_.isBoxed.toString.head) ==== 't'

    "" \ i.altOrElse(_.toString)                ==== "5"
    "" \ a.altOrElse(_.toString)                ==== "cod"
    "" \ oi.altOrElse(_.toString)               ==== "5"
    "" \ oa.altOrElse(_.toString)               ==== "cod"
    "" \ oai.altOrElse(_.toString.isnt[Int])    ==== Is(5)
    "" \ oia.altOrElse(_.toString.head)         ==== 'A'
    "" \ n.altOrElse(_ => "null")               ==== "null"
    "" \ m.altOrElse(_ => throw new Exception)  ==== null
    "" \ on.altOrElse(_ => "null")              ==== "null"
    "" \ om.altOrElse(_ => throw new Exception) ==== null
    "" \ oin.altOrElse(_.isBoxed.toString.head) ==== 'f'
    "" \ oim.altOrElse(_.isBoxed.toString.head) ==== 't'
    "" \ oan.altOrElse(c => Alt(c.toInt))       ==== null
    "" \ oam.altOrElse(c => Is(c.toInt))        ==== Alt(null)
    "" \ p.altOrElse(_ => "null")               ==== "null"
    "" \ q.altOrElse(_.toString)                ==== null
    "" \ op.altOrElse(_ => "null")              ==== "null"
    "" \ oq.altOrElse(_.toString)               ==== null
    "" \ oip.altOrElse(_.isBoxed.toString.head) ==== 'f'
    "" \ oiq.altOrElse(_.isBoxed.toString.head) ==== 't'
    "" \ oap.altOrElse(c => Alt(c.toInt))       ==== null
    "" \ oaq.altOrElse(c => Is(c.toInt))        ==== Alt(null)

    "" \ i.map(_ > 0)       ==== true
    "" \ a.map(_ => 0)      ==== Alt("cod")
    "" \ oi.map(_ > 0)      ==== true
    "" \ oa.map(_ > 0)      ==== Alt("cod")
    "" \ oii.map(_.isBoxed) ==== false
    "" \ oia.map(_.isBoxed) ==== true
    "" \ oai.map(_ == ' ')  ==== Alt(5)
    "" \ oaa.map(_ == ' ')  ==== Alt(Alt("cod"))
    "" \ n.map(_ eq null)   ==== true
    "" \ m.map(_ => 0)      ==== Alt(null)
    "" \ on.map(_ eq null)  ==== true
    "" \ om.map(_ eq null)  ==== Alt(null)
    "" \ oin.map(_.isBoxed) ==== false
    "" \ oim.map(_.isBoxed) ==== true
    "" \ oan.map(_ == ' ')  ==== Alt(null)
    "" \ oam.map(_ == ' ')  ==== Alt(Alt(null))
    "" \ p.map(_ eq null)   ==== true
    "" \ q.map(_ => 0)      ==== Alt(null)
    "" \ op.map(_ eq null)  ==== true
    "" \ oq.map(_ + 7)      ==== Alt(null)
    "" \ oip.map(_.isBoxed) ==== false
    "" \ oiq.map(_.isBoxed) ==== true
    "" \ oap.map(_ == ' ')  ==== Alt(null)
    "" \ oaq.map(_ == ' ')  ==== Alt(Alt(null))

    "" \ i.mapAlt(_ => "")     ==== 5
    "" \ a.mapAlt(_.length)    ==== Alt(3)
    "" \ oi.mapAlt(_.length)   ==== 5
    "" \ oa.mapAlt(_.length)   ==== Alt(3)
    "" \ oii.mapAlt(_ == ' ')  ==== 5
    "" \ oia.mapAlt(_ == ' ')  ==== Is(Alt("cod"))
    "" \ oai.mapAlt(_.isBoxed) ==== Alt(false)
    "" \ oaa.mapAlt(_.isBoxed) ==== Alt(true)
    "" \ n.mapAlt(_ => ' ')    ==== null
    "" \ m.mapAlt(_ eq null)   ==== Alt(true)
    "" \ on.mapAlt(_ + 1)      ==== null
    "" \ om.mapAlt(_ eq null)  ==== Alt(true)
    "" \ oin.mapAlt(_.toInt)   ==== null
    "" \ oim.mapAlt(_.toInt)   ==== Is(Alt(null))
    "" \ oan.mapAlt(_.isBoxed) ==== Alt(false)
    "" \ oam.mapAlt(_.isBoxed) ==== Alt(true)
    "" \ p.mapAlt(_ => ' ')    ==== null
    "" \ q.mapAlt(_ eq null)   ==== Alt(true)
    "" \ op.mapAlt(_ + 1)      ==== null
    "" \ oq.mapAlt(_ eq null)  ==== Alt(true)
    "" \ oip.mapAlt(_.toInt)   ==== null
    "" \ oiq.mapAlt(_.toInt)   ==== Is(Alt(null))
    "" \ oap.mapAlt(_.isBoxed) ==== Alt(false)
    "" \ oaq.mapAlt(_.isBoxed) ==== Alt(true)

    "" \ i.mapThem(_ + 1)(_ => "")         ==== 6
    "" \ a.mapThem(_ => "")(_.length)      ==== Alt(3)
    "" \ oi.mapThem(_ + 1)(_ + "!")        ==== 6
    "" \ oa.mapThem(_ + 1)(_ + "!")        ==== Alt("cod!")
    "" \ oii.mapThem(_.isBoxed)(_.toUpper) ==== false
    "" \ oia.mapThem(_.isBoxed)(_.toUpper) ==== true
    "" \ oai.mapThem(_.toUpper)(_.isBoxed) ==== Alt(false)
    "" \ oaa.mapThem(_.toUpper)(_.isBoxed) ==== Alt(true)
    "" \ n.mapThem(_ eq null)(_ => "")     ==== true
    "" \ m.mapThem(_ => "")(_ eq null)     ==== Alt(true)
    "" \ on.mapThem(_ eq null)(_ + 1)      ==== true
    "" \ om.mapThem(_ + 1)(_ eq null)      ==== Alt(true)
    "" \ oin.mapThem(_.isBoxed)(_.toUpper) ==== false
    "" \ oim.mapThem(_.isBoxed)(_.toUpper) ==== true
    "" \ oan.mapThem(_.toUpper)(_.isBoxed) ==== Alt(false)
    "" \ oam.mapThem(_.toUpper)(_.isBoxed) ==== Alt(true)
    "" \ p.mapThem(_ eq null)(_ => "")     ==== true
    "" \ q.mapThem(_ => "")(_ eq null)     ==== Alt(true)
    "" \ op.mapThem(_ eq null)(_ + 1)      ==== true
    "" \ oq.mapThem(_ + 1)(_ eq null)      ==== Alt(true)
    "" \ oip.mapThem(_.isBoxed)(_.toUpper) ==== false
    "" \ oiq.mapThem(_.isBoxed)(_.toUpper) ==== true
    "" \ oap.mapThem(_.toUpper)(_.isBoxed) ==== Alt(false)
    "" \ oaq.mapThem(_.toUpper)(_.isBoxed) ==== Alt(true)

    "" \ i.flatMap(_.isIf(_ > 0))         =&&= 5               -> typed[Int Or Int]
    "" \ a.flatMap(_ => 0.or[String])     =&&= Alt("cod")      -> typed[Int Or String]
    "" \ oi.flatMap(_.or[String])         =&&= 5               -> typed[Int Or String]
    "" \ oa.flatMap(_.or[String])         =&&= Alt("cod")      -> typed[Int Or String]
    "" \ oii.flatMap(_.mapAlt(_.head))    =&&= 5               -> typed[Int Or Char]
    "" \ oia.flatMap(_.mapAlt(_.head))    =&&= Alt('c')        -> typed[Int Or Char]
    "" \ oai.flatMap(c => Is(c.toInt))    =&&= Alt(5)          -> typed[Int Or (Int Or String)]
    "" \ oaa.flatMap(c => Is(c.toInt))    =&&= Alt(Alt("cod")) -> typed[Int Or (Int Or String)]
    "" \ n.flatMap(_.isIf(_ ne null))     =&&= Alt(null)       -> typed[Null Or Null]
    "" \ m.flatMap(_ => 0.or[Null])       =&&= Alt(null)       -> typed[Int Or Null]
    "" \ on.flatMap(_.isOr[Int])          =&&= null          :->: typed[Null Or Int]   // null -> stuff doesn't work so used method on typed
    "" \ om.flatMap(_.or[String])         =&&= Alt(null)       -> typed[Int Or String]
    "" \ oin.flatMap(_.isBoxed.or[Char])  =&&= false           -> typed[Boolean Or Char]
    "" \ oim.flatMap(_.isBoxed.or[Char])  =&&= true            -> typed[Boolean Or Char]
    "" \ oan.flatMap(c => Is(c.toInt))    =&&= Alt(null)       -> typed[Int Or (Null Or Int)]
    "" \ oam.flatMap(c => Is(c.toInt))    =&&= Alt(Alt(null))  -> typed[Int Or (Int Or Null)]
    "" \ p.flatMap(_.isIf(_ ne null))     =&&= Alt(null)       -> typed[String Or String]
    "" \ q.flatMap(_ => 0.or[String])     =&&= Alt(null)       -> typed[Int Or String]
    "" \ op.flatMap(_.isOr[Int])          =&&= null          :->: typed[String Or Int]   // null -> stuff doesn't work so used method on typed
    "" \ oq.flatMap(_.or[String])         =&&= Alt(null)       -> typed[Int Or String]
    "" \ oip.flatMap(_.isBoxed.or[Char])  =&&= false           -> typed[Boolean Or Char]
    "" \ oiq.flatMap(_.isBoxed.or[Char])  =&&= true            -> typed[Boolean Or Char]
    "" \ oap.flatMap(c => Is(c.toInt))    =&&= Alt(null)       -> typed[Int Or (String Or Int)]
    "" \ oaq.flatMap(c => Is(c.toInt))    =&&= Alt(Alt(null))  -> typed[Int Or (Int Or String)]

    "" \ i.flatMapAlt(_ => 0.isnt[Int])             =&&= 5               -> typed[Int Or Int]
    "" \ a.flatMapAlt(_.isIf(_.nonEmpty))           =&&= "cod"           -> typed[String Or String]
    "" \ oi.flatMapAlt(_.isnt[Int])                 =&&= 5               -> typed[Int Or String]
    "" \ oa.flatMapAlt(_.isnt[Int])                 =&&= Alt("cod")      -> typed[Int Or String]
    "" \ oii.flatMapAlt(c => Alt(c.toInt))          =&&= 5               -> typed[(Int Or String) Or Int]
    "" \ oia.flatMapAlt(c => Alt(c.toInt))          =&&= Is(Alt("cod"))  -> typed[(Int Or String) Or Int]
    "" \ oai.flatMapAlt(_.map(i => ('a'+i).toChar)) =&&= 'f'             -> typed[Char Or String]
    "" \ oaa.flatMapAlt(_.map(i => ('a'+i).toChar)) =&&= Alt("cod")      -> typed[Char Or String]
    "" \ n.flatMapAlt(_ => 0.isnt[Null])            =&&= null          :->: typed[Null Or Int]
    "" \ m.flatMapAlt(_.isIf(_ eq null))            =&&= null          :->: typed[Null Or Null]
    "" \ on.flatMapAlt(_.isnt[Null])                =&&= null          :->: typed[Null Or Int]
    "" \ om.flatMapAlt(x => (x eq null).isnt[Int])  =&&= Alt(true)       -> typed[Int Or Boolean]
    "" \ oin.flatMapAlt(_ => Alt('e'))              =&&= null          :->: typed[(Null Or Int) Or Char]
    "" \ oim.flatMapAlt(_ => Alt('e'))              =&&= Is(Alt(null)) :->: typed[(Int Or Null) Or Char]
    "" \ oan.flatMapAlt(_.isBoxed.isnt[Char])       =&&= Alt(false)      -> typed[Char Or Boolean]
    "" \ oam.flatMapAlt(_.isBoxed.isnt[Char])       =&&= Alt(true)       -> typed[Char Or Boolean]
    "" \ p.flatMapAlt(_ => 0.isnt[String])          =&&= null          :->: typed[String Or Int]
    "" \ q.flatMapAlt(_.isIf(_ eq null))            =&&= null          :->: typed[String Or String]
    "" \ op.flatMapAlt(_.isnt[String])              =&&= null          :->: typed[String Or Int]
    "" \ oq.flatMapAlt(x => (x eq null).isnt[Int])  =&&= Alt(true)       -> typed[Int Or Boolean]
    "" \ oip.flatMapAlt(_ => Alt('e'))              =&&= null          :->: typed[(String Or Int) Or Char]
    "" \ oiq.flatMapAlt(_ => Alt('e'))              =&&= Is(Alt(null)) :->: typed[(Int Or String) Or Char]
    "" \ oap.flatMapAlt(_.isBoxed.isnt[Char])       =&&= Alt(false)      -> typed[Char Or Boolean]
    "" \ oaq.flatMapAlt(_.isBoxed.isnt[Char])       =&&= Alt(true)       -> typed[Char Or Boolean]

    "" \ i.flatMapThem(_.isIf(_ > 0))(_ => 0.or[Int])                                   =&&= 5         -> typed[Int Or Int]
    "" \ a.flatMapThem(_ => "eel".or[String])(_.isIf(_.nonEmpty))                       =&&= "cod"     -> typed[String Or String]
    "" \ oi.flatMapThem(_.isIf(_ > 0))(s => 0.or[Int])                                  =&&= 5         -> typed[Int Or Int]
    "" \ oa.flatMapThem(_ => "eel".or[String])(_.isIf(_.nonEmpty))                      =&&= "cod"     -> typed[String Or String]
    "" \ oii.flatMapThem(_.swap)(c => if c < ' ' then Is(c.toString) else Alt(c.toInt)) =&&= Alt(5)    -> typed[String Or Int]
    "" \ oia.flatMapThem(_.swap)(c => if c < ' ' then Is(c.toString) else Alt(c.toInt)) =&&= "cod"     -> typed[String Or Int]
    "" \ oai.flatMapThem(c => if c < ' ' then Is(c.toString) else Alt(c.toInt))(_.swap) =&&= Alt(5)    -> typed[String Or Int]
    "" \ oaa.flatMapThem(c => if c < ' ' then Is(c.toString) else Alt(c.toInt))(_.swap) =&&= "cod"     -> typed[String Or Int]
    "" \ n.flatMapThem(_.isIf(_ eq null))(_ => null.isnt[Null])                         =&&= null    :->: typed[Null Or Null]
    "" \ m.flatMapThem(_ => null.isnt[Null])(_.altIf(_ eq null))                        =&&= Alt(null) -> typed[Null Or Null]
    "" \ on.flatMapThem(_.isIf(_ eq null))(i => null.isIf(_ => i >= 0))                 =&&= null    :->: typed[Null Or Null]
    "" \ om.flatMapThem(i => null.isIf(_ => i >= 0))(_.isIf(_ ne null))                 =&&= Alt(null) -> typed[Null Or Null]
    "" \ oin.flatMapThem(_.swap)(c => if c < ' ' then Alt(null) else Is(c.toInt))       =&&= Alt(null) -> typed[Int Or Null]
    "" \ oim.flatMapThem(_.swap)(c => if c < ' ' then Alt(c.toInt) else Is(null))       =&&= null    :->: typed[Null Or Int]
    "" \ oan.flatMapThem(c => if c < ' ' then Alt(null) else Is(c.toInt))(_.swap)       =&&= Alt(null) -> typed[Int Or Null]
    "" \ oam.flatMapThem(c => if c < ' ' then Alt(c.toInt) else Is(null))(_.swap)       =&&= null    :->: typed[Null Or Int]
    "" \ p.flatMapThem(_.isIf(_ eq null))(_ => "".isnt[String])                         =&&= null    :->: typed[String Or String]
    "" \ q.flatMapThem(_ => "".isnt[String])(_.altIf(_ eq null))                        =&&= Alt(null) -> typed[String Or String]
    "" \ op.flatMapThem(_.isIf(_ eq null))(i => "".isIf(_ => i >= 0))                   =&&= null    :->: typed[String Or String]
    "" \ oq.flatMapThem(i => "".isIf(_ => i >= 0))(_.isIf(_ ne null))                   =&&= Alt(null) -> typed[String Or String]
    "" \ oip.flatMapThem(_.swap)(c => if c < ' ' then Alt(null) else Is(c.toInt))       =&&= Alt(null) -> typed[Int Or String]
    "" \ oiq.flatMapThem(_.swap)(c => if c < ' ' then Alt(c.toInt) else Is(null))       =&&= null    :->: typed[String Or Int]
    "" \ oap.flatMapThem(c => if c < ' ' then Alt(null) else Is(c.toInt))(_.swap)       =&&= Alt(null) -> typed[Int Or String]
    "" \ oaq.flatMapThem(c => if c < ' ' then Alt(c.toInt) else Is(null))(_.swap)       =&&= null    :->: typed[String Or Int]

    "" \ i.discard{ case x if x > 0 => "!"*x }                  =&&= Alt("!!!!!")    -> typed[Int Or String]
    "" \ i.discard{ case x if x < 0 => "@"*(-x) }               =&&= 5               -> typed[Int Or String]
    "" \ a.discard{ case x => "salmon" }                        ==== Alt("cod")
    "" \ oi.discard{ case x if x > 0 => "!"*x }                 =&&= Alt("!!!!!")    -> typed[Int Or String]
    "" \ oi.discard{ case x if x < 0 => "@"*(-x) }              =&&= 5               -> typed[Int Or String]
    "" \ oa.discard{ case x if x > 0 => "!"*x }                 =&&= Alt("cod")      -> typed[Int Or String]
    "" \ oii.discard{ case Alt(y) if y.nonEmpty => y.head }     =&&= 5               -> typed[(Int Or String) Or Char]
    "" \ oia.discard{ case Alt(y) if y.nonEmpty => y.head }     =&&= Alt('c')        -> typed[(Int Or String) Or Char]
    "" \ oai.discard{ case c if c < ' ' => c.toInt.or[String] } =&&= Alt(5)          -> typed[Char Or (Int Or String)]
    "" \ oaa.discard{ case c if c < ' ' => c.toInt.or[String] } =&&= Alt(Alt("cod")) -> typed[Char Or (Int Or String)]
    "" \ n.discard{ case x if x eq null => 4 }                  =&&= Alt(4)          -> typed[Null Or Int]
    "" \ n.discard{ case x if x ne null => 4 }                  =&&= null          :->: typed[Null Or Int]
    "" \ m.discard{ case x => "salmon" }                        ==== Alt(null)
    "" \ on.discard{ case x if x eq null => 4 }                 =&&= Alt(4)          -> typed[Null Or Int]
    "" \ on.discard{ case x if x ne null => 4 }                 =&&= null          :->: typed[Null Or Int]
    "" \ om.discard{ case x if x > 0 => null }                  =&&= Alt(null)       -> typed[Int Or Null]
    "" \ oin.discard{ case Alt(y) if y == 0 => '0' }            =&&= null          :->: typed[(Null Or Int) Or Char]
    "" \ oim.discard{ case Alt(y) if y eq null => '0' }         =&&= Alt('0')        -> typed[(Int Or Null) Or Char]
    "" \ oan.discard{ case c if c < ' ' => c.toInt.isnt[Null] } =&&= Alt(null)       -> typed[Char Or (Null Or Int)]
    "" \ oam.discard{ case c if c < ' ' => c.toInt.or[Null] }   =&&= Alt(Alt(null))  -> typed[Char Or (Int Or Null)]
    "" \ p.discard{ case x if x eq null => 4 }                  =&&= Alt(4)          -> typed[String Or Int]
    "" \ p.discard{ case x if x ne null => 4 }                  =&&= null          :->: typed[String Or Int]
    "" \ q.discard{ case x => "salmon" }                        ==== Alt(null)
    "" \ op.discard{ case x if x eq null => 4 }                 =&&= Alt(4)          -> typed[String Or Int]
    "" \ op.discard{ case x if x ne null => 4 }                 =&&= null          :->: typed[String Or Int]
    "" \ oq.discard{ case x if x > 0 => "bass" }                =&&= Alt(null)       -> typed[Int Or String]
    "" \ oip.discard{ case Alt(y) if y == 0 => '0' }            =&&= null          :->: typed[(String Or Int) Or Char]
    "" \ oiq.discard{ case Alt(y) if nlen(y) <= 0 => '0' }      =&&= Alt('0')        -> typed[(Int Or String) Or Char]
    "" \ oan.discard{ case ' ' => 4.isnt[String] }              =&&= Alt(null)       -> typed[Char Or (String Or Int)]
    "" \ oaq.discard{ case ' ' => 4.or[String] }                =&&= Alt(Alt(null))  -> typed[Char Or (Int Or String)]

    "" \ i.reclaim{ case x => 4 }                                 ==== 5
    "" \ a.reclaim{ case s if s.nonEmpty => s.length }            =&&= 3               -> typed[Int Or String]
    "" \ a.reclaim{ case s if s.isEmpty => 4 }                    =&&= Alt("cod")      -> typed[Int Or String]
    "" \ oi.reclaim{ case s if s.isEmpty => 4 }                   =&&= 5               -> typed[Int Or String]
    "" \ oa.reclaim{ case s if s.nonEmpty => s.length }           =&&= 3               -> typed[Int Or String]
    "" \ oa.reclaim{ case s if s.isEmpty => 4 }                   =&&= Alt("cod")      -> typed[Int Or String]
    "" \ oii.reclaim{ case c if c < ' ' => c.toInt.or[String] }   =&&= 5               -> typed[(Int Or String) Or Char]
    "" \ oia.reclaim{ case c if c < ' ' => c.toInt.or[String] }   =&&= Is(Alt("cod"))  -> typed[(Int Or String) Or Char]
    "" \ oai.reclaim{ case Alt(y) if y.nonEmpty => y.head }       =&&= Alt(5)          -> typed[Char Or (Int Or String)]
    "" \ oaa.reclaim{ case Alt(y) if y.nonEmpty => y.head }       =&&= 'c'             -> typed[Char Or (Int Or String)]
    "" \ n.reclaim{ case x => null }                              ==== null
    "" \ m.reclaim{ case x if x eq null => 4 }                    =&&= 4               -> typed[Int Or Null]
    "" \ m.reclaim{ case x if x ne null => 4 }                    =&&= Alt(null)       -> typed[Int Or Null]
    "" \ on.reclaim{ case x if x < 0 => null }                    =&&= null          :->: typed[Null Or Int]
    "" \ om.reclaim{ case x if x eq null => 4 }                   =&&= 4               -> typed[Int Or Null]
    "" \ om.reclaim{ case x if x ne null => 4 }                   =&&= Alt(null)       -> typed[Int Or Null]
    "" \ oin.reclaim{ case c if c < ' ' => c.toInt.isnt[Null] }   =&&= null          :->: typed[(Null Or Int) Or Char]
    "" \ oim.reclaim{ case c if c < ' ' => c.toInt.or[Null] }     =&&= Is(Alt(null))   -> typed[(Int Or Null) Or Char]
    "" \ oan.reclaim{ case Alt(y) if y == 0 => '0' }              =&&= Alt(null)       -> typed[Char Or (Null Or Int)]
    "" \ oam.reclaim{ case Alt(y) if y eq null => '0' }           =&&= '0'             -> typed[Char Or (Int Or Null)]
    "" \ p.reclaim{ case x => null }                              ==== null
    "" \ q.reclaim{ case x if x eq null => 4 }                    =&&= 4               -> typed[Int Or String]
    "" \ q.reclaim{ case x if x ne null => 4 }                    =&&= Alt(null)       -> typed[Int Or String]
    "" \ op.reclaim{ case x if x < 0 => "eel" }                   =&&= null          :->: typed[String Or Int]
    "" \ oq.reclaim{ case x if x eq null => 4 }                   =&&= 4               -> typed[Int Or String]
    "" \ oq.reclaim{ case x if x == "bass" => 4 }                 =&&= Alt(null)       -> typed[Int Or String]
    "" \ oip.reclaim{ case c if c < ' ' => c.toInt.isnt[String] } =&&= null          :->: typed[(String Or Int) Or Char]
    "" \ oiq.reclaim{ case c if c < ' ' => c.toInt.or[String] }   =&&= Is(Alt(null))   -> typed[(Int Or String) Or Char]
    "" \ oap.reclaim{ case Alt(y) if y == 0 => '0' }              =&&= Alt(null)       -> typed[Char Or (String Or Int)]
    "" \ oaq.reclaim{ case Alt(y) if y != "salmon" => '0' }       =&&= '0'             -> typed[Char Or (Int Or String)]

    "" \ oi.alsoDiscard{ case x if x > 0 => x > 3 }     =&&= Alt(true)       -> typed[Int Or (Boolean Or String)]
    "" \ oi.alsoDiscard{ case x if x < 0 => x < -3 }    =&&= 5               -> typed[Int Or (Boolean Or String)]
    "" \ oa.alsoDiscard{ case 0 => true }               =&&= Alt(Alt("cod")) -> typed[Int Or (Boolean Or String)]
    "" \ on.alsoDiscard{ case x if x eq null => true }  =&&= Alt(true)       -> typed[Null Or (Boolean Or Int)]
    "" \ on.alsoDiscard{ case x if x ne null => true }  =&&= null          :->: typed[Null Or (Boolean Or Int)]
    "" \ om.alsoDiscard{ case 0 => true }               =&&= Alt(Alt(null))  -> typed[Int Or (Boolean Or Null)]

  @Test
  def orReshapingTest(): Unit =
    val valueProvider = new ProvideVariousOrValues()
    import valueProvider._

    "" \ i.swap   =&&= Alt(5)          -> typed[Alt[Int]]
    "" \ a.swap   =&&= "cod"           -> typed[Is[String]]
    "" \ oi.swap  =&&= Alt(5)          -> typed[String Or Int]
    "" \ oa.swap  =&&= "cod"           -> typed[String Or Int]
    "" \ oii.swap =&&= Alt(5)          -> typed[Char Or (Int Or String)]
    "" \ oia.swap =&&= Alt(Alt("cod")) -> typed[Char Or (Int Or String)]
    "" \ oai.swap =&&= 5               -> typed[(Int Or String) Or Char]
    "" \ oaa.swap =&&= Is(Alt("cod"))  -> typed[(Int Or String) Or Char]
    "" \ on.swap  =&&= Alt(null)       -> typed[Int Or Null]
    "" \ om.swap  =&&= null          :->: typed[Null Or Int]
    "" \ oin.swap =&&= Alt(null)       -> typed[Char Or (Null Or Int)]
    "" \ oim.swap =&&= Alt(Alt(null))  -> typed[Char Or (Int Or Null)]
    "" \ oan.swap =&&= null          :->: typed[(Null Or Int) Or Char]
    "" \ oam.swap =&&= Is(Alt(null))   -> typed[(Int Or Null) Or Char]
    "" \ op.swap  =&&= Alt(null)       -> typed[Int Or String]
    "" \ oq.swap  =&&= null          :->: typed[String Or Int]
    "" \ oip.swap =&&= Alt(null)       -> typed[Char Or (String Or Int)]
    "" \ oiq.swap =&&= Alt(Alt(null))  -> typed[Char Or (Int Or String)]
    "" \ oap.swap =&&= null          :->: typed[(String Or Int) Or Char]
    "" \ oaq.swap =&&= Is(Alt(null))   -> typed[(Int Or String) Or Char]

    val isc = 'e'.isnt[Int Or String]
    val nic = 'e'.isnt[Null Or Int]
    val sic = 'e'.isnt[String Or Int]
    "" \ oii.pivot =&&= 5             -> typed[Int Or (String Or Char)]
    "" \ oia.pivot =&&= Alt("cod")    -> typed[Int Or (String Or Char)]
    "" \ isc.pivot =&&= Alt(Alt('e')) -> typed[Int Or (String Or Char)]
    "" \ oin.pivot =&&= null        :->: typed[Null Or (Int Or Char)]
    "" \ oim.pivot =&&= Alt(null)     -> typed[Int Or (Null Or Char)]
    "" \ nic.pivot =&&= Alt(Alt('e')) -> typed[Null Or (Int Or Char)]
    "" \ oip.pivot =&&= null        :->: typed[String Or (Int Or Char)]
    "" \ oiq.pivot =&&= Alt(null)     -> typed[Int Or (String Or Char)]
    "" \ sic.pivot =&&= Alt(Alt('e')) -> typed[String Or (Int Or Char)]

    val cis = 'e'.or[Int Or String]
    val cni = 'e'.or[Null Or Int]
    val csi = 'e'.or[String Or Int]
    "" \ oai.unpivot =&&= Is(Alt(5))    -> typed[(Char Or Int) Or String]
    "" \ oaa.unpivot =&&= Alt("cod")    -> typed[(Char Or Int) Or String]
    "" \ cis.unpivot =&&= 'e'           -> typed[(Char Or Int) Or String]
    "" \ oan.unpivot =&&= Is(Alt(null)) -> typed[(Char Or Null) Or Int]
    "" \ oam.unpivot =&&= Alt(null)     -> typed[(Char Or Int) Or Null]
    "" \ cni.unpivot =&&= 'e'           -> typed[(Char Or Null) Or Int]
    "" \ oap.unpivot =&&= Is(Alt(null)) -> typed[(Char Or String) Or Int]
    "" \ oaq.unpivot =&&= Alt(null)     -> typed[(Char Or Int) Or String]
    "" \ csi.unpivot =&&= 'e'           -> typed[(Char Or String) Or Int]

    "" \ i.toOk  =&&= Yes(5)    -> typed[Yes[Int]]
    "" \ a.toOk  =&&= No("cod") -> typed[No[String]]
    "" \ oi.toOk ==== Yes(5)
    "" \ oa.toOk ==== No("cod")
    "" \ on.toOk ==== Yes(null)
    "" \ om.toOk ==== No(null)
    "" \ op.toOk ==== Yes(null)
    "" \ oq.toOk ==== No(null)

    "" \ i.swapToOk  ==== No(5)
    "" \ a.swapToOk  ==== Yes("cod")
    "" \ oi.swapToOk ==== No(5)
    "" \ oa.swapToOk ==== Yes("cod")
    "" \ on.swapToOk ==== No(null)
    "" \ om.swapToOk ==== Yes(null)

    "" \ i.toEither  =&&= Right(5)    -> typed[Either[Nothing, Int]]
    "" \ a.toEither  =&&= Left("cod") -> typed[Either[String, Nothing]]
    "" \ oi.toEither =&&= Right(5)    -> typed[Either[String, Int]]
    "" \ oa.toEither =&&= Left("cod") -> typed[Either[String, Int]]
    "" \ on.toEither ==== Right(null)
    "" \ om.toEither ==== Left(null)

    "" \ i.swapToEither  =&&= Left(5)      -> typed[Either[Int, Nothing]]
    "" \ a.swapToEither  =&&= Right("cod") -> typed[Either[Nothing, String]]
    "" \ oi.swapToEither =&&= Left(5)      -> typed[Either[Int, String]]
    "" \ oa.swapToEither =&&= Right("cod") -> typed[Either[Int, String]]
    "" \ on.swapToEither ==== Left(null)
    "" \ om.swapToEither ==== Right(null)

    "" \ i.toOption  ==== Some(5)
    "" \ a.toOption  ==== None
    "" \ oi.toOption ==== Some(5)
    "" \ oa.toOption ==== None
    "" \ on.toOption ==== Some(null)
    "" \ om.toOption ==== None

    "" \ i.swapToOption  ==== None
    "" \ a.swapToOption  ==== Some("cod")
    "" \ oi.swapToOption ==== None
    "" \ oa.swapToOption ==== Some("cod")
    "" \ on.swapToOption ==== None
    "" \ om.swapToOption ==== Some(null)

    "" \ i.toTry.get  ==== 5
    "" \ a.toTry.get  ==== thrown[WrongBranchException[_]]
    "" \ oi.toTry.get ==== 5
    "" \ oa.toTry.get ==== thrown[WrongBranchException[_]]
    "" \ on.toTry.get ==== null
    "" \ om.toTry.get ==== thrown[WrongBranchException[_]]

    "" \ i.swapToTry.get  ==== thrown[WrongBranchException[_]]
    "" \ a.swapToTry.get  ==== "cod"
    "" \ oi.swapToTry.get ==== thrown[WrongBranchException[_]]
    "" \ oa.swapToTry.get ==== "cod"
    "" \ on.swapToTry.get ==== thrown[WrongBranchException[_]]
    "" \ om.swapToTry.get ==== null



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
