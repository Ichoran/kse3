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
      T(msg) ~ thing.explainAsArray()           =**= ExceptionExplainer.explainAsArray(thing)
      T(msg) ~ thing.explainAsArray()           =**= thing.explainAsVector()
      T(msg) ~ thing.explain()                  ==== thing.explainAsArray().mkString("\n")
      T(msg) ~ thing.explainSuppressedAsArray() =**= ExceptionExplainer.explainAsArray(thing, showSuppressed = true)
      T(msg) ~ thing.explainSuppressedAsArray() =**= thing.explainSuppressedAsVector()
      T(msg) ~ thing.explainSuppressed()        ==== thing.explainSuppressedAsArray().mkString("\n")

    T ~ stackless.explainAsArray().length ==== 1
    T ~ caused.explainAsArray()               exists { x => x.contains("CAUSE") }
    T ~ circular.explainSuppressedAsArray()   exists { x => x.contains("circular-exception-2") }
    T ~ caused.explainAsArray()               exists { x => x startsWith "| " }
    T ~ caused.explainAsArray(childLines = 3) exists { x => x startsWith "| . . ." }

    val short = caused.explainAsArray(lines = 10)
    val full  = caused.explainAsArray()
    T ~ short.take(9) =**= full.take(9)
    val lines = full.drop(9).count(s => !s.startsWith("| "))
    T ~ short.last ==== s". . . (+$lines lines and 1 more exception)"


  @Test
  def repeatTest(): Unit =
    var bit = 1
    cFor(0)(_ < 10)(_ + 1){ _ => bit *= 2 }
    T("int cFor") ~ bit ==== (1 << 10)

    var sum = 0L
    cFor(Int.MaxValue + 10L)(_ >= Int.MaxValue - 10L)(_ - 1){ sum += _ }
    T("long cFor") ~ sum ==== 21L * Int.MaxValue

    val babble = new StringBuilder()
    cFor("hi")(_.length < 20)(s => s + " " + s){ s => if babble.nonEmpty then babble ++= ", "; babble ++= s }
    T("generic cFor") ~ babble.toString ==== "hi, hi hi, hi hi hi hi"

    var sumi = 0
    nFor(1000)(i => sumi += i*i + 1)
    T("int nFor") ~ sumi ==== 332834500

    var suml = 0L
    nFor(10000L)(l => suml += l*l + 1)
    T("long nFor") ~ suml ==== 333283345000L

    val ab = Array[Byte](1, 3, 5, 7, 9)
    var sumab = 0L
    aFor(ab)((b, i) => sumab += b*(i+1))
    T("byte aFor") ~ sumab ==== 95L

    val ac = Array[Char]('s', 'a', 'l', 'm', 'o', 'n')
    val sumac = new StringBuilder
    aFor(ac)((c, i) => sumac ++= c.toString*(i+1))
    T("char aFor") ~ sumac.result ==== "saalllmmmmooooonnnnnn"

    val s  = ac.mkString
    val sums = new StringBuilder
    aFor(s)((c, i) => sums ++= c.toString*(i+1))
    T("string aFor") ~ sums.result ==== "saalllmmmmooooonnnnnn"


    val as = Array[Short](1, 3, 5, 7, 9)
    var sumas = 0L
    aFor(as)((s, i) => sumas += s*(i+1))
    T("short aFor") ~ sumas ==== 95L

    val aj = Array[Int](1, 3, 5, 7 ,9)
    var sumaj = 0L
    aFor(aj)((j, i) => sumaj += j*(i+1))
    T("int aFor") ~ sumaj ==== 95L

    val al = Array[Long](1, 3, 5, 7, 9)
    var sumal = 0L
    aFor(al)((l, i) => sumal += l*(i+1))
    T("long aFor") ~ sumal ==== 95L

    val af = Array[Float](0.1f, 0.3f, 0.5f, 0.7f, 0.9f)
    var sumaf = 0f
    aFor(af)((f, i) => sumaf += f*(i+1))
    T("float aFor") ~ sumaf =~~= 9.5f

    val ad = Array[Double](0.01, 0.03, 0.05, 0.07, 0.09)
    var sumad = 0.0
    aFor(ad)((d, i) => sumad += d*(i+1))
    T("double aFor") ~ sumad =~~= 9.5

    val aa = Array[Option[String]](None, Some("cod"), Some("salmon"))
    val sumaa = new StringBuilder
    aFor(aa){ (o, i) => o match
      case Some(s) =>
        if sumaa.nonEmpty then sumaa ++= ", "
        sumaa ++= s*(i+1)
      case _ =>
    }
    T("generic aFor") ~ sumaa.result ==== "codcod, salmonsalmonsalmon"

    val xs = "cod" :: "bass" :: "perch" :: "salmon" :: Nil
    val fish = new StringBuilder
    iFor(xs.iterator){ (s, i) =>
      if fish.nonEmpty then fish ++= ", "
      fish ++= s*(i+1)
    }
    T("iFor") ~ fish.result ==== "cod, bassbass, perchperchperch, salmonsalmonsalmonsalmon"


  @Test
  def orCreationConversionTest: Unit =
    T("altnum") ~ Alt(5) ==== Alt(5L)

    val aoc = Alt(Option("cod"))
    val asc = Alt(Some("cod"))
    T ~ aoc          ==== asc
    T ~ aoc.##       ==== asc.##
    T ~ aoc          =!!= Alt("cod")
    T ~ aoc.toString ==== "Alt(Some(cod))"
    T ~ aoc          ==== Alt.wrap(Some("cod"))
    T ~ aoc.unwrap   ==== Some("cod")

    T ~ Alt.unit ==== Alt(())

    aoc.favor[Int] match
      case Alt(y) => T ~ y ==== Some("cod")
      case y      => assertTrue(s"Took disfavored branch with $y", false)

    (aoc: Any) match
      case Alt(y) => T ~ y ==== Some("cod")
      case y      => assertTrue(s"Took disfavored branch with $y", false)

    T("isnum") ~ Is(5) ==== Is(5L)

    T ~ Is("salmon") ==== Is("salmon")
    T ~ Is(aoc)      ==== Is(aoc: Any)

    val ioc = Is(Option("cod"))
    val isc = Is(Some("cod"))
    val iaoc = Is(aoc)
    val iiaoc = Is(iaoc)
    T ~ ioc           ==== isc
    T ~ ioc.##        ==== isc.##
    T ~ ioc           ==== Some("cod")
    T ~ ioc.toString  ==== "Some(cod)"
    T ~ iaoc.##       ==== Is(aoc: Any).##
    T ~ iaoc.toString ==== "Is(Alt(Some(cod)))"
    T ~ iaoc          =!!= aoc
    T ~ iiaoc         =!!= Is(aoc)

    T ~ ioc          ==== Is.wrap(Some("cod"))
    T ~ ioc.unwrap   ==== Some("cod")
    T ~ iaoc.unwrap  ==== aoc
    T ~ iiaoc.unwrap ==== iaoc
    T ~ Is.unit      ==== Is(())

    ioc.disfavor[Int] match
      case Is(x) => T ~ x ==== Some("cod")
      case x     => assertTrue(s"Took favored branch with $x", false)

    (ioc: Any) match
      case Is(x) => T ~ x ==== Some("cod")
      case x     => assertTrue(s"Took favored branch with $x", false)

    Is(aoc).disfavor[Int] match
      case Is(x) => T ~ x ==== aoc
      case x     => assertTrue(s"Took favored branch with $x", false)

    (Is(aoc): Any) match
      case Is(x) => T ~ x ==== aoc
      case x     => assertTrue(s"Took favored branch with $x", false)

    T ~ Or.from(Right[Int, String]("herring")) ==== Is("herring")
    T ~ Or.from(Left[Int, String](5))          ==== Alt(5)
    T ~ Or.swapFrom(Right[Int, String]("cod")) ==== Alt("cod")
    T ~ Or.swapFrom(Left[Int, String](9))      ==== Is(9)

    T ~ Or.from(Yes("herring").typeNo[Int]) ==== Is("herring")
    T ~ Or.from(No(5).typeYes[String])      ==== Alt(5)
    T ~ Or.swapFrom(Yes("cod").typeNo[Int]) ==== Alt("cod")
    T ~ Or.swapFrom(No(9).typeYes[String])  ==== Is(9)

    T ~ Or.from(Option("herring"))                  ==== Is("herring")
    T ~ Or.from(Some("herring"))                    ==== Is("herring")
    T ~ Or.from(None: Option[String])               ==== Alt.unit
    T ~ Or.swapFrom(Option(5))                      ==== Alt(5)
    T ~ Or.swapFrom(Some(9))                        ==== Alt(9)
    T ~ Or.swapFrom(None: Option[Int])              ==== Is.unit
    T ~ Or.fromOrElse(Option("herring"), 2)         ==== Is("herring")
    T ~ Or.fromOrElse(Some("herring"), 2)           ==== Is("herring")
    T ~ Or.fromOrElse(None: Option[String], 2)      ==== Alt(2)
    T ~ Or.swapFromOrElse(Option(5), "cod")         ==== Alt(5)
    T ~ Or.swapFromOrElse(Some(9), "cod")           ==== Alt(9)
    T ~ Or.swapFromOrElse(None: Option[Int], "cod") ==== Is("cod")

    val e = new Exception("test-exception")
    val trys = Try{ "herring" }
    val trye = Try{ throw e; 5 }
    T ~ Or.from(trys)                   ==== Is("herring")
    T ~ Or.from(trye)                   ==== Alt(e)
    T ~ Or.from(trye, _.getMessage)     ==== Alt("test-exception")
    T ~ Or.swapFrom(trys)               ==== Alt("herring")
    T ~ Or.swapFrom(trye)               ==== Is(e)
    T ~ Or.swapFrom(trye, _.getMessage) ==== Is("test-exception")

    val dis = ioc.disfavor[Int]
    val fav = aoc.favor[String]
    T ~ dis                                         ==== ioc
    T ~ fav                                         ==== aoc
    T ~ Option("cod").isOr[Int]                     ==== dis   // Have to use `isOr` because of `or` on `Option`
    T ~ Option("cod").isnt[String]                  ==== fav
    T ~ Option("cod").isnt[String].or[Int]          ==== iaoc
    T ~ Option("cod").isnt[String].or[Int].or[Char] ==== iiaoc

    T ~ 5.isIf(_ > 0)                                      ==== 5            --: typed[Int Or Int]
    T ~ 5.isIf(_ < 0)                                      ==== Alt(5)       --: typed[Int Or Int]
    T ~ 5.altIf(_ > 0)                                     ==== Alt(5)       --: typed[Int Or Int]
    T ~ 5.altIf(_ < 0)                                     ==== 5            --: typed[Int Or Int]
    T ~ 5.isCase{ case x if x > 0 => "!"*x }               ==== "!!!!!"      --: typed[String Or Int]
    T ~ 5.isCase{ case x if x < 0 => x.toChar }            ==== Alt(5)       --: typed[Char Or Int]
    T ~ 5.altCase{ case x if x > 0 => "!"*x }              ==== Alt("!!!!!") --: typed[Int Or String]
    T ~ 5.altCase{ case x if x < 0 => x.toChar }           ==== 5            --: typed[Int Or Char]
    T ~ 5.isCaseOrAlt{ case x if x > 0 => "!"*x }{_ < -3}  ==== "!!!!!"      --: typed[String Or Boolean]
    T ~ 5.isCaseOrAlt{ case x if x < 0 => x.toChar}{_ > 3} ==== Alt(true)    --: typed[Char Or Boolean]
    T ~ 5.altCaseOrIs{ case x if x > 0 => "!"*x }{_ < -3}  ==== Alt("!!!!!") --: typed[Boolean Or String]
    T ~ 5.altCaseOrIs{ case x if x < 0 => x.toChar}{_ > 3} ==== true         --: typed[Boolean Or Char]
    T ~ 5.unfoldToOr(_ > 0)("!" * _)(_ < -3)               ==== "!!!!!"      --: typed[String Or Boolean]
    T ~ 5.unfoldToOr(_ < 0)(_.toChar)(_ > 3)               ==== Alt(true)    --: typed[Char Or Boolean]
    T ~ 5.isLike(true.isnt[Int])                           ==== 5            --: typed[Int Or Boolean]
    T ~ 5.altLike('e'.or[Int])                             ==== Alt(5)       --: typed[Char Or Int]
    T ~ 5.altAlso(true.or[Char])                           ==== Alt(5)       --: typed[Boolean Or (Int Or Char)]

    // Extra, probably superfluous tests
    T ~ List(5, -5).map(_.isIf( _ > 0)) ==== List(5, Alt(-5))
    T ~ List(5, -5).map(_.altIf(_ > 0)) ==== List(Alt(5), -5)
    T ~ List(5, -5).map(_.isCase{  case 5 => "five" }) ==== List("five", Alt(-5))
    T ~ List(5, -5).map(_.altCase{ case 5 => "five" }) ==== List(Alt("five"), -5)

    T ~ List("cod", null).map(_.isIf( _ ne null)) ==== List(Is("cod"), Alt(null))
    T ~ List("cod", null).map(_.altIf(_ ne null)) ==== List(Alt("cod"), null)
    T ~ List("cod", null).map(_.isCase{  case "cod" => true}) ==== List(true, Alt(null))
    T ~ List("cod", null).map(_.altCase{ case "cod" => true}) ==== List(Alt(true), null)

    T ~ null.nn           ==== Alt(()) --: typed[Null Or Unit]
    T ~ (null: String).nn ==== Alt(()) --: typed[String Or Unit]
    T ~ "cod".nn          ==== "cod"   --: typed[String Or Unit]


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
    T ~ i    ==== 5
    T ~ a    ==== Alt("cod")
    T ~ oi   ==== 5                    --: typed[Int Or String]
    T ~ oa   ==== Alt("cod")           --: typed[Int Or String]
    T ~ oii  ==== 5                    --: typed[(Int Or String) Or Char]
    T ~ oia  ==== Is(Alt("cod"))       --: typed[(Int Or String) Or Char]
    T ~ oai  ==== Alt(5)               --: typed[Char Or (Int Or String)]
    T ~ oaa  ==== Alt(Alt("cod"))      --: typed[Char Or (Int Or String)]
    T ~ oiii ==== 5                    --: typed[((Int Or String) Or Char) Or Long]
    T ~ oiia ==== Is(Is(Alt("cod")))   --: typed[((Int Or String) Or Char) Or Long]
    T ~ oiai ==== Is(Alt(5))           --: typed[(Char Or (Int Or String)) Or Long]
    T ~ oiaa ==== Is(Alt(Alt("cod")))  --: typed[(Char Or (Int Or String)) Or Long]
    T ~ oaii ==== Alt(5)               --: typed[Long Or ((Int Or String) Or Char)]
    T ~ oaia ==== Alt(Is(Alt("cod")))  --: typed[Long Or ((Int Or String) Or Char)]
    T ~ oaai ==== Alt(Alt(5))          --: typed[Long Or (Char Or (Int Or String))]
    T ~ oaaa ==== Alt(Alt(Alt("cod"))) --: typed[Long Or (Char Or (Int Or String))]
    T ~ n    ==== null
    T ~ m    ==== Alt(null)
    T ~ on   ==== null                 --: typed[Null Or Int]
    T ~ om   ==== Alt(null)            --: typed[Int Or Null]
    T ~ oin  ==== null                 --: typed[(Null Or Int) Or Char]
    T ~ oim  ==== Is(Alt(null))        --: typed[(Int Or Null) Or Char]
    T ~ oan  ==== Alt(null)            --: typed[Char Or (Null Or Int)]
    T ~ oam  ==== Alt(Alt(null))       --: typed[Char Or (Int Or Null)]
    T ~ p    ==== null
    T ~ q    ==== Alt(null)
    T ~ op   ==== null                 --: typed[String Or Int]
    T ~ oq   ==== Alt(null)            --: typed[Int Or String]
    T ~ oip  ==== null                 --: typed[(String Or Int) Or Char]
    T ~ oiq  ==== Is(Alt(null))        --: typed[(Int Or String) Or Char]
    T ~ oap  ==== Alt(null)            --: typed[Char Or (String Or Int)]
    T ~ oaq  ==== Alt(Alt(null))       --: typed[Char Or (Int Or String)]

    T ~ i.get   ==== 5
    T ~ a.get   ==== thrown[NoSuchElementException]
    T ~ i.alt   ==== thrown[NoSuchElementException]
    T ~ a.alt   ==== "cod"
    T ~ oi.get  ==== 5
    T ~ oi.alt  ==== thrown[NoSuchElementException]
    T ~ oia.get ==== Alt("cod")
    T ~ oia.alt ==== thrown[NoSuchElementException]
    T ~ oa.get  ==== thrown[NoSuchElementException]
    T ~ oa.alt  ==== "cod"
    T ~ oai.get ==== thrown[NoSuchElementException]
    T ~ oai.alt ==== Is(5)
    T ~ n.get   ==== null
    T ~ n.alt   ==== thrown[NoSuchElementException] 
    T ~ m.get   ==== thrown[NoSuchElementException]
    T ~ m.alt   ==== null    
    T ~ on.get  ==== null
    T ~ on.alt  ==== thrown[NoSuchElementException]    
    T ~ om.get  ==== thrown[NoSuchElementException]
    T ~ om.alt  ==== null    
    T ~ oin.get ==== null
    T ~ oin.alt ==== thrown[NoSuchElementException]    
    T ~ oim.get ==== Alt(null)
    T ~ oim.alt ==== thrown[NoSuchElementException]   
    T ~ oan.get ==== thrown[NoSuchElementException]
    T ~ oan.alt ==== null    
    T ~ oam.get ==== thrown[NoSuchElementException]
    T ~ oam.alt ==== Alt(null)   
    T ~ p.get   ==== null
    T ~ p.alt   ==== thrown[NoSuchElementException] 
    T ~ q.get   ==== thrown[NoSuchElementException]
    T ~ q.alt   ==== null    
    T ~ op.get  ==== null
    T ~ op.alt  ==== thrown[NoSuchElementException]    
    T ~ oq.get  ==== thrown[NoSuchElementException]
    T ~ oq.alt  ==== null    
    T ~ oip.get ==== null
    T ~ oip.alt ==== thrown[NoSuchElementException]    
    T ~ oiq.get ==== Alt(null)
    T ~ oiq.alt ==== thrown[NoSuchElementException]   
    T ~ oap.get ==== thrown[NoSuchElementException]
    T ~ oap.alt ==== null    
    T ~ oaq.get ==== thrown[NoSuchElementException]
    T ~ oaq.alt ==== Alt(null)   

    T ~ i.value      ==== 5
    T ~ a.value      ==== "cod"
    T ~ oi.value     ==== 5
    T ~ oa.value     ==== "cod"
    T ~ oii.value    ==== Is(5)
    T ~ oia.value    ==== Alt("cod")
    T ~ oai.value    ==== Is(5)
    T ~ oaa.value    ==== Alt("cod")
    T ~ oa.value     ==== typed[Int | String]
    T ~ oi.value     ==== typed[Int | String]
    T ~ n.value      ==== null
    T ~ m.value      ==== null
    T ~ on.value     ==== null
    T ~ om.value     ==== null
    T ~ oin.value    ==== null
    T ~ oim.value    ==== Alt(null)
    T ~ oan.value    ==== null
    T ~ oam.value    ==== Alt(null)
    T ~ p.value      ==== null
    T ~ q.value      ==== null
    T ~ op.value     ==== null
    T ~ oq.value     ==== null
    T ~ oip.value    ==== null
    T ~ oiq.value    ==== Alt(null)
    T ~ oap.value    ==== null
    T ~ oaq.value    ==== Alt(null)

    T ~ i.isBoxed      ==== false
    T ~ a.isBoxed      ==== true
    T ~ oi.isBoxed     ==== false
    T ~ oa.isBoxed     ==== true
    T ~ oii.isBoxed    ==== false
    T ~ oia.isBoxed    ==== true
    T ~ oai.isBoxed    ==== true
    T ~ oaa.isBoxed    ==== true
    T ~ Is(a).isBoxed  ==== true
    T ~ Alt(a).isBoxed ==== true
    T ~ n.isBoxed      ==== false
    T ~ m.isBoxed      ==== true
    T ~ on.isBoxed     ==== false
    T ~ om.isBoxed     ==== true
    T ~ oin.isBoxed    ==== false
    T ~ oim.isBoxed    ==== true
    T ~ oan.isBoxed    ==== true
    T ~ oam.isBoxed    ==== true
    T ~ p.isBoxed      ==== false
    T ~ q.isBoxed      ==== true
    T ~ op.isBoxed     ==== false
    T ~ oq.isBoxed     ==== true
    T ~ oip.isBoxed    ==== false
    T ~ oiq.isBoxed    ==== true
    T ~ oap.isBoxed    ==== true
    T ~ oaq.isBoxed    ==== true

    T ~ { var x = 0; i.foreach(x = _)                 ; x } ==== 5
    T ~ { var x = 0; a.foreach(_ => x = 1)            ; x } ==== 0
    T ~ { var x = 0; oi.foreach(x = _)                ; x } ==== 5
    T ~ { var x = 0; oa.foreach(x = _)                ; x } ==== 0
    T ~ { var x = 0; oii.foreach(_.foreach(x = _))    ; x } ==== 5
    T ~ { var x = 0; oia.foreach(_.foreach(x = _))    ; x } ==== 0
    T ~ { var x = 0; oai.foreach(c => x = c.toInt)    ; x } ==== 0
    T ~ { var x = 0; oaa.foreach(c => x = c.toInt)    ; x } ==== 0
    T ~ { var x = 0; n.foreach(y => x = nullone(y))   ; x } ==== 1
    T ~ { var x = 0; m.foreach(_ => x = 1)            ; x } ==== 0
    T ~ { var x = 0; on.foreach(y => x = nullone(y))  ; x } ==== 1
    T ~ { var x = 0; om.foreach(x = _)                ; x } ==== 0
    T ~ { var x = 0; oin.foreach(y => x = nullone(y)) ; x } ==== 1
    T ~ { var x = 0; oim.foreach(y => x = nullone(y)) ; x } ==== -1
    T ~ { var x = 0; oan.foreach(c => x = c.toInt)    ; x } ==== 0
    T ~ { var x = 0; oam.foreach(c => x = c.toInt)    ; x } ==== 0
    T ~ { var x = 0; p.foreach(y => x = nlen(y))      ; x } ==== -1
    T ~ { var x = 0; q.foreach(_ => x = 1)            ; x } ==== 0
    T ~ { var x = 0; op.foreach(y => x = nlen(y))     ; x } ==== -1
    T ~ { var x = 0; oq.foreach(x = _)                ; x } ==== 0
    T ~ { var x = 0; oip.foreach(y => x = nlen(y.get)); x } ==== -1
    T ~ { var x = 0; oiq.foreach(y => x = nlen(y.alt)); x } ==== -1
    T ~ { var x = 0; oap.foreach(c => x = c.toInt)    ; x } ==== 0
    T ~ { var x = 0; oaq.foreach(c => x = c.toInt)    ; x } ==== 0

    T ~ { var x = 0; i.foreachAlt(_ => x = 1);             ; x } ==== 0
    T ~ { var x = 0; a.foreachAlt(s => x = s.length)       ; x } ==== 3
    T ~ { var x = 0; oi.foreachAlt(s => x = s.length)      ; x } ==== 0
    T ~ { var x = 0; oa.foreachAlt(s => x = s.length)      ; x } ==== 3
    T ~ { var x = 0; oii.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    T ~ { var x = 0; oia.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    T ~ { var x = 0; oai.foreachAlt(y => x = y.get)        ; x } ==== 5
    T ~ { var x = 0; oaa.foreachAlt(y => x = y.alt.length) ; x } ==== 3
    T ~ { var x = 0; n.foreachAlt(_ => x = 1)              ; x } ==== 0
    T ~ { var x = 0; m.foreachAlt(y => x = nullone(y))     ; x } ==== 1
    T ~ { var x = 0; on.foreachAlt(x = _)                  ; x } ==== 0
    T ~ { var x = 0; om.foreachAlt(y => x = nullone(y))    ; x } ==== 1
    T ~ { var x = 0; oin.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    T ~ { var x = 0; oim.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    T ~ { var x = 0; oan.foreachAlt(y => x = nullone(y))   ; x } ==== 1
    T ~ { var x = 0; oam.foreachAlt(y => x = nullone(y))   ; x } ==== -1
    T ~ { var x = 0; p.foreachAlt(_ => x = 1)              ; x } ==== 0
    T ~ { var x = 0; q.foreachAlt(y => x = nlen(y))        ; x } ==== -1
    T ~ { var x = 0; op.foreachAlt(x = _)                  ; x } ==== 0
    T ~ { var x = 0; oq.foreachAlt(y => x = nlen(y))       ; x } ==== -1
    T ~ { var x = 0; oip.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    T ~ { var x = 0; oiq.foreachAlt(c => x = c.toInt)      ; x } ==== 0
    T ~ { var x = 0; oap.foreachAlt(y => x = nlen(y.get))  ; x } ==== -1
    T ~ { var x = 0; oaq.foreachAlt(y => x = nlen(y.alt))  ; x } ==== -1

    T ~ { var x = 0; i.foreachThem(x = _)(_ => x = 4)                                  ; x } ==== 5
    T ~ { var x = 0; a.foreachThem(_ => x = 4)(s => x = s.length)                      ; x } ==== 3
    T ~ { var x = 0; oi.foreachThem(x = _)(s => x = s.length)                          ; x } ==== 5
    T ~ { var x = 0; oa.foreachThem(x = _)(s => x = s.length)                          ; x } ==== 3
    T ~ { var x = 0; oii.foreachThem(y => x = y.fold(_+1)(_.length))(c => x = c.toInt) ; x } ==== 6
    T ~ { var x = 0; oia.foreachThem(y => x = y.fold(_+1)(_.length))(c => x = c.toInt) ; x } ==== 3
    T ~ { var x = 0; oai.foreachThem(c => x = c.toInt)(y => x = y.fold(_+1)(_.length)) ; x } ==== 6
    T ~ { var x = 0; oaa.foreachThem(c => x = c.toInt)(y => x = y.fold(_+1)(_.length)) ; x } ==== 3
    T ~ { var x = 0; n.foreachThem(y => x = nullone(y))(_ => x = 4)                    ; x } ==== 1
    T ~ { var x = 0; m.foreachThem(_ => x = 4)(y => x = nullone(y))                    ; x } ==== 1
    T ~ { var x = 0; on.foreachThem(y => x = nullone(y))(x = _)                        ; x } ==== 1
    T ~ { var x = 0; om.foreachThem(x = _)(y => x = nullone(y))                        ; x } ==== 1
    T ~ { var x = 0; oin.foreachThem(y => x = nullone(y))(c => x = c.toInt)            ; x } ==== 1
    T ~ { var x = 0; oim.foreachThem(y => x = nullone(y))(c => x = c.toInt)            ; x } ==== -1
    T ~ { var x = 0; oan.foreachThem(c => x = c.toInt)(y => x = nullone(y))            ; x } ==== 1
    T ~ { var x = 0; oam.foreachThem(c => x = c.toInt)(y => x = nullone(y))            ; x } ==== -1
    T ~ { var x = 0; p.foreachThem(y => x = nlen(y))(_ => x = 4)                       ; x } ==== -1
    T ~ { var x = 0; q.foreachThem(_ => x = 4)(y => x = nlen(y))                       ; x } ==== -1
    T ~ { var x = 0; op.foreachThem(y => x = nlen(y))(x = _)                           ; x } ==== -1
    T ~ { var x = 0; oq.foreachThem(x = _)(y => x = nlen(y))                           ; x } ==== -1
    T ~ { var x = 0; oip.foreachThem(y => x = nlen(y.get))(c => x = c.toInt)           ; x } ==== -1
    T ~ { var x = 0; oiq.foreachThem(y => x = nlen(y.alt))(c => x = c.toInt)           ; x } ==== -1
    T ~ { var x = 0; oap.foreachThem(c => x = c.toInt)(y => x = nlen(y.get))           ; x } ==== -1
    T ~ { var x = 0; oaq.foreachThem(c => x = c.toInt)(y => x = nlen(y.alt))           ; x } ==== -1

    T ~ { var x = 0; i.use(x = _)                  :==: typedLike(i)  ; x } ==== 5
    T ~ { var x = 0; a.use(_ => x = 1)             :==: typedLike(a)  ; x } ==== 0
    T ~ { var x = 0; oi.use(x = _)                 :==: typedLike(oi) ; x } ==== 5
    T ~ { var x = 0; oa.use(x = _)                 :==: typedLike(oa) ; x } ==== 0
    T ~ { var x = 0; oii.use(_.use(x = _))         :==: typedLike(oii); x } ==== 5
    T ~ { var x = 0; oia.use(_.use(x = _))         :==: typedLike(oia); x } ==== 0
    T ~ { var x = 0; oai.use(c => x = c.toInt)     :==: typedLike(oai); x } ==== 0
    T ~ { var x = 0; oaa.use(c => x = c.toInt)     :==: typedLike(oaa); x } ==== 0
    T ~ { var x = 0; n.use(y => x = nullone(y))    :==: typedLike(n)  ; x } ==== 1
    T ~ { var x = 0; m.use(_ => x = 1)             :==: typedLike(m)  ; x } ==== 0
    T ~ { var x = 0; on.use(y => x = nullone(y))   :==: typedLike(on) ; x } ==== 1
    T ~ { var x = 0; om.use(x = _)                 :==: typedLike(om) ; x } ==== 0
    T ~ { var x = 0; oin.use(y => x = nullone(y))  :==: typedLike(oin); x } ==== 1
    T ~ { var x = 0; oim.use(y => x = nullone(y))  :==: typedLike(oim); x } ==== -1
    T ~ { var x = 0; oan.use(c => x = c.toInt)     :==: typedLike(oan); x } ==== 0
    T ~ { var x = 0; oam.use(c => x = c.toInt)     :==: typedLike(oam); x } ==== 0
    T ~ { var x = 0; p.use(y => x = nlen(y))       :==: typedLike(p)  ; x } ==== -1
    T ~ { var x = 0; q.use(_ => x = 1)             :==: typedLike(q)  ; x } ==== 0
    T ~ { var x = 0; op.use(y => x = nlen(y))      :==: typedLike(op) ; x } ==== -1
    T ~ { var x = 0; oq.use(x = _)                 :==: typedLike(oq) ; x } ==== 0
    T ~ { var x = 0; oip.use(y => x = nlen(y.get)) :==: typedLike(oip); x } ==== -1
    T ~ { var x = 0; oiq.use(y => x = nlen(y.alt)) :==: typedLike(oiq); x } ==== -1
    T ~ { var x = 0; oap.use(c => x = c.toInt)     :==: typedLike(oap); x } ==== 0
    T ~ { var x = 0; oaq.use(c => x = c.toInt)     :==: typedLike(oaq); x } ==== 0

    T ~ { var x = 0; i.useAlt(_ => x = 1)              :==: typedLike(i)  ; x } ==== 0
    T ~ { var x = 0; a.useAlt(s => x = s.length)       :==: typedLike(a)  ; x } ==== 3
    T ~ { var x = 0; oi.useAlt(s => x = s.length)      :==: typedLike(oi) ; x } ==== 0
    T ~ { var x = 0; oa.useAlt(s => x = s.length)      :==: typedLike(oa) ; x } ==== 3
    T ~ { var x = 0; oii.useAlt(c => x = c.toInt)      :==: typedLike(oii); x } ==== 0
    T ~ { var x = 0; oia.useAlt(c => x = c.toInt)      :==: typedLike(oia); x } ==== 0
    T ~ { var x = 0; oai.useAlt(y => x = y.get)        :==: typedLike(oai); x } ==== 5
    T ~ { var x = 0; oaa.useAlt(y => x = y.alt.length) :==: typedLike(oaa); x } ==== 3
    T ~ { var x = 0; n.useAlt(_ => x = 1)              :==: typedLike(n)  ; x } ==== 0
    T ~ { var x = 0; m.useAlt(y => x = nullone(y))     :==: typedLike(m)  ; x } ==== 1
    T ~ { var x = 0; on.useAlt(x = _)                  :==: typedLike(on) ; x } ==== 0
    T ~ { var x = 0; om.useAlt(y => x = nullone(y))    :==: typedLike(om) ; x } ==== 1
    T ~ { var x = 0; oin.useAlt(c => x = c.toInt)      :==: typedLike(oin); x } ==== 0
    T ~ { var x = 0; oim.useAlt(c => x = c.toInt)      :==: typedLike(oim); x } ==== 0
    T ~ { var x = 0; oan.useAlt(y => x = nullone(y))   :==: typedLike(oan); x } ==== 1
    T ~ { var x = 0; oam.useAlt(y => x = nullone(y))   :==: typedLike(oam); x } ==== -1
    T ~ { var x = 0; p.useAlt(_ => x = 1)              :==: typedLike(p)  ; x } ==== 0
    T ~ { var x = 0; q.useAlt(y => x = nlen(y))        :==: typedLike(q)  ; x } ==== -1
    T ~ { var x = 0; op.useAlt(x = _)                  :==: typedLike(op) ; x } ==== 0
    T ~ { var x = 0; oq.useAlt(y => x = nlen(y))       :==: typedLike(oq) ; x } ==== -1
    T ~ { var x = 0; oip.useAlt(c => x = c.toInt)      :==: typedLike(oip); x } ==== 0
    T ~ { var x = 0; oiq.useAlt(c => x = c.toInt)      :==: typedLike(oiq); x } ==== 0
    T ~ { var x = 0; oap.useAlt(y => x = nlen(y.get))  :==: typedLike(oap); x } ==== -1
    T ~ { var x = 0; oaq.useAlt(y => x = nlen(y.alt))  :==: typedLike(oaq); x } ==== -1

    T ~ { var x = 0; i.useThem(x = _)(_ => x = 4)                                  :==: typedLike(i)  ; x } ==== 5
    T ~ { var x = 0; a.useThem(_ => x = 4)(s => x = s.length)                      :==: typedLike(a)  ; x } ==== 3
    T ~ { var x = 0; oi.useThem(x = _)(s => x = s.length)                          :==: typedLike(oi) ; x } ==== 5
    T ~ { var x = 0; oa.useThem(x = _)(s => x = s.length)                          :==: typedLike(oa) ; x } ==== 3
    T ~ { var x = 0; oii.useThem(y => x = y.fold(_+1)(_.length))(c => x = c.toInt) :==: typedLike(oii); x } ==== 6
    T ~ { var x = 0; oia.useThem(y => x = y.fold(_+1)(_.length))(c => x = c.toInt) :==: typedLike(oia); x } ==== 3
    T ~ { var x = 0; oai.useThem(c => x = c.toInt)(y => x = y.fold(_+1)(_.length)) :==: typedLike(oai); x } ==== 6
    T ~ { var x = 0; oaa.useThem(c => x = c.toInt)(y => x = y.fold(_+1)(_.length)) :==: typedLike(oaa); x } ==== 3
    T ~ { var x = 0; n.useThem(y => x = nullone(y))(_ => x = 4)                    :==: typedLike(n)  ; x } ==== 1
    T ~ { var x = 0; m.useThem(_ => x = 4)(y => x = nullone(y))                    :==: typedLike(m)  ; x } ==== 1
    T ~ { var x = 0; on.useThem(y => x = nullone(y))(x = _)                        :==: typedLike(on) ; x } ==== 1
    T ~ { var x = 0; om.useThem(x = _)(y => x = nullone(y))                        :==: typedLike(om) ; x } ==== 1
    T ~ { var x = 0; oin.useThem(y => x = nullone(y))(c => x = c.toInt)            :==: typedLike(oin); x } ==== 1
    T ~ { var x = 0; oim.useThem(y => x = nullone(y))(c => x = c.toInt)            :==: typedLike(oim); x } ==== -1
    T ~ { var x = 0; oan.useThem(c => x = c.toInt)(y => x = nullone(y))            :==: typedLike(oan); x } ==== 1
    T ~ { var x = 0; oam.useThem(c => x = c.toInt)(y => x = nullone(y))            :==: typedLike(oam); x } ==== -1
    T ~ { var x = 0; p.useThem(y => x = nlen(y))(_ => x = 4)                       :==: typedLike(p)  ; x } ==== -1
    T ~ { var x = 0; q.useThem(_ => x = 4)(y => x = nlen(y))                       :==: typedLike(q)  ; x } ==== -1
    T ~ { var x = 0; op.useThem(y => x = nlen(y))(x = _)                           :==: typedLike(op) ; x } ==== -1
    T ~ { var x = 0; oq.useThem(x = _)(y => x = nlen(y))                           :==: typedLike(oq) ; x } ==== -1
    T ~ { var x = 0; oip.useThem(y => x = nlen(y.get))(c => x = c.toInt)           :==: typedLike(oip); x } ==== -1
    T ~ { var x = 0; oiq.useThem(y => x = nlen(y.alt))(c => x = c.toInt)           :==: typedLike(oiq); x } ==== -1
    T ~ { var x = 0; oap.useThem(c => x = c.toInt)(y => x = nlen(y.get))           :==: typedLike(oap); x } ==== -1
    T ~ { var x = 0; oaq.useThem(c => x = c.toInt)(y => x = nlen(y.alt))           :==: typedLike(oaq); x } ==== -1

    T ~ i.exists(_ == 5)     ==== true
    T ~ i.exists(_ == 4)     ==== false
    T ~ a.exists(_ == 5)     ==== false
    T ~ oi.exists(_ == 5)    ==== true
    T ~ oi.exists(_ == 4)    ==== false
    T ~ oa.exists(_ == 5)    ==== false
    T ~ on.exists(_ eq null) ==== true
    T ~ on.exists(_ ne null) ==== false
    T ~ om.exists(_ == 5)    ==== false

    T ~ i.existsAlt(_ == "eel")   ==== false
    T ~ a.existsAlt(_ == "cod")   ==== true
    T ~ a.existsAlt(_ == "eel")   ==== false
    T ~ oi.existsAlt(_ == "eel")  ==== false
    T ~ oa.existsAlt(_ == "cod")  ==== true
    T ~ oa.existsAlt(_ == "eel")  ==== false
    T ~ on.existsAlt(_ == 5)      ==== false
    T ~ om.existsAlt(_ eq null)   ==== true
    T ~ om.existsAlt(_ ne null)   ==== false

    T ~ i.existsThem(_ == 5)(_ => false)     ==== true
    T ~ i.existsThem(_ == 4)(_ => true)      ==== false
    T ~ a.existsThem(_ => false)(_ == "cod") ==== true
    T ~ a.existsThem(_ => true)(_ == "eel")  ==== false
    T ~ oi.existsThem(_ == 5)(_ == "eel")    ==== true
    T ~ oi.existsThem(_ == 4)(_ == "eel")    ==== false
    T ~ oa.existsThem(_ == 5)(_ == "cod")    ==== true
    T ~ oa.existsThem(_ == 5)(_ == "eel")    ==== false
    T ~ on.existsThem(_ eq null)(_ == 4)     ==== true
    T ~ on.existsThem(_ ne null)(_ == 4)     ==== false
    T ~ om.existsThem(_ == 5)(_ eq null)     ==== true
    T ~ om.existsThem(_ == 5)(_ ne null)     ==== false

    T ~ i.forall(_ == 5)     ==== true
    T ~ i.forall(_ == 4)     ==== false
    T ~ a.forall(_ == 5)     ==== true
    T ~ oi.forall(_ == 5)    ==== true
    T ~ oi.forall(_ == 4)    ==== false
    T ~ oa.forall(_ == 5)    ==== true
    T ~ on.forall(_ eq null) ==== true
    T ~ on.forall(_ ne null) ==== false
    T ~ om.forall(_ == 5)    ==== true

    T ~ i.forallAlt(_ == "eel")   ==== true
    T ~ a.forallAlt(_ == "cod")   ==== true
    T ~ a.forallAlt(_ == "eel")   ==== false
    T ~ oi.forallAlt(_ == "eel")  ==== true
    T ~ oa.forallAlt(_ == "cod")  ==== true
    T ~ oa.forallAlt(_ == "eel")  ==== false
    T ~ on.forallAlt(_ == 5)      ==== true
    T ~ om.forallAlt(_ eq null)   ==== true
    T ~ om.forallAlt(_ ne null)   ==== false

    T ~ i.forallThem(_ == 5)(_ => false)     ==== true
    T ~ i.forallThem(_ == 4)(_ => true)      ==== false
    T ~ a.forallThem(_ => false)(_ == "cod") ==== true
    T ~ a.forallThem(_ => true)(_ == "eel")  ==== false
    T ~ oi.forallThem(_ == 5)(_ == "eel")    ==== true
    T ~ oi.forallThem(_ == 4)(_ == "eel")    ==== false
    T ~ oa.forallThem(_ == 5)(_ == "cod")    ==== true
    T ~ oa.forallThem(_ == 5)(_ == "eel")    ==== false
    T ~ on.forallThem(_ eq null)(_ == 4)     ==== true
    T ~ on.forallThem(_ ne null)(_ == 4)     ==== false
    T ~ om.forallThem(_ == 5)(_ eq null)     ==== true
    T ~ om.forallThem(_ == 5)(_ ne null)     ==== false



  @Test
  def orAlterationTest: Unit =
    val valueProvider = new ProvideVariousOrValues()
    import valueProvider._

    T ~ i.fold(_+1)(_ => 0)            ==== 6
    T ~ a.fold(_ => 0)(_.length)       ==== 3
    T ~ oi.fold(_+1)(_.length)         ==== 6
    T ~ oa.fold(_+1)(_.length)         ==== 3
    T ~ oai.fold(_.isDigit)(_.isBoxed) ==== false
    T ~ oia.fold(_.isBoxed)(_.isDigit) ==== true
    T ~ n.fold(_ eq null)(_ => false)  ==== true
    T ~ m.fold(_ => false)(_ eq null)  ==== true
    T ~ on.fold(_ eq null)(_ == 0)     ==== true
    T ~ om.fold(_ == 0)(_ eq null)     ==== true
    T ~ oin.fold(_.isBoxed)(_ == ' ')  ==== false
    T ~ oim.fold(_.isBoxed)(_ == ' ')  ==== true
    T ~ oan.fold(_ == ' ')(_.isBoxed)  ==== false
    T ~ oam.fold(_ == ' ')(_.isBoxed)  ==== true
    T ~ p.fold(_ eq null)(_ => false)  ==== true
    T ~ q.fold(_ => false)(_ eq null)  ==== true
    T ~ op.fold(_ eq null)(_ == 0)     ==== true
    T ~ oq.fold(_ == 0)(_ eq null)     ==== true
    T ~ oip.fold(_.isBoxed)(_ == ' ')  ==== false
    T ~ oiq.fold(_.isBoxed)(_ == ' ')  ==== true
    T ~ oap.fold(_ == ' ')(_.isBoxed)  ==== false
    T ~ oaq.fold(_ == ' ')(_.isBoxed)  ==== true

    T ~ i.getOrElse(_ => 0)                    ==== 5
    T ~ a.getOrElse(_.length)                  ==== 3
    T ~ oi.getOrElse(_.length)                 ==== 5
    T ~ oa.getOrElse(_.length)                 ==== 3
    T ~ oai.getOrElse(_.toString.head)         ==== '5'
    T ~ oia.getOrElse(_.toString.isnt[Int])    ==== Alt("cod")
    T ~ n.getOrElse(_ => throw new Exception)  ==== null
    T ~ m.getOrElse(_ => 0)                    ==== 0
    T ~ on.getOrElse(_ => throw new Exception) ==== null
    T ~ om.getOrElse(_ => 0)                   ==== 0
    T ~ oin.getOrElse(c => Alt(c.toInt))       ==== null
    T ~ oim.getOrElse(c => Is(c.toInt))        ==== Alt(null)
    T ~ oan.getOrElse(_.isBoxed.toString.head) ==== 'f'
    T ~ oam.getOrElse(_.isBoxed.toString.head) ==== 't'
    T ~ p.getOrElse(_.toString)                ==== null
    T ~ q.getOrElse(_ => 0)                    ==== 0
    T ~ op.getOrElse(_.toString)               ==== null
    T ~ oq.getOrElse(_ => 0)                   ==== 0
    T ~ oip.getOrElse(c => Alt(c.toInt))       ==== null
    T ~ oiq.getOrElse(c => Is(c.toInt))        ==== Alt(null)
    T ~ oap.getOrElse(_.isBoxed.toString.head) ==== 'f'
    T ~ oaq.getOrElse(_.isBoxed.toString.head) ==== 't'

    T ~ i.altOrElse(_.toString)                ==== "5"
    T ~ a.altOrElse(_.toString)                ==== "cod"
    T ~ oi.altOrElse(_.toString)               ==== "5"
    T ~ oa.altOrElse(_.toString)               ==== "cod"
    T ~ oai.altOrElse(_.toString.isnt[Int])    ==== Is(5)
    T ~ oia.altOrElse(_.toString.head)         ==== 'A'
    T ~ n.altOrElse(_ => "null")               ==== "null"
    T ~ m.altOrElse(_ => throw new Exception)  ==== null
    T ~ on.altOrElse(_ => "null")              ==== "null"
    T ~ om.altOrElse(_ => throw new Exception) ==== null
    T ~ oin.altOrElse(_.isBoxed.toString.head) ==== 'f'
    T ~ oim.altOrElse(_.isBoxed.toString.head) ==== 't'
    T ~ oan.altOrElse(c => Alt(c.toInt))       ==== null
    T ~ oam.altOrElse(c => Is(c.toInt))        ==== Alt(null)
    T ~ p.altOrElse(_ => "null")               ==== "null"
    T ~ q.altOrElse(_.toString)                ==== null
    T ~ op.altOrElse(_ => "null")              ==== "null"
    T ~ oq.altOrElse(_.toString)               ==== null
    T ~ oip.altOrElse(_.isBoxed.toString.head) ==== 'f'
    T ~ oiq.altOrElse(_.isBoxed.toString.head) ==== 't'
    T ~ oap.altOrElse(c => Alt(c.toInt))       ==== null
    T ~ oaq.altOrElse(c => Is(c.toInt))        ==== Alt(null)

    T ~ i.map(_ > 0)       ==== true
    T ~ a.map(_ => 0)      ==== Alt("cod")
    T ~ oi.map(_ > 0)      ==== true
    T ~ oa.map(_ > 0)      ==== Alt("cod")
    T ~ oii.map(_.isBoxed) ==== false
    T ~ oia.map(_.isBoxed) ==== true
    T ~ oai.map(_ == ' ')  ==== Alt(5)
    T ~ oaa.map(_ == ' ')  ==== Alt(Alt("cod"))
    T ~ n.map(_ eq null)   ==== true
    T ~ m.map(_ => 0)      ==== Alt(null)
    T ~ on.map(_ eq null)  ==== true
    T ~ om.map(_ eq null)  ==== Alt(null)
    T ~ oin.map(_.isBoxed) ==== false
    T ~ oim.map(_.isBoxed) ==== true
    T ~ oan.map(_ == ' ')  ==== Alt(null)
    T ~ oam.map(_ == ' ')  ==== Alt(Alt(null))
    T ~ p.map(_ eq null)   ==== true
    T ~ q.map(_ => 0)      ==== Alt(null)
    T ~ op.map(_ eq null)  ==== true
    T ~ oq.map(_ + 7)      ==== Alt(null)
    T ~ oip.map(_.isBoxed) ==== false
    T ~ oiq.map(_.isBoxed) ==== true
    T ~ oap.map(_ == ' ')  ==== Alt(null)
    T ~ oaq.map(_ == ' ')  ==== Alt(Alt(null))

    T ~ i.mapAlt(_ => "")     ==== 5
    T ~ a.mapAlt(_.length)    ==== Alt(3)
    T ~ oi.mapAlt(_.length)   ==== 5
    T ~ oa.mapAlt(_.length)   ==== Alt(3)
    T ~ oii.mapAlt(_ == ' ')  ==== 5
    T ~ oia.mapAlt(_ == ' ')  ==== Is(Alt("cod"))
    T ~ oai.mapAlt(_.isBoxed) ==== Alt(false)
    T ~ oaa.mapAlt(_.isBoxed) ==== Alt(true)
    T ~ n.mapAlt(_ => ' ')    ==== null
    T ~ m.mapAlt(_ eq null)   ==== Alt(true)
    T ~ on.mapAlt(_ + 1)      ==== null
    T ~ om.mapAlt(_ eq null)  ==== Alt(true)
    T ~ oin.mapAlt(_.toInt)   ==== null
    T ~ oim.mapAlt(_.toInt)   ==== Is(Alt(null))
    T ~ oan.mapAlt(_.isBoxed) ==== Alt(false)
    T ~ oam.mapAlt(_.isBoxed) ==== Alt(true)
    T ~ p.mapAlt(_ => ' ')    ==== null
    T ~ q.mapAlt(_ eq null)   ==== Alt(true)
    T ~ op.mapAlt(_ + 1)      ==== null
    T ~ oq.mapAlt(_ eq null)  ==== Alt(true)
    T ~ oip.mapAlt(_.toInt)   ==== null
    T ~ oiq.mapAlt(_.toInt)   ==== Is(Alt(null))
    T ~ oap.mapAlt(_.isBoxed) ==== Alt(false)
    T ~ oaq.mapAlt(_.isBoxed) ==== Alt(true)

    T ~ i.mapThem(_ + 1)(_ => "")         ==== 6
    T ~ a.mapThem(_ => "")(_.length)      ==== Alt(3)
    T ~ oi.mapThem(_ + 1)(_ + "!")        ==== 6
    T ~ oa.mapThem(_ + 1)(_ + "!")        ==== Alt("cod!")
    T ~ oii.mapThem(_.isBoxed)(_.toUpper) ==== false
    T ~ oia.mapThem(_.isBoxed)(_.toUpper) ==== true
    T ~ oai.mapThem(_.toUpper)(_.isBoxed) ==== Alt(false)
    T ~ oaa.mapThem(_.toUpper)(_.isBoxed) ==== Alt(true)
    T ~ n.mapThem(_ eq null)(_ => "")     ==== true
    T ~ m.mapThem(_ => "")(_ eq null)     ==== Alt(true)
    T ~ on.mapThem(_ eq null)(_ + 1)      ==== true
    T ~ om.mapThem(_ + 1)(_ eq null)      ==== Alt(true)
    T ~ oin.mapThem(_.isBoxed)(_.toUpper) ==== false
    T ~ oim.mapThem(_.isBoxed)(_.toUpper) ==== true
    T ~ oan.mapThem(_.toUpper)(_.isBoxed) ==== Alt(false)
    T ~ oam.mapThem(_.toUpper)(_.isBoxed) ==== Alt(true)
    T ~ p.mapThem(_ eq null)(_ => "")     ==== true
    T ~ q.mapThem(_ => "")(_ eq null)     ==== Alt(true)
    T ~ op.mapThem(_ eq null)(_ + 1)      ==== true
    T ~ oq.mapThem(_ + 1)(_ eq null)      ==== Alt(true)
    T ~ oip.mapThem(_.isBoxed)(_.toUpper) ==== false
    T ~ oiq.mapThem(_.isBoxed)(_.toUpper) ==== true
    T ~ oap.mapThem(_.toUpper)(_.isBoxed) ==== Alt(false)
    T ~ oaq.mapThem(_.toUpper)(_.isBoxed) ==== Alt(true)

    T ~ i.flatMap(_.isIf(_ > 0))         ==== 5               --: typed[Int Or Int]
    T ~ a.flatMap(_ => 0.or[String])     ==== Alt("cod")      --: typed[Int Or String]
    T ~ oi.flatMap(_.or[String])         ==== 5               --: typed[Int Or String]
    T ~ oa.flatMap(_.or[String])         ==== Alt("cod")      --: typed[Int Or String]
    T ~ oii.flatMap(_.mapAlt(_.head))    ==== 5               --: typed[Int Or Char]
    T ~ oia.flatMap(_.mapAlt(_.head))    ==== Alt('c')        --: typed[Int Or Char]
    T ~ oai.flatMap(c => Is(c.toInt))    ==== Alt(5)          --: typed[Int Or (Int Or String)]
    T ~ oaa.flatMap(c => Is(c.toInt))    ==== Alt(Alt("cod")) --: typed[Int Or (Int Or String)]
    T ~ n.flatMap(_.isIf(_ ne null))     ==== Alt(null)       --: typed[Null Or Null]
    T ~ m.flatMap(_ => 0.or[Null])       ==== Alt(null)       --: typed[Int Or Null]
    T ~ on.flatMap(_.isOr[Int])          ==== null            --: typed[Null Or Int]
    T ~ om.flatMap(_.or[String])         ==== Alt(null)       --: typed[Int Or String]
    T ~ oin.flatMap(_.isBoxed.or[Char])  ==== false           --: typed[Boolean Or Char]
    T ~ oim.flatMap(_.isBoxed.or[Char])  ==== true            --: typed[Boolean Or Char]
    T ~ oan.flatMap(c => Is(c.toInt))    ==== Alt(null)       --: typed[Int Or (Null Or Int)]
    T ~ oam.flatMap(c => Is(c.toInt))    ==== Alt(Alt(null))  --: typed[Int Or (Int Or Null)]
    T ~ p.flatMap(_.isIf(_ ne null))     ==== Alt(null)       --: typed[String Or String]
    T ~ q.flatMap(_ => 0.or[String])     ==== Alt(null)       --: typed[Int Or String]
    T ~ op.flatMap(_.isOr[Int])          ==== null            --: typed[String Or Int]
    T ~ oq.flatMap(_.or[String])         ==== Alt(null)       --: typed[Int Or String]
    T ~ oip.flatMap(_.isBoxed.or[Char])  ==== false           --: typed[Boolean Or Char]
    T ~ oiq.flatMap(_.isBoxed.or[Char])  ==== true            --: typed[Boolean Or Char]
    T ~ oap.flatMap(c => Is(c.toInt))    ==== Alt(null)       --: typed[Int Or (String Or Int)]
    T ~ oaq.flatMap(c => Is(c.toInt))    ==== Alt(Alt(null))  --: typed[Int Or (Int Or String)]

    T ~ i.flatMapAlt(_ => 0.isnt[Int])             ==== 5              --: typed[Int Or Int]
    T ~ a.flatMapAlt(_.isIf(_.nonEmpty))           ==== "cod"          --: typed[String Or String]
    T ~ oi.flatMapAlt(_.isnt[Int])                 ==== 5              --: typed[Int Or String]
    T ~ oa.flatMapAlt(_.isnt[Int])                 ==== Alt("cod")     --: typed[Int Or String]
    T ~ oii.flatMapAlt(c => Alt(c.toInt))          ==== 5              --: typed[(Int Or String) Or Int]
    T ~ oia.flatMapAlt(c => Alt(c.toInt))          ==== Is(Alt("cod")) --: typed[(Int Or String) Or Int]
    T ~ oai.flatMapAlt(_.map(i => ('a'+i).toChar)) ==== 'f'            --: typed[Char Or String]
    T ~ oaa.flatMapAlt(_.map(i => ('a'+i).toChar)) ==== Alt("cod")     --: typed[Char Or String]
    T ~ n.flatMapAlt(_ => 0.isnt[Null])            ==== null           --: typed[Null Or Int]
    T ~ m.flatMapAlt(_.isIf(_ eq null))            ==== null           --: typed[Null Or Null]
    T ~ on.flatMapAlt(_.isnt[Null])                ==== null           --: typed[Null Or Int]
    T ~ om.flatMapAlt(x => (x eq null).isnt[Int])  ==== Alt(true)      --: typed[Int Or Boolean]
    T ~ oin.flatMapAlt(_ => Alt('e'))              ==== null           --: typed[(Null Or Int) Or Char]
    T ~ oim.flatMapAlt(_ => Alt('e'))              ==== Is(Alt(null))  --: typed[(Int Or Null) Or Char]
    T ~ oan.flatMapAlt(_.isBoxed.isnt[Char])       ==== Alt(false)     --: typed[Char Or Boolean]
    T ~ oam.flatMapAlt(_.isBoxed.isnt[Char])       ==== Alt(true)      --: typed[Char Or Boolean]
    T ~ p.flatMapAlt(_ => 0.isnt[String])          ==== null           --: typed[String Or Int]
    T ~ q.flatMapAlt(_.isIf(_ eq null))            ==== null           --: typed[String Or String]
    T ~ op.flatMapAlt(_.isnt[String])              ==== null           --: typed[String Or Int]
    T ~ oq.flatMapAlt(x => (x eq null).isnt[Int])  ==== Alt(true)      --: typed[Int Or Boolean]
    T ~ oip.flatMapAlt(_ => Alt('e'))              ==== null           --: typed[(String Or Int) Or Char]
    T ~ oiq.flatMapAlt(_ => Alt('e'))              ==== Is(Alt(null))  --: typed[(Int Or String) Or Char]
    T ~ oap.flatMapAlt(_.isBoxed.isnt[Char])       ==== Alt(false)     --: typed[Char Or Boolean]
    T ~ oaq.flatMapAlt(_.isBoxed.isnt[Char])       ==== Alt(true)      --: typed[Char Or Boolean]

    T ~ i.flatMapThem(_.isIf(_ > 0))(_ => 0.or[Int])                                   ==== 5         --: typed[Int Or Int]
    T ~ a.flatMapThem(_ => "eel".or[String])(_.isIf(_.nonEmpty))                       ==== "cod"     --: typed[String Or String]
    T ~ oi.flatMapThem(_.isIf(_ > 0))(s => 0.or[Int])                                  ==== 5         --: typed[Int Or Int]
    T ~ oa.flatMapThem(_ => "eel".or[String])(_.isIf(_.nonEmpty))                      ==== "cod"     --: typed[String Or String]
    T ~ oii.flatMapThem(_.swap)(c => if c < ' ' then Is(c.toString) else Alt(c.toInt)) ==== Alt(5)    --: typed[String Or Int]
    T ~ oia.flatMapThem(_.swap)(c => if c < ' ' then Is(c.toString) else Alt(c.toInt)) ==== "cod"     --: typed[String Or Int]
    T ~ oai.flatMapThem(c => if c < ' ' then Is(c.toString) else Alt(c.toInt))(_.swap) ==== Alt(5)    --: typed[String Or Int]
    T ~ oaa.flatMapThem(c => if c < ' ' then Is(c.toString) else Alt(c.toInt))(_.swap) ==== "cod"     --: typed[String Or Int]
    T ~ n.flatMapThem(_.isIf(_ eq null))(_ => null.isnt[Null])                         ==== null      --: typed[Null Or Null]
    T ~ m.flatMapThem(_ => null.isnt[Null])(_.altIf(_ eq null))                        ==== Alt(null) --: typed[Null Or Null]
    T ~ on.flatMapThem(_.isIf(_ eq null))(i => null.isIf(_ => i >= 0))                 ==== null      --: typed[Null Or Null]
    T ~ om.flatMapThem(i => null.isIf(_ => i >= 0))(_.isIf(_ ne null))                 ==== Alt(null) --: typed[Null Or Null]
    T ~ oin.flatMapThem(_.swap)(c => if c < ' ' then Alt(null) else Is(c.toInt))       ==== Alt(null) --: typed[Int Or Null]
    T ~ oim.flatMapThem(_.swap)(c => if c < ' ' then Alt(c.toInt) else Is(null))       ==== null      --: typed[Null Or Int]
    T ~ oan.flatMapThem(c => if c < ' ' then Alt(null) else Is(c.toInt))(_.swap)       ==== Alt(null) --: typed[Int Or Null]
    T ~ oam.flatMapThem(c => if c < ' ' then Alt(c.toInt) else Is(null))(_.swap)       ==== null      --: typed[Null Or Int]
    T ~ p.flatMapThem(_.isIf(_ eq null))(_ => "".isnt[String])                         ==== null      --: typed[String Or String]
    T ~ q.flatMapThem(_ => "".isnt[String])(_.altIf(_ eq null))                        ==== Alt(null) --: typed[String Or String]
    T ~ op.flatMapThem(_.isIf(_ eq null))(i => "".isIf(_ => i >= 0))                   ==== null      --: typed[String Or String]
    T ~ oq.flatMapThem(i => "".isIf(_ => i >= 0))(_.isIf(_ ne null))                   ==== Alt(null) --: typed[String Or String]
    T ~ oip.flatMapThem(_.swap)(c => if c < ' ' then Alt(null) else Is(c.toInt))       ==== Alt(null) --: typed[Int Or String]
    T ~ oiq.flatMapThem(_.swap)(c => if c < ' ' then Alt(c.toInt) else Is(null))       ==== null      --: typed[String Or Int]
    T ~ oap.flatMapThem(c => if c < ' ' then Alt(null) else Is(c.toInt))(_.swap)       ==== Alt(null) --: typed[Int Or String]
    T ~ oaq.flatMapThem(c => if c < ' ' then Alt(c.toInt) else Is(null))(_.swap)       ==== null      --: typed[String Or Int]

    T ~ i.discard{ case x if x > 0 => "!"*x }                  ==== Alt("!!!!!")    --: typed[Int Or String]
    T ~ i.discard{ case x if x < 0 => "@"*(-x) }               ==== 5               --: typed[Int Or String]
    T ~ a.discard{ case x => "salmon" }                        ==== Alt("cod")
    T ~ oi.discard{ case x if x > 0 => "!"*x }                 ==== Alt("!!!!!")    --: typed[Int Or String]
    T ~ oi.discard{ case x if x < 0 => "@"*(-x) }              ==== 5               --: typed[Int Or String]
    T ~ oa.discard{ case x if x > 0 => "!"*x }                 ==== Alt("cod")      --: typed[Int Or String]
    T ~ oii.discard{ case Alt(y) if y.nonEmpty => y.head }     ==== 5               --: typed[(Int Or String) Or Char]
    T ~ oia.discard{ case Alt(y) if y.nonEmpty => y.head }     ==== Alt('c')        --: typed[(Int Or String) Or Char]
    T ~ oai.discard{ case c if c < ' ' => c.toInt.or[String] } ==== Alt(5)          --: typed[Char Or (Int Or String)]
    T ~ oaa.discard{ case c if c < ' ' => c.toInt.or[String] } ==== Alt(Alt("cod")) --: typed[Char Or (Int Or String)]
    T ~ n.discard{ case x if x eq null => 4 }                  ==== Alt(4)          --: typed[Null Or Int]
    T ~ n.discard{ case x if x ne null => 4 }                  ==== null            --: typed[Null Or Int]
    T ~ m.discard{ case x => "salmon" }                        ==== Alt(null)
    T ~ on.discard{ case x if x eq null => 4 }                 ==== Alt(4)          --: typed[Null Or Int]
    T ~ on.discard{ case x if x ne null => 4 }                 ==== null            --: typed[Null Or Int]
    T ~ om.discard{ case x if x > 0 => null }                  ==== Alt(null)       --: typed[Int Or Null]
    T ~ oin.discard{ case Alt(y) if y == 0 => '0' }            ==== null            --: typed[(Null Or Int) Or Char]
    T ~ oim.discard{ case Alt(y) if y eq null => '0' }         ==== Alt('0')        --: typed[(Int Or Null) Or Char]
    T ~ oan.discard{ case c if c < ' ' => c.toInt.isnt[Null] } ==== Alt(null)       --: typed[Char Or (Null Or Int)]
    T ~ oam.discard{ case c if c < ' ' => c.toInt.or[Null] }   ==== Alt(Alt(null))  --: typed[Char Or (Int Or Null)]
    T ~ p.discard{ case x if x eq null => 4 }                  ==== Alt(4)          --: typed[String Or Int]
    T ~ p.discard{ case x if x ne null => 4 }                  ==== null            --: typed[String Or Int]
    T ~ q.discard{ case x => "salmon" }                        ==== Alt(null)
    T ~ op.discard{ case x if x eq null => 4 }                 ==== Alt(4)          --: typed[String Or Int]
    T ~ op.discard{ case x if x ne null => 4 }                 ==== null            --: typed[String Or Int]
    T ~ oq.discard{ case x if x > 0 => "bass" }                ==== Alt(null)       --: typed[Int Or String]
    T ~ oip.discard{ case Alt(y) if y == 0 => '0' }            ==== null            --: typed[(String Or Int) Or Char]
    T ~ oiq.discard{ case Alt(y) if nlen(y) <= 0 => '0' }      ==== Alt('0')        --: typed[(Int Or String) Or Char]
    T ~ oan.discard{ case ' ' => 4.isnt[String] }              ==== Alt(null)       --: typed[Char Or (String Or Int)]
    T ~ oaq.discard{ case ' ' => 4.or[String] }                ==== Alt(Alt(null))  --: typed[Char Or (Int Or String)]

    T ~ i.reclaim{ case x => 4 }                                 ==== 5
    T ~ a.reclaim{ case s if s.nonEmpty => s.length }            ==== 3              --: typed[Int Or String]
    T ~ a.reclaim{ case s if s.isEmpty => 4 }                    ==== Alt("cod")     --: typed[Int Or String]
    T ~ oi.reclaim{ case s if s.isEmpty => 4 }                   ==== 5              --: typed[Int Or String]
    T ~ oa.reclaim{ case s if s.nonEmpty => s.length }           ==== 3              --: typed[Int Or String]
    T ~ oa.reclaim{ case s if s.isEmpty => 4 }                   ==== Alt("cod")     --: typed[Int Or String]
    T ~ oii.reclaim{ case c if c < ' ' => c.toInt.or[String] }   ==== 5              --: typed[(Int Or String) Or Char]
    T ~ oia.reclaim{ case c if c < ' ' => c.toInt.or[String] }   ==== Is(Alt("cod")) --: typed[(Int Or String) Or Char]
    T ~ oai.reclaim{ case Alt(y) if y.nonEmpty => y.head }       ==== Alt(5)         --: typed[Char Or (Int Or String)]
    T ~ oaa.reclaim{ case Alt(y) if y.nonEmpty => y.head }       ==== 'c'            --: typed[Char Or (Int Or String)]
    T ~ n.reclaim{ case x => null }                              ==== null
    T ~ m.reclaim{ case x if x eq null => 4 }                    ==== 4              --: typed[Int Or Null]
    T ~ m.reclaim{ case x if x ne null => 4 }                    ==== Alt(null)      --: typed[Int Or Null]
    T ~ on.reclaim{ case x if x < 0 => null }                    ==== null           --: typed[Null Or Int]
    T ~ om.reclaim{ case x if x eq null => 4 }                   ==== 4              --: typed[Int Or Null]
    T ~ om.reclaim{ case x if x ne null => 4 }                   ==== Alt(null)      --: typed[Int Or Null]
    T ~ oin.reclaim{ case c if c < ' ' => c.toInt.isnt[Null] }   ==== null           --: typed[(Null Or Int) Or Char]
    T ~ oim.reclaim{ case c if c < ' ' => c.toInt.or[Null] }     ==== Is(Alt(null))  --: typed[(Int Or Null) Or Char]
    T ~ oan.reclaim{ case Alt(y) if y == 0 => '0' }              ==== Alt(null)      --: typed[Char Or (Null Or Int)]
    T ~ oam.reclaim{ case Alt(y) if y eq null => '0' }           ==== '0'            --: typed[Char Or (Int Or Null)]
    T ~ p.reclaim{ case x => null }                              ==== null
    T ~ q.reclaim{ case x if x eq null => 4 }                    ==== 4              --: typed[Int Or String]
    T ~ q.reclaim{ case x if x ne null => 4 }                    ==== Alt(null)      --: typed[Int Or String]
    T ~ op.reclaim{ case x if x < 0 => "eel" }                   ==== null           --: typed[String Or Int]
    T ~ oq.reclaim{ case x if x eq null => 4 }                   ==== 4              --: typed[Int Or String]
    T ~ oq.reclaim{ case x if x == "bass" => 4 }                 ==== Alt(null)      --: typed[Int Or String]
    T ~ oip.reclaim{ case c if c < ' ' => c.toInt.isnt[String] } ==== null           --: typed[(String Or Int) Or Char]
    T ~ oiq.reclaim{ case c if c < ' ' => c.toInt.or[String] }   ==== Is(Alt(null))  --: typed[(Int Or String) Or Char]
    T ~ oap.reclaim{ case Alt(y) if y == 0 => '0' }              ==== Alt(null)      --: typed[Char Or (String Or Int)]
    T ~ oaq.reclaim{ case Alt(y) if y != "salmon" => '0' }       ==== '0'            --: typed[Char Or (Int Or String)]

    T ~ oi.alsoDiscard{ case x if x > 0 => x > 3 }     ==== Alt(true)       --: typed[Int Or (Boolean Or String)]
    T ~ oi.alsoDiscard{ case x if x < 0 => x < -3 }    ==== 5               --: typed[Int Or (Boolean Or String)]
    T ~ oa.alsoDiscard{ case 0 => true }               ==== Alt(Alt("cod")) --: typed[Int Or (Boolean Or String)]
    T ~ on.alsoDiscard{ case x if x eq null => true }  ==== Alt(true)       --: typed[Null Or (Boolean Or Int)]
    T ~ on.alsoDiscard{ case x if x ne null => true }  ==== null            --: typed[Null Or (Boolean Or Int)]
    T ~ om.alsoDiscard{ case 0 => true }               ==== Alt(Alt(null))  --: typed[Int Or (Boolean Or Null)]

  @Test
  def orReshapingTest(): Unit =
    val valueProvider = new ProvideVariousOrValues()
    import valueProvider._

    T ~ i.swap   ==== Alt(5)          --: typed[Alt[Int]]
    T ~ a.swap   ==== "cod"           --: typed[Is[String]]
    T ~ oi.swap  ==== Alt(5)          --: typed[String Or Int]
    T ~ oa.swap  ==== "cod"           --: typed[String Or Int]
    T ~ oii.swap ==== Alt(5)          --: typed[Char Or (Int Or String)]
    T ~ oia.swap ==== Alt(Alt("cod")) --: typed[Char Or (Int Or String)]
    T ~ oai.swap ==== 5               --: typed[(Int Or String) Or Char]
    T ~ oaa.swap ==== Is(Alt("cod"))  --: typed[(Int Or String) Or Char]
    T ~ on.swap  ==== Alt(null)       --: typed[Int Or Null]
    T ~ om.swap  ==== null            --: typed[Null Or Int]
    T ~ oin.swap ==== Alt(null)       --: typed[Char Or (Null Or Int)]
    T ~ oim.swap ==== Alt(Alt(null))  --: typed[Char Or (Int Or Null)]
    T ~ oan.swap ==== null            --: typed[(Null Or Int) Or Char]
    T ~ oam.swap ==== Is(Alt(null))   --: typed[(Int Or Null) Or Char]
    T ~ op.swap  ==== Alt(null)       --: typed[Int Or String]
    T ~ oq.swap  ==== null            --: typed[String Or Int]
    T ~ oip.swap ==== Alt(null)       --: typed[Char Or (String Or Int)]
    T ~ oiq.swap ==== Alt(Alt(null))  --: typed[Char Or (Int Or String)]
    T ~ oap.swap ==== null            --: typed[(String Or Int) Or Char]
    T ~ oaq.swap ==== Is(Alt(null))   --: typed[(Int Or String) Or Char]

    val isc = 'e'.isnt[Int Or String]
    val nic = 'e'.isnt[Null Or Int]
    val sic = 'e'.isnt[String Or Int]
    T ~ oii.pivot ==== 5             --: typed[Int Or (String Or Char)]
    T ~ oia.pivot ==== Alt("cod")    --: typed[Int Or (String Or Char)]
    T ~ isc.pivot ==== Alt(Alt('e')) --: typed[Int Or (String Or Char)]
    T ~ oin.pivot ==== null          --: typed[Null Or (Int Or Char)]
    T ~ oim.pivot ==== Alt(null)     --: typed[Int Or (Null Or Char)]
    T ~ nic.pivot ==== Alt(Alt('e')) --: typed[Null Or (Int Or Char)]
    T ~ oip.pivot ==== null          --: typed[String Or (Int Or Char)]
    T ~ oiq.pivot ==== Alt(null)     --: typed[Int Or (String Or Char)]
    T ~ sic.pivot ==== Alt(Alt('e')) --: typed[String Or (Int Or Char)]

    val cis = 'e'.or[Int Or String]
    val cni = 'e'.or[Null Or Int]
    val csi = 'e'.or[String Or Int]
    T ~ oai.unpivot ==== Is(Alt(5))    --: typed[(Char Or Int) Or String]
    T ~ oaa.unpivot ==== Alt("cod")    --: typed[(Char Or Int) Or String]
    T ~ cis.unpivot ==== 'e'           --: typed[(Char Or Int) Or String]
    T ~ oan.unpivot ==== Is(Alt(null)) --: typed[(Char Or Null) Or Int]
    T ~ oam.unpivot ==== Alt(null)     --: typed[(Char Or Int) Or Null]
    T ~ cni.unpivot ==== 'e'           --: typed[(Char Or Null) Or Int]
    T ~ oap.unpivot ==== Is(Alt(null)) --: typed[(Char Or String) Or Int]
    T ~ oaq.unpivot ==== Alt(null)     --: typed[(Char Or Int) Or String]
    T ~ csi.unpivot ==== 'e'           --: typed[(Char Or String) Or Int]

    T ~ i.toOk  ==== Yes(5)    --: typed[Yes[Int]]
    T ~ a.toOk  ==== No("cod") --: typed[No[String]]
    T ~ oi.toOk ==== Yes(5)
    T ~ oa.toOk ==== No("cod")
    T ~ on.toOk ==== Yes(null)
    T ~ om.toOk ==== No(null)
    T ~ op.toOk ==== Yes(null)
    T ~ oq.toOk ==== No(null)

    T ~ i.swapToOk  ==== No(5)
    T ~ a.swapToOk  ==== Yes("cod")
    T ~ oi.swapToOk ==== No(5)
    T ~ oa.swapToOk ==== Yes("cod")
    T ~ on.swapToOk ==== No(null)
    T ~ om.swapToOk ==== Yes(null)

    T ~ i.toEither  ==== Right(5)    --: typed[Either[Nothing, Int]]
    T ~ a.toEither  ==== Left("cod") --: typed[Either[String, Nothing]]
    T ~ oi.toEither ==== Right(5)    --: typed[Either[String, Int]]
    T ~ oa.toEither ==== Left("cod") --: typed[Either[String, Int]]
    T ~ on.toEither ==== Right(null)
    T ~ om.toEither ==== Left(null)

    T ~ i.swapToEither  ==== Left(5)      --: typed[Either[Int, Nothing]]
    T ~ a.swapToEither  ==== Right("cod") --: typed[Either[Nothing, String]]
    T ~ oi.swapToEither ==== Left(5)      --: typed[Either[Int, String]]
    T ~ oa.swapToEither ==== Right("cod") --: typed[Either[Int, String]]
    T ~ on.swapToEither ==== Left(null)
    T ~ om.swapToEither ==== Right(null)

    T ~ i.toOption  ==== Some(5)
    T ~ a.toOption  ==== None
    T ~ oi.toOption ==== Some(5)
    T ~ oa.toOption ==== None
    T ~ on.toOption ==== Some(null)
    T ~ om.toOption ==== None

    T ~ i.swapToOption  ==== None
    T ~ a.swapToOption  ==== Some("cod")
    T ~ oi.swapToOption ==== None
    T ~ oa.swapToOption ==== Some("cod")
    T ~ on.swapToOption ==== None
    T ~ om.swapToOption ==== Some(null)

    T ~ i.toTry.get  ==== 5
    T ~ a.toTry.get  ==== thrown[WrongBranchException[_]]
    T ~ oi.toTry.get ==== 5
    T ~ oa.toTry.get ==== thrown[WrongBranchException[_]]
    T ~ on.toTry.get ==== null
    T ~ om.toTry.get ==== thrown[WrongBranchException[_]]

    T ~ i.swapToTry.get  ==== thrown[WrongBranchException[_]]
    T ~ a.swapToTry.get  ==== "cod"
    T ~ oi.swapToTry.get ==== thrown[WrongBranchException[_]]
    T ~ oa.swapToTry.get ==== "cod"
    T ~ on.swapToTry.get ==== thrown[WrongBranchException[_]]
    T ~ om.swapToTry.get ==== null


  @Test
  def hopTest(): Unit =

    T ~ Hop.unit{ () }     ==== ()
    T ~ Hop.int{ 3 }       ==== 3
    T ~ Hop.long{ 999L }   ==== 999 --: typed[Long]
    T ~ Hop.float{ 2.1f }  ==== 2.1f
    T ~ Hop.double{ 0.6 }  ==== 0.6 
    T ~ Hop.any[Char]('e') ==== 'e'

    val fish = List("cod", "eel", "salmon", "bass")

    T ~ {
      var x = 0
      Hop.unit{
        fish.foreach{ y =>
          if y.length > 5 then ().hop
          x += y.length
        }
      }
      x
    } ==== 6

    T ~ {
      Hop.int{
        fish.foldLeft(0){ (acc, y) =>
          if y.length > 5 then acc.hop
          acc + y.length
        }
      }
    } ==== 6

    T ~ {
      Hop.long{
        fish.foldLeft(0L){ (acc, y) =>
          if y.length > 5 then acc.hop
          (acc + 1) << y.length
        }
      }
    } ==== 72 --: typed[Long]

    T ~ {
      Hop.float{
        fish.foldLeft(0f){ (acc, y) =>
          if y.length > 5 then acc.hop
          acc + 1f/y.length
        }
      }
    } =~~= 0.66666667f

    T ~ {
      Hop.double{
        fish.foldLeft(0.0){ (acc, y) =>
          if y.length > 5 then acc.hop
          acc + 1.0/y.length
        }
      }
    } =~~= 0.66666666666666667

    T ~ {
      Hop.any[String]{
        fish.foldLeft("fish:"){ (acc, y) =>
          if y.length > 5 then acc.hop
          acc + " " + y
        }
      }
    } ==== "fish: cod eel"

    T("Hop by default to inner context") ~ {
      Hop.int{
        fish.foldLeft(0){ (acc, y) =>
          acc + Hop.int{ y.foldLeft(0)( (bcc, z) => if z > 'g' then bcc.hop else bcc + z ) }
        }
      }
    } ==== "ceeba".sum.toInt

    T("Hop to outer context") ~ {
      Hop.int{ outer ?=>
        fish.foldLeft(0){ (acc, y) =>
          acc + Hop.int{ y.foldLeft(0)( (bcc, z) => if z > 'g' then bcc.hop(using outer) else bcc + z ) }
        }
      }
    } ==== 'c'.toInt

    T("Correct type-based dispatch of hops") ~ {
      var x = 0
      val s = Hop.any[String]{
        fish.foldLeft(""){ (acc, y) =>
          val c = Hop.any[Char]{ 
            y.foldLeft(0)( (bcc, z) => if z > 'g' then z.hop else if z <= 'a' then s"$bcc $z".hop else bcc + z ).toChar
          }
          x += c
          acc + c.toString
        }
      }
      (x, s)
    } ==== ("ols".sum.toInt , "98 a")

    T("Mapped hops") ~ {
      Hop.any[String]{
        fish.fold(""){ (acc, y) =>
          if y.length < acc.length then s"Error: got shorter from $acc to $y"
          else
            Hop.map((c: Char) => s"Error: bad char '$c'"){ y.foreach(c => if c == 'm' || c == 'n' then c.hop); ' ' }
            if y.length > acc.length || y > acc then y else acc
        }
      }
    } ==== "Error: bad char 'm'"

    T("Or hop") ~ {
      Hop.alt[String]{
        fish.foldLeft(0){ (acc, y) =>
          if y.length > 5 then y.hop
          else acc max y.length
        }
      }
    } ==== Alt("salmon") --: typed[Int Or String]

    T("Or did not hop") ~ {
      Hop.alt[String]{
        fish.foldLeft(0){ (acc, y) =>
          if y.length > 8 then y.hop
          else acc max y.length
        }
      }
    } ==== Is(6)


  @Test
  def valueTest(): Unit =
    ()
    /*
    T ~ Ok[Int]()("salmon") ==== Yes("salmon")
    T ~ Ok[Int]()("salmon") ==== typed[Ok[Int, String]]
    T ~ Ok(7)[String]()     ==== No(7)
    T ~ Ok(7)[String]()     ==== typed[Ok[Int, String]]

    T ~ Yes(0) ==== Yes(0)
    T ~ Yes(0) =!!= No(0)
    T ~ No(0)  =!!= Yes(0)
    T ~ No(0)  ==== No(0)

    T ~ Yes(0).isYes ==== true
    T ~ Yes(0).isNo  ==== false
    T ~ No(0).isYes  ==== false
    T ~ No(0).isNo   ==== true

    T ~ Yes("salmon").yes ==== "salmon"
    T ~ Yes("salmon").no  ==== thrown[NoSuchElementException]
    T ~ No("herring").yes ==== thrown[NoSuchElementException]
    T ~ No("herring").no  ==== "herring"

    T ~ Yes("salmon").value ==== "salmon"
    T ~ No("herring").value ==== "herring"

    T ~ Yes("salmon").typeNo[Int]    .fold(_.toString, identity) ==== "salmon"
    T ~ No(47)       .typeYes[String].fold(identity, _.length)   ==== 47
    T ~ Yes("salmon").yesOr(_ => "minnow") ==== "salmon"
    T ~ No("herring").yesOr(_ => "minnow") ==== "minnow"
    T ~ Yes("salmon").noOr( _ => "minnow") ==== "minnow"
    T ~ No("herring").noOr( _ => "minnow") ==== "herring"
    */

  @Test
  def mapTest(): Unit =
    ()
    /*
    T ~ Ok[String]()("salmon").map(_.length)    ==== Yes(6)
    T ~ Ok("herring")[String]().map(_.length)   ==== No("herring")
    T ~ Ok[String]()("salmon").mapNo(_.length)  ==== Yes("salmon")
    T ~ Ok("herring")[String]().mapNo(_.length) ==== No(7)
    */
}
object FlowTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
