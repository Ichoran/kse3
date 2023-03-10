// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-23 Rex Kerr and Calico Life Sciences, LLC.

package kse.flow.test


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable
import scala.util.boundary
import scala.util.boundary.break

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
    def wrce(t: Throwable) = ErrType.CatchableException("", t)
    T ~ basic.hasAnyStackTrace                             ==== true 
    T ~ caused.hasAnyStackTrace                            ==== true
    T ~ stackless.hasAnyStackTrace                         ==== false
    T ~ wrce(basic).hasAnyStackTrace                       ==== true
    T ~ wrce(stackless).hasAnyStackTrace                   ==== false
    T ~ wrce(wrce(wrce(wrce(basic)))).hasAnyStackTrace     ==== true
    T ~ wrce(wrce(wrce(wrce(stackless)))).hasAnyStackTrace ==== false

    val short = caused.explainAsArray(lines = 10)
    val full  = caused.explainAsArray()
    T ~ short.take(9) =**= full.take(9)
    val lines = full.drop(9).count(s => !s.startsWith("| "))
    T ~ short.last ==== s". . . (+$lines lines and 1 more exception)"

    def copeT[E](t: Throwable)(using c: Cope[E]): E = c fromThrowable t
    val e = new Exception("salmon")

    {
      given Cope[Err] = Cope.asErr
      val cT = copeT(e)
      T ~ cT.toString.contains("salmon") ==== true
      T ~ cT                             ==== typed[Err]
      T ~ cT                             ==== ErrType.ThrowableErr(e)
    }

    {
      given Cope[Array[String]] = Cope.fullTrace
      val fT = copeT(new Exception("salmon"))
      T ~ fT.count(_ contains "salmon") ==== 1
      T ~ fT                            ==== typed[Array[String]]
    }

    {
      given Cope[Throwable] = Cope.asThrowable
      val eT = copeT(new Exception("salmon"))
      T ~ eT.getMessage ==== "salmon"
      T ~ eT            ==== typed[Throwable]
    }

    // Make sure side-effecting code is called exactly once per comparison
    // (Test of test machinery)
    var sideA = 0
    var sideB = 0
    T ~ { sideA += 1; sideA } ==== { sideB += 1; sideB }
    T ~ { sideA += 1; sideA } ==== { sideB += 1; sideB }
    T ~ sideA ==== 2
    T ~ sideB ==== 2

    val herr = Err("herring")
    T ~ Err("herring")                       ==== "herring"               --: typed[Err]
    T ~ Err("herring")                       ==== herr
    T ~ Err(e)                               ==== ErrType.ThrowableErr(e) --: typed[Err]
    T ~ Err(ErrType.Explained("fish", herr)) ==== herr.explainBy("fish")  --: typed[Err]
    T ~ herr.explainBy("fish")               ==== typed[Err]
    T ~ herr.explainWith(_.toString.take(4)) ==== herr.explainBy("herr")  --: typed[Err]
    T ~ herr.toThrowable                     ==== runtype[Throwable]
    T ~ herr.toss                            ==== thrown[ErrType.StringErrException]
    T ~ "herring".errIfNot(_.length < 5)     ==== Err.or("herring")       --: typed[String Or Err]
    T ~ "herring".errIf(_.length < 5)        ==== "herring"               --: typed[String Or Err]
    T ~ "herring".errIfNot(_.length > 5)     ==== "herring"
    T ~ "herring".errIf(_.length > 5)        ==== Err.or("herring")
    T ~ 5.errCase{case x if x<6 => Err("x")} ==== Err.or("x")             --: typed[Int Or Err]
    T ~ Err.nice{ 5.toInt }                  ==== 5                       --: typed[Int Or Err]
    T ~ Err.nice{"hi".toInt}.alt.toThrowable ==== runtype[NumberFormatException]
    T ~ Err.flatNice{ "e".errIf(_.isEmpty) } ==== "e"                     --: typed[String Or Err]
    T ~ Err.flatNice{"e".errIf(_(0) == 'e')} ==== Err.or("e")
    T ~ Err.flatNice{
         "e".toInt.errCase:
           case x if x < 0 => Err("x")
       }.alt.toThrowable                     ==== runtype[NumberFormatException]
   

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
    T("double aFor") ~ sumad =~~= 0.95

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
      fish ++= s*(i + 1)
    }
    T("iFor") ~ fish.result ==== "cod, bassbass, perchperchperch, salmonsalmonsalmonsalmon"

    fish.clear
    iFor(xs.stepper){ (s, i) =>
      if fish.nonEmpty then fish ++= ", "
      fish ++= s*(4-i)
    }
    T("stepper iFor") ~ fish.result ==== "codcodcodcod, bassbassbass, perchperch, salmon"

    val ys = Array("pigeon", "sparrow", "hawk")
    val birds = new StringBuilder
    iFor(java.util.Arrays.stream(ys).iterator){ (s, i) =>
      if birds.nonEmpty then birds ++= ", "
      birds ++= s*(i+1)
    }
    T("java iFor") ~ birds.result ==== "pigeon, sparrowsparrow, hawkhawkhawk"

    val e = new java.util.Enumeration[String](){
      var i = 0
      def hasMoreElements = i < ys.length
      def nextElement = { i += 1; ys(i-1) }
    } 
    birds.clear
    iFor(e){ (s, i) =>
      birds ++= s.take(2*(i+1))
    }
    T("java enumeration iFor") ~ birds.result ==== "pisparhawk"

    birds.clear
    iFor(java.util.Arrays.stream(ys).spliterator){ (s, i) =>
      if birds.nonEmpty then birds ++= ", "
      birds ++= s*(3-i)
    }
    T("java spliterator iFor") ~ birds.result ==== "pigeonpigeonpigeon, sparrowsparrow, hawk"



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

    T ~ aoc.withIs[Int] ==== aoc --: typed[Int Or Option[String]]

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

    T ~ ioc.withAlt[Int] ==== ioc --: typed[Option[String] Or Int]

    T ~ Or.from(Right[Int, String]("herring")) ==== Is("herring")
    T ~ Or.from(Left[Int, String](5))          ==== Alt(5)
    T ~ Or.swapFrom(Right[Int, String]("cod")) ==== Alt("cod")
    T ~ Or.swapFrom(Left[Int, String](9))      ==== Is(9)

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

    val dis = ioc.withAlt[Int]
    val fav = aoc.withIs[String]
    T ~ dis                                  ==== ioc
    T ~ fav                                  ==== aoc
    T ~ Option("cod").orAlt[Int]             ==== dis
    T ~ Option("cod").orIs[String]           ==== fav

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
    T ~ 5.isLike(true.orIs[Int])                           ==== 5            --: typed[Int Or Boolean]
    T ~ 5.altLike('e'.orAlt[Int])                          ==== Alt(5)       --: typed[Char Or Int]

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
    val oi  = i.withAlt[String] // *.
    val oa  = a.withIs[Int]     // .*
    val oii = oi.orAlt[Char]    // (*.).
    val oia = oa.orAlt[Char]    // (.*).
    val oai = oi.orIs[Char]     // .(*.)
    val oaa = oa.orIs[Char]     // .(.*)
    val oiii = oii.orAlt[Long]  // ((*.).).
    val oiia = oia.orAlt[Long]  // ((.*).).
    val oiai = oai.orAlt[Long]  // (.(*.)).
    val oiaa = oaa.orAlt[Long]  // (.(.*)).
    val oaii = oii.orIs[Long]   // .((*.).)
    val oaia = oia.orIs[Long]   // .((.*).)
    val oaai = oai.orIs[Long]   // .(.(*.))
    val oaaa = oaa.orIs[Long]   // .(.(.*))
    val n = Is(null)
    val m = Alt(null)
    val on = null.orAlt[Int]
    val om = null.orIs[Int]
    val oin = on.orAlt[Char]
    val oim = om.orAlt[Char]
    val oan = on.orIs[Char]
    val oam = om.orIs[Char]
    val p = Is(null: String)
    val q = Alt(null: String)
    val op = (null: String).orAlt[Int]
    val oq = (null: String).orIs[Int]
    val oip = op.orAlt[Char]
    val oiq = oq.orAlt[Char]
    val oap = op.orIs[Char]
    val oaq = oq.orIs[Char]
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

    T ~ i.union      ==== 5
    T ~ a.union      ==== "cod"
    T ~ oi.union     ==== 5      --: typed[Int | String]
    T ~ oa.union     ==== "cod"  --: typed[Int | String]
    T ~ oii.union    ==== Is(5)
    T ~ oia.union    ==== Alt("cod")
    T ~ oai.union    ==== Is(5)
    T ~ oaa.union    ==== Alt("cod")
    T ~ n.union      ==== null
    T ~ m.union      ==== null
    T ~ on.union     ==== null
    T ~ om.union     ==== null
    T ~ oin.union    ==== null
    T ~ oim.union    ==== Alt(null)
    T ~ oan.union    ==== null
    T ~ oam.union    ==== Alt(null)
    T ~ p.union      ==== null
    T ~ q.union      ==== null
    T ~ op.union     ==== null
    T ~ oq.union     ==== null
    T ~ oip.union    ==== null
    T ~ oiq.union    ==== Alt(null)
    T ~ oap.union    ==== null
    T ~ oaq.union    ==== Alt(null)

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

    T ~ i.isIs   ==== true
    T ~ a.isIs   ==== false
    T ~ oi.isIs  ==== true
    T ~ oa.isIs  ==== false
    T ~ n.isIs   ==== true
    T ~ m.isIs   ==== false
    T ~ on.isIs  ==== true
    T ~ om.isIs  ==== false
    T ~ i.isAlt  ==== false
    T ~ a.isAlt  ==== true
    T ~ oi.isAlt ==== false
    T ~ oa.isAlt ==== true
    T ~ n.isAlt  ==== false
    T ~ m.isAlt  ==== true
    T ~ on.isAlt ==== false
    T ~ om.isAlt ==== true

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
    T ~ oia.getOrElse(_.toString.orIs[Int])    ==== Alt("cod")
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
    T ~ oai.altOrElse(_.toString.orIs[Int])    ==== Is(5)
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

    T ~ i.flatMap(_.isIf(_ > 0))           ==== 5               --: typed[Int Or Int]
    T ~ a.flatMap(_ => 0.orAlt[String])    ==== Alt("cod")      --: typed[Int Or String]
    T ~ oi.flatMap(_.orAlt[String])        ==== 5               --: typed[Int Or String]
    T ~ oa.flatMap(_.orAlt[String])        ==== Alt("cod")      --: typed[Int Or String]
    T ~ oii.flatMap(_.mapAlt(_.head))      ==== 5               --: typed[Int Or Char]
    T ~ oia.flatMap(_.mapAlt(_.head))      ==== Alt('c')        --: typed[Int Or Char]
    T ~ oai.flatMap(c => Is(c.toInt))      ==== Alt(5)          --: typed[Int Or (Int Or String)]
    T ~ oaa.flatMap(c => Is(c.toInt))      ==== Alt(Alt("cod")) --: typed[Int Or (Int Or String)]
    T ~ n.flatMap(_.isIf(_ ne null))       ==== Alt(null)       --: typed[Null Or Null]
    T ~ m.flatMap(_ => 0.orAlt[Null])      ==== Alt(null)       --: typed[Int Or Null]
    T ~ on.flatMap(_.orAlt[Int])           ==== null            --: typed[Null Or Int]
    T ~ om.flatMap(_.orAlt[String])        ==== Alt(null)       --: typed[Int Or String]
    T ~ oin.flatMap(_.isBoxed.orAlt[Char]) ==== false           --: typed[Boolean Or Char]
    T ~ oim.flatMap(_.isBoxed.orAlt[Char]) ==== true            --: typed[Boolean Or Char]
    T ~ oan.flatMap(c => Is(c.toInt))      ==== Alt(null)       --: typed[Int Or (Null Or Int)]
    T ~ oam.flatMap(c => Is(c.toInt))      ==== Alt(Alt(null))  --: typed[Int Or (Int Or Null)]
    T ~ p.flatMap(_.isIf(_ ne null))       ==== Alt(null)       --: typed[String Or String]
    T ~ q.flatMap(_ => 0.orAlt[String])    ==== Alt(null)       --: typed[Int Or String]
    T ~ op.flatMap(_.orAlt[Int])           ==== null            --: typed[String Or Int]
    T ~ oq.flatMap(_.orAlt[String])        ==== Alt(null)       --: typed[Int Or String]
    T ~ oip.flatMap(_.isBoxed.orAlt[Char]) ==== false           --: typed[Boolean Or Char]
    T ~ oiq.flatMap(_.isBoxed.orAlt[Char]) ==== true            --: typed[Boolean Or Char]
    T ~ oap.flatMap(c => Is(c.toInt))      ==== Alt(null)       --: typed[Int Or (String Or Int)]
    T ~ oaq.flatMap(c => Is(c.toInt))      ==== Alt(Alt(null))  --: typed[Int Or (Int Or String)]

    T ~ i.flatMapAlt(_ => 0.orIs[Int])             ==== 5              --: typed[Int Or Int]
    T ~ a.flatMapAlt(_.isIf(_.nonEmpty))           ==== "cod"          --: typed[String Or String]
    T ~ oi.flatMapAlt(_.orIs[Int])                 ==== 5              --: typed[Int Or String]
    T ~ oa.flatMapAlt(_.orIs[Int])                 ==== Alt("cod")     --: typed[Int Or String]
    T ~ oii.flatMapAlt(c => Alt(c.toInt))          ==== 5              --: typed[(Int Or String) Or Int]
    T ~ oia.flatMapAlt(c => Alt(c.toInt))          ==== Is(Alt("cod")) --: typed[(Int Or String) Or Int]
    T ~ oai.flatMapAlt(_.map(i => ('a'+i).toChar)) ==== 'f'            --: typed[Char Or String]
    T ~ oaa.flatMapAlt(_.map(i => ('a'+i).toChar)) ==== Alt("cod")     --: typed[Char Or String]
    T ~ n.flatMapAlt(_ => 0.orIs[Null])            ==== null           --: typed[Null Or Int]
    T ~ m.flatMapAlt(_.isIf(_ eq null))            ==== null           --: typed[Null Or Null]
    T ~ on.flatMapAlt(_.orIs[Null])                ==== null           --: typed[Null Or Int]
    T ~ om.flatMapAlt(x => (x eq null).orIs[Int])  ==== Alt(true)      --: typed[Int Or Boolean]
    T ~ oin.flatMapAlt(_ => Alt('e'))              ==== null           --: typed[(Null Or Int) Or Char]
    T ~ oim.flatMapAlt(_ => Alt('e'))              ==== Is(Alt(null))  --: typed[(Int Or Null) Or Char]
    T ~ oan.flatMapAlt(_.isBoxed.orIs[Char])       ==== Alt(false)     --: typed[Char Or Boolean]
    T ~ oam.flatMapAlt(_.isBoxed.orIs[Char])       ==== Alt(true)      --: typed[Char Or Boolean]
    T ~ p.flatMapAlt(_ => 0.orIs[String])          ==== null           --: typed[String Or Int]
    T ~ q.flatMapAlt(_.isIf(_ eq null))            ==== null           --: typed[String Or String]
    T ~ op.flatMapAlt(_.orIs[String])              ==== null           --: typed[String Or Int]
    T ~ oq.flatMapAlt(x => (x eq null).orIs[Int])  ==== Alt(true)      --: typed[Int Or Boolean]
    T ~ oip.flatMapAlt(_ => Alt('e'))              ==== null           --: typed[(String Or Int) Or Char]
    T ~ oiq.flatMapAlt(_ => Alt('e'))              ==== Is(Alt(null))  --: typed[(Int Or String) Or Char]
    T ~ oap.flatMapAlt(_.isBoxed.orIs[Char])       ==== Alt(false)     --: typed[Char Or Boolean]
    T ~ oaq.flatMapAlt(_.isBoxed.orIs[Char])       ==== Alt(true)      --: typed[Char Or Boolean]

    T ~ i.flatMapThem(_.isIf(_ > 0))(_ => 0.orAlt[Int])                                ==== 5         --: typed[Int Or Int]
    T ~ a.flatMapThem(_ => "eel".orAlt[String])(_.isIf(_.nonEmpty))                    ==== "cod"     --: typed[String Or String]
    T ~ oi.flatMapThem(_.isIf(_ > 0))(s => 0.orAlt[Int])                               ==== 5         --: typed[Int Or Int]
    T ~ oa.flatMapThem(_ => "eel".orAlt[String])(_.isIf(_.nonEmpty))                   ==== "cod"     --: typed[String Or String]
    T ~ oii.flatMapThem(_.swap)(c => if c < ' ' then Is(c.toString) else Alt(c.toInt)) ==== Alt(5)    --: typed[String Or Int]
    T ~ oia.flatMapThem(_.swap)(c => if c < ' ' then Is(c.toString) else Alt(c.toInt)) ==== "cod"     --: typed[String Or Int]
    T ~ oai.flatMapThem(c => if c < ' ' then Is(c.toString) else Alt(c.toInt))(_.swap) ==== Alt(5)    --: typed[String Or Int]
    T ~ oaa.flatMapThem(c => if c < ' ' then Is(c.toString) else Alt(c.toInt))(_.swap) ==== "cod"     --: typed[String Or Int]
    T ~ n.flatMapThem(_.isIf(_ eq null))(_ => null.orIs[Null])                         ==== null      --: typed[Null Or Null]
    T ~ m.flatMapThem(_ => null.orIs[Null])(_.altIf(_ eq null))                        ==== Alt(null) --: typed[Null Or Null]
    T ~ on.flatMapThem(_.isIf(_ eq null))(i => null.isIf(_ => i >= 0))                 ==== null      --: typed[Null Or Null]
    T ~ om.flatMapThem(i => null.isIf(_ => i >= 0))(_.isIf(_ ne null))                 ==== Alt(null) --: typed[Null Or Null]
    T ~ oin.flatMapThem(_.swap)(c => if c < ' ' then Alt(null) else Is(c.toInt))       ==== Alt(null) --: typed[Int Or Null]
    T ~ oim.flatMapThem(_.swap)(c => if c < ' ' then Alt(c.toInt) else Is(null))       ==== null      --: typed[Null Or Int]
    T ~ oan.flatMapThem(c => if c < ' ' then Alt(null) else Is(c.toInt))(_.swap)       ==== Alt(null) --: typed[Int Or Null]
    T ~ oam.flatMapThem(c => if c < ' ' then Alt(c.toInt) else Is(null))(_.swap)       ==== null      --: typed[Null Or Int]
    T ~ p.flatMapThem(_.isIf(_ eq null))(_ => "".orIs[String])                         ==== null      --: typed[String Or String]
    T ~ q.flatMapThem(_ => "".orIs[String])(_.altIf(_ eq null))                        ==== Alt(null) --: typed[String Or String]
    T ~ op.flatMapThem(_.isIf(_ eq null))(i => "".isIf(_ => i >= 0))                   ==== null      --: typed[String Or String]
    T ~ oq.flatMapThem(i => "".isIf(_ => i >= 0))(_.isIf(_ ne null))                   ==== Alt(null) --: typed[String Or String]
    T ~ oip.flatMapThem(_.swap)(c => if c < ' ' then Alt(null) else Is(c.toInt))       ==== Alt(null) --: typed[Int Or String]
    T ~ oiq.flatMapThem(_.swap)(c => if c < ' ' then Alt(c.toInt) else Is(null))       ==== null      --: typed[String Or Int]
    T ~ oap.flatMapThem(c => if c < ' ' then Alt(null) else Is(c.toInt))(_.swap)       ==== Alt(null) --: typed[Int Or String]
    T ~ oaq.flatMapThem(c => if c < ' ' then Alt(c.toInt) else Is(null))(_.swap)       ==== null      --: typed[String Or Int]

    var x = 0
    T ~ (oi || 9.orAlt[String])          ==== 5          --: typed[Int Or String]
    T ~ (oa || 9.orAlt[String])          ==== 9          --: typed[Int Or String]
    T ~ (oi || "eel".orIs[Int])          ==== 5          --: typed[Int Or String]
    T ~ (oa || "eel".orIs[Int])          ==== Alt("eel") --: typed[Int Or String]
    T ~ (oi || { x += 1; 9.orAlt[Int] }) ==== 5          --: typed[Int Or (String | Int)]
    T ~ x                                ==== 0
    T ~ (oa || { x += 1; 9.orAlt[Int] }) ==== 9
    T ~ x                                ==== 1
    T ~ (oi && 9.orAlt[String])          ==== (5, 9)     --: typed[(Int, Int) Or String]
    T ~ (oa && oi)                       ==== oa
    T ~ (oi && oa)                       ==== oa
    T ~ (oi && { x += 1; 9.orAlt[Int]})  ==== (5, 9)
    T ~ x                                ==== 2
    T ~ (oa && { x += 1; 9.orAlt[Int]})  ==== oa
    T ~ x                                ==== 2
    val ui = ().orAlt[String]
    val ua = "eel".orIs[Unit]
    T ~ (oi && oi)                       ==== (5, 5)     --: typed[(Int, Int) Or String]
    T ~ (oi && oa)                       ==== Alt("cod") --: typed[(Int, Int) Or String]
    T ~ (oa && oi)                       ==== Alt("cod") --: typed[(Int, Int) Or String]
    T ~ (oa && oa)                       ==== Alt("cod") --: typed[(Int, Int) Or String]
    T ~ (ui && oi)                       ==== 5          --: typed[Int Or String]
    T ~ (oi && ui)                       ==== 5          --: typed[Int Or String]
    T ~ (ui && ui)                       ==== ()         --: typed[Unit Or String]
    T ~ (oi && ua)                       ==== Alt("eel") --: typed[Int Or String]
    T ~ (ua && oi)                       ==== Alt("eel") --: typed[Int Or String]
    T ~ (oa && ui)                       ==== Alt("cod") --: typed[Int Or String]
    T ~ (ui && oa)                       ==== Alt("cod") --: typed[Int Or String]
    T ~ (oa && ua)                       ==== Alt("cod") --: typed[Int Or String]
    T ~ (ua && oa)                       ==== Alt("eel") --: typed[Int Or String]
    T ~ (ua && ua)                       ==== Alt("eel") --: typed[Unit Or String]

    T ~ i.discard{ case x if x > 0 => "!"*x }                  ==== Alt("!!!!!")    --: typed[Int Or String]
    T ~ i.discard{ case x if x < 0 => "@"*(-x) }               ==== 5               --: typed[Int Or String]
    T ~ a.discard{ case x => "salmon" }                        ==== Alt("cod")
    T ~ oi.discard{ case x if x > 0 => "!"*x }                 ==== Alt("!!!!!")    --: typed[Int Or String]
    T ~ oi.discard{ case x if x < 0 => "@"*(-x) }              ==== 5               --: typed[Int Or String]
    T ~ oa.discard{ case x if x > 0 => "!"*x }                 ==== Alt("cod")      --: typed[Int Or String]
    T ~ oii.discard{ case Alt(y) if y.nonEmpty => y.head }     ==== 5               --: typed[(Int Or String) Or Char]
    T ~ oia.discard{ case Alt(y) if y.nonEmpty => y.head }     ==== Alt('c')        --: typed[(Int Or String) Or Char]
    T ~ oai.discard{ case c if c<' ' => c.toInt.orAlt[String] }==== Alt(5)          --: typed[Char Or (Int Or String)]
    T ~ oaa.discard{ case c if c<' ' => c.toInt.orAlt[String] }==== Alt(Alt("cod")) --: typed[Char Or (Int Or String)]
    T ~ n.discard{ case x if x eq null => 4 }                  ==== Alt(4)          --: typed[Null Or Int]
    T ~ n.discard{ case x if x ne null => 4 }                  ==== null            --: typed[Null Or Int]
    T ~ m.discard{ case x => "salmon" }                        ==== Alt(null)
    T ~ on.discard{ case x if x eq null => 4 }                 ==== Alt(4)          --: typed[Null Or Int]
    T ~ on.discard{ case x if x ne null => 4 }                 ==== null            --: typed[Null Or Int]
    T ~ om.discard{ case x if x > 0 => null }                  ==== Alt(null)       --: typed[Int Or Null]
    T ~ oin.discard{ case Alt(y) if y == 0 => '0' }            ==== null            --: typed[(Null Or Int) Or Char]
    T ~ oim.discard{ case Alt(y) if y eq null => '0' }         ==== Alt('0')        --: typed[(Int Or Null) Or Char]
    T ~ oan.discard{ case c if c<' ' => c.toInt.orIs[Null] }   ==== Alt(null)       --: typed[Char Or (Null Or Int)]
    T ~ oam.discard{ case c if c<' ' => c.toInt.orAlt[Null] }  ==== Alt(Alt(null))  --: typed[Char Or (Int Or Null)]
    T ~ p.discard{ case x if x eq null => 4 }                  ==== Alt(4)          --: typed[String Or Int]
    T ~ p.discard{ case x if x ne null => 4 }                  ==== null            --: typed[String Or Int]
    T ~ q.discard{ case x => "salmon" }                        ==== Alt(null)
    T ~ op.discard{ case x if x eq null => 4 }                 ==== Alt(4)          --: typed[String Or Int]
    T ~ op.discard{ case x if x ne null => 4 }                 ==== null            --: typed[String Or Int]
    T ~ oq.discard{ case x if x > 0 => "bass" }                ==== Alt(null)       --: typed[Int Or String]
    T ~ oip.discard{ case Alt(y) if y == 0 => '0' }            ==== null            --: typed[(String Or Int) Or Char]
    T ~ oiq.discard{ case Alt(y) if nlen(y) <= 0 => '0' }      ==== Alt('0')        --: typed[(Int Or String) Or Char]
    T ~ oan.discard{ case ' ' => 4.orIs[String] }              ==== Alt(null)       --: typed[Char Or (String Or Int)]
    T ~ oaq.discard{ case ' ' => 4.orAlt[String] }             ==== Alt(Alt(null))  --: typed[Char Or (Int Or String)]

    T ~ i.reclaim{ case x => 4 }                                 ==== 5
    T ~ a.reclaim{ case s if s.nonEmpty => s.length }            ==== 3              --: typed[Int Or String]
    T ~ a.reclaim{ case s if s.isEmpty => 4 }                    ==== Alt("cod")     --: typed[Int Or String]
    T ~ oi.reclaim{ case s if s.isEmpty => 4 }                   ==== 5              --: typed[Int Or String]
    T ~ oa.reclaim{ case s if s.nonEmpty => s.length }           ==== 3              --: typed[Int Or String]
    T ~ oa.reclaim{ case s if s.isEmpty => 4 }                   ==== Alt("cod")     --: typed[Int Or String]
    T ~ oii.reclaim{ case c if c<' ' => c.toInt.orAlt[String] }  ==== 5              --: typed[(Int Or String) Or Char]
    T ~ oia.reclaim{ case c if c<' ' => c.toInt.orAlt[String] }  ==== Is(Alt("cod")) --: typed[(Int Or String) Or Char]
    T ~ oai.reclaim{ case Alt(y) if y.nonEmpty => y.head }       ==== Alt(5)         --: typed[Char Or (Int Or String)]
    T ~ oaa.reclaim{ case Alt(y) if y.nonEmpty => y.head }       ==== 'c'            --: typed[Char Or (Int Or String)]
    T ~ n.reclaim{ case x => null }                              ==== null
    T ~ m.reclaim{ case x if x eq null => 4 }                    ==== 4              --: typed[Int Or Null]
    T ~ m.reclaim{ case x if x ne null => 4 }                    ==== Alt(null)      --: typed[Int Or Null]
    T ~ on.reclaim{ case x if x < 0 => null }                    ==== null           --: typed[Null Or Int]
    T ~ om.reclaim{ case x if x eq null => 4 }                   ==== 4              --: typed[Int Or Null]
    T ~ om.reclaim{ case x if x ne null => 4 }                   ==== Alt(null)      --: typed[Int Or Null]
    T ~ oin.reclaim{ case c if c < ' ' => c.toInt.orIs[Null] }   ==== null           --: typed[(Null Or Int) Or Char]
    T ~ oim.reclaim{ case c if c < ' ' => c.toInt.orAlt[Null] }  ==== Is(Alt(null))  --: typed[(Int Or Null) Or Char]
    T ~ oan.reclaim{ case Alt(y) if y == 0 => '0' }              ==== Alt(null)      --: typed[Char Or (Null Or Int)]
    T ~ oam.reclaim{ case Alt(y) if y eq null => '0' }           ==== '0'            --: typed[Char Or (Int Or Null)]
    T ~ p.reclaim{ case x => null }                              ==== null
    T ~ q.reclaim{ case x if x eq null => 4 }                    ==== 4              --: typed[Int Or String]
    T ~ q.reclaim{ case x if x ne null => 4 }                    ==== Alt(null)      --: typed[Int Or String]
    T ~ op.reclaim{ case x if x < 0 => "eel" }                   ==== null           --: typed[String Or Int]
    T ~ oq.reclaim{ case x if x eq null => 4 }                   ==== 4              --: typed[Int Or String]
    T ~ oq.reclaim{ case x if x == "bass" => 4 }                 ==== Alt(null)      --: typed[Int Or String]
    T ~ oip.reclaim{ case c if c<' ' => c.toInt.orIs[String] }   ==== null           --: typed[(String Or Int) Or Char]
    T ~ oiq.reclaim{ case c if c<' ' => c.toInt.orAlt[String] }  ==== Is(Alt(null))  --: typed[(Int Or String) Or Char]
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

    val isc = 'e'.orIs[Int Or String]
    val nic = 'e'.orIs[Null Or Int]
    val sic = 'e'.orIs[String Or Int]
    T ~ oii.pivot ==== 5             --: typed[Int Or (String Or Char)]
    T ~ oia.pivot ==== Alt("cod")    --: typed[Int Or (String Or Char)]
    T ~ isc.pivot ==== Alt(Alt('e')) --: typed[Int Or (String Or Char)]
    T ~ oin.pivot ==== null          --: typed[Null Or (Int Or Char)]
    T ~ oim.pivot ==== Alt(null)     --: typed[Int Or (Null Or Char)]
    T ~ nic.pivot ==== Alt(Alt('e')) --: typed[Null Or (Int Or Char)]
    T ~ oip.pivot ==== null          --: typed[String Or (Int Or Char)]
    T ~ oiq.pivot ==== Alt(null)     --: typed[Int Or (String Or Char)]
    T ~ sic.pivot ==== Alt(Alt('e')) --: typed[String Or (Int Or Char)]

    val cis = 'e'.orAlt[Int Or String]
    val cni = 'e'.orAlt[Null Or Int]
    val csi = 'e'.orAlt[String Or Int]
    T ~ oai.unpivot ==== Is(Alt(5))    --: typed[(Char Or Int) Or String]
    T ~ oaa.unpivot ==== Alt("cod")    --: typed[(Char Or Int) Or String]
    T ~ cis.unpivot ==== 'e'           --: typed[(Char Or Int) Or String]
    T ~ oan.unpivot ==== Is(Alt(null)) --: typed[(Char Or Null) Or Int]
    T ~ oam.unpivot ==== Alt(null)     --: typed[(Char Or Int) Or Null]
    T ~ cni.unpivot ==== 'e'           --: typed[(Char Or Null) Or Int]
    T ~ oap.unpivot ==== Is(Alt(null)) --: typed[(Char Or String) Or Int]
    T ~ oaq.unpivot ==== Alt(null)     --: typed[(Char Or Int) Or String]
    T ~ csi.unpivot ==== 'e'           --: typed[(Char Or String) Or Int]

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
  def flowTest(): Unit =
    def orQ1(s: String): Int Or String = Or.Ret{
      s.isIf(_.forall(_.isDigit)).?.toInt
    }
    T ~ orQ1("salmon") ==== Alt("salmon") --: typed[Int Or String]
    T ~ orQ1("5")      ==== 5             --: typed[Int Or String]

    def orQ2(s: String): Int Or String = Or.FlatRet{
      s.isIf(_.forall(_.isDigit)).?.isIf(s => s.length >= 1 && s.length <= 9).map(_.toInt)
    }
    T ~ orQ2("1234567890") ==== Alt("1234567890") --: typed[Int Or String]
    T ~ orQ2("4")          ==== 4                 --: typed[Int Or String]
    T ~ orQ2("cod")        ==== Alt("cod")        --: typed[Int Or String]

    def orQ3(s: String): Int Or String = Or.Ret{
      s.altCase{ case x if x.exists(! _.isDigit) => x.exists(_.isDigit) }.?+(b => s"Has digits: $b").toInt
    }
    T ~ orQ3("herring") ==== Alt("Has digits: false") --: typed[Int Or String]
    T ~ orQ3("5 eels")  ==== Alt("Has digits: true")  --: typed[Int Or String]
    T ~ orQ3("14")      ==== 14                       --: typed[Int Or String]

    def orQ4(s: String): Int Or String = Or.Ret{
      given AutoMap[String Or String, String] = _.fold{ x => s"Too long: $x" }{ y => s"Non-numeric: $y" }

      s.isIf(_.forall(_.isDigit)).alsoDiscard{ case s if s.length < 1 || s.length > 9 => s}.?*.toInt
    }
    T ~ orQ4("perch")      ==== Alt("Non-numeric: perch")   --: typed[Int Or String]
    T ~ orQ4("1234567890") ==== Alt("Too long: 1234567890") --: typed[Int Or String]
    T ~ orQ4("225")        ==== 225                         --: typed[Int Or String]

    def orQ5(s: String): Int Or String = Or.Safe(_.getMessage){
      s.isIf(_.nonEmpty).?.toInt
    }
    T ~ orQ5("perch").existsAlt(_ contains "perch") ==== true
    T ~ orQ5("")                                    ==== Alt("")
    T ~ orQ5("15")                                  ==== 15      --: typed[Int Or String]

    def orQ6(s: String): Int Or Err = Or.Nice {
      s.isIf(_.nonEmpty).?+(Err apply _).toInt
    }  
    T ~ orQ6("perch").existsAlt(_.toString contains "NumberF") ==== true
    T ~ orQ6("")                                               ==== Alt("")
    T ~ orQ6("1815")                                           ==== 1815    --: typed[Int Or Err]

    def orQ7(s: String): Int Or Err = Err.Or {
      s.isIf(_.nonEmpty).?.toInt
    }
    T ~ orQ7("perch").existsAlt(_.toString contains "NumberF") ==== true
    T ~ orQ7("")                                               ==== Alt(Err(""))
    T ~ orQ7("1858")                                           ==== 1858 --: typed[Int Or Err]

    def orQ8(s: String): Int Or Err = Err.FlatOr {
      s.isIf(_.nonEmpty).?.toInt.altCase{ case i if i < 0 => Err(s"Negative: $i") }
    }
    T ~ orQ8("perch").existsAlt(_.toString contains "NumberF") ==== true
    T ~ orQ8("")                                               ==== Alt(Err(""))
    T ~ orQ8("1858")                                           ==== 1858 --: typed[Int Or Err]
    T ~ orQ8("-3")                                             ==== Alt(Err("Negative: -3"))

    def floatQ(f: Float) = Ret[Float]{
      val g = f.?
      if g < 0 then -math.sqrt(-g).toFloat
      else math.sqrt(g - 1).toFloat.? + 1
    }
    T ~ floatQ(Float.NaN) =~~= Float.NaN
    T ~ floatQ(-1f)       =~~= -1f
    T ~ floatQ(0.5f)      =~~= Float.NaN
    T ~ floatQ(2f)        =~~= 2f

    def doubleQ(f: Double) = Ret[Double]{
      val g = f.?
      if g < 0 then -math.sqrt(-g)
      else math.sqrt(g - 1).? + 1
    }
    T ~ doubleQ(Double.NaN) =~~= Double.NaN
    T ~ doubleQ(-1f)        =~~= -1.0
    T ~ doubleQ(0.5f)       =~~= Double.NaN
    T ~ doubleQ(5f)         =~~= 3.0

    def eitherQ1(s: String): Either[String, Int] = Either.Ret{
      val e: Either[String, Int] =
        if s.forall(_.isDigit) then Right(s.toInt)
        else Left(s)
      e.? + 1
    }
    T ~ eitherQ1("minnow") ==== Left("minnow") --: typed[Either[String, Int]]
    T ~ eitherQ1("55")     ==== Right(56)      --: typed[Either[String, Int]]

    def eitherQ2(s: String): Either[String, Int] = Either.FlatRet{
      val e: Either[String, Int] =
        if s.forall(_.isDigit) then Right(s.toInt)
        else Left(s)
      e.? match
        case x if x < 10 => Left(s"Bad $x")
        case x           => Right(x + 2)
    }
    T ~ eitherQ2("minnow") ==== Left("minnow") --: typed[Either[String, Int]]
    T ~ eitherQ2("55")     ==== Right(57)      --: typed[Either[String, Int]]
    T ~ eitherQ2("4")      ==== Left("Bad 4")  --: typed[Either[String, Int]]

    def optionQ1(s: String): Option[Int] = Option.Ret[Int]{
      Option(s).filter(_.forall(_.isDigit)).?.toInt
    }
    T ~ optionQ1("herring") ==== None     --: typed[Option[Int]]
    T ~ optionQ1("8")       ==== Some(8)

    def optionQ2(s: String): Option[Int] = Option.FlatRet{
      Option(s).filter(_.forall(_.isDigit)).?.toInt match
        case x if x < 10 => None
        case x           => Some(x)
    }
    T ~ optionQ2("herring") ==== None     --: typed[Option[Int]]
    T ~ optionQ2("8")       ==== None     --: typed[Option[Int]]
    T ~ optionQ2("88")      ==== Some(88)

    import scala.util.control.{ControlThrowable => ControlThrow}
    def toss(): Nothing = throw new ControlThrow("tossed") {}
    T ~ safe{ "17".toInt }                                              ==== 17 --: typed[Int Or Throwable]
    T ~ safe{ "e".toInt }.foreachAlt(throw _)                           ==== thrown[NumberFormatException]
    T ~ safe{ "e".toInt }.isAlt                                         ==== true
    T ~ safeWith(_.getMessage.isEmpty){ "17".toInt }                    ==== 17 --: typed[Int Or Boolean]
    T ~ safeWith(_.getMessage.isEmpty){ "e".toInt }                     ==== Alt(false)
    T ~ nice{ "17".toInt }                                              ==== 17 --: typed[Int Or Err]
    T ~ nice{ "e".toInt }.existsAlt(_.toString contains "NumberFormat") ==== true
    T ~ threadsafe{ "17".toInt }                                        ==== 17 --: typed[Int Or Throwable]
    T ~ threadsafe{ "e".toInt }.existsAlt(_.toString contains "Number") ==== true
    T ~ threadsafe{ toss(); 0 }.existsAlt(_.isInstanceOf[ControlThrow]) ==== true
    T ~ threadnice{ "17".toInt }                                        ==== 17 --: typed[Int Or Err]
    T ~ threadnice{ "e".toInt }.existsAlt(_.toString contains "Number") ==== true
    T ~ ratchet(7){ i => i + "17".toInt }                               ==== 24
    T ~ ratchet(7){ i => i + "e".toInt }                                ==== 7
    {
      given Cope[String] = Cope.asString
      T ~ cope{ "17".toInt }                                     ==== 17 --: typed[Int Or String]
      T ~ cope{ "e".toInt }.existsAlt(_ contains "NumberFormat") ==== true
      T ~ threadcope{ "17".toInt }                               ==== 17 --: typed[Int Or String]
      T ~ threadcope{ toss(); 0 }.existsAlt(_ contains "tossed") ==== true
    }
    T ~ safe{ "17".toInt }.get                                   ==== 17
    T ~ safe{ "e".toInt }.get                                    ==== thrown[NoSuchElementException]
    T ~ safe{ "17".toInt }.grab                                  ==== 17
    T ~ safe{ "e".toInt }.grab                                   ==== thrown[NumberFormatException]
    T ~ Alt("e").grab                                            ==== thrown[ErrType.StringErrException]
    T ~ nice{ "17".toInt }.grab                                  ==== 17
    T ~ nice{ "e".toInt }.grab                                   ==== thrown[NumberFormatException]
    T ~ threadsafe{ toss(); 0 }.grab                             ==== thrown[ErrType.CatchableException]
    T ~ threadnice{ toss(); 0 }.grab                             ==== thrown[ErrType.CatchableException]
    T ~ nice{ "e".toInt }.mapAlt(_.explainBy("Foo")).grab        ==== thrown[ErrType.CatchableException]
    T ~ 17.altIf(x => x % 2 != 0).grab                           ==== thrown[WrongBranchException[_]]


    val l = Left("herring")
    val r = Right(15)
    val eL: Either[String, Int] = l
    val eR: Either[String, Int] = r
    T ~ l.toOr  ==== Alt("herring")  --: typed[Nothing Or String]
    T ~ r.toOr  ==== 15              --: typed[Int Or Nothing]
    T ~ eL.toOr ==== Alt("herring")  --: typed[Int Or String]
    T ~ eR.toOr ==== 15              --: typed[Int Or String]

    val e = new Exception("halibut")
    val ty = Try{ 5 }
    val tn = Try{ throw e; 4}
    T ~ ty.toOr                              ==== 5               --: typed[Int Or Throwable]
    T ~ tn.toOr                              ==== Alt(e)          --: typed[Int Or Throwable]
    T ~ ty.toOrWith(_.getMessage)            ==== 5               --: typed[Int Or String]
    T ~ tn.toOrWith(_.getMessage)            ==== Alt("halibut")  --: typed[Int Or String]
    T ~ ty.niceOr                            ==== 5               --: typed[Int Or Err]
    T ~ tn.niceOr.mapAlt(_.toString take 12) ==== Alt("java.lang.Ex")

    val s = Some("snapper")
    val os: Option[String] = s
    val on: Option[String] = None
    T ~ s.toOr           ==== "snapper"          --: typed[String Or Unit]
    T ~ os.toOr          ==== "snapper"          --: typed[String Or Unit]
    T ~ on.toOr          ==== Alt(())            --: typed[String Or Unit]
    T ~ s.toOrElse(5)    ==== "snapper"          --: typed[String Or Int]
    T ~ os.toOrElse(5)   ==== "snapper"          --: typed[String Or Int]
    T ~ on.toOrElse(5)   ==== Alt(5)             --: typed[String Or Int]
    T ~ os.toTry         ==== Success("snapper") --: typed[Try[String]]
    T ~ on.toTry         ==== typed[Try[String]]
    on.toTry match
      case Failure(e) => T ~ e.isInstanceOf[WrongBranchException[_]] ==== true
      case _          => T("Success when failure expected") ~ false  ==== true

    val fish = List("cod", "eel", "salmon", "bass")

    T ~ {
      var x = 0
      boundary {
        fish.foreach{ y =>
          if y.length > 5 then ().break
          x += y.length
        }
      }
      x
    } ==== 6

    T ~ {
      boundary[String] {
        fish.foldLeft("fish:"){ (acc, y) =>
          if y.length > 5 then acc.break
          acc + " " + y
        }
      }
    } ==== "fish: cod eel"

    T("Break by default to inner context") ~ {
      boundary[Int] {
        fish.foldLeft(0){ (acc, y) =>
          acc + boundary[Int] { y.foldLeft(0)( (bcc, z) => if z > 'g' then bcc.break else bcc + z ) }
        }
      }
    } ==== "ceeba".sum.toInt

    T("Break to outer context") ~ {
      boundary[Int]{ outer ?=>
        fish.foldLeft(0){ (acc, y) =>
          acc + boundary[Int]{ y.foldLeft(0)( (bcc, z) => if z > 'g' then bcc.break(using outer) else bcc + z ) }
        }
      }
    } ==== 'c'.toInt

    T("Correct type-based dispatch of breaks") ~ {
      var x = 0
      val s = boundary[String] {
        fish.foldLeft(""){ (acc, y) =>
          val c = boundary[Char] { 
            y.foldLeft(0)( (bcc, z) => if z > 'g' then z.break else if z <= 'a' then s"$bcc $z".break else bcc + z ).toChar
          }
          x += c
          acc + c.toString
        }
      }
      (x, s)
    } ==== ("ols".sum.toInt , "98 a")

    given AutoMap[Char, String] = c => s"Error: bad char '$c'"
    T("Mapped breaks") ~ {
      boundary[String] {
        fish.fold(""){ (acc, y) =>
          if y.length < acc.length then s"Error: got shorter from $acc to $y"
          else
            y.foreach(c => if c == 'm' || c == 'n' then c.autobreak);
            if y.length > acc.length || y > acc then y else acc
        }
      }
    } ==== "Error: bad char 'm'"

    T("breakWith broke") ~ {
      val s = "salmon"
      boundary[Int] {
        if !s.forall(_.isDigit) then s.breakWith(x => -x.length)
        s.toInt
      }
    } ==== -6

    T("breakWith didn't break") ~ {
      val s = "14"
      boundary[Int] {
        if !s.forall(_.isDigit) then s.breakWith(x => -x.length)
        s.toInt
      }
    } ==== 14

    T("breakIf broke") ~ {
      val s = "salmon"
      boundary[String] {
        s.breakIf(_.length > 4)
        s.toUpperCase
      }
    } ==== "salmon"

    T("breakIf didn't break") ~ {
      val s = "cod"
      boundary[String] {
        s.breakIf(_.length > 4)
        s.toUpperCase
      }
    } ==== "COD"

    T("breakIfNot broke") ~ {
      val s = "cod"
      boundary[String] {
        s.breakIfNot(_.length > 4)
        s.toUpperCase
      }
    } ==== "cod"

    T("breakIfNot didn't break") ~ {
      val s = "salmon"
      boundary[String] {
        s.breakIfNot(_.length > 4)
        s.toUpperCase
      }
    } ==== "SALMON"

    T("breakCase broke") ~ {
      val s = Option("salmon")
      boundary[String] {
        s.breakCase{ case Some(x) => x }
        ""
      }
    } ==== "salmon"

    T("breakCase didn't break") ~ {
      val s: Option[String] = None
      boundary[String] {
        s.breakCase{ case Some(x) => x }
        ""
      }
    } ==== ""

    T("breakNotCase broke") ~ {
      val s = "salmon"
      boundary[String] {
        "!" * s.breakNotCase{ case "cod" => 4 }
      }
    } ==== "salmon"

    T("breakNotCase didn't break") ~ {
      val s = "cod"
      boundary[String] {
        "!" * s.breakNotCase{ case "cod" => 4 }
      }
    } ==== "!!!!"

    T ~ boundary[Int]{ val o = 7.orIs[String];      o.getOrBreak.length } ==== 7
    T ~ boundary[Int]{ val o = "minnow".orAlt[Int]; o.getOrBreak.length } ==== 6
    T ~ boundary[Int]{ val o = 5.orAlt[String];     o.altOrBreak.length } ==== 5
    T ~ boundary[Int]{ val o = "bass".orIs[Int];    o.altOrBreak.length } ==== 4

    T ~ boundary[Char]{ val o = "hi".orIs[Char];   o.getOrBreakWith(_.head) } ==== 'h'
    T ~ boundary[Char]{ val o = 'e'.orAlt[String]; o.getOrBreakWith(_.head) } ==== 'e'
    T ~ boundary[Char]{ val o = "bye".orAlt[Char]; o.altOrBreakWith(_.head) } ==== 'b'
    T ~ boundary[Char]{ val o = 'g'.orIs[String];  o.altOrBreakWith(_.head) } ==== 'g'

    T ~ boundary[String]{ val o = "bass".orAlt[Char]; o.getOrAutoBreak }.length ==== 4
    T ~ boundary[String]{ val o = 'c'.orIs[String];   o.getOrAutoBreak }.length ==== summon[AutoMap[Char, String]](' ').length
    T ~ boundary[String]{ val o = "eel".orIs[Char];   o.altOrAutoBreak }.length ==== 3
    T ~ boundary[String]{ val o = 'c'.orAlt[String];  o.altOrAutoBreak }.length ==== summon[AutoMap[Char, String]](' ').length


    val eisL: Either[Int, String] = Left(7)
    val eisR: Either[Int, String] = Right("minnow")
    val esiR: Either[String, Int] = Right(5)
    val esiL: Either[String, Int] = Left("bass")
    T ~ boundary[Int]{ eisL.rightOrBreak.length } ==== 7
    T ~ boundary[Int]{ eisR.rightOrBreak.length } ==== 6
    T ~ boundary[Int]{ esiR.leftOrBreak.length }  ==== 5
    T ~ boundary[Int]{ esiL.leftOrBreak.length }  ==== 4

    val oiS: Option[Int] = Some(2)
    val oiN: Option[Int] = None
    T ~ { var x = 0; boundary{ x = oiS.getOrBreak }; x } ==== 2
    T ~ { var x = 0; boundary{ x = oiN.getOrBreak }; x } ==== 0


  @Test
  def mutableDataTest(): Unit =
    import java.lang.Float.{intBitsToFloat => i2f}
    import java.lang.Double.{longBitsToDouble => l2d}
    val ab = Array[Byte](1, 2, 3)
    val bb = Array[Byte](0, 1, 2, 3, 4)
    T ~ ab.copy                     =**= ab
    T ~ (ab.copy eq ab)             ==== false
    T ~ ab.copy.tap(_(0) = 4).toSeq =!!= ab.toSeq
    T ~ ab.copyToSize(2).length     ==== 2
    T ~ ab.copyToSize(2)            =**= ab.take(2)
    T ~ ab.copyToSize(4)            =**= Array[Byte](1, 2, 3, 0)
    T ~ ab.shrinkCopy(2)            =**= ab.take(2)
    T ~ (ab.shrinkCopy(4) eq ab)    ==== true
    T ~ ab.copyOfRange(1, 3)        =**= ab.drop(1)
    T ~ ab.addLeftSlots(1)          =**= (0 +: ab)
    T ~ ab.addRightSlots(1)         =**= (ab :+ 0)
    T ~ ab.copyInto(bb)             =**= Array[Byte](1, 2, 3, 3, 4)
    T ~ ab.copyInto(bb, 2)          =**= Array[Byte](1, 2, 1, 2, 3)
    T ~ ab.copyRangeInto(1,3)(bb)   =**= Array[Byte](2, 3, 1, 2, 3)
    T ~ ab.copyRangeInto(1,3)(bb,1) =**= Array[Byte](2, 2, 3, 2, 3)
    T ~ ab.py(1)                    ==== ab(1)
    T ~ ab.py(-3)                   ==== ab(0)
    T ~ bb.py.index(-2)             ==== 3
    T ~ { bb.py(-4) = 0; bb }       =**= Array[Byte](2, 0, 3, 2, 3)
    T ~ (ab ++ bb).packInts         =**= Array[Int](0x02030201, 0x03020300)
    T ~ (ab ++ bb).packFloats       =**= Array[Float](i2f(0x02030201), i2f(0x03020300))
    T ~ (ab ++ bb).packLongs        =**= Array[Long](0x0302030002030201L)
    T ~ (ab ++ bb).packDoubles      =**= Array[Double](l2d(0x0302030002030201L))
    T ~ bb.isSorted                 ==== false
    T ~ bb.isSortedRange(1, 3)      ==== true
    T ~ ab.search(2)                ==== 1
    T ~ ab.search(0)                ==== -1
    T ~ bb.searchRange(1, 3)(3)     ==== 2
    T ~ bb.searchRange(1, 3)(2)     ==== -3
    T ~ bb.sortRange(0, 3)          =**= Array[Byte](0, 2, 3, 2, 3)
    T ~ bb.sort()                   =**= Array[Byte](0, 2, 2, 3, 3)
    T ~ bb.fillRange(2, 4)(1)       =**= Array[Byte](0, 2, 1, 1, 3)
    T ~ bb.zapAll(b=>(b+1).toByte)  =**= Array[Byte](1, 3, 2, 2, 4)
    T ~ bb.fill(4)                  =**= Array[Byte](4, 4, 4, 4, 4)


    val as = Array[Short](1, 2, 3)
    val bs = Array[Short](0, 1, 2, 3, 4)
    T ~ as.copy                     =**= as
    T ~ (as.copy eq as)             ==== false
    T ~ as.copy.tap(_(0) = 4).toSeq =!!= as.toSeq
    T ~ as.copyToSize(2).length     ==== 2
    T ~ as.copyToSize(2)            =**= as.take(2)
    T ~ as.copyToSize(4)            =**= Array[Short](1, 2, 3, 0)
    T ~ as.shrinkCopy(2)            =**= as.take(2)
    T ~ (as.shrinkCopy(4) eq as)    ==== true
    T ~ as.copyOfRange(1, 3)        =**= as.drop(1)
    T ~ as.addLeftSlots(1)          =**= (0 +: as)
    T ~ as.addRightSlots(1)         =**= (as :+ 0)
    T ~ as.copyInto(bs)             =**= Array[Short](1, 2, 3, 3, 4)
    T ~ as.copyInto(bs, 2)          =**= Array[Short](1, 2, 1, 2, 3)
    T ~ as.copyRangeInto(1,3)(bs)   =**= Array[Short](2, 3, 1, 2, 3)
    T ~ as.copyRangeInto(1,3)(bs,1) =**= Array[Short](2, 2, 3, 2, 3)
    T ~ as.py(1)                    ==== as(1)
    T ~ as.py(-3)                   ==== as(0)
    T ~ bs.py.index(-2)             ==== 3
    T ~ { bs.py(-4) = 0; bs }       =**= Array[Short](2, 0, 3, 2, 3)
    T ~ bs.isSorted                 ==== false
    T ~ bs.isSortedRange(1, 3)      ==== true
    T ~ as.search(2)                ==== 1
    T ~ as.search(0)                ==== -1
    T ~ bs.searchRange(1, 3)(3)     ==== 2
    T ~ bs.searchRange(1, 3)(2)     ==== -3
    T ~ bs.sortRange(0, 3)          =**= Array[Short](0, 2, 3, 2, 3)
    T ~ bs.sort()                   =**= Array[Short](0, 2, 2, 3, 3)
    T ~ bs.fillRange(2, 4)(1)       =**= Array[Short](0, 2, 1, 1, 3)
    T ~ bs.zapAll(s=>(s+1).toShort) =**= Array[Short](1, 3, 2, 2, 4)
    T ~ bs.fill(4)                  =**= Array[Short](4, 4, 4, 4, 4)

    val ac = Array[Char]('1', '2', '3')
    val bc = Array[Char]('0', '1', '2', '3', '4')
    T ~ ac.copy                       =**= ac
    T ~ (ac.copy eq ac)               ==== false
    T ~ ac.copy.tap(_(0) = '4').toSeq =!!= ac.toSeq
    T ~ ac.copyToSize(2).length     ==== 2
    T ~ ac.copyToSize(2)            =**= ac.take(2)
    T ~ ac.copyToSize(4)            =**= Array[Char]('1', '2', '3', '\u0000')
    T ~ ac.shrinkCopy(2)            =**= ac.take(2)
    T ~ (ac.shrinkCopy(4) eq ac)    ==== true
    T ~ ac.copyOfRange(1, 3)        =**= ac.drop(1)
    T ~ ac.addLeftSlots(1)          =**= ('\u0000' +: ac)
    T ~ ac.addRightSlots(1)         =**= (ac :+ '\u0000')
    T ~ ac.copyInto(bc)             =**= Array[Char]('1', '2', '3', '3', '4')
    T ~ ac.copyInto(bc, 2)          =**= Array[Char]('1', '2', '1', '2', '3')
    T ~ ac.copyRangeInto(1,3)(bc)   =**= Array[Char]('2', '3', '1', '2', '3')
    T ~ ac.copyRangeInto(1,3)(bc,1) =**= Array[Char]('2', '2', '3', '2', '3')
    T ~ ac.py(1)                    ==== ac(1)
    T ~ ac.py(-3)                   ==== ac(0)
    T ~ bc.py.index(-2)             ==== 3
    T ~ { bc.py(-4) = '0'; bc }     =**= Array[Char]('2', '0', '3', '2', '3')
    T ~ bc.isSorted                 ==== false
    T ~ bc.isSortedRange(1, 3)      ==== true
    T ~ ac.search('2')              ==== 1
    T ~ ac.search('0')              ==== -1
    T ~ bc.searchRange(1, 3)('3')   ==== 2
    T ~ bc.searchRange(1, 3)('2')   ==== -3
    T ~ bc.sortRange(0, 3)          =**= Array[Char]('0', '2', '3', '2', '3')
    T ~ bc.sort()                   =**= Array[Char]('0', '2', '2', '3', '3')
    T ~ bc.fillRange(2, 4)('e')     =**= Array[Char]('0', '2', 'e', 'e', '3')
    T ~ bc.zapAll(_.toUpper)        =**= Array[Char]('0', '2', 'E', 'E', '3')
    T ~ bc.fill('4')                =**= Array[Char]('4', '4', '4', '4', '4')

    val ai = Array[Int](1, 2, 3)
    val bi = Array[Int](0, 1, 2, 3, 4)
    T ~ ai.copy                     =**= ai
    T ~ (ai.copy eq ai)             ==== false
    T ~ ai.copy.tap(_(0) = 4).toSeq =!!= ai.toSeq
    T ~ ai.copyToSize(2).length     ==== 2
    T ~ ai.copyToSize(2)            =**= ai.take(2)
    T ~ ai.copyToSize(4)            =**= Array[Int](1, 2, 3, 0)
    T ~ ai.shrinkCopy(2)            =**= ai.take(2)
    T ~ (ai.shrinkCopy(4) eq ai)    ==== true
    T ~ ai.copyOfRange(1, 3)        =**= ai.drop(1)
    T ~ ai.addLeftSlots(1)          =**= (0 +: ai)
    T ~ ai.addRightSlots(1)         =**= (ai :+ 0)
    T ~ ai.copyInto(bi)             =**= Array[Int](1, 2, 3, 3, 4)
    T ~ ai.copyInto(bi, 2)          =**= Array[Int](1, 2, 1, 2, 3)
    T ~ ai.copyRangeInto(1,3)(bi)   =**= Array[Int](2, 3, 1, 2, 3)
    T ~ ai.copyRangeInto(1,3)(bi,1) =**= Array[Int](2, 2, 3, 2, 3)
    T ~ ai.py(1)                    ==== ai(1)
    T ~ ai.py(-3)                   ==== ai(0)
    T ~ bi.py.index(-2)             ==== 3
    T ~ { bi.py(-4) = 0; bi }       =**= Array[Int](2, 0, 3, 2, 3)
    T ~ Array(0x05030107).unpackBytes =**= Array[Byte](7, 1, 3, 5)
    T ~ bi.isSorted                 ==== false
    T ~ bi.isSortedRange(1, 3)      ==== true
    T ~ ai.search(2)                ==== 1
    T ~ ai.search(0)                ==== -1
    T ~ bi.searchRange(1, 3)(3)     ==== 2
    T ~ bi.searchRange(1, 3)(2)     ==== -3
    T ~ bi.sortRange(0, 3)          =**= Array[Int](0, 2, 3, 2, 3)
    T ~ bi.sort()                   =**= Array[Int](0, 2, 2, 3, 3)
    T ~ bi.fillRange(2, 4)(1)       =**= Array[Int](0, 2, 1, 1, 3)
    T ~ bi.zapAll(_ + 1)            =**= Array[Int](1, 3, 2, 2, 4)
    T ~ bi.fill(4)                  =**= Array[Int](4, 4, 4, 4, 4)

    val al = Array[Long](1, 2, 3)
    val bl = Array[Long](0, 1, 2, 3, 4)
    T ~ al.copy                     =**= al
    T ~ (al.copy eq al)             ==== false
    T ~ al.copy.tap(_(0) = 4).toSeq =!!= al.toSeq
    T ~ al.copyToSize(2).length     ==== 2
    T ~ al.copyToSize(2)            =**= al.take(2)
    T ~ al.copyToSize(4)            =**= Array[Long](1, 2, 3, 0)
    T ~ al.shrinkCopy(2)            =**= al.take(2)
    T ~ (al.shrinkCopy(4) eq al)    ==== true
    T ~ al.copyOfRange(1, 3)        =**= al.drop(1)
    T ~ al.addLeftSlots(1)          =**= (0 +: al)
    T ~ al.addRightSlots(1)         =**= (al :+ 0)
    T ~ al.copyInto(bl)             =**= Array[Long](1, 2, 3, 3, 4)
    T ~ al.copyInto(bl, 2)          =**= Array[Long](1, 2, 1, 2, 3)
    T ~ al.copyRangeInto(1,3)(bl)   =**= Array[Long](2, 3, 1, 2, 3)
    T ~ al.copyRangeInto(1,3)(bl,1) =**= Array[Long](2, 2, 3, 2, 3)
    T ~ al.py(1)                    ==== al(1)
    T ~ al.py(-3)                   ==== al(0)
    T ~ bl.py.index(-2)             ==== 3
    T ~ { bl.py(-4) = 0; bl }       =**= Array[Long](2, 0, 3, 2, 3)
    T ~ Array(0x0102030405060708L).unpackBytes =**= Array[Byte](8, 7, 6, 5, 4, 3, 2, 1)
    T ~ bl.isSorted                 ==== false
    T ~ bl.isSortedRange(1, 3)      ==== true
    T ~ al.search(2)                ==== 1
    T ~ al.search(0)                ==== -1
    T ~ bl.searchRange(1, 3)(3)     ==== 2
    T ~ bl.searchRange(1, 3)(2)     ==== -3
    T ~ bl.sortRange(0, 3)          =**= Array[Long](0, 2, 3, 2, 3)
    T ~ bl.sort()                   =**= Array[Long](0, 2, 2, 3, 3)
    T ~ bl.fillRange(2, 4)(1)       =**= Array[Long](0, 2, 1, 1, 3)
    T ~ bl.zapAll(_ + 1)            =**= Array[Long](1, 3, 2, 2, 4)
    T ~ bl.fill(4)                  =**= Array[Long](4, 4, 4, 4, 4)

    val af = Array[Float](1, 2, 3)
    val bf = Array[Float](0, 1, 2, 3, 4)
    T ~ af.copy                     =**= af
    T ~ (af.copy eq af)             ==== false
    T ~ af.copy.tap(_(0) = 4).toSeq =!!= af.toSeq
    T ~ af.copyToSize(2).length     ==== 2
    T ~ af.copyToSize(2)            =**= af.take(2)
    T ~ af.copyToSize(4)            =**= Array[Float](1, 2, 3, 0)
    T ~ af.shrinkCopy(2)            =**= af.take(2)
    T ~ (af.shrinkCopy(4) eq af)    ==== true
    T ~ af.copyOfRange(1, 3)        =**= af.drop(1)
    T ~ af.addLeftSlots(1)          =**= (0 +: af)
    T ~ af.addRightSlots(1)         =**= (af :+ 0)
    T ~ af.copyInto(bf)             =**= Array[Float](1, 2, 3, 3, 4)
    T ~ af.copyInto(bf, 2)          =**= Array[Float](1, 2, 1, 2, 3)
    T ~ af.copyRangeInto(1,3)(bf)   =**= Array[Float](2, 3, 1, 2, 3)
    T ~ af.copyRangeInto(1,3)(bf,1) =**= Array[Float](2, 2, 3, 2, 3)
    T ~ af.py(1)                    ==== af(1)
    T ~ af.py(-3)                   ==== af(0)
    T ~ bf.py.index(-2)             ==== 3
    T ~ { bf.py(-4) = 0; bf }       =**= Array[Float](2, 0, 3, 2, 3)
    T ~ Array(1.4f).unpackBytes     =**= Array[Byte](51, 51, -77, 63)
    T ~ bf.isSorted                 ==== false
    T ~ bf.isSortedRange(1, 3)      ==== true
    T ~ af.search(2)                ==== 1
    T ~ af.search(0)                ==== -1
    T ~ bf.searchRange(1, 3)(3)     ==== 2
    T ~ bf.searchRange(1, 3)(2)     ==== -3
    T ~ bf.sortRange(0, 3)          =**= Array[Float](0, 2, 3, 2, 3)
    T ~ bf.sort()                   =**= Array[Float](0, 2, 2, 3, 3)
    T ~ bf.fillRange(2, 4)(1)       =**= Array[Float](0, 2, 1, 1, 3)
    T ~ bf.zapAll(_ + 1)            =**= Array[Float](1, 3, 2, 2, 4)
    T ~ bf.fill(4)                  =**= Array[Float](4, 4, 4, 4, 4)

    val ad = Array[Double](1, 2, 3)
    val bd = Array[Double](0, 1, 2, 3, 4)
    T ~ ad.copy                     =**= ad
    T ~ (ad.copy eq ad)             ==== false
    T ~ ad.copy.tap(_(0) = 4).toSeq =!!= ad.toSeq
    T ~ ad.copyToSize(2).length     ==== 2
    T ~ ad.copyToSize(2)            =**= ad.take(2)
    T ~ ad.copyToSize(4)            =**= Array[Double](1, 2, 3, 0)
    T ~ ad.shrinkCopy(2)            =**= ad.take(2)
    T ~ (ad.shrinkCopy(4) eq ad)    ==== true
    T ~ ad.copyOfRange(1, 3)        =**= ad.drop(1)
    T ~ ad.addLeftSlots(1)          =**= (0 +: ad)
    T ~ ad.addRightSlots(1)         =**= (ad :+ 0)
    T ~ ad.copyInto(bd)             =**= Array[Double](1, 2, 3, 3, 4)
    T ~ ad.copyInto(bd, 2)          =**= Array[Double](1, 2, 1, 2, 3)
    T ~ ad.copyRangeInto(1,3)(bd)   =**= Array[Double](2, 3, 1, 2, 3)
    T ~ ad.copyRangeInto(1,3)(bd,1) =**= Array[Double](2, 2, 3, 2, 3)
    T ~ ad.py(1)                    ==== ad(1)
    T ~ ad.py(-3)                   ==== ad(0)
    T ~ bd.py.index(-2)             ==== 3
    T ~ { bd.py(-4) = 0; bd }       =**= Array[Double](2, 0, 3, 2, 3)
    T ~ Array(1.41).unpackBytes     =**= Array[Byte](-113, -62, -11, 40, 92, -113, -10, 63)
    T ~ bd.isSorted                 ==== false
    T ~ bd.isSortedRange(1, 3)      ==== true
    T ~ ad.search(2)                ==== 1
    T ~ ad.search(0)                ==== -1
    T ~ bd.searchRange(1, 3)(3)     ==== 2
    T ~ bd.searchRange(1, 3)(2)     ==== -3
    T ~ bd.sortRange(0, 3)          =**= Array[Double](0, 2, 3, 2, 3)
    T ~ bd.sort()                   =**= Array[Double](0, 2, 2, 3, 3)
    T ~ bd.fillRange(2, 4)(1)       =**= Array[Double](0, 2, 1, 1, 3)
    T ~ bd.zapAll(_ + 1)            =**= Array[Double](1, 3, 2, 2, 4)
    T ~ bd.fill(4)                  =**= Array[Double](4, 4, 4, 4, 4)

    val aa = Array[String]("1", "2", "3")
    val ba = Array[String]("0", "1", "2", "3", "4")
    T ~ aa.copy                       =**= aa
    T ~ (aa.copy eq aa)               ==== false
    T ~ aa.copy.tap(_(0) = "4").toSeq =!!= aa.toSeq
    T ~ aa.copyToSize(2).length     ==== 2
    T ~ aa.copyToSize(2)            =**= aa.take(2)
    T ~ aa.copyToSize(4)            =**= Array[String]("1", "2", "3", null)
    T ~ aa.shrinkCopy(2)            =**= aa.take(2)
    T ~ (aa.shrinkCopy(4) eq aa)    ==== true
    T ~ aa.copyOfRange(1, 3)        =**= aa.drop(1)
    T ~ aa.addLeftSlots(1)          =**= (null +: aa)
    T ~ aa.addRightSlots(1)         =**= (aa :+ null)
    T ~ aa.copyInto(ba)             =**= Array[String]("1", "2", "3", "3", "4")
    T ~ aa.copyInto(ba, 2)          =**= Array[String]("1", "2", "1", "2", "3")
    T ~ aa.copyRangeInto(1,3)(ba)   =**= Array[String]("2", "3", "1", "2", "3")
    T ~ aa.copyRangeInto(1,3)(ba,1) =**= Array[String]("2", "2", "3", "2", "3")
    T ~ aa.py(1)                    ==== aa(1)
    T ~ aa.py(-3)                   ==== aa(0)
    T ~ ba.py.index(-2)             ==== 3
    T ~ { ba.py(-4) = "0"; ba }     =**= Array[String]("2", "0", "3", "2", "3")
    T ~ ba.isSorted                 ==== false
    T ~ ba.isSortedRange(1, 3)      ==== true
    T ~ aa.search("2")              ==== 1
    T ~ aa.search("0")              ==== -1
    T ~ ba.searchRange(1, 3)("3")   ==== 2
    T ~ ba.searchRange(1, 3)("2")   ==== -3
    T ~ ba.sortRange(0, 3)          =**= Array[String]("0", "2", "3", "2", "3")
    T ~ ba.sort()                   =**= Array[String]("0", "2", "2", "3", "3")
    T ~ ba.fillRange(2, 4)("e")     =**= Array[String]("0", "2", "e", "e", "3")
    T ~ ba.zapAll(_.toUpperCase)    =**= Array[String]("0", "2", "E", "E", "3")
    T ~ ba.fill("4")                =**= Array[String]("4", "4", "4", "4", "4")


    val m = Mu(5)
    T ~ m.value            ==== 5
    T ~ { m set 4 }        ==== Mu(4)
    T ~ { m.value = 3; m } ==== Mu(3)
    T ~ m.zap(_ - 1)       ==== Mu(2)
    T ~ m.toString         ==== "~2"
    T ~ m.copy             ==== Mu(2) --: typed[Mu.MuInt]

    T ~ Mu(())      .zap(_ => ())           .pipe(x => (x, x.copy.set(())))   .sameOp(_.value) ==== ((), ())
    T ~ Mu(true)    .zap(z => !z)           .pipe(x => (x, x.copy.set(true))) .sameOp(_.value) ==== (false, true)
    T ~ Mu(1: Byte ).zap(b => (b+1).toByte) .pipe(x => (x, x.copy.set(4)))    .sameOp(_.value) ==== (2: Byte, 4: Byte)   --: typed[(Byte, Byte)]
    T ~ Mu(1: Short).zap(s => (s+1).toShort).pipe(x => (x, x.copy.set(4)))    .sameOp(_.value) ==== (2: Short, 4: Short) --: typed[(Short, Short)]
    T ~ Mu('e')     .zap(_.toUpper)         .pipe(x => (x, x.copy.set('f')))  .sameOp(_.value) ==== ('E', 'f')           --: typed[(Char, Char)]
    T ~ Mu(1)       .zap(_ + 1)             .pipe(x => (x, x.copy.set(4)))    .sameOp(_.value) ==== (2, 4)               --: typed[(Int, Int)]
    T ~ Mu(1L)      .zap(_ + 1)             .pipe(x => (x, x.copy.set(4)))    .sameOp(_.value) ==== (2L, 4L)             --: typed[(Long, Long)]
    T ~ Mu(1f)      .zap(_ + 1f)            .pipe(x => (x, x.copy.set(4f)))   .sameOp(_.value) ==== (2f, 4f)             --: typed[(Float, Float)]
    T ~ Mu(1.0)     .zap(_ + 1.0)           .pipe(x => (x, x.copy.set(4.0)))  .sameOp(_.value) ==== (2.0, 4.0)           --: typed[(Double, Double)]
    T ~ Mu("cod")   .zap(_ + "!")           .pipe(x => (x, x.copy.set("eel"))).sameOp(_.value) ==== ("cod!", "eel")      --: typed[(String, String)]


    inline def gm[A](a: A): Mu[A] = inline a match
      case _: Unit    => Mu.MuUnit.asInstanceOf[Mu[A]]
      case z: Boolean => Mu(z)
      case b: Byte    => Mu(b)
      case s: Short   => Mu(s)
      case c: Char    => Mu(c)
      case i: Int     => Mu(i)
      case l: Long    => Mu(l)
      case f: Float   => Mu(f)
      case d: Double  => Mu(d)
      case _          => Mu(a)
    T ~ Anon(Mu(()))                ==== typed[Anon[Mu.MuUnit.type]]
    T ~ Anon(gm(()))                ==== typed[Anon[Mu[Unit]]]
    T ~ Anon(gm(()).specific)       ==== typed[Anon[Mu.MuUnit.type]]
    T ~ Anon(Mu(true))              ==== typed[Anon[Mu.MuBoolean]]
    T ~ Anon(gm(true))              ==== typed[Anon[Mu[Boolean]]]
    T ~ Anon(gm(true).specific)     ==== typed[Anon[Mu.MuBoolean]]
    T ~ Anon(Mu(1: Byte))           ==== typed[Anon[Mu.MuByte]]
    T ~ Anon(gm(1: Byte))           ==== typed[Anon[Mu[Byte]]]
    T ~ Anon(gm(1: Byte).specific)  ==== typed[Anon[Mu.MuByte]]
    T ~ Anon(Mu(2: Short))          ==== typed[Anon[Mu.MuShort]]
    T ~ Anon(gm(2: Short))          ==== typed[Anon[Mu[Short]]]
    T ~ Anon(gm(2: Short).specific) ==== typed[Anon[Mu.MuShort]]
    T ~ Anon(Mu('e'))               ==== typed[Anon[Mu.MuChar]]
    T ~ Anon(gm('e'))               ==== typed[Anon[Mu[Char]]]
    T ~ Anon(gm('e').specific)      ==== typed[Anon[Mu.MuChar]]
    T ~ Anon(Mu(4))                 ==== typed[Anon[Mu.MuInt]]
    T ~ Anon(gm(4))                 ==== typed[Anon[Mu[Int]]]
    T ~ Anon(gm(4).specific)        ==== typed[Anon[Mu.MuInt]]
    T ~ Anon(Mu(5L))                ==== typed[Anon[Mu.MuLong]]
    T ~ Anon(gm(5L))                ==== typed[Anon[Mu[Long]]]
    T ~ Anon(gm(5L).specific)       ==== typed[Anon[Mu.MuLong]]
    T ~ Anon(Mu(6f))                ==== typed[Anon[Mu.MuFloat]]
    T ~ Anon(gm(6f))                ==== typed[Anon[Mu[Float]]]
    T ~ Anon(gm(6f).specific)       ==== typed[Anon[Mu.MuFloat]]
    T ~ Anon(Mu(7.0))               ==== typed[Anon[Mu.MuDouble]]
    T ~ Anon(gm(7.0))               ==== typed[Anon[Mu[Double]]]
    T ~ Anon(gm(7.0).specific)      ==== typed[Anon[Mu.MuDouble]]
    T ~ Anon(Mu("cod"))             ==== typed[Anon[Mu[String]]]
    T ~ Anon(gm("cod"))             ==== typed[Anon[Mu[String]]]
    T ~ Anon(gm("cod").specific)    ==== typed[Anon[Mu[String]]]


  @Test
  def immutableDataTest(): Unit =
    object Meter extends NewType[Double] {
      extension (t: Type) {
        def *(that: Double): Meter.Type = Meter(t.value * that)
      }
    }
    T ~ Meter(3)         ==== 3 --: typed[Meter.Type]
    T ~ { Meter(2) * 3 } ==== 6 --: typed[Meter.Type]

    T ~ "herring".fn( s => s.length + s.head)                ==== (7 + 'h')
    T ~ "salmon".pipe(s => s.length + s.head)                ==== (6 + 's')
    T ~ { var x = 0; "cod".tap(s => x = s.head).length + x } ==== (3 + 'c')
    T ~ "herring".tup(5)                                     ==== ("herring", 5)
    T ~ "herring".tupWith(_.length)                          ==== ("herring", 7)

    val f1 = (i: Int) => i+1
    val f2 = (c: Char) => c > 'e'
    T ~ (1, 'a')._1to(true)                  ==== (true, 'a')
    T ~ (1, 'a')._2to(true)                  ==== (1, true)
    T ~ (1, 'a')._1op(_ < 3)                 ==== (true, 'a')
    T ~ (1, 'a')._2op(_ < 'e')               ==== (1, true)
    T ~ (1, 'a').ops(f1, f2)                 ==== (2, false)
    T ~ (1, 'a').sameOp(_ == 'a')            ==== (false, true)
    T ~ (1, 'a').merge(_ + _)                ==== (1 + 'a')
    T ~ (1, 'a').reduce((a, b) => a)         ==== 1 --: typed[Int | Char]
    T ~ (1, 'a').tup(true)                   ==== (1, 'a', true)
    T ~ (1, 'a').tup_1(true)                 ==== (true, 1, 'a')
    T ~ (1, 'a').tup_2(true)                 ==== (1, true, 'a')
    T ~ (1, 'a').tupWith((a, b) => b + a)    ==== (1, 'a', 'b')
    T ~ (1, 'a').snip                        ==== 1
    T ~ (1, 'a').snip_1                      ==== 'a'
    T ~ (1, 'a').join((2, 3))                ==== (1, 'a', 2, 3)
    T ~ (1, 'a').join((2, 3, 4))             ==== (1, 'a', 2, 3, 4)
    T ~ (1, 'a').join((2, 3, 4, 5))          ==== (1, 'a', 2, 3, 4, 5)
    T ~ (1, 'a').join((2, 3, 4, 5, 6))       ==== (1, 'a', 2, 3, 4, 5, 6)
    T ~ (1, 'a').join((2, 3, 4, 5, 6, 7))    ==== (1, 'a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a').join((2, 3, 4, 5, 6, 7, 8)) ==== (1, 'a', 2, 3, 4, 5, 6, 7, 8)

    val f3 = (i: Int) => i-1
    T ~ (1, 'a', 2)._1to(true)                  ==== (true, 'a', 2)
    T ~ (1, 'a', 2)._2to(true)                  ==== (1, true, 2)
    T ~ (1, 'a', 2)._3to(true)                  ==== (1, 'a', true)
    T ~ (1, 'a', 2)._1op(_ < 3)                 ==== (true, 'a', 2)
    T ~ (1, 'a', 2)._2op(_ < 'e')               ==== (1, true, 2)
    T ~ (1, 'a', 2)._3op(_ < 3)                 ==== (1, 'a', true)
    T ~ (1, 'a', 2).ops(f1, f2, f3)             ==== (2, false, 1)
    T ~ (1, 'a', 2).sameOp(_ == 'a')            ==== (false, true, false)
    T ~ (1, 'a', 2).merge(_ + _ + _)            ==== (3 + 'a')
    T ~ (1, 'a', 2).reduce((a, b) => a)         ==== 1 --: typed[Int | Char]
    T ~ (1, 'a', 2).tup(true)                   ==== (1, 'a', 2, true)
    T ~ (1, 'a', 2).tup_1(true)                 ==== (true, 1, 'a', 2)
    T ~ (1, 'a', 2).tup_2(true)                 ==== (1, true, 'a', 2)
    T ~ (1, 'a', 2).tup_3(true)                 ==== (1, 'a', true, 2)
    T ~ (1, 'a', 2).tupWith((a, b, c) => b + a) ==== (1, 'a', 2, 'b')
    T ~ (1, 'a', 2).snip                        ==== (1, 'a')
    T ~ (1, 'a', 2).snip_1                      ==== ('a', 2)
    T ~ (1, 'a', 2).snip_2                      ==== (1, 2)
    T ~ (1, 'a', 2).join((3, 4))                ==== (1, 'a', 2, 3, 4)
    T ~ (1, 'a', 2).join((3, 4, 5))             ==== (1, 'a', 2, 3, 4, 5)
    T ~ (1, 'a', 2).join((3, 4, 5, 6))          ==== (1, 'a', 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2).join((3, 4, 5, 6, 7))       ==== (1, 'a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2).join((3, 4, 5, 6, 7, 8))    ==== (1, 'a', 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2).cutAt1                      ==== (1, ('a', 2))
    T ~ (1, 'a', 2).cutAt2                      ==== ((1, 'a'), 2)

    val f4 = (i: Int) => i*i
    T ~ (1, 'a', 2, 3)._1to(true)                  ==== (true, 'a', 2, 3)
    T ~ (1, 'a', 2, 3)._2to(true)                  ==== (1, true, 2, 3)
    T ~ (1, 'a', 2, 3)._3to(true)                  ==== (1, 'a', true, 3)
    T ~ (1, 'a', 2, 3)._4to(true)                  ==== (1, 'a', 2, true)
    T ~ (1, 'a', 2, 3)._1op(_ < 3)                 ==== (true, 'a', 2, 3)
    T ~ (1, 'a', 2, 3)._2op(_ < 'e')               ==== (1, true, 2, 3)
    T ~ (1, 'a', 2, 3)._3op(_ < 3)                 ==== (1, 'a', true, 3)
    T ~ (1, 'a', 2, 3)._4op(_ < 3)                 ==== (1, 'a', 2, false)
    T ~ (1, 'a', 2, 3).ops(f1, f2, f3, f4)         ==== (2, false, 1, 9)
    T ~ (1, 'a', 2, 3).sameOp(_ == 'a')            ==== (false, true, false, false)
    T ~ (1, 'a', 2, 3).merge(_ + _ + _ + _)        ==== (6 + 'a')
    T ~ (1, 'a', 2, 3).reduce((a, b) => a)         ==== 1 --: typed[Int | Char]
    T ~ (1, 'a', 2, 3).tup(true)                   ==== (1, 'a', 2, 3, true)
    T ~ (1, 'a', 2, 3).tup_1(true)                 ==== (true, 1, 'a', 2, 3)
    T ~ (1, 'a', 2, 3).tup_2(true)                 ==== (1, true, 'a', 2, 3)
    T ~ (1, 'a', 2, 3).tup_3(true)                 ==== (1, 'a', true, 2, 3)
    T ~ (1, 'a', 2, 3).tup_4(true)                 ==== (1, 'a', 2, true, 3)
    T ~ (1, 'a', 2, 3)
          .tupWith((a, b, c, d) => b + a)          ==== (1, 'a', 2, 3, 'b')
    T ~ (1, 'a', 2, 3).snip                        ==== (1, 'a', 2)
    T ~ (1, 'a', 2, 3).snip_1                      ==== ('a', 2, 3)
    T ~ (1, 'a', 2, 3).snip_2                      ==== (1, 2, 3)
    T ~ (1, 'a', 2, 3).snip_3                      ==== (1, 'a', 3)
    T ~ (1, 'a', 2, 3).join((4, 5))                ==== (1, 'a', 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3).join((4, 5, 6))             ==== (1, 'a', 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3).join((4, 5, 6, 7))          ==== (1, 'a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3).join((4, 5, 6, 7, 8))       ==== (1, 'a', 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3).cutAt1                      ==== (1, ('a', 2, 3))
    T ~ (1, 'a', 2, 3).cutAt2                      ==== ((1, 'a'), (2, 3))
    T ~ (1, 'a', 2, 3).cutAt3                      ==== ((1, 'a', 2), 3)

    val f5 = (i: Int) => 9-i
    T ~ (1, 'a', 2, 3, 4)._1to(true)                  ==== (true, 'a', 2, 3, 4)
    T ~ (1, 'a', 2, 3, 4)._2to(true)                  ==== (1, true, 2, 3, 4)
    T ~ (1, 'a', 2, 3, 4)._3to(true)                  ==== (1, 'a', true, 3, 4)
    T ~ (1, 'a', 2, 3, 4)._4to(true)                  ==== (1, 'a', 2, true, 4)
    T ~ (1, 'a', 2, 3, 4)._5to(true)                  ==== (1, 'a', 2, 3, true)
    T ~ (1, 'a', 2, 3, 4)._1op(_ < 3)                 ==== (true, 'a', 2, 3, 4)
    T ~ (1, 'a', 2, 3, 4)._2op(_ < 'e')               ==== (1, true, 2, 3, 4)
    T ~ (1, 'a', 2, 3, 4)._3op(_ < 3)                 ==== (1, 'a', true, 3, 4)
    T ~ (1, 'a', 2, 3, 4)._4op(_ < 3)                 ==== (1, 'a', 2, false, 4)
    T ~ (1, 'a', 2, 3, 4)._5op(_ < 3)                 ==== (1, 'a', 2, 3, false)
    T ~ (1, 'a', 2, 3, 4).ops(f1, f2, f3, f4, f5)     ==== (2, false, 1, 9, 5)
    T ~ (1, 'a', 2, 3, 4).sameOp(_ == 'a')            ==== (false, true, false, false, false)
    T ~ (1, 'a', 2, 3, 4).merge(_ + _ + _ + _ + _)    ==== (10 + 'a')
    T ~ (1, 'a', 2, 3, 4).reduce((a, b) => a)         ==== 1 --: typed[Int | Char]
    T ~ (1, 'a', 2, 3, 4).tup(true)                   ==== (1, 'a', 2, 3, 4, true)
    T ~ (1, 'a', 2, 3, 4).tup_1(true)                 ==== (true, 1, 'a', 2, 3, 4)
    T ~ (1, 'a', 2, 3, 4).tup_2(true)                 ==== (1, true, 'a', 2, 3, 4)
    T ~ (1, 'a', 2, 3, 4).tup_3(true)                 ==== (1, 'a', true, 2, 3, 4)
    T ~ (1, 'a', 2, 3, 4).tup_4(true)                 ==== (1, 'a', 2, true, 3, 4)
    T ~ (1, 'a', 2, 3, 4).tup_5(true)                 ==== (1, 'a', 2, 3, true, 4)    
    T ~ (1, 'a', 2, 3, 4)
          .tupWith((a, b, c, d, e) => b + a)          ==== (1, 'a', 2, 3, 4, 'b')
    T ~ (1, 'a', 2, 3, 4).snip                        ==== (1, 'a', 2, 3)
    T ~ (1, 'a', 2, 3, 4).snip_1                      ==== ('a', 2, 3, 4)
    T ~ (1, 'a', 2, 3, 4).snip_2                      ==== (1, 2, 3, 4)
    T ~ (1, 'a', 2, 3, 4).snip_3                      ==== (1, 'a', 3, 4)
    T ~ (1, 'a', 2, 3, 4).snip_4                      ==== (1, 'a', 2, 4)
    T ~ (1, 'a', 2, 3, 4).join((5, 6))                ==== (1, 'a', 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4).join((5, 6, 7))             ==== (1, 'a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4).join((5, 6, 7, 8))          ==== (1, 'a', 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4).cutAt1                      ==== (1, ('a', 2, 3, 4))
    T ~ (1, 'a', 2, 3, 4).cutAt2                      ==== ((1, 'a'), (2, 3, 4))
    T ~ (1, 'a', 2, 3, 4).cutAt3                      ==== ((1, 'a', 2), (3, 4))
    T ~ (1, 'a', 2, 3, 4).cutAt4                      ==== ((1, 'a', 2, 3), 4)

    val f6 = (i: Int) => (i+1)/2
    T ~ (1, 'a', 2, 3, 4, 5)._1to(true)                  ==== (true, 'a', 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5)._2to(true)                  ==== (1, true, 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5)._3to(true)                  ==== (1, 'a', true, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5)._4to(true)                  ==== (1, 'a', 2, true, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5)._5to(true)                  ==== (1, 'a', 2, 3, true, 5)
    T ~ (1, 'a', 2, 3, 4, 5)._6to(true)                  ==== (1, 'a', 2, 3, 4, true)
    T ~ (1, 'a', 2, 3, 4, 5)._1op(_ < 3)                 ==== (true, 'a', 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5)._2op(_ < 'e')               ==== (1, true, 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5)._3op(_ < 3)                 ==== (1, 'a', true, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5)._4op(_ < 3)                 ==== (1, 'a', 2, false, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5)._5op(_ < 3)                 ==== (1, 'a', 2, 3, false, 5)
    T ~ (1, 'a', 2, 3, 4, 5)._6op(_ < 3)                 ==== (1, 'a', 2, 3, 4, false)
    T ~ (1, 'a', 2, 3, 4, 5).ops(f1, f2, f3, f4, f5, f6) ==== (2, false, 1, 9, 5, 3)
    T ~ (1, 'a', 2, 3, 4, 5).sameOp(_ == 'a')            ==== (false, true, false, false, false, false)
    T ~ (1, 'a', 2, 3, 4, 5).merge(_ + _ + _ + _ + _ + _)==== (15 + 'a')
    T ~ (1, 'a', 2, 3, 4, 5).reduce((a, b) => a)         ==== 1 --: typed[Int | Char]
    T ~ (1, 'a', 2, 3, 4, 5).tup(true)                   ==== (1, 'a', 2, 3, 4, 5, true)
    T ~ (1, 'a', 2, 3, 4, 5).tup_1(true)                 ==== (true, 1, 'a', 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5).tup_2(true)                 ==== (1, true, 'a', 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5).tup_3(true)                 ==== (1, 'a', true, 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5).tup_4(true)                 ==== (1, 'a', 2, true, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5).tup_5(true)                 ==== (1, 'a', 2, 3, true, 4, 5)    
    T ~ (1, 'a', 2, 3, 4, 5).tup_6(true)                 ==== (1, 'a', 2, 3, 4, true, 5)    
    T ~ (1, 'a', 2, 3, 4, 5)
          .tupWith((a, b, c, d, e, f) => b + a)          ==== (1, 'a', 2, 3, 4, 5, 'b')
    T ~ (1, 'a', 2, 3, 4, 5).snip                        ==== (1, 'a', 2, 3, 4)
    T ~ (1, 'a', 2, 3, 4, 5).snip_1                      ==== ('a', 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5).snip_2                      ==== (1, 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5).snip_3                      ==== (1, 'a', 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5).snip_4                      ==== (1, 'a', 2, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5).snip_5                      ==== (1, 'a', 2, 3, 5)
    T ~ (1, 'a', 2, 3, 4, 5).join((6, 7))                ==== (1, 'a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5).join((6, 7, 8))             ==== (1, 'a', 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5).cutAt1                      ==== (1, ('a', 2, 3, 4, 5))
    T ~ (1, 'a', 2, 3, 4, 5).cutAt2                      ==== ((1, 'a'), (2, 3, 4, 5))
    T ~ (1, 'a', 2, 3, 4, 5).cutAt3                      ==== ((1, 'a', 2), (3, 4, 5))
    T ~ (1, 'a', 2, 3, 4, 5).cutAt4                      ==== ((1, 'a', 2, 3), (4, 5))
    T ~ (1, 'a', 2, 3, 4, 5).cutAt5                      ==== ((1, 'a', 2, 3, 4), 5)

    def f7 = (i: Int) => i+2
    T ~ (1, 'a', 2, 3, 4, 5, 6)._1to(true)          ==== (true, 'a', 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._2to(true)          ==== (1, true, 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._3to(true)          ==== (1, 'a', true, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._4to(true)          ==== (1, 'a', 2, true, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._5to(true)          ==== (1, 'a', 2, 3, true, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._6to(true)          ==== (1, 'a', 2, 3, 4, true, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._7to(true)          ==== (1, 'a', 2, 3, 4, 5, true)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._1op(_ < 3)         ==== (true, 'a', 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._2op(_ < 'e')       ==== (1, true, 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._3op(_ < 3)         ==== (1, 'a', true, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._4op(_ < 3)         ==== (1, 'a', 2, false, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._5op(_ < 3)         ==== (1, 'a', 2, 3, false, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._6op(_ < 3)         ==== (1, 'a', 2, 3, 4, false, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)._7op(_ < 3)         ==== (1, 'a', 2, 3, 4, 5, false)
    T ~ (1, 'a', 2, 3, 4, 5, 6)
          .ops(f1, f2, f3, f4, f5, f6, f7)          ==== (2, false, 1, 9, 5, 3, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6).sameOp(_ == 'a')    ==== (false, true, false, false, false, false, false)
    T ~ (1, 'a', 2, 3, 4, 5, 6)
          .merge(_ + _ + _ + _ + _ + _ + _)         ==== (21 + 'a')
    T ~ (1, 'a', 2, 3, 4, 5, 6).reduce((a, b) => a) ==== 1 --: typed[Int | Char]
    T ~ (1, 'a', 2, 3, 4, 5, 6).tup(true)           ==== (1, 'a', 2, 3, 4, 5, 6, true)
    T ~ (1, 'a', 2, 3, 4, 5, 6).tup_1(true)         ==== (true, 1, 'a', 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).tup_2(true)         ==== (1, true, 'a', 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).tup_3(true)         ==== (1, 'a', true, 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).tup_4(true)         ==== (1, 'a', 2, true, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).tup_5(true)         ==== (1, 'a', 2, 3, true, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).tup_6(true)         ==== (1, 'a', 2, 3, 4, true, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).tup_7(true)         ==== (1, 'a', 2, 3, 4, 5, true, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6)
          .tupWith((a, b, c, d, e, f, g) => b + a)  ==== (1, 'a', 2, 3, 4, 5, 6, 'b')
    T ~ (1, 'a', 2, 3, 4, 5, 6).snip                ==== (1, 'a', 2, 3, 4, 5)
    T ~ (1, 'a', 2, 3, 4, 5, 6).snip_1              ==== ('a', 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).snip_2              ==== (1, 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).snip_3              ==== (1, 'a', 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).snip_4              ==== (1, 'a', 2, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).snip_5              ==== (1, 'a', 2, 3, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).snip_6              ==== (1, 'a', 2, 3, 4, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6).join((7, 8))        ==== (1, 'a', 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6).cutAt1              ==== (1, ('a', 2, 3, 4, 5, 6))
    T ~ (1, 'a', 2, 3, 4, 5, 6).cutAt2              ==== ((1, 'a'), (2, 3, 4, 5, 6))
    T ~ (1, 'a', 2, 3, 4, 5, 6).cutAt3              ==== ((1, 'a', 2), (3, 4, 5, 6))
    T ~ (1, 'a', 2, 3, 4, 5, 6).cutAt4              ==== ((1, 'a', 2, 3), (4, 5, 6))
    T ~ (1, 'a', 2, 3, 4, 5, 6).cutAt5              ==== ((1, 'a', 2, 3, 4), (5, 6))
    T ~ (1, 'a', 2, 3, 4, 5, 6).cutAt6              ==== ((1, 'a', 2, 3, 4, 5), 6)


    def f8 = (i: Int) => (3*i)/5
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._1to(true)          ==== (true, 'a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._2to(true)          ==== (1, true, 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._3to(true)          ==== (1, 'a', true, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._4to(true)          ==== (1, 'a', 2, true, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._5to(true)          ==== (1, 'a', 2, 3, true, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._6to(true)          ==== (1, 'a', 2, 3, 4, true, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._7to(true)          ==== (1, 'a', 2, 3, 4, 5, true, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._8to(true)          ==== (1, 'a', 2, 3, 4, 5, 6, true)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._1op(_ < 3)         ==== (true, 'a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._2op(_ < 'e')       ==== (1, true, 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._3op(_ < 3)         ==== (1, 'a', true, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._4op(_ < 3)         ==== (1, 'a', 2, false, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._5op(_ < 3)         ==== (1, 'a', 2, 3, false, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._6op(_ < 3)         ==== (1, 'a', 2, 3, 4, false, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._7op(_ < 3)         ==== (1, 'a', 2, 3, 4, 5, false, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)._8op(_ < 3)         ==== (1, 'a', 2, 3, 4, 5, 6, false)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)
          .ops(f1, f2, f3, f4, f5, f6, f7, f8)         ==== (2, false, 1, 9, 5, 3, 8, 4)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).sameOp(_ == 'a')    ==== (false, true, false, false, false, false, false, false)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)
          .merge(_ + _ + _ + _ + _ + _ + _ + _)        ==== (28 + 'a')
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).reduce((a, b) => a) ==== 1 --: typed[Int | Char]
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).tup(true)           ==== (1, 'a', 2, 3, 4, 5, 6, 7, true)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).tup_1(true)         ==== (true, 1, 'a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).tup_2(true)         ==== (1, true, 'a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).tup_3(true)         ==== (1, 'a', true, 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).tup_4(true)         ==== (1, 'a', 2, true, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).tup_5(true)         ==== (1, 'a', 2, 3, true, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).tup_6(true)         ==== (1, 'a', 2, 3, 4, true, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).tup_7(true)         ==== (1, 'a', 2, 3, 4, 5, true, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).tup_8(true)         ==== (1, 'a', 2, 3, 4, 5, 6, true, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7)
          .tupWith((a, b, c, d, e, f, g, h) => b + a)  ==== (1, 'a', 2, 3, 4, 5, 6, 7, 'b')
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).snip                ==== (1, 'a', 2, 3, 4, 5, 6)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).snip_1              ==== ('a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).snip_2              ==== (1, 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).snip_3              ==== (1, 'a', 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).snip_4              ==== (1, 'a', 2, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).snip_5              ==== (1, 'a', 2, 3, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).snip_6              ==== (1, 'a', 2, 3, 4, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).snip_7              ==== (1, 'a', 2, 3, 4, 5, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).cutAt1              ==== (1, ('a', 2, 3, 4, 5, 6, 7))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).cutAt2              ==== ((1, 'a'), (2, 3, 4, 5, 6, 7))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).cutAt3              ==== ((1, 'a', 2), (3, 4, 5, 6, 7))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).cutAt4              ==== ((1, 'a', 2, 3), (4, 5, 6, 7))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).cutAt5              ==== ((1, 'a', 2, 3, 4), (5, 6, 7))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).cutAt6              ==== ((1, 'a', 2, 3, 4, 5), (6, 7))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7).cutAt7              ==== ((1, 'a', 2, 3, 4, 5, 6), 7)

    def f9 = (i: Int) => 1/i
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._1to(true)          ==== (true, 'a', 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._2to(true)          ==== (1, true, 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._3to(true)          ==== (1, 'a', true, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._4to(true)          ==== (1, 'a', 2, true, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._5to(true)          ==== (1, 'a', 2, 3, true, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._6to(true)          ==== (1, 'a', 2, 3, 4, true, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._7to(true)          ==== (1, 'a', 2, 3, 4, 5, true, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._8to(true)          ==== (1, 'a', 2, 3, 4, 5, 6, true, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._9to(true)          ==== (1, 'a', 2, 3, 4, 5, 6, 7, true)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._1op(_ < 3)         ==== (true, 'a', 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._2op(_ < 'e')       ==== (1, true, 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._3op(_ < 3)         ==== (1, 'a', true, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._4op(_ < 3)         ==== (1, 'a', 2, false, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._5op(_ < 3)         ==== (1, 'a', 2, 3, false, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._6op(_ < 3)         ==== (1, 'a', 2, 3, 4, false, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._7op(_ < 3)         ==== (1, 'a', 2, 3, 4, 5, false, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._8op(_ < 3)         ==== (1, 'a', 2, 3, 4, 5, 6, false, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)._9op(_ < 3)         ==== (1, 'a', 2, 3, 4, 5, 6, 7, false)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)
          .ops(f1, f2, f3, f4, f5, f6, f7, f8, f9)        ==== (2, false, 1, 9, 5, 3, 8, 4, 0)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).sameOp(_ == 'a')    ==== (false, true, false, false, false, false, false, false, false)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8)
          .merge(_ + _ + _ + _ + _ + _ + _ + _ + _)       ==== (36 + 'a')
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).reduce((a, b) => a) ==== 1 --: typed[Int | Char]
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).snip                ==== (1, 'a', 2, 3, 4, 5, 6, 7)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).snip_1              ==== ('a', 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).snip_2              ==== (1, 2, 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).snip_3              ==== (1, 'a', 3, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).snip_4              ==== (1, 'a', 2, 4, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).snip_5              ==== (1, 'a', 2, 3, 5, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).snip_6              ==== (1, 'a', 2, 3, 4, 6, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).snip_7              ==== (1, 'a', 2, 3, 4, 5, 7, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).snip_8              ==== (1, 'a', 2, 3, 4, 5, 6, 8)
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).cutAt1              ==== (1, ('a', 2, 3, 4, 5, 6, 7, 8))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).cutAt2              ==== ((1, 'a'), (2, 3, 4, 5, 6, 7, 8))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).cutAt3              ==== ((1, 'a', 2), (3, 4, 5, 6, 7, 8))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).cutAt4              ==== ((1, 'a', 2, 3), (4, 5, 6, 7, 8))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).cutAt5              ==== ((1, 'a', 2, 3, 4), (5, 6, 7, 8))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).cutAt6              ==== ((1, 'a', 2, 3, 4, 5), (6, 7, 8))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).cutAt7              ==== ((1, 'a', 2, 3, 4, 5, 6), (7, 8))
    T ~ (1, 'a', 2, 3, 4, 5, 6, 7, 8).cutAt8              ==== ((1, 'a', 2, 3, 4, 5, 6, 7), 8)


  @Test
  def dataCollectionTest(): Unit =
    T ~ Seq(Is(3), Alt("eel"), Is(5)).collectIs     ==== List(3, 5)  --: typed[Seq[Int]]
    T ~ Seq(Is(3), Alt("eel"), Is(5)).collectAlt    ==== List("eel") --: typed[Seq[String]]
    T ~ Array(Is(3), Alt("eel")).collectIs          ==== typed[Array[Int]]
    T ~ Array(Is(3), Alt("eel")).collectIs.head     ==== 3
    T ~ Iterator(Is(3), Alt("eel")).collectAlt      ==== typed[Iterator[String]]
    T ~ Iterator(Is(3), Alt("eel")).collectAlt.next ==== "eel"

    val eparse = nice{ "e".toInt }
    val xs = Vector[Int Or Err](Is(5), eparse, 6.asIs, Err("boo").orIs[Int])
    T ~ xs.valid                        ==== eparse      --: typed[Vector[Int] Or Err]
    T ~ xs.filter(_.isIs).valid         ==== Vector(5, 6)
    T ~ xs.errors                       ==== xs.collect{ case Alt(a) => a }
    T ~ xs.validOrErrors                ==== Alt(xs.collectAlt)
    T ~ xs.filter(_.isIs).validOrErrors ==== Is(xs.collectIs)

    T ~ xs.filter(_.isIs).validOrIndexedResults ==== Vector(5, 6) --: typed[Vector[Int] Or (Vector[Int], Vector[(Int, Err)])]
    T ~ xs.validOrIndexedResults                ==== Alt((Vector(5, 6), Vector((1, xs(1).alt), (3, xs(3).alt))))

    T ~ List("2", "e").validMap(s => nice{ s.toInt }) ==== typed[List[Int] Or Err]
    T ~ List("2", "e").validMap(s => nice{ s.toInt }) ==== runtype[Alt[_]]
    T ~ List("2", "3").validMap(s => nice{ s.toInt }) ==== List(2, 3)


  @Test
  def cacheTest(): Unit =
    var x = 0
    val l = Lazy{ x += 1; x }
    val lm = l.map{ v => x += 1; v + x }
    val lfm = lm.flatMap{ v => x += 1; Lazy{ x *= 2; v + x }}
    T ~ x         ==== 0
    T ~ l.value   ==== 1
    T ~ x         ==== 1
    T ~ lm.value  ==== 3
    T ~ x         ==== 2
    T ~ l.value   ==== 1
    T ~ lfm.value ==== 9
    T ~ x         ==== 6
    T ~ l.value   ==== 1
    T ~ lm.value  ==== 3

    // Not explicitly testing thread safety, but it should be thread-safe.
    val w = Worm.of[String]
    val ww = Worm.of[String]
    val www = Worm.preset("halibut")
    T ~ w.wormAsAtomic ==== typed[java.util.concurrent.atomic.AtomicReference[AnyRef]]
    T ~ w.value        ==== Alt(()) --: typed[String Or Unit]
    T ~ w.get          ==== thrown[IllegalStateException]
    w.set("cod")
    T ~ w.value ==== "cod" --: typed[String Or Unit]
    T ~ w.get   ==== "cod"
    T ~ w.setIfEmpty("herring") ==== false
    T ~ w.set("herring")        ==== thrown[IllegalStateException]
    T ~ w.getOrSet("perch")     ==== "cod" --: typed[String]
    T ~ ww.getOrSet("minnow")   ==== "minnow"
    T ~ ww.getOrSet("eel")      ==== "minnow"
    T ~ www.value               ==== "halibut"

    // Not explicitly testing soft memory clearance, but it should clear memory
    var z = 0
    val s = Soft("salmon"){ s => z += 1; s.length }
    T ~ z       ==== 0
    T ~ s.value ==== 6
    T ~ (z > 0) ==== true
    var zz = 0
    val sm = s.map{ v => zz += 1; z += v; v > 4 }
    T ~ zz ==== 0
    val z0 = z
    T ~ sm.value ==== true
    val z1 = z
    T ~ { (z1 - z0) >= 6 } ==== true
    T ~ (zz > 0)           ==== true
    T ~ s.forget().valueOrValue ==== Alt(6)
    T ~ (z > z1)                ==== true
    val z2 = z
    T ~ s.valueOrValue          ==== Is(6)
    T ~ z2                      ==== z
    T ~ s.forget().valueOrUnit ==== Alt.unit
    T ~ z2                     ==== z
    T ~ s.value                ==== 6
    T ~ (z > z2)               ==== true

    val u = Hold.unit
    T ~ u.getOrUnit   ==== ((), 0) --: typed[(Unit, Long) Or Unit]
    T ~ u.recompute() ==== ((), 0) --: typed[(Unit, Long)]
    T ~ u.force()     ==== ((), 0) --: typed[(Unit, Long)]

    val c = Hold.fixed("cod")
    T ~ c.getOrUnit   ==== ("cod", 0) --: typed[(String, Long) Or Unit]
    T ~ c.recompute() ==== ("cod", 0) --: typed[(String, Long)]
    T ~ c.force()     ==== ("cod", 0) --: typed[(String, Long)]

    var dn = -1
    val d = Hold.unheld{ dn += 1; "dace" }
    T ~ dn            ==== -1
    T ~ d.getOrUnit   ==== Alt.unit --: typed[(String, Long) Or Unit]
    T ~ d.get         ==== ("dace", 0)
    T ~ d.get         ==== ("dace", 1)
    T ~ d.recompute() ==== ("dace", 2)
    T ~ d.force()     ==== ("dace", 3)
    T ~ d.get         ==== ("dace", dn)
    T ~ d.getOrUnit   ==== Alt.unit

    var an = -1
    val a = Hold{ an += 1; "albacore" }
    T ~ an            ==== -1
    T ~ a.getOrUnit   ==== Alt.unit --: typed[(String, Long) Or Unit]
    T ~ a.get         ==== ("albacore", 0)
    T ~ a.get         ==== ("albacore", 0)
    T ~ an            ==== 0
    T ~ a.recompute() ==== ("albacore", 1)
    T ~ a.force()     ==== ("albacore", 2)
    T ~ a.get         ==== ("albacore", 2)
    T ~ a.getOrUnit   ==== ("albacore", 2)
    a.release()
    T ~ a.getOrUnit   ==== Alt.unit
    T ~ a.get         ==== ("albacore", 3)

    val m = Hold.mutable("minnow")
    val m2 = m.map(_.length)
    T ~ m.get        ==== ("minnow", 0)
    T ~ m.getOrUnit  ==== ("minnow", 0)
    T ~ m2.getOrUnit ==== Alt.unit
    T ~ m2.get       ==== (6, 0)
    T ~ m2.force()   ==== (6, 1)
    m.set("mackarel")
    T ~ m.getOrUnit  ==== ("mackarel", 1)
    T ~ m2.getOrUnit ==== Alt.unit
    T ~ m2.get       ==== (8, 2)
    T ~ m2.get       ==== (8, 2)
    m.zap(s => if s == "mackarel" then "minnow" else "mackarel")
    T ~ m2.get       ==== (6, 3)
    T ~ m.get        ==== ("minnow", 2)

    val i = Hold.iterate(0)(_ + 2)
    T ~ i.get         ==== (0, 0)
    T ~ i.getOrUnit   ==== Alt.unit --: typed[(Int, Long) Or Unit]
    T ~ i.recompute() ==== (2, 1)
    T ~ i.force()     ==== (4, 2)
    T ~ i.get         ==== (6, 3)
    T ~ i.getOrUnit   ==== Alt.unit

    var shn = -1
    val sh = Hold.soft{ shn += 1; "shad" }
    T ~ shn          ==== -1
    T ~ sh.getOrUnit ==== Alt.unit --: typed[(String, Long) Or Unit]
    T ~ sh.get       ==== ("shad", 0)
    T ~ sh.value     ==== "shad"
    T ~ sh.force()   ==== ("shad", shn)
    T ~ (shn > 0)    ==== true

    val t2 = Hold.them(i, a)
    T ~ t2.getOrUnit ==== Alt.unit --: typed[((Int, String), Long) Or Unit]
    T ~ t2.get       ==== ((8, "albacore"), 0)
    T ~ t2.get       ==== ((10, "albacore"), 1)
    T ~ t2.force()   ==== ((12, "albacore"), 2)
    T ~ a.get        ==== ("albacore", 4)

    val t3 = Hold.them(c, a, m)
    T ~ t3.getOrUnit ==== Alt.unit --: typed[((String, String, String), Long) Or Unit]
    T ~ t3.get       ==== (("cod", "albacore", "minnow"), 0)
    T ~ t3.get       ==== (("cod", "albacore", "minnow"), 0)
    m.set("mackarel")
    T ~ t3.get       ==== (("cod", "albacore", "mackarel"), 1)
    T ~ t3.force()   ==== (("cod", "albacore", "mackarel"), 2)
    T ~ a.get        ==== ("albacore", 5)
    T ~ m.get        ==== ("mackarel", 3)
    m.set("minnow")
    T ~ t3.get       ==== (("cod", "albacore", "minnow"), 3)

    val t4 = Hold.them(a, a, a, a)
    T ~ t4.getOrUnit   ==== Alt.unit --: typed[((String, String, String, String), Long) Or Unit]
    T ~ t4.get         ==== (("albacore", "albacore", "albacore", "albacore"), 0)
    T ~ t4.get         ==== (("albacore", "albacore", "albacore", "albacore"), 0)
    T ~ t4.recompute() ==== (("albacore", "albacore", "albacore", "albacore"), 1)
    T ~ a.get          ==== ("albacore", 5)
    T ~ t4.force()     ==== (("albacore", "albacore", "albacore", "albacore"), 2)
    T ~ a.get          ==== ("albacore", 6)

    val t5 = Hold.them(u, c, a, m, m2)
    T ~ t5.getOrUnit   ==== Alt.unit --: typed[((Unit, String, String, String, Int), Long) Or Unit]
    T ~ t5.get         ==== (((), "cod", "albacore", "minnow", 6), 0)
    T ~ m2.get         ==== (6, 4)
    T ~ t5.get         ==== (((), "cod", "albacore", "minnow", 6), 0)
    T ~ t5.recompute() ==== (((), "cod", "albacore", "minnow", 6), 1)
    m2.recompute()
    T ~ t5.get         ==== (((), "cod", "albacore", "minnow", 6), 2)
    T ~ m.get          ==== ("minnow", 4)
    m.set("mackarel")
    T ~ t5.get         ==== (((), "cod", "albacore", "mackarel", 8), 3)
    T ~ t5.force()     ==== (((), "cod", "albacore", "mackarel", 8), 4)
    T ~ m.get          ==== ("mackarel", 5)
    T ~ m2.get         ==== (8, 7)
    T ~ t3.get         ==== (("cod", "albacore", "mackarel"), 4)
    T ~ a.get          ==== ("albacore", 7)

    val t6 = Hold.them(m2, m2, m2, m2, i, u)
    T ~ t6.getOrUnit ==== Alt.unit --: typed[((Int, Int, Int, Int, Int, Unit), Long) Or Unit]
    T ~ t6.get       ==== ((8, 8, 8, 8, 14, ()), 0)
    T ~ t6.get       ==== ((8, 8, 8, 8, 16, ()), 1)
    T ~ t6.force()   ==== ((8, 8, 8, 8, 18, ()), 2)
    m.set("minnow")
    T ~ t6.get       ==== ((6, 6, 6, 6, 20, ()), 3)
    T ~ t6.getOrUnit ==== Alt.unit
    T ~ m2.get       ==== (6, 9)
    T ~ i.get        ==== (22, 11)

    val ah = Hold.array(Array(m2, Hold.fixed(1)))
    T ~ ah.getOrUnit      ==== Alt.unit --: typed[(Array[Int], Long) Or Unit]
    T ~ ah.value          =**= Array(6, 1)
    T ~ ah.get._2         ==== 0
    T ~ ah.getOrUnit.isIs ==== true
    T ~ ah.force()._2     ==== 1
    T ~ ah.value          =**= Array(6, 1)
    m.set("mackarel")
    T ~ ah.value          =**= Array(8, 1)
    T ~ m2.get            ==== (8, 11)

    val ti = i.trust(3)
    T ~ ti.getOrUnit ==== Alt.unit --: typed[(Int, Long) Or Unit]
    T ~ ti.get       ==== (24, 0)
    T ~ ti.get       ==== (24, 0)
    T ~ ti.getOrUnit ==== (24, 0) --: typed[(Int, Long) Or Unit]
    T ~ ti.get       ==== (26, 1)
    T ~ ti.force()   ==== (28, 2)
    T ~ ti.get       ==== (28, 2)
    T ~ ti.get       ==== (28, 2)
    T ~ ti.get       ==== (30, 3)
    T ~ i.get        ==== (32, 16)
    T ~ ti.get       ==== (30, 3)
    T ~ ti.get       ==== (30, 3)
    T ~ ti.getOrUnit ==== Alt.unit
    T ~ ti.get       ==== (34, 4)

    val di = i.trust(java.time.Duration.ofMillis(100))
    T ~ di.getOrUnit ==== Alt.unit --: typed[(Int, Long) Or Unit]
    T ~ di.get       ==== (36, 0)
    T ~ di.get       ==== (36, 0)
    T ~ di.getOrUnit ==== (36, 0) --: typed[(Int, Long) Or Unit]
    Thread.sleep(200)
    T ~ di.get       ==== (38, 1)
    Thread.sleep(200)
    T ~ di.getOrUnit ==== Alt.unit
    T ~ di.get       ==== (40, 2)
    T ~ i.get        ==== (42, 21)

    val tt = i.trust()
    T ~ tt.getOrUnit   ==== Alt.unit --: typed[(Int, Long) Or Unit]
    T ~ tt.get         ==== (44, 0)
    T ~ tt.recompute() ==== (46, 1)
    T ~ tt.get         ==== (46, 1)
    T ~ tt.getOrUnit   ==== (46, 1) --: typed[(Int, Long) Or Unit]
    T ~ i.get          ==== (48, 24)

    val xa = a.expireIn(2)
    T ~ xa.getOrUnit   ==== ("albacore", 7)
    T ~ a.get          ==== ("albacore", 7)
    T ~ xa.get         ==== ("albacore", 7)
    T ~ a.get          ==== ("albacore", 7)
    T ~ xa.getOrUnit   ==== Alt.unit --: typed[(String, Long) Or Unit]
    T ~ xa.get         ==== ("albacore", 8)
    T ~ a.get          ==== ("albacore", 8)
    T ~ xa.getOrUnit   ==== ("albacore", 8) --: typed[(String, Long) Or Unit]
    T ~ xa.get         ==== ("albacore", 9)
    T ~ xa.recompute() ==== ("albacore", 10)
    T ~ xa.recompute() ==== ("albacore", 11)
    T ~ xa.recompute() ==== ("albacore", 12)
    T ~ a.get          ==== ("albacore", 12)
    T ~ xa.force()     ==== ("albacore", 13)
    T ~ a.force()      ==== ("albacore", 14)
    T ~ xa.get         ==== ("albacore", 14)
    T ~ xa.get         ==== ("albacore", 15)

    val ya = a.expireIn(java.time.Duration.ofMillis(100))
    T ~ ya.getOrUnit ==== ("albacore", 15) --: typed[(String, Long) Or Unit]
    T ~ ya.get       ==== ("albacore", 15)
    T ~ ya.get       ==== ("albacore", 15)
    Thread.sleep(200)
    T ~ ya.getOrUnit ==== Alt.unit --: typed[(String, Long) Or Unit]
    T ~ ya.get       ==== ("albacore", 16)
    T ~ a.force()    ==== ("albacore", 17)
    T ~ ya.get       ==== ("albacore", 17)
    T ~ ya.get       ==== ("albacore", 17)
    Thread.sleep(200)
    T ~ ya.get       ==== ("albacore", 18)
    T ~ a.get        ==== ("albacore", 18)

    var expirer = "cod"
    val za = a.expireIf((v, _) => v == expirer)
    T ~ za.getOrUnit ==== ("albacore", 18) --: typed[(String, Long) Or Unit]
    T ~ za.get       ==== ("albacore", 18)
    expirer = "albacore"
    T ~ za.get       ==== ("albacore", 19)
    T ~ za.getOrUnit ==== Alt.unit --: typed[(String, Long) Or Unit]
    T ~ za.get       ==== ("albacore", 20)
    expirer = "cod"
    T ~ za.get       ==== ("albacore", 21)
    T ~ a.get        ==== ("albacore", 21)

    val pa = a.protect()
    T ~ pa.getOrUnit   ==== ("albacore", 21)
    T ~ pa.force()     ==== ("albacore", 21)
    T ~ pa.recompute() ==== ("albacore", 21)
    T ~ pa.get         ==== ("albacore", 21)
    T ~ a.get          ==== ("albacore", 21)

    val smm = m.softMap(_.length)
    T ~ smm.getOrUnit ==== Alt.unit --: typed[(Int, Long) Or Unit]
    T ~ smm.get       ==== (8, 0)
    T ~ smm.getOrUnit ==== (8, 0) --: typed[(Int, Long) Or Unit]
    m.set("salmon")
    T ~ smm.getOrUnit ==== Alt.unit
    T ~ smm.get       ==== (6, 1)

    val wm = a.mapWith(m2)((x, y) => x.take(y))
    T ~ wm.getOrUnit  ==== Alt.unit --: typed[(String, Long) Or Unit]
    T ~ wm.get        ==== ("albaco", 0)
    T ~ wm.get        ==== ("albaco", 0)
    T ~ a.recompute() ==== ("albacore", 22)
    T ~ wm.get        ==== ("albaco", 1)
    m.set("mackarel")
    T ~ wm.getOrUnit  ==== Alt.unit --: typed[(String, Long) Or Unit]
    T ~ wm.get        ==== ("albacore", 2)
    T ~ wm.force()    ==== ("albacore", 3)
    T ~ a.get         ==== ("albacore", 23)
    m.set("minnow")
    T ~ m2.get        ==== (6, 15)

    val swm = a.softMapWith(m2)((x, y) => x.take(y))
    T ~ swm.getOrUnit ==== Alt.unit --: typed[(String, Long) Or Unit]
    T ~ swm.get       ==== ("albaco", 0)
    T ~ swm.get       ==== ("albaco", 0)
    T ~ a.recompute() ==== ("albacore", 24)
    T ~ swm.get       ==== ("albaco", 1)
    m.set("mackarel")
    T ~ swm.getOrUnit ==== Alt.unit --: typed[(String, Long) Or Unit]
    T ~ swm.get       ==== ("albacore", 2)
    T ~ swm.force()   ==== ("albacore", 3)
    T ~ a.get         ==== ("albacore", 25)
    m.set("minnow")
    T ~ m2.get        ==== (6, 18)


  @Test
  def resourceTest: Unit =
    val m = Mu(0)
    T ~ Resource(m)(_.zap(_ + 1)){ x => x.set(2); x.value + 2 } ==== 4
    T ~ m.value                                                 ==== 3
    T ~ Resource(m)(_.zap(_ * 2)){ x => throw new Exception("oops"); () } ==== thrown[Exception]
    T ~ m.value                                                 ==== 6
    T ~ Err.Or[Int]{ Resource(m)(_.zap(- _)){ x => if x.value > 0 then Is.break(x.value) else if x.value < 0 then Err.break("negative") else 0 } } ==== 6 --: typed[Int Or Err]
    T ~ m.value                                                 ==== -6

    T ~ Resource.safe(m)(_.zap(_ + 1)){ x => x.set(2); x.value + 2 } ==== 4  --: typed[Int Or Throwable]
    T ~ m.value                                                      ==== 3
    T ~ Resource.safe(m)(_.zap(_ * 2)){ x => 
          throw new Exception("oops"); ()
        }.existsAlt(_.isInstanceOf[Exception])                       ==== true
    T ~ m.value                                                      ==== 6
    T ~ Or.FlatRet{
          Resource.safe(m)(_.zap(- _)){ x => 
            if x.value > 0 then Is.break(x.value)
            else if x.value < 0 then Err.break("negative")
            else 0
          }.mapAlt(Err apply _)
        }                                                            ==== 6 --: typed[Int Or Err]
    T ~ m.value                                                      ==== -6
    T ~ Resource.safe(m){ x =>
          throw new Exception("oops"); x.zap(_ * 2)
        }{ x => 
          x.zap(_ * 3); x.value
        }.existsAlt(_.isInstanceOf[Exception])                       ==== true
    T ~ m.value                                                      ==== -18

    T ~ Resource.nice(m)(_.zap(_ + 1)){ x => x.set(2); x.value + 2 } ==== 4  --: typed[Int Or Err]
    T ~ m.value                                                      ==== 3
    T ~ Resource.nice(m)(_.zap(_ * 2)){ x => 
          throw new Exception("oops"); ()
        }.existsAlt(_.toString contains "oops")                      ==== true
    T ~ m.value                                                      ==== 6
    T ~ Or.FlatRet{
          Resource.nice(m)(_.zap(- _)){ x => 
            if x.value > 0 then Is.break(x.value)
            else if x.value < 0 then Err.break("negative")
            else 0
          }
        }                                                            ==== 6
    T ~ m.value                                                      ==== -6
    T ~ Resource.nice(m){ x =>
          throw new Exception("oops"); x.zap(_ * 2)
        }{ x => 
          x.zap(_ * 3); x.value
        }.existsAlt(_.toString contains "closing resource")          ==== true
    T ~ m.value                                                      ==== -18
}
object FlowTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
