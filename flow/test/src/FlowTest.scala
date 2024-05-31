// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-24 Rex Kerr and Calico Life Sciences, LLC.

package kse.test.flow


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
  import kse.basics.{given, _}
  import kse.flow.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
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

    def erm(s: String): Int Or Err = Err.Or:
      if s.isEmpty then Err ?# "empty"
      else s.length
    T ~ erm("")    ==== Alt(Err("empty"))
    T ~ erm("eel") ==== 3 --: typed[Int Or Err]

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
    T ~ Alt.F    ==== Alt(false)
    T ~ Alt.T    ==== Alt(true)

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
    T ~ Is.F         ==== Is(false)
    T ~ Is.T         ==== Is(true)

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

    val fynn: (Int Or String) Or String = Is(Is(5))
    val fnyn: (Int Or String) Or String = Is(Alt("eel"))
    val fnny: (Int Or String) Or String = Alt("cod")
    val fann: (Any Or String) Or String = Is(Is(Alt("bass"): Any))
    T ~ fynn.flatten ==== 5  --: typed[Int Or String]
    T ~ fnyn.flatten ==== Alt("eel")
    T ~ fnny.flatten ==== Alt("cod")
    T ~ fann.flatten ==== Is(Alt("bass"))
    T ~ fann.flatten.isIs  ==== true
    T ~ fann.flatten.isAlt ==== false

    var n = 0
    T ~ { for i <- oi do n += i; n          } ==== 5
    T ~ { for i <- oa do n += i; n          } ==== 5
    T ~ { for i <- oi if i > 9 do n += 1; n } ==== 5
    T ~ { for i <- oi yield i + 1 }           ==== 6  --: typed[Int Or String]
    T ~ { for i <- oi if i > 0 yield i + 1 }  ==== 6  --: typed[Int Or (String | Unit)]
    T ~ { for i <- oi if i > 9 yield i + 1 }  ==== Alt.unit
    T ~ { for i <- oa if i > 9 yield i + 1 }  ==== Alt("cod")

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
    T ~ a.toTry.get  ==== thrown[WrongBranchException[?]]
    T ~ oi.toTry.get ==== 5
    T ~ oa.toTry.get ==== thrown[WrongBranchException[?]]
    T ~ on.toTry.get ==== null
    T ~ om.toTry.get ==== thrown[WrongBranchException[?]]

    T ~ i.swapToTry.get  ==== thrown[WrongBranchException[?]]
    T ~ a.swapToTry.get  ==== "cod"
    T ~ oi.swapToTry.get ==== thrown[WrongBranchException[?]]
    T ~ oa.swapToTry.get ==== "cod"
    T ~ on.swapToTry.get ==== thrown[WrongBranchException[?]]
    T ~ om.swapToTry.get ==== null



  @Test
  def flowTest(): Unit =
    def orQ1(s: String): Int Or String = Or.Ret:
      s.isIf(_.forall(_.isDigit)).?.toInt
    T ~ orQ1("salmon") ==== Alt("salmon") --: typed[Int Or String]
    T ~ orQ1("5")      ==== 5             --: typed[Int Or String]

    def orQ2(s: String): Int Or String = Or.FlatRet:
      s.isIf(_.forall(_.isDigit)).?.isIf(s => s.length >= 1 && s.length <= 9).map(_.toInt)
    T ~ orQ2("1234567890") ==== Alt("1234567890") --: typed[Int Or String]
    T ~ orQ2("4")          ==== 4                 --: typed[Int Or String]
    T ~ orQ2("cod")        ==== Alt("cod")        --: typed[Int Or String]

    def orQ3(s: String): Int Or String = Or.Ret:
      s.altCase{ case x if x.exists(! _.isDigit) => x.exists(_.isDigit) }.?+(b => s"Has digits: $b").toInt
    T ~ orQ3("herring") ==== Alt("Has digits: false") --: typed[Int Or String]
    T ~ orQ3("5 eels")  ==== Alt("Has digits: true")  --: typed[Int Or String]
    T ~ orQ3("14")      ==== 14                       --: typed[Int Or String]

    def orQ4(s: String): Int Or String = Or.Ret:
      given AutoMap[String Or String, String] = _.fold{ x => s"Too long: $x" }{ y => s"Non-numeric: $y" }
      s.isIf(_.forall(_.isDigit)).alsoDiscard{ case s if s.length < 1 || s.length > 9 => s}.?*.toInt
    T ~ orQ4("perch")      ==== Alt("Non-numeric: perch")   --: typed[Int Or String]
    T ~ orQ4("1234567890") ==== Alt("Too long: 1234567890") --: typed[Int Or String]
    T ~ orQ4("225")        ==== 225                         --: typed[Int Or String]

    def orQ5(s: String): Int Or String = Or.Safe(_.getMessage):
      s.isIf(_.nonEmpty).?.toInt
    T ~ orQ5("perch").existsAlt(_ contains "perch") ==== true
    T ~ orQ5("")                                    ==== Alt("")
    T ~ orQ5("15")                                  ==== 15      --: typed[Int Or String]

    def orQ6(s: String): Int Or Err = Or.Nice:
      s.isIf(_.nonEmpty).?+(Err apply _).toInt
    T ~ orQ6("perch").existsAlt(_.toString contains "NumberF") ==== true
    T ~ orQ6("")                                               ==== Alt("")
    T ~ orQ6("1815")                                           ==== 1815    --: typed[Int Or Err]

    def orQ7(s: String): Int Or Err = Err.Or:
      s.isIf(_.nonEmpty).?.toInt
    T ~ orQ7("perch").existsAlt(_.toString contains "NumberF") ==== true
    T ~ orQ7("")                                               ==== Alt(Err(""))
    T ~ orQ7("1858")                                           ==== 1858 --: typed[Int Or Err]

    def orQ8(s: String): Int Or Err = Err.FlatOr:
      s.isIf(_.nonEmpty).?.toInt.altCase{ case i if i < 0 => Err(s"Negative: $i") }
    T ~ orQ8("perch").existsAlt(_.toString contains "NumberF") ==== true
    T ~ orQ8("")                                               ==== Alt(Err(""))
    T ~ orQ8("1858")                                           ==== 1858 --: typed[Int Or Err]
    T ~ orQ8("-3")                                             ==== Alt(Err("Negative: -3"))

    def orQ9(s: String): Int Or Err = Err.Or:
      orQ7(s) ?# "Parse error"
    T ~ orQ9("perch").alt.toString.startsWith("Parse error")  ==== true
    T ~ orQ9("").existsAlt(_.isInstanceOf[ErrType.Explained]) ==== true
    T ~ orQ9("1858")                                          ==== 1858 --: typed[Int Or Err]

    def floatQ(f: Float) = calculate:
      val g = f.?
      if g < 0 then -math.sqrt(-g).toFloat
      else math.sqrt(g - 1).toFloat.? + 1.0
    T ~ floatQ(Float.NaN) =~~= Double.NaN
    T ~ floatQ(-1f)       =~~= -1.0
    T ~ floatQ(0.5f)      =~~= Double.NaN
    T ~ floatQ(2f)        =~~= 2.0

    def doubleQ(f: Double) = calculate:
      val g = f.?
      if g < 0 then -math.sqrt(-g)
      else math.sqrt(g - 1).? + 1
    T ~ doubleQ(Double.NaN) =~~= Double.NaN
    T ~ doubleQ(-1f)        =~~= -1.0
    T ~ doubleQ(0.5f)       =~~= Double.NaN
    T ~ doubleQ(5f)         =~~= 3.0

    def eitherQ1(s: String): Either[String, Int] = Either.Ret:
      val e: Either[String, Int] =
        if s.forall(_.isDigit) then Right(s.toInt)
        else Left(s)
      e.? + 1
    T ~ eitherQ1("minnow") ==== Left("minnow") --: typed[Either[String, Int]]
    T ~ eitherQ1("55")     ==== Right(56)      --: typed[Either[String, Int]]

    def eitherQ2(s: String): Either[String, Int] = Either.FlatRet:
      val e: Either[String, Int] =
        if s.forall(_.isDigit) then Right(s.toInt)
        else Left(s)
      e.? match
        case x if x < 10 => Left(s"Bad $x")
        case x           => Right(x + 2)
    T ~ eitherQ2("minnow") ==== Left("minnow") --: typed[Either[String, Int]]
    T ~ eitherQ2("55")     ==== Right(57)      --: typed[Either[String, Int]]
    T ~ eitherQ2("4")      ==== Left("Bad 4")  --: typed[Either[String, Int]]

    def optionQ1(s: String): Option[Int] = Option.Ret[Int]:
      Option(s).filter(_.forall(_.isDigit)).?.toInt
    T ~ optionQ1("herring") ==== None     --: typed[Option[Int]]
    T ~ optionQ1("8")       ==== Some(8)

    def optionQ2(s: String): Option[Int] = Option.FlatRet:
      Option(s).filter(_.forall(_.isDigit)).?.toInt match
        case x if x < 10 => None
        case x           => Some(x)
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
    T ~ 17.altIf(x => x % 2 != 0).grab                           ==== thrown[WrongBranchException[?]]

    {
      var k = 0
      T ~ escape{ k += 1; (k > 1).?; k += 1 } ==== false
      T ~ escape{ k += 1; (k > 1).?; k += 1 } ==== true
      T ~ k ==== 3
      T ~ { loop{ k += 1; (k < 10).? }; k } ==== 10
    }

    T ~ attempt( optionQ1("eel").! ).default(0)                               ==== 0
    T ~ attempt( optionQ1("888").! ).default(0)                               ==== 888
    T ~ attempt{ val n = optionQ1("eel").!; ensure(n > 9); n - 1 }.default(0) ==== 0
    T ~ attempt{ val n = optionQ1("888").!; ensure(n > 9); n - 1 }.default(0) ==== 887
    T ~ attempt{ val n = optionQ1(  "5").!; ensure(n > 9); n - 1 }.default(0) ==== 0
    T ~ attempt( eitherQ1("eel").! ).default(0)                               ==== 0
    T ~ attempt( eitherQ1("888").! ).default(0)                               ==== 889
    T ~ attempt( Try("eel".toInt).! ).default(0)                              ==== 0
    T ~ attempt( Try("888".toInt).! ).default(0)                              ==== 888
    T ~ attempt( orQ1("eel").! ).default(0)                                   ==== 0
    T ~ attempt( orQ1("888").! ).default(0)                                   ==== 888
    val itn = Iterator(5)
    T ~ attempt( itn.! ).default(0)                                           ==== 5
    T ~ attempt( itn.! ).default(0)                                           ==== 0
    val stn = Array(5).stepper
    T ~ attempt( stn.! ).default(0)                                           ==== 5
    T ~ attempt( stn.! ).default(0)                                           ==== 0
    val jtn = java.util.Arrays.stream(Array(5)).iterator
    T ~ attempt( jtn.! ).default(0)                                           ==== 5
    T ~ attempt( jtn.! ).default(0)                                           ==== 0
    val enn = new java.util.Enumeration[Int]() { 
      private var i = 5
      def hasMoreElements = i > 0
      def nextElement = { val ans = i; i = 0; ans } 
    }
    T ~ attempt( enn.! ).default(0)                                           ==== 5
    T ~ attempt( enn.! ).default(0)                                           ==== 0
    T ~ attempt.safe{ "eel".toInt }.default(0)                                ==== 0
    T ~ attempt.safe{ "888".toInt }.default(0)                                ==== 888
    T ~ threadsafe{ attempt.safe{ toss(); 5 }.default(0) }.grab               ==== thrown[ErrType.CatchableException]
    T ~ attempt.threadsafe{ "eel".toInt }.default(0)                          ==== 0
    T ~ attempt.threadsafe{ "888".toInt }.default(0)                          ==== 888
    T ~ attempt.threadsafe{ toss(); 5   }.default(0)                          ==== 0
    T ~ "eel".attemptCase{ case s if s.length > 3 => s.length }.default(0)    ==== 0
    T ~ "bass".attemptCase{ case s if s.length > 3 => s.length }.default(0)   ==== 4
    T ~ attempt(enn.!).orCase("eel"){ case "bass" => 1 }.default(0)           ==== 0
    T ~ attempt(enn.!).orCase("bass"){ case "bass" => 1 }.default(0)          ==== 1
    T ~ attempt{ "eel".inCase{ case "bass" => 1 } }.default(0)                ==== 0
    T ~ attempt{ "bass".inCase{ case "bass" => 1 } }.default(0)               ==== 1
    T ~ attempt(enn.!).safeCase("eel"(3)){ case 's' => 1 }.default(0)         ==== 0
    T ~ attempt(enn.!).safeCase("bass"(3)){ case 's' => 1 }.default(0)        ==== 1

    def jasi(s: String): java.util.Iterator[String] =
      java.util.Arrays.stream(s.copyOp( (c, _) => c.toString)).iterator
    def jemn(n: Int): java.util.Enumeration[Int] = new java.util.Enumeration[Int]() { 
      private var i = 5 - n
      def hasMoreElements = i > 0
      def nextElement = { val ans = i; i -= 1; ans } 
    }
    T ~ Array("8", "x", "9").breakable.copyOp{ (s, _) =>  optionQ1(s).orSkip } =**= Array(8, 9)
    T ~ Array("8", "x", "9").breakable.copyOp{ (s, _) =>  optionQ1(s).orQuit } =**= Array(8)
    T ~ Array("8", "x", "9").breakable.copyOp{ (s, i) =>  eitherQ1(s).orSkip } =**= Array(9, 10)
    T ~ Array("8", "x", "9").breakable.copyOp{ (s, _) =>  eitherQ1(s).orQuit } =**= Array(9)
    T ~ Array("8", "x", "9").breakable.copyOp{ (s, _) =>      orQ1(s).orSkip } =**= Array(8, 9)
    T ~ Array("8", "x", "9").breakable.copyOp{ (s, _) =>      orQ1(s).orQuit } =**= Array(8)
    T ~ Array("8", "x", "9").breakable.copyOp{ (s, _) => Try(s.toInt).orSkip } =**= Array(8, 9)
    T ~ Array("8", "x", "9").breakable.copyOp{ (s, _) => Try(s.toInt).orQuit } =**= Array(8)
    T ~ Array(2, 8, 3).breakable.copyOp{ (n, _) => "salmon".drop(n).iterator.orSkip        }.str ==== "lm"
    T ~ Array(2, 8, 3).breakable.copyOp{ (n, _) => "salmon".drop(n).iterator.orQuit        }.str ==== "l"
    T ~ Array(2, 8, 3).breakable.copyOp{ (n, _) => "salmon".drop(n).stepper .orSkip.toChar }.str ==== "lm"
    T ~ Array(2, 8, 3).breakable.copyOp{ (n, _) => "salmon".drop(n).stepper .orQuit.toChar }.str ==== "l"
    T ~ Array(2, 8, 3).breakable.copyOp{ (n, _) => "salmon".drop(n).fn(jasi).orSkip.head   }.str ==== "lm"
    T ~ Array(2, 8, 3).breakable.copyOp{ (n, _) => "salmon".drop(n).fn(jasi).orQuit.head   }.str ==== "l"
    T ~ Array(2, 8, 3).breakable.copyOp{ (n, _) => jemn(n).orSkip }                              =**= Array(3, 2)
    T ~ Array(2, 8, 3).breakable.copyOp{ (n, _) => jemn(n).orQuit }                              =**= Array(3)

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
      case Failure(e) => T ~ e.isInstanceOf[WrongBranchException[?]] ==== true
      case _          => T("Success when failure expected") ~ false  ==== true
    var ou: Int = 2
    T ~ os.use(ou += _.length) ==== os
    T ~ ou                     ==== 9

    val pf: PartialFunction[Int, String] = { case x if x > 0 && x < 5 => "!"*x }
    val orf = (x: Int) => { if x > 0 & x < 5 then Is("!" * x) else Alt.unit }
    val opf = (i: Int) => orf(i).toOption
    val eif = (i: Int) => orf(i).toEither
    val trf = (i: Int) => orf(i).toTry
    val d = (i: Int) => "..."
    T ~ pf.applyOr(3)                ==== "!!!"
    T ~ pf.applyOr(7)                ==== Alt.unit
    T ~ pf.liftToOr                  ==== typed[Int => (String Or Unit)]
    T ~ pf.liftToOr(3)               ==== "!!!"
    T ~ pf.liftToOr(7)               ==== Alt.unit
    T ~ orf.unlift.isDefinedAt(3)    ==== true
    T ~ orf.unlift.isDefinedAt(7)    ==== false
    T ~ orf.unlift(3)                ==== "!!!"
    T ~ orf.unlift(7)                ==== thrown[NoSuchElementException]
    T ~ orf.unlift.applyOrElse(3, d) ==== "!!!"
    T ~ orf.unlift.applyOrElse(7, d) ==== "..."
    T ~ pf.applyOption(3)            ==== Some("!!!")
    T ~ pf.applyOption(7)            ==== None
    T ~ pf.liftToOption              ==== typed[Int => Option[String]]
    T ~ pf.liftToOption(3)           ==== Some("!!!")
    T ~ pf.liftToOption(7)           ==== None
    T ~ opf.unlift.isDefinedAt(3)    ==== true
    T ~ opf.unlift.isDefinedAt(7)    ==== false
    T ~ opf.unlift(3)                ==== "!!!"
    T ~ opf.unlift(7)                ==== thrown[MatchError]
    T ~ opf.unlift.applyOrElse(3, d) ==== "!!!"
    T ~ opf.unlift.applyOrElse(7, d) ==== "..."
    T ~ pf.liftToEither              ==== typed[Int => Either[Unit, String]]
    T ~ pf.liftToEither(3)           ==== Right("!!!")
    T ~ pf.liftToEither(7)           ==== Left(())
    T ~ eif.unlift.isDefinedAt(3)    ==== true
    T ~ eif.unlift.isDefinedAt(7)    ==== false
    T ~ eif.unlift(3)                ==== "!!!"
    T ~ eif.unlift(7)                ==== thrown[NoSuchElementException]
    T ~ eif.unlift.applyOrElse(3, d) ==== "!!!"
    T ~ eif.unlift.applyOrElse(7, d) ==== "..."
    T ~ pf.liftToTry                 ==== typed[Int => Try[String]]
    T ~ pf.liftToTry(3)              ==== Success("!!!")
    T ~ pf.liftToTry(7).isFailure    ==== true
    T ~ trf.unlift.isDefinedAt(3)    ==== true
    T ~ trf.unlift.isDefinedAt(7)    ==== false
    T ~ trf.unlift(3)                ==== "!!!"
    T ~ trf.unlift(7)                ==== thrown[WrongBranchException[?]]
    T ~ trf.unlift.applyOrElse(3, d) ==== "!!!"
    T ~ trf.unlift.applyOrElse(7, d) ==== "..."

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
      var x = ""
      boundary:
        fish.foreach{ y =>
          x += pf.applyOrBreak(y.length)
        }
      x
    } ==== "!!!!!!"

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

    T("breakOnIs didn't break") ~ {
      val or = "cod".orAlt[Boolean]
      boundary[String Or Int]:
        or.breakOnIs
        Alt(5)
    } ==== "cod" --: typed[String Or Int]

    T("breakOnIs broke") ~ {
      val or = true.orIs[String]
      boundary[String Or Int]:
        or.breakOnIs
        Alt(5)
    } ==== Alt(5) --: typed[String Or Int]

    T("breakOnAlt broke") ~ {
      val or = "cod".orAlt[Boolean]
      boundary[Int Or Boolean]:
        or.breakOnAlt
        Is(5)
    } ==== 5 --: typed[Int Or Boolean]

    T("breakOnAlt didn't break") ~ {
      val or = true.orIs[String]
      boundary[Int Or Boolean]:
        or.breakOnAlt
        Is(5)
    } ==== Alt(true) --: typed[Int Or Boolean]


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
  def arrayDataTest(): Unit =
    var used = 0

    val bb = Array[Byte](1, 3, 2, 2, 4)
    T ~ bb.zap(2)(b=>(b-1).toByte)  =**= Array[Byte](1, 3, 1, 2, 4)
    T ~ bb.use(3)(used += _)        =**= Array[Byte](1, 3, 1, 2, 4)
    T ~ used                        ==== 2

    val bs = Array[Short](1, 3, 2, 2, 4)
    T ~ bs.zap(2)(s=>(s-1).toShort) =**= Array[Short](1, 3, 1, 2, 4)
    T ~ bs.use(3)(used += _)        =**= Array[Short](1, 3, 1, 2, 4)
    T ~ used                        ==== 4

    val bc = Array[Char]('0', '2', 'E', 'E', '3')
    T ~ bc.zap(2)(_.toLower)        =**= Array[Char]('0', '2', 'e', 'E', '3')
    T ~ bc.use(3)(used += _ - 'A')  =**= Array[Char]('0', '2', 'e', 'E', '3')
    T ~ used                        ==== 8

    val bi = Array[Int](1, 3, 2, 2, 4)
    T ~ bi.zap(2)(_ - 1)            =**= Array[Int](1, 3, 1, 2, 4)
    T ~ bi.use(3)(used += _)        =**= Array[Int](1, 3, 1, 2, 4)
    T ~ used                        ==== 10

    val bl = Array[Long](1, 3, 2, 2, 4)
    T ~ bl.zap(2)(_ - 1)            =**= Array[Long](1, 3, 1, 2, 4)
    T ~ bl.use(3)(used += _.toInt)  =**= Array[Long](1, 3, 1, 2, 4)
    T ~ used                        ==== 12

    val bf = Array[Float](1, 3, 2, 2, 4)
    T ~ bf.zap(2)(_ - 1)            =**= Array[Float](1, 3, 1, 2, 4)
    T ~ bf.use(3)(used += _.toInt)  =**= Array[Float](1, 3, 1, 2, 4)
    T ~ used                        ==== 14

    val bd = Array[Double](1, 3, 2, 2, 4)
    T ~ bd.zap(2)(_ - 1)            =**= Array[Double](1, 3, 1, 2, 4)
    T ~ bd.use(3)(used += _.toInt)  =**= Array[Double](1, 3, 1, 2, 4)
    T ~ used                        ==== 16

    val ba = Array[String]("0", "2", "E", "E", "3")
    T ~ ba.zap(2)(_.toLowerCase)    =**= Array[String]("0", "2", "e", "E", "3")
    T ~ ba.use(2)(used += _.length) =**= Array[String]("0", "2", "e", "E", "3")
    T ~ used                        ==== 17


  @Test
  def dataCollectionTest(): Unit =
    T ~ Seq(Is(3), Alt("eel"), Is(5)).collectIs     ==== List(3, 5)  --: typed[Seq[Int]]
    T ~ Seq(Is(3), Alt("eel"), Is(5)).collectAlt    ==== List("eel") --: typed[Seq[String]]
    T ~ Seq(Is(3), Alt("eel"), Is(5)).collectThem   ==== (List(3, 5), List("eel"))
    T ~ Array(Is(3), Alt("eel")).collectIs          ==== typed[Array[Int]]
    T ~ Array(Is(3), Alt("eel")).collectIs.head     ==== 3
    T ~ Array(Is(3), Alt("eel")).collectAlt.head    ==== "eel"
    T ~ Array(Is(3), Alt("eel")).collectThem._1     =**= Array(3)
    T ~ Array(Is(3), Alt("eel")).collectThem._2     =**= Array("eel")
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

    val as = xs.toArray
    T ~ as.valid                        ==== eparse      --: typed[Array[Int] Or Err]
    T ~ as.filter(_.isIs).valid.get     =**= Array(5, 6)
    T ~ as.errors                       =**= as.collect{ case Alt(a) => a }
    T ~ as.validOrErrors.alt            =**= as.collectAlt
    T ~ as.filter(_.isIs).validOrErrors.get         =**= xs.collectIs
    T ~ as.filter(_.isIs).validOrIndexedResults.get =**= Array(5, 6)
    T ~ as.validOrIndexedResults.alt.ops(_.toList, _.toList) ==== (List(5, 6), List((1, xs(1).alt), (3, xs(3).alt)))

    T ~ List("2", "e").validMap(s => nice{ s.toInt }) ==== typed[List[Int] Or Err]
    T ~ List("2", "e").validMap(s => nice{ s.toInt }) ==== runtype[Alt[?]]
    T ~ List("2", "3").validMap(s => nice{ s.toInt }) ==== List(2, 3)
    T ~ Array("2", "e").validMap(s => nice{ s.toInt }) ==== typed[Array[Int] Or Err]
    T ~ Array("2", "e").validMap(s => nice{ s.toInt }) ==== runtype[Alt[?]]
    T ~ Array("2", "3").validMap(s => nice{ s.toInt }).get =**= Array(2, 3)

    val linedC = "salmon\ncod\r\nherring\rbass\nperch\n".toCharArray
    val linedB = linedC.copyWith(_.toByte)
    val linedV = Vector("salmon", "cod", "herring", "bass", "perch")
    T ~ linedC.lines().asCopyingIterator.map(_.str).toVector ==== linedV
    T ~ linedB.textLines().asIterator.map{ case (a, iv) => new String(a, iv.i0, iv.length) }.toVector ==== linedV
    T ~ linedC.lines(13 to End-3).asCopyingIterator.map(_.str).toVector ==== Vector("erring", "bass", "per")
    T ~ linedB.textLines(13 to End-3).asIterator.map{ case (a, iv) => new String(a, iv.i0, iv.length) }.toVector ==== Vector("erring", "bass", "per")


  @Test
  def loomTest(): Unit =
    val dt = new java.util.concurrent.atomic.AtomicLong(0L)
    def time[A](t: => A): A =
      val t0 = System.nanoTime
      val ans = t
      val t1 = System.nanoTime
      dt.set(t1 - t0)
      ans
    def yikes(s: String): Nothing =
      throw new Exception(s)
    extension (ai: java.util.concurrent.atomic.AtomicInteger)
      def ++ : Unit = ai.getAndIncrement
    val n = java.util.concurrent.atomic.AtomicInteger(0)
    T ~ Fu.of{ n.++; "eel" }.ask()  ==== "eel" --: typed[String Or Err]
    T ~ n.get                       ==== 1
    T ~ Fu{ n.++; Is("eel") }.ask() ==== "eel" --: typed[String Or Err]
    T ~ n.get                       ==== 2
    val fex = Fu.Executor.create()
    val foo = Fu.of(using fex){ Thread.sleep(50); n.++; 4 }
    T ~ foo.isComplete ==== false
    T ~ foo.ask()      ==== Is(4)
    T ~ foo.isComplete ==== true
    T ~ n.get          ==== 3
    fex.unwrap.close()
    val alnum = "abcdefghijklmnopqrstuvwxyzABCDEFHIJKLMNOPQRSTUVWXYZ0123456789"
    val fs = alnum.arr.map(c => Fu of { Thread.sleep(100); n.++; c })
    val ans: Array[Char] Or Err = time{ fs.validFu.ask() }
    T ~ (dt.get/1e9 > 0.05)  ==== true
    T ~ (dt.get/1e9 < 0.15)  ==== true
    T ~ ans.map(_.mkString)  ==== alnum --: typed[String Or Err]
    T ~ fs.fu.ask().get =**= alnum.arr.copyWith(x => x.orAlt[Err])
    T ~ n.get               ==== 3 + alnum.length
    T ~ Fu{ nice{ "1".toInt }    }.map(_ * 3).ask() ==== 3 --: typed[Int Or Err]
    T ~ Fu{ Err("eel").orIs[Int] }.map(_ * 3).ask() ==== Alt(Err("eel"))
    T ~ Fu{ nice{ "1".toInt } }.map(x => 5/(x-1)).ask().isAlt ==== true
    T ~ Fu{ nice{ "1".toInt }    }.flatMap(n => (n+n).orAlt[Err]    ).ask() ==== 2 --: typed[Int Or Err]
    T ~ Fu{ nice{ "1".toInt }    }.flatMap(n => Err("cod").orIs[Int]).ask() ==== Alt(Err("cod"))
    T ~ Fu{ nice{ "1".toInt }    }.flatMap(n => (5/(n-1)).orAlt[Err]).ask() ==== runtype[Alt[?]]
    T ~ Fu{ Err("eel").orIs[Int] }.flatMap(n => (n+n).orAlt[Err]    ).ask() ==== Alt(Err("eel"))
    T ~ Fu{ Err("eel").orIs[Int] }.flatMap(n => yikes("salmon")     ).ask() ==== Alt(Err("eel"))
    T ~ Err.Or[Int]{ Fu.of{ "eel".length }.? * 3 } ==== 9 --: typed[Int Or Err]
    T ~ Err.Or[Int]{ Fu.of{ "eel".toInt  }.? * 3 } ==== runtype[Alt[?]]
    T ~ Or.Ret[Int, String]{ Fu.of{ "eel".length }.?+(_.toString) * 2 } ==== 6 --: typed[Int Or String]
    T ~ Or.Ret[Int, String]{ Fu.of{ "eel".toInt  }.?+(_.toString) * 2 } ==== runtype[Alt[?]]
    given AutoMap[Err, Char] = e => e.toString.fn(s => if s.length > 0 then '+' else '-')
    T ~ Or.Ret[Int, Char]{ Fu.of{ "eel".length }.?* + 4 } ==== 7 --: typed[Int Or Char]
    T ~ Or.Ret[Int, Char]{ Fu.of{ "eel".toInt }.?* + 4 }  ==== Alt('+')
    T ~ Err.Or[Int]{ Fu.of{ "eel".length }.?#("Yo") / 2 } ==== 1 --: typed[Int Or Err]
    T ~ Err.Or[Int]{ Fu.of{ "eel".toInt  }.?#("Yo") / 2 }.alt.toString.take(2) ==== "Yo"
    T ~ Fu{ nice{ Fu.of{ "eel".length }.? + 2 } }.ask() ==== 5
    T ~ Fu{ nice{ Fu.of{ "eel".toInt  }.? + 2 } }.ask() ==== runtype[Alt[?]]
    T ~ Fu.of{ Fu.of{ "eel".length }.?#("Yo") + 1 }.ask() ==== 4
    T ~ Fu.of{ Fu.of{ "eel".toInt  }.?#("Yo") + 1 }.ask().alt.toString.take(2) ==== "Yo"
    T ~ Fu.of{ "eel".length }.map{ x => Fu.of{ x*x }.? - 1 }.ask() ==== 8
    T ~ Fu.of{ "eel".length }.map{ x => Fu.of{ "e"(x)}.? - 1 }.ask() ==== runtype[Alt[?]]
    T ~ Fu.of{ "eel".length }.flatMap{ x => (Fu.of{ x*x }.? + 1).orAlt[Err] }.ask() ==== 10
    T ~ Fu.of{ "eel".length }.flatMap{ x => (Fu.of{ "e"(x)}.? + 1).orAlt[Err] }.ask() ==== runtype[Alt[?]]
    def fus(): Array[Fu[Int]] = Array(Fu.of{ "eel".length }, Fu.of{ "eel".toInt }, Fu{ nice{ "bass".length } }, Fu{ nice{ "bass".toInt } })
    T ~ fus().fu.ask().get.map(_.mapAlt(_ => Err("cod")))  =**= fus().map(_.ask()).map(_.mapAlt(_ => Err("cod")))
    T ~ fus().validFu.ask().alt.toString.diced(_ == '\n')(0) ==== "Multiple errors found (2)"
    T ~ fus().fuMap(n => 14/(n-3)).fu.ask().get.map(_.fold(_.abs)(_ => -1)) =**= Array(-1, -1, 14, -1)
    T ~ fus().fuFlatMap(n => nice{ "123abc".take(n).toInt }).fu.ask().get.map(_.fold(_.abs)(_ => -1)) =**= Array(123, -1, -1 ,-1)


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
    T ~ w.getOrUnit    ==== Alt(()) --: typed[String Or Unit]
    T ~ w.get          ==== thrown[IllegalStateException]
    w.set("cod")
    T ~ w.getOrUnit ==== "cod" --: typed[String Or Unit]
    T ~ w.get       ==== "cod"
    T ~ w.setIfEmpty("herring") ==== false
    T ~ w.set("herring")        ==== thrown[IllegalStateException]
    T ~ w.getOrSet("perch")     ==== "cod" --: typed[String]
    T ~ ww.getOrSet("minnow")   ==== "minnow"
    T ~ ww.getOrSet("eel")      ==== "minnow"
    T ~ www.getOrUnit           ==== "halibut"

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
    def oops(): Nothing = throw new Exception("oops")
    val m = Mu(0)
    T ~ Resource(m)(_.zap(_ + 1)){ x => x.set(2); x.value + 2 } ==== 4
    T ~ m.value                                                 ==== 3
    T ~ Resource(m)(_.zap(_ * 2)){ x => oops(); () }            ==== thrown[Exception]
    T ~ m.value                                                 ==== 6
    T ~ Err.Or[Int]{ Resource(m)(_.zap(- _)){ x => 
          if x.value > 0 then Is.break(x.value)
          else if x.value < 0 then Err.break("negative")
          else 0
        } }                                                     ==== 6 --: typed[Int Or Err]
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
        }.existsAlt(_.isInstanceOf[Exception])                             ==== true
    T ~ m.value                                                            ==== -18

    T ~ Resource.nice(m.orErr)(_.zap(_ + 1)){ x => x.set(2); x.value + 2 } ==== 4  --: typed[Int Or Err]
    T ~ m.value                                                            ==== 3
    T ~ Resource.nice(m.orErr)(_.zap(_ * 2)){ x => 
          oops(); ()
        }.existsAlt(_.toString contains "oops")                            ==== true
    T ~ m.value                                                            ==== 6
    T ~ Or.FlatRet{
          Resource.nice(m.orErr)(_.zap(- _)){ x => 
            if x.value > 0 then Is.break(x.value)
            else if x.value < 0 then Err.break("negative")
            else 0
          }
        }                                                                  ==== 6
    T ~ m.value                                                            ==== -6
    T ~ Resource.nice(m.orErr){ x =>
          oops(); x.zap(_ * 2)
        }{ x => 
          x.zap(_ * 3); x.value
        }.existsAlt(_.toString contains "closing resource")          ==== true
    T ~ Resource.nice(m.orErr){ x =>
          oops(); x.zap(_ * 3)
        }{ x => 
          x.zap(_ / 2); x.value
        }.mapAlt(_.underlying match
          case ete: ErrType.Explained => ete.context match
            case Some(i: Int) => ete.withContext(i+1).context
            case _ => None
          case _ => None
        ).altOrElse(_ => None)                                       ==== Some(-8)
    T ~ m.value                                                      ==== -9
    T ~ Resource.nice{ oops(); m.orErr }(_.zap(- _)) {
          x => x.zap(_ * 3); x.value
        }                                                            ==== runtype[Alt[?]]
    T ~ m.value                                                      ==== -9
    T ~ Resource.nice{
          m.errCase{ case x if x.value < 0 => Err("bad") }
        }(_.zap(- _)) {
          x => x.zap(_ * 3); x.value
        }                                                            ==== Err.or("bad")
    T ~ m.value                                                      ==== -9
    T ~ Resource.Nice(m.orErr)(_.zap(_ + 3)){ x =>
          x.zap(_ * 2); x.value
        }                                                            ==== -18
    T ~ m.value                                                      ==== -15
    T ~ Resource.Nice(m.orErr)(_.zap(- _)){ x =>
          oops()
          x.set(7); x.value
        }                                                            ==== runtype[Alt[?]]
    T ~ m.value                                                      ==== 15
    T ~ Resource.Nice(m.orErr)(_.zap(- _)){ x =>
          x.zap(_ - 8)
          Err.break(x.value.toString)
          x.set(4); x.value
        }                                                            ==== Alt(Err("7"))
    T ~ m.value                                                      ==== -7
    T ~ Resource.Nice(m.orErr){ x =>
          oops(); x.zap(_ * 3)
        }{ x => 
          x.zap(_ + 2); x.value
        }.mapAlt(_.underlying match
          case ete: ErrType.Explained =>
            ete.mapContext(x => x.map(_.toString)).context
          case _ => None
        ).altOrElse(_ => None)                                       ==== Some("-5")
    T ~ m.value                                                      ==== -5
    T ~ Resource.Nice{
          m.errCase{ case x if x.value < 0 => Err("bad") }
        }(_.zap(- _)){ x =>
          x.zap(_ - 8)
          Err.break(x.value.toString)
          x.set(4); x.value
        }                                                            ==== Err.or("bad")
    T ~ m.value                                                      ==== -5
}
object FlowTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
