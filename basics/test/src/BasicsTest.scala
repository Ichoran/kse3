// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-25 Rex Kerr and Calico Life Sciences, LLC.

package kse.test.basics


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

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


class BytecodeCheck {
  import kse.basics.{_, given}

  var myField: Int = 0

  def byVarHandleAccess(): Int =
    BytecodeCheck.vhField.getVolatile(this).asInstanceOf[Int]

  def repeated(xs: Int*): Int = xs.sum

  def ordinary(s: String): Char = s(2)

  def inlined(s: String): Char =
    import kse.basics.{given, _}
    s(2)

  def mktag(d: Double): Double \ "meter" =
    import labels._
    d \ "meter"

  def strcut(s: String, i0: Int, iN: Int) = s.substring(i0, iN)

  def apply3test(s: String) =
    s |-> (strcut, 3, 7)

  def tupcopies() =
    val t = (a = "eel", b = true, c = 0.5)
    val u = (c = 'c', a = 12341324L)
    val w = (u.a, t.b, u.c)
    val x = kse.basics.labels.NamesAndLabels.copyWithUpdateByName(t, u)
    (w, x)

  def pointtest(i: Int, a: Array[Char]) =
    import intervals.*
    Iv.point(End - i, a)

  def pointtest2(i: Int, a: Array[Char]) =
    import intervals.*
    Iv.point(i, a)

  def fn2test(i: Int, j: Int) =
    (i, j).fn(_ * _)

  def unuseTest(a: Array[String]) =
    var i = 0
    var j = 2
    while j <= 5 do
      i += a(j).length
      j += 1
    i

  def useTest(a: Array[String]) =
    var i = 0
    a.use(2 to 5)(i += _.length)
    i
  
  def boundaryTest(d: Double): Double =
    boundary[Double]:
      var i = d.toInt
      val f = d - i
      while i > 0 do
        i -= boundary[Int]:
          if f < 0 then boundary.break(i)
          if 1.0/i < f then boundary.break(1.0/i)
          1
      0.0

  def hopTest(d: Double): Double =
    Hop:
      var i = d.toInt
      val f = d - i
      while i > 0 do
        val j: Int = Hop:
          if f < 0 then Hop.jump(i)
          if 1.0/i < f then Hop.jump(1.0/i)
          1
        i -= j
      0.0

  def corralTest(d: Double): Double =
    Corral:
      Hop:
        var i = d.toInt
        val f = d - i
        while i > 0 do
          val j: Int = Hop:
            if f < 0 then Hop.jump(i)
            if 1.0/i < f then Hop.jump(1.0/i)
            1
          i -= j
        0.0

  def useRepeated(): Int =
    repeated(5, 6, 7, 8)
}
object BytecodeCheck {
  val vhField =
    java.lang.invoke.MethodHandles.privateLookupIn(classOf[BytecodeCheck], java.lang.invoke.MethodHandles.lookup()).
      findVarHandle(classOf[BytecodeCheck], "myField", classOf[Int])
}


@RunWith(classOf[JUnit4])
class BasicsTest() {
  import compiletime.testing.{typeChecks => cc}
  import kse.testutilities.TestUtilities.{_, given}
  import kse.basics.{_, given}
  import kse.basics.intervals._
  import kse.basics.labels.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  inline def subtyping[A, B](a: A, b: B) = compiletime.summonFrom {
    case _: (A =:= B) => '='
    case _: (A <:< B) => '<'
    case _: (B <:< A) => '>'
    case _            => 'X'
  }

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
    T ~ basic.hasAnyStackTrace                             ==== true 
    T ~ caused.hasAnyStackTrace                            ==== true
    T ~ stackless.hasAnyStackTrace                         ==== false

    val short = caused.explainAsArray(lines = 10)
    val full  = caused.explainAsArray()
    T ~ short.take(9) =**= full.take(9)
    val lines = full.drop(9).count(s => !s.startsWith("| "))
    T ~ short.last ==== s". . . (+$lines lines and 1 more exception)"


  @Test
  def hopTest(): Unit =
    T ~ {
      Corral:
        hop[Int].here:
          hop[String].here:
            Hop.jump(5)
            Hop.jump("cod")
            Corral:
              "eel"
          .length
    } ==== 5
    T ~ { compiletime.testing.typeChecks("""
      hop[Int].here:
        Corral:
          hop[String].here:
            Hop.jump(5)
        2
    """) } ==== false
   T ~ {
     hop[Option[Int]].here:
       Hop.jump(Some(3))
       None
   } ==== Some(3) --: typed[Option[Int]]
   T ~ {
     hop[Seq[Int]].here:
       hop[Vector[Int]].here:
         Hop.jump(List(2, 3))
         Hop.jump(Vector(1, 4))
         Vector.empty
   } ==== Seq(2, 3)


  @Test
  def shortcutTest(): Unit =
    var x = 0
    T ~ { shortcut.quittable{ x += 1; shortcut.quit(); x += 1 }; x } ==== 1
    T ~ { shortcut.quittable{ x += 1; shortcut.quit(x > 1).?;  x += 1 }; x } ==== 2
    T ~ { shortcut.quittable{ x += 1; shortcut.quit(x > 3).?;  x += 1 }; x } ==== 4
    T ~ { shortcut.skippable{ x += 1; shortcut.skip(); x += 1 }; x } ==== 5
    T ~ { shortcut.skippable{ x += 1; shortcut.skip(x > 5).?;  x += 1 }; x } ==== 6
    T ~ { shortcut.skippable{ x += 1; shortcut.skip(x > 7).?;  x += 1 }; x } ==== 8
    T ~ { shortcut.outer{ x += 2; shortcut.quit(); x += 1 }; x } ==== 10
    T ~ { shortcut.outer{ x += 2; shortcut.skip(); x += 1 }; x } ==== 12
    T ~ { shortcut.outer{ x += 2; shortcut.inner{ x += 1; shortcut.skip(); x += 1 }; x += 3 }; x } ==== 18
    T ~ { shortcut.outer{ x += 2; shortcut.inner{ x += 1; shortcut.quit(); x += 1 }; x += 3 }; x } ==== 21

    var y = 0
    T ~ { shortcut.hopped.quittable{ y += 1; shortcut.hopped.quit();        y += 1 }; y } ==== 1
    T ~ { shortcut.hopped.quittable{ y += 1; shortcut.hopped.quitIf(y > 1); y += 1 }; y } ==== 2
    T ~ { shortcut.hopped.quittable{ y += 1; shortcut.hopped.quitIf(y > 3); y += 1 }; y } ==== 4
    T ~ { shortcut.hopped.skippable{ y += 1; shortcut.hopped.skip();        y += 1 }; y } ==== 5
    T ~ { shortcut.hopped.skippable{ y += 1; shortcut.hopped.skipIf(y > 5); y += 1 }; y } ==== 6
    T ~ { shortcut.hopped.skippable{ y += 1; shortcut.hopped.skipIf(y > 7); y += 1 }; y } ==== 8
    T ~ { shortcut.hopped.outer{ y += 2; shortcut.hopped.quit(); y += 1 }; y } ==== 10
    T ~ { shortcut.hopped.outer{ y += 2; shortcut.hopped.skip(); y += 1 }; y } ==== 12
    T ~ { shortcut.hopped.outer{ y += 2; shortcut.hopped.inner{ y += 1; shortcut.hopped.skip(); y += 1 }; y += 3 }; y } ==== 18
    T ~ { shortcut.hopped.outer{ y += 2; shortcut.hopped.inner{ y += 1; shortcut.hopped.quit(); y += 1 }; y += 3 }; y } ==== 21

    var z = 0
    T ~ Corral{ shortcut.hopped.quittable{ z += 1; shortcut.hopped.quit();        z += 1 }; z } ==== 1
    T ~ Corral{ shortcut.hopped.quittable{ z += 1; shortcut.hopped.quitIf(z > 1); z += 1 }; z } ==== 2
    T ~ Corral{ shortcut.hopped.quittable{ z += 1; shortcut.hopped.quitIf(z > 3); z += 1 }; z } ==== 4
    T ~ Corral{ shortcut.hopped.skippable{ z += 1; shortcut.hopped.skip();        z += 1 }; z } ==== 5
    T ~ Corral{ shortcut.hopped.skippable{ z += 1; shortcut.hopped.skipIf(z > 5); z += 1 }; z } ==== 6
    T ~ Corral{ shortcut.hopped.skippable{ z += 1; shortcut.hopped.skipIf(z > 7); z += 1 }; z } ==== 8
    T ~ Corral{ shortcut.hopped.outer{ z += 2; shortcut.hopped.quit(); z += 1 }; z } ==== 10
    T ~ Corral{ shortcut.hopped.outer{ z += 2; shortcut.hopped.skip(); z += 1 }; z } ==== 12
    T ~ Corral{ shortcut.hopped.outer{ z += 2; shortcut.hopped.inner{ z += 1; shortcut.hopped.skip(); z += 1 }; z += 3 }; z } ==== 18
    T ~ Corral{ shortcut.hopped.outer{ z += 2; shortcut.hopped.inner{ z += 1; shortcut.hopped.quit(); z += 1 }; z += 3 }; z } ==== 21

    T ! """{ shortcut.hopped.quittable{ z += 1; Corral{ shortcut.hopped.quit_?();      z += 1 } }; z }"""
    T ! """{ shortcut.hopped.quittable{ z += 1; Corral{ shortcut.hopped.quit_?(z > 1); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.quittable{ z += 1; Corral{ shortcut.hopped.quit_?(z > 3); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.skippable{ z += 1; Corral{ shortcut.hopped.skip_?();      z += 1 } }; z }"""
    T ! """{ shortcut.hopped.skippable{ z += 1; Corral{ shortcut.hopped.skip_?(z > 5); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.skippable{ z += 1; Corral{ shortcut.hopped.skip_?(z > 7); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; Corral{ shortcut.hopped.quit_?(); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; Corral{ shortcut.hopped.skip_?(); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; Corral{ shortcut.hopped.inner{ z += 1; shortcut.hopped.skip_?(); z += 1 }; z += 3 } }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; Corral{ shortcut.hopped.inner{ z += 1; shortcut.hopped.quit_?(); z += 1 }; z += 3 } }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; shortcut.hopped.inner{ z += 1; Corral{ shortcut.hopped.skip_?(); z += 1 } }; z += 3 }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; shortcut.hopped.inner{ z += 1; Corral{ shortcut.hopped.quit_?(); z += 1 } }; z += 3 }; z }"""


  @Test
  def dataWrapperTest(): Unit =
    val ent = "eel" \ "fish"
    val esb = "eel" \> "fish"
    val esp = "eel" \< "fish"
    def eatstr(s: String): Unit = {}
    T ~ subtyping(ent, "eel") ==== 'X'
    T ~ subtyping(esb, "eel") ==== '<'
    T ~ subtyping(esp, "eel") ==== '>'
    T ~ cc("""eatstr(ent)""") ==== false
    T ~ cc("""eatstr(esb)""") ==== true
    T ~ cc("""eatstr(esp)""") ==== false
    T ~ ent.label             ==== "fish"
    T ~ esb.label             ==== "fish"
    T ~ esp.label             ==== "fish"
    T ~ ent.unlabel           ==== "eel"
    T ~ esb.unlabel           ==== "eel"
    T ~ esp.unlabel           ==== "eel"
    T ~ (ent ~ "fish")        ==== "eel" --: typed[String]
    T ~ (esp ~ "fish")        ==== "eel" --: typed[String]
    T ~ ent.valueTo("cod")    ==== "cod" --: typed[String \ "fish"]
    T ~ esb.valueTo("cod")    ==== "cod" --: typed[String \> "fish"]
    T ~ esp.valueTo("cod")    ==== "cod" --: typed[String \< "fish"]
    T ~ ent.valueOp(_.length) ==== 3     --: typed[Int \ "fish"]
    T ~ esb.valueOp(_.length) ==== 3     --: typed[Int \> "fish"]
    T ~ esp.valueOp(_.length) ==== 3     --: typed[Int \< "fish"]
    T ~ ent.labelTo("thing")  ==== "eel" --: typed[String \ "thing"]
    T ~ esb.labelTo("thing")  ==== "eel" --: typed[String \> "thing"]
    T ~ esp.labelTo("thing")  ==== "eel" --: typed[String \< "thing"]
    T ~ ent.subtyped          ==== "eel" --: typed[String \> "fish"]
    T ~ ent.supertyped        ==== "eel" --: typed[String \< "fish"]
    T ~ esb.newtyped          ==== "eel" --: typed[String \ "fish"]
    T ~ esb.supertyped        ==== "eel" --: typed[String \< "fish"]
    T ~ esp.newtyped          ==== "eel" --: typed[String \ "fish"]
    T ~ esp.subtyped          ==== "eel" --: typed[String \> "fish"]

    val m = Mu(5)
    T ~ m()                 ==== 5
    T ~ { m := 3; m }       ==== Mu(3)
    T ~ m.poke(_ - 1)       ==== Mu(2)
    T ~ m.toString          ==== "~2"
    T ~ m.copy              ==== Mu(2) --: typed[Mu.MuInt]
    T ~ { m.++; m() }       ==== 3
    T ~ { m.--; m }         ==== Mu(2)
    T ~ { m.zap(_ + 2); m } ==== Mu(4)

    object Meter extends NewType[Double] {}

    T ~ Mu(())      .poke(_ => ())           .pipe(x => (x, x.copy.tap(_ := ())))   .sameOp(_()) ==== ((), ())
    T ~ Mu(true)    .poke(z => !z)           .pipe(x => (x, x.copy.tap(_ := true))) .sameOp(_()) ==== (false, true)
    T ~ Mu(1: Byte ).poke(b => (b+1).toByte) .pipe(x => (x, x.copy.tap(_ := 4)))    .sameOp(_()) ==== (2: Byte, 4: Byte)   --: typed[(Byte, Byte)]
    T ~ Mu(1: Short).poke(s => (s+1).toShort).pipe(x => (x, x.copy.tap(_ := 4)))    .sameOp(_()) ==== (2: Short, 4: Short) --: typed[(Short, Short)]
    T ~ Mu('e')     .poke(_.toUpper)         .pipe(x => (x, x.copy.tap(_ := 'f')))  .sameOp(_()) ==== ('E', 'f')           --: typed[(Char, Char)]
    T ~ Mu(1)       .poke(_ + 1)             .pipe(x => (x, x.copy.tap(_ := 4)))    .sameOp(_()) ==== (2, 4)               --: typed[(Int, Int)]
    T ~ Mu(1L)      .poke(_ + 1)             .pipe(x => (x, x.copy.tap(_ := 4)))    .sameOp(_()) ==== (2L, 4L)             --: typed[(Long, Long)]
    T ~ Mu(1f)      .poke(_ + 1f)            .pipe(x => (x, x.copy.tap(_ := 4f)))   .sameOp(_()) ==== (2f, 4f)             --: typed[(Float, Float)]
    T ~ Mu(1.0)     .poke(_ + 1.0)           .pipe(x => (x, x.copy.tap(_ := 4.0)))  .sameOp(_()) ==== (2.0, 4.0)           --: typed[(Double, Double)]
    T ~ Mu("cod")   .poke(_ + "!")           .pipe(x => (x, x.copy.tap(_ := "eel"))).sameOp(_()) ==== ("cod!", "eel")      --: typed[(String, String)]
    T ~ Mu.T(Meter(2.0)).poke(m => Meter(m.value+1)).pipe(x => (x, x.copy.tap(_ := Meter(3)))).sameOp(_()) ==== (Meter(3), Meter(3)) --: typed[(Meter.Type, Meter.Type)]
    T ~ Mu.T(Meter(2.0)).getClass ==== Mu.MuDouble(2.0).getClass

    T ~ Mu(3L).tap(_.--)() ==== 2L
    T ~ Mu(3L).tap(_.++)() ==== 4L

    val az = Atom(true)
    val ab = Atom(2: Byte)
    val as = Atom(2: Short)
    val ac = Atom('e')
    val ai = Atom(2)
    val al = Atom(2L)
    val af = Atom(0.5f)
    val ad = Atom(0.5)
    val aa = Atom("eel")
    val am = Atom(Meter(1.5))
    inline def q(x: Int | Double) = inline x match
      case i: Int => Meter(0.5+i)
      case d: Double => Meter(1.0+d)
    T ~ az() ==== true       --: typed[Boolean]
    T ~ ab() ==== (2: Byte)  --: typed[Byte]
    T ~ as() ==== (2: Short) --: typed[Short]
    T ~ ac() ==== 'e'        --: typed[Char]
    T ~ ai() ==== 2          --: typed[Int]
    T ~ al() ==== 2L         --: typed[Long]
    T ~ af() ==== 0.5f       --: typed[Float]
    T ~ ad() ==== 0.5        --: typed[Double]
    T ~ aa() ==== "eel"      --: typed[String]
    T ~ am() ==== 1.5        --: typed[Meter.Type]
    T ~ { az := false; az swap true}     ==== false
    T ~ { ab := 3;     ab swap 4 }       ==== (3: Byte)
    T ~ { as := 3;     as swap 4 }       ==== (3: Short)
    T ~ { ac := 'f';   ac swap 'g' }     ==== 'f'
    T ~ { ai := 3;     ai swap 4 }       ==== 3
    T ~ { al := 3L;    al swap 4L }      ==== 3L
    T ~ { af := 0.4f;  af swap 0.3f }    ==== 0.4f
    T ~ { ad := 0.4;   ad swap 0.3 }     ==== 0.4
    T ~ { aa := "cod"; aa swap "bass" }  ==== "cod"
    T ~ { am := q(2);  am swap q(3) }    ==== 2.5
    T ~ az.getAndZap(z => !z)            ==== true
    T ~ ab.getAndZap(b => (b+1).toByte)  ==== (4: Byte)
    T ~ as.getAndZap(s => (s+1).toShort) ==== (4: Short)
    T ~ ac.getAndZap(c => (c+1).toChar)  ==== 'g'
    T ~ ai.getAndZap(_ + 1)              ==== 4
    T ~ al.getAndZap(_ + 1L)             ==== 4L
    T ~ af.getAndZap(_ - 0.1f)           ==== 0.3f
    T ~ ad.getAndZap(_ - 0.1)            ==== 0.3
    T ~ aa.getAndZap(_ + " cod")         ==== "bass"
    T ~ am.getAndZap(m => q(m.value))    ==== 3.5
    T ~ az.zapAndGet(z => !z)            ==== true
    T ~ ab.zapAndGet(b => (b+2).toByte)  ==== (7: Byte)
    T ~ as.zapAndGet(s => (s+2).toShort) ==== (7: Short)
    T ~ ac.zapAndGet(c => (c+2).toChar)  ==== 'j'
    T ~ ai.zapAndGet(_ + 2)              ==== 7
    T ~ al.zapAndGet(_ + 2)              ==== 7L
    T ~ af.zapAndGet(_ / 2.0f)           =~~= 0.1f
    T ~ ad.zapAndGet(_ / 2.0)            =~~= 0.1
    T ~ aa.zapAndGet(_ + " perch")       ==== "bass cod perch"
    T ~ am.zapAndGet(m => q(m.value+1))  ==== 6.5
    az.zap(z => !z)
    ab.zap(b => (b+2).toByte)
    as.zap(s => (s+2).toShort)
    ac.zap(c => (c+2).toChar)
    ai.zap(_ + 2)
    al.zap(_ + 2)
    af.zap(_ / 2.0f)
    ad.zap(_ / 2.0)
    aa.zap(_ + " eel")
    am.zap(m => q(m.value+1))
    T ~ az.tap(_.zap(z => !z))()            ==== true
    T ~ ab.tap(_.zap(b => (b+2).toByte))()  ==== (11: Byte)
    T ~ as.tap(_.zap(s => (s+2).toShort))() ==== (11: Short)
    T ~ ac.tap(_.zap(c => (c+2).toChar))()  ==== 'n'
    T ~ ai.tap(_.zap(_ + 2))()              ==== 11
    T ~ al.tap(_.zap(_ + 2))()              ==== 11L
    T ~ af.tap(_.zap(_ / 2.0f))()           =~~= 0.025f
    T ~ ad.tap(_.zap(_ / 2.0))()            =~~= 0.025
    T ~ aa.tap(_.zap(_ + " sole"))()        ==== "bass cod perch eel sole"
    T ~ am.tap(_.zap(m => q(m.value+1)))()  ==== 10.5

    T ~ ai.tap(_.--)() ==== 10
    T ~ ai.tap(_.++)() ==== 11
    T ~ al.tap(_.--)() ==== 10
    T ~ al.tap(_.++)() ==== 11

    val na = Atom.Count.from(1)
    T ~ na()              ==== 1 --: typed[Long]
    T ~ { na := 2; na() } ==== 2
    T ~ na.swap(3)        ==== 2
    T ~ { na.++; na() }   ==== 4
    T ~ { na.--; na() }   ==== 3
    T ~ { na += 3; na() } ==== 6
    T ~ { na -= 2; na() } ==== 4

    val tg = Atom.Toggle()
    T ~ tg()               ==== false
    T ~ { tg.on();  tg() } ==== true
    T ~ { tg.off(); tg() } ==== false
    T ~ tg.turnOff()       ==== false
    T ~ tg()               ==== false
    T ~ tg.turnOn()        ==== true
    T ~ tg()               ==== true
    T ~ tg.turnOn()        ==== false
    T ~ tg()               ==== true
    T ~ tg.turnOff()       ==== true
    T ~ tg()               ==== false


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
    T ~ Anon(Mu(()))                     ==== typed[Anon[Mu.MuUnit.type]]
    T ~ Anon(gm(()))                     ==== typed[Anon[Mu[Unit]]]
    T ~ Anon(gm(()).toPrimitiveMu)       ==== typed[Anon[Mu.MuUnit.type]]
    T ~ Anon(Mu(true))                   ==== typed[Anon[Mu.MuBoolean]]
    T ~ Anon(gm(true))                   ==== typed[Anon[Mu[Boolean]]]
    T ~ Anon(gm(true).toPrimitiveMu)     ==== typed[Anon[Mu.MuBoolean]]
    T ~ Anon(Mu(1: Byte))                ==== typed[Anon[Mu.MuByte]]
    T ~ Anon(gm(1: Byte))                ==== typed[Anon[Mu[Byte]]]
    T ~ Anon(gm(1: Byte).toPrimitiveMu)  ==== typed[Anon[Mu.MuByte]]
    T ~ Anon(Mu(2: Short))               ==== typed[Anon[Mu.MuShort]]
    T ~ Anon(gm(2: Short))               ==== typed[Anon[Mu[Short]]]
    T ~ Anon(gm(2: Short).toPrimitiveMu) ==== typed[Anon[Mu.MuShort]]
    T ~ Anon(Mu('e'))                    ==== typed[Anon[Mu.MuChar]]
    T ~ Anon(gm('e'))                    ==== typed[Anon[Mu[Char]]]
    T ~ Anon(gm('e').toPrimitiveMu)      ==== typed[Anon[Mu.MuChar]]
    T ~ Anon(Mu(4))                      ==== typed[Anon[Mu.MuInt]]
    T ~ Anon(gm(4))                      ==== typed[Anon[Mu[Int]]]
    T ~ Anon(gm(4).toPrimitiveMu)        ==== typed[Anon[Mu.MuInt]]
    T ~ Anon(Mu(5L))                     ==== typed[Anon[Mu.MuLong]]
    T ~ Anon(gm(5L))                     ==== typed[Anon[Mu[Long]]]
    T ~ Anon(gm(5L).toPrimitiveMu)       ==== typed[Anon[Mu.MuLong]]
    T ~ Anon(Mu(6f))                     ==== typed[Anon[Mu.MuFloat]]
    T ~ Anon(gm(6f))                     ==== typed[Anon[Mu[Float]]]
    T ~ Anon(gm(6f).toPrimitiveMu)       ==== typed[Anon[Mu.MuFloat]]
    T ~ Anon(Mu(7.0))                    ==== typed[Anon[Mu.MuDouble]]
    T ~ Anon(gm(7.0))                    ==== typed[Anon[Mu[Double]]]
    T ~ Anon(gm(7.0).toPrimitiveMu)      ==== typed[Anon[Mu.MuDouble]]
    T ~ Anon(Mu("cod"))                  ==== typed[Anon[Mu.MuAny[String]]]
    T ~ Anon(gm("cod"))                  ==== typed[Anon[Mu[String]]]

    T ~ Anon("secret!").toString ==== "..."
    T ~ Anon("secret!").##       ==== 1239182
    T ~ Anon("eel")              ==== Anon(Option(0.12345))

    T ~ Identity(m)                       ==== Identity(m)
    T ~ Identity(m.copy)                  =!!= Identity(m)
    T ~ Identity(m).##                    ==== System.identityHashCode(m)


  @Test
  def immutableDataTest(): Unit =
    object Meter extends NewType[Double] {
      extension (t: Type) {
        def *(that: Double): Meter.Type = Meter.wrap(t.unwrap * that)
      }
    }
    val meter = Meter.wrap(4.5)
    T ~ Meter.wrap(3)    ==== 3 --: typed[Meter.Type]
    T ~ { Meter(2) * 3 } ==== 6 --: typed[Meter.Type]
    T ~ meter.value      ==== 4.5

    T ~ "herring".fn( s => s.length + s.head)                ==== (7 + 'h')
    T ~ "salmon".pipe(s => s.length + s.head)                ==== (6 + 's')
    T ~ 5.cycle(3)(x => 4*x)                                 ==== 320
    T ~ "eel".fixIf(_.length > 3)(_ => "cod")                ==== "eel"
    T ~ "salmon".fixIf(_.length > 3)(_ => "cod")             ==== "cod"
    T ~ "eel".fixWhile(_(0) == 'e')(_.substring(1))          ==== "l"
    T ~ { var x = 0; "cod".tap(s => x = s.head).length + x } ==== (3 + 'c')
    T ~ "herring".tup(5)                                     ==== ("herring", 5)
    T ~ "herring".tupWith(_.length)                          ==== ("herring", 7)
    T ~ "herring".groupBy(__)                                ==== Map('h' -> "h", 'e' -> "e", 'r' -> "rr", 'i' -> "i", 'n' -> "n", 'g' -> "g")


  val tupleTester = new TuplesTest()


  // @Test
  // def unlabelledTupleTest(): Unit =
  //   tupleTester.unlabelledTuples()


  //@Test
  //def labeledTupleTest(): Unit =
  //  tupleTester.labelledSingleton()
  //  tupleTester.labelledDoublet()
  //  tupleTester.labelledTriplet()
  //  tupleTester.labelledQuadruplet()
  //  tupleTester.labelledQuintuplet()
  //  tupleTester.labelledSextuplet()
  //  tupleTester.labelledSeptuplet()
  //  tupleTester.labelledOctuplet()
  //  tupleTester.labelledNonuplet()


  @Test
  def intervalAndConstantTest(): Unit =
    import scala.collection.IntStepper

    var cuml = 0
    var x = 0

    inline def n[A](inline f: => A): Int =
      cuml = 0
      x = 0
      f: Unit
      cuml

    inline def step2a(s: IntStepper) =
      val b = Array.newBuilder[Int]
      while s.hasStep do
        b += s.nextStep
      b.result

    T ~ 3.where()                                           =**= Array(0, 1, 2)
    T ~ -2.where()                                          =**= Array[Int]()
    T ~ 5.whereBy(2)                                        =**= Array(0, 2, 4)
    T ~ 6.whereBy(2)                                        =**= Array(0, 2, 4)
    T ~ 5.whereBy(-2)                                       =**= Array(4, 2, 0)
    T ~ 6.whereBy(-2)                                       =**= Array(5, 3, 1)
    T ~ 3.of[Int]                                           =**= Array(0, 0, 0)
    T ~ 3.of[Int]                                           ==== typed[Array[Int]]
    T ~ 3.unfold(i => i+1)                                  =**= Array(1, 2, 3)
    T ~ 3.unfold(i => i+1)                                  ==== typed[Array[Int]]
    T ~ 3.unfoldFlex{ i => shortcut.skip(i%2 != 0).?; i+1 } =**= Array(1, 3)
    T ~ 3.unfoldFlex{ i => shortcut.quit(i%2 != 0).?; i+1 } =**= Array(1)
    T ~ 3.unfoldFlex(i => i+1)                              ==== typed[Array[Int]]
    T ~ n{ 3.times{ cuml = 2*cuml + 1 } }                   ==== 7
    T ~ n{ 5.visit(cuml += _) }                             ==== 10
    T ~ n{ -2.visit(cuml += _ + 1) }                        ==== 0
    T ~ n{ 5.visitBy(2){ i => x += 1; cuml += i * x } }     ==== 16
    T ~ n{ 6.visitBy(2){ i => x += 1; cuml += i * x } }     ==== 16
    T ~ n{ -2.visitBy(2)(cuml += _ + 1) }                   ==== 0
    T ~ n{ 5.visitBy(-2){ i => x += 1; cuml += i * x } }    ==== 8
    T ~ n{ 6.visitBy(-2){ i => x += 1; cuml += i * x } }    ==== 14
    T ~ n{ -2.visitBy(-2)(cuml += _ + 1) }                  ==== 0
    T ~ n{ 5.visitBy(0)(cuml += _ + 1) }                    ==== 0
    T ~ n{ -2.visitBy(0)(cuml += _ + 1) }                   ==== 0
    T ~ n{ 0.visitBy(0)(cuml += _ + 1) }                    ==== 0
    T ~ step2a(3.steps())                                   =**= 3.where()
    T ~ step2a(-2.steps())                                  =**= -2.where()
    T ~ step2a(5.stepsBy(2))                                =**= 5.whereBy(2)
    T ~ step2a(6.stepsBy(2))                                =**= 6.whereBy(2)
    T ~ step2a(5.stepsBy(-2))                               =**= 5.whereBy(-2)
    T ~ step2a(6.stepsBy(-2))                               =**= 6.whereBy(-2)
    T ~ 5.stepsBy(2).estimateSize                           ==== 3L --: typed[Long]
    T ~ 6.stepsBy(-2).estimateSize                          ==== 3L --: typed[Long]

    T ~ (1 to End)                                 ==== typed[Iv.Rae]
    T ~ (1 to Start+3)                             ==== typed[Iv.Ras]
    T ~ (Start to 5)                               ==== typed[Iv.Rsa]
    T ~ (Start + 1 to End - 1)                     ==== typed[Iv.Rse]
    T ~ (Start + 1 to Start + 3)                   ==== typed[Iv.Rss]
    T ~ (End - 5 to 9)                             ==== typed[Iv.Rea]
    T ~ (End - 5 to End - 3)                       ==== typed[Iv.Ree]
    T ~ (End - 5 to Start + 5)                     ==== typed[Iv.Res]
    T ~ Iv.of(3 to 4)                              ==== Iv(3, 5)
    T ~ Iv.of("salmon")                            ==== Iv(0, 6)
    T ~ Iv.of(Array(1, 2, 3, 4))                   ==== Iv(0, 4)
    T ~ (1 to End-1).of(Array(1, 2, 3, 4))         ==== Iv(1, 3)  --: typed[Iv]
    T ~ (1 to End-1).of("abcd")                    ==== Iv(1, 3)  --: typed[Iv]
    T ~ Iv(3, 5).where()                           =**= Array(3, 4)
    T ~ Iv.of(3 to 4).where()                      =**= Array(3, 4)
    T ~ (1 to End-1).of(Array(1, 2, 3, 4)).where() =**= Array(1, 2)
    T ~ (1 to End-1).of("abcd").where()            =**= Array(1, 2)
    T ~ Iv(3, 8).whereBy(2)                        =**= Array(3, 5, 7)
    T ~ Iv(3, 9).whereBy(2)                        =**= Array(3, 5, 7)
    T ~ Iv(3, 8).whereBy(-2)                       =**= Array(7, 5, 3)
    T ~ Iv(3, 9).whereBy(-2)                       =**= Array(8, 6, 4)
    T ~ Iv(-2, 5).whereBy(-1)                      =**= Array(4, 3, 2, 1, 0, -1, -2)
    T ~ Iv(1, 9).whereBy(0).isEmpty                ==== true
    T ~ Iv(7, 7).whereBy(2).isEmpty                ==== true
    T ~ Iv(1, 3).whereBy(8275919)                  =**= Array(1)
    T ~ step2a(Iv(3, 6).steps())                   =**= Array(3, 4, 5)
    T ~ step2a(Iv(3, 3).steps()).isEmpty           ==== true
    T ~ step2a(Iv(3, 8).stepsBy(2))                =**= Array(3, 5, 7)
    T ~ step2a(Iv(3, 9).stepsBy(2))                =**= Array(3, 5, 7)
    T ~ step2a(Iv(3, 8).stepsBy(-2))               =**= Array(7, 5, 3)
    T ~ step2a(Iv(3, 9).stepsBy(-2))               =**= Array(8, 6, 4)
    T ~ step2a(Iv(3, 3).stepsBy(2)).isEmpty        ==== true
    T ~ step2a(Iv(3, 3).stepsBy(-2)).isEmpty       ==== true
    T ~ step2a(Iv(4, 6).stepsBy(0)).isEmpty        ==== true
    T ~ Iv(Int.MaxValue, Int.MaxValue).steps().estimateSize   ==== 0L
    T ~ Iv(Int.MinValue, Int.MaxValue).steps().estimateSize   ==== 0xFFFFFFFFL
    T ~ Iv(1, 3).unfold(_ + 1)                                =**= Array(2, 3)
    T ~ Iv(7, 7).unfold(_ + 1)                                =**= Array.empty[Int]
    T ~ Iv(5, 9).unfoldFlex{ i => shortcut.skip(i < 6).?; i } =**= Array(6, 7, 8)
    T ~ Iv(5, 9).unfoldFlex{ i => shortcut.quit(i > 7).?; i } =**= Array(5, 6, 7)

    T ~ n{ Iv(3, 5).visit(cuml += _) }             ==== 7
    T ~ Iv(3, 5).i0                                ==== 3
    T ~ Iv(3, 5).iN                                ==== 5
    T ~ Iv(3, 5).i0To(1)                           ==== Iv(1, 5)
    T ~ Iv(3, 5).iNTo(7)                           ==== Iv(3, 7)
    T ~ Iv(3, 5).i0Op(_ + 1)                       ==== Iv(4, 5)
    T ~ Iv(3, 5).iNOp(_ - 1)                       ==== Iv(3, 4)
    T ~ Iv(3, 5).ops(_ + 1, _ - 1)                 ==== Iv(4, 4)
    T ~ (Iv(3, 5) +# 2)                            ==== Iv(5, 7)
    T ~ (Iv(3, 5) -# 2)                            ==== Iv(1, 3)
    T ~ (Iv(3, 5) +# (Int.MaxValue - 1))           ==== Iv(Int.MaxValue - 2, Int.MaxValue)
    T ~ (Iv(-5, -3) -# (Int.MaxValue - 1))         ==== Iv(Int.MinValue, Int.MinValue + 2)
    T ~ (Iv(3, 5) & Iv(4, 8))                      ==== Iv(4, 5)
    T ~ (Iv(3, 5) & Iv(6, 8))                      ==== Iv(6, 5)
    T ~ (Iv(1, 6) & Iv(2, 4))                      ==== Iv(2, 4)
    T ~ (Iv(2, 4) & Iv(3, 1))                      ==== Iv(3, 1)
    T ~ (Iv(4, 2) & Iv(1, 3))                      ==== Iv(4, 2)
    T ~ (Iv(4, 2) & Iv(3, 1))                      ==== Iv(4, 1)                      
    T ~ (Iv(3, 5) | Iv(4, 8))                      ==== Iv(3, 8)
    T ~ (Iv(3, 5) | Iv(6, 8))                      ==== Iv(3, 8)
    T ~ (Iv(3, 5) | Iv(2, 1))                      ==== Iv(3, 5)
    T ~ (Iv(2, 1) | Iv(3, 5))                      ==== Iv(3, 5)
    T ~ (Iv(2, 1) | Iv(5, 3))                      ==== Iv(5, 1)
    T ~ Iv(3, 5).length                            ==== 2
    T ~ Iv(3, -2).length                           ==== 0
    T ~ Iv(3, 5).isEmpty                           ==== false
    T ~ Iv(3, -2).isEmpty                          ==== true
    T ~ Iv(3, 5).contains(2)                       ==== false
    T ~ Iv(3, 5).contains(3)                       ==== true
    T ~ Iv(3, 5).contains(4)                       ==== true
    T ~ Iv(3, 5).contains(5)                       ==== false
    T ~ Iv(-7, 9).clippedToSize(3)                 ==== Iv(0, 3)
    T ~ Iv(2, 9).clippedToSize(3)                  ==== Iv(2, 3)
    T ~ Iv(-7, 2).clippedToSize(3)                 ==== Iv(0, 2)
    T ~ Iv(-7, -3).clippedToSize(3)                ==== Iv(0, 0)
    T ~ Iv(7, 9).clippedToSize(3)                  ==== Iv(3, 3)
    T ~ Iv(-7, 9).clippedTo(Array(1, 2, 3))        ==== Iv(0, 3)
    T ~ Iv(2, 9).clippedTo(Array(1, 2, 3))         ==== Iv(2, 3)
    T ~ Iv(-7, 2).clippedTo(Array(1, 2, 3))        ==== Iv(0, 2)
    T ~ Iv(-7, 9).clippedTo("cod")                 ==== Iv(0, 3)
    T ~ Iv(2, 9).clippedTo("cod")                  ==== Iv(2, 3)
    T ~ Iv(-7, 2).clippedTo("cod")                 ==== Iv(0, 2)

    val xs = Array(1, 2, 4, 8, 16, 32, 64)
    var m = 0
    T ~ { m = 0; (      1 to End    ).of(xs).visit(i => m += xs(i)); m } ==== 126
    T ~ { m = 0; (      2 to End-1  ).of(xs).visit(i => m += xs(i)); m } ==== 60
    T ~ { m = 0; (      0 to Start  ).of(xs).visit(i => m += xs(i)); m } ==== 1
    T ~ { m = 0; (      2 to Start+3).of(xs).visit(i => m += xs(i)); m } ==== 12
    T ~ { m = 0; (    End to 5      ).of(xs).visit(i => m += xs(i)); m } ==== 0
    T ~ { m = 0; (    End to End    ).of(xs).visit(i => m += xs(i)); m } ==== 64
    T ~ { m = 0; (    End to End-1  ).of(xs).visit(i => m += xs(i)); m } ==== 0
    T ~ { m = 0; (    End to Start  ).of(xs).visit(i => m += xs(i)); m } ==== 0
    T ~ { m = 0; (    End to Start+6).of(xs).visit(i => m += xs(i)); m } ==== 64
    T ~ { m = 0; (  End-2 to 5      ).of(xs).visit(i => m += xs(i)); m } ==== 48
    T ~ { m = 0; (  End-2 to End    ).of(xs).visit(i => m += xs(i)); m } ==== 112
    T ~ { m = 0; (  End-2 to End-2  ).of(xs).visit(i => m += xs(i)); m } ==== 16
    T ~ { m = 0; (  End-2 to Start  ).of(xs).visit(i => m += xs(i)); m } ==== 0
    T ~ { m = 0; (  End-2 to Start+5).of(xs).visit(i => m += xs(i)); m } ==== 48
    T ~ { m = 0; (  Start to 3      ).of(xs).visit(i => m += xs(i)); m } ==== 15
    T ~ { m = 0; (  Start to End    ).of(xs).visit(i => m += xs(i)); m } ==== 127
    T ~ { m = 0; (  Start to End-2  ).of(xs).visit(i => m += xs(i)); m } ==== 31
    T ~ { m = 0; (  Start to Start  ).of(xs).visit(i => m += xs(i)); m } ==== 1
    T ~ { m = 0; (  Start to Start+1).of(xs).visit(i => m += xs(i)); m } ==== 3
    T ~ { m = 0; (Start+1 to 3      ).of(xs).visit(i => m += xs(i)); m } ==== 14
    T ~ { m = 0; (Start+2 to End    ).of(xs).visit(i => m += xs(i)); m } ==== 124
    T ~ { m = 0; (Start+1 to End-1  ).of(xs).visit(i => m += xs(i)); m } ==== 62
    T ~ { m = 0; (Start+0 to Start  ).of(xs).visit(i => m += xs(i)); m } ==== 1
    T ~ { m = 0; (Start+1 to Start+2).of(xs).visit(i => m += xs(i)); m } ==== 6

    val ss = "halibut"
    T ~ (      1 to End    ).of(xs) ==== (      1 to End    ).of(ss)
    T ~ (      2 to End-1  ).of(xs) ==== (      2 to End-1  ).of(ss)
    T ~ (      0 to Start  ).of(xs) ==== (      0 to Start  ).of(ss)
    T ~ (      2 to Start+3).of(xs) ==== (      2 to Start+3).of(ss)
    T ~ (    End to 5      ).of(xs) ==== (    End to 5      ).of(ss)
    T ~ (    End to End    ).of(xs) ==== (    End to End    ).of(ss)
    T ~ (    End to End-1  ).of(xs) ==== (    End to End-1  ).of(ss)
    T ~ (    End to Start  ).of(xs) ==== (    End to Start  ).of(ss)
    T ~ (    End to Start+6).of(xs) ==== (    End to Start+6).of(ss)
    T ~ (  End-2 to 5      ).of(xs) ==== (  End-2 to 5      ).of(ss)
    T ~ (  End-2 to End    ).of(xs) ==== (  End-2 to End    ).of(ss)
    T ~ (  End-2 to End-2  ).of(xs) ==== (  End-2 to End-2  ).of(ss)
    T ~ (  End-2 to Start  ).of(xs) ==== (  End-2 to Start  ).of(ss)
    T ~ (  End-2 to Start+5).of(xs) ==== (  End-2 to Start+5).of(ss)
    T ~ (  Start to 3      ).of(xs) ==== (  Start to 3      ).of(ss)
    T ~ (  Start to End    ).of(xs) ==== (  Start to End    ).of(ss)
    T ~ (  Start to End-2  ).of(xs) ==== (  Start to End-2  ).of(ss)
    T ~ (  Start to Start  ).of(xs) ==== (  Start to Start  ).of(ss)
    T ~ (  Start to Start+1).of(xs) ==== (  Start to Start+1).of(ss)
    T ~ (Start+1 to 3      ).of(xs) ==== (Start+1 to 3      ).of(ss)
    T ~ (Start+2 to End    ).of(xs) ==== (Start+2 to End    ).of(ss)
    T ~ (Start+1 to End-1  ).of(xs) ==== (Start+1 to End-1  ).of(ss)
    T ~ (Start+0 to Start  ).of(xs) ==== (Start+0 to Start  ).of(ss)
    T ~ (Start+1 to Start+2).of(xs) ==== (Start+1 to Start+2).of(ss)

    T ~ (      1 to End    ).of(xs) ==== (      1 to End    ).of(7)
    T ~ (      2 to End-1  ).of(xs) ==== (      2 to End-1  ).of(7)
    T ~ (      0 to Start  ).of(xs) ==== (      0 to Start  ).of(7)
    T ~ (      2 to Start+3).of(xs) ==== (      2 to Start+3).of(7)
    T ~ (    End to 5      ).of(xs) ==== (    End to 5      ).of(7)
    T ~ (    End to End    ).of(xs) ==== (    End to End    ).of(7)
    T ~ (    End to End-1  ).of(xs) ==== (    End to End-1  ).of(7)
    T ~ (    End to Start  ).of(xs) ==== (    End to Start  ).of(7)
    T ~ (    End to Start+6).of(xs) ==== (    End to Start+6).of(7)
    T ~ (  End-2 to 5      ).of(xs) ==== (  End-2 to 5      ).of(7)
    T ~ (  End-2 to End    ).of(xs) ==== (  End-2 to End    ).of(7)
    T ~ (  End-2 to End-2  ).of(xs) ==== (  End-2 to End-2  ).of(7)
    T ~ (  End-2 to Start  ).of(xs) ==== (  End-2 to Start  ).of(7)
    T ~ (  End-2 to Start+5).of(xs) ==== (  End-2 to Start+5).of(7)
    T ~ (  Start to 3      ).of(xs) ==== (  Start to 3      ).of(7)
    T ~ (  Start to End    ).of(xs) ==== (  Start to End    ).of(7)
    T ~ (  Start to End-2  ).of(xs) ==== (  Start to End-2  ).of(7)
    T ~ (  Start to Start  ).of(xs) ==== (  Start to Start  ).of(7)
    T ~ (  Start to Start+1).of(xs) ==== (  Start to Start+1).of(7)
    T ~ (Start+1 to 3      ).of(xs) ==== (Start+1 to 3      ).of(7)
    T ~ (Start+2 to End    ).of(xs) ==== (Start+2 to End    ).of(7)
    T ~ (Start+1 to End-1  ).of(xs) ==== (Start+1 to End-1  ).of(7)
    T ~ (Start+0 to Start  ).of(xs) ==== (Start+0 to Start  ).of(7)
    T ~ (Start+1 to Start+2).of(xs) ==== (Start+1 to Start+2).of(7)

    val iv = Iv.of(2 to 9)
    T ~ (      1 to End    ).of(iv) ==== Iv.of(1 to 9)
    T ~ (      2 to End-1  ).of(iv) ==== Iv.of(2 to 8)
    T ~ (      0 to Start  ).of(iv) ==== Iv.of(0 to 2)
    T ~ (      2 to Start+3).of(iv) ==== Iv.of(2 to 5)
    T ~ (    End to 5      ).of(iv) ==== Iv.of(9 to 5)
    T ~ (    End to End    ).of(iv) ==== Iv.of(9 to 9)
    T ~ (    End to End-1  ).of(iv) ==== Iv.of(9 to 8)
    T ~ (    End to Start  ).of(iv) ==== Iv.of(9 to 2)
    T ~ (    End to Start+6).of(iv) ==== Iv.of(9 to 8)
    T ~ (  End-2 to 5      ).of(iv) ==== Iv.of(7 to 5)
    T ~ (  End-2 to End    ).of(iv) ==== Iv.of(7 to 9)
    T ~ (  End-2 to End-2  ).of(iv) ==== Iv.of(7 to 7)
    T ~ (  End-2 to Start  ).of(iv) ==== Iv.of(7 to 2)
    T ~ (  End-2 to Start+5).of(iv) ==== Iv.of(7 to 7)
    T ~ (  Start to 3      ).of(iv) ==== Iv.of(2 to 3)
    T ~ (  Start to End    ).of(iv) ==== Iv.of(2 to 9)
    T ~ (  Start to End-2  ).of(iv) ==== Iv.of(2 to 7)
    T ~ (  Start to Start  ).of(iv) ==== Iv.of(2 to 2)
    T ~ (  Start to Start+1).of(iv) ==== Iv.of(2 to 3)
    T ~ (Start+1 to 3      ).of(iv) ==== Iv.of(3 to 3)
    T ~ (Start+2 to End    ).of(iv) ==== Iv.of(4 to 9)
    T ~ (Start+1 to End-1  ).of(iv) ==== Iv.of(3 to 8)
    T ~ (Start+0 to Start  ).of(iv) ==== Iv.of(2 to 2)
    T ~ (Start+1 to Start+2).of(iv) ==== Iv.of(3 to 4)


  val arrayTester = new ArraysTest()

  @Test def arrayInlinedDataTest: Unit = arrayTester.arrayInlinedDataTest()

  @Test def arrayClippedInlinedDataTest: Unit = arrayTester.arrayClippedInlinedDataTest()

  @Test def arrayBreakInlinedDataTest: Unit = arrayTester.arrayBreakInlinedDataTest()

  @Test def arrayFancyIntervalTest: Unit = arrayTester.arrayFancyIntervalTest()

  @Test def arrayPrimitiveDataTest: Unit = arrayTester.arrayPrimitiveDataTest()

  @Test def stringInlinedDataTest: Unit = arrayTester.stringInlinedDataTest()

  @Test def stringClippedInlinedDataTest: Unit = arrayTester.stringClippedInlinedDataTest()

  @Test def stringBreakInlinedDataTest: Unit = arrayTester.stringBreakInlinedDataTest()

  @Test def stringFancyIntervalTest: Unit = arrayTester.stringFancyIntervalTest()
}
object BasicsTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
