// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-24 Rex Kerr and Calico Life Sciences, LLC.

package kse.test.basics


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

  def ordinary(s: String): Char = s(2)

  def inlined(s: String): Char =
    import kse.basics.{given, _}
    s(2)

  def mktag(d: Double): Double \ "meter" =
    import labels._
    d \ "meter"
  
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
}

@RunWith(classOf[JUnit4])
class BasicsTest() {
  import kse.testutilities.TestUtilities.{_, given}
  import kse.basics.{_, given}
  import kse.basics.intervals._

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )


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
          2
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
    T ~ { shortcut.quittable{ x += 1; shortcut.quit();        x += 1 }; x } ==== 1
    T ~ { shortcut.quittable{ x += 1; shortcut.quitIf(x > 1); x += 1 }; x } ==== 2
    T ~ { shortcut.quittable{ x += 1; shortcut.quitIf(x > 3); x += 1 }; x } ==== 4
    T ~ { shortcut.skippable{ x += 1; shortcut.skip();        x += 1 }; x } ==== 5
    T ~ { shortcut.skippable{ x += 1; shortcut.skipIf(x > 5); x += 1 }; x } ==== 6
    T ~ { shortcut.skippable{ x += 1; shortcut.skipIf(x > 7); x += 1 }; x } ==== 8
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

    T ! """{ shortcut.hopped.quittable{ z += 1; Corral{ shortcut.hopped.quit();        z += 1 } }; z }"""
    T ! """{ shortcut.hopped.quittable{ z += 1; Corral{ shortcut.hopped.quitIf(z > 1); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.quittable{ z += 1; Corral{ shortcut.hopped.quitIf(z > 3); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.skippable{ z += 1; Corral{ shortcut.hopped.skip();        z += 1 } }; z }"""
    T ! """{ shortcut.hopped.skippable{ z += 1; Corral{ shortcut.hopped.skipIf(z > 5); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.skippable{ z += 1; Corral{ shortcut.hopped.skipIf(z > 7); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; Corral{ shortcut.hopped.quit(); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; Corral{ shortcut.hopped.skip(); z += 1 } }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; Corral{ shortcut.hopped.inner{ z += 1; shortcut.hopped.skip(); z += 1 }; z += 3 } }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; Corral{ shortcut.hopped.inner{ z += 1; shortcut.hopped.quit(); z += 1 }; z += 3 } }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; shortcut.hopped.inner{ z += 1; Corral{ shortcut.hopped.skip(); z += 1 } }; z += 3 }; z }"""
    T ! """{ shortcut.hopped.outer{ z += 2; shortcut.hopped.inner{ z += 1; Corral{ shortcut.hopped.quit(); z += 1 } }; z += 3 }; z }"""


  @Test
  def dataWrapperTest(): Unit =
    val m = Mu(5)
    T ~ m.value            ==== 5
    T ~ { m set 4 }        ==== Mu(4)
    T ~ { m.value = 3; m } ==== Mu(3)
    T ~ m.zap(_ - 1)       ==== Mu(2)
    T ~ m.toString         ==== "~2"
    T ~ m.copy             ==== Mu(2) --: typed[Mu.MuInt]

    object Meter extends NewType[Double] {}

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
    T ~ Mu.T(Meter(2.0)).zap(m => Meter(m.value+1)).pipe(x => (x, x.copy.set(Meter(3)))).sameOp(_.value) ==== (Meter(3), Meter(3)) --: typed[(Meter.Type, Meter.Type)]
    T ~ Mu.T(Meter(2.0)).getClass ==== Mu.MuDouble(2.0).getClass

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
    T ~ ab() ==== (2: Byte)  --: typed[Byte]
    T ~ as() ==== (2: Short) --: typed[Short]
    T ~ ac() ==== 'e'        --: typed[Char]
    T ~ ai() ==== 2          --: typed[Int]
    T ~ al() ==== 2L         --: typed[Long]
    T ~ af() ==== 0.5f       --: typed[Float]
    T ~ ad() ==== 0.5        --: typed[Double]
    T ~ aa() ==== "eel"      --: typed[String]
    T ~ am() ==== 1.5        --: typed[Meter.Type]
    T ~ { ab := 3;     ab swap 4 }      ==== (3: Byte)
    T ~ { as := 3;     as swap 4 }      ==== (3: Short)
    T ~ { ac := 'f';   ac swap 'g' }    ==== 'f'
    T ~ { ai := 3;     ai swap 4 }      ==== 3
    T ~ { al := 3L;    al swap 4L }     ==== 3L
    T ~ { af := 0.4f;  af swap 0.3f }   ==== 0.4f
    T ~ { ad := 0.4;   ad swap 0.3 }    ==== 0.4
    T ~ { aa := "cod"; aa swap "bass" } ==== "cod"
    T ~ { am := q(2);  am swap q(3) }   ==== 2.5
    T ~ ab.swapOp(b => (b+1).toByte)  ==== (4: Byte)
    T ~ as.swapOp(s => (s+1).toShort) ==== (4: Short)
    T ~ ac.swapOp(c => (c+1).toChar)  ==== 'g'
    T ~ ai.swapOp(_ + 1)              ==== 4
    T ~ al.swapOp(_ + 1L)             ==== 4L
    T ~ af.swapOp(_ - 0.1f)           ==== 0.3f
    T ~ ad.swapOp(_ - 0.1)            ==== 0.3
    T ~ aa.swapOp(_ + " cod")         ==== "bass"
    T ~ am.swapOp(m => q(m.value))    ==== 3.5
    T ~ ab(b => (b+2).toByte)  ==== (7: Byte)
    T ~ as(s => (s+2).toShort) ==== (7: Short)
    T ~ ac(c => (c+2).toChar)  ==== 'j'
    T ~ ai(_ + 2)              ==== 7
    T ~ al(_ + 2)              ==== 7L
    T ~ af(_ / 2.0f)           =~~= 0.1f
    T ~ ad(_ / 2.0)            =~~= 0.1
    T ~ aa(_ + " perch")       ==== "bass cod perch"
    T ~ am(m => q(m.value+1))  ==== 6.5

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
    T ~ Anon(Mu("cod"))             ==== typed[Anon[Mu.MuAny[String]]]
    T ~ Anon(gm("cod"))             ==== typed[Anon[Mu[String]]]

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
    T ~ "eel".fixIf(_.length > 3)(_ => "cod")                ==== "eel"
    T ~ "salmon".fixIf(_.length > 3)(_ => "cod")             ==== "cod"
    T ~ { var x = 0; "cod".tap(s => x = s.head).length + x } ==== (3 + 'c')
    T ~ "herring".tup(5)                                     ==== ("herring", 5)
    T ~ "herring".tupWith(_.length)                          ==== ("herring", 7)
    T ~ "herring".groupBy(__)                                ==== Map('h' -> "h", 'e' -> "e", 'r' -> "rr", 'i' -> "i", 'n' -> "n", 'g' -> "g")


  val tupleTester = new TuplesTest()


  @Test
  def unlabelledTupleTest(): Unit =
    tupleTester.unlabelledTuples()


  @Test
  def labeledTupleTest(): Unit =
    tupleTester.labelledSingleton()
    tupleTester.labelledDoublet()
    tupleTester.labelledTriplet()
    tupleTester.labelledQuadruplet()
    tupleTester.labelledQuintuplet()
    tupleTester.labelledSextuplet()
    tupleTester.labelledSeptuplet()
    tupleTester.labelledOctuplet()
    tupleTester.labelledNonuplet()


  @Test
  def intervalAndConstantTest(): Unit =
    var cuml = 0

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f
      cuml

    T ~ 3.where()                                                 =**= Array(0, 1, 2)
    T ~ -2.where()                                                =**= Array[Int]()
    T ~ 3.arrayed[Int]()                                          =**= Array(0, 0, 0)
    T ~ 3.arrayed[Int]()                                          ==== typed[Array[Int]]
    T ~ 3.arrayed(i => i+1)                                       =**= Array(1, 2, 3)
    T ~ 3.arrayed(i => i+1)                                       ==== typed[Array[Int]]
    T ~ 3.arrayedBreakably{ i => shortcut.skipIf(i%2 != 0); i+1 } =**= Array(1, 3)
    T ~ 3.arrayedBreakably{ i => shortcut.quitIf(i%2 != 0); i+1 } =**= Array(1)
    T ~ 3.arrayedBreakably(i => i+1)                              ==== typed[Array[Int]]
    T ~ n{ 3.times{ cuml = 2*cuml + 1 } }                         ==== 7
    T ~ n{ 5.visit(cuml += _) }                                   ==== 10
    T ~ n{ -2.visit(cuml += _) }                                  ==== 0

    T ~ (1 to End)                                 ==== typed[PIv]
    T ~ Iv.of(3 to 4)                              ==== Iv(3, 5)
    T ~ Iv.of("salmon")                            ==== Iv(0, 6)
    T ~ Iv.of(Array(1, 2, 3, 4))                   ==== Iv(0, 4)
    T ~ (1 to End-1).of(Array(1, 2, 3, 4))         ==== Iv(1, 3)  --: typed[Iv]
    T ~ (1 to End-1).of("abcd")                    ==== Iv(1, 3)  --: typed[Iv]
    T ~ Iv(3, 5).where()                           =**= Array(3, 4)
    T ~ Iv.of(3 to 4).where()                      =**= Array(3, 4)
    T ~ (1 to End-1).of(Array(1, 2, 3, 4)).where() =**= Array(1, 2)
    T ~ (1 to End-1).of("abcd").where()            =**= Array(1, 2)

    T ~ n{ Iv(3, 5).visit(cuml += _) }             ==== 7
    T ~ Iv(3, 5).i0                                ==== 3
    T ~ Iv(3, 5).iN                                ==== 5
    T ~ Iv(3, 5).i0To(1)                           ==== Iv(1, 5)
    T ~ Iv(3, 5).iNTo(7)                           ==== Iv(3, 7)
    T ~ Iv(3, 5).i0Op(_ + 1)                       ==== Iv(4, 5)
    T ~ Iv(3, 5).iNOp(_ - 1)                       ==== Iv(3, 4)
    T ~ Iv(3, 5).length                            ==== 2
    T ~ Iv(3, -2).length                           ==== 0
    T ~ Iv(3, 5).isEmpty                           ==== false
    T ~ Iv(3, -2).isEmpty                          ==== true
    T ~ Iv(3, 5).contains(2)                       ==== false
    T ~ Iv(3, 5).contains(3)                       ==== true
    T ~ Iv(3, 5).contains(4)                       ==== true
    T ~ Iv(3, 5).contains(5)                       ==== false
    T ~ Iv(-7, 9).clippedTo(Array(1, 2, 3))        ==== Iv(0, 3)
    T ~ Iv(2, 9).clippedTo(Array(1, 2, 3))         ==== Iv(2, 3)
    T ~ Iv(-7, 2).clippedTo(Array(1, 2, 3))        ==== Iv(0, 2)
    T ~ Iv(-7, 9).clippedTo("cod")                 ==== Iv(0, 3)
    T ~ Iv(2, 9).clippedTo("cod")                  ==== Iv(2, 3)
    T ~ Iv(-7, 2).clippedTo("cod")                 ==== Iv(0, 2)


  val arrayTester = new ArraysTest()

  @Test def arrayInlinedDataTest: Unit = arrayTester.arrayInlinedDataTest()

  @Test def arrayClippedInlinedDataTest: Unit = arrayTester.arrayClippedInlinedDataTest()

  @Test def arrayBreakInlinedDataTest: Unit = arrayTester.arrayBreakInlinedDataTest()

  @Test def arrayClipBreakIntervalTest: Unit = arrayTester.arrayClipBreakIntervalTest()

  @Test def arrayPrimitiveDataTest: Unit = arrayTester.arrayPrimitiveDataTest()

  @Test def stringInlinedDataTest: Unit = arrayTester.stringInlinedDataTest()

  @Test def stringClippedInlinedDataTest: Unit = arrayTester.stringClippedInlinedDataTest()

  @Test def stringBreakInlinedDataTest: Unit = arrayTester.stringBreakInlinedDataTest()

  @Test def stringClipBreakIntervalTest: Unit = arrayTester.stringClipBreakIntervalTest()
}
object BasicsTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
