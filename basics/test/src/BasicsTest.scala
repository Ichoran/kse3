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

class BytecodeCheck {
  def ordinary(s: String): Char = s(2)

  def inlined(s: String): Char =
    import kse.basics.{given, _}
    s(2)
}

@RunWith(classOf[JUnit4])
class BasicsTest {
  import kse.testutilities.TestUtilities.{_, given}
  import kse.basics.{_, given}
  import kse.basics.intervals._

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue _
  )


  @Test
  def dataWrapperTest(): Unit =
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



  object C extends NewType[Char] {
    extension (c: C.Type)
      def n: Int = c.value.toInt
      def l: Boolean = c.value.isLetter
      def o: O.Type = O(Some(c.value.toString))
  }
  extension (ac: Array[C.Type])
    def cs: String =
      val sb = new java.lang.StringBuilder()
      ac.peek()(sb append _.value)
      sb.toString

  object O extends NewType[Option[String]] {
    extension (o: O.Type)
      def n: Int = o.value.map(_.length).getOrElse(-1)
      def l: Boolean = o.value.isDefined
      def c: C.Type = C(o.value.map(s => if s.length > 0 then s(s.length - 1) else '$').getOrElse('#'))
  }
  extension (ao: Array[O.Type])
    def os: String =
      val sb = new java.lang.StringBuilder()
      ao.peek()(o => sb append (if o.asInstanceOf[AnyRef] eq null then "@" else o.value.map(_+".").getOrElse("#")))
      sb.toString

  extension (s: String)
    def c: Array[C.Type] = s.toCharArray.dupWith(c => C(c))

  @Test
  def arrayInlinedDataTest(): Unit =
    var cuml = 0
    val str = "ch.ix.#n."
    val car = str.c
    val oar = Array[O.Type](O(Some("ch")), O(Some("ix")), O(None), O(Some("n")))
    val ix = Array(2, 3, 1, 1, 3)
    val ex = Array(2, -9, 400, 3)

    val civ = Iv(3, 5)
    val oiv = Iv(1, 3)
    val cpv = 3 to End-4
    val opv = 1 to End-1

    def st = ix.stepper
    def et = ex.stepper

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f
      cuml

    T ~ car.cs ==== str
    T ~ oar.os ==== str

    T ~ car(End).value   ==== '.'
    T ~ oar(End).value   ==== Some("n")
    T ~ car(End-1).value ==== 'n'
    T ~ oar(End-1).value ==== None

    T ~ z{ car.peek()(cuml += _.n) }.cs       ==== str
    T ~ cuml                                  ==== str.map(_.toInt).sum
    T ~ z{ oar.peek()(cuml += _.n) }.os       ==== str
    T ~ cuml                                  ==== oar.map(_.n).sum
    T ~ z{ car.peek(3, 5)(cuml += _.n) }.cs   ==== str
    T ~ cuml                                  ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ oar.peek(1, 3)(cuml += _.n) }.os   ==== str
    T ~ cuml                                  ==== oar.slice(1, 3).map(_.n).sum
    T ~ z{ car.peek(civ)(cuml += _.n) }.cs    ==== str
    T ~ cuml                                  ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ oar.peek(oiv)(cuml += _.n) }.os    ==== str
    T ~ cuml                                  ==== oar.slice(1, 3).map(_.n).sum
    T ~ z{ car.peek(3 to 4)(cuml += _.n) }.cs ==== str
    T ~ cuml                                  ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ oar.peek(1 to 2)(cuml += _.n) }.os ==== str
    T ~ cuml                                  ==== oar.slice(1, 3).map(_.n).sum
    T ~ z{ car.peek(cpv)(cuml += _.n) }.cs    ==== str
    T ~ cuml                                  ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ oar.peek(opv)(cuml += _.n) }.os    ==== str
    T ~ cuml                                  ==== oar.slice(1, 3).map(_.n).sum
    T ~ z{ car.peek(ix)(cuml += _.n) }.cs     ==== str
    T ~ cuml                                  ==== ".ihhi".map(_.toInt).sum
    T ~ z{ oar.peek(ix)(cuml += _.n) }.os     ==== str
    T ~ cuml                                  ==== ix.map(i => oar(i).n).sum
    T ~ z{ car.peek(st)(cuml += _.n) }.cs     ==== str
    T ~ cuml                                  ==== ".ihhi".map(_.toInt).sum
    T ~ z{ oar.peek(st)(cuml += _.n) }.os     ==== str
    T ~ cuml                                  ==== ix.map(i => oar(i).n).sum

    T ~ n{ car.visit()(cuml += _.n + _) }       ==== str.map(_.toInt).sum + str.length*(str.length-1)/2
    T ~ n{ oar.visit()(cuml += _.n + _) }       ==== oar.map(_.n).sum + oar.length*(oar.length-1)/2
    T ~ n{ car.visit(3, 5)(cuml += _.n + _) }   ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ oar.visit(1, 3)(cuml += _.n + _) }   ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ n{ car.visit(civ)(cuml += _.n + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ oar.visit(oiv)(cuml += _.n + _) }    ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ n{ car.visit(3 to 4)(cuml += _.n + _) } ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ oar.visit(1 to 2)(cuml += _.n + _) } ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ n{ car.visit(cpv)(cuml += _.n + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ oar.visit(opv)(cuml += _.n + _) }    ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ n{ car.visit(ix)(cuml += _.n + _) }     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ n{ oar.visit(ix)(cuml += _.n + _) }     ==== ix.map(i => oar(i).n).sum + ix.sum
    T ~ n{ car.visit(st)(cuml += _.n + _) }     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ n{ oar.visit(st)(cuml += _.n + _) }     ==== ix.map(i => oar(i).n).sum + ix.sum

    T ~ n{ car.wander(){  (c, i) => cuml += c.n; i+2 } } ==== str.grouped(2).map(_(0).toInt).sum
    T ~ n{ oar.wander(){  (o, i) => cuml += o.n; i+2 } } ==== Array(0, 2).map(i => oar(i).n).sum
    T ~ n{ car.wander(1){ (c, i) => cuml += c.n; i+2 } } ==== str.grouped(2).filter(_.length == 2).map(_(1).toInt).sum
    T ~ n{ oar.wander(1){ (o, i) => cuml += o.n; i+2 } } ==== Array(1, 3).map(i => oar(i).n).sum
    T ~    car.wander(){  (_, i) =>              i+2 }   ==== str.grouped(2).map(_(0).toInt).length
    T ~    oar.wander(){  (_, i) =>              i+2 }   ==== Array(0, 2).map(i => oar(i).n).length
    T ~    car.wander(1){ (_, i) =>              i+2 }   ==== str.grouped(2).filter(_.length == 2).map(_(1).toInt).length
    T ~    oar.wander(1){ (_, i) =>              i+2 }   ==== Array(1, 3).map(i => oar(i).n).length

    T ~ car.gather(0)()(_ + _.n + _)       ==== str.map(_.toInt).sum + str.length*(str.length-1)/2
    T ~ oar.gather(0)()(_ + _.n + _)       ==== oar.map(_.n).sum + oar.length*(oar.length-1)/2
    T ~ car.gather(0)(3, 5)(_ + _.n + _)   ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ oar.gather(0)(1, 3)(_ + _.n + _)   ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ car.gather(0)(civ)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ oar.gather(0)(oiv)(_ + _.n + _)    ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ car.gather(0)(3 to 4)(_ + _.n + _) ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ oar.gather(0)(1 to 2)(_ + _.n + _) ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ car.gather(0)(cpv)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ oar.gather(0)(opv)(_ + _.n + _)    ==== oar.slice(1, 3).map(_.n).sum + 3
    T ~ car.gather(0)(ix)(_ + _.n + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ oar.gather(0)(ix)(_ + _.n + _)     ==== ix.map(i => oar(i).n).sum + ix.sum
    T ~ car.gather(0)(st)(_ + _.n + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ oar.gather(0)(st)(_ + _.n + _)     ==== ix.map(i => oar(i).n).sum + ix.sum

    T ~ car.dup()                  =**= car
    T ~ (car.dup() eq car)         ==== false
    T ~ oar.dup()                  =**= oar
    T ~ (oar.dup() eq oar)         ==== false
    T ~ car.dup(_(0) = C('s')).cs  ==== "sh.ix.#n."
    T ~ oar.dup(_(0) = O(None)).os ==== "#ix.#n."
    T ~ car.dupWith(_.n)           =**= car.map(_.n)
    T ~ oar.dupWith(_.n)           =**= oar.map(_.n)

    T ~ car.addLeft(2).cs          ==== "\u0000\u0000ch.ix.#n."
    T ~ oar.addLeft(2).os          ==== "@@ch.ix.#n."
    T ~ car.addLeft(2, C('+')).cs  ==== "++ch.ix.#n."
    T ~ oar.addLeft(2, O(None)).os ==== "##ch.ix.#n."
    T ~ car.addRight(2).cs          ==== "ch.ix.#n.\u0000\u0000"
    T ~ oar.addRight(2).os          ==== "ch.ix.#n.@@"
    T ~ car.addRight(2, C('+')).cs  ==== "ch.ix.#n.++"
    T ~ oar.addRight(2, O(None)).os ==== "ch.ix.#n.##"

    T ~ car.where(_.l) =**= car.zipWithIndex.collect{ case (c, i) if c.l => i }
    T ~ oar.where(_.l) =**= oar.zipWithIndex.collect{ case (o, i) if o.l => i }

    T ~ car.dup(_() = C('x')).cs          ==== "xxxxxxxxx"
    T ~ oar.dup(_() = O(None)).os         ==== "####"
    T ~ car.dup(_() = "abcdefghi".c).cs   ==== "abcdefghi"
    T ~ oar.dup(_() = oar.reverse)        =**= oar.reverse
    T ~ car.dup(_(civ) = C('x')).cs       ==== "ch.xx.#n."
    T ~ oar.dup(_(oiv) = O(None)).os      ==== "ch.##n."
    T ~ car.dup(_(civ) = "12".c).cs       ==== "ch.12.#n."
    T ~ oar.dup(_(oiv) = oar).os          ==== "ch.ch.ix.n."
    T ~ car.dup(_(3 to 4) = C('x')).cs    ==== "ch.xx.#n."
    T ~ oar.dup(_(1 to 2) = O(None)).os   ==== "ch.##n."
    T ~ car.dup(_(3 to 4) = "12".c).cs    ==== "ch.12.#n."
    T ~ oar.dup(_(1 to 2) = oar).os       ==== "ch.ch.ix.n."
    T ~ car.dup(_(cpv) = C('x')).cs       ==== "ch.xx.#n."
    T ~ oar.dup(_(opv) = O(None)).os      ==== "ch.##n."
    T ~ car.dup(_(cpv) = "12".c).cs       ==== "ch.12.#n."
    T ~ oar.dup(_(opv) = oar).os          ==== "ch.ch.ix.n."
    T ~ car.dup(_(ix) = C('x')).cs        ==== "cxxxx.#n."
    T ~ oar.dup(_(ix) = O(None)).os       ==== "ch.###"
    T ~ car.dup(_(st) = C('x')).cs        ==== "cxxxx.#n."
    T ~ oar.dup(_(st) = O(None)).os       ==== "ch.###"
    T ~ car.dup(_(_.l) = C('x')).cs       ==== "xx.xx.#x."
    T ~ oar.dup(_(_.l) = O(Some("e"))).os ==== "e.e.#e."

    inline def gc(run: (() => C.Type) => Array[C.Type]): String =
      var x = '0'
      val f = () => { x = (x+1).toChar; C(x) }
      run(f).cs
    inline def go(run: (() => O.Type) => Array[O.Type]): String =
      var x = ""
      val f = () => { x = x + "!"; O(Some(x)) }
      run(f).os
    inline def ic(run: (Int => C.Type) => Array[C.Type]): String =
      val f = (i: Int) => C(('0' + i).toChar)
      run(f).cs
    inline def io(run: (Int => O.Type) => Array[O.Type]): String =
      val f = (i: Int) => O(Some("!"*i))
      run(f).os
    inline def fc(run: ((C.Type, Int) => C.Type) => Array[C.Type]): String =
      val f = (c: C.Type, i: Int) => C((c.value + i).toChar)
      run(f).cs
    inline def fo(run: ((O.Type, Int) => O.Type) => Array[O.Type]): String =
      val f = (o: O.Type, i: Int) => O(o.value.map(_ + "!"*i) orElse Some(i.toString))
      run(f).os
    T ~ gc{ f => car.dup(_.set()(f)) }       ==== "123456789"
    T ~ go{ f => oar.dup(_.set()(f)) }       ==== "!.!!.!!!.!!!!."
    T ~ ic{ f => car.dup(_.set()(f)) }       ==== "012345678"
    T ~ io{ f => oar.dup(_.set()(f)) }       ==== ".!.!!.!!!."
    T ~ fc{ f => car.dup(_.set()(f)) }       ==== "ci0l|3)u6"
    T ~ fo{ f => oar.dup(_.set()(f)) }       ==== "ch.ix!.2.n!!!."
    T ~ gc{ f => car.dup(_.set(3, 5)(f)) }   ==== "ch.12.#n."
    T ~ go{ f => oar.dup(_.set(1, 3)(f)) }   ==== "ch.!.!!.n."
    T ~ ic{ f => car.dup(_.set(3, 5)(f)) }   ==== "ch.34.#n."
    T ~ io{ f => oar.dup(_.set(1, 3)(f)) }   ==== "ch.!.!!.n."
    T ~ fc{ f => car.dup(_.set(3, 5)(f)) }   ==== "ch.l|.#n."
    T ~ fo{ f => oar.dup(_.set(1, 3)(f)) }   ==== "ch.ix!.2.n."
    T ~ gc{ f => car.dup(_.set(3 to 4)(f)) } ==== "ch.12.#n."
    T ~ go{ f => oar.dup(_.set(1 to 2)(f)) } ==== "ch.!.!!.n."
    T ~ ic{ f => car.dup(_.set(3 to 4)(f)) } ==== "ch.34.#n."
    T ~ io{ f => oar.dup(_.set(1 to 2)(f)) } ==== "ch.!.!!.n."
    T ~ fc{ f => car.dup(_.set(3 to 4)(f)) } ==== "ch.l|.#n."
    T ~ fo{ f => oar.dup(_.set(1 to 2)(f)) } ==== "ch.ix!.2.n."
    T ~ gc{ f => car.dup(_.set(civ)(f)) }    ==== "ch.12.#n."
    T ~ go{ f => oar.dup(_.set(oiv)(f)) }    ==== "ch.!.!!.n."
    T ~ ic{ f => car.dup(_.set(civ)(f)) }    ==== "ch.34.#n."
    T ~ io{ f => oar.dup(_.set(oiv)(f)) }    ==== "ch.!.!!.n."
    T ~ fc{ f => car.dup(_.set(civ)(f)) }    ==== "ch.l|.#n."
    T ~ fo{ f => oar.dup(_.set(oiv)(f)) }    ==== "ch.ix!.2.n."
    T ~ gc{ f => car.dup(_.set(cpv)(f)) }    ==== "ch.12.#n."
    T ~ go{ f => oar.dup(_.set(opv)(f)) }    ==== "ch.!.!!.n."
    T ~ ic{ f => car.dup(_.set(cpv)(f)) }    ==== "ch.34.#n."
    T ~ io{ f => oar.dup(_.set(opv)(f)) }    ==== "ch.!.!!.n."
    T ~ fc{ f => car.dup(_.set(cpv)(f)) }    ==== "ch.l|.#n."
    T ~ fo{ f => oar.dup(_.set(opv)(f)) }    ==== "ch.ix!.2.n."
    T ~ gc{ f => car.dup(_.set(ix)(f)) }     ==== "c415x.#n."
    T ~ go{ f => oar.dup(_.set(ix)(f)) }     ==== "ch.!!!!.!.!!!!!."
    T ~ ic{ f => car.dup(_.set(ix)(f)) }     ==== "c123x.#n."
    T ~ io{ f => oar.dup(_.set(ix)(f)) }     ==== "ch.!.!!.!!!."
    T ~ fc{ f => car.dup(_.set(ix)(f)) }     ==== "cj0ox.#n."
    T ~ fo{ f => oar.dup(_.set(ix)(f)) }     ==== "ch.ix!!.2.n!!!!!!."
    T ~ gc{ f => car.dup(_.set(st)(f)) }     ==== "c415x.#n."
    T ~ go{ f => oar.dup(_.set(st)(f)) }     ==== "ch.!!!!.!.!!!!!."
    T ~ ic{ f => car.dup(_.set(st)(f)) }     ==== "c123x.#n."
    T ~ io{ f => oar.dup(_.set(st)(f)) }     ==== "ch.!.!!.!!!."
    T ~ fc{ f => car.dup(_.set(st)(f)) }     ==== "cj0ox.#n."
    T ~ fo{ f => oar.dup(_.set(st)(f)) }     ==== "ch.ix!!.2.n!!!!!!."
    T ~ gc{ f => car.dup(_.set(_.l)(f)) }    ==== "12.34.#5."
    T ~ go{ f => oar.dup(_.set(_.l)(f)) }    ==== "!.!!.#!!!."
    T ~ ic{ f => car.dup(_.set(_.l)(f)) }    ==== "01.34.#7."
    T ~ io{ f => oar.dup(_.set(_.l)(f)) }    ==== ".!.#!!!."

    val cx = "___________".c
    val ox = Array.fill(6)(O(Some("_")))
    var ninja = 0
    T ~ cx.dup(a => ninja = car.inject(a)).cs            ==== "ch.ix.#n.__"
    T ~ { val x = ninja; ninja = 0; x }                  ==== car.length
    T ~ ox.dup(a => ninja = oar.inject(a)).os            ==== "ch.ix.#n._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== oar.length
    T ~ cx.dup(a => ninja = car.inject(a, 2)).cs         ==== "__ch.ix.#n."
    T ~ { val x = ninja; ninja = 0; x }                  ==== car.length
    T ~ ox.dup(a => ninja = oar.inject(a, 2)).os         ==== "_._.ch.ix.#n."
    T ~ { val x = ninja; ninja = 0; x }                  ==== oar.length
    T ~ cx.dup(a => ninja = car.inject(a)(3, 5)).cs      ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a)(1, 3)).os      ==== "ix.#_._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a, 2)(3, 5)).cs   ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(1, 3)).os   ==== "_._.ix.#_._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a)(3 to 4)).cs    ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a)(1 to 2)).os    ==== "ix.#_._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a, 2)(3 to 4)).cs ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(1 to 2)).os ==== "_._.ix.#_._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a)(3, 5)).cs      ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a)(1, 3)).os      ==== "ix.#_._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a, 2)(3, 5)).cs   ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(1, 3)).os   ==== "_._.ix.#_._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a)(civ)).cs       ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a)(oiv)).os       ==== "ix.#_._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a, 2)(civ)).cs    ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(oiv)).os    ==== "_._.ix.#_._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a)(cpv)).cs       ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a)(opv)).os       ==== "ix.#_._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a, 2)(cpv)).cs    ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(opv)).os    ==== "_._.ix.#_._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 2
    T ~ cx.dup(a => ninja = car.inject(a)(ix)).cs        ==== ".ihhi______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ ox.dup(a => ninja = oar.inject(a)(ix)).os        ==== "#n.ix.ix.n._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ cx.dup(a => ninja = car.inject(a, 1)(ix)).cs     ==== "_.ihhi_____"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ ox.dup(a => ninja = oar.inject(a, 1)(ix)).os     ==== "_.#n.ix.ix.n."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ cx.dup(a => ninja = car.inject(a)(st)).cs        ==== ".ihhi______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ ox.dup(a => ninja = oar.inject(a)(st)).os        ==== "#n.ix.ix.n._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ cx.dup(a => ninja = car.inject(a, 1)(st)).cs     ==== "_.ihhi_____"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ ox.dup(a => ninja = oar.inject(a, 1)(st)).os     ==== "_.#n.ix.ix.n."
    T ~ { val x = ninja; ninja = 0; x }                  ==== 5
    T ~ cx.dup(a => ninja = car.inject(a)(_.l)).cs       ==== "chixn______"
    T ~ { val x = ninja; ninja = 0; x }                  ==== car.count(_.l)
    T ~ ox.dup(a => ninja = oar.inject(a)(_.l)).os       ==== "ch.ix.n._._._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== oar.count(_.l)
    T ~ cx.dup(a => ninja = car.inject(a, 2)(_.l)).cs    ==== "__chixn____"
    T ~ { val x = ninja; ninja = 0; x }                  ==== car.count(_.l)
    T ~ ox.dup(a => ninja = oar.inject(a, 2)(_.l)).os    ==== "_._.ch.ix.n._."
    T ~ { val x = ninja; ninja = 0; x }                  ==== oar.count(_.l)

    T ~ car.select(3, 5).cs   ==== "ix"
    T ~ oar.select(1, 3).os   ==== "ix.#"
    T ~ car.select(3 to 4).cs ==== "ix"
    T ~ oar.select(1 to 2).os ==== "ix.#"
    T ~ car.select(civ).cs    ==== "ix"
    T ~ oar.select(oiv).os    ==== "ix.#"
    T ~ car.select(cpv).cs    ==== "ix"
    T ~ oar.select(opv).os    ==== "ix.#"
    T ~ car.select(ix).cs     ==== ".ihhi"
    T ~ oar.select(ix).os     ==== "#n.ix.ix.n."
    T ~ car.select(st).cs     ==== ".ihhi"
    T ~ oar.select(st).os     ==== "#n.ix.ix.n."
    T ~ car.select(_.l).cs    ==== "chixn"
    T ~ oar.select(_.l).os    ==== "ch.ix.n."

    T ~ car.selectOp()(      (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "#h.#i.#..#n.#"
    T ~ oar.selectOp()(      (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "-x-n"
    T ~ car.selectOp(3, 5)(  (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "i.#"
    T ~ oar.selectOp(1, 3)(  (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "x-"
    T ~ car.selectOp(3 to 4)((c, i) => if i%2 == 0 then O(None) else c.o).os ==== "i.#"
    T ~ oar.selectOp(1 to 2)((o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "x-"
    T ~ car.selectOp(civ)(   (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "i.#"
    T ~ oar.selectOp(oiv)(   (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "x-"
    T ~ car.selectOp(cpv)(   (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "i.#"
    T ~ oar.selectOp(opv)(   (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "x-"
    T ~ car.selectOp(ix)(    (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "#i.h.h.i."
    T ~ oar.selectOp(ix)(    (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "-nxxn"
    T ~ car.selectOp(st)(    (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "#i.h.h.i."
    T ~ oar.selectOp(st)(    (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "-nxxn"
    T ~ car.selectOp(_.l)(   (c, i) => if i%2 == 0 then O(None) else c.o).os ==== "#h.i.#n."
    T ~ oar.selectOp(_.l)(   (o, i) => if i%2 == 0 then C('-')  else o.c).cs ==== "-xn"

    T ~ car.fusion[Int]((c, i, add) => if !c.l then add(i) else Array(c.o).os.foreach(c => add(c.toInt))) =**= Array(99, 46, 104, 46, 2, 105, 46, 120, 46, 5, 6, 110, 46, 8)


  @Test
  def arrayClippedIntervalDataTest: Unit =
    var cuml = 0
    val str = "ch.#ik."
    val car = str.c

    val ix = Array(2, 3, 1, 1, 3)
    val ex = Array(2, 0, -1, 3, 6, 7, -9, 400) 
    def st = ix.stepper
    def et = ex.stepper

    val civ = Iv(3, 5)
    val cpv = 3 to End-2
    val eiv = Iv(3, 9)
    val fiv = Iv(-2, 5)
    val fpv = End-9 to End-2
    val biv = Iv(-2, 9)
    val niv = Iv(8, 9)
    val npv = End-9 to End-8

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f
      cuml

    T ~ (0 to End + 2) ==== thrown[IllegalArgumentException]

    T ~ z{ car.clip.peek(3, 5)(cuml += _.n) }.cs   ==== str
    T ~ cuml                                       ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.clip.peek(3 to 4)(cuml += _.n) }.cs ==== str
    T ~ cuml                                       ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.clip.peek(civ)(cuml += _.n) }.cs    ==== str
    T ~ cuml                                       ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.clip.peek(cpv)(cuml += _.n) }.cs    ==== str
    T ~ cuml                                       ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.clip.peek(ix)(cuml += _.n) }.cs     ==== str
    T ~ cuml                                       ==== ".#hh#".map(_.toInt).sum
    T ~ z{ car.clip.peek(st)(cuml += _.n) }.cs     ==== str
    T ~ cuml                                       ==== ".#hh#".map(_.toInt).sum

    T ~ n{ car.clip.peek(3, 9)(cuml += _.n) }    ==== str.substring(3).map(_.toInt).sum
    T ~ n{ car.clip.peek(3 to 8)(cuml += _.n) }  ==== str.substring(3).map(_.toInt).sum
    T ~ n{ car.clip.peek(eiv)(cuml += _.n) }     ==== str.substring(3).map(_.toInt).sum
    T ~    car.     peek(3, 9)(cuml += _.n)      ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(3 to 8)(cuml += _.n)    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(eiv)(cuml += _.n)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.peek(-2, 5)(cuml += _.n) }   ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.clip.peek(-2 to 4)(cuml += _.n) } ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.clip.peek(fiv)(cuml += _.n) }     ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.clip.peek(fpv)(cuml += _.n) }     ==== str.substring(0, 5).map(_.toInt).sum
    T ~    car.     peek(-2, 5)(cuml += _.n)     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(-2 to 4)(cuml += _.n)   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(fiv)(cuml += _.n)       ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(fpv)(cuml += _.n)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.peek(-2, 9)(cuml += _.n) }   ==== str.map(_.toInt).sum
    T ~ n{ car.clip.peek(-2 to 9)(cuml += _.n) } ==== str.map(_.toInt).sum
    T ~ n{ car.clip.peek(biv)(cuml += _.n) }     ==== str.map(_.toInt).sum
    T ~    car.     peek(-2, 9)(cuml += _.n)     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(-2 to 9)(cuml += _.n)   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(biv)(cuml += _.n)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.peek(8, 9)(cuml += _.n) }    ==== 0
    T ~ n{ car.clip.peek(8 to 9)(cuml += _.n) }  ==== 0
    T ~ n{ car.clip.peek(niv)(cuml += _.n) }     ==== 0
    T ~ n{ car.clip.peek(npv)(cuml += _.n) }     ==== 0
    T ~    car.     peek(8, 9)(cuml += _.n)      ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(8 to 9)(cuml += _.n)    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(niv)(cuml += _.n)       ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(npv)(cuml += _.n)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.peek(ex)(cuml += _.n) }      ==== ".c#.".map(_.toInt).sum
    T ~ n{ car.clip.peek(et)(cuml += _.n) }      ==== ".c#.".map(_.toInt).sum
    T ~    car.     peek(ex)(cuml += _.n)        ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     peek(et)(cuml += _.n)        ==== thrown[ArrayIndexOutOfBoundsException]

    def sm(i: Int, j: Int) = j*(j+1)/2 - i*(i-1)/2
    T ~ n{ car.clip.visit(3, 5)(cuml += _.n + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ car.clip.visit(3 to 4)(cuml += _.n + _) }  ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ car.clip.visit(civ)(cuml += _.n + _) }     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ car.clip.visit(cpv)(cuml += _.n + _) }     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ car.clip.visit(ix)(cuml += _.n + _) }      ==== ".#hh#".map(_.toInt).sum + 10
    T ~ n{ car.clip.visit(st)(cuml += _.n + _) }      ==== ".#hh#".map(_.toInt).sum + 10

    T ~ n{ car.clip.visit(3, 9)(cuml += _.n + _) }    ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ n{ car.clip.visit(3 to 8)(cuml += _.n + _) }  ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ n{ car.clip.visit(eiv)(cuml += _.n + _) }     ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~    car.     visit(3, 9)(cuml += _.n + _)      ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(3 to 8)(cuml += _.n + _)    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(eiv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.visit(-2, 5)(cuml += _.n + _) }   ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ car.clip.visit(-2 to 4)(cuml += _.n + _) } ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ car.clip.visit(fiv)(cuml += _.n + _) }     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ car.clip.visit(fpv)(cuml += _.n + _) }     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~    car.     visit(-2, 5)(cuml += _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(-2 to 4)(cuml += _.n + _)   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(fiv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(fpv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.visit(-2, 9)(cuml += _.n + _) }   ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ n{ car.clip.visit(-2 to 9)(cuml += _.n + _) } ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ n{ car.clip.visit(biv)(cuml += _.n + _) }     ==== str.map(_.toInt).sum + sm(0, 6)
    T ~    car.     visit(-2, 9)(cuml += _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(-2 to 9)(cuml += _.n + _)   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(biv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.visit(8, 9)(cuml += _.n + _) }    ==== 0
    T ~ n{ car.clip.visit(8 to 9)(cuml += _.n + _) }  ==== 0
    T ~ n{ car.clip.visit(niv)(cuml += _.n + _) }     ==== 0
    T ~ n{ car.clip.visit(npv)(cuml += _.n + _) }     ==== 0
    T ~    car.     visit(8, 9)(cuml += _.n + _)      ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(8 to 9)(cuml += _.n + _)    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(niv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(npv)(cuml += _.n + _)       ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ n{ car.clip.visit(ex)(cuml += _.n + _) }      ==== ".c#.".map(_.toInt).sum + 11
    T ~ n{ car.clip.visit(et)(cuml += _.n + _) }      ==== ".c#.".map(_.toInt).sum + 11
    T ~    car.     visit(ex)(cuml += _.n + _)        ==== thrown[ArrayIndexOutOfBoundsException]
    T ~    car.     visit(et)(cuml += _.n + _)        ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.gather(0)(3, 5)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.gather(0)(3 to 4)(_ + _.n + _)  ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.gather(0)(civ)(_ + _.n + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.gather(0)(cpv)(_ + _.n + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.gather(0)(ix)(_ + _.n + _)      ==== ".#hh#".map(_.toInt).sum + 10
    T ~ car.clip.gather(0)(st)(_ + _.n + _)      ==== ".#hh#".map(_.toInt).sum + 10

    T ~ car.clip.gather(0)(3, 9)(_ + _.n + _)    ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ car.clip.gather(0)(3 to 8)(_ + _.n + _)  ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ car.clip.gather(0)(eiv)(_ + _.n + _)     ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ car.     gather(0)(3, 9)(_ + _.n + _)    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(3 to 8)(_ + _.n + _)  ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(eiv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.gather(0)(-2, 5)(_ + _.n + _)   ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.gather(0)(-2 to 4)(_ + _.n + _) ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.gather(0)(fiv)(_ + _.n + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.gather(0)(fpv)(_ + _.n + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.     gather(0)(-2, 5)(_ + _.n + _)   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(-2 to 4)(_ + _.n + _) ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(fiv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(fpv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.gather(0)(-2, 9)(_ + _.n + _)   ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ car.clip.gather(0)(-2 to 9)(_ + _.n + _) ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ car.clip.gather(0)(biv)(_ + _.n + _)     ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ car.     gather(0)(-2, 9)(_ + _.n + _)   ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(-2 to 9)(_ + _.n + _) ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(biv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.gather(0)(8, 9)(_ + _.n + _)    ==== 0
    T ~ car.clip.gather(0)(8 to 9)(_ + _.n + _)  ==== 0
    T ~ car.clip.gather(0)(niv)(_ + _.n + _)     ==== 0
    T ~ car.clip.gather(0)(npv)(_ + _.n + _)     ==== 0
    T ~ car.     gather(0)(8, 9)(_ + _.n + _)    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(8 to 9)(_ + _.n + _)  ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(niv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(npv)(_ + _.n + _)     ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.clip.gather(0)(ex)(_ + _.n + _)      ==== ".c#.".map(_.toInt).sum + 11
    T ~ car.clip.gather(0)(et)(_ + _.n + _)      ==== ".c#.".map(_.toInt).sum + 11
    T ~ car.     gather(0)(ex)(_ + _.n + _)      ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.     gather(0)(et)(_ + _.n + _)      ==== thrown[ArrayIndexOutOfBoundsException]

    val ca9 = "ABCDEFGHI".c
    val ca7 = "1234567".c
    val ca3 = "890".c
    val ca1 = "%".c

    T ~ car.dup(_.clip() = ca7).cs          ==== "1234567"
    T ~ car.dup(_.clip(civ) = C('x')).cs    ==== "ch.xxk."
    T ~ car.dup(_.clip(civ) = ca3).cs       ==== "ch.89k."
    T ~ car.dup(_.clip(3 to 4) = C('x')).cs ==== "ch.xxk."
    T ~ car.dup(_.clip(3 to 4) = ca3).cs    ==== "ch.89k."
    T ~ car.dup(_.clip(cpv) = C('x')).cs    ==== "ch.xxk."
    T ~ car.dup(_.clip(cpv) = ca3).cs       ==== "ch.89k."
    T ~ car.dup(_.clip(ix) = C('x')).cs     ==== "cxxxik."
    T ~ car.dup(_.clip(ix) = ca7).cs        ==== "c415ik."    
    T ~ car.dup(_.clip(st) = C('x')).cs     ==== "cxxxik."
    T ~ car.dup(_.clip(st) = ca7).cs        ==== "c415ik."

    T ~ car.dup(_.clip() = ca3).cs ==== "890#ik."
    T ~ car.dup(_() = ca3).cs      ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup(_.clip(eiv)    = C('x')).cs ==== "ch.xxxx"
    T ~ car.dup(_.clip(3 to 8) = C('x')).cs ==== "ch.xxxx"
    T ~ car.dup(_.clip(eiv)    = ca7).cs    ==== "ch.1234"
    T ~ car.dup(_.clip(3 to 8) = ca7).cs    ==== "ch.1234"
    T ~ car.dup(_.clip(eiv)    = ca3).cs    ==== "ch.890."
    T ~ car.dup(_.clip(3 to 8) = ca3).cs    ==== "ch.890."
    T ~ car.dup(_(eiv)         = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(3 to 8)      = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(eiv)         = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(3 to 8)      = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(eiv)         = ca3).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(3 to 8)      = ca3).cs    ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup(_.clip(fiv)     = C('x')).cs ==== "xxxxxk."
    T ~ car.dup(_.clip(fpv)     = C('x')).cs ==== "xxxxxk."
    T ~ car.dup(_.clip(-2 to 4) = C('x')).cs ==== "xxxxxk."
    T ~ car.dup(_.clip(fiv)     = ca7).cs    ==== "12345k."
    T ~ car.dup(_.clip(fpv)     = ca7).cs    ==== "12345k."
    T ~ car.dup(_.clip(-2 to 4) = ca7).cs    ==== "12345k."
    T ~ car.dup(_.clip(fiv)     = ca3).cs    ==== "890#ik."
    T ~ car.dup(_.clip(fpv)     = ca3).cs    ==== "890#ik."
    T ~ car.dup(_.clip(-2 to 4) = ca3).cs    ==== "890#ik."
    T ~ car.dup(_(fiv)          = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(fpv)          = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(-2 to 4)      = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(fiv)          = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(fpv)          = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(-2 to 4)      = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup(_.clip(biv)     = C('x')).cs ==== "xxxxxxx"
    T ~ car.dup(_.clip(-2 to 8) = C('x')).cs ==== "xxxxxxx"
    T ~ car.dup(_.clip(biv)     = ca7).cs    ==== "1234567"
    T ~ car.dup(_.clip(-2 to 8) = ca7).cs    ==== "1234567"
    T ~ car.dup(_.clip(biv)     = ca3).cs    ==== "890#ik."
    T ~ car.dup(_.clip(-2 to 8) = ca3).cs    ==== "890#ik."

    T ~ car.dup(_.clip(niv)    = C('x')).cs ==== "ch.#ik."
    T ~ car.dup(_.clip(npv)    = C('x')).cs ==== "ch.#ik."
    T ~ car.dup(_.clip(8 to 9) = C('x')).cs ==== "ch.#ik."
    T ~ car.dup(_.clip(niv)    = ca1).cs    ==== "ch.#ik."
    T ~ car.dup(_.clip(npv)    = ca1).cs    ==== "ch.#ik."
    T ~ car.dup(_.clip(8 to 9) = ca1).cs    ==== "ch.#ik."
    T ~ car.dup(_(niv)         = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(npv)         = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(8 to 9)      = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(niv)         = ca1).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(npv)         = ca1).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(8 to 9)      = ca1).cs    ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup(_.clip(ix) = ca3).cs ==== "c089ik."    
    T ~ car.dup(_.clip(st) = ca3).cs ==== "c089ik."    
    T ~ car.dup(_(ix)      = ca3).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(st)      = ca3).cs ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ car.dup(_.clip(ex) = C('x')).cs ==== "xhxxikx"
    T ~ car.dup(_.clip(et) = C('x')).cs ==== "xhxxikx"
    T ~ car.dup(_.clip(ex) = ca7).cs    ==== "2h13ik4"
    T ~ car.dup(_.clip(et) = ca7).cs    ==== "2h13ik4"
    T ~ car.dup(_.clip(ex) = ca3).cs    ==== "9h80ik."
    T ~ car.dup(_.clip(et) = ca3).cs    ==== "9h80ik."
    T ~ car.dup(_(ex)      = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(et)      = C('x')).cs ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(ex)      = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ car.dup(_(et)      = ca7).cs    ==== thrown[ArrayIndexOutOfBoundsException]

    inline def gc(run: (() => C.Type) => Array[C.Type]): String =
      var x = '0'
      val f = () => { x = (x+1).toChar; C(x) }
      run(f).cs
    inline def ic(run: (Int => C.Type) => Array[C.Type]): String =
      val f = (i: Int) => C(('0' + i).toChar)
      run(f).cs
    inline def fc(run: ((C.Type, Int) => C.Type) => Array[C.Type]): String =
      val f = (c: C.Type, i: Int) => C((c.value + i).toChar)
      run(f).cs
    T ~ gc{ f => car.dup(_.clip.set(3, 5  )(f)) } ==== "ch.12k."
    T ~ ic{ f => car.dup(_.clip.set(3, 5  )(f)) } ==== "ch.34k."
    T ~ fc{ f => car.dup(_.clip.set(3, 5  )(f)) } ==== "ch.&mk."
    T ~ gc{ f => car.dup(_.clip.set(civ   )(f)) } ==== "ch.12k."
    T ~ ic{ f => car.dup(_.clip.set(civ   )(f)) } ==== "ch.34k."
    T ~ fc{ f => car.dup(_.clip.set(civ   )(f)) } ==== "ch.&mk."
    T ~ gc{ f => car.dup(_.clip.set(3 to 4)(f)) } ==== "ch.12k."
    T ~ ic{ f => car.dup(_.clip.set(3 to 4)(f)) } ==== "ch.34k."
    T ~ fc{ f => car.dup(_.clip.set(3 to 4)(f)) } ==== "ch.&mk."
    T ~ gc{ f => car.dup(_.clip.set(cpv   )(f)) } ==== "ch.12k."
    T ~ ic{ f => car.dup(_.clip.set(cpv   )(f)) } ==== "ch.34k."
    T ~ fc{ f => car.dup(_.clip.set(cpv   )(f)) } ==== "ch.&mk."
    T ~ gc{ f => car.dup(_.clip.set(ix    )(f)) } ==== "c415ik."
    T ~ ic{ f => car.dup(_.clip.set(ix    )(f)) } ==== "c123ik."    
    T ~ fc{ f => car.dup(_.clip.set(ix    )(f)) } ==== "cj0)ik."    
    T ~ gc{ f => car.dup(_.clip.set(st    )(f)) } ==== "c415ik."
    T ~ ic{ f => car.dup(_.clip.set(st    )(f)) } ==== "c123ik."
    T ~ fc{ f => car.dup(_.clip.set(st    )(f)) } ==== "cj0)ik."

    T ~ gc{ f => car.dup(_.clip.set(3, 9  )(f)) } ==== "ch.1234"
    T ~ ic{ f => car.dup(_.clip.set(3, 9  )(f)) } ==== "ch.3456"
    T ~ fc{ f => car.dup(_.clip.set(3, 9  )(f)) } ==== "ch.&mp4"
    T ~ gc{ f => car.dup(_.clip.set(eiv   )(f)) } ==== "ch.1234"
    T ~ ic{ f => car.dup(_.clip.set(eiv   )(f)) } ==== "ch.3456"
    T ~ fc{ f => car.dup(_.clip.set(eiv   )(f)) } ==== "ch.&mp4"
    T ~ gc{ f => car.dup(_.clip.set(3 to 8)(f)) } ==== "ch.1234"
    T ~ ic{ f => car.dup(_.clip.set(3 to 8)(f)) } ==== "ch.3456"
    T ~ fc{ f => car.dup(_.clip.set(3 to 8)(f)) } ==== "ch.&mp4"
    T ~ gc{ f => car.dup(_.     set(3, 9  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(3, 9  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(3, 9  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(eiv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(eiv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(eiv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(3 to 8)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(3 to 8)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(3 to 8)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ gc{ f => car.dup(_.clip.set(-2, 5  )(f)) } ==== "12345k."
    T ~ ic{ f => car.dup(_.clip.set(-2, 5  )(f)) } ==== "01234k."
    T ~ fc{ f => car.dup(_.clip.set(-2, 5  )(f)) } ==== "ci0&mk."
    T ~ gc{ f => car.dup(_.clip.set(-2 to 4)(f)) } ==== "12345k."
    T ~ ic{ f => car.dup(_.clip.set(-2 to 4)(f)) } ==== "01234k."
    T ~ fc{ f => car.dup(_.clip.set(-2 to 4)(f)) } ==== "ci0&mk."
    T ~ gc{ f => car.dup(_.clip.set(fiv    )(f)) } ==== "12345k."
    T ~ ic{ f => car.dup(_.clip.set(fiv    )(f)) } ==== "01234k."
    T ~ fc{ f => car.dup(_.clip.set(fiv    )(f)) } ==== "ci0&mk."
    T ~ gc{ f => car.dup(_.clip.set(fpv    )(f)) } ==== "12345k."
    T ~ ic{ f => car.dup(_.clip.set(fpv    )(f)) } ==== "01234k."
    T ~ fc{ f => car.dup(_.clip.set(fpv    )(f)) } ==== "ci0&mk."
    T ~ gc{ f => car.dup(_.     set(-2, 5  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(-2, 5  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(-2, 5  )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(-2 to 4)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(-2 to 4)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(-2 to 4)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(fiv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(fiv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(fiv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(fpv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(fpv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(fpv    )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ gc{ f => car.dup(_.clip.set(-2, 9  )(f)) } ==== "1234567"
    T ~ ic{ f => car.dup(_.clip.set(-2, 9  )(f)) } ==== "0123456"
    T ~ fc{ f => car.dup(_.clip.set(-2, 9  )(f)) } ==== "ci0&mp4"
    T ~ gc{ f => car.dup(_.clip.set(-2 to 8)(f)) } ==== "1234567"
    T ~ ic{ f => car.dup(_.clip.set(-2 to 8)(f)) } ==== "0123456"
    T ~ fc{ f => car.dup(_.clip.set(-2 to 8)(f)) } ==== "ci0&mp4"
    T ~ gc{ f => car.dup(_.clip.set(biv    )(f)) } ==== "1234567"
    T ~ ic{ f => car.dup(_.clip.set(biv    )(f)) } ==== "0123456"
    T ~ fc{ f => car.dup(_.clip.set(biv    )(f)) } ==== "ci0&mp4"

    T ~ gc{ f => car.dup(_.clip.set(8, 10 )(f)) } ==== "ch.#ik."
    T ~ ic{ f => car.dup(_.clip.set(8, 10 )(f)) } ==== "ch.#ik."
    T ~ fc{ f => car.dup(_.clip.set(8, 10 )(f)) } ==== "ch.#ik."
    T ~ gc{ f => car.dup(_.clip.set(8 to 9)(f)) } ==== "ch.#ik."
    T ~ ic{ f => car.dup(_.clip.set(8 to 9)(f)) } ==== "ch.#ik."
    T ~ fc{ f => car.dup(_.clip.set(8 to 9)(f)) } ==== "ch.#ik."
    T ~ gc{ f => car.dup(_.clip.set(niv   )(f)) } ==== "ch.#ik."
    T ~ ic{ f => car.dup(_.clip.set(niv   )(f)) } ==== "ch.#ik."
    T ~ fc{ f => car.dup(_.clip.set(niv   )(f)) } ==== "ch.#ik."
    T ~ gc{ f => car.dup(_.clip.set(npv   )(f)) } ==== "ch.#ik."
    T ~ ic{ f => car.dup(_.clip.set(npv   )(f)) } ==== "ch.#ik."
    T ~ fc{ f => car.dup(_.clip.set(npv   )(f)) } ==== "ch.#ik."
    T ~ gc{ f => car.dup(_.     set(8, 10 )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(8, 10 )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(8, 10 )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(8 to 9)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(8 to 9)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(8 to 9)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(niv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(niv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(niv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(npv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(npv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(npv   )(f)) } ==== thrown[ArrayIndexOutOfBoundsException]

    T ~ gc{ f => car.dup(_.clip.set(ex)(f)) } ==== "2h13ik4"
    T ~ ic{ f => car.dup(_.clip.set(ex)(f)) } ==== "0h23ik6"
    T ~ fc{ f => car.dup(_.clip.set(ex)(f)) } ==== "ch0&ik4"
    T ~ gc{ f => car.dup(_.clip.set(et)(f)) } ==== "2h13ik4"
    T ~ ic{ f => car.dup(_.clip.set(et)(f)) } ==== "0h23ik6"
    T ~ fc{ f => car.dup(_.clip.set(et)(f)) } ==== "ch0&ik4"
    T ~ gc{ f => car.dup(_.     set(ex)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(ex)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(ex)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ gc{ f => car.dup(_.     set(et)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ ic{ f => car.dup(_.     set(et)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]
    T ~ fc{ f => car.dup(_.     set(et)(f)) } ==== thrown[ArrayIndexOutOfBoundsException]

    var ninja = 0
    T ~ ca9.dup(a => ninja += car.clip.inject(a)).cs            ==== "ch.#ik.HI"
    T ~ ca9.dup(a => ninja += car.clip.inject(a, 2)).cs         ==== "ABch.#ik."
    T ~ { val x = ninja; ninja = 0; x }                         ==== 2*car.length
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(3, 5)).cs      ==== "#i34567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(3, 5)).cs   ==== "12#i567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(3 to 4)).cs    ==== "#i34567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(3 to 4)).cs ==== "12#i567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(civ)).cs       ==== "#i34567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(civ)).cs    ==== "12#i567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(cpv)).cs       ==== "#i34567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(cpv)).cs    ==== "12#i567"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 8*2
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(ix)).cs        ==== ".#hh#67"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(ix)).cs     ==== "12.#hh#"
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(st)).cs        ==== ".#hh#67"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(st)).cs     ==== "12.#hh#"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 4*5

    T ~ ca3.dup(a => ninja += car.clip.inject(a)).cs            ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)).cs         ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)).cs         ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 3+5+0

    T ~ ca1.dup(a => ninja += car.clip.inject(a)(3, 5)).cs      ==== "#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 2)(3, 5)).cs   ==== "89#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(3, 5)).cs   ==== "890"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(3 to 4)).cs    ==== "#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 2)(3 to 4)).cs ==== "89#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(3 to 4)).cs ==== "890"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(civ)).cs       ==== "#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 2)(civ)).cs    ==== "89#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(civ)).cs    ==== "890"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(cpv)).cs       ==== "#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 2)(cpv)).cs    ==== "89#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(cpv)).cs    ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 4*(1+1+0)

    T ~ ca3.dup(a => ninja += car.clip.inject(a)(3, 9)).cs      ==== "#ik"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 5)(3, 9)).cs   ==== "12345#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(3, 9)).cs   ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(3 to 8)).cs    ==== "#ik"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 5)(3 to 8)).cs ==== "12345#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(3 to 8)).cs ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(eiv)).cs       ==== "#ik"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 5)(eiv)).cs    ==== "12345#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(eiv)).cs    ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 3*(3+2+0)

    T ~ ca3.dup(a => ninja += car.clip.inject(a)(-2, 5)).cs      ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 3)(-2, 5)).cs   ==== "123ch.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(-2, 5)).cs   ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(-2 to 4)).cs    ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 3)(-2 to 4)).cs ==== "123ch.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(-2 to 4)).cs ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(fiv)).cs        ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 3)(fiv)).cs     ==== "123ch.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(fiv)).cs     ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(fpv)).cs        ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 3)(fpv)).cs     ==== "123ch.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(fpv)).cs     ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 4*(3+4+0)

    T ~ ca3.dup(a => ninja += car.clip.inject(a)(-2, 9)).cs      ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(-2, 9)).cs   ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(-2, 9)).cs   ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(-2 to 8)).cs    ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(-2 to 8)).cs ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(-2 to 8)).cs ==== "890"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(biv)).cs        ==== "ch."
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(biv)).cs     ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(biv)).cs     ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 3*(3+5+0)

    T ~ ca1.dup(a => ninja += car.clip.inject(a)(8, 10)).cs     ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a, 2)(8, 10)).cs  ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(8 to 9)).cs    ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a, 2)(8 to 9)).cs ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(niv)).cs       ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a, 2)(niv)).cs    ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a)(npv)).cs       ==== "%"
    T ~ ca1.dup(a => ninja += car.clip.inject(a, 2)(npv)).cs    ==== "%"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 0

    T ~ ca3.dup(a => ninja += car.clip.inject(a)(ix)).cs        ==== ".#h"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 1)(ix)).cs     ==== "8.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(ix)).cs     ==== "890"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, -1)(ix)).cs    ==== ".#hh#67"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(st)).cs        ==== ".#h"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 1)(st)).cs     ==== "8.#"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 4)(st)).cs     ==== "890"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, -1)(st)).cs    ==== ".#hh#67"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 2*(3+2+0+5)    
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(ex)).cs        ==== ".c#.567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 4)(ex)).cs     ==== "1234.c#"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 9)(ex)).cs     ==== "1234567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, -1)(ex)).cs    ==== ".c#.567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a)(et)).cs        ==== ".c#.567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 4)(et)).cs     ==== "1234.c#"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 9)(et)).cs     ==== "1234567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, -1)(et)).cs    ==== ".c#.567"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 2*(4+3+0+4)

    T ~ ca7.dup(a => ninja += car.clip.inject(a)(_.l)).cs       ==== "chik567"
    T ~ ca7.dup(a => ninja += car.clip.inject(a, 2)(_.l)).cs    ==== "12chik7"
    T ~ ca3.dup(a => ninja += car.clip.inject(a)(_.l)).cs       ==== "chi"
    T ~ ca3.dup(a => ninja += car.clip.inject(a, 2)(_.l)).cs    ==== "89c"
    T ~ { val x = ninja; ninja = 0; x }                         ==== 2*4 + 3 + 1

    T ~ car.clip.select(3, 5).cs   ==== "#i"
    T ~ car.clip.select(3 to 4).cs ==== "#i"
    T ~ car.clip.select(civ).cs    ==== "#i"
    T ~ car.clip.select(cpv).cs    ==== "#i"
    T ~ car.clip.select(ix).cs     ==== ".#hh#"
    T ~ car.clip.select(st).cs     ==== ".#hh#"

    T ~ car.clip.select(3, 9).cs   ==== "#ik."
    T ~ car.clip.select(3 to 8).cs ==== "#ik."
    T ~ car.clip.select(eiv).cs    ==== "#ik."

    T ~ car.clip.select(-2, 5).cs   ==== "ch.#i"
    T ~ car.clip.select(-2 to 4).cs ==== "ch.#i"
    T ~ car.clip.select(fiv).cs     ==== "ch.#i"
    T ~ car.clip.select(fpv).cs     ==== "ch.#i"

    T ~ car.clip.select(-2, 9).cs   ==== "ch.#ik."
    T ~ car.clip.select(-2 to 8).cs ==== "ch.#ik."
    T ~ car.clip.select(biv).cs     ==== "ch.#ik."

    T ~ car.clip.select(8, 10).cs  ==== ""
    T ~ car.clip.select(8 to 9).cs ==== ""
    T ~ car.clip.select(niv).cs    ==== ""
    T ~ car.clip.select(npv).cs    ==== ""

    T ~ car.clip.select(et).cs    ==== ".c#."
    T ~ car.clip.select(et).cs    ==== ".c#."

    T ~ car.clip.selectOp(3, 5  )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.selectOp(3 to 4)((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.selectOp(civ   )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.selectOp(cpv   )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.selectOp(ix    )((c, i) => c.value + i) =**= "0&ii&".map(_.toInt)
    T ~ car.clip.selectOp(st    )((c, i) => c.value + i) =**= "0&ii&".map(_.toInt)
    T ~ car.clip.selectOp(3, 5  )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.selectOp(3 to 4)((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.selectOp(civ   )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.selectOp(cpv   )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.selectOp(ix    )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.selectOp(st    )((c, i) => c.value + i) ==== typed[Array[Int]]

    T ~ car.clip.selectOp(3, 9  )((c, i) => c.value + i) =**= "&mp4".map(_.toInt)
    T ~ car.clip.selectOp(3 to 8)((c, i) => c.value + i) =**= "&mp4".map(_.toInt)
    T ~ car.clip.selectOp(eiv   )((c, i) => c.value + i) =**= "&mp4".map(_.toInt)

    T ~ car.clip.selectOp(-2, 5  )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.selectOp(-2 to 4)((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.selectOp(fiv    )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.selectOp(fpv    )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)

    T ~ car.clip.selectOp(-2, 9  )((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)
    T ~ car.clip.selectOp(-2 to 8)((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)
    T ~ car.clip.selectOp(biv    )((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)

    T ~ car.clip.selectOp(8, 10 )((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.selectOp(8 to 9)((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.selectOp(niv   )((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.selectOp(npv   )((c, i) => c.value + i) =**= "".map(_.toInt)

    T ~ car.clip.selectOp(ex)((c, i) => c.value + i) =**= "0c&4".map(_.toInt)
    T ~ car.clip.selectOp(et)((c, i) => c.value + i) =**= "0c&4".map(_.toInt)


  @Test
  def arrayBreakIntervalDataTest: Unit =
    import shortcut.{ quittable => qt, quitIf => qIf, skipIf => sIf }

    var cuml = 0
    val str = "ch.#ik."
    val car = str.c

    val ix = Array(2, 3, 1, 1, 3)
    def st = ix.stepper
    val civ = Iv(3, 5)
    val cpv = 3 to End-2

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f
      cuml

    T ~ z{ car.breakable.peek()(cuml += _.n) }.cs       ==== str
    T ~ cuml                                            ==== str.map(_.toInt).sum
    T ~ z{ car.breakable.peek(3, 5)(cuml += _.n) }.cs   ==== str
    T ~ cuml                                            ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.breakable.peek(3 to 4)(cuml += _.n) }.cs ==== str
    T ~ cuml                                            ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.breakable.peek(civ)(cuml += _.n) }.cs    ==== str
    T ~ cuml                                            ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.breakable.peek(cpv)(cuml += _.n) }.cs    ==== str
    T ~ cuml                                            ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.breakable.peek(ix)(cuml += _.n) }.cs     ==== str
    T ~ cuml                                            ==== ".#hh#".map(_.toInt).sum
    T ~ z{ car.breakable.peek(st)(cuml += _.n) }.cs     ==== str
    T ~ cuml                                            ==== ".#hh#".map(_.toInt).sum

    T ~ z{ car.breakable.peek(){ c => qIf(!c.l); cuml += c.n } }.cs      ==== str
    T ~ cuml                                                             ==== str.take(2).map(_.toInt).sum
    T ~ z{ car.breakable.peek(3, 5){ c => qIf(c.l); cuml += c.n } }.cs   ==== str
    T ~ cuml                                                             ==== str(3).toInt
    T ~ z{ car.breakable.peek(3 to 4){ c => qIf(c.l); cuml += c.n } }.cs ==== str
    T ~ cuml                                                             ==== str(3).toInt
    T ~ z{ car.breakable.peek(civ){ c => qIf(c.l); cuml += c.n } }.cs    ==== str
    T ~ cuml                                                             ==== str(3).toInt
    T ~ z{ car.breakable.peek(cpv){ c => qIf(c.l); cuml += c.n } }.cs    ==== str
    T ~ cuml                                                             ==== str(3).toInt
    T ~ z{ car.breakable.peek(ix){ c => qIf(c.l); cuml += c.n } }.cs     ==== str
    T ~ cuml                                                             ==== ".#".map(_.toInt).sum
    T ~ z{ car.breakable.peek(st){ c => qIf(c.l); cuml += c.n } }.cs     ==== str
    T ~ cuml                                                             ==== ".#".map(_.toInt).sum

    T ~ n{ qt{ car.visit(){ (c, i) => qIf(!c.l); cuml += c.n + i } } }      ==== str.take(2).map(_.toInt).sum + 1
    T ~ n{ qt{ car.visit(3, 5){   (c, i) => qIf(c.l); cuml += c.n + i } } } ==== str(3).toInt + 3
    T ~ n{ qt{ car.visit(civ){    (c, i) => qIf(c.l); cuml += c.n + i } } } ==== str(3).toInt + 3
    T ~ n{ qt{ car.visit(3 to 4){ (c, i) => qIf(c.l); cuml += c.n + i } } } ==== str(3).toInt + 3
    T ~ n{ qt{ car.visit(cpv){    (c, i) => qIf(c.l); cuml += c.n + i } } } ==== str(3).toInt + 3
    T ~ n{ qt{ car.visit(ix){ (c, i) => qIf(c.l); cuml += c.n + i } } }     ==== ".#".map(_.toInt).sum + 5
    T ~ n{ qt{ car.visit(st){ (c, i) => qIf(c.l); cuml += c.n + i } } }     ==== ".#".map(_.toInt).sum + 5

    T ~ car.breakable.gather(0)()(_ + _.n + _)       ==== str.map(_.toInt).sum + str.length*(str.length-1)/2
    T ~ car.breakable.gather(0)(3, 5)(_ + _.n + _)   ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.breakable.gather(0)(civ)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.breakable.gather(0)(3 to 4)(_ + _.n + _) ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.breakable.gather(0)(cpv)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.breakable.gather(0)(ix)(_ + _.n + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ car.breakable.gather(0)(st)(_ + _.n + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum

    T ~ car.breakable.gather(0)(){ (a, c, i) => qIf(!c.l); a + c.n + i }      ==== str.take(2).map(_.toInt).sum + 1
    T ~ car.breakable.gather(0)(3, 5){ (a, c, i) => qIf(c.l); a + c.n + i }   ==== str(3).toInt + 3
    T ~ car.breakable.gather(0)(civ){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== str(3).toInt + 3
    T ~ car.breakable.gather(0)(3 to 4){ (a, c, i) => qIf(c.l); a + c.n + i } ==== str(3).toInt + 3
    T ~ car.breakable.gather(0)(cpv){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== str(3).toInt + 3
    T ~ car.breakable.gather(0)(ix){ (a, c, i) => qIf(c.l); a + c.n + i }     ==== ".#".map(_.toInt).sum + 5
    T ~ car.breakable.gather(0)(st){ (a, c, i) => qIf(c.l); a + c.n + i }     ==== ".#".map(_.toInt).sum + 5

    T ~ car.breakable.dupWith(_.n)                   =**= car.map(_.n)
    T ~ car.breakable.dupWith{ c => qIf(!c.l); c.n } =**= car.take(2).map(_.n)

    T ~ car.breakable.where(_.l)                          =**= car.zipWithIndex.collect{ case (c, i) if c.l => i }
    T ~ car.breakable.where{ c => qIf(c.value>'i'); c.l } =**= car.zipWithIndex.takeWhile(_._1.value <= 'i').collect{ case (c, i) if c.l => i }

    T ~ car.dup{ a => qt{ var x = '0'; a.set(){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs         ==== "1h.#ik."
    T ~ car.dup{ a => qt{ a.set(){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs                   ==== "0h.#ik."
    T ~ car.dup{ a => qt{ a.set(){ (c, i) => qIf(i>3 || c.value=='&'); C((c.value+i).toChar) } } }.cs       ==== "ci0&ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(3, 5){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs     ==== "ch.1ik."
    T ~ car.dup{ a => qt{ a.set(3, 5){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs               ==== "ch.3ik."
    T ~ car.dup{ a => qt{ a.set(3, 5){ (c, i) => qIf(i>3 || c.value=='&'); C((c.value+i).toChar) } } }.cs   ==== "ch.&ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(3 to 4){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs   ==== "ch.1ik."
    T ~ car.dup{ a => qt{ a.set(3 to 4){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs             ==== "ch.3ik."
    T ~ car.dup{ a => qt{ a.set(3 to 4){ (c, i) => qIf(i>3 || c.value=='&'); C((c.value+i).toChar) } } }.cs ==== "ch.&ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(civ){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs      ==== "ch.1ik."
    T ~ car.dup{ a => qt{ a.set(civ){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs                ==== "ch.3ik."
    T ~ car.dup{ a => qt{ a.set(civ){ (c, i) => qIf(i>3 || c.value=='&'); C((c.value+i).toChar) } } }.cs    ==== "ch.&ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(cpv){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs      ==== "ch.1ik."
    T ~ car.dup{ a => qt{ a.set(cpv){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs                ==== "ch.3ik."
    T ~ car.dup{ a => qt{ a.set(cpv){ (c, i) => qIf(i>3 || c.value=='&'); C((c.value+i).toChar) } } }.cs    ==== "ch.&ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(ix){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs       ==== "ch1#ik."
    T ~ car.dup{ a => qt{ a.set(ix){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs                 ==== "ch23ik."
    T ~ car.dup{ a => qt{ a.set(ix){ (c, i) => qIf(i>3 || c.value=='&'); C((c.value+i).toChar) } } }.cs     ==== "cj0&ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(st){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs       ==== "ch1#ik."
    T ~ car.dup{ a => qt{ a.set(st){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs                 ==== "ch23ik."
    T ~ car.dup{ a => qt{ a.set(st){ (c, i) => qIf(i>3 || c.value=='&'); C((c.value+i).toChar) } } }.cs     ==== "cj0&ik."
    T ~ car.dup{ a => qt{ var x = '0'; a.set(_.l){ () => x = (x+1).toChar; qIf(x > '1'); C(x) } } }.cs      ==== "1h.#ik."
    T ~ car.dup{ a => qt{ a.set(_.l){ i => qIf(i == 1 || i == 4); C(('0'+i).toChar) } } }.cs                ==== "0h.#ik."

    val cx = "_________".c
    var ninja = 0
    T ~ cx.dup(a => ninja = car.breakable.inject(a)(_.l)).cs                              ==== "chik_____"
    T ~ { val x = ninja; ninja = 0; x }                                                   ==== car.count(_.l)
    T ~ cx.dup(a => ninja = car.breakable.inject(a, 2)(_.l)).cs                           ==== "__chik___"
    T ~ { val x = ninja; ninja = 0; x }                                                   ==== car.count(_.l)
    T ~ cx.dup(a => ninja = car.breakable.inject(a){ c => qIf(c.value=='#'); c.l }).cs    ==== "ch_______"
    T ~ { val x = ninja; ninja = 0; x }                                                   ==== car.takeWhile(_.value != '#').count(_.l)
    T ~ cx.dup(a => ninja = car.breakable.inject(a, 2){ c => qIf(c.value=='#'); c.l }).cs ==== "__ch_____"
    T ~ { val x = ninja; ninja = 0; x }                                                   ==== car.takeWhile(_.value != '#').count(_.l)

    T ~ car.breakable.select(_.l).cs                              ==== "chik"
    T ~ car.breakable.select{ c => qIf(c.value=='#'); c.l }.cs    ==== "ch"

    T ~ car.breakable.selectOp()(      (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "-h-#-k-"
    T ~ car.breakable.selectOp(3, 5)(  (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "#-"
    T ~ car.breakable.selectOp(3 to 4)((c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "#-"
    T ~ car.breakable.selectOp(civ)(   (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "#-"
    T ~ car.breakable.selectOp(cpv)(   (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "#-"
    T ~ car.breakable.selectOp(ix)(    (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "-#hh#"
    T ~ car.breakable.selectOp(st)(    (c, i) => if i%2 == 0 then '-' else c.value).mkString ==== "-#hh#"
    T ~ car.breakable.selectOp()(      (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.breakable.selectOp(3, 5)(  (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.breakable.selectOp(3 to 4)((c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.breakable.selectOp(civ)(   (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.breakable.selectOp(cpv)(   (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.breakable.selectOp(ix)(    (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]
    T ~ car.breakable.selectOp(st)(    (c, i) => if i%2 == 0 then '-' else c.value) ==== typed[Array[Char]]

    T ~ car.breakable.selectOp(){      (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "h#k"
    T ~ car.breakable.selectOp(3, 5){  (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#"
    T ~ car.breakable.selectOp(3 to 4){(c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#"
    T ~ car.breakable.selectOp(civ){   (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#"
    T ~ car.breakable.selectOp(cpv){   (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#"
    T ~ car.breakable.selectOp(ix){    (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#hh#"
    T ~ car.breakable.selectOp(st){    (c, i) => sIf(i%2 == 0); c.value }.mkString ==== "#hh#"

    T ~ car.breakable.selectOp(){      (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "c"
    T ~ car.breakable.selectOp(3, 5){  (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.breakable.selectOp(3 to 4){(c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.breakable.selectOp(civ){   (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.breakable.selectOp(cpv){   (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.breakable.selectOp(ix){    (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== ".#"
    T ~ car.breakable.selectOp(st){    (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== ".#"

    val lar = "ch.ix.#n.".c
    T ~ lar.breakable.fusion[Int]((c, i, add) => if !c.l then add(i) else Array(c.o).os.foreach(c => add(c.toInt))) =**= Array(99, 46, 104, 46, 2, 105, 46, 120, 46, 5, 6, 110, 46, 8)
    T ~ lar.breakable.fusion[Int]((c, i, add) => if !c.l then { qIf(i>5); add(i) } else Array(c.o).os.foreach(c => add(c.toInt))) =**= Array(99, 46, 104, 46, 2, 105, 46, 120, 46, 5)



  @Test
  def arrayClipBreakIntervalTest(): Unit =
    import shortcut.{ quittable => qt, quitIf => qIf, skipIf => sIf }

    var cuml = 0
    val str = "ch.#ik."
    val car = str.c

    val ix = Array(2, 3, 1, 1, 3)
    val ex = Array(2, 0, -1, 3, 6, 7, -9, 400) 
    def st = ix.stepper
    def et = ex.stepper

    val civ = Iv(3, 5)
    val cpv = 3 to End-2
    val eiv = Iv(3, 9)
    val fiv = Iv(-2, 5)
    val fpv = End-9 to End-2
    val biv = Iv(-2, 9)
    val niv = Iv(8, 9)
    val npv = End-9 to End-8

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f
      cuml

    T ~ car.clip.breakable ==== typed[ShortClipArray[C.Type]]
    T ~ car.breakable.clip ==== typed[ShortClipArray[C.Type]]

    T ~ z{ car.breakable.clip.peek(3, 5)(cuml += _.n) }.cs   ==== str
    T ~ cuml                                                 ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.breakable.clip.peek(3 to 4)(cuml += _.n) }.cs ==== str
    T ~ cuml                                                 ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.breakable.clip.peek(civ)(cuml += _.n) }.cs    ==== str
    T ~ cuml                                                 ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.breakable.clip.peek(cpv)(cuml += _.n) }.cs    ==== str
    T ~ cuml                                                 ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ car.breakable.clip.peek(ix)(cuml += _.n) }.cs     ==== str
    T ~ cuml                                                 ==== ".#hh#".map(_.toInt).sum
    T ~ z{ car.breakable.clip.peek(st)(cuml += _.n) }.cs     ==== str
    T ~ cuml                                                 ==== ".#hh#".map(_.toInt).sum

    T ~ z{ car.breakable.clip.peek(3, 5){ c => qIf(c.l); cuml += c.n } }.cs   ==== str
    T ~ cuml                                                                  ==== str(3).toInt
    T ~ z{ car.breakable.clip.peek(3 to 4){ c => qIf(c.l); cuml += c.n } }.cs ==== str
    T ~ cuml                                                                  ==== str(3).toInt
    T ~ z{ car.breakable.clip.peek(civ){ c => qIf(c.l); cuml += c.n } }.cs    ==== str
    T ~ cuml                                                                  ==== str(3).toInt
    T ~ z{ car.breakable.clip.peek(cpv){ c => qIf(c.l); cuml += c.n } }.cs    ==== str
    T ~ cuml                                                                  ==== str(3).toInt
    T ~ z{ car.breakable.clip.peek(ix){ c => qIf(c.l); cuml += c.n } }.cs     ==== str
    T ~ cuml                                                                  ==== ".#".map(_.toInt).sum
    T ~ z{ car.breakable.clip.peek(st){ c => qIf(c.l); cuml += c.n } }.cs     ==== str
    T ~ cuml                                                                  ==== ".#".map(_.toInt).sum

    T ~ n{ car.breakable.clip.peek(3, 9)(cuml += _.n) }    ==== str.substring(3).map(_.toInt).sum
    T ~ n{ car.breakable.clip.peek(3 to 8)(cuml += _.n) }  ==== str.substring(3).map(_.toInt).sum
    T ~ n{ car.breakable.clip.peek(eiv)(cuml += _.n) }     ==== str.substring(3).map(_.toInt).sum

    T ~ n{ car.breakable.clip.peek(3, 9){ c => qIf(c.l); cuml += c.n } }    ==== str(3).toInt
    T ~ n{ car.breakable.clip.peek(3 to 8){ c => qIf(c.l); cuml += c.n } }  ==== str(3).toInt
    T ~ n{ car.breakable.clip.peek(eiv){ c => qIf(c.l); cuml += c.n } }     ==== str(3).toInt

    T ~ n{ car.breakable.clip.peek(-2, 5)(cuml += _.n) }   ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.breakable.clip.peek(-2 to 4)(cuml += _.n) } ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.breakable.clip.peek(fiv)(cuml += _.n) }     ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ car.breakable.clip.peek(fpv)(cuml += _.n) }     ==== str.substring(0, 5).map(_.toInt).sum

    T ~ n{ car.breakable.clip.peek(-2, 5){ c => qIf(!c.l); cuml += c.n } }   ==== car.takeWhile(_.l).map(_.value.toInt).sum
    T ~ n{ car.breakable.clip.peek(-2 to 4){ c => qIf(!c.l); cuml += c.n } } ==== car.takeWhile(_.l).map(_.value.toInt).sum
    T ~ n{ car.breakable.clip.peek(fiv){ c => qIf(!c.l); cuml += c.n } }     ==== car.takeWhile(_.l).map(_.value.toInt).sum
    T ~ n{ car.breakable.clip.peek(fpv){ c => qIf(!c.l); cuml += c.n } }     ==== car.takeWhile(_.l).map(_.value.toInt).sum

    T ~ n{ car.breakable.clip.peek(-2, 9)(cuml += _.n) }   ==== str.map(_.toInt).sum
    T ~ n{ car.breakable.clip.peek(-2 to 9)(cuml += _.n) } ==== str.map(_.toInt).sum
    T ~ n{ car.breakable.clip.peek(biv)(cuml += _.n) }     ==== str.map(_.toInt).sum

    T ~ n{ car.breakable.clip.peek(-2, 9){ c => qIf(!c.l); cuml += c.n } }   ==== car.takeWhile(_.l).map(_.value.toInt).sum
    T ~ n{ car.breakable.clip.peek(-2 to 9){ c => qIf(!c.l); cuml += c.n } } ==== car.takeWhile(_.l).map(_.value.toInt).sum
    T ~ n{ car.breakable.clip.peek(biv){ c => qIf(!c.l); cuml += c.n } }     ==== car.takeWhile(_.l).map(_.value.toInt).sum

    T ~ n{ car.breakable.clip.peek(8, 9)(cuml += _.n) }   ==== 0
    T ~ n{ car.breakable.clip.peek(8 to 9)(cuml += _.n) } ==== 0
    T ~ n{ car.breakable.clip.peek(niv)(cuml += _.n) }    ==== 0
    T ~ n{ car.breakable.clip.peek(npv)(cuml += _.n) }    ==== 0

    T ~ n{ car.breakable.clip.peek(8, 9){ c => qIf(c.l); cuml += c.n } }    ==== 0
    T ~ n{ car.breakable.clip.peek(8 to 9){ c => qIf(c.l); cuml += c.n } }  ==== 0
    T ~ n{ car.breakable.clip.peek(niv){ c => qIf(c.l); cuml += c.n } }     ==== 0
    T ~ n{ car.breakable.clip.peek(npv){ c => qIf(c.l); cuml += c.n } }     ==== 0

    T ~ n{ car.breakable.clip.peek(ex)(cuml += _.n) } ==== ".c#.".map(_.toInt).sum
    T ~ n{ car.breakable.clip.peek(et)(cuml += _.n) } ==== ".c#.".map(_.toInt).sum

    T ~ n{ car.breakable.clip.peek(ex){ c => qIf(c.value == '#'); cuml += c.n } } ==== ".c".map(_.toInt).sum
    T ~ n{ car.breakable.clip.peek(et){ c => qIf(c.value == '#'); cuml += c.n } } ==== ".c".map(_.toInt).sum

    def sm(i: Int, j: Int) = j*(j+1)/2 - i*(i-1)/2
    T ~ car.clip.breakable.gather(0)(3, 5)(_ + _.n + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.breakable.gather(0)(3 to 4)(_ + _.n + _)  ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.breakable.gather(0)(civ)(_ + _.n + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.breakable.gather(0)(cpv)(_ + _.n + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ car.clip.breakable.gather(0)(ix)(_ + _.n + _)      ==== ".#hh#".map(_.toInt).sum + 10
    T ~ car.clip.breakable.gather(0)(st)(_ + _.n + _)      ==== ".#hh#".map(_.toInt).sum + 10

    T ~ car.clip.breakable.gather(0)(3, 5){ (a, c, i) => qIf(c.l); a + c.n + i }   ==== str(3).toInt + 3
    T ~ car.clip.breakable.gather(0)(civ){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== str(3).toInt + 3
    T ~ car.clip.breakable.gather(0)(3 to 4){ (a, c, i) => qIf(c.l); a + c.n + i } ==== str(3).toInt + 3
    T ~ car.clip.breakable.gather(0)(cpv){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== str(3).toInt + 3
    T ~ car.clip.breakable.gather(0)(ix){ (a, c, i) => qIf(c.l); a + c.n + i }     ==== ".#".map(_.toInt).sum + 5
    T ~ car.clip.breakable.gather(0)(st){ (a, c, i) => qIf(c.l); a + c.n + i }     ==== ".#".map(_.toInt).sum + 5

    T ~ car.clip.breakable.gather(0)(3, 9)(_ + _.n + _)   ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ car.clip.breakable.gather(0)(3 to 8)(_ + _.n + _) ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ car.clip.breakable.gather(0)(eiv)(_ + _.n + _)    ==== str.substring(3).map(_.toInt).sum + sm(3, 6)

    T ~ car.clip.breakable.gather(0)(3, 9){ (a, c, i) => qIf(c.l); a + c.n + i }   ==== str(3).toInt + 3
    T ~ car.clip.breakable.gather(0)(3 to 8){ (a, c, i) => qIf(c.l); a + c.n + i } ==== str(3).toInt + 3
    T ~ car.clip.breakable.gather(0)(eiv){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== str(3).toInt + 3

    T ~ car.clip.breakable.gather(0)(-2, 5)(_ + _.n + _)   ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.breakable.gather(0)(-2 to 4)(_ + _.n + _) ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.breakable.gather(0)(fiv)(_ + _.n + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ car.clip.breakable.gather(0)(fpv)(_ + _.n + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)

    T ~ car.clip.breakable.gather(0)(-2, 5){ (a, c, i) => qIf(!c.l); a + c.n + i }   ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)
    T ~ car.clip.breakable.gather(0)(-2 to 4){ (a, c, i) => qIf(!c.l); a + c.n + i } ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)
    T ~ car.clip.breakable.gather(0)(fiv){ (a, c, i) => qIf(!c.l); a + c.n + i }     ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)
    T ~ car.clip.breakable.gather(0)(fpv){ (a, c, i) => qIf(!c.l); a + c.n + i }     ==== str.substring(0, 2).map(_.toInt).sum + sm(0, 1)

    T ~ car.clip.breakable.gather(0)(-2, 9)(_ + _.n + _)   ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ car.clip.breakable.gather(0)(-2 to 9)(_ + _.n + _) ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ car.clip.breakable.gather(0)(biv)(_ + _.n + _)     ==== str.map(_.toInt).sum + sm(0, 6)

    T ~ car.clip.breakable.gather(0)(-2, 9){ (a, c, i) => qIf(!c.l); a + c.n + i }   ==== str.take(2).map(_.toInt).sum + sm(0, 1)
    T ~ car.clip.breakable.gather(0)(-2 to 9){ (a, c, i) => qIf(!c.l); a + c.n + i } ==== str.take(2).map(_.toInt).sum + sm(0, 1)
    T ~ car.clip.breakable.gather(0)(biv){ (a, c, i) => qIf(!c.l); a + c.n + i }     ==== str.take(2).map(_.toInt).sum + sm(0, 1)

    T ~ car.clip.breakable.gather(0)(8, 9)(_ + _.n + _)   ==== 0
    T ~ car.clip.breakable.gather(0)(8 to 9)(_ + _.n + _) ==== 0
    T ~ car.clip.breakable.gather(0)(niv)(_ + _.n + _)    ==== 0
    T ~ car.clip.breakable.gather(0)(npv)(_ + _.n + _)    ==== 0

    T ~ car.clip.breakable.gather(0)(8, 9){ (a, c, i) => qIf(c.l); a + c.n + i }   ==== 0
    T ~ car.clip.breakable.gather(0)(8 to 9){ (a, c, i) => qIf(c.l); a + c.n + i } ==== 0
    T ~ car.clip.breakable.gather(0)(niv){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== 0
    T ~ car.clip.breakable.gather(0)(npv){ (a, c, i) => qIf(c.l); a + c.n + i }    ==== 0

    T ~ car.clip.breakable.gather(0)(ex)(_ + _.n + _) ==== ".c#.".map(_.toInt).sum + 11
    T ~ car.clip.breakable.gather(0)(et)(_ + _.n + _) ==== ".c#.".map(_.toInt).sum + 11

    T ~ car.clip.breakable.gather(0)(ex){ (a, c, i) => qIf(c.value == '#'); a + c.n + i } ==== ".c".map(_.toInt).sum + 2
    T ~ car.clip.breakable.gather(0)(et){ (a, c, i) => qIf(c.value == '#'); a + c.n + i } ==== ".c".map(_.toInt).sum + 2

    val ca7 = "1234567".c
    val ca3 = "890".c
    var ninja = 0
    T ~ ca7.dup(a => ninja += car.breakable.clip.inject(a)(_.l)).cs    ==== "chik567"
    T ~ ca7.dup(a => ninja += car.breakable.clip.inject(a, 2)(_.l)).cs ==== "12chik7"
    T ~ ca3.dup(a => ninja += car.breakable.clip.inject(a)(_.l)).cs    ==== "chi"
    T ~ ca3.dup(a => ninja += car.breakable.clip.inject(a, 2)(_.l)).cs ==== "89c"
    T ~ { val x = ninja; ninja = 0; x }                                ==== 4 + 4 + 3 + 1
    T ~ ca7.dup(a => ninja += car.breakable.clip.inject(a){ c => qIf(c.value == 'i'); c.l }).cs    ==== "ch34567"
    T ~ ca7.dup(a => ninja += car.breakable.clip.inject(a, 2){ c => qIf(c.value == 'i'); c.l }).cs ==== "12ch567"
    T ~ ca7.dup(a => ninja += car.breakable.clip.inject(a, 6){ c => qIf(c.value == 'i'); c.l }).cs ==== "123456c"
    T ~ ca3.dup(a => ninja += car.breakable.clip.inject(a){ c => qIf(c.value == 'i'); c.l }).cs    ==== "ch0"
    T ~ ca3.dup(a => ninja += car.breakable.clip.inject(a, 2){ c => qIf(c.value == 'i'); c.l }).cs ==== "89c"
    T ~ { val x = ninja; ninja = 0; x }                                                            ==== 2+2+1+2+1

    T ~ car.clip.breakable.selectOp(3, 5  )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(3 to 4)((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(civ   )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(cpv   )((c, i) => c.value + i) =**= "&m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(ix    )((c, i) => c.value + i) =**= "0&ii&".map(_.toInt)
    T ~ car.clip.breakable.selectOp(st    )((c, i) => c.value + i) =**= "0&ii&".map(_.toInt)
    T ~ car.clip.breakable.selectOp(3, 5  )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.breakable.selectOp(3 to 4)((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.breakable.selectOp(civ   )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.breakable.selectOp(cpv   )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.breakable.selectOp(ix    )((c, i) => c.value + i) ==== typed[Array[Int]]
    T ~ car.clip.breakable.selectOp(st    )((c, i) => c.value + i) ==== typed[Array[Int]]

    T ~ car.clip.breakable.selectOp(3, 9  )((c, i) => c.value + i) =**= "&mp4".map(_.toInt)
    T ~ car.clip.breakable.selectOp(3 to 8)((c, i) => c.value + i) =**= "&mp4".map(_.toInt)
    T ~ car.clip.breakable.selectOp(eiv   )((c, i) => c.value + i) =**= "&mp4".map(_.toInt)

    T ~ car.clip.breakable.selectOp(-2, 5  )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(-2 to 4)((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(fiv    )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(fpv    )((c, i) => c.value + i) =**= "ci0&m".map(_.toInt)

    T ~ car.clip.breakable.selectOp(-2, 9  )((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)
    T ~ car.clip.breakable.selectOp(-2 to 8)((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)
    T ~ car.clip.breakable.selectOp(biv    )((c, i) => c.value + i) =**= "ci0&mp4".map(_.toInt)

    T ~ car.clip.breakable.selectOp(8, 10 )((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.breakable.selectOp(8 to 9)((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.breakable.selectOp(niv   )((c, i) => c.value + i) =**= "".map(_.toInt)
    T ~ car.clip.breakable.selectOp(npv   )((c, i) => c.value + i) =**= "".map(_.toInt)

    T ~ car.clip.breakable.selectOp(ex)((c, i) => c.value + i) =**= "0c&4".map(_.toInt)
    T ~ car.clip.breakable.selectOp(et)((c, i) => c.value + i) =**= "0c&4".map(_.toInt)

    T ~ car.clip.breakable.selectOp(3, 5){  (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.clip.breakable.selectOp(3 to 4){(c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.clip.breakable.selectOp(civ){   (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.clip.breakable.selectOp(cpv){   (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== "#"
    T ~ car.clip.breakable.selectOp(ix){    (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== ".#"
    T ~ car.clip.breakable.selectOp(st){    (c, i) => qIf(i == 1 || i == 4); c.value }.mkString ==== ".#"

    T ~ car.clip.breakable.selectOp(3, 9  ){ (c, i) => qIf(c.value == 'k'); c.value + i } =**= "&m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(3 to 8){ (c, i) => qIf(c.value == 'k'); c.value + i } =**= "&m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(eiv   ){ (c, i) => qIf(c.value == 'k'); c.value + i } =**= "&m".map(_.toInt)

    T ~ car.clip.breakable.selectOp(3, 9  ){ (c, i) => sIf(c.value == 'k'); c.value + i } =**= "&m4".map(_.toInt)
    T ~ car.clip.breakable.selectOp(3 to 8){ (c, i) => sIf(c.value == 'k'); c.value + i } =**= "&m4".map(_.toInt)
    T ~ car.clip.breakable.selectOp(eiv   ){ (c, i) => sIf(c.value == 'k'); c.value + i } =**= "&m4".map(_.toInt)

    T ~ car.clip.breakable.selectOp(-2, 5  ){ (c, i) => qIf(c.value == '#'); c.value + i } =**= "ci0".map(_.toInt)
    T ~ car.clip.breakable.selectOp(-2 to 4){ (c, i) => qIf(c.value == '#'); c.value + i } =**= "ci0".map(_.toInt)
    T ~ car.clip.breakable.selectOp(fiv    ){ (c, i) => qIf(c.value == '#'); c.value + i } =**= "ci0".map(_.toInt)
    T ~ car.clip.breakable.selectOp(fpv    ){ (c, i) => qIf(c.value == '#'); c.value + i } =**= "ci0".map(_.toInt)

    T ~ car.clip.breakable.selectOp(-2, 5  ){ (c, i) => sIf(c.value == '#'); c.value + i } =**= "ci0m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(-2 to 4){ (c, i) => sIf(c.value == '#'); c.value + i } =**= "ci0m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(fiv    ){ (c, i) => sIf(c.value == '#'); c.value + i } =**= "ci0m".map(_.toInt)
    T ~ car.clip.breakable.selectOp(fpv    ){ (c, i) => sIf(c.value == '#'); c.value + i } =**= "ci0m".map(_.toInt)

    T ~ car.clip.breakable.selectOp(-2, 9  ){ (c, i) => qIf(c.value == 'i'); c.value + i } =**= "ci0&".map(_.toInt)
    T ~ car.clip.breakable.selectOp(-2 to 8){ (c, i) => qIf(c.value == 'i'); c.value + i } =**= "ci0&".map(_.toInt)
    T ~ car.clip.breakable.selectOp(biv    ){ (c, i) => qIf(c.value == 'i'); c.value + i } =**= "ci0&".map(_.toInt)

    T ~ car.clip.breakable.selectOp(-2, 9  ){ (c, i) => sIf(c.value == 'i'); c.value + i } =**= "ci0&p4".map(_.toInt)
    T ~ car.clip.breakable.selectOp(-2 to 8){ (c, i) => sIf(c.value == 'i'); c.value + i } =**= "ci0&p4".map(_.toInt)
    T ~ car.clip.breakable.selectOp(biv    ){ (c, i) => sIf(c.value == 'i'); c.value + i } =**= "ci0&p4".map(_.toInt)

    T ~ car.clip.breakable.selectOp(8, 10 ){ (c, i) => sIf(!c.l); qIf(c.n > 99); c.value + i } =**= "".map(_.toInt)
    T ~ car.clip.breakable.selectOp(8 to 9){ (c, i) => sIf(!c.l); qIf(c.n > 99); c.value + i } =**= "".map(_.toInt)
    T ~ car.clip.breakable.selectOp(niv   ){ (c, i) => sIf(!c.l); qIf(c.n > 99); c.value + i } =**= "".map(_.toInt)
    T ~ car.clip.breakable.selectOp(npv   ){ (c, i) => sIf(!c.l); qIf(c.n > 99); c.value + i } =**= "".map(_.toInt)

    T ~ car.clip.breakable.selectOp(ex){ (c, i) => qIf(i == 3); c.value + i } =**= "0c".map(_.toInt)
    T ~ car.clip.breakable.selectOp(et){ (c, i) => qIf(i == 3); c.value + i } =**= "0c".map(_.toInt)

    T ~ car.clip.breakable.selectOp(ex){ (c, i) => sIf(i == 3); c.value + i } =**= "0c4".map(_.toInt)
    T ~ car.clip.breakable.selectOp(et){ (c, i) => sIf(i == 3); c.value + i } =**= "0c4".map(_.toInt)


  @Test
  def arrayPrimitiveDataTest(): Unit =
    import java.lang.Float.{intBitsToFloat => i2f}
    import java.lang.Double.{longBitsToDouble => l2d}

    T ~ Iv(5, 8)                    ==== 0x800000005L  --: typed[Iv]
    T ~ Iv(5, 8).i0                 ==== 5
    T ~ Iv(5, 8).iN                 ==== 8

    val atf = Array(false, false, true, true)
    val aip = Array(2, 3, -3)
    val air = Array(3, 4, 3)

    object NuZ extends NewType[Boolean] {}
    val az = Array[Boolean](true, true, false)
    val naz = Array[NuZ.Type](NuZ(true), NuZ(true), NuZ(false))
    T ~ az.copy                        =**= az
    T ~ (az.copy eq az)                ==== false
    T ~ naz.copy                       =**= naz
    T ~ az.copyToSize(4)               =**= Array(true, true, false, false)
    T ~ az.copyToSize(2)               =**= Array(true, true)
    T ~ az.shrinkCopy(4)               =**= az
    T ~ az.shrinkCopy(2)               =**= az.copyToSize(2)
    T ~ az.copyOfRange(1, 3)           =**= Array(true, false)
    T ~ {az.fill(false); az}           =**= Array(false, false, false)
    T ~ {az.fillRange(1, 3)(true); az} =**= Array(false, true, true)

    object NuB extends NewType[Byte] {}
    val ab = Array[Byte](1, 2, 3)
    val bb = Array[Byte](2, 0, 3, 2, 3)
    val nab = Array[NuB.Type](NuB(1), NuB(2), NuB(3))
    T ~ ab.copy                     =**= ab
    T ~ (ab.copy eq ab)             ==== false
    T ~ nab.copy                    =**= nab
    T ~ ab.copy.tap(_(0) = 4).toSeq =!!= ab.toSeq
    T ~ ab.copyToSize(2).length     ==== 2
    T ~ ab.copyToSize(2)            =**= ab.take(2)
    T ~ ab.copyToSize(4)            =**= Array[Byte](1, 2, 3, 0)
    T ~ ab.shrinkCopy(2)            =**= ab.take(2)
    T ~ (ab.shrinkCopy(4) eq ab)    ==== true
    T ~ ab.copyOfRange(1, 3)        =**= ab.drop(1)
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
    T ~ bb.fill(4)                  =**= Array[Byte](4, 4, 4, 4, 4)

    object NuS extends NewType[Short] {}
    val as = Array[Short](1, 2, 3)
    val bs = Array[Short](2, 0, 3, 2, 3)
    val nas = Array[NuS.Type](NuS(1), NuS(2), NuS(3))
    T ~ as.copy                     =**= as
    T ~ (as.copy eq as)             ==== false
    T ~ nas.copy                    =**= nas
    T ~ as.copy.tap(_(0) = 4).toSeq =!!= as.toSeq
    T ~ as.copyToSize(2).length     ==== 2
    T ~ as.copyToSize(2)            =**= as.take(2)
    T ~ as.copyToSize(4)            =**= Array[Short](1, 2, 3, 0)
    T ~ as.shrinkCopy(2)            =**= as.take(2)
    T ~ (as.shrinkCopy(4) eq as)    ==== true
    T ~ as.copyOfRange(1, 3)        =**= as.drop(1)
    T ~ bs.isSorted                 ==== false
    T ~ bs.isSortedRange(1, 3)      ==== true
    T ~ as.search(2)                ==== 1
    T ~ as.search(0)                ==== -1
    T ~ bs.searchRange(1, 3)(3)     ==== 2
    T ~ bs.searchRange(1, 3)(2)     ==== -3
    T ~ bs.sortRange(0, 3)          =**= Array[Short](0, 2, 3, 2, 3)
    T ~ bs.sort()                   =**= Array[Short](0, 2, 2, 3, 3)
    T ~ bs.fillRange(2, 4)(1)       =**= Array[Short](0, 2, 1, 1, 3)
    T ~ bs.fill(4)                  =**= Array[Short](4, 4, 4, 4, 4)

    val ac = Array[Char]('1', '2', '3')
    val bc = Array[Char]('2', '0', '3', '2', '3')
    T ~ ac.copy                       =**= ac
    T ~ (ac.copy eq ac)               ==== false
    T ~ ac.copy.tap(_(0) = '4').toSeq =!!= ac.toSeq
    T ~ ac.copyToSize(2).length     ==== 2
    T ~ ac.copyToSize(2)            =**= ac.take(2)
    T ~ ac.copyToSize(4)            =**= Array[Char]('1', '2', '3', '\u0000')
    T ~ ac.shrinkCopy(2)            =**= ac.take(2)
    T ~ (ac.shrinkCopy(4) eq ac)    ==== true
    T ~ ac.copyOfRange(1, 3)        =**= ac.drop(1)
    T ~ bc.isSorted                 ==== false
    T ~ bc.isSortedRange(1, 3)      ==== true
    T ~ ac.search('2')              ==== 1
    T ~ ac.search('0')              ==== -1
    T ~ bc.searchRange(1, 3)('3')   ==== 2
    T ~ bc.searchRange(1, 3)('2')   ==== -3
    T ~ bc.sortRange(0, 3)          =**= Array[Char]('0', '2', '3', '2', '3')
    T ~ bc.sort()                   =**= Array[Char]('0', '2', '2', '3', '3')
    T ~ bc.fillRange(2, 4)('e')     =**= Array[Char]('0', '2', 'e', 'e', '3')
    T ~ bc.fill('4')                =**= Array[Char]('4', '4', '4', '4', '4')

    val ai = Array[Int](1, 2, 3)
    val bi = Array[Int](2, 0, 3, 2, 3)
    T ~ ai.copy                     =**= ai
    T ~ (ai.copy eq ai)             ==== false
    T ~ ai.copy.tap(_(0) = 4).toSeq =!!= ai.toSeq
    T ~ ai.copyToSize(2).length     ==== 2
    T ~ ai.copyToSize(2)            =**= ai.take(2)
    T ~ ai.copyToSize(4)            =**= Array[Int](1, 2, 3, 0)
    T ~ ai.shrinkCopy(2)            =**= ai.take(2)
    T ~ (ai.shrinkCopy(4) eq ai)    ==== true
    T ~ ai.copyOfRange(1, 3)        =**= ai.drop(1)
    T ~ Array(0x05030107).unpackBytes =**= Array[Byte](7, 1, 3, 5)
    T ~ bi.isSorted                 ==== false
    T ~ bi.isSortedRange(1, 3)      ==== true
    T ~ ai.search(2)                ==== 1
    T ~ ai.search(0)                ==== -1
    T ~ bi.searchRange(1, 3)(3)     ==== 2
    T ~ bi.searchRange(1, 3)(2)     ==== -3
    T ~ bi.sortRange(0, 3)          =**= Array[Int](0, 2, 3, 2, 3)
    T ~ ai.copyOfRange(1, 3)        =**= ai.drop(1)
    T ~ bi.sort()                   =**= Array[Int](0, 2, 2, 3, 3)
    T ~ bi.fillRange(2, 4)(1)       =**= Array[Int](0, 2, 1, 1, 3)
    T ~ bi.fill(4)                  =**= Array[Int](4, 4, 4, 4, 4)

    val al = Array[Long](1, 2, 3)
    val bl = Array[Long](2, 0, 3, 2, 3)
    T ~ al.copy                     =**= al
    T ~ (al.copy eq al)             ==== false
    T ~ al.copy.tap(_(0) = 4).toSeq =!!= al.toSeq
    T ~ al.copyToSize(2).length     ==== 2
    T ~ al.copyToSize(2)            =**= al.take(2)
    T ~ al.copyToSize(4)            =**= Array[Long](1, 2, 3, 0)
    T ~ al.shrinkCopy(2)            =**= al.take(2)
    T ~ (al.shrinkCopy(4) eq al)    ==== true
    T ~ al.copyOfRange(1, 3)        =**= al.drop(1)
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
    T ~ bl.fill(4)                  =**= Array[Long](4, 4, 4, 4, 4)

    val af = Array[Float](1, 2, 3)
    val bf = Array[Float](2, 0, 3, 2, 3)
    T ~ af.copy                     =**= af
    T ~ (af.copy eq af)             ==== false
    T ~ af.copy.tap(_(0) = 4).toSeq =!!= af.toSeq
    T ~ af.copyToSize(2).length     ==== 2
    T ~ af.copyToSize(2)            =**= af.take(2)
    T ~ af.copyToSize(4)            =**= Array[Float](1, 2, 3, 0)
    T ~ af.shrinkCopy(2)            =**= af.take(2)
    T ~ (af.shrinkCopy(4) eq af)    ==== true
    T ~ af.copyOfRange(1, 3)        =**= af.drop(1)
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
    T ~ bf.fill(4)                  =**= Array[Float](4, 4, 4, 4, 4)

    val ad = Array[Double](1, 2, 3)
    val bd = Array[Double](2, 0, 3, 2, 3)
    T ~ ad.copy                     =**= ad
    T ~ (ad.copy eq ad)             ==== false
    T ~ ad.copy.tap(_(0) = 4).toSeq =!!= ad.toSeq
    T ~ ad.copyToSize(2).length     ==== 2
    T ~ ad.copyToSize(2)            =**= ad.take(2)
    T ~ ad.copyToSize(4)            =**= Array[Double](1, 2, 3, 0)
    T ~ ad.shrinkCopy(2)            =**= ad.take(2)
    T ~ (ad.shrinkCopy(4) eq ad)    ==== true
    T ~ ad.copyOfRange(1, 3)        =**= ad.drop(1)
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
    T ~ bd.fill(4)                  =**= Array[Double](4, 4, 4, 4, 4)

    object NuA extends NewType[String] {}
    val aa = Array[String]("1", "2", "3")
    val ba = Array[String]("2", "0", "3", "2", "3")
    val naa = Array[NuA.Type](NuA("1"), NuA("2"), NuA("3"))
    T ~ aa.copy                       =**= aa
    T ~ (aa.copy eq aa)               ==== false
    T ~ naa.copy                      =**= naa
    T ~ aa.copy.tap(_(0) = "4").toSeq =!!= aa.toSeq
    T ~ aa.copyToSize(2).length       ==== 2
    T ~ aa.copyToSize(2)              =**= aa.take(2)
    T ~ aa.copyToSize(4)              =**= Array[String]("1", "2", "3", null)
    T ~ aa.shrinkCopy(2)              =**= aa.take(2)
    T ~ (aa.shrinkCopy(4) eq aa)      ==== true
    T ~ aa.copyOfRange(1, 3)          =**= aa.drop(1)
    T ~ ba.isSorted                   ==== false
    T ~ ba.isSortedRange(1, 3)        ==== true
    T ~ aa.search("2")                ==== 1
    T ~ aa.search("0")                ==== -1
    T ~ ba.searchRange(1, 3)("3")     ==== 2
    T ~ ba.searchRange(1, 3)("2")     ==== -3
    T ~ ba.sortRange(0, 3)            =**= Array[String]("0", "2", "3", "2", "3")
    T ~ ba.sort()                     =**= Array[String]("0", "2", "2", "3", "3")
    T ~ ba.fillRange(2, 4)("e")       =**= Array[String]("0", "2", "e", "e", "3")
    T ~ ba.fill("4")                  =**= Array[String]("4", "4", "4", "4", "4")


  @Test
  def stringInlinedDataTest(): Unit =
    var cuml = 0
    val str = "ch.ix.#n."
    val arr = Array('c', 'h', '.', 'i', 'x', '.', '#', 'n', '.')
    val ix = Array(2, 3, 1, 1, 3)

    val civ = Iv(3, 5)
    val cpv = 3 to End-4

    def st = ix.stepper

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f
      cuml

    T ~ str.arr                                   =**= arr
    T ~ arr.str                                   ==== str
    T ~ str.builder().tap(_ append "##").toString ==== str + "##"
    T ~ str.build(_ append "##")                  ==== str + "##"

    T ~ str(1)     ==== 'h'
    T ~ str(End)   ==== '.'
    T ~ str(End-1) ==== 'n'

    T ~ z{ str.peek()(cuml += _) }       ==== str
    T ~ cuml                             ==== str.map(_.toInt).sum  
    T ~ z{ str.peek(3, 5)(cuml += _) }   ==== str
    T ~ cuml                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.peek(civ)(cuml += _) }    ==== str
    T ~ cuml                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.peek(3 to 4)(cuml += _) } ==== str
    T ~ cuml                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.peek(cpv)(cuml += _) }    ==== str
    T ~ cuml                             ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.peek(ix)(cuml += _) }     ==== str
    T ~ cuml                             ==== ".ihhi".map(_.toInt).sum
    T ~ z{ str.peek(st)(cuml += _) }     ==== str
    T ~ cuml                             ==== ".ihhi".map(_.toInt).sum

    T ~ n{ str.visit()(cuml += _ + _) }       ==== str.map(_.toInt).sum + str.length*(str.length-1)/2
    T ~ n{ str.visit(3, 5)(cuml += _ + _) }   ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.visit(civ)(cuml += _ + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.visit(3 to 4)(cuml += _ + _) } ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.visit(cpv)(cuml += _ + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.visit(ix)(cuml += _ + _) }     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ n{ str.visit(st)(cuml += _ + _) }     ==== ix.map(i => str(i).toInt).sum + ix.sum

    T ~ n{ str.wander(){  (c, i) => cuml += c; i+2 } } ==== str.grouped(2).map(_(0).toInt).sum
    T ~ n{ str.wander(1){ (c, i) => cuml += c; i+2 } } ==== str.grouped(2).filter(_.length == 2).map(_(1).toInt).sum
    T ~    str.wander(){  (_, i) =>            i+2 }   ==== str.grouped(2).map(_(0).toInt).length
    T ~    str.wander(1){ (_, i) =>            i+2 }   ==== str.grouped(2).filter(_.length == 2).map(_(1).toInt).length

    T ~ str.gather(0)()(_ + _ + _)       ==== str.map(_.toInt).sum + str.length*(str.length-1)/2
    T ~ str.gather(0)(3, 5)(_ + _ + _)   ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.gather(0)(civ)(_ + _ + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.gather(0)(3 to 4)(_ + _ + _) ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.gather(0)(cpv)(_ + _ + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.gather(0)(ix)(_ + _ + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum
    T ~ str.gather(0)(st)(_ + _ + _)     ==== ix.map(i => str(i).toInt).sum + ix.sum

    T ~ str.dupWith(_.toUpper) ==== str.toUpperCase

    T ~ str.where(_.isLetter) =**= str.zipWithIndex.collect{ case (c, i) if c.isLetter => i }

    val cx = "___________".toCharArray
    var ninja = 0
    T ~ cx.dup(a => ninja = str.inject(a)).str                ==== "ch.ix.#n.__"
    T ~ { val x = ninja; ninja = 0; x }                       ==== str.length
    T ~ cx.dup(a => ninja = str.inject(a, 2)).str             ==== "__ch.ix.#n."
    T ~ { val x = ninja; ninja = 0; x }                       ==== str.length
    T ~ cx.dup(a => ninja = str.inject(a)(3, 5)).str          ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a, 2)(3, 5)).str       ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a)(3 to 4)).str        ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a, 2)(3 to 4)).str     ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a)(3, 5)).str          ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a, 2)(3, 5)).str       ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a)(civ)).str           ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a, 2)(civ)).str        ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a)(cpv)).str           ==== "ix_________"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a, 2)(cpv)).str        ==== "__ix_______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2
    T ~ cx.dup(a => ninja = str.inject(a)(ix)).str            ==== ".ihhi______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 5
    T ~ cx.dup(a => ninja = str.inject(a, 1)(ix)).str         ==== "_.ihhi_____"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 5
    T ~ cx.dup(a => ninja = str.inject(a)(st)).str            ==== ".ihhi______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 5
    T ~ cx.dup(a => ninja = str.inject(a, 1)(st)).str         ==== "_.ihhi_____"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 5
    T ~ cx.dup(a => ninja = str.inject(a)(_.isLetter)).str    ==== "chixn______"
    T ~ { val x = ninja; ninja = 0; x }                       ==== str.count(_.isLetter)
    T ~ cx.dup(a => ninja = str.inject(a, 2)(_.isLetter)).str ==== "__chixn____"
    T ~ { val x = ninja; ninja = 0; x }                       ==== str.count(_.isLetter)

    T ~ str.select(3, 5)       ==== "ix"
    T ~ str.select(3 to 4)     ==== "ix"
    T ~ str.select(civ)        ==== "ix"
    T ~ str.select(cpv)        ==== "ix"
    T ~ str.select(ix)         ==== ".ihhi"
    T ~ str.select(st)         ==== ".ihhi"
    T ~ str.select(_.isLetter) ==== "chixn"

    T ~ str.selectOp()(       (c, i) => if i%2 == 0 then '-' else c) =**= "-h-i-.-n-".arr
    T ~ str.selectOp(3, 5)(   (c, i) => if i%2 == 0 then '-' else c) =**= "i-".arr
    T ~ str.selectOp(3 to 4)( (c, i) => if i%2 == 0 then '-' else c) =**= "i-".arr
    T ~ str.selectOp(civ)(    (c, i) => if i%2 == 0 then '-' else c) =**= "i-".arr
    T ~ str.selectOp(cpv)(    (c, i) => if i%2 == 0 then '-' else c) =**= "i-".arr
    T ~ str.selectOp(ix)(     (c, i) => if i%2 == 0 then '-' else c) =**= "-ihhi".arr
    T ~ str.selectOp(st)(     (c, i) => if i%2 == 0 then '-' else c) =**= "-ihhi".arr
    T ~ str.selectOp(_.isLetter)((c,i)=>if i%2 == 0 then '-' else c) =**= "-hi-n".arr

    T ~ str.fusion[Int]((c, i, add) => if !c.isLetter then add(i) else Array(O(Some(c.toString))).os.foreach(c => add(c.toInt))) =**= Array(99, 46, 104, 46, 2, 105, 46, 120, 46, 5, 6, 110, 46, 8)


  @Test
  def stringClippedIntervalDataTest: Unit =
    var cuml = 0
    val str = "ch.#ik."

    val ix = Array(2, 3, 1, 1, 3)
    val ex = Array(2, 0, -1, 3, 6, 7, -9, 400) 
    def st = ix.stepper
    def et = ex.stepper

    val civ = Iv(3, 5)
    val cpv = 3 to End-2
    val eiv = Iv(3, 9)
    val fiv = Iv(-2, 5)
    val fpv = End-9 to End-2
    val biv = Iv(-2, 9)
    val niv = Iv(8, 9)
    val npv = End-9 to End-8

    inline def z[A](inline f: => A): A =
      cuml = 0
      f

    inline def n[A](inline f: => A): Int =
      cuml = 0
      f
      cuml

    T ~ (0 to End + 2) ==== thrown[IllegalArgumentException]

    T ~ z{ str.clip.peek(3, 5)(cuml += _) }   ==== str
    T ~ cuml                                    ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.clip.peek(3 to 4)(cuml += _) } ==== str
    T ~ cuml                                    ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.clip.peek(civ)(cuml += _) }    ==== str
    T ~ cuml                                    ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.clip.peek(cpv)(cuml += _) }    ==== str
    T ~ cuml                                    ==== str.substring(3, 5).map(_.toInt).sum
    T ~ z{ str.clip.peek(ix)(cuml += _) }     ==== str
    T ~ cuml                                    ==== ".#hh#".map(_.toInt).sum
    T ~ z{ str.clip.peek(st)(cuml += _) }     ==== str
    T ~ cuml                                    ==== ".#hh#".map(_.toInt).sum

    T ~ n{ str.clip.peek(3, 9)(cuml += _) }    ==== str.substring(3).map(_.toInt).sum
    T ~ n{ str.clip.peek(3 to 8)(cuml += _) }  ==== str.substring(3).map(_.toInt).sum
    T ~ n{ str.clip.peek(eiv)(cuml += _) }     ==== str.substring(3).map(_.toInt).sum

    T ~ n{ str.clip.peek(-2, 5)(cuml += _) }   ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ str.clip.peek(-2 to 4)(cuml += _) } ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ str.clip.peek(fiv)(cuml += _) }     ==== str.substring(0, 5).map(_.toInt).sum
    T ~ n{ str.clip.peek(fpv)(cuml += _) }     ==== str.substring(0, 5).map(_.toInt).sum

    T ~ n{ str.clip.peek(-2, 9)(cuml += _) }   ==== str.map(_.toInt).sum
    T ~ n{ str.clip.peek(-2 to 9)(cuml += _) } ==== str.map(_.toInt).sum
    T ~ n{ str.clip.peek(biv)(cuml += _) }     ==== str.map(_.toInt).sum

    T ~ n{ str.clip.peek(8, 9)(cuml += _) }    ==== 0
    T ~ n{ str.clip.peek(8 to 9)(cuml += _) }  ==== 0
    T ~ n{ str.clip.peek(niv)(cuml += _) }     ==== 0
    T ~ n{ str.clip.peek(npv)(cuml += _) }     ==== 0

    T ~ n{ str.clip.peek(ex)(cuml += _) }      ==== ".c#.".map(_.toInt).sum
    T ~ n{ str.clip.peek(et)(cuml += _) }      ==== ".c#.".map(_.toInt).sum

    def sm(i: Int, j: Int) = j*(j+1)/2 - i*(i-1)/2
    T ~ n{ str.clip.visit(3, 5)(cuml += _ + _) }    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.clip.visit(3 to 4)(cuml += _ + _) }  ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.clip.visit(civ)(cuml += _ + _) }     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.clip.visit(cpv)(cuml += _ + _) }     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ n{ str.clip.visit(ix)(cuml += _ + _) }      ==== ".#hh#".map(_.toInt).sum + 10
    T ~ n{ str.clip.visit(st)(cuml += _ + _) }      ==== ".#hh#".map(_.toInt).sum + 10

    T ~ n{ str.clip.visit(3, 9)(cuml += _ + _) }    ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ n{ str.clip.visit(3 to 8)(cuml += _ + _) }  ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ n{ str.clip.visit(eiv)(cuml += _ + _) }     ==== str.substring(3).map(_.toInt).sum + sm(3, 6)

    T ~ n{ str.clip.visit(-2, 5)(cuml += _ + _) }   ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ str.clip.visit(-2 to 4)(cuml += _ + _) } ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ str.clip.visit(fiv)(cuml += _ + _) }     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ str.clip.visit(fpv)(cuml += _ + _) }     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ n{ str.clip.visit(-2, 9)(cuml += _ + _) }   ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ n{ str.clip.visit(-2 to 9)(cuml += _ + _) } ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ n{ str.clip.visit(biv)(cuml += _ + _) }     ==== str.map(_.toInt).sum + sm(0, 6)

    T ~ n{ str.clip.visit(8, 9)(cuml += _ + _) }    ==== 0
    T ~ n{ str.clip.visit(8 to 9)(cuml += _ + _) }  ==== 0
    T ~ n{ str.clip.visit(niv)(cuml += _ + _) }     ==== 0
    T ~ n{ str.clip.visit(npv)(cuml += _ + _) }     ==== 0

    T ~ n{ str.clip.visit(ex)(cuml += _ + _) }      ==== ".c#.".map(_.toInt).sum + 11
    T ~ n{ str.clip.visit(et)(cuml += _ + _) }      ==== ".c#.".map(_.toInt).sum + 11

    T ~ str.clip.gather(0)(3, 5)(_ + _ + _)    ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.clip.gather(0)(3 to 4)(_ + _ + _)  ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.clip.gather(0)(civ)(_ + _ + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.clip.gather(0)(cpv)(_ + _ + _)     ==== str.substring(3, 5).map(_.toInt).sum + 7
    T ~ str.clip.gather(0)(ix)(_ + _ + _)      ==== ".#hh#".map(_.toInt).sum + 10
    T ~ str.clip.gather(0)(st)(_ + _ + _)      ==== ".#hh#".map(_.toInt).sum + 10

    T ~ str.clip.gather(0)(3, 9)(_ + _ + _)    ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ str.clip.gather(0)(3 to 8)(_ + _ + _)  ==== str.substring(3).map(_.toInt).sum + sm(3, 6)
    T ~ str.clip.gather(0)(eiv)(_ + _ + _)     ==== str.substring(3).map(_.toInt).sum + sm(3, 6)

    T ~ str.clip.gather(0)(-2, 5)(_ + _ + _)   ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ str.clip.gather(0)(-2 to 4)(_ + _ + _) ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ str.clip.gather(0)(fiv)(_ + _ + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)
    T ~ str.clip.gather(0)(fpv)(_ + _ + _)     ==== str.substring(0, 5).map(_.toInt).sum + sm(0, 4)

    T ~ str.clip.gather(0)(-2, 9)(_ + _ + _)   ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ str.clip.gather(0)(-2 to 9)(_ + _ + _) ==== str.map(_.toInt).sum + sm(0, 6)
    T ~ str.clip.gather(0)(biv)(_ + _ + _)     ==== str.map(_.toInt).sum + sm(0, 6)

    T ~ str.clip.gather(0)(8, 9)(_ + _ + _)    ==== 0
    T ~ str.clip.gather(0)(8 to 9)(_ + _ + _)  ==== 0
    T ~ str.clip.gather(0)(niv)(_ + _ + _)     ==== 0
    T ~ str.clip.gather(0)(npv)(_ + _ + _)     ==== 0

    T ~ str.clip.gather(0)(ex)(_ + _ + _)      ==== ".c#.".map(_.toInt).sum + 11
    T ~ str.clip.gather(0)(et)(_ + _ + _)      ==== ".c#.".map(_.toInt).sum + 11

    val ca9 = "ABCDEFGHI".arr
    val ca7 = "1234567".arr
    val ca3 = "890".arr
    val ca1 = "%".arr

    var ninja = 0
    T ~ ca9.dup(a => ninja += str.clip.inject(a)).str            ==== "ch.#ik.HI"
    T ~ ca9.dup(a => ninja += str.clip.inject(a, 2)).str         ==== "ABch.#ik."
    T ~ { val x = ninja; ninja = 0; x }                          ==== 2*str.length
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(3, 5)).str      ==== "#i34567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(3, 5)).str   ==== "12#i567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(3 to 4)).str    ==== "#i34567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(3 to 4)).str ==== "12#i567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(civ)).str       ==== "#i34567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(civ)).str    ==== "12#i567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(cpv)).str       ==== "#i34567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(cpv)).str    ==== "12#i567"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 8*2
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(ix)).str        ==== ".#hh#67"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(ix)).str     ==== "12.#hh#"
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(st)).str        ==== ".#hh#67"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(st)).str     ==== "12.#hh#"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 4*5

    T ~ ca3.dup(a => ninja += str.clip.inject(a)).str    ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)).str ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)).str ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                  ==== 3+5+0

    T ~ ca1.dup(a => ninja += str.clip.inject(a)(3, 5)).str      ==== "#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 2)(3, 5)).str   ==== "89#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(3, 5)).str   ==== "890"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(3 to 4)).str    ==== "#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 2)(3 to 4)).str ==== "89#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(3 to 4)).str ==== "890"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(civ)).str       ==== "#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 2)(civ)).str    ==== "89#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(civ)).str    ==== "890"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(cpv)).str       ==== "#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 2)(cpv)).str    ==== "89#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(cpv)).str    ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 4*(1+1+0)

    T ~ ca3.dup(a => ninja += str.clip.inject(a)(3, 9)).str      ==== "#ik"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 5)(3, 9)).str   ==== "12345#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(3, 9)).str   ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(3 to 8)).str    ==== "#ik"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 5)(3 to 8)).str ==== "12345#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(3 to 8)).str ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(eiv)).str       ==== "#ik"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 5)(eiv)).str    ==== "12345#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(eiv)).str    ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 3*(3+2+0)

    T ~ ca3.dup(a => ninja += str.clip.inject(a)(-2, 5)).str      ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 3)(-2, 5)).str   ==== "123ch.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(-2, 5)).str   ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(-2 to 4)).str    ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 3)(-2 to 4)).str ==== "123ch.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(-2 to 4)).str ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(fiv)).str        ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 3)(fiv)).str     ==== "123ch.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(fiv)).str     ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(fpv)).str        ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 3)(fpv)).str     ==== "123ch.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(fpv)).str     ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                           ==== 4*(3+4+0)

    T ~ ca3.dup(a => ninja += str.clip.inject(a)(-2, 9)).str      ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(-2, 9)).str   ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(-2, 9)).str   ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(-2 to 8)).str    ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(-2 to 8)).str ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(-2 to 8)).str ==== "890"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(biv)).str        ==== "ch."
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(biv)).str     ==== "12ch.#i"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(biv)).str     ==== "890"
    T ~ { val x = ninja; ninja = 0; x }                           ==== 3*(3+5+0)

    T ~ ca1.dup(a => ninja += str.clip.inject(a)(8, 10)).str     ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a, 2)(8, 10)).str  ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(8 to 9)).str    ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a, 2)(8 to 9)).str ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(niv)).str       ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a, 2)(niv)).str    ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a)(npv)).str       ==== "%"
    T ~ ca1.dup(a => ninja += str.clip.inject(a, 2)(npv)).str    ==== "%"
    T ~ { val x = ninja; ninja = 0; x }                          ==== 0

    T ~ ca3.dup(a => ninja += str.clip.inject(a)(ix)).str     ==== ".#h"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 1)(ix)).str  ==== "8.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(ix)).str  ==== "890"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, -1)(ix)).str ==== ".#hh#67"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(st)).str     ==== ".#h"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 1)(st)).str  ==== "8.#"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 4)(st)).str  ==== "890"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, -1)(st)).str ==== ".#hh#67"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2*(3+2+0+5)    
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(ex)).str     ==== ".c#.567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 4)(ex)).str  ==== "1234.c#"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 9)(ex)).str  ==== "1234567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, -1)(ex)).str ==== ".c#.567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a)(et)).str     ==== ".c#.567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 4)(et)).str  ==== "1234.c#"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 9)(et)).str  ==== "1234567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, -1)(et)).str ==== ".c#.567"
    T ~ { val x = ninja; ninja = 0; x }                       ==== 2*(4+3+0+4)

    T ~ ca7.dup(a => ninja += str.clip.inject(a)(_.isLetter)).str    ==== "chik567"
    T ~ ca7.dup(a => ninja += str.clip.inject(a, 2)(_.isLetter)).str ==== "12chik7"
    T ~ ca3.dup(a => ninja += str.clip.inject(a)(_.isLetter)).str    ==== "chi"
    T ~ ca3.dup(a => ninja += str.clip.inject(a, 2)(_.isLetter)).str ==== "89c"
    T ~ { val x = ninja; ninja = 0; x }                              ==== 2*4 + 3 + 1

    T ~ str.clip.select(3, 5)   ==== "#i"
    T ~ str.clip.select(3 to 4) ==== "#i"
    T ~ str.clip.select(civ)    ==== "#i"
    T ~ str.clip.select(cpv)    ==== "#i"
    T ~ str.clip.select(ix)     ==== ".#hh#"
    T ~ str.clip.select(st)     ==== ".#hh#"

    T ~ str.clip.select(3, 9)   ==== "#ik."
    T ~ str.clip.select(3 to 8) ==== "#ik."
    T ~ str.clip.select(eiv)    ==== "#ik."

    T ~ str.clip.select(-2, 5)   ==== "ch.#i"
    T ~ str.clip.select(-2 to 4) ==== "ch.#i"
    T ~ str.clip.select(fiv)     ==== "ch.#i"
    T ~ str.clip.select(fpv)     ==== "ch.#i"

    T ~ str.clip.select(-2, 9)   ==== "ch.#ik."
    T ~ str.clip.select(-2 to 8) ==== "ch.#ik."
    T ~ str.clip.select(biv)     ==== "ch.#ik."

    T ~ str.clip.select(8, 10)  ==== ""
    T ~ str.clip.select(8 to 9) ==== ""
    T ~ str.clip.select(niv)    ==== ""
    T ~ str.clip.select(npv)    ==== ""

    T ~ str.clip.select(et)    ==== ".c#."
    T ~ str.clip.select(et)    ==== ".c#."

    T ~ str.clip.selectOp(3, 5  )((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.selectOp(3 to 4)((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.selectOp(civ   )((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.selectOp(cpv   )((c, i) => c + i) =**= "&m".map(_.toInt)
    T ~ str.clip.selectOp(ix    )((c, i) => c + i) =**= "0&ii&".map(_.toInt)
    T ~ str.clip.selectOp(st    )((c, i) => c + i) =**= "0&ii&".map(_.toInt)
    T ~ str.clip.selectOp(3, 5  )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.selectOp(3 to 4)((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.selectOp(civ   )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.selectOp(cpv   )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.selectOp(ix    )((c, i) => c + i) ==== typed[Array[Int]]
    T ~ str.clip.selectOp(st    )((c, i) => c + i) ==== typed[Array[Int]]

    T ~ str.clip.selectOp(3, 9  )((c, i) => c + i) =**= "&mp4".map(_.toInt)
    T ~ str.clip.selectOp(3 to 8)((c, i) => c + i) =**= "&mp4".map(_.toInt)
    T ~ str.clip.selectOp(eiv   )((c, i) => c + i) =**= "&mp4".map(_.toInt)

    T ~ str.clip.selectOp(-2, 5  )((c, i) => c + i) =**= "ci0&m".map(_.toInt)
    T ~ str.clip.selectOp(-2 to 4)((c, i) => c + i) =**= "ci0&m".map(_.toInt)
    T ~ str.clip.selectOp(fiv    )((c, i) => c + i) =**= "ci0&m".map(_.toInt)
    T ~ str.clip.selectOp(fpv    )((c, i) => c + i) =**= "ci0&m".map(_.toInt)

    T ~ str.clip.selectOp(-2, 9  )((c, i) => c + i) =**= "ci0&mp4".map(_.toInt)
    T ~ str.clip.selectOp(-2 to 8)((c, i) => c + i) =**= "ci0&mp4".map(_.toInt)
    T ~ str.clip.selectOp(biv    )((c, i) => c + i) =**= "ci0&mp4".map(_.toInt)

    T ~ str.clip.selectOp(8, 10 )((c, i) => c + i) =**= "".map(_.toInt)
    T ~ str.clip.selectOp(8 to 9)((c, i) => c + i) =**= "".map(_.toInt)
    T ~ str.clip.selectOp(niv   )((c, i) => c + i) =**= "".map(_.toInt)
    T ~ str.clip.selectOp(npv   )((c, i) => c + i) =**= "".map(_.toInt)

    T ~ str.clip.selectOp(ex)((c, i) => c + i) =**= "0c&4".map(_.toInt)
    T ~ str.clip.selectOp(et)((c, i) => c + i) =**= "0c&4".map(_.toInt)

}
object BasicsTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
