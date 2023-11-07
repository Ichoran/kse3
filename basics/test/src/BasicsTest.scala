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
class BasicsTest {
  import kse.testutilities.TestUtilities.{_, given}
  import kse.basics.{_, given}

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


  @Test
  def arrayDataTest(): Unit =
    import java.lang.Float.{intBitsToFloat => i2f}
    import java.lang.Double.{longBitsToDouble => l2d}

    T ~ Iv(5, 8)                    ==== 0x800000005L  --: typed[Iv]
    T ~ Iv(5, 8).i0                 ==== 5
    T ~ Iv(5, 8).i1                 ==== 8

    val atf = Array(false, false, true, true)
    val aip = Array(2, 3, -3)
    val air = Array(3, 4, 3)
    var cuml = 0

    object NuZ extends NewType[Boolean] {}
    val az = Array[Boolean](true, true, false)
    val naz = Array[NuZ.Type](NuZ(true), NuZ(true), NuZ(false))
    T ~ az.copy         =**= az
    T ~ (az.copy eq az) ==== false
    T ~ naz.copy        =**= naz

    object NuB extends NewType[Byte] {}
    val ab = Array[Byte](1, 2, 3)
    val bb = Array[Byte](0, 1, 2, 3, 4)
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
    T ~ ab.addLeftSlots(1)          =**= (0 +: ab)
    T ~ ab.addRightSlots(1)         =**= (ab :+ 0)
    T ~ ab.copyInto(bb)             =**= Array[Byte](1, 2, 3, 3, 4)
    T ~ ab.copyInto(bb, 2)          =**= Array[Byte](1, 2, 1, 2, 3)
    T ~ ab.copyRangeInto(1,3)(bb)   =**= Array[Byte](2, 3, 1, 2, 3)
    T ~ ab.copyRangeInto(1,3)(bb,1) =**= Array[Byte](2, 2, 3, 2, 3)
    T ~ ab.py(1)                    ==== ab(1)
    T ~ ab.py(-3)                   ==== ab(0)
    T ~ bb.py.index(-2)             ==== 3
    T ~ { bb.py(-4) = (0:Byte); bb }=**= Array[Byte](2, 0, 3, 2, 3)
    T ~ bb.py(atf)                  =**= Array[Byte](bb(2), bb(3))
    T ~ { bb.py(atf)=(8:Byte); bb } =**= Array[Byte](2, 0, 8, 8, 3)
    T ~ { bb.py(atf) = ab; bb }     =**= Array[Byte](2, 0, 1, 2, 3)
    T ~ bb.py(aip)                  =**= Array[Byte](bb(2), bb(3), bb(2))
    T ~ { bb.py(aip)=(7:Byte); bb } =**= Array[Byte](2, 0, 7, 7, 3)
    T ~ { bb.py(aip) = ab; bb }     =**= Array[Byte](2, 0, 3, 2, 3)
    T ~ ab.R(2)                     ==== ab(1)
    T ~ bb.R(air)                   =**= Array(bb(2), bb(3), bb(2))
    T ~ { bb.R(3) = (4:Byte); bb }  =**= Array[Byte](2, 0, 4, 2, 3)
    T ~ { bb.R(air) = (5:Byte); bb }=**= Array[Byte](2, 0, 5, 5, 3)
    T ~ bb.R(2 to 4)                =**= Array[Byte](0, 5, 5)
    T ~ { bb.R(3 to 4)=(6:Byte);bb} =**= Array[Byte](2, 0, 6, 6, 3)
    T ~ { bb.R(air) = ab; bb }      =**= Array[Byte](2, 0, 3, 2, 3)
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
    bb.visit((x, i) => cuml += (x+1)*(i+1))
    bb.visitRange(2, 4)((x, i) => cuml += i - x - 1)
    T ~ cuml                        ==== 42
    T ~ bb.fill(4)                  =**= Array[Byte](4, 4, 4, 4, 4)

    object NuS extends NewType[Short] {}
    val as = Array[Short](1, 2, 3)
    val bs = Array[Short](0, 1, 2, 3, 4)
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
    T ~ as.addLeftSlots(1)          =**= (0 +: as)
    T ~ as.addRightSlots(1)         =**= (as :+ 0)
    T ~ as.copyInto(bs)             =**= Array[Short](1, 2, 3, 3, 4)
    T ~ as.copyInto(bs, 2)          =**= Array[Short](1, 2, 1, 2, 3)
    T ~ as.copyRangeInto(1,3)(bs)   =**= Array[Short](2, 3, 1, 2, 3)
    T ~ as.copyRangeInto(1,3)(bs,1) =**= Array[Short](2, 2, 3, 2, 3)
    T ~ as.py(1)                    ==== as(1)
    T ~ as.py(-3)                   ==== as(0)
    T ~ bs.py.index(-2)             ==== 3
    T ~ { bs.py(-4)=(0:Short); bs } =**= Array[Short](2, 0, 3, 2, 3)
    T ~ bs.py(atf)                  =**= Array[Short](bs(2), bs(3))
    T ~ { bs.py(atf)=(8:Short); bs }=**= Array[Short](2, 0, 8, 8, 3)
    T ~ { bs.py(atf) = as; bs }     =**= Array[Short](2, 0, 1, 2, 3)
    T ~ bs.py(aip)                  =**= Array[Short](bs(2), bs(3), bs(2))
    T ~ { bs.py(aip)=(7:Short); bs }=**= Array[Short](2, 0, 7, 7, 3)
    T ~ { bs.py(aip) = as; bs }     =**= Array[Short](2, 0, 3, 2, 3)
    T ~ as.R(2)                     ==== as(1)
    T ~ bs.R(air)                   =**= Array(bs(2), bs(3), bs(2))
    T ~ { bs.R(3) = (4:Short); bs } =**= Array[Short](2, 0, 4, 2, 3)
    T ~ { bs.R(air)=(5:Short); bs } =**= Array[Short](2, 0, 5, 5, 3)
    T ~ bs.R(2 to 4)                =**= Array[Short](0, 5, 5)
    T ~ {bs.R(3 to 4)=(6:Short);bs} =**= Array[Short](2, 0, 6, 6, 3)
    T ~ { bs.R(air) = as; bs }      =**= Array[Short](2, 0, 3, 2, 3)
    T ~ bs.isSorted                 ==== false
    T ~ bs.isSortedRange(1, 3)      ==== true
    T ~ as.search(2)                ==== 1
    T ~ as.search(0)                ==== -1
    T ~ bs.searchRange(1, 3)(3)     ==== 2
    T ~ bs.searchRange(1, 3)(2)     ==== -3
    T ~ bs.sortRange(0, 3)          =**= Array[Short](0, 2, 3, 2, 3)
    T ~ bs.sort()                   =**= Array[Short](0, 2, 2, 3, 3)
    T ~ bs.fillRange(2, 4)(1)       =**= Array[Short](0, 2, 1, 1, 3)
    bs.visit((x, i) => cuml += (x+1)*(i+1))
    bs.visitRange(2, 4)((x, i) => cuml += i - x - 1)
    T ~ cuml                        ==== 84
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
    T ~ bc.py(atf)                  =**= Array[Char](bc(2), bc(3))
    T ~ { bc.py(atf) = '8'; bc }    =**= Array[Char]('2', '0', '8', '8', '3')
    T ~ { bc.py(atf) = ac; bc }     =**= Array[Char]('2', '0', '1', '2', '3')
    T ~ bc.py(aip)                  =**= Array[Char](bc(2), bc(3), bc(2))
    T ~ { bc.py(aip) = '7'; bc }    =**= Array[Char]('2', '0', '7', '7', '3')
    T ~ { bc.py(aip) = ac; bc }     =**= Array[Char]('2', '0', '3', '2', '3')
    T ~ ac.R(2)                     ==== ac(1)
    T ~ bc.R(air)                   =**= Array(bc(2), bc(3), bc(2))
    T ~ { bc.R(3) = '4'; bc }       =**= Array[Char]('2', '0', '4', '2', '3')
    T ~ { bc.R(air) = '5'; bc }     =**= Array[Char]('2', '0', '5', '5', '3')
    T ~ bc.R(2 to 4)                =**= Array[Char]('0', '5', '5')
    T ~ { bc.R(3 to 4) = '6'; bc }  =**= Array[Char]('2', '0', '6', '6', '3')
    T ~ { bc.R(air) = ac; bc }      =**= Array[Char]('2', '0', '3', '2', '3')
    T ~ bc.isSorted                 ==== false
    T ~ bc.isSortedRange(1, 3)      ==== true
    T ~ ac.search('2')              ==== 1
    T ~ ac.search('0')              ==== -1
    T ~ bc.searchRange(1, 3)('3')   ==== 2
    T ~ bc.searchRange(1, 3)('2')   ==== -3
    T ~ bc.sortRange(0, 3)          =**= Array[Char]('0', '2', '3', '2', '3')
    T ~ bc.sort()                   =**= Array[Char]('0', '2', '2', '3', '3')
    T ~ bc.fillRange(2, 4)('e')     =**= Array[Char]('0', '2', 'e', 'e', '3')
    bc.visit((x, i) => cuml += x.getNumericValue + i)
    bc.visitRange(2, 4)((x, i) => cuml += x*i - 248)
    T ~ cuml                        ==== 136
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
    T ~ bi.py(atf)                  =**= Array[Int](bi(2), bi(3))
    T ~ { bi.py(atf) = 8; bi }      =**= Array[Int](2, 0, 8, 8, 3)
    T ~ { bi.py(atf) = ai; bi }     =**= Array[Int](2, 0, 1, 2, 3)
    T ~ bi.py(aip)                  =**= Array[Int](bi(2), bi(3), bi(2))
    T ~ { bi.py(aip) = 7; bi }      =**= Array[Int](2, 0, 7, 7, 3)
    T ~ { bi.py(aip) = ai; bi }     =**= Array[Int](2, 0, 3, 2, 3)
    T ~ ai.R(2)                     ==== ai(1)
    T ~ bi.R(air)                   =**= Array(bi(2), bi(3), bi(2))
    T ~ { bi.R(3) = 4; bi }         =**= Array[Int](2, 0, 4, 2, 3)
    T ~ { bi.R(air) = 5; bi }       =**= Array[Int](2, 0, 5, 5, 3)
    T ~ bi.R(2 to 4)                =**= Array[Int](0, 5, 5)
    T ~ { bi.R(3 to 4) = 6; bi}     =**= Array[Int](2, 0, 6, 6, 3)
    T ~ { bi.R(air) = ai; bi }      =**= Array[Int](2, 0, 3, 2, 3)
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
    bi.visit((x, i) => cuml += (x+1)*(i+1))
    bi.visitRange(2, 4)((x, i) => cuml += i - x - 1)
    T ~ cuml                        ==== 178
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
    T ~ bl.py(atf)                  =**= Array[Long](bl(2), bl(3))
    T ~ { bl.py(atf) = 8; bl }      =**= Array[Long](2, 0, 8, 8, 3)
    T ~ { bl.py(atf) = al; bl }     =**= Array[Long](2, 0, 1, 2, 3)
    T ~ bl.py(aip)                  =**= Array[Long](bl(2), bl(3), bl(2))
    T ~ { bl.py(aip) = 7; bl }      =**= Array[Long](2, 0, 7, 7, 3)
    T ~ { bl.py(aip) = al; bl }     =**= Array[Long](2, 0, 3, 2, 3)
    T ~ al.R(2)                     ==== al(1)
    T ~ bl.R(air)                   =**= Array(bl(2), bl(3), bl(2))
    T ~ { bl.R(3) = 4; bl }         =**= Array[Long](2, 0, 4, 2, 3)
    T ~ { bl.R(air) = 5; bl }       =**= Array[Long](2, 0, 5, 5, 3)
    T ~ bl.R(2 to 4)                =**= Array[Long](0, 5, 5)
    T ~ { bl.R(3 to 4) = 6; bl}     =**= Array[Long](2, 0, 6, 6, 3)
    T ~ { bl.R(air) = al; bl }      =**= Array[Long](2, 0, 3, 2, 3)
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
    bl.visit((x, i) => cuml += (x+1).toInt*(i+1))
    bl.visitRange(2, 4)((x, i) => cuml += i - x.toInt - 1)
    T ~ cuml                        ==== 220
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
    T ~ bf.py(atf)                  =**= Array[Float](bf(2), bf(3))
    T ~ { bf.py(atf) = 8; bf }      =**= Array[Float](2, 0, 8, 8, 3)
    T ~ { bf.py(atf) = af; bf }     =**= Array[Float](2, 0, 1, 2, 3)
    T ~ bf.py(aip)                  =**= Array[Float](bf(2), bf(3), bf(2))
    T ~ { bf.py(aip) = 7; bf }      =**= Array[Float](2, 0, 7, 7, 3)
    T ~ { bf.py(aip) = af; bf }     =**= Array[Float](2, 0, 3, 2, 3)
    T ~ af.R(2)                     ==== af(1)
    T ~ bf.R(air)                   =**= Array(bf(2), bf(3), bf(2))
    T ~ { bf.R(3) = 4; bf }         =**= Array[Float](2, 0, 4, 2, 3)
    T ~ { bf.R(air) = 5; bf }       =**= Array[Float](2, 0, 5, 5, 3)
    T ~ bf.R(2 to 4)                =**= Array[Float](0, 5, 5)
    T ~ { bf.R(3 to 4) = 6; bf}     =**= Array[Float](2, 0, 6, 6, 3)
    T ~ { bf.R(air) = af; bf }      =**= Array[Float](2, 0, 3, 2, 3)
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
    bf.visit((x, i) => cuml += (x+1).toInt*(i+1))
    bf.visitRange(2, 4)((x, i) => cuml += i - x.toInt - 1)
    T ~ cuml                        ==== 262
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
    T ~ bd.py(atf)                  =**= Array[Double](bd(2), bd(3))
    T ~ { bd.py(atf) = 8; bd }      =**= Array[Double](2, 0, 8, 8, 3)
    T ~ { bd.py(atf) = ad; bd }     =**= Array[Double](2, 0, 1, 2, 3)
    T ~ bd.py(aip)                  =**= Array[Double](bd(2), bd(3), bd(2))
    T ~ { bd.py(aip) = 7; bd }      =**= Array[Double](2, 0, 7, 7, 3)
    T ~ { bd.py(aip) = ad; bd }     =**= Array[Double](2, 0, 3, 2, 3)
    T ~ ad.R(2)                     ==== ad(1)
    T ~ bd.R(air)                   =**= Array(bd(2), bd(3), bd(2))
    T ~ { bd.R(3) = 4; bd }         =**= Array[Double](2, 0, 4, 2, 3)
    T ~ { bd.R(air) = 5; bd }       =**= Array[Double](2, 0, 5, 5, 3)
    T ~ bd.R(2 to 4)                =**= Array[Double](0, 5, 5)
    T ~ { bd.R(3 to 4) = 6; bd}     =**= Array[Double](2, 0, 6, 6, 3)
    T ~ { bd.R(air) = ad; bd }      =**= Array[Double](2, 0, 3, 2, 3)
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
    bd.visit((x, i) => cuml += (x+1).toInt*(i+1))
    bd.visitRange(2, 4)((x, i) => cuml += i - x.toInt - 1)
    T ~ cuml                        ==== 304
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
    T ~ ba.py(atf)                  =**= Array[String](ba(2), ba(3))
    T ~ { ba.py(atf) = "8"; ba }    =**= Array[String]("2", "0", "8", "8", "3")
    T ~ { ba.py(atf) = aa; ba }     =**= Array[String]("2", "0", "1", "2", "3")
    T ~ ba.py(aip)                  =**= Array[String](ba(2), ba(3), ba(2))
    T ~ { ba.py(aip) = "7"; ba }    =**= Array[String]("2", "0", "7", "7", "3")
    T ~ { ba.py(aip) = aa; ba }     =**= Array[String]("2", "0", "3", "2", "3")
    T ~ aa.R(2)                     ==== aa(1)
    T ~ ba.R(air)                   =**= Array(ba(2), ba(3), ba(2))
    T ~ { ba.R(3) = "4"; ba }       =**= Array[String]("2", "0", "4", "2", "3")
    T ~ { ba.R(air) = "5"; ba }     =**= Array[String]("2", "0", "5", "5", "3")
    T ~ ba.R(2 to 4)                =**= Array[String]("0", "5", "5")
    T ~ { ba.R(3 to 4) = "6"; ba }  =**= Array[String]("2", "0", "6", "6", "3")
    T ~ { ba.R(air) = aa; ba }      =**= Array[String]("2", "0", "3", "2", "3")
    T ~ ba.isSorted                 ==== false
    T ~ ba.isSortedRange(1, 3)      ==== true
    T ~ aa.search("2")              ==== 1
    T ~ aa.search("0")              ==== -1
    T ~ ba.searchRange(1, 3)("3")   ==== 2
    T ~ ba.searchRange(1, 3)("2")   ==== -3
    T ~ ba.sortRange(0, 3)          =**= Array[String]("0", "2", "3", "2", "3")
    T ~ ba.sort()                   =**= Array[String]("0", "2", "2", "3", "3")
    T ~ ba.fillRange(2, 4)("e")     =**= Array[String]("0", "2", "e", "e", "3")
    ba.visit((x, i) => cuml += x.head.getNumericValue + i)
    ba.visitRange(2, 4)((x, i) => cuml += x.head*i - 253)
    T ~ cuml                        ==== 346
    T ~ ba.fill("4")                =**= Array[String]("4", "4", "4", "4", "4")
}
object BasicsTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
