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

class TuplesTest() {
  import compiletime.testing.{typeChecks => cc}

  import kse.testutilities.TestUtilities.{_, given}
  import kse.basics.{_, given}
  import kse.basics.intervals._
  import kse.basics.labels._

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


  def unlabelledTuples(): Unit =
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


  def labelledSingleton(): Unit =
    val eel: "eel" = "eel"
    val a3: Any = 3

    T ~ (kse.basics.\(3): Int \ "eel")  ==== 3 --: typed[Int \ "eel"]
    T ~ (3 \ "eel")                     ==== 3 --: typed[Int \ "eel"]
    T ~ (3 \ "eel").label               ==== "eel"
    T ~ 3.labelled["eel"]               ==== 3 --: typed[Int \ "eel"]
    T ~ (3 \ (4 \ "cod").label)         ==== 3 --: typed[Int \ "cod"]
    T ~ ((3 \ "eel") ~ "eel")           ==== 3 --: typed[Int]
    T ~ (3 \ "eel").unlabel             ==== 3 --: typed[Int]
    T ~ (3 \ "eel").valueTo(4)          ==== 4 --: typed[Int \ "eel"]
    T ~ (3 \ "eel").valueOp(_+2)        ==== 5 --: typed[Int \ "eel"]
    T ~ (3 \ "eel")().eel               ==== 3 --: typed[Int]
    T ~ subtyping(3, 3 \ "eel")         ==== 'X'
    T ~ subtyping(3 \ "eel", 3 \ "cod") ==== 'X'
    T ~ (3 \ "eel" == 3 \ "cod")        ==== true
    T ~ subtyping(3 \ eel, 3 \ "eel")   ==== '='
    T ~ subtyping(a3 \ eel, 3 \ eel)    ==== '>'


  def labelledDoublet(): Unit =
    val t2 = (1, 2)
    val l2 = (1 \ "a", 2 \ "b")
    T ~ l2                    ==== (1, 2) --: typed[(Int \ "a", Int \ "b")]
    T ~ t2.label["a", "b"]    ==== (1, 2) --: typed[(Int \ "a", Int \ "b")]
    T ~ (t2 \\ ("a", "b"))    ==== (1, 2) --: typed[(Int \ "a", Int \ "b")]
    T ~ (l2 ~ "a")            ==== 1      --: typed[Int]
    T ~ (l2 ~ "b")            ==== 2      --: typed[Int]
    T ~ l2.unlabel            ==== (1, 2) --: typed[(Int, Int)]
    T ~ l2.label_1            ==== "a"
    T ~ l2.label_2            ==== "b"
    T ~ l2.labels             ==== ("a", "b")
    T ~ l2().a                ==== 1      --: typed[Int]
    T ~ l2().b                ==== 2      --: typed[Int]
    T ~ (l2 ~~ ("a", "b"))    ==== (1, 2) --: typed[(Int, Int)]
    T ~ l2.relabel("a")("_")  ==== (1, 2) --: typed[(Int \ "_", Int \ "b")]
    T ~ l2.relabel("b")("_")  ==== (1, 2) --: typed[(Int \ "a", Int \ "_")]
    T ~ l2.revalue("a")(0)    ==== (0, 2) --: typed[(Int \ "a", Int \ "b")]
    T ~ l2.revalue("b")(0)    ==== (1, 0) --: typed[(Int \ "a", Int \ "b")]
    T ~ l2.redo("a")(0 \ "_") ==== (0, 2) --: typed[(Int \ "_", Int \ "b")]
    T ~ l2.redo("b")(0 \ "_") ==== (1, 0) --: typed[(Int \ "a", Int \ "_")]

    val x12 = (1 \ "a", 2 \ "a")
    T ~ cc("""t2.label["a", "a"]""")     ==== false
    T ~ cc("""t2 \\ ("a", "a")""")       ==== false
    T ~ cc("""l2 ~~ ("a", "_")""")       ==== false
    T ~ cc("""l2 ~~ ("_", "b")""")       ==== false
    T ~ cc("""l2 ~ "_"""")               ==== false
    T ~ cc("""l2.relabel("_")("=")""")   ==== false
    T ~ cc("""l2.revalue("_")(0)""")     ==== false
    T ~ cc("""l2.redo("_")(0 \ "=")""")  ==== false
    T ~ cc("""x12 ~ "a"""")              ==== false
    T ~ cc("""x12().a""")                ==== false
    T ~ cc("""x12.relabel("a")("=")""")  ==== false
    T ~ cc("""x12.revalue("a")(0)""")    ==== false
    T ~ cc("""x12.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""l2.relabel("a")("b")""")   ==== false
    T ~ cc("""l2.relabel("b")("a")""")   ==== false
    T ~ cc("""l2.redo("a")(0 \ "b")""")  ==== false
    T ~ cc("""l2.redo("b")(0 \ "a")""")  ==== false


  def labelledTriplet(): Unit =
    val t3 = (1, 2, 3)
    val l3 = (1 \ "a", 2 \ "b", 3 \ "c")
    T ~ l3                      ==== t3         --: typed[(Int \ "a", Int \ "b", Int \ "c")]
    T ~ t3.label["a", "b", "c"] ==== t3         --: typed[(Int \ "a", Int \ "b", Int \ "c")]
    T ~ (t3 \\ ("a", "b", "c")) ==== t3         --: typed[(Int \ "a", Int \ "b", Int \ "c")]
    T ~ (l3 ~ "a")              ==== 1          --: typed[Int]
    T ~ (l3 ~ "b")              ==== 2          --: typed[Int]
    T ~ (l3 ~ "c")              ==== 3          --: typed[Int]
    T ~ l3.unlabel              ==== t3         --: typed[(Int, Int, Int)]
    T ~ l3.label_1              ==== "a"
    T ~ l3.label_2              ==== "b"
    T ~ l3.label_3              ==== "c"
    T ~ l3.labels               ==== ("a", "b", "c")
    T ~ l3().a                  ==== 1          --: typed[Int]
    T ~ l3().b                  ==== 2          --: typed[Int]
    T ~ l3().c                  ==== 3          --: typed[Int]
    T ~ (l3 ~~ ("a", "b", "c")) ==== t3         --: typed[(Int, Int, Int)] 
    T ~ l3.relabel("a")("_")    ==== t3         --: typed[(Int \ "_", Int \ "b", Int \ "c")]
    T ~ l3.relabel("b")("_")    ==== t3         --: typed[(Int \ "a", Int \ "_", Int \ "c")]
    T ~ l3.relabel("c")("_")    ==== t3         --: typed[(Int \ "a", Int \ "b", Int \ "_")]
    T ~ l3.revalue("a")(0)      ==== t3._1to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c")]
    T ~ l3.revalue("b")(0)      ==== t3._2to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c")]
    T ~ l3.revalue("c")(0)      ==== t3._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c")]
    T ~ l3.redo("a")(0 \ "_")   ==== t3._1to(0) --: typed[(Int \ "_", Int \ "b", Int \ "c")]
    T ~ l3.redo("b")(0 \ "_")   ==== t3._2to(0) --: typed[(Int \ "a", Int \ "_", Int \ "c")]
    T ~ l3.redo("c")(0 \ "_")   ==== t3._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "_")]

    val x12 = (1 \ "a", 2 \ "a", 3 \ "c")
    val x13 = (1 \ "a", 2 \ "b", 3 \ "a")
    val x23 = (1 \ "a", 2 \ "b", 3 \ "b")
    T ~ cc("""t3.label["a", "a", "c"]""") ==== false
    T ~ cc("""t3.label["a", "b", "a"]""") ==== false
    T ~ cc("""t3.label["a", "b", "b"]""") ==== false
    T ~ cc("""t3 \\ ("a", "a", "c")""")   ==== false
    T ~ cc("""t3 \\ ("a", "b", "a")""")   ==== false
    T ~ cc("""t3 \\ ("a", "b", "b")""")   ==== false
    T ~ cc("""l3 ~~ ("_", "b", "c")""")   ==== false
    T ~ cc("""l3 ~~ ("a", "_", "c")""")   ==== false
    T ~ cc("""l3 ~~ ("a", "b", "_")""")   ==== false

    T ~ cc("""l3 ~ "_"""")                ==== false
    T ~ cc("""l3.relabel("_")("=")""")    ==== false
    T ~ cc("""l3.revalue("_")(0)""")      ==== false
    T ~ cc("""l3.redo("_")(0 \ "=")""")   ==== false

    T ~ cc("""x12 ~ "a"""")               ==== false
    T ~ cc("""x12 ~ "c"""")               ==== true
    T ~ cc("""x13 ~ "a"""")               ==== false
    T ~ cc("""x13 ~ "b"""")               ==== true
    T ~ cc("""x23 ~ "b"""")               ==== false
    T ~ cc("""x23 ~ "a"""")               ==== true
    T ~ cc("""x12().a""")                 ==== false
    T ~ cc("""x13().a""")                 ==== false
    T ~ cc("""x23().b""")                 ==== false
    T ~ cc("""x12.relabel("a")("=")""")   ==== false
    T ~ cc("""x12.relabel("c")("=")""")   ==== true
    T ~ cc("""x13.relabel("a")("=")""")   ==== false
    T ~ cc("""x13.relabel("b")("=")""")   ==== true
    T ~ cc("""x23.relabel("b")("=")""")   ==== false
    T ~ cc("""x23.relabel("a")("=")""")   ==== true
    T ~ cc("""x12.revalue("a")(0)""")     ==== false
    T ~ cc("""x12.revalue("c")(0)""")     ==== true
    T ~ cc("""x13.revalue("a")(0)""")     ==== false
    T ~ cc("""x13.revalue("b")(0)""")     ==== true
    T ~ cc("""x23.revalue("b")(0)""")     ==== false
    T ~ cc("""x23.revalue("a")(0)""")     ==== true
    T ~ cc("""x12.redo("a")(0 \ "=")""")  ==== false
    T ~ cc("""x12.redo("c")(0 \ "=")""")  ==== true
    T ~ cc("""x13.redo("a")(0 \ "=")""")  ==== false
    T ~ cc("""x13.redo("b")(0 \ "=")""")  ==== true
    T ~ cc("""x23.redo("b")(0 \ "=")""")  ==== false
    T ~ cc("""x23.redo("a")(0 \ "=")""")  ==== true

    T ~ cc("""l3.relabel("a")("b")""")    ==== false
    T ~ cc("""l3.relabel("a")("c")""")    ==== false
    T ~ cc("""l3.relabel("b")("a")""")    ==== false
    T ~ cc("""l3.relabel("b")("c")""")    ==== false
    T ~ cc("""l3.relabel("c")("a")""")    ==== false
    T ~ cc("""l3.relabel("c")("b")""")    ==== false
    T ~ cc("""l3.redo("a")(0 \ "b")""")   ==== false
    T ~ cc("""l3.redo("a")(0 \ "c")""")   ==== false
    T ~ cc("""l3.redo("b")(0 \ "a")""")   ==== false
    T ~ cc("""l3.redo("b")(0 \ "c")""")   ==== false
    T ~ cc("""l3.redo("c")(0 \ "a")""")   ==== false
    T ~ cc("""l3.redo("c")(0 \ "b")""")   ==== false


  def labelledQuadruplet(): Unit =
    val t4 = (1, 2, 3, 4)
    val l4 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d")
    T ~ l4                           ==== t4         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d")]
    T ~ t4.label["a", "b", "c", "d"] ==== t4         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d")]
    T ~ (t4 \\ ("a", "b", "c", "d")) ==== t4         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d")]
    T ~ (l4 ~ "a")                   ==== 1          --: typed[Int]
    T ~ (l4 ~ "b")                   ==== 2          --: typed[Int]
    T ~ (l4 ~ "c")                   ==== 3          --: typed[Int]
    T ~ (l4 ~ "d")                   ==== 4          --: typed[Int]
    T ~ l4.unlabel                   ==== t4         --: typed[(Int, Int, Int, Int)]
    T ~ l4.label_1                   ==== "a"
    T ~ l4.label_2                   ==== "b"
    T ~ l4.label_3                   ==== "c"
    T ~ l4.label_4                   ==== "d"
    T ~ l4.labels                    ==== ("a", "b", "c", "d")
    T ~ l4().a                       ==== 1          --: typed[Int]
    T ~ l4().b                       ==== 2          --: typed[Int]
    T ~ l4().c                       ==== 3          --: typed[Int]
    T ~ l4().d                       ==== 4          --: typed[Int]
    T ~ (l4 ~~ ("a", "b", "c", "d")) ==== t4         --: typed[(Int, Int, Int, Int)]
    T ~ l4.relabel("a")("_")         ==== t4         --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d")]
    T ~ l4.relabel("b")("_")         ==== t4         --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d")]
    T ~ l4.relabel("c")("_")         ==== t4         --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d")]
    T ~ l4.relabel("d")("_")         ==== t4         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_")]
    T ~ l4.revalue("a")(0)           ==== t4._1to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d")]
    T ~ l4.revalue("b")(0)           ==== t4._2to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d")]
    T ~ l4.revalue("c")(0)           ==== t4._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d")]
    T ~ l4.revalue("d")(0)           ==== t4._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d")]
    T ~ l4.redo("a")(0 \ "_")        ==== t4._1to(0) --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d")]
    T ~ l4.redo("b")(0 \ "_")        ==== t4._2to(0) --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d")]
    T ~ l4.redo("c")(0 \ "_")        ==== t4._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d")]
    T ~ l4.redo("d")(0 \ "_")        ==== t4._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_")]

    val x12 = (1 \ "a", 2 \ "a", 3 \ "c", 4 \ "d")
    val x13 = (1 \ "a", 2 \ "b", 3 \ "a", 4 \ "d")
    val x14 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "a")
    val x23 = (1 \ "a", 2 \ "b", 3 \ "b", 4 \ "d")
    val x24 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "b")
    val x34 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "c")
    T ~ cc("""t4.label["a", "a", "c", "d"]""") ==== false
    T ~ cc("""t4.label["a", "b", "a", "d"]""") ==== false
    T ~ cc("""t4.label["a", "b", "c", "a"]""") ==== false
    T ~ cc("""t4.label["a", "b", "b", "d"]""") ==== false
    T ~ cc("""t4.label["a", "b", "c", "b"]""") ==== false
    T ~ cc("""t4.label["a", "b", "c", "c"]""") ==== false
    T ~ cc("""t4 \\ ("a", "a", "c", "d")""")   ==== false
    T ~ cc("""t4 \\ ("a", "b", "a", "d")""")   ==== false
    T ~ cc("""t4 \\ ("a", "b", "c", "a")""")   ==== false
    T ~ cc("""t4 \\ ("a", "b", "b", "d")""")   ==== false
    T ~ cc("""t4 \\ ("a", "b", "c", "b")""")   ==== false
    T ~ cc("""t4 \\ ("a", "b", "c", "b")""")   ==== false
    T ~ cc("""l4 ~~ ("_", "b", "c", "d")""")   ==== false
    T ~ cc("""l4 ~~ ("a", "_", "c", "d")""")   ==== false
    T ~ cc("""l4 ~~ ("a", "b", "_", "d")""")   ==== false
    T ~ cc("""l4 ~~ ("a", "b", "c", "_")""")   ==== false

    T ~ cc("""l4 ~ "_"""")              ==== false
    T ~ cc("""l4.relabel("_")("=")""")  ==== false
    T ~ cc("""l4.revalue("_")(0)""")    ==== false
    T ~ cc("""l4.redo("_")(0 \ "=")""") ==== false

    T ~ cc("""x12 ~ "a"""")              ==== false
    T ~ cc("""x12 ~ "c"""")              ==== true
    T ~ cc("""x12 ~ "d"""")              ==== true
    T ~ cc("""x13 ~ "a"""")              ==== false
    T ~ cc("""x13 ~ "b"""")              ==== true
    T ~ cc("""x13 ~ "d"""")              ==== true
    T ~ cc("""x14 ~ "a"""")              ==== false
    T ~ cc("""x14 ~ "b"""")              ==== true
    T ~ cc("""x14 ~ "c"""")              ==== true
    T ~ cc("""x23 ~ "b"""")              ==== false
    T ~ cc("""x23 ~ "a"""")              ==== true
    T ~ cc("""x23 ~ "d"""")              ==== true
    T ~ cc("""x24 ~ "b"""")              ==== false
    T ~ cc("""x24 ~ "a"""")              ==== true
    T ~ cc("""x24 ~ "c"""")              ==== true
    T ~ cc("""x34 ~ "c"""")              ==== false
    T ~ cc("""x34 ~ "a"""")              ==== true
    T ~ cc("""x34 ~ "b"""")              ==== true
    T ~ cc("""x12().a""")                ==== false
    T ~ cc("""x13().a""")                ==== false
    T ~ cc("""x14().a""")                ==== false
    T ~ cc("""x23().b""")                ==== false
    T ~ cc("""x24().b""")                ==== false
    T ~ cc("""x34().c""")                ==== false
    T ~ cc("""x12.relabel("a")("=")""")  ==== false
    T ~ cc("""x12.relabel("c")("=")""")  ==== true
    T ~ cc("""x12.relabel("d")("=")""")  ==== true
    T ~ cc("""x13.relabel("a")("=")""")  ==== false
    T ~ cc("""x13.relabel("b")("=")""")  ==== true
    T ~ cc("""x13.relabel("d")("=")""")  ==== true
    T ~ cc("""x14.relabel("a")("=")""")  ==== false
    T ~ cc("""x14.relabel("b")("=")""")  ==== true
    T ~ cc("""x14.relabel("c")("=")""")  ==== true
    T ~ cc("""x23.relabel("b")("=")""")  ==== false
    T ~ cc("""x23.relabel("a")("=")""")  ==== true
    T ~ cc("""x23.relabel("d")("=")""")  ==== true
    T ~ cc("""x24.relabel("b")("=")""")  ==== false
    T ~ cc("""x24.relabel("a")("=")""")  ==== true
    T ~ cc("""x24.relabel("c")("=")""")  ==== true
    T ~ cc("""x34.relabel("c")("=")""")  ==== false
    T ~ cc("""x34.relabel("a")("=")""")  ==== true
    T ~ cc("""x34.relabel("b")("=")""")  ==== true
    T ~ cc("""x12.revalue("a")(0)""")    ==== false
    T ~ cc("""x12.revalue("c")(0)""")    ==== true
    T ~ cc("""x12.revalue("d")(0)""")    ==== true
    T ~ cc("""x13.revalue("a")(0)""")    ==== false
    T ~ cc("""x13.revalue("b")(0)""")    ==== true
    T ~ cc("""x13.revalue("d")(0)""")    ==== true
    T ~ cc("""x14.revalue("a")(0)""")    ==== false
    T ~ cc("""x14.revalue("b")(0)""")    ==== true
    T ~ cc("""x14.revalue("c")(0)""")    ==== true
    T ~ cc("""x23.revalue("b")(0)""")    ==== false
    T ~ cc("""x23.revalue("a")(0)""")    ==== true
    T ~ cc("""x23.revalue("d")(0)""")    ==== true
    T ~ cc("""x24.revalue("b")(0)""")    ==== false
    T ~ cc("""x24.revalue("a")(0)""")    ==== true
    T ~ cc("""x24.revalue("c")(0)""")    ==== true
    T ~ cc("""x34.revalue("c")(0)""")    ==== false
    T ~ cc("""x34.revalue("a")(0)""")    ==== true
    T ~ cc("""x34.revalue("b")(0)""")    ==== true
    T ~ cc("""x12.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x12.redo("c")(0 \ "=")""") ==== true
    T ~ cc("""x12.redo("d")(0 \ "=")""") ==== true
    T ~ cc("""x13.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x13.redo("b")(0 \ "=")""") ==== true
    T ~ cc("""x13.redo("d")(0 \ "=")""") ==== true
    T ~ cc("""x14.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x14.redo("b")(0 \ "=")""") ==== true
    T ~ cc("""x14.redo("c")(0 \ "=")""") ==== true
    T ~ cc("""x23.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x23.redo("a")(0 \ "=")""") ==== true
    T ~ cc("""x23.redo("d")(0 \ "=")""") ==== true
    T ~ cc("""x24.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x24.redo("a")(0 \ "=")""") ==== true
    T ~ cc("""x24.redo("c")(0 \ "=")""") ==== true
    T ~ cc("""x34.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x34.redo("a")(0 \ "=")""") ==== true
    T ~ cc("""x34.redo("b")(0 \ "=")""") ==== true

    T ~ cc("""l4.relabel("a")("b")""")  ==== false
    T ~ cc("""l4.relabel("a")("c")""")  ==== false
    T ~ cc("""l4.relabel("a")("d")""")  ==== false
    T ~ cc("""l4.relabel("b")("a")""")  ==== false
    T ~ cc("""l4.relabel("b")("c")""")  ==== false
    T ~ cc("""l4.relabel("b")("d")""")  ==== false
    T ~ cc("""l4.relabel("c")("a")""")  ==== false
    T ~ cc("""l4.relabel("c")("b")""")  ==== false
    T ~ cc("""l4.relabel("c")("d")""")  ==== false
    T ~ cc("""l4.relabel("d")("a")""")  ==== false
    T ~ cc("""l4.relabel("d")("b")""")  ==== false
    T ~ cc("""l4.relabel("d")("c")""")  ==== false
    T ~ cc("""l4.redo("a")(0 \ "b")""") ==== false
    T ~ cc("""l4.redo("a")(0 \ "c")""") ==== false
    T ~ cc("""l4.redo("a")(0 \ "d")""") ==== false
    T ~ cc("""l4.redo("b")(0 \ "a")""") ==== false
    T ~ cc("""l4.redo("b")(0 \ "c")""") ==== false
    T ~ cc("""l4.redo("b")(0 \ "d")""") ==== false
    T ~ cc("""l4.redo("c")(0 \ "a")""") ==== false
    T ~ cc("""l4.redo("c")(0 \ "b")""") ==== false
    T ~ cc("""l4.redo("c")(0 \ "d")""") ==== false
    T ~ cc("""l4.redo("d")(0 \ "a")""") ==== false
    T ~ cc("""l4.redo("d")(0 \ "b")""") ==== false
    T ~ cc("""l4.redo("d")(0 \ "c")""") ==== false

  def labelledQuintuplet(): Unit =
    val t5 = (1, 2, 3, 4, 5)
    val l5 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e")
    T ~ l5                                ==== t5         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e")]
    T ~ t5.label["a", "b", "c", "d", "e"] ==== t5         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e")]
    T ~ (t5 \\ ("a", "b", "c", "d", "e")) ==== t5         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e")]
    T ~ (l5 ~ "a")                        ==== 1          --: typed[Int]
    T ~ (l5 ~ "b")                        ==== 2          --: typed[Int]
    T ~ (l5 ~ "c")                        ==== 3          --: typed[Int]
    T ~ (l5 ~ "d")                        ==== 4          --: typed[Int]
    T ~ (l5 ~ "e")                        ==== 5          --: typed[Int]
    T ~ l5.unlabel                        ==== t5         --: typed[(Int, Int, Int, Int, Int)]
    T ~ l5.label_1                        ==== "a"
    T ~ l5.label_2                        ==== "b"
    T ~ l5.label_3                        ==== "c"
    T ~ l5.label_4                        ==== "d"
    T ~ l5.label_5                        ==== "e"
    T ~ l5.labels                         ==== ("a", "b", "c", "d", "e")
    T ~ l5().a                            ==== 1          --: typed[Int]
    T ~ l5().b                            ==== 2          --: typed[Int]
    T ~ l5().c                            ==== 3          --: typed[Int]
    T ~ l5().d                            ==== 4          --: typed[Int]
    T ~ l5().e                            ==== 5          --: typed[Int]
    T ~ (l5 ~~ ("a", "b", "c", "d", "e")) ==== t5         --: typed[(Int, Int, Int, Int, Int)]
    T ~ l5.relabel("a")("_")              ==== t5         --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d", Int \ "e")]
    T ~ l5.relabel("b")("_")              ==== t5         --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d", Int \ "e")]
    T ~ l5.relabel("c")("_")              ==== t5         --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d", Int \ "e")]
    T ~ l5.relabel("d")("_")              ==== t5         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_", Int \ "e")]
    T ~ l5.relabel("e")("_")              ==== t5         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "_")]
    T ~ l5.revalue("a")(0)                ==== t5._1to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e")]
    T ~ l5.revalue("b")(0)                ==== t5._2to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e")]
    T ~ l5.revalue("c")(0)                ==== t5._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e")]
    T ~ l5.revalue("d")(0)                ==== t5._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e")]
    T ~ l5.revalue("e")(0)                ==== t5._5to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e")]
    T ~ l5.redo("a")(0 \ "_")             ==== t5._1to(0) --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d", Int \ "e")]
    T ~ l5.redo("b")(0 \ "_")             ==== t5._2to(0) --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d", Int \ "e")]
    T ~ l5.redo("c")(0 \ "_")             ==== t5._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d", Int \ "e")]
    T ~ l5.redo("d")(0 \ "_")             ==== t5._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_", Int \ "e")]
    T ~ l5.redo("e")(0 \ "_")             ==== t5._5to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "_")]

    val x12 = (1 \ "a", 2 \ "a", 3 \ "c", 4 \ "d", 5 \ "e")
    val x13 = (1 \ "a", 2 \ "b", 3 \ "a", 4 \ "d", 5 \ "e")
    val x14 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "a", 5 \ "e")
    val x15 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "a")
    val x23 = (1 \ "a", 2 \ "b", 3 \ "b", 4 \ "d", 5 \ "e")
    val x24 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "b", 5 \ "e")
    val x25 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "b")
    val x34 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "c", 5 \ "e")
    val x35 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "c")
    val x45 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "d")
    T ~ cc("""t5.label["a", "a", "c", "d", "e"]""") ==== false
    T ~ cc("""t5.label["a", "b", "a", "d", "e"]""") ==== false
    T ~ cc("""t5.label["a", "b", "c", "a", "e"]""") ==== false
    T ~ cc("""t5.label["a", "b", "c", "d", "a"]""") ==== false
    T ~ cc("""t5.label["a", "b", "b", "d", "e"]""") ==== false
    T ~ cc("""t5.label["a", "b", "c", "b", "e"]""") ==== false
    T ~ cc("""t5.label["a", "b", "c", "d", "b"]""") ==== false
    T ~ cc("""t5.label["a", "b", "c", "c", "e"]""") ==== false
    T ~ cc("""t5.label["a", "b", "c", "d", "c"]""") ==== false
    T ~ cc("""t5.label["a", "b", "c", "d", "d"]""") ==== false
    T ~ cc("""t5 \\ ("a", "a", "c", "d", "e")""")   ==== false
    T ~ cc("""t5 \\ ("a", "b", "a", "d", "e")""")   ==== false
    T ~ cc("""t5 \\ ("a", "b", "c", "a", "e")""")   ==== false
    T ~ cc("""t5 \\ ("a", "b", "c", "d", "a")""")   ==== false
    T ~ cc("""t5 \\ ("a", "b", "b", "d", "e")""")   ==== false
    T ~ cc("""t5 \\ ("a", "b", "c", "b", "e")""")   ==== false
    T ~ cc("""t5 \\ ("a", "b", "c", "d", "b")""")   ==== false
    T ~ cc("""t5 \\ ("a", "b", "c", "c", "e")""")   ==== false
    T ~ cc("""t5 \\ ("a", "b", "c", "d", "c")""")   ==== false
    T ~ cc("""t5 \\ ("a", "b", "c", "d", "d")""")   ==== false
    T ~ cc("""l5 ~~ ("_", "b", "c", "d", "e")""")   ==== false
    T ~ cc("""l5 ~~ ("a", "_", "c", "d", "e")""")   ==== false
    T ~ cc("""l5 ~~ ("a", "b", "_", "d", "e")""")   ==== false
    T ~ cc("""l5 ~~ ("a", "b", "c", "_", "e")""")   ==== false
    T ~ cc("""l5 ~~ ("a", "b", "c", "d", "_")""")   ==== false

    T ~ cc("""l5 ~ "_"""")              ==== false
    T ~ cc("""l5.relabel("_")("=")""")  ==== false
    T ~ cc("""l5.revalue("_")(0)""")    ==== false
    T ~ cc("""l5.redo("_")(0 \ "=")""") ==== false

    T ~ cc("""x12 ~ "a"""")              ==== false
    T ~ cc("""x12 ~ "c"""")              ==== true
    T ~ cc("""x12 ~ "d"""")              ==== true
    T ~ cc("""x12 ~ "e"""")              ==== true
    T ~ cc("""x13 ~ "a"""")              ==== false
    T ~ cc("""x13 ~ "b"""")              ==== true
    T ~ cc("""x13 ~ "d"""")              ==== true
    T ~ cc("""x13 ~ "e"""")              ==== true
    T ~ cc("""x14 ~ "a"""")              ==== false
    T ~ cc("""x14 ~ "b"""")              ==== true
    T ~ cc("""x14 ~ "c"""")              ==== true
    T ~ cc("""x14 ~ "e"""")              ==== true
    T ~ cc("""x15 ~ "a"""")              ==== false
    T ~ cc("""x15 ~ "b"""")              ==== true
    T ~ cc("""x15 ~ "c"""")              ==== true
    T ~ cc("""x15 ~ "d"""")              ==== true
    T ~ cc("""x23 ~ "b"""")              ==== false
    T ~ cc("""x23 ~ "a"""")              ==== true
    T ~ cc("""x23 ~ "d"""")              ==== true
    T ~ cc("""x23 ~ "e"""")              ==== true
    T ~ cc("""x24 ~ "b"""")              ==== false
    T ~ cc("""x24 ~ "a"""")              ==== true
    T ~ cc("""x24 ~ "c"""")              ==== true
    T ~ cc("""x24 ~ "e"""")              ==== true
    T ~ cc("""x25 ~ "b"""")              ==== false
    T ~ cc("""x25 ~ "a"""")              ==== true
    T ~ cc("""x25 ~ "c"""")              ==== true
    T ~ cc("""x25 ~ "d"""")              ==== true
    T ~ cc("""x34 ~ "c"""")              ==== false
    T ~ cc("""x34 ~ "a"""")              ==== true
    T ~ cc("""x34 ~ "b"""")              ==== true
    T ~ cc("""x34 ~ "e"""")              ==== true
    T ~ cc("""x35 ~ "c"""")              ==== false
    T ~ cc("""x35 ~ "a"""")              ==== true
    T ~ cc("""x35 ~ "b"""")              ==== true
    T ~ cc("""x35 ~ "d"""")              ==== true
    T ~ cc("""x45 ~ "d"""")              ==== false
    T ~ cc("""x45 ~ "a"""")              ==== true
    T ~ cc("""x45 ~ "b"""")              ==== true
    T ~ cc("""x45 ~ "c"""")              ==== true

    T ~ cc("""x12().a""")                ==== false
    T ~ cc("""x13().a""")                ==== false
    T ~ cc("""x14().a""")                ==== false
    T ~ cc("""x15().a""")                ==== false
    T ~ cc("""x23().b""")                ==== false
    T ~ cc("""x24().b""")                ==== false
    T ~ cc("""x25().b""")                ==== false
    T ~ cc("""x34().c""")                ==== false
    T ~ cc("""x35().c""")                ==== false
    T ~ cc("""x45().d""")                ==== false
    T ~ cc("""x12.relabel("a")("=")""")  ==== false
    T ~ cc("""x12.relabel("c")("=")""")  ==== true
    T ~ cc("""x12.relabel("d")("=")""")  ==== true
    T ~ cc("""x12.relabel("e")("=")""")  ==== true
    T ~ cc("""x13.relabel("a")("=")""")  ==== false
    T ~ cc("""x13.relabel("b")("=")""")  ==== true
    T ~ cc("""x13.relabel("d")("=")""")  ==== true
    T ~ cc("""x13.relabel("e")("=")""")  ==== true
    T ~ cc("""x14.relabel("a")("=")""")  ==== false
    T ~ cc("""x14.relabel("b")("=")""")  ==== true
    T ~ cc("""x14.relabel("c")("=")""")  ==== true
    T ~ cc("""x14.relabel("e")("=")""")  ==== true
    T ~ cc("""x15.relabel("a")("=")""")  ==== false
    T ~ cc("""x15.relabel("b")("=")""")  ==== true
    T ~ cc("""x15.relabel("c")("=")""")  ==== true
    T ~ cc("""x15.relabel("d")("=")""")  ==== true
    T ~ cc("""x23.relabel("b")("=")""")  ==== false
    T ~ cc("""x23.relabel("a")("=")""")  ==== true
    T ~ cc("""x23.relabel("d")("=")""")  ==== true
    T ~ cc("""x23.relabel("e")("=")""")  ==== true
    T ~ cc("""x24.relabel("b")("=")""")  ==== false
    T ~ cc("""x24.relabel("a")("=")""")  ==== true
    T ~ cc("""x24.relabel("c")("=")""")  ==== true
    T ~ cc("""x24.relabel("e")("=")""")  ==== true
    T ~ cc("""x25.relabel("b")("=")""")  ==== false
    T ~ cc("""x25.relabel("a")("=")""")  ==== true
    T ~ cc("""x25.relabel("c")("=")""")  ==== true
    T ~ cc("""x25.relabel("d")("=")""")  ==== true
    T ~ cc("""x34.relabel("c")("=")""")  ==== false
    T ~ cc("""x34.relabel("a")("=")""")  ==== true
    T ~ cc("""x34.relabel("b")("=")""")  ==== true
    T ~ cc("""x34.relabel("e")("=")""")  ==== true
    T ~ cc("""x35.relabel("c")("=")""")  ==== false
    T ~ cc("""x35.relabel("a")("=")""")  ==== true
    T ~ cc("""x35.relabel("b")("=")""")  ==== true
    T ~ cc("""x35.relabel("d")("=")""")  ==== true
    T ~ cc("""x45.relabel("d")("=")""")  ==== false
    T ~ cc("""x45.relabel("a")("=")""")  ==== true
    T ~ cc("""x45.relabel("b")("=")""")  ==== true
    T ~ cc("""x45.relabel("c")("=")""")  ==== true
    T ~ cc("""x12.revalue("a")(0)""")    ==== false
    T ~ cc("""x12.revalue("c")(0)""")    ==== true
    T ~ cc("""x12.revalue("d")(0)""")    ==== true
    T ~ cc("""x12.revalue("e")(0)""")    ==== true
    T ~ cc("""x13.revalue("a")(0)""")    ==== false
    T ~ cc("""x13.revalue("b")(0)""")    ==== true
    T ~ cc("""x13.revalue("d")(0)""")    ==== true
    T ~ cc("""x13.revalue("e")(0)""")    ==== true
    T ~ cc("""x14.revalue("a")(0)""")    ==== false
    T ~ cc("""x14.revalue("b")(0)""")    ==== true
    T ~ cc("""x14.revalue("c")(0)""")    ==== true
    T ~ cc("""x14.revalue("e")(0)""")    ==== true
    T ~ cc("""x15.revalue("a")(0)""")    ==== false
    T ~ cc("""x15.revalue("b")(0)""")    ==== true
    T ~ cc("""x15.revalue("c")(0)""")    ==== true
    T ~ cc("""x15.revalue("d")(0)""")    ==== true
    T ~ cc("""x23.revalue("b")(0)""")    ==== false
    T ~ cc("""x23.revalue("a")(0)""")    ==== true
    T ~ cc("""x23.revalue("d")(0)""")    ==== true
    T ~ cc("""x23.revalue("e")(0)""")    ==== true
    T ~ cc("""x24.revalue("b")(0)""")    ==== false
    T ~ cc("""x24.revalue("a")(0)""")    ==== true
    T ~ cc("""x24.revalue("c")(0)""")    ==== true
    T ~ cc("""x24.revalue("e")(0)""")    ==== true
    T ~ cc("""x25.revalue("b")(0)""")    ==== false
    T ~ cc("""x25.revalue("a")(0)""")    ==== true
    T ~ cc("""x25.revalue("c")(0)""")    ==== true
    T ~ cc("""x25.revalue("d")(0)""")    ==== true
    T ~ cc("""x34.revalue("c")(0)""")    ==== false
    T ~ cc("""x34.revalue("a")(0)""")    ==== true
    T ~ cc("""x34.revalue("b")(0)""")    ==== true
    T ~ cc("""x34.revalue("e")(0)""")    ==== true
    T ~ cc("""x35.revalue("c")(0)""")    ==== false
    T ~ cc("""x35.revalue("a")(0)""")    ==== true
    T ~ cc("""x35.revalue("b")(0)""")    ==== true
    T ~ cc("""x35.revalue("d")(0)""")    ==== true
    T ~ cc("""x45.revalue("d")(0)""")    ==== false
    T ~ cc("""x45.revalue("a")(0)""")    ==== true
    T ~ cc("""x45.revalue("b")(0)""")    ==== true
    T ~ cc("""x45.revalue("c")(0)""")    ==== true
    T ~ cc("""x12.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x12.redo("c")(0 \ "=")""") ==== true
    T ~ cc("""x12.redo("d")(0 \ "=")""") ==== true
    T ~ cc("""x12.redo("e")(0 \ "=")""") ==== true
    T ~ cc("""x13.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x13.redo("b")(0 \ "=")""") ==== true
    T ~ cc("""x13.redo("d")(0 \ "=")""") ==== true
    T ~ cc("""x13.redo("e")(0 \ "=")""") ==== true
    T ~ cc("""x14.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x14.redo("b")(0 \ "=")""") ==== true
    T ~ cc("""x14.redo("c")(0 \ "=")""") ==== true
    T ~ cc("""x14.redo("e")(0 \ "=")""") ==== true
    T ~ cc("""x15.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x15.redo("b")(0 \ "=")""") ==== true
    T ~ cc("""x15.redo("c")(0 \ "=")""") ==== true
    T ~ cc("""x15.redo("d")(0 \ "=")""") ==== true
    T ~ cc("""x23.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x23.redo("a")(0 \ "=")""") ==== true
    T ~ cc("""x23.redo("d")(0 \ "=")""") ==== true
    T ~ cc("""x23.redo("e")(0 \ "=")""") ==== true
    T ~ cc("""x24.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x24.redo("a")(0 \ "=")""") ==== true
    T ~ cc("""x24.redo("c")(0 \ "=")""") ==== true
    T ~ cc("""x24.redo("e")(0 \ "=")""") ==== true
    T ~ cc("""x25.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x25.redo("a")(0 \ "=")""") ==== true
    T ~ cc("""x25.redo("c")(0 \ "=")""") ==== true
    T ~ cc("""x25.redo("d")(0 \ "=")""") ==== true
    T ~ cc("""x34.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x34.redo("a")(0 \ "=")""") ==== true
    T ~ cc("""x34.redo("b")(0 \ "=")""") ==== true
    T ~ cc("""x34.redo("e")(0 \ "=")""") ==== true
    T ~ cc("""x35.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x35.redo("a")(0 \ "=")""") ==== true
    T ~ cc("""x35.redo("b")(0 \ "=")""") ==== true
    T ~ cc("""x35.redo("d")(0 \ "=")""") ==== true
    T ~ cc("""x45.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x45.redo("a")(0 \ "=")""") ==== true
    T ~ cc("""x45.redo("b")(0 \ "=")""") ==== true
    T ~ cc("""x45.redo("c")(0 \ "=")""") ==== true

    T ~ cc("""l5.relabel("a")("b")""")  ==== false
    T ~ cc("""l5.relabel("a")("c")""")  ==== false
    T ~ cc("""l5.relabel("a")("d")""")  ==== false
    T ~ cc("""l5.relabel("a")("e")""")  ==== false
    T ~ cc("""l5.relabel("b")("a")""")  ==== false
    T ~ cc("""l5.relabel("b")("c")""")  ==== false
    T ~ cc("""l5.relabel("b")("d")""")  ==== false
    T ~ cc("""l5.relabel("b")("e")""")  ==== false
    T ~ cc("""l5.relabel("c")("a")""")  ==== false
    T ~ cc("""l5.relabel("c")("b")""")  ==== false
    T ~ cc("""l5.relabel("c")("d")""")  ==== false
    T ~ cc("""l5.relabel("c")("e")""")  ==== false
    T ~ cc("""l5.relabel("d")("a")""")  ==== false
    T ~ cc("""l5.relabel("d")("b")""")  ==== false
    T ~ cc("""l5.relabel("d")("c")""")  ==== false
    T ~ cc("""l5.relabel("d")("e")""")  ==== false
    T ~ cc("""l5.relabel("e")("a")""")  ==== false
    T ~ cc("""l5.relabel("e")("b")""")  ==== false
    T ~ cc("""l5.relabel("e")("c")""")  ==== false
    T ~ cc("""l5.relabel("e")("d")""")  ==== false
    T ~ cc("""l5.redo("a")(0 \ "b")""") ==== false
    T ~ cc("""l5.redo("a")(0 \ "c")""") ==== false
    T ~ cc("""l5.redo("a")(0 \ "d")""") ==== false
    T ~ cc("""l5.redo("a")(0 \ "e")""") ==== false
    T ~ cc("""l5.redo("b")(0 \ "a")""") ==== false
    T ~ cc("""l5.redo("b")(0 \ "c")""") ==== false
    T ~ cc("""l5.redo("b")(0 \ "d")""") ==== false
    T ~ cc("""l5.redo("b")(0 \ "e")""") ==== false
    T ~ cc("""l5.redo("c")(0 \ "a")""") ==== false
    T ~ cc("""l5.redo("c")(0 \ "b")""") ==== false
    T ~ cc("""l5.redo("c")(0 \ "d")""") ==== false
    T ~ cc("""l5.redo("c")(0 \ "e")""") ==== false
    T ~ cc("""l5.redo("d")(0 \ "a")""") ==== false
    T ~ cc("""l5.redo("d")(0 \ "b")""") ==== false
    T ~ cc("""l5.redo("d")(0 \ "c")""") ==== false
    T ~ cc("""l5.redo("d")(0 \ "e")""") ==== false
    T ~ cc("""l5.redo("e")(0 \ "a")""") ==== false
    T ~ cc("""l5.redo("e")(0 \ "b")""") ==== false
    T ~ cc("""l5.redo("e")(0 \ "c")""") ==== false
    T ~ cc("""l5.redo("e")(0 \ "d")""") ==== false


  def labelledSextuplet(): Unit =
    val t6 = (1, 2, 3, 4, 5, 6)
    val l6 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f")
    T ~ l6                                     ==== t6         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ t6.label["a", "b", "c", "d", "e", "f"] ==== t6         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ (t6 \\ ("a", "b", "c", "d", "e", "f")) ==== t6         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ (l6 ~ "a")                             ==== 1          --: typed[Int]
    T ~ (l6 ~ "b")                             ==== 2          --: typed[Int]
    T ~ (l6 ~ "c")                             ==== 3          --: typed[Int]
    T ~ (l6 ~ "d")                             ==== 4          --: typed[Int]
    T ~ (l6 ~ "e")                             ==== 5          --: typed[Int]
    T ~ (l6 ~ "f")                             ==== 6          --: typed[Int]
    T ~ l6.unlabel                             ==== t6         --: typed[(Int, Int, Int, Int, Int, Int)]
    T ~ l6.label_1                             ==== "a"
    T ~ l6.label_2                             ==== "b"
    T ~ l6.label_3                             ==== "c"
    T ~ l6.label_4                             ==== "d"
    T ~ l6.label_5                             ==== "e"
    T ~ l6.label_6                             ==== "f"
    T ~ l6.labels                              ==== ("a", "b", "c", "d", "e", "f")
    T ~ l6().a                                 ==== 1          --: typed[Int]
    T ~ l6().b                                 ==== 2          --: typed[Int]
    T ~ l6().c                                 ==== 3          --: typed[Int]
    T ~ l6().d                                 ==== 4          --: typed[Int]
    T ~ l6().e                                 ==== 5          --: typed[Int]
    T ~ l6().f                                 ==== 6          --: typed[Int]
    T ~ (l6 ~~ ("a", "b", "c", "d", "e", "f")) ==== t6         --: typed[(Int, Int, Int, Int, Int, Int)]
    T ~ l6.relabel("a")("_")                   ==== t6         --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.relabel("b")("_")                   ==== t6         --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.relabel("c")("_")                   ==== t6         --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.relabel("d")("_")                   ==== t6         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_", Int \ "e", Int \ "f")]
    T ~ l6.relabel("e")("_")                   ==== t6         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "_", Int \ "f")]
    T ~ l6.relabel("f")("_")                   ==== t6         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "_")]
    T ~ l6.revalue("a")(0)                     ==== t6._1to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.revalue("b")(0)                     ==== t6._2to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.revalue("c")(0)                     ==== t6._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.revalue("d")(0)                     ==== t6._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.revalue("e")(0)                     ==== t6._5to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.revalue("f")(0)                     ==== t6._6to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.redo("a")(0 \ "_")                  ==== t6._1to(0) --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.redo("b")(0 \ "_")                  ==== t6._2to(0) --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.redo("c")(0 \ "_")                  ==== t6._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d", Int \ "e", Int \ "f")]
    T ~ l6.redo("d")(0 \ "_")                  ==== t6._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_", Int \ "e", Int \ "f")]
    T ~ l6.redo("e")(0 \ "_")                  ==== t6._5to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "_", Int \ "f")]
    T ~ l6.redo("f")(0 \ "_")                  ==== t6._6to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "_")]

    val x12 = (1 \ "a", 2 \ "a", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f")
    val x13 = (1 \ "a", 2 \ "b", 3 \ "a", 4 \ "d", 5 \ "e", 6 \ "f")
    val x14 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "a", 5 \ "e", 6 \ "f")
    val x15 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "a", 6 \ "f")
    val x16 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "a")
    val x23 = (1 \ "a", 2 \ "b", 3 \ "b", 4 \ "d", 5 \ "e", 6 \ "f")
    val x24 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "b", 5 \ "e", 6 \ "f")
    val x25 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "b", 6 \ "f")
    val x26 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "b")
    val x34 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "c", 5 \ "e", 6 \ "f")
    val x35 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "c", 6 \ "f")
    val x36 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "c")
    val x45 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "d", 6 \ "f")
    val x46 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "d")
    val x56 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "e")
    T ~ cc("""t6.label["a", "a", "c", "d", "e", "f"]""") ==== false
    T ~ cc("""t6.label["a", "b", "a", "d", "e", "f"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "a", "e", "f"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "d", "a", "f"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "d", "e", "a"]""") ==== false
    T ~ cc("""t6.label["a", "b", "b", "d", "e", "f"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "b", "e", "f"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "d", "b", "f"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "d", "e", "b"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "c", "e", "f"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "d", "c", "f"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "d", "e", "c"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "d", "d", "f"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "d", "e", "d"]""") ==== false
    T ~ cc("""t6.label["a", "b", "c", "d", "e", "e"]""") ==== false
    T ~ cc("""t6 \\ ("a", "a", "c", "d", "e", "f")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "a", "d", "e", "f")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "a", "e", "f")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "d", "a", "f")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "d", "e", "a")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "b", "d", "e", "f")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "b", "e", "f")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "d", "b", "f")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "d", "e", "b")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "c", "e", "f")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "d", "c", "f")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "d", "e", "c")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "d", "d", "f")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "d", "e", "d")""")   ==== false
    T ~ cc("""t6 \\ ("a", "b", "c", "d", "e", "e")""")   ==== false
    T ~ cc("""l6 ~~ ("_", "b", "c", "d", "e", "f")""")   ==== false
    T ~ cc("""l6 ~~ ("a", "_", "c", "d", "e", "f")""")   ==== false
    T ~ cc("""l6 ~~ ("a", "b", "_", "d", "e", "f")""")   ==== false
    T ~ cc("""l6 ~~ ("a", "b", "c", "_", "e", "f")""")   ==== false
    T ~ cc("""l6 ~~ ("a", "b", "c", "d", "_", "f")""")   ==== false
    T ~ cc("""l6 ~~ ("a", "b", "c", "d", "e", "_")""")   ==== false

    T ~ cc("""l6 ~ "_"""")              ==== false
    T ~ cc("""l6.relabel("_")("=")""")  ==== false
    T ~ cc("""l6.revalue("_")(0)""")    ==== false
    T ~ cc("""l6.redo("_")(0 \ "=")""") ==== false

    T ~ cc("""x12 ~ "a"""")              ==== false
    T ~ cc("""x13 ~ "a"""")              ==== false
    T ~ cc("""x14 ~ "a"""")              ==== false
    T ~ cc("""x15 ~ "a"""")              ==== false
    T ~ cc("""x16 ~ "a"""")              ==== false
    T ~ cc("""x23 ~ "b"""")              ==== false
    T ~ cc("""x24 ~ "b"""")              ==== false
    T ~ cc("""x25 ~ "b"""")              ==== false
    T ~ cc("""x26 ~ "b"""")              ==== false
    T ~ cc("""x34 ~ "c"""")              ==== false
    T ~ cc("""x35 ~ "c"""")              ==== false
    T ~ cc("""x36 ~ "c"""")              ==== false
    T ~ cc("""x45 ~ "d"""")              ==== false
    T ~ cc("""x46 ~ "d"""")              ==== false
    T ~ cc("""x56 ~ "e"""")              ==== false
    T ~ cc("""x12().a""")                 ==== false
    T ~ cc("""x13().a""")                 ==== false
    T ~ cc("""x14().a""")                 ==== false
    T ~ cc("""x15().a""")                 ==== false
    T ~ cc("""x16().a""")                 ==== false
    T ~ cc("""x23().b""")                 ==== false
    T ~ cc("""x24().b""")                 ==== false
    T ~ cc("""x25().b""")                 ==== false
    T ~ cc("""x26().b""")                 ==== false
    T ~ cc("""x34().c""")                 ==== false
    T ~ cc("""x35().c""")                 ==== false
    T ~ cc("""x36().c""")                 ==== false
    T ~ cc("""x45().d""")                 ==== false
    T ~ cc("""x46().d""")                 ==== false
    T ~ cc("""x56().e""")                 ==== false
    T ~ cc("""x12.relabel("a")("=")""")  ==== false
    T ~ cc("""x13.relabel("a")("=")""")  ==== false
    T ~ cc("""x14.relabel("a")("=")""")  ==== false
    T ~ cc("""x15.relabel("a")("=")""")  ==== false
    T ~ cc("""x16.relabel("a")("=")""")  ==== false
    T ~ cc("""x23.relabel("b")("=")""")  ==== false
    T ~ cc("""x24.relabel("b")("=")""")  ==== false
    T ~ cc("""x25.relabel("b")("=")""")  ==== false
    T ~ cc("""x26.relabel("b")("=")""")  ==== false
    T ~ cc("""x34.relabel("c")("=")""")  ==== false
    T ~ cc("""x35.relabel("c")("=")""")  ==== false
    T ~ cc("""x36.relabel("c")("=")""")  ==== false
    T ~ cc("""x45.relabel("d")("=")""")  ==== false
    T ~ cc("""x46.relabel("d")("=")""")  ==== false
    T ~ cc("""x56.relabel("e")("=")""")  ==== false
    T ~ cc("""x12.revalue("a")(0)""")    ==== false
    T ~ cc("""x13.revalue("a")(0)""")    ==== false
    T ~ cc("""x14.revalue("a")(0)""")    ==== false
    T ~ cc("""x15.revalue("a")(0)""")    ==== false
    T ~ cc("""x16.revalue("a")(0)""")    ==== false
    T ~ cc("""x23.revalue("b")(0)""")    ==== false
    T ~ cc("""x24.revalue("b")(0)""")    ==== false
    T ~ cc("""x25.revalue("b")(0)""")    ==== false
    T ~ cc("""x26.revalue("b")(0)""")    ==== false
    T ~ cc("""x34.revalue("c")(0)""")    ==== false
    T ~ cc("""x35.revalue("c")(0)""")    ==== false
    T ~ cc("""x36.revalue("c")(0)""")    ==== false
    T ~ cc("""x45.revalue("d")(0)""")    ==== false
    T ~ cc("""x46.revalue("d")(0)""")    ==== false
    T ~ cc("""x56.revalue("e")(0)""")    ==== false
    T ~ cc("""x12.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x13.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x14.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x15.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x16.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x23.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x24.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x25.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x26.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x34.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x35.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x36.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x45.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x46.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x56.redo("e")(0 \ "=")""") ==== false

    T ~ cc("""l6.relabel("a")("b")""")  ==== false
    T ~ cc("""l6.relabel("a")("c")""")  ==== false
    T ~ cc("""l6.relabel("a")("d")""")  ==== false
    T ~ cc("""l6.relabel("a")("e")""")  ==== false
    T ~ cc("""l6.relabel("a")("f")""")  ==== false
    T ~ cc("""l6.relabel("b")("a")""")  ==== false
    T ~ cc("""l6.relabel("b")("c")""")  ==== false
    T ~ cc("""l6.relabel("b")("d")""")  ==== false
    T ~ cc("""l6.relabel("b")("e")""")  ==== false
    T ~ cc("""l6.relabel("b")("f")""")  ==== false
    T ~ cc("""l6.relabel("c")("a")""")  ==== false
    T ~ cc("""l6.relabel("c")("b")""")  ==== false
    T ~ cc("""l6.relabel("c")("d")""")  ==== false
    T ~ cc("""l6.relabel("c")("e")""")  ==== false
    T ~ cc("""l6.relabel("c")("f")""")  ==== false
    T ~ cc("""l6.relabel("d")("a")""")  ==== false
    T ~ cc("""l6.relabel("d")("b")""")  ==== false
    T ~ cc("""l6.relabel("d")("c")""")  ==== false
    T ~ cc("""l6.relabel("d")("e")""")  ==== false
    T ~ cc("""l6.relabel("d")("f")""")  ==== false
    T ~ cc("""l6.relabel("e")("a")""")  ==== false
    T ~ cc("""l6.relabel("e")("b")""")  ==== false
    T ~ cc("""l6.relabel("e")("c")""")  ==== false
    T ~ cc("""l6.relabel("e")("d")""")  ==== false
    T ~ cc("""l6.relabel("e")("f")""")  ==== false
    T ~ cc("""l6.relabel("f")("a")""")  ==== false
    T ~ cc("""l6.relabel("f")("b")""")  ==== false
    T ~ cc("""l6.relabel("f")("c")""")  ==== false
    T ~ cc("""l6.relabel("f")("d")""")  ==== false
    T ~ cc("""l6.relabel("f")("e")""")  ==== false
    T ~ cc("""l6.redo("a")(0 \ "b")""") ==== false
    T ~ cc("""l6.redo("a")(0 \ "c")""") ==== false
    T ~ cc("""l6.redo("a")(0 \ "d")""") ==== false
    T ~ cc("""l6.redo("a")(0 \ "e")""") ==== false
    T ~ cc("""l6.redo("a")(0 \ "f")""") ==== false
    T ~ cc("""l6.redo("b")(0 \ "a")""") ==== false
    T ~ cc("""l6.redo("b")(0 \ "c")""") ==== false
    T ~ cc("""l6.redo("b")(0 \ "d")""") ==== false
    T ~ cc("""l6.redo("b")(0 \ "e")""") ==== false
    T ~ cc("""l6.redo("b")(0 \ "f")""") ==== false
    T ~ cc("""l6.redo("c")(0 \ "a")""") ==== false
    T ~ cc("""l6.redo("c")(0 \ "b")""") ==== false
    T ~ cc("""l6.redo("c")(0 \ "d")""") ==== false
    T ~ cc("""l6.redo("c")(0 \ "e")""") ==== false
    T ~ cc("""l6.redo("c")(0 \ "f")""") ==== false
    T ~ cc("""l6.redo("d")(0 \ "a")""") ==== false
    T ~ cc("""l6.redo("d")(0 \ "b")""") ==== false
    T ~ cc("""l6.redo("d")(0 \ "c")""") ==== false
    T ~ cc("""l6.redo("d")(0 \ "e")""") ==== false
    T ~ cc("""l6.redo("d")(0 \ "f")""") ==== false
    T ~ cc("""l6.redo("e")(0 \ "a")""") ==== false
    T ~ cc("""l6.redo("e")(0 \ "b")""") ==== false
    T ~ cc("""l6.redo("e")(0 \ "c")""") ==== false
    T ~ cc("""l6.redo("e")(0 \ "d")""") ==== false
    T ~ cc("""l6.redo("e")(0 \ "f")""") ==== false
    T ~ cc("""l6.redo("f")(0 \ "a")""") ==== false
    T ~ cc("""l6.redo("f")(0 \ "b")""") ==== false
    T ~ cc("""l6.redo("f")(0 \ "c")""") ==== false
    T ~ cc("""l6.redo("f")(0 \ "d")""") ==== false
    T ~ cc("""l6.redo("f")(0 \ "e")""") ==== false


  def labelledSeptuplet(): Unit =
    val t7 = (1, 2, 3, 4, 5, 6, 7)
    val l7 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g")
    T ~ l7                                          ==== t7         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ t7.label["a", "b", "c", "d", "e", "f", "g"] ==== t7         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ (t7 \\ ("a", "b", "c", "d", "e", "f", "g")) ==== t7         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ (l7 ~ "a")                                  ==== 1          --: typed[Int]
    T ~ (l7 ~ "b")                                  ==== 2          --: typed[Int]
    T ~ (l7 ~ "c")                                  ==== 3          --: typed[Int]
    T ~ (l7 ~ "d")                                  ==== 4          --: typed[Int]
    T ~ (l7 ~ "e")                                  ==== 5          --: typed[Int]
    T ~ (l7 ~ "f")                                  ==== 6          --: typed[Int]
    T ~ (l7 ~ "g")                                  ==== 7          --: typed[Int]
    T ~ l7.unlabel                                  ==== t7         --: typed[(Int, Int, Int, Int, Int, Int, Int)]
    T ~ l7.label_1                                  ==== "a"
    T ~ l7.label_2                                  ==== "b"
    T ~ l7.label_3                                  ==== "c"
    T ~ l7.label_4                                  ==== "d"
    T ~ l7.label_5                                  ==== "e"
    T ~ l7.label_6                                  ==== "f"
    T ~ l7.label_7                                  ==== "g"
    T ~ l7.labels                                   ==== ("a", "b", "c", "d", "e", "f", "g")
    T ~ l7().a                                      ==== 1          --: typed[Int]
    T ~ l7().b                                      ==== 2          --: typed[Int]
    T ~ l7().c                                      ==== 3          --: typed[Int]
    T ~ l7().d                                      ==== 4          --: typed[Int]
    T ~ l7().e                                      ==== 5          --: typed[Int]
    T ~ l7().f                                      ==== 6          --: typed[Int]
    T ~ l7().g                                      ==== 7          --: typed[Int]
    T ~ (l7 ~~ ("a", "b", "c", "d", "e", "f", "g")) ==== t7         --: typed[(Int, Int, Int, Int, Int, Int, Int)]
    T ~ l7.relabel("a")("_")                        ==== t7         --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.relabel("b")("_")                        ==== t7         --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.relabel("c")("_")                        ==== t7         --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.relabel("d")("_")                        ==== t7         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.relabel("e")("_")                        ==== t7         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "_", Int \ "f", Int \ "g")]
    T ~ l7.relabel("f")("_")                        ==== t7         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "_", Int \ "g")]
    T ~ l7.relabel("g")("_")                        ==== t7         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "_")]
    T ~ l7.revalue("a")(0)                          ==== t7._1to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.revalue("b")(0)                          ==== t7._2to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.revalue("c")(0)                          ==== t7._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.revalue("d")(0)                          ==== t7._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.revalue("e")(0)                          ==== t7._5to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.revalue("f")(0)                          ==== t7._6to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.revalue("g")(0)                          ==== t7._7to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.redo("a")(0 \ "_")                       ==== t7._1to(0) --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.redo("b")(0 \ "_")                       ==== t7._2to(0) --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.redo("c")(0 \ "_")                       ==== t7._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.redo("d")(0 \ "_")                       ==== t7._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_", Int \ "e", Int \ "f", Int \ "g")]
    T ~ l7.redo("e")(0 \ "_")                       ==== t7._5to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "_", Int \ "f", Int \ "g")]
    T ~ l7.redo("f")(0 \ "_")                       ==== t7._6to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "_", Int \ "g")]
    T ~ l7.redo("g")(0 \ "_")                       ==== t7._7to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "_")]

    val x12 = (1 \ "a", 2 \ "a", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g")
    val x13 = (1 \ "a", 2 \ "b", 3 \ "a", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g")
    val x14 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "a", 5 \ "e", 6 \ "f", 7 \ "g")
    val x15 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "a", 6 \ "f", 7 \ "g")
    val x16 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "a", 7 \ "g")
    val x17 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "a")
    val x23 = (1 \ "a", 2 \ "b", 3 \ "b", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g")
    val x24 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "b", 5 \ "e", 6 \ "f", 7 \ "g")
    val x25 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "b", 6 \ "f", 7 \ "g")
    val x26 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "b", 7 \ "g")
    val x27 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "b")
    val x34 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "c", 5 \ "e", 6 \ "f", 7 \ "g")
    val x35 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "c", 6 \ "f", 7 \ "g")
    val x36 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "c", 7 \ "g")
    val x37 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "c")
    val x45 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "d", 6 \ "f", 7 \ "g")
    val x46 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "d", 7 \ "g")
    val x47 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "d")
    val x56 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "e", 7 \ "g")
    val x57 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "e")
    val x67 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "f")
    T ~ cc("""t7.label["a", "a", "c", "d", "e", "f", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "a", "d", "e", "f", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "a", "e", "f", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "a", "f", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "a", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "f", "a"]""") ==== false
    T ~ cc("""t7.label["a", "b", "b", "d", "e", "f", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "b", "e", "f", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "b", "f", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "b", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "f", "b"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "c", "e", "f", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "c", "f", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "c", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "f", "c"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "d", "f", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "d", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "f", "d"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "e", "g"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "f", "e"]""") ==== false
    T ~ cc("""t7.label["a", "b", "c", "d", "e", "f", "f"]""") ==== false
    T ~ cc("""t7 \\ ("a", "a", "c", "d", "e", "f", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "a", "d", "e", "f", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "a", "e", "f", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "a", "f", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "a", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "f", "a")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "b", "d", "e", "f", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "b", "e", "f", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "b", "f", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "b", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "f", "b")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "c", "e", "f", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "c", "f", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "c", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "f", "c")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "d", "f", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "d", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "f", "d")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "e", "g")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "f", "e")""")   ==== false
    T ~ cc("""t7 \\ ("a", "b", "c", "d", "e", "f", "f")""")   ==== false
    T ~ cc("""l7 ~~ ("_", "b", "c", "d", "e", "f", "g")""")   ==== false
    T ~ cc("""l7 ~~ ("a", "_", "c", "d", "e", "f", "g")""")   ==== false
    T ~ cc("""l7 ~~ ("a", "b", "_", "d", "e", "f", "g")""")   ==== false
    T ~ cc("""l7 ~~ ("a", "b", "c", "_", "e", "f", "g")""")   ==== false
    T ~ cc("""l7 ~~ ("a", "b", "c", "d", "_", "f", "g")""")   ==== false
    T ~ cc("""l7 ~~ ("a", "b", "c", "d", "e", "_", "g")""")   ==== false
    T ~ cc("""l7 ~~ ("a", "b", "c", "d", "e", "f", "_")""")   ==== false

    T ~ cc("""l7 ~ "_"""")              ==== false
    T ~ cc("""l7.relabel("_")("=")""")  ==== false
    T ~ cc("""l7.revalue("_")(0)""")    ==== false
    T ~ cc("""l7.redo("_")(0 \ "=")""") ==== false

    T ~ cc("""x12 ~ "a"""")              ==== false
    T ~ cc("""x13 ~ "a"""")              ==== false
    T ~ cc("""x14 ~ "a"""")              ==== false
    T ~ cc("""x15 ~ "a"""")              ==== false
    T ~ cc("""x16 ~ "a"""")              ==== false
    T ~ cc("""x17 ~ "a"""")              ==== false
    T ~ cc("""x23 ~ "b"""")              ==== false
    T ~ cc("""x24 ~ "b"""")              ==== false
    T ~ cc("""x25 ~ "b"""")              ==== false
    T ~ cc("""x26 ~ "b"""")              ==== false
    T ~ cc("""x27 ~ "b"""")              ==== false
    T ~ cc("""x34 ~ "c"""")              ==== false
    T ~ cc("""x35 ~ "c"""")              ==== false
    T ~ cc("""x36 ~ "c"""")              ==== false
    T ~ cc("""x37 ~ "c"""")              ==== false
    T ~ cc("""x45 ~ "d"""")              ==== false
    T ~ cc("""x46 ~ "d"""")              ==== false
    T ~ cc("""x47 ~ "d"""")              ==== false
    T ~ cc("""x56 ~ "e"""")              ==== false
    T ~ cc("""x57 ~ "e"""")              ==== false
    T ~ cc("""x67 ~ "f"""")              ==== false
    T ~ cc("""x12().a""")                ==== false
    T ~ cc("""x13().a""")                ==== false
    T ~ cc("""x14().a""")                ==== false
    T ~ cc("""x15().a""")                ==== false
    T ~ cc("""x16().a""")                ==== false
    T ~ cc("""x17().a""")                ==== false
    T ~ cc("""x23().b""")                ==== false
    T ~ cc("""x24().b""")                ==== false
    T ~ cc("""x25().b""")                ==== false
    T ~ cc("""x26().b""")                ==== false
    T ~ cc("""x27().b""")                ==== false
    T ~ cc("""x34().c""")                ==== false
    T ~ cc("""x35().c""")                ==== false
    T ~ cc("""x36().c""")                ==== false
    T ~ cc("""x37().c""")                ==== false
    T ~ cc("""x45().d""")                ==== false
    T ~ cc("""x46().d""")                ==== false
    T ~ cc("""x47().d""")                ==== false
    T ~ cc("""x56().e""")                ==== false
    T ~ cc("""x57().e""")                ==== false
    T ~ cc("""x67().f""")                ==== false
    T ~ cc("""x12.relabel("a")("=")""")  ==== false
    T ~ cc("""x13.relabel("a")("=")""")  ==== false
    T ~ cc("""x14.relabel("a")("=")""")  ==== false
    T ~ cc("""x15.relabel("a")("=")""")  ==== false
    T ~ cc("""x16.relabel("a")("=")""")  ==== false
    T ~ cc("""x17.relabel("a")("=")""")  ==== false
    T ~ cc("""x23.relabel("b")("=")""")  ==== false
    T ~ cc("""x24.relabel("b")("=")""")  ==== false
    T ~ cc("""x25.relabel("b")("=")""")  ==== false
    T ~ cc("""x26.relabel("b")("=")""")  ==== false
    T ~ cc("""x27.relabel("b")("=")""")  ==== false
    T ~ cc("""x34.relabel("c")("=")""")  ==== false
    T ~ cc("""x35.relabel("c")("=")""")  ==== false
    T ~ cc("""x36.relabel("c")("=")""")  ==== false
    T ~ cc("""x37.relabel("c")("=")""")  ==== false
    T ~ cc("""x45.relabel("d")("=")""")  ==== false
    T ~ cc("""x46.relabel("d")("=")""")  ==== false
    T ~ cc("""x47.relabel("d")("=")""")  ==== false
    T ~ cc("""x56.relabel("e")("=")""")  ==== false
    T ~ cc("""x57.relabel("e")("=")""")  ==== false
    T ~ cc("""x67.relabel("f")("=")""")  ==== false
    T ~ cc("""x12.revalue("a")(0)""")    ==== false
    T ~ cc("""x13.revalue("a")(0)""")    ==== false
    T ~ cc("""x14.revalue("a")(0)""")    ==== false
    T ~ cc("""x15.revalue("a")(0)""")    ==== false
    T ~ cc("""x16.revalue("a")(0)""")    ==== false
    T ~ cc("""x17.revalue("a")(0)""")    ==== false
    T ~ cc("""x23.revalue("b")(0)""")    ==== false
    T ~ cc("""x24.revalue("b")(0)""")    ==== false
    T ~ cc("""x25.revalue("b")(0)""")    ==== false
    T ~ cc("""x26.revalue("b")(0)""")    ==== false
    T ~ cc("""x27.revalue("b")(0)""")    ==== false
    T ~ cc("""x34.revalue("c")(0)""")    ==== false
    T ~ cc("""x35.revalue("c")(0)""")    ==== false
    T ~ cc("""x36.revalue("c")(0)""")    ==== false
    T ~ cc("""x37.revalue("c")(0)""")    ==== false
    T ~ cc("""x45.revalue("d")(0)""")    ==== false
    T ~ cc("""x46.revalue("d")(0)""")    ==== false
    T ~ cc("""x47.revalue("d")(0)""")    ==== false
    T ~ cc("""x56.revalue("e")(0)""")    ==== false
    T ~ cc("""x57.revalue("e")(0)""")    ==== false
    T ~ cc("""x67.revalue("f")(0)""")    ==== false
    T ~ cc("""x12.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x13.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x14.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x15.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x16.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x17.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x23.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x24.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x25.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x26.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x27.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x34.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x35.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x36.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x37.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x45.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x46.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x47.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x56.redo("e")(0 \ "=")""") ==== false
    T ~ cc("""x57.redo("e")(0 \ "=")""") ==== false
    T ~ cc("""x67.redo("f")(0 \ "=")""") ==== false

    T ~ cc("""l7.relabel("a")("b")""")  ==== false
    T ~ cc("""l7.relabel("a")("c")""")  ==== false
    T ~ cc("""l7.relabel("a")("d")""")  ==== false
    T ~ cc("""l7.relabel("a")("e")""")  ==== false
    T ~ cc("""l7.relabel("a")("f")""")  ==== false
    T ~ cc("""l7.relabel("a")("g")""")  ==== false
    T ~ cc("""l7.relabel("b")("a")""")  ==== false
    T ~ cc("""l7.relabel("b")("c")""")  ==== false
    T ~ cc("""l7.relabel("b")("d")""")  ==== false
    T ~ cc("""l7.relabel("b")("e")""")  ==== false
    T ~ cc("""l7.relabel("b")("f")""")  ==== false
    T ~ cc("""l7.relabel("b")("g")""")  ==== false
    T ~ cc("""l7.relabel("c")("a")""")  ==== false
    T ~ cc("""l7.relabel("c")("b")""")  ==== false
    T ~ cc("""l7.relabel("c")("d")""")  ==== false
    T ~ cc("""l7.relabel("c")("e")""")  ==== false
    T ~ cc("""l7.relabel("c")("f")""")  ==== false
    T ~ cc("""l7.relabel("c")("g")""")  ==== false
    T ~ cc("""l7.relabel("d")("a")""")  ==== false
    T ~ cc("""l7.relabel("d")("b")""")  ==== false
    T ~ cc("""l7.relabel("d")("c")""")  ==== false
    T ~ cc("""l7.relabel("d")("e")""")  ==== false
    T ~ cc("""l7.relabel("d")("f")""")  ==== false
    T ~ cc("""l7.relabel("d")("g")""")  ==== false
    T ~ cc("""l7.relabel("e")("a")""")  ==== false
    T ~ cc("""l7.relabel("e")("b")""")  ==== false
    T ~ cc("""l7.relabel("e")("c")""")  ==== false
    T ~ cc("""l7.relabel("e")("d")""")  ==== false
    T ~ cc("""l7.relabel("e")("f")""")  ==== false
    T ~ cc("""l7.relabel("e")("g")""")  ==== false
    T ~ cc("""l7.relabel("f")("a")""")  ==== false
    T ~ cc("""l7.relabel("f")("b")""")  ==== false
    T ~ cc("""l7.relabel("f")("c")""")  ==== false
    T ~ cc("""l7.relabel("f")("d")""")  ==== false
    T ~ cc("""l7.relabel("f")("e")""")  ==== false
    T ~ cc("""l7.relabel("f")("g")""")  ==== false
    T ~ cc("""l7.relabel("g")("a")""")  ==== false
    T ~ cc("""l7.relabel("g")("b")""")  ==== false
    T ~ cc("""l7.relabel("g")("c")""")  ==== false
    T ~ cc("""l7.relabel("g")("d")""")  ==== false
    T ~ cc("""l7.relabel("g")("e")""")  ==== false
    T ~ cc("""l7.relabel("g")("f")""")  ==== false
    T ~ cc("""l7.redo("a")(0 \ "b")""") ==== false
    T ~ cc("""l7.redo("a")(0 \ "c")""") ==== false
    T ~ cc("""l7.redo("a")(0 \ "d")""") ==== false
    T ~ cc("""l7.redo("a")(0 \ "e")""") ==== false
    T ~ cc("""l7.redo("a")(0 \ "f")""") ==== false
    T ~ cc("""l7.redo("a")(0 \ "g")""") ==== false
    T ~ cc("""l7.redo("b")(0 \ "a")""") ==== false
    T ~ cc("""l7.redo("b")(0 \ "c")""") ==== false
    T ~ cc("""l7.redo("b")(0 \ "d")""") ==== false
    T ~ cc("""l7.redo("b")(0 \ "e")""") ==== false
    T ~ cc("""l7.redo("b")(0 \ "f")""") ==== false
    T ~ cc("""l7.redo("b")(0 \ "g")""") ==== false
    T ~ cc("""l7.redo("c")(0 \ "a")""") ==== false
    T ~ cc("""l7.redo("c")(0 \ "b")""") ==== false
    T ~ cc("""l7.redo("c")(0 \ "d")""") ==== false
    T ~ cc("""l7.redo("c")(0 \ "e")""") ==== false
    T ~ cc("""l7.redo("c")(0 \ "f")""") ==== false
    T ~ cc("""l7.redo("c")(0 \ "g")""") ==== false
    T ~ cc("""l7.redo("d")(0 \ "a")""") ==== false
    T ~ cc("""l7.redo("d")(0 \ "b")""") ==== false
    T ~ cc("""l7.redo("d")(0 \ "c")""") ==== false
    T ~ cc("""l7.redo("d")(0 \ "e")""") ==== false
    T ~ cc("""l7.redo("d")(0 \ "f")""") ==== false
    T ~ cc("""l7.redo("d")(0 \ "g")""") ==== false
    T ~ cc("""l7.redo("e")(0 \ "a")""") ==== false
    T ~ cc("""l7.redo("e")(0 \ "b")""") ==== false
    T ~ cc("""l7.redo("e")(0 \ "c")""") ==== false
    T ~ cc("""l7.redo("e")(0 \ "d")""") ==== false
    T ~ cc("""l7.redo("e")(0 \ "f")""") ==== false
    T ~ cc("""l7.redo("e")(0 \ "g")""") ==== false
    T ~ cc("""l7.redo("f")(0 \ "a")""") ==== false
    T ~ cc("""l7.redo("f")(0 \ "b")""") ==== false
    T ~ cc("""l7.redo("f")(0 \ "c")""") ==== false
    T ~ cc("""l7.redo("f")(0 \ "d")""") ==== false
    T ~ cc("""l7.redo("f")(0 \ "e")""") ==== false
    T ~ cc("""l7.redo("f")(0 \ "g")""") ==== false
    T ~ cc("""l7.redo("g")(0 \ "a")""") ==== false
    T ~ cc("""l7.redo("g")(0 \ "b")""") ==== false
    T ~ cc("""l7.redo("g")(0 \ "c")""") ==== false
    T ~ cc("""l7.redo("g")(0 \ "d")""") ==== false
    T ~ cc("""l7.redo("g")(0 \ "e")""") ==== false
    T ~ cc("""l7.redo("g")(0 \ "f")""") ==== false


  def labelledOctuplet(): Unit =
    val t8 = (1, 2, 3, 4, 5, 6, 7, 8)
    val l8 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h")
    T ~ l8                                               ==== t8         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ t8.label["a", "b", "c", "d", "e", "f", "g", "h"] ==== t8         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ (t8 \\ ("a", "b", "c", "d", "e", "f", "g", "h")) ==== t8         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ (l8 ~ "a")                                       ==== 1          --: typed[Int]
    T ~ (l8 ~ "b")                                       ==== 2          --: typed[Int]
    T ~ (l8 ~ "c")                                       ==== 3          --: typed[Int]
    T ~ (l8 ~ "d")                                       ==== 4          --: typed[Int]
    T ~ (l8 ~ "e")                                       ==== 5          --: typed[Int]
    T ~ (l8 ~ "f")                                       ==== 6          --: typed[Int]
    T ~ (l8 ~ "g")                                       ==== 7          --: typed[Int]
    T ~ (l8 ~ "h")                                       ==== 8          --: typed[Int]
    T ~ l8.unlabel                                       ==== t8         --: typed[(Int, Int, Int, Int, Int, Int, Int, Int)]
    T ~ l8.label_1                                       ==== "a"
    T ~ l8.label_2                                       ==== "b"
    T ~ l8.label_3                                       ==== "c"
    T ~ l8.label_4                                       ==== "d"
    T ~ l8.label_5                                       ==== "e"
    T ~ l8.label_6                                       ==== "f"
    T ~ l8.label_7                                       ==== "g"
    T ~ l8.label_8                                       ==== "h"
    T ~ l8.labels                                        ==== ("a", "b", "c", "d", "e", "f", "g", "h")
    T ~ l8().a                                           ==== 1          --: typed[Int]
    T ~ l8().b                                           ==== 2          --: typed[Int]
    T ~ l8().c                                           ==== 3          --: typed[Int]
    T ~ l8().d                                           ==== 4          --: typed[Int]
    T ~ l8().e                                           ==== 5          --: typed[Int]
    T ~ l8().f                                           ==== 6          --: typed[Int]
    T ~ l8().g                                           ==== 7          --: typed[Int]
    T ~ l8().h                                           ==== 8          --: typed[Int]
    T ~ (l8 ~~ ("a", "b", "c", "d", "e", "f", "g", "h")) ==== t8         --: typed[(Int, Int, Int, Int, Int, Int, Int, Int)]
    T ~ l8.relabel("a")("_")                             ==== t8         --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.relabel("b")("_")                             ==== t8         --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.relabel("c")("_")                             ==== t8         --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.relabel("d")("_")                             ==== t8         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.relabel("e")("_")                             ==== t8         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "_", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.relabel("f")("_")                             ==== t8         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "_", Int \ "g", Int \ "h")]
    T ~ l8.relabel("g")("_")                             ==== t8         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "_", Int \ "h")]
    T ~ l8.relabel("h")("_")                             ==== t8         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "_")]
    T ~ l8.revalue("a")(0)                               ==== t8._1to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.revalue("b")(0)                               ==== t8._2to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.revalue("c")(0)                               ==== t8._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.revalue("d")(0)                               ==== t8._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.revalue("e")(0)                               ==== t8._5to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.revalue("f")(0)                               ==== t8._6to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.revalue("g")(0)                               ==== t8._7to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.revalue("h")(0)                               ==== t8._8to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.redo("a")(0 \ "_")                            ==== t8._1to(0) --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.redo("b")(0 \ "_")                            ==== t8._2to(0) --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.redo("c")(0 \ "_")                            ==== t8._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.redo("d")(0 \ "_")                            ==== t8._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_", Int \ "e", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.redo("e")(0 \ "_")                            ==== t8._5to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "_", Int \ "f", Int \ "g", Int \ "h")]
    T ~ l8.redo("f")(0 \ "_")                            ==== t8._6to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "_", Int \ "g", Int \ "h")]
    T ~ l8.redo("g")(0 \ "_")                            ==== t8._7to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "_", Int \ "h")]
    T ~ l8.redo("h")(0 \ "_")                            ==== t8._8to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "_")]

    val x12 = (1 \ "a", 2 \ "a", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h")
    val x13 = (1 \ "a", 2 \ "b", 3 \ "a", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h")
    val x14 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "a", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h")
    val x15 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "a", 6 \ "f", 7 \ "g", 8 \ "h")
    val x16 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "a", 7 \ "g", 8 \ "h")
    val x17 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "a", 8 \ "h")
    val x18 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "a")
    val x23 = (1 \ "a", 2 \ "b", 3 \ "b", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h")
    val x24 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "b", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h")
    val x25 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "b", 6 \ "f", 7 \ "g", 8 \ "h")
    val x26 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "b", 7 \ "g", 8 \ "h")
    val x27 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "b", 8 \ "h")
    val x28 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "b")
    val x34 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "c", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h")
    val x35 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "c", 6 \ "f", 7 \ "g", 8 \ "h")
    val x36 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "c", 7 \ "g", 8 \ "h")
    val x37 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "c", 8 \ "h")
    val x38 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "c")
    val x45 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "d", 6 \ "f", 7 \ "g", 8 \ "h")
    val x46 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "d", 7 \ "g", 8 \ "h")
    val x47 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "d", 8 \ "h")
    val x48 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "d")
    val x56 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "e", 7 \ "g", 8 \ "h")
    val x57 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "e", 8 \ "h")
    val x58 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "e")
    val x67 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "f", 8 \ "h")
    val x68 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "f")
    val x78 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "g")
    T ~ cc("""t8.label["a", "a", "c", "d", "e", "f", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "a", "d", "e", "f", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "a", "e", "f", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "a", "f", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "a", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "a", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "g", "a"]""") ==== false
    T ~ cc("""t8.label["a", "b", "b", "d", "e", "f", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "b", "e", "f", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "b", "f", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "b", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "b", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "g", "b"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "c", "e", "f", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "c", "f", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "c", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "c", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "g", "c"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "d", "f", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "d", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "d", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "g", "d"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "e", "g", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "e", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "g", "e"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "f", "h"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "g", "f"]""") ==== false
    T ~ cc("""t8.label["a", "b", "c", "d", "e", "f", "g", "g"]""") ==== false
    T ~ cc("""t8 \\ ("a", "a", "c", "d", "e", "f", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "a", "d", "e", "f", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "a", "e", "f", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "a", "f", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "a", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "a", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "g", "a")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "b", "d", "e", "f", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "b", "e", "f", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "b", "f", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "b", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "b", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "g", "b")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "c", "e", "f", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "c", "f", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "c", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "c", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "g", "c")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "d", "f", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "d", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "d", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "g", "d")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "e", "g", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "e", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "g", "e")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "f", "h")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "g", "f")""")   ==== false
    T ~ cc("""t8 \\ ("a", "b", "c", "d", "e", "f", "g", "g")""")   ==== false
    T ~ cc("""l8 ~~ ("_", "b", "c", "d", "e", "f", "g", "h")""")   ==== false
    T ~ cc("""l8 ~~ ("a", "_", "c", "d", "e", "f", "g", "h")""")   ==== false
    T ~ cc("""l8 ~~ ("a", "b", "_", "d", "e", "f", "g", "h")""")   ==== false
    T ~ cc("""l8 ~~ ("a", "b", "c", "_", "e", "f", "g", "h")""")   ==== false
    T ~ cc("""l8 ~~ ("a", "b", "c", "d", "_", "f", "g", "h")""")   ==== false
    T ~ cc("""l8 ~~ ("a", "b", "c", "d", "e", "_", "g", "h")""")   ==== false
    T ~ cc("""l8 ~~ ("a", "b", "c", "d", "e", "f", "_", "h")""")   ==== false
    T ~ cc("""l8 ~~ ("a", "b", "c", "d", "e", "f", "g", "_")""")   ==== false

    T ~ cc("""l8 ~ "_"""")              ==== false
    T ~ cc("""l8.relabel("_")("=")""")  ==== false
    T ~ cc("""l8.revalue("_")(0)""")    ==== false
    T ~ cc("""l8.redo("_")(0 \ "=")""") ==== false

    T ~ cc("""x12 ~ "a"""")              ==== false
    T ~ cc("""x13 ~ "a"""")              ==== false
    T ~ cc("""x14 ~ "a"""")              ==== false
    T ~ cc("""x15 ~ "a"""")              ==== false
    T ~ cc("""x16 ~ "a"""")              ==== false
    T ~ cc("""x17 ~ "a"""")              ==== false
    T ~ cc("""x18 ~ "a"""")              ==== false
    T ~ cc("""x23 ~ "b"""")              ==== false
    T ~ cc("""x24 ~ "b"""")              ==== false
    T ~ cc("""x25 ~ "b"""")              ==== false
    T ~ cc("""x26 ~ "b"""")              ==== false
    T ~ cc("""x27 ~ "b"""")              ==== false
    T ~ cc("""x28 ~ "b"""")              ==== false
    T ~ cc("""x34 ~ "c"""")              ==== false
    T ~ cc("""x35 ~ "c"""")              ==== false
    T ~ cc("""x36 ~ "c"""")              ==== false
    T ~ cc("""x37 ~ "c"""")              ==== false
    T ~ cc("""x38 ~ "c"""")              ==== false
    T ~ cc("""x45 ~ "d"""")              ==== false
    T ~ cc("""x46 ~ "d"""")              ==== false
    T ~ cc("""x47 ~ "d"""")              ==== false
    T ~ cc("""x48 ~ "d"""")              ==== false
    T ~ cc("""x56 ~ "e"""")              ==== false
    T ~ cc("""x57 ~ "e"""")              ==== false
    T ~ cc("""x58 ~ "e"""")              ==== false
    T ~ cc("""x67 ~ "f"""")              ==== false
    T ~ cc("""x68 ~ "f"""")              ==== false
    T ~ cc("""x78 ~ "g"""")              ==== false
    T ~ cc("""x12().a""")                ==== false
    T ~ cc("""x13().a""")                ==== false
    T ~ cc("""x14().a""")                ==== false
    T ~ cc("""x15().a""")                ==== false
    T ~ cc("""x16().a""")                ==== false
    T ~ cc("""x17().a""")                ==== false
    T ~ cc("""x18().a""")                ==== false
    T ~ cc("""x23().b""")                ==== false
    T ~ cc("""x24().b""")                ==== false
    T ~ cc("""x25().b""")                ==== false
    T ~ cc("""x26().b""")                ==== false
    T ~ cc("""x27().b""")                ==== false
    T ~ cc("""x28().b""")                ==== false
    T ~ cc("""x34().c""")                ==== false
    T ~ cc("""x35().c""")                ==== false
    T ~ cc("""x36().c""")                ==== false
    T ~ cc("""x37().c""")                ==== false
    T ~ cc("""x38().c""")                ==== false
    T ~ cc("""x45().d""")                ==== false
    T ~ cc("""x46().d""")                ==== false
    T ~ cc("""x47().d""")                ==== false
    T ~ cc("""x48().d""")                ==== false
    T ~ cc("""x56().e""")                ==== false
    T ~ cc("""x57().e""")                ==== false
    T ~ cc("""x58().e""")                ==== false
    T ~ cc("""x67().f""")                ==== false
    T ~ cc("""x68().f""")                ==== false
    T ~ cc("""x78().g""")                ==== false
    T ~ cc("""x12.relabel("a")("=")""")  ==== false
    T ~ cc("""x13.relabel("a")("=")""")  ==== false
    T ~ cc("""x14.relabel("a")("=")""")  ==== false
    T ~ cc("""x15.relabel("a")("=")""")  ==== false
    T ~ cc("""x16.relabel("a")("=")""")  ==== false
    T ~ cc("""x17.relabel("a")("=")""")  ==== false
    T ~ cc("""x18.relabel("a")("=")""")  ==== false
    T ~ cc("""x23.relabel("b")("=")""")  ==== false
    T ~ cc("""x24.relabel("b")("=")""")  ==== false
    T ~ cc("""x25.relabel("b")("=")""")  ==== false
    T ~ cc("""x26.relabel("b")("=")""")  ==== false
    T ~ cc("""x27.relabel("b")("=")""")  ==== false
    T ~ cc("""x28.relabel("b")("=")""")  ==== false
    T ~ cc("""x34.relabel("c")("=")""")  ==== false
    T ~ cc("""x35.relabel("c")("=")""")  ==== false
    T ~ cc("""x36.relabel("c")("=")""")  ==== false
    T ~ cc("""x37.relabel("c")("=")""")  ==== false
    T ~ cc("""x38.relabel("c")("=")""")  ==== false
    T ~ cc("""x45.relabel("d")("=")""")  ==== false
    T ~ cc("""x46.relabel("d")("=")""")  ==== false
    T ~ cc("""x47.relabel("d")("=")""")  ==== false
    T ~ cc("""x48.relabel("d")("=")""")  ==== false
    T ~ cc("""x56.relabel("e")("=")""")  ==== false
    T ~ cc("""x57.relabel("e")("=")""")  ==== false
    T ~ cc("""x58.relabel("e")("=")""")  ==== false
    T ~ cc("""x67.relabel("f")("=")""")  ==== false
    T ~ cc("""x68.relabel("f")("=")""")  ==== false
    T ~ cc("""x78.relabel("g")("=")""")  ==== false
    T ~ cc("""x12.revalue("a")(0)""")    ==== false
    T ~ cc("""x13.revalue("a")(0)""")    ==== false
    T ~ cc("""x14.revalue("a")(0)""")    ==== false
    T ~ cc("""x15.revalue("a")(0)""")    ==== false
    T ~ cc("""x16.revalue("a")(0)""")    ==== false
    T ~ cc("""x17.revalue("a")(0)""")    ==== false
    T ~ cc("""x18.revalue("a")(0)""")    ==== false
    T ~ cc("""x23.revalue("b")(0)""")    ==== false
    T ~ cc("""x24.revalue("b")(0)""")    ==== false
    T ~ cc("""x25.revalue("b")(0)""")    ==== false
    T ~ cc("""x26.revalue("b")(0)""")    ==== false
    T ~ cc("""x27.revalue("b")(0)""")    ==== false
    T ~ cc("""x28.revalue("b")(0)""")    ==== false
    T ~ cc("""x34.revalue("c")(0)""")    ==== false
    T ~ cc("""x35.revalue("c")(0)""")    ==== false
    T ~ cc("""x36.revalue("c")(0)""")    ==== false
    T ~ cc("""x37.revalue("c")(0)""")    ==== false
    T ~ cc("""x38.revalue("c")(0)""")    ==== false
    T ~ cc("""x45.revalue("d")(0)""")    ==== false
    T ~ cc("""x46.revalue("d")(0)""")    ==== false
    T ~ cc("""x47.revalue("d")(0)""")    ==== false
    T ~ cc("""x48.revalue("d")(0)""")    ==== false
    T ~ cc("""x56.revalue("e")(0)""")    ==== false
    T ~ cc("""x57.revalue("e")(0)""")    ==== false
    T ~ cc("""x58.revalue("e")(0)""")    ==== false
    T ~ cc("""x67.revalue("f")(0)""")    ==== false
    T ~ cc("""x68.revalue("f")(0)""")    ==== false
    T ~ cc("""x78.revalue("g")(0)""")    ==== false
    T ~ cc("""x12.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x13.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x14.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x15.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x16.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x17.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x18.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x23.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x24.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x25.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x26.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x27.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x28.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x34.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x35.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x36.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x37.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x38.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x45.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x46.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x47.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x48.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x56.redo("e")(0 \ "=")""") ==== false
    T ~ cc("""x57.redo("e")(0 \ "=")""") ==== false
    T ~ cc("""x58.redo("e")(0 \ "=")""") ==== false
    T ~ cc("""x67.redo("f")(0 \ "=")""") ==== false
    T ~ cc("""x68.redo("f")(0 \ "=")""") ==== false
    T ~ cc("""x78.redo("g")(0 \ "=")""") ==== false

    T ~ cc("""l8.relabel("a")("b")""")  ==== false
    T ~ cc("""l8.relabel("a")("c")""")  ==== false
    T ~ cc("""l8.relabel("a")("d")""")  ==== false
    T ~ cc("""l8.relabel("a")("e")""")  ==== false
    T ~ cc("""l8.relabel("a")("f")""")  ==== false
    T ~ cc("""l8.relabel("a")("g")""")  ==== false
    T ~ cc("""l8.relabel("a")("h")""")  ==== false
    T ~ cc("""l8.relabel("b")("a")""")  ==== false
    T ~ cc("""l8.relabel("b")("c")""")  ==== false
    T ~ cc("""l8.relabel("b")("d")""")  ==== false
    T ~ cc("""l8.relabel("b")("e")""")  ==== false
    T ~ cc("""l8.relabel("b")("f")""")  ==== false
    T ~ cc("""l8.relabel("b")("g")""")  ==== false
    T ~ cc("""l8.relabel("b")("h")""")  ==== false
    T ~ cc("""l8.relabel("c")("a")""")  ==== false
    T ~ cc("""l8.relabel("c")("b")""")  ==== false
    T ~ cc("""l8.relabel("c")("d")""")  ==== false
    T ~ cc("""l8.relabel("c")("e")""")  ==== false
    T ~ cc("""l8.relabel("c")("f")""")  ==== false
    T ~ cc("""l8.relabel("c")("g")""")  ==== false
    T ~ cc("""l8.relabel("c")("h")""")  ==== false
    T ~ cc("""l8.relabel("d")("a")""")  ==== false
    T ~ cc("""l8.relabel("d")("b")""")  ==== false
    T ~ cc("""l8.relabel("d")("c")""")  ==== false
    T ~ cc("""l8.relabel("d")("e")""")  ==== false
    T ~ cc("""l8.relabel("d")("f")""")  ==== false
    T ~ cc("""l8.relabel("d")("g")""")  ==== false
    T ~ cc("""l8.relabel("d")("h")""")  ==== false
    T ~ cc("""l8.relabel("e")("a")""")  ==== false
    T ~ cc("""l8.relabel("e")("b")""")  ==== false
    T ~ cc("""l8.relabel("e")("c")""")  ==== false
    T ~ cc("""l8.relabel("e")("d")""")  ==== false
    T ~ cc("""l8.relabel("e")("f")""")  ==== false
    T ~ cc("""l8.relabel("e")("g")""")  ==== false
    T ~ cc("""l8.relabel("e")("h")""")  ==== false
    T ~ cc("""l8.relabel("f")("a")""")  ==== false
    T ~ cc("""l8.relabel("f")("b")""")  ==== false
    T ~ cc("""l8.relabel("f")("c")""")  ==== false
    T ~ cc("""l8.relabel("f")("d")""")  ==== false
    T ~ cc("""l8.relabel("f")("e")""")  ==== false
    T ~ cc("""l8.relabel("f")("g")""")  ==== false
    T ~ cc("""l8.relabel("f")("h")""")  ==== false
    T ~ cc("""l8.relabel("g")("a")""")  ==== false
    T ~ cc("""l8.relabel("g")("b")""")  ==== false
    T ~ cc("""l8.relabel("g")("c")""")  ==== false
    T ~ cc("""l8.relabel("g")("d")""")  ==== false
    T ~ cc("""l8.relabel("g")("e")""")  ==== false
    T ~ cc("""l8.relabel("g")("f")""")  ==== false
    T ~ cc("""l8.relabel("g")("h")""")  ==== false
    T ~ cc("""l8.relabel("h")("a")""")  ==== false
    T ~ cc("""l8.relabel("h")("b")""")  ==== false
    T ~ cc("""l8.relabel("h")("c")""")  ==== false
    T ~ cc("""l8.relabel("h")("d")""")  ==== false
    T ~ cc("""l8.relabel("h")("e")""")  ==== false
    T ~ cc("""l8.relabel("h")("f")""")  ==== false
    T ~ cc("""l8.relabel("h")("g")""")  ==== false
    T ~ cc("""l8.redo("a")(0 \ "b")""") ==== false
    T ~ cc("""l8.redo("a")(0 \ "c")""") ==== false
    T ~ cc("""l8.redo("a")(0 \ "d")""") ==== false
    T ~ cc("""l8.redo("a")(0 \ "e")""") ==== false
    T ~ cc("""l8.redo("a")(0 \ "f")""") ==== false
    T ~ cc("""l8.redo("a")(0 \ "g")""") ==== false
    T ~ cc("""l8.redo("a")(0 \ "h")""") ==== false
    T ~ cc("""l8.redo("b")(0 \ "a")""") ==== false
    T ~ cc("""l8.redo("b")(0 \ "c")""") ==== false
    T ~ cc("""l8.redo("b")(0 \ "d")""") ==== false
    T ~ cc("""l8.redo("b")(0 \ "e")""") ==== false
    T ~ cc("""l8.redo("b")(0 \ "f")""") ==== false
    T ~ cc("""l8.redo("b")(0 \ "g")""") ==== false
    T ~ cc("""l8.redo("b")(0 \ "h")""") ==== false
    T ~ cc("""l8.redo("c")(0 \ "a")""") ==== false
    T ~ cc("""l8.redo("c")(0 \ "b")""") ==== false
    T ~ cc("""l8.redo("c")(0 \ "d")""") ==== false
    T ~ cc("""l8.redo("c")(0 \ "e")""") ==== false
    T ~ cc("""l8.redo("c")(0 \ "f")""") ==== false
    T ~ cc("""l8.redo("c")(0 \ "g")""") ==== false
    T ~ cc("""l8.redo("c")(0 \ "h")""") ==== false
    T ~ cc("""l8.redo("d")(0 \ "a")""") ==== false
    T ~ cc("""l8.redo("d")(0 \ "b")""") ==== false
    T ~ cc("""l8.redo("d")(0 \ "c")""") ==== false
    T ~ cc("""l8.redo("d")(0 \ "e")""") ==== false
    T ~ cc("""l8.redo("d")(0 \ "f")""") ==== false
    T ~ cc("""l8.redo("d")(0 \ "g")""") ==== false
    T ~ cc("""l8.redo("d")(0 \ "h")""") ==== false
    T ~ cc("""l8.redo("e")(0 \ "a")""") ==== false
    T ~ cc("""l8.redo("e")(0 \ "b")""") ==== false
    T ~ cc("""l8.redo("e")(0 \ "c")""") ==== false
    T ~ cc("""l8.redo("e")(0 \ "d")""") ==== false
    T ~ cc("""l8.redo("e")(0 \ "f")""") ==== false
    T ~ cc("""l8.redo("e")(0 \ "g")""") ==== false
    T ~ cc("""l8.redo("e")(0 \ "h")""") ==== false
    T ~ cc("""l8.redo("f")(0 \ "a")""") ==== false
    T ~ cc("""l8.redo("f")(0 \ "b")""") ==== false
    T ~ cc("""l8.redo("f")(0 \ "c")""") ==== false
    T ~ cc("""l8.redo("f")(0 \ "d")""") ==== false
    T ~ cc("""l8.redo("f")(0 \ "e")""") ==== false
    T ~ cc("""l8.redo("f")(0 \ "g")""") ==== false
    T ~ cc("""l8.redo("f")(0 \ "h")""") ==== false
    T ~ cc("""l8.redo("g")(0 \ "a")""") ==== false
    T ~ cc("""l8.redo("g")(0 \ "b")""") ==== false
    T ~ cc("""l8.redo("g")(0 \ "c")""") ==== false
    T ~ cc("""l8.redo("g")(0 \ "d")""") ==== false
    T ~ cc("""l8.redo("g")(0 \ "e")""") ==== false
    T ~ cc("""l8.redo("g")(0 \ "f")""") ==== false
    T ~ cc("""l8.redo("g")(0 \ "h")""") ==== false
    T ~ cc("""l8.redo("h")(0 \ "a")""") ==== false
    T ~ cc("""l8.redo("h")(0 \ "b")""") ==== false
    T ~ cc("""l8.redo("h")(0 \ "c")""") ==== false
    T ~ cc("""l8.redo("h")(0 \ "d")""") ==== false
    T ~ cc("""l8.redo("h")(0 \ "e")""") ==== false
    T ~ cc("""l8.redo("h")(0 \ "f")""") ==== false
    T ~ cc("""l8.redo("h")(0 \ "g")""") ==== false


  def labelledNonuplet(): Unit =
    val t9 = (1, 2, 3, 4, 5, 6, 7, 8, 9)
    val l9 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    T ~ l9                                                    ==== t9         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ t9.label["a", "b", "c", "d", "e", "f", "g", "h", "i"] ==== t9         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ (t9 \\ ("a", "b", "c", "d", "e", "f", "g", "h", "i")) ==== t9         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ (l9 ~ "a")                                            ==== 1          --: typed[Int]
    T ~ (l9 ~ "b")                                            ==== 2          --: typed[Int]
    T ~ (l9 ~ "c")                                            ==== 3          --: typed[Int]
    T ~ (l9 ~ "d")                                            ==== 4          --: typed[Int]
    T ~ (l9 ~ "e")                                            ==== 5          --: typed[Int]
    T ~ (l9 ~ "f")                                            ==== 6          --: typed[Int]
    T ~ (l9 ~ "g")                                            ==== 7          --: typed[Int]
    T ~ (l9 ~ "h")                                            ==== 8          --: typed[Int]
    T ~ (l9 ~ "i")                                            ==== 9          --: typed[Int]
    T ~ l9.unlabel                                            ==== t9         --: typed[(Int, Int, Int, Int, Int, Int, Int, Int, Int)]
    T ~ l9.label_1                                            ==== "a"
    T ~ l9.label_2                                            ==== "b"
    T ~ l9.label_3                                            ==== "c"
    T ~ l9.label_4                                            ==== "d"
    T ~ l9.label_5                                            ==== "e"
    T ~ l9.label_6                                            ==== "f"
    T ~ l9.label_7                                            ==== "g"
    T ~ l9.label_8                                            ==== "h"
    T ~ l9.label_9                                            ==== "i"
    T ~ l9.labels                                             ==== ("a", "b", "c", "d", "e", "f", "g", "h", "i")
    T ~ l9().a                                                ==== 1          --: typed[Int]
    T ~ l9().b                                                ==== 2          --: typed[Int]
    T ~ l9().c                                                ==== 3          --: typed[Int]
    T ~ l9().d                                                ==== 4          --: typed[Int]
    T ~ l9().e                                                ==== 5          --: typed[Int]
    T ~ l9().f                                                ==== 6          --: typed[Int]
    T ~ l9().g                                                ==== 7          --: typed[Int]
    T ~ l9().h                                                ==== 8          --: typed[Int]
    T ~ l9().i                                                ==== 9          --: typed[Int]
    T ~ (l9 ~~ ("a", "b", "c", "d", "e", "f", "g", "h", "i")) ==== t9         --: typed[(Int, Int, Int, Int, Int, Int, Int, Int, Int)]
    T ~ l9.relabel("a")("_")                                  ==== t9         --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.relabel("b")("_")                                  ==== t9         --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.relabel("c")("_")                                  ==== t9         --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.relabel("d")("_")                                  ==== t9         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.relabel("e")("_")                                  ==== t9         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "_", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.relabel("f")("_")                                  ==== t9         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "_", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.relabel("g")("_")                                  ==== t9         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "_", Int \ "h", Int \ "i")]
    T ~ l9.relabel("h")("_")                                  ==== t9         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "_", Int \ "i")]
    T ~ l9.relabel("i")("_")                                  ==== t9         --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "_")]
    T ~ l9.revalue("a")(0)                                    ==== t9._1to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.revalue("b")(0)                                    ==== t9._2to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.revalue("c")(0)                                    ==== t9._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.revalue("d")(0)                                    ==== t9._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.revalue("e")(0)                                    ==== t9._5to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.revalue("f")(0)                                    ==== t9._6to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.revalue("g")(0)                                    ==== t9._7to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.revalue("h")(0)                                    ==== t9._8to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.revalue("i")(0)                                    ==== t9._9to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.redo("a")(0 \ "_")                                 ==== t9._1to(0) --: typed[(Int \ "_", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.redo("b")(0 \ "_")                                 ==== t9._2to(0) --: typed[(Int \ "a", Int \ "_", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.redo("c")(0 \ "_")                                 ==== t9._3to(0) --: typed[(Int \ "a", Int \ "b", Int \ "_", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.redo("d")(0 \ "_")                                 ==== t9._4to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "_", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.redo("e")(0 \ "_")                                 ==== t9._5to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "_", Int \ "f", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.redo("f")(0 \ "_")                                 ==== t9._6to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "_", Int \ "g", Int \ "h", Int \ "i")]
    T ~ l9.redo("g")(0 \ "_")                                 ==== t9._7to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "_", Int \ "h", Int \ "i")]
    T ~ l9.redo("h")(0 \ "_")                                 ==== t9._8to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "_", Int \ "i")]
    T ~ l9.redo("i")(0 \ "_")                                 ==== t9._9to(0) --: typed[(Int \ "a", Int \ "b", Int \ "c", Int \ "d", Int \ "e", Int \ "f", Int \ "g", Int \ "h", Int \ "_")]

    val x12 = (1 \ "a", 2 \ "a", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    val x13 = (1 \ "a", 2 \ "b", 3 \ "a", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    val x14 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "a", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    val x15 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "a", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    val x16 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "a", 7 \ "g", 8 \ "h", 9 \ "i")
    val x17 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "a", 8 \ "h", 9 \ "i")
    val x18 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "a", 9 \ "i")
    val x19 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "a")
    val x23 = (1 \ "a", 2 \ "b", 3 \ "b", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    val x24 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "b", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    val x25 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "b", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    val x26 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "b", 7 \ "g", 8 \ "h", 9 \ "i")
    val x27 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "b", 8 \ "h", 9 \ "i")
    val x28 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "b", 9 \ "i")
    val x29 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "b")
    val x34 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "c", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    val x35 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "c", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    val x36 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "c", 7 \ "g", 8 \ "h", 9 \ "i")
    val x37 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "c", 8 \ "h", 9 \ "i")
    val x38 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "c", 9 \ "i")
    val x39 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "c")
    val x45 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "d", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "i")
    val x46 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "d", 7 \ "g", 8 \ "h", 9 \ "i")
    val x47 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "d", 8 \ "h", 9 \ "i")
    val x48 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "d", 9 \ "i")
    val x49 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "d")
    val x56 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "e", 7 \ "g", 8 \ "h", 9 \ "i")
    val x57 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "e", 8 \ "h", 9 \ "i")
    val x58 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "e", 9 \ "i")
    val x59 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "e")
    val x67 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "f", 8 \ "h", 9 \ "i")
    val x68 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "f", 9 \ "i")
    val x69 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "f")
    val x78 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "g", 9 \ "i")
    val x79 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "g")
    val x89 = (1 \ "a", 2 \ "b", 3 \ "c", 4 \ "d", 5 \ "e", 6 \ "f", 7 \ "g", 8 \ "h", 9 \ "h")
    T ~ cc("""t9.label["a", "a", "c", "d", "e", "f", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "a", "d", "e", "f", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "a", "e", "f", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "a", "f", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "a", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "a", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "a", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "h", "a"]""") ==== false
    T ~ cc("""t9.label["a", "b", "b", "d", "e", "f", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "b", "e", "f", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "b", "f", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "b", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "b", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "b", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "h", "b"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "c", "e", "f", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "c", "f", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "c", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "c", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "c", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "h", "c"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "d", "f", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "d", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "d", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "d", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "h", "d"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "e", "g", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "e", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "e", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "h", "e"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "f", "h", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "f", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "h", "f"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "g", "i"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "h", "g"]""") ==== false
    T ~ cc("""t9.label["a", "b", "c", "d", "e", "f", "g", "h", "h"]""") ==== false
    T ~ cc("""t9 \\ ("a", "a", "c", "d", "e", "f", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "a", "d", "e", "f", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "a", "e", "f", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "a", "f", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "a", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "a", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "a", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "h", "a")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "b", "d", "e", "f", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "b", "e", "f", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "b", "f", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "b", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "b", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "b", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "h", "b")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "c", "e", "f", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "c", "f", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "c", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "c", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "c", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "h", "c")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "d", "f", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "d", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "d", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "d", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "h", "d")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "e", "g", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "e", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "e", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "h", "e")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "f", "h", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "f", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "h", "f")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "g", "i")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "h", "g")""")   ==== false
    T ~ cc("""t9 \\ ("a", "b", "c", "d", "e", "f", "g", "h", "h")""")   ==== false
    T ~ cc("""l9 ~~ ("_", "b", "c", "d", "e", "f", "e", "h", "i")""")   ==== false
    T ~ cc("""l9 ~~ ("a", "_", "c", "d", "e", "f", "g", "e", "i")""")   ==== false
    T ~ cc("""l9 ~~ ("a", "b", "_", "d", "e", "f", "g", "h", "e")""")   ==== false
    T ~ cc("""l9 ~~ ("a", "b", "c", "_", "e", "f", "f", "h", "i")""")   ==== false
    T ~ cc("""l9 ~~ ("a", "b", "c", "d", "_", "f", "g", "f", "i")""")   ==== false
    T ~ cc("""l9 ~~ ("a", "b", "c", "d", "e", "_", "g", "h", "f")""")   ==== false
    T ~ cc("""l9 ~~ ("a", "b", "c", "d", "e", "f", "_", "g", "i")""")   ==== false
    T ~ cc("""l9 ~~ ("a", "b", "c", "d", "e", "f", "g", "_", "g")""")   ==== false
    T ~ cc("""l9 ~~ ("a", "b", "c", "d", "e", "f", "g", "h", "_")""")   ==== false

    T ~ cc("""l9 ~ "_"""")              ==== false
    T ~ cc("""l9.relabel("_")("=")""")  ==== false
    T ~ cc("""l9.revalue("_")(0)""")    ==== false
    T ~ cc("""l9.redo("_")(0 \ "=")""") ==== false

    T ~ cc("""x12 ~ "a"""")              ==== false
    T ~ cc("""x13 ~ "a"""")              ==== false
    T ~ cc("""x14 ~ "a"""")              ==== false
    T ~ cc("""x15 ~ "a"""")              ==== false
    T ~ cc("""x16 ~ "a"""")              ==== false
    T ~ cc("""x17 ~ "a"""")              ==== false
    T ~ cc("""x18 ~ "a"""")              ==== false
    T ~ cc("""x19 ~ "a"""")              ==== false
    T ~ cc("""x23 ~ "b"""")              ==== false
    T ~ cc("""x24 ~ "b"""")              ==== false
    T ~ cc("""x25 ~ "b"""")              ==== false
    T ~ cc("""x26 ~ "b"""")              ==== false
    T ~ cc("""x27 ~ "b"""")              ==== false
    T ~ cc("""x28 ~ "b"""")              ==== false
    T ~ cc("""x29 ~ "b"""")              ==== false
    T ~ cc("""x34 ~ "c"""")              ==== false
    T ~ cc("""x35 ~ "c"""")              ==== false
    T ~ cc("""x36 ~ "c"""")              ==== false
    T ~ cc("""x37 ~ "c"""")              ==== false
    T ~ cc("""x38 ~ "c"""")              ==== false
    T ~ cc("""x39 ~ "c"""")              ==== false
    T ~ cc("""x45 ~ "d"""")              ==== false
    T ~ cc("""x46 ~ "d"""")              ==== false
    T ~ cc("""x47 ~ "d"""")              ==== false
    T ~ cc("""x48 ~ "d"""")              ==== false
    T ~ cc("""x49 ~ "d"""")              ==== false
    T ~ cc("""x56 ~ "e"""")              ==== false
    T ~ cc("""x57 ~ "e"""")              ==== false
    T ~ cc("""x58 ~ "e"""")              ==== false
    T ~ cc("""x59 ~ "e"""")              ==== false
    T ~ cc("""x67 ~ "f"""")              ==== false
    T ~ cc("""x68 ~ "f"""")              ==== false
    T ~ cc("""x69 ~ "f"""")              ==== false
    T ~ cc("""x78 ~ "g"""")              ==== false
    T ~ cc("""x79 ~ "g"""")              ==== false
    T ~ cc("""x89 ~ "h"""")              ==== false
    T ~ cc("""x12().a""")                ==== false
    T ~ cc("""x13().a""")                ==== false
    T ~ cc("""x14().a""")                ==== false
    T ~ cc("""x15().a""")                ==== false
    T ~ cc("""x16().a""")                ==== false
    T ~ cc("""x17().a""")                ==== false
    T ~ cc("""x18().a""")                ==== false
    T ~ cc("""x19().a""")                ==== false
    T ~ cc("""x23().b""")                ==== false
    T ~ cc("""x24().b""")                ==== false
    T ~ cc("""x25().b""")                ==== false
    T ~ cc("""x26().b""")                ==== false
    T ~ cc("""x27().b""")                ==== false
    T ~ cc("""x28().b""")                ==== false
    T ~ cc("""x29().b""")                ==== false
    T ~ cc("""x34().c""")                ==== false
    T ~ cc("""x35().c""")                ==== false
    T ~ cc("""x36().c""")                ==== false
    T ~ cc("""x37().c""")                ==== false
    T ~ cc("""x38().c""")                ==== false
    T ~ cc("""x39().c""")                ==== false
    T ~ cc("""x45().d""")                ==== false
    T ~ cc("""x46().d""")                ==== false
    T ~ cc("""x47().d""")                ==== false
    T ~ cc("""x48().d""")                ==== false
    T ~ cc("""x49().d""")                ==== false
    T ~ cc("""x56().e""")                ==== false
    T ~ cc("""x57().e""")                ==== false
    T ~ cc("""x58().e""")                ==== false
    T ~ cc("""x59().e""")                ==== false
    T ~ cc("""x67().f""")                ==== false
    T ~ cc("""x68().f""")                ==== false
    T ~ cc("""x69().f""")                ==== false
    T ~ cc("""x78().g""")                ==== false
    T ~ cc("""x79().g""")                ==== false
    T ~ cc("""x89().h""")                ==== false
    T ~ cc("""x12.relabel("a")("=")""")  ==== false
    T ~ cc("""x13.relabel("a")("=")""")  ==== false
    T ~ cc("""x14.relabel("a")("=")""")  ==== false
    T ~ cc("""x15.relabel("a")("=")""")  ==== false
    T ~ cc("""x16.relabel("a")("=")""")  ==== false
    T ~ cc("""x17.relabel("a")("=")""")  ==== false
    T ~ cc("""x18.relabel("a")("=")""")  ==== false
    T ~ cc("""x19.relabel("a")("=")""")  ==== false
    T ~ cc("""x23.relabel("b")("=")""")  ==== false
    T ~ cc("""x24.relabel("b")("=")""")  ==== false
    T ~ cc("""x25.relabel("b")("=")""")  ==== false
    T ~ cc("""x26.relabel("b")("=")""")  ==== false
    T ~ cc("""x27.relabel("b")("=")""")  ==== false
    T ~ cc("""x28.relabel("b")("=")""")  ==== false
    T ~ cc("""x29.relabel("b")("=")""")  ==== false
    T ~ cc("""x34.relabel("c")("=")""")  ==== false
    T ~ cc("""x35.relabel("c")("=")""")  ==== false
    T ~ cc("""x36.relabel("c")("=")""")  ==== false
    T ~ cc("""x37.relabel("c")("=")""")  ==== false
    T ~ cc("""x38.relabel("c")("=")""")  ==== false
    T ~ cc("""x39.relabel("c")("=")""")  ==== false
    T ~ cc("""x45.relabel("d")("=")""")  ==== false
    T ~ cc("""x46.relabel("d")("=")""")  ==== false
    T ~ cc("""x47.relabel("d")("=")""")  ==== false
    T ~ cc("""x48.relabel("d")("=")""")  ==== false
    T ~ cc("""x49.relabel("d")("=")""")  ==== false
    T ~ cc("""x56.relabel("e")("=")""")  ==== false
    T ~ cc("""x57.relabel("e")("=")""")  ==== false
    T ~ cc("""x58.relabel("e")("=")""")  ==== false
    T ~ cc("""x59.relabel("e")("=")""")  ==== false
    T ~ cc("""x67.relabel("f")("=")""")  ==== false
    T ~ cc("""x68.relabel("f")("=")""")  ==== false
    T ~ cc("""x69.relabel("f")("=")""")  ==== false
    T ~ cc("""x78.relabel("g")("=")""")  ==== false
    T ~ cc("""x79.relabel("g")("=")""")  ==== false
    T ~ cc("""x89.relabel("h")("=")""")  ==== false
    T ~ cc("""x12.revalue("a")(0 )""")   ==== false
    T ~ cc("""x13.revalue("a")(0 )""")   ==== false
    T ~ cc("""x14.revalue("a")(0 )""")   ==== false
    T ~ cc("""x15.revalue("a")(0 )""")   ==== false
    T ~ cc("""x16.revalue("a")(0 )""")   ==== false
    T ~ cc("""x17.revalue("a")(0 )""")   ==== false
    T ~ cc("""x18.revalue("a")(0 )""")   ==== false
    T ~ cc("""x19.revalue("a")(0 )""")   ==== false
    T ~ cc("""x23.revalue("b")(0 )""")   ==== false
    T ~ cc("""x24.revalue("b")(0 )""")   ==== false
    T ~ cc("""x25.revalue("b")(0 )""")   ==== false
    T ~ cc("""x26.revalue("b")(0 )""")   ==== false
    T ~ cc("""x27.revalue("b")(0 )""")   ==== false
    T ~ cc("""x28.revalue("b")(0 )""")   ==== false
    T ~ cc("""x29.revalue("b")(0 )""")   ==== false
    T ~ cc("""x34.revalue("c")(0 )""")   ==== false
    T ~ cc("""x35.revalue("c")(0 )""")   ==== false
    T ~ cc("""x36.revalue("c")(0 )""")   ==== false
    T ~ cc("""x37.revalue("c")(0 )""")   ==== false
    T ~ cc("""x38.revalue("c")(0 )""")   ==== false
    T ~ cc("""x39.revalue("c")(0 )""")   ==== false
    T ~ cc("""x45.revalue("d")(0 )""")   ==== false
    T ~ cc("""x46.revalue("d")(0 )""")   ==== false
    T ~ cc("""x47.revalue("d")(0 )""")   ==== false
    T ~ cc("""x48.revalue("d")(0 )""")   ==== false
    T ~ cc("""x49.revalue("d")(0 )""")   ==== false
    T ~ cc("""x56.revalue("e")(0 )""")   ==== false
    T ~ cc("""x57.revalue("e")(0 )""")   ==== false
    T ~ cc("""x58.revalue("e")(0 )""")   ==== false
    T ~ cc("""x59.revalue("e")(0 )""")   ==== false
    T ~ cc("""x67.revalue("f")(0 )""")   ==== false
    T ~ cc("""x68.revalue("f")(0 )""")   ==== false
    T ~ cc("""x69.revalue("f")(0 )""")   ==== false
    T ~ cc("""x78.revalue("g")(0 )""")   ==== false
    T ~ cc("""x79.revalue("g")(0 )""")   ==== false
    T ~ cc("""x89.revalue("h")(0 )""")   ==== false
    T ~ cc("""x12.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x13.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x14.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x15.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x16.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x17.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x18.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x19.redo("a")(0 \ "=")""") ==== false
    T ~ cc("""x23.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x24.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x25.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x26.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x27.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x28.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x29.redo("b")(0 \ "=")""") ==== false
    T ~ cc("""x34.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x35.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x36.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x37.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x38.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x39.redo("c")(0 \ "=")""") ==== false
    T ~ cc("""x45.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x46.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x47.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x48.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x49.redo("d")(0 \ "=")""") ==== false
    T ~ cc("""x56.redo("e")(0 \ "=")""") ==== false
    T ~ cc("""x57.redo("e")(0 \ "=")""") ==== false
    T ~ cc("""x58.redo("e")(0 \ "=")""") ==== false
    T ~ cc("""x59.redo("e")(0 \ "=")""") ==== false
    T ~ cc("""x67.redo("f")(0 \ "=")""") ==== false
    T ~ cc("""x68.redo("f")(0 \ "=")""") ==== false
    T ~ cc("""x69.redo("f")(0 \ "=")""") ==== false
    T ~ cc("""x78.redo("g")(0 \ "=")""") ==== false
    T ~ cc("""x79.redo("g")(0 \ "=")""") ==== false
    T ~ cc("""x89.redo("h")(0 \ "=")""") ==== false

    T ~ cc("""l9.relabel("a")("b")""")  ==== false
    T ~ cc("""l9.relabel("a")("c")""")  ==== false
    T ~ cc("""l9.relabel("a")("d")""")  ==== false
    T ~ cc("""l9.relabel("a")("e")""")  ==== false
    T ~ cc("""l9.relabel("a")("f")""")  ==== false
    T ~ cc("""l9.relabel("a")("g")""")  ==== false
    T ~ cc("""l9.relabel("a")("h")""")  ==== false
    T ~ cc("""l9.relabel("a")("i")""")  ==== false
    T ~ cc("""l9.relabel("b")("a")""")  ==== false
    T ~ cc("""l9.relabel("b")("c")""")  ==== false
    T ~ cc("""l9.relabel("b")("d")""")  ==== false
    T ~ cc("""l9.relabel("b")("e")""")  ==== false
    T ~ cc("""l9.relabel("b")("f")""")  ==== false
    T ~ cc("""l9.relabel("b")("g")""")  ==== false
    T ~ cc("""l9.relabel("b")("h")""")  ==== false
    T ~ cc("""l9.relabel("b")("i")""")  ==== false
    T ~ cc("""l9.relabel("c")("a")""")  ==== false
    T ~ cc("""l9.relabel("c")("b")""")  ==== false
    T ~ cc("""l9.relabel("c")("d")""")  ==== false
    T ~ cc("""l9.relabel("c")("e")""")  ==== false
    T ~ cc("""l9.relabel("c")("f")""")  ==== false
    T ~ cc("""l9.relabel("c")("g")""")  ==== false
    T ~ cc("""l9.relabel("c")("h")""")  ==== false
    T ~ cc("""l9.relabel("c")("i")""")  ==== false
    T ~ cc("""l9.relabel("d")("a")""")  ==== false
    T ~ cc("""l9.relabel("d")("b")""")  ==== false
    T ~ cc("""l9.relabel("d")("c")""")  ==== false
    T ~ cc("""l9.relabel("d")("e")""")  ==== false
    T ~ cc("""l9.relabel("d")("f")""")  ==== false
    T ~ cc("""l9.relabel("d")("g")""")  ==== false
    T ~ cc("""l9.relabel("d")("h")""")  ==== false
    T ~ cc("""l9.relabel("d")("i")""")  ==== false
    T ~ cc("""l9.relabel("e")("a")""")  ==== false
    T ~ cc("""l9.relabel("e")("b")""")  ==== false
    T ~ cc("""l9.relabel("e")("c")""")  ==== false
    T ~ cc("""l9.relabel("e")("d")""")  ==== false
    T ~ cc("""l9.relabel("e")("f")""")  ==== false
    T ~ cc("""l9.relabel("e")("g")""")  ==== false
    T ~ cc("""l9.relabel("e")("h")""")  ==== false
    T ~ cc("""l9.relabel("e")("i")""")  ==== false
    T ~ cc("""l9.relabel("f")("a")""")  ==== false
    T ~ cc("""l9.relabel("f")("b")""")  ==== false
    T ~ cc("""l9.relabel("f")("c")""")  ==== false
    T ~ cc("""l9.relabel("f")("d")""")  ==== false
    T ~ cc("""l9.relabel("f")("e")""")  ==== false
    T ~ cc("""l9.relabel("f")("g")""")  ==== false
    T ~ cc("""l9.relabel("f")("h")""")  ==== false
    T ~ cc("""l9.relabel("f")("i")""")  ==== false
    T ~ cc("""l9.relabel("g")("a")""")  ==== false
    T ~ cc("""l9.relabel("g")("b")""")  ==== false
    T ~ cc("""l9.relabel("g")("c")""")  ==== false
    T ~ cc("""l9.relabel("g")("d")""")  ==== false
    T ~ cc("""l9.relabel("g")("e")""")  ==== false
    T ~ cc("""l9.relabel("g")("f")""")  ==== false
    T ~ cc("""l9.relabel("g")("h")""")  ==== false
    T ~ cc("""l9.relabel("g")("i")""")  ==== false
    T ~ cc("""l9.relabel("h")("a")""")  ==== false
    T ~ cc("""l9.relabel("h")("b")""")  ==== false
    T ~ cc("""l9.relabel("h")("c")""")  ==== false
    T ~ cc("""l9.relabel("h")("d")""")  ==== false
    T ~ cc("""l9.relabel("h")("e")""")  ==== false
    T ~ cc("""l9.relabel("h")("f")""")  ==== false
    T ~ cc("""l9.relabel("h")("g")""")  ==== false
    T ~ cc("""l9.relabel("h")("i")""")  ==== false
    T ~ cc("""l9.relabel("i")("a")""")  ==== false
    T ~ cc("""l9.relabel("i")("b")""")  ==== false
    T ~ cc("""l9.relabel("i")("c")""")  ==== false
    T ~ cc("""l9.relabel("i")("d")""")  ==== false
    T ~ cc("""l9.relabel("i")("e")""")  ==== false
    T ~ cc("""l9.relabel("i")("f")""")  ==== false
    T ~ cc("""l9.relabel("i")("g")""")  ==== false
    T ~ cc("""l9.relabel("i")("h")""")  ==== false
    T ~ cc("""l9.redo("a")(0 \ "b")""") ==== false
    T ~ cc("""l9.redo("a")(0 \ "c")""") ==== false
    T ~ cc("""l9.redo("a")(0 \ "d")""") ==== false
    T ~ cc("""l9.redo("a")(0 \ "e")""") ==== false
    T ~ cc("""l9.redo("a")(0 \ "f")""") ==== false
    T ~ cc("""l9.redo("a")(0 \ "g")""") ==== false
    T ~ cc("""l9.redo("a")(0 \ "h")""") ==== false
    T ~ cc("""l9.redo("a")(0 \ "i")""") ==== false
    T ~ cc("""l9.redo("b")(0 \ "a")""") ==== false
    T ~ cc("""l9.redo("b")(0 \ "c")""") ==== false
    T ~ cc("""l9.redo("b")(0 \ "d")""") ==== false
    T ~ cc("""l9.redo("b")(0 \ "e")""") ==== false
    T ~ cc("""l9.redo("b")(0 \ "f")""") ==== false
    T ~ cc("""l9.redo("b")(0 \ "g")""") ==== false
    T ~ cc("""l9.redo("b")(0 \ "h")""") ==== false
    T ~ cc("""l9.redo("b")(0 \ "i")""") ==== false
    T ~ cc("""l9.redo("c")(0 \ "a")""") ==== false
    T ~ cc("""l9.redo("c")(0 \ "b")""") ==== false
    T ~ cc("""l9.redo("c")(0 \ "d")""") ==== false
    T ~ cc("""l9.redo("c")(0 \ "e")""") ==== false
    T ~ cc("""l9.redo("c")(0 \ "f")""") ==== false
    T ~ cc("""l9.redo("c")(0 \ "g")""") ==== false
    T ~ cc("""l9.redo("c")(0 \ "h")""") ==== false
    T ~ cc("""l9.redo("c")(0 \ "i")""") ==== false
    T ~ cc("""l9.redo("d")(0 \ "a")""") ==== false
    T ~ cc("""l9.redo("d")(0 \ "b")""") ==== false
    T ~ cc("""l9.redo("d")(0 \ "c")""") ==== false
    T ~ cc("""l9.redo("d")(0 \ "e")""") ==== false
    T ~ cc("""l9.redo("d")(0 \ "f")""") ==== false
    T ~ cc("""l9.redo("d")(0 \ "g")""") ==== false
    T ~ cc("""l9.redo("d")(0 \ "h")""") ==== false
    T ~ cc("""l9.redo("d")(0 \ "i")""") ==== false
    T ~ cc("""l9.redo("e")(0 \ "a")""") ==== false
    T ~ cc("""l9.redo("e")(0 \ "b")""") ==== false
    T ~ cc("""l9.redo("e")(0 \ "c")""") ==== false
    T ~ cc("""l9.redo("e")(0 \ "d")""") ==== false
    T ~ cc("""l9.redo("e")(0 \ "f")""") ==== false
    T ~ cc("""l9.redo("e")(0 \ "g")""") ==== false
    T ~ cc("""l9.redo("e")(0 \ "h")""") ==== false
    T ~ cc("""l9.redo("e")(0 \ "i")""") ==== false
    T ~ cc("""l9.redo("f")(0 \ "a")""") ==== false
    T ~ cc("""l9.redo("f")(0 \ "b")""") ==== false
    T ~ cc("""l9.redo("f")(0 \ "c")""") ==== false
    T ~ cc("""l9.redo("f")(0 \ "d")""") ==== false
    T ~ cc("""l9.redo("f")(0 \ "e")""") ==== false
    T ~ cc("""l9.redo("f")(0 \ "g")""") ==== false
    T ~ cc("""l9.redo("f")(0 \ "h")""") ==== false
    T ~ cc("""l9.redo("f")(0 \ "i")""") ==== false
    T ~ cc("""l9.redo("g")(0 \ "a")""") ==== false
    T ~ cc("""l9.redo("g")(0 \ "b")""") ==== false
    T ~ cc("""l9.redo("g")(0 \ "c")""") ==== false
    T ~ cc("""l9.redo("g")(0 \ "d")""") ==== false
    T ~ cc("""l9.redo("g")(0 \ "e")""") ==== false
    T ~ cc("""l9.redo("g")(0 \ "f")""") ==== false
    T ~ cc("""l9.redo("g")(0 \ "h")""") ==== false
    T ~ cc("""l9.redo("g")(0 \ "i")""") ==== false
    T ~ cc("""l9.redo("h")(0 \ "a")""") ==== false
    T ~ cc("""l9.redo("h")(0 \ "b")""") ==== false
    T ~ cc("""l9.redo("h")(0 \ "c")""") ==== false
    T ~ cc("""l9.redo("h")(0 \ "d")""") ==== false
    T ~ cc("""l9.redo("h")(0 \ "e")""") ==== false
    T ~ cc("""l9.redo("h")(0 \ "f")""") ==== false
    T ~ cc("""l9.redo("h")(0 \ "g")""") ==== false
    T ~ cc("""l9.redo("h")(0 \ "i")""") ==== false
    T ~ cc("""l9.redo("i")(0 \ "a")""") ==== false
    T ~ cc("""l9.redo("i")(0 \ "b")""") ==== false
    T ~ cc("""l9.redo("i")(0 \ "c")""") ==== false
    T ~ cc("""l9.redo("i")(0 \ "d")""") ==== false
    T ~ cc("""l9.redo("i")(0 \ "e")""") ==== false
    T ~ cc("""l9.redo("i")(0 \ "f")""") ==== false
    T ~ cc("""l9.redo("i")(0 \ "g")""") ==== false
    T ~ cc("""l9.redo("i")(0 \ "h")""") ==== false
}
