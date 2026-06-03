// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025-26 Rex Kerr and Calico Life Sciences, LLC.

package kse.test.loom


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


@RunWith(classOf[JUnit4])
class LoomTest {
  import kse.testutilities.TestUtilities.{_, given}
  import kse.basics.{given, _}
  import kse.basics.intervals.*
  import kse.flow.{_, given}
  import kse.loom.*

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )



  @Test
  def loomTest(): Unit =
    import java.util.concurrent.atomic.{AtomicLong, AtomicInteger}
    extension (ai: AtomicInteger)
      def ++ : Unit = ai.getAndIncrement __ Unit
      def :=(i: Integer): Unit = ai.getAndSet(i) __ Unit
    val dt = new AtomicLong(0L)
    def time[A](t: => A): A =
      val t0 = System.nanoTime
      val ans = t
      val t1 = System.nanoTime
      dt.set(t1 - t0)
      ans
    def yikes(s: String): Nothing =
      throw new Exception(s)
    def zzzz(n: Int, increment: Int = Int.MaxValue): Unit =
      val t0 = System.nanoTime
      var remaining = n.toLong * 1000000L
      while remaining > 0 do
        Thread.sleep(1 max (increment min (remaining/1000000L).toInt))
        remaining = (t0 + n.toLong * 1000000L) - System.nanoTime
    val n = AtomicInteger(0)
    T ~ Fu{ n.++; "eel" }.await()  ==== "eel" --: typed[String Or Err]
    T ~ n.get                       ==== 1
    T ~ Fu.flat{ n.++; Is("eel") }.await() ==== "eel" --: typed[String Or Err]
    T ~ n.get                       ==== 2
    val fex = Fu.Executor.create()
    val foo = Fu(using fex){ zzzz(50); n.++; 4 }
    T ~ foo.isComplete ==== false
    T ~ foo.await()      ==== Is(4)
    T ~ foo.isComplete ==== true
    T ~ n.get          ==== 3
    fex.unwrap.close()
    val sluggish = Fu{ zzzz(100, 1); 100 }
    T ~ time{ zzzz(1); sluggish.cancel(): Unit; sluggish.await() } ==== runtype[Alt[?]]
    T ~ { dt.get() < 50000000L } ==== true
    val alnum = "abcdefghijklmnopqrstuvwxyzABCDEFHIJKLMNOPQRSTUVWXYZ0123456789"
    val fs = alnum.arr.map(c => Fu{ zzzz(100); n.++; c })
    val ans: Array[Char] Or Err = time{ fs.fu().await() }
    T ~ (dt.get/1e9 > 0.05)  ==== true
    T ~ (dt.get/1e9 < 0.15)  ==== true
    T ~ ans.map(_.mkString)  ==== alnum --: typed[String Or Err]
    T ~ fs.fu().await().get =**= alnum.arr.copyWith(x => x.orAlt[Err])
    T ~ n.get               ==== 3 + alnum.length
    T ~ Fu.flat{ nice{ "1".toInt }    }.map(_ * 3).await() ==== 3 --: typed[Int Or Err]
    T ~ Fu.flat{ Err("eel").orIs[Int] }.map(_ * 3).await() ==== Alt(Err("eel"))
    T ~ Fu.flat{ nice{ "1".toInt } }.map(x => 5/(x-1)).await().isAlt ==== true
    T ~ Fu.flat{ nice{ "1".toInt }    }.flatMap(n => (n+n).orAlt[Err]    ).await() ==== 2 --: typed[Int Or Err]
    T ~ Fu.flat{ nice{ "1".toInt }    }.flatMap(n => Err("cod").orIs[Int]).await() ==== Alt(Err("cod"))
    T ~ Fu.flat{ nice{ "1".toInt }    }.flatMap(n => (5/(n-1)).orAlt[Err]).await() ==== runtype[Alt[?]]
    T ~ Fu.flat{ Err("eel").orIs[Int] }.flatMap(n => (n+n).orAlt[Err]    ).await() ==== Alt(Err("eel"))
    T ~ Fu.flat{ Err("eel").orIs[Int] }.flatMap(n => yikes("salmon")     ).await() ==== Alt(Err("eel"))
    T ~ Ask[Int]{ Fu{ "eel".length }.? * 3 } ==== 9 --: typed[Int Or Err]
    T ~ Ask[Int]{ Fu{ "eel".toInt  }.? * 3 } ==== runtype[Alt[?]]
    T ~ Or.Ret[Int, String]{ Fu{ "eel".length }.?+(_.toString) * 2 } ==== 6 --: typed[Int Or String]
    T ~ Or.Ret[Int, String]{ Fu{ "eel".toInt  }.?+(_.toString) * 2 } ==== runtype[Alt[?]]
    given AutoMap[Err, Char] = e => e.toString.fn(s => if s.length > 0 then '+' else '-')
    T ~ Or.Ret[Int, Char]{ Fu{ "eel".length }.?* + 4 } ==== 7 --: typed[Int Or Char]
    T ~ Or.Ret[Int, Char]{ Fu{ "eel".toInt }.?* + 4 }  ==== Alt('+')
    T ~ Ask[Int]{ Fu{ "eel".length }.?#("Yo") / 2 } ==== 1 --: typed[Int Or Err]
    T ~ Ask[Int]{ Fu{ "eel".toInt  }.?#("Yo") / 2 }.alt.toString.take(2) ==== "Yo"
    T ~ Fu.flat{ nice{ Fu{ "eel".length }.? + 2 } }.await() ==== 5
    T ~ Fu.flat{ nice{ Fu{ "eel".toInt  }.? + 2 } }.await() ==== runtype[Alt[?]]
    T ~ Fu{ Fu{ "eel".length }.?#("Yo") + 1 }.await() ==== 4
    T ~ Fu{ Fu{ "eel".toInt  }.?#("Yo") + 1 }.await().alt.toString.take(2) ==== "Yo"
    T ~ Fu{ "eel".length }.map{ x => Fu{ x*x }.? - 1 }.await() ==== 8
    T ~ Fu{ "eel".length }.map{ x => Fu{ "e"(x)}.? - 1 }.await() ==== runtype[Alt[?]]
    T ~ Fu{ "eel".length }.flatMap{ x => (Fu{ x*x }.? + 1).orAlt[Err] }.await() ==== 10
    T ~ Fu{ "eel".length }.flatMap{ x => (Fu{ "e"(x)}.? + 1).orAlt[Err] }.await() ==== runtype[Alt[?]]
    def fus(): Array[Fu[Int]] = Array(Fu{ "eel".length }, Fu{ "eel".toInt }, Fu.flat{ nice{ "bass".length } }, Fu.flat{ nice{ "bass".toInt } })
    T ~ fus().allFu().await().get.map(_.mapAlt(_ => Err("cod")))  =**= fus().map(_.await()).map(_.mapAlt(_ => Err("cod")))
    T ~ fus().fu().await().alt.toString.diced(_ == '\n')(0) ==== "Multiple errors found (2)"
    T ~ fus().fuMap(n => 14/(n-3)).allFu().await().get.map(_.fold(_.abs)(_ => -1)) =**= Array(-1, -1, 14, -1)
    T ~ fus().fuFlatMap(n => nice{ "123abc".take(n).toInt }).allFu().await().get.map(_.fold(_.abs)(_ => -1)) =**= Array(123, -1, -1 ,-1)
    val v1 = new AtomicInteger(0)
    val v2 = new AtomicInteger(0)
    val v3 = new AtomicInteger(0)
    val v4 = new AtomicInteger(0)
    val ten = Fu.group:
      val one = Fu{ v1 := 1 ; 1 }
      val two = Fu{ Thread.sleep(20); v2 := 2; 2 }
      val three = Fu{ Thread.sleep(40); v3 := 4; nice{ "three".toInt }.? }
      val four = Fu{ Thread.sleep(60); v4 := 8; 4 }
      one.? + two.? + three.? + four.?
    T ~ ten.await() ==== runtype[Alt[?]]
    T ~ { Thread.sleep(80); v1.get + v2.get + v3.get + v4.get } ==== 7
    val thirty = Fu.flatGroup:
      val six = Fu{ v1 := 16 ; 6 }
      val seven = Fu.flat{ Thread.sleep(20); v2 := 32; nice{ "seven".toInt } }
      val eight = Fu{ Thread.sleep(40); v3 := 64; 8 }
      val nine = Fu{ Thread.sleep(60); v4 := 128; 9 }
      (nine.? + eight.? + seven.? + six.?).orErr
    T ~ { Thread.sleep(80); v1.get + v2.get + v3.get + v4.get } ==== 52
    T ~ thirty.await() ==== runtype[Alt[?]]
}
object LoomTest {
  // @BeforeClass
  // def before(): Unit = { println("Before") }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
