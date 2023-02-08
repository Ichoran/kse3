// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-23 Rex Kerr and Calico Life Sciences, LLC.

package kse.flow.bench


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import scala.reflect.{ClassTag, TypeTest}
import scala.util.control.ControlThrowable

import kse.flow._


/** This is a terrible benchmarking framework.
 *
 *  But it's very easy to deploy despite an absence of an internal benchmarking framework.
 *  Eventually that will happen and it will be nice.  For now, use at your own risk, and
 *  doubt everything!  (Use JMH for anything important.  This is just to get a clue as to
 *  whether we might be on the right track.)
 */
object Bench {

  class Nicely() {

    def foldly(): Unit =
      val a = 5.orAlt[String]
      println(a)
      val b = a.fold(_ + 10)(_.length)
      println(b)

    def maply(): Unit =
      val a = 5.orAlt[String]
      println(a)
      val b = a.map(_ + 10)
      println(b)

    def altly(): Unit =
      val a = 5.orAlt[String]
      println(a)
      val b = a.mapAlt(_.length)
      println(b)

    def nicely(): Unit =
      val a = Is(5)
      val b = Is("salmon")
      val c = Alt("salmon")
      val d = Is(c)
      val e: Any = c
      val f = Is(e)
      println(a)
      println(b)
      println(c)
      println(d)
      println(e)
      println(f)
  }

}

@RunWith(classOf[JUnit4])
class FlowBench {
  import Bench._

  def time[A](a: => A): (A, Double) =
    val t0 = System.nanoTime
    val value = a
    val t1= System.nanoTime
    (a, (t1 - t0)/1e9)

  def bestEitherString(input: Array[String]): Either[String, String] =
    var result: Either[String, String] = Right("")
    var i = 0
    while i < input.length do
      val s = input(i)
      result = result.flatMap{ t =>
        if (s.isEmpty) Left(s)
        else if (t.length != s.length) Right(s)
        else result
      }
      i += 1
    result

  def bestOrString(input: Array[String]): String Or String =
    var result: String Or String = Is("")
    var i = 0
    while i < input.length do
      val s = input(i)
      result = result.flatMap{ t =>
        if (s.isEmpty) Alt(s)
        else if (t.length != s.length) Is(s)
        else result
      }
      i += 1
    result

  def bestAnyRefString(input: Array[String]): AnyRef =
    var result: AnyRef = ""
    input.foreach{ s =>
      result = result match
        case t: String =>
          if (s.isEmpty) Some(s)
          else if (t.length != s.length) s
          else result
        case _ => result
    }
    result


  def stringBench(): Unit = {
    val timings = new Array[Double](80)
    val eithers = new Array[Either[String, String]](timings.length/3)
    val ors = new Array[String Or String](timings.length/3)
    val grounds = new Array[AnyRef](timings.length/3)
    for (i <- 0 until timings.length/3) {
      val (e, et) = time(bestEitherString(FlowBench.theStrings))
      val (o, ot) = time(bestOrString(    FlowBench.theStrings))
      val (g, gt) = time(bestAnyRefString(FlowBench.theStrings))
      eithers(i) = e
      ors(i) = o
      grounds(i) = g
      timings(3*i) = et
      timings(3*i+1) = ot
      timings(3*i+2) = gt
    }
    ((eithers zip ors) zip grounds).foreach(println _)
    for (i <- 0 until timings.length/3) {
      println(f"${timings(3*i)}%6.3f   vs  ${timings(3*i+1)}%6.3f   vs  ${timings(3*i+2)}%6.3f")
    }
  }

  def bestEitherSum(input: Array[Int], zero: Either[String, Int]): Either[String, Int] =
    var result = zero
    var i = 0
    while i < input.length do
      val v = input(i)
      result = result.map(_ + v)
      i += 1
    result

  def bestOrSum(input: Array[Int], zero: Int Or String): Int Or String =
    var result = zero
    var i = 0
    while i < input.length do
      val v = input(i)
      result = result.map(_ + v)
      i += 1
    result

  def bestAnyRefSum(input: Array[Int], zero: AnyRef): AnyRef =
    var result: Any = zero
    var i = 0
    while i < input.length do
      val v = input(i)
      result = result match
        case i: Int => (i + v).asInstanceOf[AnyRef]
        case _ => result
      i += 1
    result.asInstanceOf[AnyRef]

  def sumBench(): Unit = {
    println
    println("Sum benchmark")
    val timings = new Array[Double](80)
    val eithers = new Array[Either[String, Int]](timings.length/3)
    val ors = new Array[Int Or String](timings.length/3)
    val grounds = new Array[AnyRef](timings.length/3)
    for (i <- 0 until timings.length/3) {
      val (e, et) = time(bestEitherSum(FlowBench.theNumbers, Right(0)))
      val (o, ot) = time(bestOrSum(    FlowBench.theNumbers, Is(0)))
      val (g, gt) = time(bestAnyRefSum(FlowBench.theNumbers, 0.asInstanceOf[AnyRef]))
      eithers(i) = e
      ors(i) = o
      grounds(i) = g
      timings(3*i) = et
      timings(3*i+1) = ot
      timings(3*i+2) = gt
    }
    ((eithers zip ors) zip grounds).foreach(println _)
    for (i <- 0 until timings.length/3) {
      println(f"${timings(3*i)}%6.3f   vs  ${timings(3*i+1)}%6.3f   vs  ${timings(3*i+2)}%6.3f")
    }
  }

  @Test
  def canBench(): Unit =
    stringBench()
    sumBench()
}
object FlowBench {
  var theStrings: Array[String] = null
  var theNumbers: Array[Int] = null

  @BeforeClass
  def before(): Unit = { 
    println("Before")
    theStrings = new Array[String](10000000)
    theNumbers = new Array[Int](theStrings.length)
    val content = "This is an example"
    var i = 0
    while (i < theStrings.length) {
      theStrings(i) = content.substring(0, 1+(i%content.length))
      theNumbers(i) = theStrings(i).length
      i += 1
    }
    println("Done with Before")
  }

  // @AfterClass
  // def after(): Unit = { println("After") }
}
