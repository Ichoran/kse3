// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022 Rex Kerr and Calico Life Sciences LLC.

package kse.testutilities

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable

object TestUtilities {
  class Asserter(
    val assertEquals: (String, Any, Any) => Unit,
    val assertNotEquals: (String, Any, Any) => Unit,
    val assertTrue: (String, Boolean) => Unit
  ) {}

  trait Approximation[A] {
    def approx(a0: A, a1: A): Boolean
  }
  object Approximation {
    given Approximation[Float] = new {
      def approx(a0: Float, a1: Float) =
        (a0 == a1) ||
        (a0.isNaN && a1.isNaN) ||
        {
          val delta = math.abs(a0 - a1)
          val a = math.abs(a0) max math.abs(a1)
          if a > 1 then delta < 1e-6*a
          else delta < 1e-6
        }
    }

    given Approximation[Double] = new {
      def approx(a0: Double, a1: Double) =
        (a0 == a1) ||
        (a0.isNaN && a1.isNaN) ||
        {
          val delta = math.abs(a0 - a1)
          val a = math.abs(a0) max math.abs(a1)
          if a > 1 then delta < 1e-9*a
          else delta < 1e-9
        }
    }
  }

  case class Thrown(tag: ClassTag[_])(val classname: String) extends ControlThrowable(classname) {}
  def thrown[A](using tag: ClassTag[A]): Thrown = Thrown(tag)(tag.runtimeClass.getName)

  case class Typed[A](unit: Unit = ()) {}
  def typed[A]: Typed[A] = new Typed[A]()

  trait Messaging {
    def message: String
    def mline: String = if message.isEmpty then message else s"${message}\n"
  }

  class Labeled[A](val message: String, val value: () => A)(using asr: Asserter) extends Messaging {
    import asr._

    def ====[B](b: => B): Unit =
      val ta = Try{ value() }
      b match
        case t @ Thrown(tag) => ta match
          case Failure(x) =>
            if !tag.runtimeClass.isAssignableFrom(x.getClass) then
              assertEquals(message, ta, Failure(t))
          case _ => assertEquals(message, ta, Failure(t))
        case _ => assertEquals(message, ta.get, b)

    def =!!=[B](b: => B): Unit = Try{ value() } match
      case Success(v) => assertNotEquals(message, v, b)
      case Failure(x) => b match
        case t @ Thrown(tag) =>
          if tag.runtimeClass.isAssignableFrom(x.getClass) then
            assertTrue(s"${mline}Did not expect $x\nto be a ${t.classname}", false)
        case _ =>

    def =??=[B](t: Typed[B])(using B =:= A): Unit = {}

    def =~~=[B >: A](b: => B)(using apx: Approximation[B]): Unit =
      apx.approx(value(), b)
  }

  class LabeledCollection[C, I <: IsIterable[C]](val message: String, val value: () => C, val ii: I)(using asr: Asserter) extends Messaging {
    import asr._

    def =**=[D, J <: IsIterable[D]](d: => D)(using jj: J): Unit =
      var i = 0
      val ia = ii(value()).iterator
      val ib = jj(d).iterator
      while ia.hasNext && ib.hasNext do
        val va = ia.next
        val vb = ib.next
        if va != vb then
          assertEquals(message + s"\nerror at index $i", va, vb)
        i += 1
      if ia.hasNext then
        assertTrue(s"${mline}extra element at index $i\n${ia.next}", false)
      if ib.hasNext then
        assertTrue(s"${mline}extra element at index $i\n${ib.next}", false)

    def contains[B](b: => B): Unit =
      val ia = ii(value()).iterator
      val vb = b
      while ia.hasNext do
        val va = ia.next
        if va == vb then
          return
      assertTrue(s"${mline}could not find an element matching $vb", false )

    def exists(f: ii.A => Boolean): Unit =
      val ia = ii(value()).iterator
      while ia.hasNext do
        if f(ia.next) then return
      assertTrue(s"${mline}collection never passed test", false )
  }

  extension (message: String)(using asr: Asserter)
    def \[A](a: => A): Labeled[A] = Labeled(message, () => a)
    def \[A](a: => A)(using ii: IsIterable[A]): LabeledCollection[A, ii.type] = LabeledCollection[A, ii.type](message, () => a, ii)
}
