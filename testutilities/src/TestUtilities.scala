// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022 Rex Kerr and Calico Life Sciences LLC.

package kse.testutilities

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable

object TestUtilities {
  class Asserter(
        assertEq:    (String, Any, Any) => Unit,
        assertNotEq: (String, Any, Any) => Unit,
    val assertTrue:  (String, Boolean)  => Unit,
        isScalaEquality: Boolean = false
  ) {
    /** Checks Scala equality before passing on to error handler */
    def assertEquals(message: String, expect: Any, actual: Any): Unit =
      if isScalaEquality || expect != actual then assertEq(message, expect, actual)

    def assertNotEquals(message: String, expect: Any, actual: Any): Unit =
      if isScalaEquality || expect == actual then assertNotEq(message, expect, actual)
  }

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

  class TypeGen[B, C](val typed: Typed[B], val gen: () => C) {}

  case class Typed[A](unit: Unit = ()) {
    def --:[C](c: => C) = new TypeGen[A, C](this, () => c)

    def :--[B](b: B) = (this, b)

    def :==:[B](b: B)(using B =:= A): Unit = {}
  }
  def typed[A]: Typed[A] = new Typed[A]()
  def typedLike[A](a: A): Typed[A] = new Typed[A]()

  trait Messaging {
    def message: String = if mline.isEmpty then mline else s"${mline}\n"
    def mline: String
  }

  class Labeled[A](val mline: String, val value: () => A)(using asr: Asserter, ln: sourcecode.Line, fl: sourcecode.FileName) extends Messaging {
    import asr._

    override def message = s"error at ${fl.value}:${ln.value}\n" + super.message

    def isEqual[B](b: => B): Unit =
      val ta = Try{ value() }
      b match
        case t @ Thrown(tag) => ta match
          case Failure(x) =>
            if !tag.runtimeClass.isAssignableFrom(x.getClass) then
              assertEquals(message, ta, Failure(t))
          case _ => assertEquals(message, ta, Failure(t))
        case _ => assertEquals(message, ta.get, b)

    def ====(n: Null): Unit =
      isEqual(null)

    def ====[B, C](bc: TypeGen[B, C])(using B =:= A): Unit =
      isEqual(bc.gen())

    def ====[B](t: Typed[B])(using B =:= A): Unit = {}

    def ====[B](b: => B): Unit =
      isEqual(b)

    def =!!=[B](b: => B): Unit = Try{ value() } match
      case Success(v) => assertNotEquals(message, v, b)
      case Failure(x) => b match
        case t @ Thrown(tag) =>
          if tag.runtimeClass.isAssignableFrom(x.getClass) then
            assertTrue(s"${message}Did not expect $x\nto be a ${t.classname}", false)
        case _ =>

    def =~~=[B >: A](b: => B)(using apx: Approximation[B]): Unit =
      apx.approx(value(), b)
  }

  class LabeledCollection[C, I <: IsIterable[C]](val mline: String, val value: () => C, val ii: I)(using asr: Asserter, ln: sourcecode.Line, fl: sourcecode.FileName) extends Messaging {
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
        assertTrue(s"${message}extra element at index $i\n${ia.next}", false)
      if ib.hasNext then
        assertTrue(s"${message}extra element at index $i\n${ib.next}", false)

    def contains[B](b: => B): Unit =
      val ia = ii(value()).iterator
      val vb = b
      while ia.hasNext do
        val va = ia.next
        if va == vb then
          return
      assertTrue(s"${message}could not find an element matching $vb", false )

    def exists(f: ii.A => Boolean): Unit =
      val ia = ii(value()).iterator
      while ia.hasNext do
        if f(ia.next) then return
      assertTrue(s"${message}collection never passed test", false )
  }

  trait GenLabeled {
    def message: String

    def ~[A](a: => A)(using asr: Asserter, ln: sourcecode.Line, fl: sourcecode.FileName): Labeled[A] =
      Labeled(message, () => a)

    def ~[A](a: => A)(using ii: IsIterable[A], asr: Asserter, ln: sourcecode.Line, fl: sourcecode.FileName): LabeledCollection[A, ii.type] =
      LabeledCollection[A, ii.type](message, () => a, ii)
  }

  object T extends GenLabeled {
    def message = ""

    def apply(msg: String): GenLabeled = new GenLabeled { def message = msg }
  }
}
