// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-23 Rex Kerr and Calico Life Sciences LLC.

package kse.testutilities

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable


/** This provides a way to write unit tests.
  *
  * It's really easy to use once you're used to it!
  * 
  * Unfortunately, it doesn't have any documentation at all.
  * You can read the unit tests for kse.flow etc. to see how it is used.
  * 
  * See build.sc for how to include it in your own tests.
  */
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
    class OfFloat(eps: Float, thresh: Float, tol: Float) extends Approximation[Float] {
      def approx(a0: Float, a1: Float) =
        (a0 == a1) ||
        (a0.isNaN && a1.isNaN) ||
        {
          val delta = math.abs(a0 - a1)
          val a = math.abs(a0) max math.abs(a1)
          if a > thresh then delta < eps*a
          else delta < tol
        }
    }

    class OfDouble(eps: Double, thresh: Double, tol: Double) extends Approximation[Double] {
      def approx(a0: Double, a1: Double) =
        (a0 == a1) ||
        (a0.isNaN && a1.isNaN) ||
        {
          val delta = math.abs(a0 - a1)
          val a = math.abs(a0) max math.abs(a1)
          val ans =
            if a > thresh then delta < eps*a
            else delta < tol
          ans
        }
    }

    given defaultFloatApprox: Approximation[Float] = new OfFloat(1e-6f, 1f, 1e-6f)

    given defaultDoubleApprox: Approximation[Double] = new OfDouble(1e-9, 1.0, 1e-9)
  }

  case class Thrown(tag: ClassTag[?])(val classname: String) extends ControlThrowable(classname) {}
  def thrown[A](using tag: ClassTag[A]): Thrown = Thrown(tag)(tag.runtimeClass.getName)

  class TypeGen[B, C](val typed: Typed[B], val gen: () => C) {}

  case class Typed[A](unit: Unit = ()) {
    def --:[C](c: => C) = new TypeGen[A, C](this, () => c)

    def :--[B](b: B) = (this, b)

    def :==:[B](b: B)(using B =:= A): Unit = {}
  }
  def typed[A]: Typed[A] = new Typed[A]()
  def typedLike[A](a: A): Typed[A] = new Typed[A]()

  case class RunType[A](tag: ClassTag[A]) {}
  def runtype[A](using tag: ClassTag[A]): RunType[A] = RunType(tag)

  trait Messaging {
    def message: String = if mline.isEmpty then mline else s"${mline}\n"
    def mline: String
  }

  class Labeled[A](val mline: String, val value: () => A)(using asr: Asserter, ln: sourcecode.Line, fl: sourcecode.FileName) extends Messaging {
    import asr._

    override def message = s"error at ${fl.value}:${ln.value}\n" + super.message

    def isEqual[B](b: => B): Unit =
      val ta = Try{ value() }
      val vb = b
      vb match
        case t @ Thrown(tag) => ta match
          case Failure(x) =>
            if !tag.runtimeClass.isAssignableFrom(x.getClass) then
              assertEquals(message, ta, Failure(t))
          case _ => assertEquals(message, ta, Failure(t))
        case _ => assertEquals(message, ta.get, vb)

    def ====(n: Null): Unit =
      isEqual(null)

    def ====[B, C](bc: TypeGen[B, C])(using B =:= A): Unit =
      isEqual(bc.gen())

    def ====[B](t: Typed[B])(using B =:= A): Unit = {}

    def ====[B](t: RunType[B]): Unit =
      val v = value()
      if !t.tag.runtimeClass.isAssignableFrom(v.getClass) then 
        assertEquals(message, v.getClass, t.tag.runtimeClass)

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
      val va = value()
      val vb = b
      if !apx.approx(va, vb) then assertEquals(message, va, vb)
  }

  class LabeledCollection[C, I <: IsIterable[C]](val mline: String, val value: () => C, val ii: I)(using asr: Asserter, ln: sourcecode.Line, fl: sourcecode.FileName) extends Messaging {
    import asr._

    override def message = s"error at ${fl.value}:${ln.value}\n" + super.message

    private def pickMoreElements[A](count: Int, index: Int, it: Iterator[A], acc: List[String] = Nil): List[String] =
      if count <= 0 || ! it.hasNext then
        var result = acc
        while result.forall(_ startsWith "   ") do
          result = result.map(_.drop(1))
        if it.hasNext then ("  ..." :: result).reverse else result.reverse
      else if !it.hasNext then acc.reverse
      else
        val longest = (count + index).toString
        var istr = "  #" + index.toString
        if istr.length < 3+longest.length then istr = " "*(3+longest.length - istr.length) + istr
        pickMoreElements(count-1, index+1, it, s"$istr = ${it.next}" :: acc)

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
        val elts = pickMoreElements(4, i, ia)
        val plural = if elts.length > 1 then "Too many elements.  Extra:" else "One extra element:"
        val lines = elts.mkString("", "\n", "\n")
        assertTrue(s"${message}$plural\n$lines", false)
      if ib.hasNext then
        val elts = pickMoreElements(4, i, ib)
        val plural = if elts.length > 1 then "Not enough elements.  Missed:" else "Missed one element:"
        val lines = elts.mkString("", "\n", "\n")
        assertTrue(s"${message}$plural\n$lines", false)

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
