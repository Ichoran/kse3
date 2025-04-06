// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-24 Rex Kerr and Calico Life Sciences LLC.

package kse.testutilities

// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

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
    private def compileAnswer(compiled: Boolean) =
      if compiled then "successfully compiles"
      else "fails to compile"

    def message: String

    def ~[A](a: => A)(using asr: Asserter, ln: sourcecode.Line, fl: sourcecode.FileName): Labeled[A] =
      Labeled(message, () => a)

    def ~[A](a: => A)(using ii: IsIterable[A], asr: Asserter, ln: sourcecode.Line, fl: sourcecode.FileName): LabeledCollection[A, ii.type] =
      LabeledCollection[A, ii.type](message, () => a, ii)

    inline def !(inline code: String)(using Asserter, sourcecode.Line, sourcecode.FileName): Unit = (this ~ compileAnswer(compiletime.testing.typeChecks(code)) ==== "fails to compile")
  }

  object T extends GenLabeled {
    def message = ""

    def apply(msg: String): GenLabeled = new GenLabeled { def message = msg }
  }
}


/** Thyme provides a way to run quick yet statistically robust microbenchmarks--even within a running application! */
/*
final class Thyme(val targetTime: Double = 50e-3, val rng: Thyme.Pcg32 = Thyme.Pcg32()) {
  var t0: Long = -1L
  var t1: Long = 0L
  val playground: Array[Int] = new Array[Int](2048)
  val timings: Array[Double] = new Array[Double](512)
  val randoms: Array[Double] = new Array[Double](512)

  def getAndReset: Double =
    if t0 < t1 then Double.NaN
    else
      val delta = t1 - t0
      t0 = t1 - 1
      delta / 1e9

  def time[A](f: => A): A =
    t0 = System.nanoTime
    val ans = f
    t1 = System.nanoTime
    ans

  def timeTup[A](f: => A): (A, Double) =
    (time(f), getAndReset)

  def ptime[A](f: => A): A =
    val ans = time(f)
    val dt = getAndReset
    if dt < 1e-5 then println(f"Elapsed: ${dt*1e6}%.3f us")
    else if dt < 1e-3 then println(f"Elapsed: ${dt*1e3}%.4f ms")
    else if dt < 1 then println(f"Elapsed: $dt%.5f s")
    else println(f"Elapsed: $dt%.3f s")

  def stability(n: Int, repeats: Int = 1000): Double
    var m = if repeats > 0 then repeats else 1

    var i = 0
    var x = 0.0
    var xx = 0.0
    while i < repeats do
      val t = timings(i)
      randoms(i) = t
      x += t
      xx += t*t
      i += 1
    if repeats > 1 then x /= n
    val v = if repeats > 1 then (xx/n) - x*x else Double.NaN
    var score = 0.0
    var score0 = Double.NaN
    var nls = 0
    var j = 0
    while j < repeats do
      i = 0
      score = 0.0
      while i < n do
        val t = randoms(i)
        if t > x then
          val dx2 = (t - x)*(t - x)
          if dx2 < v then score += i
          else if dx2 < 2*v then score += 2*i
          else 
        i += 1

  def bench(f: => Int): (Double, Int) =
    var n = 0
    var m = 10
    var i = 0
    var j = 0
    var t = 0.0
    var state = 3
    while state > 0 do
      if state == 3 then
        if t < targetTime && n < playground.length then
          n = 1 + 2*n
        else
          state = 2
      t0 = System.nanoTime
      var k = n
      while k > 0 do
        playground(i) = f
        i += 1
        if i >= playground.length then
          playground(0) = playground(rng % playground.length)
          i = 1
        k -= 1
      t1 = System.nanoTime
      t = getAndReset
      timings(j) = t
      j += 1
      if j >= timings.length then j = 0
  ???
}
object Thyme {
  final class Pcg32 private (private var state: Long) {
    private inline val mult = 0x5851F42D4C957F2DL
    private var incr = 0xDA3E39CB94B95BDBL
    def seed(zero: Long, sequence: Long): Unit =
      state = 0L
      incr = (sequence << 1) | 1
      I
      state += zero
      I
    def I: Int =
      val s = state
      state = state * mult + incr
      val a = (((s >>> 18) ^ s) >>> 27).toInt
      val r = (s >>> 59).toInt
      (a >>> r) | (a << ((~r + 1) & 31))
    def %(m: Int): Int =
      if m < 0 then -(this % (-m))
      else
        var x = I
        if x < 0 && x + m > 0 then
          // Might need to employ rejection sampling.  Compute exact threshold
          val r = java.lang.Integer.remainderUnsigned(-1, m) + 1
          if r < m then
            // Reject top r values because they don't fill m
            while java.lang.Integer.compareUnsigned(-r, x) <= 0 do x = I
        java.lang.Integer.remainderUnsigned(x, m)
    def shuffle(a: Array[Double], n: Int = Int.MaxValue): Unit =
      var i = 0
      val m = math.min(n, a.length)
      while i + 1 < m do
        val j = i + (this % (m - i))
        if j > i then
          val x = a(i)
          a(i) = a(j)
          a(j) = x
        i += 1
  }
  object Pcg32 {
    def default(): Pcg32 =
      new Pcg32(0L)
    def apply(): Pcg32 =
      val p = new Pcg32(0L)
      p.seed(1, System.nanoTime)
      p
    def apply(seed: Long): Pcg32 =
      val p = new Pcg32(0L)
      p.seed(1, seed)
      p
    def apply(zero: Long, sequence: Long): Pcg32 =
      val p = new Pcg32(0L)
      p.seed(zero, sequence)
      p
  }
}
*/
