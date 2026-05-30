// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.test.basics


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import java.lang.foreign.Arena

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}

import sourcecode.{Line, given}


class MemTest() {
  import kse.testutilities.TestUtilities.{_, given}
  import kse.basics.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  // Materialize a Mem's contents so collection comparisons (=**=) can be used.
  extension [A <: Mem.Type](m: Mem[A])
    inline def vec: Vector[A] =
      val b = Vector.newBuilder[A]
      m.use()(b += _)
      b.result()

  def memInlinedDataTest(): Unit =
    var cuml = 0
    inline def n(inline f: => Unit): Int =
      cuml = 0
      f
      cuml

    def ai = Array(2, 3, 5, 7, 11)
    def aj = Array(10, 20, 30, 40, 50)
    val ix = Array[Long](3, 1, 4, 1)
    def st = Array[Long](3, 1, 4, 1).stepper

    // Wrap a fresh array, mutate through it, and return it for inspection.
    inline def mut(a: Array[Int])(inline f: Mem[Int] => Unit): Array[Int] =
      val m = Mem of a
      f(m)
      a
    inline def into(k: Int)(inline f: Mem[Int] => Any): Array[Int] =
      val a = new Array[Int](k)
      val m = Mem of a
      f(m)
      a
    inline def intoL(k: Int)(inline f: Mem[Long] => Any): Array[Long] =
      val a = new Array[Long](k)
      val m = Mem of a
      f(m)
      a

    // apply / length / segment
    T ~ (Mem of ai).length          ==== 5L
    T ~ (Mem of ai)(0)              ==== 2
    T ~ (Mem of ai)(4)              ==== 11
    T ~ (Mem of ai).segment.byteSize ==== 20L
    T ~ (Mem of ai).as[Long]        ==== typed[Mem[Long]]

    // update (single element, via assignment syntax)
    T ~ mut(ai){ m => m(2) = 99 }   =**= Array(2, 3, 99, 7, 11)

    // use
    T ~ n{ (Mem of ai).use()(cuml += _) }      ==== 28
    T ~ n{ (Mem of ai).use(1L, 4L)(cuml += _) } ==== 15
    T ~ n{ (Mem of ai).use(ix)(cuml += _) }    ==== 24
    T ~ n{ (Mem of ai).use(st)(cuml += _) }    ==== 24
    T ~ n{ (Mem of ai).use(_ > 4)(cuml += _) } ==== 23

    // alter
    T ~ mut(ai)(_.alter()(_ + 1))         =**= Array(3, 4, 6, 8, 12)
    T ~ mut(ai)(_.alter(1L, 4L)(_ + 1))   =**= Array(2, 4, 6, 8, 11)
    T ~ mut(ai)(_.alter(ix)(_ + 1))       =**= Array(2, 5, 5, 8, 12)
    T ~ mut(ai)(_.alter(st)(_ + 1))       =**= Array(2, 5, 5, 8, 12)
    T ~ mut(ai)(_.alter(_ > 4)(_ + 1))    =**= Array(2, 3, 6, 8, 12)

    // visit
    T ~ n{ (Mem of ai).visit()((x, i) => cuml += x + i.toInt) }      ==== 38
    T ~ n{ (Mem of ai).visit(1L, 4L)((x, i) => cuml += x + i.toInt) } ==== 21
    T ~ n{ (Mem of ai).visit(ix)((x, i) => cuml += x + i.toInt) }    ==== 33
    T ~ n{ (Mem of ai).visit(st)((x, i) => cuml += x + i.toInt) }    ==== 33
    T ~ n{ (Mem of ai).visit(_ > 4)((x, i) => cuml += x + i.toInt) } ==== 32

    // edit
    T ~ mut(ai)(_.edit()((x, i) => x + i.toInt))       =**= Array(2, 4, 7, 10, 15)
    T ~ mut(ai)(_.edit(1L, 4L)((x, i) => x + i.toInt)) =**= Array(2, 4, 7, 10, 11)
    T ~ mut(ai)(_.edit(ix)((x, i) => x + i.toInt))     =**= Array(2, 5, 5, 10, 15)
    T ~ mut(ai)(_.edit(_ > 4)((x, i) => x + i.toInt))  =**= Array(2, 3, 7, 10, 15)

    // pairs / trios
    T ~ n{ (Mem of ai).pairs((x, y) => if y > x then cuml += 1) } ==== 4
    T ~ n{ (Mem of ai).trios((x, y, z) => cuml += 1) }            ==== 3

    // together
    T ~ n{ (Mem of ai).together(Mem of aj)((x, y, i) => cuml += x + y + i.toInt) }              ==== 188
    T ~ n{ (Mem of ai).together(Mem of aj, Mem of ai)((x, y, z, i) => cuml += x + y + z + i.toInt) } ==== 216

    // wander
    T ~ n{ (Mem of ai).wander(){ (x, i) => cuml += x; i + 2 } __ Unit }  ==== 18
    T ~ n{ (Mem of ai).wander(1L){ (x, i) => cuml += x; i + 2 } __ Unit } ==== 10
    T ~ (Mem of ai).wander(){ (_, i) => i + 2 }                  ==== 3L
    T ~ (Mem of ai).wander(1L){ (_, i) => i + 2 }                ==== 2L

    // gather
    T ~ (Mem of ai).gather(0)()((z, x, i) => z + x + i.toInt)      ==== 38
    T ~ (Mem of ai).gather(0)(1L, 4L)((z, x, i) => z + x + i.toInt) ==== 21
    T ~ (Mem of ai).gather(0)(ix)((z, x, i) => z + x + i.toInt)    ==== 33
    T ~ (Mem of ai).gather(0)(st)((z, x, i) => z + x + i.toInt)    ==== 33
    T ~ (Mem of ai).gather(0)(_ > 4)((z, x, i) => z + x + i.toInt) ==== 32

    // update family
    T ~ mut(ai)(_.update(0))                              =**= Array(0, 0, 0, 0, 0)
    T ~ mut(ai)(_.update(1L, 4L, 9))                      =**= Array(2, 9, 9, 9, 11)
    T ~ mut(ai)(_.update(ix, 0))                          =**= Array(2, 0, 5, 0, 0)
    T ~ mut(ai)(_.update(st, 0))                          =**= Array(2, 0, 5, 0, 0)
    T ~ mut(ai)(_.update(_ > 4, 0))                       =**= Array(2, 3, 0, 0, 0)
    T ~ mut(ai)(_.update(Mem of Array(9, 9, 9, 9, 9)))    =**= Array(9, 9, 9, 9, 9)
    T ~ mut(ai)(_.update(1L, 4L, Mem of Array(100, 200, 300))) =**= Array(2, 100, 200, 300, 11)

    // set family
    T ~ mut(ai)(_.set()(() => 7))           =**= Array(7, 7, 7, 7, 7)
    T ~ mut(ai)(_.set()(i => i.toInt))      =**= Array(0, 1, 2, 3, 4)
    T ~ mut(ai)(_.set(1L, 4L)(() => 0))     =**= Array(2, 0, 0, 0, 11)
    T ~ mut(ai)(_.set(1L, 4L)(i => i.toInt)) =**= Array(2, 1, 2, 3, 11)
    T ~ mut(ai)(_.set(ix)(() => 9))         =**= Array(2, 9, 5, 9, 9)
    T ~ mut(ai)(_.set(st)(() => 9))         =**= Array(2, 9, 5, 9, 9)
    T ~ mut(ai)(_.set(_ > 4)(() => 0))      =**= Array(2, 3, 0, 0, 0)

    // where
    T ~ (Mem of ai).where()                                       =**= Array[Long](0, 1, 2, 3, 4)
    T ~ (Mem of ai).where(_ > 4)                                  =**= Array[Long](2, 3, 4)
    T ~ (Mem of ai).whereOp((x, i) => if x > 4 then i else -1L)   =**= Array[Long](2, 3, 4)
    T ~ (Mem of ai).whereIn(1L, 4L)(_ > 4)                        =**= Array[Long](2, 3)
    T ~ (Mem of ai).whereInOp(1L, 4L)((x, i) => if x > 4 then i else -1L) =**= Array[Long](2, 3)
    T ~ (Mem of ai).whereFrom(ix)(_ > 4)                          =**= Array[Long](3, 4)
    T ~ (Mem of ai).whereFromOp(ix)((x, i) => if x > 4 then i else -1L)   =**= Array[Long](3, 4)
    T ~ (Mem of ai).where()                                       ==== typed[Array[Long]]

    // inject
    T ~ into(7)(d => (Mem of ai).inject(d))            =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ into(7)(d => (Mem of ai).inject(d, 2L))        =**= Array(0, 0, 2, 3, 5, 7, 11)
    T ~ into(7)(d => (Mem of ai).inject(d)(1L, 4L))    =**= Array(3, 5, 7, 0, 0, 0, 0)
    T ~ into(7)(d => (Mem of ai).inject(d, 2L)(1L, 4L)) =**= Array(0, 0, 3, 5, 7, 0, 0)
    T ~ into(7)(d => (Mem of ai).inject(d)(ix))        =**= Array(7, 3, 11, 3, 0, 0, 0)
    T ~ into(7)(d => (Mem of ai).inject(d)(st))        =**= Array(7, 3, 11, 3, 0, 0, 0)
    T ~ into(7)(d => (Mem of ai).inject(d)(_ > 4))     =**= Array(5, 7, 11, 0, 0, 0, 0)
    T ~ (Mem of ai).inject(Mem of new Array[Int](7))         ==== 5L
    T ~ (Mem of ai).inject(Mem of new Array[Int](7))(ix)     ==== 4L
    T ~ (Mem of ai).inject(Mem of new Array[Int](7))(_ > 4)  ==== 3L

    // injectOp (Int source -> Long destination)
    T ~ intoL(5)(d => (Mem of ai).injectOp(d)()((x, i) => x + i))         =**= Array[Long](2, 4, 7, 10, 15)
    T ~ intoL(6)(d => (Mem of ai).injectOp(d, 1L)()((x, i) => x + i))     =**= Array[Long](0, 2, 4, 7, 10, 15)
    T ~ intoL(5)(d => (Mem of ai).injectOp(d)(1L, 4L)((x, i) => x + i))   =**= Array[Long](4, 7, 10, 0, 0)
    T ~ intoL(5)(d => (Mem of ai).injectOp(d)(ix)((x, i) => x + i))       =**= Array[Long](10, 4, 15, 4, 0)
    T ~ intoL(5)(d => (Mem of ai).injectOp(d)(_ > 4)((x, i) => x + i))    =**= Array[Long](7, 10, 15, 0, 0)
    T ~ (Mem of ai).injectOp(Mem of new Array[Long](5))()((x, i) => x + i) ==== 5L

    // visitCuts
    def acut = Array(1, 1, 2, 2, 2, 3)
    T ~ n{ (Mem of acut).visitCuts()((x, y) => x != y)((i, j) => cuml += 1) }            ==== 3
    T ~ n{ (Mem of acut).visitCuts()((x, y) => x != y)((i, j) => cuml += (j - i).toInt) } ==== 6
    T ~ n{ (Mem of acut).visitCuts(1L, 5L)((x, y) => x != y)((i, j) => cuml += 1) }      ==== 2

  def memClippedInlinedDataTest(): Unit =
    var cuml = 0
    inline def n(inline f: => Unit): Int =
      cuml = 0
      f
      cuml

    def ai = Array(2, 3, 5, 7, 11)
    val ix = Array[Long](1, 9, 3)
    def st = Array[Long](1, 9, 3).stepper

    inline def mut(a: Array[Int])(inline f: ClippedMem[Int] => Unit): Array[Int] =
      val ca = (Mem of a).clip
      f(ca)
      a
    inline def into(k: Int)(inline f: Mem[Int] => Unit): Array[Int] =
      val a = new Array[Int](k)
      val m = Mem of a
      f(m)
      a

    // apply with default / get
    T ~ (Mem of ai).clip(2)(-1)  ==== 5
    T ~ (Mem of ai).clip(9)(-1)  ==== -1
    T ~ (Mem of ai).clip(-1)(-1) ==== -1
    T ~ (Mem of ai).clip.get(2)  ==== Some(5)
    T ~ (Mem of ai).clip.get(9)  ==== None

    // use (single + clamped range + sparse indices)
    T ~ n{ (Mem of ai).clip.use(2)(cuml += _) }         ==== 5
    T ~ n{ (Mem of ai).clip.use(9)(cuml += _) }         ==== 0
    T ~ n{ (Mem of ai).clip.use(3L, 99L)(cuml += _) }   ==== 18
    T ~ n{ (Mem of ai).clip.use(-5L, 2L)(cuml += _) }   ==== 5
    T ~ n{ (Mem of ai).clip.use(ix)(cuml += _) }        ==== 10
    T ~ n{ (Mem of ai).clip.use(st)(cuml += _) }        ==== 10

    // alter / visit / edit / gather (clamped & index-skipping)
    T ~ mut(ai)(_.alter(3L, 99L)(_ + 1))                              =**= Array(2, 3, 5, 8, 12)
    T ~ mut(ai)(_.alter(ix)(_ + 1))                                   =**= Array(2, 4, 5, 8, 11)
    T ~ n{ (Mem of ai).clip.visit(3L, 99L)((x, i) => cuml += x + i.toInt) } ==== 25
    T ~ mut(ai)(_.edit(3L, 99L)((x, i) => x + i.toInt))               =**= Array(2, 3, 5, 10, 15)
    T ~ (Mem of ai).clip.gather(0)(3L, 99L)((z, x, i) => z + x + i.toInt) ==== 25
    T ~ (Mem of ai).clip.gather(0)(ix)((z, x, i) => z + x + i.toInt)  ==== 14

    // update / set (clamped & index-skipping)
    T ~ mut(ai)(_.update(3L, 99L, 0)) =**= Array(2, 3, 5, 0, 0)
    T ~ mut(ai)(_.update(ix, 0))      =**= Array(2, 0, 5, 0, 11)
    T ~ mut(ai)(_.update(st, 0))      =**= Array(2, 0, 5, 0, 11)
    T ~ mut(ai)(_.set(3L, 99L)(() => 0))      =**= Array(2, 3, 5, 0, 0)
    T ~ mut(ai)(_.set(3L, 99L)(i => i.toInt)) =**= Array(2, 3, 5, 3, 4)
    T ~ mut(ai)(_.set(ix)(() => 0))           =**= Array(2, 0, 5, 0, 11)

    // where (clamped & index-skipping)
    T ~ (Mem of ai).clip.whereIn(1L, 99L)(_ > 4) =**= Array[Long](2, 3, 4)
    T ~ (Mem of ai).clip.whereFrom(ix)(_ > 4)    =**= Array[Long](3)

    // inject (clamped offsets/ranges & index-skipping)
    T ~ into(7)(d => (Mem of ai).clip.inject(d) __ Unit)         =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ into(7)(d => (Mem of ai).clip.inject(d, -5L) __ Unit)    =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ into(7)(d => (Mem of ai).clip.inject(d)(1L, 99L) __ Unit) =**= Array(3, 5, 7, 11, 0, 0, 0)
    T ~ into(7)(d => (Mem of ai).clip.inject(d)(ix) __ Unit)     =**= Array(3, 7, 0, 0, 0, 0, 0)
    T ~ into(7)(d => (Mem of ai).clip.inject(d)(_ > 4) __ Unit)  =**= Array(5, 7, 11, 0, 0, 0, 0)

    // visitCuts (clamped)
    def acut = Array(1, 1, 2, 2, 2, 3)
    T ~ n{ (Mem of acut).clip.visitCuts(1L, 5L)((x, y) => x != y)((i, j) => cuml += 1) } ==== 2

  def memConstructTest(): Unit =
    // alloc (GC-managed)
    val md = Mem.alloc[Double](4)
    md.set()(i => i.toDouble * 1.5)
    T ~ md.length           ==== 4L
    T ~ md.vec              =**= Vector(0.0, 1.5, 3.0, 4.5)
    T ~ md.segment.byteSize ==== 32L

    // of shares storage with the array
    val a  = Array(1, 2, 3)
    val ma = Mem of a
    T ~ { ma(0) = 9; a(0) } ==== 9
    T ~ (Mem of a).length   ==== 3L

    // round-trip every element type
    T ~ { val x = Array[Byte](1, 2, 3);     (Mem of x)(1) = 9;   x(1) } ==== 9.toByte
    T ~ { val x = Array[Short](1, 2, 3);    (Mem of x)(1) = 9;   x(1) } ==== 9.toShort
    T ~ { val x = Array[Char]('a', 'b');    (Mem of x)(1) = 'z'; x(1) } ==== 'z'
    T ~ { val x = Array(1, 2, 3);           (Mem of x)(1) = 9;   x(1) } ==== 9
    T ~ { val x = Array(1L, 2L, 3L);        (Mem of x)(1) = 9L;  x(1) } ==== 9L
    T ~ { val x = Array(1f, 2f, 3f);        (Mem of x)(1) = 1.5f; x(1) } ==== 1.5f
    T ~ { val x = Array(1.0, 2.0, 3.0);     (Mem of x)(1) = 1.5; x(1) } ==== 1.5

    // reinterpretation (native byte order, as elsewhere in the suite)
    T ~ Mem.as[Int](Array[Byte](1, 2, 3, 4))(0)        ==== 0x04030201
    T ~ Mem.as[Int](Array[Byte](1, 2, 3, 4)).length    ==== 1L
    T ~ Mem.as[Int](Array[Byte](1, 2, 3, 4, 5, 6)).length ==== 1L      // floor: 6 bytes -> 1 int
    T ~ Mem.as[Byte](Array(0x04030201)).vec            =**= Vector[Byte](1, 2, 3, 4)
    T ~ (Mem of Array(0x04030201)).as[Byte].vec        =**= Vector[Byte](1, 2, 3, 4)
    T ~ (Mem of Array(0x04030201)).as[Byte].length     ==== 4L

    // Owned: explicit-lifetime, thread-shareable region built from a caller-owned arena
    val o = Mem.Owned.create[Double](Arena.ofShared())(_.allocate(32L))
    o.use(_.set()(i => i.toDouble * 2))
    T ~ o.op(_.length) ==== 4L
    T ~ o.op(_.vec)    =**= Vector(0.0, 2.0, 4.0, 6.0)
    o.close()
    T ~ { o.op(_(0)) } ==== thrown[IllegalStateException]
}
object MemTest {
}
