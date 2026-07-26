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



class MemTest() {
  import kse.basics.testutilities.TestUtilities.{_, given}
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
    {
      val mem = Mem of Array(1, 2, 3, 4, 5, 6, 7)
      val eom = mem.length - 1
      T ~ mem.whereFwd(0)(x => (x%2) == 0)   ==== mem.where(x => (x%2) == 0)(0)
      T ~ mem.whereFwd(0)(x => (x%2) == 1)   ==== mem.where(x => (x%2) == 1)(0)
      T ~ mem.whereFwd(2)(x => (x%2) == 0)   ==== mem.whereIn(2, eom + 1)(x => (x%2) == 0)(0)
      T ~ mem.whereFwd(2)(x => (x%2) == 1)   ==== mem.whereIn(2, eom + 1)(x => (x%2) == 1)(0)
      T ~ mem.whereFwd(eom)(x => (x%2) == 0) ==== { if (mem(eom) % 2) == 0 then eom else -1 }
      T ~ mem.whereFwd(eom)(x => (x%2) == 1) ==== { if (mem(eom) % 2) == 0 then -1 else eom }
      T ~ mem.whereFwd(0)(_ == 999)          ==== -1
      T ~ mem.whereFwd(3)(_ == 999)          ==== -1
      T ~ mem.whereBkw(eom)(x => (x%2) == 0) ==== mem.where(x => (x%2) == 0).last
      T ~ mem.whereBkw(eom)(x => (x%2) == 1) ==== mem.where(x => (x%2) == 1).last
      T ~ mem.whereBkw(2)(x => (x%2) == 0)   ==== mem.whereIn(0, 3)(x => (x%2) == 0).last
      T ~ mem.whereBkw(2)(x => (x%2) == 1)   ==== mem.whereIn(0, 3)(x => (x%2) == 1).last
      T ~ mem.whereBkw(0)(x => (x%2) == 0)   ==== { if (mem(0) % 2) == 0 then 0 else -1 }
      T ~ mem.whereBkw(0)(x => (x%2) == 1)   ==== { if (mem(0) % 2) == 0 then -1 else 0 }    
      T ~ mem.whereBkw(eom)(_ == 999)        ==== -1
      T ~ mem.whereBkw(3)(_ == 999)          ==== -1
    }

    // typed get/set (byte offset = index * bytesOf[A]; native byte order, as elsewhere in the suite)
    def ab = Array[Byte](1, 2, 3, 4, 5, 6, 7, 8)
    T ~ (Mem of ab).getB(1) ==== 2.toByte
    T ~ (Mem of ab).getS(1) ==== 0x0302.toShort
    T ~ (Mem of ab).getC(1) ==== 0x0302.toChar
    T ~ (Mem of ab).getI(1) ==== 0x05040302
    T ~ (Mem of ab).getL(0) ==== 0x0807060504030201L
    T ~ (Mem of ab).getF(0) ==== java.lang.Float.intBitsToFloat(0x04030201)
    T ~ (Mem of ab).getD(0) ==== java.lang.Double.longBitsToDouble(0x0807060504030201L)
    T ~ (Mem of Array(0x04030201, 0x08070605)).getB(1) ==== 5.toByte
    T ~ (Mem of Array(0x04030201, 0x08070605)).getS(1) ==== 0x0605.toShort
    T ~ { val x = new Array[Byte](8); (Mem of x).setB(1, 2);                       x } =**= Array[Byte](0, 2, 0, 0, 0, 0, 0, 0)
    T ~ { val x = new Array[Byte](8); (Mem of x).setS(1, 0x0302.toShort);          x } =**= Array[Byte](0, 2, 3, 0, 0, 0, 0, 0)
    T ~ { val x = new Array[Byte](8); (Mem of x).setC(1, 0x0302.toChar);           x } =**= Array[Byte](0, 2, 3, 0, 0, 0, 0, 0)
    T ~ { val x = new Array[Byte](8); (Mem of x).setI(2, 0x04030201);              x } =**= Array[Byte](0, 0, 1, 2, 3, 4, 0, 0)
    T ~ { val x = new Array[Byte](8); (Mem of x).setL(0, 0x0807060504030201L);     x } =**= Array[Byte](1, 2, 3, 4, 5, 6, 7, 8)
    T ~ { val x = new Array[Byte](4); (Mem of x).setF(0, java.lang.Float.intBitsToFloat(0x04030201));           x } =**= Array[Byte](1, 2, 3, 4)
    T ~ { val x = new Array[Byte](8); (Mem of x).setD(0, java.lang.Double.longBitsToDouble(0x0807060504030201L)); x } =**= Array[Byte](1, 2, 3, 4, 5, 6, 7, 8)
    T ~ { val x = new Array[Int](2);  (Mem of x).setB(1, 9); x(1) } ==== 9

    // view / viewAs (zero-copy slices)
    T ~ (Mem of ai).view(1L, 4L).vec    =**= Vector(3, 5, 7)
    T ~ (Mem of ai).view(1L, 4L).length ==== 3L
    T ~ { val a = ai; val v = (Mem of a).view(1L, 4L); v(0) = 99; a(1) } ==== 99
    T ~ (Mem of Array(0x04030201, 0x08070605)).viewAs[Byte](1L, 2L).vec =**= Vector[Byte](5, 6, 7, 8)
    T ~ (Mem of ab).viewAs[Int](2L, 6L).vec                             =**= Vector(0x06050403)
    T ~ (Mem of ab).viewAs[Short](2L, 6L).length                        ==== 2L

    // inject into Array
    inline def intoA(k: Int)(inline f: Array[Int] => Any): Array[Int] =
      val a = new Array[Int](k)
      f(a)
      a
    T ~ intoA(7)(d => (Mem of ai).inject(d))            =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ intoA(7)(d => (Mem of ai).inject(d, 2))         =**= Array(0, 0, 2, 3, 5, 7, 11)
    T ~ intoA(7)(d => (Mem of ai).inject(d)(1L, 4L))    =**= Array(3, 5, 7, 0, 0, 0, 0)
    T ~ intoA(7)(d => (Mem of ai).inject(d, 2)(1L, 4L)) =**= Array(0, 0, 3, 5, 7, 0, 0)
    T ~ intoA(7)(d => (Mem of ai).inject(d)(ix))        =**= Array(7, 3, 11, 3, 0, 0, 0)
    T ~ intoA(7)(d => (Mem of ai).inject(d)(st))        =**= Array(7, 3, 11, 3, 0, 0, 0)
    T ~ intoA(7)(d => (Mem of ai).inject(d)(_ > 4))     =**= Array(5, 7, 11, 0, 0, 0, 0)
    T ~ (Mem of ai).inject(new Array[Int](7))           ==== 5L
    T ~ (Mem of ai).inject(new Array[Int](7))(ix)       ==== 4L
    T ~ (Mem of ai).inject(new Array[Int](7))(_ > 4)    ==== 3L

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

    // typed get/set (None / silent no-op when any byte would be out of range)
    def ab = Array[Byte](1, 2, 3, 4)
    T ~ (Mem of ab).clip.getB(3)  ==== Some(4.toByte)
    T ~ (Mem of ab).clip.getB(4)  ==== None
    T ~ (Mem of ab).clip.getB(-1) ==== None
    T ~ (Mem of ab).clip.getS(2)  ==== Some(0x0403.toShort)
    T ~ (Mem of ab).clip.getS(3)  ==== None
    T ~ (Mem of ab).clip.getC(2)  ==== Some(0x0403.toChar)
    T ~ (Mem of ab).clip.getI(0)  ==== Some(0x04030201)
    T ~ (Mem of ab).clip.getI(1)  ==== None
    T ~ (Mem of ab).clip.getF(0)  ==== Some(java.lang.Float.intBitsToFloat(0x04030201))
    T ~ (Mem of ab).clip.getL(0)  ==== None
    T ~ (Mem of ab).clip.getD(0)  ==== None
    T ~ (Mem of Array(0x04030201, 0x08070605)).clip.getL(0) ==== Some(0x0807060504030201L)
    T ~ (Mem of Array(0x04030201, 0x08070605)).clip.getL(1) ==== None
    T ~ (Mem of Array(0x04030201, 0x08070605)).clip.getB(1) ==== Some(5.toByte)
    T ~ { val x = new Array[Byte](4); (Mem of x).clip.setI(0, 0x04030201);          x } =**= Array[Byte](1, 2, 3, 4)
    T ~ { val x = new Array[Byte](4); (Mem of x).clip.setI(1, 0x04030201);          x } =**= Array[Byte](0, 0, 0, 0)
    T ~ { val x = new Array[Byte](4); (Mem of x).clip.setB(-1, 9);                  x } =**= Array[Byte](0, 0, 0, 0)
    T ~ { val x = new Array[Byte](4); (Mem of x).clip.setS(3, 0x0302.toShort);      x } =**= Array[Byte](0, 0, 0, 0)
    T ~ { val x = new Array[Byte](4); (Mem of x).clip.setC(2, 0x0302.toChar);       x } =**= Array[Byte](0, 0, 2, 3)
    T ~ { val x = new Array[Byte](4); (Mem of x).clip.setL(0, 1L);                  x } =**= Array[Byte](0, 0, 0, 0)
    T ~ { val x = new Array[Byte](4); (Mem of x).clip.setF(0, java.lang.Float.intBitsToFloat(0x04030201)); x } =**= Array[Byte](1, 2, 3, 4)
    T ~ { val x = new Array[Byte](4); (Mem of x).clip.setD(0, 1.0);                 x } =**= Array[Byte](0, 0, 0, 0)

    // inject into Array (clamped offsets/ranges & index-skipping)
    inline def intoA(k: Int)(inline f: Array[Int] => Any): Array[Int] =
      val a = new Array[Int](k)
      f(a)
      a
    T ~ intoA(7)(d => (Mem of ai).clip.inject(d))           =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ intoA(7)(d => (Mem of ai).clip.inject(d, -5))       =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ intoA(7)(d => (Mem of ai).clip.inject(d)(1L, 99L))  =**= Array(3, 5, 7, 11, 0, 0, 0)
    T ~ intoA(7)(d => (Mem of ai).clip.inject(d)(ix))       =**= Array(3, 7, 0, 0, 0, 0, 0)
    T ~ intoA(7)(d => (Mem of ai).clip.inject(d)(_ > 4))    =**= Array(5, 7, 11, 0, 0, 0, 0)
    T ~ intoA(3)(d => (Mem of ai).clip.inject(d))           =**= Array(2, 3, 5)
    T ~ intoA(3)(d => (Mem of ai).clip.inject(d, 2))        =**= Array(0, 0, 2)
    T ~ (Mem of ai).clip.inject(new Array[Int](3))          ==== 3L
    T ~ (Mem of ai).clip.inject(new Array[Int](7))(ix)      ==== 2L
    T ~ (Mem of ai).clip.inject(new Array[Int](2))(_ > 4)   ==== 2L

    // visitCuts (clamped)
    def acut = Array(1, 1, 2, 2, 2, 3)
    T ~ n{ (Mem of acut).clip.visitCuts(1L, 5L)((x, y) => x != y)((i, j) => cuml += 1) } ==== 2

  def memWhereIsTest(): Unit =
    // Bytes: a match at every position exercises every SWAR lane plus the sub-lane tail loops
    val ab = Array.tabulate(29)(i => (i % 7).toByte)
    val mb = Mem of ab
    var i = 0
    while i < ab.length do
      T ~ mb.whereIsFwd(i, ab.length)(ab(i)) ==== i.toLong
      T ~ mb.whereIsBkw(0L, i + 1)(ab(i))    ==== i.toLong
      i += 1
    T ~ mb.whereIsFwd(0L, ab.length)(9: Byte) ==== -1L
    T ~ mb.whereIsBkw(0L, ab.length)(9: Byte) ==== -1L
    T ~ mb.whereIsFwd(0L, ab.length)(3: Byte) ==== 3L
    T ~ mb.whereIsBkw(0L, ab.length)(3: Byte) ==== 24L
    T ~ mb.whereIsFwd(4L, ab.length)(3: Byte) ==== 10L
    T ~ mb.whereIsBkw(4L, 10L)(3: Byte)       ==== -1L
    T ~ mb.whereIsFwd(-5L, 99L)(6: Byte)      ==== 6L    // bounds clamp
    T ~ mb.whereIsBkw(-5L, 99L)(6: Byte)      ==== 27L

    // Shorts: distinct hi/lo bytes at every position; element-aligned lanes must not see
    // the same bit pattern straddling two elements
    val as = Array.tabulate(21)(i => ((i << 8) | (0x40 + i)).toShort)
    val ms = Mem of as
    i = 0
    while i < as.length do
      T ~ ms.whereIsFwd(0L, as.length)(as(i)) ==== i.toLong
      T ~ ms.whereIsBkw(0L, as.length)(as(i)) ==== i.toLong
      i += 1
    T ~ ms.whereIsFwd(0L, as.length)(0x4100.toShort) ==== -1L   // exists only as a straddle
    T ~ ms.whereIsFwd(5L, 12L)(as(7)) ==== 7L
    T ~ ms.whereIsBkw(5L, 12L)(as(7)) ==== 7L
    T ~ ms.whereIsFwd(8L, 12L)(as(7)) ==== -1L
    T ~ ms.whereIsBkw(5L, 7L)(as(7))  ==== -1L

    // Chars ride the short lanes; non-ASCII is fine
    val ac = "abcdefghij☃klmnopqrs".toCharArray
    val mc = Mem of ac
    T ~ mc.whereIsFwd(0L, ac.length)('☃') ==== 10L
    T ~ mc.whereIsBkw(0L, ac.length)('k')      ==== 11L
    T ~ mc.whereIsFwd(0L, ac.length)('z')      ==== -1L

    // Ints: duplicates picked by direction; windows scale in elements, not bytes
    val am = Array.tabulate(11)(k => 0x11223300 + k)
    am(9) = am(2)
    val mm = Mem of am
    i = 0
    while i < am.length do
      if i != 9 then T ~ mm.whereIsFwd(0L, am.length)(am(i)) ==== i.toLong
      i += 1
    T ~ mm.whereIsBkw(0L, am.length)(am(2)) ==== 9L
    T ~ mm.whereIsFwd(3L, am.length)(am(2)) ==== 9L
    T ~ mm.whereIsFwd(3L, 9L)(am(2))        ==== -1L

    // Longs (plain compare loop)
    val al = Array.tabulate(9)(k => 0x0102030405060700L + k)
    val ml = Mem of al
    T ~ ml.whereIsFwd(0L, al.length)(al(5)) ==== 5L
    T ~ ml.whereIsBkw(0L, al.length)(al(5)) ==== 5L
    T ~ ml.whereIsFwd(6L, al.length)(al(5)) ==== -1L

    // Floats and Doubles match raw bits: NaN finds NaN, and 0.0 does not find -0.0
    val af = Array[Float](1.5f, Float.NaN, -0.0f, 0.0f, 1.5f)
    val mf = Mem of af
    T ~ mf.whereIsFwd(0L, af.length)(Float.NaN) ==== 1L
    T ~ mf.whereIsFwd(0L, af.length)(0.0f)      ==== 3L
    T ~ mf.whereIsBkw(0L, af.length)(1.5f)      ==== 4L
    val ad = Array[Double](2.25, Double.NaN, -0.0, 0.0, 2.25)
    val md = Mem of ad
    T ~ md.whereIsFwd(0L, ad.length)(Double.NaN) ==== 1L
    T ~ md.whereIsFwd(0L, ad.length)(0.0)        ==== 3L
    T ~ md.whereIsBkw(0L, ad.length)(2.25)       ==== 4L

    // Off-heap (native) segments take the same paths
    val mo = Mem.alloc[Byte](40)
    mo.set()(k => (k % 5).toByte)
    T ~ mo.whereIsFwd(0L, 40L)(4: Byte) ==== 4L
    T ~ mo.whereIsBkw(0L, 40L)(4: Byte) ==== 39L
    T ~ mo.whereIsFwd(0L, 40L)(7: Byte) ==== -1L

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

  def memAsInlinedDataTest(): Unit =
    import MemTest.{Count, Meter}
    var cuml = 0
    inline def n(inline f: => Unit): Int =
      cuml = 0
      f
      cuml

    val tc = Count.translucency.array
    def ai = tc.conceal(Array(2, 3, 5, 7, 11))
    def aj = tc.conceal(Array(10, 20, 30, 40, 50))
    val ix = Array[Long](3, 1, 4, 1)
    def st = Array[Long](3, 1, 4, 1).stepper

    // Wrap a fresh array, mutate through it, and return the revealed Ints for inspection.
    inline def mut(a: Array[Count.Type])(inline f: Mem.As[Count.Type] => Unit): Array[Int] =
      val m = Mem.As of a
      f(m)
      tc.reveal(a)
    inline def into(k: Int)(inline f: Mem.As[Count.Type] => Any): Array[Int] =
      val a = new Array[Int](k)
      val m = Mem.As of tc.conceal(a)
      f(m)
      a
    inline def intoA(k: Int)(inline f: Array[Count.Type] => Any): Array[Int] =
      val a = new Array[Int](k)
      f(tc.conceal(a))
      a

    // element access, length, reinterpretation, prim bridge
    T ~ (Mem.As of ai).length               ==== 5L
    T ~ (Mem.As of ai)(0).value             ==== 2
    T ~ (Mem.As of ai)(4).value             ==== 11
    T ~ (Mem.As of ai).segment.byteSize     ==== 20L
    T ~ (Mem.As of ai).as[Meter.Type]       ==== typed[Mem.As[Meter.Type]]
    T ~ (Mem.As of ai).as[Meter.Type].length ==== 2L
    T ~ (Mem.As of ai).prim                 ==== typed[Mem[Int]]
    T ~ (Mem.As of ai).prim.vec             =**= Vector(2, 3, 5, 7, 11)
    T ~ Mem.As.bytesOf[Count.Type]          ==== 4L
    T ~ Mem.As.bytesOf[Meter.Type]          ==== 8L
    T ~ Mem.As.bytesOf[Int]                 ==== 4L
    T ~ mut(ai){ m => m(2) = Count(99) }    =**= Array(2, 3, 99, 7, 11)

    // loop families with O-typed callbacks
    T ~ n{ (Mem.As of ai).use()(x => cuml += x.value) }                     ==== 28
    T ~ n{ (Mem.As of ai).use(1L, 4L)(x => cuml += x.value) }               ==== 15
    T ~ n{ (Mem.As of ai).use(ix)(x => cuml += x.value) }                   ==== 24
    T ~ n{ (Mem.As of ai).use(st)(x => cuml += x.value) }                   ==== 24
    T ~ n{ (Mem.As of ai).use(x => x.value > 4)(x => cuml += x.value) }     ==== 23
    T ~ mut(ai)(_.alter()(x => Count(x.value + 1)))                         =**= Array(3, 4, 6, 8, 12)
    T ~ mut(ai)(_.alter(ix)(x => Count(x.value + 1)))                       =**= Array(2, 5, 5, 8, 12)
    T ~ n{ (Mem.As of ai).visit()((x, i) => cuml += x.value + i.toInt) }    ==== 38
    T ~ mut(ai)(_.edit()((x, i) => Count(x.value + i.toInt)))               =**= Array(2, 4, 7, 10, 15)
    T ~ n{ (Mem.As of ai).pairs((x, y) => if y.value > x.value then cuml += 1) } ==== 4
    T ~ n{ (Mem.As of ai).trios((x, y, z) => cuml += 1) }                   ==== 3
    T ~ n{ (Mem.As of ai).together(Mem.As of aj)((x, y, i) => cuml += x.value + y.value + i.toInt) } ==== 188
    T ~ n{ (Mem.As of ai).together(Mem.As of aj, Mem.As of ai)((x, y, z, i) => cuml += x.value + y.value + z.value + i.toInt) } ==== 216
    T ~ n{ (Mem.As of ai).wander(){ (x, i) => cuml += x.value; i + 2 } __ Unit } ==== 18
    T ~ (Mem.As of ai).wander(){ (_, i) => i + 2 }                          ==== 3L
    T ~ (Mem.As of ai).gather(0)()((z, x, i) => z + x.value + i.toInt)      ==== 38

    // update family
    T ~ mut(ai)(_.update(Count(0)))                          =**= Array(0, 0, 0, 0, 0)
    T ~ mut(ai)(_.update(1L, 4L, Count(9)))                  =**= Array(2, 9, 9, 9, 11)
    T ~ mut(ai)(_.update(ix, Count(0)))                      =**= Array(2, 0, 5, 0, 0)
    T ~ mut(ai)(_.update(st, Count(0)))                      =**= Array(2, 0, 5, 0, 0)
    T ~ mut(ai)(_.update(x => x.value > 4, Count(0)))        =**= Array(2, 3, 0, 0, 0)
    T ~ mut(ai)(_.update(Mem.As of tc.conceal(Array(9, 9, 9, 9, 9))))       =**= Array(9, 9, 9, 9, 9)
    T ~ mut(ai)(_.update(1L, 4L, Mem.As of tc.conceal(Array(100, 200, 300)))) =**= Array(2, 100, 200, 300, 11)

    // set family
    T ~ mut(ai)(_.set()(() => Count(7)))            =**= Array(7, 7, 7, 7, 7)
    T ~ mut(ai)(_.set()(i => Count(i.toInt)))       =**= Array(0, 1, 2, 3, 4)
    T ~ mut(ai)(_.set(1L, 4L)(() => Count(0)))      =**= Array(2, 0, 0, 0, 11)
    T ~ mut(ai)(_.set(ix)(() => Count(9)))          =**= Array(2, 9, 5, 9, 9)

    // where family
    T ~ (Mem.As of ai).where(x => x.value > 4)                              =**= Array[Long](2, 3, 4)
    T ~ (Mem.As of ai).whereOp((x, i) => if x.value > 4 then i else -1L)    =**= Array[Long](2, 3, 4)
    T ~ (Mem.As of ai).whereIn(1L, 4L)(x => x.value > 4)                    =**= Array[Long](2, 3)
    T ~ (Mem.As of ai).whereFrom(ix)(x => x.value > 4)                      =**= Array[Long](3, 4)
    T ~ (Mem.As of ai).whereFwd(0L)(x => x.value % 2 == 1)                  ==== 1L
    T ~ (Mem.As of ai).whereBkw(4L)(x => x.value % 2 == 0)                  ==== 0L
    T ~ (Mem.As of ai).whereFwd(0L)(x => x.value == 999)                    ==== -1L

    // whereIs seeks on the backing primitive
    def ad = tc.conceal(Array(2, 3, 5, 3, 11))
    T ~ (Mem.As of ai).whereIsFwd(0L, 5L)(Count(7))  ==== 3L
    T ~ (Mem.As of ai).whereIsFwd(0L, 5L)(Count(4))  ==== -1L
    T ~ (Mem.As of ad).whereIsFwd(0L, 5L)(Count(3))  ==== 1L
    T ~ (Mem.As of ad).whereIsBkw(0L, 5L)(Count(3))  ==== 3L

    // typed get/set ride the backing primitive's width
    T ~ (Mem.As of ai).getI(1)                  ==== 3
    T ~ (Mem.As of ai).getB(1)                  ==== 3.toByte
    T ~ (Mem.As of ai).getL(0)                  ==== 0x0000000300000002L
    T ~ into(2)(m => m.setI(1, 42))             =**= Array(0, 42)

    // view / viewAs share storage
    T ~ (Mem.As of ai).view(1L, 4L).prim.vec    =**= Vector(3, 5, 7)
    T ~ { val a = ai; val v = (Mem.As of a).view(1L, 4L); v(0) = Count(99); tc.reveal(a)(1) } ==== 99
    T ~ (Mem.As of ai).viewAs[Byte](0L, 1L).length ==== 4L

    // inject into Array[O] and Mem.As destinations
    T ~ intoA(7)(d => (Mem.As of ai).inject(d))                 =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ intoA(7)(d => (Mem.As of ai).inject(d, 2))              =**= Array(0, 0, 2, 3, 5, 7, 11)
    T ~ intoA(7)(d => (Mem.As of ai).inject(d)(1L, 4L))         =**= Array(3, 5, 7, 0, 0, 0, 0)
    T ~ intoA(7)(d => (Mem.As of ai).inject(d)(ix))             =**= Array(7, 3, 11, 3, 0, 0, 0)
    T ~ intoA(7)(d => (Mem.As of ai).inject(d)(x => x.value > 4)) =**= Array(5, 7, 11, 0, 0, 0, 0)
    T ~ (Mem.As of ai).inject(tc.conceal(new Array[Int](7)))    ==== 5L
    T ~ into(7)(d => (Mem.As of ai).inject(d))                  =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ into(7)(d => (Mem.As of ai).inject(d, 2L))              =**= Array(0, 0, 2, 3, 5, 7, 11)
    T ~ into(7)(d => (Mem.As of ai).inject(d)(1L, 4L))          =**= Array(3, 5, 7, 0, 0, 0, 0)
    T ~ into(7)(d => (Mem.As of ai).inject(d)(st))              =**= Array(7, 3, 11, 3, 0, 0, 0)
    T ~ into(7)(d => (Mem.As of ai).inject(d)(x => x.value > 4)) =**= Array(5, 7, 11, 0, 0, 0, 0)

    // injectOp across element types (Count -> Meter)
    val md = Mem.As.alloc[Meter.Type](5)
    T ~ (Mem.As of ai).injectOp(md)()((x, i) => Meter(x.value + i.toDouble)) ==== 5L
    T ~ md.prim ==== typed[Mem[Double]]
    T ~ md.prim.vec =**= Vector(2.0, 4.0, 7.0, 10.0, 15.0)

    // visitCuts
    def acut = tc.conceal(Array(1, 1, 2, 2, 2, 3))
    T ~ n{ (Mem.As of acut).visitCuts()((x, y) => x.value != y.value)((i, j) => cuml += 1) } ==== 3

    // alloc with an opaque element type; Double backing takes the long-compare seek path
    val mm = Mem.As.alloc[Meter.Type](4)
    mm.set()(i => Meter(i.toDouble * 1.5))
    T ~ mm.length ==== 4L
    T ~ mm.prim.vec =**= Vector(0.0, 1.5, 3.0, 4.5)
    T ~ mm.whereIsFwd(0L, 4L)(Meter(3.0)) ==== 2L
    T ~ mm.whereIsBkw(0L, 4L)(Meter(9.0)) ==== -1L

    // raw primitives are also valid As elements (As is a superset; Mem stays primitive-only)
    val mp = Mem.As.alloc[Int](3)
    mp.set()(i => i.toInt * 2)
    T ~ mp(2)   ==== 4
    T ~ mp.prim ==== typed[Mem[Int]]

    // a two-link chain: label over opaque-over-Int
    type Nc = Count.Type \ "n"
    val tn = summon[Translucent[Nc, Count.Type]]
    val an = tn.array.conceal(ai)
    val mn = Mem.As of an
    T ~ Mem.As.bytesOf[Nc]  ==== 4L
    T ~ mn.length           ==== 5L
    T ~ mn(2)               ==== typed[Nc]
    T ~ mn(2).unlabel.value ==== 5
    T ~ mn.prim             ==== typed[Mem[Int]]
    T ~ mn.whereIsFwd(0L, 5L)(\.wrap(Count(7))["n"]) ==== 3L
    T ~ n{ mn.use()(x => cuml += x.unlabel.value) }  ==== 28

    // non-primitive and Boolean element types are rejected at compile time
    T ~ compiletime.testing.typeChecks("kse.basics.Mem.As.bytesOf[String]")  ==== false
    T ~ compiletime.testing.typeChecks("kse.basics.Mem.As.bytesOf[Boolean]") ==== false

  def memAsClippedTest(): Unit =
    import MemTest.Count
    var cuml = 0
    inline def n(inline f: => Unit): Int =
      cuml = 0
      f
      cuml

    val tc = Count.translucency.array
    def ai = tc.conceal(Array(2, 3, 5, 7, 11))
    val ix = Array[Long](1, 9, 3)
    def st = Array[Long](1, 9, 3).stepper

    inline def mut(a: Array[Count.Type])(inline f: ClippedMem.As[Count.Type] => Unit): Array[Int] =
      val ca = (Mem.As of a).clip
      f(ca)
      tc.reveal(a)
    inline def into(k: Int)(inline f: Mem.As[Count.Type] => Unit): Array[Int] =
      val a = new Array[Int](k)
      val m = Mem.As of tc.conceal(a)
      f(m)
      a
    inline def intoA(k: Int)(inline f: Array[Count.Type] => Unit): Array[Int] =
      val a = new Array[Int](k)
      f(tc.conceal(a))
      a

    T ~ (Mem.As of ai).clip(2)(Count(-1)).value  ==== 5
    T ~ (Mem.As of ai).clip(9)(Count(-1)).value  ==== -1
    T ~ (Mem.As of ai).clip(-1)(Count(-1)).value ==== -1
    T ~ (Mem.As of ai).clip.get(2).map(_.value)  ==== Some(5)
    T ~ (Mem.As of ai).clip.get(9)               ==== None
    T ~ (Mem.As of ai).clip.length               ==== 5L
    T ~ (Mem.As of ai).clip.unclip               ==== typed[Mem.As[Count.Type]]

    T ~ n{ (Mem.As of ai).clip.use(2)(x => cuml += x.value) }        ==== 5
    T ~ n{ (Mem.As of ai).clip.use(9)(x => cuml += x.value) }        ==== 0
    T ~ n{ (Mem.As of ai).clip.use(3L, 99L)(x => cuml += x.value) }  ==== 18
    T ~ n{ (Mem.As of ai).clip.use(-5L, 2L)(x => cuml += x.value) }  ==== 5
    T ~ n{ (Mem.As of ai).clip.use(ix)(x => cuml += x.value) }       ==== 10
    T ~ n{ (Mem.As of ai).clip.use(st)(x => cuml += x.value) }       ==== 10

    T ~ mut(ai)(_.alter(3L, 99L)(x => Count(x.value + 1)))           =**= Array(2, 3, 5, 8, 12)
    T ~ mut(ai)(_.alter(ix)(x => Count(x.value + 1)))                =**= Array(2, 4, 5, 8, 11)
    T ~ n{ (Mem.As of ai).clip.visit(3L, 99L)((x, i) => cuml += x.value + i.toInt) } ==== 25
    T ~ mut(ai)(_.edit(3L, 99L)((x, i) => Count(x.value + i.toInt))) =**= Array(2, 3, 5, 10, 15)
    T ~ (Mem.As of ai).clip.gather(0)(3L, 99L)((z, x, i) => z + x.value + i.toInt) ==== 25
    T ~ (Mem.As of ai).clip.gather(0)(ix)((z, x, i) => z + x.value + i.toInt)      ==== 14

    T ~ mut(ai)(_.update(3L, 99L, Count(0)))          =**= Array(2, 3, 5, 0, 0)
    T ~ mut(ai)(_.update(ix, Count(0)))               =**= Array(2, 0, 5, 0, 11)
    T ~ mut(ai)(_.set(3L, 99L)(() => Count(1)))       =**= Array(2, 3, 5, 1, 1)
    T ~ mut(ai)(_.set(ix)(j => Count(j.toInt)))       =**= Array(2, 1, 5, 3, 11)

    T ~ (Mem.As of ai).clip.whereIn(1L, 99L)(x => x.value > 4) =**= Array[Long](2, 3, 4)
    T ~ (Mem.As of ai).clip.whereFrom(ix)(x => x.value > 4)    =**= Array[Long](3)

    T ~ into(7)(d => (Mem.As of ai).clip.inject(d) __ Unit)           =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ into(7)(d => (Mem.As of ai).clip.inject(d, -5L) __ Unit)      =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ into(7)(d => (Mem.As of ai).clip.inject(d)(1L, 99L) __ Unit)  =**= Array(3, 5, 7, 11, 0, 0, 0)
    T ~ into(7)(d => (Mem.As of ai).clip.inject(d)(ix) __ Unit)       =**= Array(3, 7, 0, 0, 0, 0, 0)
    T ~ into(7)(d => (Mem.As of ai).clip.inject(d)(x => x.value > 4) __ Unit) =**= Array(5, 7, 11, 0, 0, 0, 0)
    T ~ into(3)(d => (Mem.As of ai).clip.inject(d) __ Unit)           =**= Array(2, 3, 5)
    T ~ intoA(7)(d => (Mem.As of ai).clip.inject(d) __ Unit)          =**= Array(2, 3, 5, 7, 11, 0, 0)
    T ~ intoA(3)(d => (Mem.As of ai).clip.inject(d) __ Unit)          =**= Array(2, 3, 5)

    T ~ (Mem.As of ai).clip.getI(1)  ==== Some(3)
    T ~ (Mem.As of ai).clip.getI(5)  ==== None
    T ~ (Mem.As of ai).clip.getL(3)  ==== Some(0x0000000B00000007L)
    T ~ (Mem.As of ai).clip.getL(4)  ==== None
    T ~ into(5)(m => m.clip.setI(2, 42)) =**= Array(0, 0, 42, 0, 0)
    T ~ into(5)(m => m.clip.setI(9, 42)) =**= Array(0, 0, 0, 0, 0)

    def acut = tc.conceal(Array(1, 1, 2, 2, 2, 3))
    T ~ n{ (Mem.As of acut).clip.visitCuts(1L, 99L)((x, y) => x.value != y.value)((i, j) => cuml += 1) } ==== 3

  def memAoSTest(): Unit =
    import MemTest.{Count, Meter}
    var cuml = 0
    inline def n(inline f: => Unit): Int =
      cuml = 0
      f
      cuml

    type S = (index: Int, count: Count.Type, meter: Meter.Type)

    // compile-time geometry: packed in declaration order, no padding
    T ~ Mem.AoS.strideOf[S]          ==== 16L
    T ~ Mem.AoS.offsetOf[S, "index"] ==== 0L
    T ~ Mem.AoS.offsetOf[S, "count"] ==== 4L
    T ~ Mem.AoS.offsetOf[S, "meter"] ==== 8L

    val xs = Mem.AoS.alloc[S](4)
    T ~ xs.length           ==== 4L
    T ~ xs.stride           ==== 16L
    T ~ xs.segment.byteSize ==== 64L

    // per-field dynamic read/write, primitive and translucent alike
    xs.index(0) = 7
    xs.count(0) = Count(3)
    xs.meter(0) = Meter(1.5)
    T ~ xs.index(0)       ==== 7
    T ~ xs.index(0)       ==== typed[Int]
    T ~ xs.count(0)       ==== typed[Count.Type]
    T ~ xs.count(0).value ==== 3
    T ~ xs.meter(0).value ==== 1.5

    // packing verified at the byte level (native LE order, as elsewhere in the suite)
    val raw = Mem.wrap[Byte](xs.segment)
    T ~ raw.getI(0) ==== 7
    T ~ raw.getI(4) ==== 3
    T ~ raw.getD(8) ==== 1.5

    // column views: strided loop ops over one field
    T ~ xs.meter ==== typed[Mem.AoS.Field[S, "meter"]]
    xs.index.set(i => i.toInt * 10)
    T ~ xs.index(3)      ==== 30
    T ~ xs.index.length  ==== 4L
    T ~ n{ xs.index.use(cuml += _) }                       ==== 60
    T ~ n{ xs.index.visit((x, i) => cuml += x + i.toInt) } ==== 66
    xs.count.set(i => Count(i.toInt + 1))
    T ~ xs.count.gather(0)((z, x, i) => z + x.value * (i.toInt + 1)) ==== 30
    xs.count.alter(x => Count(x.value * 2))
    T ~ xs.count(3).value ==== 8

    // whole-struct materialize and write-back as named tuples
    xs.meter.set(i => Meter(i * 0.5))
    T ~ xs(2) ==== typed[S]
    val x2 = (index = 20, count = Count(6), meter = Meter(1.0))
    T ~ xs(2) ==== x2
    xs(1) = (index = -5, count = Count(9), meter = Meter(2.25))
    T ~ xs.index(1)       ==== -5
    T ~ xs.count(1).value ==== 9
    T ~ xs.meter(1).value ==== 2.25

    // reinterpreting wrap over the same memory
    val ys = Mem.AoS.wrap[(a: Long, b: Long)](xs.segment)
    T ~ ys.length ==== 4L
    T ~ ys.a(0)   ==== 0x0000000200000000L

    // wrong names and wrong field types are compile errors
    T ~ compiletime.testing.typeChecks("xs.nope(0)")          ==== false
    T ~ compiletime.testing.typeChecks("xs.index(0) = \"a\"") ==== false

    // index loops: instance-typed indices, fields via the contextual owner array
    val zs = Mem.AoS.alloc[S](5)
    zs.index.set(i => i.toInt)
    zs.count.set(i => Count(i.toInt * 2))
    zs.meter.set(i => Meter(i * 0.25))
    T ~ n{ zs.use()(idx => cuml += idx.index + idx.count.value) }             ==== 30
    T ~ n{ zs.use(1L, 3L)(idx => cuml += idx.index) }                         ==== 3
    T ~ n{ zs.use(Array[Long](4, 0, 4))(idx => cuml += idx.index) }           ==== 8
    T ~ n{ zs.use(Array[Long](4, 0, 4).stepper)(idx => cuml += idx.index) }   ==== 8
    T ~ n{ zs.use()(idx => if idx.index % 2 == 0 then cuml += idx.index) }    ==== 6
    T ~ n{ zs.use()(idx => cuml += idx.index + idx.unwrap.toInt) }            ==== 20
    T ~ zs.gather(0)()((z, idx) => z + idx.count.value * (idx.unwrap.toInt + 1)) ==== 80
    T ~ zs.where(idx => idx.count.value >= 4)                                 =**= Array[Long](2, 3, 4)
    T ~ zs.whereIn(0L, 3L)(idx => idx.count.value >= 4)                       =**= Array[Long](2)
    T ~ zs.whereFrom(Array[Long](0, 3, 4))(idx => idx.count.value >= 4)       =**= Array[Long](3, 4)

    // writes through an index hit the struct it points to, so use subsumes alter/edit
    zs.use(): idx =>
      idx.index = idx.index + 10
      idx.meter = Meter(idx.meter.value * 2)
    T ~ zs.index(0)       ==== 10
    T ~ zs.index(4)       ==== 14
    T ~ zs.meter(2).value ==== 1.0
    T ~ zs.count(3).value ==== 6

    // a standalone cursor is a plain one-struct read/write view
    val c = zs.cursor(3)
    T ~ c.index ==== 13
    c.index = 99
    T ~ zs.index(3) ==== 99
    T ~ c.index     ==== 99
    T ~ c.meter     ==== typed[Meter.Type]
    T ~ compiletime.testing.typeChecks("c.nope")        ==== false
    T ~ compiletime.testing.typeChecks("c.index = \"a\"") ==== false

    // struct(i) is a detached copy: same view surface, independent memory
    val d = zs.struct(3)
    T ~ d             ==== typed[Mem.Struct[S]]
    T ~ d.index       ==== 99
    T ~ d.meter.value ==== 1.5
    d.index = 5
    T ~ d.index     ==== 5
    T ~ zs.index(3) ==== 99
    zs.index(3) = 41
    T ~ d.index ==== 5

    // pairs / trios: staggered indices, writes visible to later steps
    T ~ n{ zs.pairs((a, b) => cuml += b.index - a.index) }              ==== 4
    T ~ n{ zs.pairs((a, b) => cuml += a.count.value * b.count.value) }  ==== 80
    T ~ n{ zs.trios((a, b, c) => cuml += a.index + b.index + c.index) } ==== 164
    zs.pairs((a, b) => b.index = a.index + b.index)
    T ~ zs.index(1) ==== 21
    T ~ zs.index(4) ==== 88

    // aos reinterprets a Mem or Mem.As in place, sharing storage
    val bm = Mem.alloc[Byte](40)
    val ba = bm.aos[(a: Int, b: Int)]
    T ~ ba ==== typed[Mem.AoS[(a: Int, b: Int)]]
    T ~ ba.length ==== 5L
    ba.a(2) = 33
    T ~ bm.getI(16) ==== 33
    T ~ (Mem of Array(1, 2, 3, 4)).aos[(x: Int, y: Int)].y(1) ==== 4
    val am = Mem.As.alloc[Meter.Type](2)
    am(0) = Meter(1.5)
    am(1) = Meter(-2.0)
    val av = am.aos[(lo: Meter.Type, hi: Meter.Type)]
    T ~ av.length      ==== 1L
    T ~ av.lo(0).value ==== 1.5
    T ~ av.hi(0).value ==== -2.0

    // instance-typed indices: fields resolve through the contextual owner array
    val ws = Mem.AoS.alloc[S](3)
    ws.index.set(i => i.toInt * 3)
    ws.count.set(_ => Count(0))
    ws.meter.set(_ => Meter(0.0))
    var isum = 0
    ws.use()(idx => isum += idx.index)
    T ~ isum ==== 9
    ws.use(1L, 3L)(idx => idx.count = Count(idx.index + (idx - 1).index))
    T ~ ws.count(1).value ==== 3
    T ~ ws.count(2).value ==== 9
    T ~ n{ ws.use()(idx => cuml += idx.count.value) } ==== 12
    val wsIdx = new Mem.AoS.Index[S, ws.type](0L)
    T ~ wsIdx.unwrap ==== 0L
    T ~ (wsIdx + 2).unwrap ==== 2L
    // an index binds to its array: the wrong array's context (or none) does not compile
    T ~ compiletime.testing.typeChecks("ws.use()(idx => cuml += wsIdx.index)") ==== true
    T ~ compiletime.testing.typeChecks("zs.use()(idx => cuml += wsIdx.index)") ==== false
    T ~ compiletime.testing.typeChecks("cuml += wsIdx.index")                  ==== false
}
object MemTest {
  import kse.basics.*

  // Opaque element types for the Mem.As tests; NewType supplies the Translucent witness
  object Count extends NewType[Int] {}
  object Meter extends NewType[Double] {}
}
