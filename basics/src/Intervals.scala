// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2021-23 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.basics.intervals


import scala.annotation.targetName

import kse.basics._

opaque type Iv = Long
object Iv extends Translucent.Companion[Iv, Long] {
  def apply(i0: Int, iN: Int): Iv = 
    if i0 < 0 then throw new IllegalArgumentException(s"Interval start index must be non-negative, not $i0")
    (i0 & 0xFFFFFFFFL) | (iN.toLong << 32)

  def wrap(l: Long): Iv = l

  inline def of(inline r: scala.collection.immutable.Range): Iv = intervalMacroImpl.rangePackedInLong(r)

  extension (iv: Iv)
    inline def packedInLong: Long = iv
    inline def unwrap: Long = iv
    inline def i0: Int = (iv & 0xFFFFFFFFL).toInt
    inline def iN: Int = (iv >>> 32).toInt
    inline def isEmpty: Boolean = (iv & 0xFFFFFFFFL) >= (iv >>> 32)
    inline def contains(i: Int): Boolean = (i >= Iv.i0(iv)) && (i < Iv.iN(iv))
    def pr: String = s"${Iv.i0(iv)}..${Iv.iN(iv)}"
    inline def clipped[A](a: Array[A]): Iv =
      if (iv >>> 32).toInt >= a.length then apply((iv & 0xFFFFFFFFL).toInt, a.length) else iv
    inline def visit(inline f: Int => Unit): Unit =
      var i = (iv & 0xFFFFFFFFL).toInt
      val j = (iv >>> 32).toInt
      while i < j do
        f(i)
        i += 1

  val empty: Iv = 0L

  val emptyByteArray = new Array[Byte](0)
  val emptyShortArray = new Array[Short](0)
  val emptyCharArray = new Array[Char](0)
  val emptyIntArray = new Array[Int](0)
  val emptyLongArray = new Array[Long](0)
  val emptyFloatArray = new Array[Float](0)
  val emptyDoubleArray = new Array[Double](0)

  private[basics]
  transparent inline def pyApplyImpl[A](a: A, iv: Iv)(inline sz: A => Int, inline e: => A, inline cor: (A, Int, Int) => A): A =
    val len = sz(a)
    var j0 = iv.i0
    if j0 < 0 then j0 = len + j0
    var j1 = iv.i1
    if j1 < 0 then j1 = len + j1
    if j0 > j1 then e else cor(a, j0, j1+1)

  private[basics]
  transparent inline def pyUpdateValueImpl[A, X](a: A, x: X, iv: Iv)(inline sz: A => Int, inline arf: (A, Int, Int, X) => Unit): Unit =
    val len = sz(a)
    var j0 = iv.i0
    if j0 < 0 then j0 = len + j0
    var j1 = iv.i1
    if j1 < 0 then j1 = len + j1
    if j0 <= j1 then arf(a, j0, j1+1, x)

  private[basics]
  transparent inline def pyUpdateArrayImpl[A](a: A, u: A, iv: Iv)(inline sz: A => Int, inline sac: (A, Int, A, Int, Int) => Unit): Unit =
    val len = sz(a)
    var j0 = iv.i0
    if j0 < 0 then j0 = len + j0
    var j1 = iv.i1
    if j1 < 0 then j1 = len + j1
    if j0 <= j1 then sac(u, 0, a, j0, 1 + j1 - j0)
}

opaque type PIv = Long
object PIv {
  inline def apply(i: Int, j: Int): PIv = (i & 0xFFFFFFFFL) | (j.toLong << 32)
  inline def wrap(l: Long): PIv = l

  extension (piv: PIv)
    inline def unwrap: Long = piv
    def indexing[A](a: Array[A]): Iv =
      val i = ((piv: Long)&0xFFFFFFFFL).toInt
      val j = ((piv: Long) >>> 32).toInt
      Iv(if i < 0 then a.length+i else i, if j < 0 then a.length+j else j)
    def inside[A](a: Array[A]): Iv =
      var i = ((piv: Long)&0xFFFFFFFFL).toInt
      var j = ((piv: Long) >>> 32).toInt
      if i < 0 then i = 0 else if i > a.length then i = a.length
      if j < 0 then j = -1 else if j >= a.length then j = a.length-1
      Iv(i, j)
    inline def i0[A](a: Array[A]): Int =
      val i = ((piv: Long) & 0xFFFFFFFFL).toInt
      if i < 0 then a.length+i else i
    inline def iN[A](a: Array[A]): Int =
      val j = ((piv: Long) >>> 32).toInt
      if j < 0 then a.length+j+1 else if j < Int.MaxValue then j+1 else Int.MaxValue

}

opaque type BeforeEndIdx = Int
object BeforeEndIdx {
  inline def wrap(i: Int): BeforeEndIdx = i

  extension (idx: BeforeEndIdx)
    inline def unwrap: Int = idx

    @targetName("toLiteral")
    def to(j: Int): PIv =
      val i = (idx: Int)
      if i >= 0 then throw new IllegalArgumentException(s"Cannot index starting at length + $i")
      if j < 0  then throw new IllegalArgumentException(s"Cannot index ending at $j")
      PIv(i, j)

    @targetName("toBeforeEndIdx")
    def to(e: BeforeEndIdx): PIv =
      val i = (idx: Int)
      val j = (e: Int)
      if i >= 0 then throw new IllegalArgumentException(s"Cannot index starting at length + $i")
      if j >= 0 then throw new IllegalArgumentException(s"Cannot index ending at length + $j")
      PIv(i, j)

    @targetName("toEnd")
    def to(end: End.type): PIv =
      val i = (idx: Int)
      if i >= 0 then throw new IllegalArgumentException(s"Cannot index starting at length + $i")
      PIv(i, -1)
}


opaque type Pyx = Int
object Pyx extends Translucent.Companion[Pyx, Int] {
  inline def wrap(i: Int): kse.basics.Pyx = i

  extension (pyx: Pyx)
    inline def unwrap: Int = pyx
}


