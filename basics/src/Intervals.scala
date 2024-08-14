// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-23 Rex Kerr and Calico Life Sciences LLC.

package kse.basics.intervals


import scala.annotation.targetName

import kse.basics._

opaque type Iv = Long
object Iv extends Translucent.Companion[Iv, Long] {
  def apply(i0: Int, iN: Int): Iv =
    (i0 & 0xFFFFFFFFL) | (iN.toLong << 32)

  inline def wrap(l: Long): Iv = l

  inline infix def of(inline r: scala.collection.immutable.Range): Iv = basicsMacroImpl.rangePackedInLong(r)

  inline infix def of(s: String): Iv = apply(0, s.length)

  inline infix def of[A](a: Array[A]): Iv = apply(0, a.length)

  inline infix def of[A](inline v: kse.basics.intervals.Iv | kse.basics.intervals.PIv, a: Array[A]): Iv =
    inline v match
      case piv: kse.basics.intervals.PIv => piv of a
      case siv: kse.basics.intervals.Iv  => siv  

  inline infix def of[A](inline v: kse.basics.intervals.Iv | kse.basics.intervals.PIv, s: String): Iv =
    inline v match
      case piv: kse.basics.intervals.PIv => piv of s
      case siv: kse.basics.intervals.Iv  => siv

  inline infix def ofSize[A](inline v: kse.basics.intervals.Iv | kse.basics.intervals.PIv, n: Int): Iv =
    inline v match
      case piv: kse.basics.intervals.PIv => piv sized n
      case siv: kse.basics.intervals.Iv  => siv

  extension (iv: Iv)
    inline def unwrap: Long = iv
    inline def i0: Int = (iv & 0xFFFFFFFFL).toInt
    inline def iN: Int = (iv >>> 32).toInt
    inline def i0To(i: Int): Iv =
      (i & 0xFFFFFFFFL) | ((iv: Long) & 0xFFFFFFFF00000000L)
    inline def iNTo(i: Int): Iv =
      ((iv: Long) & 0xFFFFFFFFL) | (i.toLong << 32)
    inline def i0Op(inline f: Int => Int): Iv =
      (f(((iv: Long) & 0xFFFFFFFFL).toInt) & 0xFFFFFFFFL) | ((iv: Long) & 0xFFFFFFFF00000000L)
    inline def iNOp(inline f: Int => Int): Iv =
      ((iv: Long) & 0xFFFFFFFFL) | (f(((iv: Long) >>> 32).toInt).toLong << 32)
    inline def ops(inline f: Int => Int, inline g: Int => Int): Iv =
      (f(((iv: Long) & 0xFFFFFFFFL).toInt) & 0xFFFFFFFFL) | (g(((iv: Long) >>> 32).toInt).toLong << 32)
    def +#(i: Int): Iv =
      val i0 = (iv & 0xFFFFFFFFL).toInt
      val iN = (iv >>> 32).toInt
      var j = i
      if i >= 0 then
        if i0 + j < i0 then j = Int.MaxValue - i0
        if iN + j < iN then j = Int.MaxValue - iN
      else
        if i0 + j > i0 then j = Int.MinValue - i0
        if iN + j > iN then j = Int.MinValue - iN
      ((i0+j) & 0xFFFFFFFFL) | ((iN+j).toLong << 32)
    def -#(i: Int): Iv =
      val i0 = (iv & 0xFFFFFFFFL).toInt
      val iN = (iv >>> 32).toInt
      var j = i
      if i >= 0 then
        if i0 - j > i0 then j = i0 - Int.MinValue
        if iN - j > iN then j = iN - Int.MinValue
      else
        if i0 - j < i0 then j = i0 - Int.MaxValue
        if iN - j < iN then j = iN - Int.MaxValue
      ((i0-j) & 0xFFFFFFFFL) | ((iN-j).toLong << 32)
    def &(that: Iv): Iv =
      val i0 = (iv & 0xFFFFFFFFL)
      var i = (that & 0xFFFFFFFFL)
      if i0 > i then i = i0
      val iN = (iv >>> 32).toInt
      var j = (that >>> 32).toInt
      if iN < j then j = iN
      (i & 0xFFFFFFFFL) | (j.toLong << 32)
    def |(that: Iv): Iv =
      val i0 = (iv & 0xFFFFFFFFL)
      var i = (that & 0xFFFFFFFFL)
      val iN = (iv >>> 32).toInt
      var j = (that >>> 32).toInt
      if iN <= i0 then
        if j <= i then
          if i0 > i then i = i0
          if iN < j then j = iN
      else if j <= i then
        i = i0
        j = iN
      else
        if i0 < i then i = i0
        if iN > j then j = iN
      (i & 0xFFFFFFFFL) | (j.toLong << 32)
    inline def length: Int =
      val i = i0
      val j = iN
      if i < j then
        val n = j - i
        if n < 0 then Int.MaxValue else n
      else 0
    inline def isEmpty: Boolean = (iv & 0xFFFFFFFFL).toInt >= (iv >>> 32).toInt
    inline def contains(i: Int): Boolean = (i >= Iv.i0(iv)) && (i < Iv.iN(iv))
    def pr: String = s"${Iv.i0(iv)}..${Iv.iN(iv)}"
    def clippedToSize(n: Int): Iv =
      val i = i0
      val j = iN
      if i < 0 then
        if j > n then n.toLong << 32
        else if j <= 0 then 0L
        else j.toLong << 32
      else if j > n then
        if i > n then (n & 0xFFFFFFFFL) | (n.toLong << 32)
        else (i & 0xFFFFFFFFL) | (n.toLong << 32)
      else iv
    inline def clippedTo[A](a: Array[A]): Iv = clippedToSize(a.length)
    inline def clippedTo(a: String): Iv = clippedToSize(a.length)
    def shiftIntoSize(n: Int): Iv =
      if n <= 0 then 0L
      else
        var i = i0
        var j = iN
        if j <= i then
          if i < 0 then 0L
          else
            if i >= n then i = n
            (i & 0xFFFFFFFFL) | (i.toLong << 32)
        else if i < 0 then
          j -= i
          if j < 0 || j > n then j = n
          j.toLong << 32
        else if j > n then
          i -= j - n
          if i < 0 || i > j then i = 0
          (i & 0xFFFFFFFFL) | (n.toLong << 32)
        else iv
    inline def shiftInto[A](a: Array[A]): Iv = shiftIntoSize(a.length)
    inline def shiftInto(a: String): Iv = shiftIntoSize(a.length)

    inline def visit(inline f: Int => Unit): Unit =
      var i = (iv & 0xFFFFFFFFL).toInt
      val j = (iv >>> 32).toInt
      while i < j do
        f(i)
        i += 1

    def where(): Array[Int] =
      val i = (iv & 0xFFFFFFFFL).toInt
      val j = (iv >>> 32).toInt
      val a = new Array[Int](if j > i then j - i else 0)
      var k = i
      var h = 0
      while k < j do
        a(h) = k
        k += 1
        h += 1
      a

  val empty: Iv = 0L
}


opaque type PIv = Long
object PIv {
  inline def wrap(l: Long): PIv = l

  extension (piv: PIv)
    inline def unwrap: Long = piv

    def sized(n: Int): Iv =
      var i = ((piv: Long) & 0xFFFFFFFFL).toInt
      var j = ((piv: Long) >>> 32).toInt
      if i < 0 then i = n + i
      if j < 0 then
        if j > Int.MinValue then j = n + j + 1
      else if j < Int.MaxValue then j += 1
      Iv wrap ((i & 0xFFFFFFFFL) | (j.toLong << 32))

    inline def of[A](a: Array[A]): Iv = sized(a.length)

    inline def of(a: String): Iv = sized(a.length)

    def clippedToSize(n: Int): Iv =
      var i = ((piv: Long) & 0xFFFFFFFFL).toInt
      var j = ((piv: Long) >>> 32).toInt
      if i < 0 then
        i = n+i
        if i < 0 then i = 0
      if j < 0 then
        if j > Int.MinValue then j = n + j + 1
        if j < 0 then j = 0
      else if j < Int.MaxValue then j += 1
      if i > n then i = n
      if j > n then j = n
      Iv wrap ((i & 0xFFFFFFFFL) | (j.toLong << 32))

    inline def clippedTo[A](a: Array[A]): Iv = clippedToSize(a.length)

    def clippedTo(a: String): Iv = clippedToSize(a.length)

  val all: PIv = 0xFFFFFFFF00000000L
}

