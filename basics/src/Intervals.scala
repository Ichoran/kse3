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

  inline def of(inline r: scala.collection.immutable.Range): Iv = intervalMacroImpl.rangePackedInLong(r)

  inline def of[A](inline v: kse.basics.intervals.Iv | kse.basics.intervals.PIv, a: Array[A]): Iv =
    inline v match
      case piv: kse.basics.intervals.PIv => piv of a
      case siv: kse.basics.intervals.Iv  => siv  

  inline def of[A](inline v: kse.basics.intervals.Iv | kse.basics.intervals.PIv, s: String): Iv =
    inline v match
      case piv: kse.basics.intervals.PIv => piv of s
      case siv: kse.basics.intervals.Iv  => siv  

  extension (iv: Iv)
    inline def unwrap: Long = iv
    inline def i0: Int = (iv & 0xFFFFFFFFL).toInt
    inline def iN: Int = (iv >>> 32).toInt
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
    def clipped[A](a: Array[A]): Iv =
      val i = i0
      val j = iN
      if i < 0 then
        if j > a.length then a.length.toLong << 32
        else j.toLong << 32
      else if j > a.length then
        (i & 0xFFFFFFFFL) | (a.length.toLong << 32)
      else iv
    def clipped(a: String): Iv =
      val i = i0
      val j = iN
      if i < 0 then
        if j > a.length then a.length.toLong << 32
        else j.toLong << 32
      else if j > a.length then
        (i & 0xFFFFFFFFL) | (a.length.toLong << 32)
      else iv
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
}


opaque type PIv = Long
object PIv {
  inline def wrap(l: Long): PIv = l

  extension (piv: PIv)
    inline def unwrap: Long = piv

    def of[A](a: Array[A]): Iv =
      var i = ((piv: Long) & 0xFFFFFFFFL).toInt
      var j = ((piv: Long) >>> 32).toInt
      if i < 0 then i = a.length+i
      if j < 0 then
        if j > Int.MinValue then j = a.length + j + 1
      else if j < Int.MaxValue then j += 1
      Iv wrap ((i & 0xFFFFFFFFL) | (j.toLong << 32))

    def of(a: String): Iv =
      var i = ((piv: Long) & 0xFFFFFFFFL).toInt
      var j = ((piv: Long) >>> 32).toInt
      if i < 0 then i = a.length+i
      if j < 0 then
        if j > Int.MinValue then j = a.length + j + 1
      else if j < Int.MaxValue then j += 1
      Iv wrap ((i & 0xFFFFFFFFL) | (j.toLong << 32))

  val all: PIv = 0xFFFFFFFF00000000L
}

