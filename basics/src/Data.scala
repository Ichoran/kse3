// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2021-23 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.basics

import scala.annotation.targetName
import scala.reflect.ClassTag
import scala.util.boundary

import kse.basics.intervals._

////////////////////////////////////////////////
/// Packaging and wrappers to alter behavior ///
////////////////////////////////////////////////


object ArrayReform {
  private def checkBounds(a: Int, i0: Int, iN: Int, b: Int, j0: Int, scale: Int): Int =
    if i0 < iN then
      val n =
        if scale > 0 then (iN - i0) >>> scale
        else
          val l = (iN.toLong - i0) << (-scale)
          if l > Int.MaxValue - 7 then throw new ArrayIndexOutOfBoundsException(b)
          else l.toInt
      if i0 < 0 || iN > a then throw new ArrayIndexOutOfBoundsException(if i0 < 0 then i0 else a)
      if j0 < 0 || j0 > b - n then throw new ArrayIndexOutOfBoundsException(if j0 < 0 then j0 else b)
      n
    else if i0 > iN then throw new NegativeArraySizeException()
    else 0

  def rangeIntoInts(ab: Array[Byte], i0: Int, iN: Int)(target: Array[Int], where: Int): target.type =
    val n = checkBounds(ab.length, i0, iN, target.length, where, 2)
    var i = i0
    var j = 0
    while j < n do
      target(j + where) = (ab(i) & 0xFF) | ((ab(i+1) & 0xFF) << 8) | ((ab(i+2) & 0xFF) << 16) | (ab(i+3) << 24)
      i += 4
      j += 1
    target

  def rangeToInts(ab: Array[Byte], i0: Int, iN: Int): Array[Int] =
    val n = checkBounds(ab.length, i0, iN, Int.MaxValue, 0, 2)
    rangeIntoInts(ab, i0, iN)(new Array[Int](n), 0)

  inline def toInts(ab: Array[Byte]): Array[Int] = rangeToInts(ab, 0, ab.length)


  def rangeIntoFloats(ab: Array[Byte], i0: Int, iN: Int)(target: Array[Float], where: Int): target.type =
    val n = checkBounds(ab.length, i0, iN, target.length, where, 2)
    var i = i0
    var j = 0
    while j < n do
      target(j + where) = java.lang.Float.intBitsToFloat(
        (ab(i) & 0xFF) | ((ab(i+1) & 0xFF) << 8) | ((ab(i+2) & 0xFF) << 16) | (ab(i+3) << 24)
      )
      i += 4
      j += 1
    target

  def rangeToFloats(ab: Array[Byte], i0: Int, iN: Int): Array[Float] =
    val n = checkBounds(ab.length, i0, iN, Int.MaxValue, 0, 2)
    rangeIntoFloats(ab, i0, iN)(new Array[Float](n), 0)

  inline def toFloats(ab: Array[Byte]): Array[Float] = rangeToFloats(ab, 0, ab.length)


  def rangeIntoLongs(ab: Array[Byte], i0: Int, iN: Int)(target: Array[Long], where: Int): target.type =
    val n = checkBounds(ab.length, i0, iN, target.length, where, 3)
    var i = i0
    var j = 0
    while j < n do
      target(j + where) =
        ((ab(i  ) & 0xFF) | ((ab(i+1) & 0xFF) << 8) | ((ab(i+2) & 0xFF) << 16) | ((ab(i+3) & 0xFFL) << 24)) |
        (((ab(i+4) & 0xFF) | ((ab(i+5) & 0xFF) << 8) | ((ab(i+6) & 0xFF) << 16) | ((ab(i+7) & 0xFFL) << 24)) << 32)
      i += 8
      j += 1
    target

  def rangeToLongs(ab: Array[Byte], i0: Int, iN: Int): Array[Long] =
    val n = checkBounds(ab.length, i0, iN, Int.MaxValue, 0, 3)
    rangeIntoLongs(ab, i0, iN)(new Array[Long](n), 0)

  inline def toLongs(ab: Array[Byte]): Array[Long] = rangeToLongs(ab, 0, ab.length)


  def rangeIntoDoubles(ab: Array[Byte], i0: Int, iN: Int)(target: Array[Double], where: Int): target.type =
    val n = checkBounds(ab.length, i0, iN, target.length, where, 3)
    var i = i0
    var j = 0
    while j < n do
      target(j + where) = java.lang.Double.longBitsToDouble(
        ((ab(i  ) & 0xFF) | ((ab(i+1) & 0xFF) << 8) | ((ab(i+2) & 0xFF) << 16) | ((ab(i+3) & 0xFFL) << 24)) |
        (((ab(i+4) & 0xFF) | ((ab(i+5) & 0xFF) << 8) | ((ab(i+6) & 0xFF) << 16) | ((ab(i+7) & 0xFFL) << 24)) << 32)
      )
      i += 8
      j += 1
    target

  def rangeToDoubles(ab: Array[Byte], i0: Int, iN: Int): Array[Double] =
    val n = checkBounds(ab.length, i0, iN, Int.MaxValue, 0, 3)
    rangeIntoDoubles(ab, i0, iN)(new Array[Double](n), 0)

  inline def toDoubles(ab: Array[Byte]): Array[Double] = rangeToDoubles(ab, 0, ab.length)


  def rangeIntoBytes(ai: Array[Int], i0: Int, iN: Int)(target: Array[Byte], where: Int): target.type =
    checkBounds(ai.length, i0, iN, target.length, where, -2)
    var i = i0
    var j = where
    while i < iN do
      if j > target.length - 4 then return target
      val x = ai(i)
      target(j  ) = ( x         & 0xFF).toByte
      target(j+1) = ((x >>>  8) & 0xFF).toByte
      target(j+2) = ((x >>> 16) & 0xFF).toByte
      target(j+3) = ( x >>> 24        ).toByte
      i += 1
      j += 4
    target

  def rangeToBytes(ai: Array[Int], i0: Int, iN: Int): Array[Byte] =
    val n = checkBounds(ai.length, i0, iN, Int.MaxValue - 7, 0, -2)
    rangeIntoBytes(ai, i0, iN)(new Array[Byte](n.toInt), 0)

  inline def toBytes(ai: Array[Int]): Array[Byte] = rangeToBytes(ai, 0, ai.length)


  def rangeIntoBytes(af: Array[Float], i0: Int, iN: Int)(target: Array[Byte], where: Int): target.type =
    checkBounds(af.length, i0, iN, target.length, where, -2)
    var i = i0
    var j = where
    while i < iN do
      if j > target.length - 4 then return target
      val x = java.lang.Float.floatToRawIntBits(af(i))
      target(j  ) = ( x         & 0xFF).toByte
      target(j+1) = ((x >>>  8) & 0xFF).toByte
      target(j+2) = ((x >>> 16) & 0xFF).toByte
      target(j+3) = ( x >>> 24        ).toByte
      i += 1
      j += 4
    target

  def rangeToBytes(af: Array[Float], i0: Int, iN: Int): Array[Byte] =
    val n = checkBounds(af.length, i0, iN, Int.MaxValue - 7, 0, -2)
    rangeIntoBytes(af, i0, iN)(new Array[Byte](n.toInt), 0)

  inline def toBytes(af: Array[Float]): Array[Byte] = rangeToBytes(af, 0, af.length)


  def rangeIntoBytes(al: Array[Long], i0: Int, iN: Int)(target: Array[Byte], where: Int): target.type =
    checkBounds(al.length, i0, iN, target.length, where, -3)
    var i = i0
    var j = where
    while i < iN do
      if j > target.length - 8 then return target
      val x = (al(i) & 0xFFFFFFFFL).toInt
      val y = (al(i) >>> 32).toInt
      target(j  ) = ( x         & 0xFF).toByte
      target(j+1) = ((x >>>  8) & 0xFF).toByte
      target(j+2) = ((x >>> 16) & 0xFF).toByte
      target(j+3) = ( x >>> 24        ).toByte
      target(j+4) = ( y         & 0xFF).toByte
      target(j+5) = ((y >>>  8) & 0xFF).toByte
      target(j+6) = ((y >>> 16) & 0xFF).toByte
      target(j+7) = ( y >>> 24        ).toByte
      i += 1
      j += 4
    target

  def rangeToBytes(al: Array[Long], i0: Int, iN: Int): Array[Byte] =
    val n = checkBounds(al.length, i0, iN, Int.MaxValue - 7, 0, -3)
    rangeIntoBytes(al, i0, iN)(new Array[Byte](n.toInt), 0)

  inline def toBytes(al: Array[Long]): Array[Byte] = rangeToBytes(al, 0, al.length)


  def rangeIntoBytes(ad: Array[Double], i0: Int, iN: Int)(target: Array[Byte], where: Int): target.type =
    checkBounds(ad.length, i0, iN, target.length, where, -3)
    var i = i0
    var j = where
    while i < iN do
      if j > target.length - 8 then return target
      val l = java.lang.Double.doubleToRawLongBits(ad(i))
      val x = (l & 0xFFFFFFFFL).toInt
      val y = (l >>> 32).toInt
      target(j  ) = ( x         & 0xFF).toByte
      target(j+1) = ((x >>>  8) & 0xFF).toByte
      target(j+2) = ((x >>> 16) & 0xFF).toByte
      target(j+3) = ( x >>> 24        ).toByte
      target(j+4) = ( y         & 0xFF).toByte
      target(j+5) = ((y >>>  8) & 0xFF).toByte
      target(j+6) = ((y >>> 16) & 0xFF).toByte
      target(j+7) = ( y >>> 24        ).toByte
      i += 1
      j += 4
    target

  def rangeToBytes(ad: Array[Double], i0: Int, iN: Int): Array[Byte] =
    val n = checkBounds(ad.length, i0, iN, Int.MaxValue - 7, 0, -3)
    rangeIntoBytes(ad, i0, iN)(new Array[Byte](n.toInt), 0)

  inline def toBytes(ad: Array[Double]): Array[Byte] = rangeToBytes(ad, 0, ad.length)
}


opaque type FromLengthIdx = Int
object FromLengthIdx {
  inline def wrap(i: Int): kse.basics.FromLengthIdx = i

  extension (idx: FromLengthIdx)
    inline def unwrap: Int = idx

    inline def +(i: Int): kse.basics.FromLengthIdx = (idx: Int) + i
    inline def -(i: Int): kse.basics.FromLengthIdx = (idx: Int) - i
    inline def of[A](a: Array[A]): Int =
      val i: Int = idx
      if i < 0 then a.length+i else i

    @targetName("toLiteral")
    def to(j: Int): PIv =
      val i = (idx: Int)
      if i >= 0 then throw new IllegalArgumentException(s"Cannot index starting at length + $i")
      if j < 0  then throw new IllegalArgumentException(s"Cannot index ending at $j")
      PIv wrap ((i & 0xFFFFFFFFL) | (j.toLong << 32))

    @targetName("toFromLengthIdx")
    def to(e: kse.basics.FromLengthIdx): PIv =
      val i = (idx: Int)
      val j = (e: Int)
      if i >= 0 then throw new IllegalArgumentException(s"Cannot index starting at length + $i")
      if j >= 0 then throw new IllegalArgumentException(s"Cannot index ending at length + $j")
      PIv wrap ((i & 0xFFFFFFFFL) | (j.toLong << 32))

    @targetName("toEnd")
    def to(end: End.type): PIv =
      val i = (idx: Int)
      if i >= 0 then throw new IllegalArgumentException(s"Cannot index starting at length + $i")
      PIv wrap ((i & 0xFFFFFFFFL) | 0xFFFFFFFF00000000L)
}

object End {
  inline def -(i: Int): kse.basics.FromLengthIdx = FromLengthIdx.wrap(-1-i) 
  inline def +(i: Int): kse.basics.FromLengthIdx = FromLengthIdx.wrap(-1+i)
  inline def of[A](a: Array[A]): Int = a.length - 1

  @targetName("toLiteral")
  inline def to(j: Int): PIv = FromLengthIdx.to(FromLengthIdx.wrap(-1))(j)

  @targetName("toFromLengthIdx")
  inline def to(e: kse.basics.FromLengthIdx): PIv = FromLengthIdx.to(FromLengthIdx.wrap(-1))(e)

  @targetName("toEnd")
  inline def to(end: End.type): PIv = PIv.wrap(-1L)
}

extension (i: Int){
  @targetName("rangeTo")
  inline def to(j: Int): collection.immutable.Range.Inclusive =
    scala.runtime.RichInt(i).to(j)

  @targetName("toEndIdx")
  inline def to(end: End.type): PIv =
    if i < 0 then throw new IllegalArgumentException(s"Cannot index starting at $i")
    PIv wrap ((i & 0xFFFFFFFFL) | 0xFFFFFFFF00000000L)

  @targetName("toFromLengthIdx")
  inline def to(e: FromLengthIdx): PIv =
    val j = FromLengthIdx.unwrap(e)
    if i < 0 then throw new IllegalArgumentException(s"Cannot index starting at $i")
    if j >= 0 then throw new IllegalArgumentException(s"Cannot index ending at length + $j")
    PIv wrap ((i & 0xFFFFFFFFL) | (j.toLong << 32))
}



/** Higher-level high-speed array access (inlined) */
extension [A](a: Array[A]) {
  inline def apply(i: kse.basics.FromLengthIdx): A = a(i of a)
  inline def apply(e: End.type): A = a(a.length - 1)

  @targetName("update_FromLength")
  inline def update(i: kse.basics.FromLengthIdx, x: A): Unit =
    a(i of a) = x
  @targetName("update_End")
  inline def update(e: End.type, x: A): Unit =
    a(a.length - 1) = x

  inline def clip: kse.basics.ClippedArray[A] = ClippedArray wrap a

  inline def breakable: kse.basics.ShortcutArray[A] = ShortcutArray wrap a

  inline def visit()(inline f: (A, Int) => Unit): Unit =
    var i = 0
    while i < a.length do
      f(a(i), i)
      i += 1
  inline def visit(i0: Int, iN: Int)(inline f: (A, Int) => Unit): Unit =
    var i = i0
    while i < iN do
      f(a(i), i)
      i += 1
  inline def visit(v: Iv | PIv)(inline f: (A, Int) => Unit): Unit =
    val iv = inline v match
      case piv: PIv => piv.of(a)
      case siv: Iv  => siv
    visit(iv.i0, iv.iN)(f)
  inline def visit(inline rg: collection.immutable.Range)(inline f: (A, Int) => Unit): Unit =
    val iv = Iv of rg
    visit(iv.i0, iv.iN)(f)
  inline def visit(indices: Array[Int])(inline f: (A, Int) => Unit): Unit =
    var i = 0
    while i < indices.length do
      val j = indices(i)
      f(a(j), j)
      i += 1
  inline def visit(indices: scala.collection.IntStepper)(inline f: (A, Int) => Unit): Unit =
    while indices.hasStep do
      val j = indices.nextStep
      f(a(j), j)

  inline def wander()(inline f: (A, Int) => Int): Int =
    wander(0)(f)
  inline def wander(start: Int)(inline f: (A, Int) => Int): Int =
    var n = 0
    var i = 0
    while i >= 0 && i < a.length && n < Int.MaxValue do
      n += 1
      i = f(a(i), i)
    n

  inline def gather[Z]()(zero: Z)(inline f: (Z, A, Int) => Z) =
    var i = 0
    var z = zero
    while i < a.length do
      z = f(z, a(i), i)
      i += 1
    z
  inline def gather[Z](i0: Int, iN: Int)(zero: Z)(inline f: (Z, A, Int) => Z): Z =
    var i = i0
    var z = zero
    while i < iN do
      z = f(z, a(i), i)
      i += 1
    z
  inline def gather[Z](inline v: Iv | PIv)(zero: Z)(inline f: (Z, A, Int) => Z): Z =
    val iv = inline v match
      case piv: PIv => piv.of(a)
      case siv: Iv  => siv 
    gather(iv.i0, iv.iN)(zero)(f)
  inline def gather[Z](inline rg: collection.immutable.Range)(zero: Z)(inline f: (Z, A, Int) => Z): Z =
    val iv = Iv of rg
    gather(iv.i0, iv.iN)(zero)(f)
  inline def gather[Z](indices: Array[Int])(zero: Z)(inline f: (Z, A, Int) => Z): Z =
    var i = 0
    var z = zero
    while i < indices.length do
      val j = indices(i)
      z = f(z, a(j), j)
      i += 1
    z
  inline def gather[Z](indices: scala.collection.IntStepper)(zero: Z)(inline f: (Z, A, Int) => Z): Z =
    var z = zero
    while indices.hasStep do
      val j = indices.nextStep
      z = f(z, a(j), j)
    z

  @targetName("update_Iv_constant")
  inline def update(iv: Iv, value: A): Unit =
    var i = iv.i0
    val j = iv.iN
    while i < j do
      a(i) = value
      i += 1
  @targetName("update_Iv_array")
  inline def update(iv: Iv, values: Array[A]): Unit =
    val i = iv.i0
    java.lang.System.arraycopy(values, 0, a, i, iv.iN - i)

  @targetName("update_Range_constant")
  inline def update(inline rg: collection.immutable.Range, value: A): Unit =
    update(Iv of rg, value)
  @targetName("update_Range_array")
  inline def update(inline rg: collection.immutable.Range, values: Array[A]): Unit =
    update(Iv of rg, values)

  @targetName("update_Py_constant")
  inline def update(piv: PIv, value: A): Unit =
    update(piv of a, value)
  @targetName("update_Py_array")
  inline def update(piv: PIv, values: Array[A]): Unit =
    update(piv of a, values)

  @targetName("update_All_constant")
  inline def update(value: A): Unit =
    update(Iv(0, a.length), value)
  @targetName("update_All_array")
  inline def update(values: Array[A]): Unit =
    update(Iv(0, a.length), values)

  @targetName("update_Places_constant")
  inline def update(indices: Array[Int], value: A): Unit =
    var i = 0
    while i < indices.length do
      a(indices(i)) = value
      i += 1
  @targetName("update_Places_array")
  inline def update(indices: Array[Int], values: Array[A]): Unit =
    var i = 0
    while i < indices.length do
      a(indices(i)) = values(i)
      i += 1

  @targetName("update_Stepper_constant")
  inline def update(indices: scala.collection.IntStepper, value: A): Unit =
    while indices.hasStep do
      a(indices.nextStep) = value
  @targetName("update_Stepper_array")
  inline def update(indices: scala.collection.IntStepper, values: Array[A]): Unit =
    var i = 0
    while indices.hasStep do
      a(indices.nextStep) = values(i)
      i += 1

  @targetName("update_Selector")
  inline def update(inline pick: A => Boolean, value: A): Unit =
    var i = 0
    while i < a.length do
      if pick(a(i)) then a(i) = value
      i += 1

  @targetName("set_i0iN_generate")
  inline def set(i0: Int, iN: Int)(inline generator: () => A): Unit =
    var i = i0
    while i < iN do
      a(i) = generator()
      i += 1
  @targetName("set_i0iN_index")
  inline def set(i0: Int, iN: Int)(inline indexer: Int => A): Unit =
    var i = i0
    while i < iN do
      a(i) = indexer(i)
      i += 1
  @targetName("set_i0iN_function")
  inline def set(i0: Int, iN: Int)(inline function: (A, Int) => A): Unit =
    var i = i0
    while i < iN do
      a(i) = function(a(i), i)
      i += 1

  @targetName("set_Iv_generate")
  inline def set(iv: Iv)(inline generator: () => A): Unit =
    set(iv.i0, iv.iN)(generator)
  @targetName("set_Iv_index")
  inline def set(iv: Iv)(inline indexer: Int => A): Unit =
    set(iv.i0, iv.iN)(indexer)
  @targetName("set_Iv_function")
  inline def set(iv: Iv)(inline function: (A, Int) => A): Unit =
    set(iv.i0, iv.iN)(function)

  @targetName("set_Range_generate")
  inline def set(inline rg: collection.immutable.Range)(inline generator: () => A): Unit =
    val iv = Iv of rg
    set(iv.i0, iv.iN)(generator)
  @targetName("set_Range_index")
  inline def set(inline rg: collection.immutable.Range)(inline indexer: Int => A): Unit =
    val iv = Iv of rg
    set(iv.i0, iv.iN)(indexer)
  @targetName("set_Range_function")
  inline def set(inline rg: collection.immutable.Range)(inline function: (A, Int) => A): Unit =
    val iv = Iv of rg
    set(iv.i0, iv.iN)(function)

  @targetName("set_Py_generate")
  inline def set(piv: PIv)(inline generator: () => A): Unit =
    val iv = piv of a
    set(iv.i0, iv.iN)(generator)
  @targetName("set_Py_index")
  inline def set(piv: PIv)(inline indexer: Int => A): Unit =
    val iv = piv of a
    set(iv.i0, iv.iN)(indexer)
  @targetName("set_Py_function")
  inline def set(piv: PIv)(inline function: (A, Int) => A): Unit =
    val iv = piv of a
    set(iv.i0, iv.iN)(function)

  @targetName("set_All_generate")
  inline def set()(inline generator: () => A): Unit =
    set(0, a.length)(generator)
  @targetName("set_All_index")
  inline def set()(inline indexer: Int => A): Unit =
    set(0, a.length)(indexer)
  @targetName("set_All_function")
  inline def set()(inline function: (A, Int) => A): Unit =
    set(0, a.length)(function)

  @targetName("set_Places_generate")
  inline def set(indices: Array[Int])(inline generator: () => A): Unit =
    var i = 0
    while i < indices.length do
      a(indices(i)) = generator()
      i += 1
  @targetName("set_Places_index")
  inline def set(indices: Array[Int])(inline indexer: Int => A): Unit =
    var i = 0
    while i < indices.length do
      val j = indices(i)
      a(j) = indexer(j)
      i += 1
  @targetName("set_Places_function")
  inline def set(indices: Array[Int])(inline function: (A, Int) => A): Unit =
    var i = 0
    while i < indices.length do
      val j = indices(i)
      a(j) = function(a(j), j)
      i += 1

  @targetName("set_Stepper_generate")
  inline def set(indices: scala.collection.IntStepper)(inline generator: () => A): Unit =
    while indices.hasStep do
      a(indices.nextStep) = generator()
  @targetName("set_Stepper_index")
  inline def set(indices: scala.collection.IntStepper)(inline indexer: Int => A): Unit =
    while indices.hasStep do
      val j = indices.nextStep
      a(j) = indexer(j)
  @targetName("set_Stepper_function")
  inline def set(indices: scala.collection.IntStepper)(inline function: (A, Int) => A): Unit =
    while indices.hasStep do
      val j = indices.nextStep
      a(j) = function(a(j), j)

  @targetName("set_Selector_generate")
  inline def set(inline pick: A => Boolean)(inline generator: () => A): Unit =
    var i = 0
    while i < a.length do
      if pick(a(i)) then
        a(i) = generator()
      i += 1

  @targetName("set_Selector_generate")
  inline def set(inline pick: A => Boolean)(inline indexer: Int => A): Unit =
    var i = 0
    while i < a.length do
      if pick(a(i)) then
        a(i) = indexer(i)
      i += 1

  inline def where(inline pick: A => Boolean): Array[Int] =
    var ix = new Array[Int](if a.length <= 8 then a.length else 8)
    var i = 0
    var j = 0
    while i < a.length do
      if pick(a(i)) then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = i
        j += 1
      i += 1
    ix.shrinkTo(j)

  inline def enlargeTo(n: Int)(using ClassTag[A]): Array[A] =
    if n > a.length then
      val aa = new Array[A](n)
      System.arraycopy(a, 0, aa, 0, a.length)
      aa
    else a

  inline def shrinkTo(n: Int)(using ClassTag[A]): Array[A] =
    if n < a.length then
      val aa = new Array[A](n)
      System.arraycopy(a, 0, aa, 0, aa.length)
      aa
    else a

  inline def dup()(using ClassTag[A]): Array[A] =
    val aa = new Array[A](a.length)
    java.lang.System.arraycopy(a, 0, aa, 0, a.length)
    aa
  inline def dup(inline f: Array[A] => Unit)(using ClassTag[A]): Array[A] =
    val aa = new Array[A](a.length)
    java.lang.System.arraycopy(a, 0, aa, 0, a.length)
    f(aa)
    aa
  inline def dupWith[B](inline f: A => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](a.length)
    var i = 0
    while i < a.length do
      b(i) = f(a(i))
      i += 1
    b

  inline def addLeft(n: Int)(using ClassTag[A]): Array[A] =
    val m = if n > 0 then n else 0
    val aa = new Array[A](a.length + m)
    java.lang.System.arraycopy(a, 0, aa, m, a.length)
    aa
  inline def addLeft(n: Int, x: A)(using ClassTag[A]): Array[A] =
    val m = if n > 0 then n else 0
    val aa = new Array[A](a.length + m)
    java.lang.System.arraycopy(a, 0, aa, m, a.length)
    var i = 0
    while i < m do
      aa(i) = x
      i += 1
    aa
  inline def addLeft(n: Int, f: () => A)(using ClassTag[A]): Array[A] =
    val m = if n > 0 then n else 0
    val aa = new Array[A](a.length + m)
    java.lang.System.arraycopy(a, 0, aa, m, a.length)
    var i = 0
    while i < m do
      aa(i) = f()
      i += 1
    aa

  inline def addRight(n: Int)(using ClassTag[A]): Array[A] =
    val m = if n > 0 then n else 0
    val aa = new Array[A](a.length + m)
    java.lang.System.arraycopy(a, 0, aa, 0, a.length)
    aa
  inline def addRight(n: Int, x: A)(using ClassTag[A]): Array[A] =
    val m = if n > 0 then n else 0
    val aa = new Array[A](a.length + m)
    java.lang.System.arraycopy(a, 0, aa, 0, a.length)
    var i = 0
    while i < m do
      aa(i+a.length) = x
      i += 1
    aa
  inline def addRight(n: Int, f: () => A)(using ClassTag[A]): Array[A] =
    val m = if n > 0 then n else 0
    val aa = new Array[A](a.length + m)
    java.lang.System.arraycopy(a, 0, aa, 0, a.length)
    var i = 0
    while i < m do
      aa(i+a.length) = f()
      i += 1
    aa

  inline def inject(that: Array[A]): Int =
    java.lang.System.arraycopy(a, 0, that, 0, a.length)
    a.length
  inline def inject(that: Array[A], where: Int): Int =
    java.lang.System.arraycopy(a, 0, that, where, a.length)
    a.length
  inline def inject(that: Array[A])(i0: Int, iN: Int): Int =
    java.lang.System.arraycopy(a, i0, that, 0, iN-i0)
    iN - i0
  inline def inject(that: Array[A], where: Int)(i0: Int, iN: Int): Int =
    java.lang.System.arraycopy(a, i0, that, where, iN-i0)
    iN - i0
  inline def inject(that: Array[A])(inline v: Iv | PIv): Int =
    val iv = inline v match
      case piv: PIv => piv of a
      case siv: Iv  => siv
    inject(that, 0)(iv.i0, iv.iN)
  inline def inject(that: Array[A], where: Int)(inline v: Iv | PIv): Int =
    val iv = inline v match
      case piv: PIv => piv of a
      case siv: Iv  => siv
    inject(that, where)(iv.i0, iv.iN)
  inline def inject(that: Array[A])(inline rg: collection.immutable.Range): Int =
    val iv = Iv of rg
    inject(that, 0)(iv.i0, iv.iN)
  inline def inject(that: Array[A], where: Int)(inline rg: collection.immutable.Range): Int =
    val iv = Iv of rg
    inject(that, where)(iv.i0, iv.iN)
  inline def inject(that: Array[A])(indices: Array[Int]): Int =
    inject(that, 0)(indices)
  inline def inject(that: Array[A], where: Int)(indices: Array[Int]): Int =
    var i = 0
    var j = where
    while i < indices.length do
      that(j) = a(indices(i))
      i += 1
      j += 1
    i
  inline def inject(that: Array[A])(indices: scala.collection.IntStepper): Int =
    inject(that, 0)(indices)
  inline def inject(that: Array[A], where: Int)(indices: scala.collection.IntStepper): Int =
    var j = where
    while indices.hasStep do
      that(j) = a(indices.nextStep)
      j += 1
    j - where
  inline def inject(that: Array[A])(inline pick: A => Boolean): Int =
    inject(that, 0)(pick)
  inline def inject(that: Array[A], where: Int)(inline pick: A => Boolean): Int =
    var i = 0
    var j = where
    while i < a.length do
      val x = a(i)
      if pick(x) then
        that(j) = x
        j += 1 
      i += 1
    j - where

  inline def select(i0: Int, iN: Int)(using ClassTag[A]): Array[A] =
    val b = new Array[A](iN - i0)
    java.lang.System.arraycopy(a, i0, b, 0, b.length)
    b
  inline def select(inline v: Iv | PIv)(using ClassTag[A]): Array[A] =
    val iv = inline v match
      case siv: Iv  => siv
      case piv: PIv => piv.of(a)
    select(Iv.i0(iv), Iv.iN(iv))
  inline def select(inline rg: collection.immutable.Range)(using ClassTag[A]): Array[A] =
    select(Iv of rg)
  inline def select(indices: Array[Int])(using ClassTag[A]): Array[A] =
    val b = new Array[A](indices.length)
    var i = 0
    while i < indices.length do
      b(i) = a(indices(i))
      i += 1
    b
  inline def select(indices: scala.collection.IntStepper)(using ClassTag[A]): Array[A] =
    var b = new Array[A](if a.length <= 8 then a.length else 8)
    var j = 0
    while indices.hasStep do
      if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
      b(j) = a(indices.nextStep)
      j += 1
    b.shrinkTo(j)
  inline def select(inline pick: A => Boolean)(using ClassTag[A]): Array[A] =
    var b = new Array[A](if a.length <= 8 then a.length else 8)
    var i = 0
    var j = 0
    while i < a.length do
      val x = a(i)
      if pick(x) then
        if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
        b(j) = x
        j += 1
      i += 1
    b.shrinkTo(j)

  transparent inline def selectOp[B]()(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](a.length)
    var i = 0
    while i < a.length do
      b(i) = op(a(i), i)
      i += 1
    b
  transparent inline def selectOp[B](i0: Int, iN: Int)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](iN - i0)
    var i = i0
    while i < iN do
      b(i - i0) = op(a(i), i)
      i += 1
    b
  transparent inline def selectOp[B](inline v: Iv | PIv)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val iv = inline v match
      case piv: PIv => piv.of(a)
      case siv: Iv  => siv
    selectOp(Iv.i0(iv), Iv.iN(iv))(op)
  transparent inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val iv = Iv of rg
    selectOp(iv.i0, iv.iN)(op)
  transparent inline def selectOp[B](indices: Array[Int])(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](indices.length)
    var i = 0
    while i < indices.length do
      val j = indices(i)
      b(i) = op(a(j), j)
      i += 1
    b
  transparent inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    var b = new Array[B](if a.length <= 8 then a.length else 8)
    var j = 0
    while indices.hasStep do
      if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
      val i = indices.nextStep
      b(j) = op(a(i), i)
      j += 1
    b.shrinkTo(j)
  transparent inline def selectOp[B](inline pick: A => Boolean)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val indices = where(pick)
    selectOp(indices)(op)

  transparent inline def fusion[B](inline add: (A, Int, B => Unit) => Unit)(using ClassTag[B]): Array[B] =
    var bs = new Array[B](if a.length < 8 then a.length else 8)
    var i = 0
    var j = 0
    while i < a.length do
      add(a(i), i, b => { if j >= bs.length then bs = bs.enlargeTo(bs.length | (bs.length << 1)); bs(j) = b; j += 1 })
      i += 1
    bs.shrinkTo(j)
  transparent inline def fission[B](inline pick: (A, Int) => Boolean)(inline op: (A, Int) => B)(inline cut: (A, Int) => Boolean, discardEmpty: Boolean = false)(using ClassTag[B]): Array[Array[B]] =
    var bss: Array[Array[B]] = null
    var bsi = 0
    var bs = new Array[B](if a.length < 8 then a.length else 8)
    var bi = 0
    var i = 0
    while i < a.length do
      val x = a(i)
      if pick(x, i) then
        val y = op(x, i)
        if bi >= bs.length then bs = bs.enlargeTo(bs.length | (bs.length << 1))
        bs(bi) = y
        bi += 1
      if cut(x, i) then
        if !discardEmpty || bi > 0 then
          if bss eq null then bss = new Array[Array[B]](8)
          else if bsi >= bss.length then bss = bss.enlargeTo(bss.length | (bss.length << 1))
          bss(bsi) = bs.shrinkTo(bi)
          bsi += 1
          if bi == bs.length then bs = new Array[B](if a.length - i - 1 < 8 then a.length - i - 1 else 8)
          bi = 0
      i += 1
    if discardEmpty && bi == 0 then
      if bss eq null then new Array[Array[B]](0)
      else bss.shrinkTo(bsi)
    else
      if bss eq null then Array(bs.shrinkTo(bi))
      else if bsi < bss.length then
        bss(bsi) = bs.shrinkTo(bi)
        bss.shrinkTo(bsi+1)
      else
        bss = bss.enlargeTo(bss.length + 1)
        bss(bss.length - 1) = bs.shrinkTo(bi)
        bss
}



opaque type ClippedArray[A] = Array[A]
object ClippedArray {
  inline def wrap[A](a: Array[A]): ClippedArray[A] = a

  extension [A](ca: ClippedArray[A])
    inline def unwrap: Array[A] = ca

  extension [A](ca: kse.basics.ClippedArray[A]) {
    inline def breakable: kse.basics.ShortClipArray[A] = ShortClipArray wrap ca.unwrap

    inline def visit(i0: Int, iN: Int)(inline f: (A, Int) => Unit): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      while i < iM do
        f(a(i), i)
        i += 1
    inline def visit(v: Iv | PIv)(inline f: (A, Int) => Unit): Unit =
      val iv = inline v match
        case piv: PIv => piv of ca.unwrap
        case siv: Iv  => siv
      visit(iv.i0, iv.iN)(f)
    inline def visit(inline rg: collection.immutable.Range)(inline f: (A, Int) => Unit): Unit =
      val iv = Iv of rg
      visit(iv.i0, iv.iN)(f)
    inline def visit(indices: Array[Int])(inline f: (A, Int) => Unit): Unit =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then f(a(j), j)
        i += 1
    inline def visit(indices: scala.collection.IntStepper)(inline f: (A, Int) => Unit): Unit =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then f(a(j), j)

    inline def gather[Z](i0: Int, iN: Int)(zero: Z)(inline f: (Z, A, Int) => Z): Z =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var z = zero
      while i < iM do
        z = f(z, a(i), i)
        i += 1
      z
    inline def gather[Z](inline v: Iv | PIv)(zero: Z)(inline f: (Z, A, Int) => Z): Z =
      val iv = inline v match
        case piv: PIv => piv of ca.unwrap
        case siv: Iv  => siv 
      gather(iv.i0, iv.iN)(zero)(f)
    inline def gather[Z](inline rg: collection.immutable.Range)(zero: Z)(inline f: (Z, A, Int) => Z): Z =
      val iv = Iv of rg
      gather(iv.i0, iv.iN)(zero)(f)
    inline def gather[Z](indices: Array[Int])(zero: Z)(inline f: (Z, A, Int) => Z): Z =
      val a = ca.unwrap
      var i = 0
      var z = zero
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then z = f(z, a(j), j)
        i += 1
      z
    inline def gather[Z](indices: scala.collection.IntStepper)(zero: Z)(inline f: (Z, A, Int) => Z): Z =
      val a = ca.unwrap
      var z = zero
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then z = f(z, a(j), j)
      z

    @targetName("update_Iv_constant")
    inline def update(iv: Iv, value: A): Unit =
      val a = ca.unwrap
      var i = iv.i0
      if i < 0 then i = 0
      var j = iv.iN
      if j > a.length then j = a.length
      while i < j do
        a(i) = value
        i += 1
    @targetName("update_Iv_array")
    inline def update(iv: Iv, values: Array[A]): Unit =
      val a = ca.unwrap
      var i = iv.i0
      if i < 0 then i = 0
      var j = iv.iN
      if j > a.length then j = a.length
      if j >= 0 && j - i > values.length then j = i + values.length
      java.lang.System.arraycopy(values, 0, a, i, j - i)

    @targetName("update_Range_constant")
    inline def update(inline rg: collection.immutable.Range, value: A): Unit =
      update(Iv of rg, value)
    @targetName("update_Range_array")
    inline def update(inline rg: collection.immutable.Range, values: Array[A]): Unit =
      update(Iv of rg, values)

    @targetName("update_Py_constant")
    inline def update(piv: PIv, value: A): Unit =
      update(piv of ca.unwrap, value)
    @targetName("update_Py_array")
    inline def update(piv: PIv, values: Array[A]): Unit =
      update(piv of ca.unwrap, values)

    @targetName("update_All_array")
    inline def update(values: Array[A]): Unit =
      update(Iv(0, ca.unwrap.length), values)

    @targetName("update_Places_constant")
    inline def update(indices: Array[Int], value: A): Unit =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then a(j) = value
        i += 1
    @targetName("update_Places_array")
    inline def update(indices: Array[Int], values: Array[A]): Unit =
      val a = ca.unwrap
      var i = 0
      var n = indices.length
      if n > values.length then n = values.length
      while i < n do
        val j = indices(i)
        if j >= 0 && j < a.length then a(j) = values(i)
        i += 1

    @targetName("update_Stepper_constant")
    inline def update(indices: scala.collection.IntStepper, value: A): Unit =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then a(j) = value
    @targetName("update_Stepper_array")
    inline def update(indices: scala.collection.IntStepper, values: Array[A]): Unit =
      val a = ca.unwrap
      var i = 0
      while indices.hasStep && i < values.length do
        val j = indices.nextStep
        if j >= 0 && j < a.length then a(j) = values(i)
        i += 1

    @targetName("set_i0iN_generate")
    inline def set(i0: Int, iN: Int)(inline generator: () => A): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j > a.length then j = a.length
      while i < j do
        a(i) = generator()
        i += 1
    @targetName("set_i0iN_index")
    inline def set(i0: Int, iN: Int)(inline indexer: Int => A): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j > a.length then j = a.length
      while i < j do
        a(i) = indexer(i)
        i += 1
    @targetName("set_i0iN_function")
    inline def set(i0: Int, iN: Int)(inline function: (A, Int) => A): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j > a.length then j = a.length
      while i < j do
        a(i) = function(a(i), i)
        i += 1

    @targetName("set_Iv_generate")
    inline def set(iv: Iv)(inline generator: () => A): Unit =
      set(iv.i0, iv.iN)(generator)
    @targetName("set_Iv_index")
    inline def set(iv: Iv)(inline indexer: Int => A): Unit =
      set(iv.i0, iv.iN)(indexer)
    @targetName("set_Iv_function")
    inline def set(iv: Iv)(inline function: (A, Int) => A): Unit =
      set(iv.i0, iv.iN)(function)

    @targetName("set_Range_generate")
    inline def set(inline rg: collection.immutable.Range, inline generator: () => A): Unit =
      val iv = Iv of rg
      set(iv.i0, iv.iN)(generator)
    @targetName("set_Range_index")
    inline def set(inline rg: collection.immutable.Range, inline indexer: Int => A): Unit =
      val iv = Iv of rg
      set(iv.i0, iv.iN)(indexer)
    @targetName("set_Range_function")
    inline def set(inline rg: collection.immutable.Range, inline function: (A, Int) => A): Unit =
      val iv = Iv of rg
      set(iv.i0, iv.iN)(function)

    @targetName("set_Py_generate")
    inline def set(piv: PIv)(inline generator: () => A): Unit =
      val iv = piv of ca.unwrap
      set(iv.i0, iv.iN)(generator)
    @targetName("set_Py_index")
    inline def set(piv: PIv)(inline indexer: Int => A): Unit =
      val iv = piv of ca.unwrap
      set(iv.i0, iv.iN)(indexer)
    @targetName("set_Py_function")
    inline def set(piv: PIv)(inline function: (A, Int) => A): Unit =
      val iv = piv of ca.unwrap
      set(iv.i0, iv.iN)(function)

    @targetName("set_Places_generate")
    inline def set(indices: Array[Int])(inline generator: () => A): Unit =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then a(j) = generator()
        i += 1
    @targetName("set_Places_index")
    inline def set(indices: Array[Int])(inline indexer: Int => A): Unit =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then a(j) = indexer(j)
        i += 1
    @targetName("set_Places_function")
    inline def set(indices: Array[Int])(inline function: (A, Int) => A): Unit =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then a(j) = function(a(j), j)
        i += 1

    @targetName("set_Stepper_generate")
    inline def set(indices: scala.collection.IntStepper)(inline generator: () => A): Unit =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then a(j) = generator()
    @targetName("set_Stepper_index")
    inline def set(indices: scala.collection.IntStepper)(inline indexer: Int => A): Unit =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then a(j) = indexer(j)
    @targetName("set_Stepper_function")
    inline def set(indices: scala.collection.IntStepper)(inline function: (A, Int) => A): Unit =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then a(j) = function(a(j), j)

    inline def inject(that: Array[A]): Int =
      inject(that, 0)(0, ca.unwrap.length)
    inline def inject(that: Array[A], where: Int): Int =
      inject(that, where)(0, ca.unwrap.length)
    inline def inject(that: Array[A])(i0: Int, iN: Int): Int =
      inject(that, 0)(i0, iN)
    inline def inject(that: Array[A], where: Int)(i0: Int, iN: Int): Int =
      val a = ca.unwrap
      var w = where
      if w < 0 then w = 0
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j >= a.length then j = a.length
      if i < j && w < that.length then
        var n = that.length - w
        if n > j - i then n = j - i
        java.lang.System.arraycopy(a, i, that, w, n)
        n
      else 0
    inline def inject(that: Array[A])(inline v: Iv | PIv): Int =
      val iv = inline v match
        case piv: PIv => piv of ca.unwrap
        case siv: Iv  => siv
      inject(that, 0)(iv.i0, iv.iN)
    inline def inject(that: Array[A], where: Int)(inline v: Iv | PIv): Int =
      val iv = inline v match
        case piv: PIv => piv of ca.unwrap
        case siv: Iv  => siv
      inject(that, where)(iv.i0, iv.iN)
    inline def inject(that: Array[A])(inline rg: collection.immutable.Range): Int =
      val iv = Iv of rg
      inject(that, 0)(iv.i0, iv.iN)
    inline def inject(that: Array[A], where: Int)(inline rg: collection.immutable.Range): Int =
      val iv = Iv of rg
      inject(that, where)(iv.i0, iv.iN)
    inline def inject(that: Array[A])(indices: Array[Int]): Int =
      inject(that, 0)(indices)
    inline def inject(that: Array[A], where: Int)(indices: Array[Int]): Int =
      val a = ca.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      while i < indices.length && j < that.length do
        val k = indices(i)
        if k >= 0 && k < a.length then
          that(j) = a(k)
          j += 1
        i += 1
      if where < 0 then j else j - where
    inline def inject(that: Array[A])(indices: scala.collection.IntStepper): Int =
      inject(that, 0)(indices)
    inline def inject(that: Array[A], where: Int)(indices: scala.collection.IntStepper): Int =
      val a = ca.unwrap
      var j = where
      if j < 0 then j = 0
      while indices.hasStep && j < that.length do
        val i = indices.nextStep
        if i >= 0 && i < a.length then
          that(j) = a(i)
          j += 1
      if where < 0 then j else j - where
    inline def inject(that: Array[A])(inline pick: A => Boolean): Int =
      inject(that, 0)(pick)
    inline def inject(that: Array[A], where: Int)(inline pick: A => Boolean): Int =
      val a = ca.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      while i < a.length && j < that.length do
        val x = a(i)
        if pick(x) then
          that(j) = x
          j += 1 
        i += 1
      if where < 0 then j else j - where

    inline def select(i0: Int, iN: Int)(using ClassTag[A]): Array[A] =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if iN >= a.length then j = iN
      val b = new Array[A](if i < j then j - i else 0)
      if b.length > 0 then java.lang.System.arraycopy(a, i, b, 0, b.length)
      b
    inline def select(inline v: Iv | PIv)(using ClassTag[A]): Array[A] =
      val iv = inline v match
        case piv: PIv => piv of ca.unwrap
        case siv: Iv  => siv
      select(iv.i0, iv.iN)
    inline def select(inline rg: collection.immutable.Range)(using ClassTag[A]): Array[A] =
      select(Iv of rg)
    inline def select(indices: Array[Int])(using ClassTag[A]): Array[A] =
      val a = ca.unwrap
      val b = new Array[A](indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        if k >= 0 && k < a.length then
          b(j) = a(k)
          j += 1
        i += 1
      b.shrinkTo(j)
    inline def select(indices: scala.collection.IntStepper)(using ClassTag[A]): Array[A] =
      val a = ca.unwrap
      var b = new Array[A](if a.length <= 8 then a.length else 8)
      var j = 0
      while indices.hasStep do
        val i = indices.nextStep
        if i >= 0 && i < a.length then
          if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
          b(j) = a(i)
          j += 1
      b.shrinkTo(j)

    transparent inline def selectOp[B](i0: Int, iN: Int)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j > a.length then j = a.length else if j < i then j = i
      val b = new Array[B](j - i)
      val offset = i
      while i < j do
        b(i - offset) = op(a(i), i)
        i += 1
      b
    transparent inline def selectOp[B](inline v: Iv | PIv)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = inline v match
        case piv: PIv => piv of ca.unwrap
        case siv: Iv  => siv
      selectOp(Iv.i0(iv), Iv.iN(iv))(op)
    transparent inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    transparent inline def selectOp[B](indices: Array[Int])(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = ca.unwrap
      val b = new Array[B](indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        if k >= 0 && i < a.length then
          b(j) = op(a(k), k)
          j += 1
        i += 1
      b.shrinkTo(j)
    transparent inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = ca.unwrap
      var b = new Array[B](if a.length <= 8 then a.length else 8)
      var j = 0
      while indices.hasStep do
        val i = indices.nextStep
        if i >= 0 && i < a.length then
          if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
          b(j) = op(a(i), i)
          j += 1
      b.shrinkTo(j)
  }
}


object shortcut {
  sealed trait Type {}
  object Skip extends Type {}
  object Quit extends Type {}

  inline def quittable(inline f: boundary.Label[Quit.type] ?=> Unit): Unit =
    boundary[Quit.type]:
      f
      Quit

  inline def skippable(inline f: boundary.Label[Skip.type] ?=> Unit): Unit =
    boundary[Skip.type]:
      f
      Skip

  inline def outer(inline f: boundary.Label[Type] ?=> Unit): Unit =
    boundary[Type]:
      f
      Quit

  inline def inner(inline f: boundary.Label[Type] ?=> Unit)(using boundary.Label[Type]): Unit =
    val what = boundary[Type]:
      f
      Skip
    if what eq Quit then boundary.break(Quit)

  inline def skip[S >: Skip.type <: Type](using boundary.Label[S]) = boundary.break(Skip: S)

  inline def skipIf[S >: Skip.type <: Type](p: Boolean)(using boundary.Label[S]): Unit = if p then boundary.break(Skip: S)

  inline def quit[Q >: Quit.type <: Type](using boundary.Label[Q]) = boundary.break(Quit: Q)

  inline def quitIf[Q >: Quit.type <: Type](p: Boolean)(using boundary.Label[Q]): Unit = if p then boundary.break(Quit: Q)
}


opaque type ShortcutArray[A] = Array[A]
object ShortcutArray {
  inline def wrap[A](a: Array[A]): ShortcutArray[A] = a

  extension [A](sa: ShortcutArray[A])
    inline def unwrap: Array[A] = sa

  extension [A](sa: kse.basics.ShortcutArray[A]) {
    inline def clip: kse.basics.ShortClipArray[A] = ShortClipArray wrap sa.unwrap

    inline def gather[Z]()(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z) =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = 0
        while i < a.length do
          z = f(z, a(i), i)
          i += 1
      z
    inline def gather[Z](i0: Int, iN: Int)(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z): Z =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = i0
        while i < iN do
          z = f(z, a(i), i)
          i += 1
      z
    inline def gather[Z](inline v: Iv | PIv)(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z): Z =
      val iv = inline v match
        case piv: PIv => piv of sa.unwrap
        case siv: Iv  => siv 
      gather(iv.i0, iv.iN)(zero)(f)
    inline def gather[Z](inline rg: collection.immutable.Range)(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z): Z =
      val iv = Iv of rg
      gather(iv.i0, iv.iN)(zero)(f)
    inline def gather[Z](indices: Array[Int])(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z): Z =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = 0
        while i < indices.length do
          val j = indices(i)
          z = f(z, a(j), j)
          i += 1
      z
    inline def gather[Z](indices: scala.collection.IntStepper)(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z): Z =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          z = f(z, a(j), j)
      z

    inline def dupWith[B](inline f: boundary.Label[shortcut.Quit.type] ?=> A => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      val b = new Array[B](a.length)
      var i = 0
      shortcut.quittable:
        while i < a.length do
          b(i) = f(a(i))
          i += 1
      shrinkTo(b)(i)

    inline def inject(that: Array[A])(inline pick: boundary.Label[shortcut.Quit.type] ?=> A => Boolean): Int =
      inject(that, 0)(pick)
    inline def inject(that: Array[A], where: Int)(inline pick: boundary.Label[shortcut.Quit.type] ?=> A => Boolean): Int =
      val a = sa.unwrap
      var i = 0
      var j = where
      shortcut.quittable:
        while i < a.length do
          val x = a(i)
          if pick(x) then
            that(j) = x
            j += 1 
          i += 1
      j - where

    inline def select(inline pick: boundary.Label[shortcut.Quit.type] ?=> A => Boolean)(using ClassTag[A]): Array[A] =
      val a = sa.unwrap
      var b = new Array[A](if a.length <= 8 then a.length else 8)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < a.length do
          val x = a(i)
          if pick(x) then
            if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
            b(j) = x
            j += 1
          i += 1
      b.shrinkTo(j)

    transparent inline def selectOp[B]()(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      val b = new Array[B](a.length)
      var i = 0
      var j = 0
      shortcut.outer:
        while i < a.length do
          shortcut.inner:
            b(j) = op(a(i), i)
            j += 1
          i += 1
      b.shrinkTo(j)
    transparent inline def selectOp[B](i0: Int, iN: Int)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      val b = new Array[B](iN - i0)
      var i = i0
      var j = 0
      shortcut.outer:
        while i < iN do
          shortcut.inner:
            b(j) = op(a(i), i)
            j += 1
          i += 1
      b.shrinkTo(j)
    transparent inline def selectOp[B](inline v: Iv | PIv)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = inline v match
        case piv: PIv => piv of sa.unwrap
        case siv: Iv  => siv
      selectOp(iv.i0, iv.iN)(op)
    transparent inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    transparent inline def selectOp[B](indices: Array[Int])(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      val b = new Array[B](indices.length)
      var i = 0
      var k = 0
      shortcut.outer:
        while i < indices.length do
          val j = indices(i)
          shortcut.inner:
            b(k) = op(a(j), j)
            k += 1
          i += 1
      b.shrinkTo(k)
    transparent inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      var b = new Array[B](if a.length <= 8 then a.length else 8)
      var j = 0
      shortcut.outer:
        while indices.hasStep do
          val i = indices.nextStep
          shortcut.inner:
            val y = op(a(i), i)
            if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
            b(j) = y
            j += 1
      b.shrinkTo(j)
    transparent inline def selectOp[B](inline pick: boundary.Label[shortcut.Quit.type] ?=> A => Boolean)(inline op: boundary.Label[shortcut.Quit.type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      var b = new Array[B](if a.length < 8 then a.length else 8)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < a.length do
          val x = a(i)
          if pick(x) then
            val y = op(x, i)
            if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
            b(j) = y
            j += 1
          i += 1
      b.shrinkTo(j)

    transparent inline def fusion[B](inline add: boundary.Label[shortcut.Quit.type] ?=> (A, Int, B => Unit) => Unit)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      var bs = new Array[B](if a.length < 8 then a.length else 8)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < a.length do
          add(a(i), i, b => { if j >= bs.length then bs = bs.enlargeTo(bs.length | (bs.length << 1)); bs(j) = b; j += 1 })
          i += 1
      bs.shrinkTo(j)
  }
}


opaque type ShortClipArray[A] = Array[A]
object ShortClipArray {
  inline def wrap[A](a: Array[A]): ShortClipArray[A] = a

  extension [A](sc: ShortClipArray[A])
    inline def unwrap: Array[A] = sc

  extension [A](sc: kse.basics.ShortClipArray[A]) {
    inline def gather[Z](i0: Int, iN: Int)(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z): Z =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var z = zero
      shortcut.quittable:
        while i < iM do
          z = f(z, a(i), i)
          i += 1
      z
    inline def gather[Z](inline v: Iv | PIv)(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z): Z =
      val iv = inline v match
        case piv: PIv => piv of sc.unwrap
        case siv: Iv  => siv 
      gather(iv.i0, iv.iN)(zero)(f)
    inline def gather[Z](inline rg: collection.immutable.Range)(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z): Z =
      val iv = Iv of rg
      gather(iv.i0, iv.iN)(zero)(f)
    inline def gather[Z](indices: Array[Int])(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z): Z =
      val a = sc.unwrap
      var i = 0
      var z = zero
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then z = f(z, a(j), j)
          i += 1
      z
    inline def gather[Z](indices: scala.collection.IntStepper)(zero: Z)(inline f: boundary.Label[shortcut.Quit.type] ?=> (Z, A, Int) => Z): Z =
      val a = sc.unwrap
      var z = zero
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then z = f(z, a(j), j)
      z

    inline def inject(that: Array[A])(inline pick: boundary.Label[shortcut.Quit.type] ?=> A => Boolean): Int =
      inject(that, 0)(pick)
    inline def inject(that: Array[A], where: Int)(inline pick: boundary.Label[shortcut.Quit.type] ?=> A => Boolean): Int =
      val a = sc.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      shortcut.quittable:
        while i < a.length && j < that.length do
          val x = a(i)
          if pick(x) then
            if j >= 0 then that(j) = x
            j += 1 
          i += 1
      j

    transparent inline def selectOp[B](i0: Int, iN: Int)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j > a.length then j = a.length else if j < i then j = i
      val b = new Array[B](j - i)
      var k = 0
      shortcut.outer:
        while i < j do
          shortcut.inner:
            b(k) = op(a(i), i)
            k += 1
          i += 1
      b.shrinkTo(k)
    transparent inline def selectOp[B](inline v: Iv | PIv)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = inline v match
        case piv: PIv => piv of sc.unwrap
        case siv: Iv  => siv
      selectOp(iv.i0, iv.iN)(op)
    transparent inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    transparent inline def selectOp[B](indices: Array[Int])(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sc.unwrap
      val b = new Array[B](indices.length)
      var i = 0
      var j = 0
      shortcut.outer:
        while i < indices.length do
          val k = indices(i)
          if k >= 0 && i < a.length then shortcut.inner:
            b(j) = op(a(k), k)
            j += 1
          i += 1
      b.shrinkTo(j)
    transparent inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sc.unwrap
      var b = new Array[B](if a.length <= 8 then a.length else 8)
      var j = 0
      shortcut.outer:
        while indices.hasStep do
          val i = indices.nextStep
          if i >= 0 && i < a.length then
            shortcut.inner:
              val y = op(a(i), i)
              if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
              b(j) = y
              j += 1
      b.shrinkTo(j)
  }
}



/** Boolean Array specific functionality from java.lang.System */
extension (az: Array[Boolean])
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(az, size)
  inline def shrinkCopy(size: Int) = if size < az.length then java.util.Arrays.copyOf(az, size) else az
  inline def copyOfRange(i0: Int, iN: Int): Array[Boolean] = java.util.Arrays.copyOfRange(az, i0, iN)
  inline def fill(b: Boolean): az.type = { java.util.Arrays.fill(az, b); az }
  inline def fillRange(i0: Int, iN: Int)(b: Boolean): az.type = { java.util.Arrays.fill(az, i0, iN, b); az }

/** Byte Array specific functionality from java.util.Arrays and java.lang.System */
extension (ab: Array[Byte])
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(ab, size)
  inline def shrinkCopy(size: Int) = if size < ab.length then java.util.Arrays.copyOf(ab, size) else ab
  inline def copyOfRange(i0: Int, iN: Int): Array[Byte] = java.util.Arrays.copyOfRange(ab, i0, iN)
  inline def packInts: Array[Int] = ArrayReform.toInts(ab)
  inline def packFloats: Array[Float] = ArrayReform.toFloats(ab)
  inline def packLongs: Array[Long] = ArrayReform.toLongs(ab)
  inline def packDoubles: Array[Double] = ArrayReform.toDoubles(ab)
  inline def search(b: Byte): Int = java.util.Arrays.binarySearch(ab, b)
  inline def searchRange(i0: Int, iN: Int)(b: Byte): Int = java.util.Arrays.binarySearch(ab, i0, iN, b)
  inline def fill(b: Byte): ab.type = { java.util.Arrays.fill(ab, b); ab }
  inline def fillRange(i0: Int, iN: Int)(b: Byte): ab.type = { java.util.Arrays.fill(ab, i0, iN, b); ab }
  inline def sort(): ab.type = { java.util.Arrays.sort(ab); ab }
  inline def sortRange(i0: Int, iN: Int): ab.type = { java.util.Arrays.sort(ab, i0, iN); ab }
  inline def isSorted: Boolean = isSortedRange(0, ab.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ab(i-1) <= ab(i) do i += 1
      i >= iN

/** Short Array specific functionality from java.util.Arrays and java.lang.System */
extension (as: Array[Short])
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(as, size)
  inline def shrinkCopy(size: Int) = if size < as.length then java.util.Arrays.copyOf(as, size) else as
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(as, i0, iN)
  inline def search(s: Short): Int = java.util.Arrays.binarySearch(as, s)
  inline def searchRange(i0: Int, iN: Int)(s: Short): Int = java.util.Arrays.binarySearch(as, i0, iN, s)  
  inline def fill(s: Short): as.type = { java.util.Arrays.fill(as, s); as }
  inline def fillRange(i0: Int, iN: Int)(s: Short): as.type = { java.util.Arrays.fill(as, i0, iN, s); as }
  inline def sort(): as.type = { java.util.Arrays.sort(as); as }
  inline def sortRange(i0: Int, iN: Int): as.type = { java.util.Arrays.sort(as, i0, iN); as }
  inline def isSorted: Boolean = isSortedRange(0, as.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && as(i-1) <= as(i) do i += 1
      i >= iN

/** Char Array specific functionality from java.util.Arrays and java.lang.System */
extension (ac: Array[Char])
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(ac, size)
  inline def shrinkCopy(size: Int) = if size < ac.length then java.util.Arrays.copyOf(ac, size) else ac
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(ac, i0, iN)
  inline def search(c: Char): Int = java.util.Arrays.binarySearch(ac, c)
  inline def searchRange(i0: Int, iN: Int)(c: Char): Int = java.util.Arrays.binarySearch(ac, i0, iN, c)
  inline def fill(c: Char): ac.type = { java.util.Arrays.fill(ac, c); ac }
  inline def fillRange(i0: Int, iN: Int)(c: Char): ac.type = { java.util.Arrays.fill(ac, i0, iN, c); ac }
  inline def sort(): ac.type = { java.util.Arrays.sort(ac); ac }
  inline def sortRange(i0: Int, iN: Int): ac.type = { java.util.Arrays.sort(ac, i0, iN); ac }
  inline def isSorted: Boolean = isSortedRange(0, ac.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ac(i-1) <= ac(i) do i += 1
      i >= iN

/** Int Array specific functionality from java.util.Arrays and java.lang.System */
extension (ai: Array[Int])
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(ai, size)
  inline def shrinkCopy(size: Int) = if size < ai.length then java.util.Arrays.copyOf(ai, size) else ai
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(ai, i0, iN)
  inline def unpackBytes: Array[Byte] = ArrayReform.toBytes(ai)
  inline def search(i: Int): Int = java.util.Arrays.binarySearch(ai, i)
  inline def searchRange(i0: Int, iN: Int)(i: Int): Int = java.util.Arrays.binarySearch(ai, i0, iN, i)
  inline def fill(i: Int): ai.type = { java.util.Arrays.fill(ai, i); ai }
  inline def fillRange(i0: Int, iN: Int)(i: Int): ai.type = { java.util.Arrays.fill(ai, i0, iN, i); ai }
  inline def sort(): ai.type = { java.util.Arrays.sort(ai); ai }
  inline def sortRange(i0: Int, iN: Int): ai.type = { java.util.Arrays.sort(ai, i0, iN); ai }
  inline def isSorted: Boolean = isSortedRange(0, ai.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ai(i-1) <= ai(i) do i += 1
      i >= iN

/** Long Array specific functionality from java.util.Arrays and java.lang.System */
extension (al: Array[Long])
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(al, size)
  inline def shrinkCopy(size: Int) = if size < al.length then java.util.Arrays.copyOf(al, size) else al
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(al, i0, iN)
  inline def unpackBytes: Array[Byte] = ArrayReform.toBytes(al)
  inline def search(l: Long): Int = java.util.Arrays.binarySearch(al, l)
  inline def searchRange(i0: Int, iN: Int)(l: Long): Int = java.util.Arrays.binarySearch(al, i0, iN, l)
  inline def fill(l: Long): al.type = { java.util.Arrays.fill(al, l); al }
  inline def fillRange(i0: Int, iN: Int)(l: Long): al.type = { java.util.Arrays.fill(al, i0, iN, l); al }
  inline def sort(): al.type = { java.util.Arrays.sort(al); al }
  inline def sortRange(i0: Int, iN: Int): al.type = { java.util.Arrays.sort(al, i0, iN); al }
  inline def isSorted: Boolean = isSortedRange(0, al.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && al(i-1) <= al(i) do i += 1
      i >= iN

/** Float Array specific functionality from java.util.Arrays and java.lang.System */
extension (af: Array[Float])
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(af, size)
  inline def shrinkCopy(size: Int) = if size < af.length then java.util.Arrays.copyOf(af, size) else af
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(af, i0, iN)
  inline def unpackBytes: Array[Byte] = ArrayReform.toBytes(af)
  inline def search(f: Float): Int = java.util.Arrays.binarySearch(af, f)
  inline def searchRange(i0: Int, iN: Int)(f: Float): Int = java.util.Arrays.binarySearch(af, i0, iN, f)
  inline def fill(f: Float): af.type = { java.util.Arrays.fill(af, f); af }
  inline def fillRange(i0: Int, iN: Int)(f: Float): af.type = { java.util.Arrays.fill(af, i0, iN, f); af }
  inline def sort(): af.type = { java.util.Arrays.sort(af); af }
  inline def sortRange(i0: Int, iN: Int): af.type = { java.util.Arrays.sort(af, i0, iN); af }
  inline def isSorted: Boolean = isSortedRange(0, af.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && af(i-1) <= af(i) do i += 1
      i >= iN

/** Double Array specific functionality from java.util.Arrays and java.lang.System */
extension (ad: Array[Double])
  inline def copyToSize(size: Int) = java.util.Arrays.copyOf(ad, size)
  inline def shrinkCopy(size: Int) = if size < ad.length then java.util.Arrays.copyOf(ad, size) else ad
  inline def copyOfRange(i0: Int, iN: Int) = java.util.Arrays.copyOfRange(ad, i0, iN)
  inline def unpackBytes: Array[Byte] = ArrayReform.toBytes(ad)
  inline def search(d: Double): Int = java.util.Arrays.binarySearch(ad, d)
  inline def searchRange(i0: Int, iN: Int)(d: Double): Int = java.util.Arrays.binarySearch(ad, i0, iN, d)
  inline def fill(d: Double): ad.type = { java.util.Arrays.fill(ad, d); ad }
  inline def fillRange(i0: Int, iN: Int)(d: Double): ad.type = { java.util.Arrays.fill(ad, i0, iN, d); ad }
  inline def sort(): ad.type = { java.util.Arrays.sort(ad); ad }
  inline def sortRange(i0: Int, iN: Int): ad.type = { java.util.Arrays.sort(ad, i0, iN); ad }
  inline def isSorted: Boolean = isSortedRange(0, ad.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ad(i-1) <= ad(i) do i += 1
      i >= iN

/** Object Array specific functionality from java.util.Arrays and java.lang.System */
extension [A >: Null <: AnyRef](aa: Array[A])
  inline def copyToSize(size: Int): Array[A] = java.util.Arrays.copyOf(aa, size)
  inline def shrinkCopy(size: Int): Array[A] = if size < aa.length then java.util.Arrays.copyOf(aa, size) else aa
  inline def copyOfRange(i0: Int, iN: Int): Array[A] = java.util.Arrays.copyOfRange(aa, i0, iN)
  inline def search(a: A)(using o: scala.math.Ordering[A]): Int = java.util.Arrays.binarySearch(aa, a, o)
  inline def searchRange(i0: Int, iN: Int)(a: A)(using o: scala.math.Ordering[A]): Int = java.util.Arrays.binarySearch(aa, i0, iN, a, o)
  inline def fill(a: A): aa.type = { java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], a: AnyRef); aa }
  inline def fillRange(i0: Int, iN: Int)(a: A): aa.type = { java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], i0, iN, a: AnyRef); aa }
  inline def sort()(using scala.math.Ordering[A]): aa.type = { scala.util.Sorting.stableSort(aa); aa }
  inline def sortRange(i0: Int, iN: Int)(using scala.math.Ordering[A]): aa.type = { scala.util.Sorting.stableSort(aa, i0, iN); aa }
  inline def isSorted(using scala.math.Ordering[A]): Boolean = isSortedRange(0, aa.length)
  def isSortedRange(i0: Int, iN: Int)(using o: scala.math.Ordering[A]): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && o.compare(aa(i-1), aa(i)) <= 0 do i += 1
      i >= iN




/** Holds mutable data (would be better if standard library exposed this!) */
sealed abstract class Mu[A] {
  inline def apply(): A = value
  def value: A
  def value_=(a: A): Unit
  def set(a: A): this.type = { value = a; this }
  inline final def zap(inline f: A => A): this.type = { value = f(value); this }
  inline final def use(inline f: A => Unit): this.type = { f(value); this }
  def copy: Mu[A]
  override def toString = s"~$value"
  override def hashCode = value.##
  override def equals(a: Any) = a match
    case m: Mu[_] => m.value.asInstanceOf[Any] == value
    case _ => false
}
object Mu {
  object      MuUnit                   extends Mu[Unit]    { def copy: MuUnit.type = this               ; def value: Unit = (); def value_=(u: Unit): Unit = () }
  final class MuBoolean(init: Boolean) extends Mu[Boolean] { def copy: MuBoolean = new MuBoolean(value) ; var value = init }
  final class MuByte   (init: Byte)    extends Mu[Byte]    { def copy: MuByte    = new MuByte(value)    ; var value = init }
  final class MuShort  (init: Short)   extends Mu[Short]   { def copy: MuShort   = new MuShort(value)   ; var value = init }
  final class MuChar   (init: Char)    extends Mu[Char]    { def copy: MuChar    = new MuChar(value)    ; var value = init }
  final class MuInt    (init: Int)     extends Mu[Int]     { def copy: MuInt     = new MuInt(value)     ; var value = init }
  final class MuLong   (init: Long)    extends Mu[Long]    { def copy: MuLong    = new MuLong(value)    ; var value = init }
  final class MuFloat  (init: Float)   extends Mu[Float]   { def copy: MuFloat   = new MuFloat(value)   ; var value = init }
  final class MuDouble (init: Double)  extends Mu[Double]  { def copy: MuDouble  = new MuDouble(value)  ; var value = init }
  final class MuAny[A] (init: A)       extends Mu[A]       { def copy: MuAny[A]  = new MuAny[A](value)  ; var value = init }
  def apply(u: Unit):    MuUnit.type = MuUnit
  def apply(z: Boolean): MuBoolean   = new MuBoolean(z)
  def apply(b: Byte):    MuByte      = new MuByte(b)
  def apply(s: Short):   MuShort     = new MuShort(s)
  def apply(c: Char):    MuChar      = new MuChar(c)
  def apply(i: Int):     MuInt       = new MuInt(i)
  def apply(l: Long):    MuLong      = new MuLong(l)
  def apply(f: Float):   MuFloat     = new MuFloat(f)
  def apply(d: Double):  MuDouble    = new MuDouble(d)
  def apply[A](a: A):    Mu[A]       = new MuAny(a)

  given [A]: Copies[Mu[A]] with
    def copy(m: Mu[A]): Mu[A] = m.copy
}

extension [A, M <: Mu[A]](mu: M)
  transparent inline def specific: Any = inline mu match
    case mz: Mu.MuBoolean => mz
    case mb: Mu.MuByte    => mb
    case ms: Mu.MuShort   => ms
    case mc: Mu.MuChar    => mc
    case mi: Mu.MuInt     => mi
    case ml: Mu.MuLong    => ml
    case mf: Mu.MuFloat   => mf
    case md: Mu.MuDouble  => md
    case mv: Mu[Unit]     => Mu.MuUnit
    case miq: Mu[Boolean]  => miq match { case mi: Mu.MuBoolean => mi; case _ => Mu(miq.value) }
    case miq: Mu[Byte   ]  => miq match { case mi: Mu.MuByte    => mi; case _ => Mu(miq.value) }
    case miq: Mu[Short  ]  => miq match { case mi: Mu.MuShort   => mi; case _ => Mu(miq.value) }
    case miq: Mu[Char   ]  => miq match { case mi: Mu.MuChar    => mi; case _ => Mu(miq.value) }
    case miq: Mu[Int    ]  => miq match { case mi: Mu.MuInt     => mi; case _ => Mu(miq.value) }
    case miq: Mu[Long   ]  => miq match { case mi: Mu.MuLong    => mi; case _ => Mu(miq.value) }
    case miq: Mu[Float  ]  => miq match { case mi: Mu.MuFloat   => mi; case _ => Mu(miq.value) }
    case miq: Mu[Double ]  => miq match { case mi: Mu.MuDouble  => mi; case _ => Mu(miq.value) }
    case _ => mu



/** Hides data from case classes, etc.
  *
  * Do NOT use in hash maps!  Every anonymous value looks like every other!
  */
final class Anon[A](val value: A) {
  def map[B](f: A => B) = new Anon(f(value))
  inline def use(f: A => Unit): this.type = { f(value); this }
  override def toString = "..."
  override def hashCode = 1239182
  override def equals(a: Any) = a match {
    case _: Anon[_] => true
    case _ => false
  }
}
object Anon {
  def apply[A](a: A) = new Anon(a)

  given [A](using Copies[A]): Copies[Anon[A]] with
    def copy(a: Anon[A]): Anon[A] = new Anon(summon[Copies[A]].copy(a.value))
}


/** Box that uses reference equality and identity hash code */
final class Identity[A](val value: A) {
  def map[B](f: A => B) = new Identity(f(value))
  inline def use(f: A => Unit): this.type = { f(value); this }
  override def toString = value.toString
  override def hashCode = java.lang.System.identityHashCode(value)
  override def equals(a: Any) = a match
    case i: Identity[_] => i.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef] 
    case _ => false
}
object Identity {
  given [A](using Copies[A]): Copies[Identity[A]] with
    def copy(i: Identity[A]): Identity[A] = new Identity(summon[Copies[A]].copy(i.value))
}



//////////////////////////////////////////////////////////////////////////////////
/// Generally helpful evaluation/execution utilities for singletons and tuples ///
//////////////////////////////////////////////////////////////////////////////////

extension[A <: AnyRef](a: A)
  inline def identityHash: Int = java.lang.System.identityHashCode(a)


extension [A](a: A) {
  /** Apply a function to this value and return the result.  Same as `pipe`. */
  inline def fn[B](inline f: A => B): B = f(a)

  /** Apply a function to this value and return the result.  Same as `fn`. */
  inline def pipe[B](inline f: A => B): B = f(a)

  /** Apply a side-effecting function to this value; return the original value */
  inline def tap(inline f: A => Unit): A = { f(a); a }

  /** Apply a test and alter the value if it passes */
  inline def fixIf(inline p: A => Boolean)(inline f: A => A): A = if p(a) then f(a) else a


  /** Make a tuple with this value and another.  Equivalent to `a -> z`. */
  inline def tup[Z](inline z: Z): (A, Z) = (a, z)

  /** Make a tuple with this value and another, by putting the other value in slot 1.  Equivalent to `z -> a`. */
  inline def tup_1[Z](inline z: Z): (Z, A) = (z, a)

  /** Make a tuple by applying a function to this value, and keeping both this value and the result. */
  inline def tupWith[Z](inline f: A => Z): (A, Z) = (a, f(a))
}


extension [A, B](q: (A, B)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B) = (z, q._2)

  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B) = (zfn(q._1), q._2)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z) = (q._1, z)

  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z) = (q._1, zfn(q._2))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y](inline az: A => Z, inline by: B => Y): (Z, Y) = (az(q._1), by(q._2))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B) => Z): (Z, Z) = (zfn(q._1), zfn(q._2))

  /** Pass the values of this tuple into a two-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B) => Z): Z = zfn(q._1, q._2)

  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B](zop: (Z, Z) => Z) = zop(q._1, q._2)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, Z) = (q._1, q._2, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B) = (z, q._1, q._2)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B) = (q._1, z, q._2)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B) => Z): (A, B, Z) = (q._1, q._2, zfn(q._1, q._2))


  /** Cut out the first value of this tuple, leaving only the second. */
  inline def snip_1: B = q._2

  /** Cut out the last value of this tuple, leaving only the first. */
  inline def snip: A = q._1

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, Y, Z) = (q._1, q._2, p._1, p._2)

  /** Concatenate this tuple with a 3-tuple */
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3)

  /** Concatenate this tuple with a 4-tuple */
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4)

  /** Concatenate this tuple with a 5-tuple. */
  inline def join[V, W, X, Y, Z](p: (V, W, X, Y, Z)): (A, B, V, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4, p._5)

  /** Concatenate this tuple with a 6-tuple. */
  inline def join[U, V, W, X, Y, Z](p: (U, V, W, X, Y, Z)): (A, B, U, V, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4, p._5, p._6)

  /** Concatenate this tuple with a 7-tuple. */
  inline def join[T, U, V, W, X, Y, Z](p: (T, U, V, W, X, Y, Z)): (A, B, T, U, V, W, X, Y, Z) = (q._1, q._2, p._1, p._2, p._3, p._4, p._5, p._6, p._7)
}


extension [A, B, C](q: (A, B, C)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C) = (z, q._2, q._3)

  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C) = (zfn(q._1), q._2, q._3)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C) = (q._1, z, q._3)

  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C) = (q._1, zfn(q._2), q._3)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z) = (q._1, q._2, z)

  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z) = (q._1, q._2, zfn(q._3))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X](inline az: A => Z, inline by: B => Y, inline cx: C => X): (Z, Y, X) = (az(q._1), by(q._2), cx(q._3))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C) => Z): (Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3))

  /** Pass the values of this tuple into a three-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C) => Z): Z = zfn(q._1, q._2, q._3)

  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C](zop: (Z, Z) => Z) = zop(zop(q._1, q._2), q._3)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, Z) = (q._1, q._2, q._3, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C) = (z, q._1, q._2, q._3)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C) = (q._1, z, q._2, q._3)

  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C) = (q._1, q._2, z, q._3)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C) => Z): (A, B, C, Z) = (q._1, q._2, q._3, zfn(q._1, q._2, q._3))


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C) = (q._2, q._3)

  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C) = (q._1, q._3)

  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B) = (q._1, q._2)

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, C, Y, Z) = (q._1, q._2, q._3, p._1, p._2)

  /** Concatenate this tuple with a 3-tuple */
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3)

  /** Concatenate this tuple with a 4-tuple */
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, C, W, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3, p._4)

  /** Concatenate this tuple with a 5-tuple */
  inline def join[V, W, X, Y, Z](p: (V, W, X, Y, Z)): (A, B, C, V, W, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3, p._4, p._5)

  /** Concatenate this tuple with a 6-tuple */
  inline def join[U, V, W, X, Y, Z](p: (U, V, W, X, Y, Z)): (A, B, C, U, V, W, X, Y, Z) = (q._1, q._2, q._3, p._1, p._2, p._3, p._4, p._5, p._6)

  /** Split this tuple after element 1, creating a singleton and a 2-tuple. */
  inline def cutAt1: (A, (B, C)) = (q._1, (q._2, q._3))

  /** Split this tuple after element 2, creating a 2-tuple and a singleton. */
  inline def cutAt2: ((A, B), C) = ((q._1, q._2), q._3)
}


extension [A, B, C, D](q: (A, B, C, D)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D) = (z, q._2, q._3, q._4)

  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D) = (zfn(q._1), q._2, q._3, q._4)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D) = (q._1, z, q._3, q._4)

  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D) = (q._1, zfn(q._2), q._3, q._4)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D) = (q._1, q._2, z, q._4)

  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D) = (q._1, q._2, zfn(q._3), q._4)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z) = (q._1, q._2, q._3, z)

  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z) = (q._1, q._2, q._3, zfn(q._4))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W): (Z, Y, X, W) = (az(q._1), by(q._2), cx(q._3), dw(q._4))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D) => Z): (Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4))

  /** Pass the values of this tuple into a four-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D) => Z): Z = zfn(q._1, q._2, q._3, q._4)

  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D](zop: (Z, Z) => Z) = zop(zop(zop(q._1, q._2), q._3), q._4)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D) = (z, q._1, q._2, q._3, q._4)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D) = (q._1, z, q._2, q._3, q._4)

  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D) = (q._1, q._2, z, q._3, q._4)

  /** Create a new tuple that is this one with the new component in position 4 and the rest arranged around it in order */
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D) = (q._1, q._2, q._3, z, q._4)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C, D) => Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, zfn(q._1, q._2, q._3, q._4))


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D) = (q._2, q._3, q._4)

  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D) = (q._1, q._3, q._4)

  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D) = (q._1, q._2, q._4)

  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C) = (q._1, q._2, q._3)

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2)

  /** Concatenate this tuple with a 3-tuple */
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, D, X, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2, p._3)

  /** Concatenate this tuple with a 4-tuple */
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, C, D, W, X, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2, p._3, p._4)

  /** Concatenate this tuple with a 5-tuple */
  inline def join[V, W, X, Y, Z](p: (V, W, X, Y, Z)): (A, B, C, D, V, W, X, Y, Z) = (q._1, q._2, q._3, q._4, p._1, p._2, p._3, p._4, p._5)

  /** Split this tuple after element 1, creating a singleton and a 3-tuple. */
  inline def cutAt1: (A, (B, C, D)) = (q._1, (q._2, q._3, q._4))

  /** Split this tuple after element 2, creating two 2-tuples. */
  inline def cutAt2: ((A, B), (C, D)) = ((q._1, q._2), (q._3, q._4))

  /** Split this tuple after element 3, creating a 3-tuple and a singleton. */
  inline def cutAt3: ((A, B, C), D) = ((q._1, q._2, q._3), q._4)
}

extension [A, B, C, D, E](q: (A, B, C, D, E)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D, E) = (z, q._2, q._3, q._4, q._5)

  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D, E) = (zfn(q._1), q._2, q._3, q._4, q._5)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D, E) = (q._1, z, q._3, q._4, q._5)

  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D, E) = (q._1, zfn(q._2), q._3, q._4, q._5)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D, E) = (q._1, q._2, z, q._4, q._5)

  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D, E) = (q._1, q._2, zfn(q._3), q._4, q._5)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z, E) = (q._1, q._2, q._3, z, q._5)

  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z, E) = (q._1, q._2, q._3, zfn(q._4), q._5)

  /** Create a new tuple with the fifth value replaced. */
  inline def _5to[Z](inline z: Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, z)

  /** Create a new tuple with the fifth value computed from the old one using a function. */
  inline def _5op[Z](inline zfn: E => Z): (A, B, C, D, Z) = (q._1, q._2, q._3, q._4, zfn(q._5))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W, V](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V): (Z, Y, X, W, V) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D | E) => Z): (Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5))
  
  /** Pass the values of this tuple into a five-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D, E) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5)

  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D | E](zop: (Z, Z) => Z) = zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E) = (z, q._1, q._2, q._3, q._4, q._5)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E) = (q._1, z, q._2, q._3, q._4, q._5)

  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E) = (q._1, q._2, z, q._3, q._4, q._5)

  /** Create a new tuple that is this one with the new component in position 4 and the rest arranged around it in order */
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E) = (q._1, q._2, q._3, z, q._4, q._5)

  /** Create a new tuple that is this one with the new component in position 5 and the rest arranged around it in order */
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E) = (q._1, q._2, q._3, q._4, z, q._5)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C, D, E) => Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, zfn(q._1, q._2, q._3, q._4, q._5))


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D, E) = (q._2, q._3, q._4, q._5)

  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D, E) = (q._1, q._3, q._4, q._5)

  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D, E) = (q._1, q._2, q._4, q._5)

  /** Cut out the fourth value of this tuple, creating a new tuple from what's left. */
  inline def snip_4: (A, B, C, E) = (q._1, q._2, q._3, q._5)

  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C, D) = (q._1, q._2, q._3, q._4)

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, E, Y, Z) = (q._1, q._2, q._3, q._4, q._5, p._1, p._2)

  /** Concatenate this tuple with a 3-tuple */
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, D, E, X, Y, Z) = (q._1, q._2, q._3, q._4, q._5, p._1, p._2, p._3)

  /** Concatenate this tuple with a 4-tuple */
  inline def join[W, X, Y, Z](p: (W, X, Y, Z)): (A, B, C, D, E, W, X, Y, Z) = (q._1, q._2, q._3, q._4, q._5, p._1, p._2, p._3, p._4)

  /** Split this tuple after element 1, creating a singleton and a 4-tuple. */
  inline def cutAt1: (A, (B, C, D, E)) = (q._1, (q._2, q._3, q._4, q._5))

  /** Split this tuple after element 2, creating a 2-tuple and a 3-tuple. */
  inline def cutAt2: ((A, B), (C, D, E)) = ((q._1, q._2), (q._3, q._4, q._5))

  /** Split this tuple after element 3, creating a 3-tuple and a 2-tuple. */
  inline def cutAt3: ((A, B, C), (D, E)) = ((q._1, q._2, q._3), (q._4, q._5))

  /** Split this tuple after element 4, creating a 4-tuple and a singleton. */
  inline def cutAt4: ((A, B, C, D), E) = ((q._1, q._2, q._3, q._4), q._5)
}


extension [A, B, C, D, E, F](q: (A, B, C, D, E, F)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D, E, F) = (z, q._2, q._3, q._4, q._5, q._6)

  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D, E, F) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D, E, F) = (q._1, z, q._3, q._4, q._5, q._6)

  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D, E, F) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D, E, F) = (q._1, q._2, z, q._4, q._5, q._6)

  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D, E, F) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z, E, F) = (q._1, q._2, q._3, z, q._5, q._6)

  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z, E, F) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6)

  /** Create a new tuple with the fifth value replaced. */
  inline def _5to[Z](inline z: Z): (A, B, C, D, Z, F) = (q._1, q._2, q._3, q._4, z, q._6)

  /** Create a new tuple with the fifth value computed from the old one using a function. */
  inline def _5op[Z](inline zfn: E => Z): (A, B, C, D, Z, F) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6)

  /** Create a new tuple with the sixth value replaced. */
  inline def _6to[Z](inline z: Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, z)

  /** Create a new tuple with the sixth value computed from the old one using a function. */
  inline def _6op[Z](inline zfn: F => Z): (A, B, C, D, E, Z) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W, V, U](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U): (Z, Y, X, W, V, U) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D | E | F) => Z): (Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6))
  
  /** Pass the values of this tuple into a six-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D, E, F) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6)

  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D | E | F](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E, F) = (z, q._1, q._2, q._3, q._4, q._5, q._6)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E, F) = (q._1, z, q._2, q._3, q._4, q._5, q._6)

  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E, F) = (q._1, q._2, z, q._3, q._4, q._5, q._6)

  /** Create a new tuple that is this one with the new component in position 4 and the rest arranged around it in order */
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E, F) = (q._1, q._2, q._3, z, q._4, q._5, q._6)

  /** Create a new tuple that is this one with the new component in position 5 and the rest arranged around it in order */
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E, F) = (q._1, q._2, q._3, q._4, z, q._5, q._6)

  /** Create a new tuple that is this one with the new component in position 6 and the rest arranged around it in order */
  inline def tup_6[Z](inline z: Z): (A, B, C, D, E, Z, F) = (q._1, q._2, q._3, q._4, q._5, z, q._6)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C, D, E, F) => Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._1, q._2, q._3, q._4, q._5, q._6))


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D, E, F) = (q._2, q._3, q._4, q._5, q._6)

  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D, E, F) = (q._1, q._3, q._4, q._5, q._6)

  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D, E, F) = (q._1, q._2, q._4, q._5, q._6)

  /** Cut out the fourth value of this tuple, creating a new tuple from what's left. */
  inline def snip_4: (A, B, C, E, F) = (q._1, q._2, q._3, q._5, q._6)

  /** Cut out the fifth value of this tuple, creating a new tuple from what's left. */
  inline def snip_5: (A, B, C, D, F) = (q._1, q._2, q._3, q._4, q._6)

  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C, D, E) = (q._1, q._2, q._3, q._4, q._5)

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, E, F, Y, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, p._1, p._2)
  
  /** Concatenate this tuple with a 3-tuple */
  inline def join[X, Y, Z](p: (X, Y, Z)): (A, B, C, D, E, F, X, Y, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, p._1, p._2, p._3)

  /** Split this tuple after element 1, creating a singleton and a 5-tuple. */
  inline def cutAt1: (A, (B, C, D, E, F)) = (q._1, (q._2, q._3, q._4, q._5, q._6))

  /** Split this tuple after element 2, creating a 2-tuple and a 4-tuple. */
  inline def cutAt2: ((A, B), (C, D, E, F)) = ((q._1, q._2), (q._3, q._4, q._5, q._6))

  /** Split this tuple after element 3, creating two 3-tuples. */
  inline def cutAt3: ((A, B, C), (D, E, F)) = ((q._1, q._2, q._3), (q._4, q._5, q._6))

  /** Split this tuple after element 4, creating a 4-tuple and a 2-tuple. */
  inline def cutAt4: ((A, B, C, D), (E, F)) = ((q._1, q._2, q._3, q._4), (q._5, q._6))

  /** Split this tuple after element 5, creating a 5-tuple and a singleton. */
  inline def cutAt5: ((A, B, C, D, E), F) = ((q._1, q._2, q._3, q._4, q._5), q._6)
}


extension [A, B, C, D, E, F, G](q: (A, B, C, D, E, F, G)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D, E, F, G) = (z, q._2, q._3, q._4, q._5, q._6, q._7)
  
  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D, E, F, G) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6, q._7)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D, E, F, G) = (q._1, z, q._3, q._4, q._5, q._6, q._7)
  
  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D, E, F, G) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6, q._7)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D, E, F, G) = (q._1, q._2, z, q._4, q._5, q._6, q._7)
  
  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D, E, F, G) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6, q._7)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z, E, F, G) = (q._1, q._2, q._3, z, q._5, q._6, q._7)
  
  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z, E, F, G) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6, q._7)

  /** Create a new tuple with the fifth value replaced. */
  inline def _5to[Z](inline z: Z): (A, B, C, D, Z, F, G) = (q._1, q._2, q._3, q._4, z, q._6, q._7)
  
  /** Create a new tuple with the fifth value computed from the old one using a function. */
  inline def _5op[Z](inline zfn: E => Z): (A, B, C, D, Z, F, G) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6, q._7)

  /** Create a new tuple with the sixth value replaced. */
  inline def _6to[Z](inline z: Z): (A, B, C, D, E, Z, G) = (q._1, q._2, q._3, q._4, q._5, z, q._7)
  
  /** Create a new tuple with the sixth value computed from the old one using a function. */
  inline def _6op[Z](inline zfn: F => Z): (A, B, C, D, E, Z, G) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6), q._7)

  /** Create a new tuple with the seventh value replaced. */
  inline def _7to[Z](inline z: Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, z)
  
  /** Create a new tuple with the seventh value computed from the old one using a function. */
  inline def _7op[Z](inline zfn: G => Z): (A, B, C, D, E, F, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._7))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W, V, U, T](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U, inline gt: G => T): (Z, Y, X, W, V, U, T) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6), gt(q._7))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D | E | F | G) => Z): (Z, Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6), zfn(q._7))
  
  /** Pass the values of this tuple into a seven-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D, E, F, G) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7)
  
  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D | E | F | G](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6), q._7)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z)

  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E, F, G) = (z, q._1, q._2, q._3, q._4, q._5, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E, F, G) = (q._1, z, q._2, q._3, q._4, q._5, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E, F, G) = (q._1, q._2, z, q._3, q._4, q._5, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 4 and the rest arranged around it in order */
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E, F, G) = (q._1, q._2, q._3, z, q._4, q._5, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 5 and the rest arranged around it in order */
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E, F, G) = (q._1, q._2, q._3, q._4, z, q._5, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 6 and the rest arranged around it in order */
  inline def tup_6[Z](inline z: Z): (A, B, C, D, E, Z, F, G) = (q._1, q._2, q._3, q._4, q._5, z, q._6, q._7)

  /** Create a new tuple that is this one with the new component in position 7 and the rest arranged around it in order */
  inline def tup_7[Z](inline z: Z): (A, B, C, D, E, F, Z, G) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._7)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C, D, E, F, G) => Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7))


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D, E, F, G) = (q._2, q._3, q._4, q._5, q._6, q._7)
  
  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D, E, F, G) = (q._1, q._3, q._4, q._5, q._6, q._7)
  
  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D, E, F, G) = (q._1, q._2, q._4, q._5, q._6, q._7)
  
  /** Cut out the fourth value of this tuple, creating a new tuple from what's left. */
  inline def snip_4: (A, B, C, E, F, G) = (q._1, q._2, q._3, q._5, q._6, q._7)
  
  /** Cut out the fifth value of this tuple, creating a new tuple from what's left. */
  inline def snip_5: (A, B, C, D, F, G) = (q._1, q._2, q._3, q._4, q._6, q._7)
  
  /** Cut out the sixth value of this tuple, creating a new tuple from what's left. */
  inline def snip_6: (A, B, C, D, E, G) = (q._1, q._2, q._3, q._4, q._5, q._7)
  
  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C, D, E, F) = (q._1, q._2, q._3, q._4, q._5, q._6)

  /** Concatenate this tuple with a 2-tuple */
  inline def join[Y, Z](p: (Y, Z)): (A, B, C, D, E, F, G, Y, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, p._1, p._2)

  /** Split this tuple after element 1, creating a singleton and a 6-tuple. */
  inline def cutAt1: (A, (B, C, D, E, F, G)) = (q._1, (q._2, q._3, q._4, q._5, q._6, q._7))

  /** Split this tuple after element 2, creating a 2-tuple and a 5-tuple. */
  inline def cutAt2: ((A, B), (C, D, E, F, G)) = ((q._1, q._2), (q._3, q._4, q._5, q._6, q._7))

  /** Split this tuple after element 3, creating a 3-tuple and a 4-tuple. */
  inline def cutAt3: ((A, B, C), (D, E, F, G)) = ((q._1, q._2, q._3), (q._4, q._5, q._6, q._7))

  /** Split this tuple after element 4, creating a 4-tuple and a 3-tuple. */
  inline def cutAt4: ((A, B, C, D), (E, F, G)) = ((q._1, q._2, q._3, q._4), (q._5, q._6, q._7))

  /** Split this tuple after element 5, creating a 5-tuple and a 2-tuple. */
  inline def cutAt5: ((A, B, C, D, E), (F, G)) = ((q._1, q._2, q._3, q._4, q._5), (q._6, q._7))

  /** Split this tuple after element 6, creating a 6-tuple and a singleton. */
  inline def cutAt6: ((A, B, C, D, E, F), G) = ((q._1, q._2, q._3, q._4, q._5, q._6), q._7)
}


extension [A, B, C, D, E, F, G, H](q: (A, B, C, D, E, F, G, H)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D, E, F, G, H) = (z, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D, E, F, G, H) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6, q._7, q._8)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D, E, F, G, H) = (q._1, z, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D, E, F, G, H) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6, q._7, q._8)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D, E, F, G, H) = (q._1, q._2, z, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D, E, F, G, H) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6, q._7, q._8)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z, E, F, G, H) = (q._1, q._2, q._3, z, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z, E, F, G, H) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6, q._7, q._8)

  /** Create a new tuple with the fifth value replaced. */
  inline def _5to[Z](inline z: Z): (A, B, C, D, Z, F, G, H) = (q._1, q._2, q._3, q._4, z, q._6, q._7, q._8)
  
  /** Create a new tuple with the fifth value computed from the old one using a function. */
  inline def _5op[Z](inline zfn: E => Z): (A, B, C, D, Z, F, G, H) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6, q._7, q._8)

  /** Create a new tuple with the sixth value replaced. */
  inline def _6to[Z](inline z: Z): (A, B, C, D, E, Z, G, H) = (q._1, q._2, q._3, q._4, q._5, z, q._7, q._8)
  
  /** Create a new tuple with the sixth value computed from the old one using a function. */
  inline def _6op[Z](inline zfn: F => Z): (A, B, C, D, E, Z, G, H) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6), q._7, q._8)

  /** Create a new tuple with the seventh value replaced. */
  inline def _7to[Z](inline z: Z): (A, B, C, D, E, F, Z, H) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._8)
  
  /** Create a new tuple with the seventh value computed from the old one using a function. */
  inline def _7op[Z](inline zfn: G => Z): (A, B, C, D, E, F, Z, H) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._7), q._8)

  /** Create a new tuple with the eighth value replaced. */
  inline def _8to[Z](inline z: Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z)
  
  /** Create a new tuple with the eighth value computed from the old one using a function. */
  inline def _8op[Z](inline zfn: H => Z): (A, B, C, D, E, F, G, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, zfn(q._8))


  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W, V, U, T, S](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U, inline gt: G => T, inline hs: H => S): (Z, Y, X, W, V, U, T, S) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6), gt(q._7), hs(q._8))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D | E | F | G | H) => Z): (Z, Z, Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6), zfn(q._7), zfn(q._8))
  
  /** Pass the values of this tuple into an eight-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D, E, F, G, H) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D | E | F | G | H](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6), q._7), q._8)


  /** Create a new tuple that is this one with a new component on the end */
  inline def tup[Z](inline z: Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, z)
  
  /** Create a new tuple that is this one with a new component in position 1 and the rest afterwards */
  inline def tup_1[Z](inline z: Z): (Z, A, B, C, D, E, F, G, H) = (z, q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 2 and the rest arranged around it in order */
  inline def tup_2[Z](inline z: Z): (A, Z, B, C, D, E, F, G, H) = (q._1, z, q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 3 and the rest arranged around it in order */
  inline def tup_3[Z](inline z: Z): (A, B, Z, C, D, E, F, G, H) = (q._1, q._2, z, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 4 and the rest arranged around it in order */
  inline def tup_4[Z](inline z: Z): (A, B, C, Z, D, E, F, G, H) = (q._1, q._2, q._3, z, q._4, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 5 and the rest arranged around it in order */
  inline def tup_5[Z](inline z: Z): (A, B, C, D, Z, E, F, G, H) = (q._1, q._2, q._3, q._4, z, q._5, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 6 and the rest arranged around it in order */
  inline def tup_6[Z](inline z: Z): (A, B, C, D, E, Z, F, G, H) = (q._1, q._2, q._3, q._4, q._5, z, q._6, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 7 and the rest arranged around it in order */
  inline def tup_7[Z](inline z: Z): (A, B, C, D, E, F, Z, G, H) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._7, q._8)
  
  /** Create a new tuple that is this one with the new component in position 8 and the rest arranged around it in order */
  inline def tup_8[Z](inline z: Z): (A, B, C, D, E, F, G, Z, H) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z, q._8)

  /** Create a new tuple that is this one plus a value computed with a function of the elements of this tuple. */
  inline def tupWith[Z](inline zfn: (A, B, C, D, E, F, G, H) => Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8))

  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D, E, F, G, H) = (q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D, E, F, G, H) = (q._1, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D, E, F, G, H) = (q._1, q._2, q._4, q._5, q._6, q._7, q._8)
  
  /** Cut out the fourth value of this tuple, creating a new tuple from what's left. */
  inline def snip_4: (A, B, C, E, F, G, H) = (q._1, q._2, q._3, q._5, q._6, q._7, q._8)
  
  /** Cut out the fifth value of this tuple, creating a new tuple from what's left. */
  inline def snip_5: (A, B, C, D, F, G, H) = (q._1, q._2, q._3, q._4, q._6, q._7, q._8)
  
  /** Cut out the sixth value of this tuple, creating a new tuple from what's left. */
  inline def snip_6: (A, B, C, D, E, G, H) = (q._1, q._2, q._3, q._4, q._5, q._7, q._8)
  
  /** Cut out the seventh value of this tuple, creating a new tuple from what's left. */
  inline def snip_7: (A, B, C, D, E, F, H) = (q._1, q._2, q._3, q._4, q._5, q._6, q._8)
  
  /** Cut out the last value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C, D, E, F, G) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7)

  /** Split this tuple after element 1, creating a singleton and a 7-tuple. */
  inline def cutAt1: (A, (B, C, D, E, F, G, H)) = (q._1, (q._2, q._3, q._4, q._5, q._6, q._7, q._8))
  
  /** Split this tuple after element 2, creating a 2-tuple and a 6-tuple. */
  inline def cutAt2: ((A, B), (C, D, E, F, G, H)) = ((q._1, q._2), (q._3, q._4, q._5, q._6, q._7, q._8))
  
  /** Split this tuple after element 3, creating a 3-tuple and a 5-tuple. */
  inline def cutAt3: ((A, B, C), (D, E, F, G, H)) = ((q._1, q._2, q._3), (q._4, q._5, q._6, q._7, q._8))
  
  /** Split this tuple after element 4, creating two 4-tuples. */
  inline def cutAt4: ((A, B, C, D), (E, F, G, H)) = ((q._1, q._2, q._3, q._4), (q._5, q._6, q._7, q._8))
  
  /** Split this tuple after element 5, creating a 5-tuple and a 3-tuple. */
  inline def cutAt5: ((A, B, C, D, E), (F, G, H)) = ((q._1, q._2, q._3, q._4, q._5), (q._6, q._7, q._8))
  
  /** Split this tuple after element 6, creating a 6-tuple and a 2-tuple. */
  inline def cutAt6: ((A, B, C, D, E, F), (G, H)) = ((q._1, q._2, q._3, q._4, q._5, q._6), (q._7, q._8))

  /** Split this tuple after element 7, creating a 7-tuple and a singleton. */
  inline def cutAt7: ((A, B, C, D, E, F, G), H) = ((q._1, q._2, q._3, q._4, q._5, q._6, q._7), q._8)
}


extension [A, B, C, D, E, F, G, H, I](q: (A, B, C, D, E, F, G, H, I)) {
  /** Create a new tuple with the first value replaced. */
  inline def _1to[Z](inline z: Z): (Z, B, C, D, E, F, G, H, I) = (z, q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Create a new tuple with the first value computed from the old one using a function. */
  inline def _1op[Z](inline zfn: A => Z): (Z, B, C, D, E, F, G, H, I) = (zfn(q._1), q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)

  /** Create a new tuple with the second value replaced. */
  inline def _2to[Z](inline z: Z): (A, Z, C, D, E, F, G, H, I) = (q._1, z, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Create a new tuple with the second value computed from the old one using a function. */
  inline def _2op[Z](inline zfn: B => Z): (A, Z, C, D, E, F, G, H, I) = (q._1, zfn(q._2), q._3, q._4, q._5, q._6, q._7, q._8, q._9)

  /** Create a new tuple with the third value replaced. */
  inline def _3to[Z](inline z: Z): (A, B, Z, D, E, F, G, H, I) = (q._1, q._2, z, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Create a new tuple with the third value computed from the old one using a function. */
  inline def _3op[Z](inline zfn: C => Z): (A, B, Z, D, E, F, G, H, I) = (q._1, q._2, zfn(q._3), q._4, q._5, q._6, q._7, q._8, q._9)

  /** Create a new tuple with the fourth value replaced. */
  inline def _4to[Z](inline z: Z): (A, B, C, Z, E, F, G, H, I) = (q._1, q._2, q._3, z, q._5, q._6, q._7, q._8, q._9)
  
  /** Create a new tuple with the fourth value computed from the old one using a function. */
  inline def _4op[Z](inline zfn: D => Z): (A, B, C, Z, E, F, G, H, I) = (q._1, q._2, q._3, zfn(q._4), q._5, q._6, q._7, q._8, q._9)

  /** Create a new tuple with the fifth value replaced. */
  inline def _5to[Z](inline z: Z): (A, B, C, D, Z, F, G, H, I) = (q._1, q._2, q._3, q._4, z, q._6, q._7, q._8, q._9)
  
  /** Create a new tuple with the fifth value computed from the old one using a function. */
  inline def _5op[Z](inline zfn: E => Z): (A, B, C, D, Z, F, G, H, I) = (q._1, q._2, q._3, q._4, zfn(q._5), q._6, q._7, q._8, q._9)

  /** Create a new tuple with the sixth value replaced. */
  inline def _6to[Z](inline z: Z): (A, B, C, D, E, Z, G, H, I) = (q._1, q._2, q._3, q._4, q._5, z, q._7, q._8, q._9)
  
  /** Create a new tuple with the sixth value computed from the old one using a function. */
  inline def _6op[Z](inline zfn: F => Z): (A, B, C, D, E, Z, G, H, I) = (q._1, q._2, q._3, q._4, q._5, zfn(q._6), q._7, q._8, q._9)

  /** Create a new tuple with the seventh value replaced. */
  inline def _7to[Z](inline z: Z): (A, B, C, D, E, F, Z, H, I) = (q._1, q._2, q._3, q._4, q._5, q._6, z, q._8, q._9)
  
  /** Create a new tuple with the seventh value computed from the old one using a function. */
  inline def _7op[Z](inline zfn: G => Z): (A, B, C, D, E, F, Z, H, I) = (q._1, q._2, q._3, q._4, q._5, q._6, zfn(q._7), q._8, q._9)

  /** Create a new tuple with the eigth value replaced. */
  inline def _8to[Z](inline z: Z): (A, B, C, D, E, F, G, Z, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, z, q._9)
  
  /** Create a new tuple with the eigth value computed from the old one using a function. */
  inline def _8op[Z](inline zfn: H => Z): (A, B, C, D, E, F, G, Z, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, zfn(q._8), q._9)

  /** Create a new tuple with the ninth value replaced. */
  inline def _9to[Z](inline z: Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, z)
  
  /** Create a new tuple with the ninth value computed from the old one using a function. */
  inline def _9op[Z](inline zfn: I => Z): (A, B, C, D, E, F, G, H, Z) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, zfn(q._9))

  
  /** Create a new tuple by applying a different function to each position of this tuple. */
  inline def ops[Z, Y, X, W, V, U, T, S, R](inline az: A => Z, inline by: B => Y, inline cx: C => X, inline dw: D => W, inline ev: E => V, inline fu: F => U, inline gt: G => T, inline hs: H => S, inline ir: I => R): (Z, Y, X, W, V, U, T, S, R) = (az(q._1), by(q._2), cx(q._3), dw(q._4), ev(q._5), fu(q._6), gt(q._7), hs(q._8), ir(q._9))
  
  /** Create a new tuple by applying the same function to each position of this tuple. */
  inline def sameOp[Z](zfn: (A | B | C | D | E | F | G | H | I) => Z): (Z, Z, Z, Z, Z, Z, Z, Z, Z) = (zfn(q._1), zfn(q._2), zfn(q._3), zfn(q._4), zfn(q._5), zfn(q._6), zfn(q._7), zfn(q._8), zfn(q._9))
  
  /** Pass the values of this tuple into a nine-argument function and return the result */
  inline def merge[Z](inline zfn: (A, B, C, D, E, F, G, H, I) => Z): Z = zfn(q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Using a binary operation, reduce this tuple to a single value */
  inline def reduce[Z >: A | B | C | D | E | F | G | H | I](zop: (Z, Z) => Z) = zop(zop(zop(zop(zop(zop(zop(zop(q._1, q._2), q._3), q._4), q._5), q._6), q._7), q._8), q._9)


  /** Cut out the first value of this tuple, creating a new tuple from what's left. */
  inline def snip_1: (B, C, D, E, F, G, H, I) = (q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the second value of this tuple, creating a new tuple from what's left. */
  inline def snip_2: (A, C, D, E, F, G, H, I) = (q._1, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the third value of this tuple, creating a new tuple from what's left. */
  inline def snip_3: (A, B, D, E, F, G, H, I) = (q._1, q._2, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the fourth value of this tuple, creating a new tuple from what's left. */
  inline def snip_4: (A, B, C, E, F, G, H, I) = (q._1, q._2, q._3, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the fifth value of this tuple, creating a new tuple from what's left. */
  inline def snip_5: (A, B, C, D, F, G, H, I) = (q._1, q._2, q._3, q._4, q._6, q._7, q._8, q._9)
  
  /** Cut out the sixth value of this tuple, creating a new tuple from what's left. */
  inline def snip_6: (A, B, C, D, E, G, H, I) = (q._1, q._2, q._3, q._4, q._5, q._7, q._8, q._9)
  
  /** Cut out the seventh value of this tuple, creating a new tuple from what's left. */
  inline def snip_7: (A, B, C, D, E, F, H, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._8, q._9)
  
  /** Cut out the eigth value of this tuple, creating a new tuple from what's left. */
  inline def snip_8: (A, B, C, D, E, F, G, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._9)
  
  /** Cut out the ninth value of this tuple, creating a new tuple from what's left. */
  inline def snip:   (A, B, C, D, E, F, G, H) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8)

  /** Split this tuple after element 1, creating a singleton and a 7-tuple. */
  inline def cutAt1: (A, (B, C, D, E, F, G, H, I)) = (q._1, (q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9))
  
  /** Split this tuple after element 2, creating a 2-tuple and a 7-tuple. */
  inline def cutAt2: ((A, B), (C, D, E, F, G, H, I)) = ((q._1, q._2), (q._3, q._4, q._5, q._6, q._7, q._8, q._9))
  
  /** Split this tuple after element 3, creating a 3-tuple and a 6-tuple. */
  inline def cutAt3: ((A, B, C), (D, E, F, G, H, I)) = ((q._1, q._2, q._3), (q._4, q._5, q._6, q._7, q._8, q._9))
  
  /** Split this tuple after element 4, creating a 4-tuple and a 5-tuple. */
  inline def cutAt4: ((A, B, C, D), (E, F, G, H, I)) = ((q._1, q._2, q._3, q._4), (q._5, q._6, q._7, q._8, q._9))
  
  /** Split this tuple after element 5, creating a 5-tuple and a 4-tuple. */
  inline def cutAt5: ((A, B, C, D, E), (F, G, H, I)) = ((q._1, q._2, q._3, q._4, q._5), (q._6, q._7, q._8, q._9))
  
  /** Split this tuple after element 6, creating a 6-tuple and a 3-tuple. */
  inline def cutAt6: ((A, B, C, D, E, F), (G, H, I)) = ((q._1, q._2, q._3, q._4, q._5, q._6), (q._7, q._8, q._9))
  
  /** Split this tuple after element 7, creating a 7-tuple and a 2-tuple. */
  inline def cutAt7: ((A, B, C, D, E, F, G), (H, I)) = ((q._1, q._2, q._3, q._4, q._5, q._6, q._7), (q._8, q._9))
  
  /** Split this tuple after element 8, creating an 8-tuple and a singleton. */
  inline def cutAt8: ((A, B, C, D, E, F, G, H), I) = ((q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._8), q._9)
}
