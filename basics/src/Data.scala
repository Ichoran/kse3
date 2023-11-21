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
      if i < 0 then a.length + i else i
    inline def of(a: String): Int =
      val i: Int = idx
      if i < 0 then a.length + i else i
    inline def asEndpointOf[A](a: Array[A]): Int =
      val i: Int = idx
      val n = a.length + i + 1
      if i >= 0 && n < 0 then Int.MaxValue else n
    inline def asEndpointOf(a: String): Int =
      val i: Int = idx
      val n = a.length + i + 1
      if i >= 0 && n < 0 then Int.MaxValue else n

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

  inline def arrayed[A]()(using ClassTag[A]): Array[A] =
    new Array[A](if i > 0 then i else 0)
  inline def arrayed[A](f: Int => A)(using ClassTag[A]): Array[A] =
    val a = new Array[A](if i > 0 then i else 0)
    var k = 0
    while k < a.length do
      a(k) = f(k)
      k += 1
    a
  inline def arrayedBreakably[A](f: boundary.Label[shortcut.Type] ?=> Int => A)(using ClassTag[A]): Array[A] =
    val a = new Array[A](if i > 0 then i else 0)
    shortcut.outer:
      var k = 0
      while k < a.length do
        shortcut.inner:
          a(k) = f(k)
        k += 1
    a
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

  inline def peek()(inline f: A => Unit): a.type =
    var i = 0
    while i < a.length do
      f(a(i))
      i += 1
    a
  inline def peek(i0: Int, iN: Int)(inline f: A => Unit): a.type =
    var i = i0
    while i < iN do
      f(a(i))
      i += 1
    a
  inline def peek(v: Iv | PIv)(inline f: A => Unit): a.type =
    val iv = Iv.of(v, a)
    peek(iv.i0, iv.iN)(f)
  inline def peek(inline rg: collection.immutable.Range)(inline f: A => Unit): a.type =
    val iv = Iv of rg
    peek(iv.i0, iv.iN)(f)
  inline def peek(indices: Array[Int])(inline f: A => Unit): a.type =
    var i = 0
    while i < indices.length do
      f(a(indices(i)))
      i += 1
    a
  inline def peek(indices: scala.collection.IntStepper)(inline f: A => Unit): a.type =
    while indices.hasStep do
      f(a(indices.nextStep))
    a

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
    val iv = Iv.of(v, a)
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
    var i = start
    while i >= 0 && i < a.length && n < Int.MaxValue do
      n += 1
      i = f(a(i), i)
    n

  inline def gather[Z](zero: Z)()(inline f: (Z, A, Int) => Z) =
    var i = 0
    var z = zero
    while i < a.length do
      z = f(z, a(i), i)
      i += 1
    z
  inline def gather[Z](zero: Z)(i0: Int, iN: Int)(inline f: (Z, A, Int) => Z): Z =
    var i = i0
    var z = zero
    while i < iN do
      z = f(z, a(i), i)
      i += 1
    z
  inline def gather[Z](zero: Z)(v: Iv | PIv)(inline f: (Z, A, Int) => Z): Z =
    val iv = Iv.of(v, a)
    gather(zero)(iv.i0, iv.iN)(f)
  inline def gather[Z](zero: Z)(inline rg: collection.immutable.Range)(inline f: (Z, A, Int) => Z): Z =
    val iv = Iv of rg
    gather(zero)(iv.i0, iv.iN)(f)
  inline def gather[Z](zero: Z)(indices: Array[Int])(inline f: (Z, A, Int) => Z): Z =
    var i = 0
    var z = zero
    while i < indices.length do
      val j = indices(i)
      z = f(z, a(j), j)
      i += 1
    z
  inline def gather[Z](zero: Z)(indices: scala.collection.IntStepper)(inline f: (Z, A, Int) => Z): Z =
    var z = zero
    while indices.hasStep do
      val j = indices.nextStep
      z = f(z, a(j), j)
    z

  @targetName("update_All_constant")
  inline def update(value: A): Unit =
    update(Iv(0, a.length), value)
  @targetName("update_All_array")
  inline def update(values: Array[A]): Unit =
    update(Iv(0, a.length), values)

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

  @targetName("set_All_generate")
  inline def set()(inline generator: () => A): Unit =
    set(0, a.length)(generator)
  @targetName("set_All_index")
  inline def set()(inline indexer: Int => A): Unit =
    set(0, a.length)(indexer)
  @targetName("set_All_function")
  inline def set()(inline function: (A, Int) => A): Unit =
    set(0, a.length)(function)

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

  inline def copyWith[B](inline f: A => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](a.length)
    var i = 0
    while i < a.length do
      b(i) = f(a(i))
      i += 1
    b
  inline def copyOp[B](inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](a.length)
    var i = 0
    while i < a.length do
      b(i) = op(a(i), i)
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

  inline def whereIn(i0: Int, iN: Int)(inline pick: A => Boolean): Array[Int] =
    var ix = new Array[Int](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else iN - i0)
    var i = i0
    var j = 0
    while i < iN do
      if pick(a(i)) then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = i
        j += 1
      i += 1
    ix.shrinkTo(j)
  inline def whereIn(v: Iv | PIv)(inline pick: A => Boolean): Array[Int] =
    val iv = Iv.of(v, a)
    whereIn(iv.i0, iv.iN)(pick)
  inline def whereIn(inline rg: Range)(inline pick: A => Boolean): Array[Int] =
    val iv = Iv of rg
    whereIn(iv.i0, iv.iN)(pick)

  inline def whereFrom(indices: Array[Int])(inline pick: A => Boolean): Array[Int] =
    var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
    var i = 0
    var j = 0
    while i < indices.length do
      val k = indices(i)
      if pick(a(k)) then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = k
        j += 1
      i += 1  
    ix.shrinkTo(j)  

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
  inline def inject(that: Array[A])(v: Iv | PIv): Int =
    val iv = Iv.of(v, a)
    inject(that, 0)(iv.i0, iv.iN)
  inline def inject(that: Array[A], where: Int)(v: Iv | PIv): Int =
    val iv = Iv.of(v, a)
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

  inline def injectOp[B](that: Array[B])()(inline f: (A, Int) => B): Int =
    injectOp(that, 0)(0, a.length)(f)
  inline def injectOp[B](that: Array[B], where: Int)()(inline f: (A, Int) => B): Int =
    injectOp(that, where)(0, a.length)(f)
  inline def injectOp[B](that: Array[B])(i0: Int, iN: Int)(inline f: (A, Int) => B): Int =
    injectOp(that, 0)(i0, iN)(f)
  inline def injectOp[B](that: Array[B], where: Int)(i0: Int, iN: Int)(inline f: (A, Int) => B): Int =
    var i = i0
    var j = where
    while i < iN do
      that(j) = f(a(i), i)
      j += 1
      i += 1
    iN - i0
  inline def injectOp[B](that: Array[B])(v: Iv | PIv)(inline f: (A, Int) => B): Int =
    val iv = Iv.of(v, a)
    injectOp[B](that, 0)(iv.i0, iv.iN)(f)
  inline def injectOp[B](that: Array[B], where: Int)(v: Iv | PIv)(inline f: (A, Int) => B): Int =
    val iv = Iv.of(v, a)
    injectOp[B](that, where)(iv.i0, iv.iN)(f)
  inline def injectOp[B](that: Array[B])(inline rg: collection.immutable.Range)(inline f: (A, Int) => B): Int =
    val iv = Iv of rg
    injectOp[B](that, 0)(iv.i0, iv.iN)(f)
  inline def injectOp[B](that: Array[B], where: Int)(inline rg: collection.immutable.Range)(inline f: (A, Int) => B): Int =
    val iv = Iv of rg
    injectOp[B](that, where)(iv.i0, iv.iN)(f)
  inline def injectOp[B](that: Array[B])(indices: Array[Int])(inline f: (A, Int) => B): Int =
    injectOp[B](that, 0)(indices)(f)
  inline def injectOp[B](that: Array[B], where: Int)(indices: Array[Int])(inline f: (A, Int) => B): Int =
    var i = 0
    var j = where
    while i < indices.length do
      val k = indices(i)
      that(j) = f(a(k), k)
      i += 1
      j += 1
    i
  inline def injectOp[B](that: Array[B])(indices: scala.collection.IntStepper)(inline f: (A, Int) => B): Int =
    injectOp[B](that, 0)(indices)(f)
  inline def injectOp[B](that: Array[B], where: Int)(indices: scala.collection.IntStepper)(inline f: (A, Int) => B): Int =
    var j = where
    while indices.hasStep do
      val i = indices.nextStep
      that(j) = f(a(i), i)
      j += 1
    j - where
  inline def injectOp[B](that: Array[B])(inline pick: A => Boolean)(inline f: (A, Int) => B): Int =
    injectOp[B](that, 0)(pick)(f)
  inline def injectOp[B](that: Array[B], where: Int)(inline pick: A => Boolean)(inline f: (A, Int) => B): Int =
    var i = 0
    var j = where
    while i < a.length do
      val x = a(i)
      if pick(x) then
        that(j) = f(x, i)
        j += 1 
      i += 1
    j - where

  inline def select(i0: Int, iN: Int)(using ClassTag[A]): Array[A] =
    val b = new Array[A](iN - i0)
    java.lang.System.arraycopy(a, i0, b, 0, b.length)
    b
  inline def select(v: Iv | PIv)(using ClassTag[A]): Array[A] =
    val iv = Iv.of(v, a)
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

  transparent inline def selectOp[B](i0: Int, iN: Int)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](iN - i0)
    var i = i0
    while i < iN do
      b(i - i0) = op(a(i), i)
      i += 1
    b
  transparent inline def selectOp[B](v: Iv | PIv)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val iv = Iv.of(v, a)
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

  transparent inline def fuse[B](inline add: (A, Int, B => Unit) => Unit)(using ClassTag[B]): Array[B] =
    var bs = new Array[B](if a.length < 8 then a.length else 8)
    var i = 0
    var j = 0
    while i < a.length do
      add(a(i), i, b => { if j >= bs.length then bs = bs.enlargeTo(bs.length | (bs.length << 1)); bs(j) = b; j += 1 })
      i += 1
    bs.shrinkTo(j)

  transparent inline def diced(indices: Array[Int], mode: "" | "()" | "[)" | "(]" | "[]", endpoints: "endpoints" | "no endpoints")(using ClassTag[A]): Array[Array[A]] =
    val empty = inline endpoints match
      case "no endpoints" => indices.length < 2
      case _ => indices.length == 0 && a.length == 0
    if empty then
      inline if endpoints == "endpoints" && mode != "" then
        val bss = new Array[Array[A]](1)
        bss(0) = new Array[A](0)
        bss
      else new Array[Array[A]](0)
    else
      var bss: Array[Array[A]] = new Array[Array[A]](inline if endpoints == "no endpoints" then indices.length-1 else indices.length+1)
      var bzero: Array[A] = null
      var i0 = inline if endpoints == "no endpoints" then indices(0) else inline mode match
        case "[)" | "[]" => 0
        case _           => -1
      var j = 0
      var k = 0
      while j < bss.length do
        val iN =
          inline if endpoints == "no endpoints" then
            indices(j+1)
          else
            if j < indices.length then indices(j)
            else inline mode match
              case "(]" | "[]" => a.length - 1
              case _           => a.length
        j += 1
        if i0 == iN then
          inline mode match
            case ""   =>
            case "[]" =>
              bss(k) = new Array[A](1)
              bss(k)(0) = a(i0)
              k += 1
            case _    =>
              bss(k) = new Array[A](0)
              k += 1
        else
          val bump = if iN < i0 then -1 else 1
          var h0 = inline if mode != "[)" && mode != "[]" then i0 + bump else i0
          var hN = inline if mode == "(]" || mode == "[]" then iN + bump else iN
          val n = if hN < h0 then h0 - hN else hN - h0
          if n == 0 then
            inline mode match
              case "" =>
              case _  =>
                if bzero eq null then bzero = new Array[A](0)
                bss(k) = bzero
                k += 1
          else
            val b = new Array[A](n)
            if h0 > hN then
              var h = h0
              var g = 0
              while h != hN do
                b(g) = a(h)
                h -= 1
                g += 1
            else if n > 0 then java.lang.System.arraycopy(a, h0, b, 0, n)
            bss(k) = b
            k += 1
          i0 = iN
      inline if mode == "" then
        if k < bss.length then
          java.util.Arrays.copyOf(bss, k)
        else
          bss
      else
        bss
  transparent inline def diced(indices: Array[Int], style: "()" | "[)" | "(]" | "[]" | "no endpoints")(using ClassTag[A]): Array[Array[A]] =
    inline style match
      case "no endpoints"                 => diced(indices, "", "no endpoints")
      case s: ("()" | "(]" | "[)" | "[]") => diced(indices, s, "endpoints")
  transparent inline def diced(indices: Array[Int])(using ClassTag[A]): Array[Array[A]] = diced(indices, "", "endpoints")
}



opaque type ClippedArray[A] = Array[A]
object ClippedArray {
  inline def wrap[A](a: Array[A]): ClippedArray[A] = a

  extension [A](ca: ClippedArray[A])
    inline def unwrap: Array[A] = ca

  extension [A](ca: kse.basics.ClippedArray[A]) {
    inline def use(i: Int)(inline f: A => Unit): Array[A] =
      val a = ca.unwrap
      if i >= 0 && i < a.length then f(a(i))
      a
    inline def zap(i: Int)(inline f: A => A): Array[A] =
      val a = ca.unwrap
      if i >= 0 && i < a.length then a(i) = f(a(i))
      a

    inline def breakable: kse.basics.ShortClipArray[A] = ShortClipArray wrap ca.unwrap

    inline def peek(i0: Int, iN: Int)(inline f: A => Unit): Array[A] =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      while i < iM do
        f(a(i))
        i += 1
      a
    inline def peek(v: Iv | PIv)(inline f: A => Unit): Array[A] =
      val iv = Iv.of(v, ca.unwrap)
      peek(iv.i0, iv.iN)(f)
    inline def peek(inline rg: collection.immutable.Range)(inline f: A => Unit): Array[A] =
      val iv = Iv of rg
      peek(iv.i0, iv.iN)(f)
    inline def peek(indices: Array[Int])(inline f: A => Unit): Array[A] =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then f(a(j))
        i += 1
      a
    inline def peek(indices: scala.collection.IntStepper)(inline f: A => Unit): Array[A] =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then f(a(j))
      a

    inline def visit(i0: Int, iN: Int)(inline f: (A, Int) => Unit): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      while i < iM do
        f(a(i), i)
        i += 1
    inline def visit(v: Iv | PIv)(inline f: (A, Int) => Unit): Unit =
      val iv = Iv.of(v, ca.unwrap)
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

    inline def gather[Z](zero: Z)(i0: Int, iN: Int)(inline f: (Z, A, Int) => Z): Z =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var z = zero
      while i < iM do
        z = f(z, a(i), i)
        i += 1
      z
    inline def gather[Z](zero: Z)(v: Iv | PIv)(inline f: (Z, A, Int) => Z): Z =
      val iv = Iv.of(v, ca.unwrap)
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(inline rg: collection.immutable.Range)(inline f: (Z, A, Int) => Z): Z =
      val iv = Iv of rg
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(indices: Array[Int])(inline f: (Z, A, Int) => Z): Z =
      val a = ca.unwrap
      var i = 0
      var z = zero
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then z = f(z, a(j), j)
        i += 1
      z
    inline def gather[Z](zero: Z)(indices: scala.collection.IntStepper)(inline f: (Z, A, Int) => Z): Z =
      val a = ca.unwrap
      var z = zero
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then z = f(z, a(j), j)
      z

    @targetName("update_All_array")
    inline def update(values: Array[A]): Unit =
      update(Iv(0, ca.unwrap.length), values)

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
      if j > i then java.lang.System.arraycopy(values, 0, a, i, j - i)

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
      var k = 0
      while i < indices.length && k < values.length do
        val j = indices(i)
        if j >= 0 && j < a.length then
          a(j) = values(k)
          k += 1
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
        if j >= 0 && j < a.length then
          a(j) = values(i)
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

    inline def whereIn(i0: Int, iN: Int)(inline pick: A => Boolean): Array[Int] =
      val a = ca.unwrap
      var i = i0
      if i0 < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var ix = new Array[Int](if iM - i < 0 then 0 else if iM - i > 8 then 8 else iM - i)
      var j = 0
      while i < iM do
        if pick(a(i)) then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = i
          j += 1
        i += 1
      ix.shrinkTo(j)
    inline def whereIn(v: Iv | PIv)(inline pick: A => Boolean): Array[Int] =
      val iv = Iv.of(v, ca.unwrap)
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: A => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)

    inline def whereFrom(indices: Array[Int])(inline pick: A => Boolean): Array[Int] =
      val a = ca.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        if k >= 0 && k < a.length && pick(a(k)) then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = k
          j += 1
        i += 1  
      ix.shrinkTo(j)

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
    inline def inject(that: Array[A])(v: Iv | PIv): Int =
      val iv = Iv.of(v, ca.unwrap)
      inject(that, 0)(iv.i0, iv.iN)
    inline def inject(that: Array[A], where: Int)(v: Iv | PIv): Int =
      val iv = Iv.of(v, ca.unwrap)
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

    inline def injectOp[B](that: Array[B])()(inline f: (A, Int) => B): Int =
      injectOp[B](that, 0)(0, ca.unwrap.length)(f)
    inline def injectOp[B](that: Array[B], where: Int)()(inline f: (A, Int) => B): Int =
      injectOp[B](that, where)(0, ca.unwrap.length)(f)
    inline def injectOp[B](that: Array[B])(i0: Int, iN: Int)(inline f: (A, Int) => B): Int =
      injectOp[B](that, 0)(i0, iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(i0: Int, iN: Int)(inline f: (A, Int) => B): Int =
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
        val n0 = n
        while n > 0 do
          that(w) = f(a(i), i)
          w += 1
          i += 1
          n -= 1
        n0
      else 0
    inline def injectOp[B](that: Array[B])(v: Iv | PIv)(inline f: (A, Int) => B): Int =
      val iv = Iv.of(v, ca.unwrap)
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(v: Iv | PIv)(inline f: (A, Int) => B): Int =
      val iv = Iv.of(v, ca.unwrap)
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(inline rg: collection.immutable.Range)(inline f: (A, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(inline rg: collection.immutable.Range)(inline f: (A, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(indices: Array[Int])(inline f: (A, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: Array[Int])(inline f: (A, Int) => B): Int =
      val a = ca.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      while i < indices.length && j < that.length do
        val k = indices(i)
        if k >= 0 && k < a.length then
          that(j) = f(a(k), k)
          j += 1
        i += 1
      if where < 0 then j else j - where
    inline def injectOp[B](that: Array[B])(indices: scala.collection.IntStepper)(inline f: (A, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: scala.collection.IntStepper)(inline f: (A, Int) => B): Int =
      val a = ca.unwrap
      var j = where
      if j < 0 then j = 0
      while indices.hasStep && j < that.length do
        val i = indices.nextStep
        if i >= 0 && i < a.length then
          that(j) = f(a(i), i)
          j += 1
      if where < 0 then j else j - where
    inline def injectOp[B](that: Array[B])(inline pick: A => Boolean)(inline f: (A, Int) => B): Int =
      injectOp[B](that, 0)(pick)(f)
    inline def injectOp[B](that: Array[B], where: Int)(inline pick: A => Boolean)(inline f: (A, Int) => B): Int =
      val a = ca.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      while i < a.length && j < that.length do
        val x = a(i)
        if pick(x) then
          that(j) = f(x, i)
          j += 1 
        i += 1
      if where < 0 then j else j - where

    inline def select(i0: Int, iN: Int)(using ClassTag[A]): Array[A] =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j >= a.length then j = a.length
      val b = new Array[A](if i < j then j - i else 0)
      if b.length > 0 then java.lang.System.arraycopy(a, i, b, 0, b.length)
      b
    inline def select(v: Iv | PIv)(using ClassTag[A]): Array[A] =
      val iv = Iv.of(v, ca.unwrap)
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
      if j > a.length then j = a.length
      if j < i then j = i
      val b = new Array[B](j - i)
      val offset = i
      while i < j do
        b(i - offset) = op(a(i), i)
        i += 1
      b
    transparent inline def selectOp[B](v: Iv | PIv)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv.of(v, ca.unwrap)
      selectOp(iv.i0, iv.iN)(op)
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
        if k >= 0 && k < a.length then
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

    transparent inline def diced(indices: Array[Int], mode: "" | "()" | "[)" | "(]" | "[]", endpoints: "endpoints" | "no endpoints")(using ClassTag[A]): Array[Array[A]] =
      val a = ca.unwrap
      val empty = inline endpoints match
        case "no endpoints" => indices.length < 2
        case _ => indices.length == 0 && a.length == 0
      if empty then
        inline if endpoints == "endpoints" && mode != "" then
          val bss = new Array[Array[A]](1)
          bss(0) = new Array[A](0)
          bss
        else new Array[Array[A]](0)
      else
        var bss: Array[Array[A]] = new Array[Array[A]](inline if endpoints == "no endpoints" then indices.length-1 else indices.length+1)
        var bzero: Array[A] = null
        var i0 = 
          inline if endpoints == "no endpoints" then indices(0)
          else -1
        var j = 0
        var k = 0
        while j < bss.length do
          val iN =
            inline if endpoints == "no endpoints" then indices(j+1)
            else
              if j < indices.length then indices(j)
              else a.length+1
          j += 1
          if !((i0 < 0 && iN < 0) || (i0 >= a.length && iN >= a.length)) then
            if i0 == iN then
              inline mode match
                case ""   =>
                case "[]" =>
                  if i0 >= 0 && i0 < a.length then
                    bss(k) = new Array[A](1)
                    bss(k)(0) = a(i0)
                    k += 1
                case _    =>
                  if i0 >= 0 && i0 < a.length then
                    bss(k) = new Array[A](0)
                    k += 1
            else
              var h0 = i0
              var hN = iN
              inline mode match
                case "[]" =>
                  if hN < h0 then
                    if h0 >= a.length then h0 = a.length - 1
                    hN = if hN >= 0 then hN - 1 else - 1
                  else
                    if h0 < 0 then h0 = 0
                    hN = if hN < a.length then hN + 1 else a.length
                case "[)" =>
                  if hN < h0 then
                    if h0 >= a.length then h0 = a.length - 1
                    if hN < -1 then hN = -1
                  else
                    if h0 < 0 then h0 = 0
                    if hN > a.length then hN = a.length
                case "(]" =>
                  if hN < h0 then
                    h0 = if h0 > a.length then a.length-1 else h0-1
                    hN = if hN > 0 then hN - 1 else -1
                  else
                    h0 = if h0 < 0 then 0 else h0 + 1
                    hN = if hN < a.length then hN + 1 else a.length
                case _ =>
                  if hN < h0 then
                    h0 = if h0 > a.length - 1 then a.length - 1 else h0 - 1
                    if hN < -1 then hN = -1
                  else if hN > h0 then
                    h0 = if h0 < 0 then 0 else h0 + 1
                    if hN > a.length then hN = a.length
              val n = if hN < h0 then h0 - hN else hN - h0
              if n == 0 then
                inline mode match
                  case "" =>
                  case _  =>
                    if bzero eq null then bzero = new Array[A](0)
                    bss(k) = bzero
                    k += 1
              else
                val b = new Array[A](n)
                if h0 > hN then
                  var h = h0
                  var g = 0
                  while h != hN do
                    b(g) = a(h)
                    h -= 1
                    g += 1
                else if n > 0 then java.lang.System.arraycopy(a, h0, b, 0, n)
                bss(k) = b
                k += 1
          i0 = iN
        if k < bss.length then
          java.util.Arrays.copyOf(bss, k)
        else
          bss
    transparent inline def diced(indices: Array[Int], style: "()" | "[)" | "(]" | "[]" | "no endpoints")(using ClassTag[A]): Array[Array[A]] =
      inline style match
        case "no endpoints"                 => diced(indices, "", "no endpoints")
        case s: ("()" | "(]" | "[)" | "[]") => diced(indices, s, "endpoints")
    transparent inline def diced(indices: Array[Int])(using ClassTag[A]): Array[Array[A]] = diced(indices, "", "endpoints")
  }
}


opaque type ShortcutArray[A] = Array[A]
object ShortcutArray {
  inline def wrap[A](a: Array[A]): ShortcutArray[A] = a

  extension [A](sa: ShortcutArray[A])
    inline def unwrap: Array[A] = sa

  extension [A](sa: kse.basics.ShortcutArray[A]) {
    inline def clip: kse.basics.ShortClipArray[A] = ShortClipArray wrap sa.unwrap

    inline def peek()(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          f(a(i))
          i += 1
      a
    inline def peek(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val a = sa.unwrap
      var i = i0
      shortcut.quittable:
        while i < iN do
          f(a(i))
          i += 1
      a
    inline def peek(v: Iv | PIv)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val iv = Iv.of(v, sa.unwrap)
      peek(iv.i0, iv.iN)(f)
    inline def peek(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val iv = Iv of rg
      peek(iv.i0, iv.iN)(f)
    inline def peek(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          f(a(indices(i)))
          i += 1
      a
    inline def peek(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          f(a(indices.nextStep))
      a

    inline def gather[Z](zero: Z)()(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z) =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = 0
        while i < a.length do
          z = f(z, a(i), i)
          i += 1
      z
    inline def gather[Z](zero: Z)(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = i0
        while i < iN do
          z = f(z, a(i), i)
          i += 1
      z
    inline def gather[Z](zero: Z)(v: Iv | PIv)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      val iv = Iv.of(v, sa.unwrap)
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      val iv = Iv of rg
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = 0
        while i < indices.length do
          val j = indices(i)
          z = f(z, a(j), j)
          i += 1
      z
    inline def gather[Z](zero: Z)(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          z = f(z, a(j), j)
      z

    transparent inline def copyWith[B](inline f: boundary.Label[shortcut.Type] ?=> A => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      val b = new Array[B](a.length)
      var i = 0
      var j = 0
      shortcut.outer:
        while i < a.length do
          shortcut.inner:
            b(j) = f(a(i))
            j += 1
          i += 1
      b.shrinkTo(j)
    transparent inline def copyOp[B](inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
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

    inline def where(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if a.length <= 8 then a.length else 8)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < a.length do
          if pick(a(i)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = i
            j += 1
          i += 1
      ix.shrinkTo(j)

    inline def whereIn(i0: Int, iN: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else iN - i0)
      var i = i0
      var j = 0
      shortcut.quittable:
        while i < iN do
          if pick(a(i)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = i
            j += 1
          i += 1
      ix.shrinkTo(j)
    inline def whereIn(v: Iv | PIv)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val iv = Iv.of(v, sa.unwrap)
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)

    inline def whereFrom(indices: Array[Int])(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < indices.length do
          val k = indices(i)
          if pick(a(k)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = k
            j += 1
          i += 1  
      ix.shrinkTo(j)

    inline def inject(that: Array[A])(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Int =
      inject(that, 0)(pick)
    inline def inject(that: Array[A], where: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Int =
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

    inline def injectOp[B](that: Array[B])()(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      injectOp(that, 0)(0, sa.unwrap.length)(f)
    inline def injectOp[B](that: Array[B], where: Int)()(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      injectOp(that, where)(0, sa.unwrap.length)(f)
    inline def injectOp[B](that: Array[B])(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      injectOp(that, 0)(i0, iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val a = sa.unwrap
      var i = i0
      var j = where
      shortcut.outer:
        while i < iN do
          shortcut.inner:
            that(j) = f(a(i), i)
            j += 1
          i += 1
      j - where
    inline def injectOp[B](that: Array[B])(v: Iv | PIv)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = Iv.of(v, sa.unwrap)
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(v: Iv | PIv)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = Iv.of(v, sa.unwrap)
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(indices: Array[Int])(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: Array[Int])(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val a = sa.unwrap
      var i = 0
      var j = where
      shortcut.outer:
        while i < indices.length do
          val k = indices(i)
          shortcut.inner:
            that(j) = f(a(k), k)
            j += 1
          i += 1
      j - where
    inline def injectOp[B](that: Array[B])(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val a = sa.unwrap
      var j = where
      shortcut.outer:
        while indices.hasStep do
          val i = indices.nextStep
          shortcut.inner:
            that(j) = f(a(i), i)
            j += 1
      j - where

    inline def select(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean)(using ClassTag[A]): Array[A] =
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
    transparent inline def selectOp[B](v: Iv | PIv)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv.of(v, sa.unwrap)
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

    transparent inline def fuse[B](inline add: boundary.Label[shortcut.Quits.type] ?=> (A, Int, B => Unit) => Unit)(using ClassTag[B]): Array[B] =
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
    inline def peek(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      shortcut.quittable:
        while i < iM do
          f(a(i))
          i += 1
      a
    inline def peek(v: Iv | PIv)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val iv = Iv.of(v, sc.unwrap)
      peek(iv.i0, iv.iN)(f)
    inline def peek(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val iv = Iv of rg
      peek(iv.i0, iv.iN)(f)
    inline def peek(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val a = sc.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then f(a(j))
          i += 1
      a
    inline def peek(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Array[A] =
      val a = sc.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then f(a(j))
      a

    inline def gather[Z](zero: Z)(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
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
    inline def gather[Z](zero: Z)(v: Iv | PIv)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      val iv = Iv.of(v, sc.unwrap)
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      val iv = Iv of rg
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      val a = sc.unwrap
      var i = 0
      var z = zero
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then z = f(z, a(j), j)
          i += 1
      z
    inline def gather[Z](zero: Z)(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      val a = sc.unwrap
      var z = zero
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then z = f(z, a(j), j)
      z

    inline def whereIn(i0: Int, iN: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var ix = new Array[Int](if iM - i < 0 then 0 else if iM - i > 8 then 8 else iM - i)
      var j = 0
      shortcut.quittable:
        while i < iM do
          if pick(a(i)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = i
            j += 1
          i += 1
      ix.shrinkTo(j)
    inline def whereIn(v: Iv | PIv)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val iv = Iv.of(v, sc.unwrap)
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)

    inline def whereFrom(indices: Array[Int])(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val a = sc.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < indices.length do
          val k = indices(i)
          if k >= 0 && k < a.length && pick(a(k)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = k
            j += 1
          i += 1  
      ix.shrinkTo(j)

    inline def inject(that: Array[A])(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Int =
      inject(that, 0)(pick)
    inline def inject(that: Array[A], where: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Int =
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
      if where < 0 then j else j - where

    inline def injectOp[B](that: Array[B])()(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      injectOp(that, 0)(0, sc.unwrap.length)(f)
    inline def injectOp[B](that: Array[B], where: Int)()(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      injectOp(that, where)(0, sc.unwrap.length)(f)
    inline def injectOp[B](that: Array[B])(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      injectOp(that, 0)(i0, iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = where
      if j < 0 then j = 0
      var n = iN
      if iN > a.length then n = a.length
      shortcut.outer:
        while i < n && j < that.length do
          shortcut.inner:
            that(j) = f(a(i), i)
            j += 1
          i += 1
      if where < 0 then j else j - where
    inline def injectOp[B](that: Array[B])(v: Iv | PIv)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = Iv.of(v, sc.unwrap)
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(v: Iv | PIv)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = Iv.of(v, sc.unwrap)
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(indices: Array[Int])(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: Array[Int])(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val a = sc.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      shortcut.outer:
        while i < indices.length && j < that.length do
          val k = indices(i)
          if k >= 0 && k < a.length then
            shortcut.inner:
              that(j) = f(a(k), k)
              j += 1
          i += 1
      if where < 0 then j else j - where
    inline def injectOp[B](that: Array[B])(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val a = sc.unwrap
      var j = where
      if j < 0 then j = 0
      shortcut.outer:
        while indices.hasStep && j < that.length do
          val i = indices.nextStep
          if i >= 0 && i < a.length then
            shortcut.inner:
              that(j) = f(a(i), i)
              j += 1
      if where < 0 then j else j - where

    transparent inline def selectOp[B](i0: Int, iN: Int)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j > a.length then j = a.length
      if j < i then j = i
      val b = new Array[B](j - i)
      var k = 0
      shortcut.outer:
        while i < j do
          shortcut.inner:
            b(k) = op(a(i), i)
            k += 1
          i += 1
      b.shrinkTo(k)
    transparent inline def selectOp[B](v: Iv | PIv)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv.of(v, sc.unwrap)
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
          if k >= 0 && k < a.length then shortcut.inner:
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
extension (az: Array[Boolean]) {
  inline def shrinkCopy(size: Int): Array[Boolean] =
    if size < az.length then java.util.Arrays.copyOf(az, size) else az

  inline def copyToSize(size: Int): Array[Boolean] =
    java.util.Arrays.copyOf(az, size)
  @targetName("copyToEndpointSize")
  inline def copyToSize(endpoint: kse.basics.FromLengthIdx): Array[Boolean] =
    java.util.Arrays.copyOf(az, FromLengthIdx.asEndpointOf(endpoint)(az))

  inline def copyOfRange(i0: Int, iN: Int): Array[Boolean] =
    java.util.Arrays.copyOfRange(az, i0, iN)
  inline def copyOfRange(v: Iv | PIv): Array[Boolean] =
    val iv = Iv.of(v, az)
    java.util.Arrays.copyOfRange(az, iv.i0, iv.iN)
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Boolean] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(az, iv.i0, iv.iN)

  inline def fill(x: Boolean): az.type =
    java.util.Arrays.fill(az, x)
    az
  inline def fillRange(i0: Int, iN: Int)(x: Boolean): az.type = 
    java.util.Arrays.fill(az, i0, iN, x)
    az
  inline def fillRange(v: Iv | PIv)(x: Boolean): az.type = 
    val iv = Iv.of(v, az)
    java.util.Arrays.fill(az, iv.i0, iv.iN, x)
    az
  inline def fillRange(inline rg: collection.immutable.Range)(x: Boolean): az.type =
    val iv = Iv of rg
    java.util.Arrays.fill(az, iv.i0, iv.iN, x)
    az
}

/** Byte Array specific functionality from java.util.Arrays and java.lang.System */
extension (ab: Array[Byte]) {
  inline def packInts: Array[Int] = ArrayReform.toInts(ab)
  inline def packFloats: Array[Float] = ArrayReform.toFloats(ab)
  inline def packLongs: Array[Long] = ArrayReform.toLongs(ab)
  inline def packDoubles: Array[Double] = ArrayReform.toDoubles(ab)

  inline def shrinkCopy(size: Int): Array[Byte] = if size < ab.length then java.util.Arrays.copyOf(ab, size) else ab

  inline def copyToSize(size: Int): Array[Byte] =
    java.util.Arrays.copyOf(ab, size)
  @targetName("copyToEndpointSize")
  inline def copyToSize(endpoint: kse.basics.FromLengthIdx): Array[Byte] =
    java.util.Arrays.copyOf(ab, FromLengthIdx.asEndpointOf(endpoint)(ab))

  inline def copyOfRange(i0: Int, iN: Int): Array[Byte] =
    java.util.Arrays.copyOfRange(ab, i0, iN)
  inline def copyOfRange(v: Iv | PIv): Array[Byte] =
    val iv = Iv.of(v, ab)
    java.util.Arrays.copyOfRange(ab, iv.i0, iv.iN)
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Byte] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(ab, iv.i0, iv.iN)

  inline def search(x: Byte): Int =
    java.util.Arrays.binarySearch(ab, x)
  inline def searchRange(i0: Int, iN: Int)(x: Byte): Int =
    java.util.Arrays.binarySearch(ab, i0, iN, x)
  inline def searchRange(v: Iv | PIv)(x: Byte): Int =
    val iv = Iv.of(v, ab)
    java.util.Arrays.binarySearch(ab, iv.i0, iv.iN, x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Byte): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(ab, iv.i0, iv.iN, x)


  inline def fill(x: Byte): ab.type =
    java.util.Arrays.fill(ab, x)
    ab
  inline def fillRange(i0: Int, iN: Int)(x: Byte): ab.type = 
    java.util.Arrays.fill(ab, i0, iN, x)
    ab
  inline def fillRange(v: Iv | PIv)(x: Byte): ab.type =
    val iv = Iv.of(v, ab)
    java.util.Arrays.fill(ab, iv.i0, iv.iN, x)
    ab
  inline def fillRange(inline rg: collection.immutable.Range)(x: Byte): ab.type =
    val iv = Iv of rg
    java.util.Arrays.fill(ab, iv.i0, iv.iN, x)
    ab

  inline def sort(): ab.type =
    java.util.Arrays.sort(ab)
    ab
  inline def sortRange(i0: Int, iN: Int): ab.type =
    java.util.Arrays.sort(ab, i0, iN)
    ab
  inline def sortRange(v: Iv | PIv): ab.type =
    val iv = Iv.of(v, ab)
    java.util.Arrays.sort(ab, iv.i0, iv.iN)
    ab
  inline def sortRange(inline rg: collection.immutable.Range): ab.type =
    val iv = Iv of rg
    java.util.Arrays.sort(ab, iv.i0, iv.iN)
    ab

  inline def isSorted: Boolean =
    isSortedRange(0, ab.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ab(i-1) <= ab(i) do i += 1
      i >= iN
  inline def isSortedRange(v: Iv | PIv): Boolean =
    val iv = Iv.of(v, ab)
    isSortedRange(iv.i0, iv.iN)
  inline def isSortedRange(inline rg: collection.immutable.Range): Boolean =
    val iv = Iv of rg
    isSortedRange(iv.i0, iv.iN)
}

/** Short Array specific functionality from java.util.Arrays and java.lang.System */
extension (as: Array[Short]) {
  inline def shrinkCopy(size: Int): Array[Short] = if size < as.length then java.util.Arrays.copyOf(as, size) else as

  inline def copyToSize(size: Int): Array[Short] =
    java.util.Arrays.copyOf(as, size)
  @targetName("copyToEndpointSize")
  inline def copyToSize(endpoint: kse.basics.FromLengthIdx): Array[Short] =
    java.util.Arrays.copyOf(as, FromLengthIdx.asEndpointOf(endpoint)(as))

  inline def copyOfRange(i0: Int, iN: Int): Array[Short] =
    java.util.Arrays.copyOfRange(as, i0, iN)
  inline def copyOfRange(v: Iv | PIv): Array[Short] =
    val iv = Iv.of(v, as)
    java.util.Arrays.copyOfRange(as, iv.i0, iv.iN)
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Short] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(as, iv.i0, iv.iN)

  inline def search(x: Short): Int =
    java.util.Arrays.binarySearch(as, x)
  inline def searchRange(i0: Int, iN: Int)(x: Short): Int =
    java.util.Arrays.binarySearch(as, i0, iN, x)
  inline def searchRange(v: Iv | PIv)(x: Short): Int =
    val iv = Iv.of(v, as)
    java.util.Arrays.binarySearch(as, iv.i0, iv.iN, x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Short): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(as, iv.i0, iv.iN, x)


  inline def fill(x: Short): as.type =
    java.util.Arrays.fill(as, x)
    as
  inline def fillRange(i0: Int, iN: Int)(x: Short): as.type = 
    java.util.Arrays.fill(as, i0, iN, x)
    as
  inline def fillRange(v: Iv | PIv)(x: Short): as.type =
    val iv = Iv.of(v, as)
    java.util.Arrays.fill(as, iv.i0, iv.iN, x)
    as
  inline def fillRange(inline rg: collection.immutable.Range)(x: Short): as.type =
    val iv = Iv of rg
    java.util.Arrays.fill(as, iv.i0, iv.iN, x)
    as

  inline def sort(): as.type =
    java.util.Arrays.sort(as)
    as
  inline def sortRange(i0: Int, iN: Int): as.type =
    java.util.Arrays.sort(as, i0, iN)
    as
  inline def sortRange(v: Iv | PIv): as.type =
    val iv = Iv.of(v, as)
    java.util.Arrays.sort(as, iv.i0, iv.iN)
    as
  inline def sortRange(inline rg: collection.immutable.Range): as.type =
    val iv = Iv of rg
    java.util.Arrays.sort(as, iv.i0, iv.iN)
    as

  inline def isSorted: Boolean =
    isSortedRange(0, as.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && as(i-1) <= as(i) do i += 1
      i >= iN
  inline def isSortedRange(v: Iv | PIv): Boolean =
    val iv = Iv.of(v, as)
    isSortedRange(iv.i0, iv.iN)
  inline def isSortedRange(inline rg: collection.immutable.Range): Boolean =
    val iv = Iv of rg
    isSortedRange(iv.i0, iv.iN)
}

/** Char Array specific functionality from java.util.Arrays and java.lang.System */
extension (ac: Array[Char]) {
  inline def str: String = new String(ac)

  inline def shrinkCopy(size: Int): Array[Char] = if size < ac.length then java.util.Arrays.copyOf(ac, size) else ac

  inline def copyToSize(size: Int): Array[Char] =
    java.util.Arrays.copyOf(ac, size)
  @targetName("copyToEndpointSize")
  inline def copyToSize(endpoint: kse.basics.FromLengthIdx): Array[Char] =
    java.util.Arrays.copyOf(ac, FromLengthIdx.asEndpointOf(endpoint)(ac))

  inline def copyOfRange(i0: Int, iN: Int): Array[Char] =
    java.util.Arrays.copyOfRange(ac, i0, iN)
  inline def copyOfRange(v: Iv | PIv): Array[Char] =
    val iv = Iv.of(v, ac)
    java.util.Arrays.copyOfRange(ac, iv.i0, iv.iN)
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Char] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(ac, iv.i0, iv.iN)

  inline def search(x: Char): Int =
    java.util.Arrays.binarySearch(ac, x)
  inline def searchRange(i0: Int, iN: Int)(x: Char): Int =
    java.util.Arrays.binarySearch(ac, i0, iN, x)
  inline def searchRange(v: Iv | PIv)(x: Char): Int =
    val iv = Iv.of(v, ac)
    java.util.Arrays.binarySearch(ac, iv.i0, iv.iN, x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Char): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(ac, iv.i0, iv.iN, x)


  inline def fill(x: Char): ac.type =
    java.util.Arrays.fill(ac, x)
    ac
  inline def fillRange(i0: Int, iN: Int)(x: Char): ac.type = 
    java.util.Arrays.fill(ac, i0, iN, x)
    ac
  inline def fillRange(v: Iv | PIv)(x: Char): ac.type =
    val iv = Iv.of(v, ac)
    java.util.Arrays.fill(ac, iv.i0, iv.iN, x)
    ac
  inline def fillRange(inline rg: collection.immutable.Range)(x: Char): ac.type =
    val iv = Iv of rg
    java.util.Arrays.fill(ac, iv.i0, iv.iN, x)
    ac

  inline def sort(): ac.type =
    java.util.Arrays.sort(ac)
    ac
  inline def sortRange(i0: Int, iN: Int): ac.type =
    java.util.Arrays.sort(ac, i0, iN)
    ac
  inline def sortRange(v: Iv | PIv): ac.type =
    val iv = Iv.of(v, ac)
    java.util.Arrays.sort(ac, iv.i0, iv.iN)
    ac
  inline def sortRange(inline rg: collection.immutable.Range): ac.type =
    val iv = Iv of rg
    java.util.Arrays.sort(ac, iv.i0, iv.iN)
    ac

  inline def isSorted: Boolean =
    isSortedRange(0, ac.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ac(i-1) <= ac(i) do i += 1
      i >= iN
  inline def isSortedRange(v: Iv | PIv): Boolean =
    val iv = Iv.of(v, ac)
    isSortedRange(iv.i0, iv.iN)
  inline def isSortedRange(inline rg: collection.immutable.Range): Boolean =
    val iv = Iv of rg
    isSortedRange(iv.i0, iv.iN)
}

/** Int Array specific functionality from java.util.Arrays and java.lang.System */
extension (ai: Array[Int]) {
  inline def unpackBytes: Array[Byte] = ArrayReform.toBytes(ai)

  inline def shrinkCopy(size: Int): Array[Int] = if size < ai.length then java.util.Arrays.copyOf(ai, size) else ai

  inline def copyToSize(size: Int): Array[Int] =
    java.util.Arrays.copyOf(ai, size)
  @targetName("copyToEndpointSize")
  inline def copyToSize(endpoint: kse.basics.FromLengthIdx): Array[Int] =
    java.util.Arrays.copyOf(ai, FromLengthIdx.asEndpointOf(endpoint)(ai))

  inline def copyOfRange(i0: Int, iN: Int): Array[Int] =
    java.util.Arrays.copyOfRange(ai, i0, iN)
  inline def copyOfRange(v: Iv | PIv): Array[Int] =
    val iv = Iv.of(v, ai)
    java.util.Arrays.copyOfRange(ai, iv.i0, iv.iN)
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Int] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(ai, iv.i0, iv.iN)

  inline def search(x: Int): Int =
    java.util.Arrays.binarySearch(ai, x)
  inline def searchRange(i0: Int, iN: Int)(x: Int): Int =
    java.util.Arrays.binarySearch(ai, i0, iN, x)
  inline def searchRange(v: Iv | PIv)(x: Int): Int =
    val iv = Iv.of(v, ai)
    java.util.Arrays.binarySearch(ai, iv.i0, iv.iN, x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Int): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(ai, iv.i0, iv.iN, x)


  inline def fill(x: Int): ai.type =
    java.util.Arrays.fill(ai, x)
    ai
  inline def fillRange(i0: Int, iN: Int)(x: Int): ai.type = 
    java.util.Arrays.fill(ai, i0, iN, x)
    ai
  inline def fillRange(v: Iv | PIv)(x: Int): ai.type =
    val iv = Iv.of(v, ai)
    java.util.Arrays.fill(ai, iv.i0, iv.iN, x)
    ai
  inline def fillRange(inline rg: collection.immutable.Range)(x: Int): ai.type =
    val iv = Iv of rg
    java.util.Arrays.fill(ai, iv.i0, iv.iN, x)
    ai

  inline def sort(): ai.type =
    java.util.Arrays.sort(ai)
    ai
  inline def sortRange(i0: Int, iN: Int): ai.type =
    java.util.Arrays.sort(ai, i0, iN)
    ai
  inline def sortRange(v: Iv | PIv): ai.type =
    val iv = Iv.of(v, ai)
    java.util.Arrays.sort(ai, iv.i0, iv.iN)
    ai
  inline def sortRange(inline rg: collection.immutable.Range): ai.type =
    val iv = Iv of rg
    java.util.Arrays.sort(ai, iv.i0, iv.iN)
    ai

  inline def isSorted: Boolean =
    isSortedRange(0, ai.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ai(i-1) <= ai(i) do i += 1
      i >= iN
  inline def isSortedRange(v: Iv | PIv): Boolean =
    val iv = Iv.of(v, ai)
    isSortedRange(iv.i0, iv.iN)
  inline def isSortedRange(inline rg: collection.immutable.Range): Boolean =
    val iv = Iv of rg
    isSortedRange(iv.i0, iv.iN)
}

/** Long Array specific functionality from java.util.Arrays and java.lang.System */
extension (al: Array[Long]) {
  inline def unpackBytes: Array[Byte] = ArrayReform.toBytes(al)

  inline def shrinkCopy(size: Int): Array[Long] = if size < al.length then java.util.Arrays.copyOf(al, size) else al

  inline def copyToSize(size: Int): Array[Long] =
    java.util.Arrays.copyOf(al, size)
  @targetName("copyToEndpointSize")
  inline def copyToSize(endpoint: kse.basics.FromLengthIdx): Array[Long] =
    java.util.Arrays.copyOf(al, FromLengthIdx.asEndpointOf(endpoint)(al))

  inline def copyOfRange(i0: Int, iN: Int): Array[Long] =
    java.util.Arrays.copyOfRange(al, i0, iN)
  inline def copyOfRange(v: Iv | PIv): Array[Long] =
    val iv = Iv.of(v, al)
    java.util.Arrays.copyOfRange(al, iv.i0, iv.iN)
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Long] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(al, iv.i0, iv.iN)

  inline def search(x: Long): Int =
    java.util.Arrays.binarySearch(al, x)
  inline def searchRange(i0: Int, iN: Int)(x: Long): Int =
    java.util.Arrays.binarySearch(al, i0, iN, x)
  inline def searchRange(v: Iv | PIv)(x: Long): Int =
    val iv = Iv.of(v, al)
    java.util.Arrays.binarySearch(al, iv.i0, iv.iN, x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Long): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(al, iv.i0, iv.iN, x)


  inline def fill(x: Long): al.type =
    java.util.Arrays.fill(al, x)
    al
  inline def fillRange(i0: Int, iN: Int)(x: Long): al.type = 
    java.util.Arrays.fill(al, i0, iN, x)
    al
  inline def fillRange(v: Iv | PIv)(x: Long): al.type =
    val iv = Iv.of(v, al)
    java.util.Arrays.fill(al, iv.i0, iv.iN, x)
    al
  inline def fillRange(inline rg: collection.immutable.Range)(x: Long): al.type =
    val iv = Iv of rg
    java.util.Arrays.fill(al, iv.i0, iv.iN, x)
    al

  inline def sort(): al.type =
    java.util.Arrays.sort(al)
    al
  inline def sortRange(i0: Int, iN: Int): al.type =
    java.util.Arrays.sort(al, i0, iN)
    al
  inline def sortRange(v: Iv | PIv): al.type =
    val iv = Iv.of(v, al)
    java.util.Arrays.sort(al, iv.i0, iv.iN)
    al
  inline def sortRange(inline rg: collection.immutable.Range): al.type =
    val iv = Iv of rg
    java.util.Arrays.sort(al, iv.i0, iv.iN)
    al

  inline def isSorted: Boolean =
    isSortedRange(0, al.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && al(i-1) <= al(i) do i += 1
      i >= iN
  inline def isSortedRange(v: Iv | PIv): Boolean =
    val iv = Iv.of(v, al)
    isSortedRange(iv.i0, iv.iN)
  inline def isSortedRange(inline rg: collection.immutable.Range): Boolean =
    val iv = Iv of rg
    isSortedRange(iv.i0, iv.iN)
}

/** Float Array specific functionality from java.util.Arrays and java.lang.System */
extension (af: Array[Float]) {
  inline def unpackBytes: Array[Byte] = ArrayReform.toBytes(af)

  inline def shrinkCopy(size: Int): Array[Float] = if size < af.length then java.util.Arrays.copyOf(af, size) else af

  inline def copyToSize(size: Int): Array[Float] =
    java.util.Arrays.copyOf(af, size)
  @targetName("copyToEndpointSize")
  inline def copyToSize(endpoint: kse.basics.FromLengthIdx): Array[Float] =
    java.util.Arrays.copyOf(af, FromLengthIdx.asEndpointOf(endpoint)(af))

  inline def copyOfRange(i0: Int, iN: Int): Array[Float] =
    java.util.Arrays.copyOfRange(af, i0, iN)
  inline def copyOfRange(v: Iv | PIv): Array[Float] =
    val iv = Iv.of(v, af)
    java.util.Arrays.copyOfRange(af, iv.i0, iv.iN)
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Float] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(af, iv.i0, iv.iN)

  inline def search(x: Float): Int =
    java.util.Arrays.binarySearch(af, x)
  inline def searchRange(i0: Int, iN: Int)(x: Float): Int =
    java.util.Arrays.binarySearch(af, i0, iN, x)
  inline def searchRange(v: Iv | PIv)(x: Float): Int =
    val iv = Iv.of(v, af)
    java.util.Arrays.binarySearch(af, iv.i0, iv.iN, x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Float): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(af, iv.i0, iv.iN, x)


  inline def fill(x: Float): af.type =
    java.util.Arrays.fill(af, x)
    af
  inline def fillRange(i0: Int, iN: Int)(x: Float): af.type = 
    java.util.Arrays.fill(af, i0, iN, x)
    af
  inline def fillRange(v: Iv | PIv)(x: Float): af.type =
    val iv = Iv.of(v, af)
    java.util.Arrays.fill(af, iv.i0, iv.iN, x)
    af
  inline def fillRange(inline rg: collection.immutable.Range)(x: Float): af.type =
    val iv = Iv of rg
    java.util.Arrays.fill(af, iv.i0, iv.iN, x)
    af

  inline def sort(): af.type =
    java.util.Arrays.sort(af)
    af
  inline def sortRange(i0: Int, iN: Int): af.type =
    java.util.Arrays.sort(af, i0, iN)
    af
  inline def sortRange(v: Iv | PIv): af.type =
    val iv = Iv.of(v, af)
    java.util.Arrays.sort(af, iv.i0, iv.iN)
    af
  inline def sortRange(inline rg: collection.immutable.Range): af.type =
    val iv = Iv of rg
    java.util.Arrays.sort(af, iv.i0, iv.iN)
    af

  inline def isSorted: Boolean =
    isSortedRange(0, af.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && af(i-1) <= af(i) do i += 1
      i >= iN
  inline def isSortedRange(v: Iv | PIv): Boolean =
    val iv = Iv.of(v, af)
    isSortedRange(iv.i0, iv.iN)
  inline def isSortedRange(inline rg: collection.immutable.Range): Boolean =
    val iv = Iv of rg
    isSortedRange(iv.i0, iv.iN)
}

/** Double Array specific functionality from java.util.Arrays and java.lang.System */
extension (ad: Array[Double]) {
  inline def unpackBytes: Array[Byte] = ArrayReform.toBytes(ad)

  inline def shrinkCopy(size: Int): Array[Double] = if size < ad.length then java.util.Arrays.copyOf(ad, size) else ad

  inline def copyToSize(size: Int): Array[Double] =
    java.util.Arrays.copyOf(ad, size)
  @targetName("copyToEndpointSize")
  inline def copyToSize(endpoint: kse.basics.FromLengthIdx): Array[Double] =
    java.util.Arrays.copyOf(ad, FromLengthIdx.asEndpointOf(endpoint)(ad))

  inline def copyOfRange(i0: Int, iN: Int): Array[Double] =
    java.util.Arrays.copyOfRange(ad, i0, iN)
  inline def copyOfRange(v: Iv | PIv): Array[Double] =
    val iv = Iv.of(v, ad)
    java.util.Arrays.copyOfRange(ad, iv.i0, iv.iN)
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Double] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(ad, iv.i0, iv.iN)

  inline def search(x: Double): Int =
    java.util.Arrays.binarySearch(ad, x)
  inline def searchRange(i0: Int, iN: Int)(x: Double): Int =
    java.util.Arrays.binarySearch(ad, i0, iN, x)
  inline def searchRange(v: Iv | PIv)(x: Double): Int =
    val iv = Iv.of(v, ad)
    java.util.Arrays.binarySearch(ad, iv.i0, iv.iN, x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Double): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(ad, iv.i0, iv.iN, x)


  inline def fill(x: Double): ad.type =
    java.util.Arrays.fill(ad, x)
    ad
  inline def fillRange(i0: Int, iN: Int)(x: Double): ad.type = 
    java.util.Arrays.fill(ad, i0, iN, x)
    ad
  inline def fillRange(v: Iv | PIv)(x: Double): ad.type =
    val iv = Iv.of(v, ad)
    java.util.Arrays.fill(ad, iv.i0, iv.iN, x)
    ad
  inline def fillRange(inline rg: collection.immutable.Range)(x: Double): ad.type =
    val iv = Iv of rg
    java.util.Arrays.fill(ad, iv.i0, iv.iN, x)
    ad

  inline def sort(): ad.type =
    java.util.Arrays.sort(ad)
    ad
  inline def sortRange(i0: Int, iN: Int): ad.type =
    java.util.Arrays.sort(ad, i0, iN)
    ad
  inline def sortRange(v: Iv | PIv): ad.type =
    val iv = Iv.of(v, ad)
    java.util.Arrays.sort(ad, iv.i0, iv.iN)
    ad
  inline def sortRange(inline rg: collection.immutable.Range): ad.type =
    val iv = Iv of rg
    java.util.Arrays.sort(ad, iv.i0, iv.iN)
    ad

  inline def isSorted: Boolean =
    isSortedRange(0, ad.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ad(i-1) <= ad(i) do i += 1
      i >= iN
  inline def isSortedRange(v: Iv | PIv): Boolean =
    val iv = Iv.of(v, ad)
    isSortedRange(iv.i0, iv.iN)
  inline def isSortedRange(inline rg: collection.immutable.Range): Boolean =
    val iv = Iv of rg
    isSortedRange(iv.i0, iv.iN)
}

/** Object Array specific functionality from java.util.Arrays and java.lang.System */
extension [A >: Null <: AnyRef](aa: Array[A]) {
  inline def shrinkCopy(size: Int): Array[A] = if size < aa.length then java.util.Arrays.copyOf(aa, size) else aa

  inline def copyToSize(size: Int): Array[A] =
    java.util.Arrays.copyOf(aa, size)
  @targetName("copyToEndpointSize")
  inline def copyToSize(endpoint: kse.basics.FromLengthIdx): Array[A] =
    java.util.Arrays.copyOf(aa, FromLengthIdx.asEndpointOf(endpoint)(aa))

  inline def copyOfRange(i0: Int, iN: Int): Array[A] =
    java.util.Arrays.copyOfRange(aa, i0, iN)
  inline def copyOfRange(v: Iv | PIv): Array[A] =
    val iv = Iv.of(v, aa)
    java.util.Arrays.copyOfRange(aa, iv.i0, iv.iN)
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[A] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(aa, iv.i0, iv.iN)

  inline def search(x: A)(using o: scala.math.Ordering[A]): Int =
    java.util.Arrays.binarySearch(aa, x, o)
  inline def searchRange(i0: Int, iN: Int)(x: A)(using o: scala.math.Ordering[A]): Int =
    java.util.Arrays.binarySearch(aa, i0, iN, x, o)
  inline def searchRange(v: Iv | PIv)(x: A)(using o: scala.math.Ordering[A]): Int =
    val iv = Iv.of(v, aa)
    java.util.Arrays.binarySearch(aa, iv.i0, iv.iN, x, o)
  inline def searchRange(inline rg: collection.immutable.Range)(x: A)(using o: scala.math.Ordering[A]): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(aa, iv.i0, iv.iN, x, o)


  inline def fill(x: A): aa.type =
    java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], x.asInstanceOf[AnyRef])
    aa
  inline def fillRange(i0: Int, iN: Int)(x: A): aa.type = 
    java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], i0, iN, x.asInstanceOf[AnyRef])
    aa
  inline def fillRange(v: Iv | PIv)(x: A): aa.type =
    val iv = Iv.of(v, aa)
    java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], iv.i0, iv.iN, x.asInstanceOf[AnyRef])
    aa
  inline def fillRange(inline rg: collection.immutable.Range)(x: A): aa.type =
    val iv = Iv of rg
    java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], iv.i0, iv.iN, x.asInstanceOf[AnyRef])
    aa

  inline def sort()(using o: scala.math.Ordering[A]): aa.type =
    scala.util.Sorting.stableSort(aa)
    aa
  inline def sortRange(i0: Int, iN: Int)(using o: scala.math.Ordering[A]): aa.type =
    scala.util.Sorting.stableSort(aa, i0, iN)
    aa
  inline def sortRange(v: Iv | PIv)(using o: scala.math.Ordering[A]): aa.type =
    val iv = Iv.of(v, aa)
    scala.util.Sorting.stableSort(aa, iv.i0, iv.iN)
    aa
  inline def sortRange(inline rg: collection.immutable.Range)(using o: scala.math.Ordering[A]): aa.type =
    val iv = Iv of rg
    scala.util.Sorting.stableSort(aa, iv.i0, iv.iN)
    aa

  inline def isSorted(using o: scala.math.Ordering[A]): Boolean =
    isSortedRange(0, aa.length)
  def isSortedRange(i0: Int, iN: Int)(using o: scala.math.Ordering[A]): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && o.compare(aa(i-1), aa(i)) <= 0 do i += 1
      i >= iN
  inline def isSortedRange(v: Iv | PIv)(using o: scala.math.Ordering[A]): Boolean =
    val iv = Iv.of(v, aa)
    isSortedRange(iv.i0, iv.iN)
  inline def isSortedRange(inline rg: collection.immutable.Range)(using o: scala.math.Ordering[A]): Boolean =
    val iv = Iv of rg
    isSortedRange(iv.i0, iv.iN)
}



/** Higher-level high-speed String access (inlined) */
extension (a: String) {
  inline def apply(i: Int) = a.charAt(i)

  @targetName("applyFromLengthIdx")
  inline def apply(i: kse.basics.FromLengthIdx): Char = a.charAt(i of a)

  inline def apply(e: End.type): Char = a.charAt(a.length - 1)

  inline def arr: Array[Char] = a.toCharArray

  inline def builder(): java.lang.StringBuilder =
    if a.length == 0 then new java.lang.StringBuilder() else new java.lang.StringBuilder(a)

  inline def build(inline f: java.lang.StringBuilder => Unit): String =
    val b = new java.lang.StringBuilder(a)
    f(b)
    b.toString

  inline def clip: kse.basics.ClippedString = ClippedString wrap a

  inline def breakable: kse.basics.ShortcutString = ShortcutString wrap a

  inline def peek()(inline f: Char => Unit): a.type =
    var i = 0
    while i < a.length do
      f(a.charAt(i))
      i += 1
    a
  inline def peek(i0: Int, iN: Int)(inline f: Char => Unit): a.type =
    var i = i0
    while i < iN do
      f(a.charAt(i))
      i += 1
    a
  inline def peek(v: Iv | PIv)(inline f: Char => Unit): a.type =
    val iv = Iv.of(v, a)
    peek(iv.i0, iv.iN)(f)
  inline def peek(inline rg: collection.immutable.Range)(inline f: Char => Unit): a.type =
    val iv = Iv of rg
    peek(iv.i0, iv.iN)(f)
  inline def peek(indices: Array[Int])(inline f: Char => Unit): a.type =
    var i = 0
    while i < indices.length do
      f(a.charAt(indices(i)))
      i += 1
    a
  inline def peek(indices: scala.collection.IntStepper)(inline f: Char => Unit): a.type =
    while indices.hasStep do
      f(a.charAt(indices.nextStep))
    a

  inline def visit()(inline f: (Char, Int) => Unit): Unit =
    var i = 0
    while i < a.length do
      f(a.charAt(i), i)
      i += 1
  inline def visit(i0: Int, iN: Int)(inline f: (Char, Int) => Unit): Unit =
    var i = i0
    while i < iN do
      f(a.charAt(i), i)
      i += 1
  inline def visit(v: Iv | PIv)(inline f: (Char, Int) => Unit): Unit =
    val iv = Iv.of(v, a)
    visit(iv.i0, iv.iN)(f)
  inline def visit(inline rg: collection.immutable.Range)(inline f: (Char, Int) => Unit): Unit =
    val iv = Iv of rg
    visit(iv.i0, iv.iN)(f)
  inline def visit(indices: Array[Int])(inline f: (Char, Int) => Unit): Unit =
    var i = 0
    while i < indices.length do
      val j = indices(i)
      f(a.charAt(j), j)
      i += 1
  inline def visit(indices: scala.collection.IntStepper)(inline f: (Char, Int) => Unit): Unit =
    while indices.hasStep do
      val j = indices.nextStep
      f(a.charAt(j), j)

  inline def wander()(inline f: (Char, Int) => Int): Int =
    wander(0)(f)
  inline def wander(start: Int)(inline f: (Char, Int) => Int): Int =
    var n = 0
    var i = start
    while i >= 0 && i < a.length && n < Int.MaxValue do
      n += 1
      i = f(a.charAt(i), i)
    n

  inline def gather[Z](zero: Z)()(inline f: (Z, Char, Int) => Z) =
    var i = 0
    var z = zero
    while i < a.length do
      z = f(z, a.charAt(i), i)
      i += 1
    z
  inline def gather[Z](zero: Z)(i0: Int, iN: Int)(inline f: (Z, Char, Int) => Z): Z =
    var i = i0
    var z = zero
    while i < iN do
      z = f(z, a.charAt(i), i)
      i += 1
    z
  inline def gather[Z](zero: Z)(v: Iv | PIv)(inline f: (Z, Char, Int) => Z): Z =
    val iv = Iv.of(v, a)
    gather(zero)(iv.i0, iv.iN)(f)
  inline def gather[Z](zero: Z)(inline rg: collection.immutable.Range)(inline f: (Z, Char, Int) => Z): Z =
    val iv = Iv of rg
    gather(zero)(iv.i0, iv.iN)(f)
  inline def gather[Z](zero: Z)(indices: Array[Int])(inline f: (Z, Char, Int) => Z): Z =
    var i = 0
    var z = zero
    while i < indices.length do
      val j = indices(i)
      z = f(z, a.charAt(j), j)
      i += 1
    z
  inline def gather[Z](zero: Z)(indices: scala.collection.IntStepper)(inline f: (Z, Char, Int) => Z): Z =
    var z = zero
    while indices.hasStep do
      val j = indices.nextStep
      z = f(z, a.charAt(j), j)
    z

  inline def copyWith(inline f: Char => Char): String =
    val b = new java.lang.StringBuilder
    var i = 0
    while i < a.length do
      b append f(a.charAt(i))
      i += 1
    b.toString
  transparent inline def copyOp[B](inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](a.length)
    var i = 0
    while i < a.length do
      b(i) = op(a.charAt(i), i)
      i += 1
    b

  inline def where(inline pick: Char => Boolean): Array[Int] =
    var ix = new Array[Int](if a.length <= 8 then a.length else 8)
    var i = 0
    var j = 0
    while i < a.length do
      if pick(a.charAt(i)) then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = i
        j += 1
      i += 1
    ix.shrinkTo(j)

  inline def whereIn(i0: Int, iN: Int)(inline pick: Char => Boolean): Array[Int] =
    var ix = new Array[Int](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else iN - i0)
    var i = i0
    var j = 0
    while i < iN do
      if pick(a.charAt(i)) then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = i
        j += 1
      i += 1
    ix.shrinkTo(j)
  inline def whereIn(v: Iv | PIv)(inline pick: Char => Boolean): Array[Int] =
    val iv = Iv.of(v, a)
    whereIn(iv.i0, iv.iN)(pick)
  inline def whereIn(inline rg: Range)(inline pick: Char => Boolean): Array[Int] =
    val iv = Iv of rg
    whereIn(iv.i0, iv.iN)(pick)

  inline def whereFrom(indices: Array[Int])(inline pick: Char => Boolean): Array[Int] =
    var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
    var i = 0
    var j = 0
    while i < indices.length do
      val k = indices(i)
      if pick(a.charAt(k)) then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = k
        j += 1
      i += 1  
    ix.shrinkTo(j)  

  inline def inject(that: Array[Char]): Int =
    inject(that, 0)(0, a.length)
  inline def inject(that: Array[Char], where: Int): Int =
    inject(that, where)(0, a.length)
  inline def inject(that: Array[Char])(i0: Int, iN: Int): Int =
    inject(that, 0)(i0, iN)
  inline def inject(that: Array[Char], where: Int)(i0: Int, iN: Int): Int =
    var i = i0
    var j = where
    while i < iN do
      that(j) = a.charAt(i)
      i += 1
      j += 1
    iN - i0
  inline def inject(that: Array[Char])(v: Iv | PIv): Int =
    val iv = Iv.of(v, a)
    inject(that, 0)(iv.i0, iv.iN)
  inline def inject(that: Array[Char], where: Int)(v: Iv | PIv): Int =
    val iv = Iv.of(v, a)
    inject(that, where)(iv.i0, iv.iN)
  inline def inject(that: Array[Char])(inline rg: collection.immutable.Range): Int =
    val iv = Iv of rg
    inject(that, 0)(iv.i0, iv.iN)
  inline def inject(that: Array[Char], where: Int)(inline rg: collection.immutable.Range): Int =
    val iv = Iv of rg
    inject(that, where)(iv.i0, iv.iN)
  inline def inject(that: Array[Char])(indices: Array[Int]): Int =
    inject(that, 0)(indices)
  inline def inject(that: Array[Char], where: Int)(indices: Array[Int]): Int =
    var i = 0
    var j = where
    while i < indices.length do
      that(j) = a.charAt(indices(i))
      i += 1
      j += 1
    i
  inline def inject(that: Array[Char])(indices: scala.collection.IntStepper): Int =
    inject(that, 0)(indices)
  inline def inject(that: Array[Char], where: Int)(indices: scala.collection.IntStepper): Int =
    var j = where
    while indices.hasStep do
      that(j) = a.charAt(indices.nextStep)
      j += 1
    j - where
  inline def inject(that: Array[Char])(inline pick: Char => Boolean): Int =
    inject(that, 0)(pick)
  inline def inject(that: Array[Char], where: Int)(inline pick: Char => Boolean): Int =
    var i = 0
    var j = where
    while i < a.length do
      val x = a.charAt(i)
      if pick(x) then
        that(j) = x
        j += 1 
      i += 1
    j - where

  inline def injectOp[B](that: Array[B])()(inline f: (Char, Int) => B): Int =
    injectOp(that, 0)(0, a.length)(f)
  inline def injectOp[B](that: Array[B], where: Int)()(inline f: (Char, Int) => B): Int =
    injectOp(that, where)(0, a.length)(f)
  inline def injectOp[B](that: Array[B])(i0: Int, iN: Int)(inline f: (Char, Int) => B): Int =
    injectOp(that, 0)(i0, iN)(f)
  inline def injectOp[B](that: Array[B], where: Int)(i0: Int, iN: Int)(inline f: (Char, Int) => B): Int =
    var i = i0
    var j = where
    while i < iN do
      that(j) = f(a.charAt(i), i)
      j += 1
      i += 1
    iN - i0
  inline def injectOp[B](that: Array[B])(v: Iv | PIv)(inline f: (Char, Int) => B): Int =
    val iv = Iv.of(v, a)
    injectOp[B](that, 0)(iv.i0, iv.iN)(f)
  inline def injectOp[B](that: Array[B], where: Int)(v: Iv | PIv)(inline f: (Char, Int) => B): Int =
    val iv = Iv.of(v, a)
    injectOp[B](that, where)(iv.i0, iv.iN)(f)
  inline def injectOp[B](that: Array[B])(inline rg: collection.immutable.Range)(inline f: (Char, Int) => B): Int =
    val iv = Iv of rg
    injectOp[B](that, 0)(iv.i0, iv.iN)(f)
  inline def injectOp[B](that: Array[B], where: Int)(inline rg: collection.immutable.Range)(inline f: (Char, Int) => B): Int =
    val iv = Iv of rg
    injectOp[B](that, where)(iv.i0, iv.iN)(f)
  inline def injectOp[B](that: Array[B])(indices: Array[Int])(inline f: (Char, Int) => B): Int =
    injectOp[B](that, 0)(indices)(f)
  inline def injectOp[B](that: Array[B], where: Int)(indices: Array[Int])(inline f: (Char, Int) => B): Int =
    var i = 0
    var j = where
    while i < indices.length do
      val k = indices(i)
      that(j) = f(a.charAt(k), k)
      i += 1
      j += 1
    i
  inline def injectOp[B](that: Array[B])(indices: scala.collection.IntStepper)(inline f: (Char, Int) => B): Int =
    injectOp[B](that, 0)(indices)(f)
  inline def injectOp[B](that: Array[B], where: Int)(indices: scala.collection.IntStepper)(inline f: (Char, Int) => B): Int =
    var j = where
    while indices.hasStep do
      val i = indices.nextStep
      that(j) = f(a.charAt(i), i)
      j += 1
    j - where
  inline def injectOp[B](that: Array[B])(inline pick: Char => Boolean)(inline f: (Char, Int) => B): Int =
    injectOp[B](that, 0)(pick)(f)
  inline def injectOp[B](that: Array[B], where: Int)(inline pick: Char => Boolean)(inline f: (Char, Int) => B): Int =
    var i = 0
    var j = where
    while i < a.length do
      val x = a.charAt(i)
      if pick(x) then
        that(j) = f(x, i)
        j += 1 
      i += 1
    j - where

  inline def select(i0: Int, iN: Int): String =
    a.substring(i0, iN)
  inline def select(v: Iv | PIv): String =
    val iv = Iv.of(v, a)
    select(Iv.i0(iv), Iv.iN(iv))
  inline def select(inline rg: collection.immutable.Range): String =
    select(Iv of rg)
  inline def select(indices: Array[Int]): String =
    val b = new java.lang.StringBuilder(indices.length)
    var i = 0
    while i < indices.length do
      b append a.charAt(indices(i))
      i += 1
    b.toString
  inline def select(indices: scala.collection.IntStepper): String =
    var b = new java.lang.StringBuilder()
    var j = 0
    while indices.hasStep do
      b append a.charAt(indices.nextStep)
      j += 1
    b.toString
  inline def select(inline pick: Char => Boolean): String =
    var b = new java.lang.StringBuilder()
    var i = 0
    while i < a.length do
      val x = a.charAt(i)
      if pick(x) then
        b append x
      i += 1
    b.toString

  transparent inline def selectOp[B](i0: Int, iN: Int)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](iN - i0)
    var i = i0
    while i < iN do
      b(i - i0) = op(a.charAt(i), i)
      i += 1
    b
  transparent inline def selectOp[B](v: Iv | PIv)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    val iv = Iv.of(v, a)
    selectOp(Iv.i0(iv), Iv.iN(iv))(op)
  transparent inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    val iv = Iv of rg
    selectOp(iv.i0, iv.iN)(op)
  transparent inline def selectOp[B](indices: Array[Int])(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](indices.length)
    var i = 0
    while i < indices.length do
      val j = indices(i)
      b(i) = op(a.charAt(j), j)
      i += 1
    b
  transparent inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    var b = new Array[B](if a.length <= 8 then a.length else 8)
    var j = 0
    while indices.hasStep do
      if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
      val i = indices.nextStep
      b(j) = op(a.charAt(i), i)
      j += 1
    b.shrinkTo(j)
  transparent inline def selectOp[B](inline pick: Char => Boolean)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    val indices = where(pick)
    selectOp(indices)(op)

  transparent inline def fuse[B](inline add: (Char, Int, B => Unit) => Unit)(using ClassTag[B]): Array[B] =
    var bs = new Array[B](if a.length < 8 then a.length else 8)
    var i = 0
    var j = 0
    while i < a.length do
      add(a.charAt(i), i, b => { if j >= bs.length then bs = bs.enlargeTo(bs.length | (bs.length << 1)); bs(j) = b; j += 1 })
      i += 1
    bs.shrinkTo(j)

  inline def diced(indices: Array[Int], mode: "" | "()" | "[)" | "(]" | "[]", endpoints: "endpoints" | "no endpoints"): Array[String] =
    val empty = inline endpoints match
      case "no endpoints" => indices.length < 2
      case _ => indices.length == 0 && a.length == 0
    if empty then
      inline if endpoints == "endpoints" && mode != "" then
        Array("")
      else new Array[String](0)
    else
      var bss = new Array[String](inline if endpoints == "no endpoints" then indices.length-1 else indices.length+1)
      var i0 = inline if endpoints == "no endpoints" then indices(0) else inline mode match
        case "[)" | "[]" => 0
        case _           => -1
      var j = 0
      var k = 0
      while j < bss.length do
        val iN =
          inline if endpoints == "no endpoints" then
            indices(j+1)
          else
            if j < indices.length then indices(j)
            else inline mode match
              case "(]" | "[]" => a.length - 1
              case _           => a.length
        j += 1
        if i0 == iN then
          inline mode match
            case ""   =>
            case "[]" =>
              bss(k) = a.charAt(i0).toString
              k += 1
            case _    =>
              bss(k) = ""
              k += 1
        else
          val bump = if iN < i0 then -1 else 1
          var h0 = inline if mode != "[)" && mode != "[]" then i0 + bump else i0
          var hN = inline if mode == "(]" || mode == "[]" then iN + bump else iN
          val n = if hN < h0 then h0 - hN else hN - h0
          if n == 0 then
            inline mode match
              case "" =>
              case _  =>
                bss(k) = ""
                k += 1
          else
            val b = new Array[Char](n)
            if h0 > hN then
              var h = h0
              var g = 0
              while h != hN do
                b(g) = a.charAt(h)
                h -= 1
                g += 1
              bss(k) = new String(b)
            else if n > 0 then bss(k) = a.substring(h0, hN)
            else bss(k) = ""
            k += 1
          i0 = iN
      inline if mode == "" then
        if k < bss.length then
          java.util.Arrays.copyOf(bss, k)
        else
          bss
      else
        bss
  inline def diced(indices: Array[Int], style: "()" | "[)" | "(]" | "[]" | "no endpoints"): Array[String] =
    inline style match
      case "no endpoints"                 => diced(indices, "", "no endpoints")
      case s: ("()" | "(]" | "[)" | "[]") => diced(indices, s, "endpoints")
  inline def diced(indices: Array[Int]): Array[String] = diced(indices, "", "endpoints")
}


opaque type ClippedString = String
object ClippedString {
  inline def wrap(a: String): ClippedString = a

  extension (ca: ClippedString)
    inline def unwrap: String = ca

  extension (ca: kse.basics.ClippedString) {
    inline def breakable: kse.basics.ShortClipString = ShortClipString wrap ca.unwrap

    inline def peek(i0: Int, iN: Int)(inline f: Char => Unit): String =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      while i < iM do
        f(a.charAt(i))
        i += 1
      a
    inline def peek(v: Iv | PIv)(inline f: Char => Unit): String =
      val iv = Iv.of(v, ca.unwrap)
      peek(iv.i0, iv.iN)(f)
    inline def peek(inline rg: collection.immutable.Range)(inline f: Char => Unit): String =
      val iv = Iv of rg
      peek(iv.i0, iv.iN)(f)
    inline def peek(indices: Array[Int])(inline f: Char => Unit): String =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then f(a.charAt(j))
        i += 1
      a
    inline def peek(indices: scala.collection.IntStepper)(inline f: Char => Unit): String =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then f(a.charAt(j))
      a

    inline def visit(i0: Int, iN: Int)(inline f: (Char, Int) => Unit): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      while i < iM do
        f(a.charAt(i), i)
        i += 1
    inline def visit(v: Iv | PIv)(inline f: (Char, Int) => Unit): Unit =
      val iv = Iv.of(v, ca.unwrap)
      visit(iv.i0, iv.iN)(f)
    inline def visit(inline rg: collection.immutable.Range)(inline f: (Char, Int) => Unit): Unit =
      val iv = Iv of rg
      visit(iv.i0, iv.iN)(f)
    inline def visit(indices: Array[Int])(inline f: (Char, Int) => Unit): Unit =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then f(a.charAt(j), j)
        i += 1
    inline def visit(indices: scala.collection.IntStepper)(inline f: (Char, Int) => Unit): Unit =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then f(a.charAt(j), j)

    inline def gather[Z](zero: Z)(i0: Int, iN: Int)(inline f: (Z, Char, Int) => Z): Z =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var z = zero
      while i < iM do
        z = f(z, a.charAt(i), i)
        i += 1
      z
    inline def gather[Z](zero: Z)(v: Iv | PIv)(inline f: (Z, Char, Int) => Z): Z =
      val iv = Iv.of(v, ca.unwrap)
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(inline rg: collection.immutable.Range)(inline f: (Z, Char, Int) => Z): Z =
      val iv = Iv of rg
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(indices: Array[Int])(inline f: (Z, Char, Int) => Z): Z =
      val a = ca.unwrap
      var i = 0
      var z = zero
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then z = f(z, a.charAt(j), j)
        i += 1
      z
    inline def gather[Z](zero: Z)(indices: scala.collection.IntStepper)(inline f: (Z, Char, Int) => Z): Z =
      val a = ca.unwrap
      var z = zero
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then z = f(z, a.charAt(j), j)
      z

    inline def whereIn(i0: Int, iN: Int)(inline pick: Char => Boolean): Array[Int] =
      val a = ca.unwrap
      var i = i0
      if i0 < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var ix = new Array[Int](if iM - i < 0 then 0 else if iM - i > 8 then 8 else iM - i)
      var j = 0
      while i < iM do
        if pick(a.charAt(i)) then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = i
          j += 1
        i += 1
      ix.shrinkTo(j)
    inline def whereIn(v: Iv | PIv)(inline pick: Char => Boolean): Array[Int] =
      val iv = Iv.of(v, ca.unwrap)
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: Char => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)

    inline def whereFrom(indices: Array[Int])(inline pick: Char => Boolean): Array[Int] =
      val a = ca.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        if k >= 0 && k < a.length && pick(a.charAt(k)) then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = k
          j += 1
        i += 1  
      ix.shrinkTo(j)

    inline def inject(that: Array[Char]): Int =
      inject(that, 0)(0, ca.unwrap.length)
    inline def inject(that: Array[Char], where: Int): Int =
      inject(that, where)(0, ca.unwrap.length)
    inline def inject(that: Array[Char])(i0: Int, iN: Int): Int =
      inject(that, 0)(i0, iN)
    inline def inject(that: Array[Char], where: Int)(i0: Int, iN: Int): Int =
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
        val n0 = n
        while n > 0 do
          that(w) = a.charAt(i)
          i += 1
          w += 1
          n -= 1
        n0
      else 0
    inline def inject(that: Array[Char])(v: Iv | PIv): Int =
      val iv = Iv.of(v, ca.unwrap)
      inject(that, 0)(iv.i0, iv.iN)
    inline def inject(that: Array[Char], where: Int)(v: Iv | PIv): Int =
      val iv = Iv.of(v, ca.unwrap)
      inject(that, where)(iv.i0, iv.iN)
    inline def inject(that: Array[Char])(inline rg: collection.immutable.Range): Int =
      val iv = Iv of rg
      inject(that, 0)(iv.i0, iv.iN)
    inline def inject(that: Array[Char], where: Int)(inline rg: collection.immutable.Range): Int =
      val iv = Iv of rg
      inject(that, where)(iv.i0, iv.iN)
    inline def inject(that: Array[Char])(indices: Array[Int]): Int =
      inject(that, 0)(indices)
    inline def inject(that: Array[Char], where: Int)(indices: Array[Int]): Int =
      val a = ca.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      while i < indices.length && j < that.length do
        val k = indices(i)
        if k >= 0 && k < a.length then
          that(j) = a.charAt(k)
          j += 1
        i += 1
      if where < 0 then j else j - where
    inline def inject(that: Array[Char])(indices: scala.collection.IntStepper): Int =
      inject(that, 0)(indices)
    inline def inject(that: Array[Char], where: Int)(indices: scala.collection.IntStepper): Int =
      val a = ca.unwrap
      var j = where
      if j < 0 then j = 0
      while indices.hasStep && j < that.length do
        val i = indices.nextStep
        if i >= 0 && i < a.length then
          that(j) = a.charAt(i)
          j += 1
      if where < 0 then j else j - where
    inline def inject(that: Array[Char])(inline pick: Char => Boolean): Int =
      inject(that, 0)(pick)
    inline def inject(that: Array[Char], where: Int)(inline pick: Char => Boolean): Int =
      val a = ca.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      while i < a.length && j < that.length do
        val x = a.charAt(i)
        if pick(x) then
          that(j) = x
          j += 1 
        i += 1
      if where < 0 then j else j - where

    inline def injectOp[B](that: Array[B])()(inline f: (Char, Int) => B): Int =
      injectOp[B](that, 0)(0, ca.unwrap.length)(f)
    inline def injectOp[B](that: Array[B], where: Int)()(inline f: (Char, Int) => B): Int =
      injectOp[B](that, where)(0, ca.unwrap.length)(f)
    inline def injectOp[B](that: Array[B])(i0: Int, iN: Int)(inline f: (Char, Int) => B): Int =
      injectOp[B](that, 0)(i0, iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(i0: Int, iN: Int)(inline f: (Char, Int) => B): Int =
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
        val n0 = n
        while n > 0 do
          that(w) = f(a.charAt(i), i)
          w += 1
          i += 1
          n -= 1
        n0
      else 0
    inline def injectOp[B](that: Array[B])(v: Iv | PIv)(inline f: (Char, Int) => B): Int =
      val iv = Iv.of(v, ca.unwrap)
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(v: Iv | PIv)(inline f: (Char, Int) => B): Int =
      val iv = Iv.of(v, ca.unwrap)
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(inline rg: collection.immutable.Range)(inline f: (Char, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(inline rg: collection.immutable.Range)(inline f: (Char, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(indices: Array[Int])(inline f: (Char, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: Array[Int])(inline f: (Char, Int) => B): Int =
      val a = ca.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      while i < indices.length && j < that.length do
        val k = indices(i)
        if k >= 0 && k < a.length then
          that(j) = f(a.charAt(k), k)
          j += 1
        i += 1
      if where < 0 then j else j - where
    inline def injectOp[B](that: Array[B])(indices: scala.collection.IntStepper)(inline f: (Char, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: scala.collection.IntStepper)(inline f: (Char, Int) => B): Int =
      val a = ca.unwrap
      var j = where
      if j < 0 then j = 0
      while indices.hasStep && j < that.length do
        val i = indices.nextStep
        if i >= 0 && i < a.length then
          that(j) = f(a.charAt(i), i)
          j += 1
      if where < 0 then j else j - where
    inline def injectOp[B](that: Array[B])(inline pick: Char => Boolean)(inline f: (Char, Int) => B): Int =
      injectOp[B](that, 0)(pick)(f)
    inline def injectOp[B](that: Array[B], where: Int)(inline pick: Char => Boolean)(inline f: (Char, Int) => B): Int =
      val a = ca.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      while i < a.length && j < that.length do
        val x = a.charAt(i)
        if pick(x) then
          that(j) = f(x, i)
          j += 1 
        i += 1
      if where < 0 then j else j - where

    inline def select(i0: Int, iN: Int): String =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j >= a.length then j = a.length
      if i < j then a.substring(i, j) else ""
    inline def select(v: Iv | PIv): String =
      val iv = Iv.of(v, ca.unwrap)
      select(iv.i0, iv.iN)
    inline def select(inline rg: collection.immutable.Range): String =
      select(Iv of rg)
    inline def select(indices: Array[Int]): String =
      val a = ca.unwrap
      val b = new java.lang.StringBuilder(indices.length)
      var i = 0
      while i < indices.length do
        val k = indices(i)
        if k >= 0 && k < a.length then
          b append a.charAt(k)
        i += 1
      b.toString
    inline def select(indices: scala.collection.IntStepper): String =
      val a = ca.unwrap
      var b = new java.lang.StringBuilder()
      while indices.hasStep do
        val i = indices.nextStep
        if i >= 0 && i < a.length then
          b append a.charAt(i)
      b.toString

    transparent inline def selectOp[B](i0: Int, iN: Int)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j > a.length then j = a.length
      if j < i then j = i
      val b = new Array[B](j - i)
      val offset = i
      while i < j do
        b(i - offset) = op(a.charAt(i), i)
        i += 1
      b
    transparent inline def selectOp[B](v: Iv | PIv)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv.of(v, ca.unwrap)
      selectOp(Iv.i0(iv), Iv.iN(iv))(op)
    transparent inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    transparent inline def selectOp[B](indices: Array[Int])(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val a = ca.unwrap
      val b = new Array[B](indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        if k >= 0 && k < a.length then
          b(j) = op(a.charAt(k), k)
          j += 1
        i += 1
      b.shrinkTo(j)
    transparent inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val a = ca.unwrap
      var b = new Array[B](if a.length <= 8 then a.length else 8)
      var j = 0
      while indices.hasStep do
        val i = indices.nextStep
        if i >= 0 && i < a.length then
          if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
          b(j) = op(a.charAt(i), i)
          j += 1
      b.shrinkTo(j)

    inline def diced(indices: Array[Int], mode: "" | "()" | "[)" | "(]" | "[]", endpoints: "endpoints" | "no endpoints"): Array[String] =
      val a = ca.unwrap
      val empty = inline endpoints match
        case "no endpoints" => indices.length < 2
        case _ => indices.length == 0 && a.length == 0
      if empty then
        inline if endpoints == "endpoints" && mode != "" then
          Array("")
        else new Array[String](0)
      else
        var bss = new Array[String](inline if endpoints == "no endpoints" then indices.length-1 else indices.length+1)
        var i0 = 
          inline if endpoints == "no endpoints" then indices(0)
          else -1
        var j = 0
        var k = 0
        while j < bss.length do
          val iN =
            inline if endpoints == "no endpoints" then indices(j+1)
            else
              if j < indices.length then indices(j)
              else a.length+1
          j += 1
          if !((i0 < 0 && iN < 0) || (i0 >= a.length && iN >= a.length)) then
            if i0 == iN then
              inline mode match
                case ""   =>
                case "[]" =>
                  if i0 >= 0 && i0 < a.length then
                    bss(k) = a.charAt(i0).toString
                    k += 1
                case _    =>
                  if i0 >= 0 && i0 < a.length then
                    bss(k) = ""
                    k += 1
            else
              var h0 = i0
              var hN = iN
              inline mode match
                case "[]" =>
                  if hN < h0 then
                    if h0 >= a.length then h0 = a.length - 1
                    hN = if hN >= 0 then hN - 1 else - 1
                  else
                    if h0 < 0 then h0 = 0
                    hN = if hN < a.length then hN + 1 else a.length
                case "[)" =>
                  if hN < h0 then
                    if h0 >= a.length then h0 = a.length - 1
                    if hN < -1 then hN = -1
                  else
                    if h0 < 0 then h0 = 0
                    if hN > a.length then hN = a.length
                case "(]" =>
                  if hN < h0 then
                    h0 = if h0 > a.length then a.length-1 else h0-1
                    hN = if hN > 0 then hN - 1 else -1
                  else
                    h0 = if h0 < 0 then 0 else h0 + 1
                    hN = if hN < a.length then hN + 1 else a.length
                case _ =>
                  if hN < h0 then
                    h0 = if h0 > a.length - 1 then a.length - 1 else h0 - 1
                    if hN < -1 then hN = -1
                  else if hN > h0 then
                    h0 = if h0 < 0 then 0 else h0 + 1
                    if hN > a.length then hN = a.length
              val n = if hN < h0 then h0 - hN else hN - h0
              if n == 0 then
                inline mode match
                  case "" =>
                  case _  =>
                    bss(k) = ""
                    k += 1
              else
                val b = new Array[Char](n)
                if h0 > hN then
                  var h = h0
                  var g = 0
                  while h != hN do
                    b(g) = a.charAt(h)
                    h -= 1
                    g += 1
                  bss(k) = new String(b)
                else if n > 0 then bss(k) = a.substring(h0, hN)
                else bss(k) = ""
                k += 1
          i0 = iN
        if k < bss.length then
          java.util.Arrays.copyOf(bss, k)
        else
          bss
    inline def diced(indices: Array[Int], style: "()" | "[)" | "(]" | "[]" | "no endpoints"): Array[String] =
      inline style match
        case "no endpoints"                 => diced(indices, "", "no endpoints")
        case s: ("()" | "(]" | "[)" | "[]") => diced(indices, s, "endpoints")
    inline def diced(indices: Array[Int]): Array[String] = diced(indices, "", "endpoints")
  }
}


opaque type ShortcutString = String
object ShortcutString {
  inline def wrap(a: String): ShortcutString = a

  extension (sa: ShortcutString)
    inline def unwrap: String = sa

  extension (sa: kse.basics.ShortcutString) {
    inline def clip: kse.basics.ShortClipString = ShortClipString wrap sa.unwrap

    inline def peek()(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          f(a.charAt(i))
          i += 1
      a
    inline def peek(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val a = sa.unwrap
      var i = i0
      shortcut.quittable:
        while i < iN do
          f(a.charAt(i))
          i += 1
      a
    inline def peek(v: Iv | PIv)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val iv = Iv.of(v, sa.unwrap)
      peek(iv.i0, iv.iN)(f)
    inline def peek(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val iv = Iv of rg
      peek(iv.i0, iv.iN)(f)
    inline def peek(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          f(a.charAt(indices(i)))
          i += 1
      a
    inline def peek(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          f(a.charAt(indices.nextStep))
      a

    inline def gather[Z](zero: Z)()(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z) =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = 0
        while i < a.length do
          z = f(z, a.charAt(i), i)
          i += 1
      z
    inline def gather[Z](zero: Z)(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = i0
        while i < iN do
          z = f(z, a.charAt(i), i)
          i += 1
      z
    inline def gather[Z](zero: Z)(v: Iv | PIv)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      val iv = Iv.of(v, sa.unwrap)
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      val iv = Iv of rg
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = 0
        while i < indices.length do
          val j = indices(i)
          z = f(z, a.charAt(j), j)
          i += 1
      z
    inline def gather[Z](zero: Z)(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          z = f(z, a.charAt(j), j)
      z

    inline def copyWith[B](inline f: boundary.Label[shortcut.Type] ?=> Char => Char): String =
      val a = sa.unwrap
      val b = new java.lang.StringBuilder(a.length)
      var i = 0
      shortcut.outer:
        while i < a.length do
          shortcut.inner:
            b append f(a.charAt(i))
          i += 1
      b.toString
    transparent inline def copyOp[B](inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      val b = new Array[B](a.length)
      var i = 0
      var j = 0
      shortcut.outer:
        while i < a.length do
          shortcut.inner:
            b(j) = op(a.charAt(i), i)
            j += 1
          i += 1
      b.shrinkTo(j)

    inline def where(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if a.length <= 8 then a.length else 8)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < a.length do
          if pick(a.charAt(i)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = i
            j += 1
          i += 1
      ix.shrinkTo(j)

    inline def whereIn(i0: Int, iN: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else iN - i0)
      var i = i0
      var j = 0
      shortcut.quittable:
        while i < iN do
          if pick(a.charAt(i)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = i
            j += 1
          i += 1
      ix.shrinkTo(j)
    inline def whereIn(v: Iv | PIv)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val iv = Iv.of(v, sa.unwrap)
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)

    inline def whereFrom(indices: Array[Int])(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < indices.length do
          val k = indices(i)
          if pick(a.charAt(k)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = k
            j += 1
          i += 1  
      ix.shrinkTo(j)

    inline def inject(that: Array[Char])(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Int =
      inject(that, 0)(pick)
    inline def inject(that: Array[Char], where: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Int =
      val a = sa.unwrap
      var i = 0
      var j = where
      shortcut.quittable:
        while i < a.length do
          val x = a.charAt(i)
          if pick(x) then
            that(j) = x
            j += 1 
          i += 1
      j - where

    inline def injectOp[B](that: Array[B])()(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      injectOp(that, 0)(0, sa.unwrap.length)(f)
    inline def injectOp[B](that: Array[B], where: Int)()(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      injectOp(that, where)(0, sa.unwrap.length)(f)
    inline def injectOp[B](that: Array[B])(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      injectOp(that, 0)(i0, iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val a = sa.unwrap
      var i = i0
      var j = where
      shortcut.outer:
        while i < iN do
          shortcut.inner:
            that(j) = f(a.charAt(i), i)
            j += 1
          i += 1
      j - where
    inline def injectOp[B](that: Array[B])(v: Iv | PIv)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = Iv.of(v, sa.unwrap)
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(v: Iv | PIv)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = Iv.of(v, sa.unwrap)
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(indices: Array[Int])(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: Array[Int])(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val a = sa.unwrap
      var i = 0
      var j = where
      shortcut.outer:
        while i < indices.length do
          val k = indices(i)
          shortcut.inner:
            that(j) = f(a.charAt(k), k)
            j += 1
          i += 1
      j - where
    inline def injectOp[B](that: Array[B])(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val a = sa.unwrap
      var j = where
      shortcut.outer:
        while indices.hasStep do
          val i = indices.nextStep
          shortcut.inner:
            that(j) = f(a.charAt(i), i)
            j += 1
      j - where

    inline def select(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): String =
      val a = sa.unwrap
      var b = new java.lang.StringBuilder()
      var i = 0
      shortcut.quittable:
        while i < a.length do
          val x = a.charAt(i)
          if pick(x) then
            b append x
          i += 1
      b.toString

    transparent inline def selectOp[B](i0: Int, iN: Int)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      val b = new Array[B](iN - i0)
      var i = i0
      var j = 0
      shortcut.outer:
        while i < iN do
          shortcut.inner:
            b(j) = op(a.charAt(i), i)
            j += 1
          i += 1
      b.shrinkTo(j)
    transparent inline def selectOp[B](v: Iv | PIv)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv.of(v, sa.unwrap)
      selectOp(iv.i0, iv.iN)(op)
    transparent inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    transparent inline def selectOp[B](indices: Array[Int])(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      val b = new Array[B](indices.length)
      var i = 0
      var k = 0
      shortcut.outer:
        while i < indices.length do
          val j = indices(i)
          shortcut.inner:
            b(k) = op(a.charAt(j), j)
            k += 1
          i += 1
      b.shrinkTo(k)
    transparent inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      var b = new Array[B](if a.length <= 8 then a.length else 8)
      var j = 0
      shortcut.outer:
        while indices.hasStep do
          val i = indices.nextStep
          shortcut.inner:
            val y = op(a.charAt(i), i)
            if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
            b(j) = y
            j += 1
      b.shrinkTo(j)

    transparent inline def fuse[B](inline add: boundary.Label[shortcut.Quits.type] ?=> (Char, Int, B => Unit) => Unit)(using ClassTag[B]): Array[B] =
      val a = sa.unwrap
      var bs = new Array[B](if a.length < 8 then a.length else 8)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < a.length do
          add(a.charAt(i), i, b => { if j >= bs.length then bs = bs.enlargeTo(bs.length | (bs.length << 1)); bs(j) = b; j += 1 })
          i += 1
      bs.shrinkTo(j)
  }
}


opaque type ShortClipString = String
object ShortClipString {
  inline def wrap(a: String): ShortClipString = a

  extension (sc: ShortClipString)
    inline def unwrap: String = sc

  extension (sc: kse.basics.ShortClipString) {
    inline def peek(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      shortcut.quittable:
        while i < iM do
          f(a.charAt(i))
          i += 1
      a
    inline def peek(v: Iv | PIv)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val iv = Iv.of(v, sc.unwrap)
      peek(iv.i0, iv.iN)(f)
    inline def peek(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val iv = Iv of rg
      peek(iv.i0, iv.iN)(f)
    inline def peek(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val a = sc.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then f(a.charAt(j))
          i += 1
      a
    inline def peek(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): String =
      val a = sc.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then f(a.charAt(j))
      a

    inline def gather[Z](zero: Z)(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var z = zero
      shortcut.quittable:
        while i < iM do
          z = f(z, a.charAt(i), i)
          i += 1
      z
    inline def gather[Z](zero: Z)(v: Iv | PIv)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      val iv = Iv.of(v, sc.unwrap)
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      val iv = Iv of rg
      gather(zero)(iv.i0, iv.iN)(f)
    inline def gather[Z](zero: Z)(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      val a = sc.unwrap
      var i = 0
      var z = zero
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then z = f(z, a.charAt(j), j)
          i += 1
      z
    inline def gather[Z](zero: Z)(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      val a = sc.unwrap
      var z = zero
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then z = f(z, a.charAt(j), j)
      z

    inline def whereIn(i0: Int, iN: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var ix = new Array[Int](if iM - i < 0 then 0 else if iM - i > 8 then 8 else iM - i)
      var j = 0
      shortcut.quittable:
        while i < iM do
          if pick(a.charAt(i)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = i
            j += 1
          i += 1
      ix.shrinkTo(j)
    inline def whereIn(v: Iv | PIv)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val iv = Iv.of(v, sc.unwrap)
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)

    inline def whereFrom(indices: Array[Int])(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val a = sc.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < indices.length do
          val k = indices(i)
          if k >= 0 && k < a.length && pick(a.charAt(k)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = k
            j += 1
          i += 1  
      ix.shrinkTo(j)

    inline def inject(that: Array[Char])(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Int =
      inject(that, 0)(pick)
    inline def inject(that: Array[Char], where: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Int =
      val a = sc.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      shortcut.quittable:
        while i < a.length && j < that.length do
          val x = a.charAt(i)
          if pick(x) then
            if j >= 0 then that(j) = x
            j += 1 
          i += 1
      if where < 0 then j else j - where

    inline def injectOp[B](that: Array[B])()(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      injectOp(that, 0)(0, sc.unwrap.length)(f)
    inline def injectOp[B](that: Array[B], where: Int)()(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      injectOp(that, where)(0, sc.unwrap.length)(f)
    inline def injectOp[B](that: Array[B])(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      injectOp(that, 0)(i0, iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = where
      if j < 0 then j = 0
      var n = iN
      if iN > a.length then n = a.length
      shortcut.outer:
        while i < n && j < that.length do
          shortcut.inner:
            that(j) = f(a.charAt(i), i)
            j += 1
          i += 1
      if where < 0 then j else j - where
    inline def injectOp[B](that: Array[B])(v: Iv | PIv)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = Iv.of(v, sc.unwrap)
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(v: Iv | PIv)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = Iv.of(v, sc.unwrap)
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = Iv of rg
      injectOp[B](that, where)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B])(indices: Array[Int])(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: Array[Int])(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val a = sc.unwrap
      var i = 0
      var j = where
      if j < 0 then j = 0
      shortcut.outer:
        while i < indices.length && j < that.length do
          val k = indices(i)
          if k >= 0 && k < a.length then
            shortcut.inner:
              that(j) = f(a.charAt(k), k)
              j += 1
          i += 1
      if where < 0 then j else j - where
    inline def injectOp[B](that: Array[B])(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      injectOp[B](that, 0)(indices)(f)
    inline def injectOp[B](that: Array[B], where: Int)(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val a = sc.unwrap
      var j = where
      if j < 0 then j = 0
      shortcut.outer:
        while indices.hasStep && j < that.length do
          val i = indices.nextStep
          if i >= 0 && i < a.length then
            shortcut.inner:
              that(j) = f(a.charAt(i), i)
              j += 1
      if where < 0 then j else j - where

    transparent inline def selectOp[B](i0: Int, iN: Int)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j > a.length then j = a.length
      if j < i then j = i
      val b = new Array[B](j - i)
      var k = 0
      shortcut.outer:
        while i < j do
          shortcut.inner:
            b(k) = op(a.charAt(i), i)
            k += 1
          i += 1
      b.shrinkTo(k)
    transparent inline def selectOp[B](v: Iv | PIv)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv.of(v, sc.unwrap)
      selectOp(iv.i0, iv.iN)(op)
    transparent inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    transparent inline def selectOp[B](indices: Array[Int])(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sc.unwrap
      val b = new Array[B](indices.length)
      var i = 0
      var j = 0
      shortcut.outer:
        while i < indices.length do
          val k = indices(i)
          if k >= 0 && k < a.length then shortcut.inner:
            b(j) = op(a.charAt(k), k)
            j += 1
          i += 1
      b.shrinkTo(j)
    transparent inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val a = sc.unwrap
      var b = new Array[B](if a.length <= 8 then a.length else 8)
      var j = 0
      shortcut.outer:
        while indices.hasStep do
          val i = indices.nextStep
          if i >= 0 && i < a.length then
            shortcut.inner:
              val y = op(a.charAt(i), i)
              if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
              b(j) = y
              j += 1
      b.shrinkTo(j)
  }
}



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
    case m: Mu[?] => m.value.asInstanceOf[Any] == value
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
    case _: Anon[?] => true
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
    case i: Identity[?] => i.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef] 
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


  /** Cut out the first value of this tuple, creating a new tuple from what"s left. */
  inline def snip_1: (B, C) = (q._2, q._3)

  /** Cut out the second value of this tuple, creating a new tuple from what"s left. */
  inline def snip_2: (A, C) = (q._1, q._3)

  /** Cut out the last value of this tuple, creating a new tuple from what"s left. */
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


  /** Cut out the first value of this tuple, creating a new tuple from what"s left. */
  inline def snip_1: (B, C, D) = (q._2, q._3, q._4)

  /** Cut out the second value of this tuple, creating a new tuple from what"s left. */
  inline def snip_2: (A, C, D) = (q._1, q._3, q._4)

  /** Cut out the third value of this tuple, creating a new tuple from what"s left. */
  inline def snip_3: (A, B, D) = (q._1, q._2, q._4)

  /** Cut out the last value of this tuple, creating a new tuple from what"s left. */
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


  /** Cut out the first value of this tuple, creating a new tuple from what"s left. */
  inline def snip_1: (B, C, D, E) = (q._2, q._3, q._4, q._5)

  /** Cut out the second value of this tuple, creating a new tuple from what"s left. */
  inline def snip_2: (A, C, D, E) = (q._1, q._3, q._4, q._5)

  /** Cut out the third value of this tuple, creating a new tuple from what"s left. */
  inline def snip_3: (A, B, D, E) = (q._1, q._2, q._4, q._5)

  /** Cut out the fourth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_4: (A, B, C, E) = (q._1, q._2, q._3, q._5)

  /** Cut out the last value of this tuple, creating a new tuple from what"s left. */
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


  /** Cut out the first value of this tuple, creating a new tuple from what"s left. */
  inline def snip_1: (B, C, D, E, F) = (q._2, q._3, q._4, q._5, q._6)

  /** Cut out the second value of this tuple, creating a new tuple from what"s left. */
  inline def snip_2: (A, C, D, E, F) = (q._1, q._3, q._4, q._5, q._6)

  /** Cut out the third value of this tuple, creating a new tuple from what"s left. */
  inline def snip_3: (A, B, D, E, F) = (q._1, q._2, q._4, q._5, q._6)

  /** Cut out the fourth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_4: (A, B, C, E, F) = (q._1, q._2, q._3, q._5, q._6)

  /** Cut out the fifth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_5: (A, B, C, D, F) = (q._1, q._2, q._3, q._4, q._6)

  /** Cut out the last value of this tuple, creating a new tuple from what"s left. */
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


  /** Cut out the first value of this tuple, creating a new tuple from what"s left. */
  inline def snip_1: (B, C, D, E, F, G) = (q._2, q._3, q._4, q._5, q._6, q._7)
  
  /** Cut out the second value of this tuple, creating a new tuple from what"s left. */
  inline def snip_2: (A, C, D, E, F, G) = (q._1, q._3, q._4, q._5, q._6, q._7)
  
  /** Cut out the third value of this tuple, creating a new tuple from what"s left. */
  inline def snip_3: (A, B, D, E, F, G) = (q._1, q._2, q._4, q._5, q._6, q._7)
  
  /** Cut out the fourth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_4: (A, B, C, E, F, G) = (q._1, q._2, q._3, q._5, q._6, q._7)
  
  /** Cut out the fifth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_5: (A, B, C, D, F, G) = (q._1, q._2, q._3, q._4, q._6, q._7)
  
  /** Cut out the sixth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_6: (A, B, C, D, E, G) = (q._1, q._2, q._3, q._4, q._5, q._7)
  
  /** Cut out the last value of this tuple, creating a new tuple from what"s left. */
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

  /** Cut out the first value of this tuple, creating a new tuple from what"s left. */
  inline def snip_1: (B, C, D, E, F, G, H) = (q._2, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Cut out the second value of this tuple, creating a new tuple from what"s left. */
  inline def snip_2: (A, C, D, E, F, G, H) = (q._1, q._3, q._4, q._5, q._6, q._7, q._8)
  
  /** Cut out the third value of this tuple, creating a new tuple from what"s left. */
  inline def snip_3: (A, B, D, E, F, G, H) = (q._1, q._2, q._4, q._5, q._6, q._7, q._8)
  
  /** Cut out the fourth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_4: (A, B, C, E, F, G, H) = (q._1, q._2, q._3, q._5, q._6, q._7, q._8)
  
  /** Cut out the fifth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_5: (A, B, C, D, F, G, H) = (q._1, q._2, q._3, q._4, q._6, q._7, q._8)
  
  /** Cut out the sixth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_6: (A, B, C, D, E, G, H) = (q._1, q._2, q._3, q._4, q._5, q._7, q._8)
  
  /** Cut out the seventh value of this tuple, creating a new tuple from what"s left. */
  inline def snip_7: (A, B, C, D, E, F, H) = (q._1, q._2, q._3, q._4, q._5, q._6, q._8)
  
  /** Cut out the last value of this tuple, creating a new tuple from what"s left. */
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


  /** Cut out the first value of this tuple, creating a new tuple from what"s left. */
  inline def snip_1: (B, C, D, E, F, G, H, I) = (q._2, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the second value of this tuple, creating a new tuple from what"s left. */
  inline def snip_2: (A, C, D, E, F, G, H, I) = (q._1, q._3, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the third value of this tuple, creating a new tuple from what"s left. */
  inline def snip_3: (A, B, D, E, F, G, H, I) = (q._1, q._2, q._4, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the fourth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_4: (A, B, C, E, F, G, H, I) = (q._1, q._2, q._3, q._5, q._6, q._7, q._8, q._9)
  
  /** Cut out the fifth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_5: (A, B, C, D, F, G, H, I) = (q._1, q._2, q._3, q._4, q._6, q._7, q._8, q._9)
  
  /** Cut out the sixth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_6: (A, B, C, D, E, G, H, I) = (q._1, q._2, q._3, q._4, q._5, q._7, q._8, q._9)
  
  /** Cut out the seventh value of this tuple, creating a new tuple from what"s left. */
  inline def snip_7: (A, B, C, D, E, F, H, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._8, q._9)
  
  /** Cut out the eigth value of this tuple, creating a new tuple from what"s left. */
  inline def snip_8: (A, B, C, D, E, F, G, I) = (q._1, q._2, q._3, q._4, q._5, q._6, q._7, q._9)
  
  /** Cut out the ninth value of this tuple, creating a new tuple from what"s left. */
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
