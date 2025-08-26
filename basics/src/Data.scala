// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2021-25 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.basics


//import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.annotation.targetName
import scala.reflect.ClassTag
import scala.util.boundary

import scala.collection.immutable.{Range => Rg}
import scala.collection.IntStepper

import kse.basics.intervals._



/////////////////////////////////////////////
/// Collection-like operations on numbers ///
/////////////////////////////////////////////

extension (i: Int){
  inline def times(inline f: => Unit): Unit =
    var j = 0
    while j < i do
      f
      j += 1

  inline def visit(inline f: Int => Unit): Unit =
    var j = 0
    while j < i do
      f(j)
      j += 1

  inline def visitBy(step: Int)(inline f: Int => Unit): Unit =
    if step != 0 then
      var j = if step < 0 then i - 1 else 0
      while j < i && j >= 0 do
        f(j)
        j += step

  def where(): Array[Int] =
    val a = new Array[Int](if i > 0 then i else 0)
    var j = 0
    while j < i do
      a(j) = j
      j += 1
    a

  def whereBy(step: Int): Array[Int] =
    if i <= 0 || step == 0 then Array.empty[Int]
    else
      val sabs = if step < 0 then -step else step
      var n = i/sabs
      if n*sabs != i then n += 1
      var j = if step < 0 then i - 1 else 0
      val a = new Array[Int](n)
      n = 0
      while n < a.length do
        a(n) = j
        j += step
        n += 1
      a

  def steps(): IntStepper =
    if i > 0 then new Iv.AffineIntStepper(i, 0, 1)
    else Iv.AffineIntStepper.empty

  def stepsBy(step: Int): IntStepper =
    if i <= 0 || step == 0 then Iv.AffineIntStepper.empty
    else
      val sabs = if step > 0 then step else -step
      val first = if step > 0 then 0 else i - 1
      val n = i / sabs
      val extra = if n*sabs == i then 0 else 1
      new Iv.AffineIntStepper(n + extra, first, step)

  inline def unfold[A](inline f: Int => A)(using ClassTag[A]): Array[A] =
    val a = new Array[A](if i > 0 then i else 0)
    var k = 0
    while k < a.length do
      a(k) = f(k)
      k += 1
    a

  inline def unfoldFlex[A](inline f: boundary.Label[shortcut.Type] ?=> Int => A)(using ClassTag[A]): Array[A] =
    val a = new Array[A](if i > 0 then i else 0)
    var j = 0
    shortcut.outer:
      var k = 0
      while k < a.length do
        shortcut.inner:
          a(j) = f(k)
          j += 1
        k += 1
    if a.length == j then a
    else
      val aa = new Array[A](j)
      System.arraycopy(a, 0, aa, 0, j)
      aa
}


extension (l: Long){
  inline def times(inline f: => Unit): Unit =
    var j = 0L
    while j < l do
      f
      j += 1

  inline def visit(inline f: Long => Unit): Unit =
    var j = 0
    while j < l do
      f(j)
      j += 1

  inline def visitBy(step: Long)(inline f: Long => Unit): Unit =
    if step != 0 then
      var j = if step < 0 then l - 1 else 0L
      while j < l && j >= 0 do
        f(j)
        j += step
}



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
    checkBounds(ai.length, i0, iN, target.length, where, -2): Unit
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
    checkBounds(af.length, i0, iN, target.length, where, -2): Unit
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
    checkBounds(al.length, i0, iN, target.length, where, -3): Unit
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
    checkBounds(ad.length, i0, iN, target.length, where, -3): Unit
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


/** Higher-level high-speed array access (inlined) */
extension [A](a: Array[A]) {
  @targetName("update_End_At") inline def update(i: End.At, x: A): Unit = a(i of a) = x
  @targetName("update_End_type") inline def update(e: End.type, x: A): Unit = a(a.length - 1) = x
  @targetName("update_Start_At") inline def update(i: Start.At, x: A): Unit = a(i.unwrap) = x
  @targetName("update_Start_type") inline def update(i: Start.type, x: A): Unit = a(0) = x

  inline def clip: kse.basics.ClippedArray[A] = ClippedArray wrap a

  inline def flex: kse.basics.FlexArray[A] = FlexArray wrap a

  inline def fancy: kse.basics.FancyArray[A] = FancyArray wrap a

  inline def use()(inline f: A => Unit): Unit =
    var i = 0
    while i < a.length do
      f(a(i))
      i += 1
  inline def use(i0: Int, iN: Int)(inline f: A => Unit): Unit =
    var i = i0
    while i < iN do
      f(a(i))
      i += 1
  inline def use(ivx: Iv.X)(inline f: A => Unit): Unit =
    use(ivx.index0(a), ivx.indexN(a))(f)
  inline def use(inline rg: collection.immutable.Range)(inline f: A => Unit): Unit =
    val iv = Iv of rg
    use(iv.i0, iv.iN)(f)
  inline def use(indices: Array[Int])(inline f: A => Unit): Unit =
    var i = 0
    while i < indices.length do
      f(a(indices(i)))
      i += 1
  inline def use(indices: scala.collection.IntStepper)(inline f: A => Unit): Unit =
    while indices.hasStep do
      f(a(indices.nextStep))
  inline def use(inline p: A => Boolean)(inline f: A => Unit): Unit =
    var i = 0
    while i < a.length do
      val x = a(i)
      if p(x) then f(x)
      i += 1

  inline def alter()(inline f: A => A): Unit =
    var i = 0
    while i < a.length do
      a(i) = f(a(i))
      i += 1
  inline def alter(i0: Int, iN: Int)(inline f: A => A): Unit =
    var i = i0
    while i < iN do
      a(i) = f(a(i))
      i += 1
  inline def alter(ivx: Iv.X)(inline f: A => A): Unit =
    alter(ivx.index0(a), ivx.indexN(a))(f)
  inline def alter(inline rg: collection.immutable.Range)(inline f: A => A): Unit =
    val iv = Iv of rg
    alter(iv.i0, iv.iN)(f)
  inline def alter(indices: Array[Int])(inline f: A => A): Unit =
    var i = 0
    while i < indices.length do
      val j = indices(i)
      a(j) = f(a(j))
      i += 1
  inline def alter(indices: scala.collection.IntStepper)(inline f: A => A): Unit =
    while indices.hasStep do
      val j = indices.nextStep
      a(j) = f(a(j))
  inline def alter(inline p: A => Boolean)(inline f: A => A): Unit =
    var i = 0
    while i < a.length do
      val x = a(i)
      if p(x) then a(i) = f(x)
      i += 1

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
  inline def visit(ivx: Iv.X)(inline f: (A, Int) => Unit): Unit =
    visit(ivx.index0(a), ivx.indexN(a))(f)
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
  inline def visit(inline p: A => Boolean)(inline f: (A, Int) => Unit): Unit =
    var i = 0
    while i < a.length do
      val x = a(i)
      if p(x) then f(x, i)
      i += 1

  inline def edit()(inline f: (A, Int) => A): Unit =
    var i = 0
    while i < a.length do
      a(i) = f(a(i), i)
      i += 1
  inline def edit(i0: Int, iN: Int)(inline f: (A, Int) => A): Unit =
    var i = i0
    while i < iN do
      a(i) = f(a(i), i)
      i += 1
  inline def edit(ivx: Iv.X)(inline f: (A, Int) => A): Unit =
    edit(ivx.index0(a), ivx.indexN(a))(f)
  inline def edit(inline rg: collection.immutable.Range)(inline f: (A, Int) => A): Unit =
    val iv = Iv of rg
    edit(iv.i0, iv.iN)(f)
  inline def edit(indices: Array[Int])(inline f: (A, Int) => A): Unit =
    var i = 0
    while i < indices.length do
      val j = indices(i)
      a(j) = f(a(j), j)
      i += 1
  inline def edit(indices: scala.collection.IntStepper)(inline f: (A, Int) => A): Unit =
    while indices.hasStep do
      val j = indices.nextStep
      a(j) = f(a(j), j)
  inline def edit(inline p: A => Boolean)(inline f: (A, Int) => A): Unit =
    var i = 0
    while i < a.length do
      val x = a(i)
      if p(x) then a(i) = f(x, i)
      i += 1

  inline def pairs(inline f: (A, A) => Unit): Unit =
    if a.length > 0 then
      var a0 = a(0)
      var i = 1
      while i < a.length do
        val a1 = a(i)
        f(a0, a1)
        a0 = a1
        i += 1
   inline def trios(inline f: (A, A, A) => Unit): Unit =
    if a.length > 1 then
      var a0 = a(0)
      var a1 = a(1)
      var i = 2
      while i < a.length do
        val a2 = a(i)
        f(a0, a1, a2)
        a0 = a1
        a1 = a2
        i += 1

  inline def together[B](b: Array[B])(inline f: (A, B, Int) => Unit): Unit =
    val n = if a.length > b.length then b.length else a.length
    var i = 0
    while i < n do
      f(a(i), b(i), i)
      i += 1
  inline def together(b: String)(inline f: (A, Char, Int) => Unit): Unit =
    val n = if a.length > b.length then b.length else a.length
    var i = 0
    while i < n do
      f(a(i), b.charAt(i), i)
      i += 1
  inline def together[B, C](b: Array[B], c: Array[C])(inline f: (A, B, C, Int) => Unit): Unit =
    var n = a.length
    if b.length < n then n = b.length
    if c.length < n then n = c.length
    var i = 0
    while i < n do
      f(a(i), b(i), c(i), i)
      i += 1
  inline def together[B](b: Array[B], c: String)(inline f: (A, B, Char, Int) => Unit): Unit =
    var n = a.length
    if b.length < n then n = b.length
    if c.length < n then n = c.length
    var i = 0
    while i < n do
      f(a(i), b(i), c.charAt(i), i)
      i += 1
  inline def together[C](b: String, c: Array[C])(inline f: (A, Char, C, Int) => Unit): Unit =
    var n = a.length
    if b.length < n then n = b.length
    if c.length < n then n = c.length
    var i = 0
    while i < n do
      f(a(i), b.charAt(i), c(i), i)
      i += 1
  inline def together(b: String, c: String)(inline f: (A, Char, Char, Int) => Unit): Unit =
    var n = a.length
    if b.length < n then n = b.length
    if c.length < n then n = c.length
    var i = 0
    while i < n do
      f(a(i), b.charAt(i), c.charAt(i), i)
      i += 1

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
  inline def gather[Z](zero: Z)(ivx: Iv.X)(inline f: (Z, A, Int) => Z): Z =
    gather(zero)(ivx.index0(a), ivx.indexN(a))(f)
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
  inline def gather[Z](zero: Z)(inline p: A => Boolean)(inline f: (Z, A, Int) => Z) =
    var i = 0
    var z = zero
    while i < a.length do
      val x = a(i)
      if p(x) then z = f(z, x, i)
      i += 1
    z

  @targetName("update_All_constant")
  inline def update(value: A): Unit =
    update(Iv(0, a.length), value)
  @targetName("update_All_array")
  inline def update(values: Array[A]): Unit =
    update(Iv(0, a.length), values)

  @targetName("update_Iv_constant")
  inline def update(ivx: Iv.X, value: A): Unit =
    var i = ivx.index0(a)
    val j = ivx.indexN(a)
    while i < j do
      a(i) = value
      i += 1
  @targetName("update_Iv_array")
  inline def update(ivx: Iv.X, values: Array[A]): Unit =
    val i = ivx.index0(a)
    java.lang.System.arraycopy(values, 0, a, i, ivx.indexN(a) - i)

  @targetName("update_Range_constant")
  inline def update(inline rg: collection.immutable.Range, value: A): Unit =
    update(Iv of rg, value)
  @targetName("update_Range_array")
  inline def update(inline rg: collection.immutable.Range, values: Array[A]): Unit =
    update(Iv of rg, values)

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

  @targetName("set_Iv_generate")
  inline def set(ivx: Iv.X)(inline generator: () => A): Unit =
    set(ivx.index0(a), ivx.indexN(a))(generator)
  @targetName("set_Iv_index")
  inline def set(ivx: Iv.X)(inline indexer: Int => A): Unit =
    set(ivx.index0(a), ivx.indexN(a))(indexer)

  @targetName("set_Range_generate")
  inline def set(inline rg: collection.immutable.Range)(inline generator: () => A): Unit =
    val iv = Iv of rg
    set(iv.i0, iv.iN)(generator)
  @targetName("set_Range_index")
  inline def set(inline rg: collection.immutable.Range)(inline indexer: Int => A): Unit =
    val iv = Iv of rg
    set(iv.i0, iv.iN)(indexer)

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

  @targetName("set_Stepper_generate")
  inline def set(indices: scala.collection.IntStepper)(inline generator: () => A): Unit =
    while indices.hasStep do
      a(indices.nextStep) = generator()
  @targetName("set_Stepper_index")
  inline def set(indices: scala.collection.IntStepper)(inline indexer: Int => A): Unit =
    while indices.hasStep do
      val j = indices.nextStep
      a(j) = indexer(j)

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

  def where(): Array[Int] =
    val ix = new Array[Int](a.length)
    var i = 0
    while i < ix.length do
      ix(i) = i
      i += 1
    ix
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
  inline def whereOp(inline pick: (A, Int) => Int): Array[Int] =
    var ix = new Array[Int](if a.length <= 8 then a.length else 8)
    var i = 0
    var j = 0
    while i < a.length do
      val h = pick(a(i), i)
      if h >= 0 then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = h
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
  inline def whereIn(ivx: Iv.X)(inline pick: A => Boolean): Array[Int] =
    whereIn(ivx.index0(a), ivx.indexN(a))(pick)
  inline def whereIn(inline rg: Range)(inline pick: A => Boolean): Array[Int] =
    val iv = Iv of rg
    whereIn(iv.i0, iv.iN)(pick)
  inline def whereInOp(i0: Int, iN: Int)(inline pick: (A, Int) => Int): Array[Int] =
    var ix = new Array[Int](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else iN - i0)
    var i = i0
    var j = 0
    while i < iN do
      val h = pick(a(i), i)
      if h >= 0 then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = h
        j += 1
      i += 1
    ix.shrinkTo(j)
  inline def whereInOp(ivx: Iv.X)(inline pick: (A, Int) => Int): Array[Int] =
    whereInOp(ivx.index0(a), ivx.indexN(a))(pick)
  inline def whereInOp(inline rg: Range)(inline pick: (A, Int) => Int): Array[Int] =
    val iv = Iv of rg
    whereInOp(iv.i0, iv.iN)(pick)

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
  inline def whereFromOp(indices: Array[Int])(inline pick: (A, Int) => Int): Array[Int] =
    var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
    var i = 0
    var j = 0
    while i < indices.length do
      val k = indices(i)
      val h = pick(a(k), k)
      if h >= 0 then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = h
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
  inline def inject(that: Array[A])(ivx: Iv.X): Int =
    inject(that, 0)(ivx.index0(a), ivx.indexN(a))
  inline def inject(that: Array[A], where: Int)(ivx: Iv.X): Int =
    inject(that, where)(ivx.index0(a), ivx.indexN(a))
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
  inline def injectOp[B](that: Array[B])(ivx: Iv.X)(inline f: (A, Int) => B): Int =
    injectOp[B](that, 0)(ivx.index0(a), ivx.indexN(a))(f)
  inline def injectOp[B](that: Array[B], where: Int)(ivx: Iv.X)(inline f: (A, Int) => B): Int =
    injectOp[B](that, where)(ivx.index0(a), ivx.indexN(a))(f)
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
  inline def select(ivx: Iv.X)(using ClassTag[A]): Array[A] =
    select(ivx.index0(a), ivx.indexN(a))
  inline def select(inline rg: collection.immutable.Range)(using ClassTag[A]): Array[A] =
    val iv = Iv of rg
    select(iv.i0, iv.iN)
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

  inline def selectOp[B](i0: Int, iN: Int)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](iN - i0)
    var i = i0
    while i < iN do
      b(i - i0) = op(a(i), i)
      i += 1
    b
  inline def selectOp[B](ivx: Iv.X)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    selectOp(ivx.index0(a), ivx.indexN(a))(op)
  inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val iv = Iv of rg
    selectOp(iv.i0, iv.iN)(op)
  inline def selectOp[B](indices: Array[Int])(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](indices.length)
    var i = 0
    while i < indices.length do
      val j = indices(i)
      b(i) = op(a(j), j)
      i += 1
    b
  inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
    var b = new Array[B](if a.length <= 8 then a.length else 8)
    var j = 0
    while indices.hasStep do
      if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
      val i = indices.nextStep
      b(j) = op(a(i), i)
      j += 1
    b.shrinkTo(j)
  inline def selectOp[B](inline pick: A => Boolean)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
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

  transparent inline def diced(inline cut: A => Boolean, mode: "" | "()" | "[)" | "(]" | "[]")(using ClassTag[A]): Array[Array[A]] =
    var bss: Array[Array[A]] = null
    var bzero: Array[A] = null
    var k = 0
    var i0: Int = inline mode match
      case "[)" | "[]" => 0
      case _           => -1
    var j = 0
    while j <= a.length do
      if j == a.length || cut(a(j)) then
        val iN = inline mode match
          case "[]" | "(]" =>
            if j < a.length then j+1
            else j
          case _ => j
        val n = inline mode match
          case "[]" | "[)" => iN - i0
          case "" =>
            i0 += 1
            if i0 == iN then -1 else iN - i0
          case _ =>
            i0 += 1
            iN - i0
        if n >= 0 then
          val b =
            if n == 0 then
              if bzero eq null then bzero = new Array[A](0)
              bzero
            else
              val temp = new Array[A](n)
              System.arraycopy(a, i0, temp, 0, n)
              temp
          if bss eq null then
            bss = new Array[Array[A]](if j == a.length then 1 else 8)
          else if k >= bss.length then
            val temp = new Array[Array[A]](if j == a.length then k+1 else bss.length | (bss.length << 1))
            System.arraycopy(bss, 0, temp, 0, bss.length)
            bss = temp
          bss(k) = b
          k += 1
        i0 = j
      j += 1
    if bss eq null then new Array[Array[A]](0)
    else if k < bss.length then java.util.Arrays.copyOf(bss, k)
    else bss
  transparent inline def diced(inline cut: A => Boolean)(using ClassTag[A]): Array[Array[A]] = diced(cut, "")

  inline def visitCuts()(inline cut: (A, A) => Boolean)(inline f: (Int, Int) => Unit): Unit =
    visitCuts(0, a.length)(cut)(f)
  inline def visitCuts(i0: Int, iN: Int)(inline cut: (A, A) => Boolean)(inline f: (Int, Int) => Unit): Unit =
    var i = i0
    while i < iN do
      var x = a(i)
      var j = i + 1
      var continue = true
      while continue && j < iN do
        val y = a(j)
        if cut(x, y) then continue = false
        else x = y
        j += 1
      f(i, j)
      i = j
  inline def visitCuts(ivx: Iv.X)(inline cut: (A, A) => Boolean)(inline f: (Int, Int) => Unit): Unit =
    visitCuts(ivx.index0(a), ivx.indexN(a))(cut)(f)
  inline def visitCuts(inline rg: Rg)(inline cut: (A, A) => Boolean)(inline f: (Int, Int) => Unit): Unit =
    val iv = Iv of rg
    visitCuts(iv.i0, iv.iN)(cut)(f)
}



opaque type ClippedArray[A] = Array[A]
object ClippedArray {
  inline def wrap[A](a: Array[A]): ClippedArray[A] = a

  extension [A](ca: ClippedArray[A])
    inline def unwrap: Array[A] = ca

  extension [A](ca: kse.basics.ClippedArray[A]) {
    inline def flex: kse.basics.FancyArray[A] = FancyArray wrap ca.unwrap

    inline def apply(i: Int)(inline x0: => A): A =
      val a = ca.unwrap
      if i >= 0 && i < a.length then a(i)
      else x0

    inline def get(i: Int): Option[A] =
      val a = ca.unwrap
      if i >= 0 && i < a.length then Some(a(i))
      else None

    inline def use(i: Int)(inline f: A => Unit): Unit =
      val a = ca.unwrap
      if i >= 0 && i < a.length then f(a(i))
    inline def use(i0: Int, iN: Int)(inline f: A => Unit): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      while i < iM do
        f(a(i))
        i += 1
    inline def use(ivx: Iv.X)(inline f: A => Unit): Unit =
      val iv = ivx of ca.unwrap
      use(iv.i0, iv.iN)(f)
    inline def use(inline rg: collection.immutable.Range)(inline f: A => Unit): Unit =
      val iv = Iv of rg
      use(iv.i0, iv.iN)(f)
    inline def use(indices: Array[Int])(inline f: A => Unit): Unit =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then f(a(j))
        i += 1
    inline def use(indices: scala.collection.IntStepper)(inline f: A => Unit): Unit =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then f(a(j))

    inline def alter(i0: Int, iN: Int)(inline f: A => A): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      while i < iM do
        a(i) = f(a(i))
        i += 1
    inline def alter(ivx: Iv.X)(inline f: A => A): Unit =
      val iv = ivx of ca.unwrap
      alter(iv.i0, iv.iN)(f)
    inline def alter(inline rg: collection.immutable.Range)(inline f: A => A): Unit =
      val iv = Iv of rg
      alter(iv.i0, iv.iN)(f)
    inline def alter(indices: Array[Int])(inline f: A => A): Unit =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then a(j) = f(a(j))
        i += 1
    inline def alter(indices: scala.collection.IntStepper)(inline f: A => A): Unit =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then a(j) = f(a(j))

    inline def visit(i0: Int, iN: Int)(inline f: (A, Int) => Unit): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      while i < iM do
        f(a(i), i)
        i += 1
    inline def visit(ivx: Iv.X)(inline f: (A, Int) => Unit): Unit =
      val iv = ivx of ca.unwrap
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

    inline def edit(i0: Int, iN: Int)(inline f: (A, Int) => A): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      var j = iN
      if j > a.length then j = a.length
      while i < j do
        a(i) = f(a(i), i)
        i += 1
    inline def edit(ivx: Iv.X)(inline f: (A, Int) => A): Unit =
      val iv = ivx of ca.unwrap
      edit(iv.i0, iv.iN)(f)
    inline def edit(inline rg: collection.immutable.Range)(inline f: (A, Int) => A): Unit =
      val iv = Iv of rg
      edit(iv.i0, iv.iN)(f)
    inline def edit(indices: Array[Int])(inline f: (A, Int) => A): Unit =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then a(j) = f(a(j), j)
        i += 1
    inline def edit(indices: scala.collection.IntStepper)(inline f: (A, Int) => A): Unit =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then a(j) = f(a(j), j)

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
    inline def gather[Z](zero: Z)(ivx: Iv.X)(inline f: (Z, A, Int) => Z): Z =
      val iv = ivx of ca.unwrap
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
      kse.basics.ClippedArray.update(ca)(Iv(0, ca.unwrap.length), values)

    @targetName("update_Iv_constant")
    inline def update(ivx: Iv.X, value: A): Unit =
      val a = ca.unwrap
      var i = ivx.index0(a)
      if i < 0 then i = 0
      var j = ivx.indexN(a)
      if j > a.length then j = a.length
      while i < j do
        a(i) = value
        i += 1
    @targetName("update_Iv_array")
    inline def update(ivx: Iv.X, values: Array[A]): Unit =
      val a = ca.unwrap
      var i = ivx.index0(a)
      if i < 0 then i = 0
      var j = ivx.indexN(a)
      if j > a.length then j = a.length
      if j >= 0 && j - i > values.length then j = i + values.length
      if j > i then java.lang.System.arraycopy(values, 0, a, i, j - i)

    @targetName("update_Range_constant")
    inline def update(inline rg: collection.immutable.Range, value: A): Unit =
      kse.basics.ClippedArray.update(ca)(Iv of rg, value)
    @targetName("update_Range_array")
    inline def update(inline rg: collection.immutable.Range, values: Array[A]): Unit =
      kse.basics.ClippedArray.update(ca)(Iv of rg, values)

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

    @targetName("set_Iv_generate")
    inline def set(ivx: Iv.X)(inline generator: () => A): Unit =
      val iv = ivx of ca.unwrap
      set(iv.i0, iv.iN)(generator)
    @targetName("set_Iv_index")
    inline def set(ivx: Iv.X)(inline indexer: Int => A): Unit =
      val iv = ivx of ca.unwrap
      set(iv.i0, iv.iN)(indexer)

    @targetName("set_Range_generate")
    inline def set(inline rg: collection.immutable.Range)(inline generator: () => A): Unit =
      val iv = Iv of rg
      set(iv.i0, iv.iN)(generator)
    @targetName("set_Range_index")
    inline def set(inline rg: collection.immutable.Range)(inline indexer: Int => A): Unit =
      val iv = Iv of rg
      set(iv.i0, iv.iN)(indexer)

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
    inline def whereIn(ivx: Iv.X)(inline pick: A => Boolean): Array[Int] =
      val iv = ivx of ca.unwrap
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: A => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereInOp(i0: Int, iN: Int)(inline pick: (A, Int) => Int): Array[Int] =
      val a = ca.unwrap
      var i = i0
      if i0 < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var ix = new Array[Int](if iM - i < 0 then 0 else if iM - i > 8 then 8 else iM - i)
      var j = 0
      while i < iM do
        val h = pick(a(i), i)
        if h >= 0 then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = h
          j += 1
        i += 1
      ix.shrinkTo(j)
    inline def whereInOp(ivx: Iv.X)(inline pick: (A, Int) => Int): Array[Int] =
      val iv = ivx of ca.unwrap
      whereInOp(iv.i0, iv.iN)(pick)
    inline def whereInOp(inline rg: Range)(inline pick: (A, Int) => Int): Array[Int] =
      val iv = Iv of rg
      whereInOp(iv.i0, iv.iN)(pick)

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
    inline def whereFromOp(indices: Array[Int])(inline pick: (A, Int) => Int): Array[Int] =
      val a = ca.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        if k >= 0 && k < a.length then
          val h = pick(a(k), k)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
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
    inline def inject(that: Array[A])(ivx: Iv.X): Int =
      val iv = ivx of ca.unwrap
      inject(that, 0)(iv.i0, iv.iN)
    inline def inject(that: Array[A], where: Int)(ivx: Iv.X): Int =
      val iv = ivx of ca.unwrap
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
    inline def injectOp[B](that: Array[B])(ivx: Iv.X)(inline f: (A, Int) => B): Int =
      val iv = ivx of ca.unwrap
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(ivx: Iv.X)(inline f: (A, Int) => B): Int =
      val iv = ivx of ca.unwrap
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
    inline def select(ivx: Iv.X)(using ClassTag[A]): Array[A] =
      val iv = ivx of ca.unwrap
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

    inline def selectOp[B](i0: Int, iN: Int)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](ivx: Iv.X)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = ivx of ca.unwrap
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](indices: Array[Int])(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: (A, Int) => B)(using ClassTag[B]): Array[B] =
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

    inline def visitCuts(i0: Int, iN: Int)(inline cut: (A, A) => Boolean)(inline f: (Int, Int) => Unit): Unit =
      val a = ca.unwrap
      var i = i0
      if i0 < 0 then i = 0
      var l = ca.unwrap.length
      if iN < l then l = iN
      while i < l do
        var x = a(i)
        var j = i + 1
        var continue = true
        while continue && j < l do
          val y = a(j)
          if cut(x, y) then continue = false
          else x = y
          j += 1
        f(i, j)
        i = j
    inline def visitCuts(ivx: Iv.X)(inline cut: (A, A) => Boolean)(inline f: (Int, Int) => Unit): Unit =
      val a = ca.unwrap
      visitCuts(ivx.index0(a), ivx.indexN(a))(cut)(f)
    inline def visitCuts(inline rg: Rg)(inline cut: (A, A) => Boolean)(inline f: (Int, Int) => Unit): Unit =
      val iv = Iv of rg
      visitCuts(iv.i0, iv.iN)(cut)(f)
  }
}



opaque type FlexArray[A] = Array[A]
object FlexArray {
  inline def wrap[A](a: Array[A]): FlexArray[A] = a

  extension [A](sa: FlexArray[A])
    inline def unwrap: Array[A] = sa

  extension [A](sa: kse.basics.FlexArray[A]) {
    inline def clip: kse.basics.FancyArray[A] = FancyArray wrap sa.unwrap

    inline def use()(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          f(a(i))
          i += 1
    inline def use(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val a = sa.unwrap
      var i = i0
      shortcut.quittable:
        while i < iN do
          f(a(i))
          i += 1
    inline def use(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val iv = ivx of sa.unwrap
      use(iv.i0, iv.iN)(f)
    inline def use(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val iv = Iv of rg
      use(iv.i0, iv.iN)(f)
    inline def use(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          f(a(indices(i)))
          i += 1
    inline def use(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          f(a(indices.nextStep))
    inline def use(inline p: A => Boolean)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          val x = a(i)
          if p(x) then f(x)
          i += 1

    inline def alter()(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          a(i) = f(a(i))
          i += 1
    inline def alter(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val a = sa.unwrap
      var i = i0
      shortcut.quittable:
        while i < iN do
          a(i) = f(a(i))
          i += 1
    inline def alter(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val iv = ivx of sa.unwrap
      alter(iv.i0, iv.iN)(f)
    inline def alter(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val iv = Iv of rg
      alter(iv.i0, iv.iN)(f)
    inline def alter(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          a(j) = f(a(j))
          i += 1
    inline def alter(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          a(j) = f(a(j))
    inline def alter(inline p: A => Boolean)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          val x = a(i)
          if p(x) then a(i) = f(x)
          i += 1

    inline def visit()(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          f(a(i), i)
          i += 1
    inline def visit(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val a = sa.unwrap
      var i = i0
      shortcut.quittable:
        while i < iN do
          f(a(i), i)
          i += 1
    inline def visit(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val iv = ivx of sa.unwrap
      visit(iv.i0, iv.iN)(f)
    inline def visit(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val iv = Iv of rg
      visit(iv.i0, iv.iN)(f)
    inline def visit(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          f(a(j), j)
          i += 1
    inline def visit(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          f(a(j), j)
    inline def visit(inline p: A => Boolean)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          val x = a(i)
          if p(x) then f(x, i)
          i += 1

    inline def edit()(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          a(i) = f(a(i), i)
          i += 1
    inline def edit(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val a = sa.unwrap
      var i = i0
      shortcut.quittable:
        while i < iN do
          a(i) = f(a(i), i)
          i += 1
    inline def edit(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val iv = ivx of sa.unwrap
      edit(iv.i0, iv.iN)(f)
    inline def edit(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val iv = Iv of rg
      edit(iv.i0, iv.iN)(f)
    inline def edit(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          a(j) = f(a(j), j)
          i += 1
    inline def edit(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          a(j) = f(a(j), j)
    inline def edit(inline p: A => Boolean)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          val x = a(i)
          if p(x) then a(i) = f(x, i)
          i += 1

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
    inline def gather[Z](zero: Z)(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      val iv = ivx of sa.unwrap
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
    inline def gather[Z](zero: Z)(inline p: A => Boolean)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z) =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = 0
        while i < a.length do
          val x = a(i)
          if p(x) then z = f(z, a(i), i)
          i += 1
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
    inline def whereOp(inline pick: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Int): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if a.length <= 8 then a.length else 8)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < a.length do
          val h = pick(a(i), i)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
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
    inline def whereIn(ivx: Iv.X)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val iv = ivx of sa.unwrap
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereInOp(i0: Int, iN: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Int): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else iN - i0)
      var i = i0
      var j = 0
      shortcut.quittable:
        while i < iN do
          val h = pick(a(i), i)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
            j += 1
          i += 1
      ix.shrinkTo(j)
    inline def whereInOp(ivx: Iv.X)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Int): Array[Int] =
      val iv = ivx of sa.unwrap
      whereInOp(iv.i0, iv.iN)(pick)
    inline def whereInOp(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Int): Array[Int] =
      val iv = Iv of rg
      whereInOp(iv.i0, iv.iN)(pick)

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
    inline def whereFromOp(indices: Array[Int])(inline pick: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Int): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < indices.length do
          val k = indices(i)
          val h = pick(a(k), k)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
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
    inline def injectOp[B](that: Array[B])(ivx: Iv.X)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = ivx of sa.unwrap
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(ivx: Iv.X)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = ivx of sa.unwrap
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

    inline def selectOp[B](i0: Int, iN: Int)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](ivx: Iv.X)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = ivx of sa.unwrap
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](indices: Array[Int])(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
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

    transparent inline def diced(cut: boundary.Label[shortcut.Quits.type] ?=> A => Boolean, mode: "" | "()" | "[)" | "(]" | "[]")(using ClassTag[A]): Array[Array[A]] =
      val a = sa.unwrap
      var bss: Array[Array[A]] = null
      var bzero: Array[A] = null
      var k = 0
      var i0: Int = inline mode match
        case "[)" | "[]" => 0
        case _           => -1
      var j = 0
      var unbroken = true
      while j <= a.length && unbroken do
        var cutme = j == a.length
        if !cutme then
          shortcut.quittable:
            unbroken = false
            cutme = cut(a(j))
            unbroken = true
        if cutme || !unbroken then
          val iN = inline mode match
            case "[]" | "(]" =>
              if j < a.length && unbroken then j+1
              else j
            case _ => j
          val n = inline mode match
            case "[]" | "[)" => iN - i0
            case "" =>
              i0 += 1
              if i0 == iN then -1 else iN - i0
            case _ =>
              i0 += 1
              iN - i0
          if n >= 0 then
            val b =
              if n == 0 then
                if bzero eq null then bzero = new Array[A](0)
                bzero
              else
                val temp = new Array[A](n)
                System.arraycopy(a, i0, temp, 0, n)
                temp
            if bss eq null then
              bss = new Array[Array[A]](if j == a.length || !unbroken then 1 else 8)
            else if k >= bss.length then
              val temp = new Array[Array[A]](if j == a.length || !unbroken then k+1 else bss.length | (bss.length << 1))
              System.arraycopy(bss, 0, temp, 0, bss.length)
              bss = temp
            bss(k) = b
            k += 1
          i0 = j
        j += 1
      if bss eq null then new Array[Array[A]](0)
      else if k < bss.length then java.util.Arrays.copyOf(bss, k)
      else bss
    transparent inline def diced(cut: boundary.Label[shortcut.Quits.type] ?=> A => Boolean)(using ClassTag[A]): Array[Array[A]] = diced(cut, "")
  }
}



opaque type FancyArray[A] = Array[A]
object FancyArray {
  inline def wrap[A](a: Array[A]): FancyArray[A] = a

  extension [A](sc: FancyArray[A])
    inline def unwrap: Array[A] = sc

  extension [A](sc: kse.basics.FancyArray[A]) {
    inline def use(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      shortcut.quittable:
        while i < iM do
          f(a(i))
          i += 1
    inline def use(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val iv = ivx of sc.unwrap
      use(iv.i0, iv.iN)(f)
    inline def use(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val iv = Iv of rg
      use(iv.i0, iv.iN)(f)
    inline def use(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val a = sc.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then f(a(j))
          i += 1
    inline def use(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => Unit): Unit =
      val a = sc.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then f(a(j))

    inline def alter(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      shortcut.quittable:
        while i < iM do
          a(i) = f(a(i))
          i += 1
    inline def alter(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val iv = ivx of sc.unwrap
      alter(iv.i0, iv.iN)(f)
    inline def alter(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val iv = Iv of rg
      alter(iv.i0, iv.iN)(f)
    inline def alter(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val a = sc.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then a(j) = f(a(j))
          i += 1
    inline def alter(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> A => A): Unit =
      val a = sc.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then a(j) = f(a(j))

    inline def visit(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      shortcut.quittable:
        while i < iM do
          f(a(i), i)
          i += 1
    inline def visit(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val iv = ivx of sc.unwrap
      visit(iv.i0, iv.iN)(f)
    inline def visit(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val iv = Iv of rg
      visit(iv.i0, iv.iN)(f)
    inline def visit(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val a = sc.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then f(a(j), j)
          i += 1
    inline def visit(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Unit): Unit =
      val a = sc.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then f(a(j), j)

    inline def edit(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      shortcut.quittable:
        while i < iM do
          a(i) = f(a(i), i)
          i += 1
    inline def edit(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val iv = ivx of sc.unwrap
      edit(iv.i0, iv.iN)(f)
    inline def edit(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val iv = Iv of rg
      edit(iv.i0, iv.iN)(f)
    inline def edit(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val a = sc.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then a(j) = f(a(j), j)
          i += 1
    inline def edit(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => A): Unit =
      val a = sc.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then a(j) = f(a(j), j)

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
    inline def gather[Z](zero: Z)(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, A, Int) => Z): Z =
      val iv = ivx of sc.unwrap
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
    inline def whereIn(ivx: Iv.X)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val iv = ivx of sc.unwrap
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> A => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereInOp(i0: Int, iN: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Int): Array[Int] =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var ix = new Array[Int](if iM - i < 0 then 0 else if iM - i > 8 then 8 else iM - i)
      var j = 0
      shortcut.quittable:
        while i < iM do
          val h = pick(a(i), i)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
            j += 1
          i += 1
      ix.shrinkTo(j)
    inline def whereInOp(ivx: Iv.X)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Int): Array[Int] =
      val iv = ivx of sc.unwrap
      whereInOp(iv.i0, iv.iN)(pick)
    inline def whereInOp(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Int): Array[Int] =
      val iv = Iv of rg
      whereInOp(iv.i0, iv.iN)(pick)

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
    inline def whereFromOp(indices: Array[Int])(inline pick: boundary.Label[shortcut.Quits.type] ?=> (A, Int) => Int): Array[Int] =
      val a = sc.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < indices.length do
          val k = indices(i)
          if k >= 0 && k < a.length then
            val h = pick(a(k), k)
            if h >= 0 then
              if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
              ix(j) = h
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
    inline def injectOp[B](that: Array[B])(ivx: Iv.X)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = ivx of sc.unwrap
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(ivx: Iv.X)(inline f: boundary.Label[shortcut.Type] ?=> (A, Int) => B): Int =
      val iv = ivx of sc.unwrap
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

    inline def selectOp[B](i0: Int, iN: Int)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](ivx: Iv.X)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = ivx of sc.unwrap
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](indices: Array[Int])(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: boundary.Label[shortcut.Type] ?=> (A, Int) => B)(using ClassTag[B]): Array[B] =
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
  inline def copyToSize(endpoint: End.At): Array[Boolean] =
    java.util.Arrays.copyOf(az, 1 + (endpoint of az))

  inline def copyOfRange(i0: Int, iN: Int): Array[Boolean] =
    java.util.Arrays.copyOfRange(az, i0, iN)
  inline def copyOfRange(ivx: Iv.X): Array[Boolean] =
    java.util.Arrays.copyOfRange(az, ivx.index0(az), ivx.indexN(az))
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Boolean] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(az, iv.i0, iv.iN)

  inline def fill(x: Boolean): Unit =
    java.util.Arrays.fill(az, x)
  inline def fillRange(i0: Int, iN: Int)(x: Boolean): Unit = 
    java.util.Arrays.fill(az, i0, iN, x)
  inline def fillRange(ivx: Iv.X)(x: Boolean): Unit = 
    java.util.Arrays.fill(az, ivx.index0(az), ivx.indexN(az), x)
  inline def fillRange(inline rg: collection.immutable.Range)(x: Boolean): Unit =
    val iv = Iv of rg
    java.util.Arrays.fill(az, iv.i0, iv.iN, x)
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
  inline def copyToSize(endpoint: End.At): Array[Byte] =
    java.util.Arrays.copyOf(ab, 1 + (endpoint of ab))

  inline def copyOfRange(i0: Int, iN: Int): Array[Byte] =
    java.util.Arrays.copyOfRange(ab, i0, iN)
  inline def copyOfRange(ivx: Iv.X): Array[Byte] =
    java.util.Arrays.copyOfRange(ab, ivx.index0(ab), ivx.indexN(ab))
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Byte] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(ab, iv.i0, iv.iN)

  inline def search(x: Byte): Int =
    java.util.Arrays.binarySearch(ab, x)
  inline def searchRange(i0: Int, iN: Int)(x: Byte): Int =
    java.util.Arrays.binarySearch(ab, i0, iN, x)
  inline def searchRange(ivx: Iv.X)(x: Byte): Int =
    java.util.Arrays.binarySearch(ab, ivx.index0(ab), ivx.indexN(ab), x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Byte): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(ab, iv.i0, iv.iN, x)


  inline def fill(x: Byte): Unit =
    java.util.Arrays.fill(ab, x)
  inline def fillRange(i0: Int, iN: Int)(x: Byte): Unit = 
    java.util.Arrays.fill(ab, i0, iN, x)
  inline def fillRange(ivx: Iv.X)(x: Byte): Unit =
    java.util.Arrays.fill(ab, ivx.index0(ab), ivx.indexN(ab), x)
  inline def fillRange(inline rg: collection.immutable.Range)(x: Byte): Unit =
    val iv = Iv of rg
    java.util.Arrays.fill(ab, iv.i0, iv.iN, x)

  inline def sort(): Unit =
    java.util.Arrays.sort(ab)
  inline def sortRange(i0: Int, iN: Int): Unit =
    java.util.Arrays.sort(ab, i0, iN)
  inline def sortRange(ivx: Iv.X): Unit =
    java.util.Arrays.sort(ab, ivx.index0(ab), ivx.indexN(ab))
  inline def sortRange(inline rg: collection.immutable.Range): Unit =
    val iv = Iv of rg
    java.util.Arrays.sort(ab, iv.i0, iv.iN)

  inline def isSorted: Boolean =
    isSortedRange(0, ab.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ab(i-1) <= ab(i) do i += 1
      i >= iN
  inline def isSortedRange(ivx: Iv.X): Boolean =
    isSortedRange(ivx.index0(ab), ivx.indexN(ab))
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
  inline def copyToSize(endpoint: End.At): Array[Short] =
    java.util.Arrays.copyOf(as, 1 + (endpoint of as))

  inline def copyOfRange(i0: Int, iN: Int): Array[Short] =
    java.util.Arrays.copyOfRange(as, i0, iN)
  inline def copyOfRange(ivx: Iv.X): Array[Short] =
    java.util.Arrays.copyOfRange(as, ivx.index0(as), ivx.indexN(as))
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Short] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(as, iv.i0, iv.iN)

  inline def search(x: Short): Int =
    java.util.Arrays.binarySearch(as, x)
  inline def searchRange(i0: Int, iN: Int)(x: Short): Int =
    java.util.Arrays.binarySearch(as, i0, iN, x)
  inline def searchRange(ivx: Iv.X)(x: Short): Int =
    java.util.Arrays.binarySearch(as, ivx.index0(as), ivx.indexN(as), x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Short): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(as, iv.i0, iv.iN, x)


  inline def fill(x: Short): Unit =
    java.util.Arrays.fill(as, x)
  inline def fillRange(i0: Int, iN: Int)(x: Short): Unit = 
    java.util.Arrays.fill(as, i0, iN, x)
  inline def fillRange(ivx: Iv.X)(x: Short): Unit =
    java.util.Arrays.fill(as, ivx.index0(as), ivx.indexN(as), x)
  inline def fillRange(inline rg: collection.immutable.Range)(x: Short): Unit =
    val iv = Iv of rg
    java.util.Arrays.fill(as, iv.i0, iv.iN, x)

  inline def sort(): Unit =
    java.util.Arrays.sort(as)
  inline def sortRange(i0: Int, iN: Int): Unit =
    java.util.Arrays.sort(as, i0, iN)
  inline def sortRange(ivx: Iv.X): Unit =
    java.util.Arrays.sort(as, ivx.index0(as), ivx.indexN(as))
  inline def sortRange(inline rg: collection.immutable.Range): Unit =
    val iv = Iv of rg
    java.util.Arrays.sort(as, iv.i0, iv.iN)

  inline def isSorted: Boolean =
    isSortedRange(0, as.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && as(i-1) <= as(i) do i += 1
      i >= iN
  inline def isSortedRange(ivx: Iv.X): Boolean =
    isSortedRange(ivx.index0(as), ivx.indexN(as))
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
  inline def copyToSize(endpoint: End.At): Array[Char] =
    java.util.Arrays.copyOf(ac, 1 + (endpoint of ac))

  inline def copyOfRange(i0: Int, iN: Int): Array[Char] =
    java.util.Arrays.copyOfRange(ac, i0, iN)
  inline def copyOfRange(ivx: Iv.X): Array[Char] =
    java.util.Arrays.copyOfRange(ac, ivx.index0(ac), ivx.indexN(ac))
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Char] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(ac, iv.i0, iv.iN)

  inline def search(x: Char): Int =
    java.util.Arrays.binarySearch(ac, x)
  inline def searchRange(i0: Int, iN: Int)(x: Char): Int =
    java.util.Arrays.binarySearch(ac, i0, iN, x)
  inline def searchRange(ivx: Iv.X)(x: Char): Int =
    java.util.Arrays.binarySearch(ac, ivx.index0(ac), ivx.indexN(ac), x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Char): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(ac, iv.i0, iv.iN, x)


  inline def fill(x: Char): Unit =
    java.util.Arrays.fill(ac, x)
  inline def fillRange(i0: Int, iN: Int)(x: Char): Unit = 
    java.util.Arrays.fill(ac, i0, iN, x)
  inline def fillRange(ivx: Iv.X)(x: Char): Unit =
    java.util.Arrays.fill(ac, ivx.index0(ac), ivx.indexN(ac), x)
  inline def fillRange(inline rg: collection.immutable.Range)(x: Char): Unit =
    val iv = Iv of rg
    java.util.Arrays.fill(ac, iv.i0, iv.iN, x)

  inline def sort(): Unit =
    java.util.Arrays.sort(ac)
  inline def sortRange(i0: Int, iN: Int): Unit =
    java.util.Arrays.sort(ac, i0, iN)
  inline def sortRange(ivx: Iv.X): Unit =
    java.util.Arrays.sort(ac, ivx.index0(ac), ivx.indexN(ac))
  inline def sortRange(inline rg: collection.immutable.Range): Unit =
    val iv = Iv of rg
    java.util.Arrays.sort(ac, iv.i0, iv.iN)

  inline def isSorted: Boolean =
    isSortedRange(0, ac.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ac(i-1) <= ac(i) do i += 1
      i >= iN
  inline def isSortedRange(ivx: Iv.X): Boolean =
    isSortedRange(ivx.index0(ac), ivx.indexN(ac))
  inline def isSortedRange(inline rg: collection.immutable.Range): Boolean =
    val iv = Iv of rg
    isSortedRange(iv.i0, iv.iN)
}

/** Int Array specific functionality from java.util.Arrays and java.lang.System */
extension (ai: Array[Int]) {
  def clippedTo(i0: Int, iN: Int): Array[Int] =
    var m = 0
    var i = 0
    while i < ai.length do
      val x = ai(i)
      if x >= i0 && x < iN then m += 1
      i += 1
    if m == ai.length then ai
    else
      val b = new Array[Int](m)
      i = 0
      var j = 0
      boundary:
        while i < ai.length do
          val x = ai(i)
          if x >= i0 && x < iN then
            b(j) = x
            j += 1
            if j >= b.length then boundary.break()
          i += 1
      b
  inline def clippedTo(iv: Iv): Array[Int] = clippedTo(iv.i0, iv.iN)
  inline def clippedTo(inline rg: Range): Array[Int] = { val iv = Iv of rg; clippedTo(iv.i0, iv.iN) }
  inline def clippedTo[A](that: Array[A]): Array[Int] = clippedTo(0, that.length)
  inline def clippedTo(that: String): Array[Int] = clippedTo(0, that.length)

  inline def unpackBytes: Array[Byte] = ArrayReform.toBytes(ai)

  inline def shrinkCopy(size: Int): Array[Int] = if size < ai.length then java.util.Arrays.copyOf(ai, size) else ai

  inline def copyToSize(size: Int): Array[Int] =
    java.util.Arrays.copyOf(ai, size)
  @targetName("copyToEndpointSize")
  inline def copyToSize(endpoint: End.At): Array[Int] =
    java.util.Arrays.copyOf(ai, 1 + (endpoint of ai))

  inline def copyOfRange(i0: Int, iN: Int): Array[Int] =
    java.util.Arrays.copyOfRange(ai, i0, iN)
  inline def copyOfRange(ivx: Iv.X): Array[Int] =
    java.util.Arrays.copyOfRange(ai, ivx.index0(ai), ivx.indexN(ai))
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Int] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(ai, iv.i0, iv.iN)

  inline def search(x: Int): Int =
    java.util.Arrays.binarySearch(ai, x)
  inline def searchRange(i0: Int, iN: Int)(x: Int): Int =
    java.util.Arrays.binarySearch(ai, i0, iN, x)
  inline def searchRange(ivx: Iv.X)(x: Int): Int =
    java.util.Arrays.binarySearch(ai, ivx.index0(ai), ivx.indexN(ai), x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Int): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(ai, iv.i0, iv.iN, x)

  inline def fill(x: Int): Unit =
    java.util.Arrays.fill(ai, x)
  inline def fillRange(i0: Int, iN: Int)(x: Int): Unit = 
    java.util.Arrays.fill(ai, i0, iN, x)
  inline def fillRange(ivx: Iv.X)(x: Int): Unit =
    java.util.Arrays.fill(ai, ivx.index0(ai), ivx.indexN(ai), x)
  inline def fillRange(inline rg: collection.immutable.Range)(x: Int): Unit =
    val iv = Iv of rg
    java.util.Arrays.fill(ai, iv.i0, iv.iN, x)

  inline def sort(): Unit =
    java.util.Arrays.sort(ai)
  inline def sortRange(i0: Int, iN: Int): Unit =
    java.util.Arrays.sort(ai, i0, iN)
  inline def sortRange(ivx: Iv.X): Unit =
    java.util.Arrays.sort(ai, ivx.index0(ai), ivx.indexN(ai))
  inline def sortRange(inline rg: collection.immutable.Range): Unit =
    val iv = Iv of rg
    java.util.Arrays.sort(ai, iv.i0, iv.iN)

  inline def isSorted: Boolean =
    isSortedRange(0, ai.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ai(i-1) <= ai(i) do i += 1
      i >= iN
  inline def isSortedRange(ivx: Iv.X): Boolean =
    isSortedRange(ivx.index0(ai), ivx.indexN(ai))
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
  inline def copyToSize(endpoint: End.At): Array[Long] =
    java.util.Arrays.copyOf(al, 1 + (endpoint of al))

  inline def copyOfRange(i0: Int, iN: Int): Array[Long] =
    java.util.Arrays.copyOfRange(al, i0, iN)
  inline def copyOfRange(ivx: Iv.X): Array[Long] =
    java.util.Arrays.copyOfRange(al, ivx.index0(al), ivx.indexN(al))
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Long] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(al, iv.i0, iv.iN)

  inline def search(x: Long): Int =
    java.util.Arrays.binarySearch(al, x)
  inline def searchRange(i0: Int, iN: Int)(x: Long): Int =
    java.util.Arrays.binarySearch(al, i0, iN, x)
  inline def searchRange(ivx: Iv.X)(x: Long): Int =
    java.util.Arrays.binarySearch(al, ivx.index0(al), ivx.indexN(al), x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Long): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(al, iv.i0, iv.iN, x)


  inline def fill(x: Long): Unit =
    java.util.Arrays.fill(al, x)
  inline def fillRange(i0: Int, iN: Int)(x: Long): Unit = 
    java.util.Arrays.fill(al, i0, iN, x)
  inline def fillRange(ivx: Iv.X)(x: Long): Unit =
    java.util.Arrays.fill(al, ivx.index0(al), ivx.indexN(al), x)
  inline def fillRange(inline rg: collection.immutable.Range)(x: Long): Unit =
    val iv = Iv of rg
    java.util.Arrays.fill(al, iv.i0, iv.iN, x)

  inline def sort(): Unit =
    java.util.Arrays.sort(al)
  inline def sortRange(i0: Int, iN: Int): Unit =
    java.util.Arrays.sort(al, i0, iN)
  inline def sortRange(ivx: Iv.X): Unit =
    java.util.Arrays.sort(al, ivx.index0(al), ivx.indexN(al))
  inline def sortRange(inline rg: collection.immutable.Range): Unit =
    val iv = Iv of rg
    java.util.Arrays.sort(al, iv.i0, iv.iN)

  inline def isSorted: Boolean =
    isSortedRange(0, al.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && al(i-1) <= al(i) do i += 1
      i >= iN
  inline def isSortedRange(ivx: Iv.X): Boolean =
    isSortedRange(ivx.index0(al), ivx.indexN(al))
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
  inline def copyToSize(endpoint: End.At): Array[Float] =
    java.util.Arrays.copyOf(af, 1 + (endpoint of af))

  inline def copyOfRange(i0: Int, iN: Int): Array[Float] =
    java.util.Arrays.copyOfRange(af, i0, iN)
  inline def copyOfRange(ivx: Iv.X): Array[Float] =
    java.util.Arrays.copyOfRange(af, ivx.index0(af), ivx.indexN(af))
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Float] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(af, iv.i0, iv.iN)

  inline def search(x: Float): Int =
    java.util.Arrays.binarySearch(af, x)
  inline def searchRange(i0: Int, iN: Int)(x: Float): Int =
    java.util.Arrays.binarySearch(af, i0, iN, x)
  inline def searchRange(ivx: Iv.X)(x: Float): Int =
    java.util.Arrays.binarySearch(af, ivx.index0(af), ivx.indexN(af), x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Float): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(af, iv.i0, iv.iN, x)


  inline def fill(x: Float): Unit =
    java.util.Arrays.fill(af, x)
  inline def fillRange(i0: Int, iN: Int)(x: Float): Unit = 
    java.util.Arrays.fill(af, i0, iN, x)
  inline def fillRange(ivx: Iv.X)(x: Float): Unit =
    java.util.Arrays.fill(af, ivx.index0(af), ivx.indexN(af), x)
  inline def fillRange(inline rg: collection.immutable.Range)(x: Float): Unit =
    val iv = Iv of rg
    java.util.Arrays.fill(af, iv.i0, iv.iN, x)

  inline def sort(): Unit =
    java.util.Arrays.sort(af)
  inline def sortRange(i0: Int, iN: Int): Unit =
    java.util.Arrays.sort(af, i0, iN)
  inline def sortRange(ivx: Iv.X): Unit =
    java.util.Arrays.sort(af, ivx.index0(af), ivx.indexN(af))
  inline def sortRange(inline rg: collection.immutable.Range): Unit =
    val iv = Iv of rg
    java.util.Arrays.sort(af, iv.i0, iv.iN)

  inline def isSorted: Boolean =
    isSortedRange(0, af.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && af(i-1) <= af(i) do i += 1
      i >= iN
  inline def isSortedRange(ivx: Iv.X): Boolean =
    isSortedRange(ivx.index0(af), ivx.indexN(af))
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
  inline def copyToSize(endpoint: End.At): Array[Double] =
    java.util.Arrays.copyOf(ad, 1 + (endpoint of ad))

  inline def copyOfRange(i0: Int, iN: Int): Array[Double] =
    java.util.Arrays.copyOfRange(ad, i0, iN)
  inline def copyOfRange(ivx: Iv.X): Array[Double] =
    java.util.Arrays.copyOfRange(ad, ivx.index0(ad), ivx.indexN(ad))
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[Double] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(ad, iv.i0, iv.iN)

  inline def search(x: Double): Int =
    java.util.Arrays.binarySearch(ad, x)
  inline def searchRange(i0: Int, iN: Int)(x: Double): Int =
    java.util.Arrays.binarySearch(ad, i0, iN, x)
  inline def searchRange(ivx: Iv.X)(x: Double): Int =
    java.util.Arrays.binarySearch(ad, ivx.index0(ad), ivx.indexN(ad), x)
  inline def searchRange(inline rg: collection.immutable.Range)(x: Double): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(ad, iv.i0, iv.iN, x)


  inline def fill(x: Double): Unit =
    java.util.Arrays.fill(ad, x)
  inline def fillRange(i0: Int, iN: Int)(x: Double): Unit = 
    java.util.Arrays.fill(ad, i0, iN, x)
  inline def fillRange(ivx: Iv.X)(x: Double): Unit =
    java.util.Arrays.fill(ad, ivx.index0(ad), ivx.indexN(ad), x)
  inline def fillRange(inline rg: collection.immutable.Range)(x: Double): Unit =
    val iv = Iv of rg
    java.util.Arrays.fill(ad, iv.i0, iv.iN, x)

  inline def sort(): Unit =
    java.util.Arrays.sort(ad)
  inline def sortRange(i0: Int, iN: Int): Unit =
    java.util.Arrays.sort(ad, i0, iN)
  inline def sortRange(ivx: Iv.X): Unit =
    java.util.Arrays.sort(ad, ivx.index0(ad), ivx.indexN(ad))
  inline def sortRange(inline rg: collection.immutable.Range): Unit =
    val iv = Iv of rg
    java.util.Arrays.sort(ad, iv.i0, iv.iN)

  inline def isSorted: Boolean =
    isSortedRange(0, ad.length)
  def isSortedRange(i0: Int, iN: Int): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && ad(i-1) <= ad(i) do i += 1
      i >= iN
  inline def isSortedRange(ivx: Iv.X): Boolean =
    isSortedRange(ivx.index0(ad), ivx.indexN(ad))
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
  inline def copyToSize(endpoint: End.At): Array[A] =
    java.util.Arrays.copyOf(aa, 1 + (endpoint of aa))

  inline def copyOfRange(i0: Int, iN: Int): Array[A] =
    java.util.Arrays.copyOfRange(aa, i0, iN)
  inline def copyOfRange(ivx: Iv.X): Array[A] =
    java.util.Arrays.copyOfRange(aa, ivx.index0(aa), ivx.indexN(aa))
  inline def copyOfRange(inline rg: collection.immutable.Range): Array[A] =
    val iv = Iv of rg
    java.util.Arrays.copyOfRange(aa, iv.i0, iv.iN)

  inline def search(x: A)(using o: scala.math.Ordering[A]): Int =
    java.util.Arrays.binarySearch(aa, x, o)
  inline def searchRange(i0: Int, iN: Int)(x: A)(using o: scala.math.Ordering[A]): Int =
    java.util.Arrays.binarySearch(aa, i0, iN, x, o)
  inline def searchRange(ivx: Iv.X)(x: A)(using o: scala.math.Ordering[A]): Int =
    java.util.Arrays.binarySearch(aa, ivx.index0(aa), ivx.indexN(aa), x, o)
  inline def searchRange(inline rg: collection.immutable.Range)(x: A)(using o: scala.math.Ordering[A]): Int =
    val iv = Iv of rg
    java.util.Arrays.binarySearch(aa, iv.i0, iv.iN, x, o)


  inline def fill(x: A): Unit =
    java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], x.asInstanceOf[AnyRef])
  inline def fillRange(i0: Int, iN: Int)(x: A): Unit = 
    java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], i0, iN, x.asInstanceOf[AnyRef])
  inline def fillRange(ivx: Iv.X)(x: A): Unit =
    java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], ivx.index0(aa), ivx.indexN(aa), x.asInstanceOf[AnyRef])
  inline def fillRange(inline rg: collection.immutable.Range)(x: A): Unit =
    val iv = Iv of rg
    java.util.Arrays.fill(aa.asInstanceOf[Array[AnyRef]], iv.i0, iv.iN, x.asInstanceOf[AnyRef])

  inline def sort()(using o: scala.math.Ordering[A]): Unit =
    scala.util.Sorting.stableSort(aa)
  inline def sortRange(i0: Int, iN: Int)(using o: scala.math.Ordering[A]): Unit =
    scala.util.Sorting.stableSort(aa, i0, iN)
  inline def sortRange(ivx: Iv.X)(using o: scala.math.Ordering[A]): Unit =
    scala.util.Sorting.stableSort(aa, ivx.index0(aa), ivx.indexN(aa))
  inline def sortRange(inline rg: collection.immutable.Range)(using o: scala.math.Ordering[A]): Unit =
    val iv = Iv of rg
    scala.util.Sorting.stableSort(aa, iv.i0, iv.iN)

  inline def isSorted(using o: scala.math.Ordering[A]): Boolean =
    isSortedRange(0, aa.length)
  def isSortedRange(i0: Int, iN: Int)(using o: scala.math.Ordering[A]): Boolean =
    if i0 >= iN then true
    else
      var i = i0 + 1
      while i < iN && o.compare(aa(i-1), aa(i)) <= 0 do i += 1
      i >= iN
  inline def isSortedRange(ivx: Iv.X)(using o: scala.math.Ordering[A]): Boolean =
    isSortedRange(ivx.index0(aa), ivx.indexN(aa))
  inline def isSortedRange(inline rg: collection.immutable.Range)(using o: scala.math.Ordering[A]): Boolean =
    val iv = Iv of rg
    isSortedRange(iv.i0, iv.iN)
}



/** Higher-level high-speed String access (inlined) */
extension (a: String) {
  inline def clip: kse.basics.ClippedString = ClippedString wrap a

  inline def flex: kse.basics.FlexString = FlexString wrap a

  inline def fancy: kse.basics.FancyString = FancyString wrap a

  inline def use()(inline f: Char => Unit): Unit =
    var i = 0
    while i < a.length do
      f(a.charAt(i))
      i += 1
  inline def use(i0: Int, iN: Int)(inline f: Char => Unit): Unit =
    var i = i0
    while i < iN do
      f(a.charAt(i))
      i += 1
  inline def use(ivx: Iv.X)(inline f: Char => Unit): Unit =
    use(ivx.index0(a), ivx.indexN(a))(f)
  inline def use(inline rg: collection.immutable.Range)(inline f: Char => Unit): Unit =
    val iv = Iv of rg
    use(iv.i0, iv.iN)(f)
  inline def use(indices: Array[Int])(inline f: Char => Unit): Unit =
    var i = 0
    while i < indices.length do
      f(a.charAt(indices(i)))
      i += 1
  inline def use(indices: scala.collection.IntStepper)(inline f: Char => Unit): Unit =
    while indices.hasStep do
      f(a.charAt(indices.nextStep))
  inline def use(inline p: Char => Boolean)(inline f: Char => Unit): Unit =
    var i = 0
    while i < a.length do
      val c = a.charAt(i)
      if p(c) then f(c)
      i += 1

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
  inline def visit(ivx: Iv.X)(inline f: (Char, Int) => Unit): Unit =
    visit(ivx.index0(a), ivx.indexN(a))(f)
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
  inline def visit(inline p: Char => Boolean)(inline f: (Char, Int) => Unit): Unit =
    var i = 0
    while i < a.length do
      val c = a.charAt(i)
      if p(c) then f(c, i)
      i += 1

  inline def visitLineIvs(newlines: Boolean)(inline f: Iv => Unit): Unit =
    var i = 0
    var j = -1
    val n = a.length()
    while i < n do
      j = i + 1
      while j < n && a.charAt(j) != '\n' do j += 1
      val k = if j < n then j + 1 else n
      if !newlines then
        if j > i && a.charAt(j-1) == '\r' then j = j - 1
      else if j < n then j = j + 1
      f(Iv(i, j))
      i = k
  inline def visitLineIvs(inline f: Iv => Unit): Unit =
    visitLineIvs(true)(f)

  inline def pairs(inline f: (Char, Char) => Unit): Unit =
    if a.length > 0 then
      var a0 = a.charAt(0)
      var i = 1
      while i < a.length do
        val a1 = a.charAt(i)
        f(a0, a1)
        a0 = a1
        i += 1
   inline def trios(inline f: (Char, Char, Char) => Unit): Unit =
    if a.length > 1 then
      var a0 = a.charAt(0)
      var a1 = a.charAt(1)
      var i = 2
      while i < a.length do
        val a2 = a.charAt(i)
        f(a0, a1, a2)
        a0 = a1
        a1 = a2
        i += 1

  inline def together[B](b: Array[B])(inline f: (Char, B, Int) => Unit): Unit =
    val n = if a.length > b.length then b.length else a.length
    var i = 0
    while i < n do
      f(a.charAt(i), b(i), i)
      i += 1
  inline def together(b: String)(inline f: (Char, Char, Int) => Unit): Unit =
    val n = if a.length > b.length then b.length else a.length
    var i = 0
    while i < n do
      f(a.charAt(i), b.charAt(i), i)
      i += 1
  inline def together[B, C](b: Array[B], c: Array[C])(inline f: (Char, B, C, Int) => Unit): Unit =
    var n = a.length
    if b.length < n then n = b.length
    if c.length < n then n = c.length
    var i = 0
    while i < n do
      f(a.charAt(i), b(i), c(i), i)
      i += 1
  inline def together[B](b: Array[B], c: String)(inline f: (Char, B, Char, Int) => Unit): Unit =
    var n = a.length
    if b.length < n then n = b.length
    if c.length < n then n = c.length
    var i = 0
    while i < n do
      f(a.charAt(i), b(i), c.charAt(i), i)
      i += 1
  inline def together[C](b: String, c: Array[C])(inline f: (Char, Char, C, Int) => Unit): Unit =
    var n = a.length
    if b.length < n then n = b.length
    if c.length < n then n = c.length
    var i = 0
    while i < n do
      f(a.charAt(i), b.charAt(i), c(i), i)
      i += 1
  inline def together(b: String, c: String)(inline f: (Char, Char, Char, Int) => Unit): Unit =
    var n = a.length
    if b.length < n then n = b.length
    if c.length < n then n = c.length
    var i = 0
    while i < n do
      f(a.charAt(i), b.charAt(i), c.charAt(i), i)
      i += 1

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
  inline def gather[Z](zero: Z)(ivx: Iv.X)(inline f: (Z, Char, Int) => Z): Z =
    gather(zero)(ivx.index0(a), ivx.indexN(a))(f)
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

  def where(): Array[Int] =
    var ix = new Array[Int](a.length)
    var i = 0
    while i < ix.length do
      ix(i) = i
      i += 1
    ix
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
  inline def whereOp(inline pick: (Char, Int) => Int): Array[Int] =
    var ix = new Array[Int](if a.length <= 8 then a.length else 8)
    var i = 0
    var j = 0
    while i < a.length do
      val h = pick(a.charAt(i), i)
      if h >= 0 then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = h
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
  inline def whereIn(ivx: Iv.X)(inline pick: Char => Boolean): Array[Int] =
    whereIn(ivx.index0(a), ivx.indexN(a))(pick)
  inline def whereIn(inline rg: Range)(inline pick: Char => Boolean): Array[Int] =
    val iv = Iv of rg
    whereIn(iv.i0, iv.iN)(pick)
  inline def whereInOp(i0: Int, iN: Int)(inline pick: (Char, Int) => Int): Array[Int] =
    var ix = new Array[Int](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else iN - i0)
    var i = i0
    var j = 0
    while i < iN do
      val h = pick(a.charAt(i), i)
      if h >= 0 then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = h
        j += 1
      i += 1
    ix.shrinkTo(j)
  inline def whereInOp(ivx: Iv.X)(inline pick: (Char, Int) => Int): Array[Int] =
    whereInOp(ivx.index0(a), ivx.indexN(a))(pick)
  inline def whereInOp(inline rg: Range)(inline pick: (Char, Int) => Int): Array[Int] =
    val iv = Iv of rg
    whereInOp(iv.i0, iv.iN)(pick)

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
  inline def whereFromOp(indices: Array[Int])(inline pick: (Char, Int) => Int): Array[Int] =
    var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
    var i = 0
    var j = 0
    while i < indices.length do
      val k = indices(i)
      val h = pick(a.charAt(k), k)
      if h >= 0 then
        if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
        ix(j) = h
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
  inline def inject(that: Array[Char])(ivx: Iv.X): Int =
    inject(that, 0)(ivx.index0(a), ivx.indexN(a))
  inline def inject(that: Array[Char], where: Int)(ivx: Iv.X): Int =
    inject(that, where)(ivx.index0(a), ivx.indexN(a))
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
  inline def injectOp[B](that: Array[B])(ivx: Iv.X)(inline f: (Char, Int) => B): Int =
    injectOp[B](that, 0)(ivx.index0(a), ivx.indexN(a))(f)
  inline def injectOp[B](that: Array[B], where: Int)(ivx: Iv.X)(inline f: (Char, Int) => B): Int =
    injectOp[B](that, where)(ivx.index0(a), ivx.indexN(a))(f)
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
  inline def select(ivx: Iv.X): String =
    select(ivx.index0(a), ivx.indexN(a))
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
        b.append(x): Unit
      i += 1
    b.toString

  inline def selectOp[B](i0: Int, iN: Int)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](iN - i0)
    var i = i0
    while i < iN do
      b(i - i0) = op(a.charAt(i), i)
      i += 1
    b
  inline def selectOp[B](ivx: Iv.X)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    selectOp(ivx.index0(a), ivx.indexN(a))(op)
  inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    val iv = Iv of rg
    selectOp(iv.i0, iv.iN)(op)
  inline def selectOp[B](indices: Array[Int])(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    val b = new Array[B](indices.length)
    var i = 0
    while i < indices.length do
      val j = indices(i)
      b(i) = op(a.charAt(j), j)
      i += 1
    b
  inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
    var b = new Array[B](if a.length <= 8 then a.length else 8)
    var j = 0
    while indices.hasStep do
      if j >= b.length then b = b.enlargeTo(b.length | (b.length << 1))
      val i = indices.nextStep
      b(j) = op(a.charAt(i), i)
      j += 1
    b.shrinkTo(j)
  inline def selectOp[B](inline pick: Char => Boolean)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
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

  inline def diced(cut: Char => Boolean, mode: "" | "()" | "[)" | "(]" | "[]"): Array[String] =
    var bss: Array[String] = null
    var k = 0
    var i0: Int = inline mode match
      case "[)" | "[]" => 0
      case _           => -1
    var j = 0
    while j <= a.length do
      if j == a.length || cut(a.charAt(j)) then
        val iN = inline mode match
          case "[]" | "(]" =>
            if j < a.length then j+1
            else j
          case _ => j
        val n = inline mode match
          case "[]" | "[)" => iN - i0
          case "" =>
            i0 += 1
            if i0 == iN then -1 else iN - i0
          case _ =>
            i0 += 1
            iN - i0
        if n >= 0 then
          val b = if n == 0 then "" else a.substring(i0, iN)
          if bss eq null then
            bss = new Array[String](if j == a.length then 1 else 8)
          else if k >= bss.length then
            val temp = new Array[String](if j == a.length then k+1 else bss.length | (bss.length << 1))
            System.arraycopy(bss, 0, temp, 0, bss.length)
            bss = temp
          bss(k) = b
          k += 1
        i0 = j
      j += 1
    if bss eq null then new Array[String](0)
    else if k < bss.length then java.util.Arrays.copyOf(bss, k)
    else bss
  inline def diced(cut: Char => Boolean): Array[String] = diced(cut, "")

  inline def visitCuts()(inline cut: (Char, Char) => Boolean)(inline f: (Int, Int) => Unit): Unit =
    visitCuts(0, a.length)(cut)(f)
  inline def visitCuts(i0: Int, iN: Int)(inline cut: (Char, Char) => Boolean)(inline f: (Int, Int) => Unit): Unit =
    var i = i0
    while i < iN do
      var x = a(i)
      var j = i + 1
      var continue = true
      while continue && j < iN do
        val y = a(j)
        if cut(x, y) then continue = false
        else x = y
        j += 1
      f(i, j)
      i = j
  inline def visitCuts(ivx: Iv.X)(inline cut: (Char, Char) => Boolean)(inline f: (Int, Int) => Unit): Unit =
    visitCuts(ivx.index0(a), ivx.indexN(a))(cut)(f)
  inline def visitCuts(inline rg: Rg)(inline cut: (Char, Char) => Boolean)(inline f: (Int, Int) => Unit): Unit =
    val iv = Iv of rg
    visitCuts(iv.i0, iv.iN)(cut)(f)


  inline def visitLineIndices()(inline f: (Int, Int) => Unit): Unit =
    visitLineIndices(0, a.length)(f)
  inline def visitLineIndices(i0: Int, iN: Int)(inline f: (Int, Int) => Unit): Unit =
    var i = i0
    while i < iN do
      var j = i
      while j < iN && a.charAt(j) != '\n' do j += 1
      if j < iN then j += 1
      f(i, j)
      i = j
  inline def visitLineIndices(ivx: Iv.X)(inline f: (Int, Int) => Unit): Unit =
    visitLineIndices(ivx.index0(a), ivx.indexN(a))(f)
  inline def visitLineIndices(inline rg: Rg)(inline f: (Int, Int) => Unit): Unit =
    val iv = Iv of rg
    visitLineIndices(iv.i0, iv.iN)(f)
}


opaque type ClippedString = String
object ClippedString {
  inline def wrap(a: String): ClippedString = a

  extension (ca: ClippedString)
    inline def unwrap: String = ca

  extension (ca: kse.basics.ClippedString) {
    inline def flex: kse.basics.FancyString = FancyString wrap ca.unwrap

    inline def apply(i: Int)(inline c: => Char): Char =
      val a = ca.unwrap
      if i >= 0 && i < a.length then a.charAt(i)
      else c

    inline def get(i: Int): Option[Char] =
      val a = ca.unwrap
      if i >= 0 && i < a.length then Some(a.charAt(i))
      else None

    inline def use(i: Int)(inline f: Char => Unit): Unit =
      val a = ca.unwrap
      if i >= 0 && i < a.length then f(a.charAt(i))
    inline def use(i0: Int, iN: Int)(inline f: Char => Unit): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      while i < iM do
        f(a.charAt(i))
        i += 1
    inline def use(ivx: Iv.X)(inline f: Char => Unit): Unit =
      val iv = ivx of ca.unwrap
      use(iv.i0, iv.iN)(f)
    inline def use(inline rg: collection.immutable.Range)(inline f: Char => Unit): Unit =
      val iv = Iv of rg
      use(iv.i0, iv.iN)(f)
    inline def use(indices: Array[Int])(inline f: Char => Unit): Unit =
      val a = ca.unwrap
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < a.length then f(a.charAt(j))
        i += 1
    inline def use(indices: scala.collection.IntStepper)(inline f: Char => Unit): Unit =
      val a = ca.unwrap
      while indices.hasStep do
        val j = indices.nextStep
        if j >= 0 && j < a.length then f(a.charAt(j))

    inline def visit(i0: Int, iN: Int)(inline f: (Char, Int) => Unit): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      while i < iM do
        f(a.charAt(i), i)
        i += 1
    inline def visit(ivx: Iv.X)(inline f: (Char, Int) => Unit): Unit =
      val iv = ivx of ca.unwrap
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
    inline def gather[Z](zero: Z)(ivx: Iv.X)(inline f: (Z, Char, Int) => Z): Z =
      val iv = ivx of ca.unwrap
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
    inline def whereIn(ivx: Iv.X)(inline pick: Char => Boolean): Array[Int] =
      val iv = ivx of ca.unwrap
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: Char => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereInOp(i0: Int, iN: Int)(inline pick: (Char, Int) => Int): Array[Int] =
      val a = ca.unwrap
      var i = i0
      if i0 < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var ix = new Array[Int](if iM - i < 0 then 0 else if iM - i > 8 then 8 else iM - i)
      var j = 0
      while i < iM do
        val h = pick(a.charAt(i), i)
        if h >= 0 then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = h
          j += 1
        i += 1
      ix.shrinkTo(j)
    inline def whereInOp(ivx: Iv.X)(inline pick: (Char, Int) => Int): Array[Int] =
      val iv = ivx of ca.unwrap
      whereInOp(iv.i0, iv.iN)(pick)
    inline def whereInOp(inline rg: Range)(inline pick: (Char, Int) => Int): Array[Int] =
      val iv = Iv of rg
      whereInOp(iv.i0, iv.iN)(pick)

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
    inline def whereFromOp(indices: Array[Int])(inline pick: (Char, Int) => Int): Array[Int] =
      val a = ca.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        if k >= 0 && k < a.length then
          val h = pick(a.charAt(k), k)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
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
    inline def inject(that: Array[Char])(ivx: Iv.X): Int =
      val iv = ivx of ca.unwrap
      inject(that, 0)(iv.i0, iv.iN)
    inline def inject(that: Array[Char], where: Int)(ivx: Iv.X): Int =
      val iv = ivx of ca.unwrap
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
    inline def injectOp[B](that: Array[B])(ivx: Iv.X)(inline f: (Char, Int) => B): Int =
      val iv = ivx of ca.unwrap
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(ivx: Iv.X)(inline f: (Char, Int) => B): Int =
      val iv = ivx of ca.unwrap
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
    inline def select(ivx: Iv.X): String =
      val iv = ivx of ca.unwrap
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
          b.append(a.charAt(k)): Unit
        i += 1
      b.toString
    inline def select(indices: scala.collection.IntStepper): String =
      val a = ca.unwrap
      var b = new java.lang.StringBuilder()
      while indices.hasStep do
        val i = indices.nextStep
        if i >= 0 && i < a.length then
          b.append(a.charAt(i)): Unit
      b.toString

    inline def selectOp[B](i0: Int, iN: Int)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](ivx: Iv.X)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = ivx of ca.unwrap
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](indices: Array[Int])(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: (Char, Int) => B)(using ClassTag[B]): Array[B] =
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

    inline def visitCuts(i0: Int, iN: Int)(inline cut: (Char, Char) => Boolean)(inline f: (Int, Int) => Unit): Unit =
      val a = ca.unwrap
      var i = i0
      if i0 < 0 then i = 0
      var l = a.length
      if iN < l then l = iN
      while i < l do
        var x = a.charAt(i)
        var j = i + 1
        var continue = true
        while continue && j < l do
          val y = a.charAt(j)
          if cut(x, y) then continue = false
          else x = y
          j += 1
        f(i, j)
        i = j
    inline def visitCuts(ivx: Iv.X)(inline cut: (Char, Char) => Boolean)(inline f: (Int, Int) => Unit): Unit =
      val a = ca.unwrap
      visitCuts(ivx.index0(a), ivx.indexN(a))(cut)(f)
    inline def visitCuts(inline rg: Rg)(inline cut: (Char, Char) => Boolean)(inline f: (Int, Int) => Unit): Unit =
      val iv = Iv of rg
      visitCuts(iv.i0, iv.iN)(cut)(f)

    inline def visitLineIndices(i0: Int, iN: Int)(inline f: (Int, Int) => Unit): Unit =
      val a = ca.unwrap
      var i = i0
      if i < 0 then i = 0
      var n = a.length
      if iN < n then n = iN
      while i < n do
        var j = i
        while j < n && a.charAt(j) != '\n' do j += 1
        if j < n then j += 1
        f(i, j)
        i = j
    inline def visitLineIndices(ivx: Iv.X)(inline f: (Int, Int) => Unit): Unit =
      val a = ca.unwrap
      visitLineIndices(ivx.index0(a), ivx.indexN(a))(f)
    inline def visitLineIndices(inline rg: Rg)(inline f: (Int, Int) => Unit): Unit =
      val iv = Iv of rg
      visitLineIndices(iv.i0, iv.iN)(f)
  }
}


opaque type FlexString = String
object FlexString {
  inline def wrap(a: String): FlexString = a

  extension (sa: FlexString)
    inline def unwrap: String = sa

  extension (sa: kse.basics.FlexString) {
    inline def clip: kse.basics.FancyString = FancyString wrap sa.unwrap

    inline def use()(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          f(a.charAt(i))
          i += 1
    inline def use(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val a = sa.unwrap
      var i = i0
      shortcut.quittable:
        while i < iN do
          f(a.charAt(i))
          i += 1
    inline def use(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val iv = ivx of sa.unwrap
      use(iv.i0, iv.iN)(f)
    inline def use(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val iv = Iv of rg
      use(iv.i0, iv.iN)(f)
    inline def use(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          f(a.charAt(indices(i)))
          i += 1
    inline def use(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          f(a.charAt(indices.nextStep))
    inline def use(inline p: Char => Boolean)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          val c = a.charAt(i)
          if p(c) then f(c)
          i += 1

    inline def visit()(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          f(a.charAt(i), i)
          i += 1
    inline def visit(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val a = sa.unwrap
      var i = i0
      shortcut.quittable:
        while i < iN do
          f(a.charAt(i), i)
          i += 1
    inline def visit(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val iv = ivx of sa.unwrap
      visit(iv.i0, iv.iN)(f)
    inline def visit(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val iv = Iv of rg
      visit(iv.i0, iv.iN)(f)
    inline def visit(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          f(a.charAt(j), j)
          i += 1
    inline def visit(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val a = sa.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          f(a.charAt(j), j)
    inline def visit(inline p: Char => Boolean)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val a = sa.unwrap
      var i = 0
      shortcut.quittable:
        while i < a.length do
          val c = a.charAt(i)
          if p(c) then f(c, i)
          i += 1

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
    inline def gather[Z](zero: Z)(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      val iv = ivx of sa.unwrap
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
    inline def gather[Z](zero: Z)(inline p: Char => Boolean)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z) =
      var z = zero
      val a = sa.unwrap
      shortcut.quittable:
        var i = 0
        while i < a.length do
          val c = a.charAt(i)
          if p(c) then z = f(z, c, i)
          i += 1
      z

    inline def copyWith[B](inline f: boundary.Label[shortcut.Type] ?=> Char => Char): String =
      val a = sa.unwrap
      val b = new java.lang.StringBuilder(a.length)
      var i = 0
      shortcut.outer:
        while i < a.length do
          shortcut.inner:
            b.append(f(a.charAt(i))): Unit
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
    inline def whereOp(inline pick: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Int): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if a.length <= 8 then a.length else 8)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < a.length do
          val h = pick(a.charAt(i), i)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
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
    inline def whereIn(ivx: Iv.X)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val iv =ivx of sa.unwrap
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereInOp(i0: Int, iN: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Int): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else iN - i0)
      var i = i0
      var j = 0
      shortcut.quittable:
        while i < iN do
          val h = pick(a.charAt(i), i)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
            j += 1
          i += 1
      ix.shrinkTo(j)
    inline def whereInOp(ivx: Iv.X)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Int): Array[Int] =
      val iv = ivx of sa.unwrap
      whereInOp(iv.i0, iv.iN)(pick)
    inline def whereInOp(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Int): Array[Int] =
      val iv = Iv of rg
      whereInOp(iv.i0, iv.iN)(pick)

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
    inline def whereFromOp(indices: Array[Int])(inline pick: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Int): Array[Int] =
      val a = sa.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < indices.length do
          val k = indices(i)
          val h = pick(a.charAt(k), k)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
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
    inline def injectOp[B](that: Array[B])(ivx: Iv.X)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = ivx of sa.unwrap
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(ivx: Iv.X)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = ivx of sa.unwrap
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
            b.append(x): Unit
          i += 1
      b.toString

    inline def selectOp[B](i0: Int, iN: Int)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](ivx: Iv.X)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = ivx of sa.unwrap
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](indices: Array[Int])(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
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


    inline def diced(cut: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean, mode: "" | "()" | "[)" | "(]" | "[]"): Array[String] =
      val a = sa.unwrap
      var bss: Array[String] = null
      var k = 0
      var i0: Int = inline mode match
        case "[)" | "[]" => 0
        case _           => -1
      var j = 0
      var unbroken = true
      while j <= a.length && unbroken do
        var cutme = j == a.length
        if !cutme then
          shortcut.quittable:
            unbroken = false
            cutme = cut(a.charAt(j))
            unbroken = true
        if cutme || !unbroken then
          val iN = inline mode match
            case "[]" | "(]" =>
              if j < a.length && unbroken then j+1
              else j
            case _ => j
          val n = inline mode match
            case "[]" | "[)" => iN - i0
            case "" =>
              i0 += 1
              if i0 == iN then -1 else iN - i0
            case _ =>
              i0 += 1
              iN - i0
          if n >= 0 then
            val b = if n == 0 then "" else a.substring(i0, iN)
            if bss eq null then
              bss = new Array[String](if j == a.length || !unbroken then 1 else 8)
            else if k >= bss.length then
              val temp = new Array[String](if j == a.length || !unbroken then k+1 else bss.length | (bss.length << 1))
              System.arraycopy(bss, 0, temp, 0, bss.length)
              bss = temp
            bss(k) = b
            k += 1
          i0 = j
        j += 1
      if bss eq null then new Array[String](0)
      else if k < bss.length then java.util.Arrays.copyOf(bss, k)
      else bss
    inline def diced(cut: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[String] = diced(cut, "")
  }
}


opaque type FancyString = String
object FancyString {
  inline def wrap(a: String): FancyString = a

  extension (sc: FancyString)
    inline def unwrap: String = sc

  extension (sc: kse.basics.FancyString) {
    inline def use(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      shortcut.quittable:
        while i < iM do
          f(a.charAt(i))
          i += 1
    inline def use(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val iv = ivx of sc.unwrap
      use(iv.i0, iv.iN)(f)
    inline def use(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val iv = Iv of rg
      use(iv.i0, iv.iN)(f)
    inline def use(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val a = sc.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then f(a.charAt(j))
          i += 1
    inline def use(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> Char => Unit): Unit =
      val a = sc.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then f(a.charAt(j))

    inline def visit(i0: Int, iN: Int)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      shortcut.quittable:
        while i < iM do
          f(a.charAt(i), i)
          i += 1
    inline def visit(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val iv = ivx of sc.unwrap
      visit(iv.i0, iv.iN)(f)
    inline def visit(inline rg: collection.immutable.Range)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val iv = Iv of rg
      visit(iv.i0, iv.iN)(f)
    inline def visit(indices: Array[Int])(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val a = sc.unwrap
      var i = 0
      shortcut.quittable:
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < a.length then f(a.charAt(j), j)
          i += 1
    inline def visit(indices: scala.collection.IntStepper)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Unit): Unit =
      val a = sc.unwrap
      shortcut.quittable:
        while indices.hasStep do
          val j = indices.nextStep
          if j >= 0 && j < a.length then f(a.charAt(j), j)

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
    inline def gather[Z](zero: Z)(ivx: Iv.X)(inline f: boundary.Label[shortcut.Quits.type] ?=> (Z, Char, Int) => Z): Z =
      val iv = ivx of sc.unwrap
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
    inline def whereIn(ivx: Iv.X)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val iv = ivx of sc.unwrap
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereIn(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> Char => Boolean): Array[Int] =
      val iv = Iv of rg
      whereIn(iv.i0, iv.iN)(pick)
    inline def whereInOp(i0: Int, iN: Int)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Int): Array[Int] =
      val a = sc.unwrap
      var i = i0
      if i < 0 then i = 0
      val iM = if iN > a.length then a.length else iN
      var ix = new Array[Int](if iM - i < 0 then 0 else if iM - i > 8 then 8 else iM - i)
      var j = 0
      shortcut.quittable:
        while i < iM do
          val h = pick(a.charAt(i), i)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
            j += 1
          i += 1
      ix.shrinkTo(j)
    inline def whereInOp(ivx: Iv.X)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Int): Array[Int] =
      val iv = ivx of sc.unwrap
      whereInOp(iv.i0, iv.iN)(pick)
    inline def whereInOp(inline rg: Range)(inline pick: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Int): Array[Int] =
      val iv = Iv of rg
      whereInOp(iv.i0, iv.iN)(pick)

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
    inline def whereFromOp(indices: Array[Int])(inline pick: boundary.Label[shortcut.Quits.type] ?=> (Char, Int) => Int): Array[Int] =
      val a = sc.unwrap
      var ix = new Array[Int](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      shortcut.quittable:
        while i < indices.length do
          val k = indices(i)
          if k >= 0 && k < a.length then
            val h = pick(a.charAt(k), k)
            if h >= 0 then
              if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
              ix(j) = h
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
    inline def injectOp[B](that: Array[B])(ivx: Iv.X)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = ivx of sc.unwrap
      injectOp[B](that, 0)(iv.i0, iv.iN)(f)
    inline def injectOp[B](that: Array[B], where: Int)(ivx: Iv.X)(inline f: boundary.Label[shortcut.Type] ?=> (Char, Int) => B): Int =
      val iv = ivx of sc.unwrap
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

    inline def selectOp[B](i0: Int, iN: Int)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](ivx: Iv.X)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = ivx of sc.unwrap
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](inline rg: collection.immutable.Range)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
      val iv = Iv of rg
      selectOp(iv.i0, iv.iN)(op)
    inline def selectOp[B](indices: Array[Int])(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
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
    inline def selectOp[B](indices: scala.collection.IntStepper)(inline op: boundary.Label[shortcut.Type] ?=> (Char, Int) => B)(using ClassTag[B]): Array[B] =
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

