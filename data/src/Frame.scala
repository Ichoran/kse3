// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences, LLC.

package kse.data


import java.lang.{Math => jm}

import kse.flow._
import kse.maths._

trait Stripe[+A, +D] {
  def label: A
  def length: Int
  def apply(i: Int): D
  inline def py(i: Int): D = if i < 0 then apply(length - i) else apply(i)
  inline def R(i: Int): D = apply(i - 1)
}
object Stripe {
  sealed trait Direct[D] {
    inline def size: Int
    inline def get(i: Int): D
    inline def pyGet(i: Int): D = if i < 0 then get(size - i) else get(i)
    inline def rGet(i: Int): D = get(i - 1)
  }

  trait Mut[A, D, S <: Direct[D]] extends Stripe[A, D] {
    def update(i: Int, value: D): Unit
    inline def py_=(i: Int, value: D): Unit = if i < 0 then update(length - i, value) else update(i, value)
    def direct: S
  }
  trait Grow[A, D, S <: Direct[D]] extends Mut[A, D, S] {
    def push(value: D): Unit
  }

  trait Stride[+A, C, +D] extends Stripe[A, D] {
    val data: C
    val start: Int
    val step: Int
    val count: Int

    protected final def checkValid(n: Int): Unit =
      if start < 0 || start >= n then throw new ArrayIndexOutOfBoundsException(s"$start is not a valid index into data of size $n")
      if step <= 0 then throw new IllegalArgumentException("step must be positive, not $step")
      if count < 1 then throw new IllegalArgumentException("strides must have at least one element, not $count")
      if (start + step.toLong * (count - 1)) > n then throw new IllegalArgumentException(s"Final element ${count-1} at index ${start + step.toLong * (count - 1)} is outside data of size $n")
  }

  abstract class OfInt[+A](val label: A) extends Stripe[A, Int] {}
  abstract class OfDouble[+A](val label: A) extends Stripe[A, Double] {}
  abstract class OfString[+A](val label: A) extends Stripe[A, String] {}

  final class Ints[+A](lb: A, data: Array[Int]) extends OfInt[A](lb) with Direct[Int] {
    inline def size = data.length
    inline def get(i: Int) = data(i)
    def length = data.length
    def apply(i: Int) = data(i)
    override def toString = data.mkString("$label: ", ", ", "")
  }
  final class GrowInts[A](lb: A) extends OfInt[A](lb) with Grow[A, Int, Ints[A]] {
    private var data: Array[Int] = new Array[Int](8)
    private var n: Int = 0
    def length: Int = n
    def apply(i: Int): Int =
      if i >= n then throw new ArrayIndexOutOfBoundsException(s"Index $i out of bounds for length $n")
      data(i)
    def update(i: Int, value: Int): Unit =
      if i >=n then throw new ArrayIndexOutOfBoundsException(s"Index $i out of bounds for length $n")
      data(i) = value
    def direct: Ints[A] = new Ints(label, data.shrinkCopy(n))
    def push(value: Int): Unit =
      if n >= data.length then data = data.copyToSize(n | (n << 1))
      data(n) = value
      n += 1
  }
  final class StrideI[+A](lb: A, val data: IArray[Int], val start: Int, val step: Int, val count: Int)
  extends OfInt[A](lb) with Stride[A, IArray[Int], Int] with Direct[Int] {
    checkValid(data.size)
    inline def size = count
    inline def get(i: Int) =
      if i < 0 || i >= count then throw new IllegalArgumentException("Stride index out of bounds: " + i.toString)
      data(start + step * i)
    def length = count
    def apply(i: Int) = get(i)
  }

  final class Doubles[+A](lb: A, data: Array[Double]) extends OfDouble[A](lb) with Direct[Double] {
    inline def size = data.length
    inline def get(i: Int) = data(i)
    def length = data.length
    def apply(i: Int) = data(i)
    override def toString = data.mkString("$label: ", ", ", "")
  }
  final class GrowDoubles[A](lb: A) extends OfDouble[A](lb) with Grow[A, Double, Doubles[A]] {
    private var data: Array[Double] = new Array[Double](8)
    private var n: Int = 0
    def length: Int = n
    def apply(i: Int): Double =
      if i >= n then throw new ArrayIndexOutOfBoundsException(s"Index $i out of bounds for length $n")
      data(i)
    def update(i: Int, value: Double): Unit =
      if i >=n then throw new ArrayIndexOutOfBoundsException(s"Index $i out of bounds for length $n")
      data(i) = value
    def direct: Doubles[A] = new Doubles(label, data.shrinkCopy(n))
    def push(value: Double): Unit =
      if n >= data.length then data = data.copyToSize(n | (n << 1))
      data(n) = value
      n += 1
  }
  final class StrideD[+A](lb: A, val data: IArray[Double], val start: Int, val step: Int, val count: Int)
  extends OfDouble[A](lb) with Stride[A, IArray[Double], Double] with Direct[Double] {
    checkValid(data.length)
    inline def size: Int = count
    inline def get(i: Int): Double =
      if i < 0 || i >= count then throw new IllegalArgumentException("Stride index out of bounds: " + i.toString)
      data(start + step * i)
    def length = count
    def apply(i: Int): Double = get(i)
  }

  final class Strings[+A](lb: A, data: Array[String]) extends OfString[A](lb) with Direct[String] {
    inline def size = data.length
    inline def get(i: Int) = data(i)
    def length = data.length
    def apply(i: Int) = data(i)
    override def toString = data.mkString("$label: ", ", ", "")
  }
  final class GrowStrings[A](lb: A) extends OfString[A](lb) with Grow[A, String, Strings[A]] {
    private var data: Array[String] = new Array[String](8)
    private var n: Int = 0
    def length: Int = n
    def apply(i: Int): String =
      if i >= n then throw new ArrayIndexOutOfBoundsException(s"Index $i out of bounds for length $n")
      data(i)
    def update(i: Int, value: String): Unit =
      if i >=n then throw new ArrayIndexOutOfBoundsException(s"Index $i out of bounds for length $n")
      data(i) = value
    def direct: Strings[A] = new Strings(label, data.shrinkCopy(n))
    def push(value:String): Unit =
      if n >= data.length then data = data.copyToSize(n | (n << 1))
      data(n) = value
      n += 1
  }
  final class StrideString[+A](lb: A, val data: IArray[String], val start: Int, val step: Int, val count: Int)
  extends OfString[A](lb) with Stride[A, IArray[String], String] with Direct[String] {
    checkValid(data.length)
    inline def size: Int = count
    inline def get(i: Int): String =
      if i < 0 || i >= count then throw new IllegalArgumentException("Stride index out of bounds: " + i.toString)
      data(start + step * i)
    def length = count
    def apply(i: Int): String = get(i)
  }
}

trait Frame[+A, +B, +D] {
  def title: String
  def nCols: Int
  def nRows: Int
  def col(j: Int): Stripe[A, D]
  def row(i: Int): Stripe[B, D]
  def apply(i, j): D
  inline def pyCol(j: Int): Stripe[A, D]
  inline def pyRow(i: Int): Stripe[B, D]
  inline def py(i: Int, j: Int): D
  inline def rCol(j: Int): Stripe[A, D]
  inline def rRow(i: Int): Stripe[A, D]
  inline def R(i: Int, j: Int): D
}
object Frame {
  trait Direct[D] {
    inline def colsize: Int
    inline def rowsize: Int
    inline def get(i: Int, j: Int): D
    inline def pyGet(i: Int, j: Int): D =
      get(if i < 0 then rowsize - i else i, if j < 0 then colsize - j else j)
    inline def rGet(i: Int, j: Int): D = get(i - 1, j - 1)
  }
  final class FrameD[+A, +B](val title: String)
}


/*
final class Frame1D[A] private (val size: Int, preload: Array[Double] | (Array[Int], Array[Double]) = null) {
  private[maths] var content: Array[Double] | (Array[Int], Array[Double]) = preload match
    case null => Frame1D.emptySparse
    case ad: Array[Double] =>
      if ad.length != size throw new IllegalArgumentException(s"Input size ${ad.length} does not match Frame size $size")
      ad
    case aiad: (Array[Int], Array[Double]) => Frame1D.verifySparse(aiad)

      val (ai, ad) = aiad
      if ai.length == 0 && ad.length == 0 then aiad
      else
        Frame1D.verifiedSparse(ai, ad)
        if !(ai.length - 1 == ad.length) then throw new IllegalArgumentException(s"Mismatch in sparse array: 1+${ai.length-1} indices but ${ad.length} data points")

}
object Frame1D {
  val noDoubles = Array.empty[Double]
  val noInts = Array.empty[Int]
  val emptySparse = (noInts, noDoubles)

  def verifySparse(ai: )
}


final class Frame2D[A, B] private (val rowsize: Int, val colsize: Int, preload: Content = null) {
  private var content: Content =
    if preload eq null then Array[(Array[Int], Array[Double])].fill(rowsize)(emptySparse)
}
object Frame {
  type Content = Array[Double] | Array[Array[Double] | (Array[Int], Array[Double])]

}
*/
