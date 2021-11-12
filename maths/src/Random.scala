// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016, 2021 by Rex Kerr and Calico Life Sciences LLC

package kse.maths

import java.lang.{Math => jm}

import kse.flow.Copy

/** Random number generator intended to be fast, low-state, but reasonably well-distributed.
 *  You can serialize this into a few Longs--in many cases only one Long!--when the cache is
 *  empty.  You can also create copies including the cache.
 */
abstract class Prng extends Copy[Prng] {
  protected final var cache: Long = 0L
  protected final var bits: Int = 0

  /** Removes the cache, rendering this Prng ready for serialization into Longs. */
  def clean: this.type = { bits = 0; this }

  /** Checks whether there is any cache. */
  def isClean: Boolean = bits == 0

  def Z: Boolean =
    if (bits < 1) { bits = 63; cache = L } else bits -= 1
    (cache & (0x1L << bits)) != 0

  def B: Byte =
    if (bits < 8) { bits = 56; cache = L } else bits -= 8
    ((cache >>> bits) & 0xFFL).toByte

  def S: Short =
    if (bits < 16) { bits = 48; cache = L } else bits -= 16
    ((cache >>> bits) & 0xFFFFL).toShort

  def C: Char =
    if (bits < 16) { bits = 48; cache = L } else bits -= 16
    ((cache >>> bits) & 0xFFFFL).toChar

  def I: Int =
    if (bits < 32) { bits = 32; cache = L } else bits -= 32
    ((cache >>> bits) & 0xFFFFFFFFL).toInt

  def L: Long

  def F: Float = Prng.symmetricFloatFromInt(I)

  def D: Double = Prng.symmetricDoubleFromLong(L)

  def %(n: Int): Int = 
    if (n <= 0) 0
    else {
      var i = Int.MaxValue
      val mask = 0xFFFFFFFF >>> java.lang.Integer.numberOfLeadingZeros(n)
      while (i >= n) i = I & mask
      i
    }

  def %(n: Long): Long =
    if (n <= 0) 0
    else {
      var l = Long.MaxValue
      val mask = 0xFFFFFFFFFFFFFFFFL >>> java.lang.Long.numberOfLeadingZeros(n)
      while (l >= n) l = L & mask
      l & mask
    }

  def gaussian: Double =
    // Polar Box-Muller transform
    val x = D*2 - 1
    val y = D*2 - 1
    val rr = x*x + y*y
    if (rr >= 1) gaussian
    else x * jm.sqrt( (-2 * jm.log(rr)) / rr )    // Discard y, but it's valid too!

  def gaussianVc: kse.maths.packed.Vc =
    val x = D*2 - 1
    val y = D*2 - 1
    val rr = x*x + y*y
    if (rr >= 1) gaussianVc
    else {
      val scale = jm.sqrt( (-2 * jm.log(rr)) / rr )
      kse.maths.packed.Vc.from(x * scale, y * scale)
    }

  def gaussianPair(f: (Double, Double) => Unit): Unit =
    val x = D*2 - 1
    val y = D*2 - 1
    val rr = x*x + y*y
    if (rr >= 1) gaussianPair(f)
    else {
      val scale = jm.sqrt( (-2 * jm.log(rr)) / rr )
      f(x * scale, y * scale)
    }

  def arrayZ(n: Int): Array[Boolean] =
    val a = new Array[Boolean](n)
    var i = 0
    var l = 0L
    while (i < a.length) {
      if ((i & 0x3F) == 0) l = L
      a(i) = ((l & 0x1) != 0)
      l = l >>> 1
      i += 1
    }
    a

  def arrayB(n: Int): Array[Byte] =
    val a = new Array[Byte](n)
    var i = 0
    var l = 0L
    while (i < a.length) {
      if ((i & 0x7) == 0) l = L
      a(i) = (l & 0xFF).toByte
      l = l >>> 8
      i += 1
    }
    a

  def arrayS(n: Int): Array[Short] =
    val a = new Array[Short](n)
    var i = 0
    var l = 0L
    while (i < a.length) {
      if ((i & 0x3) == 0) l = L
      a(i) = (l & 0xFFFF).toShort
      l = l >>> 16
      i += 1
    }
    a

  def arrayC(n: Int): Array[Char] =
    val a = new Array[Char](n)
    var i = 0
    var l = 0L
    while (i < a.length) {
      if ((i & 0x3) == 0) l = L
      a(i) = l.toChar
      l = l >>> 16
      i += 1
    }
    a    

  def arrayI(n: Int): Array[Int] =
    val a = new Array[Int](n)
    var i = 0
    while (i < a.length) {
      val l = L
      val p = (l & 0xFFFFFFFF).toInt
      a(i) = p
      i += 1
      if (i < a.length) {
        a(i) = (l >>> 32).toInt
        i += 1
      }
    }
    a

  def arrayL(n: Int): Array[Long] =
    val a = new Array[Long](n)
    var i = 0
    while (i < a.length) {
      a(i) = L
      i += 1
    }
    a

  def arrayF(n: Int): Array[Float] =
    val a = new Array[Float](n)
    var i = 0
    while (i < a.length) {
      val l = L
      val p = (l & 0xFFFFFFFF).toInt
      a(i) = Prng.symmetricFloatFromInt(p)
      i += 1
      if (i < a.length) {
        a(i) = Prng.symmetricFloatFromInt((l >>> 32).toInt)
        i += 1
      }
    }
    a 

  def arrayD(n: Int): Array[Double] =
    val a = new Array[Double](n)
    var i = 0
    while (i < a.length) {
      a(i) = Prng.symmetricDoubleFromLong(L)
      i += 1
    }
    a    

  /** Todo--improve efficiency by inlining and reusing Long */
  def arrayMod(n: Int, mod: Int): Array[Int] =
    val a = new Array[Int](n)
    var i = 0
    while (i < a.length) {
      a(i) = this % mod
      i += 1
    }
    a

  def arrayMod(n: Int, mod: Long): Array[Long] =
    val a = new Array[Long](n)
    var i = 0
    while (i < a.length) {
      a(i) = this % mod
      i += 1
    }
    a

  def arrayGaussian(n: Int): Array[Double] =
    val a = new Array[Double](n)
    var i = 0
    while (i < a.length) {
      var x, y = 0.0
      var rr = 1.0
      while (rr >= 1.0) {
        x = D*2 - 1
        y = D*2 - 1
        rr = x*x + y*y
      }
      val scale = sqrt( (-2 * log(rr)) / rr )
      a(i) = x * scale
      i += 1
      if (i < a.length) {
        a(i) = y * scale
        i += 1
      }
    }
    a

  def shuffle(a: Array[Byte]): Unit = {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }

  def shuffle(a: Array[Short]): Unit = {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }

  def shuffle(a: Array[Char]): Unit = {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }

  def shuffle(a: Array[Int]): Unit = {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }

  def shuffle(a: Array[Float]): Unit = {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }

  def shuffle(a: Array[Long]): Unit = {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }

  def shuffle(a: Array[Double]): Unit = {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }

  def shuffle[A <: AnyRef](a: Array[A]): Unit = {
    var i = a.length - 1; while (i > 0) { val j = this % (i+1); if (j != i) { val x = a(j); a(j) = a(i); a(i) = x }; i -= 1 }
  }

  def state(i: Int): Long
  def stateLength: Int
  def setState(i: Int)(l: Long): Boolean
}
object Prng {
  def symmetricFloatFromInt(i: Int): Float =
    val leadingZeros = java.lang.Integer.numberOfLeadingZeros(i)
    if (leadingZeros <= 22) {
      val exponent = 126 - leadingZeros
      val mantissa = ((i >>> 8) << leadingZeros) & 0x007FFFFF
      java.lang.Float.intBitsToFloat( (exponent << 23) | mantissa )
    }
    else 0.001953125f*i + 9.765625E-4f     // Subnormal values aren't symmetric, so we remap them equally spaced between 0 and 1

  def symmetricDoubleFromLong(l: Long): Double =
    val leadingZeros = java.lang.Long.numberOfLeadingZeros(l)
    if (leadingZeros <= 52) {
      val exponent = 1022L - leadingZeros
      val mantissa = ((l >>> 11) << leadingZeros) & 0x000FFFFFFFFFFFFFL
      java.lang.Double.longBitsToDouble( (exponent << 52) | mantissa )
    }
    else 4.8828125E-4*l + 2.44140625E-4   // Subnormal values aren't symmetric, so we remap them equally spaced between 0 and 1

}

sealed abstract class PrngState64 extends Prng with Copy[PrngState64] {
 protected final var state64: Long = 0L

  final def stateLength = 1
  final def state(i: Int): Long = state64
  final def setState(i: Int)(l: Long) = if (i == 0) { state64 = l; true } else false
}

// From public domain code by Sebastiano Vigna
final class ShiftMix64(initialState: Long = java.lang.System.nanoTime) extends PrngState64 with Copy[ShiftMix64] {
  state64 = initialState

  def copy: ShiftMix64 = 
    val ans = new ShiftMix64(state64)
    ans.bits = bits
    ans.cache = cache
    ans

  def L =
    bits = 0
    state64 += 0x9E3779B97F4A7C15L;
    var l = (state64 ^ (state64 >>> 30)) * 0xBF58476D1CE4E5B9L
    l = (l ^ (l >>> 27)) * 0x94D049BB133111EBL
    l ^ (l >>> 31)
}

// Algorithm taken from PCG generators by Melissa O'Niell (Apache 2 license); RXS M XS 64 variant (one sequence)
final class Pcg64(initialState: Long = java.lang.System.nanoTime) extends PrngState64 with Copy[Pcg64] {
  state64 = initialState

  def copy: Pcg64 =
    val ans = new Pcg64(state64)
    ans.bits = bits
    ans.cache = cache
    ans

  def L =
    bits = 0
    state64 = (state64 * 6364136223846793005L) + 1442695040888963407L
    val l = ((state64 >>> ((state64 >>> 59) + 5)) ^ state64) * 0xAEF17502108EF2D9L   // 12605985483714917081 base 10
    (l >>> 43) ^ l
}
