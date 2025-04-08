// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016, 2021-23 by Rex Kerr and Calico Life Sciences LLC

package kse.maths


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.lang.{Math => jm}

import scala.annotation.targetName
import scala.reflect.ClassTag

import scala.collection.immutable.{Range => Rg}

import kse.basics.{given, _}
import kse.basics.intervals._


/** Random number generator intended to be fast, low-state, but reasonably well-distributed.
 *  You can serialize this into a few Longs--in many cases only one Long!--when the cache is
 *  empty.  You can also create copies including the cache.
 */
sealed abstract class Prng {
  protected final var cache: Long = 0L
  protected final var bits: Int = 0

  def copy: Prng
  inline def givable: AutoPrng = AutoPrng.wrap(this)

  /** Removes the cache, rendering this Prng ready for serialization into Longs. */
  final def clean: this.type = { bits = 0; this }

  /** Checks whether there is any cache. */
  final def isClean: Boolean = bits == 0

  final def Z: Boolean =
    if bits < 1 then { cache = L; bits = 63 } else bits -= 1
    (cache & (0x1L << bits)) != 0

  final def B: Byte =
    if bits < 8 then { cache = L; bits = 56 } else bits -= 8
    ((cache >>> bits) & 0xFFL).toByte

  final def S: Short =
    if bits < 16 then { cache = L; bits = 48 } else bits -= 16
    ((cache >>> bits) & 0xFFFFL).toShort

  final def C: Char =
    if bits < 16 then { cache = L; bits = 48 } else bits -= 16
    ((cache >>> bits) & 0xFFFFL).toChar

  final def I: Int =
    if bits < 32 then { cache = L; bits = 32 } else bits -= 32
    ((cache >>> bits) & 0xFFFFFFFFL).toInt

  def L: Long

  final def F: Float = Prng.symmetricFloatFromInt(I)

  final def D: Double = Prng.symmetricDoubleFromLong(L)

  final def %(n: Int): Int = 
    if n <= 0 then 0
    else
      var i = Int.MaxValue
      val mask = 0xFFFFFFFF >>> java.lang.Integer.numberOfLeadingZeros(n)
      while i >= n do i = I & mask
      i

  final def %(n: Long): Long =
    if n <= 0 then 0
    else
      var l = Long.MaxValue
      val mask = 0xFFFFFFFFFFFFFFFFL >>> java.lang.Long.numberOfLeadingZeros(n)
      while l >= n do l = L & mask
      l & mask

  final def W: Double = Prng.signedSymmetricDoubleFromLong(L)

  final def uniform(lo: Double, hi: Double): Double =
    val l2 = lo*0.5
    val h2 = hi*0.5
    val c = l2 + h2
    c + (h2 - l2) * W

  final def gaussian: Double =
    if bits < 0 then
      bits = 0
      java.lang.Double.longBitsToDouble(cache)
    else
      // Polar Box-Muller transform
      var x = D*2 - 1
      var y = D*2 - 1
      var rr = x*x + y*y
      while rr >= 1 do
        x = D*2 - 1
        y = D*2 - 1
        rr = x*x + y*y
      val scale = jm.sqrt( (-2 * jm.log(rr)) / rr )
      bits = -64
      cache = java.lang.Double.doubleToRawLongBits(y * scale)
      x * scale

  final def gaussianVc: kse.maths.Vc =
    val x = D*2 - 1
    val y = D*2 - 1
    val rr = x*x + y*y
    if rr >= 1 then gaussianVc
    else 
      val scale = jm.sqrt( (-2 * jm.log(rr)) / rr )
      kse.maths.Vc.D(x * scale, y * scale)

  final def gaussianPair(f: (Double, Double) => Unit): Unit =
    val x = D*2 - 1
    val y = D*2 - 1
    val rr = x*x + y*y
    if rr >= 1 then gaussianPair(f)
    else
      val scale = jm.sqrt( (-2 * jm.log(rr)) / rr )
      f(x * scale, y * scale)

  final inline def useZ(            inline f: Boolean => Unit): this.type = { f(Z);        this }
  final inline def useB(            inline f: Byte    => Unit): this.type = { f(B);        this }
  final inline def useS(            inline f: Short   => Unit): this.type = { f(S);        this }
  final inline def useC(            inline f: Char    => Unit): this.type = { f(C);        this }
  final inline def useI(            inline f: Int     => Unit): this.type = { f(I);        this }
  final inline def useL(            inline f: Long    => Unit): this.type = { f(L);        this }
  final inline def useF(            inline f: Float   => Unit): this.type = { f(F);        this }
  final inline def useD(            inline f: Double  => Unit): this.type = { f(D);        this }
  final inline def useModI(m: Int )(inline f: Int     => Unit): this.type = { f(this % m); this }
  final inline def useModL(m: Long)(inline f: Long    => Unit): this.type = { f(this % m); this }
  final inline def useGaussian(     inline f: Double  => Unit): this.type = { f(gaussian); this }

  final def fillRangeZ(target: Array[Boolean])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k && bits > 0 do
      target(i) = Z
      i += 1
    if i < k then
      while i < k - 64 do
        var l = L
        var b = 1L << 63
        while b != 0 do
          target(i) = (l & b) != 0
          i += 1
          b = b >>> 1
      while i < k do
        target(i) = Z
        i += 1
    this

  final def fillRangeB(target: Array[Byte])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k && bits > 7 do
      target(i) = B
      i += 1
    if i < k then
      bits = 0
      while i < k - 8 do
        var l = L
        target(i  ) = ((l >>> 56) & 0xFFL).toByte
        target(i+1) = ((l >>> 48) & 0xFFL).toByte
        target(i+2) = ((l >>> 40) & 0xFFL).toByte
        target(i+3) = ((l >>> 32) & 0xFFL).toByte
        target(i+4) = ((l >>> 24) & 0xFFL).toByte
        target(i+5) = ((l >>> 16) & 0xFFL).toByte
        target(i+6) = ((l >>>  8) & 0xFFL).toByte
        target(i+7) = ( l         & 0xFFL).toByte
        i += 8
      while i < k do
        target(i) = B
        i += 1
    this

  final def fillRangeS(target: Array[Short])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k && bits > 15 do
      target(i) = S
      i += 1
    if i < k then
      bits = 0
      while i < k - 4 do
        var l = L
        target(i  ) = ((l >>> 48) & 0xFFFFL).toShort
        target(i+1) = ((l >>> 32) & 0xFFFFL).toShort
        target(i+2) = ((l >>> 16) & 0xFFFFL).toShort
        target(i+3) = ( l         & 0xFFFFL).toShort
        i += 4
      while i < k do
        target(i) = S
        i += 1
    this

  final def fillRangeC(target: Array[Char])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k && bits > 15 do
      target(i) = C
      i += 1
    if i < k then
      bits = 0
      while i < k - 4 do
        var l = L
        target(i  ) = (l >>> 48).toChar
        target(i+1) = (l >>> 32).toChar
        target(i+2) = (l >>> 16).toChar
        target(i+3) = (l       ).toChar
        i += 4
      while i < k do
        target(i) = C
        i += 1
    this

  final def fillRangeI(target: Array[Int])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k && bits > 31 do
      target(i) = I
      i += 1
    if i < k then
      bits = 0
      while i < k - 32 do
        var l = L
        target(i  ) = ((l >>> 32) & 0xFFFFFFFFL).toInt
        target(i+1) = ( l         & 0xFFFFFFFFL).toInt
        i += 2
      while i < k do
        target(i) = I
        i += 1
    this

  final def fillRangeL(target: Array[Long])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = L
      i += 1
    this

  final def fillRangeF(target: Array[Float])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k && bits > 31 do
      target(i) = F
      i += 1
    if i < k then
      bits = 0
      while i < k - 32 do
        var l = L
        target(i  ) = Prng.symmetricFloatFromInt(((l >>> 32) & 0xFFFFFFFFL).toInt)
        target(i+1) = Prng.symmetricFloatFromInt(( l         & 0xFFFFFFFFL).toInt)
        i += 2
      while i < k do
        target(i) = F
        i += 1
    this

  final def fillRangeD(target: Array[Double])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = D
      i += 1
    this

  inline final def fillRangeOp[A](target: Array[A])(i0: Int, iN: Int)(inline f: Prng => A): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = f(this)
      i += 1
    this

  final def fillRangeModI(m: Int)( target: Array[Int])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = this % m
      i += 1
    this

  final def fillRangeModL(m: Long)(target: Array[Long])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = this % m
      i += 1
    this

  final def fillRangeGaussian(     target: Array[Double])(i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = gaussian
      i += 1
    this

  final inline def fillZ(            target: Array[Boolean]): this.type = fillRangeZ(            target)(0, target.length)
  final inline def fillB(            target: Array[Byte]   ): this.type = fillRangeB(            target)(0, target.length)
  final inline def fillS(            target: Array[Short]  ): this.type = fillRangeS(            target)(0, target.length)
  final inline def fillC(            target: Array[Char]   ): this.type = fillRangeC(            target)(0, target.length)
  final inline def fillI(            target: Array[Int]    ): this.type = fillRangeI(            target)(0, target.length)
  final inline def fillL(            target: Array[Long]   ): this.type = fillRangeL(            target)(0, target.length)
  final inline def fillF(            target: Array[Float]  ): this.type = fillRangeF(            target)(0, target.length)
  final inline def fillD(            target: Array[Double] ): this.type = fillRangeD(            target)(0, target.length)
  final inline def fillModI(m: Int)( target: Array[Int]    ): this.type = fillRangeModI(m: Int)( target)(0, target.length)
  final inline def fillModL(m: Long)(target: Array[Long]   ): this.type = fillRangeModL(m: Long)(target)(0, target.length)
  final inline def fillGaussian(     target: Array[Double] ): this.type = fillRangeGaussian(     target)(0, target.length)
  final inline def fillOp[A](target: Array[A])(inline f: Prng => A): this.type =
    fillRangeOp(target)(0, target.length)(f)

  final inline def fillRangeZ(target: Array[Boolean])(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeZ(target)(iv.i0, iv.iN) }
  final inline def fillRangeB(target: Array[Byte]   )(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeB(target)(iv.i0, iv.iN) }
  final inline def fillRangeS(target: Array[Short]  )(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeS(target)(iv.i0, iv.iN) }
  final inline def fillRangeC(target: Array[Char]   )(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeC(target)(iv.i0, iv.iN) }
  final inline def fillRangeI(target: Array[Int]    )(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeI(target)(iv.i0, iv.iN) }
  final inline def fillRangeL(target: Array[Long]   )(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeL(target)(iv.i0, iv.iN) }
  final inline def fillRangeF(target: Array[Float]  )(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeF(target)(iv.i0, iv.iN) }
  final inline def fillRangeD(target: Array[Double] )(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeD(target)(iv.i0, iv.iN) }
  final inline def fillRangeModI(m: Int)( target: Array[Int]   )(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeModI(m)( target)(iv.i0, iv.iN) }
  final inline def fillRangeModL(m: Long)(target: Array[Long]  )(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeModL(m)( target)(iv.i0, iv.iN) }
  final inline def fillRangeGaussian(     target: Array[Double])(inline rg: Rg): this.type = { val iv = Iv of rg; fillRangeGaussian(target)(iv.i0, iv.iN) }
  final inline def fillRangeOp[A](target: Array[A])(inline rg: Rg)(inline f: Prng => A): this.type =
    val iv = Iv of rg
    fillRangeOp(target)(iv.i0, iv.iN)(f)

  final inline def fillRangeZ(target: Array[Boolean])(inline v: Iv.X): this.type = { val iv = v of target; fillRangeZ(target)(iv.i0, iv.iN) }
  final inline def fillRangeB(target: Array[Byte]   )(inline v: Iv.X): this.type = { val iv = v of target; fillRangeB(target)(iv.i0, iv.iN) }
  final inline def fillRangeS(target: Array[Short]  )(inline v: Iv.X): this.type = { val iv = v of target; fillRangeS(target)(iv.i0, iv.iN) }
  final inline def fillRangeC(target: Array[Char]   )(inline v: Iv.X): this.type = { val iv = v of target; fillRangeC(target)(iv.i0, iv.iN) }
  final inline def fillRangeI(target: Array[Int]    )(inline v: Iv.X): this.type = { val iv = v of target; fillRangeI(target)(iv.i0, iv.iN) }
  final inline def fillRangeL(target: Array[Long]   )(inline v: Iv.X): this.type = { val iv = v of target; fillRangeL(target)(iv.i0, iv.iN) }
  final inline def fillRangeF(target: Array[Float]  )(inline v: Iv.X): this.type = { val iv = v of target; fillRangeF(target)(iv.i0, iv.iN) }
  final inline def fillRangeD(target: Array[Double] )(inline v: Iv.X): this.type = { val iv = v of target; fillRangeD(target)(iv.i0, iv.iN) }
  final inline def fillRangeModI(m: Int)( target: Array[Int]   )(inline v: Iv.X): this.type = { val iv = v of target; fillRangeModI(m)( target)(iv.i0, iv.iN) }
  final inline def fillRangeModL(m: Long)(target: Array[Long]  )(inline v: Iv.X): this.type = { val iv = v of target; fillRangeModL(m)( target)(iv.i0, iv.iN) }
  final inline def fillRangeGaussian(     target: Array[Double])(inline v: Iv.X): this.type = { val iv = v of target; fillRangeGaussian(target)(iv.i0, iv.iN) }
  final inline def fillRangeOp[A](target: Array[A])(inline v: Iv.X)(inline f: Prng => A): this.type =
    val iv = v of target
    fillRangeOp(target)(iv.i0, iv.iN)(f)

  final inline def arrayZ(            n: Int): Array[Boolean] = { val a = new Array[Boolean](if n < 0 then 0 else n); fillZ(       a); a }
  final inline def arrayB(            n: Int): Array[Byte]    = { val a = new Array[Byte]   (if n < 0 then 0 else n); fillB(       a); a }
  final inline def arrayS(            n: Int): Array[Short]   = { val a = new Array[Short]  (if n < 0 then 0 else n); fillS(       a); a }
  final inline def arrayC(            n: Int): Array[Char]    = { val a = new Array[Char]   (if n < 0 then 0 else n); fillC(       a); a }
  final inline def arrayI(            n: Int): Array[Int]     = { val a = new Array[Int]    (if n < 0 then 0 else n); fillI(       a); a }
  final inline def arrayL(            n: Int): Array[Long]    = { val a = new Array[Long]   (if n < 0 then 0 else n); fillL(       a); a }
  final inline def arrayF(            n: Int): Array[Float]   = { val a = new Array[Float]  (if n < 0 then 0 else n); fillF(       a); a }
  final inline def arrayD(            n: Int): Array[Double]  = { val a = new Array[Double] (if n < 0 then 0 else n); fillD(       a); a }
  final inline def arrayModI(m: Int)( n: Int): Array[Int]     = { val a = new Array[Int]    (if n < 0 then 0 else n); fillModI(m)( a); a }
  final inline def arrayModL(m: Long)(n: Int): Array[Long]    = { val a = new Array[Long]   (if n < 0 then 0 else n); fillModL(m)( a); a }
  final inline def arrayGaussian(     n: Int): Array[Double]  = { val a = new Array[Double] (if n < 0 then 0 else n); fillGaussian(a); a }
  final inline def arrayOp[A](n: Int)(inline f: Prng => A)(using ClassTag[A]): Array[A] =
    val a = new Array[A](if n < 0 then 0 else n)
    fillOp(a)(f)
    a

  final def shuffleRangeZ(a: Array[Boolean])(i0: Int, iN: Int): Unit =
    var i = a.length - 1
    if iN <= i then i = iN - 1
    var n = if i0 < 0 then i+1 else 1+i-i0
    while n > 1 do
      val j = this % n
      if j > 0 then
        val x = a(i-j)
        a(i-j) = a(i)
        a(i) = x
      i -= 1
      n -= 1

  final def shuffleRangeB(a: Array[Byte])(i0: Int, iN: Int): Unit =
    var i = a.length - 1
    if iN <= i then i = iN - 1
    var n = if i0 < 0 then i+1 else 1+i-i0
    while n > 1 do
      val j = this % n
      if j > 0 then
        val x = a(i-j)
        a(i-j) = a(i)
        a(i) = x
      i -= 1
      n -= 1

  final def shuffleRangeS(a: Array[Short])(i0: Int, iN: Int): Unit =
    var i = a.length - 1
    if iN <= i then i = iN - 1
    var n = if i0 < 0 then i+1 else 1+i-i0
    while n > 1 do
      val j = this % n
      if j > 0 then
        val x = a(i-j)
        a(i-j) = a(i)
        a(i) = x
      i -= 1
      n -= 1

  final def shuffleRangeC(a: Array[Char])(i0: Int, iN: Int): Unit =
    var i = a.length - 1
    if iN <= i then i = iN - 1
    var n = if i0 < 0 then i+1 else 1+i-i0
    while n > 1 do
      val j = this % n
      if j > 0 then
        val x = a(i-j)
        a(i-j) = a(i)
        a(i) = x
      i -= 1
      n -= 1

  final def shuffleRangeI(a: Array[Int])(i0: Int, iN: Int): Unit =
    var i = a.length - 1
    if iN <= i then i = iN - 1
    var n = if i0 < 0 then i+1 else 1+i-i0
    while n > 1 do
      val j = this % n
      if j > 0 then
        val x = a(i-j)
        a(i-j) = a(i)
        a(i) = x
      i -= 1
      n -= 1

  final def shuffleRangeF(a: Array[Float])(i0: Int, iN: Int): Unit =
    var i = a.length - 1
    if iN <= i then i = iN - 1
    var n = if i0 < 0 then i+1 else 1+i-i0
    while n > 1 do
      val j = this % n
      if j > 0 then
        val x = a(i-j)
        a(i-j) = a(i)
        a(i) = x
      i -= 1
      n -= 1

  final def shuffleRangeL(a: Array[Long])(i0: Int, iN: Int): Unit =
    var i = a.length - 1
    if iN <= i then i = iN - 1
    var n = if i0 < 0 then i+1 else 1+i-i0
    while n > 1 do
      val j = this % n
      if j > 0 then
        val x = a(i-j)
        a(i-j) = a(i)
        a(i) = x
      i -= 1
      n -= 1

  final def shuffleRangeD(a: Array[Double])(i0: Int, iN: Int): Unit =
    var i = a.length - 1
    if iN <= i then i = iN - 1
    var n = if i0 < 0 then i+1 else 1+i-i0
    while n > 1 do
      val j = this % n
      if j > 0 then
        val x = a(i-j)
        a(i-j) = a(i)
        a(i) = x
      i -= 1
      n -= 1

  final def shuffleRangeO[A <: AnyRef](a: Array[A])(i0: Int, iN: Int): Unit =
    var i = a.length - 1
    if iN <= i then i = iN - 1
    var n = if i0 < 0 then i+1 else 1+i-i0
    while n > 1 do
      val j = this % n
      if j > 0 then
        val x = a(i-j)
        a(i-j) = a(i)
        a(i) = x
      i -= 1
      n -= 1

  inline final def shuffleRange[A](a: Array[A])(i0: Int, iN: Int): Unit = inline a match
    case az: Array[Boolean] => shuffleRangeZ(az)(i0, iN)
    case ab: Array[Byte]    => shuffleRangeB(ab)(i0, iN)
    case as: Array[Short]   => shuffleRangeS(as)(i0, iN)
    case ac: Array[Char]    => shuffleRangeC(ac)(i0, iN)
    case ai: Array[Int]     => shuffleRangeI(ai)(i0, iN)
    case al: Array[Long]    => shuffleRangeL(al)(i0, iN)
    case af: Array[Float]   => shuffleRangeF(af)(i0, iN)
    case ad: Array[Double]  => shuffleRangeD(ad)(i0, iN)
    case _ => scala.compiletime.summonFrom {
      case _: (A <:< AnyRef) => shuffleRangeO(a.asInstanceOf[Array[AnyRef]])(i0, iN)
      case _ =>
        var i = a.length - 1
        if iN <= i then i = iN - 1
        var n = if i0 < 0 then i+1 else 1+i-i0
        while n > 1 do
          val j = this % n
          if j > 0 then
            val x = a(i-j)
            a(i-j) = a(i)
            a(i) = x
          i -= 1
          n -= 1
    }

  final inline def shuffle[A](a: Array[A]): Unit = shuffleRange(a)(0, a.length)
  final inline def shuffleRange[A](a: Array[A])(inline rg: Rg): Unit =
    val iv = Iv of rg
    shuffleRange(a)(iv.i0, iv.iN)
  final inline def shuffleRange[A](a: Array[A])(inline v: Iv.X): Unit =
    val iv = v of a
    shuffleRange(a)(iv.i0, iv.iN)

  private def liAlgorithmL(n: Int, k: Int): Array[Int] =
    val ans = new Array[Int](k)
    ans.set()(i => i)
    var i = k
    var w = math.exp(math.log(this.D)/k)
    while i <= n do
      i += 1 + math.floor(math.log(this.D)/math.log(1-w)).toInt
      if i <= n then
        ans(this % ans.length) = i - 1
        w *= math.exp(math.log(this.D)/k)
    ans

  final def chooseIndices(n: Int, k: Int, sorted: Boolean = false): Array[Int] =
    if k <= 0 || k >= n then throw new IllegalArgumentException("must have 0 < k < n for meaningful choice")
    else if k <= 3 then 
      // Direct selection
      val one = this % n
      if k == 1 then Array(one)
      else
        var two = this % (n - 1)
        if two >= 1 then two += 1
        if k == 2 then
          if !sorted || one < two then Array(one, two)
          else Array(two, one)
        else
          var three = this % (n - 2)
          if three >= one || three >= two then
            three += 1
            if three >= one && three >= two then
              three += 1
          val ans = Array(one, two, three)
          if sorted then java.util.Arrays.sort(ans)
          ans
    else if k <= 16 && k <= n/4 then
      // Resampling (quadratic in k)
      val ans = new Array[Int](k)
      ans(0) = this % n
      var i = 1
      while i < ans.length do
        val v = this % n
        var j = 0
        while j < i && ans(j) != v do j += 1
        if j == i then
          ans(i) = v
          i += 1
      if sorted then java.util.Arrays.sort(ans)
      ans
    else if k < n/16 then
      val ans = liAlgorithmL(n, k)
      if !sorted then shuffleRangeI(ans)(0, ans.length)
      else java.util.Arrays.sort(ans)
      ans
    else if k < n/2 || sorted then
      // Pursuit, uses n random doubles
      val ans = new Array[Int](k)
      var in = k
      var out = n - k
      var i = 0
      while i < n && in > 0 do
        if D < in.toDouble/(in + out) then
          ans(ans.length - in) = i
          in -= 1
        else out -= 1
        i += 1
      if !sorted then shuffleRangeI(ans)(0, ans.length)
      ans
    else
      // Truncated Fisher-Yates shuffle, uses n random integers
      val full = new Array[Int](n)
      full.set()(i => i)
      shuffleRangeI(full)(0, full.length)
      full.select(0, k)

  final inline def sample[A](a: Array[A]): A = a(this % a.length)
  final inline def sample[A](k: Int)(a: Array[A])(using ClassTag[A]): Array[A] = sampleRange(k)(a)(0, a.length)

  final inline def sampleRange[A](a: Array[A])(i0: Int, iN: Int): A =
    if i0 >= iN then throw new IllegalArgumentException("sample of empty range")
    else a(i0 + (this % (iN - i0)))
  final inline def sampleRange[A](a: Array[A])(inline rg: Rg): A =
    val iv = Iv of rg
    sampleRange(a)(iv.i0, iv.iN)
  final inline def sampleRange[A](a: Array[A])(inline v: Iv.X): A =
    val iv = v of a
    sampleRange(a)(iv.i0, iv.iN)

  final inline def sampleRange[A](k: Int)(a: Array[A])(i0: Int, iN: Int)(using ClassTag[A]): Array[A] =
    if k <= 0 then new Array[A](0)
    else if i0 >= iN then throw new IllegalArgumentException("sample of empty range")
    else if k < iN - i0 then
      val ans = new Array[A](k)
      val indices = chooseIndices(iN - i0, k)
      var i = 0
      while i < ans.length do
        ans(i) = a(indices(i) + i0)
        i += 1
      ans
    else
      val ans = new Array[A](k)
      val indices = Iv(i0, iN).where()
      var h = 0
      while h < ans.length do
        shuffleRangeI(indices)(0, indices.length)
        var i = 0
        while i < indices.length && h < ans.length do
          ans(h) = a(indices(i) + i0)
          i += 1
          h += 1
      ans

  final inline def sampleRange[A](k: Int)(a: Array[A])(inline rg: Rg)(using ClassTag[A]): Array[A] =
    val iv = Iv of rg
    sampleRange(k)(a)(iv.i0, iv.iN)
  final inline def sampleRange[A](k: Int)(a: Array[A])(inline v: Iv.X)(using ClassTag[A]): Array[A] =
    val iv = v of a
    sampleRange(k)(a)(iv.i0, iv.iN)

  final inline def sample(a: String): Char = a.charAt(this % a.length)
  final inline def sample(k: Int)(a: String): String = sampleRange(k)(a)(0, a.length)

  final inline def sampleRange(a: String)(i0: Int, iN: Int): Char =
    if i0 >= iN then throw new IllegalArgumentException("sample of empty range")
    else a.charAt(i0 + (this % (iN - i0)))
  final inline def sampleRange(a: String)(inline rg: Rg): Char =
    val iv = Iv of rg
    sampleRange(a)(iv.i0, iv.iN)
  final inline def sampleRange(a: String)(inline v: Iv.X): Char =
    val iv = v of a
    sampleRange(a)(iv.i0, iv.iN)

  final inline def sampleRange(k: Int)(a: String)(i0: Int, iN: Int): String =
    if k <= 0 then ""
    else if i0 >= iN then throw new IllegalArgumentException("sample of empty range")
    else if k < iN - i0 then
      val ans = new java.lang.StringBuilder(k)
      val indices = chooseIndices(iN - i0, k)
      var i = 0
      while i < k do
        ans append a.charAt(indices(i) + i0)
        i += 1
      ans.toString
    else
      val ans = new java.lang.StringBuilder(k)
      val indices = Iv(i0, iN).where()
      var h = 0
      while h < k do
        shuffleRangeI(indices)(0, indices.length)
        var i = 0
        while i < indices.length && h < k do
          ans append a.charAt(indices(i) + i0)
          i += 1
          h += 1
      ans.toString

  final inline def sampleRange(k: Int)(a: String)(inline rg: Rg): String =
    val iv = Iv of rg
    sampleRange(k)(a)(iv.i0, iv.iN)
  final inline def sampleRange(k: Int)(a: String)(inline v: Iv.X): String =
    val iv = v of a
    sampleRange(k)(a)(iv.i0, iv.iN)

  final def sampleIndexedSeq[A, CC <: IndexedSeq[A]](k: Int)(coll: CC)(using factory: scala.collection.Factory[A, CC]): CC =
    val b = factory.newBuilder
    if k > 0 then
      b.sizeHint(k)
      if coll.length == 0 then throw new IllegalArgumentException("sample of empty range")
      else if k < coll.length then
        val indices = chooseIndices(coll.length, k)
        var i = 0
        while i < k do
          b += coll.apply(indices(i))
          i += 1
      else
        val indices = Iv(0, coll.length).where()
        var h = 0
        while h < k do
          shuffleRangeI(indices)(0, indices.length)
          var i = 0
          while i < indices.length && h < k do
            b += coll.apply(indices(i))
            i += 1
            h += 1
    b.result

  final def sampleCollection[A, CC <: IterableOnce[A]](k: Int)(coll: CC)(using factory: scala.collection.Factory[A, CC]): CC =
    val b = factory.newBuilder
    if k > 0 then
      val i = coll.iterator
      val a = new Array[AnyRef](k)
      b.sizeHint(k)
      var n = 0
      while i.hasNext && n < a.length do
        a(n) = i.next.asInstanceOf[AnyRef]
        n += 1
      if n == k then   
        // Li algorithmL in unknown length mode
        var w = math.exp(math.log(D)/k)
        while i.hasNext do
          var h = math.floor(math.log(D)/math.log(1-w)).toInt
          while h > 0 && i.hasNext do
            i.next
            h -= 1
          if i.hasNext then
            a(this % a.length) = i.next.asInstanceOf[AnyRef]
            w *= math.exp(math.log(D)/k)
        shuffle(a)
        a.peek()(x => b += x.asInstanceOf[A])
      else
        var m = 0
        while m < k do
          shuffleRange(a)(0, n)
          var j = 0
          while j < n && m < k do
            b += a(j).asInstanceOf[A]
            j += 1
            m += 1
    b.result


  final def stringFrom(letters: String, n: Int): String =
    if n <= 0 then ""
    else if letters.isEmpty then stringFrom("\u0000", n)
    else
      val m = letters.length
      var i = n
      val sb = new java.lang.StringBuilder(m)
      if m == 1 then
        val c = letters charAt 0
        while i > 0 do
          sb append c
          i -= 1
      else
        val mask = 0xFFFFFFFF >>> java.lang.Integer.numberOfLeadingZeros(m)
        if m <= 256 then
          while i > 0 do
            val k = B & mask
            if k < m then
              sb append letters.charAt(k)
              i -= 1
        else
          while i > 0 do
            val k = C & mask
            if k < m then
              sb append letters.charAt(k)
              i -= 1
      sb.toString


  final def webString(n: Int): String =
    stringFrom(Prng.WebCharacters, n)

  final def textString(n: Int): String =
    stringFrom(Prng.TextCharacters, n)

  final def asciiString(n: Int): String =
    val sb = new java.lang.StringBuilder(math.max(0, n))
    var i = n
    while i > 0 do
      sb append (B & 0x7F).toChar
      i -= 1
    sb.toString
  final def validString(n: Int): String =
    val sb = new java.lang.StringBuilder(math.max(0, n))
    var i = n
    while i > 0 do
      val c = C
      if java.lang.Character.isSurrogate(c) then
        if i > 1 && java.lang.Character.isHighSurrogate(c) then
          sb append c
          sb append ((C & 0x3FF) + java.lang.Character.MIN_LOW_SURROGATE).toChar
          i -= 2
      else
        sb append c
        i -= 1
    sb.toString

  def getState(i: Int): Long
  def stateLength: Int
  def setState(i: Int)(l: Long): Boolean

  final def getCacheBits: Int = bits
  final def getCache: Long = cache
  final def setCacheAndBits(c: Long, b: Int): this.type =
    cache = c
    bits = if b > 63 then 63 else b
    this
}
object Prng {
  def symmetricFloatFromInt(i: Int): Float =
    val leadingZeros = java.lang.Integer.numberOfLeadingZeros(i)
    if leadingZeros <= 22 then
      val exponent = 126 - leadingZeros
      val mantissa = ((i >>> 8) << leadingZeros) & 0x007FFFFF
      java.lang.Float.intBitsToFloat( (exponent << 23) | mantissa )
    else 0.001953125f*i + 9.765625E-4f     // Subnormal values aren't symmetric, so we remap them equally spaced between 0 and 1

  def symmetricDoubleFromLong(l: Long): Double =
    val leadingZeros = java.lang.Long.numberOfLeadingZeros(l)
    if leadingZeros <= 52 then
      val exponent = 1022L - leadingZeros
      val mantissa = ((l >>> 11) << leadingZeros) & 0x000F_FFFF_FFFF_FFFFL
      java.lang.Double.longBitsToDouble( (exponent << 52) | mantissa )
    else 4.8828125E-4*l + 2.44140625E-4   // Subnormal values aren't symmetric, so we remap them equally spaced between 0 and 1

  def signedSymmetricDoubleFromLong(l: Long): Double =
    val s = l & 0x8000_0000_0000_0000L
    val h = (l << 1)
    val leadingZeros = java.lang.Long.numberOfLeadingZeros(h)
    if leadingZeros <= 52 then
      val exponent = 1022L - leadingZeros
      val mantissa = ((h >>> 11) << leadingZeros) & 0x000F_FFFF_FFFF_FFFFL
      java.lang.Double.longBitsToDouble(s | mantissa | (exponent << 52))
    else 0.0

  def apply(): Prng = new Pcg64()
  def apply(seed: Long): Prng = new Pcg64(seed)

  inline val WebCharacters = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-._~"
  inline val TextCharacters = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\n"

  given Copies[Prng] with
    def copy(a: Prng): Prng = a.copy
}

sealed abstract class PrngState64 extends Prng {
  protected final var state64: Long = 0L

  final def stateLength = 1
  final def getState(i: Int): Long = state64
  final def setState(i: Int)(l: Long) = if i == 0 then { state64 = l; true } else false
}

// From public domain code by Sebastiano Vigna
final class ShiftMix64(initialState: Long = java.lang.System.nanoTime) extends PrngState64 {
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
object ShiftMix64 {
  given Copies[ShiftMix64] with
    def copy(a: ShiftMix64): ShiftMix64 = a.copy
}

// Algorithm taken from PCG generators by Melissa O'Niell (Apache 2 license); RXS M XS 64 variant (one sequence)
final class Pcg64(initialState: Long = java.lang.System.nanoTime) extends PrngState64 {
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
object Pcg64 {
  given Copies[Pcg64] with
    def copy(a: Pcg64): Pcg64 = a.copy
}


opaque type AutoPrng = Prng
object AutoPrng {
  inline def wrap(prng: Prng): kse.maths.AutoPrng = prng

  extension (aprng: AutoPrng) {
    inline def get: Prng = aprng
  }
}


extension [A](a: Array[A])
  @targetName("that_shuffle") inline def shuffle()(r: Prng): a.type = { r.shuffleRange(a)(0, a.length); a }
  @targetName("auto_shuffle") inline def shuffle()(using ar: AutoPrng): a.type = { AutoPrng.get(ar).shuffleRange(a)(0, a.length); a }
  @targetName("that_shuffle") inline def shuffle(i0: Int, iN: Int)(r: Prng): a.type = { r.shuffleRange(a)(i0, iN); a }
  @targetName("auto_shuffle") inline def shuffle(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).shuffleRange(a)(i0, iN); a }
  @targetName("that_shuffle") inline def shuffle(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.shuffleRange(a)(iv.i0, iv.iN); a }
  @targetName("auto_shuffle") inline def shuffle(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).shuffleRange(a)(iv.i0, iv.iN); a }
  @targetName("that_shuffle") inline def shuffle(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.shuffleRange(a)(iv.i0, iv.iN); a }
  @targetName("auto_shuffle") inline def shuffle(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).shuffleRange(a)(iv.i0, iv.iN); a }
  inline def randomFillOp()(r: Prng)(f: Prng => A): a.type = { r.fillRangeOp(a)(0, a.length)(f); a }
  inline def randomFillOp()(f: Prng => A)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeOp(a)(0, a.length)(f); a }
  inline def randomFillOp(i0: Int, iN: Int)(r: Prng)(f: Prng => A): a.type = { r.fillRangeOp(a)(i0, iN)(f); a }
  inline def randomFillOp(i0: Int, iN: Int)(f: Prng => A)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeOp(a)(i0, iN)(f); a }
  inline def randomFillOp(inline rg: Rg)(r: Prng)(f: Prng => A): a.type = { val iv = Iv of rg; r.fillRangeOp(a)(iv.i0, iv.iN)(f); a }
  inline def randomFillOp(inline rg: Rg)(f: Prng => A)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeOp(a)(iv.i0, iv.iN)(f); a }
  inline def randomFillOp(inline v: Iv.X)(r: Prng)(f: Prng => A): a.type = { val iv = v of a; r.fillRangeOp(a)(iv.i0, iv.iN)(f); a }
  inline def randomFillOp(inline v: Iv.X)(f: Prng => A)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeOp(a)(iv.i0, iv.iN)(f); a }
  // inline def %(r: Prng): A = r.sample(a)    ===>   In OverloadedExtensions
  @targetName("that_sample") inline def sample()(r: Prng): A = r.sample(a)
  @targetName("auto_sample") inline def sample()(using ar: AutoPrng): A = AutoPrng.get(ar).sample(a)
  @targetName("that_sample") inline def sample(i0: Int, iN: Int)(r: Prng): A = r.sampleRange(a)(i0, iN)
  @targetName("auto_sample") inline def sample(i0: Int, iN: Int)(using ar: AutoPrng): A = AutoPrng.get(ar).sampleRange(a)(i0, iN)
  @targetName("that_sample") inline def sample(inline rg: Rg)(r: Prng): A = r.sampleRange(a)(rg)
  @targetName("auto_sample") inline def sample(inline rg: Rg)(using ar: AutoPrng): A = AutoPrng.get(ar).sampleRange(a)(rg)
  @targetName("that_sample") inline def sample(inline v: Iv.X)(r: Prng): A = r.sampleRange(a)(v)
  @targetName("auto_sample") inline def sample(inline v: Iv.X)(using ar: AutoPrng): A = AutoPrng.get(ar).sampleRange(a)(v)
  @targetName("that_sample") inline def sample(k: Int)(r: Prng)(using ClassTag[A]): Array[A] = r.sample(k)(a)
  @targetName("auto_sample") inline def sample(k: Int)(using ar: AutoPrng, tag: ClassTag[A]): Array[A] = AutoPrng.get(ar).sample(k)(a)
  @targetName("that_sample") inline def sample(k: Int)(i0: Int, iN: Int)(r: Prng)(using ClassTag[A]): Array[A] = r.sampleRange(k)(a)(i0, iN)
  @targetName("auto_sample") inline def sample(k: Int)(i0: Int, iN: Int)(using ar: AutoPrng, tag: ClassTag[A]): Array[A] = AutoPrng.get(ar).sampleRange(k)(a)(i0, iN)
  @targetName("that_sample") inline def sample(k: Int)(inline rg: Rg)(r: Prng)(using ClassTag[A]): Array[A] = r.sampleRange(k)(a)(rg)
  @targetName("auto_sample") inline def sample(k: Int)(inline rg: Rg)(using ar: AutoPrng, tag: ClassTag[A]): Array[A] = AutoPrng.get(ar).sampleRange(k)(a)(rg)
  @targetName("that_sample") inline def sample(k: Int)(inline v: Iv.X)(r: Prng)(using ClassTag[A]): Array[A] = r.sampleRange(k)(a)(v)
  @targetName("auto_sample") inline def sample(k: Int)(inline v: Iv.X)(using ar: AutoPrng, tag: ClassTag[A]): Array[A] = AutoPrng.get(ar).sampleRange(k)(a)(v)

extension (a: Array[Boolean])
  @targetName("that_ranFill") inline def randomFill(r: Prng): a.type = { r.fillZ(a); a }
  @targetName("auto_ranFill") inline def randomFill(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillZ(a); a }
  @targetName("that_ranFill") inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeZ(a)(i0, iN); a }
  @targetName("auto_ranFill") inline def randomFill(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeZ(a)(i0, iN); a }
  @targetName("that_ranFill") inline def randomFill(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeZ(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeZ(a)(iv.i0, iv.iN); a }
  @targetName("that_ranFill") inline def randomFill(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeZ(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeZ(a)(iv.i0, iv.iN); a }

extension (a: Array[Byte])
  @targetName("that_ranFill") inline def randomFill(r: Prng): a.type = { r.fillB(a); a }
  @targetName("auto_ranFill") inline def randomFill(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillB(a); a }
  @targetName("that_ranFill") inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeB(a)(i0, iN); a }
  @targetName("auto_ranFill") inline def randomFill(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeB(a)(i0, iN); a }
  @targetName("that_ranFill") inline def randomFill(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeB(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeB(a)(iv.i0, iv.iN); a }
  @targetName("that_ranFill") inline def randomFill(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeB(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeB(a)(iv.i0, iv.iN); a }

extension (a: Array[Short])
  @targetName("that_ranFill") inline def randomFill(r: Prng): a.type = { r.fillS(a); a }
  @targetName("auto_ranFill") inline def randomFill(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillS(a); a }
  @targetName("that_ranFill") inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeS(a)(i0, iN); a }
  @targetName("auto_ranFill") inline def randomFill(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeS(a)(i0, iN); a }
  @targetName("that_ranFill") inline def randomFill(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeS(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeS(a)(iv.i0, iv.iN); a }
  @targetName("that_ranFill") inline def randomFill(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeS(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeS(a)(iv.i0, iv.iN); a }

extension (a: Array[Char])
  @targetName("that_ranFill") inline def randomFill(r: Prng): a.type = { r.fillC(a); a }
  @targetName("auto_ranFill") inline def randomFill(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillC(a); a }
  @targetName("that_ranFill") inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeC(a)(i0, iN); a }
  @targetName("auto_ranFill") inline def randomFill(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeC(a)(i0, iN); a }
  @targetName("that_ranFill") inline def randomFill(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeC(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeC(a)(iv.i0, iv.iN); a }
  @targetName("that_ranFill") inline def randomFill(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeC(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeC(a)(iv.i0, iv.iN); a }

extension (a: Array[Int])
  @targetName("that_ranFill") inline def randomFill(r: Prng): a.type = { r.fillI(a); a }
  @targetName("auto_ranFill") inline def randomFill(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillI(a); a }
  @targetName("that_ranFill") inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeI(a)(i0, iN); a }
  @targetName("auto_ranFill") inline def randomFill(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeI(a)(i0, iN); a }
  @targetName("that_ranFill") inline def randomFill(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeI(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeI(a)(iv.i0, iv.iN); a }
  @targetName("that_ranFill") inline def randomFill(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeI(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeI(a)(iv.i0, iv.iN); a }
  @targetName("that_randMod") inline def randomMod(m: Int)(r: Prng): a.type = { r.fillModI(m)(a); a }
  @targetName("auto_randMod") inline def randomMod(m: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillModI(m)(a); a }
  @targetName("that_randMod") inline def randomMod(m: Int)(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeModI(m)(a)(i0, iN); a }
  @targetName("auto_randMod") inline def randomMod(m: Int)(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeModI(m)(a)(i0, iN); a }
  @targetName("that_randMod") inline def randomMod(m: Int)(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeModI(m)(a)(iv.i0, iv.iN); a }
  @targetName("auto_randMod") inline def randomMod(m: Int)(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeModI(m)(a)(iv.i0, iv.iN); a }
  @targetName("that_randMod") inline def randomMod(m: Int)(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeModI(m)(a)(iv.i0, iv.iN); a }
  @targetName("auto_randMod") inline def randomMod(m: Int)(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeModI(m)(a)(iv.i0, iv.iN); a }

extension (a: Array[Long])
  @targetName("that_ranFill") inline def randomFill(r: Prng): a.type = { r.fillL(a); a }
  @targetName("auto_ranFill") inline def randomFill(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillL(a); a }
  @targetName("that_ranFill") inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeL(a)(i0, iN); a }
  @targetName("auto_ranFill") inline def randomFill(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeL(a)(i0, iN); a }
  @targetName("that_ranFill") inline def randomFill(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeL(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeL(a)(iv.i0, iv.iN); a }
  @targetName("that_ranFill") inline def randomFill(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeL(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeL(a)(iv.i0, iv.iN); a }
  @targetName("that_randMod") inline def randomMod(m: Long)(r: Prng): a.type = { r.fillModL(m)(a); a }
  @targetName("auto_randMod") inline def randomMod(m: Long)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillModL(m)(a); a }
  @targetName("that_randMod") inline def randomMod(m: Long)(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeModL(m)(a)(i0, iN); a }
  @targetName("auto_randMod") inline def randomMod(m: Long)(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeModL(m)(a)(i0, iN); a }
  @targetName("that_randMod") inline def randomMod(m: Long)(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeModL(m)(a)(iv.i0, iv.iN); a }
  @targetName("auto_randMod") inline def randomMod(m: Long)(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeModL(m)(a)(iv.i0, iv.iN); a }
  @targetName("that_randMod") inline def randomMod(m: Long)(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeModL(m)(a)(iv.i0, iv.iN); a }
  @targetName("auto_randMod") inline def randomMod(m: Long)(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeModL(m)(a)(iv.i0, iv.iN); a }

extension (a: Array[Float])
  @targetName("that_ranFill") inline def randomFill(r: Prng): a.type = { r.fillF(a); a }
  @targetName("auto_ranFill") inline def randomFill(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillF(a); a }
  @targetName("that_ranFill") inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeF(a)(i0, iN); a }
  @targetName("auto_ranFill") inline def randomFill(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeF(a)(i0, iN); a }
  @targetName("that_ranFill") inline def randomFill(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeF(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeF(a)(iv.i0, iv.iN); a }
  @targetName("that_ranFill") inline def randomFill(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeF(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeF(a)(iv.i0, iv.iN); a }

extension (a: Array[Double])
  @targetName("that_ranFill") inline def randomFill(r: Prng): a.type = { r.fillD(a); a }
  @targetName("auto_ranFill") inline def randomFill(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillD(a); a }
  @targetName("that_ranFill") inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeD(a)(i0, iN); a }
  @targetName("auto_ranFill") inline def randomFill(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeD(a)(i0, iN); a }
  @targetName("that_ranFill") inline def randomFill(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeD(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeD(a)(iv.i0, iv.iN); a }
  @targetName("that_ranFill") inline def randomFill(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeD(a)(iv.i0, iv.iN); a }
  @targetName("auto_ranFill") inline def randomFill(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeD(a)(iv.i0, iv.iN); a }
  @targetName("that_randGau") inline def randomGaussian(r: Prng): a.type = { r.fillGaussian(a); a }
  @targetName("auto_randGau") inline def randomGaussian(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillGaussian(a); a }
  @targetName("that_randGau") inline def randomGaussian(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeGaussian(a)(i0, iN); a }
  @targetName("auto_randGau") inline def randomGaussian(i0: Int, iN: Int)(using ar: AutoPrng): a.type = { AutoPrng.get(ar).fillRangeGaussian(a)(i0, iN); a }
  @targetName("that_randGau") inline def randomGaussian(inline rg: Rg)(r: Prng): a.type = { val iv = Iv of rg; r.fillRangeGaussian(a)(iv.i0, iv.iN); a }
  @targetName("auto_randGau") inline def randomGaussian(inline rg: Rg)(using ar: AutoPrng): a.type = { val iv = Iv of rg; AutoPrng.get(ar).fillRangeGaussian(a)(iv.i0, iv.iN); a }
  @targetName("that_randGau") inline def randomGaussian(inline v: Iv.X)(r: Prng): a.type = { val iv = v of a; r.fillRangeGaussian(a)(iv.i0, iv.iN); a }
  @targetName("auto_randGau") inline def randomGaussian(inline v: Iv.X)(using ar: AutoPrng): a.type = { val iv = v of a; AutoPrng.get(ar).fillRangeGaussian(a)(iv.i0, iv.iN); a }

extension (a: String)
  // inline def %(r: Prng): Char = r.sample(a)    ===>   In OverloadedExtensions
  @targetName("that_sample") inline def sample()(r: Prng): Char = r.sample(a)
  @targetName("auto_sample") inline def sample()(using ar: AutoPrng): Char = AutoPrng.get(ar).sample(a)
  @targetName("that_sample") inline def sample(i0: Int, iN: Int)(r: Prng): Char = r.sampleRange(a)(i0, iN)
  @targetName("auto_sample") inline def sample(i0: Int, iN: Int)(using ar: AutoPrng): Char = AutoPrng.get(ar).sampleRange(a)(i0, iN)
  @targetName("that_sample") inline def sample(inline rg: Rg)(r: Prng): Char = r.sampleRange(a)(rg)
  @targetName("auto_sample") inline def sample(inline rg: Rg)(using ar: AutoPrng): Char = AutoPrng.get(ar).sampleRange(a)(rg)
  @targetName("that_sample") inline def sample(inline v: Iv.X)(r: Prng): Char = r.sampleRange(a)(v)
  @targetName("auto_sample") inline def sample(inline v: Iv.X)(using ar: AutoPrng): Char = AutoPrng.get(ar).sampleRange(a)(v)
  @targetName("that_sample") inline def sample(k: Int)(r: Prng): String = r.sample(k)(a)
  @targetName("auto_sample") inline def sample(k: Int)(using ar: AutoPrng): String = AutoPrng.get(ar).sample(k)(a)
  @targetName("that_sample") inline def sample(k: Int)(i0: Int, iN: Int)(r: Prng): String = r.sampleRange(k)(a)(i0, iN)
  @targetName("auto_sample") inline def sample(k: Int)(i0: Int, iN: Int)(using ar: AutoPrng): String = AutoPrng.get(ar).sampleRange(k)(a)(i0, iN)
  @targetName("that_sample") inline def sample(k: Int)(inline rg: Rg)(r: Prng): String = r.sampleRange(k)(a)(rg)
  @targetName("auto_sample") inline def sample(k: Int)(inline rg: Rg)(using ar: AutoPrng): String = AutoPrng.get(ar).sampleRange(k)(a)(rg)
  @targetName("that_sample") inline def sample(k: Int)(inline v: Iv.X)(r: Prng): String = r.sampleRange(k)(a)(v)
  @targetName("auto_sample") inline def sample(k: Int)(inline v: Iv.X)(using ar: AutoPrng): String = AutoPrng.get(ar).sampleRange(k)(a)(v)

extension [A, CC <: scala.collection.IterableOnce[A]](coll: CC)
  @targetName("that_coll_sample") inline def sample()(r: Prng): A = inline coll match
    case ix: IndexedSeq[A] => ix.apply(r % ix.length)
    case _ => sampleOneAlgorithmL(r)
  @targetName("auto_coll_sample") inline def sample()(using ar: AutoPrng): A = sample()(AutoPrng.get(ar))
  @targetName("that_ii_sample") inline def sample(k: Int)(r: Prng)(using factory: scala.collection.Factory[A, CC]): CC = inline coll match
    case ix: IndexedSeq[A] => r.sampleIndexedSeq(k)(ix)(using factory.asInstanceOf[scala.collection.Factory[A, IndexedSeq[A]]]).asInstanceOf[CC]
    case _                 => r.sampleCollection(k)(coll)(using factory)
  @targetName("auto_ii_sample") inline def sample(k: Int)(using ar: AutoPrng, factory: scala.collection.Factory[A, CC]): CC = sample(k)(AutoPrng.get(ar))
  def sampleOneAlgorithmL(rng: Prng): A =
      // Li algorithmL specialized to a single sample
      val i = coll.iterator
      var a = i.next
      var w = rng.D
      while i.hasNext do
        var h = math.floor(math.log(rng.D)/math.log(1-w)).toInt
        while h > 0 && i.hasNext do
          i.next
          h -= 1
        if i.hasNext then
          a = i.next
          w *= rng.D
      a

extension (i: Int)(using ar: AutoPrng)
  inline def roll: Int = 1 + (AutoPrng.get(ar) % i)
  inline infix def d(m: Int): Int =
    var sum = 0
    var j = 0
    while j < i do
      sum += 1 + (AutoPrng.get(ar) % m)
      j += 1
    sum
  inline infix def d(m: Long): Long =
    var sum = 0L
    var j = 0
    while j < i do
      sum += 1L + (AutoPrng.get(ar) % m)
      j += 1
    sum

extension (l: Long)(using ar: AutoPrng)
  inline def roll: Long = 1L + (AutoPrng.get(ar) % l)
