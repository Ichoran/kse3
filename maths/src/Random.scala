// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016, 2021-22 by Rex Kerr and Calico Life Sciences LLC

package kse.maths

import java.lang.{Math => jm}


/** Random number generator intended to be fast, low-state, but reasonably well-distributed.
 *  You can serialize this into a few Longs--in many cases only one Long!--when the cache is
 *  empty.  You can also create copies including the cache.
 */
sealed abstract class Prng {
  protected final var cache: Long = 0L
  protected final var bits: Int = 0

  def copy: Prng

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

  final def gaussianVc: kse.maths.packed.Vc =
    val x = D*2 - 1
    val y = D*2 - 1
    val rr = x*x + y*y
    if rr >= 1 then gaussianVc
    else 
      val scale = jm.sqrt( (-2 * jm.log(rr)) / rr )
      kse.maths.packed.Vc.from(x * scale, y * scale)

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

  final def fillRangeZ(            target: Array[Boolean], i0: Int, iN: Int): this.type =
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

  final def fillRangeB(            target: Array[Byte],    i0: Int, iN: Int): this.type =
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

  final def fillRangeS(            target: Array[Short],   i0: Int, iN: Int): this.type =
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

  final def fillRangeC(            target: Array[Char],    i0: Int, iN: Int): this.type =
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

  final def fillRangeI(            target: Array[Int],     i0: Int, iN: Int): this.type =
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

  final def fillRangeL(            target: Array[Long],    i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = L
      i += 1
    this

  final def fillRangeF(            target: Array[Float],   i0: Int, iN: Int): this.type =
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

  final def fillRangeD(            target: Array[Double],  i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = D
      i += 1
    this

  final def fillRangeModI(m: Int)( target: Array[Int],     i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = this % m
      i += 1
    this

  final def fillRangeModL(m: Long)(target: Array[Long],    i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = this % m
      i += 1
    this

  final def fillRangeGaussian(     target: Array[Double],  i0: Int, iN: Int): this.type =
    var i = if i0 < 0             then 0             else i0
    val k = if iN > target.length then target.length else iN
    while i < k do
      target(i) = gaussian
      i += 1
    this

  final inline def fillZ(            target: Array[Boolean]): this.type = fillRangeZ(            target, 0, target.length)
  final inline def fillB(            target: Array[Byte]   ): this.type = fillRangeB(            target, 0, target.length)
  final inline def fillS(            target: Array[Short]  ): this.type = fillRangeS(            target, 0, target.length)
  final inline def fillC(            target: Array[Char]   ): this.type = fillRangeC(            target, 0, target.length)
  final inline def fillI(            target: Array[Int]    ): this.type = fillRangeI(            target, 0, target.length)
  final inline def fillL(            target: Array[Long]   ): this.type = fillRangeL(            target, 0, target.length)
  final inline def fillF(            target: Array[Float]  ): this.type = fillRangeF(            target, 0, target.length)
  final inline def fillD(            target: Array[Double] ): this.type = fillRangeD(            target, 0, target.length)
  final inline def fillModI(m: Int)( target: Array[Int]    ): this.type = fillRangeModI(m: Int)( target, 0, target.length)
  final inline def fillModL(m: Long)(target: Array[Long]   ): this.type = fillRangeModL(m: Long)(target, 0, target.length)
  final inline def fillGaussian(     target: Array[Double] ): this.type = fillRangeGaussian(     target, 0, target.length)

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

  final def shuffleRange(a: Array[Boolean], i0: Int, iN: Int): Unit =
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

  final def shuffleRange(a: Array[Byte], i0: Int, iN: Int): Unit =
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

  final def shuffleRange(a: Array[Short], i0: Int, iN: Int): Unit =
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

  final def shuffleRange(a: Array[Char], i0: Int, iN: Int): Unit =
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

  final def shuffleRange(a: Array[Int], i0: Int, iN: Int): Unit =
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

  final def shuffleRange(a: Array[Float], i0: Int, iN: Int): Unit =
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

  final def shuffleRange(a: Array[Long], i0: Int, iN: Int): Unit =
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

  final def shuffleRange(a: Array[Double], i0: Int, iN: Int): Unit =
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

  final def shuffleRange[A <: AnyRef](a: Array[A], i0: Int, iN: Int): Unit =
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

  final inline def shuffle(             a: Array[Boolean]): Unit = shuffleRange(a, 0, a.length)
  final inline def shuffle(             a: Array[Byte]   ): Unit = shuffleRange(a, 0, a.length)
  final inline def shuffle(             a: Array[Short]  ): Unit = shuffleRange(a, 0, a.length)
  final inline def shuffle(             a: Array[Char]   ): Unit = shuffleRange(a, 0, a.length)
  final inline def shuffle(             a: Array[Int]    ): Unit = shuffleRange(a, 0, a.length)
  final inline def shuffle(             a: Array[Long]   ): Unit = shuffleRange(a, 0, a.length)
  final inline def shuffle(             a: Array[Float]  ): Unit = shuffleRange(a, 0, a.length)
  final inline def shuffle(             a: Array[Double] ): Unit = shuffleRange(a, 0, a.length)
  final inline def shuffle[A <: AnyRef](a: Array[A]      ): Unit = shuffleRange(a, 0, a.length)

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
      sb append (B & 0xFF).toChar
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
          sb append ((c & 0x3FF) + java.lang.Character.MIN_LOW_SURROGATE).toChar
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
      val mantissa = ((l >>> 11) << leadingZeros) & 0x000FFFFFFFFFFFFFL
      java.lang.Double.longBitsToDouble( (exponent << 52) | mantissa )
    else 4.8828125E-4*l + 2.44140625E-4   // Subnormal values aren't symmetric, so we remap them equally spaced between 0 and 1

  def apply(): Prng = new Pcg64()
  def apply(seed: Long): Prng = new Pcg64(seed)

  inline val WebCharacters = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-._~"
  inline val TextCharacters = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\n"
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


extension (a: Array[Boolean])
  inline def shuffle(r: Prng): a.type = { r.shuffle(a); a }
  inline def shuffle(i0: Int, iN: Int)(r: Prng): a.type = { r.shuffleRange(a, i0, iN); a }
  inline def randomFill(r: Prng): a.type = { r.fillZ(a); a }
  inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeZ(a, i0, iN); a }

extension (a: Array[Byte])
  inline def shuffle(r: Prng): a.type = { r.shuffle(a); a }
  inline def shuffle(i0: Int, iN: Int)(r: Prng): a.type = { r.shuffleRange(a, i0, iN); a }
  inline def randomFill(r: Prng): a.type = { r.fillB(a); a }
  inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeB(a, i0, iN); a }

extension (a: Array[Short])
  inline def shuffle(r: Prng): a.type = { r.shuffle(a); a }
  inline def shuffle(i0: Int, iN: Int)(r: Prng): a.type = { r.shuffleRange(a, i0, iN); a }
  inline def randomFill(r: Prng): a.type = { r.fillS(a); a }
  inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeS(a, i0, iN); a }

extension (a: Array[Char])
  inline def shuffle(r: Prng): a.type = { r.shuffle(a); a }
  inline def shuffle(i0: Int, iN: Int)(r: Prng): a.type = { r.shuffleRange(a, i0, iN); a }
  inline def randomFill(r: Prng): a.type = { r.fillC(a); a }
  inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeC(a, i0, iN); a }

extension (a: Array[Int])
  inline def shuffle(r: Prng): a.type = { r.shuffle(a); a }
  inline def shuffle(i0: Int, iN: Int)(r: Prng): a.type = { r.shuffleRange(a, i0, iN); a }
  inline def randomFill(r: Prng): a.type = { r.fillI(a); a }
  inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeI(a, i0, iN); a }
  inline def randomMod(m: Int)(r: Prng): a.type = { r.fillModI(m)(a); a }
  inline def randomMod(m: Int)(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeModI(m)(a, i0, iN); a }

extension (a: Array[Long])
  inline def shuffle(r: Prng): a.type = { r.shuffle(a); a }
  inline def shuffle(i0: Int, iN: Int)(r: Prng): a.type = { r.shuffleRange(a, i0, iN); a }
  inline def randomFill(r: Prng): a.type = { r.fillL(a); a }
  inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeL(a, i0, iN); a }
  inline def randomMod(m: Long)(r: Prng): a.type = { r.fillModL(m)(a); a }
  inline def randomMod(m: Long)(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeModL(m)(a, i0, iN); a }

extension (a: Array[Float])
  inline def shuffle(r: Prng): a.type = { r.shuffle(a); a }
  inline def shuffle(i0: Int, iN: Int)(r: Prng): a.type = { r.shuffleRange(a, i0, iN); a }
  inline def randomFill(r: Prng): a.type = { r.fillF(a); a }
  inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeF(a, i0, iN); a }

extension (a: Array[Double])
  inline def shuffle(r: Prng): a.type = { r.shuffle(a); a }
  inline def shuffle(i0: Int, iN: Int)(r: Prng): a.type = { r.shuffleRange(a, i0, iN); a }
  inline def randomFill(r: Prng): a.type = { r.fillD(a); a }
  inline def randomFill(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeD(a, i0, iN); a }
  inline def randomGaussian(r: Prng): a.type = { r.fillGaussian(a); a }
  inline def randomGaussian(i0: Int, iN: Int)(r: Prng): a.type = { r.fillRangeGaussian(a, i0, iN); a }

extension [A <: AnyRef](a: Array[A])
  inline def shuffle(r: Prng): a.type = { r.shuffle(a); a }
  inline def shuffle(i0: Int, iN: Int)(r: Prng): a.type = { r.shuffleRange(a, i0, iN); a }

extension (i: Int)
  inline def roll(using r: Prng): Int = 1 + (r % i)
  inline def d(m: Int)(using r: Prng): Int =
    var sum = 0
    var j = 0
    while j < i do
      sum += 1 + (r % m)
      j += 1
    sum
  inline def d(m: Long)(using r: Prng): Long =
    var sum = 0L
    var j = 0
    while j < i do
      sum += 1L + (r % m)
      j += 1
    sum

extension (l: Long)
  inline def roll(using r: Prng): Long = 1L + (r % l)
