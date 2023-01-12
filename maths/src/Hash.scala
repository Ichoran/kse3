// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015-16, 2021-23 by Rex Kerr and Calico Life Sciences LLC
//
// Contains code ported from xxHash C source (by Yann Collet, "Cyan5973")
//   See https://github.com/Cyan4973/xxHash
// Contains code ported from MurmurHash C++ source (by Austin Appleby)
//   See https://github.com/aappleby/smhasher

package kse.maths


import java.lang.Integer.{rotateLeft => rotl32, rotateRight => rotr32 }
import java.lang.Long.{rotateLeft => rotl64, rotateRight => rotr64 }
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.atomic.AtomicReference


trait SimpleIncrementalHash {
  def begin(): this.type
  def append(bb: ByteBuffer): this.type
  def append(ab: Array[Byte], i0: Int, iN: Int): this.type
  def append(s: String, i0: Int, iN: Int): this.type
  def appendByte(b: Byte): this.type
  def appendChar(c: Char): this.type
  def appendInt(i: Int): this.type
  def appendLong(l: Long): this.type

  inline final def +=(z: Boolean):      this.type = appendByte(if z then 1 else 0)
  inline final def +=(b: Byte):         this.type = appendByte(b)
  inline final def +=(s: Short):        this.type = appendChar(s.toChar)
  inline final def +=(c: Char):         this.type = appendChar(c)
  inline final def +=(i: Int):          this.type = appendInt(i)
  inline final def +=(l: Long):         this.type = appendLong(l)
  inline final def +=(f: Float):        this.type = appendInt(java.lang.Float.floatToRawIntBits(f))
  inline final def +=(d: Double):       this.type = appendLong(java.lang.Double.doubleToRawLongBits(d))
  inline final def +=(ab: Array[Byte]): this.type = append(ab, 0, ab.length)
  inline final def +=(s: String):       this.type = append(s, 0, s.length)
}
object SimpleIncrementalHash {
  final class AlreadyFinalizedException(msg: String) extends Exception(msg) {}
  protected[maths] def fzerr(msg: String = ""): Nothing = throw new AlreadyFinalizedException(msg)
}

trait HashInto[Z] extends SimpleIncrementalHash {
  def result(bb: ByteBuffer): Z
  def result(ab: Array[Byte], i0: Int, iN: Int): Z
  def result(s: String, i0: Int, iN: Int): Z
  def result(): Z
  def copy: HashInto[Z]
}

trait SeededIncrementalHash[A] extends SimpleIncrementalHash {
  def begin(seed: A): this.type  
}

trait IncrementalHash[A, Z] extends HashInto[Z] with SeededIncrementalHash[A] {
  final def freshHash(seed: A, bb: ByteBuffer): Z = begin(seed).result(bb)
  final def freshHash(bb: ByteBuffer): Z = begin().result(bb)
  final def freshHash(seed: A, ab: Array[Byte], i0: Int, iN: Int): Z = begin(seed).result(ab, i0, iN)
  final def freshHash(seed: A, ab: Array[Byte]): Z = begin(seed).result(ab, 0, ab.length)
  final def freshHash(ab: Array[Byte], i0: Int, iN: Int): Z = begin().result(ab, i0, iN)
  final def freshHash(ab: Array[Byte]): Z = begin().result(ab, 0, ab.length)
  final def freshHash(seed: A, s: String, i0: Int, iN: Int): Z = begin(seed).result(s, i0, iN)
  final def freshHash(seed: A, s: String): Z = begin(seed).result(s, 0, s.length)
  final def freshHash(s: String, i0: Int, iN: Int): Z = begin().result(s, i0, iN)
  final def freshHash(s: String): Z = begin().result(s, 0, s.length)
  def copy: IncrementalHash[A, Z]
}


trait FullHash32 {
  def hash32(seed: Int, ab: Array[Byte], i0: Int, iN: Int): Int
  inline final def hash32(seed: Int, a: Array[Byte]): Int = hash32(seed, a, 0, a.length)
  inline final def hash32(a: Array[Byte], i0: Int, iN: Int): Int = hash32(0, a, i0, iN)
  inline final def hash32(a: Array[Byte]): Int = hash32(0, a, 0, a.length)

  def hash32(seed: Int, bb: ByteBuffer): Int
  inline final def hash32(bb: ByteBuffer): Int = hash32(0, bb)

  def hash32(seed: Int, s: String, i0: Int, iN: Int): Int
  inline final def hash32(seed: Int, s: String): Int = hash32(seed, s, 0, s.length)
  inline final def hash32(s: String, i0: Int, iN: Int): Int = hash32(0, s, i0, iN)
  inline final def hash32(s: String): Int = hash32(0, s, 0, s.length)
}

trait Hash32 extends FullHash32 with IncrementalHash[Int, Int] {
  def hash32(seed: Int, bb: ByteBuffer): Int = begin(seed).result(bb)
  def hash32(seed: Int, ab: Array[Byte], i0: Int, iN: Int): Int = begin(seed).result(ab, i0, iN)
  def hash32(seed: Int, s: String, i0: Int, iN: Int) = begin(seed).result(s, i0, iN)
  def begin(): this.type = begin(0)
  def begin(seed: Int): this.type
  def result(bb: ByteBuffer): Int
  def result(ab: Array[Byte], i0: Int, iN: Int): Int
  def result(s: String, i0: Int, iN: Int): Int
  def result(): Int
}


trait FullHash64 {
  def hash64(seed: Long, ab: Array[Byte], i0: Int, iN: Int): Long
  inline final def hash64(seed: Long, ab: Array[Byte]): Long = hash64(seed, ab, 0, ab.length)
  inline final def hash64(ab: Array[Byte], i0: Int, iN: Int): Long = hash64(0L, ab, i0, iN)
  inline final def hash64(ab: Array[Byte]): Long = hash64(0L, ab, 0, ab.length)

  def hash64(seed: Long, bb: ByteBuffer): Long
  inline def hash64(bb: ByteBuffer): Long = hash64(0L, bb)

  def hash64(seed: Long, s: String, i0: Int, iN: Int): Long
  inline final def hash64(seed: Long, s: String): Long = hash64(seed, s, 0, s.length)
  inline final def hash64(s: String, i0: Int, iN: Int): Long = hash64(0L, s, i0, iN)
  inline final def hash64(s: String): Long = hash64(0L, s, 0, s.length)
}

trait Hash64 extends FullHash64 with IncrementalHash[Long, Long] {
  def hash64(seed: Long, bb: ByteBuffer): Long = begin(seed).result(bb)
  def hash64(seed: Long, ab: Array[Byte], i0: Int, iN: Int): Long = begin(seed).result(ab, i0, iN)
  def hash64(seed: Long, s: String, i0: Int, iN: Int): Long = begin(seed).result(s, i0, iN)
  def begin(): this.type = begin(0L)
  def begin(seed: Long): this.type
  def result(bb: ByteBuffer): Long
  def result(ab: Array[Byte], i0: Int, iN: Int): Long
  def result(s: String, i0: Int, iN: Int): Long
  def result(): Long
}


case class HashCode128(hash0: Long, hash1: Long) {
  def toArray: Array[Long] =
    val a = new Array[Long](2)
    a(0) = hash0
    a(1) = hash1
    a
  def toVector: Vector[Long] = Vector(hash0, hash1)
}
object HashCode128 {
  val empty = new HashCode128(0, 0)
}

trait FullHash128 {
  def hash128(seed0: Long, seed1: Long, ab: Array[Byte], i0: Int, iN: Int): HashCode128
  inline final def hash128(seed0: Long, seed1: Long, ab: Array[Byte]): HashCode128 = hash128(seed0, seed1, ab, 0, ab.length)
  inline final def hash128(ab: Array[Byte], i0: Int, iN: Int): HashCode128 = hash128(0L, 0L, ab, i0, iN)
  inline final def hash128(ab: Array[Byte]): HashCode128 = hash128(0L, 0L, ab, 0, ab.length)

  def hash128(seed0: Long, seed1: Long, bb: ByteBuffer): HashCode128
  inline def hash128(bb: ByteBuffer): HashCode128 = hash128(0L, 0L, bb)

  def hash128(seed0: Long, seed1: Long, s: String, i0: Int, iN: Int): HashCode128
  inline final def hash128(seed0: Long, seed1: Long, s: String): HashCode128 = hash128(seed0, seed1, s, 0, s.length)
  inline final def hash128(s: String, i0: Int, iN: Int): HashCode128 = hash128(0L, 0L, s, i0, iN)
  inline final def hash128(s: String): HashCode128 = hash128(0L, 0L, s, 0, s.length)  
}

trait Hash128 extends FullHash128 with IncrementalHash[HashCode128, HashCode128] {
  def hash128(seed0: Long, seed1: Long, bb: ByteBuffer): HashCode128 = begin(seed0, seed1).result(bb)
  def hash128(seed0: Long, seed1: Long, ab: Array[Byte], i0: Int, iN: Int): HashCode128 = begin(seed0, seed1).result(ab, i0, iN)
  def hash128(seed0: Long, seed1: Long, s: String, i0: Int, iN: Int): HashCode128 = begin(seed0, seed1).result(s, i0, iN)  
  def begin(): this.type = begin(0L, 0L)
  def begin(seed0: Long, seed1: Long): this.type
}


final class XxHash32() extends Hash32 {
  import XxHash.{Prime32_1, Prime32_2, Prime32_3, Prime32_4, Prime32_5}
  private[this] var v1: Int = Prime32_1 + Prime32_2
  private[this] var v2: Int = Prime32_2
  private[this] var v3: Int = 0
  private[this] var v4: Int = -Prime32_1
  private[this] var v5: Int = 0
  private[this] var hadBlock: Boolean = false
  private[this] var finalized: Boolean = false
  private[this] var myBuffer: ByteBuffer = null    // Do NOT mark--can't copy cleanly in that case

  private def mimicState(u1: Int, u2: Int, u3: Int, u4: Int, u5: Int, had: Boolean, fz: Boolean, bb: ByteBuffer): Unit =
    v1 = u1
    v2 = u2
    v3 = u3
    v4 = u4
    v5 = u5
    hadBlock = had
    finalized = fz
    if bb eq null then
      myBuffer = null
    else
      myBuffer = ByteBuffer.wrap(java.util.Arrays.copyOf(bb.array, 16))
      myBuffer order ByteOrder.LITTLE_ENDIAN
      myBuffer limit bb.limit
      myBuffer position bb.position

  def copy: XxHash32 =
    val ans = new XxHash32()
    ans.mimicState(v1, v2, v3, v4, v5, hadBlock, finalized, myBuffer)
    ans

  def begin(seed: Int): this.type =
    finalized = false
    v1 = seed + Prime32_1 + Prime32_2
    v2 = seed + Prime32_2
    v3 = seed
    v4 = seed - Prime32_1
    v5 = 0
    hadBlock = false
    if (myBuffer ne null) myBuffer.clear
    this

  private[this] inline def createBufferIfNeeded(): Boolean =
    if myBuffer eq null then
      myBuffer = ByteBuffer allocate 16
      myBuffer order ByteOrder.LITTLE_ENDIAN
      true
    else
      false
  
  private[this] def appendBy16(bb: ByteBuffer): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XXhash32 hasher finalized (use begin() or begin(seed) to reuse)")
    var x1 = v1
    var x2 = v2
    var x3 = v3
    var x4 = v4
    if bb.remaining >= 16 then
      hadBlock = true
      v5 += (bb.remaining & 0xFFFFFFF0)
    while bb.remaining >= 16 do
      x1 = rotl32(x1 + bb.getInt * Prime32_2, 13) * Prime32_1
      x2 = rotl32(x2 + bb.getInt * Prime32_2, 13) * Prime32_1
      x3 = rotl32(x3 + bb.getInt * Prime32_2, 13) * Prime32_1
      x4 = rotl32(x4 + bb.getInt * Prime32_2, 13) * Prime32_1
    v1 = x1
    v2 = x2
    v3 = x3
    v4 = x4
    this
  
  private[this] def appendIx4(one: Int, two: Int, three: Int, four: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XXhash32 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl32(v1 +   one * Prime32_2, 13) * Prime32_1
    v2 = rotl32(v2 +   two * Prime32_2, 13) * Prime32_1
    v3 = rotl32(v3 + three * Prime32_2, 13) * Prime32_1
    v4 = rotl32(v4 +  four * Prime32_2, 13) * Prime32_1
    v5 += 16
    hadBlock = true
    this

  private[this] def appendMyBuffer(): Unit =
    myBuffer.flip()
    appendIx4(myBuffer.getInt, myBuffer.getInt, myBuffer.getInt, myBuffer.getInt)
    myBuffer.clear()
  
  private[this] def counting(extra: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XXhash32 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = if (!hadBlock) v3 + Prime32_5 else rotl32(v1, 1) + rotl32(v2, 7) + rotl32(v3, 12) + rotl32(v4, 18)
    v1 += v5 + extra
    this
  
  private[this] def trailing(one: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XXhash32 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl32(v1 + one * Prime32_3, 17) * Prime32_4
    this
  
  private[this] def trailing(quarter: Byte): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XXhash32 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl32(v1 + (quarter&0xFF) * Prime32_5, 11) * Prime32_1
    this      
  
  private[this] def complete(): Int =
    if finalized then v1
    else
      finalized = true
      var h32 = v1
      h32 ^= h32 >>> 15
      h32 *= Prime32_2
      h32 ^= h32 >>> 13
      h32 *= Prime32_3
      v1 = h32 ^ (h32 >>> 16)
      v1
  
  def append(bb: ByteBuffer): this.type =
    bb order ByteOrder.LITTLE_ENDIAN
    if (myBuffer ne null) && (myBuffer.position > 0) then
      while myBuffer.position <= 12 && bb.remaining >= 4 do myBuffer.putInt(bb.getInt)
      while myBuffer.position <  16 && bb.remaining >= 1 do myBuffer.put(bb.get)
      if myBuffer.position == 16 then appendMyBuffer()
    if bb.remaining >= 16 then appendBy16(bb)
    if bb.remaining > 0 then
      if myBuffer eq null then
        myBuffer = ByteBuffer allocate 16
        myBuffer order ByteOrder.LITTLE_ENDIAN
      while bb.remaining >= 4 do myBuffer.putInt(bb.getInt)
      while bb.remaining >= 1 do myBuffer.put(bb.get)
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = if iN > ab.length then ab.length else iN
    if (myBuffer ne null) && myBuffer.position > 0 then
      while i < j && myBuffer.remaining > 0 do
        myBuffer put ab(i)
        i += 1
      if myBuffer.remaining == 0 then appendMyBuffer()
    while i <= j-16 do
      val x0 = (ab(i   )&0xFF) | ((ab(i+ 1)&0xFF)<<8) | ((ab(i+ 2)&0xFF)<<16) | ((ab(i+ 3)&0xFF)<<24)
      val x1 = (ab(i+ 4)&0xFF) | ((ab(i+ 5)&0xFF)<<8) | ((ab(i+ 6)&0xFF)<<16) | ((ab(i+ 7)&0xFF)<<24)
      val x2 = (ab(i+ 8)&0xFF) | ((ab(i+ 9)&0xFF)<<8) | ((ab(i+10)&0xFF)<<16) | ((ab(i+11)&0xFF)<<24)
      val x3 = (ab(i+12)&0xFF) | ((ab(i+13)&0xFF)<<8) | ((ab(i+14)&0xFF)<<16) | ((ab(i+15)&0xFF)<<24)
      appendIx4(x0, x1, x2, x3)
      i += 16
    if i < j then
      createBufferIfNeeded()
      while i < j do
        myBuffer put ab(i)
        i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = math.min(iN, s.length)
    if (myBuffer ne null) && myBuffer.position > 0 then
      if (myBuffer.position % 2) != 0 then
        while i < j do
          appendChar(s charAt i)
          i += 1
        return this
      else
        while i < j && myBuffer.remaining > 0 do
          myBuffer putChar s.charAt(i)
          i += 1
        if myBuffer.remaining == 0 then appendMyBuffer()
    while i <= j-8 do
      appendIx4(
        s.charAt(i  ) | (s.charAt(i+1) << 16),
        s.charAt(i+2) | (s.charAt(i+3) << 16),
        s.charAt(i+4) | (s.charAt(i+5) << 16),
        s.charAt(i+6) | (s.charAt(i+7) << 16),
      )
      i += 8
    if i < j then
      createBufferIfNeeded()
      while i < j do
        myBuffer putChar s.charAt(i)
        i += 1
    this
  
  def appendLong(l: Long): this.type =
    if createBufferIfNeeded() then
      myBuffer putLong l
    else if myBuffer.remaining >= 8 then
      myBuffer putLong l
      if myBuffer.remaining == 0 then
        appendMyBuffer()
    else if myBuffer.remaining > 4 then
      myBuffer putInt (l & 0xFFFFFFFFL).toInt
      myAppendInt((l >>> 32).toInt)
    else
      myAppendInt((l & 0xFFFFFFFFL).toInt)
      myBuffer putInt ((l >>> 32).toInt)
    this

  private[this] def myAppendInt(i: Int): Unit = 
    if myBuffer.remaining >= 4 then
      myBuffer putInt i
      if myBuffer.remaining == 0 then
        appendMyBuffer()
    else if myBuffer.remaining >= 2 then
      myBuffer putChar i.toChar
      if myBuffer.remaining == 0 then
        appendMyBuffer()
        myBuffer putChar (i >>> 16).toChar
      else
        myAppendChar((i >>> 16).toChar)
    else if myBuffer.remaining == 1 then
      myBuffer put (i & 0xFF).toByte
      appendMyBuffer()
      myBuffer put ((i >>> 8) & 0xFF).toByte
      myBuffer putChar (i >>> 16).toChar
    else
      appendMyBuffer()
      myBuffer putInt i

  def appendInt(i: Int): this.type =
    if createBufferIfNeeded() then
      myBuffer putInt i
    else
      myAppendInt(i)
    this

  private[this] def myAppendChar(c: Char): Unit =
    if myBuffer.remaining >= 2 then
      myBuffer putChar c
      if myBuffer.remaining == 0 then
        appendMyBuffer()
    else if myBuffer.remaining == 1 then
      myBuffer put (c & 0xFF).toByte
      appendMyBuffer()
      myBuffer put (c >>> 8).toByte
    else
      appendMyBuffer()
      myBuffer putChar c

  def appendChar(c: Char): this.type =
    if createBufferIfNeeded() then
      myBuffer putChar c
    else
      myAppendChar(c)
    this

  def appendByte(b: Byte): this.type =
    if createBufferIfNeeded() || myBuffer.remaining > 1 then
      myBuffer put b
    else if myBuffer.remaining == 1 then
      myBuffer put b
      appendMyBuffer()
    else
      appendMyBuffer()
      myBuffer put b
    this

  def result(bb: ByteBuffer): Int =
    val terminal =
      if (myBuffer ne null) && (myBuffer.position > 0) then
        append(bb)
        myBuffer.flip
        if (myBuffer.remaining == 16) appendIx4(myBuffer.getInt, myBuffer.getInt, myBuffer.getInt, myBuffer.getInt)
        myBuffer
      else
        bb order ByteOrder.LITTLE_ENDIAN
        if bb.remaining >= 16 then appendBy16(bb)
        bb
    counting(terminal.remaining)
    while terminal.remaining >= 4 do trailing(terminal.getInt)
    while terminal.remaining >= 1 do trailing(terminal.get)
    if terminal eq myBuffer then myBuffer.clear
    complete()

  def result(ab: Array[Byte], i0: Int, iN: Int): Int =
    // TODO--can rewrite to save buffer allocation if buffer is null
    append(ab, i0, iN)
    result()

  def result(s: String, i0: Int, iN: Int): Int =
    // TODO--can rewrite to save buffer allocation if buffer is null
    append(s, i0, iN)
    result()
  
  def result(): Int =
    if (myBuffer ne null) && myBuffer.position > 0 then
      myBuffer.flip
      counting(myBuffer.remaining)
      while myBuffer.remaining >= 4 do trailing(myBuffer.getInt)
      while myBuffer.remaining >= 1 do trailing(myBuffer.get)
      myBuffer.clear()
    else if !finalized then
      counting(0)
    complete()
}


final class XxHash64() extends Hash64 {
  import XxHash.{Prime64_1, Prime64_2, Prime64_3, Prime64_4, Prime64_5}
  private[this] var v1: Long = Prime64_1 + Prime64_2
  private[this] var v2: Long = Prime64_2
  private[this] var v3: Long = 0
  private[this] var v4: Long = -Prime64_1
  private[this] var v5: Long = 0
  private[this] var hadBlock: Boolean = false
  private[this] var finalized: Boolean = false
  private[this] var myBuffer: ByteBuffer = null    // Do NOT mark--can't copy cleanly in that case

  private def mimicState(u1: Long, u2: Long, u3: Long, u4: Long, u5: Long, had: Boolean, fz: Boolean, bb: ByteBuffer): Unit =
    v1 = u1
    v2 = u2
    v3 = u3
    v4 = u4
    v5 = u5
    hadBlock = had
    finalized = fz
    if bb eq null then myBuffer = null
    else
      myBuffer = ByteBuffer.wrap(java.util.Arrays.copyOf(bb.array, 32))
      myBuffer order ByteOrder.LITTLE_ENDIAN
      myBuffer limit bb.limit
      myBuffer position bb.position

  def copy: XxHash64 =
    val ans = new XxHash64()
    ans.mimicState(v1, v2, v3, v4, v5, hadBlock, finalized, myBuffer)
    ans

  def begin(seed: Long): this.type =
    finalized = false
    v1 = seed + Prime64_1 + Prime64_2
    v2 = seed + Prime64_2
    v3 = seed
    v4 = seed - Prime64_1
    v5 = 0
    hadBlock = false
    if myBuffer ne null then myBuffer.clear
    this

  private[this] def createBufferIfNeeded(): Boolean =
    if myBuffer eq null then
      myBuffer = ByteBuffer allocate 32
      myBuffer order ByteOrder.LITTLE_ENDIAN
      true
    else
      false
  
  private[this] def appendBy32(bb: ByteBuffer): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XxHash64 hasher finalized (use begin() or begin(seed) to reuse)")
    var x1 = v1
    var x2 = v2
    var x3 = v3
    var x4 = v4
    if bb.remaining >= 32 then
      hadBlock = true
      v5 += (bb.remaining & 0xFFFFFFE0)
    while bb.remaining >= 32 do
      x1 = rotl64(x1 + bb.getLong * Prime64_2, 31) * Prime64_1
      x2 = rotl64(x2 + bb.getLong * Prime64_2, 31) * Prime64_1
      x3 = rotl64(x3 + bb.getLong * Prime64_2, 31) * Prime64_1
      x4 = rotl64(x4 + bb.getLong * Prime64_2, 31) * Prime64_1
    v1 = x1
    v2 = x2
    v3 = x3
    v4 = x4
    this        
  
  private[this] def appendLx4(one: Long, two: Long, three: Long, four: Long): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XxHash64 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl64(v1 +   one * Prime64_2, 31) * Prime64_1
    v2 = rotl64(v2 +   two * Prime64_2, 31) * Prime64_1
    v3 = rotl64(v3 + three * Prime64_2, 31) * Prime64_1
    v4 = rotl64(v4 +  four * Prime64_2, 31) * Prime64_1
    v5 += 32
    hadBlock = true
    this

  private[this] inline def appendMyBuffer(): Unit =
    myBuffer.flip()
    appendLx4(myBuffer.getLong, myBuffer.getLong, myBuffer.getLong, myBuffer.getLong)
    myBuffer.clear()
  
  private[this] def counting(extra: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XxHash64 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 =
      if !hadBlock then v3 + Prime64_5
      else
        var x = rotl64(v1, 1) + rotl64(v2, 7) + rotl64(v3, 12) + rotl64(v4, 18)
        x ^= rotl64(v1 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v2 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v3 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v4 * Prime64_2, 31) * Prime64_1
        x*Prime64_1 + Prime64_4
    v1 += v5 + extra
    this
  
  private[this] def trailing(one: Long): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XxHash64 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl64(v1 ^ (rotl64(one * Prime64_2, 31) * Prime64_1), 27)*Prime64_1 + Prime64_4
    this
  
  private[this] def trailing(one: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XxHash64 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl64(v1 ^ ((one & 0xFFFFFFFFL) * Prime64_1), 23) * Prime64_2 + Prime64_3
    this
  
  private[this] def trailing(quarter: Byte): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XxHash64 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl64(v1 ^ ((quarter & 0xFF) * Prime64_5), 11) * Prime64_1
    this      
  
  private[this] def complete(): Long =
    if finalized then v1
    else
      finalized = true
      var h64 = v1
      h64 ^= h64 >>> 33
      h64 *= Prime64_2
      h64 ^= h64 >>> 29
      h64 *= Prime64_3
      v1 = h64 ^ (h64 >>> 32) 
      v1     
  
  def append(bb: ByteBuffer): this.type =
    bb order ByteOrder.LITTLE_ENDIAN
    if (myBuffer ne null) && (myBuffer.position > 0) then
      while myBuffer.position <= 24 && bb.remaining >= 8 do myBuffer.putLong(bb.getLong)
      while myBuffer.position < 32 && bb.remaining >= 1 do myBuffer.put(bb.get)
      if myBuffer.position == 32 then
        myBuffer.flip()
        appendLx4(myBuffer.getLong, myBuffer.getLong, myBuffer.getLong, myBuffer.getLong)
        myBuffer.clear()
    if bb.remaining >= 32 then appendBy32(bb)
    if bb.remaining > 0 then
      if myBuffer eq null then
        myBuffer = ByteBuffer allocate 32
        myBuffer order ByteOrder.LITTLE_ENDIAN
      while bb.remaining >= 8 do myBuffer.putLong(bb.getLong)
      while bb.remaining >= 1 do myBuffer.put(bb.get)
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = if iN > ab.length then ab.length else iN
    if (myBuffer ne null) && myBuffer.position > 0 then
      while i < j && myBuffer.remaining > 0 do
        myBuffer put ab(i)
        i += 1
      if myBuffer.remaining == 0 then appendMyBuffer()
    while i <= j-32 do
      val x0 = (ab(i   )&0xFF) | ((ab(i+ 1)&0xFF)<<8) | ((ab(i+ 2)&0xFF)<<16) | ((ab(i+ 3)&0xFF)<<24)
      val x1 = (ab(i+ 4)&0xFF) | ((ab(i+ 5)&0xFF)<<8) | ((ab(i+ 6)&0xFF)<<16) | ((ab(i+ 7)&0xFF)<<24)
      val x2 = (ab(i+ 8)&0xFF) | ((ab(i+ 9)&0xFF)<<8) | ((ab(i+10)&0xFF)<<16) | ((ab(i+11)&0xFF)<<24)
      val x3 = (ab(i+12)&0xFF) | ((ab(i+13)&0xFF)<<8) | ((ab(i+14)&0xFF)<<16) | ((ab(i+15)&0xFF)<<24)
      val x4 = (ab(i+16)&0xFF) | ((ab(i+17)&0xFF)<<8) | ((ab(i+18)&0xFF)<<16) | ((ab(i+19)&0xFF)<<24)
      val x5 = (ab(i+20)&0xFF) | ((ab(i+21)&0xFF)<<8) | ((ab(i+22)&0xFF)<<16) | ((ab(i+23)&0xFF)<<24)
      val x6 = (ab(i+24)&0xFF) | ((ab(i+25)&0xFF)<<8) | ((ab(i+26)&0xFF)<<16) | ((ab(i+27)&0xFF)<<24)
      val x7 = (ab(i+28)&0xFF) | ((ab(i+29)&0xFF)<<8) | ((ab(i+30)&0xFF)<<16) | ((ab(i+31)&0xFF)<<24)
      appendLx4(
        (x0 & 0xFFFFFFFFL) | (x1.toLong << 32),
        (x2 & 0xFFFFFFFFL) | (x3.toLong << 32),
        (x4 & 0xFFFFFFFFL) | (x5.toLong << 32),
        (x6 & 0xFFFFFFFFL) | (x7.toLong << 32)
      )
      i += 32
    if i < j then
      createBufferIfNeeded()
      while i < j do
        myBuffer put ab(i)
        i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = math.min(iN, s.length)
    if (myBuffer ne null) && myBuffer.position > 0 then
      if (myBuffer.position % 2) != 0 then
        while i < j do
          appendChar(s charAt i)
          i += 1
        return this
      else
        while i < j && myBuffer.remaining > 0 do
          myBuffer putChar s.charAt(i)
          i += 1
        if myBuffer.remaining == 0 then appendMyBuffer()
    while i <= j-16 do
      appendLx4(
        s.charAt(i   ) | (s.charAt(i+ 1).toLong << 16) | (s.charAt(i+ 2).toLong << 32) | (s.charAt(i+ 3).toLong << 48),
        s.charAt(i+ 4) | (s.charAt(i+ 5).toLong << 16) | (s.charAt(i+ 6).toLong << 32) | (s.charAt(i+ 7).toLong << 48),
        s.charAt(i+ 8) | (s.charAt(i+ 9).toLong << 16) | (s.charAt(i+10).toLong << 32) | (s.charAt(i+11).toLong << 48),
        s.charAt(i+12) | (s.charAt(i+13).toLong << 16) | (s.charAt(i+14).toLong << 32) | (s.charAt(i+15).toLong << 48),
      )
      i += 16
    if i < j then
      createBufferIfNeeded()
      while i < j do
        myBuffer putChar s.charAt(i)
        i += 1
    this

  def appendLong(l: Long): this.type =
    if createBufferIfNeeded() then
      myBuffer putLong l
    else if myBuffer.remaining >= 8 then
      myBuffer putLong l
      if myBuffer.remaining == 0 then
        appendMyBuffer()
    else if myBuffer.remaining > 4 then
      myBuffer putInt (l & 0xFFFFFFFFL).toInt
      myAppendInt((l >>> 32).toInt)
    else
      myAppendInt((l & 0xFFFFFFFFL).toInt)
      myBuffer putInt ((l >>> 32).toInt)
    this

  private[this] def myAppendInt(i: Int): Unit = 
    if myBuffer.remaining >= 4 then
      myBuffer putInt i
      if myBuffer.remaining == 0 then
        appendMyBuffer()
    else if myBuffer.remaining >= 2 then
      myBuffer putChar i.toChar
      if myBuffer.remaining == 0 then
        appendMyBuffer()
        myBuffer putChar (i >>> 16).toChar
      else
        myAppendChar((i >>> 16).toChar)
    else if myBuffer.remaining == 1 then
      myBuffer put (i & 0xFF).toByte
      appendMyBuffer()
      myBuffer put ((i >>> 8) & 0xFF).toByte
      myBuffer putChar (i >>> 16).toChar
    else
      appendMyBuffer()
      myBuffer putInt i

  def appendInt(i: Int): this.type =
    if createBufferIfNeeded() then
      myBuffer putInt i
    else
      myAppendInt(i)
    this

  private[this] def myAppendChar(c: Char): Unit =
    if myBuffer.remaining >= 2 then
      myBuffer putChar c
      if myBuffer.remaining == 0 then
        appendMyBuffer()
    else if myBuffer.remaining == 1 then
      myBuffer put (c & 0xFF).toByte
      appendMyBuffer()
      myBuffer put (c >>> 8).toByte
    else
      appendMyBuffer()
      myBuffer putChar c

  def appendChar(c: Char): this.type =
    if createBufferIfNeeded() then
      myBuffer putChar c
    else
      myAppendChar(c)
    this

  def appendByte(b: Byte): this.type =
    if createBufferIfNeeded() || myBuffer.remaining > 1 then
      myBuffer put b
    else if myBuffer.remaining == 1 then
      myBuffer put b
      appendMyBuffer()
    else
      appendMyBuffer()
      myBuffer put b
    this

  def result(bb: ByteBuffer): Long =
    val terminal =
      if (myBuffer ne null) && (myBuffer.position > 0) then
        append(bb)
        myBuffer.flip()
        if (myBuffer.remaining == 36) appendLx4(myBuffer.getLong, myBuffer.getLong, myBuffer.getLong, myBuffer.getLong)
        myBuffer
      else 
        bb order ByteOrder.LITTLE_ENDIAN
        if bb.remaining >= 32 then appendBy32(bb)
        bb
    counting(terminal.remaining)
    while terminal.remaining >= 8 do trailing(terminal.getLong)
    if terminal.remaining >= 4 then trailing(terminal.getInt)
    while terminal.remaining >= 1 do trailing(terminal.get)
    if terminal eq myBuffer then myBuffer.clear
    complete()

  def result(ab: Array[Byte], i0: Int, iN: Int): Long =
    // TODO--could avoid allocating myBuffer if it is null
    append(ab, i0, iN)
    result()
  
  def result(s: String, i0: Int, iN: Int): Long =
    // TODO--could avoid allocating myBuffer if it is null
    append(s, i0, iN)
    result()
  
  def result(): Long =
    if (myBuffer ne null) && (myBuffer.position > 0) then
      myBuffer.flip()
      counting(myBuffer.remaining)
      while myBuffer.remaining >= 8 do trailing(myBuffer.getLong)
      if myBuffer.remaining >= 4 then trailing(myBuffer.getInt)
      while myBuffer.remaining >= 1 do trailing(myBuffer.get)
      myBuffer.clear()
    else if !finalized then
      counting(0)
    complete()
}


object XxHash extends FullHash32 with FullHash64 {
  inline val Prime32_1 = 0x9e3779b1 // 2654435761
  inline val Prime32_2 = 0x85ebca77 // 2246822519
  inline val Prime32_3 = 0xc2b2ae3d // 3266489917
  inline val Prime32_4 = 0x27d4eb2f //  668265263
  inline val Prime32_5 = 0x165667b1 //  374761393
  inline val Prime64_1 = 0x9e3779b185ebca87L // 11400714785074694791L
  inline val Prime64_2 = 0xc2b2ae3d27d4eb4fL // 14029467366897019727L
  inline val Prime64_3 = 0x165667b19e3779f9L //  1609587929392839161L
  inline val Prime64_4 = 0x85ebca77c2b2ae63L //  9650029242287828579L
  inline val Prime64_5 = 0x27d4eb2f165667c5L //  2870177450012600261L

  def hash32(seed: Int, a: Array[Byte], i0: Int, iN: Int): Int =
    val iM = math.min(a.length, iN)
    var i = math.max(0, i0)
    val len = math.max(iM - i, 0)
    var h32 =
      if i > iM - 16 then seed + Prime32_5
      else
        var v1 = seed + Prime32_1 + Prime32_2
        var v2 = seed + Prime32_2
        var v3 = seed
        var v4 = seed - Prime32_1
        var more = true
        while more do
          v1 += ((a(i)&0xFF) | ((a(i+1)&0xFF) << 8 ) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24)) * Prime32_2
          v1 = rotl32(v1, 13)
          v1 *= Prime32_1
          i += 4
          v2 += ((a(i)&0xFF) | ((a(i+1)&0xFF) << 8 ) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24)) * Prime32_2
          v2 = rotl32(v2, 13)
          v2 *= Prime32_1
          i += 4
          v3 += ((a(i)&0xFF) | ((a(i+1)&0xFF) << 8 ) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24)) * Prime32_2
          v3 = rotl32(v3, 13)
          v3 *= Prime32_1
          i += 4
          v4 += ((a(i)&0xFF) | ((a(i+1)&0xFF) << 8 ) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24)) * Prime32_2
          v4 = rotl32(v4, 13)
          v4 *= Prime32_1
          i += 4
          more = i <= iM - 16
        rotl32(v1, 1) + rotl32(v2, 7) + rotl32(v3, 12) + rotl32(v4, 18)
    h32 += len
    while i <= iM - 4 do
      h32 += ((a(i)&0xFF) | ((a(i+1)&0xFF) << 8 ) | ((a(i+2)&0xFF) << 16) | (a(i+3) << 24)) * Prime32_3
      h32 = rotl32(h32, 17) * Prime32_4
      i += 4
    while i < iM do
      h32 += (a(i) & 0xFF) * Prime32_5
      h32 = rotl32(h32, 11) * Prime32_1
      i += 1
    h32 ^= h32 >>> 15
    h32 *= Prime32_2
    h32 ^= h32 >>> 13
    h32 *= Prime32_3
    h32 ^ (h32 >>> 16)

  def hash32(seed: Int, s: String, i0: Int, iN: Int): Int =
    val iM = math.min(s.length, iN)
    var i = math.max(0, i0)
    val len = math.max(iM - i, 0)
    var h32 =
      if iM - i < 8 then seed + Prime32_5
      else
        var v1 = seed + Prime32_1 + Prime32_2
        var v2 = seed + Prime32_2
        var v3 = seed
        var v4 = seed - Prime32_1
        while iM - i >= 8 do
          v1 += (s.charAt(i) | (s.charAt(i+1) << 16)) * Prime32_2
          v1 = rotl32(v1, 13)
          v1 *= Prime32_1
          i += 2
          v2 += (s.charAt(i) | (s.charAt(i+1) << 16)) * Prime32_2
          v2 = rotl32(v2, 13)
          v2 *= Prime32_1
          i += 2
          v3 += (s.charAt(i) | (s.charAt(i+1) << 16)) * Prime32_2
          v3 = rotl32(v3, 13)
          v3 *= Prime32_1
          i += 2
          v4 += (s.charAt(i) | (s.charAt(i+1) << 16)) * Prime32_2
          v4 = rotl32(v4, 13)
          v4 *= Prime32_1
          i += 2
        rotl32(v1, 1) + rotl32(v2, 7) + rotl32(v3, 12) + rotl32(v4, 18)
    h32 += 2*len
    while iM - i >= 2 do
      h32 += (s.charAt(i) | (s.charAt(i+1) << 16)) * Prime32_3
      h32 = rotl32(h32, 17) * Prime32_4
      i += 2
    while i < iM do
      val c = s charAt i
      h32 += (c & 0xFF) * Prime32_5
      h32 = rotl32(h32, 11) * Prime32_1
      h32 += (c >> 8) * Prime32_5
      h32 = rotl32(h32, 11) * Prime32_1
      i += 1
    h32 ^= h32 >>> 15
    h32 *= Prime32_2
    h32 ^= h32 >>> 13
    h32 *= Prime32_3
    h32 ^ (h32 >>> 16)

  def hash32(seed: Int, bb: ByteBuffer): Int =
    bb order ByteOrder.LITTLE_ENDIAN
    val len = bb.remaining
    var h32 =
      if bb.remaining < 16 then seed + Prime32_5
      else
        var v1 = seed + Prime32_1 + Prime32_2
        var v2 = seed + Prime32_2
        var v3 = seed
        var v4 = seed - Prime32_1
        var more = true
        while more do
          v1 += bb.getInt * Prime32_2
          v1 = rotl32(v1, 13)
          v1 *= Prime32_1
          v2 += bb.getInt * Prime32_2
          v2 = rotl32(v2, 13)
          v2 *= Prime32_1
          v3 += bb.getInt * Prime32_2
          v3 = rotl32(v3, 13)
          v3 *= Prime32_1
          v4 += bb.getInt * Prime32_2
          v4 = rotl32(v4, 13)
          v4 *= Prime32_1
          more = bb.remaining >= 16
        rotl32(v1, 1) + rotl32(v2, 7) + rotl32(v3, 12) + rotl32(v4, 18)
    h32 += len
    while bb.remaining >= 4 do
      h32 += bb.getInt * Prime32_3
      h32 = rotl32(h32, 17) * Prime32_4
    while bb.hasRemaining do
      h32 += (bb.get & 0xFF) * Prime32_5
      h32 = rotl32(h32, 11) * Prime32_1
    h32 ^= h32 >>> 15
    h32 *= Prime32_2
    h32 ^= h32 >>> 13
    h32 *= Prime32_3
    h32 ^ (h32 >>> 16)

  def hash64(seed: Long, bb: ByteBuffer): Long =
    bb order ByteOrder.LITTLE_ENDIAN
    val len = bb.remaining
    var h64 =
      if bb.remaining < 32 then seed + Prime64_5
      else
        var v1 = seed + Prime64_1 + Prime64_2
        var v2 = seed + Prime64_2
        var v3 = seed
        var v4 = seed - Prime64_1
        var more = true
        while more do
          v1 += bb.getLong * Prime64_2
          v1 = rotl64(v1, 31)
          v1 *= Prime64_1
          v2 += bb.getLong * Prime64_2
          v2 = rotl64(v2, 31)
          v2 *= Prime64_1
          v3 += bb.getLong * Prime64_2
          v3 = rotl64(v3, 31)
          v3 *= Prime64_1
          v4 += bb.getLong * Prime64_2
          v4 = rotl64(v4, 31)
          v4 *= Prime64_1
          more = bb.remaining >= 32
        var x = rotl64(v1, 1) + rotl64(v2, 7) + rotl64(v3, 12) + rotl64(v4, 18)
        x ^= rotl64(v1 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v2 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v3 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v4 * Prime64_2, 31) * Prime64_1
        x*Prime64_1 + Prime64_4
    h64 += len
    while bb.remaining >= 8 do
      h64 ^= rotl64(bb.getLong * Prime64_2, 31) * Prime64_1
      h64 = rotl64(h64, 27)*Prime64_1 + Prime64_4
    if bb.remaining >= 4 then
      h64 ^= (bb.getInt & 0xFFFFFFFFL) * Prime64_1
      h64 = rotl64(h64, 23) * Prime64_2 + Prime64_3
    while bb.hasRemaining do
      h64 ^= (bb.get & 0xFF) * Prime64_5
      h64 = rotl64(h64, 11) * Prime64_1
    h64 ^= h64 >>> 33
    h64 *= Prime64_2
    h64 ^= h64 >>> 29
    h64 *= Prime64_3
    h64 ^ (h64 >>> 32)

  def hash64(seed: Long, ab: Array[Byte], i0: Int, iN: Int): Long = 
    val iM = math.min(ab.length, iN)
    var i = math.max(0, i0)
    val len = math.max(iM - i, 0)
    var h64 =
      if iM - i < 32 then seed + Prime64_5
      else
        var v1 = seed + Prime64_1 + Prime64_2
        var v2 = seed + Prime64_2
        var v3 = seed
        var v4 = seed - Prime64_1
        while iM - i >= 32 do
          val x0 = (ab(i   )&0xFF) | ((ab(i+ 1)&0xFF)<<8) | ((ab(i+ 2)&0xFF)<<16) | ((ab(i+ 3)&0xFF)<<24)
          val x1 = (ab(i+ 4)&0xFF) | ((ab(i+ 5)&0xFF)<<8) | ((ab(i+ 6)&0xFF)<<16) | ((ab(i+ 7)&0xFF)<<24)
          v1 += ((x0 & 0xFFFFFFFFL) | (x1.toLong << 32)) * Prime64_2
          v1 = rotl64(v1, 31)
          v1 *= Prime64_1
          val x2 = (ab(i+ 8)&0xFF) | ((ab(i+ 9)&0xFF)<<8) | ((ab(i+10)&0xFF)<<16) | ((ab(i+11)&0xFF)<<24)
          val x3 = (ab(i+12)&0xFF) | ((ab(i+13)&0xFF)<<8) | ((ab(i+14)&0xFF)<<16) | ((ab(i+15)&0xFF)<<24)
          v2 += ((x2 & 0xFFFFFFFFL) | (x3.toLong << 32)) * Prime64_2
          v2 = rotl64(v2, 31)
          v2 *= Prime64_1
          val x4 = (ab(i+16)&0xFF) | ((ab(i+17)&0xFF)<<8) | ((ab(i+18)&0xFF)<<16) | ((ab(i+19)&0xFF)<<24)
          val x5 = (ab(i+20)&0xFF) | ((ab(i+21)&0xFF)<<8) | ((ab(i+22)&0xFF)<<16) | ((ab(i+23)&0xFF)<<24)
          v3 += ((x4 & 0xFFFFFFFFL) | (x5.toLong << 32)) * Prime64_2
          v3 = rotl64(v3, 31)
          v3 *= Prime64_1
          val x6 = (ab(i+24)&0xFF) | ((ab(i+25)&0xFF)<<8) | ((ab(i+26)&0xFF)<<16) | ((ab(i+27)&0xFF)<<24)
          val x7 = (ab(i+28)&0xFF) | ((ab(i+29)&0xFF)<<8) | ((ab(i+30)&0xFF)<<16) | ((ab(i+31)&0xFF)<<24)
          v4 += ((x6 & 0xFFFFFFFFL) | (x7.toLong << 32)) * Prime64_2
          v4 = rotl64(v4, 31)
          v4 *= Prime64_1
          i += 32
        var x = rotl64(v1, 1) + rotl64(v2, 7) + rotl64(v3, 12) + rotl64(v4, 18)
        x ^= rotl64(v1 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v2 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v3 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v4 * Prime64_2, 31) * Prime64_1
        x*Prime64_1 + Prime64_4
    h64 += len
    while iM - i >= 8 do
      val x0 = (ab(i  )&0xFF) | ((ab(i+1)&0xFF)<<8) | ((ab(i+2)&0xFF)<<16) | ((ab(i+3)&0xFF)<<24)
      val x1 = (ab(i+4)&0xFF) | ((ab(i+5)&0xFF)<<8) | ((ab(i+6)&0xFF)<<16) | ((ab(i+7)&0xFF)<<24)
      h64 ^= rotl64(((x0 & 0xFFFFFFFFL) | (x1.toLong << 32)) * Prime64_2, 31) * Prime64_1
      h64 = rotl64(h64, 27)*Prime64_1 + Prime64_4
      i += 8
    if iM - i >= 4 then
      h64 ^= (((ab(i  )&0xFF) | ((ab(i+1)&0xFF)<<8) | ((ab(i+2)&0xFF)<<16) | ((ab(i+3)&0xFF)<<24)) & 0xFFFFFFFFL) * Prime64_1
      h64 = rotl64(h64, 23) * Prime64_2 + Prime64_3
      i += 4
    while i < iM do
      h64 ^= (ab(i) & 0xFF) * Prime64_5
      h64 = rotl64(h64, 11) * Prime64_1
      i += 1
    h64 ^= h64 >>> 33
    h64 *= Prime64_2
    h64 ^= h64 >>> 29
    h64 *= Prime64_3
    h64 ^ (h64 >>> 32)

  def hash64(seed: Long, s: String, i0: Int, iN: Int): Long = 
    val iM = math.min(s.length, iN)
    var i = math.max(0, i0)
    val len = math.max(iM - i, 0)
    var h64 =
      if iM - i < 16 then seed + Prime64_5
      else
        var v1 = seed + Prime64_1 + Prime64_2
        var v2 = seed + Prime64_2
        var v3 = seed
        var v4 = seed - Prime64_1
        while iM - i >= 16 do
          v1 += (s.charAt(i) | (s.charAt(i+1).toLong << 16) | (s.charAt(i+2).toLong << 32) | (s.charAt(i+3).toLong << 48)) * Prime64_2
          v1 = rotl64(v1, 31)
          v1 *= Prime64_1
          i += 4
          v2 += (s.charAt(i) | (s.charAt(i+1).toLong << 16) | (s.charAt(i+2).toLong << 32) | (s.charAt(i+3).toLong << 48)) * Prime64_2
          v2 = rotl64(v2, 31)
          v2 *= Prime64_1
          i += 4
          v3 += (s.charAt(i) | (s.charAt(i+1).toLong << 16) | (s.charAt(i+2).toLong << 32) | (s.charAt(i+3).toLong << 48)) * Prime64_2
          v3 = rotl64(v3, 31)
          v3 *= Prime64_1
          i += 4
          v4 += (s.charAt(i) | (s.charAt(i+1).toLong << 16) | (s.charAt(i+2).toLong << 32) | (s.charAt(i+3).toLong << 48)) * Prime64_2
          v4 = rotl64(v4, 31)
          v4 *= Prime64_1
          i += 4
        var x = rotl64(v1, 1) + rotl64(v2, 7) + rotl64(v3, 12) + rotl64(v4, 18)
        x ^= rotl64(v1 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v2 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v3 * Prime64_2, 31) * Prime64_1
        x = x*Prime64_1 + Prime64_4
        x ^= rotl64(v4 * Prime64_2, 31) * Prime64_1
        x*Prime64_1 + Prime64_4
    h64 += 2*len
    while iM - i >= 4 do
      val v = s.charAt(i) | (s.charAt(i+1).toLong << 16) | (s.charAt(i+2).toLong << 32) | (s.charAt(i+3).toLong << 48)
      h64 ^= rotl64(v * Prime64_2, 31) * Prime64_1
      h64 = rotl64(h64, 27)*Prime64_1 + Prime64_4
      i += 4
    if iM - i >= 2 then
      val v = s.charAt(i) | (s.charAt(i+1) << 16)
      h64 ^= (v & 0xFFFFFFFFL) * Prime64_1
      h64 = rotl64(h64, 23) * Prime64_2 + Prime64_3
      i += 2
    while i < iM do
      val c = s.charAt(i)
      h64 ^= (c & 0xFF) * Prime64_5
      h64 = rotl64(h64, 11) * Prime64_1
      h64 ^= (c >>> 8) * Prime64_5
      h64 = rotl64(h64, 11) * Prime64_1
      i += 1
    h64 ^= h64 >>> 33
    h64 *= Prime64_2
    h64 ^= h64 >>> 29
    h64 *= Prime64_3
    h64 ^ (h64 >>> 32)
}




/// Austin Appleby's MurmurHash3, commit 92cf370 -- x86 32 bit algorithm
final class MurmurHash32() extends Hash32 {
  private[this] var state = 0
  private[this] var n = 0
  private[this] var partial = 0
  private[this] var partialN = 0
  private[this] var finalized = false

  private def mimicState(st: Int, m: Int, p: Int, pN: Int, fz: Boolean): Unit =
    state = st
    n = m
    partial = p
    partialN = pN
    finalized = fz

  def copy: MurmurHash32 =
    val ans = new MurmurHash32
    ans.mimicState(state, n, partial, partialN, finalized)
    ans

  private[this] def appendI(i: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("MurmurHash32 hasher finalized (use begin() or begin(seed) to reuse)")
    n += 4
    val x = state ^ (0x1B873593 * rotl32(i * 0xCC9E2D51, 15))
    state = (5 * rotl32(x, 13)) + 0xE6546B64
    this

  private[this] def appendLastI(i: Int, bytes: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("MurmurHash32 hasher finalized (use begin() or begin(seed) to reuse)")
    n += (bytes&3)
    state = state ^ (0x1B873593 * rotl32(i * 0xCC9E2D51, 15))
    this

  private[this] def finalizer(): Unit =
    if !finalized then
      val x = state ^ n
      val y = 0x85EBCA6B * (x ^ (x >>> 16))
      val z = 0xC2B2AE35 * (y ^ (y >>> 13))
      state = z ^ (z >>> 16)
      finalized = true

  def begin(seed: Int): this.type =
    finalized = false
    state = seed
    n = 0
    partial = 0
    partialN = 0
    this

  def append(bb: ByteBuffer): this.type =
    bb order ByteOrder.LITTLE_ENDIAN
    if partialN > 0 then
      while partialN < 4 && bb.hasRemaining do
        partial |= (bb.get & 0xFF) << (partialN*8)
        partialN += 1
      if partialN == 4 then
        appendI(partial)
        partialN = 0
        partial = 0
    while bb.remaining >= 4 do
      appendI(bb.getInt)
    while bb.hasRemaining do
      partial |= (bb.get & 0xFF) << (partialN*8)
      partialN += 1
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = if iN <= ab.length then iN else ab.length
    while i < j && partialN > 0 do
      partial = partial | ((ab(i) & 0xFF) << (8 * partialN))
      partialN += 1
      if partialN >= 4 then
        appendI(partial)
        partial = 0
        partialN = 0
      i += 1
    while j - i >= 4 do
      appendI((ab(i  )&0xFF) | ((ab(i+1)&0xFF)<<8) | ((ab(i+2)&0xFF)<<16) | ((ab(i+3)&0xFF)<<24))
      i += 4
    while i < j do
      partial = partial | ((ab(i) & 0xFF) << (8 * partialN))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = math.max(0, i0)
    val iM = math.min(s.length, iN)
    if (partialN % 2) != 0 then
      while i < iM do
        appendChar(s charAt i)
        i += 1
      return this
    if partialN > 0 && i < iM then
      appendI(partial | (s.charAt(i) << 16))
      partial = 0
      partialN = 0
      i += 1
    while iM - i >= 2 do
      appendI(s.charAt(i) | (s.charAt(i+1) << 16))
      i += 2
    if i < iM then
      partial = s charAt i
      partialN = 2
    this

  def appendLong(l: Long): this.type =
    if partialN == 0 then
      appendI((l & 0xFFFFFFFFL).toInt)
      appendI((l >>> 32).toInt)
    else
      val sh = 8 * partialN
      appendI(partial | ((l & (0xFFFFFFFFL >>> sh)).toInt << sh))
      appendI(((l >>> (32 - sh)) & 0xFFFFFFFFL).toInt)
      partial = (l >>> (64 - sh)).toInt
    this

  def appendInt(i: Int): this.type =
    if partialN == 0 then
      appendI(i)
    else
      val sh = 8 * partialN
      appendI(partial | (i << sh))
      partial = i >>> (32 - sh)
    this

  def appendChar(c: Char): this.type =
    if partialN < 2 then
      partial = partial | (c.toInt << (8 * partialN))
      partialN += 2
    else if partialN == 2 then
      appendI(partial | (c.toInt << 16))
      partial = 0
      partialN = 0
    else
      appendI(partial | (c & 0xFF) << 24)
      partial = (c & 0xFF00) >>> 8
      partialN = 1
    this

  def appendByte(b: Byte): this.type =
    if partialN < 3 then
      partial = partial | ((b & 0xFF) << (8 * partialN))
      partialN += 1
    else
      appendI(partial | ((b & 0xFF) << 24))
      partial = 0
      partialN = 0
    this

  def result(bb: ByteBuffer): Int =
    append(bb)
    result()

  def result(ab: Array[Byte], i0: Int, iN: Int): Int =
    append(ab, i0, iN)
    result()

  def result(s: String, i0: Int, iN: Int): Int =
    append(s, i0, iN)
    result()

  def result(): Int =
    if partialN > 0 then
      appendLastI(partial, partialN)
      partialN = 0
      partial = 0
    finalizer()
    n = 0
    state
}


final class MurmurHash128() extends Hash128 with IncrementalHash[HashCode128, HashCode128] {
  private[this] var state0, state1 = 0L
  private[this] var partial0, partial1 = 0L
  private[this] var partialN = 0
  private[this] var n = 0
  private[this] var finalized = false

  private def mimicState(s0: Long, s1: Long, p0: Long, p1: Long, pN: Int, m: Int, fz: Boolean): Unit =
    state0 = s0
    state1 = s1
    partial0 = p0
    partial1 = p1
    partialN = pN
    n = m
    finalized = fz

  def copy: MurmurHash128 =
    val ans = new MurmurHash128
    ans.mimicState(state0, state1, partial0, partial1, partialN, n, finalized)
    ans

  override def begin(): this.type = begin(0L, 0L)
  def begin(seed: Long): this.type = begin(seed, 0L)
  def begin(seed: HashCode128): this.type = begin(seed.hash0, seed.hash1)
  def begin(seed0: Long, seed1: Long): this.type =
    state0 = seed0
    state1 = seed1
    partial0 = 0
    partial1 = 0
    partialN = 0
    n = 0
    finalized = false
    this

  private[this] def appendLx2(la: Long, lb: Long) =
    if finalized then SimpleIncrementalHash.fzerr("MurmurHash128 hasher finalized (use begin() or begin(seed0, seed1) to reuse)")
    n += 16
    val x0 = state0 ^ (0x4CF5AD432745937FL * rotl64(la * 0x87C37B91114253D5L, 31))
    state0 = ((rotl64(x0, 27) + state1) * 5) + 0x52DCE729
    val x1 = state1 ^ (0x87C37B91114253D5L * rotl64(lb * 0x4CF5AD432745937FL, 33))
    state1 = ((rotl64(x1, 31) + state0) * 5) + 0x38495AB5
    this
  
  private[this] def appendLastLx2(la: Long, lb: Long, bytes: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("MurmurHash128 hasher finalized (use begin() or begin(seed0, seed1) to reuse)")
    val m = bytes & 0xF
    n += m
    if (m > 8) state1 = state1 ^ (0x87C37B91114253D5L * rotl64(lb * 0x4CF5AD432745937FL, 33))
    state0 = state0 ^ (0x4CF5AD432745937FL * rotl64(la * 0x87C37B91114253D5L, 31))
    this
  
  private[this] def mixer(l: Long): Long =
    val x = 0xFF51AFD7ED558CCDL * (l ^ (l >>> 33))
    val y = 0xC4CEB9FE1A85EC53L * (x ^ (x >>> 33))
    y ^ (y >>> 33)
  
  private[this] def finalizer(): Unit =
    if !finalized then
      state0 ^= n
      state1 ^= n
      state0 += state1
      state1 += state0
      state0 = mixer(state0)
      state1 = mixer(state1)
      state0 += state1
      state1 += state0
      finalized = true

  def append(bb: ByteBuffer): this.type =
    bb order ByteOrder.LITTLE_ENDIAN
    if partialN > 0 then
      while bb.hasRemaining && partialN < 8 do
        partial0 |= ((bb.get & 0xFFL) << (partialN*8))
        partialN += 1
      while bb.hasRemaining && partialN < 16 do
        partial1 |= ((bb.get & 0xFFL) << ((partialN - 8) * 8))
        partialN += 1
      if partialN == 16 then
        appendLx2(partial0, partial1)
        partial0 = 0
        partial1 = 0
        partialN = 0
    while bb.remaining >= 16 do appendLx2(bb.getLong, bb.getLong)
    while bb.hasRemaining && partialN < 8 do
      partial0 |= ((bb.get & 0xFFL) << (partialN*8))
      partialN += 1
    while bb.hasRemaining && partialN < 16 do
      partial1 |= ((bb.get & 0xFFL) << ((partialN - 8) * 8))
      partialN += 1
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = if iN <= ab.length then iN else ab.length
    if partialN > 0 then
      while i < j && partialN < 8 do
        partial0 |= ((ab(i) & 0xFFL) << (partialN*8))
        partialN += 1
        i += 1
      while i < j && partialN < 16 do
        partial1 |= ((ab(i) & 0xFFL) << ((partialN - 8)*8))
        partialN += 1
        i += 1
      if partialN == 16 then
        appendLx2(partial0, partial1)
        partial0 = 0
        partial1 = 0
        partialN = 0
    while j - i >= 16 do
      val x0 = (ab(i   )&0xFF) | ((ab(i+ 1)&0xFF)<<8) | ((ab(i+ 2)&0xFF)<<16) | ((ab(i+ 3)&0xFF)<<24)
      val x1 = (ab(i+ 4)&0xFF) | ((ab(i+ 5)&0xFF)<<8) | ((ab(i+ 6)&0xFF)<<16) | ((ab(i+ 7)&0xFF)<<24)
      val x2 = (ab(i+ 8)&0xFF) | ((ab(i+ 9)&0xFF)<<8) | ((ab(i+10)&0xFF)<<16) | ((ab(i+11)&0xFF)<<24)
      val x3 = (ab(i+12)&0xFF) | ((ab(i+13)&0xFF)<<8) | ((ab(i+14)&0xFF)<<16) | ((ab(i+15)&0xFF)<<24)
      appendLx2((x0 & 0xFFFFFFFFL) | (x1.toLong << 32), (x2 & 0xFFFFFFFFL) | (x3.toLong << 32))
      i += 16
    while i < j && partialN < 8 do
      partial0 |= ((ab(i) & 0xFFL) << (partialN*8))
      partialN += 1
      i += 1
    while i < j do
      partial1 |= ((ab(i) & 0xFFL) << ((partialN - 8)*8))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = math.max(0, i0)
    val iM = math.min(s.length, iN)
    if partialN > 0 then
      if (partialN % 2) != 0 then
        while i < iM do
          appendChar(s charAt i)
          i += 1
        return this
      while i < iM && partialN < 8 do
        partial0 |= s.charAt(i).toLong << (8 * partialN)
        partialN += 2
        i += 1
      while i < iM && partialN < 16 do
        partial1 |= s.charAt(i).toLong << (8 * (partialN - 8))
        partialN += 2
        i += 1
      if partialN == 16 then
        appendLx2(partial0, partial1)
        partial0 = 0
        partial1 = 0
        partialN = 0
    while iM - i >= 8 do
     appendLx2(
       (s.charAt(i  ) | (s.charAt(i+1).toLong << 16) | (s.charAt(i+2).toLong << 32) | (s.charAt(i+3).toLong << 48)),
       (s.charAt(i+4) | (s.charAt(i+5).toLong << 16) | (s.charAt(i+6).toLong << 32) | (s.charAt(i+7).toLong << 48))
     )
     i += 8
    while i < iM && partialN < 8 do
      partial0 |= s.charAt(i).toLong << (8 * partialN)
      partialN += 2
      i += 1
    while i < iM do
      partial1 |= s.charAt(i).toLong << (8 * (partialN - 8))
      partialN += 2
      i += 1
    this

  def appendLong(l: Long): this.type =
    if partialN == 0 then
      partial0 = l
      partialN = 8
    else if partialN == 8 then
      appendLx2(partial0, l)
      partial0 = 0
      partialN = 0
    else if partialN < 8 then
      val sh = 8 * partialN
      partial0 = partial0 | (l << sh)
      partial1 = l >>> (64 - sh)
      partialN += 8
    else
      val sh = 8 * (partialN - 8)
      appendLx2(partial0, partial1 | (l << sh))
      partial0 = l >>> (64 - sh)
      partial1 = 0
      partialN -= 8
    this

  def appendInt(i: Int): this.type =
    if partialN < 8 then
      partial0 = partial0 | ((i & 0xFFFFFFFFL) << (8 * partialN))
      if partialN > 4 then
        partial1 = (i & 0xFFFFFFFFL) >>> (8 * (8 - partialN))
      partialN += 4
    else
      partial1 = partial1 | ((i & 0xFFFFFFFFL) << (8 * (partialN - 8)))
      if partialN >= 12 then
        appendLx2(partial0, partial1)
        partial1 = 0
        if partialN > 12 then
          partial0 = (i & 0xFFFFFFFFL) >>> (8 * (16 - partialN))
          partialN -= 12
        else
          partial0 = 0
          partialN = 0
      else
        partialN += 4
    this

  def appendChar(c: Char): this.type =
    if partialN < 8 then
      partial0 |= c.toLong << (8 * partialN)
      if partialN == 7 then
        partial1 = c.toLong >>> 8
      partialN += 2
    else
      partial1 |= c.toLong << (8 * (partialN - 8))
      if partialN >= 14 then
        appendLx2(partial0, partial1)
        partial1 = 0
        if partialN == 15 then
          partial0 = c.toLong >>> 8
          partialN = 1
        else
          partial0 = 0
          partialN = 0
      else
        partialN += 2
    this

  def appendByte(b: Byte): this.type =
    if partialN < 8 then
      partial0 = partial0 | ((b & 0xFFL) << (8 * partialN))
      partialN += 1
    else
      partial1 = partial1 | ((b & 0xFFL) << (8 * (partialN - 8)))
      if partialN < 15 then
        partialN += 1
      else
        appendLx2(partial0, partial1)
        partial0 = 0
        partial1 = 0
        partialN = 0
    this

  def result(bb: ByteBuffer): HashCode128 = append(bb).result()

  def result(ab: Array[Byte], i0: Int, iN: Int): HashCode128 = append(ab, i0, iN).result()

  def result(s: String, i0: Int, iN: Int): HashCode128 = append(s, i0, iN).result()

  def result(): HashCode128 =
    if partialN > 0 then
      appendLastLx2(partial0, partial1, partialN)
      partial0 = 0
      partial1 = 0
      partialN = 0
    finalizer()
    new HashCode128(state0, state1)
}


object MurmurHash extends FullHash32 with FullHash128 {
  private[this] val cached32 = new AtomicReference[MurmurHash32]()

  def hash32(seed: Int, bb: ByteBuffer): Int =
    val c = cached32.getAndSet(null)
    val h = if c eq null then new MurmurHash32() else c
    val result = h.hash32(seed, bb)
    cached32.set(h)
    result

  def hash32(seed: Int, ab: Array[Byte], i0: Int, iN: Int): Int =
    val c = cached32.getAndSet(null)
    val h = if c eq null then new MurmurHash32() else c
    val result = h.hash32(seed, ab, i0, iN)
    cached32.set(h)
    result

  def hash32(seed: Int, s: String, i0: Int, iN: Int): Int =
    val c = cached32.getAndSet(null)
    val h = if c eq null then new MurmurHash32() else c
    val result = h.hash32(seed, s, i0, iN)
    cached32.set(h)
    result

  private[this] val cached128 = new AtomicReference[MurmurHash128]()

  def hash128(seed0: Long, seed1: Long, bb: ByteBuffer): HashCode128 =
    val c = cached128.getAndSet(null)
    val h = if c eq null then new MurmurHash128() else c
    val result = h.hash128(seed0, seed1, bb)
    cached128.set(h)
    result

  def hash128(seed0: Long, seed1: Long, ab: Array[Byte], i0: Int, iN: Int): HashCode128 =
    val c = cached128.getAndSet(null)
    val h = if c eq null then new MurmurHash128() else c
    val result = h.hash128(seed0, seed1, ab, i0, iN)
    cached128.set(h)
    result

  def hash128(seed0: Long, seed1: Long, s: String, i0: Int, iN: Int): HashCode128 =
    val c = cached128.getAndSet(null)
    val h = if c eq null then new MurmurHash128() else c
    val result = h.hash128(seed0, seed1, s, i0, iN)
    cached128.set(h)
    result
}



final class SumHash32() extends Hash32 {
  private[this] var sum = 0
  private[this] var partial = 0
  private[this] var partialN = 0

  private def mimicState(s: Int, p: Int, pN: Int): Unit =
    sum = s
    partial = p
    partialN = pN

  def copy: SumHash32 =
    val ans = new SumHash32
    ans.mimicState(sum, partial, partialN)
    ans

  def begin(seed: Int): this.type =
    sum = seed
    partial = 0
    partialN = 0
    this

  def append(bb: ByteBuffer): this.type =
    bb order ByteOrder.LITTLE_ENDIAN
    if partialN > 0 then
      while partialN < 4 && bb.hasRemaining do
        partial |= (bb.get & 0xFF) << (partialN*8)
        partialN += 1
      if partialN == 4 then
        sum += partial
        partialN = 0
        partial = 0
    while bb.remaining >= 4 do sum += bb.getInt
    while bb.hasRemaining do
      partial |= (bb.get & 0xFF) << (partialN*8)
      partialN += 1
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = if iN <= ab.length then iN else ab.length
    while i < j && partialN > 0 do
      partial = partial | ((ab(i) & 0xFF) << (8 * partialN))
      partialN += 1
      if partialN >= 4 then
        sum += partial
        partial = 0
        partialN = 0
      i += 1
    while j - i >= 4 do
      sum += (ab(i  )&0xFF) | ((ab(i+1)&0xFF)<<8) | ((ab(i+2)&0xFF)<<16) | ((ab(i+3)&0xFF)<<24)
      i += 4
    while i < j do
      partial = partial | ((ab(i) & 0xFF) << (8 * partialN))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = math.max(0, i0)
    val iM = math.min(s.length, iN)
    if (partialN % 2) != 0 then
      while i < iM do
        appendChar(s charAt i)
        i += 1
      return this
    if partialN > 0 && i < iM then
      sum += partial | (s.charAt(i) << 16)
      partial = 0
      partialN = 0
      i += 1
    while iM - i >= 2 do
      sum += s.charAt(i) | (s.charAt(i+1) << 16)
      i += 2
    if i < iM then
      partial = s charAt i
      partialN = 2
    this

  def appendLong(l: Long): this.type =
    if partialN == 0 then
      sum += (l & 0xFFFFFFFFL).toInt
      sum += (l >>> 32).toInt
    else
      val sh = 8 * partialN
      sum += partial | ((l & (0xFFFFFFFFL >>> sh)).toInt << sh)
      sum += ((l >>> (32 - sh)) & 0xFFFFFFFFL).toInt
      partial = (l >>> (64 - sh)).toInt
    this

  def appendInt(i: Int): this.type =
    if partialN == 0 then
      sum += i
    else
      val sh = 8 * partialN
      sum += partial | (i << sh)
      partial = i >>> (32 - sh)
    this

  def appendChar(c: Char): this.type =
    if partialN < 2 then
      partial = partial | (c.toInt << (8 * partialN))
      partialN += 2
    else if partialN == 2 then
      sum += partial | (c.toInt << 16)
      partial = 0
      partialN = 0
    else
      sum += partial | ((c & 0xFF) << 24)
      partial = (c >>> 8)
      partialN = 1
    this

  def appendByte(b: Byte): this.type =
    if partialN >= 3 then
      sum += partial | ((b & 0xFF) << 24)
      partial = 0
      partialN = 0
    else
      partial = partial | ((b & 0xFF) << (8 * partialN))
      partialN += 1
    this

  def result(bb: ByteBuffer): Int =
    append(bb)
    result()

  def result(ab: Array[Byte], i0: Int, iN: Int): Int = append(ab, i0, iN).result()

  def result(s: String, i0: Int, iN: Int): Int = append(s, i0, iN).result()
  
  def result(): Int = sum + partial
}


final class SumHash64() extends Hash64 {
  private[this] var sum = 0L
  private[this] var partial = 0L
  private[this] var partialN = 0

  private def mimicState(s: Long, p: Long, pN: Int): Unit =
    sum = s
    partial = p
    partialN = pN

  def copy: SumHash64 =
    val ans = new SumHash64
    ans.mimicState(sum, partial, partialN)
    ans

  def begin(seed: Long): this.type =
    sum = seed
    partial = 0
    partialN = 0
    this

  def append(bb: ByteBuffer): this.type =
    bb order ByteOrder.LITTLE_ENDIAN
    if partialN > 0 then
      while partialN < 8 && bb.hasRemaining do
        partial |= (bb.get & 0xFFL) << (partialN*8)
        partialN += 1
      if partialN == 8 then
        sum += partial
        partialN = 0
        partial = 0
    while bb.remaining >= 8 do sum += bb.getLong
    while bb.hasRemaining do
      partial |= (bb.get & 0xFFL) << (partialN*8)
      partialN += 1
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = if iN <= ab.length then iN else ab.length
    while i < j && partialN > 0 do
      partial = partial | ((ab(i) & 0xFFL) << (8 * partialN))
      partialN += 1
      if partialN >= 8 then
        sum += partial
        partial = 0
        partialN = 0
      i += 1
    while j - i >= 8 do
      sum +=
        ((ab(i  )&0xFFL)    ) | ((ab(i+1)&0xFFL)<< 8) | ((ab(i+2)&0xFFL)<<16) | ((ab(i+3)&0xFFL)<<24) |
        ((ab(i+4)&0xFFL)<<32) | ((ab(i+5)&0xFFL)<<40) | ((ab(i+6)&0xFFL)<<48) | ((ab(i+7)&0xFFL)<<56)
      i += 8
    while i < j do
      partial = partial | ((ab(i) & 0xFFL) << (8 * partialN))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = math.max(0, i0)
    val iM = math.min(s.length, iN)
    if (partialN % 2) != 0 then
      while i < iM do
        appendChar(s charAt i)
        i += 1
      return this
    while partialN > 0 && i < iM do
      partial |= s.charAt(i).toLong << (8 * partialN)
      partialN += 2
      if partialN >= 8 then
        sum += partial
        partial = 0
        partialN = 0
      i += 1
    while iM - i >= 4 do
      sum += s.charAt(i) | (s.charAt(i+1).toLong << 16) | (s.charAt(i+2).toLong << 32) | (s.charAt(i+3).toLong << 48)
      i += 4
    while i < iM do
      partial |= s.charAt(i).toLong << (8 * partialN)
      partialN += 2
      i += 1
    this

  def appendLong(l: Long): this.type =
    if partialN == 0 then
      sum += l
    else
      val sh = 8 * partialN
      sum += partial | (l << sh)
      partial = l >>> (64 - sh)
    this

  def appendInt(i: Int): this.type =
    val sh = 8 * partialN
    partial = partial | ((i & 0xFFFFFFFFL) << sh)
    partialN += 4
    if partialN >= 8 then
      sum += partial
      partialN -= 8
      partial =
        if partialN > 0 then (i & 0xFFFFFFFFL) >>> (8 * (4 - partialN))
        else 0
    this

  def appendChar(c: Char): this.type =
    partial = partial | (c.toLong << (8 * partialN))
    partialN += 2
    if partialN >= 8 then
      sum += partial
      partialN -= 8
      partial =
        if partialN > 0 then c.toLong >>> 8
        else 0
    this

  def appendByte(b: Byte): this.type =
    partial = partial | ((b & 0xFFL) << (8 * partialN))
    if partialN >= 7 then
      sum += partial
      partial = 0
      partialN = 0
    else
      partialN += 1
    this
  
  def result(bb: ByteBuffer): Long =
    append(bb)
    result()

  def result(ab: Array[Byte], i0: Int, iN: Int): Long =
    append(ab, i0, iN)
    result()

  def result(s: String, i0: Int, iN: Int): Long =
    append(s, i0, iN)
    result()
  
  def result(): Long = sum + partial
}


object SumHash extends FullHash32 with FullHash64 {
  private[this] val cached32 = new AtomicReference[SumHash32]()

  def hash32(seed: Int, bb: ByteBuffer): Int =
    val c = cached32.getAndSet(null)
    val h = if c eq null then new SumHash32() else c
    val result = h.hash32(seed, bb)
    cached32.set(h)
    result

  def hash32(seed: Int, ab: Array[Byte], i0: Int, iN: Int): Int =
    val c = cached32.getAndSet(null)
    val h = if c eq null then new SumHash32() else c
    val result = h.hash32(seed, ab, i0, iN)
    cached32.set(h)
    result

  def hash32(seed: Int, s: String, i0: Int, iN: Int): Int =
    val c = cached32.getAndSet(null)
    val h = if c eq null then new SumHash32() else c
    val result = h.hash32(seed, s, i0, iN)
    cached32.set(h)
    result

  private[this] val cached64 = new AtomicReference[SumHash64]()

  def hash64(seed: Long, bb: ByteBuffer): Long =
    val c = cached64.getAndSet(null)
    val h = if c eq null then new SumHash64() else c
    val result = h.hash64(seed, bb)
    cached64.set(h)
    result

  def hash64(seed: Long, ab: Array[Byte], i0: Int, iN: Int): Long =
    val c = cached64.getAndSet(null)
    val h = if c eq null then new SumHash64() else c
    val result = h.hash64(seed, ab, i0, iN)
    cached64.set(h)
    result

  def hash64(seed: Long, s: String, i0: Int, iN: Int): Long =
    val c = cached64.getAndSet(null)
    val h = if c eq null then new SumHash64() else c
    val result = h.hash64(seed, s, i0, iN)
    cached64.set(h)
    result
}



final class XorHash32() extends Hash32 {
  private[this] var xor = 0
  private[this] var partial = 0
  private[this] var partialN = 0

  private def mimicState(x: Int, p: Int, pN: Int): Unit =
    xor = x
    partial = p
    partialN = pN

  def copy: XorHash32 =
    val ans = new XorHash32
    ans.mimicState(xor, partial, partialN)
    ans

  def begin(seed: Int): this.type =
    xor = seed
    partial = 0
    partialN = 0
    this

  def append(bb: ByteBuffer): this.type =
    bb order ByteOrder.LITTLE_ENDIAN
    if partialN > 0 then
      while partialN < 4 && bb.hasRemaining do
        partial |= (bb.get & 0xFF) << (partialN*8)
        partialN += 1
      if partialN == 4 then
        xor = xor ^ partial
        partialN = 0
        partial = 0
    while bb.remaining >= 4 do xor = xor ^ bb.getInt
    while bb.hasRemaining do
      partial |= (bb.get & 0xFF) << (partialN*8)
      partialN += 1
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = if iN <= ab.length then iN else ab.length
    while i < j && partialN > 0 do
      partial = partial | ((ab(i) & 0xFF) << (8 * partialN))
      partialN += 1
      if partialN >= 4 then
        xor ^= partial
        partial = 0
        partialN = 0
      i += 1
    while j - i >= 4 do
      xor ^= (ab(i  )&0xFF) | ((ab(i+1)&0xFF)<<8) | ((ab(i+2)&0xFF)<<16) | ((ab(i+3)&0xFF)<<24)
      i += 4
    while i < j do
      partial = partial | ((ab(i) & 0xFF) << (8 * partialN))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = math.max(0, i0)
    val iM = math.min(s.length, iN)
    if (partialN % 2) != 0 then
      while i < iM do
        appendChar(s charAt i)
        i += 1
      return this
    if partialN > 0 && i < iM then
      xor ^= partial | (s.charAt(i) << 16)
      partial = 0
      partialN = 0
      i += 1
    while iM - i >= 2 do
      xor ^= s.charAt(i) | (s.charAt(i+1) << 16)
      i += 2
    if i < iM then
      partial = s charAt i
      partialN = 2
    this

  def appendLong(l: Long): this.type =
    if partialN == 0 then
      xor ^= (l & 0xFFFFFFFFL).toInt
      xor ^= (l >>> 32).toInt
    else
      val sh = 8 * partialN
      xor ^= partial | ((l & (0xFFFFFFFFL >>> sh)).toInt << sh)
      xor ^= ((l >>> (32 - sh)) & 0xFFFFFFFFL).toInt
      partial = (l >>> (64 - sh)).toInt
    this

  def appendInt(i: Int): this.type =
    if partialN == 0 then
      xor ^= i
    else
      val sh = 8 * partialN
      xor ^= partial | (i << sh)
      partial = i >>> (32 - sh)
    this

  def appendChar(c: Char): this.type =
    if partialN < 2 then
      partial = partial | (c.toInt << (8 * partialN))
      partialN += 2
    else if partialN == 2 then
      xor ^= partial | (c.toInt << 16)
      partial = 0
      partialN = 0
    else
      xor ^= partial | ((c & 0xFF) << 24)
      partial = (c >>> 8)
      partialN = 1
    this

  def appendByte(b: Byte): this.type =
    if partialN >= 3 then
      xor ^= partial | ((b & 0xFF) << 24)
      partial = 0
      partialN = 0
    else
      partial = partial | ((b & 0xFF) << (8 * partialN))
      partialN += 1
    this

  def result(bb: ByteBuffer): Int =
    append(bb)
    result()

  def result(ab: Array[Byte], i0: Int, iN: Int): Int =
    append(ab, i0, iN)
    result()

  def result(s: String, i0: Int, iN: Int): Int =
    append(s, i0, iN)
    result()
  
  def result(): Int =
    xor ^ partial
}


final class XorHash64() extends Hash64 {
  private[this] var xor = 0L
  private[this] var partial = 0L
  private[this] var partialN = 0

  private def mimicState(x: Long, p: Long, pN: Int): Unit =
    xor = x
    partial = p
    partialN = pN

  def copy: XorHash64 =
    val ans = new XorHash64
    ans.mimicState(xor, partial, partialN)
    ans

  def begin(seed: Long): this.type = { xor = seed; partial = 0; partialN = 0; this }

  def append(bb: ByteBuffer): this.type =
    bb order ByteOrder.LITTLE_ENDIAN
    if partialN > 0 then
      while partialN < 8 && bb.hasRemaining do
        partial |= (bb.get & 0xFFL) << (partialN*8)
        partialN += 1
      if partialN == 8 then
        xor = xor ^ partial
        partialN = 0
        partial = 0
    while bb.remaining >= 8 do xor = xor ^ bb.getLong
    while bb.hasRemaining do
      partial |= (bb.get & 0xFFL) << (partialN*8)
      partialN += 1
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = if iN <= ab.length then iN else ab.length
    while i < j && partialN > 0 do
      partial = partial | ((ab(i) & 0xFFL) << (8 * partialN))
      partialN += 1
      if partialN >= 8 then
        xor ^= partial
        partial = 0
        partialN = 0
      i += 1
    while j - i >= 8 do
      xor ^=
        ((ab(i  )&0xFFL)    ) | ((ab(i+1)&0xFFL)<< 8) | ((ab(i+2)&0xFFL)<<16) | ((ab(i+3)&0xFFL)<<24) |
        ((ab(i+4)&0xFFL)<<32) | ((ab(i+5)&0xFFL)<<40) | ((ab(i+6)&0xFFL)<<48) | ((ab(i+7)&0xFFL)<<56)
      i += 8
    while i < j do
      partial = partial | ((ab(i) & 0xFFL) << (8 * partialN))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = math.max(0, i0)
    val iM = math.min(s.length, iN)
    if (partialN % 2) != 0 then
      while i < iM do
        appendChar(s charAt i)
        i += 1
      return this
    while partialN > 0 && i < iM do
      partial |= s.charAt(i).toLong << (8 * partialN)
      partialN += 2
      if partialN >= 8 then
        xor ^= partial
        partial = 0
        partialN = 0
      i += 1
    while iM - i >= 4 do
      xor ^= s.charAt(i) | (s.charAt(i+1).toLong << 16) | (s.charAt(i+2).toLong << 32) | (s.charAt(i+3).toLong << 48)
      i += 4
    while i < iM do
      partial |= s.charAt(i).toLong << (8 * partialN)
      partialN += 2
      i += 1
    this

  def appendLong(l: Long): this.type =
    if partialN == 0 then
      xor ^= l
    else
      val sh = 8 * partialN
      xor ^= partial | (l << sh)
      partial = l >>> (64 - sh)
    this

  def appendInt(i: Int): this.type =
   val sh = 8 * partialN
    partial = partial | ((i & 0xFFFFFFFFL) << sh)
    partialN += 4
    if partialN >= 8 then
      xor ^= partial
      partialN -= 8
      partial =
        if partialN > 0 then (i & 0xFFFFFFFFL) >>> (64 - sh)
        else 0
    this

  def appendChar(c: Char): this.type =
    partial = partial | (c.toLong << (8 * partialN))
    partialN += 2
    if partialN >= 8 then
      xor ^= partial
      partialN -= 8
      partial =
        if partialN > 0 then c.toLong >>> 8
        else 0
    this

  def appendByte(b: Byte): this.type =
    partial = partial | ((b & 0xFFL) << (8 * partialN))
    if partialN >= 7 then
      xor ^= partial
      partial = 0
      partialN = 0
    else
      partialN += 1
    this
  
  def result(bb: ByteBuffer): Long =
    append(bb)
    result()

  def result(ab: Array[Byte], i0: Int, iN: Int): Long =
    append(ab, i0, iN)
    result()

  def result(s: String, i0: Int, iN: Int): Long =
    append(s, i0, iN)
    result()
  
  def result(): Long =
    xor ^ partial
}


object XorHash extends FullHash32 with FullHash64 {
  private[this] val cached32 = new AtomicReference[XorHash32]()

  def hash32(seed: Int, bb: ByteBuffer): Int =
    val c = cached32.getAndSet(null)
    val h = if c eq null then new XorHash32() else c
    val result = h.hash32(seed, bb)
    cached32.set(h)
    result

  def hash32(seed: Int, ab: Array[Byte], i0: Int, iN: Int): Int =
    val c = cached32.getAndSet(null)
    val h = if c eq null then new XorHash32() else c
    val result = h.hash32(seed, ab, i0, iN)
    cached32.set(h)
    result

  def hash32(seed: Int, s: String, i0: Int, iN: Int): Int =
    val c = cached32.getAndSet(null)
    val h = if c eq null then new XorHash32() else c
    val result = h.hash32(seed, s, i0, iN)
    cached32.set(h)
    result

  private[this] val cached64 = new AtomicReference[XorHash64]()

  def hash64(seed: Long, bb: ByteBuffer): Long =
    val c = cached64.getAndSet(null)
    val h = if c eq null then new XorHash64() else c
    val result = h.hash64(seed, bb)
    cached64.set(h)
    result

  def hash64(seed: Long, ab: Array[Byte], i0: Int, iN: Int): Long =
    val c = cached64.getAndSet(null)
    val h = if c eq null then new XorHash64() else c
    val result = h.hash64(seed, ab, i0, iN)
    cached64.set(h)
    result

  def hash64(seed: Long, s: String, i0: Int, iN: Int): Long =
    val c = cached64.getAndSet(null)
    val h = if c eq null then new XorHash64() else c
    val result = h.hash64(seed, s, i0, iN)
    cached64.set(h)
    result
}



object MakeHasher {
  def x32 = new XxHash32()
  def x64 = new XxHash64()
  def m32 = new MurmurHash32()
  def m128 = new MurmurHash128()
  def s32 = new SumHash32()
  def s64 = new SumHash64()
  def o32 = new XorHash32()
  def o64 = new XorHash64()
}


final class PairHash[A, B, Z, Y](h1: IncrementalHash[A, Z], h2: IncrementalHash[B, Y])
extends IncrementalHash[(A, B), (Z, Y)] {
  def copy: PairHash[A, B, Z, Y] =
    new PairHash(h1.copy, h2.copy)

  def result(bb: ByteBuffer): (Z, Y) =
    (h1 result bb.asReadOnlyBuffer, h2 result bb)

  def result(ab: Array[Byte], i0: Int, iN: Int): (Z, Y) = (h1.result(ab, i0, iN), h2.result(ab, i0, iN))

  def result(s: String, i0: Int, iN: Int): (Z, Y) = (h1.result(s, i0, iN), h2.result(s, i0, iN))

  def result(): (Z, Y) = (h1.result(), h2.result())

  def begin(seed: (A, B)): this.type =
    h1.begin(seed._1)
    h2.begin(seed._2)
    this

  def begin(): this.type =
    h1.begin()
    h2.begin()
    this

  def append(bb: ByteBuffer): this.type =
    h1 append bb.asReadOnlyBuffer
    h2 append bb
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    h1.append(ab, i0, iN)
    h2.append(ab, i0, iN)
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    h1.append(s, i0, iN)
    h2.append(s, i0, iN)
    this

  def appendLong(l: Long): this.type =
    h1 appendLong l
    h2 appendLong l
    this

  def appendInt(i: Int): this.type =
    h1 appendInt i
    h2 appendInt i
    this

  def appendChar(c: Char): this.type =
    h1 appendChar c
    h2 appendChar c
    this

  def appendByte(b: Byte): this.type =
    h1 appendByte b
    h2 appendByte b
    this
}
object PairHash {
  def of[A, B, Z, Y](h1: IncrementalHash[A, Z], h2: IncrementalHash[B, Y]): PairHash[A, B, Z, Y] =
    new PairHash(h1.begin(), h2.begin())
}


final class TrioHash[A, B, C, Z, Y, X](h1: IncrementalHash[A, Z], h2: IncrementalHash[B, Y], h3: IncrementalHash[C, X])
extends IncrementalHash[(A, B, C), (Z, Y, X)] {
  def copy: TrioHash[A, B, C, Z, Y, X] =
    new TrioHash(h1.copy, h2.copy, h3.copy)

  def result(bb: ByteBuffer): (Z, Y, X) =
    (h1 result bb.asReadOnlyBuffer, h2 result bb.asReadOnlyBuffer, h3 result bb)

  def result(ab: Array[Byte], i0: Int, iN: Int): (Z, Y, X) =
    (h1.result(ab, i0, iN), h2.result(ab, i0, iN), h3.result(ab, i0, iN))

  def result(s: String, i0: Int, iN: Int): (Z, Y, X) =
    (h1.result(s, i0, iN), h2.result(s, i0, iN), h3.result(s, i0, iN))

  def result(): (Z, Y, X) = (h1.result(), h2.result(), h3.result())

  def begin(seed: (A, B, C)): this.type =
    h1.begin(seed._1)
    h2.begin(seed._2)
    h3.begin(seed._3)
    this

  def begin(): this.type =
    h1.begin()
    h2.begin()
    h3.begin()
    this

  def append(bb: ByteBuffer): this.type =
    h1 append bb.asReadOnlyBuffer
    h2 append bb.asReadOnlyBuffer
    h3 append bb
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    h1.append(ab, i0, iN)
    h2.append(ab, i0, iN)
    h3.append(ab, i0, iN)
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    h1.append(s, i0, iN)
    h2.append(s, i0, iN)
    h3.append(s, i0, iN)
    this

  def appendLong(l: Long): this.type =
    h1 appendLong l
    h2 appendLong l
    h3 appendLong l
    this

  def appendInt(i: Int): this.type =
    h1 appendInt i
    h2 appendInt i
    h3 appendInt i
    this

  def appendChar(c: Char): this.type =
    h1 appendChar c
    h2 appendChar c
    h3 appendChar c
    this

  def appendByte(b: Byte): this.type =
    h1 appendByte b
    h2 appendByte b
    h3 appendByte b
    this
}
object TrioHash {
  def of[A, B, C, Z, Y, X](h1: IncrementalHash[A, Z], h2: IncrementalHash[B, Y], h3: IncrementalHash[C, X]): TrioHash[A, B, C, Z, Y, X] =
    new TrioHash(h1.begin(), h2.begin(), h3.begin())
}



final class QuadHash[A, B, C, D, Z, Y, X, W](
  h1: IncrementalHash[A, Z],
  h2: IncrementalHash[B, Y],
  h3: IncrementalHash[C, X],
  h4: IncrementalHash[D, W]
)
extends IncrementalHash[(A, B, C, D), (Z, Y, X, W)] {
  def copy: QuadHash[A, B, C, D, Z, Y, X, W] =
    new QuadHash(h1.copy, h2.copy, h3.copy, h4.copy)

  def result(bb: ByteBuffer): (Z, Y, X, W) =
    (h1 result bb.asReadOnlyBuffer, h2 result bb.asReadOnlyBuffer, h3 result bb.asReadOnlyBuffer, h4 result bb)

  def result(ab: Array[Byte], i0: Int, iN: Int): (Z, Y, X, W) =
    (h1.result(ab, i0, iN), h2.result(ab, i0, iN), h3.result(ab, i0, iN), h4.result(ab, i0, iN))

  def result(s: String, i0: Int, iN: Int): (Z, Y, X, W) =
    (h1.result(s, i0, iN), h2.result(s, i0, iN), h3.result(s, i0, iN), h4.result(s, i0, iN))

  def result(): (Z, Y, X, W) = (h1.result(), h2.result(), h3.result(), h4.result())

  def begin(seed: (A, B, C, D)): this.type =
    h1.begin(seed._1)
    h2.begin(seed._2)
    h3.begin(seed._3)
    h4.begin(seed._4)
    this

  def begin(): this.type =
    h1.begin()
    h2.begin()
    h3.begin()
    h4.begin()
    this

  def append(bb: ByteBuffer): this.type =
    h1 append bb.asReadOnlyBuffer
    h2 append bb.asReadOnlyBuffer
    h3 append bb.asReadOnlyBuffer
    h4 append bb
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    h1.append(ab, i0, iN)
    h2.append(ab, i0, iN)
    h3.append(ab, i0, iN)
    h4.append(ab, i0, iN)
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    h1.append(s, i0, iN)
    h2.append(s, i0, iN)
    h3.append(s, i0, iN)
    h4.append(s, i0, iN)
    this

  def appendLong(l: Long): this.type =
    h1 appendLong l
    h2 appendLong l
    h3 appendLong l
    h4 appendLong l
    this

  def appendInt(i: Int): this.type =
    h1 appendInt i
    h2 appendInt i
    h3 appendInt i
    h4 appendInt i
    this

  def appendChar(c: Char): this.type =
    h1 appendChar c
    h2 appendChar c
    h3 appendChar c
    h4 appendChar c
    this

  def appendByte(b: Byte): this.type =
    h1 appendByte b
    h2 appendByte b
    h3 appendByte b
    h4 appendByte b
    this
}
object QuadHash {
  def of[A, B, C, D, Z, Y, X, W](
    h1: IncrementalHash[A, Z],
    h2: IncrementalHash[B, Y],
    h3: IncrementalHash[C, X],
    h4: IncrementalHash[D, W]
  ): QuadHash[A, B, C, D, Z, Y, X, W] =
    new QuadHash(h1.begin(), h2.begin(), h3.begin(), h4.begin())
}


final class PreseededHash[A, Z](seed: A, h: IncrementalHash[A, Z]) extends IncrementalHash[Unit, Z] {
  def copy: PreseededHash[A, Z] =
    new PreseededHash[A, Z](seed, h.copy)

  def result(bb: ByteBuffer): Z = h.result(bb)

  def result(ab: Array[Byte], i0: Int, iN: Int): Z = h.result(ab, i0, iN)

  def result(s: String, i0: Int, iN: Int): Z = h.result(s, i0, iN)

  def result(): Z = h.result()

  def begin(seed: Unit): this.type =
    h.begin(this.seed)
    this

  def begin(): this.type =
    h.begin(this.seed)
    this

  def append(bb: ByteBuffer): this.type =
    h append bb
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    h.append(ab, i0, iN)
    this  

  def append(s: String, i0: Int, iN: Int): this.type =
    h.append(s, i0, iN)
    this

  def appendLong(l: Long): this.type =
    h appendLong l
    this 

  def appendInt(i: Int): this.type =
    h appendInt i
    this 

  def appendChar(c: Char): this.type =
    h appendChar c
    this 

  def appendByte(b: Byte): this.type =
    h appendByte b
    this 
}
object PreseededHash {
  def of[A, Z](seed: A, h: IncrementalHash[A, Z]): PreseededHash[A, Z] =
    new PreseededHash(seed, h begin seed)
}
