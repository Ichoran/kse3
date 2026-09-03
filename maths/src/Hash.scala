// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015-16, 2021-25 by Rex Kerr and Calico Life Sciences LLC
//
// Contains code ported from xxHash C source (by Yann Collet, "Cyan5973")
//   See https://github.com/Cyan4973/xxHash
// Contains code ported from MurmurHash C++ source (by Austin Appleby)
//   See https://github.com/aappleby/smhasher

package kse.maths

import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.lang.{Math => jm}
import java.lang.Integer.{rotateLeft => rotl32, rotateRight => rotr32 }
import java.lang.Long.{rotateLeft => rotl64, rotateRight => rotr64 }
import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.atomic.AtomicReference
import java.util.zip.{CRC32 => ZipCRC32, CRC32C => ZipCRC32C}

import scala.collection.immutable.{Range => Rg}

import kse.basics._
import kse.basics.intervals._


/** Accumulates a hash of whatever is appended; no seeding or finalization is promised, so this
  * suits running checksums as well as true hashers.  Multi-byte primitives append little-endian.
  * `appendRaw` is the one bulk entry point for memory: the inline `append` re-views any `Mem` as
  * bytes, and implementations re-view those bytes at whatever width they chew on.
  */
trait SimpleIncrementalHash {
  def begin(): this.type
  def append(bb: ByteBuffer): this.type
  def append(ab: Array[Byte], i0: Int, iN: Int): this.type
  def append(s: String, i0: Int, iN: Int): this.type
  def appendRaw(m: Mem[Byte]): this.type
  def appendByte(b: Byte): this.type
  def appendChar(c: Char): this.type
  def appendInt(i: Int): this.type
  def appendLong(l: Long): this.type

  inline final def append(ab: Array[Byte]): this.type = append(ab, 0, ab.length)
  inline final def append[R <: Iv.X | Rg](ab: Array[Byte], inline r: R): this.type = Iv.dispatch(r, ab)((i0, iN) => append(ab, i0, iN))
  inline final def append(s: String): this.type = append(s, 0, s.length)
  inline final def append[R <: Iv.X | Rg](s: String, inline r: R): this.type = Iv.dispatch(r, s)((i0, iN) => append(s, i0, iN))
  inline final def append[A <: Mem.Type](m: Mem[A]): this.type = appendRaw(m.as[Byte])

  inline final def +=(z: Boolean):      Unit = appendByte(if z then 1 else 0)
  inline final def +=(b: Byte):         Unit = appendByte(b)
  inline final def +=(s: Short):        Unit = appendChar(s.toChar)
  inline final def +=(c: Char):         Unit = appendChar(c)
  inline final def +=(i: Int):          Unit = appendInt(i)
  inline final def +=(l: Long):         Unit = appendLong(l)
  inline final def +=(f: Float):        Unit = appendInt(java.lang.Float.floatToRawIntBits(f))
  inline final def +=(d: Double):       Unit = appendLong(java.lang.Double.doubleToRawLongBits(d))
  inline final def +=(ab: Array[Byte]): Unit = append(ab, 0, ab.length)
  inline final def +=(s: String):       Unit = append(s, 0, s.length)
  inline final def +=[A <: Mem.Type](m: Mem[A]): Unit = appendRaw(m.as[Byte])
}
object SimpleIncrementalHash {
  final class AlreadyFinalizedException(msg: String) extends Exception(msg) {}
  protected[maths] def fzerr(msg: String = ""): Nothing = throw new AlreadyFinalizedException(msg)
}

/** An incremental hash that can deliver its answer: the bulk `result` methods append their
  * argument and finalize, and the `freshHash` methods begin, append, and finalize in one call.
  * This is the full contract of an unseeded hasher; seeding is [[IncrementalHash]]'s job.
  */
trait HashInto[Z] extends SimpleIncrementalHash {
  def result(bb: ByteBuffer): Z
  def result(ab: Array[Byte], i0: Int, iN: Int): Z
  def result(s: String, i0: Int, iN: Int): Z
  def result(): Z

  inline final def result(ab: Array[Byte]): Z = result(ab, 0, ab.length)
  inline final def result[R <: Iv.X | Rg](ab: Array[Byte], inline r: R): Z = Iv.dispatch(r, ab)((i0, iN) => result(ab, i0, iN))
  inline final def result(s: String): Z = result(s, 0, s.length)
  inline final def result[R <: Iv.X | Rg](s: String, inline r: R): Z = Iv.dispatch(r, s)((i0, iN) => result(s, i0, iN))
  inline final def result[A <: Mem.Type](m: Mem[A]): Z = appendRaw(m.as[Byte]).result()

  final def freshHash(bb: ByteBuffer): Z = begin().result(bb)
  final def freshHash(ab: Array[Byte], i0: Int, iN: Int): Z = begin().result(ab, i0, iN)
  final def freshHash(ab: Array[Byte]): Z = begin().result(ab, 0, ab.length)
  inline final def freshHash[R <: Iv.X | Rg](ab: Array[Byte], inline r: R): Z = Iv.dispatch(r, ab)((i0, iN) => begin().result(ab, i0, iN))
  final def freshHash(s: String, i0: Int, iN: Int): Z = begin().result(s, i0, iN)
  final def freshHash(s: String): Z = begin().result(s, 0, s.length)
  inline final def freshHash[R <: Iv.X | Rg](s: String, inline r: R): Z = Iv.dispatch(r, s)((i0, iN) => begin().result(s, i0, iN))
  inline final def freshHash[A <: Mem.Type](m: Mem[A]): Z = begin().appendRaw(m.as[Byte]).result()

  def copy: HashInto[Z]
}

trait SeededIncrementalHash[A] extends SimpleIncrementalHash {
  def begin(seed: A): this.type
}

trait IncrementalHash[A, Z] extends HashInto[Z] with SeededIncrementalHash[A] {
  final def freshHash(seed: A, bb: ByteBuffer): Z = begin(seed).result(bb)
  final def freshHash(seed: A, ab: Array[Byte], i0: Int, iN: Int): Z = begin(seed).result(ab, i0, iN)
  final def freshHash(seed: A, ab: Array[Byte]): Z = begin(seed).result(ab, 0, ab.length)
  inline final def freshHash[R <: Iv.X | Rg](seed: A, ab: Array[Byte], inline r: R): Z = Iv.dispatch(r, ab)((i0, iN) => begin(seed).result(ab, i0, iN))
  final def freshHash(seed: A, s: String, i0: Int, iN: Int): Z = begin(seed).result(s, i0, iN)
  final def freshHash(seed: A, s: String): Z = begin(seed).result(s, 0, s.length)
  inline final def freshHash[R <: Iv.X | Rg](seed: A, s: String, inline r: R): Z = Iv.dispatch(r, s)((i0, iN) => begin(seed).result(s, i0, iN))
  inline final def freshHash[M <: Mem.Type](seed: A, m: Mem[M]): Z = begin(seed).appendRaw(m.as[Byte]).result()
  def copy: IncrementalHash[A, Z]
}


/** One-shot 32-bit hashing of a whole byte range, unseeded; [[FullHash32]] adds the seeds. */
trait SimpleFullHash32 {
  def hash32(ab: Array[Byte], i0: Int, iN: Int): Int
  inline final def hash32(ab: Array[Byte]): Int = hash32(ab, 0, ab.length)
  inline final def hash32[R <: Iv.X | Rg](ab: Array[Byte], inline r: R): Int = Iv.dispatch(r, ab)((i0, iN) => hash32(ab, i0, iN))

  def hash32(bb: ByteBuffer): Int

  def hash32(s: String, i0: Int, iN: Int): Int
  inline final def hash32(s: String): Int = hash32(s, 0, s.length)
  inline final def hash32[R <: Iv.X | Rg](s: String, inline r: R): Int = Iv.dispatch(r, s)((i0, iN) => hash32(s, i0, iN))
}

trait FullHash32 extends SimpleFullHash32 {
  def hash32(seed: Int, ab: Array[Byte], i0: Int, iN: Int): Int
  inline final def hash32(seed: Int, ab: Array[Byte]): Int = hash32(seed, ab, 0, ab.length)
  inline final def hash32[R <: Iv.X | Rg](seed: Int, ab: Array[Byte], inline r: R): Int = Iv.dispatch(r, ab)((i0, iN) => hash32(seed, ab, i0, iN))
  def hash32(ab: Array[Byte], i0: Int, iN: Int): Int = hash32(0, ab, i0, iN)

  def hash32(seed: Int, bb: ByteBuffer): Int
  def hash32(bb: ByteBuffer): Int = hash32(0, bb)

  def hash32(seed: Int, s: String, i0: Int, iN: Int): Int
  inline final def hash32(seed: Int, s: String): Int = hash32(seed, s, 0, s.length)
  inline final def hash32[R <: Iv.X | Rg](seed: Int, s: String, inline r: R): Int = Iv.dispatch(r, s)((i0, iN) => hash32(seed, s, i0, iN))
  def hash32(s: String, i0: Int, iN: Int): Int = hash32(0, s, i0, iN)
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


/** One-shot 64-bit hashing of a whole byte range, unseeded; [[FullHash64]] adds the seeds. */
trait SimpleFullHash64 {
  def hash64(ab: Array[Byte], i0: Int, iN: Int): Long
  inline final def hash64(ab: Array[Byte]): Long = hash64(ab, 0, ab.length)
  inline final def hash64[R <: Iv.X | Rg](ab: Array[Byte], inline r: R): Long = Iv.dispatch(r, ab)((i0, iN) => hash64(ab, i0, iN))

  def hash64(bb: ByteBuffer): Long

  def hash64(s: String, i0: Int, iN: Int): Long
  inline final def hash64(s: String): Long = hash64(s, 0, s.length)
  inline final def hash64[R <: Iv.X | Rg](s: String, inline r: R): Long = Iv.dispatch(r, s)((i0, iN) => hash64(s, i0, iN))
}

trait FullHash64 extends SimpleFullHash64 {
  def hash64(seed: Long, ab: Array[Byte], i0: Int, iN: Int): Long
  inline final def hash64(seed: Long, ab: Array[Byte]): Long = hash64(seed, ab, 0, ab.length)
  inline final def hash64[R <: Iv.X | Rg](seed: Long, ab: Array[Byte], inline r: R): Long = Iv.dispatch(r, ab)((i0, iN) => hash64(seed, ab, i0, iN))
  def hash64(ab: Array[Byte], i0: Int, iN: Int): Long = hash64(0L, ab, i0, iN)

  def hash64(seed: Long, bb: ByteBuffer): Long
  def hash64(bb: ByteBuffer): Long = hash64(0L, bb)

  def hash64(seed: Long, s: String, i0: Int, iN: Int): Long
  inline final def hash64(seed: Long, s: String): Long = hash64(seed, s, 0, s.length)
  inline final def hash64[R <: Iv.X | Rg](seed: Long, s: String, inline r: R): Long = Iv.dispatch(r, s)((i0, iN) => hash64(seed, s, i0, iN))
  def hash64(s: String, i0: Int, iN: Int): Long = hash64(0L, s, i0, iN)
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

/** One-shot 128-bit hashing of a whole byte range, unseeded; [[FullHash128]] adds the seeds. */
trait SimpleFullHash128 {
  def hash128(ab: Array[Byte], i0: Int, iN: Int): HashCode128
  inline final def hash128(ab: Array[Byte]): HashCode128 = hash128(ab, 0, ab.length)
  inline final def hash128[R <: Iv.X | Rg](ab: Array[Byte], inline r: R): HashCode128 = Iv.dispatch(r, ab)((i0, iN) => hash128(ab, i0, iN))

  def hash128(bb: ByteBuffer): HashCode128

  def hash128(s: String, i0: Int, iN: Int): HashCode128
  inline final def hash128(s: String): HashCode128 = hash128(s, 0, s.length)
  inline final def hash128[R <: Iv.X | Rg](s: String, inline r: R): HashCode128 = Iv.dispatch(r, s)((i0, iN) => hash128(s, i0, iN))
}

trait FullHash128 extends SimpleFullHash128 {
  def hash128(seed0: Long, seed1: Long, ab: Array[Byte], i0: Int, iN: Int): HashCode128
  inline final def hash128(seed0: Long, seed1: Long, ab: Array[Byte]): HashCode128 = hash128(seed0, seed1, ab, 0, ab.length)
  inline final def hash128[R <: Iv.X | Rg](seed0: Long, seed1: Long, ab: Array[Byte], inline r: R): HashCode128 = Iv.dispatch(r, ab)((i0, iN) => hash128(seed0, seed1, ab, i0, iN))
  def hash128(ab: Array[Byte], i0: Int, iN: Int): HashCode128 = hash128(0L, 0L, ab, i0, iN)

  def hash128(seed0: Long, seed1: Long, bb: ByteBuffer): HashCode128
  def hash128(bb: ByteBuffer): HashCode128 = hash128(0L, 0L, bb)

  def hash128(seed0: Long, seed1: Long, s: String, i0: Int, iN: Int): HashCode128
  inline final def hash128(seed0: Long, seed1: Long, s: String): HashCode128 = hash128(seed0, seed1, s, 0, s.length)
  inline final def hash128[R <: Iv.X | Rg](seed0: Long, seed1: Long, s: String, inline r: R): HashCode128 = Iv.dispatch(r, s)((i0, iN) => hash128(seed0, seed1, s, i0, iN))
  def hash128(s: String, i0: Int, iN: Int): HashCode128 = hash128(0L, 0L, s, i0, iN)
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
  private var v1: Int = Prime32_1 + Prime32_2
  private var v2: Int = Prime32_2
  private var v3: Int = 0
  private var v4: Int = -Prime32_1
  private var v5: Int = 0
  private var hadBlock: Boolean = false
  private var finalized: Boolean = false
  private var myBuffer: ByteBuffer = null    // Do NOT mark--can't copy cleanly in that case

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
      myBuffer order ByteOrder.LITTLE_ENDIAN __ Unit
      myBuffer limit bb.limit __ Unit
      myBuffer position bb.position __ Unit

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
    if myBuffer ne null then myBuffer.clear() __ Unit
    this

  private inline def createBufferIfNeeded(): Boolean =
    if myBuffer eq null then
      myBuffer = ByteBuffer allocate 16
      myBuffer order ByteOrder.LITTLE_ENDIAN
      true
    else
      false
  
  private def appendBy16(bb: ByteBuffer): this.type =
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
  
  private def appendIx4(one: Int, two: Int, three: Int, four: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XXhash32 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl32(v1 +   one * Prime32_2, 13) * Prime32_1
    v2 = rotl32(v2 +   two * Prime32_2, 13) * Prime32_1
    v3 = rotl32(v3 + three * Prime32_2, 13) * Prime32_1
    v4 = rotl32(v4 +  four * Prime32_2, 13) * Prime32_1
    v5 += 16
    hadBlock = true
    this

  private def appendMyBuffer(): Unit =
    myBuffer.flip()
    appendIx4(myBuffer.getInt, myBuffer.getInt, myBuffer.getInt, myBuffer.getInt)
    myBuffer.clear() __ Unit
  
  private def counting(extra: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XXhash32 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = if (!hadBlock) v3 + Prime32_5 else rotl32(v1, 1) + rotl32(v2, 7) + rotl32(v3, 12) + rotl32(v4, 18)
    v1 += v5 + extra
    this
  
  private def trailing(one: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XXhash32 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl32(v1 + one * Prime32_3, 17) * Prime32_4
    this
  
  private def trailing(quarter: Byte): this.type =
    if finalized then SimpleIncrementalHash.fzerr("XXhash32 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl32(v1 + (quarter&0xFF) * Prime32_5, 11) * Prime32_1
    this      
  
  private def complete(): Int =
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
      while myBuffer.position <= 12 && bb.remaining >= 4 do myBuffer.putInt(bb.getInt) __ Unit
      while myBuffer.position <  16 && bb.remaining >= 1 do myBuffer.put(bb.get) __ Unit
      if myBuffer.position == 16 then appendMyBuffer()
    if bb.remaining >= 16 then appendBy16(bb)
    if bb.remaining > 0 then
      if myBuffer eq null then
        myBuffer = ByteBuffer allocate 16
        myBuffer order ByteOrder.LITTLE_ENDIAN __ Unit
      while bb.remaining >= 4 do myBuffer.putInt(bb.getInt) __ Unit
      while bb.remaining >= 1 do myBuffer.put(bb.get) __ Unit
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
      createBufferIfNeeded() __ Unit
      while i < j do
        myBuffer put ab(i)
        i += 1
    this

  def appendRaw(m: Mem[Byte]): this.type =
    var i = 0L
    val j = m.length
    if (myBuffer ne null) && myBuffer.position > 0 then
      while i < j && myBuffer.remaining > 0 do
        myBuffer put m(i)
        i += 1
      if myBuffer.remaining == 0 then appendMyBuffer()
    while i <= j-16 do
      appendIx4(m.getI_le(i), m.getI_le(i+4), m.getI_le(i+8), m.getI_le(i+12))
      i += 16
    if i < j then
      createBufferIfNeeded() __ Unit
      while i < j do
        myBuffer put m(i)
        i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = jm.min(iN, s.length)
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
      createBufferIfNeeded() __ Unit
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

  private def myAppendInt(i: Int): Unit = 
    if myBuffer.remaining >= 4 then
      myBuffer putInt i __ Unit
      if myBuffer.remaining == 0 then
        appendMyBuffer()
    else if myBuffer.remaining >= 2 then
      myBuffer putChar i.toChar __ Unit
      if myBuffer.remaining == 0 then
        appendMyBuffer()
        myBuffer putChar (i >>> 16).toChar __ Unit
      else
        myAppendChar((i >>> 16).toChar)
    else if myBuffer.remaining == 1 then
      myBuffer put (i & 0xFF).toByte
      appendMyBuffer()
      myBuffer put ((i >>> 8) & 0xFF).toByte __ Unit
      myBuffer putChar (i >>> 16).toChar __ Unit
    else
      appendMyBuffer()
      myBuffer putInt i __ Unit

  def appendInt(i: Int): this.type =
    if createBufferIfNeeded() then
      myBuffer putInt i __ Unit
    else
      myAppendInt(i)
    this

  private def myAppendChar(c: Char): Unit =
    if myBuffer.remaining >= 2 then
      myBuffer putChar c __ Unit
      if myBuffer.remaining == 0 then
        appendMyBuffer()
    else if myBuffer.remaining == 1 then
      myBuffer put (c & 0xFF).toByte __ Unit
      appendMyBuffer()
      myBuffer put (c >>> 8).toByte __ Unit
    else
      appendMyBuffer()
      myBuffer putChar c __ Unit

  def appendChar(c: Char): this.type =
    if createBufferIfNeeded() then
      myBuffer putChar c __ Unit
    else
      myAppendChar(c)
    this

  def appendByte(b: Byte): this.type =
    if createBufferIfNeeded() || myBuffer.remaining > 1 then
      myBuffer put b __ Unit
    else if myBuffer.remaining == 1 then
      myBuffer put b __ Unit
      appendMyBuffer()
    else
      appendMyBuffer()
      myBuffer put b __ Unit
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
    if terminal eq myBuffer then myBuffer.clear() __ Unit
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
      myBuffer.clear() __ Unit
    else if !finalized then
      counting(0)
    complete()
}


final class XxHash64() extends Hash64 {
  import XxHash.{Prime64_1, Prime64_2, Prime64_3, Prime64_4, Prime64_5}
  private var v1: Long = Prime64_1 + Prime64_2
  private var v2: Long = Prime64_2
  private var v3: Long = 0
  private var v4: Long = -Prime64_1
  private var v5: Long = 0
  private var hadBlock: Boolean = false
  private var finalized: Boolean = false
  private var myBuffer: ByteBuffer = null    // Do NOT mark--can't copy cleanly in that case

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
      myBuffer order ByteOrder.LITTLE_ENDIAN __ Unit
      myBuffer limit bb.limit __ Unit
      myBuffer position bb.position __ Unit

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
    if myBuffer ne null then myBuffer.clear() __ Unit
    this

  private def createBufferIfNeeded(): Boolean =
    if myBuffer eq null then
      myBuffer = ByteBuffer allocate 32
      myBuffer order ByteOrder.LITTLE_ENDIAN __ Unit
      true
    else
      false
  
  private def appendBy32(bb: ByteBuffer): Unit =
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
  
  private def appendLx4(one: Long, two: Long, three: Long, four: Long): Unit =
    if finalized then SimpleIncrementalHash.fzerr("XxHash64 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl64(v1 +   one * Prime64_2, 31) * Prime64_1
    v2 = rotl64(v2 +   two * Prime64_2, 31) * Prime64_1
    v3 = rotl64(v3 + three * Prime64_2, 31) * Prime64_1
    v4 = rotl64(v4 +  four * Prime64_2, 31) * Prime64_1
    v5 += 32
    hadBlock = true

  private inline def appendMyBuffer(): Unit =
    myBuffer.flip()
    appendLx4(myBuffer.getLong, myBuffer.getLong, myBuffer.getLong, myBuffer.getLong)
    myBuffer.clear() __ Unit
  
  private def counting(extra: Int): Unit =
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
  
  private def trailing(one: Long): Unit =
    if finalized then SimpleIncrementalHash.fzerr("XxHash64 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl64(v1 ^ (rotl64(one * Prime64_2, 31) * Prime64_1), 27)*Prime64_1 + Prime64_4
  
  private def trailing(one: Int): Unit =
    if finalized then SimpleIncrementalHash.fzerr("XxHash64 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl64(v1 ^ ((one & 0xFFFFFFFFL) * Prime64_1), 23) * Prime64_2 + Prime64_3
  
  private def trailing(quarter: Byte): Unit =
    if finalized then SimpleIncrementalHash.fzerr("XxHash64 hasher finalized (use begin() or begin(seed) to reuse)")
    v1 = rotl64(v1 ^ ((quarter & 0xFF) * Prime64_5), 11) * Prime64_1
  
  private def complete(): Long =
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
      while myBuffer.position <= 24 && bb.remaining >= 8 do myBuffer.putLong(bb.getLong) __ Unit
      while myBuffer.position < 32 && bb.remaining >= 1 do myBuffer.put(bb.get) __ Unit
      if myBuffer.position == 32 then
        myBuffer.flip()
        appendLx4(myBuffer.getLong, myBuffer.getLong, myBuffer.getLong, myBuffer.getLong)
        myBuffer.clear() __ Unit
    if bb.remaining >= 32 then appendBy32(bb)
    if bb.remaining > 0 then
      if myBuffer eq null then
        myBuffer = ByteBuffer allocate 32
        myBuffer order ByteOrder.LITTLE_ENDIAN __ Unit
      while bb.remaining >= 8 do myBuffer.putLong(bb.getLong) __ Unit
      while bb.remaining >= 1 do myBuffer.put(bb.get) __ Unit
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
      createBufferIfNeeded() __ Unit
      while i < j do
        myBuffer put ab(i) __ Unit
        i += 1
    this

  def appendRaw(m: Mem[Byte]): this.type =
    var i = 0L
    val j = m.length
    if (myBuffer ne null) && myBuffer.position > 0 then
      while i < j && myBuffer.remaining > 0 do
        myBuffer put m(i) __ Unit
        i += 1
      if myBuffer.remaining == 0 then appendMyBuffer()
    while i <= j-32 do
      appendLx4(m.getL_le(i), m.getL_le(i+8), m.getL_le(i+16), m.getL_le(i+24))
      i += 32
    if i < j then
      createBufferIfNeeded() __ Unit
      while i < j do
        myBuffer put m(i) __ Unit
        i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = jm.min(iN, s.length)
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
      createBufferIfNeeded() __ Unit
      while i < j do
        myBuffer putChar s.charAt(i) __ Unit
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

  private def myAppendInt(i: Int): Unit = 
    if myBuffer.remaining >= 4 then
      myBuffer putInt i __ Unit
      if myBuffer.remaining == 0 then
        appendMyBuffer()
    else if myBuffer.remaining >= 2 then
      myBuffer putChar i.toChar __ Unit
      if myBuffer.remaining == 0 then
        appendMyBuffer()
        myBuffer putChar (i >>> 16).toChar __ Unit
      else
        myAppendChar((i >>> 16).toChar)
    else if myBuffer.remaining == 1 then
      myBuffer put (i & 0xFF).toByte __ Unit
      appendMyBuffer()
      myBuffer put ((i >>> 8) & 0xFF).toByte __ Unit
      myBuffer putChar (i >>> 16).toChar __ Unit
    else
      appendMyBuffer()
      myBuffer putInt i __ Unit

  def appendInt(i: Int): this.type =
    if createBufferIfNeeded() then
      myBuffer putInt i __ Unit
    else
      myAppendInt(i)
    this

  private def myAppendChar(c: Char): Unit =
    if myBuffer.remaining >= 2 then
      myBuffer putChar c __ Unit
      if myBuffer.remaining == 0 then
        appendMyBuffer()
    else if myBuffer.remaining == 1 then
      myBuffer put (c & 0xFF).toByte __ Unit
      appendMyBuffer()
      myBuffer put (c >>> 8).toByte __ Unit
    else
      appendMyBuffer()
      myBuffer putChar c __ Unit

  def appendChar(c: Char): this.type =
    if createBufferIfNeeded() then
      myBuffer putChar c __ Unit
    else
      myAppendChar(c)
    this

  def appendByte(b: Byte): this.type =
    if createBufferIfNeeded() || myBuffer.remaining > 1 then
      myBuffer put b __ Unit
    else if myBuffer.remaining == 1 then
      myBuffer put b __ Unit
      appendMyBuffer()
    else
      appendMyBuffer()
      myBuffer put b __ Unit
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
    if terminal eq myBuffer then myBuffer.clear() __ Unit
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
      myBuffer.clear() __ Unit
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
    val iM = jm.min(a.length, iN)
    var i = jm.max(0, i0)
    val len = jm.max(iM - i, 0)
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
    val iM = jm.min(s.length, iN)
    var i = jm.max(0, i0)
    val len = jm.max(iM - i, 0)
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
    val iM = jm.min(ab.length, iN)
    var i = jm.max(0, i0)
    val len = jm.max(iM - i, 0)
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
    val iM = jm.min(s.length, iN)
    var i = jm.max(0, i0)
    val len = jm.max(iM - i, 0)
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
  private var state = 0
  private var n = 0
  private var partial = 0
  private var partialN = 0
  private var finalized = false

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

  private def appendI(i: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("MurmurHash32 hasher finalized (use begin() or begin(seed) to reuse)")
    n += 4
    val x = state ^ (0x1B873593 * rotl32(i * 0xCC9E2D51, 15))
    state = (5 * rotl32(x, 13)) + 0xE6546B64
    this

  private def appendLastI(i: Int, bytes: Int): this.type =
    if finalized then SimpleIncrementalHash.fzerr("MurmurHash32 hasher finalized (use begin() or begin(seed) to reuse)")
    n += (bytes&3)
    state = state ^ (0x1B873593 * rotl32(i * 0xCC9E2D51, 15))
    this

  private def finalizer(): Unit =
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

  def appendRaw(m: Mem[Byte]): this.type =
    var i = 0L
    val j = m.length
    while i < j && partialN > 0 do
      partial = partial | ((m(i) & 0xFF) << (8 * partialN))
      partialN += 1
      if partialN >= 4 then
        appendI(partial)
        partial = 0
        partialN = 0
      i += 1
    while j - i >= 4 do
      appendI(m.getI_le(i))
      i += 4
    while i < j do
      partial = partial | ((m(i) & 0xFF) << (8 * partialN))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = jm.max(0, i0)
    val iM = jm.min(s.length, iN)
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
  private var state0, state1 = 0L
  private var partial0, partial1 = 0L
  private var partialN = 0
  private var n = 0
  private var finalized = false

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

  private def appendLx2(la: Long, lb: Long): Unit =
    if finalized then SimpleIncrementalHash.fzerr("MurmurHash128 hasher finalized (use begin() or begin(seed0, seed1) to reuse)")
    n += 16
    val x0 = state0 ^ (0x4CF5AD432745937FL * rotl64(la * 0x87C37B91114253D5L, 31))
    state0 = ((rotl64(x0, 27) + state1) * 5) + 0x52DCE729
    val x1 = state1 ^ (0x87C37B91114253D5L * rotl64(lb * 0x4CF5AD432745937FL, 33))
    state1 = ((rotl64(x1, 31) + state0) * 5) + 0x38495AB5
  
  private def appendLastLx2(la: Long, lb: Long, bytes: Int): Unit =
    if finalized then SimpleIncrementalHash.fzerr("MurmurHash128 hasher finalized (use begin() or begin(seed0, seed1) to reuse)")
    val m = bytes & 0xF
    n += m
    if (m > 8) state1 = state1 ^ (0x87C37B91114253D5L * rotl64(lb * 0x4CF5AD432745937FL, 33))
    state0 = state0 ^ (0x4CF5AD432745937FL * rotl64(la * 0x87C37B91114253D5L, 31))
  
  private def mixer(l: Long): Long =
    val x = 0xFF51AFD7ED558CCDL * (l ^ (l >>> 33))
    val y = 0xC4CEB9FE1A85EC53L * (x ^ (x >>> 33))
    y ^ (y >>> 33)
  
  private def finalizer(): Unit =
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

  def appendRaw(m: Mem[Byte]): this.type =
    var i = 0L
    val j = m.length
    if partialN > 0 then
      while i < j && partialN < 8 do
        partial0 |= ((m(i) & 0xFFL) << (partialN*8))
        partialN += 1
        i += 1
      while i < j && partialN < 16 do
        partial1 |= ((m(i) & 0xFFL) << ((partialN - 8)*8))
        partialN += 1
        i += 1
      if partialN == 16 then
        appendLx2(partial0, partial1)
        partial0 = 0
        partial1 = 0
        partialN = 0
    while j - i >= 16 do
      appendLx2(m.getL_le(i), m.getL_le(i+8))
      i += 16
    while i < j && partialN < 8 do
      partial0 |= ((m(i) & 0xFFL) << (partialN*8))
      partialN += 1
      i += 1
    while i < j do
      partial1 |= ((m(i) & 0xFFL) << ((partialN - 8)*8))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = jm.max(0, i0)
    val iM = jm.min(s.length, iN)
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
  private val cached32 = new AtomicReference[MurmurHash32]()

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

  private val cached128 = new AtomicReference[MurmurHash128]()

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
  private var sum = 0
  private var partial = 0
  private var partialN = 0

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

  def appendRaw(m: Mem[Byte]): this.type =
    var i = 0L
    val j = m.length
    while i < j && partialN > 0 do
      partial = partial | ((m(i) & 0xFF) << (8 * partialN))
      partialN += 1
      if partialN >= 4 then
        sum += partial
        partial = 0
        partialN = 0
      i += 1
    while j - i >= 4 do
      sum += m.getI_le(i)
      i += 4
    while i < j do
      partial = partial | ((m(i) & 0xFF) << (8 * partialN))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = jm.max(0, i0)
    val iM = jm.min(s.length, iN)
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
  private var sum = 0L
  private var partial = 0L
  private var partialN = 0

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

  def appendRaw(m: Mem[Byte]): this.type =
    var i = 0L
    val j = m.length
    while i < j && partialN > 0 do
      partial = partial | ((m(i) & 0xFFL) << (8 * partialN))
      partialN += 1
      if partialN >= 8 then
        sum += partial
        partial = 0
        partialN = 0
      i += 1
    while j - i >= 8 do
      sum += m.getL_le(i)
      i += 8
    while i < j do
      partial = partial | ((m(i) & 0xFFL) << (8 * partialN))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = jm.max(0, i0)
    val iM = jm.min(s.length, iN)
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
  private val cached32 = new AtomicReference[SumHash32]()

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

  private val cached64 = new AtomicReference[SumHash64]()

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
  private var xor = 0
  private var partial = 0
  private var partialN = 0

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

  def appendRaw(m: Mem[Byte]): this.type =
    var i = 0L
    val j = m.length
    while i < j && partialN > 0 do
      partial = partial | ((m(i) & 0xFF) << (8 * partialN))
      partialN += 1
      if partialN >= 4 then
        xor ^= partial
        partial = 0
        partialN = 0
      i += 1
    while j - i >= 4 do
      xor ^= m.getI_le(i)
      i += 4
    while i < j do
      partial = partial | ((m(i) & 0xFF) << (8 * partialN))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = jm.max(0, i0)
    val iM = jm.min(s.length, iN)
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
  private var xor = 0L
  private var partial = 0L
  private var partialN = 0

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

  def appendRaw(m: Mem[Byte]): this.type =
    var i = 0L
    val j = m.length
    while i < j && partialN > 0 do
      partial = partial | ((m(i) & 0xFFL) << (8 * partialN))
      partialN += 1
      if partialN >= 8 then
        xor ^= partial
        partial = 0
        partialN = 0
      i += 1
    while j - i >= 8 do
      xor ^= m.getL_le(i)
      i += 8
    while i < j do
      partial = partial | ((m(i) & 0xFFL) << (8 * partialN))
      partialN += 1
      i += 1
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = jm.max(0, i0)
    val iM = jm.min(s.length, iN)
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
  private val cached32 = new AtomicReference[XorHash32]()

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

  private val cached64 = new AtomicReference[XorHash64]()

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



/** Replicates the state of a table-driven reflected CRC whose 32-bit register is (a bijection
  * of) its stored value, which is how both zip CRC classes behave but neither exposes:
  * `forcing(v)` is the four bytes that drive a freshly reset checksum to value `v`.  Each byte
  * step is `r' = (r >>> 8) ^ table(i)` with a free choice of table index via the input byte,
  * and four steps shift out all of the starting register, so the target determines the four
  * indices uniquely (walk backward matching high bytes--the table's high bytes are a
  * permutation) and any start can then reach it (walk forward choosing the bytes that select
  * those indices).
  */
private[maths] final class CrcForcer(poly: Int) {
  private val table: Array[Int] =
    val t = new Array[Int](256)
    var i = 0
    while i < 256 do
      var c = i
      var k = 8
      while k > 0 do
        c = if (c & 1) == 1 then poly ^ (c >>> 1) else c >>> 1
        k -= 1
      t(i) = c
      i += 1
    t

  private val unhigh: Array[Byte] =
    val a = new Array[Byte](256)
    var i = 0
    while i < 256 do
      a(table(i) >>> 24) = i.toByte
      i += 1
    a

  def forcing(v: Int): Array[Byte] =
    val idx = new Array[Int](4)
    var t = ~v
    var k = 3
    while k >= 0 do
      val i = unhigh(t >>> 24) & 0xFF
      idx(k) = i
      t = (t ^ table(i)) << 8
      k -= 1
    val ans = new Array[Byte](4)
    var r = 0xFFFFFFFF
    k = 0
    while k < 4 do
      ans(k) = ((r ^ idx(k)) & 0xFF).toByte
      r = (r >>> 8) ^ table(idx(k))
      k += 1
    ans
}


/** The CRC-32 checksum (the zip/zlib polynomial), computed by `java.util.zip.CRC32` so bulk
  * appends run on the JVM's hardware-accelerated path.  A CRC has no seed, so this is a
  * [[HashInto]] but not an [[IncrementalHash]]; and it has no finalization step, so unlike
  * the true hashers here, appending may continue after `result()` and `result()` may be read
  * repeatedly--it is a running checksum, reset by `begin()`.  Multi-byte primitives append
  * little-endian like every other hasher; the conventional unsigned value is `crcValue`, and
  * `result()` is the same 32 bits as an `Int`.
  */
final class Crc32() extends HashInto[Int] with SimpleFullHash32 {
  private val c = new ZipCRC32()
  private var tiny: Array[Byte] = null

  def begin(): this.type =
    c.reset()
    this

  def copy: Crc32 =
    val ans = new Crc32()
    val v = c.getValue.toInt
    if v != 0 then ans.c.update(Crc32.forcer.forcing(v))
    ans

  private def loadTiny(n: Int, l: Long): Array[Byte] =
    if tiny eq null then tiny = new Array[Byte](8)
    var i = 0
    var x = l
    while i < n do
      tiny(i) = (x & 0xFF).toByte
      x >>>= 8
      i += 1
    tiny

  def appendByte(b: Byte): this.type =
    c.update(b & 0xFF)
    this

  def appendChar(ch: Char): this.type =
    c.update(loadTiny(2, ch), 0, 2)
    this

  def appendInt(i: Int): this.type =
    c.update(loadTiny(4, i & 0xFFFFFFFFL), 0, 4)
    this

  def appendLong(l: Long): this.type =
    c.update(loadTiny(8, l), 0, 8)
    this

  def append(bb: ByteBuffer): this.type =
    c.update(bb)
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    val i = if i0 < 0 then 0 else i0
    val j = if iN > ab.length then ab.length else iN
    if i < j then c.update(ab, i, j - i)
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = jm.min(iN, s.length)
    if i < j then
      val buf = new Array[Byte](if j - i >= 1024 then 2048 else 2*(j - i))
      while i < j do
        var k = 0
        while k < buf.length && i < j do
          val ch = s.charAt(i)
          buf(k) = (ch & 0xFF).toByte
          buf(k+1) = (ch >>> 8).toByte
          k += 2
          i += 1
        c.update(buf, 0, k)
    this

  def appendRaw(m: Mem[Byte]): this.type =
    val n = m.length
    var i = 0L
    while i < n do
      val k = if n - i > 0x40000000L then 0x40000000L else n - i
      c.update(m.view(i, i + k).segment.asByteBuffer)
      i += k
    this

  def result(bb: ByteBuffer): Int = append(bb).result()
  def result(ab: Array[Byte], i0: Int, iN: Int): Int = append(ab, i0, iN).result()
  def result(s: String, i0: Int, iN: Int): Int = append(s, i0, iN).result()
  def result(): Int = c.getValue.toInt

  /** The checksum so far, in its conventional unsigned form. */
  def crcValue: Long = c.getValue

  def hash32(bb: ByteBuffer): Int = begin().result(bb)
  def hash32(ab: Array[Byte], i0: Int, iN: Int): Int = begin().result(ab, i0, iN)
  def hash32(s: String, i0: Int, iN: Int): Int = begin().result(s, i0, iN)
}


/** Computes CRC-32 checksums in one shot; see [[Crc32]] for conventions. */
object Crc32 extends SimpleFullHash32 {
  private[maths] val forcer = new CrcForcer(0xEDB88320)

  def hash32(bb: ByteBuffer): Int =
    val c = new ZipCRC32()
    c.update(bb)
    c.getValue.toInt

  def hash32(ab: Array[Byte], i0: Int, iN: Int): Int =
    val i = if i0 < 0 then 0 else i0
    val j = if iN > ab.length then ab.length else iN
    val c = new ZipCRC32()
    if i < j then c.update(ab, i, j - i)
    c.getValue.toInt

  def hash32(s: String, i0: Int, iN: Int): Int = (new Crc32()).append(s, i0, iN).result()

  inline def hash32[A <: Mem.Type](m: Mem[A]): Int = hash32Raw(m.as[Byte])

  def hash32Raw(m: Mem[Byte]): Int = (new Crc32()).appendRaw(m).result()
}


/** The CRC-32C checksum (the Castagnoli polynomial, as used by iSCSI, ext4, and friends),
  * computed by `java.util.zip.CRC32C` so bulk appends run on the JVM's hardware-accelerated
  * path.  The contract is [[Crc32]]'s in every other way: unseeded, never finalized (append
  * and read `result()` freely, reset with `begin()`), little-endian primitive appends, and
  * `crcValue` for the conventional unsigned form.
  */
final class Crc32C() extends HashInto[Int] with SimpleFullHash32 {
  private val c = new ZipCRC32C()
  private var tiny: Array[Byte] = null

  def begin(): this.type =
    c.reset()
    this

  def copy: Crc32C =
    val ans = new Crc32C()
    val v = c.getValue.toInt
    if v != 0 then ans.c.update(Crc32C.forcer.forcing(v))
    ans

  private def loadTiny(n: Int, l: Long): Array[Byte] =
    if tiny eq null then tiny = new Array[Byte](8)
    var i = 0
    var x = l
    while i < n do
      tiny(i) = (x & 0xFF).toByte
      x >>>= 8
      i += 1
    tiny

  def appendByte(b: Byte): this.type =
    c.update(b & 0xFF)
    this

  def appendChar(ch: Char): this.type =
    c.update(loadTiny(2, ch), 0, 2)
    this

  def appendInt(i: Int): this.type =
    c.update(loadTiny(4, i & 0xFFFFFFFFL), 0, 4)
    this

  def appendLong(l: Long): this.type =
    c.update(loadTiny(8, l), 0, 8)
    this

  def append(bb: ByteBuffer): this.type =
    c.update(bb)
    this

  def append(ab: Array[Byte], i0: Int, iN: Int): this.type =
    val i = if i0 < 0 then 0 else i0
    val j = if iN > ab.length then ab.length else iN
    if i < j then c.update(ab, i, j - i)
    this

  def append(s: String, i0: Int, iN: Int): this.type =
    var i = if i0 < 0 then 0 else i0
    val j = jm.min(iN, s.length)
    if i < j then
      val buf = new Array[Byte](if j - i >= 1024 then 2048 else 2*(j - i))
      while i < j do
        var k = 0
        while k < buf.length && i < j do
          val ch = s.charAt(i)
          buf(k) = (ch & 0xFF).toByte
          buf(k+1) = (ch >>> 8).toByte
          k += 2
          i += 1
        c.update(buf, 0, k)
    this

  def appendRaw(m: Mem[Byte]): this.type =
    val n = m.length
    var i = 0L
    while i < n do
      val k = if n - i > 0x40000000L then 0x40000000L else n - i
      c.update(m.view(i, i + k).segment.asByteBuffer)
      i += k
    this

  def result(bb: ByteBuffer): Int = append(bb).result()
  def result(ab: Array[Byte], i0: Int, iN: Int): Int = append(ab, i0, iN).result()
  def result(s: String, i0: Int, iN: Int): Int = append(s, i0, iN).result()
  def result(): Int = c.getValue.toInt

  /** The checksum so far, in its conventional unsigned form. */
  def crcValue: Long = c.getValue

  def hash32(bb: ByteBuffer): Int = begin().result(bb)
  def hash32(ab: Array[Byte], i0: Int, iN: Int): Int = begin().result(ab, i0, iN)
  def hash32(s: String, i0: Int, iN: Int): Int = begin().result(s, i0, iN)
}


/** Computes CRC-32C checksums in one shot; see [[Crc32C]] for conventions. */
object Crc32C extends SimpleFullHash32 {
  private[maths] val forcer = new CrcForcer(0x82F63B78)

  def hash32(bb: ByteBuffer): Int =
    val c = new ZipCRC32C()
    c.update(bb)
    c.getValue.toInt

  def hash32(ab: Array[Byte], i0: Int, iN: Int): Int =
    val i = if i0 < 0 then 0 else i0
    val j = if iN > ab.length then ab.length else iN
    val c = new ZipCRC32C()
    if i < j then c.update(ab, i, j - i)
    c.getValue.toInt

  def hash32(s: String, i0: Int, iN: Int): Int = (new Crc32C()).append(s, i0, iN).result()

  inline def hash32[A <: Mem.Type](m: Mem[A]): Int = hash32Raw(m.as[Byte])

  def hash32Raw(m: Mem[Byte]): Int = (new Crc32C()).appendRaw(m).result()
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
  def c32 = new Crc32()
  def c32c = new Crc32C()
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

  def appendRaw(m: Mem[Byte]): this.type =
    h1 appendRaw m
    h2 appendRaw m
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

  def appendRaw(m: Mem[Byte]): this.type =
    h1 appendRaw m
    h2 appendRaw m
    h3 appendRaw m
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

  def appendRaw(m: Mem[Byte]): this.type =
    h1 appendRaw m
    h2 appendRaw m
    h3 appendRaw m
    h4 appendRaw m
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

  def appendRaw(m: Mem[Byte]): this.type =
    h appendRaw m
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
