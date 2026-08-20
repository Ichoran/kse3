// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.basics


import java.lang.foreign.{Arena, MemorySegment, ValueLayout}
import java.lang.foreign.ValueLayout.*

import scala.annotation.targetName
import scala.collection.LongStepper
import scala.compiletime.{erasedValue, error, summonFrom}
import scala.language.dynamics
import scala.util.boundary


/** Array-like high-speed access to off-heap (or heap-array-backed) primitive memory.
  *
  * `A` is a phantom type chosen by the caller; the element layout is resolved at
  * compile time by an `inline match` on `A`.  Only primitives are supported, and
  * because everything is `inline` these methods are only usable where `A` resolves
  * to a concrete primitive.  Nothing here allocates a segment: the caller owns all
  * lifetimes, and any destination is supplied explicitly.
  */
opaque type Mem[A <: Mem.Type] = MemorySegment
object Mem {
  /** The primitive element types backable off-heap with a complete (round-trippable) byte layout. */
  type Type = Byte | Short | Char | Int | Long | Float | Double

  /** Wrap a caller-owned segment.  The caller retains responsibility for its lifetime. */
  inline def wrap[A <: Type](seg: MemorySegment): Mem[A] = seg

  /** Compile-time size in bytes of one element of type `A`. */
  inline def bytesOf[A <: Type]: Long = inline erasedValue[A] match
    case _: Byte    => 1L
    case _: Short   => 2L
    case _: Char    => 2L
    case _: Int     => 4L
    case _: Float   => 4L
    case _: Long    => 8L
    case _: Double  => 8L
    case _          => error("Mem only supports primitive element types")

  /** Compile-time (unaligned) value layout of one element of type `A`, e.g. for `MemorySegment.copy`. */
  inline def layoutOf[A <: Type]: ValueLayout = inline erasedValue[A] match
    case _: Byte    => JAVA_BYTE
    case _: Short   => JAVA_SHORT_UNALIGNED
    case _: Char    => JAVA_CHAR_UNALIGNED
    case _: Int     => JAVA_INT_UNALIGNED
    case _: Float   => JAVA_FLOAT_UNALIGNED
    case _: Long    => JAVA_LONG_UNALIGNED
    case _: Double  => JAVA_DOUBLE_UNALIGNED
    case _          => error("Mem only supports primitive element types")

  /** Allocate `n` elements of off-heap memory, reclaimed by the GC when unreachable. */
  inline def alloc[A <: Type](n: Long): Mem[A] =
    wrap[A](Arena.ofAuto().allocate(n * bytesOf[A]))

  /** Wrap a primitive array as a `Mem` of its element type (shares the array's storage). */
  infix inline def of(xs: Array[Byte]):   Mem[Byte]   = wrap(MemorySegment.ofArray(xs))
  infix inline def of(xs: Array[Short]):  Mem[Short]  = wrap(MemorySegment.ofArray(xs))
  infix inline def of(xs: Array[Char]):   Mem[Char]   = wrap(MemorySegment.ofArray(xs))
  infix inline def of(xs: Array[Int]):    Mem[Int]    = wrap(MemorySegment.ofArray(xs))
  infix inline def of(xs: Array[Long]):   Mem[Long]   = wrap(MemorySegment.ofArray(xs))
  infix inline def of(xs: Array[Float]):  Mem[Float]  = wrap(MemorySegment.ofArray(xs))
  infix inline def of(xs: Array[Double]): Mem[Double] = wrap(MemorySegment.ofArray(xs))

  /** Reinterpret a primitive array's bytes as a `Mem[A]` (any trailing partial element is ignored by `length`). */
  inline def as[A <: Type](xs: Array[Byte]):   Mem[A] = wrap[A](MemorySegment.ofArray(xs))
  inline def as[A <: Type](xs: Array[Short]):  Mem[A] = wrap[A](MemorySegment.ofArray(xs))
  inline def as[A <: Type](xs: Array[Char]):   Mem[A] = wrap[A](MemorySegment.ofArray(xs))
  inline def as[A <: Type](xs: Array[Int]):    Mem[A] = wrap[A](MemorySegment.ofArray(xs))
  inline def as[A <: Type](xs: Array[Long]):   Mem[A] = wrap[A](MemorySegment.ofArray(xs))
  inline def as[A <: Type](xs: Array[Float]):  Mem[A] = wrap[A](MemorySegment.ofArray(xs))
  inline def as[A <: Type](xs: Array[Double]): Mem[A] = wrap[A](MemorySegment.ofArray(xs))

  /** A `Mem` bundled with the `Arena` that owns its lifetime: one closeable unit, safe to share across
    * threads when the backing arena is shared.  Prefer `op`/`use` for scoped access; `close` releases it.
    * Construct via [[Owned.create]] so the memory is the single segment obtained from the owning arena.
    */
  final class Owned[A <: Type] private (val arena: Arena, val memory: Mem[A]) extends AutoCloseable {
    /** Run `f` on the owned memory and return its result. */
    inline def op[B](inline f: Mem[A] => B): B = f(memory)
    /** Run a side-effecting `f` on the owned memory. */
    inline def use(inline f: Mem[A] => Unit): Unit = f(memory)
    /** Release the backing arena, freeing (or unmapping) the memory. */
    def close(): Unit = arena.close()
  }
  object Owned {
    /** Take ownership of `arena` and the single segment `f` derives from it (e.g. `_.allocate(..)` or a
      * memory-mapped file).  The segment's lifetime must be the arena's; obtaining it is the caller's
      * explicit FFM call, so convenience constructors live atop this (e.g. in `eio`).
      */
    def create[A <: Type](arena: Arena)(f: Arena => MemorySegment): Owned[A] =
      new Owned[A](arena, wrap[A](f(arena)))
  }

  // === Byte order as a value ===

  /** True on big-endian hosts.  Both operands are JVM constants, so the JIT folds this--and
    * any branch on it--away entirely.
    */
  inline def bigEndianHost: Boolean = java.nio.ByteOrder.nativeOrder() eq java.nio.ByteOrder.BIG_ENDIAN

  /** A byte order, reduced to whether it is byte-swapped relative to this machine: ordered
    * access always reads and writes through the JDK's constant native-order layouts (the only
    * ones the JIT compiles down to raw loads and stores) and conditionally applies the
    * `reverseBytes` intrinsic--a swapped-but-fixed layout object would instead land on the
    * uninlined `VarHandle` path, roughly 20x slower.  Pick an order with `import Mem.LE` (or
    * `Mem.BE`, or `Mem.Native`), or bind one chosen at runtime with e.g.
    * `given Mem.Order = if p then Mem.bigEndian else Mem.littleEndian`.  There is
    * deliberately no default: all three givens sit in this type's implicit scope, so
    * order-aware code that never states its order fails to compile as ambiguous--plain `Mem`
    * access is already the native-order spelling.
    */
  final class Order private[basics] (val swapped: Boolean, name: String) {
    override def toString = name
  }

  /** Little-endian byte order (see [[Mem.Order]]). */
  val littleEndian: Order = new Order(bigEndianHost, "LE")

  /** Big-endian (network) byte order (see [[Mem.Order]]). */
  val bigEndian: Order = new Order(!bigEndianHost, "BE")

  /** Whichever of [[Mem.littleEndian]]/[[Mem.bigEndian]] this machine natively is. */
  val nativeEndian: Order = if littleEndian.swapped then bigEndian else littleEndian

  /** `import Mem.LE` puts little-endian order in scope. */
  given LE: Order = littleEndian
  /** `import Mem.BE` puts big-endian (network) order in scope. */
  given BE: Order = bigEndian
  /** `import Mem.Native` puts this machine's native order in scope, explicitly. */
  given Native: Order = nativeEndian

  // === Value scans: width-specialized workers backing `whereIsFwd`/`whereIsBkw` ===
  // Byte through Int lanes ride a 64-bit SWAR: read a long, xor with the value broadcast to
  // every lane, and zero-test the lanes.  Forward scans use the cheap test
  // (x - lows) & ~x & highs, which is exact at the lowest matching lane (its borrow can corrupt
  // only lanes above a true match, beyond the answer taken); backward scans need the highest
  // lane, so they use the exact test ~(((x & maxs) + maxs) | x | maxs), whose per-lane carries
  // cannot escape their lane.  Little-endian hardware is ASSUMED, here as elsewhere in kse3
  // (lane order and the borrow-direction argument both depend on it); big-endian machines are
  // not supported.  Floating types are searched as their raw bits via the Int/Long workers
  // (see `whereIsFwd`).

  private val leLong: ValueLayout.OfLong = JAVA_LONG_UNALIGNED.withOrder(java.nio.ByteOrder.LITTLE_ENDIAN)

  /** The first index in `[i0, iN)` (clamped) of `seg` holding byte `v`, or -1; backs `whereIsFwd`. */
  def seekIsByte(seg: MemorySegment, i0: Long, iN: Long, v: Byte): Long =
    var k = if i0 < 0 then 0L else i0
    val n = { val z = seg.byteSize; if iN > z then z else iN }
    val pat = 0x0101010101010101L * (v & 0xFFL)
    var ans = -1L
    while ans < 0 && k + 8 <= n do
      val x = (seg.get(leLong, k): Long) ^ pat
      val z = (x - 0x0101010101010101L) & ~x & 0x8080808080808080L
      if z == 0L then k += 8
      else ans = k + (java.lang.Long.numberOfTrailingZeros(z) >>> 3)
    if ans < 0 then
      while k < n && seg.get(JAVA_BYTE, k) != v do k += 1
      if k < n then ans = k
    ans

  /** The first index in `[i0, iN)` (clamped) of `seg` as shorts holding `v`, or -1; backs `whereIsFwd`. */
  def seekIsShort(seg: MemorySegment, i0: Long, iN: Long, v: Short): Long =
    var k = if i0 < 0 then 0L else i0
    val n = { val z = seg.byteSize >> 1; if iN > z then z else iN }
    val pat = 0x0001000100010001L * (v & 0xFFFFL)
    var ans = -1L
    while ans < 0 && k + 4 <= n do
      val x = (seg.get(leLong, k << 1): Long) ^ pat
      val z = (x - 0x0001000100010001L) & ~x & 0x8000800080008000L
      if z == 0L then k += 4
      else ans = k + (java.lang.Long.numberOfTrailingZeros(z) >>> 4)
    if ans < 0 then
      while k < n && seg.get(JAVA_SHORT_UNALIGNED, k << 1) != v do k += 1
      if k < n then ans = k
    ans

  /** The first index in `[i0, iN)` (clamped) of `seg` as ints holding `v`, or -1; backs `whereIsFwd`. */
  def seekIsInt(seg: MemorySegment, i0: Long, iN: Long, v: Int): Long =
    var k = if i0 < 0 then 0L else i0
    val n = { val z = seg.byteSize >> 2; if iN > z then z else iN }
    val pat = 0x0000000100000001L * (v & 0xFFFFFFFFL)
    var ans = -1L
    while ans < 0 && k + 2 <= n do
      val x = (seg.get(leLong, k << 2): Long) ^ pat
      val z = (x - 0x0000000100000001L) & ~x & 0x8000000080000000L
      if z == 0L then k += 2
      else ans = k + (java.lang.Long.numberOfTrailingZeros(z) >>> 5)
    if ans < 0 then
      while k < n && seg.get(JAVA_INT_UNALIGNED, k << 2) != v do k += 1
      if k < n then ans = k
    ans

  /** The first index in `[i0, iN)` (clamped) of `seg` as longs holding `v`, or -1; backs `whereIsFwd`. */
  def seekIsLong(seg: MemorySegment, i0: Long, iN: Long, v: Long): Long =
    var k = if i0 < 0 then 0L else i0
    val n = { val z = seg.byteSize >> 3; if iN > z then z else iN }
    while k < n && seg.get(JAVA_LONG_UNALIGNED, k << 3) != v do k += 1
    if k < n then k else -1L

  /** The last index in `[i0, iN)` (clamped) of `seg`'s bytes holding `v`, or -1; backs `whereIsBkw`. */
  def seekIsByteBkw(seg: MemorySegment, i0: Long, iN: Long, v: Byte): Long =
    val k0 = if i0 < 0 then 0L else i0
    var n = { val z = seg.byteSize; if iN > z then z else iN }
    val pat = 0x0101010101010101L * (v & 0xFFL)
    var ans = -1L
    while ans < 0 && n - 8 >= k0 do
      val x = (seg.get(leLong, n - 8): Long) ^ pat
      val z = ~(((x & 0x7F7F7F7F7F7F7F7FL) + 0x7F7F7F7F7F7F7F7FL) | x | 0x7F7F7F7F7F7F7F7FL)
      if z == 0L then n -= 8
      else ans = (n - 8) + ((63 - java.lang.Long.numberOfLeadingZeros(z)) >>> 3)
    if ans < 0 then
      var k = n - 1
      while k >= k0 && seg.get(JAVA_BYTE, k) != v do k -= 1
      if k >= k0 then ans = k
    ans

  /** The last index in `[i0, iN)` (clamped) of `seg` as shorts holding `v`, or -1; backs `whereIsBkw`. */
  def seekIsShortBkw(seg: MemorySegment, i0: Long, iN: Long, v: Short): Long =
    val k0 = if i0 < 0 then 0L else i0
    var n = { val z = seg.byteSize >> 1; if iN > z then z else iN }
    val pat = 0x0001000100010001L * (v & 0xFFFFL)
    var ans = -1L
    while ans < 0 && n - 4 >= k0 do
      val x = (seg.get(leLong, (n - 4) << 1): Long) ^ pat
      val z = ~(((x & 0x7FFF7FFF7FFF7FFFL) + 0x7FFF7FFF7FFF7FFFL) | x | 0x7FFF7FFF7FFF7FFFL)
      if z == 0L then n -= 4
      else ans = (n - 4) + ((63 - java.lang.Long.numberOfLeadingZeros(z)) >>> 4)
    if ans < 0 then
      var k = n - 1
      while k >= k0 && seg.get(JAVA_SHORT_UNALIGNED, k << 1) != v do k -= 1
      if k >= k0 then ans = k
    ans

  /** The last index in `[i0, iN)` (clamped) of `seg` as ints holding `v`, or -1; backs `whereIsBkw`. */
  def seekIsIntBkw(seg: MemorySegment, i0: Long, iN: Long, v: Int): Long =
    val k0 = if i0 < 0 then 0L else i0
    var n = { val z = seg.byteSize >> 2; if iN > z then z else iN }
    val pat = 0x0000000100000001L * (v & 0xFFFFFFFFL)
    var ans = -1L
    while ans < 0 && n - 2 >= k0 do
      val x = (seg.get(leLong, (n - 2) << 2): Long) ^ pat
      val z = ~(((x & 0x7FFFFFFF7FFFFFFFL) + 0x7FFFFFFF7FFFFFFFL) | x | 0x7FFFFFFF7FFFFFFFL)
      if z == 0L then n -= 2
      else ans = (n - 2) + ((63 - java.lang.Long.numberOfLeadingZeros(z)) >>> 5)
    if ans < 0 then
      var k = n - 1
      while k >= k0 && seg.get(JAVA_INT_UNALIGNED, k << 2) != v do k -= 1
      if k >= k0 then ans = k
    ans

  /** The last index in `[i0, iN)` (clamped) of `seg` as longs holding `v`, or -1; backs `whereIsBkw`. */
  def seekIsLongBkw(seg: MemorySegment, i0: Long, iN: Long, v: Long): Long =
    val k0 = if i0 < 0 then 0L else i0
    var k = { val z = seg.byteSize >> 3; if iN > z then z else iN } - 1
    while k >= k0 && seg.get(JAVA_LONG_UNALIGNED, k << 3) != v do k -= 1
    if k >= k0 then k else -1L

  extension [A <: Type](m: Mem[A]) {
    /** The underlying segment. */
    inline def segment: MemorySegment = m

    /** Number of elements = floor(byteSize / elementBytes). */
    inline def length: Long = (m: MemorySegment).byteSize / bytesOf[A]

    inline def apply(i: Long): A = inline erasedValue[A] match
      case _: Byte    => (m: MemorySegment).get(JAVA_BYTE, i).asInstanceOf[A]
      case _: Short   => (m: MemorySegment).getAtIndex(JAVA_SHORT_UNALIGNED, i).asInstanceOf[A]
      case _: Char    => (m: MemorySegment).getAtIndex(JAVA_CHAR_UNALIGNED, i).asInstanceOf[A]
      case _: Int     => (m: MemorySegment).getAtIndex(JAVA_INT_UNALIGNED, i).asInstanceOf[A]
      case _: Float   => (m: MemorySegment).getAtIndex(JAVA_FLOAT_UNALIGNED, i).asInstanceOf[A]
      case _: Long    => (m: MemorySegment).getAtIndex(JAVA_LONG_UNALIGNED, i).asInstanceOf[A]
      case _: Double  => (m: MemorySegment).getAtIndex(JAVA_DOUBLE_UNALIGNED, i).asInstanceOf[A]
      case _          => error("Mem only supports primitive element types")

    inline def update(i: Long, x: A): Unit = inline erasedValue[A] match
      case _: Byte    => (m: MemorySegment).set(JAVA_BYTE, i, x.asInstanceOf[Byte])
      case _: Short   => (m: MemorySegment).setAtIndex(JAVA_SHORT_UNALIGNED, i, x.asInstanceOf[Short])
      case _: Char    => (m: MemorySegment).setAtIndex(JAVA_CHAR_UNALIGNED, i, x.asInstanceOf[Char])
      case _: Int     => (m: MemorySegment).setAtIndex(JAVA_INT_UNALIGNED, i, x.asInstanceOf[Int])
      case _: Float   => (m: MemorySegment).setAtIndex(JAVA_FLOAT_UNALIGNED, i, x.asInstanceOf[Float])
      case _: Long    => (m: MemorySegment).setAtIndex(JAVA_LONG_UNALIGNED, i, x.asInstanceOf[Long])
      case _: Double  => (m: MemorySegment).setAtIndex(JAVA_DOUBLE_UNALIGNED, i, x.asInstanceOf[Double])
      case _          => error("Mem only supports primitive element types")

    /** Read a primitive of the stated type at element index `i`: the byte offset is `i * bytesOf[A]`,
      * so indices stay in units of `A` no matter which type is read (unaligned, native byte order).
      */
    inline def getB(i: Long): Byte   = (m: MemorySegment).get(JAVA_BYTE,             i * bytesOf[A])
    inline def getS(i: Long): Short  = (m: MemorySegment).get(JAVA_SHORT_UNALIGNED,  i * bytesOf[A])
    inline def getC(i: Long): Char   = (m: MemorySegment).get(JAVA_CHAR_UNALIGNED,   i * bytesOf[A])
    inline def getI(i: Long): Int    = (m: MemorySegment).get(JAVA_INT_UNALIGNED,    i * bytesOf[A])
    inline def getF(i: Long): Float  = (m: MemorySegment).get(JAVA_FLOAT_UNALIGNED,  i * bytesOf[A])
    inline def getL(i: Long): Long   = (m: MemorySegment).get(JAVA_LONG_UNALIGNED,   i * bytesOf[A])
    inline def getD(i: Long): Double = (m: MemorySegment).get(JAVA_DOUBLE_UNALIGNED, i * bytesOf[A])

    /** Write a primitive of the stated type at element index `i`: the byte offset is `i * bytesOf[A]`,
      * so indices stay in units of `A` no matter which type is written (unaligned, native byte order).
      */
    inline def setB(i: Long, x: Byte):   Unit = (m: MemorySegment).set(JAVA_BYTE,             i * bytesOf[A], x)
    inline def setS(i: Long, x: Short):  Unit = (m: MemorySegment).set(JAVA_SHORT_UNALIGNED,  i * bytesOf[A], x)
    inline def setC(i: Long, x: Char):   Unit = (m: MemorySegment).set(JAVA_CHAR_UNALIGNED,   i * bytesOf[A], x)
    inline def setI(i: Long, x: Int):    Unit = (m: MemorySegment).set(JAVA_INT_UNALIGNED,    i * bytesOf[A], x)
    inline def setF(i: Long, x: Float):  Unit = (m: MemorySegment).set(JAVA_FLOAT_UNALIGNED,  i * bytesOf[A], x)
    inline def setL(i: Long, x: Long):   Unit = (m: MemorySegment).set(JAVA_LONG_UNALIGNED,   i * bytesOf[A], x)
    inline def setD(i: Long, x: Double): Unit = (m: MemorySegment).set(JAVA_DOUBLE_UNALIGNED, i * bytesOf[A], x)

    /** Read a primitive of the stated type at element index `i` (byte offset `i * bytesOf[A]`)
      * in the stated byte order: `_le` little-endian, `_be` big-endian, `_xe` in whatever
      * order the `Mem.Order` given in scope says.  Bytes have no order, so `getB` covers all.
      * Access is always through the constant native-order layouts (the JIT's fast path) with
      * a conditional `reverseBytes`; for `_le`/`_be` the condition is itself a JIT constant,
      * so the matching direction compiles to a bare load.
      */
    inline def getS_le(i: Long): Short =
      val x = (m: MemorySegment).get(JAVA_SHORT_UNALIGNED, i * bytesOf[A])
      if bigEndianHost then java.lang.Short.reverseBytes(x) else x
    inline def getC_le(i: Long): Char =
      val x = (m: MemorySegment).get(JAVA_CHAR_UNALIGNED, i * bytesOf[A])
      if bigEndianHost then java.lang.Character.reverseBytes(x) else x
    inline def getI_le(i: Long): Int =
      val x = (m: MemorySegment).get(JAVA_INT_UNALIGNED, i * bytesOf[A])
      if bigEndianHost then java.lang.Integer.reverseBytes(x) else x
    inline def getL_le(i: Long): Long =
      val x = (m: MemorySegment).get(JAVA_LONG_UNALIGNED, i * bytesOf[A])
      if bigEndianHost then java.lang.Long.reverseBytes(x) else x
    inline def getF_le(i: Long): Float =
      if bigEndianHost then java.lang.Float.intBitsToFloat(java.lang.Integer.reverseBytes((m: MemorySegment).get(JAVA_INT_UNALIGNED, i * bytesOf[A])))
      else (m: MemorySegment).get(JAVA_FLOAT_UNALIGNED, i * bytesOf[A])
    inline def getD_le(i: Long): Double =
      if bigEndianHost then java.lang.Double.longBitsToDouble(java.lang.Long.reverseBytes((m: MemorySegment).get(JAVA_LONG_UNALIGNED, i * bytesOf[A])))
      else (m: MemorySegment).get(JAVA_DOUBLE_UNALIGNED, i * bytesOf[A])
    inline def getS_be(i: Long): Short =
      val x = (m: MemorySegment).get(JAVA_SHORT_UNALIGNED, i * bytesOf[A])
      if bigEndianHost then x else java.lang.Short.reverseBytes(x)
    inline def getC_be(i: Long): Char =
      val x = (m: MemorySegment).get(JAVA_CHAR_UNALIGNED, i * bytesOf[A])
      if bigEndianHost then x else java.lang.Character.reverseBytes(x)
    inline def getI_be(i: Long): Int =
      val x = (m: MemorySegment).get(JAVA_INT_UNALIGNED, i * bytesOf[A])
      if bigEndianHost then x else java.lang.Integer.reverseBytes(x)
    inline def getL_be(i: Long): Long =
      val x = (m: MemorySegment).get(JAVA_LONG_UNALIGNED, i * bytesOf[A])
      if bigEndianHost then x else java.lang.Long.reverseBytes(x)
    inline def getF_be(i: Long): Float =
      if bigEndianHost then (m: MemorySegment).get(JAVA_FLOAT_UNALIGNED, i * bytesOf[A])
      else java.lang.Float.intBitsToFloat(java.lang.Integer.reverseBytes((m: MemorySegment).get(JAVA_INT_UNALIGNED, i * bytesOf[A])))
    inline def getD_be(i: Long): Double =
      if bigEndianHost then (m: MemorySegment).get(JAVA_DOUBLE_UNALIGNED, i * bytesOf[A])
      else java.lang.Double.longBitsToDouble(java.lang.Long.reverseBytes((m: MemorySegment).get(JAVA_LONG_UNALIGNED, i * bytesOf[A])))
    inline def getS_xe(i: Long)(using o: Order): Short =
      val x = (m: MemorySegment).get(JAVA_SHORT_UNALIGNED, i * bytesOf[A])
      if o.swapped then java.lang.Short.reverseBytes(x) else x
    inline def getC_xe(i: Long)(using o: Order): Char =
      val x = (m: MemorySegment).get(JAVA_CHAR_UNALIGNED, i * bytesOf[A])
      if o.swapped then java.lang.Character.reverseBytes(x) else x
    inline def getI_xe(i: Long)(using o: Order): Int =
      val x = (m: MemorySegment).get(JAVA_INT_UNALIGNED, i * bytesOf[A])
      if o.swapped then java.lang.Integer.reverseBytes(x) else x
    inline def getL_xe(i: Long)(using o: Order): Long =
      val x = (m: MemorySegment).get(JAVA_LONG_UNALIGNED, i * bytesOf[A])
      if o.swapped then java.lang.Long.reverseBytes(x) else x
    inline def getF_xe(i: Long)(using o: Order): Float =
      if o.swapped then java.lang.Float.intBitsToFloat(java.lang.Integer.reverseBytes((m: MemorySegment).get(JAVA_INT_UNALIGNED, i * bytesOf[A])))
      else (m: MemorySegment).get(JAVA_FLOAT_UNALIGNED, i * bytesOf[A])
    inline def getD_xe(i: Long)(using o: Order): Double =
      if o.swapped then java.lang.Double.longBitsToDouble(java.lang.Long.reverseBytes((m: MemorySegment).get(JAVA_LONG_UNALIGNED, i * bytesOf[A])))
      else (m: MemorySegment).get(JAVA_DOUBLE_UNALIGNED, i * bytesOf[A])

    /** Write a primitive of the stated type at element index `i` (byte offset `i * bytesOf[A]`)
      * in the stated byte order, as for the ordered `get` family.
      */
    inline def setS_le(i: Long, x: Short): Unit =
      (m: MemorySegment).set(JAVA_SHORT_UNALIGNED, i * bytesOf[A], if bigEndianHost then java.lang.Short.reverseBytes(x) else x)
    inline def setC_le(i: Long, x: Char): Unit =
      (m: MemorySegment).set(JAVA_CHAR_UNALIGNED, i * bytesOf[A], if bigEndianHost then java.lang.Character.reverseBytes(x) else x)
    inline def setI_le(i: Long, x: Int): Unit =
      (m: MemorySegment).set(JAVA_INT_UNALIGNED, i * bytesOf[A], if bigEndianHost then java.lang.Integer.reverseBytes(x) else x)
    inline def setL_le(i: Long, x: Long): Unit =
      (m: MemorySegment).set(JAVA_LONG_UNALIGNED, i * bytesOf[A], if bigEndianHost then java.lang.Long.reverseBytes(x) else x)
    inline def setF_le(i: Long, x: Float): Unit =
      if bigEndianHost then (m: MemorySegment).set(JAVA_INT_UNALIGNED, i * bytesOf[A], java.lang.Integer.reverseBytes(java.lang.Float.floatToRawIntBits(x)))
      else (m: MemorySegment).set(JAVA_FLOAT_UNALIGNED, i * bytesOf[A], x)
    inline def setD_le(i: Long, x: Double): Unit =
      if bigEndianHost then (m: MemorySegment).set(JAVA_LONG_UNALIGNED, i * bytesOf[A], java.lang.Long.reverseBytes(java.lang.Double.doubleToRawLongBits(x)))
      else (m: MemorySegment).set(JAVA_DOUBLE_UNALIGNED, i * bytesOf[A], x)
    inline def setS_be(i: Long, x: Short): Unit =
      (m: MemorySegment).set(JAVA_SHORT_UNALIGNED, i * bytesOf[A], if bigEndianHost then x else java.lang.Short.reverseBytes(x))
    inline def setC_be(i: Long, x: Char): Unit =
      (m: MemorySegment).set(JAVA_CHAR_UNALIGNED, i * bytesOf[A], if bigEndianHost then x else java.lang.Character.reverseBytes(x))
    inline def setI_be(i: Long, x: Int): Unit =
      (m: MemorySegment).set(JAVA_INT_UNALIGNED, i * bytesOf[A], if bigEndianHost then x else java.lang.Integer.reverseBytes(x))
    inline def setL_be(i: Long, x: Long): Unit =
      (m: MemorySegment).set(JAVA_LONG_UNALIGNED, i * bytesOf[A], if bigEndianHost then x else java.lang.Long.reverseBytes(x))
    inline def setF_be(i: Long, x: Float): Unit =
      if bigEndianHost then (m: MemorySegment).set(JAVA_FLOAT_UNALIGNED, i * bytesOf[A], x)
      else (m: MemorySegment).set(JAVA_INT_UNALIGNED, i * bytesOf[A], java.lang.Integer.reverseBytes(java.lang.Float.floatToRawIntBits(x)))
    inline def setD_be(i: Long, x: Double): Unit =
      if bigEndianHost then (m: MemorySegment).set(JAVA_DOUBLE_UNALIGNED, i * bytesOf[A], x)
      else (m: MemorySegment).set(JAVA_LONG_UNALIGNED, i * bytesOf[A], java.lang.Long.reverseBytes(java.lang.Double.doubleToRawLongBits(x)))
    inline def setS_xe(i: Long, x: Short)(using o: Order): Unit =
      (m: MemorySegment).set(JAVA_SHORT_UNALIGNED, i * bytesOf[A], if o.swapped then java.lang.Short.reverseBytes(x) else x)
    inline def setC_xe(i: Long, x: Char)(using o: Order): Unit =
      (m: MemorySegment).set(JAVA_CHAR_UNALIGNED, i * bytesOf[A], if o.swapped then java.lang.Character.reverseBytes(x) else x)
    inline def setI_xe(i: Long, x: Int)(using o: Order): Unit =
      (m: MemorySegment).set(JAVA_INT_UNALIGNED, i * bytesOf[A], if o.swapped then java.lang.Integer.reverseBytes(x) else x)
    inline def setL_xe(i: Long, x: Long)(using o: Order): Unit =
      (m: MemorySegment).set(JAVA_LONG_UNALIGNED, i * bytesOf[A], if o.swapped then java.lang.Long.reverseBytes(x) else x)
    inline def setF_xe(i: Long, x: Float)(using o: Order): Unit =
      if o.swapped then (m: MemorySegment).set(JAVA_INT_UNALIGNED, i * bytesOf[A], java.lang.Integer.reverseBytes(java.lang.Float.floatToRawIntBits(x)))
      else (m: MemorySegment).set(JAVA_FLOAT_UNALIGNED, i * bytesOf[A], x)
    inline def setD_xe(i: Long, x: Double)(using o: Order): Unit =
      if o.swapped then (m: MemorySegment).set(JAVA_LONG_UNALIGNED, i * bytesOf[A], java.lang.Long.reverseBytes(java.lang.Double.doubleToRawLongBits(x)))
      else (m: MemorySegment).set(JAVA_DOUBLE_UNALIGNED, i * bytesOf[A], x)

    /** A bounds-clipping view: out-of-range indices are silently skipped or clamped. */
    inline def clip: kse.basics.ClippedMem[A] = ClippedMem wrap m

    /** An order-aware view of the same memory: element access takes the `Mem.Order` given in
      * scope (e.g. via `import Mem.BE`), so reads and writes say their byte order.
      */
    inline def orderAware: Mem.OrderAware[A] = OrderAware.wrap[A](m)

    inline def use()(inline f: A => Unit): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        f(m(i))
        i += 1
    inline def use(i0: Long, iN: Long)(inline f: A => Unit): Unit =
      var i = i0
      while i < iN do
        f(m(i))
        i += 1
    inline def use(indices: Array[Long])(inline f: A => Unit): Unit =
      var i = 0
      while i < indices.length do
        f(m(indices(i)))
        i += 1
    inline def use(indices: LongStepper)(inline f: A => Unit): Unit =
      while indices.hasStep do
        f(m(indices.nextStep()))
    inline def use(inline p: A => Boolean)(inline f: A => Unit): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        val x = m(i)
        if p(x) then f(x)
        i += 1

    inline def alter()(inline f: A => A): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        m(i) = f(m(i))
        i += 1
    inline def alter(i0: Long, iN: Long)(inline f: A => A): Unit =
      var i = i0
      while i < iN do
        m(i) = f(m(i))
        i += 1
    inline def alter(indices: Array[Long])(inline f: A => A): Unit =
      var i = 0
      while i < indices.length do
        val j = indices(i)
        m(j) = f(m(j))
        i += 1
    inline def alter(indices: LongStepper)(inline f: A => A): Unit =
      while indices.hasStep do
        val j = indices.nextStep()
        m(j) = f(m(j))
    inline def alter(inline p: A => Boolean)(inline f: A => A): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        val x = m(i)
        if p(x) then m(i) = f(x)
        i += 1

    inline def visit()(inline f: (A, Long) => Unit): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        f(m(i), i)
        i += 1
    inline def visit(i0: Long, iN: Long)(inline f: (A, Long) => Unit): Unit =
      var i = i0
      while i < iN do
        f(m(i), i)
        i += 1
    inline def visit(indices: Array[Long])(inline f: (A, Long) => Unit): Unit =
      var i = 0
      while i < indices.length do
        val j = indices(i)
        f(m(j), j)
        i += 1
    inline def visit(indices: LongStepper)(inline f: (A, Long) => Unit): Unit =
      while indices.hasStep do
        val j = indices.nextStep()
        f(m(j), j)
    inline def visit(inline p: A => Boolean)(inline f: (A, Long) => Unit): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        val x = m(i)
        if p(x) then f(x, i)
        i += 1

    inline def edit()(inline f: (A, Long) => A): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        m(i) = f(m(i), i)
        i += 1
    inline def edit(i0: Long, iN: Long)(inline f: (A, Long) => A): Unit =
      var i = i0
      while i < iN do
        m(i) = f(m(i), i)
        i += 1
    inline def edit(indices: Array[Long])(inline f: (A, Long) => A): Unit =
      var i = 0
      while i < indices.length do
        val j = indices(i)
        m(j) = f(m(j), j)
        i += 1
    inline def edit(indices: LongStepper)(inline f: (A, Long) => A): Unit =
      while indices.hasStep do
        val j = indices.nextStep()
        m(j) = f(m(j), j)
    inline def edit(inline p: A => Boolean)(inline f: (A, Long) => A): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        val x = m(i)
        if p(x) then m(i) = f(x, i)
        i += 1

    /** Visit each adjacent pair (x(i), x(i+1)). */
    inline def pairs(inline f: (A, A) => Unit): Unit =
      val n = m.length
      if n > 0 then
        var a0 = m(0)
        var i = 1L
        while i < n do
          val a1 = m(i)
          f(a0, a1)
          a0 = a1
          i += 1
    /** Visit each adjacent triple (x(i), x(i+1), x(i+2)). */
    inline def trios(inline f: (A, A, A) => Unit): Unit =
      val n = m.length
      if n > 1 then
        var a0 = m(0)
        var a1 = m(1)
        var i = 2L
        while i < n do
          val a2 = m(i)
          f(a0, a1, a2)
          a0 = a1
          a1 = a2
          i += 1

    /** Visit elements of this and `b` in lockstep, up to the shorter length. */
    inline def together[B <: Type](b: Mem[B])(inline f: (A, B, Long) => Unit): Unit =
      val nb = b.length
      var n = m.length
      if nb < n then n = nb
      var i = 0L
      while i < n do
        f(m(i), b(i), i)
        i += 1
    inline def together[B <: Type, C <: Type](b: Mem[B], c: Mem[C])(inline f: (A, B, C, Long) => Unit): Unit =
      var n = m.length
      val nb = b.length
      val nc = c.length
      if nb < n then n = nb
      if nc < n then n = nc
      var i = 0L
      while i < n do
        f(m(i), b(i), c(i), i)
        i += 1

    /** Follow indices produced by `f` until one falls out of range; returns the number of steps. */
    inline def wander()(inline f: (A, Long) => Long): Long =
      wander(0L)(f)
    inline def wander(start: Long)(inline f: (A, Long) => Long): Long =
      var n = 0L
      var i = start
      val len = m.length
      while i >= 0 && i < len && n < Long.MaxValue do
        n += 1
        i = f(m(i), i)
      n

    inline def gather[Z](zero: Z)()(inline f: (Z, A, Long) => Z): Z =
      var i = 0L
      val n = m.length
      var z = zero
      while i < n do
        z = f(z, m(i), i)
        i += 1
      z
    inline def gather[Z](zero: Z)(i0: Long, iN: Long)(inline f: (Z, A, Long) => Z): Z =
      var i = i0
      var z = zero
      while i < iN do
        z = f(z, m(i), i)
        i += 1
      z
    inline def gather[Z](zero: Z)(indices: Array[Long])(inline f: (Z, A, Long) => Z): Z =
      var i = 0
      var z = zero
      while i < indices.length do
        val j = indices(i)
        z = f(z, m(j), j)
        i += 1
      z
    inline def gather[Z](zero: Z)(indices: LongStepper)(inline f: (Z, A, Long) => Z): Z =
      var z = zero
      while indices.hasStep do
        val j = indices.nextStep()
        z = f(z, m(j), j)
      z
    inline def gather[Z](zero: Z)(inline p: A => Boolean)(inline f: (Z, A, Long) => Z): Z =
      var i = 0L
      val n = m.length
      var z = zero
      while i < n do
        val x = m(i)
        if p(x) then z = f(z, x, i)
        i += 1
      z

    @targetName("update_All_constant")
    inline def update(value: A): Unit =
      update(0L, m.length, value)
    @targetName("update_All_segment")
    inline def update(values: Mem[A]): Unit =
      update(0L, m.length, values)

    @targetName("update_i0iN_constant")
    inline def update(i0: Long, iN: Long, value: A): Unit =
      var i = i0
      while i < iN do
        m(i) = value
        i += 1
    @targetName("update_i0iN_segment")
    inline def update(i0: Long, iN: Long, values: Mem[A]): Unit =
      MemorySegment.copy(values, 0L, m, i0 * bytesOf[A], (iN - i0) * bytesOf[A])

    @targetName("update_Places_constant")
    inline def update(indices: Array[Long], value: A): Unit =
      var i = 0
      while i < indices.length do
        m(indices(i)) = value
        i += 1
    @targetName("update_Places_segment")
    inline def update(indices: Array[Long], values: Mem[A]): Unit =
      var i = 0
      while i < indices.length do
        m(indices(i)) = values(i)
        i += 1

    @targetName("update_Stepper_constant")
    inline def update(indices: LongStepper, value: A): Unit =
      while indices.hasStep do
        m(indices.nextStep()) = value
    @targetName("update_Stepper_segment")
    inline def update(indices: LongStepper, values: Mem[A]): Unit =
      var i = 0L
      while indices.hasStep do
        m(indices.nextStep()) = values(i)
        i += 1

    @targetName("update_Selector")
    inline def update(inline pick: A => Boolean, value: A): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        if pick(m(i)) then m(i) = value
        i += 1

    @targetName("set_All_generate")
    inline def set()(inline generator: () => A): Unit =
      set(0L, m.length)(generator)
    @targetName("set_All_index")
    inline def set()(inline indexer: Long => A): Unit =
      set(0L, m.length)(indexer)

    @targetName("set_i0iN_generate")
    inline def set(i0: Long, iN: Long)(inline generator: () => A): Unit =
      var i = i0
      while i < iN do
        m(i) = generator()
        i += 1
    @targetName("set_i0iN_index")
    inline def set(i0: Long, iN: Long)(inline indexer: Long => A): Unit =
      var i = i0
      while i < iN do
        m(i) = indexer(i)
        i += 1

    @targetName("set_Places_generate")
    inline def set(indices: Array[Long])(inline generator: () => A): Unit =
      var i = 0
      while i < indices.length do
        m(indices(i)) = generator()
        i += 1
    @targetName("set_Places_index")
    inline def set(indices: Array[Long])(inline indexer: Long => A): Unit =
      var i = 0
      while i < indices.length do
        val j = indices(i)
        m(j) = indexer(j)
        i += 1

    @targetName("set_Stepper_generate")
    inline def set(indices: LongStepper)(inline generator: () => A): Unit =
      while indices.hasStep do
        m(indices.nextStep()) = generator()
    @targetName("set_Stepper_index")
    inline def set(indices: LongStepper)(inline indexer: Long => A): Unit =
      while indices.hasStep do
        val j = indices.nextStep()
        m(j) = indexer(j)

    @targetName("set_Selector_generate")
    inline def set(inline pick: A => Boolean)(inline generator: () => A): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        if pick(m(i)) then m(i) = generator()
        i += 1
    @targetName("set_Selector_index")
    inline def set(inline pick: A => Boolean)(inline indexer: Long => A): Unit =
      var i = 0L
      val n = m.length
      while i < n do
        if pick(m(i)) then m(i) = indexer(i)
        i += 1

    /** All indices, 0 until length. */
    inline def where(): Array[Long] =
      val ix = new Array[Long](m.length.toInt)
      var i = 0
      while i < ix.length do
        ix(i) = i.toLong
        i += 1
      ix
    inline def where(inline pick: A => Boolean): Array[Long] =
      whereIn(0L, m.length)(pick)
    inline def whereOp(inline pick: (A, Long) => Long): Array[Long] =
      whereInOp(0L, m.length)(pick)

    inline def whereIn(i0: Long, iN: Long)(inline pick: A => Boolean): Array[Long] =
      var ix = new Array[Long](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else (iN - i0).toInt)
      var i = i0
      var j = 0
      while i < iN do
        if pick(m(i)) then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = i
          j += 1
        i += 1
      ix.shrinkTo(j)
    inline def whereInOp(i0: Long, iN: Long)(inline pick: (A, Long) => Long): Array[Long] =
      var ix = new Array[Long](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else (iN - i0).toInt)
      var i = i0
      var j = 0
      while i < iN do
        val h = pick(m(i), i)
        if h >= 0 then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = h
          j += 1
        i += 1
      ix.shrinkTo(j)

    inline def whereFrom(indices: Array[Long])(inline pick: A => Boolean): Array[Long] =
      var ix = new Array[Long](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        if pick(m(k)) then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = k
          j += 1
        i += 1
      ix.shrinkTo(j)
    inline def whereFromOp(indices: Array[Long])(inline pick: (A, Long) => Long): Array[Long] =
      var ix = new Array[Long](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        val h = pick(m(k), k)
        if h >= 0 then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = h
          j += 1
        i += 1
      ix.shrinkTo(j)

    inline def whereFwd(i: Long)(inline f: A => Boolean): Long =
      if i < 0 then -1L
      else boundary[Long]:
        var j = i
        val n = m.length
        while j < n do
          if f(m(j)) then boundary.break(j)
          j += 1
        -1L
    inline def whereBkw(i: Long)(inline f: A => Boolean): Long =
      if i >= m.length then -1
      else boundary[Long]:
        var j = i
        while j >= 0 do
          if f(m(j)) then boundary.break(j)
          j -= 1
        -1L

    /** The first index in `[i0, iN)` (clamped) holding exactly `value`, or -1 if there is none.
      * Byte through Int widths scan a 64-bit lane at a time, so big blocks go fast.  Floating
      * types are matched by raw bit pattern, not IEEE `==`: NaN finds a bit-identical NaN, and
      * 0.0 does not find -0.0.
      */
    inline def whereIsFwd(i0: Long, iN: Long)(value: A): Long = inline erasedValue[A] match
      case _: Byte   => seekIsByte((m: MemorySegment), i0, iN, value.asInstanceOf[Byte])
      case _: Short  => seekIsShort((m: MemorySegment), i0, iN, value.asInstanceOf[Short])
      case _: Char   => seekIsShort((m: MemorySegment), i0, iN, value.asInstanceOf[Char].toShort)
      case _: Int    => seekIsInt((m: MemorySegment), i0, iN, value.asInstanceOf[Int])
      case _: Long   => seekIsLong((m: MemorySegment), i0, iN, value.asInstanceOf[Long])
      case _: Float  => seekIsInt((m: MemorySegment), i0, iN, java.lang.Float.floatToRawIntBits(value.asInstanceOf[Float]))
      case _: Double => seekIsLong((m: MemorySegment), i0, iN, java.lang.Double.doubleToRawLongBits(value.asInstanceOf[Double]))
      case _         => error("Mem only supports primitive element types")

    /** The last index in `[i0, iN)` (clamped) holding exactly `value`, or -1 if there is none;
      * matching as for `whereIsFwd`.
      */
    inline def whereIsBkw(i0: Long, iN: Long)(value: A): Long = inline erasedValue[A] match
      case _: Byte   => seekIsByteBkw((m: MemorySegment), i0, iN, value.asInstanceOf[Byte])
      case _: Short  => seekIsShortBkw((m: MemorySegment), i0, iN, value.asInstanceOf[Short])
      case _: Char   => seekIsShortBkw((m: MemorySegment), i0, iN, value.asInstanceOf[Char].toShort)
      case _: Int    => seekIsIntBkw((m: MemorySegment), i0, iN, value.asInstanceOf[Int])
      case _: Long   => seekIsLongBkw((m: MemorySegment), i0, iN, value.asInstanceOf[Long])
      case _: Float  => seekIsIntBkw((m: MemorySegment), i0, iN, java.lang.Float.floatToRawIntBits(value.asInstanceOf[Float]))
      case _: Double => seekIsLongBkw((m: MemorySegment), i0, iN, java.lang.Double.doubleToRawLongBits(value.asInstanceOf[Double]))
      case _         => error("Mem only supports primitive element types")

    /** Copy elements into a caller-provided destination; returns the number copied. */
    inline def inject(that: Mem[A]): Long =
      inject(that, 0L)(0L, m.length)
    inline def inject(that: Mem[A], where: Long): Long =
      inject(that, where)(0L, m.length)
    inline def inject(that: Mem[A])(i0: Long, iN: Long): Long =
      inject(that, 0L)(i0, iN)
    inline def inject(that: Mem[A], where: Long)(i0: Long, iN: Long): Long =
      val eb = bytesOf[A]
      MemorySegment.copy(m, i0 * eb, that, where * eb, (iN - i0) * eb)
      iN - i0
    inline def inject(that: Mem[A])(indices: Array[Long]): Long =
      inject(that, 0L)(indices)
    inline def inject(that: Mem[A], where: Long)(indices: Array[Long]): Long =
      var i = 0
      var j = where
      while i < indices.length do
        that(j) = m(indices(i))
        i += 1
        j += 1
      i
    inline def inject(that: Mem[A])(indices: LongStepper): Long =
      inject(that, 0L)(indices)
    inline def inject(that: Mem[A], where: Long)(indices: LongStepper): Long =
      var j = where
      while indices.hasStep do
        that(j) = m(indices.nextStep())
        j += 1
      j - where
    inline def inject(that: Mem[A])(inline pick: A => Boolean): Long =
      inject(that, 0L)(pick)
    inline def inject(that: Mem[A], where: Long)(inline pick: A => Boolean): Long =
      var i = 0L
      val n = m.length
      var j = where
      while i < n do
        val x = m(i)
        if pick(x) then
          that(j) = x
          j += 1
        i += 1
      j - where

    /** Copy elements into a caller-provided array; returns the number copied. */
    inline def inject(that: Array[A]): Long =
      inject(that, 0)(0L, m.length)
    inline def inject(that: Array[A], where: Int): Long =
      inject(that, where)(0L, m.length)
    inline def inject(that: Array[A])(i0: Long, iN: Long): Long =
      inject(that, 0)(i0, iN)
    inline def inject(that: Array[A], where: Int)(i0: Long, iN: Long): Long =
      MemorySegment.copy(m, layoutOf[A], i0 * bytesOf[A], that, where, (iN - i0).toInt)
      iN - i0
    inline def inject(that: Array[A])(indices: Array[Long]): Long =
      inject(that, 0)(indices)
    inline def inject(that: Array[A], where: Int)(indices: Array[Long]): Long =
      var i = 0
      var j = where
      while i < indices.length do
        that(j) = m(indices(i))
        i += 1
        j += 1
      i
    inline def inject(that: Array[A])(indices: LongStepper): Long =
      inject(that, 0)(indices)
    inline def inject(that: Array[A], where: Int)(indices: LongStepper): Long =
      var j = where
      while indices.hasStep do
        that(j) = m(indices.nextStep())
        j += 1
      j - where
    inline def inject(that: Array[A])(inline pick: A => Boolean): Long =
      inject(that, 0)(pick)
    inline def inject(that: Array[A], where: Int)(inline pick: A => Boolean): Long =
      var i = 0L
      val n = m.length
      var j = where
      while i < n do
        val x = m(i)
        if pick(x) then
          that(j) = x
          j += 1
        i += 1
      j - where

    /** Map elements into a caller-provided destination; returns the number written. */
    inline def injectOp[B <: Type](that: Mem[B])()(inline f: (A, Long) => B): Long =
      injectOp(that, 0L)(0L, m.length)(f)
    inline def injectOp[B <: Type](that: Mem[B], where: Long)()(inline f: (A, Long) => B): Long =
      injectOp(that, where)(0L, m.length)(f)
    inline def injectOp[B <: Type](that: Mem[B])(i0: Long, iN: Long)(inline f: (A, Long) => B): Long =
      injectOp(that, 0L)(i0, iN)(f)
    inline def injectOp[B <: Type](that: Mem[B], where: Long)(i0: Long, iN: Long)(inline f: (A, Long) => B): Long =
      var i = i0
      var j = where
      while i < iN do
        that(j) = f(m(i), i)
        j += 1
        i += 1
      iN - i0
    inline def injectOp[B <: Type](that: Mem[B])(indices: Array[Long])(inline f: (A, Long) => B): Long =
      injectOp(that, 0L)(indices)(f)
    inline def injectOp[B <: Type](that: Mem[B], where: Long)(indices: Array[Long])(inline f: (A, Long) => B): Long =
      var i = 0
      var j = where
      while i < indices.length do
        val k = indices(i)
        that(j) = f(m(k), k)
        i += 1
        j += 1
      i
    inline def injectOp[B <: Type](that: Mem[B])(indices: LongStepper)(inline f: (A, Long) => B): Long =
      injectOp(that, 0L)(indices)(f)
    inline def injectOp[B <: Type](that: Mem[B], where: Long)(indices: LongStepper)(inline f: (A, Long) => B): Long =
      var j = where
      while indices.hasStep do
        val i = indices.nextStep()
        that(j) = f(m(i), i)
        j += 1
      j - where
    inline def injectOp[B <: Type](that: Mem[B])(inline pick: A => Boolean)(inline f: (A, Long) => B): Long =
      injectOp(that, 0L)(pick)(f)
    inline def injectOp[B <: Type](that: Mem[B], where: Long)(inline pick: A => Boolean)(inline f: (A, Long) => B): Long =
      var i = 0L
      val n = m.length
      var j = where
      while i < n do
        val x = m(i)
        if pick(x) then
          that(j) = f(x, i)
          j += 1
        i += 1
      j - where

    /** Visit maximal runs delimited where `cut(prev, next)` holds, passing each run's [i, j). */
    inline def visitCuts()(inline cut: (A, A) => Boolean)(inline f: (Long, Long) => Unit): Unit =
      visitCuts(0L, m.length)(cut)(f)
    inline def visitCuts(i0: Long, iN: Long)(inline cut: (A, A) => Boolean)(inline f: (Long, Long) => Unit): Unit =
      var i = i0
      while i < iN do
        var x = m(i)
        var j = i + 1
        var continue = true
        while continue && j < iN do
          val y = m(j)
          if cut(x, y) then continue = false
          else
            x = y
            j += 1
        f(i, j)
        i = j

    /** Zero-copy reinterpretation as another primitive (any trailing partial element is ignored by `length`). */
    inline def as[B <: Type]: Mem[B] = wrap[B](m)

    /** Zero-copy view of elements `[i0, iN)`, sharing this memory and its lifetime. */
    inline def view(i0: Long, iN: Long): Mem[A] =
      wrap[A]((m: MemorySegment).asSlice(i0 * bytesOf[A], (iN - i0) * bytesOf[A]))

    /** Zero-copy view of elements `[i0, iN)` (indices in units of `A`) reinterpreted as a `Mem[B]`. */
    inline def viewAs[B <: Type](i0: Long, iN: Long): Mem[B] =
      wrap[B]((m: MemorySegment).asSlice(i0 * bytesOf[A], (iN - i0) * bytesOf[A]))

    /** Zero-copy reinterpretation as an array of structs (any trailing partial struct is ignored by its `length`). */
    inline def aos[T <: NamedTuple.AnyNamedTuple]: Mem.AoS[T] = AoS.wrap[T](m)
  }

  // === Mem.OrderAware: element access in a byte order given by the scope ===

  /** Array-like memory access where every element read or write takes the [[Mem.Order]] given
    * in scope (e.g. via `import Mem.BE`): the same memory as the `Mem[A]` it views, with the
    * byte order stated once instead of at every access.  Methods that move raw bytes between
    * same-order views, or that never touch element values, do not consult the order.  Note
    * that the underlying `Mem[A]` remains usable (and native-order) alongside this view; keep
    * whichever handle you mean to read with.
    */
  opaque type OrderAware[A <: Mem.Type] = MemorySegment
  object OrderAware {
    /** Wrap a caller-owned segment.  The caller retains responsibility for its lifetime. */
    inline def wrap[A <: Type](seg: MemorySegment): OrderAware[A] = seg

    /** Read element `i` of `seg` in byte order `o` (kernel behind `apply`): constant native
      * layouts (the JIT's fast path) plus a conditional `reverseBytes` intrinsic--the swapped
      * flag is loop-invariant and hoists.
      */
    inline def read[A <: Type](seg: MemorySegment, o: Order, i: Long): A = inline erasedValue[A] match
      case _: Byte    => seg.get(JAVA_BYTE, i).asInstanceOf[A]
      case _: Short   =>
        val x = seg.getAtIndex(JAVA_SHORT_UNALIGNED, i)
        (if o.swapped then java.lang.Short.reverseBytes(x) else x).asInstanceOf[A]
      case _: Char    =>
        val x = seg.getAtIndex(JAVA_CHAR_UNALIGNED, i)
        (if o.swapped then java.lang.Character.reverseBytes(x) else x).asInstanceOf[A]
      case _: Int     =>
        val x = seg.getAtIndex(JAVA_INT_UNALIGNED, i)
        (if o.swapped then java.lang.Integer.reverseBytes(x) else x).asInstanceOf[A]
      case _: Float   =>
        (if o.swapped then java.lang.Float.intBitsToFloat(java.lang.Integer.reverseBytes(seg.getAtIndex(JAVA_INT_UNALIGNED, i)))
         else seg.getAtIndex(JAVA_FLOAT_UNALIGNED, i)).asInstanceOf[A]
      case _: Long    =>
        val x = seg.getAtIndex(JAVA_LONG_UNALIGNED, i)
        (if o.swapped then java.lang.Long.reverseBytes(x) else x).asInstanceOf[A]
      case _: Double  =>
        (if o.swapped then java.lang.Double.longBitsToDouble(java.lang.Long.reverseBytes(seg.getAtIndex(JAVA_LONG_UNALIGNED, i)))
         else seg.getAtIndex(JAVA_DOUBLE_UNALIGNED, i)).asInstanceOf[A]
      case _          => error("Mem only supports primitive element types")

    /** Write element `i` of `seg` in byte order `o` (kernel behind `update`); fast-path shape as for `read`. */
    inline def write[A <: Type](seg: MemorySegment, o: Order, i: Long, x: A): Unit = inline erasedValue[A] match
      case _: Byte    => seg.set(JAVA_BYTE, i, x.asInstanceOf[Byte])
      case _: Short   =>
        val v = x.asInstanceOf[Short]
        seg.setAtIndex(JAVA_SHORT_UNALIGNED, i, if o.swapped then java.lang.Short.reverseBytes(v) else v)
      case _: Char    =>
        val v = x.asInstanceOf[Char]
        seg.setAtIndex(JAVA_CHAR_UNALIGNED, i, if o.swapped then java.lang.Character.reverseBytes(v) else v)
      case _: Int     =>
        val v = x.asInstanceOf[Int]
        seg.setAtIndex(JAVA_INT_UNALIGNED, i, if o.swapped then java.lang.Integer.reverseBytes(v) else v)
      case _: Float   =>
        if o.swapped then seg.setAtIndex(JAVA_INT_UNALIGNED, i, java.lang.Integer.reverseBytes(java.lang.Float.floatToRawIntBits(x.asInstanceOf[Float])))
        else seg.setAtIndex(JAVA_FLOAT_UNALIGNED, i, x.asInstanceOf[Float])
      case _: Long    =>
        val v = x.asInstanceOf[Long]
        seg.setAtIndex(JAVA_LONG_UNALIGNED, i, if o.swapped then java.lang.Long.reverseBytes(v) else v)
      case _: Double  =>
        if o.swapped then seg.setAtIndex(JAVA_LONG_UNALIGNED, i, java.lang.Long.reverseBytes(java.lang.Double.doubleToRawLongBits(x.asInstanceOf[Double])))
        else seg.setAtIndex(JAVA_DOUBLE_UNALIGNED, i, x.asInstanceOf[Double])
      case _          => error("Mem only supports primitive element types")

    /** The sought value with its bytes as they would lie in memory under `o`, so native-lane seeks find it. */
    inline def seekBits[A <: Type](o: Order, value: A): A = inline erasedValue[A] match
      case _: Byte    => value
      case _: Short   => (if o.swapped then java.lang.Short.reverseBytes(value.asInstanceOf[Short]) else value.asInstanceOf[Short]).asInstanceOf[A]
      case _: Char    => (if o.swapped then java.lang.Character.reverseBytes(value.asInstanceOf[Char]) else value.asInstanceOf[Char]).asInstanceOf[A]
      case _: Int     => (if o.swapped then java.lang.Integer.reverseBytes(value.asInstanceOf[Int]) else value.asInstanceOf[Int]).asInstanceOf[A]
      case _: Long    => (if o.swapped then java.lang.Long.reverseBytes(value.asInstanceOf[Long]) else value.asInstanceOf[Long]).asInstanceOf[A]
      case _: Float   =>
        val b = java.lang.Float.floatToRawIntBits(value.asInstanceOf[Float])
        java.lang.Float.intBitsToFloat(if o.swapped then java.lang.Integer.reverseBytes(b) else b).asInstanceOf[A]
      case _: Double  =>
        val b = java.lang.Double.doubleToRawLongBits(value.asInstanceOf[Double])
        java.lang.Double.longBitsToDouble(if o.swapped then java.lang.Long.reverseBytes(b) else b).asInstanceOf[A]
      case _          => error("Mem only supports primitive element types")

    extension [A <: Type](m: OrderAware[A]) {
      /** The underlying storage, to use FFM calls directly. */
      inline def segment: MemorySegment = m

      /** The same memory as a plain (native-order) `Mem[A]`. */
      inline def mem: Mem[A] = Mem.wrap[A](m)

      /** Number of elements = floor(byteSize / elementBytes). */
      inline def length: Long = (m: MemorySegment).byteSize / bytesOf[A]

      inline def apply(i: Long)(using o: Order): A = read[A]((m: MemorySegment), o, i)

      inline def update(i: Long, x: A)(using o: Order): Unit = write[A]((m: MemorySegment), o, i, x)

      /** Read a primitive of the stated type at element index `i` (byte offset `i * bytesOf[A]`)
        * in the `Mem.Order` given in scope; bytes have no order, so `getB` takes none.
        */
      inline def getB(i: Long): Byte = (m: MemorySegment).get(JAVA_BYTE, i * bytesOf[A])
      inline def getS(i: Long)(using o: Order): Short  = Mem.getS_xe[A](Mem.wrap[A](m))(i)
      inline def getC(i: Long)(using o: Order): Char   = Mem.getC_xe[A](Mem.wrap[A](m))(i)
      inline def getI(i: Long)(using o: Order): Int    = Mem.getI_xe[A](Mem.wrap[A](m))(i)
      inline def getF(i: Long)(using o: Order): Float  = Mem.getF_xe[A](Mem.wrap[A](m))(i)
      inline def getL(i: Long)(using o: Order): Long   = Mem.getL_xe[A](Mem.wrap[A](m))(i)
      inline def getD(i: Long)(using o: Order): Double = Mem.getD_xe[A](Mem.wrap[A](m))(i)

      /** Write a primitive of the stated type at element index `i` (byte offset `i * bytesOf[A]`)
        * in the `Mem.Order` given in scope; bytes have no order, so `setB` takes none.
        */
      inline def setB(i: Long, x: Byte): Unit = (m: MemorySegment).set(JAVA_BYTE, i * bytesOf[A], x)
      inline def setS(i: Long, x: Short)(using o: Order):  Unit = Mem.setS_xe[A](Mem.wrap[A](m))(i, x)
      inline def setC(i: Long, x: Char)(using o: Order):   Unit = Mem.setC_xe[A](Mem.wrap[A](m))(i, x)
      inline def setI(i: Long, x: Int)(using o: Order):    Unit = Mem.setI_xe[A](Mem.wrap[A](m))(i, x)
      inline def setF(i: Long, x: Float)(using o: Order):  Unit = Mem.setF_xe[A](Mem.wrap[A](m))(i, x)
      inline def setL(i: Long, x: Long)(using o: Order):   Unit = Mem.setL_xe[A](Mem.wrap[A](m))(i, x)
      inline def setD(i: Long, x: Double)(using o: Order): Unit = Mem.setD_xe[A](Mem.wrap[A](m))(i, x)

      inline def use()(inline f: A => Unit)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          f(m(i))
          i += 1
      inline def use(i0: Long, iN: Long)(inline f: A => Unit)(using o: Order): Unit =
        var i = i0
        while i < iN do
          f(m(i))
          i += 1
      inline def use(indices: Array[Long])(inline f: A => Unit)(using o: Order): Unit =
        var i = 0
        while i < indices.length do
          f(m(indices(i)))
          i += 1
      inline def use(indices: LongStepper)(inline f: A => Unit)(using o: Order): Unit =
        while indices.hasStep do
          f(m(indices.nextStep()))
      inline def use(inline p: A => Boolean)(inline f: A => Unit)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          val x = m(i)
          if p(x) then f(x)
          i += 1

      inline def alter()(inline f: A => A)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          m(i) = f(m(i))
          i += 1
      inline def alter(i0: Long, iN: Long)(inline f: A => A)(using o: Order): Unit =
        var i = i0
        while i < iN do
          m(i) = f(m(i))
          i += 1
      inline def alter(indices: Array[Long])(inline f: A => A)(using o: Order): Unit =
        var i = 0
        while i < indices.length do
          val j = indices(i)
          m(j) = f(m(j))
          i += 1
      inline def alter(indices: LongStepper)(inline f: A => A)(using o: Order): Unit =
        while indices.hasStep do
          val j = indices.nextStep()
          m(j) = f(m(j))
      inline def alter(inline p: A => Boolean)(inline f: A => A)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          val x = m(i)
          if p(x) then m(i) = f(x)
          i += 1

      inline def visit()(inline f: (A, Long) => Unit)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          f(m(i), i)
          i += 1
      inline def visit(i0: Long, iN: Long)(inline f: (A, Long) => Unit)(using o: Order): Unit =
        var i = i0
        while i < iN do
          f(m(i), i)
          i += 1
      inline def visit(indices: Array[Long])(inline f: (A, Long) => Unit)(using o: Order): Unit =
        var i = 0
        while i < indices.length do
          val j = indices(i)
          f(m(j), j)
          i += 1
      inline def visit(indices: LongStepper)(inline f: (A, Long) => Unit)(using o: Order): Unit =
        while indices.hasStep do
          val j = indices.nextStep()
          f(m(j), j)
      inline def visit(inline p: A => Boolean)(inline f: (A, Long) => Unit)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          val x = m(i)
          if p(x) then f(x, i)
          i += 1

      inline def edit()(inline f: (A, Long) => A)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          m(i) = f(m(i), i)
          i += 1
      inline def edit(i0: Long, iN: Long)(inline f: (A, Long) => A)(using o: Order): Unit =
        var i = i0
        while i < iN do
          m(i) = f(m(i), i)
          i += 1
      inline def edit(indices: Array[Long])(inline f: (A, Long) => A)(using o: Order): Unit =
        var i = 0
        while i < indices.length do
          val j = indices(i)
          m(j) = f(m(j), j)
          i += 1
      inline def edit(indices: LongStepper)(inline f: (A, Long) => A)(using o: Order): Unit =
        while indices.hasStep do
          val j = indices.nextStep()
          m(j) = f(m(j), j)
      inline def edit(inline p: A => Boolean)(inline f: (A, Long) => A)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          val x = m(i)
          if p(x) then m(i) = f(x, i)
          i += 1

      /** Visit each adjacent pair (x(i), x(i+1)). */
      inline def pairs(inline f: (A, A) => Unit)(using o: Order): Unit =
        val n = m.length
        if n > 0 then
          var a0 = m(0)
          var i = 1L
          while i < n do
            val a1 = m(i)
            f(a0, a1)
            a0 = a1
            i += 1
      /** Visit each adjacent triple (x(i), x(i+1), x(i+2)). */
      inline def trios(inline f: (A, A, A) => Unit)(using o: Order): Unit =
        val n = m.length
        if n > 1 then
          var a0 = m(0)
          var a1 = m(1)
          var i = 2L
          while i < n do
            val a2 = m(i)
            f(a0, a1, a2)
            a0 = a1
            a1 = a2
            i += 1

      /** Visit elements of this and `b` in lockstep, both in the given order, up to the shorter length. */
      inline def together[B <: Type](b: OrderAware[B])(inline f: (A, B, Long) => Unit)(using o: Order): Unit =
        val nb = b.length
        var n = m.length
        if nb < n then n = nb
        var i = 0L
        while i < n do
          f(m(i), b(i), i)
          i += 1
      inline def together[B <: Type, C <: Type](b: OrderAware[B], c: OrderAware[C])(inline f: (A, B, C, Long) => Unit)(using o: Order): Unit =
        var n = m.length
        val nb = b.length
        val nc = c.length
        if nb < n then n = nb
        if nc < n then n = nc
        var i = 0L
        while i < n do
          f(m(i), b(i), c(i), i)
          i += 1

      /** Follow indices produced by `f` until one falls out of range; returns the number of steps. */
      inline def wander()(inline f: (A, Long) => Long)(using o: Order): Long =
        wander(0L)(f)
      inline def wander(start: Long)(inline f: (A, Long) => Long)(using o: Order): Long =
        var n = 0L
        var i = start
        val len = m.length
        while i >= 0 && i < len && n < Long.MaxValue do
          n += 1
          i = f(m(i), i)
        n

      inline def gather[Z](zero: Z)()(inline f: (Z, A, Long) => Z)(using o: Order): Z =
        var i = 0L
        val n = m.length
        var z = zero
        while i < n do
          z = f(z, m(i), i)
          i += 1
        z
      inline def gather[Z](zero: Z)(i0: Long, iN: Long)(inline f: (Z, A, Long) => Z)(using o: Order): Z =
        var i = i0
        var z = zero
        while i < iN do
          z = f(z, m(i), i)
          i += 1
        z
      inline def gather[Z](zero: Z)(indices: Array[Long])(inline f: (Z, A, Long) => Z)(using o: Order): Z =
        var i = 0
        var z = zero
        while i < indices.length do
          val j = indices(i)
          z = f(z, m(j), j)
          i += 1
        z
      inline def gather[Z](zero: Z)(indices: LongStepper)(inline f: (Z, A, Long) => Z)(using o: Order): Z =
        var z = zero
        while indices.hasStep do
          val j = indices.nextStep()
          z = f(z, m(j), j)
        z
      inline def gather[Z](zero: Z)(inline p: A => Boolean)(inline f: (Z, A, Long) => Z)(using o: Order): Z =
        var i = 0L
        val n = m.length
        var z = zero
        while i < n do
          val x = m(i)
          if p(x) then z = f(z, x, i)
          i += 1
        z

      @targetName("update_All_constant")
      inline def update(value: A)(using o: Order): Unit =
        update(0L, m.length, value)
      /** Raw byte copy from a same-order view: no element is reinterpreted, so no order is consulted. */
      @targetName("update_All_segment")
      inline def update(values: OrderAware[A]): Unit =
        update(0L, m.length, values)

      @targetName("update_i0iN_constant")
      inline def update(i0: Long, iN: Long, value: A)(using o: Order): Unit =
        var i = i0
        while i < iN do
          m(i) = value
          i += 1
      @targetName("update_i0iN_segment")
      inline def update(i0: Long, iN: Long, values: OrderAware[A]): Unit =
        MemorySegment.copy((values: MemorySegment), 0L, (m: MemorySegment), i0 * bytesOf[A], (iN - i0) * bytesOf[A])

      @targetName("update_Places_constant")
      inline def update(indices: Array[Long], value: A)(using o: Order): Unit =
        var i = 0
        while i < indices.length do
          m(indices(i)) = value
          i += 1
      @targetName("update_Places_segment")
      inline def update(indices: Array[Long], values: OrderAware[A])(using o: Order): Unit =
        var i = 0
        while i < indices.length do
          m(indices(i)) = values(i)
          i += 1

      @targetName("update_Stepper_constant")
      inline def update(indices: LongStepper, value: A)(using o: Order): Unit =
        while indices.hasStep do
          m(indices.nextStep()) = value
      @targetName("update_Stepper_segment")
      inline def update(indices: LongStepper, values: OrderAware[A])(using o: Order): Unit =
        var i = 0L
        while indices.hasStep do
          m(indices.nextStep()) = values(i)
          i += 1

      @targetName("update_Selector")
      inline def update(inline pick: A => Boolean, value: A)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          if pick(m(i)) then m(i) = value
          i += 1

      @targetName("set_All_generate")
      inline def set()(inline generator: () => A)(using o: Order): Unit =
        set(0L, m.length)(generator)
      @targetName("set_All_index")
      inline def set()(inline indexer: Long => A)(using o: Order): Unit =
        set(0L, m.length)(indexer)

      @targetName("set_i0iN_generate")
      inline def set(i0: Long, iN: Long)(inline generator: () => A)(using o: Order): Unit =
        var i = i0
        while i < iN do
          m(i) = generator()
          i += 1
      @targetName("set_i0iN_index")
      inline def set(i0: Long, iN: Long)(inline indexer: Long => A)(using o: Order): Unit =
        var i = i0
        while i < iN do
          m(i) = indexer(i)
          i += 1

      @targetName("set_Places_generate")
      inline def set(indices: Array[Long])(inline generator: () => A)(using o: Order): Unit =
        var i = 0
        while i < indices.length do
          m(indices(i)) = generator()
          i += 1
      @targetName("set_Places_index")
      inline def set(indices: Array[Long])(inline indexer: Long => A)(using o: Order): Unit =
        var i = 0
        while i < indices.length do
          val j = indices(i)
          m(j) = indexer(j)
          i += 1

      @targetName("set_Stepper_generate")
      inline def set(indices: LongStepper)(inline generator: () => A)(using o: Order): Unit =
        while indices.hasStep do
          m(indices.nextStep()) = generator()
      @targetName("set_Stepper_index")
      inline def set(indices: LongStepper)(inline indexer: Long => A)(using o: Order): Unit =
        while indices.hasStep do
          val j = indices.nextStep()
          m(j) = indexer(j)

      @targetName("set_Selector_generate")
      inline def set(inline pick: A => Boolean)(inline generator: () => A)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          if pick(m(i)) then m(i) = generator()
          i += 1
      @targetName("set_Selector_index")
      inline def set(inline pick: A => Boolean)(inline indexer: Long => A)(using o: Order): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          if pick(m(i)) then m(i) = indexer(i)
          i += 1

      /** All indices, 0 until length. */
      inline def where(): Array[Long] =
        val ix = new Array[Long](m.length.toInt)
        var i = 0
        while i < ix.length do
          ix(i) = i.toLong
          i += 1
        ix
      inline def where(inline pick: A => Boolean)(using o: Order): Array[Long] =
        whereIn(0L, m.length)(pick)
      inline def whereOp(inline pick: (A, Long) => Long)(using o: Order): Array[Long] =
        whereInOp(0L, m.length)(pick)

      inline def whereIn(i0: Long, iN: Long)(inline pick: A => Boolean)(using o: Order): Array[Long] =
        var ix = new Array[Long](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else (iN - i0).toInt)
        var i = i0
        var j = 0
        while i < iN do
          if pick(m(i)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = i
            j += 1
          i += 1
        ix.shrinkTo(j)
      inline def whereInOp(i0: Long, iN: Long)(inline pick: (A, Long) => Long)(using o: Order): Array[Long] =
        var ix = new Array[Long](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else (iN - i0).toInt)
        var i = i0
        var j = 0
        while i < iN do
          val h = pick(m(i), i)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
            j += 1
          i += 1
        ix.shrinkTo(j)

      inline def whereFrom(indices: Array[Long])(inline pick: A => Boolean)(using o: Order): Array[Long] =
        var ix = new Array[Long](if indices.length > 8 then 8 else indices.length)
        var i = 0
        var j = 0
        while i < indices.length do
          val k = indices(i)
          if pick(m(k)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = k
            j += 1
          i += 1
        ix.shrinkTo(j)
      inline def whereFromOp(indices: Array[Long])(inline pick: (A, Long) => Long)(using o: Order): Array[Long] =
        var ix = new Array[Long](if indices.length > 8 then 8 else indices.length)
        var i = 0
        var j = 0
        while i < indices.length do
          val k = indices(i)
          val h = pick(m(k), k)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
            j += 1
          i += 1
        ix.shrinkTo(j)

      inline def whereFwd(i: Long)(inline f: A => Boolean)(using o: Order): Long =
        if i < 0 then -1L
        else boundary[Long]:
          var j = i
          val n = m.length
          while j < n do
            if f(m(j)) then boundary.break(j)
            j += 1
          -1L
      inline def whereBkw(i: Long)(inline f: A => Boolean)(using o: Order): Long =
        if i >= m.length then -1
        else boundary[Long]:
          var j = i
          while j >= 0 do
            if f(m(j)) then boundary.break(j)
            j -= 1
          -1L

      /** The first index in `[i0, iN)` (clamped) holding exactly `value` in the given order, or -1
        * if there is none: the sought bits are laid out per the order once, then the native lane
        * scan runs at full speed.  Matching is by bit pattern, as for `Mem`'s `whereIsFwd`.
        */
      inline def whereIsFwd(i0: Long, iN: Long)(value: A)(using o: Order): Long =
        Mem.whereIsFwd[A](Mem.wrap[A](m))(i0, iN)(seekBits[A](o, value))

      /** The last index in `[i0, iN)` (clamped) holding exactly `value` in the given order, or -1
        * if there is none; matching as for `whereIsFwd`.
        */
      inline def whereIsBkw(i0: Long, iN: Long)(value: A)(using o: Order): Long =
        Mem.whereIsBkw[A](Mem.wrap[A](m))(i0, iN)(seekBits[A](o, value))

      /** Visit maximal runs delimited where `cut(prev, next)` holds, passing each run's [i, j). */
      inline def visitCuts()(inline cut: (A, A) => Boolean)(inline f: (Long, Long) => Unit)(using o: Order): Unit =
        visitCuts(0L, m.length)(cut)(f)
      inline def visitCuts(i0: Long, iN: Long)(inline cut: (A, A) => Boolean)(inline f: (Long, Long) => Unit)(using o: Order): Unit =
        var i = i0
        while i < iN do
          var x = m(i)
          var j = i + 1
          var continue = true
          while continue && j < iN do
            val y = m(j)
            if cut(x, y) then continue = false
            else
              x = y
              j += 1
          f(i, j)
          i = j

      /** Zero-copy reinterpretation as another primitive (any trailing partial element is ignored by `length`). */
      inline def as[B <: Type]: OrderAware[B] = wrap[B](m)

      /** Zero-copy view of elements `[i0, iN)`, sharing this memory and its lifetime. */
      inline def view(i0: Long, iN: Long): OrderAware[A] =
        wrap[A]((m: MemorySegment).asSlice(i0 * bytesOf[A], (iN - i0) * bytesOf[A]))

      /** Zero-copy view of elements `[i0, iN)` (indices in units of `A`) reinterpreted as another primitive. */
      inline def viewAs[B <: Type](i0: Long, iN: Long): OrderAware[B] =
        wrap[B]((m: MemorySegment).asSlice(i0 * bytesOf[A], (iN - i0) * bytesOf[A]))
    }
  }

  // === Mem.As: element types translucently backed by a primitive ===

  /** Array-like memory access where the element type `O` is opaquely backed by a primitive.
    *
    * `Mem` itself admits only raw primitives--it is unambiguously plain data.  `As[O]` is
    * the parallel surface for any `O` that reduces to a single non-Boolean primitive through
    * a chain of `Translucent` witnesses (a raw primitive itself also qualifies).  The
    * reduction is entirely compile-time: witnesses are pattern-matched by `summonFrom` but
    * never materialized or passed at runtime, and each chain link is an erased cast, so every
    * operation compiles to exactly the code of its `Mem` counterpart.  As with `Mem`, only
    * `alloc` allocates a segment; the caller owns all lifetimes.
    */
  opaque type As[O] = MemorySegment
  object As {
    /** Wrap a caller-owned segment.  The caller retains responsibility for its lifetime. */
    inline def wrap[O](seg: MemorySegment): As[O] = seg

    /** Compile-time size in bytes of the primitive that eventually backs `O`. */
    inline def bytesOf[O]: Long = summonFrom:
      case _: Translucent[O, b] => bytesOf[b]
      case _ => inline erasedValue[O] match
        case _: Byte    => 1L
        case _: Short   => 2L
        case _: Char    => 2L
        case _: Int     => 4L
        case _: Float   => 4L
        case _: Long    => 8L
        case _: Double  => 8L
        case _          => error("Mem.As elements must be a non-Boolean primitive or Translucent-reducible to one")

    /** Compile-time (unaligned) value layout of the primitive that eventually backs `O`. */
    inline def layoutOf[O]: ValueLayout = summonFrom:
      case _: Translucent[O, b] => layoutOf[b]
      case _ => inline erasedValue[O] match
        case _: Byte    => JAVA_BYTE
        case _: Short   => JAVA_SHORT_UNALIGNED
        case _: Char    => JAVA_CHAR_UNALIGNED
        case _: Int     => JAVA_INT_UNALIGNED
        case _: Float   => JAVA_FLOAT_UNALIGNED
        case _: Long    => JAVA_LONG_UNALIGNED
        case _: Double  => JAVA_DOUBLE_UNALIGNED
        case _          => error("Mem.As elements must be a non-Boolean primitive or Translucent-reducible to one")

    /** Read element `i` of `seg` as the primitive that eventually backs `O`, typed as `O`. */
    inline def read[O](seg: MemorySegment, i: Long): O = summonFrom:
      case _: Translucent[O, b] => read[b](seg, i).asInstanceOf[O]
      case _ => inline erasedValue[O] match
        case _: Byte    => seg.get(JAVA_BYTE, i).asInstanceOf[O]
        case _: Short   => seg.getAtIndex(JAVA_SHORT_UNALIGNED, i).asInstanceOf[O]
        case _: Char    => seg.getAtIndex(JAVA_CHAR_UNALIGNED, i).asInstanceOf[O]
        case _: Int     => seg.getAtIndex(JAVA_INT_UNALIGNED, i).asInstanceOf[O]
        case _: Float   => seg.getAtIndex(JAVA_FLOAT_UNALIGNED, i).asInstanceOf[O]
        case _: Long    => seg.getAtIndex(JAVA_LONG_UNALIGNED, i).asInstanceOf[O]
        case _: Double  => seg.getAtIndex(JAVA_DOUBLE_UNALIGNED, i).asInstanceOf[O]
        case _          => error("Mem.As elements must be a non-Boolean primitive or Translucent-reducible to one")

    /** Write `x` at element `i` of `seg` as the primitive that eventually backs `O`. */
    inline def write[O](seg: MemorySegment, i: Long, x: O): Unit = summonFrom:
      case _: Translucent[O, b] => write[b](seg, i, x.asInstanceOf[b])
      case _ => inline erasedValue[O] match
        case _: Byte    => seg.set(JAVA_BYTE, i, x.asInstanceOf[Byte])
        case _: Short   => seg.setAtIndex(JAVA_SHORT_UNALIGNED, i, x.asInstanceOf[Short])
        case _: Char    => seg.setAtIndex(JAVA_CHAR_UNALIGNED, i, x.asInstanceOf[Char])
        case _: Int     => seg.setAtIndex(JAVA_INT_UNALIGNED, i, x.asInstanceOf[Int])
        case _: Float   => seg.setAtIndex(JAVA_FLOAT_UNALIGNED, i, x.asInstanceOf[Float])
        case _: Long    => seg.setAtIndex(JAVA_LONG_UNALIGNED, i, x.asInstanceOf[Long])
        case _: Double  => seg.setAtIndex(JAVA_DOUBLE_UNALIGNED, i, x.asInstanceOf[Double])
        case _          => error("Mem.As elements must be a non-Boolean primitive or Translucent-reducible to one")

    /** Read a value of (translucently primitive) type `O` at byte offset `off` of `seg` (unaligned, native order). */
    inline def readAt[O](seg: MemorySegment, off: Long): O = summonFrom:
      case _: Translucent[O, b] => readAt[b](seg, off).asInstanceOf[O]
      case _ => inline erasedValue[O] match
        case _: Byte    => seg.get(JAVA_BYTE, off).asInstanceOf[O]
        case _: Short   => seg.get(JAVA_SHORT_UNALIGNED, off).asInstanceOf[O]
        case _: Char    => seg.get(JAVA_CHAR_UNALIGNED, off).asInstanceOf[O]
        case _: Int     => seg.get(JAVA_INT_UNALIGNED, off).asInstanceOf[O]
        case _: Float   => seg.get(JAVA_FLOAT_UNALIGNED, off).asInstanceOf[O]
        case _: Long    => seg.get(JAVA_LONG_UNALIGNED, off).asInstanceOf[O]
        case _: Double  => seg.get(JAVA_DOUBLE_UNALIGNED, off).asInstanceOf[O]
        case _          => error("Mem.As elements must be a non-Boolean primitive or Translucent-reducible to one")

    /** Write a value of (translucently primitive) type `O` at byte offset `off` of `seg` (unaligned, native order). */
    inline def writeAt[O](seg: MemorySegment, off: Long, x: O): Unit = summonFrom:
      case _: Translucent[O, b] => writeAt[b](seg, off, x.asInstanceOf[b])
      case _ => inline erasedValue[O] match
        case _: Byte    => seg.set(JAVA_BYTE, off, x.asInstanceOf[Byte])
        case _: Short   => seg.set(JAVA_SHORT_UNALIGNED, off, x.asInstanceOf[Short])
        case _: Char    => seg.set(JAVA_CHAR_UNALIGNED, off, x.asInstanceOf[Char])
        case _: Int     => seg.set(JAVA_INT_UNALIGNED, off, x.asInstanceOf[Int])
        case _: Float   => seg.set(JAVA_FLOAT_UNALIGNED, off, x.asInstanceOf[Float])
        case _: Long    => seg.set(JAVA_LONG_UNALIGNED, off, x.asInstanceOf[Long])
        case _: Double  => seg.set(JAVA_DOUBLE_UNALIGNED, off, x.asInstanceOf[Double])
        case _          => error("Mem.As elements must be a non-Boolean primitive or Translucent-reducible to one")

    /** Forward value seek dispatched to the primitive that backs `O`; see `whereIsFwd`. */
    inline def seekFwd[O](seg: MemorySegment, i0: Long, iN: Long, v: O): Long = summonFrom:
      case _: Translucent[O, b] => seekFwd[b](seg, i0, iN, v.asInstanceOf[b])
      case _ => inline erasedValue[O] match
        case _: Byte   => seekIsByte(seg, i0, iN, v.asInstanceOf[Byte])
        case _: Short  => seekIsShort(seg, i0, iN, v.asInstanceOf[Short])
        case _: Char   => seekIsShort(seg, i0, iN, v.asInstanceOf[Char].toShort)
        case _: Int    => seekIsInt(seg, i0, iN, v.asInstanceOf[Int])
        case _: Long   => seekIsLong(seg, i0, iN, v.asInstanceOf[Long])
        case _: Float  => seekIsInt(seg, i0, iN, java.lang.Float.floatToRawIntBits(v.asInstanceOf[Float]))
        case _: Double => seekIsLong(seg, i0, iN, java.lang.Double.doubleToRawLongBits(v.asInstanceOf[Double]))
        case _         => error("Mem.As elements must be a non-Boolean primitive or Translucent-reducible to one")

    /** Backward value seek dispatched to the primitive that backs `O`; see `whereIsBkw`. */
    inline def seekBkw[O](seg: MemorySegment, i0: Long, iN: Long, v: O): Long = summonFrom:
      case _: Translucent[O, b] => seekBkw[b](seg, i0, iN, v.asInstanceOf[b])
      case _ => inline erasedValue[O] match
        case _: Byte   => seekIsByteBkw(seg, i0, iN, v.asInstanceOf[Byte])
        case _: Short  => seekIsShortBkw(seg, i0, iN, v.asInstanceOf[Short])
        case _: Char   => seekIsShortBkw(seg, i0, iN, v.asInstanceOf[Char].toShort)
        case _: Int    => seekIsIntBkw(seg, i0, iN, v.asInstanceOf[Int])
        case _: Long   => seekIsLongBkw(seg, i0, iN, v.asInstanceOf[Long])
        case _: Float  => seekIsIntBkw(seg, i0, iN, java.lang.Float.floatToRawIntBits(v.asInstanceOf[Float]))
        case _: Double => seekIsLongBkw(seg, i0, iN, java.lang.Double.doubleToRawLongBits(v.asInstanceOf[Double]))
        case _         => error("Mem.As elements must be a non-Boolean primitive or Translucent-reducible to one")

    /** The memory segment sharing storage with an array whose elements are translucently primitive. */
    inline def arraySegment[O](xs: Array[O]): MemorySegment = summonFrom:
      case _: Translucent[O, b] => arraySegment[b](xs.asInstanceOf[Array[b]])
      case _ => inline erasedValue[O] match
        case _: Byte    => MemorySegment.ofArray(xs.asInstanceOf[Array[Byte]])
        case _: Short   => MemorySegment.ofArray(xs.asInstanceOf[Array[Short]])
        case _: Char    => MemorySegment.ofArray(xs.asInstanceOf[Array[Char]])
        case _: Int     => MemorySegment.ofArray(xs.asInstanceOf[Array[Int]])
        case _: Float   => MemorySegment.ofArray(xs.asInstanceOf[Array[Float]])
        case _: Long    => MemorySegment.ofArray(xs.asInstanceOf[Array[Long]])
        case _: Double  => MemorySegment.ofArray(xs.asInstanceOf[Array[Double]])
        case _          => error("Mem.As elements must be a non-Boolean primitive or Translucent-reducible to one")

    /** Any primitive-typed `Mem`; the upper bound for `prim`'s computed type. */
    type AnyMem = Mem[Byte] | Mem[Short] | Mem[Char] | Mem[Int] | Mem[Float] | Mem[Long] | Mem[Double]

    /** The same segment as a `Mem` of the primitive that eventually backs `O`; the precise type is computed at inline time. */
    transparent inline def primOf[O](seg: MemorySegment): AnyMem = summonFrom:
      case _: Translucent[O, b] => primOf[b](seg)
      case _ => inline erasedValue[O] match
        case _: Byte    => Mem.wrap[Byte](seg)
        case _: Short   => Mem.wrap[Short](seg)
        case _: Char    => Mem.wrap[Char](seg)
        case _: Int     => Mem.wrap[Int](seg)
        case _: Float   => Mem.wrap[Float](seg)
        case _: Long    => Mem.wrap[Long](seg)
        case _: Double  => Mem.wrap[Double](seg)
        case _          => error("Mem.As elements must be a non-Boolean primitive or Translucent-reducible to one")

    /** Allocate `n` elements of off-heap memory, reclaimed by the GC when unreachable. */
    inline def alloc[O](n: Long): As[O] =
      wrap[O](Arena.ofAuto().allocate(n * bytesOf[O]))

    /** Wrap an array of translucently-primitive elements as an `As` (shares the array's storage). */
    infix inline def of[O](xs: Array[O]): As[O] = wrap[O](arraySegment[O](xs))

    extension [O](m: As[O]) {
      /** The underlying segment. */
      inline def segment: MemorySegment = m

      /** This memory as a `Mem` of the primitive that eventually backs `O` (zero-cost; precise type computed). */
      transparent inline def prim: AnyMem = primOf[O](m)

      /** Number of elements = floor(byteSize / elementBytes). */
      inline def length: Long = (m: MemorySegment).byteSize / bytesOf[O]

      inline def apply(i: Long): O = read[O]((m: MemorySegment), i)

      inline def update(i: Long, x: O): Unit = write[O]((m: MemorySegment), i, x)

      /** Read a primitive of the stated type at element index `i`: the byte offset is `i * bytesOf[O]`,
        * so indices stay in units of `O`'s backing primitive no matter which type is read (unaligned, native byte order).
        */
      inline def getB(i: Long): Byte   = (m: MemorySegment).get(JAVA_BYTE,             i * bytesOf[O])
      inline def getS(i: Long): Short  = (m: MemorySegment).get(JAVA_SHORT_UNALIGNED,  i * bytesOf[O])
      inline def getC(i: Long): Char   = (m: MemorySegment).get(JAVA_CHAR_UNALIGNED,   i * bytesOf[O])
      inline def getI(i: Long): Int    = (m: MemorySegment).get(JAVA_INT_UNALIGNED,    i * bytesOf[O])
      inline def getF(i: Long): Float  = (m: MemorySegment).get(JAVA_FLOAT_UNALIGNED,  i * bytesOf[O])
      inline def getL(i: Long): Long   = (m: MemorySegment).get(JAVA_LONG_UNALIGNED,   i * bytesOf[O])
      inline def getD(i: Long): Double = (m: MemorySegment).get(JAVA_DOUBLE_UNALIGNED, i * bytesOf[O])

      /** Write a primitive of the stated type at element index `i`: the byte offset is `i * bytesOf[O]`,
        * so indices stay in units of `O`'s backing primitive no matter which type is written (unaligned, native byte order).
        */
      inline def setB(i: Long, x: Byte):   Unit = (m: MemorySegment).set(JAVA_BYTE,             i * bytesOf[O], x)
      inline def setS(i: Long, x: Short):  Unit = (m: MemorySegment).set(JAVA_SHORT_UNALIGNED,  i * bytesOf[O], x)
      inline def setC(i: Long, x: Char):   Unit = (m: MemorySegment).set(JAVA_CHAR_UNALIGNED,   i * bytesOf[O], x)
      inline def setI(i: Long, x: Int):    Unit = (m: MemorySegment).set(JAVA_INT_UNALIGNED,    i * bytesOf[O], x)
      inline def setF(i: Long, x: Float):  Unit = (m: MemorySegment).set(JAVA_FLOAT_UNALIGNED,  i * bytesOf[O], x)
      inline def setL(i: Long, x: Long):   Unit = (m: MemorySegment).set(JAVA_LONG_UNALIGNED,   i * bytesOf[O], x)
      inline def setD(i: Long, x: Double): Unit = (m: MemorySegment).set(JAVA_DOUBLE_UNALIGNED, i * bytesOf[O], x)

      /** A bounds-clipping view: out-of-range indices are silently skipped or clamped. */
      inline def clip: kse.basics.ClippedMem.As[O] = ClippedMem.As.wrap(m)

      inline def use()(inline f: O => Unit): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          f(m(i))
          i += 1
      inline def use(i0: Long, iN: Long)(inline f: O => Unit): Unit =
        var i = i0
        while i < iN do
          f(m(i))
          i += 1
      inline def use(indices: Array[Long])(inline f: O => Unit): Unit =
        var i = 0
        while i < indices.length do
          f(m(indices(i)))
          i += 1
      inline def use(indices: LongStepper)(inline f: O => Unit): Unit =
        while indices.hasStep do
          f(m(indices.nextStep()))
      inline def use(inline p: O => Boolean)(inline f: O => Unit): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          val x = m(i)
          if p(x) then f(x)
          i += 1

      inline def alter()(inline f: O => O): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          m(i) = f(m(i))
          i += 1
      inline def alter(i0: Long, iN: Long)(inline f: O => O): Unit =
        var i = i0
        while i < iN do
          m(i) = f(m(i))
          i += 1
      inline def alter(indices: Array[Long])(inline f: O => O): Unit =
        var i = 0
        while i < indices.length do
          val j = indices(i)
          m(j) = f(m(j))
          i += 1
      inline def alter(indices: LongStepper)(inline f: O => O): Unit =
        while indices.hasStep do
          val j = indices.nextStep()
          m(j) = f(m(j))
      inline def alter(inline p: O => Boolean)(inline f: O => O): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          val x = m(i)
          if p(x) then m(i) = f(x)
          i += 1

      inline def visit()(inline f: (O, Long) => Unit): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          f(m(i), i)
          i += 1
      inline def visit(i0: Long, iN: Long)(inline f: (O, Long) => Unit): Unit =
        var i = i0
        while i < iN do
          f(m(i), i)
          i += 1
      inline def visit(indices: Array[Long])(inline f: (O, Long) => Unit): Unit =
        var i = 0
        while i < indices.length do
          val j = indices(i)
          f(m(j), j)
          i += 1
      inline def visit(indices: LongStepper)(inline f: (O, Long) => Unit): Unit =
        while indices.hasStep do
          val j = indices.nextStep()
          f(m(j), j)
      inline def visit(inline p: O => Boolean)(inline f: (O, Long) => Unit): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          val x = m(i)
          if p(x) then f(x, i)
          i += 1

      inline def edit()(inline f: (O, Long) => O): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          m(i) = f(m(i), i)
          i += 1
      inline def edit(i0: Long, iN: Long)(inline f: (O, Long) => O): Unit =
        var i = i0
        while i < iN do
          m(i) = f(m(i), i)
          i += 1
      inline def edit(indices: Array[Long])(inline f: (O, Long) => O): Unit =
        var i = 0
        while i < indices.length do
          val j = indices(i)
          m(j) = f(m(j), j)
          i += 1
      inline def edit(indices: LongStepper)(inline f: (O, Long) => O): Unit =
        while indices.hasStep do
          val j = indices.nextStep()
          m(j) = f(m(j), j)
      inline def edit(inline p: O => Boolean)(inline f: (O, Long) => O): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          val x = m(i)
          if p(x) then m(i) = f(x, i)
          i += 1

      /** Visit each adjacent pair (x(i), x(i+1)). */
      inline def pairs(inline f: (O, O) => Unit): Unit =
        val n = m.length
        if n > 0 then
          var a0 = m(0)
          var i = 1L
          while i < n do
            val a1 = m(i)
            f(a0, a1)
            a0 = a1
            i += 1
      /** Visit each adjacent triple (x(i), x(i+1), x(i+2)). */
      inline def trios(inline f: (O, O, O) => Unit): Unit =
        val n = m.length
        if n > 1 then
          var a0 = m(0)
          var a1 = m(1)
          var i = 2L
          while i < n do
            val a2 = m(i)
            f(a0, a1, a2)
            a0 = a1
            a1 = a2
            i += 1

      /** Visit elements of this and `b` in lockstep, up to the shorter length. */
      inline def together[B](b: As[B])(inline f: (O, B, Long) => Unit): Unit =
        val nb = b.length
        var n = m.length
        if nb < n then n = nb
        var i = 0L
        while i < n do
          f(m(i), b(i), i)
          i += 1
      inline def together[B, C](b: As[B], c: As[C])(inline f: (O, B, C, Long) => Unit): Unit =
        var n = m.length
        val nb = b.length
        val nc = c.length
        if nb < n then n = nb
        if nc < n then n = nc
        var i = 0L
        while i < n do
          f(m(i), b(i), c(i), i)
          i += 1

      /** Follow indices produced by `f` until one falls out of range; returns the number of steps. */
      inline def wander()(inline f: (O, Long) => Long): Long =
        wander(0L)(f)
      inline def wander(start: Long)(inline f: (O, Long) => Long): Long =
        var n = 0L
        var i = start
        val len = m.length
        while i >= 0 && i < len && n < Long.MaxValue do
          n += 1
          i = f(m(i), i)
        n

      inline def gather[Z](zero: Z)()(inline f: (Z, O, Long) => Z): Z =
        var i = 0L
        val n = m.length
        var z = zero
        while i < n do
          z = f(z, m(i), i)
          i += 1
        z
      inline def gather[Z](zero: Z)(i0: Long, iN: Long)(inline f: (Z, O, Long) => Z): Z =
        var i = i0
        var z = zero
        while i < iN do
          z = f(z, m(i), i)
          i += 1
        z
      inline def gather[Z](zero: Z)(indices: Array[Long])(inline f: (Z, O, Long) => Z): Z =
        var i = 0
        var z = zero
        while i < indices.length do
          val j = indices(i)
          z = f(z, m(j), j)
          i += 1
        z
      inline def gather[Z](zero: Z)(indices: LongStepper)(inline f: (Z, O, Long) => Z): Z =
        var z = zero
        while indices.hasStep do
          val j = indices.nextStep()
          z = f(z, m(j), j)
        z
      inline def gather[Z](zero: Z)(inline p: O => Boolean)(inline f: (Z, O, Long) => Z): Z =
        var i = 0L
        val n = m.length
        var z = zero
        while i < n do
          val x = m(i)
          if p(x) then z = f(z, x, i)
          i += 1
        z

      @targetName("update_All_constant")
      inline def update(value: O): Unit =
        update(0L, m.length, value)
      @targetName("update_All_segment")
      inline def update(values: As[O]): Unit =
        update(0L, m.length, values)

      @targetName("update_i0iN_constant")
      inline def update(i0: Long, iN: Long, value: O): Unit =
        var i = i0
        while i < iN do
          m(i) = value
          i += 1
      @targetName("update_i0iN_segment")
      inline def update(i0: Long, iN: Long, values: As[O]): Unit =
        MemorySegment.copy(values, 0L, m, i0 * bytesOf[O], (iN - i0) * bytesOf[O])

      @targetName("update_Places_constant")
      inline def update(indices: Array[Long], value: O): Unit =
        var i = 0
        while i < indices.length do
          m(indices(i)) = value
          i += 1
      @targetName("update_Places_segment")
      inline def update(indices: Array[Long], values: As[O]): Unit =
        var i = 0
        while i < indices.length do
          m(indices(i)) = values(i)
          i += 1

      @targetName("update_Stepper_constant")
      inline def update(indices: LongStepper, value: O): Unit =
        while indices.hasStep do
          m(indices.nextStep()) = value
      @targetName("update_Stepper_segment")
      inline def update(indices: LongStepper, values: As[O]): Unit =
        var i = 0L
        while indices.hasStep do
          m(indices.nextStep()) = values(i)
          i += 1

      @targetName("update_Selector")
      inline def update(inline pick: O => Boolean, value: O): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          if pick(m(i)) then m(i) = value
          i += 1

      @targetName("set_All_generate")
      inline def set()(inline generator: () => O): Unit =
        set(0L, m.length)(generator)
      @targetName("set_All_index")
      inline def set()(inline indexer: Long => O): Unit =
        set(0L, m.length)(indexer)

      @targetName("set_i0iN_generate")
      inline def set(i0: Long, iN: Long)(inline generator: () => O): Unit =
        var i = i0
        while i < iN do
          m(i) = generator()
          i += 1
      @targetName("set_i0iN_index")
      inline def set(i0: Long, iN: Long)(inline indexer: Long => O): Unit =
        var i = i0
        while i < iN do
          m(i) = indexer(i)
          i += 1

      @targetName("set_Places_generate")
      inline def set(indices: Array[Long])(inline generator: () => O): Unit =
        var i = 0
        while i < indices.length do
          m(indices(i)) = generator()
          i += 1
      @targetName("set_Places_index")
      inline def set(indices: Array[Long])(inline indexer: Long => O): Unit =
        var i = 0
        while i < indices.length do
          val j = indices(i)
          m(j) = indexer(j)
          i += 1

      @targetName("set_Stepper_generate")
      inline def set(indices: LongStepper)(inline generator: () => O): Unit =
        while indices.hasStep do
          m(indices.nextStep()) = generator()
      @targetName("set_Stepper_index")
      inline def set(indices: LongStepper)(inline indexer: Long => O): Unit =
        while indices.hasStep do
          val j = indices.nextStep()
          m(j) = indexer(j)

      @targetName("set_Selector_generate")
      inline def set(inline pick: O => Boolean)(inline generator: () => O): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          if pick(m(i)) then m(i) = generator()
          i += 1
      @targetName("set_Selector_index")
      inline def set(inline pick: O => Boolean)(inline indexer: Long => O): Unit =
        var i = 0L
        val n = m.length
        while i < n do
          if pick(m(i)) then m(i) = indexer(i)
          i += 1

      /** All indices, 0 until length. */
      inline def where(): Array[Long] =
        val ix = new Array[Long](m.length.toInt)
        var i = 0
        while i < ix.length do
          ix(i) = i.toLong
          i += 1
        ix
      inline def where(inline pick: O => Boolean): Array[Long] =
        whereIn(0L, m.length)(pick)
      inline def whereOp(inline pick: (O, Long) => Long): Array[Long] =
        whereInOp(0L, m.length)(pick)

      inline def whereIn(i0: Long, iN: Long)(inline pick: O => Boolean): Array[Long] =
        var ix = new Array[Long](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else (iN - i0).toInt)
        var i = i0
        var j = 0
        while i < iN do
          if pick(m(i)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = i
            j += 1
          i += 1
        ix.shrinkTo(j)
      inline def whereInOp(i0: Long, iN: Long)(inline pick: (O, Long) => Long): Array[Long] =
        var ix = new Array[Long](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else (iN - i0).toInt)
        var i = i0
        var j = 0
        while i < iN do
          val h = pick(m(i), i)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
            j += 1
          i += 1
        ix.shrinkTo(j)

      inline def whereFrom(indices: Array[Long])(inline pick: O => Boolean): Array[Long] =
        var ix = new Array[Long](if indices.length > 8 then 8 else indices.length)
        var i = 0
        var j = 0
        while i < indices.length do
          val k = indices(i)
          if pick(m(k)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = k
            j += 1
          i += 1
        ix.shrinkTo(j)
      inline def whereFromOp(indices: Array[Long])(inline pick: (O, Long) => Long): Array[Long] =
        var ix = new Array[Long](if indices.length > 8 then 8 else indices.length)
        var i = 0
        var j = 0
        while i < indices.length do
          val k = indices(i)
          val h = pick(m(k), k)
          if h >= 0 then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = h
            j += 1
          i += 1
        ix.shrinkTo(j)

      inline def whereFwd(i: Long)(inline f: O => Boolean): Long =
        if i < 0 then -1L
        else boundary[Long]:
          var j = i
          val n = m.length
          while j < n do
            if f(m(j)) then boundary.break(j)
            j += 1
          -1L
      inline def whereBkw(i: Long)(inline f: O => Boolean): Long =
        if i >= m.length then -1
        else boundary[Long]:
          var j = i
          while j >= 0 do
            if f(m(j)) then boundary.break(j)
            j -= 1
          -1L

      /** The first index in `[i0, iN)` (clamped) holding exactly `value`, or -1 if there is none;
        * matching on the backing primitive exactly as for `Mem.whereIsFwd` (floating types by raw bits).
        */
      inline def whereIsFwd(i0: Long, iN: Long)(value: O): Long =
        seekFwd[O]((m: MemorySegment), i0, iN, value)

      /** The last index in `[i0, iN)` (clamped) holding exactly `value`, or -1 if there is none;
        * matching as for `whereIsFwd`.
        */
      inline def whereIsBkw(i0: Long, iN: Long)(value: O): Long =
        seekBkw[O]((m: MemorySegment), i0, iN, value)

      /** Copy elements into a caller-provided destination; returns the number copied. */
      inline def inject(that: As[O]): Long =
        inject(that, 0L)(0L, m.length)
      inline def inject(that: As[O], where: Long): Long =
        inject(that, where)(0L, m.length)
      inline def inject(that: As[O])(i0: Long, iN: Long): Long =
        inject(that, 0L)(i0, iN)
      inline def inject(that: As[O], where: Long)(i0: Long, iN: Long): Long =
        val eb = bytesOf[O]
        MemorySegment.copy(m, i0 * eb, that, where * eb, (iN - i0) * eb)
        iN - i0
      inline def inject(that: As[O])(indices: Array[Long]): Long =
        inject(that, 0L)(indices)
      inline def inject(that: As[O], where: Long)(indices: Array[Long]): Long =
        var i = 0
        var j = where
        while i < indices.length do
          that(j) = m(indices(i))
          i += 1
          j += 1
        i
      inline def inject(that: As[O])(indices: LongStepper): Long =
        inject(that, 0L)(indices)
      inline def inject(that: As[O], where: Long)(indices: LongStepper): Long =
        var j = where
        while indices.hasStep do
          that(j) = m(indices.nextStep())
          j += 1
        j - where
      inline def inject(that: As[O])(inline pick: O => Boolean): Long =
        inject(that, 0L)(pick)
      inline def inject(that: As[O], where: Long)(inline pick: O => Boolean): Long =
        var i = 0L
        val n = m.length
        var j = where
        while i < n do
          val x = m(i)
          if pick(x) then
            that(j) = x
            j += 1
          i += 1
        j - where

      /** Copy elements into a caller-provided array; returns the number copied. */
      inline def inject(that: Array[O]): Long =
        inject(that, 0)(0L, m.length)
      inline def inject(that: Array[O], where: Int): Long =
        inject(that, where)(0L, m.length)
      inline def inject(that: Array[O])(i0: Long, iN: Long): Long =
        inject(that, 0)(i0, iN)
      inline def inject(that: Array[O], where: Int)(i0: Long, iN: Long): Long =
        MemorySegment.copy(m, layoutOf[O], i0 * bytesOf[O], that, where, (iN - i0).toInt)
        iN - i0
      inline def inject(that: Array[O])(indices: Array[Long]): Long =
        inject(that, 0)(indices)
      inline def inject(that: Array[O], where: Int)(indices: Array[Long]): Long =
        var i = 0
        var j = where
        while i < indices.length do
          that(j) = m(indices(i))
          i += 1
          j += 1
        i
      inline def inject(that: Array[O])(indices: LongStepper): Long =
        inject(that, 0)(indices)
      inline def inject(that: Array[O], where: Int)(indices: LongStepper): Long =
        var j = where
        while indices.hasStep do
          that(j) = m(indices.nextStep())
          j += 1
        j - where
      inline def inject(that: Array[O])(inline pick: O => Boolean): Long =
        inject(that, 0)(pick)
      inline def inject(that: Array[O], where: Int)(inline pick: O => Boolean): Long =
        var i = 0L
        val n = m.length
        var j = where
        while i < n do
          val x = m(i)
          if pick(x) then
            that(j) = x
            j += 1
          i += 1
        j - where

      /** Map elements into a caller-provided destination; returns the number written. */
      inline def injectOp[B](that: As[B])()(inline f: (O, Long) => B): Long =
        injectOp(that, 0L)(0L, m.length)(f)
      inline def injectOp[B](that: As[B], where: Long)()(inline f: (O, Long) => B): Long =
        injectOp(that, where)(0L, m.length)(f)
      inline def injectOp[B](that: As[B])(i0: Long, iN: Long)(inline f: (O, Long) => B): Long =
        injectOp(that, 0L)(i0, iN)(f)
      inline def injectOp[B](that: As[B], where: Long)(i0: Long, iN: Long)(inline f: (O, Long) => B): Long =
        var i = i0
        var j = where
        while i < iN do
          that(j) = f(m(i), i)
          j += 1
          i += 1
        iN - i0
      inline def injectOp[B](that: As[B])(indices: Array[Long])(inline f: (O, Long) => B): Long =
        injectOp(that, 0L)(indices)(f)
      inline def injectOp[B](that: As[B], where: Long)(indices: Array[Long])(inline f: (O, Long) => B): Long =
        var i = 0
        var j = where
        while i < indices.length do
          val k = indices(i)
          that(j) = f(m(k), k)
          i += 1
          j += 1
        i
      inline def injectOp[B](that: As[B])(indices: LongStepper)(inline f: (O, Long) => B): Long =
        injectOp(that, 0L)(indices)(f)
      inline def injectOp[B](that: As[B], where: Long)(indices: LongStepper)(inline f: (O, Long) => B): Long =
        var j = where
        while indices.hasStep do
          val i = indices.nextStep()
          that(j) = f(m(i), i)
          j += 1
        j - where
      inline def injectOp[B](that: As[B])(inline pick: O => Boolean)(inline f: (O, Long) => B): Long =
        injectOp(that, 0L)(pick)(f)
      inline def injectOp[B](that: As[B], where: Long)(inline pick: O => Boolean)(inline f: (O, Long) => B): Long =
        var i = 0L
        val n = m.length
        var j = where
        while i < n do
          val x = m(i)
          if pick(x) then
            that(j) = f(x, i)
            j += 1
          i += 1
        j - where

      /** Visit maximal runs delimited where `cut(prev, next)` holds, passing each run's [i, j). */
      inline def visitCuts()(inline cut: (O, O) => Boolean)(inline f: (Long, Long) => Unit): Unit =
        visitCuts(0L, m.length)(cut)(f)
      inline def visitCuts(i0: Long, iN: Long)(inline cut: (O, O) => Boolean)(inline f: (Long, Long) => Unit): Unit =
        var i = i0
        while i < iN do
          var x = m(i)
          var j = i + 1
          var continue = true
          while continue && j < iN do
            val y = m(j)
            if cut(x, y) then continue = false
            else
              x = y
              j += 1
          f(i, j)
          i = j

      /** Zero-copy reinterpretation with another element type (validity is checked where `B`'s layout is used). */
      inline def as[B]: As[B] = wrap[B](m)

      /** Zero-copy view of elements `[i0, iN)`, sharing this memory and its lifetime. */
      inline def view(i0: Long, iN: Long): As[O] =
        wrap[O]((m: MemorySegment).asSlice(i0 * bytesOf[O], (iN - i0) * bytesOf[O]))

      /** Zero-copy view of elements `[i0, iN)` (indices in units of `O`'s primitive) reinterpreted as an `As[B]`. */
      inline def viewAs[B](i0: Long, iN: Long): As[B] =
        wrap[B]((m: MemorySegment).asSlice(i0 * bytesOf[O], (iN - i0) * bytesOf[O]))

      /** Zero-copy reinterpretation as an array of structs (any trailing partial struct is ignored by its `length`). */
      inline def aos[T <: NamedTuple.AnyNamedTuple]: Mem.AoS[T] = AoS.wrap[T](m)
    }
  }

  // === Mem.AoS: array-of-structs specified by a named tuple ===

  /** Array-of-structs access over caller-owned memory, with the struct given as a named
    * tuple type: `Mem.AoS[(index: Int, x: Double)]`.  Fields are packed in declaration
    * order with no padding (access is unaligned, as everywhere in `Mem`), and each field
    * type must reduce to a non-Boolean primitive via a `Translucent` chain, exactly as in
    * [[Mem.As]].
    *
    * Fields are addressed by name at zero runtime cost via inline `Dynamic` dispatch:
    * `xs.index(5)` reads, `xs.index(5) = 3` writes, and bare `xs.index` is a strided
    * [[AoS.Field]] column view with its own loop ops.  `xs(5)` materializes struct `5`
    * as a named tuple and `xs(5) = tup` writes one back (those two box; per-field access
    * does not).  This is a value class over the segment (an opaque type cannot be
    * `Dynamic`), so the handle erases to the bare `MemorySegment` in straight-line code.
    *
    * Caveat: a field named like a real member (`segment`, `stride`, `length`, `apply`,
    * `update`, `cursor`, `struct`, `use`, `pairs`, `trios`, `gather`, `where`, `whereIn`,
    * `whereFrom`) is shadowed by it; reach such a field via `AoS.Field` or [[Mem.Struct]]
    * directly.
    */
  final class AoS[T <: NamedTuple.AnyNamedTuple](val segment: MemorySegment) extends AnyVal with Dynamic {
    /** Bytes per struct: the packed sum of the field sizes. */
    inline def stride: Long = AoS.strideOf[T]

    /** Number of whole structs = floor(byteSize / stride). */
    inline def length: Long = segment.byteSize / AoS.strideOf[T]

    /** Materialize struct `i` as a named tuple (this allocates the tuple). */
    inline def apply(i: Long): T =
      AoS.readTuple[NamedTuple.DropNames[T]](segment, i * AoS.strideOf[T]).asInstanceOf[T]

    /** Write every field of struct `i` from a named tuple. */
    inline def update(i: Long, v: T): Unit =
      AoS.writeTuple[NamedTuple.DropNames[T]](segment, i * AoS.strideOf[T], v.asInstanceOf[Tuple], 0)

    /** The strided column view of the named field (also serves `xs.name(i) = x` assignment). */
    inline def selectDynamic(name: String & Singleton): AoS.Field[T, name.type] =
      new AoS.Field[T, name.type](segment)

    /** Field `name` of struct `i`, read directly. */
    inline def applyDynamic(name: String & Singleton)(i: Long): AoS.FieldType[T, name.type] =
      As.readAt[AoS.FieldType[T, name.type]](segment, i * AoS.strideOf[T] + AoS.offsetOf[T, name.type])

    /** A one-struct view of this array, placed at struct `i`. */
    inline def cursor(i: Long): Mem.Struct[T] = new Mem.Struct[T](segment, i * AoS.strideOf[T])

    /** A detached copy of struct `i`: an independently allocated one-struct view, safe to retain;
      * reads and writes touch only the copy.  Its memory is reclaimed by the GC when unreachable.
      */
    inline def struct(i: Long): Mem.Struct[T] =
      val st = AoS.strideOf[T]
      val seg = Arena.ofAuto().allocate(st)
      MemorySegment.copy(segment, i * st, seg, 0L, st)
      new Mem.Struct[T](seg, 0L)

    /** Run `f` on an instance-typed index for each complete struct in order.  This array is
      * contextually available inside `f`, so `idx.x` reads and `idx.x = v` writes its fields
      * (which subsumes alter/edit), the plain position is `idx.unwrap`, and neighbors are a
      * matter of index arithmetic: `(idx - 1).x`.  For a keepable reference to one struct,
      * use [[cursor]] or [[struct]] instead.
      */
    inline def use()(inline f: this.type ?=> AoS.Index[T, this.type] => Unit): Unit =
      use(0L, length)(f)
    inline def use(i0: Long, iN: Long)(inline f: this.type ?=> AoS.Index[T, this.type] => Unit): Unit =
      var i = i0
      while i < iN do
        f(using this)(new AoS.Index[T, this.type](i))
        i += 1
    inline def use(indices: Array[Long])(inline f: this.type ?=> AoS.Index[T, this.type] => Unit): Unit =
      var i = 0
      while i < indices.length do
        f(using this)(new AoS.Index[T, this.type](indices(i)))
        i += 1
    inline def use(indices: LongStepper)(inline f: this.type ?=> AoS.Index[T, this.type] => Unit): Unit =
      while indices.hasStep do
        f(using this)(new AoS.Index[T, this.type](indices.nextStep()))

    /** Visit each adjacent pair of structs as indices: `f(a, b)` with `a` on struct i and
      * `b` on struct i+1, this array in context.  Writes land in place, and are seen by
      * later steps.
      */
    inline def pairs(inline f: this.type ?=> (AoS.Index[T, this.type], AoS.Index[T, this.type]) => Unit): Unit =
      val n = length
      var i = 1L
      while i < n do
        f(using this)(new AoS.Index[T, this.type](i - 1), new AoS.Index[T, this.type](i))
        i += 1

    /** Visit each adjacent triple of structs as indices, as `pairs`. */
    inline def trios(inline f: this.type ?=> (AoS.Index[T, this.type], AoS.Index[T, this.type], AoS.Index[T, this.type]) => Unit): Unit =
      val n = length
      var i = 2L
      while i < n do
        f(using this)(new AoS.Index[T, this.type](i - 2), new AoS.Index[T, this.type](i - 1), new AoS.Index[T, this.type](i))
        i += 1

    /** Fold over an instance-typed index for each complete struct, in order. */
    inline def gather[Z](zero: Z)()(inline f: this.type ?=> (Z, AoS.Index[T, this.type]) => Z): Z =
      gather(zero)(0L, length)(f)
    inline def gather[Z](zero: Z)(i0: Long, iN: Long)(inline f: this.type ?=> (Z, AoS.Index[T, this.type]) => Z): Z =
      var i = i0
      var z = zero
      while i < iN do
        z = f(using this)(z, new AoS.Index[T, this.type](i))
        i += 1
      z
    inline def gather[Z](zero: Z)(indices: Array[Long])(inline f: this.type ?=> (Z, AoS.Index[T, this.type]) => Z): Z =
      var i = 0
      var z = zero
      while i < indices.length do
        z = f(using this)(z, new AoS.Index[T, this.type](indices(i)))
        i += 1
      z
    inline def gather[Z](zero: Z)(indices: LongStepper)(inline f: this.type ?=> (Z, AoS.Index[T, this.type]) => Z): Z =
      var z = zero
      while indices.hasStep do
        z = f(using this)(z, new AoS.Index[T, this.type](indices.nextStep()))
      z

    /** The indices of the structs an index predicate picks. */
    inline def where(inline pick: this.type ?=> AoS.Index[T, this.type] => Boolean): Array[Long] =
      whereIn(0L, length)(pick)
    inline def whereIn(i0: Long, iN: Long)(inline pick: this.type ?=> AoS.Index[T, this.type] => Boolean): Array[Long] =
      var ix = new Array[Long](if iN - i0 < 0 then 0 else if iN - i0 > 8 then 8 else (iN - i0).toInt)
      var i = i0
      var j = 0
      while i < iN do
        if pick(using this)(new AoS.Index[T, this.type](i)) then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = i
          j += 1
        i += 1
      ix.shrinkTo(j)
    inline def whereFrom(indices: Array[Long])(inline pick: this.type ?=> AoS.Index[T, this.type] => Boolean): Array[Long] =
      var ix = new Array[Long](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        if pick(using this)(new AoS.Index[T, this.type](k)) then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = k
          j += 1
        i += 1
      ix.shrinkTo(j)
  }
  object AoS {
    /** Wrap a caller-owned segment as an array of structs.  The caller retains responsibility for its lifetime. */
    inline def wrap[T <: NamedTuple.AnyNamedTuple](seg: MemorySegment): AoS[T] = new AoS[T](seg)

    /** Allocate `n` structs of off-heap memory, reclaimed by the GC when unreachable. */
    inline def alloc[T <: NamedTuple.AnyNamedTuple](n: Long): AoS[T] =
      new AoS[T](Arena.ofAuto().allocate(n * strideOf[T]))

    /** Packed byte size of a tuple of (translucently primitive) field types. */
    inline def tupleBytes[Ts <: Tuple]: Long = inline erasedValue[Ts] match
      case _: EmptyTuple => 0L
      case _: (t *: ts)  => As.bytesOf[t] + tupleBytes[ts]

    /** Bytes per struct of `T`: the packed field sizes, no padding. */
    inline def strideOf[T <: NamedTuple.AnyNamedTuple]: Long = inline erasedValue[NamedTuple.DropNames[T]] match
      case _: EmptyTuple => error("AoS structs need at least one field")
      case _             => tupleBytes[NamedTuple.DropNames[T]]

    /** Byte offset of field `N` within the name/value tuples `Ns`/`Ts`; compile error if there is no such field. */
    inline def tupleOffset[Ns <: Tuple, Ts <: Tuple, N <: LabelStr]: Long = inline erasedValue[Ns] match
      case _: (N *: _)  => 0L
      case _: (_ *: ns) => inline erasedValue[Ts] match
        case _: (t *: ts) => As.bytesOf[t] + tupleOffset[ns, ts, N]
      case _: EmptyTuple => error("AoS struct has no field with this name")

    /** Byte offset of field `N` within a struct of `T`. */
    inline def offsetOf[T <: NamedTuple.AnyNamedTuple, N <: LabelStr]: Long =
      tupleOffset[NamedTuple.Names[T], NamedTuple.DropNames[T], N]

    /** The declared type of field `N` in the name/value tuples `Ns`/`Ts`. */
    type TupleField[Ns <: Tuple, Ts <: Tuple, N] = (Ns, Ts) match
      case (N *: _, t *: _)   => t
      case (_ *: ns, _ *: ts) => TupleField[ns, ts, N]

    /** The declared type of field `N` of struct `T`. */
    type FieldType[T <: NamedTuple.AnyNamedTuple, N] = TupleField[NamedTuple.Names[T], NamedTuple.DropNames[T], N]

    /** Read the fields of `Ts`, packed starting at byte offset `off`, into a value tuple. */
    inline def readTuple[Ts <: Tuple](seg: MemorySegment, off: Long): Ts = inline erasedValue[Ts] match
      case _: EmptyTuple => EmptyTuple.asInstanceOf[Ts]
      case _: (t *: ts)  => (As.readAt[t](seg, off) *: readTuple[ts](seg, off + As.bytesOf[t])).asInstanceOf[Ts]

    /** Write the fields of `Ts`, packed starting at byte offset `off`, from tuple `v` starting at product index `k`. */
    inline def writeTuple[Ts <: Tuple](seg: MemorySegment, off: Long, v: Tuple, inline k: Int): Unit =
      inline erasedValue[Ts] match
        case _: EmptyTuple => ()
        case _: (t *: ts)  =>
          As.writeAt[t](seg, off, v.productElement(k).asInstanceOf[t])
          writeTuple[ts](seg, off + As.bytesOf[t], v, k + 1)

    /** A struct index bound by type to the one array instance it indexes: `A` is that array's
      * singleton type, and every field access summons the array contextually (`using A`), so
      * `idx.x` reads and `idx.x = v` writes field `x` of struct `idx` -- and an index made for
      * one array cannot be dereferenced via another, even of the same element type.  The loops
      * that hand these out (`use`, `pairs`, `trios`, `gather`, `where*`) provide the context;
      * erasure is a bare `Long`, so indices are free to keep and index arithmetic (`+`, `-`)
      * is ordinary arithmetic.
      */
    final class Index[T <: NamedTuple.AnyNamedTuple, A <: AoS[T]](private[basics] val raw: Long) extends AnyVal with Dynamic {
      /** The plain struct index. */
      inline def unwrap: Long = raw

      inline def +(k: Long): Index[T, A] = new Index[T, A](raw + k)
      inline def -(k: Long): Index[T, A] = new Index[T, A](raw - k)

      /** Field `name` of the struct this index points to in the contextual array. */
      inline def selectDynamic(name: String & Singleton)(using a: A): FieldType[T, name.type] =
        As.readAt[FieldType[T, name.type]](a.segment, raw * strideOf[T] + offsetOf[T, name.type])

      /** Set field `name` of the struct this index points to in the contextual array. */
      inline def updateDynamic(name: String & Singleton)(x: FieldType[T, name.type])(using a: A): Unit =
        As.writeAt[FieldType[T, name.type]](a.segment, raw * strideOf[T] + offsetOf[T, name.type], x)
    }

    /** One named column of an [[AoS]]: strided single-field access to every struct, zero-cost.
      * It is the same bare segment as the whole array; the stride and offset are computed
      * at compile time from `T` and `N` on every use.
      */
    final class Field[T <: NamedTuple.AnyNamedTuple, N <: LabelStr](val segment: MemorySegment) extends AnyVal {
      /** Bytes per struct of the underlying array. */
      inline def stride: Long = strideOf[T]

      /** Number of whole structs. */
      inline def length: Long = segment.byteSize / strideOf[T]

      /** This field of struct `i`. */
      inline def apply(i: Long): FieldType[T, N] =
        As.readAt[FieldType[T, N]](segment, i * strideOf[T] + offsetOf[T, N])

      /** Set this field of struct `i`. */
      inline def update(i: Long, x: FieldType[T, N]): Unit =
        As.writeAt[FieldType[T, N]](segment, i * strideOf[T] + offsetOf[T, N], x)

      /** Visit this field of every complete struct, in order. */
      inline def use(inline f: FieldType[T, N] => Unit): Unit =
        var i = 0L
        val n = length
        while i < n do
          f(apply(i))
          i += 1

      /** Visit this field of every complete struct with its struct index. */
      inline def visit(inline f: (FieldType[T, N], Long) => Unit): Unit =
        var i = 0L
        val n = length
        while i < n do
          f(apply(i), i)
          i += 1

      /** Replace this field of every complete struct. */
      inline def alter(inline f: FieldType[T, N] => FieldType[T, N]): Unit =
        var i = 0L
        val n = length
        while i < n do
          update(i, f(apply(i)))
          i += 1

      /** Set this field of every complete struct from its struct index. */
      inline def set(inline indexer: Long => FieldType[T, N]): Unit =
        var i = 0L
        val n = length
        while i < n do
          update(i, indexer(i))
          i += 1

      /** Fold over this field of every complete struct, in order. */
      inline def gather[Z](zero: Z)(inline f: (Z, FieldType[T, N], Long) => Z): Z =
        var i = 0L
        val n = length
        var z = zero
        while i < n do
          z = f(z, apply(i), i)
          i += 1
        z
    }
  }

  /** A positioned view of one struct (laid out as in [[AoS]]) within a memory segment.
    * Field names dispatch inline to reads and writes at the view's byte offset: `s.x` reads
    * field `x` and `s.x = v` writes it, at zero cost beyond the access itself.
    *
    * The position is fixed at construction, so a view always watches the same struct: this is
    * the keepable way to refer to one item of an [[AoS]].  Place a live view with
    * [[AoS.cursor]] (or directly by byte offset), or copy one out with [[AoS.struct]];
    * traversal hands out [[AoS.Index]] values instead.
    */
  final class Struct[T <: NamedTuple.AnyNamedTuple](
    private[basics] val segment: MemorySegment,
    private[basics] val offset: Long
  ) extends Dynamic {
    /** Field `name` of the struct under the view. */
    inline def selectDynamic(name: String & Singleton): AoS.FieldType[T, name.type] =
      As.readAt[AoS.FieldType[T, name.type]](segment, offset + AoS.offsetOf[T, name.type])

    /** Set field `name` of the struct under the view. */
    inline def updateDynamic(name: String & Singleton)(x: AoS.FieldType[T, name.type]): Unit =
      As.writeAt[AoS.FieldType[T, name.type]](segment, offset + AoS.offsetOf[T, name.type], x)
  }
}


/** A bounds-clipping view of a [[Mem]]: indices outside `[0, length)` are skipped, and
  * ranges are clamped, so no access ever throws.
  */
opaque type ClippedMem[A <: Mem.Type] = MemorySegment
object ClippedMem {
  inline def wrap[A <: Mem.Type](seg: MemorySegment): ClippedMem[A] = seg

  extension [A <: Mem.Type](cm: ClippedMem[A])
    inline def unclip: Mem[A] = Mem.wrap(cm)

  extension [A <: Mem.Type](cm: kse.basics.ClippedMem[A]) {
    /** Element `i`, or `x0` if `i` is out of range. */
    inline def apply(i: Long)(inline x0: => A): A =
      val m = cm.unclip
      if i >= 0 && i < Mem.length(m) then Mem.apply(m)(i) else x0

    /** Element `i` as a `Some`, or `None` if out of range. */
    inline def get(i: Long): Option[A] =
      val m = cm.unclip
      if i >= 0 && i < Mem.length(m) then Some(Mem.apply(m)(i)) else None

    inline def length: Long =
      (cm: MemorySegment).byteSize / Mem.bytesOf[A]

    /** Read a primitive of the stated type at element index `i` (byte offset `i * bytesOf[A]`),
      * or `None` if any byte of it would fall out of range.
      */
    inline def getB(i: Long): Option[Byte] =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 1 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_BYTE, off)) else None
    inline def getS(i: Long): Option[Short] =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 2 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_SHORT_UNALIGNED, off)) else None
    inline def getC(i: Long): Option[Char] =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 2 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_CHAR_UNALIGNED, off)) else None
    inline def getI(i: Long): Option[Int] =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 4 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_INT_UNALIGNED, off)) else None
    inline def getF(i: Long): Option[Float] =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 4 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_FLOAT_UNALIGNED, off)) else None
    inline def getL(i: Long): Option[Long] =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 8 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_LONG_UNALIGNED, off)) else None
    inline def getD(i: Long): Option[Double] =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 8 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_DOUBLE_UNALIGNED, off)) else None

    /** Write a primitive of the stated type at element index `i` (byte offset `i * bytesOf[A]`),
      * silently doing nothing if any byte of it would fall out of range.
      */
    inline def setB(i: Long, x: Byte): Unit =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 1 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_BYTE, off, x)
    inline def setS(i: Long, x: Short): Unit =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 2 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_SHORT_UNALIGNED, off, x)
    inline def setC(i: Long, x: Char): Unit =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 2 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_CHAR_UNALIGNED, off, x)
    inline def setI(i: Long, x: Int): Unit =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 4 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_INT_UNALIGNED, off, x)
    inline def setF(i: Long, x: Float): Unit =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 4 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_FLOAT_UNALIGNED, off, x)
    inline def setL(i: Long, x: Long): Unit =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 8 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_LONG_UNALIGNED, off, x)
    inline def setD(i: Long, x: Double): Unit =
      val off = i * Mem.bytesOf[A]
      if off >= 0 && off + 8 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_DOUBLE_UNALIGNED, off, x)


    inline def use(i: Long)(inline f: A => Unit): Unit =
      val m = cm.unclip
      if i >= 0 && i < Mem.length(m) then f(Mem.apply(m)(i))
    inline def use(i0: Long, iN: Long)(inline f: A => Unit): Unit =
      val m = cm.unclip
      var i = if i0 < 0 then 0L else i0
      val iM = if iN > Mem.length(m) then Mem.length(m) else iN
      while i < iM do
        f(Mem.apply(m)(i))
        i += 1
    inline def use(indices: Array[Long])(inline f: A => Unit): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < n then f(Mem.apply(m)(j))
        i += 1
    inline def use(indices: LongStepper)(inline f: A => Unit): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      while indices.hasStep do
        val j = indices.nextStep()
        if j >= 0 && j < n then f(Mem.apply(m)(j))

    inline def alter(i0: Long, iN: Long)(inline f: A => A): Unit =
      val m = cm.unclip
      var i = if i0 < 0 then 0L else i0
      val iM = if iN > Mem.length(m) then Mem.length(m) else iN
      while i < iM do
        Mem.update(m)(i, f(Mem.apply(m)(i)))
        i += 1
    inline def alter(indices: Array[Long])(inline f: A => A): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < n then Mem.update(m)(j, f(Mem.apply(m)(j)))
        i += 1
    inline def alter(indices: LongStepper)(inline f: A => A): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      while indices.hasStep do
        val j = indices.nextStep()
        if j >= 0 && j < n then Mem.update(m)(j, f(Mem.apply(m)(j)))

    inline def visit(i0: Long, iN: Long)(inline f: (A, Long) => Unit): Unit =
      val m = cm.unclip
      var i = if i0 < 0 then 0L else i0
      val iM = if iN > Mem.length(m) then Mem.length(m) else iN
      while i < iM do
        f(Mem.apply(m)(i), i)
        i += 1
    inline def visit(indices: Array[Long])(inline f: (A, Long) => Unit): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < n then f(Mem.apply(m)(j), j)
        i += 1
    inline def visit(indices: LongStepper)(inline f: (A, Long) => Unit): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      while indices.hasStep do
        val j = indices.nextStep()
        if j >= 0 && j < n then f(Mem.apply(m)(j), j)

    inline def edit(i0: Long, iN: Long)(inline f: (A, Long) => A): Unit =
      val m = cm.unclip
      var i = if i0 < 0 then 0L else i0
      val iM = if iN > Mem.length(m) then Mem.length(m) else iN
      while i < iM do
        Mem.update(m)(i, f(Mem.apply(m)(i), i))
        i += 1
    inline def edit(indices: Array[Long])(inline f: (A, Long) => A): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < n then Mem.update(m)(j, f(Mem.apply(m)(j), j))
        i += 1
    inline def edit(indices: LongStepper)(inline f: (A, Long) => A): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      while indices.hasStep do
        val j = indices.nextStep()
        if j >= 0 && j < n then Mem.update(m)(j, f(Mem.apply(m)(j), j))

    inline def gather[Z](zero: Z)(i0: Long, iN: Long)(inline f: (Z, A, Long) => Z): Z =
      val m = cm.unclip
      var i = if i0 < 0 then 0L else i0
      val iM = if iN > Mem.length(m) then Mem.length(m) else iN
      var z = zero
      while i < iM do
        z = f(z, Mem.apply(m)(i), i)
        i += 1
      z
    inline def gather[Z](zero: Z)(indices: Array[Long])(inline f: (Z, A, Long) => Z): Z =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0
      var z = zero
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < n then z = f(z, Mem.apply(m)(j), j)
        i += 1
      z
    inline def gather[Z](zero: Z)(indices: LongStepper)(inline f: (Z, A, Long) => Z): Z =
      val m = cm.unclip
      val n = Mem.length(m)
      var z = zero
      while indices.hasStep do
        val j = indices.nextStep()
        if j >= 0 && j < n then z = f(z, Mem.apply(m)(j), j)
      z

    @targetName("update_i0iN_constant")
    inline def update(i0: Long, iN: Long, value: A): Unit =
      val m = cm.unclip
      var i = if i0 < 0 then 0L else i0
      val iM = if iN > Mem.length(m) then Mem.length(m) else iN
      while i < iM do
        Mem.update(m)(i, value)
        i += 1
    @targetName("update_Places_constant")
    inline def update(indices: Array[Long], value: A): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < n then Mem.update(m)(j, value)
        i += 1
    @targetName("update_Stepper_constant")
    inline def update(indices: LongStepper, value: A): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      while indices.hasStep do
        val j = indices.nextStep()
        if j >= 0 && j < n then Mem.update(m)(j, value)

    @targetName("set_i0iN_generate")
    inline def set(i0: Long, iN: Long)(inline generator: () => A): Unit =
      val m = cm.unclip
      var i = if i0 < 0 then 0L else i0
      val iM = if iN > Mem.length(m) then Mem.length(m) else iN
      while i < iM do
        Mem.update(m)(i, generator())
        i += 1
    @targetName("set_i0iN_index")
    inline def set(i0: Long, iN: Long)(inline indexer: Long => A): Unit =
      val m = cm.unclip
      var i = if i0 < 0 then 0L else i0
      val iM = if iN > Mem.length(m) then Mem.length(m) else iN
      while i < iM do
        Mem.update(m)(i, indexer(i))
        i += 1
    @targetName("set_Places_generate")
    inline def set(indices: Array[Long])(inline generator: () => A): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < n then Mem.update(m)(j, generator())
        i += 1
    @targetName("set_Places_index")
    inline def set(indices: Array[Long])(inline indexer: Long => A): Unit =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0
      while i < indices.length do
        val j = indices(i)
        if j >= 0 && j < n then Mem.update(m)(j, indexer(j))
        i += 1

    inline def whereIn(i0: Long, iN: Long)(inline pick: A => Boolean): Array[Long] =
      val m = cm.unclip
      var i = if i0 < 0 then 0L else i0
      val iM = if iN > Mem.length(m) then Mem.length(m) else iN
      var ix = new Array[Long](if iM - i < 0 then 0 else if iM - i > 8 then 8 else (iM - i).toInt)
      var j = 0
      while i < iM do
        if pick(Mem.apply(m)(i)) then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = i
          j += 1
        i += 1
      ix.shrinkTo(j)
    inline def whereFrom(indices: Array[Long])(inline pick: A => Boolean): Array[Long] =
      val m = cm.unclip
      val n = Mem.length(m)
      var ix = new Array[Long](if indices.length > 8 then 8 else indices.length)
      var i = 0
      var j = 0
      while i < indices.length do
        val k = indices(i)
        if k >= 0 && k < n && pick(Mem.apply(m)(k)) then
          if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
          ix(j) = k
          j += 1
        i += 1
      ix.shrinkTo(j)

    inline def inject(that: Mem[A]): Long =
      inject(that, 0L)(0L, cm.length)
    inline def inject(that: Mem[A], where: Long): Long =
      inject(that, where)(0L, cm.length)
    inline def inject(that: Mem[A])(i0: Long, iN: Long): Long =
      inject(that, 0L)(i0, iN)
    inline def inject(that: Mem[A], where: Long)(i0: Long, iN: Long): Long =
      val m = cm.unclip
      val w = if where < 0 then 0L else where
      val i = if i0 < 0 then 0L else i0
      val j = if iN >= Mem.length(m) then Mem.length(m) else iN
      if i < j && w < Mem.length(that) then
        var n = Mem.length(that) - w
        if n > j - i then n = j - i
        val eb = Mem.bytesOf[A]
        MemorySegment.copy(Mem.segment(m), i * eb, Mem.segment(that), w * eb, n * eb)
        n
      else 0L
    inline def inject(that: Mem[A])(indices: Array[Long]): Long =
      inject(that, 0L)(indices)
    inline def inject(that: Mem[A], where: Long)(indices: Array[Long]): Long =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0
      var j = if where < 0 then 0L else where
      while i < indices.length && j < Mem.length(that) do
        val k = indices(i)
        if k >= 0 && k < n then
          Mem.update(that)(j, Mem.apply(m)(k))
          j += 1
        i += 1
      if where < 0 then j else j - where
    inline def inject(that: Mem[A])(inline pick: A => Boolean): Long =
      inject(that, 0L)(pick)
    inline def inject(that: Mem[A], where: Long)(inline pick: A => Boolean): Long =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0L
      var j = if where < 0 then 0L else where
      while i < n && j < Mem.length(that) do
        val x = Mem.apply(m)(i)
        if pick(x) then
          Mem.update(that)(j, x)
          j += 1
        i += 1
      if where < 0 then j else j - where

    inline def inject(that: Array[A]): Long =
      inject(that, 0)(0L, cm.length)
    inline def inject(that: Array[A], where: Int): Long =
      inject(that, where)(0L, cm.length)
    inline def inject(that: Array[A])(i0: Long, iN: Long): Long =
      inject(that, 0)(i0, iN)
    inline def inject(that: Array[A], where: Int)(i0: Long, iN: Long): Long =
      val m = cm.unclip
      val w = if where < 0 then 0 else where
      val i = if i0 < 0 then 0L else i0
      val j = if iN >= Mem.length(m) then Mem.length(m) else iN
      if i < j && w < that.length then
        var n = (that.length - w).toLong
        if n > j - i then n = j - i
        MemorySegment.copy(Mem.segment(m), Mem.layoutOf[A], i * Mem.bytesOf[A], that, w, n.toInt)
        n
      else 0L
    inline def inject(that: Array[A])(indices: Array[Long]): Long =
      inject(that, 0)(indices)
    inline def inject(that: Array[A], where: Int)(indices: Array[Long]): Long =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0
      var j = if where < 0 then 0 else where
      while i < indices.length && j < that.length do
        val k = indices(i)
        if k >= 0 && k < n then
          that(j) = Mem.apply(m)(k)
          j += 1
        i += 1
      (if where < 0 then j else j - where).toLong
    inline def inject(that: Array[A])(inline pick: A => Boolean): Long =
      inject(that, 0)(pick)
    inline def inject(that: Array[A], where: Int)(inline pick: A => Boolean): Long =
      val m = cm.unclip
      val n = Mem.length(m)
      var i = 0L
      var j = if where < 0 then 0 else where
      while i < n && j < that.length do
        val x = Mem.apply(m)(i)
        if pick(x) then
          that(j) = x
          j += 1
        i += 1
      (if where < 0 then j else j - where).toLong

    inline def visitCuts(i0: Long, iN: Long)(inline cut: (A, A) => Boolean)(inline f: (Long, Long) => Unit): Unit =
      val m = cm.unclip
      var i = if i0 < 0 then 0L else i0
      var l = Mem.length(m)
      if iN < l then l = iN
      while i < l do
        var x = Mem.apply(m)(i)
        var j = i + 1
        var continue = true
        while continue && j < l do
          val y = Mem.apply(m)(j)
          if cut(x, y) then continue = false
          else
            x = y
            j += 1
        f(i, j)
        i = j
  }

  // === ClippedMem.As: the bounds-clipping twin of Mem.As ===

  /** A bounds-clipping view of a [[Mem.As]]: indices outside `[0, length)` are skipped, and
    * ranges are clamped, so no access ever throws.  Element types resolve exactly as in `Mem.As`.
    */
  opaque type As[O] = MemorySegment
  object As {
    inline def wrap[O](seg: MemorySegment): As[O] = seg

    extension [O](cm: As[O])
      inline def unclip: Mem.As[O] = Mem.As.wrap[O](cm)

    extension [O](cm: kse.basics.ClippedMem.As[O]) {
      /** Element `i`, or `x0` if `i` is out of range. */
      inline def apply(i: Long)(inline x0: => O): O =
        val m = cm.unclip
        if i >= 0 && i < Mem.As.length(m) then Mem.As.apply(m)(i) else x0

      /** Element `i` as a `Some`, or `None` if out of range. */
      inline def get(i: Long): Option[O] =
        val m = cm.unclip
        if i >= 0 && i < Mem.As.length(m) then Some(Mem.As.apply(m)(i)) else None

      inline def length: Long =
        (cm: MemorySegment).byteSize / Mem.As.bytesOf[O]

      /** Read a primitive of the stated type at element index `i` (byte offset `i * bytesOf[O]`),
        * or `None` if any byte of it would fall out of range.
        */
      inline def getB(i: Long): Option[Byte] =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 1 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_BYTE, off)) else None
      inline def getS(i: Long): Option[Short] =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 2 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_SHORT_UNALIGNED, off)) else None
      inline def getC(i: Long): Option[Char] =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 2 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_CHAR_UNALIGNED, off)) else None
      inline def getI(i: Long): Option[Int] =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 4 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_INT_UNALIGNED, off)) else None
      inline def getF(i: Long): Option[Float] =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 4 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_FLOAT_UNALIGNED, off)) else None
      inline def getL(i: Long): Option[Long] =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 8 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_LONG_UNALIGNED, off)) else None
      inline def getD(i: Long): Option[Double] =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 8 <= (cm: MemorySegment).byteSize then Some((cm: MemorySegment).get(JAVA_DOUBLE_UNALIGNED, off)) else None

      /** Write a primitive of the stated type at element index `i` (byte offset `i * bytesOf[O]`),
        * silently doing nothing if any byte of it would fall out of range.
        */
      inline def setB(i: Long, x: Byte): Unit =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 1 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_BYTE, off, x)
      inline def setS(i: Long, x: Short): Unit =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 2 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_SHORT_UNALIGNED, off, x)
      inline def setC(i: Long, x: Char): Unit =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 2 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_CHAR_UNALIGNED, off, x)
      inline def setI(i: Long, x: Int): Unit =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 4 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_INT_UNALIGNED, off, x)
      inline def setF(i: Long, x: Float): Unit =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 4 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_FLOAT_UNALIGNED, off, x)
      inline def setL(i: Long, x: Long): Unit =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 8 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_LONG_UNALIGNED, off, x)
      inline def setD(i: Long, x: Double): Unit =
        val off = i * Mem.As.bytesOf[O]
        if off >= 0 && off + 8 <= (cm: MemorySegment).byteSize then (cm: MemorySegment).set(JAVA_DOUBLE_UNALIGNED, off, x)


      inline def use(i: Long)(inline f: O => Unit): Unit =
        val m = cm.unclip
        if i >= 0 && i < Mem.As.length(m) then f(Mem.As.apply(m)(i))
      inline def use(i0: Long, iN: Long)(inline f: O => Unit): Unit =
        val m = cm.unclip
        var i = if i0 < 0 then 0L else i0
        val iM = if iN > Mem.As.length(m) then Mem.As.length(m) else iN
        while i < iM do
          f(Mem.As.apply(m)(i))
          i += 1
      inline def use(indices: Array[Long])(inline f: O => Unit): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < n then f(Mem.As.apply(m)(j))
          i += 1
      inline def use(indices: LongStepper)(inline f: O => Unit): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        while indices.hasStep do
          val j = indices.nextStep()
          if j >= 0 && j < n then f(Mem.As.apply(m)(j))

      inline def alter(i0: Long, iN: Long)(inline f: O => O): Unit =
        val m = cm.unclip
        var i = if i0 < 0 then 0L else i0
        val iM = if iN > Mem.As.length(m) then Mem.As.length(m) else iN
        while i < iM do
          Mem.As.update(m)(i, f(Mem.As.apply(m)(i)))
          i += 1
      inline def alter(indices: Array[Long])(inline f: O => O): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < n then Mem.As.update(m)(j, f(Mem.As.apply(m)(j)))
          i += 1
      inline def alter(indices: LongStepper)(inline f: O => O): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        while indices.hasStep do
          val j = indices.nextStep()
          if j >= 0 && j < n then Mem.As.update(m)(j, f(Mem.As.apply(m)(j)))

      inline def visit(i0: Long, iN: Long)(inline f: (O, Long) => Unit): Unit =
        val m = cm.unclip
        var i = if i0 < 0 then 0L else i0
        val iM = if iN > Mem.As.length(m) then Mem.As.length(m) else iN
        while i < iM do
          f(Mem.As.apply(m)(i), i)
          i += 1
      inline def visit(indices: Array[Long])(inline f: (O, Long) => Unit): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < n then f(Mem.As.apply(m)(j), j)
          i += 1
      inline def visit(indices: LongStepper)(inline f: (O, Long) => Unit): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        while indices.hasStep do
          val j = indices.nextStep()
          if j >= 0 && j < n then f(Mem.As.apply(m)(j), j)

      inline def edit(i0: Long, iN: Long)(inline f: (O, Long) => O): Unit =
        val m = cm.unclip
        var i = if i0 < 0 then 0L else i0
        val iM = if iN > Mem.As.length(m) then Mem.As.length(m) else iN
        while i < iM do
          Mem.As.update(m)(i, f(Mem.As.apply(m)(i), i))
          i += 1
      inline def edit(indices: Array[Long])(inline f: (O, Long) => O): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < n then Mem.As.update(m)(j, f(Mem.As.apply(m)(j), j))
          i += 1
      inline def edit(indices: LongStepper)(inline f: (O, Long) => O): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        while indices.hasStep do
          val j = indices.nextStep()
          if j >= 0 && j < n then Mem.As.update(m)(j, f(Mem.As.apply(m)(j), j))

      inline def gather[Z](zero: Z)(i0: Long, iN: Long)(inline f: (Z, O, Long) => Z): Z =
        val m = cm.unclip
        var i = if i0 < 0 then 0L else i0
        val iM = if iN > Mem.As.length(m) then Mem.As.length(m) else iN
        var z = zero
        while i < iM do
          z = f(z, Mem.As.apply(m)(i), i)
          i += 1
        z
      inline def gather[Z](zero: Z)(indices: Array[Long])(inline f: (Z, O, Long) => Z): Z =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0
        var z = zero
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < n then z = f(z, Mem.As.apply(m)(j), j)
          i += 1
        z
      inline def gather[Z](zero: Z)(indices: LongStepper)(inline f: (Z, O, Long) => Z): Z =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var z = zero
        while indices.hasStep do
          val j = indices.nextStep()
          if j >= 0 && j < n then z = f(z, Mem.As.apply(m)(j), j)
        z

      @targetName("update_i0iN_constant")
      inline def update(i0: Long, iN: Long, value: O): Unit =
        val m = cm.unclip
        var i = if i0 < 0 then 0L else i0
        val iM = if iN > Mem.As.length(m) then Mem.As.length(m) else iN
        while i < iM do
          Mem.As.update(m)(i, value)
          i += 1
      @targetName("update_Places_constant")
      inline def update(indices: Array[Long], value: O): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < n then Mem.As.update(m)(j, value)
          i += 1
      @targetName("update_Stepper_constant")
      inline def update(indices: LongStepper, value: O): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        while indices.hasStep do
          val j = indices.nextStep()
          if j >= 0 && j < n then Mem.As.update(m)(j, value)

      @targetName("set_i0iN_generate")
      inline def set(i0: Long, iN: Long)(inline generator: () => O): Unit =
        val m = cm.unclip
        var i = if i0 < 0 then 0L else i0
        val iM = if iN > Mem.As.length(m) then Mem.As.length(m) else iN
        while i < iM do
          Mem.As.update(m)(i, generator())
          i += 1
      @targetName("set_i0iN_index")
      inline def set(i0: Long, iN: Long)(inline indexer: Long => O): Unit =
        val m = cm.unclip
        var i = if i0 < 0 then 0L else i0
        val iM = if iN > Mem.As.length(m) then Mem.As.length(m) else iN
        while i < iM do
          Mem.As.update(m)(i, indexer(i))
          i += 1
      @targetName("set_Places_generate")
      inline def set(indices: Array[Long])(inline generator: () => O): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < n then Mem.As.update(m)(j, generator())
          i += 1
      @targetName("set_Places_index")
      inline def set(indices: Array[Long])(inline indexer: Long => O): Unit =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0
        while i < indices.length do
          val j = indices(i)
          if j >= 0 && j < n then Mem.As.update(m)(j, indexer(j))
          i += 1

      inline def whereIn(i0: Long, iN: Long)(inline pick: O => Boolean): Array[Long] =
        val m = cm.unclip
        var i = if i0 < 0 then 0L else i0
        val iM = if iN > Mem.As.length(m) then Mem.As.length(m) else iN
        var ix = new Array[Long](if iM - i < 0 then 0 else if iM - i > 8 then 8 else (iM - i).toInt)
        var j = 0
        while i < iM do
          if pick(Mem.As.apply(m)(i)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = i
            j += 1
          i += 1
        ix.shrinkTo(j)
      inline def whereFrom(indices: Array[Long])(inline pick: O => Boolean): Array[Long] =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var ix = new Array[Long](if indices.length > 8 then 8 else indices.length)
        var i = 0
        var j = 0
        while i < indices.length do
          val k = indices(i)
          if k >= 0 && k < n && pick(Mem.As.apply(m)(k)) then
            if j >= ix.length then ix = ix.enlargeTo(ix.length | (ix.length << 1))
            ix(j) = k
            j += 1
          i += 1
        ix.shrinkTo(j)

      inline def inject(that: Mem.As[O]): Long =
        inject(that, 0L)(0L, cm.length)
      inline def inject(that: Mem.As[O], where: Long): Long =
        inject(that, where)(0L, cm.length)
      inline def inject(that: Mem.As[O])(i0: Long, iN: Long): Long =
        inject(that, 0L)(i0, iN)
      inline def inject(that: Mem.As[O], where: Long)(i0: Long, iN: Long): Long =
        val m = cm.unclip
        val w = if where < 0 then 0L else where
        val i = if i0 < 0 then 0L else i0
        val j = if iN >= Mem.As.length(m) then Mem.As.length(m) else iN
        if i < j && w < Mem.As.length(that) then
          var n = Mem.As.length(that) - w
          if n > j - i then n = j - i
          val eb = Mem.As.bytesOf[O]
          MemorySegment.copy(Mem.As.segment(m), i * eb, Mem.As.segment(that), w * eb, n * eb)
          n
        else 0L
      inline def inject(that: Mem.As[O])(indices: Array[Long]): Long =
        inject(that, 0L)(indices)
      inline def inject(that: Mem.As[O], where: Long)(indices: Array[Long]): Long =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0
        var j = if where < 0 then 0L else where
        while i < indices.length && j < Mem.As.length(that) do
          val k = indices(i)
          if k >= 0 && k < n then
            Mem.As.update(that)(j, Mem.As.apply(m)(k))
            j += 1
          i += 1
        if where < 0 then j else j - where
      inline def inject(that: Mem.As[O])(inline pick: O => Boolean): Long =
        inject(that, 0L)(pick)
      inline def inject(that: Mem.As[O], where: Long)(inline pick: O => Boolean): Long =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0L
        var j = if where < 0 then 0L else where
        while i < n && j < Mem.As.length(that) do
          val x = Mem.As.apply(m)(i)
          if pick(x) then
            Mem.As.update(that)(j, x)
            j += 1
          i += 1
        if where < 0 then j else j - where

      inline def inject(that: Array[O]): Long =
        inject(that, 0)(0L, cm.length)
      inline def inject(that: Array[O], where: Int): Long =
        inject(that, where)(0L, cm.length)
      inline def inject(that: Array[O])(i0: Long, iN: Long): Long =
        inject(that, 0)(i0, iN)
      inline def inject(that: Array[O], where: Int)(i0: Long, iN: Long): Long =
        val m = cm.unclip
        val w = if where < 0 then 0 else where
        val i = if i0 < 0 then 0L else i0
        val j = if iN >= Mem.As.length(m) then Mem.As.length(m) else iN
        if i < j && w < that.length then
          var n = (that.length - w).toLong
          if n > j - i then n = j - i
          MemorySegment.copy(Mem.As.segment(m), Mem.As.layoutOf[O], i * Mem.As.bytesOf[O], that, w, n.toInt)
          n
        else 0L
      inline def inject(that: Array[O])(indices: Array[Long]): Long =
        inject(that, 0)(indices)
      inline def inject(that: Array[O], where: Int)(indices: Array[Long]): Long =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0
        var j = if where < 0 then 0 else where
        while i < indices.length && j < that.length do
          val k = indices(i)
          if k >= 0 && k < n then
            that(j) = Mem.As.apply(m)(k)
            j += 1
          i += 1
        (if where < 0 then j else j - where).toLong
      inline def inject(that: Array[O])(inline pick: O => Boolean): Long =
        inject(that, 0)(pick)
      inline def inject(that: Array[O], where: Int)(inline pick: O => Boolean): Long =
        val m = cm.unclip
        val n = Mem.As.length(m)
        var i = 0L
        var j = if where < 0 then 0 else where
        while i < n && j < that.length do
          val x = Mem.As.apply(m)(i)
          if pick(x) then
            that(j) = x
            j += 1
          i += 1
        (if where < 0 then j else j - where).toLong

      inline def visitCuts(i0: Long, iN: Long)(inline cut: (O, O) => Boolean)(inline f: (Long, Long) => Unit): Unit =
        val m = cm.unclip
        var i = if i0 < 0 then 0L else i0
        var l = Mem.As.length(m)
        if iN < l then l = iN
        while i < l do
          var x = Mem.As.apply(m)(i)
          var j = i + 1
          var continue = true
          while continue && j < l do
            val y = Mem.As.apply(m)(j)
            if cut(x, y) then continue = false
            else
              x = y
              j += 1
          f(i, j)
          i = j
    }
  }
}
