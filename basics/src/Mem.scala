// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.basics


import java.lang.foreign.{Arena, MemorySegment}
import java.lang.foreign.ValueLayout.*

import scala.annotation.targetName
import scala.collection.LongStepper
import scala.compiletime.{erasedValue, error}


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

    /** A bounds-clipping view: out-of-range indices are silently skipped or clamped. */
    inline def clip: kse.basics.ClippedMem[A] = ClippedMem wrap m

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
}
