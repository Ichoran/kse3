// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024 Rex Kerr

package kse.eio


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.nio.{ByteBuffer, CharBuffer}

import scala.util.boundary
import scala.util.boundary.{Label => Lb}

import kse.basics.{given, _}
import kse.basics.intervals.{given, _}

import kse.flow.{given, _}


sealed trait Ingester[A, I, E] {
  /** If empty, must return 0.  If size is known for sure, the size.  If at least N items are available but more might be, returns -N. */
  inline def remaining(a: A, inline i: => I): Long

  /** Requests `n` items; `true` means they're available, `false` means not.  Must block / cache results to avoid a "maybe". */
  inline def has(a: A, inline i: => I)(n: Int): Boolean

  /** True if at least one item is available. */
  inline def nonEmpty(a: A, inline i: => I): Boolean

  /** Skip up to `n` items; passes the actual number skipped into `inc`, which the caller must use to handle any updates to `I` if needed. */
  inline def skip(a: A, inline i: => I, inline inc: Int => Unit)(n: Int): Unit

  /** Undo up to `n` items; passes the actual number undone into 'dec', which the caller must use to handle any updates to `I` if needed. */
  inline def undo(a: A, inline i: => I, inline dec: Int => Unit)(n: Int): Unit

  /** Return the next item if available, or jump to the boundary with an error if not. Calls `inc` if successful. */
  inline def step[X >: Alt[Err]](a: A, inline i: => I, inline inc: () => Unit)(using Lb[X]): E
}
object Ingester {
  sealed trait CompleteIngester[A, E] extends Ingester[A, Unit, E] {
    final inline def remaining(a: A, inline i: => Unit): Long = remaining(a)
    inline def remaining(a: A): Long

    final inline def has(a: A, inline i: => Unit)(n: Int): Boolean = has(a)(n)
    inline def has(a: A)(n: Int): Boolean

    final inline def nonEmpty(a: A, inline i: => Unit): Boolean = nonEmpty(a)
    inline def nonEmpty(a: A): Boolean

    final inline def skip(a: A, inline i: => Unit, inline inc: Int => Unit)(n: Int): Unit = skip(a, inc)(n)
    inline def skip(a: A, inline inc: Int => Unit)(n: Int): Unit

    final inline def undo(a: A, inline i: => Unit, inline dec: Int => Unit)(n: Int): Unit = undo(a, dec)(n)
    inline def undo(a: A, inline dec: Int => Unit)(n: Int): Unit

    final inline def step[X >: Alt[Err]](a: A, inline i: => Unit, inline inc: () => Unit)(using Lb[X]): E = step[X](a, inc)
    inline def step[X >: Alt[Err]](a: A, inline inc: () => Unit)(using Lb[X]): E
  }

  sealed trait ArrayIngester[E] extends Ingester[Array[E], Int, E] {
    inline def remaining(a: Array[E], inline i: => Int) = a.length - i
    inline def has(a: Array[E], inline i: => Int)(n: Int) = n <= 0 || i < a.length - n
    inline def nonEmpty(a: Array[E], inline i: => Int) = i < a.length

    inline def skip(a: Array[E], inline i: => Int, inline inc: Int => Unit)(n: Int): Unit =
      if n > 0 then
        val m = a.length - i
        val h = if m < n then m else n
        inc(h)

    inline def undo(a: Array[E], inline i: => Int, inline dec: Int => Unit)(n: Int): Unit =
      if n > 0 then
        val m = i
        val h = if m < n then m else n
        dec(h)

    inline def step[X >: Alt[Err]](a: Array[E], inline i: => Int, inline inc: () => Unit)(using Lb[X]): E =
      val ii = i
      if ii < a.length then
        inc()
        a(ii)
      else
        boundary.break(Err.or(s"Array at end, position ${a.length}"))
  }
  object ByteArrayIngester extends ArrayIngester[Byte] {}
  object CharArrayIngester extends ArrayIngester[Char] {}

  sealed trait BufferIngester[B <: java.nio.Buffer, A] extends CompleteIngester[B, A] {
    inline def remaining(a: B) = a.remaining()
    inline def has(a: B)(n: Int) = a.remaining() >= n
    inline def nonEmpty(a: B): Boolean = a.hasRemaining()    

    inline def skip(a: B, inline inc: Int => Unit)(n: Int): Unit =
      if n > 0 then
        val m = a.remaining()
        val h =
          if m < n then
            a.position(a.limit)
            m
          else
            a.position(a.position + n)
            n
        inc(h)

    inline def undo(a: B, inline dec: Int => Unit)(n: Int): Unit =
      if n > 0 then
        val m = a.position
        val h =
          if m < n then
            a.position(0)
            m
          else
            a.position(a.position - n)
            n
        dec(h)
  }
  object ByteBufferIngester extends BufferIngester[ByteBuffer, Byte] {
    inline def step[X >: Alt[Err]](a: ByteBuffer, inline inc: () => Unit)(using Lb[X]): Byte =
      if a.hasRemaining() then
        inc()
        a.get()
      else
        boundary.break(Err.or(s"Buffer at end, position ${a.limit}"))
  }
  object CharBufferIngester extends BufferIngester[CharBuffer, Char] {
    inline def step[X >: Alt[Err]](a: CharBuffer, inline inc: () => Unit)(using Lb[X]): Char =
      if a.hasRemaining() then
        inc()
        a.get()
      else
        boundary.break(Err.or(s"Buffer at end, position ${a.limit}"))
  }


  object StringIngester extends Ingester[String, Int, Char] {
    inline def remaining(a: String, inline i: => Int): Long = a.length - i
    inline def has(a: String, inline i: => Int)(n: Int) = n <= 0 || i < a.length - n
    inline def nonEmpty(a: String, inline i: => Int) = i < a.length

    inline def skip(a: String, inline i: => Int, inline inc: Int => Unit)(n: Int): Unit =
      if n > 0 then
        val m = a.length - i
        val h = if m < n then m else n
        inc(h)

    inline def undo(a: String, inline i: => Int, inline dec: Int => Unit)(n: Int): Unit =
      if n > 0 then
        val m = i
        val h = if m < n then m else n
        dec(h)

    inline def step[X >: Alt[Err]](a: String, inline i: => Int, inline inc: () => Unit)(using Lb[X]): Char =
      val ii = i
      if ii < a.length then
        inc()
        a(ii)
      else
        boundary.break(Err.or(s"Array at end, position ${a.length}"))
  }

  final class CyclingBytes(val source: RotatingBuffer[Byte]) {
    var bkw: Array[Byte] = null
    var cur: Array[Byte] = null
    var fwd: Array[Byte] = null

    var ib0 = 0
    var ibN = 0
    var ic0 = 0
    var icN = 0
    var if0 = 0
    var ifN = 0

    var consumed = 0L
    var index = 0

    val ma: Mu[Array[Byte] Or Unit] = Mu(Alt.unit)
    val miv: Mu.T[Iv] = Mu(Iv(0, 0))

    def remaining(): Long = ???
    def has(n: Int): Boolean = ???
    def skip(n: Int): Int = ???
    def undo(n: Int): Int = ???
    def advance(): Unit = ???
  }
  object CyclingByteIngester extends CompleteIngester[CyclingBytes, Byte] {
    inline def remaining(a: CyclingBytes): Long = a.remaining()
    inline def has(a: CyclingBytes)(n: Int): Boolean = a.has(n)
    inline def nonEmpty(a: CyclingBytes): Boolean = a.has(1)
    inline def skip(a: CyclingBytes, inline inc: Int => Unit)(n: Int): Unit = if n > 0 then inc(a.skip(n))
    inline def undo(a: CyclingBytes, inline dec: Int => Unit)(n: Int): Unit = if n > 0 then dec(a.undo(n))
    inline def step[X >: Alt[Err]](a: CyclingBytes, inline inc: () => Unit)(using Lb[X]): Byte =
      if a.index >= 0 then
        if a.index >= a.icN then
          a.advance()
          if a.index >= a.icN then boundary.break(Err.or(s"Input at end, position ${a.consumed}"))
        val i = a.index
        a.index += 1
        inc()
        a.cur(i)
      else
        val k = a.ibN + a.index
        a.index += 1
        inc()
        a.bkw(k)
  }

  /*
  trait MultiArrayByteIngester extends CompleteIngester[MultiArrayChannel, Byte] {
    inline def remaining(a: MultiArrayChannel): Long = a.size - a.position
    inline def has(a: MultiArrayChannel)(n: Int): Boolean = n <= (a.size - a.position)
    inline def nonEmpty(a: MultiArrayChannel): Boolean = a.position < a.size

    inline def skip(a: MultiArrayChannel, inline inc: Int => Unit)(n: Int): Unit =
      if n > 0 then
        val m = a.size - a.position
        val h =
          if m < n then
            a.position(a.size)
            m.toInt
          else
            a.position(a.position + n)
            n
        inc(h)

    inline def undo(a: MultiArrayChannel, inline dec: Int => Unit)(n: Int): Unit =
      if n > 0 then
        val m = a.position
        val h =
          if m < n then
            a.position(0)
            m.toInt
          else
            a.position(m - n)
            n
        dec(h)

    inline def step[X >: Alt[Err]](a: MultiArrayChannel, inline inc: () => Unit)(using Lb[X]): Byte =
      val x = a.readOne()
      if x < 0 then boundary.break(Err.or(s"Input at end, position ${a.size}"))
      else (x & 0xFF).toByte
  }
  */
}
