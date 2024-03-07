// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024 Rex Kerr

package kse.eio

import java.nio.{ByteBuffer, CharBuffer}

import scala.util.boundary
import scala.util.boundary.{Label => Lb}

import kse.basics.{given, _}
import kse.basics.intervals.{given, _}

import kse.flow.{given, _}

sealed trait Ingester[A, I, E] {
  inline def remaining(a: A, inline i: => I): Long
  inline def has(a: A, inline i: => I)(n: Int): Boolean
  inline def nonEmpty(a: A, inline i: => I): Boolean

  inline def skip(a: A, inline i: => I, inline inc: Int => Unit)(n: Int): Int

  transparent inline def step[X >: Alt[Err]](a: A, inline i: => I, inline inc: () => Unit)(using Lb[X]): E
}
object Ingester {
  sealed trait CompleteIngester[A, E] extends Ingester[A, Unit, E] {
    final inline def remaining(a: A, inline i: => Unit): Long = remaining(a)
    inline def remaining(a: A): Long

    final inline def has(a: A, inline i: => Unit)(n: Int): Boolean = has(a)(n)
    inline def has(a: A)(n: Int): Boolean

    final inline def nonEmpty(a: A, inline i: => Unit): Boolean = nonEmpty(a)
    inline def nonEmpty(a: A): Boolean

    final inline def skip(a: A, inline i: => Unit, inline inc: Int => Unit)(n: Int): Int = skip(a, inc)(n)
    inline def skip(a: A, inline inc: Int => Unit)(n: Int): Int

    final transparent inline def step[X >: Alt[Err]](a: A, inline i: => Unit, inline inc: () => Unit)(using Lb[X]): E = step[X](a, inc)
    transparent inline def step[X >: Alt[Err]](a: A, inline inc: () => Unit)(using Lb[X]): E
  }

  sealed trait ArrayIngester[A] extends Ingester[Array[A], Int, A] {
    inline def remaining(a: Array[A], inline i: => Int) = a.length - i
    inline def has(a: Array[A], inline i: => Int)(n: Int) = n <= 0 || i < a.length - n
    inline def nonEmpty(a: Array[A], inline i: => Int) = i < a.length

    inline def skip(a: Array[A], inline i: => Int, inline inc: Int => Unit)(n: Int): Int =
      if n <= 0 then 0
      else
        val m = a.length - i
        if m < n then
          inc(m)
          m
        else
          inc(n)
          n

    transparent inline def step[X >: Alt[Err]](a: Array[A], inline i: => Int, inline inc: () => Unit)(using Lb[X]): A =
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

    inline def skip(a: B, inline inc: Int => Unit)(n: Int): Int =
      if n > a.remaining() then
        val ans = a.remaining()
        a.position(a.limit)
        inc(ans)
        ans
      else if n > 0 then
        a.position(a.position + n)
        inc(n)
        n
      else 0
  }
  object ByteBufferIngester extends BufferIngester[ByteBuffer, Byte] {
    transparent inline def step[X >: Alt[Err]](a: ByteBuffer, inline inc: () => Unit)(using Lb[X]): Byte =
      if a.hasRemaining() then
        inc()
        a.get()
      else
        boundary.break(Err.or(s"Buffer at end, position ${a.limit}"))
  }
  object CharBufferIngester extends BufferIngester[CharBuffer, Char] {
    transparent inline def step[X >: Alt[Err]](a: CharBuffer, inline inc: () => Unit)(using Lb[X]): Char =
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

    inline def skip(a: String, inline i: => Int, inline inc: Int => Unit)(n: Int): Int =
      if n <= 0 then 0
      else
        val m = a.length - i
        if m < n then
          inc(m)
          m
        else
          inc(n)
          n

    transparent inline def step[X >: Alt[Err]](a: String, inline i: => Int, inline inc: () => Unit)(using Lb[X]): Char =
      val ii = i
      if ii < a.length then
        inc()
        a(ii)
      else
        boundary.break(Err.or(s"Array at end, position ${a.length}"))
  }
}
