// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024 Rex Kerr

package kse.eio

import java.nio.ByteBuffer

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
  object ByteBufferIngester extends Ingester[ByteBuffer, Int, Byte] {
    inline def remaining(a: ByteBuffer, inline i: => Int) = a.remaining()
    inline def has(a: ByteBuffer, inline i: => Int)(n: Int) = a.remaining() >= n
    inline def nonEmpty(a: ByteBuffer, inline i: => Int): Boolean = a.hasRemaining()

    inline def skip(a: ByteBuffer, inline i: => Int, inline inc: Int => Unit)(n: Int): Int =
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

    transparent inline def step[X >: Alt[Err]](a: ByteBuffer, inline i: => Int, inline inc: () => Unit)(using Lb[X]): Byte =
      if a.hasRemaining() then
        inc()
        a.get()
      else
        boundary.break(Err.or(s"Buffer at end, position ${a.limit}"))
  }

  object ByteArrayIngester extends Ingester[Array[Byte], Int, Byte] {
    inline def remaining(a: Array[Byte], inline i: => Int) = a.length - i
    inline def has(a: Array[Byte], inline i: => Int)(n: Int) = n <= 0 || i < a.length - n
    inline def nonEmpty(a: Array[Byte], inline i: => Int) = i < a.length

    inline def skip(a: Array[Byte], inline i: => Int, inline inc: Int => Unit)(n: Int): Int =
      if n <= 0 then 0
      else
        val m = a.length - i
        if m < n then
          inc(m)
          m
        else
          inc(n)
          n

    transparent inline def step[X >: Alt[Err]](a: Array[Byte], inline i: => Int, inline inc: () => Unit)(using Lb[X]): Byte =
      val ii = i
      if ii < a.length then
        inc()
        a(ii)
      else
        boundary.break(Err.or(s"Array at end, position ${a.length}"))
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
