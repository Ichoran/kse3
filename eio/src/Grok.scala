// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2024 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio.grok

import scala.util.boundary
import scala.compiletime.{uninitialized, erasedValue}

import kse.basics._
import kse.flow._
import kse.maths._


/*
trait Grok {
  def Z[E >: Alt[Err]](using Lb[E])
}
*/

trait Grok {
  type Elt <: Byte | Char
  inline def has(i: Int): Boolean
  inline def peek: Elt
  inline def peekC: Char = inline peek match
    case b: Byte => b.toChar
    case c: Char => c
  inline def advance: Boolean
  inline def next[E >: Alt[Err]](using boundary.Label[E]): Elt =
    if advance then peek
    else boundary.break(Err.or("at end of input"))
  inline def advanceIf(inline p: Elt => Boolean): Boolean
  inline def advanceWhile(inline p: Elt => Boolean): Int =
    var i = 0
    while advanceIf(p) do i += 1
    i

  inline def ensure[E >: Alt[Err]](i: Int)(using boundary.Label[E]): Unit =
    if !has(i) then
      boundary.break(Err.or("at end of input")) 

  /*
  inline def Z[E >: Alt[Err]](using boundary.Label[E]): Boolean =
    ensure(1)
    peekC match
      case 't' | 'T' =>
        if !advance then
          true
        else if delim(peek) then
          advance
          true
        else if !use(3) then boundary.break(Err.or(s"out of input while trying to grok 'true'"))
        else if !verifyC(c => c == 'r' || c == 'R') || !verifyC(c => c == 'u' || c == 'U') || !verifyC
      case 'f' | 'F' =>
      case 
    inline peek match
      case b: Byte =>
      case c: Char =>
        if delim(c) then boundary.break(Err.or("token end instead of boolean"))
        val cc 

  inline def B[E >: Alt[Err]](using boundary.Label[E]): Byte
  inline def uB[E >: Alt[Err]](using boundary.Label[E]): UByte
  inline def xB[E >: Alt[Err]](using boundary.Label[E]): Byte
  inline def S[E >: Alt[Err]](using boundary.Label[E]): Short
  inline def xS[E >: Alt[Err]](using boundary.Label[E]): Short
  inline def C[E >: Alt[Err]](using boundary.Label[E]): Char
  inline def I[E >: Alt[Err]](using boundary.Label[E]): Int
  inline def uI[E >: Alt[Err]](using boundary.Label[E]): UInt
  inline def xI[E >: Alt[Err]](using boundary.Label[E]): Int
  inline def L[E >: Alt[Err]](using boundary.Label[E]): Long
  inline def uL[E >: Alt[Err]](using boundary.Label[E]): ULong
  inline def xL[E >: Alt[Err]](using boundary.Label[E]): Long
  inline def skip[E >: Alt[Err]](using boundary.Label[E]): Unit
  */
  inline def tok[E >: Alt[Err]](using boundary.Label[E]): String
}
object Grok {
  class Str(content: String, delim: Char => Boolean, multi: Boolean, i0: Int = 0, iN: Int = Int.MaxValue) extends Grok {
    type Elt = Char

    private val n = if content.length < iN then content.length else if iN > 0 then iN else 0
    var position = if i0 < 0 then 0 else if i0 > n then n else i0

    inline def has(i: Int): Boolean = n - position >= i
    inline def peek: Char = content.charAt(position)
    inline def advance: Boolean =
      if position < n then
        position += 1
        true
      else false
    inline def advanceIf(inline p: Char => Boolean): Boolean =
      if position < n && p(peek) then
        position += 1
        true
      else false

    inline def tok[E >: Alt[Err]](using boundary.Label[E]): String =
      ensure(1)
      val i0 = position
      advanceWhile(c => !delim(c))
      val ans = content.substring(i0, position)
      advance
      ans

    inline def skip[E >: Alt[Err]](using boundary.Label[E]): Unit =
      ensure(1)
      advanceWhile(c => !delim(c))
      advance
  }
}

extension (s: String)
  inline def grok[A](inline f: boundary.Label[A Or Err] ?=> (Grok.Str => A)): A Or Err = Or.Ret[A, Err]:
    f(Grok.Str(s, _ == ' ', true, 0, s.length))
