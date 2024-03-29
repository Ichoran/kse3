// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024 Rex Kerr

package kse.maths.stringmaths

import scala.util.boundary

import kse.basics._
import kse.basics.intervals._

object SemanticOrder {
  enum Kind {
    case Empty, Digits, Text, Other
  }

  inline def determineKind(c: Char): Kind =
    if java.lang.Character.isDigit(c) then Kind.Digits
    else if java.lang.Character.isLetter(c) then Kind.Text
    else Kind.Other

  inline def determineKind(s: String, i0: Int, iN: Int): Kind =
    if i0 >= iN || i0 < 0 || i0 >= s.length then Kind.Empty
    else determineKind(s.charAt(i0))

  inline def findPiece(s: String, iv: Iv)(inline callback: Kind => Unit): Iv =
    var i = iv.i0
    val j = java.lang.Math.min(s.length, iv.iN)
    val kind = determineKind(s, i, j)
    callback(kind)
    kind match
      case Kind.Empty  =>
      case Kind.Digits =>
        i += 1
        while i < j && java.lang.Character.isDigit(s `charAt` i) do
          i += 1
      case Kind.Text =>
        i += 1
        while i < j && java.lang.Character.isLetter(s `charAt` i) do
          i += 1
      case Kind.Other =>
        i += 1
        while i < j && { val c = s `charAt` i; !java.lang.Character.isDigit(c) && !java.lang.Character.isLetter(c)} do
          i += 1
    Iv(iv.i0, i)

  inline def digitsCompare(s: String, i: Int)(t: String, j: Int)(inline iinc: Int => Unit, inline jinc: Int => Unit): Int =
    var ans = 0
    var i0 = i
    var iv = 0
    var iN = i
    boundary:
      while iN < s.length do
        iv = java.lang.Character.digit(s.charAt(iN), 10)
        if iv < 0 then boundary.break()
        iN += 1
        if iv > 0 then boundary.break()
        i0 += 1
    var j0 = j
    var jv = 0
    var jN = j
    boundary:
      while jN < t.length do
        jv = java.lang.Character.digit(t.charAt(jN), 10)
        if jv < 0 then boundary.break()
        jN += 1
        if jv > 0 then boundary.break()
        j0 += 1
    ???
  inline def intCompare(s: String, i: Int)(t: String, j: Int)(inline iinc: Int => Unit, inline jinc: Int => Unit): Int =
    var sneg = false
    var si = i
    if i < s.length then s.charAt(i) match
      case '+' => si += 1; iinc(1)
      case '-' => si += 1; iinc(1); sneg = true
      case _   =>
    var tneg = false
    var tj = j
    if j < t.length then t.charAt(j) match
      case '+' => tj += 1; jinc(1)
      case '-' => tj += 1; jinc(1); tneg = true
      case _   =>
    ???
}

object StringInt {
}

object SpokenNumber {
}
