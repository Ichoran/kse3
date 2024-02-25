// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024 Rex Kerr

package kse.maths.stringmaths

import scala.util.boundary

object SemanticOrder {
  inline def uintCompare(s: String, i: Int)(t: String, j: Int)(inline iinc: Int => Unit, inline jinc: Int => Unit): Int =
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
}

object StringInt {
}

object SpokenNumber {
}
