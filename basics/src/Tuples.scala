// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2021-23, 2025 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.basics


//import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.compiletime.{constValue, summonFrom}

import scala.annotation.targetName
import scala.NamedTuple.{NamedTuple => NTup}


extension [A](a: A) {
  /** Make this value into a tuple. */
  inline def tup(): Tuple1[A] = Tuple1(a)

  /** Make a tuple with this value and another.  Equivalent to `a -> z`. */
  inline infix def tup[Z](z: Z): (A, Z) = (a, z)

  /** Make a tuple by applying a function to this value, and keeping both this value and the result. */
  inline def tupWith[Z](inline zf: A => Z): (A, Z) = (a, zf(a))

  /** Make this value into a named tuple */
  transparent inline def ntup[L <: LabelStr](l: L) = NamedTuple[Tuple1[L], Tuple1[A]](Tuple1(a))
}


extension (tup0: EmptyTuple) {
  transparent inline def lens = OpaqueTupleLenses.Tup0

  inline infix def tup[Z](z: Z): Tuple1[Z] = Tuple1(z)
}


extension [A](tup1: Tuple1[A]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[N <: 0 | 1] = OpaqueTupleLenses.Tup1.wrap(tup1)[N]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA](inline a: A => AA): Tuple1[AA] = Tuple1(a(tup1._1))

  /** Pass the values of this tuple into a 2-argument function and return the result. */
  inline def merge[Z](inline z: A => Z): Z = z(tup1._1)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, Z) =
    (tup1._1, z)  

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: A => Z): (A, Z) =
    (tup1._1, z(tup1._1))
}


extension [A, B](tup2: (A, B)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2] = OpaqueTupleLenses.Tup2.wrap(tup2)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB](inline a: A => AA, inline b: B => BB): (AA, BB) = (a(tup2._1), b(tup2._2))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B) => Z) = (z(tup2._1), z(tup2._2))

  /** Pass the values of this tuple into a 2-argument function and return the result. */
  inline def merge[Z](inline z: (A, B) => Z): Z = z(tup2._1, tup2._2)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B)](op: (Z, Z) => Z) = op(tup2._1, tup2._2)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, Z) =
    (tup2._1, tup2._2, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B) => Z): (A, B, Z) =
    (tup2._1, tup2._2, z(tup2._1, tup2._2))
}


extension [A, B, C](tup3: (A, B, C)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3] = OpaqueTupleLenses.Tup3.wrap(tup3)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC](inline a: A => AA, inline b: B => BB, inline c: C => CC): (AA, BB, CC) = (a(tup3._1), b(tup3._2), c(tup3._3))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C) => Z) = (z(tup3._1), z(tup3._2), z(tup3._3))

  /** Pass the values of this tuple into a 3-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C) => Z): Z = z(tup3._1, tup3._2, tup3._3)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C)](op: (Z, Z) => Z) = op(op(tup3._1, tup3._2), tup3._3)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, Z) =
    (tup3._1, tup3._2, tup3._3, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C) => Z): (A, B, C, Z) =
    (tup3._1, tup3._2, tup3._3, z(tup3._1, tup3._2, tup3._3))
}


extension [A, B, C, D](tup4: (A, B, C, D)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4] = OpaqueTupleLenses.Tup4.wrap(tup4)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD): (AA, BB, CC, DD) = (a(tup4._1), b(tup4._2), c(tup4._3), d(tup4._4))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D) => Z) = (z(tup4._1), z(tup4._2), z(tup4._3), z(tup4._4))

  /** Pass the values of this tuple into a 4-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D) => Z): Z = z(tup4._1, tup4._2, tup4._3, tup4._4)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D)](op: (Z, Z) => Z) = op(op(op(tup4._1, tup4._2), tup4._3), tup4._4)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, Z) =
    (tup4._1, tup4._2, tup4._3, tup4._4, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D) => Z): (A, B, C, D, Z) =
    (tup4._1, tup4._2, tup4._3, tup4._4, z(tup4._1, tup4._2, tup4._3, tup4._4))
}


extension [A, B, C, D, E](tup5: (A, B, C, D, E)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5] = OpaqueTupleLenses.Tup5.wrap(tup5)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE): (AA, BB, CC, DD, EE) = (a(tup5._1), b(tup5._2), c(tup5._3), d(tup5._4), e(tup5._5))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E) => Z) = (z(tup5._1), z(tup5._2), z(tup5._3), z(tup5._4), z(tup5._5))

  /** Pass the values of this tuple into a 5-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E) => Z): Z = z(tup5._1, tup5._2, tup5._3, tup5._4, tup5._5)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E)](op: (Z, Z) => Z) = op(op(op(op(tup5._1, tup5._2), tup5._3), tup5._4), tup5._5)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, Z) =
    (tup5._1, tup5._2, tup5._3, tup5._4, tup5._5, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E) => Z): (A, B, C, D, E, Z) =
    (tup5._1, tup5._2, tup5._3, tup5._4, tup5._5, z(tup5._1, tup5._2, tup5._3, tup5._4, tup5._5))
}


extension [A, B, C, D, E, F](tup6: (A, B, C, D, E, F)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6] = OpaqueTupleLenses.Tup6.wrap(tup6)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF): (AA, BB, CC, DD, EE, FF) = (a(tup6._1), b(tup6._2), c(tup6._3), d(tup6._4), e(tup6._5), f(tup6._6))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F) => Z) = (z(tup6._1), z(tup6._2), z(tup6._3), z(tup6._4), z(tup6._5), z(tup6._6))

  /** Pass the values of this tuple into a 6-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F) => Z): Z = z(tup6._1, tup6._2, tup6._3, tup6._4, tup6._5, tup6._6)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F)](op: (Z, Z) => Z) = op(op(op(op(op(tup6._1, tup6._2), tup6._3), tup6._4), tup6._5), tup6._6)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, Z) =
    (tup6._1, tup6._2, tup6._3, tup6._4, tup6._5, tup6._6, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F) => Z): (A, B, C, D, E, F, Z) =
    (tup6._1, tup6._2, tup6._3, tup6._4, tup6._5, tup6._6, z(tup6._1, tup6._2, tup6._3, tup6._4, tup6._5, tup6._6))
}


extension [A, B, C, D, E, F, G](tup7: (A, B, C, D, E, F, G)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7] = OpaqueTupleLenses.Tup7.wrap(tup7)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG): (AA, BB, CC, DD, EE, FF, GG) = (a(tup7._1), b(tup7._2), c(tup7._3), d(tup7._4), e(tup7._5), f(tup7._6), g(tup7._7))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G) => Z) = (z(tup7._1), z(tup7._2), z(tup7._3), z(tup7._4), z(tup7._5), z(tup7._6), z(tup7._7))

  /** Pass the values of this tuple into a 7-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G) => Z): Z = z(tup7._1, tup7._2, tup7._3, tup7._4, tup7._5, tup7._6, tup7._7)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G)](op: (Z, Z) => Z) = op(op(op(op(op(op(tup7._1, tup7._2), tup7._3), tup7._4), tup7._5), tup7._6), tup7._7)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, Z) =
    (tup7._1, tup7._2, tup7._3, tup7._4, tup7._5, tup7._6, tup7._7, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G) => Z): (A, B, C, D, E, F, G, Z) =
    (tup7._1, tup7._2, tup7._3, tup7._4, tup7._5, tup7._6, tup7._7, z(tup7._1, tup7._2, tup7._3, tup7._4, tup7._5, tup7._6, tup7._7))
}


extension [A, B, C, D, E, F, G, H](tup8: (A, B, C, D, E, F, G, H)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8] = OpaqueTupleLenses.Tup8.wrap(tup8)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH): (AA, BB, CC, DD, EE, FF, GG, HH) = (a(tup8._1), b(tup8._2), c(tup8._3), d(tup8._4), e(tup8._5), f(tup8._6), g(tup8._7), h(tup8._8))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H) => Z) = (z(tup8._1), z(tup8._2), z(tup8._3), z(tup8._4), z(tup8._5), z(tup8._6), z(tup8._7), z(tup8._8))

  /** Pass the values of this tuple into a 8-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H) => Z): Z = z(tup8._1, tup8._2, tup8._3, tup8._4, tup8._5, tup8._6, tup8._7, tup8._8)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(tup8._1, tup8._2), tup8._3), tup8._4), tup8._5), tup8._6), tup8._7), tup8._8)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, Z) =
    (tup8._1, tup8._2, tup8._3, tup8._4, tup8._5, tup8._6, tup8._7, tup8._8, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H) => Z): (A, B, C, D, E, F, G, H, Z) =
    (tup8._1, tup8._2, tup8._3, tup8._4, tup8._5, tup8._6, tup8._7, tup8._8, z(tup8._1, tup8._2, tup8._3, tup8._4, tup8._5, tup8._6, tup8._7, tup8._8))
}


extension [A, B, C, D, E, F, G, H, I](tup9: (A, B, C, D, E, F, G, H, I)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9] = OpaqueTupleLenses.Tup9.wrap(tup9)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II): (AA, BB, CC, DD, EE, FF, GG, HH, II) = (a(tup9._1), b(tup9._2), c(tup9._3), d(tup9._4), e(tup9._5), f(tup9._6), g(tup9._7), h(tup9._8), i(tup9._9))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I) => Z) = (z(tup9._1), z(tup9._2), z(tup9._3), z(tup9._4), z(tup9._5), z(tup9._6), z(tup9._7), z(tup9._8), z(tup9._9))

  /** Pass the values of this tuple into a 9-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I) => Z): Z = z(tup9._1, tup9._2, tup9._3, tup9._4, tup9._5, tup9._6, tup9._7, tup9._8, tup9._9)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(tup9._1, tup9._2), tup9._3), tup9._4), tup9._5), tup9._6), tup9._7), tup9._8), tup9._9)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, Z) =
    (tup9._1, tup9._2, tup9._3, tup9._4, tup9._5, tup9._6, tup9._7, tup9._8, tup9._9, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I) => Z): (A, B, C, D, E, F, G, H, I, Z) =
    (tup9._1, tup9._2, tup9._3, tup9._4, tup9._5, tup9._6, tup9._7, tup9._8, tup9._9, z(tup9._1, tup9._2, tup9._3, tup9._4, tup9._5, tup9._6, tup9._7, tup9._8, tup9._9))
}


extension [A, B, C, D, E, F, G, H, I, J](tup10: (A, B, C, D, E, F, G, H, I, J)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10] = OpaqueTupleLenses.Tup10.wrap(tup10)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ) = (a(tup10._1), b(tup10._2), c(tup10._3), d(tup10._4), e(tup10._5), f(tup10._6), g(tup10._7), h(tup10._8), i(tup10._9), j(tup10._10))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J) => Z) = (z(tup10._1), z(tup10._2), z(tup10._3), z(tup10._4), z(tup10._5), z(tup10._6), z(tup10._7), z(tup10._8), z(tup10._9), z(tup10._10))

  /** Pass the values of this tuple into a 10-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J) => Z): Z = z(tup10._1, tup10._2, tup10._3, tup10._4, tup10._5, tup10._6, tup10._7, tup10._8, tup10._9, tup10._10)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(tup10._1, tup10._2), tup10._3), tup10._4), tup10._5), tup10._6), tup10._7), tup10._8), tup10._9), tup10._10)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, Z) =
    (tup10._1, tup10._2, tup10._3, tup10._4, tup10._5, tup10._6, tup10._7, tup10._8, tup10._9, tup10._10, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J) => Z): (A, B, C, D, E, F, G, H, I, J, Z) =
    (tup10._1, tup10._2, tup10._3, tup10._4, tup10._5, tup10._6, tup10._7, tup10._8, tup10._9, tup10._10, z(tup10._1, tup10._2, tup10._3, tup10._4, tup10._5, tup10._6, tup10._7, tup10._8, tup10._9, tup10._10))
}


extension [A, B, C, D, E, F, G, H, I, J, K](tup11: (A, B, C, D, E, F, G, H, I, J, K)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11] = OpaqueTupleLenses.Tup11.wrap(tup11)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK) = (a(tup11._1), b(tup11._2), c(tup11._3), d(tup11._4), e(tup11._5), f(tup11._6), g(tup11._7), h(tup11._8), i(tup11._9), j(tup11._10), k(tup11._11))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K) => Z) = (z(tup11._1), z(tup11._2), z(tup11._3), z(tup11._4), z(tup11._5), z(tup11._6), z(tup11._7), z(tup11._8), z(tup11._9), z(tup11._10), z(tup11._11))

  /** Pass the values of this tuple into a 11-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K) => Z): Z = z(tup11._1, tup11._2, tup11._3, tup11._4, tup11._5, tup11._6, tup11._7, tup11._8, tup11._9, tup11._10, tup11._11)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(tup11._1, tup11._2), tup11._3), tup11._4), tup11._5), tup11._6), tup11._7), tup11._8), tup11._9), tup11._10), tup11._11)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, Z) =
    (tup11._1, tup11._2, tup11._3, tup11._4, tup11._5, tup11._6, tup11._7, tup11._8, tup11._9, tup11._10, tup11._11, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K) => Z): (A, B, C, D, E, F, G, H, I, J, K, Z) =
    (tup11._1, tup11._2, tup11._3, tup11._4, tup11._5, tup11._6, tup11._7, tup11._8, tup11._9, tup11._10, tup11._11, z(tup11._1, tup11._2, tup11._3, tup11._4, tup11._5, tup11._6, tup11._7, tup11._8, tup11._9, tup11._10, tup11._11))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L](tup12: (A, B, C, D, E, F, G, H, I, J, K, L)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12] = OpaqueTupleLenses.Tup12.wrap(tup12)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL) = (a(tup12._1), b(tup12._2), c(tup12._3), d(tup12._4), e(tup12._5), f(tup12._6), g(tup12._7), h(tup12._8), i(tup12._9), j(tup12._10), k(tup12._11), l(tup12._12))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L) => Z) = (z(tup12._1), z(tup12._2), z(tup12._3), z(tup12._4), z(tup12._5), z(tup12._6), z(tup12._7), z(tup12._8), z(tup12._9), z(tup12._10), z(tup12._11), z(tup12._12))

  /** Pass the values of this tuple into a 12-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L) => Z): Z = z(tup12._1, tup12._2, tup12._3, tup12._4, tup12._5, tup12._6, tup12._7, tup12._8, tup12._9, tup12._10, tup12._11, tup12._12)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(tup12._1, tup12._2), tup12._3), tup12._4), tup12._5), tup12._6), tup12._7), tup12._8), tup12._9), tup12._10), tup12._11), tup12._12)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, Z) =
    (tup12._1, tup12._2, tup12._3, tup12._4, tup12._5, tup12._6, tup12._7, tup12._8, tup12._9, tup12._10, tup12._11, tup12._12, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, Z) =
    (tup12._1, tup12._2, tup12._3, tup12._4, tup12._5, tup12._6, tup12._7, tup12._8, tup12._9, tup12._10, tup12._11, tup12._12, z(tup12._1, tup12._2, tup12._3, tup12._4, tup12._5, tup12._6, tup12._7, tup12._8, tup12._9, tup12._10, tup12._11, tup12._12))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L, M](tup13: (A, B, C, D, E, F, G, H, I, J, K, L, M)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13] = OpaqueTupleLenses.Tup13.wrap(tup13)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL, inline m: M => MM): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM) = (a(tup13._1), b(tup13._2), c(tup13._3), d(tup13._4), e(tup13._5), f(tup13._6), g(tup13._7), h(tup13._8), i(tup13._9), j(tup13._10), k(tup13._11), l(tup13._12), m(tup13._13))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L | M) => Z) = (z(tup13._1), z(tup13._2), z(tup13._3), z(tup13._4), z(tup13._5), z(tup13._6), z(tup13._7), z(tup13._8), z(tup13._9), z(tup13._10), z(tup13._11), z(tup13._12), z(tup13._13))

  /** Pass the values of this tuple into a 13-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Z): Z = z(tup13._1, tup13._2, tup13._3, tup13._4, tup13._5, tup13._6, tup13._7, tup13._8, tup13._9, tup13._10, tup13._11, tup13._12, tup13._13)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L | M)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(op(tup13._1, tup13._2), tup13._3), tup13._4), tup13._5), tup13._6), tup13._7), tup13._8), tup13._9), tup13._10), tup13._11), tup13._12), tup13._13)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, Z) =
    (tup13._1, tup13._2, tup13._3, tup13._4, tup13._5, tup13._6, tup13._7, tup13._8, tup13._9, tup13._10, tup13._11, tup13._12, tup13._13, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, Z) =
    (tup13._1, tup13._2, tup13._3, tup13._4, tup13._5, tup13._6, tup13._7, tup13._8, tup13._9, tup13._10, tup13._11, tup13._12, tup13._13, z(tup13._1, tup13._2, tup13._3, tup13._4, tup13._5, tup13._6, tup13._7, tup13._8, tup13._9, tup13._10, tup13._11, tup13._12, tup13._13))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N](tup14: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14] = OpaqueTupleLenses.Tup14.wrap(tup14)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL, inline m: M => MM, inline n: N => NN): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN) = (a(tup14._1), b(tup14._2), c(tup14._3), d(tup14._4), e(tup14._5), f(tup14._6), g(tup14._7), h(tup14._8), i(tup14._9), j(tup14._10), k(tup14._11), l(tup14._12), m(tup14._13), n(tup14._14))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L | M | N) => Z) = (z(tup14._1), z(tup14._2), z(tup14._3), z(tup14._4), z(tup14._5), z(tup14._6), z(tup14._7), z(tup14._8), z(tup14._9), z(tup14._10), z(tup14._11), z(tup14._12), z(tup14._13), z(tup14._14))

  /** Pass the values of this tuple into a 14-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z): Z = z(tup14._1, tup14._2, tup14._3, tup14._4, tup14._5, tup14._6, tup14._7, tup14._8, tup14._9, tup14._10, tup14._11, tup14._12, tup14._13, tup14._14)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L | M | N)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(op(op(tup14._1, tup14._2), tup14._3), tup14._4), tup14._5), tup14._6), tup14._7), tup14._8), tup14._9), tup14._10), tup14._11), tup14._12), tup14._13), tup14._14)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z) =
    (tup14._1, tup14._2, tup14._3, tup14._4, tup14._5, tup14._6, tup14._7, tup14._8, tup14._9, tup14._10, tup14._11, tup14._12, tup14._13, tup14._14, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z) =
    (tup14._1, tup14._2, tup14._3, tup14._4, tup14._5, tup14._6, tup14._7, tup14._8, tup14._9, tup14._10, tup14._11, tup14._12, tup14._13, tup14._14, z(tup14._1, tup14._2, tup14._3, tup14._4, tup14._5, tup14._6, tup14._7, tup14._8, tup14._9, tup14._10, tup14._11, tup14._12, tup14._13, tup14._14))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](tup15: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15] = OpaqueTupleLenses.Tup15.wrap(tup15)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL, inline m: M => MM, inline n: N => NN, inline o: O => OO): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO) = (a(tup15._1), b(tup15._2), c(tup15._3), d(tup15._4), e(tup15._5), f(tup15._6), g(tup15._7), h(tup15._8), i(tup15._9), j(tup15._10), k(tup15._11), l(tup15._12), m(tup15._13), n(tup15._14), o(tup15._15))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O) => Z) = (z(tup15._1), z(tup15._2), z(tup15._3), z(tup15._4), z(tup15._5), z(tup15._6), z(tup15._7), z(tup15._8), z(tup15._9), z(tup15._10), z(tup15._11), z(tup15._12), z(tup15._13), z(tup15._14), z(tup15._15))

  /** Pass the values of this tuple into a 15-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z): Z = z(tup15._1, tup15._2, tup15._3, tup15._4, tup15._5, tup15._6, tup15._7, tup15._8, tup15._9, tup15._10, tup15._11, tup15._12, tup15._13, tup15._14, tup15._15)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(op(op(op(tup15._1, tup15._2), tup15._3), tup15._4), tup15._5), tup15._6), tup15._7), tup15._8), tup15._9), tup15._10), tup15._11), tup15._12), tup15._13), tup15._14), tup15._15)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z) =
    (tup15._1, tup15._2, tup15._3, tup15._4, tup15._5, tup15._6, tup15._7, tup15._8, tup15._9, tup15._10, tup15._11, tup15._12, tup15._13, tup15._14, tup15._15, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z) =
    (tup15._1, tup15._2, tup15._3, tup15._4, tup15._5, tup15._6, tup15._7, tup15._8, tup15._9, tup15._10, tup15._11, tup15._12, tup15._13, tup15._14, tup15._15, z(tup15._1, tup15._2, tup15._3, tup15._4, tup15._5, tup15._6, tup15._7, tup15._8, tup15._9, tup15._10, tup15._11, tup15._12, tup15._13, tup15._14, tup15._15))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](tup16: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16] = OpaqueTupleLenses.Tup16.wrap(tup16)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL, inline m: M => MM, inline n: N => NN, inline o: O => OO, inline p: P => PP): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP) = (a(tup16._1), b(tup16._2), c(tup16._3), d(tup16._4), e(tup16._5), f(tup16._6), g(tup16._7), h(tup16._8), i(tup16._9), j(tup16._10), k(tup16._11), l(tup16._12), m(tup16._13), n(tup16._14), o(tup16._15), p(tup16._16))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P) => Z) = (z(tup16._1), z(tup16._2), z(tup16._3), z(tup16._4), z(tup16._5), z(tup16._6), z(tup16._7), z(tup16._8), z(tup16._9), z(tup16._10), z(tup16._11), z(tup16._12), z(tup16._13), z(tup16._14), z(tup16._15), z(tup16._16))

  /** Pass the values of this tuple into a 16-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z): Z = z(tup16._1, tup16._2, tup16._3, tup16._4, tup16._5, tup16._6, tup16._7, tup16._8, tup16._9, tup16._10, tup16._11, tup16._12, tup16._13, tup16._14, tup16._15, tup16._16)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(tup16._1, tup16._2), tup16._3), tup16._4), tup16._5), tup16._6), tup16._7), tup16._8), tup16._9), tup16._10), tup16._11), tup16._12), tup16._13), tup16._14), tup16._15), tup16._16)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z) =
    (tup16._1, tup16._2, tup16._3, tup16._4, tup16._5, tup16._6, tup16._7, tup16._8, tup16._9, tup16._10, tup16._11, tup16._12, tup16._13, tup16._14, tup16._15, tup16._16, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z) =
    (tup16._1, tup16._2, tup16._3, tup16._4, tup16._5, tup16._6, tup16._7, tup16._8, tup16._9, tup16._10, tup16._11, tup16._12, tup16._13, tup16._14, tup16._15, tup16._16, z(tup16._1, tup16._2, tup16._3, tup16._4, tup16._5, tup16._6, tup16._7, tup16._8, tup16._9, tup16._10, tup16._11, tup16._12, tup16._13, tup16._14, tup16._15, tup16._16))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](tup17: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17] = OpaqueTupleLenses.Tup17.wrap(tup17)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL, inline m: M => MM, inline n: N => NN, inline o: O => OO, inline p: P => PP, inline q: Q => QQ): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ) = (a(tup17._1), b(tup17._2), c(tup17._3), d(tup17._4), e(tup17._5), f(tup17._6), g(tup17._7), h(tup17._8), i(tup17._9), j(tup17._10), k(tup17._11), l(tup17._12), m(tup17._13), n(tup17._14), o(tup17._15), p(tup17._16), q(tup17._17))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q) => Z) = (z(tup17._1), z(tup17._2), z(tup17._3), z(tup17._4), z(tup17._5), z(tup17._6), z(tup17._7), z(tup17._8), z(tup17._9), z(tup17._10), z(tup17._11), z(tup17._12), z(tup17._13), z(tup17._14), z(tup17._15), z(tup17._16), z(tup17._17))

  /** Pass the values of this tuple into a 17-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z): Z = z(tup17._1, tup17._2, tup17._3, tup17._4, tup17._5, tup17._6, tup17._7, tup17._8, tup17._9, tup17._10, tup17._11, tup17._12, tup17._13, tup17._14, tup17._15, tup17._16, tup17._17)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(tup17._1, tup17._2), tup17._3), tup17._4), tup17._5), tup17._6), tup17._7), tup17._8), tup17._9), tup17._10), tup17._11), tup17._12), tup17._13), tup17._14), tup17._15), tup17._16), tup17._17)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z) =
    (tup17._1, tup17._2, tup17._3, tup17._4, tup17._5, tup17._6, tup17._7, tup17._8, tup17._9, tup17._10, tup17._11, tup17._12, tup17._13, tup17._14, tup17._15, tup17._16, tup17._17, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z) =
    (tup17._1, tup17._2, tup17._3, tup17._4, tup17._5, tup17._6, tup17._7, tup17._8, tup17._9, tup17._10, tup17._11, tup17._12, tup17._13, tup17._14, tup17._15, tup17._16, tup17._17, z(tup17._1, tup17._2, tup17._3, tup17._4, tup17._5, tup17._6, tup17._7, tup17._8, tup17._9, tup17._10, tup17._11, tup17._12, tup17._13, tup17._14, tup17._15, tup17._16, tup17._17))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](tup18: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18] = OpaqueTupleLenses.Tup18.wrap(tup18)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL, inline m: M => MM, inline n: N => NN, inline o: O => OO, inline p: P => PP, inline q: Q => QQ, inline r: R => RR): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR) = (a(tup18._1), b(tup18._2), c(tup18._3), d(tup18._4), e(tup18._5), f(tup18._6), g(tup18._7), h(tup18._8), i(tup18._9), j(tup18._10), k(tup18._11), l(tup18._12), m(tup18._13), n(tup18._14), o(tup18._15), p(tup18._16), q(tup18._17), r(tup18._18))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R) => Z) = (z(tup18._1), z(tup18._2), z(tup18._3), z(tup18._4), z(tup18._5), z(tup18._6), z(tup18._7), z(tup18._8), z(tup18._9), z(tup18._10), z(tup18._11), z(tup18._12), z(tup18._13), z(tup18._14), z(tup18._15), z(tup18._16), z(tup18._17), z(tup18._18))

  /** Pass the values of this tuple into a 18-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z): Z = z(tup18._1, tup18._2, tup18._3, tup18._4, tup18._5, tup18._6, tup18._7, tup18._8, tup18._9, tup18._10, tup18._11, tup18._12, tup18._13, tup18._14, tup18._15, tup18._16, tup18._17, tup18._18)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(tup18._1, tup18._2), tup18._3), tup18._4), tup18._5), tup18._6), tup18._7), tup18._8), tup18._9), tup18._10), tup18._11), tup18._12), tup18._13), tup18._14), tup18._15), tup18._16), tup18._17), tup18._18)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z) =
    (tup18._1, tup18._2, tup18._3, tup18._4, tup18._5, tup18._6, tup18._7, tup18._8, tup18._9, tup18._10, tup18._11, tup18._12, tup18._13, tup18._14, tup18._15, tup18._16, tup18._17, tup18._18, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z) =
    (tup18._1, tup18._2, tup18._3, tup18._4, tup18._5, tup18._6, tup18._7, tup18._8, tup18._9, tup18._10, tup18._11, tup18._12, tup18._13, tup18._14, tup18._15, tup18._16, tup18._17, tup18._18, z(tup18._1, tup18._2, tup18._3, tup18._4, tup18._5, tup18._6, tup18._7, tup18._8, tup18._9, tup18._10, tup18._11, tup18._12, tup18._13, tup18._14, tup18._15, tup18._16, tup18._17, tup18._18))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](tup19: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19] = OpaqueTupleLenses.Tup19.wrap(tup19)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL, inline m: M => MM, inline n: N => NN, inline o: O => OO, inline p: P => PP, inline q: Q => QQ, inline r: R => RR, inline s: S => SS): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS) = (a(tup19._1), b(tup19._2), c(tup19._3), d(tup19._4), e(tup19._5), f(tup19._6), g(tup19._7), h(tup19._8), i(tup19._9), j(tup19._10), k(tup19._11), l(tup19._12), m(tup19._13), n(tup19._14), o(tup19._15), p(tup19._16), q(tup19._17), r(tup19._18), s(tup19._19))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S) => Z) = (z(tup19._1), z(tup19._2), z(tup19._3), z(tup19._4), z(tup19._5), z(tup19._6), z(tup19._7), z(tup19._8), z(tup19._9), z(tup19._10), z(tup19._11), z(tup19._12), z(tup19._13), z(tup19._14), z(tup19._15), z(tup19._16), z(tup19._17), z(tup19._18), z(tup19._19))

  /** Pass the values of this tuple into a 19-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z): Z = z(tup19._1, tup19._2, tup19._3, tup19._4, tup19._5, tup19._6, tup19._7, tup19._8, tup19._9, tup19._10, tup19._11, tup19._12, tup19._13, tup19._14, tup19._15, tup19._16, tup19._17, tup19._18, tup19._19)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(tup19._1, tup19._2), tup19._3), tup19._4), tup19._5), tup19._6), tup19._7), tup19._8), tup19._9), tup19._10), tup19._11), tup19._12), tup19._13), tup19._14), tup19._15), tup19._16), tup19._17), tup19._18), tup19._19)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z) =
    (tup19._1, tup19._2, tup19._3, tup19._4, tup19._5, tup19._6, tup19._7, tup19._8, tup19._9, tup19._10, tup19._11, tup19._12, tup19._13, tup19._14, tup19._15, tup19._16, tup19._17, tup19._18, tup19._19, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z) =
    (tup19._1, tup19._2, tup19._3, tup19._4, tup19._5, tup19._6, tup19._7, tup19._8, tup19._9, tup19._10, tup19._11, tup19._12, tup19._13, tup19._14, tup19._15, tup19._16, tup19._17, tup19._18, tup19._19, z(tup19._1, tup19._2, tup19._3, tup19._4, tup19._5, tup19._6, tup19._7, tup19._8, tup19._9, tup19._10, tup19._11, tup19._12, tup19._13, tup19._14, tup19._15, tup19._16, tup19._17, tup19._18, tup19._19))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](tup20: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20] = OpaqueTupleLenses.Tup20.wrap(tup20)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS, TT](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL, inline m: M => MM, inline n: N => NN, inline o: O => OO, inline p: P => PP, inline q: Q => QQ, inline r: R => RR, inline s: S => SS, inline t: T => TT): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS, TT) = (a(tup20._1), b(tup20._2), c(tup20._3), d(tup20._4), e(tup20._5), f(tup20._6), g(tup20._7), h(tup20._8), i(tup20._9), j(tup20._10), k(tup20._11), l(tup20._12), m(tup20._13), n(tup20._14), o(tup20._15), p(tup20._16), q(tup20._17), r(tup20._18), s(tup20._19), t(tup20._20))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T) => Z) = (z(tup20._1), z(tup20._2), z(tup20._3), z(tup20._4), z(tup20._5), z(tup20._6), z(tup20._7), z(tup20._8), z(tup20._9), z(tup20._10), z(tup20._11), z(tup20._12), z(tup20._13), z(tup20._14), z(tup20._15), z(tup20._16), z(tup20._17), z(tup20._18), z(tup20._19), z(tup20._20))

  /** Pass the values of this tuple into a 20-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z): Z = z(tup20._1, tup20._2, tup20._3, tup20._4, tup20._5, tup20._6, tup20._7, tup20._8, tup20._9, tup20._10, tup20._11, tup20._12, tup20._13, tup20._14, tup20._15, tup20._16, tup20._17, tup20._18, tup20._19, tup20._20)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(tup20._1, tup20._2), tup20._3), tup20._4), tup20._5), tup20._6), tup20._7), tup20._8), tup20._9), tup20._10), tup20._11), tup20._12), tup20._13), tup20._14), tup20._15), tup20._16), tup20._17), tup20._18), tup20._19), tup20._20)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z) =
    (tup20._1, tup20._2, tup20._3, tup20._4, tup20._5, tup20._6, tup20._7, tup20._8, tup20._9, tup20._10, tup20._11, tup20._12, tup20._13, tup20._14, tup20._15, tup20._16, tup20._17, tup20._18, tup20._19, tup20._20, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z) =
    (tup20._1, tup20._2, tup20._3, tup20._4, tup20._5, tup20._6, tup20._7, tup20._8, tup20._9, tup20._10, tup20._11, tup20._12, tup20._13, tup20._14, tup20._15, tup20._16, tup20._17, tup20._18, tup20._19, tup20._20, z(tup20._1, tup20._2, tup20._3, tup20._4, tup20._5, tup20._6, tup20._7, tup20._8, tup20._9, tup20._10, tup20._11, tup20._12, tup20._13, tup20._14, tup20._15, tup20._16, tup20._17, tup20._18, tup20._19, tup20._20))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](tup21: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21] = OpaqueTupleLenses.Tup21.wrap(tup21)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS, TT, UU](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL, inline m: M => MM, inline n: N => NN, inline o: O => OO, inline p: P => PP, inline q: Q => QQ, inline r: R => RR, inline s: S => SS, inline t: T => TT, inline u: U => UU): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS, TT, UU) = (a(tup21._1), b(tup21._2), c(tup21._3), d(tup21._4), e(tup21._5), f(tup21._6), g(tup21._7), h(tup21._8), i(tup21._9), j(tup21._10), k(tup21._11), l(tup21._12), m(tup21._13), n(tup21._14), o(tup21._15), p(tup21._16), q(tup21._17), r(tup21._18), s(tup21._19), t(tup21._20), u(tup21._21))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U) => Z) = (z(tup21._1), z(tup21._2), z(tup21._3), z(tup21._4), z(tup21._5), z(tup21._6), z(tup21._7), z(tup21._8), z(tup21._9), z(tup21._10), z(tup21._11), z(tup21._12), z(tup21._13), z(tup21._14), z(tup21._15), z(tup21._16), z(tup21._17), z(tup21._18), z(tup21._19), z(tup21._20), z(tup21._21))

  /** Pass the values of this tuple into a 21-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z): Z = z(tup21._1, tup21._2, tup21._3, tup21._4, tup21._5, tup21._6, tup21._7, tup21._8, tup21._9, tup21._10, tup21._11, tup21._12, tup21._13, tup21._14, tup21._15, tup21._16, tup21._17, tup21._18, tup21._19, tup21._20, tup21._21)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(tup21._1, tup21._2), tup21._3), tup21._4), tup21._5), tup21._6), tup21._7), tup21._8), tup21._9), tup21._10), tup21._11), tup21._12), tup21._13), tup21._14), tup21._15), tup21._16), tup21._17), tup21._18), tup21._19), tup21._20), tup21._21)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z) =
    (tup21._1, tup21._2, tup21._3, tup21._4, tup21._5, tup21._6, tup21._7, tup21._8, tup21._9, tup21._10, tup21._11, tup21._12, tup21._13, tup21._14, tup21._15, tup21._16, tup21._17, tup21._18, tup21._19, tup21._20, tup21._21, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z) =
    (tup21._1, tup21._2, tup21._3, tup21._4, tup21._5, tup21._6, tup21._7, tup21._8, tup21._9, tup21._10, tup21._11, tup21._12, tup21._13, tup21._14, tup21._15, tup21._16, tup21._17, tup21._18, tup21._19, tup21._20, tup21._21, z(tup21._1, tup21._2, tup21._3, tup21._4, tup21._5, tup21._6, tup21._7, tup21._8, tup21._9, tup21._10, tup21._11, tup21._12, tup21._13, tup21._14, tup21._15, tup21._16, tup21._17, tup21._18, tup21._19, tup21._20, tup21._21))
}


extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](tup22: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22] = OpaqueTupleLenses.Tup22.wrap(tup22)[Zn]

  /** Create a new tuple by applying a different function to each position of this tuple */
  inline def ops[AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS, TT, UU, VV](inline a: A => AA, inline b: B => BB, inline c: C => CC, inline d: D => DD, inline e: E => EE, inline f: F => FF, inline g: G => GG, inline h: H => HH, inline i: I => II, inline j: J => JJ, inline k: K => KK, inline l: L => LL, inline m: M => MM, inline n: N => NN, inline o: O => OO, inline p: P => PP, inline q: Q => QQ, inline r: R => RR, inline s: S => SS, inline t: T => TT, inline u: U => UU, inline v: V => VV): (AA, BB, CC, DD, EE, FF, GG, HH, II, JJ, KK, LL, MM, NN, OO, PP, QQ, RR, SS, TT, UU, VV) = (a(tup22._1), b(tup22._2), c(tup22._3), d(tup22._4), e(tup22._5), f(tup22._6), g(tup22._7), h(tup22._8), i(tup22._9), j(tup22._10), k(tup22._11), l(tup22._12), m(tup22._13), n(tup22._14), o(tup22._15), p(tup22._16), q(tup22._17), r(tup22._18), s(tup22._19), t(tup22._20), u(tup22._21), v(tup22._22))

  /** Create a new tuple by applying the same function to each position of this tuple. */
  transparent inline def sameOp[Z](z: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V) => Z) = (z(tup22._1), z(tup22._2), z(tup22._3), z(tup22._4), z(tup22._5), z(tup22._6), z(tup22._7), z(tup22._8), z(tup22._9), z(tup22._10), z(tup22._11), z(tup22._12), z(tup22._13), z(tup22._14), z(tup22._15), z(tup22._16), z(tup22._17), z(tup22._18), z(tup22._19), z(tup22._20), z(tup22._21), z(tup22._22))

  /** Pass the values of this tuple into a 22-argument function and return the result. */
  inline def merge[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z): Z = z(tup22._1, tup22._2, tup22._3, tup22._4, tup22._5, tup22._6, tup22._7, tup22._8, tup22._9, tup22._10, tup22._11, tup22._12, tup22._13, tup22._14, tup22._15, tup22._16, tup22._17, tup22._18, tup22._19, tup22._20, tup22._21, tup22._22)

  /** Using a binary operation, reduce this tuple to a single value. */
  inline def reduce[Z >: (A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V)](op: (Z, Z) => Z) = op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(op(tup22._1, tup22._2), tup22._3), tup22._4), tup22._5), tup22._6), tup22._7), tup22._8), tup22._9), tup22._10), tup22._11), tup22._12), tup22._13), tup22._14), tup22._15), tup22._16), tup22._17), tup22._18), tup22._19), tup22._20), tup22._21), tup22._22)

  /** Make into a larger tuple by appending one element. */
  inline infix def tup[Z](z: Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z) =
    (tup22._1, tup22._2, tup22._3, tup22._4, tup22._5, tup22._6, tup22._7, tup22._8, tup22._9, tup22._10, tup22._11, tup22._12, tup22._13, tup22._14, tup22._15, tup22._16, tup22._17, tup22._18, tup22._19, tup22._20, tup22._21, tup22._22, z)

  /** Make into a larger tuple by applying a function to all elements. */
  inline def tupWith[Z](inline z: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z): (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z) =
    (tup22._1, tup22._2, tup22._3, tup22._4, tup22._5, tup22._6, tup22._7, tup22._8, tup22._9, tup22._10, tup22._11, tup22._12, tup22._13, tup22._14, tup22._15, tup22._16, tup22._17, tup22._18, tup22._19, tup22._20, tup22._21, tup22._22, z(tup22._1, tup22._2, tup22._3, tup22._4, tup22._5, tup22._6, tup22._7, tup22._8, tup22._9, tup22._10, tup22._11, tup22._12, tup22._13, tup22._14, tup22._15, tup22._16, tup22._17, tup22._18, tup22._19, tup22._20, tup22._21, tup22._22))
}


extension [La <: LabelStr, A](nup1: NTup[Tuple1[La], Tuple1[A]]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <:( La | "")] = OpaqueTupleLenses.Nup1.wrap[La, A, Lz](nup1)[labels.ExplicitTypeIndices.UpTo1[La, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: La](z: Z \ Lz) =
    NamedTuple.withNames(Tuple1(z.unlabel))[Tuple1[La]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lz), (A, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, Tuple1[La]]] match
      case "" => NamedTuple[(La, Lz), (A, Z)]((nup1.asInstanceOf[Tuple1[A]]._1, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, A, B](nup2: NTup[(La, Lb), (A, B)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | "")] = OpaqueTupleLenses.Nup2.wrap[La, Lb, A, B, Lz](nup2)[labels.ExplicitTypeIndices.UpTo2[La, Lb, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb)](z: Z \ Lz) =
    NamedTuple.withNames(nup2.asInstanceOf[OpaqueTupleLenses.Tup2[A, B, labels.ExplicitTypeIndices.UpTo1[La, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb), Lz, 0, labels.ExplicitTypeIndices.UpTo1[La, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lz), (A, B, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb)]] match
      case"" =>
        val tup2 = nup2.asInstanceOf[(A, B)]
        NamedTuple[(La, Lb, Lz), (A, B, Z)]((tup2._1, tup2._2, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}



extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, A, B, C](nup3: NTup[(La, Lb, Lc), (A, B, C)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | "")] = OpaqueTupleLenses.Nup3.wrap[La, Lb, Lc, A, B, C, Lz](nup3)[labels.ExplicitTypeIndices.UpTo3[La, Lb, Lc, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc)](z: Z \ Lz) =
    NamedTuple.withNames(nup3.asInstanceOf[OpaqueTupleLenses.Tup3[A, B, C, labels.ExplicitTypeIndices.UpTo2[La, Lb, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc), Lz, 0, labels.ExplicitTypeIndices.UpTo2[La, Lb, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Lz), (A, B, C, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc)]] match
      case"" =>
        val tup3 = nup3.asInstanceOf[(A, B, C)]
        NamedTuple[(La, Lb, Lc, Lz), (A, B, C, Z)]((tup3._1, tup3._2, tup3._3, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, A, B, C, D](nup4: NTup[(La, Lb, Lc, Ld), (A, B, C, D)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | "")] = OpaqueTupleLenses.Nup4.wrap[La, Lb, Lc, Ld, A, B, C, D, Lz](nup4)[labels.ExplicitTypeIndices.UpTo4[La, Lb, Lc, Ld, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld)](z: Z \ Lz) =
    NamedTuple.withNames(nup4.asInstanceOf[OpaqueTupleLenses.Tup4[A, B, C, D, labels.ExplicitTypeIndices.UpTo3[La, Lb, Lc, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld), Lz, 0, labels.ExplicitTypeIndices.UpTo3[La, Lb, Lc, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Lz), (A, B, C, D, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld)]] match
      case"" =>
        val tup4 = nup4.asInstanceOf[(A, B, C, D)]
        NamedTuple[(La, Lb, Lc, Ld, Lz), (A, B, C, D, Z)]((tup4._1, tup4._2, tup4._3, tup4._4, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, A, B, C, D, E](nup5: NTup[(La, Lb, Lc, Ld, Le), (A, B, C, D, E)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | "")] = OpaqueTupleLenses.Nup5.wrap[La, Lb, Lc, Ld, Le, A, B, C, D, E, Lz](nup5)[labels.ExplicitTypeIndices.UpTo5[La, Lb, Lc, Ld, Le, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le)](z: Z \ Lz) =
    NamedTuple.withNames(nup5.asInstanceOf[OpaqueTupleLenses.Tup5[A, B, C, D, E, labels.ExplicitTypeIndices.UpTo4[La, Lb, Lc, Ld, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le), Lz, 0, labels.ExplicitTypeIndices.UpTo4[La, Lb, Lc, Ld, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lz), (A, B, C, D, E, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le)]] match
      case"" =>
        val tup5 = nup5.asInstanceOf[(A, B, C, D, E)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lz), (A, B, C, D, E, Z)]((tup5._1, tup5._2, tup5._3, tup5._4, tup5._5, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, A, B, C, D, E, F](nup6: NTup[(La, Lb, Lc, Ld, Le, Lf), (A, B, C, D, E, F)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | "")] = OpaqueTupleLenses.Nup6.wrap[La, Lb, Lc, Ld, Le, Lf, A, B, C, D, E, F, Lz](nup6)[labels.ExplicitTypeIndices.UpTo6[La, Lb, Lc, Ld, Le, Lf, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf)](z: Z \ Lz) =
    NamedTuple.withNames(nup6.asInstanceOf[OpaqueTupleLenses.Tup6[A, B, C, D, E, F, labels.ExplicitTypeIndices.UpTo5[La, Lb, Lc, Ld, Le, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf), Lz, 0, labels.ExplicitTypeIndices.UpTo5[La, Lb, Lc, Ld, Le, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lz), (A, B, C, D, E, F, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf)]] match
      case"" =>
        val tup6 = nup6.asInstanceOf[(A, B, C, D, E, F)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lz), (A, B, C, D, E, F, Z)]((tup6._1, tup6._2, tup6._3, tup6._4, tup6._5, tup6._6, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, A, B, C, D, E, F, G](nup7: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg), (A, B, C, D, E, F, G)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | "")] = OpaqueTupleLenses.Nup7.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, A, B, C, D, E, F, G, Lz](nup7)[labels.ExplicitTypeIndices.UpTo7[La, Lb, Lc, Ld, Le, Lf, Lg, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg)](z: Z \ Lz) =
    NamedTuple.withNames(nup7.asInstanceOf[OpaqueTupleLenses.Tup7[A, B, C, D, E, F, G, labels.ExplicitTypeIndices.UpTo6[La, Lb, Lc, Ld, Le, Lf, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg), Lz, 0, labels.ExplicitTypeIndices.UpTo6[La, Lb, Lc, Ld, Le, Lf, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lz), (A, B, C, D, E, F, G, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg)]] match
      case"" =>
        val tup7 = nup7.asInstanceOf[(A, B, C, D, E, F, G)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lz), (A, B, C, D, E, F, G, Z)]((tup7._1, tup7._2, tup7._3, tup7._4, tup7._5, tup7._6, tup7._7, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, A, B, C, D, E, F, G, H](nup8: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh), (A, B, C, D, E, F, G, H)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | "")] = OpaqueTupleLenses.Nup8.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, A, B, C, D, E, F, G, H, Lz](nup8)[labels.ExplicitTypeIndices.UpTo8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh)](z: Z \ Lz) =
    NamedTuple.withNames(nup8.asInstanceOf[OpaqueTupleLenses.Tup8[A, B, C, D, E, F, G, H, labels.ExplicitTypeIndices.UpTo7[La, Lb, Lc, Ld, Le, Lf, Lg, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh), Lz, 0, labels.ExplicitTypeIndices.UpTo7[La, Lb, Lc, Ld, Le, Lf, Lg, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Lz), (A, B, C, D, E, F, G, H, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh)]] match
      case"" =>
        val tup8 = nup8.asInstanceOf[(A, B, C, D, E, F, G, H)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Lz), (A, B, C, D, E, F, G, H, Z)]((tup8._1, tup8._2, tup8._3, tup8._4, tup8._5, tup8._6, tup8._7, tup8._8, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, A, B, C, D, E, F, G, H, I](nup9: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li), (A, B, C, D, E, F, G, H, I)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | "")] = OpaqueTupleLenses.Nup9.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, A, B, C, D, E, F, G, H, I, Lz](nup9)[labels.ExplicitTypeIndices.UpTo9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li)](z: Z \ Lz) =
    NamedTuple.withNames(nup9.asInstanceOf[OpaqueTupleLenses.Tup9[A, B, C, D, E, F, G, H, I, labels.ExplicitTypeIndices.UpTo8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li), Lz, 0, labels.ExplicitTypeIndices.UpTo8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lz), (A, B, C, D, E, F, G, H, I, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li)]] match
      case"" =>
        val tup9 = nup9.asInstanceOf[(A, B, C, D, E, F, G, H, I)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lz), (A, B, C, D, E, F, G, H, I, Z)]((tup9._1, tup9._2, tup9._3, tup9._4, tup9._5, tup9._6, tup9._7, tup9._8, tup9._9, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, A, B, C, D, E, F, G, H, I, J](nup10: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj), (A, B, C, D, E, F, G, H, I, J)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | "")] = OpaqueTupleLenses.Nup10.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, A, B, C, D, E, F, G, H, I, J, Lz](nup10)[labels.ExplicitTypeIndices.UpTo10[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj)](z: Z \ Lz) =
    NamedTuple.withNames(nup10.asInstanceOf[OpaqueTupleLenses.Tup10[A, B, C, D, E, F, G, H, I, J, labels.ExplicitTypeIndices.UpTo9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj), Lz, 0, labels.ExplicitTypeIndices.UpTo9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lz), (A, B, C, D, E, F, G, H, I, J, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj)]] match
      case"" =>
        val tup10 = nup10.asInstanceOf[(A, B, C, D, E, F, G, H, I, J)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lz), (A, B, C, D, E, F, G, H, I, J, Z)]((tup10._1, tup10._2, tup10._3, tup10._4, tup10._5, tup10._6, tup10._7, tup10._8, tup10._9, tup10._10, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, A, B, C, D, E, F, G, H, I, J, K](nup11: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk), (A, B, C, D, E, F, G, H, I, J, K)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | "")] = OpaqueTupleLenses.Nup11.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, A, B, C, D, E, F, G, H, I, J, K, Lz](nup11)[labels.ExplicitTypeIndices.UpTo11[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk)](z: Z \ Lz) =
    NamedTuple.withNames(nup11.asInstanceOf[OpaqueTupleLenses.Tup11[A, B, C, D, E, F, G, H, I, J, K, labels.ExplicitTypeIndices.UpTo10[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk), Lz, 0, labels.ExplicitTypeIndices.UpTo10[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Lz), (A, B, C, D, E, F, G, H, I, J, K, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk)]] match
      case"" =>
        val tup11 = nup11.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Lz), (A, B, C, D, E, F, G, H, I, J, K, Z)]((tup11._1, tup11._2, tup11._3, tup11._4, tup11._5, tup11._6, tup11._7, tup11._8, tup11._9, tup11._10, tup11._11, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L](nup12: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll), (A, B, C, D, E, F, G, H, I, J, K, L)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | "")] = OpaqueTupleLenses.Nup12.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, A, B, C, D, E, F, G, H, I, J, K, L, Lz](nup12)[labels.ExplicitTypeIndices.UpTo12[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll)](z: Z \ Lz) =
    NamedTuple.withNames(nup12.asInstanceOf[OpaqueTupleLenses.Tup12[A, B, C, D, E, F, G, H, I, J, K, L, labels.ExplicitTypeIndices.UpTo11[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll), Lz, 0, labels.ExplicitTypeIndices.UpTo11[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll)]] match
      case"" =>
        val tup12 = nup12.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, Z)]((tup12._1, tup12._2, tup12._3, tup12._4, tup12._5, tup12._6, tup12._7, tup12._8, tup12._9, tup12._10, tup12._11, tup12._12, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, Lm <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L, M](nup13: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm), (A, B, C, D, E, F, G, H, I, J, K, L, M)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | "")] = OpaqueTupleLenses.Nup13.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, A, B, C, D, E, F, G, H, I, J, K, L, M, Lz](nup13)[labels.ExplicitTypeIndices.UpTo13[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm)](z: Z \ Lz) =
    NamedTuple.withNames(nup13.asInstanceOf[OpaqueTupleLenses.Tup13[A, B, C, D, E, F, G, H, I, J, K, L, M, labels.ExplicitTypeIndices.UpTo12[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm), Lz, 0, labels.ExplicitTypeIndices.UpTo12[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm)]] match
      case"" =>
        val tup13 = nup13.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, Z)]((tup13._1, tup13._2, tup13._3, tup13._4, tup13._5, tup13._6, tup13._7, tup13._8, tup13._9, tup13._10, tup13._11, tup13._12, tup13._13, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, Lm <: LabelStr, Ln <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L, M, N](nup14: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln), (A, B, C, D, E, F, G, H, I, J, K, L, M, N)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | "")] = OpaqueTupleLenses.Nup14.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, A, B, C, D, E, F, G, H, I, J, K, L, M, N, Lz](nup14)[labels.ExplicitTypeIndices.UpTo14[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln)](z: Z \ Lz) =
    NamedTuple.withNames(nup14.asInstanceOf[OpaqueTupleLenses.Tup14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, labels.ExplicitTypeIndices.UpTo13[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln), Lz, 0, labels.ExplicitTypeIndices.UpTo13[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln)]] match
      case"" =>
        val tup14 = nup14.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z)]((tup14._1, tup14._2, tup14._3, tup14._4, tup14._5, tup14._6, tup14._7, tup14._8, tup14._9, tup14._10, tup14._11, tup14._12, tup14._13, tup14._14, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, Lm <: LabelStr, Ln <: LabelStr, Lo <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](nup15: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | "")] = OpaqueTupleLenses.Nup15.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Lz](nup15)[labels.ExplicitTypeIndices.UpTo15[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo)](z: Z \ Lz) =
    NamedTuple.withNames(nup15.asInstanceOf[OpaqueTupleLenses.Tup15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, labels.ExplicitTypeIndices.UpTo14[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo), Lz, 0, labels.ExplicitTypeIndices.UpTo14[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo)]] match
      case"" =>
        val tup15 = nup15.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z)]((tup15._1, tup15._2, tup15._3, tup15._4, tup15._5, tup15._6, tup15._7, tup15._8, tup15._9, tup15._10, tup15._11, tup15._12, tup15._13, tup15._14, tup15._15, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, Lm <: LabelStr, Ln <: LabelStr, Lo <: LabelStr, Lp <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](nup16: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | "")] = OpaqueTupleLenses.Nup16.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Lz](nup16)[labels.ExplicitTypeIndices.UpTo16[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp)](z: Z \ Lz) =
    NamedTuple.withNames(nup16.asInstanceOf[OpaqueTupleLenses.Tup16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, labels.ExplicitTypeIndices.UpTo15[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp), Lz, 0, labels.ExplicitTypeIndices.UpTo15[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp)]] match
      case"" =>
        val tup16 = nup16.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z)]((tup16._1, tup16._2, tup16._3, tup16._4, tup16._5, tup16._6, tup16._7, tup16._8, tup16._9, tup16._10, tup16._11, tup16._12, tup16._13, tup16._14, tup16._15, tup16._16, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, Lm <: LabelStr, Ln <: LabelStr, Lo <: LabelStr, Lp <: LabelStr, Lq <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](nup17: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | "")] = OpaqueTupleLenses.Nup17.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Lz](nup17)[labels.ExplicitTypeIndices.UpTo17[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq)](z: Z \ Lz) =
    NamedTuple.withNames(nup17.asInstanceOf[OpaqueTupleLenses.Tup17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, labels.ExplicitTypeIndices.UpTo16[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq), Lz, 0, labels.ExplicitTypeIndices.UpTo16[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq)]] match
      case"" =>
        val tup17 = nup17.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z)]((tup17._1, tup17._2, tup17._3, tup17._4, tup17._5, tup17._6, tup17._7, tup17._8, tup17._9, tup17._10, tup17._11, tup17._12, tup17._13, tup17._14, tup17._15, tup17._16, tup17._17, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, Lm <: LabelStr, Ln <: LabelStr, Lo <: LabelStr, Lp <: LabelStr, Lq <: LabelStr, Lr <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](nup18: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | Lr | "")] = OpaqueTupleLenses.Nup18.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Lz](nup18)[labels.ExplicitTypeIndices.UpTo18[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | Lr)](z: Z \ Lz) =
    NamedTuple.withNames(nup18.asInstanceOf[OpaqueTupleLenses.Tup18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, labels.ExplicitTypeIndices.UpTo17[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr), Lz, 0, labels.ExplicitTypeIndices.UpTo17[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr)]] match
      case"" =>
        val tup18 = nup18.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z)]((tup18._1, tup18._2, tup18._3, tup18._4, tup18._5, tup18._6, tup18._7, tup18._8, tup18._9, tup18._10, tup18._11, tup18._12, tup18._13, tup18._14, tup18._15, tup18._16, tup18._17, tup18._18, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, Lm <: LabelStr, Ln <: LabelStr, Lo <: LabelStr, Lp <: LabelStr, Lq <: LabelStr, Lr <: LabelStr, Ls <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](nup19: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | Lr | Ls | "")] = OpaqueTupleLenses.Nup19.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Lz](nup19)[labels.ExplicitTypeIndices.UpTo19[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | Lr | Ls)](z: Z \ Lz) =
    NamedTuple.withNames(nup19.asInstanceOf[OpaqueTupleLenses.Tup19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, labels.ExplicitTypeIndices.UpTo18[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls), Lz, 0, labels.ExplicitTypeIndices.UpTo18[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls)]] match
      case"" =>
        val tup19 = nup19.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z)]((tup19._1, tup19._2, tup19._3, tup19._4, tup19._5, tup19._6, tup19._7, tup19._8, tup19._9, tup19._10, tup19._11, tup19._12, tup19._13, tup19._14, tup19._15, tup19._16, tup19._17, tup19._18, tup19._19, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, Lm <: LabelStr, Ln <: LabelStr, Lo <: LabelStr, Lp <: LabelStr, Lq <: LabelStr, Lr <: LabelStr, Ls <: LabelStr, Lt <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](nup20: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | Lr | Ls | Lt | "")] = OpaqueTupleLenses.Nup20.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Lz](nup20)[labels.ExplicitTypeIndices.UpTo20[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | Lr | Ls | Lt)](z: Z \ Lz) =
    NamedTuple.withNames(nup20.asInstanceOf[OpaqueTupleLenses.Tup20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, labels.ExplicitTypeIndices.UpTo19[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt), Lz, 0, labels.ExplicitTypeIndices.UpTo19[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt)]] match
      case"" =>
        val tup20 = nup20.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z)]((tup20._1, tup20._2, tup20._3, tup20._4, tup20._5, tup20._6, tup20._7, tup20._8, tup20._9, tup20._10, tup20._11, tup20._12, tup20._13, tup20._14, tup20._15, tup20._16, tup20._17, tup20._18, tup20._19, tup20._20, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, Lm <: LabelStr, Ln <: LabelStr, Lo <: LabelStr, Lp <: LabelStr, Lq <: LabelStr, Lr <: LabelStr, Ls <: LabelStr, Lt <: LabelStr, Lu <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](nup21: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | Lr | Ls | Lt | Lu | "")] = OpaqueTupleLenses.Nup21.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Lz](nup21)[labels.ExplicitTypeIndices.UpTo21[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | Lr | Ls | Lt | Lu)](z: Z \ Lz) =
    NamedTuple.withNames(nup21.asInstanceOf[OpaqueTupleLenses.Tup21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, labels.ExplicitTypeIndices.UpTo20[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu), Lz, 0, labels.ExplicitTypeIndices.UpTo20[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu)]] match
      case"" =>
        val tup21 = nup21.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z)]((tup21._1, tup21._2, tup21._3, tup21._4, tup21._5, tup21._6, tup21._7, tup21._8, tup21._9, tup21._10, tup21._11, tup21._12, tup21._13, tup21._14, tup21._15, tup21._16, tup21._17, tup21._18, tup21._19, tup21._20, tup21._21, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension[La <: LabelStr, Lb <: LabelStr, Lc <: LabelStr, Ld <: LabelStr, Le <: LabelStr, Lf <: LabelStr, Lg <: LabelStr, Lh <: LabelStr, Li <: LabelStr, Lj <: LabelStr, Lk <: LabelStr, Ll <: LabelStr, Lm <: LabelStr, Ln <: LabelStr, Lo <: LabelStr, Lp <: LabelStr, Lq <: LabelStr, Lr <: LabelStr, Ls <: LabelStr, Lt <: LabelStr, Lu <: LabelStr, Lv <: LabelStr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](nup22: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]) {
  /** Operations focused on an individual element of this tuple. */
  transparent inline def lens[Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | Lr | Ls | Lt | Lu | Lv | "")] = OpaqueTupleLenses.Nup22.wrap[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Lz](nup22)[labels.ExplicitTypeIndices.UpTo22[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv, Lz]]

  /** Swaps a value corresponding to an existing name. */
  transparent inline infix def replace[Z, Lz <: (La | Lb | Lc | Ld | Le | Lf | Lg | Lh | Li | Lj | Lk | Ll | Lm | Ln | Lo | Lp | Lq | Lr | Ls | Lt | Lu | Lv)](z: Z \ Lz) =
    NamedTuple.withNames(nup22.asInstanceOf[OpaqueTupleLenses.Tup22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, labels.ExplicitTypeIndices.UpTo21[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv), Lz, 0, labels.ExplicitTypeIndices.UpTo21[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lz]]]

  /** Make into a larger named tuple by appending one named element. */
  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z)] =
    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv)]] match
      case"" =>
        val tup22 = nup22.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]
        NamedTuple[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv, Lz), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z)]((tup22._1, tup22._2, tup22._3, tup22._4, tup22._5, tup22._6, tup22._7, tup22._8, tup22._9, tup22._10, tup22._11, tup22._12, tup22._13, tup22._14, tup22._15, tup22._16, tup22._17, tup22._18, tup22._19, tup22._20, tup22._21, tup22._22, z))
      case e => compiletime.error("Duplicate or missing names: " + e)
}


extension [Ns <: Tuple, Ts <: Tuple](tup: NamedTuple.NamedTuple[Ns, Ts]) {
  transparent inline def toNames = compiletime.constValueTuple[Ns]

  transparent inline def name(i: Int) = compiletime.constValueTuple[Ns](i)

  transparent inline def indexOf[L <: LabelStr](inline l: L) =
    compiletime.constValue[labels.NamesAndLabels.IndexOfType[Ns, L, 0]]

  transparent inline def copyFrom[Nz <: Tuple, Tz <: Tuple](zup: NamedTuple.NamedTuple[Nz, Tz]) =
    labels.NamesAndLabels.copyWithUpdateByName[Ns, Ts, Nz, Tz](tup, zup)
}


/* 

################
## GENERATORS ##
################

def mkTupExt(n: Int) =
  assert(n > 1 && n < 25)
  val args = "ABCDEFGHIJKLMNOPQRSTUVWX".take(n).map(_.toString)
  val targ = args.mkString(", ")
  val torg = args.mkString(" | ")
  val argvs = (1 to n).map(i => s"tup$n._$i")
  val varg = argvs.mkString(", ")
  val norg = (0 to n).mkString(" | ")
  val ttarg = args.map(_ * 2).mkString(", ")
  val t2ttf = args.map(a => s"inline ${a.toLowerCase}: $a => $a$a").mkString(", ")
  val t2ttap = (args zip argvs).map{ case (a, v) => s"${a.toLowerCase}($v)" }.mkString(", ")
  val sameap = argvs.map(v => s"z($v)").mkString(", ")
  val redap = argvs.reduce((l, r) => s"op($l, $r)")
  println(s"extension [$targ](tup$n: ($targ)) {")
  println(s"  /** Operations focused on an individual element of this tuple. */")
  println(s"  transparent inline def lens[Zn <: $norg] = OpaqueTupleLenses.Tup$n.wrap(tup$n)[Zn]")
  println()
  println(s"  /** Create a new tuple by applying a different function to each position of this tuple */")
  println(s"  inline def ops[$ttarg]($t2ttf): ($ttarg) = ($t2ttap)")
  println()
  println(s"  /** Create a new tuple by applying the same function to each position of this tuple. */")
  println(s"  transparent inline def sameOp[Z](z: ($torg) => Z) = ($sameap)")
  println()
  println(s"  /** Pass the values of this tuple into a $n-argument function and return the result. */")
  println(s"  inline def merge[Z](inline z: ($targ) => Z): Z = z($varg)")
  println()
  println(s"  /** Using a binary operation, reduce this tuple to a single value. */")
  println(s"  inline def reduce[Z >: ($torg)](op: (Z, Z) => Z) = $redap")
  println()
  println(s"  /** Make into a larger tuple by appending one element. */")
  println(s"  inline infix def tup[Z](z: Z): ($targ, Z) =")
  println(s"    ($varg, z)")
  println()
  println(s"  /** Make into a larger tuple by applying a function to all elements. */")
  println(s"  inline def tupWith[Z](inline z: ($targ) => Z): ($targ, Z) =")
  println(s"    ($varg, z($varg))")
  println(s"}")

for n <- 2 to 22 do
  mkTupExt(n)
  println()
  println()

def mkNupExt(n: Int) =
  assert(n > 1 && n < 25)
  val args = "ABCDEFGHIJKLMNOPQRSTUVWX".take(n).map(_.toString)
  val targ = args.mkString(", ")
  val larg = args.map(a => s"L${a.toLowerCase}").mkString(", ")
  val largdr1 = args.dropRight(1).map(a => s"L${a.toLowerCase}").mkString(", ")
  val lorg = args.map(a => s"L${a.toLowerCase}").mkString(" | ")
  val tlarg = args.map(a => s"L${a.toLowerCase} <: LabelStr").mkString(", ")
  val varg = (1 to n).map(i => s"tup$n._$i").mkString(", ")
  println(s"extension[$tlarg, $targ](nup$n: NTup[($larg), ($targ)]) {")
  println(s"  /** Operations focused on an individual element of this tuple. */")
  println(s"  transparent inline def lens[Lz <: ($lorg | \"\")] = OpaqueTupleLenses.Nup$n.wrap[$larg, $targ, Lz](nup$n)[labels.ExplicitTypeIndices.UpTo$n[$larg, Lz]]")
  println()
  println(s"  /** Swaps a value corresponding to an existing name. */")
  println(s"  transparent inline infix def replace[Z, Lz <: ($lorg)](z: Z \\ Lz) =")
  println(s"    NamedTuple.withNames(nup$n.asInstanceOf[OpaqueTupleLenses.Tup$n[$targ, labels.ExplicitTypeIndices.UpTo${n-1}[$largdr1, Lz]]].to[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[($larg), Lz, 0, labels.ExplicitTypeIndices.UpTo${n-1}[$largdr1, Lz]]]")
  println()
  println(s"  /** Make into a larger named tuple by appending one named element. */")
  println(s"  inline def tup[Lz <: LabelStr](lz: Lz)[Z](z: Z): NTup[($larg, Lz), ($targ, Z)] =")
  println(s"    inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, ($larg)]] match")
  println(s"      case\"\" =>")
  println(s"        val tup$n = nup$n.asInstanceOf[($targ)]")
  println(s"        NamedTuple[($larg, Lz), ($targ, Z)](($varg, z))")
  println(s"      case e => compiletime.error(\"Duplicate or missing names: \" + e)")
  println(s"}")

for n <- 2 to 22 do
  mkNupExt(n)
  println()
  println()
*/
