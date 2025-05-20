// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025 Rex Kerr and UCSF (Kato Lab)

package kse.basics


//import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.compiletime.{constValue, summonFrom}

import scala.annotation.targetName
import scala.NamedTuple.{NamedTuple => NTup}



object OpaqueTupleLenses {
  object Tup0:
    inline def insert[Z](z: Z) = Tuple1(z)

  opaque type Tup1[A, N <: 0 | 1] = Tuple1[A]
  object Tup1:
    inline def wrap[A](tup1: Tuple1[A])[N <: 0 | 1]: Tup1[A, N] = tup1
    extension [A, N <: 0 | 1](tup1: Tup1[A, N])
      transparent inline def get =
        inline if compiletime.constValue[N] == 1 then compiletime.error("Can insert but not access past end of 1-tuple")
        else tup1(compiletime.constValue[N])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[N] == 1 then compiletime.error("Can insert but not modify past end of 1-tuple")
        else Tuple1(z)
      inline def map[Z](zf: A => Z) =
        inline if compiletime.constValue[N] == 1 then compiletime.error("Can insert but not modify past end of 1-tuple")
        else Tuple1(zf(tup1._1))
      inline def delete =
        inline if compiletime.constValue[N] == 1 then compiletime.error("Can insert but not delete past end of 1-tuple")
        else EmptyTuple
      inline def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup1, z)[0, N],
        labels.NamesAndLabels.byNumberOrExtra(tup1, z)[1, N]
      )
      transparent inline def insertOps[Z](inline zf: A => Z) =
        Tup1.insert(tup1)(zf(tup1._1))

  opaque type Tup2[A, B, Zn <: 0 | 1 | 2] = (A, B)
  object Tup2:
    inline def wrap[A, B](tup2: (A, B))[Zn <: 0 | 1 | 2]: Tup2[A, B, Zn] = tup2
    extension [A, B, Zn <: 0 | 1 | 2](tup2: Tup2[A, B, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 2 then compiletime.error("Can insert but not access past end of 2-tuple")
        else tup2(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 2 then compiletime.error("Can insert but not modify past end of 2-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup2, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup2, z)[1, Zn]
        )
      inline def map[Z](zf: Tuple.Elem[(A, B), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 2 then compiletime.error("Can insert but not modify past end of 2-tuple")
        else (
          labels.NamesAndLabels.byNumberOrMap[(A, B), Z, Zn](tup2, zf)[0],
          labels.NamesAndLabels.byNumberOrMap[(A, B), Z, Zn](tup2, zf)[1]
        )
      transparent inline def toOps[Z](inline zf: (A, B) => Z) =
        Tup2.to(tup2)(zf(tup2._1, tup2._2))
      inline def delete =
        inline if compiletime.constValue[Zn] == 2 then compiletime.error("Can insert but not delete past end of 2-tuple")
        else Tuple1(
          labels.NamesAndLabels.byNumberOrSkip(tup2)[0, Zn]
        )
      inline def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup2, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup2, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup2, z)[2, Zn]
      )
      transparent inline def insertOps[Z](inline zf: (A, B) => Z) =
        Tup2.insert(tup2)(zf(tup2._1, tup2._2))

  opaque type Tup3[A, B, C, Zn <: 0 | 1 | 2 | 3] = (A, B, C)
  object Tup3:
    inline def wrap[A, B, C](tup3: (A, B, C))[Zn <: 0 | 1 | 2 | 3]: Tup3[A, B, C, Zn] = tup3
    extension [A, B, C, Zn <: 0 | 1 | 2 | 3](tup3: Tup3[A, B, C, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 3 then compiletime.error("Can insert but not access past end of 3-tuple")
        else tup3(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 3 then compiletime.error("Can insert but not modify past end of 3-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup3, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup3, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup3, z)[2, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 3 then compiletime.error("Can insert but not modify past end of 3-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C), Z, Zn](tup3, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C), Z, Zn](tup3, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C), Z, Zn](tup3, zf)[2],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C) => Z) =
        Tup3.to(tup3)(zf(tup3._1, tup3._2, tup3._3))
      inline def delete =
        inline if compiletime.constValue[Zn] == 3 then compiletime.error("Can insert but not modify past end of 3-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup3)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup3)[1, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup3, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup3, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup3, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup3, z)[3, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C) => Z) =
        Tup3.insert(tup3)(zf(tup3._1, tup3._2, tup3._3))

  opaque type Tup4[A, B, C, D, Zn <: 0 | 1 | 2 | 3 | 4] = (A, B, C, D)
  object Tup4:
    inline def wrap[A, B, C, D](tup4: (A, B, C, D))[Zn <: 0 | 1 | 2 | 3 | 4]: Tup4[A, B, C, D, Zn] = tup4
    extension [A, B, C, D, Zn <: 0 | 1 | 2 | 3 | 4](tup4: Tup4[A, B, C, D, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 4 then compiletime.error("Can insert but not access past end of 4-tuple")
        else tup4(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 4 then compiletime.error("Can insert but not modify past end of 4-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup4, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup4, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup4, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup4, z)[3, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 4 then compiletime.error("Can insert but not modify past end of 4-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D), Z, Zn](tup4, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D), Z, Zn](tup4, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D), Z, Zn](tup4, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D), Z, Zn](tup4, zf)[3],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D) => Z) =
        Tup4.to(tup4)(zf(tup4._1, tup4._2, tup4._3, tup4._4))
      inline def delete =
        inline if compiletime.constValue[Zn] == 4 then compiletime.error("Can insert but not modify past end of 4-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup4)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup4)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup4)[2, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup4, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup4, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup4, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup4, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup4, z)[4, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D) => Z) =
        Tup4.insert(tup4)(zf(tup4._1, tup4._2, tup4._3, tup4._4))

  opaque type Tup5[A, B, C, D, E, Zn <: 0 | 1 | 2 | 3 | 4 | 5] = (A, B, C, D, E)
  object Tup5:
    inline def wrap[A, B, C, D, E](tup5: (A, B, C, D, E))[Zn <: 0 | 1 | 2 | 3 | 4 | 5]: Tup5[A, B, C, D, E, Zn] = tup5
    extension [A, B, C, D, E, Zn <: 0 | 1 | 2 | 3 | 4 | 5](tup5: Tup5[A, B, C, D, E, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 5 then compiletime.error("Can insert but not access past end of 5-tuple")
        else tup5(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 5 then compiletime.error("Can insert but not modify past end of 5-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup5, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup5, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup5, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup5, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup5, z)[4, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 5 then compiletime.error("Can insert but not modify past end of 5-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E), Z, Zn](tup5, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E), Z, Zn](tup5, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E), Z, Zn](tup5, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E), Z, Zn](tup5, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E), Z, Zn](tup5, zf)[4],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E) => Z) =
        Tup5.to(tup5)(zf(tup5._1, tup5._2, tup5._3, tup5._4, tup5._5))
      inline def delete =
        inline if compiletime.constValue[Zn] == 5 then compiletime.error("Can insert but not modify past end of 5-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup5)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup5)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup5)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup5)[3, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup5, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup5, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup5, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup5, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup5, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup5, z)[5, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E) => Z) =
        Tup5.insert(tup5)(zf(tup5._1, tup5._2, tup5._3, tup5._4, tup5._5))

  opaque type Tup6[A, B, C, D, E, F, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6] = (A, B, C, D, E, F)
  object Tup6:
    inline def wrap[A, B, C, D, E, F](tup6: (A, B, C, D, E, F))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6]: Tup6[A, B, C, D, E, F, Zn] = tup6
    extension [A, B, C, D, E, F, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6](tup6: Tup6[A, B, C, D, E, F, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 6 then compiletime.error("Can insert but not access past end of 6-tuple")
        else tup6(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 6 then compiletime.error("Can insert but not modify past end of 6-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup6, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup6, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup6, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup6, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup6, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup6, z)[5, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 6 then compiletime.error("Can insert but not modify past end of 6-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F), Z, Zn](tup6, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F), Z, Zn](tup6, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F), Z, Zn](tup6, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F), Z, Zn](tup6, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F), Z, Zn](tup6, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F), Z, Zn](tup6, zf)[5],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F) => Z) =
        Tup6.to(tup6)(zf(tup6._1, tup6._2, tup6._3, tup6._4, tup6._5, tup6._6))
      inline def delete =
        inline if compiletime.constValue[Zn] == 6 then compiletime.error("Can insert but not modify past end of 6-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup6)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup6)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup6)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup6)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup6)[4, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup6, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup6, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup6, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup6, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup6, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup6, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup6, z)[6, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F) => Z) =
        Tup6.insert(tup6)(zf(tup6._1, tup6._2, tup6._3, tup6._4, tup6._5, tup6._6))

  opaque type Tup7[A, B, C, D, E, F, G, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7] = (A, B, C, D, E, F, G)
  object Tup7:
    inline def wrap[A, B, C, D, E, F, G](tup7: (A, B, C, D, E, F, G))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7]: Tup7[A, B, C, D, E, F, G, Zn] = tup7
    extension [A, B, C, D, E, F, G, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7](tup7: Tup7[A, B, C, D, E, F, G, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 7 then compiletime.error("Can insert but not access past end of 7-tuple")
        else tup7(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 7 then compiletime.error("Can insert but not modify past end of 7-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup7, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup7, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup7, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup7, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup7, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup7, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup7, z)[6, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 7 then compiletime.error("Can insert but not modify past end of 7-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G), Z, Zn](tup7, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G), Z, Zn](tup7, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G), Z, Zn](tup7, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G), Z, Zn](tup7, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G), Z, Zn](tup7, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G), Z, Zn](tup7, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G), Z, Zn](tup7, zf)[6],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G) => Z) =
        Tup7.to(tup7)(zf(tup7._1, tup7._2, tup7._3, tup7._4, tup7._5, tup7._6, tup7._7))
      inline def delete =
        inline if compiletime.constValue[Zn] == 7 then compiletime.error("Can insert but not modify past end of 7-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup7)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup7)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup7)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup7)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup7)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup7)[5, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup7, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup7, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup7, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup7, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup7, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup7, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup7, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup7, z)[7, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G) => Z) =
        Tup7.insert(tup7)(zf(tup7._1, tup7._2, tup7._3, tup7._4, tup7._5, tup7._6, tup7._7))

  opaque type Tup8[A, B, C, D, E, F, G, H, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8] = (A, B, C, D, E, F, G, H)
  object Tup8:
    inline def wrap[A, B, C, D, E, F, G, H](tup8: (A, B, C, D, E, F, G, H))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8]: Tup8[A, B, C, D, E, F, G, H, Zn] = tup8
    extension [A, B, C, D, E, F, G, H, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8](tup8: Tup8[A, B, C, D, E, F, G, H, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 8 then compiletime.error("Can insert but not access past end of 8-tuple")
        else tup8(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 8 then compiletime.error("Can insert but not modify past end of 8-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup8, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup8, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup8, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup8, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup8, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup8, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup8, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup8, z)[7, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 8 then compiletime.error("Can insert but not modify past end of 8-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H), Z, Zn](tup8, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H), Z, Zn](tup8, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H), Z, Zn](tup8, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H), Z, Zn](tup8, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H), Z, Zn](tup8, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H), Z, Zn](tup8, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H), Z, Zn](tup8, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H), Z, Zn](tup8, zf)[7],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H) => Z) =
        Tup8.to(tup8)(zf(tup8._1, tup8._2, tup8._3, tup8._4, tup8._5, tup8._6, tup8._7, tup8._8))
      inline def delete =
        inline if compiletime.constValue[Zn] == 8 then compiletime.error("Can insert but not modify past end of 8-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup8)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup8)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup8)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup8)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup8)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup8)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup8)[6, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup8, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup8, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup8, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup8, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup8, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup8, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup8, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup8, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup8, z)[8, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H) => Z) =
        Tup8.insert(tup8)(zf(tup8._1, tup8._2, tup8._3, tup8._4, tup8._5, tup8._6, tup8._7, tup8._8))

  opaque type Tup9[A, B, C, D, E, F, G, H, I, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9] = (A, B, C, D, E, F, G, H, I)
  object Tup9:
    inline def wrap[A, B, C, D, E, F, G, H, I](tup9: (A, B, C, D, E, F, G, H, I))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9]: Tup9[A, B, C, D, E, F, G, H, I, Zn] = tup9
    extension [A, B, C, D, E, F, G, H, I, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9](tup9: Tup9[A, B, C, D, E, F, G, H, I, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 9 then compiletime.error("Can insert but not access past end of 9-tuple")
        else tup9(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 9 then compiletime.error("Can insert but not modify past end of 9-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup9, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup9, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup9, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup9, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup9, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup9, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup9, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup9, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup9, z)[8, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 9 then compiletime.error("Can insert but not modify past end of 9-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I), Z, Zn](tup9, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I), Z, Zn](tup9, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I), Z, Zn](tup9, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I), Z, Zn](tup9, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I), Z, Zn](tup9, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I), Z, Zn](tup9, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I), Z, Zn](tup9, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I), Z, Zn](tup9, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I), Z, Zn](tup9, zf)[8],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I) => Z) =
        Tup9.to(tup9)(zf(tup9._1, tup9._2, tup9._3, tup9._4, tup9._5, tup9._6, tup9._7, tup9._8, tup9._9))
      inline def delete =
        inline if compiletime.constValue[Zn] == 9 then compiletime.error("Can insert but not modify past end of 9-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup9)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup9)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup9)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup9)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup9)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup9)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup9)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup9)[7, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup9, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup9, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup9, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup9, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup9, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup9, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup9, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup9, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup9, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup9, z)[9, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I) => Z) =
        Tup9.insert(tup9)(zf(tup9._1, tup9._2, tup9._3, tup9._4, tup9._5, tup9._6, tup9._7, tup9._8, tup9._9))

  opaque type Tup10[A, B, C, D, E, F, G, H, I, J, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10] = (A, B, C, D, E, F, G, H, I, J)
  object Tup10:
    inline def wrap[A, B, C, D, E, F, G, H, I, J](tup10: (A, B, C, D, E, F, G, H, I, J))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10]: Tup10[A, B, C, D, E, F, G, H, I, J, Zn] = tup10
    extension [A, B, C, D, E, F, G, H, I, J, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10](tup10: Tup10[A, B, C, D, E, F, G, H, I, J, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 10 then compiletime.error("Can insert but not access past end of 10-tuple")
        else tup10(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 10 then compiletime.error("Can insert but not modify past end of 10-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup10, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup10, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup10, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup10, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup10, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup10, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup10, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup10, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup10, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup10, z)[9, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 10 then compiletime.error("Can insert but not modify past end of 10-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J), Z, Zn](tup10, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J), Z, Zn](tup10, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J), Z, Zn](tup10, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J), Z, Zn](tup10, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J), Z, Zn](tup10, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J), Z, Zn](tup10, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J), Z, Zn](tup10, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J), Z, Zn](tup10, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J), Z, Zn](tup10, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J), Z, Zn](tup10, zf)[9],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J) => Z) =
        Tup10.to(tup10)(zf(tup10._1, tup10._2, tup10._3, tup10._4, tup10._5, tup10._6, tup10._7, tup10._8, tup10._9, tup10._10))
      inline def delete =
        inline if compiletime.constValue[Zn] == 10 then compiletime.error("Can insert but not modify past end of 10-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup10)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup10)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup10)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup10)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup10)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup10)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup10)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup10)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup10)[8, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup10, z)[10, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J) => Z) =
        Tup10.insert(tup10)(zf(tup10._1, tup10._2, tup10._3, tup10._4, tup10._5, tup10._6, tup10._7, tup10._8, tup10._9, tup10._10))

  opaque type Tup11[A, B, C, D, E, F, G, H, I, J, K, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11] = (A, B, C, D, E, F, G, H, I, J, K)
  object Tup11:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K](tup11: (A, B, C, D, E, F, G, H, I, J, K))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11]: Tup11[A, B, C, D, E, F, G, H, I, J, K, Zn] = tup11
    extension [A, B, C, D, E, F, G, H, I, J, K, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11](tup11: Tup11[A, B, C, D, E, F, G, H, I, J, K, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 11 then compiletime.error("Can insert but not access past end of 11-tuple")
        else tup11(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 11 then compiletime.error("Can insert but not modify past end of 11-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup11, z)[10, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 11 then compiletime.error("Can insert but not modify past end of 11-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K), Z, Zn](tup11, zf)[10],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K) => Z) =
        Tup11.to(tup11)(zf(tup11._1, tup11._2, tup11._3, tup11._4, tup11._5, tup11._6, tup11._7, tup11._8, tup11._9, tup11._10, tup11._11))
      inline def delete =
        inline if compiletime.constValue[Zn] == 11 then compiletime.error("Can insert but not modify past end of 11-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup11)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup11)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup11)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup11)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup11)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup11)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup11)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup11)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup11)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup11)[9, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup11, z)[11, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K) => Z) =
        Tup11.insert(tup11)(zf(tup11._1, tup11._2, tup11._3, tup11._4, tup11._5, tup11._6, tup11._7, tup11._8, tup11._9, tup11._10, tup11._11))

  opaque type Tup12[A, B, C, D, E, F, G, H, I, J, K, L, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12] = (A, B, C, D, E, F, G, H, I, J, K, L)
  object Tup12:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L](tup12: (A, B, C, D, E, F, G, H, I, J, K, L))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12]: Tup12[A, B, C, D, E, F, G, H, I, J, K, L, Zn] = tup12
    extension [A, B, C, D, E, F, G, H, I, J, K, L, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12](tup12: Tup12[A, B, C, D, E, F, G, H, I, J, K, L, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 12 then compiletime.error("Can insert but not access past end of 12-tuple")
        else tup12(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 12 then compiletime.error("Can insert but not modify past end of 12-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup12, z)[11, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 12 then compiletime.error("Can insert but not modify past end of 12-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L), Z, Zn](tup12, zf)[11],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L) => Z) =
        Tup12.to(tup12)(zf(tup12._1, tup12._2, tup12._3, tup12._4, tup12._5, tup12._6, tup12._7, tup12._8, tup12._9, tup12._10, tup12._11, tup12._12))
      inline def delete =
        inline if compiletime.constValue[Zn] == 12 then compiletime.error("Can insert but not modify past end of 12-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup12)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup12)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup12)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup12)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup12)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup12)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup12)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup12)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup12)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup12)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup12)[10, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup12, z)[12, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L) => Z) =
        Tup12.insert(tup12)(zf(tup12._1, tup12._2, tup12._3, tup12._4, tup12._5, tup12._6, tup12._7, tup12._8, tup12._9, tup12._10, tup12._11, tup12._12))

  opaque type Tup13[A, B, C, D, E, F, G, H, I, J, K, L, M, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13] = (A, B, C, D, E, F, G, H, I, J, K, L, M)
  object Tup13:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L, M](tup13: (A, B, C, D, E, F, G, H, I, J, K, L, M))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13]: Tup13[A, B, C, D, E, F, G, H, I, J, K, L, M, Zn] = tup13
    extension [A, B, C, D, E, F, G, H, I, J, K, L, M, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13](tup13: Tup13[A, B, C, D, E, F, G, H, I, J, K, L, M, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 13 then compiletime.error("Can insert but not access past end of 13-tuple")
        else tup13(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 13 then compiletime.error("Can insert but not modify past end of 13-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[11, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup13, z)[12, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 13 then compiletime.error("Can insert but not modify past end of 13-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[11],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M), Z, Zn](tup13, zf)[12],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Z) =
        Tup13.to(tup13)(zf(tup13._1, tup13._2, tup13._3, tup13._4, tup13._5, tup13._6, tup13._7, tup13._8, tup13._9, tup13._10, tup13._11, tup13._12, tup13._13))
      inline def delete =
        inline if compiletime.constValue[Zn] == 13 then compiletime.error("Can insert but not modify past end of 13-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup13)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[10, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup13)[11, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[12, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup13, z)[13, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Z) =
        Tup13.insert(tup13)(zf(tup13._1, tup13._2, tup13._3, tup13._4, tup13._5, tup13._6, tup13._7, tup13._8, tup13._9, tup13._10, tup13._11, tup13._12, tup13._13))

  opaque type Tup14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N)
  object Tup14:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L, M, N](tup14: (A, B, C, D, E, F, G, H, I, J, K, L, M, N))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14]: Tup14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn] = tup14
    extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14](tup14: Tup14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 14 then compiletime.error("Can insert but not access past end of 14-tuple")
        else tup14(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 14 then compiletime.error("Can insert but not modify past end of 14-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[11, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[12, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup14, z)[13, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 14 then compiletime.error("Can insert but not modify past end of 14-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[11],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[12],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Z, Zn](tup14, zf)[13],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z) =
        Tup14.to(tup14)(zf(tup14._1, tup14._2, tup14._3, tup14._4, tup14._5, tup14._6, tup14._7, tup14._8, tup14._9, tup14._10, tup14._11, tup14._12, tup14._13, tup14._14))
      inline def delete =
        inline if compiletime.constValue[Zn] == 14 then compiletime.error("Can insert but not modify past end of 14-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup14)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[10, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[11, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup14)[12, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[12, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[13, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup14, z)[14, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z) =
        Tup14.insert(tup14)(zf(tup14._1, tup14._2, tup14._3, tup14._4, tup14._5, tup14._6, tup14._7, tup14._8, tup14._9, tup14._10, tup14._11, tup14._12, tup14._13, tup14._14))

  opaque type Tup15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
  object Tup15:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](tup15: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15]: Tup15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn] = tup15
    extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15](tup15: Tup15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 15 then compiletime.error("Can insert but not access past end of 15-tuple")
        else tup15(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 15 then compiletime.error("Can insert but not modify past end of 15-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[11, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[12, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[13, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup15, z)[14, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 15 then compiletime.error("Can insert but not modify past end of 15-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[11],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[12],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[13],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Z, Zn](tup15, zf)[14],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z) =
        Tup15.to(tup15)(zf(tup15._1, tup15._2, tup15._3, tup15._4, tup15._5, tup15._6, tup15._7, tup15._8, tup15._9, tup15._10, tup15._11, tup15._12, tup15._13, tup15._14, tup15._15))
      inline def delete =
        inline if compiletime.constValue[Zn] == 15 then compiletime.error("Can insert but not modify past end of 15-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup15)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[10, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[11, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[12, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup15)[13, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[12, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[13, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[14, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup15, z)[15, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z) =
        Tup15.insert(tup15)(zf(tup15._1, tup15._2, tup15._3, tup15._4, tup15._5, tup15._6, tup15._7, tup15._8, tup15._9, tup15._10, tup15._11, tup15._12, tup15._13, tup15._14, tup15._15))

  opaque type Tup16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)
  object Tup16:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](tup16: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16]: Tup16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn] = tup16
    extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16](tup16: Tup16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 16 then compiletime.error("Can insert but not access past end of 16-tuple")
        else tup16(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 16 then compiletime.error("Can insert but not modify past end of 16-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[11, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[12, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[13, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[14, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup16, z)[15, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 16 then compiletime.error("Can insert but not modify past end of 16-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[11],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[12],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[13],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[14],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Z, Zn](tup16, zf)[15],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z) =
        Tup16.to(tup16)(zf(tup16._1, tup16._2, tup16._3, tup16._4, tup16._5, tup16._6, tup16._7, tup16._8, tup16._9, tup16._10, tup16._11, tup16._12, tup16._13, tup16._14, tup16._15, tup16._16))
      inline def delete =
        inline if compiletime.constValue[Zn] == 16 then compiletime.error("Can insert but not modify past end of 16-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup16)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[10, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[11, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[12, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[13, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup16)[14, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[12, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[13, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[14, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[15, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup16, z)[16, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z) =
        Tup16.insert(tup16)(zf(tup16._1, tup16._2, tup16._3, tup16._4, tup16._5, tup16._6, tup16._7, tup16._8, tup16._9, tup16._10, tup16._11, tup16._12, tup16._13, tup16._14, tup16._15, tup16._16))

  opaque type Tup17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
  object Tup17:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](tup17: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17]: Tup17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn] = tup17
    extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17](tup17: Tup17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 17 then compiletime.error("Can insert but not access past end of 17-tuple")
        else tup17(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 17 then compiletime.error("Can insert but not modify past end of 17-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[11, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[12, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[13, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[14, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[15, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup17, z)[16, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 17 then compiletime.error("Can insert but not modify past end of 17-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[11],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[12],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[13],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[14],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[15],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Z, Zn](tup17, zf)[16],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z) =
        Tup17.to(tup17)(zf(tup17._1, tup17._2, tup17._3, tup17._4, tup17._5, tup17._6, tup17._7, tup17._8, tup17._9, tup17._10, tup17._11, tup17._12, tup17._13, tup17._14, tup17._15, tup17._16, tup17._17))
      inline def delete =
        inline if compiletime.constValue[Zn] == 17 then compiletime.error("Can insert but not modify past end of 17-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup17)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[10, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[11, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[12, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[13, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[14, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup17)[15, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[12, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[13, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[14, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[15, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[16, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup17, z)[17, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z) =
        Tup17.insert(tup17)(zf(tup17._1, tup17._2, tup17._3, tup17._4, tup17._5, tup17._6, tup17._7, tup17._8, tup17._9, tup17._10, tup17._11, tup17._12, tup17._13, tup17._14, tup17._15, tup17._16, tup17._17))

  opaque type Tup18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)
  object Tup18:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](tup18: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18]: Tup18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn] = tup18
    extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18](tup18: Tup18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 18 then compiletime.error("Can insert but not access past end of 18-tuple")
        else tup18(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 18 then compiletime.error("Can insert but not modify past end of 18-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[11, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[12, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[13, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[14, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[15, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[16, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup18, z)[17, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 18 then compiletime.error("Can insert but not modify past end of 18-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[11],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[12],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[13],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[14],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[15],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[16],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Z, Zn](tup18, zf)[17],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z) =
        Tup18.to(tup18)(zf(tup18._1, tup18._2, tup18._3, tup18._4, tup18._5, tup18._6, tup18._7, tup18._8, tup18._9, tup18._10, tup18._11, tup18._12, tup18._13, tup18._14, tup18._15, tup18._16, tup18._17, tup18._18))
      inline def delete =
        inline if compiletime.constValue[Zn] == 18 then compiletime.error("Can insert but not modify past end of 18-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup18)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[10, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[11, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[12, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[13, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[14, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[15, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup18)[16, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[12, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[13, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[14, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[15, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[16, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[17, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup18, z)[18, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z) =
        Tup18.insert(tup18)(zf(tup18._1, tup18._2, tup18._3, tup18._4, tup18._5, tup18._6, tup18._7, tup18._8, tup18._9, tup18._10, tup18._11, tup18._12, tup18._13, tup18._14, tup18._15, tup18._16, tup18._17, tup18._18))

  opaque type Tup19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
  object Tup19:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](tup19: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19]: Tup19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn] = tup19
    extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19](tup19: Tup19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 19 then compiletime.error("Can insert but not access past end of 19-tuple")
        else tup19(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 19 then compiletime.error("Can insert but not modify past end of 19-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[11, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[12, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[13, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[14, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[15, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[16, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[17, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup19, z)[18, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 19 then compiletime.error("Can insert but not modify past end of 19-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[11],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[12],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[13],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[14],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[15],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[16],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[17],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Z, Zn](tup19, zf)[18],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z) =
        Tup19.to(tup19)(zf(tup19._1, tup19._2, tup19._3, tup19._4, tup19._5, tup19._6, tup19._7, tup19._8, tup19._9, tup19._10, tup19._11, tup19._12, tup19._13, tup19._14, tup19._15, tup19._16, tup19._17, tup19._18, tup19._19))
      inline def delete =
        inline if compiletime.constValue[Zn] == 19 then compiletime.error("Can insert but not modify past end of 19-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup19)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[10, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[11, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[12, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[13, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[14, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[15, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[16, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup19)[17, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[12, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[13, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[14, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[15, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[16, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[17, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[18, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup19, z)[19, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z) =
        Tup19.insert(tup19)(zf(tup19._1, tup19._2, tup19._3, tup19._4, tup19._5, tup19._6, tup19._7, tup19._8, tup19._9, tup19._10, tup19._11, tup19._12, tup19._13, tup19._14, tup19._15, tup19._16, tup19._17, tup19._18, tup19._19))

  opaque type Tup20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
  object Tup20:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](tup20: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20]: Tup20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn] = tup20
    extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20](tup20: Tup20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 20 then compiletime.error("Can insert but not access past end of 20-tuple")
        else tup20(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 20 then compiletime.error("Can insert but not modify past end of 20-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[11, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[12, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[13, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[14, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[15, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[16, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[17, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[18, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup20, z)[19, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 20 then compiletime.error("Can insert but not modify past end of 20-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[11],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[12],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[13],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[14],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[15],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[16],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[17],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[18],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Z, Zn](tup20, zf)[19],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z) =
        Tup20.to(tup20)(zf(tup20._1, tup20._2, tup20._3, tup20._4, tup20._5, tup20._6, tup20._7, tup20._8, tup20._9, tup20._10, tup20._11, tup20._12, tup20._13, tup20._14, tup20._15, tup20._16, tup20._17, tup20._18, tup20._19, tup20._20))
      inline def delete =
        inline if compiletime.constValue[Zn] == 20 then compiletime.error("Can insert but not modify past end of 20-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup20)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[10, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[11, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[12, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[13, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[14, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[15, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[16, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[17, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup20)[18, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[12, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[13, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[14, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[15, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[16, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[17, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[18, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[19, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup20, z)[20, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z) =
        Tup20.insert(tup20)(zf(tup20._1, tup20._2, tup20._3, tup20._4, tup20._5, tup20._6, tup20._7, tup20._8, tup20._9, tup20._10, tup20._11, tup20._12, tup20._13, tup20._14, tup20._15, tup20._16, tup20._17, tup20._18, tup20._19, tup20._20))

  opaque type Tup21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
  object Tup21:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](tup21: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21]: Tup21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn] = tup21
    extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21](tup21: Tup21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 21 then compiletime.error("Can insert but not access past end of 21-tuple")
        else tup21(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 21 then compiletime.error("Can insert but not modify past end of 21-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[11, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[12, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[13, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[14, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[15, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[16, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[17, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[18, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[19, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup21, z)[20, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 21 then compiletime.error("Can insert but not modify past end of 21-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[11],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[12],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[13],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[14],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[15],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[16],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[17],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[18],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[19],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Z, Zn](tup21, zf)[20],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z) =
        Tup21.to(tup21)(zf(tup21._1, tup21._2, tup21._3, tup21._4, tup21._5, tup21._6, tup21._7, tup21._8, tup21._9, tup21._10, tup21._11, tup21._12, tup21._13, tup21._14, tup21._15, tup21._16, tup21._17, tup21._18, tup21._19, tup21._20, tup21._21))
      inline def delete =
        inline if compiletime.constValue[Zn] == 21 then compiletime.error("Can insert but not modify past end of 21-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup21)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[10, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[11, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[12, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[13, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[14, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[15, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[16, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[17, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[18, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup21)[19, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[12, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[13, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[14, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[15, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[16, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[17, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[18, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[19, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[20, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup21, z)[21, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z) =
        Tup21.insert(tup21)(zf(tup21._1, tup21._2, tup21._3, tup21._4, tup21._5, tup21._6, tup21._7, tup21._8, tup21._9, tup21._10, tup21._11, tup21._12, tup21._13, tup21._14, tup21._15, tup21._16, tup21._17, tup21._18, tup21._19, tup21._20, tup21._21))

  opaque type Tup22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
  object Tup22:
    inline def wrap[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](tup22: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22]: Tup22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn] = tup22
    extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22](tup22: Tup22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn])
      transparent inline def get =
        inline if compiletime.constValue[Zn] == 22 then compiletime.error("Can insert but not access past end of 22-tuple")
        else tup22(compiletime.constValue[Zn])
      inline infix def to[Z](z: Z) =
        inline if compiletime.constValue[Zn] == 22 then compiletime.error("Can insert but not modify past end of 22-tuple")
        else (
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[0, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[1, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[2, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[3, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[4, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[5, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[6, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[7, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[8, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[9, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[10, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[11, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[12, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[13, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[14, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[15, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[16, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[17, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[18, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[19, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[20, Zn],
          labels.NamesAndLabels.byNumberOrBackup(tup22, z)[21, Zn],
         )
      inline def map[Z](inline zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Zn] => Z) =
        inline if compiletime.constValue[Zn] == 22 then compiletime.error("Can insert but not modify past end of 22-tuple")
        else (
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[0],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[1],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[2],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[3],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[4],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[5],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[6],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[7],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[8],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[9],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[10],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[11],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[12],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[13],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[14],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[15],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[16],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[17],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[18],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[19],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[20],
           labels.NamesAndLabels.byNumberOrMap[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Z, Zn](tup22, zf)[21],
        )
      transparent inline def toOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z) =
        Tup22.to(tup22)(zf(tup22._1, tup22._2, tup22._3, tup22._4, tup22._5, tup22._6, tup22._7, tup22._8, tup22._9, tup22._10, tup22._11, tup22._12, tup22._13, tup22._14, tup22._15, tup22._16, tup22._17, tup22._18, tup22._19, tup22._20, tup22._21, tup22._22))
      inline def delete =
        inline if compiletime.constValue[Zn] == 22 then compiletime.error("Can insert but not modify past end of 22-tuple")
        else (
          labels.NamesAndLabels.byNumberOrSkip(tup22)[0, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[1, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[2, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[3, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[4, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[5, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[6, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[7, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[8, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[9, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[10, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[11, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[12, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[13, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[14, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[15, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[16, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[17, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[18, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[19, Zn],
          labels.NamesAndLabels.byNumberOrSkip(tup22)[20, Zn],
         )
      inline infix def insert[Z](z: Z) = (
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[0, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[1, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[2, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[3, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[4, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[5, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[6, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[7, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[8, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[9, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[10, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[11, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[12, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[13, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[14, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[15, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[16, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[17, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[18, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[19, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[20, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[21, Zn],
        labels.NamesAndLabels.byNumberOrExtra(tup22, z)[22, Zn],
      )
      transparent inline def insertOps[Z](inline zf: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z) =
        Tup22.insert(tup22)(zf(tup22._1, tup22._2, tup22._3, tup22._4, tup22._5, tup22._6, tup22._7, tup22._8, tup22._9, tup22._10, tup22._11, tup22._12, tup22._13, tup22._14, tup22._15, tup22._16, tup22._17, tup22._18, tup22._19, tup22._20, tup22._21, tup22._22))


  opaque type Nup1[La <: LabelVal, A, Zn <: 0 | 1, Zl <: LabelVal] = Tuple1[A]
  object Nup1:
    inline def wrap[La <: LabelVal, A, Zl <: LabelVal](nup1: NTup[Tuple1[La], Tuple1[A]])[Zn <: 0 | 1]: Nup1[La, A, Zn, Zl] = nup1.asInstanceOf[Tuple1[A]]
    extension [La <: LabelVal, A, Zn <: 0 | 1, Zl <: LabelVal](nup1: Nup1[La, A, Zn, Zl])
      transparent inline def get = Tup1.get(nup1.asInstanceOf[Tup1[A, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 1 then compiletime.error("Can insert but not access past end of 1-tuple")
        else compiletime.constValue[La]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 1 then compiletime.error("Can insert but not access past end of 1-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, Tuple1[La]]] match
          case "" => nup1.asInstanceOf[NTup[Tuple1[Lz], Tuple1[A]]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _ => compiletime.error("This field is already named " + constValue[Lz])
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 1 then compiletime.error("Can insert but not access past end of 1-tuple")
        else \.wrap(nup1.asInstanceOf[Tuple1[A]](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 1 then compiletime.error("Can insert but not access past end of 1-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, Tuple1[La]]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case _ => NamedTuple.withNames(Tuple1[Z](z.unlabel))[Tuple1[Lz]]
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tuple1[Z](z))[Tuple1[La]]
      transparent inline def map[Z](zf: A => Z) =
        NamedTuple.withNames(Tup1.map(nup1.asInstanceOf[Tup1[A, Zn]])[Z](zf))[Tuple1[La]]
      inline def delete =
        Tup1.delete(nup1.asInstanceOf[Tup1[A, Zn]])
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, Tuple1[La]]] match
          case "" => NamedTuple.withNames(Tup1.insert(nup1.asInstanceOf[Tup1[A, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[Tuple1[La], Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup2[La <: LabelVal, Lb <: LabelVal, A, B, Zn <: 0 | 1 | 2, Zl <: LabelVal] = (A, B)
  object Nup2:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, A, B, Zl <: LabelVal](nup2: NTup[(La, Lb), (A, B)])[Zn <: 0 | 1 | 2]: Nup2[La, Lb, A, B, Zn, Zl] = nup2.asInstanceOf[(A, B)]
    extension [La <: LabelVal, Lb <: LabelVal, A, B, Zn <: 0 | 1 | 2, Zl <: LabelVal](nup2: Nup2[La, Lb, A, B, Zn, Zl])
      transparent inline def get = Tup2.get(nup2.asInstanceOf[Tup2[A, B, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 2 then compiletime.error("Can insert but not access past end of 2-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 2 then compiletime.error("Can insert but not access past end of 2-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb)]] match
          case "" => nup2.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb), Lz, 0, Zn], (A, B)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 2 then compiletime.error("Can insert but not access past end of 2-tuple")
        else \.wrap(nup2.asInstanceOf[(A, B)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 2 then compiletime.error("Can insert but not access past end of 2-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup2.to(nup2.asInstanceOf[Tup2[A, B, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup2.to(nup2.asInstanceOf[Tup2[A, B, Zn]])[Z](z))[(La, Lb)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B), Zn] => Z) =
        NamedTuple.withNames(Tup2.map(nup2.asInstanceOf[Tup2[A, B, Zn]])[Z](zf))[(La, Lb)]
      inline def delete =
        NamedTuple.withNames(Tup2.delete(nup2.asInstanceOf[Tup2[A, B, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb), 0, Zn]]
      inline def insert[Lz <: LabelVal](lz: Lz)[Z](z: Z) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb)]] match
          case "" => NamedTuple.withNames(Tup2.insert(nup2.asInstanceOf[Tup2[A, B, Zn]])[Z](z))[labels.NamesAndLabels.TupleWithExtra[(La, Lb), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup3[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, A, B, C, Zn <: 0 | 1 | 2 | 3, Zl <: LabelVal] = (A, B, C)
  object Nup3:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, A, B, C, Zl <: LabelVal](nup3: NTup[(La, Lb, Lc), (A, B, C)])[Zn <: 0 | 1 | 2 | 3]: Nup3[La, Lb, Lc, A, B, C, Zn, Zl] = nup3.asInstanceOf[(A, B, C)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, A, B, C, Zn <: 0 | 1 | 2 | 3, Zl <: LabelVal](nup3: Nup3[La, Lb, Lc, A, B, C, Zn, Zl])
      transparent inline def get =
        Tup3.get(nup3.asInstanceOf[Tup3[A, B, C, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 3 then compiletime.error("Can insert but not access past end of 3-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 3 then compiletime.error("Can insert but not access past end of 3-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc)]] match
          case "" => nup3.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc), Lz, 0, Zn], (A, B, C)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 3 then compiletime.error("Can insert but not access past end of 3-tuple")
        else \.wrap(nup3.asInstanceOf[(A, B, C)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 3 then compiletime.error("Can insert but not access past end of 3-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup3.to(nup3.asInstanceOf[Tup3[A, B, C, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup3.to(nup3.asInstanceOf[Tup3[A, B, C, Zn]])[Z](z))[(La, Lb, Lc)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C), Zn] => Z) =
        NamedTuple.withNames(Tup3.map(nup3.asInstanceOf[Tup3[A, B, C, Zn]])[Z](zf))[(La, Lb, Lc)]
      inline def delete =
         NamedTuple.withNames(Tup3.delete(nup3.asInstanceOf[Tup3[A, B, C, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc)]] match
          case "" => NamedTuple.withNames(Tup3.insert(nup3.asInstanceOf[Tup3[A, B, C, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup4[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, A, B, C, D, Zn <: 0 | 1 | 2 | 3 | 4, Zl <: LabelVal] = (A, B, C, D)
  object Nup4:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, A, B, C, D, Zl <: LabelVal](nup4: NTup[(La, Lb, Lc, Ld), (A, B, C, D)])[Zn <: 0 | 1 | 2 | 3 | 4]: Nup4[La, Lb, Lc, Ld, A, B, C, D, Zn, Zl] = nup4.asInstanceOf[(A, B, C, D)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, A, B, C, D, Zn <: 0 | 1 | 2 | 3 | 4, Zl <: LabelVal](nup4: Nup4[La, Lb, Lc, Ld, A, B, C, D, Zn, Zl])
      transparent inline def get =
        Tup4.get(nup4.asInstanceOf[Tup4[A, B, C, D, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 4 then compiletime.error("Can insert but not access past end of 4-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 4 then compiletime.error("Can insert but not access past end of 4-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld)]] match
          case "" => nup4.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld), Lz, 0, Zn], (A, B, C, D)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 4 then compiletime.error("Can insert but not access past end of 4-tuple")
        else \.wrap(nup4.asInstanceOf[(A, B, C, D)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 4 then compiletime.error("Can insert but not access past end of 4-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup4.to(nup4.asInstanceOf[Tup4[A, B, C, D, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup4.to(nup4.asInstanceOf[Tup4[A, B, C, D, Zn]])[Z](z))[(La, Lb, Lc, Ld)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D), Zn] => Z) =
        NamedTuple.withNames(Tup4.map(nup4.asInstanceOf[Tup4[A, B, C, D, Zn]])[Z](zf))[(La, Lb, Lc, Ld)]
      inline def delete =
         NamedTuple.withNames(Tup4.delete(nup4.asInstanceOf[Tup4[A, B, C, D, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld)]] match
          case "" => NamedTuple.withNames(Tup4.insert(nup4.asInstanceOf[Tup4[A, B, C, D, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup5[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, A, B, C, D, E, Zn <: 0 | 1 | 2 | 3 | 4 | 5, Zl <: LabelVal] = (A, B, C, D, E)
  object Nup5:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, A, B, C, D, E, Zl <: LabelVal](nup5: NTup[(La, Lb, Lc, Ld, Le), (A, B, C, D, E)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5]: Nup5[La, Lb, Lc, Ld, Le, A, B, C, D, E, Zn, Zl] = nup5.asInstanceOf[(A, B, C, D, E)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, A, B, C, D, E, Zn <: 0 | 1 | 2 | 3 | 4 | 5, Zl <: LabelVal](nup5: Nup5[La, Lb, Lc, Ld, Le, A, B, C, D, E, Zn, Zl])
      transparent inline def get =
        Tup5.get(nup5.asInstanceOf[Tup5[A, B, C, D, E, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 5 then compiletime.error("Can insert but not access past end of 5-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 5 then compiletime.error("Can insert but not access past end of 5-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le)]] match
          case "" => nup5.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le), Lz, 0, Zn], (A, B, C, D, E)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 5 then compiletime.error("Can insert but not access past end of 5-tuple")
        else \.wrap(nup5.asInstanceOf[(A, B, C, D, E)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 5 then compiletime.error("Can insert but not access past end of 5-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup5.to(nup5.asInstanceOf[Tup5[A, B, C, D, E, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup5.to(nup5.asInstanceOf[Tup5[A, B, C, D, E, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E), Zn] => Z) =
        NamedTuple.withNames(Tup5.map(nup5.asInstanceOf[Tup5[A, B, C, D, E, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le)]
      inline def delete =
         NamedTuple.withNames(Tup5.delete(nup5.asInstanceOf[Tup5[A, B, C, D, E, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le)]] match
          case "" => NamedTuple.withNames(Tup5.insert(nup5.asInstanceOf[Tup5[A, B, C, D, E, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup6[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, A, B, C, D, E, F, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6, Zl <: LabelVal] = (A, B, C, D, E, F)
  object Nup6:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, A, B, C, D, E, F, Zl <: LabelVal](nup6: NTup[(La, Lb, Lc, Ld, Le, Lf), (A, B, C, D, E, F)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6]: Nup6[La, Lb, Lc, Ld, Le, Lf, A, B, C, D, E, F, Zn, Zl] = nup6.asInstanceOf[(A, B, C, D, E, F)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, A, B, C, D, E, F, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6, Zl <: LabelVal](nup6: Nup6[La, Lb, Lc, Ld, Le, Lf, A, B, C, D, E, F, Zn, Zl])
      transparent inline def get =
        Tup6.get(nup6.asInstanceOf[Tup6[A, B, C, D, E, F, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 6 then compiletime.error("Can insert but not access past end of 6-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 6 then compiletime.error("Can insert but not access past end of 6-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf)]] match
          case "" => nup6.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf), Lz, 0, Zn], (A, B, C, D, E, F)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 6 then compiletime.error("Can insert but not access past end of 6-tuple")
        else \.wrap(nup6.asInstanceOf[(A, B, C, D, E, F)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 6 then compiletime.error("Can insert but not access past end of 6-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup6.to(nup6.asInstanceOf[Tup6[A, B, C, D, E, F, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup6.to(nup6.asInstanceOf[Tup6[A, B, C, D, E, F, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F), Zn] => Z) =
        NamedTuple.withNames(Tup6.map(nup6.asInstanceOf[Tup6[A, B, C, D, E, F, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf)]
      inline def delete =
         NamedTuple.withNames(Tup6.delete(nup6.asInstanceOf[Tup6[A, B, C, D, E, F, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf)]] match
          case "" => NamedTuple.withNames(Tup6.insert(nup6.asInstanceOf[Tup6[A, B, C, D, E, F, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup7[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, A, B, C, D, E, F, G, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7, Zl <: LabelVal] = (A, B, C, D, E, F, G)
  object Nup7:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, A, B, C, D, E, F, G, Zl <: LabelVal](nup7: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg), (A, B, C, D, E, F, G)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7]: Nup7[La, Lb, Lc, Ld, Le, Lf, Lg, A, B, C, D, E, F, G, Zn, Zl] = nup7.asInstanceOf[(A, B, C, D, E, F, G)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, A, B, C, D, E, F, G, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7, Zl <: LabelVal](nup7: Nup7[La, Lb, Lc, Ld, Le, Lf, Lg, A, B, C, D, E, F, G, Zn, Zl])
      transparent inline def get =
        Tup7.get(nup7.asInstanceOf[Tup7[A, B, C, D, E, F, G, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 7 then compiletime.error("Can insert but not access past end of 7-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 7 then compiletime.error("Can insert but not access past end of 7-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg)]] match
          case "" => nup7.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg), Lz, 0, Zn], (A, B, C, D, E, F, G)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 7 then compiletime.error("Can insert but not access past end of 7-tuple")
        else \.wrap(nup7.asInstanceOf[(A, B, C, D, E, F, G)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 7 then compiletime.error("Can insert but not access past end of 7-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup7.to(nup7.asInstanceOf[Tup7[A, B, C, D, E, F, G, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup7.to(nup7.asInstanceOf[Tup7[A, B, C, D, E, F, G, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G), Zn] => Z) =
        NamedTuple.withNames(Tup7.map(nup7.asInstanceOf[Tup7[A, B, C, D, E, F, G, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg)]
      inline def delete =
         NamedTuple.withNames(Tup7.delete(nup7.asInstanceOf[Tup7[A, B, C, D, E, F, G, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg)]] match
          case "" => NamedTuple.withNames(Tup7.insert(nup7.asInstanceOf[Tup7[A, B, C, D, E, F, G, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup8[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, A, B, C, D, E, F, G, H, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8, Zl <: LabelVal] = (A, B, C, D, E, F, G, H)
  object Nup8:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, A, B, C, D, E, F, G, H, Zl <: LabelVal](nup8: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh), (A, B, C, D, E, F, G, H)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8]: Nup8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, A, B, C, D, E, F, G, H, Zn, Zl] = nup8.asInstanceOf[(A, B, C, D, E, F, G, H)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, A, B, C, D, E, F, G, H, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8, Zl <: LabelVal](nup8: Nup8[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, A, B, C, D, E, F, G, H, Zn, Zl])
      transparent inline def get =
        Tup8.get(nup8.asInstanceOf[Tup8[A, B, C, D, E, F, G, H, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 8 then compiletime.error("Can insert but not access past end of 8-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 8 then compiletime.error("Can insert but not access past end of 8-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh)]] match
          case "" => nup8.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh), Lz, 0, Zn], (A, B, C, D, E, F, G, H)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 8 then compiletime.error("Can insert but not access past end of 8-tuple")
        else \.wrap(nup8.asInstanceOf[(A, B, C, D, E, F, G, H)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 8 then compiletime.error("Can insert but not access past end of 8-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup8.to(nup8.asInstanceOf[Tup8[A, B, C, D, E, F, G, H, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup8.to(nup8.asInstanceOf[Tup8[A, B, C, D, E, F, G, H, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H), Zn] => Z) =
        NamedTuple.withNames(Tup8.map(nup8.asInstanceOf[Tup8[A, B, C, D, E, F, G, H, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh)]
      inline def delete =
         NamedTuple.withNames(Tup8.delete(nup8.asInstanceOf[Tup8[A, B, C, D, E, F, G, H, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh)]] match
          case "" => NamedTuple.withNames(Tup8.insert(nup8.asInstanceOf[Tup8[A, B, C, D, E, F, G, H, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup9[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, A, B, C, D, E, F, G, H, I, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I)
  object Nup9:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, A, B, C, D, E, F, G, H, I, Zl <: LabelVal](nup9: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li), (A, B, C, D, E, F, G, H, I)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9]: Nup9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, A, B, C, D, E, F, G, H, I, Zn, Zl] = nup9.asInstanceOf[(A, B, C, D, E, F, G, H, I)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, A, B, C, D, E, F, G, H, I, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9, Zl <: LabelVal](nup9: Nup9[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, A, B, C, D, E, F, G, H, I, Zn, Zl])
      transparent inline def get =
        Tup9.get(nup9.asInstanceOf[Tup9[A, B, C, D, E, F, G, H, I, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 9 then compiletime.error("Can insert but not access past end of 9-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 9 then compiletime.error("Can insert but not access past end of 9-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li)]] match
          case "" => nup9.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 9 then compiletime.error("Can insert but not access past end of 9-tuple")
        else \.wrap(nup9.asInstanceOf[(A, B, C, D, E, F, G, H, I)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 9 then compiletime.error("Can insert but not access past end of 9-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup9.to(nup9.asInstanceOf[Tup9[A, B, C, D, E, F, G, H, I, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup9.to(nup9.asInstanceOf[Tup9[A, B, C, D, E, F, G, H, I, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I), Zn] => Z) =
        NamedTuple.withNames(Tup9.map(nup9.asInstanceOf[Tup9[A, B, C, D, E, F, G, H, I, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li)]
      inline def delete =
         NamedTuple.withNames(Tup9.delete(nup9.asInstanceOf[Tup9[A, B, C, D, E, F, G, H, I, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li)]] match
          case "" => NamedTuple.withNames(Tup9.insert(nup9.asInstanceOf[Tup9[A, B, C, D, E, F, G, H, I, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup10[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, A, B, C, D, E, F, G, H, I, J, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J)
  object Nup10:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, A, B, C, D, E, F, G, H, I, J, Zl <: LabelVal](nup10: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj), (A, B, C, D, E, F, G, H, I, J)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10]: Nup10[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, A, B, C, D, E, F, G, H, I, J, Zn, Zl] = nup10.asInstanceOf[(A, B, C, D, E, F, G, H, I, J)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, A, B, C, D, E, F, G, H, I, J, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10, Zl <: LabelVal](nup10: Nup10[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, A, B, C, D, E, F, G, H, I, J, Zn, Zl])
      transparent inline def get =
        Tup10.get(nup10.asInstanceOf[Tup10[A, B, C, D, E, F, G, H, I, J, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 10 then compiletime.error("Can insert but not access past end of 10-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 10 then compiletime.error("Can insert but not access past end of 10-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj)]] match
          case "" => nup10.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 10 then compiletime.error("Can insert but not access past end of 10-tuple")
        else \.wrap(nup10.asInstanceOf[(A, B, C, D, E, F, G, H, I, J)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 10 then compiletime.error("Can insert but not access past end of 10-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup10.to(nup10.asInstanceOf[Tup10[A, B, C, D, E, F, G, H, I, J, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup10.to(nup10.asInstanceOf[Tup10[A, B, C, D, E, F, G, H, I, J, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J), Zn] => Z) =
        NamedTuple.withNames(Tup10.map(nup10.asInstanceOf[Tup10[A, B, C, D, E, F, G, H, I, J, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj)]
      inline def delete =
         NamedTuple.withNames(Tup10.delete(nup10.asInstanceOf[Tup10[A, B, C, D, E, F, G, H, I, J, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj)]] match
          case "" => NamedTuple.withNames(Tup10.insert(nup10.asInstanceOf[Tup10[A, B, C, D, E, F, G, H, I, J, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup11[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K)
  object Nup11:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, Zl <: LabelVal](nup11: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk), (A, B, C, D, E, F, G, H, I, J, K)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11]: Nup11[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, A, B, C, D, E, F, G, H, I, J, K, Zn, Zl] = nup11.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11, Zl <: LabelVal](nup11: Nup11[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, A, B, C, D, E, F, G, H, I, J, K, Zn, Zl])
      transparent inline def get =
        Tup11.get(nup11.asInstanceOf[Tup11[A, B, C, D, E, F, G, H, I, J, K, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 11 then compiletime.error("Can insert but not access past end of 11-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 11 then compiletime.error("Can insert but not access past end of 11-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk)]] match
          case "" => nup11.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 11 then compiletime.error("Can insert but not access past end of 11-tuple")
        else \.wrap(nup11.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 11 then compiletime.error("Can insert but not access past end of 11-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup11.to(nup11.asInstanceOf[Tup11[A, B, C, D, E, F, G, H, I, J, K, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup11.to(nup11.asInstanceOf[Tup11[A, B, C, D, E, F, G, H, I, J, K, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K), Zn] => Z) =
        NamedTuple.withNames(Tup11.map(nup11.asInstanceOf[Tup11[A, B, C, D, E, F, G, H, I, J, K, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk)]
      inline def delete =
         NamedTuple.withNames(Tup11.delete(nup11.asInstanceOf[Tup11[A, B, C, D, E, F, G, H, I, J, K, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk)]] match
          case "" => NamedTuple.withNames(Tup11.insert(nup11.asInstanceOf[Tup11[A, B, C, D, E, F, G, H, I, J, K, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup12[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L)
  object Nup12:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, Zl <: LabelVal](nup12: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll), (A, B, C, D, E, F, G, H, I, J, K, L)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12]: Nup12[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, A, B, C, D, E, F, G, H, I, J, K, L, Zn, Zl] = nup12.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12, Zl <: LabelVal](nup12: Nup12[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, A, B, C, D, E, F, G, H, I, J, K, L, Zn, Zl])
      transparent inline def get =
        Tup12.get(nup12.asInstanceOf[Tup12[A, B, C, D, E, F, G, H, I, J, K, L, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 12 then compiletime.error("Can insert but not access past end of 12-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 12 then compiletime.error("Can insert but not access past end of 12-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll)]] match
          case "" => nup12.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 12 then compiletime.error("Can insert but not access past end of 12-tuple")
        else \.wrap(nup12.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 12 then compiletime.error("Can insert but not access past end of 12-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup12.to(nup12.asInstanceOf[Tup12[A, B, C, D, E, F, G, H, I, J, K, L, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup12.to(nup12.asInstanceOf[Tup12[A, B, C, D, E, F, G, H, I, J, K, L, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L), Zn] => Z) =
        NamedTuple.withNames(Tup12.map(nup12.asInstanceOf[Tup12[A, B, C, D, E, F, G, H, I, J, K, L, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll)]
      inline def delete =
         NamedTuple.withNames(Tup12.delete(nup12.asInstanceOf[Tup12[A, B, C, D, E, F, G, H, I, J, K, L, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll)]] match
          case "" => NamedTuple.withNames(Tup12.insert(nup12.asInstanceOf[Tup12[A, B, C, D, E, F, G, H, I, J, K, L, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup13[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L, M)
  object Nup13:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, Zl <: LabelVal](nup13: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm), (A, B, C, D, E, F, G, H, I, J, K, L, M)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13]: Nup13[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, A, B, C, D, E, F, G, H, I, J, K, L, M, Zn, Zl] = nup13.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13, Zl <: LabelVal](nup13: Nup13[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, A, B, C, D, E, F, G, H, I, J, K, L, M, Zn, Zl])
      transparent inline def get =
        Tup13.get(nup13.asInstanceOf[Tup13[A, B, C, D, E, F, G, H, I, J, K, L, M, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 13 then compiletime.error("Can insert but not access past end of 13-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 13 then compiletime.error("Can insert but not access past end of 13-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm)]] match
          case "" => nup13.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L, M)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 13 then compiletime.error("Can insert but not access past end of 13-tuple")
        else \.wrap(nup13.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 13 then compiletime.error("Can insert but not access past end of 13-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup13.to(nup13.asInstanceOf[Tup13[A, B, C, D, E, F, G, H, I, J, K, L, M, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup13.to(nup13.asInstanceOf[Tup13[A, B, C, D, E, F, G, H, I, J, K, L, M, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M), Zn] => Z) =
        NamedTuple.withNames(Tup13.map(nup13.asInstanceOf[Tup13[A, B, C, D, E, F, G, H, I, J, K, L, M, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm)]
      inline def delete =
         NamedTuple.withNames(Tup13.delete(nup13.asInstanceOf[Tup13[A, B, C, D, E, F, G, H, I, J, K, L, M, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm)]] match
          case "" => NamedTuple.withNames(Tup13.insert(nup13.asInstanceOf[Tup13[A, B, C, D, E, F, G, H, I, J, K, L, M, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup14[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N)
  object Nup14:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zl <: LabelVal](nup14: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln), (A, B, C, D, E, F, G, H, I, J, K, L, M, N)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14]: Nup14[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn, Zl] = nup14.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14, Zl <: LabelVal](nup14: Nup14[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn, Zl])
      transparent inline def get =
        Tup14.get(nup14.asInstanceOf[Tup14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 14 then compiletime.error("Can insert but not access past end of 14-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 14 then compiletime.error("Can insert but not access past end of 14-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln)]] match
          case "" => nup14.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L, M, N)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 14 then compiletime.error("Can insert but not access past end of 14-tuple")
        else \.wrap(nup14.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 14 then compiletime.error("Can insert but not access past end of 14-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup14.to(nup14.asInstanceOf[Tup14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup14.to(nup14.asInstanceOf[Tup14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N), Zn] => Z) =
        NamedTuple.withNames(Tup14.map(nup14.asInstanceOf[Tup14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln)]
      inline def delete =
         NamedTuple.withNames(Tup14.delete(nup14.asInstanceOf[Tup14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln)]] match
          case "" => NamedTuple.withNames(Tup14.insert(nup14.asInstanceOf[Tup14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup15[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)
  object Nup15:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zl <: LabelVal](nup15: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15]: Nup15[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn, Zl] = nup15.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15, Zl <: LabelVal](nup15: Nup15[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn, Zl])
      transparent inline def get =
        Tup15.get(nup15.asInstanceOf[Tup15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 15 then compiletime.error("Can insert but not access past end of 15-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 15 then compiletime.error("Can insert but not access past end of 15-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo)]] match
          case "" => nup15.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 15 then compiletime.error("Can insert but not access past end of 15-tuple")
        else \.wrap(nup15.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 15 then compiletime.error("Can insert but not access past end of 15-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup15.to(nup15.asInstanceOf[Tup15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup15.to(nup15.asInstanceOf[Tup15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), Zn] => Z) =
        NamedTuple.withNames(Tup15.map(nup15.asInstanceOf[Tup15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo)]
      inline def delete =
         NamedTuple.withNames(Tup15.delete(nup15.asInstanceOf[Tup15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo)]] match
          case "" => NamedTuple.withNames(Tup15.insert(nup15.asInstanceOf[Tup15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup16[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)
  object Nup16:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zl <: LabelVal](nup16: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16]: Nup16[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn, Zl] = nup16.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16, Zl <: LabelVal](nup16: Nup16[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn, Zl])
      transparent inline def get =
        Tup16.get(nup16.asInstanceOf[Tup16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 16 then compiletime.error("Can insert but not access past end of 16-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 16 then compiletime.error("Can insert but not access past end of 16-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp)]] match
          case "" => nup16.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 16 then compiletime.error("Can insert but not access past end of 16-tuple")
        else \.wrap(nup16.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 16 then compiletime.error("Can insert but not access past end of 16-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup16.to(nup16.asInstanceOf[Tup16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup16.to(nup16.asInstanceOf[Tup16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), Zn] => Z) =
        NamedTuple.withNames(Tup16.map(nup16.asInstanceOf[Tup16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp)]
      inline def delete =
         NamedTuple.withNames(Tup16.delete(nup16.asInstanceOf[Tup16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp)]] match
          case "" => NamedTuple.withNames(Tup16.insert(nup16.asInstanceOf[Tup16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup17[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)
  object Nup17:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zl <: LabelVal](nup17: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17]: Nup17[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn, Zl] = nup17.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17, Zl <: LabelVal](nup17: Nup17[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn, Zl])
      transparent inline def get =
        Tup17.get(nup17.asInstanceOf[Tup17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 17 then compiletime.error("Can insert but not access past end of 17-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 17 then compiletime.error("Can insert but not access past end of 17-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq)]] match
          case "" => nup17.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 17 then compiletime.error("Can insert but not access past end of 17-tuple")
        else \.wrap(nup17.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 17 then compiletime.error("Can insert but not access past end of 17-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup17.to(nup17.asInstanceOf[Tup17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup17.to(nup17.asInstanceOf[Tup17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), Zn] => Z) =
        NamedTuple.withNames(Tup17.map(nup17.asInstanceOf[Tup17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq)]
      inline def delete =
         NamedTuple.withNames(Tup17.delete(nup17.asInstanceOf[Tup17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq)]] match
          case "" => NamedTuple.withNames(Tup17.insert(nup17.asInstanceOf[Tup17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup18[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)
  object Nup18:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zl <: LabelVal](nup18: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18]: Nup18[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn, Zl] = nup18.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18, Zl <: LabelVal](nup18: Nup18[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn, Zl])
      transparent inline def get =
        Tup18.get(nup18.asInstanceOf[Tup18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 18 then compiletime.error("Can insert but not access past end of 18-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 18 then compiletime.error("Can insert but not access past end of 18-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr)]] match
          case "" => nup18.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 18 then compiletime.error("Can insert but not access past end of 18-tuple")
        else \.wrap(nup18.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 18 then compiletime.error("Can insert but not access past end of 18-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup18.to(nup18.asInstanceOf[Tup18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup18.to(nup18.asInstanceOf[Tup18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), Zn] => Z) =
        NamedTuple.withNames(Tup18.map(nup18.asInstanceOf[Tup18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr)]
      inline def delete =
         NamedTuple.withNames(Tup18.delete(nup18.asInstanceOf[Tup18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr)]] match
          case "" => NamedTuple.withNames(Tup18.insert(nup18.asInstanceOf[Tup18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup19[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)
  object Nup19:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zl <: LabelVal](nup19: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19]: Nup19[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn, Zl] = nup19.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19, Zl <: LabelVal](nup19: Nup19[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn, Zl])
      transparent inline def get =
        Tup19.get(nup19.asInstanceOf[Tup19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 19 then compiletime.error("Can insert but not access past end of 19-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 19 then compiletime.error("Can insert but not access past end of 19-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls)]] match
          case "" => nup19.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 19 then compiletime.error("Can insert but not access past end of 19-tuple")
        else \.wrap(nup19.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 19 then compiletime.error("Can insert but not access past end of 19-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup19.to(nup19.asInstanceOf[Tup19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup19.to(nup19.asInstanceOf[Tup19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), Zn] => Z) =
        NamedTuple.withNames(Tup19.map(nup19.asInstanceOf[Tup19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls)]
      inline def delete =
         NamedTuple.withNames(Tup19.delete(nup19.asInstanceOf[Tup19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls)]] match
          case "" => NamedTuple.withNames(Tup19.insert(nup19.asInstanceOf[Tup19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup20[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, Lt <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
  object Nup20:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, Lt <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zl <: LabelVal](nup20: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20]: Nup20[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn, Zl] = nup20.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, Lt <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20, Zl <: LabelVal](nup20: Nup20[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn, Zl])
      transparent inline def get =
        Tup20.get(nup20.asInstanceOf[Tup20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 20 then compiletime.error("Can insert but not access past end of 20-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 20 then compiletime.error("Can insert but not access past end of 20-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt)]] match
          case "" => nup20.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 20 then compiletime.error("Can insert but not access past end of 20-tuple")
        else \.wrap(nup20.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 20 then compiletime.error("Can insert but not access past end of 20-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup20.to(nup20.asInstanceOf[Tup20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup20.to(nup20.asInstanceOf[Tup20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), Zn] => Z) =
        NamedTuple.withNames(Tup20.map(nup20.asInstanceOf[Tup20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt)]
      inline def delete =
         NamedTuple.withNames(Tup20.delete(nup20.asInstanceOf[Tup20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt)]] match
          case "" => NamedTuple.withNames(Tup20.insert(nup20.asInstanceOf[Tup20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup21[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, Lt <: LabelVal, Lu <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
  object Nup21:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, Lt <: LabelVal, Lu <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zl <: LabelVal](nup21: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21]: Nup21[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn, Zl] = nup21.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, Lt <: LabelVal, Lu <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21, Zl <: LabelVal](nup21: Nup21[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn, Zl])
      transparent inline def get =
        Tup21.get(nup21.asInstanceOf[Tup21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 21 then compiletime.error("Can insert but not access past end of 21-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 21 then compiletime.error("Can insert but not access past end of 21-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu)]] match
          case "" => nup21.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 21 then compiletime.error("Can insert but not access past end of 21-tuple")
        else \.wrap(nup21.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 21 then compiletime.error("Can insert but not access past end of 21-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup21.to(nup21.asInstanceOf[Tup21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup21.to(nup21.asInstanceOf[Tup21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), Zn] => Z) =
        NamedTuple.withNames(Tup21.map(nup21.asInstanceOf[Tup21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu)]
      inline def delete =
         NamedTuple.withNames(Tup21.delete(nup21.asInstanceOf[Tup21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu)]] match
          case "" => NamedTuple.withNames(Tup21.insert(nup21.asInstanceOf[Tup21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

  opaque type Nup22[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, Lt <: LabelVal, Lu <: LabelVal, Lv <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22, Zl <: LabelVal] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
  object Nup22:
    inline def wrap[La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, Lt <: LabelVal, Lu <: LabelVal, Lv <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zl <: LabelVal](nup22: NTup[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv), (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)])[Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22]: Nup22[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn, Zl] = nup22.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]
    extension [La <: LabelVal, Lb <: LabelVal, Lc <: LabelVal, Ld <: LabelVal, Le <: LabelVal, Lf <: LabelVal, Lg <: LabelVal, Lh <: LabelVal, Li <: LabelVal, Lj <: LabelVal, Lk <: LabelVal, Ll <: LabelVal, Lm <: LabelVal, Ln <: LabelVal, Lo <: LabelVal, Lp <: LabelVal, Lq <: LabelVal, Lr <: LabelVal, Ls <: LabelVal, Lt <: LabelVal, Lu <: LabelVal, Lv <: LabelVal, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn <: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22, Zl <: LabelVal](nup22: Nup22[La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn, Zl])
      transparent inline def get =
        Tup22.get(nup22.asInstanceOf[Tup22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn]])
      transparent inline def name =
        inline if compiletime.constValue[Zn] == 22 then compiletime.error("Can insert but not access past end of 22-tuple")
        else compiletime.constValue[Zl]
      transparent inline def rename[Lz <: LabelVal] =
        inline if compiletime.constValue[Zn] == 22 then compiletime.error("Can insert but not access past end of 22-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv)]] match
          case "" => nup22.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv), Lz, 0, Zn], (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]]
          case "\"\"" => compiletime.error("Invalid field name")
          case _: Zl => compiletime.error("This field is already named " + constValue[Lz])
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def pluck =
        inline if compiletime.constValue[Zn] == 22 then compiletime.error("Can insert but not access past end of 22-tuple")
        else \.wrap(nup22.asInstanceOf[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)](compiletime.constValue[Zn]))[Zl]
      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 22 then compiletime.error("Can insert but not access past end of 22-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup22.to(nup22.asInstanceOf[Tup22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)
      transparent inline def to[Z](z: Z) =
        NamedTuple.withNames(Tup22.to(nup22.asInstanceOf[Tup22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn]])[Z](z))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv)]
      transparent inline def map[Z](zf: Tuple.Elem[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), Zn] => Z) =
        NamedTuple.withNames(Tup22.map(nup22.asInstanceOf[Tup22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn]])[Z](zf))[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv)]
      inline def delete =
         NamedTuple.withNames(Tup22.delete(nup22.asInstanceOf[Tup22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn]]))[labels.NamesAndLabels.TupleWithSkip[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv), 0, Zn]]
      inline infix def insert[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv)]] match
          case "" => NamedTuple.withNames(Tup22.insert(nup22.asInstanceOf[Tup22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[(La, Lb, Lc, Ld, Le, Lf, Lg, Lh, Li, Lj, Lk, Ll, Lm, Ln, Lo, Lp, Lq, Lr, Ls, Lt, Lu, Lv), Lz, 0, Zn]]
          case e => compiletime.error("Duplicate or missing names: " + e)

}


/* 

################
## GENERATORS ##
################

def mkTupN(n: Int) =
  assert(n > 2 && n < 25)
  val args = "ABCDEFGHIJKLMNOPQRSTUVWX".take(n)
  val targ = args.mkString(", ")
  val narg = (0 to n).mkString(" | ")
  val varg = (1 to n).map(i => s"tup$n._$i").mkString(", ")
  println(s"opaque type Tup$n[$targ, Zn <: $narg] = ($targ)")
  println(s"object Tup$n:")
  println(s"  inline def wrap[$targ](tup$n: ($targ))[Zn <: $narg]: Tup$n[$targ, Zn] = tup$n")
  println(s"  extension [$targ, Zn <: $narg](tup$n: Tup$n[$targ, Zn])")
  println(s"    transparent inline def get =")
  println(s"      inline if compiletime.constValue[Zn] == $n then compiletime.error(\"Can insert but not access past end of $n-tuple\")")
  println(s"      else tup$n(compiletime.constValue[Zn])")
  println(s"    inline infix def to[Z](z: Z) =")
  println(s"      inline if compiletime.constValue[Zn] == $n then compiletime.error(\"Can insert but not modify past end of $n-tuple\")")
  println(s"      else (")
  for k <- 0 until n do println(s"        labels.NamesAndLabels.byNumberOrBackup(tup$n, z)[$k, Zn],")
  println(s"       )")
  println(s"    inline def map[Z](inline zf: Tuple.Elem[($targ), Zn] => Z) =")
  println(s"      inline if compiletime.constValue[Zn] == $n then compiletime.error(\"Can insert but not modify past end of $n-tuple\")")
  println(s"      else (")
  for k <- 0 until n do println(s"         labels.NamesAndLabels.byNumberOrMap[($targ), Z, Zn](tup$n, zf)[$k],")
  println(s"      )")
  println(s"    transparent inline def toOps[Z](inline zf: ($targ) => Z) =")
  println(s"      Tup$n.to(tup$n)(zf($varg))")
  println(s"    inline def delete =")
  println(s"      inline if compiletime.constValue[Zn] == $n then compiletime.error(\"Can insert but not modify past end of $n-tuple\")")
  println(s"      else (")
  for k <- 0 until n-1 do println(s"        labels.NamesAndLabels.byNumberOrSkip(tup$n)[$k, Zn],")
  println(s"       )")
  println(s"    inline infix def insert[Z](z: Z) = (")
  for k <- 0 to n do println(s"      labels.NamesAndLabels.byNumberOrExtra(tup$n, z)[$k, Zn],")
  println(s"    )")
  println(s"    transparent inline def insertOps[Z](inline zf: ($targ) => Z) =")
  println(s"      Tup$n.insert(tup$n)(zf($varg))")

for n <- 3 to 22 do
  mkTupN(n)
  println()

      transparent inline infix def place[Z, Lz <: LabelVal](z: Z \ Lz) =
        inline if compiletime.constValue[Zn] == 2 then compiletime.error("Can insert but not access past end of 2-tuple")
        else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, (La, Lb)]] match
          case "\"\"" => compiletime.error("Invalid field name")
          case "" | _: Zl => NamedTuple.withNames(Tup2.to(nup2.asInstanceOf[Tup2[A, B, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[(La, Lb), Lz, 0, Zn]]
          case s => compiletime.error("Duplicate field name " + s)


def mkNupN(n: Int) =
  assert(n > 2 && n < 25)
  val args = "ABCDEFGHIJKLMNOPQRSTUVWX".take(n).map(_.toString)
  val targ = args.mkString(", ")
  val norg = (0 to n).mkString(" | ")
  val varg = (1 to n).map(i => s"tup$n._$i").mkString(", ")
  val larg = args.map(a => s"L${a.toLowerCase}").mkString(", ")
  val tlarg = args.map(a => s"L${a.toLowerCase} <: LabelVal").mkString(", ")
  println(s"opaque type Nup$n[$tlarg, $targ, Zn <: $norg, Zl <: LabelVal] = ($targ)")
  println(s"object Nup$n:")
  println(s"  inline def wrap[$tlarg, $targ, Zl <: LabelVal](nup$n: NTup[($larg), ($targ)])[Zn <: $norg]: Nup$n[$larg, $targ, Zn, Zl] = nup$n.asInstanceOf[($targ)]")
  println(s"  extension [$tlarg, $targ, Zn <: $norg, Zl <: LabelVal](nup$n: Nup$n[$larg, $targ, Zn, Zl])")
  println(s"    transparent inline def get =")
  println(s"      Tup$n.get(nup$n.asInstanceOf[Tup$n[$targ, Zn]])")
  println(s"    transparent inline def name =")
  println(s"      inline if compiletime.constValue[Zn] == $n then compiletime.error(\"Can insert but not access past end of $n-tuple\")")
  println(s"      else compiletime.constValue[Zl]")
  println(s"    transparent inline def rename[Lz <: LabelVal] =")
  println(s"      inline if compiletime.constValue[Zn] == $n then compiletime.error(\"Can insert but not access past end of $n-tuple\")")
  println(s"      else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, ($larg)]] match")
  println(s"        case \"\" => nup$n.asInstanceOf[NTup[labels.NamesAndLabels.TupleWithSwap[($larg), Lz, 0, Zn], ($targ)]]")
  println(s"        case \"\\\"\\\"\" => compiletime.error(\"Invalid field name\")")
  println(s"        case _: Zl => compiletime.error(\"This field is already named \" + constValue[Lz])")
  println(s"        case s => compiletime.error(\"Duplicate field name \" + s)")
  println(s"    transparent inline def pluck =")
  println(s"      inline if compiletime.constValue[Zn] == $n then compiletime.error(\"Can insert but not access past end of $n-tuple\")")
  println(s"      else \\.wrap(nup$n.asInstanceOf[($targ)](compiletime.constValue[Zn]))[Zl]")
  println(s"    transparent inline infix def place[Z, Lz <: LabelVal](z: Z \\ Lz) =")
  println(s"      inline if compiletime.constValue[Zn] == $n then compiletime.error(\"Can insert but not access past end of $n-tuple\")")
  println(s"      else inline constValue[labels.NamesAndLabels.NameIfPresent[Lz, ($larg)]] match")
  println(s"        case \"\\\"\\\"\" => compiletime.error(\"Invalid field name\")")
  println(s"        case \"\" | _: Zl => NamedTuple.withNames(Tup$n.to(nup$n.asInstanceOf[Tup$n[$targ, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithSwap[($larg), Lz, 0, Zn]]")
  println(s"        case s => compiletime.error(\"Duplicate field name \" + s)")
  println(s"    transparent inline def to[Z](z: Z) =")
  println(s"      NamedTuple.withNames(Tup$n.to(nup$n.asInstanceOf[Tup$n[$targ, Zn]])[Z](z))[($larg)]")
  println(s"    transparent inline def map[Z](zf: Tuple.Elem[($targ), Zn] => Z) =")
  println(s"      NamedTuple.withNames(Tup$n.map(nup$n.asInstanceOf[Tup$n[$targ, Zn]])[Z](zf))[($larg)]")
  println(s"    inline def delete =")
  println(s"       NamedTuple.withNames(Tup$n.delete(nup$n.asInstanceOf[Tup$n[$targ, Zn]]))[labels.NamesAndLabels.TupleWithSkip[($larg), 0, Zn]]")
  println(s"    inline infix def insert[Z, Lz <: LabelVal](z: Z \\ Lz) =")
  println(s"      inline compiletime.constValue[labels.NamesAndLabels.NameIfPresent[Lz, ($larg)]] match")
  println(s"        case \"\" => NamedTuple.withNames(Tup$n.insert(nup$n.asInstanceOf[Tup$n[$targ, Zn]])[Z](z.unlabel))[labels.NamesAndLabels.TupleWithExtra[($larg), Lz, 0, Zn]]")
  println(s"        case e => compiletime.error(\"Duplicate or missing names: \" + e)")

for n <- 3 to 22 do
  mkNupN(n)
  println()

*/
