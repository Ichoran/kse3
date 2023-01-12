// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-23 Rex Kerr and Calico Life Sciences LLC.

package kse.maths.packed


import java.lang.{Math => jm}

import scala.annotation.targetName


type Bit = 0|1
object Bit{
  inline def apply(z: Boolean): Bit = if (z) then 1 else 0
  inline def apply(i: 0|1): Bit = i
  inline def flip(z: Bit): Bit = (1 - z).asInstanceOf[0|1]

  inline def hex(z0: Bit): Hex = z0
  inline def hex(z1: Bit, z0: Bit): Hex = (z0 | (z1 << 1)).asInstanceOf[Hex]
  inline def hex(z2: Bit, z1: Bit, z0: Bit): Hex = (z0 | (z1 << 1) | (z2 << 2)).asInstanceOf[Hex]
  inline def hex(z3: Bit, z2: Bit, z1: Bit, z0: Bit): Hex = (z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)).asInstanceOf[Hex]

  inline def B(z0: Bit): Byte = z0.toByte
  inline def B(z1: Bit, z0: Bit): Byte = (z0 | (z1 << 1)).toByte
  inline def B(z2: Bit, z1: Bit, z0: Bit): Byte = (z0 | (z1 << 1) | (z2 << 2)).toByte
  inline def B(z3: Bit, z2: Bit, z1: Bit, z0: Bit): Byte = (z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)).toByte
  inline def B(z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Byte =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | (z4 << 4)).toByte
  inline def B(z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Byte =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5))).toByte
  inline def B(z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Byte =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6))).toByte
  inline def B(z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Byte =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7))).toByte

  inline def S(z0: Bit): Short = z0.toShort
  inline def S(z1: Bit, z0: Bit): Short = (z0 | (z1 << 1)).toShort
  inline def S(z2: Bit, z1: Bit, z0: Bit): Short = (z0 | (z1 << 1) | (z2 << 2)).toShort
  inline def S(z3: Bit, z2: Bit, z1: Bit, z0: Bit): Short = (z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)).toShort
  inline def S(z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Short =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | (z4 << 4)).toShort
  inline def S(z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Short =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5))).toShort
  inline def S(z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Short =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6))).toShort
  inline def S(z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Short =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7))).toShort
  inline def S(
    z8: Bit,
    z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Short = 
    (
      ((z0     ) | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8))
    ).toShort
  inline def S(
    z9: Bit, z8: Bit,
    z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Short = 
    (
      ((z0     ) | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9))
    ).toShort
  inline def S(
    z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,  z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Short = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10))
    ).toShort
  inline def S(
    z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Short = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11))
    ).toShort
  inline def S(
    z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Short = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12))
    ).toShort
  inline def S(
    z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Short = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13))
    ).toShort
  inline def S(
    z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Short = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14))
    ).toShort
  inline def S(
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Short = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15))
    ).toShort

  inline def C(z0: Bit): Char = z0.toChar
  inline def C(z1: Bit, z0: Bit): Char = (z0 | (z1 << 1)).toChar
  inline def C(z2: Bit, z1: Bit, z0: Bit): Char = (z0 | (z1 << 1) | (z2 << 2)).toChar
  inline def C(z3: Bit, z2: Bit, z1: Bit, z0: Bit): Char = (z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)).toChar
  inline def C(z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Char =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | (z4 << 4)).toChar
  inline def C(z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Char =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5))).toChar
  inline def C(z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Char =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6))).toChar
  inline def C(z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Char =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7))).toChar
  inline def C(
    z8: Bit,
    z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Char = 
    (
      ((z0     ) | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8))
    ).toChar
  inline def C(
    z9: Bit, z8: Bit,
    z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Char = 
    (
      ((z0     ) | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9))
    ).toChar
  inline def C(
    z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,  z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Char = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10))
    ).toChar
  inline def C(
    z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Char = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11))
    ).toChar
  inline def C(
    z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Char = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12))
    ).toChar
  inline def C(
    z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Char = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13))
    ).toChar
  inline def C(
    z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Char = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14))
    ).toChar
  inline def C(
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Char = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15))
    ).toChar

  inline def I(z0: Bit): Int = z0
  inline def I(z1: Bit, z0: Bit): Int = (z0 | (z1 << 1))
  inline def I(z2: Bit, z1: Bit, z0: Bit): Int = (z0 | (z1 << 1) | (z2 << 2))
  inline def I(z3: Bit, z2: Bit, z1: Bit, z0: Bit): Int = (z0 | (z1 << 1) | (z2 << 2) | (z3 << 3))
  inline def I(z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Int =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | (z4 << 4))
  inline def I(z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Int =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5)))
  inline def I(z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Int =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6)))
  inline def I(z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Int =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)))
  inline def I(
    z8: Bit,
    z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      ((z0     ) | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8))
    )
  inline def I(
    z9: Bit, z8: Bit,
    z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      ((z0     ) | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9))
    )
  inline def I(
    z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,  z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10))
    )
  inline def I(
    z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11))
    )
  inline def I(
    z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | (z5 << 5) | (z6 << 6) | (z7 << 7) )|
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12))
    )
  inline def I(
    z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13))
    )
  inline def I(
    z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14))
    )
  inline def I(
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15))
    )
  inline def I(
    z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      (( z0      ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16))
    )
  inline def I(
    z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17))
    )
  inline def I(
    z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18))
    )
  inline def I(
    z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19))
    )
  inline def I(
    z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | (z20 << 20)
    )
  inline def I(
    z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21))
    )
  inline def I(
    z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit, z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22))
    )
  inline def I(
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23))
    )
  inline def I(
    z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24))
    )
  inline def I(
    z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25))
    )
  inline def I(
    z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26))
    )
  inline def I(
    z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27))
    )
  inline def I(
    z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | (z28 << 28)
    )
  inline def I(
    z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29))
    )
  inline def I(
    z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30))
    )
  inline def I(
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Int = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | (z31 << 31))
    )


  inline def L(z0: Bit): Long = z0
  inline def L(z1: Bit, z0: Bit): Long = (z0 | (z1 << 1))
  inline def L(z2: Bit, z1: Bit, z0: Bit): Long = (z0 | (z1 << 1) | (z2 << 2))
  inline def L(z3: Bit, z2: Bit, z1: Bit, z0: Bit): Long = (z0 | (z1 << 1) | (z2 << 2) | (z3 << 3))
  inline def L(z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Long =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | (z4 << 4))
  inline def L(z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Long =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5)))
  inline def L(z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Long =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6)))
  inline def L(z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit): Long =
    ((z0 | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)))
  inline def L(
    z8: Bit,
    z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      ((z0     ) | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8))
    )
  inline def L(
    z9: Bit, z8: Bit,
    z7: Bit, z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      ((z0     ) | (z1 << 1) | (z2 << 2) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9))
    )
  inline def L(
    z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,  z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | (z3 << 3)) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10))
    )
  inline def L(
    z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit, z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | ((z4 << 4) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11))
    )
  inline def L(
    z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit, z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | (z5 << 5) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12))
    )
  inline def L(
    z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit, z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | (z6 << 6) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13))
    )
  inline def L(
    z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit, z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | (z7 << 7)) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14))
    )
  inline def L(
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      ((z0     ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      ((z8 << 8) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15))
    )
  inline def L(
    z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      (( z0      ) | (z1 << 1) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | (z9 << 9) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16))
    )
  inline def L(
    z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17))
    )
  inline def L(
    z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18))
    )
  inline def L(
    z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19))
    )
  inline def L(
    z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20))
    )
  inline def L(
    z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit, z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit, z1: Bit, z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21))
    )
  inline def L(
    z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit, z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit, z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22))
    )
  inline def L(
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23))
    )
  inline def L(
    z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24))
    )
  inline def L(
    z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25))
    )
  inline def L(
    z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26))
    )
  inline def L(
    z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27))
    )
  inline def L(
    z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28))
    )
  inline def L(
    z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29))
    )
  inline def L(
    z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long = 
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30))
    )
  inline def L(
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7: Bit,   z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31))
    ) & 0xFFFFFFFFL
  inline def L(
    z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32))
    )
  inline def L(
    z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33))
    )
  inline def L(
    z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34))
    )
  inline def L(
    z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35))
    )
  inline def L(
    z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36))
    )
  inline def L(
    z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37))
    )
  inline def L(
    z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38))
    )
  inline def L(
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39))
    )
  inline def L(
    z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40))
    )
  inline def L(
    z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
       (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41))
    )
  inline def L(
    z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42))
    )
  inline def L(
    z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43))
    )
  inline def L(
    z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44))
    )
  inline def L(
    z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45))
    )
  inline def L(
    z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46))
    )
  inline def L(
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47))
    )
  inline def L(
    z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48))
    )
  inline def L(
    z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49))
    )
  inline def L(
    z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50))
    )
  inline def L(
    z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51))
    )
  inline def L(
    z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52))
    )
  inline def L(
    z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53))
    )
  inline def L(
    z54: Bit, z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53) | ((z54+0L) << 54))
    )
  inline def L(
    z55: Bit, z54: Bit, z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53) | ((z54+0L) << 54) | ((z55+0L) << 55))
    )
  inline def L(
    z56: Bit,
    z55: Bit, z54: Bit, z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53) | ((z54+0L) << 54) | ((z55+0L) << 55)) |
      (((z56+0L) << 56))
    )
  inline def L(
    z57: Bit, z56: Bit,
    z55: Bit, z54: Bit, z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53) | ((z54+0L) << 54) | ((z55+0L) << 55)) |
      (((z56+0L) << 56) | ((z57+0L) << 57))
    )
  inline def L(
    z58: Bit, z57: Bit, z56: Bit,
    z55: Bit, z54: Bit, z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53) | ((z54+0L) << 54) | ((z55+0L) << 55)) |
      (((z56+0L) << 56) | ((z57+0L) << 57) | ((z58+0L) << 58))
    )
  inline def L(
    z59: Bit, z58: Bit, z57: Bit, z56: Bit,
    z55: Bit, z54: Bit, z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53) | ((z54+0L) << 54) | ((z55+0L) << 55)) |
      (((z56+0L) << 56) | ((z57+0L) << 57) | ((z58+0L) << 58) | ((z59+0L) << 59))
    )
  inline def L(
    z60: Bit, z59: Bit, z58: Bit, z57: Bit, z56: Bit,
    z55: Bit, z54: Bit, z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53) | ((z54+0L) << 54) | ((z55+0L) << 55)) |
      (((z56+0L) << 56) | ((z57+0L) << 57) | ((z58+0L) << 58) | ((z59+0L) << 59)) |
      (((z60+0L) << 60))
    )
  inline def L(
    z61: Bit, z60: Bit, z59: Bit, z58: Bit, z57: Bit, z56: Bit,
    z55: Bit, z54: Bit, z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53) | ((z54+0L) << 54) | ((z55+0L) << 55)) |
      (((z56+0L) << 56) | ((z57+0L) << 57) | ((z58+0L) << 58) | ((z59+0L) << 59)) |
      (((z60+0L) << 60) | ((z61+0L) << 61))
    )
  inline def L(
    z62: Bit, z61: Bit, z60: Bit, z59: Bit, z58: Bit, z57: Bit, z56: Bit,
    z55: Bit, z54: Bit, z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53) | ((z54+0L) << 54) | ((z55+0L) << 55)) |
      (((z56+0L) << 56) | ((z57+0L) << 57) | ((z58+0L) << 58) | ((z59+0L) << 59)) |
      (((z60+0L) << 60) | ((z61+0L) << 61) | ((z62+0L) << 62))
    )
  inline def L(
    z63: Bit, z62: Bit, z61: Bit, z60: Bit, z59: Bit, z58: Bit, z57: Bit, z56: Bit,
    z55: Bit, z54: Bit, z53: Bit, z52: Bit, z51: Bit, z50: Bit, z49: Bit, z48: Bit,
    z47: Bit, z46: Bit, z45: Bit, z44: Bit, z43: Bit, z42: Bit, z41: Bit, z40: Bit,
    z39: Bit, z38: Bit, z37: Bit, z36: Bit, z35: Bit, z34: Bit, z33: Bit, z32: Bit,
    z31: Bit, z30: Bit, z29: Bit, z28: Bit, z27: Bit, z26: Bit, z25: Bit, z24: Bit,
    z23: Bit, z22: Bit, z21: Bit, z20: Bit, z19: Bit, z18: Bit, z17: Bit, z16: Bit,
    z15: Bit, z14: Bit, z13: Bit, z12: Bit, z11: Bit, z10: Bit,  z9: Bit,  z8: Bit,
    z7:  Bit,  z6: Bit,  z5: Bit,  z4: Bit,  z3: Bit,  z2: Bit,  z1: Bit,  z0: Bit
  ): Long =
    (
      (( z0      ) | ( z1 << 1 ) | ( z2 << 2 ) | ( z3 << 3 )) | (( z4 << 4 ) | ( z5 << 5 ) | ( z6 << 6 ) | ( z7 << 7 )) |
      (( z8 << 8 ) | ( z9 << 9 ) | (z10 << 10) | (z11 << 11)) | ((z12 << 12) | (z13 << 13) | (z14 << 14) | (z15 << 15)) |
      ((z16 << 16) | (z17 << 17) | (z18 << 18) | (z19 << 19)) | ((z20 << 20) | (z21 << 21) | (z22 << 22) | (z23 << 23)) |
      ((z24 << 24) | (z25 << 25) | (z26 << 26) | (z27 << 27)) | ((z28 << 28) | (z29 << 29) | (z30 << 30) | ((z31+0L) << 31)) |
      (((z32+0L) << 32) | ((z33+0L) << 33) | ((z34+0L) << 34) | ((z35+0L) << 35)) |
      (((z36+0L) << 36) | ((z37+0L) << 37) | ((z38+0L) << 38) | ((z39+0L) << 39)) |
      (((z40+0L) << 40) | ((z41+0L) << 41) | ((z42+0L) << 42) | ((z43+0L) << 43)) |
      (((z44+0L) << 44) | ((z45+0L) << 45) | ((z46+0L) << 46) | ((z47+0L) << 47)) |
      (((z48+0L) << 48) | ((z49+0L) << 49) | ((z50+0L) << 50) | ((z51+0L) << 51)) |
      (((z52+0L) << 52) | ((z53+0L) << 53) | ((z54+0L) << 54) | ((z55+0L) << 55)) |
      (((z56+0L) << 56) | ((z57+0L) << 57) | ((z58+0L) << 58) | ((z59+0L) << 59)) |
      (((z60+0L) << 60) | ((z61+0L) << 61) | ((z62+0L) << 62) | ((z63+0L) << 63))
    )

  inline def flagI(inline i: BitIndices.I): Int = (1 << i)
  inline def flagI(inline i: BitIndices.I, inline j: BitIndices.I|32): Int = (0xFFFFFFFF >>> (32 - j)) & (0xFFFFFFFF << i)
  inline def maskI(inline i: BitIndices.I): Int = ~(1 << i)
  inline def maskI(inline i: BitIndices.I, inline j: BitIndices.I|32): Int = (0xFFFFFFFF << j) | (0xFFFFFFFF >>> (32 - i))

  inline def flagL(inline i: BitIndices.L): Long = (1L << i)
  inline def flagL(inline i: BitIndices.L, inline j: BitIndices.L|64): Long = (0xFFFFFFFFFFFFFFFFL >>> (64 - j)) & (0xFFFFFFFFFFFFFFFFL << i)
  inline def maskL(inline i: BitIndices.L): Long = ~(1L << i)
  inline def maskL(inline i: BitIndices.L, inline j: BitIndices.L|64): Long = (0xFFFFFFFFFFFFFFFFL << j) | (0xFFFFFFFFFFFFFFFFL >>> (64 - i))
}


type Hex = 0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15
object Hex {
  type CharLiterals = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F'|'a'|'b'|'c'|'d'|'e'|'f'

  type BinaryLiterals = 0|1|10|11|100|101|110|111|1000|1001|1010|1011|1100|1101|1110|1111

  inline def zero: Hex = 0

  inline def apply(h: Hex): Hex = h
  def flip(h: Hex): Hex = (15 - h).asInstanceOf[Hex]

  transparent inline def from(inline h: CharLiterals): Hex = inline h match
    case _: '0' => 0
    case _: '1' => 1
    case _: '2' => 2
    case _: '3' => 3
    case _: '4' => 4
    case _: '5' => 5
    case _: '6' => 6
    case _: '7' => 7
    case _: '8' => 8
    case _: '9' => 9
    case _: 'A' => 10
    case _: 'B' => 11
    case _: 'C' => 12
    case _: 'D' => 13
    case _: 'E' => 14
    case _: 'F' => 15
    case _: 'a' => 10
    case _: 'b' => 11
    case _: 'c' => 12
    case _: 'd' => 13
    case _: 'e' => 14
    case _: 'f' => 15

  transparent inline def literal(inline h: BinaryLiterals): Hex = inline h match
    case _:    0 => 0
    case _:    1 => 1
    case _:   10 => 2
    case _:   11 => 3
    case _:  100 => 4
    case _:  101 => 5
    case _:  110 => 6
    case _:  111 => 7
    case _: 1000 => 8
    case _: 1001 => 9
    case _: 1010 => 10
    case _: 1011 => 11
    case _: 1100 => 12
    case _: 1101 => 13
    case _: 1110 => 14
    case _: 1111 => 15

  inline def B(h0: Hex): Byte = h0.toByte
  inline def B(h1: Hex, h0: Hex): Byte = (h0 | (h1 << 4)).toByte
  inline def B(inline h0: CharLiterals): Byte = from(h0).toByte
  inline def B(inline h1: CharLiterals, inline h0: CharLiterals): Byte = (from(h0) | (from(h1) << 4)).toByte
  inline def literalB(inline h1: BinaryLiterals, inline h0: BinaryLiterals) = (literal(h0) | (literal(h1) << 4)).toByte

  inline def S(h0: Hex): Short = h0.toShort
  inline def S(h1: Hex, h0: Hex): Short = (h0 | (h1 << 4)).toShort
  inline def S(h2: Hex, h1: Hex, h0: Hex): Short = (h0 | (h1 << 4) | (h2 << 8)).toShort
  inline def S(h3: Hex, h2: Hex, h1: Hex, h0: Hex): Short = (h0 | (h1 << 4) | (h2 << 8) | (h3 << 12)).toShort
  inline def S(inline h0: CharLiterals): Short = from(h0).toShort
  inline def S(inline h1: CharLiterals, inline h0: CharLiterals): Short = (from(h0) | (from(h1) << 4)).toShort
  inline def S(inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals): Short =
    (from(h0) | (from(h1) << 4) | (from(h2) << 8)).toShort
  inline def S(inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals): Short =
    (from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12)).toShort
  inline def literalS(inline h3: BinaryLiterals, inline h2: BinaryLiterals, inline h1: BinaryLiterals, inline h0: BinaryLiterals): Short =
    (literal(h0) | (literal(h1) << 4) | (literal(h2) << 8) | (literal(h3) << 12)).toShort

  inline def C(h0: Hex): Char = h0.toChar
  inline def C(h1: Hex, h0: Hex): Char = (h0 | (h1 << 4)).toChar
  inline def C(h2: Hex, h1: Hex, h0: Hex): Char = (h0 | (h1 << 4) | (h2 << 8)).toChar
  inline def C(h3: Hex, h2: Hex, h1: Hex, h0: Hex): Char = (h0 | (h1 << 4) | (h2 << 8) | (h3 << 12)).toChar
  inline def C(inline h0: CharLiterals): Char = from(h0).toChar
  inline def C(inline h1: CharLiterals, inline h0: CharLiterals): Char = (from(h0) | (from(h1) << 4)).toChar
  inline def C(inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals): Char =
    (from(h0) | (from(h1) << 4) | (from(h2) << 8)).toChar
  inline def C(inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals): Char =
    (from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12)).toChar
  inline def literalC(inline h3: BinaryLiterals, inline h2: BinaryLiterals, inline h1: BinaryLiterals, inline h0: BinaryLiterals): Char =
    (literal(h0) | (literal(h1) << 4) | (literal(h2) << 8) | (literal(h3) << 12)).toChar

  inline def I(h0: Hex): Int = h0
  inline def I(h1: Hex, h0: Hex): Int = h0 | (h1 << 4)
  inline def I(h2: Hex, h1: Hex, h0: Hex): Int = h0 | (h1 << 4) | (h2 << 8)
  inline def I(h3: Hex, h2: Hex, h1: Hex, h0: Hex): Int = h0 | (h1 << 4) | (h2 << 8) | (h3 << 12)
  inline def I(h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex): Int = 
    h0 | (h1 << 4) | (h2 << 8) | (h3 << 12) | (h4 << 16)
  inline def I(h5: Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex): Int = 
    h0 | (h1 << 4) | (h2 << 8) | (h3 << 12) | (h4 << 16) | (h5 << 20)
  inline def I(h6: Hex, h5: Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex): Int = 
    h0 | (h1 << 4) | (h2 << 8) | (h3 << 12) | (h4 << 16) | (h5 << 20) | (h6 << 24)
  inline def I(h7: Hex, h6: Hex, h5: Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex): Int = 
    h0 | (h1 << 4) | (h2 << 8) | (h3 << 12) | (h4 << 16) | (h5 << 20) | (h6 << 24) | (h7 << 28)
  inline def I(inline h0: CharLiterals): Int = from(h0)
  inline def I(inline h1: CharLiterals, inline h0: CharLiterals): Int = from(h0) | (from(h1) << 4)
  inline def I(inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals): Int =
    from(h0) | (from(h1) << 4) | (from(h2) << 8)
  inline def I(inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals): Int =
    from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12)
  inline def I(
    inline h4: CharLiterals,
    inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Int =
    from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12) | (from(h4) << 16)
  inline def I(
    inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Int =
    from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12) | (from(h4) << 16) | (from(h5) << 20)
  inline def I(
    inline h6: CharLiterals, inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Int =
    from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12) | (from(h4) << 16) | (from(h5) << 20) | (from(h6) << 24)
  inline def I(
    inline h7: CharLiterals, inline h6: CharLiterals, inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Int =
    from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12) | (from(h4) << 16) | (from(h5) << 20) | (from(h6) << 24) | (from(h7) << 28)
  inline def literalI(
    inline h7: BinaryLiterals, inline h6: BinaryLiterals, inline h5: BinaryLiterals, inline h4: BinaryLiterals,
    inline h3: BinaryLiterals, inline h2: BinaryLiterals, inline h1: BinaryLiterals, inline h0: BinaryLiterals
  ): Int =
    (literal(h0)      ) | (literal(h1) <<  4) | (literal(h2) <<  8) | (literal(h3) << 12) |
    (literal(h4) << 16) | (literal(h5) << 20) | (literal(h6) << 24) | (literal(h7) << 28)

  inline def L(h0: Hex): Long = h0
  inline def L(h1: Hex, h0: Hex): Long = h0 | (h1 << 4)
  inline def L(h2: Hex, h1: Hex, h0: Hex): Long = h0 | (h1 << 4) | (h2 << 8)
  inline def L(h3: Hex, h2: Hex, h1: Hex, h0: Hex): Long = h0 | (h1 << 4) | (h2 << 8) | (h3 << 12)
  inline def L(h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex): Long = 
    h0 | (h1 << 4) | (h2 << 8) | (h3 << 12) | (h4 << 16)
  inline def L(h5: Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex): Long = 
    h0 | (h1 << 4) | (h2 << 8) | (h3 << 12) | (h4 << 16) | (h5 << 20)
  inline def L(h6: Hex, h5: Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex): Long = 
    h0 | (h1 << 4) | (h2 << 8) | (h3 << 12) | (h4 << 16) | (h5 << 20) | (h6 << 24)
  inline def L(h7: Hex, h6: Hex, h5: Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex): Long = 
    h0 | (h1 << 4) | (h2 << 8) | (h3 << 12) | (h4 << 16) | (h5 << 20) | (h6 << 24) | ((h7+0L) << 28)
  inline def L(
    h8: Hex,
    h7: Hex, h6: Hex, h5: Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex
  ): Long = 
    ( h0          ) | (h1 << 4 ) | (h2 << 8 ) | ( h3     << 12) |
    ( h4     << 16) | (h5 << 20) | (h6 << 24) | ((h7+0L) << 28) |
    ((h8+0L) << 32)
  inline def L(
    h9: Hex, h8: Hex,
    h7: Hex, h6: Hex, h5: Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex
  ): Long = 
    ( h0          ) | ( h1     << 4 ) | (h2 << 8 ) | ( h3     << 12) |
    ( h4     << 16) | ( h5     << 20) | (h6 << 24) | ((h7+0L) << 28) |
    ((h8+0L) << 32) | ((h9+0L) << 36)
  inline def L(
    h10: Hex, h9: Hex, h8: Hex,
    h7:  Hex, h6: Hex, h5: Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex
  ): Long = 
    ( h0          ) | ( h1     << 4 ) | ( h2      << 8 ) | ( h3     << 12) |
    ( h4     << 16) | ( h5     << 20) | ( h6      << 24) | ((h7+0L) << 28) |
    ((h8+0L) << 32) | ((h9+0L) << 36) | ((h10+0L) << 40)
  inline def L(
    h11: Hex, h10: Hex, h9: Hex, h8: Hex,
    h7:  Hex, h6:  Hex, h5: Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex
  ): Long = 
    ( h0          ) | ( h1     << 4 ) | ( h2      << 8 ) | ( h3      << 12) |
    ( h4     << 16) | ( h5     << 20) | ( h6      << 24) | ((h7 +0L) << 28) |
    ((h8+0L) << 32) | ((h9+0L) << 36) | ((h10+0L) << 40) | ((h11+0L) << 44)
  inline def L(
    h12: Hex, h11: Hex, h10: Hex, h9: Hex, h8: Hex,
    h7:  Hex, h6:  Hex, h5:  Hex, h4: Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex
  ): Long = 
    ( h0           ) | ( h1     << 4 ) | ( h2      << 8 ) | ( h3      << 12) |
    ( h4      << 16) | ( h5     << 20) | ( h6      << 24) | ((h7 +0L) << 28) |
    ((h8 +0L) << 32) | ((h9+0L) << 36) | ((h10+0L) << 40) | ((h11+0L) << 44) |
    ((h12+0L) << 48)
  inline def L(
    h13: Hex, h12: Hex, h11: Hex, h10: Hex, h9: Hex, h8: Hex,
    h7:  Hex, h6:  Hex, h5:  Hex, h4:  Hex, h3: Hex, h2: Hex, h1: Hex, h0: Hex
  ): Long = 
    ( h0           ) | ( h1      << 4 ) | ( h2      << 8 ) | ( h3      << 12) |
    ( h4      << 16) | ( h5      << 20) | ( h6      << 24) | ((h7 +0L) << 28) |
    ((h8 +0L) << 32) | ((h9 +0L) << 36) | ((h10+0L) << 40) | ((h11+0L) << 44) |
    ((h12+0L) << 48) | ((h13+0L) << 52)
  inline def L(
    h14: Hex, h13: Hex, h12: Hex, h11: Hex, h10: Hex, h9: Hex, h8: Hex,
    h7:  Hex, h6:  Hex, h5:  Hex, h4:  Hex, h3:  Hex, h2: Hex, h1: Hex, h0: Hex
  ): Long = 
    ( h0           ) | ( h1      << 4 ) | ( h2      << 8 ) | ( h3      << 12) |
    ( h4      << 16) | ( h5      << 20) | ( h6      << 24) | ((h7 +0L) << 28) |
    ((h8 +0L) << 32) | ((h9 +0L) << 36) | ((h10+0L) << 40) | ((h11+0L) << 44) |
    ((h12+0L) << 48) | ((h13+0L) << 52) | ((h14+0L) << 56)
  inline def L(
    h15: Hex, h14: Hex, h13: Hex, h12: Hex, h11: Hex, h10: Hex, h9: Hex, h8: Hex,
    h7:  Hex, h6:  Hex, h5:  Hex, h4:  Hex, h3:  Hex, h2:  Hex, h1: Hex, h0: Hex
  ): Long = 
    ( h0           ) | ( h1      << 4 ) | ( h2      << 8 ) | ( h3      << 12) |
    ( h4      << 16) | ( h5      << 20) | ( h6      << 24) | ((h7 +0L) << 28) |
    ((h8 +0L) << 32) | ((h9 +0L) << 36) | ((h10+0L) << 40) | ((h11+0L) << 44) |
    ((h12+0L) << 48) | ((h13+0L) << 52) | ((h14+0L) << 56) | ((h15+0L) << 60)
  inline def L(inline h0: CharLiterals): Long = from(h0)
  inline def L(inline h1: CharLiterals, inline h0: CharLiterals): Long = from(h0) | (from(h1) << 4)
  inline def L(inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals): Long =
    from(h0) | (from(h1) << 4) | (from(h2) << 8)
  inline def L(inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals): Long =
    from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12)
  inline def L(
    inline h4: CharLiterals,
    inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Long =
    from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12) | (from(h4) << 16)
  inline def L(
    inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Long =
    from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12) | (from(h4) << 16) | (from(h5) << 20)
  inline def L(
    inline h6: CharLiterals, inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Long =
    from(h0) | (from(h1) << 4) | (from(h2) << 8) | (from(h3) << 12) | (from(h4) << 16) | (from(h5) << 20) | (from(h6) << 24)
  inline def L(
    inline h7: CharLiterals, inline h6: CharLiterals, inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Long =
    (from(h0)      ) | (from(h1) << 4 ) | (from(h2) << 8 ) | ( from(h3)     << 12) |
    (from(h4) << 16) | (from(h5) << 20) | (from(h6) << 24) | ((from(h7)+0L) << 28)
  inline def L(
    inline h8: CharLiterals,
    inline h7: CharLiterals, inline h6: CharLiterals, inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Long =
    ( from(h0)          ) | (from(h1) << 4 ) | (from(h2) << 8 ) | ( from(h3)     << 12) |
    ( from(h4)     << 16) | (from(h5) << 20) | (from(h6) << 24) | ((from(h7)+0L) << 28) |
    ((from(h8)+0L) << 32)
  inline def L(
    inline h9: CharLiterals, inline h8: CharLiterals,
    inline h7: CharLiterals, inline h6: CharLiterals, inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3: CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Long =
    ( from(h0)          ) | ( from(h1)     << 4 ) | (from(h2) << 8 ) | ( from(h3)     << 12) |
    ( from(h4)     << 16) | ( from(h5)     << 20) | (from(h6) << 24) | ((from(h7)+0L) << 28) |
    ((from(h8)+0L) << 32) | ((from(h9)+0L) << 36)
  inline def L(
    inline h10: CharLiterals, inline h9: CharLiterals, inline h8: CharLiterals,
    inline h7:  CharLiterals, inline h6: CharLiterals, inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3:  CharLiterals, inline h2: CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Long =
    ( from(h0)          ) | ( from(h1)     << 4 ) | ( from(h2 )     << 8 ) | ( from(h3)     << 12) |
    ( from(h4)     << 16) | ( from(h5)     << 20) | ( from(h6 )     << 24) | ((from(h7)+0L) << 28) |
    ((from(h8)+0L) << 32) | ((from(h9)+0L) << 36) | ((from(h10)+0L) << 40)
  inline def L(
    inline h11: CharLiterals, inline h10: CharLiterals, inline h9: CharLiterals, inline h8: CharLiterals,
    inline h7:  CharLiterals, inline h6:  CharLiterals, inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3:  CharLiterals, inline h2:  CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Long =
    ( from(h0)          ) | ( from(h1)     << 4 ) | ( from(h2 )     << 8 ) | ( from(h3 )     << 12) |
    ( from(h4)     << 16) | ( from(h5)     << 20) | ( from(h6 )     << 24) | ((from(h7 )+0L) << 28) |
    ((from(h8)+0L) << 32) | ((from(h9)+0L) << 36) | ((from(h10)+0L) << 40) | ((from(h11)+0L) << 44)
  inline def L(
    inline h12: CharLiterals,
    inline h11: CharLiterals, inline h10: CharLiterals, inline h9: CharLiterals, inline h8: CharLiterals,
    inline h7:  CharLiterals, inline h6:  CharLiterals, inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3:  CharLiterals, inline h2:  CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Long =
    ( from(h0 )          ) | ( from(h1)     << 4 ) | ( from(h2 )     << 8 ) | ( from(h3 )     << 12) |
    ( from(h4 )     << 16) | ( from(h5)     << 20) | ( from(h6 )     << 24) | ((from(h7 )+0L) << 28) |
    ((from(h8 )+0L) << 32) | ((from(h9)+0L) << 36) | ((from(h10)+0L) << 40) | ((from(h11)+0L) << 44) |
    ((from(h12)+0L) << 48)
  inline def L(
    inline h13: CharLiterals, inline h12: CharLiterals,
    inline h11: CharLiterals, inline h10: CharLiterals, inline h9: CharLiterals, inline h8: CharLiterals,
    inline h7:  CharLiterals, inline h6:  CharLiterals, inline h5: CharLiterals, inline h4: CharLiterals,
    inline h3:  CharLiterals, inline h2:  CharLiterals, inline h1: CharLiterals, inline h0: CharLiterals
  ): Long =
    ( from(h0 )          ) | ( from(h1 )     << 4 ) | ( from(h2 )     << 8 ) | ( from(h3 )     << 12) |
    ( from(h4 )     << 16) | ( from(h5 )     << 20) | ( from(h6 )     << 24) | ((from(h7 )+0L) << 28) |
    ((from(h8 )+0L) << 32) | ((from(h9 )+0L) << 36) | ((from(h10)+0L) << 40) | ((from(h11)+0L) << 44) |
    ((from(h12)+0L) << 48) | ((from(h13)+0L) << 52)
  inline def L(
    inline h14: CharLiterals, inline h13: CharLiterals, inline h12: CharLiterals,
    inline h11: CharLiterals, inline h10: CharLiterals, inline h9:  CharLiterals, inline h8: CharLiterals,
    inline h7:  CharLiterals, inline h6:  CharLiterals, inline h5:  CharLiterals, inline h4: CharLiterals,
    inline h3:  CharLiterals, inline h2:  CharLiterals, inline h1:  CharLiterals, inline h0: CharLiterals
  ): Long =
    ( from(h0 )          ) | ( from(h1 )     << 4 ) | ( from(h2 )     << 8 ) | ( from(h3 )     << 12) |
    ( from(h4 )     << 16) | ( from(h5 )     << 20) | ( from(h6 )     << 24) | ((from(h7 )+0L) << 28) |
    ((from(h8 )+0L) << 32) | ((from(h9 )+0L) << 36) | ((from(h10)+0L) << 40) | ((from(h11)+0L) << 44) |
    ((from(h12)+0L) << 48) | ((from(h13)+0L) << 52) | ((from(h14)+0L) << 56)
  inline def L(
    inline h15: CharLiterals, inline h14: CharLiterals, inline h13: CharLiterals, inline h12: CharLiterals,
    inline h11: CharLiterals, inline h10: CharLiterals, inline h9:  CharLiterals, inline h8: CharLiterals,
    inline h7:  CharLiterals, inline h6:  CharLiterals, inline h5:  CharLiterals, inline h4: CharLiterals,
    inline h3:  CharLiterals, inline h2:  CharLiterals, inline h1:  CharLiterals, inline h0: CharLiterals
  ): Long =
    ( from(h0 )          ) | ( from(h1 )     << 4 ) | ( from(h2 )     << 8 ) | ( from(h3 )     << 12) |
    ( from(h4 )     << 16) | ( from(h5 )     << 20) | ( from(h6 )     << 24) | ((from(h7 )+0L) << 28) |
    ((from(h8 )+0L) << 32) | ((from(h9 )+0L) << 36) | ((from(h10)+0L) << 40) | ((from(h11)+0L) << 44) |
    ((from(h12)+0L) << 48) | ((from(h13)+0L) << 52) | ((from(h14)+0L) << 56) | ((from(h15)+0L) << 60)
  inline def literalL(
    inline h15: BinaryLiterals, inline h14: BinaryLiterals, inline h13: BinaryLiterals, inline h12: BinaryLiterals,
    inline h11: BinaryLiterals, inline h10: BinaryLiterals, inline h9:  BinaryLiterals, inline h8: BinaryLiterals,
    inline h7:  BinaryLiterals, inline h6:  BinaryLiterals, inline h5:  BinaryLiterals, inline h4: BinaryLiterals,
    inline h3:  BinaryLiterals, inline h2:  BinaryLiterals, inline h1:  BinaryLiterals, inline h0: BinaryLiterals
  ): Long =
    ( literal(h0 )          ) | ( literal(h1 )     << 4 ) | ( literal(h2 )     << 8 ) | ( literal(h3 )     << 12) |
    ( literal(h4 )     << 16) | ( literal(h5 )     << 20) | ( literal(h6 )     << 24) | ((literal(h7 )+0L) << 28) |
    ((literal(h8 )+0L) << 32) | ((literal(h9 )+0L) << 36) | ((literal(h10)+0L) << 40) | ((literal(h11)+0L) << 44) |
    ((literal(h12)+0L) << 48) | ((literal(h13)+0L) << 52) | ((literal(h14)+0L) << 56) | ((literal(h15)+0L) << 60)
}


object Pack {
  inline def S(b0: Byte, b1: Byte): Short = ((b0 & 0xFF) | ((b1 & 0xFF) << 8)).toShort

  inline def C(b0: Byte, b1: Byte): Char = ((b0 & 0xFF) | ((b1 & 0xFF) << 8)).toChar

  inline def I(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Int = (b0 & 0xFF) | ((b1 & 0xFF) << 8) | ((b2 & 0xFF) << 16) | (b3 << 24)
  inline def I(s0: Short, s1: Short): Int = (s0 & 0xFFFF) | (s1 << 16)
  inline def I(c0: Char, c1: Char): Int = c0 | (c1 << 16)

  inline def L(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): Long =
    (((b0 & 0xFF) | ((b1 & 0xFF) << 8) | ((b2 & 0xFF) << 16) | (b3 << 24)) & 0xFFFFFFFFL) |
    (((b4 & 0xFF) | ((b5 & 0xFF) << 8) | ((b6 & 0xFF) << 16) | (b7 << 24)).toLong << 32)
  inline def L(s0: Short, s1: Short, s2: Short, s3: Short): Long =
    (s0 & 0xFFFFL) | ((s1 & 0xFFFFL) << 16) | ((s2 & 0xFFFFL) << 32) | (s3.toLong << 48)
  inline def L(c0: Char, c1: Char, c2: Char, c3: Char): Long =
    c0.toLong | (c1.toLong << 16) | (c2.toLong << 32) | (c3.toLong << 48)
  inline def L(i0: Int, i1: Int): Long =
    (i0 & 0xFFFFFFFFL) | (i1.toLong << 32)
}


object BitIndices {
  type B = 0|1|2|3|4|5|6|7
  type S = 0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15
  type C = 0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15
  type I = 0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31
  type L =
    0  | 1  | 2  | 3  | 4  | 5  | 6  | 7  | 8  | 9  | 
    10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 |
    20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 |
    30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 |
    40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 |
    50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 |
    60 | 61 | 62 | 63
}
object HexIndices {
  type B = 0|1
  type S = 0|1|2|3
  type C = 0|1|2|3
  type I = 0|1|2|3|4|5|6|7
  type L = 0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15
}
object ByteIndices {
  type S = 0|1
  type C = 0|1
  type I = 0|1|2|3
  type L = 0|1|2|3|4|5|6|7
}
object ShortIndices {
  type I = 0|1
  type L = 0|1|2|3
}
object CharIndices {
  type I = 0|1
  type L = 0|1|2|3
}
object IntIndices {
  type L = 0|1
}


extension (z8: Byte) {
  inline def bit(index: BitIndices.B): Bit =
    ((z8 >>> index) & 1).asInstanceOf[Bit]

  inline def bits(i0: BitIndices.B, iN: BitIndices.B | 8): Byte =
    ((z8 & (0xFF >> (8 - iN))) >>> i0).toByte

  inline def bitTo(index: BitIndices.B)(value: Bit): Byte =
    ((z8 & (~(1 << index))) | (value << index)).toByte

  def bitsTo(i0: BitIndices.B, iN: BitIndices.B | 8)(value: Byte): Byte =
    val m = (0xFF << i0) & (0xFF >> (8-iN))
    ((z8 & ~m) | ((value << i0) & m)).toByte

  def reverseBits: Byte =
    val h = ((z8 & 0xF0) >> 4) | ((z8 & 0xF) << 4)
    val q = ((h & 0xCC) >> 2) | ((h & 0x33) << 2)
    (((q & 0xAA) >> 1) | ((q & 0x55) << 1)).toByte

  def bitString: String =
    val ans = new Array[Char](8)
    val v = z8 & 0xFF
    var b = 0x80
    var i = 0
    while i < ans.length do
      ans(i) = if (v & b) == 0 then '0' else '1'
      b = b >>> 1
      i += 1
    new String(ans)

  inline def hex(index: HexIndices.B): Hex =
    ((z8 >>> 4*index) & 0xF).asInstanceOf[Hex]

  inline def hexTo(index: HexIndices.B)(value: Hex): Byte =
    ((z8 & (0xF0F >>> 4*(1-index))) | (value << 4*index)).toByte

  inline def reverseHex: Byte =
    (((z8 & 0xF0) >> 4) | ((z8 & 0xF) << 4)).toByte
}


extension (z16: Short) {
  inline def bit(index: BitIndices.S): Bit =
    ((z16 >>> index) & 1).asInstanceOf[Bit]

  inline def bits(i0: BitIndices.S, iN: BitIndices.S | 16): Short =
    ((z16 & (0xFFFF >> (16 - iN))) >>> i0).toShort

  inline def bitTo(index: BitIndices.S)(value: Bit): Short =
    ((z16 & (~(1 << index))) | (value << index)).toShort

  def bitsTo(i0: BitIndices.S, iN: BitIndices.S | 16)(value: Short): Short =
    val m = (0xFFFF << i0) & (0xFFFF >> (16-iN))
    ((z16 & ~m) | ((value << i0) & m)).toShort

  def reverseBits: Short =
    val b = ((z16 & 0xFF00) >> 8) | ((z16 & 0xFF) << 8)
    val h = ((b & 0xF0F0) >> 4) | ((b & 0x0F0F) << 4)
    val q = ((h & 0xCCCC) >> 2) | ((h & 0x3333) << 2)
    (((q & 0xAAAA) >> 1) | ((q & 0x5555) << 1)).toShort

  def bitString: String =
    val ans = new Array[Char](16)
    val v = z16 & 0xFFFF
    var b = 0x8000
    var i = 0
    while i < ans.length do
      ans(i) = if (v & b) == 0 then '0' else '1'
      b = b >>> 1
      i += 1
    new String(ans)

  inline def hex(index: HexIndices.S): Hex =
    ((z16 >>> 4*index) & 0xF).asInstanceOf[Hex]

  inline def hexTo(index: HexIndices.S)(value: Hex): Short =
    ((z16 & (0xFFF0FFF >>> 4*(3-index))) | (value << 4*index)).toShort

  inline def reverseHex: Short =
    val b = ((z16 & 0xFF00) >> 8) | ((z16 & 0xFF) << 8)
    (((b & 0xF0F0) >> 4) | ((b & 0x0F0F) << 4)).toShort

  inline def byte(index: ByteIndices.S): Byte =
    ((z16 >>> 8*index) & 0xFF).toByte

  inline def byteTo(index: ByteIndices.S)(value: Byte): Short =
    ((z16 & (0xFF00FF >>> 8*(1-index))) | ((value & 0xFF) << 8*index)).toShort

  inline def reverseBytes: Short =
    (((z16 & 0xFF00) >> 8) | ((z16 & 0xFF) << 8)).toShort
}


extension (z16: Char) {
  inline def bit(index: BitIndices.C): Bit =
    ((z16 >>> index) & 1).asInstanceOf[Bit]

  inline def bits(i0: BitIndices.C, iN: BitIndices.C | 16): Char =
    ((z16 & (0xFFFF >> (16 - iN))) >>> i0).toChar

  inline def bitTo(index: BitIndices.C)(value: Bit): Char =
    ((z16 & (~(1 << index))) | (value << index)).toChar

  def bitsTo(i0: BitIndices.C, iN: BitIndices.C | 16)(value: Char): Char =
    val m = (0xFFFF << i0) & (0xFFFF >> (16-iN))
    ((z16 & ~m) | ((value << i0) & m)).toChar

  def reverseBits: Char =
    val b = ((z16 & 0xFF00) >> 8) | ((z16 & 0xFF) << 8)
    val h = ((b & 0xF0F0) >> 4) | ((b & 0x0F0F) << 4)
    val q = ((h & 0xCCCC) >> 2) | ((h & 0x3333) << 2)
    (((q & 0xAAAA) >> 1) | ((q & 0x5555) << 1)).toChar

  def bitString: String =
    val ans = new Array[Char](16)
    var b = 0x8000
    var i = 0
    while i < ans.length do
      ans(i) = if (z16 & b) == 0 then '0' else '1'
      b = b >>> 1
      i += 1
    new String(ans)

  inline def hex(index: HexIndices.C): Hex =
    ((z16 >>> 4*index) & 0xF).asInstanceOf[Hex]

  inline def hexTo(index: HexIndices.C)(value: Hex): Char =
    ((z16 & (0xFFF0FFF >>> 4*(3-index))) | (value << 4*index)).toChar

  inline def reverseHex: Char =
    val b = ((z16 & 0xFF00) >> 8) | ((z16 & 0xFF) << 8)
    (((b & 0xF0F0) >> 4) | ((b & 0x0F0F) << 4)).toChar

  inline def byte(index: ByteIndices.C): Byte =
    ((z16 >>> 8*index) & 0xFF).toByte

  inline def byteTo(index: ByteIndices.C)(value: Byte): Char =
    ((z16 & (0xFF00FF >>> 8*(1-index))) | ((value & 0xFF) << 8*index)).toChar

  inline def reverseBytes: Char =
    (((z16 & 0xFF00) >> 8) | ((z16 & 0xFF) << 8)).toChar
}


extension (z32: Int) {
  inline def bit(index: BitIndices.I): Bit =
    ((z32 >>> index) & 1).asInstanceOf[Bit]

  inline def bits(i0: BitIndices.I, iN: BitIndices.I | 32): Int =
    (z32 & (0xFFFFFFFF >>> (32 - iN))) >>> i0

  inline def bitTo(index: BitIndices.I)(value: Bit): Int =
    (z32 & (~(1 << index))) | (value << index)

  def bitsTo(i0: BitIndices.I, iN: BitIndices.I | 32)(value: Int): Int =
    val m = (0xFFFFFFFF << i0) & (0xFFFFFFFF >>> (32-iN))
    (z32 & ~m) | ((value << i0) & m)

  def reverseBits: Int = java.lang.Integer.reverse(z32)

  def bitString: String =
    val ans = new Array[Char](32)
    var b = 0x80000000
    var i = 0
    while i < ans.length do
      ans(i) = if (z32 & b) == 0 then '0' else '1'
      b = b >>> 1
      i += 1
    new String(ans)

  inline def hex(index: HexIndices.I): Hex =
    ((z32 >>> 4*index) & 0xF).asInstanceOf[Hex]

  inline def hexTo(index: HexIndices.I)(value: Hex): Int =
    (z32 & ~(0xF << 4*index)) | (value << 4*index)

  def reverseHex: Int =
    val s = ((z32 & 0xFFFF0000) >>> 16) | ((z32 & 0xFFFF) << 16)
    val b = ((s & 0xFF00FF00) >>> 8) | ((s & 0x00FF00FF) << 8)
    ((b & 0xF0F0F0F0) >>> 4) | ((b & 0x0F0F0F0F) << 4)

  inline def byte(index: ByteIndices.I): Byte =
    ((z32 >>> 8*index) & 0xFF).toByte

  inline def byteTo(index: ByteIndices.I)(value: Byte): Int =
    (z32 & ~(0xFF << 8*index)) | ((value & 0xFF) << 8*index)

  inline def reverseBytes: Int = java.lang.Integer.reverseBytes(z32)

  inline def short(index: ShortIndices.I): Short =
    ((z32 >>> 16*index) & 0xFFFF).toShort

  inline def shortTo(index: ShortIndices.I)(value: Short): Int =
    (z32 & ~(0xFFFF << 16*index)) | ((value & 0xFFFF) << 16*index)

  inline def reverseShorts: Int =
    ((z32 & 0xFFFF0000) >>> 16) | ((z32 & 0xFFFF) << 16)

  inline def char(index: CharIndices.I): Char =
    ((z32 >>> 16*index) & 0xFFFF).toChar

  inline def charTo(index: CharIndices.I)(value: Char): Int =
    (z32 & ~(0xFFFF << 16*index)) | ((value + 0) << 16*index)

  inline def reverseChars: Int = reverseShorts
}


extension (z64: Long) {
  inline def bit(index: BitIndices.L): Bit =
    ((z64 >>> index) & 1).toInt.asInstanceOf[Bit]

  inline def bits(i0: BitIndices.L, iN: BitIndices.L | 64): Long =
    (z64 & (0xFFFFFFFFFFFFFFFFL >>> (64 - iN))) >>> i0

  inline def bitTo(index: BitIndices.L)(value: Bit): Long =
    (z64 & (~(1L << index))) | ((value+0L) << index)

  def bitsTo(i0: BitIndices.L, iN: BitIndices.L | 64)(value: Long): Long =
    val m = (0xFFFFFFFFFFFFFFFFL << i0) & (0xFFFFFFFFFFFFFFFFL >>> (64-iN))
    (z64 & ~m) | (((value+0L) << i0) & m)

  def reverseBits: Long = java.lang.Long.reverse(z64)

  def bitString: String =
    val ans = new Array[Char](64)
    var b = 0x8000000000000000L
    var i = 0
    while i < ans.length do
      ans(i) = if (z64 & b) == 0 then '0' else '1'
      b = b >>> 1
      i += 1
    new String(ans)

  inline def hex(index: HexIndices.L): Hex =
    ((z64 >>> 4*index) & 0xF).asInstanceOf[Hex]

  inline def hexTo(index: HexIndices.L)(value: Hex): Long =
    (z64 & ~(0xFL << 4*index)) | ((value+0L) << 4*index)

  def reverseHex: Long =
    val i = ((z64 & 0xFFFFFFFF00000000L) >>> 32) | ((z64 & 0x00000000FFFFFFFFL) << 32)
    val s = (( i  & 0xFFFF0000FFFF0000L) >>> 16) | (( i  & 0x0000FFFF0000FFFFL) << 16)
    val b = (( s  & 0xFF00FF00FF00FF00L) >>>  8) | (( s  & 0x00FF00FF00FF00FFL) <<  8)
    (       (( b  & 0xF0F0F0F0F0F0F0F0L) >>>  4) | (( b  & 0x0F0F0F0F0F0F0F0FL) <<  4) )

  inline def byte(index: ByteIndices.L): Byte =
    ((z64 >>> 8*index) & 0xFF).toByte

  inline def byteTo(index: ByteIndices.L)(value: Byte): Long =
    (z64 & ~(0xFFL << 8*index)) | ((value & 0xFFL) << 8*index)

  def reverseBytes: Long = java.lang.Long.reverseBytes(z64)

  inline def short(index: ShortIndices.L): Short =
    ((z64 >>> 16*index) & 0xFFFF).toShort

  inline def shortTo(index: ShortIndices.L)(value: Short): Long =
    (z64 & ~(0xFFFFL << 16*index)) | ((value & 0xFFFFL) << 16*index)

  inline def reverseShorts: Long =
    val i = ((z64 & 0xFFFFFFFF00000000L) >>> 32) | ((z64 & 0x00000000FFFFFFFFL) << 32)
    (       (( i  & 0xFFFF0000FFFF0000L) >>> 16) | (( i  & 0x0000FFFF0000FFFFL) << 16) )

  inline def char(index: CharIndices.L): Char =
    ((z64 >>> 16*index) & 0xFFFF).toChar

  inline def charTo(index: CharIndices.L)(value: Char): Long =
    (z64 & ~(0xFFFFL << 16*index)) | ((value + 0L) << 16*index)

  inline def reverseChars: Long = reverseShorts

  inline def int(index: IntIndices.L): Int =
    ((z64 >>> 32*index) & 0xFFFFFFFFL).toInt

  inline def intTo(index: IntIndices.L)(value: Int): Long =
    (z64 & ~(0xFFFFFFFFL << 32*index)) | ((value & 0xFFFFFFFFL) << 32*index)

  inline def reverseInts: Long =
    ((z64 & 0xFFFFFFFF00000000L) >>> 32) | ((z64 & 0x00000000FFFFFFFFL) << 32)
}
