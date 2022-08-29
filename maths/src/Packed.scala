package kse.maths.packed


import java.lang.{Math => jm}

import scala.annotation.targetName



opaque type Bitx8 = Byte
object Bitx8 {
  inline def apply(b: Byte):  kse.maths.packed.Bitx8 = b
  final def of(
    z0:  Boolean, z1:  Boolean, z2:  Boolean, z3:  Boolean, z4:  Boolean, z5:  Boolean, z6:  Boolean, z7:  Boolean,
  ): Bitx8 = (
    (if (z0)  0x1 else 0) | (if (z1)  0x2L else 0) |
    (if (z2)  0x4 else 0) | (if (z3)  0x8L else 0) |
    (if (z4) 0x10 else 0) | (if (z5) 0x20L else 0) |
    (if (z6) 0x40 else 0) | (if (z7) 0x80L else 0)
  ).toByte
}
extension (z8: Bitx8) {
  inline def B: Byte = z8

  inline def bit0 = (z8 & 0x01) != 0
  inline def bit1 = (z8 & 0x02) != 0
  inline def bit2 = (z8 & 0x04) != 0
  inline def bit3 = (z8 & 0x08) != 0
  inline def bit4 = (z8 & 0x10) != 0
  inline def bit5 = (z8 & 0x20) != 0
  inline def bit6 = (z8 & 0x40) != 0
  inline def bit7 = (z8 & 0x80) != 0
  inline def bit(i: Int) = (z8 & (1<<i)) != 0
  inline def bits(offset: Int, n: Int): kse.maths.packed.Bitx8 = ((z8 >>> offset) & (0xFF >> (8-n))).toByte
  
  inline def bit0To(b: Boolean): kse.maths.packed.Bitx8 = (if (b) (z8 | 0x01) else (z8 & 0xFE)).toByte
  inline def bit1To(b: Boolean): kse.maths.packed.Bitx8 = (if (b) (z8 | 0x02) else (z8 & 0xFD)).toByte
  inline def bit2To(b: Boolean): kse.maths.packed.Bitx8 = (if (b) (z8 | 0x04) else (z8 & 0xFB)).toByte
  inline def bit3To(b: Boolean): kse.maths.packed.Bitx8 = (if (b) (z8 | 0x08) else (z8 & 0xF7)).toByte
  inline def bit4To(b: Boolean): kse.maths.packed.Bitx8 = (if (b) (z8 | 0x10) else (z8 & 0xEF)).toByte
  inline def bit5To(b: Boolean): kse.maths.packed.Bitx8 = (if (b) (z8 | 0x20) else (z8 & 0xDF)).toByte
  inline def bit6To(b: Boolean): kse.maths.packed.Bitx8 = (if (b) (z8 | 0x40) else (z8 & 0xBF)).toByte
  inline def bit7To(b: Boolean): kse.maths.packed.Bitx8 = (if (b) (z8 | 0x80) else (z8 & 0x7F)).toByte
  inline def bitTo(i: Int)(b: Boolean): kse.maths.packed.Bitx8 = (if (b) z8 | (1<<i) else z8 & (0xFF - (1<<i))).toByte
  def bitsTo(offset: Int, n: Int)(value: Byte): kse.maths.packed.Bitx8 = {
    val m = (0xFF >>> (8-n)) << offset
    ((z8 & (0xFF - m)) | ((value << offset) & m)).toByte
  }
}
extension (z8: kse.maths.packed.Bitx8) {
  @targetName("Bitx8_pr") def pr: String =
    val sb = new java.lang.StringBuilder(10)
    sb append '0'
    sb append 'b'
    val i = z8.B & 0xFF
    var b = 0x80
    while (b != 0) {
      sb.append(if ((i & b) == 0) '0' else '1')
      b = b >>> 1
    }
    sb.toString
}

extension (b: Byte) {
  def asBits: kse.maths.packed.Bitx8 = b
  def Z8: kse.maths.packed.Bitx8     = b
}



opaque type Bytex2 = Short
object Bytex2 {
  inline def apply(s: Short): kse.maths.packed.Bytex2 = s
  inline def of(b0: Byte, b1: Byte): kse.maths.packed.Bytex2 = 
    ((b0 & 0xFF) | ((b1 & 0xFF) << 8)).toShort
}
extension (b2: Bytex2) {
  inline def b0: Byte = (b2 & 0xFF).toByte
  inline def b1: Byte = ((b2 & 0xFF00) >>> 8).toByte
  inline def b0To(b: Byte): kse.maths.packed.Bytex2 = ((b2 & 0xFF00) | (b & 0xFF)).toShort
  inline def b1To(b: Byte): kse.maths.packed.Bytex2 = (((b & 0xFF) << 8) | (b2 & 0xFF)).toShort
  inline def swapB: kse.maths.packed.Bytex2 = (((b2 & 0xFF00) >>> 8) | ((b2 & 0xFF) << 8)).toShort
}
extension (b2: kse.maths.packed.Bytex2) {
  @targetName("Bytex2_pr") def pr: String =
    val b0 = b2.b0
    val b1 = b2.b1
    val n0 = b0 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n1 = b1 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val sb = new java.lang.StringBuilder(3+n0+n1)
    sb append '<'
    sb append b0
    sb append ' '
    sb append b1
    sb append '>'
    sb.toString
}

opaque type Bitx16 = Short
object Bitx16 {
  inline def apply(s: Short): kse.maths.packed.Bitx16 = s
  final def of(
    z0:  Boolean, z1:  Boolean, z2:  Boolean, z3:  Boolean, z4:  Boolean, z5:  Boolean, z6:  Boolean, z7:  Boolean,
    z8:  Boolean, z9:  Boolean, z10: Boolean, z11: Boolean, z12: Boolean, z13: Boolean, z14: Boolean, z15: Boolean
  ): kse.maths.packed.Bitx16 = (
    (if (z0 )    0x1 else 0) | (if (z1 )    0x2L else 0) |
    (if (z2 )    0x4 else 0) | (if (z3 )    0x8L else 0) |
    (if (z4 )   0x10 else 0) | (if (z5 )   0x20L else 0) |
    (if (z6 )   0x40 else 0) | (if (z7 )   0x80L else 0) |
    (if (z8 )  0x100 else 0) | (if (z9 )  0x200L else 0) |
    (if (z10)  0x400 else 0) | (if (z11)  0x800L else 0) |
    (if (z12) 0x1000 else 0) | (if (z13) 0x2000L else 0) |
    (if (z14) 0x4000 else 0) | (if (z15) 0x8000L else 0)
  ).toShort
}
extension (z16: Bitx16) {
  inline def bit0  = (z16 & 0x0001) != 0
  inline def bit1  = (z16 & 0x0002) != 0
  inline def bit2  = (z16 & 0x0004) != 0
  inline def bit3  = (z16 & 0x0008) != 0
  inline def bit4  = (z16 & 0x0010) != 0
  inline def bit5  = (z16 & 0x0020) != 0
  inline def bit6  = (z16 & 0x0040) != 0
  inline def bit7  = (z16 & 0x0080) != 0
  inline def bit8  = (z16 & 0x0100) != 0
  inline def bit9  = (z16 & 0x0200) != 0
  inline def bit10 = (z16 & 0x0400) != 0
  inline def bit11 = (z16 & 0x0800) != 0
  inline def bit12 = (z16 & 0x1000) != 0
  inline def bit13 = (z16 & 0x2000) != 0
  inline def bit14 = (z16 & 0x4000) != 0
  inline def bit15 = (z16 & 0x8000) != 0
  inline def bit(i: Int) = (z16 & (1<<i)) != 0
  inline def bits(offset: Int, n: Int): Bitx16 = ((z16 >>> offset) & (0xFFFF >>> (16-n))).toShort

  inline def bit0To(b: Boolean): kse.maths.packed.Bitx16  = (if (b) (z16 | 0x0001) else (z16 & 0xFFFE)).toShort
  inline def bit1To(b: Boolean): kse.maths.packed.Bitx16  = (if (b) (z16 | 0x0002) else (z16 & 0xFFFD)).toShort
  inline def bit2To(b: Boolean): kse.maths.packed.Bitx16  = (if (b) (z16 | 0x0004) else (z16 & 0xFFFB)).toShort
  inline def bit3To(b: Boolean): kse.maths.packed.Bitx16  = (if (b) (z16 | 0x0008) else (z16 & 0xFFF7)).toShort
  inline def bit4To(b: Boolean): kse.maths.packed.Bitx16  = (if (b) (z16 | 0x0010) else (z16 & 0xFFEF)).toShort
  inline def bit5To(b: Boolean): kse.maths.packed.Bitx16  = (if (b) (z16 | 0x0020) else (z16 & 0xFFDF)).toShort
  inline def bit6To(b: Boolean): kse.maths.packed.Bitx16  = (if (b) (z16 | 0x0040) else (z16 & 0xFFBF)).toShort
  inline def bit7To(b: Boolean): kse.maths.packed.Bitx16  = (if (b) (z16 | 0x0080) else (z16 & 0xFF7F)).toShort
  inline def bit8To(b: Boolean): kse.maths.packed.Bitx16  = (if (b) (z16 | 0x0100) else (z16 & 0xFEFF)).toShort
  inline def bit9To(b: Boolean): kse.maths.packed.Bitx16  = (if (b) (z16 | 0x0200) else (z16 & 0xFDFF)).toShort
  inline def bit10To(b: Boolean): kse.maths.packed.Bitx16 = (if (b) (z16 | 0x0400) else (z16 & 0xFBFF)).toShort
  inline def bit11To(b: Boolean): kse.maths.packed.Bitx16 = (if (b) (z16 | 0x0800) else (z16 & 0xF7FF)).toShort
  inline def bit12To(b: Boolean): kse.maths.packed.Bitx16 = (if (b) (z16 | 0x1000) else (z16 & 0xEFFF)).toShort
  inline def bit13To(b: Boolean): kse.maths.packed.Bitx16 = (if (b) (z16 | 0x2000) else (z16 & 0xDFFF)).toShort
  inline def bit14To(b: Boolean): kse.maths.packed.Bitx16 = (if (b) (z16 | 0x4000) else (z16 & 0xBFFF)).toShort
  inline def bit15To(b: Boolean): kse.maths.packed.Bitx16 = (if (b) (z16 | 0x8000) else (z16 & 0x7FFF)).toShort
  inline def bitTo(i: Int)(b: Boolean): kse.maths.packed.Bitx16 = (if (b) z16 | (1<<i) else z16 & (0xFFFF - (1<<i))).toShort
  def bitsTo(i: Int, n: Int)(value: Short): kse.maths.packed.Bitx16 = {
    val m = (0xFFFF >>> (16-n)) << i
    ((z16 & (0xFFFF - m)) | ((value << i) & m)).toShort
  }
}
extension (z16: kse.maths.packed.Bitx16) {
  @targetName("Bitx16_pr") def pr: String =
    val sb = new java.lang.StringBuilder(18)
    sb append '0'
    sb append 'b'
    val i = z16.S & 0xFFFF
    var b = 0x8000
    while (b != 0) {
      sb.append(if ((i & b) == 0) '0' else '1')
      b = b >>> 1
    }
    sb.toString
}

extension(s: Bitx16 | Bytex2) {
  inline def S: Short = s
}

extension(s: Short | Bitx16 | Bytex2) {
  inline def C: Char = s.toChar
}

extension (s: Short | Bytex2) {
  inline def asBits: kse.maths.packed.Bitx16  = s
  inline def Z16: kse.maths.packed.Bitx16     = s
}

extension (s: Short | Bitx16) {
  inline def asBytes: kse.maths.packed.Bytex2 = s
  inline def B2: kse.maths.packed.Bytex2      = s
}

extension (c: Char) {
  inline def asBits: kse.maths.packed.Bitx16  = c.toShort
  inline def Z16: kse.maths.packed.Bitx16     = c.toShort
  inline def asBytes: kse.maths.packed.Bytex2 = c.toShort
  inline def B2: kse.maths.packed.Bytex2      = c.toShort
  inline def S: Short        = c.toShort
}



opaque type Shortx2 = Int
object Shortx2 {
  inline def apply(i: Int): kse.maths.packed.Shortx2 = i
  inline def of(s0: Short, s1: Short): kse.maths.packed.Shortx2 =
    (s0 & 0xFFFF) | (s1.toInt << 16)
}
extension (s2: Shortx2) {
  inline def s0: Short = (s2 & 0xFFFF).toShort
  inline def s1: Short = (s2 >>> 16).toShort
  inline def s0To(s: Short): kse.maths.packed.Shortx2 = (s2 & 0xFFFF0000) |  (s & 0xFFFF)
  inline def s1To(s: Short): kse.maths.packed.Shortx2 = (s2 & 0xFFFF)     | ((s & 0xFFFF) << 16)
  inline def swapS: kse.maths.packed.Shortx2 = (s2 >>> 16) | (s2 << 16)
}
extension (s2: kse.maths.packed.Shortx2) {
  @targetName("Shortx2_pr") def pr: String =
    val sb = new java.lang.StringBuilder
    sb append '<'
    sb append s2.s0
    sb append ' '
    sb append s2.s1
    sb append '>'
    sb.toString
}

opaque type Charx2 = Int
object Charx2 {
  def apply(i: Int): kse.maths.packed.Charx2 = i
  def of(c0: Char, c1: Char): kse.maths.packed.Charx2 =
    c0 | (c1.toInt << 16)
}
extension (c2: Charx2) {
  inline def c0: Char = c2.toChar
  inline def c1: Char = (c2 >>> 16).toChar
  inline def c0To(c: Char): kse.maths.packed.Charx2 = (c2 & 0xFFFF0000) | c
  inline def c1To(c: Char): kse.maths.packed.Charx2 = (c2 & 0xFFFF) | (c << 16)
  inline def swapC: kse.maths.packed.Charx2 = (c2 >>> 16) | (c2 << 16)
}
extension (c2: kse.maths.packed.Charx2) {
  @targetName("Charx2_pr") def pr: String =
    val sb = new java.lang.StringBuilder(4)
    sb append '\''
    sb append c2.c0
    sb append c2.c1
    sb append '\''
    sb.toString
}

opaque type Bytex4 = Int
object Bytex4 {
  inline def apply(i: Int): kse.maths.packed.Bytex4 = i
  final def of(b0: Byte, b1: Byte, b2: Byte, b3: Byte): kse.maths.packed.Bytex4 =
    (b0 & 0xFF) | ((b1 & 0xFF) << 8) | ((b2 & 0xFF) << 16) | (b3.toInt << 24)
}
extension (b4: Bytex4) {
  inline def b0: Byte =  (b4 & 0xFF)            .toByte
  inline def b1: Byte = ((b4 & 0xFF00)   >>   8).toByte
  inline def b2: Byte = ((b4 & 0xFF0000) >>  16).toByte
  inline def b3: Byte =  (b4             >>> 24).toByte
  inline def b0To(b: Byte): kse.maths.packed.Bytex4 = (b4 & 0xFFFFFF00) |  (b & 0xFF)
  inline def b1To(b: Byte): kse.maths.packed.Bytex4 = (b4 & 0xFFFF00FF) | ((b & 0xFF) << 8)
  inline def b2To(b: Byte): kse.maths.packed.Bytex4 = (b4 & 0xFF00FFFF) | ((b & 0xFF) << 16)
  inline def b3To(b: Byte): kse.maths.packed.Bytex4 = (b4 & 0x00FFFFFF) | ((b & 0xFF) << 24)
  inline def rotrB: kse.maths.packed.Bytex4 = (b4 >>> 8) | (b4 << 24)
  inline def rotlB: kse.maths.packed.Bytex4 = (b4 >>> 24) | (b4 << 8)
  inline def swapB: kse.maths.packed.Bytex4 = ((b4 & 0xFF00FF00) >>> 8) | ((b4 & 0x00FF00FF) << 8)
  final def reverseB: kse.maths.packed.Bytex4 = ((b4 & 0xFF000000) >>> 24) | ((b4 & 0xFF0000) >> 8) | ((b4 & 0xFF00) << 8) | ((b4 & 0xFF) << 24)
}
extension (b4: kse.maths.packed.Bytex4) {
  @targetName("Bytex4_pr") def pr: String =
    val b0 = b4.b0
    val b1 = b4.b1
    val b2 = b4.b2
    val b3 = b4.b3
    val n0 = b0 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n1 = b1 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n2 = b2 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n3 = b3 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val sb = new java.lang.StringBuilder(5+n0+n1+n2+n3)
    sb append '<'
    sb append b0
    sb append ' '
    sb append b1
    sb append ' '
    sb append b2
    sb append ' '
    sb append b3
    sb append '>'
    sb.toString
}

opaque type Bitx32 = Int
object Bitx32 {
  inline def apply(i: Int): kse.maths.packed.Bitx32 = i
  final def of(
    z0:  Boolean, z1:  Boolean, z2:  Boolean, z3:  Boolean, z4:  Boolean, z5:  Boolean, z6:  Boolean, z7:  Boolean,
    z8:  Boolean, z9:  Boolean, z10: Boolean, z11: Boolean, z12: Boolean, z13: Boolean, z14: Boolean, z15: Boolean,
    z16: Boolean, z17: Boolean, z18: Boolean, z19: Boolean, z20: Boolean, z21: Boolean, z22: Boolean, z23: Boolean,
    z24: Boolean, z25: Boolean, z26: Boolean, z27: Boolean, z28: Boolean, z29: Boolean, z30: Boolean, z31: Boolean
  ): kse.maths.packed.Bitx32 = (
    (if (z0 )        0x1 else 0) | (if (z1 )        0x2 else 0) |
    (if (z2 )        0x4 else 0) | (if (z3 )        0x8 else 0) |
    (if (z4 )       0x10 else 0) | (if (z5 )       0x20 else 0) |
    (if (z6 )       0x40 else 0) | (if (z7 )       0x80 else 0) |
    (if (z8 )      0x100 else 0) | (if (z9 )      0x200 else 0) |
    (if (z10)      0x400 else 0) | (if (z11)      0x800 else 0) |
    (if (z12)     0x1000 else 0) | (if (z13)     0x2000 else 0) |
    (if (z14)     0x4000 else 0) | (if (z15)     0x8000 else 0) |
    (if (z16)    0x10000 else 0) | (if (z17)    0x20000 else 0) |
    (if (z18)    0x40000 else 0) | (if (z19)    0x80000 else 0) |
    (if (z20)   0x100000 else 0) | (if (z21)   0x200000 else 0) |
    (if (z22)   0x400000 else 0) | (if (z23)   0x800000 else 0) |
    (if (z24)  0x1000000 else 0) | (if (z25)  0x2000000 else 0) |
    (if (z26)  0x4000000 else 0) | (if (z27)  0x8000000 else 0) |
    (if (z28) 0x10000000 else 0) | (if (z29) 0x20000000 else 0) |
    (if (z30) 0x40000000 else 0) | (if (z31) 0x80000000 else 0)
  )
}
extension (z32: Bitx32) {
  inline def F = java.lang.Float.intBitsToFloat(z32)

  inline def bit0  = (z32 & 0x00000001) != 0
  inline def bit1  = (z32 & 0x00000002) != 0
  inline def bit2  = (z32 & 0x00000004) != 0
  inline def bit3  = (z32 & 0x00000008) != 0
  inline def bit4  = (z32 & 0x00000010) != 0
  inline def bit5  = (z32 & 0x00000020) != 0
  inline def bit6  = (z32 & 0x00000040) != 0
  inline def bit7  = (z32 & 0x00000080) != 0
  inline def bit8  = (z32 & 0x00000100) != 0
  inline def bit9  = (z32 & 0x00000200) != 0
  inline def bit10 = (z32 & 0x00000400) != 0
  inline def bit11 = (z32 & 0x00000800) != 0
  inline def bit12 = (z32 & 0x00001000) != 0
  inline def bit13 = (z32 & 0x00002000) != 0
  inline def bit14 = (z32 & 0x00004000) != 0
  inline def bit15 = (z32 & 0x00008000) != 0
  inline def bit16 = (z32 & 0x00010000) != 0
  inline def bit17 = (z32 & 0x00020000) != 0
  inline def bit18 = (z32 & 0x00040000) != 0
  inline def bit19 = (z32 & 0x00080000) != 0
  inline def bit20 = (z32 & 0x00100000) != 0
  inline def bit21 = (z32 & 0x00200000) != 0
  inline def bit22 = (z32 & 0x00400000) != 0
  inline def bit23 = (z32 & 0x00800000) != 0
  inline def bit24 = (z32 & 0x01000000) != 0
  inline def bit25 = (z32 & 0x02000000) != 0
  inline def bit26 = (z32 & 0x04000000) != 0
  inline def bit27 = (z32 & 0x08000000) != 0
  inline def bit28 = (z32 & 0x10000000) != 0
  inline def bit29 = (z32 & 0x20000000) != 0
  inline def bit30 = (z32 & 0x40000000) != 0
  inline def bit31 = (z32 & 0x80000000) != 0
  inline def bit(i: Int) = (z32 & (1<<i)) != 0
  inline def bits(offset: Int, n: Int) = (z32 >>> offset) & (-1 >>> (32-n))

  inline def bit0To( b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000001) else (z32 & 0xFFFFFFFE)
  inline def bit1To( b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000002) else (z32 & 0xFFFFFFFD)
  inline def bit2To( b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000004) else (z32 & 0xFFFFFFFB)
  inline def bit3To( b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000008) else (z32 & 0xFFFFFFF7)
  inline def bit4To( b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000010) else (z32 & 0xFFFFFFEF)
  inline def bit5To( b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000020) else (z32 & 0xFFFFFFDF)
  inline def bit6To( b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000040) else (z32 & 0xFFFFFFBF)
  inline def bit7To( b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000080) else (z32 & 0xFFFFFF7F)
  inline def bit8To( b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000100) else (z32 & 0xFFFFFEFF)
  inline def bit9To( b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000200) else (z32 & 0xFFFFFDFF)
  inline def bit10To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000400) else (z32 & 0xFFFFFBFF)
  inline def bit11To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00000800) else (z32 & 0xFFFFF7FF)
  inline def bit12To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00001000) else (z32 & 0xFFFFEFFF)
  inline def bit13To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00002000) else (z32 & 0xFFFFDFFF)
  inline def bit14To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00004000) else (z32 & 0xFFFFBFFF)
  inline def bit15To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00008000) else (z32 & 0xFFFF7FFF)
  inline def bit16To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00010000) else (z32 & 0xFFFEFFFF)
  inline def bit17To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00020000) else (z32 & 0xFFFDFFFF)
  inline def bit18To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00040000) else (z32 & 0xFFFBFFFF)
  inline def bit19To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00080000) else (z32 & 0xFFF7FFFF)
  inline def bit20To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00100000) else (z32 & 0xFFEFFFFF)
  inline def bit21To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00200000) else (z32 & 0xFFDFFFFF)
  inline def bit22To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00400000) else (z32 & 0xFFBFFFFF)
  inline def bit23To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x00800000) else (z32 & 0xFF7FFFFF)
  inline def bit24To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x01000000) else (z32 & 0xFEFFFFFF)
  inline def bit25To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x02000000) else (z32 & 0xFDFFFFFF)
  inline def bit26To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x04000000) else (z32 & 0xFBFFFFFF)
  inline def bit27To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x08000000) else (z32 & 0xF7FFFFFF)
  inline def bit28To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x10000000) else (z32 & 0xEFFFFFFF)
  inline def bit29To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x20000000) else (z32 & 0xDFFFFFFF)
  inline def bit30To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x40000000) else (z32 & 0xBFFFFFFF)
  inline def bit31To(b: Boolean): kse.maths.packed.Bitx32 = if (b) (z32 | 0x80000000) else (z32 & 0x7FFFFFFF)
  inline def bitTo(i: Int)(b: Boolean): kse.maths.packed.Bitx32 = if (b) z32 | (1<<i) else z32 & (0xFFFFFFFF - (1<<i))
  final def bitsTo(offset: Int, n: Int)(value: Int): kse.maths.packed.Bitx32 = {
    val m = (-1 >>> (32-n)) << offset
    (z32 & (-1 - m)) | ((value << offset) & m)
  }
}
extension (z32: kse.maths.packed.Bitx32) {
  @targetName("Bitx32_pr") def pr: String =
    val sb = new java.lang.StringBuilder(34)
    sb append '0'
    sb append 'b'
    val i = z32.I
    var b = 0x80000000
    while (b != 0) {
      sb.append(if ((i & b) == 0) '0' else '1')
      b = b >>> 1
    }
    sb.toString
}

extension (i: Bitx32 | Bytex4 | Shortx2 | Charx2) {
  inline def I: Int = i
}

extension (i: Int | Bytex4 | Shortx2 | Charx2) {
  inline def asBits: kse.maths.packed.Bitx32 = i
  inline def Z32: kse.maths.packed.Bitx32    = i
}

extension (i: Int | Bitx32 | Shortx2 | Charx2) {
  inline def asBytes: kse.maths.packed.Bytex4 = i
  inline def B4: kse.maths.packed.Bytex4      = i
}

extension (i: Int | Bitx32 | Bytex4 | Charx2) {
  inline def S2: kse.maths.packed.Shortx2 = i
}

extension (i: Int | Bitx32 | Bytex4 | Shortx2) {
  inline def C2: kse.maths.packed.Charx2 = i
}

extension (i: Int) {
  inline def asShorts: kse.maths.packed.Shortx2 = i
  inline def asChars: kse.maths.packed.Charx2   = i
}

extension (f: Float) {
  inline def asBits: kse.maths.packed.Bitx32 = java.lang.Float.floatToRawIntBits(f)
}



opaque type Intx2 = Long
object Intx2 {
  inline def apply(l: Long): kse.maths.packed.Intx2 = l
  inline def of(i0: Int, i1: Int): kse.maths.packed.Intx2 = (i0 & 0xFFFFFFFFL) | ((i1 & 0xFFFFFFFFL) << 32)
}
extension (i2: Intx2) {
  inline def i0: Int = (i2 & 0xFFFFFFFFL).toInt
  inline def i1: Int = (i2 >>> 32).toInt
  inline def i0To(i: Int): kse.maths.packed.Intx2 = (i2 & 0xFFFFFFFF00000000L) | (i & 0xFFFFFFFFL)
  inline def i1To(i: Int): kse.maths.packed.Intx2 = (i2 & 0xFFFFFFFFL) | (i.toLong << 32)
  inline def swapI: kse.maths.packed.Intx2 = (i2 >>> 32)  | (i2 << 32)
}
extension (i2: kse.maths.packed.Intx2) {
  @targetName("Shortx2_pr") def pr: String =
    val sb = new java.lang.StringBuilder
    sb append '<'
    sb append i2.i0
    sb append ' '
    sb append i2.i1
    sb append '>'
    sb.toString
}

opaque type Floatx2 = Long
object Floatx2 {
  inline def apply(l: Long): kse.maths.packed.Floatx2 = l
  inline def of(f0: Float, f1: Float): kse.maths.packed.Floatx2 = 
    (java.lang.Float.floatToRawIntBits(f0) & 0xFFFFFFFFL) | ((java.lang.Float.floatToRawIntBits(f1).toLong << 32) & 0xFFFFFFFF00000000L)
}
extension (f2: Floatx2) {
  inline def f0: Float = java.lang.Float.intBitsToFloat((f2 & 0xFFFFFFFFL).toInt)
  inline def f1: Float = java.lang.Float.intBitsToFloat((f2 >>> 32).toInt)
  inline def f0To(f: Float): kse.maths.packed.Intx2 = (f2 & 0xFFFFFFFF00000000L) | (java.lang.Float.floatToRawIntBits(f) & 0xFFFFFFFFL)
  inline def f1To(f: Float): kse.maths.packed.Intx2 = (f2 & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(f).toLong << 32)
  inline def swapF: kse.maths.packed.Intx2 = (f2 >>> 32) | (f2 << 32)
}
extension (f2: kse.maths.packed.Floatx2) {
  @targetName("Floatx2_pr") def pr: String =
    val sb = new java.lang.StringBuilder
    sb append '<'
    sb append f2.f0
    sb append ' '
    sb append f2.f1
    sb append '>'
    sb.toString

  @targetName("Floatx2_prf") def prf(fmt: String): String =
    val sb = new java.lang.StringBuilder
    sb append '<'
    sb append fmt.format(f2.f0)
    sb append ' '
    sb append fmt.format(f2.f1)
    sb append '>'
    sb.toString
}

opaque type Shortx4 = Long
object Shortx4 {
  inline def apply(l: Long): kse.maths.packed.Shortx4 = l
  final def of(s0: Short, s1: Short, s2: Short, s3: Short): kse.maths.packed.Shortx4 =
    (s0 & 0xFFFFL) | ((s1 & 0xFFFFL) << 16) | ((s2 & 0xFFFFL) << 32) | ((s3 & 0xFFFFL) << 48)
}
extension (s4: Shortx4) {
  inline def s0: Short =  (s4 & 0xFFFF)                 .toShort
  inline def s1: Short = ((s4 & 0xFFFF0000L)     >>  16).toShort
  inline def s2: Short = ((s4 & 0xFFFF00000000L) >>  32).toShort
  inline def s3: Short = ( s4                    >>> 48).toShort
  inline def s0To(s: Short): kse.maths.packed.Shortx4 = (s4 & 0xFFFFFFFFFFFF0000L) |  (s & 0xFFFF)
  inline def s1To(s: Short): kse.maths.packed.Shortx4 = (s4 & 0xFFFFFFFF0000FFFFL) | ((s & 0xFFFF).toLong << 16)
  inline def s2To(s: Short): kse.maths.packed.Shortx4 = (s4 & 0xFFFF0000FFFFFFFFL) | ((s & 0xFFFF).toLong << 32)
  inline def s3To(s: Short): kse.maths.packed.Shortx4 = (s4 & 0x0000FFFFFFFFFFFFL) | ((s & 0xFFFF).toLong << 48)
  inline def rotrS: kse.maths.packed.Shortx4 = (s4 >>> 16) | (s4 << 48)
  inline def rotlS: kse.maths.packed.Shortx4 = (s4 >>> 48) | (s4 << 16)
  inline def swapS: kse.maths.packed.Shortx4 = ((s4 & 0xFFFF0000FFFF0000L) >>> 16) | ((s4 & 0x0000FFFF0000FFFFL) << 16)
  final def reverseS: kse.maths.packed.Shortx4 = (s4 >>> 48) | ((s4 & 0xFFFF00000000L) >> 16) | ((s4 & 0xFFFF0000L) << 16) | (s4 << 48)
}
extension(s4: kse.maths.packed.Shortx4) {
  @targetName("Shortx4_pr") def pr: String =
    val sb = new java.lang.StringBuilder
    sb append '<'
    sb append s4.s0
    sb append ' '
    sb append s4.s1
    sb append ' '
    sb append s4.s2
    sb append ' '
    sb append s4.s3
    sb append '>'
    sb.toString
}

opaque type Charx4 = Long
object Charx4 {
  inline def apply(l: Long): kse.maths.packed.Charx4 = l
  final def of(c0: Char, c1: Char, c2: Char, c3: Char): kse.maths.packed.Charx4 =
    c0.toLong | (c1.toLong << 16) | (c2.toLong << 32) | (c3.toLong << 48)
}
extension (c4: Charx4) {
  inline def c0: Char =  (c4 & 0xFFFF)                 .toChar
  inline def c1: Char = ((c4 & 0xFFFF0000L)     >>  16).toChar
  inline def c2: Char = ((c4 & 0xFFFF00000000L) >>  32).toChar
  inline def c3: Char = ( c4                    >>> 48).toChar
  inline def c0To(c: Char): kse.maths.packed.Charx4 = (c4 & 0xFFFFFFFFFFFF0000L) | c
  inline def c1To(c: Char): kse.maths.packed.Charx4 = (c4 & 0xFFFFFFFF0000FFFFL) | (c.toLong << 16)
  inline def c2To(c: Char): kse.maths.packed.Charx4 = (c4 & 0xFFFF0000FFFFFFFFL) | (c.toLong << 32)
  inline def c3To(c: Char): kse.maths.packed.Charx4 = (c4 & 0x0000FFFFFFFFFFFFL) | (c.toLong << 48)
  inline def rotrC: kse.maths.packed.Charx4 = (c4 >>> 16) | (c4 << 48)
  inline def rotlC: kse.maths.packed.Charx4 = (c4 >>> 48) | (c4 << 16)
  inline def swapC: kse.maths.packed.Charx4 = ((c4 & 0xFFFF0000FFFF0000L) >>> 16) | ((c4 & 0x0000FFFF0000FFFFL) << 16)
  inline def reverseC: kse.maths.packed.Charx4 = (c4 >>> 48) | ((c4 & 0xFFFF00000000L) >> 16) | ((c4 & 0xFFFF0000L) << 16) | (c4 << 48)
}
extension (c4: kse.maths.packed.Charx4) {
  @targetName("Charx4_pr") def pr: String =
    val sb = new java.lang.StringBuilder(6)
    sb append '\''
    sb append c4.c0
    sb append c4.c1
    sb append c4.c2
    sb append c4.c3
    sb append '\''
    sb.toString
}

opaque type Bytex8 = Long
object Bytex8 {
  inline def apply(l: Long): kse.maths.packed.Bytex8 = l
  final def of(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): kse.maths.packed.Bytex8 =
    (((b0 & 0xFF) | ((b1 & 0xFF) << 8) | ((b2 & 0xFF) << 16) | ((b3 & 0xFF) << 24)) & 0xFFFFFFFFL) |
    (((b4 & 0xFF) | ((b5 & 0xFF) << 8) | ((b6 & 0xFF) << 16) | ((b7 & 0xFF) << 24)).toLong << 32)
}
extension (b8: Bytex8) {
  inline def b0 =  (b8 & 0xFF)                     .toByte
  inline def b1 = ((b8 & 0xFF00)            >>   8).toByte
  inline def b2 = ((b8 & 0xFF0000)          >>  16).toByte
  inline def b3 = ((b8 & 0xFF000000L)       >>  24).toByte
  inline def b4 = ((b8 & 0xFF00000000L)     >>  32).toByte
  inline def b5 = ((b8 & 0xFF0000000000L)   >>  40).toByte
  inline def b6 = ((b8 & 0xFF000000000000L) >>  48).toByte
  inline def b7 = ( b8                      >>> 56).toByte
  inline def b0To(b: Byte): kse.maths.packed.Bytex8 = (b8 & 0xFFFFFFFFFFFFFF00L) |  (b & 0xFF)
  inline def b1To(b: Byte): kse.maths.packed.Bytex8 = (b8 & 0xFFFFFFFFFFFF00FFL) | ((b & 0xFF) << 8)
  inline def b2To(b: Byte): kse.maths.packed.Bytex8 = (b8 & 0xFFFFFFFFFF00FFFFL) | ((b & 0xFF) << 16)
  inline def b3To(b: Byte): kse.maths.packed.Bytex8 = (b8 & 0xFFFFFFFF00FFFFFFL) | ((b & 0xFF).toLong << 24)
  inline def b4To(b: Byte): kse.maths.packed.Bytex8 = (b8 & 0xFFFFFF00FFFFFFFFL) | ((b & 0xFF).toLong << 32)
  inline def b5To(b: Byte): kse.maths.packed.Bytex8 = (b8 & 0xFFFF00FFFFFFFFFFL) | ((b & 0xFF).toLong << 40)
  inline def b6To(b: Byte): kse.maths.packed.Bytex8 = (b8 & 0xFF00FFFFFFFFFFFFL) | ((b & 0xFF).toLong << 48)
  inline def b7To(b: Byte): kse.maths.packed.Bytex8 = (b8 & 0x00FFFFFFFFFFFFFFL) | ((b & 0xFF).toLong << 56)
  inline def rotrB: kse.maths.packed.Bytex8 = (b8 >>> 8) | (b8 << 56)
  inline def rotlB: kse.maths.packed.Bytex8 = (b8 >>> 56) | (b8 << 8)
  inline def swapB: kse.maths.packed.Bytex8 = ((b8 & 0xFF00FF00FF00FF00L) >>> 8) | ((b8 & 0x00FF00FF00FF00FFL) << 8)
  final def reverseB: kse.maths.packed.Bytex8 = {
    var m = ((b8 & 0xFF00FF00FF00FF00L) >>> 8) | ((b8 & 0x00FF00FF00FF00FFL) << 8)
    (m>>>48) | ((m&0xFFFF00000000L)>>16) | ((m&0xFFFF0000L) << 16) | (m << 48)
  }
}
extension (b8: kse.maths.packed.Bytex8) {
  @targetName("Bytex8_pr") def pr: String =
    val b0 = b8.b0
    val b1 = b8.b1
    val b2 = b8.b2
    val b3 = b8.b3
    val b4 = b8.b4
    val b5 = b8.b5
    val b6 = b8.b6
    val b7 = b8.b7
    val n0 = b0 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n1 = b1 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n2 = b2 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n3 = b3 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n4 = b4 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n5 = b5 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n6 = b6 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val n7 = b7 match
      case x if x < 0 => if (x > -10) 2 else if (x > -100) 3 else 4
      case x          => if (x < 10) 1 else if (x < 100) 2 else 3
    val sb = new java.lang.StringBuilder(9+n0+n1+n2+n3+n4+n5+n6+n7)
    sb append '<'
    sb append b0
    sb append ' '
    sb append b1
    sb append ' '
    sb append b2
    sb append ' '
    sb append b3
    sb append ' '
    sb append b4
    sb append ' '
    sb append b5
    sb append ' '
    sb append b6
    sb append ' '
    sb append b7
    sb append '>'
    sb.toString
}

opaque type Bitx64 = Long
object Bitx64 {
  inline def apply(l: Long): kse.maths.packed.Bitx64 = l
  final def of(
    z0:  Boolean, z1:  Boolean, z2:  Boolean, z3:  Boolean, z4:  Boolean, z5:  Boolean, z6:  Boolean, z7:  Boolean,
    z8:  Boolean, z9:  Boolean, z10: Boolean, z11: Boolean, z12: Boolean, z13: Boolean, z14: Boolean, z15: Boolean,
    z16: Boolean, z17: Boolean, z18: Boolean, z19: Boolean, z20: Boolean, z21: Boolean, z22: Boolean, z23: Boolean,
    z24: Boolean, z25: Boolean, z26: Boolean, z27: Boolean, z28: Boolean, z29: Boolean, z30: Boolean, z31: Boolean,
    z32: Boolean, z33: Boolean, z34: Boolean, z35: Boolean, z36: Boolean, z37: Boolean, z38: Boolean, z39: Boolean,
    z40: Boolean, z41: Boolean, z42: Boolean, z43: Boolean, z44: Boolean, z45: Boolean, z46: Boolean, z47: Boolean,
    z48: Boolean, z49: Boolean, z50: Boolean, z51: Boolean, z52: Boolean, z53: Boolean, z54: Boolean, z55: Boolean,
    z56: Boolean, z57: Boolean, z58: Boolean, z59: Boolean, z60: Boolean, z61: Boolean, z62: Boolean, z63: Boolean
  ): kse.maths.packed.Bitx64 = (
    (if (z0 )                0x1L else 0L) | (if (z1 )                0x2L else 0L) |
    (if (z2 )                0x4L else 0L) | (if (z3 )                0x8L else 0L) |
    (if (z4 )               0x10L else 0L) | (if (z5 )               0x20L else 0L) |
    (if (z6 )               0x40L else 0L) | (if (z7 )               0x80L else 0L) |
    (if (z8 )              0x100L else 0L) | (if (z9 )              0x200L else 0L) |
    (if (z10)              0x400L else 0L) | (if (z11)              0x800L else 0L) |
    (if (z12)             0x1000L else 0L) | (if (z13)             0x2000L else 0L) |
    (if (z14)             0x4000L else 0L) | (if (z15)             0x8000L else 0L) |
    (if (z16)            0x10000L else 0L) | (if (z17)            0x20000L else 0L) |
    (if (z18)            0x40000L else 0L) | (if (z19)            0x80000L else 0L) |
    (if (z20)           0x100000L else 0L) | (if (z21)           0x200000L else 0L) |
    (if (z22)           0x400000L else 0L) | (if (z23)           0x800000L else 0L) |
    (if (z24)          0x1000000L else 0L) | (if (z25)          0x2000000L else 0L) |
    (if (z26)          0x4000000L else 0L) | (if (z27)          0x8000000L else 0L) |
    (if (z28)         0x10000000L else 0L) | (if (z29)         0x20000000L else 0L) |
    (if (z30)         0x40000000L else 0L) | (if (z31)         0x80000000L else 0L) |
    (if (z32)        0x100000000L else 0L) | (if (z33)        0x200000000L else 0L) |
    (if (z34)        0x400000000L else 0L) | (if (z35)        0x800000000L else 0L) |
    (if (z36)       0x1000000000L else 0L) | (if (z37)       0x2000000000L else 0L) |
    (if (z38)       0x4000000000L else 0L) | (if (z39)       0x8000000000L else 0L) |
    (if (z40)      0x10000000000L else 0L) | (if (z41)      0x20000000000L else 0L) |
    (if (z42)      0x40000000000L else 0L) | (if (z43)      0x80000000000L else 0L) |
    (if (z44)     0x100000000000L else 0L) | (if (z45)     0x200000000000L else 0L) |
    (if (z46)     0x400000000000L else 0L) | (if (z47)     0x800000000000L else 0L) |
    (if (z48)    0x1000000000000L else 0L) | (if (z49)    0x2000000000000L else 0L) |
    (if (z50)    0x4000000000000L else 0L) | (if (z51)    0x8000000000000L else 0L) |
    (if (z52)   0x10000000000000L else 0L) | (if (z53)   0x20000000000000L else 0L) |
    (if (z54)   0x40000000000000L else 0L) | (if (z55)   0x80000000000000L else 0L) |
    (if (z56)  0x100000000000000L else 0L) | (if (z57)  0x200000000000000L else 0L) |
    (if (z58)  0x400000000000000L else 0L) | (if (z59)  0x800000000000000L else 0L) |
    (if (z60) 0x1000000000000000L else 0L) | (if (z61) 0x2000000000000000L else 0L) |
    (if (z62) 0x4000000000000000L else 0L) | (if (z63) 0x8000000000000000L else 0L)
  )
}
extension (z64: Bitx64) {
  inline def D: Double   = java.lang.Double.longBitsToDouble(z64)

  inline def bit0  = (z64 & 0x0000000000000001L) != 0
  inline def bit1  = (z64 & 0x0000000000000002L) != 0
  inline def bit2  = (z64 & 0x0000000000000004L) != 0
  inline def bit3  = (z64 & 0x0000000000000008L) != 0
  inline def bit4  = (z64 & 0x0000000000000010L) != 0
  inline def bit5  = (z64 & 0x0000000000000020L) != 0
  inline def bit6  = (z64 & 0x0000000000000040L) != 0
  inline def bit7  = (z64 & 0x0000000000000080L) != 0
  inline def bit8  = (z64 & 0x0000000000000100L) != 0
  inline def bit9  = (z64 & 0x0000000000000200L) != 0
  inline def bit10 = (z64 & 0x0000000000000400L) != 0
  inline def bit11 = (z64 & 0x0000000000000800L) != 0
  inline def bit12 = (z64 & 0x0000000000001000L) != 0
  inline def bit13 = (z64 & 0x0000000000002000L) != 0
  inline def bit14 = (z64 & 0x0000000000004000L) != 0
  inline def bit15 = (z64 & 0x0000000000008000L) != 0
  inline def bit16 = (z64 & 0x0000000000010000L) != 0
  inline def bit17 = (z64 & 0x0000000000020000L) != 0
  inline def bit18 = (z64 & 0x0000000000040000L) != 0
  inline def bit19 = (z64 & 0x0000000000080000L) != 0
  inline def bit20 = (z64 & 0x0000000000100000L) != 0
  inline def bit21 = (z64 & 0x0000000000200000L) != 0
  inline def bit22 = (z64 & 0x0000000000400000L) != 0
  inline def bit23 = (z64 & 0x0000000000800000L) != 0
  inline def bit24 = (z64 & 0x0000000001000000L) != 0
  inline def bit25 = (z64 & 0x0000000002000000L) != 0
  inline def bit26 = (z64 & 0x0000000004000000L) != 0
  inline def bit27 = (z64 & 0x0000000008000000L) != 0
  inline def bit28 = (z64 & 0x0000000010000000L) != 0
  inline def bit29 = (z64 & 0x0000000020000000L) != 0
  inline def bit30 = (z64 & 0x0000000040000000L) != 0
  inline def bit31 = (z64 & 0x0000000080000000L) != 0
  inline def bit32 = (z64 & 0x0000000100000000L) != 0
  inline def bit33 = (z64 & 0x0000000200000000L) != 0
  inline def bit34 = (z64 & 0x0000000400000000L) != 0
  inline def bit35 = (z64 & 0x0000000800000000L) != 0
  inline def bit36 = (z64 & 0x0000001000000000L) != 0
  inline def bit37 = (z64 & 0x0000002000000000L) != 0
  inline def bit38 = (z64 & 0x0000004000000000L) != 0
  inline def bit39 = (z64 & 0x0000008000000000L) != 0
  inline def bit40 = (z64 & 0x0000010000000000L) != 0
  inline def bit41 = (z64 & 0x0000020000000000L) != 0
  inline def bit42 = (z64 & 0x0000040000000000L) != 0
  inline def bit43 = (z64 & 0x0000080000000000L) != 0
  inline def bit44 = (z64 & 0x0000100000000000L) != 0
  inline def bit45 = (z64 & 0x0000200000000000L) != 0
  inline def bit46 = (z64 & 0x0000400000000000L) != 0
  inline def bit47 = (z64 & 0x0000800000000000L) != 0
  inline def bit48 = (z64 & 0x0001000000000000L) != 0
  inline def bit49 = (z64 & 0x0002000000000000L) != 0
  inline def bit50 = (z64 & 0x0004000000000000L) != 0
  inline def bit51 = (z64 & 0x0008000000000000L) != 0
  inline def bit52 = (z64 & 0x0010000000000000L) != 0
  inline def bit53 = (z64 & 0x0020000000000000L) != 0
  inline def bit54 = (z64 & 0x0040000000000000L) != 0
  inline def bit55 = (z64 & 0x0080000000000000L) != 0
  inline def bit56 = (z64 & 0x0100000000000000L) != 0
  inline def bit57 = (z64 & 0x0200000000000000L) != 0
  inline def bit58 = (z64 & 0x0400000000000000L) != 0
  inline def bit59 = (z64 & 0x0800000000000000L) != 0
  inline def bit60 = (z64 & 0x1000000000000000L) != 0
  inline def bit61 = (z64 & 0x2000000000000000L) != 0
  inline def bit62 = (z64 & 0x4000000000000000L) != 0
  inline def bit63 = (z64 & 0x8000000000000000L) != 0
  inline def bit(i: Int) = (z64 & (1L<<i)) != 0
  inline def bits(offset: Int, n: Int) = (z64 >>> offset) & (-1L >>> (64-n))

  inline def bit0To( b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000001L) else (z64 & 0xFFFFFFFFFFFFFFFEL)
  inline def bit1To( b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000002L) else (z64 & 0xFFFFFFFFFFFFFFFDL)
  inline def bit2To( b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000004L) else (z64 & 0xFFFFFFFFFFFFFFFBL)
  inline def bit3To( b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000008L) else (z64 & 0xFFFFFFFFFFFFFFF7L)
  inline def bit4To( b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000010L) else (z64 & 0xFFFFFFFFFFFFFFEFL)
  inline def bit5To( b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000020L) else (z64 & 0xFFFFFFFFFFFFFFDFL)
  inline def bit6To( b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000040L) else (z64 & 0xFFFFFFFFFFFFFFBFL)
  inline def bit7To( b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000080L) else (z64 & 0xFFFFFFFFFFFFFF7FL)
  inline def bit8To( b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000100L) else (z64 & 0xFFFFFFFFFFFFFEFFL)
  inline def bit9To( b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000200L) else (z64 & 0xFFFFFFFFFFFFFDFFL)
  inline def bit10To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000400L) else (z64 & 0xFFFFFFFFFFFFFBFFL)
  inline def bit11To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000000800L) else (z64 & 0xFFFFFFFFFFFFF7FFL)
  inline def bit12To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000001000L) else (z64 & 0xFFFFFFFFFFFFEFFFL)
  inline def bit13To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000002000L) else (z64 & 0xFFFFFFFFFFFFDFFFL)
  inline def bit14To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000004000L) else (z64 & 0xFFFFFFFFFFFFBFFFL)
  inline def bit15To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000008000L) else (z64 & 0xFFFFFFFFFFFF7FFFL)
  inline def bit16To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000010000L) else (z64 & 0xFFFFFFFFFFFEFFFFL)
  inline def bit17To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000020000L) else (z64 & 0xFFFFFFFFFFFDFFFFL)
  inline def bit18To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000040000L) else (z64 & 0xFFFFFFFFFFFBFFFFL)
  inline def bit19To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000080000L) else (z64 & 0xFFFFFFFFFFF7FFFFL)
  inline def bit20To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000100000L) else (z64 & 0xFFFFFFFFFFEFFFFFL)
  inline def bit21To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000200000L) else (z64 & 0xFFFFFFFFFFDFFFFFL)
  inline def bit22To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000400000L) else (z64 & 0xFFFFFFFFFFBFFFFFL)
  inline def bit23To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000000800000L) else (z64 & 0xFFFFFFFFFF7FFFFFL)
  inline def bit24To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000001000000L) else (z64 & 0xFFFFFFFFFEFFFFFFL)
  inline def bit25To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000002000000L) else (z64 & 0xFFFFFFFFFDFFFFFFL)
  inline def bit26To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000004000000L) else (z64 & 0xFFFFFFFFFBFFFFFFL)
  inline def bit27To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000008000000L) else (z64 & 0xFFFFFFFFF7FFFFFFL)
  inline def bit28To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000010000000L) else (z64 & 0xFFFFFFFFEFFFFFFFL)
  inline def bit29To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000020000000L) else (z64 & 0xFFFFFFFFDFFFFFFFL)
  inline def bit30To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000040000000L) else (z64 & 0xFFFFFFFFBFFFFFFFL)
  inline def bit31To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000080000000L) else (z64 & 0xFFFFFFFF7FFFFFFFL)
  inline def bit32To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000100000000L) else (z64 & 0xFFFFFFFEFFFFFFFFL)
  inline def bit33To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000200000000L) else (z64 & 0xFFFFFFFDFFFFFFFFL)
  inline def bit34To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000400000000L) else (z64 & 0xFFFFFFFBFFFFFFFFL)
  inline def bit35To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000000800000000L) else (z64 & 0xFFFFFFF7FFFFFFFFL)
  inline def bit36To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000001000000000L) else (z64 & 0xFFFFFFEFFFFFFFFFL)
  inline def bit37To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000002000000000L) else (z64 & 0xFFFFFFDFFFFFFFFFL)
  inline def bit38To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000004000000000L) else (z64 & 0xFFFFFFBFFFFFFFFFL)
  inline def bit39To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000008000000000L) else (z64 & 0xFFFFFF7FFFFFFFFFL)
  inline def bit40To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000010000000000L) else (z64 & 0xFFFFFEFFFFFFFFFFL)
  inline def bit41To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000020000000000L) else (z64 & 0xFFFFFDFFFFFFFFFFL)
  inline def bit42To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000040000000000L) else (z64 & 0xFFFFFBFFFFFFFFFFL)
  inline def bit43To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000080000000000L) else (z64 & 0xFFFFF7FFFFFFFFFFL)
  inline def bit44To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000100000000000L) else (z64 & 0xFFFFEFFFFFFFFFFFL)
  inline def bit45To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000200000000000L) else (z64 & 0xFFFFDFFFFFFFFFFFL)
  inline def bit46To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000400000000000L) else (z64 & 0xFFFFBFFFFFFFFFFFL)
  inline def bit47To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0000800000000000L) else (z64 & 0xFFFF7FFFFFFFFFFFL)
  inline def bit48To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0001000000000000L) else (z64 & 0xFFFEFFFFFFFFFFFFL)
  inline def bit49To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0002000000000000L) else (z64 & 0xFFFDFFFFFFFFFFFFL)
  inline def bit50To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0004000000000000L) else (z64 & 0xFFFBFFFFFFFFFFFFL)
  inline def bit51To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0008000000000000L) else (z64 & 0xFFF7FFFFFFFFFFFFL)
  inline def bit52To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0010000000000000L) else (z64 & 0xFFEFFFFFFFFFFFFFL)
  inline def bit53To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0020000000000000L) else (z64 & 0xFFDFFFFFFFFFFFFFL)
  inline def bit54To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0040000000000000L) else (z64 & 0xFFBFFFFFFFFFFFFFL)
  inline def bit55To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0080000000000000L) else (z64 & 0xFF7FFFFFFFFFFFFFL)
  inline def bit56To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0100000000000000L) else (z64 & 0xFEFFFFFFFFFFFFFFL)
  inline def bit57To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0200000000000000L) else (z64 & 0xFDFFFFFFFFFFFFFFL)
  inline def bit58To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0400000000000000L) else (z64 & 0xFBFFFFFFFFFFFFFFL)
  inline def bit59To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x0800000000000000L) else (z64 & 0xF7FFFFFFFFFFFFFFL)
  inline def bit60To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x1000000000000000L) else (z64 & 0xEFFFFFFFFFFFFFFFL)
  inline def bit61To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x2000000000000000L) else (z64 & 0xDFFFFFFFFFFFFFFFL)
  inline def bit62To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x4000000000000000L) else (z64 & 0xBFFFFFFFFFFFFFFFL)
  inline def bit63To(b: Boolean): kse.maths.packed.Bitx64 = if (b) (z64 | 0x8000000000000000L) else (z64 & 0x7FFFFFFFFFFFFFFFL)
  inline def bitTo(i: Int)(b: Boolean): kse.maths.packed.Bitx64 = if (b) z64 | (1L<<i) else z64 & (-1L - (1L<<i))
  final def bitsTo(offset: Int, n: Int)(value: Long): Bitx64 = {
    val m = (-1L >>> (64-n)) << offset
    (z64 & (-1L - m)) | ((value << offset) & m)
  }
}
extension (z64: kse.maths.packed.Bitx64) {
  @targetName("Bitx64_pr") def pr: String =
    val sb = new java.lang.StringBuilder(66)
    sb append '0'
    sb append 'b'
    val l = z64.L
    var b = 0x8000000000000000L
    while (b != 0) {
      sb.append(if ((l & b) == 0) '0' else '1')
      b = b >>> 1
    }
    sb.toString
}


opaque type Vc = Long
object Vc {
  inline def apply(l: Long): kse.maths.packed.Vc = l
  inline def from(f0: Float, f1: Float): kse.maths.packed.Vc = 
    (java.lang.Float.floatToRawIntBits(f0) & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(f1).toLong << 32)
  inline def from(d0: Double, d1: Double): kse.maths.packed.Vc = 
    from(d0.toFloat, d1.toFloat)

  inline def zero: kse.maths.packed.Vc = 0L
  final val NaN: kse.maths.packed.Vc = from(Float.NaN, Float.NaN)
}
extension (v: Vc) {
  inline def x: Float = java.lang.Float.intBitsToFloat((v & 0xFFFFFFFFL).toInt)
  inline def y: Float = java.lang.Float.intBitsToFloat((v >>> 32).toInt)

  inline def xTo(f: Float): kse.maths.packed.Vc =
    (v & 0xFFFFFFFF00000000L) | (java.lang.Float.floatToRawIntBits(f) & 0xFFFFFFFFL)
  inline def xFn(inline f: Float => Float): kse.maths.packed.Vc =
    (v & 0xFFFFFFFF00000000L) | (java.lang.Float.floatToRawIntBits(f(v.x)) & 0xFFFFFFFFL)

  inline def yTo(f: Float): kse.maths.packed.Vc =
    (v & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(f).toLong << 32)
  inline def yFn(inline f: Float => Float): kse.maths.packed.Vc =
    (v & 0xFFFFFFFFL) | (java.lang.Float.floatToRawIntBits(f(v.y)).toLong << 32)

  inline def isZero = (v & 0x7FFFFFFF7FFFFFFFL) == 0
  final def isFinite = { val a = v & 0x7F8000007F800000L; (a.toInt != 0x7F800000) && ((a >> 32) != 0x7F800000) }
  final def isNaN = java.lang.Float.isNaN(v.x) || java.lang.Float.isNaN(v.y)

  inline def swap: kse.maths.packed.Vc = (v >>> 32) | (v << 32)
  inline def cw: kse.maths.packed.Vc = ((v >>> 32) | (v << 32)) ^ 0x8000000000000000L
  inline def ccw: kse.maths.packed.Vc = ((v >>> 32) | (v << 32)) ^ 0x80000000L
  inline def unary_- : kse.maths.packed.Vc = v ^ 0x8000000080000000L
}
extension (v: kse.maths.packed.Vc) {
  final def rotate(angle: Float): kse.maths.packed.Vc =
    val x = v.x
    val y = v.y
    val ca = math.cos(angle)
    val sa = math.sin(angle)
    Vc.from(x*ca - y*sa, y*ca + x*sa)
  inline def theta: Double = math.atan2(v.y, v.x)

  final def lenSq: Double = { val a = v.x.toDouble; val b = v.y.toDouble; a*a + b*b }
  inline def len: Double = math.sqrt(v.lenSq)

  @targetName("Vc_plus") inline def +(f: Float): kse.maths.packed.Vc = Vc from (v.x + f, v.y + f)
  @targetName("Vc_plus") inline def +(f: Float, g: Float): kse.maths.packed.Vc = Vc from (v.x + f, v.y + g)
  @targetName("Vc_plus") final def +(u: kse.maths.packed.Vc): kse.maths.packed.Vc = Vc from (v.x + u.x, v.y + u.y)

  @targetName("Vc_minus") inline def -(f: Float): kse.maths.packed.Vc = Vc from (v.x - f, v.y - f)
  @targetName("Vc_minus") inline def -(f: Float, g: Float): kse.maths.packed.Vc = Vc from (v.x - f, v.y - g)
  @targetName("Vc_minus") final def -(u: kse.maths.packed.Vc): kse.maths.packed.Vc = Vc from (v.x - u.x, v.y - u.y)

  @targetName("Vc_times") inline def *(f: Float): kse.maths.packed.Vc = Vc from (v.x*f, v.y*f)
  @targetName("Vc_dot") inline def *(f: Float, g: Float): Double = v.x*f + v.y*g
  @targetName("Vc_dot") inline def *(u: kse.maths.packed.Vc): Double = v.x*u.x + v.y*u.y
  inline def X(f: Float, g: Float): Double = v.x*g - v.y*f
  inline def X(u: kse.maths.packed.Vc): Double = v.x*u.y - v.y*u.x

  final def proj(f: Float, g: Float): kse.maths.packed.Vc =
    val a = v.x
    val b = v.y
    val e = (a*f + b*g)/(f*f + g*g)
    Vc from (f*e, g*e)
  final def proj(u: kse.maths.packed.Vc): kse.maths.packed.Vc =
    val a = v.x
    val b = v.y
    val c = u.x
    val d = u.y
    val e = (a*c + b*d)/(c*c + d*d)
    Vc from (c*e, d*e)

  final def orth(f: Float, g: Float): kse.maths.packed.Vc =
    val a = v.x
    val b = v.y
    val e = (a*f + b*g)/(f*f + g*g)
    Vc from (a - f*e, b - g*e)
  final def orth(u: kse.maths.packed.Vc): kse.maths.packed.Vc =
    val a = v.x
    val b = v.y
    val c = u.x
    val d = u.y
    val e = (a*c + b*d)/(c*c + d*d)
    Vc from (a - c*e, b - d*e)

  final def hat: kse.maths.packed.Vc =
    val a = v.x.toDouble
    val b = v.y.toDouble
    val l2 = a*a + b*b
    if (math.abs(l2-1) < 3e-7f) v
    else if (l2 == 0) 0L
    else { 
      val il = 1.0/math.sqrt(l2)
      Vc from (a*il, b*il)
    }

  final def normDot(f: Float, g: Float): Double =
    val a = v.x
    val b = v.y
    (a*f + b*g) / math.sqrt((a*a + b*b)*(f*f + g*g)) match
      case c if c < -1 => -1
      case c if c > 1  =>  1
      case c           =>  c
  final def normDot(u: kse.maths.packed.Vc): Double =
    val a = v.x.toDouble
    val b = v.y.toDouble
    val c = u.x.toDouble
    val d = u.y.toDouble
    (a*c + b*d) / math.sqrt((a*a + b*b)*(c*c + d*d)) match
      case c if c < -1 => -1
      case c if c > 1  =>  1
      case c           =>  c

  final def distSq(f: Float, g: Float): Double =
    val a = (v.x - f).toDouble
    val b = (v.y - g).toDouble
    a*a + b*b
  final def distSq(u: kse.maths.packed.Vc): Double =
    val a = (v.x - u.x).toDouble
    val b = (v.y - u.y).toDouble
    a*a + b*b
  inline def dist(f: Float, g: Float): Float = jm.sqrt(v.distSq(f, g)).toFloat
  inline def dist(u: kse.maths.packed.Vc): Float = jm.sqrt(v.distSq(u)).toFloat

  final def angle(f: Float, g: Float): Double =
    val a = v.x.toDouble
    val b = v.y.toDouble
    val d = (a*f + b*g)/math.sqrt((a*a + b*b)*(f*f + g*g)) match
      case c if c < -1 => -1
      case c if c > 1  =>  1
      case c           =>  c
    d * math.signum(a*g - b*f)
  final def angle(u: kse.maths.packed.Vc): Double =
    val a = v.x.toDouble
    val b = v.y.toDouble
    val f = u.x.toDouble
    val g = u.y.toDouble
    val d = (a*f + b*g)/math.sqrt((a*a + b*b)*(f*f + g*g)) match
      case c if c < -1 => -1
      case c if c > 1  =>  1
      case c           =>  c
    d * math.signum(a*g - b*f)
}
extension (v: kse.maths.packed.Vc) {
  @targetName("Vc_pr") def pr: String =
    val sb = new java.lang.StringBuilder
    sb append '['
    sb append v.x
    sb append ' '
    sb append v.y
    sb append ']'
    sb.toString

  @targetName("Vc_prf") def prf(fmt: String): String =
    val sb = new java.lang.StringBuilder
    sb append '['
    sb append fmt.format(v.x)
    sb append ' '
    sb append fmt.format(v.y)
    sb append ']'
    sb.toString
}


opaque type PlusMinus = Long
object PlusMinus {
  inline def apply(l: Long): kse.maths.packed.PlusMinus = l
  inline def of(value: Float, err: Float): kse.maths.packed.PlusMinus = Floatx2.of(value, err).L
  inline def exact(value: Float): kse.maths.packed.PlusMinus = Floatx2.of(value, 0f).L
}
extension (pm: PlusMinus) {
  inline def value: Float = Floatx2(pm).f0
  inline def error: Float = Floatx2(pm).f1
}
extension (pm: kse.maths.packed.PlusMinus) {
  @targetName("PlusMinus_add") final def +(f: Float): kse.maths.packed.PlusMinus =
    PlusMinus.of(pm.value + f, pm.error)

  @targetName("PlusMinus_add") final def +(qm: kse.maths.packed.PlusMinus): kse.maths.packed.PlusMinus =
    val v = pm.value
    val u = qm.value
    val e = pm.error.toDouble
    val f = qm.error.toDouble
    PlusMinus.of(v + u, jm.sqrt((e*e + f*f).toDouble).toFloat)

  @targetName("PlusMinus_sub") final def -(f: Float): kse.maths.packed.PlusMinus =
    PlusMinus.of(pm.value - f, pm.error)

  @targetName("PlusMinus_sub") final def -(qm: kse.maths.packed.PlusMinus): kse.maths.packed.PlusMinus =
    val v = pm.value
    val u = qm.value
    val e = pm.error.toDouble
    val f = qm.error.toDouble
    PlusMinus.of(v - u, jm.sqrt(e*e + f*f).toFloat)

  @targetName("PlusMinus_mul") final def *(f: Float): kse.maths.packed.PlusMinus =
    PlusMinus.of(pm.value * f, pm.error * f)

  @targetName("PlusMinus_mul") final def *(qm: kse.maths.packed.PlusMinus): kse.maths.packed.PlusMinus =
    val v = pm.value.toDouble
    val u = qm.value.toDouble
    val e = pm.error.toDouble
    val f = qm.error.toDouble
    val a = v * u
    val xe = if (e == 0) 0.0 else e/u
    val xf = if (f == 0) 0.0 else f/u
    PlusMinus.of(a.toFloat, (a * jm.sqrt(xe*xe + xf*xf)).toFloat)

  final def /(f: Float): kse.maths.packed.PlusMinus =
    PlusMinus.of(pm.value / f, pm.error / f)

  final def /(qm: kse.maths.packed.PlusMinus): kse.maths.packed.PlusMinus =
    val v = pm.value.toDouble
    val u = qm.value.toDouble
    val e = pm.error.toDouble
    val f = qm.error.toDouble
    val a = v / u
    val xe = if (e == 0) 0.0 else e/v
    val xf = if (f == 0) 0.0 else f/u
    PlusMinus.of(a.toFloat, (a * jm.sqrt(xe*xe + xf*xf)).toFloat)

/*
  final def sq: kse.maths.packed.PlusMinus =
    val v = pm.value.toDouble
    val e = pm.error.toDouble
    if (e == 0) PlusMinus.of((v*v).toFloat, 0f)
    else        PlusMinus.of((v*v).toFloat, (e * kse.maths.NumericConstants.SqrtTwo).toFloat)
*/

/*
  final def sqrt: kse.maths.packed.PlusMinus =
    val v = pm.value.toDouble
    val e = pm.error.toDouble
    if (e == 0) PlusMinus.of(jm.sqrt(v).toFloat, 0f)
    else        PlusMinus.of(jm.sqrt(v).toFloat, (e * kse.maths.NumericConstants.OverSqrtTwo).toFloat)

  final def pow(exponent: Float): kse.maths.packed.PlusMinus =
    val v = pm.value.toDouble
    val e = pm.error.toDouble
    if (e == 0) PlusMinus.of(jm.pow(v, exponent).toFloat, 0f)
    else        PlusMinus.of(jm.pow(v, exponent).toFloat, (e * jm.sqrt(exponent)).toFloat)
*/
}
extension (pm: kse.maths.packed.PlusMinus) {
  @targetName("PlusMinus_pr") def pr: String =
    val sb = new java.lang.StringBuilder
    sb append pm.value
    sb append " +- "
    sb append pm.error
    sb.toString

  @targetName("PlusMinus_prf") def prf(fmt: String): String =
    val sb = new java.lang.StringBuilder
    sb append fmt.format(pm.value)
    sb append " +- "
    sb append fmt.format(pm.error)
    sb.toString
}

extension(l: Bitx64 | Bytex8 | Shortx4 | Charx4 | Intx2 | Floatx2 | Vc | PlusMinus) {
  inline def L: Long = l
}

extension(l: Long | Bytex8 | Shortx4 | Charx4 | Intx2 | Floatx2 | Vc | PlusMinus) {
  inline def asBits: kse.maths.packed.Bitx64 = l
}

extension(l: Long | Bytex8 | Shortx4 | Charx4 | Intx2 | Floatx2) {
  inline def Z64: kse.maths.packed.Bitx64    = l
}

extension(l: Long | Bitx64 | Shortx4 | Charx4 | Intx2 | Floatx2) {
  inline def asBytes: kse.maths.packed.Bytex8   = l
  inline def B8: kse.maths.packed.Bytex8 = l
}

extension(l: Long | Bitx64 | Charx4 | Intx2) {
  inline def S4: kse.maths.packed.Shortx4 = l
}

extension(l: Long | Bitx64 | Shortx4 | Intx2) {
  inline def C4: kse.maths.packed.Charx4 = l
}

extension(l: Long | Bitx64 | Charx4 | Shortx4) {
  inline def I2: kse.maths.packed.Intx2 = l
}

extension (l: Long | Vc /*| PlusMinus */) {
  inline def asFloats: kse.maths.packed.Floatx2 = l
}

extension (l: Bitx64 | Vc /*| PlusMinus*/) {
  inline def F2: kse.maths.packed.Floatx2 = l
}

extension (l: Long) {
  inline def asShorts: kse.maths.packed.Shortx4 = l
  inline def asChars: kse.maths.packed.Charx4   = l
  inline def asInts: kse.maths.packed.Intx2     = l
}

extension (d: Double) {
  inline def asBits: kse.maths.packed.Bitx64 = java.lang.Double.doubleToRawLongBits(d)
}

extension (f: Float) {
  inline def vc(g: Float): kse.maths.packed.Vc = Vc.from(f, g)
  inline def +-(g: Float): kse.maths.packed.PlusMinus = PlusMinus.of(f, g)
}



object Pack {
  inline def apply(
    bit0: Boolean, bit1: Boolean, bit2: Boolean, bit3: Boolean, bit4: Boolean,
    bit5: Boolean, bit6: Boolean, bit7: Boolean
  ): kse.maths.packed.Bitx8 = Bitx8.of(bit0, bit1, bit2, bit3, bit4, bit5, bit6, bit7)

  inline def apply(
    bit0:  Boolean, bit1:  Boolean, bit2:  Boolean, bit3:  Boolean, bit4:  Boolean,
    bit5:  Boolean, bit6:  Boolean, bit7:  Boolean, bit8:  Boolean, bit9:  Boolean,
    bit10: Boolean, bit11: Boolean, bit12: Boolean, bit13: Boolean, bit14: Boolean,
    bit15: Boolean
  ): kse.maths.packed.Bitx16 = Bitx16.of(
    bit0,  bit1,  bit2,  bit3,  bit4,  bit5,  bit6,  bit7,  bit8,  bit9,
    bit10, bit11, bit12, bit13, bit14, bit15
  )

  inline def apply(
    bit0:  Boolean, bit1:  Boolean, bit2:  Boolean, bit3:  Boolean, bit4:  Boolean,
    bit5:  Boolean, bit6:  Boolean, bit7:  Boolean, bit8:  Boolean, bit9:  Boolean,
    bit10: Boolean, bit11: Boolean, bit12: Boolean, bit13: Boolean, bit14: Boolean,
    bit15: Boolean, bit16: Boolean, bit17: Boolean, bit18: Boolean, bit19: Boolean,
    bit20: Boolean, bit21: Boolean, bit22: Boolean, bit23: Boolean, bit24: Boolean,
    bit25: Boolean, bit26: Boolean, bit27: Boolean, bit28: Boolean, bit29: Boolean,
    bit30: Boolean, bit31: Boolean
  ): kse.maths.packed.Bitx32 = Bitx32.of(
    bit0,  bit1,  bit2,  bit3,  bit4,  bit5,  bit6,  bit7,  bit8,  bit9,
    bit10, bit11, bit12, bit13, bit14, bit15, bit16, bit17, bit18, bit19,
    bit20, bit21, bit22, bit23, bit24, bit25, bit26, bit27, bit28, bit29,
    bit30, bit31
  )

  inline def apply(
    bit0:  Boolean, bit1:  Boolean, bit2:  Boolean, bit3:  Boolean, bit4:  Boolean,
    bit5:  Boolean, bit6:  Boolean, bit7:  Boolean, bit8:  Boolean, bit9:  Boolean,
    bit10: Boolean, bit11: Boolean, bit12: Boolean, bit13: Boolean, bit14: Boolean,
    bit15: Boolean, bit16: Boolean, bit17: Boolean, bit18: Boolean, bit19: Boolean,
    bit20: Boolean, bit21: Boolean, bit22: Boolean, bit23: Boolean, bit24: Boolean,
    bit25: Boolean, bit26: Boolean, bit27: Boolean, bit28: Boolean, bit29: Boolean,
    bit30: Boolean, bit31: Boolean, bit32: Boolean, bit33: Boolean, bit34: Boolean,
    bit35: Boolean, bit36: Boolean, bit37: Boolean, bit38: Boolean, bit39: Boolean,
    bit40: Boolean, bit41: Boolean, bit42: Boolean, bit43: Boolean, bit44: Boolean,
    bit45: Boolean, bit46: Boolean, bit47: Boolean, bit48: Boolean, bit49: Boolean,
    bit50: Boolean, bit51: Boolean, bit52: Boolean, bit53: Boolean, bit54: Boolean,
    bit55: Boolean, bit56: Boolean, bit57: Boolean, bit58: Boolean, bit59: Boolean,
    bit60: Boolean, bit61: Boolean, bit62: Boolean, bit63: Boolean
  ): kse.maths.packed.Bitx64 = Bitx64.of(
    bit0,  bit1,  bit2,  bit3,  bit4,  bit5,  bit6,  bit7,  bit8,  bit9,
    bit10, bit11, bit12, bit13, bit14, bit15, bit16, bit17, bit18, bit19,
    bit20, bit21, bit22, bit23, bit24, bit25, bit26, bit27, bit28, bit29,
    bit30, bit31, bit32, bit33, bit34, bit35, bit36, bit37, bit38, bit39,
    bit40, bit41, bit42, bit43, bit44, bit45, bit46, bit47, bit48, bit49,
    bit50, bit51, bit52, bit53, bit54, bit55, bit56, bit57, bit58, bit59,
    bit60, bit61, bit62, bit63
  )


  inline def apply(b0: Byte, b1: Byte): kse.maths.packed.Bytex2 = Bytex2.of(b0, b1)

  inline def apply(b0: Byte, b1: Byte, b2: Byte, b3: Byte): kse.maths.packed.Bytex4 = Bytex4.of(b0, b1, b2, b3)

  inline def apply(
    b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte
  ): kse.maths.packed.Bytex8 = Bytex8.of(b0, b1, b2, b3, b4, b5, b6, b7)


  inline def apply(c0: Char, c1: Char): kse.maths.packed.Charx2 = Charx2.of(c0, c1)

  inline def apply(c0: Char, c1: Char, c2: Char, c3: Char): kse.maths.packed.Charx4 = Charx4.of(c0, c1, c2, c3)


  inline def apply(s0: Short, s1: Short): kse.maths.packed.Shortx2 = Shortx2.of(s0, s1)
  
  inline def apply(s0: Short, s1: Short, s2: Short, s3: Short): kse.maths.packed.Shortx4 = Shortx4.of(s0, s1, s2, s3)


  inline def apply(i0: Int, i1: Int): kse.maths.packed.Intx2 = Intx2.of(i0, i1)


  inline def apply(f0: Float, f1: Float): kse.maths.packed.Floatx2 = Floatx2.of(f0, f1)
}


object Vcf {
  inline def apply(f0: Float, f1: Float): kse.maths.packed.Vc = Vc.from(f0, f1)
}

object Vcd {
  inline def apply(d0: Double, d1: Double): kse.maths.packed.Vc = Vc.from(d0, d1)
}
