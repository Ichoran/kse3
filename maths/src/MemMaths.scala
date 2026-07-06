// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.maths


import scala.annotation.targetName

import kse.basics.{Mem, ClippedMem}


/** Typed reads and writes of maths types stored in a [[kse.basics.Mem]].
  *
  * These mirror `Mem`'s own `getX`/`setX` accessors: the byte offset is `i * bytesOf[A]`, so
  * indices stay in units of `A` no matter which type is read or written (unaligned, native
  * byte order).  Only bijective bit-reinterpretations belong here: unsigned integers and
  * `Bf16` round-trip every bit pattern of their width.
  */
extension [A <: Mem.Type](m: Mem[A]) {
  inline def getUB(i: Long): UByte  = UByte.wrap(m.getB(i))
  inline def getUS(i: Long): UShort = UShort.wrap(m.getS(i))
  inline def getUI(i: Long): UInt   = UInt.wrap(m.getI(i))
  inline def getUL(i: Long): ULong  = ULong.wrap(m.getL(i))
  inline def getBf16(i: Long): Bf16 = Bf16.wrap(m.getC(i))

  inline def setUB(i: Long, x: UByte):  Unit = m.setB(i, x.signed)
  inline def setUS(i: Long, x: UShort): Unit = m.setS(i, x.signed)
  inline def setUI(i: Long, x: UInt):   Unit = m.setI(i, x.signed)
  inline def setUL(i: Long, x: ULong):  Unit = m.setL(i, x.signed)
  inline def setBf16(i: Long, x: Bf16): Unit = m.setC(i, x.underlying)
}

/** Clipped counterparts: reads answer `None` and writes silently do nothing if any byte
  * of the value would fall out of range.
  */
extension [A <: Mem.Type](cm: ClippedMem[A]) {
  inline def getUB(i: Long): Option[UByte] =
    val off = i * Mem.bytesOf[A]
    if off >= 0 && off + 1 <= cm.unclip.segment.byteSize then Some(UByte.wrap(cm.unclip.getB(i))) else None
  inline def getUS(i: Long): Option[UShort] =
    val off = i * Mem.bytesOf[A]
    if off >= 0 && off + 2 <= cm.unclip.segment.byteSize then Some(UShort.wrap(cm.unclip.getS(i))) else None
  inline def getUI(i: Long): Option[UInt] =
    val off = i * Mem.bytesOf[A]
    if off >= 0 && off + 4 <= cm.unclip.segment.byteSize then Some(UInt.wrap(cm.unclip.getI(i))) else None
  inline def getUL(i: Long): Option[ULong] =
    val off = i * Mem.bytesOf[A]
    if off >= 0 && off + 8 <= cm.unclip.segment.byteSize then Some(ULong.wrap(cm.unclip.getL(i))) else None
  inline def getBf16(i: Long): Option[Bf16] =
    val off = i * Mem.bytesOf[A]
    if off >= 0 && off + 2 <= cm.unclip.segment.byteSize then Some(Bf16.wrap(cm.unclip.getC(i))) else None

  @targetName("clipped_setUB")
  inline def setUB(i: Long, x: UByte):  Unit = cm.setB(i, x.signed)
  @targetName("clipped_setUS")
  inline def setUS(i: Long, x: UShort): Unit = cm.setS(i, x.signed)
  @targetName("clipped_setUI")
  inline def setUI(i: Long, x: UInt):   Unit = cm.setI(i, x.signed)
  @targetName("clipped_setUL")
  inline def setUL(i: Long, x: ULong):  Unit = cm.setL(i, x.signed)
  @targetName("clipped_setBf16")
  inline def setBf16(i: Long, x: Bf16): Unit = cm.setC(i, x.underlying)
}
