// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025 Rex Kerr and UCSF / Kato Lab

package kse.maths.colours


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.lang.{Math => jm}

import scala.annotation.targetName

import kse.basics.Translucent

import kse.maths._


object Colour {
  def u8plus_to_float(u8: Int): Float =
    (u8 & 0x7FFFFF) * 0.003921569f

  def float_to_u8plus(value: Float): Int =
    if value >= 32767.5 then 8355713 else if value > 0 then (value * 255).round else 0

  def u8_to_packed(u8: Int): Int =
    (526345 * (u8 & 0xFF)) >>> 11

  def packed_to_u(packed: Int): Int = ???

  def bits_to_float(packed: Int): Float =
    val m = packed & 0xFFFF
    val e = packed & 0xF0000
    var v = (packed & 0x100000) << 11
    if e == 0 then
      if m != 0 then
        val lz = java.lang.Integer.numberOfLeadingZeros(m)
        v = v | ((142 - lz) << 23) | ((m << (lz - 8)) & 0x7FFFFF)
    else if e == 0xF0000 && m == 0xFFFF then
      v = v | 0x7F800000
    else
      v = v | ((0x007E0000 + e + m) << 7)
    java.lang.Float.intBitsToFloat(v)

  def float_to_bits(float: Float): Int =
    var bits = java.lang.Float.floatToIntBits(float)
    val s = (bits & 0x80000000) >>> 11
    bits = bits & 0x7FFFFFFF
    if bits > 0x46FFFF7F then
      if bits > 0x7F800000 then -1  // NaN indicator
      else s | 0xFFFFF              // +- infinity
    else if bits <= 0x37000000 then s
    else if bits >= 0x3F800000 then s | ((bits - 0x3EFFFFBF) >>> 7)
    else
      val e = (bits >>> 23) - 110
      s | (((1 << e) + (((bits & 0x7FFFFF) + 0x3F) >>> (23 - e))) >>> 1)

  def pack_floats(one: Float, two: Float, three: Float): Long =
    val a = float_to_bits(one)
    if a < 0 then return -1L
    val b = float_to_bits(two)
    if b < 0 then return -1L
    val c = float_to_bits(three)
    if c < 0 then return -1L
    (a.toLong << 42) | (b.toLong << 21) | c

  inline def use_packed_floats(packed: Long)(inline fa: Float => Unit, inline fb: Float => Unit, inline fc: Float => Unit): Unit =
    if packed >= 0 then
      fa(bits_to_float(((packed >>> 42) & 0x1FFFFF).toInt))
      fb(bits_to_float(((packed >>> 21) & 0x1FFFFF).toInt))
      fc(bits_to_float(( packed         & 0x1FFFFF).toInt))
}


opaque type Rgb = Int
object Rgb {
  inline def wrap(i: Int): Ergb = i
  inline def apply(r: UByte.ValidIntValues, g: UByte.ValidIntValues, b: UByte.ValidIntValues): Rgb =
    (r << 16) | (g << 8) | b
  inline def apply(r: UByte, g: UByte, b: UByte): Rgb =
    (r.toInt << 16) | (g.toInt << 8) | b.toInt
  def F(r: Float, g: Float, b: Float): Rgb =
    val ir = if r >= 1 then 255 else if r > 0 then (r * 255).round else 0
    val ig = if g >= 1 then 255 else if g > 0 then (g * 255).round else 0
    val ib = if b >= 1 then 255 else if b > 0 then (b * 255).round else 0
    (ir << 16) | (ig << 8) | ib
  def D(r: Double, g: Double, b: Double): Rgb =
    val ir = if r >= 1 then 255 else if r > 0 then (r * 255).round.toInt else 0
    val ig = if g >= 1 then 255 else if g > 0 then (g * 255).round.toInt else 0
    val ib = if b >= 1 then 255 else if b > 0 then (b * 255).round.toInt else 0
    (ir << 16) | (ig << 8) | ib

  extension (color: Rgb) {
    inline def argb: Argb = (color: Int) | 0xFF000000
    inline def alpha(value: UByte): Argb = ((color: Int) & 0x00FFFFFF) | (value.toInt << 24)
    inline def alpha[V <: Int | Float | Double](value: V): Argb = inline value match
      case vi: Int => inline vi match
        case inBound: UByte.ValidIntValues => ((color: Int) & 0x00FFFFFF) | (inBound << 24)
        case _ => compiletime.error("Cannot prove value is in UByte range")
      case vf: Float =>  ((color: Int) & 0x00FFFFFF) | (if vf >= 1 then 0xFF000000 else if vf > 0 then (vf.round       << 24) else 0)
      case vd: Double => ((color: Int) & 0x00FFFFFF) | (if vd >= 1 then 0xFF000000 else if vd > 0 then (vd.round.toInt << 24) else 0)
    def f21: Ergb =
      (((526345 * ( (color: Int)        & 0xFF)).toLong >>> 11)                      ) |
      (((526345 * (((color: Int) >>  8) & 0xFF)).toLong  << 10) & 0x0000003FFFE00000L) |
      (((526345 * (((color: Int) >> 16) & 0xFF)).toLong  << 31) & 0x07FFFC0000000000L)

    inline def r: UByte = UByte.wrap((((color: Int) >>> 16) & 0xFF).toByte)
    inline def g: UByte = UByte.wrap((((color: Int) >>> 8)  & 0xFF).toByte)
    inline def b: UByte = UByte.wrap( ((color: Int)         & 0xFF).toByte)
    inline def rI: Int = ((color: Int) >>> 16) & 0xFF
    inline def gI: Int = ((color: Int) >>>  8) & 0xFF
    inline def bI: Int =  (color: Int)         & 0xFF
    inline def rF: Float = (((color: Int) >>> 16) & 0xFF) * 0.003921569f
    inline def gF: Float = (((color: Int) >>>  8) & 0xFF) * 0.003921569f
    inline def bF: Float = ( (color: Int)         & 0xFF) * 0.003921569f
    inline def rD: Double = (((color: Int) >>> 16) & 0xFF) * 0.00392156862745098
    inline def gD: Double = (((color: Int) >>>  8) & 0xFF) * 0.00392156862745098
    inline def bD: Double = ( (color: Int)         & 0xFF) * 0.00392156862745098
    inline def rTo(value: UByte.ValidIntValues): Rgb = (0xFF00FFFF & (color: Int)) | (value << 16)
    inline def gTo(value: UByte.ValidIntValues): Rgb = (0xFFFF00FF & (color: Int)) | (value <<  8)
    inline def bTo(value: UByte.ValidIntValues): Rgb = (0xFFFFFF00 & (color: Int)) |  value
    inline def rTo(value: UByte): Rgb = (0xFF00FFFF & (color: Int)) | (value.toInt << 16)
    inline def gTo(value: UByte): Rgb = (0xFFFF00FF & (color: Int)) | (value.toInt <<  8)
    inline def bTo(value: UByte): Rgb = (0xFFFFFF00 & (color: Int)) |  value.toInt
    inline def rOp(inline op: UByte => UByte): Rgb = (0xFF00FFFF & (color: Int)) | (op(UByte.wrap((((color: Int) & 0xFF0000) >>> 16).toByte)).toInt << 16)
    inline def gOp(inline op: UByte => UByte): Rgb = (0xFFFF00FF & (color: Int)) | (op(UByte.wrap((((color: Int) & 0x00FF00) >>>  8).toByte)).toInt <<  8)
    inline def bOp(inline op: UByte => UByte): Rgb = (0xFFFFFF00 & (color: Int)) |  op(UByte.wrap(( (color: Int) & 0x0000FF        ).toByte)).toInt

    def pr =
      val ans = new Array[Char](7)
      var v = (color: Int)
      var k = 6
      while k >= 1 do
        val digit = v & 0xF
        ans(k) = (digit + (if (digit < 10) '0' else '7')).toChar
        v = v >>> 4
        k -= 1
      ans(0) = '#'
      new String(ans)
  }

  given Translucent[Rgb, Int] {}
}


opaque type Argb = Int
object Argb {
  inline def wrap(i: Int): Argb = i
  inline def apply(a: UByte.ValidIntValues)(r: UByte.ValidIntValues, g: UByte.ValidIntValues, b: UByte.ValidIntValues): Argb =
    (a << 24) | (r << 16) | (g << 8) | b
  inline def apply(a: UByte)(r: UByte, g: UByte, b: UByte): Argb =
    (a.toInt << 24) | (r.toInt << 16) | (g.toInt << 8) | b.toInt
  def F(a: Float)(r: Float, g: Float, b: Float): Argb =
    val ia = if a >= 1 then 255 else if a > 0 then a.round else 0
    val ir = if r >= 1 then 255 else if r > 0 then r.round else 0
    val ig = if g >= 1 then 255 else if g > 0 then g.round else 0
    val ib = if b >= 1 then 255 else if b > 0 then b.round else 0
    (ia << 24) | (ir << 16) | (ig << 8) | ib
  def D(a: Double)(r: Double, g: Double, b: Double): Argb =
    val ia = if a >= 1 then 255 else if a > 0 then a.round.toInt else 0
    val ir = if r >= 1 then 255 else if r > 0 then r.round.toInt else 0
    val ig = if g >= 1 then 255 else if g > 0 then g.round.toInt else 0
    val ib = if b >= 1 then 255 else if b > 0 then b.round.toInt else 0
    (ia << 24) | (ir << 16) | (ig << 8) | ib

  extension (color: Argb) {
    inline def rgb: Rgb = (color: Int)
    def f21: Ergb =
      val scale = ((color: Int) >>> 24) * 257.00394f
      val r = ((((color: Int) >>> 16) & 0xFF) * scale).round
      val g = ((((color: Int) >>>  8) & 0xFF) * scale).round
      val b = (( (color: Int)         & 0xFF) * scale).round
      ((r << 42) | (g << 21) | b): Ergb

    inline def a: UByte = UByte.wrap((((color: Int) >>> 24) & 0xFF).toByte)
    inline def r: UByte = UByte.wrap((((color: Int) >>> 16) & 0xFF).toByte)
    inline def g: UByte = UByte.wrap((((color: Int) >>>  8) & 0xFF).toByte)
    inline def b: UByte = UByte.wrap( ((color: Int)         & 0xFF).toByte)
    inline def aI: Int = ((color: Int) >>> 24) & 0xFF
    inline def rI: Int = ((color: Int) >>> 16) & 0xFF
    inline def gI: Int = ((color: Int) >>>  8) & 0xFF
    inline def bI: Int =  (color: Int)         & 0xFF
    inline def aF: Float = (((color: Int) >>> 24) & 0xFF) * 0.003921569f
    inline def rF: Float = (((color: Int) >>> 16) & 0xFF) * 0.003921569f
    inline def gF: Float = (((color: Int) >>>  8) & 0xFF) * 0.003921569f
    inline def bF: Float = ( (color: Int)         & 0xFF) * 0.003921569f
    inline def aD: Double = (((color: Int) >>> 24) & 0xFF) * 0.00392156862745098
    inline def rD: Double = (((color: Int) >>> 16) & 0xFF) * 0.00392156862745098
    inline def gD: Double = (((color: Int) >>>  8) & 0xFF) * 0.00392156862745098
    inline def bD: Double = ( (color: Int)         & 0xFF) * 0.00392156862745098
    inline def aTo(value: UByte.ValidIntValues): Argb = (0x00FFFFFF & (color: Int)) | (value << 24)
    inline def rTo(value: UByte.ValidIntValues): Argb = (0xFF00FFFF & (color: Int)) | (value << 16)
    inline def gTo(value: UByte.ValidIntValues): Argb = (0xFFFF00FF & (color: Int)) | (value <<  8)
    inline def bTo(value: UByte.ValidIntValues): Argb = (0xFFFFFF00 & (color: Int)) |  value
    inline def aTo(value: UByte): Argb = (0x00FFFFFF & (color: Int)) | (value.toInt << 24)
    inline def rTo(value: UByte): Argb = (0xFF00FFFF & (color: Int)) | (value.toInt << 16)
    inline def gTo(value: UByte): Argb = (0xFFFF00FF & (color: Int)) | (value.toInt <<  8)
    inline def bTo(value: UByte): Argb = (0xFFFFFF00 & (color: Int)) |  value.toInt
    inline def aOp(inline op: UByte => UByte): Argb = (0x00FFFFFF & (color: Int)) | (op(UByte.wrap((((color: Int) & 0xFF000000) >>> 24).toByte)).toInt << 24)
    inline def rOp(inline op: UByte => UByte): Argb = (0xFF00FFFF & (color: Int)) | (op(UByte.wrap((((color: Int) & 0x00FF0000) >>> 16).toByte)).toInt << 16)
    inline def gOp(inline op: UByte => UByte): Argb = (0xFFFF00FF & (color: Int)) | (op(UByte.wrap((((color: Int) & 0x0000FF00) >>>  8).toByte)).toInt <<  8)
    inline def bOp(inline op: UByte => UByte): Argb = (0xFFFFFF00 & (color: Int)) |  op(UByte.wrap(( (color: Int) & 0x000000FF        ).toByte)).toInt

    def pr =
      val ans = new Array[Char](9)
      var v = (color: Int)
      var k = 8
      while k >= 1 do
        val digit = v & 0xF
        ans(k) = (digit + (if (digit < 10) '0' else '7')).toChar
        v = v >>> 4
        k -= 1
      ans(0) = '#'
      new String(ans)
  }

  given Translucent[Argb, Int] with {}
}


opaque type Ergb = Long
object Ergb {
  inline def wrap(l: Long): Ergb = l

  extension (color: Ergb) {
    def rgb(using halo: HaloModel): Rgb = ???
     // if ((color: Long) & F80003C0001E0000L) == 0 then
     //   val scaled = ((color: Long) >> 8) - (((color: Long) & 0x))
  }

  trait HaloModel {
    def quantize(red: Int, green: Int, blue: Int): Rgb
  }
  object HaloModel {
    val default: HaloModel = new:
      def quantize(red: Int, green: Int, blue: Int): Rgb =
        var r = if red   < 0 then 0 else if red   > 0xFFFF then 0xFFFF else red
        var g = if green < 0 then 0 else if green > 0xFFFF then 0xFFFF else green
        var b = if blue  < 0 then 0 else if blue  > 0xFFFF then 0xFFFF else blue
        if g > 255 then
          r += (g - 128) >> 8
          b += (g - 128) >> 8
          g = 255
        if r > 255 then
          g += (r - 128) >> 8
          r = 255
        if b > 255 then
          g += (b - 128) >> 8
          b = 255
        if g > 255 then g = 255
        ((r << 16) | (g << 8) | b)
  }

  given Translucent[Ergb, Long] with {}
}


opaque type Ehsv = Long
object Ehsv {
  given Translucent[Ehsv, Long] with {}
}
