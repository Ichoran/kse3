// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025-26 Rex Kerr and UCSF / Kato Lab

package kse.maths.colours


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.lang.{Math => jm}

import scala.annotation.targetName

import kse.basics.{Translucent, Sayable}

import kse.maths._


object Colour {
  def u8plus_to_float(u8: Int): Float =
    (u8 & 0x7FFFFF) * 0.003921569f

  /** A unit-scale channel as a byte value: 0 at or below zero, 255 at or above one, rounded between. */
  def unit_to_u8(value: Float): Int =
    if value >= 1 then 255 else if value > 0 then (value * 255).round else 0

  def unit_to_u8(value: Double): Int =
    if value >= 1 then 255 else if value > 0 then (value * 255).round.toInt else 0

  def float_to_u8plus(value: Float): Int =
    if value >= 32767.5 then 8355713 else if value > 0 then (value * 255).round else 0

  def u8_to_packed(u8: Int): Int =
    (526345 * (u8 & 0xFF)) >>> 11

  /** Converts one 21-bit packed channel to the u8plus integer scale (255 = 1.0), rounding
    * half-up with exact integer math; negatives give 0, and the infinity encoding gives
    * the same 8355713 cap as `float_to_u8plus`.
    */
  def packed_to_u(packed: Int): Int =
    if (packed & 0x100000) != 0 then 0
    else
      val m = packed & 0xFFFF
      val e = (packed & 0xF0000) >>> 16
      if e == 0 then (m * 255 + 0x8000) >>> 16
      else if e == 15 && m == 0xFFFF then 8355713
      else
        val s = 17 - e
        ((0x10000 | m) * 255 + (1 << (s-1))) >>> s

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

  /** Replaces the 21-bit channel at `shift` (42, 21 or 0) in a packed triple; an invalid triple stays
    * invalid, and a value that cannot be encoded (NaN) makes it so.
    */
  def set_packed_float(packed: Long, shift: Int, value: Float): Long =
    if packed < 0 then -1L
    else
      val bits = float_to_bits(value)
      if bits < 0 then -1L else (packed & ~(0x1FFFFFL << shift)) | (bits.toLong << shift)

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
      fa(bits_to_float(((packed >>> 42)           ).toInt))
      fb(bits_to_float(((packed >>> 21) & 0x1FFFFF).toInt))
      fc(bits_to_float(( packed         & 0x1FFFFF).toInt))

  inline def packed_float_fn[A](packed: Long)(inline f: (Float, Float, Float) => A): A =
    val one   = if packed >= 0 then bits_to_float(((packed >>> 42)           ).toInt) else Float.NaN
    val two   = if packed >= 0 then bits_to_float(((packed >>> 21) & 0x1FFFFF).toInt) else Float.NaN
    val three = if packed >= 0 then bits_to_float(( packed         & 0x1FFFFF).toInt) else Float.NaN
    f(one, two, three)
}


opaque type Rgb = Int
object Rgb {
  inline def wrap(i: Int): Rgb = i
  inline def apply(r: UByte.ValidIntValues, g: UByte.ValidIntValues, b: UByte.ValidIntValues): Rgb =
    (r << 16) | (g << 8) | b
  inline def apply(r: UByte, g: UByte, b: UByte): Rgb =
    (r.toInt << 16) | (g.toInt << 8) | b.toInt
  def F(r: Float, g: Float, b: Float): Rgb =
    (Colour.unit_to_u8(r) << 16) | (Colour.unit_to_u8(g) << 8) | Colour.unit_to_u8(b)
  def D(r: Double, g: Double, b: Double): Rgb =
    (Colour.unit_to_u8(r) << 16) | (Colour.unit_to_u8(g) << 8) | Colour.unit_to_u8(b)

  // The CSS named colours, in their CSS order, with both spellings of grey.
  final val AliceBlue: Rgb = 0xF0F8FF
  final val AntiqueWhite: Rgb = 0xFAEBD7
  final val Aqua: Rgb = 0x00FFFF
  final val Aquamarine: Rgb = 0x7FFFD4
  final val Azure: Rgb = 0xF0FFFF
  final val Beige: Rgb = 0xF5F5DC
  final val Bisque: Rgb = 0xFFE4C4
  final val Black: Rgb = 0x000000
  final val BlanchedAlmond: Rgb = 0xFFEBCD
  final val Blue: Rgb = 0x0000FF
  final val BlueViolet: Rgb = 0x8A2BE2
  final val Brown: Rgb = 0xA52A2A
  final val Burlywood: Rgb = 0xDEB887
  final val CadetBlue: Rgb = 0x5F9EA0
  final val Chartreuse: Rgb = 0x7FFF00
  final val Chocolate: Rgb = 0xD2691E
  final val Coral: Rgb = 0xFF7F50
  final val CornflowerBlue: Rgb = 0x6495ED
  final val Cornsilk: Rgb = 0xFFF8DC
  final val Crimson: Rgb = 0xDC143C
  final val Cyan: Rgb = 0x00FFFF
  final val DarkBlue: Rgb = 0x00008B
  final val DarkCyan: Rgb = 0x008B8B
  final val DarkGoldenrod: Rgb = 0xB8860B
  final val DarkGray: Rgb = 0xA9A9A9
  final val DarkGreen: Rgb = 0x006400
  final val DarkGrey: Rgb = 0xA9A9A9
  final val DarkKhaki: Rgb = 0xBDB76B
  final val DarkMagenta: Rgb = 0x8B008B
  final val DarkOliveGreen: Rgb = 0x556B2F
  final val DarkOrange: Rgb = 0xFF8C00
  final val DarkOrchid: Rgb = 0x9932CC
  final val DarkRed: Rgb = 0x8B0000
  final val DarkSalmon: Rgb = 0xE9967A
  final val DarkSeaGreen: Rgb = 0x8FBC8F
  final val DarkSlateBlue: Rgb = 0x483D8B
  final val DarkSlateGray: Rgb = 0x2F4F4F
  final val DarkSlateGrey: Rgb = 0x2F4F4F
  final val DarkTurquoise: Rgb = 0x00CED1
  final val DarkViolet: Rgb = 0x9400D3
  final val DeepPink: Rgb = 0xFF1493
  final val DeepSkyBlue: Rgb = 0x00BFFF
  final val DimGray: Rgb = 0x696969
  final val DimGrey: Rgb = 0x696969
  final val DodgerBlue: Rgb = 0x1E90FF
  final val Firebrick: Rgb = 0xB22222
  final val FloralWhite: Rgb = 0xFFFAF0
  final val ForestGreen: Rgb = 0x228B22
  final val Fuchsia: Rgb = 0xFF00FF
  final val Gainsboro: Rgb = 0xDCDCDC
  final val GhostWhite: Rgb = 0xF8F8FF
  final val Gold: Rgb = 0xFFD700
  final val Goldenrod: Rgb = 0xDAA520
  final val Gray: Rgb = 0x808080
  final val Green: Rgb = 0x008000
  final val GreenYellow: Rgb = 0xADFF2F
  final val Grey: Rgb = 0x808080
  final val Honeydew: Rgb = 0xF0FFF0
  final val HotPink: Rgb = 0xFF69B4
  final val IndianRed: Rgb = 0xCD5C5C
  final val Indigo: Rgb = 0x4B0082
  final val Ivory: Rgb = 0xFFFFF0
  final val Khaki: Rgb = 0xF0E68C
  final val Lavender: Rgb = 0xE6E6FA
  final val LavenderBlush: Rgb = 0xFFF0F5
  final val LawnGreen: Rgb = 0x7CFC00
  final val LemonChiffon: Rgb = 0xFFFACD
  final val LightBlue: Rgb = 0xADD8E6
  final val LightCoral: Rgb = 0xF08080
  final val LightCyan: Rgb = 0xE0FFFF
  final val LightGoldenrodYellow: Rgb = 0xFAFAD2
  final val LightGray: Rgb = 0xD3D3D3
  final val LightGreen: Rgb = 0x90EE90
  final val LightGrey: Rgb = 0xD3D3D3
  final val LightPink: Rgb = 0xFFB6C1
  final val LightSalmon: Rgb = 0xFFA07A
  final val LightSeaGreen: Rgb = 0x20B2AA
  final val LightSkyBlue: Rgb = 0x87CEFA
  final val LightSlateGray: Rgb = 0x778899
  final val LightSlateGrey: Rgb = 0x778899
  final val LightSteelBlue: Rgb = 0xB0C4DE
  final val LightYellow: Rgb = 0xFFFFE0
  final val Lime: Rgb = 0x00FF00
  final val LimeGreen: Rgb = 0x32CD32
  final val Linen: Rgb = 0xFAF0E6
  final val Magenta: Rgb = 0xFF00FF
  final val Maroon: Rgb = 0x800000
  final val MediumAquamarine: Rgb = 0x66CDAA
  final val MediumBlue: Rgb = 0x0000CD
  final val MediumOrchid: Rgb = 0xBA55D3
  final val MediumPurple: Rgb = 0x9370DB
  final val MediumSeaGreen: Rgb = 0x3CB371
  final val MediumSlateBlue: Rgb = 0x7B68EE
  final val MediumSpringGreen: Rgb = 0x00FA9A
  final val MediumTurquoise: Rgb = 0x48D1CC
  final val MediumVioletRed: Rgb = 0xC71585
  final val MidnightBlue: Rgb = 0x191970
  final val MintCream: Rgb = 0xF5FFFA
  final val MistyRose: Rgb = 0xFFE4E1
  final val Moccasin: Rgb = 0xFFE4B5
  final val NavajoWhite: Rgb = 0xFFDEAD
  final val Navy: Rgb = 0x000080
  final val OldLace: Rgb = 0xFDF5E6
  final val Olive: Rgb = 0x808000
  final val OliveDrab: Rgb = 0x6B8E23
  final val Orange: Rgb = 0xFFA500
  final val OrangeRed: Rgb = 0xFF4500
  final val Orchid: Rgb = 0xDA70D6
  final val PaleGoldenrod: Rgb = 0xEEE8AA
  final val PaleGreen: Rgb = 0x98FB98
  final val PaleTurquoise: Rgb = 0xAFEEEE
  final val PaleVioletRed: Rgb = 0xDB7093
  final val PapayaWhip: Rgb = 0xFFEFD5
  final val PeachPuff: Rgb = 0xFFDAB9
  final val Peru: Rgb = 0xCD853F
  final val Pink: Rgb = 0xFFC0CB
  final val Plum: Rgb = 0xDDA0DD
  final val PowderBlue: Rgb = 0xB0E0E6
  final val Purple: Rgb = 0x800080
  final val RebeccaPurple: Rgb = 0x663399
  final val Red: Rgb = 0xFF0000
  final val RosyBrown: Rgb = 0xBC8F8F
  final val RoyalBlue: Rgb = 0x4169E1
  final val SaddleBrown: Rgb = 0x8B4513
  final val Salmon: Rgb = 0xFA8072
  final val SandyBrown: Rgb = 0xF4A460
  final val SeaGreen: Rgb = 0x2E8B57
  final val Seashell: Rgb = 0xFFF5EE
  final val Sienna: Rgb = 0xA0522D
  final val Silver: Rgb = 0xC0C0C0
  final val SkyBlue: Rgb = 0x87CEEB
  final val SlateBlue: Rgb = 0x6A5ACD
  final val SlateGray: Rgb = 0x708090
  final val SlateGrey: Rgb = 0x708090
  final val Snow: Rgb = 0xFFFAFA
  final val SpringGreen: Rgb = 0x00FF7F
  final val SteelBlue: Rgb = 0x4682B4
  final val Tan: Rgb = 0xD2B48C
  final val Teal: Rgb = 0x008080
  final val Thistle: Rgb = 0xD8BFD8
  final val Tomato: Rgb = 0xFF6347
  final val Turquoise: Rgb = 0x40E0D0
  final val Violet: Rgb = 0xEE82EE
  final val Wheat: Rgb = 0xF5DEB3
  final val White: Rgb = 0xFFFFFF
  final val WhiteSmoke: Rgb = 0xF5F5F5
  final val Yellow: Rgb = 0xFFFF00
  final val YellowGreen: Rgb = 0x9ACD32
  extension (color: Rgb) {
    inline def unwrap: Int = color
    inline def argb: Argb = (color: Int) | 0xFF000000
    /** This colour with an alpha: a byte, a literal in byte range, or a unit-scale fraction. */
    inline def aTo(value: UByte): Argb = ((color: Int) & 0x00FFFFFF) | (value.toInt << 24)
    inline def aTo[V <: Int | Float | Double](value: V): Argb = inline value match
      case vi: Int => inline vi match
        case inBound: UByte.ValidIntValues => ((color: Int) & 0x00FFFFFF) | (inBound << 24)
        case _ => compiletime.error("Cannot prove value is in UByte range")
      case vf: Float =>  ((color: Int) & 0x00FFFFFF) | (Colour.unit_to_u8(vf) << 24)
      case vd: Double => ((color: Int) & 0x00FFFFFF) | (Colour.unit_to_u8(vd) << 24)
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

  given Sayable[Rgb] = (x, m, _) => m += x.pr
}


opaque type Argb = Int
object Argb {
  inline def wrap(i: Int): Argb = i
  inline def apply(a: UByte.ValidIntValues)(r: UByte.ValidIntValues, g: UByte.ValidIntValues, b: UByte.ValidIntValues): Argb =
    (a << 24) | (r << 16) | (g << 8) | b
  inline def apply(a: UByte)(r: UByte, g: UByte, b: UByte): Argb =
    (a.toInt << 24) | (r.toInt << 16) | (g.toInt << 8) | b.toInt
  def F(a: Float)(r: Float, g: Float, b: Float): Argb =
    (Colour.unit_to_u8(a) << 24) | (Colour.unit_to_u8(r) << 16) | (Colour.unit_to_u8(g) << 8) | Colour.unit_to_u8(b)
  def D(a: Double)(r: Double, g: Double, b: Double): Argb =
    (Colour.unit_to_u8(a) << 24) | (Colour.unit_to_u8(r) << 16) | (Colour.unit_to_u8(g) << 8) | Colour.unit_to_u8(b)

  extension (color: Argb) {
    inline def unwrap: Int = color

    /** The colour channels alone, alpha dropped: what this is made of, not what it looks like. */
    inline def rgbPart: Rgb = (color: Int) & 0x00FFFFFF

    /** The colour channels composited over `bg` by alpha, black by default: what this looks like on top of it. */
    def rgbFlat(bg: Rgb = Rgb.Black): Rgb =
      val a = ((color: Int) >>> 24) & 0xFF
      if a == 255 then (color: Int) & 0x00FFFFFF
      else if a == 0 then bg
      else
        val na = 255 - a
        val r = ((((color: Int) >>> 16) & 0xFF) * a + Rgb.rI(bg) * na + 127) / 255
        val g = ((((color: Int) >>>  8) & 0xFF) * a + Rgb.gI(bg) * na + 127) / 255
        val b = (( (color: Int)         & 0xFF) * a + Rgb.bI(bg) * na + 127) / 255
        (r << 16) | (g << 8) | b

    /** The light this pixel casts over black, at extended precision: every channel scaled by alpha, so a
      * translucent colour comes out dim rather than losing its alpha.  For another background, flatten
      * first: `rgbFlat(bg).f21`.
      */
    def f21: Ergb =
      val a = Argb.aF(color)
      Ergb(Argb.rF(color) * a, Argb.gF(color) * a, Argb.bF(color) * a)

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

  given Sayable[Argb] = (x, m, _) => m += x.pr
}


opaque type Ergb = Long
object Ergb {
  inline def wrap(l: Long): Ergb = l

  /** Packs red, green, blue, each as a 21-bit float on the display scale: 1.0 is full SDR
    * brightness (quantizing to 255); larger values are overbright, redistributed by the
    * `HaloModel` on conversion to `Rgb`.
    */
  inline def apply(r: Float, g: Float, b: Float): Ergb = Colour.pack_floats(r, g, b)

  /** The same from doubles, rounded to the 21-bit floats. */
  inline def D(r: Double, g: Double, b: Double): Ergb = Colour.pack_floats(r.toFloat, g.toFloat, b.toFloat)

  /** An 8-bit colour on the display scale, 255 to exactly 1.0: the same as `rgb.f21`. */
  inline def from(rgb: Rgb): Ergb = Rgb.f21(rgb)

  /** The light an 8-bit colour with alpha casts over black: the same as `argb.f21`. */
  @targetName("fromArgb")
  inline def from(argb: Argb): Ergb = Argb.f21(argb)

  extension (color: Ergb) {
    inline def unwrap: Long = color

    /** Red on the display scale, 1.0 full SDR brightness and larger overbright; NaN if invalid. */
    def r: Float =
      val x: Long = color
      if x < 0 then Float.NaN else Colour.bits_to_float((x >>> 42).toInt)

    /** Green, likewise. */
    def g: Float =
      val x: Long = color
      if x < 0 then Float.NaN else Colour.bits_to_float((x >>> 21).toInt & 0x1FFFFF)

    /** Blue, likewise. */
    def b: Float =
      val x: Long = color
      if x < 0 then Float.NaN else Colour.bits_to_float((x & 0x1FFFFF).toInt)

    /** This colour with red replaced: an invalid colour stays invalid, and a NaN makes it so. */
    def rTo(value: Float): Ergb = Colour.set_packed_float(color, 42, value)
    def gTo(value: Float): Ergb = Colour.set_packed_float(color, 21, value)
    def bTo(value: Float): Ergb = Colour.set_packed_float(color,  0, value)
    inline def rOp(inline f: Float => Float): Ergb = Colour.set_packed_float(color, 42, f(Ergb.r(color)))
    inline def gOp(inline f: Float => Float): Ergb = Colour.set_packed_float(color, 21, f(Ergb.g(color)))
    inline def bOp(inline f: Float => Float): Ergb = Colour.set_packed_float(color,  0, f(Ergb.b(color)))

    /** Passes red, green, blue to `rgbf` on the display scale, overbright as stored; all NaN if invalid. */
    inline def rgbFn[A](inline rgbf: (Float, Float, Float) => A): A = Colour.packed_float_fn(color)(rgbf)

    /** Quantizes to 8-bit `Rgb` via the ambient `HaloModel`: 1.0 maps to 255, overbright
      * channels bleed into their neighbors per the model, negative channels clamp to
      * zero, and the NaN sentinel gives black.
      */
    def rgb(using halo: HaloModel): Rgb =
      if (color: Long) < 0 then Rgb.wrap(0)
      else halo.quantize(
        Colour.packed_to_u((((color: Long) >>> 42)           ).toInt),
        Colour.packed_to_u((((color: Long) >>> 21) & 0x1FFFFF).toInt),
        Colour.packed_to_u((( color: Long)         & 0x1FFFFF).toInt)
      )

    /** The same colour as hexcone HSV, overbright kept in the value. */
    def ehsv: Ehsv = Ehsv.from(color)

    /** Brighter or dimmer by `x`, every channel scaled; an invalid colour stays invalid. */
    def *(x: Float): Ergb = Colour.packed_float_fn(color)((r, g, b) => Colour.pack_floats(r * x, g * x, b * x))
    inline def *(x: Double): Ergb = Ergb.*(color)(x.toFloat)
    inline def /(x: Float): Ergb = Ergb.*(color)(1f/x)
    inline def /(x: Double): Ergb = Ergb.*(color)((1.0/x).toFloat)

    /** Light added to light: the channels summed, overbright where they exceed 1.0. */
    def +(that: Ergb): Ergb =
      Colour.packed_float_fn(color)((r1, g1, b1) => Colour.packed_float_fn(that)((r2, g2, b2) => Colour.pack_floats(r1 + r2, g1 + g2, b1 + b2)))

    def pr: String =
      Colour.packed_float_fn(color): (r, g, b) =>
        f"Ergb[$r%.3f $g%.3f $b%.3f]"
  }

  trait HaloModel {
    def quantize(red: Int, green: Int, blue: Int): Rgb
  }
  object HaloModel {
    given default: HaloModel = new:
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

  given Sayable[Ergb] = (x, m, _) => m += x.pr
}


opaque type Ehsv = Long
object Ehsv {
  inline def wrap(l: Long): Ehsv = l

  /** Packs hue, saturation, and value, each as a 21-bit float.  Hue is a turn fraction —
    * red at 0, green at 1/3, blue at 2/3 — and any value is accepted, wrapping mod 1 on
    * conversion.  Saturation is nominally in [0, 1].  Value is nonnegative, with 1.0 full
    * SDR brightness (quantizing to 255) and larger values overbright.
    */
  inline def apply(h: Float, s: Float, v: Float): Ehsv = Colour.pack_floats(h, s, v)

  /** The same from doubles, rounded to the 21-bit floats. */
  inline def D(h: Double, s: Double, v: Double): Ehsv = Colour.pack_floats(h.toFloat, s.toFloat, v.toFloat)

  /** Hexcone HSV from red, green, blue.  Hue is a turn fraction in [0, 1): red at 0,
    * green at 1/3, blue at 2/3; grey (and black) take hue 0.  Value is the max channel,
    * so overbright inputs give v > 1 rather than clamping.
    */
  def from(r: Float, g: Float, b: Float): Ehsv =
    val max = if r >= g then (if r >= b then r else b) else (if g >= b then g else b)
    val min = if r <= g then (if r <= b then r else b) else (if g <= b then g else b)
    val c = max - min
    val h =
      if !(c > 0) then 0f
      else if max == r then
        val t = (g - b) / (6 * c)
        if t < 0 then t + 1f else t
      else if max == g then (b - r) / (6 * c) + 1f/3
      else                  (r - g) / (6 * c) + 2f/3
    val s = if max > 0 then c / max else 0f
    Colour.pack_floats(h, s, max)

  /** Hexcone HSV of an 8-bit colour, computed on its stored (gamma-encoded) channels. */
  def from(rgb: Rgb): Ehsv = from(Rgb.rF(rgb), Rgb.gF(rgb), Rgb.bF(rgb))

  /** Hexcone HSV of an extended RGB colour, preserving overbright values. */
  def from(ergb: Ergb): Ehsv = Colour.packed_float_fn(ergb)((r, g, b) => from(r, g, b))

  extension (color: Ehsv) {
    inline def unwrap: Long = color

    /** Hue as a turn fraction, as stored (not wrapped into [0, 1)); NaN if invalid. */
    def h: Float =
      val x: Long = color
      if x < 0 then Float.NaN else Colour.bits_to_float((x >>> 42).toInt)

    /** Saturation, nominally in [0, 1]; NaN if invalid. */
    def s: Float =
      val x: Long = color
      if x < 0 then Float.NaN else Colour.bits_to_float((x >>> 21).toInt & 0x1FFFFF)

    /** Value, with 1.0 full SDR brightness and larger values overbright; NaN if invalid. */
    def v: Float =
      val x: Long = color
      if x < 0 then Float.NaN else Colour.bits_to_float((x & 0x1FFFFF).toInt)

    /** This colour with the hue replaced, as given (not wrapped): an invalid colour stays invalid, and a NaN makes it so. */
    def hTo(value: Float): Ehsv = Colour.set_packed_float(color, 42, value)
    def sTo(value: Float): Ehsv = Colour.set_packed_float(color, 21, value)
    def vTo(value: Float): Ehsv = Colour.set_packed_float(color,  0, value)
    /** Hue rotated by `f`, which sees the stored (unwrapped) hue; likewise `sOp` and `vOp`, the latter an exposure change. */
    inline def hOp(inline f: Float => Float): Ehsv = Colour.set_packed_float(color, 42, f(Ehsv.h(color)))
    inline def sOp(inline f: Float => Float): Ehsv = Colour.set_packed_float(color, 21, f(Ehsv.s(color)))
    inline def vOp(inline f: Float => Float): Ehsv = Colour.set_packed_float(color,  0, f(Ehsv.v(color)))

    /** Hexcone reconstruction: passes red, green, blue to `rgbf`, hue wrapped mod 1 and
      * channels on the value scale so overbright passes through; all NaN if invalid.
      */
    inline def rgbFn[A](inline rgbf: (Float, Float, Float) => A): A =
      val x: Long = color
      var r, g, b = Float.NaN
      if x >= 0 then
        val H = Colour.bits_to_float((x >>> 42).toInt)
        val S = Colour.bits_to_float((x >>> 21).toInt & 0x1FFFFF)
        val V = Colour.bits_to_float((x & 0x1FFFFF).toInt)
        var t = (H % 1f) * 6
        if t < 0 then t += 6
        val i = t.toInt
        val f = t - i
        val p = V * (1 - S)
        val q = V * (1 - S * f)
        val u = V * (1 - S * (1 - f))
        i match
          case 0 => r = V; g = u; b = p
          case 1 => r = q; g = V; b = p
          case 2 => r = p; g = V; b = u
          case 3 => r = p; g = q; b = V
          case 4 => r = u; g = p; b = V
          case _ => r = V; g = p; b = q
      rgbf(r, g, b)

    /** Quantizes to 8-bit `Rgb` via the ambient `HaloModel`, like `Ergb.rgb`. */
    def rgb(using halo: Ergb.HaloModel): Rgb =
      Ehsv.rgbFn(color)((r, g, b) =>
        halo.quantize(Colour.float_to_u8plus(r), Colour.float_to_u8plus(g), Colour.float_to_u8plus(b)))

    /** The same colour as extended RGB, without quantization. */
    def ergb: Ergb = Ehsv.rgbFn(color)((r, g, b) => Ergb(r, g, b))

    def pr: String =
      Colour.packed_float_fn(color): (h, s, v) =>
        f"Ehsv[$h%.3f $s%.3f $v%.3f]"
  }

  given Translucent[Ehsv, Long] with {}

  given Sayable[Ehsv] = (x, m, _) => m += x.pr
}

opaque type Oklab = Long
object Oklab {
  inline def wrap(l: Long): Oklab = l
  inline def apply(l: Float, a: Float, b: Float): Oklab = Colour.pack_floats(l, a, b)
  inline def lch(l: Float, c: Float, h: Float): Oklab = Colour.pack_floats(l, (c * h.cos).toFloat, (c * h.sin).toFloat)

  def from(r: Float, g: Float, b: Float): Oklab =
    val l = (r * 0.4122214708f  +  g * 0.5363325363f  +  b * 0.0514459929f).cbrt.toFloat
    val m = (r * 0.2119034982f  +  g * 0.6806995451f  +  b * 0.1073969566f).cbrt.toFloat
    val s = (r * 0.0883024619f  +  g * 0.2817188376f  +  b * 0.6299787005f).cbrt.toFloat

    Colour.pack_floats(
      l * 0.2104542553f  +  m * 0.7936177850f  -  s * 0.0040720468f,
      l * 1.9779984951f  -  m * 2.4285922050f  +  s * 0.4505937099f,
      l * 0.0259040371f  +  m * 0.7827717662f  -  s * 0.8086757660f
    )

  def sRGB(rgb: Rgb): Oklab =
    val r = (Rgb.rF(rgb) pow 2.2).toFloat
    val g = (Rgb.gF(rgb) pow 2.2).toFloat
    val b = (Rgb.bF(rgb) pow 2.2).toFloat
    from(r, g, b)

  def lRGB(rgb: Rgb): Oklab =
    val r = Rgb.rF(rgb)
    val g = Rgb.gF(rgb)
    val b = Rgb.bF(rgb)
    from(r, g, b)

  /** From extended RGB read as gamma-encoded (2.2) channels; a channel below zero makes it invalid. */
  def sRGB(ergb: Ergb): Oklab =
    Colour.packed_float_fn(ergb)((r, g, b) => from((r pow 2.2).toFloat, (g pow 2.2).toFloat, (b pow 2.2).toFloat))

  /** From extended RGB read as linear light, overbright and all. */
  def lRGB(ergb: Ergb): Oklab =
    Colour.packed_float_fn(ergb)((r, g, b) => from(r, g, b))

  // The CSS named colours as Oklab, from their sRGB values: generated by the snippet at the end of this object.
  final val AliceBlue: Oklab = 0x03E756002BD002D3L
  final val AntiqueWhite: Oklab = 0x03CAFC003EA00782L
  final val Aqua: Oklab = 0x039F2204C8500A16L
  final val Aquamarine: Oklab = 0x03A99E040F60060EL
  final val Azure: Oklab = 0x03F5060077700122L
  final val Beige: Oklab = 0x03DBD2004C6007D2L
  final val Bisque: Oklab = 0x03BD10007E400C44L
  final val Black: Oklab = 0x0000000000000000L
  final val BlanchedAlmond: Oklab = 0x03CCAC0048E00B01L
  final val Blue: Oklab = 0x01CEDE0109F04FC0L
  final val BlueViolet: Oklab = 0x022174044E503753L
  final val Brown: Oklab = 0x01EA1404C16012A8L
  final val Burlywood: Oklab = 0x033BDC00AFC01320L
  final val CadetBlue: Oklab = 0x02A56E0204900551L
  final val Chartreuse: Oklab = 0x0390060613202F1AL
  final val Chocolate: Oklab = 0x028B5C032A401F54L
  final val Coral: Oklab = 0x02F218040BA01C26L
  final val CornflowerBlue: Oklab = 0x02B61600B9102385L
  final val Cornsilk: Oklab = 0x03E972001C60093FL
  final val Crimson: Oklab = 0x0247FC06C0A01427L
  final val Cyan: Oklab = 0x039F2204C8500A16L
  final val DarkBlue: Oklab = 0x01289E00AA70331BL
  final val DarkCyan: Oklab = 0x0252260310900676L
  final val DarkGoldenrod: Oklab = 0x029F8C0099A02216L
  final val DarkGray: Oklab = 0x02F5540000000000L
  final val DarkGreen: Oklab = 0x01BE9603C4601721L
  final val DarkGrey: Oklab = 0x02F5540000000000L
  final val DarkKhaki: Oklab = 0x03165E00CD001895L
  final val DarkMagenta: Oklab = 0x01CC7005A1701BC0L
  final val DarkOliveGreen: Oklab = 0x01FB2A01C8E01389L
  final val DarkOrange: Oklab = 0x03029802F420271DL
  final val DarkOrchid: Oklab = 0x0229C004F7902C40L
  final val DarkRed: Oklab = 0x019C10049C6014A5L
  final val DarkSalmon: Oklab = 0x03042402A34011A2L
  final val DarkSeaGreen: Oklab = 0x03055E0215E00BCCL
  final val DarkSlateBlue: Oklab = 0x01A2EC0121F02026L
  final val DarkSlateGray: Oklab = 0x0196BE013F3002CBL
  final val DarkSlateGrey: Oklab = 0x0196BE013F3002CBL
  final val DarkTurquoise: Oklab = 0x0319E6040C7009A8L
  final val DarkViolet: Oklab = 0x0212240562B0335AL
  final val DeepPink: Oklab = 0x029D58086F30044CL
  final val DeepSkyBlue: Oklab = 0x0308CA0315501E53L
  final val DimGray: Oklab = 0x0216340000000000L
  final val DimGrey: Oklab = 0x0216340000000000L
  final val DodgerBlue: Oklab = 0x029DB601D6D02E41L
  final val Firebrick: Oklab = 0x01FB2005432015CDL
  final val FloralWhite: Oklab = 0x03F258000A800384L
  final val ForestGreen: Oklab = 0x023D2A0474C01B11L
  final val Fuchsia: Oklab = 0x02CE8408C9502B4DL
  final val Gainsboro: Oklab = 0x0396EC0000000000L
  final val GhostWhite: Oklab = 0x03ED480014900234L
  final val Gold: Oklab = 0x038E2E0096C02E8CL
  final val Goldenrod: Oklab = 0x030570006EC02623L
  final val Gray: Oklab = 0x0269B80000000000L
  final val Green: Oklab = 0x02173A0483C01BB8L
  final val GreenYellow: Oklab = 0x03A7A604C7802E28L
  final val Grey: Oklab = 0x0269B80000000000L
  final val Honeydew: Oklab = 0x03F0F200A4C0038DL
  final val HotPink: Oklab = 0x02EA98063F500783L
  final val IndianRed: Oklab = 0x0277740452A00E2FL
  final val Indigo: Oklab = 0x015A3C02EC5027B3L
  final val Ivory: Oklab = 0x03FC02002CE004A9L
  final val Khaki: Oklab = 0x03A97E00CC201BC7L
  final val Lavender: Oklab = 0x03BB4C003AB00671L
  final val LavenderBlush: Oklab = 0x03E0900089B0005FL
  final val LawnGreen: Oklab = 0x038786060E602EAAL
  final val LemonChiffon: Oklab = 0x03E9E20062400E3BL
  final val LightBlue: Oklab = 0x0370360131F007DAL
  final val LightCoral: Oklab = 0x02E86C0417800C8EL
  final val LightCyan: Oklab = 0x03E9EE00F4900249L
  final val LightGoldenrodYellow: Oklab = 0x03E722007B400C5DL
  final val LightGray: Oklab = 0x037B340000000000L
  final val LightGreen: Oklab = 0x037AF20401A01732L
  final val LightGrey: Oklab = 0x037B340000000000L
  final val LightPink: Oklab = 0x0366B402A7A00353L
  final val LightSalmon: Oklab = 0x032F7C02E2801590L
  final val LightSeaGreen: Oklab = 0x02C75E03AD7004B0L
  final val LightSkyBlue: Oklab = 0x034BA601ABD013DCL
  final val LightSlateGray: Oklab = 0x027DAA00651007D8L
  final val LightSlateGrey: Oklab = 0x027DAA00651007D8L
  final val LightSteelBlue: Oklab = 0x034562005A900A69L
  final val LightYellow: Oklab = 0x03F806005E200994L
  final val Lime: Oklab = 0x03773E077C002DF3L
  final val LimeGreen: Oklab = 0x02FA1205ED802402L
  final val Linen: Oklab = 0x03D8840033C003F2L
  final val Magenta: Oklab = 0x02CE8408C9502B4DL
  final val Maroon: Oklab = 0x0183E4045720136FL
  final val MediumAquamarine: Oklab = 0x031ECA0377C0054CL
  final val MediumBlue: Oklab = 0x018A6A00E29043F4L
  final val MediumOrchid: Oklab = 0x02822804FD902226L
  final val MediumPurple: Oklab = 0x0284BC024DF02430L
  final val MediumSeaGreen: Oklab = 0x02BFCA0441800FBBL
  final val MediumSlateBlue: Oklab = 0x026C6C01AF502FDAL
  final val MediumSpringGreen: Oklab = 0x03784E0611E01464L
  final val MediumTurquoise: Oklab = 0x03288A03AFF00613L
  final val MediumVioletRed: Oklab = 0x0236EC071E100AA9L
  final val MidnightBlue: Oklab = 0x0119240007302733L
  final val MintCream: Oklab = 0x03F73E005EA000CDL
  final val MistyRose: Oklab = 0x03C44C00D8200331L
  final val Moccasin: Oklab = 0x03B9C4004E4010D0L
  final val NavajoWhite: Oklab = 0x03AC7C007BC011FCL
  final val Navy: Oklab = 0x01173A00A070301CL
  final val OldLace: Oklab = 0x03E48C0013E00554L
  final val Olive: Oklab = 0x0255F20160A01EAAL
  final val OliveDrab: Oklab = 0x02690A02AF801D4FL
  final val Orange: Oklab = 0x032E6401BD802975L
  final val OrangeRed: Oklab = 0x02A264060A8021ECL
  final val Orchid: Oklab = 0x02D17404F9701839L
  final val PaleGoldenrod: Oklab = 0x03B1420093A013A2L
  final val PaleGreen: Oklab = 0x039E7A0429201816L
  final val PaleTurquoise: Oklab = 0x03A2D601EA90046DL
  final val PaleVioletRed: Oklab = 0x02B8B40471E0002EL
  final val PapayaWhip: Oklab = 0x03D64C0033E0096AL
  final val PeachPuff: Oklab = 0x03A76000D2400D89L
  final val Peru: Oklab = 0x02B9D001CC801C88L
  final val Pink: Oklab = 0x037B700248400236L
  final val Plum: Oklab = 0x0325F802D9500F11L
  final val PowderBlue: Oklab = 0x038322016E10057DL
  final val Purple: Oklab = 0x01B170054CD01A1FL
  final val RebeccaPurple: Oklab = 0x01BF5402F350238DL
  final val Red: Oklab = 0x0283040732002037L
  final val RosyBrown: Oklab = 0x02C9C401AA000478L
  final val RoyalBlue: Oklab = 0x023D760070B0306CL
  final val SaddleBrown: Oklab = 0x01E050025C801734L
  final val Salmon: Oklab = 0x02F280043DE0123DL
  final val SandyBrown: Oklab = 0x0325E401FC001C58L
  final val SeaGreen: Oklab = 0x02484A038EE00D4FL
  final val Seashell: Oklab = 0x03E834003C8002FDL
  final val Sienna: Oklab = 0x021B3402AEC015AFL
  final val Silver: Oklab = 0x033F9C0000000000L
  final val SkyBlue: Oklab = 0x0345EA01D6500ECDL
  final val SlateBlue: Oklab = 0x022CEC017F302AC2L
  final val SlateGray: Oklab = 0x0261EE0060900780L
  final val SlateGrey: Oklab = 0x0261EE0060900780L
  final val Snow: Oklab = 0x03F5080028600064L
  final val SpringGreen: Oklab = 0x0380120692C01CEFL
  final val SteelBlue: Oklab = 0x025C96015EF01777L
  final val Tan: Oklab = 0x0329680086A00FC6L
  final val Teal: Oklab = 0x022F4A02E2900615L
  final val Thistle: Oklab = 0x0359440125700631L
  final val Tomato: Oklab = 0x02C8B40545401B26L
  final val Turquoise: Oklab = 0x034C1E0434100327L
  final val Violet: Oklab = 0x030E8804FB5019AFL
  final val Wheat: Oklab = 0x03A50C0039A00F65L
  final val White: Oklab = 0x0400000000000000L
  final val WhiteSmoke: Oklab = 0x03E2640000000000L
  final val Yellow: Oklab = 0x03DF360248A032D5L
  final val YellowGreen: Oklab = 0x03275A0388C0266BL

  def blend(c1: Oklab, w1: Float)(c2: Oklab, w2: Float): Oklab =
    val x1: Long = c1
    val x2: Long = c2
    if x1 < 0 || x2 < 0 then -1L
    else
      val l1 = Colour.bits_to_float((x1 >>> 42).toInt)
      val l2 = Colour.bits_to_float((x2 >>> 42).toInt)
      val l = l1*w1 + l2*w2
      if l >= 0 then
        val d = 1.0f / (if w1*w2 < 0 then jm.max(w1.abs, w2.abs) else (w1 + w2).abs)
        val u1 = w1*d
        val u2 = w2*d
        val a1 = Colour.bits_to_float((x1 >>> 21).toInt & 0x1FFFFF)
        val a2 = Colour.bits_to_float((x2 >>> 21).toInt & 0x1FFFFF)
        val a = a1*u1 + a2*u2
        val b1 = Colour.bits_to_float((x1 & 0x1FFFFF).toInt)
        val b2 = Colour.bits_to_float((x2 & 0x1FFFFF).toInt)
        val b = b1*u1 + b2*u2
        Colour.pack_floats(l, a, b)
      else 0L

  def spiral(c1: Oklab, c2: Oklab, fraction: Float): Oklab =
    val x1: Long = c1
    val x2: Long = c2
    if x1 < 0 || x2 < 0 then -1L
    else
      val p = fraction.clamp(0f, 1f)
      val q = 1-p
      val l1 = Colour.bits_to_float((x1 >>> 42).toInt)
      val l2 = Colour.bits_to_float((x2 >>> 42).toInt)
      val a1 = Colour.bits_to_float((x1 >>> 21).toInt & 0x1FFFFF)
      val a2 = Colour.bits_to_float((x2 >>> 21).toInt & 0x1FFFFF)
      val b1 = Colour.bits_to_float((x1 & 0x1FFFFF).toInt)
      val b2 = Colour.bits_to_float((x2 & 0x1FFFFF).toInt)
      val c1 = (a1.sq + b1.sq).sqrt.toFloat
      val c2 = (a2.sq + b2.sq).sqrt.toFloat
      val h1 = jm.atan2(b1, a1).toFloat
      val h2 = { val v = jm.atan2(b2, a2); if v < h1 then v + NumericConstants.TwoPi else v }.toFloat
      Oklab.lch(l1*p + l2*q, c1*p + c2*q, h1*p + h2*q)

  extension (color: Oklab) {
    inline def unwrap: Long = color
    def l: Float =
      val x: Long = color
      if x < 0 then Float.NaN else Colour.bits_to_float((x >>> 42).toInt)
    inline def lOp(inline f: Float => Float): Oklab = Colour.set_packed_float(color, 42, f(Oklab.l(color)))
    def a: Float =
      val x: Long = color
      if x < 0 then Float.NaN else Colour.bits_to_float((x >>> 21).toInt & 0x1FFFFF)
    def b: Float =
      val x: Long = color
      if x < 0 then Float.NaN else Colour.bits_to_float((x & 0x1FFFFF).toInt)
    def c: Float =
      val x: Long = color
      if x < 0 then Float.NaN else (Colour.bits_to_float((x & 0x1FFFFF).toInt).sq + Colour.bits_to_float((x >>> 21).toInt & 0x1FFFFF).sq).sqrt.toFloat
    def h: Float =
      val x: Long = color
      if x < 0 then Float.NaN else java.lang.Math.atan2(Colour.bits_to_float((x & 0x1FFFFF).toInt), Colour.bits_to_float((x >>> 21).toInt & 0x1FFFFF)).toFloat
    def *(x: Float): Oklab = Colour.set_packed_float(color, 42, Oklab.l(color) * x)
    inline def *(x: Double): Oklab = Oklab.*(color)(x.toFloat)
    inline def /(x: Float): Oklab = Oklab.*(color)(1f/x)
    inline def /(x: Double): Oklab = Oklab.*(color)((1.0/x).toFloat)
    inline def +(other: Oklab): Oklab = Oklab.blend(color, 1.0)(other, 1.0)

    inline def rgbFn[A](inline rgbf: (Float, Float, Float) => A): A =
      val x: Long = color
      var r, g, b = Float.NaN
      if x >= 0 then
        val L = Colour.bits_to_float((x >>> 42).toInt)
        val A = Colour.bits_to_float((x >>> 21).toInt & 0x1FFFFF)
        val B = Colour.bits_to_float((x & 0x1FFFFF).toInt)
        val l = (L  +  A * 0.3963377774f  +  B * 0.2158037573f).cube.toFloat
        val m = (L  -  A * 0.1055613458f  -  B * 0.0638541728f).cube.toFloat
        val s = (L  -  A * 0.0894841775f  -  B * 1.2914855480f).cube.toFloat
        r  =  +4.0767416621f * l  -  3.3077115913f * m  +  0.2309699292f * s
        g  =  -1.2684380046f * l  +  2.6097574011f * m  -  0.3413193965f * s
        b  =  -0.0041960863f * l  -  0.7034186147f * m  +  1.7076147010f * s
      rgbf(r, g, b)

    def rgb: Rgb = Oklab.rgbFn(color)((r, g, b) => Rgb.F(r, g, b))
    def srgb: Rgb = Oklab.rgbFn(color)((r, g, b) => Rgb.D(r pow 1/2.2, g pow 1/2.2, b pow 1/2.2))

    /** The same colour as linear extended RGB, nothing clamped: overbright and out-of-gamut channels stay as they are. */
    def ergb: Ergb = Oklab.rgbFn(color)((r, g, b) => Ergb(r, g, b))

    def pr: String =
      Colour.packed_float_fn(color): (l, a, b) =>
        f"Oklab[$l%.3f $a%.3f $b%.3f]"
  }

  given Translucent[Oklab, Long] with {}

  given Sayable[Oklab] = (x, m, _) => m += x.pr

/*
################
## GENERATORS ##
################

// The named colours above: every Rgb constant through Oklab.sRGB, with kse.maths on the classpath.
import kse.maths.colours.*
val names = Rgb.getClass.getDeclaredMethods.filter(m => m.getParameterCount == 0 && m.getReturnType == classOf[Int] && m.getName.head.isUpper).map(_.getName).sorted
for n <- names do
  val c = Oklab.sRGB(Rgb.wrap(Rgb.getClass.getMethod(n).invoke(Rgb).asInstanceOf[Int]))
  println(f"  final val $n: Oklab = 0x${c.unwrap}%016XL")
*/
}

// `f * oklab` lives in OverloadedExtensions.scala with Float's other left-hand operators: a top-level
// `*` on Float here would hide every one of them wherever both packages are wildcard-imported.
