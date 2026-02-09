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
    inline def unwrap: Int = color
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

  def blend(c1: Oklab, w1: Float)(c2: Oklab, w2: Float): Oklab =
    val x1: Long = c1
    val x2: Long = c2
    if x1 < 0 || x2 < 0 then -1L
    else
      val l1 = Colour.bits_to_float((x1 >>> 42).toInt)
      val l2 = Colour.bits_to_float((x2 >>> 42).toInt)
      val l = l1*w1 + l2*w2
      if l >= 0 then
        val d = 1.0f / (if w1*w2 < 0 then math.max(w1.abs, w2.abs) else (w1 + w2).abs)
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
      val h1 = math.atan2(b1, a1).toFloat
      val h2 = { val v = math.atan2(b2, a2); if v < h1 then v + NumericConstants.TwoPi else v }.toFloat
      Oklab.lch(l1*p + l2*q, c1*p + c2*q, h1*p + h2*q)

  extension (color: Oklab) {
    inline def unwrap: Long = color
    def l: Float =
      val x: Long = color
      if x < 0 then Float.NaN else Colour.bits_to_float((x >>> 42).toInt)
    inline def lOp(inline f: Float => Float): Oklab =
      val x: Long = color
      val v = Colour.float_to_bits(f(Oklab.l(color)))
      if v < 0 then -1L else (x & 0x3FFFFFFFFFFL) | (v.toLong << 42)
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
    def *(x: Float): Oklab =
      val y: Long = color
      if y < 0 then y
      else
        val l = Colour.float_to_bits(Colour.bits_to_float((y >>> 42).toInt) * x)
        if l < 0 then -1L else (y & 0x3FFFFFFFFFFL) | (l.toLong << 42)
    inline def /(x: Float): Oklab = Oklab.*(color)(1f/x)
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

    def pr: String =
      Colour.packed_float_fn(color): (l, a, b) =>
        f"Oklab[$l%.3f $a%.3f $b%.3f]"
  }
}

extension (f: Float)
  inline def *(color: Oklab): Oklab = Oklab.*(color)(f)
