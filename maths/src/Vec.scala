// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.maths


import java.lang.{Math => jm}

import scala.annotation.targetName

import kse.basics.{given, _}


/** A 2D vector of `Double`s, with the same method vocabulary as `Vc` but at full precision.
  * Construct with `x ~> y`; extend into 3D with `x ~> y ~> z`.
  */
final case class Vec2D(x: Double, y: Double) {
  inline def xTo(value: Double): Vec2D = Vec2D(value, y)
  inline def xOp(inline f: Double => Double): Vec2D = Vec2D(f(x), y)

  inline def yTo(value: Double): Vec2D = Vec2D(x, value)
  inline def yOp(inline f: Double => Double): Vec2D = Vec2D(x, f(y))

  inline def isZero: Boolean = x == 0 && y == 0
  inline def isFinite: Boolean = x.finite && y.finite
  inline def isNaN: Boolean = x.nan || y.nan

  inline def swapped: Vec2D = Vec2D(y, x)
  inline def cw: Vec2D = Vec2D(y, -x)
  inline def ccw: Vec2D = Vec2D(-y, x)

  inline def unary_- : Vec2D = Vec2D(-x, -y)

  def rotate(angle: Double): Vec2D =
    val ca = jm.cos(angle)
    val sa = jm.sin(angle)
    Vec2D(x*ca - y*sa, y*ca + x*sa)
  inline def theta: Double = jm.atan2(y, x)

  inline def lenSq: Double = x*x + y*y
  inline def len: Double = jm.sqrt(x*x + y*y)

  inline def +(f: Double): Vec2D = Vec2D(x + f, y + f)
  inline def +(f: Double, g: Double): Vec2D = Vec2D(x + f, y + g)
  inline def +(u: Vec2D): Vec2D = Vec2D(x + u.x, y + u.y)

  inline def -(f: Double): Vec2D = Vec2D(x - f, y - f)
  inline def -(f: Double, g: Double): Vec2D = Vec2D(x - f, y - g)
  inline def -(u: Vec2D): Vec2D = Vec2D(x - u.x, y - u.y)

  inline def *(f: Double): Vec2D = Vec2D(x*f, y*f)
  inline def *(f: Double, g: Double): Double = x*f + y*g
  inline def *(u: Vec2D): Double = x*u.x + y*u.y
  inline def X(f: Double, g: Double): Double = x*g - y*f
  inline def X(u: Vec2D): Double = x*u.y - y*u.x

  inline def T: kse.maths.Vec2Dt = Vec2Dt.wrap(this)
  @targetName("outer_Vec2Dt")
  def *(ut: kse.maths.Vec2Dt): kse.maths.Mat22D =
    Mat22D.wrap(Array(x*ut.x, y*ut.x, x*ut.y, y*ut.y))
  @targetName("outer_Vec3Dt")
  def *(ut: kse.maths.Vec3Dt): kse.maths.Mat23D =
    Mat23D.wrap(Array(x*ut.x, y*ut.x, x*ut.y, y*ut.y, x*ut.z, y*ut.z))

  def proj(f: Double, g: Double): Vec2D =
    val e = (x*f + y*g)/(f*f + g*g)
    Vec2D(f*e, g*e)
  inline def proj(u: Vec2D): Vec2D = proj(u.x, u.y)

  def orth(f: Double, g: Double): Vec2D =
    val e = (x*f + y*g)/(f*f + g*g)
    Vec2D(x - f*e, y - g*e)
  inline def orth(u: Vec2D): Vec2D = orth(u.x, u.y)

  def hat: Vec2D =
    val l2 = x*x + y*y
    if jm.abs(l2 - 1) < 6e-16 then this
    else if l2 == 0 then Vec2D.zero
    else
      val il = 1.0/jm.sqrt(l2)
      Vec2D(x*il, y*il)

  def normDot(f: Double, g: Double): Double =
    (x*f + y*g) / jm.sqrt((x*x + y*y)*(f*f + g*g)) match
      case w if w < -1 => -1
      case w if w > 1  =>  1
      case w           =>  w
  inline def normDot(u: Vec2D): Double = normDot(u.x, u.y)

  def distSq(f: Double, g: Double): Double =
    val a = x - f
    val b = y - g
    a*a + b*b
  inline def distSq(u: Vec2D): Double = distSq(u.x, u.y)
  inline def dist(f: Double, g: Double): Double = jm.sqrt(distSq(f, g))
  inline def dist(u: Vec2D): Double = jm.sqrt(distSq(u.x, u.y))

  def angle(f: Double, g: Double): Double =
    jm.acos(normDot(f, g)) * jm.signum(x*g - y*f)
  inline def angle(u: Vec2D): Double = angle(u.x, u.y)

  def ===(u: Vec2D): Boolean = x == u.x && y == u.y

  def pr: String =
    MkStr: sb =>
      sb += '['
      sb += x
      sb += ' '
      sb += y
      sb += ']'

  def prf(fmt: String): String =
    MkStr: sb =>
      sb += '['
      sb += fmt.format(x)
      sb += ' '
      sb += fmt.format(y)
      sb += ']'

  inline def toVc: Vc = Vc.D(x, y)
  inline def ~>(z: Double): Vec3D = Vec3D(x, y, z)
}
object Vec2D {
  final val zero: Vec2D = Vec2D(0, 0)
  final val NaN: Vec2D = Vec2D(Double.NaN, Double.NaN)
}


/** A 3D vector of `Double`s, with the same method vocabulary as `Vc` extended to three
  * dimensions: the cross product `X` is a vector, the angle between vectors is unsigned
  * (there is no preferred orientation), and rotation takes an axis.  Construct with
  * `x ~> y ~> z`; project onto coordinate planes with `xy`, `xz`, `yz`.
  */
final case class Vec3D(x: Double, y: Double, z: Double) {
  inline def xTo(value: Double): Vec3D = Vec3D(value, y, z)
  inline def xOp(inline f: Double => Double): Vec3D = Vec3D(f(x), y, z)

  inline def yTo(value: Double): Vec3D = Vec3D(x, value, z)
  inline def yOp(inline f: Double => Double): Vec3D = Vec3D(x, f(y), z)

  inline def zTo(value: Double): Vec3D = Vec3D(x, y, value)
  inline def zOp(inline f: Double => Double): Vec3D = Vec3D(x, y, f(z))

  inline def isZero: Boolean = x == 0 && y == 0 && z == 0
  inline def isFinite: Boolean = x.finite && y.finite && z.finite
  inline def isNaN: Boolean = x.nan || y.nan || z.nan

  inline def unary_- : Vec3D = Vec3D(-x, -y, -z)

  def rotate(axis: Vec3D, angle: Double): Vec3D =
    // Rodrigues rotation about unit vector k: v c + (k X v) s + k (k*v)(1-c)
    val k = axis.hat
    val c = jm.cos(angle)
    val s = jm.sin(angle)
    val w = (k.x*x + k.y*y + k.z*z)*(1 - c)
    Vec3D(
      x*c + (k.y*z - k.z*y)*s + k.x*w,
      y*c + (k.z*x - k.x*z)*s + k.y*w,
      z*c + (k.x*y - k.y*x)*s + k.z*w
    )

  inline def lenSq: Double = x*x + y*y + z*z
  inline def len: Double = jm.sqrt(x*x + y*y + z*z)

  inline def +(f: Double): Vec3D = Vec3D(x + f, y + f, z + f)
  inline def +(f: Double, g: Double, h: Double): Vec3D = Vec3D(x + f, y + g, z + h)
  inline def +(u: Vec3D): Vec3D = Vec3D(x + u.x, y + u.y, z + u.z)

  inline def -(f: Double): Vec3D = Vec3D(x - f, y - f, z - f)
  inline def -(f: Double, g: Double, h: Double): Vec3D = Vec3D(x - f, y - g, z - h)
  inline def -(u: Vec3D): Vec3D = Vec3D(x - u.x, y - u.y, z - u.z)

  inline def *(f: Double): Vec3D = Vec3D(x*f, y*f, z*f)
  inline def *(f: Double, g: Double, h: Double): Double = x*f + y*g + z*h
  inline def *(u: Vec3D): Double = x*u.x + y*u.y + z*u.z
  inline def X(f: Double, g: Double, h: Double): Vec3D = Vec3D(y*h - z*g, z*f - x*h, x*g - y*f)
  inline def X(u: Vec3D): Vec3D = Vec3D(y*u.z - z*u.y, z*u.x - x*u.z, x*u.y - y*u.x)

  inline def T: kse.maths.Vec3Dt = Vec3Dt.wrap(this)
  @targetName("outer_Vec2Dt")
  def *(ut: kse.maths.Vec2Dt): kse.maths.Mat32D =
    Mat32D.wrap(Array(x*ut.x, y*ut.x, z*ut.x, x*ut.y, y*ut.y, z*ut.y))
  @targetName("outer_Vec3Dt")
  def *(ut: kse.maths.Vec3Dt): kse.maths.Mat33D =
    Mat33D.wrap(Array(x*ut.x, y*ut.x, z*ut.x, x*ut.y, y*ut.y, z*ut.y, x*ut.z, y*ut.z, z*ut.z))

  def proj(f: Double, g: Double, h: Double): Vec3D =
    val e = (x*f + y*g + z*h)/(f*f + g*g + h*h)
    Vec3D(f*e, g*e, h*e)
  inline def proj(u: Vec3D): Vec3D = proj(u.x, u.y, u.z)

  def orth(f: Double, g: Double, h: Double): Vec3D =
    val e = (x*f + y*g + z*h)/(f*f + g*g + h*h)
    Vec3D(x - f*e, y - g*e, z - h*e)
  inline def orth(u: Vec3D): Vec3D = orth(u.x, u.y, u.z)

  def hat: Vec3D =
    val l2 = x*x + y*y + z*z
    if jm.abs(l2 - 1) < 6e-16 then this
    else if l2 == 0 then Vec3D.zero
    else
      val il = 1.0/jm.sqrt(l2)
      Vec3D(x*il, y*il, z*il)

  def normDot(f: Double, g: Double, h: Double): Double =
    (x*f + y*g + z*h) / jm.sqrt((x*x + y*y + z*z)*(f*f + g*g + h*h)) match
      case w if w < -1 => -1
      case w if w > 1  =>  1
      case w           =>  w
  inline def normDot(u: Vec3D): Double = normDot(u.x, u.y, u.z)

  def distSq(f: Double, g: Double, h: Double): Double =
    val a = x - f
    val b = y - g
    val c = z - h
    a*a + b*b + c*c
  inline def distSq(u: Vec3D): Double = distSq(u.x, u.y, u.z)
  inline def dist(f: Double, g: Double, h: Double): Double = jm.sqrt(distSq(f, g, h))
  inline def dist(u: Vec3D): Double = jm.sqrt(distSq(u.x, u.y, u.z))

  inline def angle(f: Double, g: Double, h: Double): Double = jm.acos(normDot(f, g, h))
  inline def angle(u: Vec3D): Double = jm.acos(normDot(u.x, u.y, u.z))

  def ===(u: Vec3D): Boolean = x == u.x && y == u.y && z == u.z

  def pr: String =
    MkStr: sb =>
      sb += '['
      sb += x
      sb += ' '
      sb += y
      sb += ' '
      sb += z
      sb += ']'

  def prf(fmt: String): String =
    MkStr: sb =>
      sb += '['
      sb += fmt.format(x)
      sb += ' '
      sb += fmt.format(y)
      sb += ' '
      sb += fmt.format(z)
      sb += ']'

  inline def xy: Vec2D = Vec2D(x, y)
  inline def xz: Vec2D = Vec2D(x, z)
  inline def yz: Vec2D = Vec2D(y, z)
  inline def toVec3F: Vec3F = Vec3F.D(x, y, z)
}
object Vec3D {
  final val zero: Vec3D = Vec3D(0, 0, 0)
  final val NaN: Vec3D = Vec3D(Double.NaN, Double.NaN, Double.NaN)
}


/** A 3D vector of `Float`s, following `Vc`'s conventions: components and vector results
  * are `Float`, but distances and dot products widen to `Double` the way `Vc`'s do.
  * Construct by extending a `Vc` with `x ~> y ~> z`; project onto coordinate planes
  * with `xy`, `xz`, `yz` (which are exact `Vc`s).
  */
final case class Vec3F(x: Float, y: Float, z: Float) {
  inline def xTo(value: Float): Vec3F = Vec3F(value, y, z)
  inline def xOp(inline f: Float => Float): Vec3F = Vec3F(f(x), y, z)

  inline def yTo(value: Float): Vec3F = Vec3F(x, value, z)
  inline def yOp(inline f: Float => Float): Vec3F = Vec3F(x, f(y), z)

  inline def zTo(value: Float): Vec3F = Vec3F(x, y, value)
  inline def zOp(inline f: Float => Float): Vec3F = Vec3F(x, y, f(z))

  inline def isZero: Boolean = x == 0 && y == 0 && z == 0
  inline def isFinite: Boolean = x.finite && y.finite && z.finite
  inline def isNaN: Boolean = x.nan || y.nan || z.nan

  inline def unary_- : Vec3F = Vec3F(-x, -y, -z)

  def rotate(axis: Vec3F, angle: Float): Vec3F =
    val k = axis.hat
    val kx = k.x.toDouble
    val ky = k.y.toDouble
    val kz = k.z.toDouble
    val a = x.toDouble
    val b = y.toDouble
    val d = z.toDouble
    val c = jm.cos(angle)
    val s = jm.sin(angle)
    val w = (kx*a + ky*b + kz*d)*(1 - c)
    Vec3F.D(
      a*c + (ky*d - kz*b)*s + kx*w,
      b*c + (kz*a - kx*d)*s + ky*w,
      d*c + (kx*b - ky*a)*s + kz*w
    )

  def lenSq: Double = { val a = x.toDouble; val b = y.toDouble; val c = z.toDouble; a*a + b*b + c*c }
  inline def len: Float = jm.sqrt(lenSq).toFloat

  inline def +(f: Float): Vec3F = Vec3F(x + f, y + f, z + f)
  inline def +(f: Float, g: Float, h: Float): Vec3F = Vec3F(x + f, y + g, z + h)
  inline def +(u: Vec3F): Vec3F = Vec3F(x + u.x, y + u.y, z + u.z)

  inline def -(f: Float): Vec3F = Vec3F(x - f, y - f, z - f)
  inline def -(f: Float, g: Float, h: Float): Vec3F = Vec3F(x - f, y - g, z - h)
  inline def -(u: Vec3F): Vec3F = Vec3F(x - u.x, y - u.y, z - u.z)

  inline def *(f: Float): Vec3F = Vec3F(x*f, y*f, z*f)
  inline def *(f: Float, g: Float, h: Float): Double = x*f + y*g + z*h
  inline def *(u: Vec3F): Double = x*u.x + y*u.y + z*u.z
  inline def X(f: Float, g: Float, h: Float): Vec3F = Vec3F(y*h - z*g, z*f - x*h, x*g - y*f)
  inline def X(u: Vec3F): Vec3F = Vec3F(y*u.z - z*u.y, z*u.x - x*u.z, x*u.y - y*u.x)

  inline def T: kse.maths.Vec3Ft = Vec3Ft.wrap(this)
  @targetName("outer_Vct")
  def *(ut: kse.maths.Vct): kse.maths.Mat32F =
    Mat32F.wrap(Array(x*ut.x, y*ut.x, z*ut.x, x*ut.y, y*ut.y, z*ut.y))
  @targetName("outer_Vec3Ft")
  def *(ut: kse.maths.Vec3Ft): kse.maths.Mat33F =
    Mat33F.wrap(Array(x*ut.x, y*ut.x, z*ut.x, x*ut.y, y*ut.y, z*ut.y, x*ut.z, y*ut.z, z*ut.z))

  def proj(f: Float, g: Float, h: Float): Vec3F =
    val e = (x*f + y*g + z*h)/(f*f + g*g + h*h)
    Vec3F(f*e, g*e, h*e)
  inline def proj(u: Vec3F): Vec3F = proj(u.x, u.y, u.z)

  def orth(f: Float, g: Float, h: Float): Vec3F =
    val e = (x*f + y*g + z*h)/(f*f + g*g + h*h)
    Vec3F(x - f*e, y - g*e, z - h*e)
  inline def orth(u: Vec3F): Vec3F = orth(u.x, u.y, u.z)

  def hat: Vec3F =
    val l2 = lenSq
    if jm.abs(l2 - 1) < 3e-7f then this
    else if l2 == 0 then Vec3F.zero
    else
      val il = 1.0/jm.sqrt(l2)
      Vec3F.D(x*il, y*il, z*il)

  def normDot(f: Float, g: Float, h: Float): Double =
    (x*f + y*g + z*h) / jm.sqrt((x*x + y*y + z*z).toDouble*(f*f + g*g + h*h)) match
      case w if w < -1 => -1
      case w if w > 1  =>  1
      case w           =>  w
  inline def normDot(u: Vec3F): Double = normDot(u.x, u.y, u.z)

  def distSq(f: Float, g: Float, h: Float): Double =
    val a = (x - f).toDouble
    val b = (y - g).toDouble
    val c = (z - h).toDouble
    a*a + b*b + c*c
  inline def distSq(u: Vec3F): Double = distSq(u.x, u.y, u.z)
  inline def dist(f: Float, g: Float, h: Float): Float = jm.sqrt(distSq(f, g, h)).toFloat
  inline def dist(u: Vec3F): Float = jm.sqrt(distSq(u.x, u.y, u.z)).toFloat

  def angle(f: Float, g: Float, h: Float): Double =
    val a = x.toDouble
    val b = y.toDouble
    val c = z.toDouble
    val p = f.toDouble
    val q = g.toDouble
    val r = h.toDouble
    val d = (a*p + b*q + c*r)/jm.sqrt((a*a + b*b + c*c)*(p*p + q*q + r*r)) match
      case w if w < -1 => -1
      case w if w > 1  =>  1
      case w           =>  w
    jm.acos(d)
  inline def angle(u: Vec3F): Double = angle(u.x, u.y, u.z)

  def ===(u: Vec3F): Boolean = x == u.x && y == u.y && z == u.z

  def pr: String =
    MkStr: sb =>
      sb += '['
      sb += x
      sb += ' '
      sb += y
      sb += ' '
      sb += z
      sb += ']'

  def prf(fmt: String): String =
    MkStr: sb =>
      sb += '['
      sb += fmt.format(x)
      sb += ' '
      sb += fmt.format(y)
      sb += ' '
      sb += fmt.format(z)
      sb += ']'

  inline def xy: Vc = Vc(x, y)
  inline def xz: Vc = Vc(x, z)
  inline def yz: Vc = Vc(y, z)
  inline def toVec3D: Vec3D = Vec3D(x, y, z)
}
object Vec3F {
  inline def D(x: Double, y: Double, z: Double): Vec3F = Vec3F(x.toFloat, y.toFloat, z.toFloat)

  final val zero: Vec3F = Vec3F(0, 0, 0)
  final val NaN: Vec3F = Vec3F(Float.NaN, Float.NaN, Float.NaN)
}


// ~>(Double) making Vec2D, and ~>(Vc, Float) making Vec3F, are in Maths.scala with Float's ~>
// (top-level overloads must share a file)

extension (v: kse.maths.Vc) {
  inline def toVec2D: kse.maths.Vec2D = Vec2D(v.x, v.y)
}
