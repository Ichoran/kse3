// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.maths


import java.lang.{Math => jm}

import scala.annotation.targetName

import kse.basics.{given, _}


// Little fixed-size matrices, all opaque types over Array[Float] or Array[Double] in
// column-major order: MatRC element (r, c) is at index r + R*c.  Companion `apply`
// methods take elements in row-major (reading) order.  Transposition is a zero-copy
// type relabeling: `.T` on a MatRC gives a MatCRt viewing the same array; the t-forms
// have a deliberately small API--mostly they exist to be multiplication arguments
// (so that e.g. `a.T * b` and `a * b.T` never copy).  Row vectors work the same way:
// `v.T` on Vc / Vec2D / Vec3D / Vec3F gives a t-form usable for row-times-matrix,
// dot (`v.T * u`), and outer (`v * u.T`) products.
//
// Xform2/3 are homogeneous (affine) transforms storing just the 6 or 12 meaningful
// elements: a linear part (stored like the corresponding square matrix) plus a
// translation column.  `x(v)` transforms a point, `x.dir(v)` a direction.

private def mmD(a: Array[Double], at: Boolean, m: Int, k: Int, b: Array[Double], bt: Boolean, n: Int): Array[Double] =
  val res = new Array[Double](m*n)
  var c = 0
  while c < n do
    var r = 0
    while r < m do
      var s = 0.0
      var j = 0
      while j < k do
        s += (if at then a(j + k*r) else a(r + m*j)) * (if bt then b(c + n*j) else b(j + k*c))
        j += 1
      res(r + m*c) = s
      r += 1
    c += 1
  res

private def mmF(a: Array[Float], at: Boolean, m: Int, k: Int, b: Array[Float], bt: Boolean, n: Int): Array[Float] =
  val res = new Array[Float](m*n)
  var c = 0
  while c < n do
    var r = 0
    while r < m do
      var s = 0f
      var j = 0
      while j < k do
        s += (if at then a(j + k*r) else a(r + m*j)) * (if bt then b(c + n*j) else b(j + k*c))
        j += 1
      res(r + m*c) = s
      r += 1
    c += 1
  res

private def zipD(a: Array[Double], b: Array[Double], sub: Boolean): Array[Double] =
  val res = new Array[Double](a.length)
  var i = 0
  while i < a.length do
    res(i) = if sub then a(i) - b(i) else a(i) + b(i)
    i += 1
  res

private def zipF(a: Array[Float], b: Array[Float], sub: Boolean): Array[Float] =
  val res = new Array[Float](a.length)
  var i = 0
  while i < a.length do
    res(i) = if sub then a(i) - b(i) else a(i) + b(i)
    i += 1
  res

private def sclD(a: Array[Double], f: Double): Array[Double] =
  val res = new Array[Double](a.length)
  var i = 0
  while i < a.length do
    res(i) = a(i)*f
    i += 1
  res

private def sclF(a: Array[Float], f: Float): Array[Float] =
  val res = new Array[Float](a.length)
  var i = 0
  while i < a.length do
    res(i) = a(i)*f
    i += 1
  res

private def eqD(a: Array[Double], b: Array[Double]): Boolean =
  var i = 0
  while i < a.length do
    if a(i) != b(i) then return false
    i += 1
  true

private def eqF(a: Array[Float], b: Array[Float]): Boolean =
  var i = 0
  while i < a.length do
    if a(i) != b(i) then return false
    i += 1
  true

private def det3D(p: Array[Double]): Double =
  p(0)*(p(4)*p(8) - p(7)*p(5)) - p(3)*(p(1)*p(8) - p(7)*p(2)) + p(6)*(p(1)*p(5) - p(4)*p(2))

private def det3F(p: Array[Float]): Double =
  val p0 = p(0).toDouble; val p1 = p(1).toDouble; val p2 = p(2).toDouble
  val p3 = p(3).toDouble; val p4 = p(4).toDouble; val p5 = p(5).toDouble
  val p6 = p(6).toDouble; val p7 = p(7).toDouble; val p8 = p(8).toDouble
  p0*(p4*p8 - p7*p5) - p3*(p1*p8 - p7*p2) + p6*(p1*p5 - p4*p2)

// Writes the inverse of the leading 3x3 of p (column-major) into q(0..8)
private def inv3D(p: Array[Double], q: Array[Double]): Unit =
  val d = 1.0/det3D(p)
  q(0) = (p(4)*p(8) - p(7)*p(5))*d
  q(1) = (p(7)*p(2) - p(1)*p(8))*d
  q(2) = (p(1)*p(5) - p(4)*p(2))*d
  q(3) = (p(6)*p(5) - p(3)*p(8))*d
  q(4) = (p(0)*p(8) - p(6)*p(2))*d
  q(5) = (p(3)*p(2) - p(0)*p(5))*d
  q(6) = (p(3)*p(7) - p(6)*p(4))*d
  q(7) = (p(6)*p(1) - p(0)*p(7))*d
  q(8) = (p(0)*p(4) - p(3)*p(1))*d

private def inv3F(p: Array[Float], q: Array[Float]): Unit =
  val t = new Array[Double](9)
  val s = new Array[Double](9)
  var i = 0
  while i < 9 do
    t(i) = p(i)
    i += 1
  inv3D(t, s)
  i = 0
  while i < 9 do
    q(i) = s(i).toFloat
    i += 1

// Column-major 3x3 Rodrigues rotation matrix about (possibly unnormalized) axis
private def rodrigues(ax: Double, ay: Double, az: Double, angle: Double): Array[Double] =
  val l = jm.sqrt(ax*ax + ay*ay + az*az)
  val il = if l == 0 then 0.0 else 1.0/l
  val x = ax*il
  val y = ay*il
  val z = az*il
  val c = jm.cos(angle)
  val s = jm.sin(angle)
  val u = 1 - c
  Array(
    c + x*x*u,   y*x*u + z*s, z*x*u - y*s,
    x*y*u - z*s, c + y*y*u,   z*y*u + x*s,
    x*z*u + y*s, y*z*u - x*s, c + z*z*u
  )

private def prD(rows: Int, cols: Int)(get: (Int, Int) => Double): String =
  MkStr: sb =>
    sb += '['
    var r = 0
    while r < rows do
      if r > 0 then sb += ' '
      sb += '['
      var c = 0
      while c < cols do
        if c > 0 then sb += ' '
        sb += get(r, c)
        c += 1
      sb += ']'
      r += 1
    sb += ']'

private def prF(rows: Int, cols: Int)(get: (Int, Int) => Float): String =
  MkStr: sb =>
    sb += '['
    var r = 0
    while r < rows do
      if r > 0 then sb += ' '
      sb += '['
      var c = 0
      while c < cols do
        if c > 0 then sb += ' '
        sb += get(r, c)
        c += 1
      sb += ']'
      r += 1
    sb += ']'

private def prFmt(rows: Int, cols: Int)(get: (Int, Int) => String): String =
  MkStr: sb =>
    sb += '['
    var r = 0
    while r < rows do
      if r > 0 then sb += ' '
      sb += '['
      var c = 0
      while c < cols do
        if c > 0 then sb += ' '
        sb += get(r, c)
        c += 1
      sb += ']'
      r += 1
    sb += ']'


//////////////////////////////
/// Transposed row vectors ///
//////////////////////////////

// Over Long rather than Vc so that Vc remains a distinct type inside this object
// (otherwise accessor delegation to Vc's extensions would resolve back to itself)
opaque type Vct = Long
object Vct {
  inline def wrap(v: kse.maths.Vc): kse.maths.Vct = v.unwrap

  extension (vt: Vct) {
    inline def unwrap: kse.maths.Vc = Vc.wrap(vt)
    inline def x: Float = Vc.wrap(vt).x
    inline def y: Float = Vc.wrap(vt).y
    inline def T: kse.maths.Vc = Vc.wrap(vt)
  }

  extension (vt: kse.maths.Vct) {
    @targetName("Vct_mul_Mat22F")
    def *(m: kse.maths.Mat22F): kse.maths.Vct =
      val p = m.unwrap
      Vct.wrap(Vc.F(vt.x*p(0) + vt.y*p(1), vt.x*p(2) + vt.y*p(3)))
    @targetName("Vct_mul_Mat23F")
    def *(m: kse.maths.Mat23F): kse.maths.Vec3Ft =
      val p = m.unwrap
      Vec3Ft.wrap(Vec3F(vt.x*p(0) + vt.y*p(1), vt.x*p(2) + vt.y*p(3), vt.x*p(4) + vt.y*p(5)))
    def *(v: kse.maths.Vc): Double = vt.x*v.x + vt.y*v.y
  }
}

opaque type Vec3Ft = Vec3F
object Vec3Ft {
  inline def wrap(v: Vec3F): kse.maths.Vec3Ft = v

  extension (vt: Vec3Ft) {
    inline def unwrap: Vec3F = vt
    inline def x: Float = (vt: Vec3F).x
    inline def y: Float = (vt: Vec3F).y
    inline def z: Float = (vt: Vec3F).z
    inline def T: Vec3F = vt
  }

  extension (vt: kse.maths.Vec3Ft) {
    @targetName("Vec3Ft_mul_Mat32F")
    def *(m: kse.maths.Mat32F): kse.maths.Vct =
      val p = m.unwrap
      Vct.wrap(Vc.F(vt.x*p(0) + vt.y*p(1) + vt.z*p(2), vt.x*p(3) + vt.y*p(4) + vt.z*p(5)))
    @targetName("Vec3Ft_mul_Mat33F")
    def *(m: kse.maths.Mat33F): kse.maths.Vec3Ft =
      val p = m.unwrap
      Vec3Ft.wrap(Vec3F(
        vt.x*p(0) + vt.y*p(1) + vt.z*p(2),
        vt.x*p(3) + vt.y*p(4) + vt.z*p(5),
        vt.x*p(6) + vt.y*p(7) + vt.z*p(8)
      ))
    def *(v: Vec3F): Double = vt.x*v.x + vt.y*v.y + vt.z*v.z
  }
}

opaque type Vec2Dt = Vec2D
object Vec2Dt {
  inline def wrap(v: Vec2D): kse.maths.Vec2Dt = v

  extension (vt: Vec2Dt) {
    inline def unwrap: Vec2D = vt
    inline def x: Double = (vt: Vec2D).x
    inline def y: Double = (vt: Vec2D).y
    inline def T: Vec2D = vt
  }

  extension (vt: kse.maths.Vec2Dt) {
    @targetName("Vec2Dt_mul_Mat22D")
    def *(m: kse.maths.Mat22D): kse.maths.Vec2Dt =
      val p = m.unwrap
      Vec2Dt.wrap(Vec2D(vt.x*p(0) + vt.y*p(1), vt.x*p(2) + vt.y*p(3)))
    @targetName("Vec2Dt_mul_Mat23D")
    def *(m: kse.maths.Mat23D): kse.maths.Vec3Dt =
      val p = m.unwrap
      Vec3Dt.wrap(Vec3D(vt.x*p(0) + vt.y*p(1), vt.x*p(2) + vt.y*p(3), vt.x*p(4) + vt.y*p(5)))
    def *(v: Vec2D): Double = vt.x*v.x + vt.y*v.y
  }
}

opaque type Vec3Dt = Vec3D
object Vec3Dt {
  inline def wrap(v: Vec3D): kse.maths.Vec3Dt = v

  extension (vt: Vec3Dt) {
    inline def unwrap: Vec3D = vt
    inline def x: Double = (vt: Vec3D).x
    inline def y: Double = (vt: Vec3D).y
    inline def z: Double = (vt: Vec3D).z
    inline def T: Vec3D = vt
  }

  extension (vt: kse.maths.Vec3Dt) {
    @targetName("Vec3Dt_mul_Mat32D")
    def *(m: kse.maths.Mat32D): kse.maths.Vec2Dt =
      val p = m.unwrap
      Vec2Dt.wrap(Vec2D(vt.x*p(0) + vt.y*p(1) + vt.z*p(2), vt.x*p(3) + vt.y*p(4) + vt.z*p(5)))
    @targetName("Vec3Dt_mul_Mat33D")
    def *(m: kse.maths.Mat33D): kse.maths.Vec3Dt =
      val p = m.unwrap
      Vec3Dt.wrap(Vec3D(
        vt.x*p(0) + vt.y*p(1) + vt.z*p(2),
        vt.x*p(3) + vt.y*p(4) + vt.z*p(5),
        vt.x*p(6) + vt.y*p(7) + vt.z*p(8)
      ))
    def *(v: Vec3D): Double = vt.x*v.x + vt.y*v.y + vt.z*v.z
  }
}


//////////////////////
/// Float matrices ///
//////////////////////

opaque type Mat22F = Array[Float]
object Mat22F {
  def apply(m00: Float, m01: Float)(m10: Float, m11: Float): kse.maths.Mat22F = Array(m00, m10, m01, m11)
  def D(m00: Double, m01: Double)(m10: Double, m11: Double): kse.maths.Mat22F = Array(m00.toFloat, m10.toFloat, m01.toFloat, m11.toFloat)
  inline def identity: kse.maths.Mat22F = Array(1f, 0f, 0f, 1f)
  inline def wrap(a: Array[Float]): kse.maths.Mat22F = a

  extension (m: Mat22F) {
    inline def unwrap: Array[Float] = m
    inline def apply(r: 0 | 1, c: 0 | 1): Float = m(r + 2*c)
    inline def T: kse.maths.Mat22Ft = Mat22Ft.wrap(m)
  }

  extension (m: kse.maths.Mat22F) {
    def +(n: kse.maths.Mat22F): kse.maths.Mat22F = wrap(zipF(m.unwrap, n.unwrap, false))
    def -(n: kse.maths.Mat22F): kse.maths.Mat22F = wrap(zipF(m.unwrap, n.unwrap, true))
    def unary_- : kse.maths.Mat22F = wrap(sclF(m.unwrap, -1f))
    def *(f: Float): kse.maths.Mat22F = wrap(sclF(m.unwrap, f))
    @targetName("Mat22F_mul_Mat22F")
    def *(n: kse.maths.Mat22F): kse.maths.Mat22F = wrap(mmF(m.unwrap, false, 2, 2, n.unwrap, false, 2))
    @targetName("Mat22F_mul_Mat23F")
    def *(n: kse.maths.Mat23F): kse.maths.Mat23F = Mat23F.wrap(mmF(m.unwrap, false, 2, 2, n.unwrap, false, 3))
    @targetName("Mat22F_mul_Mat22Ft")
    def *(n: kse.maths.Mat22Ft): kse.maths.Mat22F = wrap(mmF(m.unwrap, false, 2, 2, n.unwrap, true, 2))
    @targetName("Mat22F_mul_Mat23Ft")
    def *(n: kse.maths.Mat23Ft): kse.maths.Mat23F = Mat23F.wrap(mmF(m.unwrap, false, 2, 2, n.unwrap, true, 3))
    def *(v: kse.maths.Vc): kse.maths.Vc =
      val p = m.unwrap
      Vc.F(p(0)*v.x + p(2)*v.y, p(1)*v.x + p(3)*v.y)
    def det: Double =
      val p = m.unwrap
      p(0).toDouble*p(3) - p(2).toDouble*p(1)
    def tr: Double =
      val p = m.unwrap
      p(0).toDouble + p(3)
    def inv: kse.maths.Mat22F =
      val p = m.unwrap
      val d = 1.0/(p(0).toDouble*p(3) - p(2).toDouble*p(1))
      wrap(Array((p(3)*d).toFloat, (-p(1)*d).toFloat, (-p(2)*d).toFloat, (p(0)*d).toFloat))
    def ===(n: kse.maths.Mat22F): Boolean = eqF(m.unwrap, n.unwrap)
    def pr: String =
      val p = m.unwrap
      prF(2, 2)((r, c) => p(r + 2*c))
    def prf(fmt: String): String =
      val p = m.unwrap
      prFmt(2, 2)((r, c) => fmt.format(p(r + 2*c)))
  }
}

opaque type Mat22Ft = Array[Float]
object Mat22Ft {
  inline def wrap(a: Array[Float]): kse.maths.Mat22Ft = a

  extension (m: Mat22Ft) {
    inline def unwrap: Array[Float] = m
    inline def apply(r: 0 | 1, c: 0 | 1): Float = m(c + 2*r)
    inline def T: kse.maths.Mat22F = Mat22F.wrap(m)
  }

  extension (m: kse.maths.Mat22Ft) {
    @targetName("Mat22Ft_mul_Mat22F")
    def *(n: kse.maths.Mat22F): kse.maths.Mat22F = Mat22F.wrap(mmF(m.unwrap, true, 2, 2, n.unwrap, false, 2))
    @targetName("Mat22Ft_mul_Mat23F")
    def *(n: kse.maths.Mat23F): kse.maths.Mat23F = Mat23F.wrap(mmF(m.unwrap, true, 2, 2, n.unwrap, false, 3))
    def *(v: kse.maths.Vc): kse.maths.Vc =
      val p = m.unwrap
      Vc.F(p(0)*v.x + p(1)*v.y, p(2)*v.x + p(3)*v.y)
  }
}

opaque type Mat23F = Array[Float]
object Mat23F {
  def apply(m00: Float, m01: Float, m02: Float)(m10: Float, m11: Float, m12: Float): kse.maths.Mat23F =
    Array(m00, m10, m01, m11, m02, m12)
  def D(m00: Double, m01: Double, m02: Double)(m10: Double, m11: Double, m12: Double): kse.maths.Mat23F =
    Array(m00.toFloat, m10.toFloat, m01.toFloat, m11.toFloat, m02.toFloat, m12.toFloat)
  inline def wrap(a: Array[Float]): kse.maths.Mat23F = a

  extension (m: Mat23F) {
    inline def unwrap: Array[Float] = m
    inline def apply(r: 0 | 1, c: 0 | 1 | 2): Float = m(r + 2*c)
    inline def T: kse.maths.Mat32Ft = Mat32Ft.wrap(m)
  }

  extension (m: kse.maths.Mat23F) {
    def +(n: kse.maths.Mat23F): kse.maths.Mat23F = wrap(zipF(m.unwrap, n.unwrap, false))
    def -(n: kse.maths.Mat23F): kse.maths.Mat23F = wrap(zipF(m.unwrap, n.unwrap, true))
    def unary_- : kse.maths.Mat23F = wrap(sclF(m.unwrap, -1f))
    def *(f: Float): kse.maths.Mat23F = wrap(sclF(m.unwrap, f))
    @targetName("Mat23F_mul_Mat32F")
    def *(n: kse.maths.Mat32F): kse.maths.Mat22F = Mat22F.wrap(mmF(m.unwrap, false, 2, 3, n.unwrap, false, 2))
    @targetName("Mat23F_mul_Mat33F")
    def *(n: kse.maths.Mat33F): kse.maths.Mat23F = wrap(mmF(m.unwrap, false, 2, 3, n.unwrap, false, 3))
    @targetName("Mat23F_mul_Mat32Ft")
    def *(n: kse.maths.Mat32Ft): kse.maths.Mat22F = Mat22F.wrap(mmF(m.unwrap, false, 2, 3, n.unwrap, true, 2))
    @targetName("Mat23F_mul_Mat33Ft")
    def *(n: kse.maths.Mat33Ft): kse.maths.Mat23F = wrap(mmF(m.unwrap, false, 2, 3, n.unwrap, true, 3))
    def *(v: Vec3F): kse.maths.Vc =
      val p = m.unwrap
      Vc.F(p(0)*v.x + p(2)*v.y + p(4)*v.z, p(1)*v.x + p(3)*v.y + p(5)*v.z)
    def ===(n: kse.maths.Mat23F): Boolean = eqF(m.unwrap, n.unwrap)
    def pr: String =
      val p = m.unwrap
      prF(2, 3)((r, c) => p(r + 2*c))
    def prf(fmt: String): String =
      val p = m.unwrap
      prFmt(2, 3)((r, c) => fmt.format(p(r + 2*c)))
  }
}

opaque type Mat23Ft = Array[Float]
object Mat23Ft {
  inline def wrap(a: Array[Float]): kse.maths.Mat23Ft = a

  extension (m: Mat23Ft) {
    inline def unwrap: Array[Float] = m
    inline def apply(r: 0 | 1, c: 0 | 1 | 2): Float = m(c + 3*r)
    inline def T: kse.maths.Mat32F = Mat32F.wrap(m)
  }

  extension (m: kse.maths.Mat23Ft) {
    @targetName("Mat23Ft_mul_Mat32F")
    def *(n: kse.maths.Mat32F): kse.maths.Mat22F = Mat22F.wrap(mmF(m.unwrap, true, 2, 3, n.unwrap, false, 2))
    @targetName("Mat23Ft_mul_Mat33F")
    def *(n: kse.maths.Mat33F): kse.maths.Mat23F = Mat23F.wrap(mmF(m.unwrap, true, 2, 3, n.unwrap, false, 3))
    def *(v: Vec3F): kse.maths.Vc =
      val p = m.unwrap
      Vc.F(p(0)*v.x + p(1)*v.y + p(2)*v.z, p(3)*v.x + p(4)*v.y + p(5)*v.z)
  }
}

opaque type Mat32F = Array[Float]
object Mat32F {
  def apply(m00: Float, m01: Float)(m10: Float, m11: Float)(m20: Float, m21: Float): kse.maths.Mat32F =
    Array(m00, m10, m20, m01, m11, m21)
  def D(m00: Double, m01: Double)(m10: Double, m11: Double)(m20: Double, m21: Double): kse.maths.Mat32F =
    Array(m00.toFloat, m10.toFloat, m20.toFloat, m01.toFloat, m11.toFloat, m21.toFloat)
  inline def wrap(a: Array[Float]): kse.maths.Mat32F = a

  extension (m: Mat32F) {
    inline def unwrap: Array[Float] = m
    inline def apply(r: 0 | 1 | 2, c: 0 | 1): Float = m(r + 3*c)
    inline def T: kse.maths.Mat23Ft = Mat23Ft.wrap(m)
  }

  extension (m: kse.maths.Mat32F) {
    def +(n: kse.maths.Mat32F): kse.maths.Mat32F = wrap(zipF(m.unwrap, n.unwrap, false))
    def -(n: kse.maths.Mat32F): kse.maths.Mat32F = wrap(zipF(m.unwrap, n.unwrap, true))
    def unary_- : kse.maths.Mat32F = wrap(sclF(m.unwrap, -1f))
    def *(f: Float): kse.maths.Mat32F = wrap(sclF(m.unwrap, f))
    @targetName("Mat32F_mul_Mat22F")
    def *(n: kse.maths.Mat22F): kse.maths.Mat32F = wrap(mmF(m.unwrap, false, 3, 2, n.unwrap, false, 2))
    @targetName("Mat32F_mul_Mat23F")
    def *(n: kse.maths.Mat23F): kse.maths.Mat33F = Mat33F.wrap(mmF(m.unwrap, false, 3, 2, n.unwrap, false, 3))
    @targetName("Mat32F_mul_Mat22Ft")
    def *(n: kse.maths.Mat22Ft): kse.maths.Mat32F = wrap(mmF(m.unwrap, false, 3, 2, n.unwrap, true, 2))
    @targetName("Mat32F_mul_Mat23Ft")
    def *(n: kse.maths.Mat23Ft): kse.maths.Mat33F = Mat33F.wrap(mmF(m.unwrap, false, 3, 2, n.unwrap, true, 3))
    def *(v: kse.maths.Vc): Vec3F =
      val p = m.unwrap
      Vec3F(p(0)*v.x + p(3)*v.y, p(1)*v.x + p(4)*v.y, p(2)*v.x + p(5)*v.y)
    def ===(n: kse.maths.Mat32F): Boolean = eqF(m.unwrap, n.unwrap)
    def pr: String =
      val p = m.unwrap
      prF(3, 2)((r, c) => p(r + 3*c))
    def prf(fmt: String): String =
      val p = m.unwrap
      prFmt(3, 2)((r, c) => fmt.format(p(r + 3*c)))
  }
}

opaque type Mat32Ft = Array[Float]
object Mat32Ft {
  inline def wrap(a: Array[Float]): kse.maths.Mat32Ft = a

  extension (m: Mat32Ft) {
    inline def unwrap: Array[Float] = m
    inline def apply(r: 0 | 1 | 2, c: 0 | 1): Float = m(c + 2*r)
    inline def T: kse.maths.Mat23F = Mat23F.wrap(m)
  }

  extension (m: kse.maths.Mat32Ft) {
    @targetName("Mat32Ft_mul_Mat22F")
    def *(n: kse.maths.Mat22F): kse.maths.Mat32F = Mat32F.wrap(mmF(m.unwrap, true, 3, 2, n.unwrap, false, 2))
    @targetName("Mat32Ft_mul_Mat23F")
    def *(n: kse.maths.Mat23F): kse.maths.Mat33F = Mat33F.wrap(mmF(m.unwrap, true, 3, 2, n.unwrap, false, 3))
    def *(v: kse.maths.Vc): Vec3F =
      val p = m.unwrap
      Vec3F(p(0)*v.x + p(1)*v.y, p(2)*v.x + p(3)*v.y, p(4)*v.x + p(5)*v.y)
  }
}

opaque type Mat33F = Array[Float]
object Mat33F {
  def apply(m00: Float, m01: Float, m02: Float)(m10: Float, m11: Float, m12: Float)(m20: Float, m21: Float, m22: Float): kse.maths.Mat33F =
    Array(m00, m10, m20, m01, m11, m21, m02, m12, m22)
  def D(m00: Double, m01: Double, m02: Double)(m10: Double, m11: Double, m12: Double)(m20: Double, m21: Double, m22: Double): kse.maths.Mat33F =
    Array(m00.toFloat, m10.toFloat, m20.toFloat, m01.toFloat, m11.toFloat, m21.toFloat, m02.toFloat, m12.toFloat, m22.toFloat)
  inline def identity: kse.maths.Mat33F = Array(1f, 0f, 0f, 0f, 1f, 0f, 0f, 0f, 1f)
  inline def wrap(a: Array[Float]): kse.maths.Mat33F = a

  extension (m: Mat33F) {
    inline def unwrap: Array[Float] = m
    inline def apply(r: 0 | 1 | 2, c: 0 | 1 | 2): Float = m(r + 3*c)
    inline def T: kse.maths.Mat33Ft = Mat33Ft.wrap(m)
  }

  extension (m: kse.maths.Mat33F) {
    def +(n: kse.maths.Mat33F): kse.maths.Mat33F = wrap(zipF(m.unwrap, n.unwrap, false))
    def -(n: kse.maths.Mat33F): kse.maths.Mat33F = wrap(zipF(m.unwrap, n.unwrap, true))
    def unary_- : kse.maths.Mat33F = wrap(sclF(m.unwrap, -1f))
    def *(f: Float): kse.maths.Mat33F = wrap(sclF(m.unwrap, f))
    @targetName("Mat33F_mul_Mat32F")
    def *(n: kse.maths.Mat32F): kse.maths.Mat32F = Mat32F.wrap(mmF(m.unwrap, false, 3, 3, n.unwrap, false, 2))
    @targetName("Mat33F_mul_Mat33F")
    def *(n: kse.maths.Mat33F): kse.maths.Mat33F = wrap(mmF(m.unwrap, false, 3, 3, n.unwrap, false, 3))
    @targetName("Mat33F_mul_Mat32Ft")
    def *(n: kse.maths.Mat32Ft): kse.maths.Mat32F = Mat32F.wrap(mmF(m.unwrap, false, 3, 3, n.unwrap, true, 2))
    @targetName("Mat33F_mul_Mat33Ft")
    def *(n: kse.maths.Mat33Ft): kse.maths.Mat33F = wrap(mmF(m.unwrap, false, 3, 3, n.unwrap, true, 3))
    def *(v: Vec3F): Vec3F =
      val p = m.unwrap
      Vec3F(
        p(0)*v.x + p(3)*v.y + p(6)*v.z,
        p(1)*v.x + p(4)*v.y + p(7)*v.z,
        p(2)*v.x + p(5)*v.y + p(8)*v.z
      )
    def det: Double = det3F(m.unwrap)
    def tr: Double =
      val p = m.unwrap
      p(0).toDouble + p(4) + p(8)
    def inv: kse.maths.Mat33F =
      val q = new Array[Float](9)
      inv3F(m.unwrap, q)
      wrap(q)
    def ===(n: kse.maths.Mat33F): Boolean = eqF(m.unwrap, n.unwrap)
    def pr: String =
      val p = m.unwrap
      prF(3, 3)((r, c) => p(r + 3*c))
    def prf(fmt: String): String =
      val p = m.unwrap
      prFmt(3, 3)((r, c) => fmt.format(p(r + 3*c)))
  }
}

opaque type Mat33Ft = Array[Float]
object Mat33Ft {
  inline def wrap(a: Array[Float]): kse.maths.Mat33Ft = a

  extension (m: Mat33Ft) {
    inline def unwrap: Array[Float] = m
    inline def apply(r: 0 | 1 | 2, c: 0 | 1 | 2): Float = m(c + 3*r)
    inline def T: kse.maths.Mat33F = Mat33F.wrap(m)
  }

  extension (m: kse.maths.Mat33Ft) {
    @targetName("Mat33Ft_mul_Mat32F")
    def *(n: kse.maths.Mat32F): kse.maths.Mat32F = Mat32F.wrap(mmF(m.unwrap, true, 3, 3, n.unwrap, false, 2))
    @targetName("Mat33Ft_mul_Mat33F")
    def *(n: kse.maths.Mat33F): kse.maths.Mat33F = Mat33F.wrap(mmF(m.unwrap, true, 3, 3, n.unwrap, false, 3))
    def *(v: Vec3F): Vec3F =
      val p = m.unwrap
      Vec3F(
        p(0)*v.x + p(1)*v.y + p(2)*v.z,
        p(3)*v.x + p(4)*v.y + p(5)*v.z,
        p(6)*v.x + p(7)*v.y + p(8)*v.z
      )
  }
}


///////////////////////
/// Double matrices ///
///////////////////////

opaque type Mat22D = Array[Double]
object Mat22D {
  def apply(m00: Double, m01: Double)(m10: Double, m11: Double): kse.maths.Mat22D = Array(m00, m10, m01, m11)
  inline def identity: kse.maths.Mat22D = Array(1.0, 0, 0, 1)
  inline def wrap(a: Array[Double]): kse.maths.Mat22D = a

  extension (m: Mat22D) {
    inline def unwrap: Array[Double] = m
    inline def apply(r: 0 | 1, c: 0 | 1): Double = m(r + 2*c)
    inline def T: kse.maths.Mat22Dt = Mat22Dt.wrap(m)
  }

  extension (m: kse.maths.Mat22D) {
    def +(n: kse.maths.Mat22D): kse.maths.Mat22D = wrap(zipD(m.unwrap, n.unwrap, false))
    def -(n: kse.maths.Mat22D): kse.maths.Mat22D = wrap(zipD(m.unwrap, n.unwrap, true))
    def unary_- : kse.maths.Mat22D = wrap(sclD(m.unwrap, -1))
    def *(f: Double): kse.maths.Mat22D = wrap(sclD(m.unwrap, f))
    @targetName("Mat22D_mul_Mat22D")
    def *(n: kse.maths.Mat22D): kse.maths.Mat22D = wrap(mmD(m.unwrap, false, 2, 2, n.unwrap, false, 2))
    @targetName("Mat22D_mul_Mat23D")
    def *(n: kse.maths.Mat23D): kse.maths.Mat23D = Mat23D.wrap(mmD(m.unwrap, false, 2, 2, n.unwrap, false, 3))
    @targetName("Mat22D_mul_Mat22Dt")
    def *(n: kse.maths.Mat22Dt): kse.maths.Mat22D = wrap(mmD(m.unwrap, false, 2, 2, n.unwrap, true, 2))
    @targetName("Mat22D_mul_Mat23Dt")
    def *(n: kse.maths.Mat23Dt): kse.maths.Mat23D = Mat23D.wrap(mmD(m.unwrap, false, 2, 2, n.unwrap, true, 3))
    def *(v: Vec2D): Vec2D =
      val p = m.unwrap
      Vec2D(p(0)*v.x + p(2)*v.y, p(1)*v.x + p(3)*v.y)
    def det: Double =
      val p = m.unwrap
      p(0)*p(3) - p(2)*p(1)
    def tr: Double =
      val p = m.unwrap
      p(0) + p(3)
    def inv: kse.maths.Mat22D =
      val p = m.unwrap
      val d = 1.0/(p(0)*p(3) - p(2)*p(1))
      wrap(Array(p(3)*d, -p(1)*d, -p(2)*d, p(0)*d))
    def ===(n: kse.maths.Mat22D): Boolean = eqD(m.unwrap, n.unwrap)
    def pr: String =
      val p = m.unwrap
      prD(2, 2)((r, c) => p(r + 2*c))
    def prf(fmt: String): String =
      val p = m.unwrap
      prFmt(2, 2)((r, c) => fmt.format(p(r + 2*c)))
  }
}

opaque type Mat22Dt = Array[Double]
object Mat22Dt {
  inline def wrap(a: Array[Double]): kse.maths.Mat22Dt = a

  extension (m: Mat22Dt) {
    inline def unwrap: Array[Double] = m
    inline def apply(r: 0 | 1, c: 0 | 1): Double = m(c + 2*r)
    inline def T: kse.maths.Mat22D = Mat22D.wrap(m)
  }

  extension (m: kse.maths.Mat22Dt) {
    @targetName("Mat22Dt_mul_Mat22D")
    def *(n: kse.maths.Mat22D): kse.maths.Mat22D = Mat22D.wrap(mmD(m.unwrap, true, 2, 2, n.unwrap, false, 2))
    @targetName("Mat22Dt_mul_Mat23D")
    def *(n: kse.maths.Mat23D): kse.maths.Mat23D = Mat23D.wrap(mmD(m.unwrap, true, 2, 2, n.unwrap, false, 3))
    def *(v: Vec2D): Vec2D =
      val p = m.unwrap
      Vec2D(p(0)*v.x + p(1)*v.y, p(2)*v.x + p(3)*v.y)
  }
}

opaque type Mat23D = Array[Double]
object Mat23D {
  def apply(m00: Double, m01: Double, m02: Double)(m10: Double, m11: Double, m12: Double): kse.maths.Mat23D =
    Array(m00, m10, m01, m11, m02, m12)
  inline def wrap(a: Array[Double]): kse.maths.Mat23D = a

  extension (m: Mat23D) {
    inline def unwrap: Array[Double] = m
    inline def apply(r: 0 | 1, c: 0 | 1 | 2): Double = m(r + 2*c)
    inline def T: kse.maths.Mat32Dt = Mat32Dt.wrap(m)
  }

  extension (m: kse.maths.Mat23D) {
    def +(n: kse.maths.Mat23D): kse.maths.Mat23D = wrap(zipD(m.unwrap, n.unwrap, false))
    def -(n: kse.maths.Mat23D): kse.maths.Mat23D = wrap(zipD(m.unwrap, n.unwrap, true))
    def unary_- : kse.maths.Mat23D = wrap(sclD(m.unwrap, -1))
    def *(f: Double): kse.maths.Mat23D = wrap(sclD(m.unwrap, f))
    @targetName("Mat23D_mul_Mat32D")
    def *(n: kse.maths.Mat32D): kse.maths.Mat22D = Mat22D.wrap(mmD(m.unwrap, false, 2, 3, n.unwrap, false, 2))
    @targetName("Mat23D_mul_Mat33D")
    def *(n: kse.maths.Mat33D): kse.maths.Mat23D = wrap(mmD(m.unwrap, false, 2, 3, n.unwrap, false, 3))
    @targetName("Mat23D_mul_Mat32Dt")
    def *(n: kse.maths.Mat32Dt): kse.maths.Mat22D = Mat22D.wrap(mmD(m.unwrap, false, 2, 3, n.unwrap, true, 2))
    @targetName("Mat23D_mul_Mat33Dt")
    def *(n: kse.maths.Mat33Dt): kse.maths.Mat23D = wrap(mmD(m.unwrap, false, 2, 3, n.unwrap, true, 3))
    def *(v: Vec3D): Vec2D =
      val p = m.unwrap
      Vec2D(p(0)*v.x + p(2)*v.y + p(4)*v.z, p(1)*v.x + p(3)*v.y + p(5)*v.z)
    def ===(n: kse.maths.Mat23D): Boolean = eqD(m.unwrap, n.unwrap)
    def pr: String =
      val p = m.unwrap
      prD(2, 3)((r, c) => p(r + 2*c))
    def prf(fmt: String): String =
      val p = m.unwrap
      prFmt(2, 3)((r, c) => fmt.format(p(r + 2*c)))
  }
}

opaque type Mat23Dt = Array[Double]
object Mat23Dt {
  inline def wrap(a: Array[Double]): kse.maths.Mat23Dt = a

  extension (m: Mat23Dt) {
    inline def unwrap: Array[Double] = m
    inline def apply(r: 0 | 1, c: 0 | 1 | 2): Double = m(c + 3*r)
    inline def T: kse.maths.Mat32D = Mat32D.wrap(m)
  }

  extension (m: kse.maths.Mat23Dt) {
    @targetName("Mat23Dt_mul_Mat32D")
    def *(n: kse.maths.Mat32D): kse.maths.Mat22D = Mat22D.wrap(mmD(m.unwrap, true, 2, 3, n.unwrap, false, 2))
    @targetName("Mat23Dt_mul_Mat33D")
    def *(n: kse.maths.Mat33D): kse.maths.Mat23D = Mat23D.wrap(mmD(m.unwrap, true, 2, 3, n.unwrap, false, 3))
    def *(v: Vec3D): Vec2D =
      val p = m.unwrap
      Vec2D(p(0)*v.x + p(1)*v.y + p(2)*v.z, p(3)*v.x + p(4)*v.y + p(5)*v.z)
  }
}

opaque type Mat32D = Array[Double]
object Mat32D {
  def apply(m00: Double, m01: Double)(m10: Double, m11: Double)(m20: Double, m21: Double): kse.maths.Mat32D =
    Array(m00, m10, m20, m01, m11, m21)
  inline def wrap(a: Array[Double]): kse.maths.Mat32D = a

  extension (m: Mat32D) {
    inline def unwrap: Array[Double] = m
    inline def apply(r: 0 | 1 | 2, c: 0 | 1): Double = m(r + 3*c)
    inline def T: kse.maths.Mat23Dt = Mat23Dt.wrap(m)
  }

  extension (m: kse.maths.Mat32D) {
    def +(n: kse.maths.Mat32D): kse.maths.Mat32D = wrap(zipD(m.unwrap, n.unwrap, false))
    def -(n: kse.maths.Mat32D): kse.maths.Mat32D = wrap(zipD(m.unwrap, n.unwrap, true))
    def unary_- : kse.maths.Mat32D = wrap(sclD(m.unwrap, -1))
    def *(f: Double): kse.maths.Mat32D = wrap(sclD(m.unwrap, f))
    @targetName("Mat32D_mul_Mat22D")
    def *(n: kse.maths.Mat22D): kse.maths.Mat32D = wrap(mmD(m.unwrap, false, 3, 2, n.unwrap, false, 2))
    @targetName("Mat32D_mul_Mat23D")
    def *(n: kse.maths.Mat23D): kse.maths.Mat33D = Mat33D.wrap(mmD(m.unwrap, false, 3, 2, n.unwrap, false, 3))
    @targetName("Mat32D_mul_Mat22Dt")
    def *(n: kse.maths.Mat22Dt): kse.maths.Mat32D = wrap(mmD(m.unwrap, false, 3, 2, n.unwrap, true, 2))
    @targetName("Mat32D_mul_Mat23Dt")
    def *(n: kse.maths.Mat23Dt): kse.maths.Mat33D = Mat33D.wrap(mmD(m.unwrap, false, 3, 2, n.unwrap, true, 3))
    def *(v: Vec2D): Vec3D =
      val p = m.unwrap
      Vec3D(p(0)*v.x + p(3)*v.y, p(1)*v.x + p(4)*v.y, p(2)*v.x + p(5)*v.y)
    def ===(n: kse.maths.Mat32D): Boolean = eqD(m.unwrap, n.unwrap)
    def pr: String =
      val p = m.unwrap
      prD(3, 2)((r, c) => p(r + 3*c))
    def prf(fmt: String): String =
      val p = m.unwrap
      prFmt(3, 2)((r, c) => fmt.format(p(r + 3*c)))
  }
}

opaque type Mat32Dt = Array[Double]
object Mat32Dt {
  inline def wrap(a: Array[Double]): kse.maths.Mat32Dt = a

  extension (m: Mat32Dt) {
    inline def unwrap: Array[Double] = m
    inline def apply(r: 0 | 1 | 2, c: 0 | 1): Double = m(c + 2*r)
    inline def T: kse.maths.Mat23D = Mat23D.wrap(m)
  }

  extension (m: kse.maths.Mat32Dt) {
    @targetName("Mat32Dt_mul_Mat22D")
    def *(n: kse.maths.Mat22D): kse.maths.Mat32D = Mat32D.wrap(mmD(m.unwrap, true, 3, 2, n.unwrap, false, 2))
    @targetName("Mat32Dt_mul_Mat23D")
    def *(n: kse.maths.Mat23D): kse.maths.Mat33D = Mat33D.wrap(mmD(m.unwrap, true, 3, 2, n.unwrap, false, 3))
    def *(v: Vec2D): Vec3D =
      val p = m.unwrap
      Vec3D(p(0)*v.x + p(1)*v.y, p(2)*v.x + p(3)*v.y, p(4)*v.x + p(5)*v.y)
  }
}

opaque type Mat33D = Array[Double]
object Mat33D {
  def apply(m00: Double, m01: Double, m02: Double)(m10: Double, m11: Double, m12: Double)(m20: Double, m21: Double, m22: Double): kse.maths.Mat33D =
    Array(m00, m10, m20, m01, m11, m21, m02, m12, m22)
  inline def identity: kse.maths.Mat33D = Array(1.0, 0, 0, 0, 1, 0, 0, 0, 1)
  inline def wrap(a: Array[Double]): kse.maths.Mat33D = a

  extension (m: Mat33D) {
    inline def unwrap: Array[Double] = m
    inline def apply(r: 0 | 1 | 2, c: 0 | 1 | 2): Double = m(r + 3*c)
    inline def T: kse.maths.Mat33Dt = Mat33Dt.wrap(m)
  }

  extension (m: kse.maths.Mat33D) {
    def +(n: kse.maths.Mat33D): kse.maths.Mat33D = wrap(zipD(m.unwrap, n.unwrap, false))
    def -(n: kse.maths.Mat33D): kse.maths.Mat33D = wrap(zipD(m.unwrap, n.unwrap, true))
    def unary_- : kse.maths.Mat33D = wrap(sclD(m.unwrap, -1))
    def *(f: Double): kse.maths.Mat33D = wrap(sclD(m.unwrap, f))
    @targetName("Mat33D_mul_Mat32D")
    def *(n: kse.maths.Mat32D): kse.maths.Mat32D = Mat32D.wrap(mmD(m.unwrap, false, 3, 3, n.unwrap, false, 2))
    @targetName("Mat33D_mul_Mat33D")
    def *(n: kse.maths.Mat33D): kse.maths.Mat33D = wrap(mmD(m.unwrap, false, 3, 3, n.unwrap, false, 3))
    @targetName("Mat33D_mul_Mat32Dt")
    def *(n: kse.maths.Mat32Dt): kse.maths.Mat32D = Mat32D.wrap(mmD(m.unwrap, false, 3, 3, n.unwrap, true, 2))
    @targetName("Mat33D_mul_Mat33Dt")
    def *(n: kse.maths.Mat33Dt): kse.maths.Mat33D = wrap(mmD(m.unwrap, false, 3, 3, n.unwrap, true, 3))
    def *(v: Vec3D): Vec3D =
      val p = m.unwrap
      Vec3D(
        p(0)*v.x + p(3)*v.y + p(6)*v.z,
        p(1)*v.x + p(4)*v.y + p(7)*v.z,
        p(2)*v.x + p(5)*v.y + p(8)*v.z
      )
    def det: Double = det3D(m.unwrap)
    def tr: Double =
      val p = m.unwrap
      p(0) + p(4) + p(8)
    def inv: kse.maths.Mat33D =
      val q = new Array[Double](9)
      inv3D(m.unwrap, q)
      wrap(q)
    def ===(n: kse.maths.Mat33D): Boolean = eqD(m.unwrap, n.unwrap)
    def pr: String =
      val p = m.unwrap
      prD(3, 3)((r, c) => p(r + 3*c))
    def prf(fmt: String): String =
      val p = m.unwrap
      prFmt(3, 3)((r, c) => fmt.format(p(r + 3*c)))
  }
}

opaque type Mat33Dt = Array[Double]
object Mat33Dt {
  inline def wrap(a: Array[Double]): kse.maths.Mat33Dt = a

  extension (m: Mat33Dt) {
    inline def unwrap: Array[Double] = m
    inline def apply(r: 0 | 1 | 2, c: 0 | 1 | 2): Double = m(c + 3*r)
    inline def T: kse.maths.Mat33D = Mat33D.wrap(m)
  }

  extension (m: kse.maths.Mat33Dt) {
    @targetName("Mat33Dt_mul_Mat32D")
    def *(n: kse.maths.Mat32D): kse.maths.Mat32D = Mat32D.wrap(mmD(m.unwrap, true, 3, 3, n.unwrap, false, 2))
    @targetName("Mat33Dt_mul_Mat33D")
    def *(n: kse.maths.Mat33D): kse.maths.Mat33D = Mat33D.wrap(mmD(m.unwrap, true, 3, 3, n.unwrap, false, 3))
    def *(v: Vec3D): Vec3D =
      val p = m.unwrap
      Vec3D(
        p(0)*v.x + p(1)*v.y + p(2)*v.z,
        p(3)*v.x + p(4)*v.y + p(5)*v.z,
        p(6)*v.x + p(7)*v.y + p(8)*v.z
      )
  }
}


/////////////////////////////////////////
/// Homogeneous (affine) transforms   ///
/////////////////////////////////////////

opaque type Xform2D = Array[Double]
object Xform2D {
  def apply(m00: Double, m01: Double)(m10: Double, m11: Double)(tx: Double, ty: Double): kse.maths.Xform2D =
    Array(m00, m10, m01, m11, tx, ty)
  def apply(m: kse.maths.Mat22D, shift: Vec2D): kse.maths.Xform2D =
    val p = m.unwrap
    Array(p(0), p(1), p(2), p(3), shift.x, shift.y)
  inline def identity: kse.maths.Xform2D = Array(1.0, 0, 0, 1, 0, 0)
  def translate(dx: Double, dy: Double): kse.maths.Xform2D = Array(1.0, 0, 0, 1, dx, dy)
  inline def translate(v: Vec2D): kse.maths.Xform2D = translate(v.x, v.y)
  def rotate(angle: Double): kse.maths.Xform2D =
    val c = jm.cos(angle)
    val s = jm.sin(angle)
    Array(c, s, -s, c, 0, 0)
  def scale(s: Double): kse.maths.Xform2D = Array(s, 0, 0, s, 0, 0)
  def scale(sx: Double, sy: Double): kse.maths.Xform2D = Array(sx, 0, 0, sy, 0, 0)
  inline def wrap(a: Array[Double]): kse.maths.Xform2D = a

  extension (x: Xform2D) {
    inline def unwrap: Array[Double] = x
  }

  extension (x: kse.maths.Xform2D) {
    def apply(v: Vec2D): Vec2D =
      val p = x.unwrap
      Vec2D(p(0)*v.x + p(2)*v.y + p(4), p(1)*v.x + p(3)*v.y + p(5))
    def dir(v: Vec2D): Vec2D =
      val p = x.unwrap
      Vec2D(p(0)*v.x + p(2)*v.y, p(1)*v.x + p(3)*v.y)
    def mat: kse.maths.Mat22D =
      val p = x.unwrap
      Mat22D.wrap(Array(p(0), p(1), p(2), p(3)))
    def shift: Vec2D =
      val p = x.unwrap
      Vec2D(p(4), p(5))
    def *(that: kse.maths.Xform2D): kse.maths.Xform2D =
      val p = x.unwrap
      val q = that.unwrap
      Array(
        p(0)*q(0) + p(2)*q(1), p(1)*q(0) + p(3)*q(1),
        p(0)*q(2) + p(2)*q(3), p(1)*q(2) + p(3)*q(3),
        p(0)*q(4) + p(2)*q(5) + p(4), p(1)*q(4) + p(3)*q(5) + p(5)
      )
    def det: Double =
      val p = x.unwrap
      p(0)*p(3) - p(2)*p(1)
    def inv: kse.maths.Xform2D =
      val p = x.unwrap
      val d = 1.0/(p(0)*p(3) - p(2)*p(1))
      val i0 = p(3)*d
      val i1 = -p(1)*d
      val i2 = -p(2)*d
      val i3 = p(0)*d
      Array(i0, i1, i2, i3, -(i0*p(4) + i2*p(5)), -(i1*p(4) + i3*p(5)))
    def ===(that: kse.maths.Xform2D): Boolean = eqD(x.unwrap, that.unwrap)
    def pr: String =
      val p = x.unwrap
      prD(2, 3)((r, c) => p(r + 2*c))
    def prf(fmt: String): String =
      val p = x.unwrap
      prFmt(2, 3)((r, c) => fmt.format(p(r + 2*c)))
  }
}

opaque type Xform2F = Array[Float]
object Xform2F {
  def apply(m00: Float, m01: Float)(m10: Float, m11: Float)(tx: Float, ty: Float): kse.maths.Xform2F =
    Array(m00, m10, m01, m11, tx, ty)
  def apply(m: kse.maths.Mat22F, shift: kse.maths.Vc): kse.maths.Xform2F =
    val p = m.unwrap
    Array(p(0), p(1), p(2), p(3), shift.x, shift.y)
  inline def identity: kse.maths.Xform2F = Array(1f, 0f, 0f, 1f, 0f, 0f)
  def translate(dx: Float, dy: Float): kse.maths.Xform2F = Array(1f, 0f, 0f, 1f, dx, dy)
  inline def translate(v: kse.maths.Vc): kse.maths.Xform2F = translate(v.x, v.y)
  def rotate(angle: Float): kse.maths.Xform2F =
    val c = jm.cos(angle)
    val s = jm.sin(angle)
    Array(c.toFloat, s.toFloat, (-s).toFloat, c.toFloat, 0f, 0f)
  def scale(s: Float): kse.maths.Xform2F = Array(s, 0f, 0f, s, 0f, 0f)
  def scale(sx: Float, sy: Float): kse.maths.Xform2F = Array(sx, 0f, 0f, sy, 0f, 0f)
  inline def wrap(a: Array[Float]): kse.maths.Xform2F = a

  extension (x: Xform2F) {
    inline def unwrap: Array[Float] = x
  }

  extension (x: kse.maths.Xform2F) {
    def apply(v: kse.maths.Vc): kse.maths.Vc =
      val p = x.unwrap
      Vc.F(p(0)*v.x + p(2)*v.y + p(4), p(1)*v.x + p(3)*v.y + p(5))
    def dir(v: kse.maths.Vc): kse.maths.Vc =
      val p = x.unwrap
      Vc.F(p(0)*v.x + p(2)*v.y, p(1)*v.x + p(3)*v.y)
    def mat: kse.maths.Mat22F =
      val p = x.unwrap
      Mat22F.wrap(Array(p(0), p(1), p(2), p(3)))
    def shift: kse.maths.Vc =
      val p = x.unwrap
      Vc.F(p(4), p(5))
    def *(that: kse.maths.Xform2F): kse.maths.Xform2F =
      val p = x.unwrap
      val q = that.unwrap
      Array(
        p(0)*q(0) + p(2)*q(1), p(1)*q(0) + p(3)*q(1),
        p(0)*q(2) + p(2)*q(3), p(1)*q(2) + p(3)*q(3),
        p(0)*q(4) + p(2)*q(5) + p(4), p(1)*q(4) + p(3)*q(5) + p(5)
      )
    def det: Double =
      val p = x.unwrap
      p(0).toDouble*p(3) - p(2).toDouble*p(1)
    def inv: kse.maths.Xform2F =
      val p = x.unwrap
      val d = 1.0/(p(0).toDouble*p(3) - p(2).toDouble*p(1))
      val i0 = p(3)*d
      val i1 = -p(1)*d
      val i2 = -p(2)*d
      val i3 = p(0)*d
      Array(i0.toFloat, i1.toFloat, i2.toFloat, i3.toFloat, (-(i0*p(4) + i2*p(5))).toFloat, (-(i1*p(4) + i3*p(5))).toFloat)
    def ===(that: kse.maths.Xform2F): Boolean = eqF(x.unwrap, that.unwrap)
    def pr: String =
      val p = x.unwrap
      prF(2, 3)((r, c) => p(r + 2*c))
    def prf(fmt: String): String =
      val p = x.unwrap
      prFmt(2, 3)((r, c) => fmt.format(p(r + 2*c)))
  }
}

opaque type Xform3D = Array[Double]
object Xform3D {
  def apply(m00: Double, m01: Double, m02: Double)(m10: Double, m11: Double, m12: Double)(m20: Double, m21: Double, m22: Double)(tx: Double, ty: Double, tz: Double): kse.maths.Xform3D =
    Array(m00, m10, m20, m01, m11, m21, m02, m12, m22, tx, ty, tz)
  def apply(m: kse.maths.Mat33D, shift: Vec3D): kse.maths.Xform3D =
    val p = m.unwrap
    Array(p(0), p(1), p(2), p(3), p(4), p(5), p(6), p(7), p(8), shift.x, shift.y, shift.z)
  inline def identity: kse.maths.Xform3D = Array(1.0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0)
  def translate(dx: Double, dy: Double, dz: Double): kse.maths.Xform3D = Array(1.0, 0, 0, 0, 1, 0, 0, 0, 1, dx, dy, dz)
  inline def translate(v: Vec3D): kse.maths.Xform3D = translate(v.x, v.y, v.z)
  def rotate(axis: Vec3D, angle: Double): kse.maths.Xform3D =
    val r = rodrigues(axis.x, axis.y, axis.z, angle)
    java.util.Arrays.copyOf(r, 12)
  def scale(s: Double): kse.maths.Xform3D = Array(s, 0, 0, 0, s, 0, 0, 0, s, 0, 0, 0)
  def scale(sx: Double, sy: Double, sz: Double): kse.maths.Xform3D = Array(sx, 0, 0, 0, sy, 0, 0, 0, sz, 0, 0, 0)
  inline def wrap(a: Array[Double]): kse.maths.Xform3D = a

  extension (x: Xform3D) {
    inline def unwrap: Array[Double] = x
  }

  extension (x: kse.maths.Xform3D) {
    def apply(v: Vec3D): Vec3D =
      val p = x.unwrap
      Vec3D(
        p(0)*v.x + p(3)*v.y + p(6)*v.z + p(9),
        p(1)*v.x + p(4)*v.y + p(7)*v.z + p(10),
        p(2)*v.x + p(5)*v.y + p(8)*v.z + p(11)
      )
    def dir(v: Vec3D): Vec3D =
      val p = x.unwrap
      Vec3D(
        p(0)*v.x + p(3)*v.y + p(6)*v.z,
        p(1)*v.x + p(4)*v.y + p(7)*v.z,
        p(2)*v.x + p(5)*v.y + p(8)*v.z
      )
    def mat: kse.maths.Mat33D = Mat33D.wrap(java.util.Arrays.copyOf(x.unwrap, 9))
    def shift: Vec3D =
      val p = x.unwrap
      Vec3D(p(9), p(10), p(11))
    def *(that: kse.maths.Xform3D): kse.maths.Xform3D =
      val p = x.unwrap
      val q = that.unwrap
      val res = new Array[Double](12)
      var c = 0
      while c < 4 do
        val a = q(3*c)
        val b = q(3*c + 1)
        val d = q(3*c + 2)
        res(3*c)     = p(0)*a + p(3)*b + p(6)*d
        res(3*c + 1) = p(1)*a + p(4)*b + p(7)*d
        res(3*c + 2) = p(2)*a + p(5)*b + p(8)*d
        c += 1
      res(9) += p(9)
      res(10) += p(10)
      res(11) += p(11)
      res
    def det: Double = det3D(x.unwrap)
    def inv: kse.maths.Xform3D =
      val p = x.unwrap
      val res = new Array[Double](12)
      inv3D(p, res)
      res(9)  = -(res(0)*p(9) + res(3)*p(10) + res(6)*p(11))
      res(10) = -(res(1)*p(9) + res(4)*p(10) + res(7)*p(11))
      res(11) = -(res(2)*p(9) + res(5)*p(10) + res(8)*p(11))
      res
    def ===(that: kse.maths.Xform3D): Boolean = eqD(x.unwrap, that.unwrap)
    def pr: String =
      val p = x.unwrap
      prD(3, 4)((r, c) => p(r + 3*c))
    def prf(fmt: String): String =
      val p = x.unwrap
      prFmt(3, 4)((r, c) => fmt.format(p(r + 3*c)))
  }
}

opaque type Xform3F = Array[Float]
object Xform3F {
  def apply(m00: Float, m01: Float, m02: Float)(m10: Float, m11: Float, m12: Float)(m20: Float, m21: Float, m22: Float)(tx: Float, ty: Float, tz: Float): kse.maths.Xform3F =
    Array(m00, m10, m20, m01, m11, m21, m02, m12, m22, tx, ty, tz)
  def apply(m: kse.maths.Mat33F, shift: Vec3F): kse.maths.Xform3F =
    val p = m.unwrap
    Array(p(0), p(1), p(2), p(3), p(4), p(5), p(6), p(7), p(8), shift.x, shift.y, shift.z)
  inline def identity: kse.maths.Xform3F = Array(1f, 0f, 0f, 0f, 1f, 0f, 0f, 0f, 1f, 0f, 0f, 0f)
  def translate(dx: Float, dy: Float, dz: Float): kse.maths.Xform3F = Array(1f, 0f, 0f, 0f, 1f, 0f, 0f, 0f, 1f, dx, dy, dz)
  inline def translate(v: Vec3F): kse.maths.Xform3F = translate(v.x, v.y, v.z)
  def rotate(axis: Vec3F, angle: Float): kse.maths.Xform3F =
    val r = rodrigues(axis.x, axis.y, axis.z, angle)
    val res = new Array[Float](12)
    var i = 0
    while i < 9 do
      res(i) = r(i).toFloat
      i += 1
    res
  def scale(s: Float): kse.maths.Xform3F = Array(s, 0f, 0f, 0f, s, 0f, 0f, 0f, s, 0f, 0f, 0f)
  def scale(sx: Float, sy: Float, sz: Float): kse.maths.Xform3F = Array(sx, 0f, 0f, 0f, sy, 0f, 0f, 0f, sz, 0f, 0f, 0f)
  inline def wrap(a: Array[Float]): kse.maths.Xform3F = a

  extension (x: Xform3F) {
    inline def unwrap: Array[Float] = x
  }

  extension (x: kse.maths.Xform3F) {
    def apply(v: Vec3F): Vec3F =
      val p = x.unwrap
      Vec3F(
        p(0)*v.x + p(3)*v.y + p(6)*v.z + p(9),
        p(1)*v.x + p(4)*v.y + p(7)*v.z + p(10),
        p(2)*v.x + p(5)*v.y + p(8)*v.z + p(11)
      )
    def dir(v: Vec3F): Vec3F =
      val p = x.unwrap
      Vec3F(
        p(0)*v.x + p(3)*v.y + p(6)*v.z,
        p(1)*v.x + p(4)*v.y + p(7)*v.z,
        p(2)*v.x + p(5)*v.y + p(8)*v.z
      )
    def mat: kse.maths.Mat33F = Mat33F.wrap(java.util.Arrays.copyOf(x.unwrap, 9))
    def shift: Vec3F =
      val p = x.unwrap
      Vec3F(p(9), p(10), p(11))
    def *(that: kse.maths.Xform3F): kse.maths.Xform3F =
      val p = x.unwrap
      val q = that.unwrap
      val res = new Array[Float](12)
      var c = 0
      while c < 4 do
        val a = q(3*c)
        val b = q(3*c + 1)
        val d = q(3*c + 2)
        res(3*c)     = p(0)*a + p(3)*b + p(6)*d
        res(3*c + 1) = p(1)*a + p(4)*b + p(7)*d
        res(3*c + 2) = p(2)*a + p(5)*b + p(8)*d
        c += 1
      res(9) += p(9)
      res(10) += p(10)
      res(11) += p(11)
      res
    def det: Double = det3F(x.unwrap)
    def inv: kse.maths.Xform3F =
      val p = x.unwrap
      val res = new Array[Float](12)
      inv3F(p, res)
      res(9)  = (-(res(0).toDouble*p(9) + res(3).toDouble*p(10) + res(6).toDouble*p(11))).toFloat
      res(10) = (-(res(1).toDouble*p(9) + res(4).toDouble*p(10) + res(7).toDouble*p(11))).toFloat
      res(11) = (-(res(2).toDouble*p(9) + res(5).toDouble*p(10) + res(8).toDouble*p(11))).toFloat
      res
    def ===(that: kse.maths.Xform3F): Boolean = eqF(x.unwrap, that.unwrap)
    def pr: String =
      val p = x.unwrap
      prF(3, 4)((r, c) => p(r + 3*c))
    def prf(fmt: String): String =
      val p = x.unwrap
      prFmt(3, 4)((r, c) => fmt.format(p(r + 3*c)))
  }
}
