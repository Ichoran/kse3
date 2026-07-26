// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.maths


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._



class GeomTest() {
  import java.lang.{Math => jm}

  import kse.basics.testutilities.TestUtilities.{_, given}
  import kse.basics.{given, _}
  import kse.flow.{_, given}
  import kse.maths.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  def testVectors(): Unit =
    given Approximation[Double] = Approximation.OfDouble(1e-12, 1e-6, 1e-12)

    val v = 1.2 ~> 3.1
    val u = -1.5 ~> 0.7
    T ~ v                            ==== Vec2D(1.2, 3.1)      --: typed[Vec2D]
    T ~ (v === Vec2D(1.2, 3.1))      ==== true
    T ~ Vec2D.zero.isZero            ==== true
    T ~ Vec2D(-0.0, 0.0).isZero      ==== true
    T ~ v.isZero                     ==== false
    T ~ Vec2D.NaN.x.nan              ==== true
    T ~ Vec2D.NaN.y.nan              ==== true
    T ~ Vec2D.NaN.isNaN              ==== true
    T ~ Vec2D(Double.NaN, 0).isNaN   ==== true
    T ~ Vec2D(0, Double.NaN).isNaN   ==== true
    T ~ v.isNaN                      ==== false
    T ~ v.isFinite                   ==== true
    T ~ Vec2D.NaN.isFinite           ==== false
    T ~ Vec2D(1.0/0, 0).isFinite     ==== false
    T ~ Vec2D(0, -1.0/0).isFinite    ==== false
    T ~ v.xTo(5.4).x                 ==== 5.4
    T ~ v.xTo(5.4).y                 ==== 3.1
    T ~ v.xOp(_ + 1).x               ==== 2.2
    T ~ v.yTo(0.3).y                 ==== 0.3
    T ~ v.yOp(_ - 1).y               ==== 2.1
    T ~ v.swapped                    ==== Vec2D(3.1, 1.2)
    T ~ v.cw                         ==== Vec2D(3.1, -1.2)
    T ~ v.ccw                        ==== Vec2D(-3.1, 1.2)
    T ~ (-v)                         ==== Vec2D(-1.2, -3.1)
    T ~ v.rotate(0.37).x             =~~= -0.002215024364140824
    T ~ v.rotate(0.37).y             =~~= 3.3241532897366611
    T ~ v.rotate(0.37).len           =~~= v.len
    T ~ v.rotate(0.37).theta         =~~= (v.theta + 0.37)
    T ~ v.theta                      =~~= 1.2014626691212710
    T ~ v.lenSq                      =~~= 11.05
    T ~ v.len                        =~~= 3.3241540277189323
    T ~ (v + 2.0)                    ==== Vec2D(1.2 + 2.0, 3.1 + 2.0)
    T ~ (v + 2.0)                    ==== (2.0 + v)
    T ~ v.+(1, -1)                   ==== Vec2D(1.2 + 1, 3.1 - 1)
    T ~ (v + u)                      ==== Vec2D(1.2 + -1.5, 3.1 + 0.7)
    T ~ (v - 2.0)                    ==== Vec2D(1.2 - 2.0, 3.1 - 2.0)
    T ~ (v - 2.0)                    ==== -(2.0 - v)
    T ~ v.-(-1, 1)                   ==== Vec2D(1.2 + 1, 3.1 - 1)
    T ~ (v - u)                      ==== Vec2D(1.2 - -1.5, 3.1 - 0.7)
    T ~ (v * 2.0)                    ==== Vec2D(1.2 * 2.0, 3.1 * 2.0)
    T ~ (2.0 * v)                    ==== (v * 2.0)
    T ~ v.*(-1.5, 0.7)               =~~= 0.37
    T ~ (v * u)                      ==== v.*(-1.5, 0.7)
    T ~ v.X(-1.5, 0.7)               =~~= 5.49
    T ~ (v X u)                      ==== v.X(-1.5, 0.7)
    T ~ v.proj(-1.5, 0.7).x          =~~= -0.20255474452554745
    T ~ v.proj(-1.5, 0.7).y          =~~= 0.094525547445255474
    T ~ (v proj u)                   ==== v.proj(-1.5, 0.7)
    T ~ v.orth(-1.5, 0.7).x          =~~= 1.4025547445255474
    T ~ v.orth(-1.5, 0.7).y          =~~= 3.0054744525547445
    T ~ (v orth u)                   ==== v.orth(-1.5, 0.7)
    T ~ ((v proj u) + (v orth u)).x  =~~= v.x
    T ~ ((v proj u) + (v orth u)).y  =~~= v.y
    T ~ ((v proj u) X u)             =~~= 0.0
    T ~ ((v orth u) * u)             =~~= 0.0
    T ~ v.hat.x                      =~~= 0.36099410255771210
    T ~ v.hat.y                      =~~= 0.93256809827408960
    T ~ v.hat.len                    =~~= 1.0
    T ~ v.hat.hat                    ==== v.hat
    T ~ Vec2D.zero.hat               ==== Vec2D.zero
    T ~ v.normDot(-1.5, 0.7)         =~~= 0.067242724816079200
    T ~ (v normDot u)                ==== v.normDot(-1.5, 0.7)
    T ~ v.normDot(v)                 =~~= 1.0
    T ~ v.normDot(-v)                =~~= -1.0
    T ~ v.distSq(-1.5, 0.7)          =~~= 13.05
    T ~ (v distSq u)                 ==== v.distSq(-1.5, 0.7)
    T ~ v.dist(-1.5, 0.7)            =~~= 3.6124783736376886
    T ~ (v dist u)                   ==== v.dist(-1.5, 0.7)
    T ~ v.angle(-1.5, 0.7)           =~~= 1.5035028246549809
    T ~ (v angle u)                  ==== v.angle(-1.5, 0.7)
    T ~ (u angle v)                  ==== -(v angle u)
    T ~ v.pr                         ==== "[1.2 3.1]"
    T ~ v.prf("%.2e")                ==== "[1.20e+00 3.10e+00]"
    T ~ v.toVc                       ==== Vc.F(1.2f, 3.1f)
    T ~ Vc.F(1.2f, 3.1f).toVec2D     ==== Vec2D(1.2f, 3.1f)

    val w = 1.2 ~> 3.1 ~> -0.6
    val t = -1.5 ~> 0.7 ~> 2.2
    T ~ w                            ==== Vec3D(1.2, 3.1, -0.6) --: typed[Vec3D]
    T ~ (w === Vec3D(1.2, 3.1, -0.6))==== true
    T ~ Vec3D.zero.isZero            ==== true
    T ~ w.isZero                     ==== false
    T ~ Vec3D.NaN.isNaN              ==== true
    T ~ Vec3D(0, 0, Double.NaN).isNaN ==== true
    T ~ w.isNaN                      ==== false
    T ~ w.isFinite                   ==== true
    T ~ Vec3D(0, 1.0/0, 0).isFinite  ==== false
    T ~ w.xTo(5.4).x                 ==== 5.4
    T ~ w.xOp(_ + 1).x               ==== 2.2
    T ~ w.yTo(0.3).y                 ==== 0.3
    T ~ w.yOp(_ - 1).y               ==== 2.1
    T ~ w.zTo(0.3).z                 ==== 0.3
    T ~ w.zOp(_ - 1).z               ==== -1.6
    T ~ w.zTo(0.3).x                 ==== 1.2
    T ~ (-w)                         ==== Vec3D(-1.2, -3.1, 0.6)
    T ~ w.lenSq                      =~~= 11.41
    T ~ w.len                        =~~= 3.3778691508109073
    T ~ (w + 2.0)                    ==== Vec3D(1.2 + 2.0, 3.1 + 2.0, -0.6 + 2.0)
    T ~ (w + 2.0)                    ==== (2.0 + w)
    T ~ w.+(1, -1, 2)                ==== Vec3D(1.2 + 1, 3.1 - 1, -0.6 + 2)
    T ~ (w + t)                      ==== Vec3D(1.2 + -1.5, 3.1 + 0.7, -0.6 + 2.2)
    T ~ (w - 2.0)                    ==== -(2.0 - w)
    T ~ w.-(-1, 1, -2)               ==== Vec3D(1.2 + 1, 3.1 - 1, -0.6 + 2)
    T ~ (w - t)                      ==== Vec3D(1.2 - -1.5, 3.1 - 0.7, -0.6 - 2.2)
    T ~ (w * 2.0)                    ==== Vec3D(1.2 * 2.0, 3.1 * 2.0, -0.6 * 2.0)
    T ~ (2.0 * w)                    ==== (w * 2.0)
    T ~ w.*(-1.5, 0.7, 2.2)          =~~= -0.95
    T ~ (w * t)                      ==== w.*(-1.5, 0.7, 2.2)
    T ~ (w X t).x                    =~~= 7.24
    T ~ (w X t).y                    =~~= -1.74
    T ~ (w X t).z                    =~~= 5.49
    T ~ w.X(-1.5, 0.7, 2.2)          ==== (w X t)
    T ~ (t X w)                      ==== -(w X t)
    T ~ ((w X t) * w)                =~~= 0.0
    T ~ ((w X t) * t)                =~~= 0.0
    T ~ w.proj(-1.5, 0.7, 2.2).x     =~~= 0.18799472295514512
    T ~ w.proj(-1.5, 0.7, 2.2).y     =~~= -0.087730870712401055
    T ~ w.proj(-1.5, 0.7, 2.2).z     =~~= -0.27572559366754617
    T ~ (w proj t)                   ==== w.proj(-1.5, 0.7, 2.2)
    T ~ (w orth t).x                 =~~= 1.0120052770448549
    T ~ (w orth t)                   ==== w.orth(-1.5, 0.7, 2.2)
    T ~ ((w proj t) + (w orth t)).z  =~~= w.z
    T ~ ((w orth t) * t)             =~~= 0.0
    T ~ w.hat.x                      =~~= 0.35525354785040217
    T ~ w.hat.y                      =~~= 0.91773833194687227
    T ~ w.hat.z                      =~~= -0.17762677392520108
    T ~ w.hat.len                    =~~= 1.0
    T ~ w.hat.hat                    ==== w.hat
    T ~ Vec3D.zero.hat               ==== Vec3D.zero
    T ~ w.normDot(-1.5, 0.7, 2.2)    =~~= -0.10215183683356551
    T ~ (w normDot t)                ==== w.normDot(-1.5, 0.7, 2.2)
    T ~ w.normDot(w)                 =~~= 1.0
    T ~ w.distSq(-1.5, 0.7, 2.2)     =~~= 20.89
    T ~ (w distSq t)                 ==== w.distSq(-1.5, 0.7, 2.2)
    T ~ w.dist(-1.5, 0.7, 2.2)       =~~= 4.5705579528105756
    T ~ (w dist t)                   ==== w.dist(-1.5, 0.7, 2.2)
    T ~ w.angle(-1.5, 0.7, 2.2)      =~~= 1.6731266621210677
    T ~ (w angle t)                  ==== w.angle(-1.5, 0.7, 2.2)
    T ~ (t angle w)                  ==== (w angle t)
    val ax = 0.3 ~> -1.0 ~> 0.2
    T ~ w.rotate(ax, 0.83).x         =~~= 0.54910291738796916
    T ~ w.rotate(ax, 0.83).y         =~~= 3.2065544007635481
    T ~ w.rotate(ax, 0.83).z         =~~= 0.90911762773578661
    T ~ w.rotate(ax, 0.83).len       =~~= w.len
    T ~ (w.rotate(ax, 0.83) * ax.hat) =~~= (w * ax.hat)
    T ~ Vec3D(1, 0, 0).rotate(Vec3D(0, 0, 1), jm.PI/2).y =~~= 1.0
    T ~ w.pr                         ==== "[1.2 3.1 -0.6]"
    T ~ w.prf("%.2e")                ==== "[1.20e+00 3.10e+00 -6.00e-01]"
    T ~ w.xy                         ==== Vec2D(1.2, 3.1)
    T ~ w.xz                         ==== Vec2D(1.2, -0.6)
    T ~ w.yz                         ==== Vec2D(3.1, -0.6)
    T ~ w.toVec3F                    ==== Vec3F(1.2f, 3.1f, -0.6f)

    val w3 = 1.2f ~> 3.1f ~> -0.6f
    val t3 = 1.5f.unary_- ~> 0.7f ~> 2.2f
    val w3d = w3.toVec3D
    val t3d = t3.toVec3D
    T ~ w3                           ==== Vec3F(1.2f, 3.1f, -0.6f) --: typed[Vec3F]
    T ~ (w3 === Vec3F(1.2f, 3.1f, -0.6f)) ==== true
    T ~ Vec3F.D(1.2, 3.1, -0.6)      ==== w3
    T ~ Vec3F.zero.isZero            ==== true
    T ~ Vec3F.NaN.isNaN              ==== true
    T ~ Vec3F(0f, Float.NaN, 0f).isNaN ==== true
    T ~ w3.isFinite                  ==== true
    T ~ Vec3F(0f, 1f/0f, 0f).isFinite ==== false
    T ~ w3.xTo(5.4f).x               ==== 5.4f
    T ~ w3.xOp(_ + 1).x              ==== 2.2f
    T ~ w3.yTo(0.3f).y               ==== 0.3f
    T ~ w3.zOp(_ - 1).z              ==== -1.6f
    T ~ (-w3)                        ==== Vec3F(-1.2f, -3.1f, 0.6f)
    T ~ w3.lenSq.f32                 =~~= w3d.lenSq.f32
    T ~ w3.len                       =~~= w3d.len.toFloat
    T ~ (w3 + 2f)                    ==== Vec3F(1.2f + 2f, 3.1f + 2f, -0.6f + 2f)
    T ~ (w3 + 2f)                    ==== (2f + w3)
    T ~ w3.+(1f, -1f, 2f)            ==== Vec3F(1.2f + 1f, 3.1f - 1f, -0.6f + 2f)
    T ~ (w3 + t3)                    ==== Vec3F(1.2f + -1.5f, 3.1f + 0.7f, -0.6f + 2.2f)
    T ~ (w3 - 2f)                    ==== -(2f - w3)
    T ~ (w3 - t3)                    ==== Vec3F(1.2f - -1.5f, 3.1f - 0.7f, -0.6f - 2.2f)
    T ~ (w3 * 2f)                    ==== Vec3F(1.2f * 2f, 3.1f * 2f, -0.6f * 2f)
    T ~ (2f * w3)                    ==== (w3 * 2f)
    T ~ (w3 * t3).f32                =~~= (w3d * t3d).f32
    T ~ w3.*(-1.5f, 0.7f, 2.2f)      ==== (w3 * t3)
    T ~ (w3 X t3).x                  =~~= (w3d X t3d).x.toFloat
    T ~ (w3 X t3).y                  =~~= (w3d X t3d).y.toFloat
    T ~ (w3 X t3).z                  =~~= (w3d X t3d).z.toFloat
    T ~ w3.X(-1.5f, 0.7f, 2.2f)      ==== (w3 X t3)
    T ~ (w3 proj t3).x               =~~= (w3d proj t3d).x.toFloat
    T ~ (w3 proj t3).y               =~~= (w3d proj t3d).y.toFloat
    T ~ (w3 proj t3).z               =~~= (w3d proj t3d).z.toFloat
    T ~ (w3 orth t3).x               =~~= (w3d orth t3d).x.toFloat
    T ~ w3.hat.x                     =~~= w3d.hat.x.toFloat
    T ~ w3.hat.y                     =~~= w3d.hat.y.toFloat
    T ~ w3.hat.z                     =~~= w3d.hat.z.toFloat
    T ~ w3.hat.hat                   ==== w3.hat
    T ~ Vec3F.zero.hat               ==== Vec3F.zero
    T ~ (w3 normDot t3).f32          =~~= (w3d normDot t3d).f32
    T ~ w3.normDot(w3).f32           =~~= 1f
    T ~ (w3 distSq t3).f32           =~~= (w3d distSq t3d).f32
    T ~ (w3 dist t3)                 =~~= (w3d dist t3d).toFloat
    T ~ (w3 angle t3).f32            =~~= (w3d angle t3d).f32
    T ~ (t3 angle w3)                ==== (w3 angle t3)
    val ax3 = 0.3f ~> -1f ~> 0.2f
    T ~ w3.rotate(ax3, 0.83f).x      =~~= w3d.rotate(ax3.toVec3D, 0.83f).x.toFloat
    T ~ w3.rotate(ax3, 0.83f).y      =~~= w3d.rotate(ax3.toVec3D, 0.83f).y.toFloat
    T ~ w3.rotate(ax3, 0.83f).z      =~~= w3d.rotate(ax3.toVec3D, 0.83f).z.toFloat
    T ~ w3.rotate(ax3, 0.83f).len    =~~= w3.len
    T ~ w3.pr                        ==== "[1.2 3.1 -0.6]"
    T ~ w3.prf("%.2e")               ==== "[1.20e+00 3.10e+00 -6.00e-01]"
    T ~ w3.xy                        ==== Vc.F(1.2f, 3.1f)
    T ~ w3.xz                        ==== Vc.F(1.2f, -0.6f)
    T ~ w3.yz                        ==== Vc.F(3.1f, -0.6f)
    T ~ w3.toVec3D.toVec3F           ==== w3
    T ~ (Vc.F(1.2f, 3.1f) ~> -0.6f)  ==== w3

  def testMatD(): Unit =
    given Approximation[Double] = Approximation.OfDouble(1e-12, 1e-6, 1e-12)

    val a = Mat22D(1, 2)(3, 4)
    val b = Mat22D(5, 6)(7, 8)
    T ~ a(0, 0)                      ==== 1.0
    T ~ a(0, 1)                      ==== 2.0
    T ~ a(1, 0)                      ==== 3.0
    T ~ a(1, 1)                      ==== 4.0
    T ~ a.unwrap(1)                  ==== 3.0   // column-major storage
    T ~ ((a + b) === Mat22D(6, 8)(10, 12))     ==== true
    T ~ ((b - a) === Mat22D(4, 4)(4, 4))       ==== true
    T ~ ((-a) === Mat22D(-1, -2)(-3, -4))      ==== true
    T ~ ((a * 2.0) === Mat22D(2, 4)(6, 8))     ==== true
    T ~ ((a * b) === Mat22D(19, 22)(43, 50))   ==== true
    T ~ ((a * b.T) === Mat22D(17, 23)(39, 53)) ==== true
    T ~ ((a.T * b) === Mat22D(26, 30)(38, 44)) ==== true
    T ~ (a.T.unwrap eq a.unwrap)     ==== true  // transpose is a relabel, not a copy
    T ~ a.T(0, 1)                    ==== a(1, 0)
    T ~ a.T(1, 0)                    ==== a(0, 1)
    T ~ (a.T.T === a)                ==== true
    T ~ a.det                        ==== -2.0
    T ~ a.tr                         ==== 5.0
    T ~ (a.inv === Mat22D(-2, 1)(1.5, -0.5))   ==== true
    T ~ ((a * a.inv) === Mat22D.identity)      ==== true
    T ~ ((Mat22D.identity * a) === a)          ==== true
    T ~ (a * Vec2D(1, 2))            ==== Vec2D(5, 11)
    T ~ (a.T * Vec2D(1, 2))          ==== Vec2D(7, 10)

    val c = Mat23D(1, 2, 3)(4, 5, 6)
    val d = Mat32D(7, 8)(9, 10)(11, 12)
    val e = Mat33D(1, 2, 3)(4, 5, 6)(7, 8, 10)
    T ~ c(0, 2)                      ==== 3.0
    T ~ c(1, 0)                      ==== 4.0
    T ~ d(2, 1)                      ==== 12.0
    T ~ e(2, 2)                      ==== 10.0
    T ~ c.T(2, 1)                    ==== 6.0
    T ~ d.T(1, 2)                    ==== 12.0
    T ~ ((c + c) === (c * 2.0))      ==== true
    T ~ ((d - d) === (d * 0.0))      ==== true
    T ~ ((-e) === (e * -1.0))        ==== true

    // all plain product shapes
    T ~ ((a * c) === Mat23D(9, 12, 15)(19, 26, 33))                   ==== true
    T ~ ((c * d) === Mat22D(58, 64)(139, 154))                        ==== true
    T ~ ((c * e) === Mat23D(30, 36, 45)(66, 81, 102))                 ==== true
    T ~ ((d * a) === Mat32D(31, 46)(39, 58)(47, 70))                  ==== true
    T ~ ((d * c) === Mat33D(39, 54, 69)(49, 68, 87)(59, 82, 105))     ==== true
    T ~ ((e * d) === Mat32D(58, 64)(139, 154)(231, 256))              ==== true
    T ~ ((e * e) === Mat33D(30, 36, 45)(66, 81, 102)(109, 134, 169))  ==== true

    // transposed arguments and receivers
    T ~ ((a * d.T) === Mat23D(23, 29, 35)(53, 67, 81))                ==== true
    T ~ ((c * c.T) === Mat22D(14, 32)(32, 77))                        ==== true
    T ~ ((c * e.T) === Mat23D(14, 32, 53)(32, 77, 128))               ==== true
    T ~ ((d * a.T) === Mat32D(23, 53)(29, 67)(35, 81))                ==== true
    T ~ ((d * d.T) === Mat33D(113, 143, 173)(143, 181, 219)(173, 219, 265)) ==== true
    T ~ ((e * c.T) === Mat32D(14, 32)(32, 77)(53, 128))               ==== true
    T ~ ((e * e.T) === Mat33D(14, 32, 53)(32, 77, 128)(53, 128, 213)) ==== true
    T ~ ((c.T * a) === Mat32D(13, 18)(17, 24)(21, 30))                ==== true
    T ~ ((c.T * c) === Mat33D(17, 22, 27)(22, 29, 36)(27, 36, 45))    ==== true
    T ~ ((d.T * d) === Mat22D(251, 278)(278, 308))                    ==== true
    T ~ ((d.T * e) === Mat23D(120, 147, 185)(132, 162, 204))          ==== true
    T ~ ((e.T * d) === Mat32D(120, 132)(147, 162)(185, 204))          ==== true
    T ~ ((e.T * e) === Mat33D(66, 78, 97)(78, 93, 116)(97, 116, 145)) ==== true

    // 3x3 det, tr, inv
    T ~ e.det                        =~~= -3.0
    T ~ e.tr                         ==== 16.0
    val id3 = (e * e.inv).unwrap
    var worst = 0.0
    var i = 0
    while i < 9 do
      worst = jm.max(worst, jm.abs(id3(i) - (if i % 4 == 0 then 1.0 else 0.0)))
      i += 1
    T ~ (worst < 1e-12)              ==== true

    // matrix-vector, both plain and transposed
    T ~ (c * Vec3D(1, 2, 3))         ==== Vec2D(14, 32)
    T ~ (d * Vec2D(1, 2))            ==== Vec3D(23, 29, 35)
    T ~ (e * Vec3D(1, 2, 3))         ==== Vec3D(14, 32, 53)
    T ~ (c.T * Vec2D(1, 2))          ==== Vec3D(9, 12, 15)
    T ~ (d.T * Vec3D(1, 2, 3))       ==== Vec2D(58, 64)
    T ~ (e.T * Vec3D(1, 2, 3))       ==== Vec3D(30, 36, 45)

    // row vectors: dot, row-times-matrix, outer products
    T ~ (Vec2D(1, 2).T * Vec2D(3, 4))     ==== 11.0
    T ~ (Vec3D(1, 2, 3).T * Vec3D(4, 5, 6)) ==== 32.0
    T ~ (Vec2D(1, 2).T * a).T             ==== Vec2D(7, 10)
    T ~ (Vec2D(1, 2).T * c).T             ==== Vec3D(9, 12, 15)
    T ~ (Vec3D(1, 2, 3).T * d).T          ==== Vec2D(58, 64)
    T ~ (Vec3D(1, 2, 3).T * e).T          ==== Vec3D(30, 36, 45)
    T ~ (Vec2D(1, 2).T.T)                 ==== Vec2D(1, 2)
    T ~ Vec3D(1, 2, 3).T.x                ==== 1.0
    T ~ Vec3D(1, 2, 3).T.z                ==== 3.0
    T ~ ((Vec2D(1, 2) * Vec2D(3, 4).T) === Mat22D(3, 4)(6, 8))                    ==== true
    T ~ ((Vec2D(1, 2) * Vec3D(3, 4, 5).T) === Mat23D(3, 4, 5)(6, 8, 10))          ==== true
    T ~ ((Vec3D(1, 2, 3) * Vec2D(4, 5).T) === Mat32D(4, 5)(8, 10)(12, 15))        ==== true
    T ~ ((Vec3D(1, 2, 3) * Vec3D(4, 5, 6).T) === Mat33D(4, 5, 6)(8, 10, 12)(12, 15, 18)) ==== true

    T ~ a.pr                         ==== "[[1.0 2.0] [3.0 4.0]]"
    T ~ c.pr                         ==== "[[1.0 2.0 3.0] [4.0 5.0 6.0]]"
    T ~ d.pr                         ==== "[[7.0 8.0] [9.0 10.0] [11.0 12.0]]"
    T ~ a.prf("%.2f")                ==== "[[1.00 2.00] [3.00 4.00]]"

  def testMatF(): Unit =
    val a = Mat22F(1, 2)(3, 4)
    val b = Mat22F(5, 6)(7, 8)
    T ~ a(0, 0)                      ==== 1f
    T ~ a(1, 0)                      ==== 3f
    T ~ a.unwrap(1)                  ==== 3f
    T ~ (Mat22F.D(1.0, 2.0)(3.0, 4.0) === a)   ==== true
    T ~ ((a + b) === Mat22F(6, 8)(10, 12))     ==== true
    T ~ ((b - a) === Mat22F(4, 4)(4, 4))       ==== true
    T ~ ((-a) === Mat22F(-1, -2)(-3, -4))      ==== true
    T ~ ((a * 2f) === Mat22F(2, 4)(6, 8))      ==== true
    T ~ ((a * b) === Mat22F(19, 22)(43, 50))   ==== true
    T ~ ((a * b.T) === Mat22F(17, 23)(39, 53)) ==== true
    T ~ ((a.T * b) === Mat22F(26, 30)(38, 44)) ==== true
    T ~ (a.T.unwrap eq a.unwrap)     ==== true
    T ~ a.T(0, 1)                    ==== a(1, 0)
    T ~ (a.T.T === a)                ==== true
    T ~ a.det                        ==== -2.0
    T ~ a.tr                         ==== 5.0
    T ~ (a.inv === Mat22F(-2, 1)(1.5f, -0.5f)) ==== true
    T ~ ((a * a.inv) === Mat22F.identity)      ==== true
    T ~ (a * Vc(1, 2))               ==== Vc.F(5, 11)
    T ~ (a.T * Vc(1, 2))             ==== Vc.F(7, 10)

    val c = Mat23F(1, 2, 3)(4, 5, 6)
    val d = Mat32F(7, 8)(9, 10)(11, 12)
    val e = Mat33F(1, 2, 3)(4, 5, 6)(7, 8, 10)
    T ~ c(0, 2)                      ==== 3f
    T ~ d(2, 1)                      ==== 12f
    T ~ e(2, 2)                      ==== 10f
    T ~ c.T(2, 1)                    ==== 6f
    T ~ (Mat23F.D(1, 2, 3)(4, 5, 6) === c)     ==== true
    T ~ (Mat32F.D(7, 8)(9, 10)(11, 12) === d)  ==== true
    T ~ (Mat33F.D(1, 2, 3)(4, 5, 6)(7, 8, 10) === e) ==== true

    T ~ ((a * c) === Mat23F(9, 12, 15)(19, 26, 33))                   ==== true
    T ~ ((c * d) === Mat22F(58, 64)(139, 154))                        ==== true
    T ~ ((c * e) === Mat23F(30, 36, 45)(66, 81, 102))                 ==== true
    T ~ ((d * a) === Mat32F(31, 46)(39, 58)(47, 70))                  ==== true
    T ~ ((d * c) === Mat33F(39, 54, 69)(49, 68, 87)(59, 82, 105))     ==== true
    T ~ ((e * d) === Mat32F(58, 64)(139, 154)(231, 256))              ==== true
    T ~ ((e * e) === Mat33F(30, 36, 45)(66, 81, 102)(109, 134, 169))  ==== true
    T ~ ((a * d.T) === Mat23F(23, 29, 35)(53, 67, 81))                ==== true
    T ~ ((c * c.T) === Mat22F(14, 32)(32, 77))                        ==== true
    T ~ ((c.T * c) === Mat33F(17, 22, 27)(22, 29, 36)(27, 36, 45))    ==== true
    T ~ ((d.T * d) === Mat22F(251, 278)(278, 308))                    ==== true
    T ~ ((e.T * e) === Mat33F(66, 78, 97)(78, 93, 116)(97, 116, 145)) ==== true

    T ~ e.det                        =~~= -3.0
    T ~ e.tr                         ==== 16.0
    val id3 = (e * e.inv).unwrap
    var worst = 0.0
    var i = 0
    while i < 9 do
      worst = jm.max(worst, jm.abs(id3(i) - (if i % 4 == 0 then 1f else 0f)))
      i += 1
    T ~ (worst < 1e-6)               ==== true

    T ~ (c * Vec3F(1, 2, 3))         ==== Vc.F(14, 32)
    T ~ (d * Vc(1, 2))               ==== Vec3F(23, 29, 35)
    T ~ (e * Vec3F(1, 2, 3))         ==== Vec3F(14, 32, 53)
    T ~ (c.T * Vc(1, 2))             ==== Vec3F(9, 12, 15)
    T ~ (d.T * Vec3F(1, 2, 3))       ==== Vc.F(58, 64)
    T ~ (e.T * Vec3F(1, 2, 3))       ==== Vec3F(30, 36, 45)

    T ~ (Vc(1, 2).T * Vc(3, 4))           ==== 11.0
    T ~ (Vec3F(1, 2, 3).T * Vec3F(4, 5, 6)) ==== 32.0
    T ~ (Vc(1, 2).T * a).T                ==== Vc.F(7, 10)
    T ~ (Vc(1, 2).T * c).T                ==== Vec3F(9, 12, 15)
    T ~ (Vec3F(1, 2, 3).T * d).T          ==== Vc.F(58, 64)
    T ~ (Vec3F(1, 2, 3).T * e).T          ==== Vec3F(30, 36, 45)
    T ~ (Vc(1, 2).T.T)                    ==== Vc(1, 2)
    T ~ Vc(1, 2).T.y                      ==== 2f
    T ~ Vec3F(1, 2, 3).T.z                ==== 3f
    T ~ ((Vc(1, 2) * Vc(3, 4).T) === Mat22F(3, 4)(6, 8))                    ==== true
    T ~ ((Vc(1, 2) * Vec3F(3, 4, 5).T) === Mat23F(3, 4, 5)(6, 8, 10))       ==== true
    T ~ ((Vec3F(1, 2, 3) * Vc(4, 5).T) === Mat32F(4, 5)(8, 10)(12, 15))     ==== true
    T ~ ((Vec3F(1, 2, 3) * Vec3F(4, 5, 6).T) === Mat33F(4, 5, 6)(8, 10, 12)(12, 15, 18)) ==== true

    T ~ a.pr                         ==== "[[1.0 2.0] [3.0 4.0]]"
    T ~ d.pr                         ==== "[[7.0 8.0] [9.0 10.0] [11.0 12.0]]"
    T ~ a.prf("%.2f")                ==== "[[1.00 2.00] [3.00 4.00]]"

  def testXforms(): Unit =
    given Approximation[Double] = Approximation.OfDouble(1e-12, 1e-6, 1e-12)

    val t1 = Xform2D.translate(3, 4)
    T ~ t1(Vec2D(1, 1))              ==== Vec2D(4, 5)
    T ~ t1.dir(Vec2D(1, 1))          ==== Vec2D(1, 1)
    T ~ (t1.mat === Mat22D.identity) ==== true
    T ~ t1.shift                     ==== Vec2D(3, 4)
    T ~ t1.det                       ==== 1.0
    T ~ (Xform2D.translate(Vec2D(3, 4)) === t1) ==== true
    T ~ (Xform2D(1, 0)(0, 1)(3, 4) === t1)      ==== true
    val r1 = Xform2D.rotate(jm.PI/2)
    T ~ r1(Vec2D(1, 0)).x            =~~= 0.0
    T ~ r1(Vec2D(1, 0)).y            =~~= 1.0
    T ~ (Xform2D.scale(2, 3))(Vec2D(1, 1)) ==== Vec2D(2, 3)
    T ~ (Xform2D.scale(2))(Vec2D(1, 1))    ==== Vec2D(2, 2)
    T ~ Xform2D.scale(2, 3).det      ==== 6.0
    T ~ (t1 * r1)(Vec2D(1, 0)).x     =~~= 3.0
    T ~ (t1 * r1)(Vec2D(1, 0)).y     =~~= 5.0
    T ~ (r1 * t1)(Vec2D(1, 0)).x     =~~= -4.0
    T ~ (r1 * t1)(Vec2D(1, 0)).y     =~~= 4.0
    T ~ Xform2D.identity(Vec2D(5, 7)) ==== Vec2D(5, 7)
    val x2 = Xform2D(Mat22D(1, 2)(3, 4), Vec2D(1, -1))
    T ~ x2(Vec2D(1, 2))              ==== Vec2D(6, 10)
    T ~ (x2.mat === Mat22D(1, 2)(3, 4)) ==== true
    T ~ x2.shift                     ==== Vec2D(1, -1)
    T ~ x2.inv(x2(Vec2D(1, 2))).x    =~~= 1.0
    T ~ x2.inv(x2(Vec2D(1, 2))).y    =~~= 2.0
    val i2 = (x2.inv * x2).unwrap
    var worst = 0.0
    var i = 0
    while i < 6 do
      worst = jm.max(worst, jm.abs(i2(i) - (if i == 0 || i == 3 then 1.0 else 0.0)))
      i += 1
    T ~ (worst < 1e-12)              ==== true
    T ~ t1.pr                        ==== "[[1.0 0.0 3.0] [0.0 1.0 4.0]]"
    T ~ t1.prf("%.1f")               ==== "[[1.0 0.0 3.0] [0.0 1.0 4.0]]"

    val t3 = Xform3D.translate(1, 2, 3)
    T ~ t3(Vec3D(1, 1, 1))           ==== Vec3D(2, 3, 4)
    T ~ t3.dir(Vec3D(1, 1, 1))       ==== Vec3D(1, 1, 1)
    T ~ (t3.mat === Mat33D.identity) ==== true
    T ~ t3.shift                     ==== Vec3D(1, 2, 3)
    T ~ (Xform3D.translate(Vec3D(1, 2, 3)) === t3) ==== true
    T ~ (Xform3D(1, 0, 0)(0, 1, 0)(0, 0, 1)(1, 2, 3) === t3) ==== true
    val r3 = Xform3D.rotate(Vec3D(0, 0, 1), jm.PI/2)
    T ~ r3(Vec3D(1, 0, 0)).x         =~~= 0.0
    T ~ r3(Vec3D(1, 0, 0)).y         =~~= 1.0
    T ~ r3(Vec3D(1, 0, 0)).z         =~~= 0.0
    T ~ r3(Vec3D(0, 0, 5)).z         =~~= 5.0
    val ax = Vec3D(0.3, -1, 0.2)
    val w = Vec3D(1.2, 3.1, -0.6)
    T ~ Xform3D.rotate(ax, 0.83)(w).x =~~= w.rotate(ax, 0.83).x
    T ~ Xform3D.rotate(ax, 0.83)(w).y =~~= w.rotate(ax, 0.83).y
    T ~ Xform3D.rotate(ax, 0.83)(w).z =~~= w.rotate(ax, 0.83).z
    T ~ (Xform3D.scale(2))(Vec3D(1, 1, 1))    ==== Vec3D(2, 2, 2)
    T ~ (Xform3D.scale(2, 3, 4))(Vec3D(1, 1, 1)) ==== Vec3D(2, 3, 4)
    T ~ Xform3D.scale(2, 3, 4).det   ==== 24.0
    T ~ (t3 * r3)(Vec3D(1, 0, 0)).y  =~~= 3.0
    T ~ (r3 * t3)(Vec3D(1, 0, 0)).x  =~~= -2.0
    T ~ Xform3D.identity(Vec3D(5, 7, 9)) ==== Vec3D(5, 7, 9)
    val x3 = t3 * Xform3D.rotate(ax, 0.83) * Xform3D.scale(2)
    T ~ x3.inv(x3(Vec3D(1, 2, 3))).x =~~= 1.0
    T ~ x3.inv(x3(Vec3D(1, 2, 3))).y =~~= 2.0
    T ~ x3.inv(x3(Vec3D(1, 2, 3))).z =~~= 3.0
    T ~ x3.det                       =~~= 8.0
    val i3 = (x3.inv * x3).unwrap
    worst = 0.0
    i = 0
    while i < 12 do
      worst = jm.max(worst, jm.abs(i3(i) - (if i % 4 == 0 && i < 9 then 1.0 else 0.0)))
      i += 1
    T ~ (worst < 1e-12)              ==== true
    T ~ (Xform3D(x3.mat, x3.shift) === x3) ==== true
    T ~ Xform3D.identity.pr          ==== "[[1.0 0.0 0.0 0.0] [0.0 1.0 0.0 0.0] [0.0 0.0 1.0 0.0]]"

    val f1 = Xform2F.translate(3, 4)
    T ~ f1(Vc(1, 1))                 ==== Vc.F(4, 5)
    T ~ f1.dir(Vc(1, 1))             ==== Vc.F(1, 1)
    T ~ (f1.mat === Mat22F.identity) ==== true
    T ~ f1.shift                     ==== Vc.F(3, 4)
    T ~ f1.det                       ==== 1.0
    T ~ (Xform2F.translate(Vc(3, 4)) === f1) ==== true
    T ~ (Xform2F(1, 0)(0, 1)(3, 4) === f1)   ==== true
    val g1 = Xform2F.rotate((jm.PI/2).toFloat)
    T ~ g1(Vc(1, 0)).x               =~~= 0f
    T ~ g1(Vc(1, 0)).y               =~~= 1f
    T ~ (Xform2F.scale(2, 3))(Vc(1, 1)) ==== Vc.F(2, 3)
    T ~ (Xform2F.scale(2))(Vc(1, 1))    ==== Vc.F(2, 2)
    T ~ (f1 * g1)(Vc(1, 0)).y        =~~= 5f
    T ~ Xform2F.identity(Vc(5, 7))   ==== Vc.F(5, 7)
    val y2 = Xform2F(Mat22F(1, 2)(3, 4), Vc(1, -1))
    T ~ y2(Vc(1, 2))                 ==== Vc.F(6, 10)
    T ~ y2.inv(y2(Vc(1, 2))).x       =~~= 1f
    T ~ y2.inv(y2(Vc(1, 2))).y       =~~= 2f

    val t3f = Xform3F.translate(1, 2, 3)
    T ~ t3f(Vec3F(1, 1, 1))          ==== Vec3F(2, 3, 4)
    T ~ t3f.dir(Vec3F(1, 1, 1))      ==== Vec3F(1, 1, 1)
    T ~ (t3f.mat === Mat33F.identity) ==== true
    T ~ t3f.shift                    ==== Vec3F(1, 2, 3)
    T ~ (Xform3F(1, 0, 0)(0, 1, 0)(0, 0, 1)(1, 2, 3) === t3f) ==== true
    val r3f = Xform3F.rotate(Vec3F(0, 0, 1), (jm.PI/2).toFloat)
    T ~ r3f(Vec3F(1, 0, 0)).x        =~~= 0f
    T ~ r3f(Vec3F(1, 0, 0)).y        =~~= 1f
    T ~ (Xform3F.scale(2, 3, 4))(Vec3F(1, 1, 1)) ==== Vec3F(2, 3, 4)
    T ~ (Xform3F.scale(2))(Vec3F(1, 1, 1))       ==== Vec3F(2, 2, 2)
    T ~ Xform3F.scale(2, 3, 4).det   =~~= 24.0
    T ~ (t3f * r3f)(Vec3F(1, 0, 0)).y =~~= 3f
    T ~ Xform3F.identity(Vec3F(5, 7, 9)) ==== Vec3F(5, 7, 9)
    val x3f = t3f * r3f * Xform3F.scale(2)
    T ~ x3f.inv(x3f(Vec3F(1, 2, 3))).x =~~= 1f
    T ~ x3f.inv(x3f(Vec3F(1, 2, 3))).y =~~= 2f
    T ~ x3f.inv(x3f(Vec3F(1, 2, 3))).z =~~= 3f
    T ~ (Xform3F(x3f.mat, x3f.shift) === x3f) ==== true
}
