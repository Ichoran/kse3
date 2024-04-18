// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2023 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC

package kse.maths.fitting


import scala.collection.immutable.{Range => Rg}

import kse.basics.{given, _}
import kse.basics.intervals._

import kse.maths.{given, _}


opaque type X2Y = LinearFn2D | Fit2D.Impl
object X2Y {
  inline def wrap(linf: LinearFn2D): X2Y = linf
  inline def wrap(fit2: Fit2D.Impl): X2Y = fit2

  extension (x2y: X2Y) {
    inline def underlying: LinearFn2D | Fit2D.Impl = x2y
  }

  extension (x2y: kse.maths.fitting.X2Y) {
    inline def apply(value: Double): Double = inline x2y match
      case linf: LinearFn2D => linf(value)
      case fit2: Fit2D.Impl => fit2.exactXsampleY_x2y(value)
      case _ => x2y match
        case linf: LinearFn2D => linf(value)
        case fit2: Fit2D.Impl => fit2.exactXsampleY_x2y(value)
    inline def inverse(value: Double): Double = inline x2y match
      case linf: LinearFn2D => linf.inverse(value)
      case fit2: Fit2D.Impl => fit2.exactXsampleY_y2x(value)
      case _ => x2y match
        case linf: LinearFn2D => linf.inverse(value)
        case fit2: Fit2D.Impl => fit2.exactXsampleY_y2x(value)
    inline def intercept: Double = inline x2y match
      case linf: LinearFn2D => linf.intercept
      case fit2: Fit2D.Impl => fit2.exactXsampleY_intercept
      case _ => x2y match
        case linf: LinearFn2D => linf.intercept
        case fit2: Fit2D.Impl => fit2.exactXsampleY_intercept
    inline def slope: Double = inline x2y match
      case linf: LinearFn2D => linf.slope
      case fit2: Fit2D.Impl => fit2.exactXsampleY_slope
      case _ => x2y match
        case linf: LinearFn2D => linf.slope
        case fit2: Fit2D.Impl => fit2.exactXsampleY_slope

    inline def mirror: kse.maths.fitting.Y2X = inline x2y match
      case linf: LinearFn2D => Y2X.wrap(linf.mirror)
      case fit2: Fit2D.Impl => Y2X.wrap(fit2)
      case _ => x2y match
        case linf: LinearFn2D => Y2X.wrap(linf.mirror)
        case fit2: Fit2D.Impl => Y2X.wrap(fit2)

    inline def rsq: Double = inline x2y match
      case linf: LinearFn2D => Double.NaN
      case fit2: Fit2D.Impl => fit2.exactXsampleY_rsq
      case _ => x2y match
        case linf: LinearFn2D => Double.NaN
        case fit2: Fit2D.Impl => fit2.exactXsampleY_rsq
    inline def offsetError: Double = inline x2y match
      case linf: LinearFn2D => Double.NaN
      case fit2: Fit2D.Impl => fit2.exactXsampleY_offsetError
      case _ => x2y match
        case linf: LinearFn2D => Double.NaN
        case fit2: Fit2D.Impl => fit2.exactXsampleY_offsetError
    inline def slopeError: Double = inline x2y match
      case linf: LinearFn2D => Double.NaN
      case fit2: Fit2D.Impl => fit2.exactXsampleY_slopeError
      case _ => x2y match
        case linf: LinearFn2D => Double.NaN
        case fit2: Fit2D.Impl => fit2.exactXsampleY_slopeError
    inline def pm(value: Double): PlusMinus = inline x2y match
      case linf: LinearFn2D => PlusMinus(linf(value).toFloat, Float.NaN)
      case fit2: Fit2D.Impl => fit2.exactXsampleY_pm(value)
      case _ => x2y match
        case linf: LinearFn2D => PlusMinus(linf(value).toFloat, Float.NaN)
        case fit2: Fit2D.Impl => fit2.exactXsampleY_pm(value)
    inline def pm(value: PlusMinus): PlusMinus = inline x2y match
      case linf: LinearFn2D => linf.pm(value)
      case fit2: Fit2D.Impl => fit2.exactXsampleY_pm(value)
      case _ => x2y match
        case linf: LinearFn2D => linf.pm(value)
        case fit2: Fit2D.Impl => fit2.exactXsampleY_pm(value)

    inline def toLinFn: LinearFn2D = inline x2y match
      case linf: LinearFn2D => linf
      case fit2: Fit2D.Impl => LinearFn2D(fit2.exactXsampleY_intercept, fit2.exactXsampleY_slope)
      case _ => x2y match
        case linf: LinearFn2D => linf
        case fit2: Fit2D.Impl => LinearFn2D(fit2.exactXsampleY_intercept, fit2.exactXsampleY_slope)

    inline def toLine2D: Line2D = inline x2y match
      case linf: LinearFn2D =>
        val l = (1.0 + linf.slope*linf.slope).sqrt
        Line2D.Immutable(0, linf.intercept, 1.0/l, linf.slope/l)
      case fit2: Fit2D.Impl => fit2.line.immutable
      case _ => x2y match        
        case linf: LinearFn2D =>
          val l = (1.0 + linf.slope*linf.slope).sqrt
          Line2D.Immutable(0, linf.intercept, 1.0/l, linf.slope/l)
        case fit2: Fit2D.Impl => fit2.line.immutable
  }
}

opaque type Y2X = LinearFn2D | Fit2D.Impl
object Y2X {
  inline def wrap(linf: LinearFn2D): Y2X = linf
  inline def wrap(fit2: Fit2D.Impl): Y2X = fit2

  extension (y2x: Y2X) {
    inline def underlying: LinearFn2D | Fit2D.Impl = y2x
  }

  extension (y2x: kse.maths.fitting.Y2X) {
    inline def apply(value: Double): Double = inline y2x match
      case linf: LinearFn2D => linf(value)
      case fit2: Fit2D.Impl => fit2.exactYsampleX_y2x(value)
      case _ => y2x match
        case linf: LinearFn2D => linf(value)
        case fit2: Fit2D.Impl => fit2.exactYsampleX_y2x(value)
    inline def inverse(value: Double): Double = inline y2x match
      case linf: LinearFn2D => linf.inverse(value)
      case fit2: Fit2D.Impl => fit2.exactYsampleX_x2y(value)
      case _ => y2x match
        case linf: LinearFn2D => linf.inverse(value)
        case fit2: Fit2D.Impl => fit2.exactYsampleX_x2y(value)
    inline def intercept: Double = inline y2x match
      case linf: LinearFn2D => linf.intercept
      case fit2: Fit2D.Impl => fit2.exactYsampleX_intercept
      case _ => y2x match
        case linf: LinearFn2D => linf.intercept
        case fit2: Fit2D.Impl => fit2.exactYsampleX_intercept
    inline def slope: Double = inline y2x match
      case linf: LinearFn2D => linf.slope
      case fit2: Fit2D.Impl => fit2.exactYsampleX_slope
      case _ => y2x match
        case linf: LinearFn2D => linf.slope
        case fit2: Fit2D.Impl => fit2.exactYsampleX_slope

    inline def mirror: kse.maths.fitting.X2Y = inline y2x match
      case linf: LinearFn2D => X2Y.wrap(linf.mirror)
      case fit2: Fit2D.Impl => X2Y.wrap(fit2)
      case _ => y2x match
        case linf: LinearFn2D => X2Y.wrap(linf.mirror)
        case fit2: Fit2D.Impl => X2Y.wrap(fit2)

    inline def rsq: Double = inline y2x match
      case linf: LinearFn2D => Double.NaN
      case fit2: Fit2D.Impl => fit2.exactYsampleX_rsq
      case _ => y2x match
        case linf: LinearFn2D => Double.NaN
        case fit2: Fit2D.Impl => fit2.exactYsampleX_rsq
    inline def offsetError: Double = inline y2x match
      case linf: LinearFn2D => Double.NaN
      case fit2: Fit2D.Impl => fit2.exactYsampleX_offsetError
      case _ => y2x match
        case linf: LinearFn2D => Double.NaN
        case fit2: Fit2D.Impl => fit2.exactYsampleX_offsetError
    inline def slopeError: Double = inline y2x match
      case linf: LinearFn2D => Double.NaN
      case fit2: Fit2D.Impl => fit2.exactYsampleX_slopeError
      case _ => y2x match
        case linf: LinearFn2D => Double.NaN
        case fit2: Fit2D.Impl => fit2.exactYsampleX_slopeError
    inline def pm(value: Double): PlusMinus = inline y2x match
      case linf: LinearFn2D => PlusMinus(linf(value).toFloat, Float.NaN)
      case fit2: Fit2D.Impl => fit2.exactYsampleX_pm(value)
      case _ => y2x match
        case linf: LinearFn2D => PlusMinus(linf(value).toFloat, Float.NaN)
        case fit2: Fit2D.Impl => fit2.exactYsampleX_pm(value)
    inline def pm(value: PlusMinus): PlusMinus = inline y2x match
      case linf: LinearFn2D => linf.pm(value)
      case fit2: Fit2D.Impl => fit2.exactYsampleX_pm(value)
      case _ => y2x match
        case linf: LinearFn2D => linf.pm(value)
        case fit2: Fit2D.Impl => fit2.exactYsampleX_pm(value)     

    inline def toLinFn: LinearFn2D = inline y2x match
      case linf: LinearFn2D => linf
      case fit2: Fit2D.Impl => LinearFn2D(fit2.exactYsampleX_intercept, fit2.exactYsampleX_slope)
      case _ => y2x match
        case linf: LinearFn2D => linf
        case fit2: Fit2D.Impl => LinearFn2D(fit2.exactYsampleX_intercept, fit2.exactYsampleX_slope)

    inline def toLine2D: Line2D = inline y2x match
      case linf: LinearFn2D =>
        val l = (1.0 + linf.slope*linf.slope).sqrt
        Line2D.Immutable(linf.intercept, 0, linf.slope/l, 1.0/l)
      case fit2: Fit2D.Impl => fit2.line.immutable
      case _ => y2x match        
        case linf: LinearFn2D =>
          val l = (1.0 + linf.slope*linf.slope).sqrt
          Line2D.Immutable(linf.intercept, 0, linf.slope/l, 1.0/l)
        case fit2: Fit2D.Impl => fit2.line.immutable
  }
}



final case class LinearFn2D(val intercept: Double, val slope: Double) extends (Double => Double) {
  def apply(value: Double): Double = value*slope + intercept
  def inverse(value: Double): Double = (value - intercept)/slope
  def pm(value: PlusMinus): PlusMinus = PlusMinus.D(value.value * slope + intercept, value.error * slope)
  def mirror: LinearFn2D = LinearFn2D(-intercept/slope, 1/slope)
}


sealed trait Line2D {
  def c: Vc
  def cx: Double
  def cy: Double

  def u: Vc
  def ux: Double
  def uy: Double

  def theta: Double

  final def proj(x: Double, y: Double): Double = (x - cx) * ux + (y - cy) * uy
  final inline def proj(xy: Vc): Double = proj(xy.x, xy.y)
  final def projV(xy: Vc): Vc =
    val p = proj(xy)
    Vc.D(ux*p + cx, uy*p + cy)

  final def orth(x: Double, y: Double): Double = (y - cy) * ux - (x - cx) * uy
  final inline def orth(xy: Vc): Double = orth(xy.x, xy.y)
  final def orthV(xy: Vc): Vc =
    val o = orth(xy)
    Vc.D(-uy*o + cx, ux*o + cy)

  def immutable: Line2D.Immutable = Line2D.Immutable(cx, cy, ux, uy)
}
object Line2D {
  final case class Immutable(cx: Double, cy: Double, ux: Double, uy: Double) extends Line2D {
    val c = Vc.D(cx, cy)
    val u = Vc.D(ux, uy)
    def theta = math.atan2(uy, ux)

    def centered =
      if cx == 0 && cy == 0 then this
      else Immutable(0, 0, ux, uy)

    override def immutable: Line2D.Immutable = this
  }
}


sealed abstract class Fit2D() {
  def samples: Long

  def x2y: X2Y
  def y2x: Y2X
  def line: Line2D

  def estX: Est
  def estY: Est
  def mutableCopy: Fit2D

  final inline def +=(xy: Vc): Unit = this.+=(xy.x, xy.y)
  def +=(x: Double, y: Double): Unit

  final inline def -=(xy: Vc): Unit = this.-=(xy.x, xy.y)
  def -=(x: Double, y: Double): Unit

  def ++=(xs: Array[Double], ys: Array[Double]): Unit
  def ++=(xs: IterableOnce[Double], ys: IterableOnce[Double]): Unit
  def ++=(vs: Array[Vc]): Unit
  def ++=(vs: IterableOnce[Vc]): Unit

  def addRange(xs: Array[Double], i0: Int, iN: Int)(ys: Array[Double], j0: Int, jN: Int): Unit

  inline def addRange(xs: Array[Double], i0: Int, iN: Int)(ys: Array[Double], inline yrg: Rg): Unit =
    val jv = Iv of yrg
    addRange(xs, i0, iN)(ys, jv.i0, jv.iN)
  inline def addRange(xs: Array[Double], i0: Int, iN: Int)(ys: Array[Double], inline yv: Iv | PIv): Unit =
    val jv = Iv.of(yv, ys)
    addRange(xs, i0, iN)(ys, jv.i0, jv.iN)
  inline def addRange(xs: Array[Double], inline xrg: Rg)(ys: Array[Double], j0: Int, jN: Int): Unit =
    val iv = Iv of xrg
    addRange(xs, iv.i0, iv.iN)(ys, j0, jN)
  inline def addRange(xs: Array[Double], inline xrg: Rg)(ys: Array[Double], inline yrg: Rg): Unit =
    val iv = Iv of xrg
    val jv = Iv of yrg
    addRange(xs, iv.i0, iv.iN)(ys, jv.i0, jv.iN)
  inline def addRange(xs: Array[Double], inline xrg: Rg)(ys: Array[Double], inline yv: Iv | PIv): Unit =
    val iv = Iv of xrg
    val jv = Iv.of(yv, ys)
    addRange(xs, iv.i0, iv.iN)(ys, jv.i0, jv.iN)
  inline def addRange(xs: Array[Double], inline xv: Iv | PIv)(ys: Array[Double], j0: Int, jN: Int): Unit =
    val iv = Iv.of(xv, xs)
    addRange(xs, iv.i0, iv.iN)(ys, j0, jN)
  inline def addRange(xs: Array[Double], inline xv: Iv | PIv)(ys: Array[Double], inline yrg: Rg): Unit =
    val iv = Iv.of(xv, xs)
    val jv = Iv of yrg
    addRange(xs, iv.i0, iv.iN)(ys, jv.i0, jv.iN)
  inline def addRange(xs: Array[Double], inline xv: Iv | PIv)(ys: Array[Double], inline yv: Iv | PIv): Unit =
    val iv = Iv.of(xv, xs)
    val jv = Iv.of(yv, ys)
    addRange(xs, iv.i0, iv.iN)(ys, jv.i0, jv.iN)

  def addRange(vs: Array[Vc], i0: Int, iN: Int): Unit
  
  inline def addRange(vs: Array[Vc], inline rg: Rg): Unit =
    val iv = Iv of rg
    addRange(vs, iv.i0, iv.iN)
  inline def addRange(vs: Array[Vc], inline v: Iv | PIv): Unit =
    val iv = Iv.of(v, vs)
    addRange(vs, iv.i0, iv.iN)

  def reset(): Unit
}
object Fit2D {
  final class Impl() extends Fit2D() with Line2D {
    var n = 0L
    var cx = 0.0
    var cy = 0.0

    var Sxx = 0.0
    var Sxy = 0.0
    var Syy = 0.0

    private var cached = 0

    private var xb = Double.NaN
    private var xm = Double.NaN
    private var xe = Vc.NaN
    private var yb = Double.NaN
    private var ym = Double.NaN
    private var ye = Vc.NaN
    private var th = Double.NaN
    private var ex = Double.NaN
    private var ey = Double.NaN

    private def exactX(): Unit =
      if (cached & 1) != 1 then
        xm = if n >= 2 then Sxy/Sxx else Double.NaN
        xb = cy - xm*cx
        cached |= 1
    private def exactY(): Unit =
      if (cached & 2) != 2 then
        ym = if n >= 2 then Sxy/Syy else Double.NaN
        yb = cx - ym*cy
        cached |= 2
    private def bestFit(): Unit =
      if (cached & 4) != 4 then
        if n >= 2 && (Sxy != 0 || Sxx != Syy) then
          if Sxx == Syy then
            ex = 1
            ey = 0
          else
            th = 0.5 * math.atan2(2*Sxy, Sxx - Syy)
            ex = math.cos(th)
            ey = math.sin(th)
        else
          ex = Double.NaN
          ey = Double.NaN
        cached |= 4
    private def errorX(): Unit =
      if (cached & 0x10) != 0x10 then
        if n < 3 then xe = Vc.NaN
        else
          val ee = (Syy - Sxy.sq/Sxx) / (n-2)
          xe = Vc.D((ee/Sxx).zsqrt, ee.zsqrt)
        cached |= 0x10
    private def errorY(): Unit =
      if (cached & 0x20) != 0x20 then
        if n < 3 then ye = Vc.NaN
        else
          val ee = (Sxx - Sxy.sq/Syy) / (n-2)
          ye = Vc.D(ee.zsqrt, (ee/Syy).zsqrt)
        cached |= 0x20


    def samples = n

    def x2y: X2Y = X2Y.wrap(this)
    def y2x: Y2X = Y2X.wrap(this)
    def line: Line2D = this
    def estX: Est = Est.M(n.toDouble, cx, Sxx)
    def estY: Est = Est.M(n.toDouble, cy, Syy)

    def c: Vc = Vc.D(cx, cy)

    def u: Vc =
      bestFit()
      Vc.D(ex, ey)
    def ux: Double =
      bestFit()
      ex
    def uy: Double =
      bestFit()
      ey
    def theta: Double =
      bestFit()
      th

    def exactRsq =
      if n < 3 then Double.NaN
      else 1 - (n-1)*(1 - Sxy.sq/(Sxx * Syy))/(n-2)

    def exactXsampleY_x2y(value: Double): Double =
      exactX()
      xm*value + xb
    def exactXsampleY_y2x(value: Double): Double =
      exactX()
      (value - xb)/xm
    def exactXsampleY_intercept: Double =
      exactX()
      xb
    def exactXsampleY_slope: Double =
      exactX()
      xm
    inline def exactXsampleY_rsq: Double = exactRsq
    def exactXsampleY_offsetError: Float =
      if n < 3 then Float.NaN
      else
        errorX()
        (xe.y/n.toDouble.sqrt).toFloat
    def exactXsampleY_slopeError: Float =
      if n < 3 then Float.NaN
      else
        errorX()
        xe.x
    def exactXsampleY_pm(value: Double): PlusMinus =
      exactX()
      errorX()
      PlusMinus.D(xm*value + xb, xe.y * (1.0/n + (value-cx).sq/Sxx).zsqrt)
    def exactXsampleY_pm(value: PlusMinus): PlusMinus =
      exactX()
      errorX()
      PlusMinus.D(xm*value.value + xb, (xe.y.sq * (1.0/n + (value.value-cx).sq/Sxx) + (xm*value.error).sq).zsqrt)

    def exactYsampleX_y2x(value: Double): Double =
      exactY()
      ym*value + yb
    def exactYsampleX_x2y(value: Double): Double =
      exactY()
      (value - yb)/ym
    def exactYsampleX_intercept: Double =
      exactY()
      yb
    def exactYsampleX_slope: Double =
      exactY()
      ym
    inline def exactYsampleX_rsq: Double = exactRsq
    def exactYsampleX_offsetError: Float =
      if n < 3 then Float.NaN
      else
        errorY()
        (ye.x/n.toDouble.sqrt).toFloat
    def exactYsampleX_slopeError: Float =
      if n < 3 then Float.NaN
      else
        errorY()
        ye.y
    def exactYsampleX_pm(value: Double): PlusMinus =
      exactY()
      errorY()
      PlusMinus.D(ym*value + yb, ye.x * (1.0/n + (value-cy).sq/Syy).zsqrt)
    def exactYsampleX_pm(value: PlusMinus): PlusMinus =
      exactY()
      errorY()
      PlusMinus.D(ym*value.value + yb, (ye.x.sq * (1.0/n + (value.value-cy).sq/Syy) + (ym*value.error).sq).zsqrt)

    def reset(): Unit =
      cached = 0
      n = 0
      cx = 0
      cy = 0
      Sxx = 0
      Syy = 0
      Sxy = 0

    def mutableCopy: Fit2D.Impl =
      val fit2 = new Impl()
      fit2.n = n
      fit2.cx = cx
      fit2.cy = cy
      fit2.Sxx = Sxx
      fit2.Syy = Syy
      fit2.Sxy = Sxy
      fit2

    private def plusImpl(x: Double, y: Double): Unit =
      val cx_ = (n*cx + x)/(n+1)
      val cy_ = (n*cy + y)/(n+1)
      Sxx += (x - cx)*(x - cx_)
      Syy += (y - cy)*(y - cy_)
      Sxy += (x - cx_)*(y - cy)  // Could also be (x - cx)*(y - cy_)
      cx = cx_
      cy = cy_
      n += 1

    private def minusImpl(x: Double, y: Double): Unit =
      val cx_ = (n*cx - x)/(n-1)
      val cy_ = (n*cy - y)/(n-1)
      Sxx -= (x - cx)*(x - cx_)
      Syy -= (y - cy)*(y - cy_)
      Sxy -= (x - cx)*(y - cy_)   // Use this to be symmetric with plus
      cx = cx_
      cy = cy_
      n -= 1

    def +=(x: Double, y: Double): Unit =
      if !(x + y).nan then
        cached = 0
        if n == 0 then
          cx = x
          cy = y
          n = 1
        else plusImpl(x, y)

    def -=(x: Double, y: Double): Unit =
      if !(x + y).nan then
        cached = 0
        if n < 3 then
          if n == 2 then
            cx = (2*cx - x)
            cy = (2*cy - y)
            Sxx = 0
            Syy = 0
            Sxy = 0
            n = 1
          else
            cx = 0
            cy = 0
            n = 0
        else minusImpl(x, y)

    def ++=(xs: Array[Double], ys: Array[Double]): Unit =
      val m = math.min(xs.length, ys.length)
      addRangeImpl(xs, 0)(ys, 0)(m)

    def ++=(xs: IterableOnce[Double], ys: IterableOnce[Double]): Unit =
      val xi = xs.iterator
      val yi = ys.iterator
      val n0 = n
      while xi.hasNext && yi.hasNext do
        val x = xi.next
        val y = yi.next
        if !(x + y).nan then plusImpl(x, y)
      if n > n0 then cached = 0

    def ++=(vs: Array[Vc]): Unit =
      vs.peek(){ this += _ }

    def ++=(vs: IterableOnce[Vc]): Unit =
      val vi = vs.iterator
      while vi.hasNext do this += vi.next

    def addRange(xs: Array[Double], i0: Int, iN: Int)(ys: Array[Double], j0: Int, jN: Int): Unit =
      val k0 = math.max(0, i0)
      val kN = math.min(xs.length, iN)
      if k0 < kN then
        val l0 = math.max(0, j0)
        val lN = math.min(ys.length, jN)
        if l0 < lN then
          val m = math.min(kN - k0, lN - l0)
          addRangeImpl(xs, k0)(ys, l0)(m)

    def addRange(vs: Array[Vc], i0: Int, iN: Int): Unit =
      vs.clip.peek(i0, iN){ this += _ }

    private def addRangeImpl(xs: Array[Double], i0: Int)(ys: Array[Double], j0: Int)(m: Int): Unit =
      var i = i0
      var j = j0
      var k = m
      val n0 = n
      while k > 0 do
        plusImpl(xs(i), ys(j))
        i += 1
        j += 1
        k -= 1
      if n > n0 then cached = 0

    override def toString = s"Fit centered at [$cx, $cy], n=$n"
  }
}

