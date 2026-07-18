// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2023-25 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC

package kse.maths.fitting


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.collection.immutable.{Range => Rg}

import java.lang.{Math => jm}

import kse.basics.{given, _}
import kse.basics.intervals._

import kse.maths.{given, _}


opaque type X2Y = LinearFn2D | FitLine.Impl
object X2Y {
  inline def wrap(linf: LinearFn2D): X2Y = linf
  inline def wrap(fit2: FitLine.Impl): X2Y = fit2

  extension (x2y: X2Y) {
    inline def underlying: LinearFn2D | FitLine.Impl = x2y
  }

  extension (x2y: kse.maths.fitting.X2Y) {
    inline def apply(value: Double): Double = inline x2y match
      case linf: LinearFn2D => linf(value)
      case fit2: FitLine.Impl => fit2.exactXsampleY_x2y(value)
      case _ => x2y match
        case linf: LinearFn2D => linf(value)
        case fit2: FitLine.Impl => fit2.exactXsampleY_x2y(value)
    inline def inverse(value: Double): Double = inline x2y match
      case linf: LinearFn2D => linf.inverse(value)
      case fit2: FitLine.Impl => fit2.exactXsampleY_y2x(value)
      case _ => x2y match
        case linf: LinearFn2D => linf.inverse(value)
        case fit2: FitLine.Impl => fit2.exactXsampleY_y2x(value)
    inline def intercept: Double = inline x2y match
      case linf: LinearFn2D => linf.intercept
      case fit2: FitLine.Impl => fit2.exactXsampleY_intercept
      case _ => x2y match
        case linf: LinearFn2D => linf.intercept
        case fit2: FitLine.Impl => fit2.exactXsampleY_intercept
    inline def slope: Double = inline x2y match
      case linf: LinearFn2D => linf.slope
      case fit2: FitLine.Impl => fit2.exactXsampleY_slope
      case _ => x2y match
        case linf: LinearFn2D => linf.slope
        case fit2: FitLine.Impl => fit2.exactXsampleY_slope

    inline def mirror: kse.maths.fitting.Y2X = inline x2y match
      case linf: LinearFn2D => Y2X.wrap(linf.mirror)
      case fit2: FitLine.Impl => Y2X.wrap(fit2)
      case _ => x2y match
        case linf: LinearFn2D => Y2X.wrap(linf.mirror)
        case fit2: FitLine.Impl => Y2X.wrap(fit2)

    inline def rsq: Double = inline x2y match
      case linf: LinearFn2D => Double.NaN
      case fit2: FitLine.Impl => fit2.exactXsampleY_rsq
      case _ => x2y match
        case linf: LinearFn2D => Double.NaN
        case fit2: FitLine.Impl => fit2.exactXsampleY_rsq
    inline def offsetError: Double = inline x2y match
      case linf: LinearFn2D => Double.NaN
      case fit2: FitLine.Impl => fit2.exactXsampleY_offsetError
      case _ => x2y match
        case linf: LinearFn2D => Double.NaN
        case fit2: FitLine.Impl => fit2.exactXsampleY_offsetError
    inline def slopeError: Double = inline x2y match
      case linf: LinearFn2D => Double.NaN
      case fit2: FitLine.Impl => fit2.exactXsampleY_slopeError
      case _ => x2y match
        case linf: LinearFn2D => Double.NaN
        case fit2: FitLine.Impl => fit2.exactXsampleY_slopeError
    inline def pm(value: Double): PlusMinus = inline x2y match
      case linf: LinearFn2D => PlusMinus(linf(value).toFloat, Float.NaN)
      case fit2: FitLine.Impl => fit2.exactXsampleY_pm(value)
      case _ => x2y match
        case linf: LinearFn2D => PlusMinus(linf(value).toFloat, Float.NaN)
        case fit2: FitLine.Impl => fit2.exactXsampleY_pm(value)
    inline def pm(value: PlusMinus): PlusMinus = inline x2y match
      case linf: LinearFn2D => linf.pm(value)
      case fit2: FitLine.Impl => fit2.exactXsampleY_pm(value)
      case _ => x2y match
        case linf: LinearFn2D => linf.pm(value)
        case fit2: FitLine.Impl => fit2.exactXsampleY_pm(value)

    inline def toLinFn: LinearFn2D = inline x2y match
      case linf: LinearFn2D => linf
      case fit2: FitLine.Impl => LinearFn2D(fit2.exactXsampleY_intercept, fit2.exactXsampleY_slope)
      case _ => x2y match
        case linf: LinearFn2D => linf
        case fit2: FitLine.Impl => LinearFn2D(fit2.exactXsampleY_intercept, fit2.exactXsampleY_slope)

    inline def toLine2D: Line2D = inline x2y match
      case linf: LinearFn2D =>
        val l = (1.0 + linf.slope*linf.slope).sqrt
        Line2D.Immutable(0, linf.intercept, 1.0/l, linf.slope/l)
      case fit2: FitLine.Impl => fit2.line.immutable
      case _ => x2y match        
        case linf: LinearFn2D =>
          val l = (1.0 + linf.slope*linf.slope).sqrt
          Line2D.Immutable(0, linf.intercept, 1.0/l, linf.slope/l)
        case fit2: FitLine.Impl => fit2.line.immutable
  }
}

opaque type Y2X = LinearFn2D | FitLine.Impl
object Y2X {
  inline def wrap(linf: LinearFn2D): Y2X = linf
  inline def wrap(fit2: FitLine.Impl): Y2X = fit2

  extension (y2x: Y2X) {
    inline def underlying: LinearFn2D | FitLine.Impl = y2x
  }

  extension (y2x: kse.maths.fitting.Y2X) {
    inline def apply(value: Double): Double = inline y2x match
      case linf: LinearFn2D => linf(value)
      case fit2: FitLine.Impl => fit2.exactYsampleX_y2x(value)
      case _ => y2x match
        case linf: LinearFn2D => linf(value)
        case fit2: FitLine.Impl => fit2.exactYsampleX_y2x(value)
    inline def inverse(value: Double): Double = inline y2x match
      case linf: LinearFn2D => linf.inverse(value)
      case fit2: FitLine.Impl => fit2.exactYsampleX_x2y(value)
      case _ => y2x match
        case linf: LinearFn2D => linf.inverse(value)
        case fit2: FitLine.Impl => fit2.exactYsampleX_x2y(value)
    inline def intercept: Double = inline y2x match
      case linf: LinearFn2D => linf.intercept
      case fit2: FitLine.Impl => fit2.exactYsampleX_intercept
      case _ => y2x match
        case linf: LinearFn2D => linf.intercept
        case fit2: FitLine.Impl => fit2.exactYsampleX_intercept
    inline def slope: Double = inline y2x match
      case linf: LinearFn2D => linf.slope
      case fit2: FitLine.Impl => fit2.exactYsampleX_slope
      case _ => y2x match
        case linf: LinearFn2D => linf.slope
        case fit2: FitLine.Impl => fit2.exactYsampleX_slope

    inline def mirror: kse.maths.fitting.X2Y = inline y2x match
      case linf: LinearFn2D => X2Y.wrap(linf.mirror)
      case fit2: FitLine.Impl => X2Y.wrap(fit2)
      case _ => y2x match
        case linf: LinearFn2D => X2Y.wrap(linf.mirror)
        case fit2: FitLine.Impl => X2Y.wrap(fit2)

    inline def rsq: Double = inline y2x match
      case linf: LinearFn2D => Double.NaN
      case fit2: FitLine.Impl => fit2.exactYsampleX_rsq
      case _ => y2x match
        case linf: LinearFn2D => Double.NaN
        case fit2: FitLine.Impl => fit2.exactYsampleX_rsq
    inline def offsetError: Double = inline y2x match
      case linf: LinearFn2D => Double.NaN
      case fit2: FitLine.Impl => fit2.exactYsampleX_offsetError
      case _ => y2x match
        case linf: LinearFn2D => Double.NaN
        case fit2: FitLine.Impl => fit2.exactYsampleX_offsetError
    inline def slopeError: Double = inline y2x match
      case linf: LinearFn2D => Double.NaN
      case fit2: FitLine.Impl => fit2.exactYsampleX_slopeError
      case _ => y2x match
        case linf: LinearFn2D => Double.NaN
        case fit2: FitLine.Impl => fit2.exactYsampleX_slopeError
    inline def pm(value: Double): PlusMinus = inline y2x match
      case linf: LinearFn2D => PlusMinus(linf(value).toFloat, Float.NaN)
      case fit2: FitLine.Impl => fit2.exactYsampleX_pm(value)
      case _ => y2x match
        case linf: LinearFn2D => PlusMinus(linf(value).toFloat, Float.NaN)
        case fit2: FitLine.Impl => fit2.exactYsampleX_pm(value)
    inline def pm(value: PlusMinus): PlusMinus = inline y2x match
      case linf: LinearFn2D => linf.pm(value)
      case fit2: FitLine.Impl => fit2.exactYsampleX_pm(value)
      case _ => y2x match
        case linf: LinearFn2D => linf.pm(value)
        case fit2: FitLine.Impl => fit2.exactYsampleX_pm(value)     

    inline def toLinFn: LinearFn2D = inline y2x match
      case linf: LinearFn2D => linf
      case fit2: FitLine.Impl => LinearFn2D(fit2.exactYsampleX_intercept, fit2.exactYsampleX_slope)
      case _ => y2x match
        case linf: LinearFn2D => linf
        case fit2: FitLine.Impl => LinearFn2D(fit2.exactYsampleX_intercept, fit2.exactYsampleX_slope)

    inline def toLine2D: Line2D = inline y2x match
      case linf: LinearFn2D =>
        val l = (1.0 + linf.slope*linf.slope).sqrt
        Line2D.Immutable(linf.intercept, 0, linf.slope/l, 1.0/l)
      case fit2: FitLine.Impl => fit2.line.immutable
      case _ => y2x match        
        case linf: LinearFn2D =>
          val l = (1.0 + linf.slope*linf.slope).sqrt
          Line2D.Immutable(linf.intercept, 0, linf.slope/l, 1.0/l)
        case fit2: FitLine.Impl => fit2.line.immutable
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
  def +=(x: Double, y: Double): Unit
  final inline def +=(v: Vc): Unit = this.+=(v.x, v.y)

  def -=(x: Double, y: Double): Unit
  final inline def -=(v: Vc): Unit = this.-=(v.x, v.y)

  final inline def ++=(xs: Array[Double], ys: Array[Double]): Unit =
    if xs.length != ys.length then throw new IllegalArgumentException(s"Array length mismatch: ${xs.length} vs ${ys.length}")
    addSegment(xs, 0)(ys, 0)(xs.length)

  final inline def ++=(vs: Array[Vc]): Unit =
    addRange(vs, 0, vs.length)

  def ++=(xs: IterableOnce[Double], ys: IterableOnce[Double]): Unit =
    val i = xs.iterator
    val j = ys.iterator
    while i.hasNext && j.hasNext do
      this += (i.next(), j.next())

  def ++=(vs: IterableOnce[Vc]): Unit =
    val i = vs.iterator
    while i.hasNext do
      this += i.next()

  inline def addWith[A](a: Array[A])(inline fx: A => Double, inline fy: A => Double): Unit =
    var i = 0
    while i < a.length do
      val ai = a(i)
      this += (fx(ai), fy(ai))
      i += 1

  inline def addWith[A](it: IterableOnce[A])(inline fx: A => Double, inline fy: A => Double): Unit =
    val i = it.iterator
    while i.hasNext do
      val a = i.next()
      this += (fx(a), fy(a))

  def addSegment(xs: Array[Double], i0: Int)(ys: Array[Double], j0: Int)(m: Int): Unit

  def addRange(xs: Array[Double], i0: Int, iN: Int)(ys: Array[Double], j0: Int, jN: Int): Unit =
    var mismatch = false
    if iN >= i0 then
      val m = iN - i0
      if jN - j0 != m then mismatch = true
      else if i0 < 0 then throw new ArrayIndexOutOfBoundsException(s"Index $i0")
      else addSegment(xs, i0)(ys, j0)(m)
    else if jN >= j0 then mismatch = true
    if mismatch then throw new IllegalArgumentException(s"Range length mismatch: ${iN.toLong - i0.toLong} vs ${jN.toLong - j0.toLong}")

  inline def addRange(xs: Array[Double], i0: Int, iN: Int)(ys: Array[Double], inline yrg: Rg): Unit =
    val jv = Iv of yrg
    addRange(xs, i0, iN)(ys, jv.i0, jv.iN)
  inline def addRange(xs: Array[Double], i0: Int, iN: Int)(ys: Array[Double], inline yv: Iv.X): Unit =
    val jv = yv of ys
    addRange(xs, i0, iN)(ys, jv.i0, jv.iN)
  inline def addRange(xs: Array[Double], inline xrg: Rg)(ys: Array[Double], j0: Int, jN: Int): Unit =
    val iv = Iv of xrg
    addRange(xs, iv.i0, iv.iN)(ys, j0, jN)
  inline def addRange(xs: Array[Double], inline xrg: Rg)(ys: Array[Double], inline yrg: Rg): Unit =
    val iv = Iv of xrg
    val jv = Iv of yrg
    addRange(xs, iv.i0, iv.iN)(ys, jv.i0, jv.iN)
  inline def addRange(xs: Array[Double], inline xrg: Rg)(ys: Array[Double], inline yv: Iv.X): Unit =
    val iv = Iv of xrg
    val jv = yv of ys
    addRange(xs, iv.i0, iv.iN)(ys, jv.i0, jv.iN)
  inline def addRange(xs: Array[Double], inline xv: Iv.X)(ys: Array[Double], j0: Int, jN: Int): Unit =
    val iv = xv of xs
    addRange(xs, iv.i0, iv.iN)(ys, j0, jN)
  inline def addRange(xs: Array[Double], inline xv: Iv.X)(ys: Array[Double], inline yrg: Rg): Unit =
    val iv = xv of xs
    val jv = Iv of yrg
    addRange(xs, iv.i0, iv.iN)(ys, jv.i0, jv.iN)
  inline def addRange(xs: Array[Double], inline xv: Iv.X)(ys: Array[Double], inline yv: Iv.X): Unit =
    val iv = xv of xs
    val jv = yv of ys
    addRange(xs, iv.i0, iv.iN)(ys, jv.i0, jv.iN)

  def addRange(vs: Array[Vc], i0: Int, iN: Int): Unit
  
  inline def addRange(vs: Array[Vc], inline rg: Rg): Unit =
    val iv = Iv of rg
    addRange(vs, iv.i0, iv.iN)
  inline def addRange(vs: Array[Vc], inline v: Iv.X): Unit =
    val iv = v of vs
    addRange(vs, iv.i0, iv.iN)

  inline def addRangeWith[A](a: Array[A], i0: Int, iN: Int)(inline fx: A => Double, inline fy: A => Double): Unit =
    if iN > i0 then
      if i0 < 0 then throw new ArrayIndexOutOfBoundsException(s"Index $i0")
      var i = i0
      while i < iN do
        val ai = a(i)
        this += (fx(ai), fy(ai))
        i += 1
  inline def addRangeWith[A](a: Array[A], inline rg: Rg)(inline fx: A => Double, inline fy: A => Double): Unit =
    val iv = Iv of rg
    addRangeWith(a, iv.i0, iv.iN)(fx, fy)
  inline def addRangeWith[A](a: Array[A], inline v: Iv.X)(inline fx: A => Double, inline fy: A => Double): Unit =
    val iv = v of a
    addRangeWith(a, iv.i0, iv.iN)(fx, fy)

  def reset(): Unit
}


sealed abstract class FitLine() extends Fit2D() {
  def samples: Long

  def x2y: X2Y
  def y2x: Y2X
  def line: Line2D

  def estX: Est
  def estY: Est
  def mutableCopy: FitLine
}
object FitLine {
  final class Impl() extends FitLine() with Line2D {
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

    def mutableCopy: FitLine.Impl =
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

    def addRange(vs: Array[Vc], i0: Int, iN: Int): Unit =
      vs.use(i0, iN){ this += _ }

    def addSegment(xs: Array[Double], i0: Int)(ys: Array[Double], j0: Int)(m: Int): Unit =
      var i = i0
      var j = j0
      var k = m
      val n0 = n
      while k > 0 do
        this += (xs(i), ys(j))
        i += 1
        j += 1
        k -= 1
      if n > n0 then cached = 0

    override def toString = s"Fit centered at [$cx, $cy], n=$n"
  }
}


/** Theil-Sen robust linear fit: the slope is the median of all pairwise slopes, which makes it
  * insensitive to outliers (breakdown point ~29%) and free of Gaussian-noise assumptions.  The
  * confidence interval on the slope is the distribution-free rank-based interval (Sen 1968), using
  * the normal approximation to the distribution of Kendall's S with tie corrections.  The intercept
  * is `median(y) - slope*median(x)`.
  *
  * Computation is explicit over all `n(n-1)/2` pairs, so cost is O(n^2) in time and memory; intended
  * for modest sample sizes, not for very large data sets.
  */
object TheilSen {
  /** The result of a Theil-Sen fit: a robust line plus a distribution-free interval on the slope. */
  final class Fit(
    val slope: Double,
    val intercept: Double,
    /** Lower end of the slope confidence interval (at confidence `conf`). */
    val slopeLower: Double,
    /** Upper end of the slope confidence interval (at confidence `conf`). */
    val slopeUpper: Double,
    /** Number of finite data points used. */
    val n: Int,
    /** Number of valid (non-vertical) pairwise slopes the median was taken over. */
    val pairs: Int,
    /** Confidence level used for `slopeLower`/`slopeUpper` (e.g. 0.95). */
    val conf: Double
  ) {
    /** Predict y at a given x along the fitted line. */
    def x2y(x: Double): Double = slope * x + intercept

    /** A symmetric Gaussian-equivalent standard error for the slope, derived from the rank-based
      * interval as half-width / z, where z is the normal quantile at the interval's confidence.
      * Useful for downstream significance tests that assume approximate normality.
      */
    def slopeSem: Double =
      val z = NumericFunctions.icdfNormal(0.5 * (1 + conf))
      if z > 0 && (slopeUpper - slopeLower).finite then (slopeUpper - slopeLower) / (2 * z)
      else Double.NaN

    /** The slope with its Gaussian-equivalent standard error. */
    def slopePm: PlusMinus = slope.toFloat +- slopeSem.toFloat

    override def toString =
      f"TheilSen: y = $slope%.5g x + $intercept%.5g  (slope ${conf*100}%.0f%% CI [$slopeLower%.5g, $slopeUpper%.5g], n=$n)"
  }

  // Sum of k(k-1)(2k+5) over the sizes k>=2 of equal-value runs in an ascending-sorted array.
  private def tieCorrection(sorted: Array[Double]): Double =
    var total = 0.0
    var i = 0
    while i < sorted.length do
      var j = i + 1
      while j < sorted.length && sorted(j) == sorted(i) do j += 1
      val k = (j - i).toDouble
      if k >= 2 then total += k * (k - 1) * (2 * k + 5)
      i = j
    total

  /** Theil-Sen fit over the finite points (xs(i), ys(i)) for i in [i0, iN), with a slope confidence
    * interval at level `conf` (default 0.95).  Pairs with equal x, or with any non-finite coordinate,
    * are skipped.  Returns a `Fit` whose slope/intercept are NaN if fewer than two usable points remain.
    */
  def fit(xs: Array[Double], ys: Array[Double], i0: Int, iN: Int, conf: Double = 0.95): Fit =
    val cc = if conf <= 0 || conf >= 1 then 0.95 else conf
    // Gather the finite points into compact x/y buffers.
    val cap = if iN > i0 then iN - i0 else 0
    val fx = new Array[Double](cap)
    val fy = new Array[Double](cap)
    var n = 0
    var i = i0
    while i < iN do
      val x = xs(i)
      val y = ys(i)
      if x.finite && y.finite then
        fx(n) = x
        fy(n) = y
        n += 1
      i += 1
    if n < 2 then return new Fit(Double.NaN, Double.NaN, Double.NaN, Double.NaN, n, 0, cc)

    // All pairwise slopes (skipping vertical pairs).
    val slopes = new Array[Double](n * (n - 1) / 2)
    var p = 0
    i = 0
    while i < n do
      var j = i + 1
      while j < n do
        val dx = fx(j) - fx(i)
        if dx != 0.0 then
          slopes(p) = (fy(j) - fy(i)) / dx
          p += 1
        j += 1
      i += 1
    if p == 0 then return new Fit(Double.NaN, Double.NaN, Double.NaN, Double.NaN, n, 0, cc)

    val ss = if p == slopes.length then slopes else java.util.Arrays.copyOf(slopes, p)
    java.util.Arrays.sort(ss)
    val medslope = Quantile.ofSorted(ss, 0, p)(0.5)

    // Intercept: median(y) - slope * median(x).
    val sx = java.util.Arrays.copyOf(fx, n); java.util.Arrays.sort(sx)
    val sy = java.util.Arrays.copyOf(fy, n); java.util.Arrays.sort(sy)
    val medinter = Quantile.ofSorted(sy, 0, n)(0.5) - medslope * Quantile.ofSorted(sx, 0, n)(0.5)

    // Rank-based confidence interval on the slope (Sen 1968, normal approximation with tie corrections).
    val nn = n.toDouble
    val sigsq = (nn * (nn - 1) * (2 * nn + 5) - tieCorrection(sx) - tieCorrection(sy)) / 18.0
    val sigma = if sigsq > 0 then sigsq.sqrt else 0.0
    val z = NumericFunctions.icdfNormal(0.5 * (1 - cc))   // negative
    val ru = jm.min(jm.round((p - z * sigma) / 2.0).toInt, p - 1)
    val rl = jm.max(jm.round((p + z * sigma) / 2.0).toInt - 1, 0)
    new Fit(medslope, medinter, ss(rl), ss(ru), n, p, cc)

  /** Theil-Sen fit over all finite points, with a slope confidence interval at level `conf`. */
  inline def fit(xs: Array[Double], ys: Array[Double]): Fit = fit(xs, ys, 0, jm.min(xs.length, ys.length), 0.95)

  /** Theil-Sen fit over all finite points, with a slope confidence interval at the given level. */
  inline def fit(xs: Array[Double], ys: Array[Double], conf: Double): Fit = fit(xs, ys, 0, jm.min(xs.length, ys.length), conf)
}


/** A circle with center `(x, y)` and radius `r`. */
final case class Circle2D(x: Double, y: Double, r: Double) {
  inline def c: Vc = Vc.D(x, y)

  /** Signed radial miss distance of a point: negative inside the circle, positive outside. */
  def radialError(px: Double, py: Double): Double =
    jm.sqrt((px - x).sq + (py - y).sq) - r
  inline def radialError(v: Vc): Double = radialError(v.x, v.y)

  /** Squared radial miss distance of a point. */
  def sqError(px: Double, py: Double): Double = radialError(px, py).sq
  inline def sqError(v: Vc): Double = sqError(v.x, v.y)

  /** Angular position of a point as seen from the center, in radians from the +x axis. */
  def arcCoord(px: Double, py: Double): Double = jm.atan2(py - y, px - x)
  inline def arcCoord(v: Vc): Double = arcCoord(v.x, v.y)
}

sealed abstract class FitCirc() extends Fit2D() {
  def samples: Long

  /** The best-fit circle.  Radius (and center) are infinite or NaN if the data
    * is degenerate (collinear, coincident, or fewer than three points).
    */
  def circle: Circle2D

  /** The mean squared radial deviation of the data from the fit circle (the generalized
    * eigenvalue of the fit); NaN if there are fewer than three points.  Near-perfect fits
    * may report values a rounding error below zero.
    */
  def mse: Double

  def estX: Est
  def estY: Est
  def mutableCopy: FitCirc
}
object FitCirc {
  def apply(): FitCirc = new Impl()

  /** Circle fitting with the Hyper algebraic fit of Al-Sharadqah & Chernov, Electronic
    * Journal of Statistics 3:886-911 (2009): among non-iterative moment-based fits it is
    * uniquely unbiased to second order, with leading-order variance at the theoretical
    * (KCR) bound.  The generalized eigenproblem is solved analytically: data is kept as
    * centered moments (so the characteristic quartic has no cubic term), the eigenvalue
    * comes from `Roots.quartic`, and the eigenvector from cross products of the reduced
    * 3x3 system's rows.  Moments are rescaled to unit RMS radius before solving, so
    * conditioning does not depend on the scale of the data.
    */
  final class Impl() extends FitCirc() {
    var n = 0L
    var Ox = 0.0
    var Oy = 0.0
    var Dxx = 0.0
    var Dxy = 0.0
    var Dyy = 0.0
    var Dxq = 0.0
    var Dyq = 0.0
    var Dqq = 0.0

    private var cached = 0

    private val qroots = new Array[Double](4)

    private var fcx = Double.NaN
    private var fcy = Double.NaN
    private var fcr = Double.NaN
    private var feta = Double.NaN

    private def fitImpl(): Unit =
      if (cached & 1) != 1 then
        cached |= 1
        val scl2 = (Dxx + Dyy)/n   // mean squared distance from the centroid
        if n < 3 || !(scl2 > 0) then
          fcx = Double.NaN
          fcy = Double.NaN
          fcr = Double.NaN
          feta = Double.NaN
        else
          // Moments normalized by count and rescaled to unit RMS radius (q = x^2 + y^2)
          val ni = 1.0/n
          val u1 = 1.0/scl2
          val us = jm.sqrt(u1)
          val mxx = Dxx*ni*u1
          val myy = Dyy*ni*u1
          val mxy = Dxy*ni*u1
          val mxq = Dxq*ni*u1*us
          val myq = Dyq*ni*u1*us
          val mqq = Dqq*ni*u1*u1
          val mq  = mxx + myy   // == 1 up to rounding, kept as computed for consistency
          // Characteristic quartic of the Hyper pencil in centered moments (cubic term
          // vanishes identically): h^4 + c2 h^2 + c1 h + c0 = 0
          val c2 = -mxy.sq - 0.25*(3*mxx.sq + 2*mxx*myy + 3*myy.sq + mqq)
          val c1 = 0.25*(mq*(mqq - (mxx - myy).sq) - mxq.sq - myq.sq) - mxy.sq*mq
          val c0 = 0.25*(myq.sq*mxx - 2*mxy*mxq*myq + mxq.sq*myy + (mxy.sq - mxx*myy)*(mqq - mq.sq))
          val k = Roots.quartic(c0, c1, c2, 0.0, 1.0, qroots, 0)
          // Exactly one eigenvalue is negative; the fit is the smallest of the others
          var h = if k > 0 then qroots(0) else Double.NaN
          if k > 1 && h < 0 then h = qroots(1)
          feta = h*scl2
          // Null vector of the reduced 3x3 pencil: the largest cross product of row pairs,
          // rows r1 = (g, mxq, myq), r2 = (mxq, xh, mxy), r3 = (myq, mxy, yh)
          val g = mqq - mq.sq - 4*h*(mq + h)
          val xh = mxx - h
          val yh = myy - h
          var va = xh*yh - mxy.sq
          var vb = mxy*myq - mxq*yh
          var vc = mxq*mxy - xh*myq
          var norm = va.sq + vb.sq + vc.sq
          var t1 = mxq*yh - myq*mxy
          var t2 = myq.sq - g*yh
          var t3 = g*mxy - mxq*myq
          var best = t1.sq + t2.sq + t3.sq
          if best > norm then
            va = t1; vb = t2; vc = t3
            norm = best
          t1 = mxq*mxy - myq*xh
          t2 = myq*mxq - g*mxy
          t3 = g*xh - mxq.sq
          best = t1.sq + t2.sq + t3.sq
          if best > norm then
            va = t1; vb = t2; vc = t3
          // Circle a*q + b*x + c*y + d = 0, with d pinned by the pencil's last row
          val vd = (2*h - mq)*va
          val s = 0.5/va
          val sig = jm.sqrt(scl2)
          fcx = Ox - vb*s*sig
          fcy = Oy - vc*s*sig
          fcr = jm.sqrt(vb.sq + vc.sq - 4*va*vd) * jm.abs(s) * sig

    def samples: Long = n

    def circle: Circle2D =
      fitImpl()
      Circle2D(fcx, fcy, fcr)

    def mse: Double =
      fitImpl()
      feta

    def estX: Est = Est.M(n.toDouble, Ox, Dxx)
    def estY: Est = Est.M(n.toDouble, Oy, Dyy)

    def mutableCopy: FitCirc.Impl =
      val fc = new Impl()
      fc.n = n
      fc.Ox = Ox
      fc.Oy = Oy
      fc.Dxx = Dxx
      fc.Dyy = Dyy
      fc.Dxy = Dxy
      fc.Dxq = Dxq
      fc.Dyq = Dyq
      fc.Dqq = Dqq
      fc

    def +=(x: Double, y: Double): Unit = if !(x + y).nan then
      cached = 0
      val inv = 1.0/(n+1)
      val dx = (x - Ox) * inv
      val dy = (y - Oy) * inv
      val xp = n * dx
      val yp = n * dy
      val xx = dx * dx
      val yy = dy * dy
      val dq = xx + yy
      val qp = xp*xp + yp*yp
      Dqq += qp*qp + n*(dq * dq) - 4*dx*Dxq - 4*dy*Dyq + 8*dx*dy*Dxy + (6*xx + 2*yy)*Dxx + (6*yy + 2*xx)*Dyy
      Dyq += yp*qp - n*(dy * dq) - 2*dx*Dxy - 3*dy*Dyy - dy*Dxx
      Dxq += xp*qp - n*(dx * dq) - 2*dy*Dxy - 3*dx*Dxx - dx*Dyy
      Dxy += xp*yp + n*(dx * dy)
      Dyy += yp*yp + n*yy
      Dxx += xp*xp + n*xx
      Oy  += dy
      Ox  += dx
      n   += 1

    def -=(x: Double, y: Double): Unit = if !(x + y).nan then
      cached = 0
      n -= 1
      if n <= 0 then reset()
      else
        val inv = 1.0/n
        val dx = (x - Ox) * inv
        val dy = (y - Oy) * inv
        val xp = n * dx
        val yp = n * dy
        val xx = dx * dx
        val yy = dy * dy
        val dq = xx + yy
        val qp = xp*xp + yp*yp
        Ox  -= dx
        Oy  -= dy
        Dxx -= xp*xp + n*xx
        Dyy -= yp*yp + n*yy
        Dxy -= xp*yp + n*(dx * dy)
        Dxq -= xp*qp - n*(dx * dq) - 2*dy*Dxy - 3*dx*Dxx - dx*Dyy
        Dyq -= yp*qp - n*(dy * dq) - 2*dx*Dxy - 3*dy*Dyy - dy*Dxx
        Dqq -= qp*qp + n*(dq * dq) - 4*dx*Dxq - 4*dy*Dyq + 8*dx*dy*Dxy + (6*xx + 2*yy)*Dxx + (6*yy + 2*xx)*Dyy

    def addSegment(xs: Array[Double], i0: Int)(ys: Array[Double], j0: Int)(m: Int): Unit =
      if m > 0 then
        cached = 0
        var x0 = 0.0
        var y0 = 0.0
        m.visit: k =>
          x0 += xs(i0 + k)
          y0 += ys(j0 + k)
        x0 /= m
        y0 /= m
        var dxx = 0.0
        var dyy = 0.0
        var dxy = 0.0
        var dxq = 0.0
        var dyq = 0.0
        var dqq = 0.0
        m.visit: k =>
          val x = xs(i0 + k) - x0
          val y = ys(j0 + k) - y0
          val q = x*x + y*y
          dxx += x*x
          dyy += y*y
          dxy += x*y
          dxq += x*q
          dyq += y*q
          dqq += q*q
        combine(m, x0, y0, dxx, dyy, dxy, dxq, dyq, dqq)

    def addRange(vs: Array[Vc], i0: Int, iN: Int): Unit =
      if iN > i0 then
        cached = 0
        var x0 = 0.0
        var y0 = 0.0
        val m = iN - i0
        vs.visit(i0, iN): (v, _) =>
          x0 += v.x
          y0 += v.y
        x0 /= m
        y0 /= m
        var dxx = 0.0
        var dyy = 0.0
        var dxy = 0.0
        var dxq = 0.0
        var dyq = 0.0
        var dqq = 0.0
        vs.visit(i0, iN): (v, _) =>
          val x = v.x - x0
          val y = v.y - y0
          val q = x*x + y*y
          dxx += x*x
          dyy += y*y
          dxy += x*y
          dxq += x*q
          dyq += y*q
          dqq += q*q
        combine(m, x0, y0, dxx, dyy, dxy, dxq, dyq, dqq)

    private def combine(m: Int, x0: Double, y0: Double, dxx: Double, dyy: Double, dxy: Double, dxq: Double, dyq: Double, dqq: Double): Unit =
      if n == 0 then
        n = m
        Ox = x0
        Oy = y0
        Dxx = dxx
        Dyy = dyy
        Dxy = dxy
        Dxq = dxq
        Dyq = dyq
        Dqq = dqq
      else
        val nox = (n*Ox + m*x0)/(n + m)
        val noy = (n*Oy + m*y0)/(n + m)
        val dax = nox - Ox
        val day = noy - Oy
        val daq = dax*dax + day*day
        val dbx = nox - x0
        val dby = noy - y0
        val dbq = dbx*dbx + dby*dby
        val nxx = Dxx + n*dax*dax + dxx + m*dbx*dbx
        val nyy = Dyy + n*day*day + dyy + m*dby*dby
        val nxy = Dxy + n*dax*day + dxy + m*dbx*dby
        val nxq = Dxq - 3*dax*Dxx - 2*day*Dxy - dax*Dyy - n*dax*daq
                + dxq - 3*dbx*dxx - 2*dby*dxy - dbx*dyy - m*dbx*dbq
        val nyq = Dyq - 3*day*Dyy - 2*dax*Dxy - day*Dxx - n*day*daq
                + dyq - 3*dby*dyy - 2*dbx*dxy - dby*dxx - m*dby*dbq
        val nqq = Dqq - 4*(dax*Dxq + day*Dyq) + Dxx*(4*dax*dax + 2*daq) + Dyy*(4*day*day + 2*daq) + 8*dax*day*Dxy + n*daq*daq
                + dqq - 4*(dbx*dxq + dby*dyq) + dxx*(4*dbx*dbx + 2*dbq) + dyy*(4*dby*dby + 2*dbq) + 8*dbx*dby*dxy + m*dbq*dbq
        n += m
        Ox = nox
        Oy = noy
        Dxx = nxx 
        Dyy = nyy
        Dxy = nxy
        Dxq = nxq
        Dyq = nyq
        Dqq = nqq

    def reset(): Unit =
      cached = 0
      n = 0
      Ox = 0
      Oy = 0
      Dxx = 0
      Dyy = 0
      Dxy = 0
      Dxq = 0
      Dyq = 0
      Dqq = 0

    override def toString = s"Circle fit centered at [$Ox, $Oy], n=$n"
  }
}

