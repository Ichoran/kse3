// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2023 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC

package kse.maths.fitting


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.collection.immutable.{Range => Rg}

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
      val a = i.next
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
  inline def addRangeWith[A](a: Array[A], inline v: Iv | PIv)(inline fx: A => Double, inline fy: A => Double): Unit =
    val iv = Iv.of(v, a)
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
      vs.peek(i0, iN){ this += _ }

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


final case class Circle2D(x: Double, y: Double, r: Double) {}

sealed abstract class FitCirc() extends Fit2D() {
}
object FitCirc {
  def apply(): FitCirc = new Impl()

  /** Circle fitting using algorithm in Al-Sharadqah & Chernov, Euro J Stats 3:886 (2009)
    */
  final class Impl() extends FitCirc() {
    var n = 0
    var Ox = 0.0
    var Oy = 0.0
    var Dxx = 0.0
    var Dxy = 0.0
    var Dyy = 0.0
    var Dxq = 0.0
    var Dyq = 0.0
    var Dqq = 0.0

    private var cached = 0

    private val params = new Array[Double](8)

    private var cx = 0.0
    private var cy = 0.0
    private var r = 0.0
    private var mse = 0.0

    def +=(x: Double, y: Double): Unit =
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

    def -=(x: Double, y: Double): Unit =
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
        vs.peek(i0, iN): v =>
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
        vs.peek(i0, iN): v =>
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
      n = 0
      Ox = 0
      Oy = 0
      Dxx = 0
      Dyy = 0
      Dxy = 0
      Dxq = 0
      Dyq = 0
      Dqq = 0


    override def toString = f"$n\n  $Ox, $Oy\n    $Dxx, $Dyy, $Dxy\n    $Dxq, $Dyq, $Dqq\n"
  }

  /*
  def direct(xs: Array[Double])(ys: Array[Double]): FitCirc =
    if ys.length != xs.length then throw new ArrayIndexOutOfBoundsException(s"Mismatch in paired array lengths: ${xs.length} and ${ys.length}")
    val fc = new FitCirc.Impl()
    if xs.length > 0 then
      var x0 = xs.gather(0.0)()((s, x, _) => s + x)/xs.length
      var y0 = ys.gather(0.0)()((s, y, _) => s + y)/ys.length
      var sxx = 0.0
      var syy = 0.0
      var sxy = 0.0
      var sxq = 0.0
      var syq = 0.0
      var sqq = 0.0
      xs.length.visit: i =>
        val x = xs(i) - x0
        val y = ys(i) - y0
        val q = x*x + y*y
        sxx += x*x
        syy += y*y
        sxy += x*y
        sxq += x*q
        syq += y*q
        sqq += q*q
      fc.n = xs.length
      fc.Ox = x0
      fc.Oy = y0
      fc.Dxx = sxx
      fc.Dyy = syy
      fc.Dxy = sxy
      fc.Dxq = sxq
      fc.Dyq = syq
      fc.Dqq = sqq
    fc
  */
}

/*
object SmallEigensolver {
  def rowReduce(matrix: Array[Double], rowLen: Int, index: Int, order: Array[Int] Or Unit = Alt.unit): Boolean =
    order.foreach{ o =>
      var i = -1
      var v = 0.0
      o.visit(index to End){ (k, j) =>
        val x = matrix(rowLen*k + index)
        if x.abs > v then
          i = j
          x = v
      }
      if i >= 0 && i != index then
        val temp = o(index)
        o(index) = o(i)
        o(i) = temp
    }
    val m = rowLen*order(index) + index
    val x = 1.0/matrix(m)
    if !x.finite || x == 0 then false
    else
      matrix(m) = 1.0
      if m+1 < rowLen then
        val iv = (m+1) to (m+rowLen-index-1)
        matrix.set((m+1) to (m+rowLen-index-1)){ _ * x }
        order.foreachThem{ o =>
        }{ _ =>
        }

    
}
*/



object RealRoots {
  private def roundTowardsZero(a: Double, b: Double)(answer: Double): Double =
    import java.lang.Math.{max, abs, ulp}
    val scale = max(1.0, max(abs(a), abs(b)))
    if abs(answer) >= 100*ulp(scale) then answer
    else 0

  // private def roundTowardsZero(a: Double, b: Double, c: Double)(answer: Double): Double =
  //   import java.lang.Math.{max, abs, ulp}
  //   val scale = max( max(1.0, abs(a)), max(abs(b), abs(c)) )
  //   if abs(answer) >= 100*ulp(scale) then answer
  //   else 0

  private inline def zeroize(a: Double, b: Double)(inline f: (Double, Double) => Double): Double =
    roundTowardsZero(a, b)(f(a, b))

  // private inline def zeroize(a: Double, b: Double, c: Double)(inline f: (Double, Double, Double) => Double): Double =
  //   roundTowardsZero(a, b, c)(f(a, b, c))

  private def one_answer(x0: Double)(result: Array[Double], offset: Int): Int =
    if x0.finite then
      result(offset) = x0
      1
    else 0

  private def two_answers(x0: Double, x1: Double)(result: Array[Double], offset: Int): Int =
    if x0.finite && x1.finite then
      result(offset) = x0
      result(offset + 1) = x1
      2
    else 0

  private def three_answers(x0: Double, x1: Double, x2: Double)(result: Array[Double], offset: Int): Int =
    if x0.finite && x1.finite && x2.finite then
      result(offset) = x0
      result(offset + 1) = x1
      result(offset + 2) = x2
      3
    else 0

  private def four_answers(x0: Double, x1: Double, x2: Double, x3: Double)(result: Array[Double], offset: Int): Int =
    if x0.finite && x1.finite && x2.finite && x3.finite then
      result(offset) = x0
      result(offset + 1) = x1
      result(offset + 2) = x2
      result(offset + 3) = x3
      4
    else 0

  def linear(a0: Double, a1: Double, result: Array[Double], offset: Int = 0): Int =
    if a1 == 0 then 0
    else one_answer(-a0/a1)(result, offset)

  def quadratic(a0: Double, a1: Double, a2: Double, result: Array[Double], offset: Int = 0): Int =
    if a2 == 0 then linear(a0, a1, result, offset)
    else if a0 == 0 then
      val n = linear(a1, a2, result, offset)
      if n == 1 && result(offset) == 0.0 then n
      else
        result(offset + n) = 0.0
        n + 1
    else
      val v2 = zeroize(a1 * a1, 4 * a0 * a2)(_ - _)
      if v2 < 0 then 0
      else if v2 == 0 then one_answer(-a1 / (2*a2))(result, offset)
      else
        val v = math.sqrt(v2)
        two_answers((v - a1)/(2*a2), -(v + a1)/(2*a2))(result, offset)

  def cubic(a0: Double, a1: Double, a2: Double, a3: Double, result: Array[Double], offset: Int = 0): Int =
    import java.lang.Math.{sqrt, cbrt, cos, acos}
    if a3 == 0 then quadratic(a0, a1, a2, result, offset)
    else if a0 == 0 then
      val n = quadratic(a1, a2, a3, result, offset)
      if (n >= 1 && result(offset) == 0) || (n > 1 && result(offset+1) == 0) then n
      else
        result(offset + n) = 0.0
        n + 1
    else
      // Convert to depressed cubic form
      val r = a2/(3.0 * a3)
      val p = zeroize(a1/a3, 3*r*r)(_ - _)
      val q = 2*r*r*r + (3*a0*a3 - a1*a2)/(3*a3*a3)
      println(s"$r $p $q")
      if p == 0 then one_answer(cbrt(q) - r)(result, offset)
      else zeroize(27 * q * q,  4 * p * p * p)(_ + _) match
        case 0 =>
          // Two distinct roots
          val f = q/p
          two_answers(3*f - r, -1.5*f - r)(result, offset)
        case v if v < 0 =>
          // One real root
          val u = sqrt(-(108.0*v))
          one_answer(cbrt(-0.5*q + u) + cbrt(-0.5*q - u) - r)(result, offset)
        case _ =>
          // Three distinct real roots
          val pre = 2*sqrt(-p/3)
          val inv = acos(3*q/(p*pre))/3
          println(s"$pre $inv")
          three_answers(
            pre * cos(inv),
            pre * cos(inv - NumericConstants.TwoThirdsPi),
            pre * cos(inv - NumericConstants.FourThirdsPi)
          )(result, offset)

  /*
  private def simplified_limited_cubic(p0: Double, p1: Double, limit: Double, result: Array[Double], offset: Int = 0): Int =
    val v2 = p0 * p0 -~~- (-(p1 * p1 * p1))
    if v2 > 0 then
      val v = math.sqrt(v2)
      one_answer(math.cbrt(p0 + v) + math.cbrt(p0 - v), result, offset)
    else if v2 == 0 then
      val x0 = 2 * math.sqrt(-p1)
      two_answers(x0, -0.5*x0, result, offset)
    else
      val s = math.sqrt(-p1)
      val t = math.cos(math.acos(p0 / (s*s*s)) / 3.0)
      val x0 = 2 * s * t
      if x0 > limit then one_answer(x0, result, offset)
      else
        val w = NumericConstants.SqrtThree * s * math.sqrt(1.0 - t*t)
        three_answers(x0, -0.5*x0 + w, -0.5*x0 - w, result, offset)

  private def simplified_cubic(h0: Double, h1: Double, result: Array[Double], offset: Int = 0): Int =
    simplified_limited_cubic(h0 / 3.0, h1 * -0.5, Double.PositiveInfinity, result, offset)

  private def cubic_impl(a0: Double, a1: Double, a2: Double, a3: Double, abbreviated: Boolean, result: Array[Double], offset: Int = 0): Int =
    if a3 == 0 then quadratic(a0, a1, a2)
    else if a0 == 0 then
      val n = quadratic(a1, a2, a3)
      if n > 0 || (a1.finite && a2.finite && a3.finite) then
        result(offset + n) = 0.0
        n + 1
      else 0
    else
      var b0 = a0
      var b1 = a1
      var b2 = a2
      if a3 != 1 then
        b0 /= a3
        b1 /= a3
        b2 /= a3
      b2 /= 3.0
      b0 = b2 * (0.5*b1 - b2*b2) - 0.5*b0
      b1 = b1/3.0 - b2*b2

  def cubic(a0: Double, a1: Double, a2: Double, a3: Double, result: Array[Double], offset: Int = 0): Int =
    cubic_impl(a0, a1, a3, a3, false, result, offset)
  */
}

