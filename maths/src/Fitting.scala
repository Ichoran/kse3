// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2023 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC

package kse.maths.fitting

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
}
object Line2D {
  final case class Immutable(cx: Double, cy: Double, ux: Double, uy: Double) extends Line2D {
    val c = Vc.D(cx, cy)
    val u = Vc.D(ux, uy)
    def theta = math.atan2(uy, ux)

    def centered =
      if cx == 0 && cy == 0 then this
      else Immutable(0, 0, ux, uy)
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
    def estX: Est = Est.M(n, cx, Sxx)
    def estY: Est = Est.M(n, cy, Syy)

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

    def addRange(xs: Array[Double], i0: Int, iN: Int)(ys: Array[Double], j0: Int, jN: Int): Unit =
      val k0 = math.max(0, i0)
      val kN = math.min(xs.length, iN)
      if k0 < kN then
        val l0 = math.max(0, j0)
        val lN = math.min(ys.length, jN)
        if l0 < lN then
          val m = math.min(kN - k0, lN - l0)
          addRangeImpl(xs, k0)(ys, l0)(m)

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


/*


  def alphaX = { if !cached then compute(); myAx }
  def betaX  = { if !cached then compute(); myBx }
  def error  = { if !cached then compute(); myE }
  def onlyBetaX     = if n < 2 then Double.NaN else Stx / Stt
  def onlyBetaError = if n < 2 then Double.NaN else Sxx - Stx*Stx/Stt
  def samples = n
  def meanT = if n < 1 then Double.NaN else St / n
  def meanX = if n < 1 then Double.NaN else Sx / n

  def apply(t: Double): Double = alphaX + betaX*(t - Ot) + Ox
  def xt(t: Double): Double = apply(t)

  def xDeviation(t: Double): Double = sqrt((error/(n-2))*(1.0/n + (n*t - St).sq/(n*(n*Stt - St*St))))

  /** An estimate of the probability that a particular x at a value t comes from the linear relationship
    * fit by this fitter.  If the true (or a superior) estimate of the variance is known, it can
    * substantially improve the accuracy of the estimate especially for small n
    */
  def p(t: Double, x: Double, knownSigmaSq: Double = Double.NaN) =
    if n <= 2 then Double.NaN
    else if knownSigmaSq.nan then
      NumericFunctions.cdfStudentT(n, (x - xt(t))/sqrt((error/(n-2))*(1.0/n + (n*t - St).sq/(n*(n*Stt - St*St)))))
    else
      NumericFunctions.cdfNormal((x - xt(t))/sqrt(knownSigmaSq*(1.0/n + (n*t - St).sq/(n*(n*Stt - St*St)))))

  def xform(in: Array[Double], i0: Int = 0, iN: Int = Int.MaxValue): Unit =
    val iM = math.min(in.length, iN)
    var i = i0
    val aX = alphaX
    val bX = betaX
    while i < iM do
      in(i) = aX + bX*(in(i) - Ot) + Ox
      i += 1



}
object FitTX {
  def apply(ts: Array[Double], xs: Array[Double], i0: Int, iN: Int): FitTX =
    val fit = new FitTX
    if iN > i0 then
      var i = i0
      fit.origin(ts(i), xs(i))
      while i < iN do
        fit += (ts(i), xs(i))
        i += 1
      fit.origin(0, 0)
    fit

  def apply(ts: Array[Double], xs: Array[Double]): FitTX = apply(ts, xs, 0, min(ts.length, xs.length))

  def apply(tf: Int => Double, xf: Int => Double, i0: Int, iN: Int): FitTX =
    val fit = new FitTX
    if iN > i0 then
      var i = i0
      fit.origin(tf(i), xf(i))
      while i < iN do
        fit += (tf(i), xf(i))
        i += 1
      fit.origin(0, 0)
    fit

  def apply(ts: Array[Float], xs: Array[Float], i0: Int, iN: Int): FitTX = apply(i => ts(i), i => xs(i), i0, iN)

  def apply(ts: Array[Float], xs: Array[Float]): FitTX = apply(ts, xs, 0, min(ts.length, xs.length))
}


/** FitTXY performs a ordinary least squares fit of a parameter t against two readouts x and y.
  * We assume t is accurate; this method is not precise if t has error as well as x and y.
  */
final class FitTXY() extends Fit[FitTXY] {
  private var Ot = 0.0
  private var Ox = 0.0
  private var Oy = 0.0

  private var n = 0L
  private var St = 0.0
  private var Stt = 0.0
  private var Sx = 0.0
  private var Sxx = 0.0
  private var Sy = 0.0
  private var Syy = 0.0
  private var Stx = 0.0
  private var Sty = 0.0

  private var cached = false
  private var myAx = 0.0
  private var myAy = 0.0
  private var myBx = 0.0
  private var myBy = 0.0
  private var myE = 0.0


  private def setEverything(
    _Ot: Double, _Ox: Double, _n: Long,
    _St: Double, _Stt: Double,
    _Sx: Double, _Sxx: Double,
    _Sy: Double, _Syy: Double,
    _Stx: Double, _Sty: Double
  ): this.type =
    Ot = _Ot; Ox = _Ox;
    n = _n;
    St = _St; Stt = _Stt;
    Sx = _Sx; Sxx = _Sxx;
    Sy = _Sy; Syy = _Syy;
    Stx = _Stx; Sty = _Sty;
    this

  override def clone(): FitTXY =
    val that = new FitTXY
    that.setEverything(Ot, Ox, n, St, Stt, Sx, Sxx, Sy, Syy, Stx, Sty)


  private def compute(): Unit =
    if n < 2 then
      myAx = Double.NaN
      myAy = Double.NaN
      myBx = Double.NaN
      myBy = Double.NaN
      myE = Double.NaN
    else
      val den = n*Stt - St*St
      myBx = if den != 0 then (n*Stx - St*Sx)/den else Double.NaN
      myBy = if den != 0 then (n*Sty - St*Sy)/den else Double.NaN
      myAx = (Sx - myBx*St) / n
      myAy = (Sy - myBy*St) / n
      myE = (n*(Sxx + Syy) - Sx*Sx - Sy*Sy - (myBx*myBx + myBy*myBy)*den) / n
    cached = true

  private def computeA(): Unit =
    if n < 2 then
      myAx = Double.NaN
      myBx = Double.NaN
    else
      if !cached then compute()
      myAx = (Sx - myBx*St) / n
      myAy 

  def alphaX = { if !cached then compute(); myAx }
  def alphaY = { if !cached then compute(); myAy }
  def betaX  = { if !cached then compute(); myBx }
  def betaY  = { if !cached then compute(); myBy }
  def error  = { if !cached then compute(); myE }
  def onlyBetaX     = if n < 2 then Double.NaN else Stx / Stt
  def onlyBetaY     = if n < 2 then Double.NaN else Sty / Stt
  def onlyBetaError = if n < 2 then Double.NaN else Sxx + Syy - (Stx*Stx + Sty*Sty)/Stt
  def samples = n
  def meanT = if n < 1 then Double.NaN else St / n
  def meanX = if n < 1 then Double.NaN else Sx / n
  def meanY = if n < 1 then Double.NaN else Sy / n

  def apply(t: Double): (Double, Double) = (alphaX + betaX*(t - Ot) + Ox, alphaY + betaY*(t - Ot) + Oy)
  def xt(t: Double): Double = alphaX + betaX*(t - Ot) + Ox
  def yt(t: Double): Double = alphaY + betaY*(t - Ot) + Oy
  def inverse(x: Double, y: Double): Double =
    val xc = x - Ox - alphaX
    val yc = y - Oy - alphaY
    val bX = betaX
    val bY = betaY
    val denom = bX*bX + bY*bY
    if denom == 0 then Double.NaN else (xc*bX + yc*bY)/denom + Ot

  def xform(in: Array[Double], i0: Int = 0, iN: Int = Int.MaxValue): Unit =
    val iM = math.min(in.length, iN)
    var i = i0
    val aX = alphaX
    val bX = betaX
    while i < iM do
      in(i) = aX + bX*(in(i) - Ot) + Ox
      i += 1
  def yform(in: Array[Double], i0: Int = 0, iN: Int = Int.MaxValue): Unit =
    val iM = math.min(in.length, iN)
    var i = i0
    val aY = alphaY
    val bY = betaY
    while i < iM do
      in(i) = aY + bY*(in(i) - Ot) + Ox
      i += 1

  def clear(): Unit =
    cached = false
    n = 0
    St = 0
    Stt = 0
    Sx = 0
    Sxx = 0
    Sy = 0
    Syy = 0
    Stx = 0
    Sty = 0

  def origin(ot: Double, ox: Double, oy:Double): Unit =
    if n > 0 then
      cached = false
      val dt = Ot - ot
      val dx = Ox - ox
      val dy = Oy - oy
      val ndt = n*dt
      val ndx = n*dx
      val ndy = n*dy
      Stt += dt*(2*St + ndt)
      Sxx += dx*(2*Sx + ndx)
      Syy += dy*(2*Sy + ndy)
      Stx += dt*Sx + dx*(St + ndt)
      Sty += dt*Sy + dy*(St + ndt)
      St += ndt
      Sx += ndx
      Sy += ndy
    Ot = ot
    Ox = ox

  def +=(ti: Double, xi: Double, yi: Double): Unit =
    cached = false
    val t = ti - Ot
    val x = xi - Ox
    val y = yi - Oy
    n += 1
    St += t
    Stt += t*t
    Sx += x
    Sxx += x*x
    Sy += y
    Syy += y*y
    Stx += t*x
    Sty += t*y

  def -=(ti: Double, xi: Double, yi: Double): Unit =
    cached = false
    val t = ti - Ot
    val x = xi - Ox
    val y = yi - Oy
    n -= 1
    St -= t
    Stt -= t*t
    Sx -= x
    Sxx -= x*x
    Sy -= y
    Syy -= y*y
    Stx -= t*x
    Sty -= t*y

  override def toString = s"Fit: x = ${betaX} t + ${xt(0)}; y = ${betaY} t + ${yt(0)} (n=$n, rmse=${(sqrt(error)/n)}"
}
object FitTXY {
  def apply(ts: Array[Double], xs: Array[Double], ys: Array[Double], i0: Int, iN: Int): FitTXY =
    val fit = new FitTXY
    if iN > i0 then
      var i = i0
      fit.origin(ts(i), xs(i), ys(i))
      while i < iN do
        fit += (ts(i), xs(i), ys(i))
        i += 1
      fit.origin(0, 0, 0)
    fit

  def apply(ts: Array[Double], xs: Array[Double], ys: Array[Double]): FitTXY = apply(ts, xs, ys, 0, min(ts.length, min(xs.length, ys.length)))

  def apply(tf: Int => Double, xf: Int => Double, yf: Int => Double, i0: Int, iN: Int): FitTXY =
    val fit = new FitTXY
    if iN > i0 then
      var i = i0
      fit.origin(tf(i), xf(i), yf(i))
      while i < iN do
        fit += (tf(i), xf(i), yf(i))
        i += 1
      fit.origin(0, 0, 0)
    fit

  def apply(ts: Array[Float], xs: Array[Float], ys: Array[Float], i0: Int, iN: Int): FitTXY = apply(i => ts(i), i => xs(i), i => ys(i), i0, iN)

  def apply(ts: Array[Float], xs: Array[Float], ys: Array[Float]): FitTXY = apply(ts, xs, ys, 0, min(ts.length, xs.length))
}
*/
