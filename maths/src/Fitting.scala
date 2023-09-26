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
  }
}



final case class LinearFn2D(val intercept: Double, val slope: Double) extends (Double => Double) {
  def apply(value: Double): Double = value*slope + intercept
  def inverse(value: Double): Double = (value - intercept)/slope
  def mirror: LinearFn2D = LinearFn2D(-intercept/slope, 1/slope)
}


sealed trait Line2D {
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
  final case class Immutable(cx: Double, cy: Double, ux: Double, uy: Double) {
    def theta = math.atan2(uy, ux)

    def centered =
      if cx == 0 && cy == 0 then this
      else Immutable(0, 0, ux, uy)
  }
}


sealed trait Fit2D {
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
  final class Impl() extends Fit2D with Line2D {
    var n = 0L
    var cx = 0.0
    var cy = 0.0

    var Sxx = 0.0
    var Sxy = 0.0
    var Syy = 0.0

    private var cached = 0

    private var xb = Double.NaN
    private var xm = Double.NaN
    private var xe = Double.NaN
    private var ym = Double.NaN
    private var yb = Double.NaN
    private var ye = Double.NaN
    private var ex = Double.NaN
    private var ey = Double.NaN

    private def exactX(): Unit =
      if (cached & 1) != 1 then
        if n >= 2 then
          val den = (Sxx - n*cx*cx)
          xm = if den != 0 then (Sxy - n*cx*cy)/den else Double.NaN
          xb = cy - xm*cx
          xe = Sxx - n*cx*cx - xm*xm*den
        else
          xb = Double.NaN
          xm = Double.NaN
          xe = Double.NaN
        cached |= 1
    private def exactY(): Unit =
      if (cached & 2) != 2 then
        if n >= 2 then
          val den = (Syy - n*cy*cy)
          ym = if den != 0 then (Sxy - n*cx*cy)/den else Double.NaN
          yb = cx - ym*cy
          ye = Syy - n*cy*cy - ym*ym*den
        else
          yb = Double.NaN
          ym = Double.NaN
          ye = Double.NaN
        cached |= 2
    private def bestFit(): Unit =
      if (cached & 4) != 4 then
        if n >= 2 && (Sxy != 0 || Sxx != Syy) then
          if Sxx == Syy then
            ex = 1
            ey = 0
          else
            val tn = 2*Sxy/(Sxx - Sxy)
            val tnsq = tn*tn
            if tnsq >= 1 then
              val cosq = 1/(1 + tnsq)
              ex = cosq.sqrt
              ey = (1-cosq).sqrt
              if tn < 0 then ey = -ey
            else
              val sisq = tnsq/(tnsq+1)
              ex = (1-sisq).sqrt
              ey = sisq.sqrt
              if tn < 0 then ey = -ey
            val ssq = ex*ex + ey*ey
            if ssq > 1 then
              val len = ssq.sqrt
              ex /= len
              ey /= len
        else
          ex = Double.NaN
          ey = Double.NaN
        cached |= 4

    def samples = n

    def x2y: X2Y = X2Y.wrap(this)
    def y2x: Y2X = Y2X.wrap(this)
    def line: Line2D = this
    def estX: Est = Est.M(n, cx, Sxx)
    def estY: Est = Est.M(n, cy, Syy)

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
      math.atan2(ey, ex)

    def exactXsampleY_x2y(value: Double): Double =
      exactX()
      xm*value + xb
    def exactXsampleY_y2x(value: Double): Double =
      exactX()
      (value - xb)/xm

    def exactYsampleX_y2x(value: Double): Double =
      exactY()
      ym*value + yb
    def exactYsampleX_x2y(value: Double): Double =
      exactY()
      (value - yb)/ym

    /*
    def errSumSq =
      if !cached then compute()
      cE

    def rmse = errSumSq.sqrt / n

    def apply(t: Double): Double = ???/*
      if !cached then compute()
      ca0 + ca1*(t - Ot) + Ox*/

    def apply(ts: Array[Double]): Array[Double] = ???/*
      if !cached then compute()
      val xs = new Array[Double](ts.length)
      var i = 0
      val x0 = ca0 + Ox
      while i < ts.length do
        xs(i) = x0 + ca1*(ts(i) - Ot)
        i += 1
      xs*/

    def inverse(x: Double): Double = ???/*
      if !cached then compute()
      if ca1 == 0 then Double.NaN
      else
        (x - ca0 - Ox)/ca1 + Ot*/

    def inverse(xs: Array[Double]): Array[Double] = ???/*
      if !cached then compute()
      if ca1 == 0 then Array.fill(xs.length)(Double.NaN)
      else
        val ts = new Array[Double](xs.length)
        var i = 0
        val x0 = ca0 + Ox
        while i < xs.length do
          ts(i) = (xs(i) - x0)/ca1 + Ot
        xs*/
    */

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

    /*
    def setOrigin(ot: Double, ox: Double): Unit =
      if n > 0 then
        cached = false
        val dt = Ot - ot
        val dx = Ox - ox
        val ndt = n*dt
        val ndx = n*dx
        Stt += dt*(2*St + ndt)
        Sxx += dx*(2*Sx + ndx)
        Stx += dt*Sx + dx*(St + ndt)
        St += ndt
        Sx += ndx
      Ot = ot
      Ox = ox
    */

    /*
    def recenter(): Unit =
      if n > 0 then
        val mt = St/n
        val mx = Sx/n
        val nmt = n * mt
        val nmx = n * mx
        Stt -= mt * (2*St - nmt)
        Sxx -= mx * (2*Sx - nmx)
        Stx -= mt*Sx + mx*(St - nmt)
        St = 0
        Sx = 0
        Ot += mt
        Ox += mx
    */

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

    def ++=(ts: Array[Double], xs: Array[Double]): Unit = ???/*
      val m = math.min(ts.length, xs.length)
      addRangeImpl(ts, 0)(xs, 0)(m)*/

    def ++=(ts: IterableOnce[Double], xs: IterableOnce[Double]): Unit = ???/*
      val ti = ts.iterator
      val xi = xs.iterator
      while ti.hasNext && xi.hasNext do
        this += (ti.next, xi.next)*/

    def addRange(ts: Array[Double], i0: Int, iN: Int)(xs: Array[Double], j0: Int, jN: Int): Unit = ???/*
      val k0 = math.max(0, i0)
      val kN = math.min(ts.length, iN)
      if k0 < kN then
        val l0 = math.max(0, j0)
        val lN = math.min(xs.length, jN)
        if l0 < lN then
          val m = math.min(kN - k0, lN - l0)
          addRangeImpl(ts, k0)(xs, l0)(m)*/

    private def addRangeImpl(ts: Array[Double], i0: Int)(xs: Array[Double], j0: Int)(m: Int): Unit = ???/*
      if m > 0 then
        if (n & 0x3F) == 0 then
          if n == 0 then
            Ot = ts(i0)
            Ox = xs(j0)
            if (Ot + Ox).nan then
              Ot = 0.0
              Ox = 0.0
            else
              n += 1
          else
            recenter()
            this += (ts(i0), xs(j0))
        else
          this += (ts(i0), xs(j0))
        addRangeImpl(ts, i0 + 1)(xs, j0 + 1)(m - 1)*/

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
