// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2023 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC

package kse.maths

import scala.math._


trait Interpolate {
  def inputDimension: Int
  def size: Int
  def parameters: Array[Double]
}

trait Interpolate1D extends Interpolate {
  final def inputDimension = 1
  def lower: Double
  def upper: Double
}

final class LinearInterp(in: Array[Double], out: Array[Double]) extends Interpolate1D {
  val size = min(in.length, out.length)
  val parameters = new Array[Double](2*size)
  System.arraycopy(in, 0, parameters, 0, size)
  System.arraycopy(out, 0, parameters, size, size)
  val lower = if parameters.length > 0 then parameters(0)                     else Double.NaN
  val upper = if parameters.length > 0 then parameters(parameters.length - 1) else Double.NaN
  def apply(a: Double): Double =
    var i0 = 0
    var i1 = size - 1
    while (i0 + 1 < i1) do
      val i = (i0 + i1) >>> 1   // Avoids overflow, but we don't actually need to
      if parameters(i) < a then i0 = i
      else i1 = i
    val a0 = parameters(i0)
    val a1 = parameters(i1)
    if a <= a0 then parameters(size + i0)
    else if a >= a1 then parameters(size + i1)
    else
      val z0 = parameters(size + i0)
      val z1 = parameters(size + i1)
      z0 + (z1 - z0)*(a - a0)/(a1 - a0)
}
object LinearInterp {
  def apply(in: Array[Double], out: Array[Double]) = new LinearInterp(in, out)
}

/** Marker trait so we know what fits we could have */
sealed trait Fit[F <: Fit[F]] extends scala.Cloneable {
  def samples: Long
  def clear(): Unit
  override def clone(): F = ???
}   


/** FitTX performs a ordinary least squares fit of a parameter t against a readout x.
  * We assume t is accurate; this method is not precise if both t and x can vary.
  */
final class FitTX() extends Fit[FitTX] {
  private var Ot = 0.0
  private var Ox = 0.0

  private var n = 0L
  private var St = 0.0
  private var Stt = 0.0
  private var Sx = 0.0
  private var Sxx = 0.0
  private var Stx = 0.0

  private var cached = false
  private var myAx = 0.0
  private var myBx = 0.0
  private var myE = 0.0


  private def setEverything(_Ot: Double, _Ox: Double, _n: Long, _St: Double, _Stt: Double, _Sx: Double, _Sxx: Double, _Stx: Double): this.type =
    Ot = _Ot; Ox = _Ox;
    n = _n;
    St = _St; Stt = _Stt;
    Sx = _Sx; Sxx = _Sxx;
    Stx = _Stx;
    this

  override def clone(): FitTX =
    val that = new FitTX
    that.setEverything(Ot, Ox, n, St, Stt, Sx, Sxx, Stx)


  private def compute(): Unit =
    if n < 2 then 
      myAx = Double.NaN
      myBx = Double.NaN
      myE = Double.NaN
    else
      val den = n*Stt - St*St
      myBx = if den != 0 then (n*Stx - St*Sx)/den else Double.NaN
      myAx = (Sx - myBx*St) / n
      myE = (n*Sxx - Sx*Sx - myBx*myBx*den) / n
    cached = true

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

  def inverse(x: Double): Double =
    val bX = betaX
    if bX == 0 then Double.NaN
    else (x - alphaX - Ox)/bX + Ot

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

  def clear(): Unit =
    cached = false
    n = 0
    St = 0
    Stt = 0
    Sx = 0
    Sxx = 0
    Stx = 0

  def origin(ot: Double, ox: Double): Unit =
    if (n > 0) {
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
    }
    Ot = ot
    Ox = ox

  def +=(ti: Double, xi: Double): Unit =
    cached = false
    val t = ti - Ot
    val x = xi - Ox
    n += 1
    St += t
    Stt += t*t
    Sx += x
    Sxx += x*x
    Stx += t*x

  def -=(ti: Double, xi: Double): Unit =
    cached = false
    val t = ti - Ot
    val x = xi - Ox
    n -= 1
    St -= t
    Stt -= t*t
    Sx -= x
    Sxx -= x*x
    Stx -= t*x

  override def toString = s"Fit: x = ${betaX} t + ${xt(0)} (n=$n, rmse=${(sqrt(error)/n)})"
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
