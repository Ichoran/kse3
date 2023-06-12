// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-16, 2023 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences, LLC.

package kse.maths


import scala.annotation.targetName

import kse.maths._

trait Est {
  def n: Double
  def mean: Double
  def ssq: Double
  def sse: Double

  def sum: Double = mean * n
  def variance: Double = if n > 2 then sse/(n-1) else sse
  def sd: Double = variance.sqrt
  def semSq: Double = if n > 2 then sse/(n*(n-1)) else if n > 1 then sse/n else Double.NaN
  def sem: Double = if n > 2 then (sse/(n*(n-1))).sqrt else if n > 1 then (sse/n).sqrt else Double.NaN
  def cv: Double = sd / mean
  def snr: Double = mean / sem

  def mutableCopy: Est.M = new Est.M(n, mean, sse)
  def pmSD: PlusMinus = mean.toFloat +- sd.toFloat
  def pmSEM: PlusMinus = mean.toFloat +- sem.toFloat

  def ++(that: Est): Est =
    val e = mutableCopy
    e += that
    e

  override def toString =
    if (n - n.rint).abs < 1e-6 && n.abs < 9e18 then s"$mean +- $sem (n=${n.toLong})"
    else s"$mean +- $sem (W=$n)"
}
object Est {
  def mut: Est.M = new Est.M(0, 0, 0)

  /** Provide a running update of weighted mean and standard deviation (variance).
    *
    * Uses approximately the same scheme as in Finch (2009) for the variance.
    * See https://fanf2.user.srcf.net/hermes/doc/antiforgery/stats.pdf
    * (Note that Knuth etc. also have given formulae for this.)
    * Finch favors mean += (w/W)*(x - mean), but here we use mean = ((W-w)*mean + w*x)/W
    */
  final class M(var n: Double, var mean: Double, var sse: Double) extends Est {
    def ssq = sse + n * mean.sq
    def snapshot: Est = new M(n, mean, sse)

    def reset: this.type = { n = 0; mean = 0; sse = 0; this }

    def +=(value: Double): Unit =
      if !value.nan then
        val mold = mean
        mean = (n*mean + value)/(n+1)
        sse += (value - mean)*(value - mold)
        n += 1
    def -=(value: Double): Unit =
      if !value.nan then
        val mold = mean
        if n > M.smallestSubtractable then
          mean = (n*mean - value)/(n-1)
          sse -= (value - mold)*(value - mean)
          n -= 1
        else
          mean = 0
          sse = 0
          n = 0

    def incorporate(samples: Double, value: Double, sumsqerr: Double): Unit =
      if samples + n > M.smallestNonzeroWeight then
        if samples >= 0 then
          val nold = n
          val mold = mean
          n += samples
          mean = (nold*mean + value*samples)/n
          sse = sse + sumsqerr + (mold - value).sq*nold*samples / n    
        else
          val nold = n
          val mold = mean
          n += samples
          mean = (nold*mean + value*samples)/n
          sse = sse - sumsqerr + (mean - value).sq*n*samples / nold
          if !(sse >= 0) then sse = 0
      else
        mean = 0
        sse = 0
        n = 0

    def addWithWeight(wt: Double)(value: Double): Unit =
      if wt > 0 then
        if n > 0 then
          val nold = n
          val mold = mean
          n += wt
          mean = (nold*mean + wt*value)/n
          sse += wt*(value - mean)*(value - mold)
        else
          n = wt
          mean = value
          sse = 0
      else if wt + n > M.smallestNonzeroWeight then
        if wt < 0 then
          val nold = n
          val mold = mean
          n += wt
          mean = (nold*mean + wt*value)/n
          sse += wt*(value - mean)*(value - mold)
      else
        n = 0
        mean = 0
        sse = 0

    def +=(est: Est): Unit = incorporate(est.n, est.mean, est.sse)
    def -=(est: Est): Unit = incorporate(-est.n, est.mean, est.sse)

    def ++=(values: Array[Int]): Unit =
      var i = 0
      while i < values.length do
        val x = values(i)
        val mold = mean
        mean = (n*mean + x)/(n+1)
        sse += (x - mean)*(x - mold)
        n += 1
        i += 1
    def ++=(values: Array[Long]): Unit =
      var i = 0
      while i < values.length do
        val x = values(i)
        val mold = mean
        mean = (n*mean + x)/(n+1)
        sse += (x - mean)*(x - mold)
        n += 1
        i += 1
    def ++=(values: Array[Float]): Unit =
      var i = 0
      while i < values.length do
        val v = values(i)
        if !v.nan then
          val u = v.toDouble
          val mold = mean
          mean = (n*mean + u)/(n+1)
          sse += (u - mean)*(u - mold)
          n += 1
        i += 1
    def ++=(values: Array[Double]): Unit =
      var i = 0
      while i < values.length do
        val v = values(i)
        if !v.nan then
          val mold = mean
          mean = (n*mean + v)/(n+1)
          sse += (v - mean)*(v - mold)
          n += 1
        i += 1

    def addRange(i0: Int, iN: Int)(values: Array[Int]): Unit =
      var i = i0
      if i < 0 then i = 0
      val iM = if iN < values.length then iN else values.length
      while i < iM do
        val x = values(i).toDouble
        val mold = mean
        mean = (n*mean + x)/(n+1)
        sse += (x - mean)*(x - mold)
        n += 1
        i += 1
    def addRange(i0: Int, iN: Int)(values: Array[Long]): Unit =
      var i = i0
      if i < 0 then i = 0
      val iM = if iN < values.length then iN else values.length
      while i < iM do
        val x = values(i).toDouble
        val mold = mean
        mean = (n*mean + x)/(n+1)
        sse += (x - mean)*(x - mold)
        n += 1
        i += 1
    def addRange(i0: Int, iN: Int)(values: Array[Float]): Unit =
      var i = i0
      if i < 0 then i = 0
      val iM = if iN < values.length then iN else values.length
      while i < iM do
        val v = values(i)
        if !v.nan then
          val u = v.toDouble
          val mold = mean
          mean = (n*mean + u)/(n+1)
          sse += (u - mean)*(u - mold)
          n += 1
        i += 1
    def addRange(i0: Int, iN: Int)(values: Array[Double]): Unit =
      var i = i0
      if i < 0 then i = 0
      val iM = if iN < values.length then iN else values.length
      while i < iM do
        this += values(i)
        i += 1
    inline def addRangeBy[A](i0: Int, iN: Int)(values: Array[A])(f: A => Double): Unit =
      var i = i0
      if i < 0 then i = 0
      val iM = if iN < values.length then iN else values.length
      while i < iM do
        this += f(values(i))
        i += 1
  }
  object M {
    inline val smallestNonzeroWeight = 1e-9
    inline val smallestSubtractable = 1.000000001

    def empty: M = new M(0, 0, 0)
    def fromSD(n: Double)(pm: PlusMinus): Est.M =
      val m = if n > 2 then n else 2.0
      new M(m, pm.value.toDouble, (m-1)*pm.error.toDouble.sq)

    def fromSEM(n: Double)(pm: PlusMinus): Est.M =
      val m = if n > 2 then n else 2.0
      new M(m, pm.value.toDouble, m*(m-1)*pm.error.toDouble.sq)
  }
}
