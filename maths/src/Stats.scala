// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-16, 2023 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.maths


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.lang.{Math => jm}

import scala.annotation.targetName
import scala.compiletime.erasedValue

import scala.collection.immutable.{Range => Rg}

import kse.basics.{given, *}
import kse.basics.intervals.{given, *}


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
  def snr: Double = mean.abs / sem

  def pmSD: PlusMinus = mean.toFloat +- sd.toFloat
  def pmSEM: PlusMinus = mean.toFloat +- sem.toFloat

  def mutableCopy: Est.M = new Est.M(n, mean, sse)
  def ++(that: Est): Est =
    val e = mutableCopy
    e += that
    e

  override def toString =
    if (n - n.rint).abs < 1e-6 && n.abs < 9e18 then s"$mean +- $sem (n=${n.toLong})"
    else s"$mean +- $sem (W=$n)"

  override def equals(a: Any): Boolean = a match
    case e: Est => e.n == n && e.mean == mean && e.sse == sse
    case _      => false
}
object Est {
  inline infix def of(values: Array[Int]): Est = Est.M.from(values)

  inline infix def of(values: Array[Long]): Est = Est.M.from(values)

  inline infix def of(values: Array[Float]): Est = Est.M.from(values)

  inline infix def of(values: Array[Double]): Est = Est.M.from(values)

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

    def reset(): Unit = { n = 0; mean = 0; sse = 0 }

    def +=(value: Long): Unit =
      val mold = mean
      mean = (n*mean + value)/(n+1)
      sse += (value - mean)*(value - mold)
      n += 1
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
      values.visit(): (i, _) =>
        val x = i.toDouble
        val mold = mean
        mean = (n*mean + x)/(n+1)
        sse += (x - mean)*(x - mold)
        n += 1
    def ++=(values: Array[Long]): Unit =
      values.visit(): (l, _) =>
        val x = l.toDouble
        val mold = mean
        mean = (n*mean + x)/(n+1)
        sse += (x - mean)*(x - mold)
        n += 1
    def ++=(values: Array[Float]): Unit =
      values.visit(): (v, _) =>
        if !v.nan then
          val u = v.toDouble
          val mold = mean
          mean = (n*mean + u)/(n+1)
          sse += (u - mean)*(u - mold)
          n += 1
    def ++=(values: Array[Double]): Unit =
      values.visit(): (v, _) =>
        if !v.nan then
          val mold = mean
          mean = (n*mean + v)/(n+1)
          sse += (v - mean)*(v - mold)
          n += 1
    inline def ++=[P <: Int | Long | Float | Double](values: Iterator[P]): Unit = inline erasedValue[P] match
      case _: Int =>
        val i = values.asInstanceOf[Iterator[Int]]
        while i.hasNext do
          this += i.next
      case _: Long =>
        val i = values.asInstanceOf[Iterator[Long]]
        while i.hasNext do
          this += i.next
      case _: Float =>
        val i = values.asInstanceOf[Iterator[Float]]
        while i.hasNext do
          this += i.next
      case _: Double =>
        val i = values.asInstanceOf[Iterator[Double]]
        while i.hasNext do
          this += i.next

    inline def addWith[A](values: Array[A])(inline f: A => Double): Unit = addRangeWith(values)(0, values.length)(f)

    def addWith[A](values: Iterator[A])(f: A => Double): Unit =
      while values.hasNext do
        this += f(values.next)

    @targetName("raw_add_I") def addRange(values: Array[Int])(i0: Int, iN: Int): Unit =
      values.visit(i0, iN): (j, _) =>
        val x = j.toDouble
        val mold = mean
        mean = (n*mean + x)/(n+1)
        sse += (x - mean)*(x - mold)
        n += 1
    @targetName("raw_add_L") def addRange(values: Array[Long])(i0: Int, iN: Int): Unit =
      values.visit(i0, iN): (l, _) =>
        val x = l.toDouble
        val mold = mean
        mean = (n*mean + x)/(n+1)
        sse += (x - mean)*(x - mold)
        n += 1
    @targetName("raw_add_F") def addRange(values: Array[Float])(i0: Int, iN: Int): Unit =
      values.visit(i0, iN): (v, _) =>
        if !v.nan then
          val u = v.toDouble
          val mold = mean
          mean = (n*mean + u)/(n+1)
          sse += (u - mean)*(u - mold)
          n += 1
    @targetName("raw_add_D") def addRange(values: Array[Double])(i0: Int, iN: Int): Unit =
      values.visit(i0, iN): (v, _) =>
        if !v.nan then
          val mold = mean
          mean = (n*mean + v)/(n+1)
          sse += (v - mean)*(v - mold)
          n += 1

    @targetName("raw_add_I_rg") inline def addRange(values: Array[Int   ])(inline rg: Rg): Unit = { val iv = Iv of rg; addRange(values)(iv.i0, iv.iN) }
    @targetName("raw_add_L_rg") inline def addRange(values: Array[Long  ])(inline rg: Rg): Unit = { val iv = Iv of rg; addRange(values)(iv.i0, iv.iN) }
    @targetName("raw_add_F_rg") inline def addRange(values: Array[Float ])(inline rg: Rg): Unit = { val iv = Iv of rg; addRange(values)(iv.i0, iv.iN) }
    @targetName("raw_add_D_rg") inline def addRange(values: Array[Double])(inline rg: Rg): Unit = { val iv = Iv of rg; addRange(values)(iv.i0, iv.iN) }

    @targetName("raw_add_I_iv") inline def addRange(values: Array[Int   ])(inline v: Iv.X): Unit = { val iv = v of values; addRange(values)(iv.i0, iv.iN) }
    @targetName("raw_add_L_iv") inline def addRange(values: Array[Long  ])(inline v: Iv.X): Unit = { val iv = v of values; addRange(values)(iv.i0, iv.iN) }
    @targetName("raw_add_F_iv") inline def addRange(values: Array[Float ])(inline v: Iv.X): Unit = { val iv = v of values; addRange(values)(iv.i0, iv.iN) }
    @targetName("raw_add_D_iv") inline def addRange(values: Array[Double])(inline v: Iv.X): Unit = { val iv = v of values; addRange(values)(iv.i0, iv.iN) }

    @targetName("clip_add_I") def addRange(values: ClippedArray[Int])(i0: Int, iN: Int): Unit =
      values.visit(i0, iN): (j, _) =>
        val x = j.toDouble
        val mold = mean
        mean = (n*mean + x)/(n+1)
        sse += (x - mean)*(x - mold)
        n += 1
    @targetName("clip_add_L") def addRange(values: ClippedArray[Long])(i0: Int, iN: Int): Unit =
      values.visit(i0, iN): (l, _) =>
        val x = l.toDouble
        val mold = mean
        mean = (n*mean + x)/(n+1)
        sse += (x - mean)*(x - mold)
        n += 1
    @targetName("clip_add_F") def addRange(values: ClippedArray[Float])(i0: Int, iN: Int): Unit =
      values.visit(i0, iN): (v, _) =>
        if !v.nan then
          val u = v.toDouble
          val mold = mean
          mean = (n*mean + u)/(n+1)
          sse += (u - mean)*(u - mold)
          n += 1
    @targetName("clip_add_D") def addRange(values: ClippedArray[Double])(i0: Int, iN: Int): Unit =
      values.visit(i0, iN): (v, _) =>
        if !v.nan then
          val mold = mean
          mean = (n*mean + v)/(n+1)
          sse += (v - mean)*(v - mold)
          n += 1

    @targetName("clip_add_I_rg") inline def addRange(values: ClippedArray[Int   ])(inline rg: Rg): Unit = { val iv = Iv of rg; addRange(values)(iv.i0, iv.iN) }
    @targetName("clip_add_L_rg") inline def addRange(values: ClippedArray[Long  ])(inline rg: Rg): Unit = { val iv = Iv of rg; addRange(values)(iv.i0, iv.iN) }
    @targetName("clip_add_F_rg") inline def addRange(values: ClippedArray[Float ])(inline rg: Rg): Unit = { val iv = Iv of rg; addRange(values)(iv.i0, iv.iN) }
    @targetName("clip_add_D_rg") inline def addRange(values: ClippedArray[Double])(inline rg: Rg): Unit = { val iv = Iv of rg; addRange(values)(iv.i0, iv.iN) }

    @targetName("clip_add_I_iv") inline def addRange(values: ClippedArray[Int   ])(inline v: Iv.X): Unit = { val iv = v of values.unwrap; addRange(values)(iv.i0, iv.iN) }
    @targetName("clip_add_L_iv") inline def addRange(values: ClippedArray[Long  ])(inline v: Iv.X): Unit = { val iv = v of values.unwrap; addRange(values)(iv.i0, iv.iN) }
    @targetName("clip_add_F_iv") inline def addRange(values: ClippedArray[Float ])(inline v: Iv.X): Unit = { val iv = v of values.unwrap; addRange(values)(iv.i0, iv.iN) }
    @targetName("clip_add_D_iv") inline def addRange(values: ClippedArray[Double])(inline v: Iv.X): Unit = { val iv = v of values.unwrap; addRange(values)(iv.i0, iv.iN) }


    @targetName("raw_add_with") inline def addRangeWith[A](values: Array[A])(i0: Int, iN: Int)(inline f: A => Double): Unit =
      values.visit(i0, iN): (x, _) =>
        val v = f(x)
        if !v.nan then
          val mold = mean
          mean = (n*mean + v)/(n+1)
          sse += (v - mean)*(v - mold)
          n += 1
    @targetName("raw_add_with_rg") inline def addRangeWith[A](values: Array[A])(inline rg: Rg)(inline f: A => Double): Unit = { val iv = Iv of rg; addRangeWith(values)(iv.i0, iv.iN)(f) }
    @targetName("raw_add_with_iv") inline def addRangeWith[A](values: Array[A])(inline v: Iv.X)(inline f: A => Double): Unit = { val iv = v of values; addRangeWith(values)(iv.i0, iv.iN)(f) }


    @targetName("clip_add_with") inline def addRangeWith[A](values: ClippedArray[A])(i0: Int, iN: Int)(inline f: A => Double): Unit =
      values.visit(i0, iN): (x, _) =>
        val v = f(x)
        if !v.nan then
          val mold = mean
          mean = (n*mean + v)/(n+1)
          sse += (v - mean)*(v - mold)
          n += 1
    @targetName("clip_add_with_rg") inline def addRangeWith[A](values: ClippedArray[A])(inline rg: Rg)(inline f: A => Double): Unit = { val iv = Iv of rg; addRangeWith(values)(iv.i0, iv.iN)(f) }
    @targetName("clip_add_with_iv") inline def addRangeWith[A](values: ClippedArray[A])(inline v: Iv.X)(inline f: A => Double): Unit = { val iv = v of values.unwrap; addRangeWith(values)(iv.i0, iv.iN)(f) }
  }
  object M {
    inline val smallestNonzeroWeight = 1e-9
    inline val smallestSubtractable = 1.000000001

    inline def apply(): M = new M(0, 0, 0)
    inline def empty(): M = new M(0, 0, 0)

    inline def apply(n: Double, mean: Double, sse: Double): M = new M(n, mean, sse)

    inline def from(values: Array[Int]): M =
      val m = new M(0, 0, 0)
      m ++= values
      m

    inline def from(values: Array[Long]): M =
      val m = new M(0, 0, 0)
      m ++= values
      m

    inline def from(values: Array[Float]): M =
      val m = new M(0, 0, 0)
      m ++= values
      m

    inline def from(values: Array[Double]): M =
      val m = new M(0, 0, 0)
      m ++= values
      m

    def fromSD(n: Double)(pm: PlusMinus): Est.M =
      val m = if n > 2 then n else 2.0
      new M(m, pm.value.toDouble, (m-1)*pm.error.toDouble.sq)

    def fromSEM(n: Double)(pm: PlusMinus): Est.M =
      val m = if n > 2 then n else 2.0
      new M(m, pm.value.toDouble, m*(m-1)*pm.error.toDouble.sq)
  }
}


class Bootstrap[T](val value: T, val variants: Array[T]) {
  def n = variants.length
  def estOfVariants(f: T => Double): Est.M =
    val e = Est.M()
    var i = 0
    while i < variants.length do
      e += f(variants(i))
      i += 1
    e
  def pm(f: T => Double): PlusMinus =
    val e = estOfVariants(f)
    e.mean = f(value)
    e.pmSD
}
object Bootstrap {
  inline def apply[T](n: Int)(rng: Prng)(i0: Int, iN: Int)(init: => T)(inline update: (T, Int) => Unit)(using tag: reflect.ClassTag[T]): Bootstrap[T] =
    val value = init
    val variants = Array.fill(n)(init)
    if i0 < iN then
      val m = iN -# i0
      var i = i0
      while i < iN do
        update(value, i)
        var j = 0
        while j < n do
          update(variants(j), (rng % m) + i0)
          j += 1
        i += 1
    new Bootstrap(value, variants)
  inline def apply[T](n: Int)(i0: Int, iN: Int)(init: => T)(inline update: (T, Int) => Unit)(using reflect.ClassTag[T], AutoPrng): Bootstrap[T] =
    apply(n)(summon[AutoPrng].get)(i0, iN)(init)(update)
}


final class Hist private (val bins: Int) {
  val data = new Array[Int](bins+1)
  var n = 0
  def +=(bin: Int): Unit =
    data(if bin >= 0 && bin < bins then bin else bins) += 1
    n += 1
  inline def op(bin: Int)(inline f: Int => Int): Unit =
    val i = if bin >= 0 && bin < bins then bin else bins
    val x = data(i)
    val y = f(x)
    data(i) = y
    n += (y - x)
  inline def addWith[A](a: A)(inline f: A => Int) = this += f(a)

  def count(i: Int): Int = if i >= 0 && i < bins then data(i) else 0
  def outliers: Int = data(End)

  inline def visit(inline f: (Int, Int) => Unit): Unit =
    var i = 0
    while i < bins do
      f(data(i), i)
      i += 1

  override def toString = s"Histogram, $bins bins.  $n counts, $outliers outliers, data = ${data(0)}, ${data(1)}, ..."
}
object Hist {
  def apply(bins: Int): Hist =
    if bins < 1 then throw new IllegalArgumentException(s"Histogram must have at least 1 bin, not $bins")
    new Hist(bins)
}

final class Hist2 private (val xbins: Int, val ybins: Int) {
  val data = new Array[Int]((xbins + 1) * (ybins + 1))
  var n = 0
  def add(xbin: Int, ybin: Int): Unit =
    val x = if xbin >= 0 && xbin < xbins then xbin else xbins
    val y = if ybin >= 0 && ybin < ybins then ybin else ybins
    data(x + y * (xbins + 1)) += 1
    n += 1
  inline def op(xbin: Int, ybin: Int)(inline f: Int => Int): Unit =
    val x = if xbin >= 0 && xbin < xbins then xbin else xbins
    val y = if ybin >= 0 && ybin < ybins then ybin else ybins
    val i = x  + y * (xbins + 1)
    val a = data(i)
    val b = f(a)
    data(i) = b
    n += (b - a)
  inline def addWith[A](a1: A, a2: A)(inline f: A => Int): Unit = this.add(f(a1), f(a2))

  def count(i: Int, j: Int): Int = if i >= 0 && i < xbins && j >= 0 && j < ybins then data(i + (xbins + 1)*j) else 0
  def xOutliers(y: Int): Int = if y >= 0 && y < ybins then data(xbins + (xbins + 1)*y) else 0
  def yOutliers(x: Int): Int = if x >= 0 && x < xbins then data(x + (xbins + 1)*ybins) else 0
  def doubleOutliers: Int = data(End)

  inline def visit(inline f: (Int, Int, Int) => Unit): Unit =
    var j = 0
    while j < ybins do
      val jj = (xbins + 1) * j
      var i = 0
      while i < xbins do
        f(data(i + jj), i, j)
        i += 1
      j += 1
  inline def visitXOutliers(inline f: (Int, Int) => Unit): Unit =
    var j = 0
    while j < ybins do
      f(data(xbins + (xbins + 1)*j), j)
      j += 1
  inline def visitYOutliers(inline f: (Int, Int) => Unit): Unit =
    var i = 0
    while i < xbins do
      f(data(i + (xbins + 1)*ybins), i)
      i += 1
}
object Hist2 {
  def apply(xbins: Int, ybins: Int): Hist2 =
    if xbins < 1 || ybins < 1 then throw new IllegalArgumentException(s"Histogram bin grid must be at least 1 x 1, not $xbins x $ybins")
    new Hist2(xbins, ybins)
}

final class Dist private (val cuml: Array[Double], c: Int, val effectiveN: Double = Double.NaN) {
  inline def size = cuml.length - 1

  inline def probability(i: Int) = cuml(i+1) - cuml(i)
  inline def visitProbabilities(inline f: (Double, Int) => Unit): Unit =
    var x = 0.0
    var y = 0.0
    var i = 1
    while i < cuml.length do
      x = y
      y = cuml(i)
      f(y - x, i - 1)
      i += 1

  lazy val entropy =
    var sum = 0.0
    visitProbabilities: (p, _) =>
      if p > 0 then sum += p.entropy
    sum
  def crossEntropy(that: Dist, pseudozero: Double = 0.0, extendWithZero: Boolean = false): Double =
    if !extendWithZero && cuml.length != that.cuml.length then
      throw new IllegalArgumentException(s"Distribution size mismatch: $size vs ${that.size}. Try extendWithZero = true.")
    var e = 0.0
    if pseudozero == 0.0 then
      if !extendWithZero || that.cuml.length >= cuml.length then
        visitProbabilities: (p, i) =>
          if p > 0 then
            e += p * that.probability(i).surprisal
      else
        val n = that.size
        visitProbabilities: (p, i) =>
          if p > 0 then
            e += p * (if i < n then that.probability(i).surprisal else Double.PositiveInfinity)
    else if pseudozero > Double.NegativeInfinity && pseudozero < Double.PositiveInfinity then
      val n = that.size
      val pz =
        if pseudozero < 0 then -pseudozero
        else if effectiveN > 0 then pseudozero/effectiveN
        else throw new IllegalArgumentException(s"Effective N unknown.  Use negative pseudozero to indicate a size-unscaled pseudocount for each bin.")
      val normalizer = 1.0 + jm.max(size, that.size) * pz
      val npzsurprisal = (pz/normalizer).surprisal
      visitProbabilities: (p, i) =>
        if p > 0 then
          e += p * (if i < n then ((that.probability(i) + pz)/normalizer).surprisal else npzsurprisal)
    e
  def divKL(that: Dist, pseudozero: Double = 0.0, extendWithZero: Boolean = false): Double =
    crossEntropy(that, pseudozero, extendWithZero) - entropy
  def divJS(that: Dist, extendWithZero: Boolean = false, hemi: Boolean = false, weight: Double = 0.5): Double =
    if !extendWithZero && cuml.length != that.cuml.length then
      throw new IllegalArgumentException(s"Distribution size mismatch: $size vs ${that.size}. Try extendWithZero = true.")
    if !(0 < weight && weight < 1.0) then
      throw new IllegalArgumentException(s"Distribution weight must be between 0 and 1 (exclusive), not $weight")
    val mps = new Array[Double](jm.max(size, that.size))
    val thatw = 1.0 - weight
    visitProbabilities: (p, i) =>
      mps(i) = weight * p
    that.visitProbabilities: (p, i) =>
      mps(i) += thatw * p
    if hemi then
      var e = 0.0
      visitProbabilities: (p, i) =>
        if p > 0 then
          e += p * (mps(i).surprisal - p.surprisal)
      e    
    else
      mps.entropy - weight * entropy - thatw * that.entropy

  def sample(rng: Prng): Int =
    val v = rng.D
    var i0 = 0
    var iN = cuml.length
    var i = c
    var unfound = true
    while unfound do
      val a = cuml(i-1)
      val b = cuml(i)
      if v > b then
        i0 = i
        i = (i0 + iN) >>> 1
      else if v < a then
        iN = i
        i = (i0 + iN) >>> 1
      else
        unfound = false
    i - 1
  @targetName("sample_auto") def sample()(using AutoPrng): Int = sample(summon[AutoPrng].get)
  def linearSample(rng: Prng): Double =
    var v = rng.D
    var i0 = 0
    var iN = cuml.length
    var i = c
    var unfound = true
    while unfound do
      val a = cuml(i-1)
      val b = cuml(i)
      if v > b then
        i0 = i
        i = (i0 + iN) >>> 1
      else if v < a then
        iN = i
        i = (i0 + iN) >>> 1
      else
        unfound = false
        if b == a then v = i - 0.5
        else
          v = i - (b - v)/(b - a)
    v
  @targetName("linearSample_auto") def linearSample()(using AutoPrng): Double = linearSample(summon[AutoPrng].get)
}
object Dist {
  def fromValues(ps: Array[Double], effectiveN: Double = Double.NaN): Dist =
    if ps.length == 0 then throw new IllegalArgumentException(s"Cannot build distribution from empty input array")
    val cuml = new Array[Double](ps.length + 1)
    var max = 0.0
    var c = 0
    ps.visit(): (p, i) =>
      if !(p >= 0 && p < Double.PositiveInfinity) then throw new IllegalArgumentException(s"Cannot build distribution:\n  value $i is not finite and non-negative: $p")
      if p > max then
        max = p
        c = i + 1
    if max == 0 then
      cuml.set(1 to End)(i => i/(cuml.length - 1).toDouble)
      new Dist(cuml, cuml.length / 2, effectiveN)
    else
      val imax = 1.0/max
      ps.visit(): (p, i) =>
        cuml(i+1) = cuml(i) + p * imax
      val isum = 1.0 / cuml(End)
      cuml.alter(1 to End-1)(_ * isum)
      cuml(End) = 1.0
      new Dist(cuml, c, effectiveN)

  def fromCounts(ns: Array[Int]): Dist =
    if ns.length == 0 then throw new IllegalArgumentException(s"Cannot build distribution from empty input array")
    val cuml = new Array[Double](ns.length + 1)
    var max = 0
    var c = 0
    var sum = 0L
    ns.visit(): (n, i) =>
      if n < 0 then throw new IllegalArgumentException(s"Cannot build distribution:\n  count in bin $i is negative: $n")
      if n > max then
        max = n
        c = i + 1
      sum += n
    if sum == 0L then
      cuml.set(1 to End)(i => i/(cuml.length - 1).toDouble)
      new Dist(cuml, cuml.length / 2, 0)
    else
      val isum = 1.0/sum
      sum = 0L
      ns.visit(0 to End-1): (n, i) =>
        sum += n
        cuml(i + 1) = sum * isum
      cuml(End) = 1.0
      new Dist(cuml, c, sum.toDouble)

  def fromHist(h: Hist, withOutliers: Boolean = true): Dist =
    if withOutliers then fromCounts(h.data)
    else fromCounts(h.data.select(0 to End-1))

  final class Joint private (val rows: Array[Dist], val rowDist: Dist) {
    import kse.maths.packed.{given, *}
    lazy val cols = rows(0).size.unfold: i =>
      var effectiveN = 0.0
      val values = new Array[Double](rowDist.size)
      rowDist.visitProbabilities: (p, j) =>
        val q = rows(j).probability(i)
        effectiveN += rows(j).effectiveN * q
        values(j) = p * q
      fromValues(values, effectiveN)
    val colDist = fromValues(
      rows(0).size.unfold: i =>
        var p = 0.0
        rows.visit(): (r, j) =>
          p += rowDist.probability(j) * r.probability(i)
        p
      ,
      rowDist.effectiveN
    )

    lazy val entropy =
      var e = rowDist.entropy
      rowDist.visitProbabilities: (p, i) =>
        if p > 0 then e += p * rows(i).entropy
      e
    lazy val information =
      rowDist.entropy + colDist.entropy - entropy
    lazy val variationOfI =
      2.0*entropy - rowDist.entropy - colDist.entropy
    lazy val redundancy = (rowDist.entropy + colDist.entropy)/entropy - 1.0

    def samplePackedII(prng: Prng): Long =
      val j = rowDist.sample(prng)
      val i = rows(j).sample(prng)
      packed.Pack.L(i, j)
    def linearSamplePackedFF(prng: Prng): Long =
      val y = rowDist.linearSample(prng)
      val j = y.floor.toInt
      val x = rows(if j < rows.length then j else rows.length-1).linearSample(prng)
      packed.Pack.L(x.toFloat.bitsI, y.toFloat.bitsI)
    inline def useSample(prng: Prng)(inline f: (Int, Int) => Unit): Unit =
      val l = samplePackedII(prng)
      f(l.int(0), l.int(1))
    @targetName("useSample_auto") inline def useSample(using AutoPrng)(inline f: (Int, Int) => Unit): Unit =
      useSample(summon[AutoPrng].get)(f)
    inline def useLinearSample(prng: Prng)(inline f: (Float, Float) => Unit): Unit =
      val l = linearSamplePackedFF(prng)
      f(l.int(0).bitsF, l.int(1).bitsF)
    @targetName("useLinearSample_auto") inline def useLinearSample(using AutoPrng)(inline f: (Float, Float) => Unit): Unit =
      useLinearSample(summon[AutoPrng].get)(f)

    def andThen(next: Joint, matmul: Option[(Array[Array[Double]], Array[Array[Double]]) => Array[Array[Double]]] = None): Joint =
      if next.rowDist.size != colDist.size then throw new IllegalArgumentException(s"Joint distribution from ${rowDist.size}x${colDist.size} through ${next.rowDist.size}x${next.colDist.size} impossible; inner dimensions must match")
      val newrows = matmul match
        case Some(mm) =>
          val a = rows.copyWith: row =>
            val x = new Array[Double](row.size)
            row.visitProbabilities: (p, i) =>
              x(i) = p
            x
          val b = next.rows.copyWith: row =>
            val x = new Array[Double](row.size)
            row.visitProbabilities: (p, i) =>
              x(i) = p
            x
          mm(a, b)
        case _ =>
          val nrs = Array.fill(rows.length)(new Array[Double](next.rows(0).size))
          rows.visit(): (r, j) =>
            val nr = nrs(j)
            r.visitProbabilities: (p, i) =>
              next.rows(i).visitProbabilities: (q, h) =>
                nr(h) += q*p
          nrs
      new Joint(newrows.copyWith(row => fromValues(row)), rowDist)
  }
  object Joint {
    def fromValues(pss: Array[Array[Double]], effectiveN: Double = Double.NaN): Joint =
      if pss.isEmpty || pss(0).isEmpty then throw new IllegalArgumentException(s"Cannot build joint distribution from empty input array")
      pss.visit(1 to End): (ps, i) =>
        if ps.length != pss(0).length then throw new IllegalArgumentException(s"Rectangular values table needed for joint distribution\n  line $i mismatched: ${ps.length} not ${pss(0).length}")
      val rowDist = Dist.fromValues( pss.copyWith(ps => ps.gather(0.0)()((sum, x, _) => sum + x)), effectiveN )
      new Joint(pss.copyOp((ps, i) => Dist.fromValues(ps, rowDist.probability(i)*effectiveN)), rowDist)

    def fromCounts(nss: Array[Array[Int]]): Joint =
      if nss.isEmpty || nss(0).isEmpty then throw new IllegalArgumentException(s"Cannot build joint distribution from empty input array")
      nss.visit(1 to End): (ns, i) =>
        if ns.length != nss(0).length then throw new IllegalArgumentException(s"Rectangular values table needed for joint distribution\n  line $i mismatched: ${ns.length} not ${nss(0).length}")
      val rowDist = Dist.fromCounts( nss.copyWith(ns => ns.gather(0)()((sum, x, _) => sum + x)) )
      new Joint(nss.copyWith(Dist.fromCounts), rowDist)

    def fromHist(h: Hist2, withOutliers: Boolean = false, symmetrize: Boolean = false): Joint =
      val ny = h.ybins + (if withOutliers then 1 else 0)
      val nx = h.xbins + (if withOutliers then 1 else 0)
      val counts = ny.unfold: j =>
        h.data.select(j*(h.xbins + 1) span nx)
      if symmetrize then
        if h.xbins != h.ybins then throw new IllegalArgumentException(s"Only square matrices can be symmetrized, found ${h.ybins} x ${h.xbins} (YX)")
        counts.visit(): (ys, j) =>
          ys.edit(): (v, i) =>
            v + h.data(i*(h.xbins + 1) + j)
      fromCounts(counts)
  }
}


/*
object Ranks {
  def order(a: Array[Double], i0: Int, iN: Int)(b: Array[Double], j0: Int, jN: Int): Array[Int] =
    if iN > i0 && (i0 < 0 || iN >= a.length) then throw new NoSuchElementException(s"Bounds $i0 and $iN do not fit in array of length ${a.length}")
    if jN > j0 && (j0 < 0 || jN >= j.length) then throw new NoSuchElementException(s"Bounds $j0 and $jN do not fit in array of length ${a.length}")
    val ni = if i0 < iN then iN - i0 else 0
    val nj = if j0 < jN then jN - j0 else 0

}
*/


extension (values: Array[Int])
  inline def est(): Est = Est of values
  inline def est(i0: Int, iN: Int): Est =
    val e = Est.M()
    e.addRange(values)(i0, iN)
    e
  inline def est(inline v: Iv.X): Est =
    val iv = v of values
    est(iv.i0, iv.iN)
  inline def est(inline rg: Rg): Est =
    val iv = Iv of rg
    est(iv.i0, iv.iN)

extension (values: Array[Long])
  inline def est(): Est = Est of values
  inline def est(i0: Int, iN: Int): Est =
    val e = Est.M()
    e.addRange(values)(i0, iN)
    e
  inline def est(inline v: Iv.X): Est =
    val iv = v of values
    est(iv.i0, iv.iN)
  inline def est(inline rg: Rg): Est =
    val iv = Iv of rg
    est(iv.i0, iv.iN)

extension (values: Array[Float])
  inline def est(): Est = Est of values
  inline def est(i0: Int, iN: Int): Est =
    val e = Est.M()
    e.addRange(values)(i0, iN)
    e
  inline def est(inline v: Iv.X): Est =
    val iv = v of values
    est(iv.i0, iv.iN)
  inline def est(inline rg: Rg): Est =
    val iv = Iv of rg
    est(iv.i0, iv.iN)

extension (values: Array[Double])
  inline def est(): Est = Est of values
  inline def est(i0: Int, iN: Int): Est =
    val e = Est.M()
    e.addRange(values)(i0, iN)
    e
  inline def est(inline v: Iv.X): Est =
    val iv = v of values
    est(iv.i0, iv.iN)
  inline def est(inline rg: Rg): Est =
    val iv = Iv of rg
    est(iv.i0, iv.iN)

extension [A](values: Array[A]) {
  inline def estWith()(inline f: A => Double): Est =
    val m = Est.M()
    m.addRangeWith(values)(0, values.length)(f)
    m
  inline def estWith(i0: Int, iN: Int)(inline f: A => Double): Est =
    val m = Est.M()
    m.addRangeWith(values)(i0, iN)(f)
    m
  inline def estWith(inline v: Iv.X)(inline f: A => Double): Est =
    val iv = v of values
    estWith(iv.i0, iv.iN)(f)
  inline def estWith(inline rg: Rg)(inline f: A => Double): Est =
    val iv = Iv of rg
    estWith(iv.i0, iv.iN)(f)

  inline def intoHist(i0: Int, iN: Int)(h: Hist)(inline f: A => Int): Unit =
    var i = i0
    while i < iN do
      h.addWith(values(i))(f)
      i += 1
  inline def intoHist(inline v: Iv.X)(h: Hist)(inline f: A => Int): Unit =
    val iv = v of values
    intoHist(iv.i0, iv.iN)(h)(f)
  inline def intoHist(inline rg: Rg)(h: Hist)(inline f: A => Int): Unit =
    val iv = Iv of rg
    intoHist(iv.i0, iv.iN)(h)(f)
  inline def intoHist()(h: Hist)(inline f: A => Int): Unit =
    intoHist(0, values.length)(h)(f)
}

extension [A <: Int | Long | Float | Double](values: IterableOnce[A])
  inline def est(): Est =
    val m = Est.M()
    m ++= values.iterator
    m

extension [A](values: IterableOnce[A])
  inline def estWith()(f: A => Double): Est =
    val m = Est.M()
    m.addWith(values.iterator)(f)
    m
