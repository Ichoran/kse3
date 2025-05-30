// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-16, 2023 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.maths


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.annotation.targetName
import scala.compiletime.erasedValue

import scala.collection.immutable.{Range => Rg}

import kse.basics.{given, _}
import kse.basics.intervals.Iv


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
  inline def mut: Est.M = new Est.M(0, 0, 0)

  inline def from(values: Array[Int]): Est = Est.M.from(values)

  inline def from(values: Array[Long]): Est = Est.M.from(values)

  inline def from(values: Array[Float]): Est = Est.M.from(values)

  inline def from(values: Array[Double]): Est = Est.M.from(values)

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

    inline def empty: M = new M(0, 0, 0)

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
    val e = Est.M.empty
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
  inline def est(): Est = Est from values
  inline def est(i0: Int, iN: Int): Est =
    val e = Est.M.empty
    e.addRange(values)(i0, iN)
    e
  inline def est(inline v: Iv.X): Est =
    val iv = v of values
    est(iv.i0, iv.iN)
  inline def est(inline rg: Rg): Est =
    val iv = Iv of rg
    est(iv.i0, iv.iN)

extension (values: Array[Long])
  inline def est(): Est = Est from values
  inline def est(i0: Int, iN: Int): Est =
    val e = Est.M.empty
    e.addRange(values)(i0, iN)
    e
  inline def est(inline v: Iv.X): Est =
    val iv = v of values
    est(iv.i0, iv.iN)
  inline def est(inline rg: Rg): Est =
    val iv = Iv of rg
    est(iv.i0, iv.iN)

extension (values: Array[Float])
  inline def est(): Est = Est from values
  inline def est(i0: Int, iN: Int): Est =
    val e = Est.M.empty
    e.addRange(values)(i0, iN)
    e
  inline def est(inline v: Iv.X): Est =
    val iv = v of values
    est(iv.i0, iv.iN)
  inline def est(inline rg: Rg): Est =
    val iv = Iv of rg
    est(iv.i0, iv.iN)

extension (values: Array[Double])
  inline def est(): Est = Est from values
  inline def est(i0: Int, iN: Int): Est =
    val e = Est.M.empty
    e.addRange(values)(i0, iN)
    e
  inline def est(inline v: Iv.X): Est =
    val iv = v of values
    est(iv.i0, iv.iN)
  inline def est(inline rg: Rg): Est =
    val iv = Iv of rg
    est(iv.i0, iv.iN)

extension [A](values: Array[A])
  inline def estWith()(inline f: A => Double): Est =
    val m = Est.M.empty
    m.addRangeWith(values)(0, values.length)(f)
    m
  inline def estWith(i0: Int, iN: Int)(inline f: A => Double): Est =
    val m = Est.M.empty
    m.addRangeWith(values)(i0, iN)(f)
    m
  inline def estWith(inline v: Iv.X)(inline f: A => Double): Est =
    val iv = v of values
    estWith(iv.i0, iv.iN)(f)
  inline def estWith(inline rg: Rg)(inline f: A => Double): Est =
    val iv = Iv of rg
    estWith(iv.i0, iv.iN)(f)

extension [A <: Int | Long | Float | Double](values: IterableOnce[A])
  inline def est(): Est =
    val m = Est.M.empty
    m ++= values.iterator
    m

extension [A](values: IterableOnce[A])
  inline def estWith()(f: A => Double): Est =
    val m = Est.M.empty
    m.addWith(values.iterator)(f)
    m
