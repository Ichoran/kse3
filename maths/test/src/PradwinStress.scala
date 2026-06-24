// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.maths

// Standalone Monte Carlo stress harness for Pradwin — NOT part of the JUnit suite.
// Run with:  mill maths.test.runMain kse.test.maths.PradwinStress
//
// It measures (1) the false-positive rate on pure noise of several distributions (a check on the
// p<0.05 calibration), and (2) detection power p(found) and center-localization error as a step's
// SNR sweeps from sub-noise to obvious, each over many independent RNG draws.

import kse.maths.{_, given}

object PradwinStress {
  type Gen = (Long, Int) => Array[Double]   // (seed, n) => zero-mean, unit-σ noise

  val gaussian: Gen = (seed, n) => Pcg64(seed).arrayGaussian(n)

  val uniform: Gen = (seed, n) =>
    val a = Pcg64(seed).arrayD(n)
    val s = math.sqrt(12.0)               // U[0,1) → mean 0, σ 1
    var i = 0
    while i < n do { a(i) = (a(i) - 0.5) * s; i += 1 }
    a

  val exponential: Gen = (seed, n) =>
    val a = Pcg64(seed).arrayD(n)          // Exp(1) − 1 → mean 0, σ 1, right-skewed
    var i = 0
    while i < n do { a(i) = -math.log(1.0 - a(i)) - 1.0; i += 1 }
    a

  val quantized: Gen = (seed, n) =>
    val a = Pcg64(seed).arrayGaussian(n)   // Gaussian rounded to 0.5σ steps → heavy ties
    var i = 0
    while i < n do { a(i) = math.rint(a(i) * 2.0) / 2.0; i += 1 }
    a

  val noises = Seq("gaussian" -> gaussian, "uniform" -> uniform, "exponential" -> exponential, "quantized" -> quantized)

  /** Feed `gen` noise with a step of `step` added to the second half; return the located change. */
  def runStep(gen: Gen, seed: Long, n: Int, step: Double): Pradwin.Change =
    val data = gen(seed, n)
    val pw = Pradwin(capacity = n)
    val half = n / 2
    var i = 0
    while i < n do { pw.add(data(i) + (if i < half then 0.0 else step)); i += 1 }
    pw.locate()

  def bar(frac: Double, width: Int = 28): String =
    val k = math.rint(frac * width).toInt
    "#" * k + "." * (width - k)

  def main(args: Array[String]): Unit =
    val L = 2000

    println(s"=== False-positive rate (pure noise, no changepoint), L=$L, alpha=0.05 ===")
    val fpTrials = 400
    for (name, gen) <- noises do
      var fp = 0; var s = 0
      while s < fpTrials do
        if runStep(gen, 7_000_000L + s, L, 0.0).significant then fp += 1
        s += 1
      println(f"  $name%-12s false-positive = ${fp.toDouble / fpTrials}%.3f   (target ~0.05, n=$fpTrials)")

    println(s"\n=== Power & localization vs SNR (true change at $L/2), trials=200 each ===")
    val snrs = Seq(0.1, 0.15, 0.2, 0.3, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0)
    val powTrials = 200
    val mid = L / 2
    for (name, gen) <- noises do
      println(f"\n  $name%-12s   ${"SNR"}%5s  ${"p(found)"}%8s  ${"errMed"}%6s  ${"errMean"}%7s")
      for snr <- snrs do
        var found = 0
        val errs = collection.mutable.ArrayBuffer.empty[Long]
        var s = 0
        while s < powTrials do
          val c = runStep(gen, 20_000_000L + s * 131 + (snr * 1000).toLong, L, snr)
          if c.significant then { found += 1; errs += math.abs(c.at - mid) }
          s += 1
        val pFound = found.toDouble / powTrials
        val sorted = errs.toArray.sorted
        val errMed = if sorted.nonEmpty then sorted(sorted.length / 2) else -1L
        val errMean = if sorted.nonEmpty then sorted.sum.toDouble / sorted.length else Double.NaN
        println(f"               $snr%5.2f  $pFound%8.2f  $errMed%6d  $errMean%7.1f  ${bar(pFound)}")
}
