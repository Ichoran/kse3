// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.maths

// Standalone finite-N calibration of cdfKolmogorov used as the rank-CUSUM scan-maximum null — NOT
// part of the JUnit suite.  Run:  mill maths.test.runMain kse.test.maths.RadwinCalib
//
// For each N it Monte-Carlos the null (random orderings of a fixed rank multiset) and reports the
// ACTUAL rejection rate at nominal p-cutoffs, so we can document where the asymptotic is trustworthy.

import kse.maths.{_, given}
import kse.maths.NumericFunctions.cdfKolmogorov

object RadwinCalib {
  /** Standardized sup of the CUSUM of `scores` over all interior splits — the statistic whose null
    * is asymptotically the Kolmogorov (sup|Brownian bridge|) distribution.
    */
  def supCusum(scores: Array[Double]): Double =
    val n = scores.length
    var sum = 0.0; var i = 0
    while i < n do { sum += scores(i); i += 1 }
    val mean = sum / n
    var ss = 0.0; i = 0
    while i < n do { val d = scores(i) - mean; ss += d * d; i += 1 }
    if ss <= 0 then return 0.0
    val scale = math.sqrt(ss * n / (n - 1.0))
    var S = 0.0; var peak = 0.0; i = 0
    while i < n - 1 do
      S += scores(i) - mean
      val au = math.abs(S / scale)
      if au > peak then peak = au
      i += 1
    peak

  def permute(a: Array[Double], rng: Prng): Unit =
    var i = a.length - 1
    while i > 0 do
      val j = (rng % (i + 1)).toInt
      val t = a(i); a(i) = a(j); a(j) = t
      i -= 1

  def averageRanks(raw: Array[Double]): Array[Double] =
    val n = raw.length
    val order = (0 until n).sortBy(raw).toArray
    val rank = new Array[Double](n)
    var k = 0
    while k < n do
      var j = k
      while j + 1 < n && raw(order(j + 1)) == raw(order(k)) do j += 1
      val avg = (k + j) / 2.0 + 1.0
      var t = k
      while t <= j do { rank(order(t)) = avg; t += 1 }
      k = j + 1
    rank

  def reject(scores: Array[Double], rng: Prng, b: Int): (Double, Double, Double) =
    var c10 = 0; var c05 = 0; var c01 = 0; var t = 0
    while t < b do
      permute(scores, rng)
      val p = 1.0 - cdfKolmogorov(supCusum(scores))
      if p < 0.10 then c10 += 1
      if p < 0.05 then c05 += 1
      if p < 0.01 then c01 += 1
      t += 1
    (c10.toDouble / b, c05.toDouble / b, c01.toDouble / b)

  def main(args: Array[String]): Unit =
    val rng = Pcg64(98765L)
    val B = 200000

    println("=== Finite-N calibration: actual rejection rate vs nominal (distinct ranks) ===")
    println(f"  ${"N"}%5s  ${"@0.10"}%7s  ${"@0.05"}%7s  ${"@0.01"}%7s")
    for n <- Seq(8, 12, 20, 30, 50, 100, 200, 500, 1000) do
      val (r10, r05, r01) = reject(Array.tabulate(n)(_ + 1.0), rng, B)
      println(f"  $n%5d  $r10%7.4f  $r05%7.4f  $r01%7.4f")

    println("\n=== Heavy ties: N values on G equally-likely levels (average ranks) ===")
    println(f"  ${"N"}%5s ${"G"}%4s  ${"@0.05"}%7s  ${"@0.01"}%7s")
    for (n, g) <- Seq((50, 4), (100, 8), (200, 4), (500, 8), (500, 2)) do
      val raw = Array.tabulate(n)(i => (i.toLong * g / n).toDouble)   // g levels, balanced
      val (_, r05, r01) = reject(averageRanks(raw), rng, B)
      println(f"  $n%5d $g%4d  $r05%7.4f  $r01%7.4f")
}
