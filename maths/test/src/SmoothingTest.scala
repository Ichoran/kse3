// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.maths


import java.lang.{Math => jm}

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.maths.Smoothing


@RunWith(classOf[JUnit4])
class SmoothingTest {
  import kse.basics.testutilities.TestUtilities.{given, _}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  val xs = Array.tabulate(25)(i => i.toDouble)

  def maxErr(a: Array[Double], b: Array[Double]): Double =
    var m = 0.0
    var i = 0
    while i < a.length do
      if jm.abs(a(i) - b(i)) > m then m = jm.abs(a(i) - b(i))
      i += 1
    m

  @Test
  def loessExactnessTest(): Unit =
    val lin = xs.map(x => 2.0 * x + 1.0)
    val quad = xs.map(x => x * x - 3.0 * x + 2.0)
    // local-linear reproduces straight lines exactly at any span
    T ~ (maxErr(Smoothing.loessAt(xs, lin, xs, span = 0.3, degree = 1), lin) < 1e-9) ==== true
    T ~ (maxErr(Smoothing.loessAt(xs, lin, xs, span = 1.0, degree = 1), lin) < 1e-9) ==== true
    // local-quadratic reproduces parabolas exactly; local-linear cannot
    T ~ (maxErr(Smoothing.loessAt(xs, quad, xs, span = 0.5, degree = 2), quad) < 1e-8) ==== true
    T ~ (maxErr(Smoothing.loessAt(xs, quad, xs, span = 0.5, degree = 1), quad) > 0.01) ==== true
    // evaluation off the data grid stays on the line
    val mid = Array.tabulate(24)(i => i + 0.5)
    T ~ (maxErr(Smoothing.loessAt(xs, lin, mid, span = 0.4, degree = 1), mid.map(x => 2.0 * x + 1.0)) < 1e-9) ==== true

  @Test
  def loessRobustTest(): Unit =
    val y = xs.map(x => 0.5 * x + 3.0)
    y(12) += 100.0  // one wild outlier
    val plain = Smoothing.loessAt(xs, y, xs, span = 0.6, degree = 1)
    val robust = Smoothing.loessAt(xs, y, xs, span = 0.6, degree = 1, robustIters = 3)
    val truth = xs.map(x => 0.5 * x + 3.0)
    // ignore the outlier's own position; robustness should beat the plain fit elsewhere
    var pe = 0.0
    var re = 0.0
    var i = 0
    while i < xs.length do
      if i != 12 then
        pe = jm.max(pe, jm.abs(plain(i) - truth(i)))
        re = jm.max(re, jm.abs(robust(i) - truth(i)))
      i += 1
    T ~ (re < pe / 5) ==== true
    T ~ (re < 0.5) ==== true

  @Test
  def kernelTest(): Unit =
    val const = Array.fill(25)(7.5)
    val lin = xs.map(x => -1.5 * x + 4.0)
    T ~ (maxErr(Smoothing.kernelAt(xs, const, xs, bandwidth = 3.0, degree = 0), const) < 1e-9) ==== true
    // local-linear with any kernel reproduces straight lines exactly
    T ~ (maxErr(Smoothing.kernelAt(xs, lin, xs, bandwidth = 3.0, degree = 1), lin) < 1e-9) ==== true
    T ~ (maxErr(Smoothing.kernelAt(xs, lin, xs, bandwidth = 2.0, shape = Smoothing.Shape.Epanechnikov, degree = 1), lin) < 1e-9) ==== true
    // a compact kernel far from all data falls back to the nearest datum
    val far = Smoothing.kernelAt(xs, lin, Array(1000.0), bandwidth = 0.5, shape = Smoothing.Shape.Tricube, degree = 1)
    T ~ far(0) ==== lin(24)

  @Test
  def kdeTest(): Unit =
    val invSqrt2Pi = 0.3989422804014327
    // a single point's estimate is the kernel itself
    val one = Array(2.0)
    T ~ (jm.abs(Smoothing.kdeAt(one, Array(2.0), 1.0)(0) - invSqrt2Pi) < 1e-15) ==== true
    T ~ (jm.abs(Smoothing.kdeAt(one, Array(3.0), 1.0)(0) - invSqrt2Pi * jm.exp(-0.5)) < 1e-15) ==== true
    T ~ (jm.abs(Smoothing.kdeAt(one, Array(2.5), 1.0, Smoothing.Shape.Epanechnikov)(0) - 0.75 * (1 - 0.25)) < 1e-15) ==== true
    T ~ Smoothing.kdeAt(one, Array(4.0), 1.0, Smoothing.Shape.Epanechnikov)(0) ==== 0.0
    // bandwidth scaling: kde_h(x) = kde_1((x - x0)/h)/h
    T ~ (jm.abs(Smoothing.kdeAt(one, Array(3.0), 2.0)(0) - invSqrt2Pi * jm.exp(-0.125) / 2.0) < 1e-15) ==== true
    // the estimate integrates to one (trapezoid over a wide grid)
    val data = Array(0.0, 0.5, 1.5, 2.0, 4.0)
    val grid = Array.tabulate(4001)(i => -10.0 + i * 0.006)
    val dens = Smoothing.kdeAt(data, grid, 0.8)
    var area = 0.0
    var i = 1
    while i < grid.length do
      area += 0.5 * (dens(i) + dens(i - 1)) * (grid(i) - grid(i - 1))
      i += 1
    T ~ (jm.abs(area - 1.0) < 1e-6) ==== true
    // Silverman on 1..5: IQR/1.34 < sd, so h = 0.9 (IQR/1.34) 5^(-1/5)
    val five = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    T ~ (jm.abs(Smoothing.silvermanBandwidth(five) - 0.9 * (2.0 / 1.34) * jm.pow(5.0, -0.2)) < 1e-12) ==== true
    // constant data falls back through the chain to |x0|
    T ~ (jm.abs(Smoothing.silvermanBandwidth(Array(3.0, 3.0)) - 0.9 * 3.0 * jm.pow(2.0, -0.2)) < 1e-12) ==== true

  @Test
  def rollingTest(): Unit =
    val y = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    T ~ Smoothing.rollingMean(y, 3).toList ==== List(1.5, 2.0, 3.0, 4.0, 4.5)
    T ~ Smoothing.rollingMean(y, 1).toList ==== y.toList
    val spiky = Array(1.0, 1.0, 99.0, 1.0, 1.0)
    T ~ Smoothing.rollingMedian(spiky, 3).toList ==== List(1.0, 1.0, 1.0, 1.0, 1.0)

  @Test
  def polyFitTest(): Unit =
    val quad = xs.map(x => 0.25 * x * x - 2.0 * x + 5.0)
    T ~ (maxErr(Smoothing.polyFitAt(xs, quad, xs, degree = 2), quad) < 1e-8) ==== true
    // degree-1 fit matches the closed-form least-squares line
    val y = xs.map(x => 3.0 * x - 4.0 + (if x.toInt % 2 == 0 then 1.0 else -1.0))
    val n = xs.length
    val mx = xs.sum / n
    val my = y.sum / n
    var sxx = 0.0
    var sxy = 0.0
    var i = 0
    while i < n do
      sxx += (xs(i) - mx) * (xs(i) - mx)
      sxy += (xs(i) - mx) * (y(i) - my)
      i += 1
    val slope = sxy / sxx
    val icept = my - slope * mx
    val expect = xs.map(x => slope * x + icept)
    T ~ (maxErr(Smoothing.polyFitAt(xs, y, xs, degree = 1), expect) < 1e-8) ==== true
    // duplicate-x data cannot support degree 1: falls back to the mean
    val dup = Array(2.0, 2.0, 2.0)
    val dy = Array(1.0, 2.0, 3.0)
    T ~ (maxErr(Smoothing.polyFitAt(dup, dy, Array(2.0, 7.0), degree = 1), Array(2.0, 2.0)) < 1e-9) ==== true

  @Test
  def validationTest(): Unit =
    def throws(f: => Any): Boolean =
      try { val _ = f; false }
      catch case _: IllegalArgumentException => true
    T ~ throws(Smoothing.loessAt(Array(3.0, 1.0), Array(1.0, 2.0), Array(1.0))) ==== true
    T ~ throws(Smoothing.loessAt(Array(1.0, 2.0), Array(1.0), Array(1.0))) ==== true
    T ~ throws(Smoothing.loessAt(Array(1.0, 2.0), Array(1.0, 2.0), Array(1.0), span = 0.0)) ==== true
    T ~ throws(Smoothing.kernelAt(Array(1.0), Array(1.0), Array(1.0), bandwidth = 0.0)) ==== true
    T ~ throws(Smoothing.kdeAt(Array(1.0), Array(1.0), bandwidth = -1.0)) ==== true
    T ~ throws(Smoothing.kdeAt(Array[Double](), Array(1.0))) ==== true
    T ~ throws(Smoothing.rollingMean(Array(1.0), 0)) ==== true
    T ~ throws(Smoothing.polyFitAt(Array(1.0), Array(1.0), Array(1.0), degree = 9)) ==== true
}
