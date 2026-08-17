// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.twodee


import java.lang.{Math => jm}

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.twodee.*


@RunWith(classOf[JUnit4])
class TicksTest {
  import kse.basics.testutilities.TestUtilities.{given, _}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  def labs(lo: Double, hi: Double, target: Int): List[String] = Ticks.linear(lo, hi, target).labels.toList

  @Test
  def shortestSpellingTest(): Unit =
    // the motivating example: 1.02 is in span and rounder than the step, so it appears
    // exactly and the rest of the grid builds out from it in even 0.002 steps
    T ~ labs(1.011, 1.0248, 7) ==== List("1.012", "1.014", "1.016", "1.018", "1.02", "1.022", "1.024")
    T ~ labs(0.0, 1.0, 5) ==== List("0", "0.2", "0.4", "0.6", "0.8", "1")
    T ~ labs(0.94, 1.06, 6) ==== List("0.94", "0.96", "0.98", "1", "1.02", "1.04", "1.06")
    T ~ labs(99.4, 102.6, 6) ==== List("99.5", "100", "100.5", "101", "101.5", "102", "102.5")

  @Test
  def rounderValuesAppearTest(): Unit =
    // any in-span value rounder than the step is a tick; steps with mantissa 2.5, 3, or 4
    // would skip these, which is exactly why they are excluded
    T ~ labs(0.007, 0.033, 5) ==== List("0.01", "0.015", "0.02", "0.025", "0.03")
    T ~ labs(97.0, 143.0, 5).contains("100") ==== true
    T ~ labs(3.7, 8.2, 4) ==== List("4", "5", "6", "7", "8")

  @Test
  def zeroAndNegativeTest(): Unit =
    T ~ labs(-0.03, 0.07, 5) ==== List("-0.02", "0", "0.02", "0.04", "0.06")
    T ~ labs(-1.0, -0.2, 4) ==== List("-1", "-0.8", "-0.6", "-0.4", "-0.2")

  @Test
  def endpointsExactTest(): Unit =
    // limits that are themselves decimal ticks land on the grid despite roundoff
    T ~ labs(1.02, 1.03, 5) ==== List("1.02", "1.022", "1.024", "1.026", "1.028", "1.03")
    T ~ labs(0.1 + 0.2, 0.9, 3) ==== List("0.4", "0.6", "0.8")

  @Test
  def scientificSwitchTest(): Unit =
    // the whole axis switches to lowercase-e scientific only when plain labels run past
    // six characters AND scientific is strictly shorter; zero stays "0" either way
    T ~ labs(0.0, 1.2e6, 6) ==== List("0", "2e5", "4e5", "6e5", "8e5", "1e6", "1.2e6")
    T ~ labs(1e-5, 9.1e-5, 5) ==== List("2e-5", "4e-5", "6e-5", "8e-5")
    T ~ labs(0.0, 500000.0, 5) ==== List("0", "100000", "200000", "300000", "400000", "500000")
    // a fine grid far from zero needs its digits anyway, so plain wins there
    T ~ labs(999000.0, 1001000.0, 4) ==== List("999000", "999500", "1000000", "1000500", "1001000")

  @Test
  def atLeastTwoTicksTest(): Unit =
    // one lone tick gives a position but no scale, so the step refines until a second
    // tick lands: +-5 just outside the span leaves only "0" at step 5, so step 2 it is
    T ~ labs(-4.97, 4.97, 2) ==== List("-4", "-2", "0", "2", "4")
    // a narrow window between round numbers refines twice
    T ~ labs(2.1, 2.9, 1) ==== List("2.2", "2.4", "2.6", "2.8")
    T ~ labs(0.97, 1.03, 1) ==== List("0.98", "1", "1.02")

  @Test
  def minorTicksTest(): Unit =
    // minors subdivide by the largest {1,2,5} step dividing the major step at least four
    // ways: mantissa 1 -> fifths, 2 -> quarters, 5 -> fifths; exact majors are excluded
    val t2 = Ticks.linear(0.0, 1.0, 5)          // majors every 0.2 -> minors every 0.05
    T ~ t2.minor.length ==== 15
    T ~ t2.minor.contains(0.05) ==== true
    T ~ t2.minor.contains(0.15) ==== true
    T ~ t2.minor.count(v => t2.values.contains(v)) ==== 0
    val t1 = Ticks.linear(0.0, 10.0, 8)         // majors every 1 -> minors every 0.2
    T ~ t1.minor.length ==== 40
    val t5 = Ticks.linear(0.0, 50.0, 8)         // majors every 5 -> minors every 1
    T ~ t5.minor.length ==== 40
    T ~ t5.minor.contains(1.0) ==== true
    T ~ t5.minor.contains(5.0) ==== false
    // minors run past the outermost majors to the span edges
    val tf = Ticks.linear(1.011, 1.0248, 7)
    T ~ (tf.minor.min < 1.012) ==== true
    T ~ (tf.minor.max > 1.024) ==== true

  @Test
  def degenerateSpanTest(): Unit =
    T ~ Ticks.linear(1.0, 1.0, 5).length ==== 0
    T ~ Ticks.linear(2.0, 1.0, 5).length ==== 0
    T ~ Ticks.linear(Double.NaN, 1.0, 5).length ==== 0
    T ~ Ticks.linear(0.0, Double.PositiveInfinity, 5).length ==== 0

  @Test
  def exactRoundTripTest(): Unit =
    // labels ARE the values: parsing a label recovers the position Double exactly, no
    // label ends in a redundant zero, exponents are lowercase, counts track the target
    var seed = 872549213L
    def rnd(): Double =
      seed = seed * 6364136223846793005L + 1442695040888963407L
      (seed >>> 11).toDouble / (1L << 53).toDouble
    var trial = 0
    while trial < 500 do
      val p = (rnd() * 17).toInt - 8
      val base = (rnd() * 20 - 10) * jm.pow(10.0, p)
      val span = (0.1 + 9.9 * rnd()) * jm.pow(10.0, p)
      val target = 2 + (rnd() * 9).toInt
      val t = Ticks.linear(base, base + span, target)
      T ~ (t.length >= 2) ==== true
      T ~ (t.length <= jm.max(5, 3 * target / 2 + 2)) ==== true
      var i = 0
      while i < t.length do
        val s = t.labels(i)
        T ~ java.lang.Double.parseDouble(s) ==== t.values(i)
        T ~ s.contains("E") ==== false
        if s.contains('.') && !s.contains('e') then T ~ s.endsWith("0") ==== false
        i += 1
      var sorted = true
      i = 1
      while i < t.minor.length do
        if !(t.minor(i) > t.minor(i - 1)) then sorted = false
        i += 1
      T ~ sorted ==== true
      trial += 1
}
