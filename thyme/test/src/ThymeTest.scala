// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and Calico Life Sciences LLC.

package kse.test.thyme


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics.{given, _}
import kse.maths.{given, _}
import kse.thyme.{given, _}


@RunWith(classOf[JUnit4])
class ThymeTest {
  import kse.basics.testutilities.TestUtilities.{_, given}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  def busywork(n: Int): Long =
    var s = 0L
    var i = 0
    while i < n do
      s += i.toLong * (i ^ 0x5DEECE66L)
      i += 1
    s


  @Test
  def humanTimeTest(): Unit =
    T ~ Thyme.humanTime(2.5)   ==== "2.500 s"
    T ~ Thyme.humanTime(2.5e-3).endsWith("ms") ==== true
    T ~ Thyme.humanTime(2.5e-6).endsWith("µs") ==== true
    T ~ Thyme.humanTime(2.5e-9).endsWith("ns") ==== true
    T ~ Thyme.humanTime(0.0).endsWith("ns")    ==== true


  @Test
  def clockTest(): Unit =
    val th = new Thyme()

    // clock returns a non-negative, finite elapsed time.
    val dt = th.clock(busywork(100000))
    T ~ (dt >= 0.0) ==== true
    T ~ dt.finite    ==== true

    // clockPair hands back the computed value alongside the time (the value's escape route).
    val (v, _) = th.clockPair(busywork(1000))
    T ~ v ==== busywork(1000)

    // pclock returns the value too.
    T ~ th.pclock(busywork(1000)) ==== busywork(1000)

    // Single-shot calls accept any return type, including Unit.
    T ~ (th.clock(busywork(10).toInt)        >= 0.0) ==== true
    T ~ (th.clock(busywork(10).toDouble)     >= 0.0) ==== true
    T ~ (th.clock(busywork(10) > 0)          >= 0.0) ==== true
    T ~ (th.clock("a string of length " + 7) >= 0.0) ==== true
    T ~ (th.clock(())                        >= 0.0) ==== true


  @Test
  def benchTest(): Unit =
    val th = new Thyme()
    th.targetTime = 0.01   // keep the test quick

    // Basic sanity: a positive, finite estimate inside its own CI, built from real samples.
    val b = th.bench(busywork(1000))
    T ~ (b.time > 0.0)                       ==== true
    T ~ b.time.finite                        ==== true
    T ~ (b.lo <= b.time && b.time <= b.hi)   ==== true
    T ~ (b.totalCalls > 0L)                  ==== true
    T ~ (b.samples >= 1)                     ==== true
    T ~ b.toString.startsWith("Benchmark:")  ==== true

    // The point of Thyme: it must get the RELATIVE answer right.  Doubling the work should roughly
    // double the time.  Bounds are loose to tolerate timing noise but tight enough to catch a
    // badly-wrong (e.g. un-warmed, or wrong-by-an-order-of-magnitude) result.
    val b1 = th.bench(busywork(2000))
    val b2 = th.bench(busywork(4000))
    val ratio = b2.time / b1.time
    T(s"doubling work gave ratio $ratio (expected ~2)") ~ (ratio > 1.4 && ratio < 2.8) ==== true


  @Test
  def benchOffTest(): Unit =
    val th = new Thyme()
    th.targetTime = 0.01
    th.tooMuchTime = 2.0

    // First (1000 iters) is faster than Second (2000 iters): winner should be the first, ~2x.
    val c = th.benchOff(busywork(1000))(busywork(2000))
    T ~ (c.winner == -1)                ==== true
    T ~ c.significant                   ==== true
    T ~ (c.costSecond > c.costFirst)    ==== true
    T(s"ratio ${c.ratio} (expected ~2)") ~ (c.ratio > 1.4 && c.ratio < 2.8) ==== true
    T ~ c.toString.startsWith("Head-to-head:") ==== true

    // Swapping the arguments flips the verdict.
    val c2 = th.benchOff(busywork(2000))(busywork(1000))
    T ~ (c2.winner == 1)                ==== true
    T ~ (c2.costFirst > c2.costSecond)  ==== true

    // Identical work must not be reported as wildly different (no crying wolf).
    val c3 = th.benchOff(busywork(1000))(busywork(1000))
    T(s"same-work ratio ${c3.ratio}") ~ (c3.ratio > 0.8 && c3.ratio < 1.25) ==== true


  @Test
  def parsleyTest(): Unit =
    // Custom onClose captures the report rather than printing it.
    var closes = 0
    var captured = Vector.empty[(String, Vector[MultiRadwin.Track])]
    val p = Parsley(px => { closes += 1; captured = px.results })

    // time: returns the value, records the run at this call site.
    val a = p.time(busywork(5000))
    T ~ a ==== busywork(5000)

    // Repeated calls at one call site (the loop body line) accumulate as one site.
    var i = 0
    while i < 50 do { p.time(busywork(1000)): Unit; i += 1 }

    // timeOff "both": both run (randomized order), the value of whichever ran first is returned —
    // so every result is one of the two alternatives' values, but which one varies by coin flip.
    val vSlow = busywork(4000)
    val vFast = busywork(1000)
    var allValid = true
    var j = 0
    while j < 40 do
      val r = p.timeOff("slow", "fast"){ busywork(4000) }{ busywork(1000) }
      if r != vSlow && r != vFast then allValid = false
      j += 1
    T ~ allValid                        ==== true     // "both" returns whichever alternative ran first

    // timeOff "pick": exactly one runs per call.
    var k = 0
    while k < 40 do { p.timeOff("x", "y", mode = "pick"){ busywork(200) }{ busywork(200) }: Unit; k += 1 }

    val rs = p.results
    T ~ rs.nonEmpty ==== true

    // The "both" site has both alternatives + a synthesized "ratio" channel.
    val off = rs.find(_._2.exists(_.label == "slow")).get._2
    T ~ off.length                                 ==== 3       // slow, fast, ratio
    val slow = off.find(_.label == "slow").get.overall
    val fast = off.find(_.label == "fast").get.overall
    T ~ slow.n                                     ==== 40L
    T ~ fast.n                                     ==== 40L
    T ~ (slow.median > fast.median)                ==== true
    T ~ (slow.q90 >= slow.median)                  ==== true
    val ratio = off.find(_.label == "ratio").get.overall
    T ~ (ratio.median < 0.5)                       ==== true     // ratio = fast/slow ≈ 0.25

    // The "pick" site ran exactly one alternative per call, so the two counts sum to 40.
    val pick = rs.find(_._2.exists(_.label == "x")).get._2
    T ~ (pick.find(_.label == "x").get.overall.n + pick.find(_.label == "y").get.overall.n) ==== 40L

    // close runs onClose exactly once and is idempotent.
    p.close()
    T ~ closes          ==== 1
    p.close()
    T ~ closes          ==== 1
    T ~ captured.nonEmpty ==== true

    // After close, further records are ignored.
    p.time(busywork(100)): Unit
    T ~ p.results.length ==== rs.length


  @Test
  def parsleySegmentTest(): Unit =
    // Drive the regime detector with synthetic timings: a warmup regime (~5) then steady state (~1).
    val p = Parsley(_ => (), segCadence = 32, segCapacity = 1024, segMinSeg = 20, segAlpha = 0.01)
    val rng = Pcg64(424242L)
    var i = 0
    while i < 300  do { p.recordOne("warm", "t", 5.0 + 0.1 * rng.gaussian); i += 1 }   // warmup
    i = 0
    while i < 2000 do { p.recordOne("warm", "t", 1.0 + 0.1 * rng.gaussian); i += 1 }   // steady

    val tr = p.results.find(_._1 == "warm").get._2.find(_.label == "t").get
    T ~ tr.overall.n                          ==== 2300L
    T ~ (tr.segments.length >= 2)             ==== true       // warmup split off from steady
    T ~ (tr.segments.head.median > 3.0)       ==== true       // first regime is the slow warmup
    T ~ (tr.segments.last.median < 2.0)       ==== true       // last regime is steady state
    // The split lands near the true boundary: the first (warmup) segment is roughly 300 calls.
    T ~ (tr.segments.head.n >= 250L && tr.segments.head.n <= 450L) ==== true

    // No false regimes on a clean stationary stream.
    val q = Parsley(_ => (), segCadence = 32, segMinSeg = 20, segAlpha = 0.001)
    i = 0
    while i < 2000 do { q.recordOne("flat", "t", 2.0 + 0.1 * rng.gaussian); i += 1 }
    T ~ q.results.find(_._1 == "flat").get._2.find(_.label == "t").get.segments.length ==== 1

    p.close()
    q.close()


  @Test
  def timeTest(): Unit =
    val th = new Thyme()
    val (v, report) = th.timePair(busywork(200000))
    T ~ v ==== busywork(200000)
    T ~ (report.elapsed >= 0.0) ==== true
    T ~ report.effort           ==== 1L
    T ~ (report.perOp == report.elapsed) ==== true
    // A clean, brief run usually loads no classes and triggers no GC, hence is not suspect;
    // but we don't assert that (environment-dependent).  We do assert the report renders.
    T ~ (report.toString.startsWith("Elapsed:")) ==== true
}
