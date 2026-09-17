// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.test.thyme

// Every example in GUIDE-thyme.md lives here between a `// guide: name` and `// guide: end` pair,
// so that the guide's code is compiled and run.  `check-guides.py` verifies the two stay identical.

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics.{given, *}
import kse.thyme.{given, *}

object GuideExamples {

  def work(n: Int): Long =
    var s = 0L
    var i = 0
    while i < n do { s += i.toLong * (i ^ 0x5DEECE66L); i += 1 }
    s

  val table = Array.tabulate(1000)(i => i * 7L)
  val index = table.zipWithIndex.toMap
  def linear(key: Long): Int = table.indexOf(key)
  def hashed(key: Long): Int = index.getOrElse(key, -1)

  // guide: bench.plain
  def howLongPlain(n: Int): Double =
    val t0 = System.nanoTime
    var i = 0
    while i < 1000 do { work(n): Unit; i += 1 }        // cold, and the JIT may drop the unused result
    (System.nanoTime - t0) * 1e-9 / 1000
  // guide: end

  // guide: bench.kse3
  val th = Thyme()                                     // one per thread; targetTime, tooMuchTime, accuracyTarget are its knobs
  def howLong(n: Int): Thyme.Benched =
    th.bench(work(n))                                  // warmed to steady state, outlier-trimmed, with a 95% CI, in seconds
  // guide: end

  // guide: benchoff.kse3
  def whichIsFaster(n: Int): Thyme.Comparison =
    th.benchOff(work(n))(work(2 * n))                  // interleaved in mixtures, so drift cancels; winner is -1, 0, or 1
  // guide: end

  // guide: time.kse3
  def once(n: Int): (Long, Thyme.Report) =
    th.timePair(work(n))                               // one run: the value, and a report that flags GC, class loads, and JIT
  // guide: end


  // guide: parsley.kse3
  object Prof { val parsley = Parsley() }              // one per program, in a companion; it reports at close or at JVM exit
  def lookup(key: Long): Int =
    Prof.parsley.timeOff("linear", "hashed"){ linear(key) }{ hashed(key) }   // both run, in random order; one value returns
  def load(n: Int): Long =
    Prof.parsley.time{ work(n) }                       // one call site, one track; regimes (warmup, steady) split by themselves
  // guide: end
}


@RunWith(classOf[JUnit4])
class GuideTest {
  import kse.basics.testutilities.TestUtilities.{_, given}
  import GuideExamples as G

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  @Test
  def thymeTest(): Unit =
    G.th.targetTime = 0.01
    G.th.tooMuchTime = 2.0
    T ~ (G.howLongPlain(2000) > 0.0) ==== true
    val b = G.howLong(2000)
    T ~ (b.time > 0.0 && b.lo <= b.time && b.time <= b.hi) ==== true
    T ~ b.toString.startsWith("Benchmark:") ==== true
    val c = G.whichIsFaster(2000)
    T ~ c.winner ==== -1
    T ~ (c.costSecond > c.costFirst) ==== true
    val (v, r) = G.once(2000)
    T ~ v ==== G.work(2000)
    T ~ (r.elapsed >= 0.0 && r.effort == 1L) ==== true

  @Test
  def parsleyTest(): Unit =
    var i = 0
    while i < 40 do
      G.lookup((i * 7).toLong): Unit
      G.load(500): Unit
      i += 1
    val rs = G.Prof.parsley.results
    T ~ rs.length ==== 2
    val off = rs.find(_._2.exists(_.label == "linear")).get._2
    T ~ off.map(_.label).toSet ==== Set("linear", "hashed", "ratio")
    T ~ off.find(_.label == "linear").get.overall.n ==== 40L
    T ~ Parsley.formatReport(G.Prof.parsley).startsWith("Parsley: 2 site(s)") ==== true
    G.Prof.parsley.close()
}
