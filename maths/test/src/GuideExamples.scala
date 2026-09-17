// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.maths

// Every example in GUIDE-maths.md lives here between a `// guide: name` and `// guide: end` pair,
// so that the guide's code is compiled and run.  `check-guides.py` verifies the two stay identical.

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import java.lang.{Math => jm}

import kse.basics.{given, *}
import kse.maths.{given, *}
import kse.maths.packed.*
import kse.maths.fitting.*
import kse.maths.colours.*
import kse.maths.stringmaths.*

object GuideExamples {

  def numbersPlain(x: Double, lo: Double, hi: Double): (Double, Double, Double, Boolean) =
    // guide: numbers.plain
    val d = jm.sqrt(x * x + 1)
    val c = jm.max(lo, jm.min(hi, x))
    val t = lo + 0.25 * (hi - lo)
    val finite = !x.isNaN && !x.isInfinite
    // guide: end
    (d, c, t, finite)

  def numbersKse3(x: Double, lo: Double, hi: Double): (Double, Double, Double, Boolean) =
    // guide: numbers.kse3
    val d = (x.sq + 1).sqrt
    val c = x.clamp(lo, hi)
    val t = 0.25.between(lo, hi)
    val finite = x.finite
    // guide: end
    (d, c, t, finite)


  def arithmeticPlain(a: Int, b: Int): (Int, Int) =
    // guide: arithmetic.plain
    val exact = jm.addExact(a, b)
    val wide = a.toLong * b
    val clamped = if wide > Int.MaxValue then Int.MaxValue else if wide < Int.MinValue then Int.MinValue else wide.toInt
    // guide: end
    (exact, clamped)

  def arithmeticKse3(a: Int, b: Int): (Int, Int) =
    // guide: arithmetic.kse3
    val exact = a +! b                        // throws ArithmeticException on overflow
    val clamped = a *# b                      // saturates at Int.MinValue or Int.MaxValue
    // guide: end
    (exact, clamped)


  def unsigned(b: Byte, i: Int): (Int, String, Long, Byte, Int, Int) =
    // guide: unsigned.kse3
    val ub = b.u                              // a UByte: the same bits, read as 0 to 255
    val n = ub.toInt                          // 200 where b.toInt would be -56
    val text = i.u.pr                         // printed unsigned: "4294967295" for -1
    val wide = i.u.toLong                     // 4294967295L for -1
    val third = i.byte(2)                     // byte 2 of 4, counting from the low end
    val set = i.bitTo(0)(1)                   // i with bit 0 set
    val ones = i.bitCount
    // guide: end
    (n, text, wide, third, set, ones)


  def timePlain(work: () => Unit): (java.time.Duration, Long, java.time.Instant) =
    // guide: time.plain
    val timeout = java.time.Duration.ofMillis(1500)
    val t0 = System.nanoTime
    work()
    val elapsedNs = System.nanoTime - t0
    val deadline = java.time.Instant.now.plus(timeout)
    // guide: end
    (timeout, elapsedNs, deadline)

  def timeKse3(work: () => Unit): (java.time.Duration, NanoDuration, java.time.Instant) =
    // guide: time.kse3
    val timeout = 1500.ms                     // a java.time.Duration; 1.5.s would be a DoubleDuration
    val t0 = tic()
    work()
    val elapsed = t0.toc()                    // a NanoDuration: a Long of nanoseconds that saturates
    val deadline = Now() + timeout
    // guide: end
    (timeout, elapsed, deadline)

  def durations(d: java.time.Duration): (NanoDuration, DoubleDuration, java.time.Duration, Boolean) =
    // guide: durations.kse3
    val n = d.nano                            // saturates where d.toNanos would throw
    val secs = n.double                       // seconds as a Double
    val twice = d + d                         // java.time.Duration gains +, -, *, comparisons
    val soon = d < 2.s
    // guide: end
    (n, secs, twice, soon)


  def randomPlain(seed: Long, xs: Array[Int]): (Double, Int, Boolean, Double, Int) =
    // guide: random.plain
    val r = new scala.util.Random(seed)
    val x = r.nextDouble()
    val k = r.nextInt(6)
    val coin = r.nextBoolean()
    val g = r.nextGaussian()
    val pick = xs(r.nextInt(xs.length))
    // guide: end
    (x, k, coin, g, pick)

  def randomKse3(seed: Long, xs: Array[Int]): (Double, Int, Boolean, Double, Int, Int) =
    // guide: random.kse3
    val rng = Prng(seed)                      // Pcg64: fast, one Long of state, seedable
    val x = rng.D                             // uniform in [0, 1)
    val k = rng % 6                           // uniform in 0 until 6
    val coin = rng.Z
    val g = rng.gaussian
    val pick = rng.sample(xs)
    given AutoPrng = rng.givable              // lets the short forms find the generator
    val die = 6.roll                          // 1 to 6
    // guide: end
    (x, k, coin, g, pick, die)

  def hashingPlain(s: String, xs: Array[Byte]): (Int, Long) =
    // guide: hashing.plain
    val h = java.util.Arrays.hashCode(xs)                 // what HashMap uses: fast, weakly mixed
    val crc = new java.util.zip.CRC32()
    crc.update(xs)
    val check = crc.getValue
    // guide: end
    (h, check)

  def hashingKse3(s: String, xs: Array[Byte]): (Int, Long, Int, Long) =
    // guide: hashing.kse3
    val h = MurmurHash.hash32(xs, 0, xs.length)           // a well-mixed 32-bit hash of the bytes
    val big = XxHash.hash64(s, 0, s.length)               // 64 bits, straight from a String's chars
    val check = Crc32.hash32(xs, 0, xs.length)
    val mixed = MakeHasher.x64.begin(1234L).appendInt(xs.length).append(s, 0, s.length).result()
    // guide: end
    (h, big, check, mixed)


  def statsPlain(xs: Array[Double]): (Double, Double, Double) =
    // guide: stats.plain
    val n = xs.length
    val mean = xs.sum / n
    val sd = jm.sqrt(xs.map(x => (x - mean) * (x - mean)).sum / (n - 1))
    val sorted = xs.sorted
    val median = if n % 2 == 1 then sorted(n / 2) else (sorted(n / 2 - 1) + sorted(n / 2)) / 2
    // guide: end
    (mean, sd, median)

  def statsKse3(xs: Array[Double]): (Double, Double, Double) =
    // guide: stats.kse3
    val est = Est of xs                      // mean, sd, sem, and variance, in one pass
    val median = xs.median                   // R type-7, over a sorted copy of the finite values
    // guide: end
    (est.mean, est.sd, median)

  def running(stream: Iterator[Double], xs: Array[Double]): (Double, Double, Int, Int, PlusMinus) =
    // guide: running.kse3
    val est = Est.M()                        // a running mean and variance you add to
    val hist = Hist(10)                      // ten bins; out-of-range values count as outliers
    stream.foreach: x =>
      est += x
      hist += (x * 10).toInt
    val boot = Bootstrap(200)(Prng(1))(0, xs.length)(Est.M())((e, i) => e += xs(i))
    val meanPm = boot.pm(_.mean)             // the mean with its bootstrap standard error
    // guide: end
    (est.mean, est.sem, hist.count(3), hist.outliers, meanPm)


  def streams(latencies: Iterator[Double]): (Double, Double, Long, Boolean, Double) =
    // guide: streams.kse3
    val sk = UDDSketch()                     // quantiles to 1% relative error in a few kilobytes
    val ad = Adwin()                         // an adaptive window that drops old data when the mean shifts
    var shifted = false
    latencies.foreach: x =>
      sk += x
      if ad.add(x) then shifted = true
    // guide: end
    (sk.quantile(0.99), sk.median, sk.count, shifted, ad.mean)


  def fittingPlain(xs: Array[Double], ys: Array[Double]): (Double, Double) =
    // guide: fitting.plain
    val n = xs.length
    val mx = xs.sum / n
    val my = ys.sum / n
    var sxy = 0.0
    var sxx = 0.0
    var i = 0
    while i < n do
      sxy += (xs(i) - mx) * (ys(i) - my)
      sxx += (xs(i) - mx) * (xs(i) - mx)
      i += 1
    val slope = sxy / sxx
    val intercept = my - slope * mx
    // guide: end
    (slope, intercept)

  def fittingKse3(xs: Array[Double], ys: Array[Double]): (Double, Double, Double, Double) =
    // guide: fitting.kse3
    val fit = FitLine.Impl()
    fit.addRange(xs, 0, xs.length)(ys, 0, ys.length)
    val slope = fit.x2y.slope
    val intercept = fit.x2y.intercept
    val at3 = fit.x2y(3.0)                   // the fitted y at x = 3
    val robust = TheilSen.fit(xs, ys)        // median of pairwise slopes, with a confidence interval
    // guide: end
    (slope, intercept, at3, robust.slope)

  def circleAndRoots(pts: Array[Vc]): (Circle2D, Int, Array[Double], Array[Double]) =
    // guide: circle.kse3
    val fc = FitCirc()
    fc.addRange(pts, 0, pts.length)
    val circle = fc.circle                   // x, y, r; NaN when the points are degenerate
    val roots = new Array[Double](2)
    val n = Roots.quadratic(-6, 1, 1, roots) // -6 + x + x^2: writes the 2 real roots, -3 and 2
    val smooth = Smoothing.rollingMedian(Array(1.0, 9.0, 2.0, 3.0, 8.0), 3)
    // guide: end
    (circle, n, roots, smooth)

  def vectors(): (Double, Vec2D, Vec2D, Vec2D, PlusMinus) =
    // guide: vectors.kse3
    val v = Vec2D(1.2, 3.1)                   // Doubles; Vc(1.2f, 3.1f) packs two Floats into one Long
    val u = Vec2D(-1.5, 0.7)
    val dot = v * u                           // dot product; v X u is the 2D cross product
    val n = v.hat                             // unit vector; also len, theta, rotate(angle), proj, orth
    val m = Mat22D(0, -1)(1, 0)               // rows as written; m * v applies it, m.T transposes with no copy
    val r = m * v
    val x = Xform2D(m, Vec2D(10, 0))          // an affine transform: x(p) moves a point, x.dir(d) a direction
    val p = x(v)
    val pm = 2.5f +- 0.1f                     // a value with an error, as a PlusMinus
    // guide: end
    (dot, n, r, p, pm)


  def colours(): (Rgb, Boolean, String, Float, Argb, Rgb, Ehsv) =
    // guide: colour.kse3
    val c = Rgb(250, 128, 114)                // 8-bit sRGB; Rgb.F(0.98f, 0.5f, 0.45f) from unit floats
    val named = Rgb.Salmon == c               // the CSS names as constants; Rgb.byName("salmon") from text
    val hex = c.pr                            // "#FA8072"
    val red = c.rF                            // channels as UBytes (r, g, b), Ints (rI), or unit floats (rF)
    val faded = c.aTo(0.5)                    // an Argb with alpha
    val lab = Oklab.sRGB(c)                   // perceptual space, for mixing that looks right
    val dusk = Oklab.blend(lab, 0.5f)(Oklab.sRGB(Rgb.Black), 0.5f).rgb
    val hsv = Ehsv.from(c)                    // hue as a turn fraction, saturation, value
    // guide: end
    (c, named, hex, red, faded, dusk, hsv)


  def numbersAsText(): (String, String, Long, List[String], String, String) =
    // guide: text.kse3
    val text = Ryu.string(0.1 + 0.2)          // "0.30000000000000004": the shortest digits that round-trip
    val three = Ryu.fmt(0.1 + 0.2, 0, 3)      // "0.3": at most three significant figures, the shortest decimal within that
    val n = Parse.long("12345")               // failure is the in-band Parse.failLong; see spellsFailLong
    val order = List("file10", "file2").sorted(using SemanticOrder)
    val nine = RomanNumber.text(9)            // "IX"
    val words = SpokenNumber.text(42L.u)      // "forty-two"
    // guide: end
    (text, three, n, order, nine, words)
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
  def numbersAndArithmeticTest(): Unit =
    for x <- List(-3.0, 0.0, 2.5, 12.0, Double.NaN) do
      val (d0, c0, t0, f0) = G.numbersPlain(x, 0.0, 10.0)
      val (d1, c1, t1, f1) = G.numbersKse3(x, 0.0, 10.0)
      T(x.toString) ~ (d0 === d1, c0 === c1, t0, f0) ==== (true, true, t1, f1)
    T ~ G.numbersKse3(2.5, 0.0, 10.0) ==== (jm.sqrt(7.25), 2.5, 2.5, true)
    for (a, b) <- List((3, 4), (46341, 46341), (-46341, 46341), (-7, 3)) do
      T(s"$a $b") ~ G.arithmeticPlain(a, b)._2 ==== G.arithmeticKse3(a, b)._2
    T ~ G.arithmeticKse3(3, 4) ==== (7, 12)
    T ~ G.arithmeticKse3(46341, 46341)._2 ==== Int.MaxValue
    T ~ G.arithmeticKse3(-46341, 46341)._2 ==== Int.MinValue
    T ~ G.arithmeticKse3(Int.MaxValue, 1) ==== thrown[ArithmeticException]
    T ~ G.arithmeticPlain(Int.MaxValue, 1) ==== thrown[ArithmeticException]

  @Test
  def unsignedAndTimeTest(): Unit =
    val (n, text, wide, third, set, ones) = G.unsigned(200.toByte, -1)
    T ~ n ==== 200
    T ~ text ==== "4294967295"
    T ~ wide ==== 4294967295L
    T ~ third ==== (-1).toByte
    T ~ set ==== -1
    T ~ ones ==== 32
    T ~ G.unsigned(0, 0x04030200)._4 ==== 3.toByte
    T ~ G.unsigned(0, 0x04030200)._5 ==== 0x04030201
    var worked = 0
    val (to0, ns0, dl0) = G.timePlain(() => worked += 1)
    val (to1, el1, dl1) = G.timeKse3(() => worked += 1)
    T ~ worked ==== 2
    T ~ to0 ==== to1
    T ~ (ns0 >= 0L) ==== true
    T ~ (el1 >= NanoDuration.Zero) ==== true
    T ~ (dl1 >= dl0) ==== true
    val (nd, secs, twice, soon) = G.durations(java.time.Duration.ofMillis(1500))
    T ~ nd.unwrap ==== 1500000000L
    T ~ secs.unwrap ==== 1.5
    T ~ twice ==== java.time.Duration.ofSeconds(3)
    T ~ soon ==== true
    T ~ G.durations(java.time.Duration.ofSeconds(Long.MaxValue))._1 ==== NanoDuration.MaxValue

  @Test
  def randomTest(): Unit =
    val xs = Array(10, 20, 30)
    val (x0, k0, c0, g0, p0) = G.randomPlain(42L, xs)
    T ~ (x0 >= 0 && x0 < 1) ==== true
    T ~ (k0 >= 0 && k0 < 6) ==== true
    T ~ xs.contains(p0) ==== true
    val (x1, k1, c1, g1, p1, die) = G.randomKse3(42L, xs)
    T ~ (x1 >= 0 && x1 < 1) ==== true
    T ~ (k1 >= 0 && k1 < 6) ==== true
    T ~ xs.contains(p1) ==== true
    T ~ (die >= 1 && die <= 6) ==== true
    T ~ G.randomKse3(42L, xs) ==== G.randomKse3(42L, xs)
    T ~ G.randomPlain(42L, xs) ==== G.randomPlain(42L, xs)

  @Test
  def hashingAndStatsTest(): Unit =
    val bytes = "salmon and cod".getBytes("UTF-8")
    val (h0, c0) = G.hashingPlain("salmon", bytes)
    val (h1, big, c1, mixed) = G.hashingKse3("salmon", bytes)
    T ~ (c1 & 0xFFFFFFFFL) ==== c0
    T ~ h1 ==== MurmurHash.hash32(bytes, 0, bytes.length)
    T ~ (h1 == h0) ==== false
    T ~ big ==== XxHash.hash64("salmon", 0, 6)
    T ~ mixed ==== MakeHasher.x64.begin(1234L).appendInt(bytes.length).append("salmon", 0, 6).result()
    val xs = Array(2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0)
    val (m0, s0, d0) = G.statsPlain(xs)
    val (m1, s1, d1) = G.statsKse3(xs)
    T ~ m1 =~~= m0
    T ~ s1 =~~= s0
    T ~ d1 =~~= d0
    T ~ (m1, d1) ==== (5.0, 4.5)
    val (rm, rsem, c3, out, pm) = G.running(xs.iterator.map(_ / 10), xs)
    T ~ rm =~~= 0.5
    T ~ rsem =~~= s0 / jm.sqrt(8) / 10
    T ~ c3 ==== 0
    T ~ out ==== 0
    T ~ (pm.value > 4 && pm.value < 6) ==== true
    T ~ (pm.error > 0 && pm.error < 2) ==== true

  @Test
  def streamsAndFittingTest(): Unit =
    val rng = Prng(7)
    val early = Array.fill(2000)(1.0 + 0.1 * rng.gaussian)
    val late = Array.fill(2000)(5.0 + 0.1 * rng.gaussian)
    val (q99, med, n, shifted, mean) = G.streams(early.iterator ++ late.iterator)
    T ~ n ==== 4000L
    T ~ shifted ==== true
    T ~ (mean > 4.5 && mean < 5.5) ==== true
    T ~ (q99 > 4.9 && q99 < 5.6) ==== true
    T ~ (med > 0.9 && med < 5.1) ==== true
    val xs = Array.tabulate(10)(_.toDouble)
    val ys = xs.map(x => 2 * x + 1)
    val (sl0, ic0) = G.fittingPlain(xs, ys)
    val (sl1, ic1, at3, rob) = G.fittingKse3(xs, ys)
    T ~ sl1 =~~= sl0
    T ~ ic1 =~~= ic0
    T ~ (sl1, ic1, at3, rob) ==== (2.0, 1.0, 7.0, 2.0)
    val pts = Array(Vc(1f, 0f), Vc(0f, 1f), Vc(-1f, 0f), Vc(0f, -1f), Vc(0.6f, 0.8f))
    val (circle, nroots, roots, smooth) = G.circleAndRoots(pts)
    T ~ ((circle.x - 0.0).abs < 1e-6) ==== true
    T ~ ((circle.y - 0.0).abs < 1e-6) ==== true
    T ~ ((circle.r - 1.0).abs < 1e-6) ==== true
    T ~ nroots ==== 2
    T ~ roots =**= Array(-3.0, 2.0)
    T ~ smooth =**= Array(5.0, 2.0, 3.0, 3.0, 5.5)   // the window shrinks at the edges

  @Test
  def vectorsColoursTextTest(): Unit =
    val (dot, n, r, p, pm) = G.vectors()
    T ~ dot =~~= 1.2 * -1.5 + 3.1 * 0.7
    T ~ n.len =~~= 1.0
    T ~ r ==== Vec2D(-3.1, 1.2)
    T ~ p ==== Vec2D(6.9, 1.2)
    T ~ (pm.value, pm.error) ==== (2.5f, 0.1f)
    val (c, named, hex, red, faded, dusk, hsv) = G.colours()
    T ~ named ==== true
    T ~ hex ==== "#FA8072"
    T ~ red =~~= 250f / 255f
    T ~ faded.pr ==== "#80FA8072"
    T ~ (dusk.rI < c.rI && dusk.rI > 0) ==== true
    T ~ (hsv.s > 0.5f && hsv.v > 0.9f) ==== true
    val (text, three, num, order, nine, words) = G.numbersAsText()
    T ~ three ==== "0.3"
    T ~ text ==== "0.30000000000000004"
    T ~ num ==== 12345L
    T ~ order ==== List("file2", "file10")
    T ~ nine ==== "IX"
    T ~ words ==== "forty-two"
}
