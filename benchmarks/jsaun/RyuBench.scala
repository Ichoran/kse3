// JMH benchmark: shortest-round-trip Double rendering -- kse.maths.Ryu vs the JDK.
//
// The JDK's Double.toString (JDK 19+) already renders the shortest round-tripping decimal;
// Ryu's job is to produce the identical bytes with no allocation, written straight into a
// caller-supplied buffer, which is what a serializer wants.  `jdkToString` is the String-
// allocating call jsaun's byte path used to make; `jdkAppendSb` is the StringBuilder path
// (the JDK writes digits into the builder without an intermediate String), which is what
// jsaun's String path still uses; `ryuAppend` is the new byte-buffer kernel.
//
// The `kind` axis: "full" = random finite bit patterns (~17 significant digits, the hard
// case and the honest one for data interchange); "data" = milli-precision values (short
// decimals, the friendly case).
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              scala-cli --power run benchmarks/jsaun --jmh -- RyuBench
// Pin for stability, e.g.:                    taskset -c 2,3 scala-cli --power run benchmarks/jsaun --jmh -- RyuBench

//> using scala 3.8.3
//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar

package kse.bench.jsaun

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import kse.maths.Ryu


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class RyuBench {
  @Param(Array("full", "data"))
  var kind: String = ""

  var ds: Array[Double] = Array.empty
  val buf = new Array[Byte](32)
  val sb = new java.lang.StringBuilder(32)

  @Setup(Level.Trial)
  def setup(): Unit =
    val r = new java.util.Random(0x5EED5L)
    ds = kind match
      case "full" =>
        Array.fill(1000) {
          var d = java.lang.Double.longBitsToDouble(r.nextLong())
          while d.isNaN || d.isInfinite do d = java.lang.Double.longBitsToDouble(r.nextLong())
          d
        }
      case _ =>
        Array.fill(1000)((r.nextInt(2000001) - 1000000) * 0.001)
    // Cross-check while we are here: identical output up to the deliberate lowercase exponent
    for d <- ds do
      val n = Ryu.append(buf, 0, d)
      val s = new String(buf, 0, n, java.nio.charset.StandardCharsets.ISO_8859_1)
      assert(s.equalsIgnoreCase(java.lang.Double.toString(d)), s"$s != $d")

  @Benchmark
  @OperationsPerInvocation(1000)
  def ryuAppend(bh: Blackhole): Unit =
    var i = 0
    while i < ds.length do
      bh.consume(Ryu.append(buf, 0, ds(i)))
      i += 1

  @Benchmark
  @OperationsPerInvocation(1000)
  def jdkToString(bh: Blackhole): Unit =
    var i = 0
    while i < ds.length do
      bh.consume(java.lang.Double.toString(ds(i)))
      i += 1

  @Benchmark
  @OperationsPerInvocation(1000)
  def jdkAppendSb(bh: Blackhole): Unit =
    var i = 0
    while i < ds.length do
      sb.setLength(0)
      bh.consume(sb.append(ds(i)).length)
      i += 1

  // Precision-limited rendering (3 significant figures) three ways: the Ryu interval-widening
  // kernel (what Jstyle sig/fixed/limit styles now use), java.util.Formatter, and the
  // BigDecimal round-and-print shape the styles used before.

  @Benchmark
  @OperationsPerInvocation(1000)
  def ryuFmtSig3(bh: Blackhole): Unit =
    var i = 0
    while i < ds.length do
      bh.consume(Ryu.fmt(buf, 0, ds(i), 0, 3))
      i += 1

  @Benchmark
  @OperationsPerInvocation(1000)
  def jdkFormatG3(bh: Blackhole): Unit =
    var i = 0
    while i < ds.length do
      bh.consume(String.format("%.3g", ds(i)))
      i += 1

  @Benchmark
  @OperationsPerInvocation(1000)
  def bigDecimalSig3(bh: Blackhole): Unit =
    var i = 0
    while i < ds.length do
      bh.consume(new java.math.BigDecimal(ds(i)).round(new java.math.MathContext(3)).stripTrailingZeros.toString)
      i += 1
}
