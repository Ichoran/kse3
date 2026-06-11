// JMH benchmarks for ChanN, the chunked channel — same shapes as GoChanBench.scala so the
// Chan benchmarks there (spsc / selectN / fanIn) and their ABQ controls are the direct
// comparison points.  The thesis under test: chunked transport divides the per-item
// coordination costs (lock, cache traffic, park/unpark) by the batch size, so batch=1
// should reproduce Chan's numbers and batch=32 should approach the SplitDeque pipe ceiling
// (see benchmarks/splitdeque).  fanIn at batch=32 is the headline claim.
//
// Throughput cannot see the cost of batching: an item produced first in a batch *waits*
// for its batchmates before it can move.  ChanNLatencyBench / ChanLatencyBench measure
// per-item delivery latency in regimes where that matters (trickle: steady slow production;
// bursty: production in clumps), printing p50/p99/p999 per iteration from a power-of-two
// histogram (so percentiles are exact only to within 2x — fine for regime comparison).
//
// Run (from the repo root, after `mill all.assembly`):
//     scala-cli --power run benchmarks/loom --jmh -- ChanN
// Headline vs control:
//     scala-cli --power run benchmarks/loom --jmh -- -p producers=8 'FanInBench|ChanNFanInBench'

//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar

package kse.bench.loom

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import kse.basics.*
import kse.flow.*
import kse.loom.*


/** Single producer -> single consumer through ChanN: batched generation (`put(n)`) and
  * array drain (`putN`).  Compare to `SpscBench.spsc` / `.abqSpsc`. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ChanNSpscBench {
  import Bench.{Total, Pool, Mask}

  @Param(Array("1024"))
  var capacity: Int = 0

  @Param(Array("1", "8", "32"))
  var batch: Int = 0

  /** The producer side is also pre-materialized for `spscNArr`. */
  var src: Array[String] = null

  @Setup(Level.Trial)
  def setup(): Unit =
    src = Array.tabulate(Total)(i => Pool(i & Mask))

  @Benchmark
  @OperationsPerInvocation(Total)
  def spscN(): Long =
    val ch = ChanN[String](capacity, batch)
    var acc = 0L
    Go.session:
      Go:
        ch.put(batch): i =>
          shortcut.quit(i >= Total).?
          Pool(i & Mask)
      ch.get: v =>
        acc += v.length
    .await() __ Unit
    acc

  @Benchmark
  @OperationsPerInvocation(Total)
  def spscNArr(): Long =
    val ch = ChanN[String](capacity, batch)
    val a = src
    var acc = 0L
    Go.session:
      Go:
        ch.putN(a, 0, Total)
      ch.get: v =>
        acc += v.length
    .await() __ Unit
    acc
}


/** Fan-in: several batched producers contend on one shared ChanN — the headline shape:
  * Chan's fan-in was lock-contention bound at 62-82% of ABQ, and chunking divides exactly
  * that cost by the batch.  Compare to `FanInBench.fanIn` / `.abqFanIn`. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ChanNFanInBench {
  import Bench.{Total, Pool, Mask}

  @Param(Array("1024"))
  var capacity: Int = 0

  @Param(Array("2", "4", "8"))
  var producers: Int = 0

  @Param(Array("1", "8", "32"))
  var batch: Int = 0

  @Benchmark
  @OperationsPerInvocation(Total)
  def fanInN(): Long =
    val per = Total / producers
    val ch = ChanN[String](capacity, batch)
    var acc = 0L
    Go.session:
      var p = 0
      while p < producers do
        Go:
          ch.put(batch): i =>
            shortcut.quit(i >= per).?
            Pool(i & Mask)
        p += 1
      ch.get: v =>
        acc += v.length
    .await() __ Unit
    acc
}


/** One consumer selecting over N persistent chunked recv handlers; with `work > 0` the
  * producers are not saturated, so the consumer actually blocks and wakeup costs show.
  * Compare to `SelectBench.selectN` / `.abqSelectN`. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ChanNSelectBench {
  import Bench.{Total, Pool, Mask, collatzWork}

  @Param(Array("1024"))
  var capacity: Int = 0

  @Param(Array("1", "4", "16"))
  var channels: Int = 0

  @Param(Array("0", "256"))
  var work: Int = 0

  @Param(Array("1", "32"))
  var batch: Int = 0

  @Benchmark
  @OperationsPerInvocation(Total)
  def selectN(): Long =
    val per = Total / channels
    val w = work
    val chs = Array.fill(channels)(ChanN[String](capacity, batch))
    var acc = 0L
    Go.session:
      var c = 0
      while c < chs.length do
        val ch = chs(c)
        Go:
          var perturb = 0
          ch.put(batch): i =>
            shortcut.quit(i >= per).?
            val s = Pool((i + perturb) & Mask)
            perturb = collatzWork(s.length, w)          // work result steers the next selection
            s
        ch.get: v =>
          acc += v.length
        c += 1
    .await() __ Unit
    acc
}


/** Per-item delivery latency machinery: a power-of-two histogram (bucket b counts latencies
  * in [2^b, 2^(b+1)) ns), recorded by the consumer, reported per iteration.  Percentiles are
  * bucket midpoints, so they are exact only to within 2x — enough to compare regimes. */
object Lat:
  /** Items per invocation — small, because the producers do real per-item work. */
  final val Total = 20_000

  def record(h: Array[Long], nanos: Long): Unit =
    val n = if nanos < 1 then 1L else nanos
    h(63 - java.lang.Long.numberOfLeadingZeros(n)) += 1

  private def fmt(ns: Long): String =
    if ns < 1_000L then s"${ns}ns"
    else if ns < 1_000_000L then f"${ns / 1e3}%.1fus"
    else if ns < 1_000_000_000L then f"${ns / 1e6}%.2fms"
    else f"${ns / 1e9}%.2fs"

  private def pct(h: Array[Long], total: Long, p: Double): String =
    val target = math.ceil(total * p).toLong
    var seen = 0L
    var b = 0
    while b < h.length && seen < target do
      seen += h(b)
      b += 1
    val k = b - 1
    fmt(if k < 1 then 1L else 3L << (k - 1))            // ~1.5 * 2^k, the bucket midpoint

  def report(tag: String, h: Array[Long]): Unit =
    var total = 0L
    var b = 0
    while b < h.length do
      total += h(b)
      b += 1
    if total > 0 then
      println(s"  [latency] $tag  p50=${pct(h, total, 0.50)}  p99=${pct(h, total, 0.99)}  p999=${pct(h, total, 0.999)}  (n=$total, 2x buckets)")
    java.util.Arrays.fill(h, 0L)


/** Latency through ChanN: items carry their send-side `nanoTime`, the consumer records the
  * delivery delta.  Trickle = steady per-item work, so a batched producer's first item waits
  * for its batchmates; bursty = the same work clumped before each run of 64 items.  The JMH
  * score is still items/s; the latency percentiles print per iteration. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ChanNLatencyBench {
  import Bench.collatzWork

  @Param(Array("1024"))
  var capacity: Int = 0

  @Param(Array("1", "32"))
  var batch: Int = 0

  @Param(Array("trickle", "bursty"))
  var regime: String = ""

  @Param(Array("256"))
  var work: Int = 0

  val hist = new Array[Long](64)

  @TearDown(Level.Iteration)
  def reportIteration(): Unit =
    Lat.report(s"chanN batch=$batch $regime work=$work", hist)

  @Benchmark
  @OperationsPerInvocation(Lat.Total)
  def latencyN(): Long =
    val ch = ChanN[java.lang.Long](capacity, batch)
    val h = hist
    val w = work
    val bursty = regime == "bursty"
    var acc = 0L
    Go.session:
      Go:
        var perturb = 0
        ch.put(batch): i =>
          shortcut.quit(i >= Lat.Total).?
          if bursty then
            if (i & 63) == 0 then perturb = collatzWork(i + perturb, w * 64)
          else perturb = collatzWork(i + perturb, w)
          java.lang.Long.valueOf(System.nanoTime() + (perturb & 1L))
      ch.get: t =>
        val d = System.nanoTime() - t.longValue
        Lat.record(h, d)
        acc += d
    .await() __ Unit
    acc
}


/** Control: the same latency shapes through plain Chan (one item at a time). */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ChanLatencyBench {
  import Bench.collatzWork

  @Param(Array("1024"))
  var capacity: Int = 0

  @Param(Array("trickle", "bursty"))
  var regime: String = ""

  @Param(Array("256"))
  var work: Int = 0

  val hist = new Array[Long](64)

  @TearDown(Level.Iteration)
  def reportIteration(): Unit =
    Lat.report(s"chan $regime work=$work", hist)

  @Benchmark
  @OperationsPerInvocation(Lat.Total)
  def latencyChan(): Long =
    val ch = Chan[java.lang.Long](capacity)
    val h = hist
    val w = work
    val bursty = regime == "bursty"
    var acc = 0L
    Go.session:
      Go:
        var i = 0
        var perturb = 0
        ch.put:
          if bursty then
            if (i & 63) == 0 then perturb = collatzWork(i + perturb, w * 64)
          else perturb = collatzWork(i + perturb, w)
          i += 1
          java.lang.Long.valueOf(System.nanoTime() + (perturb & 1L))
        Stop.on(i >= Lat.Total)
      ch.get: t =>
        val d = System.nanoTime() - t.longValue
        Lat.record(h, d)
        acc += d
    .await() __ Unit
    acc
}


/** Full-grab consumption (`getFull`: only whole chunks, remainder on close) against
  * as-available (`get`) at independently chosen producer and consumer chunk sizes — all
  * producers agree on `pbatch`, the consumer picks its own `cbatch`, and the channel is the
  * m-to-n rebatcher between them.  Identical params on both methods, so any difference is
  * purely the consumption mode. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class ChanNFullGrabBench {
  import Bench.{Total, Pool, Mask}

  @Param(Array("1024"))
  var capacity: Int = 0

  @Param(Array("1", "8"))
  var producers: Int = 0

  @Param(Array("8", "32"))
  var pbatch: Int = 0

  @Param(Array("8", "32"))
  var cbatch: Int = 0

  private def run(full: Boolean): Long =
    val per = Total / producers
    val ch = ChanN[String](capacity)
    var acc = 0L
    Go.session:
      var p = 0
      while p < producers do
        Go:
          ch.put(pbatch): i =>
            shortcut.quit(i >= per).?
            Pool(i & Mask)
        p += 1
      if full then
        ch.getFull(cbatch): v =>
          acc += v.length
      else
        ch.get(cbatch): v =>
          acc += v.length
    .await() __ Unit
    acc

  @Benchmark
  @OperationsPerInvocation(Total)
  def fullGrab(): Long = run(true)

  @Benchmark
  @OperationsPerInvocation(Total)
  def asAvailable(): Long = run(false)
}
