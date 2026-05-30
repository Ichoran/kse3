// JMH benchmarks for the kse.loom Go/Chan concurrency model.
//
// These run against an *assembled* build of the library — exactly what an external user
// would get from a released artifact — so the only thing kse-specific here is the import.
// To run against a real release, swap the `using jar` line for:
//     //> using dep com.github.ichoran::kse3-all:0.5.0
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              scala-cli --power run benchmarks/loom --jmh
// One group, quick:                           scala-cli --power run benchmarks/loom --jmh -- -f 1 -wi 2 -i 3 SpscBench
//
// To avoid measuring the garbage collector, items are references drawn from a fixed,
// pre-allocated pool of Strings — nothing is allocated per item.  Each `*` benchmark has a
// plain-java control (`abq*`) of the same shape so the channel/coordination cost is isolated.
// Every score is in items/second (`@OperationsPerInvocation`).

//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar

package kse.bench.loom

import java.util.concurrent.TimeUnit
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.locks.LockSupport

import org.openjdk.jmh.annotations.*

import kse.basics.*
import kse.flow.*
import kse.loom.*


object Bench:
  /** Items moved end-to-end per invocation.  Compile-time constant (feeds
    * `@OperationsPerInvocation`); evenly divisible by every producer/channel count used.
    * Kept small enough that each timed iteration collects many invocations (tight CI),
    * large enough that per-invocation scope setup is negligible. */
  final val Total = 200_000

  /** A fixed pool of distinct Strings, so producing an "item" is a reference read, not an
    * allocation.  Size is a power of two for cheap masking. */
  val Pool: Array[String] = Array.tabulate(256)(i => s"item-$i")
  inline val Mask = 0xFF

  /** Throwaway CPU work for a producer: `reps` rounds of Collatz total-stopping-time seeded
    * from the string length.  Returns an accumulator so the result can be folded back into the
    * data path (preventing dead-code elimination) and to keep producers from saturating their
    * channels — which forces the consumer to actually block, exercising the select wait path. */
  def collatzWork(seed: Int, reps: Int): Int =
    var total = 0
    var r = 0
    while r < reps do
      var x = (seed | 1).toLong          // odd, positive
      while x != 1L do
        x = if (x & 1L) == 0L then x >> 1 else 3L * x + 1L
        total += 1
      r += 1
    total


/** Single producer -> single consumer, against our channel and a plain `ArrayBlockingQueue`
  * of identical shape (the control). */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class SpscBench {
  import Bench.{Total, Pool, Mask}

  @Param(Array("1024"))
  var capacity: Int = 0

  /** Control: a plain blocking queue, no select/lifecycle. */
  @Benchmark
  @OperationsPerInvocation(Total)
  def abqSpsc(): Long =
    val q = new ArrayBlockingQueue[String](capacity)
    val producer = Thread.ofVirtual().start { () =>
      var i = 0
      while i < Total do { q.put(Pool(i & Mask)); i += 1 }
    }
    var acc = 0L
    var got = 0
    while got < Total do { acc += q.take().length; got += 1 }
    producer.join()
    acc

  @Benchmark
  @OperationsPerInvocation(Total)
  def spsc(): Long =
    val ch = Chan[String](capacity)
    var acc = 0L
    Go: g ?=>
      g.go:
        var i = 0
        ch.onSendWhile(i < Total){ val s = Pool(i & Mask); i += 1; s }
      ch.onRecv{ v => acc += v.length }
    .ask() __ Unit
    acc
}


/** One consumer selecting over N persistent recv handlers — the design's thesis: handlers are
  * registered once, not rebuilt each iteration.  With `work > 0` the producers do Collatz work
  * per item, so their channels are *not* saturated and the consumer must actually block on the
  * select — the regime where amortizing the per-block registration cost should matter, and where
  * `channels` is pushed well past a handful.
  *
  * Control `abqSelectN`: the only way to "select" across N `ArrayBlockingQueue`s without a
  * blocking multi-queue primitive — round-robin `poll()` all N, backing off when a sweep is dry.
  * It either spins (CPU) or adds backoff latency; our select blocks and wakes immediately. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class SelectBench {
  import Bench.{Total, Pool, Mask, collatzWork}

  @Param(Array("1024"))
  var capacity: Int = 0

  @Param(Array("1", "4", "16", "64"))
  var channels: Int = 0

  @Param(Array("0", "256"))
  var work: Int = 0

  @Benchmark
  @OperationsPerInvocation(Total)
  def selectN(): Long =
    val per = Total / channels
    val w = work
    val chs = Array.fill(channels)(Chan[String](capacity))
    var acc = 0L
    Go: g ?=>
      var c = 0
      while c < chs.length do
        val ch = chs(c)
        g.go:
          var i = 0
          var perturb = 0
          ch.onSendWhile(i < per){
            val s = Pool((i + perturb) & Mask)
            perturb = collatzWork(s.length, w)          // work result steers the next selection
            i += 1
            s
          }
        ch.onRecv{ v => acc += v.length }
        c += 1
    .ask() __ Unit
    acc

  @Benchmark
  @OperationsPerInvocation(Total)
  def abqSelectN(): Long =
    val per = Total / channels
    val w = work
    val qs = Array.fill(channels)(new ArrayBlockingQueue[String](capacity))
    var p = 0
    while p < channels do
      val q = qs(p)
      Thread.ofVirtual().start { () =>
        var i = 0
        var perturb = 0
        while i < per do
          val s = Pool((i + perturb) & Mask)
          perturb = collatzWork(s.length, w)
          i += 1
          q.put(s)
      }
      p += 1
    var acc = 0L
    var got = 0
    while got < Total do
      var swept = false
      var c = 0
      while c < channels do
        val v = qs(c).poll()
        if v ne null then { acc += v.length; got += 1; swept = true }
        c += 1
      if !swept then LockSupport.parkNanos(1000L)
    acc
}


/** Fan-in: several producers contend on one shared channel feeding a single consumer — the
  * case most sensitive to the single per-channel lock.  Control `abqFanIn` is the same shape
  * over an `ArrayBlockingQueue`. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class FanInBench {
  import Bench.{Total, Pool, Mask}

  @Param(Array("1024"))
  var capacity: Int = 0

  @Param(Array("2", "4", "8"))
  var producers: Int = 0

  @Benchmark
  @OperationsPerInvocation(Total)
  def fanIn(): Long =
    val per = Total / producers
    val ch = Chan[String](capacity)
    var acc = 0L
    Go: g ?=>
      var p = 0
      while p < producers do
        g.go:
          var i = 0
          ch.onSendWhile(i < per){ val s = Pool(i & Mask); i += 1; s }
        p += 1
      ch.onRecv{ v => acc += v.length }
    .ask() __ Unit
    acc

  /** Control: same shape over a plain blocking queue. */
  @Benchmark
  @OperationsPerInvocation(Total)
  def abqFanIn(): Long =
    val per = Total / producers
    val q = new ArrayBlockingQueue[String](capacity)
    var p = 0
    while p < producers do
      Thread.ofVirtual().start { () =>
        var i = 0
        while i < per do { q.put(Pool(i & Mask)); i += 1 }
      }
      p += 1
    var acc = 0L
    var got = 0
    while got < Total do { acc += q.take().length; got += 1 }
    acc
}
