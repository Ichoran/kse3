// JMH benchmarks for kse.loom.SplitDeque against common java.util(.concurrent) collections.
//
// These run against an *assembled* build of the library — exactly what an external user
// would get from a released artifact — so the only thing kse-specific here is the import.
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              scala-cli --power run benchmarks/splitdeque --jmh
// One class, quick:    scala-cli --power run benchmarks/splitdeque --jmh -- -f 1 -wi 2 -i 3 SoloBench
// One combo:           scala-cli --power run benchmarks/splitdeque --jmh -- -p pairs=4 -p in=32 SdPipeBench
//
// Methodology:
//   - Items are references drawn from a fixed pre-allocated String pool, walked in a
//     scrambled order (odd-multiplier stride) so nothing is allocated per item and
//     neighboring sends aren't neighboring references.  This measures coordination,
//     not the garbage collector or the prefetcher.
//   - No Blackhole: the consumer does a real (trivial) task — `sum` adds up string
//     lengths, `copy` stores each item into a pre-allocated output array — and the
//     result is returned to JMH.
//   - Workers are *platform* threads: this is a data-structure benchmark, and the
//     virtual-thread scheduler would blur attribution.  Thread spawn is inside the
//     timed region but amortized over Total items (~1% at the highest thread count).
//   - Pin concurrent runs to one core complex or variance swamps the results, e.g.
//     `taskset -c 0-15 scala-cli ...` (P-cores on a 14900HX).
//
// Every score is items moved end-to-end per second (`@OperationsPerInvocation`).

//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar

package kse.bench.splitdeque

import java.util.ArrayList
import java.util.ArrayDeque
import java.util.concurrent.TimeUnit
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.LinkedBlockingDeque
import java.util.concurrent.locks.LockSupport

import org.openjdk.jmh.annotations.*

import kse.flow.*
import kse.loom.*


object Bench:
  /** Items moved end-to-end per invocation.  Compile-time constant (feeds
    * `@OperationsPerInvocation`); evenly divisible by every pair count, block size, and
    * cycle length used.  Large enough that per-invocation setup (structure allocation,
    * thread spawn) is amortized to the ~1% level. */
  final val Total = 600_000

  /** A fixed pool of distinct Strings, so producing an "item" is a reference read, not an
    * allocation.  Size is a power of two for cheap masking. */
  val Pool: Array[String] = Array.tabulate(256)(i => s"item-$i")

  /** Pool index of the i-th item: an odd multiplier walks the whole pool in a scrambled
    * order, so consecutive items are not adjacent references. */
  inline def pick(i: Int): Int = (i * 0x9E37) & 0xFF

  /** Consumer-side wait when the structure is momentarily empty (SplitDeque and
    * ConcurrentLinkedDeque don't block): spin briefly, then 1 µs parks. */
  def rest(idle: Int): Int =
    if idle < 256 then Thread.onSpinWait()
    else LockSupport.parkNanos(1000L)
    idle + 1


/** Pipeline: `pairs` producers feed `pairs` consumers through one shared SplitDeque.
  * Producers insert singly (`in=1`, `pushRight`) or in blocks (`in=32`: fill a local
  * Batch, then one `spliceRight`).  Consumers extract singly (`out=1`, `popLeft`) or
  * in blocks: `sd` goes through `splitLeft` + drain of the private batch, `sdInto`
  * uses `popLeftInto` straight into an array (for the copy task, directly into the
  * final output — no intermediate at all).  Block extraction is the structure's
  * thesis: under contention, one lock acquisition per block instead of per item.
  *
  * Controls of the same shape are in [[BlockingPipeBench]] (ArrayBlockingQueue,
  * LinkedBlockingDeque — both support block extraction via `drainTo`, neither has a
  * block insert) and [[CldPipeBench]] (ConcurrentLinkedDeque — no block ops at all).
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class SdPipeBench {
  import Bench.{Total, Pool, pick, rest}

  /** Producer/consumer pairs: 1 = no like-side contention, 4 = moderate, 8 = heavy. */
  @Param(Array("1", "4", "8"))
  var pairs: Int = 0

  @Param(Array("1", "32"))
  var in: Int = 0

  @Param(Array("1", "8", "32"))
  var out: Int = 0

  @Param(Array("sum", "copy"))
  var task: String = ""

  /** Pre-allocated target for the `copy` task; each consumer owns one segment. */
  var dst: Array[String] = null

  @Setup(Level.Trial)
  def setup(): Unit =
    dst = new Array[String](Total)

  private def startProducers(deque: SplitDeque[String], per: Int, workers: Array[Thread]): Unit =
    val nIn = in
    var p = 0
    while p < pairs do
      val base = p * per
      workers(p) = Thread.ofPlatform().start { () =>
        var i = 0
        if nIn == 1 then
          while i < per do
            deque.pushRight(Pool(pick(base + i)))
            i += 1
        else
          val b = SplitDeque.Batch.empty[String]()
          while i < per do
            var j = 0
            val lim = math.min(nIn, per - i)
            while j < lim do
              b.pushRight(Pool(pick(base + i + j)))
              j += 1
            deque.spliceRight(b)        // b is left empty and is reused
            i += lim
      }
      p += 1

  @Benchmark
  @OperationsPerInvocation(Total)
  def sd(): Long =
    val per = Total / pairs
    val nOut = out
    val copy = task == "copy"
    val target = dst
    val deque = SplitDeque.empty[String]
    val workers = new Array[Thread](2 * pairs)
    startProducers(deque, per, workers)
    val accs = new Array[Long](pairs)
    var c = 0
    while c < pairs do
      val ci = c
      workers(pairs + c) = Thread.ofPlatform().start { () =>
        val base = ci * per
        var got = 0
        var acc = 0L
        var idle = 0
        if nOut == 1 then
          while got < per do
            val s = deque.popLeft().getOrElse(_ => null)
            if s eq null then idle = rest(idle)
            else
              idle = 0
              if copy then target(base + got) = s else acc += s.length
              got += 1
        else
          while got < per do
            if deque.isEmpty then idle = rest(idle)   // cheap volatile read; splitLeft on empty would allocate
            else
              val chunk = deque.splitLeft(math.min(nOut, per - got))
              if chunk.isEmpty then idle = rest(idle)
              else
                idle = 0
                var s = chunk.popLeft().getOrElse(_ => null)
                while s != null do
                  if copy then target(base + got) = s else acc += s.length
                  got += 1
                  s = chunk.popLeft().getOrElse(_ => null)
        accs(ci) = acc
      }
      c += 1
    var t = 0
    while t < workers.length do
      workers(t).join()
      t += 1
    if deque.length != 0 then throw new IllegalStateException(s"deque not drained: ${deque.length}")
    if copy then (target(0).length + target(Total - 1).length).toLong
    else
      var sum = 0L
      var k = 0
      while k < pairs do
        sum += accs(k)
        k += 1
      sum

  @Benchmark
  @OperationsPerInvocation(Total)
  def sdInto(): Long =
    val per = Total / pairs
    val nOut = out
    val copy = task == "copy"
    val target = dst
    val deque = SplitDeque.empty[String]
    val workers = new Array[Thread](2 * pairs)
    startProducers(deque, per, workers)
    val accs = new Array[Long](pairs)
    var c = 0
    while c < pairs do
      val ci = c
      workers(pairs + c) = Thread.ofPlatform().start { () =>
        val base = ci * per
        var got = 0
        var acc = 0L
        var idle = 0
        val buf = if copy then null else new Array[String](nOut)
        while got < per do
          val want = math.min(nOut, per - got)
          val k =
            if copy then deque.popLeftInto(target, base + got, want)
            else deque.popLeftInto(buf, 0, want)
          if k == 0 then idle = rest(idle)
          else
            idle = 0
            if !copy then
              var j = 0
              while j < k do
                acc += buf(j).length
                j += 1
            got += k
        accs(ci) = acc
      }
      c += 1
    var t = 0
    while t < workers.length do
      workers(t).join()
      t += 1
    if deque.length != 0 then throw new IllegalStateException(s"deque not drained: ${deque.length}")
    if copy then (target(0).length + target(Total - 1).length).toLong
    else
      var sum = 0L
      var k = 0
      while k < pairs do
        sum += accs(k)
        k += 1
      sum
}


/** Controls: the same pipeline over `ArrayBlockingQueue` (the j.u.c. throughput gold
  * standard) and `LinkedBlockingDeque` (the blocking *deque*, the fairest API rival).
  * Both support block extraction (`out=32` via take + `drainTo`); neither has a block
  * insert, so producers always `put` singly — compare against `SdPipeBench` rows with
  * `in=1`.  Capacity 1024 is conventional usage; producers feel backpressure that the
  * unbounded SplitDeque never imposes (that asymmetry is inherent to the designs).
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class BlockingPipeBench {
  import Bench.{Total, Pool, pick}

  @Param(Array("1", "4", "8"))
  var pairs: Int = 0

  @Param(Array("1", "8", "32"))
  var out: Int = 0

  @Param(Array("sum", "copy"))
  var task: String = ""

  @Param(Array("1024"))
  var capacity: Int = 0

  var dst: Array[String] = null

  @Setup(Level.Trial)
  def setup(): Unit =
    dst = new Array[String](Total)

  // Producers put singly; consumers take() then drainTo up to out-1 more.  Totals are
  // exactly balanced, so the blocking take() always eventually succeeds.
  private def run(q: java.util.concurrent.BlockingQueue[String]): Long =
    val per = Total / pairs
    val nOut = out
    val copy = task == "copy"
    val target = dst
    val workers = new Array[Thread](2 * pairs)
    var p = 0
    while p < pairs do
      val base = p * per
      workers(p) = Thread.ofPlatform().start { () =>
        var i = 0
        while i < per do
          q.put(Pool(pick(base + i)))
          i += 1
      }
      p += 1
    val accs = new Array[Long](pairs)
    var c = 0
    while c < pairs do
      val ci = c
      workers(pairs + c) = Thread.ofPlatform().start { () =>
        val base = ci * per
        var got = 0
        var acc = 0L
        val buf = if nOut > 1 then new ArrayList[String](nOut) else null
        while got < per do
          val s = q.take()
          if copy then target(base + got) = s else acc += s.length
          got += 1
          if nOut > 1 && got < per then
            buf.clear()
            val n = q.drainTo(buf, math.min(nOut - 1, per - got))
            var j = 0
            while j < n do
              val v = buf.get(j)
              if copy then target(base + got) = v else acc += v.length
              got += 1
              j += 1
        accs(ci) = acc
      }
      c += 1
    var t = 0
    while t < workers.length do
      workers(t).join()
      t += 1
    if !q.isEmpty then throw new IllegalStateException("queue not drained")
    if copy then (target(0).length + target(Total - 1).length).toLong
    else
      var sum = 0L
      var k = 0
      while k < pairs do
        sum += accs(k)
        k += 1
      sum

  @Benchmark
  @OperationsPerInvocation(Total)
  def abq(): Long = run(new ArrayBlockingQueue[String](capacity))

  @Benchmark
  @OperationsPerInvocation(Total)
  def lbd(): Long = run(new LinkedBlockingDeque[String](capacity))
}


/** Control: the same pipeline over `ConcurrentLinkedDeque` — lock-free and unbounded
  * like SplitDeque, but allocates a node per element (inherent to the structure) and
  * has no block operations, so there is only the single-item shape — compare against
  * `SdPipeBench` rows with `in=1, out=1`. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class CldPipeBench {
  import Bench.{Total, Pool, pick, rest}

  @Param(Array("1", "4", "8"))
  var pairs: Int = 0

  @Param(Array("sum", "copy"))
  var task: String = ""

  var dst: Array[String] = null

  @Setup(Level.Trial)
  def setup(): Unit =
    dst = new Array[String](Total)

  @Benchmark
  @OperationsPerInvocation(Total)
  def cld(): Long =
    val per = Total / pairs
    val copy = task == "copy"
    val target = dst
    val q = new ConcurrentLinkedDeque[String]()
    val workers = new Array[Thread](2 * pairs)
    var p = 0
    while p < pairs do
      val base = p * per
      workers(p) = Thread.ofPlatform().start { () =>
        var i = 0
        while i < per do
          q.addLast(Pool(pick(base + i)))
          i += 1
      }
      p += 1
    val accs = new Array[Long](pairs)
    var c = 0
    while c < pairs do
      val ci = c
      workers(pairs + c) = Thread.ofPlatform().start { () =>
        val base = ci * per
        var got = 0
        var acc = 0L
        var idle = 0
        while got < per do
          val s = q.pollFirst()
          if s eq null then idle = rest(idle)
          else
            idle = 0
            if copy then target(base + got) = s else acc += s.length
            got += 1
        accs(ci) = acc
      }
      c += 1
    var t = 0
    while t < workers.length do
      workers(t).join()
      t += 1
    if !q.isEmpty then throw new IllegalStateException("deque not drained")
    if copy then (target(0).length + target(Total - 1).length).toLong
    else
      var sum = 0L
      var k = 0
      while k < pairs do
        sum += accs(k)
        k += 1
      sum
}


/** Non-concurrent single-element throughput: repeated fill-1000/drain-1000 cycles in
  * queue order on one thread.  `batch` is the raw engine (no atomics at all); `sd` is
  * the concurrent shell with zero contention, so `sd` vs `batch` is the price of the
  * uncontended lock; `adq` (ArrayDeque) is the j.u. baseline; `abq` shows what a
  * j.u.c. lock costs uncontended. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class SoloBench {
  import Bench.{Total, Pool, pick}

  inline val Cycle = 1000   // items per fill/drain cycle; Total/Cycle cycles per invocation

  @Benchmark
  @OperationsPerInvocation(Total)
  def batch(): Long =
    val b = SplitDeque.Batch.empty[String]()
    var acc = 0L
    var done = 0
    while done < Total do
      var i = 0
      while i < Cycle do
        b.pushRight(Pool(pick(done + i)))
        i += 1
      i = 0
      while i < Cycle do
        acc += b.popLeft().getOrElse(_ => null).length
        i += 1
      done += Cycle
    acc

  @Benchmark
  @OperationsPerInvocation(Total)
  def sd(): Long =
    val d = SplitDeque.empty[String]
    var acc = 0L
    var done = 0
    while done < Total do
      var i = 0
      while i < Cycle do
        d.pushRight(Pool(pick(done + i)))
        i += 1
      i = 0
      while i < Cycle do
        acc += d.popLeft().getOrElse(_ => null).length
        i += 1
      done += Cycle
    acc

  @Benchmark
  @OperationsPerInvocation(Total)
  def adq(): Long =
    val d = new ArrayDeque[String](Cycle + 8)
    var acc = 0L
    var done = 0
    while done < Total do
      var i = 0
      while i < Cycle do
        d.addLast(Pool(pick(done + i)))
        i += 1
      i = 0
      while i < Cycle do
        acc += d.pollFirst().length
        i += 1
      done += Cycle
    acc

  @Benchmark
  @OperationsPerInvocation(Total)
  def abq(): Long =
    val q = new ArrayBlockingQueue[String](Cycle + 8)
    var acc = 0L
    var done = 0
    while done < Total do
      var i = 0
      while i < Cycle do
        if !q.offer(Pool(pick(done + i))) then throw new IllegalStateException("full")
        i += 1
      i = 0
      while i < Cycle do
        acc += q.poll().length
        i += 1
      done += Cycle
    acc
}


/** Non-concurrent rebatching: move the full contents of one container into another in
  * chunks, back and forth, single-threaded.  `splice` (Batch→Batch via
  * splitLeft/spliceRight) is the O(blockSize·log n)-per-chunk claim; `spliceSd`
  * (SplitDeque→SplitDeque) adds the uncontended lock; `singles` moves item-by-item on
  * the same engine, and `adq` is the ArrayDeque element-loop baseline — both of those
  * are O(chunk) per chunk and so ignore the `chunk` parameter. */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class RebatchBench {
  import Bench.{Total, Pool, pick}

  inline val Load = 10_000   // items resident in the source container; Total/Load passes per invocation

  @Param(Array("32", "1024"))
  var chunk: Int = 0

  @Benchmark
  @OperationsPerInvocation(Total)
  def splice(): Long =
    var src = SplitDeque.Batch.empty[String]()
    var dst = SplitDeque.Batch.empty[String]()
    var i = 0
    while i < Load do
      src.pushRight(Pool(pick(i)))
      i += 1
    var moved = 0
    while moved < Total do
      while !src.isEmpty do
        dst.spliceRight(src.splitLeft(chunk))
      moved += Load
      val t = src; src = dst; dst = t
    var acc = 0L
    src.foreach(s => acc += s.length)
    acc

  @Benchmark
  @OperationsPerInvocation(Total)
  def spliceSd(): Long =
    var src = SplitDeque.empty[String]
    var dst = SplitDeque.empty[String]
    var i = 0
    while i < Load do
      src.pushRight(Pool(pick(i)))
      i += 1
    var moved = 0
    while moved < Total do
      while src.length > 0 do
        dst.spliceRight(src.splitLeft(chunk))
      moved += Load
      val t = src; src = dst; dst = t
    var acc = 0L
    val rem = src.splitLeft(Load)
    rem.foreach(s => acc += s.length)
    acc

  @Benchmark
  @OperationsPerInvocation(Total)
  def singles(): Long =
    var src = SplitDeque.Batch.empty[String]()
    var dst = SplitDeque.Batch.empty[String]()
    var i = 0
    while i < Load do
      src.pushRight(Pool(pick(i)))
      i += 1
    var moved = 0
    while moved < Total do
      var s = src.popLeft().getOrElse(_ => null)
      while s != null do
        dst.pushRight(s)
        s = src.popLeft().getOrElse(_ => null)
      moved += Load
      val t = src; src = dst; dst = t
    var acc = 0L
    src.foreach(s => acc += s.length)
    acc

  @Benchmark
  @OperationsPerInvocation(Total)
  def adq(): Long =
    var src = new ArrayDeque[String](Load + 8)
    var dst = new ArrayDeque[String](Load + 8)
    var i = 0
    while i < Load do
      src.addLast(Pool(pick(i)))
      i += 1
    var moved = 0
    while moved < Total do
      var s = src.pollFirst()
      while s != null do
        dst.addLast(s)
        s = src.pollFirst()
      moved += Load
      val t = src; src = dst; dst = t
    var acc = 0L
    val it = src.iterator()
    while it.hasNext do acc += it.next().length
    acc
}
