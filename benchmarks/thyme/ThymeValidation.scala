// Ground-truth validation for kse.thyme.Thyme: measure identical workloads with JMH and with
// Thyme, then compare.  Thyme is a quick in-context estimator; JMH is the reference.  Thyme's
// per-call time should land within roughly 1.5–2x of JMH's AverageTime score.  If Thyme reads
// *wildly* high (say >3x), it is almost certainly measuring un-warmed (interpreter/C1) code, which
// means our steady-state detection is not doing its job.
//
// These run against an *assembled* build of the library — exactly what an external user would get.
// Build the jar first (from the repo root):   mill all.assembly
//
// Ground truth (JMH AverageTime, ns/op):
//   scala-cli --power run benchmarks/thyme --jmh -- -f 1 -wi 5 -i 5 -tu ns
//   scala-cli --power run benchmarks/thyme --jmh -- -f 1 -wi 5 -i 5 GroundTruth.w1000   # one
//
// Thyme's own estimates (no --jmh; runs the main below):
//   scala-cli --power run benchmarks/thyme
//
// Pin both to the same cores or variance swamps the comparison, e.g. `taskset -c 0-15 scala-cli ...`.

//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar
//> using dep org.openjdk.jmh:jmh-core:1.37

package kse.bench.thyme

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import kse.thyme.*


/** Workloads shared verbatim by JMH and by Thyme, so both measure identical code.  Each is O(n)
  * with a loop-carried dependency (so it cannot be unrolled away) and returns a value depending on
  * every iteration (so nothing can be eliminated).
  */
object Work:
  def busywork(n: Int): Long =
    var s = 1L
    var i = 0
    while i < n do
      s = (s + i.toLong * 2862933555777941757L) ^ (s >>> 7)
      i += 1
    s


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(1)
class GroundTruth:
  @Benchmark def w10():    Long = Work.busywork(10)
  @Benchmark def w100():   Long = Work.busywork(100)
  @Benchmark def w1000():  Long = Work.busywork(1000)
  @Benchmark def w10000(): Long = Work.busywork(10000)


/** Prints Thyme's estimate of the very same workloads, for side-by-side comparison with JMH. */
object ThymeReport:
  def main(args: Array[String]): Unit =
    val th = new Thyme()
    println(s"Thyme timer-error floor (tick): ${Thyme.humanTime(th.tick)}\n")
    println(f"${"workload"}%-9s ${"Thyme/call"}%12s   ${"95% CI"}%-26s converged")
    def line(label: String, b: Thyme.Benched): Unit =
      val ci = s"[${Thyme.humanTime(b.lo)}, ${Thyme.humanTime(b.hi)}]"
      println(f"$label%-9s ${Thyme.humanTime(b.time)}%12s   $ci%-26s ${b.converged}")
    line("w10",    th.bench(Work.busywork(10)))
    line("w100",   th.bench(Work.busywork(100)))
    line("w1000",  th.bench(Work.busywork(1000)))
    line("w10000", th.bench(Work.busywork(10000)))

    // Head-to-head: benchOff should agree with the ratio of the JMH absolute scores.
    println("\nHead-to-head (benchOff) — compare 'faster by Nx' to the ratio of JMH scores above:")
    println("w1000 vs w2000  (true ~2x):")
    println("  " + th.benchOff(Work.busywork(1000))(Work.busywork(2000)).toString.replace("\n", "\n  "))
    println("w1000 vs w10000 (true ~10.5x):")
    println("  " + th.benchOff(Work.busywork(1000))(Work.busywork(10000)).toString.replace("\n", "\n  "))
    println("w1000 vs w1000  (identical — must be indistinguishable):")
    println("  " + th.benchOff(Work.busywork(1000))(Work.busywork(1000)).toString.replace("\n", "\n  "))
