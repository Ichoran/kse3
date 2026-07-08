// JMH benchmark: per-parse fixed cost, isolated by parsing almost nothing.
//
// Each benchmark parses one small value from a tiny input, so the score is dominated by
// setup: source construction, boundary Label, field initialization, first-load.  The JDK
// parsers are the no-framework floor; the difference is what Grok's harness costs, and
// the gc profiler (-prof gc) attributes the allocation share.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              taskset -c 4 scala-cli --power run benchmarks/grok --jmh -- 'GrokInitBench' -f 1 -wi 5 -i 5 -w 1 -r 1 -prof gc

//> using scala 3.8.3
//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar

package kse.bench.grok

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import kse.basics.*
import kse.flow.*
import kse.eio.*


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class GrokInitBench {
  val intText = "5742"
  val intBytes = intText.getBytes(java.nio.charset.StandardCharsets.UTF_8)
  val dblText = "3.14159"

  @Benchmark
  def grokIntStr(): Int = Grok(intText)(g => g.I).getOrElse(_ => -1)

  @Benchmark
  def grokIntBytes(): Int = Grok(intBytes)(g => g.I).getOrElse(_ => -1)

  @Benchmark
  def grokDblStr(): Double = Grok(dblText)(g => g.D).getOrElse(_ => -1)

  @Benchmark
  def grokIntBuffered(): Int = Grok.buffered(intBytes)(g => g.I).getOrElse(_ => -1)

  @Benchmark
  def jdkInt(): Int = Integer.parseInt(intText)

  @Benchmark
  def jdkDbl(): Double = java.lang.Double.parseDouble(dblText)
}
