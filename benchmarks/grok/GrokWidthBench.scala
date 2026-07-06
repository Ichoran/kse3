// JMH benchmark: does declared width intent matter?  The same 4-digit decimal values
// (always fit Short; never hit any reader's digit cap) parsed with S, I, and L.
// S orchestrates from the base class (smallLongWork: virtual skipDelims + digitsWork per
// number); I and L run the monomorphic per-source longWork.  Identical Long accumulator
// in all three so the reader is the only variable.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              scala-cli --power run benchmarks/grok --jmh -- -f 1 -wi 6 -i 8 -w 1 -r 1 GrokWidthBench

//> using scala 3.8.3
//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar
//> using dep com.fasterxml.jackson.core:jackson-databind:2.18.2
//> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:2.30.1
//> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:2.30.1

package kse.bench.grok

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import kse.basics.*
import kse.flow.*
import kse.eio.*


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class GrokWidthBench {
  @Param(Array("100"))
  var size: Int = 0

  var bytes: Array[Byte] = Array.empty

  @Setup(Level.Trial)
  def setup(): Unit =
    val r = new java.util.Random(0x5EED)
    val vs = Array.fill(size)(r.nextInt(9000) + 1000)   // always exactly 4 digits, no sign
    bytes = vs.mkString("[", ", ", "]").getBytes(java.nio.charset.StandardCharsets.UTF_8)

  @Benchmark
  def viaS(): Long =
    Grok(bytes, Delim.white, true, false): g =>
      (g < '[') __ Unit
      var acc = 0L
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => acc += g.S
      acc
    .get

  @Benchmark
  def viaI(): Long =
    Grok(bytes, Delim.white, true, false): g =>
      (g < '[') __ Unit
      var acc = 0L
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => acc += g.I
      acc
    .get

  @Benchmark
  def viaL(): Long =
    Grok(bytes, Delim.white, true, false): g =>
      (g < '[') __ Unit
      var acc = 0L
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => acc += g.L
      acc
    .get
}
