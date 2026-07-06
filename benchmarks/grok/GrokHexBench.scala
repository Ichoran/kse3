// JMH benchmark: hex readers (xI/xL) vs the decimal readers (I/L) on the same values.
//
// JSON has no hex, so there is no external parser to race: the decimal readers are the
// baseline.  Differences to watch: hex needs fewer chars for full-range values (8 vs ~10.5
// for Int) and its per-digit op is shift-or (no multiply), but xI/xL orchestrate from the
// base class (smallHexWork: virtual skipDelims + hexWork per number) while I/L run inside
// one monomorphic per-source worker.  The hand-rolled loop is the practical ceiling.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              scala-cli --power run benchmarks/grok --jmh -- -f 1 -wi 6 -i 8 -w 1 -r 1 GrokHexBench
// Pin for stability, e.g.:                    taskset -c 4 scala-cli --power run benchmarks/grok --jmh -- ...

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
class GrokHexBench {
  @Param(Array("100"))
  var size: Int = 0

  /** "full" = random over all of Int/Long (8 / 16 hex digits, ~10 / ~19 decimal); "1" = random 0-15 (per-element overhead) */
  @Param(Array("full", "1"))
  var digits: String = ""

  var hexText: String = ""
  var decText: String = ""
  var hexBytes: Array[Byte] = Array.empty
  var decBytes: Array[Byte] = Array.empty
  var hexLongBytes: Array[Byte] = Array.empty
  var decLongBytes: Array[Byte] = Array.empty

  @Setup(Level.Trial)
  def setup(): Unit =
    val r = new java.util.Random(0x5EED)
    val vs = digits match
      case "1" => Array.fill(size)(r.nextInt(16))
      case _   => Array.fill(size)(r.nextInt())
    val ls = digits match
      case "1" => Array.fill(size)(r.nextInt(16).toLong)
      case _   => Array.fill(size)(r.nextLong())
    hexText = vs.map(java.lang.Integer.toHexString).mkString("[", ", ", "]")
    decText = vs.mkString("[", ", ", "]")
    hexBytes = hexText.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    decBytes = decText.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    hexLongBytes = ls.map(java.lang.Long.toHexString).mkString("[", ", ", "]").getBytes(java.nio.charset.StandardCharsets.UTF_8)
    decLongBytes = ls.mkString("[", ", ", "]").getBytes(java.nio.charset.StandardCharsets.UTF_8)

  @Benchmark
  def hexIntStr(): Array[Int] =
    val xsb = Array.newBuilder[Int]
    Grok(hexText, delim = Delim.white, partial = true): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.xI
      xsb.result()
    .get

  @Benchmark
  def decIntStr(): Array[Int] =
    val xsb = Array.newBuilder[Int]
    Grok(decText, delim = Delim.white, partial = true): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.I
      xsb.result()
    .get

  @Benchmark
  def hexIntBytes(): Array[Int] =
    val xsb = Array.newBuilder[Int]
    Grok(hexBytes, Delim.white, true, false): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.xI
      xsb.result()
    .get

  @Benchmark
  def decIntBytes(): Array[Int] =
    val xsb = Array.newBuilder[Int]
    Grok(decBytes, Delim.white, true, false): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.I
      xsb.result()
    .get

  @Benchmark
  def hexLongBytes_(): Array[Long] =
    val xsb = Array.newBuilder[Long]
    Grok(hexLongBytes, Delim.white, true, false): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.xL
      xsb.result()
    .get

  @Benchmark
  def decLongBytes_(): Array[Long] =
    val xsb = Array.newBuilder[Long]
    Grok(decLongBytes, Delim.white, true, false): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.L
      xsb.result()
    .get

  // Practical ceiling: fused charAt loop, hex digits normalized the same way (select-and-add)
  @Benchmark
  def handRolledHexStr(): Array[Int] =
    val s = hexText
    var out = new Array[Int](16)
    var n = 0
    var i = 1   // past '['
    val N = s.length
    while i < N do
      var c = s.charAt(i): Int
      if c == ']' then i = N
      else if c == ',' || c == ' ' then i += 1
      else
        var x = 0
        var ok = true
        while ok && i < N do
          var y = c | 0x20
          y += (if y <= '9' then -'0' else 0) + (if y >= 'a' then -87 else 0)
          if y < 0 || y > 15 then ok = false
          else
            x = (x << 4) | y
            i += 1
            if i < N then c = s.charAt(i): Int
        if n >= out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
        out(n) = x
        n += 1
    java.util.Arrays.copyOf(out, n)
}
