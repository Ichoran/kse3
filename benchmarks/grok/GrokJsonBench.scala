// JMH benchmark: parsing a JSON array of ~100 random Ints when you KNOW it's an int array.
//
// Grok is a direct-mode parser, so it gets to cheat: no tokenizer state machine, just
// "expect '[', then ints and commas until ']'".  The bar: substantially beat Jackson,
// be competitive with jsoniter-scala (which also compiles a dedicated Array[Int] codec),
// and land near the hand-rolled charAt loop that is the practical speed ceiling.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              scala-cli --power run benchmarks/grok --jmh -- -f 1 -wi 5 -i 5 -w 1 -r 1
// Pin for stability, e.g.:                    taskset -c 4 scala-cli --power run benchmarks/grok --jmh -- ...
//
// Inputs: each parser gets its natural form — String for Grok/handRolled/Jackson-String,
// UTF-8 bytes for jsoniter and Jackson-bytes.  Scores are parses of the whole array per second.

//> using scala 3.8.3
//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar
//> using dep com.fasterxml.jackson.core:jackson-databind:2.18.2
//> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:2.30.1
//> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:2.30.1

package kse.bench.grok

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import com.fasterxml.jackson.core.{JsonFactory, JsonToken}
import com.fasterxml.jackson.databind.ObjectMapper
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import kse.basics.*
import kse.flow.*
import kse.eio.*


object Codecs:
  given intArrayCodec: JsonValueCodec[Array[Int]] = JsonCodecMaker.make
  given dblArrayCodec: JsonValueCodec[Array[Double]] = JsonCodecMaker.make

import Codecs.given


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class GrokJsonBench {
  @Param(Array("100"))
  var size: Int = 0

  /** "full" = random over all of Int (~10 digits); "1" = random 0-9 (isolates per-element overhead from digit-scanning cost) */
  @Param(Array("full", "1"))
  var digits: String = ""

  /** Double precision: "full" = shortest-roundtrip (~17 sig digits, defeats Clinger fast paths); "10" = rounded to 10 decimal digits */
  @Param(Array("full", "10"))
  var dprec: String = ""

  var text: String = ""
  var bytes: Array[Byte] = Array.empty
  var chars: Array[Char] = Array.empty
  var mem: Mem[Byte] = Mem.of(Array.empty[Byte])
  var dtext: String = ""
  var dbytes: Array[Byte] = Array.empty

  val jsonFactory = new JsonFactory()
  val objectMapper = new ObjectMapper()

  @Setup(Level.Trial)
  def setup(): Unit =
    val r = new java.util.Random(0x5EED)
    val vs = digits match
      case "1"  => Array.fill(size)(r.nextInt(10))
      case "1G" => Array.fill(size)(r.nextInt(2000000001) - 1000000000)   // ±1e9: ~9 digits, never 10
      case _    => Array.fill(size)(r.nextInt())
    text = vs.mkString("[", ", ", "]")
    bytes = text.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    chars = text.toCharArray
    mem = Mem.of(bytes)
    val ds = Array.fill(size){
      val d = r.nextDouble() * 2 - 1
      if dprec == "10" then math.rint(d * 1e10) / 1e10 else d
    }
    dtext = ds.mkString("[", ", ", "]")
    dbytes = dtext.getBytes(java.nio.charset.StandardCharsets.UTF_8)

  @Benchmark
  def grok(): Array[Int] =
    val xsb = Array.newBuilder[Int]
    Grok(text, delim = Delim.white, partial = true): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit   // token match: whitespace-tolerant on both sides
          case _   => xsb += g.I
      xsb.result()
    .get

  @Benchmark
  def jsoniterScala(): Array[Int] =
    readFromArray[Array[Int]](bytes)

  @Benchmark
  def jacksonStreamBytes(): Array[Int] =
    val p = jsonFactory.createParser(bytes)
    var out = new Array[Int](16)
    var n = 0
    p.nextToken()  // START_ARRAY
    var t = p.nextToken()
    while t != JsonToken.END_ARRAY do
      if n >= out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
      out(n) = p.getIntValue
      n += 1
      t = p.nextToken()
    p.close()
    java.util.Arrays.copyOf(out, n)

  @Benchmark
  def jacksonStreamString(): Array[Int] =
    val p = jsonFactory.createParser(text)
    var out = new Array[Int](16)
    var n = 0
    p.nextToken()  // START_ARRAY
    var t = p.nextToken()
    while t != JsonToken.END_ARRAY do
      if n >= out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
      out(n) = p.getIntValue
      n += 1
      t = p.nextToken()
    p.close()
    java.util.Arrays.copyOf(out, n)

  @Benchmark
  def jacksonDatabind(): Array[Int] =
    objectMapper.readValue(bytes, classOf[Array[Int]])

  // Same parse over raw UTF-8 bytes: byte loads instead of UTF-16 charAt (jsoniter's home turf)
  @Benchmark
  def grokBytes(): Array[Int] =
    val xsb = Array.newBuilder[Int]
    Grok(bytes, Delim.white, true, false): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.I
      xsb.result()
    .get

  // Same parse over Mem[Byte] (MemorySegment-backed): checks the FFM read path JITs to plain loads
  @Benchmark
  def grokMem(): Array[Int] =
    val xsb = Array.newBuilder[Int]
    Grok(mem, Delim.white, true, false): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.I
      xsb.result()
    .get

  // Same parse over Array[Char]: 2-byte loads like String but no coder branch — discriminates
  // byte-narrowness from String-access overhead
  @Benchmark
  def grokChars(): Array[Int] =
    val xsb = Array.newBuilder[Int]
    Grok(chars, Delim.white, true, false): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.I
      xsb.result()
    .get

  // Int result via the (unfolded) Long worker: g.L.toInt with an Int builder, so the only
  // difference from `grok` is which digit-parsing worker runs
  @Benchmark
  def grokIL(): Array[Int] =
    val xsb = Array.newBuilder[Int]
    Grok(text, delim = Delim.white, partial = true): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.L.toInt
      xsb.result()
    .get

  // Same parse as `grok` but via g.L: measures the Long worker independently of I's shim
  @Benchmark
  def grokL(): Array[Long] =
    val xsb = Array.newBuilder[Long]
    Grok(text, delim = Delim.white, partial = true): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.L
      xsb.result()
    .get

  @Benchmark
  def grokD(): Array[Double] =
    val xsb = Array.newBuilder[Double]
    Grok(dtext, delim = Delim.white, partial = true): g =>
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.D
      xsb.result()
    .get

  @Benchmark
  def jsoniterScalaD(): Array[Double] =
    readFromArray[Array[Double]](dbytes)

  @Benchmark
  def jacksonStreamStringD(): Array[Double] =
    val p = jsonFactory.createParser(dtext)
    var out = new Array[Double](16)
    var n = 0
    p.nextToken()  // START_ARRAY
    var t = p.nextToken()
    while t != JsonToken.END_ARRAY do
      if n >= out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
      out(n) = p.getDoubleValue
      n += 1
      t = p.nextToken()
    p.close()
    java.util.Arrays.copyOf(out, n)

  // Pragmatic hand-rolled double parsing: find the span by hand, let the JDK do the rounding.
  @Benchmark
  def handRolledD(): Array[Double] =
    val s = dtext
    var out = new Array[Double](16)
    var n = 0
    var i = 1
    var more = true
    while more do
      var c = s.charAt(i)
      while c == ' ' || c == ',' do
        i += 1
        c = s.charAt(i)
      if c == ']' then more = false
      else
        val a = i
        while c != ',' && c != ']' && c != ' ' do
          i += 1
          c = s.charAt(i)
        if n >= out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
        out(n) = java.lang.Double.parseDouble(s.substring(a, i))
        n += 1
    java.util.Arrays.copyOf(out, n)

  // Grok's op decomposition with none of Grok's machinery: cursor in a field, separate methods
  // that re-read chars exactly the way Grok's sp/peek/</parse sequence does, but no error
  // protocol, no builder, no boundary.  This measures the floor cost of the decomposition itself.
  final class Ops(s: String) {
    var i = 0
    val n = s.length
    def skipSpaces(): Unit =
      while i < n && { val c = s.charAt(i); c == ' ' || c == '\t' || c == '\n' || c == '\r' } do i += 1
    def peekChar(): Char =
      if i < n then s.charAt(i) else (0: Char)
    def matchLit(c: Char): Boolean =
      skipSpaces()
      if i < n && s.charAt(i) == c then
        i += 1
        true
      else false
    def parseLong(): Long =
      skipSpaces()
      var j = i
      var neg = false
      if j < n then
        val c0 = s.charAt(j)
        if c0 == '-' then
          neg = true
          j += 1
        else if c0 == '+' then j += 1
      var x = 0L
      var c = (0: Char)
      while j < n && { c = s.charAt(j); c >= '0' && c <= '9' } do
        x = x * 10 - (c - '0')
        j += 1
      i = j
      if neg then x else -x
  }

  @Benchmark
  def handRolledOps(): Array[Int] =
    val g = new Ops(text)
    var out = new Array[Int](16)
    var n = 0
    g.matchLit('[')
    var more = true
    while more do
      g.skipSpaces()
      g.peekChar() match
        case ']' => more = false
        case ',' =>
          g.matchLit(',')
          ()
        case _   =>
          val x = g.parseLong().toInt
          if n >= out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
          out(n) = x
          n += 1
    java.util.Arrays.copyOf(out, n)

  // The practical speed ceiling: a raw charAt loop that trusts the input completely
  // (no overflow checks, no error reporting; wrapping arithmetic still lands Int.MinValue).
  @Benchmark
  def handRolled(): Array[Int] =
    val s = text
    var out = new Array[Int](16)
    var n = 0
    var i = 1
    var more = true
    while more do
      var c = s.charAt(i)
      while c == ' ' || c == ',' do
        i += 1
        c = s.charAt(i)
      if c == ']' then more = false
      else
        var neg = false
        if c == '-' then
          neg = true
          i += 1
          c = s.charAt(i)
        var x = 0
        while c >= '0' && c <= '9' do
          x = x * 10 + (c - '0')
          i += 1
          c = s.charAt(i)
        if n >= out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
        out(n) = if neg then -x else x
        n += 1
    java.util.Arrays.copyOf(out, n)
}
