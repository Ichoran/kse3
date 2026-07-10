// JMH benchmark: numeric matrix (n×n doubles) -- exercises jsaun's PACKED array backing.
//
// When every element of a JSON array is a Double, jsaun's parser stores the row as a `Jarr.D`
// (a bare `Array[Double]`), not a `Jarr.A` of boxed `Jnum` objects.  So a 10×10 matrix parses
// to an outer `Jarr.A` of ten `Jarr.D` rows -- 100 doubles in ten primitive arrays, zero per-
// number heap objects.  Printing walks those arrays directly, and `.dbls` hands the row's
// `Array[Double]` straight back.  @Setup asserts the rows really did pack, so a regression that
// silently fell back to `Jarr.A` would fail loudly rather than just get slower.
//
// The `prec` axis controls how hard the numbers are to parse: "4sig" values round-trip through
// short decimals (Eisel-Lemire / Clinger fast path), "full" values are shortest-round-trip
// doubles (~17 significant digits) that defeat the fast path.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              scala-cli --power run benchmarks/jsaun --jmh -- -f 1 -wi 5 -i 5 -w 1 -r 1 JsaunMatrixBench
// Pin for stability, e.g.:                    taskset -c 4 scala-cli --power run benchmarks/jsaun --jmh -- JsaunMatrixBench

//> using scala 3.8.3
//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar
//> using dep com.fasterxml.jackson.core:jackson-databind:2.18.2
//> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:2.30.1
//> using dep com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:2.30.1
//> using dep com.lihaoyi::upickle:4.1.0

package kse.bench.jsaun

import java.util.concurrent.TimeUnit
import java.nio.charset.StandardCharsets.UTF_8

import org.openjdk.jmh.annotations.*

import com.fasterxml.jackson.databind.{ObjectMapper, JsonNode}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray, writeToArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import kse.flow.{given, *}
import kse.jsaun.{given, *}


// A matrix is a list of double-array rows: outer collection parses to Jarr.A, each row to Jarr.D.
object MatrixCodecs:
  given matrixCodec: JsonValueCodec[List[Array[Double]]] = JsonCodecMaker.make

import MatrixCodecs.given


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class JsaunMatrixBench {
  @Param(Array("10"))
  var n: Int = 0

  /** "4sig" = 4 significant figures (short decimals, fast path); "full" = shortest-round-trip doubles */
  @Param(Array("4sig", "full"))
  var prec: String = ""

  var text: String = ""
  var bytes: Array[Byte] = Array.empty
  var chars: Array[Char] = Array.empty

  var model: List[Array[Double]] = Nil
  var tree: Json = Jnull
  var node: JsonNode = null
  var uval: ujson.Value = ujson.Null

  val objectMapper = new ObjectMapper()

  @Setup(Level.Trial)
  def setup(): Unit =
    val r = new java.util.Random(0x0DDBA11)
    model = List.fill(n){
      Array.fill(n){
        val raw = r.nextDouble() * 200 - 100
        if prec == "4sig" then new java.math.BigDecimal(raw).round(new java.math.MathContext(4)).doubleValue
        else raw
      }
    }
    bytes = writeToArray(model)         // shared input, serialized once by jsoniter
    text  = new String(bytes, UTF_8)
    chars = text.toCharArray
    tree  = Json.parse(bytes).jsonOr(Jnull)
    node  = objectMapper.readTree(bytes)
    uval  = ujson.read(bytes)
    // Guard: the rows must have packed into Jarr.D, or this benchmark measures the wrong thing.
    val row0 = Json.parse(bytes)(0).jsonOr(Jnull)
    if !row0.isInstanceOf[Jarr.D] then
      throw new AssertionError(s"expected packed Jarr.D rows, got ${row0.getClass.getName} (${row0.kind})")

  // === parse to tree (jsaun packs rows into Jarr.D) ===

  @Benchmark
  def jsaunParseBytes(): Json = Json.parse(bytes).jsonOr(Jnull)

  @Benchmark
  def jsaunParseString(): Json = Json.parse(text).jsonOr(Jnull)

  @Benchmark
  def jsaunParseChars(): Json = Json.parse(chars).jsonOr(Jnull)

  @Benchmark
  def jsaunParseExact(): Json = Json.parse(bytes, exact = true).jsonOr(Jnull)

  @Benchmark
  def jacksonTreeBytes(): JsonNode = objectMapper.readTree(bytes)

  @Benchmark
  def ujsonParseBytes(): ujson.Value = ujson.read(bytes)

  // === serialize tree back ===

  @Benchmark
  def jsaunPrintBytes(): Array[Byte] = tree.printBytes

  @Benchmark
  def jacksonWriteBytes(): Array[Byte] = objectMapper.writeValueAsBytes(node)

  @Benchmark
  def ujsonWriteBytes(): Array[Byte] = ujson.writeToByteArray(uval)

  // === typed: bytes -> List[Array[Double]] (jsaun reads each row via the packed .dbls) ===

  @Benchmark
  def jsaunCodecDecode(): List[Array[Double]] = Json.parse(bytes).to[List[Array[Double]]].get   // .get: fail loud, never a silent error path

  @Benchmark
  def jsoniterDecode(): List[Array[Double]] = readFromArray[List[Array[Double]]](bytes)

  @Benchmark
  def upickleDecode(): List[Array[Double]] = upickle.default.read[List[Array[Double]]](bytes)

  // === typed: List[Array[Double]] -> bytes ===

  @Benchmark
  def jsaunCodecEncode(): Array[Byte] = Json(model).printBytes

  @Benchmark
  def jsoniterEncode(): Array[Byte] = writeToArray(model)

  // === packed-backing payoff: pull every row's Array[Double] straight back out and sum it ===

  @Benchmark
  def jsaunSumDbls(): Double =
    var total = 0.0
    val rows = tree.asInstanceOf[Jarr]
    val empty = new Array[Double](0)
    var i = 0
    while i < rows.size do
      val row = rows(i).jsonOr(Jnull).asInstanceOf[Jarr]     // a packed Jarr.D
      val xs = row.dbls.getOrElse(_ => empty)                // the row's Array[Double], no per-element boxing
      var k = 0
      while k < xs.length do { total += xs(k); k += 1 }
      i += 1
    total
}
