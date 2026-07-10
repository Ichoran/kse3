// JMH benchmark: jsaun parse / serialize / codec against three references spanning the
// implementation spectrum -- Jackson (Java, reflective), jsoniter-scala (macro-specialized),
// and uPickle/uJson (pure Scala).
//
// Two comparison planes, both over the SAME shared input bytes (a ~100-record array of
// mixed-field objects, the shape of a typical API payload):
//
//   tree plane  -- parse to a dynamic JSON tree and serialize it back.  jsaun's Json tree
//                  vs Jackson's JsonNode vs uJson's ujson.Value.  This is the apples-to-
//                  apples comparison: no side knows the schema.
//   typed plane -- parse to / serialize from a concrete case-class model.  jsaun's derived
//                  codec vs jsoniter-scala's compiled codec (the "you told me the schema"
//                  ceiling) vs uPickle's derived codec.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              scala-cli --power run benchmarks/jsaun --jmh -- -f 1 -wi 5 -i 5 -w 1 -r 1
// Pin for stability, e.g.:                    taskset -c 4 scala-cli --power run benchmarks/jsaun --jmh -- JsaunBench

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
import com.github.plokhotnyuk.jsoniter_scala.macros.{JsonCodecMaker, CodecMakerConfig}

import upickle.default.{ReadWriter, macroRW}

import kse.flow.{given, *}
import kse.jsaun.{given, *}


// The shared schema.  `derives` gives jsaun its codec; jsoniter and uPickle make theirs
// below; Jackson works structurally (tree only).
case class GeoRecord(id: Int, name: String, active: Boolean, score: Double, tags: List[String], path: List[Double])
    derives Jsonize, FromJson

object BenchCodecs:
  // transientEmpty(false): keep empty collections (e.g. an empty `tags`) in the output, so the
  // shared input is faithful and every decoder -- jsaun/uPickle included, which require all
  // fields -- sees the same complete objects.  (jsoniter's default would omit them.)
  given recordsCodec: JsonValueCodec[List[GeoRecord]] = JsonCodecMaker.make(CodecMakerConfig.withTransientEmpty(false))
  given recordRW: ReadWriter[GeoRecord] = macroRW

import BenchCodecs.given


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class JsaunBench {
  @Param(Array("100"))
  var size: Int = 0

  var text: String = ""
  var bytes: Array[Byte] = Array.empty
  var chars: Array[Char] = Array.empty

  var model: List[GeoRecord] = Nil          // for the typed-serialize side
  var tree: Json = Jnull                     // pre-parsed jsaun tree, for the serialize side
  var node: JsonNode = null                  // pre-parsed Jackson tree, for the serialize side
  var uval: ujson.Value = ujson.Null         // pre-parsed uJson tree, for the serialize side

  val objectMapper = new ObjectMapper()

  @Setup(Level.Trial)
  def setup(): Unit =
    val r = new java.util.Random(0xC0FFEE)
    model = (0 until size).map { i =>
      GeoRecord(
        id = i,
        name = "record-" + r.nextInt(1000000),
        active = r.nextBoolean(),
        score = math.rint(r.nextDouble() * 1e6) / 1e4,
        tags = List.fill(r.nextInt(4))("t" + r.nextInt(50)),
        path = List.fill(2 + r.nextInt(4))(math.rint((r.nextDouble() * 360 - 180) * 1e6) / 1e6)
      )
    }.toList
    // Serialize the shared input with jsoniter so no parser is fed its own dialect.
    bytes = writeToArray(model)
    text  = new String(bytes, UTF_8)
    chars = text.toCharArray
    tree  = Json.parse(bytes).jsonOr(Jnull)
    node  = objectMapper.readTree(bytes)
    uval  = ujson.read(bytes)

  // === tree plane: parse ===

  @Benchmark
  def jsaunParseString(): Json = Json.parse(text).jsonOr(Jnull)

  @Benchmark
  def jsaunParseBytes(): Json = Json.parse(bytes).jsonOr(Jnull)

  @Benchmark
  def jsaunParseChars(): Json = Json.parse(chars).jsonOr(Jnull)

  // exact mode: numbers that a Double cannot hold exactly are kept as text (dyadic ones
  // still land in Jnum.D) -- measures the cost of the exactness check on this payload
  @Benchmark
  def jsaunParseExact(): Json = Json.parse(bytes, exact = true).jsonOr(Jnull)

  @Benchmark
  def jacksonTreeBytes(): JsonNode = objectMapper.readTree(bytes)

  @Benchmark
  def jacksonTreeString(): JsonNode = objectMapper.readTree(text)

  @Benchmark
  def ujsonParseBytes(): ujson.Value = ujson.read(bytes)

  @Benchmark
  def ujsonParseString(): ujson.Value = ujson.read(text)

  // === tree plane: serialize ===

  @Benchmark
  def jsaunPrint(): String = tree.print

  @Benchmark
  def jsaunPrintBytes(): Array[Byte] = tree.printBytes

  @Benchmark
  def jacksonWriteBytes(): Array[Byte] = objectMapper.writeValueAsBytes(node)

  @Benchmark
  def ujsonWriteBytes(): Array[Byte] = ujson.writeToByteArray(uval)

  // === typed plane: parse ===

  @Benchmark
  def jsaunCodecDecode(): List[GeoRecord] = Json.parse(bytes).to[List[GeoRecord]].get   // .get: a decode failure aborts the bench, never a silent fast error path

  @Benchmark
  def jsoniterDecode(): List[GeoRecord] = readFromArray[List[GeoRecord]](bytes)

  @Benchmark
  def upickleDecode(): List[GeoRecord] = upickle.default.read[List[GeoRecord]](bytes)

  // === typed plane: serialize ===

  @Benchmark
  def jsaunCodecEncode(): Array[Byte] = Json(model).printBytes

  @Benchmark
  def jsoniterEncode(): Array[Byte] = writeToArray(model)

  @Benchmark
  def upickleEncode(): Array[Byte] = upickle.default.writeToByteArray(model)
}
