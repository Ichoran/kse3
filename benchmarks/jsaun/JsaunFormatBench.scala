// JMH benchmark: format-preserving read-modify-write, jsaun's headline feature.
//
// The task: parse a formatted JSON document, change ONE field, serialize.  jsaun's
// parseFmt keeps every untouched byte exactly as it was and re-emits only the edited
// token; the other libraries have no format memory, so their output reflows the whole
// document (different whitespace, key order, number spelling).  These benchmarks measure
// the throughput cost of that guarantee against the reflow-everything alternatives.
//
// The jsaunPlain* row is the honest in-family baseline: same jsaun tree, but a plain
// parse + reprint that does NOT preserve formatting -- so the gap to jsaunFmt is the price
// of the span bookkeeping, and the gap from jsaunPlain to Jackson/uJson is just jsaun's
// parser/printer speed.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              scala-cli --power run benchmarks/jsaun --jmh -- -f 1 -wi 5 -i 5 -w 1 -r 1 JsaunFormatBench
// Pin for stability, e.g.:                    taskset -c 4 scala-cli --power run benchmarks/jsaun --jmh -- JsaunFormatBench

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

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.ObjectNode

import kse.flow.{given, *}
import kse.jsaun.{given, *}


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class JsaunFormatBench {
  @Param(Array("40"))
  var size: Int = 0

  var text: String = ""             // a pretty-printed object with a top-level numeric "version"

  val objectMapper = new ObjectMapper()

  @Setup(Level.Trial)
  def setup(): Unit =
    val r = new java.util.Random(0xF00D)
    val items = (0 until size).map { i =>
      Jobj(
        "id"    -> Jnum(i.toLong),
        "name"  -> Jstr("item-" + r.nextInt(100000)),
        "on"    -> Jbool(r.nextBoolean()),
        "vals"  -> Jarr(Array.fill(3)(math.rint(r.nextDouble() * 1e4) / 1e2))
      )
    }
    val doc = Jobj(
      "name"    -> Jstr("config"),
      "version" -> Jnum(1L),
      "items"   -> Jarr(items*)
    )
    text = doc.print(using Jstyle.pretty)

  // jsaun, format-preserving: edit "version" in place; every other byte is emitted verbatim
  // from the retained source.  Output differs from input only in the "version" token.
  @Benchmark
  def jsaunFmtEdit(): String =
    Json.M.parseFmt(text).jsonOr(Jnull) match
      case o: Jobj.M =>
        o("version") = Jnum(2L)
        o.print
      case _ => ""

  // jsaun, in-family baseline: plain parse then canonical reprint (formatting NOT preserved)
  @Benchmark
  def jsaunPlainEdit(): String =
    Json.M.parse(text).jsonOr(Jnull) match
      case o: Jobj.M =>
        o("version") = Jnum(2L)
        o.print(using Jstyle.pretty)
      case _ => ""

  // Jackson tree: read, set field, re-emit with the pretty printer (reflows; cannot match
  // the original layout, key order, or number spelling)
  @Benchmark
  def jacksonEdit(): String =
    val n = objectMapper.readTree(text).asInstanceOf[ObjectNode]
    n.put("version", 2L)
    objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(n)

  // uJson tree: read, set field, re-emit indented (also reflows)
  @Benchmark
  def ujsonEdit(): String =
    val v = ujson.read(text)
    v.obj("version") = ujson.Num(2.0)
    ujson.write(v, indent = 2)
}
