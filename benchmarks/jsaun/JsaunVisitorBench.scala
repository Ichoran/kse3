// JMH benchmark: the SAX-style visitor (Json.stream) and its skip gates.
//
// The document is an array of records, each with two small fields we want (id, name) and bulk we
// don't (a 20-element `data` array, `tags`, `active`).  Extracting just id+name is where the
// visitor earns its keep: declining a key makes the parser skip its value structurally (match
// brackets/quotes, decode nothing, allocate nothing) instead of building it.
//
//   extract plane -- pull id+name from every record.  jsaun's skipping visitor vs building the
//                    whole tree then navigating (jsaunTreeExtract) vs Jackson's streaming parser
//                    with skipChildren (the same idea in the reference).
//   full plane    -- touch every number (no skipping).  Visitor vs full tree parse vs Jackson
//                    streaming -- the cost of a complete traversal that builds nothing.
//   source check  -- the same extract over String / bytes / chars / Mem[Byte], confirming Mem
//                    works as a visitor source and how the encodings compare.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):              scala-cli --power run benchmarks/jsaun --jmh -- -f 2 -wi 5 -i 5 -w 1 -r 1 JsaunVisitorBench

//> using scala 3.8.3
//> using jvm system
//> using jar ../../out/all/assembly.dest/out.jar
//> using dep com.fasterxml.jackson.core:jackson-databind:2.18.2

package kse.bench.jsaun

import java.util.concurrent.TimeUnit
import java.nio.charset.StandardCharsets.UTF_8

import org.openjdk.jmh.annotations.*

import com.fasterxml.jackson.core.{JsonFactory, JsonToken}

import kse.basics.{given, *}
import kse.flow.{given, *}
import kse.jsaun.{given, *}


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class JsaunVisitorBench {
  @Param(Array("100"))
  var size: Int = 0

  var text: String = ""
  var bytes: Array[Byte] = Array.empty
  var chars: Array[Char] = Array.empty
  var mem: Mem[Byte] = Mem.of(Array.empty[Byte])

  val jsonFactory = new JsonFactory()

  @Setup(Level.Trial)
  def setup(): Unit =
    val r = new java.util.Random(0xBEEF)
    val sb = new java.lang.StringBuilder
    sb.append('[')
    var i = 0
    while i < size do
      if i > 0 then sb.append(',')
      sb.append("{\"id\":").append(i)
      sb.append(",\"name\":\"rec-").append(r.nextInt(1000000)).append('"')
      sb.append(",\"data\":[")
      var j = 0
      while j < 20 do
        if j > 0 then sb.append(',')
        sb.append(r.nextInt(1000000) / 1000.0)
        j += 1
      sb.append("],\"tags\":[\"a\",\"b\",\"c\"],\"active\":").append(r.nextBoolean()).append('}')
      i += 1
    sb.append(']')
    text  = sb.toString
    bytes = text.getBytes(UTF_8)
    chars = text.toCharArray
    mem   = Mem.of(bytes)

  // === visitors ===

  // Wants only id + name; every other key is declined and its value skipped structurally.
  final class Extract extends Jvisitor {
    var acc = 0.0
    private var k = ""
    override def key(key: String): Boolean = { k = key; (k == "id") || (k == "name") }
    override def num(l: Long): Unit = if k == "id" then acc += l.toDouble
    override def str(s: String): Unit = if k == "name" then acc += s.length
  }

  // Touches every number; no skipping.
  final class SumAll extends Jvisitor {
    var acc = 0.0
    override def num(l: Long): Unit = acc += l.toDouble
    override def num(d: Double): Unit = acc += d
  }

  // === extract plane ===

  @Benchmark
  def jsaunVisitExtract(): Double =
    val v = new Extract
    Json.stream(bytes)(v) __ Unit
    v.acc

  @Benchmark
  def jsaunVisitExtractString(): Double =
    val v = new Extract
    Json.stream(text)(v) __ Unit
    v.acc

  @Benchmark
  def jsaunVisitExtractChars(): Double =
    val v = new Extract
    Json.stream(chars)(v) __ Unit
    v.acc

  // Confirms Mem[Byte] drives the visitor, and measures the off-heap source.
  @Benchmark
  def jsaunVisitExtractMem(): Double =
    val v = new Extract
    Json.stream(mem)(v) __ Unit
    v.acc

  // Baseline: build the whole tree, then navigate to id+name.  Pays to decode everything skipped.
  @Benchmark
  def jsaunTreeExtract(): Double =
    val arr = Json.parse(bytes).jsonOr(Jnull).asInstanceOf[Jarr]
    var acc = 0.0
    var i = 0
    while i < arr.size do
      val rec = arr(i)
      acc += rec("id").dblOr(0.0)
      acc += rec("name").strOr("").length
      i += 1
    acc

  // Jackson streaming with skipChildren: the reference's version of "decline this value".
  @Benchmark
  def jacksonStreamExtract(): Double =
    val p = jsonFactory.createParser(bytes)
    var acc = 0.0
    p.nextToken()                                  // START_ARRAY
    var t = p.nextToken()
    while t != JsonToken.END_ARRAY do              // t == START_OBJECT
      var f = p.nextToken()
      while f != JsonToken.END_OBJECT do           // f == FIELD_NAME
        val name = p.getCurrentName
        p.nextToken()                              // move onto the value
        if name == "id" then acc += p.getDoubleValue
        else if name == "name" then acc += p.getText.length
        else p.skipChildren()
        f = p.nextToken()
      t = p.nextToken()
    p.close()
    acc

  // === full plane ===

  @Benchmark
  def jsaunVisitSumAll(): Double =
    val v = new SumAll
    Json.stream(bytes)(v) __ Unit
    v.acc

  @Benchmark
  def jsaunParseTree(): Json = Json.parse(bytes).jsonOr(Jnull)

  @Benchmark
  def jacksonStreamSumAll(): Double =
    val p = jsonFactory.createParser(bytes)
    var acc = 0.0
    var t = p.nextToken()
    while t != null do
      if t == JsonToken.VALUE_NUMBER_INT || t == JsonToken.VALUE_NUMBER_FLOAT then acc += p.getDoubleValue
      t = p.nextToken()
    p.close()
    acc
}
