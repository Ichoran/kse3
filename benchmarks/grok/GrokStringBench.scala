// JMH benchmark: parsing a JSON array of strings when you KNOW it's a string array.
//
// Two workloads: "easy" is 50 short common words/numbers with no escapes (the clean fast
// path: one substring / byte copy per string); "tricky" is 20 long strings dense with
// embedded quotes, backslashes, \uXXXX escapes, controls, and raw non-ASCII (the builder
// slow path).  The bar here is modest: Jackson and jsoniter have heavily tuned string
// decoders, so we mainly want to be not-absurdly-behind on tricky while winning or tying
// on easy — and grokStrBytesRaw shows what skipping the decode entirely buys on bytes.
//
// The grokCsv* rows parse the SAME content encoded CSV-style (RFC 4180: everything literal,
// including raw controls and non-ASCII; only embedded quotes escaped, by doubling), read
// with Quote.csv.  handCsvStr is a hand-rolled indexOf-based CSV string decoder over the
// same text — the speed-of-light reference for the doubled-quote style.
//
// Build the jar first (from the repo root):   mill all.assembly
// Then run (from the repo root):
//   taskset -c 4 scala-cli --power run benchmarks/grok --jmh -- -f 1 -wi 6 -i 8 -w 1 -r 1 GrokStringBench

package kse.bench.grok

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.*

import com.fasterxml.jackson.core.{JsonFactory, JsonToken}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromArray}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import kse.basics.*
import kse.flow.*
import kse.eio.*


object StrCodecs:
  given strArrayCodec: JsonValueCodec[Array[String]] = JsonCodecMaker.make

import StrCodecs.given


@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class GrokStringBench {
  /** "easy" = 50 short clean words/numbers; "tricky" = 20 long escape-dense strings */
  @Param(Array("easy", "tricky"))
  var mode: String = ""

  var content: Array[String] = Array.empty
  var text: String = ""
  var bytes: Array[Byte] = Array.empty
  var charsArr: Array[Char] = Array.empty
  var csvText: String = ""
  var csvBytes: Array[Byte] = Array.empty
  var csvCharsArr: Array[Char] = Array.empty
  var csvMem: Mem[Byte] = null.asInstanceOf[Mem[Byte]]

  val jsonFactory = new JsonFactory()

  private def jsonEnc(s: String, uEsc: Boolean): String =
    val sb = new java.lang.StringBuilder("\"")
    var k = 0
    while k < s.length do
      val c = s.charAt(k)
      if c == '"' then sb.append("\\\"")
      else if c == '\\' then sb.append("\\\\")
      else if c == '\n' then sb.append("\\n")
      else if c == '\r' then sb.append("\\r")
      else if c == '\t' then sb.append("\\t")
      else if c < ' ' || (uEsc && c > 126) then sb.append("\\u%04x".format(c.toInt))
      else sb.append(c)
      k += 1
    sb.append('"').toString

  private def csvEnc(s: String): String =
    "\"" + s.replace("\"", "\"\"") + "\""

  @Setup(Level.Trial)
  def setup(): Unit =
    val words = Array(
      "the","of","and","to","a","in","that","is","was","he",
      "for","it","with","as","his","on","be","at","by","had",
      "not","are","but","from","or","have","an","they","which","one",
      "you","were","her","all","she","there","would","their","we","him",
      "42","7","3.14","100","2026","-1","0.5","1e6","987654","0"
    )
    val r = new java.util.Random(0x5EED5)
    content = mode match
      case "easy" => words
      case "esc" =>            // simple escapes only (\" \\ \n \t \r), pure ASCII
        Array.tabulate(20){ k =>
          k % 3 match
            case 0 => Array.fill(30)("say \"" + words(r.nextInt(words.length)) + "\"").mkString(", ")
            case 1 => ("C:\\path\\to\\" + words(r.nextInt(words.length)) + "\\") * 10
            case _ => "line one\nline two\ttabbed\r\n" * 12
        }
      case "uni" | "raw" =>    // unicode-rich; "uni" encodes every non-ASCII char as \uXXXX, "raw" none
        Array.tabulate(20){ _ => Array.fill(40)(words(r.nextInt(words.length)) + " ☃ π ").mkString }
      case _ =>
        Array.tabulate(20){ k =>
          k % 5 match
            case 0 => Array.fill(60)(words(r.nextInt(words.length))).mkString(" ")            // long, clean
            case 1 => Array.fill(30)("say \"" + words(r.nextInt(words.length)) + "\"").mkString(", ")  // quote-dense
            case 2 => ("C:\\path\\to\\" + words(r.nextInt(words.length)) + "\\") * 10         // backslash-dense
            case 3 => Array.fill(40)(words(r.nextInt(words.length)) + " ☃ π ").mkString       // non-ASCII, raw or \u
            case _ => "line one\nline two\ttabbed\r\n" * 12                                   // control escapes
        }
    val uEscOf: Int => Boolean = mode match
      case "uni" => _ => true
      case "raw" => _ => false
      case _     => k => k % 2 == 0
    text = content.zipWithIndex.map((s, k) => jsonEnc(s, uEscOf(k))).mkString("[", ", ", "]")
    bytes = text.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    charsArr = text.toCharArray
    csvText = content.map(csvEnc).mkString("[", ", ", "]")
    csvBytes = csvText.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    csvCharsArr = csvText.toCharArray
    csvMem = Mem of csvBytes
    // Guard against benchmarking a wrong parse
    val check = Grok(text, delim = Delim.white, partial = true): g =>
      val xsb = Array.newBuilder[String]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.str
      xsb.result()
    if check.get.toSeq != content.toSeq then throw new AssertionError("Grok parse does not match content")
    if grokCsvStr().toSeq != content.toSeq then throw new AssertionError("Grok CSV parse does not match content")
    if grokBufCsvStr().toSeq != content.toSeq then throw new AssertionError("Grok buffered CSV parse does not match content")
    if handCsvStr().toSeq != content.toSeq then throw new AssertionError("Hand-rolled CSV parse does not match content")
  @Benchmark
  def grokStr(): Array[String] =
    Grok(text, delim = Delim.white, partial = true): g =>
      val xsb = Array.newBuilder[String]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.str
      xsb.result()
    .get

  @Benchmark
  def grokStrBytes(): Array[String] =
    Grok(bytes, Delim.white, true, false): g =>
      val xsb = Array.newBuilder[String]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.str
      xsb.result()
    .get

  // Strings kept as UTF-8: on the clean path this never decodes (pure byte copy)
  @Benchmark
  def grokStrBytesRaw(): Array[Array[Byte]] =
    Grok(bytes, Delim.white, true, false): g =>
      val xsb = Array.newBuilder[Array[Byte]]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.strBytes
      xsb.result()
    .get

  @Benchmark
  def grokBufStr(): Array[String] =
    Grok.buffered(bytes, Delim.white, partial = true): g =>
      val xsb = Array.newBuilder[String]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.str
      xsb.result()
    .get

  // Windowed char source fed from a CharBuffer: the chunked kernel's blit copies out of the window
  @Benchmark
  def grokBufCharsStr(): Array[String] =
    Grok.buffered(java.nio.CharBuffer.wrap(charsArr), Delim.white, true, false, 64): g =>
      val xsb = Array.newBuilder[String]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.str
      xsb.result()
    .get

  @Benchmark
  def grokCsvStr(): Array[String] =
    Grok(csvText, delim = Delim.white, partial = true): g =>
      val xsb = Array.newBuilder[String]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.str(Quote.csv)
      xsb.result()
    .get

  @Benchmark
  def grokCsvStrBytes(): Array[String] =
    Grok(csvBytes, Delim.white, true, false): g =>
      val xsb = Array.newBuilder[String]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.str(Quote.csv)
      xsb.result()
    .get

  @Benchmark
  def grokCsvStrBytesRaw(): Array[Array[Byte]] =
    Grok(csvBytes, Delim.white, true, false): g =>
      val xsb = Array.newBuilder[Array[Byte]]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.strBytes(Quote.csv)
      xsb.result()
    .get

  @Benchmark
  def grokCsvMemStr(): Array[String] =
    Grok(csvMem, Delim.white, true, false): g =>
      val xsb = Array.newBuilder[String]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.str(Quote.csv)
      xsb.result()
    .get

  @Benchmark
  def grokCsvMemStrBytesRaw(): Array[Array[Byte]] =
    Grok(csvMem, Delim.white, true, false): g =>
      val xsb = Array.newBuilder[Array[Byte]]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.strBytes(Quote.csv)
      xsb.result()
    .get

  @Benchmark
  def grokBufCsvStr(): Array[String] =
    Grok.buffered(csvBytes, Delim.white, partial = true): g =>
      val xsb = Array.newBuilder[String]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.str(Quote.csv)
      xsb.result()
    .get

  @Benchmark
  def grokBufCharsCsvStr(): Array[String] =
    Grok.buffered(java.nio.CharBuffer.wrap(csvCharsArr), Delim.white, true, false, 64): g =>
      val xsb = Array.newBuilder[String]
      (g < '[') __ Unit
      var more = true
      while more do
        g.sp.peek match
          case ']' => more = false
          case ',' => (g < ",") __ Unit
          case _   => xsb += g.str(Quote.csv)
      xsb.result()
    .get

  // Speed-of-light reference: indexOf to each quote, substring when clean, StringBuilder
  // segments across doubled quotes.  No error handling, no generality.
  @Benchmark
  def handCsvStr(): Array[String] =
    val s = csvText
    val out = Array.newBuilder[String]
    var p = s.indexOf('"')
    while p >= 0 do
      var q = s.indexOf('"', p + 1)
      if q + 1 < s.length && s.charAt(q + 1) == '"' then
        val sb = new java.lang.StringBuilder
        var k = p + 1
        while q + 1 < s.length && s.charAt(q + 1) == '"' do
          sb.append(s, k, q + 1) __ Unit
          k = q + 2
          q = s.indexOf('"', k)
        sb.append(s, k, q) __ Unit
        out += sb.toString
      else
        out += s.substring(p + 1, q)
      p = s.indexOf('"', q + 1)
    out.result()

  @Benchmark
  def jsoniterScalaStr(): Array[String] =
    readFromArray[Array[String]](bytes)

  @Benchmark
  def jacksonStreamBytesStr(): Array[String] =
    val p = jsonFactory.createParser(bytes)
    var out = new Array[String](16)
    var n = 0
    p.nextToken()  // START_ARRAY
    var t = p.nextToken()
    while t != JsonToken.END_ARRAY do
      if n >= out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
      out(n) = p.getText
      n += 1
      t = p.nextToken()
    p.close()
    java.util.Arrays.copyOf(out, n)

  @Benchmark
  def jacksonStreamStringStr(): Array[String] =
    val p = jsonFactory.createParser(text)
    var out = new Array[String](16)
    var n = 0
    p.nextToken()  // START_ARRAY
    var t = p.nextToken()
    while t != JsonToken.END_ARRAY do
      if n >= out.length then out = java.util.Arrays.copyOf(out, out.length * 2)
      out(n) = p.getText
      n += 1
      t = p.nextToken()
    p.close()
    java.util.Arrays.copyOf(out, n)
}
