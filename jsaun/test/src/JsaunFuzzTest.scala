// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.jsaun


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import scala.collection.mutable.ArrayBuffer

import kse.maths.Pcg64

import kse.jsaun.{Jsonize, FromJson}


// Property / fuzz tests: random JSON trees and random whitespace-laden JSON text,
// driven by a fixed-seed Pcg64 so failures are reproducible.  The fixed-case suite
// in JsaunTest covers the corners; this stresses combinations of nesting, escaping,
// whitespace, numeric forms, and edit sequences that hand-written cases can't reach.
@RunWith(classOf[JUnit4])
class JsaunFuzzTest {
  import kse.testutilities.TestUtilities.{given, _}
  import kse.basics.{given, _}
  import kse.flow.{given, _}
  import kse.jsaun.{given, _}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  private val u8 = java.nio.charset.StandardCharsets.UTF_8

  private def unwrap(ja: JAny): Json = ja.jsonOr(null)

  // A small pool of code points, biased toward the awkward ones: quotes, backslashes,
  // control chars, whitespace, non-ASCII BMP, and two astral code points.
  private val codePoints: Array[Int] =
    val b = ArrayBuffer.empty[Int]
    for c <- 'a' to 'e' do b += c.toInt
    b += ' '.toInt; b += '\t'.toInt; b += '\n'.toInt; b += '\r'.toInt
    b += '"'.toInt; b += '\\'.toInt; b += '/'.toInt
    b += 0x00; b += 0x01; b += 0x07; b += 0x1f     // controls (must be escaped)
    b += 0xe9; b += 0x20ac                          // é  €
    b += 0x1f600; b += 0x1d11e                       // 😀  𝄞  (astral)
    b.toArray

  private def randString(r: Pcg64, maxLen: Int): String =
    val n = r % (maxLen + 1)
    val sb = new java.lang.StringBuilder
    var i = 0
    while i < n do
      sb.appendCodePoint(codePoints(r % codePoints.length))
      i += 1
    sb.toString

  // Encode `s` as a legal JSON string literal, choosing among the permitted escapes at
  // random so parseFmt's verbatim preservation and the unescape path both get exercised.
  // Iterates by code point so a supplementary character's surrogate pair is kept together
  // (a lone raw surrogate would not survive UTF-8 encoding in the bytes-source check).
  private def jsonStringLiteral(r: Pcg64, s: String): String =
    val sb = new java.lang.StringBuilder
    sb.append('"')
    var i = 0
    while i < s.length do
      val cp = s.codePointAt(i)
      if cp > 0xFFFF then
        if r.Z then sb.appendCodePoint(cp) __ Unit
        else
          val hi = 0xD800 + ((cp - 0x10000) >> 10)
          val lo = 0xDC00 + ((cp - 0x10000) & 0x3FF)
          sb.append("\\u%04x\\u%04x".format(hi, lo)) __ Unit
        i += 2
      else
        cp.toChar match
          case '"'  => sb.append("\\\"")
          case '\\' => sb.append("\\\\")
          case '\b' => sb.append(if r.Z then "\\b" else "\\u0008")
          case '\f' => sb.append(if r.Z then "\\f" else "\\u000c")
          case '\n' => sb.append(if r.Z then "\\n" else "\\u000a")
          case '\r' => sb.append(if r.Z then "\\r" else "\\u000d")
          case '\t' => sb.append(if r.Z then "\\t" else "\\u0009")
          case '/'  => sb.append(if r.Z then "/" else "\\/")
          case c =>
            if c < 0x20 then sb.append("\\u%04x".format(c.toInt))
            else if (r % 4) == 0 then sb.append("\\u%04x".format(c.toInt))  // optional \u for any BMP char
            else sb.append(c)
        i += 1
    sb.append('"')
    sb.toString

  // A valid JSON number literal in one of several shapes.  Returned as text (for the
  // text generator) so the exact spelling can be checked for verbatim preservation.
  private def randNumberLiteral(r: Pcg64): String =
    r % 6 match
      case 0 => r.I.toString
      case 1 => r.L.toString
      case 2 =>
        val d = (r.D * 2 - 1) * math.pow(10, (r % 40) - 20)
        d.toString
      case 3 => "0"
      case 4 => s"${r % 1000}.${r % 1000}"
      case _ => s"${r % 100}e${if r.Z then "+" else "-"}${r % 30}"

  // === random tree generator (for print -> parse round trips) ===

  private def randFiniteDouble(r: Pcg64): Double =
    var d = 0.0
    while { d = java.lang.Double.longBitsToDouble(r.L); d.isNaN || d.isInfinite || d == 0.0 } do ()
    d

  private def randTree(r: Pcg64, depth: Int): Json =
    val leafOnly = depth <= 0
    val pick = if leafOnly then r % 5 else r % 7
    pick match
      case 0 => Jnull
      case 1 => Jbool(r.Z)
      case 2 => if r.Z then Jnum(r.L) else Jnum(randFiniteDouble(r))
      case 3 => Jstr(randString(r, 6))
      case 4 => Jstr("")
      case 5 =>
        val n = r % 5
        Jarr(Array.fill(n)(randTree(r, depth - 1))*)
      case _ =>
        val n = r % 5
        val used = scala.collection.mutable.HashSet.empty[String]
        val kvs = ArrayBuffer.empty[(String, Json)]
        var i = 0
        while i < n do
          val k = randString(r, 5)
          if used.add(k) then kvs += (k -> randTree(r, depth - 1))   // keep keys unique for clean equality
          i += 1
        Jobj(kvs.toSeq*)

  // === random JSON text generator (for verbatim + cross-source) ===

  private def ws(r: Pcg64): String =
    val n = r % 3
    val sb = new java.lang.StringBuilder
    var i = 0
    while i < n do
      sb.append(" \t\n\r".charAt(r % 4))
      i += 1
    sb.toString

  // `container` forces an array/object at this position; used at the document root because
  // format preservation attaches to containers -- a bare scalar root is re-serialized
  // canonically (see bareScalarRootTest), so verbatim checks need a container to hang on.
  private def randText(r: Pcg64, depth: Int, sb: java.lang.StringBuilder, container: Boolean = false): Unit =
    val leafOnly = depth <= 0
    val pick = if container then 4 + (r % 2) else if leafOnly then r % 4 else r % 6
    pick match
      case 0 => sb.append("null") __ Unit
      case 1 => sb.append(if r.Z then "true" else "false") __ Unit
      case 2 => sb.append(randNumberLiteral(r)) __ Unit
      case 3 => sb.append(jsonStringLiteral(r, randString(r, 6))) __ Unit
      case 4 =>
        val n = r % 5
        sb.append('[').append(ws(r)) __ Unit
        var i = 0
        while i < n do
          if i > 0 then sb.append(ws(r)).append(',').append(ws(r)) __ Unit
          randText(r, depth - 1, sb)
          i += 1
        sb.append(ws(r)).append(']') __ Unit
      case _ =>
        val n = r % 5
        sb.append('{').append(ws(r)) __ Unit
        var i = 0
        while i < n do
          if i > 0 then sb.append(ws(r)).append(',').append(ws(r)) __ Unit
          sb.append(jsonStringLiteral(r, randString(r, 4))) __ Unit
          sb.append(ws(r)).append(':').append(ws(r)) __ Unit
          randText(r, depth - 1, sb)
          i += 1
        sb.append(ws(r)).append('}') __ Unit


  @Test
  def bareScalarRootTest(): Unit =
    // Format preservation attaches spans to containers; a bare scalar at the document root
    // has nothing to hang them on, so it is re-serialized canonically (same in both modes).
    // Inside a container, the very same token IS preserved verbatim.
    T ~ unwrap(Json.parseFmt("1e2")).print          ==== "100.0"
    T ~ unwrap(Json.M.parseFmt("1e2")).print        ==== "100.0"
    T ~ unwrap(Json.parseFmt("\"\\u0041\"")).print  ==== "\"A\""
    T ~ unwrap(Json.parseFmt("[1e2]")).print        ==== "[1e2]"        // preserved once wrapped
    T ~ unwrap(Json.parseFmt("""{"k":"A"}""")).print ==== """{"k":"A"}"""
    T ~ unwrap(Json.M.parseFmt("[1e2]")).print      ==== "[1e2]"

  @Test
  def treeRoundTripFuzz(): Unit =
    val r = Pcg64(0x15A0F1L)
    var iter = 0
    while iter < 3000 do
      val tree = randTree(r, 4)
      val s = tree.print
      // print -> parse recovers the same tree (structural equality, backing-agnostic)
      T ~ Json.parse(s).ask               ==== Is(tree)
      // UTF-8 byte rendering round-trips identically
      T ~ Json.parse(tree.printBytes).ask ==== Is(tree)
      // the two renderings agree byte-for-byte
      T ~ new String(tree.printBytes, u8) ==== s
      // printing is idempotent: re-parsing and re-printing yields the same text
      T ~ unwrap(Json.parse(s)).print     ==== s
      iter += 1

  @Test
  def textVerbatimFuzz(): Unit =
    val r = Pcg64(0x25A0F2L)
    var iter = 0
    while iter < 3000 do
      val sb = new java.lang.StringBuilder
      randText(r, 4, sb, container = true)
      val text = sb.toString
      val parsed = Json.parse(text)
      // the generator only emits legal JSON
      T ~ parsed.isErr ==== false
      // format-preserving parse reproduces the source verbatim, from String and from bytes
      T ~ unwrap(Json.parseFmt(text)).print ==== text
      T ~ (new String(unwrap(Json.parseFmt(text.getBytes(u8))).printBytes, u8)) ==== text
      // an unedited format-preserving mutable tree is verbatim too
      T ~ unwrap(Json.M.parseFmt(text)).print ==== text
      // all three sources parse to the same value
      val fromStr   = parsed.ask
      val fromBytes = Json.parse(text.getBytes(u8)).ask
      val fromChars = Json.parse(text.toCharArray).ask
      T ~ fromStr ==== fromBytes
      T ~ fromStr ==== fromChars
      // off-heap Mem sources agree with their on-heap counterparts, plain and format-preserving
      T ~ Json.parse(Mem.of(text.getBytes(u8))).ask ==== fromStr
      T ~ Json.parse(Mem.of(text.toCharArray)).ask  ==== fromStr
      T ~ unwrap(Json.parseFmt(Mem.of(text.getBytes(u8)))).print ==== text
      // the visitor stays structurally in sync: a full visit and a skip-everything visit both
      // consume the whole document (the latter exercises the structural skip over the entire tree)
      def ok(a: Ask[Unit]) = a match { case Alt(_) => false; case _ => true }
      T ~ ok(Json.stream(text)(new Jvisitor {})) ==== true
      T ~ ok(Json.stream(text)(new Jvisitor { override def objStart() = false; override def arrStart() = false })) ==== true
      // exact mode keeps the exact decimal value: printing then re-parsing in exact
      // mode is a fixed point (a non-exact re-parse would round Jnum.Big down to Jnum.D)
      val exact = unwrap(Json.parse(text, exact = true))
      T ~ Json.parse(exact.print, exact = true).ask ==== Is(exact)
      iter += 1

  // An InputStream that answers at most `max` bytes per read, so refills land mid-token
  private class DribbleIn(b: Array[Byte], max: Int) extends java.io.InputStream {
    private var p = 0
    def read(): Int = if p >= b.length then -1 else { val x = b(p) & 0xFF; p += 1; x }
    override def read(dst: Array[Byte], off: Int, len: Int): Int =
      if p >= b.length then -1
      else
        var n = len
        if n > max then n = max
        if n > b.length - p then n = b.length - p
        System.arraycopy(b, p, dst, off, n)
        p += n
        n
  }

  private class DribbleRd(c: Array[Char], max: Int) extends java.io.Reader {
    private var p = 0
    def close(): Unit = ()
    def read(dst: Array[Char], off: Int, len: Int): Int =
      if p >= c.length then -1
      else
        var n = len
        if n > max then n = max
        if n > c.length - p then n = c.length - p
        System.arraycopy(c, p, dst, off, n)
        p += n
        n
  }

  // Random-sized chunks (with occasional empty ones) covering `n` items via `cut`
  private def randChunks[A](r: Pcg64, n: Int)(cut: (Int, Int) => A): Iterator[A] =
    val chunks = ArrayBuffer.empty[A]
    var p = 0
    while p < n do
      if (r % 5) == 0 then chunks += cut(p, p)   // empty chunk: must be invisible
      val q = { val x = p + 1 + (r % 6); if x > n then n else x }
      chunks += cut(p, q)
      p = q
    chunks.iterator

  @Test
  def streamedCrossSourceFuzz(): Unit =
    val r = Pcg64(0x65A0F6L)
    def ok(a: Ask[Unit]) = a match { case Alt(_) => false; case _ => true }
    var iter = 0
    while iter < 800 do
      val sb = new java.lang.StringBuilder
      randText(r, 4, sb, container = true)
      val text = sb.toString
      val ref = Json.parse(text).ask
      val bytes = text.getBytes(u8)
      val chars = text.toCharArray
      // chunk iterators with random boundaries through a tiny window: every token shape
      // eventually straddles a chunk edge, a refill, and a window slide
      T ~ Json.parse(randChunks(r, bytes.length)((a, b) => java.util.Arrays.copyOfRange(bytes, a, b)), 16).ask ==== ref
      T ~ Json.parse(randChunks(r, chars.length)((a, b) => java.util.Arrays.copyOfRange(chars, a, b)), 16).ask ==== ref
      // short-read InputStream/Reader
      T ~ Json.parse(new DribbleIn(bytes, 1 + (r % 3)), 16).ask ==== ref
      T ~ Json.parse(new DribbleRd(chars, 1 + (r % 3)), 16).ask ==== ref
      // line-fed: splitting on newlines reconstructs the same document through implied newlines
      // (generated strings never contain a raw newline -- it would be illegal JSON)
      T ~ Json.parse(text.split("\n", -1).iterator).ask ==== ref
      // skip-everything visitor over a dribbled tiny-window stream tracks structure exactly
      T ~ ok(Json.stream(new DribbleIn(bytes, 2), 16)(new Jvisitor {
            override def objStart() = false
            override def arrStart() = false
          })) ==== true
      iter += 1

  @Test
  def compactFormatFuzz(): Unit =
    val r = Pcg64(0x35A0F3L)
    var iter = 0
    while iter < 1500 do
      val sb = new java.lang.StringBuilder
      randText(r, 4, sb, container = true)
      val text = sb.toString
      // compactFormat releases the source but must still print to something that
      // parses back to the same value (both on immutable and mutable trees)
      val a = unwrap(Json.parseFmt(text)).compactFormat()
      T ~ Json.parse(a.print).ask ==== Json.parse(text).ask
      val b = unwrap(Json.M.parseFmt(text)).compactFormat()
      T ~ Json.parse(b.print).ask ==== Json.parse(text).ask
      // reprint in the two canned styles is always re-parseable to the same value
      T ~ Json.parse(unwrap(Json.parseFmt(text)).reprint(Jstyle.compact)).ask ==== Json.parse(text).ask
      T ~ Json.parse(unwrap(Json.parseFmt(text)).reprint(Jstyle.pretty)).ask  ==== Json.parse(text).ask
      iter += 1

  @Test
  def objectEditFuzz(): Unit =
    val r = Pcg64(0x45A0F4L)
    val keys = ('a' to 'l').map(_.toString).toArray    // small alphabet -> collisions, crosses the 8-key index threshold
    var trial = 0
    while trial < 400 do
      val m = Jobj.M()
      // reference model: insertion-ordered, duplicate-free (we only ever put/remove)
      val model = ArrayBuffer.empty[(String, Json)]
      def modelPut(k: String, v: Json): Unit =
        val i = model.indexWhere(_._1 == k)
        if i >= 0 then model(i) = (k -> v) else model += (k -> v)
      def modelRemove(k: String): Unit =
        val i = model.indexWhere(_._1 == k)
        if i >= 0 then model.remove(i) __ Unit
      var op = 0
      val ops = 30
      while op < ops do
        val k = keys(r % keys.length)
        r % 4 match
          case 0 | 1 =>
            val v = randTree(r, 2)
            m.put(k, v) __ Unit
            modelPut(k, v)
          case 2 =>
            m.remove(k) __ Unit
            modelRemove(k)
          case _ =>
            m(k) = Jnum(op.toLong)
            modelPut(k, Jnum(op.toLong))
        T ~ m.size ==== model.length
        for (kk, vv) <- model do T ~ m(kk).ask ==== Is(vv)
        T ~ m.contains(k) ==== model.exists(_._1 == k)
        op += 1
      // the edited tree prints to something that parses back to the reference object
      val expected: Json = Jobj(model.toSeq*)
      T ~ Json.parse(m.print).ask ==== Is(expected)
      // view aliasing: upcast sees the same state, no copy
      val view: Jobj = m
      T ~ (view: Json) ==== (expected: Json)
      trial += 1

  @Test
  def arrayEditFuzz(): Unit =
    val r = Pcg64(0x55A0F5L)
    var trial = 0
    while trial < 400 do
      val a = Jarr.A.M()
      val model = ArrayBuffer.empty[Json]
      var op = 0
      val ops = 30
      while op < ops do
        r % 5 match
          case 0 | 1 =>
            val v = randTree(r, 2)
            a.add(v) __ Unit
            model += v
          case 2 if model.nonEmpty =>
            val i = r % (model.length + 1)
            val v = randTree(r, 2)
            a.insert(i, v) __ Unit
            model.insert(i, v)
          case 3 if model.nonEmpty =>
            val i = r % model.length
            a.remove(i) __ Unit
            model.remove(i) __ Unit
          case 4 if model.nonEmpty =>
            val i = r % model.length
            val v = randTree(r, 2)
            a(i) = v
            model(i) = v
          case _ =>
            val v = randTree(r, 2)
            a.add(v) __ Unit
            model += v
        T ~ a.size ==== model.length
        var i = 0
        while i < model.length do
          T ~ a(i).ask ==== Is(model(i))
          i += 1
        op += 1
      val expected: Json = Jarr(model.toSeq*)
      T ~ Json.parse(a.print).ask ==== Is(expected)
      trial += 1
}
