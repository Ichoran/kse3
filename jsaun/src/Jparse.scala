// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


import scala.util.boundary
import scala.compiletime.{erasedValue, error}

import kse.basics.{given, _}
import kse.flow.{given, _}
import kse.maths.{ULong, EiselLemire}


/** A strict (RFC 8259) recursive-descent JSON parser.
  *
  * Per-character work (whitespace, strings, numbers, literals) lives in inline templates that
  * each source subclass instantiates with its own accessor lambdas, so the hot loops are
  * monomorphic with no per-character virtual calls.  The structure-level descent is written
  * once, here, against the (at most bimorphic) worker methods: per-token dispatch is noise
  * next to per-character costs, and sharing it keeps the code from scaling with source count.
  *
  * The cursor is a `Long`.  In-memory sources use it as a plain index; streaming sources
  * (`Buffered`, `BufferedChars`) as an absolute offset that may pass `Int.MaxValue`; and the
  * line-fed source (`Lines`) packs (line index, offset within line) into its halves.  Sources
  * that do not know their length up front leave `iZ` at `Long.MaxValue`, answer -1 from their
  * accessor at the true end, and clamp `iZ` there once it is found.
  *
  * Failure is one `eErr` field set at the point of error and wrapped with structural context
  * (`explainBy`) as the descent unwinds, so errors arrive fully unrolled: what broke, where,
  * and inside which elements of which containers.  Only the public rim (`parseTop`) speaks
  * `Ask`.  A `Jparse` is mutable state for exactly one parse; do not reuse or share one.
  */
sealed abstract class Jparse protected () {

  protected var i = 0L
  protected var iZ = 0L
  protected var depth = 0
  protected var exactNum = false
  protected var asM = false
  protected var fmtMode = false
  protected var src: Jsrc = null   // set exactly when fmtMode
  protected var eErr: Err = Err("(no error)")   // meaningful only after a worker answers null/false

  // === Workers, instantiated per concrete source type from the inline templates below ===

  protected def wsWork(): Int
  protected def litWork(lit: String): Boolean
  protected def strWork(): String | Null
  protected def strEscWork(j0: Long, jN: Long): String | Null
  protected def numWork(): Json | Null
  protected def skipWork(): Boolean

  // === Cold access to raw content, for error reporting only ===

  protected def rawLength: Long
  protected def rawCharAt(pos: Long): Char


  //////////////////////////////
  /// Error construction     ///
  //////////////////////////////

  /** Pack (1-based line, 1-based char within line) for `pos`, for error messages.  The default
    * scans the input from the start; sources that cannot re-read (or that already know) override.
    */
  protected def lineColOf(pos: Long): Long =
    val n = rawLength
    val q = if pos > n then n else if pos < 0 then 0L else pos
    var line = 1
    var last = -1L
    var j = 0L
    while j < q do
      if rawCharAt(j) == '\n' then
        line += 1
        last = j
      j += 1
    (line.toLong << 32) | ((q - last) & 0xFFFFFFFFL)

  private def foundAt(pos: Long): String =
    if pos >= rawLength then "end of input"
    else rawCharAt(pos) match
      case '\n' | '\r' => "end of line"
      case '\t' => "tab"
      case c if c < ' ' => "control character " + c.toInt
      case c => "'" + c + "'"

  protected def posText(pos: Long): String =
    val lc = lineColOf(pos)
    s"line ${(lc >>> 32).toInt}, char ${(lc & 0xFFFFFFFFL).toInt}"

  /** Build a `Jerr` at `pos` with a caret-marked excerpt of the input there. */
  protected def errAt(desc: String, pos: Long): Err =
    val n = rawLength
    val q = if pos > n then n else if pos < 0 then 0L else pos
    val lc = lineColOf(q)
    var x0 = q - 8
    if x0 < 0 then x0 = 0
    var x1 = q + 8
    if x1 > n then x1 = n
    val sb = new java.lang.StringBuilder
    if x0 > 0 then sb.append("...") __ Unit
    val off = sb.length + (q - x0).toInt
    var j = x0
    while j < x1 do
      val c = rawCharAt(j)
      sb.append(if c < ' ' then '·' else c) __ Unit
      j += 1
    if x1 < n then sb.append("...") __ Unit
    Err(new Jerr(desc, q, (lc >>> 32).toInt, (lc & 0xFFFFFFFFL).toInt, sb.toString, off))

  protected final def fail(wanted: String, pos: Long): Null =
    eErr = errAt(s"expected $wanted, found ${foundAt(pos)}", pos)
    null

  protected final def failMsg(msg: String, pos: Long): Null =
    eErr = errAt(msg, pos)
    null

  /** Wrap the pending error with one level of structural context on the way out. */
  protected final def explain(msg: String): Null =
    eErr = eErr.explainBy(msg)
    null


  ///////////////////////////////////////
  /// Structure-level recursive descent ///
  ///////////////////////////////////////

  /** Parse the value whose first character `c` is at the cursor (as reported by `wsWork`). */
  protected final def parseValue(c: Int): Json | Null =
    if c == '{' then parseObj()
    else if c == '[' then parseArr()
    else if c == '"' then
      val s = strWork()
      if s eq null then null else new Jstr(s)
    else if c == '-' || (c >= '0' && c <= '9') then numWork()
    else if c == 't' then { if litWork("true") then Jbool.True else null }
    else if c == 'f' then { if litWork("false") then Jbool.False else null }
    else if c == 'n' then { if litWork("null") then Jnull else null }
    else fail("a JSON value", i)

  protected final def parseArr(): Json | Null =
    val p0 = i
    if depth >= Jparse.maxDepth then return failMsg(s"JSON nested more than ${Jparse.maxDepth} levels deep", p0)
    depth += 1
    i += 1
    var c = wsWork()
    if c == ']' then
      i += 1
      depth -= 1
      return
        if asM || fmtMode then
          val node: Jarr = if asM then new Jarr.A.M() else new Jarr.A(new Array[Json](0), 0)
          if fmtMode then node.fmt = new Jfmt(src, Jfmt.span(p0.toInt, i.toInt), new Array[Long](0))
          node
        else Jarr.empty
    var vs = new Array[Json](8)
    var sp: Array[Long] = if fmtMode then new Array[Long](8) else null
    var n = 0
    var allD = true
    while true do
      val v0 = i
      val v = parseValue(c)
      if v eq null then return explain(s"in element $n of array started at ${posText(p0)}:")
      if n >= vs.length then
        vs = java.util.Arrays.copyOf(vs, vs.length * 2)
        if sp ne null then sp = java.util.Arrays.copyOf(sp, sp.length * 2)
      vs(n) = v
      if sp ne null then sp(n) = Jfmt.span(v0.toInt, i.toInt)
      if allD && !v.isInstanceOf[Jnum.D] then allD = false
      n += 1
      c = wsWork()
      if c == ',' then
        i += 1
        c = wsWork()
      else if c == ']' then
        i += 1
        depth -= 1
        val node: Jarr =
          if asM then new Jarr.A.M(vs, n)   // editability first: no packing, keep the slack
          else if allD then   // pack, since element access, equality, and printing all come out identical
            val xs = new Array[Double](n)
            var k = 0
            while k < n do
              xs(k) = vs(k).asInstanceOf[Jnum.D].value
              k += 1
            new Jarr.D(xs, n)
          else new Jarr.A(if n == vs.length then vs else java.util.Arrays.copyOf(vs, n), n)
        if sp ne null then node.fmt = new Jfmt(src, Jfmt.span(p0.toInt, i.toInt), sp)
        return node
      else return fail("',' or ']' in array", i)
    null

  protected final def parseObj(): Json | Null =
    val p0 = i
    if depth >= Jparse.maxDepth then return failMsg(s"JSON nested more than ${Jparse.maxDepth} levels deep", p0)
    depth += 1
    i += 1
    var c = wsWork()
    if c == '}' then
      i += 1
      depth -= 1
      return
        if asM || fmtMode then
          val node = if asM then new Jobj.M() else new Jobj(new Array[String](0), new Array[Json](0), 0)
          if fmtMode then node.fmt = new Jfmt(src, Jfmt.span(p0.toInt, i.toInt), new Array[Long](0))
          node
        else Jobj.empty
    var ks = new Array[String](8)
    var vs = new Array[Json](8)
    var sp: Array[Long] = if fmtMode then new Array[Long](16) else null
    var n = 0
    while true do
      if c != '"' then return fail("'\"' to begin a key", i)
      val k0 = i
      val key = strWork()
      if key eq null then return explain(s"in key $n of object started at ${posText(p0)}:")
      val k1 = i
      c = wsWork()
      if c != ':' then return fail(s"':' after key \"$key\"", i)
      i += 1
      c = wsWork()
      val v0 = i
      val v = parseValue(c)
      if v eq null then return explain(s"in value for key \"$key\" of object started at ${posText(p0)}:")
      if n >= ks.length then
        ks = java.util.Arrays.copyOf(ks, ks.length * 2)
        vs = java.util.Arrays.copyOf(vs, vs.length * 2)
        if sp ne null then sp = java.util.Arrays.copyOf(sp, sp.length * 2)
      ks(n) = key
      vs(n) = v
      if sp ne null then
        sp(2 * n) = Jfmt.span(k0.toInt, k1.toInt)
        sp(2 * n + 1) = Jfmt.span(v0.toInt, i.toInt)
      n += 1
      c = wsWork()
      if c == ',' then
        i += 1
        c = wsWork()
      else if c == '}' then
        i += 1
        depth -= 1
        val node: Jobj =
          if asM then new Jobj.M(ks, vs, n)   // keep the slack for further edits
          else new Jobj(
            if n == ks.length then ks else java.util.Arrays.copyOf(ks, n),
            if n == vs.length then vs else java.util.Arrays.copyOf(vs, n),
            n
          )
        if sp ne null then node.fmt = new Jfmt(src, Jfmt.span(p0.toInt, i.toInt), sp)
        return node
      else return fail("',' or '}' in object", i)
    null

  /** Parse one complete JSON value spanning the whole input (bar whitespace). */
  final def parseTop(): Ask[Json] =
    val v = parseValue(wsWork())
    if v eq null then Alt(eErr)
    else if wsWork() >= 0 then Alt(errAt("unexpected content after the JSON value", i))
    else Is(v)


  ///////////////////////////////////////////
  /// Visitor traversal (no tree, SAX-style) ///
  ///////////////////////////////////////////

  /** Walk one complete JSON value, driving `vis`; builds nothing.  Values the visitor declines
    * (via a false `key`/`index`/`objStart`/`arrStart`) are scanned structurally, not decoded.
    */
  final def visitTop(vis: Jvisitor): Ask[Unit] =
    val c = wsWork()
    if c < 0 then Alt(errAt("expected a JSON value", i))
    else if !visitValue(c, vis) then Alt(eErr)
    else if wsWork() >= 0 then Alt(errAt("unexpected content after the JSON value", i))
    else Is(())

  private def visitValue(c: Int, vis: Jvisitor): Boolean =
    if c == '{' then visitObj(vis)
    else if c == '[' then visitArr(vis)
    else if c == '"' then
      val s = strWork()
      if s eq null then false else { vis.str(s); true }
    else if c == '-' || (c >= '0' && c <= '9') then
      val jn = numWork()
      if jn eq null then false else { vis.num(jn.asInstanceOf[Jnum]); true }
    else if c == 't' then { if litWork("true") then { vis.bool(true); true } else false }
    else if c == 'f' then { if litWork("false") then { vis.bool(false); true } else false }
    else if c == 'n' then { if litWork("null") then { vis.nul(); true } else false }
    else { fail("a JSON value", i); false }

  private def visitObj(vis: Jvisitor): Boolean =
    if !vis.objStart() then return skipWork()
    val p0 = i
    if depth >= Jparse.maxDepth then { failMsg(s"JSON nested more than ${Jparse.maxDepth} levels deep", p0); return false }
    depth += 1
    i += 1
    var c = wsWork()
    if c == '}' then { i += 1; depth -= 1; vis.objEnd(); return true }
    var n = 0
    while true do
      if c != '"' then return { fail("'\"' to begin a key", i); false }
      val key = strWork()
      if key eq null then return { explain(s"in key $n of object started at ${posText(p0)}:"); false }
      c = wsWork()
      if c != ':' then return { fail(s"':' after key \"$key\"", i); false }
      i += 1
      c = wsWork()
      val want = vis.key(key)
      val ok = if want then visitValue(c, vis) else skipWork()
      if !ok then
        return { if want then explain(s"in value for key \"$key\" of object started at ${posText(p0)}:") __ Unit; false }
      n += 1
      c = wsWork()
      if c == ',' then { i += 1; c = wsWork() }
      else if c == '}' then { i += 1; depth -= 1; vis.objEnd(); return true }
      else return { fail("',' or '}' in object", i); false }
    false

  private def visitArr(vis: Jvisitor): Boolean =
    if !vis.arrStart() then return skipWork()
    val p0 = i
    if depth >= Jparse.maxDepth then { failMsg(s"JSON nested more than ${Jparse.maxDepth} levels deep", p0); return false }
    depth += 1
    i += 1
    var c = wsWork()
    if c == ']' then { i += 1; depth -= 1; vis.arrEnd(); return true }
    var k = 0
    while true do
      val want = vis.index(k)
      val ok = if want then visitValue(c, vis) else skipWork()
      if !ok then
        return { if want then explain(s"in element $k of array started at ${posText(p0)}:") __ Unit; false }
      k += 1
      c = wsWork()
      if c == ',' then { i += 1; c = wsWork() }
      else if c == ']' then { i += 1; depth -= 1; vis.arrEnd(); return true }
      else return { fail("',' or ']' in array", i); false }
    false


  ////////////////////////////////
  /// Inline kernel templates  ///
  ////////////////////////////////

  /** Skip whitespace; answer the character code at the cursor afterwards, or -1 at the end.
    * `adv` is told each position as it is passed: windowed sources move the cursor along so a
    * long whitespace run does not force window growth; everyone else passes a no-op.
    */
  protected inline def wsImpl(inline at: Long => Int, inline adv: Long => Unit): Int =
    var j = i
    var c = -1
    boundary:
      while j < iZ do
        adv(j)
        val x = at(j)
        if x != ' ' && x != '\t' && x != '\n' && x != '\r' then
          c = x
          boundary.break()
        j += 1
    i = j
    c

  /** Match the rest of `lit` (its first character, at the cursor, already dispatched on). */
  protected inline def litImpl(inline at: Long => Int)(lit: String): Boolean =
    val n = lit.length
    var ok = iZ - i >= n
    var k = 1
    while ok && k < n do
      if at(i + k) != lit.charAt(k) then ok = false
      k += 1
    if ok then i += n
    else fail("'" + lit + "'", i) __ Unit
    ok

  /** Read the string whose opening quote is at the cursor.  The body is scanned for the
    * closing quote first: clean spans become one `sub` call, and only escaped ones pay for
    * decoding.  UTF-8 multi-byte units pass through untouched (they read as >= 0x80, never
    * matching quote, backslash, or a control code).
    */
  protected inline def strImpl(inline at: Long => Int, inline sub: (Long, Long) => String): String | Null =
    val q0 = i
    var j = i + 1
    var esc = false
    var bad = false
    boundary:
      while j < iZ do
        val c = at(j)
        if c == '"' then boundary.break()
        else if c == '\\' then
          esc = true
          j += 2
        else if c >= ' ' then j += 1
        else if c < 0 then boundary.break()   // a streaming source found its end here (and clamped iZ)
        else
          bad = true
          boundary.break()
    if bad then failMsg("raw control character in string (use an escape)", j)
    else if j >= iZ then failMsg("unterminated string", q0)
    else
      i = j + 1
      if esc then strEscWork(q0 + 1, j)
      else sub(q0 + 1, j)

  /** Decode the escape-bearing string body `[j0, jN)`: clean runs are copied with `sub`,
    * escapes are expanded by hand.  Escaped surrogate pairs arrive as two adjacent `\ u`
    * escapes and reassemble by simple adjacency.
    */
  protected inline def strEscImpl(inline at: Long => Int, inline sub: (Long, Long) => String)(j0: Long, jN: Long): String | Null =
    val sb = new java.lang.StringBuilder((jN - j0).toInt)
    var k0 = j0
    var k = j0
    var dead = false
    while !dead && k < jN do
      if at(k) != '\\' then k += 1
      else
        if k > k0 then sb.append(sub(k0, k)) __ Unit
        val c = at(k + 1)   // exists: the terminator scan never leaves a trailing backslash in the body
        k += 2
        c match
          case '"' | '\\' | '/' => sb.append(c.toChar) __ Unit
          case 'n' => sb.append('\n') __ Unit
          case 't' => sb.append('\t') __ Unit
          case 'b' => sb.append('\b') __ Unit
          case 'f' => sb.append('\f') __ Unit
          case 'r' => sb.append('\r') __ Unit
          case 'u' =>
            if jN - k < 4 then
              failMsg("truncated \\u escape in string", k - 2) __ Unit
              dead = true
            else
              var h = 0
              var m = 0
              while m >= 0 && m < 4 do
                var y = at(k + m) | 0x20
                y += (if y <= '9' then -'0' else 0) + (if y >= 'a' then -87 else 0)
                if y < 0 || y > 15 then m = -2
                else
                  h = (h << 4) | y
                  m += 1
              if m < 0 then
                failMsg("invalid hex digit in \\u escape", k) __ Unit
                dead = true
              else
                sb.append(h.toChar) __ Unit
                k += 4
          case _ =>
            failMsg(s"invalid escape '\\${c.toChar}' in string", k - 2) __ Unit
            dead = true
        k0 = k
    if dead then null
    else
      if k > k0 then sb.append(sub(k0, k)) __ Unit
      sb.toString

  /** Parse the number starting at the cursor (strict JSON grammar: `-? (0|[1-9]\d*) frac? exp?`).
    * Integers with at most 19 digits become `Jnum.L` by direct accumulation; everything else
    * goes through the Eisel-Lemire kernel on up to 19 significant digits, with the same
    * one-ulp agreement test on truncated mantissas and JDK fallback on the (rare) undecided
    * cases that Grok uses.
    */
  protected inline def numImpl(inline at: Long => Int, inline sub: (Long, Long) => String): Json | Null =
    val i0 = i
    var j = i
    var c = at(j)
    var neg = false
    if c == '-' then
      neg = true
      j += 1
      c = if j < iZ then at(j) else -1
    var dead = false
    var mant = 0L
    var nd = 0
    var droppedInt = 0
    var truncated = false
    if c == '0' then
      j += 1
      c = if j < iZ then at(j) else -1
      if c >= '0' && c <= '9' then
        failMsg("leading zero in number", i0) __ Unit
        dead = true
    else if c >= '1' && c <= '9' then
      while c >= '0' && c <= '9' && nd < 19 do
        mant = mant * 10 + (c - '0')
        nd += 1
        j += 1
        c = if j < iZ then at(j) else -1
      while c >= '0' && c <= '9' do   // significance exhausted: count dropped integer digits
        droppedInt += 1
        if c != '0' then truncated = true
        j += 1
        c = if j < iZ then at(j) else -1
    else
      fail("a digit", j) __ Unit
      dead = true
    var whole = true
    var fracScale = 0
    if !dead && c == '.' then
      whole = false
      j += 1
      c = if j < iZ then at(j) else -1
      if c < '0' || c > '9' then
        fail("a digit after the decimal point", j) __ Unit
        dead = true
      else
        if nd == 0 then
          while c == '0' do   // leading fraction zeros scale the value but are not significant
            fracScale += 1
            j += 1
            c = if j < iZ then at(j) else -1
        while c >= '0' && c <= '9' && nd < 19 do
          mant = mant * 10 + (c - '0')
          nd += 1
          fracScale += 1
          j += 1
          c = if j < iZ then at(j) else -1
        while c >= '0' && c <= '9' do   // dropped fraction digits: only roundability matters
          if c != '0' then truncated = true
          j += 1
          c = if j < iZ then at(j) else -1
    var e10 = 0
    if !dead && (c == 'e' || c == 'E') then
      whole = false
      j += 1
      c = if j < iZ then at(j) else -1
      if c == '+' || c == '-' then
        if c == '-' then e10 = -1
        j += 1
        c = if j < iZ then at(j) else -1
      if c < '0' || c > '9' then
        fail("a digit in the exponent", j) __ Unit
        dead = true
      else
        val esign = e10 < 0
        var ex = 0
        while c >= '0' && c <= '9' do
          if ex < 100000000 then ex = ex * 10 + (c - '0')
          j += 1
          c = if j < iZ then at(j) else -1
        e10 = if esign then -ex else ex
    if dead then null
    else
      i = j
      e10 += droppedInt - fracScale
      // A 19-digit accumulation may wrap into u64 space, where mant < 0 as a Long; Eisel-Lemire
      // reads u64, but the Long path needs true values (plus the one u64 that IS Long.MinValue)
      if whole && droppedInt == 0 && (mant >= 0 || (neg && mant == Long.MinValue)) then
        new Jnum.L(if neg then -mant else mant)
      else if mant == 0 then new Jnum.D(if neg then -0.0 else 0.0)
      else
        val v = EiselLemire.toDouble(ULong.wrap(mant), e10)
        val ok = if truncated then v == EiselLemire.toDouble(ULong.wrap(mant + 1), e10) else v == v
        val d = if ok then (if neg then -v else v) else java.lang.Double.parseDouble(sub(i0, j))
        if !exactNum then new Jnum.D(d)
        else
          val text = sub(i0, j)
          if Jnum.exactDouble(d, text) then new Jnum.D(d) else new Jnum.Big(text)

  /** Scan one value structurally, advancing the cursor past it, decoding nothing.  Strings are
    * respected (so brackets and quotes inside them do not miscount), escapes are stepped over,
    * and scalars run to the next structural delimiter.  Skipped regions are not fully validated
    * -- that is the point: a declined value is meant to be cheap to step over.  `adv` is told
    * each position as it is passed (see `wsImpl`), so windowed sources skip a huge declined
    * subtree in bounded memory.
    */
  protected inline def skipImpl(inline at: Long => Int, inline adv: Long => Unit): Boolean =
    val c0 = if i < iZ then at(i) else -1
    if c0 == '{' || c0 == '[' then
      var j = i
      var depth = 0
      var ok = true
      var go = true
      while go do
        adv(j)
        val c = if j < iZ then at(j) else -1
        if c < 0 then { ok = false; go = false }
        else if c == '"' then
          j += 1
          var s = true
          while s do
            adv(j)
            val d = if j < iZ then at(j) else -1
            if d < 0 then { s = false; ok = false; go = false }
            else if d == '\\' then j += 2
            else if d == '"' then { j += 1; s = false }
            else j += 1
        else if c == '{' || c == '[' then { depth += 1; j += 1 }
        else if c == '}' || c == ']' then
          depth -= 1
          j += 1
          if depth <= 0 then go = false
        else j += 1
      if ok then { i = j; true } else { failMsg("unterminated container", i); false }
    else if c0 == '"' then
      var j = i + 1
      var s = true
      var ok = true
      while s do
        adv(j)
        val d = if j < iZ then at(j) else -1
        if d < 0 then { s = false; ok = false }
        else if d == '\\' then j += 2
        else if d == '"' then { j += 1; s = false }
        else j += 1
      if ok then { i = j; true } else { failMsg("unterminated string", i); false }
    else if c0 < 0 then { fail("a JSON value", i); false }
    else
      var j = i
      var go = true
      while go do
        val c = if j < iZ then at(j) else -1
        if c < 0 || c == ',' || c == '}' || c == ']' || c == ' ' || c == '\t' || c == '\n' || c == '\r' then go = false
        else j += 1
      i = j
      true
}
object Jparse {

  /** Refuse to nest deeper than this, so adversarial input cannot blow the JVM stack. */
  val maxDepth = 512

  /** Default sliding-window size (in elements) for streaming parses; windows double whenever
    * one token outgrows them, so this only sets where they start.
    */
  val defaultWindow = 8192

  /** Parses JSON from a `String`.  Create one per parse. */
  final class Str(content: String, exact: Boolean = false, mutable: Boolean = false, fmt: Boolean = false) extends Jparse {
    iZ = content.length
    exactNum = exact
    asM = mutable
    if fmt then
      fmtMode = true
      src = Jsrc(content)

    protected def rawLength: Long = content.length
    protected def rawCharAt(pos: Long): Char = content.charAt(pos.toInt)

    protected def wsWork(): Int = wsImpl(j => content.charAt(j.toInt), _ => ())
    protected def litWork(lit: String): Boolean = litImpl(j => content.charAt(j.toInt))(lit)
    protected def strWork(): String | Null = strImpl(j => content.charAt(j.toInt), (a, b) => content.substring(a.toInt, b.toInt))
    protected def strEscWork(j0: Long, jN: Long): String | Null =
      strEscImpl(j => content.charAt(j.toInt), (a, b) => content.substring(a.toInt, b.toInt))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => content.charAt(j.toInt), (a, b) => content.substring(a.toInt, b.toInt))
    protected def skipWork(): Boolean = skipImpl(j => content.charAt(j.toInt), _ => ())
  }

  /** Parses JSON from an `Array[Char]`.  Create one per parse. */
  final class Chars(content: Array[Char], exact: Boolean = false, mutable: Boolean = false, fmt: Boolean = false) extends Jparse {
    iZ = content.length
    exactNum = exact
    asM = mutable
    if fmt then
      fmtMode = true
      src = Jsrc(content)

    protected def rawLength: Long = content.length
    protected def rawCharAt(pos: Long): Char = content(pos.toInt)

    private def substr(a: Long, b: Long): String = new String(content, a.toInt, (b - a).toInt)

    protected def wsWork(): Int = wsImpl(j => content(j.toInt), _ => ())
    protected def litWork(lit: String): Boolean = litImpl(j => content(j.toInt))(lit)
    protected def strWork(): String | Null = strImpl(j => content(j.toInt), (a, b) => substr(a, b))
    protected def strEscWork(j0: Long, jN: Long): String | Null =
      strEscImpl(j => content(j.toInt), (a, b) => substr(a, b))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => content(j.toInt), (a, b) => substr(a, b))
    protected def skipWork(): Boolean = skipImpl(j => content(j.toInt), _ => ())
  }

  /** Parses JSON from raw bytes: structure (whitespace, literals, numbers) is ASCII, read as
    * unsigned 0-255, and strings decode their spans as UTF-8 (safe because multi-byte
    * sequences never contain ASCII bytes).  Error positions are byte positions.  Create one
    * per parse.
    */
  final class Bytes(content: Array[Byte], exact: Boolean = false, mutable: Boolean = false, fmt: Boolean = false) extends Jparse {
    iZ = content.length
    exactNum = exact
    asM = mutable
    if fmt then
      fmtMode = true
      src = Jsrc(content)

    protected def rawLength: Long = content.length
    protected def rawCharAt(pos: Long): Char = (content(pos.toInt) & 0xFF).toChar

    private def utf8(a: Long, b: Long): String = new String(content, a.toInt, (b - a).toInt, java.nio.charset.StandardCharsets.UTF_8)

    protected def wsWork(): Int = wsImpl(j => content(j.toInt) & 0xFF, _ => ())
    protected def litWork(lit: String): Boolean = litImpl(j => content(j.toInt) & 0xFF)(lit)
    protected def strWork(): String | Null = strImpl(j => content(j.toInt) & 0xFF, (a, b) => utf8(a, b))
    protected def strEscWork(j0: Long, jN: Long): String | Null =
      strEscImpl(j => content(j.toInt) & 0xFF, (a, b) => utf8(a, b))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => content(j.toInt) & 0xFF, (a, b) => utf8(a, b))
    protected def skipWork(): Boolean = skipImpl(j => content(j.toInt) & 0xFF, _ => ())
  }

  /** Parses JSON straight from an off-heap `Mem[Byte]` (UTF-8, ASCII structure) with no copy --
    * the segment is read element by element.  Same rules as `Bytes`; error positions are byte
    * positions.  The segment may exceed 2 GiB (the cursor is a Long), though any single token
    * must still fit in a `String`.  Create one per parse.
    */
  final class MemBytes(content: Mem[Byte], exact: Boolean = false, mutable: Boolean = false) extends Jparse {
    iZ = content.length
    exactNum = exact
    asM = mutable

    protected def rawLength: Long = iZ
    protected def rawCharAt(pos: Long): Char = (content(pos) & 0xFF).toChar

    private def utf8(a: Long, b: Long): String =
      val arr = new Array[Byte]((b - a).toInt)
      content.inject(arr, 0)(a, b) __ Unit
      new String(arr, java.nio.charset.StandardCharsets.UTF_8)

    protected def wsWork(): Int = wsImpl(j => content(j) & 0xFF, _ => ())
    protected def litWork(lit: String): Boolean = litImpl(j => content(j) & 0xFF)(lit)
    protected def strWork(): String | Null = strImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b))
    protected def strEscWork(j0: Long, jN: Long): String | Null =
      strEscImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b))
    protected def skipWork(): Boolean = skipImpl(j => content(j) & 0xFF, _ => ())
  }

  /** Parses JSON straight from an off-heap `Mem[Char]` (UTF-16) with no copy.  Same rules as
    * `Chars`; may exceed `Int.MaxValue` chars, though any single token must still fit in a
    * `String`.  Create one per parse.
    */
  final class MemChars(content: Mem[Char], exact: Boolean = false, mutable: Boolean = false) extends Jparse {
    iZ = content.length
    exactNum = exact
    asM = mutable

    protected def rawLength: Long = iZ
    protected def rawCharAt(pos: Long): Char = content(pos)

    private def substr(a: Long, b: Long): String =
      val arr = new Array[Char]((b - a).toInt)
      content.inject(arr, 0)(a, b) __ Unit
      new String(arr)

    protected def wsWork(): Int = wsImpl(j => content(j).toInt, _ => ())
    protected def litWork(lit: String): Boolean = litImpl(j => content(j).toInt)(lit)
    protected def strWork(): String | Null = strImpl(j => content(j).toInt, (a, b) => substr(a, b))
    protected def strEscWork(j0: Long, jN: Long): String | Null =
      strEscImpl(j => content(j).toInt, (a, b) => substr(a, b))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => content(j).toInt, (a, b) => substr(a, b))
    protected def skipWork(): Boolean = skipImpl(j => content(j).toInt, _ => ())
  }

  /** Parses JSON from bytes pulled on demand through a sliding window (ASCII structure, UTF-8
    * strings, like `Bytes`), so arbitrarily large inputs parse in bounded memory: the window
    * keeps only the token in progress plus unconsumed readahead, slides forward as the parse
    * advances, and doubles only when a single token outgrows it.  `fill(dst, off, max)` answers
    * how many bytes it wrote, or a negative number once the input has ended (0 stops the current
    * refill and is asked again later, but a fill that cannot progress at all throws).
    *
    * The input is not retained, so there is no `exact` or format-preserving mode.  Newlines are
    * counted as they slide out of the window, so error positions keep exact line/col; excerpts
    * degrade to `?` behind the window, and container-start positions that have slid away are
    * reported as absolute offsets.  Create one per parse.
    */
  final class Buffered(fill: (Array[Byte], Int, Int) => Int, mutable: Boolean = false, window: Int = Jparse.defaultWindow) extends Jparse {
    asM = mutable
    iZ = Long.MaxValue

    private var buf = new Array[Byte](if window < 16 then 16 else window)
    private var discard = 0L    // absolute position of buf(0)
    private var loaded = 0      // bytes of buf currently valid
    private var srcEnd = Long.MaxValue   // where input truly ends; discovered when fill answers negative
    private var nlBehind = 0L   // newlines that have slid out of the window...
    private var lastNlBehind = -1L   // ...and where the last of them sat, so line/col stay exact
    private var memb: Mem[Byte] = null.asInstanceOf[Mem[Byte]]   // wraps buf for SWAR newline seeks; remade when buf regrows
    private var membOf: Array[Byte] = null
    refill()

    protected def rawLength: Long = if srcEnd == Long.MaxValue then discard + loaded else srcEnd
    protected def rawCharAt(pos: Long): Char =
      val d = pos - discard
      if 0 <= d && d < loaded then (buf(d.toInt) & 0xFF).toChar else '?'   // cold path: excerpts degrade outside the window

    private def refill(): Unit =
      var more = discard + loaded < srcEnd
      while more && loaded < buf.length do
        val n = fill(buf, loaded, buf.length - loaded)
        if n > 0 then loaded += n
        else
          if n < 0 then
            srcEnd = discard + loaded
            if iZ > srcEnd then iZ = srcEnd
          more = false

    // SWAR-count the newlines in buf [k0, kN) before they slide away
    private def nlSeek(k0: Int, kN: Int): Unit =
      if membOf ne buf then
        memb = Mem of buf
        membOf = buf
      var k = k0.toLong
      var going = true
      while going do
        val h = memb.whereIsFwd(k, kN.toLong)('\n'.toByte)
        if h >= 0 then
          nlBehind += 1
          lastNlBehind = discard + h
          k = h + 1
        else going = false

    // Off the hot path: drop everything behind the current op's start (which is all the
    // templates ever re-read), grow until the needed position fits -- the drop alone may not
    // make room, since a token can outgrow the window while its start pins retention -- and
    // refill.  The cursor and the needed position can sit past the loaded frontier (a skipped
    // escape steps by 2), so the retention clamp and need-based sizing, not a fullness check.
    private def scoot(need: Long): Unit =
      var keep = (i - discard).toInt
      if keep > loaded then keep = loaded
      if keep < 0 then throw new IllegalStateException("jsaun Buffered: backtrack to " + i + " behind the window at " + discard)
      if keep > 0 then
        nlSeek(0, keep)
        System.arraycopy(buf, keep, buf, 0, loaded - keep)
        discard += keep
        loaded -= keep
      while need - discard >= buf.length do buf = java.util.Arrays.copyOf(buf, buf.length << 1)
      refill()

    private def fetched(j: Long): Int =
      if j >= srcEnd then
        if iZ > srcEnd then iZ = srcEnd
        -1
      else
        scoot(j)
        val d = j - discard
        if 0 <= d && d < loaded then buf(d.toInt) & 0xFF
        else if j >= srcEnd then
          if iZ > srcEnd then iZ = srcEnd
          -1
        else throw new IllegalStateException("jsaun Buffered: cannot load position " + j + " (window at " + discard + ", " + loaded + " loaded)")

    private inline def atc(j: Long): Int =
      val d = j - discard
      if 0 <= d && d < loaded then buf(d.toInt) & 0xFF
      else fetched(j)

    private def utf8(a: Long, b: Long): String =
      new String(buf, (a - discard).toInt, (b - a).toInt, java.nio.charset.StandardCharsets.UTF_8)

    protected override def lineColOf(pos: Long): Long =
      // newlines behind the window were counted as they slid past, so this stays exact
      val n = rawLength
      val q = if pos > n then n else if pos < discard then discard else pos
      var line = 1L + nlBehind
      var last = lastNlBehind
      var j = discard
      while j < q do
        if buf((j - discard).toInt) == '\n' then
          line += 1
          last = j
        j += 1
      val hi = if line > Int.MaxValue then Int.MaxValue.toLong else line
      val lo = { val c = q - last; if c > Int.MaxValue then Int.MaxValue.toLong else c }
      (hi << 32) | lo

    protected override def posText(pos: Long): String =
      if pos >= discard then super.posText(pos)
      else "char " + (pos + 1) + " of the input"   // slid out of the window; only the offset is still known

    protected def wsWork(): Int = wsImpl(j => atc(j), j => i = j)
    protected def litWork(lit: String): Boolean = litImpl(j => atc(j))(lit)
    protected def strWork(): String | Null = strImpl(j => atc(j), (a, b) => utf8(a, b))
    protected def strEscWork(j0: Long, jN: Long): String | Null =
      strEscImpl(j => atc(j), (a, b) => utf8(a, b))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => atc(j), (a, b) => utf8(a, b))
    protected def skipWork(): Boolean = skipImpl(j => atc(j), j => i = j)
  }
  object Buffered {
    /** Streams UTF-8 JSON from `in`, reading as needed but never closing it. */
    def apply(in: java.io.InputStream, mutable: Boolean, window: Int): Buffered =
      new Buffered((dst, off, max) => in.read(dst, off, max), mutable, window)

    /** Streams UTF-8 JSON arriving one chunk at a time: chunk boundaries are invisible to the
      * parse (tokens may straddle them) and empty chunks are fine.
      */
    def apply(chunks: Iterator[Array[Byte]], mutable: Boolean, window: Int): Buffered =
      new Buffered(chunkFill(chunks), mutable, window)

    private def chunkFill(chunks: Iterator[Array[Byte]]): (Array[Byte], Int, Int) => Int =
      var cur: Array[Byte] = new Array[Byte](0)
      var pos = 0
      var done = false
      (dst, off, max) =>
        var n = 0
        while n == 0 && !done do
          if pos >= cur.length then
            if chunks.hasNext then
              cur = chunks.next()
              pos = 0
            else done = true
          else
            n = cur.length - pos
            if n > max then n = max
            System.arraycopy(cur, pos, dst, off, n)
            pos += n
        if n == 0 then -1 else n
  }

  /** Parses JSON from chars pulled on demand through a sliding window; the char-flavored twin
    * of `Buffered`, with the same retention discipline, end-of-input discovery, and error
    * behavior.  Create one per parse.
    */
  final class BufferedChars(fill: (Array[Char], Int, Int) => Int, mutable: Boolean = false, window: Int = Jparse.defaultWindow) extends Jparse {
    asM = mutable
    iZ = Long.MaxValue

    private var buf = new Array[Char](if window < 16 then 16 else window)
    private var discard = 0L
    private var loaded = 0
    private var srcEnd = Long.MaxValue
    private var nlBehind = 0L
    private var lastNlBehind = -1L
    private var memb: Mem[Char] = null.asInstanceOf[Mem[Char]]
    private var membOf: Array[Char] = null
    refill()

    protected def rawLength: Long = if srcEnd == Long.MaxValue then discard + loaded else srcEnd
    protected def rawCharAt(pos: Long): Char =
      val d = pos - discard
      if 0 <= d && d < loaded then buf(d.toInt) else '?'

    private def refill(): Unit =
      var more = discard + loaded < srcEnd
      while more && loaded < buf.length do
        val n = fill(buf, loaded, buf.length - loaded)
        if n > 0 then loaded += n
        else
          if n < 0 then
            srcEnd = discard + loaded
            if iZ > srcEnd then iZ = srcEnd
          more = false

    private def nlSeek(k0: Int, kN: Int): Unit =
      if membOf ne buf then
        memb = Mem of buf
        membOf = buf
      var k = k0.toLong
      var going = true
      while going do
        val h = memb.whereIsFwd(k, kN.toLong)('\n')
        if h >= 0 then
          nlBehind += 1
          lastNlBehind = discard + h
          k = h + 1
        else going = false

    private def scoot(need: Long): Unit =   // drop, size for `need`, refill (see Buffered)
      var keep = (i - discard).toInt
      if keep > loaded then keep = loaded
      if keep < 0 then throw new IllegalStateException("jsaun BufferedChars: backtrack to " + i + " behind the window at " + discard)
      if keep > 0 then
        nlSeek(0, keep)
        System.arraycopy(buf, keep, buf, 0, loaded - keep)
        discard += keep
        loaded -= keep
      while need - discard >= buf.length do buf = java.util.Arrays.copyOf(buf, buf.length << 1)
      refill()

    private def fetched(j: Long): Int =
      if j >= srcEnd then
        if iZ > srcEnd then iZ = srcEnd
        -1
      else
        scoot(j)
        val d = j - discard
        if 0 <= d && d < loaded then buf(d.toInt)
        else if j >= srcEnd then
          if iZ > srcEnd then iZ = srcEnd
          -1
        else throw new IllegalStateException("jsaun BufferedChars: cannot load position " + j + " (window at " + discard + ", " + loaded + " loaded)")

    private inline def atc(j: Long): Int =
      val d = j - discard
      if 0 <= d && d < loaded then buf(d.toInt) else fetched(j)

    private def substr(a: Long, b: Long): String = new String(buf, (a - discard).toInt, (b - a).toInt)

    protected override def lineColOf(pos: Long): Long =
      val n = rawLength
      val q = if pos > n then n else if pos < discard then discard else pos
      var line = 1L + nlBehind
      var last = lastNlBehind
      var j = discard
      while j < q do
        if buf((j - discard).toInt) == '\n' then
          line += 1
          last = j
        j += 1
      val hi = if line > Int.MaxValue then Int.MaxValue.toLong else line
      val lo = { val c = q - last; if c > Int.MaxValue then Int.MaxValue.toLong else c }
      (hi << 32) | lo

    protected override def posText(pos: Long): String =
      if pos >= discard then super.posText(pos)
      else "char " + (pos + 1) + " of the input"

    protected def wsWork(): Int = wsImpl(j => atc(j), j => i = j)
    protected def litWork(lit: String): Boolean = litImpl(j => atc(j))(lit)
    protected def strWork(): String | Null = strImpl(j => atc(j), (a, b) => substr(a, b))
    protected def strEscWork(j0: Long, jN: Long): String | Null =
      strEscImpl(j => atc(j), (a, b) => substr(a, b))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => atc(j), (a, b) => substr(a, b))
    protected def skipWork(): Boolean = skipImpl(j => atc(j), j => i = j)
  }
  object BufferedChars {
    /** Streams JSON text from `rd`, reading as needed but never closing it. */
    def apply(rd: java.io.Reader, mutable: Boolean, window: Int): BufferedChars =
      new BufferedChars((dst, off, max) => rd.read(dst, off, max), mutable, window)

    /** Streams JSON text arriving one char-array chunk at a time; boundaries are invisible. */
    def apply(chunks: Iterator[Array[Char]], mutable: Boolean, window: Int): BufferedChars =
      new BufferedChars(chunkFill(chunks), mutable, window)

    private def chunkFill(chunks: Iterator[Array[Char]]): (Array[Char], Int, Int) => Int =
      var cur: Array[Char] = new Array[Char](0)
      var pos = 0
      var done = false
      (dst, off, max) =>
        var n = 0
        while n == 0 && !done do
          if pos >= cur.length then
            if chunks.hasNext then
              cur = chunks.next()
              pos = 0
            else done = true
          else
            n = cur.length - pos
            if n > max then n = max
            System.arraycopy(cur, pos, dst, off, n)
            pos += n
        if n == 0 then -1 else n
  }

  /** Parses JSON supplied line by line, retaining only the current line: consecutive lines are
    * separated by an implied newline, and since JSON strings cannot contain raw newlines and an
    * (implied) newline terminates any number or literal, no token can span lines -- so the hot
    * template loops never leave the line, and only whitespace skipping and structural skipping
    * (which have line-aware implementations here) ever cross a boundary.
    *
    * Positions pack (line index, offset within line) into the halves of the Long cursor, which
    * both orders correctly and makes error line/col exact at any input size with nothing
    * retained.  At most `Int.MaxValue` lines (the packed position must stay non-negative for
    * the shared signed cursor comparisons).  No `exact` or format-preserving mode.  Create one
    * per parse.
    */
  final class Lines(lines: Iterator[String], mutable: Boolean = false) extends Jparse {
    asM = mutable
    iZ = Long.MaxValue

    private var cur = ""       // the one retained line
    private var curL = -1      // its index: the high half of every live position
    private var ended = false
    if !advance() then curL = 0

    private def advance(): Boolean =
      if ended then false
      else if lines.hasNext then
        if curL == Int.MaxValue then throw new IllegalStateException("jsaun Lines: more than Int.MaxValue lines")
        curL += 1
        cur = lines.next()
        true
      else
        ended = true
        iZ = if curL < 0 then 0L else (curL.toLong << 32) | cur.length.toLong
        false

    // Reads at or past the end of the line answer the implied newline, which correctly
    // terminates numbers and literals and correctly fails a still-open string.
    private inline def atc(j: Long): Int =
      if (j >>> 32).toInt == curL then
        val k = (j & 0xFFFFFFFFL).toInt
        if k < cur.length then cur.charAt(k).toInt else '\n'.toInt
      else '\n'.toInt

    private inline def subc(a: Long, b: Long): String =
      cur.substring((a & 0xFFFFFFFFL).toInt, (b & 0xFFFFFFFFL).toInt)

    protected def rawLength: Long = if ended then iZ else Long.MaxValue
    protected def rawCharAt(pos: Long): Char =
      if (pos >>> 32).toInt == curL then
        val k = (pos & 0xFFFFFFFFL).toInt
        if k < cur.length then cur.charAt(k) else '\n'
      else '?'

    protected override def lineColOf(pos: Long): Long =
      (((pos >>> 32) + 1L) << 32) | (((pos & 0xFFFFFFFFL) + 1L) & 0xFFFFFFFFL)

    protected override def errAt(desc: String, pos: Long): Err =
      val L = (pos >>> 32).toInt
      val q = (pos & 0xFFFFFFFFL).toInt
      val sb = new java.lang.StringBuilder
      var off = 0
      if L == curL then   // an earlier line is no longer held; its error keeps line/col but no excerpt
        var x0 = q - 8
        if x0 < 0 then x0 = 0
        var x1 = q + 8
        if x1 > cur.length then x1 = cur.length
        if x0 > 0 then sb.append("...") __ Unit
        off = sb.length + (q - x0)
        var k = x0
        while k < x1 do
          val c = cur.charAt(k)
          sb.append(if c < ' ' then '·' else c) __ Unit
          k += 1
        if x1 < cur.length then sb.append("...") __ Unit
      Err(new Jerr(desc, pos, L + 1, q + 1, sb.toString, off))

    protected def wsWork(): Int =
      var k = (i & 0xFFFFFFFFL).toInt
      var c = -1
      boundary:
        while true do
          while k < cur.length do
            val x = cur.charAt(k)
            if x != ' ' && x != '\t' && x != '\n' && x != '\r' then
              c = x.toInt
              boundary.break()
            k += 1
          if !advance() then boundary.break()
          k = 0
      i = if c < 0 then iZ else (curL.toLong << 32) | k.toLong
      c

    protected def litWork(lit: String): Boolean = litImpl(j => atc(j))(lit)
    protected def strWork(): String | Null = strImpl(j => atc(j), (a, b) => subc(a, b))
    protected def strEscWork(j0: Long, jN: Long): String | Null =
      strEscImpl(j => atc(j), (a, b) => subc(a, b))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => atc(j), (a, b) => subc(a, b))

    protected def skipWork(): Boolean =
      var k = (i & 0xFFFFFFFFL).toInt
      val c0 = if k < cur.length then cur.charAt(k).toInt else -1
      if c0 == '{' || c0 == '[' then
        var depth = 0
        var inStr = false
        var ok = true
        var go = true
        while go do
          if k >= cur.length then
            if advance() then k = 0
            else { ok = false; go = false }
          else
            val c = cur.charAt(k)
            if inStr then
              if c == '\\' then k += 2
              else
                if c == '"' then inStr = false
                k += 1
            else if c == '"' then { inStr = true; k += 1 }
            else if c == '{' || c == '[' then { depth += 1; k += 1 }
            else if c == '}' || c == ']' then
              depth -= 1
              k += 1
              if depth <= 0 then go = false
            else k += 1
        if ok then { i = (curL.toLong << 32) | k.toLong; true }
        else { failMsg("unterminated container", iZ); false }
      else if c0 == '"' then
        var k2 = k + 1
        var s = true
        var ok = true
        while s do
          if k2 >= cur.length then   // like the in-memory skip, a skipped string is scanned leniently across lines
            if advance() then k2 = 0
            else { s = false; ok = false }
          else
            val d = cur.charAt(k2)
            if d == '\\' then k2 += 2
            else if d == '"' then { k2 += 1; s = false }
            else k2 += 1
        if ok then { i = (curL.toLong << 32) | k2.toLong; true }
        else { failMsg("unterminated string", i); false }
      else if c0 < 0 then { fail("a JSON value", i); false }
      else
        var k3 = k
        var go = true
        while go && k3 < cur.length do
          val c = cur.charAt(k3)
          if c == ',' || c == '}' || c == ']' || c == ' ' || c == '\t' || c == '\n' || c == '\r' then go = false
          else k3 += 1
        i = (curL.toLong << 32) | k3.toLong
        true
  }

  /** Compile-time source selection for streaming input: bytes (`InputStream`, byte chunks)
    * parse as UTF-8, chars (`Reader`, char chunks) as text.
    */
  inline def chunkedParser(inline in: Json.Chunked, mutable: Boolean, window: Int): Jparse =
    inline in match
      case s: java.io.InputStream   => Buffered(s, mutable, window)
      case b: Iterator[Array[Byte]] => Buffered(b, mutable, window)
      case r: java.io.Reader        => BufferedChars(r, mutable, window)
      case c: Iterator[Array[Char]] => BufferedChars(c, mutable, window)

  /** Compile-time source selection for line-fed input. */
  inline def linedParser(inline in: Json.Lined, mutable: Boolean): Jparse =
    inline in match
      case it: Iterator[String] => new Lines(it, mutable)
      case ib: Iterable[String] => new Lines(ib.iterator, mutable)

  // A format-preserving parse retains the whole source, and off-heap memory is caller-owned
  // (it may be freed or reused after the call), so fmt mode snapshots the Mem into a heap array
  // and parses that; plain/exact/mutable parses read the segment directly with no copy.
  private def ofMemBytes(m: Mem[Byte], exact: Boolean, mutable: Boolean, fmt: Boolean): Jparse =
    if fmt then
      val arr = new Array[Byte](m.length.toInt)
      m.inject(arr) __ Unit
      new Bytes(arr, exact, mutable, fmt = true)
    else new MemBytes(m, exact, mutable)

  private def ofMemChars(m: Mem[Char], exact: Boolean, mutable: Boolean, fmt: Boolean): Jparse =
    if fmt then
      val arr = new Array[Char](m.length.toInt)
      m.inject(arr) __ Unit
      new Chars(arr, exact, mutable, fmt = true)
    else new MemChars(m, exact, mutable)

  /** Compile-time source selection for a `Mem[A]`: only `Mem[Byte]` (UTF-8) and `Mem[Char]`
    * (UTF-16) name a textual encoding, so anything else is rejected at compile time.
    */
  inline def memParser[A <: Mem.Type](inline m: Mem[A], exact: Boolean, mutable: Boolean, fmt: Boolean): Jparse =
    inline erasedValue[A] match
      case _: Byte => ofMemBytes(m.asInstanceOf[Mem[Byte]], exact, mutable, fmt)
      case _: Char => ofMemChars(m.asInstanceOf[Mem[Char]], exact, mutable, fmt)
      case _       => error("jsaun can parse only Mem[Byte] (UTF-8) or Mem[Char] (UTF-16)")
}
