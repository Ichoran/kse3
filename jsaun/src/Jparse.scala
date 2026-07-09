// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


import scala.util.boundary

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
  * Failure is one `eErr` field set at the point of error and wrapped with structural context
  * (`explainBy`) as the descent unwinds, so errors arrive fully unrolled: what broke, where,
  * and inside which elements of which containers.  Only the public rim (`parseTop`) speaks
  * `Ask`.  A `Jparse` is mutable state for exactly one parse; do not reuse or share one.
  */
sealed abstract class Jparse protected () {

  protected var i = 0
  protected var iZ = 0
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
  protected def strEscWork(j0: Int, jN: Int): String | Null
  protected def numWork(): Json | Null

  // === Cold access to raw content, for error reporting only ===

  protected def rawLength: Int
  protected def rawCharAt(pos: Int): Char


  //////////////////////////////
  /// Error construction     ///
  //////////////////////////////

  private def lineColOf(pos: Int): Long =
    val n = rawLength
    val q = if pos > n then n else if pos < 0 then 0 else pos
    var line = 1
    var last = -1
    var j = 0
    while j < q do
      if rawCharAt(j) == '\n' then
        line += 1
        last = j
      j += 1
    (line.toLong << 32) | (q - last).toLong

  private def foundAt(pos: Int): String =
    if pos >= rawLength then "end of input"
    else rawCharAt(pos) match
      case '\n' | '\r' => "end of line"
      case '\t' => "tab"
      case c if c < ' ' => "control character " + c.toInt
      case c => "'" + c + "'"

  protected final def posText(pos: Int): String =
    val lc = lineColOf(pos)
    s"line ${(lc >>> 32).toInt}, char ${(lc & 0xFFFFFFFFL).toInt}"

  /** Build a `Jerr` at `pos` with a caret-marked excerpt of the input there. */
  protected final def errAt(desc: String, pos: Int): Err =
    val n = rawLength
    val q = if pos > n then n else if pos < 0 then 0 else pos
    val lc = lineColOf(q)
    var x0 = q - 8
    if x0 < 0 then x0 = 0
    var x1 = q + 8
    if x1 > n then x1 = n
    val sb = new java.lang.StringBuilder
    if x0 > 0 then sb.append("...") __ Unit
    val off = sb.length + (q - x0)
    var j = x0
    while j < x1 do
      val c = rawCharAt(j)
      sb.append(if c < ' ' then '·' else c) __ Unit
      j += 1
    if x1 < n then sb.append("...") __ Unit
    Err(new Jerr(desc, q, (lc >>> 32).toInt, (lc & 0xFFFFFFFFL).toInt, sb.toString, off))

  protected final def fail(wanted: String, pos: Int): Null =
    eErr = errAt(s"expected $wanted, found ${foundAt(pos)}", pos)
    null

  protected final def failMsg(msg: String, pos: Int): Null =
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
          if fmtMode then node.fmt = new Jfmt(src, Jfmt.span(p0, i), new Array[Long](0))
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
      if sp ne null then sp(n) = Jfmt.span(v0, i)
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
        if sp ne null then node.fmt = new Jfmt(src, Jfmt.span(p0, i), sp)
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
          if fmtMode then node.fmt = new Jfmt(src, Jfmt.span(p0, i), new Array[Long](0))
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
        sp(2 * n) = Jfmt.span(k0, k1)
        sp(2 * n + 1) = Jfmt.span(v0, i)
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
        if sp ne null then node.fmt = new Jfmt(src, Jfmt.span(p0, i), sp)
        return node
      else return fail("',' or '}' in object", i)
    null

  /** Parse one complete JSON value spanning the whole input (bar whitespace). */
  final def parseTop(): Ask[Json] =
    val v = parseValue(wsWork())
    if v eq null then Alt(eErr)
    else if wsWork() >= 0 then Alt(errAt("unexpected content after the JSON value", i))
    else Is(v)


  ////////////////////////////////
  /// Inline kernel templates  ///
  ////////////////////////////////

  /** Skip whitespace; answer the character code at the cursor afterwards, or -1 at the end. */
  protected inline def wsImpl(inline at: Int => Int): Int =
    var j = i
    var c = -1
    boundary:
      while j < iZ do
        val x = at(j)
        if x != ' ' && x != '\t' && x != '\n' && x != '\r' then
          c = x
          boundary.break()
        j += 1
    i = j
    c

  /** Match the rest of `lit` (its first character, at the cursor, already dispatched on). */
  protected inline def litImpl(inline at: Int => Int)(lit: String): Boolean =
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
  protected inline def strImpl(inline at: Int => Int, inline sub: (Int, Int) => String): String | Null =
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
  protected inline def strEscImpl(inline at: Int => Int, inline sub: (Int, Int) => String)(j0: Int, jN: Int): String | Null =
    val sb = new java.lang.StringBuilder(jN - j0)
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
  protected inline def numImpl(inline at: Int => Int, inline sub: (Int, Int) => String): Json | Null =
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
}
object Jparse {

  /** Refuse to nest deeper than this, so adversarial input cannot blow the JVM stack. */
  val maxDepth = 512

  /** Parses JSON from a `String`.  Create one per parse. */
  final class Str(content: String, exact: Boolean = false, mutable: Boolean = false, fmt: Boolean = false) extends Jparse {
    iZ = content.length
    exactNum = exact
    asM = mutable
    if fmt then
      fmtMode = true
      src = Jsrc(content)

    protected def rawLength: Int = content.length
    protected def rawCharAt(pos: Int): Char = content.charAt(pos)

    protected def wsWork(): Int = wsImpl(j => content.charAt(j))
    protected def litWork(lit: String): Boolean = litImpl(j => content.charAt(j))(lit)
    protected def strWork(): String | Null = strImpl(j => content.charAt(j), (a, b) => content.substring(a, b))
    protected def strEscWork(j0: Int, jN: Int): String | Null =
      strEscImpl(j => content.charAt(j), (a, b) => content.substring(a, b))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => content.charAt(j), (a, b) => content.substring(a, b))
  }

  /** Parses JSON from an `Array[Char]`.  Create one per parse. */
  final class Chars(content: Array[Char], exact: Boolean = false, mutable: Boolean = false, fmt: Boolean = false) extends Jparse {
    iZ = content.length
    exactNum = exact
    asM = mutable
    if fmt then
      fmtMode = true
      src = Jsrc(content)

    protected def rawLength: Int = content.length
    protected def rawCharAt(pos: Int): Char = content(pos)

    protected def wsWork(): Int = wsImpl(j => content(j))
    protected def litWork(lit: String): Boolean = litImpl(j => content(j))(lit)
    protected def strWork(): String | Null = strImpl(j => content(j), (a, b) => new String(content, a, b - a))
    protected def strEscWork(j0: Int, jN: Int): String | Null =
      strEscImpl(j => content(j), (a, b) => new String(content, a, b - a))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => content(j), (a, b) => new String(content, a, b - a))
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

    protected def rawLength: Int = content.length
    protected def rawCharAt(pos: Int): Char = (content(pos) & 0xFF).toChar

    private def utf8(a: Int, b: Int): String = new String(content, a, b - a, java.nio.charset.StandardCharsets.UTF_8)

    protected def wsWork(): Int = wsImpl(j => content(j) & 0xFF)
    protected def litWork(lit: String): Boolean = litImpl(j => content(j) & 0xFF)(lit)
    protected def strWork(): String | Null = strImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b))
    protected def strEscWork(j0: Int, jN: Int): String | Null =
      strEscImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b))(j0, jN)
    protected def numWork(): Json | Null = numImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b))
  }
}
