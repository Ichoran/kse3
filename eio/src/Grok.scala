// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2024, 2026 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio


import scala.annotation.publicInBinary
import scala.util.boundary
import scala.util.boundary.Label

import kse.basics.{given, _}
import kse.flow.{given, _}


//////////////////////////
/// Delimiter machinery ///
//////////////////////////

/** A set of token-delimiting characters, stored as a 128-bit ASCII bitmask plus a flag that
  * covers every non-ASCII character.  Membership testing is therefore two or three instructions
  * with no virtual calls, which matters because it runs once per input character.
  *
  * Every `Delim` also knows which delimiter applies to a sub-parse within one of its own tokens,
  * so tokenization can progress naturally, e.g. from lines to words to nothing (`Delim.lines.sub`
  * is `Delim.white`, whose `sub` is `Delim.none`).
  */
final class Delim private (val lo: Long, val hi: Long, val high: Boolean, sub0: Delim | Null) {
  /** The delimiter to use for a sub-parse within one of our tokens (ourself, if nothing finer exists). */
  def sub: Delim = sub0 match
    case null => this
    case d: Delim => d

  /** Test whether a character code (0-65535 for text, 0-255 for raw bytes) is a delimiter. */
  inline def apply(c: Int): Boolean =
    if c < 64 then c >= 0 && ((lo >>> c) & 1L) != 0
    else if c < 128 then ((hi >>> (c - 64)) & 1L) != 0
    else high
}
object Delim {
  /** No delimiters at all: the token is everything that remains. */
  val none: Delim = new Delim(0L, 0L, false, null)

  /** Whitespace-delimited tokens; sub-tokens are undelimited. */
  val white: Delim = of(" \t\n\r\u000B\f", none)

  /** Line-delimited tokens; sub-tokens are whitespace-delimited. */
  val lines: Delim = of("\n\r", white)

  /** A delimiter set of exactly the characters in `chars`, with `sub` used within tokens.
    * Any non-ASCII character in `chars` makes _every_ non-ASCII character a delimiter.
    */
  def of(chars: String, sub: Delim = none): Delim =
    var lo = 0L
    var hi = 0L
    var high = false
    var k = 0
    while k < chars.length do
      val c = chars.charAt(k)
      if c < 64 then lo |= 1L << c
      else if c < 128 then hi |= 1L << (c - 64)
      else high = true
      k += 1
    new Delim(lo, hi, high, sub)
}


/////////////////////////////
/// The Grok parser itself ///
/////////////////////////////

/** A direct-mode parser: within a `Grok(input){ g => ... }` block you simply ask for what you
  * want (`g.I` for an Int, `g.tok` for a delimited token, `g < '-'` to require a character) and
  * either get it, with the cursor advanced, or jump to the block's boundary with a data-bearing
  * `Err` that says what was expected, what was found, and where.  A failed read leaves the cursor
  * at the point of failure.
  *
  * {{{
  * Grok("2025-06-25", partial = true): g =>
  *   (g.I, (g < '-').I, (g < '-').I)      // Ask[(Int, Int, Int)]
  * }}}
  *
  * Token-level operations (`tok`, `< "literal"`, `grok`, `skip`, and value parses) skip leading
  * delimiters unless the Grok was created with `exact = true`; `< (c: Char)` and `C` never skip.
  * Numbers and booleans must normally end at a delimiter or the end of input; with
  * `partial = true` they instead consume the longest valid prefix.
  *
  * Heavy lifting happens in per-source-type worker methods that report failure through an error
  * code; the tiny inline shims that jump on error are the only boundary-aware code, so the happy
  * path is monomorphic and jump-free.  A Grok is mutable state for exactly one parse: use it in
  * one thread and do not let it escape its block.
  */
sealed abstract class Grok protected () {

  // === Parse state (absolute indices into the root content; sub-parses narrow iA/iZ) ===

  @publicInBinary protected[eio] var i = 0L
  @publicInBinary protected[eio] var iA = 0L
  @publicInBinary protected[eio] var iZ = 0L
  @publicInBinary protected[eio] var dlm: Delim = Delim.lines
  @publicInBinary protected[eio] var partialMode = false
  @publicInBinary protected[eio] var skipMode = true

  // === Failure report, set by workers when they fail ===
  // eCode: 0 = ok; 1 = expected eWant, found <char at ePos>; 2 = message in eWant; 3 = expected eWant, found <token at ePos>
  // Invariant: error handling happens entirely on the error branch.  eCode is 0 except in flight
  // between an error branch setting it and failErr() consuming (and clearing) it, so success paths
  // never touch it.  All error deliveries go through failErr(); continue() and user-level Err
  // breaks bypass eCode entirely.

  @publicInBinary protected[eio] var eCode = 0
  @publicInBinary protected[eio] var ePos = 0L
  @publicInBinary protected[eio] var eWant = ""
  @publicInBinary protected[eio] var vPos = 0L

  // === Single-character lookahead cache ===
  // Invariant: cc IS the character code at the cursor (cc == at(i)), or -1 exactly when i >= iZ.
  // It is always loaded, so ops need no bounds checks of their own: cc == -1 encodes end-of-view,
  // and a loop's terminating read becomes the next op's lookahead, touching each character once.
  // Every move of the cursor must eagerly reload cc (the advance needed a guarded load anyway);
  // skipped means delimiters are already consumed at the cursor and clears on any move or
  // delimiter change.  Code that breaks out mid-parse (sub-parse failure) may leave these stale;
  // select restores them with the rest of the state, and otherwise the parse is over.

  @publicInBinary protected[eio] var cc = -1
  @publicInBinary protected[eio] var skipped = false

  // === Workers, instantiated per concrete source type from the inline templates below ===

  @publicInBinary protected[eio] def skipDelimsWork(): Unit
  @publicInBinary protected[eio] def tokEndWork(): Long
  @publicInBinary protected[eio] def loadWork(): Unit
  @publicInBinary protected[eio] def cWork(): Char
  @publicInBinary protected[eio] def matchCWork(c: Char): Unit
  @publicInBinary protected[eio] def matchTokWork(s: String): Unit
  @publicInBinary protected[eio] def longWork(): Long
  @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long
  @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long
  @publicInBinary protected[eio] def doubleWork(compute: Boolean): Double
  @publicInBinary protected[eio] def zWork(): Boolean
  @publicInBinary protected[eio] def tokWork(): String
  @publicInBinary protected[eio] def tokSpanWork(): Int

  // === Backtrack retention hooks ===
  // Indexed sources retain everything, so these defaults do nothing; windowed sources override
  // them to keep input from the pinned position onward available for re-reading (select uses
  // them around its alternatives).  Pins nest LIFO: pinWork answers the previous pin for the
  // matching releaseWork to restore.

  @publicInBinary protected[eio] def pinWork(pos: Long): Long = 0L
  @publicInBinary protected[eio] def releaseWork(token: Long): Unit = ()

  // === Cold access to raw content, for error reporting only ===

  protected def rawLength: Long
  protected def rawCharAt(pos: Long): Char


  ///////////////////////////////////////////////
  /// Non-jumping status and configuration API ///
  ///////////////////////////////////////////////

  /** Absolute position of the cursor within the original input. */
  final def position: Long = i

  /** True if any input remains in the current view. */
  final def hasMore: Boolean = i < iZ

  /** How many elements remain in the current view. */
  final def remaining: Long = iZ - i

  /** Consume any delimiters at the cursor.  Never fails. */
  final def sp: this.type = { skipDelimsWork(); this }

  /** Use a different delimiter set from here on. */
  final def delimit(d: Delim): this.type = { dlm = d; skipped = false; this }

  /** The character at the cursor (bytes as 0-255) without consuming it, or `alt` if no input remains. */
  inline final def peekOr(alt: Char): Char =
    val c = cc
    if c < 0 then alt else c.toChar


  /////////////////////////////////////////////////
  /// Readers: parse a value or jump with an Err ///
  /////////////////////////////////////////////////

  /** The character at the cursor (bytes as 0-255) without consuming it; fails if no input remains. */
  inline final def peek[E >: Alt[Err]](using Label[E]): Char =
    val c = cc
    if c < 0 then
      eCode = 1
      eWant = "a character"
      ePos = i
      boundary.break(Alt(failErr()))
    c.toChar

  /** Read a single character exactly as it is (bytes read as 0-255); delimiters are not skipped. */
  inline final def C[E >: Alt[Err]](using Label[E]): Char =
    val c = cWork()
    if eCode != 0 then boundary.break(Alt(failErr()))
    c

  /** Require character `c` exactly at the cursor (no delimiter skipping) and consume it. */
  inline final def <[E >: Alt[Err]](c: Char)(using Label[E]): this.type =
    matchCWork(c)
    if eCode != 0 then boundary.break(Alt(failErr()))
    this

  /** Require the literal `s` here (after skipping leading delimiters) and consume it.  `s` is
    * treated as its own token no matter what follows, so e.g. `g < ","` works in `"1, 2"` and `"1,2"` alike.
    * For whole-token equality, use `tok_?(_ == s)`.
    */
  inline final def <[E >: Alt[Err]](s: String)(using Label[E]): this.type =
    matchTokWork(s)
    if eCode != 0 then boundary.break(Alt(failErr()))
    this

  /** Parse a Long. */
  inline final def L[E >: Alt[Err]](using Label[E]): Long =
    val x = longWork()
    if eCode != 0 then boundary.break(Alt(failErr()))
    x

  /** Parse an Int (same rules as `L`, plus a range check). */
  inline final def I[E >: Alt[Err]](using Label[E]): Int =
    val x = longWork()
    if eCode != 0 then boundary.break(Alt(failErr()))
    if x < Int.MinValue || x > Int.MaxValue then
      eCode = 2
      eWant = "number out of Int range: " + x
      ePos = vPos
      boundary.break(Alt(failErr()))
    x.toInt

  /** Parse a Double (correctly rounded; also accepts NaN, Infinity, -Infinity). */
  inline final def D[E >: Alt[Err]](using Label[E]): Double =
    val x = doubleWork(true)
    if eCode != 0 then boundary.break(Alt(failErr()))
    x

  /** Advance past a Double without computing it; answers how many elements it occupied. */
  inline final def Dspan[E >: Alt[Err]](using Label[E]): Int =
    doubleWork(false) __ Unit
    if eCode != 0 then boundary.break(Alt(failErr()))
    (i - vPos).toInt

  /** Parse a Boolean (`true` or `false`, any case). */
  inline final def Z[E >: Alt[Err]](using Label[E]): Boolean =
    val b = zWork()
    if eCode != 0 then boundary.break(Alt(failErr()))
    b

  /** Read the next delimited token as a String.  The trailing delimiter is not consumed. */
  inline final def tok[E >: Alt[Err]](using Label[E]): String =
    val s = tokWork()
    if eCode != 0 then boundary.break(Alt(failErr()))
    s

  /** Advance past the next delimited token without creating a String; answers its length. */
  inline final def tokSpan[E >: Alt[Err]](using Label[E]): Int =
    val n = tokSpanWork()
    if eCode != 0 then boundary.break(Alt(failErr()))
    n

  /** Skip one token. */
  inline final def skip[E >: Alt[Err]]()(using Label[E]): Unit = skip(1)

  /** Skip `n` tokens. */
  inline final def skip[E >: Alt[Err]](n: Int)(using Label[E]): Unit =
    skipTokWork(n)
    if eCode != 0 then boundary.break(Alt(failErr()))

  /** Require that no input remains in the current view. */
  inline final def end[E >: Alt[Err]](using Label[E]): Unit =
    if i < iZ then
      eCode = 1
      eWant = "end of input"
      ePos = i
      boundary.break(Alt(failErr()))

  /** Fail the parse right here with a custom message. */
  inline final def oops[E >: Alt[Err]](msg: String)(using Label[E]): Nothing =
    eCode = 2
    eWant = msg
    ePos = i
    boundary.break(Alt(failErr()))


  //////////////////////////////////////////////////////
  /// Validating readers: parse, test, jump if unfit  ///
  //////////////////////////////////////////////////////

  /** Parse an Int and check it; fail with `msg` (pointing at the value) if the test does not pass. */
  inline final def I_?[E >: Alt[Err]](inline p: Int => Boolean, msg: String = "invalid value")(using Label[E]): Int =
    val x = I
    if !p(x) then
      eCode = 2
      eWant = msg
      ePos = vPos
      boundary.break(Alt(failErr()))
    x

  /** Parse a Long and check it; fail with `msg` if the test does not pass. */
  inline final def L_?[E >: Alt[Err]](inline p: Long => Boolean, msg: String = "invalid value")(using Label[E]): Long =
    val x = L
    if !p(x) then
      eCode = 2
      eWant = msg
      ePos = vPos
      boundary.break(Alt(failErr()))
    x

  /** Parse a Double and check it; fail with `msg` if the test does not pass. */
  inline final def D_?[E >: Alt[Err]](inline p: Double => Boolean, msg: String = "invalid value")(using Label[E]): Double =
    val x = D
    if !p(x) then
      eCode = 2
      eWant = msg
      ePos = vPos
      boundary.break(Alt(failErr()))
    x

  /** Read a token and check it; fail with `msg` if the test does not pass. */
  inline final def tok_?[E >: Alt[Err]](inline p: String => Boolean, msg: String = "invalid token")(using Label[E]): String =
    val s = tok
    if !p(s) then
      eCode = 2
      eWant = msg
      ePos = vPos
      boundary.break(Alt(failErr()))
    s


  //////////////////////////////////
  /// Structured parsing: sub-parse and alternatives ///
  //////////////////////////////////

  /** Parse within the next token using a sub-parser over just that token's extent; the delimiter
    * narrows one level (lines to words to nothing).  On success the cursor moves past the token;
    * on failure the error is wrapped with the sub-parse's starting position as it unwinds.
    */
  inline final def grok[B, E >: Alt[Err]]()(inline f: Label[B Or Err] ?=> this.type => B)(using Label[E]): B =
    grokImpl(dlm.sub)(f)

  /** Parse within the next token using a sub-parser with the delimiter given explicitly. */
  inline final def grok[B, E >: Alt[Err]](d: Delim)(inline f: Label[B Or Err] ?=> this.type => B)(using Label[E]): B =
    grokImpl(d)(f)

  protected inline def grokImpl[B, E >: Alt[Err]](d: Delim)(inline f: Label[B Or Err] ?=> this.type => B)(using Label[E]): B =
    if skipMode then skipDelimsWork()
    if i >= iZ then
      eCode = 1
      eWant = "a token to parse"
      ePos = i
      boundary.break(Alt(failErr()))
    val e = tokEndWork()
    val a0 = iA
    val z0 = iZ
    val d0 = dlm
    val t0 = i
    iA = i
    iZ = e
    dlm = d
    skipped = false
    val r = boundary[B Or Err]{ Is(f(this)) }
    iA = a0
    iZ = z0
    dlm = d0
    skipped = false
    r match
      case a: Alt[Err @unchecked] => boundary.break(Alt(subErr(t0, a.alt)))
      case _ =>
        i = e
        loadWork()
        r.get

  /** Try each alternative in order; the first to succeed supplies the answer.  A failed
    * alternative restores the cursor (and view, delimiter, and modes) before the next is tried.
    * Within an alternative, `continue()` abandons it without manufacturing an error.  If every
    * alternative fails, the whole parse fails with a trace of each attempt.
    */
  final def select[A, E >: Alt[Err]](alts: (Label[A Or Err] ?=> A)*)(using lb: Label[E]): A =
    val p0 = i
    val a0 = iA
    val z0 = iZ
    val d0 = dlm
    val pm0 = partialMode
    val sm0 = skipMode
    val cc0 = cc
    val sk0 = skipped
    val pin0 = pinWork(p0)   // windowed sources must retain from here so failed alternatives can be re-read
    var errs: List[Err] = Nil
    var found: A Or Err = Grok.declinedAlt
    var searching = true
    var k = 0
    while searching && k < alts.length do
      boundary[A Or Err]{ Is(alts(k)) } match
        case a: Alt[Err @unchecked] =>
          errs = a.alt :: errs
          i = p0
          iA = a0
          iZ = z0
          dlm = d0
          partialMode = pm0
          skipMode = sm0
          cc = cc0
          skipped = sk0
        case r =>
          found = r
          searching = false
      k += 1
    releaseWork(pin0)
    if searching then boundary.break(Alt(failAt(p0, "no alternative matched (" + alts.length + " tried)", errs.reverse)): E)(using lb)
    else found.get

  /** Abandon the current `select` alternative (or, outside any select, the whole parse). */
  inline final def continue[E >: Alt[Err]]()(using Label[E]): Nothing =
    boundary.break(Grok.declinedAlt: E)


  ///////////////////////////////
  /// Cold-path error assembly ///
  ///////////////////////////////

  private def lineColOf(pos: Long): Long =
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
    (line.toLong << 32) | (q - last)

  private def foundChar(p: Long): String =
    if p >= rawLength then "end of input"
    else if p >= iZ then "end of section"
    else rawCharAt(p) match
      case '\n' | '\r' => "end of line"
      case '\t' => "tab"
      case c if c < ' ' => "control character " + c.toInt
      case c => "'" + c + "'"

  private def foundTok(p: Long): String =
    if p >= rawLength then "end of input"
    else if p >= iZ then "end of section"
    else
      val sb = new java.lang.StringBuilder
      var j = p
      while j < iZ && j < p + 20 && !dlm(rawCharAt(j)) && rawCharAt(j) >= ' ' do
        sb.append(rawCharAt(j)) __ Unit
        j += 1
      if j < iZ && j == p + 20 && !dlm(rawCharAt(j)) then sb.append('…') __ Unit
      "\"" + sb + "\""

  /** Build a data-bearing parse error at an arbitrary position (used by `select`; workers go through `failErr`). */
  protected final def failAt(pos: Long, desc: String, attempts: List[Err]): Err =
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
    Err(new Grok.Failure(desc, q, (lc >>> 32).toInt, (lc & 0xFFFFFFFFL).toInt, sb.toString, off, attempts))

  /** Materialize the failure recorded in the error fields and clear them (called by the inline
    * shims when eCode is nonzero; the only place an in-flight error is consumed).
    */
  @publicInBinary protected[eio] def failErr(): Err =
    val desc = eCode match
      case 1 => "expected " + eWant + ", found " + foundChar(ePos)
      case 3 => "expected " + eWant + ", found " + foundTok(ePos)
      case _ => eWant
    eCode = 0
    failAt(ePos, desc, Nil)

  /** Wrap a sub-parse failure with where the sub-parse began, as data and text. */
  @publicInBinary protected[eio] def subErr(pos: Long, err: Err): Err =
    val lc = lineColOf(pos)
    Err(ErrType.Explained("in sub-parse starting at line " + (lc >>> 32).toInt + ", pos " + (lc & 0xFFFFFFFFL).toInt + ":", err, context = Some(pos)))

  /** Cold error-field setter, kept out of the hot workers so they stay within the JIT inlining budget. */
  @publicInBinary protected[eio] final def wantAt(want: String, pos: Long): Unit =
    eCode = 1
    eWant = want
    ePos = pos

  @publicInBinary protected[eio] def skipTokWork(n: Int): Unit =
    var k = n
    while k > 0 && eCode == 0 do
      if skipMode then skipDelimsWork()
      if i >= iZ then
        eCode = 1
        eWant = "a token"
        ePos = i
      else
        i = tokEndWork()
        loadWork()
        skipped = false
        k -= 1


  //////////////////////////////////////////////////////////////////////////
  /// Inline worker templates: written once, instantiated per source type ///
  //////////////////////////////////////////////////////////////////////////

  // `at` answers the unsigned character code at an absolute index (bytes as 0-255); `sub` cuts a
  // String from an absolute index range.  Each concrete subclass instantiates these inside plain
  // final methods so the JIT sees one monomorphic copy of each parser per source type.

  protected inline def loadImpl(inline at: Long => Int): Unit =
    cc = if i < iZ then at(i) else -1

  protected inline def skipDelimsImpl(inline at: Long => Int): Unit =
    if !skipped then
      var j = i
      var c = cc
      while c >= 0 && dlm(c) do
        j += 1
        c = if j < iZ then at(j) else -1
      i = j
      cc = c
      skipped = true

  protected inline def tokEndImpl(inline at: Long => Int): Long =
    var j = i
    var c = cc
    while c >= 0 && !dlm(c) do
      j += 1
      c = if j < iZ then at(j) else -1
    j

  protected inline def cImpl(inline at: Long => Int): Char =
    val c = cc
    if c >= 0 then
      i += 1
      cc = if i < iZ then at(i) else -1
      skipped = false
      c.toChar
    else
      eCode = 1
      eWant = "a character"
      ePos = i
      (0: Char)

  protected inline def matchCImpl(inline at: Long => Int)(c0: Char): Unit =
    if cc == c0.toInt then
      i += 1
      cc = if i < iZ then at(i) else -1
      skipped = false
    else
      eCode = 1
      eWant = "'" + c0 + "'"
      ePos = i

  protected inline def matchTokImpl(inline at: Long => Int, inline advise: Int => Unit)(s: String): Unit =
    if skipMode then skipDelimsImpl(at)
    advise(s.length + 1)
    val m = s.length
    var ok = iZ - i >= m
    var k = 0
    if ok && m > 0 then
      ok = cc == s.charAt(0).toInt
      k = 1
    while ok && k < m do
      if at(i + k) != s.charAt(k).toInt then ok = false
      k += 1
    if ok then
      i += m
      cc = if i < iZ then at(i) else -1
      skipped = false
    else
      eCode = 3
      eWant = "\"" + s + "\""
      ePos = i

  // === Digit-run kernel: the shared core of number scanning ===
  // Accumulates up to `budget` decimal digits POSITIVELY (callers negate as needed; a future
  // 19-digit budget for Eisel-Lemire relies on unsigned interpretation of the accumulator).
  // Starts at position j0 whose already-loaded lookahead is c0.  Commits the cursor — i moves
  // to the end of the run, cc holds the guarded lookahead there — and answers the accumulated
  // value; the caller recovers the digit count as i - j0.  Instantiated per source as the
  // digitsWork method (167 bytecodes, well under the JIT hot-inline budget; the call from each
  // worker is monomorphic) rather than expanded inline — measured equal, and it keeps callers
  // small when several ops share it.  NOTE for doubleImpl adoption: on windowed sources the
  // eager commit advances the retention point mid-number, so the double worker must pin vPos
  // to keep the slow-path sub(vPos, ...) readable.

  protected inline def digitsImpl(inline at: Long => Int)(j0: Long, c0: Int, budget: Int): Long =
    var j = j0
    var c = c0
    var x = 0L
    val jF = if iZ - j0 >= budget then j0 + budget else iZ
    boundary:
      while c >= '0' && c <= '9' do
        x = x * 10 + (c - '0')
        j += 1
        if j >= jF then boundary.break()
        c = at(j)
    i = j
    cc = if j >= jF then (if j < iZ then at(j) else -1) else c
    x

  protected inline def longImpl(inline at: Long => Int, inline advise: Int => Unit): Long =
    if skipMode then skipDelimsImpl(at)
    advise(20)
    vPos = i
    var j = i
    var neg = false
    var c = cc
    if c == '-' then
      neg = true
      j += 1
      c = if j < iZ then at(j) else -1
    else if c == '+' then
      j += 1
      c = if j < iZ then at(j) else -1
    val j0 = j
    val x = digitsWork(j, c, 18)
    skipped = false
    val cE = cc
    if i == j0 then
      wantAt("a number", i)
      0L
    else if i - j0 == 18 && cE >= '0' && cE <= '9' then
      longTailWork(-x, neg)     // a 19th digit exists: the checked tail continues in negative space
    else if !partialMode && cE >= 0 && !dlm(cE) then
      wantAt("end of number", i)
      0L
    else if neg then -x
    else x


  protected inline def longTailImpl(inline at: Long => Int)(x0: Long, neg: Boolean): Long =
    var j = i
    var x = x0
    val limit = if neg then Long.MinValue else -Long.MaxValue
    val before = limit / 10
    var over = false
    var c = cc
    while c >= '0' && c <= '9' do
      if x < before then over = true
      else
        x *= 10
        if x < limit + (c - '0') then over = true
        else x -= c - '0'
      j += 1
      c = if j < iZ then at(j) else -1
    i = j
    cc = c
    skipped = false
    if over then
      eCode = 2
      eWant = "number out of Long range"
      ePos = vPos
      0L
    else if !partialMode && c >= 0 && !dlm(c) then
      wantAt("end of number", j)
      0L
    else if neg then x
    else -x

  protected inline def zImpl(inline at: Long => Int, inline advise: Int => Unit): Boolean =
    if skipMode then skipDelimsImpl(at)
    advise(6)   // "false" + lookahead
    vPos = i
    var v = false
    var len = 0
    if i < iZ then
      val c = at(i) | 0x20
      if c == 't' && iZ - i >= 4 && (at(i+1)|0x20) == 'r' && (at(i+2)|0x20) == 'u' && (at(i+3)|0x20) == 'e' then
        v = true
        len = 4
      else if c == 'f' && iZ - i >= 5 && (at(i+1)|0x20) == 'a' && (at(i+2)|0x20) == 'l' && (at(i+3)|0x20) == 's' && (at(i+4)|0x20) == 'e' then
        len = 5
    if len == 0 then
      eCode = 1
      eWant = "'true' or 'false'"
      ePos = i
      false
    else
      i += len
      val c2 = if i < iZ then at(i) else -1
      cc = c2
      skipped = false
      if !partialMode && c2 >= 0 && !dlm(c2) then
        eCode = 1
        eWant = "end of boolean"
        ePos = i
        false
      else v

  protected inline def tokImpl(inline at: Long => Int, inline sub: (Long, Long) => String): String =
    if skipMode then skipDelimsImpl(at)
    vPos = i
    if i >= iZ then
      eCode = 1
      eWant = "a token"
      ePos = i
      ""
    else
      val j = tokEndImpl(at)
      val s = sub(i, j)
      i = j
      cc = if j < iZ then at(j) else -1
      skipped = false
      s

  protected inline def tokSpanImpl(inline at: Long => Int): Int =
    if skipMode then skipDelimsImpl(at)
    vPos = i
    if i >= iZ then
      eCode = 1
      eWant = "a token"
      ePos = i
      0
    else
      val j = tokEndImpl(at)
      val n = (j - i).toInt
      i = j
      cc = if j < iZ then at(j) else -1
      skipped = false
      n

  // cc-threaded throughout (each char is read once), significant digits gathered by the
  // digitsWork kernel, and a single exit so windowed sources can pin vPos across the kernel's
  // eager cursor commits (the slow path re-reads [vPos, end) at the very end)
  protected inline def doubleImpl(inline at: Long => Int, inline sub: (Long, Long) => String, inline advise: Int => Unit)(compute: Boolean): Double =
    if skipMode then skipDelimsImpl(at)
    advise(28)   // covers typical doubles; longer ones self-heal read by read
    vPos = i
    val pv = pinWork(vPos)
    val result =
      var j = i
      var c = cc
      var neg = false
      if c == '-' then
        neg = true
        j += 1
        c = if j < iZ then at(j) else -1
      else if c == '+' then
        j += 1
        c = if j < iZ then at(j) else -1
      var special = 0.0
      var isSpecial = false
      if c == 'N' && iZ - j >= 3 && at(j+1) == 'a' && at(j+2) == 'N' then
        j += 3
        special = Double.NaN
        isSpecial = true
      else if c == 'I' && iZ - j >= 8 && at(j+1)=='n' && at(j+2)=='f' && at(j+3)=='i' && at(j+4)=='n' && at(j+5)=='i' && at(j+6)=='t' && at(j+7)=='y' then
        j += 8
        special = if neg then Double.NegativeInfinity else Double.PositiveInfinity
        isSpecial = true
      if isSpecial then
        c = if j < iZ then at(j) else -1
        i = j
        cc = c
        skipped = false
        if !partialMode && c >= 0 && !dlm(c) then
          eCode = 1
          eWant = "end of number"
          ePos = j
          0.0
        else special
      else
        var anyDigit = false
        while c == '0' do   // leading integer zeros are not significant
          anyDigit = true
          j += 1
          c = if j < iZ then at(j) else -1
        var mant = 0L
        var nd = 0
        var droppedInt = 0
        var truncated = false
        if c >= '1' && c <= '9' then
          mant = digitsWork(j, c, 18)
          nd = (i - j).toInt
          anyDigit = true
          j = i
          c = cc
          while c >= '0' && c <= '9' do   // significance exhausted: count dropped integer digits
            droppedInt += 1
            if c != '0' then truncated = true
            j += 1
            c = if j < iZ then at(j) else -1
        var fracScale = 0
        if c == '.' then
          j += 1
          c = if j < iZ then at(j) else -1
          if nd == 0 then
            while c == '0' do   // leading fraction zeros scale the value but are not significant
              anyDigit = true
              fracScale += 1
              j += 1
              c = if j < iZ then at(j) else -1
          if nd < 18 && c >= '0' && c <= '9' then
            val v = digitsWork(j, c, 18 - nd)
            val fd = (i - j).toInt
            mant = mant * Grok.pow10L(fd) + v   // nd + fd <= 18, so this cannot overflow
            nd += fd
            fracScale += fd
            anyDigit = true
            j = i
            c = cc
          while c >= '0' && c <= '9' do   // dropped fraction digits: only roundability matters
            if c != '0' then truncated = true
            j += 1
            c = if j < iZ then at(j) else -1
        if !anyDigit then
          i = j
          cc = c
          skipped = false
          eCode = 1
          eWant = "a number"
          ePos = j
          0.0
        else
          var e10 = droppedInt - fracScale
          if c == 'e' || c == 'E' then
            var k = j + 1
            var esign = false
            var cq = if k < iZ then at(k) else -1
            if cq == '+' || cq == '-' then
              esign = cq == '-'
              k += 1
              cq = if k < iZ then at(k) else -1
            if cq >= '0' && cq <= '9' then
              var ex = 0
              while cq >= '0' && cq <= '9' do
                if ex < 100000000 then ex = ex * 10 + (cq - '0')
                k += 1
                cq = if k < iZ then at(k) else -1
              e10 += (if esign then -ex else ex)
              j = k
              c = cq
          i = j
          cc = c
          skipped = false
          if !partialMode && c >= 0 && !dlm(c) then
            eCode = 1
            eWant = "end of number"
            ePos = j
            0.0
          else if !compute then 0.0
          else if mant == 0 then
            if neg then -0.0 else 0.0
          else if !truncated && nd <= 15 && e10 >= -22 && e10 <= 22 then
            // Exact-arithmetic fast path (Clinger): mantissa and power of ten are both exactly
            // representable, so the single multiply or divide rounds correctly.
            var v = mant.toDouble
            if e10 > 0 then v *= Grok.pow10(e10)
            else if e10 < 0 then v /= Grok.pow10(-e10)
            if neg then -v else v
          else
            // Rare hard cases (>15 digits or extreme exponent): let the JDK do the correct rounding.
            java.lang.Double.parseDouble(sub(vPos, j))
    releaseWork(pv)
    result
}


object Grok {

  /** A data-bearing parse failure: what was being sought, where the input broke, and what the
    * input looked like there.  `attempts` holds the individual failures when alternatives were
    * tried and none matched.  Rendered as e.g.
    * `Could not grok 2025-06b-25: expected '-', found 'b' (line 1, pos 8)`.
    */
  final class Failure(
    val description: String,
    val position: Long,
    val line: Int,
    val column: Int,
    val excerpt: String,
    val excerptOffset: Int,
    val attempts: List[Err]
  ) extends ErrType {
    type E = String
    def error: E = description

    /** The one-line summary (attempted alternatives are only in `buildLines`/`toString`). */
    def message: String = s"Could not grok $excerpt: $description (line $line, pos $column)"

    override lazy val toString: String =
      if attempts.isEmpty then message
      else MkStr: sb =>
        buildLines(sb, "")

    def buildLines(sb: MkStr, prefix: String): Unit =
      ErrType.buildLinesFromString(sb, message, prefix)
      var k = 0
      attempts.foreach{ e =>
        k += 1
        Err.buildLines(e)(sb, prefix + s"#$k: ")
      }

    def toThrowable: Throwable = ErrType.StringErrException(toString)
  }

  @publicInBinary private[eio] val pow10: Array[Double] = Array(
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11,
    1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22
  )

  @publicInBinary private[eio] val pow10L: Array[Long] = Array(
    1L, 10L, 100L, 1000L, 10000L, 100000L, 1000000L, 10000000L, 100000000L, 1000000000L,
    10000000000L, 100000000000L, 1000000000000L, 10000000000000L, 100000000000000L,
    1000000000000000L, 10000000000000000L, 100000000000000000L, 1000000000000000000L
  )

  @publicInBinary private[eio] val declinedAlt: Alt[Err] = Alt(Err("alternative declined"))


  /** Groks a String.  Create via `Grok(...)`, not directly. */
  final class Str(content: String, d: Delim, partial: Boolean, autoskip: Boolean) extends Grok {
    dlm = d
    partialMode = partial
    skipMode = autoskip
    iZ = content.length
    cc = if content.length > 0 then content.charAt(0) else -1

    protected def rawLength: Long = content.length
    protected def rawCharAt(pos: Long): Char = content.charAt(pos.toInt)

    @publicInBinary protected[eio] def skipDelimsWork(): Unit = skipDelimsImpl(j => content.charAt(j.toInt))
    @publicInBinary protected[eio] def tokEndWork(): Long = tokEndImpl(j => content.charAt(j.toInt))
    @publicInBinary protected[eio] def loadWork(): Unit = loadImpl(j => content.charAt(j.toInt))
    @publicInBinary protected[eio] def cWork(): Char = cImpl(j => content.charAt(j.toInt))
    @publicInBinary protected[eio] def matchCWork(c: Char): Unit = matchCImpl(j => content.charAt(j.toInt))(c)
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit = matchTokImpl(j => content.charAt(j.toInt), _ => ())(s)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => content.charAt(j.toInt), _ => ())

    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => content.charAt(j.toInt))(x0, neg)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => content.charAt(j.toInt))(j0, c0, budget)
    @publicInBinary protected[eio] def doubleWork(compute: Boolean): Double = doubleImpl(j => content.charAt(j.toInt), (a, b) => content.substring(a.toInt, b.toInt), _ => ())(compute)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => content.charAt(j.toInt), _ => ())
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => content.charAt(j.toInt), (a, b) => content.substring(a.toInt, b.toInt))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => content.charAt(j.toInt))
  }

  /** Parse a String in direct mode; any failure jumps out as a data-bearing `Err`.
    *
    * `delim` sets the token delimiter (default: line ends).  `partial = true` lets numbers and
    * booleans stop at the first character that cannot continue them, rather than requiring a
    * delimiter.  `exact = true` turns off the automatic skipping of leading delimiters.
    */
  inline def apply[A](content: String, delim: Delim = Delim.lines, partial: Boolean = false, exact: Boolean = false)(inline f: Label[A Or Err] ?=> Str => A): Ask[A] =
    Or.Nice(f(new Str(content, delim, partial, !exact)))


  /** Groks raw bytes.  Structure (delimiters, numbers, and `<` literals) is ASCII, read as
    * unsigned 0-255; `tok` decodes its span as UTF-8, which is safe because multi-byte sequences
    * never contain ASCII bytes.  Error positions are byte positions.  Create via `Grok(...)`.
    */
  final class Bytes(content: Array[Byte], d: Delim, partial: Boolean, autoskip: Boolean) extends Grok {
    dlm = d
    partialMode = partial
    skipMode = autoskip
    iZ = content.length
    cc = if content.length > 0 then content(0) & 0xFF else -1

    protected def rawLength: Long = content.length
    protected def rawCharAt(pos: Long): Char = (content(pos.toInt) & 0xFF).toChar

    private def utf8(a: Long, b: Long): String = new String(content, a.toInt, (b - a).toInt, java.nio.charset.StandardCharsets.UTF_8)

    @publicInBinary protected[eio] def skipDelimsWork(): Unit = skipDelimsImpl(j => content(j.toInt) & 0xFF)
    @publicInBinary protected[eio] def tokEndWork(): Long = tokEndImpl(j => content(j.toInt) & 0xFF)
    @publicInBinary protected[eio] def loadWork(): Unit = loadImpl(j => content(j.toInt) & 0xFF)
    @publicInBinary protected[eio] def cWork(): Char = cImpl(j => content(j.toInt) & 0xFF)
    @publicInBinary protected[eio] def matchCWork(c: Char): Unit = matchCImpl(j => content(j.toInt) & 0xFF)(c)
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit = matchTokImpl(j => content(j.toInt) & 0xFF, _ => ())(s)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => content(j.toInt) & 0xFF, _ => ())
    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => content(j.toInt) & 0xFF)(x0, neg)
    @publicInBinary protected[eio] def doubleWork(compute: Boolean): Double = doubleImpl(j => content(j.toInt) & 0xFF, (a, b) => utf8(a, b), _ => ())(compute)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => content(j.toInt) & 0xFF, _ => ())
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => content(j.toInt) & 0xFF, (a, b) => utf8(a, b))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => content(j.toInt) & 0xFF)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => content(j.toInt) & 0xFF)(j0, c0, budget)
  }

  /** Groks characters from an `Array[Char]`; semantics as for the String form.  Create via `Grok(...)`. */
  final class Chars(content: Array[Char], d: Delim, partial: Boolean, autoskip: Boolean) extends Grok {
    dlm = d
    partialMode = partial
    skipMode = autoskip
    iZ = content.length
    cc = if content.length > 0 then content(0).toInt else -1

    protected def rawLength: Long = content.length
    protected def rawCharAt(pos: Long): Char = content(pos.toInt)

    @publicInBinary protected[eio] def skipDelimsWork(): Unit = skipDelimsImpl(j => content(j.toInt))
    @publicInBinary protected[eio] def tokEndWork(): Long = tokEndImpl(j => content(j.toInt))
    @publicInBinary protected[eio] def loadWork(): Unit = loadImpl(j => content(j.toInt))
    @publicInBinary protected[eio] def cWork(): Char = cImpl(j => content(j.toInt))
    @publicInBinary protected[eio] def matchCWork(c: Char): Unit = matchCImpl(j => content(j.toInt))(c)
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit = matchTokImpl(j => content(j.toInt), _ => ())(s)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => content(j.toInt), _ => ())
    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => content(j.toInt))(x0, neg)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => content(j.toInt))(j0, c0, budget)
    @publicInBinary protected[eio] def doubleWork(compute: Boolean): Double = doubleImpl(j => content(j.toInt), (a, b) => new String(content, a.toInt, (b - a).toInt), _ => ())(compute)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => content(j.toInt), _ => ())
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => content(j.toInt), (a, b) => new String(content, a.toInt, (b - a).toInt))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => content(j.toInt))
  }

  /** Parse raw bytes in direct mode; parameters as for the String form.  (Scala permits default
    * arguments on only one overload, so this one takes all of them explicitly.)
    */
  inline def apply[A](content: Array[Byte], delim: Delim, partial: Boolean, exact: Boolean)(inline f: Label[A Or Err] ?=> Bytes => A): Ask[A] =
    Or.Nice(f(new Bytes(content, delim, partial, !exact)))

  /** Parse raw bytes in direct mode with the default settings (line tokens, strict numbers, delimiter skipping). */
  inline def apply[A](content: Array[Byte])(inline f: Label[A Or Err] ?=> Bytes => A): Ask[A] =
    apply(content, Delim.lines, false, false)(f)

  /** Groks raw bytes held in a `Mem[Byte]` (heap-wrapped or off-heap); semantics as for `Bytes`.
    * The Mem must stay alive and unmodified for the duration of the parse.  Create via `Grok(...)`.
    */
  final class MemBytes(content: Mem[Byte], d: Delim, partial: Boolean, autoskip: Boolean) extends Grok {
    dlm = d
    partialMode = partial
    skipMode = autoskip
    iZ = content.length
    cc = if content.length > 0 then content(0L) & 0xFF else -1

    protected def rawLength: Long = content.length
    protected def rawCharAt(pos: Long): Char = (content(pos) & 0xFF).toChar

    private def utf8(a: Long, b: Long): String =
      val n = (b - a).toInt
      val arr = new Array[Byte](n)
      var k = 0
      while k < n do
        arr(k) = content(a + k)
        k += 1
      new String(arr, java.nio.charset.StandardCharsets.UTF_8)

    @publicInBinary protected[eio] def skipDelimsWork(): Unit = skipDelimsImpl(j => content(j) & 0xFF)
    @publicInBinary protected[eio] def tokEndWork(): Long = tokEndImpl(j => content(j) & 0xFF)
    @publicInBinary protected[eio] def loadWork(): Unit = loadImpl(j => content(j) & 0xFF)
    @publicInBinary protected[eio] def cWork(): Char = cImpl(j => content(j) & 0xFF)
    @publicInBinary protected[eio] def matchCWork(c: Char): Unit = matchCImpl(j => content(j) & 0xFF)(c)
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit = matchTokImpl(j => content(j) & 0xFF, _ => ())(s)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => content(j) & 0xFF, _ => ())
    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => content(j) & 0xFF)(x0, neg)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => content(j) & 0xFF)(j0, c0, budget)
    @publicInBinary protected[eio] def doubleWork(compute: Boolean): Double = doubleImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b), _ => ())(compute)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => content(j) & 0xFF, _ => ())
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => content(j) & 0xFF)
  }

  /** Parse bytes held in a `Mem[Byte]` in direct mode; parameters as for the String form. */
  inline def apply[A](content: Mem[Byte], delim: Delim, partial: Boolean, exact: Boolean)(inline f: Label[A Or Err] ?=> MemBytes => A): Ask[A] =
    Or.Nice(f(new MemBytes(content, delim, partial, !exact)))

  /** Parse bytes held in a `Mem[Byte]` in direct mode with the default settings (line tokens, strict numbers, delimiter skipping). */
  inline def apply[A](content: Mem[Byte])(inline f: Label[A Or Err] ?=> MemBytes => A): Ask[A] =
    apply(content, Delim.lines, false, false)(f)

  /** Parse an `Array[Char]` in direct mode; parameters as for the String form. */
  inline def apply[A](content: Array[Char], delim: Delim, partial: Boolean, exact: Boolean)(inline f: Label[A Or Err] ?=> Chars => A): Ask[A] =
    Or.Nice(f(new Chars(content, delim, partial, !exact)))

  /** Parse an `Array[Char]` in direct mode with the default settings (line tokens, strict numbers, delimiter skipping). */
  inline def apply[A](content: Array[Char])(inline f: Label[A Or Err] ?=> Chars => A): Ask[A] =
    apply(content, Delim.lines, false, false)(f)


  /** Groks bytes pulled on demand through a sliding window, using the ordinary indexed worker
    * templates.  `index - discard` addresses the window; a read past the loaded end (or an
    * `advise(n)` that does not fit) scoots the window forward and refills from the pull
    * function.  The window retains everything from the current op's start — which is all the
    * templates ever re-read — or from the earliest live `select` mark, whose alternatives may
    * need re-reading; when retention leaves no room to advance, the window doubles.
    * `fill(dst, off, max)` answers how many bytes it wrote (0 when it has no more); the total
    * input length must be known up front.  Error excerpts degrade to `?` behind the window.
    */
  final class Buffered(fill: (Array[Byte], Int, Int) => Int, totalLen: Long, d: Delim, partial: Boolean, autoskip: Boolean, window: Int = 64) extends Grok {
    dlm = d
    partialMode = partial
    skipMode = autoskip
    iZ = totalLen

    private var buf = new Array[Byte](if window < 8 then 8 else window)
    private var discard = 0L   // absolute position of buf(0)
    private var loaded = 0     // bytes of buf currently valid
    private var pinned = -1L   // earliest select mark to retain, or -1 for none
    refill()
    cc = if loaded > 0 then buf(0) & 0xFF else -1

    protected def rawLength: Long = totalLen
    protected def rawCharAt(pos: Long): Char =
      val d = (pos - discard).toInt
      if 0 <= d && d < loaded then (buf(d) & 0xFF).toChar else '?'   // cold path: excerpts degrade outside the window

    private def refill(): Unit =
      var more = true
      while more && loaded < buf.length do
        val n = fill(buf, loaded, buf.length - loaded)
        if n > 0 then loaded += n else more = false

    // Off the hot path: drop everything behind the retention point (the current op's start, or
    // the earliest live select mark), double the window if that frees no room, then refill.
    private def scoot(): Unit =
      val from = if 0 <= pinned && pinned < i then pinned else i
      val keep = (from - discard).toInt
      if keep < 0 then throw new IllegalStateException("Grok.Buffered: backtrack to " + from + " behind the window at " + discard)
      if keep > 0 then
        System.arraycopy(buf, keep, buf, 0, loaded - keep)
        discard += keep
        loaded -= keep
      if loaded == buf.length then buf = java.util.Arrays.copyOf(buf, buf.length << 1)
      refill()

    private def fetched(j: Long): Int =
      scoot()
      val d = (j - discard).toInt
      if 0 <= d && d < loaded then buf(d) & 0xFF
      else throw new IllegalStateException("Grok.Buffered: cannot load position " + j + " (window at " + discard + ", " + loaded + " loaded)")

    @publicInBinary protected[eio] override def pinWork(pos: Long): Long =
      val prev = pinned
      if prev < 0 || pos < prev then pinned = pos
      prev

    @publicInBinary protected[eio] override def releaseWork(token: Long): Unit = pinned = token

    private inline def atc(j: Long): Int =
      val d = (j - discard).toInt
      if 0 <= d && d < loaded then buf(d) & 0xFF
      else fetched(j)

    private inline def advc(n: Int): Unit =
      if i + n > discard + loaded && discard + loaded < totalLen then scoot()

    private def utf8(a: Long, b: Long): String = new String(buf, (a - discard).toInt, (b - a).toInt, java.nio.charset.StandardCharsets.UTF_8)

    @publicInBinary protected[eio] def skipDelimsWork(): Unit = skipDelimsImpl(j => atc(j))
    @publicInBinary protected[eio] def tokEndWork(): Long = tokEndImpl(j => atc(j))
    @publicInBinary protected[eio] def loadWork(): Unit = loadImpl(j => atc(j))
    @publicInBinary protected[eio] def cWork(): Char = cImpl(j => atc(j))
    @publicInBinary protected[eio] def matchCWork(c: Char): Unit = matchCImpl(j => atc(j))(c)
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit = matchTokImpl(j => atc(j), n => advc(n))(s)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => atc(j), n => advc(n))
    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => atc(j))(x0, neg)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => atc(j))(j0, c0, budget)
    @publicInBinary protected[eio] def doubleWork(compute: Boolean): Double = doubleImpl(j => atc(j), (a, b) => utf8(a, b), n => advc(n))(compute)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => atc(j), n => advc(n))
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => atc(j), (a, b) => utf8(a, b))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => atc(j))
  }

  /** Parse raw bytes pulled through a sliding window that starts at `window` bytes (mostly
    * useful for testing the windowed machinery against direct indexing — prefer `Grok(bytes)`
    * when the whole array is at hand; parameters otherwise as for the String form).
    */
  inline def buffered[A](content: Array[Byte], delim: Delim = Delim.lines, partial: Boolean = false, exact: Boolean = false, window: Int = 64)(inline f: Label[A Or Err] ?=> Buffered => A): Ask[A] =
    var pos = 0
    val fill = (dst: Array[Byte], off: Int, max: Int) => {
      val n = if content.length - pos < max then content.length - pos else max
      if n > 0 then System.arraycopy(content, pos, dst, off, n)
      pos += n
      n
    }
    Or.Nice(f(new Buffered(fill, content.length, delim, partial, !exact, window)))

  /** Parse a ByteBuffer's remaining bytes, consumed via relative bulk `get()` through a sliding
    * window that starts at `window` bytes; parameters otherwise as for the String form.
    */
  inline def buffered[A](content: java.nio.ByteBuffer, delim: Delim, partial: Boolean, exact: Boolean, window: Int)(inline f: Label[A Or Err] ?=> Buffered => A): Ask[A] =
    val fill = (dst: Array[Byte], off: Int, max: Int) => {
      val n = if content.remaining() < max then content.remaining() else max
      content.get(dst, off, n) __ Unit
      n
    }
    Or.Nice(f(new Buffered(fill, content.remaining(), delim, partial, !exact, window)))

  /** Parse a ByteBuffer's remaining bytes with the default settings (line tokens, strict numbers,
    * delimiter skipping, 64-byte initial window).
    */
  inline def buffered[A](content: java.nio.ByteBuffer)(inline f: Label[A Or Err] ?=> Buffered => A): Ask[A] =
    buffered(content, Delim.lines, false, false, 64)(f)
}
