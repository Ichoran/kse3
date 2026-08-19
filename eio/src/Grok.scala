// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2024, 2026 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio


import scala.annotation.publicInBinary
import scala.util.boundary
import scala.util.boundary.Label

import kse.basics.{given, _}
import kse.flow.{given, _}
import kse.maths.{UByte, UShort, UInt, ULong, EiselLemire}


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

  /** The union: characters of either delimiter delimit.  Composed delimiters have no sub
    * chain of their own; use `over` to attach one.
    */
  def |(that: Delim): Delim = new Delim(lo | that.lo, hi | that.hi, high || that.high, null)

  /** The intersection: only characters in both delimiters delimit. */
  def &(that: Delim): Delim = new Delim(lo & that.lo, hi & that.hi, high && that.high, null)

  /** The difference: this delimiter with `that`'s characters removed. */
  def &~(that: Delim): Delim = new Delim(lo & ~that.lo, hi & ~that.hi, high && !that.high, null)

  /** This delimiter with `finer` appended at the bottom of its sub chain (replacing an
    * undelimited terminal), so sub-parses narrow through every level in turn:
    * `Delim.lines over Delim.of(",")` tokenizes lines, then words within lines, then
    * comma-fields within words.
    */
  def over(finer: Delim): Delim = sub0 match
    case null => new Delim(lo, hi, high, finer)
    case d: Delim =>
      if d.lo == 0L && d.hi == 0L && !d.high then new Delim(lo, hi, high, finer)
      else new Delim(lo, hi, high, d.over(finer))
}
object Delim {
  /** No delimiters at all: the token is everything that remains. */
  val none: Delim = new Delim(0L, 0L, false, null)

  /** Whitespace-delimited tokens; sub-tokens are undelimited. */
  val white: Delim = of(" \t\n\r\u000B\f", none)

  /** Line-delimited tokens; sub-tokens are whitespace-delimited. */
  val lines: Delim = of("\n\r", white)

  /** A delimiter set of exactly the one character `c` (sub-tokens undelimited); non-ASCII `c`
    * makes _every_ non-ASCII character a delimiter.
    */
  def one(c: Char): Delim =
    if c < 64 then new Delim(1L << c, 0L, false, null)
    else if c < 128 then new Delim(0L, 1L << (c - 64), false, null)
    else new Delim(0L, 0L, true, null)

  /** Normalize a delimiter specification at compile time: a Delim is itself, a Char is
    * `Delim.one`, a String is `Delim.of`.
    */
  inline def from(inline spec: Delim | Char | String): Delim = inline spec match
    case d: Delim => d
    case c: Char => one(c)
    case s: String => of(s)

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


/** A quoted-string style: which character quotes the string, and how a quote is embedded
  * within one.  With `doubled = false` the backslash convention applies (the JSON escape set:
  * `\" \\ \/ \b \f \n \r \t \uXXXX`, plus the active quote character); with `doubled = true`
  * the quote character is embedded by writing it twice, and a backslash is ordinary content
  * (RFC 4180).  The quote character must be ASCII, because byte-based sources match it as a
  * raw byte.
  */
final class Quote private[eio] (val quote: Char, val doubled: Boolean) {}
object Quote {
  /** JSON-style: `"` quotes, backslash escapes, `\uXXXX` unicode. */
  val json: Quote = new Quote('"', false)

  /** CSV-style (RFC 4180): `"` quotes, `""` embeds a quote, no other escapes. */
  val csv: Quote = new Quote('"', true)

  /** SQL-style string literal: `'` quotes, `''` embeds a quote. */
  val sql: Quote = new Quote('\'', true)

  /** A custom style: `quote` (ASCII) delimits the string, embedded by doubling if `doubled`
    * and by the backslash convention otherwise.
    */
  def apply(quote: Char, doubled: Boolean = false): Quote =
    if quote >= 128 then throw new IllegalArgumentException("quote character must be ASCII but got '" + quote + "'")
    new Quote(quote, doubled)
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
  * Token-level operations (`tok`, `str`, `< "literal"`, `grok`, `skip`, and value parses) skip
  * leading delimiters unless the Grok was created with `exact = true`.  In exact mode a
  * delimiter is a separator: completing a token-level operation permits the next one to
  * consume exactly one delimiter, so adjacent separators delimit empty tokens (`tok` three
  * times on `"a,,c"` yields `"a"`, `""`, `"c"`) and iteration always makes progress.
  * `< (c: Char)` and `C` never skip, and in exact mode taking the cursor manually revokes the
  * separator permission.
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

  // === Separator grant (exact mode only) ===
  // In exact mode a delimiter is a separator: each token-level op that completes grants
  // permission (sepOk) to consume exactly one delimiter, and the next token-level op spends
  // the grant before it reads.  An empty token between adjacent separators is thus real data,
  // yet progress is guaranteed: an op that consumes nothing either errors or is the single
  // ungranted-to-granted transition possible at one position.  `C` and `< char` take manual
  // control of the cursor and revoke the grant.  Skip mode never consults it.

  @publicInBinary protected[eio] var sepOk = false

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
  @publicInBinary protected[eio] def hexWork(j0: Long, c0: Int, budget: Int): Long
  @publicInBinary protected[eio] def doubleWork(mode: Int): Double   // 0 = span only, 1 = Double, 2 = Float (exact in the Double)
  @publicInBinary protected[eio] def zWork(): Boolean
  @publicInBinary protected[eio] def tokWork(): String
  @publicInBinary protected[eio] def tokSpanWork(): Int
  @publicInBinary protected[eio] def charsWork(n: Int, exact: Boolean): String
  @publicInBinary protected[eio] def strWork(qc: Char, mode: Int): Grok.StrOut    // backslash-escape styles
  @publicInBinary protected[eio] def strqWork(qc: Char, mode: Int): Grok.StrOut   // quote-doubling styles

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


  //////////////////////////////////
  /// Running a parse to Ask[A]  ///
  //////////////////////////////////

  /** Run a parsing block against this Grok: the block's result becomes `Is`, and any read
    * failure, `Err` break, or caught exception becomes `Alt`.  Normally used curried with the
    * factories, `Grok(content, ...){ g => ... }`.
    */
  inline final def apply[A](inline f: Label[A Or Err] ?=> this.type => A): Ask[A] =
    Or.Nice(f(this))


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

  /** Use a different delimiter set from here on: a Delim, or a Char or String meaning a
    * delimiter of exactly those characters.
    */
  inline final def delimit(inline d: Delim | Char | String): this.type =
    dlm = Delim.from(d)
    skipped = false
    this

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

  /** Parse a Short (same rules as `L`, plus a range check). */
  inline final def S[E >: Alt[Err]](using Label[E]): Short =
    val x = smallLongWork(5, "Short", true)
    if eCode != 0 then boundary.break(Alt(failErr()))
    if x < Short.MinValue || x > Short.MaxValue then
      eCode = 2
      eWant = "number out of Short range: " + x
      ePos = vPos
      boundary.break(Alt(failErr()))
    x.toShort

  /** Parse a Byte (same rules as `L`, plus a range check). */
  inline final def B[E >: Alt[Err]](using Label[E]): Byte =
    val x = smallLongWork(3, "Byte", true)
    if eCode != 0 then boundary.break(Alt(failErr()))
    if x < Byte.MinValue || x > Byte.MaxValue then
      eCode = 2
      eWant = "number out of Byte range: " + x
      ePos = vPos
      boundary.break(Alt(failErr()))
    x.toByte

  /** Parse a ULong: an unsigned decimal up to 2^64-1.  A leading `+` is allowed, `-` is not. */
  inline final def uL[E >: Alt[Err]](using Label[E]): ULong =
    val x = uLongWork()
    if eCode != 0 then boundary.break(Alt(failErr()))
    ULong.wrap(x)

  /** Parse a UInt (same rules as `uL`, plus a range check). */
  inline final def uI[E >: Alt[Err]](using Label[E]): UInt =
    val x = smallLongWork(10, "UInt", false)
    if eCode != 0 then boundary.break(Alt(failErr()))
    if (x & 0xFFFFFFFF00000000L) != 0 then
      eCode = 2
      eWant = "number out of UInt range: " + x
      ePos = vPos
      boundary.break(Alt(failErr()))
    UInt.wrap(x.toInt)

  /** Parse a UShort (same rules as `uL`, plus a range check). */
  inline final def uS[E >: Alt[Err]](using Label[E]): UShort =
    val x = smallLongWork(5, "UShort", false)
    if eCode != 0 then boundary.break(Alt(failErr()))
    if (x & 0xFFFFFFFFFFFF0000L) != 0 then
      eCode = 2
      eWant = "number out of UShort range: " + x
      ePos = vPos
      boundary.break(Alt(failErr()))
    UShort.wrap(x.toShort)

  /** Parse a UByte (same rules as `uL`, plus a range check). */
  inline final def uB[E >: Alt[Err]](using Label[E]): UByte =
    val x = smallLongWork(3, "UByte", false)
    if eCode != 0 then boundary.break(Alt(failErr()))
    if (x & 0xFFFFFFFFFFFFFF00L) != 0 then
      eCode = 2
      eWant = "number out of UByte range: " + x
      ePos = vPos
      boundary.break(Alt(failErr()))
    UByte.wrap(x.toByte)

  /** Parse a Long from up to 16 bare hex digits (case-insensitive, no `0x` prefix, no sign);
    * the digits are the bit pattern, so e.g. `FFFFFFFFFFFFFFFF` is -1.
    */
  inline final def xL[E >: Alt[Err]](using Label[E]): Long =
    val x = smallHexWork(16, "hex Long")
    if eCode != 0 then boundary.break(Alt(failErr()))
    x

  /** Parse an Int from up to 8 bare hex digits (same rules as `xL`): `FFFFFFFF` is -1. */
  inline final def xI[E >: Alt[Err]](using Label[E]): Int =
    val x = smallHexWork(8, "hex Int")
    if eCode != 0 then boundary.break(Alt(failErr()))
    x.toInt

  /** Parse a Short from up to 4 bare hex digits (same rules as `xL`): `FFFF` is -1. */
  inline final def xS[E >: Alt[Err]](using Label[E]): Short =
    val x = smallHexWork(4, "hex Short")
    if eCode != 0 then boundary.break(Alt(failErr()))
    x.toShort

  /** Parse a Byte from up to 2 bare hex digits (same rules as `xL`): `FF` is -1. */
  inline final def xB[E >: Alt[Err]](using Label[E]): Byte =
    val x = smallHexWork(2, "hex Byte")
    if eCode != 0 then boundary.break(Alt(failErr()))
    x.toByte

  /** Parse a ULong from up to 16 bare hex digits (same rules as `xL`). */
  inline final def uxL[E >: Alt[Err]](using Label[E]): ULong =
    val x = smallHexWork(16, "hex ULong")
    if eCode != 0 then boundary.break(Alt(failErr()))
    ULong.wrap(x)

  /** Parse a UInt from up to 8 bare hex digits (same rules as `xL`). */
  inline final def uxI[E >: Alt[Err]](using Label[E]): UInt =
    val x = smallHexWork(8, "hex UInt")
    if eCode != 0 then boundary.break(Alt(failErr()))
    UInt.wrap(x.toInt)

  /** Parse a UShort from up to 4 bare hex digits (same rules as `xL`). */
  inline final def uxS[E >: Alt[Err]](using Label[E]): UShort =
    val x = smallHexWork(4, "hex UShort")
    if eCode != 0 then boundary.break(Alt(failErr()))
    UShort.wrap(x.toShort)

  /** Parse a UByte from up to 2 bare hex digits (same rules as `xL`). */
  inline final def uxB[E >: Alt[Err]](using Label[E]): UByte =
    val x = smallHexWork(2, "hex UByte")
    if eCode != 0 then boundary.break(Alt(failErr()))
    UByte.wrap(x.toByte)

  /** Read up to `n` decimal digits as a nonnegative Long, with no sign, no leading-zero
    * skimming, and no token boundary needed: the cursor stops after the `n`th digit even if
    * more follow (so fixed-width fields like `20250706` read as `digits(4)`, `digits(2)`,
    * `digits(2)`).  With `exact = true`, fewer than `n` digits is an error.  At least one
    * digit is required unless `n` is 0 (which reads nothing).
    */
  inline final def digits[E >: Alt[Err]](n: 0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18, exact: Boolean = false)(using Label[E]): Long =
    val x = digitsFieldWork(n, false, exact)
    if eCode != 0 then boundary.break(Alt(failErr()))
    x

  /** Read up to `n` bare hex digits (case-insensitive) as their Long bit pattern; same
    * cursor rules as `digits` (no token boundary needed).
    */
  inline final def hexDigits[E >: Alt[Err]](n: 0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16, exact: Boolean = false)(using Label[E]): Long =
    val x = digitsFieldWork(n, true, exact)
    if eCode != 0 then boundary.break(Alt(failErr()))
    x

  /** Read up to `n` input elements as a String, regardless of delimiters — the reader for
    * fixed-width fields, so the cursor stops after the `n`th element even if more follow.
    * With `exact = true`, having fewer than `n` elements left is an error.  At least one
    * element is required unless `n` is 0 (which reads nothing).  On byte-based sources the
    * elements are bytes, decoded as UTF-8 at the end.
    */
  inline final def chars[E >: Alt[Err]](n: Int, exact: Boolean = false)(using Label[E]): String =
    val s = charsWork(n, exact)
    if eCode != 0 then boundary.break(Alt(failErr()))
    s

  /** Parse a Double (correctly rounded; also accepts NaN, Infinity, -Infinity). */
  inline final def D[E >: Alt[Err]](using Label[E]): Double =
    val x = doubleWork(1)
    if eCode != 0 then boundary.break(Alt(failErr()))
    x

  /** Parse a Float, correctly rounded from the text (not narrowed from Double, which would
    * double-round some inputs); also accepts NaN, Infinity, -Infinity.
    */
  inline final def F[E >: Alt[Err]](using Label[E]): Float =
    val x = doubleWork(2)
    if eCode != 0 then boundary.break(Alt(failErr()))
    x.toFloat

  /** Advance past a Double without computing it; answers how many elements it occupied. */
  inline final def Dspan[E >: Alt[Err]](using Label[E]): Int =
    doubleWork(0) __ Unit
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

  /** Read a JSON-style quoted string: skip leading delimiters, require the opening `"`, decode
    * backslash escapes, and leave the cursor just past the closing quote.  The quote ends the
    * token, so no trailing delimiter is needed (`"a","b"` works with any delimiter set), and
    * anything may appear inside the quotes — including delimiters and raw control characters.
    */
  inline final def str[E >: Alt[Err]](using Label[E]): String = str(Quote.json)

  /** Read a quoted string in the given style (e.g. `Quote.csv` for `""`-doubling). */
  inline final def str[E >: Alt[Err]](q: Quote)(using Label[E]): String =
    val r = if q.doubled then strqWork(q.quote, 1) else strWork(q.quote, 1)
    if eCode != 0 then boundary.break(Alt(failErr()))
    r.asInstanceOf[String]

  /** Read a JSON-style quoted string as its characters. */
  inline final def strChars[E >: Alt[Err]](using Label[E]): Array[Char] = strChars(Quote.json)

  /** Read a quoted string in the given style, as its characters. */
  inline final def strChars[E >: Alt[Err]](q: Quote)(using Label[E]): Array[Char] =
    val r = if q.doubled then strqWork(q.quote, 3) else strWork(q.quote, 3)
    if eCode != 0 then boundary.break(Alt(failErr()))
    r.asInstanceOf[Array[Char]]

  /** Read a JSON-style quoted string as UTF-8 bytes.  On byte-based sources an escape-free
    * string is copied without ever being decoded; an escaped lone surrogate becomes U+FFFD.
    */
  inline final def strBytes[E >: Alt[Err]](using Label[E]): Array[Byte] = strBytes(Quote.json)

  /** Read a quoted string in the given style, as UTF-8 bytes. */
  inline final def strBytes[E >: Alt[Err]](q: Quote)(using Label[E]): Array[Byte] =
    val r = if q.doubled then strqWork(q.quote, 2) else strWork(q.quote, 2)
    if eCode != 0 then boundary.break(Alt(failErr()))
    r.asInstanceOf[Array[Byte]]

  /** Advance past a JSON-style quoted string without building anything; answers how many input
    * elements it occupied, quotes included.
    */
  inline final def strSpan[E >: Alt[Err]](using Label[E]): Int = strSpan(Quote.json)

  /** Advance past a quoted string in the given style; answers how many input elements it
    * occupied, quotes included.
    */
  inline final def strSpan[E >: Alt[Err]](q: Quote)(using Label[E]): Int =
    (if q.doubled then strqWork(q.quote, 0) else strWork(q.quote, 0)) __ Unit
    if eCode != 0 then boundary.break(Alt(failErr()))
    (i - vPos).toInt

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

  /** Parse a Short and check it; fail with `msg` if the test does not pass. */
  inline final def S_?[E >: Alt[Err]](inline p: Short => Boolean, msg: String = "invalid value")(using Label[E]): Short =
    val x = S
    if !p(x) then
      eCode = 2
      eWant = msg
      ePos = vPos
      boundary.break(Alt(failErr()))
    x

  /** Parse a Byte and check it; fail with `msg` if the test does not pass. */
  inline final def B_?[E >: Alt[Err]](inline p: Byte => Boolean, msg: String = "invalid value")(using Label[E]): Byte =
    val x = B
    if !p(x) then
      eCode = 2
      eWant = msg
      ePos = vPos
      boundary.break(Alt(failErr()))
    x

  /** Parse a ULong and check it; fail with `msg` if the test does not pass. */
  inline final def uL_?[E >: Alt[Err]](inline p: ULong => Boolean, msg: String = "invalid value")(using Label[E]): ULong =
    val x = uL
    if !p(x) then
      eCode = 2
      eWant = msg
      ePos = vPos
      boundary.break(Alt(failErr()))
    x

  /** Parse a UInt and check it; fail with `msg` if the test does not pass. */
  inline final def uI_?[E >: Alt[Err]](inline p: UInt => Boolean, msg: String = "invalid value")(using Label[E]): UInt =
    val x = uI
    if !p(x) then
      eCode = 2
      eWant = msg
      ePos = vPos
      boundary.break(Alt(failErr()))
    x

  /** Parse a UShort and check it; fail with `msg` if the test does not pass. */
  inline final def uS_?[E >: Alt[Err]](inline p: UShort => Boolean, msg: String = "invalid value")(using Label[E]): UShort =
    val x = uS
    if !p(x) then
      eCode = 2
      eWant = msg
      ePos = vPos
      boundary.break(Alt(failErr()))
    x

  /** Parse a UByte and check it; fail with `msg` if the test does not pass. */
  inline final def uB_?[E >: Alt[Err]](inline p: UByte => Boolean, msg: String = "invalid value")(using Label[E]): UByte =
    val x = uB
    if !p(x) then
      eCode = 2
      eWant = msg
      ePos = vPos
      boundary.break(Alt(failErr()))
    x

  /** Parse a Float and check it; fail with `msg` if the test does not pass. */
  inline final def F_?[E >: Alt[Err]](inline p: Float => Boolean, msg: String = "invalid value")(using Label[E]): Float =
    val x = F
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

  /** Parse within the next token: the block sees this same Grok narrowed to just the token's
    * extent, so it cannot read past the token, and the delimiter narrows one level (lines to
    * words to nothing).  On success the cursor moves past the whole token even if the block
    * left some of it unread (`done(value)` finishes early); on failure the error is wrapped
    * with the sub-parse's starting position as it unwinds.
    */
  inline final def grok[B, E >: Alt[Err]]()(inline f: Label[B Or Err] ?=> B)(using Label[E]): B =
    grokImpl(dlm.sub, partialMode, !skipMode)(f)

  /** Parse within the next token with the sub-parse's delimiter given explicitly — a Delim,
    * or a Char or String meaning a delimiter of exactly those characters — and optionally its
    * own `partial` and `exact` modes (inherited if unspecified).  So on a whitespace-delimited
    * Grok, `g.grok(',', exact = true){ ... }` parses the next word as comma-separated fields.
    */
  inline final def grok[B, E >: Alt[Err]](inline d: Delim | Char | String, partial: Boolean = partialMode, exact: Boolean = !skipMode)(inline f: Label[B Or Err] ?=> B)(using Label[E]): B =
    grokImpl(Delim.from(d), partial, exact)(f)

  protected inline def grokImpl[B, E >: Alt[Err]](d: Delim, partial: Boolean, exact: Boolean)(inline f: Label[B Or Err] ?=> B)(using Label[E]): B =
    var took = false
    if skipMode then skipDelimsWork() else took = sepWork()
    if i >= iZ && !took then
      eCode = 1
      eWant = "a token to parse"
      ePos = i
      boundary.break(Alt(failErr()))
    val e = tokEndWork()
    val a0 = iA
    val z0 = iZ
    val d0 = dlm
    val pm0 = partialMode
    val sm0 = skipMode
    val t0 = i
    iA = i
    iZ = e
    dlm = d
    partialMode = partial
    skipMode = !exact
    skipped = false
    sepOk = false   // the sub-parse starts fresh, like a whole parse
    val r = boundary[B Or Err]{ Is(f) }
    iA = a0
    iZ = z0
    dlm = d0
    partialMode = pm0
    skipMode = sm0
    skipped = false
    r match
      case a: Alt[Err @unchecked] => boundary.break(Alt(subErr(t0, a.alt)))
      case _ =>
        i = e
        loadWork()
        sepOk = true
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
    val so0 = sepOk
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
          sepOk = so0
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

  /** Finish the innermost parse, sub-parse, or select alternative early and successfully,
    * with `value` as its result.  (A sub-parse finished early still moves the cursor past
    * its whole token.)
    */
  inline final def done[B](value: B)(using Label[B Or Err]): Nothing =
    boundary.break(Is(value))


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

  /** Spend the separator grant (exact mode): consume at most one delimiter at the cursor;
    * answers whether one was consumed (so `tok` can tell a final empty field from no field).
    */
  @publicInBinary protected[eio] final def sepWork(): Boolean =
    if sepOk then
      sepOk = false
      if cc >= 0 && dlm(cc) then
        cWork() __ Unit
        true
      else false
    else false

  @publicInBinary protected[eio] def skipTokWork(n: Int): Unit =
    var k = n
    while k > 0 && eCode == 0 do
      var took = false
      if skipMode then skipDelimsWork() else took = sepWork()
      if i >= iZ && !took then
        eCode = 1
        eWant = "a token"
        ePos = i
      else
        i = tokEndWork()
        loadWork()
        skipped = false
        sepOk = true
        k -= 1

  /** Parse an unsigned 64-bit decimal (shared by the `u`-prefixed readers).  A leading `+` is
    * allowed, `-` is not; overflow past 2^64-1 is an error.  Orchestrates the per-source digit
    * kernel, so no per-source code is needed: the first 18 digits go in one `digitsWork` call,
    * and the at-most-two that can follow are taken one at a time with an unsigned overflow check.
    */
  @publicInBinary protected[eio] final def uLongWork(): Long =
    if skipMode then skipDelimsWork() else sepWork() __ Unit
    vPos = i
    if cc == '+' then cWork() __ Unit
    val j0 = i
    var x = digitsWork(i, cc, 18)
    if i == j0 then
      wantAt(if cc == '-' then "a nonnegative number" else "a number", i)
      0L
    else
      skipped = false
      var over = false
      if i - j0 == 18 then
        while cc >= '0' && cc <= '9' do
          val d = digitsWork(i, cc, 1)
          if x < 0 || x > 1844674407370955161L then over = true   // (2^64-1)/10: one more digit always overflows
          else if x == 1844674407370955161L && d > 5 then over = true
          else x = x * 10 + d
      if over then
        eCode = 2
        eWant = "number out of ULong range"
        ePos = vPos
        0L
      else if !partialMode && cc >= 0 && !dlm(cc) then
        wantAt("end of number", i)
        0L
      else
        sepOk = true
        x

  /** Parse a decimal that fits in `maxDigits` significant digits (shared by the readers of types
    * smaller than Long/ULong).  Also orchestrates the per-source digit kernel with no per-source
    * code: leading zeros are skimmed, then one budgeted `digitsWork` call reads the digits that
    * could possibly fit; if the stashed next character is yet another digit, the number is out of
    * `what`'s range no matter what it is, reported without reading it (so a thousand-digit input
    * cannot produce a misleading out-of-Long-range complaint from a Byte reader).  The caller
    * still range-checks the value it gets (budget 3 admits 999, which is not a Byte).
    */
  @publicInBinary protected[eio] final def smallLongWork(maxDigits: Int, what: String, signed: Boolean): Long =
    if skipMode then skipDelimsWork() else sepWork() __ Unit
    vPos = i
    if cc == '-' && !signed then
      wantAt("a nonnegative number", i)
      0L
    else
      var neg = false
      if cc == '-' then
        neg = true
        cWork() __ Unit
      else if cc == '+' then
        cWork() __ Unit
      val j0 = i
      while cc == '0' do digitsWork(i, cc, 1) __ Unit   // leading zeros are not significant
      val x = digitsWork(i, cc, maxDigits)
      if i == j0 then
        wantAt("a number", i)
        0L
      else
        skipped = false
        if cc >= '0' && cc <= '9' then
          eCode = 2
          eWant = "number out of " + what + " range"
          ePos = vPos
          0L
        else if !partialMode && cc >= 0 && !dlm(cc) then
          wantAt("end of number", i)
          0L
        else
          sepOk = true
          if neg then -x else x

  /** Hex twin of `smallLongWork`: parse a bare hexadecimal (case-insensitive, no `0x` prefix,
    * no sign) of at most `maxDigits` digits.  The hex budgets fit their types exactly (2 digits
    * per byte), so the value needs no further range check: it is the bit pattern.
    */
  @publicInBinary protected[eio] final def smallHexWork(maxDigits: Int, what: String): Long =
    if skipMode then skipDelimsWork() else sepWork() __ Unit
    vPos = i
    val j0 = i
    while cc == '0' do hexWork(i, cc, 1) __ Unit   // leading zeros are not significant
    val x = hexWork(i, cc, maxDigits)
    if i == j0 then
      wantAt("a hexadecimal number", i)
      0L
    else
      skipped = false
      if Grok.hexVal(cc) >= 0 then
        eCode = 2
        eWant = "number out of " + what + " range"
        ePos = vPos
        0L
      else if !partialMode && cc >= 0 && !dlm(cc) then
        wantAt("end of number", i)
        0L
      else
        sepOk = true
        x

  /** Fixed-field digit reader behind `digits`/`hexDigits`: up to `n` digits from the cursor,
    * no sign, no zero-skimming, no token-boundary or delimiter check afterwards.  `n == 0`
    * bypasses the kernels (they consume one char before checking the budget).
    */
  @publicInBinary protected[eio] final def digitsFieldWork(n: Int, hex: Boolean, exact: Boolean): Long =
    if skipMode then skipDelimsWork() else sepWork() __ Unit
    vPos = i
    if n == 0 then
      sepOk = true
      0L
    else
      val x = if hex then hexWork(i, cc, n) else digitsWork(i, cc, n)
      val consumed = i - vPos
      if consumed > 0 then skipped = false
      if exact && consumed != n then
        wantAt(n.toString + (if hex then " hex digits" else " digits"), i)
        0L
      else if consumed == 0 then
        wantAt(if hex then "a hexadecimal number" else "a number", i)
        0L
      else
        sepOk = true
        x


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
      sepOk = false   // manual cursor control revokes the separator grant
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
      sepOk = false   // manual cursor control revokes the separator grant
    else
      eCode = 1
      eWant = "'" + c0 + "'"
      ePos = i

  // `lit` answers code unit k of the m-unit literal in the source's own width — chars for text
  // sources, UTF-8 bytes for byte sources — so non-ASCII literals match what is actually in the
  // input; `s` is only for the error report.
  protected inline def matchTokImpl(inline at: Long => Int, inline advise: Int => Unit)(s: String, m: Int, inline lit: Int => Int): Unit =
    if skipMode then skipDelimsImpl(at) else sepWork() __ Unit
    advise(m + 1)
    var ok = iZ - i >= m
    var k = 0
    if ok && m > 0 then
      ok = cc == lit(0)
      k = 1
    while ok && k < m do
      if at(i + k) != lit(k) then ok = false
      k += 1
    if ok then
      i += m
      cc = if i < iZ then at(i) else -1
      skipped = false
      sepOk = true
    else
      eCode = 3
      eWant = "\"" + s + "\""
      ePos = i

  // === Digit-run kernel: the shared core of number scanning ===
  // Accumulates up to `budget` decimal digits POSITIVELY (callers negate as needed; doubleImpl's
  // 19-digit budget can wrap negative, which Eisel-Lemire reads as the intended u64).
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

  /** Hex twin of `digitsImpl`: shift-or accumulation with the same select-and-add digit
    * normalization as `hex4`, one char at a time (the loop exit doubles as the validity test).
    */
  protected inline def hexImpl(inline at: Long => Int)(j0: Long, c0: Int, budget: Int): Long =
    var j = j0
    var c = c0
    var x = 0L
    val jF = if iZ - j0 >= budget then j0 + budget else iZ
    boundary:
      while true do
        var y = c | 0x20
        y += (if y <= '9' then -'0' else 0) + (if y >= 'a' then -87 else 0)
        if y < 0 || y > 15 then boundary.break()
        x = (x << 4) | y
        j += 1
        if j >= jF then boundary.break()
        c = at(j)
    i = j
    cc = if j >= jF then (if j < iZ then at(j) else -1) else c
    x

  protected inline def longImpl(inline at: Long => Int, inline advise: Int => Unit): Long =
    if skipMode then skipDelimsImpl(at) else sepWork() __ Unit
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
    else
      sepOk = true
      if neg then -x else x


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
    else
      sepOk = true
      if neg then x else -x

  protected inline def zImpl(inline at: Long => Int, inline advise: Int => Unit): Boolean =
    if skipMode then skipDelimsImpl(at) else sepWork() __ Unit
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
      else
        sepOk = true
        v

  protected inline def tokImpl(inline at: Long => Int, inline sub: (Long, Long) => String): String =
    var took = false
    if skipMode then skipDelimsImpl(at) else took = sepWork()
    vPos = i
    if i >= iZ && !took then   // a just-consumed separator means a (final) empty field, not no field
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
      sepOk = true
      s

  protected inline def tokSpanImpl(inline at: Long => Int): Int =
    var took = false
    if skipMode then skipDelimsImpl(at) else took = sepWork()
    vPos = i
    if i >= iZ && !took then
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
      sepOk = true
      n

  // Fixed-width field: up to n raw elements regardless of delimiters; exact requires all n.
  protected inline def charsFieldImpl(inline at: Long => Int, inline sub: (Long, Long) => String, inline advise: Int => Unit)(n: Int, exact: Boolean): String =
    if skipMode then skipDelimsImpl(at) else sepWork() __ Unit
    vPos = i
    if n <= 0 then
      sepOk = true
      ""
    else
      advise(n)
      val j0 = i
      var j = i
      var c = cc
      while c >= 0 && j - j0 < n do
        j += 1
        c = if j < iZ then at(j) else -1
      i = j
      cc = c
      if j > j0 then skipped = false
      if j == j0 then
        wantAt("a character", j)
        ""
      else if exact && j - j0 < n then
        wantAt(n.toString + " characters", j)
        ""
      else
        sepOk = true
        sub(j0, j)

  // === Quoted-string kernels: JSON-style backslash escapes or CSV-style quote doubling ===
  // The scan runs to the closing quote ignoring delimiters entirely (raw newlines and controls
  // are content); a string with no escapes is extracted in one piece, so only escaped strings
  // pay for a builder.  mode selects the output form: 0 = validate and advance only, 1 = String,
  // 2 = UTF-8 bytes, 3 = chars.  Each kernel builds in its source's native width — UTF-16 chars
  // for char sources, UTF-8 bytes for byte sources (where a clean strBytes is a raw copy that
  // never decodes, and escaped surrogate pairs join explicitly since UTF-8 encodes the joint
  // code point) — and the cross-width output forms convert once at the end via fixed routines
  // (utf8Bytes / String's decoder), never inside the loop.  Escaped lone surrogates become
  // U+FFFD everywhere.  The cursor commits only after the closing quote; failures commit it to
  // the failure point.
  //
  // The two styles get separate kernels (str* backslash, strq* doubling) rather than one with a
  // style flag: quote doubling makes `c == q` a loop continue instead of a pure exit, and that
  // alone compiled the shared scan ~2.5x slower on clean input (measured; GrokPerformance.md).
  // The strq kernels have no escapes at all, so they are just scan-peek-blit.

  // Segment chunking and the outlined hex4 both beat the plausible alternatives only in
  // template context — the study, with rejected variants, is in GrokPerformance.md; do not
  // re-inline hex4 or switch to per-char building without re-measuring in situ.  blitOk is
  // reserved for non-retaining sources, which cannot re-read segments to blit and will have
  // to build as they scan.
  protected inline def strCharImpl(inline at: Long => Int, inline sub: (Long, Long) => String, inline cutc: (Long, Long) => Array[Char], inline blit: (Long, Long, Array[Char], Int) => Unit, inline blitOk: Boolean)(qc: Char, mode: Int): Grok.StrOut =
    if skipMode then skipDelimsImpl(at) else sepWork() __ Unit
    vPos = i
    val pv = pinWork(vPos)   // windowed sources must retain the string for blit/sub extraction
    val result: Grok.StrOut =
      if cc != qc.toInt then
        eCode = 1
        eWant = "'" + qc + "'"
        ePos = i
        null
      else if i + 1 >= iZ then        // cannot close; also licenses the unguarded at(j) reads below
        eCode = 2
        eWant = "unclosed quoted string"
        ePos = i
        null
      else
        val q = qc.toInt
        val jq = i                    // the opening quote, for unterminated reports
        var j = i + 1
        var j0 = j                    // start of the pending clean segment
        var buf = Grok.noChars
        var ib = 0
        var closed = false
        var more = true
        while more do
          val c = at(j)
          if c == q then
            closed = true
            more = false
          else if c == '\\' then
            if mode != 0 then                       // flush the segment; ensure room for it + 1
              val n = (j - j0).toInt
              if n + 1 > buf.length - ib then buf = Grok.grownChars(buf, ib, n + 1)
              blit(j0, j, buf, ib)
              ib += n
            j += 1
            if j >= iZ then
              eCode = 2
              eWant = "unclosed quoted string"
              ePos = jq
              more = false
            else
              val e = at(j)
              val x =
                if e == '"' || e == '\\' || e == '/' || e == q then e
                else if e == 'u' then -2    // unicode is often not rare: test it early
                else if e == 'n' then 10
                else if e == 't' then 9
                else if e == 'r' then 13
                else if e == 'b' then 8
                else if e == 'f' then 12
                else -1
              if x >= 0 then
                if mode != 0 then
                  buf(ib) = x.toChar                // room ensured by the flush above
                  ib += 1
                j += 1
                j0 = j
                more = j < iZ
              else if x == -2 then
                if j + 4 >= iZ then
                  eCode = 2
                  eWant = "unclosed quoted string"
                  ePos = jq
                  more = false
                else
                  val h = Grok.hex4(at(j + 1), at(j + 2), at(j + 3), at(j + 4))
                  if h < 0 then
                    var m = 0                       // cold: rediscover which digit was bad
                    while m < 4 && Grok.hexVal(at(j + 1 + m)) >= 0 do m += 1
                    eCode = 1
                    eWant = "four hexadecimal digits"
                    ePos = j + 1 + m
                    more = false
                  else
                    if mode != 0 then
                      buf(ib) = h.toChar            // room ensured by the flush above
                      ib += 1
                    j += 5
                    j0 = j
                    more = j < iZ
              else
                eCode = 1
                eWant = "a valid escape"
                ePos = j
                more = false
          else
            j += 1
            more = j < iZ
        if eCode == 0 && !closed then               // ran off the end without a closing quote
          eCode = 2
          eWant = "unclosed quoted string"
          ePos = jq
        if eCode != 0 then
          i = j
          cc = if j < iZ then at(j) else -1
          skipped = false
          null
        else
          val res: Grok.StrOut =
            if mode == 0 then null
            else if ib == 0 then                    // never built: the whole content is clean
              if mode == 2 then Grok.utf8Bytes(sub(jq + 1, j))
              else if mode == 3 then cutc(jq + 1, j)
              else sub(jq + 1, j)
            else
              val n = (j - j0).toInt                // flush the tail, exact fit
              if n > 0 then
                if n > buf.length - ib then buf = java.util.Arrays.copyOf(buf, ib + n)
                blit(j0, j, buf, ib)
                ib += n
              if mode == 2 then Grok.utf8Bytes(new String(buf, 0, ib))
              else if mode == 3 then java.util.Arrays.copyOf(buf, ib)
              else new String(buf, 0, ib)
          i = j + 1
          cc = if i < iZ then at(i) else -1
          skipped = false
          res
    if eCode == 0 then sepOk = true
    releaseWork(pv)
    result

  protected inline def strByteImpl(inline at: Long => Int, inline sub: (Long, Long) => String, inline cutb: (Long, Long) => Array[Byte], inline blitb: (Long, Long, Array[Byte], Int) => Unit, inline blitOk: Boolean)(qc: Char, mode: Int): Grok.StrOut =
    if skipMode then skipDelimsImpl(at) else sepWork() __ Unit
    vPos = i
    val pv = pinWork(vPos)   // windowed sources must retain the string for blitb/sub extraction
    val result: Grok.StrOut =
      if cc != qc.toInt then
        eCode = 1
        eWant = "'" + qc + "'"
        ePos = i
        null
      else if i + 1 >= iZ then        // cannot close; also licenses the unguarded at(j) reads below
        eCode = 2
        eWant = "unclosed quoted string"
        ePos = i
        null
      else
        val q = qc.toInt
        val jq = i                    // the opening quote, for unterminated reports
        var j = i + 1
        var j0 = j                    // start of the pending clean segment
        var buf = Grok.noBytes
        var ib = 0
        var closed = false
        var more = true
        while more do
          val c = at(j)
          if c == q then
            closed = true
            more = false
          else if c == '\\' then
            if mode != 0 then           // flush the segment; ensure room for it + one escape (up to 4 bytes)
              val n = (j - j0).toInt
              if n + 4 > buf.length - ib then buf = Grok.grownBytes(buf, ib, n + 4)
              blitb(j0, j, buf, ib)
              ib += n
            j += 1
            if j >= iZ then
              eCode = 2
              eWant = "unclosed quoted string"
              ePos = jq
              more = false
            else
              val e = at(j)
              val x =
                if e == '"' || e == '\\' || e == '/' || e == q then e
                else if e == 'u' then -2    // unicode is often not rare: test it early
                else if e == 'n' then 10
                else if e == 't' then 9
                else if e == 'r' then 13
                else if e == 'b' then 8
                else if e == 'f' then 12
                else -1
              if x >= 0 then
                if mode != 0 then
                  buf(ib) = x.toByte                // every simple escape is ASCII
                  ib += 1
                j += 1
                j0 = j
                more = j < iZ
              else if x == -2 then
                if j + 4 >= iZ then
                  eCode = 2
                  eWant = "unclosed quoted string"
                  ePos = jq
                  more = false
                else
                  val h = Grok.hex4(at(j + 1), at(j + 2), at(j + 3), at(j + 4))
                  if h < 0 then
                    var m = 0                       // cold: rediscover which digit was bad
                    while m < 4 && Grok.hexVal(at(j + 1 + m)) >= 0 do m += 1
                    eCode = 1
                    eWant = "four hexadecimal digits"
                    ePos = j + 1 + m
                    more = false
                  else
                    var cp = h
                    var adv = 5
                    // A high surrogate pairs with an immediately following low-surrogate escape
                    // (UTF-8 must encode the joint code point); anything else leaves it lone
                    // (U+FFFD via putUtf8) and unconsumed.
                    if h >= 0xD800 && h <= 0xDBFF && j + 10 < iZ && at(j + 5) == '\\' && at(j + 6) == 'u' then
                      val lo = Grok.hex4(at(j + 7), at(j + 8), at(j + 9), at(j + 10))
                      if lo >= 0xDC00 && lo <= 0xDFFF then
                        cp = 0x10000 + ((h - 0xD800) << 10) + (lo - 0xDC00)
                        adv = 11
                    if mode != 0 then ib = Grok.putUtf8(buf, ib, cp)   // room ensured by the flush above
                    j += adv
                    j0 = j
                    more = j < iZ
              else
                eCode = 1
                eWant = "a valid escape"
                ePos = j
                more = false
          else
            j += 1
            more = j < iZ
        if eCode == 0 && !closed then               // ran off the end without a closing quote
          eCode = 2
          eWant = "unclosed quoted string"
          ePos = jq
        if eCode != 0 then
          i = j
          cc = if j < iZ then at(j) else -1
          skipped = false
          null
        else
          val res: Grok.StrOut =
            if mode == 0 then null
            else if ib == 0 then                    // never built: the whole content is clean
              if mode == 2 then cutb(jq + 1, j)     // raw copy, no decode
              else if mode == 3 then sub(jq + 1, j).toCharArray
              else sub(jq + 1, j)
            else
              val n = (j - j0).toInt                // flush the tail, exact fit
              if n > 0 then
                if n > buf.length - ib then buf = java.util.Arrays.copyOf(buf, ib + n)
                blitb(j0, j, buf, ib)
                ib += n
              if mode == 2 then java.util.Arrays.copyOf(buf, ib)
              else if mode == 3 then new String(buf, 0, ib, java.nio.charset.StandardCharsets.UTF_8).toCharArray
              else new String(buf, 0, ib, java.nio.charset.StandardCharsets.UTF_8)
          i = j + 1
          cc = if i < iZ then at(i) else -1
          skipped = false
          res
    if eCode == 0 then sepOk = true
    releaseWork(pv)
    result

  // Quote-doubling kernels (CSV/SQL): no escapes exist, so the scan is just find-the-quote,
  // peek one ahead — a doubled quote flushes the segment through the first of the pair and
  // continues; anything else closes.  Only end-of-input can fail once the opening quote matched.
  // The scan itself is the per-source `seek`: next quote at or after the given position, or
  // anything >= iZ if none in view (String.indexOf on Str, Mem.whereIsFwd SWAR on arrays and
  // Mem, window-chunked SWAR on buffered sources) — element-at-a-time template loops lose ~3-8x
  // to these on long clean strings.
  protected inline def strqCharImpl(inline at: Long => Int, inline sub: (Long, Long) => String, inline cutc: (Long, Long) => Array[Char], inline blit: (Long, Long, Array[Char], Int) => Unit, inline seek: Long => Long, inline blitOk: Boolean)(qc: Char, mode: Int): Grok.StrOut =
    if skipMode then skipDelimsImpl(at) else sepWork() __ Unit
    vPos = i
    val pv = pinWork(vPos)   // windowed sources must retain the string for blit/sub extraction
    val result: Grok.StrOut =
      if cc != qc.toInt then
        eCode = 1
        eWant = "'" + qc + "'"
        ePos = i
        null
      else if i + 1 >= iZ then        // cannot close; also licenses the unguarded at(j) reads below
        eCode = 2
        eWant = "unclosed quoted string"
        ePos = i
        null
      else
        val q = qc.toInt
        val jq = i                    // the opening quote, for unterminated reports
        var j = i + 1
        var j0 = j                    // start of the pending clean segment
        var buf = Grok.noChars
        var ib = 0
        var closed = false
        var more = true
        while more do
          j = seek(j)                                 // next quote, or >= iZ if none in view
          if j >= iZ then more = false                // unclosed
          else if j + 1 < iZ && at(j + 1) == q then   // doubled quote: keep one, carry on
            if mode != 0 then
              val n = (j + 1 - j0).toInt
              if n > buf.length - ib then buf = Grok.grownChars(buf, ib, n)
              blit(j0, j + 1, buf, ib)
              ib += n
            j += 2
            j0 = j
          else
            closed = true
            more = false
        if !closed then               // ran off the end without a closing quote
          eCode = 2
          eWant = "unclosed quoted string"
          ePos = jq
          i = iZ
          cc = -1
          skipped = false
          null
        else
          val res: Grok.StrOut =
            if mode == 0 then null
            else if ib == 0 then                    // never built: the whole content is clean
              if mode == 2 then Grok.utf8Bytes(sub(jq + 1, j))
              else if mode == 3 then cutc(jq + 1, j)
              else sub(jq + 1, j)
            else
              val n = (j - j0).toInt                // flush the tail, exact fit
              if n > 0 then
                if n > buf.length - ib then buf = java.util.Arrays.copyOf(buf, ib + n)
                blit(j0, j, buf, ib)
                ib += n
              if mode == 2 then Grok.utf8Bytes(new String(buf, 0, ib))
              else if mode == 3 then java.util.Arrays.copyOf(buf, ib)
              else new String(buf, 0, ib)
          i = j + 1
          cc = if i < iZ then at(i) else -1
          skipped = false
          res
    if eCode == 0 then sepOk = true
    releaseWork(pv)
    result

  protected inline def strqByteImpl(inline at: Long => Int, inline sub: (Long, Long) => String, inline cutb: (Long, Long) => Array[Byte], inline blitb: (Long, Long, Array[Byte], Int) => Unit, inline seek: Long => Long, inline blitOk: Boolean)(qc: Char, mode: Int): Grok.StrOut =
    if skipMode then skipDelimsImpl(at) else sepWork() __ Unit
    vPos = i
    val pv = pinWork(vPos)   // windowed sources must retain the string for blitb/sub extraction
    val result: Grok.StrOut =
      if cc != qc.toInt then
        eCode = 1
        eWant = "'" + qc + "'"
        ePos = i
        null
      else if i + 1 >= iZ then        // cannot close; also licenses the unguarded at(j) reads below
        eCode = 2
        eWant = "unclosed quoted string"
        ePos = i
        null
      else
        val q = qc.toInt
        val jq = i                    // the opening quote, for unterminated reports
        var j = i + 1
        var j0 = j                    // start of the pending clean segment
        var buf = Grok.noBytes
        var ib = 0
        var closed = false
        var more = true
        while more do
          j = seek(j)                                 // next quote, or >= iZ if none in view
          if j >= iZ then more = false                // unclosed
          else if j + 1 < iZ && at(j + 1) == q then   // doubled quote: keep one, carry on
            if mode != 0 then
              val n = (j + 1 - j0).toInt
              if n > buf.length - ib then buf = Grok.grownBytes(buf, ib, n)
              blitb(j0, j + 1, buf, ib)
              ib += n
            j += 2
            j0 = j
          else
            closed = true
            more = false
        if !closed then               // ran off the end without a closing quote
          eCode = 2
          eWant = "unclosed quoted string"
          ePos = jq
          i = iZ
          cc = -1
          skipped = false
          null
        else
          val res: Grok.StrOut =
            if mode == 0 then null
            else if ib == 0 then                    // never built: the whole content is clean
              if mode == 2 then cutb(jq + 1, j)     // raw copy, no decode
              else if mode == 3 then sub(jq + 1, j).toCharArray
              else sub(jq + 1, j)
            else
              val n = (j - j0).toInt                // flush the tail, exact fit
              if n > 0 then
                if n > buf.length - ib then buf = java.util.Arrays.copyOf(buf, ib + n)
                blitb(j0, j, buf, ib)
                ib += n
              if mode == 2 then java.util.Arrays.copyOf(buf, ib)
              else if mode == 3 then new String(buf, 0, ib, java.nio.charset.StandardCharsets.UTF_8).toCharArray
              else new String(buf, 0, ib, java.nio.charset.StandardCharsets.UTF_8)
          i = j + 1
          cc = if i < iZ then at(i) else -1
          skipped = false
          res
    if eCode == 0 then sepOk = true
    releaseWork(pv)
    result

  // cc-threaded throughout (each char is read once), significant digits gathered by the
  // digitsWork kernel, and a single exit so windowed sources can pin vPos across the kernel's
  // eager cursor commits (the slow path re-reads [vPos, end) at the very end)
  protected inline def doubleImpl(inline at: Long => Int, inline sub: (Long, Long) => String, inline advise: Int => Unit)(mode: Int): Double =
    if skipMode then skipDelimsImpl(at) else sepWork() __ Unit
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
          mant = digitsWork(j, c, 19)
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
          if nd < 19 && c >= '0' && c <= '9' then
            val v = digitsWork(j, c, 19 - nd)
            val fd = (i - j).toInt
            mant = mant * Grok.pow10L(fd) + v   // nd + fd <= 19: may wrap negative, but only into u64 space, which Eisel-Lemire reads
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
          else if mode == 0 then 0.0
          else if mant == 0 then
            if neg then -0.0 else 0.0
          else if mode == 2 then
            // The kernel punts (NaN) whenever narrowing could go wrong (subnormal Floats,
            // narrowing midpoints, undecided Doubles); a truncated mantissa (>19 digits) is
            // trusted only when one ulp of slop agrees, since the value is in [mant, mant+1) *
            // 10^e10.  NaN punts compare unequal, so both tests fail into the JDK together.
            val v = EiselLemire.toFloat(ULong.wrap(mant), e10)
            val ok = if truncated then v == EiselLemire.toFloat(ULong.wrap(mant + 1), e10) else v == v
            if ok then (if neg then -v else v).toDouble
            else java.lang.Float.parseFloat(sub(vPos, j)).toDouble
          else
            val v = EiselLemire.toDouble(ULong.wrap(mant), e10)
            val ok = if truncated then v == EiselLemire.toDouble(ULong.wrap(mant + 1), e10) else v == v
            if ok then (if neg then -v else v)
            else java.lang.Double.parseDouble(sub(vPos, j))
    if eCode == 0 then sepOk = true
    releaseWork(pv)
    result
}


object Grok {

  /** What the quoted-string worker produces: one of the three requested output forms, or null
    * when only advancing (mode 0) or on error.
    */
  type StrOut = String | Array[Byte] | Array[Char] | Null

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

  @publicInBinary private[eio] val pow10L: Array[Long] = Array(
    1L, 10L, 100L, 1000L, 10000L, 100000L, 1000000L, 10000000L, 100000000L, 1000000000L,
    10000000000L, 100000000000L, 1000000000000L, 10000000000000L, 100000000000000L,
    1000000000000000L, 10000000000000000L, 100000000000000000L, 1000000000000000000L,
    0x8AC7230489E80000L   // 10^19 as u64: indexed only when the integer part was empty, so the mantissa it scales is 0
  )

  @publicInBinary private[eio] val declinedAlt: Alt[Err] = Alt(Err("alternative declined"))

  private[eio] val noChars = new Array[Char](0)   // shared builder sentinel; grown via copyOf before any write

  private[eio] val noBytes = new Array[Byte](0)   // shared builder sentinel; grown via copyOf before any write

  // Grow so that at least `need` bytes fit above `ib`: double, with a floor of 8.
  private[eio] def grownBytes(buf: Array[Byte], ib: Int, need: Int): Array[Byte] =
    var cap = buf.length
    while need > cap - ib do cap = (cap << 1) | 8
    java.util.Arrays.copyOf(buf, cap)

  /** Encode a code point as UTF-8 at `buf(ib)`; answers the next index.  A lone surrogate
    * becomes U+FFFD.  The caller must ensure four bytes of room.
    */
  private[eio] def putUtf8(buf: Array[Byte], ib: Int, cp0: Int): Int =
    val cp = if cp0 >= 0xD800 && cp0 <= 0xDFFF then 0xFFFD else cp0
    if cp < 0x80 then
      buf(ib) = cp.toByte
      ib + 1
    else if cp < 0x800 then
      buf(ib) = (0xC0 | (cp >> 6)).toByte
      buf(ib + 1) = (0x80 | (cp & 0x3F)).toByte
      ib + 2
    else if cp < 0x10000 then
      buf(ib) = (0xE0 | (cp >> 12)).toByte
      buf(ib + 1) = (0x80 | ((cp >> 6) & 0x3F)).toByte
      buf(ib + 2) = (0x80 | (cp & 0x3F)).toByte
      ib + 3
    else
      buf(ib) = (0xF0 | (cp >> 18)).toByte
      buf(ib + 1) = (0x80 | ((cp >> 12) & 0x3F)).toByte
      buf(ib + 2) = (0x80 | ((cp >> 6) & 0x3F)).toByte
      buf(ib + 3) = (0x80 | (cp & 0x3F)).toByte
      ib + 4

  // Grow so that at least `need` chars fit above `ib`: double, with a floor of 8.
  private[eio] def grownChars(buf: Array[Char], ib: Int, need: Int): Array[Char] =
    var cap = buf.length
    while need > cap - ib do cap = (cap << 1) | 8
    java.util.Arrays.copyOf(buf, cap)

  /** Combine the four chars of a `\uXXXX` escape into a code point, or -1 if any is not hex.
    * Select-and-add normalization; deliberately a plain routine rather than template-inlined,
    * so the JIT compiles it as its own unit instead of entangling it with the caller's loop.
    */
  private[eio] def hex4(c0: Int, c1: Int, c2: Int, c3: Int): Int =
    var y = c0 | 0x20
    y += (if y <= '9' then -'0' else 0) + (if y >= 'a' then -87 else 0)
    var v = y
    var h = y << 12
    y = c1 | 0x20
    y += (if y <= '9' then -'0' else 0) + (if y >= 'a' then -87 else 0)
    v |= y
    h |= y << 8
    y = c2 | 0x20
    y += (if y <= '9' then -'0' else 0) + (if y >= 'a' then -87 else 0)
    v |= y
    h |= y << 4
    y = c3 | 0x20
    y += (if y <= '9' then -'0' else 0) + (if y >= 'a' then -87 else 0)
    v |= y
    if v < 0 || v > 15 then -1 else h | y

  // ASCII literals match byte sources unit for unit; anything else must be UTF-8 encoded first.
  private[eio] def isAscii(s: String): Boolean =
    var k = 0
    while k < s.length && s.charAt(k) < 128 do k += 1
    k >= s.length

  private[eio] def hexVal(c: Int): Int =
    if c >= '0' && c <= '9' then c - '0'
    else
      val l = c | 0x20
      if l >= 'a' && l <= 'f' then l - 'a' + 10 else -1

  // Encoded by hand so a lone surrogate becomes U+FFFD as on byte sources (String.getBytes uses '?')
  private[eio] def utf8Bytes(s: String): Array[Byte] =
    val buf = new Array[Byte](3 * s.length)   // 3/char covers everything: pairs are 2 chars -> 4 bytes
    var ib = 0
    var k = 0
    while k < s.length do
      val c = s.charAt(k)
      if Character.isHighSurrogate(c) && k + 1 < s.length && Character.isLowSurrogate(s.charAt(k + 1)) then
        ib = putUtf8(buf, ib, Character.toCodePoint(c, s.charAt(k + 1)))
        k += 2
      else
        ib = putUtf8(buf, ib, c)
        k += 1
    java.util.Arrays.copyOf(buf, ib)


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
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit = matchTokImpl(j => content.charAt(j.toInt), _ => ())(s, s.length, k => s.charAt(k).toInt)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => content.charAt(j.toInt), _ => ())

    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => content.charAt(j.toInt))(x0, neg)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => content.charAt(j.toInt))(j0, c0, budget)
    @publicInBinary protected[eio] def hexWork(j0: Long, c0: Int, budget: Int): Long = hexImpl(j => content.charAt(j.toInt))(j0, c0, budget)
    @publicInBinary protected[eio] def doubleWork(mode: Int): Double = doubleImpl(j => content.charAt(j.toInt), (a, b) => content.substring(a.toInt, b.toInt), _ => ())(mode)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => content.charAt(j.toInt), _ => ())
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => content.charAt(j.toInt), (a, b) => content.substring(a.toInt, b.toInt))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => content.charAt(j.toInt))
    @publicInBinary protected[eio] def charsWork(n: Int, exact: Boolean): String = charsFieldImpl(j => content.charAt(j.toInt), (a, b) => content.substring(a.toInt, b.toInt), _ => ())(n, exact)
    @publicInBinary protected[eio] def strWork(qc: Char, mode: Int): Grok.StrOut =
      strCharImpl(
        j => content.charAt(j.toInt),
        (a, b) => content.substring(a.toInt, b.toInt),
        (a, b) => { val arr = new Array[Char]((b - a).toInt); content.getChars(a.toInt, b.toInt, arr, 0); arr },
        (a, b, arr, off) => content.getChars(a.toInt, b.toInt, arr, off),
        true
      )(qc, mode)
    @publicInBinary protected[eio] def strqWork(qc: Char, mode: Int): Grok.StrOut =
      strqCharImpl(
        j => content.charAt(j.toInt),
        (a, b) => content.substring(a.toInt, b.toInt),
        (a, b) => { val arr = new Array[Char]((b - a).toInt); content.getChars(a.toInt, b.toInt, arr, 0); arr },
        (a, b, arr, off) => content.getChars(a.toInt, b.toInt, arr, off),
        j => { val k = content.indexOf(qc.toInt, j.toInt); if k < 0 then iZ else k.toLong },
        true
      )(qc, mode)
  }

  /** Set up a direct-mode parse of `content`, which may be a `String`, `Array[Char]`,
    * `Array[Byte]`, or `Mem[Byte]`; byte-based sources read structure (delimiters, numbers,
    * literals) as ASCII and decode tokens as UTF-8.  Answers the source-appropriate Grok,
    * normally applied immediately to a parsing block: `Grok(content, ...){ g => ... }` gives
    * an `Ask` of the block's result, with any failure as a data-bearing `Err`.
    *
    * `delim` sets the token delimiter (default: line ends).  `partial = true` lets numbers and
    * booleans stop at the first character that cannot continue them, rather than requiring a
    * delimiter.  `exact = true` switches from delimiter skipping to separator semantics: each
    * token-level read may consume at most one leading delimiter, permitted by the completion
    * of the previous read, so empty tokens between adjacent delimiters are preserved.
    */
  transparent inline def apply(inline content: String | Array[Char] | Array[Byte] | Mem[Byte], inline delim: Delim | Char | String = Delim.lines, partial: Boolean = false, exact: Boolean = false): Grok =
    inline content match
      case s: String       => new Str(s, Delim.from(delim), partial, !exact)
      case c: Array[Char]  => new Chars(c, Delim.from(delim), partial, !exact)
      case b: Array[Byte]  => new Bytes(b, Delim.from(delim), partial, !exact)
      case m: Mem[Byte]    => new MemBytes(m, Delim.from(delim), partial, !exact)

  /** Covering parse of `content`: exactly `Grok(content, ...){ g => ... }`, plus a check that
    * the block consumed every element of the input, so trailing unparsed input is an error
    * rather than silently ignored.  `Grok.all(s)(_.I)` is thus a whole-string integer parse.
    */
  inline def all[A](inline content: String | Array[Char] | Array[Byte] | Mem[Byte], inline delim: Delim | Char | String = Delim.lines, partial: Boolean = false, exact: Boolean = false)(inline f: Label[A Or Err] ?=> Grok => A): Ask[A] =
    val g = apply(content, delim, partial, exact)
    Or.Nice:
      val a = f(g)
      g.end
      a


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
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit =
      if Grok.isAscii(s) then matchTokImpl(j => content(j.toInt) & 0xFF, _ => ())(s, s.length, k => s.charAt(k).toInt)
      else
        val b = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        matchTokImpl(j => content(j.toInt) & 0xFF, _ => ())(s, b.length, k => b(k) & 0xFF)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => content(j.toInt) & 0xFF, _ => ())
    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => content(j.toInt) & 0xFF)(x0, neg)
    @publicInBinary protected[eio] def doubleWork(mode: Int): Double = doubleImpl(j => content(j.toInt) & 0xFF, (a, b) => utf8(a, b), _ => ())(mode)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => content(j.toInt) & 0xFF, _ => ())
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => content(j.toInt) & 0xFF, (a, b) => utf8(a, b))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => content(j.toInt) & 0xFF)
    @publicInBinary protected[eio] def charsWork(n: Int, exact: Boolean): String = charsFieldImpl(j => content(j.toInt) & 0xFF, (a, b) => utf8(a, b), _ => ())(n, exact)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => content(j.toInt) & 0xFF)(j0, c0, budget)
    @publicInBinary protected[eio] def hexWork(j0: Long, c0: Int, budget: Int): Long = hexImpl(j => content(j.toInt) & 0xFF)(j0, c0, budget)
    @publicInBinary protected[eio] def strWork(qc: Char, mode: Int): Grok.StrOut =
      strByteImpl(
        j => content(j.toInt) & 0xFF,
        (a, b) => utf8(a, b),
        (a, b) => java.util.Arrays.copyOfRange(content, a.toInt, b.toInt),
        (a, b, arr, off) => System.arraycopy(content, a.toInt, arr, off, (b - a).toInt),
        true
      )(qc, mode)
    private var memc: Mem[Byte] = null.asInstanceOf[Mem[Byte]]   // wraps content for SWAR seeks; made on first strq use

    @publicInBinary protected[eio] def strqWork(qc: Char, mode: Int): Grok.StrOut =
      if memc.asInstanceOf[AnyRef] eq null then memc = Mem of content
      strqByteImpl(
        j => content(j.toInt) & 0xFF,
        (a, b) => utf8(a, b),
        (a, b) => java.util.Arrays.copyOfRange(content, a.toInt, b.toInt),
        (a, b, arr, off) => System.arraycopy(content, a.toInt, arr, off, (b - a).toInt),
        j => { val k = memc.whereIsFwd(j, iZ)(qc.toByte); if k < 0 then iZ else k },
        true
      )(qc, mode)
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
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit = matchTokImpl(j => content(j.toInt), _ => ())(s, s.length, k => s.charAt(k).toInt)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => content(j.toInt), _ => ())
    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => content(j.toInt))(x0, neg)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => content(j.toInt))(j0, c0, budget)
    @publicInBinary protected[eio] def hexWork(j0: Long, c0: Int, budget: Int): Long = hexImpl(j => content(j.toInt))(j0, c0, budget)
    @publicInBinary protected[eio] def doubleWork(mode: Int): Double = doubleImpl(j => content(j.toInt), (a, b) => new String(content, a.toInt, (b - a).toInt), _ => ())(mode)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => content(j.toInt), _ => ())
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => content(j.toInt), (a, b) => new String(content, a.toInt, (b - a).toInt))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => content(j.toInt))
    @publicInBinary protected[eio] def charsWork(n: Int, exact: Boolean): String = charsFieldImpl(j => content(j.toInt), (a, b) => new String(content, a.toInt, (b - a).toInt), _ => ())(n, exact)
    @publicInBinary protected[eio] def strWork(qc: Char, mode: Int): Grok.StrOut =
      strCharImpl(
        j => content(j.toInt),
        (a, b) => new String(content, a.toInt, (b - a).toInt),
        (a, b) => java.util.Arrays.copyOfRange(content, a.toInt, b.toInt),
        (a, b, arr, off) => System.arraycopy(content, a.toInt, arr, off, (b - a).toInt),
        true
      )(qc, mode)
    private var memc: Mem[Char] = null.asInstanceOf[Mem[Char]]   // wraps content for SWAR seeks; made on first strq use

    @publicInBinary protected[eio] def strqWork(qc: Char, mode: Int): Grok.StrOut =
      if memc.asInstanceOf[AnyRef] eq null then memc = Mem of content
      strqCharImpl(
        j => content(j.toInt),
        (a, b) => new String(content, a.toInt, (b - a).toInt),
        (a, b) => java.util.Arrays.copyOfRange(content, a.toInt, b.toInt),
        (a, b, arr, off) => System.arraycopy(content, a.toInt, arr, off, (b - a).toInt),
        j => { val k = memc.whereIsFwd(j, iZ)(qc); if k < 0 then iZ else k },
        true
      )(qc, mode)
  }

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

    private def cut(a: Long, b: Long): Array[Byte] =
      val arr = new Array[Byte]((b - a).toInt)
      content.inject(arr)(a, b) __ Unit
      arr

    private def utf8(a: Long, b: Long): String = new String(cut(a, b), java.nio.charset.StandardCharsets.UTF_8)

    @publicInBinary protected[eio] def skipDelimsWork(): Unit = skipDelimsImpl(j => content(j) & 0xFF)
    @publicInBinary protected[eio] def tokEndWork(): Long = tokEndImpl(j => content(j) & 0xFF)
    @publicInBinary protected[eio] def loadWork(): Unit = loadImpl(j => content(j) & 0xFF)
    @publicInBinary protected[eio] def cWork(): Char = cImpl(j => content(j) & 0xFF)
    @publicInBinary protected[eio] def matchCWork(c: Char): Unit = matchCImpl(j => content(j) & 0xFF)(c)
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit =
      if Grok.isAscii(s) then matchTokImpl(j => content(j) & 0xFF, _ => ())(s, s.length, k => s.charAt(k).toInt)
      else
        val b = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        matchTokImpl(j => content(j) & 0xFF, _ => ())(s, b.length, k => b(k) & 0xFF)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => content(j) & 0xFF, _ => ())
    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => content(j) & 0xFF)(x0, neg)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => content(j) & 0xFF)(j0, c0, budget)
    @publicInBinary protected[eio] def hexWork(j0: Long, c0: Int, budget: Int): Long = hexImpl(j => content(j) & 0xFF)(j0, c0, budget)
    @publicInBinary protected[eio] def doubleWork(mode: Int): Double = doubleImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b), _ => ())(mode)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => content(j) & 0xFF, _ => ())
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => content(j) & 0xFF)
    @publicInBinary protected[eio] def charsWork(n: Int, exact: Boolean): String = charsFieldImpl(j => content(j) & 0xFF, (a, b) => utf8(a, b), _ => ())(n, exact)
    @publicInBinary protected[eio] def strWork(qc: Char, mode: Int): Grok.StrOut =
      strByteImpl(
        j => content(j) & 0xFF,
        (a, b) => utf8(a, b),
        (a, b) => cut(a, b),
        (a, b, arr, off) => content.inject(arr, off)(a, b) __ Unit,
        true
      )(qc, mode)
    @publicInBinary protected[eio] def strqWork(qc: Char, mode: Int): Grok.StrOut =
      strqByteImpl(
        j => content(j) & 0xFF,
        (a, b) => utf8(a, b),
        (a, b) => cut(a, b),
        (a, b, arr, off) => content.inject(arr, off)(a, b) __ Unit,
        j => { val k = content.whereIsFwd(j, iZ)(qc.toByte); if k < 0 then iZ else k },
        true
      )(qc, mode)
  }

  /** Groks bytes pulled on demand through a sliding window, using the ordinary indexed worker
    * templates.  `index - discard` addresses the window; a read past the loaded end (or an
    * `advise(n)` that does not fit) scoots the window forward and refills from the pull
    * function.  The window retains everything from the current op's start — which is all the
    * templates ever re-read — or from the earliest live `select` mark, whose alternatives may
    * need re-reading; when retention leaves no room to advance, the window doubles.
    * `fill(dst, off, max)` answers how many bytes it wrote, 0 for none right now, or a negative
    * number once the input has ended.  Pass the total input length as `totalLen` if it is known
    * up front, or any negative number if it is not: the end of input is then wherever `fill`
    * first answers negative.  Error excerpts degrade to `?` behind the window.
    */
  final class Buffered(fill: (Array[Byte], Int, Int) => Int, totalLen: Long, d: Delim, partial: Boolean, autoskip: Boolean, window: Int = 64) extends Grok {
    dlm = d
    partialMode = partial
    skipMode = autoskip

    private var buf = new Array[Byte](if window < 8 then 8 else window)
    private var discard = 0L   // absolute position of buf(0)
    private var loaded = 0     // bytes of buf currently valid
    private var pinned = -1L   // earliest select mark to retain, or -1 for none
    private var srcEnd = if totalLen < 0 then Long.MaxValue else totalLen   // where input truly ends; found via fill if not known up front
    iZ = srcEnd
    refill()
    cc = if loaded > 0 then buf(0) & 0xFF else -1

    protected def rawLength: Long = if srcEnd == Long.MaxValue then discard + loaded else srcEnd
    protected def rawCharAt(pos: Long): Char =
      val d = (pos - discard).toInt
      if 0 <= d && d < loaded then (buf(d) & 0xFF).toChar else '?'   // cold path: excerpts degrade outside the window

    private def refill(): Unit =
      var more = discard + loaded < srcEnd
      while more && loaded < buf.length do
        val n = fill(buf, loaded, buf.length - loaded)
        if n > 0 then loaded += n
        else
          if n < 0 then            // definitive end of input: the length is now known exactly
            srcEnd = discard + loaded
            if iZ > srcEnd then iZ = srcEnd
          more = false

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

    // A view restore (select, sub-parse) can re-widen iZ past a discovered end; re-clamp on the
    // next past-end read so the templates' iZ guards stay truthful.
    private def pastEnd(): Int =
      if iZ > srcEnd then iZ = srcEnd
      -1

    private def fetched(j: Long): Int =
      if j >= srcEnd then pastEnd()
      else
        scoot()
        val d = (j - discard).toInt
        if 0 <= d && d < loaded then buf(d) & 0xFF
        else if j >= srcEnd then pastEnd()
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
      if i + n > discard + loaded && discard + loaded < srcEnd then scoot()

    private def utf8(a: Long, b: Long): String = new String(buf, (a - discard).toInt, (b - a).toInt, java.nio.charset.StandardCharsets.UTF_8)

    @publicInBinary protected[eio] def skipDelimsWork(): Unit = skipDelimsImpl(j => atc(j))
    @publicInBinary protected[eio] def tokEndWork(): Long = tokEndImpl(j => atc(j))
    @publicInBinary protected[eio] def loadWork(): Unit = loadImpl(j => atc(j))
    @publicInBinary protected[eio] def cWork(): Char = cImpl(j => atc(j))
    @publicInBinary protected[eio] def matchCWork(c: Char): Unit = matchCImpl(j => atc(j))(c)
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit =
      if Grok.isAscii(s) then matchTokImpl(j => atc(j), n => advc(n))(s, s.length, k => s.charAt(k).toInt)
      else
        val b = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        matchTokImpl(j => atc(j), n => advc(n))(s, b.length, k => b(k) & 0xFF)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => atc(j), n => advc(n))
    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => atc(j))(x0, neg)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => atc(j))(j0, c0, budget)
    @publicInBinary protected[eio] def hexWork(j0: Long, c0: Int, budget: Int): Long = hexImpl(j => atc(j))(j0, c0, budget)
    @publicInBinary protected[eio] def doubleWork(mode: Int): Double = doubleImpl(j => atc(j), (a, b) => utf8(a, b), n => advc(n))(mode)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => atc(j), n => advc(n))
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => atc(j), (a, b) => utf8(a, b))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => atc(j))
    @publicInBinary protected[eio] def charsWork(n: Int, exact: Boolean): String = charsFieldImpl(j => atc(j), (a, b) => utf8(a, b), m => advc(m))(n, exact)
    @publicInBinary protected[eio] def strWork(qc: Char, mode: Int): Grok.StrOut =
      strByteImpl(
        j => atc(j),
        (a, b) => utf8(a, b),
        (a, b) => java.util.Arrays.copyOfRange(buf, (a - discard).toInt, (b - discard).toInt),
        (a, b, arr, off) => System.arraycopy(buf, (a - discard).toInt, arr, off, (b - a).toInt),
        true
      )(qc, mode)
    private var memb: Mem[Byte] = null.asInstanceOf[Mem[Byte]]   // wraps buf for SWAR seeks; remade when buf regrows
    private var membOf: Array[Byte] = null

    // strq's quote scan: SWAR across the loaded window, refilling through atc between windows
    private def seekq(j: Long, q: Byte): Long =
      var k = j
      var going = true
      while going && k < iZ do
        val d = k - discard
        val lim = { val w = iZ - discard; if w < loaded then w else loaded.toLong }
        if d >= 0 && d < lim then
          if membOf ne buf then
            memb = Mem of buf
            membOf = buf
          val f = memb.whereIsFwd(d, lim)(q)
          if f >= 0 then
            k = discard + f
            going = false
          else k = discard + lim
        else if atc(k) == (q & 0xFF) then going = false
        else k += 1
      k

    @publicInBinary protected[eio] def strqWork(qc: Char, mode: Int): Grok.StrOut =
      strqByteImpl(
        j => atc(j),
        (a, b) => utf8(a, b),
        (a, b) => java.util.Arrays.copyOfRange(buf, (a - discard).toInt, (b - discard).toInt),
        (a, b, arr, off) => System.arraycopy(buf, (a - discard).toInt, arr, off, (b - a).toInt),
        j => seekq(j, qc.toByte),
        true
      )(qc, mode)
  }

  /** Set up to parse content pulled through a sliding window that starts at `window` elements
    * (doubling whenever retention needs more room).  The source may be an `Array[Byte]`
    * (mostly useful for testing the windowed machinery against direct indexing — prefer
    * `Grok(bytes)` when the whole array is at hand), a `ByteBuffer` or `CharBuffer` (remaining
    * contents, consumed via relative bulk `get`), or an `InputStream` or `Reader` (read as
    * needed but never closed; the length need not be known — the end of input is wherever
    * `read` answers -1).  Parameters and the parsing block are otherwise as for `Grok(...)`;
    * error excerpts degrade to `?` behind the window.
    */
  transparent inline def buffered(inline content: Array[Byte] | java.nio.ByteBuffer | java.io.InputStream | java.nio.CharBuffer | java.io.Reader, inline delim: Delim | Char | String = Delim.lines, partial: Boolean = false, exact: Boolean = false, window: Int = 64): Grok =
    inline content match
      case a: Array[Byte] =>
        var pos = 0
        val fill = (dst: Array[Byte], off: Int, max: Int) => {
          val n = if a.length - pos < max then a.length - pos else max
          if n > 0 then System.arraycopy(a, pos, dst, off, n)
          pos += n
          n
        }
        new Buffered(fill, a.length, Delim.from(delim), partial, !exact, window)
      case bb: java.nio.ByteBuffer =>
        val fill = (dst: Array[Byte], off: Int, max: Int) => {
          val n = if bb.remaining() < max then bb.remaining() else max
          bb.get(dst, off, n) __ Unit
          n
        }
        new Buffered(fill, bb.remaining(), Delim.from(delim), partial, !exact, window)
      case in: java.io.InputStream =>
        new Buffered((dst, off, max) => in.read(dst, off, max), -1L, Delim.from(delim), partial, !exact, window)
      case cb: java.nio.CharBuffer =>
        val fill = (dst: Array[Char], off: Int, max: Int) => {
          val n = if cb.remaining() < max then cb.remaining() else max
          cb.get(dst, off, n) __ Unit
          n
        }
        new BufferedChars(fill, cb.remaining(), Delim.from(delim), partial, !exact, window)
      case rd: java.io.Reader =>
        new BufferedChars((dst, off, max) => rd.read(dst, off, max), -1L, Delim.from(delim), partial, !exact, window)


  /** Groks chars pulled on demand through a sliding window; the char-flavored twin of
    * `Buffered`, with the same retention discipline (everything from the current op's start,
    * or the earliest live pin, stays available — which is what lets the quoted-string kernel
    * bulk-copy segments out of the window).  `fill(dst, off, max)` answers how many chars it
    * wrote, 0 for none right now, or a negative number once the input has ended.  Pass the
    * total input length as `totalLen` if it is known up front, or any negative number if it is
    * not: the end of input is then wherever `fill` first answers negative.
    */
  final class BufferedChars(fill: (Array[Char], Int, Int) => Int, totalLen: Long, d: Delim, partial: Boolean, autoskip: Boolean, window: Int = 64) extends Grok {
    dlm = d
    partialMode = partial
    skipMode = autoskip

    private var buf = new Array[Char](if window < 8 then 8 else window)
    private var discard = 0L   // absolute position of buf(0)
    private var loaded = 0     // chars of buf currently valid
    private var pinned = -1L   // earliest pin to retain, or -1 for none
    private var srcEnd = if totalLen < 0 then Long.MaxValue else totalLen   // where input truly ends; found via fill if not known up front
    iZ = srcEnd
    refill()
    cc = if loaded > 0 then buf(0) else -1

    protected def rawLength: Long = if srcEnd == Long.MaxValue then discard + loaded else srcEnd
    protected def rawCharAt(pos: Long): Char =
      val d = (pos - discard).toInt
      if 0 <= d && d < loaded then buf(d) else '?'   // cold path: excerpts degrade outside the window

    private def refill(): Unit =
      var more = discard + loaded < srcEnd
      while more && loaded < buf.length do
        val n = fill(buf, loaded, buf.length - loaded)
        if n > 0 then loaded += n
        else
          if n < 0 then            // definitive end of input: the length is now known exactly
            srcEnd = discard + loaded
            if iZ > srcEnd then iZ = srcEnd
          more = false

    private def scoot(): Unit =
      val from = if 0 <= pinned && pinned < i then pinned else i
      val keep = (from - discard).toInt
      if keep < 0 then throw new IllegalStateException("Grok.BufferedChars: backtrack to " + from + " behind the window at " + discard)
      if keep > 0 then
        System.arraycopy(buf, keep, buf, 0, loaded - keep)
        discard += keep
        loaded -= keep
      if loaded == buf.length then buf = java.util.Arrays.copyOf(buf, buf.length << 1)
      refill()

    // A view restore (select, sub-parse) can re-widen iZ past a discovered end; re-clamp on the
    // next past-end read so the templates' iZ guards stay truthful.
    private def pastEnd(): Int =
      if iZ > srcEnd then iZ = srcEnd
      -1

    private def fetched(j: Long): Int =
      if j >= srcEnd then pastEnd()
      else
        scoot()
        val d = (j - discard).toInt
        if 0 <= d && d < loaded then buf(d)
        else if j >= srcEnd then pastEnd()
        else throw new IllegalStateException("Grok.BufferedChars: cannot load position " + j + " (window at " + discard + ", " + loaded + " loaded)")

    @publicInBinary protected[eio] override def pinWork(pos: Long): Long =
      val prev = pinned
      if prev < 0 || pos < prev then pinned = pos
      prev

    @publicInBinary protected[eio] override def releaseWork(token: Long): Unit = pinned = token

    private inline def atc(j: Long): Int =
      val d = (j - discard).toInt
      if 0 <= d && d < loaded then buf(d)
      else fetched(j)

    private inline def advc(n: Int): Unit =
      if i + n > discard + loaded && discard + loaded < srcEnd then scoot()

    private def sub(a: Long, b: Long): String = new String(buf, (a - discard).toInt, (b - a).toInt)

    @publicInBinary protected[eio] def skipDelimsWork(): Unit = skipDelimsImpl(j => atc(j))
    @publicInBinary protected[eio] def tokEndWork(): Long = tokEndImpl(j => atc(j))
    @publicInBinary protected[eio] def loadWork(): Unit = loadImpl(j => atc(j))
    @publicInBinary protected[eio] def cWork(): Char = cImpl(j => atc(j))
    @publicInBinary protected[eio] def matchCWork(c: Char): Unit = matchCImpl(j => atc(j))(c)
    @publicInBinary protected[eio] def matchTokWork(s: String): Unit = matchTokImpl(j => atc(j), n => advc(n))(s, s.length, k => s.charAt(k).toInt)
    @publicInBinary protected[eio] def longWork(): Long = longImpl(j => atc(j), n => advc(n))
    @publicInBinary protected[eio] def longTailWork(x0: Long, neg: Boolean): Long = longTailImpl(j => atc(j))(x0, neg)
    @publicInBinary protected[eio] def digitsWork(j0: Long, c0: Int, budget: Int): Long = digitsImpl(j => atc(j))(j0, c0, budget)
    @publicInBinary protected[eio] def hexWork(j0: Long, c0: Int, budget: Int): Long = hexImpl(j => atc(j))(j0, c0, budget)
    @publicInBinary protected[eio] def doubleWork(mode: Int): Double = doubleImpl(j => atc(j), (a, b) => sub(a, b), n => advc(n))(mode)
    @publicInBinary protected[eio] def zWork(): Boolean = zImpl(j => atc(j), n => advc(n))
    @publicInBinary protected[eio] def tokWork(): String = tokImpl(j => atc(j), (a, b) => sub(a, b))
    @publicInBinary protected[eio] def tokSpanWork(): Int = tokSpanImpl(j => atc(j))
    @publicInBinary protected[eio] def charsWork(n: Int, exact: Boolean): String = charsFieldImpl(j => atc(j), (a, b) => sub(a, b), m => advc(m))(n, exact)
    @publicInBinary protected[eio] def strWork(qc: Char, mode: Int): Grok.StrOut =
      strCharImpl(
        j => atc(j),
        (a, b) => sub(a, b),
        (a, b) => java.util.Arrays.copyOfRange(buf, (a - discard).toInt, (b - discard).toInt),
        (a, b, arr, off) => System.arraycopy(buf, (a - discard).toInt, arr, off, (b - a).toInt),
        true
      )(qc, mode)
    private var memb: Mem[Char] = null.asInstanceOf[Mem[Char]]   // wraps buf for SWAR seeks; remade when buf regrows
    private var membOf: Array[Char] = null

    // strq's quote scan: SWAR across the loaded window, refilling through atc between windows
    private def seekq(j: Long, q: Char): Long =
      var k = j
      var going = true
      while going && k < iZ do
        val d = k - discard
        val lim = { val w = iZ - discard; if w < loaded then w else loaded.toLong }
        if d >= 0 && d < lim then
          if membOf ne buf then
            memb = Mem of buf
            membOf = buf
          val f = memb.whereIsFwd(d, lim)(q)
          if f >= 0 then
            k = discard + f
            going = false
          else k = discard + lim
        else if atc(k) == q.toInt then going = false
        else k += 1
      k

    @publicInBinary protected[eio] def strqWork(qc: Char, mode: Int): Grok.StrOut =
      strqCharImpl(
        j => atc(j),
        (a, b) => sub(a, b),
        (a, b) => java.util.Arrays.copyOfRange(buf, (a - discard).toInt, (b - discard).toInt),
        (a, b, arr, off) => System.arraycopy(buf, (a - discard).toInt, arr, off, (b - a).toInt),
        j => seekq(j, qc),
        true
      )(qc, mode)
  }


  // === Fill adapters: chunk-at-a-time and element-at-a-time inputs feed the windowed sources ===
  // Each answers a `fill` in Buffered's contract (elements written, or negative at end of input)
  // and never answers 0: chunk adapters skip empty chunks internally, element adapters write at
  // least one element or report the end.

  @publicInBinary private[eio] def chunkFillBytes(chunks: Iterator[Array[Byte]]): (Array[Byte], Int, Int) => Int =
    var cur = noBytes
    var pos = 0
    (dst, off, max) =>
      var live = true
      while live && pos >= cur.length do
        if chunks.hasNext then
          cur = chunks.next()
          pos = 0
        else live = false
      if !live then
        cur = noBytes   // drop the last chunk; the end answer repeats harmlessly if asked again
        -1
      else
        val n = if cur.length - pos < max then cur.length - pos else max
        System.arraycopy(cur, pos, dst, off, n)
        pos += n
        n

  @publicInBinary private[eio] def chunkFillChars(chunks: Iterator[Array[Char]]): (Array[Char], Int, Int) => Int =
    var cur = noChars
    var pos = 0
    (dst, off, max) =>
      var live = true
      while live && pos >= cur.length do
        if chunks.hasNext then
          cur = chunks.next()
          pos = 0
        else live = false
      if !live then
        cur = noChars
        -1
      else
        val n = if cur.length - pos < max then cur.length - pos else max
        System.arraycopy(cur, pos, dst, off, n)
        pos += n
        n

  @publicInBinary private[eio] def chunkFillString(chunks: Iterator[String]): (Array[Char], Int, Int) => Int =
    var cur = ""
    var pos = 0
    (dst, off, max) =>
      var live = true
      while live && pos >= cur.length do
        if chunks.hasNext then
          cur = chunks.next()
          pos = 0
        else live = false
      if !live then
        cur = ""
        -1
      else
        val n = if cur.length - pos < max then cur.length - pos else max
        cur.getChars(pos, pos + n, dst, off)
        pos += n
        n

  @publicInBinary private[eio] def elemFillBytes[A](elems: Iterator[A], f: A => Byte): (Array[Byte], Int, Int) => Int =
    (dst, off, max) =>
      var k = 0
      while k < max && elems.hasNext do
        dst(off + k) = f(elems.next())
        k += 1
      if k == 0 then -1 else k

  @publicInBinary private[eio] def elemFillChars[A](elems: Iterator[A], f: A => Char): (Array[Char], Int, Int) => Int =
    (dst, off, max) =>
      var k = 0
      while k < max && elems.hasNext do
        dst(off + k) = f(elems.next())
        k += 1
      if k == 0 then -1 else k


  /** Set up to parse input arriving one chunk at a time — `Array[Byte]`, `Array[Char]`, or
    * `String` chunks — through the same sliding window as `buffered`.  Byte chunks parse with
    * raw-byte semantics (like `Grok(bytes)`: structure is ASCII, `tok` decodes UTF-8); char and
    * String chunks parse as text.  Chunk boundaries are invisible to the parse — values,
    * literals, and quoted strings may straddle them — and empty chunks are fine.  The total
    * length need not be known: input ends when the iterator does.  Parameters and the parsing
    * block are otherwise as for `buffered`.
    */
  transparent inline def chunked(inline content: Iterator[Array[Byte]] | Iterator[Array[Char]] | Iterator[String], inline delim: Delim | Char | String = Delim.lines, partial: Boolean = false, exact: Boolean = false, window: Int = 64): Grok =
    inline content match
      case ib: Iterator[Array[Byte]] => new Buffered(chunkFillBytes(ib), -1L, Delim.from(delim), partial, !exact, window)
      case ic: Iterator[Array[Char]] => new BufferedChars(chunkFillChars(ic), -1L, Delim.from(delim), partial, !exact, window)
      case is: Iterator[String]      => new BufferedChars(chunkFillString(is), -1L, Delim.from(delim), partial, !exact, window)

  /** Set up to parse elements pulled one at a time from any iterator, each converted by `f` to
    * a `Byte` (raw-byte semantics, as for `Grok(bytes)`) or a `Char` (text semantics).  Far
    * slower than the array, `Mem`, or `chunked` forms — one or two virtual calls per element —
    * but works with anything that can present itself element by element.  A bare lambda cannot
    * be elaborated against the either-flavor union, so annotate its parameter: `(b: Byte) => b`,
    * not `_.toByte`.  Parameters and the parsing block are otherwise as for `buffered`.
    */
  transparent inline def iterate[A](content: Iterator[A], inline f: (A => Byte) | (A => Char), inline delim: Delim | Char | String = Delim.lines, partial: Boolean = false, exact: Boolean = false, window: Int = 64): Grok =
    inline f match
      case fb: (A => Byte) => new Buffered(elemFillBytes(content, fb), -1L, Delim.from(delim), partial, !exact, window)
      case fc: (A => Char) => new BufferedChars(elemFillChars(content, fc), -1L, Delim.from(delim), partial, !exact, window)

}
