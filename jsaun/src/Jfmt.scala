// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


import kse.basics.{given, _}


/** Source text retained by a format-preserving parse, ready to be spliced verbatim into
  * output.  Each `Jfmt` holds its own `Jsrc`, so a subtree cut from one document remains
  * verbatim-printable inside another, and the source is garbage-collected exactly when no
  * node refers to it any more.
  */
final class Jsrc private (private val content: String | Array[Byte] | Array[Char]) {
  def length: Int = content match
    case s: String => s.length
    case b: Array[Byte] => b.length
    case c: Array[Char] => c.length

  /** Copy the raw span `[i0, iN)` into `out` exactly as it appeared (byte source to byte
    * target is a plain array copy).
    */
  def copyTo(out: Jout, i0: Int, iN: Int): Unit = content match
    case s: String => out.add(s, i0, iN)
    case b: Array[Byte] => out.add(b, i0, iN)
    case c: Array[Char] => out.add(new String(c, i0, iN - i0))

  /** The raw span `[i0, iN)` as a String (bytes decoded as UTF-8). */
  def substring(i0: Int, iN: Int): String = content match
    case s: String => s.substring(i0, iN)
    case b: Array[Byte] => new String(b, i0, iN - i0, java.nio.charset.StandardCharsets.UTF_8)
    case c: Array[Char] => new String(c, i0, iN - i0)
}
object Jsrc {
  def apply(s: String): Jsrc = new Jsrc(s)
  def apply(b: Array[Byte]): Jsrc = new Jsrc(b)
  def apply(c: Array[Char]): Jsrc = new Jsrc(c)
}


/** Format sidecar for one collection node parsed in format-preserving mode: where the node
  * itself (`self`, brackets included) and each of its syntactic slots sat in the retained
  * source.  Arrays have one slot per element; objects two per entry (key, then value).  The
  * verbatim gaps BETWEEN slots -- commas, colons, newlines, indentation -- are what
  * re-serialization copies around edited islands.
  *
  * Editing marks slots dirty rather than clobbering their spans: a dirty slot's content is
  * re-serialized, but its old span still bounds the untouched gaps on either side of it.
  */
final class Jfmt private[jsaun] (val src: Jsrc, val self: Long, val spans: Array[Long]) {
  private[jsaun] var bits: Array[Long] | Null = null

  /** Offset of the first character of this node in `src`. */
  inline def start: Int = (self >>> 32).toInt

  /** Offset just past the last character of this node in `src`. */
  inline def end: Int = (self & 0xFFFFFFFFL).toInt

  private[jsaun] def markDirty(slot: Int): Unit =
    val b = bits match
      case null =>
        val b2 = new Array[Long]((spans.length + 63) >> 6)
        bits = b2
        b2
      case b2 => b2
    b(slot >> 6) |= 1L << (slot & 63)

  private[jsaun] def isDirty(slot: Int): Boolean = bits match
    case null => false
    case b => ((b(slot >> 6) >>> (slot & 63)) & 1L) != 0

  /** True if any slot of this node (not descendants) has been marked dirty. */
  private[jsaun] def anyDirty: Boolean = bits ne null
}
object Jfmt {
  inline def span(i0: Int, iN: Int): Long = (i0.toLong << 32) | (iN.toLong & 0xFFFFFFFFL)
  inline def start(span: Long): Int = (span >>> 32).toInt
  inline def end(span: Long): Int = (span & 0xFFFFFFFFL).toInt

  /** A collection's own separator style, inferred from its preserved format the moment a
    * structural edit invalidates the span bookkeeping: the node re-serializes with these
    * verbatim-sampled pieces (`'[' open elem sep elem ... close ']'`; objects put `mid`
    * between key and value), so an insertion comes out matching its siblings' layout while
    * unedited children still print from their own retained source.
    */
  final class Local private[jsaun] (val open: String, val sep: String, val mid: String, val close: String) {}
  object Local {
    private[jsaun] def ofArr(f: Jfmt, n: Int): Local =
      if n == 0 then new Local("", ", ", ": ", "")
      else
        val open = f.src.substring(f.start + 1, start(f.spans(0)))
        val sep =
          if n >= 2 then f.src.substring(end(f.spans(0)), start(f.spans(1)))
          else "," + open
        new Local(open, sep, ": ", f.src.substring(end(f.spans(n - 1)), f.end - 1))

    private[jsaun] def ofObj(f: Jfmt, n: Int): Local =
      if n == 0 then new Local("", ", ", ": ", "")
      else
        val open = f.src.substring(f.start + 1, start(f.spans(0)))
        val mid = f.src.substring(end(f.spans(0)), start(f.spans(1)))
        val sep =
          if n >= 2 then f.src.substring(end(f.spans(1)), start(f.spans(2)))
          else "," + open
        new Local(open, sep, mid, f.src.substring(end(f.spans(2 * n - 1)), f.end - 1))
  }
}


/** Serialization style: indentation, spacing, and how Doubles are rendered.  A style applies
  * only where no preserved format exists -- preserved format always wins for untouched
  * content -- except through `reprint`, which restyles everything.
  *
  * The numeric policy is the cure for `[0.30000000000000001, 0.5]`: under `Sig(n)` (at most
  * `n` significant digits) or `Fixed(n)` (at most `n` decimals), each Double prints as the
  * SHORTER of the rounded form and the exact shortest-round-trip form, so `0.5` never grows
  * and noise digits vanish.
  */
final class Jstyle private (
  val indent: String,
  val spaceAfterColon: Boolean,
  val spaceAfterComma: Boolean,
  val num: Jstyle.Num
) {
  def sig(n: Int): Jstyle = new Jstyle(indent, spaceAfterColon, spaceAfterComma, Jstyle.Num.Sig(if n < 1 then 1 else n))
  def fixed(n: Int): Jstyle = new Jstyle(indent, spaceAfterColon, spaceAfterComma, Jstyle.Num.Fixed(if n < 0 then 0 else n))
  def exactly: Jstyle = new Jstyle(indent, spaceAfterColon, spaceAfterComma, Jstyle.Num.Exact)
  def indentBy(s: String): Jstyle = new Jstyle(s, spaceAfterColon, spaceAfterComma, num)
}
object Jstyle {
  enum Num {
    case Exact
    case Sig(digits: Int)
    case Fixed(decimals: Int)
  }

  /** Everything on one line, no spaces, exact numbers (the default). */
  val compact: Jstyle = new Jstyle("", false, false, Num.Exact)

  /** Two-space indentation, spaced separators, exact numbers. */
  val pretty: Jstyle = new Jstyle("  ", true, true, Num.Exact)

  given default: Jstyle = compact

  private[jsaun] def pad(indent: String, depth: Int): String =
    val sb = new java.lang.StringBuilder(1 + indent.length * depth)
    sb.append('\n') __ Unit
    var k = 0
    while k < depth do
      sb.append(indent) __ Unit
      k += 1
    sb.toString

  private[jsaun] def sigText(d: Double, n: Int): String =
    val exact = java.lang.Double.toString(d)
    val rounded = (new java.math.BigDecimal(d)).round(new java.math.MathContext(n)).stripTrailingZeros.toString
    if exact.length <= rounded.length then exact else rounded

  private[jsaun] def fixedText(d: Double, n: Int): String =
    val exact = java.lang.Double.toString(d)
    val rounded = (new java.math.BigDecimal(d)).setScale(n, java.math.RoundingMode.HALF_EVEN).stripTrailingZeros.toString
    if exact.length <= rounded.length then exact else rounded
}
