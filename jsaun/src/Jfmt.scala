// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


/** Source text retained by a format-preserving parse, ready to be spliced verbatim into
  * output.  Each `Jfmt` holds its own `Jsrc`, so a subtree cut from one document remains
  * verbatim-printable inside another, and the source is garbage-collected exactly when no
  * node refers to it any more.
  */
final class Jsrc private (private val content: String | Array[Byte]) {
  def length: Int = content match
    case s: String => s.length
    case b: Array[Byte] => b.length

  /** Copy the raw span `[i0, iN)` into `out` exactly as it appeared (byte source to byte
    * target is a plain array copy).
    */
  def copyTo(out: Jout, i0: Int, iN: Int): Unit = content match
    case s: String => out.add(s, i0, iN)
    case b: Array[Byte] => out.add(b, i0, iN)
}
object Jsrc {
  def apply(s: String): Jsrc = new Jsrc(s)
  def apply(b: Array[Byte]): Jsrc = new Jsrc(b)
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
}
