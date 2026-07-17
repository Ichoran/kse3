// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


import kse.basics.{given, _}
import kse.maths.Ryu


/** A JSON output target.  Serialization is append-dominated and at most two implementations
  * are ever hot, so these are plain virtual calls; character-level cleverness (UTF-8
  * encoding, growth) lives inside each target.
  */
abstract class Jout {
  /** The style used for content with no preserved format (and for numeric rendering). */
  def style: Jstyle

  /** Pretty-printing depth of the node being emitted (containers push and pop it). */
  private[jsaun] var depth: Int = 0

  /** When set (by `reprint`), preserved formats are skipped and everything restyles. */
  private[jsaun] var ignoreFmt: Boolean = false

  /** Append one character (Bytes targets UTF-8 encode it; a lone surrogate becomes U+FFFD). */
  def add(c: Char): Unit

  /** Append the characters `[i0, iN)` of `s` (Bytes targets UTF-8 encode, pairing surrogates). */
  def add(s: String, i0: Int, iN: Int): Unit

  /** Append all of `s`. */
  final def add(s: String): Unit = add(s, 0, s.length)

  /** Append the UTF-8 bytes `[i0, iN)` (Str targets decode; Bytes targets bulk-copy). */
  def add(b: Array[Byte], i0: Int, iN: Int): Unit

  def add(l: Long): Unit
  def add(d: Double): Unit

  /** Append `d` precision-limited per `Ryu.fmt`: the shortest decimal within the don't-care
    * tolerance named by `mag` (last absolute place kept) and `sig` (significant figures).
    */
  def add(d: Double, mag: Int, sig: Int): Unit
}
object Jout {

  /** Builds a String (thin wrapper over a StringBuilder). */
  final class Str(val sb: java.lang.StringBuilder = new java.lang.StringBuilder, val style: Jstyle = Jstyle.compact) extends Jout {
    def add(c: Char): Unit = sb.append(c) __ Unit
    def add(s: String, i0: Int, iN: Int): Unit = sb.append(s, i0, iN) __ Unit
    def add(b: Array[Byte], i0: Int, iN: Int): Unit =
      sb.append(new String(b, i0, iN - i0, java.nio.charset.StandardCharsets.UTF_8)) __ Unit
    def add(l: Long): Unit = sb.append(l) __ Unit
    def add(d: Double): Unit = Ryu.append(MkStr.wrap(sb), d)   // not sb.append(d): both targets emit the same text, lowercase exponent included
    def add(d: Double, mag: Int, sig: Int): Unit = Ryu.fmt(MkStr.wrap(sb), d, mag, sig)
    def result: String = sb.toString
  }

  /** Builds UTF-8 bytes in a growable buffer. */
  final class Bytes(initialSize: Int = 256, val style: Jstyle = Jstyle.compact) extends Jout {
    private var buf = new Array[Byte](if initialSize < 16 then 16 else initialSize)
    private var n = 0

    private def ensure(k: Int): Unit =
      if n + k > buf.length then
        var m = buf.length * 2
        while m < n + k do m *= 2
        buf = java.util.Arrays.copyOf(buf, m)

    private def put(b: Int): Unit =
      buf(n) = b.toByte
      n += 1

    private def encode(c: Char): Unit =
      if c < 0x80 then put(c)
      else if c < 0x800 then
        put(0xC0 | (c >> 6))
        put(0x80 | (c & 0x3F))
      else if c >= 0xD800 && c < 0xE000 then
        put(0xEF); put(0xBF); put(0xBD)   // lone surrogate: U+FFFD replacement
      else
        put(0xE0 | (c >> 12))
        put(0x80 | ((c >> 6) & 0x3F))
        put(0x80 | (c & 0x3F))

    def add(c: Char): Unit =
      ensure(4)
      encode(c)

    def add(s: String, i0: Int, iN: Int): Unit =
      ensure(4 * (iN - i0))
      var i = i0
      while i < iN do
        val c = s.charAt(i)
        if c < 0x80 then put(c)
        else if c >= 0xD800 && c < 0xDC00 && i + 1 < iN && (s.charAt(i+1) & 0xFC00) == 0xDC00 then
          val cp = 0x10000 + ((c - 0xD800) << 10) + (s.charAt(i+1) - 0xDC00)
          put(0xF0 | (cp >> 18))
          put(0x80 | ((cp >> 12) & 0x3F))
          put(0x80 | ((cp >> 6) & 0x3F))
          put(0x80 | (cp & 0x3F))
          i += 1
        else encode(c)
        i += 1

    def add(b: Array[Byte], i0: Int, iN: Int): Unit =
      ensure(iN - i0)
      System.arraycopy(b, i0, buf, n, iN - i0)
      n += iN - i0

    def add(l: Long): Unit = add(java.lang.Long.toString(l))
    def add(d: Double): Unit =
      ensure(24)
      n = Ryu.append(buf, n, d)
    def add(d: Double, mag: Int, sig: Int): Unit =
      ensure(24)
      n = Ryu.fmt(buf, n, d, mag, sig)

    def result: Array[Byte] = java.util.Arrays.copyOf(buf, n)
  }
}
