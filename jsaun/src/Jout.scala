// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


import kse.basics.{given, _}


/** A JSON output target.  Serialization is append-dominated and at most two implementations
  * are ever hot, so these are plain virtual calls; character-level cleverness (UTF-8
  * encoding, growth) lives inside each target.
  */
abstract class Jout {
  /** Append one character (Bytes targets UTF-8 encode it; a lone surrogate becomes U+FFFD). */
  def add(c: Char): Unit

  /** Append the characters `[i0, iN)` of `s` (Bytes targets UTF-8 encode, pairing surrogates). */
  def add(s: String, i0: Int, iN: Int): Unit

  /** Append all of `s`. */
  final def add(s: String): Unit = add(s, 0, s.length)

  def add(l: Long): Unit
  def add(d: Double): Unit
}
object Jout {

  /** Builds a String (thin wrapper over a StringBuilder). */
  final class Str(val sb: java.lang.StringBuilder = new java.lang.StringBuilder) extends Jout {
    def add(c: Char): Unit = sb.append(c) __ Unit
    def add(s: String, i0: Int, iN: Int): Unit = sb.append(s, i0, iN) __ Unit
    def add(l: Long): Unit = sb.append(l) __ Unit
    def add(d: Double): Unit = sb.append(d) __ Unit
    def result: String = sb.toString
  }

  /** Builds UTF-8 bytes in a growable buffer. */
  final class Bytes(initialSize: Int = 256) extends Jout {
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

    def add(l: Long): Unit = add(java.lang.Long.toString(l))
    def add(d: Double): Unit = add(java.lang.Double.toString(d))

    def result: Array[Byte] = java.util.Arrays.copyOf(buf, n)
  }
}
