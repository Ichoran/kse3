// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import java.nio._

import kse.flow._


/** A type class that provides JSONizing functionality. */
trait Jsonize[A] {
  /** Convert the object to its JSON representation. */
  def jsonize(a: A): Json

  /** Accumulate the serialized JSON representation of the object in a Java StringBuilder. */
  def jsonizeString(a: A, sb: java.lang.StringBuilder): Unit = jsonize(a).jsonString(sb)

  /** Accumulate the serialized JSON representation of the object in a ByteBuffer.
    *
    * Note: if the `ByteBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 bytes more will be made available per call.
    */
  def jsonizeBytes(a: A, bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = jsonize(a).jsonBytes(bb, refresh)

  /** Accumulate the serialized JSON representation of this object in a CharBuffer.
    *
    * Note: if the `CharBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 chars more will be made available per call.
    */
  def jsonizeChars(a: A, cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = jsonize(a).jsonChars(cb, refresh)
}
object Jsonize {
  def numericArray(af: Array[Float]): Json.Arr.Dbl =
    val a = new Array[Double](af.length)
    aFor(af)((f, i) => a(i) = f.toDouble)
    Json.Arr.Dbl(a)

  def numericArray(al: Array[Long]): Json.Arr =
    var fits: Boolean = true
    var i = 0
    while (i < al.length && fits) { val l = al(i); fits = -0x20000000000000L < l && l < 0x20000000000000L; i += 1 }
    if (fits) {
      val a = new Array[Double](al.length)
      aFor(al)((l, i) => a(i) = l.toDouble)
      Json.Arr.Dbl(a)
    }
    else {
      val a = new Array[Json](al.length)
      aFor(al)((l, i) => a(i) = Json.Num(l))
      Json.Arr.All(a)
    }

  def numericArray(ai: Array[Int]): Json.Arr.Dbl =
    val a = new Array[Double](ai.length)
    aFor(ai)((j, i) => a(i) = j.toDouble)
    Json.Arr.Dbl(a)

  def numericArray(as: Array[Short]): Json.Arr.Dbl =
    val a = new Array[Double](as.length)
    aFor(as)((s, i) => a(i) = s.toDouble)
    Json.Arr.Dbl(a)

  def numericArray(ab: Array[Byte]): Json.Arr.Dbl =
    val a = new Array[Double](ab.length)
    aFor(ab)((b, i) => a(i) = b.toDouble)
    Json.Arr.Dbl(a)

  private[this] val charsAsStrings =
    val cs = new Array[Json.Str](127-32)
    nFor(cs.length){ i => cs(i) = Json.Str((i+32).toChar.toString) }
    cs
  def stringArray(ac: Array[Char]): Json.Arr.All =
    val a = new Array[Json](ac.length)
    aFor(ac)((c, i) => a(i) = if (c >= ' ' && c < 127) charsAsStrings(c-32) else Json.Str(c.toString))
    Json.Arr.All(a)

  def arbitraryArray[A](aa: Array[A])(using Jsonize[A]): Json.Arr.All =
    val a = new Array[Json](aa.length)
    val jz = summon[Jsonize[A]]
    aFor(aa)((x, i) => a(i) = jz.jsonize(x))
    Json.Arr.All(a)
}
