// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import java.nio._


/** Represents an object that can be mapped to JSON without error.
  * Methods for serializing the JSON representation are also provided.
  */
trait AsJson {
  /** The JSON representation of this object. */
  def json: Json

  /** Accumulate the serialized JSON representation of this object in a Java StringBuilder. */
  def jsonString(sb: java.lang.StringBuilder): Unit =
    json.jsonString(sb)

  /** Accumulate the serialized JSON representation of this object in a ByteBuffer.
    *
    * Note: if the `ByteBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 bytes more will be made available per call.
    */
  def jsonBytes(bb: ByteBuffer, refresh: ByteBuffer => ByteBuffer): ByteBuffer = 
    json.jsonBytes(bb, refresh)

  /** Accumulate the serialized JSON representation of this object in a CharBuffer.
    *
    * Note: if the `CharBuffer` runs out of room, `refresh` will be called.  It is assumed
    * that at least 64 chars more will be made available per call.
    */
  def jsonChars(cb: CharBuffer, refresh: CharBuffer => CharBuffer): CharBuffer = 
    json.jsonChars(cb, refresh)
}
object AsJson {
  private object JsonizeAsJson extends Jsonize[AsJson] {
    def jsonize(aj: AsJson): Json = aj.json
  }

  given [A <: AsJson]: Jsonize[A] = JsonizeAsJson.asInstanceOf[Jsonize[A]]
}

