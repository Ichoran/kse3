// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014, 2015, 2020, 2021 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio

import java.io._
import java.nio._
import java.nio.file._
import java.nio.charset.StandardCharsets._
import java.util.Base64

import kse.flow.{given, _}
import kse.maths.{given, _}
import kse.maths.packed.{given, _}

object EioConversion {
  private val encode64urlTable: Array[Byte] = Array(
    'A'.toByte, 'B'.toByte, 'C'.toByte, 'D'.toByte, 'E'.toByte, 'F'.toByte, 'G'.toByte, 'H'.toByte,
    'I'.toByte, 'J'.toByte, 'K'.toByte, 'L'.toByte, 'M'.toByte, 'N'.toByte, 'O'.toByte, 'P'.toByte,
    'Q'.toByte, 'R'.toByte, 'S'.toByte, 'T'.toByte, 'U'.toByte, 'V'.toByte, 'W'.toByte, 'X'.toByte,
    'Y'.toByte, 'Z'.toByte, 'a'.toByte, 'b'.toByte, 'c'.toByte, 'd'.toByte, 'e'.toByte, 'f'.toByte,
    'g'.toByte, 'h'.toByte, 'i'.toByte, 'j'.toByte, 'k'.toByte, 'l'.toByte, 'm'.toByte, 'n'.toByte,
    'o'.toByte, 'p'.toByte, 'q'.toByte, 'r'.toByte, 's'.toByte, 't'.toByte, 'u'.toByte, 'v'.toByte,
    'w'.toByte, 'x'.toByte, 'y'.toByte, 'z'.toByte, '0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte,
    '4'.toByte, '5'.toByte, '6'.toByte, '7'.toByte, '8'.toByte, '9'.toByte, '-'.toByte, '_'.toByte
  )

  private val decode64Table: Array[Byte] = Array(
    // Starts at index 9, given in groups of 9
    64, 64, -1, -1, 64, -1, -1, -1, -1,  //  9-17
    -1, -1, -1, -1, -1, -1, -1, -1, -1,  // 18-26
    -1, -1, -1, -1, -1, 64, -1, -1, -1,  // 27-35
    -1, -1, -1, -1, -1, -1, -1, 62, -1,  // 36-44
    62, -1, 63, 52, 53, 54, 55, 56, 57,  // 45-53
    58, 59, 60, 61, -1, -1, -1, 64, -1,  // 54-62
    -1, -1,  0,  1,  2,  3,  4,  5,  6,  // 63-71
     7,  8,  9, 10, 11, 12, 13, 14, 15,  // 72-80
    16, 17, 18, 19, 20, 21, 22, 23, 24,  // 81-89
    25, -1, -1, -1, -1, 63, -1, 26, 27,  // 90-98
    28, 29, 30, 31, 32, 33, 34, 35, 36,  // 99-107
    37, 38, 39, 40, 41, 42, 43, 44, 45,  //108-115
    46, 47, 48, 49, 50, 51               //117-122
  )

  def encodeUrlStyleBase64Range(raw: Array[Byte], i0: Int, iN: Int, lineLength: Int = Int.MaxValue): Array[Byte] =
    if i0 < 0 || iN > raw.length then throw new RuntimeException(s"Invalid input range: $i0 until $iN in input of length ${raw.length}")
    var l = ((iN - i0)*4 + 2)/3
    val n = if lineLength < 4 then 1 else lineLength/4
    if l > 4*n then l += (l-1)/(4*n)
    if l > Int.MaxValue - 7 then throw new RuntimeException(s"Overly long Base64 encode: would require $l bytes")
    val a = new Array[Byte](l.toInt)
    var i = i0
    var j = 0
    var k = 0
    while i+3 < iN do
      k += 1
      if k > n then
        k = 1
        a(j) = '\n'.toByte
        j += 1
      val chunk = ((raw(i) & 0xFF) << 16) | ((raw(i+1) & 0xFF) << 8) | (raw(i+2) & 0xFF)
      a(j  ) = encode64urlTable( chunk >> 18        )
      a(j+1) = encode64urlTable((chunk >> 12) & 0x2F)
      a(j+2) = encode64urlTable((chunk >>  6) & 0x2F)
      a(j+3) = encode64urlTable( chunk        & 0x2F)
      i += 3
      j += 4
    if i < iN then
      if k >= n then
        a(j) = '\n'.toByte
        j += 1
      if i + 1 == iN then
        val chunk = (raw(i) & 0xFF) << 4
        a(j  ) = encode64urlTable(chunk >> 6)
        a(j+1) = encode64urlTable(chunk & 0x2F)
      else
        val chunk = ((raw(i) & 0xFF) << 10) | ((raw(i+1) & 0xFF) << 2)
        a(j  ) = encode64urlTable( chunk >> 12        )
        a(j+1) = encode64urlTable((chunk >>  6) & 0x2F)
        a(j+2) = encode64urlTable( chunk        & 0x2F)
    a

  def encodeUrlStyleBase64(raw: Array[Byte], lineLength: Int = Int.MaxValue): Array[Byte] =
    encodeUrlStyleBase64Range(raw, 0, raw.length, lineLength)

  def stringEncodeUrlStyleBase64(raw: Array[Byte], lineLength: Int = Int.MaxValue): String =
    new String(encodeUrlStyleBase64Range(raw, 0, raw.length, lineLength), ISO_8859_1)

  def decodeAnyBase64RangeInto(encoded: Array[Byte], i0: Int, iN: Int)(target: Array[Byte], index: Int): Int Or Err = Or.Ret:
    var bits = 0
    var nb = 0
    var i = i0
    if i0 < 0 || iN > encoded.length then Err.break(s"Input $i0 until $iN not within input of length ${encoded.length}")
    if index < 0 then Err.break(s"Output index is negative: $index")
    var j = index
    while i < iN do
      val b = encoded(i)
      val y =
        if b >= 9 && b <= 122 then decode64Table(b-9)
        else -1
      if y == -1 then Err.break(s"Invalid Base64 character ${b.toChar} at index $i")
      if y != 64 then 
        bits = (bits << 6) | y
        nb += 6
      if nb >= 24 then
        if j > target.length-2 then Err.break(s"Insufficient space in decoding array with ${4+(iN-i)} input bytes remaining")
        target(j) = bits.byte(2); j += 1
        target(j) = bits.byte(1); j += 1
        target(j) = bits.byte(0); j += 1
        bits = 0
        nb = 0
      i += 1
    if nb >= 8 then
      if j >= target.length then Err.break(s"Insufficient space in decoding array with $nb input bits remaining")
      bits = bits << (24 - nb)
      target(j) = bits.byte(2)
      bits = bits & 0xFFFF
      j += 1
      if nb >= 16 then
        if j >= target.length then Err.break(s"Insufficient space in decoding array with ${nb - 8} input bits remaining")
        target(j) = bits.byte(1)
        bits = bits & 0xFF
        j += 1
    if bits != 0 then Err.break(s"Trailing bits: ${if bits.byte(1) != 0 then bits.byte(1).bitString else bits.byte(0).bitString}; Base64 data must be incomplete")
    j - index

  def decodeAnyBase64Into(encoded: Array[Byte])(target: Array[Byte], index: Int): Int Or Err =
    decodeAnyBase64RangeInto(encoded, 0, encoded.length)(target, index)

  def decodeAnyBase64Range(encoded: Array[Byte], i0: Int, iN: Int): Array[Byte] Or Err =
    if i0 < 0 || iN > encoded.length then Err.or(s"Base64 decode range $i0 until $iN not within array of length ${encoded.length}")
    else
      val a = new Array[Byte]((((iN - i0).toLong*3 + 2) / 4).toInt)
      decodeAnyBase64RangeInto(encoded, i0, iN)(a, 0).map(i => a.shrinkCopy(i))

  def decodeAnyBase64(encoded: Array[Byte]): Array[Byte] Or Err =
    val a = new Array[Byte](((encoded.length.toLong*3 + 2)/4).toInt)
    decodeAnyBase64RangeInto(encoded, 0, encoded.length)(a, 0).map(i => a.shrinkCopy(i))

  def decodeAnyBase64RangeInto(encoded: String, i0: Int, iN: Int)(target: Array[Byte], index: Int): Int Or Err = Or.Ret:
    var bits = 0
    var nb = 0
    var i = i0
    if iN > encoded.length then Err.break(s"Input length exceeded: $i0 until $iN not within ${encoded.length}")
    if index < 0 then Err.break(s"Output index is negative: $index")
    var j = index
    while i < iN do
      val c = encoded.charAt(i)
      val y =
        if c >= 9 && c <= 122 then decode64Table(c-9)
        else -1
      if y == -1 then Err.break(s"Invalid Base64 character $c at index $i")
      if y != 64 then 
        bits = (bits << 6) | y
        nb += 6
      if nb >= 24 then
        if j > target.length-2 then Err.break(s"Insufficient space in decoding array with ${4+(iN-i)} input bytes remaining")
        target(j) = bits.byte(2); j += 1
        target(j) = bits.byte(1); j += 1
        target(j) = bits.byte(0); j += 1
        bits = 0
        nb = 0
      i += 1
    if nb >= 8 then
      if j >= target.length then Err.break(s"Insufficient space in decoding array with $nb input bits remaining")
      bits = bits << (24 - nb)
      target(j) = bits.byte(2)
      bits = bits & 0xFFFF
      j += 1
      if nb >= 16 then
        if j >= target.length then Err.break(s"Insufficient space in decoding array with ${nb - 8} input bits remaining")
        target(j) = bits.byte(1)
        bits = bits & 0xFF
        j += 1
    if bits != 0 then Err.break(s"Trailing bits: ${if bits.byte(1) != 0 then bits.byte(1).bitString else bits.byte(0).bitString}; Base64 data must be incomplete")
    j - index

  def decodeAnyBase64Into(encoded: String)(target: Array[Byte], index: Int): Int Or Err =
    decodeAnyBase64RangeInto(encoded, 0, encoded.length)(target, index)

  def decodeAnyBase64Range(encoded: String, i0: Int, iN: Int): Array[Byte] Or Err =
    if i0 < 0 || iN > encoded.length then Err.or(s"Base64 decode range $i0 until $iN not within string of length ${encoded.length}")
    else
      val a = new Array[Byte]((((iN - i0).toLong*3) / 4).toInt)
      decodeAnyBase64RangeInto(encoded, i0, iN)(a, 0).map(i => a.shrinkCopy(i))

  def decodeAnyBase64(encoded: String): Array[Byte] Or Err =
    val a = new Array[Byte](((encoded.length.toLong*3)/4).toInt)
    decodeAnyBase64RangeInto(encoded, 0, encoded.length)(a, 0).map(i => a.shrinkCopy(i))

  private def encodeTable_to_decodeTable(encoder: Array[Byte]): Array[Byte] =
    if encoder.length == 0 then encoder
    else
      var bmin = '\t'.toByte
      var bmax = ' '.toByte
      aFor(encoder){ (b, _) =>
        if b < bmin then bmin = b
        if b > bmax then bmax = b
      }
      if bmin < '\t' then throw new Exception(s"Tried to decode a table with indices smaller than tab: $bmin")
      val decoder: Array[Byte] = Array.fill[Byte](1 + bmax - 9)(-1)
      aFor(encoder){ (b, i) =>
        if decoder(b - 9) != -1 then throw new Exception(s"Encoder irreversible because of double mapping to $b")
        decoder(b - 9) = i.toByte
      }
      if decoder( 0) == -1 then decoder( 0) = (encoder.length).toByte
      if decoder( 1) == -1 then decoder( 1) = (encoder.length).toByte
      if decoder( 3) == -1 then decoder( 3) = (encoder.length).toByte
      if decoder(23) == -1 then decoder(23) = (encoder.length).toByte
      decoder

  private val encode85asciiTable: Array[Byte] = Array.tabulate(85)(i => (i+33).toByte)

  private val encodeZ85Table: Array[Byte] = Array(
    '0'.toByte, '1'.toByte, '2'.toByte, '3'.toByte, '4'.toByte, '5'.toByte, '6'.toByte, '7'.toByte, '8'.toByte, '9'.toByte,
    'a'.toByte, 'b'.toByte, 'c'.toByte, 'd'.toByte, 'e'.toByte, 'f'.toByte, 'g'.toByte, 'h'.toByte, 'i'.toByte, 'j'.toByte,
    'k'.toByte, 'l'.toByte, 'm'.toByte, 'n'.toByte, 'o'.toByte, 'p'.toByte, 'q'.toByte, 'r'.toByte, 's'.toByte, 't'.toByte,
    'u'.toByte, 'v'.toByte, 'w'.toByte, 'x'.toByte, 'y'.toByte, 'z'.toByte, 'A'.toByte, 'B'.toByte, 'C'.toByte, 'D'.toByte,
    'E'.toByte, 'F'.toByte, 'G'.toByte, 'H'.toByte, 'I'.toByte, 'J'.toByte, 'K'.toByte, 'L'.toByte, 'M'.toByte, 'N'.toByte,
    'O'.toByte, 'P'.toByte, 'Q'.toByte, 'R'.toByte, 'S'.toByte, 'T'.toByte, 'U'.toByte, 'V'.toByte, 'W'.toByte, 'X'.toByte,
    'Y'.toByte, 'Z'.toByte, '.'.toByte, '-'.toByte, ':'.toByte, '+'.toByte, '='.toByte, '^'.toByte, '!'.toByte, '/'.toByte,
    '*'.toByte, '?'.toByte, '&'.toByte, '<'.toByte, '>'.toByte, '('.toByte, ')'.toByte, '['.toByte, ']'.toByte, '{'.toByte,
    '}'.toByte, '@'.toByte, '%'.toByte, '$'.toByte, '#'.toByte
  )

  private val decodeZ85Table = encodeTable_to_decodeTable(encodeZ85Table)

  private val decode85asciiTable = encodeTable_to_decodeTable(encode85asciiTable)

  private def encode85RangeWithTable(raw: Array[Byte], i0: Int, iN: Int, table: Array[Byte]): Array[Byte] =
    if i0 < 0 || iN > raw.length then throw new RuntimeException(s"Range $i0 until $iN out of bounds of array of length ${raw.length}")
    val l = ((iN - i0)*5 + 3)/4
    if l > Int.MaxValue - 7 then throw new RuntimeException(s"Overly long Base85 encode: would require $l bytes")
    val result = new Array[Byte](l.toInt)
    var code = 0
    var i = i0
    var j = 0
    while i+3 < iN do
      code = Pack.I(raw(i+3), raw(i+2), raw(i+1), raw(i))
      i += 4
      var c85 = (code.u / 85.u).s
      result(j+4) = table(code - 85*c85)
      code = c85
      c85 = c85 / 85
      result(j+3) = table(code - 85*c85)
      code = c85
      c85 = c85 / 85
      result(j+2) = table(code - 85*c85)
      code = c85
      c85 = c85 / 85
      result(j+1) = table(code - 85*c85)
      result(j) = table(c85)
      j += 5
    if i < iN then
      val overflow = iN - i
      code = 0
      while i < iN do
        code = (code << 8) | (raw(i) & 0xFF)
        i += 1
      code = code << (8 * (4 - overflow))
      var c = (code.u / 52200625.u).s
      result(j) = table(c)
      code = code - 52200625*c
      c = code / 614125
      result(j+1) = table(c)
      if overflow > 1 then
        code = code - 614125*c
        c = code / 7225
        result(j+2) = table(c)
        if overflow > 2 then
          code = code - 7225*c
          c = code / 85
          result(j+3) = table(c)
    result

  def encode85ascii(raw: Array[Byte]): Array[Byte] =
    inline def table(i: Int) = (i + 33).toByte
    val l = (raw.length.toLong*5 + 3)/4
    if l > Int.MaxValue - 7 then throw new RuntimeException(s"Overly long Base85 encode: would require $l bytes")
    val result = new Array[Byte](l.toInt)
    var code = 0
    var i = 0
    var j = 0
    while i+3 < raw.length do
      code = Pack.I(raw(i+3), raw(i+2), raw(i+1), raw(i))
      i += 4
      var c85 = (code.u / 85.u).s
      result(j+4) = table(code - 85*c85)
      code = c85
      c85 = c85 / 85
      result(j+3) = table(code - 85*c85)
      code = c85
      c85 = c85 / 85
      result(j+2) = table(code - 85*c85)
      code = c85
      c85 = c85 / 85
      result(j+1) = table(code - 85*c85)
      result(j) = table(c85)
      j += 5
    if i < raw.length then
      val overflow = raw.length - i
      code = 0
      while i < raw.length do
        code = (code << 8) | (raw(i) & 0xFF)
        i += 1
      code = code << (8 * (4 - overflow))
      var c = (code.u / 52200625.u).s
      result(j) = table(c)
      code = code - 52200625*c
      c = code / 614125
      result(j+1) = table(c)
      if overflow > 1 then
        code = code - 614125*c
        c = code / 7225
        result(j+2) = table(c)
        if overflow > 2 then
          code = code - 7225*c
          c = code / 85
          result(j+3) = table(c)
    result

  def stringEncode85ascii(raw: Array[Byte]): String =
    new String(encode85ascii(raw), ISO_8859_1)

  def encode85asciiRange(raw: Array[Byte], i0: Int, iN: Int): Array[Byte] = encode85RangeWithTable(raw, i0, iN, encode85asciiTable)

  def encodeZ85(raw: Array[Byte]): Array[Byte] = encode85RangeWithTable(raw, 0, raw.length, encodeZ85Table)

  def stringEncodeZ85(raw: Array[Byte]): String =
    new String(encodeZ85(raw), ISO_8859_1)

  def encodeZ85Range(raw: Array[Byte], i0: Int, iN: Int): Array[Byte] = encode85RangeWithTable(raw, i0, iN, encodeZ85Table)

  private def decode85WithTable(encoded: Array[Byte], table: Array[Byte]): Array[Byte] Or Err = Or.Ret:
    val l = (encoded.length.toLong*4)/5
    var a = new Array[Byte](l.toInt)
    var lead: Int = 0
    var rest: Int = 0
    var stored = 0
    var i = 0
    var j = 0
    val bmax = 9 + table.length - 1
    while i < encoded.length do
      val b = encoded(i)
      val y =
        if b >= 9 && b <= bmax then table(b - 9)
        else -1
      if y < 0 then Err.break(s"Invalid Base85 character for this encoding: '${b.toChar}' at $i")
      else if y < 85 then
        if stored == 0 then
          lead = y
          stored = 1
        else
          rest = 85*rest + y
          stored += 1
          if stored == 5 then
            stored = 0
            if lead > 82 || (lead == 82 && rest > 14516045) then Err.break(s"Over-full Base85 number $lead*85^4 + $rest ending at $i")
            rest = lead*52200625 + rest
            a(j) =   ( rest >>> 24        ).toByte
            a(j+1) = ((rest >>> 16) & 0xFF).toByte
            a(j+2) = ((rest >>>  8) & 0xFF).toByte
            a(j+3) = ( rest         & 0xFF).toByte
            rest = 0
            j += 4
      i += 1
    if stored > 0 then
      val n = stored
      while stored < 5 do
        rest = rest*85 + 84
        stored += 1
      if lead > 82 || (lead == 82 && rest > 14516045) then Err.break(s"Over-full Base85 number $lead*85^4 + $rest at end of input")
      rest = lead*52200625 + rest
      a(j) = (rest >>> 24).toByte
      if n > 2 then
        a(j+1) = ((rest >>> 16) & 0xFF).toByte
        if n > 3 then
          a(j+2) = ((rest >>> 8) & 0xFF).toByte
    a

  def decodeZ85(encoded: Array[Byte]): Array[Byte] Or Err = decode85WithTable(encoded, decodeZ85Table)

  def decode85ascii(encoded: Array[Byte]): Array[Byte] Or Err = decode85WithTable(encoded, decode85asciiTable)

  private def decode85WithTable(encoded: String, table: Array[Byte]): Array[Byte] Or Err = Or.Ret:
    val l = (encoded.length.toLong*4)/5
    var a = new Array[Byte](l.toInt)
    var lead: Int = 0
    var rest: Int = 0
    var stored = 0
    var i = 0
    var j = 0
    val bmax = 9 + table.length - 1
    while i < encoded.length do
      val b = encoded.charAt(i)
      val y =
        if b >= 9 && b <= bmax then table(b - 9)
        else -1
      if y < 0 then Err.break(s"Invalid Base85 character for this encoding: '${b.toChar}' at $i")
      else if y < 85 then
        if stored == 0 then
          lead = y
          stored = 1
        else
          rest = 85*rest + y
          stored += 1
          if stored == 5 then
            stored = 0
            if lead > 82 || (lead == 82 && rest > 14516045) then Err.break(s"Over-full Base85 number $lead*85^4 + $rest ending at $i")
            rest = lead*52200625 + rest
            a(j) =   ( rest >>> 24        ).toByte
            a(j+1) = ((rest >>> 16) & 0xFF).toByte
            a(j+2) = ((rest >>>  8) & 0xFF).toByte
            a(j+3) = ( rest         & 0xFF).toByte
            rest = 0
            j += 4
      i += 1
    if stored > 0 then
      val n = stored
      while stored < 5 do
        rest = rest*85 + 84
        stored += 1
      if lead > 82 || (lead == 82 && rest > 14516045) then Err.break(s"Over-full Base85 number $lead*85^4 + $rest at end of input")
      rest = lead*52200625 + rest
      a(j) = (rest >>> 24).toByte
      if n > 2 then
        a(j+1) = ((rest >>> 16) & 0xFF).toByte
        if n > 3 then
          a(j+2) = ((rest >>> 8) & 0xFF).toByte
    a

  def decodeZ85(encoded: String): Array[Byte] Or Err = decode85WithTable(encoded, decodeZ85Table)

  def decode85ascii(encoded: String): Array[Byte] Or Err = decode85WithTable(encoded, decode85asciiTable)


  /*
  def encode85(raw: Array[Byte]): Array[Byte] = encode85WithTable(raw, encode85Table)
  def encode85ipv6(raw: Array[Byte]): Array[Byte] = encode85WithTable(raw, encode85ipv6Table)
  def encode85(raw: Array[Int]): Array[Byte] = encode85WithTable(raw, encode85Table)
  def encodeZ85(raw: Array[Int]): Array[Byte] = encode85WithTable(raw, encodeZ85Table)
  def encode85ipv(raw: Array[Byte]): Array[Byte] = encode85WithTable(raw, encode85ipv6Table)
  */
}

extension (underlying: Array[Byte]) {
  inline def utf8 = new String(underlying, UTF_8)
  inline def ascii = new String(underlying, US_ASCII)
  inline def rawString = new String(underlying, ISO_8859_1)
  inline def iso8859_1 = new String(underlying, ISO_8859_1)
  inline def buffer = ByteBuffer.wrap(underlying)
  inline def input = new ByteArrayInputStream(underlying)

  inline def stringEncode64 = Base64.getUrlEncoder.encodeToString(underlying)
  inline def stringEncode64basic = Base64.getEncoder.encodeToString(underlying)
  inline def stringEncode64url = Base64.getUrlEncoder.encodeToString(underlying)
  inline def stringEncode64mime = Base64.getMimeEncoder.encodeToString(underlying)
  inline def stringEncode64lines = EioConversion.stringEncodeUrlStyleBase64(underlying, 76)

  inline def stringEncode85 = EioConversion.stringEncodeZ85(underlying)
  inline def stringEncodeZ85 = EioConversion.stringEncodeZ85(underlying)
  inline def stringEncode85ascii = EioConversion.stringEncode85ascii(underlying)

  inline def encode64 = Base64.getUrlEncoder.encode(underlying)
  inline def encode64basic = Base64.getEncoder().encode(underlying)
  inline def encode64url = Base64.getUrlEncoder.encode(underlying)
  inline def encode64mime = Base64.getMimeEncoder().encode(underlying)
  inline def encode64lines = EioConversion.encodeUrlStyleBase64(underlying, 76)

  inline def decode64 = EioConversion.decodeAnyBase64(underlying)
  inline def decode64basic = safe{ Base64.getDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  inline def decode64url = safe{ Base64.getUrlDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  inline def decode64mime = safe{ Base64.getMimeDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))

  inline def encode85 = EioConversion.encodeZ85(underlying)
  inline def encodeZ85 = EioConversion.encodeZ85(underlying)
  inline def encode85ascii = EioConversion.encode85ascii(underlying)

  inline def decode85 = EioConversion.decodeZ85(underlying)
  inline def decodeZ85 = EioConversion.decodeZ85(underlying)
  inline def decode85ascii = EioConversion.decode85ascii(underlying)
}

extension (underlying: String) {
  inline def bytes: Array[Byte] = underlying.getBytes(UTF_8)

  inline def decode64 = EioConversion.decodeAnyBase64(underlying)
  inline def decode64basic = safe{ Base64.getDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  inline def decode64url = safe{ Base64.getUrlDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  inline def decode64mime = safe{ Base64.getMimeDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))

  inline def decode85 = EioConversion.decodeZ85(underlying)
  inline def decodeZ85 = EioConversion.decodeZ85(underlying)
  inline def decode85ascii = EioConversion.decode85ascii(underlying)
}

extension (underlying: java.util.zip.ZipEntry) {
  def fixedName =
    val n = underlying.getName
    val i = n.indexOf('/')
    val j = n.indexOf('\\')
    if i < 0 && j > 0 then n.replace('\\', '/') else n
  
  def nameAsFile =
    val n = underlying.fixedName
    new File(if File.separatorChar == '/' then n else n.replace('/',File.separatorChar))
  
  def nameAsPath =
    val n = underlying.fixedName
    val fs = FileSystems.getDefault
    fs.getPath(if fs.getSeparator == "/" then n else n.replace("/", fs.getSeparator))
}
