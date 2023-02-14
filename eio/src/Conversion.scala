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

object EioBase64 {
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

  def encodeUrlRange(raw: Array[Byte], i0: Int, iN: Int, lineLength: Int = Int.MaxValue): Array[Byte] =
    if i0 < 0 || iN > raw.length then throw new RuntimeException(s"Invalid input range: $i0 until $iN in input of length ${raw.length}")
    var l = ((iN - i0)*4 + 2)/3
    val n = if lineLength < 4 then 1 else lineLength/4
    if l > 4*n then l += (l-1)/(4*n)
    if l > Int.MaxValue - 7 then throw new RuntimeException(s"Overly long Base64 encode: would require $l bytes")
    val a = new Array[Byte](l.toInt)
    var i = i0
    var j = 0
    var k = 0
    while i+2 < iN do
      k += 1
      if k > n then
        k = 1
        a(j) = '\n'.toByte
        j += 1
      val chunk = ((raw(i) & 0xFF) << 16) | ((raw(i+1) & 0xFF) << 8) | (raw(i+2) & 0xFF)
      a(j  ) = encode64urlTable( chunk >>> 18        )
      a(j+1) = encode64urlTable((chunk >>> 12) & 0x3F)
      a(j+2) = encode64urlTable((chunk >>>  6) & 0x3F)
      a(j+3) = encode64urlTable( chunk         & 0x3F)
      i += 3
      j += 4
    if i < iN then
      if k >= n then
        a(j) = '\n'.toByte
        j += 1
      if i + 1 == iN then
        val chunk = (raw(i) & 0xFF) << 16
        a(j  ) = encode64urlTable( chunk >>> 18)
        a(j+1) = encode64urlTable((chunk >>> 12) & 0x3F)
      else
        val chunk = ((raw(i) & 0xFF) << 16) | ((raw(i+1) & 0xFF) << 8)
        a(j  ) = encode64urlTable( chunk >>> 18        )
        a(j+1) = encode64urlTable((chunk >>> 12) & 0x3F)
        a(j+2) = encode64urlTable((chunk >>>  6) & 0x3F)
    a

  def encodeUrl(raw: Array[Byte], lineLength: Int = Int.MaxValue): Array[Byte] =
    encodeUrlRange(raw, 0, raw.length, lineLength)

  def stringEncodeUrl(raw: Array[Byte], lineLength: Int = Int.MaxValue): String =
    new String(encodeUrlRange(raw, 0, raw.length, lineLength), ISO_8859_1)

  def decodeRangeInto(encoded: Array[Byte], i0: Int, iN: Int)(target: Array[Byte], index: Int): Int Or Err = Or.Ret:
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

  def decodeInto(encoded: Array[Byte])(target: Array[Byte], index: Int): Int Or Err =
    decodeRangeInto(encoded, 0, encoded.length)(target, index)

  def decodeRange(encoded: Array[Byte], i0: Int, iN: Int): Array[Byte] Or Err =
    if i0 < 0 || iN > encoded.length then Err.or(s"Base64 decode range $i0 until $iN not within array of length ${encoded.length}")
    else
      val a = new Array[Byte]((((iN - i0).toLong*3 + 2) / 4).toInt)
      decodeRangeInto(encoded, i0, iN)(a, 0).map(i => a.shrinkCopy(i))

  def decode(encoded: Array[Byte]): Array[Byte] Or Err =
    val a = new Array[Byte](((encoded.length.toLong*3 + 2)/4).toInt)
    decodeRangeInto(encoded, 0, encoded.length)(a, 0).map(i => a.shrinkCopy(i))

  def decodeRangeInto(encoded: String, i0: Int, iN: Int)(target: Array[Byte], index: Int): Int Or Err = Or.Ret:
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

  def decodeInto(encoded: String)(target: Array[Byte], index: Int): Int Or Err =
    decodeRangeInto(encoded, 0, encoded.length)(target, index)

  def decodeRange(encoded: String, i0: Int, iN: Int): Array[Byte] Or Err =
    if i0 < 0 || iN > encoded.length then Err.or(s"Base64 decode range $i0 until $iN not within string of length ${encoded.length}")
    else
      val a = new Array[Byte]((((iN - i0).toLong*3) / 4).toInt)
      decodeRangeInto(encoded, i0, iN)(a, 0).map(i => a.shrinkCopy(i))

  def decode(encoded: String): Array[Byte] Or Err =
    val a = new Array[Byte](((encoded.length.toLong*3)/4).toInt)
    decodeRangeInto(encoded, 0, encoded.length)(a, 0).map(i => a.shrinkCopy(i))
}


object EioBase85 {
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

  private val encodeAsciiTable: Array[Byte] = Array.tabulate(85)(i => (i+33).toByte)

  private val encodeZmqTable: Array[Byte] = Array(
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

  private val decodeZmqTable = encodeTable_to_decodeTable(encodeZmqTable)

  private val decodeAsciiTable = encodeTable_to_decodeTable(encodeAsciiTable)

  private def encodeRangeWithTable(raw: Array[Byte], i0: Int, iN: Int, table: Array[Byte]): Array[Byte] =
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

  def encodeZmqRange(raw: Array[Byte], i0: Int, iN: Int): Array[Byte] = encodeRangeWithTable(raw, i0, iN, encodeZmqTable)

  def encodeZmq(raw: Array[Byte]): Array[Byte] = encodeRangeWithTable(raw, 0, raw.length, encodeZmqTable)

  def encodeZmq(raw: Array[Int]): Array[Byte] = encodeRangeWithTable(raw.unpackBytes, 0, raw.length*4, encodeZmqTable)

  def stringEncodeZmq(raw: Array[Byte]): String = new String(encodeZmq(raw), ISO_8859_1)

  def encodeAsciiRange(raw: Array[Byte], i0: Int, iN: Int): Array[Byte] = encodeRangeWithTable(raw, i0, iN, encodeAsciiTable)

  def encodeAscii(raw: Array[Byte]): Array[Byte] =
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

  def stringEncodeAscii(raw: Array[Byte]): String = new String(encodeAscii(raw), ISO_8859_1)


  private def decodeRangeWithTable(encoded: Array[Byte], i0: Int, iN: Int, table: Array[Byte]): Array[Byte] Or Err = Or.Ret:
    if i0 < 0 || iN > encoded.length then Err.break(s"Invalid decoding range: $i0 until $iN in array of length ${encoded.length}")
    val l = ((iN - i0).toLong*4)/5
    var a = new Array[Byte](l.toInt)
    var lead: Int = 0
    var rest: Int = 0
    var stored = 0
    var i = i0
    var j = 0
    val bmax = 9 + table.length - 1
    while i < iN do
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
      j += 1
      if n > 2 then
        a(j) = ((rest >>> 16) & 0xFF).toByte
        j += 1
        if n > 3 then
          a(j) = ((rest >>> 8) & 0xFF).toByte
          j += 1
    a.shrinkCopy(j)

  def decodeZmqRange(encoded: Array[Byte], i0: Int, iN: Int): Array[Byte] Or Err = decodeRangeWithTable(encoded, i0, iN, decodeZmqTable)

  def decodeAsciiRange(encoded: Array[Byte], i0: Int, iN: Int): Array[Byte] Or Err = decodeRangeWithTable(encoded, i0, iN, decodeAsciiTable)

  def decodeZmq(encoded: Array[Byte]): Array[Byte] Or Err = decodeRangeWithTable(encoded, 0, encoded.length, decodeZmqTable)

  def decodeAscii(encoded: Array[Byte]): Array[Byte] Or Err = decodeRangeWithTable(encoded, 0, encoded.length, decodeAsciiTable)

  private def decodeRangeWithTable(encoded: String, i0: Int, iN: Int, table: Array[Byte]): Array[Byte] Or Err = Or.Ret:
    if i0 < 0 || iN > encoded.length then Err.break(s"Invalid decoding range: $i0 until $iN in string of length ${encoded.length}")
    val l = ((iN - i0).toLong*4)/5
    var a = new Array[Byte](l.toInt)
    var lead: Int = 0
    var rest: Int = 0
    var stored = 0
    var i = i0
    var j = 0
    val bmax = 9 + table.length - 1
    while i < iN do
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
      j += 1
      if n > 2 then
        a(j) = ((rest >>> 16) & 0xFF).toByte
        j += 1
        if n > 3 then
          a(j) = ((rest >>> 8) & 0xFF).toByte
          j += 1
    a.shrinkCopy(j)

  def decodeZmqRange(encoded: String, i0: Int, iN: Int): Array[Byte] Or Err = decodeRangeWithTable(encoded, i0, iN, decodeZmqTable)

  def decodeAsciiRange(encoded: String, i0: Int, iN: Int): Array[Byte] Or Err = decodeRangeWithTable(encoded, i0, iN, decodeAsciiTable)

  def decodeZmq(encoded: String): Array[Byte] Or Err = decodeRangeWithTable(encoded, 0, encoded.length, decodeZmqTable)

  def decodeAscii(encoded: String): Array[Byte] Or Err = decodeRangeWithTable(encoded, 0, encoded.length, decodeAsciiTable)
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
  inline def stringEncode64lines = EioBase64.stringEncodeUrl(underlying, 76)

  inline def stringEncode85 = EioBase85.stringEncodeZmq(underlying)
  inline def stringEncode85zmq = EioBase85.stringEncodeZmq(underlying)
  inline def stringEncode85ascii = EioBase85.stringEncodeAscii(underlying)

  inline def encode64 = Base64.getUrlEncoder.encode(underlying)
  inline def encode64basic = Base64.getEncoder().encode(underlying)
  inline def encode64url = Base64.getUrlEncoder.encode(underlying)
  inline def encode64mime = Base64.getMimeEncoder().encode(underlying)
  inline def encode64lines = EioBase64.encodeUrl(underlying, 76)

  inline def decode64 = EioBase64.decode(underlying)
  inline def decode64basic = safe{ Base64.getDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  inline def decode64url = safe{ Base64.getUrlDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  inline def decode64mime = safe{ Base64.getMimeDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))

  inline def encode85 = EioBase85.encodeZmq(underlying)
  inline def encode85zmq = EioBase85.encodeZmq(underlying)
  inline def encode85ascii = EioBase85.encodeAscii(underlying)

  inline def decode85 = EioBase85.decodeZmq(underlying)
  inline def decode85zmq = EioBase85.decodeZmq(underlying)
  inline def decode85ascii = EioBase85.decodeAscii(underlying)
}

extension (underlying: String) {
  inline def bytes: Array[Byte] = underlying.getBytes(UTF_8)

  inline def decode64 = EioBase64.decode(underlying)
  inline def decode64basic = safe{ Base64.getDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  inline def decode64url = safe{ Base64.getUrlDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  inline def decode64mime = safe{ Base64.getMimeDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))

  inline def decode85 = EioBase85.decodeZmq(underlying)
  inline def decode85zmq = EioBase85.decodeZmq(underlying)
  inline def decode85ascii = EioBase85.decodeAscii(underlying)
}

extension (underlying: java.util.zip.ZipEntry) {
  def cleanName =
    val n = underlying.getName
    val i = n.indexOf('/')
    val j = n.indexOf('\\')
    if i < 0 && j > 0 then n.replace('\\', '/') else n

  def cleanPath =
    val n = underlying.cleanName
    val fs = FileSystems.getDefault
    fs.getPath(if fs.getSeparator == "/" then n else n.replace("/", fs.getSeparator))
}
