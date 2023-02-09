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

object EioBase64Helper {
  val mimeSeparator = Array[Byte]('\n')

  private val decodeTable: Array[Byte] = Array(
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

  def decodeAnyBase64(encoded: Array[Byte]): Array[Byte] Or Err = Or.Ret:
    val a = new Array[Byte](((encoded.length.toLong*3 + 2)/4).toInt)
    var bits = 0
    var nb = 0
    var i = 0
    aFor(encoded){ (b, j) => 
      val y =
        if b >= 9 && b <= 122 then decodeTable(b-9)
        else -1
      if y == -1 then Err.break(s"Invalid Base64 character ${b.toChar} at index $j")
      if y != 64 then 
        bits = (bits << 6) | y
        nb += 6
      if nb >= 24 then
        a(i) = bits.byte(2); i += 1
        a(i) = bits.byte(1); i += 1
        a(i) = bits.byte(0); i += 1
        bits = 0
        nb = 0
    }
    if nb >= 8 then
      bits = bits << (24 - nb)
      a(i) = bits.byte(2)
      bits = bits & 0xFFFF
      i += 1
      if nb >= 16 then
        a(i) = bits.byte(1)
        bits = bits & 0xFF
        i += 1
    if bits != 0 then Err.break(s"Trailing bits: ${if bits.byte(1) != 0 then bits.byte(1).bitString else bits.byte(0).bitString}; Base64 data must be incomplete")
    a.shrinkCopy(i)
}

extension (underlying: Array[Byte]) {
  def utf8 = new String(underlying, UTF_8)
  def ascii = new String(underlying, US_ASCII)
  def rawString = new String(underlying, ISO_8859_1)
  def iso8859_1 = new String(underlying, ISO_8859_1)
  def buffer = ByteBuffer.wrap(underlying)
  def input = new ByteArrayInputStream(underlying)
  def encode64 = Base64.getEncoder().encode(underlying)
  def encode64url = Base64.getUrlEncoder.encode(underlying)
  def encode64mime = Base64.getMimeEncoder(78, EioBase64Helper.mimeSeparator).encode(underlying)
  def decode64 = safe{ Base64.getDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  def decode64url = safe{ Base64.getUrlDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  def decode64mime = safe{ Base64.getMimeDecoder().decode(underlying) }.mapAlt(t => Err(t.getMessage))
  def decode64any = EioBase64Helper.decodeAnyBase64(underlying)
}

extension (underlying: String) {
  def bytes: Array[Byte] = underlying.getBytes(UTF_8)
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
