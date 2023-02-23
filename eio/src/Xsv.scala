// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio

import java.io._
import java.nio._
import java.nio.file._
import java.nio.channels.{ReadableByteChannel, WritableByteChannel, SeekableByteChannel}
import java.nio.charset.StandardCharsets._

import scala.collection.mutable.Builder

import kse.flow.{given, _}
import kse.maths.{given, _}
import kse.maths.packed.{given, _}


class Xsv[A](
  separator: Char,
  newline: String = "\n",
  strictlyRectangular: Boolean = false,
  permissiveWhitespace: Boolean = false
) {
  if separator > 127 then throw new IllegalArgumentException("Xsv only supports basic one-byte separators (0-127)")
  if separator == '\n' || separator == '\r' then throw new IllegalArgumentException("Separator cannot be a newline character")
  if separator == '"' then throw new IllegalArgumentException("Separator cannot be a double quote character")

  def readFrom(p: Path): Array[Xsv.Data] Or Err = p.gulp.flatMap(decode)

  def writeTo(p: Path, content: Array[Array[String]]): Unit Or Err = ???

  private var line: UInt = 0
  private var char: Int = 0
  private var erronious: Int = 0

  /** Returns the number of characters consumed if the read completed, or
    * -n-1 of the number of characters at which the last valid data was read
    * if we ran out of input and need to load more (and more is available).
    * As a side-effect, line and char will be updated to the last line where
    * there was valid data, and if there is an error, erronious will be set
    * to a non-zero error value.
    */
  private def decodeRow(row: Builder[Array[String]], data: Array[Byte], i0: Int, moreAvailable: Boolean): Int =
    erronious = false
    var i = i0
    var j = i0
    var q = false
    var c0 = i0 - char
    while i < bytes.length do
      bytes(i) match
        case b if b == '\n' || b == '\r' =>
          line += 1
          val k = i
          if b == '\r' && i+1 < bytes.length && bytes(i+1) == '\n' then i += 1
          if q then
            c0 = i+1
          else
            char = 0
            row += (if k <= j then "" else new String(data, j, k))
            return i+1
        case '"' =>
          if q then ???
          else
            if permissiveWhitespace then while j < i && b(j) == ' ' do j += 1
            if i > j then return Pack.L(-i-1, lines + 1)
            j = i+1
            i = j
            while i < bytes.length && bytes(i) != '"' do i += 1
            if i == 
        case b if b == separator && !q =>
          row += (if i <= j then "" else new String(data, j, i))
          j = i+1
        case _ =>
      i += 1
    row += (if i <= j then "" else new String(data, j, i))
    Pack.L(i, lines+1)

  def decode(bytes: Array[Byte]): Array[Array[String]] Or Err =
    line = 0
    char = 0
    erronious = false
    val rows = Array.newBuilder[Array[String]]
    var row: Builder[Array[String]] = null
    var i = 0
    while i < bytes.length && erronious != 0 do
      if row ne null then rows += row.result
      val row = Array.newBuilder[String]
      val k = decodeRow(row, bytes, i, false)
      i += k
    if erronious != 0 then
    else
      if row ne null then
        val rr = row.result
        if rr.length > 0 then rows += rr
      Is(rows.result)


    var row: Builder[Array[String]] = null
    var i = 0
    var line = 0
    while i < bytes.length
      val b = bytes(i)
      i += 1
      if b == '\n' then
        lines += 1
        if row ne null then
          rows += row.result
          row = null
      else if b == '\r' then
        if i < bytes.length && bytes(i) == '\n' then i += 1
        lines += 1
        if row ne null then
          rows += row.result
          row = null
      else if b == '"' then
        ???
      else if b == bsep then
        if row eq null then row = Array.newBuilder[String]
        row += ""
      else



  def decode(buffer: ByteBuffer): Array[Array[String]] Or Err =
    decode(buffer.getBytes)

  def decode(content: String): Array[Array[String]] Or Err = ???
  def decode(lines: Array[String]): Array[Array[String]] Or Err = ???

  def decode(input: InputBuffer): Array[Array[String]] Or Err = Err.Or:
    val mac = new MultiArrayChannel()
    nice{ mac.read(input) }.?
    decode(mac.getBytes)

  def decode(seeker: SeekableByteChannel): Array[Array[String]] Or Err =
    decode(seeker.input)

  def decode(multi: MultiArrayChannel): Array[Array[String]] Or Err =
    decode(multi.getBytes)
}
object Xsv {
  def comma = new Xsv(',')
  def tab   = new Xsv('\t')
  def space = new Xsv(' ')
  def semi  = new Xsv(';')
}

