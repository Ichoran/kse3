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

  private var line: UInt = UInt(0)
  private var char: Int = 0
  private var erronious: Int = 0
  private var skips: List[Int] = Nil

  private def findMatchingQuote(data: Array[Byte], i0: Int): Int =
    var i = i0
    var l0 = i0 - char
    var cr = false
    while i < data.length do
      data(i) match
        case '"' =>
          char = i - l0
          return i
        case '\r' =>
          line = line +# 1.u
          char = 0
          l0 = i
          cr = true
        case '\n' =>
          if cr then
            l0 = i
            cr = false
          else
            line = line +# 1.u
            char = 0
            l0 = i
        case _ =>
          cr = false
          i += 1
    char = i - l0
    i

  private def findMatchingQuote(data: String, i0: Int): Int =
    var i = i0
    var l0 = i0 - char
    var cr = false
    while i < data.length do
      data.charAt(i) match
        case '"' =>
          char = i - l0
          return i
        case '\r' =>
          line = line +# 1.u
          char = 0
          l0 = i
          cr = true
        case '\n' =>
          if cr then
            l0 = i
            cr = false
          else
            line = line +# 1.u
            char = 0
            l0 = i
        case _ =>
          cr = false
          i += 1
    char = i - l0
    i


  /*
  Find the next quote character, assuming we're already in a string,
  updating `line` and `char` along the way, if necessary.
  Returns the index of the terminal quote, or data.length if none found.
  */
  //private def findMatchingQuote(data: Array[Byte], i0: Int): Int = findMatchingQuoteImpl(data, i0)

  /** Returns the number of characters consumed if the read completed, or
    * -n-1 of the number of characters at which the last valid data was read
    * if we ran out of input and need to load more (and more is available).
    * As a side-effect, line and char will be updated to the last line where
    * there was valid data, and if there is an error, erronious will be set
    * to a non-zero error value.
    */
  /*
  private def decodeRow(row: Builder[Array[String]], data: Array[Byte], i0: Int, moreAvailable: Boolean): Int =
    // Assume line and char are in sensible positions
    erronious = 0
    var i = i0
    var inQuote = false
    // Update these
    var valid = i0
    var l = line
    var c = char
    while i < bytes.length && erronious == 0 do
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
  */

  /*
  def readFrom(p: Path): Array[Xsv.Data] Or Err = p.gulp.flatMap(decode)

  def writeTo(p: Path, content: Array[Array[String]]): Unit Or Err = ???


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

  def decode(input: InputBuffer): Array[Array[String]] Or Err =
    val mac = new MultiArrayChannel()
    nice{ mac.read(input) } >>> decode(mac.getBytes)

  def decode(seeker: SeekableByteChannel): Array[Array[String]] Or Err =
    decode(seeker.input)

  def decode(multi: MultiArrayChannel): Array[Array[String]] Or Err =
    decode(multi.getBytes)
  */
  def decode[U](content: Array[Byte])(using Xsv.GetVisitor[Array[Byte], U]): U Or Err = ???
  def decode[U](content: String)(using Xsv.GetVisitor[String, U]): U Or Err = ???
}
object Xsv {
  def comma = new Xsv(',')
  def tab   = new Xsv('\t')
  def space = new Xsv(' ')
  def semi  = new Xsv(';')

  trait GetVisitor[T <: Array[Byte] | String, U] {
    def get(size: Long): Visitor[T, U]
  }
  object GetVisitor {
    given string2table: GetVisitor[String, Array[Array[String]]] =
      new GetVisitor[String, Array[Array[String]]] {
        def get(size: Long) = new Visitor.TableFromString()
      }

    given bytes2table: GetVisitor[Array[Byte], Array[Array[String]]] =
      new GetVisitor[Array[Byte], Array[Array[String]]] {
        def get(size: Long) = new Visitor.TableFromBytes()
      }
  }

  trait Visitor[T <: Array[Byte] | String, U] {
    def clear(): Unit
    def unquoted(data: T, start: Int, end: Int): Unit Or Err
    def quoted(data: T, start: Int, end: Int): Unit
    def endquote(): Unit Or Err
    def newline(line: UInt): Unit Or Err
    def complete(line: UInt): U Or Err
    def error(err: Err): Unit
  }
  object Visitor {
    val emptyRow = Array.empty[String]
    val emptyTable = Array.empty[Array[String]]

    abstract class ToStringTable[T <: Array[Byte] | String](strictlyRectangular: Boolean)
    extends Visitor[T, Array[Array[String]]] {
      private var table: Array[Array[String]] = null
      private var row: Array[String] = null
      private var rowIdx: Int = 0
      private var colIdx: Int = 0
      protected var q: String | java.lang.StringBuilder | Null = null

      def clear(): Unit =
        table = null
        row = null
        rowIdx = 0
        colIdx = 0
        q = null

      protected def addToRow(cell: String): Unit Or Err =
        if row eq null then
          if (table eq null) || rowIdx == 0 || rowIdx >= table.length then row = new Array[String](4)
          else row = new Array[String](table(rowIdx-1).length)
        else if colIdx >= row.length then
          val m = (colIdx *# 2) min (Int.MaxValue -7)
          if m == row.length then return Err.or(s"Too many columns for row: ${m+1}")
          row = row.copyToSize(m)
        row(colIdx) = cell
        colIdx += 1
        Is.unit

      def endquote(): Unit Or Err = 
        val result = q match
          case null => Err.or(s"Supposed end-quote with no starting quote?")
          case s: String => addToRow(s)
          case sb: java.lang.StringBuilder => addToRow(sb.toString)
        q = null
        result

      def newline(line: UInt): Unit Or Err =
        if q ne null then return Err.or("New row in middle of quote")
        if (row ne null) && colIdx > 0 then
          if table eq null then table = new Array[Array[String]](4)
          else if rowIdx >= table.length then
            val m = (rowIdx *# 2) min (Int.MaxValue - 7)
            if m == table.length then return Err.or(s"Too many lines for table: ${m+1}")
            table = table.copyToSize(m)
          table(rowIdx) = row.shrinkCopy(colIdx)
          var i = rowIdx - 1
          while i >= 0 && (table(i) eq null) do
            table(i) = emptyRow
            i -= 1
          if strictlyRectangular && rowIdx > 0 && table(rowIdx-1).length != colIdx then
            return Err.or(s"Row $rowIdx has ${table(rowIdx-1).length} columns but row ${rowIdx+1} has $colIdx (line ${line.toLong})")
        else
          if (table ne null) && rowIdx < table.length then table(rowIdx) = emptyRow
        row = null
        colIdx = 0
        rowIdx += 1
        Is.unit

      def complete(line: UInt): Array[Array[String]] Or Err = Or.Ret:
        if q ne null then return Err.or("End of table in middle of quote")
        if colIdx > 0 then newline(line).?
        if table eq null then emptyTable
        else
          var i = rowIdx min table.length
          while i > 0 && table(i-1).fn(x => (x eq null) || x.length == 0) do i -= 1
          val ans = table.shrinkCopy(i)
          clear()
          ans

      def error(err: Err): Unit =
        clear()
    }

    final class TableFromString(strictRect: Boolean = false)
    extends ToStringTable[String](strictRect) {
      def unquoted(data: String, start: Int, end: Int): Unit Or Err =
        if q ne null then Err.or(s"New token in middle of quote")
        addToRow(data.substring(start, end))

      def quoted(data: String, start: Int, end: Int): Unit = q match
        case null => q = data.substring(start, end)
        case s: String =>
          val sb = new java.lang.StringBuilder
          sb append s
          sb append '"'
          sb append data.substring(start, end)
          q = sb
        case sb: java.lang.StringBuilder =>
          sb append '"'
          sb append data.substring(start, end)
    }


    final class TableFromBytes(strictRect: Boolean = false)
    extends ToStringTable[Array[Byte]](strictRect) {
      def unquoted(data: Array[Byte], start: Int, end: Int): Unit Or Err =
        if q ne null then Err.or("New token in middle of quote")
        addToRow(new String(data, start, end-start))

      def quoted(data: Array[Byte], start: Int, end: Int): Unit = q match
        case null => q = new String(data, start, end-start)
        case s: String =>
          val sb = new java.lang.StringBuilder
          sb append s
          sb append '"'
          sb append (new String(data, start, end-start))
          q = sb
        case sb: java.lang.StringBuilder =>
          sb append '"'
          sb append (new String(data, start, end-start))
    }
  }
}

