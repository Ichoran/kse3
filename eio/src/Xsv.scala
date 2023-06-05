// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio

import java.io._
import java.nio._
import java.nio.file._
import java.nio.channels.{ReadableByteChannel, WritableByteChannel, SeekableByteChannel}
import java.nio.charset.StandardCharsets._

import scala.collection.mutable.Builder
import scala.util.boundary
import scala.annotation.targetName
import scala.collection.{ IterableOnce => IOnce }

import kse.flow.{given, _}
import kse.maths.{given, _}
import kse.maths.packed.{given, _}
import kse.eio._


class Xsv private (
  separator: Char,
  permissiveWhitespace: Boolean = false
) {
  // Current parse line
  private var line: UInt = UInt(0)

  // Current parse position within line
  private var pos: Int = 0

  // Index within current buffer: points at the next character to consume.
  private var index: Int = 0

  // Token parsing state--states are bits so we can test for various combinations of them with one operation
  private var state: Int = 0

  private inline val Tk = 0x01   // Inside a token
  private inline val Tn = 0x02   // Just after a LF
  private inline val Tr = 0x04   // Just after a CR
  private inline val Tc = 0x08   // Just after a separator
  private inline val Q1 = 0x10   // Just started a quote
  private inline val Qt = 0x20   // Inside a quote
  private inline val Qr = 0x40   // Just hit a CR inside a quote
  private inline val Qq = 0x80   // Just after a quote when previously inside a quote
  private inline val Sp = 0x100  // Removing whitespace after the end of a quote

  private inline val EoI = 0x1  // End of input is just the end of this bit of data, nothing special
  private inline val EoL = 0x2  // End of input is also an end of line
  private inline val EoF = 0x4  // There is no more input to be had: "file" or equivalent is finished 

  private def sayWhere(prefix: String): String = s"${prefix} on line ${line.toLong + 1}, position $pos"

  private given AutoMap[Err, Err] = _.explainBy(sayWhere("Error"))

  private def clear(): Unit =
    line = UInt(0)
    pos = 0
    index = 0
    state = 0

  // Ignores spaces after a quote in input.
  // If input runs out, index will be iN and state will stay Sp
  // Otherwise state will transition to Tn, Tr, or Tc, or an error will be emitted.
  // If the transition is to Tn or Tr, visitor will receive a newline call.
  // Assumes 0 <= i0 <= iN <= data.length, and Sp state on entry.
  private transparent inline def skipSpaceImpl[T <: Array[Byte] | String, U, C <: Byte | Char](
    data: T, inline lookup: (T, Int) => C, i0: Int, iN: Int, visitor: Xsv.Visitor[T, _]
  ): Unit Or Err =
    Or.Ret:
      var i = i0
      while i < iN do
        val c = lookup(data, i)
        c match
          case x if x == separator =>
            pos += i - i0
            index = i + 1
            state = Tc
            Is.unit.break
          case ' '  | '\t' =>
            i += 1
          case '\r' | '\n' =>
            line = line +# UInt(1)
            pos = 0
            index = i + 1
            state = if c == '\n' then Tn else Tr
            visitor.newline(line).?*
          case _ =>
            pos += i - i0
            index = i
            visitor.error(Err(sayWhere("Non-space cell content after quote"))).asAlt.break
      pos += i - i0
      index = i
      Is.unit

  private def skipSpace[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], _]): Unit Or Err =
    skipSpaceImpl[Array[Byte], U, Byte](data, (a, i) => a(i), i0, iN, visitor)

  private def skipSpace[U](data: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, _]): Unit Or Err =
    skipSpaceImpl[String, U, Char](data, (s, i) => s.charAt(i), i0, iN, visitor)

  // Loads quoted input into a visitor.
  // State will be Qr, Qq, or Qt if we are not known to be done;
  //   Tn, Tr, Tc, or Sp if we are (in which case endquote will have been called)
  // If incoming state is Q1, quoted will always be called.  Otherwise it will be called on any processed input.
  // Assumes 0 <= i0 < iN <= data.length, and a Q state on entry.
  private transparent inline def visitQuoteImpl[T <: Array[Byte] | String, U, C <: Byte | Char](
    data: T, inline lookup: (T, Int) => C, inline lf: T, i0: Int, iN: Int, visitor: Xsv.Visitor[T, U]
  ): Unit Or Err =
    Or.Ret:
      var i = i0
      while i < iN do
        // First we mark where we've gotten to successfully, and see if there's any state to handle.
        index = i
        state match
          case Qr =>
            if lookup(data, i) == '\n' then i += 1
            else visitor.quoted(lf, 0, 1)   // Found \r alone but we like \n so we emit one
            state = Qt
          case Qq =>
            // Found what could be an ending quote, but is it really?
            val c = lookup(data, i)
            if c == '"' then
              // No, it was an embedded quote.  Pick up the extra quote.
              i += 1
              state = Qt
            else if c == '\n' || c == '\r' then
              // The quote is over and we've reached the end of a line
              line = line +# UInt(1)
              pos = 0
              index = i + 1
              state = if c == '\n' then Tn else Tr
              (visitor.endquote() && visitor.newline(line)).?*
              Is.unit.break
            else if c == separator then
              // The quote is over and we've reached the end of a cell
              pos += 1
              index = i + 1
              state = Tc
              visitor.endquote().?*
              Is.unit.break
            else if permissiveWhitespace && (c == ' ' || c == '\t') then
              // The quote is over but there's whitespace after it and we've promised we'll handle it.
              pos += 1
              index = i + 1
              state = Sp
              visitor.endquote().?*
              Is.unit.break
            else
              // The quote is over and there's some junk here.
              visitor.error(Err(sayWhere("Non-space cell content after quote"))).asAlt.break
          case _ =>
        // Now we know we're inside a quote so we try to walk forwards until something happens.
        while i < iN && (state & (Qq | Qr)) == 0 do
          lookup(data, i) match
            case '"' =>
              // Quote might have ended!  Save anything we found.
              if index < i || state == Q1 then
                visitor.quoted(data, index, i)
              i += 1
              pos += i - index
              index = i
              state = Qq
            case '\r' =>
              // Might be an annoying Windows newline.  We definitely advanced a line, but let state handler deal.
              if index < i || state == Q1 then
                visitor.quoted(data, index, i)
              line = line +# UInt(1)
              pos = 0
              i += 1
              index = i
              state = Qr
            case '\n' =>
              // Regular newline--intercept to update line count, but we can just keep scanning input.
              line = line +# UInt(1)
              pos = 0
              i += 1
            case _ =>
              // Quoted text. Just pass it.
              i += 1
        // At this point, we're either out of input, or saved our progress and changed state
      // At this point we know we're out of input.  Save progress if we're missing anything.
      if index < i || state == Q1 then
        visitor.quoted(data, index, i)
        index = i
        state = Qt
      ()

  private def visitQuote[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U]): Unit Or Err =
    visitQuoteImpl[Array[Byte], U, Byte](data, (d, i) => d(i), Xsv.lfByte, i0, iN, visitor)

  private def visitQuote[U](data: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, U]): Unit Or Err =
    visitQuoteImpl[String, U, Char](data, (d, i) => d(i), "\n", i0, iN, visitor)

  private transparent inline def trimmedImpl[T <: Array[Byte] | String, U, C <: Byte | Char](
    data: T, inline lookup: (T, Int) => C, i0: Int, iN: Int, visitor: Xsv.Visitor[T, U]
  ): Unit Or Err =
    var jN = iN
    while jN > i0 && lookup(data, jN-1).fn(c => c == ' ' || c == '\t') do jN -= 1
    var j0 = i0
    while j0 < jN && lookup(data, j0).fn(c => c == ' ' || c == '\t') do j0 += 1
    visitor.unquoted(data, j0, jN)

  private def trimmed[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U]): Unit Or Err =
    trimmedImpl[Array[Byte], U, Byte](data, (d, i) => d(i), i0, iN, visitor)

  private def trimmed[U](data: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, U]): Unit Or Err =
    trimmedImpl[String, U, Char](data, (s, i) => s.charAt(i), i0, iN, visitor)

  private transparent inline def tokLineImpl[T <: Array[Byte] | String, U, C <: Byte | Char](
    data: T, inline lookup: (T, Int) => C,
    inline trim: (T, Int, Int, Xsv.Visitor[T, U]) => Unit Or Err,
    i: Int, visitor: Xsv.Visitor[T, U]
  ): Unit Or Err =
    Or.Ret:
      (if permissiveWhitespace then trim(data, index, i, visitor) else visitor.unquoted(data, index, i)).?*
      line = line +# UInt(1)
      pos = 0
      visitor.newline(line).?*
      index = i+1
      ()

  private def tokLine[U](data: Array[Byte], i: Int, visitor: Xsv.Visitor[Array[Byte], U]): Unit Or Err =
    tokLineImpl[Array[Byte], U, Byte](data, (d, i) => d(i), (d, i, j, v) => trimmed(d, i, j, v), i, visitor)

  private def tokLine[U](data: String, i: Int, visitor: Xsv.Visitor[String, U]): Unit Or Err =
    tokLineImpl[String, U, Char](data, (s, i) => s.charAt(i), (s, i, j, v) => trimmed[U](s, i, j, v), i, visitor)


  // Loads input into a visitor.
  // Index is advanced to the last valid data--this will be iN unless state ends as Tk with e == EoI (which means more data is needed)
  // Any state can be emitted if e == EoI, but if it's a Q state it means that endquote has not been called.
  // If e & EoL != 0, a virtual \n will be inserted at the end of the input.  Any Q state can be emitted but otherwise only Tn can be
  // If e & EoD != 0, a trailing cell is terminated and considered complete (Tn state will be set.)
  // Assumes 0 <= i0 <= iN <= data.length.  Empty input okay.  Any state okay.
  private transparent inline def visitRangeImpl[T <: Array[Byte] | String, U, C <: Byte | Char](
    data: T,
    inline lookup: (T, Int) => C,
    inline sspace: (T, Int, Int, Xsv.Visitor[T, U]) => Unit Or Err,
    inline vquote: (T, Int, Int, Xsv.Visitor[T, U]) => Unit Or Err,
    inline trim: (T, Int, Int, Xsv.Visitor[T, U]) => Unit Or Err,
    inline tokl: (T, Int, Xsv.Visitor[T, U]) => Unit Or Err,
    inline lf: T,
    i0: Int, iN: Int, visitor: Xsv.Visitor[T, U], e: Int
  ): Unit Or Err =
    Or.Ret:
      var i = i0
      while i < iN do
        state match
          case s if (s & (Q1 | Qr | Qq | Qt)) != 0 =>
            vquote(data, i, iN, visitor).?
            i = index
          case Sp =>
            sspace(data, i, iN, visitor).?
            i = index
          case _ =>
            boundary:  // Exit on quote
              while i < iN do
                lookup(data, i) match
                  case '"' =>
                    if i > index then
                      if !permissiveWhitespace || { while index < i && lookup(data, index).fn(c => c == ' ' || c == '\t') do { index += 1; pos += 1 }; index < i } then
                        visitor.error(Err(sayWhere("Extra content before quote."))).asAlt.break
                    i += 1
                    index = i
                    state = Q1
                    boundary.break()
                  case '\n' =>
                    if state == Tr && i == index then
                      i += 1
                      index = i
                      state = Tn
                    else
                      tokl(data, i, visitor).?
                      i = index
                      state = Tn
                  case '\r' =>
                    tokl(data, i, visitor).?
                    i = index
                    state = Tr
                  case c if c == separator =>
                    (if permissiveWhitespace then trim(data, index, i, visitor) else visitor.unquoted(data, index, i)).?*
                    i += 1
                    pos += i - index
                    index = i
                    state = Tc
                  case _ =>
                    i += 1
      // If we get here, we're at the end of the input and need to figure out what that means
      if (e & EoL) != 0 then
        // Manually add a newline
        if (state & (Q1 | Qq | Qt | Qr)) != 0 then
          if state == Qq then
            visitor.endquote().?*
            state = Tn
          else if state != Qr then visitor.quoted(lf, 0, 1)
          line = line +# UInt(1)
          pos = 0
        else tokl(data, i, visitor).?
      if (e & EoF) != 0 then
        // Make sure we consumed all the data and are not in a quote
        if (state & (Q1 | Qq | Qt | Qr)) != 0 then
          if state == Qq then visitor.endquote().?*
          else visitor.error(Err(sayWhere("Input ended inside of quote"))).asAlt.break
        else if i > index || state == Tc then
          (if permissiveWhitespace then trim(data, index, i, visitor) else visitor.unquoted(data, index, i)).?*
          index = i
      ()

  private def visitRange[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U], e: Int): Unit Or Err =
    visitRangeImpl[Array[Byte], U, Byte](
      data, (d, i) => d(i),
      (d, i, j, v) => skipSpace(d, i, j, v),
      (d, i, j, v) => visitQuote(d, i, j, v),
      (d, i, j, v) => trimmed(d, i, j, v),
      (d, i, v) => tokLine(d, i, v),
      Xsv.lfByte,
      i0, iN, visitor, e
    )

  private def visitRange[U](data: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, U], e: Int): Unit Or Err =
    visitRangeImpl[String, U, Char](
      data, (s, i) => s.charAt(i),
      (s, i, j, v) => skipSpace(s, i, j, v),
      (s, i, j, v) => visitQuote(s, i, j, v),
      (s, i, j, v) => trimmed(s, i, j, v),
      (s, i, v) => tokLine(s, i, v),
      "\n",
      i0, iN, visitor, e
    )

  def visit[U](content: Array[Byte], visitor: Xsv.Visitor[Array[Byte], U]): U Or Err =
    clear()
    visitor.clear()
    visitRange(content, 0, content.length, visitor, EoF) && visitor.complete(line)

  def visit[U](content: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U]): U Or Err =
    clear()
    visitor.clear()
    val j0 = 0 max i0
    val jN = (iN min content.length) max j0
    index = j0
    visitRange(content, j0, jN, visitor, EoF) && visitor.complete(line)

  @targetName("visitByteArrays")
  def visit[U](content: IOnce[Array[Byte]], visitor: Xsv.Visitor[Array[Byte], U]): U Or Err =
    Or.FlatRet:
      clear()
      visitor.clear()
      var buffer: Array[Byte] = null
      var k: Int = 0
      val it = content.iterator
      var more = it.hasNext
      while more do
        val a = it.next
        more = it.hasNext
        index = 0
        if k > 0 then
          var n = (128 min a.length) min (buffer.length - k)
          a.copyRangeInto(0, n)(buffer, k)
          var moreExtra = n > 0
          while index < k && moreExtra do
            visitRange(buffer, index, k + n, visitor, if !more && n == a.length then EoF else EoI).?
            if index < k then
              var m = ((4L * n) min a.length.toLong).toInt
              if m > buffer.length - k then
                val h = (((k.toLong + m) max (2L * buffer.length)) min (Int.MaxValue - 7L)).toInt
                if h <= buffer.length then visitor.error(Err(sayWhere("Buffer overflow"))).asAlt.break
                val b = new Array[Byte](h)
                buffer.copyRangeInto(0, k + n)(b)
                buffer = b
                if m > b.length - k then m = b.length - k
              moreExtra = m > n
              a.copyRangeInto(n, m)(buffer, k + n)
              n = m
          if index >= k then
            index -= k
            k = 0
        if index < a.length then
          visitRange(a, index, a.length, visitor, if more then EoI else EoF).?
          if index < a.length then
            k = a.length - index
            if (buffer eq null) || buffer.length - 128 < k then
              val g = if buffer eq null then 64 else buffer.length
              val h = (((k + 128L) max (2L * g)) min (Int.MaxValue - 7L)).toInt
              if k >= h then visitor.error(Err(sayWhere(s"Buffer overflow"))).asAlt.break
              buffer = new Array[Byte](h)
            a.copyRangeInto(index, a.length)(buffer)
          else k = 0
      visitor.complete(line)

  def visit[U](content: String, visitor: Xsv.Visitor[String, U]): U Or Err =
    clear()
    visitor.clear()
    visitRange(content, 0, content.length, visitor, EoF) && visitor.complete(line)

  def visit[U](content: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, U]): U Or Err =
    clear()
    visitor.clear()
    val j0 = 0 max i0
    val jN = (iN min content.length) max j0
    index = j0
    visitRange(content, j0, jN, visitor, EoF) && visitor.complete(line)

  @targetName("visitStrings")
  def visit[U](content: IOnce[String], visitor: Xsv.Visitor[String, U]): U Or Err =
    Or.FlatRet:
      clear()
      visitor.clear()
      val it = content.iterator
      var more = it.hasNext
      while more do
        index = 0
        val s = it.next
        visitRange(s, 0, s.length, visitor, { more = it.hasNext; if more then EoL else EoF })
        if index < s.length then visitor.error(Err(sayWhere(s"Only consumed $index of ${s.length} characters"))).asAlt.break
      visitor.complete(line)

  def visitInputStream[U](content: InputStream, visitor: Xsv.Visitor[Array[Byte], U], startBufferSize: Int = 256, maxBufferSize: Int = 4194304): U Or Err =
    Or.FlatRet:
      nice{ visit(Send.IterateInputStream(content, startBufferSize, maxBufferSize), visitor).? }

  def visit[U](content: InputStream, visitor: Xsv.Visitor[Array[Byte], U]): U Or Err = visitInputStream[U](content, visitor)

  def visitByteChannel[U](content: ReadableByteChannel, visitor: Xsv.Visitor[Array[Byte], U], startBufferSize: Int = 256, maxBufferSize: Int = 4194304): U Or Err =
    Or.FlatRet:
      nice{ visit(Send.IterateByteChannel(content, startBufferSize, maxBufferSize), visitor).? }

  def visit[U](content: ReadableByteChannel, visitor: Xsv.Visitor[Array[Byte], U]): U Or Err = visitByteChannel[U](content, visitor)

  def visit[U](content: Path, visitor: Xsv.Visitor[Array[Byte], U]): U Or Err =
      if !content.exists then Err.or(s"File not found: $content")
      else Resource.Nice(content.openRead(0))(_.close): input =>
        val sz = content.raw.size
        var n = sz.clamp(256, 4194304).toInt
        if sz > n && sz - n < 65536 then n = 2621440
        visit(Send.IterateInputStream(input, n, n), visitor).?

  def decode[U](content: Array[Byte]        )(using gv: Xsv.GetVisitor[Array[Byte], U]): U Or Err = visit(content, gv.get(content.length))
  @targetName("decodeByteArrays")
  def decode[U](content: IOnce[Array[Byte]] )(using gv: Xsv.GetVisitor[Array[Byte], U]): U Or Err = visit(content, gv.get(-1))
  def decode[U](content: String             )(using gv: Xsv.GetVisitor[String,      U]): U Or Err = visit(content, gv.get(content.length))
  @targetName("decodeStrings")
  def decode[U](content: IOnce[String]      )(using gv: Xsv.GetVisitor[String,      U]): U Or Err = visit(content, gv.get(-1))
  def decode[U](content: InputStream        )(using gv: Xsv.GetVisitor[Array[Byte], U]): U Or Err = visit(content, gv.get(-1))
  def decode[U](content: ReadableByteChannel)(using gv: Xsv.GetVisitor[Array[Byte], U]): U Or Err = visit(content, gv.get(-1))
  def decode[U](path: Path                  )(using gv: Xsv.GetVisitor[Array[Byte], U]): U Or Err = visit(path,    gv.get(path.size))
}
object Xsv {
  def create(separator: Char, permissiveWhitespace: Boolean = false): Xsv Or Err =
    if separator > 127 then Err.or("Xsv only supports basic one-byte separators (0-127)")
    else if separator == '\n' || separator == '\r' then Err.or("Separator cannot be a newline character")
    else if separator == '"' then Err.or("Separator cannot be a double quote character")
    else Is(new Xsv(separator, permissiveWhitespace))

  def comma = Xsv.create(',') .get
  def tab   = Xsv.create('\t').get
  def space = Xsv.create(' ') .get
  def semi  = Xsv.create(';') .get

  def trimComma = Xsv.create(',',  permissiveWhitespace = true) .get
  def trimTab   = Xsv.create('\t', permissiveWhitespace = true) .get
  def trimSpace = Xsv.create(' ',  permissiveWhitespace = true) .get
  def trimSemi  = Xsv.create(';',  permissiveWhitespace = true) .get

  val lfByte = Array[Byte]('\n'.toByte)

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
    def clear(): this.type
    def unquoted(data: T, start: Int, end: Int): Unit Or Err
    def quoted(data: T, start: Int, end: Int): Unit
    def endquote(): Unit Or Err
    def newline(line: UInt): Unit Or Err
    def complete(line: UInt): U Or Err
    def error(err: Err): Err
  }
  object Visitor {
    val emptyRow = Array("")
    val emptyTable = Array.empty[Array[String]]

    abstract class ToStringTable[T <: Array[Byte] | String](strictlyRectangular: Boolean)
    extends Visitor[T, Array[Array[String]]] {
      private var table: Array[Array[String]] = null
      private var row: Array[String] = null
      private var rowIdx: Int = 0
      private var colIdx: Int = 0
      protected var q: String | java.lang.StringBuilder | Null = null

      def clear(): this.type =
        table = null
        row = null
        rowIdx = 0
        colIdx = 0
        q = null
        this

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
            return Err.or(s"Row $rowIdx has ${table(rowIdx-1).length} columns but row ${rowIdx+1} has $colIdx")
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

      def error(err: Err): Err =
        clear()
        err
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
          sb append data.substring(start, end)
          q = sb
        case sb: java.lang.StringBuilder =>
          sb append data.substring(start, end)
    }


    final class TableFromBytes(strictRect: Boolean = false)
    extends ToStringTable[Array[Byte]](strictRect) {
      def unquoted(data: Array[Byte], start: Int, end: Int): Unit Or Err =
        if q ne null then Err.or("New token in middle of quote")
        addToRow(new String(data, start, end-start))

      def quoted(data: Array[Byte], start: Int, end: Int): Unit =
        val x = if start == 0 && end == 1 && data(0) == '\n' then "\n" else new String(data, start, end-start)
        q match
          case null => q = x
          case s: String =>
            val sb = new java.lang.StringBuilder
            sb append s
            sb append x
            q = sb
          case sb: java.lang.StringBuilder =>
            sb append x
    }

    def onString(strictRect: Boolean = false) = new TableFromString(strictRect)

    def onBytes( strictRect: Boolean = false) = new TableFromBytes( strictRect)
  }
}

object Csv {
  def decode(content: Array[Byte]       ): Array[Array[String]] Or Err = Xsv.comma.decode(content)
  @targetName("decodeByteArrays")
  def decode(content: IOnce[Array[Byte]]): Array[Array[String]] Or Err = Xsv.comma.decode(content)
  def decode(content: String            ): Array[Array[String]] Or Err = Xsv.comma.decode(content)
  @targetName("decodeStrings")
  def decode(content: IOnce[String]     ): Array[Array[String]] Or Err = Xsv.comma.decode(content)
  def decode(content: InputStream       ): Array[Array[String]] Or Err = Xsv.comma.decode(content)
  def decode(path: Path                 ): Array[Array[String]] Or Err = Xsv.comma.decode(path)
}

object Tsv {
  def decode(content: Array[Byte]       ): Array[Array[String]] Or Err = Xsv.tab.decode(content)
  @targetName("decodeByteArrays")
  def decode(content: IOnce[Array[Byte]]): Array[Array[String]] Or Err = Xsv.tab.decode(content)
  def decode(content: String            ): Array[Array[String]] Or Err = Xsv.tab.decode(content)
  @targetName("decodeStrings")
  def decode(content: IOnce[String]     ): Array[Array[String]] Or Err = Xsv.tab.decode(content)
  def decode(content: InputStream       ): Array[Array[String]] Or Err = Xsv.tab.decode(content)
  def decode(path: Path                 ): Array[Array[String]] Or Err = Xsv.tab.decode(path)
}
