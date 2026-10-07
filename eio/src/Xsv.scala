// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023-26 Rex Kerr, UCSF, and Calico Life Sciences LLC

package kse.eio


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.io._
import java.nio._
import java.nio.file._
import java.nio.channels.{ReadableByteChannel, WritableByteChannel, SeekableByteChannel}
import java.nio.charset.StandardCharsets._

import scala.collection.mutable.Builder
import scala.util.boundary
import scala.annotation.targetName
import scala.collection.{ IterableOnce => IOnce }
import scala.collection.immutable.{Range => Rg}

import kse.basics.{given, _}
import kse.basics.intervals._
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
    data: T, inline lookup: (T, Int) => C, i0: Int, iN: Int, visitor: Xsv.Visitor[T, ?]
  ): Ask[Unit] =
    Or.Ret:
      var i = i0
      while i < iN do
        val c = lookup(data, i)
        c match
          case x if x == separator =>
            pos += i - i0
            index = i + 1
            state = Tc
            Is.unit.break()
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
            visitor.breakWithError(Err(sayWhere("Non-space cell content after quote")))
      pos += i - i0
      index = i

  private def skipSpace[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], ?]): Ask[Unit] =
    skipSpaceImpl[Array[Byte], U, Byte](data, (a, i) => a(i), i0, iN, visitor)

  private def skipSpace[U](data: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, ?]): Ask[Unit] =
    skipSpaceImpl[String, U, Char](data, (s, i) => s.charAt(i), i0, iN, visitor)

  // Loads quoted input into a visitor.
  // State will be Qr, Qq, or Qt if we are not known to be done;
  //   Tn, Tr, Tc, or Sp if we are (in which case endquote will have been called)
  // If incoming state is Q1, quoted will always be called.  Otherwise it will be called on any processed input.
  // Assumes 0 <= i0 < iN <= data.length, and a Q state on entry.
  private transparent inline def visitQuoteImpl[T <: Array[Byte] | String, U, C <: Byte | Char](
    data: T, inline lookup: (T, Int) => C, inline lf: T, i0: Int, iN: Int, visitor: Xsv.Visitor[T, U]
  ): Ask[Unit] =
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
              visitor.endquote().?*
              visitor.newline(line).?*
              Is.unit.break()
            else if c == separator then
              // The quote is over and we've reached the end of a cell
              pos += 1
              index = i + 1
              state = Tc
              visitor.endquote().?*
              Is.unit.break()
            else if permissiveWhitespace && (c == ' ' || c == '\t') then
              // The quote is over but there's whitespace after it and we've promised we'll handle it.
              pos += 1
              index = i + 1
              state = Sp
              visitor.endquote().?*
              Is.unit.break()
            else
              // The quote is over and there's some junk here.
              visitor.breakWithError(Err(sayWhere("Non-space cell content after quote")))
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

  private def visitQuote[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U]): Ask[Unit] =
    visitQuoteImpl[Array[Byte], U, Byte](data, (d, i) => d(i), Xsv.lfByte, i0, iN, visitor)

  private def visitQuote[U](data: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, U]): Ask[Unit] =
    visitQuoteImpl[String, U, Char](data, (d, i) => d(i), "\n", i0, iN, visitor)

  private transparent inline def trimmedImpl[T <: Array[Byte] | String, U, C <: Byte | Char](
    data: T, inline lookup: (T, Int) => C, i0: Int, iN: Int, visitor: Xsv.Visitor[T, U]
  ): Ask[Unit] =
    var jN = iN
    while jN > i0 && lookup(data, jN-1).fn(c => c == ' ' || c == '\t') do jN -= 1
    var j0 = i0
    while j0 < jN && lookup(data, j0).fn(c => c == ' ' || c == '\t') do j0 += 1
    visitor.unquoted(data, j0, jN)

  private def trimmed[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U]): Ask[Unit] =
    trimmedImpl[Array[Byte], U, Byte](data, (d, i) => d(i), i0, iN, visitor)

  private def trimmed[U](data: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, U]): Ask[Unit] =
    trimmedImpl[String, U, Char](data, (s, i) => s.charAt(i), i0, iN, visitor)

  private transparent inline def tokLineImpl[T <: Array[Byte] | String, U, C <: Byte | Char](
    data: T, inline lookup: (T, Int) => C,
    inline trim: (T, Int, Int, Xsv.Visitor[T, U]) => Unit Or Err,
    i: Int, visitor: Xsv.Visitor[T, U]
  ): Ask[Unit] =
    Or.Ret:
      (if permissiveWhitespace then trim(data, index, i, visitor) else visitor.unquoted(data, index, i)).?*
      line = line +# UInt(1)
      pos = 0
      visitor.newline(line).?*
      index = i+1
      ()

  private def tokLine[U](data: Array[Byte], i: Int, visitor: Xsv.Visitor[Array[Byte], U]): Ask[Unit] =
    tokLineImpl[Array[Byte], U, Byte](data, (d, i) => d(i), (d, i, j, v) => trimmed(d, i, j, v), i, visitor)

  private def tokLine[U](data: String, i: Int, visitor: Xsv.Visitor[String, U]): Ask[Unit] =
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
    inline sspace: (T, Int, Int, Xsv.Visitor[T, U]) => Ask[Unit],
    inline vquote: (T, Int, Int, Xsv.Visitor[T, U]) => Ask[Unit],
    inline trim: (T, Int, Int, Xsv.Visitor[T, U]) => Ask[Unit],
    inline tokl: (T, Int, Xsv.Visitor[T, U]) => Ask[Unit],
    inline lf: T,
    i0: Int, iN: Int, visitor: Xsv.Visitor[T, U], e: Int
  ): Ask[Unit] =
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
            boundary[Unit]:  // Exit on quote
              while i < iN do
                lookup(data, i) match
                  case '"' =>
                    if i > index then
                      if !permissiveWhitespace || { while index < i && lookup(data, index).fn(c => c == ' ' || c == '\t') do { index += 1; pos += 1 }; index < i } then
                        visitor.breakWithError(Err(sayWhere("Extra content before quote.")))
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
          else visitor.breakWithError(Err(sayWhere("Input ended inside of quote")))
        else if i > index || state == Tc then
          (if permissiveWhitespace then trim(data, index, i, visitor) else visitor.unquoted(data, index, i)).?*
          index = i
      ()

  private def visitRange[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U], e: Int): Ask[Unit] =
    visitRangeImpl[Array[Byte], U, Byte](
      data, (d, i) => d(i),
      (d, i, j, v) => skipSpace(d, i, j, v),
      (d, i, j, v) => visitQuote(d, i, j, v),
      (d, i, j, v) => trimmed(d, i, j, v),
      (d, i, v) => tokLine(d, i, v),
      Xsv.lfByte,
      i0, iN, visitor, e
    )

  private def visitRange[U](data: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, U], e: Int): Ask[Unit] =
    visitRangeImpl[String, U, Char](
      data, (s, i) => s.charAt(i),
      (s, i, j, v) => skipSpace(s, i, j, v),
      (s, i, j, v) => visitQuote(s, i, j, v),
      (s, i, j, v) => trimmed(s, i, j, v),
      (s, i, v) => tokLine(s, i, v),
      "\n",
      i0, iN, visitor, e
    )

  def visit[U](content: Array[Byte], visitor: Xsv.Visitor[Array[Byte], U]): Ask[U] =
    clear()
    visitor.clear()
    visitRange(content, 0, content.length, visitor, EoF) && visitor.complete(line)

  def visit[U](content: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U]): Ask[U] =
    clear()
    visitor.clear()
    val j0 = 0 max i0
    val jN = (iN min content.length) max j0
    index = j0
    visitRange(content, j0, jN, visitor, EoF) && visitor.complete(line)

  inline def visit[U, R <: Iv.X | Rg](content: Array[Byte], inline v: R, visitor: Xsv.Visitor[Array[Byte], U]): Ask[U] =
    Iv.dispatch(v, content)((i0, iN) => visit(content, i0, iN, visitor))

  @targetName("visitByteArrays")
  def visit[U](content: IOnce[Array[Byte]], visitor: Xsv.Visitor[Array[Byte], U]): Ask[U] =
    Or.FlatRet:
      clear()
      visitor.clear()
      var buffer: Array[Byte] = null
      var k: Int = 0
      val it = content.iterator
      var more = it.hasNext
      while more do
        val a = it.next()
        more = it.hasNext
        index = 0
        if k > 0 then
          var n = (128 min a.length) min (buffer.length - k)
          a.inject(buffer, k)(0, n) __ Unit
          var moreExtra = n > 0 || !more
          while index < k && moreExtra do
            visitRange(buffer, index, k + n, visitor, if !more && n == a.length then EoF else EoI).?
            if index < k then
              var m = ((4L * n) min a.length.toLong).toInt
              if m > buffer.length - k then
                val h = (((k.toLong + m) max (2L * buffer.length)) min (Int.MaxValue - 7L)).toInt
                if h <= buffer.length then visitor.breakWithError(Err(sayWhere("Buffer overflow")))
                buffer = buffer.copyToSize(h)
                if m > buffer.length - k then m = buffer.length - k
              moreExtra = m > n
              a.inject(buffer, k + n)(n, m) __ Unit
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
              if k >= h then visitor.breakWithError(Err(sayWhere(s"Buffer overflow")))
              buffer = new Array[Byte](h)
            a.inject(buffer)(index to End) __ Unit
          else k = 0
      visitor.complete(line)

  def visit[U](content: String, visitor: Xsv.Visitor[String, U]): Ask[U] =
    clear()
    visitor.clear()
    visitRange(content, 0, content.length, visitor, EoF) && visitor.complete(line)

  def visit[U](content: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, U]): Ask[U] =
    clear()
    visitor.clear()
    val j0 = 0 max i0
    val jN = (iN min content.length) max j0
    index = j0
    visitRange(content, j0, jN, visitor, EoF) && visitor.complete(line)

  inline def visit[U, R <: Iv.X | Rg](content: String, inline v: R, visitor: Xsv.Visitor[String, U]): Ask[U] =
    Iv.dispatch(v, content)((i0, iN) => visit(content, i0, iN, visitor))

  @targetName("visitStrings")
  def visit[U](content: IOnce[String], visitor: Xsv.Visitor[String, U]): Ask[U] =
    Or.FlatRet:
      clear()
      visitor.clear()
      val it = content.iterator
      var more = it.hasNext
      while more do
        index = 0
        val s = it.next()
        visitRange(s, 0, s.length, visitor, { more = it.hasNext; if more then EoL else EoF }).?
        if index < s.length then visitor.breakWithError(Err(sayWhere(s"Only consumed $index of ${s.length} characters")))
      visitor.complete(line)

  def visitInputStream[U](content: InputStream, visitor: Xsv.Visitor[Array[Byte], U], startBufferSize: Int = 256, maxBufferSize: Int = 4194304): Ask[U] =
    Or.FlatRet:
      nice{ visit(Send.IterateInputStream(content, startBufferSize, maxBufferSize), visitor).? }

  def visit[U](content: InputStream, visitor: Xsv.Visitor[Array[Byte], U]): Ask[U] = visitInputStream[U](content, visitor)

  def visitByteChannel[U](content: ReadableByteChannel, visitor: Xsv.Visitor[Array[Byte], U], startBufferSize: Int = 256, maxBufferSize: Int = 4194304): Ask[U] =
    Or.FlatRet:
      nice{ visit(Send.IterateByteChannel(content, startBufferSize, maxBufferSize), visitor).? }

  def visit[U](content: ReadableByteChannel, visitor: Xsv.Visitor[Array[Byte], U]): Ask[U] = visitByteChannel[U](content, visitor)

  def visit[U](content: Path, visitor: Xsv.Visitor[Array[Byte], U]): Ask[U] =
      if !content.exists then Err.or(s"File not found: $content")
      else Resource.Nice(content.openRead(0))(_.close): input =>
        val sz = content.raw.size
        var n = sz.clamp(256, 4194304).toInt
        if sz > n && sz - n < 65536 then n = 2621440
        visit(Send.IterateInputStream(input, n, n), visitor).?

  def decode[U](content: Array[Byte]        )(using gv: Xsv.GetVisitor[Array[Byte], U]): Ask[U] = visit(content, gv.get(content.length))
  @targetName("decodeByteArrays")
  def decode[U](content: IOnce[Array[Byte]] )(using gv: Xsv.GetVisitor[Array[Byte], U]): Ask[U] = visit(content, gv.get(-1))
  def decode[U](content: String             )(using gv: Xsv.GetVisitor[String,      U]): Ask[U] = visit(content, gv.get(content.length))
  @targetName("decodeStrings")
  def decode[U](content: IOnce[String]      )(using gv: Xsv.GetVisitor[String,      U]): Ask[U] = visit(content, gv.get(-1))
  def decode[U](content: InputStream        )(using gv: Xsv.GetVisitor[Array[Byte], U]): Ask[U] = visit(content, gv.get(-1))
  def decode[U](content: ReadableByteChannel)(using gv: Xsv.GetVisitor[Array[Byte], U]): Ask[U] = visit(content, gv.get(-1))
  def   read[U](path: Path                  )(using gv: Xsv.GetVisitor[Array[Byte], U]): Ask[U] = visit(path,    gv.get(path.size))

  def bomless: Xsv.Bomless = new Xsv.Bomless(this)

  /** Reads a table of numbers: `Right` with every cell a `Double` when the input is one, and otherwise `Left` with
    * the plain string table, exactly as `decode` gives it.
    *
    * The first row is the column headers if any cell in it is not a number, or always if `colHeaders = "always"`
    * (for headers that look numeric, such as years); the first column is row labels if the first data row's first
    * cell is not a number, or always if `rowLabels = "always"`, and then every row's first cell is its label.  With
    * both, the top-left cell is `corner`, so that `cols(j)` names `table(i)(j)`.  An empty cell is `Double.NA`, the NaN
    * that marks a missing value (`isNA` picks it out); `nan` and `NaN` are NaN, and `inf` or `Infinity` with an optional sign is infinite, in any case, as pandas writes them; any
    * other non-number, or rows of different lengths, gives `Left`.  Blank lines are skipped and a byte order mark
    * is dropped.  The cells are parsed from the bytes where they lie, so the whole input is held in memory to allow
    * the fall back to strings; a stream is read to its end and not closed.
    */
  def dataset(
    content: Array[Byte] | String | InputStream | Path,
    colHeaders: "detect" | "always" = "detect",
    rowLabels: "detect" | "always" = "detect"
  ): Ask[Either[Array[Array[String]], Xsv.Dataset]] =
    val c = colHeaders == "always"
    val r = rowLabels == "always"
    content match
      case b: Array[Byte] => datasetOf(b, c, r)
      case s: String => datasetOf(s, c, r)
      case in: InputStream => nice{ in.readAllBytes() }.flatMap(datasetOf(_, c, r))
      case p: Path => p.gulp.flatMap(datasetOf(_, c, r))

  /** Reads a table column by column, each column the narrowest of `Array[OptBoolean]`, `Array[Double]`, or
    * `Array[String]` that holds every cell in it.
    *
    * A column is Boolean when every non-empty cell is one by `isBool`, which answers 1 for true, 0 for false, and
    * anything else for a cell that is not a Boolean; an empty cell is `OptBoolean.Missing`.  Otherwise it is numbers
    * when every cell is one as `dataset` reads them, an empty cell `Double.NA`; otherwise it is text.  A column named in
    * `forceStrings`, by index or by header, is always text, as for identifiers that look like numbers.  With
    * `colHeaders = "detect"` the first row is the headers when it does not fit the type of a column below it, or
    * when every column is text, which cannot tell; `"always"` and `"never"` decide it.  Rows of different lengths
    * are an error, blank lines are skipped, and a byte order mark is dropped.
    */
  def columnar(
    content: Array[Byte] | String | InputStream | Path,
    colHeaders: "detect" | "always" | "never" = "detect",
    isBool: String => Int = Xsv.isBool,
    forceStrings: Set[Int | String] = Set.empty
  ): Ask[Xsv.ColumnData] =
    val h = if colHeaders == "always" then 1 else if colHeaders == "never" then -1 else 0
    content match
      case b: Array[Byte] => columnarOf(b, h, isBool, forceStrings)
      case s: String => columnarOf(s, h, isBool, forceStrings)
      case in: InputStream => nice{ in.readAllBytes() }.flatMap(columnarOf(_, h, isBool, forceStrings))
      case p: Path => p.gulp.flatMap(columnarOf(_, h, isBool, forceStrings))

  private def columnarOf(content: Array[Byte], h: Int, isBool: String => Int, force: Set[Int | String]): Ask[Xsv.ColumnData] =
    visit(content, if content.hasBOM then 3 else 0, content.length, new Xsv.Visitor.ColumnsFromBytes(h, isBool, force))

  private def columnarOf(content: String, h: Int, isBool: String => Int, force: Set[Int | String]): Ask[Xsv.ColumnData] =
    val i0 = if content.length > 0 && content.charAt(0) == '\uFEFF' then 1 else 0
    visit(content, i0, content.length, new Xsv.Visitor.ColumnsFromString(h, isBool, force))

  private def datasetOf(content: Array[Byte], c: Boolean, r: Boolean): Ask[Either[Array[Array[String]], Xsv.Dataset]] =
    val i0 = if content.hasBOM then 3 else 0
    val v = new Xsv.Visitor.DatasetFromBytes(c, r)
    visit(content, i0, content.length, v).fold(d => Is(Right(d))){ e =>
      if !v.notNumeric then Alt(e)
      else visit(content, i0, content.length, new Xsv.Visitor.TableFromBytes()).map(t => Left(t))
    }

  private def datasetOf(content: String, c: Boolean, r: Boolean): Ask[Either[Array[Array[String]], Xsv.Dataset]] =
    val i0 = if content.length > 0 && content.charAt(0) == '\uFEFF' then 1 else 0
    val v = new Xsv.Visitor.DatasetFromString(c, r)
    visit(content, i0, content.length, v).fold(d => Is(Right(d))){ e =>
      if !v.notNumeric then Alt(e)
      else visit(content, i0, content.length, new Xsv.Visitor.TableFromString()).map(t => Left(t))
    }

  def encode(table: Array[Array[String]]): Iterator[Array[Byte]] =
    Xsv.encodeTable(table, separator)
  def write(table: Array[Array[String]])(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Xsv.encodeTable(table, separator).writeTo(p)

  /** Writes a table of numbers as `dataset` reads it back, and as pandas does: the headers (after `corner`, or an
    * empty cell, when there are row labels) and then each row (after its label), numbers in their shortest
    * round-trip form, `Double.NA` as an empty cell and any other NaN as `NaN` (pandas reads both as NaN), and the
    * infinities as `inf` and `-inf`. */
  def encode(data: Xsv.Dataset): Iterator[Array[Byte]] =
    Xsv.encodeDataset(data, separator)
  def write(data: Xsv.Dataset)(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Xsv.encodeDataset(data, separator).writeTo(p)

  /** Writes columns as `columnar` reads them back, and as pandas does: Booleans as `True` and `False`, numbers as
    * `encode` writes a `Dataset`'s, and a missing Boolean or `Double.NA` as an empty cell.  A short column is padded with
    * empty cells. */
  def encode(data: Xsv.ColumnData): Iterator[Array[Byte]] =
    Xsv.encodeColumns(data, separator)
  def write(data: Xsv.ColumnData)(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Xsv.encodeColumns(data, separator).writeTo(p)
}
object Xsv {
  def create(separator: Char, permissiveWhitespace: Boolean = false): Ask[Xsv] =
    if separator > 127 then Err.or("Xsv only supports basic one-byte separators (0-127)")
    else if separator == '\n' || separator == '\r' then Err.or("Separator cannot be a newline character")
    else if separator == '"' then Err.or("Separator cannot be a double quote character")
    else Is(new Xsv(separator, permissiveWhitespace))

  def comma = Xsv.create(',') .get
  def tab   = Xsv.create('\t').get
  def space = Xsv.create(' ') .get
  def semi  = Xsv.create(';') .get

  def trimComma = Xsv.create(',',  permissiveWhitespace = true).get
  def trimTab   = Xsv.create('\t', permissiveWhitespace = true).get
  def trimSpace = Xsv.create(' ',  permissiveWhitespace = true).get
  def trimSemi  = Xsv.create(';',  permissiveWhitespace = true).get

  val lfByte = Array[Byte]('\n'.toByte)

  /** A table of numbers with its optional headers and labels, as `dataset` reads it and `encode` writes it.
    * `cols(j)` names `table(i)(j)` and `rows(i)` labels `table(i)`; `corner` is the top-left cell, present only
    * when there are both. */
  type Dataset = (corner: Option[String], cols: Option[Array[String]], rows: Option[Array[String]], table: Array[Array[Double]])

  /** A Boolean that may be missing, as a cell of a Boolean column.  An enum, so that an `Array[OptBoolean]` is a
    * class of its own at run time and a `match` on a column's type is checked. */
  enum OptBoolean {
    case False, Missing, True
    inline def isTrue: Boolean = this eq OptBoolean.True
    inline def isFalse: Boolean = this eq OptBoolean.False
    inline def isMissing: Boolean = this eq OptBoolean.Missing
  }
  object OptBoolean {
    inline def apply(b: Boolean): OptBoolean = if b then True else False
  }

  /** A table as columns, as `columnar` reads it: `headers(j)`, when there are headers, names `columns(j)`, which is
    * an `Array[OptBoolean]`, an `Array[Double]`, or an `Array[String]`, so a `match` on the array's type takes it
    * apart. */
  final class ColumnData(val headers: Option[Array[String]], val columns: Array[Array[OptBoolean] | Array[Double] | Array[String]]) {
    /** The number of rows: the length of the longest column. */
    def rows: Int =
      var n = 0
      var j = 0
      while j < columns.length do
        n = n max (columns(j): Array[?]).length
        j += 1
      n
  }

  /** `columnar`'s default Boolean test: `true` is 1 and `false` is 0, in any case, and anything else is -1. */
  val isBool: String => Int = s => if s.equalsIgnoreCase("true") then 1 else if s.equalsIgnoreCase("false") then 0 else -1

  /** The value of `[i0, iN)` as a cell of a dataset, or `EiselLemire.parseFailD` (test with `EiselLemire.failed`)
    * if it is not a number: empty is `Double.NA`, and NaN or an optionally signed infinity may be spelled in any case. */
  private[eio] inline def cellValue(inline at: Int => Int, inline parse: (Int, Int) => Double, i0: Int, iN: Int): Double =
    if i0 == iN then Double.NA
    else
      val d = parse(i0, iN)
      if !EiselLemire.failed(d) then d
      else
        var j = i0
        val c0 = at(j)
        val neg = c0 == '-'
        if neg || c0 == '+' then j += 1
        val m = iN - j
        if m == 3 && (at(j) | 0x20) == 'n' && (at(j+1) | 0x20) == 'a' && (at(j+2) | 0x20) == 'n' then Double.NaN
        else if (m == 3 || m == 8) && (at(j) | 0x20) == 'i' && (at(j+1) | 0x20) == 'n' && (at(j+2) | 0x20) == 'f' &&
                (m == 3 || ((at(j+3) | 0x20) == 'i' && (at(j+4) | 0x20) == 'n' && (at(j+5) | 0x20) == 'i' &&
                            (at(j+6) | 0x20) == 't' && (at(j+7) | 0x20) == 'y')) then
          if neg then Double.NegativeInfinity else Double.PositiveInfinity
        else EiselLemire.parseFailD

  final class Bomless(xsv: Xsv) {
    def decode[U](content: Array[Byte])(using gv: GetVisitor[Array[Byte], U]): Ask[U] =
      if content.hasBOM then xsv.visit(content, 3 to End, gv.get(content.length-3))
      else xsv.decode(content)
    def decode[U](content: IOnce[Array[Byte]])(using gv: Xsv.GetVisitor[Array[Byte], U]): Ask[U] =
      val it = content.iterator
      if !it.hasNext then xsv.decode(it)
      else
        val first = it.next()
        if first.hasBOM then
          if it.hasNext then xsv.decode(Iterator(first.select(3 to End)) ++ it)
          else xsv.visit(first, 3 to End, gv.get(first.length - 3))
        else
          if it.hasNext then xsv.decode(Iterator(first) ++ it)
          else xsv.decode(first)
    def decode[U](content: InputStream)(using gv: Xsv.GetVisitor[Array[Byte], U]): Ask[U] =
      decode(Send.IterateInputStream(content, 256, 4194304))
    def decode[U](content: ReadableByteChannel)(using gv: Xsv.GetVisitor[Array[Byte], U]): Ask[U] =
      decode(Send.IterateByteChannel(content, 256, 4194304))
    def read[U](content: Path)(using gv: Xsv.GetVisitor[Array[Byte], U]): Ask[U] =
        if !content.exists then Err.or(s"File not found: $content")
        else Resource.Nice(content.openRead(0))(_.close): input =>
          val sz = content.raw.size
          var n = sz.clamp(256, 4194304).toInt
          if sz > n && sz - n < 65536 then n = 2621440
          decode(Send.IterateInputStream(input, n, n)).?
  }

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
    def unquoted(data: T, start: Int, end: Int): Ask[Unit]
    def quoted(data: T, start: Int, end: Int): Unit
    def endquote(): Ask[Unit]
    def newline(line: UInt): Ask[Unit]
    def complete(line: UInt): Ask[U]
    def error(err: Err): Err
    def breakWithError[L >: Alt[Err]](err: Err)(using boundary.Label[L]): Nothing = boundary.break(Alt(error(err)))
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

      protected def addToRow(cell: String): Ask[Unit] =
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

      def endquote(): Ask[Unit] =
        val result = q match
          case null => Err.or(s"Supposed end-quote with no starting quote?")
          case s: String => addToRow(s)
          case sb: java.lang.StringBuilder => addToRow(sb.toString)
        q = null
        result

      def newline(line: UInt): Ask[Unit] = Or.Ret:
        if q ne null then Err ?# "New row in middle of quote"
        if (row ne null) && colIdx > 0 then
          if table eq null then table = new Array[Array[String]](4)
          else if rowIdx >= table.length then
            val m = (rowIdx *# 2) min (Int.MaxValue - 7)
            if m == table.length then Err ?# s"Too many lines for table: ${m+1}"
            table = table.copyToSize(m)
          table(rowIdx) = row.shrinkCopy(colIdx)
          var i = rowIdx - 1
          while i >= 0 && (table(i) eq null) do
            table(i) = emptyRow
            i -= 1
          if strictlyRectangular && rowIdx > 0 && table(rowIdx-1).length != colIdx then
            Err ?# s"Row $rowIdx has ${table(rowIdx-1).length} columns but row ${rowIdx+1} has $colIdx"
        else
          if (table ne null) && rowIdx < table.length then table(rowIdx) = emptyRow
        row = null
        colIdx = 0
        rowIdx += 1

      def complete(line: UInt): Ask[Array[Array[String]]] = Or.Ret:
        if q ne null then Err ?# "End of table in middle of quote"
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
      def unquoted(data: String, start: Int, end: Int): Ask[Unit] =
        if q ne null then Err.or(s"New token in middle of quote")
        else addToRow(data.substring(start, end))

      def quoted(data: String, start: Int, end: Int): Unit = q match
        case null => q = data.substring(start, end)
        case s: String =>
          val sb = s.maker()
          sb.add(data, start, end)
          q = sb.unwrap
        case sb: java.lang.StringBuilder =>
          MkStr.wrap(sb).add(data, start, end)
    }


    final class TableFromBytes(strictRect: Boolean = false)
    extends ToStringTable[Array[Byte]](strictRect) {
      def unquoted(data: Array[Byte], start: Int, end: Int): Ask[Unit] =
        if q ne null then Err.or("New token in middle of quote")
        else addToRow(new String(data, start, end-start))

      def quoted(data: Array[Byte], start: Int, end: Int): Unit =
        val x = if start == 0 && end == 1 && data(0) == '\n' then "\n" else new String(data, start, end-start)
        q match
          case null => q = x
          case s: String =>
            val sb = s.maker()
            sb += x
            q = sb.unwrap
          case sb: java.lang.StringBuilder =>
            MkStr.wrap(sb) += x
    }

    /** Builds a `Dataset`, giving up with `notNumeric` set as soon as the input cannot be one, so that the caller
      * can read it as strings instead.  See `Xsv.dataset` for the rules. */
    abstract class ToDataset[T <: Array[Byte] | String](alwaysCols: Boolean, alwaysRows: Boolean)
    extends Visitor[T, Dataset] {
      /** True once the input is known not to be a numeric dataset. */
      var notNumeric = false
      private var header: Array[String] = null   // the first row's cells, as text until it is known to be a header
      private var row0 = true
      private var undecided: Array[String] = null  // a first row whose first cell alone is text: header or labeled data?
      private var labeled = 0                      // 1 when the first column is labels, -1 when not, 0 until known
      private var labels: Array[String] = null
      private var table: Array[Array[Double]] = null
      private var n = 0                            // rows in table
      private var row: Array[Double] = null
      private var width = -1                       // numbers per row, once a data row has ended
      private var col = 0                          // cells so far on this line
      private var k = 0                            // numbers so far on this line
      private var pendingEmpty = false             // the line so far is one empty unquoted cell, which may be a blank line
      protected var inQuote = false
      protected val sb = new java.lang.StringBuilder

      protected def number(data: T, i0: Int, iN: Int): Double
      protected def text(data: T, i0: Int, iN: Int): String

      private val giveUp: Ask[Unit] = Err.or("not a numeric dataset")

      def clear(): this.type =
        notNumeric = false
        header = null
        row0 = true
        undecided = null
        labeled = if alwaysRows then 1 else 0
        labels = null
        table = null
        n = 0
        row = null
        width = -1
        col = 0
        k = 0
        pendingEmpty = false
        inQuote = false
        sb.setLength(0)
        this

      private def fail(): Ask[Unit] =
        notNumeric = true
        giveUp

      private def addNumber(d: Double): Ask[Unit] =
        if EiselLemire.failed(d) then fail()
        else
          if row eq null then row = new Array[Double](if width >= 0 then width else 8)
          else if k >= row.length then
            if width >= 0 then return fail()
            row = row.copyToSize(row.length *# 2)
          row(k) = d
          k += 1
          Is.unit

      private def addLabel(s: String): Unit =
        if labels eq null then labels = new Array[String](if table eq null then 8 else table.length)
        else if n >= labels.length then labels = labels.copyToSize(labels.length *# 2)
        labels(n) = s

      private def addHeader(s: String): Unit =
        if header eq null then header = new Array[String](8)
        else if col >= header.length then header = header.copyToSize(header.length *# 2)
        header(col) = s

      // The first cell of a data row is a label, a number, or (on the first data row) decides which
      private def firstCell(d: Double, s: => String): Ask[Unit] =
        if labeled == 0 then labeled = if EiselLemire.failed(d) then 1 else -1
        if labeled > 0 then { addLabel(s); Is.unit }
        else addNumber(d)

      // The second row's first cell settles an undecided first row: text means the first column is labels and the
      // first row is data, a number means the first row's text can only be a header
      private def settle(text: Boolean): Ask[Unit] =
        val h = undecided
        undecided = null
        if !text then
          header = h
          Is.unit
        else replay(h)

      private def replay(h: Array[String]): Ask[Unit] = Or.Ret:
        var j = 0
        while j < h.length do
          cell(h(j)).?
          j += 1
        endDataRow().?

      private def cell(data: T, i0: Int, iN: Int): Ask[Unit] = Or.Ret:
        if row0 then addHeader(text(data, i0, iN))
        else if col == 0 then
          if undecided ne null then settle(EiselLemire.failed(number(data, i0, iN))).?
          if labeled > 0 then addLabel(text(data, i0, iN))
          else firstCell(number(data, i0, iN), text(data, i0, iN)).?
        else addNumber(number(data, i0, iN)).?
        col += 1

      private def cell(s: String): Ask[Unit] = Or.Ret:
        if row0 then addHeader(s)
        else
          val d = cellValue(s.charAt(_), EiselLemire.parseDouble(s, _, _), 0, s.length)
          if col == 0 then
            if undecided ne null then settle(EiselLemire.failed(d)).?
            firstCell(d, s).?
          else addNumber(d).?
        col += 1

      private def flushEmpty(): Ask[Unit] =
        if !pendingEmpty then Is.unit
        else
          pendingEmpty = false
          cell("")

      def unquoted(data: T, start: Int, end: Int): Ask[Unit] =
        if col == 0 && start == end && !pendingEmpty then
          pendingEmpty = true
          Is.unit
        else flushEmpty() && cell(data, start, end)

      def endquote(): Ask[Unit] =
        inQuote = false
        val s = sb.toString
        sb.setLength(0)
        flushEmpty() && cell(s)

      private def endHeaderRow(): Ask[Unit] =
        val h = header.shrinkCopy(col)
        header = null
        row0 = false
        col = 0
        def isText(s: String) = EiselLemire.failed(cellValue(s.charAt(_), EiselLemire.parseDouble(s, _, _), 0, s.length))
        var j = 1
        while j < h.length && !isText(h(j)) do j += 1
        if alwaysCols || j < h.length then
          header = h
          Is.unit
        else if isText(h(0)) then
          undecided = h
          Is.unit
        else replay(h)

      private def endDataRow(): Ask[Unit] =
        if width < 0 then width = k
        if k != width then fail()
        else
          if table eq null then table = new Array[Array[Double]](8)
          else if n >= table.length then table = table.copyToSize(table.length *# 2)
          table(n) = if (row ne null) && row.length == k then row else if row eq null then new Array[Double](0) else row.shrinkCopy(k)
          n += 1
          row = null
          col = 0
          k = 0
          Is.unit

      def newline(line: UInt): Ask[Unit] =
        if pendingEmpty && col == 0 then
          pendingEmpty = false
          Is.unit
        else if col == 0 then Is.unit
        else if row0 then endHeaderRow()
        else endDataRow()

      def complete(line: UInt): Ask[Dataset] = Or.Ret:
        if inQuote then Err ?# "End of table in middle of quote"
        if col > 0 || pendingEmpty then newline(line).?
        if undecided ne null then settle(false).?
        val lab = labeled > 0
        val (corner, cols) =
          if header eq null then (None, None)
          else if !lab then (None, Some(header))
          else if header.length == 0 then (None, None)
          else (Some(header(0)), Some(header.copyOfRange(1, header.length)))
        if width >= 0 && cols.exists(_.length != width) then fail().?
        val t = if table eq null then new Array[Array[Double]](0) else table.shrinkCopy(n)
        val rows = if !lab then None else Some(if labels eq null then new Array[String](0) else labels.shrinkCopy(n))
        (corner = corner, cols = cols, rows = rows, table = t)

      def error(err: Err): Err = err
    }

    final class DatasetFromBytes(alwaysCols: Boolean = false, alwaysRows: Boolean = false)
    extends ToDataset[Array[Byte]](alwaysCols, alwaysRows) {
      protected def number(data: Array[Byte], i0: Int, iN: Int): Double =
        cellValue(data(_), EiselLemire.parseDouble(data, _, _), i0, iN)
      protected def text(data: Array[Byte], i0: Int, iN: Int): String = new String(data, i0, iN - i0, UTF_8)
      def quoted(data: Array[Byte], start: Int, end: Int): Unit =
        inQuote = true
        sb.append(new String(data, start, end - start, UTF_8)) __ Unit
    }

    final class DatasetFromString(alwaysCols: Boolean = false, alwaysRows: Boolean = false)
    extends ToDataset[String](alwaysCols, alwaysRows) {
      protected def number(data: String, i0: Int, iN: Int): Double =
        cellValue(data.charAt(_), EiselLemire.parseDouble(data, _, _), i0, iN)
      protected def text(data: String, i0: Int, iN: Int): String = data.substring(i0, iN)
      def quoted(data: String, start: Int, end: Int): Unit =
        inQuote = true
        sb.append(data, start, end) __ Unit
    }

    /** Builds `ColumnData`; see `Xsv.columnar` for the rules.  Every cell's place is kept, and each column parses its
      * cells as numbers and as Booleans for as long as either could still be its type, so that the column is built at
      * the end in whichever type it turns out to be without a second pass over the input.  The places refer to the
      * input, which must therefore arrive in one piece. */
    private[eio] abstract class ToColumns[T <: Array[Byte] | String](headers: Int, isBool: String => Int, forceStrings: Set[Int | String])
    extends Visitor[T, ColumnData] {
      protected val fastBool = isBool eq Xsv.isBool

      protected final class Col {
        var where = new Array[Int](16)        // start and end of each cell; a quoted cell is (-1 - index into quoted, 0)
        var quoted: Array[String] = null
        var nq = 0
        var nums = new Array[Double](8)       // null once a cell after the first is not a number
        var bools = new Array[Byte](8)        // null once a cell after the first is not a Boolean
        var anyBool = false                   // a cell after the first is a Boolean, not empty
        var num0 = true                       // the first cell is a number
        var bool0 = 0                         // the first cell is a Boolean (1), empty (0), or neither (-1)
        var n = 0
      }

      private var src: T = null.asInstanceOf[T]
      private var cols: Array[Col] = null
      private var width = -1
      private var col = 0
      private var rows = 0
      private var pendingEmpty = false
      protected var inQuote = false
      protected val sb = new java.lang.StringBuilder

      protected def number(data: T, i0: Int, iN: Int): Double
      protected def bool(data: T, i0: Int, iN: Int): Int     // 1, -1, or 0 for a cell that is not one
      protected def text(data: T, i0: Int, iN: Int): String

      def clear(): this.type =
        src = null.asInstanceOf[T]
        cols = null
        width = -1
        col = 0
        rows = 0
        pendingEmpty = false
        inQuote = false
        sb.setLength(0)
        this

      private def target(): Ask[Col] =
        if cols eq null then cols = new Array[Col](8)
        if col >= cols.length then
          if width >= 0 then return Err.or(s"Row ${rows + 1} has more than the $width cells of row 1")
          cols = cols.copyToSize(cols.length *# 2)
        if cols(col) eq null then cols(col) = new Col
        Is(cols(col))

      // The kinds a cell can be, recorded against its column; `place` has already been stored
      private def kinds(c: Col, d: => Double, b: => Int, empty: Boolean): Unit =
        val i = c.n
        if (c.nums ne null) || i == 0 then
          val x = d
          if i == 0 then c.num0 = !EiselLemire.failed(x)
          else if EiselLemire.failed(x) then c.nums = null
          if c.nums ne null then
            if i >= c.nums.length then c.nums = c.nums.copyToSize(c.nums.length *# 2)
            c.nums(i) = x
        if (c.bools ne null) || i == 0 then
          val v = if empty then 0 else b
          val ok = empty || v != 0
          if i == 0 then c.bool0 = if empty then 0 else if ok then 1 else -1
          else if !ok then c.bools = null
          else if !empty then c.anyBool = true
          if c.bools ne null then
            if i >= c.bools.length then c.bools = c.bools.copyToSize(c.bools.length *# 2)
            c.bools(i) = (if ok then v else 0).toByte
        c.n += 1

      private def place(c: Col, a: Int, b: Int): Unit =
        val k = 2 * c.n
        if k + 1 >= c.where.length then c.where = c.where.copyToSize(c.where.length *# 2)
        c.where(k) = a
        c.where(k + 1) = b

      private def cell(data: T, i0: Int, iN: Int): Ask[Unit] = Or.Ret:
        val c = target().?
        src = data
        place(c, i0, iN)
        kinds(c, number(data, i0, iN), bool(data, i0, iN), i0 == iN)
        col += 1

      private def cell(s: String): Ask[Unit] = Or.Ret:
        val c = target().?
        if c.quoted eq null then c.quoted = new Array[String](4)
        else if c.nq >= c.quoted.length then c.quoted = c.quoted.copyToSize(c.quoted.length *# 2)
        c.quoted(c.nq) = s
        place(c, -1 - c.nq, 0)
        c.nq += 1
        val b = isBool(s)
        kinds(c, cellValue(s.charAt(_), EiselLemire.parseDouble(s, _, _), 0, s.length), if b == 1 then 1 else if b == 0 then -1 else 0, s.isEmpty)
        col += 1

      def unquoted(data: T, start: Int, end: Int): Ask[Unit] =
        if col == 0 && start == end && !pendingEmpty then
          pendingEmpty = true
          Is.unit
        else if pendingEmpty then
          pendingEmpty = false
          cell(data, start, start) && cell(data, start, end)
        else cell(data, start, end)

      def endquote(): Ask[Unit] =
        inQuote = false
        val s = sb.toString
        sb.setLength(0)
        if pendingEmpty then
          pendingEmpty = false
          cell("") && cell(s)
        else cell(s)

      def newline(line: UInt): Ask[Unit] =
        if col == 0 then
          pendingEmpty = false
          Is.unit
        else
          if width < 0 then width = col
          if col != width then Err.or(s"Row ${rows + 1} has $col cells but row 1 has $width")
          else
            col = 0
            rows += 1
            Is.unit

      private def textOf(c: Col, i: Int): String =
        val a = c.where(2 * i)
        if a < 0 then c.quoted(-1 - a) else text(src, a, c.where(2 * i + 1))

      def complete(line: UInt): Ask[ColumnData] = Or.Ret:
        if inQuote then Err ?# "End of table in middle of quote"
        if col > 0 then newline(UInt(0)).?
        val w = width max 0
        // Type from the rows after the first: 0 Boolean, 1 number, 2 text
        def kind1(c: Col): Int = if (c.bools ne null) && c.anyBool then 0 else if c.nums ne null then 1 else 2
        val head =
          if headers != 0 then headers > 0
          else if rows == 0 then false
          else if rows == 1 then true
          else
            var all = true
            var misfit = false
            var j = 0
            while j < w do
              val c = cols(j)
              kind1(c) match
                case 0 => all = false; if c.bool0 < 0 then misfit = true
                case 1 => all = false; if !c.num0 then misfit = true
                case _ =>
              j += 1
            misfit || all
        val names = if !head then None else Some(Array.tabulate(w)(j => textOf(cols(j), 0)))
        val i0 = if head then 1 else 0
        val out = new Array[Array[OptBoolean] | Array[Double] | Array[String]](w)
        var j = 0
        while j < w do
          val c = cols(j)
          val forced = forceStrings.contains(j) || names.exists(ns => forceStrings.contains(ns(j)))
          val k =
            if forced then 2
            else if head then kind1(c)
            else if (c.bools ne null) && c.bool0 >= 0 && (c.anyBool || c.bool0 > 0) then 0
            else if (c.nums ne null) && c.num0 then 1
            else 2
          out(j) = k match
            case 0 => Array.tabulate(c.n - i0)(i => OptBoolean.fromOrdinal(c.bools(i + i0) + 1))
            case 1 => c.nums.copyOfRange(i0, c.n)
            case _ => Array.tabulate(c.n - i0)(i => textOf(c, i + i0))
          j += 1
        new ColumnData(names, out)

      def error(err: Err): Err = err
    }

    private[eio] final class ColumnsFromBytes(headers: Int, isBool: String => Int, forceStrings: Set[Int | String])
    extends ToColumns[Array[Byte]](headers, isBool, forceStrings) {
      protected def number(data: Array[Byte], i0: Int, iN: Int): Double =
        cellValue(data(_), EiselLemire.parseDouble(data, _, _), i0, iN)
      protected def bool(data: Array[Byte], i0: Int, iN: Int): Int =
        if fastBool then
          val m = iN - i0
          if m == 4 && (data(i0) | 0x20) == 't' && (data(i0+1) | 0x20) == 'r' && (data(i0+2) | 0x20) == 'u' && (data(i0+3) | 0x20) == 'e' then 1
          else if m == 5 && (data(i0) | 0x20) == 'f' && (data(i0+1) | 0x20) == 'a' && (data(i0+2) | 0x20) == 'l' &&
                  (data(i0+3) | 0x20) == 's' && (data(i0+4) | 0x20) == 'e' then -1
          else 0
        else
          val b = isBool(text(data, i0, iN))
          if b == 1 then 1 else if b == 0 then -1 else 0
      protected def text(data: Array[Byte], i0: Int, iN: Int): String = new String(data, i0, iN - i0, UTF_8)
      def quoted(data: Array[Byte], start: Int, end: Int): Unit =
        inQuote = true
        sb.append(new String(data, start, end - start, UTF_8)) __ Unit
    }

    private[eio] final class ColumnsFromString(headers: Int, isBool: String => Int, forceStrings: Set[Int | String])
    extends ToColumns[String](headers, isBool, forceStrings) {
      protected def number(data: String, i0: Int, iN: Int): Double =
        cellValue(data.charAt(_), EiselLemire.parseDouble(data, _, _), i0, iN)
      protected def bool(data: String, i0: Int, iN: Int): Int =
        val b = isBool(data.substring(i0, iN))
        if b == 1 then 1 else if b == 0 then -1 else 0
      protected def text(data: String, i0: Int, iN: Int): String = data.substring(i0, iN)
      def quoted(data: String, start: Int, end: Int): Unit =
        inQuote = true
        sb.append(data, start, end) __ Unit
    }

    def onString(strictRect: Boolean = false) = new TableFromString(strictRect)

    def onBytes( strictRect: Boolean = false) = new TableFromBytes( strictRect)
  }

  def encodeCell(cell: String, separator: Char)(sb: MkStr): Unit =
    var n = -1
    escape:
      cell.visit(){ (c, i) =>
        escape.unless_?(c != '\r' && c != '\n' && c != '"' && c != separator)
        n = i
      }
    if n == cell.length-1 then sb += cell
    else
      sb += '"'
      var m = 0
      escape:
        cell.visit(n+1 to End){ (c, i) =>
          escape.unless_?(c != '"')
          n = i
        }
      while n != cell.length-1 do
        sb.add(cell, m, n+1)
        sb += "\"\""
        m = n+2
        if m < cell.length then
          n = m
          escape:
            cell.visit(n to End){ (c, i) =>
              escape.unless_?(c != '"')
              n = i
            }
        else n = cell.length -1
      if m <= n then sb.add(cell, m, n+1)
      sb += '"'

  def encodeRow(cells: Array[String], separator: Char)(sb: MkStr): Unit =
    cells.visit(){ (cell, i) =>
      encodeCell(cell, separator)(sb)
      if i+1 == cells.length then sb += "\r\n" else sb += separator
    }

  def encodeTable(table: Array[Array[String]], separator: Char): Iterator[Array[Byte]] =
    EncodeIterator(table, separator)

  def encodeDataset(data: Dataset, separator: Char): Iterator[Array[Byte]] =
    new DatasetEncodeIterator(data, separator)

  def encodeColumns(data: ColumnData, separator: Char): Iterator[Array[Byte]] =
    new ColumnEncodeIterator(data, separator)

  /** Writes `ColumnData` row by row in chunks of about 64 KiB, each number and Boolean straight into the chunk. */
  class ColumnEncodeIterator(data: ColumnData, separator: Char) extends Iterator[Array[Byte]] {
    private val cols = data.columns
    private val kind = cols.map{ case _: Array[Double] => 1; case _: Array[String] => 2; case _ => 0 }
    private val n = data.rows
    private var head = data.headers.isDefined
    private var i = 0
    private var buf = new Array[Byte](65536)
    private var at = 0

    private def room(m: Int): Unit =
      if buf.length - at < m then buf = buf.copyToSize(((at.toLong + m) max (2L * buf.length) min (Int.MaxValue - 7L)).toInt)

    private def put(s: String): Unit =
      val sb = MkStr.empty()
      encodeCell(s, separator)(sb)
      val b = sb.str().bytes
      room(b.length + 1)
      b.inject(buf, at)(0, b.length) __ Unit
      at += b.length

    private def ascii(s: String): Unit =
      room(s.length)
      var k = 0
      while k < s.length do
        buf(at) = s.charAt(k).toByte
        at += 1
        k += 1

    private def sep(): Unit =
      room(1)
      buf(at) = separator.toByte
      at += 1

    def hasNext = head || i < n

    def next(): Array[Byte] =
      if !hasNext then Iterator.empty.next()
      at = 0
      if head then
        head = false
        data.headers.get.visit(){ (c, j) =>
          if j > 0 then sep()
          put(c)
        }
        ascii("\r\n")
      while i < n && at < 65536 - 256 do
        var j = 0
        while j < cols.length do
          if j > 0 then sep()
          kind(j) match
            case 1 =>
              val xs = cols(j).asInstanceOf[Array[Double]]
              if i < xs.length then
                val d = xs(i)
                if d == d then
                  if d == Double.PositiveInfinity then ascii("inf")
                  else if d == Double.NegativeInfinity then ascii("-inf")
                  else
                    room(24)
                    at = Ryu.append(buf, at, d)
                else if !d.isNA then ascii("NaN")
            case 2 =>
              val xs = cols(j).asInstanceOf[Array[String]]
              if i < xs.length then put(xs(i))
            case _ =>
              val xs = cols(j).asInstanceOf[Array[OptBoolean]]
              if i < xs.length then
                if xs(i).isTrue then ascii("True") else if xs(i).isFalse then ascii("False")
          j += 1
        ascii("\r\n")
        i += 1
      val ans = buf.shrinkCopy(at)
      if !hasNext then buf = null
      ans
  }

  /** Writes a `Dataset` in chunks of about 64 KiB, each number straight into the chunk. */
  class DatasetEncodeIterator(data: Dataset, separator: Char) extends Iterator[Array[Byte]] {
    private val table = if data.table eq null then new Array[Array[Double]](0) else data.table
    private val labels = data.rows.getOrElse(null)
    private var head = data.cols.isDefined
    private var i = 0
    private var buf = new Array[Byte](65536)
    private var at = 0

    private def room(m: Int): Unit =
      if buf.length - at < m then buf = buf.copyToSize(((at.toLong + m) max (2L * buf.length) min (Int.MaxValue - 7L)).toInt)

    private def put(s: String): Unit =
      val sb = MkStr.empty()
      encodeCell(s, separator)(sb)
      val b = sb.str().bytes
      room(b.length + 1)
      b.inject(buf, at)(0, b.length) __ Unit
      at += b.length

    private def eol(): Unit =
      room(2)
      buf(at) = '\r'
      buf(at+1) = '\n'
      at += 2

    private def sep(): Unit =
      room(1)
      buf(at) = separator.toByte
      at += 1

    def hasNext = head || i < table.length

    def next(): Array[Byte] =
      if !hasNext then Iterator.empty.next()
      at = 0
      if head then
        head = false
        if labels ne null then
          put(data.corner.getOrElse(""))
          if data.cols.get.length > 0 then sep()
        data.cols.get.visit(){ (c, j) =>
          if j > 0 then sep()
          put(c)
        }
        eol()
      while i < table.length && at < 65536 - 256 do
        val r = table(i)
        if labels ne null then
          put(if i < labels.length then labels(i) else "")
          if r.length > 0 then sep()
        room(26 * r.length + 2)
        var j = 0
        while j < r.length do
          if j > 0 then
            buf(at) = separator.toByte
            at += 1
          val d = r(j)
          if d == d then
            if d == Double.PositiveInfinity then { buf(at) = 'i'; buf(at+1) = 'n'; buf(at+2) = 'f'; at += 3 }
            else if d == Double.NegativeInfinity then { buf(at) = '-'; buf(at+1) = 'i'; buf(at+2) = 'n'; buf(at+3) = 'f'; at += 4 }
            else at = Ryu.append(buf, at, d)
          else if !d.isNA then { buf(at) = 'N'; buf(at+1) = 'a'; buf(at+2) = 'N'; at += 3 }
          j += 1
        eol()
        i += 1
      val ans = buf.shrinkCopy(at)
      if !hasNext then buf = null
      ans
  }

  class EncodeIterator(private var table: Array[Array[String]], val separator: Char) extends Iterator[Array[Byte]] {
    private var row = if (table eq null) || table.length == 0 then -1 else 0
    def hasNext = row >= 0
    def next: Array[Byte] =
      if row < 0 then Iterator.empty.next()
      val sb = MkStr.empty()
      encodeRow(table(row), separator)(sb)
      val b = sb.str().bytes
      row += 1
      if row >= table.length then
        row = -1
        table = null
      b
   }
}

object Csv {
  def decode(content: Array[Byte]       ): Ask[Array[Array[String]]] = Xsv.comma.decode(content)
  @targetName("decodeByteArrays")
  def decode(content: IOnce[Array[Byte]]): Ask[Array[Array[String]]] = Xsv.comma.decode(content)
  def decode(content: String            ): Ask[Array[Array[String]]] = Xsv.comma.decode(content)
  @targetName("decodeStrings")
  def decode(content: IOnce[String]     ): Ask[Array[Array[String]]] = Xsv.comma.decode(content)
  def decode(content: InputStream       ): Ask[Array[Array[String]]] = Xsv.comma.decode(content)
  def read(path: Path                   ): Ask[Array[Array[String]]] = Xsv.comma.read(path)

  object bomless {
    def decode(content: Array[Byte]       ): Ask[Array[Array[String]]] = Xsv.comma.bomless.decode(content)
    def decode(content: IOnce[Array[Byte]]): Ask[Array[Array[String]]] = Xsv.comma.bomless.decode(content)
    def decode(content: InputStream       ): Ask[Array[Array[String]]] = Xsv.comma.bomless.decode(content)
    def read(path: Path                   ): Ask[Array[Array[String]]] = Xsv.comma.bomless.read(path)
  }

  /** A table of numbers, or the strings if it is not one; see `Xsv.dataset`. */
  def dataset(
    content: Array[Byte] | String | InputStream | Path,
    colHeaders: "detect" | "always" = "detect",
    rowLabels: "detect" | "always" = "detect"
  ): Ask[Either[Array[Array[String]], Xsv.Dataset]] =
    Xsv.comma.dataset(content, colHeaders, rowLabels)

  /** A table as columns of Booleans, numbers, or text; see `Xsv.columnar`. */
  def columnar(
    content: Array[Byte] | String | InputStream | Path,
    colHeaders: "detect" | "always" | "never" = "detect",
    isBool: String => Int = Xsv.isBool,
    forceStrings: Set[Int | String] = Set.empty
  ): Ask[Xsv.ColumnData] =
    Xsv.comma.columnar(content, colHeaders, isBool, forceStrings)

  def encode(table: Array[Array[String]]): Iterator[Array[Byte]] =
    Xsv.comma.encode(table)
  def write(table: Array[Array[String]])(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Unit Or Err =
    Xsv.comma.write(table)(p)

  def encode(data: Xsv.Dataset): Iterator[Array[Byte]] =
    Xsv.comma.encode(data)
  def write(data: Xsv.Dataset)(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Xsv.comma.write(data)(p)

  def encode(data: Xsv.ColumnData): Iterator[Array[Byte]] =
    Xsv.comma.encode(data)
  def write(data: Xsv.ColumnData)(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Xsv.comma.write(data)(p)
}

object Tsv {
  def decode(content: Array[Byte]       ): Ask[Array[Array[String]]] = Xsv.tab.decode(content)
  @targetName("decodeByteArrays")
  def decode(content: IOnce[Array[Byte]]): Ask[Array[Array[String]]] = Xsv.tab.decode(content)
  def decode(content: String            ): Ask[Array[Array[String]]] = Xsv.tab.decode(content)
  @targetName("decodeStrings")
  def decode(content: IOnce[String]     ): Ask[Array[Array[String]]] = Xsv.tab.decode(content)
  def decode(content: InputStream       ): Ask[Array[Array[String]]] = Xsv.tab.decode(content)
  def read(path: Path                   ): Ask[Array[Array[String]]] = Xsv.tab.read(path)

  object bomless {
    def decode(content: Array[Byte]       ): Ask[Array[Array[String]]] = Xsv.tab.bomless.decode(content)
    def decode(content: IOnce[Array[Byte]]): Ask[Array[Array[String]]] = Xsv.tab.bomless.decode(content)
    def decode(content: InputStream       ): Ask[Array[Array[String]]] = Xsv.tab.bomless.decode(content)
    def read(path: Path                   ): Ask[Array[Array[String]]] = Xsv.tab.bomless.read(path)
  }


  /** A table of numbers, or the strings if it is not one; see `Xsv.dataset`. */
  def dataset(
    content: Array[Byte] | String | InputStream | Path,
    colHeaders: "detect" | "always" = "detect",
    rowLabels: "detect" | "always" = "detect"
  ): Ask[Either[Array[Array[String]], Xsv.Dataset]] =
    Xsv.tab.dataset(content, colHeaders, rowLabels)

  /** A table as columns of Booleans, numbers, or text; see `Xsv.columnar`. */
  def columnar(
    content: Array[Byte] | String | InputStream | Path,
    colHeaders: "detect" | "always" | "never" = "detect",
    isBool: String => Int = Xsv.isBool,
    forceStrings: Set[Int | String] = Set.empty
  ): Ask[Xsv.ColumnData] =
    Xsv.tab.columnar(content, colHeaders, isBool, forceStrings)

  def encode(table: Array[Array[String]]): Iterator[Array[Byte]] =
    Xsv.tab.encode(table)
  def write(table: Array[Array[String]])(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Xsv.tab.write(table)(p)

  def encode(data: Xsv.Dataset): Iterator[Array[Byte]] =
    Xsv.tab.encode(data)
  def write(data: Xsv.Dataset)(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Xsv.tab.write(data)(p)

  def encode(data: Xsv.ColumnData): Iterator[Array[Byte]] =
    Xsv.tab.encode(data)
  def write(data: Xsv.ColumnData)(p: Path)(using tr: Send[Iterator[Array[Byte]], OutputStream]): Ask[Unit] =
    Xsv.tab.write(data)(p)
}
