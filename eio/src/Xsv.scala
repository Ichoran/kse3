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

import kse.flow.{given, _}
import kse.maths.{given, _}
import kse.maths.packed.{given, _}


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

  // Ignores spaces after a quote in input.
  // If input runs out, index will be iN and state will stay Sp
  // Otherwise state will transition to Tn, Tr, or Tc, or an error will be emitted.
  // If the transition is to Tn or Tr, visitor will receive a newline call.
  // Assumes 0 <= i0 <= iN <= data.length, and Sp state on entry.
  private def skipSpace(data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], _]): Unit Or Err =
      var i = i0
      while i < iN do
        val c = data(i)
        c match
          case x if x == separator =>
            pos += i - i0
            index = i + 1
            state = Tc
            return Is.unit
          case ' '  | '\t' =>
            i += 1
          case '\r' | '\n' =>
            line = line +# 1.u
            pos = 0
            index = i + 1
            state = if c == '\n' then Tn else Tr
            return visitor.newline(line).mapAlt(summon[AutoMap[Err, Err]])
          case _ =>
            pos += i - i0
            index = i
            return visitor.error(Err(sayWhere("Non-space cell content after quote"))).asAlt
      pos += i - i0
      index = i
      Is.unit

  // Loads quoted input into a visitor.
  // State will be Qr, Qq, or Qt if we are not known to be done;
  //   Tn, Tr, Tc, or Sp if we are (in which case endquote will have been called)
  // If incoming state is Q1, quoted will always be called.  Otherwise it will be called on any processed input.
  // Assumes 0 <= i0 < iN <= data.length, and a Q state on entry.
  private def visitQuoteImpl[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U]): Unit Or Err =
    Or.Ret:
      var i = i0
      while i < iN do
        // First we mark where we've gotten to successfully, and see if there's any state to handle.
        index = i
        state match
          case Qr =>
            if data(i) == '\n' then i += 1
            else visitor.quoted(Xsv.lfByte, 0, 1)   // Found \r alone but we like \n so we emit one
            state = Qt
          case Qq =>
            // Found what could be an ending quote, but is it really?
            val c = data(i)
            if c == '"' then
              // No, it was an embedded quote.  Pick up the extra quote.
              i += 1
              state = Qt
            else if c == '\n' || c == '\r' then
              // The quote is over and we've reached the end of a line
              line = line +# 1.u
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
          data(i) match
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
              line = line +# 1.u
              pos = 0
              i += 1
              index = i
              state = Qr
            case '\n' =>
              // Regular newline--intercept to update line count, but we can just keep scanning input.
              line = line +# 1.u
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

  private def trimmed[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U]): Unit Or Err =
    var jN = iN
    while jN > i0 && data(jN-1).fn(c => c == ' ' || c == '\t') do jN -= 1
    var j0 = i0
    while j0 < jN && data(j0).fn(c => c == ' ' || c == '\t') do j0 += 1
    visitor.unquoted(data, j0, jN)

  private def tokLine[U](data: Array[Byte], i: Int, visitor: Xsv.Visitor[Array[Byte], U]): Unit Or Err =
    Or.Ret:
      (if permissiveWhitespace then trimmed(data, index, i, visitor) else visitor.unquoted(data, index, i)).?*
      line = line +# 1.u
      pos = 0
      visitor.newline(line).?*
      index = i+1
      ()

  // Loads input into a visitor.
  // Index is advanced to the last valid data--this will be iN unless state ends as Tk with e == EoI (which means more data is needed)
  // Any state can be emitted if e == EoI, but if it's a Q state it means that endquote has not been called.
  // If e & EoL != 0, a virtual \n will be inserted at the end of the input.  Any Q state can be emitted but otherwise only Tn can be
  // If e & EoD != 0, a trailing cell is terminated and considered complete (Tn state will be set.)
  // Assumes 0 <= i0 <= iN <= data.length.  Empty input okay.  Any state okay.
  private def visitRangeImpl[U](data: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U], e: Int): Unit Or Err =
    Or.Ret:
      var i = i0
      while i < iN do
        state match
          case s if (s & (Q1 | Qr | Qq | Qt)) != 0 =>
            visitQuoteImpl(data, i, iN, visitor).?
            i = index
          case Sp =>
            skipSpace(data, i, iN, visitor).?
            i = index
          case _ =>
            boundary:  // Exit on quote
              while i < iN do
                data(i) match
                  case '"' =>
                    if i > index then
                      if !permissiveWhitespace || { while index < i && data(index).fn(c => c == ' ' || c == '\t') do { index += 1; pos += 1 }; index < i } then
                        val bad =
                          if i - index <= 10 then (new String(data, index, i-index))
                          else "..." + (new String(data, i-7, 7))
                        Err.or(s"Extra content before quote: $bad").?*
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
                      tokLine(data, i, visitor).?
                      i = index
                      state = Tn
                  case '\r' =>
                    tokLine(data, i, visitor).?
                    i = index
                    state = Tr
                  case c if c == separator =>
                    (if permissiveWhitespace then trimmed(data, index, i, visitor) else visitor.unquoted(data, index, i)).?*
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
          else if state != Qr then visitor.quoted(Xsv.lfByte, 0, 1)
          line = line +# 1.u
          pos = 0
        else tokLine(data, i, visitor).?
      if (e & EoF) != 0 then
        // Make sure we consumed all the data and are not in a quote
        if (state & (Q1 | Qq | Qt | Qr)) != 0 then
          if state == Qq then visitor.endquote().?*
          else Err.or(s"Input ended inside of quote").?*
        else if i > index || state == Tc then
          (if permissiveWhitespace then trimmed(data, index, i, visitor) else visitor.unquoted(data, index, i)).?*
      ()


  
/*
  // Walks forward trying to find a matching quote, returning the index of the final character or iN if nothing is found
  // line and pos are updated to one past the last character in the quote; index is not altered
  // If a quote is found:   context is set to '"'
  // If a CR is found:      context is set to '\r'
  // If the input runs out: context is set to 1
  // No error checking.  Assumes 0 <= i0 <= iN <= data.length.
  private def findMatchingQuote(data: Array[Byte], i0: Int, iN: Int): Int =
    var i = i0
    var c0 = i0
    while i < iN do
      data(i) match
        case '"' =>
          pos += i - c0
          context = '"'
          return i
        case '\r' =>
          line = line +# 1.u
          pos = 0
          context = '\r'
          return i
        case '\n' =>
          line = line +# 1.u
          pos = 0
          i += 1
          c0 = i 
        case _ =>
          i += 1
    pos += i - c0
    context = 1
    i

  // See comment above findMatchingQuote(data: Array...) for explanation.
  // Keep this in perfect accord with the other variant!  The only difference should be apply vs charAt
  private def findMatchingQuote(data: String, i0: Int, iN: Int): Int =
    var i = i0
    var c0 = i0
    while i < iN do
      data.charAt(i) match
        case '"' =>
          pos += i - c0
          context = '"'
          return i
        case '\r' =>
          line = line +# 1.u
          pos = 0
          context = '\r'
          return i
        case '\n' =>
          line = line +# 1.u
          pos = 0
          i += 1
          c0 = i 
        case _ =>
          i += 1
    pos += i - c0
    context = 1
    i

  private def skipWhitespace(data: Array[Byte], i0: Int, iN: Int): Boolean =
    var i = i0
    var loop = true
    while loop && i < iN do
      data(i) match
        case ',' =>
          index = i + 1
          char += i + 1 - i0
          context = ','
          return true
        case '\n' =>
*/

/*
  // Walks forward inside a quote, loading more content into a visitor until the quote is exhausted.
  // line, char, and index will all be updated to past the last consumed character.
  // If the input runs out, context will be:
  //   1 if inside a quote at a normal position (just read more input if available)
  //   '\r' if immediately after a '\r' inside a quote (nee)
  //   '"' if immediately after a '"' (need to check for a second '"' for embedded quote mark)
  //   ' ' if we have consumed some trailing whitespace and need to check for more
  // If the content does not run out, context will be set appropriately given what follows.
  // If context is set to one of the run-out values on entry, the appropriate check will be performed.
  // If context is 0 on entry, visitor.quoted will be called at least once even on empty input.
  // visitor.endquote will be called if the content is consumed successfully.
  // No error checking on ranges.  Assume 0 <= i0 <= iN <= content.length.
  // Returns true if the read was known to be complete, false otherwise.
  private def visitQuoteImpl[U](content: Array[Byte], i0: Int, iN: Int, visitor: Xsv.Visitor[Array[Byte], U]): Boolean Or Err = 
    Or.FlatRet:
      var j0 = i0
      var always = context == 0
      while j0 < iN do
        val j = context match
          case '"' =>
            var c = j0
            if permissiveWhitespace
            context(j0) match
              case '"' =>
                pos += 1
                j0 + 1
              case '\n' =>
                line = line +# 1
                pos = 0
                index = j0 + 1
                context = 0
                Is.T.break
              case '\r' =>
                line = line +# 1
                pos = 0
                index = j0 + 1
                context = -2
                Is.T.break

            else
              index = j0
              return true
          case '\r' =>
            if content(j0) == '\n' then
              pos += 1
              j0 + 1
            else
              always = false
              visitor.quoted(Xsv.lfByte, 0, 1)
              j0
          case _ => j0
        val i = findMatchingQuote(content, j, iN)
        if always || j0 < i then
          always = false
          visitor.quoted(content, j0, i)
        j0 = if context == 1 then i else { pos += 1; i+1 }
      index = j0
      Is.F

  // See comment above visitQuoteImpl[U](content: Array[Byte]...) for explanation.
  // Keep this in perfect accord with the above!  The only difference should be apply vs charAt (and "\n" vs lfByte)
  private def visitQuoteImpl[U](content: String, i0: Int, iN: Int, visitor: Xsv.Visitor[String, U]): Boolean =
    var j0 = i0
    var always = context == 0
    while j0 < iN do
      val j = context match
        case '"' =>
          if content.charAt(j0) == '"' then
            pos += 1
            j0 + 1
          else
            index = j0
            return true
        case '\r' =>
          if content.charAt(j0) == '\n' then
            pos += 1
            j0 + 1
          else
            always = false
            visitor.quoted("\n", 0, 1)
            j0
        case _ => j0
      val i = findMatchingQuote(content, j, iN)
      if always || j0 < i then
        always = false
        visitor.quoted(content, j0, i)
      j0 = if context == 1 then i else { pos += 1; i+1 }
    index = j0
    false
*/
  // TODO--explain how this works!
  /*
  private def visitRangeImpl[U](content: Array[Byte], visitor: Xsv.Visitor[Array[Byte], U], terminal: Boolean)(start: Int, end: Int): Unit Or Err =
    Or.Ret:
      var i =
        if context = ' ' then
          dewhiteImpl(content, start, end).?
          index
        else start
      var i0 = i
      while i < end do
        content(i) match
          case '"' =>      
            if i > i0 && !permissiveWhitespace || { var j = i0; while j < i && content(j).fn(c => c == ' ' || c == '\t') do j += 1; j < i } then
              visitor.error(Err(s"Quote in the middle of cell content near line ${line.toLong+1}, position ${char + i - i0}")).asAlt.break
            char += i + 1 - i0
            val knownFinished = visitQuoteImpl(content, i+1, end, visitor)
            if terminal then
              if context != '"' then visitor.error(Err(sayWhere("Input ended in middle of quote at"))).asAlt.break
              visitor.endquote().?*
              context = 0
            else if knownFinished then
              visitor.endquote().?*
              context = 0
            else
              Is.unit.break
            i0 = index
            i = index
            if permissiveWhitespace then
              while i < end && content(i).fn(c => c == ' ' || c == '\t') do i += 1
              if i == end then context = ' '
            if i < end then content(i) match
              case '\n' | '\r' =>
                line = line +# 1.u
                char = 0
                visitor.newline(line).?*
                i0 = i + 1
              case b if b == separator =>
                i0 = i + 1
              case _ => visitor.error(Err(s"Extra content after quote near line ${line.toLong+1}, position ${char+1}")).asAlt.break
            char += i - i0
            i0 = i + 1
          case '\n' =>
            visitor.unquoted(content, i0, i).?*
            line = line +# 1.u
            char = 0
            visitor.newline(line).?*
            i0 = i + 1
            endtok = false
          case '\r' =>
            visitor.unquoted(content, i0, i).?*
            line = line +# 1.u
            char = 0
            visitor.newline(line).?*
            if i+1 < end && content(i+1) == '\n' then i += 1
            i0 = i + 1
            endtok = false
          case b if b == separator =>
            visitor.unquoted(content, i0, i).?*
            char += i - i0
            i0 = i + 1
            endtok = true
          case b =>
        i += 1
      if terminal && (i > i0 || endtok) then
        visitor.unquoted(content, i0, i).?*
        context = 0
        i0 = i
      else
        if i0 < end || endtok then context = -1
      index = i0
      ()

  // TODO--explain how this works!
  private def visitRangeImpl[U](content: String, visitor: Xsv.Visitor[String, U], terminal: Boolean)(start: Int, end: Int): Unit Or Err =
    Or.Ret:
      var i = start
      var i0 = i
      var endtok = false
      var endquot = false
      while i < end do
        content.charAt(i) match
          case '"' =>      
            if i > i0 && !permissiveWhitespace || { var j = i0; while j < i && content.charAt(j).fn(c => c == ' ' || c == '\t') do j += 1; j < i } then
              visitor.error(Err(s"Quote in the middle of cell content near line ${line.toLong+1}, position ${char + i - i0}")).asAlt.break
            char += i + 1 - i0
            val knownFinished = visitQuoteImpl(content, i+1, end, visitor)
            if terminal then
              if context != '"' then visitor.error(Err(sayWhere("Input ended in middle of quote at"))).asAlt.break
              visitor.endquote().?*
              context = 0
            else if knownFinished then
              visitor.endquote().?*
              context = 0
            else
              Is.unit.break
            i0 = index
            i = index
            if permissiveWhitespace then while i < end && content.charAt(i).fn(c => c == ' ' || c == '\t') do i += 1
            if i < end then content.charAt(i) match
              case '\n' | '\r' =>
                line = line +# 1.u
                char = 0
                visitor.newline(line).?*
                endtok = true
              case b if b == separator =>
                endtok = false
                i0 = i + 1
              case _ => visitor.error(Err(s"Extra content after quote near line ${line.toLong+1}, position ${char+1}")).asAlt.break
            char += i - i0
            i0 = i + 1
          case '\n' =>
            visitor.unquoted(content, i0, i).?*
            line = line +# 1.u
            char = 0
            visitor.newline(line).?*
            i0 = i + 1
            endtok = false
          case '\r' =>
            visitor.unquoted(content, i0, i).?*
            line = line +# 1.u
            char = 0
            visitor.newline(line).?*
            if i+1 < end && content.charAt(i+1) == '\n' then i += 1
            i0 = i + 1
            endtok = false
          case b if b == separator =>
            visitor.unquoted(content, i0, i).?*
            char += i - i0
            i0 = i + 1
            endtok = true
          case b =>
        i += 1
      if terminal && (i > i0 || endtok) then
        visitor.unquoted(content, i0, i).?*
        context = 0
        i0 = i
      else
        if i0 < end || endtok then context = -1
      index = i0
      ()

  def visitRange[U](content: Array[Byte], visitor: Xsv.Visitor[Array[Byte], U])(start: Int, end: Int): U Or Err = Or.FlatRet:
    if start < 0 then visitor.error(Err("Start index negative")).asAlt.break
    if end > content.length then visitor.error(Err("End index $end exceeds content length ${content.length}")).asAlt.break
    visitor.clear()
    if end <= start then visitor.complete(0.u).break
    line = 0.u
    char = 0
    context = 0
    visitRangeImpl(content, visitor, terminal = true)(start, end).?
    if index < end - start || context != 0 then
      visitor.error(Err(s"Input ended mid-quote near line ${line.toLong + 1}, position ${char+1}")).asAlt
    else visitor.complete(line)

  def visitRange[U](content: String, visitor: Xsv.Visitor[String, U])(start: Int, end: Int): U Or Err = Or.FlatRet:
    if start < 0 then visitor.error(Err("Start index negative")).asAlt.break
    if end > content.length then visitor.error(Err("End index $end exceeds content length ${content.length}")).asAlt.break
    visitor.clear()
    if end <= start then visitor.complete(0.u).break
    line = 0.u
    char = 0
    context = 0
    visitRangeImpl(content, visitor, terminal = true)(start, end).?
    if index < end - start || context != 0 then
      visitor.error(Err(s"Input ended mid-quote near line ${line.toLong + 1}, position ${char+1}")).asAlt
    else visitor.complete(line)
  */

  def visit[U](content: Array[Byte], visitor: Xsv.Visitor[Array[Byte], U]): U Or Err =
    visitor.clear()
    visitRangeImpl(content, 0, content.length, visitor, EoF) && visitor.complete(line)

  def visit[U](content: String,      visitor: Xsv.Visitor[Array[Byte], U]): U Or Err =
    visitor.clear()
    visitRangeImpl(content.bytes, 0, content.length, visitor, EoF) && visitor.complete(line)


  def decode[U](content: Array[Byte])(using gv: Xsv.GetVisitor[Array[Byte], U]): U Or Err = visit(content, gv.get(content.length))
  def decode[U](content: String     )(using gv: Xsv.GetVisitor[Array[Byte], U]): U Or Err = visit(content, gv.get(content.length))
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
    def clear(): Unit
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
  }
}

object Csv {
  def decode(content: Array[Byte]): Array[Array[String]] Or Err = Xsv.comma.decode(content)
  def decode(content: String     ): Array[Array[String]] Or Err = Xsv.comma.decode(content)
}

object Tsv {
  def decode(content: Array[Byte]): Array[Array[String]] Or Err = Xsv.tab.decode(content)
  def decode(content: String     ): Array[Array[String]] Or Err = Xsv.tab.decode(content)
}
