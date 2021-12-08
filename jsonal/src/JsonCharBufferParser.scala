// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import java.nio._

import kse.flow._


/** This class parses JSON out of CharBuffer parsers. */
final class JsonCharBufferParser() extends JsonGenericCharParser[CharBuffer] {
  import JsonGenericParser._

  ///////////////////////////////
  // Implementation of inline methods that actually deal with the data buffer
  //////////////////////////////

  private[jsonal] inline def getI(in: CharBuffer): Int = in.get
  private[jsonal] inline def hasSome(in: CharBuffer): Boolean = in.hasRemaining
  private[jsonal] inline def getPos(in: CharBuffer): Int = in.position
  private[jsonal] inline def globalPos(in: CharBuffer): Long = in.position
  private[jsonal] inline def canZeroPos(in: CharBuffer): Unit = {}
  private[jsonal] inline def setPos(in: CharBuffer)(pos: Int): Unit = in.position(pos)
  private[jsonal] inline def movePos(in: CharBuffer)(delta: Int): Unit = in.position(in.position + delta)
  private[jsonal] inline def hasAtLeast(in: CharBuffer)(n: Int): Boolean = in.remaining >= n
  private[jsonal] inline def whiteless(in: CharBuffer): Int =
    var c = -129
    while (
      { if (hasSome(in)) true else { c = -129; false } } && 
      { c = in.get; isWhite(c-8) }
    ) {}
    c

  private[jsonal] inline def getC(in: CharBuffer): Char = in.get
  private[jsonal] inline def hexify(in: CharBuffer): Int = JsonGenericParser.hexify(in.get)
  private[jsonal] inline def subString(in: CharBuffer)(start: Int, end: Int): String =
    JsonCharBufferParser.extractString(in)(start, end)
  private[jsonal] inline def subBuffer(in: CharBuffer)(buffer: java.lang.StringBuilder)(start: Int, end: Int): Unit =
    JsonCharBufferParser.buildString(in)(buffer)(start, end)

  ///////////////////////////////
  // End of implementation of inline methods dealing with the data buffer
  ///////////////////////////////


  def parse(input: CharBuffer): Jast = myParseVal(input)

  /////////////
  // Important invariants within methods:
  //    c holds the current character
  //    If a unique character is already parsed, the method that parses
  //      the rest of it gets a CharBuffer pointing past that character
  //    Note that (c & 0xFFFFFFE0) == 0 && ((1 << c) & 0x1000026) != 0
  //      is a magic incantation to test whether (c+8) is whitespace (c: Int)
  /////////////

  private[jsonal] def parseVal(input: CharBuffer): Jast = myParseVal(input)
  private[this] def myParseVal(input: CharBuffer): Jast =
    val c = whiteless(input)
    if (c == -129) JastError("end of input, no value found", getPos(input) - 1)
    else parseValStartingWith(input, c)

  private[jsonal] def parseValStartingWith(input: CharBuffer, c: Int): Jast = c match
    case '"' => myParseStr(input)
    case '[' => myParseArr(input)
    case '{' => myParseObj(input)
    case x if x >= '0' && x <= '9' => myParseJastNum(input, c)
    case '-' => myParseJastNum(input, c)
    case 'n' => myParseNull(input)
    case 't' => myParseTrue(input)
    case 'f' => myParseFalse(input)
    case _ =>
      input.position(input.position - 1)
      JastError(s"invalid character: '${c.toChar}'", input.position)


  private[jsonal] def parseNull(input: CharBuffer): Json.Null.type | JastError = myParseNull(input)
  private[this] def myParseNull(input: CharBuffer): Json.Null.type | JastError = parseNullImpl(input)

  private[this] def myParseTrue(input: CharBuffer): Json.Bool.True.type | JastError = parseTrueImpl(input)
  private[this] def myParseFalse(input: CharBuffer): Json.Bool.False.type | JastError = parseFalseImpl(input)

  private[jsonal] def parseBool(input: CharBuffer): Json.Bool | JastError = myParseBool(input)
  private[this] def myParseBool(input: CharBuffer): Json.Bool | JastError = parseBoolImpl(input)

  private[jsonal] def parseStr(input: CharBuffer): Json.Str | JastError = myParseStr(input)
  private[this] def myParseStr(input: CharBuffer): Json.Str | JastError = parseStrImpl(input)


  private[jsonal] def parseRawNum(input: CharBuffer, initial: Int, toCache: Boolean): Double = myParseRawNum(input, initial, toCache)
  private[this] def myParseRawNum(input: CharBuffer, initial: Int, toCache: Boolean): Double = parseRawNumImpl(input, initial, toCache)

  private[jsonal] def parseJastNum(input: CharBuffer, initial: Int): Json.Num | JastError = myParseJastNum(input, initial)
  private[this] def myParseJastNum(input: CharBuffer, initial: Int): Json.Num | JastError =
    myParseRawNum(input, initial, toCache = true)
    decacheAs[Json.Num | JastError]()

  private[jsonal] def parseArrD(input: CharBuffer, initial: Int): Int = myParseArrD(input, initial)
  private[this] def myParseArrD(input: CharBuffer, initial: Int): Int = parseArrDImpl(input, initial)

  private[jsonal] def parseArr(input: CharBuffer): Json.Arr | JastError = myParseArr(input)
  private[this] def myParseArr(input: CharBuffer): Json.Arr | JastError = parseArrImpl(input)

  private[jsonal] def parseObj(input: CharBuffer): Json.Obj | JastError = myParseObj(input)
  private[this] def myParseObj(input: CharBuffer): Json.Obj | JastError = parseObjImpl(input)
}


object JsonCharBufferParser extends JsonGenericParserCompanion.Positional[CharBuffer, JsonCharBufferParser] {
  import JsonGenericParser._

  def extractString(buffer: CharBuffer)(start: Int, end: Int): String =
    if (buffer.hasArray) new String(buffer.array, buffer.arrayOffset + start, end - start)
    else {
      val p = buffer.position
      buffer.position(0)
      val ans = buffer.subSequence(start, end).toString
      buffer.position(p)
      ans
    }

  def buildString(buffer: CharBuffer)(sb: java.lang.StringBuilder)(start: Int, end: Int): Unit =
    val p = buffer.position
    buffer.position(start)
    val q = buffer.limit
    buffer.limit(end min q)
    sb append buffer
    buffer.position(p)
    buffer.limit(q)

  protected inline def hasAtLeast(in: CharBuffer)(n: Int): Boolean = in.remaining >= n
  protected inline def getPos(in: CharBuffer): Int = in.position
  protected inline def setPos(in: CharBuffer)(pos: Int): Unit = in.position(pos)
  protected inline def backOne(in: CharBuffer): Unit = in.position(in.position - 1)
  protected inline def getC(in: CharBuffer): Char = in.get.toChar

  def newParser = new JsonCharBufferParser

  def parse(input: CharBuffer, relaxed: Boolean = false): Jast = parseImpl(input, relaxed)

  def parseJson(input: CharBuffer, relaxed: Boolean = false): kse.jsonal.Json | JastError = parseJsonImpl(input, relaxed)

  def parseNull(input: CharBuffer): kse.jsonal.Json.Null.type | JastError = parseNullImpl(input)
  
  def parseBool(input: CharBuffer): kse.jsonal.Json.Bool | JastError = parseBoolImpl(input)
  
  def parseStr(input: CharBuffer): kse.jsonal.Json.Str | JastError = parseStrImpl(input)

  def parseNum(input: CharBuffer, relaxed: Boolean = false): kse.jsonal.Json.Num | JastError = parseNumImpl(input, relaxed)
  
  def parseArr(input: CharBuffer, relaxed: Boolean = false): kse.jsonal.Json.Arr | JastError = parseArrImpl(input, relaxed)
  
  def parseObj(input: CharBuffer, relaxed: Boolean = false): kse.jsonal.Json.Obj | JastError = parseObjImpl(input, relaxed)
}
