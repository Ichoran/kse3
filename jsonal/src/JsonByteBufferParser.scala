// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import java.nio._

import kse.flow._


/** This class parses JSON out of ByteBuffer parsers. */
final class JsonByteBufferParser() extends JsonGenericByteParser[ByteBuffer] {
  import JsonGenericParser._

  ///////////////////////////////
  // Implementation of inline methods that actually deal with the data buffer
  //////////////////////////////

  private[jsonal] inline def getI(in: ByteBuffer): Int = in.get
  private[jsonal] inline def hasSome(in: ByteBuffer): Boolean = in.hasRemaining
  private[jsonal] inline def getPos(in: ByteBuffer): Int = in.position
  private[jsonal] inline def globalPos(in: ByteBuffer): Long = in.position
  private[jsonal] inline def canZeroPos(in: ByteBuffer): Unit = {}
  private[jsonal] inline def setPos(in: ByteBuffer)(pos: Int): Unit = in.position(pos)
  private[jsonal] inline def movePos(in: ByteBuffer)(delta: Int): Unit = in.position(in.position + delta)
  private[jsonal] inline def hasAtLeast(in: ByteBuffer)(n: Int): Boolean = in.remaining >= n
  private[jsonal] inline def whiteless(in: ByteBuffer): Int =
    var c = -129
    while (
      { if (hasSome(in)) true else { c = -129; false } } && 
      { c = in.get; isWhite(c-8) }
    ) {}
    c

  private[jsonal] inline def getB(in: ByteBuffer): Byte = in.get
  private[jsonal] inline def hexify(in: ByteBuffer): Int = JsonGenericParser.hexify(in.get)
  private[jsonal] inline def subString(in: ByteBuffer)(start: Int, end: Int): String =
    JsonByteBufferParser.extractString(in)(start, end)
  private[jsonal] inline def subBuffer(in: ByteBuffer)(buffer: java.lang.StringBuilder)(start: Int, end: Int): Unit =
    JsonByteBufferParser.buildString(in)(buffer)(start, end)

  ///////////////////////////////
  // End of implementation of inline methods dealing with the data buffer
  ///////////////////////////////


  def parse(input: ByteBuffer): Jast = myParseVal(input)

  /////////////
  // Important invariants within methods:
  //    c holds the current character
  //    If a unique character is already parsed, the method that parses
  //      the rest of it gets a ByteBuffer pointing past that character
  //    Note that (c & 0xFFFFFFE0) == 0 && ((1 << c) & 0x1000026) != 0
  //      is a magic incantation to test whether (c+8) is whitespace (c: Int)
  /////////////

  private[jsonal] def parseVal(input: ByteBuffer): Jast = myParseVal(input)
  private[this] def myParseVal(input: ByteBuffer): Jast =
    val c = whiteless(input)
    if (c == -129) JastError("end of input, no value found", getPos(input) - 1)
    else parseValStartingWith(input, c)

  private[jsonal] def parseValStartingWith(input: ByteBuffer, c: Int): Jast = c match
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


  private[jsonal] def parseNull(input: ByteBuffer): Json.Null.type | JastError = myParseNull(input)
  private[this] def myParseNull(input: ByteBuffer): Json.Null.type | JastError = parseNullImpl(input)

  private[this] def myParseTrue(input: ByteBuffer): Json.Bool.True.type | JastError = parseTrueImpl(input)
  private[this] def myParseFalse(input: ByteBuffer): Json.Bool.False.type | JastError = parseFalseImpl(input)

  private[jsonal] def parseBool(input: ByteBuffer): Json.Bool | JastError = myParseBool(input)
  private[this] def myParseBool(input: ByteBuffer): Json.Bool | JastError = parseBoolImpl(input)

  private[jsonal] def parseStr(input: ByteBuffer): Json.Str | JastError = myParseStr(input)
  private[this] def myParseStr(input: ByteBuffer): Json.Str | JastError = parseStrImpl(input)


  private[jsonal] def parseRawNum(input: ByteBuffer, initial: Int, toCache: Boolean): Double = myParseRawNum(input, initial, toCache)
  private[this] def myParseRawNum(input: ByteBuffer, initial: Int, toCache: Boolean): Double = parseRawNumImpl(input, initial, toCache)

  private[jsonal] def parseJastNum(input: ByteBuffer, initial: Int): Json.Num | JastError = myParseJastNum(input, initial)
  private[this] def myParseJastNum(input: ByteBuffer, initial: Int): Json.Num | JastError =
    myParseRawNum(input, initial, toCache = true)
    decacheAs[Json.Num | JastError]()

  private[jsonal] def parseArrD(input: ByteBuffer, initial: Int): Int = myParseArrD(input, initial)
  private[this] def myParseArrD(input: ByteBuffer, initial: Int): Int = parseArrDImpl(input, initial)

  private[jsonal] def parseArr(input: ByteBuffer): Json.Arr | JastError = myParseArr(input)
  private[this] def myParseArr(input: ByteBuffer): Json.Arr | JastError = parseArrImpl(input)

  private[jsonal] def parseObj(input: ByteBuffer): Json.Obj | JastError = myParseObj(input)
  private[this] def myParseObj(input: ByteBuffer): Json.Obj | JastError = parseObjImpl(input)
}


object JsonByteBufferParser extends JsonGenericParserCompanion.Positional[ByteBuffer, JsonByteBufferParser] {
  import JsonGenericParser._

  def extractString(buffer: ByteBuffer)(start: Int, end: Int): String =
    if (buffer.hasArray) new String(buffer.array, buffer.arrayOffset + start, end - start, java.nio.charset.StandardCharsets.UTF_8)
    else {
      val p = buffer.position
      buffer.position(start)
      val a = new Array[Byte](end - start)
      buffer.get(a)
      buffer.position(p)
      new String(a, java.nio.charset.StandardCharsets.UTF_8)
    }

  def buildString(buffer: ByteBuffer)(sb: java.lang.StringBuilder)(start: Int, end: Int): Unit =
    val p = buffer.position
    buffer.position(start)
    while (buffer.position < end) {
      val c = buffer.get
      if (c >= 0) sb append c.toChar
      else {
        sb append extractString(buffer)(buffer.position - 1, end)
        buffer.position(end)
      }
    }
    buffer.position(p)

  protected inline def hasAtLeast(in: ByteBuffer)(n: Int): Boolean = in.remaining >= n
  protected inline def getPos(in: ByteBuffer): Int = in.position
  protected inline def setPos(in: ByteBuffer)(pos: Int): Unit = in.position(pos)
  protected inline def backOne(in: ByteBuffer): Unit = in.position(in.position - 1)
  protected inline def getC(in: ByteBuffer): Char = in.get.toChar

  def newParser = new JsonByteBufferParser

  def parse(input: ByteBuffer, relaxed: Boolean = false): Jast = parseImpl(input, relaxed)

  def parseJson(input: ByteBuffer, relaxed: Boolean = false): kse.jsonal.Json | JastError = parseJsonImpl(input, relaxed)

  def parseNull(input: ByteBuffer): kse.jsonal.Json.Null.type | JastError = parseNullImpl(input)
  
  def parseBool(input: ByteBuffer): kse.jsonal.Json.Bool | JastError = parseBoolImpl(input)
  
  def parseStr(input: ByteBuffer): kse.jsonal.Json.Str | JastError = parseStrImpl(input)

  def parseNum(input: ByteBuffer, relaxed: Boolean = false): kse.jsonal.Json.Num | JastError = parseNumImpl(input, relaxed)
  
  def parseArr(input: ByteBuffer, relaxed: Boolean = false): kse.jsonal.Json.Arr | JastError = parseArrImpl(input, relaxed)
  
  def parseObj(input: ByteBuffer, relaxed: Boolean = false): kse.jsonal.Json.Obj | JastError = parseObjImpl(input, relaxed)
}
