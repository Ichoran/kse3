// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import java.nio._

import kse.flow._


final class JsonCharArrayParser() extends JsonGenericCharParser[Array[Char]] with JsonGenericParser.Slicing[Array[Char]] {
  import JsonGenericParser._

  ///////////////////////////////
  // Implementation of inline methods that deal with the data buffer
  //////////////////////////////

  private[jsonal] var ix0 = 0
  private[jsonal] var ixN = Int.MaxValue
  private[jsonal] var ix = 0

  private[jsonal] inline def getI(in: Array[Char]): Int = { val c = in(ix); ix += 1; c }
  private[jsonal] inline def hasSome(in: Array[Char]): Boolean = ix < ixN
  private[jsonal] inline def getPos(in: Array[Char]): Int = ix
  private[jsonal] inline def globalPos(in: Array[Char]): Long = ix - ix0
  private[jsonal] inline def canZeroPos(in: Array[Char]): Unit = {}
  private[jsonal] inline def setPos(in: Array[Char])(pos: Int): Unit = { ix = pos }
  private[jsonal] inline def movePos(in: Array[Char])(delta: Int): Unit = { ix += delta }
  private[jsonal] inline def hasAtLeast(in: Array[Char])(n: Int): Boolean = ixN - ix >= n
  private[jsonal] inline def whiteless(in: Array[Char]): Int =
    var c = -129
    while (
      { if (ix < ixN) true else { c = -129; false } } && 
      { c = in(ix); ix += 1; isWhite(c-8) }
    ) {}
    c

  private[jsonal] inline def getC(in: Array[Char]): Char = { val c = in(ix); ix += 1; c }
  private[jsonal] inline def hexify(in: Array[Char]): Int = JsonGenericParser.hexify({ val c = in(ix); ix += 1; c })
  private[jsonal] inline def subString(in: Array[Char])(start: Int, end: Int): String = new String(in, start, end - start)
  private[jsonal] inline def subBuffer(in: Array[Char])(buffer: java.lang.StringBuilder)(start: Int, end: Int): Unit = buffer.append(in, start, end)

  private[jsonal] inline def initSlice(in: Array[Char], i0: Int, iN: Int): this.type =
    ix0 = if (i0 < 0) 0 else if (i0 > in.length) in.length else i0
    ix = ix0
    ixN = if (iN < ix0) ix0 else if (iN > in.length) in.length else iN
    this

  ///////////////////////////////
  // End of implementation of inline methods dealing with the data buffer
  ///////////////////////////////


  def parse(input: Array[Char]): Jast =
    ix0 = 0
    ix = 0
    ixN = input.length
    myParseVal(input)

  def parse(input: Array[Char], start: Int, end: Int): Jast =
    ix0 = start
    ix = start
    ixN = if (start < 0 || end < start) -1 else end
    myParseVal(input)

  /////////////
  // Important invariants within methods:
  //    c holds the current character
  //    If a unique character is already parsed, the method that parses
  //      the rest of it expects it to already be consumed
  /////////////

  private[jsonal] def parseVal(input: Array[Char]): Jast = myParseVal(input)
  private[this] def myParseVal(input: Array[Char]): Jast =
    val c = whiteless(input)
    if (c == -129) JastError("end of input, no value found", getPos(input) - 1)
    else parseValStartingWith(input, c)

  private[jsonal] def parseValStartingWith(input: Array[Char], c: Int): Jast = c match
    case '"' => myParseStr(input)
    case '[' => myParseArr(input)
    case '{' => myParseObj(input)
    case x if x >= '0' && x <= '9' => myParseJastNum(input, c)
    case '-' => myParseJastNum(input, c)
    case 'n' => myParseNull(input)
    case 't' => myParseTrue(input)
    case 'f' => myParseFalse(input)
    case _ =>
      ix -= 1
      JastError(s"invalid character: '${c.toChar}'", ix)


  private[jsonal] def parseNull(input: Array[Char]): Json.Null.type | JastError = myParseNull(input)
  private[this] def myParseNull(input: Array[Char]): Json.Null.type | JastError = parseNullImpl(input)

  private[this] def myParseTrue(input: Array[Char]): Json.Bool.True.type | JastError = parseTrueImpl(input)
  private[this] def myParseFalse(input: Array[Char]): Json.Bool.False.type | JastError = parseFalseImpl(input)

  private[jsonal] def parseBool(input: Array[Char]): Json.Bool | JastError = myParseBool(input)
  private[this] def myParseBool(input: Array[Char]): Json.Bool | JastError = parseBoolImpl(input)

  private[jsonal] def parseStr(input: Array[Char]): Json.Str | JastError = myParseStr(input)
  private[this] def myParseStr(input: Array[Char]): Json.Str | JastError = parseStrImpl(input)


  private[jsonal] def parseRawNum(input: Array[Char], initial: Int, toCache: Boolean): Double = myParseRawNum(input, initial, toCache)
  private[this] def myParseRawNum(input: Array[Char], initial: Int, toCache: Boolean): Double = parseRawNumImpl(input, initial, toCache)

  private[jsonal] def parseJastNum(input: Array[Char], initial: Int): Json.Num | JastError = myParseJastNum(input, initial)
  private[this] def myParseJastNum(input: Array[Char], initial: Int): Json.Num | JastError =
    myParseRawNum(input, initial, toCache = true)
    decacheAs[Json.Num | JastError]()

  private[jsonal] def parseArrD(input: Array[Char], initial: Int): Int = myParseArrD(input, initial)
  private[this] def myParseArrD(input: Array[Char], initial: Int): Int = parseArrDImpl(input, initial)

  private[jsonal] def parseArr(input: Array[Char]): Json.Arr | JastError = myParseArr(input)
  private[this] def myParseArr(input: Array[Char]): Json.Arr | JastError = parseArrImpl(input)

  private[jsonal] def parseObj(input: Array[Char]): Json.Obj | JastError = myParseObj(input)
  private[this] def myParseObj(input: Array[Char]): Json.Obj | JastError = parseObjImpl(input)
}
object JsonCharArrayParser extends JsonGenericParserCompanion.Sliced[Array[Char], JsonCharArrayParser] {
  def newParser = new JsonCharArrayParser

  def parse(input: Array[Char], relaxed: Boolean = false): Jast = parseImpl(input, relaxed)
  def parse(input: Array[Char], i0: Int, iN: Int): Jast = parseSliceImpl(input, i0, iN, false)
  def parse(input: Array[Char], i0: Int, iN: Int, relaxed: Boolean): Jast = parseSliceImpl(input, i0, iN, relaxed)

  def parseJson(input: Array[Char], relaxed: Boolean = false): kse.jsonal.Json | JastError = parseJsonImpl(input, relaxed)
  def parseJson(input: Array[Char], i0: Int, iN: Int): kse.jsonal.Json | JastError = parseJsonSliceImpl(input, i0, iN, false)
  def parseJson(input: Array[Char], i0: Int, iN: Int, relaxed: Boolean): kse.jsonal.Json | JastError = parseJsonSliceImpl(input, i0, iN, relaxed)

  def parseNull(input: Array[Char]): kse.jsonal.Json.Null.type | JastError = parseNullImpl(input)
  def parseNull(input: Array[Char], i0: Int, iN: Int): kse.jsonal.Json.Null.type | JastError = parseNullSliceImpl(input, i0, iN)
  
  def parseBool(input: Array[Char]): kse.jsonal.Json.Bool | JastError = parseBoolImpl(input)
  def parseBool(input: Array[Char], i0: Int, iN: Int): kse.jsonal.Json.Bool | JastError = parseBoolSliceImpl(input, i0, iN)
  
  def parseStr(input: Array[Char]): kse.jsonal.Json.Str | JastError = parseStrImpl(input)
  def parseStr(input: Array[Char], i0: Int, iN: Int): kse.jsonal.Json.Str | JastError = parseStrSliceImpl(input, i0, iN)

  def parseNum(input: Array[Char], relaxed: Boolean = false): kse.jsonal.Json.Num | JastError = parseNumImpl(input, relaxed)
  def parseNum(input: Array[Char], i0: Int, iN: Int): kse.jsonal.Json.Num | JastError = parseNumSliceImpl(input, i0, iN, false)
  def parseNum(input: Array[Char], i0: Int, iN: Int, relaxed: Boolean): kse.jsonal.Json.Num | JastError = parseNumSliceImpl(input, i0, iN, relaxed)
  
  def parseArr(input: Array[Char], relaxed: Boolean = false): kse.jsonal.Json.Arr | JastError = parseArrImpl(input, relaxed)
  def parseArr(input: Array[Char], i0: Int, iN: Int): kse.jsonal.Json.Arr | JastError = parseArrSliceImpl(input, i0, iN, false)
  def parseArr(input: Array[Char], i0: Int, iN: Int, relaxed: Boolean): kse.jsonal.Json.Arr | JastError = parseArrSliceImpl(input, i0, iN, relaxed)
  
  def parseObj(input: Array[Char], relaxed: Boolean = false): kse.jsonal.Json.Obj | JastError = parseObjImpl(input, relaxed)
  def parseObj(input: Array[Char], i0: Int, iN: Int): kse.jsonal.Json.Obj | JastError = parseObjSliceImpl(input, i0, iN, false)
  def parseObj(input: Array[Char], i0: Int, iN: Int, relaxed: Boolean): kse.jsonal.Json.Obj | JastError = parseObjSliceImpl(input, i0, iN, relaxed)
}
