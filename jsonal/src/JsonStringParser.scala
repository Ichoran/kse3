// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import java.nio._

import kse.flow._


final class JsonStringParser() extends JsonGenericCharParser[String] with JsonGenericParser.Slicing[String] {
  import JsonGenericParser._

  ///////////////////////////////
  // Implementation of inline methods that deal with the data buffer
  //////////////////////////////

  private[jsonal] var ix0 = 0
  private[jsonal] var ixN = Int.MaxValue
  private[jsonal] var ix = 0

  private[jsonal] inline def getI(in: String): Int = { val c = in.charAt(ix); ix += 1; c }
  private[jsonal] inline def hasSome(in: String): Boolean = ix < ixN
  private[jsonal] inline def getPos(in: String): Int = ix
  private[jsonal] inline def globalPos(in: String): Long = ix - ix0
  private[jsonal] inline def canZeroPos(in: String): Unit = {}
  private[jsonal] inline def setPos(in: String)(pos: Int): Unit = { ix = pos }
  private[jsonal] inline def movePos(in: String)(delta: Int): Unit = { ix += delta }
  private[jsonal] inline def hasAtLeast(in: String)(n: Int): Boolean = ixN - ix >= n
  private[jsonal] inline def whiteless(in: String): Int =
    var c = -129
    while (
      { if (ix < ixN) true else { c = -129; false } } && 
      { c = in.charAt(ix); ix += 1; isWhite(c-8) }
    ) {}
    c

  private[jsonal] inline def getC(in: String): Char = { val c = in.charAt(ix); ix += 1; c }
  private[jsonal] inline def hexify(in: String): Int = JsonGenericParser.hexify({ val c = in.charAt(ix); ix += 1; c })
  private[jsonal] inline def subString(in: String)(start: Int, end: Int): String = in.substring(start, end)
  private[jsonal] inline def subBuffer(in: String)(buffer: java.lang.StringBuilder)(start: Int, end: Int): Unit = buffer.append(in, start, end)

  private[jsonal] inline def initSlice(in: String, i0: Int, iN: Int): this.type =
    ix0 = if (i0 < 0) 0 else if (i0 > in.length) in.length else i0
    ix = ix0
    ixN = if (iN < ix0) ix0 else if (iN > in.length) in.length else iN
    this

  private[jsonal] inline def optionalize[A >: JastError](in: String)(inline f: => A): A =
    val start = ix
    if (options.trim) { if (whiteless(in) != -129) ix -= 1 }
    val ans = f
    val err = ans.isInstanceOf[JastError]
    if (!err && options.trim) { if (whiteless(in) != -129) ix -= 1 }
    val full = ix >= ixN
    options.outcome match
      case Some(o) => o.complete = full; o.error = err; o.consumed += ix - start
      case _ =>
    if (full || !options.complete) ans else JastError("JSON parse covered only part of input", globalPos(in))

  ///////////////////////////////
  // End of implementation of inline methods dealing with the data buffer
  ///////////////////////////////


  def parse(input: String): Jast =
    ix0 = 0
    ix = 0
    ixN = input.length
    optionalize(input)(myParseVal(input))

  def parse(input: String, start: Int, end: Int): Jast =
    ix0 = start
    ix = start
    ixN = if (start < 0 || end < start) -1 else end
    optionalize(input)(myParseVal(input))

  /////////////
  // Important invariants within methods:
  //    c holds the current character
  //    If a unique character is already parsed, the method that parses
  //      the rest of it expects it to already be consumed
  /////////////

  private[jsonal] def parseVal(input: String): Jast = myParseVal(input)
  private[this] def myParseVal(input: String): Jast =
    val c = whiteless(input)
    if (c == -129) JastError("end of input, no value found", getPos(input) - 1)
    else parseValStartingWith(input, c)

  private[jsonal] def parseValStartingWith(input: String, c: Int): Jast = c match
    case '"' => myParseStr(input)
    case '[' => myParseArr(input)
    case '{' => myParseObj(input)
    case x if x >= '0' && x <= '9' => myParseJastNum(input, c)
    case '-' => myParseJastNum(input, c)
    case 'n' => myParseNull(input)
    case 't' => myParseTrue(input)
    case 'f' => myParseFalse(input)
    case _ =>   JastError(s"invalid character: '${c.toChar}'", globalPos(input))


  private[jsonal] def parseNull(input: String): Json.Null.type | JastError = myParseNull(input)
  private[this] def myParseNull(input: String): Json.Null.type | JastError = parseNullImpl(input)

  private[this] def myParseTrue(input: String): Json.Bool.True.type | JastError = parseTrueImpl(input)
  private[this] def myParseFalse(input: String): Json.Bool.False.type | JastError = parseFalseImpl(input)

  private[jsonal] def parseBool(input: String): Json.Bool | JastError = myParseBool(input)
  private[this] def myParseBool(input: String): Json.Bool | JastError = parseBoolImpl(input)

  private[jsonal] def parseStr(input: String): Json.Str | JastError = myParseStr(input)
  private[this] def myParseStr(input: String): Json.Str | JastError = parseStrImpl(input)


  private[jsonal] def parseRawNum(input: String, initial: Int, toCache: Boolean): Double = myParseRawNum(input, initial, toCache)
  private[this] def myParseRawNum(input: String, initial: Int, toCache: Boolean): Double = parseRawNumImpl(input, initial, toCache)

  private[jsonal] def parseJastNum(input: String, initial: Int): Json.Num | JastError = myParseJastNum(input, initial)
  private[this] def myParseJastNum(input: String, initial: Int): Json.Num | JastError =
    myParseRawNum(input, initial, toCache = true)
    decacheAs[Json.Num | JastError]()

  private[jsonal] def parseArrD(input: String, initial: Int): Int = myParseArrD(input, initial)
  private[this] def myParseArrD(input: String, initial: Int): Int = parseArrDImpl(input, initial)

  private[jsonal] def parseArr(input: String): Json.Arr | JastError = myParseArr(input)
  private[this] def myParseArr(input: String): Json.Arr | JastError = parseArrImpl(input)

  private[jsonal] def parseObj(input: String): Json.Obj | JastError = myParseObj(input)
  private[this] def myParseObj(input: String): Json.Obj | JastError = parseObjImpl(input)
}
object JsonStringParser extends JsonGenericParserCompanion.Sliced[String, JsonStringParser] {
  def newParser = new JsonStringParser

  def parse(input: String, options: JsonOptions = JsonOptions.Default): Jast = parseImpl(input, options)
  def parse(input: String, i0: Int, iN: Int, options: JsonOptions): Jast = parseSliceImpl(input, i0, iN, options)

  def parseJson(input: String, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json | JastError = parseJsonImpl(input, options)
  def parseJson(input: String, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json | JastError = parseJsonSliceImpl(input, i0, iN, options)

  def parseNull(input: String, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Null.type | JastError = parseNullImpl(input, options)
  def parseNull(input: String, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Null.type | JastError = 
    parseNullSliceImpl(input, i0, iN, options)
  
  def parseBool(input: String, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Bool | JastError = parseBoolImpl(input, options)
  def parseBool(input: String, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Bool | JastError =
    parseBoolSliceImpl(input, i0, iN, options)
  
  def parseStr(input: String, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Str | JastError = parseStrImpl(input, options)
  def parseStr(input: String, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Str | JastError =
    parseStrSliceImpl(input, i0, iN, options)

  def parseNum(input: String, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Num | JastError = parseNumImpl(input, options)
  def parseNum(input: String, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Num | JastError = 
    parseNumSliceImpl(input, i0, iN, options)
  
  def parseArr(input: String, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Arr | JastError = parseArrImpl(input, options)
  def parseArr(input: String, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Arr | JastError = 
    parseArrSliceImpl(input, i0, iN, options)
  
  def parseObj(input: String, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Obj | JastError = parseObjImpl(input, options)
  def parseObj(input: String, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Obj | JastError =
    parseObjSliceImpl(input, i0, iN, options)
}

