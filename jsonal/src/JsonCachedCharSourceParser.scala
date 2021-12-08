// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import java.nio._

import kse.flow._


final class JsonCachedCharSourceParser(minCacheSize: Int = 14) extends JsonGenericCharParser[JsonCachedCharSourceParser.CharSource] {
  import JsonGenericParser._
  import JsonCachedCharSourceParser.CharSource

  ///////////////////////////////
  // Implementation of methods that deal with the data buffer
  //////////////////////////////

  private[jsonal] var ixN = 0
  private[jsonal] var ix = 0
  private[jsonal] var ixZ = 0L
  private[jsonal] var buf: Array[Char] = 
    var n = 14
    while (n < minCacheSize && n < 0x7FFFFFFE) n = (n << 1) | 2
    new Array[Char](n)
  private[jsonal] def myZeroPos(): Unit =
    val n = 2 + (ixN - ix)
    System.arraycopy(buf, ix - 2, buf, 0, n)
    ixZ += (ix - 2)
    ix = 2
    ixN = n
  private[jsonal] def myExpandCache(input: CharSource): Boolean =
    if (ixN > 0 && (buf.length < 4096 || ixN > buf.length - (buf.length >> 3)))
      buf = java.util.Arrays.copyOf(buf, (0x7FFFFFFE & ((buf.length << 1) | 2)))
    val r = input.read(buf, ixN)
    if (r > 0) ixN += r
    r > 0

  private[jsonal] inline def getI(in: CharSource): Int = { val c = buf(ix); ix += 1; c }
  private[jsonal] inline def hasSome(in: CharSource): Boolean = ix < ixN || myExpandCache(in)
  private[jsonal] inline def getPos(in: CharSource): Int = ix
  private[jsonal] inline def globalPos(in: CharSource): Long = ixZ + ix
  private[jsonal] inline def canZeroPos(in: CharSource): Unit =
    if (ix > 0x1FFFFFF || (ix > 8192 && ix > buf.length - (buf.length >> 2))) myZeroPos()
  private[jsonal] inline def setPos(in: CharSource)(pos: Int): Unit = { ix = pos }
  private[jsonal] inline def movePos(in: CharSource)(delta: Int): Unit = { ix += delta }
  private[jsonal] inline def hasAtLeast(in: CharSource)(n: Int): Boolean = ixN - ix >= n || (myExpandCache(in) && ixN - ix >= n)
  private[jsonal] inline def whiteless(in: CharSource): Int =
    var c = -129
    while (
      { if (ix < ixN || myExpandCache(in)) true else { c = -129; false } } && 
      { c = buf(ix); ix += 1; isWhite(c-8) }
    ) {}
    c

  private[jsonal] inline def getC(in: CharSource): Char = { val c = buf(ix); ix += 1; c }
  private[jsonal] inline def hexify(in: CharSource): Int = JsonGenericParser.hexify({ val c = buf(ix); ix += 1; c })
  private[jsonal] inline def subString(in: CharSource)(start: Int, end: Int): String = new String(buf, start, end - start)
  private[jsonal] inline def subBuffer(in: CharSource)(buffer: java.lang.StringBuilder)(start: Int, end: Int): Unit = buffer.append(buf, start, end)

  ///////////////////////////////
  // End of implementation of methods dealing with the data buffer
  ///////////////////////////////


  def parse(input: CharSource): Jast =
    ix = 0
    ixN = 0
    ixZ = 0L
    myParseVal(input)

  /////////////
  // Important invariants within methods:
  //    c holds the current character
  //    If a unique character is already parsed, the method that parses
  //      the rest of it expects it to already be consumed
  /////////////

  private[jsonal] def parseVal(input: CharSource): Jast = myParseVal(input)
  private[this] def myParseVal(input: CharSource): Jast =
    val c = whiteless(input)
    if (c == -129) JastError("end of input, no value found", globalPos(input) - 1)
    else parseValStartingWith(input, c)

  private[jsonal] def parseValStartingWith(input: CharSource, c: Int): Jast = c match
    case '"' => myParseStr(input)
    case '[' => myParseArr(input)
    case '{' => myParseObj(input)
    case x if x >= '0' && x <= '9' => myParseJastNum(input, c)
    case '-' => myParseJastNum(input, c)
    case 'n' => myParseNull(input)
    case 't' => myParseTrue(input)
    case 'f' => myParseFalse(input)
    case _ =>
      JastError(s"invalid character: '${c.toChar}'", globalPos(input))


  private[jsonal] def parseNull(input: CharSource): Json.Null.type | JastError = myParseNull(input)
  private[this] def myParseNull(input: CharSource): Json.Null.type | JastError = parseNullImpl(input)

  private[this] def myParseTrue(input: CharSource): Json.Bool.True.type | JastError = parseTrueImpl(input)
  private[this] def myParseFalse(input: CharSource): Json.Bool.False.type | JastError = parseFalseImpl(input)

  private[jsonal] def parseBool(input: CharSource): Json.Bool | JastError = myParseBool(input)
  private[this] def myParseBool(input: CharSource): Json.Bool | JastError = parseBoolImpl(input)

  private[jsonal] def parseStr(input: CharSource): Json.Str | JastError = myParseStr(input)
  private[this] def myParseStr(input: CharSource): Json.Str | JastError = parseStrImpl(input)


  private[jsonal] def parseRawNum(input: CharSource, initial: Int, toCache: Boolean): Double = myParseRawNum(input, initial, toCache)
  private[this] def myParseRawNum(input: CharSource, initial: Int, toCache: Boolean): Double = parseRawNumImpl(input, initial, toCache)

  private[jsonal] def parseJastNum(input: CharSource, initial: Int): Json.Num | JastError = myParseJastNum(input, initial)
  private[this] def myParseJastNum(input: CharSource, initial: Int): Json.Num | JastError =
    myParseRawNum(input, initial, toCache = true)
    decacheAs[Json.Num | JastError]()

  private[jsonal] def parseArrD(input: CharSource, initial: Int): Int = myParseArrD(input, initial)
  private[this] def myParseArrD(input: CharSource, initial: Int): Int = parseArrDImpl(input, initial)

  private[jsonal] def parseArr(input: CharSource): Json.Arr | JastError = myParseArr(input)
  private[this] def myParseArr(input: CharSource): Json.Arr | JastError = parseArrImpl(input)

  private[jsonal] def parseObj(input: CharSource): Json.Obj | JastError = myParseObj(input)
  private[this] def myParseObj(input: CharSource): Json.Obj | JastError = parseObjImpl(input)
}
object JsonCachedCharSourceParser {
  trait CharSource {
    /** Read as many bytes are as available from source filling as much of dest as available, starting at start.
     *  Returns the number of bytes actually read.
     * 
     *  This should block until at least one character is available.
     *  A result of 0 or negative means that the source is permanently exhausted.
     */
    def read(dest: Array[Char], start: Int): Int
  }

  final class IteratorSource(val source: Iterator[Char]) extends CharSource {
    def read(dest: Array[Char], start: Int): Int =
      var i = start
      while (source.hasNext && i < dest.length) {
        dest(i) = source.next
        i += 1
      }
      i - start
  }

  def source(source: Iterator[Char]): IteratorSource = new IteratorSource(source)
}
