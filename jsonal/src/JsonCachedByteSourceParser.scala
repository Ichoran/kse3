// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import java.nio._

import kse.flow._

final class JsonCachedByteSourceParser(minCacheSize: Int = 14) extends JsonGenericByteParser[JsonCachedByteSourceParser.ByteSource] {
  import JsonGenericParser._
  import JsonCachedByteSourceParser.ByteSource

  ///////////////////////////////
  // Implementation of inline methods that actually deal with the data buffer
  //////////////////////////////
  private[jsonal] var ixN = 0
  private[jsonal] var ix = 0
  private[jsonal] var ixZ = 0L
  private[jsonal] var buf: Array[Byte] = 
    var n = 14
    while (n < minCacheSize && n < 0x7FFFFFFC) n = (n << 1) | 0x4
    new Array[Byte](n)
  private[jsonal] def myZeroPos(): Unit =
    val n = 2 + (ixN - ix)
    System.arraycopy(buf, ix - 2, buf, 0, n)
    ixZ += (ix - 2)
    ix = 2
    ixN = n
  private[jsonal] def myExpandCache(input: ByteSource): Boolean =
    if (ixN > 0 && (buf.length < 4096 || ixN > buf.length - (buf.length >> 3)))
      buf = java.util.Arrays.copyOf(buf, (0x7FFFFFFC & ((buf.length << 1) | 0x4)))
    val r = input.read(buf, ixN)
    if (r > 0) ixN += r
    r > 0

  private[jsonal] inline def getI(in: ByteSource): Int = { val c = buf(ix); ix += 1; c }
  private[jsonal] inline def hasSome(in: ByteSource): Boolean = ix < ixN || myExpandCache(in)
  private[jsonal] inline def getPos(in: ByteSource): Int = ix
  private[jsonal] inline def globalPos(in: ByteSource): Long = ixZ + ix
  private[jsonal] inline def canZeroPos(in: ByteSource): Unit =
    if (ix > 0x1FFFFFF || (ix > 8192 && ix > buf.length - (buf.length >> 2))) myZeroPos()
  private[jsonal] inline def setPos(in: ByteSource)(pos: Int): Unit = { ix = pos }
  private[jsonal] inline def movePos(in: ByteSource)(delta: Int): Unit = { ix += delta }
  private[jsonal] inline def hasAtLeast(in: ByteSource)(n: Int): Boolean = ixN - ix >= n || (myExpandCache(in) && ixN - ix >= n)
  private[jsonal] inline def whiteless(in: ByteSource): Int =
    var c = -129
    while (
      { if (ix < ixN || myExpandCache(in)) true else { c = -129; false } } && 
      { c = buf(ix); ix += 1; isWhite(c-8) }
    ) {}
    c

  private[jsonal] inline def getB(in: ByteSource): Byte = { val c = buf(ix); ix += 1; c }
  private[jsonal] inline def hexify(in: ByteSource): Int = JsonGenericParser.hexify({ val c = buf(ix); ix += 1; c })
  private[jsonal] inline def subString(in: ByteSource)(start: Int, end: Int): String =
    new String(buf, start, end - start, java.nio.charset.StandardCharsets.UTF_8)
  private[jsonal] inline def subBuffer(in: ByteSource)(buffer: java.lang.StringBuilder)(start: Int, end: Int): Unit =
    buffer append subString(in)(start, end)

  ///////////////////////////////
  // End of implementation of inline methods dealing with the data buffer
  ///////////////////////////////


  def parse(input: ByteSource): Jast =
    ix = 0
    ixN = 0
    ixZ = 0L
    if (options.trim) { if (whiteless(input) != -129) movePos(input)(-1) }
    val ans = myParseVal(input)
    val err = ans.isInstanceOf[JastError]
    if (!err & options.trim) { if (whiteless(input) != -129) movePos(input)(-1) }
    val full = !hasSome(input)
    options.outcome match
      case Some(o) => o.complete = full; o.error = err; o.consumed += ixZ + ix
      case _ =>
    if (full || !options.complete) ans else JastError("JSON parse covered only part of input", globalPos(input))

  /////////////
  // Important invariants within methods:
  //    c holds the current character
  //    If a unique character is already parsed, the method that parses
  //      the rest of it gets a ByteBuffer pointing past that character
  //    Note that (c & 0xFFFFFFE0) == 0 && ((1 << c) & 0x1000026) != 0
  //      is a magic incantation to test whether (c+8) is whitespace (c: Int)
  /////////////

  private[jsonal] def parseVal(input: ByteSource): Jast = myParseVal(input)
  private[this] def myParseVal(input: ByteSource): Jast =
    val c = whiteless(input)
    if (c == -129) JastError("end of input, no value found", getPos(input) - 1)
    else parseValStartingWith(input, c)

  private[jsonal] def parseValStartingWith(input: ByteSource, c: Int): Jast = c match
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


  private[jsonal] def parseNull(input: ByteSource): Json.Null.type | JastError = myParseNull(input)
  private[this] def myParseNull(input: ByteSource): Json.Null.type | JastError = parseNullImpl(input)

  private[this] def myParseTrue(input: ByteSource): Json.Bool.True.type | JastError = parseTrueImpl(input)
  private[this] def myParseFalse(input: ByteSource): Json.Bool.False.type | JastError = parseFalseImpl(input)

  private[jsonal] def parseBool(input: ByteSource): Json.Bool | JastError = myParseBool(input)
  private[this] def myParseBool(input: ByteSource): Json.Bool | JastError = parseBoolImpl(input)

  private[jsonal] def parseStr(input: ByteSource): Json.Str | JastError = myParseStr(input)
  private[this] def myParseStr(input: ByteSource): Json.Str | JastError = parseStrImpl(input)


  private[jsonal] def parseRawNum(input: ByteSource, initial: Int, toCache: Boolean): Double = myParseRawNum(input, initial, toCache)
  private[this] def myParseRawNum(input: ByteSource, initial: Int, toCache: Boolean): Double = parseRawNumImpl(input, initial, toCache)

  private[jsonal] def parseJastNum(input: ByteSource, initial: Int): Json.Num | JastError = myParseJastNum(input, initial)
  private[this] def myParseJastNum(input: ByteSource, initial: Int): Json.Num | JastError =
    myParseRawNum(input, initial, toCache = true)
    decacheAs[Json.Num | JastError]()

  private[jsonal] def parseArrD(input: ByteSource, initial: Int): Int = myParseArrD(input, initial)
  private[this] def myParseArrD(input: ByteSource, initial: Int): Int = parseArrDImpl(input, initial)

  private[jsonal] def parseArr(input: ByteSource): Json.Arr | JastError = myParseArr(input)
  private[this] def myParseArr(input: ByteSource): Json.Arr | JastError = parseArrImpl(input)

  private[jsonal] def parseObj(input: ByteSource): Json.Obj | JastError = myParseObj(input)
  private[this] def myParseObj(input: ByteSource): Json.Obj | JastError = parseObjImpl(input)
}
object JsonCachedByteSourceParser {
  trait ByteSource {
    /** Read as many bytes are as available from source filling as much of dest as available, starting at start.
     *  Returns the number of bytes actually read.
     * 
     *  This should block until at least one character is available.
     *  A result of 0 or negative means that the source is permanently exhausted.
     */
    def read(dest: Array[Byte], start: Int): Int
  }

  final class InputStreamSource(val source: java.io.InputStream) extends ByteSource {
    def read(dest: Array[Byte], start: Int): Int = source.read(dest, start, dest.length - start)
  }

  final class IteratorSource(val source: Iterator[Byte]) extends ByteSource {
    def read(dest: Array[Byte], start: Int): Int =
      var i = start
      while (source.hasNext && i < dest.length) {
        dest(i) = source.next
        i += 1
      }
      i - start
  }

  def source(source: java.io.InputStream): InputStreamSource = new InputStreamSource(source)
  def source(source: Iterator[Byte]): IteratorSource = new IteratorSource(source)
}
