// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import kse.flow._

/** This class abstracts out the components of JSON parsing that are byte-specific.
  *
  * Since almost all parsing is Byte-specific, this contains almost all the parsing code.  See JsonGenericCharParser for the
  * Char variant, which is mostly a cut-and-paste of this (necessary for good code generation, even with inlined implementations).
  */
trait JsonGenericByteParser[In] extends JsonGenericParser[In] {
  import JsonGenericParser._

  private[jsonal] inline def getB(input: In): Byte
  private[jsonal] inline def hexify(input: In): Int
  private[jsonal] inline def subString(input: In)(start: Int, end: Int): String
  private[jsonal] inline def subBuffer(input: In)(buffer: java.lang.StringBuilder)(start: Int, end: Int): Unit

  protected inline def parseNullImpl(input: In): Json.Null.type | JastError =
    val zero = getPos(input) - 1
    if (hasAtLeast(input)(3) && getB(input) == 'u' && getB(input) == 'l' && getB(input) == 'l') Json.Null
    else {
      setPos(input)(zero)
      JastError("Expected 'null'", globalPos(input))
    }


  protected inline def parseTrueImpl(input: In): Json.Bool.True.type | JastError =
    val zero = getPos(input) - 1
    if (hasAtLeast(input)(3) && getB(input) == 'r' && getB(input) == 'u' && getB(input) == 'e') Json.Bool.True
    else {
      setPos(input)(zero)
      JastError("Expected 'true'", globalPos(input))
    }


  protected inline def parseFalseImpl(input: In): Json.Bool.False.type | JastError =
    val zero = getPos(input) - 1
    if (hasAtLeast(input)(4) && getB(input) == 'a' && getB(input) == 'l' && getB(input) == 's' && getB(input) == 'e') Json.Bool.False
    else {
      setPos(input)(zero)
      JastError("Expected 'false'", globalPos(input))
    }

  protected inline def parseBoolImpl(input: In): Json.Bool | JastError =
    if (hasSome(input)) getB(input) match {
      case 't' => parseTrueImpl(input)
      case 'f' => parseFalseImpl(input)
      case c => movePos(input)(-1); JastError("Expected boolean but found character '"+c.toChar+"'", globalPos(input))
    }
    else JastError("Expected boolean but found end of input", globalPos(input))

 
  protected inline def parseStrImpl(input: In): Json.Str | JastError =
    val first = getPos(input)
    val globalFirst = globalPos(input)
    var c: Int = -129
    while ({ if (!hasSome(input)) { c = -129; false} else true } && { c = getB(input); c != '"' && c != '\\' && c >= 0 }) {}
    if (c == '"') Json.Str(subString(input)(first, getPos(input) - 1))
    else if (c == -129) JastError("No closing quotes on string", globalFirst)
    else {
      val sb = new java.lang.StringBuilder
      var p = getPos(input)
      if (p-first > 1) subBuffer(input)(sb)(first, p-1)
      var result: Json.Str | JastError = null

      while ((result eq null) && hasSome(input)) {
        // If we get here, we expect an escape or start of unicode, so need to handle that specially
        val c2 = getB(input)
        if (c == '\\') {
          sb append (c2 match
            case 'n' => '\n'
            case 'r' => '\r'
            case 't' => '\t'
            case 'u' =>
              if (!hasAtLeast(input)(4)) {
                result = JastError("string ends mid-unicode-escape", globalPos(input) - 1)
                '\uFFFF'
              }
              else {
                val h = (hexify(input) << 12) | (hexify(input) << 8) | (hexify(input) << 4) | hexify(input)
                if (h < 0) result = JastError("non-hex value in unicode escape", globalPos(input) - 4)
                h.toChar
              }
            case 'f' => '\f'
            case 'b' => '\b'
            case x => 
              if (x == '"' || x == '/' || x == '\\') x.toChar
              else {
                result = JastError("invalid quoted character '" + x + "'", globalPos(input) - 1)
                '\uFFFF'
              }
          )
        }
        else if ((c & 0xE0) == 0xC0) {
          if ((c2 & 0xC0) != 0x80) result = JastError("Improper UTF-8 encoding", globalPos(input) - 1)
          else {
            c = ((c&0x1F) << 6) | (c2&0x3F)
            if (c < 0x80) result = JastError("Overlong UTF-8 encoding", globalPos(input) - 2)
            else sb append c.toChar
          }
        }
        else if ((c & 0xF0) == 0xE0) {
          if (!hasSome(input)) result = JastError("string ends in the middle of UTF-8 multi-byte character", globalPos(input) - 1)
          else {
            val c3 = getB(input)
            if ((c2 & 0xC0) + (c3 & 0xC0) != 0x100) result = JastError("Improper UTF-8 encoding", globalPos(input) - 2)
            else {
              c = ((c&0xF) << 12) | ((c2&0x3F) << 6) | (c3&0x3F)
              if (c < 0x800) result = JastError("Overlong UTF-8 encoding", globalPos(input) - 3)
              else sb append c.toChar
            }
          }
        }
        else if ((c & 0xF8) == 0xF0) {
          if (!hasAtLeast(input)(2)) result = JastError("string ends in the middle of UTF-8 multi-byte character", globalPos(input)-1)
          else {
            val c3 = getB(input)
            val c4 = getB(input)
            if ((c2 & 0xC0) + (c3 & 0xC0) + (c4 & 0xC0) != 0x180) result = JastError("Improper UTF-8 encoding", globalPos(input) - 3)
            else {
              c = ((c & 0x7) << 18) | ((c2 & 0x3F) << 12) | ((c3 & 0x3F) << 6) | (c4 & 0x3F)
              if (c < 0x10000 || c > 0x10FFFF) result = JastError("Overlong or out of bounds UTF-8 encoding", globalPos(input) - 4)
              else sb appendCodePoint c
            }
          }
        }
        else result = JastError("Improper UTF-8 encoding", globalPos(input) - 2)

        // Now we handle anything remaining (at least the closing quotes), unless we already got an error
        if (result eq null) {
          p = getPos(input)
          var c: Int = -129
          while ({ if (!hasSome(input)) { c = -129; false} else true } && { c = getB(input); c != '"' && c != '\\' && c >= 0 }) {}
          if (c == '"') {
            val q = getPos(input)
            if (q > p+1) subBuffer(input)(sb)(p, q-1)
            result = Json.Str(sb.toString)
          }
          else if (c == -129) result = JastError("No closing quotes on string", globalPos(input) - 1)
          else {
            val q = getPos(input)
            if (q > p+1) subBuffer(input)(sb)(p, q-1)
            p = q
          }
        }

        // When we're done here, we either have a result, or we've loaded the first character of the next escape/unicode sequence
      }
      if (result eq null) JastError("No closing quotes on string", globalPos(input) - 1) else result
    }

  protected inline def parseRawNumImpl(input: In, initial: Int, toCache: Boolean): Double =
    val zero = getPos(input) - 1
    val globalZero = globalPos(input) - 1
    cache = null
    var dadp = 0  // How many of our digits are after the decimal point?
    var dbdp = 0  // How many are before the decimal point?
    var digits = 0L
    var done = false
    var result = Double.NaN
    var c = if (initial != '-') initial else {
      if (hasSome(input)) getB(input).toInt
      else { setPos(input)(zero); cache = JastError("unfinished number", globalZero); done = true; -129 }
    }
    if (!done) {
      if (c > '0' && c <= '9') {
        digits = c - '0'
        dbdp = 1
        while (dbdp < 19 && hasSome(input) && { c = getB(input); c >= '0' && c <= '9'}) { dbdp += 1; digits = digits*10 + (c - '0') }
        if (dbdp >= 19) while (hasSome(input) && { c = getB(input); c >= '0' && c <= '9' }) dbdp += 1
      }
      else if (c == '0') {
        if (hasSome(input) && {c = getB(input); c >= '0' && c <= '9'}) {
          cache = JastError("multi-digit number cannot start with 0", globalPos(input) - 1)
          setPos(input)(zero)
          done = true
        }
      }
      else { cache = JastError("number should start with a numeric digit", globalPos(input) - 1); setPos(input)(zero); done = true }
      if (!done) {
        if (c != '.' && (c | 0x20) != 'e') {
          // Number should be all done.  Might be a Long.  Save it if so!
          if (c < '0' || c > '9') movePos(input)(-1)  // Overshot, so back up
          if (dbdp < 20 && (digits >= 0) || (initial == '-' && digits == Long.MinValue)) {
            // Yes, it's a Long!  Save it.
            if (initial == '-') digits = -digits   // No-op for Long.MinValue, so we're okay
            result = digits.toDouble
            if (toCache) cache = JsonGenericParser.createNum(java.lang.Double.longBitsToDouble(digits), null)
            else if (strictNumbers && result.toLong != digits) cache = wouldNotFitInDouble
          }
          else {
            val text = subString(input)(zero, getPos(input))
            result = text.toDouble
            if (toCache) cache = JsonGenericParser.createNum(result, text)
            else if (strictNumbers && !Json.Num.numericStringEquals(result.toString, text))
              cache = wouldNotFitInDouble
          }
          done = true  // Strictly speaking this is unnecessary, but we leave this here in case it's refactored such that it matters
        }
        else {
          // Number is not done.  Keep parsing it.
          if (c == '.') {
            val M = math.max(19-dbdp, 0)
            while (dadp < M && hasSome(input) && { c = getB(input); c >= '0' && c <= '9' }) { dadp += 1; digits = digits*10 + (c - '0') }
            if (dadp >= M) while (hasSome(input) && { c = getB(input); c >= '0' && c <= '9' }) dadp += 1
            if (dadp == 0) { cache = JastError("need digits after . in number", globalPos(input) - 1); setPos(input)(zero); done = true }
          }
          if (!done) {
            var ex = 0
            if ((c | 0x20) == 'e') {
              // Handle exponent, if it exists
              if (!hasSome(input)) { cache = JastError("need digits after e in number", globalPos(input) - 1); setPos(input)(zero); done = true }
              else {
                c = getB(input)
                val negex = c match
                  case '-' =>
                    if (!hasSome(input)) { 
                      setPos(input)(zero)
                      cache = JastError("need digits after - in number exponent", globalPos(input))
                      done = true
                    }
                    else c = getB(input)
                    true
                  case '+' =>
                    if (!hasSome(input)) { 
                      setPos(input)(zero)
                      cache = JastError("need digits after + in number exponent", globalPos(input))
                      done = true
                    }
                    else c = getB(input)
                    false
                  case _ => false
                var x = (c - '0')
                if (x < 0 || x >= 10) { 
                  cache = JastError("exponent in number must be numeric digits", globalPos(input) - 1)
                  setPos(input)(zero)
                  done = true
                }
                else {
                  while (hasSome(input) && x < 999 && { c = getB(input); c >= '0' && c <= '9' }) x = x*10 + (c - '0')
                  if (x >= 999) {
                    while (hasSome(input) && { c = getB(input); c >= '0' && c <= '9' }) {}
                    if (c < '0' || c > '9') movePos(input)(-1)
                    val str = subString(input)(zero, getPos(input))
                    result = maybeCacheParsedDblImpl(str, toCache)
                    done = true
                  }
                  ex = if (negex) -x else x
                }
              }
            }
            if (!done) {
              val shift = ex - dadp
              if (c < '0' || c > '9') movePos(input)(-1)
              if (
                dadp + dadp < 19 &&
                java.lang.Long.numberOfLeadingZeros(digits) + java.lang.Long.numberOfTrailingZeros(digits) >= 11 &&
                shift >= -22 &&
                shift <= 22
              ) {
                // We can store the digits in a Double and IEEE demands that * and / are exact (and 1e22 is exact)
                // Thus, we can get the exact result with a single multiplication or division!
                result =
                  if (shift == 0) digits.toDouble
                  else if (shift > 0) digits * smallPowersOfTen(shift)
                  else digits / smallPowersOfTen(-shift)
                if (initial == '-') result = -result
                if (toCache) cache = JsonGenericParser.createNum(result, "")
                done = true
              }
              else {
                val str = subString(input)(zero, getPos(input))
                result = maybeCacheParsedDblImpl(str, toCache)
                done = true
              }
            }
          }
        }
      }
    }
    result

  // Returns Int.MaxValue if we successfully parsed a Double array.
  // Returns a non-negative number of how many elements were successfully parsed before and leaves them in a Json.Arr.Dbl
  // (unless there are zero) with the input position ready to parse the next (non-Double) element.
  // Returns -1-n if we left an error in cache (n=# parsed successfully) and should report that instead of trying an Arr.All parse
  protected inline def parseArrDImpl(input: In, initial: Int): Int =
    var c = initial
    var buffer = new Array[Double](6)
    var n = 0
    var result = Int.MaxValue
    while (result == Int.MaxValue && c != ']') {
      if (((c < '0' && (c != '-')) || c > '9') && c != 'n') {
        if (n > 0) cache = new Json.Arr.Dbl(buffer)
        movePos(input)(-1)
        result = n
      }
      else {
        cache = null
        canZeroPos(input)
        val zero = getPos(input) - 1
        val ans = 
          if (c != 'n') parseRawNum(input, c, toCache = false)
          else if (!hasAtLeast(input)(3) || getB(input) != 'u' || getB(input) != 'l' || getB(input) != 'l') {
            cache = JastError("Expected 'null'", globalPos(input) - 1)
            setPos(input)(zero)
            result = -1-n
            Double.NaN
          }
          else Double.NaN
        if (strictNumbers && (cache eq wouldNotFitInDouble) && result == Int.MaxValue) {
          if (n > 0) cache = new Json.Arr.Dbl(buffer)
          setPos(input)(zero)
          result = n
        }
        if (ans.isNaN && (cache ne null) && cache.isInstanceOf[JastError] && result == Int.MaxValue) result = -1
        if (result == Int.MaxValue) {
          if (n >= buffer.length) buffer = java.util.Arrays.copyOf(buffer, 0x7FFFFFFE & ((buffer.length << 1) | 0x2))
          buffer(n) = ans      
          n += 1
          c = whiteless(input)
          if (c == -129) { cache = JastError("Closing ] not found", globalPos(input) - 1); result = -1-n }
          else if (c == ',') {
            c = whiteless(input)
            if (c == -129) { cache = JastError("Expected JSON value after , but found nothing", globalPos(input) - 1); result = -1-n }
            else if (c == ']') { cache = JastError("Expected JSON value after , but found ]", globalPos(input) - 1); result = -1-n }
          }        
        }
      }
    }
    if (result == Int.MaxValue)
      cache = new Json.Arr.Dbl(if (buffer.length != n) java.util.Arrays.copyOf(buffer, n) else buffer)
    result
}
