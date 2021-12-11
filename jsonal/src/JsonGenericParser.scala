// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2015, 2016, 2021 Rex Kerr and Calico Life Sciences, LLC.

package kse.jsonal

import kse.flow._

trait JsonGenericParser[In] {
  final private[jsonal] var options: JsonOptions = JsonOptions.Default
  final protected var cache: Jast = null

  inline protected def decache(): Jast =
    val ans = cache
    cache = null
    ans

  inline protected def decacheAs[A >: Null <: Jast](): A =
    if (cache eq null) null
    else decache().asInstanceOf[A]

  final def setOptions(opts: JsonOptions): this.type =
    options = opts
    this

  final def copyOptions(): JsonOptions = options.copy

  def parse(input: In): Jast

  final def parse(input: In, opts: JsonOptions): Jast =
    setOptions(opts)
    parse(input)

  private[jsonal] inline def getI(in: In): Int
  private[jsonal] inline def hasSome(in: In): Boolean
  private[jsonal] inline def getPos(in: In): Int
  private[jsonal] inline def globalPos(in: In): Long
  private[jsonal] inline def canZeroPos(in: In): Unit
  private[jsonal] inline def setPos(in: In)(pos: Int): Unit
  private[jsonal] inline def movePos(in: In)(delta: Int): Unit
  private[jsonal] inline def hasAtLeast(in: In)(n: Int): Boolean
  private[jsonal] inline def whiteless(in: In): Int

  private[jsonal] def parseNull(input: In): Json.Null.type | JastError
  private[jsonal] def parseBool(input: In): Json.Bool | JastError
  private[jsonal] def parseStr(input: In): Json.Str | JastError
  private[jsonal] def parseRawNum(input: In, initial: Int, toCache: Boolean): Double
  private[jsonal] def parseJastNum(input: In, initial: Int): Json.Num | JastError
  private[jsonal] def parseArrD(input: In, initial: Int): Int
  private[jsonal] def parseArr(input: In): Json.Arr | JastError
  private[jsonal] def parseObj(input: In): Json.Obj | JastError
  private[jsonal] def parseValStartingWith(input: In, initial: Int): Jast
  private[jsonal] def parseVal(input: In): Jast


  protected inline def maybeCacheParsedDblImpl(str: String, toCache: Boolean): Double =
    val dbl = str.toDouble
    if (toCache)
      cache = 
        if (!options.lossy) JsonGenericParser.createNum(dbl, str)
        else if (java.lang.Double.isNaN(dbl) || java.lang.Double.isInfinite(dbl)) Json.Null
        else JsonGenericParser.createNum(dbl, "")
    else if (!options.lossy && !Json.Num.numericStringEquals(str, dbl.toString))
      cache = JsonGenericParser.wouldNotFitInDouble
    dbl

  protected inline def parseArrImpl(input: In): Json.Arr | JastError =
    var c: Int = whiteless(input)
    if (c == -129) JastError("end of input with unclosed array", globalPos(input) - 1)
    else if (c == ']') Json.Arr.All.empty
    else {
      var result: (Json.Arr | JastError) = null
      var contents: Json.Arr.All.Build[Json.Arr.All] = null
      var n = 0
      if (c == '-' || (c >= '0' && c <='9') || c == 'n') {
        parseArrD(input, c) match
          case Int.MaxValue => result = decacheAs[Json.Arr.Dbl]()
          case m if m < 0 =>
            result = JastError("error in array element " + (-m), globalPos(input) - 1, decacheAs[JastError]())
          case 0 =>
            contents = Json.Arr.All.builder
          case m =>  // > 0
            val a = decacheAs[Json.Arr.Dbl]().doubles
            contents = Json.Arr.All.builder
            while (n < m) {
              contents ~ Json.Num(a(n))
              n += 1
            }
        if (result eq null) c = getI(input)  // We need to keep parsing if we get here; success or error will have returned early
      }
      else contents = Json.Arr.All.builder
      while (c != ']' && (result eq null)) {
        n += 1
        val p = globalPos(input)
        parseValStartingWith(input, c) match {
          case js: Json =>
            canZeroPos(input)
            contents ~ js
          case je: JastError => 
            result = JastError("error in array element "+n, p, je)
        }
        if (result eq null) {
          c = whiteless(input)
          if (c == -129) result = JastError("end of input with unclosed array", globalPos(input) - 1)
          else if (c == ',') {
            val comma = globalPos(input)
            c = whiteless(input)
            if (c == -129) result = JastError("end of input with unclosed array", comma)
          }
          else if (c != ']') result = JastError("unexpected character '" + c.toChar + "' in array after index "+n, globalPos(input) - 1)
        }
      }
      if (result eq null) contents ~ Json.Arr.All else result
    }

  protected inline def parseObjImpl(input: In): Json.Obj | JastError =
    var c = whiteless(input)
    if (c == -129) JastError("end of input with unclosed object", globalPos(input) - 1)
    else if (c == '}') Json.Obj.empty
    else {
      var kvs = new Array[AnyRef](6)
      var n = 0

      var result: (Json.Obj | JastError) = null

      while (c != '}' && (result eq null)) {
        if (n >= kvs.length-1) kvs = java.util.Arrays.copyOf(kvs, 0x7FFFFFFC & ((kvs.length << 1) | 0x4))
        if (c != '"') result = JastError("object keys must be strings", globalPos(input) - 1)
        else {
          val p = globalPos(input)
          parseStr(input) match
            case js: Json.Str =>
              canZeroPos(input)
              kvs(n) = js.text
            case je: JastError =>
              result = JastError("error reading key "+(n/2+1)+" in object", p, je)
          if (result eq null) {
            n += 1
            c = whiteless(input)
            if (c == -129) result = JastError("end of input after object key", globalPos(input) - 1)
            else if (c != ':') result = JastError(f"object's key ${kvs(n-1)} not followed with ':'", globalPos(input) - 1)
            else {
              c = whiteless(input)
              if (c == -129) result = JastError("end of input after object key", globalPos(input) - 1)
              else {
                val q = globalPos(input)
                parseValStartingWith(input, c) match
                  case js: Json =>
                    canZeroPos(input)
                    kvs(n) = js
                  case je: JastError =>
                    result = JastError("error reading value "+(n/2+1)+" (key " + kvs(n-1) + ") in object", q, je)
                if (result eq null) {
                  c = whiteless(input)
                  if (c != '}' && c != ',') {
                    val why = if (c == -129) "unclosed object" else "unexpected character '" + c.toChar + "' in object"
                    result = JastError(why + " after entry " + (n/2+1) + "(key " + kvs(n-1) + ")", globalPos(input) - 1)
                  }
                  if (c == ',') c = whiteless(input)
                  if (c == -129) result = JastError("unclosed object after entry " + (n/2+1) + "(key " + kvs(n-1) + ")", globalPos(input) - 1)
                  n += 1
                }
              }
            }
          }
        }
      }
      if (result ne null) result else Json.Obj.fromFlatArray(if (kvs.length == n) kvs else java.util.Arrays.copyOf(kvs, n))
    }
}
object JsonGenericParser {
  private[jsonal] val smallPowersOfTen = Array.tabulate(23)(i => s"1e$i".toDouble)

  private[jsonal] val yesNull = Yes(kse.jsonal.Json.Null)
  private[jsonal] val yesTrue = Yes(kse.jsonal.Json.Bool.True)
  private[jsonal] val yesFalse = Yes(kse.jsonal.Json.Bool.False)

  private[jsonal] val wouldNotFitInDouble = JastError("Text number would not fit in a Double")

  /** Hack to check if c+8 is whitespace; normal usage is isWhite(c-8) to check character c */
  private[jsonal] inline def isWhite(cm8: Int): Boolean = (cm8 & 0xFFFFFFE0) == 0 && ((1 << cm8) & 0x1000026) != 0

  private[jsonal] def hexify(c: Byte): Int =
    val x = (c - '0') & 0xFF
    if (x < 10) x
    else {
      val y = x | 0x20
      if (y >= 49 && y <= 54) y - 39
      else -1
    }

  private[jsonal] def hexify(c: Char): Int =
    val x = (c - '0') & 0xFFFF
    if (x < 10) x
    else {
      val y = x | 0x20
      if (y >= 49 && y <= 54) y - 39
      else -1
    }

  private[jsonal] def createNum(dbl: Double, str: String): Json.Num = new Json.Num(dbl, str)

  trait Slicing[In] extends JsonGenericParser[In] {
    private[jsonal] inline def initSlice(input: In, i0: Int, iN: Int): this.type

    def parse(input: In, i0: Int, iN: Int): Jast
  }
}

trait JsonGenericParserCompanion[In, Par >: Null <: JsonGenericParser[In]] {
  protected inline def withParser[A >: JastError](input: In)(inline init: Par => Par)(inline f: Par => A): A

  def newParser: Par

  def parse(input: In, options: JsonOptions = JsonOptions.Default): Jast

  def parseJson(input: In, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json | JastError
  def parseNull(input: In, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Null.type | JastError
  def parseBool(input: In, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Bool | JastError
  def parseStr(input: In, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Str | JastError
  def parseNum(input: In, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Num | JastError
  def parseArr(input: In, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Arr | JastError
  def parseObj(input: In, options: JsonOptions = JsonOptions.Default): kse.jsonal.Json.Obj | JastError

  final def Json(input: In, options: JsonOptions = JsonOptions.Default): Jast.To[kse.jsonal.Json] = parseJson(input, options) match
    case je: JastError => No(je)
    case jx: kse.jsonal.Json => Yes(jx)

  final def Null(input: In, options: JsonOptions = JsonOptions.Default): Jast.To[kse.jsonal.Json.Null.type] = parseNull(input, options) match
    case je: JastError => No(je)
    case _ => JsonGenericParser.yesNull

  final def Bool(input: In, options: JsonOptions = JsonOptions.Default): Jast.To[kse.jsonal.Json.Bool] = parseBool(input, options) match
    case je: JastError => No(je)
    case jx: kse.jsonal.Json.Bool => if (jx.value) JsonGenericParser.yesTrue else JsonGenericParser.yesFalse

  final def Str(input: In, options: JsonOptions = JsonOptions.Default): Jast.To[kse.jsonal.Json.Str] = parseStr(input, options) match
    case je: JastError => No(je)
    case jx: kse.jsonal.Json.Str => Yes(jx)

  final def Num(input: In, options: JsonOptions = JsonOptions.Default): Jast.To[kse.jsonal.Json.Num] = parseNum(input, options) match
    case je: JastError => No(je)
    case jx: kse.jsonal.Json.Num => Yes(jx)

  final def Arr(input: In, options: JsonOptions = JsonOptions.Default): Jast.To[kse.jsonal.Json.Arr] = parseArr(input, options) match
    case je: JastError => No(je)
    case jx: kse.jsonal.Json.Arr => Yes(jx)

  final def Obj(input: In, options: JsonOptions = JsonOptions.Default): Jast.To[kse.jsonal.Json.Obj] = parseObj(input, options) match
    case je: JastError => No(je)
    case jx: kse.jsonal.Json.Obj => Yes(jx)


  protected inline def parseImpl(input: In, opts: JsonOptions): Jast =
    withParser(input)(_ setOptions opts)(_.parseVal(input))

  protected inline def parseJsonImpl(input: In, opts: JsonOptions): kse.jsonal.Json | JastError =
    withParser(input)(_ setOptions opts)(_.parseVal(input)) match
      case je: JastError => je
      case j: kse.jsonal.Json => j

  protected inline def parseBoolImpl(input: In, opts: JsonOptions): kse.jsonal.Json.Bool | JastError =
    withParser(input)(_ setOptions opts)(_.parseBool(input))
}
object JsonGenericParserCompanion {
  trait Positional[In, Par >: Null <: JsonGenericParser[In]] extends JsonGenericParserCompanion[In, Par] {
    import JsonGenericParser._

    protected inline def hasAtLeast(in: In)(n: Int): Boolean
    protected inline def getPos(in: In): Int
    protected inline def setPos(in: In)(pos: Int): Unit
    protected inline def backOne(in: In): Unit
    protected inline def getC(in: In): Char
    protected inline def whiteless(in: In): Int

    protected inline def withParser[A >: JastError](input: In)(inline init: Par => Par)(inline f: Par => A): A = f(init(newParser))

    protected inline def optionalize[A >: JastError](in: In, opts: JsonOptions)(inline f: => A): A =
      val start = getPos(in)
      if (opts.trim) { if (whiteless(in) != -129) backOne(in) }
      val ans = f
      val err = ans.isInstanceOf[JastError]
      if (opts.trim && !err) { if (whiteless(in) != -129) backOne(in) }
      val full = !hasAtLeast(in)(1)
      opts.outcome match
        case Some(o) => o.complete = full; o.error = err; o.consumed += getPos(in) - start
        case _ =>
      if (full || !opts.complete) ans else JastError("JSON parse covered only part of input", getPos(in))

    protected inline def parseNullImpl(input: In, opts: JsonOptions): kse.jsonal.Json.Null.type | JastError = optionalize(input, opts) {
      if (!hasAtLeast(input)(4)) JastError("Expected JSON null but not enough input", getPos(input))
      else {
        if (getC(input) != 'n' || getC(input) != 'u' || getC(input) != 'l' || getC(input) != 'l')
          JastError("Expected JSON null but did not find literal text 'null'", getPos(input))
        else kse.jsonal.Json.Null
      }
    }

    protected inline def parseStrImpl(input: In, opts: JsonOptions): kse.jsonal.Json.Str | JastError = optionalize(input, opts) {
      if (!hasAtLeast(input)(2)) JastError("Expected JSON string but at end of input")
      else if (getC(input) != '"') JastError("Expected JSON string but did not find '\"'", getPos(input))
      else newParser.setOptions(opts).parseStr(input)
    }

    protected inline def parseNumImpl(input: In, opts: JsonOptions): kse.jsonal.Json.Num | JastError = optionalize(input, opts) {
      if (!hasAtLeast(input)(1)) JastError("Expected JSON number but at end of input")
      else {
        val c = getC(input)
        if (c != '-' && (c < '0' || c > '9')) {
          backOne(input)
          JastError("Expected JSON number but found character "+c, getPos(input))
        }
        else newParser.setOptions(opts).parseJastNum(input, c)
      }
    }

    protected inline def parseArrImpl(input: In, opts: JsonOptions): kse.jsonal.Json.Arr | JastError = optionalize(input, opts) {
      if (!hasAtLeast(input)(1)) JastError("Expected JSON array but at end of input")
      else {
        val c = getC(input)
        if (c != '[') {
          backOne(input)
          JastError("Expected JSON array but found character "+c, getPos(input))
        }
        else newParser.setOptions(opts).parseArr(input)
      }
    }
    
    protected inline def parseObjImpl(input: In, opts: JsonOptions): kse.jsonal.Json.Obj | JastError = optionalize(input, opts) {
      if (!hasAtLeast(input)(1)) JastError("Expected JSON object but at end of input")
      else {
        val c = getC(input)
        if (c != '{') {
          backOne(input)
          JastError("Expected JSON object but found character "+c, getPos(input))
        }
        else newParser.setOptions(opts).parseObj(input)
      }
    }
  }

  trait Instanced[In, Par >: Null <: JsonGenericParser[In]] extends JsonGenericParserCompanion[In, Par] {
    import java.util.concurrent.atomic.AtomicReference

    protected val myCachedPar: AtomicReference[Par] = new AtomicReference(null)

    protected inline def withParser[A >: JastError](input: In)(inline init: Par => Par)(inline f: Par => A): A =
      val p = myCachedPar.getAndSet(null)
      val par = init(if (p eq null) newParser else p)
      val start = par.globalPos(input)
      if (par.options.trim) if (par.whiteless(input) != -129) par.movePos(input)(-1)
      var ans: A = f(par)
      val err = ans.isInstanceOf[JastError]
      if (!err && par.options.trim) if (par.whiteless(input) != -129) par.movePos(input)(-1)
      val full = !par.hasSome(input)
      par.options.outcome match
        case Some(o) => o.complete = full; o.error = err; o.consumed += par.globalPos(input) - start
        case _ =>
      if (!full && par.options.complete) ans = JastError("JSON parse covered only part of input", par.globalPos(input))
      myCachedPar.compareAndSet(null, par)
      ans

    protected inline def parseNullImpl(input: In, opts: JsonOptions): kse.jsonal.Json.Null.type | JastError =
      withParser(input)(_ setOptions opts){ par =>
        par.getI(input) match
          case 'n' => par.parseNull(input)
          case -129 => JastError("Expected JSON null but not enough input", par.globalPos(input))
          case _ => JastError("Expected JSON null but did not start with n", par.globalPos(input))
      }

    protected inline def parseStrImpl(input: In, opts: JsonOptions): kse.jsonal.Json.Str | JastError = 
      withParser(input)(_ setOptions opts){ par =>
        par.getI(input) match
          case '"' => par.parseStr(input)
          case -129 => JastError("Expected JSON string but not enough input", par.globalPos(input))
          case _ => JastError("Expected JSON string but did not start with \"", par.globalPos(input))
      }

    protected inline def parseNumImpl(input: In, opts: JsonOptions):kse.jsonal.Json.Num | JastError = 
      withParser(input)(_ setOptions opts){ par =>
        par.getI(input) match
          case c if (c >= '0' && c <= '9' || c == '-') => par.parseJastNum(input, c)
          case -129 => JastError("Expected JSON number but not enough input", par.globalPos(input))
          case _ => JastError("Expected JSON number but did not start with - or digit", par.globalPos(input))
      }


    protected inline def parseArrImpl(input: In, opts: JsonOptions): kse.jsonal.Json.Arr | JastError = 
      withParser(input)(_ setOptions opts){ par =>
        par.getI(input) match
          case '[' => par.parseArr(input)
          case -129 => JastError("Expected JSON array but not enough input", par.globalPos(input))
          case _ => JastError("Expected JSON array but did not start with [", par.globalPos(input))
      }

    protected inline def parseObjImpl(input: In, opts: JsonOptions): kse.jsonal.Json.Obj | JastError =
      withParser(input)(_ setOptions opts){ par =>
        par.getI(input) match
          case '{' => par.parseObj(input)
          case -129 => JastError("Expected JSON object but not enough input", par.globalPos(input))
          case _ => JastError("Expected JSON object but did not start with {", par.globalPos(input))
      }
  }

  trait Sliced[In, Par >: Null <: JsonGenericParser.Slicing[In]] extends Instanced[In, Par] {
    final def parse(input: In, i0: Int, iN: Int): Jast = parse(input, i0, iN, JsonOptions.Default)
    def parse(input: In, i0: Int, iN: Int, options: JsonOptions): Jast

    def parseJson(input: In, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json           | JastError
    def parseNull(input: In, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Null.type | JastError
    def parseBool(input: In, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Bool      | JastError
    def parseStr( input: In, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Str       | JastError
    def parseNum( input: In, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Num       | JastError
    def parseArr( input: In, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Arr       | JastError
    def parseObj( input: In, i0: Int, iN: Int, options: JsonOptions): kse.jsonal.Json.Obj       | JastError

    final def parseJson(input: In, i0: Int, iN: Int): kse.jsonal.Json           | JastError = parseJson(input, i0, iN, JsonOptions.Default)
    final def parseNull(input: In, i0: Int, iN: Int): kse.jsonal.Json.Null.type | JastError = parseNull(input, i0, iN, JsonOptions.Default)
    final def parseBool(input: In, i0: Int, iN: Int): kse.jsonal.Json.Bool      | JastError = parseBool(input, i0, iN, JsonOptions.Default)
    final def parseStr( input: In, i0: Int, iN: Int): kse.jsonal.Json.Str       | JastError = parseStr( input, i0, iN, JsonOptions.Default)
    final def parseNum( input: In, i0: Int, iN: Int): kse.jsonal.Json.Num       | JastError = parseNum( input, i0, iN, JsonOptions.Default)
    final def parseArr( input: In, i0: Int, iN: Int): kse.jsonal.Json.Arr       | JastError = parseArr( input, i0, iN, JsonOptions.Default)
    final def parseObj( input: In, i0: Int, iN: Int): kse.jsonal.Json.Obj       | JastError = parseObj( input, i0, iN, JsonOptions.Default)

    final def Json(input: In, i0: Int, iN: Int): Jast.To[kse.jsonal.Json]           = Json(input, i0, iN, JsonOptions.Default)
    final def Null(input: In, i0: Int, iN: Int): Jast.To[kse.jsonal.Json.Null.type] = Null(input, i0, iN, JsonOptions.Default)
    final def Bool(input: In, i0: Int, iN: Int): Jast.To[kse.jsonal.Json.Bool]      = Bool(input, i0, iN, JsonOptions.Default)
    final def Str( input: In, i0: Int, iN: Int): Jast.To[kse.jsonal.Json.Str]       = Str( input, i0, iN, JsonOptions.Default)
    final def Num( input: In, i0: Int, iN: Int): Jast.To[kse.jsonal.Json.Num]       = Num( input, i0, iN, JsonOptions.Default)
    final def Arr( input: In, i0: Int, iN: Int): Jast.To[kse.jsonal.Json.Arr]       = Arr( input, i0, iN, JsonOptions.Default)
    final def Obj( input: In, i0: Int, iN: Int): Jast.To[kse.jsonal.Json.Obj]       = Obj( input, i0, iN, JsonOptions.Default)
    
    final def Json(input: In, i0: Int, iN: Int, options: JsonOptions): Jast.To[kse.jsonal.Json] = parse(input, i0, iN, options) match
      case je: JastError => No(je)
      case jx: kse.jsonal.Json => Yes(jx) 
    
    final def Null(input: In, i0: Int, iN: Int, options: JsonOptions): Jast.To[kse.jsonal.Json.Null.type] = parseNull(input, i0, iN, options) match
      case je: JastError => No(je)
      case _ => JsonGenericParser.yesNull
    
    final def Bool(input: In, i0: Int, iN: Int, options: JsonOptions): Jast.To[kse.jsonal.Json.Bool] = parseBool(input, i0, iN, options) match
      case je: JastError => No(je)
      case jx: kse.jsonal.Json.Bool => if (jx.value) JsonGenericParser.yesTrue else JsonGenericParser.yesFalse
    
    final def Str(input: In, i0: Int, iN: Int, options: JsonOptions): Jast.To[kse.jsonal.Json.Str] = parseStr(input, i0, iN, options) match
      case je: JastError => No(je)
      case jx: kse.jsonal.Json.Str => Yes(jx) 
    
    final def Num(input: In, i0: Int, iN: Int, options: JsonOptions): Jast.To[kse.jsonal.Json.Num] = parseNum(input, i0, iN, options) match
      case je: JastError => No(je)
      case jx: kse.jsonal.Json.Num => Yes(jx) 
    
    final def Arr(input: In, i0: Int, iN: Int, options: JsonOptions): Jast.To[kse.jsonal.Json.Arr] = parseArr(input, i0, iN, options) match
      case je: JastError => No(je)
      case jx: kse.jsonal.Json.Arr => Yes(jx) 
    
    final def Obj(input: In, i0: Int, iN: Int, options: JsonOptions): Jast.To[kse.jsonal.Json.Obj] = parseObj(input, i0, iN, options) match
      case je: JastError => No(je)
      case jx: kse.jsonal.Json.Obj => Yes(jx) 
    

    protected inline def parseSliceImpl(input: In, i0: Int, iN: Int, opts: JsonOptions): Jast =
      withParser(input)(_.initSlice(input, i0, iN).setOptions(opts))(_.parseVal(input))

    protected inline def parseJsonSliceImpl(input: In, i0: Int, iN: Int, opts: JsonOptions): kse.jsonal.Json | JastError =
      withParser(input)(_.initSlice(input, i0, iN).setOptions(opts)){ par =>
        par.parseVal(input) match
          case je: JastError => je
          case j: kse.jsonal.Json => j
      }

    protected inline def parseNullSliceImpl(input: In, i0: Int, iN: Int, opts: JsonOptions): kse.jsonal.Json.Null.type | JastError =
      withParser(input)(_.initSlice(input, i0, iN).setOptions(opts)){ par =>
        par.getI(input) match
          case 'n'  => par.parseNull(input)
          case -129 => JastError("Expected JSON null but not enough input", par.globalPos(input))
          case _    => JastError("Expected JSON null but did not start with n", par.globalPos(input))
      }

    protected inline def parseBoolSliceImpl(input: In, i0: Int, iN: Int, opts: JsonOptions): kse.jsonal.Json.Bool | JastError =
      withParser(input)(_.initSlice(input, i0, iN).setOptions(opts))(_.parseBool(input))

    protected inline def parseStrSliceImpl(input: In, i0: Int, iN: Int, opts: JsonOptions): kse.jsonal.Json.Str | JastError = 
      withParser(input)(_.initSlice(input, i0, iN).setOptions(opts)){ par =>
        par.getI(input) match
          case '"'  => par.parseStr(input)
          case -129 => JastError("Expected JSON string but not enough input", par.globalPos(input))
          case _    => JastError("Expected JSON string but did not start with \"", par.globalPos(input))
      }

    protected inline def parseNumSliceImpl(input: In, i0: Int, iN: Int, opts: JsonOptions): kse.jsonal.Json.Num | JastError =
      withParser(input)(_.initSlice(input, i0, iN).setOptions(opts)){ par =>
        par.getI(input) match
          case c if (c >= '0' && c <= '9' || c == '-') => par.parseJastNum(input, c)
          case -129 => JastError("Expected JSON number but not enough input", par.globalPos(input))
          case _    => JastError("Expected JSON number but did not start with - or digit", par.globalPos(input))
      }

    protected inline def parseArrSliceImpl(input: In, i0: Int, iN: Int, opts: JsonOptions): kse.jsonal.Json.Arr | JastError = 
      withParser(input)(_.initSlice(input, i0, iN).setOptions(opts)){ par =>
        par.getI(input) match
          case '['  => par.parseArr(input)
          case -129 => JastError("Expected JSON array but not enough input", par.globalPos(input))
          case _    => JastError("Expected JSON array but did not start with [", par.globalPos(input))
      }

    protected inline def parseObjSliceImpl(input: In, i0: Int, iN: Int, opts: JsonOptions): kse.jsonal.Json.Obj | JastError = 
      withParser(input)(_.initSlice(input, i0, iN).setOptions(opts)){ par =>
        par.getI(input) match
          case '{'  => par.parseObj(input)
          case -129 => JastError("Expected JSON object but not enough input", par.globalPos(input))
          case _    => JastError("Expected JSON object but did not start with {", par.globalPos(input))
      }
  }
}
