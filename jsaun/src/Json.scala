// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


import scala.util.boundary
import scala.util.boundary.Label

import kse.basics.{given, _}
import kse.flow.{given, _}


///////////////////////////////
/// Errors and the JAny type ///
///////////////////////////////

/** A data-bearing JSON parse failure: what went wrong, where the input broke, and what the
  * input looked like there.  Rendered with a caret-marked excerpt, e.g.
  * {{{
  * expected ',' or ']' in array, found 'x' (line 1, char 9)
  *   [1, 2, 3x]
  *           ^
  * }}}
  * Structural context ("in element 2 of array started at line 1, char 5:") is layered on top
  * with the standard `ErrType.Explained` chain, so multi-level parse errors unroll level by
  * level when printed.
  */
final class Jerr private[jsaun] (
  val description: String,
  val pos: Int,
  val line: Int,
  val col: Int,
  val excerpt: String,
  val excerptOffset: Int
) extends ErrType {
  type E = String
  def error: E = description

  /** The one-line summary, without the input excerpt. */
  def message: String =
    if pos < 0 then description
    else s"$description (line $line, char $col)"

  override lazy val toString: String =
    if excerpt.isEmpty then message
    else message + "\n  " + excerpt + "\n  " + (" " * excerptOffset) + "^"

  def buildLines(sb: MkStr, prefix: String): Unit = ErrType.buildLinesFromString(sb, toString, prefix)

  def toThrowable: Throwable = ErrType.StringErrException(toString)
}


/** A JSON value or an error: the standard currency of jsaun.  Because the favored branch of
  * `Or` is unboxed for reference types, a `JAny` at runtime is either a plain `Json` or an
  * `Alt[Err]`--using it costs one type test and no allocation.
  *
  * Accessors mirror those on `Json` and let errors flow through unchanged, so a whole access
  * chain can be written without checking intermediate results:
  * {{{
  * Json.parse(text)("stations")(3)("id").str    // Ask[String], even if text didn't parse
  * }}}
  * Parse errors and access errors share this one channel; whichever failure happened first is
  * the one that comes out the end.
  */
opaque type JAny = Ask[Json]
object JAny {
  inline def apply(j: Json): JAny = Is(j)
  inline def err(e: Err): JAny = Alt(e)
  inline def wrap(a: Ask[Json]): JAny = a

  extension (ja: JAny)
    /** This value as a plain `Ask[Json]` (a no-op: they are the same thing). */
    inline def ask: Ask[Json] = ja

    /** True if this holds an error rather than a JSON value. */
    inline def isErr: Boolean = ja.isInstanceOf[Alt[?]]

    /** The JSON value, or a jump to the enclosing boundary with the error. */
    inline def json[E >: Alt[Err]](using Label[E]): Json = (ja: Ask[Json]).?

    /** The JSON value, or `alt` if this is an error. */
    def jsonOr(alt: Json): Json = (ja: Any) match
      case _: Alt[?] => alt
      case j => j.asInstanceOf[Json]

    /** The value at `key`, or an error if this is an error, not an object, or lacks the key. */
    def apply(key: String): JAny = (ja: Any) match
      case _: Alt[?] => ja
      case j => j.asInstanceOf[Json].apply(key)

    /** The element at `i`, or an error if this is an error, not an array, or out of bounds. */
    def apply(i: Int): JAny = (ja: Any) match
      case _: Alt[?] => ja
      case j => j.asInstanceOf[Json].apply(i)

    def str: Ask[String] = (ja: Any) match
      case _: Alt[?] => ja.asInstanceOf[Ask[String]]
      case j => j.asInstanceOf[Json].str

    def bool: Ask[Boolean] = (ja: Any) match
      case _: Alt[?] => ja.asInstanceOf[Ask[Boolean]]
      case j => j.asInstanceOf[Json].bool

    def long: Ask[Long] = (ja: Any) match
      case _: Alt[?] => ja.asInstanceOf[Ask[Long]]
      case j => j.asInstanceOf[Json].long

    def dbl: Ask[Double] = (ja: Any) match
      case _: Alt[?] => ja.asInstanceOf[Ask[Double]]
      case j => j.asInstanceOf[Json].dbl

    def arr: Ask[Jarr] = (ja: Any) match
      case _: Alt[?] => ja.asInstanceOf[Ask[Jarr]]
      case j => j.asInstanceOf[Json].arr

    def obj: Ask[Jobj] = (ja: Any) match
      case _: Alt[?] => ja.asInstanceOf[Ask[Jobj]]
      case j => j.asInstanceOf[Json].obj

    def strOr(alt: String): String = (ja: Any) match
      case _: Alt[?] => alt
      case j => j.asInstanceOf[Json].strOr(alt)

    def boolOr(alt: Boolean): Boolean = (ja: Any) match
      case _: Alt[?] => alt
      case j => j.asInstanceOf[Json].boolOr(alt)

    def longOr(alt: Long): Long = (ja: Any) match
      case _: Alt[?] => alt
      case j => j.asInstanceOf[Json].longOr(alt)

    def dblOr(alt: Double): Double = (ja: Any) match
      case _: Alt[?] => alt
      case j => j.asInstanceOf[Json].dblOr(alt)

    def isNull: Boolean = (ja: Any) match
      case _: Alt[?] => false
      case j => j.asInstanceOf[Json].isNull

    /** Number of elements or keys, 0 for simple values, -1 for an error. */
    def size: Int = (ja: Any) match
      case _: Alt[?] => -1
      case j => j.asInstanceOf[Json].size
}


///////////////
/// The AST ///
///////////////

/** An immutable JSON value: `Jnull`, `Jbool`, `Jnum`, `Jstr`, `Jarr`, or `Jobj`.
  *
  * Structural accessors (`apply`, `str`, `long`, ...) never throw: mismatches come back as
  * errors in `JAny`/`Ask`, so access chains compose and the first failure wins.  `print` (and
  * `toString`) render compact JSON text.
  */
sealed abstract class Json protected () {
  /** The JSON type of this value: "null", "boolean", "number", "string", "array", or "object". */
  def kind: String

  /** Number of elements (array) or keys (object); 0 for simple values. */
  def size: Int = 0

  /** The value at `key`, or an error if this is not an object or lacks the key. */
  def apply(key: String): JAny = JAny.err(Json.expectErr(s"an object with key \"$key\"", this))

  /** The element at `i`, or an error if this is not an array or `i` is out of bounds. */
  def apply(i: Int): JAny = JAny.err(Json.expectErr(s"an array with element $i", this))

  def str: Ask[String] = Alt(Json.expectErr("a string", this))
  def bool: Ask[Boolean] = Alt(Json.expectErr("a boolean", this))
  def long: Ask[Long] = Alt(Json.expectErr("an integer", this))
  def dbl: Ask[Double] = Alt(Json.expectErr("a number", this))
  def arr: Ask[Jarr] = Alt(Json.expectErr("an array", this))
  def obj: Ask[Jobj] = Alt(Json.expectErr("an object", this))

  def strOr(alt: String): String = alt
  def boolOr(alt: Boolean): Boolean = alt
  def longOr(alt: Long): Long = alt
  def dblOr(alt: Double): Double = alt

  def isNull: Boolean = false

  private[jsaun] def printTo(sb: java.lang.StringBuilder): Unit

  /** Render as compact JSON text (no whitespace). */
  final def print: String =
    val sb = new java.lang.StringBuilder
    printTo(sb)
    sb.toString

  final override def toString = print
}
object Json {
  /** Parse JSON text into a tree, or an `Err` detailing what went wrong and where.
    * The parser is strict (RFC 8259): no trailing commas, no leading zeros, no `NaN`, and
    * nothing but whitespace after the value.
    */
  def parse(in: String): JAny = JAny.wrap(Ask.flat{ (new Jparse.Str(in)).parseTop() })

  private[jsaun] def expectErr(what: String, j: Json): Err = Err(s"expected $what, found ${j.kind}")
}


/** The JSON null value. */
object Jnull extends Json {
  def kind = "null"
  override def isNull = true
  private[jsaun] def printTo(sb: java.lang.StringBuilder): Unit = sb.append("null") __ Unit
}


/** A JSON boolean; the only instances are `Jbool.True` and `Jbool.False`. */
sealed abstract class Jbool protected () extends Json {
  def value: Boolean
  def kind = "boolean"
  override def bool: Ask[Boolean] = Is(value)
  override def boolOr(alt: Boolean): Boolean = value
  private[jsaun] def printTo(sb: java.lang.StringBuilder): Unit = sb.append(if value then "true" else "false") __ Unit
}
object Jbool {
  object True extends Jbool { def value = true }
  object False extends Jbool { def value = false }

  inline def apply(value: Boolean): Jbool = if value then True else False
  def unapply(jb: Jbool): Some[Boolean] = Some(jb.value)
}


/** A JSON string (stored unescaped; escaping happens on print). */
final class Jstr(val text: String) extends Json {
  def kind = "string"
  override def str: Ask[String] = Is(text)
  override def strOr(alt: String): String = text

  override def equals(a: Any): Boolean = a match
    case s: Jstr => text == s.text
    case _ => false
  override def hashCode: Int = text.##

  private[jsaun] def printTo(sb: java.lang.StringBuilder): Unit = Jstr.encodeTo(sb, text)
}
object Jstr {
  def apply(text: String): Jstr = new Jstr(text)
  def unapply(js: Jstr): Some[String] = Some(js.text)

  /** Append `s` as a quoted JSON string, escaping only what JSON requires. */
  private[jsaun] def encodeTo(sb: java.lang.StringBuilder, s: String): Unit =
    sb.append('"') __ Unit
    var i = 0
    while i < s.length do
      val c = s.charAt(i)
      if c == '"' then sb.append("\\\"") __ Unit
      else if c == '\\' then sb.append("\\\\") __ Unit
      else if c >= ' ' then sb.append(c) __ Unit
      else c match
        case '\b' => sb.append("\\b") __ Unit
        case '\t' => sb.append("\\t") __ Unit
        case '\n' => sb.append("\\n") __ Unit
        case '\f' => sb.append("\\f") __ Unit
        case '\r' => sb.append("\\r") __ Unit
        case _ =>
          sb.append("\\u00") __ Unit
          sb.append("0123456789abcdef".charAt((c >> 4) & 0xF)) __ Unit
          sb.append("0123456789abcdef".charAt(c & 0xF)) __ Unit
      i += 1
    sb.append('"') __ Unit
}


/** A JSON number, stored as whichever of `Jnum.L` (Long) or `Jnum.D` (Double) the parser
  * could use faithfully.  Longs and Doubles that denote the same value compare equal (and
  * hash alike), so `Jnum(3) == Jnum(3.0)`.
  */
sealed abstract class Jnum protected () extends Json {
  def kind = "number"

  /** This number as a Double (lossy for Longs beyond 2^53). */
  def double: Double

  /** True if this number has no fractional part. */
  def isWhole: Boolean

  override def dbl: Ask[Double] = Is(double)
  override def dblOr(alt: Double): Double = double
}
object Jnum {
  def apply(value: Long): Jnum = new L(value)
  def apply(value: Double): Jnum = new D(value)

  /** A JSON integer that fits in a Long. */
  final class L(val value: Long) extends Jnum {
    def double = value.toDouble
    def isWhole = true
    override def long: Ask[Long] = Is(value)
    override def longOr(alt: Long): Long = value

    override def equals(a: Any): Boolean = a match
      case l: L => value == l.value
      case d: D => d.value == value.toDouble && d.value.toLong == value
      case _ => false
    override def hashCode: Int = value.##   // scala.## makes this agree with D when the values are equal

    private[jsaun] def printTo(sb: java.lang.StringBuilder): Unit = sb.append(value) __ Unit
  }

  /** A JSON number held as a Double; NaN and infinities (never produced by parsing) print as null. */
  final class D(val value: Double) extends Jnum {
    def double = value
    def isWhole = Math.rint(value) == value && !value.isInfinite

    override def long: Ask[Long] =
      // Long.MinValue.toDouble is exactly -2^63 but Long.MaxValue.toDouble rounds up to 2^63,
      // so the interval is closed below and open above
      if isWhole && value >= -9.223372036854776E18 && value < 9.223372036854776E18 then Is(value.toLong)
      else Alt(Err(s"number is not an integer Long can hold: ${this.print}"))
    override def longOr(alt: Long): Long =
      if isWhole && value >= -9.223372036854776E18 && value < 9.223372036854776E18 then value.toLong
      else alt

    override def equals(a: Any): Boolean = a match
      case d: D => value == d.value
      case l: L => value == l.value.toDouble && value.toLong == l.value
      case _ => false
    override def hashCode: Int = value.##

    private[jsaun] def printTo(sb: java.lang.StringBuilder): Unit =
      if value.isNaN || value.isInfinite then sb.append("null") __ Unit
      else sb.append(value) __ Unit
  }
}


/** A JSON array.  `Jarr.A` holds arbitrary values; packed numeric backings (e.g. all-Double)
  * come later as further companion subclasses.
  */
sealed abstract class Jarr protected () extends Json {
  def kind = "array"
  override def arr: Ask[Jarr] = Is(this)
  def foreach(f: Json => Unit): Unit
}
object Jarr {
  def apply(values: Json*): Jarr =
    val a = new Array[Json](values.length)
    values.copyToArray(a) __ Unit
    new A(a, a.length)

  private[jsaun] val empty: A = new A(new Array[Json](0), 0)

  /** A general array of JSON values.  Instances reachable as `Jarr.A` are immutable; editing
    * happens only through the mutable subclass (to come), which shares this representation.
    */
  sealed class A private[jsaun] (private[jsaun] var vs: Array[Json], private[jsaun] var n: Int) extends Jarr {
    final override def size: Int = n

    final override def apply(i: Int): JAny =
      if i >= 0 && i < n then JAny(vs(i))
      else JAny.err(Err(s"index $i out of bounds for array of size $n"))

    final def foreach(f: Json => Unit): Unit =
      var k = 0
      while k < n do
        f(vs(k))
        k += 1

    final override def equals(a: Any): Boolean = a match
      case x: A =>
        if n != x.n then false
        else
          var k = 0
          while k < n && vs(k) == x.vs(k) do k += 1
          k == n
      case _ => false

    final override def hashCode: Int =
      var h = 1
      var k = 0
      while k < n do
        h = h * 31 + vs(k).##
        k += 1
      h

    private[jsaun] def printTo(sb: java.lang.StringBuilder): Unit =
      sb.append('[') __ Unit
      var k = 0
      while k < n do
        if k > 0 then sb.append(',') __ Unit
        vs(k).printTo(sb)
        k += 1
      sb.append(']') __ Unit
  }
}


/** A JSON object: insertion-ordered keys with values, duplicates retained.  Lookup answers
  * the last occurrence of a duplicated key (as `JSON.parse` does); a hash index is built
  * lazily once the object is large enough for linear scans to hurt.  Instances reachable as
  * `Jobj` are immutable; editing happens only through the mutable subclass (to come).
  */
sealed class Jobj private[jsaun] (
  private[jsaun] var ks: Array[String],
  private[jsaun] var vs: Array[Json],
  private[jsaun] var n: Int
) extends Json {
  def kind = "object"
  final override def size: Int = n
  final override def obj: Ask[Jobj] = Is(this)

  // Built at most once per content; harmless to rebuild on a race (single-threaded use expected)
  private var index: java.util.HashMap[String, Json] | Null = null

  private def indexed: java.util.HashMap[String, Json] = index match
    case null =>
      val m = new java.util.HashMap[String, Json]
      var k = 0
      while k < n do
        m.put(ks(k), vs(k)) __ Unit   // forward fill: later duplicates overwrite, so last wins
        k += 1
      index = m
      m
    case m => m

  /** The value at `key` (the last one, if duplicated), or `null` if absent. */
  final def get(key: String): Json | Null =
    if n < 8 then
      var k = n - 1
      while k >= 0 && ks(k) != key do k -= 1
      if k >= 0 then vs(k) else null
    else indexed.get(key)

  final override def apply(key: String): JAny =
    val j = get(key)
    if j eq null then JAny.err(Err(s"no key \"$key\" in object"))
    else JAny(j)

  final def contains(key: String): Boolean = get(key) ne null

  final def foreach(f: (String, Json) => Unit): Unit =
    var k = 0
    while k < n do
      f(ks(k), vs(k))
      k += 1

  // Order-insensitive multiset equality (duplicate keys must pair up); O(n^2) worst case,
  // which equality-of-objects use doesn't care about
  final override def equals(a: Any): Boolean = a match
    case o: Jobj =>
      (this eq o) || (n == o.n && {
        if n == 0 then true
        else
          val used = new Array[Boolean](n)
          var good = true
          var i = 0
          while good && i < n do
            var j = 0
            var found = false
            while !found && j < n do
              if !used(j) && ks(i) == o.ks(j) && vs(i) == o.vs(j) then
                used(j) = true
                found = true
              j += 1
            good = found
            i += 1
          good
      })
    case _ => false

  final override def hashCode: Int =
    var h = 0
    var k = 0
    while k < n do
      h += ks(k).## ^ (vs(k).## * 31)   // commutative, to match order-insensitive equality
      k += 1
    h ^ n

  private[jsaun] def printTo(sb: java.lang.StringBuilder): Unit =
    sb.append('{') __ Unit
    var k = 0
    while k < n do
      if k > 0 then sb.append(',') __ Unit
      Jstr.encodeTo(sb, ks(k))
      sb.append(':') __ Unit
      vs(k).printTo(sb)
      k += 1
    sb.append('}') __ Unit
}
object Jobj {
  def apply(kvs: (String, Json)*): Jobj =
    val ks = new Array[String](kvs.length)
    val vs = new Array[Json](kvs.length)
    var k = 0
    for (key, v) <- kvs do
      ks(k) = key
      vs(k) = v
      k += 1
    new Jobj(ks, vs, k)

  private[jsaun] val empty: Jobj = new Jobj(new Array[String](0), new Array[Json](0), 0)
}
