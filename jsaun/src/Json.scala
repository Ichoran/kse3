// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


import scala.annotation.{publicInBinary, targetName}
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
  *
  * `pos` is the offset into the input (a byte offset for byte-fed sources); for line-fed
  * sources it instead packs (line index, offset within line) into its halves.  `line`/`col`
  * are 1-based and always plain.
  */
final class Jerr private[jsaun] (
  val description: String,
  val pos: Long,
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

  /** Append this value as compact JSON to `out`. */
  def printTo(out: Jout): Unit

  /** Render as JSON text: preserved formatting verbatim where it exists, `st` (default
    * compact) for everything else, including the numeric policy for fresh Doubles.
    */
  final def print(using st: Jstyle): String =
    val out = new Jout.Str(style = st)
    printTo(out)
    out.result

  /** Render as JSON in UTF-8 bytes; same rules as `print`. */
  final def printBytes(using st: Jstyle): Array[Byte] =
    val out = new Jout.Bytes(style = st)
    printTo(out)
    out.result

  /** Render entirely in `st`, ignoring all preserved formatting (the restyling escape hatch). */
  final def reprint(st: Jstyle): String =
    val out = new Jout.Str(style = st)
    out.ignoreFmt = true
    printTo(out)
    out.result

  /** Trade byte-exact preserved formatting for each collection's compact inferred style,
    * releasing the retained source text for garbage collection.  Values are untouched, and
    * uniformly formatted documents still print the same; only irregular spacing normalizes.
    * The format sidecars are updated in place (they never participate in equality).
    */
  final def compactFormat(): this.type =
    this match
      case a: Jarr.A =>
        val f = a.fmt
        if f ne null then
          a.sty = Jfmt.Local.ofArr(f, a.n)
          a.fmt = null
        var k = 0
        while k < a.n do
          a.vs(k).compactFormat() __ Unit
          k += 1
      case d: Jarr.D =>
        val f = d.fmt
        if f ne null then
          d.sty = Jfmt.Local.ofArr(f, d.n)
          d.fmt = null
      case o: Jobj =>
        val f = o.fmt
        if f ne null then
          o.sty = Jfmt.Local.ofObj(f, o.n)
          o.fmt = null
        var k = 0
        while k < o.n do
          o.vs(k).compactFormat() __ Unit
          k += 1
      case _ => ()
    this

  final override def toString = print(using Jstyle.compact)
}
object Json {
  /** The in-memory text sources jsaun parses directly: a `String`, UTF-8 `Array[Byte]`, or
    * UTF-16 `Array[Char]`.  (`Mem[Byte]`/`Mem[Char]` are separate overloads -- they erase to the
    * same type, so they cannot join a union that is dispatched by a runtime match.)
    */
  type Source = String | Array[Byte] | Array[Char]

  /** Streaming input, parsed once through a sliding window: an `InputStream` or UTF-8
    * byte-array chunks (structure ASCII, strings UTF-8, positions byte positions), or a
    * `Reader` or char-array chunks.  Only a window around the token in progress is retained
    * (starting at `window` elements and doubling only when one token outgrows it), so
    * arbitrarily large input parses in bounded memory; chunk boundaries are invisible to the
    * parse, and empty chunks are fine.  Because the input is not retained there is no `exact`
    * or format-preserving mode; error line/col stay exact, but excerpts degrade to `?` behind
    * the window.  Streams and readers are read as needed but never closed.
    */
  type Chunked = java.io.InputStream | Iterator[Array[Byte]] | java.io.Reader | Iterator[Array[Char]]

  /** Input supplied line by line (`Iterable[String]` or `Iterator[String]`), with an implied
    * newline between consecutive elements, so no token except whitespace can span lines and
    * only the current line is ever retained.  Positions pack (line, offset within line), so
    * error line/col are exact at any input size; at most `Int.MaxValue` lines.  For lines
    * embedded in other values, see `parse(in)(line)`.  No `exact` or format-preserving mode.
    */
  type Lined = Iterable[String] | Iterator[String]

  /** Convert any value with a `Jsonize` instance into its JSON tree: `Json(myCaseClass)`. */
  def apply[A](a: A)(using jz: Jsonize[A]): Json = jz.jsonize(a)

  /** Parse JSON text into a tree, or an `Err` detailing what went wrong and where.
    * The parser is strict (RFC 8259): no trailing commas, no leading zeros, no `NaN`, and
    * nothing but whitespace after the value.  Byte input reads structure as ASCII and
    * decodes strings as UTF-8 (error positions are byte positions).
    *
    * By default numbers become `Jnum.L` when they are integers a Long can hold and `Jnum.D`
    * (correctly rounded) otherwise; with `exact = true`, a number whose value a Double cannot
    * represent exactly is kept as `Jnum.Big` with its original text, so nothing is lost.
    */
  inline def parse(inline in: Source, exact: Boolean = false): JAny = inline in match
    case s: String      => JAny.wrap(Ask.flat{ (new Jparse.Str(s, exact)).parseTop() })
    case b: Array[Byte] => JAny.wrap(Ask.flat{ (new Jparse.Bytes(b, exact)).parseTop() })
    case c: Array[Char] => JAny.wrap(Ask.flat{ (new Jparse.Chars(c, exact)).parseTop() })

  /** Parse straight from off-heap memory with no copy: `Mem[Byte]` is UTF-8, `Mem[Char]` is
    * UTF-16.  Other element types are a compile error (they name no text encoding).
    */
  inline def parse[A <: Mem.Type](inline mem: Mem[A], exact: Boolean): JAny =
    JAny.wrap(Ask.flat{ Jparse.memParser[A](mem, exact, mutable = false, fmt = false).parseTop() })
  inline def parse[A <: Mem.Type](inline mem: Mem[A]): JAny =
    JAny.wrap(Ask.flat{ Jparse.memParser[A](mem, exact = false, mutable = false, fmt = false).parseTop() })

  /** Parse streaming JSON pulled through a sliding window that starts at `window` elements
    * (see `Json.Chunked`).  Exceptions from the source (e.g. an `IOException` mid-read) come
    * back as an `Err`, like any other failure.
    */
  inline def parse(inline in: Chunked, window: Int): JAny =
    JAny.wrap(Ask.flat{ Jparse.chunkedParser(in, mutable = false, window).parseTop() })

  /** Parse streaming JSON through a default-sized sliding window (see `Json.Chunked`). */
  inline def parse(inline in: Chunked): JAny =
    JAny.wrap(Ask.flat{ Jparse.chunkedParser(in, mutable = false, Jparse.defaultWindow).parseTop() })

  /** Parse JSON supplied line by line (see `Json.Lined`). */
  @targetName("parseLined")
  inline def parse(inline in: Lined): JAny =
    JAny.wrap(Ask.flat{ Jparse.linedParser(in, mutable = false).parseTop() })

  /** Parse JSON from lines carried inside `in`'s elements, one line per element as extracted
    * by `line` (see `Json.Lined`).
    */
  def parse[A](in: Iterator[A])(line: A => String): JAny =
    JAny.wrap(Ask.flat{ (new Jparse.Lines(in.map(line))).parseTop() })

  /** SAX-style streaming parse: walk the input once, driving `vis` (see `Jvisitor`), building
    * no tree.  Values the visitor declines are skipped structurally.  There is no `exact` or
    * format-preserving variant -- a single forward pass cannot revisit or retain what it skipped.
    */
  inline def stream(inline in: Source)(vis: Jvisitor): Ask[Unit] = inline in match
    case s: String      => Ask.flat{ (new Jparse.Str(s)).visitTop(vis) }
    case b: Array[Byte] => Ask.flat{ (new Jparse.Bytes(b)).visitTop(vis) }
    case c: Array[Char] => Ask.flat{ (new Jparse.Chars(c)).visitTop(vis) }

  /** SAX-style streaming parse from off-heap memory (`Mem[Byte]` UTF-8, `Mem[Char]` UTF-16). */
  inline def stream[A <: Mem.Type](inline mem: Mem[A])(vis: Jvisitor): Ask[Unit] =
    Ask.flat{ Jparse.memParser[A](mem, exact = false, mutable = false, fmt = false).visitTop(vis) }

  /** SAX-style streaming parse of streaming input (see `Json.Chunked`): the natural pair for
    * huge documents, since neither the input nor a tree is ever held whole -- values the
    * visitor declines are stepped over in bounded memory however large they are.
    */
  inline def stream(inline in: Chunked, window: Int)(vis: Jvisitor): Ask[Unit] =
    Ask.flat{ Jparse.chunkedParser(in, mutable = false, window).visitTop(vis) }

  /** SAX-style streaming parse of streaming input through a default-sized window. */
  @targetName("streamChunked")
  inline def stream(inline in: Chunked)(vis: Jvisitor): Ask[Unit] =
    Ask.flat{ Jparse.chunkedParser(in, mutable = false, Jparse.defaultWindow).visitTop(vis) }

  /** SAX-style streaming parse of line-fed input (see `Json.Lined`). */
  @targetName("streamLined")
  inline def stream(inline in: Lined)(vis: Jvisitor): Ask[Unit] =
    Ask.flat{ Jparse.linedParser(in, mutable = false).visitTop(vis) }

  /** SAX-style streaming parse of lines carried inside `in`'s elements (see `Json.Lined`). */
  def stream[A](in: Iterator[A])(line: A => String)(vis: Jvisitor): Ask[Unit] =
    Ask.flat{ (new Jparse.Lines(in.map(line))).visitTop(vis) }

  /** Format-preserving parse: every collection remembers where it and its contents sat in
    * the input, so an unedited tree prints back byte-for-byte (bar whitespace outside the
    * root value), and an edited one reprints only what was touched, with verbatim source
    * around it.
    */
  inline def parseFmt(inline in: Source, exact: Boolean = false): JAny = inline in match
    case s: String      => JAny.wrap(Ask.flat{ (new Jparse.Str(s, exact, fmt = true)).parseTop() })
    case b: Array[Byte] => JAny.wrap(Ask.flat{ (new Jparse.Bytes(b, exact, fmt = true)).parseTop() })
    case c: Array[Char] => JAny.wrap(Ask.flat{ (new Jparse.Chars(c, exact, fmt = true)).parseTop() })

  /** Format-preserving parse from off-heap memory.  Because format preservation retains the
    * whole source and the segment is caller-owned, the bytes/chars are snapshotted to the heap.
    */
  inline def parseFmt[A <: Mem.Type](inline mem: Mem[A], exact: Boolean): JAny =
    JAny.wrap(Ask.flat{ Jparse.memParser[A](mem, exact, mutable = false, fmt = true).parseTop() })
  inline def parseFmt[A <: Mem.Type](inline mem: Mem[A]): JAny =
    JAny.wrap(Ask.flat{ Jparse.memParser[A](mem, exact = false, mutable = false, fmt = true).parseTop() })

  /** The mutable side of the JSON hierarchy: each container's editable class mixes this in
    * (`Jobj.M`, and `Jarr.M` for the array backings), so a mutable tree can be worked with
    * exhaustively in its own hierarchy just like the immutable one:
    * {{{
    * (j: Json) match { case a: Jarr => ... }        // any array
    * (j: Json) match { case m: Jarr.M => ... }      // only an editable array
    * }}}
    * The editing contract is by upcast, with no copying, ever: edit through `.M`-typed
    * references, and hand off the plain `Json`-typed view when done.  Whoever keeps an `.M`
    * reference can still mutate, so losing the editing handles is the owner's responsibility.
    */
  sealed trait M {}

  /** Mutable-tree parsing: every container comes back as its editable `.M` class, upcast --
    * pattern-match (e.g. on `Jobj.M`) to edit.  Numeric arrays are not packed in mutable
    * mode.
    */
  object M {
    inline def parse(inline in: Source, exact: Boolean = false): JAny = inline in match
      case s: String      => JAny.wrap(Ask.flat{ (new Jparse.Str(s, exact, mutable = true)).parseTop() })
      case b: Array[Byte] => JAny.wrap(Ask.flat{ (new Jparse.Bytes(b, exact, mutable = true)).parseTop() })
      case c: Array[Char] => JAny.wrap(Ask.flat{ (new Jparse.Chars(c, exact, mutable = true)).parseTop() })

    /** Mutable-tree parse straight from off-heap memory (`Mem[Byte]` UTF-8, `Mem[Char]` UTF-16). */
    inline def parse[A <: Mem.Type](inline mem: Mem[A], exact: Boolean): JAny =
      JAny.wrap(Ask.flat{ Jparse.memParser[A](mem, exact, mutable = true, fmt = false).parseTop() })
    inline def parse[A <: Mem.Type](inline mem: Mem[A]): JAny =
      JAny.wrap(Ask.flat{ Jparse.memParser[A](mem, exact = false, mutable = true, fmt = false).parseTop() })

    /** Mutable-tree parse of streaming input (see `Json.Chunked`). */
    inline def parse(inline in: Chunked, window: Int): JAny =
      JAny.wrap(Ask.flat{ Jparse.chunkedParser(in, mutable = true, window).parseTop() })
    inline def parse(inline in: Chunked): JAny =
      JAny.wrap(Ask.flat{ Jparse.chunkedParser(in, mutable = true, Jparse.defaultWindow).parseTop() })

    /** Mutable-tree parse of line-fed input (see `Json.Lined`). */
    @targetName("parseLined")
    inline def parse(inline in: Lined): JAny =
      JAny.wrap(Ask.flat{ Jparse.linedParser(in, mutable = true).parseTop() })

    /** Mutable-tree parse of lines carried inside `in`'s elements (see `Json.Lined`). */
    def parse[A](in: Iterator[A])(line: A => String): JAny =
      JAny.wrap(Ask.flat{ (new Jparse.Lines(in.map(line), mutable = true)).parseTop() })

    /** Format-preserving mutable parse: the primary editing flow.  Value replacements keep
      * the formatting around them; a structural edit drops only the edited node's own
      * preserved format (that node re-serializes fresh, everything else stays verbatim).
      */
    inline def parseFmt(inline in: Source, exact: Boolean = false): JAny = inline in match
      case s: String      => JAny.wrap(Ask.flat{ (new Jparse.Str(s, exact, mutable = true, fmt = true)).parseTop() })
      case b: Array[Byte] => JAny.wrap(Ask.flat{ (new Jparse.Bytes(b, exact, mutable = true, fmt = true)).parseTop() })
      case c: Array[Char] => JAny.wrap(Ask.flat{ (new Jparse.Chars(c, exact, mutable = true, fmt = true)).parseTop() })

    /** Format-preserving mutable parse from off-heap memory (snapshots to the heap; see `parseFmt`). */
    inline def parseFmt[A <: Mem.Type](inline mem: Mem[A], exact: Boolean): JAny =
      JAny.wrap(Ask.flat{ Jparse.memParser[A](mem, exact, mutable = true, fmt = true).parseTop() })
    inline def parseFmt[A <: Mem.Type](inline mem: Mem[A]): JAny =
      JAny.wrap(Ask.flat{ Jparse.memParser[A](mem, exact = false, mutable = true, fmt = true).parseTop() })
  }

  private[jsaun] def expectErr(what: String, j: Json): Err = Err(s"expected $what, found ${j.kind}")

  /** True if `j` can be emitted verbatim from retained source: it has format info, nothing
    * in it was edited, and the same holds all the way down.  Leaves are always clean (their
    * replacement dirties the parent's slot instead).
    */
  private[jsaun] def cleanBelow(j: Json): Boolean = j match
    case a: Jarr.A =>
      val f = a.fmt
      (f ne null) && !f.anyDirty && {
        var k = 0
        while k < a.n && cleanBelow(a.vs(k)) do k += 1
        k == a.n
      }
    case d: Jarr.D =>
      val f = d.fmt
      (f ne null) && !f.anyDirty
    case o: Jobj =>
      val f = o.fmt
      (f ne null) && !f.anyDirty && {
        var k = 0
        while k < o.n && cleanBelow(o.vs(k)) do k += 1
        k == o.n
      }
    case _ => true
}


/** The JSON null value. */
object Jnull extends Json {
  def kind = "null"
  override def isNull = true
  def printTo(out: Jout): Unit = out.add("null")
}


/** A JSON boolean; the only instances are `Jbool.True` and `Jbool.False`. */
sealed abstract class Jbool protected () extends Json {
  def value: Boolean
  def kind = "boolean"
  override def bool: Ask[Boolean] = Is(value)
  override def boolOr(alt: Boolean): Boolean = value
  def printTo(out: Jout): Unit = out.add(if value then "true" else "false")
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

  def printTo(out: Jout): Unit = Jstr.encodeTo(out, text)
}
object Jstr {
  def apply(text: String): Jstr = new Jstr(text)
  def unapply(js: Jstr): Some[String] = Some(js.text)

  /** Append `s` as a quoted JSON string, escaping only what JSON requires; clean runs
    * (including all non-ASCII, which needs no escaping) go through in bulk.
    */
  private[jsaun] def encodeTo(out: Jout, s: String): Unit =
    out.add('"')
    var i0 = 0
    var i = 0
    while i < s.length do
      val c = s.charAt(i)
      if c == '"' || c == '\\' || c < ' ' then
        if i > i0 then out.add(s, i0, i)
        c match
          case '"'  => out.add("\\\"")
          case '\\' => out.add("\\\\")
          case '\b' => out.add("\\b")
          case '\t' => out.add("\\t")
          case '\n' => out.add("\\n")
          case '\f' => out.add("\\f")
          case '\r' => out.add("\\r")
          case _ =>
            out.add("\\u00")
            out.add("0123456789abcdef".charAt((c >> 4) & 0xF))
            out.add("0123456789abcdef".charAt(c & 0xF))
        i0 = i + 1
      i += 1
    if i > i0 then out.add(s, i0, i)
    out.add('"')
}


/** A JSON number, stored as whichever of `Jnum.L` (Long), `Jnum.D` (Double), or `Jnum.Big`
  * (original text, exact-mode parsing only) represents it faithfully.  Representations that
  * denote the same value compare equal (and hash alike), so `Jnum(3) == Jnum(3.0)`.
  */
sealed abstract class Jnum protected () extends Json {
  def kind = "number"

  /** This number as a Double (lossy for values a Double cannot hold). */
  def double: Double

  /** True if this number has no fractional part. */
  def isWhole: Boolean

  override def dbl: Ask[Double] = Is(double)
  override def dblOr(alt: Double): Double = double
}
object Jnum {
  def apply(value: Long): Jnum = new L(value)
  def apply(value: Double): Jnum = new D(value)
  def apply(value: BigDecimal): Jnum = new Big(value.underlying.toString)

  /** Append `d` as JSON under the target's numeric policy (NaN and infinities, which JSON
    * cannot express, become null).
    */
  private[jsaun] def printDbl(out: Jout, d: Double): Unit =
    if d.isNaN || d.isInfinite then out.add("null")
    else out.style.num match
      case Jstyle.Num.Exact => out.add(d)
      case Jstyle.Num.Sig(n) => out.add(Jstyle.sigText(d, n))
      case Jstyle.Num.Fixed(n) => out.add(Jstyle.fixedText(d, n))

  /** True if `d` is exactly the value that `text` denotes (cold; exact-mode parsing only). */
  private[jsaun] def exactDouble(d: Double, text: String): Boolean =
    !d.isInfinite && (new java.math.BigDecimal(d)).compareTo(new java.math.BigDecimal(text)) == 0

  /** A JSON integer that fits in a Long. */
  final class L(val value: Long) extends Jnum {
    def double = value.toDouble
    def isWhole = true
    override def long: Ask[Long] = Is(value)
    override def longOr(alt: Long): Long = value

    override def equals(a: Any): Boolean = a match
      case l: L => value == l.value
      case d: D => d.value == value.toDouble && d.value.toLong == value
      case b: Big => b == this
      case _ => false
    override def hashCode: Int = value.##   // scala.## makes this agree with D and Big when the values are equal

    def printTo(out: Jout): Unit = out.add(value)
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
      case b: Big => b == this
      case _ => false
    override def hashCode: Int = value.##

    def printTo(out: Jout): Unit = Jnum.printDbl(out, value)
  }

  /** A JSON number kept as its original text because a Double cannot hold it exactly (only
    * produced by exact-mode parsing, or from a BigDecimal).  Prints verbatim, so exact-mode
    * numbers round-trip character for character.
    */
  final class Big @publicInBinary() private[jsaun] (val text: String) extends Jnum {
    lazy val big: BigDecimal = BigDecimal(new java.math.BigDecimal(text))
    override lazy val double: Double = java.lang.Double.parseDouble(text)
    def isWhole: Boolean = big.isWhole

    override def long: Ask[Long] =
      if big.isValidLong then Is(big.toLong)
      else Alt(Err(s"number is not an integer Long can hold: $text"))
    override def longOr(alt: Long): Long = if big.isValidLong then big.toLong else alt

    override def equals(a: Any): Boolean = a match
      case b: Big => big.compare(b.big) == 0
      case l: L => big.isValidLong && big.toLong == l.value
      case d: D => !d.value.isNaN && !d.value.isInfinite && big.compare(BigDecimal(new java.math.BigDecimal(d.value))) == 0
      case _ => false
    override def hashCode: Int = big.##   // scala.## on BigDecimal agrees with Long/Double ## when values coincide

    def printTo(out: Jout): Unit = out.add(text)
  }
}


/** A JSON array.  `Jarr.A` holds arbitrary values; `Jarr.D` packs all-Double arrays (the
  * parser packs automatically when every element parsed as a `Jnum.D`); `Jarr.F` and
  * `Jarr.I` pack Floats and Ints for user-built compact numeric data (the parser never
  * produces them).  All backings are interchangeable in use and compare (and hash) equal
  * element by element.
  */
sealed abstract class Jarr protected () extends Json {
  def kind = "array"
  override def arr: Ask[Jarr] = Is(this)

  /** Format info from a format-preserving parse (see `Jfmt`); null when none exists. */
  private[jsaun] var fmt: Jfmt | Null = null

  /** Inferred separator style, kept when a structural edit invalidates `fmt`. */
  private[jsaun] var sty: Jfmt.Local | Null = null

  def foreach(f: Json => Unit): Unit

  /** The elements as a (copied) `Array[Double]`, if every element is a number. */
  def dbls: Ask[Array[Double]]

  /** Element `k` as a Json (packed backings materialize a `Jnum`); no bounds check. */
  private[jsaun] def elem(k: Int): Json

  final override def equals(a: Any): Boolean = a match
    case x: Jarr =>
      if this eq x then true
      else if size != x.size then false
      else this match
        case d: Jarr.D if x.isInstanceOf[Jarr.D] =>
          val y = x.asInstanceOf[Jarr.D]
          var k = 0
          while k < d.n && d.xs(k) == y.xs(k) do k += 1
          k == d.n
        case _ =>
          var k = 0
          while k < size && elem(k) == x.elem(k) do k += 1
          k == size
    case _ => false

  final override def hashCode: Int =
    var h = 1
    var k = 0
    while k < size do
      h = h * 31 + elem(k).##
      k += 1
    h
}
object Jarr {
  def apply(values: Json*): Jarr =
    val a = new Array[Json](values.length)
    values.copyToArray(a) __ Unit
    new A(a, a.length)

  def apply(values: Array[Double]): Jarr = new D(values.clone, values.length)
  def apply(values: Array[Float]): Jarr = new F(values.clone, values.length)
  def apply(values: Array[Int]): Jarr = new I(values.clone, values.length)

  private[jsaun] val empty: A = new A(new Array[Json](0), 0)

  /** Marker for the editable arrays (`Jarr.A.M`, `Jarr.D.M`); see `Json.M` for the contract. */
  sealed trait M extends Json.M {}

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

    final def dbls: Ask[Array[Double]] =
      val a = new Array[Double](n)
      var k = 0
      var bad = -1
      while bad < 0 && k < n do
        vs(k) match
          case m: Jnum =>
            a(k) = m.double
            k += 1
          case _ => bad = k
      if bad < 0 then Is(a)
      else Alt(Err(s"element $bad is not a number but ${vs(bad).kind}"))

    private[jsaun] def elem(k: Int): Json = vs(k)

    private def emitWith(out: Jout, open: String, sep: String, close: String): Unit =
      out.add('[')
      out.add(open)
      var k = 0
      while k < n do
        if k > 0 then out.add(sep)
        vs(k).printTo(out)
        k += 1
      out.add(close)
      out.add(']')

    def printTo(out: Jout): Unit =
      val f = fmt
      if (f ne null) && !out.ignoreFmt then
        if Json.cleanBelow(this) then f.src.copyTo(out, f.start, f.end)
        else
          var prev = f.start
          var k = 0
          while k < n do
            val sp = f.spans(k)
            f.src.copyTo(out, prev, Jfmt.start(sp))
            if !f.isDirty(k) && Json.cleanBelow(vs(k)) then f.src.copyTo(out, Jfmt.start(sp), Jfmt.end(sp))
            else vs(k).printTo(out)
            prev = Jfmt.end(sp)
            k += 1
          f.src.copyTo(out, prev, f.end)
      else
        val s = sty
        if (s ne null) && !out.ignoreFmt then emitWith(out, s.open, s.sep, s.close)
        else
          val st = out.style
          if st.indent.isEmpty || n == 0 then emitWith(out, "", if st.spaceAfterComma then ", " else ",", "")
          else
            out.depth += 1
            emitWith(out, Jstyle.pad(st.indent, out.depth), "," + Jstyle.pad(st.indent, out.depth), Jstyle.pad(st.indent, out.depth - 1))
            out.depth -= 1
  }
  object A {
    /** Growable editable general array; upcast to `Jarr.A`/`Jarr` to hand off a view with no
      * editing surface (no copy is made -- see `Json.M`).
      */
    final class M private[jsaun] (vs0: Array[Json], n0: Int) extends A(vs0, n0) with Jarr.M {
      def this() = this(new Array[Json](8), 0)

      private def ensure(k: Int): Unit =
        if n + k > vs.length then
          var m = vs.length * 2
          while m < n + k do m *= 2
          vs = java.util.Arrays.copyOf(vs, m)

      /** Set element `i`, which must exist.  Preserved formatting around it is kept. */
      def update(i: Int, v: Json): Unit =
        if i < 0 || i >= n then throw new IndexOutOfBoundsException(s"index $i of array of size $n")
        vs(i) = v
        fmt match
          case null => ()
          case f => f.markDirty(i)

      // Structural changes invalidate the span bookkeeping, but the layout style is inferred
      // from it first, so edits keep matching their siblings' formatting
      private def demoteFmt(): Unit =
        val f = fmt
        if f ne null then
          sty = Jfmt.Local.ofArr(f, n)
          fmt = null

      def add(v: Json): this.type =
        demoteFmt()
        ensure(1)
        vs(n) = v
        n += 1
        this

      def insert(i: Int, v: Json): this.type =
        if i < 0 || i > n then throw new IndexOutOfBoundsException(s"insertion point $i in array of size $n")
        demoteFmt()
        ensure(1)
        System.arraycopy(vs, i, vs, i + 1, n - i)
        vs(i) = v
        n += 1
        this

      /** Remove and answer element `i`, which must exist. */
      def remove(i: Int): Json =
        if i < 0 || i >= n then throw new IndexOutOfBoundsException(s"index $i of array of size $n")
        demoteFmt()
        val v = vs(i)
        System.arraycopy(vs, i + 1, vs, i, n - i - 1)
        n -= 1
        vs(n) = null
        v

      def clear(): this.type =
        demoteFmt()
        var k = 0
        while k < n do
          vs(k) = null
          k += 1
        n = 0
        this
    }
    object M {
      def apply(values: Json*): M =
        val m = new M()
        values.foreach(v => m.add(v) __ Unit)
        m
    }
  }

  /** An all-numeric JSON array packed as unboxed Doubles.  Element access materializes a
    * `Jnum.D`; bulk numeric use should go through `dbls`.
    */
  sealed class D private[jsaun] (private[jsaun] var xs: Array[Double], private[jsaun] var n: Int) extends Jarr {
    final override def size: Int = n

    final override def apply(i: Int): JAny =
      if i >= 0 && i < n then JAny(Jnum(xs(i)))
      else JAny.err(Err(s"index $i out of bounds for array of size $n"))

    final def foreach(f: Json => Unit): Unit =
      var k = 0
      while k < n do
        f(Jnum(xs(k)))
        k += 1

    final def dbls: Ask[Array[Double]] = Is(java.util.Arrays.copyOf(xs, n))

    private[jsaun] def elem(k: Int): Json = Jnum(xs(k))

    private def emitWith(out: Jout, open: String, sep: String, close: String): Unit =
      out.add('[')
      out.add(open)
      var k = 0
      while k < n do
        if k > 0 then out.add(sep)
        Jnum.printDbl(out, xs(k))
        k += 1
      out.add(close)
      out.add(']')

    def printTo(out: Jout): Unit =
      val f = fmt
      if (f ne null) && !out.ignoreFmt then
        if !f.anyDirty then f.src.copyTo(out, f.start, f.end)
        else
          var prev = f.start
          var k = 0
          while k < n do
            val sp = f.spans(k)
            f.src.copyTo(out, prev, Jfmt.start(sp))
            if !f.isDirty(k) then f.src.copyTo(out, Jfmt.start(sp), Jfmt.end(sp))
            else Jnum.printDbl(out, xs(k))
            prev = Jfmt.end(sp)
            k += 1
          f.src.copyTo(out, prev, f.end)
      else
        val s = sty
        if (s ne null) && !out.ignoreFmt then emitWith(out, s.open, s.sep, s.close)
        else
          val st = out.style
          if st.indent.isEmpty || n == 0 then emitWith(out, "", if st.spaceAfterComma then ", " else ",", "")
          else
            out.depth += 1
            emitWith(out, Jstyle.pad(st.indent, out.depth), "," + Jstyle.pad(st.indent, out.depth), Jstyle.pad(st.indent, out.depth - 1))
            out.depth -= 1
  }
  object D {
    /** Growable editable packed-Double array; upcast to `Jarr.D`/`Jarr` to hand off a view
      * with no editing surface (no copy is made -- see `Json.M`).
      */
    final class M private[jsaun] (xs0: Array[Double], n0: Int) extends D(xs0, n0) with Jarr.M {
      def this() = this(new Array[Double](8), 0)

      private def ensure(k: Int): Unit =
        if n + k > xs.length then
          var m = xs.length * 2
          while m < n + k do m *= 2
          xs = java.util.Arrays.copyOf(xs, m)

      /** Set element `i`, which must exist.  Preserved formatting around it is kept. */
      def update(i: Int, x: Double): Unit =
        if i < 0 || i >= n then throw new IndexOutOfBoundsException(s"index $i of array of size $n")
        xs(i) = x
        fmt match
          case null => ()
          case f => f.markDirty(i)

      // See Jarr.A.M.demoteFmt
      private def demoteFmt(): Unit =
        val f = fmt
        if f ne null then
          sty = Jfmt.Local.ofArr(f, n)
          fmt = null

      def add(x: Double): this.type =
        demoteFmt()
        ensure(1)
        xs(n) = x
        n += 1
        this

      def insert(i: Int, x: Double): this.type =
        if i < 0 || i > n then throw new IndexOutOfBoundsException(s"insertion point $i in array of size $n")
        demoteFmt()
        ensure(1)
        System.arraycopy(xs, i, xs, i + 1, n - i)
        xs(i) = x
        n += 1
        this

      /** Remove and answer element `i`, which must exist. */
      def remove(i: Int): Double =
        if i < 0 || i >= n then throw new IndexOutOfBoundsException(s"index $i of array of size $n")
        demoteFmt()
        val x = xs(i)
        System.arraycopy(xs, i + 1, xs, i, n - i - 1)
        n -= 1
        x

      def clear(): this.type =
        demoteFmt()
        n = 0
        this
    }
    object M {
      def apply(values: Double*): M =
        val m = new M()
        values.foreach(x => m.add(x) __ Unit)
        m
    }
  }

  /** An all-numeric JSON array packed as Floats, for user-built compact data (the parser
    * never packs to Float).  Elements read as `Jnum.D` of the widened value.
    */
  sealed class F private[jsaun] (private[jsaun] var xs: Array[Float], private[jsaun] var n: Int) extends Jarr {
    final override def size: Int = n

    final override def apply(i: Int): JAny =
      if i >= 0 && i < n then JAny(Jnum(xs(i).toDouble))
      else JAny.err(Err(s"index $i out of bounds for array of size $n"))

    final def foreach(f: Json => Unit): Unit =
      var k = 0
      while k < n do
        f(Jnum(xs(k).toDouble))
        k += 1

    final def dbls: Ask[Array[Double]] =
      val a = new Array[Double](n)
      var k = 0
      while k < n do
        a(k) = xs(k).toDouble
        k += 1
      Is(a)

    private[jsaun] def elem(k: Int): Json = Jnum(xs(k).toDouble)

    private def emitWith(out: Jout, open: String, sep: String, close: String): Unit =
      out.add('[')
      out.add(open)
      var k = 0
      while k < n do
        if k > 0 then out.add(sep)
        Jnum.printDbl(out, xs(k).toDouble)
        k += 1
      out.add(close)
      out.add(']')

    def printTo(out: Jout): Unit =
      val s = sty
      if (s ne null) && !out.ignoreFmt then emitWith(out, s.open, s.sep, s.close)
      else
        val st = out.style
        if st.indent.isEmpty || n == 0 then emitWith(out, "", if st.spaceAfterComma then ", " else ",", "")
        else
          out.depth += 1
          emitWith(out, Jstyle.pad(st.indent, out.depth), "," + Jstyle.pad(st.indent, out.depth), Jstyle.pad(st.indent, out.depth - 1))
          out.depth -= 1
  }

  /** An all-integer JSON array packed as Ints, for user-built compact data (the parser never
    * packs to Int).  Elements read as `Jnum.L`.
    */
  sealed class I private[jsaun] (private[jsaun] var xs: Array[Int], private[jsaun] var n: Int) extends Jarr {
    final override def size: Int = n

    final override def apply(i: Int): JAny =
      if i >= 0 && i < n then JAny(Jnum(xs(i).toLong))
      else JAny.err(Err(s"index $i out of bounds for array of size $n"))

    final def foreach(f: Json => Unit): Unit =
      var k = 0
      while k < n do
        f(Jnum(xs(k).toLong))
        k += 1

    final def dbls: Ask[Array[Double]] =
      val a = new Array[Double](n)
      var k = 0
      while k < n do
        a(k) = xs(k).toDouble
        k += 1
      Is(a)

    private[jsaun] def elem(k: Int): Json = Jnum(xs(k).toLong)

    private def emitWith(out: Jout, open: String, sep: String, close: String): Unit =
      out.add('[')
      out.add(open)
      var k = 0
      while k < n do
        if k > 0 then out.add(sep)
        out.add(xs(k).toLong)
        k += 1
      out.add(close)
      out.add(']')

    def printTo(out: Jout): Unit =
      val s = sty
      if (s ne null) && !out.ignoreFmt then emitWith(out, s.open, s.sep, s.close)
      else
        val st = out.style
        if st.indent.isEmpty || n == 0 then emitWith(out, "", if st.spaceAfterComma then ", " else ",", "")
        else
          out.depth += 1
          emitWith(out, Jstyle.pad(st.indent, out.depth), "," + Jstyle.pad(st.indent, out.depth), Jstyle.pad(st.indent, out.depth - 1))
          out.depth -= 1
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

  /** Format info from a format-preserving parse (see `Jfmt`); null when none exists. */
  private[jsaun] var fmt: Jfmt | Null = null

  /** Inferred separator style, kept when a structural edit invalidates `fmt`. */
  private[jsaun] var sty: Jfmt.Local | Null = null

  // Built at most once per content; harmless to rebuild on a race (single-threaded use
  // expected); mutation (Jobj.M only) resets it to null
  private[jsaun] var index: java.util.HashMap[String, Json] | Null = null

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

  private def emitWith(out: Jout, open: String, sep: String, mid: String, close: String): Unit =
    out.add('{')
    out.add(open)
    var k = 0
    while k < n do
      if k > 0 then out.add(sep)
      Jstr.encodeTo(out, ks(k))
      out.add(mid)
      vs(k).printTo(out)
      k += 1
    out.add(close)
    out.add('}')

  def printTo(out: Jout): Unit =
    val f = fmt
    if (f ne null) && !out.ignoreFmt then
      if Json.cleanBelow(this) then f.src.copyTo(out, f.start, f.end)
      else
        var prev = f.start
        var k = 0
        while k < n do
          val ksp = f.spans(2 * k)
          val vsp = f.spans(2 * k + 1)
          f.src.copyTo(out, prev, Jfmt.start(ksp))
          if !f.isDirty(2 * k) then f.src.copyTo(out, Jfmt.start(ksp), Jfmt.end(ksp))
          else Jstr.encodeTo(out, ks(k))
          f.src.copyTo(out, Jfmt.end(ksp), Jfmt.start(vsp))
          if !f.isDirty(2 * k + 1) && Json.cleanBelow(vs(k)) then f.src.copyTo(out, Jfmt.start(vsp), Jfmt.end(vsp))
          else vs(k).printTo(out)
          prev = Jfmt.end(vsp)
          k += 1
        f.src.copyTo(out, prev, f.end)
    else
      val s = sty
      if (s ne null) && !out.ignoreFmt then emitWith(out, s.open, s.sep, s.mid, s.close)
      else
        val st = out.style
        if st.indent.isEmpty || n == 0 then
          emitWith(out, "", if st.spaceAfterComma then ", " else ",", if st.spaceAfterColon then ": " else ":", "")
        else
          out.depth += 1
          emitWith(out, Jstyle.pad(st.indent, out.depth), "," + Jstyle.pad(st.indent, out.depth),
                   if st.spaceAfterColon then ": " else ":", Jstyle.pad(st.indent, out.depth - 1))
          out.depth -= 1
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

  /** Growable editable object; upcast to `Jobj` to hand off a view with no editing surface
    * (no copy is made -- see `Json.M`).
    */
  final class M private[jsaun] (ks0: Array[String], vs0: Array[Json], n0: Int) extends Jobj(ks0, vs0, n0) with Json.M {
    def this() = this(new Array[String](8), new Array[Json](8), 0)

    private def ensure(k: Int): Unit =
      if n + k > ks.length then
        var m = ks.length * 2
        while m < n + k do m *= 2
        ks = java.util.Arrays.copyOf(ks, m)
        vs = java.util.Arrays.copyOf(vs, m)

    // See Jarr.A.M.demoteFmt
    private def demoteFmt(): Unit =
      val f = fmt
      if f ne null then
        sty = Jfmt.Local.ofObj(f, n)
        fmt = null

    /** Append an entry, permitting duplicate keys. */
    def add(key: String, v: Json): this.type =
      demoteFmt()
      ensure(1)
      ks(n) = key
      vs(n) = v
      n += 1
      index = null
      this

    /** Replace the value at `key` (the last occurrence, if duplicated), appending if absent.
      * An in-place replacement keeps the preserved formatting around the value.
      */
    def put(key: String, v: Json): this.type =
      var k = n - 1
      while k >= 0 && ks(k) != key do k -= 1
      if k >= 0 then
        vs(k) = v
        index = null
        fmt match
          case null => ()
          case f => f.markDirty(2 * k + 1)
      else add(key, v) __ Unit
      this

    def update(key: String, v: Json): Unit = put(key, v) __ Unit

    /** Remove every entry with `key`; answers how many were removed. */
    def remove(key: String): Int =
      if contains(key) then demoteFmt()
      var w = 0
      var k = 0
      while k < n do
        if ks(k) != key then
          if w != k then
            ks(w) = ks(k)
            vs(w) = vs(k)
          w += 1
        k += 1
      val removed = n - w
      if removed > 0 then
        var z = w
        while z < n do
          ks(z) = null
          vs(z) = null
          z += 1
        n = w
        index = null
      removed

    def clear(): this.type =
      demoteFmt()
      var k = 0
      while k < n do
        ks(k) = null
        vs(k) = null
        k += 1
      n = 0
      index = null
      this
  }
  object M {
    def apply(kvs: (String, Json)*): M =
      val m = new M()
      kvs.foreach((k, v) => m.add(k, v) __ Unit)
      m
  }
}
