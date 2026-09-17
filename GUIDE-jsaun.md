# kse3 jsaun

The jsaun module is JSON.  A `Json` tree is six node types with accessors that answer `Ask` instead of throwing, so
a whole path into a document is one expression and the first thing that goes wrong is what comes out.  The parser is
strict, positions its errors, reads from strings, bytes, off-heap memory, streams, or lines, and can remember where
every token sat so that an edited document prints back with only the edits changed.  `Jsonize` and `FromJson` are
the typeclasses for your own types, derived for case classes and sealed traits.  Underneath, numbers stay unboxed
where they can: an all-number array is an `Array[Double]`, and a streaming walk delivers `Long` and `Double` without
building anything.

`import kse.jsaun.{given, *}` brings in everything below.  It leans on `kse.basics` and `kse.flow` (`Ask`, `.?`,
`nice`), which you'll want imported alongside it.

<!-- guide examples: jsaun/test/src/GuideExamples.scala -->
Every code example below is compiled and run from `jsaun/test/src/GuideExamples.scala`; `mill guides.run` verifies
that the two copies are identical.

## What's here

- **Parsing and access**: `Json.parse` gives a `JAny`, an `Ask[Json]` whose accessors chain; errors say where and inside what.
- **Building and printing**: `Jobj`, `Jarr`, `Jstr`, `Jnum`, `Jbool`, `Jnull`; `print` in a `Jstyle`, compact or pretty, with a numeric policy.
- **Jsonize and FromJson**: `derives` on a case class or sealed trait; `Json.print(a)` and `.to[A]`; one lambda each for a type of your own.
- **Format-preserving edits**: `Json.M.parseFmt`, edit through `Jobj.M` and `Jarr.A.M`, print back with everything else verbatim.
- **Streaming with Jvisitor and Jbuilder**: one pass over input of any size, skipping what you don't want and boxing nothing.
- **Numbers and packed arrays**: `Long`, `Double`, or exact text; `NaN` and the infinities; `Jarr.D` and `dbls` for bulk data.

## Parsing and access

`Json.parse(text)` gives a `JAny`, which is `Ask[Json]` under an opaque name: at runtime a `Json` or an `Err`, with
no box either way.  `JAny` has the accessors `Json` has, so `("key")`, `(i)`, `str`, `long`, `dbl`, `bool`, `arr`,
`obj`, `size`, `has`, and `isNull` chain straight through, and the first failure, whether the parse itself, a missing
key, or a wrong type, is what comes out at the end.  `strOr(alt)` and its kin answer a default instead, and `json_?`
is `.?` for the `Json`.  The parser is RFC 8259 and strict (no trailing commas, no comments, no leading zeros), and
adds one thing: `NaN`, `Infinity`, and `-Infinity` read as the Doubles they name, so every Double round-trips.
Duplicate keys keep the last.  The input may be a `String`, UTF-8 `Array[Byte]`, `Array[Char]`, or `Mem`; an
`InputStream`, `Reader`, or iterator of chunks is parsed through a sliding window, and an `Iterable[String]` is
read one line at a time, so size is not a concern.  A failure carries a `Jerr`: description, position, line, column,
and a caret-marked excerpt, wrapped in the chain of containers that led there.

**Reach for this when** you'd reach for Jackson's tree model, uJson, or circe's `Json`, or would `.get` your way
down a chain of `Option`s.

<!-- guide: parse.kse3 -->
```scala
Json.parse(text)("stations")(0)("id").str       // Ask[String]; the first failure wins, parse or access
```

<!-- guide: access.kse3 -->
```scala
val j = Json.parse(text).json_?                 // the Json, or leave with the parse error
val name = j("name").strOr("unnamed")           // the Or forms answer a default instead
val n = j("stations").size                      // elements or keys; 0 for a scalar, -1 for an error
val depth = j("stations")(0)("depth").dbl.?     // a Double, or leave with "expected a number, found string"
```

<!-- guide: parseerr.kse3 -->
```scala
Json.parse(text).ask.fold(_ => "parsed")(_.toString)   // "expected ',' or ']' in array, found 'x' (line 1, char 9)" and a caret
```

`ask` is the `JAny` as a plain `Ask[Json]`, for `fold`, `map`, and the rest.  Nesting deeper than 512 levels is
refused, so hostile input can't exhaust the stack.  Whitespace outside the root value is the one thing a
format-preserving parse (below) does not keep.

Full API: `jsaun/src/Json.scala`, `JAny` at the top and `object Json` for the `parse` overloads; `jsaun/src/Jparse.scala`
is the parser itself.

## Building and printing

`Jobj("k" -> v, ...)`, `Jarr(v, ...)`, `Jstr(s)`, `Jnum(n)`, `Jbool(b)`, and `Jnull` build a tree; an object keeps its
key order and the immutable tree has no setters.  `print` gives text and `printBytes` UTF-8, in the `Jstyle` that
is the given in scope: `Jstyle.compact` by default, `Jstyle.pretty` for two-space indentation with any collection
that fits in the rest of a 78-column line kept on one line, `.fitTo(n)` for another width and `fitTo(0)` for one
element per line.  The numeric policy lives on the style too: `.sig(n)` prints each Double as the shortest decimal
within `n` significant figures, `.fixed(n)` to `n` decimal places, and `exactly` restores the shortest exact form; so
`0.30000000000000004` prints as `0.3` under a limit while `0.5` never grows.  `toString` is compact print.  Equality
is by value: `Jnum(3) == Jnum(3.0)`, objects compare as multisets of entries, and arrays element by element whatever
their backing.

**Reach for these when** you'd build JSON with string interpolation, or reach for a `Map[String, Any]` and a
serializer.

<!-- guide: build.kse3 -->
```scala
val j = Jobj(
  "name" -> Jstr("eel survey"),
  "stations" -> Jarr(Jobj("id" -> Jstr("a1"), "depth" -> Jnum(12.5)), Jobj("id" -> Jstr("b2"), "depth" -> Jnum(3))),
  "done" -> Jbool(true),
  "note" -> Jnull
)
val compact = j.print                           // {"name":"eel survey",...}: the default style, and toString
val pretty = j.print(using Jstyle.pretty)       // indented, with whatever fits on a 78-column line kept on one
```

<!-- guide: styles.kse3 -->
```scala
val a = Jarr(xs)                                // a packed array of Doubles
val exact = a.print                             // [0.30000000000000004,0.5,86.0]
val tidy = a.print(using Jstyle.compact.sig(4)) // [0.3,0.5,86]: the shortest decimal within 4 significant figures
val spaced = a.print(using Jstyle.pretty)       // [0.30000000000000004, 0.5, 86.0]: fits the line, so stays on it
```

A fit-aware style measures before it breaks, so an all-scalar array too long for one line wraps into aligned
columns rather than one element per line.  A `Jout.Str` or `Jout.Bytes` is the output target if you're writing a
lot of small values into one buffer.

Full API: `jsaun/src/Json.scala` for the node types and their companions; `jsaun/src/Jfmt.scala` for `Jstyle`;
`jsaun/src/Jout.scala` for the targets.

## Jsonize and FromJson

`derives Jsonize, FromJson` on a case class gives it an object encoding keyed by field name.  A nested case class
without its own `derives` is derived on demand; an `Option` field prints as `null` and may be absent on the way in;
collections, arrays, and `Map[String, A]` are arrays and objects; a sealed trait prints its case's object with a
`"type"` field added and reads it back by that field.  `Json(a)` builds the tree, `Json.print(a)` writes text from
the value with no tree in between, and `Json.parse(text).to[A]` decodes.  A field that fails to decode is reported
with its key, and when several fail they are all reported at once.  For a type of your own, a `given Jsonize[A]` is
one lambda and so is a `given FromJson[A]`.

**Reach for these when** you'd write `toJson` by hand, or would pull in a codec library for one case class.

<!-- guide: codec.kse3 -->
```scala
case class Station(id: String, depth: Double, tags: List[String]) derives Jsonize, FromJson
case class Survey(name: String, stations: Vector[Station], note: Option[String]) derives Jsonize, FromJson
sealed trait Gear derives Jsonize, FromJson         // a sum prints its case's object with a "type" field added
case class Net(mesh: Double) extends Gear
case class Trap(count: Int) extends Gear
```

<!-- guide: codecuse.kse3 -->
```scala
val text = Json.print(s)                        // straight to text with no tree; Json(s) builds the tree
Json.parse(text).to[Survey]                     // Ask[Survey]; every field that fails is reported, with its key
```

<!-- guide: codecgiven.kse3 -->
```scala
given Jsonize[java.time.Instant] = t => Jstr(t.toString)                                   // ISO-8601 text
given FromJson[java.time.Instant] = j => j.str.flatMap(s => nice{ java.time.Instant.parse(s) })   // a bad string is an Err
```

`FromJson[Double]` accepts `null` as `NaN` and the quoted names `"NaN"` and `"Infinity"`, since those are what
JavaScript and protobuf emit; `Option[Double]` still sees `null` as `None`.  `Int` fails on a value outside its range
rather than wrapping.

Full API: `jsaun/src/Jcodec.scala`; the givens for the standard types are in the two companions.

## Format-preserving edits

`Json.M.parseFmt(text)` parses into the editable hierarchy, `Jobj.M` and `Jarr.A.M`, and remembers where every token
sat in the input.  Edit through those types, reached by a type match: `o("k") = v` and `o.put(k, v)` replace or
append, and `add`, `insert`, `remove`, `sortKeys`, and `clear` do what they say.  On `print`, whatever was not
touched is copied from the input byte for byte, a replaced value re-serializes in its slot, and a structural edit
re-serializes just that node, with separators sampled from its own layout so a new entry matches its siblings.
`Json.parseFmt` is the read-only form, which prints back identically.  `reprint(style)` ignores every preserved
format and restyles; `compactFormat()` trades the retained source for each node's inferred layout, which frees the
text and keeps a regularly formatted file printing as it was.  The editing contract is by upcast: the `.M`
reference edits, the same object seen as `Json` doesn't, and nothing is ever copied, so drop the `.M` reference
once the view is handed off.

**Reach for these when** you'd read a config file, change one value, and write it back reformatted and reordered,
or when a JSON file is under version control and the diff should show only the change.

<!-- guide: edit.kse3 -->
```scala
val o = Json.M.parseFmt(text).json_? match      // a mutable tree that remembers where every token sat
  case o: Jobj.M => o
  case _ => Err ?# "expected an object at the top"
o("count") = Jnum(o("count").longOr(0) + 1)     // a value edit: only this token changes on output
o("checked") = Jbool(true)                      // absent, so appended, with separators copied from the object's own layout
o.print                                         // everything untouched is byte for byte the input
```

`Json.M.parse` gives the editable tree without the formatting memory, for building or rewriting freely; a
`Jobj.M()` or `Jarr.A.M()` starts empty.  A subtree parsed from one document can be placed in another and keeps
its own formatting.  A scalar at the very top of a document is re-serialized on print, since spans belong to
containers.

Full API: `Jobj.M`, `Jarr.A.M`, and `Jarr.D.M` in `jsaun/src/Json.scala`; `jsaun/src/Jfmt.scala` for what is
retained.

## Streaming with Jvisitor and Jbuilder

`Json.stream(in)(visitor)` walks the input once and builds nothing.  A `Jvisitor` receives `objStart`, `key`,
`index`, the leaf values with numbers as `Long` or `Double`, and `objEnd` and `arrEnd`; answering `false` from
`key`, `index`, `objStart`, or `arrStart` skips that value structurally, matching brackets without decoding, so a
visitor after one field of a large document pays for one field.  `Json.build(in)(builder)` is the same walk with a
result: a `Jbuilder[B, A]` makes its working state with `zero()`, answers a `Jexpect` per key or index (`Skip`,
`Value`, or a required form such as `D`, `L`, `Str`, or `Arr`, which the walker checks and reports with position
and key when it doesn't hold), receives leaves together with the state, and finishes with `build(b)`, an `Ask[A]`.
Both take every input `Json.parse` does, so a stream of any size is read in bounded memory.

**Reach for these when** the document is large and you need a little of it, or when decoding must not allocate.

<!-- guide: stream.kse3 -->
```scala
var deepest = Double.NaN
val v = new Jvisitor {
  override def key(k: String) = k == "stations" || k == "depth"   // false skips the value without decoding it
  override def num(d: Double) = if !(d <= deepest) then deepest = d
  override def num(l: Long) = num(l.toDouble)
}
Json.stream(in)(v).map(_ => deepest)            // one pass through the stream, no tree, numbers unboxed
```

<!-- guide: builder.kse3 -->
```scala
class Depths { var sum = 0.0; var n = 0 }
object MeanDepth extends Jbuilder[Depths, Double] {   // a stateless recipe; zero() makes the state for one walk
  def zero() = new Depths
  override def key(b: Depths, k: String) = k match
    case "stations" => Jexpect.Arr                // must be an array, which is then visited
    case "depth" => Jexpect.D                     // must be a number, delivered to num(b, d)
    case _ => Jexpect.Skip                        // stepped over, undecoded
  override def num(b: Depths, d: Double) = { b.sum += d; b.n += 1; Is.unit }
  def build(b: Depths): Ask[Double] = if b.n == 0 then Err.or("no stations") else Is(b.sum / b.n)
}
```

<!-- guide: builduse.kse3 -->
```scala
Json.build(text)(MeanDepth)                     // a wrong form under "depth" fails with its position and key
```

The leaf callbacks of a builder answer `Ask[Unit]`, so a value that is well-formed but wrong can be refused as it
arrives, and the walk stops there with the position.  Neither walk offers exact numbers or format preservation:
consumed input can't be revisited.

Full API: `jsaun/src/Jvisit.scala`; the `stream` and `build` overloads are in `object Json`.

## Numbers and packed arrays

A number parses as `Jnum.L` when it is an integer a Long can hold and as `Jnum.D`, correctly rounded, otherwise;
`long` fails on a fraction or a Double past the Long range, and `dbl` never fails on a number.  With `exact = true`,
a number a Double cannot represent is kept as `Jnum.Big` with its original text and prints back unchanged.  An
array of numbers parses packed as a `Jarr.D` of unboxed Doubles, so `dbls` hands over the `Array[Double]` in one
copy and `to[Array[Double]]` or `to[List[Double]]` reads it without boxing; `Jarr(Array[Double])`,
`Jarr(Array[Float])`, and `Jarr(Array[Int])` build packed arrays.  A non-finite value inside a packed array prints
as the quoted name, `"NaN"` or `"Infinity"` or `"-Infinity"`, which is valid JSON and is read back as a number by
`dbls` and `to[Double]`, as is `null`.  The `Jarr.Pack` given chooses how eagerly a numeric-looking array packs.

**Reach for these when** the JSON holds a matrix or a time series and boxing every element would be the cost.

<!-- guide: numbers.kse3 -->
```scala
val n = Json.parse("42").long                   // Ask[Long]: a whole number a Long can hold is a Jnum.L
val d = Json.parse("42").dbl                    // Ask[Double]: every number reads as a Double
val kept = Json.parse("0.30000000000000001", exact = true).jsonOr(Jnull).print   // exact keeps what a Double can't
val lost = Json.parse("0.30000000000000001").jsonOr(Jnull).print                 // "0.3": the nearest Double, shortest
val nan = Json.parse("NaN").dbl                 // the one extension to the standard: NaN and the infinities round-trip
val xs = Json.parse("[1.5, 2.5, 3.5]").arr.flatMap(_.dbls)   // an Array[Double] straight out of a packed Jarr.D
```

A mutable-mode parse never packs, so an edited numeric array is a `Jarr.A.M` of `Jnum` nodes.

Full API: `Jnum` and `Jarr` in `jsaun/src/Json.scala`, with `Jarr.Pack` explained at its definition.
