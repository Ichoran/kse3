# kse3 basics

The basics module is the ground floor of kse3: things you reach for in nearly every file.  Most of it is extension
methods on types you already have (`Int`, `Array`, `String`, tuples), plus a few small new types (`Iv`, `Mu`, `Atom`,
`Mem`).  Everything on a hot path is `inline`, so the convenience costs nothing at runtime.

`import kse.basics.{given, *}` brings in the extensions.  `import kse.basics.intervals.*` adds `Iv`, `Start`, and `End`,
and `import kse.basics.labels.*` adds the `\ "label"` syntax.

<!-- guide examples: basics/test/src/GuideExamples.scala -->
Every code example below is compiled and run from `basics/test/src/GuideExamples.scala`; `mill guides.run` verifies
that the two copies are identical.

## What's here

- **Inline glue**: `__ Unit` to discard a value on purpose; `tap`, `fn`, `fixIf` to work on a value without naming it.
- **Arrays with ranges**: `use`, `alter`, `visit`, `edit`, `select`, `where`, `inject`, each over everything, a range, some indices, or a predicate.
- **Copying, resizing, and chopping arrays**: `dup`, `copyWith`, `addRight`, `gather`, `diced`, and bulk assignment `xs(range) = value`.
- **Clip, flex, and fancy**: ignore out-of-range indices, quit a traversal early, or both.
- **Intervals**: `Iv`, `Start`/`End`, Python-style `ꓺ` (or `` `..` ``), and accepting any of them in your own method.
- **Counting and traversing**: `n.times`, `n.visit`, `n.unfold`, `iv.visit` in place of a hand-written loop.
- **Mu and Atom**: mutable cells and atomics with one small API; `Atom.Count` and `Atom.Toggle` for the common concurrent cases.
- **Break and continue**: `shortcut.quittable`, `skippable`, `quit_?`, `skip_?`, compiled to jumps.
- **Strings**: the `say` interpolator with plurals and readable arrays, `MkStr` for building, `dedent` and `demargin` for text blocks.
- **Sorting**: stable index sorts with the comparison inlined; `indicesInOrder`, `sortInOrder`, `reorder`, `rankOrder`, and orders of your own.
- **Opaque types**: `NewType` for a zero-cost wrapper with its own methods; `Translucent` and `Copies` let generic code see through it.
- **Labels**: `x \ "name"`, a named argument the caller can't leave off.
- **Tuples, named tuples, and lenses**: `ops`, `merge`, `tup`, `lens[i].to(x)`, and a bridge between named tuples and labels.
- **Mem**: off-heap primitive arrays with the array verbs; `Mem.As` for opaque types, `Mem.AoS` for packed records, chosen byte order, `Mem.Atom`.
- **Exceptions**: `t.explain()` for a compact trace, `t.catchable` for which throwables to let through.
- **Writing tests**: `T ~ x ==== y`, `=**=`, `=~~=`, `thrown[E]`, `T ! "code"`, and the `Asserter` boilerplate.

## Inline glue

kse3 leans on a handful of tiny inline helpers that let you act on a value where it is, rather than naming it first.
They turn up in almost every file, so they come first.

**Reach for these when** you're about to write `val _ = ...` to silence a warning, `{ f(x); x }`, or a temporary `val`
whose only job is to feed the next expression.

Plain Scala:

<!-- guide: glue.plain -->
```scala
val _ = set.add(x)
val y = x * 2 - 7
record(y)
if y < 0 then -y else y
```

kse3:

<!-- guide: glue.kse3 -->
```scala
set.add(x) __ Unit
(x * 2 - 7).tap(record).fixIf(_ < 0)(- _)
```

`__ Unit` says out loud that a result is being thrown away.  kse3 compiles with the non-Unit-statement warning on, and
this is the sanctioned way to satisfy it.  (`__` by itself is an inline identity function, for where a function value is
required.)  `tap` runs a side effect and hands the value back; `fixIf(test)(fix)` patches a value that fails a test.
The standard library has `tap` and `pipe` too, but as ordinary methods that allocate a closure; these don't.

A few relatives: `x.fn(f)` applies `f` to `x` (that's `pipe`, except that a tuple is spread across `f`'s arguments:
`(a, b).fn(_ + _)`); `x.fixWhile(test)(fix)` keeps fixing until the test passes; `x.cycle(n)(f)` applies `f` n times;
and `x |-> (f, b, c)` calls `f(x, b, c)`, R-style, for when the thing you have is the first argument of the method you
want.

Full API: `basics/src/Datum.scala`, the `extension [A](a: A)` block near the end.

## Arrays with ranges

Almost every array operation in kse3 can run over the whole array (`()`), a range, a list of indices, or the elements matching
a predicate.  Ranges may be measured from the end: `1 to End-1` is everything but the first and last element.  `String`
gets the same operations.

**Reach for these when** you're about to write `xs.slice(i, xs.length - k)`, a `while` over part of an array, or
`zipWithIndex.filter(...).map(_._2)` to find indices.

Plain Scala:

<!-- guide: arrays.plain -->
```scala
val inner = xs.slice(1, xs.length - 1)
val negs = xs.zipWithIndex.collect{ case (x, i) if x < 0 => i }
var i = 2
while i < xs.length - 2 do
  xs(i) += 1
  i += 1
```

kse3:

<!-- guide: arrays.kse3 -->
```scala
val inner = xs.select(1 to End-1)
val negs = xs.where(_ < 0)
xs.alter(2 to End-2)(_ + 1)
```

The kse3 form is as fast as the hand-written loop, because that is what it inlines to.  There is no boxing and no
intermediate collection, and the length arithmetic lives in the range rather than at each use.

The verbs follow one pattern.  `use` reads values, `alter` rewrites them, `visit` reads with the index, `edit` rewrites
with the index; `select` copies out, `where` finds indices, `inject` copies in.  Each accepts the same four kinds of
target.  Prefix `.clip` to have out-of-range indices ignored instead of throwing (more on that below).

One caution: each call inlines a loop, so a method with a dozen nested array operations gets big.  Use them where they
clarify and keep methods small, as usual.

Full API: `basics/src/Data.scala`, the `extension [A](a: Array[A])` block; the `String` twin follows it.

## Copying, resizing, and chopping arrays

The standard library's array operations go through `ArrayOps`, which boxes primitives and builds intermediate
collections.  kse3 gives arrays their own versions, `inline` throughout, with the same names where the meaning is the
same.

**Reach for these when** you'd write `xs.clone`, `xs.map`, `xs ++ ...`, `foldLeft`, or a `while` loop that only
accumulates.

Plain Scala:

<!-- guide: copying.plain -->
```scala
val ys = xs.clone
val halved = xs.map(_ / 2.0)
val padded = xs ++ Array.fill(2)(-1)
var sum = 0L
var i = 0
while i < xs.length do
  sum += xs(i)
  i += 1
```

kse3:

<!-- guide: copying.kse3 -->
```scala
val ys = xs.dup()
val halved = xs.copyWith(_ / 2.0)
val padded = xs.addRight(2, -1)
val sum = xs.gather(0L)()((acc, x, _) => acc + x)
```

`dup(f)` copies and then runs `f` on the copy, for a modified duplicate in one step.  `copyOp` is `copyWith` with the
index too.  `addLeft` and `addRight` grow an array by `n` slots, blank, constant, or generated; `shrinkTo` and
`enlargeTo` resize.  `gather` is a left fold that also sees the index, and it takes the same range, indices, or
predicate targets as the other verbs.

Bulk assignment reads like ordinary indexing:

<!-- guide: assigning.kse3 -->
```scala
xs() = 0                      // every element
xs(2 to End) = ys             // ys copied in, starting at index 2
xs(_ == 0) = -1               // every element that is still zero
xs.set(0 to 1)(i => i * 10)   // computed from the index
```

To cut an array into pieces, `diced` splits at given indices or wherever a predicate holds.  By default the elements
that matched are dropped; a mode string such as `"(]"` keeps them on one side or the other.

<!-- guide: chopping.kse3 -->
```scala
val words = "eel,cod,gar".arr.diced(_ == ',')
val runs = Array(1, 2, 0, 3, 0, 4).diced(_ == 0)
```

That gives three `Array[Char]`s with no commas, and `Array(Array(1, 2), Array(3), Array(4))`.  `fuse` (a `flatMap`
that hands you an `add` function) and `wander` (a traversal that chooses its next index) cover rarer needs; see the
source.

Full API: `basics/src/Data.scala`, same block as above.  `ArrayReform` in the same file converts between primitive
array types.

## Clip, flex, and fancy

Three prefixes change how the array verbs behave.  `.clip` makes every index-taking operation ignore indices that are
out of range rather than throw.  `.flex` lets the lambda you pass stop the traversal early with
`shortcut.quit_?(condition)`.  `.fancy` does both.  `String` has all three as well.

**Reach for these when** you'd write a bounds check before an access, or a `while` loop whose only reason to be a
`while` is that it stops early.

Plain Scala:

<!-- guide: modes.plain -->
```scala
val v = if i >= 0 && i < xs.length then xs(i) else -1
var sum = 0
var j = 0
while j < xs.length && xs(j) >= 0 do
  sum += xs(j)
  j += 1
```

kse3:

<!-- guide: modes.kse3 -->
```scala
val v = xs.clip(i)(-1)
var sum = 0
xs.flex.use(): x =>
  shortcut.quit_?(x < 0)
  sum += x
```

With `.clip`, a range that overhangs the array is trimmed to fit: `xs.clip.alter(5 to 100)(_ + 1)` touches only the
elements that exist, and `xs.clip.get(i)` gives an `Option`.  The early exit in `.flex` is a real jump, not an
exception, so it costs nothing on the path that doesn't take it.

Full API: `basics/src/Data.scala`, `ClippedArray`, `FlexArray`, and `FancyArray`.

## Intervals

An `Iv` is a half-open index interval packed into one `Long`: `Iv(2, 5)` covers indices 2, 3, and 4.  You'll mostly
meet intervals as arguments to array verbs, where a Scala range literal (`2 to 4`, `2 until 5`) or a length-relative
one (`1 to End-1`, `Start+2 to End`) does the same job without allocating.  `End` is the last index, so `1 to End-1`
drops the first and last elements.  `ꓺ` gives Python-style negative indexing, exclusive at the end: `1 ꓺ -1` is that
same interval.  Since `ꓺ` is not on any keyboard, `` `..` `` (backticks included) is the same operator.

**Reach for these when** you're passing `(i0, iN)` pairs around, doing `xs.length - k` arithmetic at call sites, or
want one method to accept any way of naming a range.

<!-- guide: intervals.kse3 -->
```scala
val iv = Iv(2, 5)                    // indices 2, 3, 4: i0 is included, iN is not
val inner = (1 to End-1).of(xs)      // resolved against xs.length into an Iv
val same = (1 ꓺ -1).of(xs)           // Python-style: the same interval
val typed = (1 `..` -1).of(xs)       // the same again, from an ordinary keyboard
val both = Iv(0, 4) & Iv(2, 8)       // Iv(2, 4); | is union
```

Writing a method that accepts a range literal, an `Iv`, or a relative interval takes one line with `Iv.dispatch`,
which turns whatever it was given into `(i0, iN)`:

Plain Scala:

<!-- guide: dispatch.plain -->
```scala
def middlePlain(s: String, i0: Int, iN: Int): String = s.substring(i0, iN)
```

kse3:

<!-- guide: dispatch.kse3 -->
```scala
inline def middle[R <: Iv.X | Range](s: String, inline r: R): String =
  Iv.dispatch(r, s)((i0, iN) => s.substring(i0, iN))
```

Now `middle("salmon", 1 to End-1)` and `middle("salmon", 2 to 4)` both work, and a range literal is packed at compile
time.  The method and its `r` parameter must both be `inline` for that to happen, and the second argument to `dispatch`
(an array, string, length, or `Iv`) is what gives `End` its meaning.

`Iv` has `i0`, `iN`, `length`, `contains`, `visit`, `where()`, and `clippedTo(xs)`; shift it with `+#` and `-#`.
Intersection is `&`, union is `|` (filling any gap between the two).

Full API: `basics/src/Intervals.scala` for `Iv` and the relative flavours; `Iv.dispatch` and `.of` are in
`basics/src/Datum.scala`.

## Counting and traversing

Most hand-written loops just count to `n` or walk a range.  kse3 puts the loop in an inline method so the body is all
that is left to write.

**Reach for these when** you're typing `var i = 0; while i < n do`, `for i <- 0 until n`, or `Array.tabulate`.

Plain Scala:

<!-- guide: counting.plain -->
```scala
var i = 0
while i < n do
  record(i)
  i += 1
val squares = Array.tabulate(n)(j => j * j)
val countdown = (n - 1 to 0 by -1).toArray
```

kse3:

<!-- guide: counting.kse3 -->
```scala
n.visit(record)
val squares = n.unfold(j => j * j)
val countdown = n.whereBy(-1)
```

`n.times(body)` when the index isn't needed; `n.visit(i => ...)` when it is; `n.visitBy(step)` for strides, negative
to count down.  `n.where()` is the array `0, 1, ..., n-1`, and `n.unfold(f)` fills a new array from the index.  An `Iv`
has the same `visit`, `where()`, and `unfold`, so `Iv(2, 5).visit(f)` and `(1 to End-1).of(xs).visit(f)` walk just that
stretch.  `Long` has `times`, `visit`, and `visitBy` too.

Full API: `basics/src/Data.scala`, the `extension (i: Int)` block at the top, and `Iv` in `basics/src/Intervals.scala`.

## Mu and Atom

A `Mu` is a mutable cell: `Mu(0)` holds an `Int`, `Mu(0L)` a `Long`, and so on, one class per primitive so nothing is
boxed.  `Atom` is the same idea with atomic operations, over `java.util.concurrent.atomic` underneath.  Both read with
`()`, assign with `:=`, and update with `zap(f)`; `Int` and `Long` cells also have `++`, `--`, `+=`, and `-=`.

**Reach for these when** you'd capture a `var` in a closure (the compiler quietly boxes it into an `IntRef`), pass a
one-element array around as a mutable slot, or spell out `AtomicInteger`, `AtomicLong`, and `AtomicBoolean` calls by
hand.

Plain Scala:

<!-- guide: cells.plain -->
```scala
var n = 0
xs.foreach(x => if x > 0 then n += 1)
val total = new java.util.concurrent.atomic.AtomicLong(0)
xs.foreach(x => total.addAndGet(x): Unit)
```

kse3:

<!-- guide: cells.kse3 -->
```scala
val n = Mu(0)
xs.foreach(x => if x > 0 then n.++)
val total = Atom(0L)
xs.foreach(x => total += x)
```

Afterwards `n()` and `total()` read the values.  `Mu` also holds an opaque type over a primitive without boxing, and
`Mu(anything)` works for references.  `Atom` follows the same rule and refuses at compile time anything it can't make
atomic.  For counters that many threads bump, `Atom.Count` is a `LongAdder`; `Atom.Toggle` is a flag whose `turnOn()`
and `turnOff()` report whether this call was the one that flipped it; and `Atom.Loan` runs an at-most-once
compute-and-swap.

<!-- guide: once.kse3 -->
```scala
val hits = Atom.Count()                 // a LongAdder: many threads can bump it cheaply
val ready = Atom.Toggle()
4.times:
  hits.++
  if ready.turnOn() then initialize()   // only the first caller sees true
```

Full API: `basics/src/Datum.scala`, `Mu` and `Atom`.

## Break and continue

Scala has no `break` or `continue`.  `shortcut` supplies both, built on `boundary` and `break` and inlined so they
compile to jumps rather than thrown exceptions.  `shortcut.quittable:` marks a block you can leave with
`shortcut.quit_?(condition)`; `shortcut.skippable:` marks one you can leave with `skip_?`.  A literal `true`
always jumps, so `quit_?(true)` is the unconditional form.  Put the first around a loop and the second inside it and you have `break` and `continue`.

**Reach for these when** you have a flag whose only job is to end a loop, or an `if`/`else` ladder that exists because
there's no way to say "not this one, next".

Plain Scala:

<!-- guide: shortcut.plain -->
```scala
var stop = false
var i = 0
while i < lines.length && !stop do
  val line = lines(i).trim
  if line == "END" then stop = true
  else if line.nonEmpty && !line.startsWith("#") then process(line)
  i += 1
```

kse3:

<!-- guide: shortcut.kse3 -->
```scala
shortcut.quittable:
  var i = 0
  while i < lines.length do
    shortcut.skippable:
      val line = lines(i).trim
      shortcut.quit_?(line == "END")
      shortcut.skip_?(line.isEmpty || line.startsWith("#"))
      process(line)
    i += 1
```

The kse3 version reads as a list of guard clauses.  Since the shortcuts are jumps, they cost nothing when not taken and
throw nothing when they are.  Array traversals give you the same without writing the block: `.flex` (see above) wraps
each lambda in a `quittable`.  If you write your own inline method that takes a body which should be able to quit or
skip, declare it as `inline f: boundary.Label[shortcut.Type] ?=> ...` and use `shortcut.outer:` and `shortcut.inner:`
for the two landing points.

Full API: `basics/src/Abstractions.scala`, `object shortcut`.

## Strings

`say"..."` is an interpolator that renders each argument through a `Sayable` typeclass chosen at compile time.  Arrays
print as `[1, 2, 3]` rather than `[I@1b2c3d`, a type can say how it prints by providing its own `Sayable`, and the
result is assembled by straight-line code with no varargs `Seq`.  It also handles plurals: right after an argument,
`#prefix/singular/plural#` picks a form by the count.  `MkStr` is a `StringBuilder` with `+=` for anything, and
`MkStr(m => ...)` builds and returns the string in one expression.

**Reach for these when** you'd write `s"..."` with an `if n == 1` inside it, `mkString` just to print an array, or
`new StringBuilder` and a chain of `append`s.

Plain Scala:

<!-- guide: strings.plain -->
```scala
val msg = s"Found $n file${if n == 1 then "" else "s"} in ${dirs.mkString("[", ", ", "]")}"
val sb = new java.lang.StringBuilder
sb.append(name).append(": ")
for x <- xs do sb.append(x).append(' ')
val line = sb.toString.trim
```

kse3:

<!-- guide: strings.kse3 -->
```scala
val msg = say"Found $n# file//s# in $dirs"
val line = MkStr: m =>
  m += name
  m += ": "
  xs.visit(): (x, i) =>
    if i > 0 then m += ' '
    m += x
```

`$n# file//s#` gives `file` or `files`.  The three fields are prefix, singular, and plural, so `$n# tr/y/ies#` gives
`try` or `tries`, and text before an argument can depend on it too: `#is/are/ <#$n`.  `silently(n)` pluralizes without
printing the number; `spoken(n)` prints it as a word (`kse.maths` supplies the instances).  `MkStr` indexes from the
end (`m(End)`, `m(End) = '!'`) and has `del`, `ins`, `alter`, `edit`, `reverse`, `repeat`, and `visit`.

For text blocks, `dedent()` removes the common leading whitespace, and `demargin()` removes a margin that you mark
with `|` on a first line of its own:

<!-- guide: margins.kse3 -->
```scala
val poem = """
  |
  Salmon swim upstream
    to spawn
  """.demargin()
val same = "  Salmon swim upstream\n    to spawn".dedent()
```

Both give `Salmon swim upstream` followed by `  to spawn`: the relative indent of the second line survives, and
`demargin` drops the newline before the closing quotes.

Full API: `basics/src/Say.scala`.

## Sorting

kse3 sorts are stable merge sorts whose comparison is inlined, so sorting an `Array[Int]` or an `Array[String]` runs
with no boxing and no virtual call per comparison.  The headline operation is an index sort: `indicesInOrder()` gives
the indices of an array in ascending order of its values without moving anything, which is what you need when one
array defines the order and others must follow it.

**Reach for these when** you'd write `zipWithIndex.sortBy(_._1).map(_._2)`, sort a copy just to learn an ordering, or
call `java.util.Arrays.sort` and then lose track of which element went where.

Plain Scala:

<!-- guide: sorting.plain -->
```scala
val order = ages.indices.sortBy(i => ages(i)).toArray
val byAge = order.map(i => names(i))
java.util.Arrays.sort(ages)
```

kse3:

<!-- guide: sorting.kse3 -->
```scala
val order = ages.indicesInOrder()
names.reorder(order)
ages.sortInOrder()
```

With `ages` of `31, 25, 40, 25`, `order` is `1, 3, 0, 2` (the two 25s keep their original order), `names` is
rearranged in place by walking the permutation's cycles, and `ages` ends up sorted.  `rankOrder()` is the inverse of
`indicesInOrder()`: each value's position in sorted order.  `sortWithIndices()` sorts in place and hands back where
each element came from.  All of these take a range, and each has an `...Into` variant that fills buffers you supply,
so repeated sorts allocate nothing.  Keys that don't compare with themselves, such as `NaN`, go last.

An order of your own is an object with an inline `leq` and one line that compiles the sort kernels for it:

<!-- guide: order.kse3 -->
```scala
object Descending extends Sorting.Total[Int] {
  inline def leq(a: Int, b: Int): Boolean = a >= b
  val kernels = build()
}
```

Pass it at a call site, or declare `given Descending.type = Descending` to make it the order for every `Array[Int]`
sort in that scope.  A `given` typed as plain `Sorting.Order[Int]` is rejected on purpose, since it would fall back to
a boxed call per comparison.

<!-- guide: descending.kse3 -->
```scala
xs.sortInOrder()(using Descending)
```

Use `Sorting.Partial` in place of `Total` when some keys fail `leq(k, k)`, as `NaN` does.  An opaque type's order
belongs in its companion.  `Mem` and `Mem.As` (below) share the whole API.

Full API: `basics/src/Sorting.scala`; its header comment has more worked examples.

## Opaque types

`NewType[A]` is the least-ceremony way to get a distinct type that is an `A` at runtime.  Extend it in an object:
`Meters.Type` is the type, `Meters(x)` makes one, `.value` (or `.unwrap`) gets the `Double` back, and extension methods
declared inside the object are its methods.  Unlike a value class it never boxes: not in an array, not in a generic
method, not in a `Mu`.

**Reach for these when** you'd write `case class Meters(value: Double) extends AnyVal`, or when two `Double` parameters
keep getting swapped and you want the compiler to tell them apart.

Plain Scala:

<!-- guide: newtype.plain -->
```scala
final case class MetersPlain(value: Double) extends AnyVal {
  def feet: Double = value * 3.28084
}
```

kse3:

<!-- guide: newtype.kse3 -->
```scala
object Meters extends NewType[Double] {
  extension (m: Type)
    inline def feet: Double = m.value * 3.28084
}
```

<!-- guide: newtypeuse.kse3 -->
```scala
val m = Meters(2.0)                 // a Double at runtime, a Meters.Type to the compiler
val cell = Mu(m)                    // a MuDouble, not a boxed cell, thanks to Translucent
cell.zap(x => Meters(x.value * 2))
```

`NewType` also supplies a `Translucent[Meters.Type, Double]`, the witness that lets generic kse3 code see through the
wrapper.  That is how `Mu(m)` picks the `Double` cell, how `Atom` does the same, and how `.copy` works on an
`Array[Meters.Type]`.  If you write an opaque type by hand instead, `extends Translucent.Companion[Name, Underlying]`
on its companion buys the same treatment.  `Copies[A]` is the typeclass behind `.copy`; instances exist for every
array type, and you can add one for a mutable class of your own.

Full API: `basics/src/Abstractions.scala`: `NewType`, `Translucent`, `Copies`.

## Labels

A label is a string literal attached to a type: `Double \ "lo"` is a `Double` that must be called `"lo"`.  Make one
with `x \ "lo"` and read it with `.unlabel`, or with `x ~ "lo"` if you'd like the name repeated at the point of use.
Nothing exists at runtime; the label lives only in the type.

**Reach for these when** a method takes two or three parameters of the same type and callers can swap them without
the compiler noticing.  Scala's named arguments help only if the caller remembers to use them; a label is a named
argument that can't be left off.

Plain Scala:

<!-- guide: labels.plain -->
```scala
def clampPlain(x: Double, lo: Double, hi: Double): Double =
  if x < lo then lo else if x > hi then hi else x
```

kse3:

<!-- guide: labels.kse3 -->
```scala
def clamp(x: Double, lo: Double \ "lo", hi: Double \ "hi"): Double =
  if x < lo.unlabel then lo.unlabel else if x > hi.unlabel then hi.unlabel else x
```

<!-- guide: labelsuse.kse3 -->
```scala
val a = clamp(15, 0.0 \ "lo", 10.0 \ "hi")   // 10
// clamp(15, 10.0 \ "hi", 0.0 \ "lo")        // refused at compile time: the labels don't match
```

`Double \> "lo"` is the variant that is also a subtype of `Double`, so it can be used directly as a number while still
being distinct as an argument; `\<` is the supertype variant.  Labels convert to and from one-field named tuples with
`.nt` and `.kv`, which is where the next section picks up.

Full API: `basics/src/Abstractions.scala` for the three label types, and `basics/src/Labels.scala` for the `\ "name"`
syntax, imported from `kse.basics.labels`.

## Tuples, named tuples, and lenses

Scala 3 tuples are handy value carriers with few operations of their own.  kse3 adds inline ones for tuples up to 22
wide: `ops` applies one function per slot, `sameOp` the same function to every slot, `merge` spreads the slots into a
function's arguments, `reduce` folds them, `tup` and `tupWith` append a slot, `join` concatenates two tuples, and `fn`
(from Inline glue) spreads a tuple across a function.  A lens picks one slot by index: `t.lens[1]` and then `.get`,
`.to(x)`, `.map(f)`, `.delete`, or `.insert(x)`.

**Reach for these when** you're writing `(t._1 + 1, t._2.toUpper)`, `(t._1, t._2, x)`, or `f(t._1, t._2)`.

<!-- guide: tuples.kse3 -->
```scala
val t = (1, 'a')
val u = t.ops(_ + 1, _.toUpper)             // (2, 'A'): one function per slot
val s = t.merge((n, c) => c.toString * n)   // "a": the slots become arguments
val w = t.tup(true)                         // (1, 'a', true)
val v = t.lens[0].to("one")                 // ("one", 'a')
```

Named tuples get a bridge to labels.  `pluck("name")` pulls one field out as a labeled value; `asLabeled` turns a
whole named tuple into a plain tuple of labeled values and `asNamed` goes back; `.nt` turns a labeled value into a
one-field named tuple.  And since Scala's named tuples have no `copy`, `NamesAndLabels.copyWithUpdateByName(t, u)`
makes one with the fields of `u` replaced, refusing at compile time if `u` names a field that `t` lacks.

<!-- guide: named.kse3 -->
```scala
val fish = (name = "eel", mass = 2.5)
val nm = fish.pluck("name")                 // String \ "name", a labeled value
val bigger = NamesAndLabels.copyWithUpdateByName(fish, (mass = 3.0))
val roundTrip = fish.asLabeled.asNamed      // via (String \ "name", Double \ "mass") and back
val nt = (3 \ "count").nt                  // (count = 3)
```

Full API: `basics/src/Tuples.scala` for the tuple operations, `basics/src/TupleLenses.scala` for lenses, and
`basics/src/Labels.scala` for the named-tuple bridge.

## Mem

`Mem[A]` is an array of primitives that lives off-heap, or wraps an on-heap array, built on the JDK's foreign memory
API.  It has the array verbs from earlier (`use`, `alter`, `visit`, `edit`, `select`, `where`, `inject`, `gather`,
`set`, bulk `update`, `.clip`, and the sorting API) with `Long` indices, plus typed byte access at any offset such as
`getI(8)` or `setD(16, x)`.  `Mem.alloc[A](n)` gives memory the GC reclaims; `Mem.of(array)` shares an array's
storage; `Mem.Owned` pairs memory with an `Arena` so that `close()` frees it when you say so.

**Reach for these when** you'd otherwise juggle `MemorySegment` and `ValueLayout` by hand, need more than two billion
elements, want memory to hand to native code or map from a file, or want records laid out contiguously.

Plain Scala:

<!-- guide: mem.plain -->
```scala
import java.lang.foreign.{Arena, ValueLayout}
val seg = Arena.ofAuto().allocate(4 * 8)
var i = 0L
while i < 4 do
  seg.setAtIndex(ValueLayout.JAVA_DOUBLE, i, i * 1.5)
  i += 1
seg.setAtIndex(ValueLayout.JAVA_DOUBLE, 1, 9.0)
var total = 0.0
i = 0
while i < 4 do
  total += seg.getAtIndex(ValueLayout.JAVA_DOUBLE, i)
  i += 1
val back = seg.toArray(ValueLayout.JAVA_DOUBLE)
```

kse3:

<!-- guide: mem.kse3 -->
```scala
val m = Mem.alloc[Double](4)             // off-heap, freed when the GC finds it unreachable
m.set()(i => i * 1.5)
m(1) = 9.0
val total = m.gather(0.0)()((acc, x, _) => acc + x)
val back = m.copyToArray()               // Array(0.0, 9.0, 3.0, 4.5)
```

Three views build on it.  `Mem.As[O]` is a `Mem` of an opaque type (`Mem.As.alloc[Meters.Type](n)`), with
`Translucent` resolving the layout.  `Mem.AoS[T]` is an array of packed records described by a named tuple type: each
field is read and written by name and index, and is also a strided column with the usual verbs.  `m.orderAware` reads
and writes in a byte order you pick with `import Mem.LE` or `Mem.BE`, for file and wire formats; there is
deliberately no default order.

<!-- guide: aos.kse3 -->
```scala
type Fish = (id: Int, mass: Double)
val fish = Mem.AoS.alloc[Fish](3)        // packed records, 12 bytes each, no padding
fish.id(0) = 7
fish.mass(0) = 2.5
fish.mass.set(i => i * 1.25)             // a whole column at once
```

`Mem.Struct` is a single record with the same field access, and `Mem.Atom` is a `Mem[Int]` or `Mem[Long]` with
atomic operations at each index.  Everything here is inline and dispatches on `A` at compile time, so `A` must be a
concrete primitive wherever you use it.

Full API: `basics/src/Mem.scala`; the sorting extensions for `Mem`, `Mem.As`, and `Mem.AoS` are in
`basics/src/Sorting.scala`.

## Exceptions

`t.explain()` renders a throwable as text: the message, the frames, then each cause and suppressed exception, one line
each and indented, with a cap on lines per exception if you ask for one.  `t.catchable` is false for the throwables
you should let through: VM errors, `InterruptedException`, linkage errors, and control-flow throwables such as
`boundary`'s `Break`.  `threadCatchable` is the looser test for a thread's top-level handler.

**Reach for these when** you'd write `catch { case e: Throwable => ... }` (test `catchable` first so the fatal ones
get by), or `printStackTrace` into a `StringWriter` just to log the thing.

<!-- guide: explain.kse3 -->
```scala
val report = t.explain()                 // message, frames, causes and suppressed, one line each
val brief = t.explain(lines = 3)         // the first three lines of each
val safe = t.catchable                   // false for VM errors like OutOfMemoryError
```

Full API: `basics/src/Exceptions.scala`; `ExceptionExplainer` there is the formatter behind `explain`.

## Writing tests

kse3's tests are JUnit 4 plus a small assertion language from `kse.basics.testutilities.TestUtilities`, which ships
in the `basics` jar so your own tests can use it too.  Every assertion starts `T ~ value` and continues with an
operator: `====` for equality, `=**=` for element-by-element equality of anything iterable (arrays included), `=~~=`
for approximate equality of doubles and floats, `=!!=` for inequality, and `==== thrown[E]` for an expected exception.
A failure message names the source line.

**Reach for these when** you write a test in this repository, or want assertions that read like specifications.

Plain JUnit:

<!-- guide: tests.plain -->
```scala
assertEquals("almo", "salmon".substring(1, 5))
assertArrayEquals(Array(1, 2, 0), Array(3, 1, 2).indices.sortBy(i => Array(3, 1, 2)(i)).toArray)
assertEquals(0.333333333, 1.0 / 3, 1e-9)
val e = assertThrows(classOf[ArrayIndexOutOfBoundsException], () => Array(1, 2)(5): Unit)
assertEquals("Index 5 out of bounds for length 2", e.getMessage)
```

kse3:

<!-- guide: tests.kse3 -->
```scala
T ~ "salmon".select(1 to End-1)      ==== "almo"
T ~ Array(3, 1, 2).indicesInOrder()  =**= Array(1, 2, 0)
T ~ (1.0 / 3)                        =~~= 0.333333333
T ~ Array(1, 2)(5)                   ==== thrown[ArrayIndexOutOfBoundsException]
T ~ Iv(2, 5).length                  ==== 3 --: typed[Int]
T("labels must match") ! """clamp(15, 10.0 \ "hi", 0.0 \ "lo")"""
T("labels must match") \ """clamp(15, 0.0 \ "lo", 10.0 \ "hi")"""
```

`--: typed[T]` checks the static type along with the value.  `T ! """code"""` asserts that the code does not compile
and `T \ """code"""` that it does.  Only class members, constructors, and imports are visible inside such a string,
never method-local vals, so an assertion that names a local passes vacuously: always pair a `!` with a `\` twin of the
same shape to prove the shape compiles.  `T("message") ~ ...` labels an assertion.  Note that `~` binds tighter than
arithmetic, hence the parentheses around `1.0 / 3`.  The `given Asserter` below wires the operators to JUnit's
assertions and goes at the top of each test class:

<!-- guide: tests.boilerplate -->
```scala
given Asserter(
  (m, test, x) => assertEquals(m, x, test),
  (m, test, x) => assertNotEquals(m, x, test),
  assertTrue
)
```

Full API: `basics/src/TestUtilities.scala`; the test suites of every module are the examples.
