// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

package kse

/**
  * KSE Basics provides high-level and highly efficient extensions for basic operations
  * and mutable and immutable functionality for arrays (also duplicated on `String`).
  * 
  * Because KSE Basics makes very heavy use of extension methods and opaque types,
  * and ScalaDoc does not gather extensions in a particularly usable way, the documentation
  * is more awkward to traverse than in a more traditionally structured library.
  * 
  * If the examples below do not cover your use cases, please check the test suite in the
  * repository.  Practically everything that can be done is tested there, albeit briefly.
  * 
  * == Working with Opaque Types ==
  * 
  * `NewType` functionality with opaque types is provided via a trait intended for use on a companion object.
  * 
  * {{{
  * object Name extends NewType[String] {
  *   extension (n: Name.Type)
  *     def length: Int = n.value.length
  * }
  * println(Name("salmon").length)  // Prints 6
  * }}}
  * 
  * The name of the type is Name.Type.  Create values with Name(x).  Get the underlying value with n.value.
  * Define your own extensions inside the object.  That's about it!
  * 
  * `Translucent[O, I]` witnesses that an opaque type `O` is backed by an implementing type `I`.
  * This can be used to enable copying of arrays, for example (see below).  You get it for free if you
  * use `NewType`.  An alternative `.Type`-free pattern for opaque types works too:
  * 
  * {{{
  * opaque type Title = String
  * object Title extends Translucent[Title, String] {
  *   inline def wrap(s: String): Title = s
  *   extension (t: Title)
  *     inline def unwrap: String = t
  * }
  * }}}
  * 
  * == Copying Things ==
  * 
  * kse.flow defines a typeclass `Copies` that enables an extension method `.copy` on any class `A`
  * for which there is a given `Copies[A]`.  If you import givens, you get `Copies` for array types
  * (the primitives and AnyRef).  Support for arrays of opaques is based on Translucent.
  * (see source or test suite).
  * 
  * == Break and Continue Equivalents ==
  * 
  * By using `scala.util.boundary` functionality, one can implement break-and-continue-like shortcuts.
  * `shortcut.outer:` defines a point equivalent to a break (customarily one would place this outside
  * a loop); you can stop execution and return from the `shortcut.quittable:` block--which must be side-effecting
  * because it only returns `Unit`--by using `shortcut.quit`, or conditionally using `shortcut.quitIf`.
  * `shortcut.skippable:` would generally go inside a loop, and can be exited by using `shortcut.skip` and
  * `shortcut.skipIf`.
  * 
  * {{{
  * var i = 0
  * shortcut.quittable:
  *   while i < 100 do
  *     shortcut.skippable:
  *       if i % 2 == 0 then shortcut.skip
  *       println(i)
  *     if i.toString.length > 1 then shortcut.quit
  *     i += 1
  * // This prints numbers from 0 to 9
  * }}}
  * 
  * If you want to abstract the functionality, use a context function argument `inline f: boundary.Label[shortcut.Type] ?=> ...`
  * (generally you want it inline so it can be implemented as a jump instead of throwing a stackless exception), and within
  * your code use `shortcut.outer:` and `shortcut.inner:` for the points to quit and skip to, respectively.  If you only
  * need skips or quits, use `shortcut.Skips.type` or `shortcut.Quits.type` instead of `shortcut.Type`.
  * 
  * == Simple Intervals ==
  * 
  * The `kse.basics.intervals` packages contains two simple intervals: `Iv` which is an absolute interval, and `PIv` which is
  * a position-relative interval.  Create `Iv` itervals with `Iv(i0, iN)` where the interval is normally interpreted as exclusive
  * at the endpoint (`i0` is the first element, `iN` is one after the last element).  You can traverse these values using `iv.visit`,
  * get a string representation using `iv.pr`, and get the start and stop indices with `iv.i0` and `iv.iN`.  `PIv`-style intervals
  * are only relative to the end of an `Array` or `String`, and are created with the syntax `1 to End` or `1 to End-3` and such.
  * These can be converted into specific `Iv` intervals using `piv of a` where `a` is an `Array` or `String`.
  * 
  * Scala range literals of the form `1 to 3` or `5 until 9` can also be converted into `Iv`-style intervals with `Iv.of(1 to 3)`.
  * `Iv` can also convert type unions of `Iv | PIv` to definitively `Iv` using `Iv.of(v, a)` where `v: Iv | PIv` and `a` is an
  * `Array` or `String`.
  * 
  * This enables easy length-relative creation of intervals.  For instance, `1 to End-1` is the interval that leaves off the first
  * and last elements of an `Array` or `String`.
  * 
  * == Standard Inline-Style Utility Methods ==
  * 
  * The Scala standard library defines `tap`, which allows operating on a value in-line, and `pipe`, which allows
  * changing a value.  Unfortunately, these are regular methods, which have substantial overhead.
  * 
  * `kse.basics` redefines these as inline, with `fn` as a shorter alternative for `pipe`, allowing in-line data
  * flow with no loss of speed.
  * 
  * {{{
  * var s = 0
  * var n = 0
  * var i = 0
  * while i < 10 do
  *   s += (i * i).tap(sq => if sq > 10 then n += 1)
  *   i += 1
  * }}}
  * 
  * You can also patch up a value with a test and a fix operation using `fixIf`, e.g. `x.fixIf(_ < 0)(- _)` 
  * computes the absolute value of `x`.
  * 
  * == Tuple, Mutability, and Visibility Helpers ===
  * 
  * A variety of tuple helpers are specified in `Data.scala`.  Check them out!  You can join tuples with `.join`,
  * create simple mutable boxes with `Mu(x)`, hide identity with `Anon`, and more!
  * 
  * == Full-Powered Arrays ==
  * 
  * The Scala standard library abstracts its collection operations over arrays, but they are slow,
  * not very suited for arrays specifically, and focused on immutable operations.
  * 
  * `kse.basics` embraces arrays, including arrays of primitives, with normal or opaque types, and
  * gives them the kind of powerhouse functionality one might expect from R data frames and tables
  * or from Python arrays.
  * 
  * Most operations can be done on a whole array (generally indicated with `()`), an interval (`Iv` or other),
  * at selected indices (with indices given either in an `Array` or in an `IntStepper`--use `.stepper` on any
  * Scala collection containing `Int`s), or sometimes at places indicated by a predicate.
  * 
  * For safe conditional access to individual array elements, `use(i)(A => Unit)` allows you to operate on an array
  * value, or do nothing if the index is out of range.  `zap(i)(A => A)` allows you to alter a value if in range.
  * 
  * `where` finds all indices matching a predicate.  For instance, `xs.where(_ < 0)` would list all indices with
  * negative values.  `whereIn` finds only indices within a range (e.g. `xs.whereIn(5 to End)(_ % 2 == 0)`).  If
  * you already have indices but want to rule in a subset, use `xs.whereFrom(indices)(_ startsWith "A")`.
  * 
  * You can copy arrays with `dup()`, no matter the type.  Want to modify the array in-line before returning it?
  * use `dup(a => ...)`--it's just like `dup().tap(a => ...)` but shorter.  Want to duplicate the array but apply
  * a function (just like `map`)?  Use `copyWith(x => y)`--no boxing applies.  Need to know the index, too?  Use `copyOp((x, i) => ...)`.
  * 
  * Want your arrays to be smaller, if they're too big?  `shrinkTo(size)`.  Add elements at the end?  `xs.addRight(5)`.
  * Want to set them?  `xs.addRight(5, "eel")`.  Want to set them variably?  `xs.addRight(5, () => randomFish())`.
  * Want to add elements on the left?  `addLeft`.
  * 
  * To access the values of an array, just use `peek` (it returns the array, so can be used inline).  It works with ranges,
  * too, so you can `xs.peek(1 to 3)(println)`.  Need to change the values?  Just `poke`: `xs.poke(1 to 3)(i => i+1)`.
  * 
  * Values not enough, you need indices too?  `visit`.  Need to change the contents based on value and index?  `edit`.
  * 
  * Maybe you want to zero your array, or set it to some constant.  `xs() = ""`.  Copy in the contents of another array?
  * Sure thing:  `xs() = ys`.  Just part of it?  No problem `xs(_ < 0) = 0`.  `xs(5 to End) = ys`.
  * 
  * Want to set some or all of your array to values based on index?  `xs.set(2 to 3)(i => "salmon".substring(i))`.  Just
  * set them with a generator?  `xs.set(2 to 3)(() => randomFish())`.  Want to stick your array into another?  `xs.inject(ys)`
  * and it will tell you how many elements you copied as a result.  Need to only copy some, and start not at the beginning?
  * No problem!  `xs.inject(ys, 5)(_ > 0)`.  Need to change the type, too?  `xs.injectOp(ys, 5)((x, i) => ...)`.
  * 
  * Do you need a copy of part of your array?  `xs.select(_ > 5)`, `xs.select(3 to End-5)`.  Need to change the type along
  * the way, or do something based on index?  `xs.selectOp(3 to End-5)((x, i) => ...)`.
  * 
  * There's a fast left-to-right fold called `gather`: `xs.gather(0L)((acc, x, i) => ...)`.  You can chop arrays up by predicate
  * or index: `xs.diced(Array(3, 10))` and control whether the boundary-elements are included or not: `xs.diced(_ == ',', "(]")`
  * splits a string at `,`s, including the commas (`"(]"` means when you start, don't include your delimiter, but when you end, 
  * grab it).
  * 
  * And there's an advanced random-jump traversal called `wander` and a generalization of `flatMap` called `fuse`.
  * 
  * Still not enough?  Worried about falling out of bounds?  Every relevant operation can have `.clip` prepended to force indices
  * to stay in bounds (and prevent overflow).  For example, `xs.clip.poke(3, 10)(_ + 1)` will only update those elements that
  * actually exist.
  * 
  * But what if you want to stop in the middle of some operation but still want your return value?  No worries!
  * Prepend `.breakable` and you can use `shortcut.quit` to terminate early inside your lambda, returning any
  * partial results you might have found.  And if that might go out of bounds? `.breakable.clip`.
  * Or `.clip.breakable`.  Spell it however you like.
  * 
  * And, okay, `String` _is_ immutable, but a fair number of these things don't require mutability.  Wouldn't it be nice if...yep!
  * It all exists on `String` too.  (Even including clipping and breakability.)
  * 
  * All of this is as fast as hand-rolled code because it's all implemented `inline`.  This does bring up _one_ caveat: don't
  * spam high-powered array operations, nested, all over your code, for no reason.  You'll get code bloat.  So use it when you
  * need it, and keep your other methods small as is good practice ordinarily.
  * 
  * If you use Scala, and you use `Array`s, `kse.basics` is what you want.
  * 
  */
package object basics {}