// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


import kse.flow.{given, _}


/** A SAX-style callback interface for `Json.stream`: the parser walks the document once and
  * calls these methods as it goes, building no tree and boxing nothing -- numbers arrive as a
  * raw `Long` or `Double`.
  *
  * The `Boolean`-returning methods are skip gates.  Answer `false` to decline a value, and the
  * parser scans past it structurally -- matching brackets and quotes only, decoding nothing and
  * allocating nothing -- which is much cheaper than visiting it.  So a visitor that wants one
  * field of a large object returns `false` from `key` for every other key and races to the one
  * it cares about.  (To *produce* something from a walk, or to have value forms checked for
  * you, see [[Jbuilder]].)
  *
  * Streaming/visiting cannot revisit consumed input, so there is no `exact` mode (numbers are
  * always a `Long` or a `Double`) and no format preservation here.  A visitor is stateful;
  * track your own nesting/context.  All methods default to "visit everything, do nothing", so
  * subclass and override only what you need.
  */
trait Jvisitor {
  /** Start of an object; answer `false` to skip the whole object (no keys/values reported). */
  def objStart(): Boolean = true

  /** A key was just read; answer `false` to skip its value and advance to the next key. */
  def key(key: String): Boolean = true

  /** End of an object that was entered (i.e. `objStart` answered `true`). */
  def objEnd(): Unit = ()

  /** Start of an array; answer `false` to skip the whole array. */
  def arrStart(): Boolean = true

  /** About to reach element `i` of an array; answer `false` to skip just that element. */
  def index(i: Int): Boolean = true

  /** End of an array that was entered. */
  def arrEnd(): Unit = ()

  /** A visited string value (fully decoded). */
  def str(value: String): Unit = ()

  /** A visited integer. */
  def num(value: Long): Unit = ()

  /** A visited non-integer number. */
  def num(value: Double): Unit = ()

  /** A visited boolean value. */
  def bool(value: Boolean): Unit = ()

  /** A visited null value. */
  def nul(): Unit = ()
}


/** What a builder wants done with the value about to arrive at a key or index.
  *
  * `Value` visits it generically (containers recurse, leaves land in the leaf callbacks) and
  * `Skip` steps over it structurally, decoding nothing.  The rest are typed expectations: the
  * walker checks the value's form and delivers it through the matching leaf callback -- `L`
  * insists on an integer (a whole number in Long range, exactly as `Json.long` does) and `D`
  * on any number (integers widen), `Str`/`Bool` on that leaf type, and `Obj`/`Arr` insist on
  * the container and then visit it.  A value of the wrong form fails the walk with a
  * positioned error naming the key it was under -- the builder never sees a value that does
  * not match what it asked for.
  */
enum Jexpect {
  case Skip, Value, L, D, Str, Bool, Obj, Arr
}


/** A stateless recipe for building an `A` from a JSON walk: the primitive for custom no-tree,
  * no-boxing decoders.  `zero()` makes the working state `B` -- typically a mutable builder --
  * which the walker then passes to every callback, and `build(b)` finishes it (and is the
  * place to report anything structurally missing, since it answers `Ask`).  When `B` *is* the
  * result, `build` can be `Is(b)` -- no further allocation, since the favored branch of `Or`
  * is unboxed.
  *
  * `key`/`index` answer a [[Jexpect]]: skip the value, visit it, or demand a form and have
  * the walker type-check it, failing the walk with a positioned error on mismatch.  Numbers
  * arrive unboxed.  The value and end callbacks answer `Ask[Unit]` so semantically bad values
  * in well-formed JSON can be refused as they arrive: accept with the prewrapped `Is.unit`
  * (no allocation), or answer an `Alt` and the walker fails the walk right there, wrapping
  * your error with the value's position and whose-key context.
  *
  * Instances hold no per-parse state, so one (an `object`, say) serves any number of parses;
  * run one with `Json.build(in)(builder)`, over any source `Json.stream` accepts, and note
  * that `build(b)` is only consulted if the walk succeeded.
  */
trait Jbuilder[B, A] {
  /** Fresh working state for one walk. */
  def zero(): B

  /** Start of an object; answer `false` to skip the whole object. */
  def objStart(b: B): Boolean = true

  /** A key was just read; answer what to do with its value (see `Jexpect`). */
  def key(b: B, key: String): Jexpect = Jexpect.Value

  /** End of an object that was entered; `Is.unit` to accept it. */
  def objEnd(b: B): Ask[Unit] = Is.unit

  /** Start of an array; answer `false` to skip the whole array. */
  def arrStart(b: B): Boolean = true

  /** About to reach element `i` of an array; answer what to do with it (see `Jexpect`). */
  def index(b: B, i: Int): Jexpect = Jexpect.Value

  /** End of an array that was entered; `Is.unit` to accept it. */
  def arrEnd(b: B): Ask[Unit] = Is.unit

  /** A string value (also how `Jexpect.Str` delivers); `Is.unit` to accept it. */
  def str(b: B, value: String): Ask[Unit] = Is.unit

  /** An integer (also how `Jexpect.L` delivers); `Is.unit` to accept it. */
  def num(b: B, value: Long): Ask[Unit] = Is.unit

  /** A non-integer number (also how `Jexpect.D` delivers, widening integers); `Is.unit` to
    * accept it. */
  def num(b: B, value: Double): Ask[Unit] = Is.unit

  /** A boolean value (also how `Jexpect.Bool` delivers); `Is.unit` to accept it. */
  def bool(b: B, value: Boolean): Ask[Unit] = Is.unit

  /** A null value; `Is.unit` to accept it. */
  def nul(b: B): Ask[Unit] = Is.unit

  /** Finish: turn the walked state into the result, or explain what was missing. */
  def build(b: B): Ask[A]
}
