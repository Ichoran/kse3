// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.jsaun


/** A SAX-style callback interface for `Json.stream`: the parser walks the document once and
  * calls these methods as it goes, building no tree.
  *
  * The `Boolean`-returning methods are skip gates.  Answer `false` to decline a value, and the
  * parser scans past it structurally -- matching brackets and quotes only, decoding nothing and
  * allocating nothing -- which is much cheaper than visiting it.  So a visitor that wants one
  * field of a large object returns `false` from `key` for every other key and races to the one
  * it cares about.
  *
  * Streaming/visiting cannot revisit consumed input, so there is no `exact` mode (no `Jnum.Big`)
  * and no format preservation here.  A visitor is stateful; track your own nesting/context.
  * All methods default to "visit everything, do nothing", so subclass and override only what you
  * need.
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

  /** A visited number (`Jnum.L` or `Jnum.D`; never `Jnum.Big`, as there is no exact mode). */
  def num(value: Jnum): Unit = ()

  /** A visited boolean value. */
  def bool(value: Boolean): Unit = ()

  /** A visited null value. */
  def nul(): Unit = ()
}
