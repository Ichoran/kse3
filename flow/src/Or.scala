// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-23 Rex Kerr and Calico Life Sciences LLC.

package kse.flow


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.compiletime.{erasedValue, summonFrom}
import scala.util.NotGiven
import scala.util.{Try, Success, Failure}
import scala.util.boundary


/** Supertype of any boxed branch of an `Or`.
  * 
  * This class is not intended to be used directly.
  */
sealed abstract class BoxedOr[+Z]() {
  /** The value of the `Or`. */
  def value: Z

  def isBoxed = true
}

/** Witnesses that a value is being used in an `Or`, but does not box it.
  *
  * This type is not intended to be used directly.
  */
opaque type IsJust[+X] = X
object IsJust {
  inline def wrap[X](x: X): IsJust[X] = x
  extension [X](just: IsJust[X])
    def unwrap: X = just
}

/** A boxed favored branch.  A favored branch is boxed if and only if
  * it contains another `Or` and the value is either a disfavored branch
  * or a boxed favored branch.
  * 
  * This class is not intended to be used directly.
  */
final class IsBox[+X](val get: X) extends BoxedOr[X]() {
  def value = get
  override def toString = s"Is($get)"
  override def hashCode = -1837305301 + get.##
  override def equals(a: Any) = a match
    case i: IsBox[?] => get == i.get
    case _ => false
}


/** The disfavored branch of an `Or`.
  *
  * The value is stored in `alt`.
  * 
  * It can also be extracted by pattern matching into `case Alt(y)` or accessed through
  * various extension methods on `Or`.
  * 
  * Note that you cannot extract `Is` values by pattern matching because `Is` is a
  * fictional type, and the pattern matching exaustiveness checks work poorly in that
  * case.
  */
final class Alt[+Y](val alt: Y) extends BoxedOr[Y]() {
  def get: Nothing = throw new NoSuchElementException("get on Alt")
  def value = alt
  override def toString = s"Alt($alt)"
  override def hashCode = -1867746107 + alt.##
  override def equals(a: Any) = a match
    case aa: Alt[?] => alt == aa.alt
    case _ => false
}
object Alt {
  /** Wraps a value into an `Alt`.
    *
    * `Alt wrap y` is equivalent to `Alt(y)` or `new Alt(y)`.
    */
  inline def wrap[Y](y: Y): Alt[Y] = new Alt(y)

  def unapply[X, Y](o: X Or Y): Option[Y] = o match
    case a: Alt[?] => Some(a.asInstanceOf[Alt[Y]].alt)
    case _ => None

  @annotation.targetName("unapplyAny")
  def unapply(a: Any): Option[Any] = a match
    case a: Alt[?] => Some(a.asInstanceOf[Alt[Any]].alt)
    case _ => None

  /** Breaks out to a boundary with a value wrapped into an `Alt` */
  inline def break[Y, L >: Alt[Y]](y: Y)(using boundary.Label[L]): Nothing = boundary.break(Alt(y))

  /** A canonical instance for Alt[Unit].  Although others are allowed, why not use this one? */
  val unit = Alt(())

  /** A canonical instance for Alt[Boolean] which is false. */
  val F = Alt(false)

  /** A canonical instance for Alt[Boolean] which is true. */
  val T = Alt(true)
}
extension[Y](alt: Alt[Y]) {
  /** Extracts the value stored in this `Alt`, if we are sure that the type is `Alt` and not an `Or`. */
  inline def unwrap: Y = alt.alt

  /** Extends the type to an `Or` with specified favored branch */
  inline def withIs[X]: X Or Y = alt

  /** We're sure this is not the favored branch */
  inline def isIs: Boolean = false

  /** We're sure this is the disfavored branch */
  inline def isAlt: Boolean = true

  /** Uses a partial function to reclaim some values for the favored branch */
  inline def reclaim[X](pf: PartialFunction[Y, X]): X Or Y =
    pf.applyOrElse(alt.alt, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => alt
      case x => Is(x.asInstanceOf[X])

  /** Simply pack into an `Is` instead. */
  inline def swap: Is[Y] =
    Is(alt.alt)
}


/** The favored branch of an `Or`.
  *
  * Internally, the value is stored bare if possible, or in a `IsBox` if not.
  * 
  * You can retrieve the value with `get`, by pattern matching into `Is(x)`, or by
  * using various exension methods on `Or`.
  */
type Is[+X] = kse.flow.IsJust[X] | IsBox[X]
object Is {
  /** Extracts the value stored in this `Is`, if we are sure that the type is `Is`. */
  inline def unwrap[X](is: Is[X]): X = inline erasedValue[X] match
    case _: Null => is.asInstanceOf[X]
    case _: Alt[?] => is.asInstanceOf[IsBox[X]].get
    case _: IsBox[?] => is.asInstanceOf[IsBox[X]].get
    case _ => summonFrom {
      case _: (IsBox[?] <:< X) => (is: Any) match
        case _: BoxedOr[?] => is.asInstanceOf[IsBox[X]].get
        case _ => is.asInstanceOf[X]
      case _: (Alt[?] <:< X) => (is: Any) match
        case _: BoxedOr[?] => is.asInstanceOf[IsBox[X]].get
        case _ => is.asInstanceOf[X]
      case _ => is.asInstanceOf[X]
    }

  /** Wraps a value into an `Is` (by doing nothing unless it's wrapping some other boxed `Or`) */
  inline def apply[X](x: X): Is[X] = inline erasedValue[X] match
    case _: Null => x.asInstanceOf[kse.flow.IsJust[X]]
    case _: Alt[?] => new IsBox(x)
    case _: IsBox[?] => new IsBox(x)
    case _ => summonFrom {
      case _: (IsBox[?] <:< X) => (x: Any) match
        case _: BoxedOr[?] => new IsBox(x)
        case _ => x
      case _: (Alt[?] <:< X) => (x: Any) match
        case w: BoxedOr[?] => new IsBox(x)
        case _ => x.asInstanceOf[kse.flow.IsJust[X]]
      case _ => x.asInstanceOf[kse.flow.IsJust[X]]
    }

  /** Wraps a value into an `Is`.  `Is wrap x` is equivalent to `Is(x)`. */
  inline def wrap[X](x: X): Is[X] = apply(x)

  // Exhaustiveness checking prevents proper pattern matching, sadly
  /*
  def unapply[X, Y](o: X Or Y): Option[X] = o match
    case _: BoxedOr[_] => o match
      case x: IsBox[_] => Some(x.get.asInstanceOf[X])
      case _         => None
    case _ => Some(o.asInstanceOf[X])

  @annotation.targetName("unapplyAny")
  def unapply(a: Any): Option[Any] = a match
    case _: BoxedOr[_] => a match
      case x: IsBox[_] => Some(x.get.asInstanceOf[Any])
      case _         => None
    case _ => Some(a)
  */

  /** Breaks out to a boundary with a value marked as an `Is` */
  inline def break[X, L >: Is[X]](x: X)(using boundary.Label[L]): Nothing = boundary.break(Is(x))


  /** The canonical Is[Unit], which is actually always just `()`, the only possible `Unit`; but this one is typed as `Is[Unit]`. */
  val unit: Is[Unit] = ()

  /** A canonical Is[Boolean] which is false.*/
  val F = Is(false)

  /** A canonical Is[Boolean] which is true. */
  val T = Is(true)
}
extension [X](is: Is[X]) {
  /** Unwraps this type. */
  inline def unwrap: X = Is unwrap is

  /** Extends the type to an `Or` with specified disfavored branch */
  inline def withAlt[Y]: X Or Y = is

  /** Unwraps this type */
  inline def get: X = Is unwrap is

  /** Can't get the alternative version of this type */
  inline def alt: Nothing = throw new NoSuchElementException("alt on Is")

  /** We're sure this is the favored branch */
  inline def isIs: Boolean = true

  /** We're sure this is not the disfavored branch */
  inline def isAlt: Boolean = false

  /** Fold is trivial--ignore the other branch */
  inline def fold[Z](inline f: X => Z)(inline g: Nothing => Z): Z =
    f(Is unwrap is)

  /** Map is trivial--just apply function and wrap */
  inline def map[XX](inline f: X => XX): Is[XX] =
    Is(f(Is unwrap is))

  /** flatMap is trivial--just apply function */
  inline def flatMap[XX, Y](inline f: X => XX Or Y): XX Or Y =
    f(Is unwrap is)

  /** foreach is trivial--just apply function and return unit */
  inline def foreach(inline f: X => Unit): Unit =
    f(Is unwrap is)

  // use in OverloadedExtensions

  /** exists is trivial--just apply predicate */
  inline def exists(inline p: X => Boolean): Boolean =
    p(Is unwrap is)

  /** forall is trivial--just apply predicate */
  inline def forall(inline p: X => Boolean): Boolean =
    p(Is unwrap is)

  /** discard is a handy way to build an Or out of an Is */
  inline def discard[Y](inline pf: PartialFunction[X, Y]): X Or Y =
    pf.applyOrElse(Is unwrap is, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => is
      case y => Alt(y.asInstanceOf[Y])

  /** Simply pack into an `Alt` instead. */
  inline def swap: Alt[X] =
    Alt(Is unwrap is)
}



/** This is the implementation for `Or`, an unboxed union sum type with the favored branch on the left.
  * 
  * `Or` is left-biased and reads in normal order: `A Or B` has a
  * favored value of type `A` and an alternative value of type `B`.
  * The favored branch is encoded as an type `Is[A]`, while
  * the disfavored branch is encoded in a box (class) `Alt[B]`.
  * 
  * Internally, `Is[X]` is either a bare `X` typed as an opaque `IsJust[X]`
  * or a box class `IsBox[X]`. The latter is only the case if `X`
  * itself is either an `Alt` or another `IsBox`.
  *
  * Create `Or` values by using `Is(x)` for a favored branch or `Alt(y)` for disfavored.
  * Alternatively, use `x.or[Y]` to specify that `x` is a favored branch and the alternative
  * type is `Y`, and `y.isnt[X]` to pack `y` as a disfavored branch and specify that the
  * favored branch's type is `X`.
  */
infix type Or[+X, +Y] = Is[X] | Alt[Y]
object Or {
  /** Converts an `Either` into an `Or`, preserving the favored branch (i.e. Right[R] to Is[R]). */
  inline def from[L, R, E <: Either[L, R]](e: E): R Or L = (e: Either[L, R]) match
    case Left(l) => Alt(l)
    case Right(r) => Is(r)

  /** Converts an `Either` into an `Or`, swapping favored and disfavored branches (i.e Left[L] to Is[L]). */
  inline def swapFrom[L, R, E <: Either[L, R]](e: E): L Or R = (e: Either[L, R]) match
    case Left(l) => Is(l)
    case Right(r) => Alt(r)

  /** Converts an `Option` into an `Or`, with a value favored and `None` disfavored and mapping to `Unit`. */
  inline def from[A, O <: Option[A]](o: O): A Or Unit = (o: Option[A]) match
    case Some(a) => Is(a)
    case _ => Alt.unit

  /** Converts an `Option` into an `Or`, with a value disfavored and `None` mapping to `Is[Unit]` */
  inline def swapFrom[A, O <: Option[A]](o: O): Unit Or A = (o: Option[A]) match
    case Some(a) => Alt(a)
    case _ => Is.unit

  /** Converts an `Option` into the favored value of an `Or`, or loads a default disfavored value. */
  inline def fromOrElse[A, B, O <: Option[A]](o: O, b: => B): A Or B = (o: Option[A]) match
    case Some(a) => Is(a)
    case _       => Alt(b)

  /** Converts an `Option` into the disfavored value of an `Or`, or loads a default favored value. */
  inline def swapFromOrElse[A, B, O <: Option[A]](o: O, b: => B): B Or A = (o: Option[A]) match
    case Some(a) => Alt(a)
    case _       => Is(b)

  /** Converts a `Try` into an `Or`, preserving the favored branch and simply storing the `Throwable` in the disfavored branch. */
  inline def from[T](t: Try[T]): T Or Throwable = t match
    case Failure(e) => Alt(e)
    case Success(s) => Is(s)

  /** Converts a `Try` into an `Or`, swapping the favored and disfavored branches and simply storing the `Throwable` in the favored branch. */
  inline def swapFrom[T](t: Try[T]): Throwable Or T = t match
    case Failure(e) => Is(e)
    case Success(s) => Alt(s)

  /** Converts a `Try` into an `Or`, preserving the favored branch and remapping the `Throwable` into the disfavored branch. */
  inline def from[T, E](t: Try[T], fix: Throwable => E): T Or E = t match
    case Failure(e) => Alt(fix(e))
    case Success(s) => Is(s)

  /** Converts a `Try` into an `Or`, swapping favored and disfavored branches and remapping the `Throwable` into the favored branch. */
  inline def swapFrom[T, E](t: Try[T], fix: Throwable => E): E Or T = t match
    case Failure(e) => Is(fix(e))
    case Success(s) => Alt(s)

  /** Not intended for public use: this is used as an indicator argument to `PartialFunction`'s `applyOrElse` method
    * to detect when the function is not in its domain.
    */
  val defaultApplyOrElse: Any = new Function1[Any, Any] { def apply(a: Any) = this }
}
extension [X, Y](or: Or[X, Y]) {
  /** The favored value of an `Or`, or a `NoSuchElementException` if the branch is disfavored. */
  inline def get: X = or match
    case _: Alt[?] => throw new NoSuchElementException("get when Or is Alt")
    case _ => Is unwrap or.asInstanceOf[Is[X]]

  /** The disfavored branch of an `Or`, or a `NoSuchElementException` if the branch is favored. */
  inline def alt: Y = or match
    case a: Alt[?] => a.alt.asInstanceOf[Y]
    case _         => throw new NoSuchElementException(s"alt when Or is Is")

  /** Either value of an `Or`, returned as a type union of the two branches. */
  inline def union: X | Y = (or: Any) match    // Avoid error on Is(null)...compiler knows answer but how do we tell it here???
    case w: BoxedOr[?] => w.value.asInstanceOf[X | Y]
    case x             => x.asInstanceOf[X]

  /** Informational method that tells whether the favored branch is ever boxed
    * (if you follow it all the way down in case of a favored branch itself being an `Or`).
    */
  inline def isBoxed: Boolean = (or: Any) match  // Avoid error on Is(Alt(x))...compiler knows answer but how do we tell it here???
    case _: BoxedOr[?] => true
    case _             => false

  /** Informational method that tells whether this or holds the favored branch. */
  inline def isIs: Boolean = (or: Any) match
    case _: Alt[?] => false
    case _         => true

  /** Informational method that tells whether this or holds the favored branch. */
  inline def isAlt: Boolean = (or: Any) match
    case _: Alt[?] => true
    case _         => false

  /** Access to both branches of an `Or`.
    *
    * Usage example:
    * {{{
    * 5.orAlt[String].fold(_ + 1)(_.length)
    * }}}
    */
  inline def fold[Z](inline f: X => Z)(inline g: Y => Z): Z = inline or match
    case _: Null => f(or.asInstanceOf[X])
    case _: Alt[?] => g(or.asInstanceOf[Alt[Y]].alt)
    case _ => or match
      case _: Alt[?] => g(or.asInstanceOf[Alt[Y]].alt)
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

  /** Extracts the favored branch of an `Or`, or remaps the disfavored value.
    *
    * Equivalent to `o.fold{ x => x }{ y => f(y) }
    */
  inline def getOrElse[Z >: X](inline f: Y => Z): Z = (or: X Or Y) match
    case _: Alt[?] => f(or.asInstanceOf[Alt[Y]].alt)
    case _ => Is unwrap or.asInstanceOf[Is[X]]

  /** Extracts the disfavored branch of an `Or`, or remaps the favored value.
    * 
    * Equivalent to `o.fold{ x => f(x )}{ y => y }`
    */
  inline def altOrElse[Z >: Y](inline f: X => Z): Z = (or: X Or Y) match
    case a: Alt[?] => a.alt.asInstanceOf[Y]
    case _ => f(Is unwrap or.asInstanceOf[Is[X]])

  /** Applies a function to the favored value to create a new `Or`; a disfavored value is left unchanged. */
  inline def map[XX](inline f: X => XX): XX Or Y = (or: X Or Y) match
    case _: Alt[?] => or.asInstanceOf[Alt[Y]]
    case _ => Is(f(Is unwrap or.asInstanceOf[Is[X]]))

  /** Applies a function to the disfavored value to create a new `Or`; a favored value is left unchanged. */
  inline def mapAlt[YY](inline f: Y => YY): X Or YY = (or: X Or Y) match
    case a: Alt[?] => Alt(f(a.alt.asInstanceOf[Y]))
    case x => x.asInstanceOf[Is[X]]

  /** Applies a function to whichever value exists.
    *
    * Equivalent to `o.fold{ x => Is(f(x)) }{ y => Alt(g(y))` }
    */
  inline def mapThem[XX, YY](inline f: X => XX)(inline g: Y => YY): XX Or YY = (or: X Or Y) match
    case a: Alt[?] => Alt(g(a.alt.asInstanceOf[Y]))
    case _ => Is(f(Is unwrap or.asInstanceOf[Is[X]]))

  /** Applies a function that converts a favored value into an `Or`; the disfavored value is retained. */
  inline def flatMap[YY >: Y, XX](inline f: X => XX Or YY): XX Or YY = (or: X Or Y) match
    case _: Alt[?] => or.asInstanceOf[Alt[Y]]
    case _ => f(Is unwrap or.asInstanceOf[Is[X]]) 

  /** Applies a function that converts a disfavored value into an `Or; the favored value is retained. */
  inline def flatMapAlt[XX >: X, YY](inline g: Y => XX Or YY): XX Or YY = (or: X Or Y) match
    case a: Alt[?] => g(a.alt.asInstanceOf[Y])
    case _ => or.asInstanceOf[Is[X]]

  /** Applies a function to whichever value exists, creating a new `Or`.
    * 
    * Equivalent to `o.fold{ x => f(x) }{ y => g(y) }` where `f` and `g` both produce type `XX Or YY`.
    */
  inline def flatMapThem[XX, YY](inline f: X => XX Or YY)(inline g: Y => XX Or YY): XX Or YY = (or: X Or Y) match
    case a: Alt[?] => g(a.alt.asInstanceOf[Y])
    case _ => f(Is unwrap or.asInstanceOf[Is[X]])

  /** Keeps a favored value, or switches to another `Or` while discarding a disfavored value.
    *
    * Equivalent to `o.flatMapAlt(_ => that)`.
    */
  inline def ||[XX >: X, YY >: Y](inline that: => XX Or YY): XX Or YY = (or: X Or Y) match
    case _: Alt[?] => that
    case _         => or.asInstanceOf[Is[XX]]

  /** Pairs two existing favored values, or returns the earliest disfavored value.
    * If the first or second `Or` is a unit success, just return the second; don't bother tupling.
    *
    * Equivalent to `o1.flatMap{ x1 => o2.map{ x2 => (x1, x2) } }` if neither `Or` has a `Unit` success type,
    * or `o1.flatMap{_ => o2}` if `o1` has `Unit` success type,
    * or `o1.flatMap{x => o2.map(_ => x)}` if `o2` has `Unit` success type and `o1` does not.
    */
  transparent inline def &&[XX, YY >: Y](inline that: => XX Or YY) =
    inline erasedValue[X] match
      case _: Unit => (or: X Or Y) match
        case a: Alt[?] => a.asInstanceOf[XX Or YY]
        case _         => that
      case _ => inline erasedValue[XX] match
        case _: Unit => (or: X Or Y) match
          case a: Alt[?] => a.asInstanceOf[X Or YY]
          case _ => (that: XX Or YY) match
            case b: Alt[?] => b.asInstanceOf[X Or YY]
            case _ => or.asInstanceOf[X Or YY]
        case _ => (or: X Or Y) match
          case a: Alt[?] => a.asInstanceOf[(X, XX) Or YY]
          case _ => (that: XX Or YY) match
            case b: Alt[?] => b.asInstanceOf[(X, XX) Or YY]
            case xx => Is((Is unwrap or.asInstanceOf[Is[X]], Is unwrap xx.asInstanceOf[Is[XX]])): (X, XX) Or YY

  /** Operate on the favored value if it exists. */
  inline def foreach(inline f: X => Unit): Unit = (or: X Or Y) match
    case _: Alt[?] => ()
    case _ => f(Is unwrap or.asInstanceOf[Is[X]])

  /** Operate on the disfavored value if it exists. */
  inline def foreachAlt(inline f: Y => Unit): Unit = (or: X Or Y) match
    case a: Alt[?] => f(a.alt.asInstanceOf[Y])
    case _ => ()

  /** Operate on whichever value exists.
    * 
    * Equivalent to `o.fold( x => f(x) ){ y => g(y) }; ()`
    */
  inline def foreachThem(inline f: X => Unit)(inline g: Y => Unit) = (or: X Or Y) match
    case a: Alt[?] => g(a.alt.asInstanceOf[Y])
    case _ => f(Is unwrap or.asInstanceOf[Is[X]])

  // use in OverloadedExtensions

  /** Operate on the disfavored value if it exists, but pass on the original `Or`. */
  inline def useAlt(inline f: Y => Unit): or.type =
    (or: X Or Y) match
      case a: Alt[?] => f(a.alt.asInstanceOf[Y])
      case _ =>
    or

  /** Operate on whichever value exists, but pass on the original `Or`.
    * 
    * Equivalent to `o.fold{ x => f(x) }{ y => g(y) }; o`
    */
  inline def useThem(inline f: X => Unit)(inline g: Y => Unit): or.type =
    (or: X Or Y) match
      case a: Alt[?] => g(a.alt.asInstanceOf[Y])
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])
    or

  /** Tests whether the favored branch is there and a predicate is true on it */
  inline def exists(inline p: X => Boolean): Boolean = (or: X Or Y) match
    case a: Alt[?] => false
    case _ => p(Is unwrap or.asInstanceOf[Is[X]])

  /** Tests whether the disfavored branch is there and a predicate is true on it */
  inline def existsAlt(inline q: Y => Boolean): Boolean = (or: X Or Y) match
    case a: Alt[?] => q(a.alt.asInstanceOf[Y])
    case _ => false

  /** Tests a predicate on the favored branch or a second predicate on the disfavored branch, depending on which is there */
  inline def existsThem(inline p: X => Boolean)(inline q: Y => Boolean): Boolean = (or: X Or Y) match
    case a: Alt[?] => q(a.alt.asInstanceOf[Y])
    case _ => p(Is unwrap or.asInstanceOf[Is[X]])

  /** Tests whether either the disfavored branch is there, or if the favored branch is there, whether a predicate is true on it */
  inline def forall(inline p: X => Boolean): Boolean = (or: X Or Y) match
    case a: Alt[?] => true
    case _ => p(Is unwrap or.asInstanceOf[Is[X]])

  /** Tests whether either the favored branch is there, or if the disfavored branch is there, whether a predicate is true on it */
  inline def forallAlt(inline q: Y => Boolean): Boolean = (or: X Or Y) match
    case a: Alt[?] => q(a.alt.asInstanceOf[Y])
    case _ => true

  /** Tests a predicate on the favored branch or a second predicate on the disfavored branch, depending on which is there.
    * 
    * Note--this is identical to `existsThem` since one branch does exist.
    */
  inline def forallThem(inline p: X => Boolean)(inline q: Y => Boolean): Boolean = (or: X Or Y) match
    case a: Alt[?] => q(a.alt.asInstanceOf[Y])
    case _ => p(Is unwrap or.asInstanceOf[Is[X]])

  /** Reject some favored values, when a favored value exists, by converting those into disfavored values.
    *
    * A partial function determines which favored values are to be rejected, and performs the remapping.
    * 
    * Usage example:
    * {{{
    * -5.or[String].reject{ case x if x < 0 => "Negative value" }
    * }}}
    */
  inline def reject[YY >: Y](pf: PartialFunction[X, YY]): X Or YY = (or: X Or Y) match
    case _: Alt[?] => or.asInstanceOf[Alt[Y]]
    case _ => pf.applyOrElse(Is unwrap or.asInstanceOf[Is[X]], Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => or
      case yy => Alt(yy.asInstanceOf[YY])

  /** Rescue some disfavored values, when a disfavored value exists, by converting them into favored values.
    * 
    * A partial function determines which disfavored values are to be restored, and performs the remapping.
    * 
    * Usage example:
    * {{{
    * '7'.isnt[Int].reclaim{ case c if c.isDigit => java.lang.Character.digit(c, 10) }
    * }}}
    */
  inline def rescue[XX >: X](pf: PartialFunction[Y, XX]): XX Or Y = (or: X Or Y) match
    case a: Alt[?] => pf.applyOrElse(a.alt.asInstanceOf[Y], Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => or
      case xx => Is(xx.asInstanceOf[XX])
    case _ => or

  /** Reject favored values without losing track of the other possible types of disfavored values. */
  inline def alsoReject[Z](pf: PartialFunction[X, Z]): X Or (Z Or Y) = (or: X Or Y) match
    case _: Alt[?] => Alt(or.asInstanceOf[Alt[Y]])
    case _ => pf.applyOrElse(Is unwrap or.asInstanceOf[Is[X]], Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => or.asInstanceOf[Is[X]]
      case z => Alt(Is(z.asInstanceOf[Z]))

  /** Reshapes `(P Or Q) Or Y` into `P Or (Q or Y)`; the latter is recommended as the canonical packing for three alternatives, as it has less boxing for disfavored cases. */
  def pivot[P, Q](using (P Or Q) =:= X): P Or (Q Or Y) = (or: X Or Y) match
    case _: BoxedOr[?] => or match
      case y: Alt[?] => Alt(y.asInstanceOf[Alt[Y]])
      case _ => (Is unwrap or.asInstanceOf[P Or Q]) match
        case q: Alt[?] => Alt(Is(q.alt.asInstanceOf[Q]))
        case p => Is(Is unwrap p.asInstanceOf[Is[P]])
    case _ => or.asInstanceOf[Is[P]]

  /** Reshapes `P Or (U or V)` into `(P Or U) Or V`; the latter is not recommended as the canonical packing for three alternatives, but may be useful at times. */
  def unpivot[U, V](using (U Or V) =:= Y): (X Or U) Or V = (or: X Or Y) match
    case _: BoxedOr[?] => or match
      case y: Alt[?] => y.alt.asInstanceOf[U Or V] match
        case v: Alt[?] => v.asInstanceOf[Alt[V]]
        case u => Is(Alt(Is unwrap u.asInstanceOf[Is[U]]))
      case _ => Is(or.asInstanceOf[Is[X]])
    case _ => or.asInstanceOf[Is[Is[X]]]

  /** Flattens `(P Or Y) Or Y` into `P Or Y`. */
  def flatten[P](using (P Or Y) =:= X): P Or Y = (or: X Or Y) match
    case b: BoxedOr[?] => b match
      case y: Alt[?] => y.asInstanceOf[Alt[Y]]
      case x: IsBox[?] => x.get.asInstanceOf[P Or Y]
    case _ => or.asInstanceOf[Is[P]]

  /** Keeps only those favored values that pass a test, converting others to a disfavored Unit.
    * Note that this is only present for `for` compatibility; otherwise, favor `discard` or `flatMap`.
    */
  inline def withFilter(inline p: X => Boolean): X Or (Y | Unit) = (or: X Or Y) match
    case _: Alt[?] => or
    case _ =>
      val v = Is unwrap or.asInstanceOf[Is[X]]
      if p(v) then or else Alt.unit

  /** Breaks with the favored branch leaving the disfavored branch behind. */
  inline def breakOnIs[Z >: kse.flow.Is[X]](using boundary.Label[Z]): Alt[Y] = (or: X Or Y) match
    case _: Alt[?] => or.asInstanceOf[Alt[Y]]
    case _         => boundary.break(or.asInstanceOf[Is[X]])

  /** Breaks with the disfavored branch leaving the favored branch behind */
  inline def breakOnAlt[Z >: Alt[Y]](using boundary.Label[Z]): kse.flow.Is[X] = (or: X Or Y) match
    case _: Alt[?] => boundary.break(or.asInstanceOf[Alt[Y]])
    case _         => or.asInstanceOf[Is[X]]

  /** An `Or` with favored and disfavored branches swapped. */
  inline def swap: Y Or X = or match
    case _: Alt[?] => Is(or.asInstanceOf[Alt[Y]].alt)
    case _ => Alt(Is unwrap or.asInstanceOf[Is[X]])
}


extension [A](a: A) {
  /** Wraps this as an Is while indicating what the bad alternative would be */
  inline def orAlt[B]: A Or B = Is(a)

  /** Wraps this as an Alt while indicating what the good alternative would be */
  inline def orIs[B]: B Or A = Alt(a)

  /** Marks this as a favored value */
  inline def asIs: kse.flow.Is[A] = Is(a)

  /** Wraps this as a disfavored value */
  inline def asAlt: Alt[A] = Alt(a)

  /** Separates values into favored and disfavored based on a predicate; true means favored. */
  inline def isIf(inline p: A => Boolean): A Or A =
    if p(a) then Is(a) else Alt(a)

  /** Separates values into disfavored and favored based on a predicate; true means disfavored. */
  inline def altIf(inline q: A => Boolean): A Or A =
    if q(a) then Alt(a) else Is(a)

  /** Maps certain vales into a favored branch based on a partial function; those that don't map are disfavored. */
  inline def isCase[X](pf: PartialFunction[A, X]): X Or A =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Alt(a)
      case x => Is(x.asInstanceOf[X])

  /** Maps certain values into a disfavored branch based on a partial function; those that don't map are favored. */
  inline def altCase[Y](pf: PartialFunction[A, Y]): A Or Y =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Is(a)
      case y => Alt(y.asInstanceOf[Y])

  /** Maps certain values into a favored branch based on a partial function; those that don't are mapped to a disfavored branch */
  inline def isCaseOrAlt[X, Y](pf: PartialFunction[A, X])(g: A => Y): X Or Y =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Alt(g(a))
      case x => Is(x.asInstanceOf[X])

  /** Maps certain values into a favored branch based on a partial function; those that don't are mapped to a disfavored branch */
  inline def altCaseOrIs[X, Y](pf: PartialFunction[A, Y])(f: A => X): X Or Y =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Is(f(a))
      case y => Alt(y.asInstanceOf[Y])

  /** Uses a predicate to determine which function to use to map a value into a favored or disfavored Or branch */
  inline def unfoldToOr[X, Y](p: A => Boolean)(f: A => X)(g: A => Y): X Or Y =
    if p(a) then Is(f(a)) else Alt(g(a))

  /** This is a favored value, but make it look like another Or */
  inline def isLike[X >: A, Y](that: X Or Y): X Or Y = Is(a: X)

  /** This is a disfavored value, but make it look like another Or */
  inline def altLike[X, Y >: A](that: X Or Y): X Or Y = Alt(a: Y)

  /** Keep the value if it passes, otherwise discard it */
  inline def keepIf(inline p: A => Boolean): A Or Unit = if p(a) then Is(a) else Alt.unit

  /** Keep the value of pattern matches, otherwise discard it */
  inline def keepCase[Z](pf: PartialFunction[A, Z]): Z Or Unit =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Alt.unit
      case z => Is(z.asInstanceOf[Z])
}

extension [A >: Null] (a: A) {
  /** Ensures that a value is not null and places it as the favored branch; null is converted to a disfavored Unit value */
  inline def nn: A Or Unit =
    if a == null then Alt.unit else Is(a)
}

