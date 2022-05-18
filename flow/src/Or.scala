// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022 Rex Kerr and Calico Life Sciences LLC.

package kse.flow

import scala.compiletime.{erasedValue, summonFrom}
import scala.util.NotGiven
import scala.util.{Try, Success, Failure}
import scala.util.control.{NonFatal, ControlThrowable}

/** This is the implementation for `Or`, an unboxed union sum type.
  * 
  * `Or` is left-biased and reads in normal order: `A Or B` has a
  * favored value of type `A` and an alternative value of type `B`.
  * The favored branch is encoded as an type `Is[A]`, while
  * the disfavored branch is encoded in a box (class) `Alt[B]`.
  * 
  * Internally, `Is[X]` is either a bare `X` typed as an opaque `IsJust[X]`
  * or a box class `IsBox[X]`. The latter is only the case if `X`
  * itself is either an `Alt` or another `IsBox`.
  */
object AorB {
  /** Supertype of any boxed branch of an `Or`.
    * 
    * This class is not intended to be used directly.
    */
  sealed abstract class BoxedOr[+Z]() {
    /** The value of the `Or`. */
    def value: Z
  }

  /** Witnesses that a value is being used in an `Or`, but does not box it.
    *
    * This type is not intended to be used directly.
    */
  opaque type IsJust[+X] = X

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
      case i: IsBox[_] => get == i.get
      case _ => false
  }

  /** The disfavored branch of an `Or`.
    *
    * The value is stored in `alt`.
    * 
    * It can also be extracted by pattern matching into `case Alt(y)` or accessed through
    * various extension methods on `Or`.
    */
  final class Alt[+Y](val alt: Y) extends BoxedOr[Y]() {
    def value = alt
    override def toString = s"Alt($alt)"
    override def hashCode = -1867746107 + alt.##
    override def equals(a: Any) = a match
      case aa: Alt[_] => alt == aa.alt
      case _ => false
  }
  object Alt {
    extension[Y](alt: Alt[Y]) {
      /** Extracts the value stored in this `Alt`, if we are sure that the type is `Alt` and not an `Or`. */
      inline def unwrap: Y = alt.alt

      /** Extends the type to an `Or` with specified favored branch */
      inline def favor[X]: X Or Y = alt
    }

    /** Wraps a value into an `Alt`.
      *
      * `Alt wrap y` is equivalent to `Alt(y)` or `new Alt(y)`.
      */
    inline def wrap[Y](y: Y): Alt[Y] = new Alt(y)

    def unapply[X, Y](o: X Or Y): Option[Y] = o match
      case a: Alt[_] => Some(a.asInstanceOf[Alt[Y]].alt)
      case _ => None

    @annotation.targetName("unapplyAny")
    def unapply(a: Any): Option[Any] = a match
      case a: Alt[_] => Some(a.asInstanceOf[Alt[Any]].alt)
      case _ => None

    /** A canonical instance for Alt[Unit].  Although others are allowed, why not use this one? */
    val unit = Alt(())
  }

  /** The favored branch of an `Or`.
    *
    * Internally, the value is stored bare if possible, or in a `IsBox` if not.
    * 
    * You can retrieve the value with `get`, by pattern matching into `Is(x)`, or by
    * using various exension methods on `Or`.
    */
  type Is[+X] = IsJust[X] | IsBox[X]
  object Is {
    /** Extracts the value stored in this `Is`, if we are sure that the type is `Is`. */
    inline def unwrap[X](is: Is[X]): X = inline erasedValue[X] match
      case _: Alt[_] => is.asInstanceOf[IsBox[X]].get
      case _: IsBox[_] => is.asInstanceOf[IsBox[X]].get
      case _ => summonFrom {
        case _: (IsBox[_] <:< X) => (is: Any) match
          case _: BoxedOr[_] => is.asInstanceOf[IsBox[X]].get
          case _ => is.asInstanceOf[X]
        case _: (Alt[_] <:< X) => (is: Any) match
          case _: BoxedOr[_] => is.asInstanceOf[IsBox[X]].get
          case _ => is.asInstanceOf[X]
        case _ => is.asInstanceOf[X]
      }

    /** Wraps a value into an `Is` (by doing nothing unless it's wrapping some other boxed `Or`) */
    inline def apply[X](x: X): Is[X] = inline erasedValue[X] match
      case _: Alt[_] => new IsBox(x)
      case _: IsBox[_] => new IsBox(x)
      case _ => summonFrom {
        case _: (IsBox[_] <:< X) => (x: Any) match
          case _: BoxedOr[_] => new IsBox(x)
          case _ => x
        case _: (Alt[_] <:< X) => (x: Any) match
          case w: BoxedOr[_] => new IsBox(x)
          case _ => x.asInstanceOf[IsJust[X]]
        case _ => x.asInstanceOf[IsJust[X]]
      }

    /** Wraps a value into an `Is`.  `Is wrap x` is equivalent to `Is(x)`. */
    inline def wrap[X](x: X): Is[X] = apply(x)

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

    /** The canonical Is[Unit], which is actually always just `()`, the only possible `Unit`; but this one is typed as `Is[Unit]`. */
    val unit: Is[Unit] = ()
  }
  extension [X](is: Is[X]) {
    /** Unwraps this type. */
    inline def unwrap: X = Is.unwrap(is)

    /** Extends the type to an `Or` with specified favored branch */
    inline def disfavor[Y]: X Or Y = is
  }



  /** An unboxed sum type with the favored branch on the left.
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

    /** Converts an `Ok` into an `Or`, preserving the favored branch. */
    inline def from[N, Y](ok: Ok[N, Y]): Y Or N = ok match
      case No(n) => Alt(n)
      case Yes(y) => Is(y)

    /** Converts an `Ok` into an `Or`, swapping favored and disfavored branches. */
    inline def swapFrom[N, Y](ok: Ok[N, Y]): N Or Y = ok match
      case No(n) => Is(n)
      case Yes(y) => Alt(y)

    /** Converts an `Option` into an `Or`, with a value favored and `None` disfavored and mapping to `Unit`. */
    inline def from[A, O <: Option[A]](o: O): A Or Unit = (o: Option[A]) match
      case Some(a) => Is(a)
      case _ => Alt.unit

    /** Converts an `Option` into an `Or`, with a value disfavored and `None` mapping to `Is[Unit]` */
    inline def swapFrom[A, O <: Option[A]](o: O): Unit Or A = (o: Option[A]) match
      case Some(a) => Alt(a)
      case _ => Is.unit

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
      case _: Alt[X] => throw new NoSuchElementException("get when Or is Alt")
      case _ => Is unwrap or.asInstanceOf[Is[X]]

    /** The disfavored branch of an `Or`, or a `NoSuchElementException` if the branch is favored. */
    inline def alt: Y = or match
      case a: Alt[_] => a.alt.asInstanceOf[Y]
      case _         => throw new NoSuchElementException(s"alt when Or is Is")

    /** Either value of an `Or`, returned as a type union of the two branches. */
    inline def value: X | Y = or match
      case w: BoxedOr[_] => w.value.asInstanceOf[X | Y]
      case x               => x.asInstanceOf[X]

    /** Informational method that tells whether the favored branch is ever boxed
      * (if you follow it all the way down in case of a favored branch itself being an `Or`).
      */
    inline def isBoxed: Boolean = or match
      case w: BoxedOr[_] => true
      case _               => false

    /** Access to both branches of an `Or`.
      *
      * Usage example:
      * {{{
      * 5.or[String].fold(_ + 1)(_.length)
      * }}}
      */
    inline def fold[Z](inline f: X => Z)(inline g: Y => Z): Z = or match
      case _: Alt[_] => g(or.asInstanceOf[Alt[Y]].alt)
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

    /** Extracts the favored branch of an `Or`, or remaps the disfavored value.
      *
      * Equivalent to `o.fold{ x => x }{ y => f(y) }
      */
    inline def getOrElse[Z >: X](inline f: Y => Z): Z = or match
      case _: Alt[_] => f(or.asInstanceOf[Alt[Y]].alt)
      case _ => Is unwrap or.asInstanceOf[Is[X]]

    /** Extracts the disfavored branch of an `Or`, or remaps the favored value.
      * 
      * Equivalent to `o.fold{ x => f(x )}{ y => y }`
      */
    inline def altOrElse[Z >: Y](inline f: X => Z): Z = or match
      case a: Alt[_] => a.alt.asInstanceOf[Y]
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

    /** Applies a function to the favored value to create a new `Or`; a disfavored value is left unchanged. */
    inline def map[XX](inline f: X => XX): XX Or Y = or match
      case _: Alt[_] => or.asInstanceOf[Alt[Y]]
      case _ => Is(f(Is unwrap or.asInstanceOf[Is[X]]))

    /** Applies a function to the disfavored value to create a new `Or`; a favored value is left unchanged. */
    inline def mapAlt[YY](inline f: Y => YY): X Or YY = or match
      case a: Alt[_] => Alt(f(a.alt.asInstanceOf[Y]))
      case x => x.asInstanceOf[Is[X]]

    /** Applies a function to whichever value exists.
      *
      * Equivalent to `o.fold{ x => Is(f(x)) }{ y => Alt(g(y))` }
      */
    inline def mapThem[XX, YY](inline f: X => XX)(inline g: Y => YY): XX Or YY = or match
      case a: Alt[_] => Alt(g(a.alt.asInstanceOf[Y]))
      case _ => Is(f(Is unwrap or.asInstanceOf[Is[X]]))

    /** Applies a function that converts a favored value into an `Or`; the disfavored value is retained. */
    inline def flatMap[YY >: Y, XX](inline f: X => XX Or YY): XX Or YY = or match
      case _: Alt[_] => or.asInstanceOf[Alt[Y]]
      case _ => f(Is unwrap or.asInstanceOf[Is[X]]) 

    /** Applies a function that converts a disfavored value into an `Or; the favored value is retained. */
    inline def flatMapAlt[XX >: X, YY](inline g: Y => XX Or YY): XX Or YY = or match
      case a: Alt[_] => g(a.get.asInstanceOf[Y])
      case _ => or.asInstanceOf[Is[X]]

    /** Applies a function to whichever value exists, creating a new Or.
      * 
      * Equivalent to `o.fold{ x => f(x) }{ y => g(y) }` where `f` and `g` both produce type `XX Or YY`.
      */
    inline def flatMapThem[XX, YY](inline f: X => XX Or YY)(inline g: Y => XX Or YY): XX Or YY = or match
      case a: Alt[_] => g(a.get.asInstanceOf[Y])
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

    /** Operate on the favored value if it exists. */
    inline def foreach(inline f: X => Unit): Unit = or match
      case _: Alt[_] => ()
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

    /** Operate on the disfavored value if it exists. */
    inline def foreachAlt(inline f: Y => Unit): Unit = or match
      case a: Alt[_] => f(a.alt.asInstanceOf[Y])
      case _ => ()

    /** Operate on whichever value exists.
      * 
      * Equivalent to `o.fold( x => f(x) ){ y => g(y) }; ()`
      */
    inline def foreachThem(inline f: X => Unit)(inline g: Y => Unit) = or match
      case a: Alt[_] => g(a.alt.asInstanceOf[Y])
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

    /** Operate on the favored value if it exists, but pass on the original `Or`. */
    inline def use(inline f: X => Unit): or.type =
      or match
        case _: Alt[_] =>
        case _ => f(Is unwrap or.asInstanceOf[Is[X]])
      or

    /** Operate on the disfavored value if it exists, but pass on the original `Or`. */
    inline def useAlt(inline f: Y => Unit): or.type =
      or match
        case a: Alt[_] => f(a.alt.asInstanceOf[Y])
        case _ =>
      or

    /** Operate on whichever value exists, but pass on the original `Or`.
      * 
      * Equivalent to `o.fold{ x => f(x) }{ y => g(y) }; o`
      */
    inline def useThem(inline f: X => Unit)(inline g: Y => Unit): or.type =
      or match
        case a: Alt[_] => g(a.alt.asInstanceOf[Y])
        case _ => f(Is unwrap or.asInstanceOf[Is[X]])
      or

    /** Discards some favored values, when a favored value exists, by converting those into disfavored values.
      *
      * A partial function determines which favored values are to be rejected, and performs the remapping.
      * 
      * Usage example:
      * {{{
      * -5.or[String].discard{ case x if x < 0 => "Negative value" }
      * }}}
      */
    inline def discard[YY >: Y](pf: PartialFunction[X, YY]): X Or YY = or match
      case _: Alt[_] => or.asInstanceOf[Alt[Y]]
      case _ => pf.applyOrElse(Is unwrap or.asInstanceOf[Is[X]], Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
        case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => or
        case yy => Alt(yy.asInstanceOf[YY])

    /** Restores some disfavored values, when a disfavored value exists, by converting them into favored values.
      * 
      * A partial function determines which disfavored values are to be restored, and performs the remapping.
      * 
      * Usage example:
      * {{{
      * '7'.isnt[Int].restore{ case c if c.isDigit => java.lang.Character.digit(c, 10) }
      * }}}
      */
    inline def restore[XX >: X](pf: PartialFunction[Y, XX]): XX Or Y = or match
      case a: Alt[_] => pf.applyOrElse(a.alt.asInstanceOf[Y], Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
        case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => or
        case xx => Is(xx.asInstanceOf[XX])
      case _ => or

    /** Reshapes `(P Or Q) Or Y` into `P Or (Q or Y)`; the latter is recommended as the canonical packing for three alternatives, as it has less boxing for disfavored cases. */
    inline def pivot[P, Q](using (P Or Q) =:= X): P Or (Q Or Y) = or match
      case _: BoxedOr[_] => or match
        case y: Alt[_] => Alt(y.asInstanceOf[Alt[Y]])
        case _ => (Is unwrap or.asInstanceOf[P Or Q]) match
          case q: Alt[_] => Alt(Is(q.alt.asInstanceOf[Q]))
          case p => Is(Is unwrap p.asInstanceOf[Is[P]])
      case _ => or.asInstanceOf[Is[P]]

    /** Reshapes `P Or (U or V)` into `(P Or U) Or V`; the latter is not recommended as the canonical packing for three alternatives, but may be useful at times. */
    inline def unpivot[U, V](using (U Or V) =:= Y): (X Or U) Or V = or match
      case _: BoxedOr[_] => or match
        case y: Alt[_] => y.alt.asInstanceOf[U Or V] match
          case v: Alt[_] => v.asInstanceOf[Alt[V]]
          case u => Is(Alt(Is unwrap u.asInstanceOf[Is[U]]))
        case _ => Is(or.asInstanceOf[Is[X]])
      case _ => or.asInstanceOf[Is[Is[X]]]

    /** An `Or` with favored and disfavored branches swapped. */
    inline def swap: Y Or X = or match
      case _: Alt[_] => Is(or.asInstanceOf[Alt[Y]].alt)
      case _ => Alt(Is unwrap or.asInstanceOf[Is[X]])

    /** Turns this `Or` into an `Ok` maintaining favored and disfavored branches. */
    inline def toOk: Ok[Y, X] = or match
      case a: Alt[_] => No(a.alt.asInstanceOf[Y])
      case _ => Yes(Is unwrap or.asInstanceOf[Is[X]])

    /** Turns this `Or` into an `Ok` swapping favored and disfavored branches. */
    inline def swapToOk: Ok[X, Y] = or match
      case a: Alt[_] => Yes(a.alt.asInstanceOf[Y])
      case _ => No(Is unwrap or.asInstanceOf[Is[X]])

    /** Turns this `Or` into an `Either` maintaining favored and disfavored branches (i.e `Is[X]` becomes `Right[X]`) */
    inline def toEither: Either[Y, X] = or match
      case a: Alt[_] => Left(a.alt.asInstanceOf[Y])
      case _ => Right(Is unwrap or.asInstanceOf[Is[X]])

    /** Turns this `Or` into an `Either` swapping favored and disfavored branches (i.e `Is[X]` becomes `Left[X]`) */
    inline def swapToEither: Either[X, Y] = or match
      case a: Alt[_] => Right(a.alt.asInstanceOf[Y])
      case _ => Left(Is unwrap or.asInstanceOf[Is[X]])

    /** Turns this `Or` into an `Option` by discarding the disfavored branch if present. */
    inline def toOption: Option[X] = or match
      case _: Alt[_] => None
      case _ => Some(Is unwrap or.asInstanceOf[Is[X]])

    /** Turns this `Or` into an `Option` by discarding the favored branch if present. */
    inline def swapToOption: Option[Y] = or match
      case _: Alt[_] => Some(or.asInstanceOf[Alt[Y]].alt)
      case _ => None

    /** Turns this `Or` into a `Try` by packing the disfavored branch into a `WrongBranchException` created for that purpose. */
    inline def toTry: Try[X] = or match
      case a: Alt[_] => Failure(new WrongBranchException(a.alt.asInstanceOf[Y]))
      case _ => Success(Is unwrap or.asInstanceOf[Is[X]])

    /** Turns this `Or` into a `Try` by packing the favored branch into a `WrongBranchException` created for that purpose. */
    inline def swapToTry: Try[Y] = or match
      case a: Alt[_] => Success(a.alt.asInstanceOf[Y])
      case _ => Failure(new WrongBranchException(Is unwrap or.asInstanceOf[Is[X]]))
  }

  extension [A](a: A) {
    /** Uses a value as the favored branch of an `Or` while specifying the type of a disfavored branch. */
    inline def or[Y]: A Or Y = Is(a)

    /** Uses a value as the disfavored branch of an `Or` while specifying the type of a favored branch. */
    inline def isnt[X]: X Or A = Alt(a)

    /** Separates values into favored and disfavored based on a predicate; true means favored. */
    inline def isIf(inline p: A => Boolean): A Or A =
      if p(a) then Is(a) else Alt(a)

    /** Separates values into disfavored and favored based on a predicate; true means disfavored. */
    inline def isntIf(inline q: A => Boolean): A Or A =
      if q(a) then Alt(a) else Is(a)

    /** Maps certain vales into a favored branch based on a partial function; those that don't map are disfavored. */
    inline def orCase[X](pf: PartialFunction[A, X]): X Or A =
      pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
        case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Alt(a)
        case x => Is(x.asInstanceOf[X])

    /** Maps certain values into a disfavored branch based on a partial function; those that don't map are favored. */
    inline def isntCase[Y](pf: PartialFunction[A, Y]): A Or Y =
      pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
        case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Is(a)
        case y => Alt(y.asInstanceOf[Y])
  }
}
