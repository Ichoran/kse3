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
  * The favored branch is encoded as an opaque type `Is[A]`, while
  * the disfavored branch is encoded in a box (class) `Alt[B]`.
  * 
  * Internally, `Is[X]` is either a bare `X` or a box class `IsWrap[X]`.
  * The latter is only the case if `X` itself is either an `Alt` or
  * another `IsWrap`.
  */
object AorB {
  sealed abstract class WrappedOr[+Z]() {
    def value: Z
  }

  final class IsWrap[+X](val get: X) extends WrappedOr[X]() {
    def value = get
    override def toString = s"Is($get)"
    override def hashCode = -1837305301 + get.##
    override def equals(a: Any) = a match
      case i: IsWrap[_] => get == i.get
      case _ => false
  }

  final class Alt[+Y](val alt: Y) extends WrappedOr[Y]() {
    def value = alt
    override def toString = s"Alt($alt)"
    override def hashCode = -1867746107 + alt.##
    override def equals(a: Any) = a match
      case aa: Alt[_] => alt == aa.alt
      case _ => false
  }
  object Alt {
    extension[Y](alt: Alt[Y]) {
      inline def unwrap: Y = alt.alt
    }

    inline def wrap[Y](y: Y): Alt[Y] = new Alt(y)

    def unapply[X, Y](o: X Or Y): Option[Y] = o match
      case a: Alt[_] => Some(a.asInstanceOf[Alt[Y]].alt)
      case _ => None

    @annotation.targetName("unapplyAny")
    def unapply(a: Any): Option[Any] = a match
      case a: Alt[_] => Some(a.asInstanceOf[Alt[Any]].alt)
      case _ => None

    val unit = Alt(())
  }

  opaque type Is[+X] = X | IsWrap[X]
  object Is {
    extension [X](is: Is[X]) {
      inline def unwrap: X = summonFrom {
        case _: (Is[_] <:< X) => is match
          case i: IsWrap[_] => i.get.asInstanceOf[X]
          case _ => is.asInstanceOf[X]
        case _: (Alt[_] <:< X) => is match
          case i: IsWrap[_] => i.get.asInstanceOf[X]
          case _ => is.asInstanceOf[X]
        case _ => is.asInstanceOf[X]
      }
    }

    inline def apply[X](x: X): Is[X] = inline erasedValue[X] match
      case _: Alt[_] => new IsWrap(x)
      case _ => summonFrom {
        case _: (Is[_] <:< X) => x match
          case _: WrappedOr[_] => new IsWrap(x)
          case _ => x
        case _: (Alt[_] <:< X) => x match
          case w: WrappedOr[_] => new IsWrap(x)
          case _ => x
        case _ => x
      }

    inline def wrap[X](x: X): Is[X] = apply(x)

    def unapply[X, Y](o: X Or Y): Option[X] = o match
      case _: WrappedOr[_] => o match
        case x: IsWrap[_] => Some(x.asInstanceOf[IsWrap[X]].get)
        case _         => None
      case _ => Some(o.asInstanceOf[X])

    @annotation.targetName("unapplyAny")
    def unapply(a: Any): Option[Any] = a match
      case _: WrappedOr[_] => a match
        case x: IsWrap[_] => Some(x.asInstanceOf[IsWrap[Any]].get)
        case _         => None
      case _ => Some(a)

    val unit: Is[Unit] = ()
  }


  infix type Or[+X, +Y] = Is[X] | Alt[Y]
  object Or {
    inline def from[L, R](e: Either[L, R]): R Or L = e match
      case Left(l) => Alt(l)
      case Right(r) => Is(r)

    inline def from[N, Y](ok: Ok[N, Y]): Y Or N = ok match
      case No(n) => Alt(n)
      case Yes(y) => Is(y)

    inline def from[A](o: Option[A]): A Or Unit = o match
      case Some(a) => Is(a)
      case _ => Alt.unit

    inline def from[T](t: Try[T]): T Or Throwable = t match
      case Failure(e) => Alt(e)
      case Success(s) => Is(s)

    inline def from[T, E](t: Try[T], fix: Throwable => E): T Or E = t match
      case Failure(e) => Alt(fix(e))
      case Success(s) => Is(s)

    val defaultApplyOrElse: Any = new Function1[Any, Any] { def apply(a: Any) = this }
  }
  extension [X, Y](or: Or[X, Y]) {
    inline def get: X = or match
      case _: Alt[X] => throw new NoSuchElementException("get when Or is Alt")
      case _ => Is unwrap or.asInstanceOf[Is[X]]

    inline def alt: Y = or match
      case a: Alt[_] => a.alt.asInstanceOf[Y]
      case _         => throw new NoSuchElementException(s"alt when Or is Is")

    inline def value: X | Y = or match
      case w: WrappedOr[_] => w.value.asInstanceOf[X | Y]
      case x               => x.asInstanceOf[X]

    inline def isBoxed: Boolean = or match
      case w: WrappedOr[_] => true
      case _               => false

    inline def fold[Z](inline f: X => Z)(inline g: Y => Z): Z = or match
      case _: Alt[_] => g(or.asInstanceOf[Alt[Y]].alt)
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

    inline def getOrElse[Z >: X](inline f: Y => Z): Z = or match
      case _: Alt[_] => f(or.asInstanceOf[Alt[Y]].alt)
      case _ => Is unwrap or.asInstanceOf[Is[X]]

    inline def altOrElse[Z >: Y](inline f: X => Z): Z = or match
      case a: Alt[_] => a.alt.asInstanceOf[Y]
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

    inline def map[XX](inline f: X => XX): XX Or Y = or match
      case _: Alt[_] => or.asInstanceOf[Alt[Y]]
      case _ => Is(f(Is unwrap or.asInstanceOf[Is[X]]))

    inline def mapAlt[YY](inline f: Y => YY): X Or YY = or match
      case a: Alt[_] => Alt(f(a.alt.asInstanceOf[Y]))
      case x => x.asInstanceOf[Is[X]]

    inline def mapThem[XX, YY](inline f: X => XX)(inline g: Y => YY): XX Or YY = or match
      case a: Alt[_] => Alt(g(a.alt.asInstanceOf[Y]))
      case _ => Is(f(Is unwrap or.asInstanceOf[Is[X]]))

    inline def flatMap[YY >: Y, XX](inline f: X => XX Or YY): XX Or YY = or match
      case _: Alt[_] => or.asInstanceOf[Alt[Y]]
      case _ => f(Is unwrap or.asInstanceOf[Is[X]]) 

    inline def flatMapAlt[XX >: X, YY](inline g: Y => XX Or YY): XX Or YY = or match
      case a: Alt[_] => g(a.get.asInstanceOf[Y])
      case _ => or.asInstanceOf[Is[X]]

    inline def flatMapThem[XX, YY](inline f: X => XX Or YY)(inline g: Y => XX Or YY): XX Or YY = or match
      case a: Alt[_] => g(a.get.asInstanceOf[Y])
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

    inline def foreach(inline f: X => Unit): Unit = or match
      case _: Alt[_] => ()
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

    inline def foreachAlt(inline f: Y => Unit): Unit = or match
      case a: Alt[_] => f(a.alt.asInstanceOf[Y])
      case _ => ()

    inline def foreachThem(inline f: X => Unit)(inline g: Y => Unit) = or match
      case a: Alt[_] => g(a.alt.asInstanceOf[Y])
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])

    inline def use(inline f: X => Unit): or.type =
      or match
        case _: Alt[_] =>
        case _ => f(Is unwrap or.asInstanceOf[Is[X]])
      or

    inline def useAlt(inline f: Y => Unit): or.type =
      or match
        case a: Alt[_] => f(a.alt.asInstanceOf[Y])
        case _ =>
      or

    inline def useThem(inline f: X => Unit)(inline g: Y => Unit): or.type =
      or match
        case a: Alt[_] => g(a.alt.asInstanceOf[Y])
        case _ => f(Is unwrap or.asInstanceOf[Is[X]])
      or

    inline def discard[YY >: Y](pf: PartialFunction[X, YY]): X Or YY = or match
      case _: Alt[_] => or.asInstanceOf[Alt[Y]]
      case _ => pf.applyOrElse(Is unwrap or.asInstanceOf[Is[X]], Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
        case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => or
        case yy => Alt(yy.asInstanceOf[YY])

    inline def restore[XX >: X](pf: PartialFunction[Y, XX]): XX Or Y = or match
      case a: Alt[_] => pf.applyOrElse(a.alt.asInstanceOf[Y], Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
        case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => or
        case xx => Is(xx.asInstanceOf[XX])
      case _ => or

    inline def pivot[P, Q](using (P Or Q) =:= X): P Or (Q Or Y) = or match
      case _: WrappedOr[_] => or match
        case y: Alt[_] => Alt(y.asInstanceOf[Alt[Y]])
        case _ => (Is unwrap or.asInstanceOf[P Or Q]) match
          case q: Alt[_] => Alt(Is(q.alt.asInstanceOf[Q]))
          case p => Is(Is unwrap p.asInstanceOf[Is[P]])
      case _ => or.asInstanceOf[Is[P]]

    inline def unpivot[U, V](using (U Or V) =:= Y): (X Or U) Or V = or match
      case _: WrappedOr[_] => or match
        case y: Alt[_] => y.alt.asInstanceOf[U Or V] match
          case v: Alt[_] => v.asInstanceOf[Alt[V]]
          case u => Is(Alt(Is unwrap u.asInstanceOf[Is[U]]))
        case _ => Is(or.asInstanceOf[Is[X]])
      case _ => or.asInstanceOf[Is[Is[X]]]

    inline def swap: Y Or X = or match
      case _: Alt[_] => Is(or.asInstanceOf[Alt[Y]].alt)
      case _ => Alt(Is unwrap or.asInstanceOf[Is[X]])

    inline def toOk: Ok[Y, X] = or match
      case a: Alt[_] => No(a.alt.asInstanceOf[Y])
      case _ => Yes(Is unwrap or.asInstanceOf[Is[X]])

    inline def toEither: Either[Y, X] = or match
      case a: Alt[_] => Left(a.alt.asInstanceOf[Y])
      case _ => Right(Is unwrap or.asInstanceOf[Is[X]])

    inline def toOption: Option[X] = or match
      case _: Alt[_] => None
      case _ => Some(Is unwrap or.asInstanceOf[Is[X]])

    inline def toTry: Try[X] = or match
      case a: Alt[_] => Failure(new WrongBranchException(a.alt.asInstanceOf[Y]))
      case _ => Success(Is unwrap or.asInstanceOf[Is[X]])
  }

  extension [A](a: A) {
    inline def or[Y]: A Or Y = Is(a)

    inline def isnt[X]: X Or A = Alt(a)

    inline def orIf(inline p: A => Boolean): A Or A =
      if p(a) then Is(a) else Alt(a)

    inline def isntIf(inline q: A => Boolean): A Or A =
      if q(a) then Alt(a) else Is(a)

    inline def orCase[X](pf: PartialFunction[A, X]): X Or A =
      pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
        case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Alt(a)
        case x => Is(x.asInstanceOf[X])

    inline def isntCase[Y](pf: PartialFunction[A, Y]): A Or Y =
      pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
        case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Is(a)
        case y => Alt(y.asInstanceOf[Y])
  }
}
