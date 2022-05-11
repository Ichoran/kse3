// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022 Rex Kerr and Calico Life Sciences LLC.

package kse.flow

import scala.compiletime.{erasedValue, summonFrom}
import scala.util.NotGiven
import scala.util.{Try, Success, Failure}
import scala.util.control.{NonFatal, ControlThrowable}

object AorB {
  sealed abstract class WrappedOr[+Z]() {
    def value: Z
  }

  final class IsWrap[+X](val get: X) extends WrappedOr[X]() {
    def value = get
    override def toString = s"Is($get)"
    override def hashCode = -1837305302 ^ get.##
    override def equals(a: Any) = a match
      case i: IsWrap[_] => get == i.get
      case _ => false
  }

  final class Alt[+Y](val alt: Y) extends WrappedOr[Y]() {
    def value = alt
    override def toString = s"Alt($alt)"
    override def hashCode = -1867746108 ^ alt.##
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
      case Right(r) => summonFrom {
        case _: (Is[_] <:< R) => Is(r)
        case _: (Alt[_] <:< R) => Is(r)
        case _ => r
      }

    inline def from[N, Y](ok: Ok[N, Y]): Y Or N = ok match
      case No(n) => Alt(n)
      case Yes(y) => summonFrom {
        case _: (Is[_] <:< Y) => Is(y)
        case _: (Alt[_] <:< Y) => Is(y)
        case _ => y
      }

    inline def from[A](o: Option[A]): A Or Unit = o match
      case Some(a) => summonFrom {
        case _: (Is[_] <:< A) => Is(a)
        case _: (Alt[_] <:< A) => Is(a)
        case _ => a
      }
      case _ => Alt.unit

    inline def from[T](t: Try[T]): T Or Throwable = t match
      case Failure(e) => Alt(e)
      case Success(s) => summonFrom {
        case _: (Is[_] <:< T) => Is(s)
        case _: (Alt[_] <:< T) => Is(s)
        case _ => s
      }

    inline def from[T, E](t: Try[T], fix: Throwable => E): T Or E = t match
      case Failure(e) => Alt(fix(e))
      case Success(s) => summonFrom {
        case _: (Is[_] <:< T) => Is(s)
        case _: (Alt[_] <:< T) => Is(s)
        case _ => s
      }
  }
  extension [X, Y](or: Or[X, Y]) {
    inline def get: X = summonFrom {
      case _: (Is[_] <:< X) => or match
        case _: WrappedOr[_] => or match
          case i: IsWrap[_] => i.get.asInstanceOf[X]
          case _ => throw new NoSuchElementException("get when Or is Alt")
        case _ => or.asInstanceOf[X]
      case _: (Alt[_] <:< X) => or match
        case _: WrappedOr[_] => or match
          case i: IsWrap[_] => i.get.asInstanceOf[X]
          case _ => throw new NoSuchElementException("get when Or is Alt")
        case _ => or.asInstanceOf[X]
      case _ => or match
        case _: Alt[_] => throw new NoSuchElementException("get when Or is Alt")
        case _ => or.asInstanceOf[X]
    }

    inline def alt: Y = or match
      case a: Alt[_] => a.alt.asInstanceOf[Y]
      case _         => throw new NoSuchElementException(s"alt when Or is Is")

    inline def value: X | Y = or match
      case w: WrappedOr[_] => w.value.asInstanceOf[X | Y]
      case x               => x.asInstanceOf[X]

    inline def isBoxed: Boolean = or match
      case w: WrappedOr[_] => true
      case _               => false

    inline def map[XX](inline f: X => XX): XX Or Y = summonFrom {
      case _: (Is[_] <:< X) => or match
        case _: Alt[_] => or.asInstanceOf[Alt[Y]]
        case _ => Is(f(or match { case i: IsWrap[_] => i.get.asInstanceOf[X]; case _ => or.asInstanceOf[X] }))
      case _: (Alt[_] <:< X) => or match
        case _: Alt[_] => or.asInstanceOf[Alt[Y]]
        case _ => Is(f(or match { case i: IsWrap[_] => i.get.asInstanceOf[X]; case _ => or.asInstanceOf[X] }))
      case _ =>
        if or.isInstanceOf[WrappedOr[_]] then or.asInstanceOf[Alt[Y]]
        else Is(f(or.asInstanceOf[X]))
    }

    inline def mapAlt[YY](inline f: Y => YY): X Or YY = or match
      case a: Alt[_] => Alt(f(a.alt.asInstanceOf[Y]))
      case x => x.asInstanceOf[Is[X]]

    inline def flatMap[YY >: Y, XX](inline f: X => XX Or YY): XX Or YY = or match
      case w: WrappedOr[_] => w match
        case i: IsWrap[_] => f(i.get.asInstanceOf[X])
        case y       => y.asInstanceOf[Alt[Y]]
      case x => f(x.asInstanceOf[X])

    inline def flatMapAlt[XX >: X, YY](inline g: Y => XX Or YY): XX Or YY = or match
      case y: Alt[_] => g(y.get.asInstanceOf[Y])
      case _ => or.asInstanceOf[Is[X]]

    inline def pivot[P, Q](using (P Or Q) =:= X): P Or (Q Or Y) = or match
      case y: Alt[_] => Alt(y.asInstanceOf[Alt[Y]])
      case i: IsWrap[_] => i.get.asInstanceOf[P Or Q] match
        case q: Alt[_] => q.asInstanceOf[Alt[Q Or Y]]
        case p => p.asInstanceOf[Is[P]]
      case _ => or.asInstanceOf[Is[P]]

    inline def fold[Z](inline f: X => Z)(inline g: Y => Z): Z = or match
      case w: WrappedOr[_] => w match
        case y: Alt[_] => g(y.alt.asInstanceOf[Y])
        case _ => f(or.asInstanceOf[IsWrap[X]].get)
      case _ => f(or.asInstanceOf[X])
  }

  extension [X](x: X)
    inline def or[Y]: X Or Y = Is(x)

  extension [Y](y: Y)
    inline def isnt[X]: X Or Y = Alt(y)
}
