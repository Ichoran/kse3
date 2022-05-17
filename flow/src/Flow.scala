// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2021-22 Rex Kerr, UCSF, and Calico Life Sciences LLC.


package kse.flow

import scala.util.control.NonFatal

import scala.util.{Try, Success, Failure}


///////////////////////////////////////////////
/// Exports of other nicely packaged things ///
///////////////////////////////////////////////

export kse.flow.AorB._


//////////////////////////////////////
/// Early returns with ? a la Rust ///
//////////////////////////////////////

infix trait AutoMap[Y, YY] extends Function1[Y, YY] {}

extension [X, Y](or: X Or Y)
  inline def ?(using TransformsFlow[Alt[Y]]) : X = (or: @unchecked) match
    case y: Alt[Y @unchecked] => throw new UntransformedFlowException(y)
    case _ => Is unwrap or.asInstanceOf[Is[X]]

  inline def ?+[YY](inline f: Y => YY)(using TransformsFlow[Alt[YY]]) : X =
    or.mapAlt(f).?

  inline def ?*[YY](using tr: TransformsFlow[Alt[YY]], m: Y AutoMap YY) : X =
    or.mapAlt(m).?

extension [N, Y](ok: Ok[N, Y])
  inline def ?(using TransformsFlow[No[N]]) : Y = ok match
    case Yes(y) => y
    case n: No[N] => throw new UntransformedFlowException(n)


extension [L, R](either: Either[L, R])
  inline def ?(using TransformsFlow[Left[L, R]]) : R = either match
    case Right(r) => r
    case l: Left[L, R] => throw new UntransformedFlowException(l)

extension [A](option: Option[A])
  inline def ?(using TransformsFlow[None.type]) : A = option match
    case Some(a) => a
    case _ => throw new UntransformedFlowException(None)

extension [A](`try`: Try[A])
  inline def ?(using TransformsFlow[Failure[A]]) : A = `try` match
    case Success(a) => a
    case f: Failure[A] => throw new UntransformedFlowException(f)

extension (double: Double)
  inline def ?(using TransformsFlow[Double]) : Double = double match
    case x if java.lang.Double.isNaN(x) => throw new UntransformedFlowException(x)
    case y => y

extension (float: Float)
  inline def ?(using TransformsFlow[Float]) : Float = float match
    case x if java.lang.Float.isNaN(x) => throw new UntransformedFlowException(x)
    case y => y

inline def Ret[A](inline a: TransformsFlow[A] ?=> A): A =
  ${ EarlyReturnMacro.transform('{a(using TransformsFlow.of[A])}) }

extension (objectOr: Or.type)
  inline def Ret[X, Y](inline x: TransformsFlow[Alt[Y]] ?=> X): X Or Y =
    ${ EarlyReturnMacro.transform('{ val or: X Or Y = Is(x(using TransformsFlow.of[Alt[Y]])); or }) }

extension (objectOk: Ok.type)
  inline def Ret[N, Y](inline y: TransformsFlow[No[N]] ?=> Y): Ok[N, Y] =
    ${ EarlyReturnMacro.transform('{ val ok: Ok[N, Y] = Yes(y(using TransformsFlow.of[No[N]])); ok }) }
  inline def FlatRet[N, Y](inline y: TransformsFlow[No[N]] ?=> Ok[N, Y]): Ok[N, Y] =
    ${ EarlyReturnMacro.transform('{ val ok: Ok[N, Y] = y(using TransformsFlow.of[No[N]]); ok }) }

extension (objectEither: Either.type)
  inline def Ret[L, R](inline r: TransformsFlow[Left[L, R]] ?=> R): Either[L, R] =
    ${ EarlyReturnMacro.transform('{ val either: Either[L, R] = Right(r(using TransformsFlow.of[Left[L, R]])); either }) }
  inline def FlatRet[L, R](inline r: TransformsFlow[Left[L, R]] ?=> Either[L, R]): Either[L, R] =
    ${ EarlyReturnMacro.transform('{ val either: Either[L, R] = r(using TransformsFlow.of[Left[L, R]]); either }) }

extension (objectOption: Option.type)
  inline def Ret[A](inline a: TransformsFlow[None.type] ?=> A): Option[A] =
    ${ EarlyReturnMacro.transform('{ val option: Option[A] = Some(a(using TransformsFlow.of[None.type])); option }) }
  inline def FlatRet[A](inline a: TransformsFlow[None.type] ?=> Option[A]): Option[A] =
    ${ EarlyReturnMacro.transform('{ val option: Option[A] = a(using TransformsFlow.of[None.type]); option }) }

extension (tryObject: Try.type)
  inline def Ret[A](inline a: TransformsFlow[Failure[A]] ?=> A): Try[A] =
    ${ EarlyReturnMacro.transform('{ val tri: Try[A] = Success(a(using TransformsFlow.of[Failure[A]])); tri }) }
  inline def FlatRet[A](inline a: TransformsFlow[Failure[A]] ?=> Try[A]): Try[A] =
    ${ EarlyReturnMacro.transform('{ val tri: Try[A] = a(using TransformsFlow.of[Failure[A]]); tri }) }



/////////////////////////////////////////
/// Validation and exception handling ///
/////////////////////////////////////////


inline def safe[Y](inline y: => Y): Ok[Throwable, Y] =
  try Yes(y)
  catch
    case e if NonFatal(e) => No(e)

inline def nice[N, Y](inline y: => Y)(using cope: Cope[N]): Ok[N, Y] = 
  try Yes(y)
  catch
    case e if NonFatal(e) => No(cope fromThrowable e)


extension (objectOk: Ok.type) {
  inline def Safer[N, Y](erf: Throwable => N)(inline y: Y): Ok[N, Y] =
    ${ EarlyReturnMacro.transform('{ val ok: Ok[N, Y] = try { Yes(y) } catch { case t if scala.util.control.NonFatal(t) => No(erf(t)) }; ok }) }
  inline def Nicer[N, Y](inline y: Y)(using cope: Cope[N]): Ok[N, Y] =
    ${ EarlyReturnMacro.transform('{ try{ Yes(y) } catch { case t if NonFatal(t) => No(cope fromThrowable t) }}) }
}

extension [Y](throwOk: Ok[Throwable, Y])
  def text: Ok[String, Y] = throwOk match
    case y: Yes[Y] => y
    case No(e) => No(e.explain(50))

extension [Y](ok: Ok[Throwable | String, Y])
  inline def why(inline msg: String, lines: Int = 50): Ok[String, Y] =
    inline ok match
      case okt: Ok[Throwable, Y] => okt match
        case y: Yes[Y] => y
        case No(e) => No(msg + "\n" + e.explain(lines))
      case oks: Ok[String, Y] => oks match
        case y: Yes[Y] => y
        case No(e) => No(msg + "\n" + e)


//////////////////////////////////////////
/// Interconversions between sum types ///
//////////////////////////////////////////


extension [L, R](either: Either[L, R]) {
  inline def toOk: Ok[L, R] = either match
    case Right(r) => Yes(r)
    case Left(l)  => No(l)

  inline def toOr: R Or L = Or from either
}

extension [A](`try`: Try[A]) {
  inline def toOk: Ok[Throwable, A] = `try` match
    case Success(s) => Yes(s)
    case Failure(t) => No(t)

  inline def okOr[B](inline f: Throwable => B): Ok[B, A] = `try` match
    case Success(s) => Yes(s)
    case Failure(t) => No(f(t))

  inline def toOr: A Or Throwable = Or from `try`

  inline def explained[E](using cope: Cope[E]) = Or.from(`try`, cope fromThrowable _)
}

extension [A](option: Option[A]) {
  inline def toOk: Ok[Unit, A] = option match
    case Some(a) => Yes(a)
    case _       => Ok.UnitNo

  inline def okOr[B](inline b: => B) = option match
    case Some(a) => Yes(a)
    case _       => No(b)

  inline def toTry: Try[A] = option match
    case Some(a) => Success(a)
    case _ => Failure(new WrongBranchException(None))

  inline def toOr: A Or Unit = Or from option
}

extension [N, Y](ok: Ok[N, Y]) {
  inline def toOr: Y Or N = Or from ok
}


////////////////////////////////////////////////////////
/// Empowering sum types and others to work with Hop ///
////////////////////////////////////////////////////////

extension [A](a: A) {
  inline def hop(using ch: CanHop[A]): Nothing = ch hop a
  inline def hopMap[B](inline f: A => B)(using ch: CanHop[B]): Nothing = ch hop f(a)
  inline def hopIf(inline p: A => Boolean)(using ch: CanHop[A]): A = if (p(a)) ch hop a else a
  inline def hopOrMap[B](pf: PartialFunction[A, B])(using ch: CanHop[A]): B =
    if pf isDefinedAt a then pf(a)
    else ch hop a
}

extension [N, Y](ok: Ok[N, Y]) {
  inline def good[M >: N](using ch: CanHop[M]): Y = ok match
    case Yes(y) => y
    case No(n)  => throw ch hop n
  inline def bad[Z >: Y](using ch: CanHop[Z]): N = ok match
    case No(n)  => n
    case Yes(y) => throw ch hop y
}

extension (objectOk: Ok.type) {
  inline def hops[N, Y](f: CanHop[N] ?=> Y) =
    given ch: Hop.AnyImpl[N] = new Hop.AnyImpl[N]
    try { Yes(f) }
    catch { case h: Hop[_] if ch eq h => No(ch.value) }
  inline def hopsUnit[Y](f: CanHop.Unit ?=> Y) =
    given ch: Hop.UnitImpl = new Hop.UnitImpl
    try { Yes(f) }
    catch { case h: Hop[_] if ch eq h => Ok.UnitNo }
}

extension [L, R](either: Either[L, R]) {
  inline def good[K >: L](using ch: CanHop[K]): R = either match
    case Right(r) => r 
    case Left(l)  => ch hop l
  inline def bad[S >: R](using ch: CanHop[S]): L = either match
    case Left(l)  => l
    case Right(r) => ch hop r
}

extension (objectEither: Either.type)
  inline def hops[L, R](f: CanHop[L] ?=> R) =
    given ch: Hop.AnyImpl[L] = new Hop.AnyImpl[L]
    try { Right(f) }
    catch { case h: Hop[_] if ch eq h => Left(ch.value) }

extension [A](option: Option[A])
  inline def good(using ch: CanHop[Unit]): A = option match
    case Some(a) => a
    case _ => ch hop None

extension (objectOption: Option.type)
  inline def hops[A](f: CanHop.Unit ?=> A) =
    given ch: Hop.UnitImpl = new Hop.UnitImpl
    try { Some(f) }
    catch { case h: Hop[_] if ch eq h => None }
