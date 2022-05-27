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

/** Trait for automatic mapping of disfavored branches using `?*` returns */
infix trait AutoMap[Y, YY] extends Function1[Y, YY] {}

extension [X, Y](or: X Or Y)
  /** Delivers the favored branch or returns, perhaps nonlocally the disfavored branch. */
  inline def ?(using TransformsFlow[Alt[Y]]) : X = (or: @unchecked) match
    case y: Alt[Y @unchecked] => throw new UntransformedFlowException(y)
    case _ => Is unwrap or.asInstanceOf[Is[X]]

  /** Delivers the favored branch or remaps the disfavored branch and returns it perhaps nonlocally. */
  inline def ?+[YY](inline f: Y => YY)(using TransformsFlow[Alt[YY]]) : X =
    or.mapAlt(f).?

  /** Delivers the favored branch or automatically remaps the disfavored branch before returning it perhaps nonlocally. */
  inline def ?*[YY](using tr: TransformsFlow[Alt[YY]], m: Y AutoMap YY) : X =
    or.mapAlt(m).?

extension [N, Y](ok: Ok[N, Y])
  inline def ?(using TransformsFlow[No[N]]) : Y = ok match
    case Yes(y) => y
    case n: No[N] => throw new UntransformedFlowException(n)


extension [L, R](either: Either[L, R])
  /** Delivers the value in Right if it exists, or does a perhaps nonlocal return of the Left branch. */
  inline def ?(using TransformsFlow[Left[L, R]]) : R = either match
    case Right(r) => r
    case l: Left[L, R] => throw new UntransformedFlowException(l)

extension [A](option: Option[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return of `None`. */
  inline def ?(using TransformsFlow[Option[Nothing]]) : A = option match
    case Some(a) => a
    case n: None.type => throw new UntransformedFlowException(n)

extension [A](`try`: Try[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return of the `Failure` branch. */
  inline def ?(using TransformsFlow[Failure[A]]) : A = `try` match
    case Success(a) => a
    case f: Failure[A] => throw new UntransformedFlowException(f)

extension (double: Double)
  /** Delivers the value if it is not NaN, or does a perhaps nonlocal return of NaN if the value is NaN */
  inline def ?(using TransformsFlow[Double]) : Double = double match
    case x if java.lang.Double.isNaN(x) => throw new UntransformedFlowException(x)
    case y => y

extension (float: Float)
  /** Delivers the value if it is not NaN, or does a perhaps nonlocal return of NaN if the value is NaN */
  inline def ?(using TransformsFlow[Float]) : Float = float match
    case x if java.lang.Float.isNaN(x) => throw new UntransformedFlowException(x)
    case y => y

/** Macro to enable returning Double or Float NaN values from within a method.  Must enclose entire mthod.
  *
  * Usage:
  * {{{
  * def nansgn(d: Double): Double = Ret{
  *   if d.? < 0 then -1.0 else 1.0
  * }
  * }}}
  */
inline def Ret[A](inline a: TransformsFlow[A] ?=> A): A =
  ${ EarlyReturnMacro.transform('{a(using TransformsFlow.of[A])}) }

extension (objectOr: Or.type)
  /** Macro to enable Rust-style early error returns into an `Or`.  The value from normal control flow is wrapped in `Is`.
    *
    * Usage:
    * {{{
    * def lastDigit(s: String): Int Or String = Or.Ret{
    *   s.trim
    *     .isIf(_.forall(_.isDigit)).?+("Non numeric: " + _)
    *     .altCase{ case x if x.isEmpty => "Empty" }.?
    *     .takeRight(1)
    *     .toInt
    * }
    * }}}
    */
  inline def Ret[X, Y](inline x: TransformsFlow[Alt[Y]] ?=> X): X Or Y =
    ${ EarlyReturnMacro.transform('{ val or: X Or Y = Is(x(using TransformsFlow.of[Alt[Y]])); or }) }
  /** Macro to enable Rust-style early error returns into an `Or`.  The value from normal control flow must be the same type of `Or`.
    *
    * Usage:
    * {{{
    * def lastDigit(s: String): Int Or String = Or.FlatRet{
    *   s.trim
    *     .isIf(_.forall(_.isDigit)).?+("Non numeric: " + _)
    *     .altCase{ case x if x.isEmpty => "Empty" }.
    *     .map(_.takeRight(1).toInt)
    * }
    * }}}
    */
  inline def FlatRet[X, Y](inline x: TransformsFlow[Alt[Y]] ?=> X Or Y): X Or Y =
    ${ EarlyReturnMacro.transform('{ val or: X Or Y = x(using TransformsFlow.of[Alt[Y]]); or }) }
  /** Macro to enable Rust-style early error returns into an `Or`.  The value from normal control flow is wrapped in `Is`.
    * Any exceptions are converted explicitly by a supplied function mapping `Throwable` to the disfavored case. 
    *
    * Usage:
    * {{{
    * def parseTwice(s: String): Int Or String = Or.Safe(_.toString){
    *   s.toInt.altCase{ case x if x >= 100000 => "Too big: " + x }.? * 2
    * }
    * }}}
    */
  inline def Safe[X, Y](inline erf: Throwable => Y)(inline x: TransformsFlow[Alt[Y]] ?=> X): X Or Y =
    ${ EarlyReturnMacro.transform('{ val or: X Or Y = try { Is(x(using TransformsFlow.of[Alt[Y]])) } catch { case t if NonFatal(t) => Alt(erf(t)) }; or }) }
  /** Macro to enable Rust-style early error returns into an `Or`.  The value from normal control flow is wrapped in `Is`.
    * Any exceptions are caught and converted via a given `Cope`. 
    *
    * Usage:
    * {{{
    * def parseTwice(s: String): Int Or String = Or.Nice{
    *   s.toInt.altCase{ case x if x >= 100000 => "Too big: " + x }.? * 2
    * }
    * }}}
    */
  inline def Nice[X, Y](inline x: TransformsFlow[Alt[Y]] ?=> X)(using cope: Cope[Y]): X Or Y =
    ${ EarlyReturnMacro.transform('{ val or: X Or Y = try { Is(x(using TransformsFlow.of[Alt[Y]])) } catch { case t if NonFatal(t) => Alt(cope fromThrowable t) }; or }) }

extension (objectOk: Ok.type)
  inline def Ret[N, Y](inline y: TransformsFlow[No[N]] ?=> Y): Ok[N, Y] =
    ${ EarlyReturnMacro.transform('{ val ok: Ok[N, Y] = Yes(y(using TransformsFlow.of[No[N]])); ok }) }
  inline def FlatRet[N, Y](inline y: TransformsFlow[No[N]] ?=> Ok[N, Y]): Ok[N, Y] =
    ${ EarlyReturnMacro.transform('{ val ok: Ok[N, Y] = y(using TransformsFlow.of[No[N]]); ok }) }

extension (objectEither: Either.type)
  /** Macro to enable Rust-style early returns of the `Left` branch of an `Either` that match the method's return type.
    * The value from normal control flow is wrapped in a `Right`.
    *
    * Because `Left` and `Right` have two types, this is awkward and not recommended.
    */
  inline def Ret[L, R](inline r: TransformsFlow[Left[L, R]] ?=> R): Either[L, R] =
    ${ EarlyReturnMacro.transform('{ val either: Either[L, R] = Right(r(using TransformsFlow.of[Left[L, R]])); either }) }
  /** Macro to enable Rust-style early returns of the `Left` branch of an `Either` that match the method's return type.
    * The value from normal control flow must be an `Either` of the same type.
    *
    * Because `Left` and `Right` have two types, this is awkward and not recommended.
    */
  inline def FlatRet[L, R](inline r: TransformsFlow[Left[L, R]] ?=> Either[L, R]): Either[L, R] =
    ${ EarlyReturnMacro.transform('{ val either: Either[L, R] = r(using TransformsFlow.of[Left[L, R]]); either }) }

extension (objectOption: Option.type)
  /** Macro to enable Rust-style early returns of the `None` branch of an `Option`.  The value from normal control flow is wrapped `Some`.
    *
    * Usage:
    * {{{
    * def firstLetter(s: String): Option[Char] = Option.Ret{
    *   Option(s).filter(_.nonEmpty).?.head
    * }
    * }}}
    */   
  inline def Ret[A](inline a: TransformsFlow[Option[Nothing]] ?=> A): Option[A] =
    ${ EarlyReturnMacro.transform('{ val option: Option[A] = Some(a(using TransformsFlow.of[Option[Nothing]])); option }) }
  /** Macro to eable Rust-style early returns of the `None` branch of an `Option`.  The value from normal control flow must also be an `Option`.
    *
    * Usage:
    * {{{
    * def firstLetter(s: String): Option[Char] = Option.FlatRet{
    *   val notNull = Option(s).?
    *   if notNull.isEmpty then None else Some(notNull.head)
    * }
    * }}}
    */ 
  inline def FlatRet[A](inline a: TransformsFlow[Option[Nothing]] ?=> Option[A]): Option[A] =
    ${ EarlyReturnMacro.transform('{ val option: Option[A] = a(using TransformsFlow.of[Option[Nothing]]); option }) }

extension (tryObject: Try.type)
  /** Macro to enable Rust-style early returns of the `Failure` branch of a `Try`.  The value from normal control flow is wrapped in `Success`.
    * It does NOT catch any additional exceptions.  Note that normally you'd just want to wrap your entire block in a `Try{}` instead.
    * 
    * Usage:
    * {{{
    * def parseTwice(s: String): Try[Int] = Try.Ret{
    *   Try{ s.toInt }.? * 2
    * }
    * }}}
    */
  inline def Ret[A](inline a: TransformsFlow[Failure[A]] ?=> A): Try[A] =
    ${ EarlyReturnMacro.transform('{ val tri: Try[A] = Success(a(using TransformsFlow.of[Failure[A]])); tri }) }
  /** Macro to enable Rust-style early returns of the `Failure` branch of a `Try`.  The value from normal control flow must also be a `Try`.
    * It does NOT catch any additional exceptions.  Note that normally you'd just want to wrap your entire block in a `Try{}` instead.
    * 
    * Usage:
    * {{{
    * def parseDiv(s: String, value: Int): Try[Int] = Try.FlatRet{
    *   Try{Try{ s.toInt }.? / value}
    * }
    * }}}
    */
  inline def FlatRet[A](inline a: TransformsFlow[Failure[A]] ?=> Try[A]): Try[A] =
    ${ EarlyReturnMacro.transform('{ val tri: Try[A] = a(using TransformsFlow.of[Failure[A]]); tri }) }
  /** Macro to enable Rust-style early returns of the `Failure` branch of a `Try`.  The value from normal control flow is wrapped in `Success`.
    * It DOES catch any additional exceptions.  Note that normally you'd just want to wrap your entire block in a `Try{}` instead.
    * 
    * Usage:
    * {{{
    * def parseDiv(s: String, value: Int): Try[Int] = Try.Safe{
    *   Try{ s.toInt }.? / value
    * }
    * }}}
    */
  inline def Safe[A](inline a: TransformsFlow[Failure[A]] ?=> A): Try[A] =
    ${ EarlyReturnMacro.transform('{ val tri: Try[A] = Try(a(using TransformsFlow.of[Failure[A]])); tri }) }




/////////////////////////////////////////
/// Validation and exception handling ///
/////////////////////////////////////////


/** Run something safely, packing all non-fatal exceptions into a `Throwable`, and returning
  * the no-exception result as the favored branch of an `Or`.
  */
inline def safe[X](inline x: => X): X Or Throwable =
  try Is(x)
  catch
    case e if NonFatal(e) => Alt(e)

/** Run something safely, packing all non-fatal exceptions into the disfavored branch by mapping
  * the Throwable that was created, and returning the result as the favored branch of an `Or`.
  */
inline def safeWith[X, Y](f: Throwable => Y)(inline x: => X): X Or Y =
  try Is(x)
  catch
    case e if NonFatal(e) => Alt(f(e))

/** Run something safely, using a `Cope` to map any non-fatal exceptions into a disfavored branch,
  * and returning the non-exception result as the favored branch of an `Or`.
  */
inline def nice[X, Y](inline x: => X)(using cope: Cope[Y]): X Or Y = 
  try Is(x)
  catch
    case e if NonFatal(e) => Alt(cope fromThrowable e)

/*
extension [Y](ok: Ok[Throwable | String, Y])
  inline def why(inline msg: String, lines: Int = 50): Ok[String, Y] =
    inline ok match
      case okt: Ok[Throwable, Y] => okt match
        case y: Yes[Y] => y
        case No(e) => No(msg + "\n" + e.explain(lines))
      case oks: Ok[String, Y] => oks match
        case y: Yes[Y] => y
        case No(e) => No(msg + "\n" + e)
*/

//////////////////////////////////////////
/// Interconversions between sum types ///
//////////////////////////////////////////


extension [L, R](either: Either[L, R]) {
  /** Converts to an `Or`, placing `Right` as the favored branch */
  inline def toOr: R Or L = Or from either
}

extension [A](`try`: Try[A]) {
  /** Converts to an `Or`, placing a success as the favored branch and a failure as a `Throwable` in the disfavored branch. */
 inline def toOr: A Or Throwable = Or from `try`

  inline def toOrWith[B](f: Throwable => B): A Or B = Or.from(`try`, f)

  inline def niceOr[E](using cope: Cope[E]): A Or E = Or.from(`try`, cope fromThrowable _)
}

extension [A](option: Option[A]) {
  inline def toTry: Try[A] = option match
    case Some(a) => Success(a)
    case _ => Failure(new WrongBranchException(None))

  inline def toOr: A Or Unit = Or from option

  inline def or[B](b: => B): A Or B = Or.fromOrElse(option, b)
}


////////////////////////////////////////////////////////
/// Empowering sum types and others to work with Hop ///
////////////////////////////////////////////////////////

extension [A](a: A) {
  inline def hopWith[B](inline f: A => B)(using ch: CanHop[B]): Nothing = ch hop f(a)
  inline def hopIf(inline p: A => Boolean)(using ch: CanHop[A]): A = if (p(a)) ch hop a else a
  inline def hopCase[B](pf: PartialFunction[A, B])(using ch: CanHop[B]): A =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => a
      case b => ch hop b.asInstanceOf[B]
  inline def hopNotCase[B](pf: PartialFunction[A, B])(using ch: CanHop[A]): B =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => ch hop a
      case b => b.asInstanceOf[B]
}

extension [X, Y](or: X Or Y) {
  inline def getOrHop(using ch: CanHop[Y]): X = or.fold{ x => x }{ ch hop _ }
  inline def altOrHop(using ch: CanHop[X]): Y = or.fold{ ch hop _ }{ y => y }
  inline def hoppit(using ch: CanHop[Y]): X   = or.fold{ x => x }{ ch hop _ }
}

extension[L, R](either: Either[L, R]) {
  inline def rightOrHop(using ch: CanHop[L]): R = either match
    case Right(r) => r
    case Left(l) => ch hop l
  inline def leftOrHop(using ch: CanHop[R]): L = either match
    case Left(l) => l
    case Right(r) => ch hop r
  inline def hoppit(using ch: CanHop[L]): R = either match
    case Right(r) => r
    case Left(l) => ch hop l
}

extension[A](option: Option[A]) {
  inline def getOrHop(using ch: CanHop[Unit]): A = option match
    case Some(a) => a
    case _ => ch.hop(())
  inline def hoppit(using ch: CanHop[Unit]): A = option match
    case Some(a) => a
    case _ => ch.hop(())
}

extension[A](`try`: Try[A]) {
  inline def getOrHop(using ch: CanHop[Throwable]): A = `try` match
    case Success(a) => a
    case Failure(e) => ch hop e

  inline def hopIfSuccess(using ch: CanHop[A]): Throwable = `try` match
    case Success(a) => ch hop a
    case Failure(e) => e

  inline def hoppit(using ch: CanHop[Throwable]): A = `try` match
    case Success(a) => a
    case Failure(e) => ch hop e
}
