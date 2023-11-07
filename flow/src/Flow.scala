// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2021-23 Rex Kerr, UCSF, and Calico Life Sciences LLC.


package kse.flow

import scala.annotation.targetName

import scala.util.control.ControlThrowable
import scala.util.boundary
import scala.util.boundary.Label
import scala.util.boundary.break

import scala.util.{Try, Success, Failure}



//////////////////////////////////////
/// Early returns with ? a la Rust ///
//////////////////////////////////////

/** Trait for automatic mapping of disfavored branches using `?*` returns and `autobreak` */
infix trait AutoMap[Y, YY] extends Function1[Y, YY] {}

extension [X, Y](or: X Or Y)
  /** Delivers the favored branch or returns, perhaps nonlocally the disfavored branch. */
  inline def ?[L >: Alt[Y]](using Label[L]): X = (or: @unchecked) match
    case y: Alt[Y @unchecked] => boundary.break(y)
    case _ => Is unwrap or.asInstanceOf[Is[X]]

  /** Delivers the favored branch or remaps the disfavored branch and returns it perhaps nonlocally. */
  inline def ?+[YY, L >: Alt[YY]](inline f: Y => YY)(using Label[L]): X =
    or.mapAlt(f).?

  /** Delivers the favored branch or automatically remaps the disfavored branch before returning it perhaps nonlocally. */
  inline def ?*[YY, L >: Alt[YY]](using lb: Label[L], m: Y AutoMap YY): X =
    or.mapAlt(m).?

extension [L, R](either: Either[L, R])
  /** Delivers the value in Right if it exists, or does a perhaps nonlocal return of the Left branch. */
  inline def ?[E >: Left[L, R]](using Label[E]): R = either match
    case Right(r) => r
    case l: Left[L, R] => boundary.break(l)

extension [A](option: Option[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return of `None`. */
  inline def ?[N >: None.type](using Label[N]): A = option match
    case Some(a) => a
    case n: None.type => boundary.break(n)

extension [A](iterator: Iterator[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return of `Unit`. */
  inline def ?(using Label[Unit]): A =
    if iterator.hasNext then iterator.next else boundary.break()

extension [A](stepper: scala.collection.Stepper[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return of `Unit`. */
  inline def ?(using Label[Unit]): A =
    if stepper.hasStep then stepper.nextStep else boundary.break()

extension [A](iterator: java.util.Iterator[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return of `Unit`. */
  inline def ?(using Label[Unit]): A =
    if iterator.hasNext then iterator.next else boundary.break()

extension [A](enumerator: java.util.Enumeration[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return of `Unit`. */
  inline def ?(using Label[Unit]): A =
    if enumerator.hasMoreElements then enumerator.nextElement else boundary.break()

extension (double: Double)
  /** Delivers the value if it is not NaN, or does a perhaps nonlocal return of NaN if the value is NaN */
  inline def ?(using Label[Double]): Double = double match
    case x if java.lang.Double.isNaN(x) => boundary.break(x)
    case y => y

extension (float: Float)
  /** Delivers the value if it is not NaN, or does a perhaps nonlocal return of NaN if the value is NaN */
  inline def ?(using Label[Double]): Float = float match
    case x if java.lang.Float.isNaN(x) => boundary.break(Double.NaN)
    case y => y

extension (test: Boolean)
  /** Does a perhaps nonlocal return if the test value is false */
  inline def ?(using Label[Unit]): Unit =
    if !test then boundary.break()


/** Enables early returns in side-effecting code.  Returns true if completed normally.
  *
  * Usage:
  * {{{
  * def printFirstThree[A](ia: Iterator[A]) = escape:
  *   println(ia.?)
  *   println(ia.?)
  *   println(ia.?)
  * }}}
  */
inline def escape(inline f: Label[Unit] ?=> Unit): Boolean =
  var executionComplete = false
  boundary{ f; executionComplete = true }
  executionComplete


/** Enables returning Double or Float NaN values from within a method.  Must enclose entire mthod.
  *
  * Usage:
  * {{{
  * def nansgn(d: Double) = calculate:
  *   if d.? < 0 then -1.0 else 1.0
  * }}}
  */
inline def calculate(inline a: Label[Double] ?=> Double): Double = boundary{ a }

extension (objectOr: Or.type) {
  /** Enables Rust-style early error returns into an `Or`.  The value from normal control flow is wrapped in `Is`.
    *
    * Usage:
    * {{{
    * def lastDigit(s: String): Int Or String = Or.Ret {
    *   s.trim
    *     .isIf(_.forall(_.isDigit)).?+("Non numeric: " + _)
    *     .altCase{ case x if x.isEmpty => "Empty" }.?
    *     .takeRight(1)
    *     .toInt
    * }
    * }}}
    */
  inline def Ret[X, Y](inline x: Label[X Or Y] ?=> X): X Or Y = boundary{ Is(x) }

  /** Enables Rust-style early error returns into an `Or`.  The value from normal control flow must be the same type of `Or`.
    *
    * Usage:
    * {{{
    * def lastDigit(s: String): Int Or String = Or.FlatRet {
    *   s.trim
    *     .isIf(_.forall(_.isDigit)).?+("Non numeric: " + _)
    *     .altCase{ case x if x.isEmpty => "Empty" }.
    *     .map(_.takeRight(1).toInt)
    * }
    * }}}
    */
  inline def FlatRet[X, Y](inline xy: Label[X Or Y] ?=> X Or Y): X Or Y = boundary{ xy }

  /** Enables Rust-style early error returns into an `Or`.  The value from normal control flow is wrapped in `Is`.
    * Any exceptions are converted explicitly by a supplied function mapping `Throwable` to the disfavored case. 
    *
    * Usage:
    * {{{
    * def parseTwice(s: String): Int Or String = Or.Safe(_.toString) {
    *   s.toInt.altCase{ case x if x >= 100000 => "Too big: " + x }.? * 2
    * }
    * }}}
    */
  inline def Safe[X, Y](inline erf: Throwable => Y)(inline x: Label[X Or Y] ?=> X): X Or Y = boundary[X Or Y]{
    try Is(x)
    catch case t if t.catchable => Alt(erf(t))
  }

  /** Enables Rust-style early error returns into an `Or`.  The value from normal control flow is wrapped in `Is`.
    * Any exceptions are caught and converted into `Err`. 
    *
    * Usage:
    * {{{
    * def parseTwice(s: String): Int Or String = Or.Nice {
    *   s.toInt.altCase{ case x if x >= 100000 => "Too big: " + x }.? * 2
    * }
    * }}}
    */
  inline def Nice[X](inline x: Label[X Or Err] ?=> X): X Or Err = boundary[X Or Err]{
    try Is(x)
    catch case t if t.catchable => Alt(Err(t))
  }
}


extension (objectEither: Either.type){
  /** Enables Rust-style early returns of the `Left` branch of an `Either` that match the method's return type.
    * The value from normal control flow is wrapped in a `Right`.
    *
    * Because `Left` and `Right` have two types, this is awkward and not recommended.
    */
  inline def Ret[L, R](inline r: Label[Either[L, R]] ?=> R): Either[L, R] = boundary{ Right[L, R](r) }

  /** Enables Rust-style early returns of the `Left` branch of an `Either` that match the method's return type.
    * The value from normal control flow must be an `Either` of the same type.
    *
    * Because `Left` and `Right` have two types, this is awkward and not recommended.
    */
  inline def FlatRet[L, R](inline r: Label[Either[L, R]] ?=> Either[L, R]): Either[L, R] = boundary{ r }
}


extension (objectOption: Option.type) {
  /** Enables Rust-style early returns of the `None` branch of an `Option`.  The value from normal control flow is wrapped `Some`.
    *
    * Usage:
    * {{{
    * def firstLetter(s: String): Option[Char] = Option.Ret{
    *   Option(s).filter(_.nonEmpty).?.head
    * }
    * }}}
    */   
  inline def Ret[A](inline a: Label[Option[A]] ?=> A): Option[A] = boundary{ Some(a) }

  /** Enables Rust-style early returns of the `None` branch of an `Option`.  The value from normal control flow must also be an `Option`.
    *
    * Usage:
    * {{{
    * def firstLetter(s: String): Option[Char] = Option.FlatRet{
    *   val notNull = Option(s).?
    *   if notNull.isEmpty then None else Some(notNull.head)
    * }
    * }}}
    */ 
  inline def FlatRet[A](inline a: Label[Option[A]] ?=> Option[A]): Option[A] = boundary{ a }
}



/////////////////////////////////////////
/// Validation and exception handling ///
/////////////////////////////////////////


/** Run something safely, packing all non-fatal exceptions into a `Throwable`, and returning
  * the no-exception result as the favored branch of an `Or`.
  */
inline def safe[X](inline x: => X): X Or Throwable =
  try Is(x)
  catch case e if e.catchable => Alt(e)

/** Run something safely, packing all non-fatal exceptions into the disfavored branch by mapping
  * the Throwable that was created, and returning the result as the favored branch of an `Or`.
  */
inline def safeWith[X, Y](f: Throwable => Y)(inline x: => X): X Or Y =
  try Is(x)
  catch case e if e.catchable => Alt(f(e))

/** Run something safely, packing all non-fatal exceptions into an `Err`, and returning
  * the no-exception result as the favored branch of an `Or`.
  */
inline def nice[X](inline x: => X): X Or Err =
  try Is(x)
  catch case e if e.catchable => Alt(Err(e))    

/** Run something safely, using a `Cope` to map any non-fatal exceptions into a disfavored branch,
  * and returning the non-exception result as the favored branch of an `Or`.
  */
inline def cope[X, Y](inline x: => X)(using cope: Cope[Y]): X Or Y = 
  try Is(x)
  catch case e if e.catchable => Alt(cope fromThrowable e)

/** Run something safely, also catching any control constructs that might escape. */
inline def threadsafe[X](inline x: => X): X Or Throwable =
  try Is(x)
  catch case e if e.threadCatchable => Alt(e)

/** Run something safely and pack errors into an Err, also catching any control constructs that might escape. */
inline def threadnice[X](inline x: => X): X Or Err =
  try Is(x)
  catch case e if e.threadCatchable => Alt(Err(e))

/** Run something safely, using a `Cope` to map any non-fatal exceptions into a disfavored branch,
  * and returning the non-exception result as the favored branch of an `Or`.
  */
inline def threadcope[X, Y](inline x: => X)(using cope: Cope[Y]): X Or Y = 
  try Is(x)
  catch case e if e.threadCatchable => Alt(cope fromThrowable e)

/** Run something safely, with a pregenerated value if things go wrong */
inline def ratchet[A](default: A)(inline f: A => A): A =
  try f(default)
  catch case e if e.catchable => default


extension [A](or: A Or Err)
  inline def niceMap[B](f: A => B): B Or Err =
    or.flatMap(a => Err.nice(f(a)))

extension [A, E](or: A Or E)
  def copeMap[B](f: A => B)(using cope: Cope[E]): B Or E =
    or.flatMap{ a =>
      try Is(f(a))
      catch case t if t.catchable => Alt(cope fromThrowable t)
    }


//////////////////////////////////////////
/// Interconversions between sum types ///
//////////////////////////////////////////

extension [Y](alt: Alt[Y]) {
  /** Put value into Left */
  inline def toEither: Either[Y, Nothing] =
    Left(alt.alt)

  /** Put value into Right */
  inline def swapToEither: Either[Nothing, Y] =
    Right(alt.alt)

  /** Discard value, return None */
  inline def toOption: Option[Nothing] =
    None

  /** Store value in Option */
  inline def swapToOption: Option[Y] =
    Some(alt.alt)

  /** Put value into WrongBranchException and pack in Try */
  inline def toTry: Try[Nothing] =
    Failure(new WrongBranchException(alt.alt))

  /** Store value in Try as a Success */
  inline def swapToTry: Try[Y] =
    Success(alt.alt)
}

extension [X](is: Is[X]) {
  /** Put value into Right */
  inline def toEither: Either[Nothing, X] =
    Right(Is unwrap is)

  /** Put value into Left */
  inline def swapToEither: Either[X, Nothing] =
    Left(Is unwrap is)

  /** Put value into Option */
  inline def toOption: Option[X] =
    Some(Is unwrap is)

  /** Discard value and return None */
  inline def swapToOption: Option[Nothing] =
    None

  /** Put value into Try */
  inline def toTry: Try[X] =
    Success(Is unwrap is)

  /** Pack value into WrongBranchException and return as a Try */
  inline def swapToTry: Try[Nothing] =
    Failure(new WrongBranchException(Is unwrap is))
}

extension [X, Y](or: X Or Y) {
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

  /** Tries to get this value, throwing the disfavored branch directly, if possible. */
  @targetName("grabXOrY")
  inline def grab: X = or.getOrElse{ a => (a: Any) match
    case s: String => throw new ErrType.StringErrException(s)
    case t: Throwable => { if t.catchable then throw t else throw ErrType.CatchableException("", t) }
    case e: ErrType => throw e.toThrowable
    case y => throw new WrongBranchException(y)
  }
}


extension [X](or: X Or Err)
  @targetName("grabXOrErr")
  inline def grab: X = or.getOrElse(_.toss)


extension [L, R](either: Either[L, R]) {
  /** Converts to an `Or`, placing `Right` as the favored branch */
  inline def toOr: R Or L = Or from either
}


extension [A](`try`: Try[A]) {
  /** Converts to an `Or`, placing a success as the favored branch and a failure as a `Throwable` in the disfavored branch. */
  inline def toOr: A Or Throwable = Or from `try`

  /** Converts to an `Or`, mapping a failure to a disfavored value, or keeping a success as the favored branch. */
  inline def toOrWith[B](f: Throwable => B): A Or B = Or.from(`try`, f)

  /** Converts to an `Or`, storing failures in `Err`, or keeping a success as the favored branch. */
  inline def niceOr: A Or Err = Or.from(`try`, t => Err(t))

  /** Converts to an `Or`, mapping a failure automatically to a disfavored value, or keeping a success as the favored branch. */
  inline def copeOr[E](using cope: Cope[E]): A Or E = Or.from(`try`, cope fromThrowable _)
}


extension [A](option: Option[A]) {
  /** Converts to a `Try` by creating a new `None`-containing WrongBranchException if the `Option` is empty. */
  inline def toTry: Try[A] = option match
    case Some(a) => Success(a)
    case _ => Failure(new WrongBranchException(None))

  /** Converts to an Or by using a value as the favored branch and unit as the disfavored branch if there is no value */
  inline def toOr: A Or Unit = Or from option

  /** Converts to an Or by using a default value for the disfavored branch if there is no value */
  inline def toOrElse[B](b: => B): A Or B = Or.fromOrElse(option, b)
}


extension [A, B](pf: PartialFunction[A, B]) {
  inline def applyOr(a: A): B Or Unit =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Alt.unit
      case b => Is(b.asInstanceOf[B])

  inline def applyOption(a: A): Option[B] =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => None
      case b => Some(b.asInstanceOf[B])

  inline def applyOrBreak(a: A)(using lb: Label[Unit]): B =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => boundary.break()
      case b => b.asInstanceOf[B]

  inline def liftToOr: A => B Or Unit =
    (a: A) => pf.applyOr(a)

  inline def liftToOption: A => Option[B] = pf.lift

  inline def liftToTry: A => Try[B] =
    (a: A) => Try(pf(a))

  inline def liftToEither: A => Either[Unit, B] =
    (a: A) => pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Left[Unit, B](())
      case b => Right[Unit, B](b.asInstanceOf[B])
}


extension [A, B](f: A => (B Or Unit)) {
  inline def unlift: PartialFunction[A, B] = new PartialFunction[A, B] {
    override def applyOrElse[AA <: A, BB >: B](a: AA, default: AA => BB): BB = f(a).fold(b => b)(_ => default(a))
    def isDefinedAt(a: A) = f(a).isIs
    def apply(a: A) = f(a).get
  }
}


extension [A, B](f: A => Either[Unit, B]) {
  @targetName("unliftEither")
  def unlift: PartialFunction[A, B] = new PartialFunction[A, B] {
    override def applyOrElse[AA <: A, BB >: B](a: AA, default: AA => BB): BB = f(a) match
      case Right(b) => b
      case _        => default(a)
    def isDefinedAt(a: A) = f(a).isRight
    def apply(a: A) = f(a) match
      case Right(b) => b
      case _  => throw new NoSuchElementException("Result not defined for this argument")
  }
}



extension [A, B](f: A => Try[B]) {
  @targetName("unliftTry")
  def unlift: PartialFunction[A, B] = new PartialFunction[A, B] {
    override def applyOrElse[AA <: A, BB >: B](a: AA, default: AA => BB): BB = f(a) match
      case scala.util.Success(b) => b
      case _                     => default(a)
    def isDefinedAt(a: A) = f(a).isSuccess
    def apply(a: A) = f(a).get
  }
}



//////////////////////////////////////////////////////////////////
/// Empowering sum types and others to work with postfix break ///
//////////////////////////////////////////////////////////////////

extension [A](a: A) {
  /** Exit to a boundary that is this type */
  inline def break(using Label[A]): Nothing = boundary.break(a)

  /** Exit to a boundary that we can map from this type */
  inline def autobreak[B](using am: AutoMap[A, B], l: Label[B]) = boundary.break(am(a))

  /** Remap this value with a lambda, and then exit to boundary with that new value */
  inline def breakWith[B](inline f: A => B)(using Label[B]): Nothing = boundary.break(f(a))

  /** Exit to boundary with this value if a predicate is met, otherwise keep going with the same value */
  inline def breakIf(inline p: A => Boolean)(using Label[A]): A = if p(a) then boundary.break(a) else a

  /** Exit to boundary with this value if a predicate is not met, otherwise keep going with the same value */
  inline def breakIfNot(inline p: A => Boolean)(using Label[A]): A = if p(a) then a else boundary.break(a)

  /** Remap some of these values and exit to boundary with if the remap succeeded; otherwise keep going with the same value */
  inline def breakCase[B](pf: PartialFunction[A, B])(using Label[B]): A =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => a
      case b => boundary.break(b.asInstanceOf[B])

  /** Remap some of these values, or exit to boundary with the value if it wasn't remapped */
  inline def breakNotCase[B](pf: PartialFunction[A, B])(using Label[A]): B =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => boundary.break(a)
      case b => b.asInstanceOf[B]
}

extension [X, Y](or: X Or Y) {
  /** Exit to boundary matching the disfavored branch, or keep going with the favored branch's value.  Same as `.?` */
  inline def getOrBreak(using Label[Y]): X = or.fold{ x => x }{ y => boundary.break(y) }

  /** Exit to boundary matching mapping of the disfavored branch, or keep going with the favored branch's value.  Same as `.?+` */
  inline def getOrBreakWith[Z](inline f: Y => Z)(using Label[Z]): X = or.fold{ x => x }{ y => boundary.break(f(y)) }

  /** Exit to boundary matching automatically mapped disfavored branch, or keep going with the favored branch's value.  Same as `.?*` */
  inline def getOrAutoBreak[Z](using am: AutoMap[Y, Z], l: Label[Z]): X = or.fold{ x => x }{ y => boundary.break(am(y)) }

  /** Exit to boundary matching the favored branch, or keep going with the disfavored branch's value */
  inline def altOrBreak(using Label[X]): Y = or.fold{ x => boundary.break(x) }{ y => y }

  /** Exit to boundary matching the favored branch, or keep going with the disfavored branch's value */
  inline def altOrBreakWith[W](inline f: X => W)(using Label[W]): Y = or.fold{ x => boundary.break(f(x)) }{ y => y }

  /** Exit to boundary matching the favored branch, or keep going with the disfavored branch's value */
  inline def altOrAutoBreak[W](using am: AutoMap[X, W], l: Label[W]): Y = or.fold{ x => boundary.break(am(x)) }{ y => y }
}

extension [L, R](either: Either[L, R]) {
  /** Exit to boundary matching the left branch, or keep going with the right branch's value. */
  inline def rightOrBreak(using Label[L]): R = either match
    case Right(r) => r
    case Left(l) => boundary.break(l)

  /** Exit to boundary matching the right branch, or keep going with the left branch's value. */
  inline def leftOrBreak(using Label[R]): L = either match
    case Left(l) => l
    case Right(r) => boundary.break(r)
}

extension [A](option: Option[A]) {
  /** Exit to unit-type boundary if the option is empty, or keep going with the option's value. */
  inline def getOrBreak(using Label[Unit]): A = option match
    case Some(a) => a
    case _ => break(())
}


opaque type Attempt[+A] = A Or Unit
object Attempt {
  opaque type Passed = Unit
  object Passed {
    inline def value: Passed = ()

    extension (passed: Passed)
      inline def &&[A](inline a: => A) = a
  }

  inline def failed: Attempt[Nothing] = Alt.unit

  inline def asAttempt[A](or: A Or Unit): Attempt[A] = or

  extension [A](att: Attempt[A]) {
    inline def underlying: A Or Unit = att
    inline def default(inline a: => A): A = att.getOrElse(_ => a)
  }

  extension [A](att: kse.flow.Attempt[A]) {
    inline def attempt(inline f: Label[Attempt[A]] ?=> A): Attempt[A] =
      att || kse.flow.attempt(f).underlying
    inline def safe(inline f: Label[Attempt[A]] ?=> A): Attempt[A] =
      att || {
        try kse.flow.attempt(f).underlying
        catch case e if e.catchable => Attempt.failed: Attempt[A]
      }
  }
}
object attempt {
  inline def apply[A](inline f: Label[Attempt[A]] ?=> A): Attempt[A] =
    boundary[Attempt[A]]:
      Attempt asAttempt Is(f)

  inline def safe[A](inline f: Label[Attempt[A]] ?=> A): Attempt[A] =
    try apply(f)
    catch case e if e.catchable => Attempt.failed: Attempt[A]

  inline def threadsafe[A](inline f: Label[Attempt[A]] ?=> A): Attempt[A] =
    try apply(f)
    catch case e if e.threadCatchable => Attempt.failed: Attempt[A]
}

extension [X, Y](or: X Or Y)
  inline def ![A](using Label[Attempt[A]]): X =
    or.getOrElse(_ => break(Attempt.failed))

extension [L, R](either: Either[L, R])
  inline def ![A](using Label[Attempt[A]]): R = either match
    case Right(r) => r
    case _        => break(Attempt.failed)

extension [O](option: Option[O])
  inline def ![A](using Label[Attempt[A]]): O = option match
    case Some(o) => o
    case _       => break(Attempt.failed)

extension [T](`try`: Try[T])
  inline def ![A](using Label[Attempt[A]]): T = `try` match
    case Success(t) => t
    case _          => break(Attempt.failed)

extension [I](iterator: Iterator[I])
  inline def ![A](using Label[Attempt[A]]): I =
    if iterator.hasNext then iterator.next
    else break(Attempt.failed)

extension [I](stepper: scala.collection.Stepper[I])
  inline def ![A](using Label[Attempt[A]]): I =
    if stepper.hasStep then stepper.nextStep
    else break(Attempt.failed)

extension [I](iterator: java.util.Iterator[I])
  inline def ![A](using Label[Attempt[A]]): I =
    if iterator.hasNext then iterator.next
    else break(Attempt.failed)

extension [I](enumerator: java.util.Enumeration[I])
  inline def ![A](using Label[Attempt[A]]): I =
    if enumerator.hasMoreElements then enumerator.nextElement
    else break(Attempt.failed)

inline def ensure[A](inline z: Boolean)(using Label[Attempt[A]]): kse.flow.Attempt.Passed =
  if z then Attempt.Passed.value
  else break(Attempt.failed)
