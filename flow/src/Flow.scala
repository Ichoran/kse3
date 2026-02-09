// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2021-25 Rex Kerr, UCSF, and Calico Life Sciences LLC.

package kse.flow


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.annotation.targetName

import scala.util.control.ControlThrowable
import scala.util.boundary
import scala.util.boundary.Label

import scala.util.{Try, Success, Failure}


import kse.basics.{given, _}

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

  /** Delivers the favored branch or jumps to a skip boundary if not. */
  inline def skip_?[S >: shortcut.Skips.type <: shortcut.Type](using lb: Label[S]): X = (or: @unchecked) match
    case y: Alt[?] => boundary.break(shortcut.Skips: S)
    case _ => Is unwrap or.asInstanceOf[Is[X]]

  /** Delivers the favored branch or jumps to a quit boundary if not. */
  inline def quit_?[Q >: shortcut.Quits.type <: shortcut.Type](using lb: Label[Q]): X = (or: @unchecked) match
    case y: Alt[?] => boundary.break(shortcut.Quits: Q)
    case _ => Is unwrap or.asInstanceOf[Is[X]]

  /** Delivers the favored branch or jumps to a loop/escape boundary if not. */
  inline def escape_?(using lb: Label[escape.Token]): X = (or: @unchecked) match
    case y: Alt[?] => boundary.break(escape.Token())
    case _ => Is unwrap or.asInstanceOf[Is[X]]

extension [A](or: A Or Err)
  /** Adds an explanation for an error (independent of error content) */
  inline def ?#[L >: Alt[Err]](inline msg: String)(using Label[L]): A = (or: @unchecked) match
    case e: Alt[Err] => boundary.break(Alt(e.alt explainBy msg))
    case _ => Is unwrap or.asInstanceOf[Is[A]]


extension [L, R](either: Either[L, R])
  /** Delivers the value in Right if it exists, or does a perhaps nonlocal return of the Left branch. */
  inline def ?[E >: Left[L, Nothing]](using Label[E]): R = either match
    case Right(r) => r
    case l: Left[L, R] => boundary.break(l.asInstanceOf[E])

  /** Delivers the value in Right if it exists, or remaps the left branch and returns it perhaps nonlocally. */
  inline def ?+[LL, E >: Left[LL, Nothing]](inline f: L => LL)(using Label[E]): R = either match
    case Right(r) => r
    case Left(l) => boundary.break(Left[LL, Nothing](f(l)))

  /** Delivers the favored branch or jumps to a skip boundary if not. */
  inline def skip_?[S >: shortcut.Skips.type <: shortcut.Type](using lb: Label[S]): R = either match
    case Right(r) => r
    case _ => boundary.break(shortcut.Skips: S)

  /** Delivers the favored branch or jumps to a quit boundary if not. */
  inline def quit_?[Q >: shortcut.Quits.type <: shortcut.Type](using lb: Label[Q]): R = either match
    case Right(r) => r
    case _ => boundary.break(shortcut.Quits: Q)

  /** Delivers the favored branch or jumps to a loop/escape boundary if not. */
  inline def escape_?(using lb: Label[escape.Token]): R = either match
    case Right(r) => r
    case _ => boundary.break(escape.Token())


extension [A](option: Option[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return of `None` or `()`. */
  inline def ? : A = option match
    case Some(a) => a
    case _ => compiletime.summonFrom {
      case ln: Label[None.type]    => boundary.break(None)(using ln)
      case lo: Label[Option[?]]    => boundary.break(None)(using lo)
      case le: Label[escape.Token] => boundary.break(escape.Token())(using le)
      case _                       => compiletime.error("No Label found with Unit or None target")
    }

  /** Delivers the value if it exists, or does a perhaps nonlocal return of a default value. */
  inline def ?+[L](inline l: L)(using Label[L]): A = option match
    case Some(a) => a
    case _ => boundary.break(l)

  /** Gets the value or jumps to a skip boundary. */
  inline def skip_?[S >: shortcut.Skips.type <: shortcut.Type](using lb: Label[S]): A = option match
    case Some(a) => a
    case _ => boundary.break(shortcut.Skips: S)

  /** Gets the value or jumps to a quit boundary. */
  inline def quit_?[Q >: shortcut.Quits.type <: shortcut.Type](using lb: Label[Q]): A = option match
    case Some(a) => a
    case _ => boundary.break(shortcut.Quits: Q)

  /** Delivers the value if it exists, or exits early with an `Alt[Err]` with a message. */
  inline def ?#[L >: Alt[Err]](inline msg: String)(using Label[L]): A = option match
    case Some(a) => a
    case _ => boundary.break(Alt(Err(msg)))


extension [A](iterator: Iterator[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return to an escape/loop boundary. */
  inline def ?(using Label[escape.Token]): A =
    if iterator.hasNext then iterator.next else boundary.break(escape.Token())

  /** Delivers the value if it exists, or does a perhaps nonlocal return of a default value. */
  inline def ?+[L](inline l: L)(using Label[L]): A =
    if iterator.hasNext then iterator.next else boundary.break(l)

  /** Delivers the value if it exists, or does a perhaps nonlocal return to a Skip boundary. */
  inline def skip_?[S >: shortcut.Skips.type <: shortcut.Type](using lb: Label[S]): A =
    if iterator.hasNext then iterator.next else boundary.break(shortcut.Skips: S)

  /** Delivers the value if it exists, or does a perhaps nonlocal return to a Quit boundary. */
  inline def quit_?[Q >: shortcut.Quits.type <: shortcut.Type](using lb: Label[Q]): A =
    if iterator.hasNext then iterator.next else boundary.break(shortcut.Quits: Q)

  /** Delivers the value if it exists, or exits early with an `Alt[Err]` with a message. */
  inline def ?#[L >: Alt[Err]](inline msg: String)(using Label[L]): A =
    if iterator.hasNext then iterator.next else boundary.break(Alt(Err(msg)))


extension [A](stepper: scala.collection.Stepper[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return to an escape/loop boundary. */
  inline def ?(using Label[escape.Token]): A =
    if stepper.hasStep then stepper.nextStep else boundary.break(escape.Token())

  /** Delivers the value if it exists, or does a perhaps nonlocal return of a default value. */
  inline def ?+[L](inline l: L)(using Label[L]): A =
    if stepper.hasStep then stepper.nextStep else boundary.break(l)

  /** Delivers the value if it exists, or does a perhaps nonlocal return to a Skip boundary. */
  inline def skip_?[S >: shortcut.Skips.type <: shortcut.Type](using lb: Label[S]): A =
    if stepper.hasStep then stepper.nextStep else boundary.break(shortcut.Skips: S)

  /** Delivers the value if it exists, or does a perhaps nonlocal return to a Quit boundary. */
  inline def quit_?[Q >: shortcut.Quits.type <: shortcut.Type](using lb: Label[Q]): A =
    if stepper.hasStep then stepper.nextStep else boundary.break(shortcut.Quits: Q)

  /** Delivers the value if it exists, or exits early with an `Alt[Err]` with a message. */
  inline def ?#[L >: Alt[Err]](inline msg: String)(using Label[L]): A =
    if stepper.hasStep then stepper.nextStep else boundary.break(Alt(Err(msg)))


extension [A](iterator: java.util.Iterator[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return of `Unit`. */
  inline def ?(using Label[escape.Token]): A =
    if iterator.hasNext then iterator.next else boundary.break(escape.Token())

  /** Delivers the value if it exists, or does a perhaps nonlocal return of a default value. */
  inline def ?+[L](inline l: L)(using Label[L]): A =
    if iterator.hasNext then iterator.next else boundary.break(l)

  /** Delivers the value if it exists, or does a perhaps nonlocal return to a Skip boundary. */
  inline def skip_?[S >: shortcut.Skips.type <: shortcut.Type](using lb: Label[S]): A =
    if iterator.hasNext then iterator.next else boundary.break(shortcut.Skips: S)

  /** Delivers the value if it exists, or does a perhaps nonlocal return to a Quit boundary. */
  inline def quit_?[Q >: shortcut.Quits.type <: shortcut.Type](using lb: Label[Q]): A =
    if iterator.hasNext then iterator.next else boundary.break(shortcut.Quits: Q)

  /** Delivers the value if it exists, or exits early with an `Alt[Err]` with a message. */
  inline def ?#[L >: Alt[Err]](inline msg: String)(using Label[L]): A =
    if iterator.hasNext then iterator.next else boundary.break(Alt(Err(msg)))


extension [A](enumerator: java.util.Enumeration[A])
  /** Delivers the value if it exists, or does a perhaps nonlocal return of `Unit`. */
  inline def ?(using Label[escape.Token]): A =
    if enumerator.hasMoreElements then enumerator.nextElement else boundary.break(escape.Token())

  /** Delivers the value if it exists, or does a perhaps nonlocal return of a default value. */
  inline def ?+[L](inline l: L)(using Label[L]): A =
    if enumerator.hasMoreElements then enumerator.nextElement else boundary.break(l)

  /** Delivers the value if it exists, or does a perhaps nonlocal return to a Skip boundary. */
  inline def skip_?[S >: shortcut.Skips.type <: shortcut.Type](using lb: Label[S]): A =
    if enumerator.hasMoreElements then enumerator.nextElement else boundary.break(shortcut.Skips: S)

  /** Delivers the value if it exists, or does a perhaps nonlocal return to a Quit boundary. */
  inline def quit_?[Q >: shortcut.Quits.type <: shortcut.Type](using lb: Label[Q]): A =
    if enumerator.hasMoreElements then enumerator.nextElement else boundary.break(shortcut.Quits: Q)

  /** Delivers the value if it exists, or exits early with an `Alt[Err]` with a message. */
  inline def ?#[L >: Alt[Err]](inline msg: String)(using Label[L]): A =
    if enumerator.hasMoreElements then enumerator.nextElement else boundary.break(Alt(Err(msg)))


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

extension (b: boundary.type)
    /** Delivers a default value unless there is an early return. */
    inline def default[A](inline a: A)(inline f: boundary.Label[A] ?=> Unit): A =
      boundary[A]:
        f
        a

/** Enables early returns in side-effecting code.
  *
  * Usage:
  * {{{
  * def printFirstThree[A](ia: Iterator[A]) = escape:
  *   println(ia.?)
  *   println(ia.?)
  *   println(ia.?)
  * }}}
  */
object escape {
  opaque type Token = Unit
  object Token:
    inline def apply(): Token = ()

  /** Returns true if completed normally; false if not */
  inline def completed(inline f: Label[escape.Token] ?=> Unit): Boolean =
    var executionComplete = false
    val _ = boundary[escape.Token]:
      f
      executionComplete = true
      (): escape.Token
    executionComplete

  /** Just supports early return. */
  inline def apply(inline f: Label[escape.Token] ?=> Unit): Unit =
    val _ = boundary[escape.Token]:
      f
      (): escape.Token

  opaque type Test = Boolean
  object Test {
    extension (p: Test)
      inline def ?(using Label[escape.Token]): Unit =
        if p then boundary.break(Token())
  }
  
  /** Exit early if test condition is true */
  inline def when(test: Boolean)(using Label[escape.Token]): Test = test

  /** Exit early if test condition is false */
  inline def unless(test: Boolean)(using Label[escape.Token]): Test = !test

  /** Exit early immediately */
  inline def break()(using Label[escape.Token]): Nothing = boundary.break(Token())
}



/** Enables early return of Double NaN values
  *
  * Usage:
  * {{{
  * def nansgn(d: Double) = calculate:
  *   if d.? < 0 then -1.0 else 1.0
  * }}}
  */
inline def calculate(inline a: Label[Double] ?=> Double): Double = boundary{ a }



/** Loops until broken */
object loop {
  inline def apply(inline f: Label[escape.Token] ?=> Unit): Unit =
    boundary[escape.Token] {
      while true do
        f
      escape.Token()
    }: Unit

  inline def break()(using Label[escape.Token]): Nothing =
    boundary.break(escape.Token())

  opaque type Test = Boolean
  object Test {
    extension (p: Test)
      inline def ?(using Label[escape.Token]): Unit =
        if p then boundary.break(escape.Token())
  }

  inline def stop(p: Boolean): Test = p

  inline def proceed(p: Boolean): Test = !p
}


      

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

extension (objectRight: Right.type) {
  /** Exits to a compatible boundary--perhaps an Either.Ret--with a newly constructed Right value. */
  inline def break[R, E >: Right[Nothing, R]](inline r: => R)(using Label[E]): Nothing = boundary.break(Right(r))
}

extension (objectRight: Left.type) {
  /** Exits to a compatible boundary--perhaps an Either.Ret--with a newly constructed Left value. */
  inline def break[L, E >: Left[L, Nothing]](inline l: => L)(using Label[E]): Nothing = boundary.break(Left(l))
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
inline def nice[X](inline x: => X): Ask[X] =
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
inline def threadnice[X](inline x: => X): Ask[X] =
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

/** Run something, catching specific exceptions, and then map or match the success case.
  *
  * This matches the features of the draft exception matching JEP https://openjdk.org/jeps/8323658
  * albeit with different (clearer) syntax: all the exception handling is in the first block.
  * 
  * Usage:
  * {{{
  * catchmatch("eel".charAt(4)) {
  *   case _: StringIndexOutOfBoundsException => "out of bounds"
  * }{
  *   case 'e' => "eel has lots of e's!"
  *   case c   => s"the letter was '$c'" 
  * }
  * }}}
  */
inline def catchmatch[A](inline a: => A)[Z](inline handler: PartialFunction[Throwable, Z])(inline f: A => Z) =
  boundary[Z]: outer ?=>
    f(
      boundary[A]: inner ?=>
        boundary.break(
          try boundary.break(a)(using inner)
          catch handler
        )(using outer)
    )

/** Defer an action until the end of the block.  This form cannot be used in a procrastinator block.
  *
  * Equivalent to try/finally, except the finally block is at the top.
  *
  * Works similarly to defer in Go, but in a block-centric not function-centric manner.
  * Unlike Go, there are no complex rules about capture: the deferred code is simply executed
  * at the end of the block, using whatever any mutable values happen to be at the time.  However,
  * because the deferred code doesn't have the block in scope, it can't capture those values.
  * 
  * If one wants to persist a mutable value that will change, one must manually assign it to a val and
  * use the val.
  * 
  * {{{
  * var x = 1
  * defer(println(x)):
  *   if foo() then x += 1
  *   println("Hello")
  *   x = x*3
  *   while bar(x) do x -= 1
  * }}}
  */
inline def defer[A](inline later: => Unit)(inline a: => A)(using scala.util.NotGiven[procrastinator.Procrastination]): A =
  try a
  finally later

/** Defer an action managed by a procrastinator.  Runs at the end of a procrastinator block.
  * 
  * Multiple deferrals are run in stack order (first-on, last-off).
  * 
  * In case of exception, the code will still run, but an exception during deferral will terminate deferral handling
  * unless a `procrastinator.nice:` block is used.
  */
inline def defer(using p: procrastinator.Procrastination)(later: => Unit): Unit =
  p.asInstanceOf[Mu[List[() => Unit]]].zap((() => later) :: _)

object procrastinator {
  opaque type Procrastination = Mu[List[() => Unit]]
  opaque type Unnested = Unit

  private def catchup(items: Mu[List[() => Unit]]): Unit = items() match
    case f :: more =>
      items := more  // f() might add things to items!
      f()
      catchup(items)
    case _ =>

  /** Denote a block where one may defer code for later using a `later` block.
    * When the block ends, the items are run in the reverse order they were added (i.e. as a stack).
    */
  inline def apply[A](inline f: Procrastination ?=> A): A =
    val p: Procrastination = Mu(Nil)
    try f(using p)
    finally catchup(p.asInstanceOf[Mu[List[() => Unit]]])

  private def catchupNice(items: Mu[List[() => Unit]], errors: Mu[List[Err]]): Unit = items() match
    case f :: more =>
      items := more  // f() might add things to items!
      try kse.flow.nice(f()).foreachAlt(e => errors.zap(e :: _))
      finally catchupNice(items, errors)
    case _ =>

  /** Denotes a block where one will catch exceptions and defer code for later using a `later` block.
    * When the block ends, delayed items are run in the reverse order they were added (i.e. as a stack).
    * 
    * A best effort is made to run everything, even in case of repeated exceptions.  This may overflow
    * the stack, so do not enqueue too many items (it is not stack-safe).
    * 
    * If no exceptions are encountered, the normal return value is given.  Otherwise, all errors will be
    * accumulated into a single error value (both from the primary code block and from the later blocks).
    */
  inline def nice[A](inline f: Procrastination ?=> A): Ask[A] =
    val p: Procrastination = Mu(Nil)
    val em = Mu(Nil: List[Err])
    var a = Ask.ghosted[A]
    try a = kse.flow.nice(f(using p))
    finally
      try catchupNice(p.asInstanceOf[Mu[List[() => Unit]]], em)
      finally
        val es = em()
        if es.nonEmpty then
          a.foreachThem{ _ => a = Alt(Err(ErrType.Many(es))) }{ e => a = Alt(Err(ErrType.Many(e :: es))) }
    a
}


extension [A](ask: Ask[A])
  inline def niceMap[B](f: A => B): Ask[B] = Ask.flat:
    ask.map(a => f(a))

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
    case a: Alt[?] => Left(a.alt.asInstanceOf[Y])
    case _ => Right(Is unwrap or.asInstanceOf[Is[X]])

  /** Turns this `Or` into an `Either` swapping favored and disfavored branches (i.e `Is[X]` becomes `Left[X]`) */
  inline def swapToEither: Either[X, Y] = or match
    case a: Alt[?] => Right(a.alt.asInstanceOf[Y])
    case _ => Left(Is unwrap or.asInstanceOf[Is[X]])

  /** Turns this `Or` into an `Option` by discarding the disfavored branch if present. */
  inline def toOption: Option[X] = or match
    case _: Alt[?] => None
    case _ => Some(Is unwrap or.asInstanceOf[Is[X]])

  /** Turns this `Or` into an `Option` by discarding the favored branch if present. */
  inline def swapToOption: Option[Y] = or match
    case _: Alt[?] => Some(or.asInstanceOf[Alt[Y]].alt)
    case _ => None

  /** Turns this `Or` into a `Try` by packing the disfavored branch into a `WrongBranchException` created for that purpose. */
  inline def toTry: Try[X] = or match
    case a: Alt[?] => Failure(new WrongBranchException(a.alt.asInstanceOf[Y]))
    case _ => Success(Is unwrap or.asInstanceOf[Is[X]])

  /** Turns this `Or` into a `Try` by packing the favored branch into a `WrongBranchException` created for that purpose. */
  inline def swapToTry: Try[Y] = or match
    case a: Alt[?] => Success(a.alt.asInstanceOf[Y])
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


extension [X](ask: Ask[X])
  @targetName("grabXOrErr")
  inline def grab: X = ask.getOrElse(_.toss)


extension [L, R](either: Either[L, R])
  /** Converts to an `Or`, placing `Right` as the favored branch */
  inline def toOr: R Or L = Or from either


extension [A](`try`: Try[A]) {
  /** Converts to an `Or`, placing a success as the favored branch and a failure as a `Throwable` in the disfavored branch. */
  inline def toOr: A Or Throwable = Or from `try`

  /** Converts to an `Or`, mapping a failure to a disfavored value, or keeping a success as the favored branch. */
  inline def toOrWith[B](f: Throwable => B): A Or B = Or.from(`try`, f)

  /** Converts to an `Or`, storing failures in `Err`, or keeping a success as the favored branch. */
  inline def niceOr: A Or Err = Or.from(`try`, t => Err(t))

  /** Converts to an `Or`, mapping a failure automatically to a disfavored value, or keeping a success as the favored branch. */
  inline def copeOr[E](using cope: Cope[E]): A Or E = Or.from(`try`, cope fromThrowable _)

  /** Same as `niceOr`--repacks into an `A Or Err` (typed as `Ask[A]`) */
  inline def ask(): Ask[A] = `try` match
    case Success(a) => Is(a)
    case Failure(t) => Alt(Err(t))
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

  inline def apply_?(a: A)(using lb: Label[escape.Token]): B =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => boundary.break(escape.Token())
      case b => b.asInstanceOf[B]

  inline def apply_![Z](a: A)(using lb: Label[Attempt[Z]]): B =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => boundary.break(Attempt.failed)
      case b => b.asInstanceOf[B]

  inline def applyOrBreak(a: A)(using lb: Label[A]): B =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case y if y.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => boundary.break(a)
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
  @annotation.nowarn
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
  inline def break()(using Label[A]): Nothing = boundary.break(a)

  /** Exit to a boundary that we can map from this type */
  inline def autobreak[B]()(using am: AutoMap[A, B], l: Label[B]) = boundary.break(am(a))

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
  /** Exit to boundary matching the disfavored branch, or keep going with the favored branch's value.  Like `.?` but unwraps Alt */
  inline def getOrBreak(using Label[Y]): X = or.fold{ x => x }{ y => boundary.break(y) }

  /** Exit to boundary matching mapping of the disfavored branch, or keep going with the favored branch's value.  Like `.?+` but unwraps Alt */
  inline def getOrBreakWith[Z](inline f: Y => Z)(using Label[Z]): X = or.fold{ x => x }{ y => boundary.break(f(y)) }

  /** Exit to boundary matching automatically mapped disfavored branch, or keep going with the favored branch's value.  Like `.?*` but unwraps Alt */
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
    case _ => boundary.break()
}


opaque type Attempt[+A] = A Or Unit
object Attempt {
  opaque type Passed = Unit
  object Passed {
    inline def value: Passed = ()

    extension (passed: Passed)
      inline def &&[A](inline a: => A) = a
  }

  inline def failed: kse.flow.Attempt[Nothing] = Alt.unit

  inline def asAttempt[A](or: A Or Unit): kse.flow.Attempt[A] = or

  extension [A](att: Attempt[A]) {
    inline def underlying: A Or Unit = att
    inline def default(inline a: => A): A = att.getOrElse(_ => a)
  }

  extension [A](att: kse.flow.Attempt[A]) {
    inline def attempt(inline f: Label[kse.flow.Attempt[A]] ?=> A): kse.flow.Attempt[A] =
      att || kse.flow.attempt(f).underlying
    inline def safe(inline f: Label[kse.flow.Attempt[A]] ?=> A): kse.flow.Attempt[A] =
      att || {
        try kse.flow.attempt(f)
        catch case e if e.catchable => Attempt.failed: kse.flow.Attempt[A]
      }
    inline def threadsafe(inline f: Label[kse.flow.Attempt[A]] ?=> A): kse.flow.Attempt[A] =
      att || {
        try kse.flow.attempt(f)
        catch case e if e.threadCatchable => Attempt.failed: kse.flow.Attempt[A]
      }
    inline def orCase[B](inline b: B)(inline pf: PartialFunction[B, A]): kse.flow.Attempt[A] =
      att || kse.flow.attempt(kse.flow.case_!(b)(pf))
    inline def safeCase[B](inline b: B)(inline pf: PartialFunction[B, A]): kse.flow.Attempt[A] =
      att || {
        try kse.flow.attempt(kse.flow.case_!(b)(pf))
        catch case e if e.catchable => Attempt.failed: kse.flow.Attempt[A]
      }
    inline def threadsafeCase[B](inline b: B)(inline pf: PartialFunction[B, A]): kse.flow.Attempt[A] =
      att || {
        try kse.flow.attempt(kse.flow.case_!(b)(pf))
        catch case e if e.threadCatchable => Attempt.failed: kse.flow.Attempt[A]
      }
  }
}
object attempt {
  inline def apply[A](inline f: Label[kse.flow.Attempt[A]] ?=> A): kse.flow.Attempt[A] =
    boundary[kse.flow.Attempt[A]]:
      Attempt asAttempt Is(f)

  inline def safe[A](inline f: Label[kse.flow.Attempt[A]] ?=> A): kse.flow.Attempt[A] =
    try apply(f)
    catch case e if e.catchable => Attempt.failed: kse.flow.Attempt[A]

  inline def threadsafe[A](inline f: Label[kse.flow.Attempt[A]] ?=> A): kse.flow.Attempt[A] =
    try apply(f)
    catch case e if e.threadCatchable => Attempt.failed: kse.flow.Attempt[A]
}


extension [X, Y](or: X Or Y)
  inline def ![A](using Label[kse.flow.Attempt[A]]): X =
    or.getOrElse(_ => boundary.break(kse.flow.Attempt.failed))

extension [L, R](either: Either[L, R])
  inline def ![A](using Label[kse.flow.Attempt[A]]): R = either match
    case Right(r) => r
    case _        => boundary.break(kse.flow.Attempt.failed)

extension [O](option: Option[O])
  inline def ![A](using Label[kse.flow.Attempt[A]]): O = option match
    case Some(o) => o
    case _       => boundary.break(kse.flow.Attempt.failed)

extension [T](`try`: Try[T])
  inline def ![A](using Label[kse.flow.Attempt[A]]): T = `try` match
    case Success(t) => t
    case _          => boundary.break(kse.flow.Attempt.failed)
  inline def quit_?[Q >: shortcut.Quits.type <: shortcut.Type](using Label[Q]): T = `try` match
    case Success(t) => t
    case _          => boundary.break(kse.basics.shortcut.Quits: Q)
  inline def skip_?[S >: shortcut.Skips.type <: shortcut.Type](using Label[S]): T = `try` match
    case Success(t) => t
    case _          => boundary.break(kse.basics.shortcut.Skips: S)

extension [I](iterator: Iterator[I])
  inline def ![A](using Label[kse.flow.Attempt[A]]): I =
    if iterator.hasNext then iterator.next
    else boundary.break(kse.flow.Attempt.failed)

extension [I](stepper: scala.collection.Stepper[I])
  inline def ![A](using Label[kse.flow.Attempt[A]]): I =
    if stepper.hasStep then stepper.nextStep
    else boundary.break(kse.flow.Attempt.failed)

extension [I](iterator: java.util.Iterator[I])
  inline def ![A](using Label[kse.flow.Attempt[A]]): I =
    if iterator.hasNext then iterator.next
    else boundary.break(kse.flow.Attempt.failed)

extension [I](enumerator: java.util.Enumeration[I])
  inline def ![A](using Label[kse.flow.Attempt[A]]): I =
    if enumerator.hasMoreElements then enumerator.nextElement
    else boundary.break(kse.flow.Attempt.failed)

extension (z: Boolean)
  inline def true_![A](using Label[kse.flow.Attempt[A]]): Unit =
    if !z then boundary.break(kse.flow.Attempt.failed)
  inline def false_![A](using Label[kse.flow.Attempt[A]]): Unit =
    if z then boundary.break(kse.flow.Attempt.failed)
  inline def quit_?[T >: shortcut.Quits.type](using Label[T]): Unit =
    if !z then boundary.break(shortcut.Quits: T)
  inline def skip_?[T >: shortcut.Skips.type](using Label[T]): Unit =
    if !z then boundary.break(shortcut.Skips: T)

extension [A](a: A)
  inline def case_![B, Z](pf: PartialFunction[A, B])(using Label[kse.flow.Attempt[Z]]): B =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => boundary.break(kse.flow.Attempt.failed)
      case y => y.asInstanceOf[B]
  inline def attemptCase[B](pf: PartialFunction[A, B]): kse.flow.Attempt[B] =
    attempt:
      a.case_!(pf)
  inline def attemptCaseSafe[B](pf: PartialFunction[A, B]): kse.flow.Attempt[B] =
    attempt.safe:
      a.case_!(pf)
