// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-2021 Rex Kerr, UCSF, and Calico Life Sciences LLC.

package kse.flow

import scala.util.control.{NonFatal, ControlThrowable}

/** An `Ok` holds either of two possibilities, either the favored one, `Yes`,
  * or an alternative, `No`.  The use cases are much like those of
  * `scala.util.Either` with a few important differences: since the favored
  * (second) branch is the default, you can use for comprehensions; a richer
  * set of methods are provided; and each of the two branches does not remember
  * what the other alternative might have been, simplifying the code. 
  */
sealed trait Ok[+N, +Y] extends Product with Serializable {
  
  /** True if this holds a `Y` which can be retrieved with `yes` */
  def isYes: Boolean

  /** True if this holds a `N` which can be retrieved with `no` */
  def isNo: Boolean
  
  /** Retrieves a stored `Y` value if available
    * @throws NoSuchElementException if this is an instance of `No`
    */
  def yes: Y
  
  /** Retrieves a stored `N` value if available
    * @throws NoSuchElementException if this is an instance of `Yes`
    */
  def no: N

  /** Retrieves whatever value is there */
  def value[A >: Y | N]: A
  
  
  /** Produces a result by applying `f` to the disfavored value or `g`
    * to the favored one, depending on which is available.
    */
  def fold[A](f: N => A, g: Y => A): A

  /** Retrieves a stored `Y` value or produces one from a `N` using `f`. */
  def yesOr[Z >: Y](f: N => Z): Z
  
  /** Retrieves a stored `N` value or produces one from a `Y` using `f`. */
  def noOr[M >: N](f: Y => M): M

  
  /** Maps a favored value from `Y` to `Z` using `f`; does nothing to a disfavored value. */
  def map[Z](f: Y => Z): Ok[N, Z]
  
  /** Maps a disfavored value from `Y` to `Z` using `f`; does nothing to a favored value. */
  def mapNo[M](f: N => M): Ok[M, Y]

  /** Maps both values */
  def mapThem[M, Z](f: N => M, g: Y => Z): Ok[M, Z]
  
  /** Turn a favored value into a `Z` or into a disfavored `M` via `f`; leaves a disfavored value alone. */
  def flatMap[M >: N, Z](f: Y => Ok[M, Z]): Ok[M, Z]

  /** Turn a disfavored value into a `M` or into a favored `Z` via `f`; leaves a favored value alone. */
  def flatMapNo[M, Z >: Y](f: N => Ok[M, Z]): Ok[M, Z]

  /** Flat maps both values as if running both .flatMapNo and .flatMap */
  def flatMapThem[M, Z](f: N => Ok[M, Z], g: Y => Ok[M, Z]): Ok[M, Z]

  /** Flattens Ok[N, Ok[M, Z]] into a single depth */
  def flatten[M, Z](using ev: Y <:< Ok[M, Z], nontrivial: scala.util.NotGiven[Y <:< Nothing]): Ok[M | N, Z] = this match
    case Yes(y) => ev(y)
    case n: No[N] => n

  /** Flattens Ok[Ok[M, Z], Y] into a single depth */
  def flattenNo[M, Z](using ev: N <:< Ok[M, Z], nontrivial: scala.util.NotGiven[N <:< Nothing]): Ok[M, Y | Z] = this match
    case No(n) => ev(n)
    case y: Yes[Y] => y

  /** Flattens Ok[Ok[M, Z], [L, X]] into a single depth */
  def flattenThem[M, Z, L, X](using nev: N <:< Ok[M, Z], yev: Y <:< Ok[L, X]): Ok[L | M, X | Z] = this match
    case No(n) => nev(n)
    case Yes(y) => yev(y)


  /** Selects only favored values selected by `p`; a given `Defaulter`
    * must be available to produce a disfavored value from a favored value
    * not selected by `p`.
    */
  def filter[M >: N](p: Y => Boolean)(using noify: Ok.Defaulter[M,Y]): Ok[M, Y]
  
  /** Selects only favored values selected by `p`; a given `Defaulter` 
    * must be available to produce a disfavored value from a favored value
    * not selected by `p`.  This simply defers to `filter`.
    */
  def withFilter[M >: N](p: Y => Boolean)(using noify: Ok.Defaulter[M,Y]): Ok[M, Y] = filter[M](p)

  
  /** Apply an operation only to a favored value. */
  def foreach[A](f: Y => A): Unit
  
  /** Apply an operation only to a disfavored value. */
  def foreachNo[A](f: N => A): Unit

  /** Apply an operation to whichever value */
  def foreachThem[A](f: N => A, g: Y => A): Unit
  
  
  /** Apply an operation only to a favored value and return the same `Ok`. */
  def tapYes[A](f: Y => A): this.type
  
  /** Apply an operation only to a disfavored value and return the same `Ok`. */
  def tapNo[A](f: N => A): this.type

  /** Apply an operation to either value depending on which is available. */
  def tapThem[A](f: N => A, g: Y => A): this.type
  
  
  /** True only if a value is favored and passes test `p`. */
  def exists(p: Y => Boolean): Boolean

  /** True only if a value is disfavored and passes test `p`. */
  def existsNo(p: N => Boolean): Boolean
  
  /** True if a value is favored and passes `p`, or if the value is disfavored. */
  def forall(p: Y => Boolean): Boolean

  /** True if a value is disfavored and passes `p` or if the value is favored. */
  def forallNo(p: N => Boolean): Boolean

  /** True if the appropriate test passes */
  def testThem(p: N => Boolean, q: Y => Boolean): Boolean
  
  
  /** Converts any favored value covered by `pf` into a disfavored value. */
  def refine[M >: N](pf: PartialFunction[Y, M]): Ok[M, Y]
  
  /** Converts any disfavored value covered by `pf` into a favored value. */
  def refineNo[Z >: Y](pf: PartialFunction[N, Z]): Ok[N, Z]

  /** Converts in both directions using partial functions. */
  def refineThem[M >: N, Z >: Y](pf: PartialFunction[N, Z], qf: PartialFunction[Y, M]): Ok[M, Z]
  
  
  /** Converts to a `scala.util.Either`. */
  def toEither: Either[N, Y] = this match
    case Yes(y) => Right(y)
    case No(n) => Left(n)

  /** Converts to an `Option` by dropping any disfavored value. */
  def toOption: Option[Y] = this match {
    case Yes(y) => Some(y)
    case _ => None
  }
  
  /** Converts to a `scala.util.Try` by wrapping a disfavored value in a [[NotOkException]]
    * or just by unwrapping a `Throwable`, as appropriate.
    */
  def toTry: scala.util.Try[Y] = this match {
    case Yes(y) => scala.util.Success(y)
    case No(t: Throwable) => scala.util.Failure(t)
    case No(n) => scala.util.Failure(new NotOkException(no))
  }
  
  /** Switches which alternative is favored and which is not. */
  def swap: Ok[Y, N] = this match {
    case Yes(y) => No(y)
    case No(n) => Yes(n)
  }

  /** Widens the type of Yes */
  inline def typeYes[Z >: Y]: Ok[N, Z] = this

  /** Widens the type of No */
  inline def typeNo[M >: N]: Ok[M, Y] = this

  /** Widens both types */
  inline def typeThem[M >: N, Z >: Y]: Ok[M, Z] = this
}


/** The favored alternative from among the two possible in an [[Ok]]. */
final case class Yes[+Y](yes: Y) extends Ok[Nothing, Y] {
  def isYes = true
  def isNo = false
  def no = throw new NoSuchElementException("Attempt to retrieve No case when Yes")
  def value[A >: Y] = yes

  def fold[A](f: Nothing => A, g: Y => A) = g(yes)
  def yesOr[Z >: Y](f: Nothing => Z) = yes
  def noOr[M](f: Y => M) = f(yes)
  
  def map[Z](f: Y => Z): Ok[Nothing, Z] = Yes(f(yes))
  def mapNo[M](f: Nothing => M): Ok[M, Y] = this
  def mapThem[M, Z](f: Nothing => M, g: Y => Z): Ok[M, Z] = Yes(g(yes))
  
  def flatMap[M, Z](f: Y => Ok[M, Z]): Ok[M, Z] = f(yes)
  def flatMapNo[M, Z >: Y](f: Nothing => Ok[M, Z]): Ok[M, Z] = this
  def flatMapThem[M, Z](f: Nothing => Ok[M, Z], g: Y => Ok[M, Z]): Ok[M, Z] = g(yes)
  
  def filter[M](p: Y => Boolean)(using noify: Ok.Defaulter[M,Y]): Ok[M, Y] = if (p(yes)) this else noify(yes)
  
  def foreach[A](f: Y => A): Unit = { f(yes); () }
  def foreachNo[A](f: Nothing => A): Unit = {}
  def foreachThem[A](f: Nothing => A, g: Y => A): Unit = { g(yes); () }
  
  def tapYes[A](f: Y => A): this.type = { f(yes); this }
  def tapNo[A](f: Nothing => A): this.type = this
  def tapThem[A](f: Nothing => A, g: Y => A): this.type = { g(yes); this }
  
  def exists(f: Y => Boolean) = f(yes)
  def existsNo(f: Nothing => Boolean) = false
  def forall(f: Y => Boolean) = f(yes)
  def forallNo(f: Nothing => Boolean) = true
  def testThem(f: Nothing => Boolean, g: Y => Boolean) = g(yes)
  
  def refine[M](pf: PartialFunction[Y, M]): Ok[M, Y] =
    val m = pf.applyOrElse(yes, Ok.pfApplyFailure)
    if (m.asInstanceOf[AnyRef] eq Ok.pfApplyFailure) this else No(m.asInstanceOf[M])
  def refineNo[Z >: Y](pf: PartialFunction[Nothing, Z]): Ok[Nothing, Z] = this
  def refineThem[M, Z >: Y](pf: PartialFunction[Nothing, Z], qf: PartialFunction[Y, M]) =
    val m = qf.applyOrElse(yes, Ok.pfApplyFailure)
    if (m.asInstanceOf[AnyRef] eq Ok.pfApplyFailure) this else No(m.asInstanceOf[M])
}


/** The disfavored alternative from among the two possible in an [[Ok]]. */
final case class No[+N](no: N) extends Ok[N, Nothing] {
  def isYes = false
  def isNo = true
  def yes = throw new NoSuchElementException("Attempt to retrieve Yes case when No")
  def value[A >: N] = no
  
  def fold[A](f: N => A, g: Nothing => A) = f(no)
  def yesOr[Z](f: N => Z) = f(no)
  def noOr[M >: N](f: Nothing => M) = no
  
  def map[Z](f: Nothing => Z): Ok[N, Z] = this
  def mapNo[M](f: N => M): Ok[M, Nothing] = No(f(no))
  def mapThem[M, Z](f: N => M, g: Nothing => Z): Ok[M, Z] = No(f(no))
  
  def flatMap[M >: N, Z](f: Nothing => Ok[M, Z]): Ok[M, Z] = this
  def flatMapNo[M, Z](f: N => Ok[M, Z]): Ok[M, Z] = f(no)
  def flatMapThem[M, Z](f: N => Ok[M, Z], g: Nothing => Ok[M, Z]): Ok[M, Z] = f(no)

  def filter[M >: N](p: Nothing => Boolean)(using noify: Ok.Defaulter[M,Nothing]) = this

  def foreach[A](f: Nothing => A): Unit = {}
  def foreachNo[A](f: N => A): Unit = { f(no); () }
  def foreachThem[A](f: N => A, g: Nothing => A): Unit = { f(no); () }
  
  def tapYes[A](f: Nothing => A): this.type = this
  def tapNo[A](f: N => A): this.type = { f(no); this }
  def tapThem[A](f: N => A, g: Nothing => A): this.type = { f(no); this }
  
  def exists(f: Nothing => Boolean) = false
  def existsNo(f: N => Boolean) = f(no)
  def forall(f: Nothing => Boolean) = true
  def forallNo(f: N => Boolean) = f(no)
  def testThem(f: N => Boolean, g: Nothing => Boolean) = f(no)
  
  def refine[M >: N](pf: PartialFunction[Nothing, M]): Ok[M, Nothing] = this
  def refineNo[Z](pf: PartialFunction[N, Z]): Ok[N, Z] =
    val z = pf.applyOrElse(no, Ok.pfApplyFailure)
    if (z.asInstanceOf[AnyRef] eq Ok.pfApplyFailure) this else Yes(z.asInstanceOf[Z])
  def refineThem[M >: N, Z](pf: PartialFunction[N, Z], qf: PartialFunction[Nothing, M]) =
    val z = pf.applyOrElse(no, Ok.pfApplyFailure)
    if (z.asInstanceOf[AnyRef] eq Ok.pfApplyFailure) this else Yes(z.asInstanceOf[Z])
}

/** Used to convert a disfavored alternative into an `Exception` so that [[Ok]] can be mapped into `scala.util.Try`. */
class NotOkException[N](val no: N) extends Exception {
  override def getMessage = no.toString
  override def toString = "kse.flow.NotOkException("+no.toString+")"
}


object Ok {
  private[flow] val pfApplyFailure = new Function1[Any, AnyRef]{ def apply(a: Any) = this }

  /** The canonical disfavored unit value. */
  val UnitNo = No(())
  
  /** The canonical favored unit value */
  val UnitYes = Yes(())

  /** Implicit handler for rejecting favored values. */
  trait Defaulter[N, -Y] { def apply(yes: Y): No[N] }
  
  private val DefaultUnitToUnit = new Defaulter[Unit, Any] { def apply(yes: Any) = UnitNo }

  /** Enables returning a `Unit` on the [[No]] branch when filtering. */
  given [Y]: Defaulter[Unit, Y] = DefaultUnitToUnit


  def wrap[N, Y](yes: Y): Ok[N, Y] = Yes(yes)

  def wrapNo[N, Y](no: N): Ok[N, Y] = No(no)
  
  
  /** Converts an `Option` to a disfavored value.  `None` maps to a content-free (`Unit`) favored value. */
  def ifNot[N](o: Option[N]): Ok[N, Unit] = o match {
    case Some(n) => No(n)
    case None => UnitYes
  }
  
  /** Converts an `Option` to a favored value.  `None` maps to a content-free (`Unit`) disfavored value. */
  def from[Y](o: Option[Y]): Ok[Unit,Y] = o match {
    case Some(y) => Yes(y)
    case None => UnitNo
  }
  
  /** Converts an `Either` to an [[Ok]], favoring the `Right` alternative. */
  def from[N,Y](e: Either[N,Y]): Ok[N,Y] = e match {
    case Left(n) => No(n)
    case Right(y) => Yes(y)
  }
  
  /** Converts a `Try` to an [[Ok]]; the disfavored alternative is a `Throwable`. */
  def from[Y](t: scala.util.Try[Y]): Ok[Throwable, Y] = t match {
    case scala.util.Success(y) => Yes(y)
    case scala.util.Failure(t) => No(t)
  }
  
  /** Given a bunch of [[Ok]]s, return either all the `No` values if there are
    * any (disfavored result); otherwise return all the `Yes` values (favored result).
    */
  def perfect[N, Y](oks: Ok[N,Y]*): Ok[Seq[N], Seq[Y]] = {
    val nos = oks.collect{ case No(n) => n }
    if (nos.size > 0) No(nos) else Yes(oks.map(_.yes))
  }

  given [N, Y, M, Z](using CanEqual[N, M], CanEqual[Y, Z]): CanEqual[Ok[N, Y], Ok[M, Z]] = CanEqual.derived
}
