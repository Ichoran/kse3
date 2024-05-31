// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr, UCSF, and Calico Life Sciences LLC.


package kse.flow

import scala.util.boundary
import scala.util.boundary.Label

import kse.basics._


opaque type Err = String | ErrType
object Err extends Translucent.Companion[Err, String | ErrType] {
  extension (e: Err)
    inline def underlying: String | ErrType = e

  extension (e: kse.flow.Err)
    def explainWith(f: kse.flow.Err => String, indent: String = "  "): kse.flow.Err = Err.apply(ErrType.Explained(f(e), e, indent))
    def explainBy(s: String, indent: String = "  "): Err = Err.apply(ErrType.Explained(s, e, indent))
    def explainValue[A](s: String, value: A): kse.flow.Err = Err.apply(ErrType.Explained(s, e, context = Some(value)))
    def toThrowable: Throwable = e.underlying match
      case s: String => ErrType.StringErrException(s)
      case t: ErrType => t.toThrowable
    def toss: Nothing = throw e.toThrowable
    def toOr: String Or ErrType = e.underlying match
      case s: String => Is(s)
      case t: ErrType => Alt(t)

  def apply(s: String): kse.flow.Err = s
  def apply(et: ErrType): kse.flow.Err = et
  def apply[E](e: E)(using ef: ErrFrom[E]): kse.flow.Err = ef(e)

  def or(s: String): kse.flow.Alt[kse.flow.Err] = Alt(s)
  def or(et: ErrType): kse.flow.Alt[kse.flow.Err] = Alt(et)
  def or[E](e: E)(using ef: ErrFrom[E]): kse.flow.Alt[kse.flow.Err] = Alt(ef(e))

  inline def break[L >: Alt[Err]](s: String)(using Label[L]): Nothing = boundary.break(Alt(apply(s)))
  inline def break[L >: Alt[Err]](et: ErrType)(using Label[L]): Nothing = boundary.break(Alt(apply(et)))
  inline def break[E, L >: Alt[Err]](e: E)(using ef: ErrFrom[E], lb: Label[L]): Nothing = boundary.break(Alt(apply(e)))

  inline def ?#[L >: Alt[Err]](s: String)(using Label[L]): Nothing = boundary.break(Alt(apply(s)))


  /** Enables Rust-style early error returns into an `Or`.  The value from normal control flow is wrapped in `Is`.
    * Any exceptions are caught and converted into an Err.  A `Label` is provided to break out user-created `Err`s.
    * Strings and ErrTypes are allowed to jump out too.
    *
    * Usage:
    * {{{
    * def parseTwice(s: String): Int Or Err = Err.Or:
    *   s.toInt.altCase{ case x if x >= 100000 => "Too big: " + x }.? * 2
    * }}}
    */
  inline def Or[X](inline x: Label[X Or (Err | String | ErrType)] ?=> X): X Or Err = boundary[X Or Err] { label ?=>
    try Is(x(using label.asInstanceOf[Label[X Or (Err | String | ErrType)]]))  // Cheat visibility of opaque type
    catch case t if t.catchable => Alt(Err(t))
  }

  /** Enables Rust-style early error returns into an `Or`.  The value from normal control flow should return
    * something `Or Err`.
    * Any exceptions are caught and converted into an Err.  A `Label` is provided to break out user-created `Err`s.
    * Strings and ErrTypes are allowed to jump out too.
    *
    * Usage:
    * {{{
    * def parseTwice(s: String): Int Or Err = Err.FlatOr:
    *   nice{ s.toInt.altCase{ case x if x >= 100000 => "Too big: " + x }.? * 2 }
    * }}}
    */
  inline def FlatOr[X](inline x: Label[X Or (Err | String | ErrType)] ?=> X Or Err): X Or Err = boundary[X Or Err]{ label ?=>
    try x(using label.asInstanceOf[Label[X Or (Err | String | ErrType)]])
    catch case t if t.catchable => Alt(Err(t))
  }

  /** Catches exceptions and packs them into an Err. */
  inline def nice[X](x: => X): X Or Err =
    try Is(x)
    catch case t if t.catchable => Alt(Err(t))

  /** Catches exceptions and packs them into an Err while evaluating something that might return an `Err` anyway. */
  inline def flatNice[X](x: => X Or Err): X Or Err =
    try x
    catch case t if t.catchable => Alt(Err(t))
}


trait ErrType {
  type E
  def error: E

  /** Converts this into a `Throwable`.  This `Throwable` should be `catchable`--wrap in `ErrType.CatchableException` if necessary. */
  def toThrowable: Throwable
}
object ErrType {
  private def indentString(string: String, indent: String = "  ", header: String = ""): String =
    val b = new java.lang.StringBuilder
    if header.nonEmpty then
      b append header
      if header.charAt(header.length - 1) != '\n' && string.nonEmpty then b append '\n'
    var i = 0
    var j = string.indexOf('\n', i)
    while j > 0 do
      b append indent
      b.append(string, i, j + 1)
      i = j + 1
      j = string.indexOf('\n', i)
    if i < string.length then
      b append indent
      b.append(string, i, string.length)
    b.toString

  final class StringErrException(msg: String) extends RuntimeException(msg, null, true, false) {
    override def toString = 
      if msg.indexOf('\n') < 0 then "Error message: " + msg
      else "Error message:\n" + msg
  }

  final class CatchableException(msg: String, cause: Throwable) extends RuntimeException(msg, cause, true, false) {
    override def toString = if msg.isEmpty then "(Catchable) " + cause.toString else super.toString
  }

  final class ThrowableErr(val error: Throwable, explainer: Throwable => String = _.explain(40, 10)) extends ErrType {
    type E = Throwable

    override def equals(a: Any) = a match
      case te: ThrowableErr => error == te.error
      case _ => false

    override def hashCode = error.hashCode

    override lazy val toString = explainer(error)

    def toThrowable = if error.catchable then error else new CatchableException("", error)
  }

  final class Explained(val explanation: String, val error: kse.flow.Err, val indent: String = "  ", val context: Option[Any] = None) extends ErrType {
    type E = kse.flow.Err

    def withoutContext: Explained =
      if context.isEmpty then this else new Explained(explanation, error, indent, None)

    def withContext[A](value: A): Explained =
      new Explained(explanation, error, indent, Some(value))

    def mapContext[A](f: Option[Any] => Option[A]): Explained =
      val c2 = f(context)
      if c2.isEmpty then withoutContext
      else new Explained(explanation, error, indent, c2)

    override def equals(a: Any) = a match
      case ex: Explained => explanation == ex.explanation && error == ex.error && context == ex.context
      case _ => false

    override def hashCode = explanation.## ^ error.## ^ context.##

    override lazy val toString = indentString(error.toString, indent = indent, header = explanation)

    def toThrowable =
      var explanations: List[Explained] = Nil
      var x: kse.flow.Err = Err(this)
      var last: String | ErrType = ""
      var continue = true
      while continue do x match
        case e: Explained =>
          explanations = e :: explanations
          x = e.error
        case _ =>
          last = x.underlying
          continue = false
      last match
        case s: String => new StringErrException(this.toString)
        case t: ErrType =>
          val u = t.toThrowable
          val msg = explanations.foldLeft(u.toString)((m, x) => indentString(m, indent = x.indent, header = x.explanation))
          new CatchableException(msg, u)
  }

   final class Many(val errs: scala.collection.Seq[Err]) extends ErrType {
    type E = scala.collection.Seq[Err]

    def error: E = errs

    override def equals(a: Any) = a match
      case m: Many => errs == m.errs
      case _ => false

    override def hashCode = errs.##

    override lazy val toString = s"Multiple errors found (${errs.length})"

    def toThrowable = new StringErrException(this.toString)
  }
}


trait ErrFrom[-E] {
  def apply(e: E): Err
}
object ErrFrom {
  given ErrFrom[String]    = e => Err(e)
  given ErrFrom[Throwable] = e => Err(ErrType.ThrowableErr(e))
}


extension [A](a: A) {
  inline def orErr: A Or Err = Is(a)

  inline def errIf(p: A => Boolean)(using ef: ErrFrom[A]): A Or Err =
    if p(a) then Alt(ef(a)) else Is(a)

  inline def errIfNot(p: A => Boolean)(using ef: ErrFrom[A]): A Or Err =
    if p(a) then Is(a) else Alt(ef(a))

  inline def errCase(pf: PartialFunction[A, Err]): A Or Err =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Is(a)
      case e => Alt(e.asInstanceOf[Err])
}

