// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr, UCSF, and Calico Life Sciences LLC.


package kse.flow

import scala.util.boundary
import scala.util.boundary.Label


opaque type Err = String | ErrType
object Err {
  extension (e: Err)
    inline def underlying: String | ErrType = e

  extension (e: kse.flow.Err)
    def explainWith(f: kse.flow.Err => String, indent: String = "  "): kse.flow.Err = Err.apply(ErrType.Explained(f(e), e, indent))
    def explainBy(s: String, indent: String = "  "): Err = Err.apply(ErrType.Explained(s, e, indent))

  def apply(s: String): kse.flow.Err = s
  def apply(et: ErrType): kse.flow.Err = et
  def apply[E](e: E)(using ef: ErrFrom[E]): kse.flow.Err = ef(e)

  def or(s: String): kse.flow.Alt[kse.flow.Err] = Alt(s)
  def or(et: ErrType): kse.flow.Alt[kse.flow.Err] = Alt(et)
  def or[E](e: E)(using ef: ErrFrom[E]): kse.flow.Alt[kse.flow.Err] = Alt(ef(e))

  inline def break[L >: Alt[Err]](s: String)(using Label[L]): Nothing = boundary.break(Alt(apply(s)))
  inline def break[L >: Alt[Err]](et: ErrType)(using Label[L]): Nothing = boundary.break(Alt(apply(et)))
  inline def break[E, L >: Alt[Err]](e: E)(using ef: ErrFrom[E], lb: Label[L]): Nothing = boundary.break(Alt(apply(e)))


  /** Enables Rust-style early error returns into an `Or`.  The value from normal control flow is wrapped in `Is`.
    * Any exceptions are caught and converted into an Err.  A `Label` is provided to break out user-created `Err`s.
    * Strings and ErrTypes are allowed to jump out too.
    *
    * Usage:
    * {{{
    * def parseTwice(s: String): Int Or Err = Err.Or {
    *   s.toInt.altCase{ case x if x >= 100000 => "Too big: " + x }.? * 2
    * }
    * }}}
    */
  inline def Or[X](inline x: Label[X Or (Err | String | ErrType)] ?=> X): X Or Err = boundary[X Or Err] { label ?=>
    try Is(x(using label.asInstanceOf[Label[X Or (Err | String | ErrType)]]))  // Cheat visibility of opaque type
    catch case t if t.catchable => Alt(Err(t))
  }
}


trait ErrType {
  type E
  def error: E
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

  final class ThrowableErr(val error: Throwable, explainer: Throwable => String = _.explain(60, 10)) extends ErrType {
    type E = Throwable

    override def equals(a: Any) = a match
      case te: ThrowableErr => error == te.error
      case _ => false

    override def hashCode = error.hashCode

    override lazy val toString = explainer(error)
  }

  final class Explained(val explanation: String, val error: kse.flow.Err, indent: String = "  ") extends ErrType {
    type E = kse.flow.Err

    override def equals(a: Any) = a match
      case ex: Explained => explanation == ex.explanation && error == ex.error
      case _ => false

    override def hashCode = explanation.## ^ error.##

    override lazy val toString = indentString(error.toString, indent = indent, header = explanation)
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
  inline def errIf(p: A => Boolean)(using ef: ErrFrom[A]): A Or Err =
    if p(a) then Is(a) else Alt(ef(a))

  inline def errCase(pf: PartialFunction[A, Err]): A Or Err =
    pf.applyOrElse(a, Or.defaultApplyOrElse.asInstanceOf[Any => Any]) match
      case x if x.asInstanceOf[AnyRef] eq Or.defaultApplyOrElse.asInstanceOf[AnyRef] => Is(a)
      case e => Alt(e.asInstanceOf[Err])
}
