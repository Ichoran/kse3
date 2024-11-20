// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023-24 Rex Kerr, UCSF, and Calico Life Sciences LLC.

package kse.flow


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.util.boundary
import scala.util.boundary.Label

import kse.basics._


opaque type Err = String | ErrType
object Err extends Translucent.Companion[Err, String | ErrType] {
  extension (e: Err)
    inline def underlying: String | ErrType = e

  extension (e: kse.flow.Err)
    def +#(s: String): kse.flow.Err = ErrType.Explained(s, e, "  ")
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
    def buildLines(sb: java.lang.StringBuilder, prefix: String): Unit = e.underlying match
      case s: String  => ErrType.buildLinesFromString(sb, s, prefix)
      case t: ErrType => t.buildLines(sb, prefix)

  def apply(s: String): kse.flow.Err = s
  def apply(et: ErrType): kse.flow.Err = et
  def apply[E](e: E)(using ef: ErrFrom[E]): kse.flow.Err = ef(e)
  def apply(es: Err*)(desc: String): kse.flow.Err =
    if es.isEmpty then desc
    else ErrType.Many(es, desc)

  def or(s: String): kse.flow.Alt[kse.flow.Err] = Alt(s)
  def or(et: ErrType): kse.flow.Alt[kse.flow.Err] = Alt(et)
  def or[E](e: E)(using ef: ErrFrom[E]): kse.flow.Alt[kse.flow.Err] = Alt(ef(e))

  inline def break[L >: Alt[Err]](s: String)(using Label[L]): Nothing = boundary.break(Alt(apply(s)))
  inline def break[L >: Alt[Err]](et: ErrType)(using Label[L]): Nothing = boundary.break(Alt(apply(et)))
  inline def break[E, L >: Alt[Err]](e: E)(using ef: ErrFrom[E], lb: Label[L]): Nothing = boundary.break(Alt(apply(e)))

  inline def ?#[L >: Alt[Err]](s: String)(using Label[L]): Nothing = boundary.break(Alt(apply(s)))
}


trait ErrType {
  type E
  def error: E

  /** Converts this into a `Throwable`.  This `Throwable` should be `catchable`--wrap in `ErrType.CatchableException` if necessary. */
  def toThrowable: Throwable

  /** Adds a description of this error, indented as requested, to a `StringBuilder` */
  def buildLines(sb: java.lang.StringBuilder, prefix: String): Unit
}
object ErrType {
  def buildLinesFromString(sb: java.lang.StringBuilder, s: String, prefix: String): Unit =
    var i = 0
    var j = s.indexOf('\n')
    while j >= 0 do
      sb append prefix
      if j > i then
        if s.charAt(j-1) == '\r' then
          if j-1 > i then sb.append(s, i, j-1)
        else sb.append(s, i, j)
      sb append '\n'
      i = j + 1
      j = if i < s.length then s.indexOf('\n', i) else -1
    if i < s.length then
      sb append prefix
      sb.append(s, i, s.length)
      if s.charAt(s.length-1) != '\n' then sb append '\n'
  
  private def indentString(string: String, indent: String = "  ", header: String = ""): String =
    val sb = new java.lang.StringBuilder
    if header.nonEmpty then buildLinesFromString(sb, header, "")
    buildLinesFromString(sb, string, indent)
    sb.toString


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

    def buildLines(sb: java.lang.StringBuilder, prefix: String): Unit =
      buildLinesFromString(sb, this.toString, prefix)

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

    def buildLines(sb: java.lang.StringBuilder, prefix: String): Unit =
      if explanation.nonEmpty then buildLinesFromString(sb, explanation, prefix)
      Err.buildLines(error)(sb, prefix + indent)

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

   final class Many(val errs: scala.collection.Seq[Err], desc: String = "") extends ErrType {
    type E = scala.collection.Seq[Err]

    def error: E = errs

    override def equals(a: Any) = a match
      case m: Many => errs == m.errs
      case _ => false

    override def hashCode = errs.##

    override lazy val toString = 
      val sb = new java.lang.StringBuilder()
      buildLines(sb, "")
      sb.toString

    def buildLines(sb: java.lang.StringBuilder, prefix: String): Unit =
      buildLinesFromString(sb, if desc.isEmpty then s"Multiple errors found (${errs.length})" else desc, prefix)
      val fmt = s"%0${errs.length.toString.length}d: "
      var i = 0
      val it = errs.iterator
      while it.hasNext do
        i += 1
        val e = it.next
        Err.buildLines(e)(sb, prefix + fmt.format(i))

    def toThrowable =
      val see = new StringErrException(this.toString)
      errs.foreach(e => see.addSuppressed(e.toThrowable))
      see
  }
}


trait ErrFrom[-E] {
  def apply(e: E): Err
}
object ErrFrom {
  given ErrFrom[String]    = e => Err(e)
  given ErrFrom[Throwable] = e => Err(ErrType.ThrowableErr(e))
}


type Ask[A] = A Or Err
object Ask {
  /** Enables Rust-style early error returns into an `Or`.  The value from normal control flow is wrapped in `Is`.
    * Any exceptions are caught and converted into an Err.  A `Label` is provided to break out user-created `Err`s.
    * Strings and ErrTypes are allowed to jump out too.
    *
    * Usage:
    * {{{
    * def parseTwice(s: String): Ask[Int] = Ask:
    *   s.toInt.altCase{ case x if x >= 100000 => "Too big: " + x }.? * 2
    * }}}
    */
  inline def apply[X](inline x: Label[X Or (Err | String | ErrType)] ?=> X): Ask[X] = boundary[X Or Err] { label ?=>
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
    * def parseTwice(s: String): Ask[Int] = Ask.flat:
    *   nice{ s.toInt.altCase{ case x if x >= 100000 => "Too big: " + x }.? * 2 }
    * }}}
    */
  inline def flat[X](inline x: Label[X Or (Err | String | ErrType)] ?=> Ask[X]): Ask[X] = boundary[X Or Err] { label ?=>
    try x(using label.asInstanceOf[Label[X Or (Err | String | ErrType)]])  // Cheat visibility of opaque type
    catch case t if t.catchable => Alt(Err(t))
  }
}



/** Provides the ability to cope with Throwable by converting to an error type `E`
  * that can, for instance, be stored and passed as a disfavored branch in an `Or`.
  */
trait Cope[E] {
  def fromThrowable(t: Throwable): E
}
object Cope {
  /** The default coping strategy: store throwables */
  val asErr = new Cope[kse.flow.Err] {
    def fromThrowable(t: Throwable) = Err(t)
  }

  val asString = new Cope[String] { 
    def fromThrowable(t: Throwable) = t.explainSuppressed(60, 12)
  }

  val fullTrace = new Cope[Array[String]] {
    def fromThrowable(t: Throwable) = t.explainSuppressedAsArray()
  }

  val asThrowable = new Cope[Throwable] {
    def fromThrowable(t: Throwable) = t
  }
}

/** An exception specifically to reify the idea of the disfavored branch of a sum type
  * (`Or`, `Either`, etc.) being packed into a `Try`: the disfavored branch of `Try`
  * _must_ be a `Throwable`.
  * 
  * The exception does compute a stack trace because if you didn't need a stack trace
  * you probably wouldn't be using `Try` to begin with.
  */
final case class WrongBranchException[+W](value: W) extends Exception {
  override def getMessage: String = value.toString
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

