// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences, LLC.

package kse.flow


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.util.boundary

import kse.basics._


trait Tidy[-T] extends (T => Unit) {
  def apply(t: T): Unit
}
object Tidy {
  val doNothing: Tidy[Any] = (a: Any) => ()

  trait Nice[-T] extends Tidy[T] {}

  sealed trait CanClose {
    def close(): Unit
  }
  final class Managed[R](private var r: R, done: Tidy[R]) extends CanClose {
    private var closed: Boolean = false
    def close(): Unit =
      if !closed then
        done(r)
        r = null.asInstanceOf[R]
        closed = true
  }
}


object Resource {
  // TODO: handle more thoughtfully the case where there is an exception during closing the resource
  // in combination with nonlocal control flow--if we have normal control flow BUT an exception in
  // closing, probably the nonlocal control should be overridden by the local exception UNLESS it
  // too is nonlocal--and anyway, what about overriding the target of the nonlocal control in the
  // close block?  Also, because of the complexity of the issue, we might want fewer than four options.
  // Each different option has its own different choices and different complexity.

  def apply[R, A](rsc: Tidy[R] ?=> R)(done: Tidy[R])(f: R => A): A =
    val r = rsc(using done)
    try f(r)
    finally done(r)

  def safe[R, A](rsc: Tidy[R] ?=> R)(done: Tidy[R])(f: R => A): A Or Throwable = boundary:
    var wrong: Throwable = null
    val result =
      val r = try { rsc(using done) } catch { case e if e.catchable => boundary.break(Alt(e)) }
      try Is(f(r))
      catch case e if e.catchable => Alt(e)
      finally
        try done(r)
        catch case e if e.catchable => wrong = e
    if result.isIs && (wrong ne null) then Alt(wrong) else result

  def nice[R, A](rsc: Tidy.Nice[R] ?=> Ask[R])(done: Tidy.Nice[R])(f: R => A): Ask[A] = boundary:
    var wrong: Throwable = null
    val result =
      val r = try { rsc(using done).? } catch { case e if e.catchable => boundary.break(Err.or(e)) }
      try Is(f(r))
      catch case e if e.catchable => Err.or(e)
      finally
        try done(r)
        catch case e if e.catchable => wrong = e
    if result.isIs && (wrong ne null) then
      Alt(Err(wrong).explainValue("Operation succeeded but error encountered while closing resource", result.get))
    else result

  inline def Nice[R, A](rsc: Tidy.Nice[R] ?=> Ask[R])(done: Tidy.Nice[R])(inline f: boundary.Label[A Or Err] ?=> (R => A)): Ask[A] =
    boundary:
      var wrong: Throwable = null
      val result =
        val r = try { rsc(using done).? } catch { case e if e.catchable => boundary.break(Err.or(e)) }
        try Is(f(r))
        catch case e if e.catchable => Err.or(e)
        finally
          try done(r)
          catch case e if e.catchable => wrong = e
      if result.isIs && (wrong ne null) then
        Alt(Err(wrong).explainValue("Operation succeeded but error encountered while closing resource", result.get))
      else result

  def unmanaged[R](rsc: Tidy[R] ?=> R): R = rsc(using Tidy.doNothing)

  final class Manager() extends Tidy.CanClose {
    private var items: List[Tidy.CanClose] = Nil
    private def closeItems(exceptions: List[Throwable] = Nil, n: Int = 0): Unit = items match
      case item :: rest =>
        items = rest
        var es = exceptions
        try item.close()
        catch case e if e.catchable => es = e :: es
        closeItems(es, n + 1)
      case _ => exceptions match
        case Nil =>
        case e :: Nil => throw e
        case lots => Err(ErrType.Many(lots.map(Err.apply), s"${lots.length} exceptions while closing $n resources")).toss
    def +=(cc: Tidy.CanClose): Unit =
      items = cc :: items
    def close(): Unit =
      if items ne null then
        closeItems()
        items = null
  }
}

/** Within a `resourced` block, use `manage` to acquire a resource that will be closed (in reverse order) when the block exits.
  *
  * Does not work across thread boundaries.
  */
inline def resourced[A](inline f: Resource.Manager ?=> A): A =
  val m = new Resource.Manager()
  try f(using m)
  finally m.close()

def manage_closeably[A](rsc: Tidy[A] ?=> A)(done: Tidy[A])(using manager: Resource.Manager): (A, Tidy.CanClose) =
  val r = rsc(using done)
  val mg = Tidy.Managed(r, done)
  manager += mg
  (r, mg: Tidy.CanClose)

def manage[A](rsc: Tidy[A] ?=> A)(done: Tidy[A])(using manager: Resource.Manager): A =
  val r = rsc(using done)
  manager += Tidy.Managed(r, done)
  r
