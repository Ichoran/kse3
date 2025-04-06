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
}
