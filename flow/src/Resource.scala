// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences, LLC.


package kse.flow

import scala.util.boundary;


trait Tidy[-T] extends (T => Unit) {
  def apply(t: T): Unit
}
object Tidy {
  val doNothing: Tidy[Any] = (a: Any) => ()
}


object Resource {
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

  def nice[R, A](rsc: Tidy[R] ?=> R)(done: Tidy[R])(f: R => A): A Or Err = boundary:
    var wrong: Throwable = null
    val result =
      val r = try { rsc(using done) } catch { case e if e.catchable => boundary.break(Err.or(e)) }
      try Is(f(r))
      catch case e if e.catchable => Err.or(e)
      finally
        try done(r)
        catch case e if e.catchable => wrong = e
    if result.isIs && (wrong ne null) then
      Alt(Err(wrong).explainBy("Operation succeeded but error encountered while closing resource"))
    else result

  def unmanaged[R](rsc: Tidy[R] ?=> R): R = rsc(using Tidy.doNothing)
}
