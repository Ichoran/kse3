// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences, LLC.


package kse.flow


object Resource {
  def apply[R, A](rsc: R)(done: R => Unit)(f: R => A): A =
    try f(rsc)
    finally done(rsc)

  def safe[R, A](rsc: R)(done: R => Unit)(f: R => A): A Or Throwable =
    var wrong: Throwable = null
    val result =
      try Is(f(rsc))
      catch case e if e.catchable => Alt(e)
      finally
        try done(rsc)
        catch case e if e.catchable => wrong = e
    if result.isIs && (wrong ne null) then Alt(wrong) else result

  def nice[R, A](rsc: R)(done: R => Unit)(f: R => A): A Or Err =
    var wrong: Throwable = null
    val result =
      try Is(f(rsc))
      catch case e if e.catchable => Err.or(e)
      finally
        try done(rsc)
        catch case e if e.catchable => wrong = e
    if result.isIs && (wrong ne null) then
      Alt(Err(wrong).explainBy("Operation succeeded but error encountered while closing resource"))
    else result
}
