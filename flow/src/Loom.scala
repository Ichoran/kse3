// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024 Rex Kerr.


package kse.flow

import java.util.concurrent.{Future => JFuture, Executors => JExecutors, ExecutorService => JExecutorService}

import scala.util.boundary
import scala.util.boundary.Label
import scala.reflect.ClassTag

import kse.basics._

opaque type Fu[A] = JFuture[A Or Err]
object Fu {
  opaque type Executor = JExecutorService
  object Executor {
    def create(): kse.flow.Fu.Executor = JExecutors.newVirtualThreadPerTaskExecutor()
    extension (e: Executor)
      def unwrap: JExecutorService = e
  }

  val defaultExecutor = Executor.create()
  given Executor = defaultExecutor

  inline def flat[A](using exec: Executor)(inline work: Label[A Or Err] ?=> A Or Err): kse.flow.Fu[A] = exec.submit(() => boundary{ work })

  inline def apply[A](using exec: Executor)(inline work: Label[A Or Err] ?=> A): kse.flow.Fu[A] = exec.submit(() => Err.Or{ work })

  inline def flatGroup(using exec: Executor)(inline makeWork: Executor ?=> Label[A Or Err] ?=> A): kse.flow.Fu[A] = exec.submit(() => {
    given ex: Executor = Executor.create()
    try boundary{ makeWork }
    finally ex.shutdownNow()
  })

  inline def group[A](using exec: Executor)(inline makeWork: Executor ?=> Label[A Or Err] ?=> A): kse.flow.Fu[A] = exec.submit(() => {
    given ex: Executor = Executor.create()
    try Err.Or{ makeWork }
    finally ex.shutdownNow()
  })

  extension [A](fu: Fu[A]) {
    def isComplete: Boolean = (fu: JFuture[A Or Err]).isDone

    def ask(): A Or Err =
      try (fu: JFuture[A Or Err]).get()
      catch case e if e.catchable => Alt(Err(e))

    inline def ?[L >: Alt[Err]](using lb: Label[L]): A = kse.flow.?[A, Err](Fu.ask(fu)())(using lb)
    inline def ?+[E, L >: Alt[E]](inline f: Err => E)(using lb: Label[L]): A = kse.flow.?+(Fu.ask(fu)())(f)(using lb)
    inline def ?*[E, L >: Alt[E]](using lb: Label[L], m: Err AutoMap E): A = kse.flow.?*[A, Err](Fu.ask(fu)())(using lb, m)
    inline def ?#[L >: Alt[Err]](inline msg: String)(using lb: Label[L]): A = kse.flow.?#(Fu.ask(fu)())(msg)(using lb)

    inline def map[B](using exec: Executor)(inline f: Label[B Or Err] ?=> (A => B)): kse.flow.Fu[B] = exec.submit(() => Err.Or{
      val result = kse.flow.?(Fu.ask(fu)())
      f(result)
    })

    inline def flatMap[B](using exec: Executor)(inline f: Label[B Or Err] ?=> (A => (B Or Err))): kse.flow.Fu[B] = exec.submit(() => boundary[B Or Err]{
      val result = Fu.ask(fu)()
      kse.flow.flatMap(result)(x => f(x))
    })
  }
}

extension [A](a: Array[kse.flow.Fu[A]]) {
  def fu(using exec: Fu.Executor, tag: ClassTag[A Or Err]): kse.flow.Fu[Array[A Or Err]] = Fu:
    a.copyWith(fu => Fu.ask(fu)())

  def validFu(using exec: Fu.Executor, tag: ClassTag[A]): kse.flow.Fu[Array[A]] = Fu.flat:
    var b: scala.collection.mutable.ArrayBuffer[Err] = null
    val v = new Array[A](a.length)
    a.visit(){ (x, i) =>
      Fu.ask(x)().fold(v(i) = _){ e =>
        if b eq null then b = scala.collection.mutable.ArrayBuffer.empty[Err]
        b += e
      }
    }
    if b ne null then
      if b.length == 1 then Alt(b.head)
      else Alt(Err(ErrType.Many(b)))
    else Is(v)

  inline def fuMap[B](using exec: Fu.Executor)(inline f: Label[B Or Err] ?=> (A => B)): Array[kse.flow.Fu[B]] =
    a.copyWith(_ map f)

  inline def fuFlatMap[B](using exec: Fu.Executor)(inline f: Label[B Or Err] ?=> (A => (B Or Err))): Array[kse.flow.Fu[B]] =
    a.copyWith(_ flatMap f)
}
