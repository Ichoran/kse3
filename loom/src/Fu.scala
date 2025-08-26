// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024-25 Rex Kerr.

package kse.loom


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.util.concurrent.{Future, ExecutorService, ArrayBlockingQueue, SynchronousQueue}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock

import scala.util.boundary
import scala.util.boundary.Label
import scala.reflect.ClassTag

import kse.basics._
import kse.flow._


opaque type Fu[A] = Future[Ask[A]]
object Fu {
  case class GroupExecutor(service: ExecutorService) {
    val failure: AtomicReference[Err Or Unit] = new AtomicReference(Alt.unit)
    def fail(err: Err): Unit =
      if failure.compareAndSet(Alt.unit, Is(err)) then
        service.shutdownNow() __ Unit
  }

  opaque type Executor = ExecutorService | GroupExecutor
  object Executor {
    def create(): kse.loom.Fu.Executor = java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor()
    def group(): kse.loom.Fu.Executor = GroupExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
    extension (e: Executor)
      def unwrap: ExecutorService = e match
        case es: ExecutorService => es
        case GroupExecutor(es)   => es
      def stopIfGroup(): Unit = e match
        case GroupExecutor(es) => es.shutdownNow() __ Unit
        case _ =>
      def swapError(old: Err): Err = e match
        case g: GroupExecutor => g.failure.get.getOrElse(_ => old)
        case _ => old
  }

  val defaultExecutor = Executor.create()
  given Executor = defaultExecutor

  inline def flat[A](using exec: Executor)(inline work: Label[Ask[A]] ?=> Ask[A]): kse.loom.Fu[A] = exec match
    case service: ExecutorService => service.submit(() => Ask.threadsafeFlat{ work })
    case g: GroupExecutor => g.service.submit(() => Ask.threadsafeFlat{ work }.peekAlt(g.fail))

  inline def apply[A](using exec: Executor)(inline work: Label[Ask[A]] ?=> A): kse.loom.Fu[A] = 
    val run: java.util.concurrent.Callable[Ask[A]] = () => Ask.threadsafe{ work }
    exec match
      case service: ExecutorService => service.submit(run)
      case g: GroupExecutor => g.service.submit(() => run.call().peekAlt(g.fail))

  inline def flatGroup[A](using exec: Executor)(inline makeWork: Executor ?=> Label[Ask[A]] ?=> Ask[A]): kse.loom.Fu[A] = exec match
    case service: ExecutorService =>
      service.submit(() => {
        val ex: Executor = Executor.group()
        try Ask.threadsafeFlat{ makeWork(using ex) }.mapAlt(e => Executor.swapError(ex)(e))
        finally Executor.stopIfGroup(ex)()
      })
    case g: GroupExecutor =>
      g.service.submit(() => {
        val ex: Executor = Executor.group()
        try Ask.threadsafeFlat{ makeWork(using ex) }.mapAlt(e => Executor.swapError(ex)(e)).peekAlt(g.fail)
        finally Executor.stopIfGroup(ex)()
      })

  inline def group[A](using exec: Executor)(inline makeWork: Executor ?=> Label[Ask[A]] ?=> A): kse.loom.Fu[A] = exec match
    case service: ExecutorService =>
      service.submit(() => {
        val ex: Executor = Executor.group()
        try Ask.threadsafe{ makeWork(using ex) }.mapAlt(e => Executor.swapError(ex)(e))
        finally Executor.stopIfGroup(ex)()
      })
    case g: GroupExecutor =>
      g.service.submit(() => {
        val ex: Executor = Executor.group()
        try Ask.threadsafe{ makeWork(using ex) }.mapAlt(e => Executor.swapError(ex)(e)).peekAlt(g.fail)
        finally Executor.stopIfGroup(ex)()
      })

  extension [A](fu: Fu[A]) {
    def isComplete: Boolean = (fu: Future[Ask[A]]).isDone

    def ask(): Ask[A] =
      try (fu: Future[Ask[A]]).get()
      catch case e if e.catchable => Alt(Err(e))

    def askWithinMs(timeout: Long): A Or Option[Err] =
      try (fu: Future[Ask[A]]).get(timeout, java.util.concurrent.TimeUnit.MILLISECONDS).mapAlt(e => Some(e))
      catch
        case t: java.util.concurrent.TimeoutException => Alt(None)
        case e if e.catchable => Alt(Some(Err(e)))

    def cancel(): Boolean = (fu: Future[Ask[A]]).cancel(true)

    inline def ?[L >: Alt[Err]](using lb: Label[L]): A = kse.flow.?[A, Err](Fu.ask(fu)())(using lb)
    inline def ?+[E, L >: Alt[E]](inline f: Err => E)(using lb: Label[L]): A = kse.flow.?+(Fu.ask(fu)())(f)(using lb)
    inline def ?*[E, L >: Alt[E]](using lb: Label[L], m: Err AutoMap E): A = kse.flow.?*[A, Err](Fu.ask(fu)())(using lb, m)
    inline def ?#[L >: Alt[Err]](inline msg: String)(using lb: Label[L]): A = kse.flow.?#(Fu.ask(fu)())(msg)(using lb)

    inline def map[B](using exec: Executor)(inline f: Label[B Or Err] ?=> (A => B)): kse.loom.Fu[B] = kse.loom.Fu.apply[B]:
      val result = kse.flow.?(Fu.ask(fu)())
      f(result)

    inline def flatMap[B](using exec: Executor)(inline f: Label[B Or Err] ?=> (A => (B Or Err))): kse.loom.Fu[B] = kse.loom.Fu.flat[B]:
      val result = Fu.ask(fu)()
      kse.flow.flatMap(result)(x => f(x))
  }
}

extension [A](a: Array[kse.loom.Fu[A]]) {
  def allFu(using exec: Fu.Executor, tag: ClassTag[Ask[A]]): kse.loom.Fu[Array[Ask[A]]] = Fu:
    a.copyWith(fu => Fu.ask(fu)())

  def fu(using exec: Fu.Executor, tag: ClassTag[A]): kse.loom.Fu[Array[A]] = Fu.flat:
    var b: scala.collection.mutable.ArrayBuffer[Err] = null
    val v = new Array[A](a.length)
    a.visit(){ (x, i) =>
      Fu.ask(x)().fold(v(i) = _){ e =>
        if b eq null then b = scala.collection.mutable.ArrayBuffer.empty[Err]
        val _ = b += e
      }
    }
    if b ne null then
      if b.length == 1 then Alt(b.head)
      else Alt(Err(ErrType.Many(b)))
    else Is(v)

  inline def fuMap[B](using exec: Fu.Executor)(inline f: Label[B Or Err] ?=> (A => B)): Array[kse.loom.Fu[B]] =
    a.copyWith(_ map f)

  inline def fuFlatMap[B](using exec: Fu.Executor)(inline f: Label[B Or Err] ?=> (A => Ask[B])): Array[kse.loom.Fu[B]] =
    a.copyWith(_ flatMap f)
}



class Threaded[A] private (f: () => A) extends Thread {
  private val result = new java.util.concurrent.CompletableFuture[Ask[A]]()
  override def run(): Unit =
    result.complete(threadnice(f())): Unit
  def ask(): Ask[A] =
    try result.get()
    catch case e if e.catchable => Alt(Err(e))
  def isComplete: Boolean = result.isDone
}
object Threaded {
  def apply[A](a: => A): Threaded[A] =
    val th = new Threaded(() => a)
    th.start()
    th
}
