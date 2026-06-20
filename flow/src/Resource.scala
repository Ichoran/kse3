// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023-25 Rex Kerr and Calico Life Sciences, LLC.

package kse.flow


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.util.boundary

import java.lang.ref.Cleaner
import java.util.concurrent.ConcurrentHashMap

import kse.basics._


/** Can tidy up a resource of arbitrary type.  In sequential code, will be called once in a managed block (via `finally`) */
trait Tidy[-T] extends (T => Unit) {
  def apply(t: T): Unit
}
object Tidy {
  val doNothing: Tidy[Any] = (a: Any) => ()

  /** Can tidy up a resource of arbitrary type.  Marks that Ask-semantics should be employed. */
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

  /** Will tidy up a resource of arbitrary type, with a shutdown hook to make sure.  Uses Ask-semantics. */
  trait Clean[-T] extends Nice[T] {}


  /** Sole owner of a resource whose lifetime can't be scoped lexically.  The `clean` cleanup runs exactly
    * once — at [[Later.close]], at JVM shutdown, or when this `Later` is garbage-collected, whichever comes
    * first (a `SIGKILL`/`halt` defeats all three). No attempt is made to serialize access; when used
    * concurrently, use locks or other methods if the resource does not support concurrent access. */
  final class Later[R] private[Tidy] (reap: Later.Reapable[R]) {
    private val cleanable = Later.reaper.register(this, reap)

    /** Release the resource now (idempotent); otherwise it is released at shutdown or GC. */
    def close(): Unit = cleanable.clean()

    /** Run `f` on the resource; throws if already closed. */
    def use(f: R => Unit): Unit = if reap.open then f(reap.held) else throw new IllegalStateException("Already closed")

    /** Compute from the resource; throws if already closed. */
    def op[A](f: R => A): A = if reap.open then f(reap.held) else throw new IllegalStateException("Already closed")

    /** Compute from the resource with `.?` early return; a closed `Later` yields `Alt(Err)`. */
    def nice[A](f: boundary.Label[A Or Err] ?=> (R => A)): Ask[A] = boundary:
      if reap.open then
        try Is(f(reap.held))
        catch case e if e.catchable => Err.or(e)
      else Alt(Err("Already closed"))

    /** Like [[nice]], but `f` itself yields an `Ask`. */
    def flatNice[A](f: boundary.Label[A Or Err] ?=> (R => Ask[A])): Ask[A] = boundary:
      if reap.open then
        try f(reap.held)
        catch case e if e.catchable => Err.or(e)
      else Alt(Err("Already closed"))

    /** [[use]] then [[close]].  If both throw, the close failure is added as suppressed to the op's. */
    def useAndClose(f: R => Unit): Unit =
      var primary: Throwable = null
      try use(f)
      catch
        case t if t.catchable =>
          primary = t
          throw t
      finally
        if primary eq null then close()
        else try close() catch case e if e.catchable => primary.addSuppressed(e)

    /** [[op]] then [[close]].  If both throw, the close failure is added as suppressed to the op's. */
    def opAndClose[A](f: R => A): A =
      var primary: Throwable = null
      try op(f)
      catch
        case t if t.catchable =>
          primary = t
          throw t
      finally
        if primary eq null then close()
        else try close() catch case e if e.catchable => primary.addSuppressed(e)

    /** [[nice]] then [[close]].  A close failure is never dropped: folded into a successful result as an
      * explanation of its value, or combined with the op's own error when both fail. */
    def niceAndClose[A](f: boundary.Label[A Or Err] ?=> (R => A)): Ask[A] = andClose(nice(f))

    /** [[flatNice]] then [[close]], with the same both-errors-preserved folding as [[niceAndClose]]. */
    def flatNiceAndClose[A](f: boundary.Label[A Or Err] ?=> (R => Ask[A])): Ask[A] = andClose(flatNice(f))

    /** A point-in-time snapshot of whether the resource is still open — for sequential introspection, not
      * a guard against a concurrent [[close]] (see the class note). */
    def isOpen: Boolean = reap.open

    inline private def andClose[A](inline ans: => Ask[A]): Ask[A] =
      var wrong: Throwable = null
      val a =
        try ans  // NOTE: catch not necessary because we only wrap thunks that already catch
        finally
          try close()
          catch case e if e.catchable => wrong = e
      if wrong eq null then a
      else a.fold{ 
          x => Alt(Err(wrong).explainValue("Operation succeeded but error encountered while closing resource", x))
        }{
          e => Alt(Err(e, Err(wrong))("Failure in operation on resource and in closing"))
        }
  }
  object Later {
    private val reaper  = Cleaner.create()
    private val pending = new ConcurrentHashMap[Reapable[?], java.lang.Long]()
    private val reapSeq = Atom(0L)
    private val hooked  = Atom(false)

    private def installHook(): Unit =
      if hooked.cas(false, true) then
        try Runtime.getRuntime.addShutdownHook(new Thread(() => reapAll(), "kse-reaper"))
        catch case e if e.catchable => ()      // already shutting down; nothing left to back up

    private def reapAll(): Unit =
      val es = new java.util.ArrayList(pending.entrySet)
      es.sort((a, b) => b.getValue.compareTo(a.getValue))   // newest first
      val it = es.iterator
      while it.hasNext do
        try it.next.getKey.close()
        catch case e if e.catchable => ()

    /** Run every still-pending backstopped cleanup now, newest first (for tests or explicit teardown). */
    def reapNow(): Unit = reapAll()

    /** The cleanup state, kept apart from [[Later]] on purpose: the registry above and the `Cleaner`
      * action both hold a `Reapable` and must *not* hold the `Later`, or it could never become unreachable
      * and the GC backstop would never fire.  So `Reapable` knows nothing of `Later`; `Later` only points
      * here. */
    private[Tidy] final class Reapable[R](r: R, clean: Clean[R]) extends Runnable, CanClose {
      private val spent = Atom(false)
      def open: Boolean = !spent()
      def held: R = r
      def run(): Unit = close()
      def close(): Unit =
        if spent.cas(false, true) then
          try clean(r) finally pending.remove(this) __ Unit
    }

    private def enroll[R](r: R, clean: Clean[R]): Reapable[R] =
      installHook()
      val reap = new Reapable(r, clean)
      pending.put(reap, java.lang.Long.valueOf(reapSeq.zapAndGet(_ + 1L))) __ Unit
      reap

    private[flow] def keepScoped[R](r: R, clean: Clean[R]): CanClose = enroll(r, clean)

    /** Take sole ownership of `r`, cleaned up by `clean` at [[Later.close]], shutdown, or GC. */
    def apply[R](r: R)(using clean: Clean[R]): Later[R] = new Later(enroll(r, clean))
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

  /** Acquire a resource and hand back the owning [[Tidy.Later]] instead of scoping it: cleanup runs at
    * `Later.close`, JVM shutdown, or GC.  The unmanaged-but-backstopped counterpart to [[unmanaged]]. */
  def closedLater[R](rsc: Tidy.Clean[R] ?=> R)(done: Tidy.Clean[R]): Tidy.Later[R] =
    Tidy.Later(rsc(using done))(using done)

  /** Like [[nice]], but the resource is also enrolled for cleanup at JVM shutdown for the duration of `f`,
    * so a `SIGTERM`/`SIGINT` (or normal exit) mid-`f` still releases it — which the `finally` alone cannot
    * guarantee.  Requires a [[Tidy.Clean]] (cleanup safe to run from the hook thread). */
  def clean[R, A](rsc: Tidy.Clean[R] ?=> Ask[R])(done: Tidy.Clean[R])(f: R => A): Ask[A] = boundary:
    var wrong: Throwable = null
    val result =
      val r = try { rsc(using done).? } catch { case e if e.catchable => boundary.break(Err.or(e)) }
      val keep = Tidy.Later.keepScoped(r, done)
      try Is(f(r))
      catch case e if e.catchable => Err.or(e)
      finally
        try keep.close()
        catch case e if e.catchable => wrong = e
    if result.isIs && (wrong ne null) then
      Alt(Err(wrong).explainValue("Operation succeeded but error encountered while closing resource", result.get))
    else result

  /** [[clean]] with `.?` early-return available inside `f`. */
  def Clean[R, A](rsc: Tidy.Clean[R] ?=> Ask[R])(done: Tidy.Clean[R])(f: boundary.Label[A Or Err] ?=> (R => A)): Ask[A] = boundary:
    var wrong: Throwable = null
    val result =
      val r = try { rsc(using done).? } catch { case e if e.catchable => boundary.break(Err.or(e)) }
      val keep = Tidy.Later.keepScoped(r, done)
      try Is(f(r))
      catch case e if e.catchable => Err.or(e)
      finally
        try keep.close()
        catch case e if e.catchable => wrong = e
    if result.isIs && (wrong ne null) then
      Alt(Err(wrong).explainValue("Operation succeeded but error encountered while closing resource", result.get))
    else result

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
