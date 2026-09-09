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

  private val closesAutoCloseable: Clean[AutoCloseable] = _.close()

  /** The cleanup every `AutoCloseable` already carries; pass as the `done` of any `Resource` verb
    * or `Later`/`Lease` factory, e.g. `Resource.clean(acquire)(Tidy.closes)(f)`.  Deliberately not
    * a given: `Tidy` is contravariant, so a given here would satisfy every `Tidy.Nice` witness and
    * erase the responsibility-taken discipline the witness exists to state.
    */
  def closes[R <: AutoCloseable]: Clean[R] = closesAutoCloseable

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

    /** [[use]] then [[close]].  If both throw, the close failure is added as suppressed to the op's — an
      * interrupted op included, so a close that fails cannot replace the interruption; a close interrupted
      * beside an op failure rides as suppressed too, with the thread's interrupt status re-set. */
    def useAndClose(f: R => Unit): Unit =
      var primary: Throwable = null
      try use(f)
      catch
        case t: InterruptedException =>
          primary = t
          throw t
        case t if t.catchable =>
          primary = t
          throw t
      finally closeAfter(primary)

    /** [[op]] then [[close]].  If both throw, the close failure is added as suppressed to the op's, an
      * interrupted op included (see [[useAndClose]]). */
    def opAndClose[A](f: R => A): A =
      var primary: Throwable = null
      try op(f)
      catch
        case t: InterruptedException =>
          primary = t
          throw t
        case t if t.catchable =>
          primary = t
          throw t
      finally closeAfter(primary)

    private def closeAfter(primary: Throwable): Unit =
      if primary eq null then close()
      else
        val unwind = new Unwind
        Unwind.fold(primary, unwind(close())) __ Unit
        unwind.restore(primary)

    /** [[nice]] then [[close]].  A close failure is never dropped: folded into a successful result as an
      * explanation of its value, or combined with the op's own error when both fail. */
    def niceAndClose[A](f: boundary.Label[A Or Err] ?=> (R => A)): Ask[A] = andClose(nice(f))

    /** [[flatNice]] then [[close]], with the same both-errors-preserved folding as [[niceAndClose]]. */
    def flatNiceAndClose[A](f: boundary.Label[A Or Err] ?=> (R => Ask[A])): Ask[A] = andClose(flatNice(f))

    /** A point-in-time snapshot of whether the resource is still open — for sequential introspection, not
      * a guard against a concurrent [[close]] (see the class note). */
    def isOpen: Boolean = reap.open

    // An interruption leaves as the exception once the close has run, as everywhere in the nice family:
    // see Resource.closing.
    inline private def andClose[A](inline ans: => Ask[A]): Ask[A] =
      var exit: Throwable = null
      var wrong: Throwable = null
      var a = Ask.ghosted[A]
      try a = ans  // NOTE: catch not necessary because we only wrap thunks that already catch
      catch case t: Throwable => { exit = t; throw t }
      finally wrong = Resource.closing(close(), exit, a.fold(_ => null)(_.toThrowable))
      if wrong eq null then a
      else a.fold{ 
          x => Alt(Err(wrong).explainValue("Operation succeeded but error encountered while closing resource", x))
        }{
          e => Alt(Err(e, Err(wrong))("Failure in operation on resource and in closing"))
        }
  }
  object Later {
    private[Tidy] val reaper  = Cleaner.create()
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
      val unwind = new Unwind
      val it = es.iterator
      while it.hasNext do unwind(it.next.getKey.close()) __ Unit
      unwind.restore()

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

    private[Tidy] def enroll[R](r: R, clean: Clean[R]): Reapable[R] =
      installHook()
      val reap = new Reapable(r, clean)
      pending.put(reap, java.lang.Long.valueOf(reapSeq.zapAndGet(_ + 1L))) __ Unit
      reap

    private[flow] def keepScoped[R](r: R, clean: Clean[R]): CanClose = enroll(r, clean)

    /** Take sole ownership of `r`, cleaned up by `clean` at [[Later.close]], shutdown, or GC. */
    def apply[R](r: R)(using clean: Clean[R]): Later[R] = new Later(enroll(r, clean))
  }


  /** Shared owner of a resource that concurrent borrowers use in scoped calls: each `use`/`op`/
    * `nice`/`flatNice` is a counted borrow, and [[close]] refuses new borrows but defers the
    * (exactly-once) cleanup until the last borrow returns -- so a server can retire or replace a
    * live resource without either leaking it or yanking it from a mid-flight reader.  The
    * check-and-borrow is a single compare-and-swap, so there is no check-then-act race against a
    * concurrent close.  As with [[Later]], cleanup is backstopped at JVM shutdown or GC -- there it
    * runs immediately, since borrowers cannot delay a dying process and an unreachable `Lease` has
    * none.  To replace a shared resource, swap in a fresh `Lease` and `close` the old one: it
    * drains and cleans itself.
    */
  final class Lease[R] private[Tidy] (reap: Later.Reapable[R]) {
    // The ENTIRE protocol lives on this one atomic: borrow count in the low bits, sign bit set =
    // closing.  Every guarded transition is a read-check-CAS loop -- never a bare increment --
    // because the sign bit changing the value is exactly what makes a raced CAS fail.  ABA is
    // moot: the state is fully encoded in the value.  Reapable's own once-latch is a downstream
    // idempotence backstop, never a coordinator; no invariant may span both atomics.
    private val state = Atom(0L)
    private val cleanable = Later.reaper.register(this, reap)

    private def borrowed(): Boolean =
      var ok = false
      var go = reap.open
      while go do
        val cur = state()
        if cur < 0 then go = false
        else if state.cas(cur, cur + 1) then   // check and increment must stay fused in this CAS
          ok = true
          go = false
      ok

    // The unguarded decrement is safe only because pairing is structural: release is called
    // solely from the finally of a successful borrow.  The return-value test is the
    // linearization point: exactly one release can step MinValue+1 -> MinValue, so the drain
    // cleans exactly once, and same-variable ordering makes every borrower's work visible to it.
    private def release(): Unit =
      if state.zapAndGet(_ - 1) == Long.MinValue then cleanable.clean()

    /** Refuse new borrows; cleanup runs now if no borrow is outstanding, otherwise when the last
      * borrow returns.  Idempotent, and never blocks: the drain is asynchronous.
      */
    def close(): Unit =
      var go = true
      while go do
        val cur = state()
        if cur < 0 then go = false
        else if state.cas(cur, cur | Long.MinValue) then   // only one CAS from a sign-free value can win
          if cur == 0L then cleanable.clean()              // idle -> closed: we clean; else the drain does
          go = false

    /** Borrow the resource for a side-effecting `f`; throws if closing or closed. */
    def use(f: R => Unit): Unit =
      if borrowed() then
        try f(reap.held) finally release()
      else throw new IllegalStateException("Already closed")

    /** Borrow the resource to compute with `f`; throws if closing or closed. */
    def op[A](f: R => A): A =
      if borrowed() then
        try f(reap.held) finally release()
      else throw new IllegalStateException("Already closed")

    /** Borrow the resource with `.?` early return available; closing or closed yields `Alt(Err)`. */
    def nice[A](f: boundary.Label[A Or Err] ?=> (R => A)): Ask[A] =
      if borrowed() then
        try
          boundary:
            try Is(f(reap.held))
            catch case e if e.catchable => Err.or(e)
        finally release()
      else Alt(Err("Already closed"))

    /** Like [[nice]], but `f` itself yields an `Ask`. */
    def flatNice[A](f: boundary.Label[A Or Err] ?=> (R => Ask[A])): Ask[A] =
      if borrowed() then
        try
          boundary:
            try f(reap.held)
            catch case e if e.catchable => Err.or(e)
        finally release()
      else Alt(Err("Already closed"))

    /** A point-in-time snapshot of whether new borrows are being accepted -- for sequential
      * introspection, not a guard against a concurrent [[close]] (the borrow itself is the guard). */
    def isOpen: Boolean = state() >= 0 && reap.open
  }
  object Lease {
    /** Take shared ownership of `r`, cleaned by `clean` once [[Lease.close]] has been called and
      * every borrow has returned -- or at JVM shutdown or GC, immediately. */
    def apply[R](r: R)(using clean: Clean[R]): Lease[R] = new Lease(Later.enroll(r, clean))
  }
}


/** Runs the steps of a teardown so that none of them can cut the rest short, and keeps the thread's
  * interruption aside while they run.  Each step goes through [[apply]], which answers what the step threw
  * instead of throwing it, so the caller decides how failures combine ([[Unwind.fold]] is the one rule for
  * joining a second to a first).  Before every step a pending interrupt status is cleared, and remembered:
  * each step runs as though never interrupted, so a release that must block to be clean gets to, and a step
  * that blocks is cut only by a fresh interrupt from outside, which is remembered too and cuts only that
  * step.  Once the last step has run, [[restore]] re-establishes what was remembered.  One per teardown.
  */
final class Unwind {
  private var noted = false
  private var stop: InterruptedException = null

  /** Runs `step` in isolation, answering what it threw — `null` if nothing.  A stray control-flow break is
    * caught too (`threadCatchable`, not `catchable`): a teardown step is no place for a non-local exit, and
    * one that escaped would abandon every step after it. */
  def apply(step: => Unit): Throwable =
    if Thread.interrupted() then noted = true
    try
      step
      null
    catch
      case t: InterruptedException =>
        noted = true
        if stop eq null then stop = t
        t
      case t if t.threadCatchable => t

  /** Whether an interruption has been seen: pending before a step, or thrown by one. */
  def interrupted: Boolean = noted

  /** The first interruption a step threw, or `null` — for a caller that must let it leave as the exception
    * it is, where what was leaving could not carry it (an early return, say), rather than [[restore]] it. */
  def interruption: InterruptedException = stop

  /** Re-establishes the thread's interrupt status if an interruption was seen, unless `leaving` — the
    * exception about to leave the enclosing block, if any — is that interruption itself, which needs no
    * flag beside it.  Call once every step has run. */
  def restore(leaving: Throwable = null): Unit =
    if noted && !leaving.isInstanceOf[InterruptedException] then Thread.currentThread.interrupt()
}
object Unwind {
  /** `extra` joined to `primary` as suppressed — never itself, which Java refuses — answering the primary,
    * or `extra` alone when there was none. */
  def fold(primary: Throwable, extra: Throwable): Throwable =
    if primary eq null then extra
    else
      if (extra ne null) && (extra ne primary) then primary.addSuppressed(extra)
      primary
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

  /** The close of a use-scope, run as one [[Unwind]] step: with the thread's interrupt status clear, and that
    * status restored after.  Answers a catchable close failure for the caller to report, or `null`.  An
    * interruption never becomes a value here, as it never does in `nice`: it leaves as the exception, once
    * the close has run, with whatever else there was to report suppressed into it — a close failure into the
    * body's interruption (`exit`), or the body's failure (`failed`; `null` for a success, whose value is
    * dropped, since a close cut short makes it unreliable) into the close's.  An early return in flight
    * cannot carry the close's interruption, so the interruption leaves in its place.  A stray control-flow
    * break from the close flies, as it always did. */
  private[flow] def closing(close: => Unit, exit: Throwable, failed: => Throwable): Throwable =
    val unwind = new Unwind
    val t = unwind(close)
    if exit ne null then
      if t.isInstanceOf[InterruptedException] && isJump(exit) then throw t
      Unwind.fold(exit, t) __ Unit
      unwind.restore(exit)
      null
    else if t eq null then
      unwind.restore()
      null
    else if t.isInstanceOf[InterruptedException] then
      Unwind.fold(t, failed) __ Unit
      throw t
    else
      unwind.restore(t)
      if t.catchable then t else throw t

  private def isJump(t: Throwable): Boolean =
    t.isInstanceOf[scala.util.control.ControlThrowable] || t.isInstanceOf[scala.util.boundary.Break[?]]

  def safe[R, A](rsc: Tidy[R] ?=> R)(done: Tidy[R])(f: R => A): A Or Throwable = boundary:
    val r = try { rsc(using done) } catch { case e if e.catchable => boundary.break(Alt(e)) }
    var exit: Throwable = null
    var failed: Throwable = null
    var wrong: Throwable = null
    val result =
      try Is(f(r))
      catch
        case e if e.catchable => { failed = e; Alt(e) }
        case t: Throwable => { exit = t; throw t }
      finally wrong = closing(done(r), exit, failed)
    if result.isIs && (wrong ne null) then Alt(wrong) else result

  /** Acquires a resource, uses it, and closes it, as values: a failure to acquire, a failure of the use, or a
    * failure of the close after a success is the `Alt`.  An interruption is not a failure and leaves as the
    * exception it is, once the close has run (see [[closing]]). */
  def nice[R, A](rsc: Tidy.Nice[R] ?=> Ask[R])(done: Tidy.Nice[R])(f: R => A): Ask[A] = boundary:
    val r = try { rsc(using done).? } catch { case e if e.catchable => boundary.break(Err.or(e)) }
    var exit: Throwable = null
    var failed: Throwable = null
    var wrong: Throwable = null
    val result =
      try Is(f(r))
      catch
        case e if e.catchable => { failed = e; Err.or(e) }
        case t: Throwable => { exit = t; throw t }
      finally wrong = closing(done(r), exit, failed)
    if result.isIs && (wrong ne null) then
      Alt(Err(wrong).explainValue("Operation succeeded but error encountered while closing resource", result.get))
    else result

  /** [[nice]] with `.?` early-return available inside `f`. */
  inline def Nice[R, A](rsc: Tidy.Nice[R] ?=> Ask[R])(done: Tidy.Nice[R])(inline f: boundary.Label[A Or Err] ?=> (R => A)): Ask[A] =
    boundary:
      val r = try { rsc(using done).? } catch { case e if e.catchable => boundary.break(Err.or(e)) }
      var exit: Throwable = null
      var failed: Throwable = null
      var wrong: Throwable = null
      val result =
        try Is(f(r))
        catch
          case e if e.catchable => { failed = e; Err.or(e) }
          case t: Throwable => { exit = t; throw t }
        finally wrong = closing(done(r), exit, failed)
      if result.isIs && (wrong ne null) then
        Alt(Err(wrong).explainValue("Operation succeeded but error encountered while closing resource", result.get))
      else result

  def unmanaged[R](rsc: Tidy[R] ?=> R): R = rsc(using Tidy.doNothing)

  /** Acquire a resource and hand back the owning [[Tidy.Later]] instead of scoping it: cleanup runs at
    * `Later.close`, JVM shutdown, or GC.  The unmanaged-but-backstopped counterpart to [[unmanaged]]. */
  def closedLater[R](rsc: Tidy.Clean[R] ?=> R)(done: Tidy.Clean[R]): Tidy.Later[R] =
    Tidy.Later(rsc(using done))(using done)

  /** Acquire a resource and hand back an owning [[Tidy.Lease]]: concurrent borrowers use it in
    * scoped calls, and `Lease.close` defers cleanup until the borrows drain.  The shared-ownership
    * counterpart to [[closedLater]]. */
  def leased[R](rsc: Tidy.Clean[R] ?=> R)(done: Tidy.Clean[R]): Tidy.Lease[R] =
    Tidy.Lease(rsc(using done))(using done)

  /** Like [[nice]], but the resource is also enrolled for cleanup at JVM shutdown for the duration of `f`,
    * so a `SIGTERM`/`SIGINT` (or normal exit) mid-`f` still releases it — which the `finally` alone cannot
    * guarantee.  Requires a [[Tidy.Clean]] (cleanup safe to run from the hook thread). */
  def clean[R, A](rsc: Tidy.Clean[R] ?=> Ask[R])(done: Tidy.Clean[R])(f: R => A): Ask[A] = boundary:
    val r = try { rsc(using done).? } catch { case e if e.catchable => boundary.break(Err.or(e)) }
    val keep = Tidy.Later.keepScoped(r, done)
    var exit: Throwable = null
    var failed: Throwable = null
    var wrong: Throwable = null
    val result =
      try Is(f(r))
      catch
        case e if e.catchable => { failed = e; Err.or(e) }
        case t: Throwable => { exit = t; throw t }
      finally wrong = closing(keep.close(), exit, failed)
    if result.isIs && (wrong ne null) then
      Alt(Err(wrong).explainValue("Operation succeeded but error encountered while closing resource", result.get))
    else result

  /** [[clean]] with `.?` early-return available inside `f`. */
  def Clean[R, A](rsc: Tidy.Clean[R] ?=> Ask[R])(done: Tidy.Clean[R])(f: boundary.Label[A Or Err] ?=> (R => A)): Ask[A] = boundary:
    val r = try { rsc(using done).? } catch { case e if e.catchable => boundary.break(Err.or(e)) }
    val keep = Tidy.Later.keepScoped(r, done)
    var exit: Throwable = null
    var failed: Throwable = null
    var wrong: Throwable = null
    val result =
      try Is(f(r))
      catch
        case e if e.catchable => { failed = e; Err.or(e) }
        case t: Throwable => { exit = t; throw t }
      finally wrong = closing(keep.close(), exit, failed)
    if result.isIs && (wrong ne null) then
      Alt(Err(wrong).explainValue("Operation succeeded but error encountered while closing resource", result.get))
    else result

  ////////////////////////////////////////////////////////////////////////
  /// assemble: acquire a chain of things and hand some of them on       ///
  ////////////////////////////////////////////////////////////////////////

  /** A value acquired inside [[assemble]] with [[guard]]: torn down as the block exits, whichever way, unless
    * released or unguarded first.  A `Guarded[X, U]` is an `X` — use it as one inside the block — but it cannot
    * be handed out; only a [[Released]] can.  `U` is the block it belongs to, the singleton type of that block's
    * [[Undo]], so an inner `assemble` cannot release an outer block's guard: `release` finds the innermost `Undo`
    * and demands that the guard's tag be its type.  A guard's fate is decided by the block that armed it.
    */
  opaque type Guarded[+X, +U <: Undo] <: X = X

  /** A value [[release]]d inside [[assemble]]: the only thing the block may hand out, singly or in a tuple, and
    * nothing else can be done with it — it is on its way out, and the caller receives it bare.  A released value
    * is still torn down if the block then fails: release says what survives a success, not that its undo is
    * forgotten.  `U` is the block that released it.
    */
  opaque type Released[+X, +U <: Undo] = X

  /** The registry an [[assemble]] block adds to, and the block's identity in the types of what it guards.
    * Tear-downs run newest first, so a chain unwinds in reverse.  For one thread's use, within one block, through
    * the verbs [[guard]], [[temp]], [[release]], [[releaseOp]], [[unguarded]] and [[unguardedOp]].
    */
  final class Undo private[Resource] () {
    private[Resource] final class Entry(val value: Any, val undo: () => Unit, val guarded: Boolean) { var released = false }
    private var entries: List[Entry] = Nil
    /** Every tear-down runs through this, so an interruption among them is kept aside and re-established by
      * [[assemble]] once the last has run — a tear-down run in between cannot consume it. */
    private[Resource] val steps = new Unwind

    private[Resource] def add(value: Any, undo: () => Unit, guarded: Boolean): Unit =
      entries = new Entry(value, undo, guarded) :: entries

    private[Resource] def drop(e: Entry): Unit =
      entries = entries.filterNot(_ eq e)

    /** Identity, or equality for the box a primitive guard passes through, since an `Int` descriptor is boxed anew each time. */
    private def same(a: Any, b: Any): Boolean =
      (a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]) ||
      ((a.isInstanceOf[java.lang.Number] || a.isInstanceOf[java.lang.Character] || a.isInstanceOf[java.lang.Boolean]) && a.getClass == b.getClass && a == b)

    /** The live guard for `g`, newest first: an error if `g` was never guarded here, or has been released already. */
    private[Resource] def guardOf(g: Any): Entry =
      var es = entries
      var found: Entry = null
      while (found eq null) && (es ne Nil) do
        val e = es.head
        if e.guarded && same(e.value, g) then found = e
        es = es.tail
      if found eq null then throw new IllegalStateException(s"not guarded by this assembly: $g")
      if found.released then throw new IllegalStateException(s"already released: $g")
      found

    /** Runs and drops every registered tear-down (or only those not released), newest first, answering the
      * first failure with the rest suppressed into it, or `null` if all went well.  An interrupted tear-down
      * does not stop the unwind: the interruption is kept as a failure like any other, and propagates after
      * the rest have had their turn. */
    private[Resource] def unwind(all: Boolean): Throwable =
      var first: Throwable = null
      var es = entries
      var keep: List[Entry] = Nil
      while es ne Nil do
        val e = es.head
        es = es.tail
        if all || !e.released then first = Unwind.fold(first, steps(e.undo()))
        else keep = e :: keep
      entries = keep.reverse
      first
  }

  /** Acquires a chain of things in order to hand some of them on, which neither a use-scope (release everything
    * at the end) nor an owner ([[Tidy.Later]]) expresses.  Inside `f`, everything acquired is registered by one
    * of two verbs: [[guard]] for what may be handed out, [[temp]] for what may not.  Both are torn down as the
    * block exits — a guard that is never released is torn down on success too, so nothing acquired can simply be
    * lost — and a guard survives only through the one expression that consumes it: [[release]] where the guard
    * itself is the result, [[releaseOp]] where something built from it is, or [[unguardedOp]] where that
    * something is a new owner, to be guarded in its turn.  Tear-downs run newest first, so the chain unwinds in
    * reverse of its acquisition.  The block fails by exception or by early return (`.?` to an enclosing boundary)
    * alike, and then everything is torn down, released or not: a failed assembly hands out nothing, so it keeps
    * nothing.  It succeeds only if it completes and every tear-down that runs is clean — if one is not, the
    * released values are torn down too and that failure is thrown.  A tear-down that fails during a failed
    * unwind is suppressed into the exception; on an early return there is nothing to attach it to, and it is
    * dropped.  An interrupted tear-down never cuts the unwind short: the interruption is thrown once the rest
    * are done, or, where another failure is already on its way out, suppressed into it — and where an early
    * return is on its way out, which cannot carry it, the interruption leaves in the return's place, since an
    * interruption never becomes a value.  In every case where the interruption is not itself the exception
    * leaving, the thread's interrupt status is set once the last tear-down has run, so none in between can
    * consume it.  Every tear-down runs with that status clear (see [[Unwind]]), so one that must block to be
    * clean gets to.  Only [[Released]] values, singly or in a tuple, may be returned, and the caller receives
    * them bare.
    * {{{
    * Resource.assemble:
    *   val tmp = Resource.temp(Arena.ofConfined())(_.close())      // gone when the block exits
    *   val fd  = Resource.temp(open(name))(close)                  // the mapping keeps the memory: transient too
    *   size(fd, bytes)
    *   val file  = Resource.guard(name)(unlink)                    // torn down unless released: the file goes if we fail from here
    *   val arena = Resource.guard(Arena.ofShared())(_.close())     // likewise, and usable as an Arena meanwhile
    *   Resource.releaseOp(file, arena)((n, a) => new Region(n, map(fd, a)))   // the result, built from both and handed out in their place
    * }}}
    */
  def assemble[T](f: Undo ?=> T)(using as: Assembled[T]): as.Out =
    val u = new Undo()
    var done = false
    var exit: Throwable = null
    var t: T = null.asInstanceOf[T]
    try
      t = f(using u)
      done = true
    catch
      case e: InterruptedException =>
        exit = e
        throw e
      case e if e.catchable =>
        exit = e
        throw e
    finally
      val bad =
        if done then
          val b = u.unwind(all = false)
          if b ne null then Unwind.fold(b, u.unwind(all = true)) else null
        else u.unwind(all = true)
      val leaving: Throwable =                        // the exception this block exits by, if any
        if done then bad
        else if exit ne null then Unwind.fold(exit, bad)
        else u.steps.interruption                     // an early return: a tear-down failure has nowhere to go, but an interruption leaves in its place
      u.steps.restore(leaving)
      if (leaving ne null) && (exit eq null) then throw leaving
    as.out(t)

  /** Within [[assemble]]: `x` is torn down as the block exits unless released or unguarded first — a thing the
    * block is making, tagged as this block's.  Its fate is decided by exactly one later expression: [[release]]
    * where it is handed out itself, or the [[releaseOp]] or [[unguardedOp]] that builds from it. */
  def guard[X](x: X)(undo: X => Unit)(using u: Undo): Guarded[X, u.type] =
    u.add(x, () => undo(x), guarded = true)
    x

  /** Within [[assemble]]: holds `x` only while assembling, torn down whichever way the block exits — a
    * transient, usable inside and never handed out. */
  def temp[X](x: X)(undo: X => Unit)(using u: Undo): X =
    u.add(x, () => undo(x), guarded = false)
    x

  /** Within [[assemble]]: `g` is meant to survive — not torn down on success, still torn down on failure — and
    * is answered as the [[Released]] the block may hand out.  Only this block's own guards can be released here.
    * This is the block's result, alone or in a tuple: a `release(g)` whose value is discarded has released `g`
    * early and says nothing about what carries it — build that thing with [[releaseOp]] instead. */
  def release[X, U <: Undo & Singleton](g: Guarded[X, U])(using u: Undo)(using U =:= u.type): Released[X, u.type] =
    u.guardOf(g).released = true
    g

  /** Within [[assemble]]: hands out `f(g)` in place of `g`, which is released once `f` has succeeded and not
    * before, so a failure building the result still tears `g` down — a handle around a descriptor, say, without
    * pretending it needs an undo of its own. */
  def releaseOp[X, U <: Undo & Singleton, Y](g: Guarded[X, U])(f: X => Y)(using u: Undo)(using U =:= u.type): Released[Y, u.type] =
    val e = u.guardOf(g)
    val y = f(g)
    e.released = true
    y

  /** [[releaseOp]] over two guards, for a result that carries both — a server around a descriptor and the
    * socket file it is bound to.  Both are released once `f` has succeeded, and neither before. */
  def releaseOp[X1, X2, U <: Undo & Singleton, Y](g1: Guarded[X1, U], g2: Guarded[X2, U])(f: (X1, X2) => Y)(using u: Undo)(using U =:= u.type): Released[Y, u.type] =
    val e1 = u.guardOf(g1)
    val e2 = u.guardOf(g2)
    val y = f(g1, g2)
    e1.released = true
    e2.released = true
    y

  /** Within [[assemble]]: forgets `g`'s undo and answers `g` bare, for a call that consumes it whichever way that
    * call ends — `consume(Resource.unguarded(g))` — where a guard still armed would close it a second time.
    * Where the new owner is something to be built, use [[unguardedOp]], which keeps `g` guarded until it is. */
  def unguarded[X, U <: Undo & Singleton](g: Guarded[X, U])(using u: Undo)(using U =:= u.type): X =
    u.drop(u.guardOf(g))
    g

  /** Within [[assemble]]: `f(g)`, an owner that tears `g` down itself from now on, so `g`'s own undo is forgotten
    * once `f` has succeeded — and not before, so a failure building the owner still tears `g` down.  The owner
    * comes back bare, to be guarded (or released) in its turn. */
  def unguardedOp[X, U <: Undo & Singleton, Y](g: Guarded[X, U])(f: X => Y)(using u: Undo)(using U =:= u.type): Y =
    val e = u.guardOf(g)
    val y = f(g)
    u.drop(e)
    y

  /** [[unguardedOp]] over two guards, for an owner that takes both. */
  def unguardedOp[X1, X2, U <: Undo & Singleton, Y](g1: Guarded[X1, U], g2: Guarded[X2, U])(f: (X1, X2) => Y)(using u: Undo)(using U =:= u.type): Y =
    val e1 = u.guardOf(g1)
    val e2 = u.guardOf(g2)
    val y = f(g1, g2)
    u.drop(e1)
    u.drop(e2)
    y

  final class Manager() extends Tidy.CanClose {
    private var items: List[Tidy.CanClose] = Nil
    def +=(cc: Tidy.CanClose): Unit =
      items = cc :: items
    /** Closes everything, newest first, and lets nothing cut that short (see [[Unwind]]): the one failure is
      * thrown, or all of them as one, with an interruption among them re-set as the thread's interrupt status. */
    def close(): Unit =
      if items ne null then
        val unwind = new Unwind
        var es: List[Throwable] = Nil
        var n = 0
        while items ne Nil do
          val item = items.head
          items = items.tail
          val t = unwind(item.close())
          if t ne null then es = t :: es
          n += 1
        items = null
        val thrown: Throwable = es match
          case Nil => null
          case e :: Nil => e
          case lots => Err(ErrType.Many(lots.map(Err.apply), s"${lots.length} exceptions while closing $n resources")).toThrowable
        unwind.restore(thrown)
        if thrown ne null then throw thrown
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

/** What a [[Resource.assemble]] block may hand out — one [[Resource.Released]] value, or a tuple of them — and the
  * same with the wrappers taken off, which is what the caller receives.  Lives outside `Resource` because a match
  * type can only take a `Released` apart where it is opaque, not where it is the alias it is defined as.  By the
  * time a block's result reaches here its tag has widened to `Undo`, the block's own `Undo` being out of scope,
  * so the tag is checked where it matters — at [[Resource.release]] — and taken as read here. */
sealed trait Assembled[T] {
  type Out
  def out(t: T): Out
}
object Assembled {
  given one[A, U <: Resource.Undo]: (Assembled[Resource.Released[A, U]] { type Out = A }) = new Assembled[Resource.Released[A, U]] {
    type Out = A
    def out(t: Resource.Released[A, U]): A = t.asInstanceOf[A]   // a Released is its value
  }
  given many[T <: Tuple](using Tuple.IsMappedBy[[x] =>> Resource.Released[x, Resource.Undo]][T]): (Assembled[T] { type Out = Tuple.InverseMap[T, [x] =>> Resource.Released[x, Resource.Undo]] }) =
    new Assembled[T] {
      type Out = Tuple.InverseMap[T, [x] =>> Resource.Released[x, Resource.Undo]]
      def out(t: T): Out = t.asInstanceOf[Out]   // each Released is its value, so the tuple already is its own bare form
    }
}

def manage[A](rsc: Tidy[A] ?=> A)(done: Tidy[A])(using manager: Resource.Manager): A =
  val r = rsc(using done)
  manager += Tidy.Managed(r, done)
  r
