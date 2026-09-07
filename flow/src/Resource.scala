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

  /** Acquire a resource and hand back an owning [[Tidy.Lease]]: concurrent borrowers use it in
    * scoped calls, and `Lease.close` defers cleanup until the borrows drain.  The shared-ownership
    * counterpart to [[closedLater]]. */
  def leased[R](rsc: Tidy.Clean[R] ?=> R)(done: Tidy.Clean[R]): Tidy.Lease[R] =
    Tidy.Lease(rsc(using done))(using done)

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

  ////////////////////////////////////////////////////////////////////////
  /// assemble: acquire a chain of things and hand the survivors on      ///
  ////////////////////////////////////////////////////////////////////////

  /** A value acquired inside [[assemble]] and registered with `onFailure`, to be released only if the assembly
    * fails: the one kind of thing an `assemble` block may hand out, since anything held [[whileAssembling]] is
    * released as the block exits.  A `Guarded[R]` is an `R` — use it as one inside the block — but a bare `R`
    * is never a `Guarded[R]`, so a transient cannot be returned by mistake.  (A value *derived* from a
    * transient, such as a handle built around a descriptor held only while assembling, is beyond what a type
    * can catch: guard the derived value and hold the transient with nothing, or think again about which one
    * survives.)
    */
  opaque type Guarded[+R] <: R = R
  extension [R](g: Guarded[R])
    /** The monadic map: a result built from a survivor is a survivor, which is how a `Region` made of a guarded
      * arena is handed out rather than the arena itself, without pretending the result needs an undo of its
      * own.  Named in full because a `Guarded[R]` is an `R`, and `R` may well have a `map` of its own. */
    def mapGuarded[S](f: R => S): Guarded[S] = f(g)

  /** The registry an [[assemble]] block adds to.  Releases run newest first, so a chain unwinds in reverse:
    * [[onFailure]] for what survives on success and is released on failure, [[whileAssembling]] for what is
    * released either way.  For one thread's use, within one block.
    */
  final class Undo private[Resource] () {
    private final class Entry(val release: () => Unit, val always: Boolean)
    private var entries: List[Entry] = Nil
    /** Whether any release was interrupted, kept apart from the failures so [[assemble]] can signal it once
      * every release has run — a release run in between could otherwise consume the thread's interrupt status. */
    private[Resource] var interrupted: Boolean = false

    /** Registers `r` to be released only if the assembly fails, answering it as the [[Guarded]] the block may hand out. */
    def onFailure[R](r: R)(release: R => Unit): Guarded[R] =
      entries = new Entry(() => release(r), false) :: entries
      r

    /** Registers `r` to be released whichever way the block exits: a transient, usable inside and nowhere else. */
    def whileAssembling[R](r: R)(release: R => Unit): R =
      entries = new Entry(() => release(r), true) :: entries
      r

    /** Runs and drops every registered release (or only the `always` ones), newest first, answering the
      * first failure with the rest suppressed into it, or `null` if all went well.  An interrupted release
      * does not stop the unwind: the interruption is kept as a failure like any other, and propagates after
      * the rest have had their turn. */
    private[Resource] def unwind(all: Boolean): Throwable =
      var first: Throwable = null
      var es = entries
      var keep: List[Entry] = Nil
      while es ne Nil do
        val e = es.head
        es = es.tail
        if all || e.always then
          try e.release()
          catch
            case t: InterruptedException =>
              interrupted = true
              first = fold(first, t)
            case t if t.catchable => first = fold(first, t)
        else keep = e :: keep
      entries = keep.reverse
      first
  }

  /** `extra` joined to `primary` as suppressed — never itself, which Java refuses — answering the primary, or
    * `extra` alone when there was none. */
  private def fold(primary: Throwable, extra: Throwable): Throwable =
    if primary eq null then extra
    else
      if (extra ne null) && (extra ne primary) then primary.addSuppressed(extra)
      primary

  /** Acquires a chain of things in order to hand the survivors on, which neither a use-scope (release
    * everything at the end) nor an owner ([[Tidy.Later]]) expresses.  Inside `f`, `x.onFailure(release)`
    * registers `x` as meant to survive, releasing it only if the block fails; [[whileAssembling]] registers a
    * transient, released either way.  Releases run newest first, so the chain unwinds in reverse of its acquisition.  The block fails by
    * exception or by early return (`.?` to an enclosing boundary) alike, and succeeds only if it completes and
    * every transient releases cleanly — if one does not, the survivors are undone too and that failure is
    * thrown.  A release that fails during a failed unwind is suppressed into the exception; on an early
    * return there is nothing to attach it to, and it is dropped.  An interrupted release never cuts the
    * unwind short: the interruption is thrown once the rest are released, or, where another failure is
    * already on its way out, suppressed into it — and in every case where the interruption is not itself
    * the exception leaving, the thread's interrupt status is set once the last release has run, so no
    * release in between can consume it.  Only [[Guarded]] values, singly or in a
    * tuple, may be returned — what `onFailure` answered, or what `mapGuarded` built from it — and the caller
    * receives them bare.
    * {{{
    * Resource.assemble:
    *   val tmp = Resource.whileAssembling(Arena.ofConfined())(_.close())   // gone when the block exits
    *   val fd  = Resource.whileAssembling(open(name))(close)               // the mapping keeps the memory: transient too
    *   size(fd, bytes)
    *   name.onFailure(unlink) __ Unit                                      // only if we fail from here on
    *   val arena = Arena.ofShared().onFailure(_.close())                   // survives: Guarded[Arena], usable as an Arena
    *   arena.mapGuarded(a => new Region(name, map(fd, a)))                 // the result, guarded because built from one
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
          if b ne null then fold(b, u.unwind(all = true)) else null
        else u.unwind(all = true)
      val leaving: Throwable =                        // the exception this block exits by, if any
        if done then bad
        else if exit ne null then fold(exit, bad)
        else null                                     // an early return: a release failure has nowhere to go
      if u.interrupted && !leaving.isInstanceOf[InterruptedException] then Thread.currentThread.interrupt()
      if done && (bad ne null) then throw bad
    as.out(t)

  /** Within [[assemble]]: holds `r` only while assembling, released whichever way the block exits — a
    * transient, usable inside and never handed out. */
  def whileAssembling[R](r: R)(release: R => Unit)(using u: Undo): R = u.whileAssembling(r)(release)

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

/** What a [[Resource.assemble]] block may hand out — one [[Resource.Guarded]] value, or a tuple of them — and the
  * same with the guards taken off, which is what the caller receives.  Lives outside `Resource` because a match
  * type can only take a `Guarded` apart where it is opaque, not where it is the alias it is defined as. */
sealed trait Assembled[T] {
  type Out
  def out(t: T): Out
}
object Assembled {
  given one[A]: (Assembled[Resource.Guarded[A]] { type Out = A }) = new Assembled[Resource.Guarded[A]] {
    type Out = A
    def out(t: Resource.Guarded[A]): A = t
  }
  given many[T <: Tuple](using Tuple.IsMappedBy[Resource.Guarded][T]): (Assembled[T] { type Out = Tuple.InverseMap[T, Resource.Guarded] }) =
    new Assembled[T] {
      type Out = Tuple.InverseMap[T, Resource.Guarded]
      def out(t: T): Out = t.asInstanceOf[Out]   // a Guarded is its value, so the tuple already is its own unguarded form
    }
}

extension [R](r: R)
  /** Within [[Resource.assemble]]: `r`, already made, is released only if the assembly fails, and is answered
    * as a [[Resource.Guarded]], the form the block may hand out.  Reads in the order things happen — the value
    * exists, and from here its undo is armed — which `guarded(r)` did not. */
  def onFailure(release: R => Unit)(using u: Resource.Undo): Resource.Guarded[R] = u.onFailure(r)(release)

def manage[A](rsc: Tidy[A] ?=> A)(done: Tidy[A])(using manager: Resource.Manager): A =
  val r = rsc(using done)
  manager += Tidy.Managed(r, done)
  r
