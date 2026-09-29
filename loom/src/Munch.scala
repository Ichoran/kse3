// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025-26 Rex Kerr.

package kse.loom

import java.util.concurrent.{CompletableFuture, ConcurrentHashMap}

import scala.annotation.publicInBinary
import scala.util.boundary.Label

import kse.basics._
import kse.flow._


/** Addressable, supervised, virtual-thread-per-entity consumers.
  *
  * A **muncher** is a behavior `M => Unit` running single-threaded over its own mailbox, with
  * private state in closed-over `var`s.  That much is already a `Go` `ch.get{ … }`; what Munch
  * adds is the part `Go` deliberately gave up to get its auto-close cascade:
  *
  *  - **First-class identity.**  A muncher is named (a key) and addressed as a *thing*, not as a
  *    channel.  The sender-set is open — anyone holding a [[Munch.Ref]] may send — and munchers are
  *    spawned dynamically (`spawn` / `getOrSpawn`), not wired at init.
  *  - **Per-entity supervision.**  A failing muncher consults *its own* [[Munch.Decider]] and
  *    Stops / Restarts / Escalates — it does **not** cancel its siblings.  This is the deliberate
  *    inversion of `Go.session`'s structured tree-fail, which is exactly wrong for independent
  *    entities (one bad connection must not kill the server).
  *  - **Typed feed (request/reply).**  `ref.feed(Msg.Stats(_))` returns a `Fu[B]` with `.?` baked
  *    in; the reply slot travels in the message as a [[Munch.Reply]], so the reply type flows
  *    through statically.  A muncher that dies with a feed outstanding completes it `Alt(gone)` —
  *    the caller never hangs.  (`feed`, not `ask`: in this codebase `ask` reads as "returns an
  *    `Ask`"; a feed hands back a `Fu`.)
  *  - **Absence is first-class.**  A `Ref` to a retired/absent muncher *drops* a tell (Erlang
  *    fire-and-forget; routed to the supervisor's dead-letter sink) and *fails* a feed cleanly.
  *    "Use it if it's there, else …" is then plain `Option`/`Or`/`Fu`: `get(k).getOrElse(other)`,
  *    `getOrSpawn(k){…}`, `feed(…).?`.
  *
  * Termination is **explicit** in v1 — a muncher ends on `Stop()` (drains its mailbox first) or when
  * the supervisor is stopped.  There is deliberately no global quiescence auto-detection
  * (the notoriously subtle part); add it only when a use-case demands it.
  *
  * For known static pipelines use `Go`/`Chan`; for CPU-bound work-generates-work use `Percolate`.
  */
object Munch {

  /** A muncher's behavior: what to do with each message.  Runs single-threaded; closes over the
    * muncher's private state.  A `.?`-break (or a thrown exception) fails *this message* and hands
    * control to the muncher's [[Decider]] — it never escapes to a sibling. */
  type Behavior[M] = M => (Go.CanFail[Unit] ?=> Unit)

  /** What a supervisor does when a muncher's behavior fails on a message. */
  enum Directive:
    case Stop       // run cleanups, fail outstanding feeds, remove the address — it becomes "gone"
    case Restart    // re-run the factory for fresh state, keeping the same mailbox + address
    case Escalate   // treat as fatal: stop the whole supervisor

  /** Decides a muncher's fate from the error and how many times it has already restarted.  The
    * common policies are named on [[Supervise]]; `Supervise.decide{ … }` gives full control. */
  type Decider = (Err, Int) => Directive

  object Supervise {
    /** Die on failure (temporary): the address goes gone, outstanding feeds fail.  The right default
      * for a handler bound to a now-broken external thing (a dead socket can't be restarted). */
    val stop: Decider = (_, _) => Directive.Stop
    /** Always restart with fresh state, keeping identity (Erlang `permanent`). */
    val restart: Decider = (_, _) => Directive.Restart
    /** Restart up to `n` times, then give up and Stop. */
    def restartUpTo(n: Int): Decider = (_, restarts) => if restarts < n then Directive.Restart else Directive.Stop
    /** Escalate: a muncher failure is fatal to the whole supervisor. */
    val escalate: Decider = (_, _) => Directive.Escalate
    /** Full control. */
    def decide(f: (Err, Int) => Directive): Decider = f
  }


  /** One feed, as one object: the caller's [[Fed]], the handler's [[Reply]], and the mailbox entry that carries the
    * message.  Its phase moves Queued -> Running -> Done, or Queued -> Withdrawn, or Running -> Cancelling -> Done;
    * every move out of Running is made under the muncher's monitor, which is what keeps a cancellation's interrupt
    * from landing on the next message. */
  private[loom] final class Feeding[M, B](private[loom] val mu: Muncher[M] | Null) extends CompletableFuture[Ask[B]] {
    @publicInBinary private[loom] var phase: Int = Feeding.Queued
    private[loom] var msg: M = scala.compiletime.uninitialized

    private[loom] def answer(a: Ask[B]): Unit = if complete(a) then settled()
    private[loom] def settled(): Unit = { val m = mu; if m ne null then m.settle(this) }

    /** Remove it if it has not started. */
    private[loom] def withdraw(): Boolean =
      Feeding.phaseH.compareAndSet(this, Feeding.Queued, Feeding.Withdrawn) && { answer(Alt(Err("feed withdrawn"))); true }

    /** Withdraw it, or, if it is running, fail its reply and interrupt its handler; false once it has answered. */
    private[loom] def cancelFeed(): Boolean =
      withdraw() || {
        val m = mu
        (m ne null) && complete(Alt(Err("feed cancelled"))) && { settled(); m.cancelRunning(this); true }
      }

    /** `Future.cancel`, so a plain `Fu` of a feed cancels the feed: with `mayInterrupt`, as `cancel()`; without, as
      * `withdraw()`, declining work already begun, which the contract allows. */
    override def cancel(mayInterrupt: Boolean): Boolean = if mayInterrupt then cancelFeed() else withdraw()
  }
  private[loom] object Feeding {
    import java.lang.invoke.MethodHandles
    import scala.annotation.static
    @static private[loom] final val phaseH =
      MethodHandles.privateLookupIn(classOf[Feeding[?, ?]], MethodHandles.lookup()).findVarHandle(classOf[Feeding[?, ?]], "phase", classOf[Int])
    final val Queued = 0
    final val Running = 1
    final val Cancelling = 2
    final val Withdrawn = 3
    final val Done = 4
  }

  /** What a priority message puts in the mailbox so a muncher parked on an empty one wakes and takes it. */
  private[loom] object Wake

  /** The caller's handle on one feed: the reply (`.?`, `await()`, or `fu` for a plain [[Fu]]), and two ways to say
    * it is no longer wanted, which differ on work already begun.  `withdraw()` removes it only if it has not
    * started; `cancel()` also stops it if it has, interrupting the muncher's thread, which is running that message
    * and no other.  Either way its reply fails and the muncher goes on to its next message; each answers whether it
    * took effect (a feed already answered, or already begun for `withdraw`, is left alone).  Stopping the muncher
    * itself is `Ref.stop` or `Ref.abort`.  The handle is the feed itself, so it costs nothing. */
  opaque type Fed[B] = Feeding[?, B]
  object Fed {
    private[loom] def of[B](f: Feeding[?, B]): Fed[B] = f
    private[loom] def failed[B](err: Err): Fed[B] =
      val f = new Feeding[Any, B](null)
      f.phase = Feeding.Done
      f.complete(Alt(err)) __ Unit
      f

    extension [B](f: Fed[B]) {
      /** The reply as a plain `Fu`; its `cancel()` cancels the feed as `Fed.cancel()` does. */
      def fu: Fu[B] = Fu.wrap(f)
      /** Wait for the reply. */
      def await(): Ask[B] =
        try f.get()
        catch case e if e.catchable => Alt(Err(e))
      def isComplete: Boolean = f.isDone
      /** Remove the feed if it has not started; true if it was removed. */
      def withdraw(): Boolean = f.withdraw()
      /** Remove the feed, or stop it if it is running; true unless it had already answered. */
      def cancel(): Boolean = f.cancelFeed()
      inline def ?[L >: Alt[Err]](using lb: Label[L]): B = kse.flow.?[B, Err](Fed.await(f)())(using lb)
    }
  }

  /** A typed, write-once reply slot carried inside a `feed` message.  The handler completes it with
    * `reply(value)` (or `reply := value`); the caller observes the result through its [[Fed]].  If
    * the muncher dies first, the runtime completes it with an error, so the caller never hangs.  A reply to a
    * feed already withdrawn or cancelled goes nowhere. */
  opaque type Reply[B] = Feeding[?, B]
  object Reply {
    private[loom] def of[B](f: Feeding[?, B]): Reply[B] = f
    extension [B](r: Reply[B]) {
      /** Answer the feed. */
      def apply(value: B): Unit = r.answer(Is(value))
      /** Answer the feed (alias for `apply`). */
      inline def :=(value: B): Unit = r.answer(Is(value))
      /** Answer the feed with an error (surfaces via the caller's `.?`). */
      def fail(err: Err): Unit = r.answer(Alt(err))
    }
  }


  /** The self-managing context a muncher's behavior factory runs *inside* (an [[Orchestrated]]).
    * It is contextual, so `Defer { … }` and `Stop()` work without naming it — no `self =>` ceremony.
    * `self` is still here for the cases that need it (hand your own address out, self-send); reach it
    * with [[Munch.self]].  `Defer` cleanup runs on retire, restart, or failure (LIFO); `Stop()` ends
    * the muncher after draining what it holds. */
  sealed trait Context[M] {
    def self: Ref[M]
    private[loom] def stopSelf(): Unit
    private[loom] def addDefer(body: List[Err] => Unit): Unit
    private[loom] def cancelledNow: Boolean
  }

  /** This muncher's own address, from inside its behavior (when you need to hand it out or self-send). */
  def self[M](using ctx: Context[M]): Ref[M] = ctx.self

  /** From inside a behavior: whether the message being handled is no longer wanted -- its feed was cancelled, or the
    * muncher is being aborted -- so a long handler can stop at a clean point instead of waiting for the interrupt
    * (which a handler that never blocks would not see).  A tell is never cancelled on its own. */
  def isCancelled[M](using ctx: Context[M]): Boolean = ctx.cancelledNow


  /** A handle to a muncher addressed by key.  Always resolvable to *something*: if the target is
    * absent or retired, a tell drops (to the supervisor's dead-letter sink) and a feed fails with a
    * clean "gone" error rather than hanging. */
  final class Ref[M] private[loom] (resolve: () => Muncher[M] | Null, onDrop: M => Unit) {
    /** Fire-and-forget.  Delivered if the target is live; otherwise dropped (dead-lettered). */
    infix def !(m: M): Unit =
      val mu = resolve()
      if (mu eq null) || !mu.deliver(m) then onDrop(m)

    /** Feed a request and get its typed reply as a [[Fed]], which can also withdraw or cancel it.  `make` builds
      * the message from the reply slot, e.g. `ref.feed(Msg.Stats(_))`.  A gone target yields a failed reply
      * (observe via `.?`). */
    def feed[B](make: Reply[B] => M): Fed[B] =
      val mu = resolve()
      if mu eq null then Fed.failed[B](Err("feed to a muncher that is gone"))
      else mu.feedDeliver(make)

    /** Put `m` ahead of everything queued: the muncher takes it next, once the message it is handling ends.  There
      * is one such slot, so this is refused (false) while another priority message waits, or if the muncher is
      * gone; nothing is queued then, and nothing is dead-lettered, since the caller is told.  For what makes the
      * queued work moot -- a changed state, how to wind down -- not for ordinary traffic. */
    def priority(m: M): Boolean =
      val mu = resolve()
      (mu ne null) && mu.offerPriority(m)

    /** Remove every message still queued, and answer how many.  The message being handled finishes and the muncher
      * goes on; the removed feeds' replies fail as withdrawn; a waiting priority message stays.  To stop one
      * request, withdraw or cancel its [[Fed]]. */
    def clear(): Int =
      val mu = resolve()
      if mu eq null then 0 else mu.clear()

    /** End the muncher gracefully, as `Stop()` does from inside: it takes nothing new, handles what is queued, runs
      * its cleanups and goes.  False if it was already gone.  Its supervisor and siblings go on. */
    def stop(): Boolean =
      val mu = resolve()
      (mu ne null) && { mu.stopGraceful(); true }

    /** End the muncher now: the message being handled is interrupted, what is queued fails, its cleanups run and it
      * goes.  False if it was already gone.  Use with care; to drop one request, cancel its [[Fed]], and to drop
      * the queue but keep the muncher, `clear()`. */
    def abort(): Boolean =
      val mu = resolve()
      (mu ne null) && { mu.stopHard(Err(s"${mu.name} aborted")); true }

    /** Is the target currently live? */
    def isLive: Boolean = resolve() ne null

    /** Route to `other` if this target is gone. */
    def orElse(other: Ref[M]): Ref[M] =
      new Ref(() => { val mu = resolve(); if mu ne null then mu else null }, m => other ! m)
  }


  // === The muncher itself: a mailbox + a virtual thread + supervised behavior ===

  final class Muncher[M] private[loom] (
    val name: String,
    capacity: Int,
    factory: Context[M] ?=> Behavior[M],
    decider: Decider,
    sup: Supervisor,
    removeSelf: Muncher[M] => Unit
  ) extends Context[M] {
    // A tell rides as itself, a feed as its Feeding, and a priority message's wake-up as Wake.
    private val mailbox = Chan[Any](capacity)
    @volatile private var live = true
    @volatile private var hardStop: Err Or Unit = Alt.unit   // the cause of a hard stop, once one is under way
    private val pending = ConcurrentHashMap.newKeySet[CompletableFuture[?]]()
    private var defers: List[List[Err] => Unit] = Nil
    private var thread: Thread = null
    // The feed being handled; set and cleared under this muncher's monitor, which a cancellation takes to interrupt.
    private var current: Feeding[M, ?] | Null = null
    // The one priority message waiting, or null.
    @publicInBinary private[loom] var urgent: AnyRef = null

    def isLive: Boolean = live

    val self: Ref[M] = new Ref(() => if live then this else null, m => sup.deadLetter(m))

    private[loom] def stopSelf(): Unit = mailbox.close() __ Unit
    private[loom] def addDefer(body: List[Err] => Unit): Unit = defers = body :: defers
    private[loom] def cancelledNow: Boolean =
      hardStop.isIs || {
        val f = current
        (f ne null) && { val st = Feeding.phaseH.getVolatile(f).asInstanceOf[Int]; st == Feeding.Cancelling }
      }

    /** Try to enqueue a message; false if the muncher is gone or its mailbox has closed. */
    private[loom] def deliver(m: Any): Boolean =
      live && (mailbox.send(m) == RunStatus.Okay)

    /** Build a feed message around its own reply slot, register it for gone-completion, and deliver it. */
    private[loom] def feedDeliver[B](make: Reply[B] => M): Fed[B] =
      val f = new Feeding[M, B](this)
      f.msg = make(Reply.of(f))
      pending.add(f) __ Unit
      if !deliver(f) then f.answer(Alt(Err(s"feed to $name failed: it is gone")))
      Fed.of(f)

    private[loom] def settle(f: Feeding[?, ?]): Unit = pending.remove(f) __ Unit

    /** Mark a running feed cancelled and interrupt its handler, if it is still the one being handled; under the
      * monitor, so the interrupt cannot reach the next message. */
    private[loom] def cancelRunning(f: Feeding[M, ?]): Unit = synchronized:
      if Feeding.phaseH.compareAndSet(f, Feeding.Running, Feeding.Cancelling) && (current eq f) then
        val t = thread
        if t ne null then t.interrupt()

    private[loom] def offerPriority(m: M): Boolean =
      val box = m.asInstanceOf[AnyRef]
      live && (box ne null) && Muncher.urgentH.compareAndSet(this, null: AnyRef, box) && {
        mailbox.trySend(Wake) __ Unit   // wakes a muncher parked on an empty mailbox; a busy one looks before its next
        true
      }

    private[loom] def clear(): Int =
      var n = 0
      var more = true
      while more do
        mailbox.tryRecv().fold{
          case Wake => ()
          case f: Feeding[?, ?] => if f.withdraw() then n += 1
          case _ => n += 1
        }{ _ => more = false }
      n

    private[loom] def start(): Unit =
      val t = Thread.ofVirtual().name(name).unstarted(() =>
        try threadnice{ run() }.foreachAlt(sup.record)
        catch case e: InterruptedException => sup.record(Err(e))   // the thread ends here; nothing above to hand it to
      )
      thread = t
      t.start()

    /** Graceful end: stop accepting, drain what's buffered, then tear down. */
    private[loom] def stopGraceful(): Unit = { live = false; mailbox.close() __ Unit }
    /** Hard end: fail the mailbox and interrupt, so a blocking handler unwinds. */
    private[loom] def stopHard(e: Err): Unit =
      live = false
      hardStop = Is(e)
      mailbox.fail(e) __ Unit
      val t = thread
      if t ne null then t.interrupt()
    /** Waits for the thread to end; under a hard stop the wait nags (see [[Join]]). */
    private[loom] def join(nag: Boolean): Unit =
      val t = thread
      if t ne null then Join(t, Join.nagNanos)(nag)

    // Each defer runs in isolation (see `Unwind`), so one that throws, or is cut by an interrupt, does not
    // stop the rest.  An interruption under a hard stop is the stop, already on record; any other is a failure.
    private def runDefers(errs: List[Err]): Unit =
      val ds = defers
      defers = Nil
      val unwind = new Unwind
      ds.foreach{ d =>
        val t = unwind(d(errs))
        if (t ne null) && !(t.isInstanceOf[InterruptedException] && hardStop.isIs) then sup.record(Err(t))
      }
      unwind.restore()

    private def build(): Behavior[M] =
      defers = Nil
      factory(using this)

    // `teardown` ALWAYS runs (finally) — even if a blocking handler is interrupted on hard-stop —
    // so outstanding feeds are failed and the address is removed; nothing hangs or leaks.
    private def run(): Unit =
      try loop()
      finally teardown()

    /** Begin a feed if it is still queued, making it the one a cancellation may interrupt. */
    private def begin(f: Feeding[M, ?]): Boolean = synchronized:
      Feeding.phaseH.compareAndSet(f, Feeding.Queued, Feeding.Running) && { current = f; true }

    /** End the current feed; true if it was cancelled while it ran.  An interrupt aimed at it is cleared here, under
      * the monitor, so none survives into the next message; a hard stop's is kept. */
    private def finish(f: Feeding[M, ?]): Boolean = synchronized:
      current = null
      val was = Feeding.phaseH.getAndSet(f, Feeding.Done).asInstanceOf[Int]
      if was == Feeding.Cancelling && hardStop.isAlt then Thread.interrupted() __ Unit
      was == Feeding.Cancelling

    private def loop(): Unit =
      var restarts = 0
      var behavior = build()
      var looping = true
      def handle(m: M, f: Feeding[M, ?] | Null): Unit =
        var interrupted: InterruptedException = null
        val outcome: Ask[Unit] =
          try Go.attempt(behavior(m))
          catch case e: InterruptedException => { interrupted = e; Is.unit }
        val cancelled = (f ne null) && finish(f)
        if hardStop.isIs && ((interrupted ne null) || outcome.isAlt) then
          // Aborted: its cause is what gets recorded, and the muncher ends; there is nothing to retry.
          sup.record(hardStop.getOrElse(_ => Err("aborted")))
          looping = false
        else if cancelled then ()   // not wanted any more, which is not a fault: the muncher goes on
        else if interrupted ne null then
          // Interrupted by an outsider: this muncher ends.  The decider is not asked: there is nothing to retry.
          sup.record(Err(interrupted))
          looping = false
        else outcome.foreachAlt: e =>
          runDefers(e :: Nil)                            // this incarnation's cleanup sees the error
          decider(e, restarts) match
            case Directive.Stop     => sup.record(e); looping = false
            case Directive.Escalate => sup.record(e); looping = false; sup.escalate()
            case Directive.Restart  => restarts += 1; behavior = build()
      def takePriority(): Unit =
        val u = Muncher.urgentH.getAndSet(this, null: AnyRef).asInstanceOf[AnyRef]
        if u ne null then handle(u.asInstanceOf[M], null)
      while looping do
        val waiting = Muncher.urgentH.getVolatile(this).asInstanceOf[AnyRef]
        if waiting ne null then takePriority()
        else mailbox.recv().fold{
          case Wake => ()                                // a priority message arrived; the top of the loop takes it
          case f: Feeding[?, ?] =>
            val g = f.asInstanceOf[Feeding[M, ?]]
            if begin(g) then handle(g.msg, g)            // one withdrawn while queued is passed over
          case m => handle(m.asInstanceOf[M], null)
        }{ st =>
          st match
            case RunStatus.Fail(e) => sup.record(e)      // hard-stopped
            case _                 => takePriority()     // closed and drained: a priority message that came last still goes
          looping = false
        }

    private def teardown(): Unit =
      live = false
      val u = Muncher.urgentH.getAndSet(this, null: AnyRef).asInstanceOf[AnyRef]
      if u ne null then sup.deadLetter(u)                // a priority message the muncher ended without taking
      runDefers(Nil)                                     // ended okay (a failure path already ran + cleared)
      pending.forEach(cf => cf.asInstanceOf[CompletableFuture[Ask[Any]]].complete(Alt(Err(s"$name retired"))) __ Unit)
      pending.clear()
      removeSelf(this)
      sup.deregister(this)
  }


  private[loom] object Muncher {
    import java.lang.invoke.MethodHandles
    import scala.annotation.static
    @static private[loom] final val urgentH =
      MethodHandles.privateLookupIn(classOf[Muncher[?]], MethodHandles.lookup()).findVarHandle(classOf[Muncher[?]], "urgent", classOf[AnyRef])
  }


  // === A keyed address book of munchers ===

  final class Registry[K, M] private[loom] (name: String, sup: Supervisor, decider: Decider, capacity: Int) {
    private val map = new ConcurrentHashMap[K, Muncher[M]]()

    private[loom] def resolve(k: K): Muncher[M] | Null =
      val mu = map.get(k)
      if mu eq null then null
      else if mu.isLive then mu
      else { map.remove(k, mu) __ Unit; null }

    /** A handle to the muncher at `k` (resolved lazily on each use; tell drops / feed fails if gone). */
    def apply(k: K): Ref[M] = new Ref(() => resolve(k), m => sup.deadLetter(m))

    /** The muncher at `k` if one is live — for explicit presence handling (`fold`/`getOrElse`). */
    def get(k: K): Option[Ref[M]] = if resolve(k) ne null then Some(apply(k)) else None

    /** Spawn the muncher for `k` (or return the existing live one); replaces a dead one.  The body
      * is a context function, so `Defer`/`Stop` and [[Munch.self]] work inside it directly. */
    def spawn(k: K)(factory: Context[M] ?=> Behavior[M]): Ref[M] = getOrSpawn(k)(factory)

    /** Get the live muncher for `k`, or spawn it on demand — the lazy keyed-actor idiom. */
    def getOrSpawn(k: K)(factory: Context[M] ?=> Behavior[M]): Ref[M] =
      var made: Muncher[M] = null
      map.compute(k, (_, cur) =>
        if (cur ne null) && cur.isLive then cur
        else
          val mu = new Muncher[M](s"$name[$k]", capacity, factory, decider, sup, m => map.remove(k, m) __ Unit)
          made = mu
          mu
      ) __ Unit
      if made ne null then { sup.register(made); made.start() }
      apply(k)

    /** Live munchers right now (a snapshot; may be stale immediately). */
    def keys: Set[K] =
      import scala.jdk.CollectionConverters._
      map.entrySet().asScala.collect{ case e if e.getValue.isLive => e.getKey }.toSet
  }


  // === The supervisor: owns the fleet, isolates failures, collects errors ===

  final class Supervisor private[loom] (report: Err => Unit, onDeadLetter: Any => Unit) {
    private val munchers = ConcurrentHashMap.newKeySet[Muncher[?]]()
    private val errs = Atom(Nil: List[Err])
    private val stopped = Atom(false)
    private val done = new CompletableFuture[Ask[Unit]]()

    private[loom] def register(m: Muncher[?]): Unit = munchers.add(m) __ Unit
    private[loom] def deregister(m: Muncher[?]): Unit = munchers.remove(m) __ Unit
    private[loom] def record(e: Err): Unit = { errs.zap(e :: _); try report(e) catch case t if t.threadCatchable => () }
    private[loom] def deadLetter(m: Any): Unit = try onDeadLetter(m) catch case t if t.threadCatchable => ()
    private[loom] def escalate(): Unit = stop() __ Unit

    /** Open a keyed registry of munchers under this supervisor. */
    def registry[K, M](name: String = "munch", onError: Decider = Supervise.stop, capacity: Int = 1024): Registry[K, M] =
      new Registry[K, M](name, this, onError, capacity)

    /** All errors recorded so far (newest first). */
    def errors: List[Err] = errs()

    private def aggregate(): Ask[Unit] = errs() match
      case Nil      => Is.unit
      case e :: Nil => Alt(e)
      case many     => Alt(Err(ErrType.Many(many.reverse)))

    /** Gracefully stop every muncher (drain mailboxes, run cleanups), then report the outcome. */
    def stop(): Ask[Unit] =
      if !stopped.swap(true) then
        val snap = snapshot()
        snap.foreach(_.stopGraceful())
        snap.foreach(_.join(nag = false))
        done.complete(aggregate()) __ Unit
      await()

    /** Hard-stop: fail mailboxes and interrupt blocking handlers. */
    def cancel(): Ask[Unit] =
      if !stopped.swap(true) then
        val snap = snapshot()
        snap.foreach(_.stopHard(Err("supervisor cancelled")))
        snap.foreach(_.join(nag = true))
        done.complete(aggregate()) __ Unit
      await()

    /** Block until the supervisor has been stopped and every muncher has torn down. */
    def await(): Ask[Unit] =
      try done.get()
      catch case e if e.catchable => Alt(Err(e))

    // Sized by the copy, not up front: a muncher retiring between a size read and the copy would leave
    // a null at the end of a pre-sized array.
    private def snapshot(): Array[Muncher[?]] =
      munchers.toArray(new Array[Muncher[?]](0))
  }

  /** Open a supervisor.  `report` sees each recorded error live (logging); `onDeadLetter` sees
    * messages dropped to a gone target. */
  def supervisor(report: Err => Unit = _ => (), onDeadLetter: Any => Unit = _ => ()): Supervisor =
    new Supervisor(report, onDeadLetter)
}
