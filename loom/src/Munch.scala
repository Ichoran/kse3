// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025-26 Rex Kerr.

package kse.loom

import java.util.concurrent.{CompletableFuture, ConcurrentHashMap}

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


  /** A typed, write-once reply slot carried inside a `feed` message.  The handler completes it with
    * `reply(value)` (or `reply := value`); the caller observes the result through its `Fu[B]`.  If
    * the muncher dies first, the runtime completes it with an error, so the caller never hangs. */
  final class Reply[B] private[loom] (private val cf: CompletableFuture[Ask[B]], private val onDone: () => Unit) {
    /** Answer the feed. */
    def apply(value: B): Unit = if cf.complete(Is(value)) then onDone()
    /** Answer the feed (alias for `apply`). */
    inline def :=(value: B): Unit = apply(value)
    /** Answer the feed with an error (surfaces via the caller's `.?`). */
    def fail(err: Err): Unit = if cf.complete(Alt(err)) then onDone()
    private[loom] def gone(err: Err): Unit = cf.complete(Alt(err)) __ Unit
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
  }

  /** This muncher's own address, from inside its behavior (when you need to hand it out or self-send). */
  def self[M](using ctx: Context[M]): Ref[M] = ctx.self


  /** A handle to a muncher addressed by key.  Always resolvable to *something*: if the target is
    * absent or retired, a tell drops (to the supervisor's dead-letter sink) and a feed fails with a
    * clean "gone" error rather than hanging. */
  final class Ref[M] private[loom] (resolve: () => Muncher[M] | Null, onDrop: M => Unit) {
    /** Fire-and-forget.  Delivered if the target is live; otherwise dropped (dead-lettered). */
    infix def !(m: M): Unit =
      val mu = resolve()
      if (mu eq null) || !mu.deliver(m) then onDrop(m)

    /** Feed a request and get a typed `Fu[B]` reply.  `make` builds the message from the reply
      * slot, e.g. `ref.feed(Msg.Stats(_))`.  A gone target yields a failed `Fu` (observe via `.?`). */
    def feed[B](make: Reply[B] => M): Fu[B] =
      val mu = resolve()
      if mu eq null then Fu.of[B](Alt(Err("feed to a muncher that is gone")))
      else mu.feedDeliver(make)

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
    private val mailbox = Chan[M](capacity)
    @volatile private var live = true
    private val pending = ConcurrentHashMap.newKeySet[CompletableFuture[?]]()
    private var defers: List[List[Err] => Unit] = Nil
    private var thread: Thread = null

    def isLive: Boolean = live

    val self: Ref[M] = new Ref(() => if live then this else null, m => sup.deadLetter(m))

    private[loom] def stopSelf(): Unit = mailbox.close() __ Unit
    private[loom] def addDefer(body: List[Err] => Unit): Unit = defers = body :: defers

    /** Try to enqueue a message; false if the muncher is gone or its mailbox has closed. */
    private[loom] def deliver(m: M): Boolean =
      live && (mailbox.send(m) == RunStatus.Okay)

    /** Build a feed message, register its reply for gone-completion, and deliver it. */
    private[loom] def feedDeliver[B](make: Reply[B] => M): Fu[B] =
      val cf = new CompletableFuture[Ask[B]]()
      pending.add(cf) __ Unit
      val rep = new Reply[B](cf, () => pending.remove(cf) __ Unit)
      val m = make(rep)
      if !deliver(m) then
        cf.complete(Alt(Err(s"feed to $name failed: it is gone"))) __ Unit
        pending.remove(cf) __ Unit
      Fu.wrap(cf)

    private[loom] def start(): Unit =
      val t = Thread.ofVirtual().name(name).unstarted(() => threadnice{ run() }.foreachAlt(sup.record))
      thread = t
      t.start()

    /** Graceful end: stop accepting, drain what's buffered, then tear down. */
    private[loom] def stopGraceful(): Unit = { live = false; mailbox.close() __ Unit }
    /** Hard end: fail the mailbox and interrupt, so a blocking handler unwinds. */
    private[loom] def stopHard(e: Err): Unit =
      live = false
      mailbox.fail(e) __ Unit
      val t = thread
      if t ne null then t.interrupt()
    private[loom] def join(): Unit =
      val t = thread
      if t ne null then
        var joined = false
        while !joined do
          try { t.join(); joined = true }
          catch case _: InterruptedException => Thread.currentThread().interrupt()

    private def runDefers(errs: List[Err]): Unit =
      val ds = defers
      defers = Nil
      ds.foreach(d => try d(errs) catch case e if e.threadCatchable => sup.record(Err(e)))

    private def build(): Behavior[M] =
      defers = Nil
      factory(using this)

    // `teardown` ALWAYS runs (finally) — even if a blocking handler is interrupted on hard-stop —
    // so outstanding feeds are failed and the address is removed; nothing hangs or leaks.
    private def run(): Unit =
      try loop()
      finally teardown()

    private def loop(): Unit =
      var restarts = 0
      var behavior = build()
      var looping = true
      while looping do
        mailbox.recv().fold{ m =>
          Go.attempt(behavior(m)).foreachAlt: e =>
            runDefers(e :: Nil)                          // this incarnation's cleanup sees the error
            decider(e, restarts) match
              case Directive.Stop     => sup.record(e); looping = false
              case Directive.Escalate => sup.record(e); looping = false; sup.escalate()
              case Directive.Restart  => restarts += 1; behavior = build()
        }{ st =>
          st match
            case RunStatus.Fail(e) => sup.record(e)      // hard-stopped
            case _                 => ()                 // closed/drained — normal retire
          looping = false
        }

    private def teardown(): Unit =
      live = false
      runDefers(Nil)                                     // ended okay (a failure path already ran + cleared)
      pending.forEach(cf => cf.asInstanceOf[CompletableFuture[Ask[Any]]].complete(Alt(Err(s"$name retired"))) __ Unit)
      pending.clear()
      removeSelf(this)
      sup.deregister(this)
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
        snap.foreach(_.join())
        done.complete(aggregate()) __ Unit
      await()

    /** Hard-stop: fail mailboxes and interrupt blocking handlers. */
    def cancel(): Ask[Unit] =
      if !stopped.swap(true) then
        val snap = snapshot()
        snap.foreach(_.stopHard(Err("supervisor cancelled")))
        snap.foreach(_.join())
        done.complete(aggregate()) __ Unit
      await()

    /** Block until the supervisor has been stopped and every muncher has torn down. */
    def await(): Ask[Unit] =
      try done.get()
      catch case e if e.catchable => Alt(Err(e))

    private def snapshot(): Array[Muncher[?]] =
      munchers.toArray(new Array[Muncher[?]](munchers.size))
  }

  /** Open a supervisor.  `report` sees each recorded error live (logging); `onDeadLetter` sees
    * messages dropped to a gone target. */
  def supervisor(report: Err => Unit = _ => (), onDeadLetter: Any => Unit = _ => ()): Supervisor =
    new Supervisor(report, onDeadLetter)
}
