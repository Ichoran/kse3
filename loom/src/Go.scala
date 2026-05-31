// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025 Rex Kerr.

package kse.loom

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.locks.LockSupport

import scala.util.boundary

import kse.basics._
import kse.flow._

import Chan.Woe


/** A persistent structured-concurrency scope running on a virtual thread.
  *
  * `Go.session { ... }` opens a coordination scope and returns a handle.  Inside it you spawn
  * tasks with `Go { ... }` (the managing session is the contextual receiver), wire channels
  * together, and let the scope tear itself down when the work is finished.
  *
  * The block you pass to a task runs **once** as an initializer: it registers the channel
  * operations the task cares about — `put` (produce), `get` (consume), `into` (transfer) —
  * along with any `Stop.on` conditions and `Defer` cleanups.  After registration the task
  * enters a **select loop** that services its registrations over and over, with no
  * per-iteration setup, until it terminates.  A task that registers one channel op is a plain
  * loop; a task that registers several is, by construction, a select over them.
  *
  * Because every registered lambda runs sequentially on the task's single carrier thread, any
  * `var` the initializer closes over is safe to read and write without synchronization — only
  * state visible to *other* tasks needs care.
  *
  * A task terminates when:
  *  - it has no registration that can ever fire again (every channel it reads is complete and
  *    every channel it writes is closed) — the implicit, cascading case; or
  *  - one of its `Stop.on` conditions becomes true, or it calls `Stop()` (graceful: any value
  *    already produced is flushed first); or
  *  - the scope is cancelled (a sibling failed, or someone called `cancel()`).
  *
  * **Establishment barrier.**  Data may start flowing the instant a task's initializer
  * returns, but no task may *finish* (auto-quit or drain-to-stop) until every initializer in
  * the tree — the session body and all task bodies — has run.  Until then everyone is still
  * being wired up, so it is not yet knowable who is truly done.  (Hard cancellation bypasses
  * this; a failing tree tears down immediately.)
  */
final class Go private (private val parent: Go | Null, coordIn: Go.Coord | Null) {
  import Go._

  private[loom] val coord: Coord = if coordIn eq null then new Coord else coordIn

  private val result = new CompletableFuture[Unit Or Err]()
  private val handlers = new java.util.ArrayList[Handler](4)
  private var hs: Array[Handler] = null   // handlers frozen into a bare array once the loop starts
  private val children = new java.util.ArrayList[Go](2)
  private val writingTo =
    java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Chan[?], java.lang.Boolean]())
  private var stopConds: java.util.ArrayList[() => Boolean] = null   // Stop.on predicates
  private var deferred: java.util.ArrayList[() => Unit] = null       // Defer cleanups (LIFO)

  @volatile private var myThread: Thread = null
  @volatile private var myParker: Parker = null
  @volatile private[loom] var stopRequested = false
  private var inInit = true
  private var handlerIndex = 0
  private var errorField: Err Or Unit = Alt.unit       // Is(err) once this scope has failed

  private[loom] def thread: Thread = myThread
  private[loom] def failure: Err Or Unit = errorField


  // === Public handle API ===

  /** True once this scope (and all its descendants) have fully completed. */
  def isComplete: Boolean = result.isDone

  /** Block until the whole scope tree finishes, yielding success or the first error. */
  def await(): Unit Or Err =
    try result.get()
    catch case e if e.catchable => Alt(Err(e))

  /** Alias for `await()`. */
  def ask(): Unit Or Err = await()

  /** Like `await()`, but usable as `go.?` inside an `Or`/`Ask` boundary. */
  inline def ?[L >: Alt[Err]](using lb: boundary.Label[L]): Unit =
    kse.flow.?(await())(using lb)

  /** Request a graceful stop of the whole scope tree (each task exits after flushing any value
    * it has already produced).  Completes successfully — use `cancel()` if you also need to
    * interrupt handlers stuck in blocking calls we don't control. */
  def stop(): Unit = coord.stopAll()

  /** Cancel the entire scope tree: cooperatively (stop flag + unpark) and then hard
    * (interrupt), so handlers stuck in blocking calls we don't control also unwind. */
  def cancel(): Unit = coord.cancel(Err("cancelled"))

  /** Spawn a child task managed by *this* scope.  The child runs to completion and is joined
    * when this scope finishes, so its errors propagate here and channels it writes to
    * auto-close as part of the cascade.  Usually written `Go { ... }` (this scope is the
    * contextual manager).  Spawn from an initializer. */
  def go(body: Go ?=> Unit): Unit =
    val child = new Go(this, coord)
    children.add(child) __ Unit
    coord.pendingInit.incrementAndGet() __ Unit
    child.launch(body)


  // === Registration (init phase only) ===

  private[loom] def addHandler(h: Handler): Unit =
    if !inInit then throw new IllegalStateException("Channel operations can only be registered during a task's initializer")
    handlers.add(h) __ Unit

  private[loom] def trackWriter(chan: Chan[?]): Unit =
    if writingTo.add(chan) then chan.registerWriter()

  private[loom] def addStopCond(cond: () => Boolean): Unit =
    if !inInit then throw new IllegalStateException("Stop.on can only be registered during a task's initializer")
    if stopConds eq null then stopConds = new java.util.ArrayList[() => Boolean](2)
    stopConds.add(cond) __ Unit

  private[loom] def addDefer(body: () => Unit): Unit =
    if !inInit then throw new IllegalStateException("Defer can only be registered during a task's initializer")
    if deferred eq null then deferred = new java.util.ArrayList[() => Unit](2)
    deferred.add(body) __ Unit

  /** Mark this task for a graceful stop from within one of its own handlers. */
  private[loom] def stopSelf(): Unit = stopRequested = true


  // === Cooperative cancellation hook ===

  private[loom] def requestStop(): Unit =
    stopRequested = true
    val t = myThread
    if t ne null then LockSupport.unpark(t)


  // === Lifecycle ===

  /** Create the (unstarted) virtual thread, publish thread + parker synchronously so the
    * parent can join us and the coordinator can cancel us, then start running. */
  private def launch(body: Go ?=> Unit): Unit =
    val t = Thread.ofVirtual().unstarted(() => runScope(body))
    myThread = t
    myParker = new Parker(t)
    coord.register(this)
    t.start()

  private def runScope(body: Go ?=> Unit): Unit =
    try
      // --- init phase ---
      try body(using this)
      catch
        case e: InterruptedException =>
          Thread.currentThread().interrupt()
          fail(Err(e))
        case e if e.catchable =>
          fail(Err(e))
      inInit = false

      // --- announce that this scope is established; release the barrier once all are ---
      if coord.pendingInit.decrementAndGet() == 0 then coord.releaseInit()

      // --- run phase (skipped if we already failed or are being cancelled).  The loop may run
      //     before the barrier opens; it just can't *finish* until then. ---
      if errorField.isAlt && coord.failure.get().isAlt && !stopRequested then
        runLoop()
    catch
      case e: InterruptedException =>
        Thread.currentThread().interrupt()
        if errorField.isAlt then errorField = Is(Err(e))
      case e if e.catchable =>
        if errorField.isAlt then errorField = Is(Err(e))
    finally
      cleanup()

  /** Record an error for this scope (first one wins) and cancel the whole tree. */
  private def fail(err: Err): Unit =
    if errorField.isAlt then errorField = Is(err)
    coord.cancel(err)

  /** Are we still producing/consuming new work, or stopping (explicit stop, or a Stop.on
    * condition has fired)?  When not producing, handlers only flush what they already hold. */
  private def producing(): Boolean =
    if stopRequested then false
    else if stopConds eq null then true
    else
      var k = 0
      while k < stopConds.size do
        if (try stopConds.get(k)() catch case e if e.catchable => false) then return false
        k += 1
      true

  private def runLoop(): Unit =
    val p = myParker
    // Freeze the handlers into a bare array — the loop hits it hard, and it never changes now.
    hs = handlers.toArray(new Array[Handler](handlers.size))
    var i = 0
    while i < hs.length do { hs(i).register(p); i += 1 }

    var running = true
    while running do
      if coord.failure.get().isIs || Thread.currentThread().isInterrupted then
        running = false
      else
        tryExecuteOne(producing()) match
          case Outcome.Executed  => ()                                 // made progress; keep going
          case Outcome.Failed(e) => fail(e); running = false
          case Outcome.Idle      =>
            // Arm *before* the re-scan, so a producer/consumer that fires now sees us armed.
            arm(p)
            if coord.failure.get().isIs then
              disarm(p)
              running = false
            else
              val prod = producing()
              tryExecuteOne(prod) match
                case Outcome.Executed  => disarm(p)
                case Outcome.Failed(e) => disarm(p); fail(e); running = false
                case Outcome.Idle      =>
                  // We may only *finish* once every initializer in the tree has run.
                  if coord.initGate.isDone && finished(prod) then
                    disarm(p)
                    running = false
                  else
                    LockSupport.parkNanos(Chan.parkCapNanos)
                    disarm(p)

    i = 0
    while i < hs.length do { hs(i).unregister(p); i += 1 }

  /** Idle with nothing to do: are we actually done?  While producing, done means no handler
    * can ever fire again.  While self-stopping (one of our own `Stop.on`/`Stop()` fired) peers
    * are still live, so we wait to flush every value we hold; under a tree-wide stop nobody is
    * listening, so we exit promptly and abandon anything we couldn't deliver. */
  private def finished(prod: Boolean): Boolean =
    if prod then !scopeAlive()
    else coord.stopping || !anyPending()

  /** Mark this scope as (about to be) parked: bump each touched channel's armed count and set
    * the parker flag, both *before* the readiness re-scan so wakeups can't be lost. */
  private def arm(p: Parker): Unit =
    var i = 0
    while i < hs.length do { hs(i).arm(); i += 1 }
    p.armed = true

  private def disarm(p: Parker): Unit =
    p.armed = false
    var i = 0
    while i < hs.length do { hs(i).disarm(); i += 1 }

  /** Try each handler once, round-robin, executing the first that is ready. */
  private def tryExecuteOne(prod: Boolean): Outcome =
    val n = hs.length
    var k = 0
    while k < n do
      val idx = { val t = handlerIndex + k; if t >= n then t - n else t }
      hs(idx).tryRun(prod) match
        case Tried.Executed =>
          handlerIndex = { val t = idx + 1; if t >= n then 0 else t }
          return Outcome.Executed
        case Tried.Failed(e) => return Outcome.Failed(e)
        case Tried.NotReady  => ()
        case Tried.Inactive  => ()
      k += 1
    Outcome.Idle

  /** Could any handler ever fire again? */
  private def scopeAlive(): Boolean =
    var k = 0
    while k < hs.length do
      if hs(k).alive then return true
      k += 1
    false

  /** Is any handler holding a produced-but-not-yet-delivered value? */
  private def anyPending(): Boolean =
    var k = 0
    while k < hs.length do
      if hs(k).hasPending then return true
      k += 1
    false

  private def cleanup(): Unit =
    // 1. Tell every channel we write to that one writer is gone (cascade-closes channels).
    writingTo.forEach(c => c.writerDone())

    // 2. Join children (tolerating interruption, since they're being torn down too) and
    //    collect their errors.
    var childErrs: java.util.ArrayList[Err] = null
    var j = 0
    while j < children.size do
      val c = children.get(j)
      val ct = c.thread
      if ct ne null then
        var joined = false
        while !joined do
          try { ct.join(); joined = true }
          catch case _: InterruptedException => Thread.currentThread().interrupt()
      c.failure.foreach { e =>
        if childErrs eq null then childErrs = new java.util.ArrayList[Err](2)
        childErrs.add(e) __ Unit
      }
      j += 1

    if errorField.isAlt && (childErrs ne null) && !childErrs.isEmpty then
      errorField = Is(
        if childErrs.size == 1 then childErrs.get(0)
        else
          import scala.jdk.CollectionConverters._
          Err(ErrType.Many(childErrs.asScala.toSeq))
      )

    // 3. Run deferred cleanups, last-registered first.  Always runs (success or failure); a
    //    throwing defer records an error if we don't already have one.
    if deferred ne null then
      var k = deferred.size - 1
      while k >= 0 do
        try deferred.get(k).apply()
        catch case e if e.catchable => if errorField.isAlt then errorField = Is(Err(e))
        k -= 1

    // 4. Publish our result.  Always runs, so `await()` can never hang.  A scope with no error
    //    of its own still reports the tree-wide cause if it was cancelled/failed elsewhere
    //    (but a graceful `stop()` leaves the cause unset, so it reports success).
    val effective: Err Or Unit = if errorField.isIs then errorField else coord.failure.get()
    val out: Unit Or Err = effective.fold(e => Alt(e))(_ => Is.unit)
    result.complete(out) __ Unit
}


object Go {
  /** The label a task body may `.?`-break to: the loop runs each handler inside `attempt`, so
    * an `Alt[Err]` (or a thrown exception) fails the task — and thus, by cancellation, the whole
    * scope tree.  The break is a stackless exception, so it's for error handling, not hot loops. */
  type CanFail[Z] = boundary.Label[Ask[Z]]

  /** Run a handler body that may `.?`-break with an error, capturing the outcome as an `Ask`. */
  private[loom] inline def attempt[Z](inline body: CanFail[Z] ?=> Z): Ask[Z] =
    try boundary[Ask[Z]]{ label ?=> Is(body(using label)) }
    catch case t if t.catchable => Alt(Err(t))

  /** Open a coordination scope and return a handle you can `await()`.  Spawn tasks inside it
    * with `Go { ... }` (this session is their contextual manager). */
  def session(body: Go ?=> Unit): Go =
    val go = new Go(null, null)
    go.launch(body)
    go

  /** Spawn a task in the enclosing session.  Written `Go { ... }`; the managing session is the
    * contextual `Go`, so this only compiles inside a `Go.session` (or another task). */
  def apply(body: Go ?=> Unit)(using parent: Go): Unit =
    parent.go(body)


  // === Coordination shared by an entire scope tree ===

  private[loom] final class Coord {
    val pendingInit = new AtomicInteger(1)              // starts at 1 for the session root
    val initGate = new CompletableFuture[Unit]()
    val failure = new AtomicReference[Err Or Unit](Alt.unit)  // Is(err) once any scope fails
    @volatile var stopping = false                            // true once a tree-wide stop/cancel began
    private val gos = new ConcurrentLinkedQueue[Go]()

    def register(go: Go): Unit = gos.add(go) __ Unit

    /** Every initializer has run: open the barrier and nudge anyone parked waiting to finish. */
    def releaseInit(): Unit =
      initGate.complete(()) __ Unit
      val it = gos.iterator()
      while it.hasNext do
        val t = it.next().thread
        if t ne null then LockSupport.unpark(t)

    /** Cooperatively stop the whole tree (no error): release the barrier and ask every scope
      * to exit after flushing what it holds. */
    def stopAll(): Unit =
      stopping = true
      initGate.complete(()) __ Unit
      val it = gos.iterator()
      while it.hasNext do it.next().requestStop()

    /** Cancel the whole tree: record the (first) cause, release the barrier, then cooperatively
      * stop + hard-interrupt every scope so all parks/blocking calls unwind. */
    def cancel(cause: Err): Unit =
      stopping = true
      failure.compareAndSet(Alt.unit, Is(cause)) __ Unit
      initGate.complete(()) __ Unit
      val it = gos.iterator()
      while it.hasNext do
        val g = it.next()
        g.requestStop()
        val t = g.thread
        if t ne null then t.interrupt()
  }


  // === Select-loop handlers ===

  private enum Outcome:
    case Executed
    case Idle
    case Failed(err: Err)

  private[loom] enum Tried:
    case Executed
    case NotReady
    case Inactive
    case Failed(e: Err)

  private[loom] sealed trait Handler {
    /** Could this handler still fire at some point in the future? */
    def alive: Boolean
    /** Is this handler holding a produced-but-not-yet-delivered value? */
    def hasPending: Boolean
    /** Try to make progress.  `producing` is false once the task is stopping, in which case the
      * handler only flushes a value it already holds (it neither produces nor consumes anew). */
    def tryRun(producing: Boolean): Tried
    def register(p: Parker): Unit
    def unregister(p: Parker): Unit
    /** Bump / drop this channel's armed count when the scope parks / wakes. */
    def arm(): Unit
    def disarm(): Unit
  }

  private[loom] final class RecvHandler[A](chan: Chan[A], f: A => Ask[Unit]) extends Handler {
    def alive: Boolean = !chan.isComplete               // more data (or an error) may still arrive
    def hasPending: Boolean = false
    def register(p: Parker): Unit = chan.addRecvWaiter(p)
    def unregister(p: Parker): Unit = chan.removeRecvWaiter(p)
    def arm(): Unit = chan.recvArm()
    def disarm(): Unit = chan.recvDisarm()
    def tryRun(producing: Boolean): Tried =
      if !producing then return Tried.Inactive          // stopping: don't consume anything new
      chan.tryRecv().fold{ v =>
        f(v).fold(_ => Tried.Executed)(e => Tried.Failed(e))
      }{
        case Woe.Wait    => Tried.NotReady
        case Woe.Done    => Tried.Inactive
        case Woe.Fail(e) => Tried.Failed(e)
      }
  }

  private[loom] final class SendHandler[A](chan: Chan[A], cond: () => Boolean, producer: () => Ask[A]) extends Handler {
    // A produced-but-not-yet-delivered value is cached so contention never drops it.
    private var pendingFlag = false
    private var pending: A = null.asInstanceOf[A]

    // Live while a value is buffered to deliver, or the channel is open and `cond` may yet
    // be true.  `cond` is a producer's per-handler termination signal, so liveness consults it.
    def alive: Boolean =
      pendingFlag || (chan.isOpen && { try cond() catch case e if e.catchable => true })
    def hasPending: Boolean = pendingFlag
    def register(p: Parker): Unit = chan.addSendWaiter(p)
    def unregister(p: Parker): Unit = chan.removeSendWaiter(p)
    def arm(): Unit = chan.sendArm()
    def disarm(): Unit = chan.sendDisarm()
    def tryRun(producing: Boolean): Tried =
      if !pendingFlag then
        if !producing then return Tried.Inactive        // stopping: produce nothing new
        val c =
          try cond()
          catch case e if e.catchable => return Tried.Failed(Err(e))
        if !c then return Tried.Inactive
        producer().fold{ a => pending = a; pendingFlag = true }{ e => return Tried.Failed(e) }
      chan.trySend(pending).fold{ _ =>
        pendingFlag = false
        pending = null.asInstanceOf[A]
        Tried.Executed
      }{
        case Woe.Wait    => Tried.NotReady               // keep pending for next round
        case Woe.Done    => Tried.Inactive
        case Woe.Fail(e) => Tried.Failed(e)
      }
  }

  /** Reads `src`, transforms with `f`, writes `dst` — holding at most one in-flight item, so
    * backpressure is deterministic: it won't read the next input until the current output is
    * delivered, and it parks on a full `dst` rather than buffering. */
  private[loom] final class TransferHandler[A, B](src: Chan[A], dst: Chan[B], f: A => Ask[B]) extends Handler {
    private var pendingFlag = false
    private var pending: B = null.asInstanceOf[B]

    // Live while we can still deliver (dst open) and either hold a value or src may yet give one.
    def alive: Boolean = dst.isOpen && (pendingFlag || !src.isComplete)
    def hasPending: Boolean = pendingFlag
    def register(p: Parker): Unit = { src.addRecvWaiter(p); dst.addSendWaiter(p) }
    def unregister(p: Parker): Unit = { src.removeRecvWaiter(p); dst.removeSendWaiter(p) }
    def arm(): Unit = { src.recvArm(); dst.sendArm() }
    def disarm(): Unit = { src.recvDisarm(); dst.sendDisarm() }
    def tryRun(producing: Boolean): Tried =
      if !pendingFlag then
        if !producing then return Tried.Inactive         // stopping: read nothing new
        src.tryRecv().fold{ v =>
          f(v).fold{ b => pending = b; pendingFlag = true }{ e => return Tried.Failed(e) }
        }{
          case Woe.Wait    => return Tried.NotReady
          case Woe.Done    => return Tried.Inactive       // input exhausted
          case Woe.Fail(e) => return Tried.Failed(e)
        }
      dst.trySend(pending).fold{ _ =>
        pendingFlag = false
        pending = null.asInstanceOf[B]
        Tried.Executed
      }{
        case Woe.Wait    => Tried.NotReady                // dst full: hold the item, park
        case Woe.Done    => Tried.Inactive                // downstream gone
        case Woe.Fail(e) => Tried.Failed(e)
      }
  }
}


// === Coordination verbs available inside a task ===

/** Stop the current task.  `Stop()` stops it now (gracefully); `Stop.on(cond)` stops it once
  * `cond` holds; `Stop.session()` gracefully stops the whole scope tree.  In every case a value
  * already produced is flushed before the task tears down. */
object Stop {
  def apply()(using go: Go): Unit = go.stopSelf()
  def on(cond: => Boolean)(using go: Go): Unit = go.addStopCond(() => cond)
  def session()(using go: Go): Unit = go.coord.stopAll()
}

/** Register cleanup to run when the current task finishes (success, stop, or failure).  Like
  * Scala's `Using`/Go's `defer`, multiple `Defer`s run last-registered-first. */
object Defer {
  def apply(body: => Unit)(using go: Go): Unit = go.addDefer(() => body)
}


// === Channel operations available inside a task ===

extension [A](chan: Chan[A]) {
  /** Consume each value as it arrives (a terminal sink — to forward to another channel use
    * `into`).  When the channel completes this registration goes inactive; if that leaves the
    * task with nothing to do, the task terminates. */
  def get(f: A => (Go.CanFail[Unit] ?=> Unit))(using go: Go): Unit =
    go.addHandler(new Go.RecvHandler(chan, v => Go.attempt(f(v))))

  /** Produce a value whenever the channel has room.  Registers this task as a writer, so the
    * channel auto-closes once every writer has finished.  Termination is via `Stop.on`/`Stop()`
    * (or the channel being closed elsewhere); a value already produced is always delivered. */
  def put(produce: Go.CanFail[A] ?=> A)(using go: Go): Unit =
    go.trackWriter(chan)
    go.addHandler(new Go.SendHandler(chan, () => true, () => Go.attempt(produce)))

  /** Transfer values from this channel into `dst`, transforming with `f`, one in-flight item at
    * a time (deterministic backpressure).  Registers this task as a writer of `dst`. */
  def into[B](dst: Chan[B])(f: A => (Go.CanFail[B] ?=> B))(using go: Go): Unit =
    go.trackWriter(dst)
    go.addHandler(new Go.TransferHandler(chan, dst, v => Go.attempt(f(v))))

  // --- Lower-level declarative forms (used by the conditional/relay patterns) ---

  /** Produce values while `cond` holds and the channel is open.  Registers this task as a
    * writer, so the channel auto-closes once every such task has finished. */
  def onSendWhile(cond: => Boolean)(producer: => A)(using go: Go): Unit =
    go.trackWriter(chan)
    go.addHandler(new Go.SendHandler(chan, () => cond, () => { try Is(producer) catch case e if e.catchable => Alt(Err(e)) }))

  /** Send a single value, once. */
  def onSend(value: => A)(using go: Go): Unit =
    var sent = false
    onSendWhile(!sent)({ sent = true; value })

  /** Alias for `get`. */
  def onRecv(f: A => Unit)(using go: Go): Unit =
    go.addHandler(new Go.RecvHandler(chan, v => { try { f(v); Is.unit } catch case e if e.catchable => Alt(Err(e)) }))

  /** Declare that the current task writes to this channel via imperative `send` (rather than a
    * `put`/`into`/`onSend*` registration), so the channel still auto-closes when the task
    * finishes.  Use this for relay tasks that `recv` then `send` by hand. */
  def writing(using go: Go): Unit = go.trackWriter(chan)
}
