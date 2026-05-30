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
  * The block you pass to `Go { ... }` runs **once** as an initializer: it registers the
  * channel operations the scope cares about (`onRecv`, `onSend`, `onSendWhile`) and may
  * spawn nested `Go` blocks.  After every scope in the tree has finished initializing, the
  * scope enters a **select loop** that services its registered handlers — over and over,
  * with no per-iteration setup — until it terminates.
  *
  * A scope terminates when:
  *  - it has no handler that can ever fire again (every channel it reads is complete and
  *    every channel/condition it writes is closed/false) — the implicit, cascading case; or
  *  - it explicitly calls `stop()`; or
  *  - the scope is cancelled (a sibling failed, or someone called `cancel()`).
  *
  * A scope that registers no handlers simply runs its body once and completes, like a
  * `Future[Unit]`.
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
  def ask(): Unit Or Err =
    try result.get()
    catch case e if e.catchable => Alt(Err(e))

  /** Like `ask()`, but usable as `go.?` inside an `Or`/`Ask` boundary. */
  inline def ?[L >: Alt[Err]](using lb: boundary.Label[L]): Unit =
    kse.flow.?(ask())(using lb)

  /** Request a graceful stop of the whole scope tree (each scope exits after its current
    * handler).  Completes successfully — use `cancel()` if you also need to interrupt
    * handlers stuck in blocking calls we don't control. */
  def stop(): Unit = coord.stopAll()

  /** Cancel the entire scope tree: cooperatively (stop flag + unpark) and then hard
    * (interrupt), so handlers stuck in blocking calls we don't control also unwind. */
  def cancel(): Unit = coord.cancel(Err("cancelled"))

  /** Spawn a child scope managed by *this* scope.  The child runs to completion and is
    * joined when this scope finishes, so its errors propagate here and channels it writes
    * to auto-close as part of the cascade.  The manager is explicit — it's the receiver —
    * so there's no guessing about who owns the child.  Spawn from the initializer. */
  def go(body: Go ?=> Unit): Unit =
    val child = new Go(this, coord)
    children.add(child) __ Unit
    coord.pendingInit.incrementAndGet() __ Unit
    child.launch(body)


  // === Registration (init phase only) ===

  private[loom] def addHandler(h: Handler): Unit =
    if !inInit then throw new IllegalStateException("Channel handlers can only be registered during a Go block's initializer")
    handlers.add(h) __ Unit

  private[loom] def trackWriter(chan: Chan[?]): Unit =
    if writingTo.add(chan) then chan.registerWriter()


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

      // --- init barrier: wait until every scope in the tree has finished initializing ---
      if coord.pendingInit.decrementAndGet() == 0 then coord.initGate.complete(()) __ Unit
      try coord.initGate.get()
      catch case e if e.catchable => ()

      // --- run phase (skipped if we already failed or are being cancelled) ---
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

  private def runLoop(): Unit =
    val p = myParker
    // Freeze the handlers into a bare array — the loop hits it hard, and it never changes now.
    hs = handlers.toArray(new Array[Handler](handlers.size))
    // Register on every channel we touch so producers/consumers can wake us.
    var i = 0
    while i < hs.length do { hs(i).register(p); i += 1 }

    var running = true
    while running do
      if stopRequested || coord.failure.get().isIs || Thread.currentThread().isInterrupted then
        running = false
      else
        tryExecuteOne() match
          case Outcome.Executed  => ()                                 // made progress; keep going
          case Outcome.Failed(e) => fail(e); running = false
          case Outcome.Idle      =>
            // Arm *before* the re-scan, so a producer/consumer that fires now sees us armed.
            arm(p)
            if stopRequested || coord.failure.get().isIs then
              disarm(p)
              running = false
            else tryExecuteOne() match
              case Outcome.Executed  => disarm(p)
              case Outcome.Failed(e) => disarm(p); fail(e); running = false
              case Outcome.Idle      =>
                if !scopeAlive() then
                  disarm(p)
                  running = false                                      // nothing can ever fire again
                else
                  LockSupport.parkNanos(Chan.parkCapNanos)
                  disarm(p)

    i = 0
    while i < hs.length do { hs(i).unregister(p); i += 1 }

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
  private def tryExecuteOne(): Outcome =
    val n = hs.length
    var k = 0
    while k < n do
      val idx = { val t = handlerIndex + k; if t >= n then t - n else t }
      hs(idx).tryRun() match
        case Tried.Executed =>
          handlerIndex = { val t = idx + 1; if t >= n then 0 else t }
          return Outcome.Executed
        case Tried.Failed(e) => return Outcome.Failed(e)
        case Tried.NotReady  => ()
        case Tried.Inactive  => ()
      k += 1
    Outcome.Idle

  /** Could any handler ever fire again?  Live input (an open recv channel) can flip a send
    * condition or deliver a value; a send handler is live while it can still produce. */
  private def scopeAlive(): Boolean =
    var k = 0
    while k < hs.length do
      if hs(k).alive then return true
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

    // 3. Publish our result.  Always runs, so `ask()` can never hang.  A scope with no error
    //    of its own still reports the tree-wide cause if it was cancelled/failed elsewhere
    //    (but a graceful `stop()` leaves the cause unset, so it reports success).
    val effective: Err Or Unit = if errorField.isIs then errorField else coord.failure.get()
    val out: Unit Or Err = effective.fold(e => Alt(e))(_ => Is.unit)
    result.complete(out) __ Unit
}


object Go {
  /** Start a root scope and return a handle you can `ask()` for completion.  Spawn children
    * inside it by naming their manager explicitly: `Go: g ?=> g.go { ... }`. */
  def apply(body: Go ?=> Unit): Go =
    val go = new Go(null, null)
    go.launch(body)
    go


  // === Coordination shared by an entire scope tree ===

  private[loom] final class Coord {
    val pendingInit = new AtomicInteger(1)              // starts at 1 for the root
    val initGate = new CompletableFuture[Unit]()
    val failure = new AtomicReference[Err Or Unit](Alt.unit)  // Is(err) once any scope fails
    private val gos = new ConcurrentLinkedQueue[Go]()

    def register(go: Go): Unit = gos.add(go) __ Unit

    /** Cooperatively stop the whole tree (no error): release the init barrier and ask every
      * scope to exit after its current handler. */
    def stopAll(): Unit =
      initGate.complete(()) __ Unit
      val it = gos.iterator()
      while it.hasNext do it.next().requestStop()

    /** Cancel the whole tree: record the (first) cause, release the init barrier, then
      * cooperatively stop + hard-interrupt every scope so all parks/blocking calls unwind. */
    def cancel(cause: Err): Unit =
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
    def tryRun(): Tried
    def register(p: Parker): Unit
    def unregister(p: Parker): Unit
    /** Bump / drop this channel's armed count when the scope parks / wakes. */
    def arm(): Unit
    def disarm(): Unit
  }

  private[loom] final class RecvHandler[A](chan: Chan[A], f: A => Unit) extends Handler {
    def alive: Boolean = !chan.isComplete               // more data (or an error) may still arrive
    def register(p: Parker): Unit = chan.addRecvWaiter(p)
    def unregister(p: Parker): Unit = chan.removeRecvWaiter(p)
    def arm(): Unit = chan.recvArm()
    def disarm(): Unit = chan.recvDisarm()
    def tryRun(): Tried =
      chan.tryRecv().fold{ v =>
        try { f(v); Tried.Executed }
        catch case e if e.catchable => Tried.Failed(Err(e))
      }{
        case Woe.Wait    => Tried.NotReady
        case Woe.Done    => Tried.Inactive
        case Woe.Fail(e) => Tried.Failed(e)
      }
  }

  private[loom] final class SendHandler[A](chan: Chan[A], cond: () => Boolean, producer: () => A) extends Handler {
    // A produced-but-not-yet-delivered value is cached so contention never drops it.
    private var hasPending = false
    private var pending: A = null.asInstanceOf[A]

    // Live while a value is buffered to deliver, or the channel is open and `cond` may yet
    // be true.  `cond` is the producer's termination signal, so liveness must consult it.
    def alive: Boolean =
      hasPending || (chan.isOpen && { try cond() catch case e if e.catchable => true })
    def register(p: Parker): Unit = chan.addSendWaiter(p)
    def unregister(p: Parker): Unit = chan.removeSendWaiter(p)
    def arm(): Unit = chan.sendArm()
    def disarm(): Unit = chan.sendDisarm()
    def tryRun(): Tried =
      if !hasPending then
        val c =
          try cond()
          catch case e if e.catchable => return Tried.Failed(Err(e))
        if !c then return Tried.Inactive
        pending =
          try producer()
          catch case e if e.catchable => return Tried.Failed(Err(e))
        hasPending = true
      chan.trySend(pending).fold{ _ =>
        hasPending = false
        pending = null.asInstanceOf[A]
        Tried.Executed
      }{
        case Woe.Wait    => Tried.NotReady               // keep pending for next round
        case Woe.Done    => Tried.Inactive
        case Woe.Fail(e) => Tried.Failed(e)
      }
  }
}


// === Channel operations available inside a Go block ===

extension [A](chan: Chan[A]) {
  /** Handle each value as it arrives.  When the channel completes, this handler goes
    * inactive; if that leaves the scope with nothing to do, the scope terminates. */
  def onRecv(f: A => Unit)(using go: Go): Unit =
    go.addHandler(new Go.RecvHandler(chan, f))

  /** Produce values while `cond` holds and the channel is open.  Registers this scope as
    * a writer, so the channel auto-closes once every such scope has finished. */
  def onSendWhile(cond: => Boolean)(producer: => A)(using go: Go): Unit =
    go.trackWriter(chan)
    go.addHandler(new Go.SendHandler(chan, () => cond, () => producer))

  /** Send a single value, once. */
  def onSend(value: => A)(using go: Go): Unit =
    var sent = false
    onSendWhile(!sent)({ sent = true; value })

  /** Declare that the current scope writes to this channel via imperative `send` (as
    * opposed to an `onSend*` handler), so the channel still auto-closes when the scope
    * finishes.  Use this for transform/relay scopes that `recv` then `send`. */
  def writing(using go: Go): Unit = go.trackWriter(chan)
}
