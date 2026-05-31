// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025 Rex Kerr.

package kse.loom

import java.util.concurrent.locks.LockSupport
import java.util.concurrent.atomic.AtomicInteger

import kse.basics._
import kse.flow._


/** A thread-parking token shared by everything that waits on a channel.
  *
  * A single `Parker` belongs to one thread (a `Go` select-loop, or a thread that is
  * making a blocking `send`/`recv` call).  The same `Parker` may be registered with many
  * channels at once (a select loop reads from some and writes to others).
  *
  * The discipline that makes wakeups lossless: a waiter sets `armed = true` *before* it
  * probes a channel for readiness, and a channel mutates its state and reads `armed`
  * while holding its own lock.  Per-channel lock serialization then guarantees that a
  * producer either is seen by the consumer's probe, or sees `armed` and unparks it.
  */
private[loom] final class Parker(val thread: Thread) {
  @volatile var armed: Boolean = false
}


/** A bounded MPMC channel with a fixed-capacity ring buffer.
  *
  * Two ways to use it:
  *
  *  - Imperatively, from any thread: `send`, `recv` (blocking) or `trySend`, `tryRecv`
  *    (non-blocking), plus explicit `close`/`fail`.
  *
  *  - Declaratively, inside a `Go` block: `onRecv`, `onSend`, `onSendWhile` register
  *    handlers that the block's select loop services.  Registering a sender also tells
  *    the channel one more writer exists; when every registered writer has finished the
  *    channel auto-closes.
  */
final class Chan[A] private (buffer: Array[AnyRef], initialState: Chan.State) {
  import Chan.{Status, State, Sentinel}

  val lock = Sync()
  private var head = 0                                     // index of next item to read
  private var count = 0                                    // number of buffered items
  @volatile private var myState: State = initialState
  private val writerCount = new AtomicInteger(0)

  // Threads waiting for data to arrive (consumers) and for space to free up (producers).
  private val recvWaiters = new java.util.ArrayList[Parker](2)
  private val sendWaiters = new java.util.ArrayList[Parker](2)

  // Count of waiters currently *armed* (parked or about to park) in each direction.  The
  // common case under load is zero — the partner is busy, not blocked — so a single volatile
  // read lets push/poll skip the waiter-list scan entirely and keep the critical section short.
  private val recvArmedN = new AtomicInteger(0)
  private val sendArmedN = new AtomicInteger(0)

  def capacity: Int = buffer.length
  def state: State = myState

  def isOpen: Boolean = myState == State.Open
  def isClosed: Boolean = myState match
    case State.Closed | State.Complete => true
    case _                             => false
  def isComplete: Boolean = myState == State.Complete
  def isErrored: Boolean = myState match
    case State.Errored(_) => true
    case _                => false

  // === Waiter registration (used by Go select loops; called under no external lock) ===

  private[loom] def addRecvWaiter(p: Parker): Unit = lock.uninterrupted{ recvWaiters.add(p) __ Unit }
  private[loom] def addSendWaiter(p: Parker): Unit = lock.uninterrupted{ sendWaiters.add(p) __ Unit }
  private[loom] def removeRecvWaiter(p: Parker): Unit = lock.uninterrupted{ recvWaiters.remove(p) __ Unit }
  private[loom] def removeSendWaiter(p: Parker): Unit = lock.uninterrupted{ sendWaiters.remove(p) __ Unit }

  // Arming bumps the count *before* the waiter re-checks readiness; a partner that publishes
  // data/space then reads the count (below, under `lock`) is guaranteed to see the bump
  // (the re-check acquires `lock` after the bump), so wakeups are never lost.
  private[loom] def recvArm(): Unit = recvArmedN.incrementAndGet() __ Unit
  private[loom] def recvDisarm(): Unit = recvArmedN.decrementAndGet() __ Unit
  private[loom] def sendArm(): Unit = sendArmedN.incrementAndGet() __ Unit
  private[loom] def sendDisarm(): Unit = sendArmedN.decrementAndGet() __ Unit

  // Must be called while holding `lock`.  Fast path: nobody armed -> nothing to do.
  private def wakeRecvers(): Unit =
    if recvArmedN.get() > 0 then
      var i = 0
      while i < recvWaiters.size do
        val p = recvWaiters.get(i)
        if p.armed then LockSupport.unpark(p.thread)
        i += 1
  private def wakeSenders(): Unit =
    if sendArmedN.get() > 0 then
      var i = 0
      while i < sendWaiters.size do
        val p = sendWaiters.get(i)
        if p.armed then LockSupport.unpark(p.thread)
        i += 1

  // === Writer tracking (drives auto-close) ===

  private[loom] def registerWriter(): Unit = writerCount.incrementAndGet() __ Unit

  /** A writer scope has finished; close the channel once the last writer is gone. */
  private[loom] def writerDone(): Unit =
    if writerCount.decrementAndGet() == 0 then close() __ Unit


  // === Non-blocking core ===

  /** Attempt to enqueue without blocking.  Has no value to return, so reports a flat `Status`. */
  def trySend(a: A): Status = lock.uninterrupted:
    myState match
      case State.Open =>
        if count >= buffer.length then Status.Wait
        else
          val idx = head + count
          buffer(if idx >= buffer.length then idx - buffer.length else idx) =
            if a.asInstanceOf[AnyRef] eq null then Sentinel else a.asInstanceOf[AnyRef]
          count += 1
          wakeRecvers()
          Status.Okay
      case State.Closed | State.Complete => Status.Done
      case State.Errored(e)              => Status.Fail(e)

  /** Attempt to dequeue without blocking.  Carries the value in the `Is`, so `Status.Okay`
    * never appears here — a successful receive *is* the favored branch. */
  def tryRecv(): A Or Status = lock.uninterrupted:
    if count > 0 then
      val wasFull = count >= buffer.length
      val v = buffer(head)
      buffer(head) = null
      head += 1
      if head >= buffer.length then head = 0
      count -= 1
      if myState == State.Closed && count == 0 then myState = State.Complete
      if wasFull then wakeSenders()
      Is((if v eq Sentinel then null else v).asInstanceOf[A])
    else myState match
      case State.Open => Chan.altWait
      case State.Closed =>
        myState = State.Complete
        Chan.altDone
      case State.Complete   => Chan.altDone
      case State.Errored(e) => Alt(Status.Fail(e))


  // === Blocking ===

  /** Block until the value is sent, or the channel is closed/failed. */
  def send(a: A): Status =
    val r0 = trySend(a)
    if r0 != Status.Wait then return r0
    val p = Chan.parkerForCurrentThread()
    addSendWaiter(p)
    sendArm()
    try
      var res: Status = Status.Wait
      while res == Status.Wait do
        p.armed = true
        res = trySend(a)
        if res == Status.Wait then
          if Thread.interrupted() then res = Status.Fail(Err("interrupted while sending"))
          else LockSupport.parkNanos(Chan.parkCapNanos)
      res
    finally
      p.armed = false
      sendDisarm()
      removeSendWaiter(p)

  /** Block until a value is available, or the channel is closed/failed. */
  def recv(): A Or Status =
    val r0 = tryRecv()
    if !r0.existsAlt(_ == Status.Wait) then return r0
    val p = Chan.parkerForCurrentThread()
    addRecvWaiter(p)
    recvArm()
    try
      var res: A Or Status = Alt(Status.Wait)
      while res.existsAlt(_ == Status.Wait) do
        p.armed = true
        res = tryRecv()
        if res.existsAlt(_ == Status.Wait) then
          if Thread.interrupted() then res = Alt(Status.Fail(Err("interrupted while receiving")))
          else LockSupport.parkNanos(Chan.parkCapNanos)
      res
    finally
      p.armed = false
      recvDisarm()
      removeRecvWaiter(p)


  // === Explicit control ===

  /** Stop accepting writes.  Buffered items can still be received; the channel becomes
    * `Complete` once drained. */
  def close(): Boolean = lock.uninterrupted:
    myState match
      case State.Open =>
        myState = if count == 0 then State.Complete else State.Closed
        wakeRecvers()
        wakeSenders()
        true
      case _ => false

  /** Mark the channel as failed; pending and future operations observe the error. */
  def fail(e: Err): Boolean = lock.uninterrupted:
    myState match
      case State.Errored(_) => false
      case _ =>
        myState = State.Errored(e)
        wakeRecvers()
        wakeSenders()
        true
}
object Chan {
  /** Upper bound on a single park, so a stop/interrupt flag is always noticed promptly
    * even in a pathological missed-wakeup case.  Wakeups are not normally lost. */
  private[loom] val parkCapNanos = 20_000_000L  // 20 ms

  private object Sentinel

  // Pre-allocated alts for the empty-poll receive paths, so they don't rebox an `Alt` every
  // call.  The favored side of an `Or` is phantom, so one instance serves every element type.
  // (Sends report a flat `Status`, so they need no wrapper at all.)
  private[loom] val altWait: Alt[Status] = Alt(Status.Wait)
  private[loom] val altDone: Alt[Status] = Alt(Status.Done)

  private val parkers = new ThreadLocal[Parker]
  private[loom] def parkerForCurrentThread(): Parker =
    val t = Thread.currentThread()
    var p = parkers.get()
    if (p eq null) || (p.thread ne t) then
      p = new Parker(t)
      parkers.set(p)
    p

  enum State {
    case Open
    case Closed            // no more writes; buffered items remain
    case Complete          // closed and drained
    case Errored(e: Err)
  }

  /** The outcome of a channel step or a select-loop step: progress, a transient block, a clean
    * finish, or an error.  Sends and select-loop steps return it directly; a receive carries its
    * value in the `Is` of an `A Or Status`, so there a success *is* the favored branch and `Okay`
    * does not appear. */
  enum Status {
    case Okay              // progress: a value moved, or a handler ran
    case Wait              // transient: full (send) or empty (recv), still open; or the loop is idle
    case Done              // permanent: channel closed/drained; or a handler is inactive
    case Fail(e: Err)      // permanent: errored
  }

  /** Create an open channel with the given capacity (clamped to at least 1). */
  def apply[A](capacity: Int): Chan[A] =
    val cap =
      if capacity <= 0 then 1
      else if capacity > Int.MaxValue - 8 then Int.MaxValue - 8
      else capacity
    new Chan[A](new Array[AnyRef](cap), State.Open)
}
