// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025-26 Rex Kerr.

package kse.loom

import java.util.concurrent.locks.LockSupport

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
  inline def unparkArmed(): Unit =
    if armed then LockSupport.unpark(thread)
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
final class Chan[A] private (buffer: Array[AnyRef]) {
  import Chan.{State, Sentinel}

  val lock = Sync()
  private var head = 0                                     // index of next item to read (use under lock only)
  private var count = 0                                    // number of buffered items (use under lock only)
  @volatile private var myState: State = State.Open
  private var recv0 = 4                                    // Index of first receiver in waiters array (builds from end)
  private var sendN = 0                                    // Index past last sender in waiters array (builds from start)
  private var waiters = new Array[Parker](4)               // Threads waiting for data

  // Count of waiters currently *armed* (parked or about to park) in each direction.  The
  // common case under load is zero — the partner is busy, not blocked — so a single volatile
  // read lets push/poll skip the waiter-list scan entirely and keep the critical section short.
  private val recvArmedN = Atom(0)
  private val sendArmedN = Atom(0)

  def capacity: Int = buffer.length
  def state: State = myState

  def isOpen: Boolean = myState == State.Open
  def isClosed: Boolean = myState match
    case State.Closed | State.Complete => true
    case _                             => false
  def isComplete: Boolean = myState == State.Complete
  def isErrored: Boolean = myState match
    case _: State.Errored => true
    case _                => false

  // === Waiter registration (used by Go select loops; called under no external lock) ===

  // Must be used under lock
  private def ensureWaiterSpace(): Unit =
    if sendN == recv0 then
      val w = new Array[Parker](2 * waiters.length)
      if sendN > 0 then
        waiters.inject(w)(0, sendN) __ Unit
      if recv0 < waiters.length then
        waiters.inject(w, recv0 + waiters.length)(recv0, waiters.length) __ Unit
      recv0 += waiters.length
      waiters = w

  private[loom] def addRecvWaiter(p: Parker): Unit = lock.uninterrupted:
    ensureWaiterSpace()
    recv0 -= 1
    waiters(recv0) = p
  private[loom] def addSendWaiter(p: Parker): Unit = lock.uninterrupted:
    ensureWaiterSpace()
    waiters(sendN) = p
    sendN += 1
  private[loom] def delRecvWaiter(p: Parker): Unit = lock.uninterrupted:
    var i = waiters.length - 1
    var seeking = true
    while i >= recv0 && seeking do
      if waiters(i) eq p then seeking = false
      i -= 1
    if i >= recv0 then waiters(i+1) = waiters(recv0)
    if !seeking then
      waiters(recv0) = null
      recv0 += 1
  private[loom] def delSendWaiter(p: Parker): Unit = lock.uninterrupted:
    var i = 0
    var seeking = true
    while i < sendN && seeking do
      if waiters(i) eq p then seeking = false
      i += 1
    if i < sendN then waiters(i-1) = waiters(sendN-1)
    if !seeking then
      sendN -= 1
      waiters(sendN) = null

  // Arming bumps the count *before* the waiter re-checks readiness; a partner that publishes
  // data/space then reads the count (below, under `lock`) is guaranteed to see the bump
  // (the re-check acquires `lock` after the bump), so wakeups are never lost.
  private[loom] def recvArm(): Unit = recvArmedN.++
  private[loom] def recvDisarm(): Unit = recvArmedN.--
  private[loom] def sendArm(): Unit = sendArmedN.++
  private[loom] def sendDisarm(): Unit = sendArmedN.--

  // Must be called while holding `lock`.  Fast path: nobody armed -> nothing to do.
  private def wakeRecvers(): Unit =
    if recvArmedN() > 0 then
      waiters.use(recv0, waiters.length): p =>
        p.unparkArmed()
  private def wakeSenders(): Unit =
    if sendArmedN() > 0 then
      waiters.use(0, sendN): p =>
        p.unparkArmed()

  // === Writer tracking (drives auto-close) ===

  private val writerCount = Atom(0)

  private[loom] def registerWriter(): Unit = writerCount.++

  /** A writer scope has finished; close the channel once the last writer is gone. */
  private[loom] def writerDone(): Unit =
    if writerCount.subAndGet(1) == 0 then close() __ Unit


  // === Non-blocking core ===

  /** Attempt to enqueue without blocking.  Has no value to return, so reports a flat `RunStatus`. */
  def trySend(a: A): RunStatus = lock.uninterrupted:
    myState match
      case State.Open =>
        if count >= buffer.length then RunStatus.Wait
        else
          val idx = head + count
          buffer(if idx >= buffer.length then idx - buffer.length else idx) =
            if a.asInstanceOf[AnyRef] eq null then Sentinel else a.asInstanceOf[AnyRef]
          count += 1
          wakeRecvers()
          RunStatus.Okay
      case State.Closed | State.Complete => RunStatus.Done
      case State.Errored(e)              => RunStatus.Fail(e)

  /** Attempt to dequeue without blocking.  Carries the value in the `Is`, so `RunStatus.Okay`
    * never appears here — a successful receive *is* the favored branch. */
  def tryRecv(): A Or RunStatus = lock.uninterrupted:
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
      case State.Open => RunStatus.altWait
      case State.Closed =>
        myState = State.Complete
        RunStatus.altDone
      case State.Complete   => RunStatus.altDone
      case State.Errored(e) => Alt(RunStatus.Fail(e))


  // === Blocking ===

  /** Block until the value is sent, or the channel is closed/failed. */
  def send(a: A): RunStatus =
    trySend(a) match
      case RunStatus.Wait =>
        val p = Chan.parkerForCurrentThread()
        addSendWaiter(p)
        sendArm()
        try
          var res: RunStatus = RunStatus.Wait
          while res == RunStatus.Wait do
            p.armed = true
            res = trySend(a)
            if res == RunStatus.Wait then
              if Thread.interrupted() then res = RunStatus.Fail(Err("interrupted while sending"))
              else LockSupport.parkNanos(Chan.parkCapNanos)
          res
        finally
          p.armed = false
          sendDisarm()
          delSendWaiter(p)
      case x => x

  /** Block until a value is available, or the channel is closed/failed. */
  def recv(): A Or RunStatus =
    val r0 = tryRecv()
    if !r0.existsAlt(_ == RunStatus.Wait) then return r0
    val p = Chan.parkerForCurrentThread()
    addRecvWaiter(p)
    recvArm()
    try
      var res: A Or RunStatus = Alt(RunStatus.Wait)
      while res.existsAlt(_ == RunStatus.Wait) do
        p.armed = true
        res = tryRecv()
        if res.existsAlt(_ == RunStatus.Wait) then
          if Thread.interrupted() then res = Alt(RunStatus.Fail(Err("interrupted while receiving")))
          else LockSupport.parkNanos(Chan.parkCapNanos)
      res
    finally
      p.armed = false
      recvDisarm()
      delRecvWaiter(p)


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

  /** Create an open channel with the given capacity (clamped to at least 1). */
  def apply[A](capacity: Int): Chan[A] =
    val cap =
      if capacity <= 0 then 1
      else if capacity > Int.MaxValue - 8 then Int.MaxValue - 8
      else capacity
    new Chan[A](new Array[AnyRef](cap))
}
