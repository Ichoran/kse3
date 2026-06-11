// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.loom

import java.util.concurrent.locks.LockSupport

import scala.util.boundary

import kse.basics._
import kse.flow._


/** A bounded MPMC channel that moves data in chunks: the `Chan` lifecycle and verbs
  * over the `SplitDequeImpl` engine, so producers and consumers each touch the shared
  * structure once per batch instead of once per element.
  *
  * Two ways to use it:
  *
  *  - Imperatively, from any thread: `send`/`recv` (blocking singles), `sendN`/`recvN`
  *    (blocking bulk, straight out of / into an array), or the `try` forms of all four,
  *    plus explicit `close`/`fail`.
  *
  * Consumption has two modes: as-much-as-is-available (`get`, `recvN` — take whatever is
  * there, up to the chunk size) and only-the-full-chunk (`getFull`, `recvFullN` — take
  * nothing until a whole chunk is buffered, except that once the channel is no longer open
  * the final partial chunk is delivered rather than stranded).  Full-chunk asks are clamped
  * to the capacity, so a chunk that could never assemble is not waited for.
  *
  *  - Declaratively, inside a `Go` block: `put(n)(f)` produces elements n at a time
  *    (`f` is `i => element`, with `shortcut.skip()` to omit an element and
  *    `shortcut.quit()` to finish), `put()(produce)` is the one-at-a-time Chan-like
  *    form, `putN(a, x0, xN)` drains an array slice, and `get(f)` consumes per-item but
  *    extracts up to a batch per lock acquisition.
  *
  * The batch size is an *endpoint* parameter: each `put(n)`/`get(n)` chooses its own,
  * with [[batch]] as the channel-level default; batch = 1 degenerates to ordinary
  * `Chan` behavior, so it is a pure tuning knob.  Capacity is enforced in the channel's
  * accounting (the engine itself is unbounded), and a bulk insert with insufficient
  * room moves what fits — so a batch larger than the remaining (or even total) capacity
  * flows through in pieces rather than deadlocking.
  *
  * '''Ordering note.'''  Each producer's elements arrive in order, but with several
  * producers the merged stream interleaves in runs of up to a batch, not element by
  * element.  Cross-producer order was never specified for `Chan` either, but anyone
  * expecting rough round-robin should expect per-producer runs here.
  */
final class ChanN[A] private (val capacity: Int, val batch: Int) extends WriterTracked {
  import Chan.State
  import ChanN.Sentinel

  val lock = Sync()
  private val impl = new SplitDequeImpl(SplitDeque.defaultLgCap, SplitDeque.defaultBlockSize, 0)
  @volatile private var myState: State = State.Open
  private var recv0 = 4                                    // Index of first receiver in waiters array (builds from end)
  private var sends = 0                                    // Count of senders in waiters array (builds from start)
  private var waiters = new Array[Parker](4)

  // Count of waiters currently *armed* (parked or about to park) in each direction; a single
  // volatile read lets the hot paths skip the waiter scan when nobody is blocked (see Chan).
  private val recvArmedN = Atom(0)
  private val sendArmedN = Atom(0)

  def state: State = myState

  def isOpen: Boolean = myState == State.Open
  def isClosed: Boolean = myState match
    case State.Closed | State.Complete => true
    case _                             => false
  def isComplete: Boolean = myState == State.Complete
  def isErrored: Boolean = myState match
    case _: State.Errored => true
    case _                => false

  /** The instantaneous number of buffered elements. */
  def length: Int = lock.uninterrupted(impl.count)


  // === Waiter registration (used by Go select loops and the blocking calls) ===

  // Must be used under lock
  private def ensureWaiterSpace(): Unit =
    if sends == recv0 then
      val w = new Array[Parker](2 * waiters.length)
      if sends > 0 then
        waiters.inject(w)(0, sends) __ Unit
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
    waiters(sends) = p
    sends += 1
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
    while i < sends && seeking do
      if waiters(i) eq p then seeking = false
      i += 1
    if i < sends then waiters(i-1) = waiters(sends-1)
    if !seeking then
      sends -= 1
      waiters(sends) = null

  private[loom] def recvArm(): Unit = recvArmedN.++
  private[loom] def recvDisarm(): Unit = recvArmedN.--
  private[loom] def sendArm(): Unit = sendArmedN.++
  private[loom] def sendDisarm(): Unit = sendArmedN.--

  // Must be called while holding `lock`.  Batch-aware wakeup: an insert of k elements can
  // satisfy at most k waiting takers (usually far fewer — one chunked consumer drains many),
  // so wake at most k instead of broadcasting.  Mirrored for freed space and senders.
  // Caveat: the scan is positional, so a full-chunk waiter (which re-parks until its chunk
  // assembles) can soak up wakeups that an as-available waiter could have used; the park cap
  // (~20ms) bounds the resulting delay.  Mixing the two modes on one channel under trickle
  // loads pays that; same-mode mixes do not.
  private def wakeRecvers(k: Int): Unit =
    if recvArmedN() > 0 then
      var togo = k
      var i = recv0
      while i < waiters.length && togo > 0 do
        val p = waiters(i)
        if p.armed then
          LockSupport.unpark(p.thread)
          togo -= 1
        i += 1
  private def wakeSenders(k: Int): Unit =
    if sendArmedN() > 0 then
      var togo = k
      var i = 0
      while i < sends && togo > 0 do
        val p = waiters(i)
        if p.armed then
          LockSupport.unpark(p.thread)
          togo -= 1
        i += 1


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
        if impl.count >= capacity then RunStatus.Wait
        else
          impl.pushRight(if a.asInstanceOf[AnyRef] eq null then Sentinel else a.asInstanceOf[AnyRef])
          wakeRecvers(1)
          RunStatus.Okay
      case State.Closed | State.Complete => RunStatus.Done
      case State.Errored(e)              => RunStatus.Fail(e)

  /** Attempt to dequeue without blocking; a successful receive *is* the favored branch. */
  def tryRecv(): A Or RunStatus = lock.uninterrupted:
    if impl.count > 0 then
      val wasFull = impl.count >= capacity
      val x = impl.popLeft()
      if myState == State.Closed && impl.count == 0 then myState = State.Complete
      if wasFull then wakeSenders(1)
      Is((if x eq Sentinel then null else x).asInstanceOf[A])
    else myState match
      case State.Open => RunStatus.altWait
      case State.Closed =>
        myState = State.Complete
        RunStatus.altDone
      case State.Complete   => RunStatus.altDone
      case State.Errored(e) => Alt(RunStatus.Fail(e))

  // The one bulk-send critical section: move as many of `total` elements as there is room
  // for, with `push` doing the per-element work, all under one lock acquisition.  `Is(k)`
  // (k >= 1) elements moved; `Wait` means no room at all.
  private inline def sendCore(total: Int)(inline push: Int => Unit): Int Or RunStatus = lock.uninterrupted:
    myState match
      case State.Open =>
        val room = capacity - impl.count
        if room <= 0 then RunStatus.altWait
        else
          var k = total
          if k > room then k = room
          push(k)
          wakeRecvers(k)
          Is(k)
      case State.Closed | State.Complete => RunStatus.altDone
      case State.Errored(e)              => Alt(RunStatus.Fail(e))

  // The matching bulk-receive critical section: take min(n, available) via `take`.  Two
  // consumption modes: as-much-as-is-available (`full` = false: take whenever anything is
  // there), or only-the-full-chunk (`full` = true: take nothing until n are ready — unless
  // the channel is no longer open, in which case take whatever remains).
  private inline def recvCore(n: Int, full: Boolean)(inline take: Int => Unit): Int Or RunStatus = lock.uninterrupted:
    val m = impl.count
    if m > 0 && (!full || m >= n || myState != State.Open) then
      val wasFull = m >= capacity
      var k = n
      if k > m then k = m
      take(k)
      if myState == State.Closed && impl.count == 0 then myState = State.Complete
      if wasFull then wakeSenders(k)
      Is(k)
    else if m > 0 then RunStatus.altWait     // full-chunk mode: not enough yet, still open
    else myState match
      case State.Open => RunStatus.altWait
      case State.Closed =>
        myState = State.Complete
        RunStatus.altDone
      case State.Complete   => RunStatus.altDone
      case State.Errored(e) => Alt(RunStatus.Fail(e))

  // Reference-array fast paths for the Go handlers (caller must ensure i0 < iN / n >= 1).
  // Nulls are sentineled going in and restored coming out, so handler buffers hold plain values.
  private[loom] def trySendRaw(src: Array[AnyRef], i0: Int, iN: Int): Int Or RunStatus =
    sendCore(iN - i0): k =>
      var i = 0
      while i < k do
        val x = src(i0 + i)
        impl.pushRight(if x eq null then Sentinel else x)
        i += 1

  private[loom] def tryRecvRaw(dst: Array[AnyRef], where: Int, n: Int, full: Boolean): Int Or RunStatus =
    recvCore(n, full): k =>
      impl.popLeftInto(dst, where, k, Sentinel)

  /** Attempt to enqueue `source(x0 until xN)` without blocking, answering how many elements
    * were moved: limited by the remaining room, so possibly fewer than asked (a partial send
    * keeps the pipe full; continue from `x0 + k`).  `Alt(Wait)` means no room at all.
    */
  def trySendN(source: Array[A], x0: Int, xN: Int): Int Or RunStatus =
    if x0 < 0 || xN > source.length || x0 > xN then throw new ArrayIndexOutOfBoundsException(s"range $x0 until $xN in array of length ${source.length}")
    if x0 == xN then Is(0)
    else (source: Any) match
      case s: Array[AnyRef] => trySendRaw(s, x0, xN)
      case _ =>
        sendCore(xN - x0): k =>
          var i = 0
          while i < k do
            val x = source(x0 + i).asInstanceOf[AnyRef]
            impl.pushRight(if x eq null then Sentinel else x)
            i += 1

  // Shared body of the two non-blocking bulk receives.  The ask is clamped by the space in
  // `target` and by `capacity` — the latter matters in full-chunk mode, where waiting for
  // more than can ever be buffered at once would never end.
  private inline def tryRecvHow(target: Array[A], where: Int, n: Int, full: Boolean): Int Or RunStatus =
    if where < 0 || where > target.length then throw new ArrayIndexOutOfBoundsException(where)
    var k = if n > target.length - where then target.length - where else n
    if k > capacity then k = capacity
    if k <= 0 then Is(0)
    else (target: Any) match
      case t: Array[AnyRef] => tryRecvRaw(t, where, k, full)
      case _ =>
        recvCore(k, full): c =>
          var i = 0
          while i < c do
            val x = impl.popLeft()
            target(where + i) = (if x eq Sentinel then null else x).asInstanceOf[A]
            i += 1

  /** Attempt to dequeue up to `n` elements without blocking, copying them in order into
    * `target` starting at `where`, answering how many were moved (at least one, also limited
    * by the space in `target`).  `Alt(Wait)` means empty but still open.
    */
  def tryRecvN(target: Array[A], where: Int, n: Int): Int Or RunStatus =
    tryRecvHow(target, where, n, false)

  /** Like `tryRecvN`, but takes nothing until a full chunk of `n` is buffered — unless the
    * channel is no longer open, in which case whatever remains is taken (so a final partial
    * chunk is delivered, not stranded).  The chunk size is clamped to the capacity (and the
    * space in `target`): a chunk that could never assemble is not waited for.  `Alt(Wait)`
    * now also means "some, but not yet a chunkful".
    */
  def tryRecvFullN(target: Array[A], where: Int, n: Int): Int Or RunStatus =
    tryRecvHow(target, where, n, true)


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

  /** Block until every element of `source(x0 until xN)` has been sent, blocking whenever the
    * channel is full.  `Okay` means all were delivered; `Done`/`Fail` mean the channel
    * closed/failed first (delivery may then be partial).
    */
  def sendN(source: Array[A], x0: Int, xN: Int): RunStatus =
    if x0 < 0 || xN > source.length || x0 > xN then throw new ArrayIndexOutOfBoundsException(s"range $x0 until $xN in array of length ${source.length}")
    var i = x0
    var res: RunStatus = if i >= xN then RunStatus.Okay else RunStatus.Wait
    if res == RunStatus.Wait then
      trySendN(source, i, xN).fold{ k =>
        i += k
        if i >= xN then res = RunStatus.Okay
      }{ st => if st != RunStatus.Wait then res = st }
    if res != RunStatus.Wait then return res
    val p = Chan.parkerForCurrentThread()
    addSendWaiter(p)
    sendArm()
    try
      while res == RunStatus.Wait do
        p.armed = true
        trySendN(source, i, xN).fold{ k =>
          i += k
          if i >= xN then res = RunStatus.Okay
        }{ st =>
          if st != RunStatus.Wait then res = st
          else if Thread.interrupted() then res = RunStatus.Fail(Err("interrupted while sending"))
          else LockSupport.parkNanos(Chan.parkCapNanos)
        }
      res
    finally
      p.armed = false
      sendDisarm()
      delSendWaiter(p)

  /** Block until at least one element is available (or the channel is done/failed), then take
    * up to `n` in one gulp, copying them in order into `target` starting at `where` and
    * answering the count — the take-plus-drain idiom as a single call.  If `target` has no
    * space at `where` the ask is trivially satisfied: `Is(0)` immediately, whatever the
    * channel's state — so a drain loop should bound itself by its own space, or recycle a
    * scratch buffer and rely on `Alt(Done)`.
    */
  def recvN(target: Array[A], where: Int, n: Int): Int Or RunStatus =
    recvHow(target, where, n, false)

  /** Like `recvN`, but blocks until a full chunk of `n` is available (clamped to the capacity
    * and the space in `target`) — or the channel leaves the open state, in which case whatever
    * remains is taken.
    */
  def recvFullN(target: Array[A], where: Int, n: Int): Int Or RunStatus =
    recvHow(target, where, n, true)

  private def recvHow(target: Array[A], where: Int, n: Int, full: Boolean): Int Or RunStatus =
    val r0 = tryRecvHow(target, where, n, full)
    if !r0.existsAlt(_ == RunStatus.Wait) then return r0
    val p = Chan.parkerForCurrentThread()
    addRecvWaiter(p)
    recvArm()
    try
      var res: Int Or RunStatus = RunStatus.altWait
      while res.existsAlt(_ == RunStatus.Wait) do
        p.armed = true
        res = tryRecvHow(target, where, n, full)
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
        myState = if impl.count == 0 then State.Complete else State.Closed
        wakeRecvers(Int.MaxValue)
        wakeSenders(Int.MaxValue)
        true
      case _ => false

  /** Mark the channel as failed; pending and future operations observe the error. */
  def fail(e: Err): Boolean = lock.uninterrupted:
    myState match
      case State.Errored(_) => false
      case _ =>
        myState = State.Errored(e)
        wakeRecvers(Int.MaxValue)
        wakeSenders(Int.MaxValue)
        true
}


object ChanN {
  private[loom] object Sentinel   // Stored in place of null elements (the engine uses null for vacancy)

  /** The channel-level default batch size when none is given. */
  val defaultBatch = 16

  /** Create an open channel with the given capacity (clamped to at least 1) and the default
    * endpoint batch size. */
  def apply[A](capacity: Int): ChanN[A] = apply(capacity, defaultBatch)

  /** Create an open channel with the given capacity and default endpoint batch size (each
    * clamped to at least 1). */
  def apply[A](capacity: Int, batch: Int): ChanN[A] =
    val cap =
      if capacity <= 0 then 1
      else if capacity > Int.MaxValue - 8 then Int.MaxValue - 8
      else capacity
    new ChanN[A](cap, if batch <= 0 then 1 else batch)


  // === Select-loop handlers (chunked counterparts of the Chan handlers in Go.scala) ===

  /** Wakes once, extracts up to n elements in one lock acquisition into a reused local array,
    * then runs the per-item handler from the local copy with the shared structure untouched.
    * With `full` set it extracts nothing until a whole chunk is ready (or the channel has left
    * the open state); the chunk is clamped to the capacity so it can always assemble. */
  private[loom] final class GetNHandler[A](chan: ChanN[A], n: Int, full: Boolean, f: A => Ask[Unit]) extends Go.Handler {
    private val lim = if n > chan.capacity then chan.capacity else n
    private val buf = new Array[AnyRef](lim)
    def alive: Boolean = !chan.isComplete               // more data (or an error) may still arrive
    def hasPending: Boolean = false
    def register(p: Parker): Unit = chan.addRecvWaiter(p)
    def unregister(p: Parker): Unit = chan.delRecvWaiter(p)
    def arm(): Unit = chan.recvArm()
    def disarm(): Unit = chan.recvDisarm()
    def tryRun(producing: Boolean): RunStatus =
      if !producing then return RunStatus.Done             // stopping: don't consume anything new
      chan.tryRecvRaw(buf, 0, lim, full).fold{ k =>
        var res: RunStatus = RunStatus.Okay
        var i = 0
        while i < k do                                   // f failing skips the rest but still clears the buffer
          val v = buf(i)
          buf(i) = null
          if res == RunStatus.Okay then f(v.asInstanceOf[A]).fold{ _ => () }{ e => res = RunStatus.Fail(e) }
          i += 1
        res
      }(identity)                                        // Wait / Done / Fail pass straight through

  }

  /** One-at-a-time producer, exactly Chan's `put` shape (see `Go.SendHandler`). */
  private[loom] final class PutOneHandler[A](chan: ChanN[A], producer: () => Ask[A]) extends Go.Handler {
    private var pendingFlag = false
    private var pending: A = null.asInstanceOf[A]
    def alive: Boolean = pendingFlag || chan.isOpen
    def hasPending: Boolean = pendingFlag
    def register(p: Parker): Unit = chan.addSendWaiter(p)
    def unregister(p: Parker): Unit = chan.delSendWaiter(p)
    def arm(): Unit = chan.sendArm()
    def disarm(): Unit = chan.sendDisarm()
    def tryRun(producing: Boolean): RunStatus =
      if !pendingFlag then
        if !producing then return RunStatus.Done           // stopping: produce nothing new
        producer().fold{ a => pending = a; pendingFlag = true }{ e => return RunStatus.Fail(e) }
      chan.trySend(pending) match
        case RunStatus.Okay =>
          pendingFlag = false
          pending = null.asInstanceOf[A]
          RunStatus.Okay
        case other => other
  }

  /** Batched producer: fills a reused local array by calling `f` on a running element index
    * (skips omit an element, a quit ends production for good), then delivers the whole batch
    * in as few lock acquisitions as room allows.  An undelivered remainder is pending: it is
    * flushed before the task may stop, and only ever dropped if the channel closes under us
    * (exactly when Chan drops a pending value). */
  private[loom] final class PutNHandler[A](chan: ChanN[A], n: Int,
    f: (Go.CanFail[Unit], boundary.Label[shortcut.Type]) ?=> Int => A
  ) extends Go.Handler {
    private val buf = new Array[AnyRef](n)
    private var fill = 0      // produced elements in buf
    private var sent = 0      // prefix of buf already delivered
    private var idx = 0       // next element index to offer f
    private var live = true   // false once f has quit
    def alive: Boolean = sent < fill || (live && chan.isOpen)
    def hasPending: Boolean = sent < fill
    def register(p: Parker): Unit = chan.addSendWaiter(p)
    def unregister(p: Parker): Unit = chan.delSendWaiter(p)
    def arm(): Unit = chan.sendArm()
    def disarm(): Unit = chan.sendDisarm()
    def tryRun(producing: Boolean): RunStatus =
      if sent >= fill then
        if !producing || !live then return RunStatus.Done  // stopping or quit: produce nothing new
        sent = 0
        fill = 0
        Go.attempt[Unit]{
          shortcut.outer:
            while fill < n do
              shortcut.inner:
                buf(fill) = f(idx).asInstanceOf[AnyRef]
                fill += 1
              idx += 1
        }.fold{ _ =>
          if fill < n then live = false                    // only a quit exits the loop short
        }{ e =>
          var i = 0
          while i < fill do
            buf(i) = null
            i += 1
          fill = 0
          return RunStatus.Fail(e)
        }
        if fill == 0 then return RunStatus.Done            // quit before producing anything
      flush()
    private def flush(): RunStatus =
      chan.trySendRaw(buf, sent, fill).fold{ k =>
        val e = sent + k
        while sent < e do
          buf(sent) = null
          sent += 1
        RunStatus.Okay
      }(identity)                                          // Wait (no room) / Done / Fail
  }

  /** Drains `source(x0 until xN)` into the channel, up to `chunk` elements per lock
    * acquisition, then goes inactive.  The elements live in the caller's array, not in any
    * handler buffer, so there is nothing to flush on a stop. */
  private[loom] final class PutArrayHandler[A](chan: ChanN[A], source: Array[A], x0: Int, xN: Int, chunk: Int) extends Go.Handler {
    private var x = x0
    def alive: Boolean = x < xN && chan.isOpen
    def hasPending: Boolean = false
    def register(p: Parker): Unit = chan.addSendWaiter(p)
    def unregister(p: Parker): Unit = chan.delSendWaiter(p)
    def arm(): Unit = chan.sendArm()
    def disarm(): Unit = chan.sendDisarm()
    def tryRun(producing: Boolean): RunStatus =
      if !producing || x >= xN then return RunStatus.Done
      var lim = x + chunk
      if lim > xN then lim = xN
      chan.trySendN(source, x, lim).fold{ k =>
        x += k
        RunStatus.Okay
      }(identity)
  }
}

// The `put`/`get`/`putN` verbs for ChanN live with Chan's in Go.scala: top-level extension
// methods that overload (`get`, `put`, `writing`) must share a source file.
