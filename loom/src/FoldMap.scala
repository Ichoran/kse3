// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.loom

import java.util.concurrent.locks.LockSupport

import kse.basics._
import kse.flow._


/** A channel that folds what arrives under one key into one pending value, handing each key's value to a reader in
  * the order its key first arrived: the latest of a state, the first of some work, a count, a collection.
  *
  * Each item `u` sent has a key, `key(u)` (or given, `fold(k, u)`).  A key with nothing pending starts its value with
  * `put(u)`: `Some(v)` queues it, `None` filters the item out and queues nothing.  A key already pending folds `u` in
  * with `join(u, v)`: `Some(v2)` is the new value (`Some(v)` ignores the item), `None` withdraws the pending value and
  * its key leaves the queue.  `join` sees the value it replaces, so it is where anything that value owns is released;
  * an item that was filtered or that withdrew a key is still the sender's.  Either may answer an `Err`, which refuses
  * that item alone and leaves the channel as it was.  A reader takes [[FoldMap.Item]]s, each a key with its folded
  * value, in first-arrival order; a key updated while pending keeps its place (so a busy key cannot starve the
  * others) unless `moveToBack`, which gives debounce order instead.
  *
  * `capacity` counts distinct pending keys, so backpressure is on the depth of the queue: sending a new key waits
  * while it is full, while folding into a pending key waits only when `full(k, v)` says that key's value can take no
  * more.  A `join` that grows its value without bound has chosen to.  `onTake(k, v)` hears each value as a reader
  * takes it, so state that `key`, `put` and `join` keep beside the channel can follow what leaves.
  *
  * `key`, `put`, `join`, `full` and `onTake` all run UNDER THE CHANNEL'S LOCK: keep them quick and never wait in
  * them.  The lock is also why they may close over state of their own without locking it themselves, provided nothing
  * else touches that state.  A `key`, `put`, `join` or `full` that throws refuses that item, as an `Err` would.
  *
  * `fold` and `tryFold` answer what became of the item, as an [[FoldMap.Status]], or an `Err` when something is
  * wrong: a [[FoldMap.Refused]] when the item alone was refused, or the channel's own error once it has failed.
  * `send` and `trySend` are the same as a [[ChanOut]] answers them, `Okay` for anything the channel took a look at:
  * it is a [[ChanOut]] and a [[ChanIn]], so it closes, drains, counts its readers and writers, and plugs into `Go`
  * tasks as a [[Chan]] does.
  */
final class FoldMap[U, K, V] private (
  val capacity: Int, moveToBack: Boolean, key: U => K, put: U => (Option[V] Or Err), join: (U, V) => (Option[V] Or Err),
  full: (K, V) => Boolean, onTake: (K, V) => Unit
) extends ChanOut[U], ChanIn[FoldMap.Item[K, V]] {
  import Chan.State
  import FoldMap.{Item, Status, Refused}

  val lock = Sync()
  private val pending = new java.util.HashMap[K, Item[K, V]]()
  private var first: Item[K, V] = null                     // the oldest pending key (under lock)
  private var last: Item[K, V] = null
  @volatile private var myState: State = State.Open
  private var recv0 = 4
  private var sendN = 0
  private var waiters = new Array[Parker](4)
  private val recvArmedN = Atom(0)
  private val sendArmedN = Atom(0)
  private val writerCount = Atom(0)
  private val readerCount = Atom(0)

  def state: State = myState
  /** How many keys are pending. */
  def size: Int = lock.uninterrupted(pending.size)
  def isOpen: Boolean = myState == State.Open
  def isClosed: Boolean = myState match
    case State.Closed | State.Complete => true
    case _                             => false
  def isComplete: Boolean = myState == State.Complete


  // === The pending keys, in first-arrival order (all under lock) ===

  private def append(it: Item[K, V]): Unit =
    it.prev = last
    it.next = null
    if last eq null then first = it else last.next = it
    last = it

  private def unlink(it: Item[K, V]): Unit =
    if it.prev eq null then first = it.next else it.prev.next = it.next
    if it.next eq null then last = it.prev else it.next.prev = it.prev
    it.prev = null
    it.next = null


  // === Sending ===

  private def refused(e: Err): Ask[Status] = Alt(Err(Refused(e)))

  /** Fold `u` in under its own key without blocking: `Wait` when its key is new and the channel is full, or when its
    * key's pending value is `full`. */
  def tryFold(u: U): Ask[Status] =
    val k =
      try key(u)
      catch case e if e.catchable => return refused(Err(e))
    tryFold(k, u)

  /** Fold `u` in under `k` without blocking: `Wait` when `k` is new and the channel is full, or when `k`'s pending
    * value is `full`. */
  def tryFold(k: K, u: U): Ask[Status] = lock.uninterrupted:
    myState match
      case State.Open =>
        try
          val it = pending.get(k)
          if it ne null then
            if full(k, it.value) then Status.isWait
            else join(u, it.value).fold{
              case Some(v) =>
                it.value0 = v
                if moveToBack && (it ne last) then { unlink(it); append(it) }
                Status.isTaken
              case None =>
                unlink(it)
                pending.remove(k) __ Unit
                wakeSenders()                              // a slot came free
                Status.isWithdrawn
            }(refused)
          else if pending.size >= capacity then Status.isWait
          else put(u).fold{
            case Some(v) =>
              val fresh = new Item[K, V](k, v)
              pending.put(k, fresh) __ Unit
              append(fresh)
              wakeRecvers()
              Status.isTaken
            case None => Status.isFiltered
          }(refused)
        catch case e if e.catchable => refused(Err(e))
      case State.Closed | State.Complete => Status.isDone
      case State.Errored(e)              => Alt(e)

  /** Fold `u` in under its own key, blocking while it would `Wait`. */
  def fold(u: U): Ask[Status] =
    val k =
      try key(u)
      catch case e if e.catchable => return refused(Err(e))
    fold(k, u)

  /** Fold `u` in under `k`, blocking while it would `Wait`. */
  def fold(k: K, u: U): Ask[Status] =
    val r0 = tryFold(k, u)
    if !r0.exists(_ == Status.Wait) then r0
    else
      val p = Chan.parkerForCurrentThread()
      addSendWaiter(p)
      sendArm()
      try
        var res: Ask[Status] = r0
        while res.exists(_ == Status.Wait) do
          p.armed = true
          res = tryFold(k, u)
          if res.exists(_ == Status.Wait) then
            if Thread.currentThread.isInterrupted then res = Err.or("interrupted while sending")
            else LockSupport.parkNanos(Chan.parkCapNanos)
        res
      finally
        p.armed = false
        sendDisarm()
        delSendWaiter(p)

  private def runStatus(r: Ask[Status]): RunStatus = r.fold{
    case Status.Wait => RunStatus.Wait
    case Status.Done => RunStatus.Done
    case _           => RunStatus.Okay
  }(e => RunStatus.Fail(e))

  /** `tryFold` as a [[ChanOut]] answers it: taken, filtered and withdrawn are all `Okay`, and an `Err` is `Fail`. */
  def trySend(u: U): RunStatus = runStatus(tryFold(u))
  /** `tryFold(k, u)` as a [[ChanOut]] answers it. */
  def trySend(k: K, u: U): RunStatus = runStatus(tryFold(k, u))
  /** `fold` as a [[ChanOut]] answers it. */
  def send(u: U): RunStatus = runStatus(fold(u))
  /** `fold(k, u)` as a [[ChanOut]] answers it. */
  def send(k: K, u: U): RunStatus = runStatus(fold(k, u))


  // === Receiving ===

  // `onTake` for a value that has just left, under the lock.  One that throws has left the state it keeps unknown,
  // so the channel fails with its error; the value is still handed over.
  private def taken(it: Item[K, V]): Unit =
    try onTake(it.key, it.value0)
    catch case e if e.catchable => fail(Err(e)) __ Unit

  /** Take the oldest pending key with its value, without blocking. */
  def tryRecv(): Item[K, V] Or RunStatus = lock.uninterrupted:
    val it = first
    if it ne null then
      unlink(it)
      pending.remove(it.key) __ Unit
      if myState == State.Closed && (first eq null) then myState = State.Complete
      wakeSenders()
      taken(it)
      Is(it)
    else myState match
      case State.Open => RunStatus.altWait
      case State.Closed =>
        myState = State.Complete
        RunStatus.altDone
      case State.Complete   => RunStatus.altDone
      case State.Errored(e) => Alt(RunStatus.Fail(e))

  /** Take the oldest pending key with its value, blocking until one is pending or the channel is done. */
  def recv(): Item[K, V] Or RunStatus =
    val r0 = tryRecv()
    if !r0.existsAlt(_ == RunStatus.Wait) then return r0
    val p = Chan.parkerForCurrentThread()
    addRecvWaiter(p)
    recvArm()
    try
      var res: Item[K, V] Or RunStatus = Alt(RunStatus.Wait)
      while res.existsAlt(_ == RunStatus.Wait) do
        p.armed = true
        res = tryRecv()
        if res.existsAlt(_ == RunStatus.Wait) then
          if Thread.currentThread.isInterrupted then res = Alt(RunStatus.Fail(Err("interrupted while receiving")))
          else LockSupport.parkNanos(Chan.parkCapNanos)
      res
    finally
      p.armed = false
      recvDisarm()
      delRecvWaiter(p)

  /** Take everything pending at once, oldest first, without blocking: all of this round's state in one go.  Empty
    * when nothing is pending but more may come; the status once the channel is done. */
  def drain(): Array[Item[K, V]] Or RunStatus = lock.uninterrupted:
    if first ne null then
      val out = new Array[Item[K, V]](pending.size)
      var i = 0
      var it = first
      while it ne null do
        val nx = it.next
        it.prev = null
        it.next = null
        out(i) = it
        i += 1
        it = nx
      first = null
      last = null
      pending.clear()
      if myState == State.Closed then myState = State.Complete
      wakeSenders()
      i = 0
      while i < out.length && !myState.isInstanceOf[State.Errored] do { taken(out(i)); i += 1 }
      Is(out)
    else myState match
      case State.Open => Is(new Array[Item[K, V]](0))
      case State.Closed =>
        myState = State.Complete
        RunStatus.altDone
      case State.Complete   => RunStatus.altDone
      case State.Errored(e) => Alt(RunStatus.Fail(e))


  // === Waiters (as Chan's) ===

  private def ensureWaiterSpace(): Unit =
    if sendN == recv0 then
      val w = new Array[Parker](2 * waiters.length)
      if sendN > 0 then waiters.inject(w)(0, sendN) __ Unit
      if recv0 < waiters.length then waiters.inject(w, recv0 + waiters.length)(recv0, waiters.length) __ Unit
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
    if i >= recv0 then waiters(i + 1) = waiters(recv0)
    if !seeking then
      waiters(recv0) = null
      recv0 += 1
  private[loom] def delSendWaiter(p: Parker): Unit = lock.uninterrupted:
    var i = 0
    var seeking = true
    while i < sendN && seeking do
      if waiters(i) eq p then seeking = false
      i += 1
    if i < sendN then waiters(i - 1) = waiters(sendN - 1)
    if !seeking then
      sendN -= 1
      waiters(sendN) = null

  private[loom] def recvArm(): Unit = recvArmedN.++
  private[loom] def recvDisarm(): Unit = recvArmedN.--
  private[loom] def sendArm(): Unit = sendArmedN.++
  private[loom] def sendDisarm(): Unit = sendArmedN.--

  private def wakeRecvers(): Unit =
    if recvArmedN() > 0 then
      waiters.use(recv0, waiters.length): p =>
        p.unparkArmed()
  private def wakeSenders(): Unit =
    if sendArmedN() > 0 then
      waiters.use(0, sendN): p =>
        p.unparkArmed()


  // === Writers and readers (as Chan's) ===

  private[loom] def registerWriter(): Unit = writerCount.++
  private[loom] def writerDone(): Unit = if writerCount.subAndGet(1) == 0 then close() __ Unit
  private[loom] def registerReader(): Unit = readerCount.++
  private[loom] def readerDone(): Unit = if readerCount.subAndGet(1) == 0 then close() __ Unit
  /** Declare that this thread, outside any `Go` task, reads the channel; see `Chan.startReading`. */
  def startReading(): Unit = registerReader()
  /** This thread no longer reads the channel. */
  def stopReading(): Unit = readerDone()


  // === Explicit control ===

  /** Stop accepting sends; what is pending can still be taken, and the channel completes once it has been. */
  def close(): Boolean = lock.uninterrupted:
    myState match
      case State.Open =>
        myState = if first eq null then State.Complete else State.Closed
        wakeRecvers()
        wakeSenders()
        true
      case _ => false

  /** Fail the channel; pending and later operations observe the error. */
  def fail(e: Err): Boolean = lock.uninterrupted:
    myState match
      case State.Errored(_) => false
      case _ =>
        myState = State.Errored(e)
        wakeRecvers()
        wakeSenders()
        true
}
object FoldMap {
  /** One key with its folded value, as a reader takes it; the channel no longer touches it once handed over. */
  final class Item[K, V] private[loom] (val key: K, private[loom] var value0: V) {
    private[loom] var prev: Item[K, V] = null
    private[loom] var next: Item[K, V] = null
    def value: V = value0
    override def toString: String = s"$key -> $value0"
  }
  object Item {
    def unapply[K, V](it: Item[K, V]): (K, V) = (it.key, it.value)
  }

  /** What became of an item folded in: `Taken` into a pending value (the channel holds it now), `Filtered` out by
    * `put` or `Withdrawn` with its key's pending value by `join` (either way the item is still the sender's), `Wait`
    * from `tryFold` when it could not go in yet, `Done` once the channel is closed. */
  enum Status {
    case Taken, Filtered, Withdrawn, Wait, Done
  }
  object Status {
    private[loom] val isTaken: Ask[Status] = Is(Taken)
    private[loom] val isFiltered: Ask[Status] = Is(Filtered)
    private[loom] val isWithdrawn: Ask[Status] = Is(Withdrawn)
    private[loom] val isWait: Ask[Status] = Is(Wait)
    private[loom] val isDone: Ask[Status] = Is(Done)
  }

  /** One item refused: its `key`, `put`, `join` or `full` failed on it, with `error` as the cause.  The channel is as
    * it was, so skipping the item and going on is a fine recovery, and so is giving up (`.?`). */
  final class Refused(val error: Err) extends ErrType {
    type E = Err
    override def equals(a: Any) = a match
      case r: Refused => error == r.error
      case _ => false
    override def hashCode = error.## ^ 0x52656675
    override lazy val toString = s"item refused: $error"
    def buildLines(sb: MkStr, prefix: String): Unit =
      ErrType.buildLinesFromString(sb, "item refused:", prefix)
      Err.buildLines(error)(sb, prefix + "  ")
    def toThrowable = error.toThrowable
  }
  object Refused {
    /** The refusal `e` is, if it is one: the item alone, where anything else is the channel's. */
    def of(e: Err): Option[Refused] = e.underlying match
      case r: Refused => Some(r)
      case _ => None
  }

  private def clamped(capacity: Int): Int =
    if capacity <= 0 then 1 else if capacity > Int.MaxValue - 8 then Int.MaxValue - 8 else capacity

  /** A fold map over `capacity` pending keys: `key` names each item's key, `put` starts a key's value (`None`
    * queues nothing), `join` folds another item into it (`None` withdraws the key); `full` says when a pending value
    * takes no more, and `onTake` hears each value a reader takes.  All run under the channel's lock. */
  def apply[U, K, V](capacity: Int, moveToBack: Boolean = false)(key: U => K)(put: U => (Option[V] Or Err))(
    join: (U, V) => (Option[V] Or Err), full: (K, V) => Boolean = (_: K, _: V) => false, onTake: (K, V) => Unit = (_: K, _: V) => ()
  ): FoldMap[U, K, V] =
    new FoldMap[U, K, V](clamped(capacity), moveToBack, key, put, join, full, onTake)

  /** The latest item per key: a state feed a slow reader can fall behind on without seeing every step. */
  def latest[U, K](capacity: Int, moveToBack: Boolean = false)(key: U => K): FoldMap[U, K, U] =
    apply[U, K, U](capacity, moveToBack)(key)(u => Is(Some(u)))((u, _) => Is(Some(u)))

  /** The first item per key, later ones for a key still pending dropped: work queued once however often it is asked for. */
  def first[U, K](capacity: Int)(key: U => K): FoldMap[U, K, U] =
    apply[U, K, U](capacity)(key)(u => Is(Some(u)))((_, v) => Is(Some(v)))

  /** How many items arrived per key since it was last taken. */
  def count[U, K](capacity: Int)(key: U => K): FoldMap[U, K, Long] =
    apply[U, K, Long](capacity)(key)(_ => Is(Some(1L)))((_, n) => Is(Some(n + 1)))
}
