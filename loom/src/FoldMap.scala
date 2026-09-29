// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.loom

import java.util.concurrent.locks.LockSupport

import kse.basics._
import kse.flow._


/** A channel that folds what arrives under one key into one pending value, handing each key's value to a reader in
  * the order its key first arrived: the latest of a state, the first of some work, a count, a collection.
  *
  * Each item `u` sent has a key, `key(u)` (or given, `send(k, u)`).  A key with nothing pending starts its value with
  * `put(u)`; a key already pending folds `u` in with `join(u, v)`.  Either may answer `Alt.unit`: from `put`, the item
  * starts nothing and nothing is queued; from `join`, the pending value is withdrawn and its key leaves the queue.
  * `join` sees the value it replaces, so it is where anything that value owns is released.  A reader takes
  * [[FoldMap.Item]]s, each a key with its folded value, in first-arrival order; a key updated while pending keeps its
  * place (so a busy key cannot starve the others) unless `moveToBack`, which gives debounce order instead.
  *
  * `capacity` counts distinct pending keys, so backpressure is on the depth of the queue: sending a new key waits
  * while it is full, while folding into a pending key never waits.  A `join` that grows its value without bound has
  * chosen to.  `key`, `put` and `join` run under the channel's lock, so keep them quick; one that throws fails that
  * send and leaves the channel as it was.
  *
  * It is a [[ChanOut]] and a [[ChanIn]], so it closes, drains, counts its readers and writers, and plugs into `Go`
  * tasks as a [[Chan]] does.
  */
final class FoldMap[U, K, V] private (
  val capacity: Int, moveToBack: Boolean, key: U => K, put: U => (V Or Unit), join: (U, V) => (V Or Unit)
) extends ChanOut[U], ChanIn[FoldMap.Item[K, V]] {
  import Chan.State
  import FoldMap.Item

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

  /** Fold `u` in under its own key without blocking: `Wait` only when its key is new and the channel is full. */
  def trySend(u: U): RunStatus =
    val k =
      try key(u)
      catch case e if e.catchable => return RunStatus.Fail(Err(e))
    trySend(k, u)

  /** Fold `u` in under `k` without blocking: `Wait` only when `k` is new and the channel is full. */
  def trySend(k: K, u: U): RunStatus = lock.uninterrupted:
    myState match
      case State.Open =>
        try
          val it = pending.get(k)
          if it ne null then
            join(u, it.value).fold{ v =>
              it.value0 = v
              if moveToBack && (it ne last) then { unlink(it); append(it) }
            }{ _ =>
              unlink(it)
              pending.remove(k) __ Unit
              wakeSenders()                              // a slot came free
            }
            RunStatus.Okay
          else if pending.size >= capacity then RunStatus.Wait
          else
            put(u).foreach{ v =>
              val fresh = new Item[K, V](k, v)
              pending.put(k, fresh) __ Unit
              append(fresh)
              wakeRecvers()
            }
            RunStatus.Okay
        catch case e if e.catchable => RunStatus.Fail(Err(e))
      case State.Closed | State.Complete => RunStatus.Done
      case State.Errored(e)              => RunStatus.Fail(e)

  /** Fold `u` in under its own key, blocking while its key is new and the channel is full. */
  def send(u: U): RunStatus =
    val k =
      try key(u)
      catch case e if e.catchable => return RunStatus.Fail(Err(e))
    send(k, u)

  /** Fold `u` in under `k`, blocking while `k` is new and the channel is full. */
  def send(k: K, u: U): RunStatus =
    trySend(k, u) match
      case RunStatus.Wait =>
        val p = Chan.parkerForCurrentThread()
        addSendWaiter(p)
        sendArm()
        try
          var res: RunStatus = RunStatus.Wait
          while res == RunStatus.Wait do
            p.armed = true
            res = trySend(k, u)
            if res == RunStatus.Wait then
              if Thread.currentThread.isInterrupted then res = RunStatus.Fail(Err("interrupted while sending"))
              else LockSupport.parkNanos(Chan.parkCapNanos)
          res
        finally
          p.armed = false
          sendDisarm()
          delSendWaiter(p)
      case x => x


  // === Receiving ===

  /** Take the oldest pending key with its value, without blocking. */
  def tryRecv(): Item[K, V] Or RunStatus = lock.uninterrupted:
    val it = first
    if it ne null then
      unlink(it)
      pending.remove(it.key) __ Unit
      if myState == State.Closed && (first eq null) then myState = State.Complete
      wakeSenders()
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

  private def clamped(capacity: Int): Int =
    if capacity <= 0 then 1 else if capacity > Int.MaxValue - 8 then Int.MaxValue - 8 else capacity

  /** A fold map over `capacity` pending keys: `key` names each item's key, `put` starts a key's value, `join` folds
    * another item into it; `Alt.unit` from `put` queues nothing, from `join` withdraws the key. */
  def apply[U, K, V](capacity: Int, moveToBack: Boolean = false)(key: U => K)(put: U => (V Or Unit))(join: (U, V) => (V Or Unit)): FoldMap[U, K, V] =
    new FoldMap[U, K, V](clamped(capacity), moveToBack, key, put, join)

  /** The latest item per key: a state feed a slow reader can fall behind on without seeing every step. */
  def latest[U, K](capacity: Int, moveToBack: Boolean = false)(key: U => K): FoldMap[U, K, U] =
    apply[U, K, U](capacity, moveToBack)(key)(u => Is(u))((u, _) => Is(u))

  /** The first item per key, later ones for a key still pending dropped: work queued once however often it is asked for. */
  def first[U, K](capacity: Int)(key: U => K): FoldMap[U, K, U] =
    apply[U, K, U](capacity)(key)(u => Is(u))((_, v) => Is(v))

  /** How many items arrived per key since it was last taken. */
  def count[U, K](capacity: Int)(key: U => K): FoldMap[U, K, Long] =
    apply[U, K, Long](capacity)(key)(_ => Is(1L))((_, n) => Is(n + 1))
}
