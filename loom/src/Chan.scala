// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025-26 Rex Kerr.

package kse.loom

import java.util.concurrent.locks.LockSupport

import scala.collection.immutable.{Range => Rg}

import kse.basics._
import kse.basics.intervals._
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


/** A channel whose writers are counted, so it auto-closes when the last registered
  * writer finishes.  Lets `Go` track [[Chan]] and [[ChanN]] uniformly.
  */
private[loom] trait WriterTracked {
  private[loom] def registerWriter(): Unit
  private[loom] def writerDone(): Unit
}


// === The receive / send surfaces the verbs and select-loop handlers target ===
//
// Splitting `Chan` into a read end ([[ChanIn]]) and a write end ([[ChanOut]]) lets the same
// `get`/`put`/`into` verbs and `Go` handlers drive every channel-like: a full [[Chan]]/[[ChanN]]
// (both ends), a [[Chan.Source]] (read end only, backed by data already on hand), or a sink
// (write end only).  Source and sink are the degenerate endpoints that can never say `Wait` —
// a source always has its next value, a sink always has room — so a select loop never parks on
// one.  The public surface is `tryRecv`/`recv`/`trySend`/`send` plus liveness; the waiter and
// arm hooks are package-private plumbing the loop wires up.

/** The receive end of a channel-like source of values: pull one value (the favored branch) or
  * learn why you can't — `Wait` (nothing yet, but more may come), `Done` (drained and closed),
  * `Fail`.  A [[Chan.Source]] is the case that never says `Wait`. */
trait ChanIn[A] {
  /** Pull one value without blocking; a successful receive *is* the favored branch. */
  def tryRecv(): A Or RunStatus
  /** Pull one value, blocking until one is available or the source is done/failed. */
  def recv(): A Or RunStatus
  /** True once no further value can ever be received. */
  def isComplete: Boolean

  private[loom] def addRecvWaiter(p: Parker): Unit
  private[loom] def delRecvWaiter(p: Parker): Unit
  private[loom] def recvArm(): Unit
  private[loom] def recvDisarm(): Unit
}

/** A [[ChanIn]] that can also hand out values in bulk, filling a caller's array in one lock
  * acquisition.  Implemented by [[ChanN]] and [[Chan.Source]]; drives the chunked `get` verbs. */
trait ChanInN[A] extends ChanIn[A] {
  /** The most elements deliverable in a single bulk receive (bounds handler scratch buffers). */
  def capacity: Int
  private[loom] def tryRecvRaw(dst: Array[AnyRef], where: Int, n: Int, full: Boolean): Int Or RunStatus
}

/** The send end of a channel-like destination: enqueue one value or learn `Wait` (no room yet)
  * / `Done` (closed) / `Fail`.  A sink is the case that never says `Wait`.  Extends
  * [[WriterTracked]] so the `Go` auto-close cascade can finalize any send end uniformly. */
trait ChanOut[A] extends WriterTracked {
  /** Enqueue one value without blocking. */
  def trySend(a: A): RunStatus
  /** Enqueue one value, blocking until there is room or the destination is closed/failed. */
  def send(a: A): RunStatus
  /** True while the destination still accepts writes. */
  def isOpen: Boolean
  /** True once the destination accepts no more writes. */
  def isClosed: Boolean

  private[loom] def addSendWaiter(p: Parker): Unit
  private[loom] def delSendWaiter(p: Parker): Unit
  private[loom] def sendArm(): Unit
  private[loom] def sendDisarm(): Unit
}

/** A [[ChanOut]] that also accepts values in bulk, straight out of a caller's array.
  * Implemented by [[ChanN]] (and bulk-capable sinks); drives the chunked `put`/`putN` verbs. */
trait ChanOutN[A] extends ChanOut[A] {
  def capacity: Int
  private[loom] def trySendRaw(src: Array[AnyRef], i0: Int, iN: Int): Int Or RunStatus
  private[loom] def trySendN(source: Array[A], x0: Int, xN: Int): Int Or RunStatus
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
final class Chan[A] private (buffer: Array[AnyRef]) extends ChanIn[A], ChanOut[A] {
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


  /** A read-only, never-blocking channel end backed by data already on hand — an array, an
    * `Iterable`, or an iterator/enumeration.  A `Chan.Source` plugs into the same `get`/`into`
    * consume verbs and `Go` select loops as a [[Chan]], but a consumer never *parks* on it: every
    * pull either yields the next value(s) or reports the source exhausted.
    *
    * Pulls are serialized under a lock, so one source is safely shared by many consumers —
    * `Go.x(8){ src.get{ … } }` is a parallel drain of a single iterator with no pump task.  The
    * backing is advanced *synchronously* on the consuming thread, so a backing whose `hasNext`/
    * `next` blocks will block that consumer; a source is for data on hand, not a substitute for a
    * pumped [[Chan]].
    *
    * `close()` makes the source *immediately* and completely unavailable: values not yet pulled
    * are abandoned (unlike `Chan.close`, which drains).  Natural exhaustion is the graceful path.
    *
    * Build one with the `Chan.Source` factory, e.g. `Chan.Source(xs)`.
    */
  sealed abstract class Source[A] extends ChanInN[A] {
    import Source.End

    private val lock = Sync()
    @volatile private var ended = false           // exhausted or closed: no value will ever come
    @volatile private var failing: Err | Null = null

    /** A source is not ring-bounded, so a bulk `get(n)` is capped only by `n` itself. */
    final def capacity: Int = Int.MaxValue - 8

    final def isComplete: Boolean = ended
    final def isErrored: Boolean = failing != null

    /** Hand over the next value under the lock, or [[Source.End]] once the backing is spent.
      * `null` is a legitimate value and passes through (the end marker is a distinct object). */
    protected def pullOne(): AnyRef

    /** Fill `dst(where until where+n)` from the backing, returning how many were copied (0 at the
      * end).  Called under the lock; the default drives [[pullOne]], but a random-access backing
      * (an array) overrides it with a block copy. */
    protected def pull(dst: Array[AnyRef], where: Int, n: Int): Int =
      var k = 0
      while k < n do
        val v = pullOne()
        if v eq End then return k
        dst(where + k) = v
        k += 1
      k

    private def endedStatus: Alt[RunStatus] =
      if failing != null then Alt(RunStatus.Fail(failing.asInstanceOf[Err])) else RunStatus.altDone

    // End the source exactly once (every caller holds the lock).
    private def finish(err: Err | Null): Boolean =
      if ended then false
      else
        if err != null then failing = err
        ended = true
        true

    def tryRecv(): A Or RunStatus = lock.uninterrupted:
      if ended then endedStatus
      else
        var out: A Or RunStatus = RunStatus.altDone
        try
          val v = pullOne()
          if v eq End then { finish(null) __ Unit; out = endedStatus }
          else out = Is(v.asInstanceOf[A])
        catch case e if e.catchable => { finish(Err(e)) __ Unit; out = endedStatus }
        out

    def recv(): A Or RunStatus = tryRecv()          // a source never blocks

    private[loom] def tryRecvRaw(dst: Array[AnyRef], where: Int, n: Int, full: Boolean): Int Or RunStatus =
      lock.uninterrupted:
        if ended then endedStatus
        else
          var out: Int Or RunStatus = RunStatus.altDone
          try
            val k = pull(dst, where, n)           // `full` is moot: a source's only short read is its last
            if k > 0 then out = Is(k)
            else { finish(null) __ Unit; out = endedStatus }
          catch case e if e.catchable => { finish(Err(e)) __ Unit; out = endedStatus }
          out

    // A live source always has its next value, so no consumer ever parks on it: nothing to wire.
    private[loom] def addRecvWaiter(p: Parker): Unit = ()
    private[loom] def delRecvWaiter(p: Parker): Unit = ()
    private[loom] def recvArm(): Unit = ()
    private[loom] def recvDisarm(): Unit = ()

    /** Abandon any remaining values; the source becomes immediately unavailable.  Any resource
      * behind the backing is the caller's to release — use `Defer` in the enclosing `Go` scope. */
    def close(): Boolean = lock.uninterrupted:
      finish(null)

    /** Poison the source: subsequent receives observe `Fail(e)` instead of more values. */
    def fail(e: Err): Boolean = lock.uninterrupted:
      if failing != null then false else finish(e)
  }
  object Source {
    /** Returned by a backing's `pullOne` once it is spent (distinct from a `null` element). */
    private[loom] object End

    private final class ArraySource[A](arr: Array[A], private var x: Int, xN: Int) extends Source[A] {
      protected def pullOne(): AnyRef =
        if x >= xN then End
        else { val v = arr(x); x += 1; v.asInstanceOf[AnyRef] }
      override protected def pull(dst: Array[AnyRef], where: Int, n: Int): Int =
        var k = xN - x
        if k > n then k = n
        var i = 0
        while i < k do { dst(where + i) = arr(x + i).asInstanceOf[AnyRef]; i += 1 }
        x += k
        k
    }

    private final class IteratorSource[A](it: Iterator[A]) extends Source[A] {
      protected def pullOne(): AnyRef = if it.hasNext then it.next().asInstanceOf[AnyRef] else End
    }

    private final class JavaIteratorSource[A](j: java.util.Iterator[A]) extends Source[A] {
      protected def pullOne(): AnyRef = if j.hasNext then j.next().asInstanceOf[AnyRef] else End
    }

    private final class EnumerationSource[A](e: java.util.Enumeration[A]) extends Source[A] {
      protected def pullOne(): AnyRef = if e.hasMoreElements then e.nextElement().asInstanceOf[AnyRef] else End
    }

    private final class FnSource[A](pull: () => (A Or Unit)) extends Source[A] {
      protected def pullOne(): AnyRef = pull().fold(a => a.asInstanceOf[AnyRef])(_ => End)
    }

    private final class StepperSource[A](s: scala.collection.Stepper[A]) extends Source[A] {
      protected def pullOne(): AnyRef = if s.hasStep then s.nextStep().asInstanceOf[AnyRef] else End
    }

    private final class SpliteratorSource[A](sp: java.util.Spliterator[A]) extends Source[A] {
      private var slot: AnyRef = End                       // set by `grab` during a tryAdvance
      private val grab: java.util.function.Consumer[A] = (a: A) => slot = a.asInstanceOf[AnyRef]
      protected def pullOne(): AnyRef =
        slot = End
        if sp.tryAdvance(grab) then slot else End          // a null element leaves slot != End
    }

    /** A source over an entire array. */
    def apply[A](xs: Array[A]): Source[A] = new ArraySource(xs, 0, xs.length)

    /** A source over `xs(i0 until iN)`.  The slice must not be mutated while the source is live. */
    def apply[A](xs: Array[A], i0: Int, iN: Int): Source[A] =
      if i0 < 0 || iN > xs.length || i0 > iN then
        throw new ArrayIndexOutOfBoundsException(s"range $i0 until $iN in array of length ${xs.length}")
      new ArraySource(xs, i0, iN)

    /** A source over the slice of `xs` given by a range literal or an `Iv.X` interval.  The slice must not be mutated while the source is live. */
    inline def apply[A, R <: Iv.X | Rg](xs: Array[A], inline r: R): Source[A] = Iv.dispatch(r, xs)((i0, iN) => apply(xs, i0, iN))

    /** A source over any `Iterable` or `Iterator` (drives its `iterator` once). */
    def apply[A](xs: IterableOnce[A]): Source[A] = new IteratorSource(xs.iterator)

    /** A source over a Java `Iterator`. */
    def apply[A](j: java.util.Iterator[A]): Source[A] = new JavaIteratorSource(j)

    /** A source over a Java `Enumeration`. */
    def apply[A](e: java.util.Enumeration[A]): Source[A] = new EnumerationSource(e)

    /** A source over a `Stepper` — the standard-library Spliterator/Stream bridge (and fast
      * primitive access).  Assumed ready to step, like an `Iterator`. */
    def apply[A](s: scala.collection.Stepper[A]): Source[A] = new StepperSource(s)

    // `from` gates the backings that may break the never-block assumption: a caller-supplied
    // generator, or a Java Stream/Spliterator whose elements may be produced lazily or block.

    /** A source that generates each value by calling `pull`: `Is(a)` is the next value, `Alt.unit`
      * means spent.  `pull` runs synchronously on the consumer, so it may block or fail — hence
      * `from`, not `apply`.  Release any resource it reads from with a `Defer` in the `Go` scope. */
    def from[A](pull: () => (A Or Unit)): Source[A] = new FnSource(pull)

    /** A source over a Java `Stream` (via its `iterator`).  Under `from` because a stream's elements
      * may be produced lazily or block; the stream is not closed for you — use `Defer`. */
    def from[A](st: java.util.stream.Stream[A]): Source[A] = new JavaIteratorSource(st.iterator())

    /** A source over a Java `Spliterator`.  Under `from` for the same reason as `Stream`. */
    def from[A](sp: java.util.Spliterator[A]): Source[A] = new SpliteratorSource(sp)
  }


  /** A write-only, never-blocking channel end backed by a consumer already on hand — a callback or
    * a growable collection.  A `Chan.Sink` plugs into the same `put`/`into` produce verbs and `Go`
    * select loops as a [[Chan]], but a producer never *parks* on it: it always has room, so every
    * send is accepted at once until the sink is closed.
    *
    * Sends are serialized under a lock, so one sink safely absorbs many producers.  The consumer
    * runs *synchronously* on the sending thread, so a slow consumer slows that producer; a sink is
    * for a consumer that keeps up, not a substitute for a buffered [[Chan]].
    *
    * It closes exactly once, when the last registered writer finishes (the `Go` auto-close cascade)
    * or on an explicit `close()`/`fail`.  A resource behind the consumer is the caller's to release
    * — register a `Defer` in the enclosing `Go` scope.  Build one with the `Chan.Sink` factory, e.g.
    * `Chan.Sink(f)` or `Chan.Sink.into(buf)`.
    */
  sealed abstract class Sink[A] extends ChanOutN[A] {
    private val lock = Sync()
    @volatile private var closed = false
    @volatile private var failing: Err | Null = null
    private val writerCount = Atom(0)

    /** A sink is unbounded, so a bulk send always takes the whole request. */
    final def capacity: Int = Int.MaxValue - 8

    final def isOpen: Boolean = !closed
    final def isClosed: Boolean = closed
    final def isErrored: Boolean = failing != null

    /** Accept one value under the lock. */
    protected def push(a: A): Unit

    private def endedStatus: RunStatus =
      if failing != null then RunStatus.Fail(failing.asInstanceOf[Err]) else RunStatus.Done

    // Close the sink exactly once (every caller holds the lock).
    private def finish(err: Err | Null): Boolean =
      if closed then false
      else
        if err != null then failing = err
        closed = true
        true

    def trySend(a: A): RunStatus = lock.uninterrupted:
      if closed then endedStatus
      else
        try { push(a); RunStatus.Okay }
        catch case e if e.catchable => { finish(Err(e)) __ Unit; endedStatus }

    def send(a: A): RunStatus = trySend(a)          // a sink never blocks

    private[loom] def trySendRaw(src: Array[AnyRef], i0: Int, iN: Int): Int Or RunStatus = lock.uninterrupted:
      if closed then Alt(endedStatus)
      else
        try
          var i = i0
          while i < iN do { push(src(i).asInstanceOf[A]); i += 1 }
          Is(iN - i0)
        catch case e if e.catchable => { finish(Err(e)) __ Unit; Alt(endedStatus) }

    def trySendN(source: Array[A], x0: Int, xN: Int): Int Or RunStatus =
      if x0 < 0 || xN > source.length || x0 > xN then
        throw new ArrayIndexOutOfBoundsException(s"range $x0 until $xN in array of length ${source.length}")
      if x0 == xN then Is(0)
      else lock.uninterrupted:
        if closed then Alt(endedStatus)
        else
          try
            var i = x0
            while i < xN do { push(source(i)); i += 1 }
            Is(xN - x0)
          catch case e if e.catchable => { finish(Err(e)) __ Unit; Alt(endedStatus) }

    // A live sink always has room, so no producer ever parks on it: nothing to wire.
    private[loom] def addSendWaiter(p: Parker): Unit = ()
    private[loom] def delSendWaiter(p: Parker): Unit = ()
    private[loom] def sendArm(): Unit = ()
    private[loom] def sendDisarm(): Unit = ()

    private[loom] def registerWriter(): Unit = writerCount.++
    private[loom] def writerDone(): Unit = if writerCount.subAndGet(1) == 0 then close() __ Unit

    /** Stop accepting values.  Any resource behind the consumer is the caller's to release — use
      * `Defer` in the enclosing `Go` scope. */
    def close(): Boolean = lock.uninterrupted:
      finish(null)

    /** Poison the sink: it finalizes and subsequent sends observe `Fail(e)`. */
    def fail(e: Err): Boolean = lock.uninterrupted:
      if failing != null then false else finish(e)
  }
  object Sink {
    private final class FnSink[A](f: A => Unit) extends Sink[A] {
      protected def push(a: A): Unit = f(a)
    }

    /** A sink that appends each value to a mutable `Growable` — a `Builder`, `ArrayBuffer`, or a
      * `scala.jdk.Accumulator` (which converts back to a Java `Stream`).  Read the accumulated
      * result straight from `g` once the enclosing scope completes. */
    def apply[A](g: scala.collection.mutable.Growable[A]): Sink[A] = new FnSink(a => (g += a) __ Unit)

    /** A sink that adds each value to a Java `Collection`. */
    def apply[A](c: java.util.Collection[A]): Sink[A] = new FnSink(a => c.add(a) __ Unit)

    /** A sink that hands each value to `f`.  Unlike the container sinks, `f` may block or fail — use
      * it for a consumer whose readiness you can't guarantee (an IO write, a bounded queue), hence
      * `to`, not `apply`.  Release any resource `f` writes to with a `Defer` in the `Go` scope. */
    def to[A](f: A => Unit): Sink[A] = new FnSink(f)
  }
}
