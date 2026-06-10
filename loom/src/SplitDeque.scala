// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025-26 Rex Kerr

package kse.loom


import java.util.concurrent.locks.LockSupport

import scala.annotation.publicInBinary

import kse.flow._


/** A concurrent deque with O(blockSize * log n) extraction and insertion of batches
  * of any size, plus fast single-element access at both ends.  This suits m-to-n
  * rebatching: many fast producers feeding a chunk-based consumer, one producer
  * feeding many consumers, or anything between.
  *
  * == Concurrency ==
  *
  * Every operation is atomic.  A single atomic `len` field is simultaneously the
  * size, the lock, and the empty/full check: non-negative means unlocked with that
  * many elements, -1 means some thread is inside.  Acquisition is
  * `getAndSetAcquire(-1)` with a spin/yield/park backoff ladder under contention;
  * release is a single `setRelease` of the updated size.  Lock hold times are
  * bounded by the most expensive operation, O(blockSize * log n), so the structure
  * is suited to moderate contention; use [[contention]] to measure.  A progress
  * version (`vzn`) lets spinning threads distinguish a busy deque from a stuck one
  * (a thread that died inside, which cannot happen barring VM error): no progress
  * over a full backoff cycle throws `ConcurrentModificationException` rather than
  * spinning forever.
  *
  * == Batches ==
  *
  * Batch operations traffic in [[SplitDeque.Batch]]: a plain, fully independent,
  * non-thread-safe deque over the same engine.  `splitLeft`/`splitRight` move the
  * first/last n elements into a fresh batch in O(blockSize * log n) time;
  * `spliceLeft`/`spliceRight` move a batch's entire contents in at the same cost,
  * leaving the batch empty but reusable.  A batch shares no structure with any
  * deque or other batch, so it needs only the usual care of any non-concurrent
  * collection (e.g. an array): confine it to one thread at a time.
  *
  * The single-threaded engine underneath is `SplitDequeImpl` (see there for the
  * representation); this shell adds only locking, null sentineling, and size
  * accounting.
  */
final class SplitDeque[A] private[kse] (lgCap: Int, blockSize: Int) {

  def this() = this(SplitDeque.defaultLgCap, SplitDeque.defaultBlockSize)

  import SplitDeque.{lenHandle, vznHandle, Sentinel, Batch}

  private val impl = new SplitDequeImpl(lgCap, blockSize, 0)
  // Accessed via VarHandle: @publicInBinary makes each field part of the binary API, so the
  // compiler may neither elide it (len has no direct Scala access!) nor change its name.
  @publicInBinary private[loom] var len: Int = 0   // The size, or -1 while a thread is inside; guards everything but vzn
  @publicInBinary private[loom] var vzn: Int = 1   // Progress version, bumped on every release; written only while holding len


  // === The lock: acquire/release, backoff, and no-progress detection ===

  private def backoff(fatigue: Int): Int =
    if (fatigue & 0xF) != 0xF then
      fatigue + 1
    else if (fatigue & 0x3F0) != 0x3F0 then
      Thread.onSpinWait()
      fatigue + 0x10
    else if (fatigue & 0xFFC00) != 0xFFC00 then
      var delay = 1 + (fatigue & 0xFFC00)
      if delay > 0xC0000 then delay = delay << 3
      LockSupport.parkNanos(delay)
      fatigue + 0x400
    else
      Int.MaxValue

  // v0 == 0 means first check (vzn is always odd); Int.MinValue means no progress since last check
  private def vercheck(v0: Int): Int =
    val v = vznHandle.getVolatile(this).asInstanceOf[Int]
    if v0 != 0 && v0 == v then Int.MinValue
    else v

  private def DIE_BLOCKED(): Nothing =
    throw new java.util.ConcurrentModificationException("SplitDeque blocked and making no progress")

  private def DIE_FULL(m: Int): Nothing =
    release(m)
    throw new IllegalStateException("Add to a full SplitDeque")

  // Spin for the lock with escalating backoff; Int.MaxValue (an impossible size) means we gave up
  private def compete(): Int =
    var m = -1
    var b = 0
    var v = 0
    while m < 0 do
      b = backoff(b)
      if b == Int.MaxValue then
        v = vercheck(v)
        b = 0xC03FFF
      m = if v == Int.MinValue then Int.MaxValue else lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    m

  private def acquire(): Int =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m < 0 then
      m = compete()
      if m == Int.MaxValue then DIE_BLOCKED()
    m

  private def release(m: Int): Unit =
    vzn += 2
    lenHandle.setRelease(this, m)


  // === Size and contention diagnostics ===

  /** The instantaneous stable size.  Some other thread will observe this size in the
    * future, but there are no guarantees as to which one.
    */
  def length: Int =
    var m = lenHandle.getVolatile(this).asInstanceOf[Int]
    var b = 0
    var v = 0
    while m < 0 do
      b = backoff(b)
      if b == Int.MaxValue then
        v = vercheck(v)
        if v == Int.MinValue then DIE_BLOCKED()
        else b = 0xC03FFF
      m = lenHandle.getVolatile(this).asInstanceOf[Int]
    m

  def isEmpty: Boolean = length == 0

  /** A numerical representation of instantaneous contention--average multiple calls to get a more representative picture.
    *
    * 0.0 indicates no contention.  Values above 0.0 but below 1.0 indicate moderate contention handled with spin-waiting.
    * Values at 1.0 or above indicate heavy contention handled with timed sleeps via `LockSupport.park`.  PositiveInfinity
    * indicates that access was never obtained.
    */
  def contention(): Double =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m >= 0 then
      lenHandle.setRelease(this, m)
      0.0
    else
      var b = 0
      while m < 0 && b < Int.MaxValue do
        b = backoff(b)
        m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
      if m >= 0 then lenHandle.setRelease(this, m)
      if b <= 0xF then 0.005*b
      else if b <= 0x3FF then 0.01*(1 + (b >> 4))
      else if b <= 0xFFFFF then (b >> 10).toDouble
      else Double.PositiveInfinity

  /** A numerical representation of instantaneous contention--average multiple calls to get a more representative picture.
    *
    * 0.0 indicates no contention.  Values above 0.0 but below 1.0 indicate moderate contention handled with spin-waiting.
    * Values at 1.0 or above indicate heavy contention handled with timed sleeps via `LockSupport.park`.  PositiveInfinity
    * indicates that access was never obtained.
    *
    * This version calls a wakeup callback every time a parked thread resumes with an internal restart counter (which may
    * regress if other threads make progress) and only continues if the callback returns true.
    */
  def contention(onWake: Int => Boolean): Double =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m >= 0 then
      lenHandle.setRelease(this, m)
      0.0
    else
      var b = 0
      var run = true
      while run do
        b = backoff(b)
        m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
        run = m < 0 && (b != Int.MaxValue) && (((b & 0xFFC00) == 0) || onWake(b >>> 10))
      if m >= 0 then lenHandle.setRelease(this, m)
      if b <= 0xF then 0.005*b
      else if b <= 0x3FF then 0.01*(1 + (b >> 4))
      else if b <= 0xFFFFF then (b >> 10).toDouble
      else Double.PositiveInfinity


  // === Single-element operations ===

  /** Add an element at the front (typical for stack ordering). */
  def pushLeft(a: A): Unit =
    val x = if a.asInstanceOf[AnyRef] eq null then Sentinel else a.asInstanceOf[AnyRef]
    val m = acquire()
    if Int.MaxValue - m < 8 then DIE_FULL(m)
    impl.pushLeft(x)
    release(m + 1)

  /** Add an element at the back (typical for queue ordering). */
  def pushRight(a: A): Unit =
    val x = if a.asInstanceOf[AnyRef] eq null then Sentinel else a.asInstanceOf[AnyRef]
    val m = acquire()
    if Int.MaxValue - m < 8 then DIE_FULL(m)
    impl.pushRight(x)
    release(m + 1)

  /** Take the first element, or Alt.unit if empty (typical for both stack and queue ordering). */
  def popLeft(): A Or Unit =
    val m = acquire()
    if m == 0 then
      release(0)
      Alt.unit
    else
      val x = impl.popLeft()
      release(m - 1)
      Is((if x eq Sentinel then null else x).asInstanceOf[A])

  /** Take the last element, or Alt.unit if empty (typical for a second stack at the back). */
  def popRight(): A Or Unit =
    val m = acquire()
    if m == 0 then
      release(0)
      Alt.unit
    else
      val x = impl.popRight()
      release(m - 1)
      Is((if x eq Sentinel then null else x).asInstanceOf[A])

  /** Adds a new item at the end of the deque.  Equivalent to pushRight. */
  inline def +=(a: A): Unit = pushRight(a)

  /** Adds a new item to the front of the deque.  Equivalent to pushLeft. */
  inline infix def push(a: A): Unit = pushLeft(a)

  /** Gets an item from the front of the deque if one is available.  Equivalent to popLeft. */
  inline def get(): A Or Unit = popLeft()

  /** Removes an item from the front of the deque.  Equivalent to popLeft. */
  inline def pop(): A Or Unit = popLeft()


  // === Bulk extraction into arrays ===

  // Largest cut copied directly under the lock; bigger cuts detach in
  // O(blockSize * log n) and copy outside, so lock hold time never becomes O(n).
  // Also keeps the direct path's boxed count inside the preallocated Integer cache.
  private inline val DirectCut = 127

  // Remove up to n of the first/last elements under one lock acquisition: either
  // copied straight into target (answering the count, boxed for free), or, when
  // there are many, detached wholesale as an engine for the caller to drain with
  // no lock held at all.
  private def cutLeft(target: Array[AnyRef], where: Int, n: Int): SplitDequeImpl | Int =
    val m = acquire()
    var k = n
    if k > m then k = m
    if k <= DirectCut then
      impl.popLeftInto(target, where, k, Sentinel)
      release(m - k)
      k
    else
      val e = impl.splitLeft(k)
      release(m - k)
      e

  private def cutRight(target: Array[AnyRef], where: Int, n: Int): SplitDequeImpl | Int =
    val m = acquire()
    var k = n
    if k > m then k = m
    if k <= DirectCut then
      impl.popRightInto(target, where, k, Sentinel)
      release(m - k)
      k
    else
      val e = impl.splitRight(k)
      release(m - k)
      e

  // Fallback for primitive-element targets (the engine's bulk copy needs a reference
  // array); elements were boxed going in, and the generic update unboxes them here.
  private def popSlowly(target: Array[A], where: Int, n: Int, left: Boolean): Int =
    val m = acquire()
    var k = n
    if k > m then k = m
    var i = 0
    while i < k do
      val x = if left then impl.popLeft() else impl.popRight()
      val j = if left then where + i else where + k - 1 - i
      target(j) = (if x eq Sentinel then null else x).asInstanceOf[A]
      i += 1
    release(m - k)
    k

  /** Remove up to n of the first elements, copying them in order into `target`
    * starting at index `where`, and answer how many were moved (limited by the
    * deque's size and the space in `target`).  No allocation, and never more than
    * O(blockSize * log n) time under the lock: when the destination is an array this
    * beats `splitLeft`, whose batch is only worth building if it will be consumed
    * *as* a batch (spliced onward, kept structured).
    */
  def popLeftInto(target: Array[A], where: Int, n: Int): Int =
    if where < 0 || where > target.length then throw new ArrayIndexOutOfBoundsException(where)
    var k = if n > target.length - where then target.length - where else n
    if k <= 0 then 0
    else (target: Any) match
      case t: Array[AnyRef] =>
        cutLeft(t, where, k) match
          case i: Int => i
          case e: SplitDequeImpl =>
            val c = e.count
            e.popLeftInto(t, where, c, Sentinel)
            c
      case _ => popSlowly(target, where, k, true)

  /** Remove up to n of the last elements, copying them *in order* (as splitRight
    * would: the deque's last element lands last) into `target` starting at index
    * `where`, and answer how many were moved (limited by the deque's size and the
    * space in `target`).
    */
  def popRightInto(target: Array[A], where: Int, n: Int): Int =
    if where < 0 || where > target.length then throw new ArrayIndexOutOfBoundsException(where)
    var k = if n > target.length - where then target.length - where else n
    if k <= 0 then 0
    else (target: Any) match
      case t: Array[AnyRef] =>
        cutRight(t, where, k) match
          case i: Int => i
          case e: SplitDequeImpl =>
            val c = e.count
            e.popRightInto(t, where, c, Sentinel)
            c
      case _ => popSlowly(target, where, k, false)


  // === Batch operations ===

  /** Remove the first n elements (all of them, if no more than n) into an independent
    * batch, in O(blockSize * log n) time.
    */
  def splitLeft(n: Int): Batch[A] =
    if n <= 0 then new Batch(new SplitDequeImpl(lgCap, blockSize, 0))
    else
      val m = acquire()
      val e = impl.splitLeft(n)
      release(m - e.count)
      new Batch(e)

  /** Remove the last n elements (all of them, if no more than n) into an independent
    * batch, in O(blockSize * log n) time.
    */
  def splitRight(n: Int): Batch[A] =
    if n <= 0 then new Batch(new SplitDequeImpl(lgCap, blockSize, 0))
    else
      val m = acquire()
      val e = impl.splitRight(n)
      release(m - e.count)
      new Batch(e)

  /** Insert a batch's entire contents before our first element, in O(blockSize * log n)
    * time, leaving the batch empty (but reusable).  The caller must own the batch: no
    * other thread may touch it during the splice.
    */
  def spliceLeft(donor: Batch[A]): Unit =
    val e = donor.impl
    if e.lgCap != lgCap || e.blockSize != blockSize then
      throw new IllegalArgumentException(s"geometry mismatch: batch ${e.lgCap}/${e.blockSize} vs $lgCap/$blockSize")
    val k = e.count
    if k > 0 then
      val m = acquire()
      if m.toLong + k > Int.MaxValue - 8 then DIE_FULL(m)
      impl.spliceLeft(e)
      release(m + k)

  /** Append a batch's entire contents after our last element, in O(blockSize * log n)
    * time, leaving the batch empty (but reusable).  The caller must own the batch: no
    * other thread may touch it during the splice.
    */
  def spliceRight(donor: Batch[A]): Unit =
    val e = donor.impl
    if e.lgCap != lgCap || e.blockSize != blockSize then
      throw new IllegalArgumentException(s"geometry mismatch: batch ${e.lgCap}/${e.blockSize} vs $lgCap/$blockSize")
    val k = e.count
    if k > 0 then
      val m = acquire()
      if m.toLong + k > Int.MaxValue - 8 then DIE_FULL(m)
      impl.spliceRight(e)
      release(m + k)


  // === Validation (test support) ===

  // Atomically checks every engine invariant plus shell/engine size agreement.
  private[kse] def validate(): String =
    val m = acquire()
    var err = impl.validate()
    if (err eq null) && impl.count != m then err = s"shell len $m but engine count ${impl.count}"
    release(m)
    err
}

object SplitDeque {
  import java.lang.invoke.MethodHandles

  // Production geometry: 64-slot rings, 24-wide blocks.  To be frozen to inline
  // constants only after soak-fuzzing at full size (see roadmap in tests).
  private[kse] val defaultLgCap = 6
  private[kse] val defaultBlockSize = 24

  private object Sentinel   // Stored in place of null elements (the engine uses null for vacancy)

  private[SplitDeque] final val lenHandle =
    MethodHandles.privateLookupIn(classOf[SplitDeque[?]], MethodHandles.lookup())
      .findVarHandle(classOf[SplitDeque[?]], "len", classOf[Int])

  private[SplitDeque] final val vznHandle =
    MethodHandles.privateLookupIn(classOf[SplitDeque[?]], MethodHandles.lookup())
      .findVarHandle(classOf[SplitDeque[?]], "vzn", classOf[Int])

  def empty[A]: SplitDeque[A] = new SplitDeque[A]()

  /** A non-concurrent deque with the same O(blockSize * log n) split and splice as
    * [[SplitDeque]], minus the locking: single-element operations are a couple of
    * array accesses, and draining or filling one costs no atomics at all.
    *
    * A batch is fully independent: it shares no structure with the deque it came
    * from or anything else, so it requires only the usual care of any
    * non-concurrent collection.  Splicing a batch (into a deque or another batch)
    * moves its contents wholesale, leaving it empty but valid for reuse.
    */
  final class Batch[A] private[loom] (private[loom] val impl: SplitDequeImpl) {

    def length: Int = impl.count

    def isEmpty: Boolean = impl.count == 0

    /** Add an element at the front. */
    def pushLeft(a: A): Unit =
      impl.pushLeft(if a.asInstanceOf[AnyRef] eq null then Sentinel else a.asInstanceOf[AnyRef])

    /** Add an element at the back. */
    def pushRight(a: A): Unit =
      impl.pushRight(if a.asInstanceOf[AnyRef] eq null then Sentinel else a.asInstanceOf[AnyRef])

    /** Take the first element, or Alt.unit if empty. */
    def popLeft(): A Or Unit =
      if impl.count == 0 then Alt.unit
      else
        val x = impl.popLeft()
        Is((if x eq Sentinel then null else x).asInstanceOf[A])

    /** Take the last element, or Alt.unit if empty. */
    def popRight(): A Or Unit =
      if impl.count == 0 then Alt.unit
      else
        val x = impl.popRight()
        Is((if x eq Sentinel then null else x).asInstanceOf[A])

    /** Remove up to n of the first elements, copying them in order into `target`
      * starting at index `where`, and answer how many were moved (limited by the
      * batch's size and the space in `target`).
      */
    def popLeftInto(target: Array[A], where: Int, n: Int): Int =
      if where < 0 || where > target.length then throw new ArrayIndexOutOfBoundsException(where)
      var k = if n > target.length - where then target.length - where else n
      if k > impl.count then k = impl.count
      if k <= 0 then 0
      else (target: Any) match
        case t: Array[AnyRef] =>
          impl.popLeftInto(t, where, k, Sentinel)
          k
        case _ =>
          var i = 0
          while i < k do
            val x = impl.popLeft()
            target(where + i) = (if x eq Sentinel then null else x).asInstanceOf[A]
            i += 1
          k

    /** Remove up to n of the last elements, copying them *in order* (as splitRight
      * would: the batch's last element lands last) into `target` starting at index
      * `where`, and answer how many were moved (limited by the batch's size and the
      * space in `target`).
      */
    def popRightInto(target: Array[A], where: Int, n: Int): Int =
      if where < 0 || where > target.length then throw new ArrayIndexOutOfBoundsException(where)
      var k = if n > target.length - where then target.length - where else n
      if k > impl.count then k = impl.count
      if k <= 0 then 0
      else (target: Any) match
        case t: Array[AnyRef] =>
          impl.popRightInto(t, where, k, Sentinel)
          k
        case _ =>
          var i = 0
          while i < k do
            val x = impl.popRight()
            target(where + k - 1 - i) = (if x eq Sentinel then null else x).asInstanceOf[A]
            i += 1
          k

    /** Remove the first n elements (all, if no more than n) into a new independent batch. */
    def splitLeft(n: Int): Batch[A] = new Batch(impl.splitLeft(n))

    /** Remove the last n elements (all, if no more than n) into a new independent batch. */
    def splitRight(n: Int): Batch[A] = new Batch(impl.splitRight(n))

    /** Move the donor's entire contents before our first element, leaving the donor empty. */
    def spliceLeft(donor: Batch[A]): Unit = impl.spliceLeft(donor.impl)

    /** Move the donor's entire contents after our last element, leaving the donor empty. */
    def spliceRight(donor: Batch[A]): Unit = impl.spliceRight(donor.impl)

    /** Visit every element in order. */
    def foreach(f: A => Unit): Unit =
      impl.foreachForward(x => f((if x eq Sentinel then null else x).asInstanceOf[A]))

    private[kse] def validate(): String = impl.validate()
  }
  object Batch {
    def empty[A](): Batch[A] = new Batch[A](new SplitDequeImpl(defaultLgCap, defaultBlockSize, 0))

    private[kse] def empty[A](lgCap: Int, blockSize: Int): Batch[A] = new Batch[A](new SplitDequeImpl(lgCap, blockSize, 0))
  }
}
