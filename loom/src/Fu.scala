// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024-26 Rex Kerr.

package kse.loom


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.util.concurrent.{Future, ExecutorService, ArrayBlockingQueue, SynchronousQueue, CompletableFuture}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.locks.Condition
import java.util.concurrent.TimeUnit
import java.time.Duration

import scala.util.boundary
import scala.util.boundary.Label
import scala.reflect.ClassTag

import kse.basics.*
import kse.flow.*
import kse.maths.*


opaque type Fu[A] = Future[Ask[A]]
object Fu {
  case class GroupExecutor(service: ExecutorService) {
    val failure: AtomicReference[Err Or Unit] = new AtomicReference(Alt.unit)
    def fail(err: Err): Unit =
      if failure.compareAndSet(Alt.unit, Is(err)) then
        service.shutdownNow() __ Unit
  }

  opaque type Executor = ExecutorService | GroupExecutor
  object Executor {
    def create(): kse.loom.Fu.Executor = java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor()
    def group(): kse.loom.Fu.Executor = GroupExecutor(java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor())
    extension (e: Executor)
      def unwrap: ExecutorService = e match
        case es: ExecutorService => es
        case GroupExecutor(es)   => es
      def stopIfGroup(): Unit = e match
        case GroupExecutor(es) => es.shutdownNow() __ Unit
        case _ =>
      def swapError(old: Err): Err = e match
        case g: GroupExecutor => g.failure.get.getOrElse(_ => old)
        case _ => old
  }

  val defaultExecutor = Executor.create()
  given Executor = defaultExecutor

  /** Adopt an externally-completed `CompletableFuture[Ask[A]]` as a `Fu[A]`, so a value produced
    * elsewhere (a promise, a reply slot) joins the `Fu` world with its `.?`/`await` machinery.
    * No executor is involved — completion is whoever holds the future's job. */
  def wrap[A](cf: CompletableFuture[Ask[A]]): kse.loom.Fu[A] = cf

  /** A completed `Fu[A]` carrying an already-known outcome (no work scheduled). */
  def of[A](result: Ask[A]): kse.loom.Fu[A] =
    val cf = new CompletableFuture[Ask[A]]()
    cf.complete(result) __ Unit
    cf

  inline def flat[A](using exec: Executor)(inline work: Label[Ask[A]] ?=> Ask[A]): kse.loom.Fu[A] = exec match
    case service: ExecutorService => service.submit(() => Ask.threadsafeFlat{ work })
    case g: GroupExecutor => g.service.submit(() => Ask.threadsafeFlat{ work }.peekAlt(g.fail))

  inline def apply[A](using exec: Executor)(inline work: Label[Ask[A]] ?=> A): kse.loom.Fu[A] = 
    val run: java.util.concurrent.Callable[Ask[A]] = () => Ask.threadsafe{ work }
    exec match
      case service: ExecutorService => service.submit(run)
      case g: GroupExecutor => g.service.submit(() => run.call().peekAlt(g.fail))

  inline def flatGroup[A](using exec: Executor)(inline makeWork: Executor ?=> Label[Ask[A]] ?=> Ask[A]): kse.loom.Fu[A] = exec match
    case service: ExecutorService =>
      service.submit(() => {
        val ex: Executor = Executor.group()
        try Ask.threadsafeFlat{ makeWork(using ex) }.mapAlt(e => Executor.swapError(ex)(e))
        finally Executor.stopIfGroup(ex)()
      })
    case g: GroupExecutor =>
      g.service.submit(() => {
        val ex: Executor = Executor.group()
        try Ask.threadsafeFlat{ makeWork(using ex) }.mapAlt(e => Executor.swapError(ex)(e)).peekAlt(g.fail)
        finally Executor.stopIfGroup(ex)()
      })

  inline def group[A](using exec: Executor)(inline makeWork: Executor ?=> Label[Ask[A]] ?=> A): kse.loom.Fu[A] = exec match
    case service: ExecutorService =>
      service.submit(() => {
        val ex: Executor = Executor.group()
        try Ask.threadsafe{ makeWork(using ex) }.mapAlt(e => Executor.swapError(ex)(e))
        finally Executor.stopIfGroup(ex)()
      })
    case g: GroupExecutor =>
      g.service.submit(() => {
        val ex: Executor = Executor.group()
        try Ask.threadsafe{ makeWork(using ex) }.mapAlt(e => Executor.swapError(ex)(e)).peekAlt(g.fail)
        finally Executor.stopIfGroup(ex)()
      })

  // === Waiting for answers that arrive from elsewhere, by key ===

  /** What became of an answer handed to a [[TimedMap]]: taken by its waiter; `Expired` for a key whose deadline had
    * passed first; `Withdrawn` for one given up on purpose (`cancel`, `giveUp`, or the map's `close`); or `Stray` for
    * a key the map never knew or has forgotten.  An answer that is not `Delivered` reached nobody, so whatever it owns
    * is the caller's to release; the difference is whether it was expected -- a withdrawn key's answer crossing the
    * withdrawal is ordinary, an expired one's says the other side was slow, a stray one's that something is amiss. */
  enum Delivery {
    case Delivered, Expired, Withdrawn, Stray
  }

  /** One answer being waited for: its `key`, and the answer as `fu` (`await()` and `.?` for short) -- `Some(v)` when
    * it arrives, `None` if it never does (the deadline passed, or someone gave up on it), or an error if the map was
    * closed with one.  `cancel()` gives up on it, as `fu.cancel()` does.  The entry is the future itself, so it costs
    * nothing beyond one object, and however it ends it leaves its map at that moment. */
  final class MapEntry[K, V] private[loom] (map: TimedMap[K, V] | Null, val key: K, private[loom] val deadline: Long)
  extends CompletableFuture[Ask[Option[V]]] {
    def fu: kse.loom.Fu[Option[V]] = this
    def await(): Ask[Option[V]] =
      try get()
      catch case e if e.catchable => Alt(Err(e))
    inline def ?[L >: Alt[Err]](using lb: boundary.Label[L]): Option[V] = kse.flow.?(await())(using lb)
    /** Give up on the answer; false once it had already ended. */
    def cancel(): Boolean = { val m = map; (m ne null) && m.retire(this, Is(None), Delivery.Withdrawn) }
    /** Waits no longer than the deadline in a map with no timer thread, ending the entry when it passes; so do
      * `get` with a timeout and `join`, which is what every wait on the entry comes to. */
    override def get(): Ask[Option[V]] =
      val m = map
      if (m eq null) || m.threaded then super.get()
      else
        val left = deadline - System.nanoTime
        try super.get(left max 0L, java.util.concurrent.TimeUnit.NANOSECONDS)
        catch case _: java.util.concurrent.TimeoutException => { m.sweep(); super.get() }
    override def get(timeout: Long, unit: java.util.concurrent.TimeUnit): Ask[Option[V]] =
      val m = map
      if (m eq null) || m.threaded then super.get(timeout, unit)
      else
        val asked = System.nanoTime + unit.toNanos(timeout)
        if asked - deadline < 0 then super.get(timeout, unit)
        else get()
    override def join(): Ask[Option[V]] =
      val m = map
      if (m eq null) || m.threaded then super.join()
      else
        try get()
        catch
          case e: java.util.concurrent.ExecutionException => throw new java.util.concurrent.CompletionException(e.getCause)
          case e: InterruptedException => { Thread.currentThread.interrupt(); super.join() }
    /** However a caller ends it -- `cancel`, `complete` -- it leaves the map, as the map's own endings do. */
    override def cancel(mayInterrupt: Boolean): Boolean = cancel()
    override def complete(value: Ask[Option[V]]): Boolean =
      val m = map
      if m eq null then super.complete(value) else m.retire(this, value, Delivery.Delivered)
    private[loom] def settle(value: Ask[Option[V]]): Boolean = super.complete(value)
  }

  /** A table of answers being waited for, each under a key the caller chooses, each with a deadline: `expect(key)`
    * gives a [[MapEntry]] whose `fu` the waiter holds, and `fulfil(key, value)` completes it when the answer arrives
    * from wherever answers come from (a socket, a pipe, another process).  Not being answered is ordinary, so an
    * entry that expires or is given up completes with `None`, and only closing the map with a cause is an error.  An
    * entry leaves the table the moment it ends, by any route, so the table holds exactly what is still awaited.
    *
    * Every entry has a deadline -- `within`, or the map's `patience`.  A `threaded` map enforces it with one virtual
    * thread that sleeps until the earliest and, each time it wakes, ends every entry whose deadline has passed, so a
    * thread that was held up clears the whole backlog at once.  An unthreaded one starts no thread: every call on the
    * map (`expect`, `fulfil`, `giveUp`, `pending`, and `sweep()` for a caller with a clock of its own) first ends
    * what is due, and a wait on an entry waits no longer than its deadline and then ends it; an entry nobody waits on
    * and nothing touches the map about ends at the next call.  Keys that ended unanswered are remembered (the most
    * recent `remember` of them) with how they ended, so an answer arriving for one is `Expired` or `Withdrawn`, not
    * `Stray`.  Sending the request is the caller's affair: the table is transport-free.  `onGiveUp`, if given, hears
    * each key its waiter stopped waiting for, with `Expired` or `Withdrawn` (what an answer for it would now get), for
    * a transport that tells the other side; closing the map tells it nothing, since the transport is ending too. */
  class TimedMap[K, V](patience: DoubleDuration, remember: Int = 1024, onGiveUp: (K, Delivery) => Unit = (_: K, _: Delivery) => (),
                       val threaded: Boolean = true) {
    protected val lock = new AnyRef
    private val entries = new java.util.concurrent.ConcurrentHashMap[K, MapEntry[K, V]]()
    // Deadlines, earliest first.  An entry that ends early stays here until its time comes, and is passed over.
    private val deadlines = new java.util.PriorityQueue[MapEntry[K, V]](16, (a, b) => java.lang.Long.compare(a.deadline, b.deadline))
    private val gaveUp = new java.util.LinkedHashMap[K, Delivery]()
    @volatile private var closedWith: Err Or Unit = Alt.unit   // the cause, once closed
    @volatile private var timer: Thread = null

    /** How many answers are still awaited. */
    def pending: Int = { if !threaded then sweep(); entries.size }

    /** End every entry whose deadline has passed.  An unthreaded map does this at every call; a threaded one's timer
      * does it, so calling it there only does early what is about to be done. */
    def sweep(): Unit =
      val now = System.nanoTime
      var due: List[MapEntry[K, V]] = Nil
      lock.synchronized:
        while !deadlines.isEmpty && deadlines.peek().deadline - now <= 0 do due = deadlines.poll() :: due
      due.foreach(e => retire(e, Is(None), Delivery.Expired) __ Unit)

    /** Wait for the answer under `key`, until `within` has passed; refused while `key` is already awaited, or once
      * the map is closed. */
    def expect(key: K, within: DoubleDuration = patience): MapEntry[K, V] Or Err =
      if !threaded then sweep()
      if closedWith.isIs then Alt(closedWith.getOrElse(_ => Err("closed")))
      else
        val e = new MapEntry[K, V](this, key, TimedMap.deadlineIn(within))
        if entries.putIfAbsent(key, e) ne null then Err.or(s"$key is already awaited")
        else { schedule(e); Is(e) }

    /** Hand over the answer for `key`. */
    def fulfil(key: K, value: V): Delivery =
      if !threaded then sweep()
      val e = entries.get(key)
      if (e ne null) && retire(e, Is(Some(value)), Delivery.Delivered) then Delivery.Delivered
      else
        val how = lock.synchronized(gaveUp.remove(key))
        if how == null then Delivery.Stray else how

    /** Stop waiting for `key`, as its entry's `cancel()` does; false if it was not awaited. */
    def giveUp(key: K): Boolean =
      if !threaded then sweep()
      val e = entries.get(key)
      (e ne null) && retire(e, Is(None), Delivery.Withdrawn)

    /** Fail every awaited answer with `why`, refuse new ones, stop the timer; answers how many were failed.  Their
      * keys are remembered as `Withdrawn`. */
    def close(why: Err): Int =
      closedWith = Is(why)
      var n = 0
      entries.values.forEach(e => if retire(e, Alt(why), Delivery.Withdrawn, tell = false) then n += 1)
      val t = timer
      if t ne null then t.interrupt()
      n

    /** End `e` with `value` if it is still awaited: it leaves the table first, so exactly one ending wins.  `how` is
      * `Delivered` for an answer, else how it ended unanswered, which is remembered and, if `tell`, told. */
    private[loom] def retire(e: MapEntry[K, V], value: Ask[Option[V]], how: Delivery, tell: Boolean = true): Boolean =
      entries.remove(e.key, e) && {
        if how != Delivery.Delivered then
          lock.synchronized:
            this.gaveUp.put(e.key, how) __ Unit
            if this.gaveUp.size > remember then this.gaveUp.remove(this.gaveUp.keySet.iterator.next()) __ Unit
          if tell then
            try onGiveUp(e.key, how) catch case t if t.catchable => ()
        e.settle(value) __ Unit
        true
      }

    protected def isPending(key: K): Boolean = entries.containsKey(key)
    protected def closedCause: Err Or Unit = closedWith
    protected def adopt(e: MapEntry[K, V]): Boolean = (entries.putIfAbsent(e.key, e) eq null) && { schedule(e); true }

    private def schedule(e: MapEntry[K, V]): Unit =
      val wake = lock.synchronized:
        deadlines.add(e) __ Unit
        deadlines.peek() eq e
      if threaded then
        if timer eq null then startTimer()
        else if wake then java.util.concurrent.locks.LockSupport.unpark(timer)

    private def startTimer(): Unit = lock.synchronized:
      if (timer eq null) && closedWith.isAlt then
        timer = Thread.ofVirtual().name("Fu.TimedMap timer").start(() => expire())

    // Each wake ends EVERY entry past its deadline, not just the first, so a starved timer catches up in one pass.
    private def expire(): Unit =
      while closedWith.isAlt do
        val now = System.nanoTime
        var due: List[MapEntry[K, V]] = Nil
        val next = lock.synchronized:
          while !deadlines.isEmpty && deadlines.peek().deadline - now <= 0 do due = deadlines.poll() :: due
          if deadlines.isEmpty then TimedMap.Idle else deadlines.peek().deadline - now
        due.foreach(e => retire(e, Is(None), Delivery.Expired) __ Unit)
        java.util.concurrent.locks.LockSupport.parkNanos(next)
        Thread.interrupted() __ Unit   // a stray interrupt must not turn the sleep into a spin; only a close ends it
  }
  object TimedMap {
    // How long an idle timer sleeps before looking again; any new earliest deadline wakes it sooner.
    private[loom] final val Idle = 60_000_000_000L
    private[loom] def deadlineIn(within: DoubleDuration): Long =
      val s = within.unwrap
      val ns = if !(s > 0) then 0L else if s >= 9.2e9 then Long.MaxValue / 4 else (s * 1e9).toLong
      System.nanoTime + ns
  }

  /** A [[TimedMap]] that chooses the keys: `grant()` takes the next from `zero` by `next`, passing over any still
    * awaited, so a key comes round again only once its last use has ended.  If every key `next` reaches is in use,
    * the grant fails at once. */
  class GrantMap[K, V](zero: K)(next: K => K)(patience: DoubleDuration, remember: Int = 1024, onGiveUp: (K, Delivery) => Unit = (_: K, _: Delivery) => (),
                       threaded: Boolean = true)
  extends TimedMap[K, V](patience, remember, onGiveUp, threaded) {
    private var cursor = zero

    /** Wait for an answer under a fresh key, until `within` has passed: the entry says which key to send it under. */
    def grant(within: DoubleDuration = patience): MapEntry[K, V] =
      if !threaded then sweep()
      val deadline = TimedMap.deadlineIn(within)
      if closedCause.isIs then failed(zero, deadline, closedCause.getOrElse(_ => Err("closed")))
      else
        val e = lock.synchronized:
          var k = cursor
          var tries = 0
          while isPending(k) && tries <= pending do { k = next(k); tries += 1 }
          cursor = next(k)
          if isPending(k) then null
          else new MapEntry[K, V](this, k, deadline)
        if (e eq null) then failed(zero, deadline, Err("every key is awaited"))
        else if !adopt(e) then failed(e.key, deadline, Err(s"${e.key} is already awaited"))
        else e

    private def failed(k: K, deadline: Long, why: Err): MapEntry[K, V] =
      val e = new MapEntry[K, V](null, k, deadline)
      e.settle(Alt(why)) __ Unit
      e
  }

  /** A [[GrantMap]] whose keys are `Long`s counting up from `zero`: request ids. */
  class IdMap[V](zero: Long = 0L)(patience: DoubleDuration, remember: Int = 1024, onGiveUp: (Long, Delivery) => Unit = (_: Long, _: Delivery) => (),
                 threaded: Boolean = true)
  extends GrantMap[Long, V](zero)(_ + 1)(patience, remember, onGiveUp, threaded)


  extension [A](fu: Fu[A]) {
    def isComplete: Boolean = (fu: Future[Ask[A]]).isDone

    def await(): Ask[A] =
      try (fu: Future[Ask[A]]).get()
      catch case e if e.catchable => Alt(Err(e))

    def await(timeout: DoubleDuration): A Or Option[Err] =
      import java.util.concurrent.TimeUnit
      var t_value = 0L
      var t_unit =
        val dt = timeout.unwrap
        if dt <= 0 then java.util.concurrent.TimeUnit.NANOSECONDS
        else if dt < 1e-1 then
          t_value = (dt*1e6).rint.toLong
          TimeUnit.MICROSECONDS
        else if dt < 1e6 then
          t_value = (dt*1e3).rint.toLong
          TimeUnit.MILLISECONDS
        else
          t_value = dt.rint.toLong
          TimeUnit.SECONDS
      try (fu: Future[Ask[A]]).get(t_value, t_unit).mapAlt(e => Some(e))
      catch
        case t: java.util.concurrent.TimeoutException => Alt(None)
        case e if e.catchable => Alt(Some(Err(e)))

    def cancel(): Boolean = (fu: Future[Ask[A]]).cancel(true)

    inline def ?[L >: Alt[Err]](using lb: Label[L]): A = kse.flow.?[A, Err](Fu.await(fu)())(using lb)
    inline def ?+[E, L >: Alt[E]](inline f: Err => E)(using lb: Label[L]): A = kse.flow.?+(Fu.await(fu)())(f)(using lb)
    inline def ?*[E, L >: Alt[E]](using lb: Label[L], m: Err AutoMap E): A = kse.flow.?*[A, Err](Fu.await(fu)())(using lb, m)
    inline def ?#[L >: Alt[Err]](inline msg: String)(using lb: Label[L]): A = kse.flow.?#(Fu.await(fu)())(msg)(using lb)

    inline def map[B](using exec: Executor)(inline f: Label[B Or Err] ?=> (A => B)): kse.loom.Fu[B] = kse.loom.Fu.apply[B]:
      val result = kse.flow.?(Fu.await(fu)())
      f(result)

    inline def flatMap[B](using exec: Executor)(inline f: Label[B Or Err] ?=> (A => (B Or Err))): kse.loom.Fu[B] = kse.loom.Fu.flat[B]:
      val result = Fu.await(fu)()
      kse.flow.flatMap(result)(x => f(x))
  }
}

extension [A](a: Array[kse.loom.Fu[A]]) {
  def allFu()(using exec: Fu.Executor, tag: ClassTag[Ask[A]]): kse.loom.Fu[Array[Ask[A]]] = Fu:
    a.copyWith(fu => Fu.await(fu)())

  def fu()(using exec: Fu.Executor, tag: ClassTag[A]): kse.loom.Fu[Array[A]] = Fu.flat:
    var b: scala.collection.mutable.ArrayBuffer[Err] = null
    val v = new Array[A](a.length)
    a.visit(){ (x, i) =>
      Fu.await(x)().fold(v(i) = _){ e =>
        if b eq null then b = scala.collection.mutable.ArrayBuffer.empty[Err]
        val _ = b += e
      }
    }
    if b ne null then
      if b.length == 1 then Alt(b.head)
      else Alt(Err(ErrType.Many(b)))
    else Is(v)

  inline def fuMap[B](using exec: Fu.Executor)(inline f: Label[B Or Err] ?=> (A => B)): Array[kse.loom.Fu[B]] =
    a.copyWith(_ map f)

  inline def fuFlatMap[B](using exec: Fu.Executor)(inline f: Label[B Or Err] ?=> (A => Ask[B])): Array[kse.loom.Fu[B]] =
    a.copyWith(_ flatMap f)
}



/** A thread with a result: `await()` answers what the body produced, or the error it failed with -- an
  * interruption included, reported rather than swallowed, since a `Threaded` owns no scope that could act on
  * it.  Nothing leaves the thread without a result being published, so `await()` cannot hang. */
class Threaded[A] private (f: () => A) extends Thread {
  private val result = new java.util.concurrent.CompletableFuture[Ask[A]]()
  override def run(): Unit =
    try result.complete(threadnice(f())) __ Unit
    catch case e: InterruptedException => result.complete(Alt(Err(e))) __ Unit
    finally result.complete(Alt(Err("thread ended without a result"))) __ Unit   // only if nothing above did: a fatal error is leaving
  def await(): Ask[A] =
    try result.get()
    catch case e if e.catchable => Alt(Err(e))
  def isComplete: Boolean = result.isDone
}
object Threaded {
  def apply[A](a: => A): Threaded[A] =
    val th = new Threaded(() => a)
    th.start()
    th
}


opaque type Sync = ReentrantLock
object Sync:
  def apply(): Sync = new ReentrantLock()
  def fair(): Sync = new ReentrantLock(true)
  extension [S <: Sync & Singleton](sync: S)
    def unwrap: ReentrantLock = sync
    inline def apply[A](inline f: S ?=> A): A =
      (sync: ReentrantLock).lockInterruptibly()
      try f(using sync)
      finally (sync: ReentrantLock).unlock()
    inline def uninterrupted[A](inline f: S ?=> A): A =
      (sync: ReentrantLock).lock()
      try f(using sync)
      finally (sync: ReentrantLock).unlock()
    inline def ifFree[A](inline f: S ?=> A): A Or Unit =
      if (sync: ReentrantLock).tryLock() then
        try Is(f(using sync))
        finally (sync: java.util.concurrent.locks.ReentrantLock).unlock()
      else Alt.unit
    inline def waiter(): Wait[S] = (sync: ReentrantLock).newCondition()
  
  opaque type Wait[S <: Sync & Singleton] = Condition
  object Wait:
    extension [S <: Sync & Singleton, W <: Wait[S]](wait: W)
      def unwrap: Condition = wait
      inline def ! : Unit = (wait: Condition).signal()
      inline def !!! : Unit = (wait: Condition).signalAll()
      inline def apply(nd: NanoDuration)(using sync: S): Boolean = (wait: Condition).await(nd.unwrap, TimeUnit.NANOSECONDS)
      /** A `Duration` beyond what nanoseconds can hold waits the longest they can, rather than overflowing:
        * `1e9.days.duration` is an explicit very long wait, which is what it should be. */
      inline def apply(d: Duration)(using sync: S): Boolean = (wait: Condition).await(d.nano.unwrap, TimeUnit.NANOSECONDS)
