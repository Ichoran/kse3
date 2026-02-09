// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024-25 Rex Kerr and UCSF (Kato Lab).


package kse.loom

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, CountDownLatch, LinkedTransferQueue, Semaphore}
import java.time.Duration

import scala.collection.mutable.TreeMap
import scala.reflect.ClassTag
import scala.util.boundary
import scala.NamedTuple.{NamedTuple => NTup}

import kse.basics.*
import kse.basics.intervals.*
import kse.basics.labels.*
import kse.flow.*


/** Class to organize a multithreaded processing operation.
  * 
  * To define a specific processing operation, you inherit from `Percolate`.
  * 
  * Actions that need to be taken inherit from `Work` and define the work to be done
  * and produce follow-on work.  The upside is that arbitrarily complex looping or recursive
  * operations are possible.  The downside is that the computation graph is implicit in
  * the data structures.
  * 
  * This can be a worthwhile tradeoff because simple computations are still not that hard to
  * inspect; and complex computations are often difficult to express.
  * 
  * Forking is trivial using this model; if data needs to be joined, the preferred method is
  * to accumulate data sequentially if pairing is needed; or special-purpose accumulators if
  * arbitrary pairing, batching, or sorting is needed.
  * 
  * Work is placed on a default queue that is consumed by a predefined number of worker threads.
  * It is also possible to schedule work to go to custom queues, which may optionally have a
  * dedicated thread that handles the contents.  This is the mechanism by which access to single-
  * threaded resources should be guarded.
  * 
  * Work is initally created via a `newWork` method that runs on the main thread; this work
  * is assumed to be sufficiently heavyweight that the amount needs to be guarded by a sempahore
  * which is automatically tracked as the work is distributed; if the semaphores are
  * exhausted, then no new work will be created until either something completes and releases
  * a work permit, or all work is in accumulating work queues which have not reached their
  * trigger condition, in which case one additional permit will be granted.
  * 
  * When new work is exhausted, the newWork method should emit `Work.Stop` as additional work.
  * If, instead, no new work can be created until something else happens, it should emit
  * `Work.NoOp` instead.
  * 
  * Note that work permits are the only backpressure mechanism.  Permits are inherited as
  * one work item creates additional follow-up work until all follow-ups report completion
  * (or create work which is detatched from a permit).
  * 
  * `Percolate` is intended only for succeed-or-fail operations.  For complex long-running
  * concurrent tasks with cancellation and error-recovery, a full-fledged framework for
  * concurrent processing is recommended.  However, for many tasks, this simpler model is
  * more straightforward and possibly lower-overhead.
  */

/*
abstract class Percolate(parallelism: Int, maxPermits: Int | (Int => Int))(using Percolate.Runner) {
  import Percolate.*

  protected val inflight = Atom(1)
  protected val complete = Atom(0L)

  protected val uncategorized_t = Atom(0L \ "ns")
  

  private val myPermits = new Semaphore(
    maxPermits match
      case i: Int => 1 max i
      case f: (Int => Int) => 1 max f(parallelism max 0)
  )
  final class Permit(val id: Long, count: Atom[Int]) {
    def another(): this.type Or Unit =
      val newCount = count.zapAndGet(n => if n > 0 then n + 1 else 0)
      if newCount > 0 then Is(this) else Alt.unit

    def retire(): Unit =
      val oldCount = count.getAndZap(n => if n > 1 then n - 1 else 0)
      if oldCount == 1 then myPermits.release()
  }
  object Permit {
    private val id = Atom(0L)

    def get(): Permit Or Unit =
      val got = myPermits.tryAcquire()
      if got then Is(new Permit(id.zapAndGet(_ + 1), Atom(1)))
      else Alt.unit
  }


  abstract class Todo[+W <: Work](val title: String)(using Todo.Registry) {
    final val liveness: Atom[Int] = Atom(1)
    final val completed = new CountDownLatch(1)
    val agency: Todo.Agency
    def take(): W Or Boolean
    def close(): Boolean
    final inline def alive(): Boolean = liveness() > 0
    def onStart(): Ask[Unit] = Is.unit
  }
  object Todo {
    opaque type Registry = Unit

    opaque type Agency = Int
    object Agency:
      val Default: Agency = 0
      val Core: Agency = 1
      val Solo: Agency = 2
      extension (a: Agency)
        def +(aa: Agency): Agency = ((a: Int) | (aa: Int)): Agency
        def in(aa: Agency): Boolean = ((a: Int) & (aa: Int)) == (a: Int)
        def has(aa: Agency): Boolean = ((a: Int) & (aa: Int)) == (aa: Int)

    val all = Atom[List[Todo[Work]]](Nil)
    val byWorkers = Atom[List[Todo[Work]]](Nil)
    val byCore = Atom[List[Todo[Work]]](Nil)

    def register[W <: Work, T <: Todo[W]](f: Registry ?=> T): T =
      val todo = f(using (): Registry)
      all.zap(todo :: _)
      if todo.agency has Agency.Core then byCore.zap(todo :: _)
      if !(todo.agency has Agency.Solo) then byWorkers.zap(todo :: _)

    abstract class For[W <: Work](title: String)(using reg: Todo.Registry) extends Todo[W](title)(using reg) {
      def put(w: W): Ask[Boolean]
    }

    final class General(title: String)(using reg: Todo.Registry) extends For[Work](title)(using reg) {
      val agency = Agency.Core
      val content = new ConcurrentLinkedQueue[Work]()
      def take(): Work Or Boolean =
        if liveness() > 0 then
          content.poll() match
            case null => Alt.T
            case w => Is(w)
        else
          content.poll() match
            case null => Alt.F
            case w => Is(w)
      def close(): Boolean =
        alive.zap(x => if (x & 1) == 1 then x - 1 else x)
        true
      def put(w: Work): Ask[Boolean] =
        if liveness.zapAndGet(x => if x > 0 then x + 2 else x) > 0 then
          content.put(w)
          liveness.zap(_ - 2)
          Is.T
        else Is.F
    }

    class Resourced[R, W <: Work](title: String, dedicatedThread: Boolean = true)(opener: () => Ask[R])(closer: R => Ask[Unit], shutdownHook: Option[R => Unit] = None)(using reg: Todo.Registry)
    extends For[W](title)(using reg) {
      val agency = if dedicatedThread then Agency.Solo else Agency.Solo + Agency.Core
      val content = new ConcurrentLinkedQueue[W]()
      val resource: Atom[Option[R]] = Atom(None)
      val 
    }
  }

  val todo = Todo.register:
    Todo.General("main queue")


  abstract class Work protected (final val title: String, final val timer: Atom[Long \ "ns"] = uncategorized_t) {
    final val permit: Atom[Option[Permit]] = Atom(None)
    final val hooks: Atom[Option[() => Ask[Unit], () => Ask[Unit]]] = Atom(None)
    final val status = Atom(0)
    def queue(): Todo.For[? >: this.type] = todo
  }


  protected val errors: Atom[List[Err]] = Atom(Nil)
  def errored(from: Work, e: Err): Unit =
    errors.zap(e :: _)
    from.cancel(depermitOnly = true)



  abstract class Todo[W <: Work]


  /** Tries to do some work, but only if resources are available.
   * 
   * The usual executors for work will increment `ran` upon successful complete, or change it to -1-n in case of error.
   * 
   * The usual executors will also halt the program in case of error. It is possible to resubmit the same work, but the
   * typical workflow is to create new work rather than reuse an old one.  However, it is possible to iterate on the same
   * batch of work by having the item resubmit itself if it isn't yet complete.
   */
  abstract class Work(
    final val inheritsPermit: Boolean = true,
    final val beforeWork: Option[() => Unit] = None,
    final val afterWork: Option[() => Unit] = None,
    final val timer: Atom[Long \ "ns"] = uncategorized_t
  ) {
    final val ran: Atom[Int] = Atom(0L)
    final val permit: Atom[Option[Permit]] = Atom(None)
    def queue:
    def desc: String = s"$Work item $toString"
    def work(): Ask[Array[Work]]
    def and(more: => Work): And = And(this, more)
    def enqueueSelf: Ask[Boolean] = Is(false)
    def cancel(depermitOnly: Boolean = false): Unit =
      permit().foreach(_.stop())
      incomplete.--
      if !depermitOnly then canceled.++
  }
  object Work {
    val none = Array.empty[Work]

    val queues = new LinkedTransferQueue[Q[?]]

    sealed trait Reg {}
    private val regInstance: Reg = new Reg {}
    def enqueue[W >: Null <: Work, K <: Q[W]](q: Reg ?=> K): K =
      val ans = q(using regInstance)
      queues.put(ans)
      ans

    class Q[W >: Null <: Work]() {
      /** The underlying queue for this type of work. */
      val queue: LinkedTransferQueue[W] = new LinkedTransferQueue[W]()

      /** Adds a new work item. */
      def put(w: W): Unit = queue.put(w)

      /** Removes a work item if one is available; returns null otherwise */
      def poll(): W = queue.poll()


      /** Blocks until a work item is available. */
      def take(): W = queue.take()
    }

    final class RQ[R <: AnyRef](
      val init: () => Ask[R],
      val close: R => Ask[Array[Work]],
      val isClosed: R => Boolean,
      val onError: R => Unit,
      needsShutdownHook: Boolean = false
    ) extends Q[WorkWith[R]]() {
      final val worker: Atom[Option[WorkerWith[R]]] = Atom(None)
      final val resource: Atom[Resource.Type[R]] = Atom(Resource.Uninitialized)
      private val emergencyThread: Atom[Thread] = Atom(null)

      private def thereWasNoEmergency(): Unit =
        emergencyThread.swap(null) match
          case null =>
          case th => threadsafe{ java.lang.Runtime.getRuntime.removeShutdownHook(th) }: Unit

      private def backoff(fatigue: Int): Int =
        val f = fatigue & 0x7FFFFFFF
        if (f & 0x3FF) < 0xF then f+1
        else if (f & 0x3FF) != 0x3FF then
          Thread.onSpinWait()
          f + 1
        else if (f & 0xFC00) != 0xFC00 then
          Thread.`yield`
          f + 0x400
        else if (f & 0xFF0000) != 0xFF0000 then
          Thread.sleep(((f >> 16) + 8) >> 4)
          f + 0x10000
        else Int.MaxValue

      /** Checks whether a resource is open.  Should be treated as advisory (for optimization) only, because the state may change after the result is returned.
        * 
        * If the resource is being modified or used, it is indeterminate whether it is open or closed, so `None` is returned.
        */
      def isOpen: Option[Boolean] =
        resource() match
          case Resource.Unknown => None
          case Resource.Completed | Resource.Uninitialized => Some(false)
          case _ => Some(true)

      /** Opens a resource.  Intended to be used only when there is no contention at startup; most other use will cause an error.  Contention for the resource is handled nonetheless. */
      @annotation.tailrec
      def openResource(fatigue: Int = 0): Ask[Unit] =
        resource.swap(Resource.Unknown) match
          case Resource.Unknown =>
            val f2 = backoff(fatigue | 0xC00000)
            if f2 == Int.MaxValue then Err.or(s"Resource contention could not be resolved; cannot open resource")
            else openResource(f2)
          case Resource.Uninitialized => Ask.threadsafe:
            resource := Resource.Borrowed  // Mark status quickly in case init takes a while
            var rr: Resource.Type[R] = Resource.Uninitialized
            try
              val x = init().?
              if needsShutdownHook then
                val th = new Thread(() => threadsafe{ onError(x) } __ Unit)
                java.lang.Runtime.getRuntime.addShutdownHook(th)
                emergencyThread := th
              rr = Resource.Active(x)
            finally while !resource.cas(Resource.Borrowed, rr) do {}   // Wait for our `Borrowed` to be put back if someone took it
          case r =>
            resource := r   // Put back whatever it was
            Err.or("Only inactive, uninitialized resource queues may be opened")

      /** Closes a resource.  Closing a closed resource is permitted, but only the first close can return additional work.  Closing an unopened resource is an error, however.
       * 
        * If the resource is busy, it implements a contention-handling strategy that will eventually fail to an error if the resource cannot be obtained.
        */
      @annotation.tailrec
      def closeResource(fatigue: Int = 0): Ask[Array[Work]] =
        resource.swap(Resource.Unknown) match
          case Resource.Unknown =>
            val f2 = backoff(fatigue)
            if f2 == Int.MaxValue then Err.or(s"Resource contention could not be resolved; cannot close resource")
            else closeResource(f2)
          case Resource.Borrowed =>
            resource := Resource.Borrowed   // Put back marker
            val f2 = backoff(fatigue | 0x3F0)
            if f2 == Int.MaxValue then Err.or(s"Resource contention could not be resolved; cannot close resource")
            else closeResource(f2)
          case Resource.Uninitialized =>
            resource := Resource.Uninitialized
            Err.or("Cannot close unopened resource")
          case Resource.Completed =>
            resource := Resource.Completed
            Is(none) // We already closed or are closing the resource.
          case r: Resource.Active[R] =>
            resource := Resource.Completed       // Just put the close marker in right away--nobody else is going to get to use this
            Ask.threadsafe{
              (if isClosed(r.active) then none else close(r.active).?).tap{ _ => thereWasNoEmergency() }
            }.mapAlt{ e =>
              threadnice{ onError(r.active) }.
                fold{ _ => thereWasNoEmergency(); e }{ ee => Err(e, ee)("Error while handling error during closing") }
            }

      /** Use a resource if it is available.
        *
        * If the resource is being used by some other thread, the `Ask` will  be `Is.unit`.  In case of error, the `Ask` will be an `Alt[Err]` as normal.
        * If the resource is available and the operation succeeds, the value will be returned.
        * 
        * If the resource is being actively queried but not used, a sensible waiting strategy is adopted;
        * if the resource never attains some definite status after a while, an error will be returned.
        */
      @annotation.tailrec
      def wield[A](f: R => Ask[A], fatigue: Int = 0): A Or Ask[Unit] =
        resource.swap(Resource.Unknown) match
          case Resource.Unknown =>
            val f2 = backoff(fatigue)
            if f2 == Int.MaxValue then Alt(Err.or("Resource contention could not be resolved; cannot use resource"))
            else wield(f, f2)
          case Resource.Borrowed =>
            resource := Resource.Borrowed   // Put back marker; whoever marked the borrow will wait for it
            Alt(Is.unit)  // Nothing happened because resource wasn't free; it's the caller's responsibility to busy-wait if that's what they want
          case Resource.Uninitialized =>
            resource := Resource.Uninitialized  // Put back marker
            Alt(Err.or("Uninitialized resource"))
          case Resource.Completed =>
            resource := Resource.Completed  // Put back marker
            Alt(Err.or("Resource already closed"))
          case r: Resource.Active[R] =>
            resource := Resource.Borrowed  // Mark as borrowed because we might take a while
            var rr: Resource.Type[R] = Resource.Completed
            try
              val x = r.active
              Ask.threadsafe{
                val a = f(x).?
                rr =
                  if isClosed(x) then
                    thereWasNoEmergency()
                    queue.put(WorkWith.Close[R](this))
                    Resource.Completed
                  else r
                a
              }.mapAlt{ e =>
                val ee = threadnice{ onError(r.active) }.
                  fold{ _ => thereWasNoEmergency(); e }{ y => Err(e, y)("Error while handling error during resource use") }
                Alt(e)
              }
            finally while !resource.cas(Resource.Borrowed, rr) do {}   // Set new status in place of our Borrowed marker
    }
  }

  protected val worklist = new Work.Q[Work]()


  abstract class WorkWith[R <: AnyRef](val rq: Work.RQ[R], inheritsPermit: Boolean = true, beforeWork: Option[() => Unit] = None, afterWork: Option[() => Unit] = None)
  extends Work(inheritsPermit, beforeWork, afterWork) {
    override def enqueueSelf: Ask[Boolean] = Ask:
      rq.put(this)
      true
  }
  object WorkWith {
    final class Close[R <: AnyRef](queue: Work.RQ[R]) extends WorkWith[R](queue, inheritsPermit = false) {
      def work(): Ask[Array[Work]] = Err.or("Close token is not executable work")
    }
  }

  object Stop extends Work(inheritsPermit = false) {
    def work(): Ask[Array[Work]] = Is(Work.none)
  }

  object NoOp extends Work(inheritsPermit = false) {
    def work(): Ask[Array[Work]] = Is(Work.none)
  }

  final class And(w: Work, more: => Work) extends Work() {
    def work(): Ask[Array[Work]] = threadnice:
      Array(w, more)
  }

  final class Lots(ws: Array[() => Work]) extends Work() {
    def work(): Ask[Array[Work]] = threadnice:
      ws.copyWith(_())
  }

  /*
  protected val corework = new LinkedTransferQueue[Work]()
  */

  /** Creates new work from nothing, or returns `Stop` if there is no more work that can be created. */
  def newWork(): Ask[Work]

  /** Sets up anything needed; if there is an error it must dispose of any resources acquired before returning */
  def setup(): Ask[Unit]

  /** Releases resources when done _or_ in case of error; should only return `Err` if there is an error in the release process */
  def teardown(): Ask[Unit]

  def todone(from: Work, ws: Array[Work]): Ask[Unit] = Ask:
    ws.visit(): (x, _) =>
      incomplete.++
      if x.inheritsPermit then
        from.permit() match
          case sp @ Some(p) =>
            p.again()
            x.permit := sp
          case _ =>
      if !x.enqueueSelf.? then worklist.put(x)
    from.permit().foreach(_.stop())
    complete.++
    incomplete.--

  abstract class AbstractWorker[W >: Null <: Work, Q <: Work.Q[W]](jobs: Q) extends Thread {
    val elapsed = Atom(0L)

    def doWork(w: W): Array[Work] Or Err = Ask:
      w.beforeWork.foreach(_())
      val ans = w.work().?
      w.afterWork.foreach(_())
      ans

    def isStop(w: W): Boolean

    def runloop(): Unit =
      var w: W = null
      while { w = jobs.take(); !isStop(w) } do
        if errors().nonEmpty then w.cancel()
        else
          val t = System.nanoTime
          val result = threadnice:
            doWork(w)
          .flatten
          val dt = System.nanoTime - t
          elapsed.zap(_ + dt)
          w.timer.zap(_.valueOp(_ + dt))
          result.flatMap(ws => todone(w, ws)).foreachAlt(e => errored(w, e))

    override def run(): Unit =
      threadnice{ runloop() }.foreachAlt(e => errors.zap(e :: _))
  }

  final class Worker(jobs: Work.Q[Work]) extends AbstractWorker[Work, Work.Q[Work]](jobs) {
    def isStop(w: Work) = w match
      case Stop => true
      case _ => false
  }

  final class WorkerWith[R <: AnyRef](jobs: Work.RQ[R]) extends AbstractWorker[WorkWith[R], Work.RQ[R]](jobs) {
    def isStop(w: WorkWith[R]) = w match
      case _: WorkWith.Close[_] => true
      case _ => false
  }

  protected val workers = new Array[Worker](parallelism max 0)
  protected val stopped = Atom(0)

  def startup(): Ask[Unit] = Ask:
    workers.set(): () =>
      Worker(worklist).tap(_.start())
    iFor(Work.queues.iterator): (q, _) =>
      q match
        case rq: Work.RQ[?] =>
          rq.openResource().?
          rq.worker().foreach(_.start())

  protected def stopworkers(): Unit =
    var caught = 0
    while caught < workers.length do
      worklist.queue.drainWith(_ __ Unit)
      ((workers.length - caught) + 1).times:
        worklist.put(Stop)
      workers.visit(caught to End): (worker, i) =>
        worker.join(10)
        if !worker.isAlive() then
          caught += 1
          if i > caught then
            val temp = workers(caught-1)
            workers(caught-1) = worker
            workers(i) = temp
    iFor(Work.queues.iterator): (q, _) =>
      q match
        case rq: Work.RQ[?] =>
          rq.worker().foreach: w =>
            if w.isAlive then
              rq.queue.drainWith(_ __ Unit)
              rq.put(WorkWith.Close(rq))
              w.join(2000L)
              if w.isAlive then
                w.interrupt()
                Thread.sleep(100L)
          if rq.isOpen.forall(_ == true) then
            rq.closeResource().foreachAlt(e => errors.zap(e :: _))


  /*
  def shutdown(): (Long \ "ns") Or Err =
    stopworkers()
    var elapsed = 0L
    workers.visit(): (worker, _) =>
      elapsed += worker.elapsed()
    teardown().map(_ => elapsed \ "ns")


  def step(): Boolean Or Err = Or.Ret:
    var didCore: Boolean = false
    corework.drainWith:
      case Stop => Err.break(s"Forced stop of processing loop")
      case w =>
        threadnice:
          w.touch.foreach(_())
          w.work().foreachThem(ws => todone(w, ws))(e => errored(w, e))
        .?
        didCore = true
    val didWork: Boolean =
      if obtainPermit() then
        newWork().? match
          case Stop =>
            returnPermit()
            if incomplete() == 0 then false
            else
              if corework.isEmpty() then Thread.`yield`
              true
          case w =>
            w.permit := Some(Permit())
            incomplete.++
            if w.isCore then corework.put(w) else worklist.put(w)
            true
      else worklist.poll() match
        case null =>
          Thread.`yield`
          true
        case Stop => Err.break("Forced stop of processing loop")
        case w =>
          threadnice:
            w.touch.foreach(_())
            w.work().foreachThem(ws => todone(w, ws))(e => errored(w, e))
          .?
          true
    didCore || didWork

  def go(): (Long \ "ns_wall", Long \ "ns_workers") Or Err = Ask:
    threadnice(setup()).flatten.?
    threadnice(startup())
      .flatten
      .flatMapAlt: e =>
        stopworkers()
        teardown()
      .?
    var elapsed = 0L
    threadnice{
      val t = System.nanoTime
      while step().fold(__){ e => errors.op(e :: _); false } && errors().isEmpty do {}
      elapsed = System.nanoTime - t
    }.foreachAlt(e => errors.op(e :: _))
    var welapsed = 0L
    threadnice{
      shutdown().tap(_.foreach(welapsed += _ ~ "ns"))
    }.flatten.foreachAlt(e => errors.op(e :: _))

    runOnErrorExit.drainWith: erun =>
      threadnice{
        erun().foreachAlt(ee => errors.op(ee :: _))
      }.foreachAlt(e => errors.op(e :: _))

    val wcanceled = canceled()
    errors() match
      case Nil => (elapsed \ "ns_wall", welapsed \ "ns_workers")
      case e :: Nil if wcanceled == 0 => Alt.break(e)
      case es =>
        val all = (if wcanceled == 0 then es else Err(s"Canceled $wcanceled work items after detection of error") :: es).toVector.reverse
        Alt.break(Err(ErrType.Many(all)))
  */
}
*/
object Percolate {
  /*
  /** Contains a resource managed for single-thread access in a multi-thread context.
    *
    * The contract is: when you want to use the resource, swap in `Unknown`.  If you
    * get back `Unknown`, someone else is using it; either quit or try again.  If
    * you get a state that you don't want, put it back and quit or try again.  If you
    * get a state that you do want, use it, and replace it with the sensible next state
    * (usually just put the resource back, if it's there).  If it may take a while to
    * compute, swap `Unknown` for `Borrowed` and when you're done CAS Borrowed for your
    * result (others might replace `Borrowed` for `Unknown` but they're required to put
    * it back quickly).
    * 
    * You must put back the correct value even if there is an exception.  Users are responsible
    * for deciding whether an error means close and mark completed or something else.
    * 
    * This is not for managing the state of the resource itself, only access to it.
    * 
    * You should not have to manage this yourself; Work.RQ uses it to guard resource access.
    */
  object Resource {
    type Type[A] = Uninitialized.type | Active[A] | Completed.type | Borrowed.type | Unknown.type
    object Uninitialized {}
    case class Active[A](active: A) {}
    object Unknown {}
    object Completed {}
    object Borrowed {}
  }

  /** Creates a flexible size-limited cache for objects. Note that the maximum size is only approximately enforced. */
  class Cache[K <: AnyRef, V](val maxSize: Int)(val create: K => Ask[V], val clean: V => (V Or Unit) = v => Is(v)) {
    private val cache: ConcurrentHashMap[K, Atom[List[V]]] = new ConcurrentHashMap()
    val count: Atom[Int] = Atom(0)
    def get(k: K): Ask[V] = Ask:
      if count() < 1 then create(k).?
      else
        val avs = cache.get(k)
        if avs eq null then create(k).?
        else
          loop:
            val vs = avs()
            vs match
              case Nil =>
                loop.break()
              case v :: rest =>
                if avs.cas(vs, rest) then
                  count.--
                  Is.break(v)
          create(k).?
    def put(k: K, v: V): Unit =
      if count() >= maxSize then ()
      else threadsafe{ clean(v) }.mapAlt(_ => ()).flatten.foreach: cv =>
        if count() < maxSize then
          val avs = cache.computeIfAbsent(k, _ => Atom[List[V]](Nil))
          loop:
            val vs = avs()
            if avs.cas(vs, cv :: vs) then
              count.++
              loop.break()
            if count() >= maxSize then loop.break()

  }


  /** Creates zero or more items.
    *
    * `Some(Array.empty)` indicates that no items are available now.  `None` indicates that no more
    * will ever become available.  `Some(Array.empty)` does not imply that items will become available,
    * only that there is a possibility that they will.
    * 
    * Generally a `Work` item that puts items in a `Source` should then check it and produce follow-on
    * work if adequate items are available.
    */
  trait Source[A] {
    protected final val storedCount: Atom[Int] = Atom(0)

    /** Get items if they exist, `Some(Array.empty)` if they don't, or an error if there's an error.  `atMost` can be used to limit the number items (down to a minimum of 1). */
    def get(atMost: Int = Int.MaxValue): Ask[Option[Array[A]]]

    /** Gives an estimate of how many items are being cached by this source.  Value may be inexact; primarily useful as a load-balancing guide. */
    def loading: Int = storedCount()
  }

  /** Source that obtains its contents by someone adding items to it explicitly, which may be a different type than what's returned. */
  trait Workshop[A, B] extends Source[B] {
    /** Contributes a new item towards the eventual construction of the output. Returns a new error if the item cannot be added. */
    def put(a: A): Ask[Unit]

    /** Contributes a new item and retrieves any content that is now available. */
    def putAndGet(a: A, atMost: Int = Int.MaxValue): Ask[Option[Array[B]]] = Ask.flat:
      put(a).?
      get(atMost)
  }
  object Workshop {
    final class Chain[A, B, C](val first: Workshop[A, B], second: Workshop[B, C]) extends Workshop[A, C] {
      override def loading: Int = first.loading + second.loading

      def put(a: A): Ask[Unit] = Ask:
        first.putAndGet(a).? match
          case Some(bs) => bs.visit(){ (b, _) => second.put(b).? }
          case _ =>

      def get(atMost: Int = Int.MaxValue): Ask[Option[Array[C]]] = second.get(atMost)
    }
  }

  /** Sorts randomly-arriving items, returning as many as are available and sorted. */
  class Sort[W](title: String, val index: W => Int)(more: (W, Int) => Boolean, errorIfNonemptyStop: Boolean = false)(using ClassTag[W])
  extends Workshop[W, W] {
    private val items: TreeMap[Int, W] = TreeMap.empty
    private var lastIndex = -1
    private val empty: Option[Array[W]] = Some(Array.empty)

    /** Estimate of how many items will be available on the next call to `get`.  Answer may change by the time `get` is called, however. */
    def available(): Int = synchronized:
      if lastIndex == Int.MaxValue || items.isEmpty then 0
      else 
        var i = lastIndex + 1
        val it = items.keysIterator
        while it.hasNext && it.next == i do
          i += 1
        i - (lastIndex+1)

    def put(w: W): Ask[Unit] = synchronized:
      val i = index(w)
      if i <= lastIndex then Alt(Err(s"Tried to schedule $title number $i but processing is already past $lastIndex"))
      else if items contains i then Alt(Err(s"Tried to schedule $title number $i but one already exists"))
      else
        storedCount.++
        items(i) = w
        Is.unit

    def get(atMost: Int = Int.MaxValue): Ask[Option[Array[W]]] = Ask:
      synchronized:
        if lastIndex == Int.MaxValue then None
        else if items contains lastIndex+1 then
          val b = Array.newBuilder[W]
          var n = math.max(1, atMost)
          while n > 0 && lastIndex < Int.MaxValue && items.remove(lastIndex+1).map(b += _).isDefined do
            lastIndex += 1
            n -= 1
          val ws = b.result()
          storedCount.zap(_ - ws.length)
          if !more(ws(End), lastIndex) then lastIndex = Int.MaxValue
          if lastIndex == Int.MaxValue then
            if errorIfNonemptyStop && !items.isEmpty then Err ?# s"Sort terminated with ${items.size} items still pending retrieval"
            items.clear()
            storedCount := 0
          Some(ws)
        else empty
  }

  /** Gathers together like items, in parallel across threads, returning them when an entire set is available.
    * 
    * You must know how many items are in a category; it will only be returned once, and adding any more of that category is an error.
    */
  final class Gather[A, B](categorize: A => B)(needed: B => Int)(polish: Array[A] => Array[A] = __)(using ClassTag[A], ClassTag[(B, Array[A])])
  extends Workshop[A, (B, Array[A])] {
    import scala.collection.mutable.Queue
    // Note: ConcurrentHashMap has lower max-latency on resize than a regular HashMap, so we use it here even though it's only accessed while synchronized
    private val gathering: ConcurrentHashMap[B, Gather.Basket[A]] = new ConcurrentHashMap[B, Gather.Basket[A]]
    private val gathered: Queue[(B, Gather.Basket[A])] = Queue.empty
    private val empty: Option[Array[(B, Array[A])]] = Some(Array.empty)
    private val unloaded = Gather.Basket[A](1, -1, Nil)

    def put(a: A): Ask[Unit] = Ask:
      val b = categorize(a)
      synchronized:
        val basket = gathering.computeIfAbsent(b, _ => Gather.Basket(needed(b)))
        if basket.gathered < 0 then Err ?# s"Cannot add more items; $b already full (${basket.needed} items)"
        basket.gathered += 1
        basket.items = a :: basket.items
        if basket.isFull then
          gathered += ((b, basket))
          gathering.put(b, unloaded): Unit
        storedCount.++

    def get(atMost: Int = Int.MaxValue): Ask[Option[Array[(B, Array[A])]]] = Ask:
      val limit = math.max(1, atMost)
      val baskets = synchronized:
        if gathered.isEmpty then Is.break(empty)
        val a = new Array[(B, Gather.Basket[A])](math.max(1, atMost) min gathered.length)
        a.set(){ () => gathered.dequeue().tap(b => storedCount.zap(_ - b._2.needed)) }
        a
      Some(baskets.copyWith(x => x._1 -> polish(x._2.unsafeUnload())))
  }
  object Gather {
    final class Basket[A](askedFor: Int, var gathered: Int = 0, var items: List[A] = Nil) {
      val needed = 1 max askedFor
      inline def isFull = needed == gathered
      def unsafeUnload()(using ClassTag[A]): Array[A] =
        val a = new Array[A](needed)
        needed.visit: i =>
          a(End-i) = items.head
          items = items.tail
        gathered = -1
        a
    }
  }

  /** Accumulates items in parallel across threads, returning the result when an entire set is available.
   * 
   * If the aggregation operation is expensive, it might block for a while, so try to avoid running this on a
   * thread that has important low-latency responsibilities.  (Put is as fast as `accumulate` is, but merge merges everything.)
   */
  class Aggregate[A, B, Z](categorize: A => B)(needed: B => Int)(fresh: () => Z)(accumulate: (Z, A) => Z)(merge: Option[(Z, Z) => Z] = None)(using ClassTag[Z])
  extends Workshop[A, Z] {
    import collection.mutable.Queue
    private val accumulating: ConcurrentHashMap[B, Aggregate.Basket[Z]] = new ConcurrentHashMap
    private val gathered: Queue[Z] = Queue.empty
    private val empty: Option[Array[Z]] = Some(Array.empty)

    def put(a: A): Ask[Unit] = Ask:
      val b = categorize(a)
      val basket = synchronized:
        accumulating.computeIfAbsent(b, _ => Aggregate.Basket[Z](needed(b)))
      var zold: Z Or Unit = Alt.unit
      var consistent = false
      while !consistent do
        val zs = basket.accumulators()
        zs match
          case Nil => consistent = true
          case z :: rest =>
            if basket.accumulators.cas(zs, rest) then
              zold = Is(z)
              consistent = true
      val z = accumulate( zold.getOrElse{ _ => storedCount.++; fresh() } , a )
      basket.accumulators.zap(z :: _)
      val merges: List[Z] Or Unit = synchronized:
        basket.aggregated.++
        basket.aggregated() match
          case x if x == basket.needed =>
            basket.aggregated := Int.MinValue
            Is(basket.accumulators.swap(Nil))
          case x if x < 0 =>
            Err ?# s"Cannot add more items; $b already aggregated"
          case _ =>
            Alt.unit
      merges.foreach: zs =>
        val m = zs.length
        merge match
          case Some(op) =>
            val zall = zs.reduce(op)
            storedCount.zap(_ - (m-1))
            synchronized:
              gathered.+=(zall): Unit
          case _ => synchronized:
            gathered.++=(zs): Unit
        if m != basket.needed then Err ?# s"Internal error: was supposed to accumulate ${basket.needed} items but only got $m\n  while aggregating $b"
      ()


    def get(atMost: Int = Int.MaxValue): Ask[Option[Array[Z]]] =
      synchronized:
        if gathered.isEmpty then Is(empty)
        else
          val zs = new Array[Z](math.max(1, atMost) min gathered.length)
          zs.set(){ () => gathered.dequeue() }
          storedCount.zap(_ - zs.length)
          Is(Some(zs))
  }
  object Aggregate {
    final class Basket[Z](askedFor: Int, val aggregated: Mu.T[Int] = Mu(0), val accumulators: Atom[List[Z]] = Atom(Nil: List[Z])) {
      val needed = 1 max askedFor
    }
  }

  /*
  final class Bind[NT <: NamedTuple.AnyNamedTuple] private (nameIndices: Map[String, Int])
  extends Workshop[(Int, Bind.Unionized[NT]), NT] {
  }
  object Bind {
    inline def apply[NT <: NamedTuple.AnyNamedTuple](): Bind[NT] = byIndexedNameImpl[NT](compiletime.constValueTuple[NamedTuple.Names[NT]])

    def byIndexedNameImpl[NT <: NamedTuple.AnyNamedTuple](explicitNameTuple: NamedTuple.Names[NT]) =
      new Bind[NT](explicitNameTuple.toIArray.zipWithIndex.toMap.asInstanceOf[Map[String, Int]])

    type Unionized[NT] = NamedTuple.Names[NT] match
      case n *: EmptyTuple => NamedTuple.DropNames[NT] match
        case v *: EmptyTuple => (n, v)
      case n *: ns => NamedTuple.DropNames[NT] match
        case v *: vs => (n, v) | Unionized[NamedTuple.NamedTuple[ns, vs]]
  }
  */
  */
}

