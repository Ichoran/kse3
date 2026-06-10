// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2024-26 Rex Kerr and UCSF (Kato Lab).

package kse.loom

import java.util.concurrent.{ConcurrentLinkedQueue, LinkedTransferQueue, Semaphore}
import java.util.concurrent.locks.ReentrantLock

import scala.collection.mutable.{ArrayBuffer, HashMap, Queue, TreeMap}
import scala.reflect.ClassTag
import scala.util.boundary

import kse.basics.*
import kse.flow.*
import kse.maths.*


/** Which thread(s) serve a [[Percolate.Resource]].  `Any` (default): any worker or the main thread, one
  * at a time.  `Main`: only the primary (`go`-calling) thread.  `Own`: a dedicated thread of its own (for
  * thread-pinned things like a GPU/CUDA context).  `Main` and `Own` are *affine* — an unchanging thread —
  * and `Own` is the one case whose work is not main-runnable; everything else still completes on main. */
enum Affinity:
  case Any
  case Main
  case Own


/** A work-sharing engine for succeed-or-fail batch computations on conventional threads.
  *
  * You subclass `Percolate` and define a kind of `Work` whose `work()` runs a chunk and returns the
  * follow-on work it spawned (an `Array[Work]`, possibly empty).  Work-generates-work, so arbitrarily
  * complex looping or recursion is expressed implicitly in the data.  The engine handles all the
  * scheduling: you write the work and what it produces, and the rest is taken care of.
  *
  * **The crown-jewel invariant: every work item is runnable by the main thread.**  Worker threads are
  * an optimization.  With `parallelism = 0` the main thread's `step()` loop alone drains everything and
  * completes correctly; a permit budget bounds work-in-flight so the single-thread fallback never
  * overflows.
  *
  * **Producers** create root work.  The engine-level `newWork()` is the general producer; a [[Producer]]
  * resource is a producer tied to a single-threaded source (a file, a socket).  Each producer hands back
  * a `Work`, or `Work.Empty` when *it* is exhausted; the whole run ends once every producer is `Empty`,
  * all queues are drained, and nothing is in flight.  (A premature whole-system stop is the error path,
  * not a normal signal.)  Producers must be independent — one may not produce work that depends on
  * another resource's output; cross-resource flow goes through consumer work and shared structures.
  *
  * **Resources** are things used by one thread at a time — an output stream, a single-threaded library.
  * You subclass `Resource` to say how to `open`/`close` it (it holds its own typed fields), and define
  * its work as `On` inner classes that, by scoping, see those fields directly.  The engine owns a FIFO
  * queue per resource and serves it under a non-fair lock (`tryLock`): a worker that finds a resource
  * busy simply does other work — it never blocks on it — and the main thread is the universal fallback
  * server, so the single-thread invariant still holds.  An `On` body never acquires or releases anything;
  * it only *emits* targeted follow-on work, which is what keeps every item main-runnable.
  *
  * A resource's [[Affinity]] chooses which thread(s) serve it: `Any` (the default above), `Main` (only the
  * primary thread), or `Own` (a dedicated thread, for a thread-pinned GPU/CUDA context — the one case whose
  * work is not main-runnable).
  *
  * For long-running concurrent tasks with cancellation and recovery, use `Go`/`Chan` instead.
  */
abstract class Percolate(parallelism: Int, maxPermits: Option[Int] = None) {
  protected val canceled = Atom(0)
  protected val complete = Atom(0L)
  protected val incomplete = Atom(0)

  protected val errors: Atom[List[Err]] = Atom(Nil: List[Err])

  private val busyNanos = Atom(0L)


  // === Crash-safe cleanup that MUST run even on a hard exit ===

  private val runOnErrorExit = new LinkedTransferQueue[() => Ask[Unit]]()
  private var cleanupCount = 0
  protected def registerErrorExitItem(item: () => Ask[Unit]): Unit =
    runOnErrorExit.put(item)
    synchronized:
      if cleanupCount == 0 then
        java.lang.Runtime.getRuntime.addShutdownHook(new Thread(() => {
          var erun: () => Ask[Unit] = null
          while { erun = runOnErrorExit.poll(); erun ne null } do
            // Try REALLY hard to run everything -- these are only registered when we can't safely
            // crash without them.
            try erun() __ Unit
            catch case _: Throwable => ()
        }))
      cleanupCount += 1


  // === Permits: the only backpressure.  A permit gates source admission, then is inherited and
  //     refcounted down the whole derived work lineage, so it releases only when that lineage drains. ===

  private val myPermits = new Semaphore(1 max maxPermits.getOrElse(2 * parallelism + 1))
  final def obtainPermit(): Boolean = myPermits.tryAcquire()
  final def returnPermit(): Unit = myPermits.release()

  final class Permit(val id: Int, count: Atom[Int]) {
    def again(): Unit = count.zap(n => if n > 0 then n + 1 else 0)
    def stop(): Unit = if count.getAndZap(n => if n > 1 then n - 1 else 0) == 1 then returnPermit()
  }
  object Permit {
    private val pid = Atom(0)
    def apply(): Permit = new Permit(pid.zapAndGet(_ + 1), Atom(1))
  }


  // === Errors ===

  /** Record an error and depermit the offending work; the run loops wind down on `errors().nonEmpty`. */
  def errored(from: Work, e: Err): Unit =
    errors.zap(e :: _)
    from.cancel(depermitOnly = true)


  // === Work ===

  /** A unit of work.  `work()` runs a chunk and returns the follow-on work it spawned (empty for none).
    * `target` says where it runs: `null` = the general pool (any thread); otherwise the resource whose
    * queue it joins (set automatically for a `Resource#On`).  `inheritsPermit` makes follow-ons share
    * this item's lineage permit; `touch` is a metrics hook run just before the body. */
  abstract class Work(
    final val inheritsPermit: Boolean = true,
    final val touch: Option[() => Unit] = None
  ) {
    def target: Resource | Null = null
    final val permit: Atom[Option[Permit]] = Atom(None: Option[Permit])
    def desc: String = s"Work item $toString"
    def work(): Ask[Array[Work]]
    def and(more: => Work): And = new And(this, more)
    def cancel(depermitOnly: Boolean = false): Unit =
      permit().foreach(_.stop())
      incomplete.--
      if !depermitOnly then canceled.++
  }
  object Work {
    /** No follow-on work. */
    def none: Array[Work] = Array.empty[Work]
    /** Producer sentinel: return from `newWork`/`produce` to retire *that producer* (no more from it).
      * Never enqueued or run; the whole run ends once every producer is `Empty` and nothing's in flight. */
    object Empty extends Work(inheritsPermit = false) { def work(): Ask[Array[Work]] = Is(none) }
    /** A real do-nothing work item — e.g. an array-slot filler.  Unlike `Empty` it *is* enqueued/run. */
    object NoOp extends Work(inheritsPermit = false) { def work(): Ask[Array[Work]] = Is(none) }
  }

  /** Emit two pieces of work. */
  final class And(w: Work, more: => Work) extends Work() {
    def work(): Ask[Array[Work]] = threadnice:
      Array[Work](w, more)
  }

  /** Emit a batch of work from thunks. */
  final class Lots(ws: Array[() => Work]) extends Work() {
    def work(): Ask[Array[Work]] = threadnice:
      ws.copyWith(_())
  }


  // === Resources: serially-used things with an engine-owned queue and lifecycle ===

  /** A resource used by one thread at a time.  Subclass it to supply `open`/`close` and hold whatever
    * state the resource needs (its own typed fields), and define its work as `On` inner classes that see
    * that state directly.  The engine owns the FIFO queue and the open/close lifecycle; a work body never
    * acquires or releases it.  Served exclusively under a non-fair `tryLock` (workers help, main is the
    * fallback), opened lazily on first use and closed at teardown.  An optional `emergencyClose` runs on a
    * hard JVM exit if the resource was open and not cleanly closed. */
  abstract class Resource(val name: String, val affinity: Affinity = Affinity.Any, emergencyClose: Option[() => Unit] = None) {
    registerResource(this)

    protected def open(): Ask[Unit] = Is.unit
    protected def close(): Ask[Unit] = Is.unit

    private val q = new ConcurrentLinkedQueue[Work]()
    private[Percolate] val guard = new ReentrantLock()
    private[Percolate] val ownSignal = new Semaphore(0)   // wakes this resource's dedicated thread (Own only)
    @volatile private var opened = false
    private var openFailed = false
    private var closed = false
    private val emergencyDone = Atom(false)

    /** Work bound to this resource.  Lives inside it, so `work()` may use the resource's own fields and
      * methods directly — nothing is injected, nothing is acquired. */
    abstract class On(inheritsPermit: Boolean = true, touch: Option[() => Unit] = None)
    extends Work(inheritsPermit, touch) {
      final override def target: Resource = Resource.this
    }

    private[Percolate] def isEmpty: Boolean = q.isEmpty

    private[Percolate] def enqueue(w: Work): Unit =
      q.add(w) __ Unit
      // Wake the right server: an `Own` resource's dedicated thread, or a worker for `Any`.  `Main` is
      // served only by the never-parked main loop, so it needs no wakeup.
      affinity match
        case Affinity.Own => ownSignal.release()
        case Affinity.Any => available.release()
        case Affinity.Main =>

    /** Open lazily, under the guard.  Any failure (incl. `???`) becomes data via `errors`; the resource
      * then stops serving. */
    private[Percolate] def ensureOpen(): Boolean =
      if opened then true
      else if openFailed then false
      else
        threadnice{ open() }.flatten.fold{ _ =>
          opened = true
          if emergencyClose.isDefined then
            registerErrorExitItem(() => nice{ if !emergencyDone.swap(true) then emergencyClose.foreach(_()) })
          true
        }{ e =>
          errors.zap(e :: _)
          openFailed = true
          false
        }

    /** Run one queued item under exclusive access, or skip.  Never blocks.  Returns true iff it ran one. */
    private[Percolate] def tryServe(): Boolean =
      if q.isEmpty then false
      else if !guard.tryLock() then false                  // someone else is serving it; skip
      else
        try
          if !ensureOpen() then false
          else
            val w = q.poll()
            if w eq null then false                         // lost the race for the item; fine
            else { runItem(w); true }
        finally guard.unlock()

    /** Close once, at teardown (under the guard); a `close` failure is recorded.  `emergencyDone` is set
      * only on a *successful* close — if close fails, the resource is not cleanly closed, so the
      * emergency-close hook stays armed to try on hard exit. */
    private[Percolate] def closeIfOpen(): Unit =
      guard.lock()
      try
        if opened && !closed then
          closed = true
          threadnice{ close() }.flatten.fold(_ => emergencyDone := true)(e => errors.zap(e :: _))
      finally guard.unlock()
  }

  /** A resource that also *produces* root work from its own single-threaded source.  `produce()` runs in
    * the resource's domain (under its guard), handing back a `Work`, or `Work.Empty` when exhausted. */
  abstract class Producer(name: String, affinity: Affinity = Affinity.Any, emergencyClose: Option[() => Unit] = None)
  extends Resource(name, affinity, emergencyClose) {
    @volatile private[Percolate] var producerDone = false
    protected def produce(): Ask[Work]
    private[Percolate] def pullProduce(): Ask[Work] = produce()
  }

  private val resourceList = ArrayBuffer.empty[Resource]
  private var resources: Array[Resource] = Array.empty[Resource]
  private[Percolate] def registerResource(r: Resource): Unit = { val _ = resourceList += r }

  // Close `Any`/`Main` resources here (on main); `Own` resources are closed by their own threads.
  private def closeAllResources(): Unit =
    var j = resources.length - 1
    while j >= 0 do
      if resources(j).affinity != Affinity.Own then resources(j).closeIfOpen()
      j -= 1


  // === Producers (the engine-level newWork + each Producer resource), rotated under one permit pool ===

  private enum Pull:
    case Made(w: Work)     // here's a work item
    case Done              // this producer is exhausted (Work.Empty)
    case Busy              // a consumer holds the resource right now; try again later
    case Broke(e: Err)     // the producer errored

  private final class Prod(val pull: () => Pull) { var done = false }
  private var prods: Array[Prod] = Array.empty[Prod]
  private var prodIdx = 0

  private def nextActiveProd(): Prod | Null =
    var k = 0
    while k < prods.length do
      val i = { val t = prodIdx + k; if t >= prods.length then t - prods.length else t }
      if !prods(i).done then
        prodIdx = { val n = i + 1; if n >= prods.length then 0 else n }
        return prods(i)
      k += 1
    null

  /** Are the producers main rotates over (general `newWork` + `Any`/`Main` producers) all exhausted? */
  private def mainProdsDone: Boolean =
    var i = 0
    while i < prods.length do
      if !prods(i).done then return false
      i += 1
    true

  /** Are all `Own`-affinity producers (driven by their dedicated threads) exhausted? */
  private def ownProdsDone: Boolean =
    var i = 0
    while i < resources.length do
      resources(i) match
        case p: Producer if (p.affinity == Affinity.Own) && !p.producerDone => return false
        case _ =>
      i += 1
    true

  /** Admit one item from a producer under a permit; returns true iff it yielded work.  `onDone` fires
    * when the producer signals `Work.Empty`. */
  private def admit(pull: () => Pull, onDone: () => Unit): Boolean =
    if !obtainPermit() then false
    else
      pull() match
        case Pull.Made(w)  => w.permit := Some(Permit()); incomplete.++; route(w); true
        case Pull.Done     => onDone(); returnPermit(); false
        case Pull.Busy     => returnPermit(); false
        case Pull.Broke(e) => returnPermit(); errors.zap(e :: _); false

  /** Pull one item from a producer, under its guard.  `Busy` only for *transient* lock contention; a
    * permanent open failure (already recorded in `errors`) retires the producer with `Done`. */
  private def pullFrom(p: Producer): Pull =
    if !p.guard.tryLock() then Pull.Busy
    else
      try
        if !p.ensureOpen() then Pull.Done    // open failed (permanent, recorded) — retire, don't spin on Busy
        else threadnice{ p.pullProduce() }.flatten.fold(w => if w eq Work.Empty then Pull.Done else Pull.Made(w))(e => Pull.Broke(e))
      finally p.guard.unlock()


  // === Queues, wakeup, and subclass contract ===

  private val worklist = new ConcurrentLinkedQueue[Work]()
  private val available = new Semaphore(0)                  // wakeup: one permit per enqueued item
  @volatile private var running = true

  /** Create fresh general work, or `Work.Empty` when this producer is exhausted. */
  def newWork(): Ask[Work]

  /** Acquire whatever the run needs; on error, dispose of anything acquired before returning. */
  def setup(): Ask[Unit]

  /** Release resources, on success *or* error; only return `Err` if the release itself fails. */
  def teardown(): Ask[Unit]

  /** Route work to its destination queue (general or a resource), and wake a worker. */
  private def route(w: Work): Unit =
    val t = w.target
    if t eq null then
      worklist.add(w) __ Unit
      available.release()
    else t.enqueue(w)

  private def allQueuesEmpty: Boolean =
    if !worklist.isEmpty then false
    else
      var i = 0
      var empty = true
      while empty && i < resources.length do
        if !resources(i).isEmpty then empty = false
        i += 1
      empty

  /** Account for a finished item: enqueue its follow-ons (inheriting the permit), then release the
    * item's own hold on the lineage permit. */
  def todone(from: Work, ws: Array[Work]): Unit =
    var i = 0
    while i < ws.length do
      val x = ws(i)
      incomplete.++
      if x.inheritsPermit then
        from.permit() match
          case sp @ Some(p) => p.again(); x.permit := sp
          case _            =>
      route(x)
      i += 1
    from.permit().foreach(_.stop())
    complete.++
    incomplete.--

  /** The single hardened entry both workers and the main loop use to run an item.  Every user-code call
    * is inside a `threadnice` boundary, so any throwable (incl. `???`) becomes an `Err` via `errored`
    * and the run shuts down normally. */
  private def runItem(w: Work): Unit =
    if errors().nonEmpty then w.cancel()
    else
      val t0 = System.nanoTime
      val result = threadnice:
        w.touch.foreach(_())
        w.work()
      .flatten
      busyNanos += (System.nanoTime - t0)
      result.foreachThem(ws => todone(w, ws))(e => errored(w, e))


  // === Workers: platform threads that drain general work and help across resources ===

  final class Worker extends Thread {
    override def run(): Unit =
      threadnice{ runloop() }.foreachAlt(e => errors.zap(e :: _))

    private def runloop(): Unit =
      while running do
        var did = false
        val w = worklist.poll()
        if w ne null then { runItem(w); did = true }
        else
          var i = 0
          while i < resources.length && !did do
            val r = resources(i)
            if (r.affinity == Affinity.Any) && r.tryServe() then did = true    // Main/Own are not ours
            i += 1
        // Nothing to do: park until an enqueue (or shutdown) releases a permit, then re-scan.
        if !did && running then available.acquireUninterruptibly()
  }

  private val workers = new Array[Worker](parallelism max 0)

  // === Own-affinity resources: each gets one dedicated thread that opens it, serves + produces on that
  //     thread alone, and closes it on exit.  Workers and main never touch an `Own` resource. ===

  private var ownThreads: Array[Thread] = Array.empty[Thread]

  private def ownLoop(r: Resource): Unit =
    threadnice:
      while running do
        var did = r.tryServe()                                  // sole server: tryLock always succeeds
        var producing = false
        if !did then
          r match
            case p: Producer if !p.producerDone =>
              producing = true
              if admit(() => pullFrom(p), () => p.producerDone = true) then did = true
            case _ =>
        if !did && running then
          if producing then Thread.`yield`()                    // permit-blocked but more to make: spin
          else r.ownSignal.acquireUninterruptibly()             // idle: wait for routed work / shutdown
    .foreachAlt(e => errors.zap(e :: _))
    r.closeIfOpen()                                             // close on this (the pinned) thread

  private def startOwnThreads(): Unit =
    val ot = ArrayBuffer.empty[Thread]
    var i = 0
    while i < resources.length do
      val r = resources(i)
      if r.affinity == Affinity.Own then
        val t = new Thread(() => ownLoop(r), s"percolate-own-${r.name}")
        ot += t
        t.start()
      i += 1
    ownThreads = ot.toArray

  protected def startWorkers(): Ask[Unit] = Ask:
    var i = 0
    while i < workers.length do
      val wk = new Worker()
      workers(i) = wk
      wk.start()
      i += 1

  // Join, tolerating interruption (we're tearing down, so we don't want to abandon a thread mid-exit).
  private def joinThread(t: Thread): Unit =
    if t ne null then
      var joined = false
      while !joined do
        try { t.join(); joined = true }
        catch case _: InterruptedException => Thread.currentThread().interrupt()

  protected def stopWorkers(): Unit =
    running = false
    if workers.length > 0 then available.release(workers.length)    // wake parked workers
    var i = 0
    while i < resources.length do                                   // wake parked dedicated (Own) threads
      if resources(i).affinity == Affinity.Own then resources(i).ownSignal.release()
      i += 1
    i = 0
    while i < workers.length do { joinThread(workers(i)); i += 1 }
    i = 0
    while i < ownThreads.length do { joinThread(ownThreads(i)); i += 1 }

  /** Stop workers, close resources (LIFO), tear down; returns total busy nanos across all threads. */
  def shutdown(): Ask[Long] = Ask:
    stopWorkers()
    closeAllResources()
    teardown().?
    busyNanos()


  // === The main-thread loop: serve resources, steal general work, admit from a producer under a permit ===

  /** One pass of the main loop.  Returns `false` once every producer is exhausted and nothing's in flight. */
  def step(): Ask[Boolean] = Ask:
    var progressed = false

    // 1. Serve `Main`-affinity resources — main is their *sole* server, so this is its own duty, not
    //    stealing; it must run every pass regardless of whether main is producing.  (`Any` is served
    //    below only when not producing; `Own` has its own thread.)  tryServe has already caught any
    //    failure (open/work) and routed it to `errors`, so it surfaces only progress.
    var i = 0
    while i < resources.length do
      val r = resources(i)
      if (r.affinity == Affinity.Main) && r.tryServe() then progressed = true
      i += 1

    // 2. Produce in preference to stealing.  Admitting is work that, among the rotation servers, only
    //    main drives, so while producers remain and a permit is free, main feeds the pool instead of
    //    running an item a worker could take — otherwise a busy main starves idle workers of work only
    //    it can generate.
    var admitted = false
    if !mainProdsDone then
      val p = nextActiveProd()
      if (p ne null) && admit(p.pull, () => p.done = true) then { progressed = true; admitted = true }

    // 3. Steal only when *not* producing — a permit was unavailable (budget exhausted), the chosen
    //    producer was busy, or all producers are done.  This drains the `Any` resources (a worker's
    //    equals) and the general worklist, preserving the single-thread invariant: with no workers,
    //    main produces up to the permit budget, then falls here to drain (releasing permits), and loops
    //    back to produce more.
    if !admitted then
      i = 0
      while i < resources.length do
        val r = resources(i)
        if (r.affinity == Affinity.Any) && r.tryServe() then progressed = true
        i += 1
      val w = worklist.poll()
      if w ne null then { runItem(w); progressed = true }

    if mainProdsDone && ownProdsDone && incomplete() == 0 && allQueuesEmpty then false
    else
      if !progressed then Thread.`yield`()
      true

  /** Run the whole computation to completion (or first error), returning (wall, total-busy) time. */
  def go(): Ask[(NanoDuration, NanoDuration)] = boundary[Ask[(NanoDuration, NanoDuration)]]:
    resources = resourceList.toArray
    running = true

    // Producers: the general `newWork`, plus every `Producer` resource.
    val pbuf = ArrayBuffer.empty[Prod]
    pbuf += new Prod(() =>
      threadnice{ newWork() }.flatten.fold(w => if w eq Work.Empty then Pull.Done else Pull.Made(w))(e => Pull.Broke(e))
    )
    var pi = 0
    while pi < resources.length do
      resources(pi) match
        case prod: Producer if prod.affinity != Affinity.Own => pbuf += new Prod(() => pullFrom(prod))
        case _              =>
      pi += 1
    prods = pbuf.toArray
    prodIdx = 0
    ownThreads = Array.empty[Thread]

    // On a startup failure after `setup()` succeeded, stop whatever started, close resources, run
    // teardown, and surface the error — so teardown always runs even if thread creation fails.
    def abortStartup(e: Err): Nothing =
      stopWorkers()
      closeAllResources()
      threadnice(teardown()).flatten.foreachAlt(_ => ())
      boundary.break(Alt(e))

    threadnice(setup()).flatten.?                                  // setup cleans up after itself on error
    threadnice(startWorkers()).flatten.foreachAlt(abortStartup)
    threadnice{ startOwnThreads() }.foreachAlt(abortStartup)

    var wall = 0L
    threadnice:
      val t0 = System.nanoTime
      var goOn = true
      while goOn && errors().isEmpty do
        goOn = step().fold(b => b)(e => { errors.zap(e :: _); false })
      wall = System.nanoTime - t0
    .foreachAlt(e => errors.zap(e :: _))

    var busy = 0L
    threadnice{ shutdown() }.flatten.fold(busy = _)(e => errors.zap(e :: _))

    var erun: () => Ask[Unit] = null
    while { erun = runOnErrorExit.poll(); erun ne null } do
      threadnice{ erun().foreachAlt(ee => errors.zap(ee :: _)) }.foreachAlt(e => errors.zap(e :: _))

    val wcanceled = canceled()
    errors() match
      case Nil => Is((wall.ns_nano, busy.ns_nano))
      case e :: Nil if wcanceled == 0 => Alt(e)
      case es =>
        val all =
          if wcanceled == 0 then es.reverse
          else (Err(s"Canceled $wcanceled work items after detection of error") :: es).reverse
        Alt(Err(ErrType.Many(all)))
}
object Percolate {
  /** The outcome of drawing from a [[Source]]: a batch of assembled items, nothing ready yet (but more may
    * still come), or exhausted.  This explicit three-way replaces an `Option[Array]` whose `Some(empty)` vs
    * `None` carried the not-yet-vs-done distinction implicitly. */
  enum Drawn[+A]:
    case Items(values: IArray[A])
    case NotReady
    case Done

  /** Something you can draw assembled items from, one batch at a time.  Thread-safe implementations let
    * many workers feed and drain concurrently. */
  trait Source[A]:
    /** Draw whatever is ready now (at most `atMost` items). */
    def draw(atMost: Int = Int.MaxValue): Drawn[A]
    /** Rough count of items currently buffered — a load-balancing hint; may be stale. */
    def loading: Int

  /** A `Source` you feed with `put`, assembling `A`s into `B`s.  `seal()` declares that no more input will
    * arrive, so once everything assembled has been drawn, `draw` returns `Done` (which is how a draining
    * worker knows to stop re-queuing itself). */
  trait Workshop[A, B] extends Source[B]:
    def put(a: A): Ask[Unit]
    def seal(): Unit
    def putAndDraw(a: A, atMost: Int = Int.MaxValue): Ask[Drawn[B]] = Ask:
      put(a).?
      draw(atMost)

  /** Reorders randomly-arriving indexed items into contiguous runs, emitting a run only once every earlier
    * index has arrived.  `continues(lastItem, lastIndex)` decides whether more runs follow; returning false
    * (or calling `seal()`) ends the stream. */
  final class Sort[W](title: String, index: W => Int)(continues: (W, Int) => Boolean)(using ClassTag[W])
  extends Workshop[W, W]:
    private val items = TreeMap.empty[Int, W]
    private var lastIndex = -1
    private var ended = false
    private val stored = Atom(0)

    def loading: Int = stored()

    def put(w: W): Ask[Unit] = synchronized:
      if ended then Alt(Err(s"$title is finished; cannot add item ${index(w)}"))
      else
        val i = index(w)
        if i <= lastIndex then Alt(Err(s"Tried to schedule $title number $i but processing is already past $lastIndex"))
        else if items contains i then Alt(Err(s"Tried to schedule $title number $i but one already exists"))
        else { items(i) = w; stored.++; Is.unit }

    def seal(): Unit = synchronized { ended = true }

    def draw(atMost: Int = Int.MaxValue): Drawn[W] = synchronized:
      if items contains (lastIndex + 1) then
        val b = Array.newBuilder[W]
        var n = math.max(1, atMost)
        while n > 0 && items.remove(lastIndex + 1).map(b += _).isDefined do { lastIndex += 1; n -= 1 }
        val ws = b.result()
        stored.zap(_ - ws.length)
        if !continues(ws(ws.length - 1), lastIndex) then { stored.zap(_ - items.size); items.clear(); ended = true }
        Drawn.Items(IArray.unsafeFromArray(ws))
      else if ended then Drawn.Done           // sealed/finished: drain contiguous, then done (gap items dropped)
      else Drawn.NotReady

  /** Collects items by category, emitting a category's items as one batch once `needed(category)` of them
    * have arrived.  `seal()` ends it (any still-incomplete categories are then abandoned). */
  final class Gather[A, B](categorize: A => B)(needed: B => Int)(using ClassTag[A], ClassTag[(B, IArray[A])])
  extends Workshop[A, (B, IArray[A])]:
    private val baskets = HashMap.empty[B, ArrayBuffer[A]]
    private val ready = Queue.empty[(B, IArray[A])]
    private var ended = false
    private val stored = Atom(0)

    def loading: Int = stored()

    def put(a: A): Ask[Unit] = synchronized:
      if ended then Alt(Err("Gather is sealed; cannot add more items"))
      else
        val b = categorize(a)
        val buf = baskets.getOrElseUpdate(b, new ArrayBuffer[A])
        buf += a
        stored.++
        if buf.length >= math.max(1, needed(b)) then
          baskets -= b
          ready.enqueue(b -> IArray.unsafeFromArray(buf.toArray))
        Is.unit

    def seal(): Unit = synchronized:
      ended = true
      baskets.valuesIterator.foreach(b => stored.zap(_ - b.length))   // abandon incomplete baskets; stop counting them
      baskets.clear()

    def draw(atMost: Int = Int.MaxValue): Drawn[(B, IArray[A])] = synchronized:
      if ready.nonEmpty then
        val k = math.max(1, atMost) min ready.length
        val out = new Array[(B, IArray[A])](k)
        var i = 0
        while i < k do
          val t = ready.dequeue()
          stored.zap(_ - t._2.length)
          out(i) = t
          i += 1
        Drawn.Items(IArray.unsafeFromArray(out))
      else if ended then Drawn.Done
      else Drawn.NotReady

  /** Accumulates items by category into a `Z`, emitting the `Z` once `needed(category)` items have folded
    * in.  `fresh()` starts a category's accumulator; `fold(z, a)` adds one item.  `seal()` ends it (any
    * still-incomplete categories are then abandoned). */
  final class Aggregate[A, B, Z](categorize: A => B)(needed: B => Int)(fresh: () => Z)(fold: (Z, A) => Z)(using ClassTag[Z])
  extends Workshop[A, Z]:
    private val acc = HashMap.empty[B, (Z, Int)]
    private val ready = Queue.empty[Z]
    private var ended = false
    private val stored = Atom(0)

    def loading: Int = stored()

    def put(a: A): Ask[Unit] = synchronized:
      if ended then Alt(Err("Aggregate is sealed; cannot add more items"))
      else
        val b = categorize(a)
        val (z0, c0) = acc.getOrElse(b, (fresh(), 0))
        val z1 = fold(z0, a)
        val c1 = c0 + 1
        if c1 >= math.max(1, needed(b)) then { acc -= b; ready.enqueue(z1); stored.++ }
        else acc(b) = (z1, c1)
        Is.unit

    def seal(): Unit = synchronized { ended = true }

    def draw(atMost: Int = Int.MaxValue): Drawn[Z] = synchronized:
      if ready.nonEmpty then
        val k = math.max(1, atMost) min ready.length
        val out = new Array[Z](k)
        var i = 0
        while i < k do { out(i) = ready.dequeue(); i += 1 }
        stored.zap(_ - k)
        Drawn.Items(IArray.unsafeFromArray(out))
      else if ended then Drawn.Done
      else Drawn.NotReady
}
