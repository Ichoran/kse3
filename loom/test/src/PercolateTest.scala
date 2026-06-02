// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025 Rex Kerr.

package kse.test.loom

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics._
import kse.flow._
import kse.loom._
import kse.loom.Percolate.Drawn


@RunWith(classOf[JUnit4])
class PercolateTest {
  val Reps = 50

  /** A configurable test engine.  Seeds `n` items.  If `useResource`, each is a `Put` on the exclusive
    * `sink` resource; else if `fan > 0` each is a `Branch` spawning `fan` `Leaf` children; else a `Leaf`.
    * `failAt` makes the matching item fail (an `Alt`, or a thrown `NotImplementedError` if `bomb`). */
  class Fan(
    parallelism: Int, n: Int,
    fan: Int = 0, failAt: Int = -1, bomb: Boolean = false, useResource: Boolean = false,
    maxPermits: Option[Int] = None
  ) extends Percolate(parallelism, maxPermits) {
    val ran = Atom(0)
    val toreDown = Atom(false)
    val didSetup = Atom(false)
    val opened = Atom(0)
    val closedN = Atom(0)
    val concurrent = Atom(0)
    val maxConcurrent = Atom(0)
    private var seeded = 0

    def inFlight: Int = incomplete()
    def doneCount: Long = complete()

    def setup(): Ask[Unit] = { didSetup := true; Is.unit }
    def teardown(): Ask[Unit] = { toreDown := true; Is.unit }

    // An exclusive resource holding its own state (no type parameter); its work sees the field by scoping.
    final class Sink extends Resource("sink") {
      private var buf: StringBuilder = null
      protected override def open():  Ask[Unit] = { opened.++; buf = new StringBuilder; Is.unit }
      protected override def close(): Ask[Unit] = { closedN.++; Is.unit }
      final class Put(k: Int) extends On() {
        def work(): Ask[Array[Work]] =
          val c = concurrent.zapAndGet(_ + 1)
          maxConcurrent.zap(m => if c > m then c else m)
          try
            if k == failAt then (if bomb then ??? else Alt(Err(s"boom at $k")))
            else
              buf.append(k).append(',') __ Unit                // uses the resource's own field
              ran.++
              Is(Work.none)
          finally concurrent.--
      }
    }
    val sink = new Sink

    final class Leaf(k: Int) extends Work() {
      def work(): Ask[Array[Work]] =
        if k == failAt then (if bomb then ??? else Alt(Err(s"boom at $k")))
        else { ran.++; Is(Work.none) }
    }

    final class Branch(k: Int) extends Work() {
      def work(): Ask[Array[Work]] =
        ran.++
        Is(Array.tabulate[Work](fan)(j => new Leaf(k * 1000 + j)))
    }

    def newWork(): Ask[Work] =
      if seeded < n then
        seeded += 1
        Is(
          if useResource then new sink.Put(seeded)
          else if fan > 0 then new Branch(seeded)
          else new Leaf(seeded)
        )
      else Is(Work.Empty)
  }

  /** An engine whose root work comes from a `Producer` resource (a single-threaded source) rather than
    * the general `newWork`. */
  class Feed(parallelism: Int, m: Int, affinity: Affinity = Affinity.Any, failOpen: Boolean = false)
  extends Percolate(parallelism) {
    val ran = Atom(0)
    val opened = Atom(0)
    val closedN = Atom(0)

    def setup(): Ask[Unit] = Is.unit
    def teardown(): Ask[Unit] = Is.unit
    def newWork(): Ask[Work] = Is(Work.Empty)                  // all production via the Producer

    final class Src extends Producer("src", affinity) {
      private var made = 0
      protected override def open():  Ask[Unit] = if failOpen then Alt(Err("cannot open src")) else { opened.++; Is.unit }
      protected override def close(): Ask[Unit] = { closedN.++; Is.unit }
      protected def produce(): Ask[Work] =
        if made < m then { made += 1; Is(new Job(made)) }
        else Is(Work.Empty)
      final class Job(k: Int) extends Work() {                 // general work emitted by the producer
        def work(): Ask[Array[Work]] = { ran.++; Is(Work.none) }
      }
    }
    val src = new Src
  }

  /** An engine whose work targets a resource with a fixed thread affinity, recording which thread runs it. */
  class Pinned(parallelism: Int, n: Int, affinity: Affinity) extends Percolate(parallelism) {
    val ran = Atom(0)
    val opened = Atom(0)
    val closedN = Atom(0)
    val servingId = Atom(0L)        // thread-id of the (single) thread that served the resource; 0 = unset
    val sawOther = Atom(false)      // set if a second distinct thread ever served it
    private var seeded = 0

    def setup(): Ask[Unit] = Is.unit
    def teardown(): Ask[Unit] = Is.unit

    final class Dev extends Resource("dev", affinity) {
      protected override def open():  Ask[Unit] = { opened.++; Is.unit }
      protected override def close(): Ask[Unit] = { closedN.++; Is.unit }
      final class Op(k: Int) extends On() {
        def work(): Ask[Array[Work]] =
          val id = Thread.currentThread().threadId()
          if !servingId.cas(0L, id) && servingId() != id then sawOther := true
          ran.++
          Is(Work.none)
      }
    }
    val dev = new Dev

    def newWork(): Ask[Work] =
      if seeded < n then { seeded += 1; Is(new dev.Op(seeded)) }
      else Is(Work.Empty)
  }


  // === The crown-jewel invariant: with zero workers the main thread completes everything ===

  @Test(timeout = 30000)
  def zeroWorkerCompletes(): Unit = Reps.times:
    val p = new Fan(parallelism = 0, n = 200)
    val r = p.go()
    assertTrue(s"expected success, got $r", r.isIs)
    assertEquals(200, p.ran())
    assertEquals(0, p.inFlight)
    assertEquals(200L, p.doneCount)
    assertTrue(p.didSetup())
    assertTrue(p.toreDown())

  @Test(timeout = 30000)
  def parallelCompletes(): Unit = Reps.times:
    val p = new Fan(parallelism = 4, n = 2000)
    assertTrue(p.go().isIs)
    assertEquals(2000, p.ran())
    assertEquals(0, p.inFlight)

  // Work-generates-work: each of n branches spawns `fan` leaves -> n*(1+fan) items run.
  @Test(timeout = 30000)
  def followOnWork(): Unit = Reps.times:
    val p = new Fan(parallelism = 4, n = 100, fan = 5)
    assertTrue(p.go().isIs)
    assertEquals(100 * 6, p.ran())
    assertEquals(0, p.inFlight)

  // A tiny permit budget must throttle, not deadlock.
  @Test(timeout = 30000)
  def singlePermitNoDeadlock(): Unit = Reps.times:
    val p = new Fan(parallelism = 4, n = 500, fan = 3, maxPermits = Some(1))
    assertTrue(p.go().isIs)
    assertEquals(500 * 4, p.ran())


  // === Errors shut the whole thing down, and teardown still runs ===

  @Test(timeout = 30000)
  def errorShutsDown(): Unit = Reps.times:
    val p = new Fan(parallelism = 2, n = 200, failAt = 137)
    val r = p.go()
    assertTrue(s"expected failure, got $r", r.isAlt)
    assertTrue(p.toreDown())
    // On the error path the engine abandons queued work (drained, not completed), so `inFlight`
    // is diagnostic residue, not guaranteed zero — only the success path zeroes it.

  // `def work() = ???` (a NotImplementedError, an Error not an Exception) must be caught and shut down.
  @Test(timeout = 30000)
  def notImplementedIsCaught(): Unit = Reps.times:
    val p = new Fan(parallelism = 2, n = 200, failAt = 50, bomb = true)
    val r = p.go()
    assertTrue(s"expected failure, got $r", r.isAlt)
    assertTrue(p.toreDown())


  // === Exclusive resources ===

  // With zero workers, resource work is served by the main thread alone (the invariant).
  @Test(timeout = 30000)
  def zeroWorkerServesResource(): Unit = Reps.times:
    val p = new Fan(parallelism = 0, n = 200, useResource = true)
    assertTrue(p.go().isIs)
    assertEquals(200, p.ran())
    assertEquals(0, p.inFlight)
    assertEquals(1, p.opened())               // opened once, lazily
    assertEquals(1, p.closedN())              // closed once, at teardown
    assertEquals(1, p.maxConcurrent())

  // Under contention the resource is never served by two threads at once.
  @Test(timeout = 30000)
  def exclusiveResourceSerialized(): Unit = Reps.times:
    val p = new Fan(parallelism = 4, n = 1000, useResource = true)
    assertTrue(p.go().isIs)
    assertEquals(1000, p.ran())
    assertEquals(1, p.maxConcurrent())        // <- exclusivity
    assertEquals(1, p.opened())
    assertEquals(1, p.closedN())

  // A consumer resource with no work is never opened (lazy), and so never closed.
  @Test(timeout = 30000)
  def resourceOpensLazily(): Unit = Reps.times:
    val p = new Fan(parallelism = 4, n = 0, useResource = true)
    assertTrue(p.go().isIs)
    assertEquals(0, p.opened())
    assertEquals(0, p.closedN())

  // A resource opened during a run that then fails is still closed at teardown.
  @Test(timeout = 30000)
  def resourceClosedOnError(): Unit = Reps.times:
    val p = new Fan(parallelism = 4, n = 200, useResource = true, failAt = 88)
    assertTrue(p.go().isAlt)
    assertEquals(1, p.opened())
    assertEquals(1, p.closedN())
    assertTrue(p.toreDown())


  // === Producer resources ===

  @Test(timeout = 30000)
  def feedProduces(): Unit = Reps.times:
    val f = new Feed(parallelism = 4, m = 500)
    assertTrue(f.go().isIs)
    assertEquals(500, f.ran())
    assertEquals(1, f.opened())               // the producer opened once
    assertEquals(1, f.closedN())

  // A producer is pulled by the main thread alone when there are no workers.
  @Test(timeout = 30000)
  def feedZeroWorker(): Unit = Reps.times:
    val f = new Feed(parallelism = 0, m = 200)
    assertTrue(f.go().isIs)
    assertEquals(200, f.ran())
    assertEquals(1, f.closedN())

  // An `Own` producer: it opens/produces on its dedicated thread, emitted work runs on the pool.
  @Test(timeout = 30000)
  def ownProducerCompletes(): Unit = Reps.times:
    val f = new Feed(parallelism = 4, m = 300, affinity = Affinity.Own)
    assertTrue(f.go().isIs)
    assertEquals(300, f.ran())
    assertEquals(1, f.opened())
    assertEquals(1, f.closedN())

  // A producer whose open() fails retires cleanly (no busy-spin / hang) and the run reports the error.
  @Test(timeout = 30000)
  def producerOpenFailureShutsDown(): Unit = Reps.times:
    val f = new Feed(parallelism = 4, m = 300, affinity = Affinity.Own, failOpen = true)
    assertTrue(f.go().isAlt)
    assertEquals(0, f.opened())
    assertEquals(0, f.closedN())


  // === Affine resources (a fixed, unchanging serving thread) ===

  // `Main` affinity: all work runs on the primary (go-calling) thread, even with workers available.
  @Test(timeout = 30000)
  def mainAffinityRunsOnMain(): Unit = Reps.times:
    val mainId = Thread.currentThread().threadId()
    val p = new Pinned(parallelism = 4, n = 200, Affinity.Main)
    assertTrue(p.go().isIs)
    assertEquals(200, p.ran())
    assertFalse(p.sawOther())
    assertEquals(mainId, p.servingId())
    assertEquals(1, p.opened())
    assertEquals(1, p.closedN())

  // `Own` affinity: all work runs on a single dedicated thread that is NOT the main thread.
  @Test(timeout = 30000)
  def ownAffinityRunsOnOneThread(): Unit = Reps.times:
    val mainId = Thread.currentThread().threadId()
    val p = new Pinned(parallelism = 4, n = 200, Affinity.Own)
    assertTrue(p.go().isIs)
    assertEquals(200, p.ran())
    assertFalse(p.sawOther())
    assertNotEquals(mainId, p.servingId())
    assertTrue(p.servingId() != 0L)
    assertEquals(1, p.opened())
    assertEquals(1, p.closedN())

  // `Own` affinity still completes with zero workers (its dedicated thread does the work).
  @Test(timeout = 30000)
  def ownAffinityZeroWorker(): Unit = Reps.times:
    val mainId = Thread.currentThread().threadId()
    val p = new Pinned(parallelism = 0, n = 200, Affinity.Own)
    assertTrue(p.go().isIs)
    assertEquals(200, p.ran())
    assertFalse(p.sawOther())
    assertNotEquals(mainId, p.servingId())
    assertEquals(1, p.closedN())


  // === Data-grouping sources (standalone utilities) ===

  @Test(timeout = 30000)
  def sortReordersRuns(): Unit =
    val s = new Percolate.Sort[Int]("x", i => i)((_, _) => true)
    assertTrue(s.put(2).isIs)
    assertTrue(s.put(0).isIs)
    s.draw() match
      case Drawn.Items(v) => assertEquals(List(0), v.toList)
      case d              => fail(s"expected [0], got $d")
    assertEquals(Drawn.NotReady, s.draw())              // gap at index 1
    assertTrue(s.put(1).isIs)
    s.draw() match
      case Drawn.Items(v) => assertEquals(List(1, 2), v.toList)
      case d              => fail(s"expected [1,2], got $d")
    assertTrue(s.put(0).isAlt)                          // already past index 0
    s.seal()
    assertEquals(Drawn.Done, s.draw())

  @Test(timeout = 30000)
  def gatherCompletesCategories(): Unit =
    val g = new Percolate.Gather[Int, Boolean](i => i % 2 == 0)(_ => 2)   // 2 items per parity
    assertTrue(g.put(2).isIs)
    assertEquals(Drawn.NotReady, g.draw())
    assertTrue(g.put(4).isIs)                           // even category now full (2, 4)
    g.draw() match
      case Drawn.Items(v) =>
        assertEquals(1, v.length)
        assertEquals(true, v(0)._1)
        assertEquals(List(2, 4), v(0)._2.toList)
      case d => fail(s"expected one even basket, got $d")
    g.seal()
    assertEquals(Drawn.Done, g.draw())

  @Test(timeout = 30000)
  def aggregateFolds(): Unit =
    val a = new Percolate.Aggregate[Int, Boolean, Int](i => i % 2 == 0)(_ => 3)(() => 0)((z, x) => z + x)
    assertTrue(a.put(2).isIs)
    assertTrue(a.put(4).isIs)
    assertEquals(Drawn.NotReady, a.draw())
    assertTrue(a.put(6).isIs)                           // 3rd even → emit the sum (2+4+6)
    a.draw() match
      case Drawn.Items(v) => assertEquals(List(12), v.toList)
      case d              => fail(s"expected [12], got $d")
    a.seal()
    assertEquals(Drawn.Done, a.draw())
}
