// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.test.loom

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics._
import kse.flow._
import kse.loom._


@RunWith(classOf[JUnit4])
class ChanNTest {
  // Each test repeats its body to shake out concurrency races.  `Reps` for cheap tests,
  // `SleepReps` for the few that pace themselves with sleeps.
  val Reps = 200
  val SleepReps = 15

  // === Imperative singles (Chan parity) ===

  @Test(timeout = 30000)
  def singlesTrySendRecv(): Unit = Reps.times:
    val ch = ChanN[Int](2)
    assertEquals(RunStatus.Okay, ch.trySend(1))
    assertEquals(RunStatus.Okay, ch.trySend(2))
    assertEquals(RunStatus.Wait, ch.trySend(3))         // full
    assertEquals(1, ch.tryRecv().getOrElse(_ => -1))
    assertEquals(2, ch.tryRecv().getOrElse(_ => -1))
    assertTrue(ch.tryRecv().isAlt)                        // empty

  @Test(timeout = 30000)
  def singlesNullRoundtrip(): Unit = Reps.times:
    val ch = ChanN[String](2)
    assertEquals(RunStatus.Okay, ch.trySend(null))
    assertNull(ch.tryRecv().getOrElse(_ => "nope"))


  // === Imperative bulk: primitive (generic) and reference (fast-path) arrays ===

  @Test(timeout = 30000)
  def bulkTrySendRecvN(): Unit = Reps.times:
    val ch = ChanN[Int](8)
    val a = Array.tabulate(10)(_ + 1)                     // primitive Array[Int]: generic path
    assertEquals(8, ch.trySendN(a, 0, 10).getOrElse(_ => -1))     // room-limited partial send
    assertTrue(ch.trySendN(a, 8, 10).existsAlt(_ == RunStatus.Wait))
    val out = new Array[Int](16)
    assertEquals(3, ch.tryRecvN(out, 0, 3).getOrElse(_ => -1))
    assertEquals(List(1, 2, 3), out.take(3).toList)
    assertEquals(2, ch.trySendN(a, 8, 10).getOrElse(_ => -1))     // the rest fits now
    assertEquals(7, ch.tryRecvN(out, 0, 16).getOrElse(_ => -1))
    assertEquals((4 to 10).toList, out.take(7).toList)
    assertTrue(ch.tryRecvN(out, 0, 4).existsAlt(_ == RunStatus.Wait))
    assertEquals(0, ch.tryRecvN(out, 0, 0).getOrElse(_ => -1))    // empty ask is trivially satisfied

  @Test(timeout = 30000)
  def bulkRefsAndNulls(): Unit = Reps.times:
    val ch = ChanN[String](8)
    val a = Array("a", null, "c", null)                   // Array[AnyRef]: fast path, with nulls
    assertEquals(4, ch.trySendN(a, 0, 4).getOrElse(_ => -1))
    val out = new Array[String](8)
    assertEquals(4, ch.tryRecvN(out, 0, 8).getOrElse(_ => -1))
    assertEquals("a", out(0))
    assertNull(out(1))
    assertEquals("c", out(2))
    assertNull(out(3))

  @Test(timeout = 30000)
  def bulkCloseDrains(): Unit = Reps.times:
    val ch = ChanN[Int](8)
    val a = Array.tabulate(5)(_ + 1)
    ch.trySendN(a, 0, 5) __ Unit
    assertTrue(ch.close())
    assertEquals(RunStatus.Done, ch.trySend(99))
    assertTrue(ch.trySendN(a, 0, 2).existsAlt(_ == RunStatus.Done))
    val out = new Array[Int](8)
    assertEquals(5, ch.tryRecvN(out, 0, 8).getOrElse(_ => -1))    // buffered items still drain
    assertEquals(List(1, 2, 3, 4, 5), out.take(5).toList)
    assertTrue(ch.tryRecvN(out, 0, 8).existsAlt(_ == RunStatus.Done))
    assertTrue(ch.isComplete)


  // === Full-chunk consumption: nothing until a whole chunk, remainder on close ===

  @Test(timeout = 30000)
  def bulkTryRecvFullN(): Unit = Reps.times:
    val ch = ChanN[Int](16)
    val a = Array.tabulate(16)(_ + 1)
    val out = new Array[Int](32)
    ch.trySendN(a, 0, 3) __ Unit
    assertTrue(ch.tryRecvFullN(out, 0, 8).existsAlt(_ == RunStatus.Wait))   // 3 < 8, still open
    ch.trySendN(a, 3, 8) __ Unit
    assertEquals(8, ch.tryRecvFullN(out, 0, 8).getOrElse(_ => -1))          // exactly a chunk
    assertEquals((1 to 8).toList, out.take(8).toList)
    ch.trySendN(a, 8, 12) __ Unit
    ch.close() __ Unit
    assertEquals(4, ch.tryRecvFullN(out, 0, 8).getOrElse(_ => -1))          // closed: remainder
    assertEquals((9 to 12).toList, out.take(4).toList)
    assertTrue(ch.tryRecvFullN(out, 0, 8).existsAlt(_ == RunStatus.Done))
    assertTrue(ch.isComplete)

  @Test(timeout = 30000)
  def fullChunkClampsToCapacity(): Unit = Reps.times:
    val ch = ChanN[Int](4)
    val a = Array.tabulate(4)(_ + 1)
    val out = new Array[Int](32)
    ch.trySendN(a, 0, 4) __ Unit
    assertEquals(4, ch.tryRecvFullN(out, 0, 32).getOrElse(_ => -1))   // ask > capacity: full = full channel
    assertEquals((1 to 4).toList, out.take(4).toList)

  @Test(timeout = 30000)
  def blockingRecvFullN(): Unit = SleepReps.times:
    val ch = ChanN[Int](16)
    val buf = new Array[Int](8)
    val res = new AtomicInteger(-1)
    val t = new Thread(() => res.set(ch.recvFullN(buf, 0, 8).getOrElse(_ => -2)))
    t.start()
    ch.trySendN(Array.tabulate(5)(_ + 1), 0, 5) __ Unit
    Thread.sleep(50)
    assertEquals(-1, res.get())                                 // 5 < 8: still blocked
    ch.trySendN(Array(6, 7, 8), 0, 3) __ Unit
    t.join(2000)
    assertEquals(8, res.get())
    assertEquals((1 to 8).toList, buf.toList)

  @Test(timeout = 30000)
  def goGetFullWaitsForChunk(): Unit = SleepReps.times:
    val ch = ChanN[Int](64, 8)
    val a = Array.tabulate(12)(_ + 1)
    val got = new AtomicInteger(0)
    val h = Go.session:
      ch.getFull: _ =>
        got.incrementAndGet() __ Unit
    ch.trySendN(a, 0, 5) __ Unit
    Thread.sleep(50)
    assertEquals(0, got.get())                                  // 5 < 8: nothing consumed yet
    ch.trySendN(a, 5, 8) __ Unit                                // now a full chunk exists
    var tries = 0
    while got.get() < 8 && tries < 400 do { Thread.sleep(5); tries += 1 }
    assertEquals(8, got.get())
    ch.trySendN(a, 8, 12) __ Unit
    ch.close() __ Unit                                          // remainder of 4 delivered on close
    assertTrue(h.await().isIs)
    assertEquals(12, got.get())

  @Test(timeout = 30000)
  def goGetFullClampedDelivery(): Unit = Reps.times:
    val a = Array.tabulate(100)(_ + 1)
    val ch = ChanN[Int](4, 8)                                   // chunk of 8 can never assemble in 4
    val collected = new AtomicReference[List[Int]](null)
    val h = Go.session:
      Go:
        ch.putN(a, 0, 100)
      Go:
        var stack = List.empty[Int]
        ch.getFull(8): v =>
          stack = v :: stack
        Defer:
          collected.set(stack.reverse)
    assertTrue(h.await().isIs)
    assertEquals((1 to 100).toList, collected.get())            // clamp to capacity: no deadlock


  // === Blocking, across threads ===

  @Test(timeout = 30000)
  def blockingSinglesAcrossThreads(): Unit = SleepReps.times:
    val ch = ChanN[Int](1)
    val got = new AtomicInteger(-1)
    val t = new Thread(() => ch.recv().foreach(got.set))
    t.start()
    Thread.sleep(50)
    assertEquals(-1, got.get())                           // blocked, nothing yet
    assertEquals(RunStatus.Okay, ch.send(42))
    t.join(2000)
    assertEquals(42, got.get())

  @Test(timeout = 30000)
  def blockingBulkAcrossThreads(): Unit = SleepReps.times:
    val n = 10000
    val ch = ChanN[Int](8)                                // much smaller than the transfer
    val a = Array.tabulate(n)(identity)
    val got = new Array[Int](n)
    val moved = new AtomicInteger(0)
    val t = new Thread(() => {
      val buf = new Array[Int](64)                        // reusable scratch buffer; ends via Done
      var off = 0
      var run = true
      while run do
        ch.recvN(buf, 0, 64).fold{ k =>
          System.arraycopy(buf, 0, got, off, k)
          off += k
        }{ _ => run = false }
      moved.set(off)
    })
    t.start()
    assertEquals(RunStatus.Okay, ch.sendN(a, 0, n))
    ch.close() __ Unit
    t.join(5000)
    assertEquals(n, moved.get())
    assertArrayEquals(a, got)


  // === Go: one-at-a-time put()/get (Chan-like degenerate use) ===

  @Test(timeout = 30000)
  def goPutGetOneAtATime(): Unit = Reps.times:
    val ch = ChanN[String](100)
    val collected = new AtomicReference[List[String]](null)
    val h = Go.session:
      Go:
        var i = 0
        ch.put():
          i += 1
          "eel"
        Stop.on(i >= 100)
      Go:
        var stack = List.empty[String]
        ch.get: x =>
          stack = x :: stack
        Defer:
          collected.set(stack.reverse)
    assertTrue(h.await().isIs)
    val got = collected.get()
    assertEquals(100, got.size)
    assertTrue(got.forall(_ == "eel"))


  // === Go: batched put(n) with quit — exact sequence, nothing hoarded is lost ===

  @Test(timeout = 30000)
  def goPutBatchQuit(): Unit = Reps.times:
    val ch = ChanN[Int](32, 8)
    val collected = new AtomicReference[List[Int]](null)
    val h = Go.session:
      Go:
        ch.put(8): i =>
          shortcut.quit(i >= 100).?
          i + 1
      Go:
        var stack = List.empty[Int]
        ch.get: v =>
          stack = v :: stack
        Defer:
          collected.set(stack.reverse)
    assertTrue(h.await().isIs)
    assertEquals((1 to 100).toList, collected.get())      // in order, complete, no duplicates

  @Test(timeout = 30000)
  def goPutBatchSkip(): Unit = Reps.times:
    val ch = ChanN[Int](32, 8)
    val collected = new AtomicReference[List[Int]](null)
    val h = Go.session:
      Go:
        ch.put(8): i =>
          shortcut.quit(i >= 100).?
          shortcut.skip(i % 2 == 1).?
          i
      Go:
        var stack = List.empty[Int]
        ch.get: v =>
          stack = v :: stack
        Defer:
          collected.set(stack.reverse)
    assertTrue(h.await().isIs)
    assertEquals((0 until 100 by 2).toList, collected.get())


  // === Go: putN drains an array slice and auto-closes ===

  @Test(timeout = 30000)
  def goPutNDrainsArray(): Unit = Reps.times:
    val a = Array.tabulate(1000)(_ + 1)
    val ch = ChanN[Int](32, 8)
    val collected = new AtomicReference[List[Int]](null)
    val h = Go.session:
      Go:
        ch.putN(a, 0, a.length)
      Go:
        var stack = List.empty[Int]
        ch.get: v =>
          stack = v :: stack
        Defer:
          collected.set(stack.reverse)
    assertTrue(h.await().isIs)
    assertEquals((1 to 1000).toList, collected.get())

  @Test(timeout = 30000)
  def goPutNSubrange(): Unit = Reps.times:
    val a = Array.tabulate(100)(_ + 1)
    val ch = ChanN[Int](4, 8)                             // capacity smaller than batch, too
    val collected = new AtomicReference[List[Int]](null)
    val h = Go.session:
      Go:
        ch.putN(a, 10, 20)
      Go:
        var stack = List.empty[Int]
        ch.get: v =>
          stack = v :: stack
        Defer:
          collected.set(stack.reverse)
    assertTrue(h.await().isIs)
    assertEquals((11 to 20).toList, collected.get())


  // === Go: MPMC — every element delivered exactly once ===

  @Test(timeout = 30000)
  def goMpmcExactDelivery(): Unit = Reps.times:
    val producers = 3
    val per = 200
    val a = Array.tabulate(producers * per)(identity)
    val ch = ChanN[Int](64, 8)
    val seen = ConcurrentHashMap.newKeySet[Int]()
    val dups = new AtomicInteger(0)
    val h = Go.session:
      var p = 0
      while p < producers do
        val p0 = p * per
        Go:
          ch.putN(a, p0, p0 + per)
        p += 1
      Go.x(3):
        ch.get: v =>
          if !seen.add(v) then dups.incrementAndGet() __ Unit
    assertTrue(h.await().isIs)
    assertEquals(0, dups.get())
    assertEquals(producers * per, seen.size)


  // === Go: graceful stop mid-stream — no duplicates, clean success ===

  @Test(timeout = 30000)
  def goStopMidstream(): Unit = Reps.times:
    val ch = ChanN[Int](64, 16)
    val seen = ConcurrentHashMap.newKeySet[Int]()
    val dups = new AtomicInteger(0)
    val h = Go.session:
      Go:
        ch.put(16): i =>
          i                                               // unbounded; only the stop ends it
      Go:
        ch.get: v =>
          if !seen.add(v) then dups.incrementAndGet() __ Unit
          if seen.size >= 100 then Stop.session()
    assertTrue(h.await().isIs)
    assertEquals(0, dups.get())
    assertTrue(seen.size >= 100)


  // === Go: an error in a chunked handler tears the tree down ===

  @Test(timeout = 30000)
  def goErrorPropagates(): Unit = Reps.times:
    val ch = ChanN[Int](16, 4)
    val h = Go.session:
      Go:
        ch.put(4): i =>
          i
      Go:
        ch.get: v =>
          if v == 5 then throw new RuntimeException("boom")
    val r = h.await()
    assertTrue(r.isAlt)
    assertTrue(r.altOrElse(_ => Err("")).toString.contains("boom"))


  // === batch = 1 degenerates to ordinary Chan behavior ===

  @Test(timeout = 30000)
  def goBatchOneDegenerates(): Unit = Reps.times:
    val ch = ChanN[Int](4, 1)
    val collected = new AtomicReference[List[Int]](null)
    val h = Go.session:
      Go:
        ch.put(1): i =>
          shortcut.quit(i >= 20).?
          i
      Go:
        var stack = List.empty[Int]
        ch.get: v =>
          stack = v :: stack
        Defer:
          collected.set(stack.reverse)
    assertTrue(h.await().isIs)
    assertEquals((0 until 20).toList, collected.get())


  // === Capacity smaller than the producer batch: partial flushes keep it flowing ===

  @Test(timeout = 30000)
  def goCapacitySmallerThanBatch(): Unit = Reps.times:
    val ch = ChanN[Int](2, 8)
    val collected = new AtomicReference[List[Int]](null)
    val h = Go.session:
      Go:
        ch.put(8): i =>
          shortcut.quit(i >= 50).?
          i
      Go:
        var stack = List.empty[Int]
        ch.get: v =>
          stack = v :: stack
        Defer:
          collected.set(stack.reverse)
    assertTrue(h.await().isIs)
    assertEquals((0 until 50).toList, collected.get())


  // === Throughput sanity (single producer, single consumer, chunked) ===

  @Test(timeout = 30000)
  def throughput(): Unit =
    val n = 200000
    var best = 0.0
    5.times:
      val ch = ChanN[Int](1024, 32)
      val sum = new AtomicLong(0)
      val t0 = System.nanoTime()
      val h = Go.session:
        Go:
          ch.put(32): i =>
            shortcut.quit(i >= n).?
            i + 1
        Go:
          ch.get: v =>
            sum.addAndGet(v.toLong) __ Unit
      assertTrue(h.await().isIs)
      val rate = n / ((System.nanoTime() - t0) / 1e9)
      if rate > best then best = rate
      assertEquals(n.toLong * (n + 1) / 2, sum.get())
    info(f"throughput: best ${best}%.0f items/s over 5 runs of $n")

  private def info(s: String): Unit = println(s"[ChanNTest] $s")
}
