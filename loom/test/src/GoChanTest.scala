// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025 Rex Kerr.

package kse.test.loom

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.Collections

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics._
import kse.flow._
import kse.loom._


@RunWith(classOf[JUnit4])
class GoChanTest {
  // Each test repeats its body to shake out concurrency races.  `Reps` for cheap tests,
  // `SleepReps` for the few that pace themselves with sleeps.
  val Reps = 200
  val SleepReps = 15

  // === Channel basics (imperative, outside any Go block) ===

  @Test(timeout = 30000)
  def channelTrySendRecv(): Unit = Reps.times:
    val ch = Chan[Int](2)
    assertTrue(ch.trySend(1).isIs)
    assertTrue(ch.trySend(2).isIs)
    assertTrue(ch.trySend(3).isAlt)                       // full
    assertEquals(1, ch.tryRecv().getOrElse(_ => -1))
    assertEquals(2, ch.tryRecv().getOrElse(_ => -1))
    assertTrue(ch.tryRecv().isAlt)                        // empty

  @Test(timeout = 30000)
  def channelCloseDrains(): Unit = Reps.times:
    val ch = Chan[Int](4)
    ch.trySend(10) __ Unit
    ch.trySend(20) __ Unit
    assertTrue(ch.close())
    assertTrue(ch.trySend(30).isAlt)                      // no more writes
    assertEquals(10, ch.tryRecv().getOrElse(_ => -1))     // buffered items still drain
    assertEquals(20, ch.tryRecv().getOrElse(_ => -1))
    assertTrue(ch.tryRecv().existsAlt(_ == Chan.Woe.Done))
    assertTrue(ch.isComplete)

  @Test(timeout = 30000)
  def channelBlockingAcrossThreads(): Unit = SleepReps.times:
    val ch = Chan[Int](1)
    val got = new AtomicInteger(-1)
    val t = new Thread(() => ch.recv().foreach(got.set))
    t.start()
    Thread.sleep(50)
    assertEquals(-1, got.get())                           // blocked, nothing yet
    assertTrue(ch.send(42).isIs)
    t.join(2000)
    assertEquals(42, got.get())


  // === Go as a future-with-no-result ===

  @Test(timeout = 30000)
  def goRunsBodyOnce(): Unit = Reps.times:
    val n = new AtomicInteger(0)
    val h = Go { _ ?=> n.incrementAndGet(): Unit }
    assertTrue(h.ask().isIs)
    assertEquals(1, n.get())


  // === Producer -> consumer with automatic close cascade ===

  @Test(timeout = 30000)
  def producerConsumer(): Unit = Reps.times:
    val ch = Chan[Int](4)
    val sum = new AtomicLong(0)
    val h = Go: g ?=>
      g.go:
        var i = 0
        ch.onSendWhile(i < 100){ i += 1; i }
      ch.onRecv{ v => sum.addAndGet(v.toLong) __ Unit }
    assertTrue(h.ask().isIs)
    assertEquals(5050L, sum.get())                        // producer auto-closed -> consumer ended


  // === Fan-in: two writers share a channel (exercises the init barrier) ===

  @Test(timeout = 30000)
  def fanInTwoWriters(): Unit = Reps.times:
    val ch = Chan[Int](4)
    val count = new AtomicInteger(0)
    val sum = new AtomicLong(0)
    val h = Go: g ?=>
      g.go{ var i = 0; ch.onSendWhile(i < 50){ i += 1; i } }
      g.go{ var i = 0; ch.onSendWhile(i < 50){ i += 1; i } }
      ch.onRecv{ v => count.incrementAndGet(); sum.addAndGet(v.toLong) __ Unit }
    assertTrue(h.ask().isIs)
    assertEquals(100, count.get())                        // closes only after BOTH writers done
    assertEquals(2L * 1275, sum.get())


  // === Select over multiple channels in one scope ===

  @Test(timeout = 30000)
  def selectOverChannels(): Unit = Reps.times:
    val a = Chan[Int](2)
    val b = Chan[Int](2)
    val got = Collections.synchronizedList(new java.util.ArrayList[Int]())
    val h = Go: g ?=>
      g.go{ var i = 0; a.onSendWhile(i < 5){ i += 1; i } }
      g.go{ var i = 0; b.onSendWhile(i < 5){ i += 1; 100 + i } }
      a.onRecv{ v => got.add(v) __ Unit }
      b.onRecv{ v => got.add(v) __ Unit }
    assertTrue(h.ask().isIs)
    assertEquals(10, got.size())


  // === Transform pipeline: recv from one channel, send to another ===

  @Test(timeout = 30000)
  def transformPipeline(): Unit = Reps.times:
    val in = Chan[Int](4)
    val out = Chan[Int](4)
    val results = Collections.synchronizedList(new java.util.ArrayList[Int]())
    val h = Go: g ?=>
      g.go{ var i = 0; in.onSendWhile(i < 10){ i += 1; i } }     // produce 1..10
      g.go:
        out.writing                                              // declare we write to `out`
        in.onRecv{ v => out.send(v * 2) __ Unit }                // transform in -> out
      out.onRecv{ v => results.add(v) __ Unit }                  // collect
    assertTrue(h.ask().isIs)
    import scala.jdk.CollectionConverters._
    assertEquals((1 to 10).map(_ * 2).toSet, results.asScala.toSet)


  // === Stateful accumulation persists across loop iterations ===

  @Test(timeout = 30000)
  def statefulAccumulator(): Unit = Reps.times:
    val ch = Chan[Int](4)
    val finalSum = new AtomicInteger(-1)
    val h = Go: g ?=>
      g.go{ var i = 0; ch.onSendWhile(i < 20){ i += 1; i } }
      var sum = 0                                                // lives for the whole scope
      ch.onRecv{ v => sum += v; finalSum.set(sum) }
    assertTrue(h.ask().isIs)
    assertEquals(210, finalSum.get())


  // === Error in a handler propagates and tears the tree down ===

  @Test(timeout = 30000)
  def errorPropagates(): Unit = Reps.times:
    val ch = Chan[Int](2)
    val h = Go: g ?=>
      g.go{ var i = 0; ch.onSendWhile(i < 1000){ i += 1; i } }
      ch.onRecv{ v => if v == 5 then throw new RuntimeException("boom") }
    val r = h.ask()
    assertTrue(r.isAlt)
    assertTrue(r.altOrElse(_ => "").toString.contains("boom"))


  // === Explicit stop shuts the whole tree down, successfully ===

  @Test(timeout = 30000)
  def explicitStop(): Unit = Reps.times:
    val ch = Chan[Int](16)
    val seen = new AtomicInteger(0)
    val h = Go: g ?=>
      g.go{ var i = 0; ch.onSendWhile(i < 10_000_000){ i += 1; i } }   // effectively unbounded
      ch.onRecv{ v => if seen.incrementAndGet() >= 10 then g.stop() }
    assertTrue(h.ask().isIs)                                   // graceful stop -> success
    assertTrue(seen.get() >= 10)


  // === cancel() unwinds even an unbounded producer with no consumer ===

  @Test(timeout = 30000)
  def cancelUnwinds(): Unit = SleepReps.times:
    val ch = Chan[Int](2)
    val h = Go: g ?=>
      // producer with no consumer: fills the channel then blocks forever
      g.go{ var i = 0; ch.onSendWhile(i < 10_000_000){ i += 1; i } }
    Thread.sleep(50)
    assertFalse(h.isComplete)
    h.cancel()
    val r = h.ask()
    assertTrue(r.isAlt)                                        // cancellation surfaces as an error


  // === Throughput sanity (single producer, single consumer) ===

  @Test(timeout = 30000)
  def throughput(): Unit =
    val n = 200000
    var best = 0.0
    5.times:
      val ch = Chan[Int](1024)
      val sum = new AtomicLong(0)
      val t0 = System.nanoTime()
      val h = Go: g ?=>
        g.go{ var i = 0; ch.onSendWhile(i < n){ i += 1; i } }
        ch.onRecv{ v => sum.addAndGet(v.toLong) __ Unit }
      assertTrue(h.ask().isIs)
      val rate = n / ((System.nanoTime() - t0) / 1e9)
      if rate > best then best = rate
      assertEquals(n.toLong * (n + 1) / 2, sum.get())
    info(f"throughput: best ${best}%.0f items/s over 5 runs of $n")

  private def info(s: String): Unit = println(s"[GoChanTest] $s")
}
