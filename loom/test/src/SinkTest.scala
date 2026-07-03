// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

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
class SinkTest {
  val Reps = 200

  // === Imperative sends: never block, refuse after close ===

  @Test(timeout = 30000)
  def sinkAcceptsThenClosed(): Unit = Reps.times:
    val got = new AtomicLong(0)
    val sink = Chan.Sink.to[Int](v => got.addAndGet(v.toLong) __ Unit)
    assertEquals(RunStatus.Okay, sink.trySend(1))
    assertEquals(RunStatus.Okay, sink.send(2))              // send never blocks on a sink
    assertTrue(sink.close())
    assertFalse(sink.close())                               // idempotent
    assertEquals(RunStatus.Done, sink.trySend(3))           // refused after close
    assertEquals(3L, got.get())
    assertTrue(sink.isClosed)

  @Test(timeout = 30000)
  def sinkFailSurfaces(): Unit = Reps.times:
    val sink = Chan.Sink.to[Int](_ => ())
    assertTrue(sink.fail(Err("boom")))
    assertTrue(sink.trySend(1) match { case RunStatus.Fail(_) => true; case _ => false })
    assertTrue(sink.isErrored)


  // === Fed by `into` from a source / channel, auto-closed by the cascade ===

  @Test(timeout = 30000)
  def sinkCallbackFromSource(): Unit = Reps.times:
    val got = Collections.synchronizedList(new java.util.ArrayList[Int]())
    val sink = Chan.Sink.to[Int](v => got.add(v) __ Unit)
    val h = Go.session: g ?=>
      Chan.Source(1 to 10).into(sink){ v => v }
    assertTrue(h.await().isIs)
    import scala.jdk.CollectionConverters._
    assertEquals((1 to 10).toSet, got.asScala.toSet)
    assertTrue(sink.isClosed)                               // writer finished -> sink closed

  @Test(timeout = 30000)
  def sinkIntoBuilder(): Unit = Reps.times:
    val buf = scala.collection.mutable.ArrayBuffer.empty[Int]
    val sink = Chan.Sink(buf)                               // Growable -> safe bare apply
    val h = Go.session: g ?=>
      Chan.Source(1 to 20).into(sink){ v => v * 2 }
    assertTrue(h.await().isIs)
    assertEquals((1 to 20).map(_ * 2).toVector, buf.toVector)   // single writer preserves order

  @Test(timeout = 30000)
  def sinkIntoJavaCollection(): Unit = Reps.times:
    val list = Collections.synchronizedList(new java.util.ArrayList[Int]())
    val sink = Chan.Sink(list)
    val ch = Chan[Int](4)
    val h = Go.session: g ?=>
      g.go{ var i = 0; ch.onSendWhile(i < 15){ i += 1; i } }
      ch.into(sink){ v => v }
    assertTrue(h.await().isIs)
    import scala.jdk.CollectionConverters._
    assertEquals((1 to 15).toSet, list.asScala.toSet)

  @Test(timeout = 30000)
  def sinkIntoAccumulator(): Unit = Reps.times:
    val acc = new scala.jdk.AnyAccumulator[Int]
    val sink = Chan.Sink(acc)                               // Accumulator is a Growable
    val h = Go.session: g ?=>
      Chan.Source(1 to 20).into(sink){ v => v * 2 }
    assertTrue(h.await().isIs)
    assertEquals((1 to 20).map(_ * 2).toVector, acc.toVector)
    import scala.jdk.StreamConverters._
    assertEquals(20L, acc.asJavaSeqStream.count())         // bridges back to a Java Stream


  // === Fan-in: two writers into one sink, closed once after both finish ===

  @Test(timeout = 30000)
  def sinkFanInClosesAfterAllWriters(): Unit = Reps.times:
    val count = new AtomicInteger(0)
    val sink = Chan.Sink.to[Int](_ => count.incrementAndGet() __ Unit)
    val h = Go.session: g ?=>
      g.go{ Chan.Source(1 to 50).into(sink){ v => v } }
      g.go{ Chan.Source(1 to 50).into(sink){ v => v } }
    assertTrue(h.await().isIs)
    assertEquals(100, count.get())
    assertTrue(sink.isClosed)                              // closes only after BOTH writers done


  // === Resource cleanup is the scope's job via `Defer`, not the sink's ===

  @Test(timeout = 30000)
  def sinkCleanupViaDefer(): Unit = Reps.times:
    val closed = new AtomicInteger(0)
    val written = Collections.synchronizedList(new java.util.ArrayList[Int]())
    class Res extends AutoCloseable {
      def write(v: Int): Unit = written.add(v) __ Unit
      def close(): Unit = closed.incrementAndGet() __ Unit
    }
    val h = Go.session: g ?=>
      val r = new Res
      Defer { r.close() }
      val sink = Chan.Sink.to[Int](r.write)                // arbitrary consumer -> `to`
      Chan.Source(1 to 10).into(sink){ v => v }
    assertTrue(h.await().isIs)
    assertEquals(10, written.size())
    assertEquals(1, closed.get())


  // === Produced into directly with `put`, bounded by Stop.on ===

  @Test(timeout = 30000)
  def sinkPutGenerator(): Unit = Reps.times:
    val sum = new AtomicLong(0)
    val sink = Chan.Sink.to[Int](v => sum.addAndGet(v.toLong) __ Unit)
    val h = Go.session: g ?=>
      var i = 0
      Stop.on(i >= 100)
      sink.put{ i += 1; i }
    assertTrue(h.await().isIs)
    assertEquals(5050L, sum.get())                         // 1..100
    assertTrue(sink.isClosed)


  // === Imperative relay: `writing` + send, still auto-closed ===

  @Test(timeout = 30000)
  def sinkWritingRelay(): Unit = Reps.times:
    val out = Collections.synchronizedList(new java.util.ArrayList[Int]())
    val sink = Chan.Sink.to[Int](v => out.add(v) __ Unit)
    val in = Chan[Int](4)
    val h = Go.session: g ?=>
      g.go{ var i = 0; in.onSendWhile(i < 10){ i += 1; i } }
      g.go:
        sink.writing
        in.onRecv{ v => sink.send(v * 10) __ Unit }
    assertTrue(h.await().isIs)
    import scala.jdk.CollectionConverters._
    assertEquals((1 to 10).map(_ * 10).toSet, out.asScala.toSet)
    assertTrue(sink.isClosed)
}
