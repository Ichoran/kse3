// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.loom

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicIntegerArray}
import java.util.Collections

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics._
import kse.flow._
import kse.loom._


@RunWith(classOf[JUnit4])
class SourceTest {
  val Reps = 200

  // === Imperative pulls: never block, exhaust to Done ===

  @Test(timeout = 30000)
  def sourcePullsThenDone(): Unit = Reps.times:
    val s = Chan.Source(Array(1, 2, 3))
    assertEquals(1, s.tryRecv().getOrElse(_ => -1))
    assertEquals(2, s.recv().getOrElse(_ => -1))            // recv never blocks on a source
    assertEquals(3, s.tryRecv().getOrElse(_ => -1))
    assertTrue(s.tryRecv().existsAlt(_ == RunStatus.Done))  // exhausted
    assertTrue(s.isComplete)

  @Test(timeout = 30000)
  def sourceOverSlice(): Unit = Reps.times:
    val s = Chan.Source(Array(10, 20, 30, 40, 50), 1, 4)
    assertEquals(20, s.tryRecv().getOrElse(_ => -1))
    assertEquals(30, s.tryRecv().getOrElse(_ => -1))
    assertEquals(40, s.tryRecv().getOrElse(_ => -1))
    assertTrue(s.tryRecv().isAlt)

  @Test(timeout = 30000)
  def sourcePassesNullThrough(): Unit = Reps.times:
    val s = Chan.Source(Array[String]("a", null, "b"))
    assertEquals("a", s.tryRecv().getOrElse(_ => "X"))
    assertNull(s.tryRecv().getOrElse(_ => "X"))             // null is a value, not the end marker
    assertEquals("b", s.tryRecv().getOrElse(_ => "X"))
    assertTrue(s.tryRecv().isAlt)

  // === close() hard-drops the remainder (unlike Chan.close) ===

  @Test(timeout = 30000)
  def sourceCloseDropsRemainder(): Unit = Reps.times:
    val s = Chan.Source(Array(1, 2, 3, 4, 5))
    assertEquals(1, s.tryRecv().getOrElse(_ => -1))
    assertTrue(s.close())
    assertFalse(s.close())                                  // idempotent
    assertTrue(s.tryRecv().existsAlt(_ == RunStatus.Done))  // 2..5 abandoned
    assertTrue(s.isComplete)

  // === fail() surfaces an error to subsequent receives ===

  @Test(timeout = 30000)
  def sourceFailSurfaces(): Unit = Reps.times:
    val s = Chan.Source(Array(1, 2, 3))
    assertEquals(1, s.tryRecv().getOrElse(_ => -1))
    assertTrue(s.fail(Err("boom")))
    assertTrue(s.tryRecv().existsAlt{ case RunStatus.Fail(_) => true; case _ => false })
    assertTrue(s.isErrored)


  // === Consumed inside a Go session via `get` ===

  @Test(timeout = 30000)
  def sourceGetSums(): Unit = Reps.times:
    val s = Chan.Source(1 to 100)                           // Iterable backing
    val sum = new AtomicLong(0)
    val h = Go.session: g ?=>
      s.get{ v => sum.addAndGet(v.toLong) __ Unit }
    assertTrue(h.await().isIs)
    assertEquals(5050L, sum.get())

  @Test(timeout = 30000)
  def sourceGetFromIterator(): Unit = Reps.times:
    val sum = new AtomicLong(0)
    val h = Go.session: g ?=>
      Chan.Source((1 to 50).iterator).get(8){ v => sum.addAndGet(v.toLong) __ Unit }  // batched pulls
    assertTrue(h.await().isIs)
    assertEquals(1275L, sum.get())

  @Test(timeout = 30000)
  def sourceGetFromJavaIteratorAndEnumeration(): Unit = Reps.times:
    val list = java.util.Arrays.asList(1, 2, 3, 4, 5)
    val a = new AtomicLong(0)
    val b = new AtomicLong(0)
    val h = Go.session: g ?=>
      Chan.Source(list.iterator()).get{ v => a.addAndGet(v.toLong) __ Unit }
      Chan.Source(Collections.enumeration(list)).get{ v => b.addAndGet(v.toLong) __ Unit }
    assertTrue(h.await().isIs)
    assertEquals(15L, a.get())
    assertEquals(15L, b.get())


  // === One source, many consumers: each element delivered exactly once ===

  @Test(timeout = 30000)
  def sourceParallelDrainExactlyOnce(): Unit = Reps.times:
    val N = 500
    val seen = new AtomicIntegerArray(N)
    val count = new AtomicInteger(0)
    val s = Chan.Source(Array.range(0, N))
    val h = Go.session: g ?=>
      Go.x(8):
        s.get{ v => seen.incrementAndGet(v); count.incrementAndGet() __ Unit }
    assertTrue(h.await().isIs)
    assertEquals(N, count.get())
    var i = 0
    while i < N do { assertEquals(1, seen.get(i)); i += 1 }


  // === Relay a source into a real channel with `into` (backpressure + auto-close cascade) ===

  @Test(timeout = 30000)
  def sourceIntoChannel(): Unit = Reps.times:
    val out = Chan[Int](4)
    val results = Collections.synchronizedList(new java.util.ArrayList[Int]())
    val h = Go.session: g ?=>
      g.go:
        Chan.Source(1 to 10).into(out){ v => v * 2 }        // source -> out, doubling
      out.onRecv{ v => results.add(v) __ Unit }
    assertTrue(h.await().isIs)
    import scala.jdk.CollectionConverters._
    assertEquals((1 to 10).map(_ * 2).toSet, results.asScala.toSet)


  // === Generator via `from(() => A Or Unit)`, exhausts on Alt.unit ===

  @Test(timeout = 30000)
  def sourceFromFunction(): Unit = Reps.times:
    val sum = new AtomicLong(0)
    val h = Go.session: g ?=>
      var i = 0
      Chan.Source.from{ () => i += 1; if i <= 100 then Is(i) else Alt.unit }
        .get{ v => sum.addAndGet(v.toLong) __ Unit }
    assertTrue(h.await().isIs)
    assertEquals(5050L, sum.get())


  // === Standard-library / Java-Stream interop: Stepper (apply), Stream & Spliterator (from) ===

  @Test(timeout = 30000)
  def sourceFromStepper(): Unit = Reps.times:
    val s = Chan.Source(List("a", "bb", "ccc").stepper)    // Stepper is bare apply
    assertEquals("a", s.tryRecv().getOrElse(_ => "X"))
    assertEquals("bb", s.tryRecv().getOrElse(_ => "X"))
    assertEquals("ccc", s.tryRecv().getOrElse(_ => "X"))
    assertTrue(s.tryRecv().isAlt)

  @Test(timeout = 30000)
  def sourceFromJavaStream(): Unit = Reps.times:
    val got = Collections.synchronizedList(new java.util.ArrayList[String]())
    val h = Go.session: g ?=>
      Chan.Source.from(java.util.Arrays.asList("a", "b", "c", "d").stream())
        .get{ v => got.add(v) __ Unit }
    assertTrue(h.await().isIs)
    import scala.jdk.CollectionConverters._
    assertEquals(Set("a", "b", "c", "d"), got.asScala.toSet)

  @Test(timeout = 30000)
  def sourceFromSpliterator(): Unit = Reps.times:
    val s = Chan.Source.from(java.util.Arrays.asList("x", "y", "z").spliterator())
    assertEquals("x", s.tryRecv().getOrElse(_ => "?"))
    assertEquals("y", s.tryRecv().getOrElse(_ => "?"))
    assertEquals("z", s.tryRecv().getOrElse(_ => "?"))
    assertTrue(s.tryRecv().isAlt)


  // === Resource cleanup is the scope's job via `Defer`, not the source's ===

  @Test(timeout = 30000)
  def sourceCleanupViaDefer(): Unit = Reps.times:
    val closed = new AtomicInteger(0)
    class Res extends AutoCloseable {
      val it = (1 to 5).iterator
      def close(): Unit = closed.incrementAndGet() __ Unit
    }
    val sum = new AtomicLong(0)
    val h = Go.session: g ?=>
      val r = new Res
      Defer { r.close() }                                  // released at scope end, however it ends
      Chan.Source(r.it).get{ v => sum.addAndGet(v.toLong) __ Unit }
    assertTrue(h.await().isIs)
    assertEquals(15L, sum.get())
    assertEquals(1, closed.get())


  // === A source selected alongside a live channel ===

  @Test(timeout = 30000)
  def sourceSelectedWithChannel(): Unit = Reps.times:
    val ch = Chan[Int](4)
    val fromSrc = new AtomicLong(0)
    val fromCh = new AtomicLong(0)
    val h = Go.session: g ?=>
      g.go{ var i = 0; ch.onSendWhile(i < 20){ i += 1; i } }
      Chan.Source(1 to 20).get{ v => fromSrc.addAndGet(v.toLong) __ Unit }
      ch.onRecv{ v => fromCh.addAndGet(v.toLong) __ Unit }
    assertTrue(h.await().isIs)
    assertEquals(210L, fromSrc.get())
    assertEquals(210L, fromCh.get())
}
