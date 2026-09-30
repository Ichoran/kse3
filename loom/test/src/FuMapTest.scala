// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.test.loom

import java.util.concurrent.ConcurrentLinkedQueue

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics._
import kse.flow._
import kse.maths._
import kse.loom._


@RunWith(classOf[JUnit4])
class FuMapTest {
  val Reps = 50
  private def ok[A](a: A Or Err): A = a.fold(x => x)(e => throw new AssertionError(e.toString))

  @Test(timeout = 30000)
  def idsAreGrantedAndAnswersDelivered(): Unit = Reps.times:
    val m = Fu.IdMap[String]()(30.0.s)
    val a = m.grant()
    val b = m.grant()
    val c = m.grant()
    assertEquals(List(0L, 1L, 2L), List(a.key, b.key, c.key))
    assertEquals(Fu.Delivery.Delivered, m.fulfil(1L, "b"))
    assertEquals(Is(Some("b")), b.await())
    assertEquals("an answered entry leaves at once", 2, m.pending)
    assertEquals("a key answered once is gone", Fu.Delivery.Stray, m.fulfil(1L, "again"))
    assertEquals(Fu.Delivery.Stray, m.fulfil(99L, "never known"))
    m.close(Err("done")) __ Unit

  @Test(timeout = 30000)
  def notBeingAnsweredIsNoneAndLateAnswersAreKnown(): Unit = Reps.times:
    val gaveUp = new ConcurrentLinkedQueue[(Long, Fu.Delivery)]
    val m = Fu.IdMap[String]()(30.0.s, onGiveUp = (k, how) => gaveUp.add((k, how)) __ Unit)
    val byEntry = m.grant()
    val byFu = m.grant()
    val byKey = m.grant()
    val answered = m.grant()
    assertTrue(byEntry.cancel())
    assertFalse("only once", byEntry.cancel())
    assertTrue(byFu.fu.cancel())
    assertTrue(m.giveUp(byKey.key))
    assertEquals(Fu.Delivery.Delivered, m.fulfil(answered.key, "yes"))
    for e <- List(byEntry, byFu, byKey) do assertEquals(Is(None), e.await())
    assertEquals(0, m.pending)
    for e <- List(byEntry, byFu, byKey) do assertEquals("given up on purpose", Fu.Delivery.Withdrawn, m.fulfil(e.key, "too late"))
    assertEquals("late is said once; after that it is a stranger", Fu.Delivery.Stray, m.fulfil(byKey.key, "later still"))
    assertEquals("heard for every give-up, never for an answer", Set(byEntry.key, byFu.key, byKey.key).map(k => (k, Fu.Delivery.Withdrawn)),
      gaveUp.toArray.toSet)
    m.close(Err("done")) __ Unit

  @Test(timeout = 30000)
  def deadlinesExpireWithNoOtherTrafficAndAllAtOnce(): Unit = Reps.times:
    val m = Fu.IdMap[Int]()(30.0.s)
    val soon = Array.fill(200)(m.grant(0.02.s))
    val later = m.grant()
    soon.foreach(e => assertEquals(Is(None), e.await()))            // nothing else happens: the timer alone ends them
    assertEquals("every expired entry went, and only those", 1, m.pending)
    assertEquals("an answer after the deadline is expired, not withdrawn", Fu.Delivery.Expired, m.fulfil(soon(17).key, 17))
    assertEquals(Fu.Delivery.Delivered, m.fulfil(later.key, 5))
    assertEquals(Is(Some(5)), later.await())
    m.close(Err("done")) __ Unit

  @Test(timeout = 30000)
  def closingFailsWhatIsAwaitedAndRefusesMore(): Unit = Reps.times:
    val m = Fu.TimedMap[String, Int](30.0.s)
    val a = ok(m.expect("a"))
    assertTrue("a key already awaited is refused", m.expect("a").isAlt)
    val b = ok(m.expect("b"))
    assertEquals(2, m.close(Err("stream ended")))
    for e <- List(a, b) do assertTrue(e.await().existsAlt(_.toString.contains("stream ended")))
    assertTrue(m.expect("c").existsAlt(_.toString.contains("stream ended")))
    assertEquals(0, m.pending)
    assertEquals("closing withdrew what was awaited", Fu.Delivery.Withdrawn, m.fulfil("a", 1))

  @Test(timeout = 30000)
  def grantsPassOverKeysStillAwaited(): Unit = Reps.times:
    val m = Fu.GrantMap[Int, String](0)(k => (k + 1) % 3)(30.0.s)
    val es = Array.fill(3)(m.grant())
    assertEquals(List(0, 1, 2), es.map(_.key).toList)
    val full = m.grant()
    assertTrue("every key in use: refused at once", full.await().existsAlt(_.toString.contains("every key is awaited")))
    assertEquals(3, m.pending)
    assertEquals(Fu.Delivery.Delivered, m.fulfil(1, "one"))
    assertEquals("the free key comes round again", 1, m.grant().key)
    m.close(Err("done")) __ Unit

  /** Spin (never sleep a fixed time) until `e`'s deadline has passed by the clock. */
  private def pastDeadline(within: Double, since: Long): Unit =
    while (System.nanoTime - since) < (within * 1e9).toLong + 1_000_000L do Thread.onSpinWait()

  @Test(timeout = 30000)
  def withoutAThreadTheCallsKeepTheTime(): Unit = Reps.times:
    val heard = new ConcurrentLinkedQueue[(Long, Fu.Delivery)]
    val m = Fu.IdMap[Int]()(30.0.s, onGiveUp = (k, how) => heard.add((k, how)) __ Unit, threaded = false)
    val idle = m.grant(0.01.s)
    val t0 = System.nanoTime                                       // after the grant, so its deadline is behind t0 + 10 ms
    val kept = m.grant()
    pastDeadline(0.01, t0)
    assertFalse("nothing has touched the map, and nothing else keeps its time", idle.isDone)
    assertEquals("the next call ends what is due", 1, m.pending)
    assertEquals(Is(None), idle.await())
    assertEquals(Fu.Delivery.Expired, m.fulfil(idle.key, 1))
    assertEquals(List((idle.key, Fu.Delivery.Expired)), heard.toArray.toList)
    assertEquals(Fu.Delivery.Delivered, m.fulfil(kept.key, 2))
    assertEquals(Is(Some(2)), kept.await())
    m.close(Err("done")) __ Unit

  @Test(timeout = 30000)
  def withoutAThreadAWaitEndsAtItsDeadline(): Unit = Reps.times:
    val m = Fu.IdMap[Int]()(30.0.s, threaded = false)
    val e = m.grant(0.01.s)
    assertEquals("its own wait ends it", Is(None), e.await())
    val f = m.grant(0.01.s)
    assertEquals("so does a Fu's wait for longer", Is(None), f.fu.await(30.0.s).mapAlt(_ => Err("timed out")))
    val g = m.grant(0.01.s)
    assertEquals("and a CompletableFuture's join", Is(None), g.join())
    assertEquals(0, m.pending)
    val h = m.grant()
    assertTrue("a wait shorter than the deadline is only that", h.fu.await(0.01.s).isAlt)
    assertEquals(Fu.Delivery.Delivered, m.fulfil(h.key, 3))
    assertEquals(Is(Some(3)), h.await())
    m.close(Err("done")) __ Unit

  @Test(timeout = 30000)
  def sweepEndsWhatIsDue(): Unit = Reps.times:
    val m = Fu.TimedMap[String, Int](30.0.s, threaded = false)
    val a = m.expect("a", 0.01.s).fold(x => x)(e => throw new AssertionError(e.toString))
    val t0 = System.nanoTime
    pastDeadline(0.01, t0)
    m.sweep()
    assertTrue(a.isDone)
    assertEquals(Is(None), a.await())
    m.close(Err("done")) __ Unit
}
