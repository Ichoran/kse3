// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.test.loom

import java.util.concurrent.atomic.AtomicLong

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics._
import kse.flow._
import kse.loom._


@RunWith(classOf[JUnit4])
class FoldMapTest {
  val Reps = 100

  private def all[U, K, V](fm: FoldMap[U, K, V]): List[(K, V)] =
    var out = List.empty[(K, V)]
    var more = true
    while more do fm.tryRecv().fold(it => out = (it.key, it.value) :: out)(_ => more = false)
    out.reverse

  final case class Ev(path: String, n: Int)

  @Test
  def latestKeepsPlaceUnlessMovedToBack(): Unit =
    val kept = FoldMap.latest[Ev, String](8)(_.path)
    for e <- List(Ev("a", 1), Ev("b", 1), Ev("a", 2)) do assertEquals(RunStatus.Okay, kept.send(e))
    assertEquals(List("a" -> Ev("a", 2), "b" -> Ev("b", 1)), all(kept))
    val moved = FoldMap.latest[Ev, String](8, moveToBack = true)(_.path)
    for e <- List(Ev("a", 1), Ev("b", 1), Ev("a", 2)) do moved.send(e) __ Unit
    assertEquals(List("b" -> Ev("b", 1), "a" -> Ev("a", 2)), all(moved))

  @Test
  def firstAndCountAreFolds(): Unit =
    val once = FoldMap.first[Ev, String](8)(_.path)
    for e <- List(Ev("a", 1), Ev("a", 2), Ev("b", 3)) do once.send(e) __ Unit
    assertEquals(List("a" -> Ev("a", 1), "b" -> Ev("b", 3)), all(once))
    val counts = FoldMap.count[Ev, String](8)(_.path)
    for e <- List(Ev("a", 1), Ev("a", 2), Ev("b", 3), Ev("a", 4)) do counts.send(e) __ Unit
    assertEquals(List("a" -> 3L, "b" -> 1L), all(counts))
    // an accumulation, spelled out: the backpressure is on keys, and the fold may grow as it likes
    val lists = FoldMap[Ev, String, List[Int]](8)(_.path)(e => Is(e.n :: Nil))((e, ns) => Is(e.n :: ns))
    for e <- List(Ev("a", 1), Ev("a", 2), Ev("b", 3)) do lists.send(e) __ Unit
    assertEquals(List("a" -> List(2, 1), "b" -> List(3)), all(lists))

  @Test
  def unitFromPutQueuesNothingAndFromJoinWithdraws(): Unit =
    // created then deleted cancels out; a deletion of nothing pending starts nothing
    val fm = FoldMap[Ev, String, Int](2)(_.path)(e => if e.n < 0 then Alt.unit else Is(e.n))((e, _) => if e.n < 0 then Alt.unit else Is(e.n))
    fm.send(Ev("a", 1)) __ Unit
    fm.send(Ev("b", 1)) __ Unit
    assertEquals("full of distinct keys", RunStatus.Wait, fm.trySend(Ev("c", 1)))
    fm.send(Ev("a", -1)) __ Unit                                     // withdraws a, freeing its slot
    assertEquals(1, fm.size)
    assertEquals(RunStatus.Okay, fm.trySend(Ev("c", 1)))
    assertEquals("a new key waits for room before put is asked", RunStatus.Wait, fm.trySend(Ev("z", -1)))
    assertEquals(List("b" -> 1, "c" -> 1), all(fm))
    assertEquals(RunStatus.Okay, fm.trySend(Ev("z", -1)))           // with room, put refuses it: nothing starts
    assertEquals(0, fm.size)

  @Test
  def backpressureIsOnNewKeysOnly(): Unit =
    val fm = FoldMap.count[String, String](2)(identity)
    assertEquals(RunStatus.Okay, fm.trySend("a"))
    assertEquals(RunStatus.Okay, fm.trySend("b"))
    assertEquals(RunStatus.Wait, fm.trySend("c"))
    for _ <- 1 to 1000 do assertEquals("folding into a pending key never waits", RunStatus.Okay, fm.trySend("a"))
    assertEquals("an explicit key folds as its own would", RunStatus.Okay, fm.trySend("b", "z"))
    assertEquals(List("a" -> 1001L, "b" -> 2L), all(fm))

  @Test
  def drainTakesTheRoundAndCloseKeepsWhatIsPending(): Unit =
    val fm = FoldMap.latest[Ev, String](8)(_.path)
    assertEquals(0, fm.drain().fold(_.length)(_ => -1))
    for e <- List(Ev("a", 1), Ev("b", 1), Ev("a", 2)) do fm.send(e) __ Unit
    assertEquals(List("a", "b"), fm.drain().fold(_.toList.map(_.key))(_ => Nil))
    fm.send(Ev("c", 1)) __ Unit
    assertTrue(fm.close())
    assertEquals(RunStatus.Done, fm.trySend(Ev("d", 1)))
    assertEquals(List("c" -> Ev("c", 1)), all(fm))
    assertTrue(fm.isComplete)
    assertTrue(fm.drain().existsAlt(_ == RunStatus.Done))

  @Test
  def aThrowingFoldFailsThatSendAlone(): Unit =
    val fm = FoldMap[Ev, String, Int](8)(_.path)(e => Is(e.n))((e, v) => if e.n == 13 then throw new Exception("unlucky") else Is(v + e.n))
    fm.send(Ev("a", 1)) __ Unit
    assertTrue(fm.trySend(Ev("a", 13)) match { case RunStatus.Fail(_) => true; case _ => false })
    fm.send(Ev("a", 2)) __ Unit
    assertEquals(List("a" -> 3), all(fm))

  @Test(timeout = 30000)
  def inAGoPipelineEveryItemIsCounted(): Unit = Reps.times:
    val events = Chan[Ev](16)
    val counts = FoldMap.count[Ev, String](4)(_.path)
    val total = new AtomicLong(0)
    val h = Go.session: g ?=>
      Go:
        var i = 0
        events.onSendWhile(i < 10_000){ i += 1; Ev(s"p${i % 5}", i) }
      Go:
        events.into(counts)(identity)                                // a plain ChanOut
      Go:
        counts.onRecv{ it => total.addAndGet(it.value) __ Unit }
    assertEquals(Is.unit, h.await())
    assertEquals(10_000L, total.get())

  @Test(timeout = 30000)
  def aStoppedReaderReleasesItsWriters(): Unit = Reps.times:
    val counts = FoldMap.count[Int, Int](2)(identity)
    val h = Go.session: g ?=>
      Go:
        var i = 0
        counts.onSendWhile(true){ i += 1; i }                        // new keys forever: only a refusal ends it
      Go:
        var seen = 0
        Stop.on(seen >= 3)
        counts.onRecv{ _ => seen += 1 }
    assertEquals(Is.unit, h.await())
}
