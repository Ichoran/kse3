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
    val lists = FoldMap[Ev, String, List[Int]](8)(_.path)(e => Is(Some(e.n :: Nil)))((e, ns) => Is(Some(e.n :: ns)))
    for e <- List(Ev("a", 1), Ev("a", 2), Ev("b", 3)) do lists.send(e) __ Unit
    assertEquals(List("a" -> List(2, 1), "b" -> List(3)), all(lists))

  @Test
  def noneFromPutQueuesNothingAndFromJoinWithdraws(): Unit =
    // created then deleted cancels out; a deletion of nothing pending starts nothing
    val fm = FoldMap[Ev, String, Int](2)(_.path)(e => Is(if e.n < 0 then None else Some(e.n)))((e, _) => Is(if e.n < 0 then None else Some(e.n)))
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
    val fm = FoldMap[Ev, String, Int](8)(_.path)(e => Is(Some(e.n)))((e, v) => if e.n == 13 then throw new Exception("unlucky") else Is(Some(v + e.n)))
    fm.send(Ev("a", 1)) __ Unit
    assertTrue(fm.trySend(Ev("a", 13)) match { case RunStatus.Fail(e) => FoldMap.Refused.of(e).isDefined; case _ => false })
    fm.send(Ev("a", 2)) __ Unit
    assertEquals(List("a" -> 3), all(fm))

  private def status(r: Ask[FoldMap.Status]): String = r.fold(_.toString)(e => FoldMap.Refused.of(e).fold(s"failed: $e")(x => s"refused: ${x.error}"))

  @Test
  def foldSaysWhatBecameOfTheItem(): Unit =
    // negative withdraws or filters; 13 is an error in either; a key starting with "x" is one we cannot name
    val fm = FoldMap[Ev, String, Int](2)(e => if e.path.startsWith("x") then throw new Exception("no key") else e.path)(
      e => if e.n == 13 then Err.or("bad start") else Is(if e.n < 0 then None else Some(e.n)))(
      (e, v) => if e.n == 13 then Err.or("bad fold") else Is(if e.n < 0 then None else Some(v + e.n)))
    assertEquals("Taken", status(fm.tryFold(Ev("a", 1))))
    assertEquals("Taken", status(fm.tryFold(Ev("a", 2))))
    assertEquals("Filtered", status(fm.tryFold(Ev("b", -1))))
    assertEquals("refused: bad start", status(fm.tryFold(Ev("b", 13))))
    assertEquals("refused: bad fold", status(fm.tryFold(Ev("a", 13))))
    assertTrue(status(fm.tryFold(Ev("x", 1))).startsWith("refused: "))
    assertEquals("a refusal leaves the channel as it was", 1, fm.size)
    assertEquals("Taken", status(fm.tryFold(Ev("b", 5))))
    assertEquals("Wait", status(fm.tryFold(Ev("c", 1))))
    assertEquals("Withdrawn", status(fm.tryFold(Ev("a", -1))))
    assertEquals("Taken", status(fm.fold(Ev("c", 1))))
    assertEquals(List("b" -> 5, "c" -> 1), all(fm))
    assertTrue(fm.close())
    assertEquals("Done", status(fm.tryFold(Ev("d", 1))))
    val failed = FoldMap.count[String, String](2)(identity)
    assertTrue(failed.fail(Err("broken")))
    assertEquals("the channel's own failure is no refusal", "failed: broken", status(failed.tryFold("a")))

  @Test
  def aFullValueWaitsAsAFullChannelDoes(): Unit = Reps.times:
    // at most three items per key, and key "big" takes five
    val fm = FoldMap[String, String, Int](4)(identity)(_ => Is(Some(1)))((_, n) => Is(Some(n + 1)), full = (k, n) => n >= (if k == "big" then 5 else 3))
    for _ <- 1 to 3 do assertEquals("Taken", status(fm.tryFold("a")))
    assertEquals("Wait", status(fm.tryFold("a")))
    assertEquals(RunStatus.Wait, fm.trySend("a"))
    for _ <- 1 to 5 do assertEquals("Taken", status(fm.tryFold("big")))
    assertEquals("Wait", status(fm.tryFold("big")))
    assertEquals("another key is not held up", "Taken", status(fm.tryFold("b")))
    val later = Fu{ fm.fold("a") }                                   // blocks until "a" is taken
    // one atomic take, so the waiting fold cannot slip a new "a" in between two receives
    assertEquals(List("a" -> 3, "big" -> 5, "b" -> 1), fm.drain().fold(_.toList.map(it => it.key -> it.value))(_ => Nil))
    assertEquals(Is(FoldMap.Status.Taken), later.await().flatMap(__))
    assertEquals(List("a" -> 1), all(fm))

  @Test
  def onTakeFollowsWhatLeaves(): Unit =
    // a side table of what is pending, kept by put, join and onTake alone -- all under the channel's lock
    val shadow = scala.collection.mutable.Set.empty[String]
    val fm = FoldMap[String, String, Int](8)(identity)(k => { shadow += k; Is(Some(1)) })(
      (k, n) => if n >= 2 then { shadow -= k; Is(None) } else Is(Some(n + 1)), onTake = (k, _) => shadow -= k)
    for k <- List("a", "b", "c", "c", "c") do fm.send(k) __ Unit       // the third "c" withdraws it
    assertEquals(Set("a", "b"), shadow.toSet)
    fm.tryRecv() __ Unit
    assertEquals(Set("b"), shadow.toSet)
    fm.send("d") __ Unit
    assertEquals(List("b", "d"), fm.drain().fold(_.toList.map(_.key))(_ => Nil))
    assertEquals(Set.empty[String], shadow.toSet)

  @Test
  def aThrowingOnTakeFailsTheChannelButHandsTheValueOver(): Unit =
    val fm = FoldMap[String, String, Int](8)(identity)(_ => Is(Some(1)))((_, n) => Is(Some(n + 1)), onTake = (k, _) => if k == "bad" then throw new Exception("lost track"))
    fm.send("bad") __ Unit
    fm.send("b") __ Unit
    assertEquals(Is("bad"), fm.tryRecv().map(_.key).mapAlt(_ => Err("none")))
    assertTrue("the state onTake keeps is unknown now", fm.trySend("c") match { case RunStatus.Fail(e) => FoldMap.Refused.of(e).isEmpty; case _ => false })
    assertTrue(fm.state match { case Chan.State.Errored(e) => e.toString.contains("lost track"); case _ => false })

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
