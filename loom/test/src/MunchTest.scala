// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025-26 Rex Kerr.

package kse.test.loom

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch}

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics._
import kse.flow._
import kse.loom._


@RunWith(classOf[JUnit4])
class MunchTest {
  val Reps = 100

  // The per-connection protocol: blocking Data, a feed for Stats, an explicit Drain.
  enum ConnMsg:
    case Data(bytes: Int)
    case Stats(reply: Munch.Reply[ConnStats])
    case Drain
  case class ConnStats(served: Int, bytes: Long)


  // === Basic tell delivery ===

  @Test(timeout = 30000)
  def tellDelivered(): Unit = Reps.times:
    val sup = Munch.supervisor()
    val acc = new AtomicLong(0)
    val reg = sup.registry[String, Int]()
    val r = reg.spawn("adder"){ (n: Int) => acc.addAndGet(n.toLong) __ Unit }
    for i <- 1 to 100 do r ! i
    sup.stop().foreachThem(_ => ())(e => fail(e.toString))     // drains everything before returning
    assertEquals(5050L, acc.get())


  // === Typed feed round-trips, and .? integrates ===

  @Test(timeout = 30000)
  def feedReturnsTyped(): Unit = Reps.times:
    val sup = Munch.supervisor()
    val conns = sup.registry[Int, ConnMsg]("conn")
    val c = conns.spawn(7){
      var served = 0
      var bytes = 0L
      (m: ConnMsg) => m match
        case ConnMsg.Data(b)    => served += 1; bytes += b
        case ConnMsg.Stats(rep) => rep := ConnStats(served, bytes)
        case ConnMsg.Drain      => Stop()
    }
    c ! ConnMsg.Data(10)
    c ! ConnMsg.Data(20)
    Ask{ conns(7).feed(ConnMsg.Stats(_)).? }.foreachThem{ stats =>
      assertEquals(2, stats.served)
      assertEquals(30L, stats.bytes)
    }{ e => fail(e.toString) }
    sup.stop() __ Unit


  // === THE THESIS: one muncher fails; siblings keep serving; its cleanup ran; error is collected ===

  @Test(timeout = 30000)
  def failureIsolation(): Unit = Reps.times:
    val sup = Munch.supervisor()
    val cleaned = new AtomicInteger(0)
    val survivorSum = new AtomicLong(0)
    val conns = sup.registry[String, Int]("worker", onError = Munch.Supervise.stop)

    // a "poison" muncher: throws on -1, and has a cleanup that MUST run on death
    val bad = conns.spawn("bad"){
      Defer { cleaned.incrementAndGet() __ Unit }
      n => if n < 0 then throw new RuntimeException("boom") else ()
    }
    // an independent survivor
    val good = conns.spawn("good"){ (n: Int) => survivorSum.addAndGet(n.toLong) __ Unit }

    good ! 1
    bad ! -1                       // kills only `bad`
    soon { conns.get("bad").isEmpty && cleaned.get() == 1 }
    good ! 2                       // survivor still serving AFTER the sibling died
    good ! 3
    soon { survivorSum.get() == 6L }

    assertFalse("bad must be gone", conns.get("bad").isDefined)
    assertTrue("good must still be live", conns.get("good").isDefined)
    assertEquals("bad's cleanup ran exactly once", 1, cleaned.get())
    assertEquals("survivor unaffected by sibling failure", 6L, survivorSum.get())
    assertTrue("error was collected by the supervisor", sup.errors.exists(_.toString.contains("boom")))
    sup.stop() __ Unit


  // === Feed to a never-spawned target fails cleanly (no hang), surfaced via .? ===

  @Test(timeout = 30000)
  def feedToGoneIsCleanError(): Unit = Reps.times:
    val sup = Munch.supervisor()
    val conns = sup.registry[Int, ConnMsg]("conn")
    val outcome: ConnStats Or Err = Ask{ conns(99).feed(ConnMsg.Stats(_)).? }
    assertTrue(outcome.isAlt)
    sup.stop() __ Unit


  // === Feed after a muncher has retired fails cleanly rather than hanging ===

  @Test(timeout = 30000)
  def feedAfterRetireFails(): Unit = Reps.times:
    val sup = Munch.supervisor()
    val conns = sup.registry[Int, ConnMsg]("conn")
    val c = conns.spawn(1){
      (m: ConnMsg) => m match
        case ConnMsg.Drain      => Stop()
        case ConnMsg.Stats(rep) => rep := ConnStats(1, 1)
        case ConnMsg.Data(_)    => ()
    }
    c ! ConnMsg.Drain
    soon { conns.get(1).isEmpty }
    val r = conns(1).feed(ConnMsg.Stats(_)).await()
    assertTrue(r.isAlt)
    sup.stop() __ Unit


  // === Restart keeps the address but resets state (Erlang permanent) ===

  @Test(timeout = 30000)
  def restartKeepsAddressResetsState(): Unit = Reps.times:
    val sup = Munch.supervisor()
    val reg = sup.registry[String, Int]("counter", onError = Munch.Supervise.restart)
    val seen = new AtomicReference[List[Int]](Nil)
    val r = reg.spawn("c"){
      var sum = 0
      n =>
        if n < 0 then throw new RuntimeException("reset")     // crash → restart → fresh sum
        else { sum += n; seen.updateAndGet(sum :: _) __ Unit }
    }
    r ! 5
    r ! 5
    r ! -1            // crash; restart with sum=0 on the same mailbox, so order holds
    r ! 5             // fresh muncher, same address
    soon { seen.get().size == 3 }
    assertTrue("same address still live after restart", reg.get("c").isDefined)
    val s = seen.get()
    assertTrue(s"running sums seen: $s", s.contains(10) && s.contains(5))
    assertFalse("state reset: 15 never reached", s.contains(15))
    sup.stop() __ Unit


  // === getOrSpawn materializes on demand (the lazy keyed-actor idiom) ===

  @Test(timeout = 30000)
  def getOrSpawnOnDemand(): Unit = Reps.times:
    val sup = Munch.supervisor()
    val tallies = new ConcurrentHashMap[String, AtomicLong]()
    val reg = sup.registry[String, Long]("acct")
    def bump(key: String, amt: Long): Unit =
      reg.getOrSpawn(key){
        val box = tallies.computeIfAbsent(key, _ => new AtomicLong(0))
        n => box.addAndGet(n) __ Unit
      } ! amt
    bump("alice", 10); bump("alice", 5); bump("bob", 100)
    soon { tallies.size == 2 && tallies.get("alice").get() == 15L && tallies.get("bob").get() == 100L }
    assertEquals(15L, tallies.get("alice").get())
    assertEquals(100L, tallies.get("bob").get())
    assertEquals(Set("alice", "bob"), reg.keys)
    sup.stop() __ Unit


  // === "Use it if there, else …" routes through Option without ceremony ===

  @Test(timeout = 30000)
  def absentTargetIsFirstClass(): Unit = Reps.times:
    val sup = Munch.supervisor()
    val reg = sup.registry[String, Int]("svc")
    val deliveredTo = new AtomicReference[String]("none")
    reg.spawn("live"){ (_: Int) => deliveredTo.set("live") } __ Unit

    reg.get("live").foreach(_ ! 1)                                 // present → used
    assertEquals("used",     reg.get("live").fold("absent")(_ => "used"))
    assertEquals("fellback", reg.get("ghost").fold("fellback")(_ => "used"))  // absent → fall back
    soon { deliveredTo.get() == "live" }
    assertEquals("live", deliveredTo.get())
    sup.stop() __ Unit


  // === Integration: the full per-connection scenario, control-plane feed included ===

  @Test(timeout = 30000)
  def perConnectionScenario(): Unit = (Reps / 4).times:
    val sup = Munch.supervisor()
    val conns = sup.registry[Int, ConnMsg]("conn", onError = Munch.Supervise.stop)
    val closed = new AtomicInteger(0)

    // accept 20 connections, each does blocking-ish work per Data, retires on Drain
    def accept(id: Int): Munch.Ref[ConnMsg] =
      conns.spawn(id){
        var served = 0
        var bytes = 0L
        Defer { closed.incrementAndGet() __ Unit }   // "sock.close()"
        (m: ConnMsg) => m match
          case ConnMsg.Data(b)    => served += 1; bytes += b; Thread.`yield`()   // stand-in for blocking I/O
          case ConnMsg.Stats(rep) => rep := ConnStats(served, bytes)
          case ConnMsg.Drain      => Stop()
      }

    val refs = (1 to 20).map(accept)
    refs.foreach(r => { r ! ConnMsg.Data(100); r ! ConnMsg.Data(50) })

    // control plane addresses a specific connection at runtime and feeds it
    Ask{ conns(7).feed(ConnMsg.Stats(_)).? }.foreachThem{ s7 =>
      assertEquals(2, s7.served)
      assertEquals(150L, s7.bytes)
    }{ e => fail(e.toString) }

    refs.foreach(_ ! ConnMsg.Drain)        // graceful retire
    soon { closed.get() == 20 && conns.keys.isEmpty }
    assertEquals("every connection's socket closed", 20, closed.get())
    assertTrue("all retired", conns.keys.isEmpty)
    sup.stop().foreachThem(_ => ())(e => fail(e.toString))


  // === cancel() reaches a handler blocked in an interruptible call: the stop is what gets recorded,
  //     the muncher's Defers run, and nothing is printed or lost ===

  @Test(timeout = 30000)
  def cancelInterruptsBlockedHandler(): Unit = (Reps / 10).times:
    val sup = Munch.supervisor()
    val reg = sup.registry[String, Int]("blocked")
    val cleaned = new AtomicInteger(0)
    val blocked = new CountDownLatch(1)
    val ref = reg.spawn("a"){
      Defer { cleaned.incrementAndGet() __ Unit }
      (i: Int) => { blocked.countDown(); Thread.sleep(10_000) }
    }
    ref ! 1
    blocked.await()
    val t0 = System.nanoTime
    val r = sup.cancel()
    val ms = (System.nanoTime - t0) / 1e6
    assertTrue(s"cancel took $ms ms", ms < 2000)
    assertTrue(s"expected the stop as the error, got $r", r.existsAlt(_.toString.contains("supervisor cancelled")))
    assertFalse(s"the interruption is the stop, not an error of its own: $r", r.existsAlt(_.toString.contains("InterruptedException")))
    assertEquals(1, cleaned.get())
    assertFalse(ref.isLive)

  // === cancel() of a muncher idle in recv records the useful cause, not the interrupt text ===

  @Test(timeout = 30000)
  def cancelIdleRecordsCause(): Unit = (Reps / 10).times:
    val sup = Munch.supervisor()
    val reg = sup.registry[String, Int]("idle")
    reg.spawn("a"){ (i: Int) => () } __ Unit             // blocks in recv on an empty mailbox
    Thread.sleep(20)
    val r = sup.cancel()
    assertTrue(s"expected the cancel cause, got $r", r.existsAlt(_.toString.contains("supervisor cancelled")))
    assertFalse(s"the channel cause wins over the interrupt text: $r", r.existsAlt(_.toString.contains("interrupted while")))


  // === cancel() nags: a Defer that blocks past the nag period is cut, and the ones after it still run ===

  @Test(timeout = 30000)
  def cancelNagsBlockedDefer(): Unit = 3.times:
    val sup = Munch.supervisor()
    val reg = sup.registry[String, Int]("slow")
    val after = new AtomicInteger(0)
    val served = new CountDownLatch(1)
    val ref = reg.spawn("a"){
      Defer { after.incrementAndGet() __ Unit }          // registered first: runs last, after the blocking one
      Defer { Thread.sleep(10_000) }
      (i: Int) => served.countDown()
    }
    ref ! 1
    served.await()
    val t0 = System.nanoTime
    val r = sup.cancel()
    val ms = (System.nanoTime - t0) / 1e6
    assertTrue(s"cancel took $ms ms", ms < 2000)
    assertEquals(1, after.get())
    assertFalse(s"the interruption is the stop, not an error of its own: $r", r.existsAlt(_.toString.contains("InterruptedException")))


  // === Controls that say "not this any more", apart from those that stop a muncher ===

  // A muncher whose Hold(gate) message blocks until the test opens the gate; Take feeds answer their number,
  // blocking first for `slow` of them (the ones a cancel should cut short); the decider escalates, so a
  // cancellation mistaken for a fault would stop the whole supervisor.
  enum Q:
    case Hold(started: CountDownLatch, gate: CountDownLatch)
    case Take(n: Int, reply: Munch.Reply[Int])
    case Slow(started: CountDownLatch, reply: Munch.Reply[Int])
    case Tell(n: Int)
    case Spin(started: CountDownLatch, saw: AtomicInteger, reply: Munch.Reply[Int])

  private def queued(sup: Munch.Supervisor, seen: java.util.Queue[Int]): Munch.Ref[Q] =
    sup.registry[String, Q]("q", onError = Munch.Supervise.escalate).spawn("a"){
      (m: Q) => m match
        case Q.Hold(s, g)       => s.countDown(); g.await()
        case Q.Take(n, r)       => seen.add(n) __ Unit; r := n
        case Q.Slow(s, r)       => s.countDown(); Thread.sleep(10_000); r := -1   // the loser: only an interrupt ends it early
        case Q.Tell(n)          => seen.add(n) __ Unit
        case Q.Spin(s, saw, r)  =>
          s.countDown()
          while !Munch.isCancelled do Thread.onSpinWait()                   // never blocks, so only the poll can see it
          saw.incrementAndGet() __ Unit
          r := -2
    }

  private def failedWith(r: Ask[Int], word: String): Boolean = r.existsAlt(_.toString.contains(word))

  @Test(timeout = 30000)
  def withdrawRemovesOnlyWhatHasNotStarted(): Unit = (Reps / 10).times:
    val sup = Munch.supervisor()
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[Int]
    val ref = queued(sup, seen)
    val started = new CountDownLatch(1)
    val gate = new CountDownLatch(1)
    ref ! Q.Hold(started, gate)
    started.await()
    val queuedFeed = ref.feed(Q.Take(1, _))
    assertTrue("a queued feed is withdrawn", queuedFeed.withdraw())
    assertFalse("and only once", queuedFeed.withdraw())
    assertTrue(failedWith(queuedFeed.await(), "withdrawn"))
    gate.countDown()
    assertEquals(Is(2), ref.feed(Q.Take(2, _)).await())
    assertEquals(List(2), seen.toArray.toList)                            // the withdrawn one never ran
    // a feed already running is not withdrawn: it finishes and answers
    val slowStarted = new CountDownLatch(1)
    val gate2 = new CountDownLatch(1)
    ref ! Q.Hold(slowStarted, gate2)
    slowStarted.await()
    val later = ref.feed(Q.Take(3, _))
    gate2.countDown()
    assertEquals(Is(3), later.await())
    assertFalse("an answered feed cannot be withdrawn", later.withdraw())
    assertTrue(ref.isLive)
    assertEquals(Nil, sup.errors)
    sup.stop() __ Unit

  @Test(timeout = 30000)
  def cancelStopsOnlyThatMessage(): Unit = (Reps / 10).times:
    val sup = Munch.supervisor()
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[Int]
    val ref = queued(sup, seen)
    val started = new CountDownLatch(1)
    val slow = ref.feed(Q.Slow(started, _))
    started.await()
    assertTrue("a running feed is cancelled", slow.cancel())
    assertTrue(failedWith(slow.await(), "cancelled"))
    // the interrupt was for that message alone: the next one blocks on a gate and is not cut
    val s2 = new CountDownLatch(1)
    val gate = new CountDownLatch(1)
    ref ! Q.Hold(s2, gate)
    s2.await()
    val next = ref.feed(Q.Take(7, _))
    gate.countDown()
    assertEquals(Is(7), next.await())
    assertFalse("an answered feed cannot be cancelled", next.cancel())
    assertTrue("the muncher went on", ref.isLive)
    assertEquals("a cancellation is not a fault: nothing recorded, no escalation", Nil, sup.errors)
    // a plain Fu of a feed cancels the feed too
    val s3 = new CountDownLatch(1)
    val viaFu = ref.feed(Q.Slow(s3, _)).fu
    s3.await()
    assertTrue(viaFu.cancel())
    assertTrue(viaFu.await().existsAlt(_.toString.contains("cancelled")))
    assertEquals(Is(8), ref.feed(Q.Take(8, _)).await())
    sup.stop().foreachThem(_ => ())(e => fail(e.toString))

  @Test(timeout = 30000)
  def aFedCanBeAwaitedWithATimeout(): Unit = (Reps / 10).times:
    import kse.maths._
    val sup = Munch.supervisor()
    val ref = queued(sup, new java.util.concurrent.ConcurrentLinkedQueue[Int])
    val started = new CountDownLatch(1)
    val slow = ref.feed(Q.Slow(started, _))
    started.await()
    assertEquals("time ran out: None, and the feed is still in flight", Alt(None), slow.await(0.02.s))
    assertTrue(slow.cancel())
    assertEquals(Is(4), ref.feed(Q.Take(4, _)).await(10.0.s))
    sup.stop() __ Unit

  @Test(timeout = 30000)
  def aHandlerThatNeverBlocksSeesCancellationByPolling(): Unit = (Reps / 10).times:
    val sup = Munch.supervisor()
    val ref = queued(sup, new java.util.concurrent.ConcurrentLinkedQueue[Int])
    val started = new CountDownLatch(1)
    val saw = new AtomicInteger(0)
    val spin = ref.feed(Q.Spin(started, saw, _))
    started.await()
    assertTrue(spin.cancel())
    assertEquals(Is(9), ref.feed(Q.Take(9, _)).await())                 // the spin ended, and left no interrupt behind
    assertEquals(1, saw.get())
    assertEquals(Nil, sup.errors)
    sup.stop() __ Unit

  @Test(timeout = 30000)
  def priorityGoesNextAndHoldsOneSlot(): Unit = (Reps / 10).times:
    val sup = Munch.supervisor()
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[Int]
    val ref = queued(sup, seen)
    val started = new CountDownLatch(1)
    val gate = new CountDownLatch(1)
    ref ! Q.Hold(started, gate)
    started.await()
    ref ! Q.Tell(1)
    ref ! Q.Tell(2)
    assertTrue(ref.priority(Q.Tell(99)))
    assertFalse("one slot: a second is refused while the first waits", ref.priority(Q.Tell(100)))
    gate.countDown()
    assertEquals(Is(3), ref.feed(Q.Take(3, _)).await())
    assertEquals(List(99, 1, 2, 3), seen.toArray.toList)
    // a muncher parked on an empty mailbox wakes for one
    assertTrue(ref.priority(Q.Tell(42)))
    soon(seen.contains(42))
    assertTrue(seen.contains(42))
    sup.stop() __ Unit

  @Test(timeout = 30000)
  def clearDropsTheQueueButNotTheMuncher(): Unit = (Reps / 10).times:
    val sup = Munch.supervisor()
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[Int]
    val ref = queued(sup, seen)
    val started = new CountDownLatch(1)
    val gate = new CountDownLatch(1)
    ref ! Q.Hold(started, gate)
    started.await()
    ref ! Q.Tell(1)
    val f = ref.feed(Q.Take(2, _))
    ref ! Q.Tell(3)
    assertTrue(ref.priority(Q.Tell(50)))
    assertEquals(3, ref.clear())
    assertTrue(failedWith(f.await(), "withdrawn"))
    gate.countDown()
    ref ! Q.Tell(4)
    assertEquals(Is(5), ref.feed(Q.Take(5, _)).await())
    assertEquals("the priority message stayed; the queue went", List(50, 4, 5), seen.toArray.toList)
    assertTrue(ref.isLive)
    sup.stop() __ Unit

  @Test(timeout = 30000)
  def stopAndAbortEndOneMuncher(): Unit = (Reps / 10).times:
    val sup = Munch.supervisor()
    val reg = sup.registry[String, Int]("ends")
    val served = new AtomicInteger(0)
    val cleaned = new AtomicInteger(0)
    val graceful = reg.spawn("g"){ Defer { cleaned.incrementAndGet() __ Unit }; (i: Int) => served.addAndGet(i) __ Unit }
    val other = reg.spawn("o"){ (i: Int) => served.addAndGet(1000 * i) __ Unit }
    graceful ! 1
    graceful ! 2
    assertTrue(graceful.stop())
    assertFalse("gone from its address as soon as it stops", graceful.isLive)
    soon(served.get() == 3 && cleaned.get() == 1)
    assertEquals("stop handles what was queued", 3, served.get())
    assertFalse("already gone", graceful.stop())
    val started = new CountDownLatch(1)
    val hard = reg.spawn("h"){ Defer { cleaned.incrementAndGet() __ Unit }; (i: Int) => { started.countDown(); Thread.sleep(10_000) } }
    hard ! 1
    started.await()
    assertTrue(hard.abort())
    soon(!hard.isLive)
    assertFalse(hard.isLive)
    soon(cleaned.get() == 2)
    assertEquals("both ran their cleanups", 2, cleaned.get())
    assertTrue("the abort is on record", sup.errors.exists(_.toString.contains("aborted")))
    other ! 1
    soon(served.get() >= 1000)
    assertTrue("a sibling goes on", other.isLive && served.get() >= 1000)
    sup.stop() __ Unit


  /** Waits, up to five seconds, for something the munchers do asynchronously, so the assertion after it
    * checks the settled state instead of racing a slow or loaded runner. */
  private def soon(ready: => Boolean): Unit =
    val deadline = System.nanoTime + 5_000_000_000L
    while !ready && System.nanoTime < deadline do Thread.sleep(1)
}
