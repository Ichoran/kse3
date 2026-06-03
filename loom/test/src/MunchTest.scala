// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025-26 Rex Kerr.

package kse.test.loom

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}
import java.util.concurrent.ConcurrentHashMap

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
    Thread.sleep(20)
    good ! 2                       // survivor still serving AFTER the sibling died
    good ! 3
    Thread.sleep(20)

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
    Thread.sleep(20)
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
    Thread.sleep(10)
    r ! -1            // crash; restart with sum=0
    Thread.sleep(10)
    r ! 5             // fresh muncher, same address
    Thread.sleep(20)
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
    Thread.sleep(20)
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
    Thread.sleep(10)
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
    Thread.sleep(30)
    assertEquals("every connection's socket closed", 20, closed.get())
    assertTrue("all retired", conns.keys.isEmpty)
    sup.stop().foreachThem(_ => ())(e => fail(e.toString))
}
