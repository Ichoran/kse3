// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.test.loom

// Every example in GUIDE-loom.md lives here between a `// guide: name` and `// guide: end` pair,
// so that the guide's code is compiled and run.  `check-guides.py` verifies the two stay identical.

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.basics.{given, *}
import kse.flow.{given, *}
import kse.maths.{given, *}
import kse.loom.{given, *}

object GuideExamples {

  def port(s: String): Ask[Int] = Ask:
    val n = s.toInt
    if n < 1 || n > 65535 then Err ?# s"port out of range: $n"
    n

  def portPlain(s: String): Either[String, Int] =
    try
      val n = s.toInt
      if n < 1 || n > 65535 then Left(s"port out of range: $n") else Right(n)
    catch
      case e: NumberFormatException => Left(s"bad port: ${e.getMessage}")

  def futuresPlain(x: String, y: String): Either[String, Int] =
    // guide: fu.plain
    import scala.concurrent.{Await, Future}
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.*
    val a = Future(portPlain(x))
    val b = Future(portPlain(y))
    val sum = for ea <- a; eb <- b yield for va <- ea; vb <- eb yield va + vb
    Await.result(sum, 10.seconds)
    // guide: end

  def futuresKse3(x: String, y: String): Ask[Int] =
    // guide: fu.kse3
    val a = Fu{ port(x).? }                   // a Fu[Int]: the body may leave with an Err via .?
    val b = Fu{ port(y).? }
    val sum = Fu{ a.? + b.? }                 // waits for both, or carries the first error
    sum.await()                               // an Ask[Int]
    // guide: end

  def fanOut(items: Array[String]): Ask[Array[Int]] =
    // guide: fuarray.kse3
    val fus = items.map(s => Fu{ port(s).? })
    fus.fu().await()                          // every value, or the first error
    // guide: end


  def waitingPlain(): (() => Unit, () => Unit) =
    // guide: sync.plain
    val lock = new Object
    var ready = false
    def waitReady(): Unit = lock.synchronized { while !ready do lock.wait() }
    def signal(): Unit = lock.synchronized { ready = true; lock.notifyAll() }
    // guide: end
    (() => waitReady(), () => signal())

  def waitingKse3(): (() => Unit, () => Unit) =
    // guide: sync.kse3
    val lock = Sync()
    val w = lock.waiter()
    var ready = false
    def waitReady(): Unit = lock { while !ready do w(1.s_nano) __ Unit }   // every wait is bounded and re-checked
    def signal(): Unit = lock { ready = true; w.!!! }
    // guide: end
    (() => waitReady(), () => signal())

  def threaded(): Ask[Int] =
    // guide: threaded.kse3
    val t = Threaded{ (1 to 100).sum }        // a Thread with a result
    t.await()                                 // Ask[Int]: the value, or what the thread failed with
    // guide: end


  def pipelinePlain(n: Int): Long =
    // guide: gochan.plain
    val q = new java.util.concurrent.ArrayBlockingQueue[Int](4)
    val total = new java.util.concurrent.atomic.AtomicLong(0)
    val producer = new Thread(() => { var i = 0; while i < n do { i += 1; q.put(i) }; q.put(-1) })
    val consumer = new Thread(() => { var v = q.take(); while v >= 0 do { total.addAndGet(v): Unit; v = q.take() } })
    producer.start()
    consumer.start()
    producer.join()
    consumer.join()
    total.get
    // guide: end

  def pipelineKse3(n: Int): Long =
    // guide: gochan.kse3
    val ch = Chan[Int](4)
    val total = Atom(0L)
    val h = Go.session: g ?=>
      Go:
        var i = 0
        ch.onSendWhile(i < n){ i += 1; i }     // producer; ch closes by itself when this task ends
      ch.onRecv{ v => total += v }             // consumer; ends once ch is drained and closed
    h.await() __ Unit                          // Ask[Unit]: success, or the first failure in the tree
    total()
    // guide: end

  def transform(names: Array[String]): (Int, Int) =
    // guide: gointo.kse3
    val words = Chan[String](8)
    val lengths = Chan[Int](8)
    var longest = 0
    var count = 0
    val h = Go.session: g ?=>
      Go:
        var i = 0
        words.onSendWhile(i < names.length){ i += 1; names(i - 1) }
      Go:
        words.into(lengths)(_.length)          // reads words, writes lengths, closes lengths after
      Go:
        Defer{ count = -count }                // runs when this task ends, however it ends
        Stop.on(longest >= 6)                  // a graceful exit condition, checked each loop
        lengths.onRecv{ n => count += 1; if n > longest then longest = n }
    // guide: end
    h.await() __ Unit
    (longest, count)


  def imperative(): Ask[Int] =
    // guide: chan.kse3
    val ch = Chan[Int](2)
    val summer = Threaded:
      var s = 0
      var more = true
      while more do ch.recv().fold(v => s += v)(_ => more = false)   // the Alt is Wait, Done, or Fail
      s
    ch.send(1) __ Unit                         // blocks while full; the RunStatus says Okay or why not
    ch.send(2) __ Unit
    ch.send(3) __ Unit
    ch.close() __ Unit                         // the receiver drains 3 then sees Done
    summer.await()
    // guide: end


  // guide: percolate.kse3
  class SumSquares(n: Int, parallelism: Int) extends Percolate(parallelism) {
    val total = Atom(0L)
    private var next = 1
    def setup(): Ask[Unit] = Is.unit
    def teardown(): Ask[Unit] = Is.unit
    def newWork(): Ask[Work] = Is:              // called until it answers Work.Empty
      if next > n then Work.Empty
      else
        val k = next
        next += 1
        new Work() { def work(): Ask[Array[Work]] = { total += k.toLong * k; Is(Work.none) } }
  }
  // guide: end

  def percolate(): (Long, Boolean) =
    // guide: percolaterun.kse3
    val p = SumSquares(100, 4)
    val timing = p.go()                       // runs to completion on 4 workers plus this thread
    // guide: end
    (p.total(), timing.isIs)


  def deque(): (Int, Int, Int) =
    // guide: splitdeque.kse3
    val d = SplitDeque.empty[Int]
    100.visit(i => d.pushRight(i))
    val first = d.popLeft()                   // Int Or Unit
    val batch = d.splitLeft(10)               // the next 10, moved out in O(blockSize * log n)
    val moved = batch.length                  // a Batch is a plain deque of its own
    d.spliceRight(batch)                      // and back on the other end; the batch is empty again
    // guide: end
    (first.getOrElse(_ => -1), moved, d.length)


  def munch(): Ask[Long] =
    // guide: munch.kse3
    enum Job:
      case Add(n: Int)
      case Total(reply: Munch.Reply[Long])
    val sup = Munch.supervisor()
    val jobs = sup.registry[String, Job]("jobs")
    val worker = jobs.spawn("main"):
      var total = 0L                              // private state: only this muncher's thread ever touches it
      (job: Job) => job match
        case Job.Add(n) => total += n
        case Job.Total(reply) => reply(total)
    worker ! Job.Add(5)                           // fire-and-forget, from any thread
    worker ! Job.Add(7)
    val t = worker.feed(Job.Total(_)).await()     // a request with a typed reply, as an Ask[Long]
    sup.stop() __ Unit                            // drains the mailboxes, then ends every muncher
    // guide: end
    t
}


@RunWith(classOf[JUnit4])
class GuideTest {
  import kse.basics.testutilities.TestUtilities.{_, given}
  import GuideExamples as G

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  @Test
  def futuresAndWaitingTest(): Unit =
    T ~ G.futuresPlain("80", "443") ==== Right(523)
    T ~ G.futuresKse3("80", "443") ==== 523
    T ~ G.futuresPlain("80", "eel").isLeft ==== G.futuresKse3("80", "eel").isAlt
    T ~ G.fanOut(Array("1", "2", "3")).map(_.toList) ==== List(1, 2, 3)
    T ~ G.fanOut(Array("1", "x", "3")).isAlt ==== true
    for (waitReady, signal) <- List(G.waitingPlain(), G.waitingKse3()) do
      val t = Threaded{ waitReady(); "woke" }
      Thread.sleep(20)
      signal()
      T ~ t.await() ==== "woke"
    T ~ G.threaded() ==== 5050

  @Test
  def channelsTest(): Unit =
    T ~ G.pipelinePlain(100) ==== G.pipelineKse3(100)
    T ~ G.pipelineKse3(100) ==== 5050L
    T ~ G.transform(Array("eel", "cod", "salmon", "gar")) ==== (6, -3)
    T ~ G.imperative() ==== 6

  @Test
  def enginesTest(): Unit =
    T ~ G.percolate() ==== (338350L, true)
    T ~ G.deque() ==== (0, 10, 99)
    T ~ G.munch() ==== 12L
}
