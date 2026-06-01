// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025 Rex Kerr.

package kse.test.loom

import java.util.concurrent.{CountDownLatch, ThreadLocalRandom, TimeUnit, CyclicBarrier}
import java.util.concurrent.atomic.{AtomicInteger, AtomicBoolean}

import scala.collection.mutable.ArrayBuffer

import kse.loom._

object GoTestImpl {
  /*
  def runAllTests(): Unit = {
    testBasicSendReceive()
    testBlockingWhenFull()
    testBlockingWhenEmpty()
    testCloseChannel()
    testRetireChannel()
    testCloseWithError()
    testTrySendReceive()
    testMultipleProducersConsumers()
    testSelectReceive()
    testSelectSend()
    testSelectWithTimeout()
    testSelectMixed()
    testGoUtilities()
    testFanOut()
    testFanIn()
    testPipeline()
    testRingBufferWraparound()
    testConcurrentStress()
    testSelectStress()
    performanceBenchmark()
    println("All Go channel tests passed!")
  }

  def testBasicSendReceive(): Unit = {
    val chan = new Chan[Int](10)

    assert(chan.send(42))
    assert(chan.receive() == Some(42))
    assert(chan.tryReceive() == None)
  }

  def testBlockingWhenFull(): Unit = {
    val chan = new Chan[Int](2)

    assert(chan.send(1))
    assert(chan.send(2))

    val sendThread = Go {
      chan.send(3)
      chan.send(4)
    }

    Thread.sleep(50)
    assert(chan.receive() == Some(1))
    assert(chan.receive() == Some(2))
    assert(chan.receive() == Some(3))
    assert(chan.receive() == Some(4))

    sendThread.join()
  }

  def testBlockingWhenEmpty(): Unit = {
    val chan = new Chan[Int](10)
    val received = new AtomicInteger(0)

    val receiveThread = Go {
      chan.receive().foreach(received.set)
    }

    Thread.sleep(50)
    assert(received.get() == 0)

    chan.send(42)
    Thread.sleep(50)
    assert(received.get() == 42)

    receiveThread.join()
  }

  def testCloseChannel(): Unit = {
    val chan = new Chan[Int](10)

    chan.send(1)
    chan.send(2)
    chan.close()

    assert(!chan.send(3))
    assert(chan.receive() == Some(1))
    assert(chan.receive() == Some(2))
    assert(chan.receive() == None)
  }

  def testRetireChannel(): Unit = {
    val chan = new Chan[Int](10)

    chan.send(1)
    chan.retire()

    assert(!chan.isOpen)
    assert(!chan.send(2))
    assert(chan.receive() == Some(1))
    assert(chan.tryReceive() == None)
  }

  def testCloseWithError(): Unit = {
    val chan = new Chan[String](5)
    val error = new RuntimeException("Test error")

    chan.send("hello")
    chan.closeWith(error)

    assert(chan.receive() == Some("hello"))
    assert(chan.receive() == None)
    assert(chan.isClosed)

    chan.getState match {
      case Chan.State.Closed(Some(e)) => assert(e eq error)
      case _ => sys.error("Expected closed state with error")
    }
  }

  def testTrySendReceive(): Unit = {
    val chan = new Chan[Int](2)

    assert(chan.trySend(1))
    assert(chan.trySend(2))
    assert(!chan.trySend(3))

    assert(chan.tryReceive() == Some(1))
    assert(chan.tryReceive() == Some(2))
    assert(chan.tryReceive() == None)
  }

  def testMultipleProducersConsumers(): Unit = {
    val chan = new Chan[Int](10)
    val numProducers = 5
    val numConsumers = 3
    val itemsPerProducer = 100
    val totalItems = numProducers * itemsPerProducer

    val received = new AtomicInteger(0)
    val latch = new CountDownLatch(numProducers + numConsumers)

    for (i <- 0 until numProducers) {
      Go {
        for (j <- 0 until itemsPerProducer) {
          chan.send(i * itemsPerProducer + j)
        }
        latch.countDown()
      }
    }

    for (_ <- 0 until numConsumers) {
      Go {
        var continue = true
        while (continue) {
          chan.receive() match {
            case Some(_) => received.incrementAndGet()
            case None => continue = false
          }
        }
        latch.countDown()
      }
    }

    Thread.sleep(100)
    chan.close()

    assert(latch.await(5, TimeUnit.SECONDS))
    val actualReceived = received.get()
    assert(actualReceived == totalItems, s"Expected $totalItems but got $actualReceived")
  }

  def testSelectReceive(): Unit = {
    val chan1 = new Chan[String](5)
    val chan2 = new Chan[String](5)
    val chan3 = new Chan[String](5)

    chan2.send("from chan2")

    Select.receive(chan1, chan2, chan3) match {
      case Some(Select.Received(ch, value)) =>
        assert(ch eq chan2)
        assert(value == "from chan2")
      case _ => sys.error("Expected to receive from chan2")
    }

    val result = new AtomicBoolean(false)
    val thread = Go {
      Select.receive(chan1, chan2, chan3) match {
        case Some(Select.Received(ch, value)) =>
          if ((ch eq chan1) && value == "test") {
            result.set(true)
          }
        case _ =>
      }
    }

    Thread.sleep(50)
    chan1.send("test")
    thread.join(1000)
    assert(result.get())
  }

  def testSelectSend(): Unit = {
    val chan1 = new Chan[Int](1)
    val chan2 = new Chan[Int](1)

    chan1.send(1)

    Select.send(chan1, chan2) match {
      case Some(Select.Ready(ch)) =>
        assert(ch eq chan2)
        assert(ch.send(42))
      case _ => sys.error("Expected chan2 to be ready")
    }

    chan2.send(2)

    val thread = Go {
      Thread.sleep(50)
      chan1.close()
      chan2.close()
    }

    assert(Select.send(chan1, chan2) == None)
    thread.join()
  }

  def testSelectWithTimeout(): Unit = {
    val chan1 = new Chan[Int](5)
    val chan2 = new Chan[Int](5)

    val start = System.currentTimeMillis()
    val result = Select(
      receiveFrom = Seq(chan1, chan2),
      sendTo = Seq.empty,
      timeoutMs = 100
    )
    val elapsed = System.currentTimeMillis() - start

    assert(result == Select.Timeout)
    assert(elapsed >= 100 && elapsed < 200, s"Timeout took $elapsed ms")
  }

  def testSelectMixed(): Unit = {
    val recvChan = new Chan[String](5)
    val sendChan = new Chan[String](1)

    recvChan.send("data")

    Select(
      receiveFrom = Seq(recvChan),
      sendTo = Seq(sendChan),
      timeoutMs = 1000
    ) match {
      case Select.Received(ch, value) =>
        assert(ch eq recvChan)
        assert(value.asInstanceOf[String] == "data")
      case _ => sys.error("Expected to receive")
    }

    Select(
      receiveFrom = Seq.empty,
      sendTo = Seq(sendChan),
      timeoutMs = 1000
    ) match {
      case Select.Ready(ch) =>
        assert(ch eq sendChan)
        assert(ch.send("test"))
      case _ => sys.error("Expected send to be ready")
    }
  }

  def testGoUtilities(): Unit = {
    val counter = new AtomicInteger(0)
    val threads = Go.all(
      () => counter.incrementAndGet(),
      () => counter.incrementAndGet(),
      () => counter.incrementAndGet()
    )

    threads.foreach(_.join())
    assert(counter.get() == 3)

    counter.set(0)
    Go.join(
      () => counter.incrementAndGet(),
      () => counter.incrementAndGet()
    )
    assert(counter.get() == 2)
  }

  def testFanOut(): Unit = {
    val source = new Chan[Int](10)
    val target1 = new Chan[Int](10)
    val target2 = new Chan[Int](10)

    val fanOutThread = Go.fanOut(source, target1, target2)

    source.send(1)
    source.send(2)
    source.send(3)
    source.close()

    fanOutThread.join()

    assert(target1.receive() == Some(1))
    assert(target1.receive() == Some(2))
    assert(target1.receive() == Some(3))
    assert(target1.receive() == None)

    assert(target2.receive() == Some(1))
    assert(target2.receive() == Some(2))
    assert(target2.receive() == Some(3))
    assert(target2.receive() == None)
  }

  def testFanIn(): Unit = {
    val source1 = new Chan[Int](5)
    val source2 = new Chan[Int](5)
    val target = new Chan[Int](10)

    val fanInThread = Go.fanIn(Seq(source1, source2), target)

    source1.send(1)
    source1.send(2)
    source2.send(3)
    source2.send(4)

    source1.close()
    source2.close()

    fanInThread.join()

    val results = ArrayBuffer[Int]()
    var continue = true
    while (continue) {
      target.receive() match {
        case Some(value) => results += value
        case None => continue = false
      }
    }

    assert(results.toSet == Set(1, 2, 3, 4))
  }

  def testPipeline(): Unit = {
    val source = new Chan[Int](5)
    val target = new Chan[String](5)

    val pipelineThread = Go.pipeline(source, (x: Int) => s"Value: $x", target)

    source.send(1)
    source.send(2)
    source.send(3)
    source.close()

    pipelineThread.join()

    assert(target.receive() == Some("Value: 1"))
    assert(target.receive() == Some("Value: 2"))
    assert(target.receive() == Some("Value: 3"))
    assert(target.receive() == None)
  }

  def testRingBufferWraparound(): Unit = {
    val chan = new Chan[Int](3)

    for (cycle <- 0 until 10) {
      for (i <- 0 until 3) {
        assert(chan.send(cycle * 10 + i))
      }
      for (i <- 0 until 3) {
        assert(chan.receive() == Some(cycle * 10 + i))
      }
    }
  }

  def testConcurrentStress(): Unit = {
    val chan = new Chan[Int](100)
    val numThreads = 10
    val itemsPerThread = 1000
    val barrier = new CyclicBarrier(numThreads * 2 + 1)
    val errors = new AtomicInteger(0)
    val sent = new AtomicInteger(0)
    val received = new AtomicInteger(0)

    for (t <- 0 until numThreads) {
      Go {
        try {
          barrier.await()
          for (i <- 0 until itemsPerThread) {
            if (chan.send(t * itemsPerThread + i)) {
              sent.incrementAndGet()
            }
            if (i % 10 == 0) Thread.`yield`()
          }
        } catch {
          case _: Exception => errors.incrementAndGet()
        }
      }
    }

    for (_ <- 0 until numThreads) {
      Go {
        try {
          barrier.await()
          var continue = true
          while (continue) {
            chan.receive() match {
              case Some(_) => received.incrementAndGet()
              case None => continue = false
            }
          }
        } catch {
          case _: Exception => errors.incrementAndGet()
        }
      }
    }

    barrier.await()
    Thread.sleep(500)
    chan.close()
    Thread.sleep(500)

    assert(errors.get() == 0)
    assert(sent.get() == received.get())
    assert(sent.get() >= 5000, s"Expected at least 5000 items, got ${sent.get()}")
  }

  def testSelectStress(): Unit = {
    val channels = Array.fill(10)(new Chan[Int](10))
    val received = new AtomicInteger(0)
    val numSelectors = 5
    val latch = new CountDownLatch(numSelectors)

    for (_ <- 0 until numSelectors) {
      Go {
        var continue = true
        while (continue) {
          Select.receive(channels*) match {
            case Some(Select.Received(_, _)) =>
              received.incrementAndGet()
            case None =>
              continue = false
          }
        }
        latch.countDown()
      }
    }

    val random = ThreadLocalRandom.current()
    for (i <- 0 until 1000) {
      val chan = channels(random.nextInt(channels.length))
      chan.send(i)
      if (i % 100 == 0) Thread.`yield`()
    }

    channels.foreach(_.close())

    assert(latch.await(5, TimeUnit.SECONDS))
    assert(received.get() == 1000)
  }

  def performanceBenchmark(): Unit = {
    println("\n=== Channel Performance Benchmark ===")

    val iterations = 100000
    val chan = new Chan[Int](1000)

    val start1 = System.nanoTime()
    for (i <- 0 until iterations) {
      chan.send(i)
    }
    for (_ <- 0 until iterations) {
      chan.receive()
    }
    val elapsed1 = (System.nanoTime() - start1) / 1000000.0
    println(f"Single-threaded: $iterations ops in $elapsed1%.2f ms (${iterations / elapsed1 * 1000}%.0f ops/sec)")

    val chan2 = new Chan[Int](100)
    val latch2 = new CountDownLatch(2)

    val start2 = System.nanoTime()
    Go {
      for (i <- 0 until iterations) {
        chan2.send(i)
      }
      chan2.close()
      latch2.countDown()
    }

    Go {
      var continue = true
      while (continue) {
        chan2.receive() match {
          case Some(_) =>
          case None => continue = false
        }
      }
      latch2.countDown()
    }

    latch2.await()
    val elapsed2 = (System.nanoTime() - start2) / 1000000.0
    println(f"Producer-Consumer: $iterations ops in $elapsed2%.2f ms (${iterations / elapsed2 * 1000}%.0f ops/sec)")

    val selectChans = Array.fill(5)(new Chan[Int](100))
    val selectReceived = new AtomicInteger(0)
    val selectLatch = new CountDownLatch(1)

    Go {
      var continue = true
      while (continue) {
        Select.receive(selectChans: _*) match {
          case Some(_) => selectReceived.incrementAndGet()
          case None => continue = false
        }
      }
      selectLatch.countDown()
    }

    val start3 = System.nanoTime()
    for (i <- 0 until iterations) {
      selectChans(i % selectChans.length).send(i)
    }
    selectChans.foreach(_.close())
    selectLatch.await()
    val elapsed3 = (System.nanoTime() - start3) / 1000000.0
    println(f"Select (5 channels): $iterations ops in $elapsed3%.2f ms (${iterations / elapsed3 * 1000}%.0f ops/sec)")

    println("=====================================\n")
  }
  */
}