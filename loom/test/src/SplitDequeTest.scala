// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr

package kse.test.loom

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import kse.basics._
import kse.flow._
import kse.loom.SplitDeque


/** Tests of the concurrent SplitDeque shell and its Batch companion: differential
  * fuzzing of the full API surface single-threaded (the engine underneath has its
  * own exhaustive tests), then multi-threaded delivery, ordering, and invariant
  * checks under real contention.
  */
@RunWith(classOf[JUnit4])
class SplitDequeTest {

  def got(r: Integer Or Unit): Integer = r.fold(x => x)(_ => throw new AssertionError("unexpectedly empty"))

  def checkD(msg: String, d: SplitDeque[Integer], ref: java.util.ArrayDeque[AnyRef]): Unit =
    assertEquals(s"$msg length", ref.size, d.length)
    val err = d.validate()
    if err ne null then fail(s"$msg invalid: $err")

  def checkB(msg: String, b: SplitDeque.Batch[Integer], ref: java.util.ArrayDeque[AnyRef]): Unit =
    assertEquals(s"$msg length", ref.size, b.length)
    val err = b.validate()
    if err ne null then fail(s"$msg invalid: $err")

  def compareB(msg: String, b: SplitDeque.Batch[Integer], ref: java.util.ArrayDeque[AnyRef]): Unit =
    val buf = new scala.collection.mutable.ArrayBuffer[AnyRef](ref.size)
    b.foreach(buf += _)
    assertEquals(s"$msg size", ref.size, buf.length)
    val it = ref.iterator
    var j = 0
    while it.hasNext do
      assertSame(s"$msg elt $j", it.next, buf(j))
      j += 1

  // === Single-threaded differential fuzz of the whole shell + Batch surface ===

  def shellFuzz(lgCap: Int, bs: Int, seed: Long, ops: Int): Unit =
    val rng = new java.util.Random(seed)
    val d = new SplitDeque[Integer](lgCap, bs)
    val dref = new java.util.ArrayDeque[AnyRef]
    val batches = scala.collection.mutable.ArrayBuffer.empty[(SplitDeque.Batch[Integer], java.util.ArrayDeque[AnyRef])]
    val ctx = s"shell lgCap=$lgCap bs=$bs seed=$seed"
    var next = 0
    var i = 0
    while i < ops do
      val r = rng.nextInt(24)
      if r < 8 then
        val v = Integer.valueOf(next)
        next += 1
        if (r & 1) == 0 then { d.pushLeft(v); dref.addFirst(v) }
        else { d.pushRight(v); dref.addLast(v) }
      else if r < 12 then
        if dref.isEmpty then assertTrue(s"$ctx op=$i empty", d.popLeft().isAlt)
        else if (r & 1) == 0 then assertSame(s"$ctx op=$i popL", dref.pollFirst(), got(d.popLeft()))
        else assertSame(s"$ctx op=$i popR", dref.pollLast(), got(d.popRight()))
      else if r < 15 then
        // Split a batch off the deque
        if batches.size < 4 then
          val k = rng.nextInt(dref.size + 3)
          val take = math.min(k, dref.size)
          val bref = new java.util.ArrayDeque[AnyRef]
          val b =
            if rng.nextBoolean() then
              var t = 0
              while t < take do { bref.addLast(dref.pollFirst()); t += 1 }
              d.splitLeft(k)
            else
              var t = 0
              while t < take do { bref.addFirst(dref.pollLast()); t += 1 }
              d.splitRight(k)
          assertEquals(s"$ctx op=$i split k=$k", take, b.length)
          batches += ((b, bref))
      else if r < 18 then
        // Splice a batch into the deque; the batch survives, empty
        if batches.nonEmpty then
          val (b, bref) = batches(rng.nextInt(batches.size))
          if (r & 1) == 0 then
            d.spliceLeft(b)
            while !bref.isEmpty do dref.addFirst(bref.pollLast())
          else
            d.spliceRight(b)
            while !bref.isEmpty do dref.addLast(bref.pollFirst())
          assertEquals(s"$ctx op=$i donor emptied", 0, b.length)
      else if r < 21 then
        // Work within a batch: push/pop singles
        if batches.nonEmpty then
          val (b, bref) = batches(rng.nextInt(batches.size))
          if rng.nextBoolean() then
            val v = Integer.valueOf(next)
            next += 1
            if (r & 1) == 0 then { b.pushLeft(v); bref.addFirst(v) }
            else { b.pushRight(v); bref.addLast(v) }
          else if bref.isEmpty then assertTrue(s"$ctx op=$i batch empty", b.popLeft().isAlt)
          else if (r & 1) == 0 then assertSame(s"$ctx op=$i batch popL", bref.pollFirst(), got(b.popLeft()))
          else assertSame(s"$ctx op=$i batch popR", bref.pollLast(), got(b.popRight()))
      else
        // Batch-to-batch split or splice
        if batches.size >= 2 then
          val (b1, ref1) = batches(0)
          val (b2, ref2) = batches(1)
          if rng.nextBoolean() then
            b1.spliceRight(b2)
            while !ref2.isEmpty do ref1.addLast(ref2.pollFirst())
            assertEquals(s"$ctx op=$i b2b donor emptied", 0, b2.length)
          else
            val k = rng.nextInt(ref1.size + 2)
            val take = math.min(k, ref1.size)
            val b3 = b1.splitLeft(k)
            val ref3 = new java.util.ArrayDeque[AnyRef]
            var t = 0
            while t < take do { ref3.addLast(ref1.pollFirst()); t += 1 }
            if batches.size < 6 then batches += ((b3, ref3))
            else
              b1.spliceLeft(b3)
              while !ref3.isEmpty do ref1.addFirst(ref3.pollLast())
      checkD(s"$ctx op=$i", d, dref)
      var q = 0
      while q < batches.size do
        checkB(s"$ctx op=$i batch($q)", batches(q)._1, batches(q)._2)
        q += 1
      if i % 97 == 0 then
        q = 0
        while q < batches.size do
          compareB(s"$ctx op=$i batch($q)", batches(q)._1, batches(q)._2)
          q += 1
      i += 1
    // Splice everything back and drain through the shell
    for (b, bref) <- batches do
      d.spliceRight(b)
      while !bref.isEmpty do dref.addLast(bref.pollFirst())
    checkD(s"$ctx merged", d, dref)
    while !dref.isEmpty do
      assertSame(s"$ctx drain n=${dref.size}", dref.pollFirst(), got(d.popLeft()))
    assertTrue(s"$ctx drained", d.popLeft().isAlt)
    assertEquals(s"$ctx drained len", 0, d.length)

  @Test
  def shellDifferentialFuzz(): Unit =
    var seed = 0
    while seed < 15 do
      shellFuzz(2, 2, seed, 800)
      shellFuzz(3, 3, seed, 1000)
      seed += 1
    shellFuzz(SplitDeque.defaultLgCap, SplitDeque.defaultBlockSize, 42, 4000)

  @Test
  def nullsAndEmptiesBehave(): Unit =
    val d = new SplitDeque[String]()
    assertTrue(d.popLeft().isAlt)
    assertTrue(d.popRight().isAlt)
    d.pushRight(null)
    d.pushRight("x")
    assertEquals(2, d.length)
    assertNull(d.popLeft().fold(x => x)(_ => "NONEMPTY"))
    assertEquals("x", d.popLeft().fold(x => x)(_ => "NONEMPTY"))
    assertEquals(0.0, d.contention(), 0.0)
    val b = SplitDeque.Batch.empty[String]()
    b.pushLeft(null)
    assertNull(b.popRight().fold(x => x)(_ => "NONEMPTY"))
    assertTrue(b.popLeft().isAlt)

  @Test
  def geometryMismatchRejected(): Unit =
    val d = new SplitDeque[Integer](3, 3)
    val b = SplitDeque.Batch.empty[Integer](4, 4)
    b.pushLeft(Integer.valueOf(1))
    var caught = false
    try d.spliceRight(b)
    catch case _: IllegalArgumentException => caught = true
    assertTrue("geometry mismatch must be rejected", caught)
    assertEquals("rejected donor must be untouched", 1, b.length)
    val err = d.validate()
    if err ne null then fail(s"deque invalid after rejected splice: $err")

  // === Concurrent tests ===

  def spawn(errs: java.util.concurrent.ConcurrentLinkedQueue[Throwable])(body: => Unit): Thread =
    val t = new Thread(() => try body catch case e: Throwable => errs.add(e) __ Unit)
    t.start()
    t

  def joinAll(errs: java.util.concurrent.ConcurrentLinkedQueue[Throwable], threads: Seq[Thread]): Unit =
    val deadline = System.nanoTime + 60_000_000_000L
    for t <- threads do
      val left = deadline - System.nanoTime
      t.join(math.max(1L, left / 1_000_000))
      if t.isAlive then
        errs.add(new AssertionError(s"thread ${t.getName} did not finish")) __ Unit
        t.interrupt()
    if !errs.isEmpty then
      val e = new AssertionError(s"${errs.size} thread failure(s); first shown", errs.peek)
      throw e

  @Test
  def spscPreservesOrder(): Unit =
    val n = 100000
    val d = new SplitDeque[Integer]()
    val errs = new java.util.concurrent.ConcurrentLinkedQueue[Throwable]
    val producer = spawn(errs):
      var i = 0
      while i < n do
        d.pushRight(Integer.valueOf(i))
        i += 1
    val consumer = spawn(errs):
      var expect = 0
      val deadline = System.nanoTime + 30_000_000_000L
      while expect < n do
        d.popLeft().fold{ x =>
          assertEquals("FIFO order violated", expect, x.intValue)
          expect += 1
        }{ _ =>
          if System.nanoTime > deadline then throw new AssertionError(s"timed out at $expect of $n")
          Thread.onSpinWait()
        }
    joinAll(errs, List(producer, consumer))
    assertEquals(0, d.length)

  @Test
  def mpmcDeliversExactly(): Unit =
    val nProd = 4
    val nCons = 4
    val perProd = 25000
    val d = new SplitDeque[Integer]()
    val errs = new java.util.concurrent.ConcurrentLinkedQueue[Throwable]
    val produced = new AtomicInteger(0)
    val consumed = new AtomicInteger(0)
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[Integer]
    val producers = List.tabulate(nProd){ p =>
      spawn(errs):
        var i = 0
        while i < perProd do
          val v = Integer.valueOf(p * perProd + i)
          if (i & 3) == 0 then d.pushLeft(v) else d.pushRight(v)
          produced.incrementAndGet() __ Unit
          i += 1
    }
    val consumers = List.tabulate(nCons){ c =>
      spawn(errs):
        val local = new scala.collection.mutable.ArrayBuffer[Integer]
        val deadline = System.nanoTime + 30_000_000_000L
        var done = false
        while !done do
          ((if (c & 1) == 0 then d.popLeft() else d.popRight())).fold{ x =>
            local += x
            consumed.incrementAndGet() __ Unit
          }{ _ =>
            if consumed.get >= nProd * perProd then done = true
            else if System.nanoTime > deadline then throw new AssertionError(s"timed out with ${consumed.get}")
            else Thread.onSpinWait()
          }
        local.foreach(x => seen.add(x) __ Unit)
    }
    // Validate (atomically) while everything is in flight
    var probes = 0
    while probes < 50 && consumed.get < nProd * perProd do
      val err = d.validate()
      if err ne null then fail(s"invalid in flight: $err")
      probes += 1
      Thread.sleep(1)
    joinAll(errs, producers ++ consumers)
    assertEquals("everything consumed", nProd * perProd, seen.size)
    val all = new Array[Boolean](nProd * perProd)
    seen.forEach{ x =>
      assertFalse(s"duplicate delivery of $x", all(x.intValue))
      all(x.intValue) = true
    }
    assertEquals(0, d.length)
    val err = d.validate()
    if err ne null then fail(s"invalid after drain: $err")

  @Test
  def rebatchingPipelineDeliversExactly(): Unit =
    val nProd = 3
    val perProd = 20000
    val total = nProd * perProd
    val a = new SplitDeque[Integer]()
    val b = new SplitDeque[Integer]()
    val errs = new java.util.concurrent.ConcurrentLinkedQueue[Throwable]
    val producersDone = new AtomicBoolean(false)
    val rebatchersDone = new AtomicBoolean(false)
    val consumed = new AtomicInteger(0)
    val seen = new java.util.concurrent.ConcurrentLinkedQueue[Integer]
    val producers = List.tabulate(nProd){ p =>
      spawn(errs):
        var i = 0
        while i < perProd do
          a.pushRight(Integer.valueOf(p * perProd + i))
          i += 1
    }
    val rebatchers = List.tabulate(2){ w =>
      spawn(errs):
        val rng = new java.util.Random(w)
        val deadline = System.nanoTime + 30_000_000_000L
        var idle = false
        while !idle do
          val batch = a.splitLeft(1 + rng.nextInt(64))
          if batch.isEmpty then
            if producersDone.get && a.length == 0 then idle = true
            else if System.nanoTime > deadline then throw new AssertionError("rebatcher timed out")
            else Thread.onSpinWait()
          else if rng.nextInt(4) == 0 then
            // Sometimes carve the batch and splice the pieces separately
            val rest = batch.splitRight(batch.length / 2)
            b.spliceRight(batch)
            b.spliceRight(rest)
          else b.spliceRight(batch)
    }
    val consumers = List.tabulate(3){ _ =>
      spawn(errs):
        val local = new scala.collection.mutable.ArrayBuffer[Integer]
        val deadline = System.nanoTime + 30_000_000_000L
        var done = false
        while !done do
          b.popLeft().fold{ x =>
            local += x
            consumed.incrementAndGet() __ Unit
          }{ _ =>
            if consumed.get >= total && rebatchersDone.get then done = true
            else if System.nanoTime > deadline then throw new AssertionError(s"consumer timed out with ${consumed.get}")
            else Thread.onSpinWait()
          }
        local.foreach(x => seen.add(x) __ Unit)
    }
    joinAll(errs, producers)
    producersDone.set(true)
    joinAll(errs, rebatchers)
    rebatchersDone.set(true)
    joinAll(errs, consumers)
    assertEquals("everything delivered", total, seen.size)
    val all = new Array[Boolean](total)
    seen.forEach{ x =>
      assertFalse(s"duplicate delivery of $x", all(x.intValue))
      all(x.intValue) = true
    }
    assertEquals(0, a.length + b.length)
    var err = a.validate()
    if err eq null then err = b.validate()
    if err ne null then fail(s"invalid after pipeline: $err")
}
