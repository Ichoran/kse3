// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr

package kse.test.loom

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.loom.SplitDequeImpl


/** Differential fuzzing of the SplitDeque engine against java.util.ArrayDeque,
  * at deliberately tiny geometries so that deep levels and every restructuring
  * path fire constantly.  Every operation is followed by a full invariant
  * validation; contents are compared in full at intervals and at the end.
  * Failures report (geometry, seed, op index) for replay.
  */
@RunWith(classOf[JUnit4])
class SplitDequeImplTest {

  def compareAll(msg: String, d: SplitDequeImpl, ref: java.util.ArrayDeque[AnyRef]): Unit =
    val buf = new scala.collection.mutable.ArrayBuffer[AnyRef](ref.size)
    d.foreachForward(buf += _)
    assertEquals(s"$msg size", ref.size, buf.length)
    val it = ref.iterator
    var j = 0
    while it.hasNext do
      assertSame(s"$msg elt $j", it.next, buf(j))
      j += 1

  def check(msg: String, d: SplitDequeImpl, ref: java.util.ArrayDeque[AnyRef]): Unit =
    assertEquals(s"$msg count", ref.size, d.count)
    val err = d.validate()
    if err ne null then fail(s"$msg invalid: $err")

  // pushBias of 10 means equal push/pop chance; higher grows, lower shrinks.
  def fuzz(lgCap: Int, bs: Int, seed: Long, ops: Int, pushBias: Int = 12): Unit =
    val rng = new java.util.Random(seed)
    val d = new SplitDequeImpl(lgCap, bs, 0)
    val ref = new java.util.ArrayDeque[AnyRef]
    val ctx = s"lgCap=$lgCap bs=$bs seed=$seed"
    var next = 0
    var i = 0
    while i < ops do
      val r = rng.nextInt(20)
      if r < pushBias then
        val v = Integer.valueOf(next)
        next += 1
        if (r & 1) == 0 then { d.pushLeft(v); ref.addFirst(v) }
        else { d.pushRight(v); ref.addLast(v) }
      else if ref.isEmpty then assertEquals(s"$ctx op=$i empty", 0, d.count)
      else if (r & 1) == 0 then assertSame(s"$ctx op=$i popL", ref.pollFirst(), d.popLeft())
      else assertSame(s"$ctx op=$i popR", ref.pollLast(), d.popRight())
      check(s"$ctx op=$i", d, ref)
      if i % 101 == 0 then compareAll(s"$ctx op=$i", d, ref)
      i += 1
    compareAll(s"$ctx done", d, ref)
    // Drain alternating ends, validating the whole way down
    var flip = false
    while !ref.isEmpty do
      if flip then assertSame(s"$ctx drainR n=${ref.size}", ref.pollLast(), d.popRight())
      else assertSame(s"$ctx drainL n=${ref.size}", ref.pollFirst(), d.popLeft())
      flip = !flip
      check(s"$ctx drain n=${ref.size}", d, ref)
    assertEquals(s"$ctx drained", 0, d.count)

  @Test
  def fuzzTiny(): Unit =
    var seed = 0
    while seed < 40 do
      fuzz(2, 2, seed, 1500)
      seed += 1

  @Test
  def fuzzSmall(): Unit =
    var seed = 0
    while seed < 25 do
      fuzz(3, 2, seed, 2000)
      fuzz(3, 3, seed, 2000)
      fuzz(3, 4, seed, 2000)
      seed += 1

  @Test
  def fuzzMid(): Unit =
    var seed = 0
    while seed < 15 do
      fuzz(4, 5, seed, 3000)
      fuzz(4, 8, seed, 3000)
      seed += 1

  @Test
  def fuzzShrinkBiased(): Unit =
    // Pop-biased after a grow phase: exercises refill paths heavily
    var seed = 100
    while seed < 115 do
      fuzz(2, 2, seed, 1200, pushBias = 14)
      fuzz(3, 3, seed, 1200, pushBias = 8)
      seed += 1

  // === Directed patterns: pure regimes that fuzzing visits only briefly ===

  def pattern(lgCap: Int, bs: Int, n: Int)(step: (SplitDequeImpl, java.util.ArrayDeque[AnyRef], Int) => Unit): Unit =
    val d = new SplitDequeImpl(lgCap, bs, 0)
    val ref = new java.util.ArrayDeque[AnyRef]
    var i = 0
    while i < n do
      step(d, ref, i)
      check(s"lgCap=$lgCap bs=$bs step=$i", d, ref)
      i += 1
    compareAll(s"lgCap=$lgCap bs=$bs final", d, ref)
    while !ref.isEmpty do
      assertSame(s"lgCap=$lgCap bs=$bs drain n=${ref.size}", ref.pollFirst(), d.popLeft())
      check(s"lgCap=$lgCap bs=$bs drain n=${ref.size}", d, ref)

  @Test
  def fifoSteady(): Unit =
    // Grow to a plateau, then steady-state flow-through in both directions
    pattern(2, 2, 3000){ (d, ref, i) =>
      val v = Integer.valueOf(i)
      d.pushRight(v); ref.addLast(v)
      if i >= 300 then assertSame(s"fifo $i", ref.pollFirst(), d.popLeft())
    }
    pattern(3, 4, 3000){ (d, ref, i) =>
      val v = Integer.valueOf(i)
      d.pushLeft(v); ref.addFirst(v)
      if i >= 300 then assertSame(s"ofif $i", ref.pollLast(), d.popRight())
    }

  @Test
  def lifoSteady(): Unit =
    // Sawtooth stack use on both ends, with a deep base so spill/refill cycles at the boundary
    pattern(2, 2, 4000){ (d, ref, i) =>
      val phase = (i / 50) % 2
      if phase == 0 || ref.isEmpty then
        val v = Integer.valueOf(i)
        d.pushLeft(v); ref.addFirst(v)
      else assertSame(s"lifoL $i", ref.pollFirst(), d.popLeft())
    }
    pattern(3, 3, 4000){ (d, ref, i) =>
      val phase = (i / 50) % 2
      if phase == 0 || ref.isEmpty then
        val v = Integer.valueOf(i)
        d.pushRight(v); ref.addLast(v)
      else assertSame(s"lifoR $i", ref.pollLast(), d.popRight())
    }

  // === Split and splice ===

  // Random push/pop/split/splice over a pool of live structures, every one
  // mirrored against an ArrayDeque and fully validated after every operation.
  def fuzzPool(lgCap: Int, bs: Int, seed: Long, ops: Int): Unit =
    val rng = new java.util.Random(seed)
    val pool = scala.collection.mutable.ArrayBuffer((new SplitDequeImpl(lgCap, bs, 0), new java.util.ArrayDeque[AnyRef]))
    val ctx = s"pool lgCap=$lgCap bs=$bs seed=$seed"
    var next = 0
    var i = 0
    while i < ops do
      val r = rng.nextInt(20)
      val idx = rng.nextInt(pool.size)
      val (d, ref) = pool(idx)
      if r < 8 then
        val v = Integer.valueOf(next)
        next += 1
        if (r & 1) == 0 then { d.pushLeft(v); ref.addFirst(v) }
        else { d.pushRight(v); ref.addLast(v) }
      else if r < 12 then
        if ref.isEmpty then assertEquals(s"$ctx op=$i empty", 0, d.count)
        else if (r & 1) == 0 then assertSame(s"$ctx op=$i popL", ref.pollFirst(), d.popLeft())
        else assertSame(s"$ctx op=$i popR", ref.pollLast(), d.popRight())
      else if r < 16 then
        if pool.size < 5 then
          val k = rng.nextInt(ref.size + 2)
          val take = math.min(k, ref.size)
          val ref2 = new java.util.ArrayDeque[AnyRef]
          val d2 =
            if rng.nextBoolean() then
              var t = 0
              while t < take do { ref2.addLast(ref.pollFirst()); t += 1 }
              d.splitLeft(k)
            else
              var t = 0
              while t < take do { ref2.addFirst(ref.pollLast()); t += 1 }
              d.splitRight(k)
          assertEquals(s"$ctx op=$i split k=$k", take, d2.count)
          pool += ((d2, ref2))
      else if pool.size >= 2 then
        var jdx = rng.nextInt(pool.size)
        if jdx == idx then jdx = (jdx + 1) % pool.size
        val (dj, refj) = pool(jdx)
        if rng.nextBoolean() then
          d.spliceRight(dj)
          while !refj.isEmpty do ref.addLast(refj.pollFirst())
        else
          d.spliceLeft(dj)
          while !refj.isEmpty do ref.addFirst(refj.pollLast())
        assertEquals(s"$ctx op=$i donor emptied", 0, dj.count)
        val errj = dj.validate()
        if errj ne null then fail(s"$ctx op=$i donor invalid: $errj")
        pool.remove(jdx)
      var q = 0
      while q < pool.size do
        check(s"$ctx op=$i pool($q)", pool(q)._1, pool(q)._2)
        q += 1
      if i % 79 == 0 then
        q = 0
        while q < pool.size do
          compareAll(s"$ctx op=$i pool($q)", pool(q)._1, pool(q)._2)
          q += 1
      i += 1
    // Merge everything back into one structure and drain it
    while pool.size > 1 do
      val (dj, refj) = pool.remove(pool.size - 1)
      pool(0)._1.spliceRight(dj)
      while !refj.isEmpty do pool(0)._2.addLast(refj.pollFirst())
      check(s"$ctx merge", pool(0)._1, pool(0)._2)
    val (d, ref) = pool(0)
    compareAll(s"$ctx merged", d, ref)
    while !ref.isEmpty do
      assertSame(s"$ctx final drain n=${ref.size}", ref.pollFirst(), d.popLeft())
      if ref.size % 41 == 0 then check(s"$ctx final drain n=${ref.size}", d, ref)
    assertEquals(s"$ctx done", 0, d.count)

  @Test
  def fuzzSplitSplice(): Unit =
    var seed = 0
    while seed < 20 do
      fuzzPool(2, 2, seed, 1200)
      fuzzPool(3, 3, seed, 1200)
      fuzzPool(4, 5, seed, 1500)
      seed += 1

  @Test
  def splitRoundtripEveryPoint(): Unit =
    // Split at every possible point and splice back; contents must be intact
    val n = 347
    var cfg = 0
    val lgs = Array(2, 3, 4)
    val bss = Array(2, 3, 6)
    while cfg < 3 do
      val lg = lgs(cfg)
      val bs = bss(cfg)
      val d = new SplitDequeImpl(lg, bs, 0)
      val ref = new java.util.ArrayDeque[AnyRef]
      var i = 0
      while i < n do
        val v = Integer.valueOf(i)
        d.pushRight(v); ref.addLast(v)
        i += 1
      var k = 0
      while k <= n do
        val msg = s"roundtrip lg=$lg bs=$bs k=$k"
        val d2 = d.splitLeft(k)
        assertEquals(s"$msg head", k, d2.count)
        assertEquals(s"$msg tail", n - k, d.count)
        var err = d2.validate()
        if err ne null then fail(s"$msg head invalid: $err")
        err = d.validate()
        if err ne null then fail(s"$msg tail invalid: $err")
        d.spliceLeft(d2)
        err = d.validate()
        if err ne null then fail(s"$msg rejoined invalid: $err")
        if k % 13 == 0 then compareAll(msg, d, ref)
        val d3 = d.splitRight(k)
        assertEquals(s"$msg rhead", n - k, d.count)
        assertEquals(s"$msg rtail", k, d3.count)
        err = d3.validate()
        if err ne null then fail(s"$msg rtail invalid: $err")
        d.spliceRight(d3)
        err = d.validate()
        if err ne null then fail(s"$msg rrejoined invalid: $err")
        if k % 17 == 0 then compareAll(s"$msg r", d, ref)
        k += 1
      compareAll(s"roundtrip lg=$lg bs=$bs final", d, ref)
      cfg += 1

  @Test
  def deepSpliceAndSplit(): Unit =
    // Two deep structures merged, then carved up at scale
    val d1 = new SplitDequeImpl(2, 2, 0)
    val d2 = new SplitDequeImpl(2, 2, 0)
    val ref = new java.util.ArrayDeque[AnyRef]
    var i = 0
    while i < 5000 do
      val v = Integer.valueOf(i)
      d1.pushRight(v); ref.addLast(v)
      i += 1
    while i < 10000 do
      val v = Integer.valueOf(i)
      d2.pushRight(v); ref.addLast(v)
      i += 1
    d1.spliceRight(d2)
    check("deepSplice merged", d1, ref)
    assertEquals(0, d2.count)
    // Carve into chunks of varying sizes from alternating ends; verify totality
    val front = scala.collection.mutable.ArrayBuffer.empty[SplitDequeImpl]
    val back = scala.collection.mutable.ArrayBuffer.empty[SplitDequeImpl]
    var w = 1
    var fromLeft = true
    while d1.count > 0 do
      val part = if fromLeft then d1.splitLeft(w) else d1.splitRight(w)
      var err = part.validate()
      if err ne null then fail(s"deepSplice part w=$w invalid: $err")
      err = d1.validate()
      if err ne null then fail(s"deepSplice rest w=$w invalid: $err")
      if fromLeft then front += part else back.insert(0, part)
      fromLeft = !fromLeft
      w = (w * 2) min 1500
    // Reassemble in order and verify against reference
    val all = new SplitDequeImpl(2, 2, 0)
    i = 0
    while i < front.length do
      all.spliceRight(front(i))
      i += 1
    i = 0
    while i < back.length do
      all.spliceRight(back(i))
      i += 1
    check("deepSplice reassembled", all, ref)
    compareAll("deepSplice reassembled", all, ref)

  @Test
  def poolingReusesBlocks(): Unit =
    // In steady-state flow-through every level packs and unpacks at the same
    // rate, so the two-slot stash should supply every pack: after warmup,
    // fresh allocations must stop completely.
    var cfg = 0
    val lgs = Array(2, 3, 4)
    val bss = Array(2, 3, 6)
    while cfg < 3 do
      val lg = lgs(cfg)
      val bs = bss(cfg)
      val d = new SplitDequeImpl(lg, bs, 0)
      var i = 0
      while i < 2000 do
        d.pushRight(Integer.valueOf(i))
        i += 1
      i = 0
      while i < 1000 do
        d.pushRight(Integer.valueOf(i))
        assertNotNull(d.popLeft())
        i += 1
      val a0 = d.totalAllocs
      i = 0
      while i < 5000 do
        d.pushRight(Integer.valueOf(i))
        assertNotNull(d.popLeft())
        i += 1
      assertEquals(s"lg=$lg bs=$bs fifo steady-state should not allocate", a0, d.totalAllocs)
      assertNull(d.validate())
      // Sawtooth on one end: spill/refill oscillation at the flank boundary.
      // Warm up in this regime first — switching regimes may transiently
      // allocate while the stash refills.
      i = 0
      while i < 1000 do
        if (i / (2 * bs)) % 2 == 0 then d.pushRight(Integer.valueOf(i))
        else assertNotNull(d.popRight())
        i += 1
      val a1 = d.totalAllocs
      while i < 6000 do
        if (i / (2 * bs)) % 2 == 0 then d.pushRight(Integer.valueOf(i))
        else assertNotNull(d.popRight())
        i += 1
      assertEquals(s"lg=$lg bs=$bs sawtooth steady-state should not allocate", a1, d.totalAllocs)
      assertNull(d.validate())
      cfg += 1

  @Test
  def deepThenDrain(): Unit =
    // Build a deep structure (several levels), then drain entirely from one end,
    // then rebuild and drain from the other
    val d = new SplitDequeImpl(2, 2, 0)
    val ref = new java.util.ArrayDeque[AnyRef]
    var i = 0
    while i < 5000 do
      val v = Integer.valueOf(i)
      d.pushRight(v); ref.addLast(v)
      if i % 257 == 0 then check(s"deep build $i", d, ref)
      i += 1
    check("deep built", d, ref)
    while !ref.isEmpty do
      assertSame(s"deep drainL n=${ref.size}", ref.pollFirst(), d.popLeft())
      if ref.size % 97 == 0 then check(s"deep drainL n=${ref.size}", d, ref)
    assertEquals(0, d.count)
    i = 0
    while i < 5000 do
      val v = Integer.valueOf(i)
      d.pushLeft(v); ref.addFirst(v)
      i += 1
    check("deep rebuilt", d, ref)
    while !ref.isEmpty do
      assertSame(s"deep drainR n=${ref.size}", ref.pollLast(), d.popRight())
      if ref.size % 97 == 0 then check(s"deep drainR n=${ref.size}", d, ref)
    assertEquals(0, d.count)
}
