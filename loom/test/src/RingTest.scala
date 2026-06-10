// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr

package kse.test.loom

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.loom.Ring


/** Exhaustively enumerates every Ring op over its full precondition domain at
  * small capacities, against a naive reference model.  The reference is written
  * in a deliberately different style (true mathematical modulo, snapshot then
  * apply) so that a shared misconception cannot pass both implementations.
  *
  * Logical positions are swept across two full periods on both sides of zero,
  * which covers every residue with positive and negative representatives; n is
  * swept over the whole precondition range.
  */
@RunWith(classOf[JUnit4])
class RingTest {
  val caps = Array(2, 4, 8)

  // True mathematical mod, unlike the & in Ring
  def m(cap: Int, i: Int): Int =
    val j = i % cap
    if j < 0 then j + cap else j

  def fresh(cap: Int): Array[AnyRef] = Array.tabulate[AnyRef](cap)(i => "v" + i)
  def freshI(cap: Int): Array[Int] = Array.tabulate(cap)(i => 100 + i)

  def flat(n: Int): Array[AnyRef] = Array.tabulate[AnyRef](n)(i => "f" + i)
  def flatI(n: Int): Array[Int] = Array.tabulate(n)(i => 900 + i)


  // === AnyRef ops ===

  @Test
  def ringCopyOut(): Unit =
    for cap <- caps; i0 <- -2*cap to 2*cap; n <- 0 to cap; d0 <- 0 to 2 do
      val ring = fresh(cap)
      val dst = flat(n + d0 + 2)
      val expRing = ring.clone
      val expDst = dst.clone
      var k = 0
      while k < n do
        expDst(d0 + k) = expRing(m(cap, i0 + k))
        k += 1
      Ring.copyOut(ring, cap - 1, i0, n, dst, d0)
      val msg = s"copyOut cap=$cap i0=$i0 n=$n d0=$d0"
      assertArrayEquals(msg, expRing, ring)
      assertArrayEquals(msg, expDst, dst)

  @Test
  def ringMoveOut(): Unit =
    for cap <- caps; i0 <- -2*cap to 2*cap; n <- 0 to cap; d0 <- 0 to 2 do
      val ring = fresh(cap)
      val dst = flat(n + d0 + 2)
      val init = ring.clone
      val expRing = ring.clone
      val expDst = dst.clone
      var k = 0
      while k < n do
        expDst(d0 + k) = init(m(cap, i0 + k))
        expRing(m(cap, i0 + k)) = null
        k += 1
      Ring.moveOut(ring, cap - 1, i0, n, dst, d0)
      val msg = s"moveOut cap=$cap i0=$i0 n=$n d0=$d0"
      assertArrayEquals(msg, expRing, ring)
      assertArrayEquals(msg, expDst, dst)

  @Test
  def ringCopyIn(): Unit =
    for cap <- caps; i0 <- -2*cap to 2*cap; n <- 0 to cap; s0 <- 0 to 2 do
      val ring = fresh(cap)
      val src = flat(n + s0 + 2)
      val expRing = ring.clone
      val expSrc = src.clone
      var k = 0
      while k < n do
        expRing(m(cap, i0 + k)) = expSrc(s0 + k)
        k += 1
      Ring.copyIn(ring, cap - 1, i0, src, s0, n)
      val msg = s"copyIn cap=$cap i0=$i0 n=$n s0=$s0"
      assertArrayEquals(msg, expRing, ring)
      assertArrayEquals(msg, expSrc, src)

  @Test
  def ringMoveIn(): Unit =
    for cap <- caps; i0 <- -2*cap to 2*cap; n <- 0 to cap; s0 <- 0 to 2 do
      val ring = fresh(cap)
      val src = flat(n + s0 + 2)
      val init = src.clone
      val expRing = ring.clone
      val expSrc = src.clone
      var k = 0
      while k < n do
        expRing(m(cap, i0 + k)) = init(s0 + k)
        expSrc(s0 + k) = null
        k += 1
      Ring.moveIn(ring, cap - 1, i0, src, s0, n)
      val msg = s"moveIn cap=$cap i0=$i0 n=$n s0=$s0"
      assertArrayEquals(msg, expRing, ring)
      assertArrayEquals(msg, expSrc, src)

  @Test
  def ringSlide(): Unit =
    for
      cap <- caps; from <- -cap to 2*cap; to <- -cap to 2*cap; n <- 0 to cap
      if n + math.abs(to - from) <= cap
    do
      val ring = fresh(cap)
      val init = ring.clone
      val exp = ring.clone
      var k = 0
      while k < n do
        exp(m(cap, to + k)) = init(m(cap, from + k))
        k += 1
      var p = from
      while p < from + n do
        if p < to || p >= to + n then exp(m(cap, p)) = null
        p += 1
      Ring.slide(ring, cap - 1, from, to, n)
      assertArrayEquals(s"slide cap=$cap from=$from to=$to n=$n", exp, ring)

  @Test
  def ringClear(): Unit =
    for cap <- caps; i0 <- -2*cap to 2*cap; n <- 0 to cap do
      val ring = fresh(cap)
      val exp = ring.clone
      var k = 0
      while k < n do
        exp(m(cap, i0 + k)) = null
        k += 1
      Ring.clear(ring, cap - 1, i0, n)
      assertArrayEquals(s"clear cap=$cap i0=$i0 n=$n", exp, ring)

  @Test
  def ringTransfer(): Unit =
    for
      scap <- caps; dcap <- caps
      s0 <- -scap to 2*scap; d0 <- -dcap to 2*dcap
      n <- 0 to math.min(scap, dcap)
    do
      val src = fresh(scap)
      val dst = flat(dcap)
      val init = src.clone
      val expSrc = src.clone
      val expDst = dst.clone
      var k = 0
      while k < n do
        expDst(m(dcap, d0 + k)) = init(m(scap, s0 + k))
        expSrc(m(scap, s0 + k)) = null
        k += 1
      Ring.transfer(src, scap - 1, s0, dst, dcap - 1, d0, n)
      val msg = s"transfer scap=$scap dcap=$dcap s0=$s0 d0=$d0 n=$n"
      assertArrayEquals(msg, expSrc, src)
      assertArrayEquals(msg, expDst, dst)


  // === Int ops (mechanical mirror; 0 in place of null) ===

  @Test
  def ringCopyOutInt(): Unit =
    for cap <- caps; i0 <- -2*cap to 2*cap; n <- 0 to cap; d0 <- 0 to 2 do
      val ring = freshI(cap)
      val dst = flatI(n + d0 + 2)
      val expRing = ring.clone
      val expDst = dst.clone
      var k = 0
      while k < n do
        expDst(d0 + k) = expRing(m(cap, i0 + k))
        k += 1
      Ring.copyOut(ring, cap - 1, i0, n, dst, d0)
      val msg = s"copyOutInt cap=$cap i0=$i0 n=$n d0=$d0"
      assertArrayEquals(msg, expRing, ring)
      assertArrayEquals(msg, expDst, dst)

  @Test
  def ringMoveOutInt(): Unit =
    for cap <- caps; i0 <- -2*cap to 2*cap; n <- 0 to cap; d0 <- 0 to 2 do
      val ring = freshI(cap)
      val dst = flatI(n + d0 + 2)
      val init = ring.clone
      val expRing = ring.clone
      val expDst = dst.clone
      var k = 0
      while k < n do
        expDst(d0 + k) = init(m(cap, i0 + k))
        expRing(m(cap, i0 + k)) = 0
        k += 1
      Ring.moveOut(ring, cap - 1, i0, n, dst, d0)
      val msg = s"moveOutInt cap=$cap i0=$i0 n=$n d0=$d0"
      assertArrayEquals(msg, expRing, ring)
      assertArrayEquals(msg, expDst, dst)

  @Test
  def ringCopyInInt(): Unit =
    for cap <- caps; i0 <- -2*cap to 2*cap; n <- 0 to cap; s0 <- 0 to 2 do
      val ring = freshI(cap)
      val src = flatI(n + s0 + 2)
      val expRing = ring.clone
      val expSrc = src.clone
      var k = 0
      while k < n do
        expRing(m(cap, i0 + k)) = expSrc(s0 + k)
        k += 1
      Ring.copyIn(ring, cap - 1, i0, src, s0, n)
      val msg = s"copyInInt cap=$cap i0=$i0 n=$n s0=$s0"
      assertArrayEquals(msg, expRing, ring)
      assertArrayEquals(msg, expSrc, src)

  @Test
  def ringMoveInInt(): Unit =
    for cap <- caps; i0 <- -2*cap to 2*cap; n <- 0 to cap; s0 <- 0 to 2 do
      val ring = freshI(cap)
      val src = flatI(n + s0 + 2)
      val init = src.clone
      val expRing = ring.clone
      val expSrc = src.clone
      var k = 0
      while k < n do
        expRing(m(cap, i0 + k)) = init(s0 + k)
        expSrc(s0 + k) = 0
        k += 1
      Ring.moveIn(ring, cap - 1, i0, src, s0, n)
      val msg = s"moveInInt cap=$cap i0=$i0 n=$n s0=$s0"
      assertArrayEquals(msg, expRing, ring)
      assertArrayEquals(msg, expSrc, src)

  @Test
  def ringSlideInt(): Unit =
    for
      cap <- caps; from <- -cap to 2*cap; to <- -cap to 2*cap; n <- 0 to cap
      if n + math.abs(to - from) <= cap
    do
      val ring = freshI(cap)
      val init = ring.clone
      val exp = ring.clone
      var k = 0
      while k < n do
        exp(m(cap, to + k)) = init(m(cap, from + k))
        k += 1
      var p = from
      while p < from + n do
        if p < to || p >= to + n then exp(m(cap, p)) = 0
        p += 1
      Ring.slide(ring, cap - 1, from, to, n)
      assertArrayEquals(s"slideInt cap=$cap from=$from to=$to n=$n", exp, ring)

  @Test
  def ringClearInt(): Unit =
    for cap <- caps; i0 <- -2*cap to 2*cap; n <- 0 to cap do
      val ring = freshI(cap)
      val exp = ring.clone
      var k = 0
      while k < n do
        exp(m(cap, i0 + k)) = 0
        k += 1
      Ring.clear(ring, cap - 1, i0, n)
      assertArrayEquals(s"clearInt cap=$cap i0=$i0 n=$n", exp, ring)

  @Test
  def ringTransferInt(): Unit =
    for
      scap <- caps; dcap <- caps
      s0 <- -scap to 2*scap; d0 <- -dcap to 2*dcap
      n <- 0 to math.min(scap, dcap)
    do
      val src = freshI(scap)
      val dst = flatI(dcap)
      val init = src.clone
      val expSrc = src.clone
      val expDst = dst.clone
      var k = 0
      while k < n do
        expDst(m(dcap, d0 + k)) = init(m(scap, s0 + k))
        expSrc(m(scap, s0 + k)) = 0
        k += 1
      Ring.transfer(src, scap - 1, s0, dst, dcap - 1, d0, n)
      val msg = s"transferInt scap=$scap dcap=$dcap s0=$s0 d0=$d0 n=$n"
      assertArrayEquals(msg, expSrc, src)
      assertArrayEquals(msg, expDst, dst)

  // === at/set masking convention ===

  @Test
  def ringAtSet(): Unit =
    for cap <- caps; i <- -3*cap to 3*cap do
      val ring = fresh(cap)
      assertSame(s"at cap=$cap i=$i", ring(m(cap, i)), Ring.at(ring, cap - 1, i))
      Ring.set(ring, cap - 1, i, "X")
      assertSame(s"set cap=$cap i=$i", "X", ring(m(cap, i)))
      val ringI = freshI(cap)
      assertEquals(s"atInt cap=$cap i=$i", ringI(m(cap, i)), Ring.at(ringI, cap - 1, i))
      Ring.set(ringI, cap - 1, i, -7)
      assertEquals(s"setInt cap=$cap i=$i", -7, ringI(m(cap, i)))
  }
