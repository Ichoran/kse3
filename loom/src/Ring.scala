// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr

package kse.loom


/** The single home of all ring-buffer wraparound arithmetic for SplitDeque.
  *
  * Contract, relied upon everywhere above this layer:
  *   - A ring is an `Array` whose length is a power of two; `mask` is `length - 1`.
  *   - Positions are *logical* `Int` indices, possibly negative or beyond the array
  *     length; they map to a slot only via `& mask` at the moment of access.
  *     (Java's `&` handles negative values correctly: `-1 & 7 == 7`.)
  *   - Spans are `(i0, n)` pairs covering logical positions `i0 until i0 + n`,
  *     always traversed by a count `k` running `0 until n` — no comparison is ever
  *     made between wrapped indices, so no off-by-one can hide in modular space.
  *   - Ops that vacate slots null them (or zero them, for `Int` rings), preserving
  *     the structure-wide invariant that every slot outside a live span is null/0.
  *
  * Preconditions are documented per-op and are the caller's burden; every op is
  * exhaustively enumerated against a naive modulo model in RingTest over its full
  * precondition domain.
  *
  * Deliberately bare: plain `while` loops, no abstraction in the bytecode.  The
  * `Int` overloads are mechanical copies of the `AnyRef` versions (zero in place
  * of null); they exist so size rings and element rings share one set of
  * verified position conventions.
  */
private[kse] object Ring {

  /** The slot for logical position `i`. */
  inline def at(a: Array[AnyRef], mask: Int, i: Int): AnyRef = a(i & mask)

  /** Set the slot for logical position `i`. */
  inline def set(a: Array[AnyRef], mask: Int, i: Int, x: AnyRef): Unit = a(i & mask) = x

  inline def at(a: Array[Int], mask: Int, i: Int): Int = a(i & mask)

  inline def set(a: Array[Int], mask: Int, i: Int, x: Int): Unit = a(i & mask) = x


  /** Copy ring span `(i0, n)` into `dst(d0 until d0+n)`, leaving the ring untouched.
    * Precondition: `n <= mask+1`; `(d0, n)` within `dst`.
    */
  def copyOut(a: Array[AnyRef], mask: Int, i0: Int, n: Int, dst: Array[AnyRef], d0: Int): Unit =
    var k = 0
    while k < n do
      dst(d0 + k) = a((i0 + k) & mask)
      k += 1

  /** Copy ring span `(i0, n)` into `dst(d0 until d0+n)`, nulling the ring span.
    * Precondition: `n <= mask+1`; `(d0, n)` within `dst`; `a ne dst`.
    */
  def moveOut(a: Array[AnyRef], mask: Int, i0: Int, n: Int, dst: Array[AnyRef], d0: Int): Unit =
    var k = 0
    while k < n do
      val j = (i0 + k) & mask
      dst(d0 + k) = a(j)
      a(j) = null
      k += 1

  /** Copy `src(s0 until s0+n)` into ring span `(i0, n)`, leaving `src` untouched.
    * Precondition: `n <= mask+1`; `(s0, n)` within `src`.
    */
  def copyIn(a: Array[AnyRef], mask: Int, i0: Int, src: Array[AnyRef], s0: Int, n: Int): Unit =
    var k = 0
    while k < n do
      a((i0 + k) & mask) = src(s0 + k)
      k += 1

  /** Copy `src(s0 until s0+n)` into ring span `(i0, n)`, nulling the source span.
    * Precondition: `n <= mask+1`; `(s0, n)` within `src`; `a ne src`.
    */
  def moveIn(a: Array[AnyRef], mask: Int, i0: Int, src: Array[AnyRef], s0: Int, n: Int): Unit =
    var k = 0
    while k < n do
      a((i0 + k) & mask) = src(s0 + k)
      src(s0 + k) = null
      k += 1

  /** Move ring span `(from, n)` to `(to, n)` within one ring, nulling the vacated
    * slots (those in the source span but not the destination span).
    * Precondition: `n + |to - from| <= mask+1` — otherwise the spans interlock
    * circularly and no single-pass copy order is correct.
    */
  def slide(a: Array[AnyRef], mask: Int, from: Int, to: Int, n: Int): Unit =
    if to < from then
      var k = 0
      while k < n do
        a((to + k) & mask) = a((from + k) & mask)
        k += 1
      var i = if from >= to + n then from else to + n
      while i < from + n do
        a(i & mask) = null
        i += 1
    else
      var k = n - 1
      while k >= 0 do
        a((to + k) & mask) = a((from + k) & mask)
        k -= 1
      var i = from
      val stop = if to <= from + n then to else from + n
      while i < stop do
        a(i & mask) = null
        i += 1

  /** Null ring span `(i0, n)`.  Precondition: `n <= mask+1`. */
  def clear(a: Array[AnyRef], mask: Int, i0: Int, n: Int): Unit =
    var k = 0
    while k < n do
      a((i0 + k) & mask) = null
      k += 1

  /** Copy span `(s0, n)` of ring `src` into span `(d0, n)` of ring `dst`,
    * nulling the source span.
    * Precondition: `src ne dst`; `n <= smask+1`; `n <= dmask+1`.
    */
  def transfer(src: Array[AnyRef], smask: Int, s0: Int, dst: Array[AnyRef], dmask: Int, d0: Int, n: Int): Unit =
    var k = 0
    while k < n do
      val j = (s0 + k) & smask
      dst((d0 + k) & dmask) = src(j)
      src(j) = null
      k += 1


  def copyOut(a: Array[Int], mask: Int, i0: Int, n: Int, dst: Array[Int], d0: Int): Unit =
    var k = 0
    while k < n do
      dst(d0 + k) = a((i0 + k) & mask)
      k += 1

  def moveOut(a: Array[Int], mask: Int, i0: Int, n: Int, dst: Array[Int], d0: Int): Unit =
    var k = 0
    while k < n do
      val j = (i0 + k) & mask
      dst(d0 + k) = a(j)
      a(j) = 0
      k += 1

  def copyIn(a: Array[Int], mask: Int, i0: Int, src: Array[Int], s0: Int, n: Int): Unit =
    var k = 0
    while k < n do
      a((i0 + k) & mask) = src(s0 + k)
      k += 1

  def moveIn(a: Array[Int], mask: Int, i0: Int, src: Array[Int], s0: Int, n: Int): Unit =
    var k = 0
    while k < n do
      a((i0 + k) & mask) = src(s0 + k)
      src(s0 + k) = 0
      k += 1

  def slide(a: Array[Int], mask: Int, from: Int, to: Int, n: Int): Unit =
    if to < from then
      var k = 0
      while k < n do
        a((to + k) & mask) = a((from + k) & mask)
        k += 1
      var i = if from >= to + n then from else to + n
      while i < from + n do
        a(i & mask) = 0
        i += 1
    else
      var k = n - 1
      while k >= 0 do
        a((to + k) & mask) = a((from + k) & mask)
        k -= 1
      var i = from
      val stop = if to <= from + n then to else from + n
      while i < stop do
        a(i & mask) = 0
        i += 1

  def clear(a: Array[Int], mask: Int, i0: Int, n: Int): Unit =
    var k = 0
    while k < n do
      a((i0 + k) & mask) = 0
      k += 1

  def transfer(src: Array[Int], smask: Int, s0: Int, dst: Array[Int], dmask: Int, d0: Int, n: Int): Unit =
    var k = 0
    while k < n do
      val j = (s0 + k) & smask
      dst((d0 + k) & dmask) = src(j)
      src(j) = 0
      k += 1
}
