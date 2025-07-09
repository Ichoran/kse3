// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2025 Rex Kerr and UCSF (Kato Lab)

package kse.flow


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.util.concurrent.locks.LockSupport

import scala.reflect.ClassTag

import kse.basics._
import kse.basics.intervals._


/** Implements a fast concurrent deque that allows O(log n) retrieval of chunks of up to size n.
  * This makes it suitable for scenarios with many fast producers and a chunk-based consumer, though
  * it also has good single-element queue and stack performance.
  * 
  * Under very heavy contention, overhead will increase markedly due to spin-waiting for access.  One
  * can use the `contention()` method to get a numeric contention score.
  * 
  * Because of the difficulty of retaining O(log n) performance with Scala collections, this is _not_
  * a Scala collection.  It does not support concurrent access of its elements, only adding, removing,
  * and querying size (which is guaranteed to be correct when accessed, but may change before one can
  * do anything based upon it if the deque is being concurrently modified).
  * 
  * Use `chunk()` to drain the entire deque to a `ConcurrentSplitDeque.Chunk`, or a portion of it with
  * `chunk(n)` (where `n` is the maximum number of elements to drain).  These elements can then be
  * accessed.
  * 
  * Because `ConcurrentSplitDeque` maintains empty buffers and `ConcurrentSplitDeque.Chunk` does not, if one
  * has filled a deque and is finished with it, favor calling `chunk()` and holding the chunk over
  * persisting the deque.
  */
final class ConcurrentSplitDeque[A]() {
  import ConcurrentSplitDeque.{lenHandle, vznHandle}

  // The data structure is loosely inspired by a finger tree, where we have a double-edged element-level ring buffer
  // plus deeper tree layers where each layer contains blocks of 24 items from the previous layer.  This allows
  // addition and removal very rapidly on average (only a single array access and updating a couple values), with
  // worst-case O(log n) restructuring as items in the buffer are put into blocks and pushed deeper into the tree.

  // The place in the ring buffer where the deeper blocks are considered to exist is given by the `ic` variable.
  // This should always be in-range; since the buffer is size 64, ic should be between 0 and 63 inclusive.  The
  // elements in the left edge of the buffer are at ic - nl, ic - nl + 1, ..., ic - 1, all modulo 64 (i.e. & 0x3F).
  // The elements in the right edge are at ic + 1, ..., ic + nr - 1.  There should be at most 62 items in the ring
  // buffer, leaving a one-element gap between the ends, and one element for the deeper inner layers.

  // On the outer layer, all elements in the gap are null.  In the inner layers, they may be emptied buffers, but at
  // least _one_ element must remain null as a sentinel.  A best-effort is made to keep emptied buffers contiguous
  // with the ends of the utilized portion so re-use is easier, but this should not be relied upon for correctness,
  // only efficiency.  The at-least-one-null invariant should be maintained and can be relied upon for correctness.

  // The len variable is both used for total length and used to maintain an acquire-release lock that guards access to
  // every other variable save vzn (which is used to assess the progress of potentially concurrent operations).
  // You may only safely access variables after a getAndSetAcquire of the len (which will be non-negative when it represents
  // the real value, or -1 if another thread has acquired it); and before the setRelease of the updated len value.

  // The one exception is `vzn` which may be checked (atomically) to see if any progress is being made when one fails to acquire
  // a non-negative len in a spin-wait.  The value of vzn should only be updated inside the acquire-release boundaries.

  // Because of this, the invariant that nl + nr + 24*(if deeper eq null 0 else deeper.len) == len will be broken during modification.
  // It is very important to keep track of this invariant when deeper layers are updated because they do not use the same
  // top-level functions; the outermost layer should not adjust len until all modifications are done, whereas inner layers should
  // eagerly adjust len as soon as possible so we don't need to consider whether an inner layer is in a consistent or inconsistent state.

  // When emptied, arrays in inner layers are preserved, with a best-effort attempt to keep the empty buffers adjacent to the end of the
  // active area for efficient reuse.  So if there is an empty, nulled-out, waiting buffer, it will be at (ic - nl - 1) or (ic + nr).

  private[ConcurrentSplitDeque] var len: Int = 0   // The total number of stored items, or negative to indicate a computation underway
  private[ConcurrentSplitDeque] var vzn: Int = 1   // A modification version number that is updated whenever something changes (values must be odd; inner copies are even)
  private var ic: Int = 32   // The insertion point for any more deeply nested items (null if there are none); should have 0 <= ic < 64
  private var nl: Int = 0    // The number of items inserted to the left of the insertion point (growing left)
  private var nr: Int = 0    // The number of items inserted at and to the right of the insertion point (growing right)
  private val buffer: Array[AnyRef] = new Array[AnyRef](64)  // A buffer to hold items at this level; accessed mod 64 (i.e. & 0x3F)

  private inline def deeper: ConcurrentSplitDeque[Array[AnyRef]] = buffer(ic).asInstanceOf[ConcurrentSplitDeque[Array[AnyRef]]] // Items in the center of the deque, batched in 24-wide blocks
  private inline def deeper_=(csdao: ConcurrentSplitDeque[Array[AnyRef]]): Unit = buffer(ic) = csdao


  ////////////////////////////////////////////////////////////////////////////////////
  // Methods to add single elements including batching and moving to deeper layers //
  ///////////////////////////////////////////////////////////////////////////////////

  // Make sure deeper exists, creating it if not.  Only call if it is needed.
  private def ensureDeeper(): Unit =
    if deeper eq null then
      val deep = new ConcurrentSplitDeque[Array[AnyRef]]()
      deep.vzn = if (vzn & 1) == 1 then 2 else vzn + 2
      deeper = deep

  // Adds an element to the front of the deque (unguarded--make sure there's room!).
  // Breaks len invariant, so only call on outer layer.
  private def outerAddLeft(a: A): Unit =
    if nl + nr == 62 then implMoveIn()
    nl += 1
    buffer((ic - nl) & 0x3F) = a.asInstanceOf[AnyRef]

  // Adds an element to the back of the queue (unguarded).
  // Breaks len invariant, so only call on outer layer.
  private def outerAddRight(a: A): Unit =
    if nl + nr == 62 then implMoveIn()
    nr += 1
    buffer((ic + nr) & 0x3F) = a.asInstanceOf[AnyRef]

  // Adds a block (by reuse or allocation) to the beginning of the deque (inner layers only).
  // Maintains len invariant and preserves the null element invariant.  Only call on inner layers.
  private def innerReuseLeft(gen: () => A): A =
    if nl + nr == 62 then implMoveIn()
    nl += 1
    len += 1
    val i = (ic - nl) & 0x3F
    val a = buffer(i)
    if a eq null then
      // We might be about to overwrite the sentinel null, so we'd better check that there's another null
      val ii = (i - 1) & 0x3F
      val b = buffer(ii)
      if b ne null then
        // Move the neighbor to our spot and use it, and null out the neighbor
        buffer(i) = b
        buffer(ii) = null
        b.asInstanceOf[A]
      else
        // Safe to use this null spot because there's another null past it
        val ans = gen()
        buffer(i) = ans.asInstanceOf[AnyRef]
        ans
    else a.asInstanceOf[A]

  // Adds a block (by reuse or allocation) to the end of the deque (inner layers only).
  // Maintains len invariant and preserves the null element invariant.  Only call on inner layers.
  private def innerReuseRight(gen: () => A): A =
    if nl + nr == 62 then implMoveIn()
    nr += 1
    len += 1
    val i = (ic + nr) & 0x3F
    val a = buffer(i)
    if a eq null then
      // We might be about to overwrite the sentinel null, so we'd better check that there's another null
      val ii = (i + 1) & 0x3F
      val b = buffer(ii)
      if b ne null then
        // Move the neighbor to our spot and use it, and null out the neighbor
        buffer(i) = b
        buffer(ii) = null
        b.asInstanceOf[A]
      else
        // Safe to use this null spot because there's another null past it
        val ans = gen()
        buffer(i) = ans.asInstanceOf[AnyRef]
        ans
    else a.asInstanceOf[A]

  // Moves a requested number of elements from the inside of the left edge into a buffer, healing the left edge
  // and, if it's an inner layer, dragging along empty any buffers.  The caller must ensure there are
  // enough elements.
  // len is NOT updated because this might be an extraction into a buffer we own.
  private def extractLeft(k: Int, target: Array[AnyRef], start: Int = 0): Unit =
    // Moving left, put items into buffer
    var j = (ic - 1) & 0x3F
    var i = start + k - 1
    var stop = (ic - k - 1) & 0x3F
    while j != stop do
      target(i) = buffer(j)
      i -= 1
      j = (j - 1) & 0x3F
    // Keep moving left, moving any remaining elements in close
    stop = (ic - nl - 1) & 0x3F
    i = (ic - 1) & 0x3F
    while j != stop do
      buffer(i) = buffer(j)
      i = (i - 1) & 0x3F
      j = (j - 1) & 0x3F
    if (vzn & 1) == 0 then
      // We are in an inner buffer, so drag along empty buffers until we hit null sentinel
      var found = true
      while found do
        val x = buffer(j)
        found = x ne null
        if found then
          buffer(i) = x
          j = (j - 1) & 0x3F
          i = (i - 1) & 0x3F
      // Null out until we hit sentinel
      while i != j do
        buffer(i) = null
        i = (i - 1) & 0x3F
    else
      // Outer buffer, just null out to end
      while i != j do
        buffer(i) = null
        i = (i - 1) & 0x3F
    nl -= k 


  // Moves a requested number of elements from the inside of the right edge into a buffer, healing the right edge
  // and, if it's an inner layer, dragging along empty any buffers.  The caller must ensure there are
  // enough elements.
  // len is NOT updated because this might be an extraction into a buffer we own.
  private def extractRight(k: Int, target: Array[AnyRef], start: Int = 0): Unit =
    // Moving right, put items into buffer
    var j = (ic + 1) & 0x3F
    var i = start
    var stop = (ic + k + 1) & 0x3F
    while j != stop do
      target(i) = buffer(j)
      i += 1
      j = (j + 1) & 0x3F
    // Keep moving right, moving any remaining elements in close
    stop = (ic + nr + 1) & 0x3F
    i = (ic + 1) & 0x3F
    while j != stop do
      buffer(i) = buffer(j)
      i = (i + 1) & 0x3F
      j = (j + 1) & 0x3F
    if (vzn & 1) == 0 then
      // We are in an inner buffer, so drag along empty buffers until we hit null sentinel
      var found = true
      while found do
        val x = buffer(j)
        found = x ne null
        if found then
          buffer(i) = x
          j = (j + 1) & 0x3F
          i = (i + 1) & 0x3F
      // Null out until we hit sentinel
      while i != j do
        buffer(i) = null
        i = (i + 1) & 0x3F
    else
      // Outer buffer, just null out to end
      while i != j do
        buffer(i) = null
        i = (i + 1) & 0x3F
    nr -= k

  // Restructures by moving 24 items from the left edge of the buffer at our level
  // into a deeper level.
  // Valid for both inner and outer layers.  Because the content is the same, just
  // organized differently, len does not need to be updated at this level, and
  // the inner level has its len appropriately maintained.
  // Only call when there are more than 24 items on the left (leave at least one).
  private def implMoveInLeft(): Unit =
    ensureDeeper()
    val a = deeper.innerReuseLeft(() => new Array[AnyRef](24))
    extractLeft(24, a, 0)

  // Restructures by moving 24 items from the right edge of the buffer at our level
  // into a deeper level.
  // Valid for both inner and outer layers.  Because the content is the same, just
  // organized differently, len does not need to be updated at this level, and
  // the inner level has its len appropriately maintained.
  // Only call when there are more than 24 items on the right (leave at least one).
  private def implMoveInRight(): Unit =
    ensureDeeper()
    val a = deeper.innerReuseRight(() => new Array[AnyRef](24))
    extractRight(24, a, 0)

  // Restructures by moving 24 items from our buffer into a deeper level.
  // Only call when nl+nr is at least 49 (so one will be greater than 24).
  // Favors moving the right edge; standard queue operation adds to the right.
  private def implMoveIn(): Unit =
    if nr > 24 then implMoveInRight()
    else if nl > 24 then implMoveInLeft()


  /////////////////////////////////////////////////////////////////////////////////////////////////////
  // Methods to take single elements including removing deeper batches and expanding into our buffer //
  /////////////////////////////////////////////////////////////////////////////////////////////////////

  // Retrieves a single element from the front of the deque while maintaining len invariant.
  // Deeper layers are expanded or shifted if necessary.
  // Only call on inner layers, and only when they are nonempty.
  private def innerUnloadLeft(): A =
    if nl == 0 then implMoveOutLeft()
    len -= 1
    val ans = buffer((ic - nl) & 0x3F)
    nl -= 1
    ans.asInstanceOf[A]

  // Retrieves a single element from the back of the deque while maintaining len invariant.
  // Deeper layers are expanded or shifted if necessary.
  // Only call on inner layers, and only when they are nonempty.
  private def innerUnloadRight(): A =
    if nr == 0 then implMoveOutRight()
    len -= 1
    val ans = buffer((ic + nr) & 0x3F).asInstanceOf[A]
    nr -= 1
    ans.asInstanceOf[A]
    

  // Restructures deeper layers so we have up to an extra 24 elements on the left edge.  Only call if we
  // already have fewer than 24 elements on the left edge.
  // The right edge may shrink if necessary to make space for the left edge; if the two are contiguous we
  // just shuffle the insertion point ic to reflect that we're considering more to be on the left.
  // Because this is a restructuring, the this layer does not need its len updated.
  // Deeper layers have their lengths updated as needed.
  private def implMoveOutLeft(): Unit =
    val deep = deeper
    if (deep eq null) || deep.len == 0 then
      // Shift only
      val n = (nr >> 2)
      val stop = (ic + (1 + nr - n)) & 0x3F
      var i = (ic + 1) & 0x3F
      while i != stop do
        buffer(ic) = buffer(i)
        ic = i
        i = (i + 1) & 0x3F
      buffer(ic) = deep
      nl += nr - n
      nr = n
    else
      if nr + nl + 24 > 62 then implMoveInRight()  // Make space if needed by pushing right edge deeper
      val a = deeper.innerUnloadLeft()             // Get one inner block to expand into our left edge
      var i = 1
      while i <= nl do
        buffer((ic - (i+24)) & 0x3F) = buffer((ic - i) & 0x3F)
        i += 1
      i = 0
      while i < a.length do
        buffer((ic - (24-i)) & 0x3F) = a(i)
        a(i) = null
        i += 1
      nl += 24

  // Restructures deeper layers so we have up to an extra 24 elements on the right edge.  Only call if we
  // already have fewer than 24 elements on the right edge.
  // The left edge may shrink if necessary to make space for the right edge; if the two are contiguous we
  // just update the insertion point ic to reflect that we're considering more to be on the right.
  // Because this is a restructuring, the this layer does not need its len updated.
  // Deeper layers have their lengths updated as needed.
  private def implMoveOutRight(): Unit =
    val deep = deeper
    if (deep eq null) || deep.len == 0 then
      val n = (nl >> 2)
      val stop = (ic - (1 + nl - n)) & 0x3F
      var i = (ic - 1) & 0x3F
      while i != stop do
        buffer(ic) = buffer(i)
        ic = i
        i = (i - 1) & 0x3F
      buffer(ic) = deep
      nr += nl - n
      nl = n
    else
      if nr + nl + 24 > 62 then implMoveInLeft()  // Make space if needed by pushing left edge deeper
      val a = deeper.innerUnloadRight()           // Get one inner block to expand into our right edge
      var i = 1
      while i <= nr do
        buffer((ic + (i + 24)) & 0x3F) = buffer((ic + i) & 0x3F)
        i += 1
      i = 0
      while i < a.length do
        buffer((ic + i + 1) & 0x3F) = a(i)
        a(i) = null
        i += 1
      nr += 24

  // Retrieves a single element from the front of the deque, restructuring as needed.
  // Only call from the outer layer; this does not update len.
  private def outerTakeLeft(): A =
    if nl == 0 then implMoveOutLeft()
    val j = (ic - nl) & 0x3F
    val ans = buffer(j)
    buffer(j) = null
    nl -= 1
    ans.asInstanceOf[A]

  // Retrieves a single element from the back of the deque, restructuring as needed.
  // Only call from the outer layer; this does not update len.
  private def outerTakeRight(): A =
    if nr == 0 then implMoveOutRight()
    val j = (ic + nr) & 0x3F
    val ans = buffer(j)
    buffer(j) = null
    nr -= 1
    ans.asInstanceOf[A]



  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  // Methods to extract multiple elements, transferring internal structure to external Chunk as needed //
  ///////////////////////////////////////////////////////////////////////////////////////////////////////

  // Extracts the entire (non-empty) internal structure into a Chunk, leaving the collection empty.
  // len is set if and only if this is an inner layer (which we detect by checking if vzn is even)
  private def implTakeFullChunk(): ConcurrentSplitDeque.Chunk[A] =
    val c = new ConcurrentSplitDeque.Chunk[A](nl, nr)
    var i = 0
    var j = (ic - nl) & 0x3F
    while i < nl do
      c.buffer(i) = buffer(j)
      buffer(j) = null
      i += 1
      j = (j + 1) & 0x3F
    j = (ic + 1) & 0x3F
    while i < nl + nr do
      c.buffer(i) = buffer(j)
      buffer(j) = null
      i += 1
      j = (j + 1) & 0x3F
    val deep = deeper
    if (deep ne null) && deep.len > 0 then
      c.deeper = deep.implTakeFullChunk()
      c.len += 24 * c.deeper.len
      if deep.deeper ne null then deep.deeper = null
    ic = 32
    nl = 0
    nr = 0
    if (vzn & 1) == 0 then len = 0   // Inner layer needs len maintained
    c

  // Replaces the entire (non-empty) internal structure with a chunk which is consumed in the process.
  // len is set if and only if this is an inner layer (which we detect by checking if vzn is even)
  private def implBecomeChunk(consumed: ConcurrentSplitDeque.Chunk[A]): Unit =
    nl = consumed.nl
    nr = consumed.nr
    ic = nl+1
    var i = 0
    while i < nl do
      buffer(i) = consumed.buffer(i)
      consumed.buffer(i) = null
      i += 1
    var j = i + 1  // Skip ic
    while i < consumed.buffer.length do
      buffer(j) = consumed.buffer(i)
      consumed.buffer(i) = null
      i += 1
      j += 1
    if consumed.deeper ne null then
      ensureDeeper()
      deeper.implBecomeChunk(consumed.deeper)
    if (vzn & 1) == 0 then
      len = nl + nr
      if consumed.deeper ne null then
        len += 24 * deeper.len

  // Extracts up to n elements--where n is at least 1 but is less than all the elements--from the front of the
  // deque, returning the removed structure as a Chunk.  len is set if and only if this is an internal layer
  // (which we detect by checking if vzn is even)
  private def implTakeLeftChunk(n: Int): ConcurrentSplitDeque.Chunk[A] =
    val deep = deeper
    var nc = if deep eq null then 0 else 24*deep.len
    // First figure out what the structure of the chunk will be
    var cl = if n > nl then nl else n
    var cc = if (n - cl) > nc then nc else n - cl
    var cr = n - (cl + cc)
    if cc < nc then
      cr = cc % 24
      cc -= cr
    val c = new ConcurrentSplitDeque.Chunk[A](cl, cr)
    // Take as much of the left edge as is needed--pretend it's always everything so we can use extractLeft
    val scoot = nl - cl
    ic = (ic - scoot) & 0x3F
    nr += scoot
    nl -= scoot
    extractLeft(cl, c.buffer, 0)
    ic = (ic + scoot) & 0x3F
    nr -= scoot
    nl = scoot   // This is also how much is left
    // Take inner layers (recursively) if there are any
    if cc > 0 then
      c.deeper = deep.implTakeLeftChunk(cc / 24)
      c.len += 24 * c.deeper.len
    // Fill out the right-hand side if there is any
    if cr > 0 then
      if cc >= nc then
        // We used up everything else, so we take from the inside of the right edge
        extractRight(cr, c.buffer, cl)
      else
        // We ended in a deeper block, so we need to pull it out (it's now on the left edge)
        val x = deep.innerUnloadLeft()
        val excess = 24 - cr
        var i = 0
        while i < cr do
          c.buffer(i+cl) = x(i)
          x(i) = null
          i += 1
        if excess > 0 then
          // Put the excess at our left edge which we just cleared out
          if nr + excess > 62 then implMoveInRight()
          var j = (ic - excess) & 0x3F
          while i < 24 do
            buffer(j) = x(i)
            x(i) = null
            i += 1
            j = (j + 1) & 0x3F
          nl = excess
    if (vzn & 1) == 0 then len -= n
    c


  // Extracts up to n elements--where n is at least 1 but is less than all the elements--from the back of the
  // deque, returning the removed structure as a Chunk.  len is set if and only if this is an internal layer
  // (which we detect by checking if vzn is even)
  private def implTakeRightChunk(n: Int): ConcurrentSplitDeque.Chunk[A] =
    val deep = deeper
    var nc = if deep eq null then 0 else 24*deep.len
    // First figure out what the structure of the chunk will be
    var cr = if n > nr then nr else n
    var cc = if (n - cr) > nc then nc else n - cr
    var cl = n - (cr + cc)
    if cc < nc then
      cl = cc % 24
      cc -= cl
    val c = new ConcurrentSplitDeque.Chunk[A](cl, cr)
    // Take as much of the right edge as needed--pretend it's always everything so we can use extractRight
    val scoot = nr - cr
    ic = (ic + scoot) & 0x3F
    nr -= scoot
    nl += scoot
    extractRight(cr, c.buffer, cl)
    nl -= scoot
    ic = (ic - scoot) & 0x3F
    nr = scoot   // This is also how much is left
    // Take inner layers (recursively) if there are any
    if cc > 0 then
      c.deeper = deep.implTakeRightChunk(cc / 24)
      c.len += 24 * c.deeper.len
    // Fill out the left-hand side if there is any
    if cl > 0 then
      if cc >= nc then
        // We used up everything else, so we take from the inside of the left edge
        extractLeft(cl, c.buffer, 0)
      else
        // We ended in a deeper block, so we need to pull it out (it's now on the right edge)
        val x = deep.innerUnloadRight()
        val excess = 24 - cl
        var i = 0
        while i < cl do
          c.buffer(i) = x(i + excess)
          x(i + excess) = null
          i += 1
        if excess > 0 then
          // Put the excess at our right edge which we just cleared out
          if nl + excess > 62 then implMoveInLeft()
          var j = (ic + 1) & 0x3F
          i = 0
          while i < excess do
            buffer(j) = x(i)
            x(i) = null
            i += 1
            j = (j + 1) & 0x3F
          nr = excess
    if (vzn & 1) == 0 then len -= n
    c



  ////////////////////////////////////////////////////
  // Concurrent API, including user-facing routines //
  ////////////////////////////////////////////////////

  private def backoff(fatigue: Int): Int =
    if (fatigue & 0xF) != 0xF then
      fatigue + 1
    else if (fatigue & 0x3F0) != 0x3F0 then
      Thread.onSpinWait()
      fatigue + 0x10
    else if (fatigue & 0xFFC00) != 0xFFC00 then
      var delay = 1 + (fatigue & 0xFFC00)
      if delay > 0xC0000 then delay = delay << 3
      LockSupport.parkNanos(delay)
      fatigue + 0x400
    else
      Int.MaxValue

  private def vercheck(v0: Int): Int =
    if v0 == 0 then vznHandle.getVolatile(this).asInstanceOf[Int]
    else
      val v = vznHandle.getVolatile(this).asInstanceOf[Int]
      if v0 == v then Int.MinValue   // Signals that no progress has been made since the last check and we should probably terminate
      else v

  private def DIE_BLOCKED(): Nothing =
    throw new java.util.ConcurrentModificationException("ConcurrentSplitDeque blocked and making no progress")

  private def DIE_FULL(m: Int): Nothing = 
    lenHandle.setRelease(this, m)
    throw new IllegalArgumentException("Add on full ConcurrentSplitDeque")

  private def compete(): Int =
    var m = -1
    var b = 0
    var v = 0
    while m < 0 do
      b = backoff(b)
      if b == Int.MaxValue then
        v = vercheck(v)
        b = 0xC03FFF
      m = if v == Int.MinValue then Int.MaxValue else lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    m

  /** A numerical representation of instantaneous contention--average multiple calls to get a more representative picture.
    * 
    * 0.0 indicates no contention.  Values above 0.0 but below 1.0 indicate moderate contention handled with spin-waiting.
    * Values at 1.0 or above indicate heavy contention handled with timed sleeps via `LockSupport.park`.  PositiveInfinity
    * indicates that access was never obtained.
    */
  def contention(): Double =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m >= 0 then
      lenHandle.setRelease(this, m)
      0.0
    else
      var b = 0
      while m < 0 && b < Int.MaxValue do
        b = backoff(b)
        m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
      if m >= 0 then lenHandle.setRelease(this, m)
      if b <= 0xF then 0.005*b
      else if b <= 0x3FF then 0.01*(1 + (b >> 4))
      else if b <= 0xFFFFF then (b >> 10).toDouble
      else Double.PositiveInfinity

  /** A numerical representation of instantaneous contention--average multiple calls to get a more representative picture.
    * 
    * 0.0 indicates no contention.  Values above 0.0 but below 1.0 indicate moderate contention handled with spin-waiting.
    * Values at 1.0 or above indicate heavy contention handled with timed sleeps via `LockSupport.park`.  PositiveInfinity
    * indicates that access was never obtained.
    * 
    * This version calls a wakeup callback every time a parked thread resumes with an internal restart counter (which may
    * regress if other threads make progress) and only continues if the callback returns true.
    */
  def contention(onWake: Int => Boolean): Double =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m >= 0 then
      lenHandle.setRelease(this, m)
      0.0
    else
      var b = 0
      var run = true
      while run do
        b = backoff(b)
        m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
        run = m < 0 && (b != Int.MaxValue) && (((b & 0xFFC00) == 0) || onWake(b >>> 10))
      if m >= 0 then lenHandle.setRelease(this, m)
      if b <= 0xF then 0.005*b
      else if b <= 0x3FF then 0.01*(1 + (b >> 4))
      else if b <= 0xFFFFFF then (b >> 10).toDouble
      else Double.PositiveInfinity


  /** The instantaneous stable length of this collection.  This length will be observed by some other thread in the future
    * but there are no guarantees as to which one.
    */
  def length: Int =
    var m = lenHandle.getVolatile(this).asInstanceOf[Int]
    var b = 0
    var v = 0
    while m < 0 do
      b = backoff(b)
      if b == Int.MaxValue then
        v = vercheck(v)
        if v == Int.MinValue then DIE_BLOCKED()
        else b = 0xC03FFF // Get another 256 tries
      m = lenHandle.getVolatile(this).asInstanceOf[Int]
    m

  /** Add a new element at the front of this deque (typical for stack ordering). */
  def pushLeft(a: A): Unit =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m < 0 then
      m = compete()
      if m == Int.MaxValue then DIE_BLOCKED()
      else if Int.MaxValue - m < 8 then DIE_FULL(m)
    outerAddLeft(a)
    vzn += 2
    lenHandle.setRelease(this, m + 1)

  /** Add a new element at the back of this deque (typical for queue ordering or adding to the "second stack"). */
  def pushRight(a: A): Unit =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m < 0 then
      m = compete()
      if m == Int.MaxValue then DIE_BLOCKED()
      else if Int.MaxValue - m < 8 then DIE_FULL(m)
    outerAddRight(a)
    vzn += 2
    lenHandle.setRelease(this, m + 1)

  /** Take the first element from the deque if available or Alt.unit if the deque is empty.  This is typical for both stack and queue ordering. */
  def popLeft(): A Or Unit =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m < 0 then
      m = compete()
      if m == Int.MaxValue then DIE_BLOCKED()
    if m == 0 then
      vzn += 2
      lenHandle.setRelease(this, 0)
      Alt.unit
    else
      val ans = outerTakeLeft()
      vzn += 2
      lenHandle.setRelease(this, m - 1)
      Is(ans)

  /** Take the first element from the deque or throw an exception if none is available. */
  def popLeftUnsafe(): A =
    popLeft().fold(__)(_ => throw new NoSuchElementException("Empty ConcurrentSplitDeque"))

  /** Take the last element from the deque if available or Alt.unit if the deque is empty.  This is typical for using the back of the deque as a second stack. */
  def popRight(): A Or Unit =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m < 0 then
      m = compete()
      if m == Int.MaxValue then DIE_BLOCKED()
    if m == 0 then
      vzn += 2
      lenHandle.setRelease(this, 0)
      Alt.unit
    else
      val ans = outerTakeRight()
      vzn += 2
      lenHandle.setRelease(this, m - 1)
      Is(ans)

  /** Take the last element from the deque or throw an exception if none is available. */
  def popRightUnsafe(): A =
      popRight().fold(__)(_ => throw new NoSuchElementException("Empty ConcurrentSplitDeque"))

  /** Adds a new item at the end of the deque.  Equivalent to pushRight. */
  inline def +=(a: A): Unit = pushRight(a)

  /** Adds a new item to the front of the deque.  Equivalent to pushLeft. */
  inline infix def push(a: A): Unit = pushLeft(a)

  /** Gets an item from the front of the deque if one is available.  Equivalent to popLeft. */
  inline def get() = popLeft()

  /** Removes an item from the front of the deque.  Equivalent to popLeft. */
  inline def pop() = popLeft()

  /** Traverse the queue, copying all contents, without any concurrency controls.  This may throw an exception,
    * return inconsistent data, or otherwise be in error if it is performed while being concurrently modified.
    * It does not modify the underlying data structure, however, so it will not cause other threads to fail.
    */
  def unsafeNondestructiveCopy(): ConcurrentSplitDeque.Chunk[A] =
    val c = new ConcurrentSplitDeque.Chunk[A](nl, nr)
    var i = 0
    var j = (ic - nl) & 0x3F
    while i < nl do
      c.buffer(i) = buffer(j)
      i += 1
      j = (j + 1) & 0x3F
    j = (ic + 1) & 0x3F
    while i < nl + nr do
      c.buffer(i) = buffer(j)
      i += 1
      j = (j + 1) & 0x3F
    val deep = deeper
    if (deep ne null) && deep.len > 0 then
      c.deeper = deep.unsafeNondestructiveCopy()
      c.len += 24 * c.deeper.len
    c


  private def chunkImpl(n: Int, left: Boolean): ConcurrentSplitDeque.Chunk[A] =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m < 0 then
      m = compete()
      if m == Int.MaxValue then DIE_BLOCKED()
    if m == 0 then
      vzn += 2
      lenHandle.setRelease(this, 0)
      ConcurrentSplitDeque.Chunk.empty[A]
    else if n >= m then
      val ans = implTakeFullChunk()
      vzn += 2
      lenHandle.setRelease(this, 0)
      ans
    else
      val ans = if left then implTakeLeftChunk(n) else implTakeRightChunk(n)
      vzn += 2
      lenHandle.setRelease(this, m - n)
      ans

  /** Removes up to a specified number of items from the front of the deque while blocking access for no more than O(log N) time.
    * 
    * It is not an error if there are zero items; it just returns an empty chunk.
    */
  def chunkLeft(maxItems: Int): ConcurrentSplitDeque.Chunk[A] = chunkImpl(math.max(1, maxItems), left = true)

  /** Removes up to a specified number of items from the back of the deque (but still in forward order) while blocking access for no more than O(log N) time.
    * 
    * It is not an error if there are zero items; it just returns an empty chunk.
    */
  def chunkRight(maxItems: Int): ConcurrentSplitDeque.Chunk[A] = chunkImpl(math.max(1, maxItems), left = false)

  /** Equivalent to chunkLeft.  Removes up to a specified number of items in queue-order while blocking access for no more than O(log N) time.
    * 
    * It is not an error if there are zero items; it just returns an empty chunk.
    */
  inline def chunk(maxItems: Int = Int.MaxValue): ConcurrentSplitDeque.Chunk[A] = chunkLeft(maxItems)

  /** Swaps the entire current buffer with an existing chunk.  The chunk must not be used after this, as its internals may be altered.
    * 
    * This method is unsafe because no measures are taken to ensure that the consumed chunk is used only once, and if the same chunk
    * is inserted into two different ConcurrentSplitDeques, arbitrarily bad behavior including long-lasting blocking may result.
    */
  def unsafeSwapChunks(consumed: ConcurrentSplitDeque.Chunk[A]): ConcurrentSplitDeque.Chunk[A] =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m < 0 then
      m = compete()
      if m == Int.MaxValue then DIE_BLOCKED()
    val ans = if m == 0 then ConcurrentSplitDeque.Chunk.empty[A] else implTakeFullChunk()
    m = consumed.len
    implBecomeChunk(consumed)
    vzn += 2
    lenHandle.setRelease(this, m)
    ans

  /** Clears the deque, including all caches, returning the number of elements discarded. */
  def clear(): Int =
    var m = lenHandle.getAndSetAcquire(this, -1).asInstanceOf[Int]
    if m < 0 then
      m = compete()
      if m == Int.MaxValue then DIE_BLOCKED()
    nl = 0
    nr = 0
    ic = 32
    var i = 0
    while i < buffer.length do
      buffer(i) = null  // Sets deeper to null too, along the way!
      i += 1
    lenHandle.setRelease(this, 0)
    m


  def debug(indent: String = ""): Unit =
    println(s"$indent$len $vzn : $nl $ic $nr")
    println(buffer.zipWithIndex.map{ case (x, i) =>
      if x eq null then
        if i == ic then "_" else "-"
      else if x.isInstanceOf[Array[AnyRef]] then
        val a = x.asInstanceOf[Array[AnyRef]]
        if a.length != 24 then "?"
        else
          val n = a.count(_ ne null)
          if n == 0 then "|"
          else if n == 24 then "#"
          else "H"
      else
        if i == ic then "v" else "@"
    }.mkString(indent, "", ""))
    if deeper ne null then deeper.debug(indent + "> ")
}
object ConcurrentSplitDeque {
  import java.lang.invoke.MethodHandles

  private[ConcurrentSplitDeque] final val lenHandle =
    MethodHandles.privateLookupIn(classOf[ConcurrentSplitDeque[?]], MethodHandles.lookup())
      .findVarHandle(classOf[ConcurrentSplitDeque[?]], "len", classOf[Int])

  private[ConcurrentSplitDeque] final val vznHandle =
    MethodHandles.privateLookupIn(classOf[ConcurrentSplitDeque[?]], MethodHandles.lookup())
      .findVarHandle(classOf[ConcurrentSplitDeque[?]], "vzn", classOf[Int])

  def empty[A]: ConcurrentSplitDeque[A] = new ConcurrentSplitDeque[A]()

  final class Chunk[A] private[ConcurrentSplitDeque] (private[ConcurrentSplitDeque] val nl: Int, private[ConcurrentSplitDeque] val nr: Int) {
    private[ConcurrentSplitDeque] var len = nl + nr
    private[ConcurrentSplitDeque] val buffer = new Array[AnyRef](len)
    private[ConcurrentSplitDeque] var deeper: Chunk[Array[AnyRef]] = null
     
    def length: Int = len

    private def unpackArrayFwd[X](a: Array[X], start: Int, x: Array[AnyRef], depth: Int): Int =
      if depth == 0 then
        if a.isInstanceOf[Array[AnyRef]] then
          System.arraycopy(x, 0, a, start, x.length)
        else
          var i = 0
          while i < x.length do
            a(start+i) = x(i).asInstanceOf[X]
            i += 1
        start + x.length
      else
        var i = 0
        var k = start
        while i < x.length do
          k = unpackArrayFwd(a, k, x(i).asInstanceOf[Array[AnyRef]], depth-1)
          i += 1
        k

    private def unpackArrayBkw[X](a: Array[X], start: Int, x: Array[AnyRef], depth: Int): Int =
      var k = start
      if depth == 0 then
        if a.isInstanceOf[Array[AnyRef]] then
          val b = a.asInstanceOf[Array[AnyRef]]
          var i = x.length - 1
          while i >= 0 do
            b(k) = x(i)
            k += 1
            i -= 1
        else
          var i = x.length - 1
          while i >= 0 do
            a(k) = x(i).asInstanceOf[X]
            k += 1
            i -= 1
      else
        var i = x.length - 1
        while i >= 0 do
          k = unpackArrayBkw(a, k, x(i).asInstanceOf[Array[AnyRef]], depth-1)
          i -= 1
      k

    private def intoArrayFwd[X](a: Array[X], start: Int, depth: Int): Int =
      var k = start
      var i = 0
      while i < nl do
        val x = buffer(i).asInstanceOf[Array[AnyRef]]
        k = unpackArrayFwd(a, k, x, depth-1)
        i += 1
      if deeper ne null then
        k = deeper.intoArrayFwd(a, k, depth+1)
      while i < buffer.length do
        val x = buffer(i).asInstanceOf[Array[AnyRef]]
        k = unpackArrayFwd(a, k, x, depth-1)
        i += 1
      k

    private def intoArrayBkw[X](a: Array[X], start: Int, depth: Int): Int =
      var k = start
      var i = buffer.length - 1
      while i >= nl do
        val x = buffer(i).asInstanceOf[Array[AnyRef]]
        k = unpackArrayBkw(a, k, x, depth-1)
        i -= 1
      if deeper ne null then
        k = deeper.intoArrayBkw(a, k, depth+1)
      while i >= 0 do
        val x = buffer(i).asInstanceOf[Array[AnyRef]]
        k = unpackArrayBkw(a, k, x, depth-1)
        i -= 1
      k

    private def unpackForeach[X](a: Array[AnyRef], f: X => Unit, depth: Int = 1): Unit =
      if depth == 0 then
        var i = 0
        while i < a.length do
          f(a(i).asInstanceOf[X])
          i += 1
      else
        var i = 0
        while i < a.length do
          unpackForeach(a(i).asInstanceOf[Array[AnyRef]], f, depth - 1)
          i += 1

    private def deepForeach[X](f: X => Unit, depth: Int = 1): Unit =
      var i = 0
      while i < nl do
        unpackForeach(buffer(i).asInstanceOf[Array[AnyRef]], f, depth-1)
        i += 1
      if deeper ne null then deeper.deepForeach(f, depth + 1)
      while i < buffer.length do
        unpackForeach(buffer(i).asInstanceOf[Array[AnyRef]], f, depth-1)
        i += 1

    def foreach(f: A => Unit): Unit =
      var i = 0
      while i < nl do
        f(buffer(i).asInstanceOf[A])
        i += 1
      if deeper ne null then deeper.deepForeach(f)
      while i < buffer.length do
        f(buffer(i).asInstanceOf[A])
        i += 1

    def toArray(using ClassTag[A]): Array[A] =
      // Fill array by traversing data forward
      val a = new Array[A](len)
      var i = 0
      while i < nl do
        a(i) = buffer(i).asInstanceOf[A]
        i += 1
      if deeper ne null then
        i = deeper.intoArrayFwd(a, i, 1)
      var j = nl
      while j < buffer.length do
        a(i) = buffer(j).asInstanceOf[A]
        i += 1
        j += 1
      a

    def toReversedArray(using ClassTag[A]): Array[A] =
      // Fill array by traversing data backwards
      val a = new Array[A](len)
      var i = 0
      var j = buffer.length - 1
      while j >= nl do
        a(i) = buffer(j).asInstanceOf[A]
        j -= 1
        i += 1
      if deeper ne null then
        i = deeper.intoArrayBkw(a, i, 1)
      while j >= 0 do
        a(i) = buffer(j).asInstanceOf[A]
        j -= 1
        i += 1
      a

    def debug(depth: Int = 0): Unit =
      val inset = "> "*depth
      println(s"$inset$nl + $nr ; $len")
      println(s"$inset${buffer.map(x => if x eq null then "!" else ".").mkString}")
      if deeper ne null then deeper.debug(depth + 1)
  }
  object Chunk {
    private val emptyImpl = new Chunk[Any](0, 0)
    def empty[A] = emptyImpl.asInstanceOf[Chunk[A]]
  }

  /*
  final class Step[A] private (n: Int, depth: Int) extends scala.collection.Stepper[A] {
    private[ConcurrentSplitDeque] val xs = new Array[Array[AnyRef]](if depth > 0 then 2+3*(depth-1) else 0)
    private[ConcurrentSplitDeque] val ix = new Array[Int](2*xs.length)
    private[ConcurrentSplitDeque] val a = new Array[AnyRef](math.min(n, 64))
    private[ConcurrentSplitDeque] var j = -1
    private[ConcurrentSplitDeque] var m = -1
    private[ConcurrentSplitDeque] var i = -1
    private def advance(level: Int = 0): Unit =
      if level == 0 then
        m = ix(1) - ix(0)
        System.arraycopy(xs(0), ix(0), a, 0, m)
        if xs(1) ne null && xs.length > 2 then advance(level + 1)
        else if ix(ix.length - 1) > ix(ix.length - 2) then
          xs(0) = xs(xs.length - 1)
          xs(xs.length - 1) = null
          ix(0) = ix(ix.length - 2)
          ix(1) = ix(ix.length - 1)
          ix(ix.length - 2) = ix(ix.length - 1)
        else
          xs = null
          ix(0) = ix(1)
      else
    def hasStep: Boolean = i >= n
    def nextStep: A =
      if j < 0 then
        if i >= n then throw new NoSuchElementException("Empty Chunk stepper")
        else advance()
      val ans = a(j).asInstanceOf[A]
      a(j) = null
      i += 1
      j += 1
      if j >= m then
        j = -1
  }
  */
}


// /** Provides a `Deqeue` interface.
//   * The underlying data structure is a 32-wide B-tree of indices, somewhat like `immutable.Vector`, to enable rapid
//   * `splitAt` and retain decent enqueue/dequeue/push/pop performance.
//   */
// final class ConcurrentSplitDeque[A >: Null <: AnyRef] private (initialContents: ConcurrentSplitDeque.Content[A])
// extends mutable.AbstractSeq[A]
// with SeqOps[A, ConcurrentSplitDeque, ConcurrentSplitDeque[A]]
// with mutable.Growable[A]
// with IterableFactoryDefaults[A, ConcurrentSplitDeque]
// with DefaultSerializable {
//   private var contents: ConcurrentSplitDeque.Content[A] = initialContents

//   // TODO -- turn this into a collection
// }


/*
object ConcurrentSplitDeque 
extends IterableFactory[ConcurrentSplitQueue] {


  final class Content[A] private[ConcurrentSplitDeque] () {
    private[ConcurrentSplitDeque] var len: Int = 0
    private[ConcurrentSplitDeque] var version: Int = 0
    private var left: Int = 16
    private var right: Int = 16
    private var data = new Array[AnyRef](32)
    private var chunks: Content[Array[AnyRef]] = null
  }
  object Content {

    private[ConcurrentSplitDeque] final val versionHandle =
      java.lang.invoke.MethodHandles.privateLookupIn(classOf[Content[_]], java.lang.invoke.MethodHandles.lookup()).findVarHandle(classOf[Content[_]], "len", classOf[Int])
  }
}
*/
