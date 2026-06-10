// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr

package kse.loom


/** The single-threaded engine for SplitDeque: one instance per level of a
  * finger-tree-flavored blocked deque.  Level 0 slots are user elements; level
  * k slots are blocks holding `blockSize` level k-1 slots.  A concurrent shell
  * provides locking; nothing here is thread-safe.
  *
  * == Representation ==
  *
  * Each level is a ring (`Array[AnyRef]`, power-of-two capacity) split by a
  * logical `anchor`: the left flank occupies logical positions
  * `anchor - nl until anchor`, the right flank `anchor until anchor + nr`, and
  * the `deeper` level sits conceptually between them.  All positions are
  * logical and only masked at access, via Ring (see Ring.scala for the
  * conventions; no wraparound arithmetic appears in this file).
  *
  * Because the boundary between flanks is just `anchor`, rebalancing the two
  * flanks when `deeper` is empty is pure accounting — move `anchor`, adjust
  * `nl`/`nr`, copy nothing.
  *
  * == Blocks and raggedness ==
  *
  * Raggedness is the only representation: a slot at level k >= 1 may hold any
  * number of elements from 1 to blockSize^k, and every code path consults
  * stored sizes rather than assuming fullness.  Sizes live with the container:
  *   - a level's slot weights are in its parallel `sizes` ring;
  *   - a level-1 block is a plain `Array[AnyRef](blockSize)` with children
  *     packed at `0 until m` and nulls after (its own weight m is in the
  *     containing ring or block);
  *   - a deeper block is `Array[AnyRef](blockSize + 1)`: slot 0 holds an
  *     `Array[Int](blockSize)` of child weights (packed, zeros after), slots
  *     `1 until m+1` hold the children.
  *
  * Spills create full blocks; ragged blocks arise only from splicing, but no
  * path may assume uniformity.
  *
  * == Mirror symmetry ==
  *
  * Left/right pairs are written once.  Hot ops (`pushSlot`, `popSlot`) take an
  * `inline` direction and specialize at compile time, costing nothing at
  * runtime; cold restructuring ops (`makeRoom`, `refill`, `unpack`) branch on a
  * runtime boolean, which is fine off the fast path and keeps one source of
  * truth.
  *
  * `validate()` is the executable spec: it checks every invariant of the whole
  * structure and is run after every operation by the differential fuzz tests.
  */
private[kse] final class SplitDequeImpl(val lgCap: Int, val blockSize: Int, val depth: Int) {
  if lgCap < 1 || lgCap > 16 then throw new IllegalArgumentException(s"lgCap out of range: $lgCap")
  if blockSize < 1 || (blockSize << 1) > (1 << lgCap) then throw new IllegalArgumentException(s"blockSize $blockSize not in 1..${1 << (lgCap-1)}")

  private[kse] val mask = (1 << lgCap) - 1
  private val ring = new Array[AnyRef](mask + 1)
  private val sizes: Array[Int] = if depth > 0 then new Array[Int](mask + 1) else null
  private var anchor = 0
  private var nl = 0
  private var nr = 0
  private[kse] var count = 0                       // total elements at and below this level
  private var deeper: SplitDequeImpl = null
  private[kse] var xfer = 0                        // weight of the slot last packed or popped
  private var spare: Array[AnyRef] = null          // pooled empty blocks: at depth 0 a leaf block,
  private var spare2: Array[AnyRef] = null         //   deeper a block with its zeroed header in slot 0
  private[kse] var allocs = 0                      // blocks allocated fresh at this level (test support)


  // === Hot paths: single-slot push and pop, direction specialized at compile time ===

  private inline def pushSlot(inline left: Boolean)(x: AnyRef, w: Int): Unit =
    if nl + nr > mask then makeRoom(left)
    inline if left then
      nl += 1
      Ring.set(ring, mask, anchor - nl, x)
      if depth > 0 then Ring.set(sizes, mask, anchor - nl, w)
    else
      Ring.set(ring, mask, anchor + nr, x)
      if depth > 0 then Ring.set(sizes, mask, anchor + nr, w)
      nr += 1
    count += w

  // Caller must ensure count > 0.  Weight of the popped slot is left in xfer.
  private inline def popSlot(inline left: Boolean): AnyRef =
    inline if left then
      if nl == 0 then refill(true)
      val p = anchor - nl
      val x = Ring.at(ring, mask, p)
      Ring.set(ring, mask, p, null)
      var w = 1
      if depth > 0 then
        w = Ring.at(sizes, mask, p)
        Ring.set(sizes, mask, p, 0)
      nl -= 1
      count -= w
      xfer = w
      x
    else
      if nr == 0 then refill(false)
      val p = anchor + nr - 1
      val x = Ring.at(ring, mask, p)
      Ring.set(ring, mask, p, null)
      var w = 1
      if depth > 0 then
        w = Ring.at(sizes, mask, p)
        Ring.set(sizes, mask, p, 0)
      nr -= 1
      count -= w
      xfer = w
      x


  // === Cold paths: restructuring between levels ===

  // Spill blockSize inner slots of the larger flank (ties break toward `left`)
  // into the deeper level, then close the hole next to the anchor by sliding
  // whichever flank is cheaper.  Requires the larger flank to have at least
  // blockSize slots, which `blockSize <= cap/2` guarantees whenever the ring is
  // full; refill() also calls this with one flank empty and the other large.
  private def makeRoom(left: Boolean): Unit =
    if deeper eq null then deeper = new SplitDequeImpl(lgCap, blockSize, depth + 1)
    val spillL = if nl == nr then left else nl > nr
    if spillL then
      val blk = packBlockN(anchor - blockSize, blockSize)
      deeper.pushSlot(true)(blk, xfer)
      nl -= blockSize
      if nl <= nr then
        Ring.slide(ring, mask, anchor - nl - blockSize, anchor - nl, nl)
        if depth > 0 then Ring.slide(sizes, mask, anchor - nl - blockSize, anchor - nl, nl)
      else
        Ring.slide(ring, mask, anchor, anchor - blockSize, nr)
        if depth > 0 then Ring.slide(sizes, mask, anchor, anchor - blockSize, nr)
        anchor = (anchor - blockSize) & mask
    else
      val blk = packBlockN(anchor, blockSize)
      deeper.pushSlot(false)(blk, xfer)
      nr -= blockSize
      if nr <= nl then
        Ring.slide(ring, mask, anchor + blockSize, anchor, nr)
        if depth > 0 then Ring.slide(sizes, mask, anchor + blockSize, anchor, nr)
      else
        Ring.slide(ring, mask, anchor - nl, anchor - nl + blockSize, nl)
        if depth > 0 then Ring.slide(sizes, mask, anchor - nl, anchor - nl + blockSize, nl)
        anchor = (anchor + blockSize) & mask

  // Both pooling sites are at the same level: packBlockN makes blocks here to
  // push one level down, and unpack dismantles blocks popped from one level
  // down, so the stash never mixes kinds.  Two slots cover steady-state
  // pack/unpack oscillation at a flank boundary; bursts beyond that go to GC.
  private def takeSpare(): Array[AnyRef] =
    val b = spare
    if b ne null then
      spare = spare2
      spare2 = null
    b

  private def stash(blk: Array[AnyRef]): Unit =
    if spare eq null then spare = blk
    else if spare2 eq null then spare2 = blk

  // Pack the c slots (1 <= c <= blockSize) at logical positions p0 until p0+c
  // into an empty block, nulling them in the ring(s).  Total weight is left in
  // xfer.  Spills pack full blocks; flushes may pack ragged ones.
  private def packBlockN(p0: Int, c: Int): AnyRef =
    if depth == 0 then
      var blk = takeSpare()
      if blk eq null then
        blk = new Array[AnyRef](blockSize)
        allocs += 1
      Ring.moveOut(ring, mask, p0, c, blk, 0)
      xfer = c
      blk
    else
      var blk = takeSpare()
      var hdr: Array[Int] = null
      if blk eq null then
        blk = new Array[AnyRef](blockSize + 1)
        hdr = new Array[Int](blockSize)
        blk(0) = hdr
        allocs += 1
      else hdr = blk(0).asInstanceOf[Array[Int]]
      Ring.moveOut(ring, mask, p0, c, blk, 1)
      Ring.moveOut(sizes, mask, p0, c, hdr, 0)
      var w = 0
      var k = 0
      while k < c do
        w += hdr(k)
        k += 1
      xfer = w
      blk

  // Make the `left` flank nonempty.  Caller must ensure count > 0 and that the
  // flank is empty.  Either unpacks one block from the deeper level (spilling
  // the opposite flank first if the incoming children might not fit), or, with
  // nothing deeper, donates the whole opposite flank by moving the anchor —
  // which is free.
  private def refill(left: Boolean): Unit =
    val deep = deeper
    if (deep ne null) && deep.count > 0 then
      if nl + nr + blockSize > mask + 1 then makeRoom(left)
      val blk = (if left then deep.popSlot(true) else deep.popSlot(false)).asInstanceOf[Array[AnyRef]]
      unpack(left, blk)
    else if left then
      anchor = (anchor + nr) & mask
      nl += nr
      nr = 0
    else
      anchor = (anchor - nl) & mask
      nr += nl
      nl = 0

  // Unpack a block's children onto the outside of the `left` flank.  The
  // emptied block (header zeroed back into slot 0) is stashed for reuse.
  private def unpack(left: Boolean, blk: Array[AnyRef]): Unit =
    if depth == 0 then
      var m = 0
      while m < blockSize && (blk(m) ne null) do m += 1
      if left then
        Ring.moveIn(ring, mask, anchor - nl - m, blk, 0, m)
        nl += m
      else
        Ring.moveIn(ring, mask, anchor + nr, blk, 0, m)
        nr += m
    else
      val hdr = blk(0).asInstanceOf[Array[Int]]
      var m = 0
      while m < blockSize && hdr(m) != 0 do m += 1
      if left then
        Ring.moveIn(ring, mask, anchor - nl - m, blk, 1, m)
        Ring.moveIn(sizes, mask, anchor - nl - m, hdr, 0, m)
        nl += m
      else
        Ring.moveIn(ring, mask, anchor + nr, blk, 1, m)
        Ring.moveIn(sizes, mask, anchor + nr, hdr, 0, m)
        nr += m
    stash(blk)


  // === Splicing and splitting ===

  private def ensureDeeper(): SplitDequeImpl =
    if deeper eq null then deeper = new SplitDequeImpl(lgCap, blockSize, depth + 1)
    deeper

  // Pack one whole flank into blocks (as evenly sized as possible, so seam
  // blocks stay at least half full whenever the flank has at least blockSize/2
  // slots) and move them into the deeper level on the same side: the right
  // flank appends to deeper's right, the left flank prepends to deeper's left.
  // Pure rearrangement: count is unchanged.
  private def flushFlank(rightSide: Boolean): Unit =
    val q = if rightSide then nr else nl
    if q > 0 then
      val deep = ensureDeeper()
      val nblk = (q + blockSize - 1) / blockSize
      val base = q / nblk
      val extra = q % nblk
      if rightSide then
        var done = 0
        var i = 0
        while i < nblk do
          val c = base + (if i < extra then 1 else 0)
          val blk = packBlockN(anchor + done, c)
          deep.pushSlot(false)(blk, xfer)
          done += c
          i += 1
        nr = 0
      else
        var i = nblk - 1
        while i >= 0 do
          val c = base + (if i < extra then 1 else 0)
          val off = i * base + (if i < extra then i else extra)
          val blk = packBlockN(anchor - nl + off, c)
          deep.pushSlot(true)(blk, xfer)
          i -= 1
        nl = 0

  // Take over another level's entire contents, leaving it empty.  Both our
  // flanks must already be empty (our deeper may be an empty shell, which is
  // discarded).  Geometry and depth must match.
  private def become(y: SplitDequeImpl): Unit =
    System.arraycopy(y.ring, 0, ring, 0, ring.length)
    if depth > 0 then System.arraycopy(y.sizes, 0, sizes, 0, sizes.length)
    anchor = y.anchor
    nl = y.nl
    nr = y.nr
    count = y.count
    deeper = y.deeper
    java.util.Arrays.fill(y.ring, null.asInstanceOf[AnyRef])
    if y.depth > 0 then java.util.Arrays.fill(y.sizes, 0)
    y.anchor = 0
    y.nl = 0
    y.nr = 0
    y.count = 0
    y.deeper = null

  // Append all of y's contents (same depth and geometry) after ours, leaving y
  // empty.  O(cap) slot moves per level plus recursion: O(blockSize * levels).
  private def absorbRight(y: SplitDequeImpl): Unit =
    val yDeep = (y.deeper ne null) && y.deeper.count > 0
    val weDeep = (deeper ne null) && deeper.count > 0
    if yDeep && !weDeep then
      // We are shallow: prepend our slots onto y (rightmost first), then take y over
      while nl + nr > 0 do
        val x = popSlot(false)
        y.pushSlot(true)(x, xfer)
      become(y)
    else
      if yDeep then
        // Both deep: our right flank sinks (appended), y's left flank sinks
        // (prepended), the deeper levels merge, and y's right flank remains
        // for the common drain below.
        flushFlank(true)
        y.flushFlank(false)
        val taken = y.deeper.count
        deeper.absorbRight(y.deeper)
        count += taken
        y.count -= taken
      // Drain whatever remains of y (its whole contents if it was shallow)
      while y.nl + y.nr > 0 do
        val x = y.popSlot(true)
        pushSlot(false)(x, y.xfer)
      y.deeper = null

  // Move a maximal prefix (left = true) or suffix (left = false) of whole
  // slots, totaling at most lim elements, from this level into out (same
  // depth), and return the number of elements moved.  out receives on the
  // opposite side: prefixes append to out's right, suffixes prepend to out's
  // left, so elements keep their order.
  //
  // Within one level: take flank slots while they fit; on flank exhaustion
  // either adopt the entire deeper level in O(1) when it fits, or recurse to
  // move whole deeper blocks, after which exactly one boundary block is too
  // heavy — refill() unpacks it into our flank and the loop resumes taking its
  // finer slots.  At depth 0 slot weight is 1, so the take is always exact.
  //
  // Ordering invariant: before anything may be appended to out's deeper level,
  // out's receiving flank must be flushed down, since those slots come earlier
  // in sequence order.
  private def takeEnd(out: SplitDequeImpl, lim: Int, left: Boolean): Int =
    var moved = 0
    while moved < lim do
      if (if left then nl else nr) == 0 && ((deeper eq null) || deeper.count == 0) then refill(left)
      val fn = if left then nl else nr
      if fn > 0 then
        val p = if left then anchor - nl else anchor + nr - 1
        val w = if depth > 0 then Ring.at(sizes, mask, p) else 1
        if moved + w > lim then return moved      // boundary block; parent will unpack it
        val x = Ring.at(ring, mask, p)
        Ring.set(ring, mask, p, null)
        if depth > 0 then Ring.set(sizes, mask, p, 0)
        if left then nl -= 1 else nr -= 1
        count -= w
        if left then out.pushSlot(false)(x, w) else out.pushSlot(true)(x, w)
        moved += w
      else
        val deep = deeper                          // nonempty, else refill had handled it
        out.flushFlank(left)
        if deep.count <= lim - moved then
          val dc = deep.count
          moved += dc
          count -= dc
          deeper = null
          if (out.deeper eq null) || out.deeper.count == 0 then out.deeper = deep
          else if left then out.deeper.absorbRight(deep)
          else
            deep.absorbRight(out.deeper)
            out.deeper = deep
          out.count += dc
        else
          val got = deep.takeEnd(out.ensureDeeper(), lim - moved, left)
          moved += got
          count -= got
          out.count += got
          if moved < lim then refill(left)
    moved

  /** Append the donor's entire contents after ours, in O(blockSize * log n)
    * time, leaving the donor empty.  The donor must have identical geometry
    * and must not be this structure itself.
    */
  def spliceRight(donor: SplitDequeImpl): Unit =
    checkMate(donor)
    absorbRight(donor)

  /** Prepend the donor's entire contents before ours, in O(blockSize * log n)
    * time, leaving the donor empty.  The donor must have identical geometry
    * and must not be this structure itself.
    */
  def spliceLeft(donor: SplitDequeImpl): Unit =
    checkMate(donor)
    donor.absorbRight(this)
    become(donor)

  /** Remove the first n elements (all of them, if we have no more than n) into
    * a new structure of the same geometry, in O(blockSize * log n) time.
    */
  def splitLeft(n: Int): SplitDequeImpl =
    val out = new SplitDequeImpl(lgCap, blockSize, depth)
    val lim = if n >= count then count else if n > 0 then n else 0
    if lim > 0 then
      val moved = takeEnd(out, lim, true)
      if moved != lim then throw new IllegalStateException(s"splitLeft moved $moved of $lim")
    out

  /** Remove the last n elements (all of them, if we have no more than n) into
    * a new structure of the same geometry, in O(blockSize * log n) time.
    */
  def splitRight(n: Int): SplitDequeImpl =
    val out = new SplitDequeImpl(lgCap, blockSize, depth)
    val lim = if n >= count then count else if n > 0 then n else 0
    if lim > 0 then
      val moved = takeEnd(out, lim, false)
      if moved != lim then throw new IllegalStateException(s"splitRight moved $moved of $lim")
    out

  private def checkMate(donor: SplitDequeImpl): Unit =
    if donor eq this then throw new IllegalArgumentException("cannot splice a SplitDeque into itself")
    if donor.lgCap != lgCap || donor.blockSize != blockSize || donor.depth != depth then
      throw new IllegalArgumentException(s"geometry mismatch: ${donor.lgCap}/${donor.blockSize}/${donor.depth} vs $lgCap/$blockSize/$depth")


  // === Element-level API (valid at depth 0 only) ===

  def isEmpty: Boolean = count == 0

  def pushLeft(x: AnyRef): Unit = pushSlot(true)(x, 1)

  def pushRight(x: AnyRef): Unit = pushSlot(false)(x, 1)

  /** Caller must ensure nonempty. */
  def popLeft(): AnyRef = popSlot(true)

  /** Caller must ensure nonempty. */
  def popRight(): AnyRef = popSlot(false)

  /** Remove the first n elements (caller must ensure n <= count), copying them in
    * order into `target` starting at `where`, with any element eq to `sentinel`
    * replaced by null.  Block-grained: whole flank runs and whole deeper blocks are
    * copied without per-element bookkeeping (the emptied blocks are stashed); at most
    * one boundary block gets unpacked back into the flank.
    */
  def popLeftInto(target: Array[AnyRef], where: Int, n: Int, sentinel: AnyRef): Unit =
    var done = 0
    while done < n do
      if nl > 0 then
        var c = n - done
        if c > nl then c = nl
        val p0 = anchor - nl
        var i = 0
        while i < c do
          val x = Ring.at(ring, mask, p0 + i)
          Ring.set(ring, mask, p0 + i, null)
          target(where + done + i) = if x eq sentinel then null else x
          i += 1
        nl -= c
        count -= c
        done += c
      else
        val deep = deeper
        if (deep ne null) && deep.count > 0 then
          val blk = deep.popSlot(true).asInstanceOf[Array[AnyRef]]
          val w = deep.xfer
          if w <= n - done then
            var i = 0
            while i < w do
              val x = blk(i)
              blk(i) = null
              target(where + done + i) = if x eq sentinel then null else x
              i += 1
            stash(blk)
            count -= w
            done += w
          else
            if nl + nr + blockSize > mask + 1 then makeRoom(true)
            unpack(true, blk)
        else refill(true)

  /** Remove the last n elements (caller must ensure n <= count), copying them *in
    * order* (as splitRight would) into `target` starting at `where`, with any element
    * eq to `sentinel` replaced by null.  Block-grained, mirroring popLeftInto: the
    * target span fills from its far end backward.
    */
  def popRightInto(target: Array[AnyRef], where: Int, n: Int, sentinel: AnyRef): Unit =
    var done = 0
    while done < n do
      if nr > 0 then
        var c = n - done
        if c > nr then c = nr
        val p0 = anchor + nr - c
        val t0 = where + n - done - c
        var i = 0
        while i < c do
          val x = Ring.at(ring, mask, p0 + i)
          Ring.set(ring, mask, p0 + i, null)
          target(t0 + i) = if x eq sentinel then null else x
          i += 1
        nr -= c
        count -= c
        done += c
      else
        val deep = deeper
        if (deep ne null) && deep.count > 0 then
          val blk = deep.popSlot(false).asInstanceOf[Array[AnyRef]]
          val w = deep.xfer
          if w <= n - done then
            val t0 = where + n - done - w
            var i = 0
            while i < w do
              val x = blk(i)
              blk(i) = null
              target(t0 + i) = if x eq sentinel then null else x
              i += 1
            stash(blk)
            count -= w
            done += w
          else
            if nl + nr + blockSize > mask + 1 then makeRoom(false)
            unpack(false, blk)
        else refill(false)


  // === Validation and traversal (test support; correctness over speed) ===

  /** Checks every invariant of this level and everything below; null if all is
    * well, else a description of the first problem found.
    */
  def validate(): String = validateLevel()

  private def validateLevel(): String =
    val tag = "L" + depth
    if nl < 0 || nr < 0 || nl + nr > mask + 1 then return s"$tag: bad flanks nl=$nl nr=$nr cap=${mask + 1}"
    if anchor < 0 || anchor > mask then return s"$tag: anchor $anchor not masked"
    val gap = mask + 1 - nl - nr
    var k = 0
    while k < gap do
      if Ring.at(ring, mask, anchor + nr + k) ne null then return s"$tag: gap slot $k (of $gap) not null"
      if depth > 0 && Ring.at(sizes, mask, anchor + nr + k) != 0 then return s"$tag: gap size $k (of $gap) not 0"
      k += 1
    var total = 0L
    k = 0
    while k < nl + nr do
      val p = if k < nl then anchor - nl + k else anchor + (k - nl)
      val x = Ring.at(ring, mask, p)
      if x eq null then return s"$tag: live slot $k (nl=$nl nr=$nr) is null"
      if depth == 0 then total += 1
      else
        val w = Ring.at(sizes, mask, p)
        val err = validateBlock(s"$tag.$k", x.asInstanceOf[Array[AnyRef]], w, depth - 1)
        if err ne null then return err
        total += w
      k += 1
    if deeper ne null then
      if deeper.depth != depth + 1 then return s"$tag: deeper has depth ${deeper.depth}"
      if deeper.blockSize != blockSize || deeper.lgCap != lgCap then return s"$tag: deeper has mismatched geometry"
      val err = deeper.validateLevel()
      if err ne null then return err
      total += deeper.count
    if total != count then return s"$tag: count=$count but contents total $total"
    if (spare2 ne null) && (spare eq null) then return s"$tag: spare2 occupied but spare empty"
    var err = validateSpare(s"$tag spare", spare)
    if err eq null then err = validateSpare(s"$tag spare2", spare2)
    err

  // A stashed block must be indistinguishable from a freshly allocated one.
  private def validateSpare(tag: String, blk: Array[AnyRef]): String =
    if blk eq null then return null
    if depth == 0 then
      if blk.length != blockSize then return s"$tag: length ${blk.length}"
      var k = 0
      while k < blockSize do
        if blk(k) ne null then return s"$tag: not null at $k"
        k += 1
    else
      if blk.length != blockSize + 1 then return s"$tag: length ${blk.length}"
      blk(0) match
        case hdr: Array[Int] =>
          if hdr.length != blockSize then return s"$tag: header length ${hdr.length}"
          var k = 0
          while k < blockSize do
            if hdr(k) != 0 then return s"$tag: header not 0 at $k"
            if blk(k + 1) ne null then return s"$tag: not null at ${k + 1}"
            k += 1
        case _ => return s"$tag: slot 0 is not a header"
    null

  /** Fresh block allocations at all levels (test support). */
  private[kse] def totalAllocs: Int =
    var n = allocs
    if deeper ne null then n += deeper.totalAllocs
    n

  private def validateBlock(tag: String, blk: Array[AnyRef], w: Int, childDepth: Int): String =
    if childDepth == 0 then
      if blk.length != blockSize then return s"$tag: leaf block length ${blk.length}"
      if w < 1 || w > blockSize then return s"$tag: leaf block weight $w"
      var k = 0
      while k < blockSize do
        if (blk(k) eq null) != (k >= w) then return s"$tag: leaf block packing wrong at $k (w=$w)"
        k += 1
      null
    else
      if blk.length != blockSize + 1 then return s"$tag: deep block length ${blk.length}"
      blk(0) match
        case hdr: Array[Int] =>
          if hdr.length != blockSize then return s"$tag: header length ${hdr.length}"
          var m = 0
          while m < blockSize && hdr(m) != 0 do m += 1
          if m == 0 then return s"$tag: empty deep block"
          var sum = 0L
          var k = 0
          while k < blockSize do
            if k < m then
              if hdr(k) < 1 then return s"$tag: header weight ${hdr(k)} at $k"
              if blk(k + 1) eq null then return s"$tag: missing child at $k (m=$m)"
              val err = validateBlock(s"$tag.$k", blk(k + 1).asInstanceOf[Array[AnyRef]], hdr(k), childDepth - 1)
              if err ne null then return err
              sum += hdr(k)
            else
              if hdr(k) != 0 then return s"$tag: header not packed at $k (m=$m)"
              if blk(k + 1) ne null then return s"$tag: child after packed end at $k (m=$m)"
            k += 1
          if sum != w then return s"$tag: header sums to $sum but declared weight is $w"
          null
        case _ => s"$tag: deep block slot 0 is not a header"

  /** Visits every element left to right.  Test support. */
  def foreachForward(f: AnyRef => Unit): Unit =
    var k = 0
    while k < nl do
      val x = Ring.at(ring, mask, anchor - nl + k)
      if depth == 0 then f(x) else visitBlock(x.asInstanceOf[Array[AnyRef]], depth - 1, f)
      k += 1
    if deeper ne null then deeper.foreachForward(f)
    k = 0
    while k < nr do
      val x = Ring.at(ring, mask, anchor + k)
      if depth == 0 then f(x) else visitBlock(x.asInstanceOf[Array[AnyRef]], depth - 1, f)
      k += 1

  private def visitBlock(blk: Array[AnyRef], childDepth: Int, f: AnyRef => Unit): Unit =
    if childDepth == 0 then
      var k = 0
      while k < blockSize && (blk(k) ne null) do
        f(blk(k))
        k += 1
    else
      var k = 1
      while k <= blockSize && (blk(k) ne null) do
        visitBlock(blk(k).asInstanceOf[Array[AnyRef]], childDepth - 1, f)
        k += 1
}
