// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.twodee


import java.lang.{Math => jm}


/** An axis-aligned rectangle in figure coordinates (y down). */
final case class Rect(x: Double, y: Double, w: Double, h: Double):
  def right: Double = x + w
  def bottom: Double = y + h


/** Per-side decoration extents that live in the gutters around a block's content rect. */
final case class Prot(left: Double, right: Double, top: Double, bottom: Double)

object Prot:
  val zero: Prot = Prot(0, 0, 0, 0)


/** Column/row size preference.  Fixed wins over Relative wins over Auto when blocks in one
  * column disagree; Auto columns share whatever space remains.
  */
enum Size:
  case Auto
  case Fixed(px: Double)
  case Relative(fraction: Double)


/** A layout participant.  The grid aligns *content* rects; decorations (tick labels, axis
  * titles, colorbars) are declared as protrusions and live in the gutters — that is what
  * makes multi-panel figures align by default.
  *
  * `protrusions` must be pure, cheap, and deterministic: the solver calls it more than once
  * while iterating.  A block must render sensibly at whatever final content size it is
  * given (degrading gracefully — fewer ticks, smaller markers, nonlinear point scaling —
  * when small); the solver guarantees gutters at least as large as the last measured
  * protrusions except in `cramped` layouts, where decorations were clamped to keep a
  * usable share of the figure for content.
  */
trait Block:
  def widthPref: Size = Size.Auto
  def heightPref: Size = Size.Auto
  /** Content aspect ratio (w/h), enforced by shrinking within the cell; NaN = free. */
  def aspect: Double = Double.NaN
  def protrusions(w: Double, h: Double): Prot


/** A block that can render itself into its solved content rect. */
trait GlyphBlock extends Block:
  def glyphs(rect: Rect, put: Glyph => Unit): Unit


/** A protrusion-aligning grid of blocks; itself a Block, so grids nest.  A nested grid
  * REPORTS its outer decoration needs as protrusions — its own pad plus whatever its edge
  * cells protrude — so a parent grid aligns nested *content*, not bounding boxes: a cell
  * holding a grid is granted the grid's content footprint (internal gutters included),
  * and the grid's outer decorations live in the parent's gutters like any other block's.
  * Floats are the exception: a floated grid is a self-contained overlay, everything
  * inside its box.
  *
  * Solving is estimate-verify-retry: measure protrusions at estimated content sizes,
  * allocate gutters with a safety `margin` of headroom, distribute the remaining space,
  * then re-measure at the solved sizes.  If everything still fits inside the headroom —
  * the common case — one allocation was enough; otherwise re-allocate from the fresh
  * measurements and try again, up to `maxPasses`.  If decorations would eat the figure,
  * gutters are scaled back so content keeps at least a quarter of each dimension and the
  * layout is flagged `cramped` (blocks then degrade to fit).  Deterministic throughout.
  */
final class Grid(val rows: Int, val cols: Int, val colGap: Double = 8.0, val rowGap: Double = 8.0, val pad: Double = 6.0) extends Block:
  import Grid._

  private val entries = collection.mutable.ArrayBuffer.empty[Entry]
  private val floats = collection.mutable.ArrayBuffer.empty[Floater]

  def put(r: Int, c: Int)(b: Block): this.type = put(r, r, c, c)(b)

  def put(r0: Int, r1: Int, c0: Int, c1: Int)(b: Block): this.type =
    if r0 < 0 || r1 < r0 || r1 >= rows || c0 < 0 || c1 < c0 || c1 >= cols then
      throw new IllegalArgumentException(s"span ($r0..$r1, $c0..$c1) does not fit a ${rows}x$cols grid")
    val _ = entries.addOne(Entry(b, r0, r1, c0, c1))
    this

  /** Anchors a floating block (e.g. an inset) to a host block's final content rect, in
    * host-relative coordinates.  The host must already have been `put`.
    */
  def putFloat(host: Block)(rx: Double, ry: Double, rw: Double, rh: Double)(b: Block): this.type =
    val hi = entries.indexWhere(_.block eq host)
    if hi < 0 then throw new IllegalArgumentException("float host must already be in the grid")
    val _ = floats.addOne(Floater(b, hi, rx, ry, rw, rh))
    this

  /** What this grid needs outside a content footprint of (w, h): its own pad plus the
    * protrusions of its edge cells, measured by the same estimate loop the solver uses.
    */
  def protrusions(w: Double, h: Double): Prot =
    val s = settle(w, h, 0.08, 4, footprint = true)
    Prot(pad + s.cNeed(0), pad + s.cNeed(cols), pad + s.rNeed(0), pad + s.rNeed(rows))

  private[twodee] def blockCount: Int = entries.length
  private[twodee] def blockAt(i: Int): Block = entries(i).block
  private[twodee] def floatCount: Int = floats.length
  private[twodee] def floatBlockAt(i: Int): Block = floats(i).block

  /** The content-footprint width this grid naturally wants at roughly height `h`: when
    * every column is Fixed, the columns plus the internal gutters they need — so a parent
    * can size a cell to fit and keep neighboring annotations glued to the content.  NaN
    * when any column is Auto or Relative: width is then contextual, not natural.
    */
  private[twodee] def naturalWidth(h: Double): Double =
    val colW = new Array[Double](cols)
    var c = 0
    while c < cols do
      prefFor(true, c) match
        case Size.Fixed(px) => colW(c) = px
        case _              => return Double.NaN
      c += 1
    val rowH = Array.fill(rows)(jm.max(10.0, (h - 2 * pad - rowGap * (rows - 1)) / rows))
    val startL = new Array[Double](cols + 1)
    val endR = new Array[Double](cols + 1)
    entries.foreach: e =>
      val p = e.block.protrusions(sumOf(colW, e.c0, e.c1), sumOf(rowH, e.r0, e.r1))
      if p.left > startL(e.c0) then startL(e.c0) = p.left
      if p.right > endR(e.c1 + 1) then endR(e.c1 + 1) = p.right
    var total = 0.0
    c = 0
    while c < cols do
      total += colW(c)
      if c > 0 then total += colGap + (endR(c) + startL(c)) * 1.08
      c += 1
    total

  def solve(w: Double, h: Double, margin: Double = 0.08, maxPasses: Int = 4): Layout =
    solveAt(0.0, 0.0, w, h, margin, maxPasses)

  /** The estimate-verify-retry loop, shared by `solveAt` and `protrusions`.  In
    * `footprint` mode (w, h) is the content span alone: outer gutters cost nothing here
    * (they live outside, reported as protrusions) while `cNeed`/`rNeed` still carry the
    * raw measured needs at every boundary, outer ones included.
    */
  private def settle(w: Double, h: Double, margin: Double, maxPasses: Int, footprint: Boolean): Settled =
    val n = entries.length

    val colPref = Array.tabulate(cols)(c => prefFor(true, c))
    val rowPref = Array.tabulate(rows)(r => prefFor(false, r))

    // gutter base spacing: outer boundaries get pad, internal ones the gap
    def baseC(j: Int): Double =
      if j == 0 || j == cols then (if footprint then 0.0 else pad) else colGap
    def baseR(j: Int): Double =
      if j == 0 || j == rows then (if footprint then 0.0 else pad) else rowGap

    // in footprint mode the outer boundaries are the parent's problem: zero them in the
    // budgeted copies so allocation and the convergence check see internal needs only
    def trimC(a: Array[Double]): Array[Double] =
      if footprint then
        a(0) = 0.0
        a(cols) = 0.0
      a
    def trimR(a: Array[Double]): Array[Double] =
      if footprint then
        a(0) = 0.0
        a(rows) = 0.0
      a

    // raw decoration needs per boundary: adjacent protrusions stack (an axis ending at a
    // boundary and one starting there both get their space)
    def needs(cw: Array[Double], rh: Array[Double]): (Array[Double], Array[Double]) =
      val endR = new Array[Double](cols + 1)
      val startL = new Array[Double](cols + 1)
      val endB = new Array[Double](rows + 1)
      val startT = new Array[Double](rows + 1)
      var i = 0
      while i < n do
        val e = entries(i)
        val p = e.block.protrusions(sumOf(cw, e.c0, e.c1), sumOf(rh, e.r0, e.r1))
        if p.left > startL(e.c0) then startL(e.c0) = p.left
        if p.right > endR(e.c1 + 1) then endR(e.c1 + 1) = p.right
        if p.top > startT(e.r0) then startT(e.r0) = p.top
        if p.bottom > endB(e.r1 + 1) then endB(e.r1 + 1) = p.bottom
        i += 1
      (Array.tabulate(cols + 1)(j => endR(j) + startL(j)), Array.tabulate(rows + 1)(j => endB(j) + startT(j)))

    def alloc(need: Array[Double], base: Int => Double): Array[Double] =
      Array.tabulate(need.length)(j => base(j) + need(j) * (1.0 + margin))

    var cramped = false

    // distributes total across gutters (already allocated) and tracks: Fixed, Relative,
    // then Auto shares what remains; clamps rather than overflows, flagging cramped
    def distribute(total: Double, gut: Array[Double], prefs: Array[Size]): Array[Double] =
      var gsum = 0.0
      var j = 0
      while j < gut.length do
        gsum += gut(j)
        j += 1
      val minContent = 0.25 * total
      if total - gsum < minContent && gsum > 0 then
        val scale = jm.max(0.0, (total - minContent) / gsum)
        j = 0
        while j < gut.length do
          gut(j) *= scale
          j += 1
        gsum = total - minContent
        cramped = true
      val avail = jm.max(0.0, total - gsum)
      var fixedSum = 0.0
      var relSum = 0.0
      var nAuto = 0
      prefs.foreach:
        case Size.Fixed(px)     => fixedSum += px
        case Size.Relative(f)   => relSum += f * avail
        case Size.Auto          => nAuto += 1
      var squeeze = 1.0
      if fixedSum + relSum > avail && fixedSum + relSum > 0 then
        squeeze = avail / (fixedSum + relSum)
        cramped = true
      val autoShare = if nAuto > 0 then jm.max(0.0, avail - (fixedSum + relSum) * squeeze) / nAuto else 0.0
      prefs.map:
        case Size.Fixed(px)   => px * squeeze
        case Size.Relative(f) => f * avail * squeeze
        case Size.Auto        => autoShare

    // initial content-size estimate: distribute with base spacing only
    var colW = distribute(w, Array.tabulate(cols + 1)(baseC), colPref)
    var rowH = distribute(h, Array.tabulate(rows + 1)(baseR), rowPref)
    var (cFull, rFull) = needs(colW, rowH)
    var cNeed = trimC(cFull.clone)
    var rNeed = trimR(rFull.clone)
    var colGut = alloc(cNeed, baseC)
    var rowGut = alloc(rNeed, baseR)

    var passes = 0
    var converged = false
    while passes < maxPasses && !converged do
      passes += 1
      colGut = alloc(cNeed, baseC)
      rowGut = alloc(rNeed, baseR)
      colW = distribute(w, colGut, colPref)
      rowH = distribute(h, rowGut, rowPref)
      val (c2f, r2f) = needs(colW, rowH)
      cFull = c2f
      rFull = r2f
      val c2 = trimC(c2f.clone)
      val r2 = trimR(r2f.clone)
      if fits(c2, cNeed, margin) && fits(r2, rNeed, margin) then converged = true
      else
        cNeed = c2
        rNeed = r2
    if !converged then
      // final honest allocation from the freshest measurements, verification foregone
      colGut = alloc(cNeed, baseC)
      rowGut = alloc(rNeed, baseR)
      colW = distribute(w, colGut, colPref)
      rowH = distribute(h, rowGut, rowPref)
    Settled(colW, rowH, colGut, rowGut, cFull, rFull, passes, converged, cramped)

  private[twodee] def solveAt(x0: Double, y0: Double, w: Double, h: Double, margin: Double, maxPasses: Int, footprint: Boolean = false): Layout =
    val n = entries.length
    val s = settle(w, h, margin, maxPasses, footprint)
    val colW = s.colW
    val rowH = s.rowH
    val colGut = s.colGut
    val rowGut = s.rowGut
    val passes = s.passes
    val converged = s.converged
    val cramped = s.cramped

    // geometry: accumulate positions; spans absorb their internal gutters.  When every
    // track is Fixed/Relative and space is left over, the whole assembly centers in it —
    // content should never end up huddled left of a blank it was allocated
    inline def slackOf(total: Double, gut: Array[Double], trk: Array[Double]): Double =
      var used = 0.0
      var j = 0
      while j < gut.length do
        used += gut(j)
        if j < trk.length then used += trk(j)
        j += 1
      jm.max(0.0, total - used) / 2
    val colX = new Array[Double](cols + 1)
    var acc = x0 + slackOf(w, colGut, colW) + colGut(0)
    var c = 0
    while c < cols do
      colX(c) = acc
      acc += colW(c) + colGut(c + 1)
      c += 1
    colX(cols) = acc
    val rowY = new Array[Double](rows + 1)
    acc = y0 + slackOf(h, rowGut, rowH) + rowGut(0)
    var r = 0
    while r < rows do
      rowY(r) = acc
      acc += rowH(r) + rowGut(r + 1)
      r += 1
    rowY(rows) = acc

    val content = new Array[Rect](n)
    val sub = Array.fill[Layout | Null](n)(null)
    var i = 0
    while i < n do
      val e = entries(i)
      var rx = colX(e.c0)
      var ry = rowY(e.r0)
      var rw = (colX(e.c1) + colW(e.c1)) - rx
      var rh = (rowY(e.r1) + rowH(e.r1)) - ry
      val a = e.block.aspect
      if !a.isNaN && rw > 0 && rh > 0 then
        val w2 = jm.min(rw, rh * a)
        val h2 = w2 / a
        rx += (rw - w2) / 2
        ry += (rh - h2) / 2
        rw = w2
        rh = h2
      content(i) = Rect(rx, ry, rw, rh)
      i += 1
    i = 0
    while i < n do
      entries(i).block match
        case g: Grid =>
          // the cell is the nested grid's content footprint; its outer decorations were
          // reserved in this grid's gutters via the protrusions it reported
          val rct = content(i)
          sub(i) = g.solveAt(rct.x, rct.y, rct.w, rct.h, margin, maxPasses, footprint = true)
        case _ => ()
      i += 1

    val floatRects = new Array[Rect](floats.length)
    val floatSub = Array.fill[Layout | Null](floats.length)(null)
    i = 0
    while i < floats.length do
      val f = floats(i)
      val hr = content(f.host)
      floatRects(i) = Rect(hr.x + f.rx * hr.w, hr.y + f.ry * hr.h, f.rw * hr.w, f.rh * hr.h)
      f.block match
        case g: Grid =>
          val fr = floatRects(i)
          floatSub(i) = g.solveAt(fr.x, fr.y, fr.w, fr.h, margin, maxPasses)
        case _ => ()
      i += 1

    Layout(content, floatRects, colW, rowH, colGut, rowGut, sub, floatSub, passes, converged, cramped)

  private def prefFor(isCol: Boolean, k: Int): Size =
    // strongest requirement among single-track blocks; spanning blocks don't constrain
    var fixed = Double.NaN
    var rel = Double.NaN
    entries.foreach: e =>
      val single = if isCol then e.c0 == k && e.c1 == k else e.r0 == k && e.r1 == k
      if single then
        (if isCol then e.block.widthPref else e.block.heightPref) match
          case Size.Fixed(px)   => fixed = if fixed.isNaN then px else jm.max(fixed, px)
          case Size.Relative(f) => rel = if rel.isNaN then f else jm.max(rel, f)
          case Size.Auto        => ()
    if !fixed.isNaN then Size.Fixed(fixed)
    else if !rel.isNaN then Size.Relative(rel)
    else Size.Auto

object Grid:
  private final case class Entry(block: Block, r0: Int, r1: Int, c0: Int, c1: Int)
  private final case class Floater(block: Block, host: Int, rx: Double, ry: Double, rw: Double, rh: Double)

  /** The settled track sizes and gutters; `cNeed`/`rNeed` are the raw measured needs at
    * every boundary (outer entries preserved even when the budgeted gutters were zeroed
    * in footprint mode, so `protrusions` can report them).
    */
  private final case class Settled(
    colW: Array[Double], rowH: Array[Double],
    colGut: Array[Double], rowGut: Array[Double],
    cNeed: Array[Double], rNeed: Array[Double],
    passes: Int, converged: Boolean, cramped: Boolean
  )

  private def sumOf(a: Array[Double], i0: Int, i1: Int): Double =
    var s = 0.0
    var i = i0
    while i <= i1 do
      s += a(i)
      i += 1
    s

  private def fits(fresh: Array[Double], allocatedFrom: Array[Double], margin: Double): Boolean =
    var ok = true
    var j = 0
    while ok && j < fresh.length do
      if fresh(j) > allocatedFrom(j) * (1.0 + margin) + 1e-9 then ok = false
      j += 1
    ok

  /** A solved grid.  `content` follows `put` order; `sub` and `floatSub` hold layouts of
    * nested grids (null elsewhere).  `converged` false means the pass cap was hit and the
    * last allocation went unverified; `cramped` means decorations were clamped to preserve
    * a usable content share.
    */
  final case class Layout(
    content: Array[Rect],
    floatRects: Array[Rect],
    colWidths: Array[Double],
    rowHeights: Array[Double],
    colGutters: Array[Double],
    rowGutters: Array[Double],
    sub: Array[Layout | Null],
    floatSub: Array[Layout | Null],
    passes: Int,
    converged: Boolean,
    cramped: Boolean
  )
