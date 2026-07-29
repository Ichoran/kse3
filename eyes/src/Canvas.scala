// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.eyes


import java.lang.{Math => jm}


/** Text measurement for layout.  The default is a deterministic per-character-class
  * approximation, good enough for margins and legends; font-table-accurate metrics replace
  * it later (DESIGN 8) without changing any consumer.
  */
trait Measurer:
  def width(text: String, size: Double): Double
  def lineHeight(size: Double): Double
  def ascent(size: Double): Double

object Measurer:
  given approx: Measurer = new Measurer:
    def width(text: String, size: Double): Double =
      var w = 0.0
      var i = 0
      while i < text.length do
        val c = text.charAt(i)
        w += (
          if ".,:;!'|iIljtf ".indexOf(c) >= 0 then 0.32
          else if "mwMW@".indexOf(c) >= 0 then 0.9
          else if c >= 'A' && c <= 'Z' then 0.68
          else 0.56
        )
        i += 1
      w * size
    def lineHeight(size: Double): Double = size * 1.3
    def ascent(size: Double): Double = size * 0.78


/** Backend-independent display list — the proof-of-concept seed of the canvas IR
  * (DESIGN 9).  Coordinates are figure pixels, y-down; frames do the data-space flip.
  */
enum Glyph:
  case Segment(x1: Double, y1: Double, x2: Double, y2: Double, stroke: String, w: Double, alpha: Double = 1.0)
  case Polyline(xs: Array[Double], ys: Array[Double], stroke: String, w: Double, alpha: Double = 1.0)
  case Disc(x: Double, y: Double, r: Double, fill: String)
  case Box(x: Double, y: Double, w: Double, h: Double, fill: String)
  case Poly(xs: Array[Double], ys: Array[Double], fill: String, alpha: Double = 1.0)
  case Txt(x: Double, y: Double, text: String, size: Double, fill: String, anchor: Glyph.Anchor, bold: Boolean = false, rotate: Double = 0, halo: Boolean = false)

object Glyph:
  enum Anchor:
    case Start, Middle, End


/** Arrow outlines: the whole arrow — shaft and head — as ONE filled polygon, so a
  * translucent arrow composites once with no seam where shaft meets head, and nothing can
  * poke past the head: the shaft edges terminate exactly on the head's back edge (flat
  * head) or barb-notch edges (barbed head).  The head is always straight; a curved shaft
  * is a circular arc that begins at the head's back-junction plane, tangent to the head
  * axis there, so the tip points exactly where aimed and the bend starts only behind the
  * head.
  */
object Arrow:
  /** An arrow's outline plus the shaft's travel direction at the tail (for hanging a
    * label off the tail end).
    */
  final case class Outline(xs: Array[Double], ys: Array[Double], tailDirX: Double, tailDirY: Double)

  /** Builds an arrow from (tailX, tailY) aimed at (aimX, aimY).  `headLen`/`headHalf`
    * size the head; `barb` in [0, 0.9] pulls the back-center toward the tip leaving
    * swept-back barbs (0 = flat-backed); `shaftW` is the full shaft width; `backoff`
    * pulls the tip short of the aim point along the head axis.  `radius`, when finite and
    * nonzero, is the shaft's radius of curvature in the same units — positive bows the
    * shaft to the traveler's left (upward, for a rightward arrow in y-down coordinates),
    * negative to the right; the radius is quietly raised to the smallest reachable value
    * if the bend asked for cannot span the endpoints.
    *
    * An arrow shorter than its own head keeps its proportions and shrinks — a short edge
    * in a dense graph still draws, just small.  Null comes back only for a sub-pixel
    * arrow, where there is genuinely nothing to say.
    */
  def outline(tailX: Double, tailY: Double, aimX: Double, aimY: Double,
              headLen: Double, headHalf: Double, barb: Double, shaftW: Double,
              radius: Double = Double.NaN, backoff: Double = 0.0): Outline | Null =
    val b = jm.max(0.0, jm.min(0.9, barb))
    var dx = aimX - tailX
    var dy = aimY - tailY
    val chord = jm.sqrt(dx * dx + dy * dy)
    val avail = chord - backoff
    if !(avail > 0.75) then return null
    val shrink = jm.min(1.0, avail / (1.2 * jm.max(0.1, headLen)))
    val bigW = jm.max(0.05, headHalf * shrink)
    val bigL = jm.max(0.1, headLen * shrink)
    val s = jm.min(0.9 * bigW, jm.max(0.05, shaftW / 2))
    val axJ = (1 - b) * bigL + (s / bigW) * b * bigL
    if radius.isNaN || radius.isInfinite || radius == 0 then
      dx /= chord
      dy /= chord
      straight(tailX, tailY, aimX - dx * backoff, aimY - dy * backoff, dx, dy, bigL, bigW, b, s)
    else
      curved(tailX, tailY, aimX, aimY, bigL, bigW, b, s, axJ, radius, backoff) match
        case o: Outline => o
        case null =>
          dx /= chord
          dy /= chord
          straight(tailX, tailY, aimX - dx * backoff, aimY - dy * backoff, dx, dy, bigL, bigW, b, s)

  /** The points of a circular arc from (x1, y1) to (x2, y2) with the given radius of
    * curvature (positive bows to the traveler's left), for headless connectors; null when
    * the endpoints are too close to bend between.
    */
  def arc(x1: Double, y1: Double, x2: Double, y2: Double, radius: Double): (Array[Double], Array[Double]) | Null =
    val chord = jm.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
    if !(chord > 0.75) then return null
    val sigma = if radius > 0 then 1.0 else -1.0
    val r = jm.max(jm.abs(radius), 0.51 * chord)
    val mx = (x1 + x2) / 2
    val my = (y1 + y2) / 2
    val ux = (x2 - x1) / chord
    val uy = (y2 - y1) / chord
    val sag = jm.sqrt(jm.max(0.0, r * r - chord * chord / 4))
    val cx = mx + sigma * sag * uy
    val cy = my - sigma * sag * ux
    val p1 = jm.atan2(y1 - cy, x1 - cx)
    val p2 = jm.atan2(y2 - cy, x2 - cx)
    var delta = p2 - p1
    while delta > jm.PI do delta -= 2 * jm.PI
    while delta < -jm.PI do delta += 2 * jm.PI
    val n = jm.max(2, jm.min(64, jm.ceil(jm.abs(delta) / (2 * jm.sqrt(0.3 / r))).toInt))
    val xs = new Array[Double](n + 1)
    val ys = new Array[Double](n + 1)
    var k = 0
    while k <= n do
      val phi = p1 + delta * k / n
      xs(k) = cx + r * jm.cos(phi)
      ys(k) = cy + r * jm.sin(phi)
      k += 1
    (xs, ys)

  private def straight(px: Double, py: Double, tx: Double, ty: Double, dx: Double, dy: Double,
                       bigL: Double, bigW: Double, b: Double, s: Double): Outline =
    val ppx = -dy
    val ppy = dx
    val mX = tx - (1 - b) * bigL * dx
    val mY = ty - (1 - b) * bigL * dy
    val bpX = tx - bigL * dx + bigW * ppx
    val bpY = ty - bigL * dy + bigW * ppy
    val bmX = tx - bigL * dx - bigW * ppx
    val bmY = ty - bigL * dy - bigW * ppy
    val f = s / bigW
    val jpX = mX + f * (bpX - mX)
    val jpY = mY + f * (bpY - mY)
    val jmX = mX + f * (bmX - mX)
    val jmY = mY + f * (bmY - mY)
    val xs = Array(px + s * ppx, jpX, bpX, tx, bmX, jmX, px - s * ppx)
    val ys = Array(py + s * ppy, jpY, bpY, ty, bmY, jmY, py - s * ppy)
    Outline(xs, ys, dx, dy)

  /** Solves the head-axis angle so an arc of radius `r` starting tangent at the head's
    * back junction passes through the tail; NaN if no bend in scan range works.
    */
  private def headAngle(px: Double, py: Double, tx: Double, ty: Double, axJ: Double, sigma: Double, r: Double): Double =
    val th0 = jm.atan2(ty - py, tx - px)
    def f(th: Double): Double =
      val dX = jm.cos(th)
      val dY = jm.sin(th)
      val cx = tx - axJ * dX - sigma * r * dY
      val cy = ty - axJ * dY + sigma * r * dX
      val ex = cx - px
      val ey = cy - py
      ex * ex + ey * ey - r * r
    val n = 64
    val lo0 = th0 - 1.55
    var bl = 0.0
    var bh = 0.0
    var bestDist = Double.MaxValue
    var prevTh = lo0
    var prevF = f(lo0)
    var k = 1
    while k <= n do
      val th = lo0 + 3.1 * k / n
      val v = f(th)
      if v * prevF < 0 then
        val dd = jm.abs((prevTh + th) / 2 - th0)
        if dd < bestDist then
          bestDist = dd
          bl = prevTh
          bh = th
      prevTh = th
      prevF = v
      k += 1
    if bestDist == Double.MaxValue then Double.NaN
    else
      var a0 = bl
      var b0 = bh
      var fa = f(a0)
      var it = 0
      while it < 48 do
        val mth = (a0 + b0) / 2
        val fm = f(mth)
        if fa * fm <= 0 then b0 = mth
        else
          a0 = mth
          fa = fm
        it += 1
      (a0 + b0) / 2

  private def curved(px: Double, py: Double, aimX: Double, aimY: Double,
                     bigL: Double, bigW: Double, b: Double, s: Double, axJ: Double,
                     radius: Double, backoff: Double): Outline | Null =
    val sigma = if radius > 0 then 1.0 else -1.0
    val chord0 = jm.sqrt((aimX - px) * (aimX - px) + (aimY - py) * (aimY - py))
    if chord0 <= backoff + axJ + 2 then return null
    val r = jm.max(jm.abs(radius), 0.51 * chord0)
    var tx = aimX
    var ty = aimY
    var th = headAngle(px, py, tx, ty, axJ, sigma, r)
    if th.isNaN then return null
    if backoff > 0 then
      tx = aimX - backoff * jm.cos(th)
      ty = aimY - backoff * jm.sin(th)
      val th2 = headAngle(px, py, tx, ty, axJ, sigma, r)
      if !th2.isNaN then th = th2
    val dX = jm.cos(th)
    val dY = jm.sin(th)
    val ppx = -dY
    val ppy = dX
    val sX = tx - axJ * dX
    val sY = ty - axJ * dY
    val cx = sX + sigma * r * ppx
    val cy = sY + sigma * r * ppy
    val phiP = jm.atan2(py - cy, px - cx)
    val phiS = jm.atan2(sY - cy, sX - cx)
    // travel at the junction must be +d; the ccw tangent there decides the walk direction
    val ccwT = -jm.sin(phiS) * dX + jm.cos(phiS) * dY
    val tau = if ccwT >= 0 then 1.0 else -1.0
    var delta = phiS - phiP
    if tau > 0 then
      while delta < 0 do delta += 2 * jm.PI
      while delta > 2 * jm.PI do delta -= 2 * jm.PI
    else
      while delta > 0 do delta -= 2 * jm.PI
      while delta < -2 * jm.PI do delta += 2 * jm.PI
    if jm.abs(delta) * r < 2 then return null
    val nArc = jm.max(2, jm.min(96, jm.ceil(jm.abs(delta) / (2 * jm.sqrt(0.3 / r))).toInt))
    val mX = tx - (1 - b) * bigL * dX
    val mY = ty - (1 - b) * bigL * dY
    val bpX = tx - bigL * dX + bigW * ppx
    val bpY = ty - bigL * dY + bigW * ppy
    val bmX = tx - bigL * dX - bigW * ppx
    val bmY = ty - bigL * dY - bigW * ppy
    val f = s / bigW
    val total = 2 * nArc + 5
    val xs = new Array[Double](total)
    val ys = new Array[Double](total)
    var k = 0
    while k < nArc do
      val phi = phiP + delta * k / nArc
      val rx = jm.cos(phi)
      val ry = jm.sin(phi)
      xs(k) = cx + r * rx - sigma * s * rx
      ys(k) = cy + r * ry - sigma * s * ry
      xs(total - 1 - k) = cx + r * rx + sigma * s * rx
      ys(total - 1 - k) = cy + r * ry + sigma * s * ry
      k += 1
    xs(nArc) = mX + f * (bpX - mX)
    ys(nArc) = mY + f * (bpY - mY)
    xs(nArc + 1) = bpX
    ys(nArc + 1) = bpY
    xs(nArc + 2) = tx
    ys(nArc + 2) = ty
    xs(nArc + 3) = bmX
    ys(nArc + 3) = bmY
    xs(nArc + 4) = mX + f * (bmX - mX)
    ys(nArc + 4) = mY + f * (bmY - mY)
    val tdx = tau * -jm.sin(phiP)
    val tdy = tau * jm.cos(phiP)
    Outline(xs, ys, tdx, tdy)


/** A coarse raster of how occupied each patch of a rectangle is, built from the geometry
  * actually drawn there — the measured-output feedback that placing annotations in
  * relatively clear space needs, without rendering to pixels.  Marks carry weights in
  * [0, 1] (a solid disc matters more than a pale band) and combine by max; loads are the
  * mean weight under the queried shape.  Deterministic, cheap (cells are `cell` px square),
  * and out-of-bounds marks are simply ignored.
  */
final class Occupancy(val x0: Double, val y0: Double, val w: Double, val h: Double, val cell: Double):
  val nx: Int = jm.max(1, jm.ceil(w / cell).toInt)
  val ny: Int = jm.max(1, jm.ceil(h / cell).toInt)
  private val a = new Array[Double](nx * ny)

  private def xi(x: Double): Int = jm.floor((x - x0) / cell).toInt
  private def yi(y: Double): Int = jm.floor((y - y0) / cell).toInt

  private def bump(ix: Int, iy: Int, wt: Double): Unit =
    if ix >= 0 && ix < nx && iy >= 0 && iy < ny then
      val k = iy * nx + ix
      if wt > a(k) then a(k) = wt

  def markDisc(x: Double, y: Double, r: Double, wt: Double = 1.0): Unit =
    val reach = (r + cell * 0.5) * (r + cell * 0.5)
    var iy = yi(y - r)
    val iy1 = yi(y + r)
    while iy <= iy1 do
      var ix = xi(x - r)
      val ix1 = xi(x + r)
      while ix <= ix1 do
        val cx = x0 + (ix + 0.5) * cell - x
        val cy = y0 + (iy + 0.5) * cell - y
        if cx * cx + cy * cy <= reach then bump(ix, iy, wt)
        ix += 1
      iy += 1

  def markSegment(x1: Double, y1: Double, x2: Double, y2: Double, halfWidth: Double, wt: Double = 1.0): Unit =
    val len = jm.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
    val n = jm.max(1, jm.ceil(len / (cell * 0.5)).toInt)
    var k = 0
    while k <= n do
      val t = k.toDouble / n
      val px = x1 + (x2 - x1) * t
      val py = y1 + (y2 - y1) * t
      if halfWidth > cell * 0.5 then markDisc(px, py, halfWidth, wt)
      else bump(xi(px), yi(py), wt)
      k += 1

  def markBox(x: Double, y: Double, bw: Double, bh: Double, wt: Double = 1.0): Unit =
    var iy = yi(y)
    val iy1 = yi(y + bh)
    while iy <= iy1 do
      var ix = xi(x)
      val ix1 = xi(x + bw)
      while ix <= ix1 do
        bump(ix, iy, wt)
        ix += 1
      iy += 1

  /** Marks the vertical run of cells at `x` spanning `yA..yB` (either order). */
  def markColumn(x: Double, yA: Double, yB: Double, wt: Double = 1.0): Unit =
    val ix = xi(x)
    var iy = yi(jm.min(yA, yB))
    val iy1 = yi(jm.max(yA, yB))
    while iy <= iy1 do
      bump(ix, iy, wt)
      iy += 1

  /** Mean occupancy under a box; 0 if no cells fall inside the raster. */
  def boxLoad(x: Double, y: Double, bw: Double, bh: Double): Double =
    var s = 0.0
    var n = 0
    var iy = jm.max(0, yi(y))
    val iy1 = jm.min(ny - 1, yi(y + bh))
    while iy <= iy1 do
      var ix = jm.max(0, xi(x))
      val ix1 = jm.min(nx - 1, xi(x + bw))
      while ix <= ix1 do
        s += a(iy * nx + ix)
        n += 1
        ix += 1
      iy += 1
    if n == 0 then 0.0 else s / n

  /** Mean occupancy sampled along a segment; 0 if every sample misses the raster. */
  def lineLoad(x1: Double, y1: Double, x2: Double, y2: Double): Double =
    val len = jm.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
    val n = jm.max(1, jm.ceil(len / (cell * 0.5)).toInt)
    var s = 0.0
    var m = 0
    var k = 0
    while k <= n do
      val t = k.toDouble / n
      val ix = xi(x1 + (x2 - x1) * t)
      val iy = yi(y1 + (y2 - y1) * t)
      if ix >= 0 && ix < nx && iy >= 0 && iy < ny then
        s += a(iy * nx + ix)
        m += 1
      k += 1
    if m == 0 then 0.0 else s / m


/** Plain-text SVG emission from a display list.  No dependencies, deterministic output. */
object Svg:
  def num(d: Double): String =
    val r = jm.rint(d * 100) / 100
    if r == jm.rint(r) && jm.abs(r) < 1e15 then r.toLong.toString else r.toString

  def esc(s: String): String =
    if s.indexOf('&') < 0 && s.indexOf('<') < 0 && s.indexOf('>') < 0 then s
    else s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  def render(width: Double, height: Double, glyphs: List[Glyph]): String =
    val sb = new java.lang.StringBuilder
    def emit(s: String): Unit = { sb.append(s); () }
    emit(s"""<svg xmlns="http://www.w3.org/2000/svg" width="${num(width)}" height="${num(height)}" viewBox="0 0 ${num(width)} ${num(height)}">""")
    emit("\n")
    emit(s"""<rect x="0" y="0" width="${num(width)}" height="${num(height)}" fill="#FFFFFF"/>""")
    emit("\n")
    def pts(xs: Array[Double], ys: Array[Double]): String =
      val b = new java.lang.StringBuilder
      var i = 0
      while i < xs.length do
        if i > 0 then { b.append(' '); () }
        val _ = b.append(num(xs(i))).append(',').append(num(ys(i)))
        i += 1
      b.toString
    glyphs.foreach:
      case Glyph.Segment(x1, y1, x2, y2, stroke, w, alpha) =>
        val op = if alpha >= 1 then "" else s""" stroke-opacity="${num(alpha)}""""
        emit(s"""<line x1="${num(x1)}" y1="${num(y1)}" x2="${num(x2)}" y2="${num(y2)}" stroke="$stroke" stroke-width="${num(w)}"$op/>""")
        emit("\n")
      case Glyph.Polyline(xs, ys, stroke, w, alpha) =>
        val op = if alpha >= 1 then "" else s""" stroke-opacity="${num(alpha)}""""
        emit(s"""<polyline points="${pts(xs, ys)}" fill="none" stroke="$stroke" stroke-width="${num(w)}" stroke-linejoin="round" stroke-linecap="round"$op/>""")
        emit("\n")
      case Glyph.Poly(xs, ys, fill, alpha) =>
        val op = if alpha >= 1 then "" else s""" fill-opacity="${num(alpha)}""""
        emit(s"""<polygon points="${pts(xs, ys)}" fill="$fill"$op/>""")
        emit("\n")
      case Glyph.Disc(x, y, r, fill) =>
        emit(s"""<circle cx="${num(x)}" cy="${num(y)}" r="${num(r)}" fill="$fill"/>""")
        emit("\n")
      case Glyph.Box(x, y, w, h, fill) =>
        emit(s"""<rect x="${num(x)}" y="${num(y)}" width="${num(w)}" height="${num(h)}" fill="$fill"/>""")
        emit("\n")
      case Glyph.Txt(x, y, text, size, fill, anchor, bold, rotate, halo) =>
        val anch = anchor match
          case Glyph.Anchor.Start  => ""
          case Glyph.Anchor.Middle => """ text-anchor="middle""""
          case Glyph.Anchor.End    => """ text-anchor="end""""
        val wt = if bold then """ font-weight="bold"""" else ""
        val rot = if rotate == 0 then "" else s""" transform="rotate(${num(rotate)} ${num(x)} ${num(y)})""""
        val hal = if halo then s""" stroke="#FFFFFF" stroke-width="${num(size * 0.28)}" stroke-linejoin="round" paint-order="stroke"""" else ""
        emit(s"""<text x="${num(x)}" y="${num(y)}" font-family="sans-serif" font-size="${num(size)}" fill="$fill"$anch$wt$rot$hal>${esc(text)}</text>""")
        emit("\n")
    emit("</svg>\n")
    sb.toString
