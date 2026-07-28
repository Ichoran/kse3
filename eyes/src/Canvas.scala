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
  case Segment(x1: Double, y1: Double, x2: Double, y2: Double, stroke: String, w: Double)
  case Polyline(xs: Array[Double], ys: Array[Double], stroke: String, w: Double)
  case Disc(x: Double, y: Double, r: Double, fill: String)
  case Box(x: Double, y: Double, w: Double, h: Double, fill: String)
  case Poly(xs: Array[Double], ys: Array[Double], fill: String, alpha: Double = 1.0)
  case Txt(x: Double, y: Double, text: String, size: Double, fill: String, anchor: Glyph.Anchor, bold: Boolean = false, rotate: Double = 0, halo: Boolean = false)

object Glyph:
  enum Anchor:
    case Start, Middle, End


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
      case Glyph.Segment(x1, y1, x2, y2, stroke, w) =>
        emit(s"""<line x1="${num(x1)}" y1="${num(y1)}" x2="${num(x2)}" y2="${num(y2)}" stroke="$stroke" stroke-width="${num(w)}"/>""")
        emit("\n")
      case Glyph.Polyline(xs, ys, stroke, w) =>
        emit(s"""<polyline points="${pts(xs, ys)}" fill="none" stroke="$stroke" stroke-width="${num(w)}" stroke-linejoin="round" stroke-linecap="round"/>""")
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
