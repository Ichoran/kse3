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
  case Txt(x: Double, y: Double, text: String, size: Double, fill: String, anchor: Glyph.Anchor, bold: Boolean = false, rotate: Double = 0)

object Glyph:
  enum Anchor:
    case Start, Middle, End


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
      case Glyph.Txt(x, y, text, size, fill, anchor, bold, rotate) =>
        val anch = anchor match
          case Glyph.Anchor.Start  => ""
          case Glyph.Anchor.Middle => """ text-anchor="middle""""
          case Glyph.Anchor.End    => """ text-anchor="end""""
        val wt = if bold then """ font-weight="bold"""" else ""
        val rot = if rotate == 0 then "" else s""" transform="rotate(${num(rotate)} ${num(x)} ${num(y)})""""
        emit(s"""<text x="${num(x)}" y="${num(y)}" font-family="sans-serif" font-size="${num(size)}" fill="$fill"$anch$wt$rot>${esc(text)}</text>""")
        emit("\n")
    emit("</svg>\n")
    sb.toString
