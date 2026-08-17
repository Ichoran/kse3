// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.twodee


import java.lang.{Math => jm}

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.twodee.*


class Steady(l: Double, r: Double = 0, t: Double = 0, b: Double = 0, wp: Size = Size.Auto, hp: Size = Size.Auto, asp: Double = Double.NaN) extends Block {
  override def widthPref = wp
  override def heightPref = hp
  override def aspect = asp
  def protrusions(w: Double, h: Double): Prot = Prot(l, r, t, b)
}

/** Adversarial: decorations grow as content shrinks (like tick labels crowding a small
  * axis), so the first allocation undershoots and the solver must take a second pass.
  */
class Nasty(base: Double, slope: Double) extends Block {
  def protrusions(w: Double, h: Double): Prot = Prot(jm.max(0.0, base - slope * w), 0, 0, 0)
}


@RunWith(classOf[JUnit4])
class GridTest {
  import kse.basics.testutilities.TestUtilities.{given, _}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  def close(a: Double, b: Double, tol: Double = 1e-6): Boolean = jm.abs(a - b) < tol

  @Test
  def singleCellTest(): Unit =
    val g = Grid(1, 1)
    val blk = Steady(30, 10, 5, 20)
    val lay = (g.put(0, 0)(blk)).solve(640, 480)
    T ~ lay.passes ==== 1
    T ~ lay.converged ==== true
    T ~ lay.cramped ==== false
    val rct = lay.content(0)
    // gutters = pad + need*(1+margin); content fills what remains
    T ~ close(lay.colGutters(0), 6 + 30 * 1.08) ==== true
    T ~ close(lay.colGutters(1), 6 + 10 * 1.08) ==== true
    T ~ close(rct.x, lay.colGutters(0)) ==== true
    T ~ close(rct.right + lay.colGutters(1), 640) ==== true
    T ~ close(rct.y, lay.rowGutters(0)) ==== true
    T ~ close(rct.bottom + lay.rowGutters(1), 480) ==== true

  @Test
  def iterationTest(): Unit =
    // needs a second pass: the margin cannot absorb decorations that grow this fast
    val g = Grid(1, 1)
    val lay = (g.put(0, 0)(Nasty(100, 0.1))).solve(640, 480)
    T ~ lay.passes ==== 2
    T ~ lay.converged ==== true
    // final gutter really does cover the freshly measured need
    val need = 100 - 0.1 * lay.content(0).w
    T ~ (lay.colGutters(0) >= 6 + need) ==== true

  @Test
  def alignmentTest(): Unit =
    // the protrusion property: content rects in one column share x even when their
    // decorations differ; the widest decoration sets the shared gutter
    val g = Grid(2, 2)
    val lay = (g
      .put(0, 0)(Steady(50))
      .put(1, 0)(Steady(20))
      .put(0, 1)(Steady(15, r = 25))
      .put(1, 1)(Steady(5))
    ).solve(800, 600)
    T ~ close(lay.content(0).x, lay.content(1).x) ==== true
    T ~ close(lay.content(2).x, lay.content(3).x) ==== true
    T ~ close(lay.content(0).w, lay.content(1).w) ==== true
    T ~ close(lay.colGutters(0), 6 + 50 * 1.08) ==== true
    // internal boundary stacks the right protrusion of column 0 (none) with the left of column 1
    T ~ close(lay.colGutters(1), 8 + 15 * 1.08) ==== true
    T ~ close(lay.colGutters(2), 6 + 25 * 1.08) ==== true

  @Test
  def overlayTest(): Unit =
    // two blocks sharing a cell overlay on the same content rect; the gutter takes the max
    val g = Grid(1, 1)
    val lay = (g.put(0, 0)(Steady(40)).put(0, 0)(Steady(10, r = 30))).solve(640, 480)
    T ~ close(lay.content(0).x, lay.content(1).x) ==== true
    T ~ close(lay.content(0).w, lay.content(1).w) ==== true
    T ~ close(lay.colGutters(0), 6 + 40 * 1.08) ==== true
    T ~ close(lay.colGutters(1), 6 + 30 * 1.08) ==== true

  @Test
  def spanTest(): Unit =
    val g = Grid(2, 2)
    val lay = (g
      .put(0, 0)(Steady(10))
      .put(0, 1)(Steady(0))
      .put(1, 1, 0, 1)(Steady(10))
    ).solve(800, 600)
    val top0 = lay.content(0)
    val top1 = lay.content(1)
    val span = lay.content(2)
    // the spanning block runs from column 0's left edge to column 1's right edge,
    // absorbing the internal gutter
    T ~ close(span.x, top0.x) ==== true
    T ~ close(span.right, top1.right) ==== true
    T ~ close(span.w, top0.w + top1.w + lay.colGutters(1)) ==== true

  @Test
  def sizePrefTest(): Unit =
    val g = Grid(1, 2)
    val lay = (g.put(0, 0)(Steady(0, wp = Size.Fixed(100))).put(0, 1)(Steady(0))).solve(640, 480)
    T ~ close(lay.colWidths(0), 100) ==== true
    T ~ close(lay.colWidths(1), 640 - 100 - lay.colGutters.sum) ==== true

  @Test
  def nestedTest(): Unit =
    val inner = Grid(1, 2)
    val ia = Steady(12)
    val ib = Steady(3)
    val _ = inner.put(0, 0)(ia).put(0, 1)(ib)
    val outer = Grid(1, 2)
    val lay = (outer.put(0, 0)(Steady(25)).put(0, 1)(inner)).solve(900, 500)
    val cell = lay.content(1)
    val subLay = lay.sub(1)
    T ~ (subLay != null) ==== true
    val s = subLay.asInstanceOf[Grid.Layout]
    // nested content stays inside the outer cell, in absolute figure coordinates
    T ~ (s.content(0).x >= cell.x - 1e-6) ==== true
    T ~ (s.content(1).right <= cell.right + 1e-6) ==== true
    T ~ (s.content(0).y >= cell.y - 1e-6) ==== true
    T ~ (s.content(1).bottom <= cell.bottom + 1e-6) ==== true

  @Test
  def nestedProtrusionTest(): Unit =
    // a nested grid reports its outer needs (pad + edge-cell protrusions); the parent
    // reserves them in its gutters and grants the content footprint, so the column
    // alignment property holds THROUGH the nesting
    val inner = Grid(1, 1, pad = 4)
    val _ = inner.put(0, 0)(Steady(30, b = 12))
    val p = inner.protrusions(200, 100)
    T ~ close(p.left, 4 + 30) ==== true
    T ~ close(p.bottom, 4 + 12) ==== true
    T ~ close(p.right, 4) ==== true
    val outer = Grid(2, 1, pad = 6)
    val lay = (outer.put(0, 0)(Steady(30)).put(1, 0)(inner)).solve(400, 400)
    T ~ close(lay.content(0).x, lay.content(1).x) ==== true
    T ~ (lay.content(1).x >= 6 + 34 - 1e-6) ==== true
    // the granted cell IS the inner content footprint: inner content starts right at it
    val sub = lay.sub(1).asInstanceOf[Grid.Layout]
    T ~ close(sub.content(0).x, lay.content(1).x) ==== true
    T ~ close(sub.content(0).y, lay.content(1).y) ==== true

  @Test
  def floatTest(): Unit =
    val g = Grid(1, 1)
    val host = Steady(20)
    val inset = Steady(0)
    val lay = (g.put(0, 0)(host).putFloat(host)(0.5, 0.1, 0.4, 0.3)(inset)).solve(640, 480)
    val hr = lay.content(0)
    val fr = lay.floatRects(0)
    T ~ close(fr.x, hr.x + 0.5 * hr.w) ==== true
    T ~ close(fr.y, hr.y + 0.1 * hr.h) ==== true
    T ~ close(fr.w, 0.4 * hr.w) ==== true
    T ~ close(fr.h, 0.3 * hr.h) ==== true

  @Test
  def aspectTest(): Unit =
    val g = Grid(1, 1)
    val lay = (g.put(0, 0)(Steady(0, asp = 2.0))).solve(900, 300)
    val rct = lay.content(0)
    T ~ close(rct.w / rct.h, 2.0) ==== true
    // shrunk dimension is centered in its cell
    T ~ close(rct.x - lay.colGutters(0), 900 - lay.colGutters(1) - rct.right) ==== true

  @Test
  def crampedTest(): Unit =
    val g = Grid(1, 1)
    val lay = (g.put(0, 0)(Steady(10000, 10000, 10000, 10000))).solve(640, 480)
    T ~ lay.cramped ==== true
    // content keeps at least a quarter of each dimension even under absurd decorations
    T ~ (lay.content(0).w >= 0.25 * 640 - 1e-6) ==== true
    T ~ (lay.content(0).h >= 0.25 * 480 - 1e-6) ==== true
    T ~ (lay.content(0).x >= 0) ==== true
    T ~ (lay.content(0).right <= 640 + 1e-6) ==== true

  @Test
  def fuzzTest(): Unit =
    var seed = 88172645463325252L
    def rnd(): Long =
      seed ^= seed << 13
      seed ^= seed >>> 7
      seed ^= seed << 17
      seed
    def rint(n: Int): Int = ((rnd() >>> 33) % n).toInt
    def rdbl(lo: Double, hi: Double): Double = lo + (hi - lo) * ((rnd() >>> 11).toDouble / (1L << 53).toDouble)

    var trial = 0
    while trial < 200 do
      val rows = 1 + rint(3)
      val cols = 1 + rint(3)
      val g = Grid(rows, cols)
      var r = 0
      while r < rows do
        var c = 0
        while c < cols do
          if rint(10) < 8 then
            val blk =
              if rint(5) == 0 then Nasty(rdbl(10, 80), rdbl(0.01, 0.2))
              else Steady(rdbl(0, 40), rdbl(0, 40), rdbl(0, 40), rdbl(0, 40))
            val _ = g.put(r, c)(blk)
          c += 1
        r += 1
      val w = 300.0 + rint(500)
      val h = 200.0 + rint(400)
      val lay = g.solve(w, h)
      val again = g.solve(w, h)
      // determinism
      T ~ lay.passes ==== again.passes
      var i = 0
      while i < lay.content.length do
        T ~ lay.content(i) ==== again.content(i)
        // containment and sanity
        T ~ (lay.content(i).w >= -1e-6) ==== true
        T ~ (lay.content(i).h >= -1e-6) ==== true
        T ~ (lay.content(i).x >= -1e-6) ==== true
        T ~ (lay.content(i).y >= -1e-6) ==== true
        T ~ (lay.content(i).right <= w + 1e-6) ==== true
        T ~ (lay.content(i).bottom <= h + 1e-6) ==== true
        i += 1
      // column monotonicity: gutters are non-negative, so widths partition cleanly
      var c2 = 0
      var acc = 0.0
      while c2 < cols + 1 do
        T ~ (lay.colGutters(c2) >= -1e-6) ==== true
        acc += lay.colGutters(c2) + (if c2 < cols then lay.colWidths(c2) else 0.0)
        c2 += 1
      T ~ (acc <= w + 1e-6) ==== true
      trial += 1
}
