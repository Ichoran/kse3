// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.eyes


import java.lang.{Math => jm}

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.eyes.*


case class Sale(date: Double, revenue: Double, region: String, channel: String)


@RunWith(classOf[JUnit4])
class EyesTest {
  import kse.basics.testutilities.TestUtilities.{given, _}
  import kse.flow.{given, _}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  val sales = Array(Sale(1.0, 10.0, "west", "web"), Sale(2.0, 20.0, "east", "store"))
  val ts = Array(1.0, 2.0, 3.0)
  val vs = Array(10.0, 20.0, 30.0)
  val labs = Array("a", "b", "c")

  @Test
  def sketchOneTest(): Unit =
    val xs = Array(1.0, 2.0, 3.0)
    val fig = Fig: p =>
      p.data((y = xs)) * p.timeseries +
      p.legend("The best figure ever") +
      p.axis.vert.limit(min = 0.0)
    T ~ fig.parts.layers.length ==== 1
    T ~ fig.parts.config.length ==== 2
    T ~ fig.parts.layers.head.data.names ==== List("y")
    T ~ fig.parts.layers.head.look.visual ==== Visual(Visual.Kind.Line)
    T ~ fig.parts.config.head ==== Parts.Config.LegendTitle("The best figure ever")
    fig.parts.config(1) match
      case Parts.Config.AxisLimit(which, min, max) =>
        T ~ which ==== Parts.Axis.Vert
        T ~ min ==== 0.0
        T ~ max.isNaN ==== true
      case c => assertTrue(s"expected AxisLimit, got $c", false)

  @Test
  def sketchTwoTest(): Unit =
    val fig = Fig: fig =>
      import fig.*
      val base = data.from(sales)(s => (x = s.date, y = s.revenue, color = s.region))
      val chart = base * (visual(Scatter) + visual(Line) * smooth(Loess(0.3)))
      val faceted = chart * facet(col = sales.map(_.channel))
      faceted + legend("Revenue by region")
    T ~ fig.parts.layers.length ==== 2
    T ~ fig.parts.config ==== List(Parts.Config.LegendTitle("Revenue by region"))
    val l0 = fig.parts.layers(0)
    val l1 = fig.parts.layers(1)
    T ~ l0.look.visual ==== Visual(Visual.Kind.Scatter)
    T ~ l0.look.stats ==== Nil
    T ~ l1.look.visual ==== Visual(Visual.Kind.Line)
    T ~ l1.look.stats ==== List(Smooth(Loess(0.3)))
    T ~ l0.data.names ==== List("x", "y", "color", "col")
    T ~ l1.data.names ==== List("x", "y", "color", "col")
    T ~ l0.data.length ==== 2

  @Test
  def algebraLawsTest(): Unit =
    import kse.eyes.Fig.*
    val base = data(x = ts, y = vs)
    val vS = visual(Scatter)
    val vL = visual(Line)
    val sm = smooth(Loess(0.5))
    val sm2 = smooth(Loess(0.25))

    // associativity of * on looks
    T ~ ((vS * sm) * sm2) ==== (vS * (sm * sm2))
    // identity of *
    T ~ (vS * Look.empty) ==== vS
    T ~ (Look.empty * sm) ==== sm
    // the bundle product law: bundling is *, with right-biased merge
    T ~ (data((x = ts)) * data((y = vs))) ==== data(x = ts, y = vs)
    T ~ (data((x = ts)) * data((x = labs))).data.names ==== List("x")
    T ~ (data((x = ts)) * data((x = labs))).data.fields.head.column.scale.kind ==== ScaleKind.Discrete
    // left distribution: layer * (sum of looks)
    T ~ (base * (vS + vL)) ==== ((base * vS) + (base * vL))
    // right distribution: (sum of layers) * look
    val lineLayer = base * vL
    T ~ ((base + lineLayer) * sm) ==== ((base * sm) + (lineLayer * sm))
    // look applied from the left agrees with right application (modulo bias direction)
    T ~ (vS * base) ==== (base * vS)
    // associativity of + preserves term (= draw) order
    T ~ ((vS + vL) + sm).terms ==== (vS + (vL + sm)).terms
    T ~ (base * (vS + vL)).terms.map(_.look.visual) ==== List(Visual(Visual.Kind.Scatter), Visual(Visual.Kind.Line))

  @Test
  def mixedLayersTest(): Unit =
    import kse.eyes.Fig.*
    // five aesthetics in one layer, two in another: superposition is total and each layer
    // keeps its own aesthetic set (unmapped ones resolve to style/theme at render)
    val rich = data.from(sales)(s => (x = s.date, y = s.revenue, hue = s.region, size = s.revenue, spikiness = s.date)) * visual(Scatter)
    val plain = data(x = ts, y = vs)
    val both = rich + plain
    T ~ both.terms.length ==== 2
    T ~ both.terms(0).data.names ==== List("x", "y", "hue", "size", "spikiness")
    T ~ both.terms(1).data.names ==== List("x", "y")
    T ~ (both + legend("mixed")).layers.length ==== 2

  @Test
  def lengthMismatchTest(): Unit =
    import kse.eyes.Fig.*
    var bundleCaught = false
    try
      val bad = data(x = Array(1.0, 2.0), y = Array(1.0))
      T ~ bad.data.length ==== -1  // unreachable
    catch case _: IllegalArgumentException => bundleCaught = true
    T ~ bundleCaught ==== true

    var mergeCaught = false
    try
      val bad = data((x = ts)) * facet(col = Array("a", "b"))
      T ~ bad.data.length ==== -1  // unreachable
    catch case _: IllegalArgumentException => mergeCaught = true
    T ~ mergeCaught ==== true

  @Test
  def compileRejectionTest(): Unit =
    import scala.compiletime.testing.typeChecks
    // positive twin: the same shapes compile when used correctly,
    // so the rejections below fail for the right reason
    T ~ typeChecks("""kse.eyes.Fig.data(x = Array(1.0), y = Array(2.0))""") ==== true
    // scalars are not columns: constants belong in style, not data
    T ~ typeChecks("""kse.eyes.Fig.data(x = Array(1.0), color = "red")""") ==== false
    // row function must match the row type
    T ~ typeChecks("""kse.eyes.Fig.data.from(Array(1.0, 2.0))((s: String) => (x = s.length))""") ==== false
    // Parts has no *: config does not multiply
    T ~ typeChecks("""kse.eyes.Parts.empty * kse.eyes.Fig.visual(kse.eyes.Visual.Kind.Scatter)""") ==== false
    // a bare look cannot be superposed onto a figure without a layer
    T ~ typeChecks("""kse.eyes.Fig.visual(kse.eyes.Visual.Kind.Scatter) + kse.eyes.Parts.empty""") ==== false

  @Test
  def dataSurfaceTest(): Unit =
    import kse.eyes.Fig.*
    val bundle = data(x = ts, y = vs, color = labs)
    T ~ bundle.data.names ==== List("x", "y", "color")
    T ~ bundle.data.fields.map(_.column.scale.kind) ==== List(ScaleKind.Continuous, ScaleKind.Continuous, ScaleKind.Discrete)
    T ~ bundle.data.length ==== 3
    T ~ bundle.data.fields(0).column.values(0) ==== 1.0
    T ~ bundle.data.fields(2).column.values(1) ==== "b"

    val rows = data.from(sales)(s => (x = s.date, y = s.revenue / s.date, color = s.region))
    T ~ rows.data.names ==== List("x", "y", "color")
    T ~ rows.data.length ==== 2
    T ~ rows.data.fields(0).column.values(0) ==== 1.0
    T ~ rows.data.fields(1).column.values(1) ==== 10.0
    T ~ rows.data.fields(2).column.values(0) ==== "west"
    T ~ rows.data.fields.map(_.column.scale.kind) ==== List(ScaleKind.Continuous, ScaleKind.Continuous, ScaleKind.Discrete)

  @Test
  def facetTest(): Unit =
    import kse.eyes.Fig.*
    T ~ facet(col = labs).data.names ==== List("col")
    T ~ facet(row = labs).data.names ==== List("row")
    T ~ facet(col = labs, row = labs).data.names ==== List("col", "row")
    T ~ facet(col = labs).data.fields.head.column.scale.kind ==== ScaleKind.Discrete
    val faceted = data(x = ts, y = vs) * facet(col = labs)
    T ~ faceted.data.names ==== List("x", "y", "col")

  @Test
  def renderTest(): Unit =
    val fig = Fig: f =>
      import f.*
      data(x = ts, y = vs, color = labs) * visual(Scatter) + legend("Kinds")
    val once = fig.svg()
    T ~ once.isIs ==== true
    val s = once.get
    T ~ s.startsWith("<svg") ==== true
    T ~ (s.split("<circle").length - 1) ==== 3
    T ~ s.contains("Kinds") ==== true
    T ~ s.contains("#0072B2") ==== true
    T ~ s.contains("<polyline") ==== false
    T ~ fig.svg() ==== once

  @Test
  def renderDefaultsTest(): Unit =
    val fig = Fig: p =>
      p.data((y = vs)) * p.timeseries + p.legend("Title here") + p.axis.vert.limit(min = 0.0)
    val r = fig.svg()
    T ~ r.isIs ==== true
    val s = r.get
    T ~ s.contains("<polyline") ==== true
    T ~ s.contains("Title here") ==== true
    T ~ s.contains(">0<") ==== true  // the y axis is floored at zero and labels it

  @Test
  def renderRefusalTest(): Unit =
    def failsWith(fig: Figure, part: String): Unit =
      fig.svg().fold{ _ =>
        assertTrue(s"expected failure mentioning '$part' but the figure rendered", false)
      }{ e => T ~ e.toString.contains(part) ==== true }
    failsWith(Fig(f => f.data((x = ts)) * f.visual(f.Line)), "needs aesthetic 'y'")
    failsWith(Fig(f => f.data(x = labs, y = vs)), "continuous or temporal")
    failsWith(Fig(f => f.data(x = ts, y = vs, col = ts2) * f.visual(f.Scatter)), "needs a discrete column")
    failsWith(Fig(f => f.data(x = ts, y = vs) * f.smooth(f.Loess(span = -1.0))), "span must be positive")

  def ts2 = Array(1.0, 2.0, 3.0)

  @Test
  def renderSmoothTest(): Unit =
    val n = 30
    val sx = Array.tabulate(n)(i => i.toDouble)
    val sy = sx.map(x => 2.0 * x + 1.0)
    // ungrouped loess: one smoothed line
    val fig = Fig: f =>
      import f.*
      data(x = sx, y = sy) * visual(Line) * smooth(Loess(0.6))
    val r = fig.svg()
    T ~ r.isIs ==== true
    T ~ (r.get.split("<polyline").length - 1) ==== 1
    // grouped: rolling mean per colour level, one line each
    val grp = Array.tabulate(n)(i => if i % 2 == 0 then "a" else "b")
    val fig2 = Fig: f =>
      import f.*
      data(x = sx, y = sy, color = grp) * visual(Line) * smooth(Rolling(5)) + legend("g")
    val r2 = fig2.svg()
    T ~ r2.isIs ==== true
    T ~ (r2.get.split("<polyline").length - 1) ==== 2

  @Test
  def renderFacetTest(): Unit =
    val fig = Fig: f =>
      import f.*
      data.from(sales)(s => (x = s.date, y = s.revenue, color = s.region)) *
        visual(Scatter) * facet(col = sales.map(_.channel)) +
      legend("Regions")
    val r = fig.svg()
    T ~ r.isIs ==== true
    val s = r.get
    // one panel per channel level, strips labeled, both points present
    T ~ (s.split("<circle").length - 1) ==== 2
    T ~ s.contains(">web<") ==== true
    T ~ s.contains(">store<") ==== true
    // shared scales, outer-edge labels only: y tick labels appear once (left column),
    // x tick labels once per column (bottom row is every panel here)
    T ~ (s.split(">10<").length - 1) ==== 1
    T ~ s.contains("Regions") ==== true

  @Test
  def titlesTest(): Unit =
    val fig = Fig: f =>
      import f.*
      data(x = ts, y = vs) * visual(Scatter) +
        title("Top") + axis.horz.title("day") + axis.vert.title("rev")
    val r = fig.svg()
    T ~ r.isIs ==== true
    val s = r.get
    T ~ s.contains(">Top<") ==== true
    T ~ s.contains(">day<") ==== true
    T ~ s.contains(">rev<") ==== true
    T ~ s.contains("rotate(-90") ==== true  // the y-axis title is vertical

  @Test
  def freeScalesTest(): Unit =
    val xs6 = Array(1.0, 2.0, 3.0, 1.0, 2.0, 3.0)
    val ys6 = Array(0.2, 0.4, 0.6, 120.0, 160.0, 200.0)
    val cc = Array("A", "A", "A", "B", "B", "B")
    val shared = Fig(f => f.data(x = xs6, y = ys6, col = cc) * f.visual(f.Scatter))
    val free = Fig(f => f.data(x = xs6, y = ys6, col = cc) * f.visual(f.Scatter) + f.axis.vert.free)
    val ss = shared.svg().get
    val fs = free.svg().get
    // shared: one big domain, so no fine-grained labels for the small panel
    T ~ ss.contains(">0.4<") ==== false
    // free: each panel labels its own domain
    T ~ fs.contains(">0.4<") ==== true
    T ~ fs.contains(">160<") ==== true

  @Test
  def eachLabeledTest(): Unit =
    def build(extra: Boolean) = Fig: f =>
      import f.*
      val base = data.from(sales)(s => (x = s.date, y = s.revenue)) *
        visual(Scatter) * facet(col = sales.map(_.channel))
      if extra then base + panels.eachLabeled else base + legend("x")
    val plain = build(false).svg().get
    val each = build(true).svg().get
    T ~ (plain.split(">10<").length - 1) ==== 1
    T ~ (each.split(">10<").length - 1) ==== 2

  @Test
  def panelGapTest(): Unit =
    def build(g: Double) = Fig: f =>
      import f.*
      data.from(sales)(s => (x = s.date, y = s.revenue)) *
        visual(Scatter) * facet(col = sales.map(_.channel)) + panels.gap(g)
    val tight = build(2.0).svg().get
    val loose = build(40.0).svg().get
    T ~ (tight == loose) ==== false

  @Test
  def boardTest(): Unit =
    def tiny(t: String) = Fig(f => f.data(x = ts, y = vs) * f.visual(f.Scatter) + f.title(t))
    val a = tiny("AA")
    val b = tiny("BB")
    val c = tiny("CC")
    // flattening: | and / associate into single rows/stacks
    (a | b | c) match
      case Board.Beside(items) => T ~ items.length ==== 3
      case other               => assertTrue(s"expected Beside, got $other", false)
    val board = (a | b) / c
    board match
      case Board.Above(items) => T ~ items.length ==== 2
      case other              => assertTrue(s"expected Above, got $other", false)
    val r = board.svg(800, 600)
    T ~ r.isIs ==== true
    val s = r.get
    T ~ (s.split("<svg").length - 1) ==== 1
    T ~ s.contains(">AA<") ==== true
    T ~ s.contains(">BB<") ==== true
    T ~ s.contains(">CC<") ==== true

  @Test
  def fontScaleTest(): Unit =
    val fig = Fig(f => f.data(x = ts, y = vs) * f.visual(f.Scatter) + f.title("T"))
    val big = fig.svg().get
    val small = fig.svg(220, 160).get
    // full size uses full-size type; a quarter-size figure scales it down by ~sqrt
    T ~ big.contains("font-size=\"12\"") ==== true
    T ~ small.contains("font-size=\"12\"") ==== false
    T ~ small.contains("font-size=\"14\"") ==== false

  @Test
  def insetTest(): Unit =
    val mini = Fig(f => f.data(x = ts, y = vs) * f.visual(f.Line) + f.title("MINI"))
    val fig = Fig: f =>
      import f.*
      data(x = ts, y = vs) * visual(Scatter) + inset(mini, 0.55, 0.05, 0.4, 0.38)
    val r = fig.svg()
    T ~ r.isIs ==== true
    val s = r.get
    T ~ s.contains(">MINI<") ==== true
    // background rect plus the inset's backing box
    T ~ (s.split("<rect").length - 1) ==== 2

  @Test
  def insetPlacementTest(): Unit =
    val mini = Fig(f => f.data(x = ts, y = vs) * f.visual(f.Line))
    val rising = Array.tabulate(40)(i => i.toDouble)
    def insetBox(s: String): (Double, Double) =
      val rects = """<rect x="([0-9.-]+)" y="([0-9.-]+)"""".r.findAllMatchIn(s).toList
      (rects(1).group(1).toDouble, rects(1).group(2).toDouble)  // 0 is the background
    // rising data runs sw..ne, leaving nw and se free; the tie-break prefers nw
    val up = Fig(f => f.data(x = rising, y = rising) * f.visual(f.Scatter) + f.inset(mini))
    val (ux, uy) = insetBox(up.svg().get)
    T ~ (ux < 150) ==== true
    T ~ (uy < 100) ==== true
    // falling data runs nw..se; auto placement moves to ne
    val falling = rising.map(v => -v)
    val down = Fig(f => f.data(x = rising, y = falling) * f.visual(f.Scatter) + f.inset(mini))
    val (dx, dy) = insetBox(down.svg().get)
    T ~ (dx > 300) ==== true
    T ~ (dy < 100) ==== true
    // compass anchoring goes exactly where asked
    val se = Fig(f => f.data(x = rising, y = rising) * f.visual(f.Scatter) + f.inset(mini, "se"))
    val (sx, sy) = insetBox(se.svg().get)
    T ~ (sx > 300) ==== true
    T ~ (sy > 220) ==== true
    // compass strings are checked at compile time
    import scala.compiletime.testing.typeChecks
    T ~ typeChecks("""val m = kse.eyes.Fig(f => f.data(x = Array(1.0, 2.0), y = Array(1.0, 2.0))); kse.eyes.Fig.inset(m, "ne")""") ==== true
    T ~ typeChecks("""val m = kse.eyes.Fig(f => f.data(x = Array(1.0, 2.0), y = Array(1.0, 2.0))); kse.eyes.Fig.inset(m, "qq")""") ==== false

  @Test
  def histogramTest(): Unit =
    // 1.0 x3, 2.0 x2, 5.0 x1 over nice 0.1-wide bins: three nonzero bars plus background
    val vals = Array(1.0, 1.0, 1.0, 2.0, 2.0, 5.0)
    val fig = Fig(f => f.data((x = vals)) * f.histogram())
    val r = fig.svg()
    T ~ r.isIs ==== true
    val s = r.get
    T ~ (s.split("<rect").length - 1) ==== 4
    T ~ (s.contains(">0<") || s.contains(">0.0<")) ==== true  // bars are grounded: the y domain includes zero
    // bin() alone defaults the visual to Bar
    T ~ Fig(f => f.data((x = vals)) * f.bin()).svg() ==== r

  @Test
  def dodgeTest(): Unit =
    // two colour levels, two occupied bins each: four bars, two legend swatches, background
    val xs6 = Array(1.0, 1.0, 2.0, 1.0, 2.0, 2.0)
    val grp = Array("a", "a", "a", "b", "b", "b")
    val fig = Fig(f => f.data(x = xs6, color = grp) * f.histogram() + f.legend("g"))
    val r = fig.svg()
    T ~ r.isIs ==== true
    T ~ (r.get.split("<rect").length - 1) ==== 7

  @Test
  def countTest(): Unit =
    val vals = Array(1.0, 2.0, 1.0, 3.0, 1.0)
    val fig = Fig(f => f.data((x = vals)) * f.count)
    val r = fig.svg()
    T ~ r.isIs ==== true
    T ~ (r.get.split("<rect").length - 1) ==== 4  // three distinct values, background

  @Test
  def densityTest(): Unit =
    val vals = Array(1.0, 1.5, 2.0, 2.5, 3.0, 5.0, 5.5, 6.0)
    // density() alone defaults to Area: a filled polygon under a top edge line
    val area = Fig(f => f.data((x = vals)) * f.density()).svg()
    T ~ area.isIs ==== true
    T ~ area.get.contains("<polygon") ==== true
    T ~ area.get.contains("fill-opacity") ==== true
    // an explicit Line visual draws just the curve
    val line = Fig(f => f.data((x = vals)) * f.visual(f.Line) * f.density()).svg()
    T ~ line.isIs ==== true
    T ~ line.get.contains("<polygon") ==== false
    T ~ (line.get.split("<polyline").length - 1) ==== 1

  @Test
  def bandTest(): Unit =
    val bx = Array(1.0, 2.0, 3.0, 4.0)
    val lo = Array(0.5, 1.0, 1.5, 2.0)
    val hi = Array(1.5, 2.5, 3.0, 4.0)
    val fig = Fig(f => f.data(x = bx, ylow = lo, yhigh = hi) * f.visual(f.Band))
    val r = fig.svg()
    T ~ r.isIs ==== true
    val s = r.get
    T ~ s.contains("<polygon") ==== true
    T ~ s.contains("<polyline") ==== false
    // the y domain covers the whole band, not just one edge
    T ~ (s.contains(">4<") || s.contains(">4.0<")) ==== true

  @Test
  def statMisuseTest(): Unit =
    def failsWith(fig: Figure, part: String): Unit =
      fig.svg().fold{ _ =>
        assertTrue(s"expected failure mentioning '$part' but the figure rendered", false)
      }{ e => T ~ e.toString.contains(part) ==== true }
    failsWith(Fig(f => f.data(x = ts, y = vs) * f.histogram()), "computes 'y' from the x values")
    failsWith(Fig(f => f.data((y = vs)) * f.histogram()), "bin() needs aesthetic 'x'")
    failsWith(Fig(f => f.data(x = ts, ylow = vs) * f.visual(f.Band)), "needs aesthetics 'ylow' and 'yhigh'")

  @Test
  def continuousColourTest(): Unit =
    val cv = Array(0.0, 5.0, 10.0)
    val fig = Fig(f => f.data(x = ts, y = vs, color = cv) * f.visual(f.Scatter) + f.legend("heat"))
    val r = fig.svg()
    T ~ r.isIs ==== true
    val s = r.get
    T ~ (s.split("<circle").length - 1) ==== 3
    // domain endpoints and midpoint hit the exact viridis anchors
    T ~ s.contains("#440154") ==== true
    T ~ s.contains("#21918C") ==== true
    T ~ s.contains("#FDE725") ==== true
    // the colorbar: 64 gradient slabs plus the background rect
    T ~ (s.split("<rect").length - 1) ==== 65
    T ~ s.contains(">heat<") ==== true

  @Test
  def colourScaleClashTest(): Unit =
    def failsWith(fig: Figure, part: String): Unit =
      fig.svg().fold{ _ =>
        assertTrue(s"expected failure mentioning '$part' but the figure rendered", false)
      }{ e => T ~ e.toString.contains(part) ==== true }
    val cv = Array(0.0, 5.0, 10.0)
    failsWith(
      Fig(f => f.data(x = ts, y = vs, color = cv) * f.visual(f.Scatter) + f.data(x = ts, y = vs, color = labs) * f.visual(f.Scatter)),
      "one colour scale per figure")
    failsWith(Fig(f => f.data(x = ts, y = vs, color = cv) * f.visual(f.Line)), "Scatter only")
    failsWith(Fig(f => f.data(x = ts, y = vs, color = cv) * f.smooth(f.Fit(1))), "cannot carry continuous colour")

  @Test
  def styledColourTest(): Unit =
    // a styled constant beats the layer-index default...
    val plain = Fig(f => f.data(x = ts, y = vs) * f.visual(f.Scatter) * f.color("#123456")).svg().get
    T ~ plain.contains("#123456") ==== true
    // ...but a mapped colour column beats the styled constant
    val mapped = Fig(f => f.data(x = ts, y = vs, color = labs) * f.visual(f.Scatter) * f.color("#123456") + f.legend("g")).svg().get
    T ~ mapped.contains("#123456") ==== false
    // band and line sharing one styled hue: both draw in it
    val bx = Array(1.0, 2.0, 3.0)
    val fig = Fig: f =>
      import f.*
      val steel = color("#336699")
      data(x = bx, ylow = Array(0.0, 1.0, 2.0), yhigh = Array(2.0, 3.0, 4.0)) * visual(Band) * steel +
        data(x = bx, y = Array(1.0, 2.0, 3.0)) * visual(Line) * steel
    val s = fig.svg().get
    T ~ (s.split("#336699").length - 1) ==== 2

  @Test
  def noteTest(): Unit =
    val fig = Fig: f =>
      import f.*
      data(x = ts, y = vs) * visual(Scatter) + note("the middle one", x = 2.0, y = 20.0)
    val once = fig.svg()
    T ~ once.isIs ==== true
    val s = once.get
    T ~ s.contains(">the middle one<") ==== true
    T ~ s.contains("paint-order") ==== true   // haloed label stays legible over anything
    T ~ s.contains("<polygon") ==== true      // the arrowhead
    T ~ fig.svg() ==== once                   // placement is deterministic
    // axis-target forms render too, arrow and all
    val ax = Fig: f =>
      import f.*
      data(x = ts, y = vs) * visual(Line) + note.x("here", 2.0) + note.y("level", 20.0)
    val s2 = ax.svg().get
    T ~ s2.contains(">here<") ==== true
    T ~ s2.contains(">level<") ==== true
    T ~ (s2.split("<polygon").length - 1) ==== 2

  @Test
  def noteFacetTest(): Unit =
    // shared scales contain the target in both panels, so the note appears in each
    val fig = Fig: f =>
      import f.*
      data.from(sales)(s => (x = s.date, y = s.revenue)) * visual(Scatter) *
        facet(col = sales.map(_.channel)) + note("flag", x = 1.5, y = 15.0)
    val s = fig.svg().get
    T ~ (s.split(">flag<").length - 1) ==== 2

  @Test
  def notePlacementTest(): Unit =
    // a dense wall of points sits up and to the right of the target; clear space is on
    // the left, so placement must override its up-right taste and go left
    val n = 300
    val wx = Array.tabulate(n)(i => 10.2 + 3.8 * ((i * 7) % n) / n.toDouble)
    val wy = Array.tabulate(n)(i => 2.0 + 6.0 * ((i * 13) % n) / n.toDouble)
    val fig = Fig: f =>
      import f.*
      data(x = wx, y = wy) * visual(Scatter) +
        axis.horz.limit(min = 0.0) + note("peaky", x = 10.0, y = 5.0)
    val s = fig.svg().get
    val mt = """<text x="([0-9.-]+)" y="([0-9.-]+)"[^>]*>peaky<""".r.findFirstMatchIn(s)
    T ~ mt.isDefined ==== true
    // the target maps to ~x=448; the label must sit clear of the wall, left of the target
    T ~ (mt.get.group(1).toDouble < 444) ==== true

  @Test
  def noteRefusalTest(): Unit =
    val fig = Fig: f =>
      import f.*
      data(x = ts, y = vs) * visual(Scatter) +
        axis.horz.limit(max = 5.0) + note("far away", x = 10.0, y = 20.0)
    fig.svg().fold{ _ =>
      assertTrue("expected the out-of-view note to refuse", false)
    }{ e => T ~ e.toString.contains("points outside every panel") ==== true }

  @Test
  def arrowGeometryTest(): Unit =
    // straight flat-backed: 7 points; shaft edges land exactly on the back edge at the
    // shaft's half-width, and nothing reaches past the tip
    val o = Arrow.outline(0, 0, 100, 0, 8, 4, 0, 2)
    T ~ (o == null) ==== false
    val flat = o.asInstanceOf[Arrow.Outline]
    T ~ flat.xs.length ==== 7
    T ~ flat.xs(3) ==== 100.0   // the tip, exactly at the aim point
    T ~ flat.ys(3) ==== 0.0
    T ~ flat.xs.max ==== 100.0
    T ~ flat.xs(1) ==== 92.0    // junction on the back plane (headLen behind the tip)
    T ~ flat.ys(1) ==== 1.0     // at the shaft half-width
    T ~ flat.ys(0) ==== 1.0
    T ~ flat.ys(2) ==== 4.0     // the head corner
    // barbed: back-center pulled tipward, so the junction rides the notch edge and the
    // barb corners trail behind it
    val barb = Arrow.outline(0, 0, 100, 0, 8, 4, 0.5, 2).asInstanceOf[Arrow.Outline]
    T ~ barb.xs(1) ==== 95.0
    T ~ barb.ys(1) ==== 1.0
    T ~ barb.xs(2) ==== 92.0
    T ~ barb.ys(2) ==== 4.0
    // backoff pulls the tip short of the aim point
    val backed = Arrow.outline(0, 0, 100, 0, 8, 4, 0, 2, backoff = 10).asInstanceOf[Arrow.Outline]
    T ~ backed.xs(3) ==== 90.0
    // an arrow shorter than its own head shrinks in proportion rather than vanishing —
    // a short edge in a dense graph must still draw
    val tiny = Arrow.outline(0, 0, 6, 0, 8, 4, 0, 2).asInstanceOf[Arrow.Outline]
    T ~ tiny.xs(3) ==== 6.0                  // tip still exactly on the aim point
    T ~ (tiny.ys(2) < 4.0) ==== true         // head narrowed with its length
    T ~ (tiny.ys(2) / tiny.xs(3) - 4.0 / 8.0 < 1e-9) ==== true  // proportions kept
    // only a sub-pixel arrow has nothing to say
    T ~ (Arrow.outline(0, 0, 0.5, 0, 8, 4, 0, 2) == null) ==== true
    // curved: positive radius bows to the traveler's left (up, for a rightward arrow),
    // the tip still lands exactly on the aim point, and the tail direction reports the bow
    val bent = Arrow.outline(0, 100, 100, 100, 8, 4, 0, 2, radius = 200).asInstanceOf[Arrow.Outline]
    T ~ (bent.xs.length > 7) ==== true
    var tipHit = false
    var i = 0
    while i < bent.xs.length do
      if jm.abs(bent.xs(i) - 100) < 1e-6 && jm.abs(bent.ys(i) - 100) < 1e-6 then tipHit = true
      i += 1
    T ~ tipHit ==== true
    T ~ (bent.ys.min < 97.0) ==== true
    T ~ (bent.tailDirY < 0) ==== true   // launching upward to bow over the chord
    val bentDown = Arrow.outline(0, 100, 100, 100, 8, 4, 0, 2, radius = -200).asInstanceOf[Arrow.Outline]
    T ~ (bentDown.ys.max > 103.0) ==== true

  @Test
  def arrowWordTest(): Unit =
    val fig = Fig: f =>
      import f.*
      data(x = ts, y = vs) * visual(Scatter) +
        arrow(1.2, 28.0, 2.0, 21.0, label = "gap", alpha = 0.5)
    val s = fig.svg().get
    T ~ s.contains(">gap<") ==== true
    T ~ (s.split("<polygon").length - 1) ==== 1     // one filled outline: no shaft/head seam
    T ~ s.contains("fill-opacity=\"0.5\"") ==== true
    // an endpoint cropped out by pinned limits refuses loudly
    val bad = Fig: f =>
      import f.*
      data(x = ts, y = vs) * visual(Scatter) + axis.horz.limit(max = 5.0) + arrow(1.0, 20.0, 10.0, 20.0)
    bad.svg().fold{ _ =>
      assertTrue("expected the cropped arrow to refuse", false)
    }{ e => T ~ e.toString.contains("points outside every panel") ==== true }
    // curved barbed callout: deterministic, and the outline is a real polyline-arc
    val bent = Fig: f =>
      import f.*
      data(x = ts, y = vs) * visual(Scatter) + note("bent", x = 2.0, y = 20.0, radius = 60.0, shape = ArrowShape.barbed)
    val once = bent.svg()
    T ~ once.isIs ==== true
    T ~ bent.svg() ==== once
    val poly = """<polygon points="([^"]*)"""".r.findFirstMatchIn(once.get).get.group(1)
    T ~ (poly.split(' ').length > 8) ==== true

  @Test
  def edgeLayerTest(): Unit =
    // edges are columns, one row per connection: four arrows and four plain segments
    val ax = Array(0.0, 1.0, 2.0, 3.0)
    val ay = Array(0.0, 1.0, 0.0, 1.0)
    val bx = Array(1.0, 2.0, 3.0, 0.0)
    val by = Array(1.0, 0.0, 1.0, 0.0)
    val arrows = Fig: f =>
      import f.*
      data(x = ax, y = ay, xend = bx, yend = by) * visual(Arrow)
    val sa = arrows.svg().get
    T ~ (sa.split("<polygon").length - 1) ==== 4
    val segs = Fig: f =>
      import f.*
      data(x = ax, y = ay, xend = bx, yend = by) * visual(Segment) * arrowStyle(alpha = 0.3)
    val ss = segs.svg().get
    // headless edges stroke instead of filling; the alpha marks them out from the frame
    T ~ (ss.split("stroke-opacity=\"0.3\"").length - 1) ==== 4
    T ~ ss.contains("<polygon") ==== false
    // curvature turns segments into arcs
    val bent = Fig: f =>
      import f.*
      data(x = ax, y = ay, xend = bx, yend = by) * visual(Segment) * arrowStyle(curve = 40.0)
    T ~ (bent.svg().get.split("<polyline").length - 1) ==== 4
    // far endpoints resolve through the x/y slots, so the domain covers them
    val wide = Fig: f =>
      import f.*
      data(x = Array(0.0), y = Array(0.0), xend = Array(50.0), yend = Array(80.0)) * visual(Arrow)
    val sw = wide.svg().get
    T ~ sw.contains(">40<") ==== true    // an x tick out at the far endpoint
    T ~ sw.contains(">80<") ==== true
    // colour maps per edge, exactly like any other aesthetic
    val hued = Fig: f =>
      import f.*
      data(x = ax, y = ay, xend = bx, yend = by, color = Array("a", "b", "a", "b")) * visual(Arrow) + legend("kind")
    val sh = hued.svg().get
    T ~ sh.contains("#0072B2") ==== true
    T ~ sh.contains("#E69F00") ==== true

  @Test
  def edgeScaleTest(): Unit =
    // ten thousand edges as columns: every one drawn (short edges shrink rather than
    // vanishing), and the spec stays one layer rather than ten thousand config entries
    val k = 10000
    def r(i: Int, salt: Long): Double =
      val h = i * 2654435761L + salt * 0x9E3779B97F4A7C15L
      val m = h ^ (h >>> 33)
      (m & 0xFFFFFF).toDouble / 0xFFFFFF.toDouble
    val sx = Array.tabulate(k)(i => 100 * r(i, 1))
    val sy = Array.tabulate(k)(i => 100 * r(i, 2))
    // deliberately includes sub-pixel-to-short edges, which must still draw
    val tx = Array.tabulate(k)(i => sx(i) + 3 * (r(i, 3) - 0.5))
    val ty = Array.tabulate(k)(i => sy(i) + 3 * (r(i, 4) - 0.5))
    val fig = Fig: f =>
      import f.*
      data(x = sx, y = sy, xend = tx, yend = ty) * visual(Arrow) * arrowStyle(alpha = 0.3)
    T ~ fig.parts.layers.length ==== 1
    val s = fig.svg(1200, 900).get
    T ~ (s.split("<polygon").length - 1) ==== k
    T ~ s.contains("NaN") ==== false

  @Test
  def arrowBackoffTest(): Unit =
    // an arrow aimed bit-exactly at a drawn marker stops at the disc's edge by default
    // instead of disappearing under it; explicit backoff overrides; no marker, no backoff
    val ex = Array(0.0); val ey = Array(0.0)
    val tx = Array(10.0); val ty = Array(5.0)
    val nx = Array(0.0, 10.0); val ny = Array(0.0, 5.0)
    def tipX(s: String): Double =
      val pts = s.split("<polygon points=\"")(1).split("\"")(0)
      pts.split(" ").map(p => p.split(",")(0).toDouble).max
    val bare = Fig: f =>
      import f.*
      data(x = ex, y = ey, xend = tx, yend = ty) * visual(Arrow)
    val marked = Fig: f =>
      import f.*
      data(x = ex, y = ey, xend = tx, yend = ty) * visual(Arrow) +
        data(x = nx, y = ny) * visual(Scatter)
    val manual = Fig: f =>
      import f.*
      data(x = ex, y = ey, xend = tx, yend = ty) * visual(Arrow) * arrowStyle(backoff = 20.0) +
        data(x = nx, y = ny) * visual(Scatter)
    val b = tipX(bare.svg().get)
    val d = tipX(marked.svg().get)
    val m = tipX(manual.svg().get)
    T ~ (d < b - 2) ==== true
    T ~ (m < d - 2) ==== true

  @Test
  def axisTickDensityTest(): Unit =
    // axis.*.ticks(n) asks for about n ticks; nice steps quantize delivery and the
    // collision cap still has the last word, so a wide panel really gets 21 labels
    val xs = Array.tabulate(50)(i => 0.2 + 9.6 * i / 49.0)
    val ys = Array.tabulate(50)(i => (i % 7).toDouble)
    def fig(dense: Boolean) = Fig: f =>
      import f.*
      val base = data(x = xs, y = ys) * visual(Line) + axis.horz.limit(min = 0.0, max = 10.0)
      if dense then base + axis.horz.ticks(24) else base
    val plain = fig(false).svg(1900, 340).get
    val dense = fig(true).svg(1900, 340).get
    T ~ plain.contains(">0.5<") ==== false
    var k = 0
    var all = true
    while k <= 20 do
      val v = k * 0.5
      val s = if v == jm.rint(v) then v.toLong.toString else v.toString
      all &= dense.contains(">" + s + "<")
      k += 1
    T ~ all ==== true
    // and the knob turns the other way: fewer than the panel would have chosen
    val fewY = Fig: f =>
      import f.*
      data(x = xs, y = ys) * visual(Line) + axis.vert.ticks(3)
    val fy = fewY.svg().get
    T ~ fy.contains(">5<") ==== false
    T ~ fy.contains(">4<") ==== true

  @Test
  def edgeLabelsOnCanvasTest(): Unit =
    // a label centered under the last tick must not leak off the canvas: the panel
    // reports edge-label overhang as protrusion, so the layout reserves room for it
    val xs = Array.tabulate(50)(i => 0.2 + 9.6 * i / 49.0)
    val ys = Array.tabulate(50)(i => (i % 5).toDouble)
    val fig = Fig: f =>
      import f.*
      data(x = xs, y = ys) * visual(Line) +
        axis.horz.limit(min = 0.0, max = 10.0) + axis.horz.ticks(24)
    val s = fig.svg(1900, 340).get
    T ~ s.contains(">10<") ==== true
    val mzr = Measurer.approx
    val cent = """<text x="([-0-9.]+)"[^>]*font-size="([-0-9.]+)"[^>]*text-anchor="middle"[^>]*>([^<]+)</text>""".r
    var found = 0
    cent.findAllMatchIn(s).foreach: mm =>
      val x = mm.group(1).toDouble
      val sz = mm.group(2).toDouble
      val t = mm.group(3)
      T ~ (x + mzr.width(t, sz) / 2 <= 1900.5) ==== true
      T ~ (x - mzr.width(t, sz) / 2 >= -0.5) ==== true
      found += 1
    T ~ (found >= 21) ==== true

  @Test
  def axisTitleAttachmentTest(): Unit =
    // the x-axis title is part of the axis: centered on the data area (not on a grid
    // cell that also contains the y-label gutter) and snug below the tick labels
    val xs = Array.tabulate(40)(i => i * 0.25)
    val ys = Array.tabulate(40)(i => 3.0 + (i % 7))
    val fig = Fig: f =>
      import f.*
      data(x = xs, y = ys) * visual(Line) + axis.horz.title("day") + axis.vert.title("value")
    val s = fig.svg().get
    val frameRx = """<line x1="([-0-9.]+)" y1="([-0-9.]+)" x2="([-0-9.]+)" y2="([-0-9.]+)" stroke="#555555"""".r
    var fx1 = 0.0
    var fx2 = 0.0
    var fy = 0.0
    frameRx.findAllMatchIn(s).foreach: mm =>
      val x1 = mm.group(1).toDouble
      val y1 = mm.group(2).toDouble
      val x2 = mm.group(3).toDouble
      val y2 = mm.group(4).toDouble
      if y1 == y2 && x2 - x1 > 100 then
        fx1 = x1
        fx2 = x2
        fy = y1
    val dm = """<text x="([-0-9.]+)" y="([-0-9.]+)"[^>]*>day</text>""".r.findFirstMatchIn(s).get
    val dx = dm.group(1).toDouble
    val dy = dm.group(2).toDouble
    T ~ (jm.abs(dx - (fx1 + fx2) / 2) < 0.6) ==== true
    val mzr = Measurer.approx
    val labelBase = fy + 4 + mzr.ascent(12) + 2
    val capTop = dy - mzr.ascent(15)
    T ~ (capTop - labelBase > 3) ==== true
    T ~ (capTop - labelBase < 11) ==== true

  @Test
  def edgeMisuseTest(): Unit =
    def failsWith(fig: Figure, part: String): Unit =
      fig.svg().fold{ _ =>
        assertTrue(s"expected failure mentioning '$part' but the figure rendered", false)
      }{ e => T ~ e.toString.contains(part) ==== true }
    failsWith(Fig(f => f.data(x = ts, y = vs) * f.visual(f.Arrow)), "needs aesthetics 'xend' and 'yend'")
    failsWith(
      Fig(f => f.data(x = ts, y = vs, xend = ts, yend = vs) * f.visual(f.Arrow) * f.smooth(f.Fit(1))),
      "cannot transform edge geometry")

  @Test
  def scaleKindTest(): Unit =
    T ~ summon[ScaleOf[Double]].kind ==== ScaleKind.Continuous
    T ~ summon[ScaleOf[Int]].kind ==== ScaleKind.Continuous
    T ~ summon[ScaleOf[String]].kind ==== ScaleKind.Discrete
    T ~ summon[ScaleOf[Boolean]].kind ==== ScaleKind.Discrete
    T ~ summon[ScaleOf[java.time.Instant]].kind ==== ScaleKind.Temporal
    T ~ summon[ScaleOf[java.time.LocalDate]].kind ==== ScaleKind.Temporal
    T ~ summon[ScaleOf[kse.maths.colours.Rgb]].kind ==== ScaleKind.Identity
}
