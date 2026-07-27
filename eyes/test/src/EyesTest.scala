// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.eyes


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
    failsWith(Fig(f => f.data(x = ts, y = vs) * f.smooth(f.Loess(0.5))), "stats are not interpreted yet")
    failsWith(Fig(f => f.data(x = ts, y = vs, col = ts2) * f.visual(f.Scatter)), "needs a discrete column")

  def ts2 = Array(1.0, 2.0, 3.0)

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
  def scaleKindTest(): Unit =
    T ~ summon[ScaleOf[Double]].kind ==== ScaleKind.Continuous
    T ~ summon[ScaleOf[Int]].kind ==== ScaleKind.Continuous
    T ~ summon[ScaleOf[String]].kind ==== ScaleKind.Discrete
    T ~ summon[ScaleOf[Boolean]].kind ==== ScaleKind.Discrete
    T ~ summon[ScaleOf[java.time.Instant]].kind ==== ScaleKind.Temporal
    T ~ summon[ScaleOf[java.time.LocalDate]].kind ==== ScaleKind.Temporal
    T ~ summon[ScaleOf[kse.maths.colours.Rgb]].kind ==== ScaleKind.Identity
}
