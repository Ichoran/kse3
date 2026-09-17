// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr.

package kse.test.twodee

// Every example in GUIDE_EXTRA-twodee.md lives here between a `// guide: name` and `// guide: end` pair,
// so that the guide's code is compiled and run.  `check-guides.py` verifies the two stay identical.

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import java.lang.{Math => jm}

import kse.flow.{given, *}
import kse.twodee.{given, *}

object GuideExamples {

  // guide: figure.kse3
  def trend(days: Array[Double], temps: Array[Double]): Figure =
    Fig: f =>
      import f.*                                        // the vocabulary: data, visual, title, axis, note, ...
      data(x = days, y = temps) * visual(Line) +        // a layer is columns times a look
        title("Water temperature") + axis.horz.title("day") + axis.vert.title("°C")
  // guide: end

  def render(fig: Figure): Ask[String] =
    // guide: render.kse3
    fig.svg(640, 480)                                   // Ask[String]: refuses with an Err rather than draw a lie
    // guide: end


  // guide: layers.kse3
  case class Reading(day: Double, temp: Double, region: String)
  def byRegion(rows: Array[Reading]): Figure =
    Fig: f =>
      import f.*
      val base = data.from(rows)(r => (x = r.day, y = r.temp, color = r.region))   // columns pulled from rows once
      base * (visual(Scatter) + visual(Line) * smooth(Loess())) +                  // points, then a smoothed line, per region
        legend("Region") + axis.horz.title("day")
  // guide: end


  // guide: chosen.kse3
  def profile(xs: Array[Double], ys: Array[Double], joined: Boolean): Figure =
    Fig: f =>
      import f.*
      val look = if joined then visual(Line) + visual(Scatter) else visual(Scatter)   // a Looking: one Look, or a sum of them
      data(x = xs, y = ys) * look + axis.horz.title("distance") + axis.vert.title("value")
  // guide: end


  // guide: stats.kse3
  def distributions(values: Array[Double], arm: Array[String]): Board =
    val hist = Fig: f =>
      import f.*
      data(x = values, color = arm) * histogram(18) + axis.vert.title("count")   // the stat makes y; don't map it
    val boxes = Fig: f =>
      import f.*
      data(x = arm, y = values) * boxplot() + axis.vert.title("value")            // categorical x: a box per level
    hist | boxes                                                                   // a board: beside; / stacks
  // guide: end


  // guide: notes.kse3
  def annotated(days: Array[Double], temps: Array[Double], peak: Int): Figure =
    Fig: f =>
      import f.*
      data(x = days, y = temps) * visual(Line) * color("#0072B2") +
        note("warmest", x = days(peak), y = temps(peak)) +   // a callout, its label placed where the panel is clear
        note.x("sensor moved", 30.0) +                        // pointing at a spot on the axis
        axis.vert.limit(min = 0.0) + axis.horz.minorGrid(true)
  // guide: end


  // guide: facets.kse3
  def faceted(rows: Array[Reading], half: Array[String]): Figure =
    Fig: f =>
      import f.*
      data.from(rows)(r => (x = r.day, y = r.temp, color = r.region)) * visual(Scatter) * facet(col = half) +
        axis.free + panels.gap(12.0)                      // one panel per level of half, each fitting its own data
  // guide: end
}


@RunWith(classOf[JUnit4])
class GuideTest {
  import kse.basics.testutilities.TestUtilities.{_, given}
  import GuideExamples as G

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  val days = Array.tabulate(60)(_.toDouble)
  val temps = days.map(d => 12.0 + 6.0 * jm.sin(d * 0.1) + 0.5 * jm.cos(d * 0.7))
  val rows = Array.tabulate(60): i =>
    val north = i % 2 == 0
    G.Reading((i / 2).toDouble, (if north then 10.0 else 14.0) + 3.0 * jm.sin(i * 0.2), if north then "north" else "south")
  val values = Array.tabulate(120)(i => 10.0 + 4.0 * jm.sin(i * 1.7) + 2.0 * jm.cos(i * 0.31))
  val arm = Array.tabulate(120)(i => if i < 60 then "control" else "treated")
  val half = rows.map(r => if r.day < 15 then "H1" else "H2")

  private def count(s: String, sub: String): Int = s.sliding(sub.length).count(_ == sub)

  @Test
  def figureTest(): Unit =
    val svg = G.render(G.trend(days, temps))
    T ~ svg.map(_.startsWith("<svg")) ==== true
    T ~ svg.map(_.contains("<polyline")) ==== true
    T ~ svg.map(_.contains("Water temperature")) ==== true
    T ~ G.trend(days, temps).image(320, 240).map(_.getWidth) ==== 320

  @Test
  def layersTest(): Unit =
    val svg = G.byRegion(rows).svg()
    T ~ svg.map(_.contains("<circle")) ==== true
    T ~ svg.map(count(_, "<polyline") >= 2) ==== true
    T ~ svg.map(s => s.contains("north") && s.contains("south")) ==== true

  @Test
  def chosenTest(): Unit =
    val joined = G.profile(days, temps, true).svg()
    T ~ joined.map(s => s.contains("<polyline") && s.contains("<circle")) ==== true
    val dots = G.profile(days, temps, false).svg()
    T ~ dots.map(_.contains("<circle")) ==== true
    T ~ dots.map(count(_, "<polyline") < count(joined.getOrElse(_ => ""), "<polyline")) ==== true

  @Test
  def statsTest(): Unit =
    val svg = G.distributions(values, arm).svg(900, 400)
    T ~ svg.map(_.contains("<rect")) ==== true
    T ~ svg.map(s => s.contains("control") && s.contains("treated")) ==== true

  @Test
  def notesTest(): Unit =
    val svg = G.annotated(days, temps, 16).svg()
    T ~ svg.map(s => s.contains("warmest") && s.contains("sensor moved")) ==== true
    T ~ svg.map(_.contains("<polygon")) ==== true

  @Test
  def facetsTest(): Unit =
    val svg = G.faceted(rows, half).svg()
    T ~ svg.map(s => s.contains("H1") && s.contains("H2")) ==== true
    T ~ svg.map(count(_, "<circle") >= 60) ==== true
}
