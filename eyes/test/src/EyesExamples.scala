// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.eyes


import java.lang.{Math => jm}
import java.nio.file.{Files, Paths}

import kse.flow.{given, _}
import kse.eyes.{given, _}


/** Renders the example figures to SVG files for human review:
  * `mill eyes.test.runMain kse.test.eyes.EyesExamples <outdir>`
  */
object EyesExamples:
  case class Rev(day: Double, revenue: Double, region: String)

  def main(args: Array[String]): Unit =
    val dir = Paths.get(if args.nonEmpty then args(0) else "eyes/examples")
    val _ = Files.createDirectories(dir)

    // Sketch 1: a bare series through the timeseries recipe, titled, floored at zero
    val series = Array.tabulate(120)(i => 4.0 + 2.5 * jm.sin(i * 0.11) + 1.3 * jm.sin(i * 0.037 + 1.0) + 0.6 * jm.cos(i * 0.61))
    val fig1 = Fig: p =>
      p.data((y = series)) * p.timeseries +
      p.legend("The best figure ever") +
      p.axis.vert.limit(min = 0.0)
    write(dir, "sketch1.svg", fig1)

    // Sketch 2, the full shape: scatter by region + loess-smoothed lines, faceted
    // deterministic pseudo-noise so the scatter reads as noise around a trend
    def noise(i: Int, salt: Long): Double =
      val h = i * 2654435761L + salt * 0x9E3779B97F4A7C15L
      val mixed = h ^ (h >>> 33)
      (mixed & 0xFFFF).toDouble / 0xFFFF.toDouble - 0.5
    val rows =
      val b = List.newBuilder[Rev]
      var i = 0
      while i < 40 do
        b += Rev(i.toDouble, 20.0 + 0.9 * i + 2.0 * jm.sin(i * 0.5) + 7.0 * noise(i, 1), "west")
        b += Rev(i.toDouble, 14.0 + 1.3 * i + 1.5 * jm.sin(i * 0.4 + 2.0) + 7.0 * noise(i, 2), "east")
        i += 1
      b.result().toArray
    val halves = rows.map(s => if s.day < 20 then "H1" else "H2")
    val fig2 = Fig: f =>
      import f.*
      val base = data.from(rows)(s => (x = s.day, y = s.revenue, color = s.region))
      base * (visual(Scatter) + visual(Line) * smooth(Loess())) * facet(col = halves) +
        legend("Revenue by region") + axis.horz.title("day") + axis.vert.title("revenue (k$)")
    write(dir, "sketch2.svg", fig2)

    // Sketch 3: a board of independent figures, one carrying a mini-graph inset
    val diffs = Array.tabulate(series.length - 1)(i => series(i + 1) - series(i))
    val mini = Fig: f =>
      import f.*
      data((y = diffs)) * visual(Line) + title("day-over-day")
    val big = Fig: f =>
      import f.*
      // the explicit idiom for a guaranteed-clear inset spot: make the headroom yourself
      data((y = series)) * timeseries + title("Signal") +
        axis.vert.limit(max = 15) + inset(mini, "ne", w = 0.44, h = 0.4)
    val west = rows.filter(_.region == "west")
    val scat = Fig: f =>
      import f.*
      data.from(west)(s => (x = s.day, y = s.revenue)) * visual(Scatter) +
        title("West raw") + axis.horz.title("day")
    val fit = Fig: f =>
      import f.*
      data.from(west)(s => (x = s.day, y = s.revenue)) * (visual(Scatter) + visual(Line) * smooth(Fit(1))) +
        title("West + linear fit") + axis.horz.title("day")
    write(dir, "sketch3.svg", (scat | fit) / big)

    // Sketch 4: distributions — dodged histogram beside per-group density curves
    def sample(n: Int, center: Double, spread: Double, salt: Long): Array[Double] =
      Array.tabulate(n)(i => center + spread * (noise(i, salt) + noise(i + 1000, salt + 1) + noise(i + 2000, salt + 2)))
    val control = sample(150, 10.0, 6.0, 11)
    val treated = sample(150, 14.5, 9.0, 21)
    val pooled = control ++ treated
    val arm = Array.tabulate(pooled.length)(i => if i < control.length then "control" else "treated")
    val histo = Fig: f =>
      import f.*
      data(x = pooled, color = arm) * histogram(18) +
        title("Response counts") + axis.horz.title("response") + axis.vert.title("count")
    val dens = Fig: f =>
      import f.*
      data(x = pooled, color = arm) * density() +
        legend("Arm") + axis.horz.title("response") + axis.vert.title("density")
    write(dir, "sketch4.svg", histo | dens)

    // Sketch 5: a projection with widening uncertainty — band and line share a styled hue
    val fx = Array.tabulate(61)(i => i.toDouble)
    val fy = fx.map(x => 5.0 + 0.15 * x + 1.2 * jm.sin(x * 0.3))
    val half = fx.map(x => 0.7 + (if x > 40 then (x - 40) * 0.15 else 0.0))
    val fig5 = Fig: f =>
      import f.*
      val steel = color("#0072B2")
      data(x = fx, ylow = Array.tabulate(fx.length)(i => fy(i) - half(i)),
                   yhigh = Array.tabulate(fx.length)(i => fy(i) + half(i))) * visual(Band) * steel +
        data(x = fx, y = fy) * visual(Line) * steel +
        title("Projection with widening uncertainty") + axis.horz.title("day") +
        note("last firm estimate", x = 40.0, y = fy(40), radius = 90.0, shape = ArrowShape.barbed) +
        note.x("projection begins", 40.0) +
        arrow(50.0, fy(50) - 4.5, 50.0, fy(50) - half(50) - 0.1, label = "lower bound")
    write(dir, "sketch5.svg", fig5)

    // Sketch 6: continuous colour on a scatter, with its colorbar
    val n6 = 90
    val s6x = Array.tabulate(n6)(i => 5.0 + 10.0 * noise(i, 31))
    val s6y = Array.tabulate(n6)(i => 4.0 + 8.0 * noise(i, 41))
    val heat = Array.tabulate(n6)(i => s6x(i) + s6y(i) + 3.0 * noise(i, 51))
    val fig6 = Fig: f =>
      import f.*
      data(x = s6x, y = s6y, color = heat) * visual(Scatter) +
        legend("x + y + noise") + axis.horz.title("x") + axis.vert.title("y")
    write(dir, "sketch6.svg", fig6)

  private def write(dir: java.nio.file.Path, name: String, fig: Figure | Board): Unit =
    fig.svg().fold{ s =>
      val p = dir.resolve(name)
      val _ = Files.writeString(p, s)
      println(s"wrote $p (${s.length} chars)")
    }{ e => println(s"FAILED to render $name:\n$e") }
