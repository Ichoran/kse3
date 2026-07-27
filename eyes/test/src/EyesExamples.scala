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

    // Sketch 2 (minus smooth/facet for now): scatter + line, coloured by region
    val rows =
      val b = List.newBuilder[Rev]
      var i = 0
      while i < 40 do
        b += Rev(i.toDouble, 20.0 + 0.9 * i + 6.0 * jm.sin(i * 0.5), "west")
        b += Rev(i.toDouble, 14.0 + 1.3 * i + 5.0 * jm.sin(i * 0.4 + 2.0), "east")
        i += 1
      b.result().toArray
    val fig2 = Fig: f =>
      import f.*
      val base = data.from(rows)(s => (x = s.day, y = s.revenue, color = s.region))
      base * (visual(Scatter) + visual(Line)) + legend("Revenue by region")
    write(dir, "sketch2.svg", fig2)

  private def write(dir: java.nio.file.Path, name: String, fig: Figure): Unit =
    fig.svg().fold{ s =>
      val p = dir.resolve(name)
      val _ = Files.writeString(p, s)
      println(s"wrote $p (${s.length} chars)")
    }{ e => println(s"FAILED to render $name:\n$e") }
