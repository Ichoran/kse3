// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.twodee


import java.lang.{Math => jm}
import java.nio.file.{Files, Paths}

import kse.flow.{given, _}
import kse.twodee.{given, _}


/** Renders the example figures to SVG files for human review:
  * `mill twodee.test.runMain kse.test.twodee.TwodeeExamples <outdir>`
  */
object TwodeeExamples:
  case class Rev(day: Double, revenue: Double, region: String)

  def main(args: Array[String]): Unit =
    val dir = Paths.get(if args.nonEmpty then args(0) else "twodee/examples")
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

    // Sketch 7: a directed network — edges are columns, one row per connection, so the
    // count is data-sized rather than spec-sized (10k+ edges render in tens of ms)
    val nNode = 72
    val angle = Array.tabulate(nNode)(i => 2 * jm.PI * i / nNode)
    val nodeX = angle.map(a => jm.cos(a))
    val nodeY = angle.map(a => jm.sin(a))
    def edgesOf(count: Int, salt: Long, reach: Int): (Array[Double], Array[Double], Array[Double], Array[Double]) =
      val sx = new Array[Double](count)
      val sy = new Array[Double](count)
      val tx = new Array[Double](count)
      val ty = new Array[Double](count)
      var i = 0
      while i < count do
        val a = (jm.abs((noise(i, salt) * 4096).toInt) % nNode)
        // connections favor nearby neurons, with a few long-range projections
        val hop = 1 + (jm.abs((noise(i, salt + 7) * 4096).toInt) % reach)
        val b = (a + hop) % nNode
        sx(i) = nodeX(a)
        sy(i) = nodeY(a)
        tx(i) = nodeX(b)
        ty(i) = nodeY(b)
        i += 1
      (sx, sy, tx, ty)
    val (chemSx, chemSy, chemTx, chemTy) = edgesOf(900, 101, 26)
    val (gapSx, gapSy, gapTx, gapTy) = edgesOf(180, 202, 8)
    val net = Fig: f =>
      import f.*
      data(x = gapSx, y = gapSy, xend = gapTx, yend = gapTy) * visual(Segment) *
        arrowStyle(curve = 260.0, alpha = 0.5) * color("#009E73") +
      data(x = chemSx, y = chemSy, xend = chemTx, yend = chemTy) * visual(Arrow) *
        arrowStyle(shape = ArrowShape(headLength = 5.0, headHalfWidth = 1.8, barb = 0.35, shaftWidth = 0.8),
                   curve = 260.0, alpha = 0.32) * color("#0072B2") +
      data(x = nodeX, y = nodeY) * visual(Scatter) * color("#333333") +
      title("Directed network: 900 arrows + 180 undirected links")
    writeAt(dir, "sketch7.svg", net, 760, 760)

    // Sketch 8: the tick gallery — decimal-exact labels across regimes.  Every panel is
    // the same simple wave; only the ranges (and two explicit density requests) differ.
    def waveXY(xlo: Double, xhi: Double, ylo: Double, yhi: Double, salt: Long): (Array[Double], Array[Double]) =
      val n = 72
      (Array.tabulate(n)(i => xlo + (xhi - xlo) * i / (n - 1.0)),
       Array.tabulate(n)(i => ylo + (yhi - ylo) * (0.5 + 0.38 * jm.sin(i * 0.19 + salt) + 0.24 * noise(i, salt))))
    def graph(xlo: Double, xhi: Double, ylo: Double, yhi: Double, ttl: String, salt: Long): Figure =
      val (xs, ys) = waveXY(xlo, xhi, ylo, yhi, salt)
      Fig: f =>
        import f.*
        data(x = xs, y = ys) * visual(Line) + title(ttl)
    val sciBoth = Fig: f =>
      import f.*
      val (xs, ys) = waveXY(5e4, 1.15e6, 2e-5, 1.8e-4, 3)
      data(x = xs, y = ys) * visual(Line) + axis.vert.ticks(4) +
        title("long labels go scientific, on both axes")
    val everyday = Fig: f =>
      import f.*
      val (xs, ys) = waveXY(0.03, 0.97, 0.0, 240.0, 6)
      data(x = xs, y = ys) * visual(Line) +
        axis.horz.minorGrid(true) + axis.vert.minorGrid(true) +
        title("the everyday axis, minor grid on")
    val alphaAxes = Fig: f =>
      import f.*
      val (xs, ys) = waveXY(-1.2, 1.2, 0.0, 1.0, 10)
      data(x = xs, y = ys) * visual(Line) +
        axis.horz.color("#555555", alpha = 0.5) + axis.vert.color("#555555", alpha = 0.5) +
        axis.horz.minorGrid(true) +
        title("axes at half alpha")
    val dense = Fig: f =>
      import f.*
      val (xs, ys) = waveXY(0.2, 9.8, 0.5, 5.8, 7)
      data(x = xs, y = ys) * visual(Line) +
        axis.horz.limit(min = 0.0, max = 10.0) + axis.horz.ticks(24) +
        title("dense by request: axis.horz.ticks(24), still collision-capped")
    val gallery =
      (graph(1.0115, 1.0245, 0.0, 1.0, "fine decimals: the 0.002 grid passes through 1.02", 1) |
       graph(-0.028, 0.066, -5.0, 5.0, "through zero: the origin is just \"0\"", 2)) /
      (sciBoth |
       graph(1.2e-5, 8.8e-5, 0.0, 1.0, "tiny values: 2e-5 beats 0.00002", 4)) /
      (graph(999100.0, 1000900.0, 0.0, 1.0, "fine grid far from zero: plain wins", 5) |
       everyday) /
      dense /
      (graph(0.0, 3.0, 0.0, 1.0, "0 to 3", 7) | graph(41.0, 89.0, 0.0, 1.0, "41 to 89", 8) |
       graph(0.4, 2.6, 0.0, 1.0, "0.4 to 2.6", 9) | alphaAxes)
    writeAt(dir, "sketch8.svg", gallery, 1800, 1250)

    // Sketch 9: collated / summarized data — categorical axes, box-whisker-outlier plots,
    // violins, honest never-jittered strips, direct-from-summary boxes, and binned-x boxes
    def draws(n: Int, center: Double, spread: Double, salt: Long): Array[Double] =
      Array.tabulate(n)(i => center + spread * (noise(i, salt) + noise(i + 5000, salt + 3) + noise(i + 9000, salt + 6)))
    val armNames = Array("placebo", "low dose", "high dose")
    val n9 = 180
    val arm9 = Array.tabulate(n9)(i => armNames(i % 3))
    val sex9 = Array.tabulate(n9)(i => if (i / 3) % 2 == 0 then "F" else "M")
    val resp9 = Array.tabulate(n9): i =>
      val a = i % 3
      val s = (i / 3) % 2
      val burst = if i % 47 == 0 then 11.0 else 0.0  // a few genuine extreme responders
      8.0 + 3.5 * a + 1.2 * s + burst +
        (1.8 + 0.7 * a) * (noise(i, 71) + noise(i + 500, 72) + noise(i + 900, 73))
    val box9 = Fig: f =>
      import f.*
      data(x = arm9, y = resp9, color = sex9) * boxplot() +
        legend("Sex") + title("Response by arm") + axis.vert.title("response")
    val vio9 = Fig: f =>
      import f.*
      data(x = arm9, y = resp9, color = sex9) * violin() +
        legend("Sex") + title("Same data, violins") + axis.vert.title("response")
    val ties9 = Fig: f =>
      import f.*
      // integer readings tie exactly; on a categorical axis the points swarm — spread
      // deterministically across content-free width, never jittered — and only pile-ups
      // past the slot's edge would merge into thicker rings
      data(x = arm9, y = resp9.map(v => jm.rint(v))) * strip +
        title("Integer readings: swarm, no jitter") + axis.vert.title("response (counts)")
    val overlay9 = Fig: f =>
      import f.*
      val d = data(x = arm9, y = resp9)
      d * boxplot() * color("#B8B8B8") + d * strip * color("#0072B2") * fade(0.35) +
        title("Summary and every point, superposed") + axis.vert.title("response")
    val direct9 = Fig: f =>
      import f.*
      // no raw data at all: a published 5/25/50/75/95th-percentile set per station
      data(x = Array("north", "east", "south", "west"),
           y    = Array(12.1, 14.9, 10.2, 16.4),
           ylow = Array(10.0, 12.6,  8.9, 13.8), yhigh = Array(14.5, 16.8, 11.8, 18.9),
           ymin = Array( 7.1,  9.4,  6.6, 10.2), ymax  = Array(18.0, 20.3, 14.9, 23.1)) *
        visual(Boxplot) +
        title("From published percentiles (5/25/50/75/95)") + axis.vert.title("yield")
    val ages9 = Array.tabulate(240)(i => 21.0 + 46.0 * (0.5 + noise(i, 81)))
    val income9 = Array.tabulate(240): i =>
      val a = ages9(i)
      28.0 + 1.6 * (a - 20.0) - 0.022 * (a - 47.0) * (a - 47.0) +
        14.0 * (noise(i, 91) + noise(i + 700, 92))
    val binned9 = Fig: f =>
      import f.*
      data(x = ages9, y = income9) * binBy(10.0) * boxplot() +
        title("Income by age decade: binBy(10) * boxplot()") +
        axis.horz.title("age") + axis.vert.title("income (k$)")
    val gallery9 = (box9 | vio9 | ties9) / (overlay9 | direct9 | binned9)
    writeAt(dir, "sketch9.svg", gallery9, 1250, 820)

  private def write(dir: java.nio.file.Path, name: String, fig: Figure | Board): Unit =
    writeAt(dir, name, fig, 640, 480)

  private def writeAt(dir: java.nio.file.Path, name: String, fig: Figure | Board, w: Double, h: Double): Unit =
    fig.svg(w, h).fold{ s =>
      val p = dir.resolve(name)
      val _ = Files.writeString(p, s)
      println(s"wrote $p (${s.length} chars)")
    }{ e => println(s"FAILED to render $name:\n$e") }
