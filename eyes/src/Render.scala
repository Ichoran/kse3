// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.eyes


import java.lang.{Math => jm}
import java.util.Locale

import kse.flow.{given, _}


/** Proof-of-concept interpretation: spec → scales → single-panel layout → glyphs → SVG.
  * Continuous/temporal x and y, discrete colour, scatter and line visuals, legend or title.
  * Stats and facets refuse loudly rather than render a lie; the real scene/grid/frame
  * machinery (DESIGN 5–6) replaces the layout portion.
  */
object Render:
  /** Okabe–Ito colorblind-safe palette, blue first. */
  val palette: Array[String] = Array("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442", "#000000")

  /** Colour for unmapped layers when a colour scale is in use by other layers. */
  val neutral: String = "#606060"

  private final case class Ticks(step: Double, values: Array[Double]):
    def labels: Array[String] =
      val dec = jm.max(0, -jm.floor(jm.log10(step)).toInt)
      values.map(v => String.format(Locale.ROOT, "%." + dec + "f", v))

  private def niceStep(raw: Double): Double =
    val mag = jm.pow(10, jm.floor(jm.log10(raw)))
    val frac = raw / mag
    (if frac < 1.5 then 1.0 else if frac < 3 then 2.0 else if frac < 7 then 5.0 else 10.0) * mag

  private def ticksIn(lo: Double, hi: Double, target: Int = 5): Ticks =
    val step = niceStep((hi - lo) / target)
    val t0 = jm.ceil(lo / step - 1e-9) * step
    val n = jm.max(0, ((hi + step * 1e-9 - t0) / step).toInt + 1)
    Ticks(step, Array.tabulate(n)(i => t0 + i * step))

  private def numbersOf(c: Column, what: String): Ask[Array[Double]] = Ask:
    val n = c.length
    val out = new Array[Double](n)
    c.scale match
      case sc: ScaleOf.AsContinuous[?] =>
        val f = sc.asInstanceOf[ScaleOf.AsContinuous[Any]]
        var i = 0
        while i < n do
          out(i) = f.toDouble(c.values(i))
          i += 1
      case st: ScaleOf.AsTemporal[?] =>
        val f = st.asInstanceOf[ScaleOf.AsTemporal[Any]]
        var i = 0
        while i < n do
          out(i) = f.epochSeconds(c.values(i))
          i += 1
      case _ => Err.break(s"$what needs a continuous or temporal column, but its kind is ${c.scale.kind}")
    out

  private final case class Prepped(xs: Array[Double], ys: Array[Double], colorIdx: Array[Int] | Null, kind: Visual.Kind, index: Int)

  def figureSvg(fig: Figure, width: Double, height: Double)(using m: Measurer): Ask[String] = Ask:
    val layers = fig.parts.layers
    if layers.isEmpty then Err.break("figure has no layers")

    var xLo = Double.NaN
    var xHi = Double.NaN
    var yLo = Double.NaN
    var yHi = Double.NaN
    var title: String | Null = null
    fig.parts.config.foreach:
      case Parts.Config.LegendTitle(t) => title = t
      case Parts.Config.AxisLimit(a, lo, hi) =>
        if a == Parts.Axis.Horz then
          if !lo.isNaN then xLo = lo
          if !hi.isNaN then xHi = hi
        else
          if !lo.isNaN then yLo = lo
          if !hi.isNaN then yHi = hi

    val levels = collection.mutable.ArrayBuffer.empty[String]

    val prepped = layers.zipWithIndex.map: (layer, li) =>
      if layer.look.stats.nonEmpty then
        Err.break(s"layer ${li + 1}: stats are not interpreted yet (${layer.look.stats.mkString(", ")})")
      val names = layer.data.names
      if names.contains("col") || names.contains("row") then
        Err.break(s"layer ${li + 1}: facets are not interpreted yet")
      val kind = layer.look.visual match
        case null      => Visual.Kind.Scatter
        case v: Visual => v.kind
      val yField = layer.data.fields.find(_.name == "y").getOrElse(
        Err.break(s"layer ${li + 1} needs aesthetic 'y'; it has [${names.mkString(", ")}]"))
      val ys = numbersOf(yField.column, s"layer ${li + 1} aesthetic 'y'").?
      val xs = layer.data.fields.find(_.name == "x") match
        case Some(f) => numbersOf(f.column, s"layer ${li + 1} aesthetic 'x'").?
        case None    => Array.tabulate(ys.length)(_.toDouble)  // default: observation index
      val colorIdx = layer.data.fields.find(_.name == "color") match
        case Some(f) =>
          f.column.scale match
            case d: ScaleOf.AsDiscrete[?] =>
              val lab = d.asInstanceOf[ScaleOf.AsDiscrete[Any]]
              val out = new Array[Int](f.column.length)
              var i = 0
              while i < out.length do
                val s = lab.label(f.column.values(i))
                var k = levels.indexOf(s)
                if k < 0 then
                  k = levels.length
                  val _ = levels.addOne(s)
                out(i) = k
                i += 1
              out
            case other => Err.break(s"layer ${li + 1}: only discrete colour is supported so far (got ${other.kind})")
        case None => null
      Prepped(xs, ys, colorIdx, kind, li)

    // data extent, padded, then overridden by any explicit limits
    var dxLo = Double.PositiveInfinity
    var dxHi = Double.NegativeInfinity
    var dyLo = Double.PositiveInfinity
    var dyHi = Double.NegativeInfinity
    prepped.foreach: p =>
      var i = 0
      while i < p.xs.length do
        if p.xs(i) < dxLo then dxLo = p.xs(i)
        if p.xs(i) > dxHi then dxHi = p.xs(i)
        if p.ys(i) < dyLo then dyLo = p.ys(i)
        if p.ys(i) > dyHi then dyHi = p.ys(i)
        i += 1
    if !(dxLo <= dxHi && dyLo <= dyHi) then Err.break("figure has no data points")
    if dxHi == dxLo then { dxLo -= jm.max(1.0, jm.abs(dxLo) * 0.05); dxHi += jm.max(1.0, jm.abs(dxHi) * 0.05) }
    if dyHi == dyLo then { dyLo -= jm.max(1.0, jm.abs(dyLo) * 0.05); dyHi += jm.max(1.0, jm.abs(dyHi) * 0.05) }
    val xPad = 0.04 * (dxHi - dxLo)
    val yPad = 0.04 * (dyHi - dyLo)
    val x0 = if xLo.isNaN then dxLo - xPad else xLo
    val x1 = if xHi.isNaN then dxHi + xPad else xHi
    val y0 = if yLo.isNaN then dyLo - yPad else yLo
    val y1 = if yHi.isNaN then dyHi + yPad else yHi

    val xt = ticksIn(x0, x1)
    val yt = ticksIn(y0, y1)

    // baby protrusions: tick labels, title, legend carve margins out of the figure
    val labSz = 12.0
    val titleSz = 14.0
    val tickLen = 4.0
    val yLabels = yt.labels
    val xLabels = xt.labels
    val yLabW = yLabels.foldLeft(0.0)((w, s) => jm.max(w, m.width(s, labSz)))
    val left = yLabW + tickLen + 14
    val bottom = m.lineHeight(labSz) + tickLen + 10
    val top = if title != null && levels.isEmpty then m.lineHeight(titleSz) + 12 else 14.0
    val right =
      if levels.isEmpty then 14.0
      else
        val titleW = if title == null then 0.0 else m.width(title, labSz)
        val entryW = levels.foldLeft(titleW)((w, s) => jm.max(w, 11 + 6 + m.width(s, labSz)))
        entryW + 24
    val pw = width - left - right
    val ph = height - top - bottom
    if pw < 20 || ph < 20 then Err.break(s"figure too small: ${width}x$height leaves a ${pw}x$ph panel")

    def sx(v: Double): Double = left + (v - x0) / (x1 - x0) * pw
    def sy(v: Double): Double = top + ph - (v - y0) / (y1 - y0) * ph

    val gs = List.newBuilder[Glyph]
    def put(g: Glyph): Unit = { gs += g; () }

    // gridlines under everything, then axes and tick labels
    xt.values.foreach(v => put(Glyph.Segment(sx(v), top, sx(v), top + ph, "#ECECEC", 1)))
    yt.values.foreach(v => put(Glyph.Segment(left, sy(v), left + pw, sy(v), "#ECECEC", 1)))
    put(Glyph.Segment(left, top + ph, left + pw, top + ph, "#555555", 1))
    put(Glyph.Segment(left, top, left, top + ph, "#555555", 1))
    var ti = 0
    while ti < xt.values.length do
      val px = sx(xt.values(ti))
      put(Glyph.Segment(px, top + ph, px, top + ph + tickLen, "#555555", 1))
      put(Glyph.Txt(px, top + ph + tickLen + m.ascent(labSz) + 2, xLabels(ti), labSz, "#333333", Glyph.Anchor.Middle))
      ti += 1
    ti = 0
    while ti < yt.values.length do
      val py = sy(yt.values(ti))
      put(Glyph.Segment(left - tickLen, py, left, py, "#555555", 1))
      put(Glyph.Txt(left - tickLen - 4, py + m.ascent(labSz) * 0.38, yLabels(ti), labSz, "#333333", Glyph.Anchor.End))
      ti += 1

    // data, in layer (draw) order
    prepped.foreach: p =>
      val flat = if levels.isEmpty then palette(p.index % palette.length) else neutral
      p.kind match
        case Visual.Kind.Scatter =>
          var i = 0
          while i < p.xs.length do
            val fill = p.colorIdx match
              case null => flat
              case ci   => palette(ci(i) % palette.length)
            put(Glyph.Disc(sx(p.xs(i)), sy(p.ys(i)), 3.5, fill))
            i += 1
        case Visual.Kind.Line =>
          p.colorIdx match
            case null =>
              put(Glyph.Polyline(p.xs.map(sx), p.ys.map(sy), flat, 1.8))
            case ci =>
              var lv = 0
              while lv < levels.length do
                val idx = (0 until p.xs.length).filter(i => ci(i) == lv)
                if idx.length >= 2 then
                  put(Glyph.Polyline(idx.map(i => sx(p.xs(i))).toArray, idx.map(i => sy(p.ys(i))).toArray, palette(lv % palette.length), 1.8))
                lv += 1

    // legend (when a colour scale exists) or centered title
    if levels.nonEmpty then
      val lx = width - right + 12
      var ly = top + 4
      if title != null then
        put(Glyph.Txt(lx, ly + m.ascent(labSz), title, labSz, "#222222", Glyph.Anchor.Start, bold = true))
        ly += m.lineHeight(labSz) + 2
      var lv = 0
      while lv < levels.length do
        put(Glyph.Box(lx, ly + 2, 11, 11, palette(lv % palette.length)))
        put(Glyph.Txt(lx + 11 + 6, ly + 2 + m.ascent(labSz) * 0.95, levels(lv), labSz, "#333333", Glyph.Anchor.Start))
        ly += 17
        lv += 1
    else if title != null then
      put(Glyph.Txt(left + pw / 2, m.ascent(titleSz) + 8, title, titleSz, "#222222", Glyph.Anchor.Middle, bold = true))

    Svg.render(width, height, gs.result())


extension (fig: Figure)
  /** Renders the figure to SVG text.  Proof-of-concept pipeline; see `Render`. */
  def svg(width: Double = 640, height: Double = 480)(using Measurer): Ask[String] =
    Render.figureSvg(fig, width, height)
