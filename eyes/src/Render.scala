// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.eyes


import java.lang.{Math => jm}
import java.util.Locale

import kse.flow.{given, _}


/** Interpretation: spec → shared scales → protrusion-grid layout → glyphs → SVG.
  * Panels are Blocks: tick labels, ticks, and facet strips are protrusions, so multi-panel
  * figures align on content by construction.  Facets come from the reserved `col`/`row`
  * columns: panels form a grid with shared x/y scales, tick labels only on outer edges,
  * col strips above the top row and row strips right of the last column; layers without
  * facet columns appear in every panel.  Stats and continuous colour still refuse loudly
  * rather than render a lie.
  */
object Render:
  /** Okabe–Ito colorblind-safe palette, blue first. */
  val palette: Array[String] = Array("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442", "#000000")

  /** Colour for unmapped layers when a colour scale is in use by other layers. */
  val neutral: String = "#606060"

  private val labSz = 12.0
  private val titleSz = 14.0
  private val tickLen = 4.0

  /** Type size scale for a figure granted (w, h): an n-fold smaller figure gets sqrt(n)
    * smaller type, with the two dimension ratios combined by RMS so the larger dimension
    * dominates when shrinkage is uneven.  Clamped for legibility.
    */
  private def fontScale(w: Double, h: Double): Double =
    val sx = w / 640.0
    val sy = h / 480.0
    val s = jm.sqrt((sx * sx + sy * sy) / 2)
    jm.min(1.4, jm.max(0.5, jm.sqrt(s)))

  private final case class Ticks(step: Double, values: Array[Double]):
    def labels: Array[String] =
      val dec = jm.max(0, -jm.floor(jm.log10(step)).toInt)
      values.map(v => String.format(Locale.ROOT, "%." + dec + "f", v))

  private def niceStep(raw: Double): Double =
    val mag = jm.pow(10, jm.floor(jm.log10(raw)))
    val frac = raw / mag
    (if frac < 1.5 then 1.0 else if frac < 3 then 2.0 else if frac < 7 then 5.0 else 10.0) * mag

  private def ticksIn(lo: Double, hi: Double, target: Int): Ticks =
    val step = niceStep((hi - lo) / jm.max(1, target))
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

  private def labelsOf(c: Column, what: String): Ask[Array[String]] = Ask:
    c.scale match
      case d: ScaleOf.AsDiscrete[?] =>
        val f = d.asInstanceOf[ScaleOf.AsDiscrete[Any]]
        val out = new Array[String](c.length)
        var i = 0
        while i < out.length do
          out(i) = f.label(c.values(i))
          i += 1
        out
      case other => Err.break(s"$what needs a discrete column, but its kind is ${other.kind}")

  private final case class Prep(
    xs: Array[Double], ys: Array[Double], colorIdx: Array[Int] | Null,
    kind: Visual.Kind, layerIdx: Int,
    colLabs: Array[String] | Null, rowLabs: Array[String] | Null
  )

  private final case class Slice(xs: Array[Double], ys: Array[Double], colorIdx: Array[Int] | Null, kind: Visual.Kind, layerIdx: Int)

  private def sliceFor(p: Prep, cl: String | Null, rl: String | Null): Slice =
    inline def keep(i: Int): Boolean =
      (cl == null || p.colLabs == null || p.colLabs(i) == cl) &&
      (rl == null || p.rowLabs == null || p.rowLabs(i) == rl)
    val n = p.xs.length
    var cnt = 0
    var i = 0
    while i < n do
      if keep(i) then cnt += 1
      i += 1
    if cnt == n then Slice(p.xs, p.ys, p.colorIdx, p.kind, p.layerIdx)
    else
      val xs = new Array[Double](cnt)
      val ys = new Array[Double](cnt)
      val ci = if p.colorIdx == null then null else new Array[Int](cnt)
      var j = 0
      i = 0
      while i < n do
        if keep(i) then
          xs(j) = p.xs(i)
          ys(j) = p.ys(i)
          if ci != null then ci(j) = p.colorIdx(i)
          j += 1
        i += 1
      Slice(xs, ys, ci, p.kind, p.layerIdx)

  private def grid(lo: Double, hi: Double, n: Int): Array[Double] =
    if hi <= lo then Array(lo)
    else Array.tabulate(n)(i => lo + (hi - lo) * i / (n - 1).toDouble)

  private def statted(p: Prep, stats: List[Stat]): Prep =
    stats.foldLeft(p): (q, st) =>
      st match
        case Smooth(how) => smoothPrep(q, how)

  private def applySmoother(how: Smoother, sx: Array[Double], sy: Array[Double]): (Array[Double], Array[Double]) =
    if sx.length < 2 then (sx, sy)
    else how match
      case Loess(span, deg, rob) =>
        val ex = grid(sx(0), sx(sx.length - 1), 80)
        (ex, kse.maths.Smoothing.loessAt(sx, sy, ex, span, deg, rob))
      case Kernel(bw, shape, deg) =>
        val ex = grid(sx(0), sx(sx.length - 1), 80)
        val sh = shape match
          case Kernel.Shape.Gaussian     => kse.maths.Smoothing.Shape.Gaussian
          case Kernel.Shape.Epanechnikov => kse.maths.Smoothing.Shape.Epanechnikov
          case Kernel.Shape.Tricube      => kse.maths.Smoothing.Shape.Tricube
        (ex, kse.maths.Smoothing.kernelAt(sx, sy, ex, bw, sh, deg))
      case Fit(d) =>
        val ex = grid(sx(0), sx(sx.length - 1), 80)
        (ex, kse.maths.Smoothing.polyFitAt(sx, sy, ex, d))
      case Rolling(w)       => (sx, kse.maths.Smoothing.rollingMean(sy, w))
      case RollingMedian(w) => (sx, kse.maths.Smoothing.rollingMedian(sy, w))

  /** Applies a smoother per group — one group per (colour level × facet cell), the stat
    * contract in action: grouped, cardinality-changing, grouping columns passed through
    * so colour and facet assignments stay consistent on the smoothed output.
    */
  private def smoothPrep(p: Prep, how: Smoother): Prep =
    val n = p.xs.length
    if n == 0 then return p
    val keyCi = collection.mutable.ArrayBuffer.empty[Int]
    val keyCl = collection.mutable.ArrayBuffer.empty[String | Null]
    val keyRl = collection.mutable.ArrayBuffer.empty[String | Null]
    val members = collection.mutable.ArrayBuffer.empty[collection.mutable.ArrayBuffer[Int]]
    var i = 0
    while i < n do
      val ci = p.colorIdx match { case null => -1; case a => a(i) }
      val cl: String | Null = p.colLabs match { case null => null; case a => a(i) }
      val rl: String | Null = p.rowLabs match { case null => null; case a => a(i) }
      var found = -1
      var g = 0
      while found < 0 && g < keyCi.length do
        if keyCi(g) == ci && keyCl(g) == cl && keyRl(g) == rl then found = g
        g += 1
      if found < 0 then
        found = keyCi.length
        val _ = keyCi.addOne(ci)
        val _ = keyCl.addOne(cl)
        val _ = keyRl.addOne(rl)
        val _ = members.addOne(collection.mutable.ArrayBuffer.empty[Int])
      val _ = members(found).addOne(i)
      i += 1
    val gx = new Array[Array[Double]](members.length)
    val gy = new Array[Array[Double]](members.length)
    var g = 0
    while g < members.length do
      val order = members(g).toArray.sortBy(p.xs(_))
      val sx = order.map(p.xs(_))
      val sy = order.map(p.ys(_))
      val (ex, ey) = applySmoother(how, sx, sy)
      gx(g) = ex
      gy(g) = ey
      g += 1
    var total = 0
    g = 0
    while g < gx.length do
      total += gx(g).length
      g += 1
    val xs2 = new Array[Double](total)
    val ys2 = new Array[Double](total)
    val ci2 = if p.colorIdx == null then null else new Array[Int](total)
    val cl2 = if p.colLabs == null then null else new Array[String](total)
    val rl2 = if p.rowLabs == null then null else new Array[String](total)
    var o = 0
    g = 0
    while g < gx.length do
      val ex = gx(g)
      val ey = gy(g)
      var k = 0
      while k < ex.length do
        xs2(o) = ex(k)
        ys2(o) = ey(k)
        if ci2 != null then ci2(o) = keyCi(g)
        if cl2 != null then cl2(o) = keyCl(g)
        if rl2 != null then rl2(o) = keyRl(g)
        o += 1
        k += 1
      g += 1
    Prep(xs2, ys2, ci2, p.kind, p.layerIdx, cl2, rl2)

  /** One data panel: axis decorations and facet strips are protrusions; the content rect
    * is pure data area.  Tick density adapts to the granted size, so panels degrade
    * gracefully when small; equal panel sizes plus shared domains give identical ticks
    * across a facet grid.
    */
  private final class Panel(
    slices: List[Slice],
    x0: Double, x1: Double, y0: Double, y1: Double,
    anyColourScale: Boolean,
    showLeft: Boolean, showBottom: Boolean,
    colStrip: String | Null, rowStrip: String | Null,
    fs: Double,
    m: Measurer
  ) extends GlyphBlock:
    private val lab = labSz * fs
    private val tick = tickLen * fs

    // ticks aim for a pleasing density but are hard-capped so labels cannot collide even
    // on a very short axis; a zero-centered domain keeps -x, 0, +x at the cap
    private def fitTicks(lo: Double, hi: Double, target: Int, cap: Int): Ticks =
      var t = jm.max(1, jm.min(target, cap))
      var ts = ticksIn(lo, hi, t)
      while ts.values.length > jm.max(2, cap) && t > 1 do
        t -= 1
        ts = ticksIn(lo, hi, t)
      ts
    private def xTicks(w: Double): Ticks =
      fitTicks(x0, x1, jm.max(2, jm.min(8, (w / (lab * 7.5)).toInt)), jm.max(2, (w / (lab * 4.5)).toInt))
    private def yTicks(h: Double): Ticks =
      fitTicks(y0, y1, jm.max(2, jm.min(8, (h / (m.lineHeight(lab) * 4.5)).toInt)), jm.max(2, (h / (m.lineHeight(lab) * 1.5)).toInt))

    def protrusions(w: Double, h: Double): Prot =
      val left =
        if showLeft then yTicks(h).labels.foldLeft(0.0)((mx, s) => jm.max(mx, m.width(s, lab))) + tick + 8 * fs
        else 0.0
      val bottom = if showBottom then m.lineHeight(lab) + tick + 6 * fs else 0.0
      val top = if colStrip != null then m.lineHeight(lab) + 4 * fs else 0.0
      val right = if rowStrip != null then m.width(rowStrip, lab) + 8 * fs else 0.0
      Prot(left, right, top, bottom)

    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      val xt = xTicks(rect.w)
      val yt = yTicks(rect.h)
      def sx(v: Double): Double = rect.x + (v - x0) / (x1 - x0) * rect.w
      def sy(v: Double): Double = rect.y + rect.h - (v - y0) / (y1 - y0) * rect.h
      xt.values.foreach(v => put(Glyph.Segment(sx(v), rect.y, sx(v), rect.bottom, "#ECECEC", 1)))
      yt.values.foreach(v => put(Glyph.Segment(rect.x, sy(v), rect.right, sy(v), "#ECECEC", 1)))
      put(Glyph.Segment(rect.x, rect.bottom, rect.right, rect.bottom, "#555555", 1))
      put(Glyph.Segment(rect.x, rect.y, rect.x, rect.bottom, "#555555", 1))
      if showBottom then
        val xL = xt.labels
        var i = 0
        while i < xt.values.length do
          val px = sx(xt.values(i))
          put(Glyph.Segment(px, rect.bottom, px, rect.bottom + tick, "#555555", 1))
          put(Glyph.Txt(px, rect.bottom + tick + m.ascent(lab) + 2, xL(i), lab, "#333333", Glyph.Anchor.Middle))
          i += 1
      if showLeft then
        val yL = yt.labels
        var i = 0
        while i < yt.values.length do
          val py = sy(yt.values(i))
          put(Glyph.Segment(rect.x - tick, py, rect.x, py, "#555555", 1))
          put(Glyph.Txt(rect.x - tick - 4, py + m.ascent(lab) * 0.38, yL(i), lab, "#333333", Glyph.Anchor.End))
          i += 1
      if colStrip != null then
        put(Glyph.Txt(rect.x + rect.w / 2, rect.y - 5 * fs, colStrip, lab, "#222222", Glyph.Anchor.Middle, bold = true))
      if rowStrip != null then
        put(Glyph.Txt(rect.right + 6, rect.y + rect.h / 2 + m.ascent(lab) * 0.38, rowStrip, lab, "#222222", Glyph.Anchor.Start, bold = true))
      slices.foreach: s =>
        val flat = if anyColourScale then neutral else palette(s.layerIdx % palette.length)
        s.kind match
          case Visual.Kind.Scatter =>
            var i = 0
            while i < s.xs.length do
              val fill = s.colorIdx match
                case null => flat
                case ci   => palette(ci(i) % palette.length)
              put(Glyph.Disc(sx(s.xs(i)), sy(s.ys(i)), 3.5 * fs, fill))
              i += 1
          case Visual.Kind.Line =>
            s.colorIdx match
              case null =>
                if s.xs.length >= 2 then put(Glyph.Polyline(s.xs.map(sx), s.ys.map(sy), flat, jm.max(0.8, 1.8 * fs)))
              case ci =>
                var maxLv = -1
                var i = 0
                while i < ci.length do
                  if ci(i) > maxLv then maxLv = ci(i)
                  i += 1
                var lv = 0
                while lv <= maxLv do
                  val idx = (0 until s.xs.length).filter(i => ci(i) == lv)
                  if idx.length >= 2 then
                    put(Glyph.Polyline(idx.map(i => sx(s.xs(i))).toArray, idx.map(i => sy(s.ys(i))).toArray, palette(lv % palette.length), jm.max(0.8, 1.8 * fs)))
                  lv += 1

  private final class LegendBlock(title: String | Null, levels: Array[String], fs: Double, m: Measurer) extends GlyphBlock:
    private val lab = labSz * fs
    private def innerWidth: Double =
      val titleW = if title == null then 0.0 else m.width(title, lab)
      levels.foldLeft(titleW)((w, s) => jm.max(w, 11 * fs + 6 + m.width(s, lab))) + 16 * fs
    override def widthPref: Size = Size.Fixed(innerWidth)
    def protrusions(w: Double, h: Double): Prot = Prot.zero
    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      val lx = rect.x + 4
      var ly = rect.y + 2
      if title != null then
        put(Glyph.Txt(lx, ly + m.ascent(lab), title, lab, "#222222", Glyph.Anchor.Start, bold = true))
        ly += m.lineHeight(lab) + 2
      var lv = 0
      while lv < levels.length do
        put(Glyph.Box(lx, ly + 2, 11 * fs, 11 * fs, palette(lv % palette.length)))
        put(Glyph.Txt(lx + 11 * fs + 6, ly + 2 + m.ascent(lab) * 0.95, levels(lv), lab, "#333333", Glyph.Anchor.Start))
        ly += 17 * fs
        lv += 1

  private final class TitleBlock(text: String, fs: Double, m: Measurer) extends GlyphBlock:
    private val ttl = titleSz * fs
    override def heightPref: Size = Size.Fixed(m.lineHeight(ttl) + 6 * fs)
    def protrusions(w: Double, h: Double): Prot = Prot.zero
    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      put(Glyph.Txt(rect.x + rect.w / 2, rect.y + m.ascent(ttl) + 2, text, ttl, "#222222", Glyph.Anchor.Middle, bold = true))

  private final class XTitleBlock(text: String, fs: Double, m: Measurer) extends GlyphBlock:
    private val lab = labSz * fs
    override def heightPref: Size = Size.Fixed(m.lineHeight(lab) + 4 * fs)
    def protrusions(w: Double, h: Double): Prot = Prot.zero
    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      put(Glyph.Txt(rect.x + rect.w / 2, rect.y + m.ascent(lab) + 2, text, lab, "#222222", Glyph.Anchor.Middle))

  private final class YTitleBlock(text: String, fs: Double, m: Measurer) extends GlyphBlock:
    private val lab = labSz * fs
    override def widthPref: Size = Size.Fixed(m.lineHeight(lab) + 2 * fs)
    def protrusions(w: Double, h: Double): Prot = Prot.zero
    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      put(Glyph.Txt(rect.x + m.ascent(lab) + 2, rect.y + rect.h / 2, text, lab, "#222222", Glyph.Anchor.Middle, rotate = -90))

  private def axisSpan(lo0: Double, hi0: Double, cfgLo: Double, cfgHi: Double): (Double, Double) =
    var lo = lo0
    var hi = hi0
    if hi == lo then
      lo -= jm.max(1.0, jm.abs(lo) * 0.05)
      hi += jm.max(1.0, jm.abs(hi) * 0.05)
    val pad = 0.04 * (hi - lo)
    (if cfgLo.isNaN then lo - pad else cfgLo, if cfgHi.isNaN then hi + pad else cfgHi)

  private def freeSpan(slices: List[Slice], horz: Boolean, cfgLo: Double, cfgHi: Double, fb0: Double, fb1: Double): (Double, Double) =
    var lo = Double.PositiveInfinity
    var hi = Double.NegativeInfinity
    slices.foreach: s =>
      val a = if horz then s.xs else s.ys
      var i = 0
      while i < a.length do
        if a(i) < lo then lo = a(i)
        if a(i) > hi then hi = a(i)
        i += 1
    if !(lo <= hi) then (fb0, fb1) else axisSpan(lo, hi, cfgLo, cfgHi)

  /** Builds a figure's full block tree — facet grid of panels, legend, titles, insets —
    * rooted in one outer grid, ready to solve at any size.
    */
  private def buildFigure(fig: Figure, estW: Double, estH: Double)(using m: Measurer): Ask[Grid] = Ask:
    val fs = fontScale(estW, estH)
    val layers = fig.parts.layers
    if layers.isEmpty then Err.break("figure has no layers")

    var xLoC = Double.NaN
    var xHiC = Double.NaN
    var yLoC = Double.NaN
    var yHiC = Double.NaN
    var legTitle: String | Null = null
    var figTitle: String | Null = null
    var xTitle: String | Null = null
    var yTitle: String | Null = null
    var freeX = false
    var freeY = false
    var gapH = 12.0
    var gapV = 12.0
    var everyLabel = false
    val insets = collection.mutable.ArrayBuffer.empty[Parts.Config.Inset]
    fig.parts.config.foreach:
      case Parts.Config.LegendTitle(t) => legTitle = t
      case Parts.Config.FigTitle(t)    => figTitle = t
      case Parts.Config.AxisTitle(a, t) =>
        if a == Parts.Axis.Horz then xTitle = t else yTitle = t
      case Parts.Config.AxisLimit(a, lo, hi) =>
        if a == Parts.Axis.Horz then
          if !lo.isNaN then xLoC = lo
          if !hi.isNaN then xHiC = hi
        else
          if !lo.isNaN then yLoC = lo
          if !hi.isNaN then yHiC = hi
      case Parts.Config.FreeAxis(h, v) =>
        freeX |= h
        freeY |= v
      case Parts.Config.PanelGap(h, v) =>
        gapH = h
        gapV = v
      case Parts.Config.EachLabeled => everyLabel = true
      case ins: Parts.Config.Inset  => val _ = insets.addOne(ins)

    val levels = collection.mutable.ArrayBuffer.empty[String]

    val prepped = layers.zipWithIndex.map: (layer, li) =>
      val kind = layer.look.visual match
        case null      => Visual.Kind.Scatter
        case v: Visual => v.kind
      val names = layer.data.names
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
      val colLabs = layer.data.fields.find(_.name == "col") match
        case Some(f) => labelsOf(f.column, s"layer ${li + 1} facet 'col'").?
        case None    => null
      val rowLabs = layer.data.fields.find(_.name == "row") match
        case Some(f) => labelsOf(f.column, s"layer ${li + 1} facet 'row'").?
        case None    => null
      val raw = Prep(xs, ys, colorIdx, kind, li, colLabs, rowLabs)
      // stats run upstream of scale resolution, so domains cover the transformed data
      if layer.look.stats.isEmpty then raw else statted(raw, layer.look.stats)

    // shared scales: one x and one y domain across every panel
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
    val (fx0, fx1) = axisSpan(dxLo, dxHi, xLoC, xHiC)
    val (fy0, fy1) = axisSpan(dyLo, dyHi, yLoC, yHiC)

    // facet levels, first appearance in layer order; null level = the unfaceted dimension
    def levelsFor(get: Prep => Array[String] | Null): Array[String | Null] =
      val buf = collection.mutable.ArrayBuffer.empty[String]
      prepped.foreach: p =>
        val labs = get(p)
        if labs != null then
          var i = 0
          while i < labs.length do
            if !buf.contains(labs(i)) then { val _ = buf.addOne(labs(i)) }
            i += 1
      if buf.isEmpty then Array[String | Null](null) else buf.toArray[String | Null]
    val colLevels = levelsFor(_.colLabs)
    val rowLevels = levelsFor(_.rowLabs)
    val nC = colLevels.length
    val nR = rowLevels.length

    val facetGrid = Grid(nR, nC, colGap = gapH, rowGap = gapV, pad = 6)
    var r = 0
    while r < nR do
      var c = 0
      while c < nC do
        val cl = colLevels(c)
        val rl = rowLevels(r)
        val slices = prepped.map(p => sliceFor(p, cl, rl))
        val (px0, px1) = if freeX then freeSpan(slices, true, xLoC, xHiC, fx0, fx1) else (fx0, fx1)
        val (py0, py1) = if freeY then freeSpan(slices, false, yLoC, yHiC, fy0, fy1) else (fy0, fy1)
        val pan = Panel(
          slices, px0, px1, py0, py1,
          anyColourScale = levels.nonEmpty,
          showLeft = freeY || everyLabel || c == 0,
          showBottom = freeX || everyLabel || r == nR - 1,
          colStrip = if r == 0 then cl else null,
          rowStrip = if c == nC - 1 then rl else null,
          fs,
          m
        )
        val _ = facetGrid.put(r, c)(pan)
        c += 1
      r += 1

    val legendB = if levels.nonEmpty then LegendBlock(legTitle, levels.toArray, fs, m) else null
    // legend(...) doubles as the figure title when no legend is drawn; title(...) always wins
    val topText: String | Null = if figTitle != null then figTitle else if levels.isEmpty then legTitle else null
    val titleB = if topText != null then TitleBlock(topText, fs, m) else null
    val xtB = if xTitle != null then XTitleBlock(xTitle, fs, m) else null
    val ytB = if yTitle != null then YTitleBlock(yTitle, fs, m) else null

    val rT = if titleB != null then 1 else 0
    val rX = if xtB != null then 1 else 0
    val cY = if ytB != null then 1 else 0
    val cL = if legendB != null then 1 else 0
    val outer = Grid(rT + 1 + rX, cY + 1 + cL, colGap = 0, rowGap = 0, pad = 0)
    if titleB != null then { val _ = outer.put(0, 0, 0, cY + cL)(titleB) }
    if ytB != null then { val _ = outer.put(rT, 0)(ytB) }
    val _ = outer.put(rT, cY)(facetGrid)
    if legendB != null then { val _ = outer.put(rT, cY + 1)(legendB) }
    if xtB != null then { val _ = outer.put(rT + 1, cY)(xtB) }
    insets.foreach: ins =>
      val sub = buildFigure(ins.fig, estW * ins.w, estH * ins.h).?
      val _ = outer.putFloat(facetGrid)(ins.x, ins.y, ins.w, ins.h)(sub)
    outer

  private def emitGrid(g: Grid, lay: Grid.Layout, put: Glyph => Unit): Unit =
    var i = 0
    while i < g.blockCount do
      g.blockAt(i) match
        case sub: Grid =>
          val sl = lay.sub(i)
          if sl != null then emitGrid(sub, sl, put)
        case e: GlyphBlock => e.glyphs(lay.content(i), put)
        case _ => ()
      i += 1
    i = 0
    while i < g.floatCount do
      val fr = lay.floatRects(i)
      put(Glyph.Box(fr.x, fr.y, fr.w, fr.h, "#FFFFFF"))
      g.floatBlockAt(i) match
        case sub: Grid =>
          val sl = lay.floatSub(i)
          if sl != null then emitGrid(sub, sl, put)
        case e: GlyphBlock => e.glyphs(fr, put)
        case _ => ()
      i += 1

  private def solveAndRender(root: Grid, width: Double, height: Double): String =
    val lay = root.solve(width, height)
    val gs = List.newBuilder[Glyph]
    def put(g: Glyph): Unit = { gs += g; () }
    emitGrid(root, lay, put)
    Svg.render(width, height, gs.result())

  def figureSvg(fig: Figure, width: Double, height: Double)(using m: Measurer): Ask[String] = Ask:
    solveAndRender(buildFigure(fig, width, height).?, width, height)

  private def buildBoard(b: Board, estW: Double, estH: Double)(using m: Measurer): Ask[Grid] = Ask:
    b match
      case Board.One(f) => buildFigure(f, estW, estH).?
      case Board.Beside(items) =>
        val g = Grid(1, items.length, colGap = 4, rowGap = 4, pad = 0)
        val cw = jm.max(40.0, (estW - 4 * (items.length - 1)) / items.length)
        items.zipWithIndex.foreach: (it, i) =>
          val _ = g.put(0, i)(buildBoard(it, cw, estH).?)
        g
      case Board.Above(items) =>
        val g = Grid(items.length, 1, colGap = 4, rowGap = 4, pad = 0)
        val ch = jm.max(40.0, (estH - 4 * (items.length - 1)) / items.length)
        items.zipWithIndex.foreach: (it, i) =>
          val _ = g.put(i, 0)(buildBoard(it, estW, ch).?)
        g

  def boardSvg(b: Board, width: Double, height: Double)(using m: Measurer): Ask[String] = Ask:
    solveAndRender(buildBoard(b, width, height).?, width, height)


extension (fb: Figure | Board)
  /** Renders a figure or a figure composition to SVG text; see `Render` for what is
    * interpreted so far.
    */
  def svg(width: Double = 640, height: Double = 480)(using Measurer): Ask[String] = fb match
    case f: Figure => Render.figureSvg(f, width, height)
    case b: Board  => Render.boardSvg(b, width, height)
