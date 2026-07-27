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
    m: Measurer
  ) extends Block:
    private def xTicks(w: Double): Ticks = ticksIn(x0, x1, jm.max(2, jm.min(8, (w / 90).toInt)))
    private def yTicks(h: Double): Ticks = ticksIn(y0, y1, jm.max(2, jm.min(8, (h / 70).toInt)))

    def protrusions(w: Double, h: Double): Prot =
      val left =
        if showLeft then yTicks(h).labels.foldLeft(0.0)((mx, s) => jm.max(mx, m.width(s, labSz))) + tickLen + 8
        else 0.0
      val bottom = if showBottom then m.lineHeight(labSz) + tickLen + 6 else 0.0
      val top = if colStrip != null then m.lineHeight(labSz) + 4 else 0.0
      val right = if rowStrip != null then m.width(rowStrip, labSz) + 8 else 0.0
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
          put(Glyph.Segment(px, rect.bottom, px, rect.bottom + tickLen, "#555555", 1))
          put(Glyph.Txt(px, rect.bottom + tickLen + m.ascent(labSz) + 2, xL(i), labSz, "#333333", Glyph.Anchor.Middle))
          i += 1
      if showLeft then
        val yL = yt.labels
        var i = 0
        while i < yt.values.length do
          val py = sy(yt.values(i))
          put(Glyph.Segment(rect.x - tickLen, py, rect.x, py, "#555555", 1))
          put(Glyph.Txt(rect.x - tickLen - 4, py + m.ascent(labSz) * 0.38, yL(i), labSz, "#333333", Glyph.Anchor.End))
          i += 1
      if colStrip != null then
        put(Glyph.Txt(rect.x + rect.w / 2, rect.y - 5, colStrip, labSz, "#222222", Glyph.Anchor.Middle, bold = true))
      if rowStrip != null then
        put(Glyph.Txt(rect.right + 6, rect.y + rect.h / 2 + m.ascent(labSz) * 0.38, rowStrip, labSz, "#222222", Glyph.Anchor.Start, bold = true))
      slices.foreach: s =>
        val flat = if anyColourScale then neutral else palette(s.layerIdx % palette.length)
        s.kind match
          case Visual.Kind.Scatter =>
            var i = 0
            while i < s.xs.length do
              val fill = s.colorIdx match
                case null => flat
                case ci   => palette(ci(i) % palette.length)
              put(Glyph.Disc(sx(s.xs(i)), sy(s.ys(i)), 3.5, fill))
              i += 1
          case Visual.Kind.Line =>
            s.colorIdx match
              case null =>
                if s.xs.length >= 2 then put(Glyph.Polyline(s.xs.map(sx), s.ys.map(sy), flat, 1.8))
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
                    put(Glyph.Polyline(idx.map(i => sx(s.xs(i))).toArray, idx.map(i => sy(s.ys(i))).toArray, palette(lv % palette.length), 1.8))
                  lv += 1

  private final class LegendBlock(title: String | Null, levels: Array[String], m: Measurer) extends Block:
    private def innerWidth: Double =
      val titleW = if title == null then 0.0 else m.width(title, labSz)
      levels.foldLeft(titleW)((w, s) => jm.max(w, 11 + 6 + m.width(s, labSz))) + 16
    override def widthPref: Size = Size.Fixed(innerWidth)
    def protrusions(w: Double, h: Double): Prot = Prot.zero
    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      val lx = rect.x + 4
      var ly = rect.y + 2
      if title != null then
        put(Glyph.Txt(lx, ly + m.ascent(labSz), title, labSz, "#222222", Glyph.Anchor.Start, bold = true))
        ly += m.lineHeight(labSz) + 2
      var lv = 0
      while lv < levels.length do
        put(Glyph.Box(lx, ly + 2, 11, 11, palette(lv % palette.length)))
        put(Glyph.Txt(lx + 11 + 6, ly + 2 + m.ascent(labSz) * 0.95, levels(lv), labSz, "#333333", Glyph.Anchor.Start))
        ly += 17
        lv += 1

  private final class TitleBlock(text: String, m: Measurer) extends Block:
    override def heightPref: Size = Size.Fixed(m.lineHeight(titleSz) + 6)
    def protrusions(w: Double, h: Double): Prot = Prot.zero
    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      put(Glyph.Txt(rect.x + rect.w / 2, rect.y + m.ascent(titleSz) + 2, text, titleSz, "#222222", Glyph.Anchor.Middle, bold = true))

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
    if dxHi == dxLo then { dxLo -= jm.max(1.0, jm.abs(dxLo) * 0.05); dxHi += jm.max(1.0, jm.abs(dxHi) * 0.05) }
    if dyHi == dyLo then { dyLo -= jm.max(1.0, jm.abs(dyLo) * 0.05); dyHi += jm.max(1.0, jm.abs(dyHi) * 0.05) }
    val xPad = 0.04 * (dxHi - dxLo)
    val yPad = 0.04 * (dyHi - dyLo)
    val fx0 = if xLo.isNaN then dxLo - xPad else xLo
    val fx1 = if xHi.isNaN then dxHi + xPad else xHi
    val fy0 = if yLo.isNaN then dyLo - yPad else yLo
    val fy1 = if yHi.isNaN then dyHi + yPad else yHi

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

    val facetGrid = Grid(nR, nC, colGap = 12, rowGap = 12, pad = 6)
    val panels = collection.mutable.ArrayBuffer.empty[Panel]
    var r = 0
    while r < nR do
      var c = 0
      while c < nC do
        val cl = colLevels(c)
        val rl = rowLevels(r)
        val slices = prepped.map(p => sliceFor(p, cl, rl))
        val pan = Panel(
          slices, fx0, fx1, fy0, fy1,
          anyColourScale = levels.nonEmpty,
          showLeft = c == 0, showBottom = r == nR - 1,
          colStrip = if r == 0 then cl else null,
          rowStrip = if c == nC - 1 then rl else null,
          m
        )
        val _ = panels.addOne(pan)
        val _ = facetGrid.put(r, c)(pan)
        c += 1
      r += 1

    val legendB = if levels.nonEmpty then LegendBlock(title, levels.toArray, m) else null
    val titleB = if levels.isEmpty && title != null then TitleBlock(title, m) else null

    val gs = List.newBuilder[Glyph]
    def put(g: Glyph): Unit = { gs += g; () }

    // outer composition: content cell plus optional legend column / title row
    if legendB != null then
      val outer = Grid(1, 2, colGap = 0, rowGap = 0, pad = 0)
      val _ = outer.put(0, 0)(facetGrid).put(0, 1)(legendB)
      val lay = outer.solve(width, height)
      val fl = lay.sub(0)
      if fl == null then Err.break("internal: facet grid did not solve")
      var i = 0
      while i < panels.length do
        panels(i).glyphs(fl.content(i), put)
        i += 1
      legendB.glyphs(lay.content(1), put)
    else if titleB != null then
      val outer = Grid(2, 1, colGap = 0, rowGap = 0, pad = 0)
      val _ = outer.put(0, 0)(titleB).put(1, 0)(facetGrid)
      val lay = outer.solve(width, height)
      val fl = lay.sub(1)
      if fl == null then Err.break("internal: facet grid did not solve")
      titleB.glyphs(lay.content(0), put)
      var i = 0
      while i < panels.length do
        panels(i).glyphs(fl.content(i), put)
        i += 1
    else
      val fl = facetGrid.solve(width, height)
      var i = 0
      while i < panels.length do
        panels(i).glyphs(fl.content(i), put)
        i += 1

    Svg.render(width, height, gs.result())


extension (fig: Figure)
  /** Renders the figure to SVG text; see `Render` for what is interpreted so far. */
  def svg(width: Double = 640, height: Double = 480)(using Measurer): Ask[String] =
    Render.figureSvg(fig, width, height)
