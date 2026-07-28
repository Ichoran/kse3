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
  * facet columns appear in every panel.  Stats (smoothers, bin/density/count) run per
  * (colour level × facet cell) group upstream of scale resolution; colour is discrete
  * (swatch legend) or continuous (viridis ramp + colorbar), never both in one figure.
  * Whatever is not really interpreted still refuses loudly rather than render a lie.
  */
object Render:
  /** Okabe–Ito colorblind-safe palette, blue first. */
  val palette: Array[String] = Array("#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442", "#000000")

  /** Colour for unmapped layers when a colour scale is in use by other layers. */
  val neutral: String = "#606060"

  // viridis (matplotlib's exact table sampled every 1/32), linearly interpolated between anchors
  private val viridis: Array[Int] = Array(
    0x440154, 0x470D60, 0x48186A, 0x482374, 0x472D7B, 0x453781, 0x424086, 0x3E4989, 0x3B528B, 0x375B8D, 0x33638D,
    0x2F6B8E, 0x2C728E, 0x297A8E, 0x26828E, 0x23898E, 0x21918C, 0x1F978B, 0x1F9F88, 0x21A685, 0x27AD81, 0x31B57B,
    0x3DBC74, 0x4CC26C, 0x5CC863, 0x6ECE58, 0x81D34D, 0x95D840, 0xAADC32, 0xC0DF25, 0xD5E21A, 0xEAE51A, 0xFDE725
  )

  private def rampColour(t: Double): String =
    val c = jm.max(0.0, jm.min(1.0, t)) * 32
    val i = jm.min(31, jm.floor(c).toInt)
    val f = c - i
    val lo = viridis(i)
    val hi = viridis(i + 1)
    inline def ch(sh: Int): Int = jm.round((1 - f) * ((lo >> sh) & 0xFF) + f * ((hi >> sh) & 0xFF)).toInt
    String.format("#%02X%02X%02X", ch(16), ch(8), ch(0))

  /** The figure's one colour scale: discrete palette levels, a continuous ramp, or none. */
  private enum Hue:
    case Off
    case Levels(n: Int)
    case Ramp(lo: Double, hi: Double)

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

  /** One layer's data resolved to numbers: x always present, the y channels per visual
    * (Band uses yLo/yHi, everything else ys), colour either discrete indices or continuous
    * values, facet labels along for the ride.
    */
  private final case class Prep(
    xs: Array[Double], ys: Array[Double] | Null,
    yLo: Array[Double] | Null, yHi: Array[Double] | Null,
    colorIdx: Array[Int] | Null, colorVal: Array[Double] | Null,
    styled: String | Null,
    kind: Visual.Kind, layerIdx: Int,
    colLabs: Array[String] | Null, rowLabs: Array[String] | Null
  )

  private final case class Slice(
    xs: Array[Double], ys: Array[Double] | Null,
    yLo: Array[Double] | Null, yHi: Array[Double] | Null,
    colorIdx: Array[Int] | Null, colorVal: Array[Double] | Null,
    styled: String | Null,
    kind: Visual.Kind, layerIdx: Int
  )

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
    if cnt == n then Slice(p.xs, p.ys, p.yLo, p.yHi, p.colorIdx, p.colorVal, p.styled, p.kind, p.layerIdx)
    else
      inline def pick(src: Array[Double]): Array[Double] =
        val a = new Array[Double](cnt)
        var j = 0
        var k = 0
        while k < n do
          if keep(k) then
            a(j) = src(k)
            j += 1
          k += 1
        a
      val xs = pick(p.xs)
      val ys = if p.ys == null then null else pick(p.ys)
      val lo = if p.yLo == null then null else pick(p.yLo)
      val hi = if p.yHi == null then null else pick(p.yHi)
      val cv = if p.colorVal == null then null else pick(p.colorVal)
      val ci = if p.colorIdx == null then null else
        val a = new Array[Int](cnt)
        var j = 0
        var k = 0
        while k < n do
          if keep(k) then
            a(j) = p.colorIdx(k)
            j += 1
          k += 1
        a
      Slice(xs, ys, lo, hi, ci, cv, p.styled, p.kind, p.layerIdx)

  private def grid(lo: Double, hi: Double, n: Int): Array[Double] =
    if hi <= lo then Array(lo)
    else Array.tabulate(n)(i => lo + (hi - lo) * i / (n - 1).toDouble)

  ///////////////////////////
  /// Stat interpretation ///
  ///////////////////////////

  /** One stat group: colour level (or -1) crossed with facet cell (or null). */
  private final case class GroupKey(ci: Int, cl: String | Null, rl: String | Null)

  private def groupsOf(p: Prep): (Array[GroupKey], Array[Array[Int]]) =
    val keys = collection.mutable.ArrayBuffer.empty[GroupKey]
    val members = collection.mutable.ArrayBuffer.empty[collection.mutable.ArrayBuffer[Int]]
    var i = 0
    while i < p.xs.length do
      val k = GroupKey(
        if p.colorIdx == null then -1 else p.colorIdx(i),
        if p.colLabs == null then null else p.colLabs(i),
        if p.rowLabs == null then null else p.rowLabs(i))
      var g = keys.indexOf(k)
      if g < 0 then
        g = keys.length
        val _ = keys.addOne(k)
        val _ = members.addOne(collection.mutable.ArrayBuffer.empty[Int])
      val _ = members(g).addOne(i)
      i += 1
    (keys.toArray, members.map(_.toArray).toArray)

  /** Rebuilds a Prep from per-group output columns, replicating each group's colour level
    * and facet labels onto every output row — the grouping-column pass-through of the stat
    * contract.  Continuous colour never survives a stat (checked upstream).
    */
  private def reassemble(
    p: Prep, keys: Array[GroupKey],
    gx: Array[Array[Double]], gy: Array[Array[Double]] | Null,
    gLo: Array[Array[Double]] | Null, gHi: Array[Array[Double]] | Null
  ): Prep =
    var total = 0
    var g = 0
    while g < gx.length do
      total += gx(g).length
      g += 1
    val xs2 = new Array[Double](total)
    val ys2 = if gy == null then null else new Array[Double](total)
    val lo2 = if gLo == null then null else new Array[Double](total)
    val hi2 = if gHi == null then null else new Array[Double](total)
    val ci2 = if p.colorIdx == null then null else new Array[Int](total)
    val cl2 = if p.colLabs == null then null else new Array[String](total)
    val rl2 = if p.rowLabs == null then null else new Array[String](total)
    var o = 0
    g = 0
    while g < gx.length do
      val ex = gx(g)
      var k = 0
      while k < ex.length do
        xs2(o) = ex(k)
        if ys2 != null then ys2(o) = gy(g)(k)
        if lo2 != null then lo2(o) = gLo(g)(k)
        if hi2 != null then hi2(o) = gHi(g)(k)
        if ci2 != null then ci2(o) = keys(g).ci
        if cl2 != null then cl2(o) = keys(g).cl
        if rl2 != null then rl2(o) = keys(g).rl
        o += 1
        k += 1
      g += 1
    Prep(xs2, ys2, lo2, hi2, ci2, null, p.styled, p.kind, p.layerIdx, cl2, rl2)

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

  /** Applies a smoother per group to every y channel the layer carries; grouping columns
    * pass through so colour and facet assignments stay consistent on the smoothed output.
    */
  private def smoothPrep(p: Prep, how: Smoother): Prep =
    if p.xs.length == 0 then return p
    val (keys, members) = groupsOf(p)
    val ng = keys.length
    val gx = new Array[Array[Double]](ng)
    val gy = if p.ys == null then null else new Array[Array[Double]](ng)
    val gl = if p.yLo == null then null else new Array[Array[Double]](ng)
    val gh = if p.yHi == null then null else new Array[Array[Double]](ng)
    var g = 0
    while g < ng do
      val order = members(g).sortBy(p.xs(_))
      val sx = order.map(p.xs(_))
      def run(src: Array[Double]): Array[Double] =
        val (ax, ay) = applySmoother(how, sx, order.map(src(_)))
        gx(g) = ax
        ay
      if gy != null then gy(g) = run(p.ys)
      if gl != null then gl(g) = run(p.yLo)
      if gh != null then gh(g) = run(p.yHi)
      if gx(g) == null then gx(g) = sx
      g += 1
    reassemble(p, keys, gx, gy, gl, gh)

  /** Histogram counts.  All groups share one set of edges (dodged bars align), the width
    * snaps to a nice step so edges land on round numbers, and empty bins are kept so a
    * line or area over binned counts stays honest.  NaN x values are ignored.
    */
  private def binPrep(p: Prep, bins: Int): Prep =
    if p.xs.length == 0 then return p
    var lo = Double.PositiveInfinity
    var hi = Double.NegativeInfinity
    var i = 0
    while i < p.xs.length do
      val v = p.xs(i)
      if v < lo then lo = v
      if v > hi then hi = v
      i += 1
    if !(lo <= hi) then return p
    val width = if hi > lo then niceStep((hi - lo) / jm.max(1, bins)) else jm.max(1.0, jm.abs(lo) * 0.01)
    val e0 = jm.floor(lo / width + 1e-9) * width
    val nb = jm.max(1, jm.ceil((hi - e0) / width - 1e-9).toInt)
    val centers = Array.tabulate(nb)(b => e0 + (b + 0.5) * width)
    val (keys, members) = groupsOf(p)
    val gx = new Array[Array[Double]](keys.length)
    val gy = new Array[Array[Double]](keys.length)
    var g = 0
    while g < keys.length do
      val counts = new Array[Double](nb)
      members(g).foreach: k =>
        val v = p.xs(k)
        if !v.isNaN then
          // the epsilon keeps edge values in their own bin despite division rounding
          var b = jm.floor((v - e0) / width + 1e-9).toInt
          if b < 0 then b = 0
          if b >= nb then b = nb - 1
          counts(b) += 1
      gx(g) = centers
      gy(g) = counts
      g += 1
    reassemble(p, keys, gx, gy, null, null)

  /** Kernel density per group, evaluated on one shared grid covering every group's data
    * plus three bandwidths of tail, so overlaid curves share their extent.  Bandwidth is
    * per group when Silverman's rule chooses it.  NaN x values are ignored.
    */
  private def densityPrep(p: Prep, bandwidth: Double): Prep =
    if p.xs.length == 0 then return p
    val (keys, members) = groupsOf(p)
    val gxs = new Array[Array[Double]](keys.length)
    val bw = new Array[Double](keys.length)
    var bwMax = 0.0
    var lo = Double.PositiveInfinity
    var hi = Double.NegativeInfinity
    var g = 0
    while g < keys.length do
      val sx = members(g).map(p.xs(_)).filter(v => !v.isNaN)
      gxs(g) = sx  // an all-NaN group makes silvermanBandwidth throw "no data", caught into Err upstream
      var i = 0
      while i < sx.length do
        if sx(i) < lo then lo = sx(i)
        if sx(i) > hi then hi = sx(i)
        i += 1
      bw(g) = if bandwidth.isNaN then kse.maths.Smoothing.silvermanBandwidth(sx) else bandwidth
      if bw(g) > bwMax then bwMax = bw(g)
      g += 1
    val ex = grid(lo - 3 * bwMax, hi + 3 * bwMax, 120)
    val gx = new Array[Array[Double]](keys.length)
    val gy = new Array[Array[Double]](keys.length)
    g = 0
    while g < keys.length do
      gx(g) = ex
      gy(g) = kse.maths.Smoothing.kdeAt(gxs(g), ex, bw(g))
      g += 1
    reassemble(p, keys, gx, gy, null, null)

  /** Occurrence counts of each distinct x value, per group, x ascending; zero counts are
    * not emitted.  NaN x values are ignored.
    */
  private def countPrep(p: Prep): Prep =
    if p.xs.length == 0 then return p
    val distinct = p.xs.filter(v => !v.isNaN).distinct.sorted
    val index = collection.mutable.HashMap.empty[Double, Int]
    var i = 0
    while i < distinct.length do
      index(distinct(i)) = i
      i += 1
    val (keys, members) = groupsOf(p)
    val gx = new Array[Array[Double]](keys.length)
    val gy = new Array[Array[Double]](keys.length)
    var g = 0
    while g < keys.length do
      val counts = new Array[Int](distinct.length)
      members(g).foreach: k =>
        val v = p.xs(k)
        if !v.isNaN then counts(index(v)) += 1
      var nz = 0
      i = 0
      while i < counts.length do
        if counts(i) > 0 then nz += 1
        i += 1
      val cx = new Array[Double](nz)
      val cy = new Array[Double](nz)
      var j = 0
      i = 0
      while i < counts.length do
        if counts(i) > 0 then
          cx(j) = distinct(i)
          cy(j) = counts(i)
          j += 1
        i += 1
      gx(g) = cx
      gy(g) = cy
      g += 1
    reassemble(p, keys, gx, gy, null, null)

  private def statName(st: Stat): String = st match
    case Smooth(_)  => "smooth()"
    case Bin(_)     => "bin()"
    case Density(_) => "density()"
    case Count      => "count"

  /** Runs a layer's stats in declared order.  The distribution stats consume x and refuse
    * a mapped y; no stat can carry continuous colour through (there is no level to group
    * by), so that combination refuses too.
    */
  private def statted(p: Prep, stats: List[Stat], li: Int): Ask[Prep] = Ask:
    var q = p
    stats.foreach: st =>
      if q.colorVal != null then
        Err.break(s"layer ${li + 1}: ${statName(st)} cannot carry continuous colour through; use discrete colour to group the output")
      st match
        case Smooth(how) => q = smoothPrep(q, how)
        case other =>
          if q.ys != null || q.yLo != null || q.yHi != null then
            Err.break(s"layer ${li + 1}: ${statName(other)} computes 'y' from the x values; remove the layer's y mapping")
          other match
            case Bin(bins)   => q = binPrep(q, bins)
            case Density(bw) => q = densityPrep(q, bw)
            case _           => q = countPrep(q)
    q

  //////////////////////////
  /// Blocks and drawing ///
  //////////////////////////

  /** One data panel: axis decorations and facet strips are protrusions; the content rect
    * is pure data area.  Tick density adapts to the granted size, so panels degrade
    * gracefully when small; equal panel sizes plus shared domains give identical ticks
    * across a facet grid.
    */
  private final class Panel(
    slices: List[Slice],
    x0: Double, x1: Double, y0: Double, y1: Double,
    hue: Hue,
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

    private def levelCount: Int = hue match
      case Hue.Levels(n) => n
      case _             => 1

    private def flatColour(s: Slice): String =
      if s.styled != null then s.styled
      else hue match
        case Hue.Off => palette(s.layerIdx % palette.length)
        case _       => neutral

    private def pointColour(s: Slice, i: Int): String =
      if s.colorIdx != null then palette(s.colorIdx(i) % palette.length)
      else if s.colorVal != null then
        hue match
          case Hue.Ramp(lo, hi) =>
            val v = s.colorVal(i)
            if v.isNaN then neutral
            else rampColour(if hi > lo then (v - lo) / (hi - lo) else 0.5)
          case _ => flatColour(s)
      else flatColour(s)

    /** Indices of this slice at colour level `lv` (or everything when unmapped), ascending
      * by x — the geometry order for bands, areas, and bars.
      */
    private def levelIdx(s: Slice, lv: Int): Array[Int] =
      val all =
        if s.colorIdx == null then Array.range(0, s.xs.length)
        else
          val b = collection.mutable.ArrayBuffer.empty[Int]
          var i = 0
          while i < s.xs.length do
            if s.colorIdx(i) == lv then { val _ = b.addOne(i) }
            i += 1
          b.toArray
      all.sortBy(s.xs(_))

    private def presentLevels(s: Slice): Array[Int] =
      if s.colorIdx == null then Array(-1)
      else s.colorIdx.distinct.sorted

    private def drawSlice(s: Slice, rect: Rect, sx: Double => Double, sy: Double => Double, put: Glyph => Unit): Unit =
      val flat = flatColour(s)
      s.kind match
        case Visual.Kind.Scatter =>
          var i = 0
          while i < s.xs.length do
            put(Glyph.Disc(sx(s.xs(i)), sy(s.ys(i)), 3.5 * fs, pointColour(s, i)))
            i += 1
        case Visual.Kind.Line =>
          s.colorIdx match
            case null =>
              if s.xs.length >= 2 then put(Glyph.Polyline(s.xs.map(sx), s.ys.map(sy), flat, jm.max(0.8, 1.8 * fs)))
            case ci =>
              presentLevels(s).foreach: lv =>
                val idx = (0 until s.xs.length).filter(i => ci(i) == lv)
                if idx.length >= 2 then
                  put(Glyph.Polyline(idx.map(i => sx(s.xs(i))).toArray, idx.map(i => sy(s.ys(i))).toArray, palette(lv % palette.length), jm.max(0.8, 1.8 * fs)))
        case Visual.Kind.Band =>
          presentLevels(s).foreach: lv =>
            val idx = levelIdx(s, lv)
            if idx.length >= 2 then
              val n = idx.length
              val px = new Array[Double](2 * n)
              val py = new Array[Double](2 * n)
              var i = 0
              while i < n do
                px(i) = sx(s.xs(idx(i)))
                py(i) = sy(s.yHi(idx(i)))
                px(n + i) = sx(s.xs(idx(n - 1 - i)))
                py(n + i) = sy(s.yLo(idx(n - 1 - i)))
                i += 1
              val fill = if lv >= 0 then palette(lv % palette.length) else flat
              put(Glyph.Poly(px, py, fill, 0.25))
        case Visual.Kind.Area =>
          val base = jm.min(rect.bottom, jm.max(rect.y, sy(0.0)))
          presentLevels(s).foreach: lv =>
            val idx = levelIdx(s, lv)
            if idx.length >= 2 then
              val n = idx.length
              val px = new Array[Double](n + 2)
              val py = new Array[Double](n + 2)
              px(0) = sx(s.xs(idx(0)))
              py(0) = base
              var i = 0
              while i < n do
                px(i + 1) = sx(s.xs(idx(i)))
                py(i + 1) = sy(s.ys(idx(i)))
                i += 1
              px(n + 1) = sx(s.xs(idx(n - 1)))
              py(n + 1) = base
              val fill = if lv >= 0 then palette(lv % palette.length) else flat
              put(Glyph.Poly(px, py, fill, 0.4))
              put(Glyph.Polyline(java.util.Arrays.copyOfRange(px, 1, n + 1), java.util.Arrays.copyOfRange(py, 1, n + 1), fill, jm.max(0.9, 1.4 * fs)))
        case Visual.Kind.Bar =>
          val distinct = s.xs.distinct.sorted
          var gap = Double.PositiveInfinity
          var i = 1
          while i < distinct.length do
            val d = distinct(i) - distinct(i - 1)
            if d > 0 && d < gap then gap = d
            i += 1
          if !(gap < Double.PositiveInfinity) then gap = (x1 - x0) / 10.0
          val full = 0.9 * gap
          val nLev = if s.colorIdx == null then 1 else levelCount
          val slot = full / nLev
          val pxPerX = rect.w / (x1 - x0)
          val base = jm.min(rect.bottom, jm.max(rect.y, sy(0.0)))
          i = 0
          while i < s.xs.length do
            if s.ys(i) != 0.0 then
              val lv = if s.colorIdx == null then 0 else s.colorIdx(i)
              val xL = sx(s.xs(i) - full / 2 + lv * slot)
              val yp = sy(s.ys(i))
              val top = jm.min(yp, base)
              val hgt = jm.abs(yp - base)
              val fill = if s.colorIdx == null then flat else palette(lv % palette.length)
              if hgt > 0.01 then put(Glyph.Box(xL, top, jm.max(0.5, slot * pxPerX), hgt, fill))
            i += 1

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
      slices.foreach(s => drawSlice(s, rect, sx, sy, put))

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

  /** Continuous colour guide: a vertical viridis gradient with ticks, high values up. */
  private final class ColorbarBlock(title: String | Null, lo: Double, hi: Double, fs: Double, m: Measurer) extends GlyphBlock:
    private val lab = labSz * fs
    private val barW = 12.0 * fs
    private def ticks: Ticks = ticksIn(lo, hi, 5)
    private def innerWidth: Double =
      val labelW = if hi > lo then ticks.labels.foldLeft(0.0)((w, s) => jm.max(w, m.width(s, lab))) else 0.0
      val titleW = if title == null then 0.0 else m.width(title, lab)
      jm.max(barW + tickLen * fs + 7 + labelW, titleW) + 16 * fs
    override def widthPref: Size = Size.Fixed(innerWidth)
    def protrusions(w: Double, h: Double): Prot = Prot.zero
    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      val lx = rect.x + 4
      var ty = rect.y + 2
      if title != null then
        put(Glyph.Txt(lx, ty + m.ascent(lab), title, lab, "#222222", Glyph.Anchor.Start, bold = true))
        ty += m.lineHeight(lab) + 4
      val bh = jm.max(20.0, rect.bottom - 6 - ty)
      val n = 64
      var k = 0
      while k < n do
        val t0 = k.toDouble / n
        val t1 = (k + 1).toDouble / n
        // slabs overlap slightly so antialiasing cannot leave hairline seams
        put(Glyph.Box(lx, ty + bh * (1 - t1) - 0.25, barW, bh / n + 0.5, rampColour((t0 + t1) / 2)))
        k += 1
      put(Glyph.Segment(lx, ty, lx, ty + bh, "#555555", 0.75))
      put(Glyph.Segment(lx + barW, ty, lx + barW, ty + bh, "#555555", 0.75))
      put(Glyph.Segment(lx, ty, lx + barW, ty, "#555555", 0.75))
      put(Glyph.Segment(lx, ty + bh, lx + barW, ty + bh, "#555555", 0.75))
      if hi > lo then
        val tk = ticks
        val labs = tk.labels
        var i = 0
        while i < tk.values.length do
          val yv = ty + bh * (1 - (tk.values(i) - lo) / (hi - lo))
          put(Glyph.Segment(lx + barW, yv, lx + barW + tickLen * fs, yv, "#555555", 1))
          put(Glyph.Txt(lx + barW + tickLen * fs + 3, yv + m.ascent(lab) * 0.38, labs(i), lab, "#333333", Glyph.Anchor.Start))
          i += 1

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

  ////////////////////////////
  /// Figure interpretation ///
  ////////////////////////////

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
    inline def sweep(a: Array[Double] | Null): Unit =
      if a != null then
        var i = 0
        while i < a.length do
          if a(i) < lo then lo = a(i)
          if a(i) > hi then hi = a(i)
          i += 1
    slices.foreach: s =>
      if horz then sweep(s.xs)
      else
        sweep(s.ys)
        sweep(s.yLo)
        sweep(s.yHi)
        if s.kind == Visual.Kind.Bar || s.kind == Visual.Kind.Area then
          if lo > 0 then lo = 0.0
          if hi < 0 then hi = 0.0
    if !(lo <= hi) then (fb0, fb1) else axisSpan(lo, hi, cfgLo, cfgHi)

  /** Builds a figure's full block tree — facet grid of panels, legend or colorbar, titles,
    * insets — rooted in one outer grid, ready to solve at any size.
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
      // when the user names no visual, the last stat picks a sensible one
      val kind = layer.look.visual match
        case null =>
          layer.look.stats.lastOption match
            case Some(Smooth(_))            => Visual.Kind.Line
            case Some(Bin(_)) | Some(Count) => Visual.Kind.Bar
            case Some(Density(_))           => Visual.Kind.Area
            case _                          => Visual.Kind.Scatter
        case v: Visual => v.kind
      val names = layer.data.names
      def channel(name: String): Array[Double] | Null =
        layer.data.fields.find(_.name == name) match
          case Some(f) => numbersOf(f.column, s"layer ${li + 1} aesthetic '$name'").?
          case None    => null
      val ys = channel("y")
      val yLo = channel("ylow")
      val yHi = channel("yhigh")
      val xs = channel("x") match
        case a: Array[Double] => a
        case null =>
          layer.look.stats.find(st => !st.isInstanceOf[Smooth]) match
            case Some(st) => Err.break(s"layer ${li + 1}: ${statName(st)} needs aesthetic 'x'; it has [${names.mkString(", ")}]")
            case None => ()
          val ref = if ys != null then ys else if yHi != null then yHi else yLo
          ref match
            case a: Array[Double] => Array.tabulate(a.length)(_.toDouble)  // default: observation index
            case null => Err.break(s"layer ${li + 1} needs aesthetic 'y'; it has [${names.mkString(", ")}]")
      var colorIdx: Array[Int] | Null = null
      var colorVal: Array[Double] | Null = null
      layer.data.fields.find(_.name == "color") match
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
              colorIdx = out
            case _: ScaleOf.AsIdentity[?] =>
              Err.break(s"layer ${li + 1}: identity colour columns are not interpreted yet")
            case _ =>
              colorVal = numbersOf(f.column, s"layer ${li + 1} aesthetic 'color'").?
        case None => ()
      val colLabs = layer.data.fields.find(_.name == "col") match
        case Some(f) => labelsOf(f.column, s"layer ${li + 1} facet 'col'").?
        case None    => null
      val rowLabs = layer.data.fields.find(_.name == "row") match
        case Some(f) => labelsOf(f.column, s"layer ${li + 1} facet 'row'").?
        case None    => null
      // rightmost styled constant wins, per the cascade; a mapped colour column still beats it
      var styled: String | Null = null
      layer.look.style.entries.foreach: (k, v) =>
        if k eq Style.Color then styled = v.asInstanceOf[String]
      val raw = Prep(xs, ys, yLo, yHi, colorIdx, colorVal, styled, kind, li, colLabs, rowLabs)
      // stats run upstream of scale resolution, so domains cover the transformed data
      val p = if layer.look.stats.isEmpty then raw else statted(raw, layer.look.stats, li).?
      // aesthetic completeness per visual, checked after stats have had their say
      p.kind match
        case Visual.Kind.Band =>
          if p.yLo == null || p.yHi == null then
            Err.break(s"layer ${li + 1} with visual Band needs aesthetics 'ylow' and 'yhigh'; it has [${names.mkString(", ")}]")
        case k =>
          if p.ys == null then Err.break(s"layer ${li + 1} needs aesthetic 'y'; it has [${names.mkString(", ")}]")
      if p.colorVal != null && p.kind != Visual.Kind.Scatter then
        Err.break(s"layer ${li + 1}: continuous colour is interpreted on Scatter only so far; use discrete colour for ${p.kind}")
      p

    // one colour scale per figure: discrete levels or a continuous ramp, never both
    var cLo = Double.PositiveInfinity
    var cHi = Double.NegativeInfinity
    prepped.foreach: p =>
      if p.colorVal != null then
        var i = 0
        while i < p.colorVal.length do
          val v = p.colorVal(i)
          if v < cLo then cLo = v
          if v > cHi then cHi = v
          i += 1
    val contColour = cLo <= cHi
    if contColour && levels.nonEmpty then
      Err.break("one colour scale per figure: some layers map discrete colour and others continuous")
    val hue: Hue =
      if levels.nonEmpty then Hue.Levels(levels.length)
      else if contColour then Hue.Ramp(cLo, cHi)
      else Hue.Off

    // shared scales: one x and one y domain across every panel
    var dxLo = Double.PositiveInfinity
    var dxHi = Double.NegativeInfinity
    var dyLo = Double.PositiveInfinity
    var dyHi = Double.NegativeInfinity
    inline def dySweep(a: Array[Double] | Null): Unit =
      if a != null then
        var i = 0
        while i < a.length do
          if a(i) < dyLo then dyLo = a(i)
          if a(i) > dyHi then dyHi = a(i)
          i += 1
    prepped.foreach: p =>
      var i = 0
      while i < p.xs.length do
        if p.xs(i) < dxLo then dxLo = p.xs(i)
        if p.xs(i) > dxHi then dxHi = p.xs(i)
        i += 1
      dySweep(p.ys)
      dySweep(p.yLo)
      dySweep(p.yHi)
      if p.kind == Visual.Kind.Bar || p.kind == Visual.Kind.Area then
        if dyLo > 0 then dyLo = 0.0
        if dyHi < 0 then dyHi = 0.0
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

    // resolve inset placements: exact rects pass through; compass anchors compute their
    // rect; auto placement scores the corners by data occupancy and takes the emptiest
    val mgIn = 0.025
    val placedInsets = collection.mutable.ArrayBuffer.empty[(Figure, Double, Double, Double, Double)]

    def compassRect(cp: Compass, w: Double, h: Double): (Double, Double) = cp match
      case "nw" => (mgIn, mgIn)
      case "n"  => ((1 - w) / 2, mgIn)
      case "ne" => (1 - w - mgIn, mgIn)
      case "e"  => (1 - w - mgIn, (1 - h) / 2)
      case "se" => (1 - w - mgIn, 1 - h - mgIn)
      case "s"  => ((1 - w) / 2, 1 - h - mgIn)
      case "sw" => (mgIn, 1 - h - mgIn)
      case "w"  => (mgIn, (1 - h) / 2)

    def insetOccupancy(rx: Double, ry: Double, rw: Double, rh: Double): Double =
      var s = 0.0
      placedInsets.foreach: (_, px, py, pw, ph) =>
        if rx < px + pw && px < rx + rw && ry < py + ph && py < ry + rh then s += 1e9
      prepped.foreach: p =>
        inline def countChannel(ya: Array[Double] | Null): Unit =
          if ya != null then
            var i = 0
            while i < p.xs.length do
              val xf = (p.xs(i) - fx0) / (fx1 - fx0)
              val yf = 1 - (ya(i) - fy0) / (fy1 - fy0)
              val c0 = if p.colLabs == null then 0 else jm.max(0, colLevels.indexOf(p.colLabs(i)))
              val c1 = if p.colLabs == null then nC - 1 else c0
              val r0 = if p.rowLabs == null then 0 else jm.max(0, rowLevels.indexOf(p.rowLabs(i)))
              val r1 = if p.rowLabs == null then nR - 1 else r0
              var cc = c0
              while cc <= c1 do
                var rr = r0
                while rr <= r1 do
                  val gx = (cc + xf) / nC
                  val gy = (rr + yf) / nR
                  if gx >= rx && gx <= rx + rw && gy >= ry && gy <= ry + rh then s += 1
                  rr += 1
                cc += 1
              i += 1
        countChannel(p.ys)
        countChannel(p.yLo)
        countChannel(p.yHi)
      s

    insets.foreach: ins =>
      ins.place match
        case Place.Exact(x, y, w, h) =>
          val _ = placedInsets.addOne((ins.fig, x, y, w, h))
        case Place.At(cp, w, h) =>
          val (x, y) = compassRect(cp, w, h)
          val _ = placedInsets.addOne((ins.fig, x, y, w, h))
        case Place.Auto(w, h) =>
          val cands: List[Compass] = List("nw", "ne", "sw", "se")
          var best: Compass = "nw"
          var bestScore = Double.PositiveInfinity
          var k = 0
          cands.foreach: cp =>
            val (cx, cy) = compassRect(cp, w, h)
            val sc = insetOccupancy(cx, cy, w, h) + k * 1e-6
            if sc < bestScore then
              bestScore = sc
              best = cp
            k += 1
          val (x, y) = compassRect(best, w, h)
          val _ = placedInsets.addOne((ins.fig, x, y, w, h))

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
          hue,
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

    val legendB: Block | Null =
      if levels.nonEmpty then LegendBlock(legTitle, levels.toArray, fs, m)
      else if contColour then ColorbarBlock(legTitle, cLo, cHi, fs, m)
      else null
    // legend(...) doubles as the figure title when no guide is drawn; title(...) always wins
    val topText: String | Null = if figTitle != null then figTitle else if legendB == null then legTitle else null
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
    placedInsets.foreach: (sfig, x, y, w, h) =>
      val sub = buildFigure(sfig, estW * w, estH * h).?
      val _ = outer.putFloat(facetGrid)(x, y, w, h)(sub)
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
