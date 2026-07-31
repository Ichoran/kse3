// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.eyes


import java.lang.{Math => jm}

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

  /** A panel annotation: a placed callout, or a user-anchored arrow. */
  private enum Mark:
    case Noted(text: String, at: NoteAt, backoff: Double, radius: Double, shape: ArrowShape)
    case Arrowed(label: String, x1: Double, y1: Double, x2: Double, y2: Double,
                 backoff: Double, radius: Double, colour: String, alpha: Double, shape: ArrowShape)

  // The type scale: a major-third ladder (steps of 1.25x) on the tick-label base — the
  // same proportions as ggplot2's defaults (axis titles 1.25x tick text, figure title
  // ~1.56x = 1.25 squared).  The half-step rung 1.25^1.5 ~ 1.4x is reserved for
  // subtitles when they land.  Hierarchy lives in these ratios, not absolute sizes, so
  // it survives fontScale shrinking figures up and down.
  private val labSz = 12.0
  private val axisTitleSz = labSz * 1.25
  private val titleSz = labSz * 1.5625
  private val tickLen = 4.0
  private val dotRad = 3.5

  // fewer points than this and a violin has no business bulging: the group's points
  // draw individually instead
  private val violinMin = 5

  /** Type size scale for a figure granted (w, h): an n-fold smaller figure gets sqrt(n)
    * smaller type, with the two dimension ratios combined by RMS so the larger dimension
    * dominates when shrinkage is uneven.  Clamped for legibility.
    */
  private def fontScale(w: Double, h: Double): Double =
    val sx = w / 640.0
    val sy = h / 480.0
    val s = jm.sqrt((sx * sx + sy * sy) / 2)
    jm.min(1.4, jm.max(0.5, jm.sqrt(s)))

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

  // Channel absence is a zero-length array, never null: a present channel always has one
  // value per row, so on any layer with rows the two cannot be confused, and every
  // consumer can index or iterate without a guard.  These are the canonical empties.
  private val noD = new Array[Double](0)
  private val noI = new Array[Int](0)
  private val noS = new Array[String](0)
  private val noDD = new Array[Array[Double]](0)

  /** One layer's data resolved to numbers: x always present (level indices when the x
    * axis is categorical), the y channels per visual (Band uses yLo/yHi, Boxplot the
    * whisker ends yMin/yMax too, everything else ys), `wd` the violin width channel,
    * colour either discrete indices or continuous values, facet labels along for the
    * ride.  A zero-length channel is absent; a present channel has one value per row.
    * `xIdx` is the x grouping (category level or bin index) when x is grouped, and
    * `slotW` the data-space pitch one group occupies (NaN = derive from spacing);
    * `xRep` is each row's group representative position when that differs from its
    * literal x (binned x) — summaries draw at the representative, but each row keeps its
    * literal x so outliers plot where the data actually is.  `styled` is the constant
    * colour, empty when unset.
    */
  private final case class Prep(
    xs: Array[Double], ys: Array[Double],
    yLo: Array[Double], yHi: Array[Double],
    yMin: Array[Double], yMax: Array[Double],
    xEnd: Array[Double], yEnd: Array[Double],
    wd: Array[Double], xRep: Array[Double],
    xIdx: Array[Int], slotW: Double,
    colorIdx: Array[Int], colorVal: Array[Double],
    styled: String, edge: EdgeStyle,
    kind: Visual.Kind, layerIdx: Int,
    colLabs: Array[String], rowLabs: Array[String]
  )

  private final case class Slice(
    xs: Array[Double], ys: Array[Double],
    yLo: Array[Double], yHi: Array[Double],
    yMin: Array[Double], yMax: Array[Double],
    xEnd: Array[Double], yEnd: Array[Double],
    wd: Array[Double],
    xIdx: Array[Int], slotW: Double,
    colorIdx: Array[Int], colorVal: Array[Double],
    styled: String, edge: EdgeStyle,
    kind: Visual.Kind, layerIdx: Int
  )

  /** Minor-tick treatment for one axis: unlabeled marks (default on), faint grid (default off). */
  private final case class Minor(ticks: Boolean, grid: Boolean)

  /** Ink for one axis's frame line and tick marks. */
  private final case class Ink(colour: String, alpha: Double)
  private val plainInk = Ink("#555555", 1.0)

  // half the frame stroke width: ticks start here (past the frame, not on top of it) and
  // gridlines stop here, so translucent axis ink composites exactly once everywhere
  private val axisHalf = 0.5

  // minor tick marks protrude 1/sqrt(2) as far as the majors and stroke 1/sqrt(2) as wide
  private val minorFrac = 1.0 / jm.sqrt(2.0)

  /** Resolved per-layer geometry style for Segment/Arrow layers. */
  private final case class EdgeStyle(shape: ArrowShape, curve: Double, alpha: Double, backoff: Double)

  private val plainEdge = EdgeStyle(ArrowShape(), Double.NaN, 1.0, Double.NaN)

  /** Slices a layer's rows to one facet cell; `clOn`/`rlOn` say whether the figure facets
    * that dimension at all, so a genuine empty-string level still matches by equality.
    */
  private def sliceFor(p: Prep, cl: String, clOn: Boolean, rl: String, rlOn: Boolean): Slice =
    inline def keep(i: Int): Boolean =
      (!clOn || p.colLabs.length == 0 || p.colLabs(i) == cl) &&
      (!rlOn || p.rowLabs.length == 0 || p.rowLabs(i) == rl)
    val n = p.xs.length
    var cnt = 0
    var i = 0
    while i < n do
      if keep(i) then cnt += 1
      i += 1
    if cnt == n then Slice(p.xs, p.ys, p.yLo, p.yHi, p.yMin, p.yMax, p.xEnd, p.yEnd, p.wd,
                           p.xIdx, p.slotW, p.colorIdx, p.colorVal, p.styled, p.edge, p.kind, p.layerIdx)
    else
      inline def pick(src: Array[Double]): Array[Double] =
        if src.length == 0 then src
        else
          val a = new Array[Double](cnt)
          var j = 0
          var k = 0
          while k < n do
            if keep(k) then
              a(j) = src(k)
              j += 1
            k += 1
          a
      inline def pickI(src: Array[Int]): Array[Int] =
        if src.length == 0 then src
        else
          val a = new Array[Int](cnt)
          var j = 0
          var k = 0
          while k < n do
            if keep(k) then
              a(j) = src(k)
              j += 1
            k += 1
          a
      Slice(pick(p.xs), pick(p.ys), pick(p.yLo), pick(p.yHi), pick(p.yMin), pick(p.yMax),
            pick(p.xEnd), pick(p.yEnd), pick(p.wd), pickI(p.xIdx), p.slotW,
            pickI(p.colorIdx), pick(p.colorVal), p.styled, p.edge, p.kind, p.layerIdx)

  private def grid(lo: Double, hi: Double, n: Int): Array[Double] =
    if hi <= lo then Array(lo)
    else Array.tabulate(n)(i => lo + (hi - lo) * i / (n - 1).toDouble)

  ///////////////////////////
  /// Stat interpretation ///
  ///////////////////////////

  /** One stat group: x group (level or bin, -1 = ungrouped) crossed with colour level
    * (-1 = unmapped) and facet cell ("" = unfaceted; a layer's facet channel is present
    * for all rows or none, so no collision with a real empty-string level is possible).
    */
  private final case class GroupKey(xi: Int, ci: Int, cl: String, rl: String)

  private def groupsOf(p: Prep): (Array[GroupKey], Array[Array[Int]]) =
    val keys = collection.mutable.ArrayBuffer.empty[GroupKey]
    val members = collection.mutable.ArrayBuffer.empty[collection.mutable.ArrayBuffer[Int]]
    var i = 0
    while i < p.xs.length do
      val k = GroupKey(
        if p.xIdx.length == 0 then -1 else p.xIdx(i),
        if p.colorIdx.length == 0 then -1 else p.colorIdx(i),
        if p.colLabs.length == 0 then "" else p.colLabs(i),
        if p.rowLabs.length == 0 then "" else p.rowLabs(i))
      var g = keys.indexOf(k)
      if g < 0 then
        g = keys.length
        val _ = keys.addOne(k)
        val _ = members.addOne(collection.mutable.ArrayBuffer.empty[Int])
      val _ = members(g).addOne(i)
      i += 1
    (keys.toArray, members.map(_.toArray).toArray)

  /** Rebuilds a Prep from per-group output columns (zero-length outer array = channel not
    * produced), replicating each group's x group, colour level, and facet labels onto
    * every output row — the grouping-column pass-through of the stat contract.
    * Continuous colour never survives a stat (checked upstream).
    */
  private def reassemble(
    p: Prep, keys: Array[GroupKey],
    gx: Array[Array[Double]], gy: Array[Array[Double]],
    gLo: Array[Array[Double]], gHi: Array[Array[Double]],
    gMn: Array[Array[Double]], gMx: Array[Array[Double]],
    gWd: Array[Array[Double]]
  ): Prep =
    var total = 0
    var g = 0
    while g < gx.length do
      total += gx(g).length
      g += 1
    inline def out(gc: Array[Array[Double]]): Array[Double] =
      if gc.length == 0 then noD else new Array[Double](total)
    val xs2 = new Array[Double](total)
    val ys2 = out(gy)
    val lo2 = out(gLo)
    val hi2 = out(gHi)
    val mn2 = out(gMn)
    val mx2 = out(gMx)
    val wd2 = out(gWd)
    val xi2 = if p.xIdx.length == 0 then noI else new Array[Int](total)
    val ci2 = if p.colorIdx.length == 0 then noI else new Array[Int](total)
    val cl2 = if p.colLabs.length == 0 then noS else new Array[String](total)
    val rl2 = if p.rowLabs.length == 0 then noS else new Array[String](total)
    var o = 0
    g = 0
    while g < gx.length do
      val ex = gx(g)
      var k = 0
      while k < ex.length do
        xs2(o) = ex(k)
        if ys2.length > 0 then ys2(o) = gy(g)(k)
        if lo2.length > 0 then lo2(o) = gLo(g)(k)
        if hi2.length > 0 then hi2(o) = gHi(g)(k)
        if mn2.length > 0 then mn2(o) = gMn(g)(k)
        if mx2.length > 0 then mx2(o) = gMx(g)(k)
        if wd2.length > 0 then wd2(o) = gWd(g)(k)
        if xi2.length > 0 then xi2(o) = keys(g).xi
        if ci2.length > 0 then ci2(o) = keys(g).ci
        if cl2.length > 0 then cl2(o) = keys(g).cl
        if rl2.length > 0 then rl2(o) = keys(g).rl
        o += 1
        k += 1
      g += 1
    p.copy(xs = xs2, ys = ys2, yLo = lo2, yHi = hi2, yMin = mn2, yMax = mx2,
           xEnd = noD, yEnd = noD, wd = wd2, xRep = noD, xIdx = xi2, colorIdx = ci2,
           colorVal = noD, colLabs = cl2, rowLabs = rl2)

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
    val gy = if p.ys.length == 0 then noDD else new Array[Array[Double]](ng)
    val gl = if p.yLo.length == 0 then noDD else new Array[Array[Double]](ng)
    val gh = if p.yHi.length == 0 then noDD else new Array[Array[Double]](ng)
    var g = 0
    while g < ng do
      val order = members(g).sortBy(p.xs(_))
      val sx = order.map(p.xs(_))
      def run(src: Array[Double]): Array[Double] =
        val (ax, ay) = applySmoother(how, sx, order.map(src(_)))
        gx(g) = ax
        ay
      if gy.length > 0 then gy(g) = run(p.ys)
      if gl.length > 0 then gl(g) = run(p.yLo)
      if gh.length > 0 then gh(g) = run(p.yHi)
      if gx(g) == null then gx(g) = sx
      g += 1
    reassemble(p, keys, gx, gy, gl, gh, noDD, noDD, noDD)

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
    val width = if hi > lo then Ticks.step((hi - lo) / jm.max(1, bins)) else jm.max(1.0, jm.abs(lo) * 0.01)
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
    reassemble(p, keys, gx, gy, noDD, noDD, noDD, noDD, noDD)

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
    reassemble(p, keys, gx, gy, noDD, noDD, noDD, noDD, noDD)

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
    reassemble(p, keys, gx, gy, noDD, noDD, noDD, noDD, noDD)

  /** Drops rows whose x is NaN — a row with no x cannot be grouped by x. */
  private def finiteX(p: Prep): Prep =
    val n = p.xs.length
    var cnt = 0
    var i = 0
    while i < n do
      if !p.xs(i).isNaN then cnt += 1
      i += 1
    if cnt == n then p
    else
      inline def pick(src: Array[Double]): Array[Double] =
        if src.length == 0 then src
        else
          val a = new Array[Double](cnt)
          var j = 0
          var k = 0
          while k < n do
            if !p.xs(k).isNaN then
              a(j) = src(k)
              j += 1
            k += 1
          a
      inline def pickI(src: Array[Int]): Array[Int] =
        if src.length == 0 then src
        else
          val a = new Array[Int](cnt)
          var j = 0
          var k = 0
          while k < n do
            if !p.xs(k).isNaN then
              a(j) = src(k)
              j += 1
            k += 1
          a
      inline def pickS(src: Array[String]): Array[String] =
        if src.length == 0 then src
        else
          val a = new Array[String](cnt)
          var j = 0
          var k = 0
          while k < n do
            if !p.xs(k).isNaN then
              a(j) = src(k)
              j += 1
            k += 1
          a
      p.copy(xs = pick(p.xs), ys = pick(p.ys), yLo = pick(p.yLo), yHi = pick(p.yHi),
             yMin = pick(p.yMin), yMax = pick(p.yMax), xEnd = pick(p.xEnd), yEnd = pick(p.yEnd),
             wd = pick(p.wd), xRep = pick(p.xRep), xIdx = pickI(p.xIdx), colorIdx = pickI(p.colorIdx),
             colorVal = pick(p.colorVal), colLabs = pickS(p.colLabs), rowLabs = pickS(p.rowLabs))

  /** Assigns each row to a bin of x and repositions it at the bin's representative, so a
    * following summary stat groups per bin and draws there.  All colour levels share one
    * representative per bin, so dodged summaries stay aligned; `slotW` records the bin
    * width so the summaries can size themselves to their bin.
    */
  private def binByPrep(p0: Prep, width: Double, bins: Int, at: BinBy.At): Ask[Prep] = Ask:
    val p = finiteX(p0)
    if p.xs.length == 0 then Err.break("binBy(): no finite x values to bin")
    var lo = Double.PositiveInfinity
    var hi = Double.NegativeInfinity
    var i = 0
    while i < p.xs.length do
      val v = p.xs(i)
      if v < lo then lo = v
      if v > hi then hi = v
      i += 1
    val w =
      if width == width && width > 0 then width
      else if hi > lo then Ticks.step((hi - lo) / jm.max(1, bins))
      else jm.max(1.0, jm.abs(lo) * 0.01)
    val e0 = jm.floor(lo / w + 1e-9) * w
    val nb = jm.max(1, jm.ceil((hi - e0) / w - 1e-9).toInt)
    val xi = new Array[Int](p.xs.length)
    i = 0
    while i < p.xs.length do
      var b = jm.floor((p.xs(i) - e0) / w + 1e-9).toInt
      if b < 0 then b = 0
      if b >= nb then b = nb - 1
      xi(i) = b
      i += 1
    val rep = new Array[Double](nb)
    at match
      case BinBy.At.Center =>
        var b = 0
        while b < nb do
          rep(b) = e0 + (b + 0.5) * w
          b += 1
      case BinBy.At.Mean =>
        val cnt = new Array[Int](nb)
        i = 0
        while i < p.xs.length do
          rep(xi(i)) += p.xs(i)
          cnt(xi(i)) += 1
          i += 1
        var b = 0
        while b < nb do
          rep(b) = if cnt(b) > 0 then rep(b) / cnt(b) else e0 + (b + 0.5) * w
          b += 1
      case BinBy.At.Median =>
        val cnt = new Array[Int](nb)
        i = 0
        while i < p.xs.length do
          cnt(xi(i)) += 1
          i += 1
        val vals = new Array[Array[Double]](nb)
        val fill = new Array[Int](nb)
        var b = 0
        while b < nb do
          vals(b) = new Array[Double](cnt(b))
          b += 1
        i = 0
        while i < p.xs.length do
          vals(xi(i))(fill(xi(i))) = p.xs(i)
          fill(xi(i)) += 1
          i += 1
        b = 0
        while b < nb do
          rep(b) =
            if cnt(b) == 0 then e0 + (b + 0.5) * w
            else
              val s = kse.maths.Quantile.finiteSorted(vals(b), 0, vals(b).length)
              kse.maths.Quantile.ofSorted(s, 0, s.length)(0.5)
          b += 1
    // rows keep their literal x (an outlier at 28 plots at 28, not at its bin's 25);
    // the per-row representative rides along for the summary bodies to draw at
    val xr = new Array[Double](p.xs.length)
    i = 0
    while i < p.xs.length do
      xr(i) = rep(xi(i))
      i += 1
    p.copy(xRep = xr, xIdx = xi, slotW = w)

  /** Summaries on an ungrouped continuous x group by distinct x value — the natural read
    * when x is a small set of numeric conditions.  A genuinely continuous x would make a
    * group per point, which is not a summary of anything: refuse and point at binBy.
    */
  private def groupedX(p0: Prep, li: Int, what: String): Ask[Prep] = Ask:
    if p0.xIdx.length > 0 then p0
    else
      val p = finiteX(p0)
      val distinct = p.xs.distinct.sorted
      if distinct.length == 0 then Err.break(s"layer ${li + 1}: $what found no finite x values")
      if distinct.length > 8 && distinct.length * 2 > p.xs.length then
        Err.break(s"layer ${li + 1}: $what grouped by distinct x would make ${distinct.length} groups of nearly one point each; bin a continuous x first with binBy(...)")
      val index = collection.mutable.HashMap.empty[Double, Int]
      var i = 0
      while i < distinct.length do
        index(distinct(i)) = i
        i += 1
      val xi = new Array[Int](p.xs.length)
      i = 0
      while i < p.xs.length do
        xi(i) = index(p.xs(i))
        i += 1
      p.copy(xIdx = xi)

  /** Whisker ends over sorted finite values: the most extreme data within k IQRs of the
    * box (Tukey — the quartiles are always within, so the scans cannot cross), the given
    * quantiles, or the extremes.
    */
  private def whiskEnds(s: Array[Double], q1: Double, q3: Double, whisk: Whisk): (Double, Double) =
    val n = s.length
    whisk match
      case Whisk.Iqr(k) =>
        val r = k * (q3 - q1)
        var a = 0
        while a < n && s(a) < q1 - r do a += 1
        var b = n - 1
        while b >= 0 && s(b) > q3 + r do b -= 1
        (s(a), s(b))
      case Whisk.Quantiles(ql, qh) =>
        (kse.maths.Quantile.ofSorted(s, 0, n)(jm.min(ql, qh)),
         kse.maths.Quantile.ofSorted(s, 0, n)(jm.max(ql, qh)))
      case Whisk.Extremes => (s(0), s(n - 1))

  /** Five-number box summary per group, plus the outliers beyond the whiskers as a second
    * table of individually drawn Strip points — the stat contract's multi-output seam,
    * exercised for real.  NaN y values are ignored; a group with no finite y emits
    * nothing.
    */
  private def boxPrep(p: Prep, whisk: Whisk): List[Prep] =
    if p.xs.length == 0 then return p :: Nil
    val (keys, members) = groupsOf(p)
    val ng = keys.length
    val gx = new Array[Array[Double]](ng)
    val gy = new Array[Array[Double]](ng)
    val gLo = new Array[Array[Double]](ng)
    val gHi = new Array[Array[Double]](ng)
    val gMn = new Array[Array[Double]](ng)
    val gMx = new Array[Array[Double]](ng)
    val gox = new Array[Array[Double]](ng)
    val goy = new Array[Array[Double]](ng)
    var haveOut = false
    var g = 0
    while g < ng do
      val mem = members(g)
      val vals = new Array[Double](mem.length)
      var i = 0
      while i < mem.length do
        vals(i) = p.ys(mem(i))
        i += 1
      val s = kse.maths.Quantile.finiteSorted(vals, 0, vals.length)
      if s.length == 0 then
        gx(g) = noD
        gy(g) = noD
        gLo(g) = noD
        gHi(g) = noD
        gMn(g) = noD
        gMx(g) = noD
        gox(g) = noD
        goy(g) = noD
      else
        val n = s.length
        val q1 = kse.maths.Quantile.ofSorted(s, 0, n)(0.25)
        val med = kse.maths.Quantile.ofSorted(s, 0, n)(0.5)
        val q3 = kse.maths.Quantile.ofSorted(s, 0, n)(0.75)
        val (wLo, wHi) = whiskEnds(s, q1, q3, whisk)
        val rep = if p.xRep.length > 0 then p.xRep(mem(0)) else p.xs(mem(0))
        gx(g) = Array(rep)
        gy(g) = Array(med)
        gLo(g) = Array(q1)
        gHi(g) = Array(q3)
        gMn(g) = Array(wLo)
        gMx(g) = Array(wHi)
        // outliers keep each row's literal x — a binned outlier plots where it is, not
        // at its bin's representative
        val ox = collection.mutable.ArrayBuffer.empty[Double]
        val oy = collection.mutable.ArrayBuffer.empty[Double]
        i = 0
        while i < mem.length do
          val v = p.ys(mem(i))
          if v == v && (v < wLo || v > wHi) then
            val _ = ox.addOne(p.xs(mem(i)))
            val _ = oy.addOne(v)
          i += 1
        if ox.isEmpty then
          gox(g) = noD
          goy(g) = noD
        else
          haveOut = true
          gox(g) = ox.toArray
          goy(g) = oy.toArray
      g += 1
    val main = reassemble(p, keys, gx, gy, gLo, gHi, gMn, gMx, noDD)
    if !haveOut then main :: Nil
    else main :: reassemble(p, keys, gox, goy, noDD, noDD, noDD, noDD, noDD).copy(kind = Visual.Kind.Strip) :: Nil

  /** Kernel density of y per group as the violin width channel — over the whisker-fenced
    * body only, on the group's own grid trimmed to the body's actual range, normalized so
    * every violin has the same maximum width.  Outliers, groups with fewer than
    * `violinMin` points, and zero-spread bodies emit their points individually as a
    * second Strip table (literal x): a kernel bump over an outlier or a handful of
    * points looks like statistical support for a gap-and-blip the data does not
    * contain, so nothing bulges without enough points behind it.  NaN y values are
    * ignored.
    */
  private def violinPrep(p: Prep, bandwidth: Double, whisk: Whisk): List[Prep] =
    if p.xs.length == 0 then return p :: Nil
    val (keys, members) = groupsOf(p)
    val ng = keys.length
    val gx = new Array[Array[Double]](ng)
    val gy = new Array[Array[Double]](ng)
    val gWd = new Array[Array[Double]](ng)
    val gox = new Array[Array[Double]](ng)
    val goy = new Array[Array[Double]](ng)
    var havePts = false
    var g = 0
    while g < ng do
      val mem = members(g)
      val vals = new Array[Double](mem.length)
      var i = 0
      while i < mem.length do
        vals(i) = p.ys(mem(i))
        i += 1
      val s = kse.maths.Quantile.finiteSorted(vals, 0, vals.length)
      gx(g) = noD
      gy(g) = noD
      gWd(g) = noD
      gox(g) = noD
      goy(g) = noD
      if s.length > 0 then
        val rep = if p.xRep.length > 0 then p.xRep(mem(0)) else p.xs(mem(0))
        var wLo = s(0)
        var wHi = s(s.length - 1)
        var q1 = wLo
        var q3 = wHi
        if s.length >= violinMin then
          q1 = kse.maths.Quantile.ofSorted(s, 0, s.length)(0.25)
          q3 = kse.maths.Quantile.ofSorted(s, 0, s.length)(0.75)
          val ends = whiskEnds(s, q1, q3, whisk)
          wLo = ends._1
          wHi = ends._2
        var b0 = 0
        var b1 = s.length
        while b0 < s.length && s(b0) < wLo do b0 += 1
        while b1 > 0 && s(b1 - 1) > wHi do b1 -= 1
        val body = if b0 == 0 && b1 == s.length then s else java.util.Arrays.copyOfRange(s, b0, b1)
        val bw =
          if s.length < violinMin || body.length < violinMin then 0.0
          else if bandwidth.isNaN then kse.maths.Smoothing.silvermanBandwidth(body)
          else bandwidth
        if bw > 0 then
          // the density tapers past the body by up to three bandwidths — a taper is more
          // honest than a guillotined end — but never past the numeric fence: for Tukey
          // whiskers no datum lies between the whisker end and the fence, so the taper
          // provably cannot wrap an outlier point; for quantile whiskers data sits
          // immediately past the cut, so there the blunt end is the true shape
          val (fLo, fHi) = whisk match
            case Whisk.Iqr(k)          => (q1 - k * (q3 - q1), q3 + k * (q3 - q1))
            case Whisk.Quantiles(_, _) => (wLo, wHi)
            case Whisk.Extremes        => (Double.NegativeInfinity, Double.PositiveInfinity)
          val ex0 = grid(jm.max(body(0) - 3 * bw, fLo), jm.min(body(body.length - 1) + 3 * bw, fHi), 96)
          val d0 = kse.maths.Smoothing.kdeAt(body, ex0, bw)
          var mx = 0.0
          i = 0
          while i < d0.length do
            if d0(i) > mx then mx = d0(i)
            i += 1
          if mx > 0 then
            i = 0
            while i < d0.length do
              d0(i) /= mx
              i += 1
          // drop near-zero filament ends (outside the body only) so the taper closes
          var a0 = 0
          var a1 = d0.length - 1
          while a0 < a1 && d0(a0) < 0.012 && ex0(a0) < body(0) do a0 += 1
          while a1 > a0 && d0(a1) < 0.012 && ex0(a1) > body(body.length - 1) do a1 -= 1
          val ex = java.util.Arrays.copyOfRange(ex0, a0, a1 + 1)
          val d = java.util.Arrays.copyOfRange(d0, a0, a1 + 1)
          gx(g) = Array.fill(ex.length)(rep)
          gy(g) = ex
          gWd(g) = d
          val ox = collection.mutable.ArrayBuffer.empty[Double]
          val oy = collection.mutable.ArrayBuffer.empty[Double]
          i = 0
          while i < mem.length do
            val v = p.ys(mem(i))
            if v == v && (v < wLo || v > wHi) then
              val _ = ox.addOne(p.xs(mem(i)))
              val _ = oy.addOne(v)
            i += 1
          if ox.nonEmpty then
            havePts = true
            gox(g) = ox.toArray
            goy(g) = oy.toArray
        else
          // no density worth drawing: every point in the group shows individually
          val ox = new Array[Double](s.length)
          val oy = new Array[Double](s.length)
          var j = 0
          i = 0
          while i < mem.length do
            val v = p.ys(mem(i))
            if v == v then
              ox(j) = p.xs(mem(i))
              oy(j) = v
              j += 1
            i += 1
          havePts = true
          gox(g) = ox
          goy(g) = oy
      g += 1
    val main = reassemble(p, keys, gx, gy, noDD, noDD, noDD, noDD, gWd)
    if !havePts then main :: Nil
    else main :: reassemble(p, keys, gox, goy, noDD, noDD, noDD, noDD, noDD).copy(kind = Visual.Kind.Strip) :: Nil

  private def statName(st: Stat): String = st match
    case Smooth(_)     => "smooth()"
    case Bin(_)        => "bin()"
    case Density(_)    => "density()"
    case Count         => "count"
    case BinBy(_, _, _) => "binBy()"
    case BoxSummary(_)  => "boxplot()"
    case YDensity(_, _) => "violin()"

  /** Runs a layer's stats in declared order; a summary stat (boxplot/violin) may emit a
    * second table (outliers) and must come last.  The distribution stats consume x and
    * refuse a mapped y; the summary stats consume y grouped by x.  No stat can carry
    * continuous colour through (there is no level to group by), so that combination
    * refuses too.
    */
  private def statted(p: Prep, stats: List[Stat], li: Int, catX: Boolean): Ask[List[Prep]] = Ask:
    var q = p
    var side: List[Prep] = Nil
    var closed = ""
    stats.foreach: st =>
      if closed.nonEmpty then
        Err.break(s"layer ${li + 1}: ${statName(st)} cannot follow $closed — a summary stat comes last")
      if q.xEnd.length > 0 || q.yEnd.length > 0 then
        Err.break(s"layer ${li + 1}: ${statName(st)} cannot transform edge geometry ('xend'/'yend'); compute the endpoints yourself")
      if q.colorVal.length > 0 then
        Err.break(s"layer ${li + 1}: ${statName(st)} cannot carry continuous colour through; use discrete colour to group the output")
      st match
        case Smooth(how) =>
          if catX then Err.break(s"layer ${li + 1}: smooth() needs a continuous x, but the x axis is categorical")
          if q.xIdx.length > 0 then Err.break(s"layer ${li + 1}: smooth() cannot follow binBy(); binBy() groups for a summary stat")
          q = smoothPrep(q, how)
        case BinBy(w, n, at) =>
          if catX then Err.break(s"layer ${li + 1}: binBy() bins a continuous x, but the x axis is categorical — its levels already group")
          if q.xIdx.length > 0 then Err.break(s"layer ${li + 1}: binBy() applied twice")
          q = binByPrep(q, w, n, at).?
        case BoxSummary(whisk) =>
          if q.ys.length == 0 then Err.break(s"layer ${li + 1}: boxplot() summarizes 'y' grouped by 'x'; map aesthetic 'y'")
          if q.yLo.length > 0 || q.yHi.length > 0 || q.yMin.length > 0 || q.yMax.length > 0 || q.wd.length > 0 then
            Err.break(s"layer ${li + 1}: boxplot() computes the summary channels itself; map them directly with visual(Boxplot) and no stat instead")
          boxPrep(groupedX(q, li, "boxplot()").?, whisk) match
            case main :: rest =>
              q = main
              side = rest
            case _ => ()
          closed = "boxplot()"
        case YDensity(bw, whisk) =>
          if q.ys.length == 0 then Err.break(s"layer ${li + 1}: violin() summarizes 'y' grouped by 'x'; map aesthetic 'y'")
          if q.yLo.length > 0 || q.yHi.length > 0 || q.yMin.length > 0 || q.yMax.length > 0 || q.wd.length > 0 then
            Err.break(s"layer ${li + 1}: violin() computes the width channel itself; map 'width' directly with visual(Violin) and no stat instead")
          violinPrep(groupedX(q, li, "violin()").?, bw, whisk) match
            case main :: rest =>
              q = main
              side = rest
            case _ => ()
          closed = "violin()"
        case other =>
          if q.xIdx.length > 0 && !catX then
            Err.break(s"layer ${li + 1}: after binBy(), use boxplot() or violin(); ${statName(other)} does not summarize groups")
          if q.ys.length > 0 || q.yLo.length > 0 || q.yHi.length > 0 then
            Err.break(s"layer ${li + 1}: ${statName(other)} computes 'y' from the x values; remove the layer's y mapping")
          other match
            case Bin(bins) =>
              if catX then Err.break(s"layer ${li + 1}: bin() needs a continuous x; on a categorical axis use count")
              q = binPrep(q, bins)
            case Density(bw) =>
              if catX then Err.break(s"layer ${li + 1}: density() needs a continuous x; on a categorical axis use count")
              q = densityPrep(q, bw)
            case _ => q = countPrep(q)
    if stats.lastOption.exists(_.isInstanceOf[BinBy]) then
      Err.break(s"layer ${li + 1}: binBy() groups for a summary stat; follow it with boxplot() or violin()")
    q :: side

  //////////////////////////
  /// Blocks and drawing ///
  //////////////////////////

  /** One data panel: axis decorations and facet strips are protrusions; the content rect
    * is pure data area.  Tick density adapts to the granted size, so panels degrade
    * gracefully when small; equal panel sizes plus shared domains give identical ticks
    * across a facet grid.
    */
  private final class Panel(
    slices: Seq[Slice],
    x0: Double, x1: Double, y0: Double, y1: Double,
    xCats: Array[String],
    hue: Hue,
    showLeft: Boolean, showBottom: Boolean,
    colStrip: String, rowStrip: String,
    xTickN: Int, yTickN: Int,
    xMinor: Minor, yMinor: Minor,
    xInk: Ink, yInk: Ink,
    marks: List[Mark],
    fs: Double,
    m: Measurer
  ) extends GlyphBlock:
    private val lab = labSz * fs
    private val tick = tickLen * fs

    // A categorical axis carries no information across its width, so the panel asks for
    // its natural width — slots times a per-visual pitch — and never stretches past it:
    // three boxes across a wide canvas should read as three boxes, not three planks, and
    // twenty-odd bars should fit a square comfortably.  Granted less, everything already
    // degrades (glyph caps shrink, swarms compress into partial overlap).
    private lazy val natW: Double =
      if xCats.length == 0 then Double.PositiveInfinity
      else
        var per = 0.0
        slices.foreach: s =>
          val b = s.kind match
            case Visual.Kind.Bar     => 22.0
            case Visual.Kind.Boxplot => 30.0
            case Visual.Kind.Violin  => 40.0
            case Visual.Kind.Strip   => 52.0  // widest: swarm spread actually encodes count
            case _                   => 24.0
          val nLev = if s.colorIdx.length == 0 then 1 else levelCount
          if b * nLev > per then per = b * nLev
        if per == 0 then per = 24.0
        xCats.length * per * fs

    override def widthPref: Size =
      if xCats.length > 0 then Size.Fixed(natW) else Size.Auto

    // data positions this panel draws as marker discs: an arrow tip aimed bit-exactly at
    // one of these backs off to the disc's edge by default rather than vanishing under it
    private val dotted: collection.Set[(Long, Long)] =
      if !slices.exists(s => s.kind == Visual.Kind.Arrow && s.xEnd.length > 0) then Set.empty
      else
        val b = collection.mutable.HashSet.empty[(Long, Long)]
        slices.foreach: s =>
          if s.kind == Visual.Kind.Scatter then
            var i = 0
            while i < s.xs.length do
              b += ((java.lang.Double.doubleToLongBits(s.xs(i)), java.lang.Double.doubleToLongBits(s.ys(i))))
              i += 1
        b

    // ticks aim for a pleasing density but are hard-capped so labels cannot collide even
    // on a very short axis; a zero-centered domain keeps -x, 0, +x at the cap
    private def fitTicks(lo: Double, hi: Double, target: Int, cap: Int): Ticks =
      var t = jm.max(1, jm.min(target, cap))
      var ts = Ticks.linear(lo, hi, t)
      while ts.values.length > jm.max(2, cap) && t > 1 do
        t -= 1
        ts = Ticks.linear(lo, hi, t)
      ts
    private def xTicks(w: Double): Ticks =
      if xCats.length > 0 then new Ticks(Array.tabulate(xCats.length)(_.toDouble), xCats, noD)
      else
        val t = if xTickN > 0 then xTickN else jm.max(2, jm.min(8, (w / (lab * 7.5)).toInt))
        fitTicks(x0, x1, t, jm.max(2, (w / (lab * 4.5)).toInt))
    private def yTicks(h: Double): Ticks =
      val t = if yTickN > 0 then yTickN else jm.max(2, jm.min(8, (h / (m.lineHeight(lab) * 4.5)).toInt))
      fitTicks(y0, y1, t, jm.max(2, (h / (m.lineHeight(lab) * 1.5)).toInt))

    private lazy val catMaxW: Double = xCats.foldLeft(0.0)((w, s) => jm.max(w, m.width(s, lab)))

    // category labels never drop and never touch: horizontal while they fit the slot
    // pitch, then diagonal (adjacent baselines are pitch*sin40 apart), then vertical
    private def catRot(w: Double): Int =
      if xCats.length == 0 then 0
      else
        val pitch = w / jm.max(1, xCats.length)
        if catMaxW + 6 * fs <= pitch then 0
        else if m.lineHeight(lab) * 1.556 + 2 * fs <= pitch then -40
        else -90

    // when even vertical labels would touch, label every k-th category (ticks still mark
    // every one; each drawn label sits at its true slot)
    private def catEvery(w: Double): Int =
      if xCats.length == 0 then 1
      else
        val rot = catRot(w)
        if rot != -90 then 1
        else
          val pitch = w / jm.max(1, xCats.length)
          val need = m.lineHeight(lab) + 2 * fs
          if need <= pitch then 1 else jm.ceil(need / pitch).toInt

    def protrusions(w0: Double, h: Double): Prot =
      val w = jm.min(w0, natW)
      val yt = if showLeft then yTicks(h) else null
      val xt = if showBottom then xTicks(w) else null
      val yLabels =
        if yt != null then yt.labels.foldLeft(0.0)((mx, s) => jm.max(mx, m.width(s, lab))) + axisHalf + tick + 8 * fs
        else 0.0
      // centered x tick labels overhang the content rect at its corners; reporting the
      // real overhang as protrusion keeps them on canvas (and out of neighbor panels)
      def atX(v: Double): Double = if x1 > x0 then (v - x0) / (x1 - x0) * w else 0.0
      val rot = if xt != null then catRot(w) else 0
      var xLeft = 0.0
      var xRight = 0.0
      if xt != null && xt.length > 0 then
        if rot == 0 then
          xLeft = jm.max(0.0, m.width(xt.labels(0), lab) / 2 - atX(xt.values(0)))
          xRight = jm.max(0.0, atX(xt.values(xt.length - 1)) + m.width(xt.labels(xt.length - 1), lab) / 2 - w)
        else if rot == -40 then
          // end-anchored diagonal labels extend down-left of their tick
          xLeft = jm.max(0.0, 0.766 * catMaxW - atX(xt.values(0)))
        else
          xLeft = jm.max(0.0, m.lineHeight(lab) / 2 - atX(xt.values(0)))
      // and the topmost y label's cap can poke about half a line above the content top
      var yTop = 0.0
      if yt != null && yt.length > 0 then
        val dTop = if y1 > y0 then h * (1.0 - (yt.values(yt.length - 1) - y0) / (y1 - y0)) else h
        yTop = jm.max(0.0, m.lineHeight(lab) / 2 - dTop)
      val left = jm.max(yLabels, xLeft)
      val right = jm.max(if rowStrip.nonEmpty then m.width(rowStrip, lab) + 8 * fs else 0.0, xRight)
      val top = jm.max(if colStrip.nonEmpty then m.lineHeight(lab) + 4 * fs else 0.0, yTop)
      // exactly the labels' extent (drawn at bottom + half frame + tick + 2 within a line
      // box); any extra here reads as dead space between the labels and the axis title
      val xLabDepth =
        if rot == 0 then m.lineHeight(lab)
        else if rot == -40 then 0.643 * catMaxW + 0.766 * m.lineHeight(lab)
        else catMaxW + 2 * fs
      val bottom = if showBottom then xLabDepth + axisHalf + tick + 2 * fs else 0.0
      Prot(left, right, top, bottom)

    private def levelCount: Int = hue match
      case Hue.Levels(n) => n
      case _             => 1

    private def flatColour(s: Slice): String =
      if s.styled.nonEmpty then s.styled
      else hue match
        case Hue.Off => palette(s.layerIdx % palette.length)
        case _       => neutral

    private def pointColour(s: Slice, i: Int): String =
      if s.colorIdx.length > 0 then palette(s.colorIdx(i) % palette.length)
      else if s.colorVal.length > 0 then
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
        if s.colorIdx.length == 0 then Array.range(0, s.xs.length)
        else
          val b = collection.mutable.ArrayBuffer.empty[Int]
          var i = 0
          while i < s.xs.length do
            if s.colorIdx(i) == lv then { val _ = b.addOne(i) }
            i += 1
          b.toArray
      all.sortBy(s.xs(_))

    private def presentLevels(s: Slice): Array[Int] =
      if s.colorIdx.length == 0 then Array(-1)
      else s.colorIdx.distinct.sorted

    /** Slot pitch in data units for grouped visuals: the recorded bin width or category
      * pitch, else the smallest gap between distinct x positions, else a tenth of the
      * span.
      */
    private def pitchOf(s: Slice): Double =
      if !s.slotW.isNaN then s.slotW
      else
        val distinct = s.xs.distinct.sorted
        var gap = Double.PositiveInfinity
        var i = 1
        while i < distinct.length do
          val d = distinct(i) - distinct(i - 1)
          if d > 0 && d < gap then gap = d
          i += 1
        if gap < Double.PositiveInfinity then gap else (x1 - x0) / 10.0

    /** Dodged sub-slot for colour level `lv` within a group's slot: (center offset from
      * the slot position, sub-slot width), in data units.  Strip, Boxplot, and Violin
      * share the centers, so a box's outliers land exactly on their dodged box; each
      * visual sizes its own glyph within the sub-slot, capped in px — a box or violin is
      * a glyph, and the cross-slot direction carries no information, so its width must
      * not grow with panel size.
      */
    private def dodge(s: Slice, lv: Int): (Double, Double) =
      val full = 0.78 * pitchOf(s)
      val nLev = if s.colorIdx.length == 0 then 1 else levelCount
      val sub = full / nLev
      val lvv = if lv < 0 then 0 else lv
      (-full / 2 + (lvv + 0.5) * sub, sub)

    /** Deterministic beeswarm within each (slot, colour) group: points in y order take
      * the nearest horizontal offset that clears the already-placed — no randomness, and
      * spreading is honest here only because a categorical slot's cross direction
      * carries no content.  A swarm wider than the dodged sub-slot compresses
      * proportionally, so the bees partially overlap rather than the slot bloating —
      * fine, since rings keep overlaps visible.
      */
    private def swarm(s: Slice, pxA: Array[Double], pyA: Array[Double], r: Double, pxPerX: Double): Unit =
      val d = 2 * r + 1.2
      val groups = collection.mutable.LinkedHashMap.empty[(Long, Int), collection.mutable.ArrayBuffer[Int]]
      var i = 0
      while i < s.xs.length do
        val key = (java.lang.Double.doubleToLongBits(s.xs(i)), if s.colorIdx.length == 0 then -1 else s.colorIdx(i))
        val buf = groups.getOrElseUpdate(key, collection.mutable.ArrayBuffer.empty[Int])
        val _ = buf.addOne(i)
        i += 1
      groups.foreach: (key, idxs) =>
        val (_, sub) = dodge(s, key._2)
        val cap = jm.max(2.0, 0.45 * sub * pxPerX - r)
        val order = idxs.toArray.sortBy(j => (s.ys(j), j))
        val offs = new Array[Double](order.length)
        val placedX = new Array[Double](order.length)
        val placedY = new Array[Double](order.length)
        var np = 0
        var maxAbs = 0.0
        order.foreach: j =>
          val py = pyA(j)
          val cx = pxA(j)
          val cands = collection.mutable.ArrayBuffer(0.0)
          var k = np - 1
          while k >= 0 && placedY(k) - py < d do
            val dy = placedY(k) - py
            val dx = jm.sqrt(jm.max(0.0, d * d - dy * dy))
            val _ = cands.addOne(placedX(k) - cx + dx)
            val _ = cands.addOne(placedX(k) - cx - dx)
            k -= 1
          val sorted = cands.toArray.sortBy(c => (jm.abs(c), c))
          // the outermost flank of the outermost neighbor is always clear, so this finds
          // a candidate; placement is unclamped and the whole group compresses after
          var chosen = Double.NaN
          var ci = 0
          while chosen.isNaN && ci < sorted.length do
            val c = sorted(ci)
            var clear = true
            var k2 = np - 1
            while clear && k2 >= 0 && placedY(k2) - py < d do
              val ddx = cx + c - placedX(k2)
              val ddy = placedY(k2) - py
              if ddx * ddx + ddy * ddy < d * d - 1e-6 then clear = false
              k2 -= 1
            if clear then chosen = c
            ci += 1
          offs(np) = chosen
          placedX(np) = cx + chosen
          placedY(np) = py
          if jm.abs(chosen) > maxAbs then maxAbs = jm.abs(chosen)
          np += 1
        val squeeze = if maxAbs > cap then cap / maxAbs else 1.0
        var k = 0
        while k < order.length do
          pxA(order(k)) = pxA(order(k)) + offs(k) * squeeze
          k += 1

    private def drawSlice(s: Slice, rect: Rect, sx: Double => Double, sy: Double => Double, occ: Occupancy | Null, put: Glyph => Unit): Unit =
      val flat = flatColour(s)
      s.kind match
        case Visual.Kind.Scatter =>
          var i = 0
          while i < s.xs.length do
            val px = sx(s.xs(i))
            val py = sy(s.ys(i))
            put(Glyph.Disc(px, py, dotRad * fs, pointColour(s, i), s.edge.alpha))
            if occ != null then occ.markDisc(px, py, dotRad * fs + 2)
            i += 1
        case Visual.Kind.Strip =>
          // never jittered: on a categorical axis points spread beeswarm-style — the
          // cross-slot direction carries no content there — while anywhere x means
          // something (a continuous axis, binned summaries' outliers) each point sits at
          // its literal x.  Overlap shows honestly: under fade(), translucent discs
          // accumulate ink; otherwise thin rings merge only when a reader could not tell
          // separate rings apart anyway, thickening with the count until solid.
          val r = dotRad * fs * 0.85
          val pxPerX = rect.w / (x1 - x0)
          val nPts = s.xs.length
          val pxA = new Array[Double](nPts)
          val pyA = new Array[Double](nPts)
          var i = 0
          while i < nPts do
            val off =
              if xCats.length > 0 && s.xIdx.length > 0 then
                dodge(s, if s.colorIdx.length == 0 then -1 else s.colorIdx(i))._1
              else 0.0
            pxA(i) = sx(s.xs(i) + off)
            pyA(i) = sy(s.ys(i))
            i += 1
          if xCats.length > 0 then swarm(s, pxA, pyA, r, pxPerX)
          if s.edge.alpha < 1 then
            i = 0
            while i < nPts do
              put(Glyph.Disc(pxA(i), pyA(i), r, pointColour(s, i), s.edge.alpha))
              if occ != null then occ.markDisc(pxA(i), pyA(i), r + 2)
              i += 1
          else
            val eps = jm.max(1.0, 0.45 * r)
            val cx = collection.mutable.ArrayBuffer.empty[Double]
            val cy = collection.mutable.ArrayBuffer.empty[Double]
            val cc = collection.mutable.ArrayBuffer.empty[String]
            val cn = collection.mutable.ArrayBuffer.empty[Int]
            val cells = collection.mutable.HashMap.empty[(Long, Long, Int), List[Int]]
            i = 0
            while i < nPts do
              val lv = if s.colorIdx.length == 0 then -1 else s.colorIdx(i)
              val px = pxA(i)
              val py = pyA(i)
              val gx0 = jm.floor(px / eps).toLong
              val gy0 = jm.floor(py / eps).toLong
              var found = -1
              var dx = -1
              while dx <= 1 && found < 0 do
                var dy = -1
                while dy <= 1 && found < 0 do
                  cells.getOrElse((gx0 + dx, gy0 + dy, lv), Nil).foreach: id =>
                    if found < 0 then
                      val ddx = cx(id) - px
                      val ddy = cy(id) - py
                      if ddx * ddx + ddy * ddy <= eps * eps then found = id
                  dy += 1
                dx += 1
              if found >= 0 then cn(found) += 1
              else
                val id = cx.length
                val _ = cx.addOne(px)
                val _ = cy.addOne(py)
                val _ = cc.addOne(pointColour(s, i))
                val _ = cn.addOne(1)
                val key = (gx0, gy0, lv)
                cells(key) = id :: cells.getOrElse(key, Nil)
              i += 1
            var c = 0
            while c < cx.length do
              val w0 = 0.9 * fs * jm.sqrt(cn(c).toDouble)
              if w0 >= r then put(Glyph.Disc(cx(c), cy(c), r, cc(c)))
              else put(Glyph.Ring(cx(c), cy(c), r - w0 / 2, cc(c), w0))
              if occ != null then occ.markDisc(cx(c), cy(c), r + 2)
              c += 1
        case Visual.Kind.Boxplot =>
          val hasWh = s.yMin.length > 0
          val lw = jm.max(1.0, 1.2 * fs)
          val pxPerX = rect.w / (x1 - x0)
          var i = 0
          while i < s.xs.length do
            val lv = if s.colorIdx.length == 0 then -1 else s.colorIdx(i)
            val (off, sub) = dodge(s, lv)
            val hw = jm.min(11 * fs, 0.3 * sub * pxPerX) / pxPerX
            val cxp = sx(s.xs(i) + off)
            val xL = sx(s.xs(i) + off - hw)
            val xR = sx(s.xs(i) + off + hw)
            val colour = if lv >= 0 then palette(lv % palette.length) else flatColour(s)
            val yMed = sy(s.ys(i))
            val yQ1 = sy(s.yLo(i))
            val yQ3 = sy(s.yHi(i))
            if hasWh then
              val yW0 = sy(s.yMin(i))
              val yW1 = sy(s.yMax(i))
              val capw = 0.25 * (xR - xL)
              put(Glyph.Segment(cxp, yW0, cxp, yQ1, colour, lw))
              put(Glyph.Segment(cxp, yQ3, cxp, yW1, colour, lw))
              put(Glyph.Segment(cxp - capw, yW0, cxp + capw, yW0, colour, lw))
              put(Glyph.Segment(cxp - capw, yW1, cxp + capw, yW1, colour, lw))
              if occ != null then occ.markSegment(cxp, yW0, cxp, yW1, 2)
            put(Glyph.Box(xL, yQ3, xR - xL, jm.max(0.0, yQ1 - yQ3), colour, 0.18, colour, lw))
            put(Glyph.Segment(xL, yMed, xR, yMed, colour, jm.max(1.6, 2.0 * fs)))
            if occ != null then occ.markBox(xL, yQ3, xR - xL, jm.max(0.0, yQ1 - yQ3))
            i += 1
        case Visual.Kind.Violin =>
          // rows of one violin are contiguous, share an x position and colour level, and
          // run in y order (the stat emits them so; direct mapping must supply them so)
          val pxPerX = rect.w / (x1 - x0)
          var i = 0
          while i < s.xs.length do
            val xb = java.lang.Double.doubleToLongBits(s.xs(i))
            val lv = if s.colorIdx.length == 0 then -1 else s.colorIdx(i)
            var j = i + 1
            while j < s.xs.length
                  && java.lang.Double.doubleToLongBits(s.xs(j)) == xb
                  && (if s.colorIdx.length == 0 then -1 else s.colorIdx(j)) == lv do
              j += 1
            val (off, sub) = dodge(s, lv)
            val hw = jm.min(16 * fs, 0.45 * sub * pxPerX) / pxPerX
            val colour = if lv >= 0 then palette(lv % palette.length) else flatColour(s)
            val n = j - i
            if n == 1 then
              put(Glyph.Disc(sx(s.xs(i) + off), sy(s.ys(i)), jm.max(2.0, 2.2 * fs), colour))
            else
              val px = new Array[Double](2 * n + 1)
              val py = new Array[Double](2 * n + 1)
              var k = 0
              while k < n do
                val yv = sy(s.ys(i + k))
                px(k) = sx(s.xs(i + k) + off + s.wd(i + k) * hw)
                py(k) = yv
                px(2 * n - 1 - k) = sx(s.xs(i + k) + off - s.wd(i + k) * hw)
                py(2 * n - 1 - k) = yv
                k += 1
              px(2 * n) = px(0)
              py(2 * n) = py(0)
              put(Glyph.Poly(px, py, colour, 0.3))
              put(Glyph.Polyline(px, py, colour, jm.max(0.9, 1.1 * fs)))
              if occ != null then
                k = 0
                while k < n do
                  occ.markSegment(px(2 * n - 1 - k), py(k), px(k), py(k), 1.0, 0.5)
                  k += 1
            i = j
        case Visual.Kind.Line =>
          inline def polyline(idx: IndexedSeq[Int], colour: String): Unit =
            val px = idx.map(i => sx(s.xs(i))).toArray
            val py = idx.map(i => sy(s.ys(i))).toArray
            put(Glyph.Polyline(px, py, colour, jm.max(0.8, 1.8 * fs)))
            if occ != null then
              var i = 1
              while i < px.length do
                occ.markSegment(px(i - 1), py(i - 1), px(i), py(i), 2 * fs)
                i += 1
          if s.colorIdx.length == 0 then
            if s.xs.length >= 2 then polyline(0 until s.xs.length, flat)
          else
            presentLevels(s).foreach: lv =>
              val idx = (0 until s.xs.length).filter(i => s.colorIdx(i) == lv)
              if idx.length >= 2 then polyline(idx, palette(lv % palette.length))
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
              if occ != null then
                i = 1
                while i < n do
                  // pale fill: weight the interior lightly so truly clear space still wins
                  val steps = jm.max(1, jm.ceil((px(i) - px(i - 1)) / occ.cell).toInt)
                  var k = 0
                  while k <= steps do
                    val t = k.toDouble / steps
                    val cx = px(i - 1) + (px(i) - px(i - 1)) * t
                    val hiY = py(i - 1) + (py(i) - py(i - 1)) * t
                    val loY = py(2 * n - i) + (py(2 * n - 1 - i) - py(2 * n - i)) * t
                    occ.markColumn(cx, hiY, loY, 0.35)
                    k += 1
                  i += 1
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
              if occ != null then
                i = 2
                while i <= n do
                  occ.markSegment(px(i - 1), py(i - 1), px(i), py(i), 1.5 * fs)
                  val steps = jm.max(1, jm.ceil((px(i) - px(i - 1)) / occ.cell).toInt)
                  var k = 0
                  while k <= steps do
                    val t = k.toDouble / steps
                    occ.markColumn(px(i - 1) + (px(i) - px(i - 1)) * t, py(i - 1) + (py(i) - py(i - 1)) * t, base, 0.5)
                    k += 1
                  i += 1
        case Visual.Kind.Segment | Visual.Kind.Arrow =>
          // one row per connection: this is the shape edge sets want, and it keeps
          // thousands of connections as columns rather than figure-level annotations
          val sh = s.edge.shape
          val radPx = if s.edge.curve.isNaN || s.edge.curve == 0 then Double.NaN else s.edge.curve * fs
          val headed = s.kind == Visual.Kind.Arrow
          var i = 0
          while i < s.xs.length do
            val px = sx(s.xs(i))
            val py = sy(s.ys(i))
            val qx = sx(s.xEnd(i))
            val qy = sy(s.yEnd(i))
            val colour = pointColour(s, i)
            if headed then
              val backPx =
                if !s.edge.backoff.isNaN then s.edge.backoff * fs
                else if dotted.contains((java.lang.Double.doubleToLongBits(s.xEnd(i)), java.lang.Double.doubleToLongBits(s.yEnd(i)))) then (dotRad + 1.0) * fs
                else 0.0
              Arrow.outline(px, py, qx, qy, sh.headLength * fs, sh.headHalfWidth * fs, sh.barb,
                            jm.max(0.6, sh.shaftWidth * fs), radPx, backPx) match
                case null => ()
                case o: Arrow.Outline => put(Glyph.Poly(o.xs, o.ys, colour, s.edge.alpha))
            else
              // headless edges stroke rather than fill: cheaper to encode, and there is no
              // head for the shaft to meet, so nothing needs a single composited outline
              val wdt = jm.max(0.6, sh.shaftWidth * fs)
              if radPx.isNaN then put(Glyph.Segment(px, py, qx, qy, colour, wdt, s.edge.alpha))
              else
                Arrow.arc(px, py, qx, qy, radPx) match
                  case null      => put(Glyph.Segment(px, py, qx, qy, colour, wdt, s.edge.alpha))
                  case (ax, ay)  => put(Glyph.Polyline(ax, ay, colour, wdt, s.edge.alpha))
            if occ != null then occ.markSegment(px, py, qx, qy, sh.headHalfWidth * fs)
            i += 1
        case Visual.Kind.Bar =>
          val full = 0.9 * pitchOf(s)
          val nLev = if s.colorIdx.length == 0 then 1 else levelCount
          val slot = full / nLev
          val pxPerX = rect.w / (x1 - x0)
          val base = jm.min(rect.bottom, jm.max(rect.y, sy(0.0)))
          var i = 0
          while i < s.xs.length do
            if s.ys(i) != 0.0 then
              val lv = if s.colorIdx.length == 0 then 0 else s.colorIdx(i)
              val xL = sx(s.xs(i) - full / 2 + lv * slot)
              val yp = sy(s.ys(i))
              val top = jm.min(yp, base)
              val hgt = jm.abs(yp - base)
              val fill = if s.colorIdx.length == 0 then flat else palette(lv % palette.length)
              if hgt > 0.01 then
                val bwPx = jm.max(0.5, slot * pxPerX)
                put(Glyph.Box(xL, top, bwPx, hgt, fill))
                if occ != null then occ.markBox(xL, top, bwPx, hgt)
            i += 1

    private def wrapAngle(a: Double): Double =
      var x = a % (2 * jm.PI)
      if x > jm.PI then x -= 2 * jm.PI
      if x < -jm.PI then x += 2 * jm.PI
      x

    /** Where the ray from a box's center toward (tx, ty) crosses the box border. */
    private def edgePoint(cx: Double, cy: Double, hw: Double, hh: Double, tx: Double, ty: Double): (Double, Double) =
      val dx = tx - cx
      val dy = ty - cy
      val s = jm.min(
        if dx == 0 then Double.PositiveInfinity else hw / jm.abs(dx),
        if dy == 0 then Double.PositiveInfinity else hh / jm.abs(dy))
      if s.isInfinite then (cx, cy + hh) else (cx + dx * s, cy + dy * s)

    /** Places each callout by scoring a ring of candidate label positions around its
      * target: clear space under the label dominates, then a leader line that crosses
      * little data, then closeness, then a mild taste for up-and-right.  Placed labels
      * and leaders are marked in the raster so later notes avoid earlier ones.
      */
    private def placeNote(mk: Mark.Noted, rect: Rect, sxF: Double => Double, syF: Double => Double, occ: Occupancy, put: Glyph => Unit): Unit =
      val prefAngle = -jm.PI / 4
      val angles = Array.tabulate(24)(k => k * jm.PI / 12).sortBy(a => jm.abs(wrapAngle(a - prefAngle)))
      val th = m.lineHeight(lab)
      val radii = Array(2.0 * th, 3.0 * th, 4.2 * th, 5.6 * th)
      val (tx, ty) = mk.at match
        case NoteAt.Point(x, y) => (sxF(x), syF(y))
        case NoteAt.OnX(x)      => (sxF(x), rect.bottom)
        case NoteAt.OnY(y)      => (rect.x, syF(y))
      val pad = 3 * fs
      val bw = m.width(mk.text, lab) + 2 * pad
      val bh = th + 2 * pad
      var bestX = 0.0
      var bestY = 0.0
      var bestSc = Double.PositiveInfinity
      var ri = 0
      while ri < radii.length do
        var ai = 0
        while ai < angles.length do
          val ang = angles(ai)
          val cx = tx + radii(ri) * jm.cos(ang)
          val cy = ty + radii(ri) * jm.sin(ang)
          val bx = cx - bw / 2
          val by = cy - bh / 2
          val inRect = bx >= rect.x + 1 && by >= rect.y + 1 && bx + bw <= rect.right - 1 && by + bh <= rect.bottom - 1
          val overTarget = tx >= bx - 2 && tx <= bx + bw + 2 && ty >= by - 2 && ty <= by + bh + 2
          if inRect && !overTarget then
            val (axp, ayp) = edgePoint(cx, cy, bw / 2 + 1, bh / 2 + 1, tx, ty)
            val sc = 3.0 * occ.boxLoad(bx, by, bw, bh) + occ.lineLoad(axp, ayp, tx, ty) +
              0.3 * ri / (radii.length - 1.0) + 0.12 * jm.abs(wrapAngle(ang - prefAngle)) / jm.PI
            if sc < bestSc then
              bestSc = sc
              bestX = cx
              bestY = cy
          ai += 1
        ri += 1
      if bestSc.isInfinite then
        // nothing fits cleanly (tiny panel or huge label): sit near the target, clamped
        bestX = jm.max(rect.x + bw / 2 + 1, jm.min(rect.right - bw / 2 - 1, tx))
        bestY =
          if ty - 2.2 * th >= rect.y + bh / 2 + 1 then ty - 2.2 * th
          else jm.min(rect.bottom - bh / 2 - 1, ty + 2.2 * th)
      val (axp, ayp) = edgePoint(bestX, bestY, bw / 2 + 1, bh / 2 + 1, tx, ty)
      val backPx = (if mk.backoff.isNaN then (mk.at match { case NoteAt.Point(_, _) => 4.5; case _ => 1.0 }) else mk.backoff) * fs
      val radPx = if mk.radius.isNaN || mk.radius == 0 then Double.NaN else mk.radius * fs
      val sh = mk.shape
      Arrow.outline(axp, ayp, tx, ty, sh.headLength * fs, sh.headHalfWidth * fs, sh.barb,
                    jm.max(0.8, sh.shaftWidth * fs), radPx, backPx) match
        case null => ()
        case o: Arrow.Outline =>
          put(Glyph.Poly(o.xs, o.ys, "#3F3F3F", 1.0))
          occ.markSegment(axp, ayp, tx, ty, 1.5)
      put(Glyph.Txt(bestX, bestY + m.ascent(lab) * 0.38, mk.text, lab, "#1F1F1F", Glyph.Anchor.Middle, halo = true))
      occ.markBox(bestX - bw / 2, bestY - bh / 2, bw, bh)

    /** Draws a user-anchored arrow through the shared outline geometry, hanging the label
      * (if any) off the tail end, and marks the raster so callouts placed later steer
      * clear of it.
      */
    private def drawArrow(a: Mark.Arrowed, sxF: Double => Double, syF: Double => Double, occ: Occupancy, put: Glyph => Unit): Unit =
      val px = sxF(a.x1)
      val py = syF(a.y1)
      val qx = sxF(a.x2)
      val qy = syF(a.y2)
      val sh = a.shape
      val radPx = if a.radius.isNaN || a.radius == 0 then Double.NaN else a.radius * fs
      Arrow.outline(px, py, qx, qy, sh.headLength * fs, sh.headHalfWidth * fs, sh.barb,
                    jm.max(0.8, sh.shaftWidth * fs), radPx, a.backoff * fs) match
        case null => ()
        case o: Arrow.Outline =>
          put(Glyph.Poly(o.xs, o.ys, a.colour, a.alpha))
          occ.markSegment(px, py, qx, qy, sh.headHalfWidth * fs)
          if a.label.nonEmpty then
            val ux = -o.tailDirX
            val uy = -o.tailDirY
            val off = 5 * fs
            val gx = px + ux * off
            val gy = py + uy * off
            if jm.abs(ux) >= 0.6 then
              val anchor = if ux > 0 then Glyph.Anchor.Start else Glyph.Anchor.End
              put(Glyph.Txt(gx, gy + m.ascent(lab) * 0.38, a.label, lab, "#1F1F1F", anchor, halo = true))
            else
              val base = if uy > 0 then gy + m.ascent(lab) else gy - 2
              put(Glyph.Txt(gx, base, a.label, lab, "#1F1F1F", Glyph.Anchor.Middle, halo = true))
            occ.markBox(gx - m.width(a.label, lab) / 2 - 2, gy - m.lineHeight(lab) / 2, m.width(a.label, lab) + 4, m.lineHeight(lab))

    def glyphs(rect0: Rect, put: Glyph => Unit): Unit =
      // safety net: granted more than the natural categorical width anyway (the layout
      // normally sizes the cell to it), draw at natural width, centered in the excess
      val rect = if rect0.w > natW then rect0.copy(x = rect0.x + (rect0.w - natW) / 2, w = natW) else rect0
      val xt = xTicks(rect.w)
      val yt = yTicks(rect.h)
      def sx(v: Double): Double = rect.x + (v - x0) / (x1 - x0) * rect.w
      def sy(v: Double): Double = rect.y + rect.h - (v - y0) / (y1 - y0) * rect.h
      // gridlines stop at the frame's edge and skip its exact position, so a translucent
      // frame never shows a gridline through itself or a doubled stripe beneath it
      def vGrid(v: Double, wdt: Double): Unit =
        val px = sx(v)
        if px - rect.x > 0.6 then put(Glyph.Segment(px, rect.y, px, rect.bottom - axisHalf, "#ECECEC", wdt))
      def hGrid(v: Double, wdt: Double): Unit =
        val py = sy(v)
        if rect.bottom - py > 0.6 then put(Glyph.Segment(rect.x + axisHalf, py, rect.right, py, "#ECECEC", wdt))
      // a categorical x draws no vertical gridlines: the slots are labels, not values,
      // and gridlines through boxes and violins would only add noise
      if xCats.length == 0 then
        if xMinor.grid then xt.minor.foreach(vGrid(_, 0.5))
        xt.values.foreach(vGrid(_, 1))
      if yMinor.grid then yt.minor.foreach(hGrid(_, 0.5))
      yt.values.foreach(hGrid(_, 1))
      // the frame: the bottom line owns the corner (starting half a stroke early) and the
      // left line stops half a stroke short -- full coverage, zero overlap, so alpha
      // composites exactly once even where the two axes meet
      put(Glyph.Segment(rect.x - axisHalf, rect.bottom, rect.right, rect.bottom, xInk.colour, 1, xInk.alpha))
      put(Glyph.Segment(rect.x, rect.y, rect.x, rect.bottom - axisHalf, yInk.colour, 1, yInk.alpha))
      if showBottom then
        val xL = xt.labels
        val rot = catRot(rect.w)
        val every = catEvery(rect.w)
        var i = 0
        while i < xt.values.length do
          val px = sx(xt.values(i))
          put(Glyph.Segment(px, rect.bottom + axisHalf, px, rect.bottom + axisHalf + tick, xInk.colour, 1, xInk.alpha))
          if i % every == 0 then
            if rot == 0 then
              put(Glyph.Txt(px, rect.bottom + axisHalf + tick + m.ascent(lab) + 2, xL(i), lab, "#333333", Glyph.Anchor.Middle))
            else if rot == -40 then
              put(Glyph.Txt(px + 2, rect.bottom + axisHalf + tick + m.ascent(lab) * 0.85 + 1, xL(i), lab, "#333333", Glyph.Anchor.End, rotate = -40))
            else
              put(Glyph.Txt(px + m.ascent(lab) * 0.38, rect.bottom + axisHalf + tick + 2, xL(i), lab, "#333333", Glyph.Anchor.End, rotate = -90))
          i += 1
        if xMinor.ticks then
          xt.minor.foreach(v => put(Glyph.Segment(sx(v), rect.bottom + axisHalf, sx(v), rect.bottom + axisHalf + minorFrac * tick, xInk.colour, minorFrac, xInk.alpha)))
      if showLeft then
        val yL = yt.labels
        var i = 0
        while i < yt.values.length do
          val py = sy(yt.values(i))
          put(Glyph.Segment(rect.x - axisHalf - tick, py, rect.x - axisHalf, py, yInk.colour, 1, yInk.alpha))
          put(Glyph.Txt(rect.x - axisHalf - tick - 4, py + m.ascent(lab) * 0.38, yL(i), lab, "#333333", Glyph.Anchor.End))
          i += 1
        if yMinor.ticks then
          yt.minor.foreach(v => put(Glyph.Segment(rect.x - axisHalf - minorFrac * tick, sy(v), rect.x - axisHalf, sy(v), yInk.colour, minorFrac, yInk.alpha)))
      if colStrip.nonEmpty then
        put(Glyph.Txt(rect.x + rect.w / 2, rect.y - 5 * fs, colStrip, lab, "#222222", Glyph.Anchor.Middle, bold = true))
      if rowStrip.nonEmpty then
        put(Glyph.Txt(rect.right + 6, rect.y + rect.h / 2 + m.ascent(lab) * 0.38, rowStrip, lab, "#222222", Glyph.Anchor.Start, bold = true))
      val occ: Occupancy | Null = if marks.isEmpty then null else Occupancy(rect.x, rect.y, rect.w, rect.h, jm.max(4.0, 6 * fs))
      slices.foreach(s => drawSlice(s, rect, sx, sy, occ, put))
      occ match
        case null => ()
        case o: Occupancy =>
          // arrows are fully user-anchored, so they draw first and callouts route around them
          marks.foreach:
            case a: Mark.Arrowed => drawArrow(a, sx, sy, o, put)
            case _ => ()
          marks.foreach:
            case n: Mark.Noted => placeNote(n, rect, sx, sy, o, put)
            case _ => ()

  private final class LegendBlock(title: String | Null, levels: Array[String], fs: Double, m: Measurer) extends GlyphBlock:
    private val lab = labSz * fs
    private val ttl = axisTitleSz * fs
    private def innerWidth: Double =
      val titleW = if title == null then 0.0 else m.width(title, ttl)
      levels.foldLeft(titleW)((w, s) => jm.max(w, 11 * fs + 6 + m.width(s, lab))) + 16 * fs
    override def widthPref: Size = Size.Fixed(innerWidth)
    def protrusions(w: Double, h: Double): Prot = Prot.zero
    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      val lx = rect.x + 4
      var ly = rect.y + 2
      if title != null then
        put(Glyph.Txt(lx, ly + m.ascent(ttl), title, ttl, "#222222", Glyph.Anchor.Start, bold = true))
        ly += m.lineHeight(ttl) + 2
      var lv = 0
      while lv < levels.length do
        put(Glyph.Box(lx, ly + 2, 11 * fs, 11 * fs, palette(lv % palette.length)))
        put(Glyph.Txt(lx + 11 * fs + 6, ly + 2 + m.ascent(lab) * 0.95, levels(lv), lab, "#333333", Glyph.Anchor.Start))
        ly += 17 * fs
        lv += 1

  /** Continuous colour guide: a vertical viridis gradient with ticks, high values up. */
  private final class ColorbarBlock(title: String | Null, lo: Double, hi: Double, fs: Double, m: Measurer) extends GlyphBlock:
    private val lab = labSz * fs
    private val ttl = axisTitleSz * fs
    private val barW = 12.0 * fs
    private def ticks: Ticks = Ticks.linear(lo, hi, 5)
    private def innerWidth: Double =
      val labelW = if hi > lo then ticks.labels.foldLeft(0.0)((w, s) => jm.max(w, m.width(s, lab))) else 0.0
      val titleW = if title == null then 0.0 else m.width(title, ttl)
      jm.max(barW + tickLen * fs + 7 + labelW, titleW) + 16 * fs
    override def widthPref: Size = Size.Fixed(innerWidth)
    def protrusions(w: Double, h: Double): Prot = Prot.zero
    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      val lx = rect.x + 4
      var ty = rect.y + 2
      if title != null then
        put(Glyph.Txt(lx, ty + m.ascent(ttl), title, ttl, "#222222", Glyph.Anchor.Start, bold = true))
        ty += m.lineHeight(ttl) + 4
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

  /** The panel grid with its axis titles attached.  Titles are part of the axis: they
    * join the protrusion stack just outside the panels' own decorations, and they center
    * on the content span — not on any grid cell — so "day" sits snug under the tick
    * labels and dead-center on the data area it names.
    */
  private final class AxesBlock(inner: Grid, xTitle: String | Null, yTitle: String | Null, fs: Double, estH: Double, m: Measurer) extends GlyphBlock:
    private val ttl = axisTitleSz * fs
    private val lead = 2 * fs
    private def stack(t: String | Null): Double = if t == null then 0.0 else m.lineHeight(ttl) + lead
    // when the panels have a natural (categorical) width, the whole axes assembly asks
    // for exactly that, so the figure's title and legend stay glued to the graph instead
    // of spanning space the graph declined to fill
    override def widthPref: Size =
      val nw = inner.naturalWidth(estH)
      if nw.isNaN then Size.Auto else Size.Fixed(nw)
    def protrusions(w: Double, h: Double): Prot =
      val p = inner.protrusions(w, h)
      Prot(p.left + stack(yTitle), p.right, p.top, p.bottom + stack(xTitle))
    def glyphs(rect: Rect, put: Glyph => Unit): Unit =
      val lay = inner.solveAt(rect.x, rect.y, rect.w, rect.h, 0.08, 4, footprint = true)
      emitGrid(inner, lay, put)
      val p = inner.protrusions(rect.w, rect.h)
      // fixed-width (categorical) panels can leave the grid's right edge blank; the axis
      // title centers on the panels actually drawn, not on the granted width
      var right = rect.x
      lay.content.foreach(r => if r.right > right then right = r.right)
      if xTitle != null then
        put(Glyph.Txt((rect.x + right) / 2, rect.bottom + p.bottom + lead + m.ascent(ttl), xTitle, ttl, "#222222", Glyph.Anchor.Middle))
      if yTitle != null then
        put(Glyph.Txt(rect.x - p.left - lead - m.lineHeight(ttl) + m.ascent(ttl), rect.y + rect.h / 2, yTitle, ttl, "#222222", Glyph.Anchor.Middle, rotate = -90))

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

  private def freeSpan(slices: Seq[Slice], horz: Boolean, cfgLo: Double, cfgHi: Double, fb0: Double, fb1: Double): (Double, Double) =
    var lo = Double.PositiveInfinity
    var hi = Double.NegativeInfinity
    inline def sweep(a: Array[Double]): Unit =
      var i = 0
      while i < a.length do
        if a(i) < lo then lo = a(i)
        if a(i) > hi then hi = a(i)
        i += 1
    slices.foreach: s =>
      if horz then
        sweep(s.xs)
        sweep(s.xEnd)
      else
        sweep(s.ys)
        sweep(s.yLo)
        sweep(s.yHi)
        sweep(s.yMin)
        sweep(s.yMax)
        sweep(s.yEnd)
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
    var xTickC = 0
    var yTickC = 0
    var xMinorTicks = true
    var yMinorTicks = true
    var xMinorGrid = false
    var yMinorGrid = false
    var xInkC = plainInk
    var yInkC = plainInk
    var gapH = 12.0
    var gapV = 12.0
    var everyLabel = false
    val insets = collection.mutable.ArrayBuffer.empty[Parts.Config.Inset]
    val marks = collection.mutable.ArrayBuffer.empty[Mark]
    fig.parts.config.foreach:
      case Parts.Config.Note(t, at, back, rad, shp) =>
        val _ = marks.addOne(Mark.Noted(t, at, back, rad, shp))
      case Parts.Config.Arrow(lbl, x1, y1, x2, y2, back, rad, col, al, shp) =>
        val _ = marks.addOne(Mark.Arrowed(lbl, x1, y1, x2, y2, back, rad, col, al, shp))
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
      case Parts.Config.AxisTicks(a, n) =>
        if a == Parts.Axis.Horz then xTickC = jm.max(1, n) else yTickC = jm.max(1, n)
      case Parts.Config.MinorTicks(a, on) =>
        if a == Parts.Axis.Horz then xMinorTicks = on else yMinorTicks = on
      case Parts.Config.MinorGrid(a, on) =>
        if a == Parts.Axis.Horz then xMinorGrid = on else yMinorGrid = on
      case Parts.Config.AxisColor(a, c, al) =>
        if a == Parts.Axis.Horz then xInkC = Ink(c, al) else yInkC = Ink(c, al)
      case Parts.Config.FreeAxis(h, v) =>
        freeX |= h
        freeY |= v
      case Parts.Config.PanelGap(h, v) =>
        gapH = h
        gapV = v
      case Parts.Config.EachLabeled => everyLabel = true
      case ins: Parts.Config.Inset  => val _ = insets.addOne(ins)

    // one x slot for the whole figure: every layer's x is categorical or none is, since
    // slot positions and level dictionaries must agree across superposed layers
    var sawCatX = false
    var sawNumX = false
    layers.foreach: layer =>
      layer.data.fields.foreach: f =>
        if f.name == "x" then
          if f.column.scale.kind == ScaleKind.Discrete then sawCatX = true else sawNumX = true
        else if f.name == "xend" then sawNumX = true
    if sawCatX && sawNumX then
      Err.break("cannot mix a categorical x with a continuous x (or edge geometry) across layers")
    val catX = sawCatX
    if catX then
      if freeX then Err.break("free horizontal scales do not apply to a categorical x: every panel shows the levels")
      if !xLoC.isNaN || !xHiC.isNaN then Err.break("axis.horz.limit does not apply to a categorical x: every level shows")
    val xCatBuf = collection.mutable.ArrayBuffer.empty[String]

    val levels = collection.mutable.ArrayBuffer.empty[String]

    val prepped = layers.zipWithIndex.flatMap: (layer, li) =>
      // when the user names no visual, the last stat picks a sensible one
      val kind = layer.look.visual match
        case null =>
          layer.look.stats.lastOption match
            case Some(Smooth(_))            => Visual.Kind.Line
            case Some(Bin(_)) | Some(Count) => Visual.Kind.Bar
            case Some(Density(_))           => Visual.Kind.Area
            case Some(BoxSummary(_)) | Some(BinBy(_, _, _)) => Visual.Kind.Boxplot
            case Some(YDensity(_, _))       => Visual.Kind.Violin
            case _                          => Visual.Kind.Scatter
        case v: Visual => v.kind
      val names = layer.data.names
      if layer.data.fields.nonEmpty && layer.data.length == 0 then
        Err.break(s"layer ${li + 1} has no rows: its columns [${names.mkString(", ")}] are empty")
      def channel(name: String): Array[Double] =
        layer.data.fields.find(_.name == name) match
          case Some(f) => numbersOf(f.column, s"layer ${li + 1} aesthetic '$name'").?
          case None    => noD
      val ys = channel("y")
      val yLo = channel("ylow")
      val yHi = channel("yhigh")
      val yMn = channel("ymin")
      val yMx = channel("ymax")
      val wdC = channel("width")
      val xEnd = channel("xend")
      val yEnd = channel("yend")
      var xIdx = noI
      val xs: Array[Double] =
        if catX then
          layer.data.fields.find(_.name == "x") match
            case Some(f) =>
              // shared level dictionary, first appearance across layers in layer order
              val labsX = labelsOf(f.column, s"layer ${li + 1} aesthetic 'x'").?
              val out = new Array[Int](labsX.length)
              var i = 0
              while i < out.length do
                var k = xCatBuf.indexOf(labsX(i))
                if k < 0 then
                  k = xCatBuf.length
                  val _ = xCatBuf.addOne(labsX(i))
                out(i) = k
                i += 1
              xIdx = out
              Array.tabulate(out.length)(out(_).toDouble)
            case None => Err.break(s"layer ${li + 1} needs aesthetic 'x': the figure's x axis is categorical")
        else
          val cx = channel("x")
          if cx.length > 0 then cx
          else
            layer.look.stats.find(st => !st.isInstanceOf[Smooth]) match
              case Some(st) => Err.break(s"layer ${li + 1}: ${statName(st)} needs aesthetic 'x'; it has [${names.mkString(", ")}]")
              case None => ()
            val ref = if ys.length > 0 then ys else if yHi.length > 0 then yHi else yLo
            if ref.length > 0 then Array.tabulate(ref.length)(_.toDouble)  // default: observation index
            else Err.break(s"layer ${li + 1} needs aesthetic 'y'; it has [${names.mkString(", ")}]")
      var colorIdx = noI
      var colorVal = noD
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
        case None    => noS
      val rowLabs = layer.data.fields.find(_.name == "row") match
        case Some(f) => labelsOf(f.column, s"layer ${li + 1} facet 'row'").?
        case None    => noS
      // rightmost styled constant wins, per the cascade; a mapped colour column still beats it
      var styled = ""
      layer.look.style.entries.foreach: (k, v) =>
        if k eq Style.Color then styled = v.asInstanceOf[String]
      var edge = plainEdge
      layer.look.style.entries.foreach: (k, v) =>
        if k eq Style.Arrow then edge = edge.copy(shape = v.asInstanceOf[ArrowShape])
        else if k eq Style.Curve then edge = edge.copy(curve = v.asInstanceOf[Double])
        else if k eq Style.Alpha then edge = edge.copy(alpha = v.asInstanceOf[Double])
        else if k eq Style.Backoff then edge = edge.copy(backoff = v.asInstanceOf[Double])
      val raw = Prep(xs, ys, yLo, yHi, yMn, yMx, xEnd, yEnd, wdC, noD,
                     xIdx, if catX then 1.0 else Double.NaN,
                     colorIdx, colorVal, styled, edge, kind, li, colLabs, rowLabs)
      // stats run upstream of scale resolution, so domains cover the transformed data;
      // a summary stat may emit a second table (its outliers), so a layer is a list
      val ps = if layer.look.stats.isEmpty then raw :: Nil else statted(raw, layer.look.stats, li, catX).?
      // aesthetic completeness per visual, checked after stats have had their say
      ps.foreach: p =>
        if p.xs.length > 0 then
          p.kind match
            case Visual.Kind.Band =>
              if p.yLo.length == 0 || p.yHi.length == 0 then
                Err.break(s"layer ${li + 1} with visual Band needs aesthetics 'ylow' and 'yhigh'; it has [${names.mkString(", ")}]")
            case Visual.Kind.Boxplot =>
              if p.ys.length == 0 || p.yLo.length == 0 || p.yHi.length == 0 then
                Err.break(s"layer ${li + 1} with visual Boxplot needs aesthetics 'y' (median), 'ylow', and 'yhigh' (quartiles) — or the boxplot() stat; it has [${names.mkString(", ")}]")
              if (p.yMin.length == 0) != (p.yMax.length == 0) then
                Err.break(s"layer ${li + 1} with visual Boxplot: map both 'ymin' and 'ymax' (the whisker ends) or neither")
            case Visual.Kind.Violin =>
              if p.ys.length == 0 || p.wd.length == 0 then
                Err.break(s"layer ${li + 1} with visual Violin needs aesthetics 'y' and 'width' — or the violin() stat; it has [${names.mkString(", ")}]")
            case k =>
              if p.ys.length == 0 then Err.break(s"layer ${li + 1} needs aesthetic 'y'; it has [${names.mkString(", ")}]")
              if k == Visual.Kind.Segment || k == Visual.Kind.Arrow then
                if p.xEnd.length == 0 || p.yEnd.length == 0 then
                  Err.break(s"layer ${li + 1} with visual $k needs aesthetics 'xend' and 'yend'; it has [${names.mkString(", ")}]")
          if catX then
            p.kind match
              case Visual.Kind.Scatter =>
                Err.break(s"layer ${li + 1}: Scatter on a categorical x hides coincident points under one dot; use strip — exact positions, overlap shown honestly")
              case Visual.Kind.Line | Visual.Kind.Band | Visual.Kind.Area | Visual.Kind.Segment | Visual.Kind.Arrow =>
                Err.break(s"layer ${li + 1}: visual ${p.kind} needs a continuous or temporal x, but the x axis is categorical")
              case _ => ()
          if p.colorVal.length > 0 && p.kind != Visual.Kind.Scatter then
            Err.break(s"layer ${li + 1}: continuous colour is interpreted on Scatter only so far; use discrete colour for ${p.kind}")
      ps

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
    inline def dySweep(a: Array[Double]): Unit =
      var i = 0
      while i < a.length do
        if a(i) < dyLo then dyLo = a(i)
        if a(i) > dyHi then dyHi = a(i)
        i += 1
    inline def dxSweep(a: Array[Double]): Unit =
      var i = 0
      while i < a.length do
        if a(i) < dxLo then dxLo = a(i)
        if a(i) > dxHi then dxHi = a(i)
        i += 1
    prepped.foreach: p =>
      dxSweep(p.xs)
      dxSweep(p.xEnd)
      dySweep(p.ys)
      dySweep(p.yLo)
      dySweep(p.yHi)
      dySweep(p.yMin)
      dySweep(p.yMax)
      dySweep(p.yEnd)
      if p.kind == Visual.Kind.Bar || p.kind == Visual.Kind.Area then
        if dyLo > 0 then dyLo = 0.0
        if dyHi < 0 then dyHi = 0.0
    if !(dxLo <= dxHi && dyLo <= dyHi) then Err.break("figure has no data points")
    // annotation targets count as data when domains are fit, so they land in view by
    // default (on a categorical x the domain is the levels; x targets are slot indices)
    inline def dxTake(x: Double): Unit =
      if !catX then
        if x < dxLo then dxLo = x
        if x > dxHi then dxHi = x
    inline def dyTake(y: Double): Unit =
      if y < dyLo then dyLo = y
      if y > dyHi then dyHi = y
    marks.foreach:
      case Mark.Noted(_, at, _, _, _) =>
        at match
          case NoteAt.Point(x, y) =>
            dxTake(x)
            dyTake(y)
          case NoteAt.OnX(x) => dxTake(x)
          case NoteAt.OnY(y) => dyTake(y)
      case a: Mark.Arrowed =>
        dxTake(a.x1)
        dxTake(a.x2)
        dyTake(a.y1)
        dyTake(a.y2)
    val (fx0, fx1) =
      if catX then (-0.5, xCatBuf.length - 0.5)
      else axisSpan(dxLo, dxHi, xLoC, xHiC)
    val (fy0, fy1) = axisSpan(dyLo, dyHi, yLoC, yHiC)

    // facet levels, first appearance in layer order; an empty array = unfaceted dimension
    def levelsFor(get: Prep => Array[String]): Array[String] =
      val buf = collection.mutable.ArrayBuffer.empty[String]
      prepped.foreach: p =>
        val labs = get(p)
        var i = 0
        while i < labs.length do
          if !buf.contains(labs(i)) then { val _ = buf.addOne(labs(i)) }
          i += 1
      buf.toArray
    val colLevels = levelsFor(_.colLabs)
    val rowLevels = levelsFor(_.rowLabs)
    val nC = jm.max(1, colLevels.length)
    val nR = jm.max(1, rowLevels.length)

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
        inline def countChannel(ya: Array[Double]): Unit =
          if ya.length > 0 then
            var i = 0
            while i < p.xs.length do
              val xf = (p.xs(i) - fx0) / (fx1 - fx0)
              val yf = 1 - (ya(i) - fy0) / (fy1 - fy0)
              val c0 = if p.colLabs.length == 0 then 0 else jm.max(0, colLevels.indexOf(p.colLabs(i)))
              val c1 = if p.colLabs.length == 0 then nC - 1 else c0
              val r0 = if p.rowLabs.length == 0 then 0 else jm.max(0, rowLevels.indexOf(p.rowLabs(i)))
              val r1 = if p.rowLabs.length == 0 then nR - 1 else r0
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

    // pad 0: the facet grid's outer decorations ARE its protrusions now, and the canvas
    // breathing room comes from the outer grid's pad instead of dead space in here
    val facetGrid = Grid(nR, nC, colGap = gapH, rowGap = gapV, pad = 0)
    val markHits = new Array[Int](marks.length)
    val xCatArr = xCatBuf.toArray
    val clOn = colLevels.length > 0
    val rlOn = rowLevels.length > 0
    var r = 0
    while r < nR do
      var c = 0
      while c < nC do
        val cl = if clOn then colLevels(c) else ""
        val rl = if rlOn then rowLevels(r) else ""
        val slices = prepped.map(p => sliceFor(p, cl, clOn, rl, rlOn))
        val (px0, px1) = if freeX then freeSpan(slices, true, xLoC, xHiC, fx0, fx1) else (fx0, fx1)
        val (py0, py1) = if freeY then freeSpan(slices, false, yLoC, yHiC, fy0, fy1) else (fy0, fy1)
        // an annotation shows wherever its anchors are on this panel's axes
        val panMarks = List.newBuilder[Mark]
        var k = 0
        marks.foreach: mk =>
          val ok = mk match
            case Mark.Noted(_, at, _, _, _) =>
              at match
                case NoteAt.Point(x, y) => x >= px0 && x <= px1 && y >= py0 && y <= py1
                case NoteAt.OnX(x)      => x >= px0 && x <= px1
                case NoteAt.OnY(y)      => y >= py0 && y <= py1
            case a: Mark.Arrowed =>
              a.x1 >= px0 && a.x1 <= px1 && a.x2 >= px0 && a.x2 <= px1 &&
              a.y1 >= py0 && a.y1 <= py1 && a.y2 >= py0 && a.y2 <= py1
          if ok then
            markHits(k) += 1
            panMarks += mk
          k += 1
        val pan = Panel(
          slices, px0, px1, py0, py1,
          xCatArr,
          hue,
          showLeft = freeY || everyLabel || c == 0,
          showBottom = freeX || everyLabel || r == nR - 1,
          colStrip = if r == 0 then cl else "",
          rowStrip = if c == nC - 1 then rl else "",
          xTickN = xTickC, yTickN = yTickC,
          xMinor = Minor(xMinorTicks, xMinorGrid), yMinor = Minor(yMinorTicks, yMinorGrid),
          xInk = xInkC, yInk = yInkC,
          panMarks.result(),
          fs,
          m
        )
        val _ = facetGrid.put(r, c)(pan)
        c += 1
      r += 1
    var nk = 0
    marks.foreach: mk =>
      if markHits(nk) == 0 then
        val what = mk match
          case Mark.Noted(t, _, _, _, _) => s"note '$t'"
          case a: Mark.Arrowed => if a.label.isEmpty then s"arrow to (${a.x2}, ${a.y2})" else s"arrow '${a.label}'"
        Err.break(s"$what points outside every panel's axes; move the target or relax the axis limits")
      nk += 1

    val legendB: Block | Null =
      if levels.nonEmpty then LegendBlock(legTitle, levels.toArray, fs, m)
      else if contColour then ColorbarBlock(legTitle, cLo, cHi, fs, m)
      else null
    // legend(...) doubles as the figure title when no guide is drawn; title(...) always wins
    val topText: String | Null = if figTitle != null then figTitle else if legendB == null then legTitle else null
    val titleB = if topText != null then TitleBlock(topText, fs, m) else null
    val axes = AxesBlock(facetGrid, xTitle, yTitle, fs, estH, m)

    val rT = if titleB != null then 1 else 0
    val cL = if legendB != null then 1 else 0
    val outer = Grid(rT + 1, 1 + cL, colGap = 0, rowGap = 0, pad = 6)
    if titleB != null then { val _ = outer.put(0, 0, 0, cL)(titleB) }
    val _ = outer.put(rT, 0)(axes)
    if legendB != null then { val _ = outer.put(rT, 1)(legendB) }
    placedInsets.foreach: (sfig, x, y, w, h) =>
      val sub = buildFigure(sfig, estW * w, estH * h).?
      val _ = outer.putFloat(axes)(x, y, w, h)(sub)
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
