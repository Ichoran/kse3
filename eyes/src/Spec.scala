// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.eyes


import scala.annotation.targetName
import scala.NamedTuple.{AnyNamedTuple, Names, DropNames}
import scala.compiletime.{constValueTuple, summonAll}


///////////////////////////////
/// Columns and their bundles ///
///////////////////////////////

/** One column of aesthetic values with its compile-time-resolved scale evidence.  Values are
  * held by reference (do not mutate the source array afterward); row-extracted columns box,
  * and packing by kind happens at interpretation.
  */
final case class Column(values: Array[?], scale: ScaleOf[?]):
  def length: Int = values.length


/** Evidence that `C` can serve as a column of data.  The extension point for accepting more
  * column-like types (immutable seqs, Mem views, ...) without touching the data words.
  */
trait AsColumn[C]:
  def column(c: C): Column

object AsColumn:
  given [V](using s: ScaleOf[V]): AsColumn[Array[V]] = xs => Column(xs, s)


/** A named bundle of equal-length columns — the data content of a layer.  Bundling is a
  * product: columns in one bundle are indexed by the same observation set, which is why
  * lengths must agree and why merge lives under `*`, not `+`.
  */
final case class Data(fields: List[Data.Field]):
  def isEmpty: Boolean = fields.isEmpty
  def names: List[String] = fields.map(_.name)
  def length: Int = fields match
    case f :: _ => f.column.length
    case _      => 0

  def describe: String = fields.map(f => s"${f.name}(${f.column.length})").mkString(", ")

  /** Right-biased union by name: fields of `that` win; surviving fields of `this` keep their
    * relative order and come first.  Lengths must agree — the shared observation index is
    * the whole point of a bundle.
    */
  infix def mergedWith(that: Data): Data =
    if fields.isEmpty then that
    else if that.fields.isEmpty then this
    else if length != that.length then
      throw new IllegalArgumentException(s"column length mismatch merging [$describe] with [${that.describe}]")
    else
      val shadowed = that.fields.map(_.name).toSet
      Data(fields.filterNot(f => shadowed contains f.name) ::: that.fields)

object Data:
  final case class Field(name: String, column: Column)

  val empty: Data = Data(Nil)

  def checked(d: Data): Data =
    if d.fields.forall(_.column.length == d.length) then d
    else throw new IllegalArgumentException(s"columns differ in length: ${d.describe}")

  /** Internal: builds a bundle from a named tuple of column-like values.  Public only
    * because the inline data words must reference it from user code; not intended API.
    */
  def build(names: List[String], makers: List[AsColumn[Any]], p: Product): Data =
    checked(Data(names.zip(makers).zipWithIndex.map{ case ((n, m), i) => Field(n, m.column(p.productElement(i))) }))

  /** Internal: extracts columns from rows, one row-function call per row.  Boxed for now;
    * interpretation packs by scale kind.
    */
  def fromRows[A](names: List[String], scales: List[ScaleOf[Any]], xs: Array[A], f: A => Product): Data =
    val k = names.length
    val arrs = Array.fill(k)(new Array[AnyRef](xs.length))
    var i = 0
    while i < xs.length do
      val row = f(xs(i))
      var j = 0
      while j < k do
        arrs(j)(i) = row.productElement(j).asInstanceOf[AnyRef]
        j += 1
      i += 1
    Data(List.tabulate(k)(j => Field(names(j), Column(arrs(j), scales(j)))))


///////////////////////////////////////
/// Looks: how layers are rendered ///
///////////////////////////////////////

/** How a layer's geometry is drawn.  Just the kind for now; per-visual style hooks in when
  * the styling layer is built.
  *
  * `Band` fills between the `ylow` and `yhigh` aesthetics (both resolve through the y
  * scale slot); `Area` fills from the y values down to zero; `Bar` draws bars from zero to
  * y, dodging side-by-side within a discrete colour scale.  `Segment` and `Arrow` connect
  * (x, y) to the `xend`/`yend` aesthetics — one row per connection, so edge sets are
  * columns rather than thousands of figure-level annotations; `Arrow` adds a head, styled
  * and curved by `arrowStyle(...)`.
  *
  * `Strip` draws points exactly where their data lie (never jittered — position is
  * information), showing overlap honestly: coincident points merge into one ring whose
  * outline thickens with the count, or accumulate translucent ink under `fade(...)`.
  * `Boxplot` draws a box from `ylow` to `yhigh` with a line at `y` and optional whiskers
  * to `ymin`/`ymax` — usually made by the `boxplot()` stat, but mappable directly from
  * precomputed summary columns.  `Violin` mirrors the `width` aesthetic about the slot
  * center across `y` — usually made by the `violin()` stat.  All three dodge side-by-side
  * within a discrete colour scale like `Bar`.
  */
final case class Visual(kind: Visual.Kind)

object Visual:
  enum Kind:
    case Scatter, Line, Band, Area, Bar, Segment, Arrow, Strip, Boxplot, Violin


/** A statistical transform applied to a layer's columns before its visual.  The real
  * contract (grouped, cardinality-changing, multi-table: DESIGN 4.6) arrives with
  * interpretation; for now stats are inert spec values composed in order under `*`.
  */
sealed trait Stat

final case class Smooth(how: Smoother) extends Stat

/** The smoother family; kernels live in `kse.maths.Smoothing` (dependency-free by policy —
  * anything needing real linear algebra waits for an analytics module).  Curve smoothers
  * (Loess/Kernel/Fit) evaluate on an even grid per group; rolling smoothers evaluate at
  * the data, ordered by x.
  */
sealed trait Smoother
final case class Loess(span: Double = 0.75, degree: Int = 2, robust: Int = 0) extends Smoother
final case class Kernel(bandwidth: Double, shape: Kernel.Shape = Kernel.Shape.Gaussian, degree: Int = 1) extends Smoother
object Kernel:
  enum Shape:
    case Gaussian, Epanechnikov, Tricube
final case class Rolling(window: Int) extends Smoother
final case class RollingMedian(window: Int) extends Smoother
final case class Fit(degree: Int = 1) extends Smoother

/** Distribution stats: these consume a layer's x values (the layer must not map y — the
  * stat computes it) and emit new columns, one output set per (colour level × facet cell)
  * group.  `Bin` counts into shared round-edged bins (`bins` is a target; the width snaps
  * to a nice step so edges land on round numbers, and all groups share the same edges so
  * dodged bars align).  `Density` is a kernel density estimate on a shared grid (NaN
  * bandwidth = Silverman's rule).  `Count` tallies occurrences of each distinct x value.
  */
final case class Bin(bins: Int = 30) extends Stat
final case class Density(bandwidth: Double = Double.NaN) extends Stat
case object Count extends Stat


/** How far box-plot whiskers reach.  `Iqr(k)` is the Tukey rule: each whisker runs to the
  * most extreme datum within `k` interquartile ranges of its quartile, and anything beyond
  * is drawn individually as an outlier.  `Quantiles(lo, hi)` puts the whisker ends at those
  * quantiles of the data, with data beyond drawn as outliers.  `Extremes` runs whiskers to
  * the minimum and maximum, so no outliers exist.
  */
enum Whisk:
  case Iqr(k: Double = 1.5)
  case Quantiles(lo: Double = 0.05, hi: Double = 0.95)
  case Extremes

/** Summary stats: these consume a layer's y values grouped by x — categorical levels
  * directly, or bins of a continuous x via `BinBy` (bare distinct values group as-is) —
  * crossed with colour level and facet cell.  `BoxSummary` emits the five-number box
  * summary per group plus the outliers as a second table of individually drawn points
  * (the stat contract's multi-output seam).  `YDensity` emits a kernel density of y per
  * group as a violin's `width` channel (NaN bandwidth = Silverman's rule per group) —
  * over the whisker-fenced body only, with outliers emitted as points like the box's,
  * because a kernel bump over an outlier fakes statistical support for a gap-and-blip
  * that the data does not contain.  The density tapers naturally past the body but
  * never past the fence, so the taper cannot reach an outlying point.  Groups too small
  * for a density to mean anything (fewer than five points) emit their points
  * individually instead.
  */
final case class BoxSummary(whisk: Whisk = Whisk.Iqr()) extends Stat
final case class YDensity(bandwidth: Double = Double.NaN, whisk: Whisk = Whisk.Iqr()) extends Stat

/** Groups a continuous x for a downstream summary stat: rows fall into bins of `width`
  * (or about `bins` bins of a nice snapped width), and each bin's summaries are positioned
  * at `at` — the bin center, or the mean or median x of the bin's contents.
  */
final case class BinBy(width: Double = Double.NaN, bins: Int = 8, at: BinBy.At = BinBy.At.Center) extends Stat
object BinBy:
  enum At:
    case Center, Mean, Median


/** Typed-key attribute store stub (DESIGN 7).  Rightmost entry wins at lookup; the cascade
  * and the real key vocabulary come later.  Constants live here, not in Data: a mapped
  * attribute is a column, a styled attribute is a constant broadcast at render.
  */
final case class Style(entries: List[(Style.Key[?], Any)]):
  infix def mergedWith(that: Style): Style =
    if entries.isEmpty then that
    else if that.entries.isEmpty then this
    else Style(entries ::: that.entries)

object Style:
  final class Key[V](val name: String):
    override def toString = name
  val empty: Style = Style(Nil)
  /** The styled-constant colour key; `color("#0072B2")` in the vocabulary sets it. */
  val Color: Key[String] = new Key[String]("color")
  /** Head and shaft geometry for `Arrow` layers; `arrowStyle(...)` sets it. */
  val Arrow: Key[ArrowShape] = new Key[ArrowShape]("arrowShape")
  /** Radius of curvature for `Arrow`/`Segment` layers; `arrowStyle(curve = ...)` sets it. */
  val Curve: Key[Double] = new Key[Double]("curve")
  /** Stroke opacity for layer geometry; `arrowStyle(alpha = ...)` sets it. */
  val Alpha: Key[Double] = new Key[Double]("alpha")
  /** How far short of the aim point an `Arrow` layer's head stops (px, pre type-scale);
    * NaN = automatic — a tip aimed exactly where a marker is drawn stops at the marker's
    * edge rather than under it.  `arrowStyle(backoff = ...)` sets it.
    */
  val Backoff: Key[Double] = new Key[Double]("backoff")


/** The data-free part of a layer: visual, stats, style.  `visual(...)`, `smooth(...)`, and
  * recipes like `timeseries` are Looks; a Look meets data via `layer * look`.
  */
final case class Look(visual: Visual | Null, stats: List[Stat], style: Style):
  /** Right-biased merge: `that`'s visual wins if set; stats compose in order; styles merge. */
  def *(that: Look): Look =
    Look(if that.visual == null then visual else that.visual, stats ::: that.stats, style mergedWith that.style)
  def *(those: Looks): Looks = Looks(those.terms.map(this * _))
  def *(l: Layer): Layer = Layer(l.data, this * l.look)
  def *(ls: Layers): Layers = Layers(ls.terms.map(this * _))

  def +(that: Look): Looks = Looks(this :: that :: Nil)
  def +(those: Looks): Looks = Looks(this :: those.terms)

object Look:
  val empty: Look = Look(null, Nil, Style.empty)


/** An ordered sum of Looks; `+` order is draw order once applied to a layer. */
final case class Looks(terms: List[Look]):
  def *(that: Look): Looks = Looks(terms.map(_ * that))
  def *(those: Looks): Looks = Looks(terms.flatMap(a => those.terms.map(a * _)))
  def *(l: Layer): Layers = Layers(terms.map(t => Layer(l.data, t * l.look)))
  def *(ls: Layers): Layers = Layers(terms.flatMap(t => ls.terms.map(l => Layer(l.data, t * l.look))))

  def +(that: Look): Looks = Looks(terms :+ that)
  def +(those: Looks): Looks = Looks(terms ::: those.terms)


//////////////////////////////
/// Layers and the algebra ///
//////////////////////////////

/** One product term of the spec algebra: a bundle of columns plus a look.  Ungraded — every
  * layer is born with its aesthetics, so superposition is total and layers with different
  * aesthetic sets coexist (unmapped attributes resolve to styled constants or theme
  * defaults, broadcast at render).
  *
  * `*` is record merge (right-biased on both data fields and look), distributing over sums
  * on either side; `+` is superposition, and its order is draw order.
  */
final case class Layer(data: Data, look: Look):
  def *(that: Layer): Layer = Layer(data mergedWith that.data, look * that.look)
  def *(lk: Look): Layer = Layer(data, look * lk)
  def *(lks: Looks): Layers = Layers(lks.terms.map(t => Layer(data, look * t)))
  def *(those: Layers): Layers = Layers(those.terms.map(this * _))

  def +(that: Layer): Layers = Layers(this :: that :: Nil)
  def +(those: Layers): Layers = Layers(this :: those.terms)
  def +(parts: Parts): Parts = Parts.of(this) + parts


/** An ordered sum of layers; term order is draw order. */
final case class Layers(terms: List[Layer]):
  def *(that: Layer): Layers = Layers(terms.map(_ * that))
  def *(lk: Look): Layers = Layers(terms.map(_ * lk))
  def *(lks: Looks): Layers = Layers(terms.flatMap(l => lks.terms.map(t => Layer(l.data, l.look * t))))
  def *(those: Layers): Layers = Layers(terms.flatMap(l => those.terms.map(l * _)))

  def +(that: Layer): Layers = Layers(terms :+ that)
  def +(those: Layers): Layers = Layers(terms ::: those.terms)
  def +(parts: Parts): Parts = Parts.of(this) + parts


/** Compass directions for anchored placement — checked string literals, so no names are
  * added to the namespace and typos fail to compile.
  */
type Compass = "nw" | "n" | "ne" | "e" | "se" | "s" | "sw" | "w"


/** Where a floated mini-figure goes, in fractions of the host panel area (y down from the
  * top-left).  `At` anchors to a compass point; `Auto` scores the corners by data occupancy
  * and takes the least obstructed.  There is deliberately no automatic space *reservation*:
  * blanking field by axis manipulation cannot be done well without render-and-inspect
  * feedback, and a bad automatic is worse than none — set the axis limit yourself and
  * anchor the inset in the space you made.
  */
enum Place:
  case Exact(x: Double, y: Double, w: Double, h: Double)
  case At(compass: Compass, w: Double, h: Double)
  case Auto(w: Double, h: Double)


/** What a callout arrow points at, in data coordinates: a point, or a value on one axis. */
enum NoteAt:
  case Point(x: Double, y: Double)
  case OnX(x: Double)
  case OnY(y: Double)


/** Arrow styling for notes and arrows: head length and half-width; `barb` (0 = flat-backed
  * triangle, up to 0.9 pulls the back-center toward the tip leaving swept-back barbs); and
  * full shaft width.  All in px at nominal figure size, scaled with the figure's type.
  * The whole arrow renders as one filled outline, so translucency shows no seam where the
  * shaft meets the head and the shaft can never poke past the head's silhouette.
  */
final case class ArrowShape(headLength: Double = 5.5, headHalfWidth: Double = 2.0, barb: Double = 0.0, shaftWidth: Double = 1.1)

object ArrowShape:
  /** A swept-back barbed head, sized up a touch so the barbs read at figure scale. */
  val barbed: ArrowShape = ArrowShape(headLength = 7.0, headHalfWidth = 2.6, barb = 0.45)


/////////////////////////////////////
/// The figure-level Parts monoid ///
/////////////////////////////////////

/** Everything a figure is built from: layers plus config fragments.  `+` is slot-wise
  * merge — layer lists concatenate in draw order; config fragments accumulate and
  * cascade-merge at interpretation.
  */
final case class Parts(layers: Vector[Layer], config: Vector[Parts.Config]):
  def +(that: Parts): Parts = Parts(layers ++ that.layers, config ++ that.config)
  def +(that: Layer): Parts = Parts(layers :+ that, config)
  def +(those: Layers): Parts = Parts(layers ++ those.terms, config)

object Parts:
  val empty: Parts = Parts(Vector.empty, Vector.empty)
  def of(l: Layer): Parts = Parts(Vector(l), Vector.empty)
  def of(ls: Layers): Parts = Parts(ls.terms.toVector, Vector.empty)

  enum Axis:
    case Horz, Vert

  /** Config fragment stub; the typed-key cascade (DESIGN 7) replaces this as it grows. */
  enum Config:
    case LegendTitle(title: String)
    case FigTitle(title: String)
    case AxisTitle(axis: Axis, title: String)
    case AxisLimit(axis: Axis, min: Double, max: Double)  // NaN = unset
    case AxisTicks(axis: Axis, target: Int)
    case MinorTicks(axis: Axis, on: Boolean)
    case MinorGrid(axis: Axis, on: Boolean)
    case AxisColor(axis: Axis, colour: String, alpha: Double)
    case FreeAxis(horz: Boolean, vert: Boolean)
    case PanelGap(horz: Double, vert: Double)
    case EachLabeled
    case Inset(fig: Figure, place: Place)
    case Note(text: String, at: NoteAt, backoff: Double, radius: Double, shape: ArrowShape)
    case Arrow(label: String, x1: Double, y1: Double, x2: Double, y2: Double,
               backoff: Double, radius: Double, colour: String, alpha: Double, shape: ArrowShape)


/** An interpreted figure.  For now just the normalized spec; scene, layout, and rendering
  * attach here as they are built.  Whole figures compose onto one canvas with `|` (beside)
  * and `/` (above), each keeping its own scales, legend, and titles.
  */
final case class Figure(parts: Parts):
  def |(that: Figure): Board = Board.One(this) | that
  def |(that: Board): Board = Board.One(this) | that
  def /(that: Figure): Board = Board.One(this) / that
  def /(that: Board): Board = Board.One(this) / that


/** A canvas of independent figures: `a | b` puts figures beside each other, `a / b` stacks
  * them.  `/` binds tighter than `|`, so `a | b / c` is a beside a b-over-c stack; use
  * parentheses for the other reading.  Rows and stacks flatten, so `a | b | c` is one
  * three-across row.
  */
enum Board:
  case One(fig: Figure)
  case Beside(items: List[Board])
  case Above(items: List[Board])

  def |(that: Figure): Board = this | Board.One(that)
  def /(that: Figure): Board = this / Board.One(that)

  def |(that: Board): Board = (this, that) match
    case (Board.Beside(a), Board.Beside(b)) => Board.Beside(a ::: b)
    case (Board.Beside(a), b)               => Board.Beside(a :+ b)
    case (a, Board.Beside(b))               => Board.Beside(a :: b)
    case (a, b)                             => Board.Beside(a :: b :: Nil)

  def /(that: Board): Board = (this, that) match
    case (Board.Above(a), Board.Above(b)) => Board.Above(a ::: b)
    case (Board.Above(a), b)              => Board.Above(a :+ b)
    case (a, Board.Above(b))              => Board.Above(a :: b)
    case (a, b)                           => Board.Above(a :: b :: Nil)


//////////////////////////////
/// The vocabulary and Fig ///
//////////////////////////////

/** The `data` word: bundles of named columns.
  * {{{
  * data(x = times, y = temps, color = labels)          // columns directly
  * data((y = values))                                  // single column: extra parens needed
  * data.from(sales)(s => (x = s.date, y = s.revenue))  // columns extracted from rows
  * }}}
  * A lone `name = value` parses as a named method argument, hence the extra parens in the
  * single-column case.
  */
final class DataWord private[eyes] ():
  inline def apply[T <: AnyNamedTuple](t: T): Layer =
    val names = constValueTuple[Names[T]].toList.asInstanceOf[List[String]]
    val makers = summonAll[Tuple.Map[DropNames[T], AsColumn]].toList.asInstanceOf[List[AsColumn[Any]]]
    Layer(Data.build(names, makers, t.asInstanceOf[Product]), Look.empty)

  /** Extracts columns from rows eagerly, calling the row function once per row.  Scale
    * kinds resolve per field at compile time via `ScaleOf`; the lambda's parameter type
    * comes from `xs`, so inference is local and context-free.
    */
  inline def from[A, T <: AnyNamedTuple](xs: Array[A])(f: A => T): Layer =
    val names = constValueTuple[Names[T]].toList.asInstanceOf[List[String]]
    val scales = summonAll[Tuple.Map[DropNames[T], ScaleOf]].toList.asInstanceOf[List[ScaleOf[Any]]]
    Layer(Data.fromRows(names, scales, xs, f.asInstanceOf[A => Product]), Look.empty)


/** Axis config words, reached as `axis.vert.limit(...)` etc. */
final class AxisWords private[eyes] (which: Parts.Axis):
  def limit(min: Double = Double.NaN, max: Double = Double.NaN): Parts =
    Parts(Vector.empty, Vector(Parts.Config.AxisLimit(which, min, max)))
  def title(text: String): Parts =
    Parts(Vector.empty, Vector(Parts.Config.AxisTitle(which, text)))
  /** Ask for about this many ticks instead of the density the panel's size suggests.
    * Nice steps quantize what is actually delivered, and the collision cap still has the
    * last word: labels never touch, however many were requested.
    */
  def ticks(target: Int): Parts =
    Parts(Vector.empty, Vector(Parts.Config.AxisTicks(which, target)))
  /** Unlabeled minor ticks subdividing the major intervals; on by default.  Minors never
    * get labels — a labeled minor would just be another major.
    */
  def minorTicks(on: Boolean = true): Parts =
    Parts(Vector.empty, Vector(Parts.Config.MinorTicks(which, on)))
  /** Faint gridlines at the minor tick positions; off by default. */
  def minorGrid(on: Boolean = true): Parts =
    Parts(Vector.empty, Vector(Parts.Config.MinorGrid(which, on)))
  /** Ink for this axis's frame line and tick marks (labels keep their own colour).
    * Translucent ink composites cleanly: frame, ticks, and gridlines never double-draw.
    */
  def color(c: String, alpha: Double = 1.0): Parts =
    Parts(Vector.empty, Vector(Parts.Config.AxisColor(which, c, alpha)))
  /** This axis fits each facet panel's own data instead of the shared domain. */
  def free: Parts =
    Parts(Vector.empty, Vector(Parts.Config.FreeAxis(which == Parts.Axis.Horz, which == Parts.Axis.Vert)))

final class AxisVocab private[eyes] ():
  val horz: AxisWords = AxisWords(Parts.Axis.Horz)
  val vert: AxisWords = AxisWords(Parts.Axis.Vert)
  /** Both axes free: every facet panel fits its own data. */
  def free: Parts = Parts(Vector.empty, Vector(Parts.Config.FreeAxis(true, true)))

/** Panel-arrangement words for facet grids. */
final class PanelsVocab private[eyes] ():
  def gap(both: Double): Parts = gap(both, both)
  def gap(horz: Double, vert: Double): Parts = Parts(Vector.empty, Vector(Parts.Config.PanelGap(horz, vert)))
  /** Every panel gets its own tick labels (scales still shared unless axes are free). */
  def eachLabeled: Parts = Parts(Vector.empty, Vector(Parts.Config.EachLabeled))


/** The `note` words: callouts — a short label with an arrow pointing at a spot, the label
  * placed automatically in relatively clear space (measured from the geometry actually
  * drawn; notes are placed in spec order, later notes avoiding earlier ones).
  * {{{
  * note("peak demand", x = 31.0, y = 55.2)   // points at a data-space location
  * note.x("launch", 20.0)                    // points at a spot on the horizontal axis
  * note.y("threshold", 4.5)                  // points at a spot on the vertical axis
  * }}}
  * `backoff` (px) sets how far short of the target the tip stops (NaN = a sensible
  * default per target kind); `radius` (px) bows the arrow shaft — positive to the
  * traveler's left, negative right, with the head staying straight and the bend starting
  * only behind it; `shape` restyles the head and shaft (e.g. `ArrowShape.barbed`).
  * Note targets are included when axis domains are fit.  In a faceted figure the note
  * appears in every panel whose scales contain its target; if no panel does (pinned axis
  * limits, free scales), rendering fails loudly rather than dropping the annotation.
  */
final class NoteWord private[eyes] ():
  def apply(text: String, x: Double, y: Double, backoff: Double = Double.NaN, radius: Double = Double.NaN, shape: ArrowShape = ArrowShape()): Parts =
    Parts(Vector.empty, Vector(Parts.Config.Note(text, NoteAt.Point(x, y), backoff, radius, shape)))
  def x(text: String, at: Double, backoff: Double = Double.NaN, radius: Double = Double.NaN, shape: ArrowShape = ArrowShape()): Parts =
    Parts(Vector.empty, Vector(Parts.Config.Note(text, NoteAt.OnX(at), backoff, radius, shape)))
  def y(text: String, at: Double, backoff: Double = Double.NaN, radius: Double = Double.NaN, shape: ArrowShape = ArrowShape()): Parts =
    Parts(Vector.empty, Vector(Parts.Config.Note(text, NoteAt.OnY(at), backoff, radius, shape)))


/** The spec-building words.  Everything is a method on some object (`Fig`, or the scope
  * passed to a `Fig` block), so bare-name use is one `import` away.
  */
trait Vocabulary:
  val data: DataWord = DataWord()

  def visual(kind: Visual.Kind): Look = Look(Visual(kind), Nil, Style.empty)

  def smooth(how: Smoother): Look = Look(null, Smooth(how) :: Nil, Style.empty)

  /** Counts the layer's x values into bins as y; `bins` is a target count (edges snap to
    * round numbers).  Do not map y — the stat computes it.
    */
  def bin(bins: Int = 30): Look = Look(null, Bin(bins) :: Nil, Style.empty)

  /** Kernel density estimate of the layer's x values as y; NaN bandwidth = Silverman's
    * rule of thumb.  Do not map y — the stat computes it.
    */
  def density(bandwidth: Double = Double.NaN): Look = Look(null, Density(bandwidth) :: Nil, Style.empty)

  /** Occurrence count of each distinct x value as y.  Do not map y — the stat computes it. */
  def count: Look = Look(null, Count :: Nil, Style.empty)

  /** A styled constant colour for this look's layers (any SVG colour string).  Per
    * attribute the resolution order is mapped column ▸ styled constant ▸ default, so a
    * colour *column* in the data still wins, but layers with no colour mapping — a band
    * and the line through it, say — can share one hue instead of cycling the palette.
    */
  def color(c: String): Look = Look(null, Nil, Style((Style.Color, c) :: Nil))

  /** Styles `Segment`/`Arrow` layer geometry: head and shaft shape, radius of curvature
    * (positive bows to the traveler's left; the head stays straight and the bend starts
    * behind it), stroke opacity — useful when thousands of edges overlap — and head
    * backoff (px short of the aim point; NaN = automatic, where a tip aimed exactly at a
    * drawn marker stops at the marker's edge instead of disappearing under it).
    */
  def arrowStyle(shape: ArrowShape = ArrowShape(), curve: Double = Double.NaN, alpha: Double = 1.0, backoff: Double = Double.NaN): Look =
    Look(null, Nil, Style((Style.Arrow, shape) :: (Style.Curve, curve) :: (Style.Alpha, alpha) :: (Style.Backoff, backoff) :: Nil))

  /** Binned bars of the x distribution: `visual(Bar) * bin(bins)`. */
  def histogram(bins: Int = 30): Look = visual(Visual.Kind.Bar) * bin(bins)

  /** Points drawn where their data lie — never jittered, because readers reason from
    * position and a randomly displaced point is plotted where the data is not.  On a
    * categorical axis the direction across the slot carries no content, so points spread
    * beeswarm-style there — deterministically, each at the nearest clear offset — which
    * is honest for the same reason jitter is not.  Wherever x does carry content (a
    * continuous axis, including binned summaries' outliers) every point sits at its
    * literal x.  Residual overlap shows honestly: thin-outlined rings merge only when
    * too close to tell apart, thickening with the count (saturating to a solid disc);
    * distinguishable overlaps simply draw and visibly cross.  Compose with `fade(a)` for
    * translucent filled dots whose overlap accumulates ink instead.
    */
  def strip: Look = visual(Visual.Kind.Strip)

  /** Box-whisker-outlier summary of y grouped by x — categorical levels directly, or bins
    * of a continuous x via `binBy(...)` — dodged within a discrete colour scale.  Whiskers
    * per [[Whisk]]: Tukey `Iqr(1.5)` by default, with data beyond drawn individually as
    * outliers.  To draw boxes from precomputed numbers instead, map the summary columns
    * directly with no stat:
    * `data(x = labs, y = medians, ylow = q1, yhigh = q3, ymin = lo, ymax = hi) * visual(Boxplot)`
    * (whiskers optional: map both `ymin` and `ymax` or neither).
    */
  def boxplot(whisk: Whisk = Whisk.Iqr()): Look =
    visual(Visual.Kind.Boxplot) * Look(null, BoxSummary(whisk) :: Nil, Style.empty)

  /** Violin — mirrored kernel density — of y grouped by x (categorical levels, or bins of
    * a continuous x via `binBy(...)`), dodged within a discrete colour scale; NaN
    * bandwidth = Silverman's rule per group.  Each violin is normalized to the same
    * maximum width.  The density covers only the whisker-fenced body (`whisk`, Tukey
    * 1.5 IQR by default); outliers draw as individual points, never as kernel bumps — a
    * bump over an outlier fakes statistical support for a gap-and-blip the data does not
    * contain.  The shape still tapers naturally past its body, but never past the fence,
    * so the taper cannot reach an outlying point.  Groups with fewer than five points
    * show their points individually: too few for a bulge to mean anything.
    * `Whisk.Extremes` opts back into whole-sample violins.
    */
  def violin(bandwidth: Double = Double.NaN, whisk: Whisk = Whisk.Iqr()): Look =
    visual(Visual.Kind.Violin) * Look(null, YDensity(bandwidth, whisk) :: Nil, Style.empty)

  /** Groups a continuous x into bins for a following summary stat: e.g.
    * `data(x = age, y = income) * binBy(10.0) * boxplot()` draws one box per decade.
    * Give `width` directly or a target `bins` count (the width snaps to a nice step);
    * summaries sit at the bin center, or at the mean or median x of the bin's contents
    * via `at`.
    */
  def binBy(width: Double = Double.NaN, bins: Int = 8, at: BinBy.At = BinBy.At.Center): Look =
    Look(null, kse.eyes.BinBy(width, bins, at) :: Nil, Style.empty)

  /** Translucent geometry for this look's layers: overlapping marks accumulate ink, so
    * density of overlap stays visible instead of vanishing under the topmost mark.
    */
  def fade(alpha: Double): Look = Look(null, Nil, Style((Style.Alpha, alpha) :: Nil))

  /** Facet by discrete columns; `col` and `row` are the reserved facet slots, stored as
    * columns like any other aesthetic.  Lengths must match the layer's data when merged.
    */
  inline def facet[C](col: C)(using ac: AsColumn[C]): Layer =
    Layer(Data(Data.Field("col", ac.column(col)) :: Nil), Look.empty)
  @targetName("facetRow")
  inline def facet[R](row: R)(using ar: AsColumn[R]): Layer =
    Layer(Data(Data.Field("row", ar.column(row)) :: Nil), Look.empty)
  @targetName("facetColRow")
  inline def facet[C, R](col: C, row: R)(using ac: AsColumn[C], ar: AsColumn[R]): Layer =
    Layer(Data.checked(Data(Data.Field("col", ac.column(col)) :: Data.Field("row", ar.column(row)) :: Nil)), Look.empty)

  def legend(title: String): Parts = Parts(Vector.empty, Vector(Parts.Config.LegendTitle(title)))

  def title(text: String): Parts = Parts(Vector.empty, Vector(Parts.Config.FigTitle(text)))

  /** A miniature figure floated over the panel area.  Placement:
    * {{{
    * inset(mini)                        // automatic: the least-obstructed corner
    * inset(mini, "ne")                  // compass anchor: "nw","n","ne","e","se","s","sw","w"
    * inset(mini, 0.55, 0.05, 0.4, 0.3)  // explicit rect in panel fractions
    * }}}
    * To guarantee the spot is data-free, make the space explicitly — e.g.
    * `axis.vert.limit(max = ...) + inset(mini, "ne")`.
    */
  def inset(fig: Figure): Parts =
    Parts(Vector.empty, Vector(Parts.Config.Inset(fig, Place.Auto(0.38, 0.35))))
  def inset(fig: Figure, at: Compass, w: Double = 0.38, h: Double = 0.35): Parts =
    Parts(Vector.empty, Vector(Parts.Config.Inset(fig, Place.At(at, w, h))))
  def inset(fig: Figure, x: Double, y: Double, w: Double, h: Double): Parts =
    Parts(Vector.empty, Vector(Parts.Config.Inset(fig, Place.Exact(x, y, w, h))))

  val axis: AxisVocab = AxisVocab()

  val panels: PanelsVocab = PanelsVocab()

  val note: NoteWord = NoteWord()

  /** A user-anchored arrow in data coordinates, tail to tip, with an optional label hung
    * off the tail.  `backoff` (px) pulls the tip short of the aim point; `radius` (px)
    * bows the shaft — positive to the traveler's left, negative right; the head stays
    * straight and the bend begins only behind it.  Restyle via [[ArrowShape]].  The arrow
    * draws in every panel whose axes contain both endpoints; if none can, rendering fails
    * loudly.
    */
  def arrow(fromX: Double, fromY: Double, toX: Double, toY: Double, label: String = "",
            backoff: Double = 0.0, radius: Double = Double.NaN,
            color: String = "#3F3F3F", alpha: Double = 1.0, shape: ArrowShape = ArrowShape()): Parts =
    Parts(Vector.empty, Vector(Parts.Config.Arrow(label, fromX, fromY, toX, toY, backoff, radius, color, alpha, shape)))

  /** Placeholder recipe: a line look.  Becomes a real recipe (default x = index, scales,
    * style) once interpretation exists.
    */
  def timeseries: Look = visual(Visual.Kind.Line)

  final val Scatter = Visual.Kind.Scatter
  final val Line = Visual.Kind.Line
  final val Band = Visual.Kind.Band
  final val Area = Visual.Kind.Area
  final val Bar = Visual.Kind.Bar
  final val Segment = Visual.Kind.Segment
  final val Arrow = Visual.Kind.Arrow
  final val Strip = Visual.Kind.Strip
  final val Boxplot = Visual.Kind.Boxplot
  final val Violin = Visual.Kind.Violin

  type Loess = kse.eyes.Loess
  final val Loess = kse.eyes.Loess
  type Kernel = kse.eyes.Kernel
  final val Kernel = kse.eyes.Kernel
  type Rolling = kse.eyes.Rolling
  final val Rolling = kse.eyes.Rolling
  type RollingMedian = kse.eyes.RollingMedian
  final val RollingMedian = kse.eyes.RollingMedian
  type Fit = kse.eyes.Fit
  final val Fit = kse.eyes.Fit
  type ArrowShape = kse.eyes.ArrowShape
  final val ArrowShape = kse.eyes.ArrowShape
  type Whisk = kse.eyes.Whisk
  final val Whisk = kse.eyes.Whisk
  final val BinAt = kse.eyes.BinBy.At


/** Figure entry point:
  * {{{
  * Fig: fig =>
  *   import fig.*
  *   data((y = xs)) * timeseries + legend("...") + axis.vert.limit(min = 0.0)
  * }}}
  * The scope object carries the vocabulary (and, later, ambient theme/defaults); the block
  * may end in `Parts`, a `Layer`, or `Layers`.
  */
object Fig extends Vocabulary:
  final class Scope private[Fig] () extends Vocabulary

  def apply(f: Scope => (Parts | Layer | Layers)): Figure =
    val parts = f(new Scope()) match
      case p: Parts   => p
      case l: Layer   => Parts.of(l)
      case ls: Layers => Parts.of(ls)
    Figure(parts)
