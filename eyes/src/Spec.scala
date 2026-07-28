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
  * y, dodging side-by-side within a discrete colour scale.
  */
final case class Visual(kind: Visual.Kind)

object Visual:
  enum Kind:
    case Scatter, Line, Band, Area, Bar


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


/////////////////////////////////////
/// The figure-level Parts monoid ///
/////////////////////////////////////

/** Everything a figure is built from: layers plus config fragments.  `+` is slot-wise
  * merge — layer lists concatenate in draw order; config fragments accumulate and
  * cascade-merge at interpretation.
  */
final case class Parts(layers: List[Layer], config: List[Parts.Config]):
  def +(that: Parts): Parts = Parts(layers ::: that.layers, config ::: that.config)
  def +(that: Layer): Parts = Parts(layers :+ that, config)
  def +(those: Layers): Parts = Parts(layers ::: those.terms, config)

object Parts:
  val empty: Parts = Parts(Nil, Nil)
  def of(l: Layer): Parts = Parts(l :: Nil, Nil)
  def of(ls: Layers): Parts = Parts(ls.terms, Nil)

  enum Axis:
    case Horz, Vert

  /** Config fragment stub; the typed-key cascade (DESIGN 7) replaces this as it grows. */
  enum Config:
    case LegendTitle(title: String)
    case FigTitle(title: String)
    case AxisTitle(axis: Axis, title: String)
    case AxisLimit(axis: Axis, min: Double, max: Double)  // NaN = unset
    case FreeAxis(horz: Boolean, vert: Boolean)
    case PanelGap(horz: Double, vert: Double)
    case EachLabeled
    case Inset(fig: Figure, place: Place)


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
    Parts(Nil, Parts.Config.AxisLimit(which, min, max) :: Nil)
  def title(text: String): Parts =
    Parts(Nil, Parts.Config.AxisTitle(which, text) :: Nil)
  /** This axis fits each facet panel's own data instead of the shared domain. */
  def free: Parts =
    Parts(Nil, Parts.Config.FreeAxis(which == Parts.Axis.Horz, which == Parts.Axis.Vert) :: Nil)

final class AxisVocab private[eyes] ():
  val horz: AxisWords = AxisWords(Parts.Axis.Horz)
  val vert: AxisWords = AxisWords(Parts.Axis.Vert)
  /** Both axes free: every facet panel fits its own data. */
  def free: Parts = Parts(Nil, Parts.Config.FreeAxis(true, true) :: Nil)

/** Panel-arrangement words for facet grids. */
final class PanelsVocab private[eyes] ():
  def gap(both: Double): Parts = gap(both, both)
  def gap(horz: Double, vert: Double): Parts = Parts(Nil, Parts.Config.PanelGap(horz, vert) :: Nil)
  /** Every panel gets its own tick labels (scales still shared unless axes are free). */
  def eachLabeled: Parts = Parts(Nil, Parts.Config.EachLabeled :: Nil)


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

  /** Binned bars of the x distribution: `visual(Bar) * bin(bins)`. */
  def histogram(bins: Int = 30): Look = visual(Visual.Kind.Bar) * bin(bins)

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

  def legend(title: String): Parts = Parts(Nil, Parts.Config.LegendTitle(title) :: Nil)

  def title(text: String): Parts = Parts(Nil, Parts.Config.FigTitle(text) :: Nil)

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
    Parts(Nil, Parts.Config.Inset(fig, Place.Auto(0.38, 0.35)) :: Nil)
  def inset(fig: Figure, at: Compass, w: Double = 0.38, h: Double = 0.35): Parts =
    Parts(Nil, Parts.Config.Inset(fig, Place.At(at, w, h)) :: Nil)
  def inset(fig: Figure, x: Double, y: Double, w: Double, h: Double): Parts =
    Parts(Nil, Parts.Config.Inset(fig, Place.Exact(x, y, w, h)) :: Nil)

  val axis: AxisVocab = AxisVocab()

  val panels: PanelsVocab = PanelsVocab()

  /** Placeholder recipe: a line look.  Becomes a real recipe (default x = index, scales,
    * style) once interpretation exists.
    */
  def timeseries: Look = visual(Visual.Kind.Line)

  final val Scatter = Visual.Kind.Scatter
  final val Line = Visual.Kind.Line
  final val Band = Visual.Kind.Band
  final val Area = Visual.Kind.Area
  final val Bar = Visual.Kind.Bar

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
