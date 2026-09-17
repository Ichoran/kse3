# kse3 twodee

The twodee module is static two-dimensional plotting, in the spirit of Makie and AlgebraOfGraphics.  A figure is an
expression: `data(...)` bundles named columns, a look says how to draw them, `*` joins a layer's data to its look
and `+` superposes layers, and titles, axes, notes, and insets add on with `+` as well.  Interpretation happens at
render: scales are chosen, ticks are laid on a 1-2-5 grid so a round number in the data always lands on a tick,
labels are sized to the figure and never allowed to touch, and anything the library cannot draw honestly is refused
with an `Err` that names the word to fix, rather than drawn wrong.  Output is SVG text with no dependencies, or
Java2D for a PNG, a window, or any `Graphics2D`.  Twodee is in `all`, not `foundation`: it is still moving, and it may
take outside dependencies.

`import kse.twodee.{given, *}` brings in `Fig`, `Figure`, `Board`, and the types.  The words themselves (`data`,
`visual`, `title`, `axis`, ...) live on the scope a `Fig` block receives, so a block opens with `import f.*`; for a
whole file, `import kse.twodee.Fig.*` once.  Rendering answers `Ask`, so `kse.flow` is wanted alongside.

<!-- guide examples: twodee/test/src/GuideExamples.scala -->
Every code example below is compiled and run from `twodee/test/src/GuideExamples.scala`; `mill guides.run` verifies
that the two copies are identical.

## What's here

- **Figures**: `Fig: f => ...` with `import f.*`; `svg`, `image`, `png`, `draw`, and `show` to render, each an `Ask`.
- **Layers and looks**: `data(x = ..., y = ...)`, `data.from(rows)(...)`, `visual(Line)` and the other kinds, `color`, `fade`, and the `*` and `+` algebra.
- **Statistics**: `smooth(Loess())`, `histogram`, `density`, `count`, `boxplot`, `violin`, `binBy`, computed per colour level and facet.
- **Axes and annotations**: `title`, `legend`, `axis.horz` and `axis.vert` words, `note`, `arrow`.
- **Facets, boards, and insets**: `facet(col = ...)`, `axis.free`, `panels`, `a | b` and `a / b`, `inset(mini, "ne")`.

## Figures

`Fig: f => ...` builds a `Figure` from an expression in the vocabulary that `f` carries.  A layer is columns times a
look, `data(x = xs, y = ys) * visual(Line)`; titles, axis words, notes, and insets are parts, added with `+`.
Nothing is drawn until a target is named: `svg(width, height)` gives SVG text, `image` a `BufferedImage` and
`png(path)` a file through Java2D, `draw(g)` paints onto any `Graphics2D`, and `show()` opens a window.  Each
answers an `Ask`, and a figure that can't be drawn honestly (a stat on a layer that maps `y`, a colour string that
names no colour, a note with no panel to land in) is refused with a message that says which word is wrong.  Render
is where scales are chosen, ticks laid on the 1-2-5 grid, labels sized and kept apart, and margins computed from
the labels actually drawn, so the result is the same at any size.

**Reach for this when** you'd write out a CSV and open a notebook, or shell out to gnuplot.

<!-- guide: figure.kse3 -->
```scala
def trend(days: Array[Double], temps: Array[Double]): Figure =
  Fig: f =>
    import f.*                                        // the vocabulary: data, visual, title, axis, note, ...
    data(x = days, y = temps) * visual(Line) +        // a layer is columns times a look
      title("Water temperature") + axis.horz.title("day") + axis.vert.title("°C")
```

<!-- guide: render.kse3 -->
```scala
fig.svg(640, 480)                                   // Ask[String]: refuses with an Err rather than draw a lie
```

Columns are plain arrays, held by reference and not copied.  `data(x = xs, y = ys)` takes two or more columns; a
single column is written `data((y = ys))`, and a layer with no `x` plots against the index.  Column lengths are
checked whenever bundles combine.  Colours are CSS: `#RRGGBB`, a name, `rgb()`, or `hsl()`, checked at
interpretation on every target alike.

Full API: `twodee/src/Spec.scala`, where the `Vocabulary` trait documents every word; `twodee/src/Render.scala` for
interpretation and the `svg`, `image`, `png`, `draw`, and `show` extension.

## Layers and looks

`*` and `+` are the algebra.  `data(...)` is a layer with no look and `visual(kind)` a look with no data; a look
times a layer is a layer that draws; `look + look` is a sum of looks, and a layer times a sum is one layer per look
over the same columns, drawn in order.  `data.from(rows)(r => (x = r.day, y = r.temp))` pulls columns out of an
array of records with one function.  A column named `color` colours by it: a discrete column picks from the
Okabe-Ito palette and gets a legend, a continuous one runs through viridis and gets a colour bar.
`color("#0072B2")` is a styled constant that an unmapped layer takes, so a band and the line through it share a
hue, and `fade(alpha)` makes marks translucent so overlap accumulates.  The visual kinds are `Scatter`, `Line`,
`Band` (`ylow` to `yhigh`), `Area`, `Bar`, `Segment` and `Arrow` (`xend` and `yend`, one row per edge, so a
network is data-sized), `Strip` (points exactly where they lie, never jittered), `Boxplot`, and `Violin`.

**Reach for these when** you'd draw the same data twice with two plotting calls, or loop over groups to colour them.

<!-- guide: layers.kse3 -->
```scala
case class Reading(day: Double, temp: Double, region: String)
def byRegion(rows: Array[Reading]): Figure =
  Fig: f =>
    import f.*
    val base = data.from(rows)(r => (x = r.day, y = r.temp, color = r.region))   // columns pulled from rows once
    base * (visual(Scatter) + visual(Line) * smooth(Loess())) +                  // points, then a smoothed line, per region
      legend("Region") + axis.horz.title("day")
```

A look chosen at runtime composes the same way: a `Look` and a sum of looks are both a `Looking`, a `Layer` and a
sum of layers both a `Layered`, and `*` and `+` accept either side, so the choice needs no cast and no duplicated
expression.

<!-- guide: chosen.kse3 -->
```scala
def profile(xs: Array[Double], ys: Array[Double], joined: Boolean): Figure =
  Fig: f =>
    import f.*
    val look = if joined then visual(Line) + visual(Scatter) else visual(Scatter)   // a Looking: one Look, or a sum of them
    data(x = xs, y = ys) * look + axis.horz.title("distance") + axis.vert.title("value")
```

When an attribute is both mapped and styled, the column wins; when neither, the theme default applies.  A layer
mapping a column the visual doesn't use is fine, and one missing a column the visual needs is refused.

Full API: `Look`, `Layer`, `Layers`, and `Visual.Kind` in `twodee/src/Spec.scala`.

## Statistics

A stat is a look that computes columns before drawing, separately for each colour level and facet cell.
`smooth(Loess())` draws a curve through `y` (also `Kernel(bandwidth)`, `Rolling(window)`, `RollingMedian`, and
`Fit(degree)` for a polynomial).  `histogram(bins)`, `density()`, and `count` make `y` from `x`, so a layer that
maps `y` with one of them is refused.  `boxplot()` and `violin()` summarize `y` for each level of a categorical `x`,
dodged by colour, with outliers drawn as points and never as bumps in a density; `binBy(width)` groups a
continuous `x` so those work on it too.  Bins snap to round widths on shared edges, so dodged groups align, and a
kernel bandwidth defaults to Silverman's rule.

**Reach for these when** you'd compute the histogram or the fit yourself and plot the result as ordinary lines.

<!-- guide: stats.kse3 -->
```scala
def distributions(values: Array[Double], arm: Array[String]): Board =
  val hist = Fig: f =>
    import f.*
    data(x = values, color = arm) * histogram(18) + axis.vert.title("count")   // the stat makes y; don't map it
  val boxes = Fig: f =>
    import f.*
    data(x = arm, y = values) * boxplot() + axis.vert.title("value")            // categorical x: a box per level
  hist | boxes                                                                   // a board: beside; / stacks
```

A summary can also be drawn from numbers you already have: map `y`, `ylow`, `yhigh`, and optionally `ymin` and
`ymax` and use `visual(Boxplot)` with no stat.

Full API: the stat case classes in `twodee/src/Spec.scala`; the kernels in `maths/src/Smoothing.scala`.

## Axes and annotations

`title(text)` names the figure and `legend(title)` its colour key.  `axis.horz` and `axis.vert` carry `title`,
`limit(min, max)` (either end), `ticks(n)` to ask for a denser or sparser grid (labels still never collide),
`minorTicks`, `minorGrid`, and `color(c, alpha)` for the frame and tick ink.  `note(text, x, y)` is a callout: an
arrow to the point and a haloed label placed where the panel's own geometry is sparse, later notes avoiding
earlier ones; `note.x(text, at)` and `note.y` point at a position on one axis; `arrow(x1, y1, x2, y2)` is an arrow
you anchor yourself, with an optional tail label.  `radius` bows a shaft and an `ArrowShape` restyles the head.

**Reach for these when** you'd position a text label by trial and error, or draw a line and a triangle to make an
arrow.

<!-- guide: notes.kse3 -->
```scala
def annotated(days: Array[Double], temps: Array[Double], peak: Int): Figure =
  Fig: f =>
    import f.*
    data(x = days, y = temps) * visual(Line) * color("#0072B2") +
      note("warmest", x = days(peak), y = temps(peak)) +   // a callout, its label placed where the panel is clear
      note.x("sensor moved", 30.0) +                        // pointing at a spot on the axis
      axis.vert.limit(min = 0.0) + axis.horz.minorGrid(true)
```

A note's target joins the axis fit, and in a faceted figure it appears in every panel whose scales contain it; if
no panel does, rendering fails rather than dropping the annotation.

Full API: `AxisWords`, `NoteWord`, and `arrow` in `twodee/src/Spec.scala`.

## Facets, boards, and insets

`facet(col = levels)`, `facet(row = ...)`, or both split a layer into panels by a discrete column.  Panels share
scales unless `axis.free` (or `axis.horz.free`) lets each fit its own data; `panels.gap(px)` spaces them and
`panels.eachLabeled` gives every panel its own tick labels.  Whole figures compose onto one canvas: `a | b` puts
them beside each other and `a / b` stacks them, `/` binding tighter, each figure keeping its own scales, legend,
and titles; a `Board` renders exactly like a `Figure`.  `inset(mini, "ne")` floats a miniature figure over a panel
at a compass point, sized as fractions of the panel; with no anchor it takes the least-obstructed corner.  There is
no automatic space reservation on purpose: make the room with `axis.vert.limit(max = ...)` and anchor the inset in
it.

**Reach for these when** you'd make one figure per group and line them up by hand.

<!-- guide: facets.kse3 -->
```scala
def faceted(rows: Array[Reading], half: Array[String]): Figure =
  Fig: f =>
    import f.*
    data.from(rows)(r => (x = r.day, y = r.temp, color = r.region)) * visual(Scatter) * facet(col = half) +
      axis.free + panels.gap(12.0)                      // one panel per level of half, each fitting its own data
```

Full API: `Board`, `Place`, and the facet, panel, and inset words in `twodee/src/Spec.scala`; `twodee/src/Grid.scala`
is the layout solver underneath.
