// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr

package kse.twodee


import java.lang.{Math => jm}

import java.awt.{BasicStroke, Color, EventQueue, Font, Frame, Graphics, Graphics2D, RenderingHints => RH}
import java.awt.font.FontRenderContext
import java.awt.geom.{Ellipse2D, Line2D, Path2D, Rectangle2D}
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.util.concurrent.{ExecutionException, FutureTask}

import kse.flow.{given, _}


/** The Java2D backend: paints a display list onto any `Graphics2D` — an image bound for a
  * PNG, a component on screen, a printer page.  Text is the logical sans-serif family, the
  * face SVG output asks its viewer for, and `measurer` reports that face's metrics under
  * the rendering hints `paint` sets, so layout and ink agree by construction.  Colours are
  * whatever `Paint` reads, the same vocabulary SVG output carries to its viewer.
  */
object Java2D:
  private val frc = new FontRenderContext(null, true, true)
  private val plain = new Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val heavy = new Font(Font.SANS_SERIF, Font.BOLD, 12)

  private def font(size: Double, bold: Boolean): Font = (if bold then heavy else plain).deriveFont(size.toFloat)

  /** Metrics of the face `paint` draws with. */
  val measurer: Measurer = new Measurer:
    def width(text: String, size: Double): Double =
      if text.isEmpty then 0.0 else font(size, false).getStringBounds(text, frc).getWidth
    def lineHeight(size: Double): Double = font(size, false).getLineMetrics("Ag", frc).getHeight.toDouble
    def ascent(size: Double): Double = font(size, false).getLineMetrics("Ag", frc).getAscent.toDouble

  /** The colour a display-list string names, at the given opacity on top of its own, or an
    * `IllegalArgumentException` for a string that names none (`render` turns that into an
    * `Err`; interpretation has already refused such a string in any figure).
    */
  def colour(s: String, alpha: Double): Color =
    Paint.parse(s).fold{ c =>
      val a0 = c.aI
      val a = if alpha >= 1 then a0 else if alpha <= 0 then 0 else jm.round(a0 * alpha).toInt
      new Color((a << 24) | (c.unwrap & 0xFFFFFF), true)
    }{ _ => throw new IllegalArgumentException(s"'$s' is not a colour; ${Paint.hint}") }

  private def path(xs: Array[Double], ys: Array[Double], closed: Boolean): Path2D.Double =
    val p = new Path2D.Double(Path2D.WIND_NON_ZERO, xs.length + 1)
    if xs.length > 0 then
      p.moveTo(xs(0), ys(0))
      var i = 1
      while i < xs.length do
        p.lineTo(xs(i), ys(i))
        i += 1
      if closed then p.closePath()
    p

  private def txt(g: Graphics2D, x: Double, y: Double, text: String, size: Double, fill: String,
                  anchor: Glyph.Anchor, bold: Boolean, rotate: Double, halo: Boolean): Unit =
    if text.isEmpty then return
    val gv = font(size, bold).createGlyphVector(frc, text)
    val adv = anchor match
      case Glyph.Anchor.Start  => 0.0
      case Glyph.Anchor.Middle => gv.getLogicalBounds.getWidth / 2
      case Glyph.Anchor.End    => gv.getLogicalBounds.getWidth
    val saved = g.getTransform
    if rotate != 0 then g.rotate(jm.toRadians(rotate), x, y)
    val tx = (x - adv).toFloat
    val ty = y.toFloat
    if halo then
      g.setColor(Color.WHITE)
      g.setStroke(new BasicStroke((size * 0.28).toFloat, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
      g.draw(gv.getOutline(tx, ty))
    g.setColor(colour(fill, 1.0))
    g.drawGlyphVector(gv, tx, ty)
    if rotate != 0 then g.setTransform(saved)

  /** Paints the display list onto `g` in figure pixels from its origin, white background
    * included.  The caller's graphics state is left as it was.
    */
  def paint(g: Graphics2D, width: Double, height: Double, glyphs: List[Glyph]): Unit =
    val h = g.create() match
      case g2: Graphics2D => g2
      case other => throw new IllegalStateException(s"not a Graphics2D: ${other.getClass.getName}")
    try
      h.setRenderingHint(RH.KEY_ANTIALIASING, RH.VALUE_ANTIALIAS_ON)
      h.setRenderingHint(RH.KEY_TEXT_ANTIALIASING, RH.VALUE_TEXT_ANTIALIAS_ON)
      h.setRenderingHint(RH.KEY_FRACTIONALMETRICS, RH.VALUE_FRACTIONALMETRICS_ON)
      h.setRenderingHint(RH.KEY_STROKE_CONTROL, RH.VALUE_STROKE_PURE)
      h.setRenderingHint(RH.KEY_RENDERING, RH.VALUE_RENDER_QUALITY)
      h.setColor(Color.WHITE)
      h.fill(new Rectangle2D.Double(0, 0, width, height))
      glyphs.foreach:
        case Glyph.Segment(x1, y1, x2, y2, stroke, w, alpha) =>
          h.setColor(colour(stroke, alpha))
          h.setStroke(new BasicStroke(w.toFloat, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER))
          h.draw(new Line2D.Double(x1, y1, x2, y2))
        case Glyph.Polyline(xs, ys, stroke, w, alpha) =>
          h.setColor(colour(stroke, alpha))
          h.setStroke(new BasicStroke(w.toFloat, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
          h.draw(path(xs, ys, false))
        case Glyph.Poly(xs, ys, fill, alpha) =>
          h.setColor(colour(fill, alpha))
          h.fill(path(xs, ys, true))
        case Glyph.Disc(x, y, r, fill, alpha) =>
          h.setColor(colour(fill, alpha))
          h.fill(new Ellipse2D.Double(x - r, y - r, 2 * r, 2 * r))
        case Glyph.Ring(x, y, r, stroke, w, alpha) =>
          h.setColor(colour(stroke, alpha))
          h.setStroke(new BasicStroke(w.toFloat))
          h.draw(new Ellipse2D.Double(x - r, y - r, 2 * r, 2 * r))
        case Glyph.Box(x, y, w, bh, fill, alpha, stroke, strokeW) =>
          h.setColor(colour(fill, alpha))
          h.fill(new Rectangle2D.Double(x, y, w, bh))
          if stroke.nonEmpty && strokeW > 0 then
            h.setColor(colour(stroke, 1.0))
            h.setStroke(new BasicStroke(strokeW.toFloat, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER))
            h.draw(new Rectangle2D.Double(x, y, w, bh))
        case Glyph.Txt(x, y, text, size, fill, anchor, bold, rotate, halo) =>
          txt(h, x, y, text, size, fill, anchor, bold, rotate, halo)
    finally h.dispose()

  /** Paints onto an existing graphics context: a component's, a printer's, an image's. */
  final class On(g: Graphics2D) extends Target[Unit]:
    def measurer: Measurer = Java2D.measurer
    def render(width: Double, height: Double, glyphs: List[Glyph]): Unit = paint(g, width, height, glyphs)

  /** Rasterizes into a fresh opaque image, `scale` device pixels per figure pixel (2 for a
    * crisp high-density PNG), so the image is `scale` times the figure size, rounded up.
    */
  final class Image(scale: Double = 1.0) extends Target[BufferedImage]:
    if !(scale > 0) then throw new IllegalArgumentException(s"image scale must be positive, not $scale")
    def measurer: Measurer = Java2D.measurer
    def render(width: Double, height: Double, glyphs: List[Glyph]): BufferedImage =
      val img = new BufferedImage(jm.max(1, jm.ceil(width * scale).toInt), jm.max(1, jm.ceil(height * scale).toInt), BufferedImage.TYPE_INT_RGB)
      val g = img.createGraphics()
      try
        g.scale(scale, scale)
        paint(g, width, height, glyphs)
      finally g.dispose()
      img

  /** Writes an image as a PNG file. */
  def png(img: BufferedImage, path: Path): Unit =
    val os = Files.newOutputStream(path)
    try
      if !javax.imageio.ImageIO.write(img, "png", os) then throw new java.io.IOException(s"no PNG writer for $path")
    finally os.close()

  /** An AWT canvas that shows a figure laid out for its own size.  It paints from an image
    * rendered at the display's device scale — one blit, so resizing does not flicker, and
    * high-density displays get real detail rather than an upscaled figure.
    */
  private final class View(fb: Figure | Board, w0: Int, h0: Int) extends java.awt.Canvas:
    setPreferredSize(new java.awt.Dimension(w0, h0))
    private var img = new BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB)
    private var imgW = 0
    private var imgH = 0
    private var imgScale = 0.0

    // AWT's update clears to the background first; we cover every pixel, so skip the flash
    override def update(g: Graphics): Unit = paint(g)

    override def paint(g: Graphics): Unit =
      val w = getWidth
      val h = getHeight
      if w > 0 && h > 0 then
        val scale = g match
          case g2: Graphics2D => jm.max(1.0, g2.getTransform.getScaleX)
          case _ => 1.0
        if w != imgW || h != imgH || scale != imgScale then
          img = rendered(w, h, scale)
          imgW = w
          imgH = h
          imgScale = scale
        val _ = g.drawImage(img, 0, 0, w, h, null)

    private def rendered(w: Int, h: Int, scale: Double): BufferedImage =
      fb.image(w, h, scale).fold(x => x){ e =>
        val lines = e.toString.linesIterator.take(12).toList
        val gs = List.newBuilder[Glyph]
        var y = 24.0
        lines.foreach: line =>
          gs += Glyph.Txt(8, y, line, 12, "#B00020", Glyph.Anchor.Start)
          y += 16
        Image(scale).render(w, h, gs.result())
      }

  /** Opens a window on the figure, laid out afresh as the window is resized; closing the
    * window disposes it and nothing else.  Throws where there is no display.
    */
  def show(fb: Figure | Board, width: Double, height: Double, title: String): Frame =
    def build(): Frame =
      val f = new Frame(title)
      f.add(new View(fb, jm.max(1, jm.round(width).toInt), jm.max(1, jm.round(height).toInt)))
      f.addWindowListener(new java.awt.event.WindowAdapter {
        override def windowClosing(e: java.awt.event.WindowEvent): Unit = f.dispose()
      })
      f.pack()
      f.setLocationByPlatform(true)
      f.setVisible(true)
      f
    if EventQueue.isDispatchThread then build()
    else
      val task = new FutureTask[Frame](() => build())
      EventQueue.invokeAndWait(task)
      try task.get()
      catch case e: ExecutionException => throw e.getCause
