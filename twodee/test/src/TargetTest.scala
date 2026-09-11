// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr

package kse.test.twodee


import java.lang.{Math => jm}
import java.awt.image.BufferedImage
import java.nio.file.Files

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import kse.twodee.*


@RunWith(classOf[JUnit4])
class TargetTest {
  import kse.basics.testutilities.TestUtilities.{given, _}
  import kse.flow.{given, _}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  private def rgb(img: BufferedImage, x: Int, y: Int): (Int, Int, Int) =
    val p = img.getRGB(x, y)
    ((p >> 16) & 0xFF, (p >> 8) & 0xFF, p & 0xFF)

  // inclusive bounds of the non-white pixels; x1 < x0 when there are none
  private def inkBox(img: BufferedImage): (Int, Int, Int, Int) =
    var x0 = img.getWidth
    var y0 = img.getHeight
    var x1 = -1
    var y1 = -1
    var y = 0
    while y < img.getHeight do
      var x = 0
      while x < img.getWidth do
        if (img.getRGB(x, y) & 0xFFFFFF) != 0xFFFFFF then
          if x < x0 then x0 = x
          if x > x1 then x1 = x
          if y < y0 then y0 = y
          if y > y1 then y1 = y
        x += 1
      y += 1
    (x0, y0, x1, y1)

  private val xs = Array.tabulate(50)(i => i.toDouble)
  private val ys = xs.map(x => 3.0 + 2.0 * jm.sin(x * 0.2))
  private def fig = Fig: f =>
    import f.*
    data(x = xs, y = ys) * visual(Line) + title("Java2D") + axis.horz.title("x") + axis.vert.title("y")

  @Test
  def imageTest(): Unit =
    val img = fig.image(400, 300).get
    T ~ img.getWidth ==== 400
    T ~ img.getHeight ==== 300
    T ~ rgb(img, 0, 0) ==== (255, 255, 255)
    T ~ rgb(img, 399, 299) ==== (255, 255, 255)
    val (x0, y0, x1, y1) = inkBox(img)
    T ~ (x1 - x0 > 300) ==== true
    T ~ (y1 - y0 > 200) ==== true
    val big = fig.image(200, 100, scale = 2.5).get
    T ~ big.getWidth ==== 500
    T ~ big.getHeight ==== 250
    val board = (fig | fig).image(400, 200).get
    T ~ board.getWidth ==== 400
    T ~ nice(Java2D.Image(0.0)).isAlt ==== true

  @Test
  def primitiveInkTest(): Unit =
    val gs = List(
      Glyph.Box(10, 10, 20, 20, "#0072B2"),
      Glyph.Box(40, 10, 20, 20, "#FF0000", alpha = 0.5),
      Glyph.Disc(80, 20, 8, "#00FF00"),
      Glyph.Ring(110, 20, 8, "#000000", 2),
      Glyph.Box(130, 10, 20, 20, "#FFF"),
      Glyph.Segment(160, 10, 160, 30, "#000000", 4),
      Glyph.Poly(Array(180.0, 200.0, 180.0), Array(10.0, 20.0, 30.0), "#0000FF"),
      Glyph.Polyline(Array(210.0, 230.0), Array(20.0, 20.0), "#000000", 4)
    )
    val img = Java2D.Image().render(240, 40, gs)
    T ~ rgb(img, 20, 20) ==== (0x00, 0x72, 0xB2)
    val (r, g, b) = rgb(img, 50, 20)
    T ~ (jm.abs(r - 255) <= 1 && jm.abs(g - 128) <= 1 && jm.abs(b - 128) <= 1) ==== true
    T ~ rgb(img, 80, 20) ==== (0, 255, 0)
    T ~ rgb(img, 110, 20) ==== (255, 255, 255)
    T ~ (rgb(img, 118, 20)._1 < 64) ==== true
    T ~ rgb(img, 140, 20) ==== (255, 255, 255)
    T ~ rgb(img, 160, 20) ==== (0, 0, 0)
    T ~ rgb(img, 157, 20) ==== (255, 255, 255)
    T ~ rgb(img, 185, 20) ==== (0, 0, 255)
    T ~ rgb(img, 220, 20) ==== (0, 0, 0)
    T ~ rgb(img, 220, 25) ==== (255, 255, 255)

  @Test
  def paintParseTest(): Unit =
    def p(s: String): Long = Paint.parse(s).fold(c => c.unwrap.toLong & 0xFFFFFFFFL)(_ => -1L)
    T ~ p("#F00") ==== 0xFFFF0000L
    T ~ p("#F008") ==== 0x88FF0000L
    T ~ p("#0072B2") ==== 0xFF0072B2L
    T ~ p("#0072b280") ==== 0x800072B2L
    T ~ p("#ffffffff") ==== 0xFFFFFFFFL
    T ~ p(" Red ") ==== 0xFFFF0000L
    T ~ p("ALICEBLUE") ==== 0xFFF0F8FFL
    T ~ p("transparent") ==== 0L
    T ~ p("rgb(255, 0, 0)") ==== 0xFFFF0000L
    T ~ p("rgb(100%, 0%, 50%)") ==== 0xFFFF0080L
    T ~ p("rgba(0, 114, 178, 0.5)") ==== 0x800072B2L
    T ~ p("rgb(0 114 178 / 25%)") ==== 0x400072B2L
    T ~ p("RGB(300, -5, 12.6)") ==== 0xFFFF000DL
    T ~ p("rgb(0 0 0 / .5)") ==== 0x80000000L
    T ~ p("hsl(0, 100%, 50%)") ==== 0xFFFF0000L
    T ~ p("hsl(120 100% 25%)") ==== 0xFF008000L
    T ~ p("hsl(240deg, 100%, 50%)") ==== 0xFF0000FFL
    T ~ p("hsl(0.5turn, 100%, 50%)") ==== 0xFF00FFFFL
    T ~ p("hsla(0, 0%, 50%, 1)") ==== 0xFF808080L
    T ~ p("hsl(0 0% 50% / 0)") ==== 0x00808080L
    T ~ p("hsl(-120, 100%, 50%)") ==== 0xFF0000FFL
    T ~ p("") ==== -1L
    T ~ p("#GG0000") ==== -1L
    T ~ p("#12345") ==== -1L
    T ~ p("rgb(1, 2)") ==== -1L
    T ~ p("rgb(1,2,3") ==== -1L
    T ~ p("rgb(a, b, c)") ==== -1L
    T ~ p("hsl(1 2 3 4 5)") ==== -1L
    T ~ p("nonsense") ==== -1L
    T ~ p("lab(50% 0 0)") ==== -1L
    kse.maths.colours.Rgb.byName.foreach: (k, c) =>
      T ~ p(k) ==== (0xFF000000L | c.unwrap.toLong)

  @Test
  def colourParsingTest(): Unit =
    T ~ Java2D.colour("#0072B2", 1.0).getRGB ==== 0xFF0072B2
    T ~ Java2D.colour("#ABC", 1.0).getRGB ==== 0xFFAABBCC
    T ~ Java2D.colour("#FFFFFF", 1.0).getRGB ==== 0xFFFFFFFF
    T ~ Java2D.colour("#0072B280", 1.0).getAlpha ==== 0x80
    T ~ Java2D.colour("#000000", 0.5).getAlpha ==== 128
    T ~ Java2D.colour("#0072B280", 0.5).getAlpha ==== 64
    T ~ Java2D.colour("red", 1.0).getRGB ==== 0xFFFF0000
    T ~ Java2D.colour("Red", 1.0).getRGB ==== 0xFFFF0000
    T ~ Java2D.colour("rgb(0, 114, 178)", 1.0).getRGB ==== 0xFF0072B2
    T ~ Java2D.colour("rgba(0,114,178,0.5)", 1.0).getAlpha ==== 128
    T ~ Java2D.colour("hsl(120, 100%, 25%)", 1.0).getRGB ==== 0xFF008000
    T ~ Java2D.colour("transparent", 1.0).getAlpha ==== 0
    T ~ nice(Java2D.colour("nonsense", 1.0)).isAlt ==== true
    T ~ nice(Java2D.colour("#GG0000", 1.0)).isAlt ==== true
    T ~ nice(Java2D.colour("", 1.0)).isAlt ==== true
    // a colour name draws on every target, and a non-colour refuses on every target at
    // interpretation, naming itself
    val named = Fig: f =>
      import f.*
      data(x = xs, y = ys) * visual(Line) * color("red")
    T ~ named.svg(200, 100).get.contains("stroke=\"red\"") ==== true
    val img = named.image(200, 100).get
    var reds = 0
    var y = 0
    while y < 100 do
      var x = 0
      while x < 200 do
        val (r, g, b) = rgb(img, x, y)
        if r > 200 && g < 60 && b < 60 then reds += 1
        x += 1
      y += 1
    T ~ (reds > 50) ==== true
    val bad = Fig: f =>
      import f.*
      data(x = xs, y = ys) * visual(Line) * color("nonsense")
    T ~ bad.image(200, 100).isAlt ==== true
    T ~ bad.svg(200, 100).isAlt ==== true
    T ~ bad.svg(200, 100).fold(_ => "")(_.toString).contains("'nonsense'") ==== true
    val badAxis = Fig: f =>
      import f.*
      data(x = xs, y = ys) * visual(Line) + axis.horz.color("nonsense")
    T ~ badAxis.svg(200, 100).fold(_ => "")(_.toString).contains("axis.horz.color") ==== true
    val badArrow = Fig: f =>
      import f.*
      data(x = xs, y = ys) * visual(Line) + arrow(1.0, 3.0, 5.0, 3.0, label = "here", color = "nonsense")
    T ~ badArrow.svg(200, 100).fold(_ => "")(_.toString).contains("arrow 'here'") ==== true
    val okAxis = Fig: f =>
      import f.*
      data(x = xs, y = ys) * visual(Line) + axis.horz.color("steelblue", alpha = 0.5) +
        arrow(1.0, 3.0, 5.0, 3.0, color = "hsl(30, 100%, 50%)")
    T ~ okAxis.svg(200, 100).isIs ==== true
    T ~ okAxis.image(200, 100).isIs ==== true

  @Test
  def textAnchorTest(): Unit =
    def box(anchor: Glyph.Anchor, rotate: Double = 0): (Int, Int, Int, Int) =
      inkBox(Java2D.Image().render(200, 100, List(Glyph.Txt(100, 60, "Hello", 16, "#000000", anchor, rotate = rotate))))
    val (s0, sy0, s1, sy1) = box(Glyph.Anchor.Start)
    val (m0, _, m1, _) = box(Glyph.Anchor.Middle)
    val (e0, _, e1, _) = box(Glyph.Anchor.End)
    T ~ (jm.abs(s0 - 100) <= 2) ==== true
    T ~ (jm.abs((m0 + m1) / 2.0 - 100) <= 2) ==== true
    T ~ (jm.abs(e1 - 100) <= 2) ==== true
    T ~ (jm.abs((s1 - s0) - (e1 - e0)) <= 1) ==== true
    T ~ (sy1 <= 60 && sy0 < 60 - 8) ==== true
    val w = Java2D.measurer.width("Hello", 16)
    T ~ (jm.abs(w - (s1 - s0 + 1)) <= 4) ==== true
    val (r0, ry0, r1, ry1) = box(Glyph.Anchor.Middle, rotate = -90)
    T ~ (ry1 - ry0 > r1 - r0) ==== true
    T ~ (jm.abs((ry0 + ry1) / 2.0 - 60) <= 2) ==== true
    T ~ (r1 <= 100) ==== true

  @Test
  def haloTest(): Unit =
    def whites(halo: Boolean): Int =
      val gs = List(
        Glyph.Box(0, 0, 200, 100, "#000000"),
        Glyph.Txt(100, 60, "Hello", 16, "#000000", Glyph.Anchor.Middle, halo = halo)
      )
      val img = Java2D.Image().render(200, 100, gs)
      var n = 0
      var y = 0
      while y < 100 do
        var x = 0
        while x < 200 do
          if rgb(img, x, y)._1 > 200 then n += 1
          x += 1
        y += 1
      n
    T ~ (whites(true) > 100) ==== true
    T ~ whites(false) ==== 0

  @Test
  def measurerTest(): Unit =
    val m = Java2D.measurer
    T ~ m.width("", 12) ==== 0.0
    T ~ (m.width("iii", 12) < m.width("mmm", 12)) ==== true
    T ~ (jm.abs(m.width("Hello", 24) - 2 * m.width("Hello", 12)) < 0.5) ==== true
    T ~ (m.ascent(12) > 0 && m.ascent(12) < m.lineHeight(12)) ==== true
    // laid out against these metrics, every centered label still sits on the canvas
    val dense = Fig: f =>
      import f.*
      data(x = xs, y = ys) * visual(Line) + axis.horz.limit(min = 0.0, max = 10.0) + axis.horz.ticks(24)
    val gs = dense.glyphs(1900, 340)(using m).get
    var found = 0
    gs.foreach:
      case Glyph.Txt(x, _, t, sz, _, Glyph.Anchor.Middle, _, rot, _) if rot == 0 =>
        T ~ (x + m.width(t, sz) / 2 <= 1900.5) ==== true
        T ~ (x - m.width(t, sz) / 2 >= -0.5) ==== true
        found += 1
      case _ => ()
    T ~ (found >= 21) ==== true

  @Test
  def targetSeamTest(): Unit =
    T ~ fig.render(Svg, 300, 200).get ==== fig.svg(300, 200).get
    T ~ Svg.render(300, 200, fig.glyphs(300, 200).get) ==== fig.svg(300, 200).get
    val counter = new Target[Int]:
      def measurer: Measurer = Java2D.measurer
      def render(w: Double, h: Double, glyphs: List[Glyph]): Int = glyphs.length
    T ~ fig.render(counter, 300, 200).get ==== fig.glyphs(300, 200)(using Java2D.measurer).get.length
    val img = new BufferedImage(120, 80, BufferedImage.TYPE_INT_RGB)
    val g = img.createGraphics()
    g.setColor(java.awt.Color.RED)
    T ~ fig.draw(g, 120, 80).isIs ==== true
    T ~ g.getColor ==== java.awt.Color.RED
    g.dispose()
    T ~ rgb(img, 0, 0) ==== (255, 255, 255)
    val (x0, _, x1, _) = inkBox(img)
    T ~ (x1 - x0 > 80) ==== true

  @Test
  def pngTest(): Unit =
    val dir = Files.createTempDirectory("twodee-png")
    val p = dir.resolve("fig.png")
    T ~ fig.png(p, 300, 200).get ==== p
    val bytes = Files.readAllBytes(p)
    T ~ (bytes.length > 100) ==== true
    T ~ (bytes(0) & 0xFF) ==== 0x89
    T ~ new String(bytes, 1, 3, "ISO-8859-1") ==== "PNG"
    val back = javax.imageio.ImageIO.read(p.toFile)
    T ~ back.getWidth ==== 300
    T ~ back.getHeight ==== 200
    T ~ fig.png(dir.resolve("nope/fig.png"), 50, 50).isAlt ==== true
    Files.delete(p)
    Files.delete(dir)

  @Test
  def showTest(): Unit =
    val bad = Fig: f =>
      import f.*
      data(x = xs, y = ys) * visual(Line) * color("nonsense")
    T ~ bad.show(200, 150).isAlt ==== true
    if java.awt.GraphicsEnvironment.isHeadless then
      T ~ fig.show(200, 150).isAlt ==== true
    else
      fig.show(200, 150, title = "twodee test").fold{ f =>
        try
          T ~ f.getTitle ==== "twodee test"
          T ~ f.isVisible ==== true
          T ~ (f.getWidth >= 200 && f.getHeight >= 150) ==== true
        finally f.dispose()
      }{ e => assertTrue(s"show failed with a display present: $e", false) }
}
