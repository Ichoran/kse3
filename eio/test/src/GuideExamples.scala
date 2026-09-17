// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.eio

// Every example in GUIDE-eio.md lives here between a `// guide: name` and `// guide: end` pair,
// so that the guide's code is compiled and run.  `check-guides.py` verifies the two stay identical.
// The socket and shared-memory examples are compiled but not run: they need native access and a peer.

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import java.nio.file.{Files, Path}

import kse.basics.{given, *}
import kse.flow.{given, *}
import kse.maths.{given, *}
import kse.eio.{given, *}
import kse.eio.cleasy.*

object GuideExamples {

  // guide: paths.plain
  import java.nio.file.StandardCopyOption
  import scala.jdk.CollectionConverters.*
  def countLinesPlain(p: Path): Either[String, Int] =
    try Right(Files.readAllLines(p).size)
    catch case e: java.io.IOException => Left(s"$p: ${e.getMessage}")
  def replacePlain(p: Path, lines: Seq[String]): Either[String, Unit] =
    try
      val tmp = p.resolveSibling(p.getFileName.toString + ".tmp")
      Files.write(tmp, lines.asJava): Unit
      Files.move(tmp, p, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING): Unit
      Right(())
    catch case e: java.io.IOException => Left(s"$p: ${e.getMessage}")
  // guide: end

  // guide: paths.kse3
  def countLines(p: Path): Ask[Int] = p.slurp.map(_.length)                        // slurp is Ask[Array[String]]
  def replace(p: Path, lines: Seq[String]): Ask[Unit] = p.atomically.writeLines(lines)   // temp file, fsync, rename
  // guide: end

  def siblings(dir: Path): (Path, Path, String, Array[Path]) =
    // guide: pathnames.kse3
    val p = dir / "eel.csv"                       // resolve; "eel.csv".path makes one from a String
    val q = p.extTo("tsv")                        // eel.tsv, beside it
    val r = p.nameOp("old-" + _)                  // old-eel.csv
    val stem = p.base                             // "eel"; p.ext is "csv", p.parent is a Path Or Unit
    val csvs = dir.paths.select(_.ext == "csv")   // the entries of a directory, or empty if it isn't one
    // guide: end
    (q, r, stem, csvs)

  def firstByte(p: Path): Ask[Int] =
    // guide: pathopen.kse3
    Resource.nice(p.openRead())(_.close)(in => in.read())   // closed however the block ends; openWrite, openAppend likewise
    // guide: end

  def workspace(f: Path => Unit): Ask[Boolean] = Ask:
    // guide: pathdirs.kse3
    val dir = Files.createTempDirectory("guide")
    (dir / "work" / "data").mkdirs().?            // parents included
    f(dir / "work")
    dir.recursively.delete()                      // everything under dir, then dir; refuses to escape its root
    !dir.exists
    // guide: end


  // guide: xsv.plain
  def tablePlain(text: String): Array[Array[String]] =
    text.split("\n").map(_.split(",", -1))        // wrong the moment a cell is quoted
  // guide: end

  // guide: xsv.kse3
  def table(text: String): Ask[Array[Array[String]]] = Csv.decode(text)   // quotes, embedded commas and newlines, CRLF
  // guide: end

  def roundTrip(p: Path, rows: Array[Array[String]]): Ask[Array[Array[String]]] = Ask:
    // guide: xsvfiles.kse3
    Csv.write(rows)(p).?                          // quotes only the cells that need it
    val back = Csv.read(p).?                      // Tsv likewise; Xsv.semi, Xsv.space, Xsv.trimComma for other shapes
    // guide: end
    back


  // guide: parse.plain
  def ratioPlain(a: String, b: String): Either[String, Double] =
    try
      val x = a.toInt
      val y = b.toInt
      if y == 0 then Left("zero denominator") else Right(x.toDouble / y)
    catch case e: NumberFormatException => Left(e.getMessage)
  // guide: end

  // guide: parse.kse3
  def ratio(a: String, b: String): Ask[Double] = Ask:
    val x = a.parseI_?                            // the whole string must be the number; leaves with the Err if not
    val y = b.parseI_?
    if y == 0 then Err ?# "zero denominator"
    x.toDouble / y
  // guide: end

  def widths(s: String, h: String): (Ask[Int], Ask[Int], Ask[Double]) =
    // guide: parseask.kse3
    val n = s.parseI                              // Ask[Int]; Int.from(s) is the same call
    val bits = h.parseXI                          // a hex bit pattern, no 0x: "ff" is 255 and "ffffffff" is -1
    val d = s.parseD                              // correctly rounded; parseL, parseUL, parseF, and so on likewise
    // guide: end
    (n, bits, d)


  // guide: grok.plain
  def recordsPlain(text: String): Either[String, List[(String, Double)]] =
    try
      val rs = text.linesIterator.filter(_.trim.nonEmpty).map{ line =>
        val ws = line.trim.split("\\s+")
        if ws.length != 2 then throw new IllegalArgumentException(s"bad line: $line")
        (ws(0), ws(1).toDouble)
      }
      Right(rs.toList)
    catch case e: IllegalArgumentException => Left(e.getMessage)   // NumberFormatException is one
  // guide: end

  // guide: grok.kse3
  def records(text: String): Ask[List[(String, Double)]] = Grok.all(text){ g =>   // tokens are lines; within one, words
    var rs = List.empty[(String, Double)]
    while g.sp.hasMore do rs = g.grok(){ (g.tok, g.D) } :: rs   // each line is its own sub-parse
    rs.reverse
  }
  // guide: end

  def stamp(s: String): Ask[(Int, Int, Int, String)] =
    // guide: grokdate.kse3
    Grok(s, Delim.white, partial = true){ g => (g.I, (g < '-').I, (g < '-').I, g.tok) }   // "2026-09-16 eel"
    // guide: end

  def answer(s: String): Ask[String] =
    // guide: grokselect.kse3
    Grok(s, Delim.white){ g =>
      g.select(                                   // the first alternative to parse wins; each starts at the same cursor
        (g.I + g.I).toString,                     // "3 4" gives "7"
        { g.skip(1); g.D.toString },              // "x 2.5" gives "2.5"
        g.tok_?(_ == "none", "expected none")     // "none" gives "none"; anything else fails with all three attempts
      )
    }
    // guide: end


  // guide: cleasy.plain
  def optionsPlain(args: Array[String]): Either[String, (Int, Boolean, List[String])] =
    var n = 4
    var verbose = false
    var rest = List.empty[String]
    var i = 0
    try
      while i < args.length do
        args(i) match
          case "-n" => n = args(i + 1).toInt; i += 1
          case s if s.startsWith("--count=") => n = s.drop(8).toInt
          case "-v" | "--verbose" => verbose = true
          case s => rest = s :: rest
        i += 1
      Right((n, verbose, rest.reverse))
    catch case e: RuntimeException => Left(e.getMessage)   // NumberFormatException, ArrayIndexOutOfBoundsException
  // guide: end

  // guide: cleasy.kse3
  val spec = Cleasy("Count fish.")
    -- "count" ~ 'n' ~ (_int, () => 4) % "How many to count"   // --count=7 or -n 7; 4 when absent
    -- "verbose" ~ 'v' % "Say more"                              // a flag: --verbose or -v
  def options(args: Array[String]): Ask[(Int, Boolean, List[String])] =
    spec.parse(args).map(a => (a("count"), a.found("verbose"), a.args.toList))   // typed by name; args is what's left
  // guide: end


  // guide: streams.plain
  def copyPlain(in: java.io.InputStream, out: java.io.OutputStream): Long =
    val buf = new Array[Byte](8192)
    var total = 0L
    var n = in.read(buf)
    while n >= 0 do
      out.write(buf, 0, n)
      total += n
      n = in.read(buf)
    total
  // guide: end

  // guide: streams.kse3
  def copy(in: java.io.InputStream, out: java.io.OutputStream): Ask[Long] = in.sendTo(out)   // streams, channels, iterators
  // guide: end

  def views(text: String): (Ask[Long], Int) =
    // guide: streamviews.kse3
    val bytes = text.bytes                        // UTF-8; bytes.utf8 goes back
    val sink = new Array[Byte](64)
    val moved = bytes.input().sendTo(sink.output())   // a ByteArrayInputStream into a bounded OutputStream over sink
    val first = bytes.buffer().getInt             // a little-endian ByteBuffer over the same array
    // guide: end
    (moved, first)

  def lines(p: Path, ls: List[String]): Ask[Array[String]] = Ask:
    // guide: streamlines.kse3
    ls.writeTo(p).?                               // any collection of String or Array[Byte]; appendTo and createAt too
    p.slurp.?
    // guide: end


  // guide: encode.plain
  def hexPlain(bs: Array[Byte]): String = bs.map(b => f"$b%02X").mkString
  def base64Plain(bs: Array[Byte]): String = java.util.Base64.getUrlEncoder.encodeToString(bs)
  // guide: end

  // guide: encode.kse3
  def hex(bs: Array[Byte]): String = bs.stringEncodeHex          // "DEADBEEF"; stringEncodeHexLo for lower case
  def base64(bs: Array[Byte]): String = bs.stringEncode64        // the URL-safe alphabet; encode64basic is the standard one
  def unhex(s: String): Ask[Array[Byte]] = s.decodeHex           // an Err, not an exception, for a bad digit or odd length
  // guide: end


  // guide: fdsock.kse3
  def ping(sock: Path): Ask[String] =
    Resource.Nice(FdSock.connect(sock, 5.s))(_.close()){ conn =>
      conn.write("ping".bytes).?
      val buf = new Array[Byte](64)
      val n = conn.read(buf).?                    // bounded by the timeout given at connect
      new String(buf, 0, n)
    }
  // guide: end

  // guide: shm.kse3
  def offer(sock: Path): Ask[Unit] =
    Resource.Nice(SharedMemory.offerFd[Long](sock, 1024))(_.close()){ later =>
      later.use(_.use(_.set()(i => i * i)))       // fill the region; the Tidy.Later owns it until close
      later.op(_.serveOne()).?                    // one peer connects at sock and receives the descriptor
    }
  def accept(sock: Path): Ask[Long] =
    Resource.nice(SharedMemory.acceptFd[Long](sock))(_.close()){ view =>
      view.op(m => m(3))                          // a Mem.Owned over the same physical memory: 9
    }
  // guide: end
}


@RunWith(classOf[JUnit4])
class GuideTest {
  import kse.basics.testutilities.TestUtilities.{_, given}
  import GuideExamples as G

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  private def scratch(f: Path => Unit): Unit =
    val dir = Files.createTempDirectory("kse3-guide")
    try f(dir)
    finally dir.recursively.delete()

  @Test
  def pathsTest(): Unit = scratch { dir =>
    val p = dir / "eel.txt"
    val lines = Seq("eel", "cod", "salmon")
    T ~ G.replacePlain(p, lines) ==== Right(())
    T ~ G.countLinesPlain(p) ==== Right(3)
    T ~ G.replace(p, lines :+ "gar") ==== ()
    T ~ G.countLines(p) ==== 4
    T ~ G.countLinesPlain(p).toOption ==== G.countLines(p).toOption
    T ~ G.countLinesPlain(dir / "nope.txt").isLeft ==== G.countLines(dir / "nope.txt").isAlt
    val (q, r, stem, csvs) = G.siblings(dir)
    T ~ q ==== dir / "eel.tsv"
    T ~ r ==== dir / "old-eel.csv"
    T ~ stem ==== "eel"
    T ~ csvs.length ==== 0
    (dir / "a.csv").touch(): Unit
    T ~ G.siblings(dir)._4.map(_.name).toList ==== List("a.csv")
    T ~ G.firstByte(p) ==== 'e'.toInt
    T ~ G.workspace(d => (d / "x.txt").touch() __ Unit) ==== true
  }

  @Test
  def xsvTest(): Unit = scratch { dir =>
    val simple = "eel,3\ncod,4"
    T ~ G.table(simple).map(_.map(_.toList).toList) ==== G.tablePlain(simple).map(_.toList).toList
    T ~ G.table("\"Smith, J\",3\ncod,4").map(_.map(_.toList).toList) ==== List(List("Smith, J", "3"), List("cod", "4"))
    val rows = Array(Array("name", "n"), Array("eel, the", "3"))
    T ~ G.roundTrip(dir / "t.csv", rows).map(_.map(_.toList).toList) ==== rows.map(_.toList).toList
  }

  @Test
  def parsingTest(): Unit =
    for (a, b) <- List(("6", "4"), ("6", "0"), ("six", "4"), ("6", "")) do
      T ~ G.ratioPlain(a, b).toOption ==== G.ratio(a, b).toOption
    T ~ G.ratio("6", "4") ==== 1.5
    val (n, bits, d) = G.widths("12", "ffffffff")
    T ~ n ==== 12
    T ~ bits ==== -1
    T ~ d ==== 12.0

  @Test
  def grokTest(): Unit =
    val text = "eel 3.5\ncod 12\n\nsalmon 0.25\n"
    T ~ G.records(text).toOption ==== G.recordsPlain(text).toOption
    T ~ G.records(text) ==== List(("eel", 3.5), ("cod", 12.0), ("salmon", 0.25))
    T ~ G.records("eel x").isAlt ==== G.recordsPlain("eel x").isLeft
    T ~ G.stamp("2026-09-16 eel") ==== (2026, 9, 16, "eel")
    T ~ G.stamp("2026-09 eel").isAlt ==== true
    T ~ G.answer("3 4") ==== "7"
    T ~ G.answer("x 2.5") ==== "2.5"
    T ~ G.answer("none") ==== "none"
    T ~ G.answer("x y").isAlt ==== true

  @Test
  def cleasyTest(): Unit =
    for args <- List(Array("-n", "7", "eel", "--verbose", "cod"), Array("--count=9", "-v", "salmon"), Array("eel"), Array("-n", "x")) do
      T ~ G.options(args).toOption ==== G.optionsPlain(args).toOption
    T ~ G.options(Array("-n", "7", "eel", "--verbose", "cod")) ==== (7, true, List("eel", "cod"))
    T ~ G.options(Array("--eel")).isAlt ==== true
    T ~ G.spec.userString().contains("count") ==== true

  @Test
  def streamsTest(): Unit = scratch { dir =>
    val data = "eel and cod".bytes
    val a = new java.io.ByteArrayOutputStream()
    val b = new java.io.ByteArrayOutputStream()
    T ~ G.copyPlain(data.input(), a) ==== 11L
    T ~ G.copy(data.input(), b) ==== 11L
    T ~ a.toByteArray.toList ==== b.toByteArray.toList
    val (moved, first) = G.views("eel and cod")
    T ~ moved ==== 11L
    T ~ first ==== java.nio.ByteBuffer.wrap(data).order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt
    T ~ G.lines(dir / "l.txt", List("eel", "cod")).map(_.toList) ==== List("eel", "cod")
  }

  @Test
  def encodingsTest(): Unit =
    val bs = Array(0xDE, 0xAD, 0xBE, 0xEF).map(_.toByte)
    T ~ G.hex(bs) ==== G.hexPlain(bs)
    T ~ G.hex(bs) ==== "DEADBEEF"
    T ~ G.base64(bs) ==== G.base64Plain(bs)
    T ~ G.unhex(G.hex(bs)).map(_.toList) ==== bs.toList
    T ~ G.unhex("DEADBEE").isAlt ==== true
}
