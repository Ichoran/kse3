// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-23 Rex Kerr and Calico Life Sciences, LLC.

package kse.eio.test


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import java.io._
import java.nio.file._
import java.nio.file.attribute.FileTime
import java.time._
import java.util.zip._

import scala.collection.generic.IsIterable
import scala.reflect.{ClassTag, TypeTest}
import scala.util.{Try, Success, Failure}
import scala.util.control.ControlThrowable
import scala.util.boundary
import scala.util.boundary.break

import sourcecode.{Line, given}


@RunWith(classOf[JUnit4])
class EioTest {
  import kse.testutilities.TestUtilities.{given, _}
  import kse.flow.{given, _}
  import kse.maths.{given, _}
  import kse.maths.packed.{given, _}
  import kse.eio.{given, _}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue _
  )

  @Test
  def conversionTest(): Unit =
    val r: Prng = Pcg64(239751892795L)
    T ~ "fish".bytes =**= Array('f'.toByte, 'i'.toByte, 's'.toByte, 'h'.toByte)
    nFor(32){ i =>
      val s = r.validString(i)
      T ~ s.bytes.utf8 ==== s
    }
    nFor(32){ i =>
      val s = r.asciiString(i)
      T ~ s.bytes.ascii ==== s
    }
    nFor(32){ i =>
      val b = r.arrayB(i)
      T ~ b.rawString.map(_.toByte).toArray =**= b
      T ~ b.iso8859_1.map(_.toByte).toArray =**= b
    }
    val bb = "fish".bytes.buffer
    T ~ bb.remaining    ==== 4
    T ~ bb.get          ==== 'f'.toByte --: typed[Byte]
    T ~ bb.get          ==== 'i'
    T ~ bb.get          ==== 's'
    T ~ bb.get          ==== 'h'
    T ~ bb.hasRemaining ==== false
    val bis = "fish".bytes.input
    T ~ bis.available   ==== 4
    T ~ { val a = new Array[Byte](4); bis.read(a); a.utf8 } ==== "fish"
    T ~ bis.available   ==== 0

    nFor(32){ i =>
      val b = r.arrayB(i)
      T ~ b.stringEncode64.decode64.map(_.toVector) ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode64.decode64.map(_.toVector)       ==== b.toVector --: typed[Vector[Byte] Or Err]
    }
    nFor(32){ i =>
      val b = r.arrayB(i)
      T ~ b.stringEncode64basic.decode64.map(_.toVector)      ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.stringEncode64basic.decode64basic.map(_.toVector) ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode64basic.decode64.map(_.toVector)            ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode64basic.decode64basic.map(_.toVector)       ==== b.toVector --: typed[Vector[Byte] Or Err]
    }
    nFor(32){ i =>
      val b = r.arrayB(i)
      T ~ b.stringEncode64url.decode64.map(_.toVector)    ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.stringEncode64url.decode64url.map(_.toVector) ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode64url.decode64.map(_.toVector)          ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode64url.decode64url.map(_.toVector)       ==== b.toVector --: typed[Vector[Byte] Or Err]
    }
    nFor(32){ i =>
      val b = r.arrayB(if i < 16 then i else 16 + 7*(i-16))
      T ~ b.stringEncode64mime.decode64.map(_.toVector)     ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.stringEncode64mime.decode64mime.map(_.toVector) ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode64mime.decode64.map(_.toVector)           ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode64mime.decode64mime.map(_.toVector)       ==== b.toVector --: typed[Vector[Byte] Or Err]
    }
    nFor(32){ i =>
      val b = r.arrayB(if i < 16 then i else 16 + 7*(i-16))
      T ~ b.stringEncode64lines.decode64.map(_.toVector) ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode64lines.decode64.map(_.toVector)       ==== b.toVector --: typed[Vector[Byte] Or Err]
    }

    def chs(s: String): String = if s.length > 31 then s.grouped(31).mkString("\n") else s
    def chb(ab: Array[Byte]): Array[Byte] = chs(ab.ascii).bytes
    nFor(32){ i =>
      val b = r.arrayB(if i < 16 then i else 16 + 7*(i-16))
      T ~ b.stringEncode85.decode85.map(_.toVector)      ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode85.decode85.map(_.toVector)            ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ chs(b.stringEncode85).decode85.map(_.toVector) ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ chb(b.encode85).decode85.map(_.toVector)       ==== b.toVector --: typed[Vector[Byte] Or Err]
    }
    nFor(32){ i =>
      val b = r.arrayB(if i < 16 then i else 16 + 7*(i-16))
      T ~ b.stringEncode85zmq.decode85zmq.map(_.toVector)      ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode85zmq.decode85zmq.map(_.toVector)            ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ chs(b.stringEncode85zmq).decode85zmq.map(_.toVector) ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ chb(b.encode85zmq).decode85zmq.map(_.toVector)       ==== b.toVector --: typed[Vector[Byte] Or Err]
    }
    nFor(32){ i =>
      val b = r.arrayB(if i < 16 then i else 16 + 7*(i-16))
      T ~ b.stringEncode85ascii.decode85ascii.map(_.toVector) ==== b.toVector --: typed[Vector[Byte] Or Err]
      T ~ b.encode85ascii.decode85ascii.map(_.toVector)       ==== b.toVector --: typed[Vector[Byte] Or Err]
    }

    val b = Array.tabulate(256)(i => i.toByte)
    r.shuffle(b)
    val target = new Array[Byte](360)
    val z8 = new Array[Byte](8)
    def clq(ab: Array[Byte]): Array[Byte] = ab.reverse.dropWhile(_ == '=').reverse
    T ~ EioBase64.encodeUrlRange(b, 15, 22) =**= clq(b.slice(15, 22).encode64url)
    T ~ EioBase64.decodeRangeInto(b.encode64url, 24, 48)(target, 16) ==== 18 --: typed[Int Or Err]
    T ~ { EioBase64.decodeRangeInto(b.encode64url, 64, 88)(target, 60); target.slice(52, 86) } =**= (z8 ++ b.encode64url.slice(64, 88).decode64.get ++ z8)
    T ~ { EioBase64.decodeInto(b.encode64url)(target, 8); target.slice(0, 272) } =**= (z8 ++ b ++ z8)
    T ~ EioBase64.decodeRange(b.encode64url, 12, 24).get =**= b.encode64url.slice(12, 24).decode64.get
    aFor(target){ (_, i) => target(i) = 0: Byte }
    T ~ EioBase64.decodeRangeInto(b.stringEncode64url, 24, 48)(target, 16) ==== 18 --: typed[Int Or Err]
    T ~ { EioBase64.decodeRangeInto(b.stringEncode64url, 64, 88)(target, 60); target.slice(52, 86) } =**= (z8 ++ b.encode64url.slice(64, 88).decode64.get ++ z8)
    T ~ { EioBase64.decodeInto(b.stringEncode64url)(target, 8); target.slice(0, 272) } =**= (z8 ++ b ++ z8)
    T ~ EioBase64.decodeRange(b.stringEncode64url, 12, 24).get =**= b.encode64url.slice(12, 24).decode64.get

    aFor(target){ (_, i) => target(i) = 0: Byte }
    T ~ EioBase85.encodeZmqRange(b, 83, 99) =**= b.slice(83, 99).encode85zmq
    T ~ EioBase85.encodeAsciiRange(b, 83, 99) =**= b.slice(83, 99).encode85ascii
    T ~ EioBase85.decodeZmqRange("Well, HelloWorld to you!", 6, 16).get =**= Array(0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B).map(_.toByte)
    T ~ EioBase85.decodeZmqRange("Well, HelloWorld to you!".bytes, 6, 16).get =**= Array(0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B).map(_.toByte)
    T ~ EioBase85.decodeAsciiRange(b.encode85ascii, 90, 105).get =**= b.encode85ascii.slice(90, 105).decode85ascii.get
    T ~ EioBase85.decodeAsciiRange(b.stringEncode85ascii, 90, 105).get =**= b.encode85ascii.slice(90, 105).decode85ascii.get

    val zok = new ZipEntry("all/fine/here.txt")
    val zwin = new ZipEntry("bad\\win\\name.txt")
    T ~ zok.cleanName  ==== "all/fine/here.txt"
    T ~ zwin.cleanName ==== "bad/win/name.txt"
    T ~ zok.cleanPath  ==== (new java.io.File("all/fine/here.txt")).toPath
    T ~ zwin.cleanPath ==== (new java.io.File("bad/win/name.txt")).toPath

  @Test
  def pathTest: Unit =
    val p = "temp/eio".path
    val q = "temp/eio/quartz.txt".path
    val r = "temp/flow/../eio/quartz.txt".path
    val s = "temp/eio/s/s".path
    val ft = Instant.now.round.ms.filetime
    Thread.sleep(10)
    T ~ p                            ==== FileSystems.getDefault.getPath("temp/eio")
    T ~ "temp/eio".file              ==== p.file
    T ~ "temp/eio".file.path         ==== p
    T ~ p.name                       ==== "eio"
    T ~ p.nameTo("fred")             ==== "temp/fred".path
    T ~ p.nameOp(_ + "s")            ==== "temp/eios".path
    T ~ p.ext                        ==== ""
    T ~ ".foo".path.ext              ==== ""
    T ~ q.ext                        ==== "txt"
    T ~ p.extTo("png")               ==== "temp/eio.png".path
    T ~ ".foo".path.extTo("png")     ==== ".foo.png".path
    T ~ q.extTo("png")               ==== "temp/eio/quartz.png".path
    T ~ p.extOp(_ + "xls")           ==== "temp/eio.xls".path
    T ~ ".foo".path.extOp(_ + "e")   ==== ".foo.e".path
    T ~ q.extOp(_ drop 4)            ==== "temp/eio/quartz".path
    T ~ p.base                       ==== "eio"
    T ~ ".foo".path.base             ==== ".foo"
    T ~ q.base                       ==== "quartz"
    T ~ p.baseTo("flow")             ==== "temp/flow".path
    T ~ ".foo".path.baseTo(".bar")   ==== ".bar".path
    T ~ q.baseTo("pearl")            ==== "temp/eio/pearl.txt".path
    T ~ p.baseOp(_ drop 1)           ==== "temp/io".path
    T ~ ".foo".path.baseOp(_ + "t")  ==== ".foot".path
    T ~ q.baseOp(_ dropRight 1)      ==== "temp/eio/quart.txt".path
    T ~ p.parentName                 ==== "temp"
    T ~ ".foo".path.parentName       ==== ""
    T ~ q.parentName                 ==== "eio"
    T ~ p.namesIterator.toVector     ==== Vector("eio", "temp")
    T ~ q.namesIterator.toVector     ==== Vector("quartz.txt", "eio", "temp")
    T ~ p.pathsIterator.toVector     ==== Vector(p, "temp".path)
    T ~ q.pathsIterator.toVector     ==== Vector(q, p, "temp".path)
    T ~ p.parent                     ==== "temp".path --: typed[Path Or Unit]
    T ~ ".foo".path.parent           ==== Alt.unit
    T ~ q.parent                     ==== p
    T ~ r.real                       ==== q.absolute
    T ~ p.absolute.isAbsolute        ==== true
    T ~ p.isAbsolute                 ==== false
    T ~ (p / "quartz.txt")           ==== q
    T ~ (p / q)                      ==== "temp/eio/temp/eio/quartz.txt".path
    T ~ p.`..`                       ==== "temp".path
    T ~ p.`..`.`..`                  ==== "temp".path
    T ~ p.sib("flow")                ==== "temp/flow".path
    T ~ q.sib(p)                     ==== (p / p)
    T ~ q.reroot(p, ".foo".path)     ==== ".foo/quartz.txt".path
    T ~ q.reroot("temp".path, p)     ==== "temp/eio/eio/quartz.txt".path
    T ~ p.adopt(q)                   ==== "quartz.txt".path --: typed[Path Or Unit]
    T ~ q.adopt(p)                   ==== Alt.unit
    T ~ p.adopt(p)                   ==== "".path
    T ~ p.adopt("temp/eios".path)    ==== Alt.unit
    T ~ p.exists                     ==== true
    T ~ q.exists                     ==== false
    T ~ { q.touch(); q.exists }      ==== true
    T ~ q.size                       ==== 0L
    T ~ p.isDirectory                ==== true
    T ~ q.isDirectory                ==== false
    T ~ (p / "dir").exists           ==== false
    T ~ (p / "dir").mkdir()          ==== (p / "dir")
    T ~ (p / "a").exists             ==== false
    T ~ (p / "a/b").exists           ==== false
    T ~ (p / "a/b").mkdirs()         ==== (p.absolute / "a/b")
    T ~ (p / "a").exists             ==== true
    T ~ (p / "a" / "b").exists       ==== true
    T ~ (q.time - ft).in(0.s, 10.s)  ==== true
    T ~ { q.time = ft; q.time }      ==== ft
    T ~ { q.touch(); q.time == ft }  ==== false
    T ~ p.paths.sorted               =**= Array(p / "a", p / "dir", p / "quartz.txt")
    T ~ q.write("blorp".bytes)       ==== ()
    T ~ q.gulp                       =**= "blorp".bytes
    T ~ q.append("y".bytes)          ==== ()
    T ~ q.slurp                      =**= Array("blorpy")
    T ~ q.create("bad".bytes)        ==== false
    T ~ q.delete()                   ==== true
    T ~ q.delete()                   ==== false
    T ~ q.exists                     ==== false
    T ~ q.create("hi\nworld".bytes)  ==== true
    T ~ q.slurp                      =**= Array("hi", "world")
    T ~ q.delete()                   ==== true
    T ~ q.exists                     ==== false
    T ~ q.append("foo".bytes)        ==== ()
    T ~ q.gulp.utf8                  ==== "foo"
    T ~ q.writeLines("eel" :: Nil)   ==== ()
    T ~ q.slurp                      =**= Array("eel")
    T ~ q.appendLines("cod" :: Nil)  ==== ()
    T ~ q.slurp                      =**= Array("eel", "cod")
    T ~ q.append("perch".bytes)      ==== ()
    T ~ q.slurp                      =**= Array("eel", "cod", "perch")
    T ~ q.append("bass".bytes)       ==== ()
    T ~ q.slurp                      =**= Array("eel", "cod", "perchbass")
    T ~ q.delete()                   ==== true
    T ~ q.createLines("cod" :: Nil)  ==== true
    T ~ q.gulp.utf8                  ==== "cod\n"
    T ~ q.createLines("eel" :: Nil)  ==== false
    T ~ q.copyTo(p / "fish.txt")     ==== ()
    T ~ (p / "fish.txt").slurp       =**= Array("cod")
    T ~ q.moveTo(p / "move.txt")     ==== ()
    T ~ (p / "move.txt").slurp       =**= Array("cod")
    T ~ q.exists                     ==== false

    val ps = "temp/eio/sym".path
    T ~ (p / "sym").isSymlink               ==== false
    T ~ ps.symlink                          ==== Alt.unit --: typed[String Or Unit]
    T ~ { ps.makeSymlink("a"); ps.symlink } ==== "a"
    T ~ ps.followSymlink                    ==== (p / "a") --: typed[Path Or Unit]
    T ~ ps.exists                           ==== true
    T ~ (ps / "b").exists                   ==== true
    T ~ (p / "a").isSymlink                 ==== false
    T ~ ps.isSymlink                        ==== true
    T ~ ps.real                             ==== (p.real / "a")
    T ~ (ps / "b").real                     ==== (p.real / "a/b")
    (p / "a" / "b").delete()
    (p / "a").delete()
    T ~ ps.exists                           ==== false
    T ~ ps.isSymlink                        ==== true
    T ~ ps.real                             ==== (p.real / "a")
    T ~ (ps / "b").real                     ==== (p.real / "a/b")
    val a2b = "temp/eio/ab".path
    val b2a = "temp/eio/ba".path
    a2b.symlinkTo(b2a)
    b2a.symlinkTo(a2b)
    T ~ (a2b.exists || b2a.exists)          ==== false
    T ~ (a2b.isSymlink && b2a.isSymlink)    ==== true
    T ~ a2b.followSymlink                   ==== b2a
    T ~ b2a.followSymlink                   ==== a2b
    T ~ a2b.real                            ==== (p.real / "ab")
    T ~ b2a.real                            ==== (p.real / "ba")
    T ~ (a2b / "x").real                    ==== (p.real / "ab/x")
}
object EioTest {
  import kse.flow.{given, _}

  val testPath = FileSystems.getDefault.getPath("temp/eio")

  def cleanTempEio(): Unit Or Err = nice:
    var targets: List[(Path, Boolean)] = List((testPath, false), (testPath.getParent, false))
    while targets.nonEmpty do
      val (t, del) = targets.head
      targets = targets.tail
      if del then Files delete t
      else if (Files isDirectory t) && !(Files isSymbolicLink t) then
        Resource(Files list t)(_.close): fs =>
          targets = ((t, true)) :: targets
          iFor(fs.iterator){ (q, _) => targets = ((q, false)) :: targets }
      else Files deleteIfExists t

  def createTempEio(): Unit Or Err = nice:
    Files.createDirectories(testPath)

  @BeforeClass
  def before(): Unit =
    cleanTempEio().foreachAlt(e => e.explainBy(s"Error pre-cleaning test arena at $testPath").tap(println))
    createTempEio().foreachAlt(e => e.explainBy(s"Error creating test arena at $testPath").tap(println))

  @AfterClass
  def after(): Unit =
    () // cleanTempEio().foreachAlt(e => e.explainBy(s"Error cleaning test arena at $testPath"))
}
