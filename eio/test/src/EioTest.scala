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
    T ~ p                        ==== FileSystems.getDefault.getPath("temp/eio")
    T ~ "temp/eio".file          ==== p.file
    T ~ "temp/eio".file.path     ==== p
    T ~ p.name                   ==== "eio"
    T ~ p.nameTo("fred")         ==== "temp/fred".path
    T ~ p.nameOp(_ + "s")        ==== "temp/eios".path
    T ~ p.ext                    ==== ""
    T ~ ".foo".path.ext          ==== ""
    T ~ q.ext                    ==== "txt"
    T ~ p.extTo("png")           ==== "temp/eio.png".path
    T ~ ".foo".path.extTo("png") ==== ".foo.png".path
    T ~ q.extTo("png")           ==== "temp/eio/quartz.png".path
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
      else if Files exists t then
        if (Files isDirectory t) || !(Files isSymbolicLink t) then
          Resource(Files list t)(_.close): fs =>
            targets = ((t, true)) :: targets
            iFor(fs.iterator){ (q, _) => targets = ((q, false)) :: targets }
        else Files delete t

  def createTempEio(): Unit Or Err = nice:
    Files.createDirectories(testPath)

  @BeforeClass
  def before(): Unit =
    cleanTempEio().foreachAlt(e => e.explainBy(s"Error pre-cleaning test arena at $testPath").tap(println))
    createTempEio().foreachAlt(e => e.explainBy(s"Error creating test arena at $testPath").tap(println))

  @AfterClass
  def after(): Unit =
    cleanTempEio().foreachAlt(e => e.explainBy(s"Error cleaning test arena at $testPath"))
}
