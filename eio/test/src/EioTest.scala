// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences, LLC.

package kse.eio.test


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import java.io._
import java.nio._
import java.nio.file._
import java.nio.file.attribute.FileTime
import java.nio.channels._
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

    val bf = "fish".bytes.buffer
    T ~ bf.remaining     ==== 4
    T ~ bf.get           ==== 'f'.toByte --: typed[Byte]
    T ~ bf.get           ==== 'i'
    T ~ bf.getBytes.utf8 ==== "sh"
    T ~ bf.hasRemaining  ==== false
    val bis = "fish".bytes.input
    T ~ bis.available    ==== 4
    T ~ { val a = new Array[Byte](4); bis.read(a); a.utf8 } ==== "fish"
    T ~ bis.available    ==== 0

    val ba = new Array[Byte](8)
    val bb = ByteBuffer.wrap(ba)
    val ab2 = new Array[Byte](2)
    val bbos = bb.output
    T ~ bbos.buffer                    ==== bb
    T ~ bbos.write('m'.toByte)         ==== ()
    T ~ bb.position                    ==== 1
    T ~ bbos.write("inn".bytes)        ==== ()
    T ~ bb.position                    ==== 4
    T ~ bbos.write("howl".bytes, 1, 2) ==== ()
    T ~ bb.position                    ==== 6
    T ~ bbos.write("ing".bytes)        ==== thrown[BufferOverflowException]
    T ~ bbos.flush                     ==== ()
    T ~ bbos.close                     ==== ()
    T ~ bbos.write('s'.toByte)         ==== thrown[IOException]
    T ~ ba.take(6).utf8                ==== "minnow"
    val bbis = bb.input
    T ~ bbis.available                 ==== 6
    T ~ bbis.skip(1)                   ==== 1   --: typed[Long]
    T ~ bbis.read()                    ==== 'i' --: typed[Int]
    T ~ bbis.skip(1)                   ==== 1
    T ~ bbis.markSupported             ==== true
    T ~ bbis.mark(0)                   ==== ()
    T ~ bbis.read(ab2)                 ==== 2
    T ~ ab2.utf8                       ==== "no"
    T ~ bbis.skip(3)                   ==== 1
    T ~ bbis.reset                     ==== ()
    T ~ bbis.read(ab2, 1, 1)           ==== 1
    T ~ ab2.utf8                       ==== "nn"
    T ~ bbis.skip(1)                   ==== 1
    T ~ bbis.read(ab2)                 ==== 1
    T ~ ab2.utf8                       ==== "wn"
    T ~ bb.remaining                   ==== 0
    T ~ bb.position                    ==== 6
    T ~ bbis.read()                    ==== thrown[BufferUnderflowException]
    T ~ bbis.close()                   ==== ()
    T ~ bbis.reset                     ==== thrown[IOException]

    val wsbc = ba.writeChannel
    T ~ wsbc.bufferAvailableToWrite         ==== 8
    T ~ wsbc.maxAvailableToWrite            ==== 8
    T ~ wsbc.position                       ==== 0
    T ~ wsbc.position(1)                    ==== wsbc
    T ~ wsbc.size                           ==== 0
    T ~ wsbc.write("sup".bytes.input)       ==== 3
    T ~ wsbc.size                           ==== 4
    T ~ wsbc.write("person".bytes.input)    ==== 4
    T ~ ba.drop(1).utf8                     ==== "suppers"
    T ~ wsbc.position(3)                    ==== wsbc
    T ~ wsbc.write("mm".bytes.readChannel)  ==== 2
    T ~ wsbc.position(7)                    ==== wsbc
    T ~ wsbc.write("yes".bytes.readChannel) ==== 1
    T ~ ba.drop(1).utf8                     ==== "summery"
    T ~ wsbc.position(1)                    ==== wsbc
    T ~ wsbc.write("hi".bytes)              ==== 2
    T ~ wsbc.write("everyone".bytes.buffer) ==== 5
    T ~ wsbc.write("here".bytes.buffer)     ==== -1
    T ~ wsbc.position(0)                    ==== wsbc
    T ~ { wsbc.read(ab2.buffer); ab2 }      =**= Array[Byte](0, 'h'.toByte)
    T ~ wsbc.position(0)                    ==== wsbc
    T ~ wsbc.write("e".bytes.input)         ==== 1
    T ~ wsbc.position(0)                    ==== wsbc
    T ~ { wsbc.read(ab2.buffer); ab2 }      =**= "eh".bytes
    T ~ wsbc.position(0)                    ==== wsbc
    T ~ wsbc.write("YETI".bytes, 2)(1)      ==== 1
    T ~ wsbc.close                          ==== ()
    T ~ wsbc.position(3)                    ==== thrown[ClosedChannelException]
    ba(0) = 'T'
    bb.clear
    val rsbc = ba.readChannel
    val bc = ByteBuffer.allocate(12)
    val ab3 = new Array[Byte](3)
    val ab9 = new Array[Byte](9)
    T ~ rsbc.bufferAvailableToWrite              ==== 8
    T ~ rsbc.maxAvailableToWrite                 ==== 8
    T ~ rsbc.availableToRead                     ==== 8
    T ~ rsbc.size                                ==== 8
    T ~ rsbc.position                            ==== 0
    T ~ rsbc.read(bc)                            ==== 8
    T ~ rsbc.read(bc)                            ==== -1
    T ~ rsbc.read(ab3)                           ==== -1
    T ~ rsbc.write(bc.tap(_.flip))               ==== -1
    T ~ bc.getBytes.utf8                         ==== "Thievery"
    T ~ rsbc.position(3)                         ==== rsbc
    T ~ { bc.clear; bc.limit(3); rsbc.read(bc) } ==== 3
    T ~ rsbc.availableToRead                     ==== 2
    T ~ rsbc.read(ab3)                           ==== 2
    T ~ bc.tap(_.flip).getBytes.utf8             ==== "eve"
    T ~ ab3                                      =**= Array('r'.toByte, 'y'.toByte, 0: Byte)
    T ~ rsbc.position(1)                         ==== rsbc
    T ~ rsbc.read(ab2.writeChannel)              ==== 2
    T ~ ab2.utf8                                 ==== "hi"
    T ~ rsbc.read(ab9.writeChannel)              ==== 5
    T ~ ab9.utf8                                 ==== "every\u0000\u0000\u0000\u0000"
    T ~ rsbc.position(3)                         ==== rsbc
    T ~ rsbc.read(ab3)                           ==== 3
    T ~ rsbc.read(ab3, 1)(1)                     ==== 1
    T ~ ab3.utf8                                 ==== "ere"
    T ~ rsbc.position(1)                         ==== rsbc
    T ~ rsbc.read(ab3.output)(3)                 ==== 3
    T ~ ab3.utf8                                 ==== "hie"
    T ~ rsbc.position(4)                         ==== rsbc
    T ~ rsbc.read(ab3.output)(1)                 ==== 1
    T ~ ab3.utf8                                 ==== "vie"
    T ~ rsbc.position(6)                         ==== rsbc
    T ~ rsbc.read(ab3.output)                    ==== 2
    T ~ ab3.utf8                                 ==== "rye"
    T ~ rsbc.close                               ==== ()
    T ~ rsbc.position(1)                         ==== thrown[ClosedChannelException]
    val rng = Pcg64(342552345346L)
    val big = rng.arrayB(32768)
    val sbc = Array.empty[Byte].growCopyBy(512)
    T ~ sbc ==== typed[MultiArrayChannel]
    var nwrote = 0
    var counter = 0
    while nwrote < big.length do
      counter += 1
      def pickn() = 1 + rng % (if (counter % 10) == 0 then 2048 else 128)
      val more = (big.length - nwrote) min pickn()
      T ~ sbc.size ==== nwrote
      T ~ sbc.write(big.copyOfRange(nwrote, nwrote + more).buffer) ==== more
      nwrote += more
      val p = sbc.position
      val ptarg = r % p.toInt
      sbc.position(ptarg)
      val bx = ByteBuffer.allocate(pickn())
      val readable = sbc.availableToRead.toInt
      T ~ sbc.read(bx)            ==== (bx.capacity min readable)
      T ~ bx.tap(_.flip).getBytes =**= big.copyOfRange(ptarg, ptarg + (bx.capacity min readable))
      sbc.position(p)
    val bx = ByteBuffer.allocate(big.length)
    val abx = new Array[Byte](big.length)
    T ~ sbc.position(0).read(bx)             ==== big.length
    T ~ bx.tap(_.flip).getBytes              =**= big
    T ~ sbc.position(0).read(abx)            ==== big.length
    T ~ abx                                  =**= big
    val weird = new Array[Byte](1917)
    val sbc2 = Array.empty[Byte].growCopyBy(337)
    val sbc3 = Array.empty[Byte].growCopyBy(294)
    T ~ sbc.position(0)                      ==== sbc
    T ~ sbc.read(weird.writeChannel)         ==== weird.length
    T ~ sbc.read(sbc2)                       ==== (big.length - weird.length)
    T ~ weird                                =**= big.take(weird.length)
    T ~ { sbc2.close; sbc2.getBytes.get }    =**= big.drop(weird.length)
    T ~ sbc.position(0)                      ==== sbc
    T ~ sbc.read(sbc3)                       ==== big.length
    T ~ sbc3.position(0)                     ==== sbc3
    T ~ sbc3.availableToRead                 ==== big.length
    T ~ sbc3.position(294*20 + 170)          ==== sbc3
    T ~ sbc3.availableToRead                 ==== big.length - 294*20 - 170
    T ~ sbc3.compact().position              ==== 170
    T ~ sbc3.availableToRead                 ==== big.length - 294*20 - 170
    T ~ { sbc3.close; sbc3.getBytes.get }    =**= big.drop(294*20)
    T ~ sbc.detatchBuffers()                 ==== thrown[IllegalArgumentException]
    T ~ sbc.getBytes                         ==== thrown[IllegalArgumentException]
    sbc.close
    T ~ sbc.getBytes.get                     =**= big
    T ~ sbc.detatchBuffers().map(_.toVector) =**= big.grouped(512).toArray.map(_.toVector)
    T ~ sbc.detatchBuffers()                 ==== thrown[IllegalArgumentException]
    T ~ sbc.getBytes                         ==== thrown[IllegalArgumentException]

    val f = new File("temp/eio/raf.txt")
    def nraf = new RandomAccessFile(f, "rw")
    val mos = Mu[OutputStream](null)
    val mis = Mu[InputStream](null)
    T ~ { mos.set(nraf.output); mos.value } ==== runtype[OutputStream]
    T ~ mos.value.write('e')                ==== ()
    T ~ mos.value.write("el".bytes)         ==== ()
    T ~ mos.value.write("bass".bytes, 2, 1) ==== ()
    T ~ mos.value.close                     ==== ()
    T ~ mos.value.write('x')                ==== thrown[IOException]
    T ~ f.exists                            ==== true
    T ~ Files.readAllBytes(f.toPath).utf8   ==== "eels"
    T ~ { mis.set(nraf.input); mis.value }  ==== runtype[InputStream]
    T ~ mis.value.skip(1)                   ==== 1   --: typed[Long]
    T ~ mis.value.read()                    ==== 'e' --: typed[Int]
    T ~ mis.value.markSupported             ==== true
    T ~ mis.value.mark(1)                   ==== ()
    T ~ mis.value.available()               ==== 2
    T ~ mis.value.read(ab2)                 ==== 2
    T ~ ab2.utf8                            ==== "ls"
    T ~ mis.value.skip(2)                   ==== 0
    T ~ mis.value.reset                     ==== ()
    T ~ mis.value.read(ab2, 1, 1)           ==== 1
    T ~ ab2.utf8                            ==== "ll"
    T ~ mis.value.read(ab2)                 ==== 1
    T ~ ab2.utf8                            ==== "sl"
    T ~ mis.value.close()                   ==== ()
    T ~ mis.value.read()                    ==== thrown[IOException]

    def nsbc = {
      val sbc = Files.newByteChannel(f.toPath, StandardOpenOption.WRITE, StandardOpenOption.READ)
      sbc.position(1)
    }
    T ~ { mos.set(nsbc.output); mos.value }    ==== runtype[OutputStream]
    T ~ mos.value.write("beagles".bytes, 2, 4) ==== ()
    T ~ mos.value.close                        ==== ()
    T ~ Files.readAllBytes(f.toPath).utf8      ==== "eagle"
    T ~ { mis.set(nsbc.input); mis.value }     ==== runtype[InputStream]
    T ~ mis.value.skip(1)                      ==== 1   --: typed[Long]
    T ~ mis.value.read()                       ==== 'g' --: typed[Int]
    T ~ mis.value.markSupported                ==== true
    T ~ mis.value.mark(1)                      ==== ()
    T ~ mis.value.available()                  ==== 2
    T ~ mis.value.read(ab2)                    ==== 2
    T ~ ab2.utf8                               ==== "le"
    T ~ mis.value.skip(2)                      ==== 0
    T ~ mis.value.reset                        ==== ()
    T ~ mis.value.read(ab2, 1, 1)              ==== 1
    T ~ ab2.utf8                               ==== "ll"
    T ~ mis.value.read(ab2)                    ==== 1
    T ~ ab2.utf8                               ==== "el"
    T ~ mis.value.close()                      ==== ()
    T ~ mis.value.read()                       ==== thrown[IOException]
    T ~ f.delete()                             ==== true


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
    T ~ "No.".path.ext               ==== ""
    T ~ q.ext                        ==== "txt"
    T ~ p.extTo("png")               ==== "temp/eio.png".path
    T ~ ".foo".path.extTo("png")     ==== ".foo.png".path
    T ~ "No.".path.extTo("png")      ==== "No..png".path
    T ~ q.extTo("png")               ==== "temp/eio/quartz.png".path
    T ~ p.extOp(_ + "xls")           ==== "temp/eio.xls".path
    T ~ ".foo".path.extOp(_ + "e")   ==== ".foo.e".path
    T ~ "No.".path.extOp(_ + "!")    ==== "No..!".path
    T ~ q.extOp(_ drop 4)            ==== "temp/eio/quartz".path
    T ~ p.base                       ==== "eio"
    T ~ ".foo".path.base             ==== ".foo"
    T ~ "No.".path.base              ==== "No."
    T ~ q.base                       ==== "quartz"
    T ~ p.baseTo("flow")             ==== "temp/flow".path
    T ~ ".foo".path.baseTo(".bar")   ==== ".bar".path
    T ~ "No.".path.baseTo("Yes")     ==== "Yes".path
    T ~ q.baseTo("pearl")            ==== "temp/eio/pearl.txt".path
    T ~ p.baseOp(_ drop 1)           ==== "temp/io".path
    T ~ ".foo".path.baseOp(_ + "t")  ==== ".foot".path
    T ~ "No.".path.baseOp(_ + "!")   ==== "No.!".path
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
    T ~ (p / "dir").mkdir()          ==== ()
    T ~ (p / "a").exists             ==== false
    T ~ (p / "a/b").exists           ==== false
    T ~ (p / "a/b").mkdirs()         ==== ()
    T ~ (p / "a").exists             ==== true
    T ~ (p / "a" / "b").exists       ==== true
    T ~ (p / "eel").exists           ==== false
    T ~ (p / "eel/x").mkParents()    ==== (p / "eel/x")
    T ~ (p / "eel").exists           ==== true
    T ~ (q.time - ft).in(0.s, 10.s)  ==== true
    T ~ { q.time = ft; q.time }      ==== ft
    T ~ { q.touch(); q.time == ft }  ==== false
    T ~ p.paths.sorted               =**= Array(p / "a", p / "dir", p / "eel", p / "quartz.txt")
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

    val ab2 = "ft".bytes
    Resource(q.openCreate())(_.close)(x => T ~ x ==== runtype[BufferedOutputStream])
    T ~ q.exists                                                         ==== true
    T ~ q.size                                                           ==== 0L
    Resource(q.openAppend())(_.close){ x => x.write('e'); T ~ x ==== runtype[BufferedOutputStream] }
    T ~ q.size                                                           ==== 1L
    Resource(q.openRead())(_.close)(x => T ~ x ==== runtype[BufferedInputStream])
    T ~ Resource(q.openRead())(_.close)(_.available)                     ==== 1
    T ~ Resource(q.openRead())(_.close)(_.read)                          ==== 'e'
    Resource(q.openWrite())(_.close){ o => o.write("eel".bytes); T ~ o ==== runtype[BufferedOutputStream] }
    T ~ Resource(q.openIO())(_.close)(_.position(1).write(ab2.buffer))   ==== 2
    T ~ Resource(q.openIO())(_.close){o => o.read(ab2.buffer); ab2.utf8} ==== "ef"

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
    T ~ (ps / "b").mkParents()              ==== (ps / "b")
    T ~ ps.exists                           ==== true
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

    val mfs = com.github.marschall.memoryfilesystem.MemoryFileSystemBuilder.newEmpty().build()
    T ~ "/life/fish".pathIn(mfs).exists          ==== false
    T ~ "/life/fish".pathIn(mfs).mkdirs()        ==== ()
    T ~ "/life/fish".pathIn(mfs).file            ==== thrown[Exception]
    val mp = "/life/fish".pathIn(mfs)
    T ~ (mp / "eel.txt").write("hi".bytes)       ==== ()
    T ~ "/life/fish/eel.txt".pathLike(mp).exists ==== true


  @Test
  def readwriteTest(): Unit =
    val rng = Pcg64(9569856892L)
    val b9999 = rng.arrayB(9999)
    val z9999 = new Array[Byte](9999)
    val z1024 = new Array[Byte](1024)
    def rsbc: SeekableByteChannel = b9999.readChannel
    def rmac: MultiArrayChannel = b9999.readChannel
    def zwc: WritableByteChannel = z9999.writeChannel
    def zws: WritableByteChannel = z1024.writeChannel

    val sms2s = Transfer.StreamToStream(391)
    T ~ b9999.input.transferTo(z9999.output)              ==== 9999L  --: typed[Long Or Err]
    T ~ z9999                                             =**= b9999
    z9999.fill(0)
    T ~ b9999.input.transferTo(z9999.output)(using sms2s) ==== 9999L
    T ~ z9999                                             =**= b9999
    T ~ b9999.input.transferTo(z1024.output).isAlt        ==== true
    T ~ b9999.input.transferTo(z1024.output).alt.toss     ==== thrown[IOException]

    val sms2c = Transfer.StreamToChannel(391, allowFullTarget = true)
    z9999.fill(0)
    T ~ b9999.input.transferTo(zwc)                        ==== 9999L
    T ~ z9999                                              =**= b9999
    z9999.fill(0)
    T ~ b9999.input.transferTo(zwc)(using sms2c)           ==== 9999L
    T ~ z9999                                              =**= b9999
    T ~ b9999.input.transferTo(zws).isAlt                  ==== true
    T ~ b9999.input.transferTo(zws).alt.toss               ==== thrown[ErrType.StringErrException]
    T ~ b9999.input.transferTo(zws)(using sms2c)           ==== 1024
    T ~ z1024                                              =**= b9999.take(1024)

    val smc2s = Transfer.ChannelToStream(391)
    z9999.fill(0)
    T ~ rsbc.transferTo(z9999.output)                      ==== 9999L
    T ~ z9999                                              =**= b9999
    z9999.fill(0)
    T ~ rsbc.transferTo(z9999.output)(using smc2s)         ==== 9999L
    T ~ z9999                                              =**= b9999
    T ~ rsbc.transferTo(z1024.output).isAlt                ==== true
    T ~ rsbc.transferTo(z1024.output).alt.toss             ==== thrown[IOException]

    val smc2c = Transfer.ChannelToChannel(391, allowFullTarget = true)
    z9999.fill(0)
    T ~ rsbc.transferTo(zwc)                               ==== 9999L
    T ~ z9999                                              =**= b9999
    z9999.fill(0)
    T ~ rsbc.transferTo(zwc)(using smc2c)                  ==== 9999L
    T ~ z9999                                              =**= b9999
    T ~ rsbc.transferTo(zws).isAlt                         ==== true
    T ~ rsbc.transferTo(zws).alt.toss                      ==== thrown[ErrType.StringErrException]
    z1024.fill(0)
    T ~ rsbc.transferTo(zws)(using smc2c)                  ==== 1024
    T ~ z1024                                              =**= b9999.take(1024)

    z9999.fill(0)
    T ~ rmac.transferTo(z9999.output)                      ==== 9999L
    T ~ z9999                                              =**= b9999
    T ~ rmac.transferTo(z1024.output).isAlt                ==== true
    T ~ rmac.transferTo(z1024.output).alt.toss             ==== thrown[IOException]

    val smm2c = Transfer.MultiToChannel(allowFullTarget = true)
    z9999.fill(0)
    T ~ rmac.transferTo(zwc)                               ==== 9999L
    T ~ z9999                                              =**= b9999
    T ~ rmac.transferTo(zws).isAlt                         ==== true
    T ~ rmac.transferTo(zws).alt.toss                      ==== thrown[ErrType.StringErrException]
    z1024.fill(0)
    T ~ rmac.transferTo(zws)(using smm2c)                  ==== 1024L
    T ~ z1024                                              =**= b9999.take(1024)

    val bits = (Array.fill(27)(rng % 9999) ++ Array(0, 9999)).sorted.sliding(2).map(xs => b9999.copyOfRange(xs(0), xs(1))).toList
    def biter = bits.iterator
    z9999.fill(0)
    T ~ biter.transferTo(z9999.output)                     ==== 9999L
    T ~ z9999                                              =**= b9999

    val smb2c = Transfer.IterBytesToChannel(allowFullTarget = true)
    z9999.fill(0)
    T ~ biter.transferTo(zwc)                              ==== 9999L
    T ~ z9999                                              =**= b9999
    T ~ biter.transferTo(zws).isAlt                        ==== true
    T ~ biter.transferTo(zws).alt.toss                     ==== thrown[ErrType.StringErrException]
    z1024.fill(0)
    T ~ biter.transferTo(zws)(using smb2c)                 ==== 1024L
    T ~ z1024                                              =**= b9999.take(1024)

    val strings = List("salmon", "herring", "cod", "perch")
    def siter = strings.iterator
    z9999.fill(0)
    T ~ siter.transferTo(z9999.output)                     ==== strings.map(_.length + 1).sum
    T ~ z9999.take(strings.map(_.length + 1).sum)          =**= "salmon\nherring\ncod\nperch\n".bytes

    val smi2c = Transfer.IterStringToChannel(allowFullTarget = true)
    val z20 = new Array[Byte](20)
    def w20: WritableByteChannel = z20.writeChannel
    z9999.fill(0)
    T ~ siter.transferTo(zwc)                              ==== strings.map(_.length + 1).sum
    T ~ z9999.take(strings.map(_.length + 1).sum).utf8     ==== "salmon\nherring\ncod\nperch\n"
    T ~ siter.transferTo(w20).isAlt                        ==== true
    T ~ siter.transferTo(w20).alt.toss                     ==== thrown[ErrType.StringErrException]
    z20.fill(0)
    T ~ siter.transferTo(w20)(using smi2c)                 ==== 20L
    T ~ z20.utf8                                           ==== "salmon\nherring\ncod\np"

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
    cleanTempEio().foreachAlt(e => e.explainBy(s"Error cleaning test arena at $testPath"))
}
