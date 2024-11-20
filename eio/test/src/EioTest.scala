// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023-24 Rex Kerr and Calico Life Sciences, LLC.

package kse.test.eio


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

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
  import kse.basics.{given, _}
  import kse.basics.intervals._
  import kse.flow.{given, _}
  import kse.maths.{given, _}
  import kse.maths.packed.{given, _}
  import kse.eio.{given, _}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  @Test
  def displayTest(): Unit =
    ()
    /*
    import Display.{PadLeft, PadRight, Pad, ShowSign, OneLine, StrictSize, SixSig, ClipSig, SigFigs, Flags}
    T ~ (PadLeft & PadRight)                    ==== Pad
    T ~ (Pad has PadLeft)                       ==== true
    T ~ Pad.has(PadLeft & ShowSign)             ==== false
    T ~ Pad.hasAny(PadLeft & ShowSign)          ==== true
    T ~ (PadLeft & ShowSign).mask(Pad)          ==== PadLeft
    T ~ (Pad & StrictSize & OneLine & ShowSign) ==== Flags(0x1F)
    T ~ (SixSig & ClipSig & SigFigs)            ==== Flags(0x60)

    def sb(s: String = ""): java.lang.StringBuilder =
      val sb = new java.lang.StringBuilder
      if s.nonEmpty then sb append s
      sb
    import Display.addPadding
    T ~ sb("hi").tap(x => addPadding(3)(x    )).toString ==== "hi   "
    T ~ sb("hi").tap(x => addPadding(3)(x,  2)).toString ==== "hi   "
    T ~ sb("hi").tap(x => addPadding(3)(x,  1)).toString ==== "h   i"
    T ~ sb("hi").tap(x => addPadding(3)(x,  0)).toString ==== "   hi"
    T ~ sb("hi").tap(x => addPadding(3)(x, -2)).toString ==== " hi"
    T ~ sb("hi").tap(x => addPadding(3)(x, -5)).toString ==== "hi"
    T ~ sb("hi").tap(x => addPadding(10007)(x)).toString ==== ("hi" + " "*10007)
    T ~ sb("hi").tap(x => addPadding(999)(x,0)).toString ==== (" "*999 + "hi")

    import Display.{booleanFmt => bf}
    T ~ sb("hi").tupWith(x => bf(x, 7, 6                  )(true))._1op(_.toString)  ==== ("hitrue", 4)
    T ~ sb("hi").tupWith(x => bf(x, 7, 6, flags = PadLeft )(true))._1op(_.toString)  ==== ("hi  true", 6)
    T ~ sb("hi").tupWith(x => bf(x, 7, 6, flags = PadRight)(true))._1op(_.toString)  ==== ("hitrue   ", 4)
    T ~ sb("hi").tupWith(x => bf(x, 7, 6, flags = Pad     )(true))._1op(_.toString)  ==== ("hi  true ", 6)
    T ~ sb("hi").tupWith(x => bf(x, 7, 6                  )(false))._1op(_.toString) ==== ("hifalse", 5)
    T ~ sb("hi").tupWith(x => bf(x, 7, 6, flags = PadLeft )(false))._1op(_.toString) ==== ("hi false", 6)
    T ~ sb("hi").tupWith(x => bf(x, 7, 6, flags = PadRight)(false))._1op(_.toString) ==== ("hifalse  ", 5)
    T ~ sb("hi").tupWith(x => bf(x, 7, 6, flags = Pad     )(false))._1op(_.toString) ==== ("hi false ", 6)
    T ~ sb("hi").tupWith(x => bf(x, 3, 2                  )(true))._1op(_.toString)  ==== ("hiT", 1)
    T ~ sb("hi").tupWith(x => bf(x, 3, 2, flags = Pad     )(true))._1op(_.toString)  ==== ("hi T ", 2)
    T ~ sb("hi").tupWith(x => bf(x, 3, 2                  )(false))._1op(_.toString) ==== ("hiF", 1)
    T ~ sb("hi").tupWith(x => bf(x, 3, 2, flags = Pad     )(false))._1op(_.toString) ==== ("hi F ", 2)

    import Display.{numberFmt => nf}
    T ~ sb("hi").tupWith(x => nf(x, 5, 3)(4))._1op(_.toString)                                     ==== ("hi4", 1)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3)(-4))._1op(_.toString)                                    ==== ("hi-4", 2)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, flags = ShowSign             )(     4))._1op(_.toString) ==== ("hi+4", 2)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, flags = PadLeft              )(     4))._1op(_.toString) ==== ("hi  4", 3)
    T ~ sb("hi").tupWith(x => nf(x, 5, 9, flags = PadLeft              )(     4))._1op(_.toString) ==== ("hi    4", 5)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, flags = PadRight             )(     4))._1op(_.toString) ==== ("hi4    ", 1)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, flags = Pad                  )(     4))._1op(_.toString) ==== ("hi  4  ", 3)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, flags = StrictSize           )(     4))._1op(_.toString) ==== ("hi4", 1)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, flags = StrictSize           )(123456))._1op(_.toString) ==== ("hi#####", 5)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, flags = StrictSize           )(-12345))._1op(_.toString) ==== ("hi-####", 5)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, flags = StrictSize & ShowSign)( 12345))._1op(_.toString) ==== ("hi+####", 5)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, flags = StrictSize           )( 12345))._1op(_.toString) ==== ("hi12345", 5)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, unsigned = true)( 0x9000000000000000L))._1op(_.toString) ==== ("hi10376293541461622784", 20)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, true, StrictSize)(                -1L))._1op(_.toString) ==== ("hi#####", 5)
    T ~ sb("hi").tupWith(x => nf(x, 5, 3, true, StrictSize & ShowSign)(     -1L))._1op(_.toString) ==== ("hi#####", 5)

    import Display.Opts
    T ~ true.display                             ==== "true"
    T ~ false.displayFmt(Opts.strict(2))         ==== "F"
    T ~ true.displayFmt(Opts.strictpad(3, 2))    ==== " T "
    T ~ 'm'.display                              ==== "m"
    T ~ 'm'.displayFmt(Opts.padded(5, 3))        ==== "  m  "
    T ~ (-5: Byte).display                       ==== "-5"
    T ~ UByte(251).display                       ==== "251"
    T ~ (-555: Short).display                    ==== "-555"
    T ~ (-555555).display                        ==== "-555555"
    T ~ UInt(-555555).display                    ==== "4294411741"
    T ~ (-5555555555L).display                   ==== "-5555555555"
    T ~ ULong(-5555555555L).display              ==== "18446744068153996061"
    T ~ "salmon".display                         ==== "salmon"
    T ~ "salmon".displayFmt(Opts.strict(5))      ==== "sal.."
    T ~ "sturgeon".displayFmt(Opts.strict(7))    ==== "stur..."
    T ~ "sturgeon".displayFmt(Opts.strict(7, 7)) ==== "...geon"
    T ~ "salmon".displayFmt(Opts.padleft(8))     ==== "  salmon"
    T ~ "salmon".displayFmt(Opts.padright(8))    ==== "salmon  "
    T ~ "salmon".displayFmt(Opts.padded(8, 4))   ==== " salmon "
    T ~ "salmon".displayFmt(Opts.padded(9, 4))   ==== " salmon  "
    T ~ "salmon".displayFmt(Opts.padded(9, 5))   ==== "  salmon "
    */

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

    T ~ " \uFEFF".bytes.hasBOM                   ==== false
    T ~ "\uFEFF".bytes.hasBOM                    ==== true
    T ~ "\uFEFFDon't do this.".bytes.bomlessUtf8 ==== "Don't do this."
    T ~ "Always do it like this!".bytes.utf8     ==== "Always do it like this!"

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
    target() = 0
    T ~ EioBase64.decodeRangeInto(b.stringEncode64url, 24, 48)(target, 16) ==== 18 --: typed[Int Or Err]
    T ~ { EioBase64.decodeRangeInto(b.stringEncode64url, 64, 88)(target, 60); target.slice(52, 86) } =**= (z8 ++ b.encode64url.slice(64, 88).decode64.get ++ z8)
    T ~ { EioBase64.decodeInto(b.stringEncode64url)(target, 8); target.slice(0, 272) } =**= (z8 ++ b ++ z8)
    T ~ EioBase64.decodeRange(b.stringEncode64url, 12, 24).get =**= b.encode64url.slice(12, 24).decode64.get
    target() = 0

    T ~ EioBase64.encodeUrlRange(b, Iv(15, 22)) =**= clq(b.slice(15, 22).encode64url)
    T ~ EioBase64.decodeRangeInto(b.encode64url, Iv(24, 48))(target, 16) ==== 18 --: typed[Int Or Err]
    T ~ { EioBase64.decodeRangeInto(b.encode64url, Iv(64, 88))(target, 60); target.slice(52, 86) } =**= (z8 ++ b.encode64url.slice(64, 88).decode64.get ++ z8)
    T ~ EioBase64.decodeRange(b.encode64url, Iv(12, 24)).get =**= b.encode64url.slice(12, 24).decode64.get
    target() = 0
    T ~ EioBase64.decodeRangeInto(b.stringEncode64url, Iv(24, 48))(target, 16) ==== 18 --: typed[Int Or Err]
    T ~ { EioBase64.decodeRangeInto(b.stringEncode64url, Iv(64, 88))(target, 60); target.slice(52, 86) } =**= (z8 ++ b.encode64url.slice(64, 88).decode64.get ++ z8)
    T ~ EioBase64.decodeRange(b.stringEncode64url, Iv(12, 24)).get =**= b.encode64url.slice(12, 24).decode64.get
    target() = 0

    T ~ EioBase64.encodeUrlRange(b, 15 to 21) =**= clq(b.slice(15, 22).encode64url)
    T ~ EioBase64.decodeRangeInto(b.encode64url, 24 to 47)(target, 16) ==== 18 --: typed[Int Or Err]
    T ~ { EioBase64.decodeRangeInto(b.encode64url, 64 to 87)(target, 60); target.slice(52, 86) } =**= (z8 ++ b.encode64url.slice(64, 88).decode64.get ++ z8)
    T ~ EioBase64.decodeRange(b.encode64url, 12 to 23).get =**= b.encode64url.slice(12, 24).decode64.get
    target() = 0
    T ~ EioBase64.decodeRangeInto(b.stringEncode64url, 24 to 47)(target, 16) ==== 18 --: typed[Int Or Err]
    T ~ { EioBase64.decodeRangeInto(b.stringEncode64url, 64 to 87)(target, 60); target.slice(52, 86) } =**= (z8 ++ b.encode64url.slice(64, 88).decode64.get ++ z8)
    T ~ EioBase64.decodeRange(b.stringEncode64url, 12 to 23).get =**= b.encode64url.slice(12, 24).decode64.get
    target() = 0

    T ~ EioBase64.encodeUrlRange(b, 15 to End-234) =**= clq(b.slice(15, 22).encode64url)
    T ~ EioBase64.decodeRangeInto(b.encode64url, 24 to End-296)(target, 16) ==== 18 --: typed[Int Or Err]
    T ~ { EioBase64.decodeRangeInto(b.encode64url, 64 to End-256)(target, 60); target.slice(52, 86) } =**= (z8 ++ b.encode64url.slice(64, 88).decode64.get ++ z8)
    T ~ EioBase64.decodeRange(b.encode64url, 12 to End-320).get =**= b.encode64url.slice(12, 24).decode64.get
    target() = 0
    T ~ EioBase64.decodeRangeInto(b.stringEncode64url, 24 to End-296)(target, 16) ==== 18 --: typed[Int Or Err]
    T ~ { EioBase64.decodeRangeInto(b.stringEncode64url, 64 to End-256)(target, 60); target.slice(52, 86) } =**= (z8 ++ b.encode64url.slice(64, 88).decode64.get ++ z8)
    T ~ EioBase64.decodeRange(b.stringEncode64url, 12 to End-320).get =**= b.encode64url.slice(12, 24).decode64.get
    target() = 0

    T ~ EioBase85.encodeZmqRange(b, 83, 99) =**= b.slice(83, 99).encode85zmq
    T ~ EioBase85.encodeAsciiRange(b, 83, 99) =**= b.slice(83, 99).encode85ascii
    T ~ EioBase85.decodeZmqRange("Well, HelloWorld to you!", 6, 16).get =**= Array(0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B).map(_.toByte)
    T ~ EioBase85.decodeZmqRange("Well, HelloWorld to you!".bytes, 6, 16).get =**= Array(0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B).map(_.toByte)
    T ~ EioBase85.decodeAsciiRange(b.encode85ascii, 90, 105).get =**= b.encode85ascii.slice(90, 105).decode85ascii.get
    T ~ EioBase85.decodeAsciiRange(b.stringEncode85ascii, 90, 105).get =**= b.encode85ascii.slice(90, 105).decode85ascii.get

    T ~ EioBase85.encodeZmqRange(b, Iv(83, 99)) =**= b.slice(83, 99).encode85zmq
    T ~ EioBase85.encodeAsciiRange(b, Iv(83, 99)) =**= b.slice(83, 99).encode85ascii
    T ~ EioBase85.decodeZmqRange("Well, HelloWorld to you!", Iv(6, 16)).get =**= Array(0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B).map(_.toByte)
    T ~ EioBase85.decodeZmqRange("Well, HelloWorld to you!".bytes, Iv(6, 16)).get =**= Array(0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B).map(_.toByte)
    T ~ EioBase85.decodeAsciiRange(b.encode85ascii, Iv(90, 105)).get =**= b.encode85ascii.slice(90, 105).decode85ascii.get
    T ~ EioBase85.decodeAsciiRange(b.stringEncode85ascii, Iv(90, 105)).get =**= b.encode85ascii.slice(90, 105).decode85ascii.get

    T ~ EioBase85.encodeZmqRange(b, 83 to 98) =**= b.slice(83, 99).encode85zmq
    T ~ EioBase85.encodeAsciiRange(b, 83 to 98) =**= b.slice(83, 99).encode85ascii
    T ~ EioBase85.decodeZmqRange("Well, HelloWorld to you!", 6 until 16).get =**= Array(0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B).map(_.toByte)
    T ~ EioBase85.decodeZmqRange("Well, HelloWorld to you!".bytes, 6 until 16).get =**= Array(0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B).map(_.toByte)
    T ~ EioBase85.decodeAsciiRange(b.encode85ascii, 90 to 104).get =**= b.encode85ascii.slice(90, 105).decode85ascii.get
    T ~ EioBase85.decodeAsciiRange(b.stringEncode85ascii, 90 to 104).get =**= b.encode85ascii.slice(90, 105).decode85ascii.get

    T ~ EioBase85.encodeZmqRange(b, 83 to End-157) =**= b.slice(83, 99).encode85zmq
    T ~ EioBase85.encodeAsciiRange(b, 83 to End-157) =**= b.slice(83, 99).encode85ascii
    T ~ EioBase85.decodeZmqRange("Well, HelloWorld to you!", 6 to End-8).get =**= Array(0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B).map(_.toByte)
    T ~ EioBase85.decodeZmqRange("Well, HelloWorld to you!".bytes, 6 to End-8).get =**= Array(0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B).map(_.toByte)
    T ~ EioBase85.decodeAsciiRange(b.encode85ascii, 90 to End-215).get =**= b.encode85ascii.slice(90, 105).decode85ascii.get
    T ~ EioBase85.decodeAsciiRange(b.stringEncode85ascii, 90 to End-215).get =**= b.encode85ascii.slice(90, 105).decode85ascii.get

    val zok = new ZipEntry("all/fine/here.txt")
    val zwin = new ZipEntry("bad\\win\\name.txt")
    T ~ zok.cleanName  ==== "all/fine/here.txt"
    T ~ zwin.cleanName ==== "bad/win/name.txt"
    T ~ zok.cleanPath  ==== (new java.io.File("all/fine/here.txt")).toPath
    T ~ zwin.cleanPath ==== (new java.io.File("bad/win/name.txt")).toPath

    val bf = "fish".bytes.buffer()
    T ~ bf.remaining     ==== 4
    T ~ bf.get           ==== 'f'.toByte --: typed[Byte]
    T ~ bf.get           ==== 'i'
    T ~ bf.getBytes.utf8 ==== "sh"
    T ~ bf.hasRemaining  ==== false
    def partFishB(b: ByteBuffer): Unit =
      T ~ b.remaining     ==== 2
      T ~ b.get           ==== 'i'.toByte --: typed[Byte]
      T ~ b.getBytes.utf8 ==== "s"
    partFishB("fish".bytes.buffer(1, 3))
    partFishB("fish".bytes.buffer(1 to 2))
    partFishB("fish".bytes.buffer(Iv(1, 3)))
    partFishB("fish".bytes.buffer(1 to End-1))
    val bis = "fish".bytes.input()
    T ~ bis.available    ==== 4
    T ~ { val a = new Array[Byte](4); bis.read(a); a.utf8 } ==== "fish"
    T ~ bis.available    ==== 0
    def partFishI(i: InputStream): Unit =
      T ~ i.available ==== 2
      T ~ { val a = new Array[Byte](2); i.read(a); a.utf8 } ==== "is"
    partFishI("fish".bytes.input(1, 3))
    partFishI("fish".bytes.input(1 to 2))
    partFishI("fish".bytes.input(Iv(1, 3)))
    partFishI("fish".bytes.input(1 to End-1))

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
    val bbis = bb.flip.input
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

    val wsbc = ba.writeChannel()
    T ~ wsbc.bufferAvailableToWrite           ==== 8
    T ~ wsbc.maxAvailableToWrite              ==== 8
    T ~ wsbc.position                         ==== 0
    T ~ wsbc.position(1)                      ==== wsbc
    T ~ wsbc.size                             ==== 0
    T ~ wsbc.write("sup".bytes.input())       ==== 3
    T ~ wsbc.size                             ==== 4
    T ~ wsbc.write("person".bytes.input())    ==== 4
    T ~ ba.drop(1).utf8                       ==== "suppers"
    T ~ wsbc.position(3)                      ==== wsbc
    T ~ wsbc.write("mm".bytes.readChannel())  ==== 2
    T ~ wsbc.position(7)                      ==== wsbc
    T ~ wsbc.write("yes".bytes.readChannel()) ==== 1
    T ~ ba.drop(1).utf8                       ==== "summery"
    T ~ wsbc.position(1)                      ==== wsbc
    T ~ wsbc.write("hi".bytes)                ==== 2
    T ~ wsbc.write("everyone".bytes.buffer()) ==== 5
    T ~ wsbc.write("here".bytes.buffer())     ==== -1
    T ~ wsbc.position(0)                      ==== wsbc
    T ~ { wsbc.read(ab2.buffer()); ab2 }      =**= Array[Byte](0, 'h'.toByte)
    T ~ wsbc.position(0)                      ==== wsbc
    T ~ wsbc.write("e".bytes.input())         ==== 1
    T ~ wsbc.position(0)                      ==== wsbc
    T ~ { wsbc.read(ab2.buffer()); ab2 }      =**= "eh".bytes
    T ~ wsbc.position(0)                      ==== wsbc
    T ~ wsbc.write("YETI".bytes, 2)(1)        ==== 1
    T ~ wsbc.close                            ==== ()
    T ~ wsbc.position(3)                      ==== thrown[ClosedChannelException]
    ba(0) = 'T'
    bb.clear
    val rsbc = ba.readChannel()
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
    T ~ rsbc.read(ab2.writeChannel())            ==== 2
    T ~ ab2.utf8                                 ==== "hi"
    T ~ rsbc.read(ab9.writeChannel())            ==== 5
    T ~ ab9.utf8                                 ==== "every\u0000\u0000\u0000\u0000"
    T ~ rsbc.position(3)                         ==== rsbc
    T ~ rsbc.read(ab3)                           ==== 3
    T ~ rsbc.read(ab3, 1)(1)                     ==== 1
    T ~ ab3.utf8                                 ==== "ere"
    T ~ rsbc.position(1)                         ==== rsbc
    T ~ rsbc.read(ab3.output())(3)               ==== 3
    T ~ ab3.utf8                                 ==== "hie"
    T ~ rsbc.position(4)                         ==== rsbc
    T ~ rsbc.read(ab3.output())(1)               ==== 1
    T ~ ab3.utf8                                 ==== "vie"
    T ~ rsbc.position(6)                         ==== rsbc
    T ~ rsbc.read(ab3.output())                  ==== 2
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
      T ~ sbc.write(big.copyOfRange(nwrote, nwrote + more).buffer()) ==== more
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
    T ~ sbc.read(weird.writeChannel())       ==== weird.length
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

    T ~ q.size                       ==== -1L
    T ~ q.raw.size                   ==== thrown[IOException]
    T ~ { q.touch(); q.exists }      ==== true
    T ~ q.size                       ==== 0L
    T ~ p.isDirectory                ==== true
    T ~ q.isDirectory                ==== false
    T ~ (p / "dir").exists           ==== false
    T ~ (p / "dir").mkdir()          ==== true  --: typed[Boolean Or Err]
    T ~ (p / "dir").mkdir()          ==== false
    T ~ (p / "dir").raw.mkdir()      ==== thrown[IOException]
    T ~ (p / "a").exists             ==== false
    T ~ (p / "a/b").exists           ==== false
    T ~ (p / "a/b").mkdirs()         ==== ()    --: typed[Unit Or Err]
    T ~ (p / "a").exists             ==== true
    T ~ (p / "a" / "b").exists       ==== true
    T ~ (p / "eel").exists           ==== false
    T ~ (p / "eel/x").mkParents()    ==== ()    --: typed[Unit Or Err]
    T ~ (p / "eel").exists           ==== true

    val ft1 = ft - 1.s
    (p / "rw").mkdir()   // Might be race with readWriteTest, but it shouldn't matter!
    T ~ (q.raw.time - ft).in(0.s, 10.s)       ==== true
    T ~ (q.time.get - ft).in(0.s, 10.s)       ==== true
    T ~ { q.raw.time = ft; q.raw.time }       ==== ft
    T ~ { ((q.time = ft1).isIs, q.time.get) } ==== ((true, ft1))
    T ~ { q.touch(); q.time.get == ft }       ==== false
    T ~ p.paths.sorted                        =**= Array(p / "a", p / "dir", p / "eel", p / "quartz.txt", p / "rw")
    T ~ q.raw.delete()                        ==== ()
    T ~ q.raw.delete()                        ==== thrown[IOException]
    T ~ q.raw.time                            ==== thrown[IOException]
    T ~ { q.raw.time = ft1 }                  ==== thrown[IOException]
    T ~ q.time.alt                            ==== runtype[String]
    T ~ { q.time = ft1 }.alt                  ==== runtype[String]
    T ~ q.exists                              ==== false

    T ~ q.slurp     ==== runtype[Alt[?]]
    T ~ q.slurp.alt ==== runtype[String]
    T ~ q.gulp      ==== runtype[Alt[?]]
    T ~ q.gulp.alt  ==== runtype[String]
    T ~ q.raw.slurp ==== thrown[IOException]
    T ~ q.raw.gulp  ==== thrown[IOException]

    val correctly: Unit Or Err = Or.Ret:
      T ~ q.write("blorp".bytes)              ==== ()              --: typed[Unit Or Err]
      T ~ q.gulp.?                            =**= "blorp".bytes
      T ~ q.append("y".bytes)                 ==== ()              --: typed[Unit Or Err]
      T ~ q.slurp.?                           =**= Array("blorpy")
      T ~ q.createIfAbsent("bad".bytes)       ==== false           --: typed[Boolean Or Err]
      T ~ q.create("bad".bytes)               ==== runtype[Alt[?]]
      T ~ q.create("bad".bytes).alt           ==== runtype[String]
      T ~ q.raw.create("bad".bytes)           ==== thrown[IOException]
      T ~ q.delete()                          ==== true
      T ~ q.delete()                          ==== false
      T ~ q.exists                            ==== false
      T ~ q.createIfAbsent("hi\nworld".bytes) ==== true
      T ~ q.slurp.?                           =**= Array("hi", "world")
      T ~ q.delete()                          ==== true
      T ~ q.create("hello\nworld".bytes)      ==== ()              --: typed[Unit Or Err]
      T ~ q.slurp.?                           =**= Array("hello", "world")
      T ~ q.delete()                          ==== true
      T ~ q.exists                            ==== false
      T ~ q.append("foo".bytes)               ==== ()
      T ~ q.gulp.?.utf8                       ==== "foo"
      T ~ q.raw.write("cod".bytes)            ==== ()
      T ~ q.gulp.?.utf8                       ==== "cod"
      T ~ q.raw.append("fish".bytes)          ==== ()
      T ~ q.gulp.?.utf8                       ==== "codfish"
      T ~ q.writeLines("eel" :: Nil)          ==== ()
      T ~ q.raw.slurp                         =**= Array("eel")
      T ~ q.appendLines("cod" :: Nil)         ==== ()
      T ~ q.raw.slurp                         =**= Array("eel", "cod")
      T ~ q.append("perch".bytes)             ==== ()
      T ~ q.raw.slurp                         =**= Array("eel", "cod", "perch")
      T ~ q.append("bass".bytes)              ==== ()
      T ~ q.raw.slurp                         =**= Array("eel", "cod", "perchbass")
      T ~ q.delete()                          ==== true
      T ~ q.createLines("cod" :: Nil)         ==== ()
      T ~ q.raw.gulp.utf8                     ==== "cod\n"
      T ~ q.delete()                          ==== true
      T ~ q.raw.createLines("cod" :: Nil)     ==== ()
      T ~ q.raw.gulp.utf8                     ==== "cod\n"
      T ~ q.delete()                          ==== true
      T ~ q.createLinesIfAbsent("cod" :: Nil) ==== true
      T ~ q.raw.gulp.utf8                     ==== "cod\n"
      T ~ q.createLinesIfAbsent("eel" :: Nil) ==== false
      T ~ q.raw.createLines("cod" :: Nil)     ==== thrown[IOException]
      T ~ q.createLines("cod" :: Nil)         ==== runtype[Alt[?]]
      T ~ q.createLines("cod" :: Nil).alt     ==== runtype[String]
      T ~ q.copyTo(p / "fish.txt")            ==== ()             --: typed[Unit Or Err]
      T ~ (p / "fish.txt").slurp.?            =**= Array("cod")
      T ~ (p / "swan.txt").copyTo(q).alt      ==== runtype[String]
      T ~ q.moveTo(p / "move.txt")            ==== ()             --: typed[Unit Or Err]
      T ~ (p / "move.txt").slurp.?            =**= Array("cod")
      T ~ q.moveTo(p / "move.txt").alt        ==== runtype[String]
      T ~ q.exists                            ==== false
      T ~ (p / "move.txt").raw.moveTo(q)      ==== ()
      T ~ (p / "move.txt").raw.moveTo(q)      ==== thrown[IOException]
      T ~ (p / "swan.txt").raw.copyTo(q)      ==== thrown[IOException]
      T ~ q.raw.copyTo(p / "bass.txt")        ==== ()
      T ~ (p / "bass.txt").slurp.?            =**= Array("cod")
      T ~ q.writeLines("eel" :: Nil)          ==== ()
      T ~ q.copyCreate(p / "pike.txt")        ==== ()
      T ~ q.copyTo(p / "bass.txt")            ==== ()
      T ~ q.copyCreate(p / "bass.txt").alt    ==== runtype[String]
      T ~ (p / "pike.txt").copyInto(p / "a")  ==== (p / "a/pike.txt")
      T ~ (p / "like.txt").copyInto(p / "a")  ==== runtype[Alt[?]]
      T ~ (p / "pike.txt").copyInto(p / "c")  ==== runtype[Alt[?]]
      T ~ (p / "pike.txt").write("pike".bytes)==== ()
      T ~ (p / "pike.txt").copyInto(p / "a")  ==== (p / "a/pike.txt")
      T ~ (p / "a/pike.txt").gulp.?           =**= "pike".bytes
      T ~ (p / "bass.txt").moveInto(p / "a")  ==== (p / "a/bass.txt")
      T ~ (p / "bass.txt").moveInto(p / "a")  ==== runtype[Alt[?]]
      T ~ (p / "move.txt").moveInto(p / "c")  ==== runtype[Alt[?]]
      T ~ (p / "a/bass.txt").gulp.?           =**= "eel\n".bytes

    T ~ correctly ==== ()

    val ab2 = "ft".bytes
    q.delete()
    Resource(q.raw.openCreate())(_.close)(x => T ~ x ==== runtype[BufferedOutputStream])
    T ~ q.exists                                     ==== true
    T ~ q.size                                       ==== 0L
    Resource(q.raw.openAppend())(_.close){ x => x.write('e'); T ~ x ==== runtype[BufferedOutputStream] }
    T ~ q.size                                                      ==== 1L
    Resource(q.raw.openRead())(_.close)(x =>       T ~ x ==== runtype[BufferedInputStream])
    T ~ Resource(q.raw.openRead())(_.close)(_.available) ==== 1
    T ~ Resource(q.raw.openRead())(_.close)(_.read)      ==== 'e'
    Resource(q.raw.openWrite())(_.close){ o => o.write("eel".bytes);   T ~ o   ==== runtype[BufferedOutputStream] }
    T ~ Resource(q.raw.openIO())(_.close)(_.position(1).write(ab2.buffer()))   ==== 2
    T ~ Resource(q.raw.openIO())(_.close){o => o.read(ab2.buffer()); ab2.utf8} ==== "ef"

    "ft".bytes.inject(ab2)
    q.delete()
    Resource.nice(q.openCreate())(_.close)(x => T ~ x ==== runtype[BufferedOutputStream])
    T ~ q.exists                                      ==== true
    T ~ q.size                                        ==== 0L
    Resource.nice(q.openAppend())(_.close){ x => x.write('e'); T ~ x ==== runtype[BufferedOutputStream] }
    T ~ q.size                                                       ==== 1L
    Resource.nice(q.openRead())(_.close)(x =>       T ~ x ==== runtype[BufferedInputStream])
    T ~ Resource.nice(q.openRead())(_.close)(_.available) ==== 1
    T ~ Resource.nice(q.openRead())(_.close)(_.read)      ==== 'e'
    Resource.nice(q.openWrite())(_.close){ o => o.write("eel".bytes);   T ~ o   ==== runtype[BufferedOutputStream] }
    T ~ Resource.nice(q.openIO())(_.close)(_.position(1).write(ab2.buffer()))   ==== 2
    T ~ Resource.nice(q.openIO())(_.close){o => o.read(ab2.buffer()); ab2.utf8} ==== "ef"

    val ps = "temp/eio/sym".path
    T ~ (p / "x" / "y").mkdirs().isIs       ==== true
    T ~ (p / "sym").isSymlink               ==== false
    T ~ ps.raw.symlink                      ==== thrown[IOException]
    T ~ ps.symlink.alt                      ==== runtype[String]
    T ~ { ps.makeSymlink("x"); ps.symlink } ==== "x"       --: typed[String Or Err]
    T ~ ps.followSymlink                    ==== (p / "x") --: typed[Path Or Err]
    T ~ ps.exists                           ==== true
    T ~ (ps / "y").exists                   ==== true
    T ~ (p / "x").isSymlink                 ==== false
    T ~ ps.isSymlink                        ==== true
    T ~ ps.real                             ==== (p.real / "x")
    T ~ (ps / "y").real                     ==== (p.real / "x/y")
    (p / "x" / "y").delete()
    (p / "x").delete()
    T ~ (p / "x").exists                    ==== false
    T ~ ps.exists                           ==== false
    T ~ ps.isSymlink                        ==== true
    T ~ ps.real                             ==== (p.real / "x")
    T ~ (ps / "y").real                     ==== (p.real / "x/y")
    T ~ (ps / "y").mkParents()              ==== ()
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
    T ~ (a2b / "w").real                    ==== (p.real / "ab/w")

    val mfs = com.github.marschall.memoryfilesystem.MemoryFileSystemBuilder.newEmpty().build()
    T ~ "/life/fish".pathIn(mfs).exists          ==== false
    T ~ "/life/fish".pathIn(mfs).mkdirs()        ==== ()
    T ~ "/life/fish".pathIn(mfs).raw.file        ==== thrown[Exception]
    T ~ "/life/fish".pathIn(mfs).file            ==== runtype[Alt[?]]
    val mp = "/life/fish".pathIn(mfs)
    T ~ (mp / "eel.txt").write("hi".bytes)       ==== ()
    T ~ "/life/fish/eel.txt".pathLike(mp).exists ==== true


  @Test
  def cleasyTest(): Unit =
    import cleasy.*

    T ~ _u("")  ==== Is(()) --: typed[Unit Or Err]
    T ~ _u("x") ==== runtype[Alt[?]]

    Array("t", "T", "true", "y", "Y", "yes", "on").peek(): x =>
      T ~ _tf(x) ==== Is(true) --: typed[Boolean Or Err]
    Array("f", "F", "false", "n", "N", "no", "off").peek(): x =>
      T ~ _tf(x) ==== Is(false) --: typed[Boolean Or Err]
    T ~ _tf("eel") ==== runtype[Alt[?]]

    T ~ _int("715") ==== Is(715) --: typed[Int Or Err]
    T ~ _int("-74") ==== Is(-74)
    T ~ _int("2.9") ==== runtype[Alt[?]]
    T ~ _int("eel") ==== runtype[Alt[?]]
    T ~ _int("")    ==== runtype[Alt[?]]

    T ~ _uint("715") ==== Is(715) --: typed[UInt Or Err]
    T ~ _uint("-74") ==== runtype[Alt[?]]
    T ~ _uint("2.9") ==== runtype[Alt[?]]
    T ~ _uint("eel") ==== runtype[Alt[?]]

    T ~ _long("7150250350") ==== Is(7150250350L) --: typed[Long Or Err]
    T ~ _long("-74")        ==== Is(-74L)
    T ~ _long("2.9")        ==== runtype[Alt[?]]
    T ~ _long("eel")        ==== runtype[Alt[?]]

    T ~ _ulong("7150250350") ==== Is(7150250350L) --: typed[ULong Or Err]
    T ~ _ulong("-74")        ==== runtype[Alt[?]]
    T ~ _ulong("2.9")        ==== runtype[Alt[?]]
    T ~ _ulong("eel")        ==== runtype[Alt[?]]

    T ~ _double("3.14159")  ==== Is(3.14159) --: typed[Double Or Err]
    T ~ _double("-0.31e1")  ==== Is(-3.1)
    T ~ _double("infinity") ==== Is(Double.PositiveInfinity)
    T ~ _double("inf")      ==== Is(Double.PositiveInfinity)
    T ~ _double("-Inf")     ==== Is(Double.NegativeInfinity)
    T ~ _double("nan")      ==== Is(Double.NaN)
    T ~ _double("-NaN")     ==== Is(Double.NaN)
    T ~ _double("eel")      ==== runtype[Alt[?]]

    T ~ _str("herring") ==== Is("herring") --: typed[String Or Err]

    T ~ _path("eel/cod") ==== Is("eel/cod".path) --: typed[Path Or Err]

    T ~ _dir("eio/test")  ==== Is("eio/test".path) --: typed[Path Or Err]
    T ~ _dir("not/found") ==== runtype[Alt[?]]

    T ~ _file("eio/test/src/EioTest.scala") ==== Is("eio/test/src/EioTest.scala".path) --: typed[Path Or Err]
    T ~ _file("not/found")                  ==== runtype[Alt[?]]

    T ~ the("eel")("eel")   ==== Is("eel")
    T ~ ("y" | "n")("y")    ==== Is("y")
    T ~ ("y" | "n")("n")    ==== Is("n")
    T ~ ("y" | "n")("eel")  ==== runtype[Alt[?]]

    T ~ _int.maybe("")  ==== None --: typed[Option[Int] Or Err]
    T ~ _int.maybe("1") ==== Some(1)
    T ~ _int.maybe("x") ==== runtype[Alt[?]]

    T ~ (_int | the("no"))("12")  ==== Right(12) --: typed[Either["no", Int] Or Err]
    T ~ (_int | the("no"))("no")  ==== Left("no")
    T ~ (_int | the("no"))("eel") ==== runtype[Alt[?]]

    T ~ _str.userString            ==== "text"
    T ~ _str.desc("hi").userString ==== "hi"

    T ~ "eel".x               ==== typed[OptNLabel["eel"]]
    T ~ "eel".x.label         ==== "eel"
    T ~ 'e'.x                 ==== typed[OptNChar['e']]
    T ~ 'e'.x.short           ==== 'e'
    T ~ ("eel" ~ 'e')         ==== typed[OptLabelChar['e', "eel"]]
    T ~ ("eel" ~ 'e').label   ==== "eel"
    T ~ ("eel" ~ 'e').short   ==== 'e'
    T ~ ("eel".x ~ 'e')       ==== typed[OptNLabelChar['e', "eel"]]
    T ~ ("eel".x ~ 'e').label ==== "eel"
    T ~ ("eel".x ~ 'e').short ==== 'e'

    T ! """'e' ~ "eel""""
    T ! """("eel" ~ 'e').x"""

    T ~ ("eel"   ~ _int)         ==== typed[Opt[Int, '-', "eel"]]
    T ~ ("eel"   ~ _int).label   ==== "eel"
    T ~ ("eel"   ~ _int).short   ==== '-'
    T ~ ("eel"   ~ _int).parse   ==== _int
    T ~ ('e'     ~ _int)         ==== typed[Opt[Int, 'e', "e"]]
    T ~ ('e'     ~ _int).label   ==== "e"
    T ~ ('e'     ~ _int).short   ==== 'e'
    T ~ ('e'     ~ _int).parse   ==== _int
    T ~ ("eel".x ~ _int)         ==== typed[OptN[Int, '-', "eel"]]
    T ~ ("eel".x ~ _int).label   ==== "eel"
    T ~ ("eel".x ~ _int).short   ==== '-'
    T ~ ("eel".x ~ _int).parse   ==== _int
    T ~ ("eel".x ~ _int).default ==== None
    T ~ ('e'.x   ~ _int)         ==== typed[OptN[Int, 'e', "e"]]
    T ~ ('e'.x   ~ _int).label   ==== "e"
    T ~ ('e'.x   ~ _int).short   ==== 'e'
    T ~ ('e'.x   ~ _int).parse   ==== _int
    T ~ ('e'.x   ~ _int).default ==== None

    T ~ ("eel"   ~ (_int, () => 7))            ==== typed[OptD[Int, '-', "eel"]]
    T ~ ("eel"   ~ (_int, () => 7)).label      ==== "eel"
    T ~ ("eel"   ~ (_int, () => 7)).short      ==== '-'
    T ~ ("eel"   ~ (_int, () => 7)).parse      ==== _int
    T ~ ("eel"   ~ (_int, () => 7)).default()  ==== 7
    T ~ ('e'     ~ (_int, () => 7))            ==== typed[OptD[Int, 'e', "e"]]
    T ~ ('e'     ~ (_int, () => 7)).label      ==== "e"
    T ~ ('e'     ~ (_int, () => 7)).short      ==== 'e'
    T ~ ('e'     ~ (_int, () => 7)).parse      ==== _int
    T ~ ('e'     ~ (_int, () => 7)).default()  ==== 7
    T ~ ("eel".x ~ (_int, () => 7))            ==== typed[OptN[Int, '-', "eel"]]
    T ~ ("eel".x ~ (_int, () => 7)).label      ==== "eel"
    T ~ ("eel".x ~ (_int, () => 7)).short      ==== '-'
    T ~ ("eel".x ~ (_int, () => 7)).parse      ==== _int
    T ~ ("eel".x ~ (_int, () => 7)).getDefault ==== Some(7)
    T ~ ('e'.x   ~ (_int, () => 7))            ==== typed[OptN[Int, 'e', "e"]]
    T ~ ('e'.x   ~ (_int, () => 7)).label      ==== "e"
    T ~ ('e'.x   ~ (_int, () => 7)).short      ==== 'e'
    T ~ ('e'.x   ~ (_int, () => 7)).parse      ==== _int
    T ~ ('e'.x   ~ (_int, () => 7)).getDefault ==== Some(7)

    T ~ ("eel"   % "fish")         ==== typed[Opt[Unit, '-', "eel"]]
    T ~ ("eel"   % "fish").label   ==== "eel"
    T ~ ("eel"   % "fish").short   ==== '-'
    T ~ ("eel"   % "fish").parse   ==== _u
    T ~ ("eel"   % "fish").about   ==== "fish"
    T ~ ('e'     % "fish")         ==== typed[Opt[Unit, 'e', "e"]]
    T ~ ('e'     % "fish").label   ==== "e"
    T ~ ('e'     % "fish").short   ==== 'e'
    T ~ ('e'     % "fish").parse   ==== _u
    T ~ ('e'     % "fish").about   ==== "fish"
    T ~ ("eel".x % "fish")         ==== typed[OptN[Unit, '-', "eel"]]
    T ~ ("eel".x % "fish").label   ==== "eel"
    T ~ ("eel".x % "fish").short   ==== '-'
    T ~ ("eel".x % "fish").parse   ==== _u
    T ~ ("eel".x % "fish").default ==== None
    T ~ ("eel".x % "fish").about   ==== "fish"
    T ~ ('e'.x   % "fish")         ==== typed[OptN[Unit, 'e', "e"]]
    T ~ ('e'.x   % "fish").label   ==== "e"
    T ~ ('e'.x   % "fish").short   ==== 'e'
    T ~ ('e'.x   % "fish").parse   ==== _u
    T ~ ('e'.x   % "fish").default ==== None
    T ~ ('e'.x   % "fish").about   ==== "fish"

    T ~ Opt("eel"                    )          ==== typed[Opt [Unit, '-', "eel"]]
    T ~ Opt(       'e'               )          ==== typed[Opt [Unit, 'e', "e"  ]]
    T ~ Opt("eel", 'e'               )          ==== typed[Opt [Unit, 'e', "eel"]]
    T ~ Opt("eel",      _int         )          ==== typed[Opt [Int,  '-', "eel"]]
    T ~ Opt(       'e', _int         )          ==== typed[Opt [Int,  'e', "e"  ]]
    T ~ Opt("eel", 'e', _int         )          ==== typed[Opt [Int,  'e', "eel"]]
    T ~ Opt("eel",      _int, () => 4)          ==== typed[OptD[Int,  '-', "eel"]]
    T ~ Opt(       'e', _int, () => 4)          ==== typed[OptD[Int,  'e', "e"  ]]
    T ~ Opt("eel", 'e', _int, () => 4)          ==== typed[OptD[Int,  'e', "eel"]]
    T ~ Opt("eel"                    ).label    ==== "eel"
    T ~ Opt(       'e'               ).label    ==== "e"
    T ~ Opt("eel", 'e'               ).label    ==== "eel"
    T ~ Opt("eel",      _int         ).label    ==== "eel"
    T ~ Opt(       'e', _int         ).label    ==== "e"
    T ~ Opt("eel", 'e', _int         ).label    ==== "eel"
    T ~ Opt("eel",      _int, () => 4).label    ==== "eel"
    T ~ Opt(       'e', _int, () => 4).label    ==== "e"
    T ~ Opt("eel", 'e', _int, () => 4).label    ==== "eel"
    T ~ Opt("eel"                    ).short    ==== '-'
    T ~ Opt(       'e'               ).short    ==== 'e'
    T ~ Opt("eel", 'e'               ).short    ==== 'e'
    T ~ Opt("eel",      _int         ).short    ==== '-'
    T ~ Opt(       'e', _int         ).short    ==== 'e'
    T ~ Opt("eel", 'e', _int         ).short    ==== 'e'
    T ~ Opt("eel",      _int, () => 4).short    ==== '-'
    T ~ Opt(       'e', _int, () => 4).short    ==== 'e'
    T ~ Opt("eel", 'e', _int, () => 4).short    ==== 'e'
    T ~ Opt("eel"                    ).parse    ==== _u
    T ~ Opt(       'e'               ).parse    ==== _u
    T ~ Opt("eel", 'e'               ).parse    ==== _u
    T ~ Opt("eel",      _int         ).parse    ==== _int
    T ~ Opt(       'e', _int         ).parse    ==== _int
    T ~ Opt("eel", 'e', _int         ).parse    ==== _int
    T ~ Opt("eel",      _int, () => 4).parse    ==== _int
    T ~ Opt(       'e', _int, () => 4).parse    ==== _int
    T ~ Opt("eel", 'e', _int, () => 4).parse    ==== _int
    T ~ Opt("eel",      _int, () => 4).default() ==== 4
    T ~ Opt(       'e', _int, () => 4).default() ==== 4
    T ~ Opt("eel", 'e', _int, () => 4).default() ==== 4

    T ~ OptN("eel"                    )           ==== typed[OptN[Unit, '-', "eel"]]
    T ~ OptN(       'e'               )           ==== typed[OptN[Unit, 'e', "e"  ]]
    T ~ OptN("eel", 'e'               )           ==== typed[OptN[Unit, 'e', "eel"]]
    T ~ OptN("eel",      _int         )           ==== typed[OptN[Int,  '-', "eel"]]
    T ~ OptN(       'e', _int         )           ==== typed[OptN[Int,  'e', "e"  ]]
    T ~ OptN("eel", 'e', _int         )           ==== typed[OptN[Int,  'e', "eel"]]
    T ~ OptN("eel",      _int, () => 4)           ==== typed[OptN[Int,  '-', "eel"]]
    T ~ OptN(       'e', _int, () => 4)           ==== typed[OptN[Int,  'e', "e"  ]]
    T ~ OptN("eel", 'e', _int, () => 4)           ==== typed[OptN[Int,  'e', "eel"]]
    T ~ OptN("eel"                    ).label     ==== "eel"
    T ~ OptN(       'e'               ).label     ==== "e"
    T ~ OptN("eel", 'e'               ).label     ==== "eel"
    T ~ OptN("eel",      _int         ).label     ==== "eel"
    T ~ OptN(       'e', _int         ).label     ==== "e"
    T ~ OptN("eel", 'e', _int         ).label     ==== "eel"
    T ~ OptN("eel",      _int, () => 4).label     ==== "eel"
    T ~ OptN(       'e', _int, () => 4).label     ==== "e"
    T ~ OptN("eel", 'e', _int, () => 4).label     ==== "eel"
    T ~ OptN("eel"                    ).short     ==== '-'
    T ~ OptN(       'e'               ).short     ==== 'e'
    T ~ OptN("eel", 'e'               ).short     ==== 'e'
    T ~ OptN("eel",      _int         ).short     ==== '-'
    T ~ OptN(       'e', _int         ).short     ==== 'e'
    T ~ OptN("eel", 'e', _int         ).short     ==== 'e'
    T ~ OptN("eel",      _int, () => 4).short     ==== '-'
    T ~ OptN(       'e', _int, () => 4).short     ==== 'e'
    T ~ OptN("eel", 'e', _int, () => 4).short     ==== 'e'
    T ~ OptN("eel"                    ).parse     ==== _u
    T ~ OptN(       'e'               ).parse     ==== _u
    T ~ OptN("eel", 'e'               ).parse     ==== _u
    T ~ OptN("eel",      _int         ).parse     ==== _int
    T ~ OptN(       'e', _int         ).parse     ==== _int
    T ~ OptN("eel", 'e', _int         ).parse     ==== _int
    T ~ OptN("eel",      _int, () => 4).parse     ==== _int
    T ~ OptN(       'e', _int, () => 4).parse     ==== _int
    T ~ OptN("eel", 'e', _int, () => 4).parse     ==== _int
    T ~ OptN("eel"                    ).getDefault ==== None
    T ~ OptN(       'e'               ).getDefault ==== None
    T ~ OptN("eel", 'e'               ).getDefault ==== None
    T ~ OptN("eel",      _int         ).getDefault ==== None
    T ~ OptN(       'e', _int         ).getDefault ==== None
    T ~ OptN("eel", 'e', _int         ).getDefault ==== None
    T ~ OptN("eel",      _int, () => 4).getDefault ==== Some(4)
    T ~ OptN(       'e', _int, () => 4).getDefault ==== Some(4)
    T ~ OptN("eel", 'e', _int, () => 4).getDefault ==== Some(4)

    val nine = () => 9L.u
    T ~ Opt.of("herring", 'h', _ulong)                 ==== Opt("herring", 'h', _ulong)
    T ~ Opt.withDefault("herring", 'h', _ulong, nine)  ==== Opt("herring", 'h', _ulong, nine)
    T ~ OptD("herring", _ulong, nine)                  ==== Opt("herring", _ulong, nine)
    T ~ OptD('h', _ulong, nine)                        ==== Opt('h', _ulong, nine)
    T ~ OptD("herring", 'h', _ulong, nine)             ==== Opt("herring", 'h', _ulong, nine)
    T ~ OptD.of("herring", 'h', _ulong, nine)          ==== Opt("herring", 'h', _ulong, nine)
    T ~ OptN.of("herring", 'h', _ulong)                ==== OptN("herring", 'h', _ulong)
    T ~ OptN.withDefault("herring", 'h', _ulong, nine) ==== OptN("herring", 'h', _ulong, nine)

    val bass = Opt.of("bass", 'b', _tf)
    T ~ bass.comment("fish")       ==== typed[Opt[Boolean, 'b', "bass"]]
    T ~ (bass % "fish")            ==== typed[Opt[Boolean, 'b', "bass"]]
    T ~ bass.comment("fish").label ==== bass.label
    T ~ bass.comment("fish").short ==== bass.short
    T ~ bass.comment("fish").parse ==== bass.parse
    T ~ bass.comment("fish").about ==== "fish"
    T ~ (bass % "fish")            ==== bass.comment("fish")

    val bassfish = bass % "fish"
    val nope = () => false
    T ~ bassfish.x            ==== typed[OptN[Boolean, 'b', "bass"]]
    T ~ bassfish.x.label      ==== bassfish.label
    T ~ bassfish.x.short      ==== bassfish.short
    T ~ bassfish.x.parse      ==== bassfish.parse
    T ~ bassfish.x.about      ==== bassfish.about
    T ~ bassfish.x.getDefault ==== None
    T ~ bassfish.withDefault(nope)           ==== typed[OptD[Boolean, 'b', "bass"]]
    T ~ bassfish.withDefault(nope).label     ==== bassfish.label
    T ~ bassfish.withDefault(nope).short     ==== bassfish.short
    T ~ bassfish.withDefault(nope).parse     ==== bassfish.parse
    T ~ bassfish.withDefault(nope).about     ==== bassfish.about
    T ~ bassfish.withDefault(nope).default() ==== false

    val sole = OptD.of("sole", 's', _tf, nope)
    T ~ sole.comment("fish")       ==== typed[OptD[Boolean, 's', "sole"]]
    T ~ (sole % "fish")            ==== typed[OptD[Boolean, 's', "sole"]]
    T ~ sole.comment("fish").label ==== sole.label
    T ~ sole.comment("fish").short ==== sole.short
    T ~ sole.comment("fish").parse ==== sole.parse
    T ~ sole.comment("fish").about ==== "fish"
    T ~ (sole % "fish")            ==== sole.comment("fish")

    val solefish = sole % "fish"
    T ~ solefish.x            ==== typed[OptN[Boolean, 's', "sole"]]
    T ~ solefish.x.label      ==== solefish.label
    T ~ solefish.x.short      ==== solefish.short
    T ~ solefish.x.parse      ==== solefish.parse
    T ~ solefish.x.about      ==== solefish.about
    T ~ solefish.x.getDefault ==== Some(false)

    val tuna = OptN.withDefault("tuna", 't', _tf, nope)
    T ~ tuna.comment("fish")         ==== typed[OptN[Boolean, 't', "tuna"]]
    T ~ (tuna % "fish")              ==== typed[OptN[Boolean, 't', "tuna"]]
    T ~ tuna.comment("fish").label   ==== tuna.label
    T ~ tuna.comment("fish").short   ==== tuna.short
    T ~ tuna.comment("fish").parse   ==== tuna.parse
    T ~ tuna.comment("fish").default ==== tuna.default
    T ~ tuna.comment("fish").about   ==== "fish"
    T ~ (tuna % "fish")              ==== tuna.comment("fish")

    val c0 = Cleasy()
      -- "eel"
      -- 'h'
      -- "bass" ~ 'b'
      -- "minnow".x
      -- 't'.x
      -- "salmon".x ~ 's'

    T ~ c0.parse(Array("--eel")).get("eel")                   ==== Some(())
    T ~ c0.parse(Array("-h")).get("h")                        ==== Some(())
    T ~ c0.parse(Array("--bass")).get("bass")                 ==== Some(())
    T ~ c0.parse(Array("-b")).get("bass")                     ==== Some(())
    T ~ c0.parse(Array("--minnow", "--minnow")).get("minnow") ==== List((), ())
    T ~ c0.parse(Array("-tttt", "-t")).get("t")               ==== List((), (), (), (), ())
    T ~ c0.parse(Array("--salmon", "-s")).get("salmon")       ==== List((), ())
    T ~ c0.parse(Array("--sturgeon"))                         ==== runtype[Alt[?]]
    T ~ c0.parse(Array("-hz"))                                ==== runtype[Alt[?]]
    T ~ c0.parse(Array("-zh"))                                ==== runtype[Alt[?]]
    T ~ c0.parse(Array("eel", "-z"))                          ==== runtype[Alt[?]]

    T ! """c0 ~ 'b'"""
    T ! """c0 ~ "t""""

    val c1 = Cleasy()
      + Opt("perch", _int)
      + Opt("bass", _int.maybe)
      + OptD("minnow", _int, () => 2)
      + OptD("cod", _int.maybe, () => Some(6))

    T ~ c1.parse(Array("--perch=4")).get("perch")   ==== Some(4)
    T ~ c1.parse(Array("--perch"))                  ==== runtype[Alt[?]]
    T ~ c1.parse(Array("--bass=5")).get("bass")     ==== Some(Some(5))
    T ~ c1.parse(Array("--bass")).get("bass")       ==== Some(None)
    T ~ c1.parse(Array("--minnow=3")).get("minnow") ==== 3
    T ~ c1.parse(Array("foo", "bar")).get("minnow") ==== 2
    T ~ c1.parse(Array("--cod=7")).get("cod")       ==== Some(7)
    T ~ c1.parse(Array("--cod")).get("cod")         ==== None
    T ~ c1.parse(Array("foo", "bar")).get("cod")    ==== Some(6)

    val c2 = Cleasy()
      + OptN("perch", _int)
      + OptN("bass", _int.maybe)
      + OptN("minnow", _int, () => 2)
      + OptN("cod", _int.maybe, () => Some(6))

    T ~ c2.parse(Array("foo")).get("perch")                       ==== Nil
    T ~ c2.parse(Array("--perch=5", "--perch=3")).get("perch")    ==== List(5, 3)
    T ~ c2.parse(Array("foo")).get("bass")                        ==== Nil
    T ~ c2.parse(Array("--bass=5", "--bass")).get("bass")         ==== List(Some(5), None)
    T ~ c2.parse(Array("foo")).get("minnow")                      ==== List(2)
    T ~ c2.parse(Array("--minnow=8")).get("minnow")               ==== List(8)
    T ~ c2.parse(Array("--minnow=8", "--minnow=1")).get("minnow") ==== List(8, 1)
    T ~ c2.parse(Array("foo")).get("cod")                         ==== List(Some(6))
    T ~ c2.parse(Array("--cod=7")).get("cod")                     ==== List(Some(7))
    T ~ c2.parse(Array("--cod")).get("cod")                       ==== List(None)
    T ~ c2.parse(Array("--cod=7", "--cod")).get("cod")            ==== List(Some(7), None)

    T ! """c2 + OptN("perch", _tf)"""
    T ! """c2 + Opt("perch", _tf)"""
    T ! """c2 + OptD("perch", _tf, () => true)"""

    val c3 = Cleasy()
      + Opt('x')
      + OptN('y')
      + Opt('z', _int)
      + OptN('w', _int)
      + OptD('v', _int, () => -1)
      + OptN('u', _int, () => -1)
      + Opt('t', _int.maybe)
      + Opt('s', _str)

    T ~ c3.parse(Array("1", "2", "3", "4")).get.args          =**= Array("1", "2", "3", "4")
    T ~ c3.parse(Array("1", "-x", "2", "3", "4")).get.args    =**= Array("1", "2", "3", "4")
    T ~ c3.parse(Array("1", "-t", "2", "3", "4")).get.args    =**= Array("1", "2", "3", "4")
    T ~ c3.parse(Array("1", "-z", "2", "3", "4")).get.args    =**= Array("1", "3", "4")
    T ~ c3.parse(Array("1", "-v", "2", "3", "4")).get.args    =**= Array("1", "3", "4")
    T ~ c3.parse(Array("1", "2", "3", "4", "-z"))             ==== runtype[Alt[?]]
    T ~ c3.parse(Array("1", "2", "3", "4", "-w"))             ==== runtype[Alt[?]]
    T ~ c3.parse(Array("1", "2", "3", "4", "-v"))             ==== runtype[Alt[?]]
    T ~ c3.parse(Array("1", "2", "3", "4", "-u"))             ==== runtype[Alt[?]]
    T ~ c3.parse(Array("1", "-xz", "2", "3", "4")).get.args   =**= Array("1", "3", "4")
    T ~ c3.parse(Array("1", "-xv", "2", "3", "4")).get.args   =**= Array("1", "3", "4")
    T ~ c3.parse(Array("1", "-y", "2", "3", "-y")).get.args   =**= Array("1", "2", "3")
    T ~ c3.parse(Array("1", "-w", "2", "-w", "3")).get.args   =**= Array("1")
    T ~ c3.parse(Array("1", "-xw", "2", "-yw", "3")).get.args =**= Array("1")
    T ~ c3.parse(Array("1", "-u", "2", "-u", "3")).get.args   =**= Array("1")
    T ~ c3.parse(Array("1", "-xu", "2", "-yu", "3")).get.args =**= Array("1")
    T ~ c3.parse(Array("-z", "-x"))                           ==== runtype[Alt[?]]
    T ~ c3.parse(Array("-w", "-x"))                           ==== runtype[Alt[?]]
    T ~ c3.parse(Array("-v", "-x"))                           ==== runtype[Alt[?]]
    T ~ c3.parse(Array("-u", "-x"))                           ==== runtype[Alt[?]]
    T ~ c3.parse(Array("-s", "-x"))                           ==== runtype[Alt[?]]
    T ~ c3.parse(Array("-z", "--x"))                          ==== runtype[Alt[?]]
    T ~ c3.parse(Array("-w", "--x"))                          ==== runtype[Alt[?]]
    T ~ c3.parse(Array("-v", "--x"))                          ==== runtype[Alt[?]]
    T ~ c3.parse(Array("-u", "--x"))                          ==== runtype[Alt[?]]
    T ~ c3.parse(Array("-s", "--x"))                          ==== runtype[Alt[?]]
    T ~ c3.parse(Array("1", "--", "-s", "--x")).get.args           =**= Array("1", "-s", "--x")
    T ~ c3.parse(Array("-w", "1", "2", "--", "-w", "-3")).get.args =**= Array("2", "-w", "-3")

    val c4 = Cleasy("Big Grand Title", _ => "That's all, folks!")
      -- "salmon" ~ 's' ~ _str.desc("style") % "Request your preferred method of preparation"
      -- 'n'.x ~ _int.desc("count") % "How many?  (May repeat.)"
      -- "eel" ~ (_double.desc("length"), () => 1.0) % "You may specify the size of your eel"
      -- 'x' % "Crossed out"
      -- 'y'.x % "Whyed out, and why shouldn't it be?  X can't have all the fun, now, can it?  Surely not!"
      -- 'z' % "Zonked out"

    val a4 = c4.parse(Array("foo", "-n", "5", "--n=7", "-yn", "3", "bar", "-xyys", "raw", "baz", "--", "-n", "9")).get

    T ~ a4.args              =**= Array("foo", "bar", "baz", "-n", "9")
    T ~ a4.indexedArgs       =**= Array(("foo", 0), ("bar", 6), ("baz", 9), ("-n", 11), ("9", 12))
    T ~ a4("salmon")         ==== Some("raw")
    T ~ a4("n")              ==== List(5, 7, 3)
    T ~ a4("eel")            ==== 1.0
    T ~ a4("x")              ==== Some(())
    T ~ a4("y")              ==== List((), (), ())
    T ~ a4("z")              ==== None
    T ~ a4.found("salmon")   ==== true
    T ~ a4.found("n")        ==== true
    T ~ a4.found("eel")      ==== false
    T ~ a4.found("x")        ==== true
    T ~ a4.found("y")        ==== true
    T ~ a4.found("z")        ==== false
    T ~ a4.require("salmon") ==== ()
    T ~ a4.require("n")      ==== ()
    T ~ a4.require("eel")    ==== runtype[Alt[?]]
    T ~ a4.require("x")      ==== ()
    T ~ a4.require("y")      ==== ()
    T ~ a4.require("z")      ==== runtype[Alt[?]]
    T ~ a4.indexed("salmon") ==== Some(("raw", 7)) --: typed[Option[(String, Int)]]
    T ~ a4.indexed("n")      ==== List((5, 1), (7, 3), (3, 4))
    T ~ a4.indexed("eel")    ==== (1.0, None)
    T ~ a4.indexed("x")      ==== Some(((), 7))
    T ~ a4.indexed("y")      ==== List(((), 4), ((), 7), ((), 7))
    T ~ a4.indexed("z")      ==== None

    val t4 = a4.options

    {
      import kse.basics.labels.*
      T ~ (t4 ~ "salmon")   ==== Some(("raw", 7))
      T ~ (t4 ~ "n")        ==== List((5, 1), (7, 3), (3, 4))
      T ~ (t4 ~ "eel")      ==== (1.0, None)
      T ~ (t4 ~ "x")        ==== Some(((), 7))
      T ~ (t4 ~ "y")        ==== List(((), 4), ((), 7), ((), 7))
      T ~ (t4 ~ "z")        ==== None
    }

    val u4 = c4.userString().linesIterator.toArray

    T ~ u4(0) ==== "Big Grand Title"
    T ~ u4.find(_.startsWith("--salmon")).exists(x => x.contains("preferred") && x.contains("-s style")) ==== true
    T ~ u4.forall(_.length < 80) ==== true
    T ~ u4(End) ==== "That's all, folks!"


  @Test
  def readwriteTest(): Unit =
    val rng = Pcg64(9569856892L)
    val b9999 = rng.arrayB(9999)
    val z9999 = new Array[Byte](9999)
    val z1024 = new Array[Byte](1024)
    def rsbc: SeekableByteChannel = b9999.readChannel()
    def rmac: MultiArrayChannel = b9999.readChannel()
    def zwc: WritableByteChannel = z9999.writeChannel()
    def zws: WritableByteChannel = z1024.writeChannel()

    val sms2s = Send.StreamToStream(391)
    T ~ b9999.input().sendTo(z9999.output())              ==== 9999L  --: typed[Long Or Err]
    T ~ z9999                                             =**= b9999
    z9999.fill(0)
    T ~ b9999.input().sendTo(z9999.output())(using sms2s) ==== 9999L
    T ~ z9999                                             =**= b9999
    T ~ b9999.input().sendTo(z1024.output()).isAlt        ==== true
    T ~ b9999.input().sendTo(z1024.output()).alt.toss     ==== thrown[IOException]

    val sms2c = Send.StreamToChannel(391, allowFullTarget = true)
    z9999.fill(0)
    T ~ b9999.input().sendTo(zwc)                        ==== 9999L
    T ~ z9999                                            =**= b9999
    z9999.fill(0)
    T ~ b9999.input().sendTo(zwc)(using sms2c)           ==== 9999L
    T ~ z9999                                            =**= b9999
    T ~ b9999.input().sendTo(zws).isAlt                  ==== true
    T ~ b9999.input().sendTo(zws).alt.toss               ==== thrown[ErrType.StringErrException]
    T ~ b9999.input().sendTo(zws)(using sms2c)           ==== 1024
    T ~ z1024                                            =**= b9999.take(1024)

    val smc2s = Send.ChannelToStream(391)
    z9999.fill(0)
    T ~ rsbc.sendTo(z9999.output())                      ==== 9999L
    T ~ z9999                                            =**= b9999
    z9999.fill(0)
    T ~ rsbc.sendTo(z9999.output())(using smc2s)         ==== 9999L
    T ~ z9999                                            =**= b9999
    T ~ rsbc.sendTo(z1024.output()).isAlt                ==== true
    T ~ rsbc.sendTo(z1024.output()).alt.toss             ==== thrown[IOException]

    val smc2c = Send.ChannelToChannel(391, allowFullTarget = true)
    z9999.fill(0)
    T ~ rsbc.sendTo(zwc)                                 ==== 9999L
    T ~ z9999                                            =**= b9999
    z9999.fill(0)
    T ~ rsbc.sendTo(zwc)(using smc2c)                    ==== 9999L
    T ~ z9999                                            =**= b9999
    T ~ rsbc.sendTo(zws).isAlt                           ==== true
    T ~ rsbc.sendTo(zws).alt.toss                        ==== thrown[ErrType.StringErrException]
    z1024.fill(0)
    T ~ rsbc.sendTo(zws)(using smc2c)                    ==== 1024
    T ~ z1024                                            =**= b9999.take(1024)

    z9999.fill(0)
    T ~ rmac.sendTo(z9999.output())                      ==== 9999L
    T ~ z9999                                            =**= b9999
    T ~ rmac.sendTo(z1024.output()).isAlt                ==== true
    T ~ rmac.sendTo(z1024.output()).alt.toss             ==== thrown[IOException]

    val smm2c = Send.MultiToChannel(allowFullTarget = true)
    z9999.fill(0)
    T ~ rmac.sendTo(zwc)                                  ==== 9999L
    T ~ z9999                                             =**= b9999
    T ~ rmac.sendTo(zws).isAlt                            ==== true
    T ~ rmac.sendTo(zws).alt.toss                         ==== thrown[ErrType.StringErrException]
    z1024.fill(0)
    T ~ rmac.sendTo(zws)(using smm2c)                     ==== 1024L
    T ~ z1024                                             =**= b9999.take(1024)

    val bits = (Array.fill(27)(rng % 9999) ++ Array(0, 9999)).sorted.sliding(2).map(xs => b9999.copyOfRange(xs(0), xs(1))).toList
    def biter = bits.iterator
    z9999.fill(0)
    T ~ biter.sendTo(z9999.output())                     ==== 9999L
    T ~ z9999                                            =**= b9999
    z9999.fill(0)
    T ~ bits.sendTo(z9999.output())                      ==== 9999L
    T ~ z9999                                            =**= b9999

    val smb2c = Send.IterBytesToChannel(allowFullTarget = true)
    z9999.fill(0)
    T ~ biter.sendTo(zwc)                                ==== 9999L
    T ~ z9999                                            =**= b9999
    z9999.fill(0)
    T ~ bits.sendTo(zwc)                                 ==== 9999L
    T ~ z9999                                            =**= b9999
    T ~ biter.sendTo(zws).isAlt                          ==== true
    T ~ biter.sendTo(zws).alt.toss                       ==== thrown[ErrType.StringErrException]
    z1024.fill(0)
    T ~ biter.sendTo(zws)(using smb2c)                   ==== 1024L
    T ~ z1024                                            =**= b9999.take(1024)

    val smb2m = Send.IterBytesToMulti(allowFullTarget = true)
    z9999.fill(0)
    T ~ biter.sendTo(z9999.writeChannel())               ==== 9999L
    T ~ z9999                                            =**= b9999
    z9999.fill(0)
    T ~ bits.sendTo(z9999.writeChannel())                ==== 9999L
    T ~ z9999                                            =**= b9999
    T ~ biter.sendTo(z1024.writeChannel()).isAlt         ==== true
    T ~ biter.sendTo(z1024.writeChannel()).alt.toss      ==== thrown[ErrType.StringErrException]
    z1024.fill(0)
    T ~ biter.sendTo(z1024.writeChannel())(using smb2m)  ==== 1024L
    T ~ z1024                                            =**= b9999.take(1024)

    val strings = List("salmon", "herring", "cod", "perch")
    def siter = strings.iterator
    z9999.fill(0)
    T ~ siter.sendTo(z9999.output())                     ==== strings.map(_.length + 1).sum
    T ~ z9999.take(strings.map(_.length + 1).sum)        =**= "salmon\nherring\ncod\nperch\n".bytes
    z9999.fill(0)
    T ~ strings.sendTo(z9999.output())                   ==== strings.map(_.length + 1).sum
    T ~ z9999.take(strings.map(_.length + 1).sum)        =**= "salmon\nherring\ncod\nperch\n".bytes

    val smi2c = Send.IterStringToChannel(allowFullTarget = true)
    val z20 = new Array[Byte](20)
    def w20: WritableByteChannel = z20.writeChannel()
    z9999.fill(0)
    T ~ siter.sendTo(zwc)                                ==== strings.map(_.length + 1).sum
    T ~ z9999.take(strings.map(_.length + 1).sum).utf8   ==== "salmon\nherring\ncod\nperch\n"
    z9999.fill(0)
    T ~ strings.sendTo(zwc)                              ==== strings.map(_.length + 1).sum
    T ~ z9999.take(strings.map(_.length + 1).sum).utf8   ==== "salmon\nherring\ncod\nperch\n"
    T ~ siter.sendTo(w20).isAlt                          ==== true
    T ~ siter.sendTo(w20).alt.toss                       ==== thrown[ErrType.StringErrException]
    z20.fill(0)
    T ~ siter.sendTo(w20)(using smi2c)                   ==== 20L
    T ~ z20.utf8                                         ==== "salmon\nherring\ncod\np"

    val smi2m = Send.IterStringToMulti(allowFullTarget = true)
    z9999.fill(0)
    T ~ siter.sendTo(z9999.writeChannel())               ==== strings.map(_.length + 1).sum
    T ~ z9999.take(strings.map(_.length + 1).sum).utf8   ==== "salmon\nherring\ncod\nperch\n"
    z9999.fill(0)
    T ~ strings.sendTo(z9999.writeChannel())             ==== strings.map(_.length + 1).sum
    T ~ z9999.take(strings.map(_.length + 1).sum).utf8   ==== "salmon\nherring\ncod\nperch\n"
    T ~ siter.sendTo(z20.writeChannel()).isAlt           ==== true
    T ~ siter.sendTo(z20.writeChannel()).alt.toss        ==== thrown[ErrType.StringErrException]
    z20.fill(0)
    T ~ siter.sendTo(z20.writeChannel())(using smi2m)    ==== 20L
    T ~ z20.utf8                                         ==== "salmon\nherring\ncod\np"

    val p = "temp/eio".path
    val q = p / "rw"
    val r = p / "wr"
    q.mkdirs()   // Might be race with pathsTest, but it shouldn't matter!
    T ~ List("bass", "salmon", "cod").map(_.bytes).writeTo(r / "fish.bin") ==== runtype[Alt[?]]
    T ~ List("bass", "salmon", "cod").map(_.bytes).writeTo(q / "fish.bin") ==== ()  --: typed[Unit Or Err]
    T ~ (q / "fish.bin").gulp.get                                          =**= "basssalmoncod".bytes
    T ~ List("herring", "sturgeon").map(_.bytes).appendTo(r / "fish.bin")  ==== runtype[Alt[?]]
    T ~ List("herring", "sturgeon").map(_.bytes).appendTo(q / "fish.bin")  ==== ()  --: typed[Unit Or Err]
    T ~ (q / "fish.bin").gulp.get                                          =**= "basssalmoncodherringsturgeon".bytes
    T ~ List("herring", "sturgeon").map(_.bytes).appendTo(q / "fishy.bin") ==== ()  --: typed[Unit Or Err]
    T ~ (q / "fishy.bin").gulp.get                                         =**= "herringsturgeon".bytes
    T ~ List("pike", "halibut").map(_.bytes).createAt(r / "fish.bin")      ==== runtype[Alt[?]]
    T ~ List("pike", "halibut").map(_.bytes).createAt(q / "fish.bin")      ==== runtype[Alt[?]]
    T ~ List("heron", "pelican").map(_.bytes).createAt(q / "birds.bin")    ==== ()  --: typed[Unit Or Err]
    T ~ (q / "birds.bin").gulp.get                                         =**= "heronpelican".bytes
    List("fish", "fishy", "birds").foreach(n => (q/n).extTo("bin").delete())
    T ~ List("bass", "salmon", "cod").writeTo(r / "fish.txt") ==== runtype[Alt[?]]
    T ~ List("bass", "salmon", "cod").writeTo(q / "fish.txt") ==== ()  --: typed[Unit Or Err]
    T ~ (q / "fish.txt").slurp.get.mkString                   ==== "basssalmoncod"
    T ~ List("herring", "sturgeon").appendTo(r / "fish.txt")  ==== runtype[Alt[?]]
    T ~ List("herring", "sturgeon").appendTo(q / "fish.txt")  ==== ()  --: typed[Unit Or Err]
    T ~ (q / "fish.txt").slurp.get.mkString                   ==== "basssalmoncodherringsturgeon"
    T ~ List("herring", "sturgeon").appendTo(q / "fishy.txt") ==== ()  --: typed[Unit Or Err]
    T ~ (q / "fishy.txt").slurp.get.mkString                  ==== "herringsturgeon"
    T ~ List("pike", "halibut").createAt(r / "fish.txt")      ==== runtype[Alt[?]]
    T ~ List("pike", "halibut").createAt(q / "fish.txt")      ==== runtype[Alt[?]]
    T ~ List("heron", "pelican").createAt(q / "birds.txt")    ==== ()  --: typed[Unit Or Err]
    T ~ (q / "birds.txt").slurp.get.mkString                  ==== "heronpelican"
    T ~ (q / "birds.txt").gulp.get                            =**= "heron\npelican\n".bytes

    z9999.fill(0)
    b9999.input().channel.sendTo(zwc)
    T ~ b9999 =**= z9999

    T ~ Send.IterateInputStream("hi".bytes.input()).map(_.utf8).toList.map(_.toString)                                  =**= List("hi")
    T ~ Send.IterateByteChannel("hi".bytes.readChannel()).map(_.utf8).toList.map(_.toString)                            =**= List("hi")
    T ~ Send.IterateInputStream("hi".bytes.input(), reloadUnderfilled = false).map(_.utf8).toList.map(_.toString)       =**= List("hi", "")
    T ~ Send.IterateByteChannel("hi".bytes.readChannel(), reloadUnderfilled = false).map(_.utf8).toList.map(_.toString) =**= List("hi", "")


  @Test
  def xsvTest(): Unit =
              //          1           2             3           4          5
              //01234567890 1 234567890 1 2345678 9 0 12 34567890 1234567890123456
    val text = "hi,it's,me\n\"everyone \"\"agrees\"\"\",\"it's me\",don't you see\n"
    val bint = text.bytes
    val vs = Xsv.Visitor.TableFromString()
    val vz = Xsv.Visitor.TableFromString(true)
    val vb = Xsv.Visitor.TableFromBytes()
    val vd = Xsv.Visitor.TableFromBytes(true)
    def lls(in: Array[Array[String]] Or Err) = in.map(_.map(_.toList).toList)
    T ~ vs.unquoted(text, 3, 7)   ==== ()   --: typed[Unit Or Err]
    T ~ vs.complete(1.u).fn(lls)  ==== List(List("it's"))
    T ~ vs.unquoted(text, 0, 2)   ==== ()
    T ~ vs.newline(1.u)           ==== ()  --: typed[Unit Or Err]
    T ~ vs.quoted(text, 34, 41)   ==== ()
    T ~ vs.endquote()             ==== ()  --: typed[Unit Or Err]
    T ~ vs.newline(2.u)           ==== ()
    T ~ vs.quoted(text, 12, 21)   ==== ()
    T ~ vs.quoted(text, 22, 29)   ==== ()
    T ~ vs.quoted(text, 30, 31)   ==== ()
    T ~ vs.endquote()             ==== ()
    T ~ vs.complete(3.u).fn(lls)  ==== List(List("hi"), List("it's me"), List("""everyone "agrees""""))
    T ~ vs.unquoted(text, 43, 56) ==== ()
    T ~ vs.endquote()             ==== runtype[Alt[?]]
    T ~ vs.clear()                ==== vs
    T ~ vs.quoted(text, 12, 21)   ==== ()
    T ~ vs.newline(1.u)           ==== runtype[Alt[?]]
    T ~ vs.clear()                ==== vs
    T ~ vs.quoted(text, 12, 21)   ==== ()
    T ~ vs.complete(1.u).fn(lls)  ==== runtype[Alt[?]]
    T ~ vs.clear()                ==== vs
    T ~ vs.newline(1.u)           ==== ()
    T ~ vs.unquoted(text, 0, 2)   ==== ()
    T ~ vs.unquoted(text, 8, 10)  ==== ()
    T ~ vs.complete(2.u).fn(lls)  ==== List(List(""), List("hi", "me"))
    T ~ vz.newline(1.u)           ==== ()
    T ~ vz.unquoted(text, 0, 2)   ==== ()
    T ~ vz.unquoted(text, 8, 10)  ==== ()
    T ~ vz.complete(2.u).fn(lls)  ==== runtype[Alt[?]]
    T ~ vb.unquoted(bint, 3, 7)   ==== ()   --: typed[Unit Or Err]
    T ~ vb.complete(1.u).fn(lls)  ==== List(List("it's"))
    T ~ vb.unquoted(bint, 0, 2)   ==== ()
    T ~ vb.newline(1.u)           ==== ()  --: typed[Unit Or Err]
    T ~ vb.quoted(bint, 34, 41)   ==== ()
    T ~ vb.endquote()             ==== ()  --: typed[Unit Or Err]
    T ~ vb.newline(2.u)           ==== ()
    T ~ vb.quoted(bint, 12, 21)   ==== ()
    T ~ vb.quoted(bint, 22, 29)   ==== ()
    T ~ vb.quoted(bint, 30, 31)   ==== ()
    T ~ vb.endquote()             ==== ()
    T ~ vb.complete(3.u).fn(lls)  ==== List(List("hi"), List("it's me"), List("""everyone "agrees""""))
    T ~ vb.unquoted(bint, 43, 56) ==== ()
    T ~ vb.endquote()             ==== runtype[Alt[?]]
    T ~ vb.clear()                ==== vb
    T ~ vb.quoted(bint, 12, 21)   ==== ()
    T ~ vb.newline(1.u)           ==== runtype[Alt[?]]
    T ~ vb.clear()                ==== vb
    T ~ vb.quoted(bint, 12, 21)   ==== ()
    T ~ vb.complete(1.u).fn(lls)  ==== runtype[Alt[?]]
    T ~ vb.clear()                ==== vb
    T ~ vb.newline(1.u)           ==== ()
    T ~ vb.unquoted(bint, 0, 2)   ==== ()
    T ~ vb.unquoted(bint, 8, 10)  ==== ()
    T ~ vb.complete(2.u).fn(lls)  ==== List(List(""), List("hi", "me"))
    T ~ vd.newline(1.u)           ==== ()
    T ~ vd.unquoted(bint, 0, 2)   ==== ()
    T ~ vd.unquoted(bint, 8, 10)  ==== ()
    T ~ vd.complete(2.u).fn(lls)  ==== runtype[Alt[?]]

    val wanted = List(List("hi", "it's", "me"), List("""everyone "agrees"""", "it's me", "don't you see"))
    def asLines(s: String): Array[String] =
      val ss = s.linesIterator.toArray
      if s.length > 0 && s.last.fn(c => c == '\n' || c == '\r') && ss.last.isEmpty then ss :+ "" else ss

    case class Those[A](a: A, b: A, c: A, d: A) {
      override def toString = s"\n1: $a\n2: $b\n3: $c\n4: $d\n"
    }
    def each(s: String) =
      Those(
        Csv.decode(s).fn(lls),
        Csv.decode(s.bytes).fn(lls),
        Csv.decode(asLines(s)).fn(lls),
        Csv.decode(s.bytes.flatMap(b => Array(b))).fn(lls)
      )
    def same(ans: List[List[String]]) =
      Those(ans, ans, ans, ans)
    T ~ each(""        ) ==== same( Nil                          )
    T ~ each("e"       ) ==== same( List(List("e"))              )
    T ~ each(","       ) ==== same( List(List("", ""))           )
    T ~ each("e,"      ) ==== same( List(List("e", ""))          )
    T ~ each(",e"      ) ==== same( List(List("", "e"))          )
    T ~ each("e,e"     ) ==== same( List(List("e", "e"))         )
    T ~ each("\n"      ) ==== same( List(List(""))               )
    T ~ each("\r\n"    ) ==== same( List(List(""))               )
    T ~ each("\r"      ) ==== same( List(List(""))               )
    T ~ each("\n\n"    ) ==== same( List(List(""), List(""))     )
    T ~ each("\r\n\r\n") ==== same( List(List(""), List(""))     )
    T ~ each("\r\r"    ) ==== same( List(List(""), List(""))     )
    T ~ each("e\n"     ) ==== same( List(List("e"))              )
    T ~ each("\ne"     ) ==== same( List(List(""), List("e"))    )
    T ~ each("e\r\n"   ) ==== same( List(List("e"))              )
    T ~ each("\r\ne"   ) ==== same( List(List(""), List("e"))    )
    T ~ each("e\r"     ) ==== same( List(List("e"))              )
    T ~ each("\re"     ) ==== same( List(List(""), List("e"))    )
    T ~ each(",\n"     ) ==== same( List(List("", ""))           )
    T ~ each("\n,"     ) ==== same( List(List(""), List("", "")) )
    T ~ each(",\r\n"   ) ==== same( List(List("", ""))           )
    T ~ each("\r\n,"   ) ==== same( List(List(""), List("", "")) )
    T ~ each(",\r"     ) ==== same( List(List("", ""))           )
    T ~ each("\r,"     ) ==== same( List(List(""), List("", "")) )
    T ~ each("\"\""    ) ==== same( List(List(""))               )
    T ~ each("\"e\""   ) ==== same( List(List("e"))              )
    T ~ each("\",\""   ) ==== same( List(List(","))              )
    T ~ each("\"\n\""  ) ==== same( List(List("\n"))             )
    T ~ each("\"\r\n\"") ==== same( List(List("\n"))             )
    T ~ each("\"\r\""  ) ==== same( List(List("\n"))             )
    T ~ each("\"\","   ) ==== same( List(List("", ""))           )
    T ~ each(",\"\""   ) ==== same( List(List("", ""))           )
    T ~ each("\"\"\n"  ) ==== same( List(List(""))               )
    T ~ each("\"\"\r\n") ==== same( List(List(""))               )
    T ~ each("\"\"\r"  ) ==== same( List(List(""))               )
    T ~ each(text      ) ==== same( wanted                       )
    T ~ Xsv.comma.visit(text, 4, 22, Xsv.Visitor.onString()).fn(lls) ==== List(List("t's", "me"), List("everyone "))
    T ~ Xsv.comma.visit(bint, 4, 22, Xsv.Visitor.onBytes() ).fn(lls) ==== List(List("t's", "me"), List("everyone "))
    T ~ Xsv.comma.visit(text, Iv(4, 22), Xsv.Visitor.onString()).fn(lls) ==== List(List("t's", "me"), List("everyone "))
    T ~ Xsv.comma.visit(bint, Iv(4, 22), Xsv.Visitor.onBytes() ).fn(lls) ==== List(List("t's", "me"), List("everyone "))
    T ~ Xsv.comma.visit(text, 4 to 21, Xsv.Visitor.onString()).fn(lls) ==== List(List("t's", "me"), List("everyone "))
    T ~ Xsv.comma.visit(bint, 4 to 21, Xsv.Visitor.onBytes() ).fn(lls) ==== List(List("t's", "me"), List("everyone "))
    T ~ Xsv.comma.visit(text, 4 to End-(text.length-22), Xsv.Visitor.onString()).fn(lls) ==== List(List("t's", "me"), List("everyone "))
    T ~ Xsv.comma.visit(bint, 4 to End-(bint.length-22), Xsv.Visitor.onBytes() ).fn(lls) ==== List(List("t's", "me"), List("everyone "))
    T ~ Xsv.comma    .decode(" \t,;\n;,\t ").fn(lls) ==== List(List(" \t", ";"), List(";", "\t "))
    T ~ Xsv.tab      .decode(" \t,;\n;,\t ").fn(lls) ==== List(List(" ", ",;"), List(";,", " "))
    T ~ Xsv.space    .decode(" \t,;\n;,\t ").fn(lls) ==== List(List("", "\t,;"), List(";,\t", ""))
    T ~ Xsv.semi     .decode(" \t,;\n;,\t ").fn(lls) ==== List(List(" \t,", ""), List("", ",\t "))
    T ~ Xsv.trimComma.decode(" \t,;\n;,\t ").fn(lls) ==== List(List("", ";"), List(";", ""))
    T ~ Xsv.trimTab  .decode(" \t,;\n;,\t ").fn(lls) ==== List(List("", ",;"), List(";,", ""))
    T ~ Xsv.trimSpace.decode(" \t,;\n;,\t ").fn(lls) ==== List(List("", ",;"), List(";,", ""))
    T ~ Xsv.trimSemi .decode(" \t,;\n;,\t ").fn(lls) ==== List(List(",", ""), List("", ","))
    val texe = text.take(32) + "\n" + text.slice(33, 42) + "\n" + text.drop(43)
    T ~ Xsv.create('e').get.decode(texe).fn(lls) ==== List(List("hi,it's,m", ""), List("everyone \"agrees\""), List("it's me"), List("don't you s", "", ""))

    T ~ Csv.decode(Vector("hi,there\nwe,are\ndone,now".bytes, "".bytes)).get.map(_.toList) =**= Array(List("hi", "there"), List("we", "are"), List("done", "now"))

    T ~ Csv.bomless.decode("\uFEFFHi,there\nevery,one".bytes).get.flatten                       =**= Array("Hi", "there", "every", "one")
    T ~ Csv.bomless.decode(Seq("\uFEFFHi,th", "ere\nev", "ery,one").map(_.bytes)).get.flatten   =**= Array("Hi", "there", "every", "one")
    T ~ Csv.bomless.decode("\uFEFFHi,there\nevery,one".bytes.input()).get.flatten               =**= Array("Hi", "there", "every", "one")
    T ~ Tsv.bomless.decode("\uFEFFHi\tthere\nevery\tone".bytes).get.flatten                     =**= Array("Hi", "there", "every", "one")
    T ~ Tsv.bomless.decode(Seq("\uFEFFHi\tth", "ere\nev", "ery\tone").map(_.bytes)).get.flatten =**= Array("Hi", "there", "every", "one")
    T ~ Tsv.bomless.decode("\uFEFFHi\tthere\nevery\tone".bytes.input()).get.flatten             =**= Array("Hi", "there", "every", "one")
    T ~ Csv.bomless.decode("Hi,there\nevery,one".bytes).get.flatten                             =**= Array("Hi", "there", "every", "one")
    T ~ Csv.bomless.decode(Seq("Hi,th", "ere\nev", "ery,one").map(_.bytes)).get.flatten         =**= Array("Hi", "there", "every", "one")
    T ~ Csv.bomless.decode("Hi,there\nevery,one".bytes.input()).get.flatten                     =**= Array("Hi", "there", "every", "one")
    T ~ Tsv.bomless.decode("Hi\tthere\nevery\tone".bytes).get.flatten                           =**= Array("Hi", "there", "every", "one")
    T ~ Tsv.bomless.decode(Seq("Hi\tth", "ere\nev", "ery\tone").map(_.bytes)).get.flatten       =**= Array("Hi", "there", "every", "one")
    T ~ Tsv.bomless.decode("Hi\tthere\nevery\tone".bytes.input()).get.flatten                   =**= Array("Hi", "there", "every", "one")

    val tab = Array(Array("apple", "pear\n", "pe\"ach"), Array("herring", "", ",", "salmon"))
    T ~ Xsv.encodeTable(tab, 'l').toArray.map(_.utf8)                           =**= Array("\"apple\"l\"pear\n\"l\"pe\"\"ach\"\r\n", "herringll,l\"salmon\"\r\n")
    T ~ Xsv.create('l').get.decode(Xsv.encodeTable(tab, 'l')).get.map(_.toList) =**= tab.map(_.toList)
}
object EioTest {
  import kse.basics.{given, _}
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
