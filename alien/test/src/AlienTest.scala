// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.alien


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._




@RunWith(classOf[JUnit4])
class AlienTest {
  import kse.basics.testutilities.TestUtilities.{given, _}
  import kse.basics.intervals._
  import kse.basics.{given, _}
  import kse.flow.{given, _}
  import kse.maths.{given, _}
  import kse.alien.{Pb, Proto}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  def enc(f: Pb.Out => Unit): Array[Byte] =
    val o = Pb.Out()
    f(o)
    o.result

  def got[A](ask: Ask[A]): A = ask.fold(a => a)(e => throw new AssertionError("unexpected failure: " + e.toString))

  def errText[A](ask: Ask[A]): String = ask.fold(a => "SUCCESS: " + a)(e => e.toString)

  def norm(a: Any): Any = a match
    case arr: Array[?] => arr.toList
    case x => x

  /** Encode one field, decode it back through BOTH substrates, insist they agree, return the value. */
  def one[A](w: Pb.Out => Unit)(r: Pb.In => A): A =
    val bs = enc(w)
    val va = got(Pb.decode(bs){ in =>
      T ~ in.next() ==== true
      val a = r(in)
      T ~ in.next() ==== false
      a
    })
    val vm = got(Pb.decode(Mem of bs){ in =>
      T ~ in.next() ==== true
      val a = r(in)
      T ~ in.next() ==== false
      a
    })
    T ~ norm(va) ==== norm(vm)
    va

  @Test
  def pbVarintTest(): Unit =
    T ~ enc(_.int32Always(1, 0))       =**= Array[Byte](0x08, 0x00)
    T ~ enc(_.int32Always(1, 1))       =**= Array[Byte](0x08, 0x01)
    T ~ enc(_.int32Always(1, 127))     =**= Array[Byte](0x08, 0x7F)
    T ~ enc(_.int32Always(1, 128))     =**= Array[Byte](0x08, -0x80, 0x01)
    T ~ enc(_.int32Always(1, 300))     =**= Array[Byte](0x08, -0x54, 0x02)
    T ~ enc(_.int32Always(1, -1))      =**= Array[Byte](0x08, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0x01)
    T ~ enc(_.int64Always(1, -1L))     =**= Array[Byte](0x08, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0x01)
    T ~ { one(_.int32Always(2, Int.MinValue))(_.int32()) }  ==== Int.MinValue
    T ~ { one(_.int32Always(2, Int.MaxValue))(_.int32()) }  ==== Int.MaxValue
    T ~ { one(_.int64Always(2, Long.MinValue))(_.int64()) } ==== Long.MinValue
    T ~ { one(_.int64Always(2, Long.MaxValue))(_.int64()) } ==== Long.MaxValue
    T ~ { one(_.int64Always(2, 0L))(_.int64()) }            ==== 0L
    T ~ { one(_.uint32Always(3, UInt(-1)))(_.uint32()).signed }          ==== -1
    T ~ { one(_.uint32Always(3, UInt(0x89ABCDEF)))(_.uint32()).signed }  ==== 0x89ABCDEF
    T ~ { one(_.uint64Always(3, ULong(-1L)))(_.uint64()).signed }        ==== -1L
    T ~ enc(_.uint32Always(3, UInt(-1))) =**= Array[Byte](0x18, -1, -1, -1, -1, 0x0F)

  @Test
  def pbZigzagTest(): Unit =
    T ~ enc(_.sint32Always(1, 0))    =**= Array[Byte](0x08, 0x00)
    T ~ enc(_.sint32Always(1, -1))   =**= Array[Byte](0x08, 0x01)
    T ~ enc(_.sint32Always(1, 1))    =**= Array[Byte](0x08, 0x02)
    T ~ enc(_.sint32Always(1, -2))   =**= Array[Byte](0x08, 0x03)
    T ~ enc(_.sint64Always(1, -1L))  =**= Array[Byte](0x08, 0x01)
    T ~ { one(_.sint32Always(2, Int.MinValue))(_.sint32()) }   ==== Int.MinValue
    T ~ { one(_.sint32Always(2, Int.MaxValue))(_.sint32()) }   ==== Int.MaxValue
    T ~ { one(_.sint64Always(2, Long.MinValue))(_.sint64()) }  ==== Long.MinValue
    T ~ { one(_.sint64Always(2, Long.MaxValue))(_.sint64()) }  ==== Long.MaxValue
    T ~ { one(_.sint64Always(2, -987654321012345L))(_.sint64()) } ==== -987654321012345L

  @Test
  def pbFixedTest(): Unit =
    T ~ enc(_.fixed32Always(2, UInt(0x11223344)))  =**= Array[Byte](0x15, 0x44, 0x33, 0x22, 0x11)
    T ~ enc(_.sfixed32Always(2, -2))               =**= Array[Byte](0x15, -2, -1, -1, -1)
    T ~ enc(_.doubleAlways(3, 1.0))                =**= Array[Byte](0x19, 0, 0, 0, 0, 0, 0, -16, 0x3F)
    T ~ enc(_.fixed64Always(3, ULong(0x1122334455667788L))) =**= Array[Byte](0x19, -0x78, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11)
    T ~ { one(_.fixed32Always(1, UInt(-1)))(_.fixed32()).signed }  ==== -1
    T ~ { one(_.sfixed32Always(1, Int.MinValue))(_.sfixed32()) }   ==== Int.MinValue
    T ~ { one(_.fixed64Always(1, ULong(-1L)))(_.fixed64()).signed } ==== -1L
    T ~ { one(_.sfixed64Always(1, Long.MinValue))(_.sfixed64()) }  ==== Long.MinValue
    T ~ { one(_.floatAlways(1, 2.5f))(_.float()) }   ==== 2.5f
    T ~ { one(_.doubleAlways(1, -1e300))(_.double()) } ==== -1e300
    T ~ { one(_.floatAlways(1, Float.NegativeInfinity))(_.float()) } ==== Float.NegativeInfinity
    // NaN payloads and -0.0 travel bit-exact (compare raw bits: Some(NaN)-style == is a trap)
    T ~ { one(_.doubleAlways(1, java.lang.Double.longBitsToDouble(0x7FF8000000000123L)))(_.double()).bitsL } ==== 0x7FF8000000000123L
    T ~ { one(_.doubleAlways(1, -0.0))(_.double()).bitsL } ==== 0x8000000000000000L

  @Test
  def pbDefaultSuppressionTest(): Unit =
    T ~ enc(_.int32(1, 0)).length     ==== 0
    T ~ enc(_.int64(1, 0L)).length    ==== 0
    T ~ enc(_.uint32(1, UInt(0))).length ==== 0
    T ~ enc(_.sint32(1, 0)).length    ==== 0
    T ~ enc(_.bool(1, false)).length  ==== 0
    T ~ enc(_.fixed32(1, UInt(0))).length ==== 0
    T ~ enc(_.float(1, 0.0f)).length  ==== 0
    T ~ enc(_.double(1, 0.0)).length  ==== 0
    T ~ enc(_.string(1, "")).length   ==== 0
    T ~ enc(_.bytes(1, Array.empty[Byte])).length ==== 0
    T ~ enc(_.packedInt32(1, Array.empty[Int])).length ==== 0
    // ...but explicit presence emits even the zero...
    T ~ enc(_.boolAlways(1, false))   =**= Array[Byte](0x08, 0x00)
    T ~ enc(_.stringAlways(1, "")).length ==== 2
    // ...and negative zero is NOT double's default (raw-bits check, not numeric ==)
    T ~ enc(_.double(1, -0.0)).length ==== 9
    T ~ enc(_.float(1, -0.0f)).length ==== 5
    T ~ enc(_.double(1, Double.NaN)).length ==== 9

  @Test
  def pbStringBytesTest(): Unit =
    T ~ enc(_.stringAlways(1, "π")) =**= Array[Byte](0x0A, 0x02, -0x31, -0x80)
    T ~ { one(_.stringAlways(1, "hi there"))(_.string()) }  ==== "hi there"
    T ~ { one(_.stringAlways(1, "ümläüt → 😀"))(_.string()) } ==== "ümläüt → 😀"
    T ~ { one(_.stringAlways(1, ""))(_.string()) }          ==== ""
    T ~ { one(_.bytesAlways(1, Array[Byte](0, -1, 127, -128)))(_.bytes()) } =**= Array[Byte](0, -1, 127, -128)

  @Test
  def pbNestedSkipTest(): Unit =
    val inner = Pb.Out()
    inner.int32(1, 5)
    inner.string(2, "deep")
    val bs = enc: o =>
      o.string(1, "top")
      o.int32Always(77, 42)          // unknown varint
      o.doubleAlways(78, 3.5)        // unknown fixed64
      o.stringAlways(79, "junk")     // unknown length-delimited
      o.fixed32Always(80, UInt(9))   // unknown fixed32
      o.msg(2, inner)
      o.int32(3, 7)
    def read(in: Pb.In): (String, Int, String, Int) =
      var top = ""
      var five = 0
      var deep = ""
      var seven = 0
      while in.next() do in.field match
        case 1 => top = in.string()
        case 2 =>
          val s = in.sub()
          while s.next() do s.field match
            case 1 => five = s.int32()
            case 2 => deep = s.string()
            case _ => s.skip()
        case 3 => seven = in.int32()
        case _ => in.skip()
      (top, five, deep, seven)
    T ~ got(Pb.decode(bs)(read))          ==== ("top", 5, "deep", 7)
    T ~ got(Pb.decode(Mem of bs)(read))   ==== ("top", 5, "deep", 7)
    T ~ got(Pb.decode(bs, 0 to End)(read))        ==== ("top", 5, "deep", 7)
    T ~ got(Pb.decode(bs, Iv(0, bs.length))(read)) ==== ("top", 5, "deep", 7)
    // an empty present message is two bytes and reads as all-defaults
    T ~ enc(_.msg(4, Pb.Out())) =**= Array[Byte](0x22, 0x00)

  @Test
  def pbPackedTest(): Unit =
    T ~ enc(_.packedInt32(4, Array(1, 2, 300))) =**= Array[Byte](0x22, 0x04, 0x01, 0x02, -0x54, 0x02)
    def bothWays[A](w: Pb.Out => Unit)(r: Pb.In => A): A =
      val bs = enc(w)
      val va = got(Pb.decode(bs)(r))
      T ~ norm(va) ==== norm(got(Pb.decode(Mem of bs)(r)))
      va
    def readInts(in: Pb.In): Array[Int] =
      val acc = Pb.IntAcc()
      while in.next() do in.field match
        case 1 => in.int32s(acc)
        case _ => in.skip()
      acc.result
    T ~ bothWays(_.packedInt32(1, Array(1, -1, Int.MinValue, Int.MaxValue)))(readInts) =**= Array(1, -1, Int.MinValue, Int.MaxValue)
    // unpacked spelling of the same field decodes identically
    T ~ bothWays{ o => o.int32Always(1, 1); o.int32Always(1, -1) }(readInts) =**= Array(1, -1)
    // interleaved packed runs and single values concatenate in order
    T ~ bothWays{ o => o.packedInt32(1, Array(1, 2)); o.string(2, "x"); o.int32Always(1, 3) }(readInts) =**= Array(1, 2, 3)
    def readVia[N](get: (Pb.In, N) => Unit)(make: => N)(done: N => Any)(w: Pb.Out => Unit): Any =
      bothWays(w){ in =>
        val acc = make
        while in.next() do in.field match
          case 1 => get(in, acc)
          case _ => in.skip()
        done(acc)
      }
    T ~ readVia[Pb.LongAcc](_.int64s(_))(Pb.LongAcc())(_.result)(_.packedInt64(1, Array(1L, -1L, Long.MinValue))).asInstanceOf[Array[Long]] =**= Array(1L, -1L, Long.MinValue)
    T ~ readVia[Pb.IntAcc](_.sint32s(_))(Pb.IntAcc())(_.result)(_.packedSInt32(1, Array(-1, 1, Int.MinValue))).asInstanceOf[Array[Int]] =**= Array(-1, 1, Int.MinValue)
    T ~ readVia[Pb.LongAcc](_.sint64s(_))(Pb.LongAcc())(_.result)(_.packedSInt64(1, Array(-1L, Long.MaxValue))).asInstanceOf[Array[Long]] =**= Array(-1L, Long.MaxValue)
    T ~ readVia[Pb.IntAcc](_.uint32s(_))(Pb.IntAcc())(_.result)(_.packedUInt32(1, Array(-1, 7))).asInstanceOf[Array[Int]] =**= Array(-1, 7)
    T ~ readVia[Pb.IntAcc](_.fixed32s(_))(Pb.IntAcc())(_.result)(_.packedFixed32(1, Array(-1, 0x11223344))).asInstanceOf[Array[Int]] =**= Array(-1, 0x11223344)
    T ~ readVia[Pb.LongAcc](_.fixed64s(_))(Pb.LongAcc())(_.result)(_.packedFixed64(1, Array(-1L, 3L))).asInstanceOf[Array[Long]] =**= Array(-1L, 3L)
    T ~ readVia[Pb.FloatAcc](_.floats(_))(Pb.FloatAcc())(_.result)(_.packedFloat(1, Array(1.5f, -0.0f))).asInstanceOf[Array[Float]] =**= Array(1.5f, -0.0f)
    T ~ readVia[Pb.DoubleAcc](_.doubles(_))(Pb.DoubleAcc())(_.result)(_.packedDouble(1, Array(2.5, 1e-300))).asInstanceOf[Array[Double]] =**= Array(2.5, 1e-300)
    T ~ readVia[Pb.BoolAcc](_.bools(_))(Pb.BoolAcc())(_.result)(_.packedBool(1, Array(true, false, true))).asInstanceOf[Array[Boolean]] =**= Array(true, false, true)

  @Test
  def pbErrorTest(): Unit =
    T ~ errText(Pb.decode(Array[Byte](0x08)){ in => in.next() __ Unit; in.int32() }).contains("runs off the end") ==== true
    T ~ errText(Pb.decode(Array[Byte](0x0D, 0x01)){ in => in.next() __ Unit; in.float() }).contains("fixed32 runs off") ==== true
    T ~ errText(Pb.decode(Array[Byte](0x0A, 0x05, 'a')){ in => in.next() __ Unit; in.string() }).contains("overruns") ==== true
    T ~ errText(Pb.decode(Array[Byte](0x00)){ in => in.next() }).contains("field number 0") ==== true
    T ~ errText(Pb.decode(Array[Byte](0x0B)){ in => in.next() __ Unit; in.skip() }).contains("never ends") ==== true
    T ~ errText(Pb.decode(Array[Byte](0x08, 0x05)){ in => in.next() __ Unit; in.string() }).contains("expected length-delimited") ==== true
    // an 11-byte varint is malformed even though the value would fit
    T ~ errText(Pb.decode(Array[Byte](0x08, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0x01)){ in => in.next() __ Unit; in.int64() }).contains("longer than 10") ==== true
    // context frames prepend outermost-first
    val deep = Pb.decode(Array[Byte](0x08)){ in => Pb.context("Outer"){ Pb.context("Inner"){ in.next() __ Unit; in.int32() } } }
    T ~ errText(deep).contains("Outer: Inner:") ==== true
    // Mem substrate reports the same failure
    T ~ errText(Pb.decode(Mem of Array[Byte](0x08)){ in => in.next() __ Unit; in.int32() }).contains("runs off the end") ==== true
    // recursion bomb: nesting past MaxDepth halts instead of overflowing the stack
    var bomb = Pb.Out()
    var k = 0
    while k < 600 do
      val o = Pb.Out()
      o.msg(1, bomb)
      bomb = o
      k += 1
    def burrow(in: Pb.In): Int =
      var d = 0
      while in.next() do
        if in.wire == Pb.WLen then d = 1 + burrow(in.sub()) else in.skip()
      d
    T ~ errText(Pb.decode(bomb.result)(burrow)).contains("nesting deeper") ==== true

  @Test
  def pbKeepTest(): Unit =
    val junk = Array[Byte](
      0x08, -0x7F, 0x00,                   // field 1 varint 1, in a deliberately over-long spelling
      0x11, 1, 2, 3, 4, 5, 6, 7, 8,        // field 2 fixed64
      0x1A, 0x02, 0x68, 0x69,              // field 3 length-delimited "hi"
      0x25, 4, 3, 2, 1)                    // field 4 fixed32
    def keepAll(in: Pb.In): List[Pb.Unknown] =
      var ks = List.empty[Pb.Unknown]
      while in.next() do ks = in.keep() :: ks
      ks.reverse
    val kept = got(Pb.decode(junk)(keepAll))
    T ~ kept.map(u => (u.field, u.wire, u.data.length)) ==== List((1, 0, 2), (2, 1, 8), (3, 2, 2), (4, 5, 4))
    T ~ kept.head.data.toList ==== List[Byte](-0x7F, 0x00)   // verbatim: still over-long
    T ~ kept(2).data.toList ==== List[Byte](0x68, 0x69)
    val o = Pb.Out()
    o.unknowns(kept)
    T ~ o.result =**= junk                                   // byte-identical resurrection
    T ~ got(Pb.decode(Mem of junk)(keepAll)).map(u => (u.field, u.wire, u.data.toList)) ==== kept.map(u => (u.field, u.wire, u.data.toList))
    // a stray end-group is refused, and malformed hand-built unknowns refuse to write
    T ~ errText(Pb.decode(Array[Byte](0x0C)){ in => in.next() __ Unit; in.keep() }).contains("no start-group") ==== true
    T ~ errText(nice{ Pb.Out().unknown(1, Pb.WFix64, Array[Byte](1, 2, 3)) }).contains("8 bytes") ==== true
    T ~ errText(nice{ Pb.Out().unknown(1, Pb.WVarint, Array[Byte](-0x80)) }).contains("not one varint") ==== true

  @Test
  def pbGroupTest(): Unit =
    // group field 6 { f1: varint 1; group f2 { f1: varint 7 }; f3: "x" }
    val g = Array[Byte](0x33, 0x08, 0x01, 0x13, 0x08, 0x07, 0x14, 0x1A, 0x01, 0x78, 0x34)
    T ~ got(Pb.decode(g){ in => in.next() __ Unit; in.skip(); in.hasMore }) ==== false
    val kept = got(Pb.decode(g){ in => in.next() __ Unit; in.keep() })
    T ~ (kept.field, kept.wire, kept.data.length) ==== (6, 3, 9)
    val o = Pb.Out()
    o.unknown(kept)
    T ~ o.result =**= g                                   // interior + bracketing tags resurrect exactly
    T ~ got(Pb.decode(Mem of g){ in => in.next() __ Unit; in.keep() }).data.toList ==== kept.data.toList
    // mismatched closer, missing closer, and closer-with-no-opener are all loud
    T ~ errText(Pb.decode(Array[Byte](0x2B, 0x14)){ in => in.next() __ Unit; in.skip() }).contains("end-group for field 2") ==== true
    T ~ errText(Pb.decode(Array[Byte](0x2B, 0x08, 0x01)){ in => in.next() __ Unit; in.skip() }).contains("never ends") ==== true
    T ~ errText(Pb.decode(Array[Byte](0x0C)){ in => in.next() __ Unit; in.skip() }).contains("no start-group") ==== true
    // corrupt hand-built interiors refuse to write
    T ~ errText(nice{ Pb.Out().unknown(5, Pb.WSGroup, Array[Byte](0x08)) }).contains("runs off") ==== true
    // and a group rides through a generated message's read-modify-write untouched
    import kse.test.alien.track.Track
    val gu = got(Track.parse(Track(id = "g").toBytes ++ g))
    T ~ gu.unknown.map(u => (u.field, u.wire)) ==== List((6, 3))
    T ~ got(Track.parse(gu.copy(id = "g2").toBytes)).unknown.map(u => (u.field, u.wire, u.data.toList)) ==== List((6, 3, kept.data.toList))

  @Test
  def pbUtf8Test(): Unit =
    def strOf(bs: Array[Byte]): Ask[String] =
      val o = Pb.Out()
      o.bytesAlways(1, bs)
      Pb.decode(o.result){ in => in.next() __ Unit; in.string() }
    T ~ got(strOf("π→😀".getBytes(java.nio.charset.StandardCharsets.UTF_8))) ==== "π→😀"
    T ~ errText(strOf(Array[Byte](0x61, -0x40))).contains("UTF-8") ==== true          // bare C0 lead
    T ~ errText(strOf(Array[Byte](-0x40, -0x80))).contains("UTF-8") ==== true         // over-long NUL
    T ~ errText(strOf(Array[Byte](-0x13, -0x60, -0x80))).contains("UTF-8") ==== true  // encoded surrogate
    T ~ errText(strOf(Array[Byte](-0x0B, -0x70, -0x80, -0x80))).contains("UTF-8") ==== true  // past U+10FFFF
    T ~ errText(strOf(Array[Byte](-0x1E))).contains("UTF-8") ==== true                // truncated sequence
    val bad = { val o = Pb.Out(); o.bytesAlways(1, Array[Byte](-0x40)); o.result }
    T ~ errText(Pb.decode(Mem of bad){ in => in.next() __ Unit; in.string() }).contains("UTF-8") ==== true

  @Test
  def pbViewTest(): Unit =
    import kse.alien.PbGen
    import kse.test.alien.track.Track
    // wire level: a view aliases the decode buffer; an owned copy does not
    val src = enc(_.bytesAlways(1, Array[Byte](10, 20, 30)))
    val view = got(Pb.decode(src){ in => in.next() __ Unit; in.bytesView() })
    T ~ view.length ==== 3L
    T ~ view(1) ==== (20: Byte)
    val own = Pb.ownedBytes(view)
    src(src.length - 2) = 99
    T ~ view(1) ==== (99: Byte)     // the view follows the buffer
    T ~ own(1) ==== (20: Byte)      // the owned copy stands
    // packed fixed-width views work from both substrates
    val pk = enc(_.packedDouble(1, Array(1.5, -2.5, 3.25)))
    val pv = got(Pb.decode(Mem of pk){ in => in.next() __ Unit; in.packedDoubleView(Mem of Array.empty[Double]) })
    T ~ pv.length ==== 3L
    T ~ (pv(0), pv(1), pv(2)) ==== (1.5, -2.5, 3.25)
    // chunked and unpacked spellings concatenate (degrading to a private copy)
    val chunked = enc{ o => o.packedDouble(1, Array(1.0, 2.0)); o.string(2, "gap"); o.doubleAlways(1, 3.0) }
    def readRun(in: Pb.In): Mem[Double] =
      var w: Mem[Double] = Mem of Array.empty[Double]
      while in.next() do in.field match
        case 1 => w = in.packedDoubleView(w)
        case _ => in.skip()
      w
    val cv = got(Pb.decode(chunked)(readRun))
    T ~ (cv(0), cv(1), cv(2)) ==== (1.0, 2.0, 3.0)
    // ragged packed payloads are loud
    T ~ errText(Pb.decode(Array[Byte](0x0A, 0x05, 1, 2, 3, 4, 5)){ in => in.next() __ Unit; in.packedDoubleView(Mem of Array.empty[Double]) }).contains("ragged") ==== true
    // generated bindings: Mem-typed fields are views, owned severs them
    val t = Track(id = "v", payload = Mem of Array[Byte](7, 8), wave = Mem of Array(0.5, 1.5))
    val bs = t.toBytes
    val u = got(Track.parse(bs))
    T ~ u.payload.length ==== 2L
    T ~ (u.wave(0), u.wave(1)) ==== (0.5, 1.5)
    val uo = u.owned
    java.util.Arrays.fill(bs, 0: Byte)
    T ~ u.payload(0) ==== (0: Byte)    // parsed views die with the buffer...
    T ~ uo.payload(0) ==== (7: Byte)   // ...owned survives it
    T ~ (uo.wave(0), uo.wave(1)) ==== (0.5, 1.5)
    T ~ got(Track.parse(uo.toBytes)).wave(1) ==== 1.5
    // spec merge: a later packed chunk appends to the view field
    val wm = got(Track.parse(Track(wave = Mem of Array(1.0)).toBytes ++ Track(wave = Mem of Array(2.0, 3.0)).toBytes))
    T ~ (wm.wave(0), wm.wave(1), wm.wave(2)) ==== (1.0, 2.0, 3.0)
    // views by Config selection, without touching the .proto
    val viaCfg = got(PbGen.generate(got(Proto.read("""syntax = "proto3"; package v; message M { bytes data = 1; repeated fixed32 raw = 2; }""")),
                                    PbGen.Config(viewFields = Set("v.M.data", "v.M.raw"))))
    T ~ viaCfg.head._2.contains("data: Mem[Byte]") ==== true
    T ~ viaCfg.head._2.contains("raw: Mem[Int]") ==== true
    // ineligible view requests fail generation with an explanation
    T ~ errText(PbGen.generate(got(Proto.read("""syntax = "proto3"; message M { repeated int32 x = 1 [(kse3.view) = true]; }""")))).contains("varint-encoded") ==== true
    T ~ errText(PbGen.generate(got(Proto.read("""syntax = "proto3"; message M { int32 x = 1 [(kse3.view) = true]; }""")))).contains("singular bytes") ==== true
    T ~ errText(PbGen.generate(got(Proto.read("""syntax = "proto3"; message M { repeated bytes b = 1 [(kse3.view) = true]; }""")))).contains("contiguous span") ==== true

  @Test
  def pbMergeTest(): Unit =
    import kse.test.alien.track.{Mood, Pt, Track}
    val t1 = Track(id = "a", pts = Array(Pt(1.0, 2.0)), tags = Map("k" -> 1L, "only1" -> 5L), hops = Array(1),
                   extra = Track.Extra.Anchor(Pt(1.0, 0.0)), meta = Is(Track.Meta(Mood.GRUMPY, Array[Byte](1))), stamp = ULong(1L))
    val t2 = Track(id = "b", pts = Array(Pt(3.0, 4.0)), tags = Map("k" -> 2L), hops = Array(2, 3),
                   extra = Track.Extra.Anchor(Pt(0.0, 2.0)), meta = Is(Track.Meta(blob = Array[Byte](9))))
    val mm = got(Track.parse(t1.toBytes ++ t2.toBytes))
    T ~ mm.id ==== "b"                                                       // later singular scalar wins
    T ~ mm.pts.map(p => (p.x, p.y)).toList ==== List((1.0, 2.0), (3.0, 4.0)) // repeated appends
    T ~ mm.tags ==== Map("k" -> 2L, "only1" -> 5L)                           // maps merge, later key wins
    T ~ mm.hops.toList ==== List(1, 2, 3)
    T ~ mm.stamp.signed ==== 1L                                              // absent later field keeps the earlier value
    T ~ mm.meta.fold(m => (m.mood.number, m.blob.toList))(_ => (-1, Nil)) ==== (16, List[Byte](9))  // messages merge field-by-field
    T ~ mm.extra ==== Track.Extra.Anchor(Pt(1.0, 2.0))                       // same oneof arm merges too
    val t3 = Track(extra = Track.Extra.Note("n"))
    T ~ got(Track.parse(t1.toBytes ++ t3.toBytes)).extra ==== Track.Extra.Note("n")   // a different arm replaces
    // merge-parse over a prior instance is the public face of the same machinery
    val m2 = got(Pb.decode(t2.toBytes)(in => Track.readFrom(in, t1)))
    T ~ m2.id ==== "b"
    T ~ m2.meta.fold(m => (m.mood.number, m.blob.toList))(_ => (-1, Nil)) ==== (16, List[Byte](9))

  @Test
  def pbMemOutTest(): Unit =
    def sample(o: Pb.Out): Unit =
      o.string(1, "slot")
      o.packedDouble(2, Array(1.5, -2.5, 3.75))
      val inner = Pb.Out()
      inner.int64(1, 123456789L)
      o.msg(3, inner)
      o.sint32Always(4, -17)
    val viaArr = enc(sample)
    val backing = new Array[Byte](viaArr.length)
    T ~ got(Pb.encodeInto(Mem of backing)(sample)) ==== viaArr.length.toLong
    T ~ backing =**= viaArr
    // same through a real off-heap segment, decoded straight back out of it
    val off = Mem.alloc[Byte](viaArr.length + 10)
    T ~ got(Pb.encodeInto(off)(sample)) ==== viaArr.length.toLong
    def read(in: Pb.In): (String, Int) =
      var s = ""
      var v = 0
      while in.next() do in.field match
        case 1 => s = in.string()
        case 4 => v = in.sint32()
        case _ => in.skip()
      (s, v)
    T ~ got(Pb.decode(off, 0L, viaArr.length.toLong)(read)) ==== ("slot", -17)
    // a span too small fails as a value, naming the overrun
    T ~ errText(Pb.encodeInto(Mem of new Array[Byte](4))(sample)).contains("overran") ==== true
    // offset spans write where told
    val wide = new Array[Byte](viaArr.length + 8)
    T ~ got(Pb.encodeInto(Mem of wide, 8L, wide.length.toLong)(sample)) ==== viaArr.length.toLong
    T ~ java.util.Arrays.copyOfRange(wide, 8, wide.length) =**= viaArr

  def msgOf(sch: Proto.Schema, fqn: String): Proto.Message =
    sch.message(fqn).fold(m => m)(_ => throw new AssertionError(s"no message $fqn"))
  def enmOf(sch: Proto.Schema, fqn: String): Proto.EnumDef =
    sch.enumDef(fqn).fold(e => e)(_ => throw new AssertionError(s"no enum $fqn"))
  def fieldOf(m: Proto.Message, name: String): Proto.Field =
    m.fields.find(_.name == name).getOrElse(throw new AssertionError(s"no field $name in ${m.name}"))

  def trackProto = TestProtos.track

  @Test
  def protoParseTest(): Unit =
    val sch = got(Proto.read(trackProto, "track.proto"))
    val file = sch.files.head
    T ~ file.pkg ==== "alien.test"
    T ~ file.imports.map(_.path) ==== List("other.proto")
    T ~ file.options.head.value ==== "com.example.suffix"
    val mood = enmOf(sch, "alien.test.Mood")
    T ~ mood.allowAlias ==== true
    T ~ mood.values.map(v => (v.name, v.number)) ==== List(("MOOD_UNSPECIFIED", 0), ("HAPPY", 1), ("GLAD", 1), ("GRUMPY", 16))
    T ~ mood.reserved.hasNumber(6) ==== true
    T ~ mood.reserved.hasNumber(-2) ==== true
    T ~ mood.reserved.hasName("SULLEN") ==== true
    val track = msgOf(sch, "alien.test.Track")
    T ~ fieldOf(track, "id").tpe     ==== Proto.PType.Prim(Proto.Scalar.Str)
    T ~ fieldOf(track, "id").number  ==== 1
    T ~ fieldOf(track, "pts").label  ==== Proto.Label.Rep
    T ~ fieldOf(track, "pts").tpe    ==== Proto.PType.MsgT("alien.test.Pt")
    T ~ fieldOf(track, "tags").tpe   ==== Proto.PType.MapOf(Proto.Scalar.Str, Proto.PType.Prim(Proto.Scalar.SInt64))
    T ~ fieldOf(track, "score").label ==== Proto.Label.Opt
    T ~ fieldOf(track, "score").optioned("deprecated", false) ==== true
    T ~ fieldOf(track, "hops").optioned("packed", true)       ==== false
    T ~ fieldOf(track, "note").oneof   ==== 0
    T ~ fieldOf(track, "anchor").oneof ==== 0
    T ~ fieldOf(track, "anchor").tpe   ==== Proto.PType.MsgT("alien.test.Pt")
    T ~ fieldOf(track, "mood").tpe     ==== Proto.PType.EnumT("alien.test.Mood")
    T ~ fieldOf(track, "meta").tpe     ==== Proto.PType.MsgT("alien.test.Track.Meta")
    T ~ fieldOf(track, "stamp").tpe    ==== Proto.PType.Prim(Proto.Scalar.UInt64)
    T ~ track.oneofs.map(_.name)       ==== List("extra")
    T ~ track.reserved.hasNumber(100)  ==== true
    T ~ track.reserved.hasNumber(536870911) ==== true
    T ~ track.reserved.hasName("old_name")  ==== true
    // Meta.mood resolves outward: alien.test.Track.Meta -> alien.test.Track -> alien.test
    val meta = msgOf(sch, "alien.test.Track.Meta")
    T ~ fieldOf(meta, "mood").tpe ==== Proto.PType.EnumT("alien.test.Mood")
    val svc = file.services.head
    T ~ svc.name ==== "Tracker"
    T ~ svc.rpcs.map(_.name) ==== List("Get", "Watch")
    T ~ svc.rpcs(0).in  ==== Proto.PType.MsgT("alien.test.Pt")
    T ~ svc.rpcs(0).outStream ==== false
    T ~ svc.rpcs(1).outStream ==== true
    T ~ svc.rpcs(1).out ==== Proto.PType.MsgT("alien.test.Track")

  @Test
  def protoResolveTest(): Unit =
    // protoc semantics: the innermost scope owning the FIRST component wins outright;
    // if the rest does not resolve there, that is an error, not a cue to look further out.
    val shadowed = """syntax = "proto3";
      |package a.b;
      |message M { message N { } }
      |message P {
      |  message M { }
      |  M.N x = 1;
      |}
      |""".stripMargin
    T ~ errText(Proto.read(shadowed, "shadow.proto")).contains("is not a type") ==== true
    // ...but the absolute reference threads past the shadow
    val absolute = shadowed.replace("M.N x = 1;", ".a.b.M.N x = 1;")
    val sch = got(Proto.read(absolute, "shadow.proto"))
    T ~ fieldOf(msgOf(sch, "a.b.P"), "x").tpe ==== Proto.PType.MsgT("a.b.M.N")
    // dotted reference from the file root
    val rooted = """syntax = "proto3";
      |message Outer { message Inner { } }
      |message Q { Outer.Inner f = 1; }
      |""".stripMargin
    T ~ fieldOf(msgOf(got(Proto.read(rooted)), "Q"), "f").tpe ==== Proto.PType.MsgT("Outer.Inner")
    // cross-file, cross-package resolution through read(Seq(...))
    val fa = ("a.proto", """syntax = "proto3"; package base; message Shared { int32 v = 1; }""")
    val fb = ("b.proto", """syntax = "proto3"; package app; import "a.proto"; message Use { base.Shared s = 1; }""")
    val sch2 = got(Proto.read(List(fa, fb)))
    T ~ fieldOf(msgOf(sch2, "app.Use"), "s").tpe ==== Proto.PType.MsgT("base.Shared")
    // unresolved types name themselves and where the search stood
    T ~ errText(Proto.read("""syntax = "proto3"; message W { Missing x = 1; }""")).contains("'Missing' is not defined") ==== true
    // colliding declarations across files of one package fail loudly at link
    val dup = List(
      ("one.proto", """syntax = "proto3"; package p; message Twin { }"""),
      ("two.proto", """syntax = "proto3"; package p; message Twin { }""")
    )
    T ~ errText(Proto.read(dup)).contains("already declared") ==== true

  @Test
  def protoRefusalTest(): Unit =
    def refuses(proto: String, key: String): Unit =
      val e = errText(Proto.read(proto, "t.proto"))
      T ~ e.contains(key) ==== true
    refuses("""syntax = "proto2"; message M { }""", "proto3 only")
    refuses("""message M { }""", "proto2")
    refuses("""edition = "2023"; message M { }""", "proto3 only")
    refuses("""syntax = "proto3"; message M { required int32 x = 1; }""", "proto2")
    refuses("""syntax = "proto3"; message M { group G = 1 { } }""", "proto2")
    refuses("""syntax = "proto3"; message M { extensions 100 to 200; }""", "proto2")
    refuses("""syntax = "proto3"; extend M { }""", "proto2")
    refuses("""syntax = "proto3"; message M { int32 x = 19500; }""", "reserved by protobuf")
    refuses("""syntax = "proto3"; message M { int32 x = 1; int64 y = 1; }""", "already used")
    refuses("""syntax = "proto3"; message M { int32 x = 1; int64 x = 2; }""", "already used")
    refuses("""syntax = "proto3"; message M { reserved 3; int32 x = 3; }""", "is reserved")
    refuses("""syntax = "proto3"; message M { reserved "x"; int32 x = 1; }""", "is reserved")
    refuses("""syntax = "proto3"; enum E { FIRST = 1; }""", "must be 0")
    refuses("""syntax = "proto3"; enum E { A = 0; B = 0; }""", "allow_alias")
    refuses("""syntax = "proto3"; message M { map<float, int32> m = 1; }""", "map key")
    refuses("""syntax = "proto3"; message M { oneof o { repeated int32 x = 1; } }""", "cannot be repeated")
    refuses("""syntax = "proto3"; message M { oneof o { map<string, int32> m = 1; } }""", "cannot be a map")
    refuses("""syntax = "proto3"; message M { int32 x = 0; }""", "out of range")
    refuses("""syntax = "proto3"; service S { rpc R (E) returns (E); } enum E { A = 0; }""", "must be messages")
    // and the failure position is named file:line:col
    T ~ errText(Proto.read("syntax = \"proto3\";\nmessage M {\n  int32 x = 0;\n}", "pos.proto")).contains("pos.proto:3:") ==== true

  @Test
  def pbGenGoldenTest(): Unit =
    import kse.alien.PbGen
    val out = got(Or.Ret[List[(String, String)], Err]{ PbGen.generate(Proto.read(trackProto, "track.proto").?, TestProtos.trackConfig).? })
    T ~ out.length ==== 1
    T ~ out.head._1 ==== "TrackProto.scala"
    val src = out.head._2
    T ~ src.contains("package kse.test.alien.track") ==== true
    T ~ src.contains("final case class Track(") ==== true
    T ~ src.contains("opaque type Mood = Int") ==== true
    T ~ src.contains("case MoodArm(value: Mood)") ==== true      // Arm suffix dodged the Mood shadow
    T ~ src.contains("stamp: ULong = ULong(0L)") ==== true
    T ~ src.contains("hops.use()(v => o.int32Always(5, v))") ==== true   // [packed = false] honored
    T ~ src.contains("unknown: List[Pb.Unknown] = Nil") ==== true        // retention is the default
    T ~ src.contains("def readFrom(in: Pb.In, prior: Track): Track") ==== true   // spec merge semantics
    T ~ src.contains("payload: Mem[Byte] = Mem of Array.empty[Byte]") ==== true  // Mem type = zero-copy view
    T ~ src.contains("def owned: Track") ==== true                               // the view-severing copy
    T ~ src.contains("object TrackProtoWalk {") ==== true                        // Config(walker = true)
    T ~ src.contains("def walk(m: Track.Meta)(v: Pb.Visit): Unit =") ==== true  // nested messages walked too

  @Test
  def pbGenWalkTest(): Unit =
    import kse.test.alien.track.{Mood, Pt, Track, TrackProtoWalk}
    val t = Track(
      id = "t1", pts = Array(Pt(1.5, -2.5), Pt(0.0, 3.0)), tags = Map("a" -> -1L, "b" -> 700L), hops = Array(3, -4, 5),
      extra = Track.Extra.Note("hello"), meta = Is(Track.Meta(Mood.GRUMPY, Array[Byte](1, 2, 3))),
      payload = Mem of Array[Byte](9, 9, 9, 9), wave = Mem of Array(1.0, 2.0)
    )
    val seen = scala.collection.mutable.ArrayBuffer.empty[(String, String, Long)]
    TrackProtoWalk.walk(t)(new Pb.Visit {
      def repeated(owner: String, field: String, count: Long): Unit = seen += ((owner, "#" + field, count))
      def bytes(owner: String, field: String, length: Long): Unit = seen += ((owner, field, length))
    })
    // declaration order, oneofs after the plain fields, the view fields by their Mem lengths, the nested message by its path
    T ~ seen.toList ==== List(
      ("Track", "id", 2L), ("Track", "#pts", 2L), ("Track", "#tags", 2L), ("Track", "#hops", 3L),
      ("Track.Meta", "blob", 3L), ("Track", "payload", 4L), ("Track", "#wave", 2L), ("Track", "note", 5L)
    )
    // an all-defaults message still names its empty repeated and scalar fields, so a bound can see zero
    val zeros = scala.collection.mutable.ArrayBuffer.empty[String]
    TrackProtoWalk.walk(Track())(new Pb.Visit {
      def repeated(owner: String, field: String, count: Long): Unit = { zeros += field; T ~ count ==== 0L }
      def bytes(owner: String, field: String, length: Long): Unit = { zeros += field; T ~ length ==== 0L }
    })
    T ~ zeros.toList ==== List("id", "pts", "tags", "hops", "payload", "wave")
    // the boxless optional carriers can be built from an Option
    T ~ Pb.OptInt.from(Some(3)) ==== Pb.OptInt(3)
    T ~ Pb.OptInt.from(None) ==== Pb.OptInt.unit
    T ~ Pb.OptInt(3).isDefined ==== true
    T ~ Pb.OptInt(3).isEmpty ==== false
    T ~ Pb.OptInt.unit.isEmpty ==== true
    T ~ Pb.OptInt.unit.nonEmpty ==== false
    T ~ Pb.OptInt(3).toOption ==== Some(3)
    T ~ Pb.OptInt.unit.toOption ==== None
    T ~ Pb.OptInt(3).map(_ + 1) ==== Pb.OptInt(4)
    T ~ Pb.OptInt.unit.map(_ + 1) ==== Pb.OptInt.unit
    T ~ Pb.OptInt(3).filter(_ > 5) ==== Pb.OptInt.unit
    T ~ Pb.OptInt(3).filter(_ > 1) ==== Pb.OptInt(3)
    T ~ Pb.OptInt(3).contains(3) ==== true
    T ~ Pb.OptInt.unit.contains(3) ==== false
    T ~ Pb.OptInt.unit.orElse(Pb.OptInt(9)) ==== Pb.OptInt(9)
    T ~ Pb.OptInt(3).orElse(Pb.OptInt(9)) ==== Pb.OptInt(3)
    T ~ Pb.OptInt(3).exists(_ == 3) ==== true
    T ~ Pb.OptInt.unit.forall(_ == 3) ==== true
    T ~ Pb.OptInt(-2).map(_ * 2).get ==== -4
    T ~ Pb.OptFloat(0.5f).map(_ * 2) ==== Pb.OptFloat(1.0f)
    T ~ Pb.OptFloat(Float.NaN).contains(Float.NaN) ==== false
    T ~ Pb.OptFloat(Float.NaN).isDefined ==== true
    T ~ Pb.OptBool(true).contains(true) ==== true
    T ~ Pb.OptBool(false).toOption ==== Some(false)
    T ~ Pb.OptBool.unit.nonEmpty ==== false
    T ~ Pb.OptUInt.from(None).isEmpty ==== true
    T ~ Pb.OptBool.from(Some(false)) ==== Pb.OptBool(false)
    T ~ Pb.OptFloat.from(None) ==== Pb.OptFloat.unit

  @Test
  def pbGenKnownGoldenTest(): Unit =
    import kse.alien.PbGen
    val out = got(Or.Ret[List[(String, String)], Err]{ PbGen.generate(Proto.read(TestProtos.knownSources).?, TestProtos.knownConfig).? })
    T ~ out.map(_._1) ==== List("KnownbaseProto.scala", "StageProto.scala")
    T ~ out(0)._2.contains("object KnownbaseProtoKnown {") ==== true
    T ~ out(1)._2.contains("def known(m: Stage.Row): Stage.Row =") ==== true
    T ~ out(1)._2.contains("def unknownArm(m: Stage): Pb.UnknownArm Or Unit =") ==== true
    T ~ out(1)._2.contains("_root_.kse.test.alien.knownbase.KnownbaseProtoKnown.known(_)") ==== true   // cross-file reach
    T ~ out(0)._2.contains("def unknownArm(m: Flat): Pb.UnknownArm Or Unit = Alt.unit") ==== true      // no oneof below
    // known needs unknowns to drop, and the default is off
    T ~ errText(PbGen.generate(got(Proto.read(TestProtos.knownSources)), TestProtos.knownConfig.copy(retainUnknown = false))).contains("retainUnknown") ==== true
    T ~ got(PbGen.generate(got(Proto.read(TestProtos.knownSources)))).exists(_._2.contains("Known {")) ==== false

  /** A varint unknown field, as a later schema's scalar arm would arrive. */
  def unk(n: Int): Pb.Unknown = Pb.Unknown(n, 0, Array[Byte](1))

  @Test
  def pbGenKnownStripTest(): Unit =
    import kse.test.alien.known.{Stage, StageProtoKnown}
    import kse.test.alien.knownbase.{Flat, Shape, Value, KnownbaseProtoKnown}
    import StageProtoKnown.known
    def dirty(u: Int) = Value(Value.Kind.Num(1.5), "m", List(unk(u)))
    val clean = Value(Value.Kind.Num(1.5), "m")
    val s = Stage(
      name = "s",
      input = Is(Shape(Shape.X.Scale(dirty(40)), List(unk(41)))),
      params = Map("gain" -> dirty(42), "bias" -> clean),
      rows = Array(Stage.Row(1, Is(clean)), Stage.Row(2, Is(dirty(43)), List(unk(44)))),
      byId = Map(UInt(7) -> Stage.Row(3, Is(dirty(45)))),
      body = Stage.Body.Single(Stage.Row(4, Is(dirty(46)), List(unk(47)))),
      flat = Is(Flat(5, Array(Flat(6, unknown = List(unk(48)))))),
      unknown = List(unk(49))
    )
    val k = known(s)
    T ~ k.unknown ==== Nil
    T ~ k.input.fold(i => (i.unknown, i.x == Shape.X.Scale(clean)))(_ => (List(unk(0)), false)) ==== (Nil, true)
    T ~ k.params ==== Map("gain" -> clean, "bias" -> clean)
    T ~ k.rows.toList ==== List(Stage.Row(1, Is(clean)), Stage.Row(2, Is(clean)))
    T ~ k.byId ==== Map(UInt(7) -> Stage.Row(3, Is(clean)))
    T ~ k.body ==== Stage.Body.Single(Stage.Row(4, Is(clean)))
    T ~ k.flat.fold(f => f.more.toList.map(_.unknown))(_ => Nil) ==== List(Nil)
    T ~ k.name ==== "s"
    // nothing to drop is no copy at all, at the top and wherever a branch was already clean
    val ok = Stage(name = "ok", input = Is(Shape(Shape.X.Width(3))), params = Map("bias" -> clean), rows = Array(Stage.Row(1)))
    T ~ (known(ok) eq ok) ==== true
    T ~ (KnownbaseProtoKnown.known(clean) eq clean) ==== true
    T ~ (k.rows(0) eq s.rows(0)) ==== true
    T ~ (k.params("bias") eq s.params("bias")) ==== true
    // a map already in key order is kept; one out of order becomes a TreeMap, unsigned keys as unsigned
    val inOrder = Stage(tags = Map(1 -> "x", 2 -> "y"))
    T ~ (known(inOrder) eq inOrder) ==== true
    val outOfOrder = Stage(tags = Map(2 -> "y", 1 -> "x"), byId = Map(UInt(-1) -> Stage.Row(1), UInt(5) -> Stage.Row(2)))
    val ko = known(outOfOrder)
    T ~ (ko eq outOfOrder) ==== false
    T ~ ko.tags.keys.toList ==== List(1, 2)
    T ~ ko.byId.keys.toList.map(_.toLong) ==== List(5L, 4294967295L)
    T ~ (known(ko) eq ko) ==== true
    // and the original is untouched
    T ~ s.rows(1).unknown.map(_.field) ==== List(44)

  @Test
  def pbGenKnownEqualityTest(): Unit =
    import kse.test.alien.known.{Stage, StageProtoKnown}
    import kse.test.alien.knownbase.{Shape, Value}
    import StageProtoKnown.known
    def stage(extra: List[Pb.Unknown], deep: List[Pb.Unknown], gain: Double) = Stage(
      name = "s",
      input = Is(Shape(Shape.X.Scale(Value(Value.Kind.Num(2.0), unknown = deep)))),
      params = Map("gain" -> Value(Value.Kind.Num(gain), unknown = deep)),
      rows = Array(Stage.Row(1, Is(Value(Value.Kind.Text("t"), unknown = deep)))),
      unknown = extra
    )
    // decoded from the wire, as a receiver would see what a later schema sent
    def wire(st: Stage): Stage = got(Stage.parse(st.toBytes))
    val a = wire(stage(Nil, Nil, 0.5))
    val b = wire(stage(List(unk(30)), List(unk(31), unk(32)), 0.5))
    val c = wire(stage(List(unk(30)), List(unk(31)), 0.25))
    T ~ (a.toBytes sameElements b.toBytes) ==== false
    T ~ (known(a).toBytes sameElements known(b).toBytes) ==== true    // unknowns at any depth take no part
    T ~ (known(b).toBytes sameElements known(c).toBytes) ==== false   // but known content still does
    T ~ (known(a).toBytes sameElements a.toBytes) ==== true
    // map entries may arrive in any order; known puts every map, of messages or of scalars, in key order
    val v1 = Value(Value.Kind.Num(1.0))
    val v2 = Value(Value.Kind.Text("two"), unknown = List(unk(33)))
    val fwd = wire(Stage(params = Map("a" -> v1, "b" -> v2), tags = Map(1 -> "x", 2 -> "y", 3 -> "z")))
    val rev = wire(Stage(params = Map("b" -> v2, "a" -> v1), tags = Map(3 -> "z", 1 -> "x", 2 -> "y")))
    T ~ (fwd.toBytes sameElements rev.toBytes) ==== false
    T ~ (known(fwd).toBytes sameElements known(rev).toBytes) ==== true
    val many = (1 to 9).map(i => i -> i.toString)
    T ~ (known(wire(Stage(tags = many.toMap))).toBytes sameElements known(wire(Stage(tags = many.reverse.toMap))).toBytes) ==== true

  @Test
  def pbGenUnknownArmTest(): Unit =
    import kse.test.alien.known.{Stage, StageProtoKnown}
    import kse.test.alien.knownbase.{Flat, Shape, Value, KnownbaseProtoKnown}
    import StageProtoKnown.unknownArm
    def arm(st: Stage): String = unknownArm(got(Stage.parse(st.toBytes))).fold(_.toString)(_ => "none")
    val later = Value(unknown = List(unk(7)))           // a later minor's kind: no arm set, an unknown field
    val known = Value(Value.Kind.Num(1.0))
    // inside a map value, by key, least key first whatever the hash order
    T ~ arm(Stage(params = Map("zeta" -> later, "gain" -> later, "alpha" -> known))) ==== "params[\"gain\"] Value.kind (field 7)"
    // inside a nested optional message, and an arm of that message's own oneof
    T ~ arm(Stage(input = Is(Shape(unknown = List(unk(9)))))) ==== "input Shape.x (field 9)"
    T ~ arm(Stage(input = Is(Shape(Shape.X.Scale(later))))) ==== "input.scale Value.kind (field 7)"
    // repeated by index, unsigned map keys as unsigned, oneof arms by the arm's name
    T ~ arm(Stage(rows = Array(Stage.Row(1, Is(known)), Stage.Row(2, Is(later))))) ==== "rows[1].v Value.kind (field 7)"
    T ~ arm(Stage(byId = Map(UInt(-1) -> Stage.Row(v = Is(later))))) ==== "by_id[4294967295].v Value.kind (field 7)"
    // the least key is least unsigned: a signed order would take 4294967295 (-1) first
    T ~ arm(Stage(byId = Map(UInt(-1) -> Stage.Row(v = Is(later)), UInt(5) -> Stage.Row(v = Is(later))))) ==== "by_id[5].v Value.kind (field 7)"
    T ~ arm(Stage(body = Stage.Body.Single(Stage.Row(v = Is(later))))) ==== "single.v Value.kind (field 7)"
    // in the message itself: the first unset oneof, every unknown number once, in encounter order
    T ~ arm(Stage(unknown = List(unk(20), unk(21), unk(20)))) ==== "Stage.body (fields 20, 21)"
    T ~ arm(Stage(body = Stage.Body.Note("n"), unknown = List(unk(20)))) ==== "Stage.mode (field 20)"
    T ~ unknownArm(Stage(unknown = List(unk(20)))).fold(a => (a.path, a.owner, a.oneof, a.fields))(_ => ("", "", "", Nil)) ==== ("", "Stage", "body", List(20))
    // depth first in declaration order: the input before the params
    T ~ arm(Stage(input = Is(Shape(unknown = List(unk(9)))), params = Map("gain" -> later))) ==== "input Shape.x (field 9)"
    // none when every oneof is set or nothing is unknown
    T ~ arm(Stage(body = Stage.Body.Note("n"), mode = Stage.Mode.Off(true), unknown = List(unk(20)))) ==== "none"
    T ~ arm(Stage(params = Map("gain" -> Value(Value.Kind.Num(1.0), unknown = List(unk(7)))))) ==== "none"
    T ~ arm(Stage(name = "s", params = Map("gain" -> known), rows = Array(Stage.Row(1, Is(known))))) ==== "none"
    T ~ KnownbaseProtoKnown.unknownArm(Flat(1, unknown = List(unk(3)))).isAlt ==== true

  @Test
  def pbGenRoundTripTest(): Unit =
    import kse.test.alien.track.{Mood, Pt, Track}
    val t = Track(
      id = "t1",
      pts = Array(Pt(1.5, -2.5), Pt(0.0, 3.0)),
      tags = Map("a" -> -1L, "b" -> 700L),
      score = Pb.OptFloat(0.5f),
      hops = Array(3, -4, 5),
      extra = Track.Extra.Anchor(Pt(9.0, 9.0)),
      meta = Is(Track.Meta(Mood.GRUMPY, Array[Byte](1, 2, 3))),
      stamp = ULong(-1L)
    )
    val bs = t.toBytes
    def check(u: Track): Unit =
      T ~ u.id ==== "t1"
      T ~ u.pts.map(p => (p.x, p.y)).toList ==== List((1.5, -2.5), (0.0, 3.0))
      T ~ u.tags ==== Map("a" -> -1L, "b" -> 700L)
      T ~ u.score ==== Pb.OptFloat(0.5f)
      T ~ u.hops.toList ==== List(3, -4, 5)
      T ~ u.extra ==== Track.Extra.Anchor(Pt(9.0, 9.0))
      T ~ u.meta.fold(m => (m.mood.number, m.blob.toList))(_ => (-1, Nil)) ==== (16, List[Byte](1, 2, 3))
      T ~ u.stamp.signed ==== -1L
    check(got(Track.parse(bs)))
    check(got(Track.parse(Mem of bs)))
    // an all-default message is zero bytes, and reads back as all defaults
    T ~ Track().toBytes.length ==== 0
    val d = got(Track.parse(Array.empty[Byte]))
    T ~ d.score ==== Pb.OptFloat.unit
    T ~ d.meta ==== Alt.unit
    T ~ d.extra ==== Track.Extra.Unset
    T ~ d.pts.length ==== 0
    // explicit presence: optional zero and oneof zero-value arms still emit and survive
    T ~ Track(score = Pb.OptFloat(0.0f)).toBytes.length ==== 5
    T ~ got(Track.parse(Track(score = Pb.OptFloat(0.0f)).toBytes)).score ==== Pb.OptFloat(0.0f)
    T ~ Track(extra = Track.Extra.Note("")).toBytes.length ==== 2
    T ~ got(Track.parse(Track(extra = Track.Extra.Note("")).toBytes)).extra ==== Track.Extra.Note("")
    T ~ got(Track.parse(Track(extra = Track.Extra.MoodArm(Mood.MOOD_UNSPECIFIED)).toBytes)).extra ==== Track.Extra.MoodArm(Mood(0))
    // open enums: unknown numbers ride through untouched
    val odd = got(Track.parse(Track(meta = Is(Track.Meta(mood = Mood(42)))).toBytes))
    T ~ odd.meta.fold(_.mood.number)(_ => -1) ==== 42
    T ~ Mood(42).name.contains("unknown") ==== true
    T ~ Mood.GLAD.name ==== "HAPPY"   // alias resolves to the first-named value
    // unknown fields are RETAINED in encounter order, and survive read-modify-write
    val junky = Pb.Out()
    junky.string(1, "still me")
    junky.int32Always(99, 12345)
    junky.doubleAlways(98, 2.25)
    junky.stringAlways(97, "junk")
    junky.uint64Always(21, ULong(7L))
    val j = got(Track.parse(junky.result))
    T ~ j.id ==== "still me"
    T ~ j.stamp.signed ==== 7L
    T ~ j.unknown.map(u => (u.field, u.wire)) ==== List((99, 0), (98, 1), (97, 2))
    val j2 = got(Track.parse(j.copy(id = "renamed").toBytes))
    T ~ j2.id ==== "renamed"
    T ~ j2.stamp.signed ==== 7L
    T ~ j2.unknown.map(u => (u.field, u.wire, u.data.toList)) ==== j.unknown.map(u => (u.field, u.wire, u.data.toList))
    // ...but a schema that names every field carries none
    T ~ got(Track.parse(t.toBytes)).unknown ==== Nil
    // and the whole thing round-trips straight out of off-heap memory
    val slot = Mem.alloc[Byte](bs.length + 16)
    T ~ got(Pb.encodeInto(slot)(t.writeTo)) ==== bs.length.toLong
    check(got(Track.parse(slot, 0L, bs.length.toLong)))
}
