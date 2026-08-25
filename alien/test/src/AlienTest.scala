// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.alien


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._


object TestProtos {
  val track = """
    |// Everything the parser should chew through, commentary included.
    |syntax = "proto3";  /* block
    |                       comment */
    |package alien.test;
    |
    |import "other.proto";
    |option java_package = "com.example" ".suffix";
    |
    |enum Mood {
    |  option allow_alias = true;
    |  MOOD_UNSPECIFIED = 0;
    |  HAPPY = 1;
    |  GLAD = 1;       // alias, blessed above
    |  GRUMPY = 0x10;
    |  reserved 5 to 8, -2;
    |  reserved "SULLEN";
    |}
    |
    |message Pt {
    |  double x = 1;
    |  double y = 2;
    |}
    |
    |message Track {
    |  string id = 1;
    |  repeated Pt pts = 2;
    |  map<string, sint64> tags = 3;
    |  optional float score = 4 [deprecated = true];
    |  repeated int32 hops = 5 [packed = false];
    |  oneof extra {
    |    string note = 10;
    |    Pt anchor = 11;
    |    .alien.test.Mood mood = 12;
    |  }
    |  message Meta {
    |    Mood mood = 1;
    |    bytes blob = 2;
    |  }
    |  Meta meta = 20;
    |  uint64 stamp = 21;
    |  reserved 100 to max;
    |  reserved "old_name";
    |}
    |
    |service Tracker {
    |  rpc Get (Pt) returns (Track);
    |  rpc Watch (Pt) returns (stream Track) { option idempotency_level = NO_SIDE_EFFECTS; }
    |}
    |""".stripMargin

  /** The config the checked-in TrackProto.scala was generated with. */
  val trackConfig = kse.alien.PbGen.Config(pkgOf = _ => "kse.test.alien.track")
}


@RunWith(classOf[JUnit4])
class AlienTest {
  import kse.basics.testutilities.TestUtilities.{given, _}
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
    // if the checked-in generated file is reachable from here, it matches regeneration exactly
    val p = java.nio.file.Path.of("alien/test/src/TrackProto.scala")
    if java.nio.file.Files.exists(p) then
      T ~ java.nio.file.Files.readString(p) ==== src

  @Test
  def pbGenRoundTripTest(): Unit =
    import kse.test.alien.track.{Mood, Pt, Track}
    val t = Track(
      id = "t1",
      pts = Array(Pt(1.5, -2.5), Pt(0.0, 3.0)),
      tags = Map("a" -> -1L, "b" -> 700L),
      score = Is(0.5f),
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
      T ~ u.score ==== Is(0.5f)
      T ~ u.hops.toList ==== List(3, -4, 5)
      T ~ u.extra ==== Track.Extra.Anchor(Pt(9.0, 9.0))
      T ~ u.meta.fold(m => (m.mood.number, m.blob.toList))(_ => (-1, Nil)) ==== (16, List[Byte](1, 2, 3))
      T ~ u.stamp.signed ==== -1L
    check(got(Track.parse(bs)))
    check(got(Track.parse(Mem of bs)))
    // an all-default message is zero bytes, and reads back as all defaults
    T ~ Track().toBytes.length ==== 0
    val d = got(Track.parse(Array.empty[Byte]))
    T ~ d.score ==== Alt.unit
    T ~ d.meta ==== Alt.unit
    T ~ d.extra ==== Track.Extra.Unset
    T ~ d.pts.length ==== 0
    // explicit presence: optional zero and oneof zero-value arms still emit and survive
    T ~ Track(score = Is(0.0f)).toBytes.length ==== 5
    T ~ got(Track.parse(Track(score = Is(0.0f)).toBytes)).score ==== Is(0.0f)
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
