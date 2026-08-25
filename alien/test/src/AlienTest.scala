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
  import kse.basics.{given, _}
  import kse.flow.{given, _}
  import kse.maths.{given, _}
  import kse.alien.Pb

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
    T ~ errText(Pb.decode(Array[Byte](0x0B)){ in => in.next() __ Unit; in.skip() }).contains("wire type 3") ==== true
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
}
