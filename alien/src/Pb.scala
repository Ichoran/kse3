// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.alien


import java.lang.{Math as jm}
import java.lang.foreign.MemorySegment

import scala.collection.immutable.{Range => Rg}
import scala.reflect.ClassTag

import kse.basics.{given, _}
import kse.basics.intervals._
import kse.flow.{given, _}
import kse.maths.{given, _}


/** The protocol buffers (proto3) wire format: a low-level, allocation-light encoder (`Pb.Out`)
  * and decoder (`Pb.In`), plus the small runtime that generated message bindings lean on.
  *
  * The encoding itself is tiny: a message is a sequence of (tag, value) pairs, where the tag
  * packs a field number with one of four wire types (varint, fixed64, length-delimited,
  * fixed32), and nesting is by length-delimited embedding.  All fixed-width values are
  * little-endian by spec, read and written through explicitly-ordered accessors so byte order
  * never depends on the host.
  *
  * Both `In` and `Out` come in two substrates: `Array[Byte]` (where socket and gRPC traffic
  * lives) and `Mem[Byte]` (so records can be decoded straight out of — or published straight
  * into — shared memory or a mapped file, with no intermediate copy; this is also what lets
  * message bindings target `Mem.Struct`/`Mem.AoS` layouts rather than case classes).  The
  * structure-level logic is written once, here, against a handful of worker methods; the
  * per-byte hot loops live in each concrete substrate, so they are monomorphic, and the
  * (at most bimorphic) dispatch happens per token, where it is noise.  Pick a substrate via
  * `In.of`/`Out()`/`Out.into` — the surface API is identical.
  *
  * Errors while decoding are reported by throwing a stackless `Pb.Halt` at the point of
  * failure — the same cost class as a `boundary.break` — and only the public rim
  * (`Pb.decode`, `Pb.encodeInto`) speaks `Ask`.  Generated readers wrap themselves in
  * `Pb.context` so a failure surfaces with its full message path, e.g.
  * `HostMsg: ViewState: field 5 at byte 117: varint runs off the end`.
  *
  * Zero-copy has a bright line here.  The `*View` accessors (`bytesView`, `packedDoubleView`
  * and kin) return `Mem` slices that ALIAS the decode source — free, but they must not
  * outlive the buffer (a reused socket buffer, a regranted mapping).  Everything else —
  * `bytes()`, `string()`, the accumulators — is an owned copy, safe forever.  The alias
  * forms exist because they are the only zero-copy protobuf permits: a length-delimited
  * payload and a packed fixed-width run are the format's two statically-shaped spans.
  * The `Pb.owned*` helpers copy a view into fresh storage when independence is needed.
  *
  * Deliberate proto3-isms and limits, chosen loudly rather than silently:
  *   - Unknown fields are the reader's choice: `skip()` drops one, `keep()` captures it as a
  *     `Pb.Unknown` — verbatim bytes, so even a non-canonical varint spelling survives — for
  *     re-emission with `Out.unknowns`.  Generated bindings retain by default, so a
  *     read-modify-write pass cannot silently strip fields a newer schema added.
  *   - Unknown groups (wire types 3/4, proto2 legacy) skip and keep like any other stray
  *     field, interior preserved verbatim; an end-group with no start is an error.
  *   - `string` fields must be valid UTF-8 — invalid sequences are an error, as proto3
  *     demands, not a silent U+FFFD substitution.
  *   - A singular field read with the wrong wire type is an error, not an unknown field
  *     (no legitimate schema evolution changes a field's wire type except packedness,
  *     which repeated readers already accept both ways).
  *   - Repeated scalars decode from either packed or unpacked spelling, per spec.
  *   - Nesting depth is capped at `Pb.MaxDepth` so hostile input cannot overflow the stack
  *     through a recursive message type.
  */
object Pb {

  inline val WVarint = 0
  inline val WFix64 = 1
  inline val WLen = 2
  inline val WSGroup = 3
  inline val WEGroup = 4
  inline val WFix32 = 5

  /** Deepest message nesting `In.sub` will follow. */
  inline val MaxDepth = 512

  /** Coding failure in flight: stackless (construction is cheap, like `boundary.break`),
    * with a mutable message so enclosing readers can prepend context without re-throwing
    * anything new.  Caught at the `decode`/`encodeInto` rim and converted to `Err`; user
    * code should never see one escape.
    */
  final class Halt(var message: String) extends Exception(null, null, false, false) {
    override def getMessage: String = message
  }

  /** Abandon an encode or decode with an explanation. */
  def fail(message: String): Nothing = throw new Halt(message)

  /** One unknown field, captured verbatim by `In.keep` in encounter order: field number,
    * wire type, and the value bytes exactly as read (no tag; for length-delimited values,
    * no length prefix).  `Out.unknowns` writes a run of them back byte-identically.
    */
  final case class Unknown(field: Int, wire: Int, data: Array[Byte])

  /** Strict UTF-8 well-formedness: no over-long forms, no surrogates, nothing past
    * U+10FFFF.  Proto3 requires `string` fields to hold exactly this.
    */
  def validUtf8(bs: Array[Byte], i0: Int, iN: Int): Boolean =
    var k = i0
    var ok = true
    while ok && k < iN do
      val b = bs(k) & 0xFF
      if b < 0x80 then k += 1
      else if b < 0xC2 then ok = false
      else if b < 0xE0 then
        if k + 1 >= iN || (bs(k + 1) & 0xC0) != 0x80 then ok = false
        else k += 2
      else if b < 0xF0 then
        if k + 2 >= iN then ok = false
        else
          val c1 = bs(k + 1) & 0xFF
          if (c1 & 0xC0) != 0x80 || (bs(k + 2) & 0xC0) != 0x80 then ok = false
          else if b == 0xE0 && c1 < 0xA0 then ok = false
          else if b == 0xED && c1 > 0x9F then ok = false
          else k += 3
      else if b < 0xF5 then
        if k + 3 >= iN then ok = false
        else
          val c1 = bs(k + 1) & 0xFF
          if (c1 & 0xC0) != 0x80 || (bs(k + 2) & 0xC0) != 0x80 || (bs(k + 3) & 0xC0) != 0x80 then ok = false
          else if b == 0xF0 && c1 < 0x90 then ok = false
          else if b == 0xF4 && c1 > 0x8F then ok = false
          else k += 4
      else ok = false
    ok

  //////////////////////////////
  /// Wire-size arithmetic   ///
  //////////////////////////////

  /** Encoded length of a varint, 1 to 10 bytes. */
  def varintSize(v: Long): Int =
    if v == 0L then 1 else (70 - java.lang.Long.numberOfLeadingZeros(v)) / 7

  /** Encoded length of a field tag.  The wire-type bits live below the lowest varint
    * group boundary a shifted field number can sit on, so they never change the size.
    */
  def tagSize(field: Int): Int = varintSize(field.toLong << 3)

  /** The byte length `String.getBytes(UTF_8)` will produce, without producing it.
    * Unpaired surrogates count 1, matching the encoder's `?` replacement.
    */
  def utf8Size(s: String): Int =
    var n = 0
    var k = 0
    while k < s.length do
      val c = s.charAt(k)
      if c < 0x80 then n += 1
      else if c < 0x800 then n += 2
      else if java.lang.Character.isHighSurrogate(c) && k + 1 < s.length && java.lang.Character.isLowSurrogate(s.charAt(k + 1)) then
        n += 4
        k += 1
      else if java.lang.Character.isSurrogate(c) then n += 1
      else n += 3
      k += 1
    n

  // Encoded length of one field, each mirroring its same-named `Out` verb exactly --
  // including writing nothing at the proto3 default.  Generated map entries add these up
  // so the entry header can be written before the entry, with no staging buffer.

  def sizeInt32(field: Int, v: Int): Int = if v == 0 then 0 else tagSize(field) + varintSize(v.toLong)
  def sizeInt64(field: Int, v: Long): Int = if v == 0L then 0 else tagSize(field) + varintSize(v)
  def sizeUInt32(field: Int, v: UInt): Int = if v.signed == 0 then 0 else tagSize(field) + varintSize(v.signed & 0xFFFFFFFFL)
  def sizeUInt64(field: Int, v: ULong): Int = if v.signed == 0L then 0 else tagSize(field) + varintSize(v.signed)
  def sizeSInt32(field: Int, v: Int): Int = if v == 0 then 0 else tagSize(field) + varintSize(((v << 1) ^ (v >> 31)).toLong & 0xFFFFFFFFL)
  def sizeSInt64(field: Int, v: Long): Int = if v == 0L then 0 else tagSize(field) + varintSize((v << 1) ^ (v >> 63))
  def sizeBool(field: Int, v: Boolean): Int = if v then tagSize(field) + 1 else 0
  def sizeFixed32(field: Int, v: UInt): Int = if v.signed == 0 then 0 else tagSize(field) + 4
  def sizeSFixed32(field: Int, v: Int): Int = if v == 0 then 0 else tagSize(field) + 4
  def sizeFloat(field: Int, v: Float): Int = if v.bitsI == 0 then 0 else tagSize(field) + 4
  def sizeFixed64(field: Int, v: ULong): Int = if v.signed == 0L then 0 else tagSize(field) + 8
  def sizeSFixed64(field: Int, v: Long): Int = if v == 0L then 0 else tagSize(field) + 8
  def sizeDouble(field: Int, v: Double): Int = if v.bitsL == 0L then 0 else tagSize(field) + 8
  def sizeString(field: Int, s: String): Int =
    if s.isEmpty then 0 else { val u = utf8Size(s); tagSize(field) + varintSize(u.toLong) + u }
  def sizeBytes(field: Int, bs: Array[Byte]): Int =
    if bs.length == 0 then 0 else tagSize(field) + varintSize(bs.length.toLong) + bs.length
  def sizeMsg(field: Int, w: Writable): Int =
    val u = w.sizeOf
    tagSize(field) + varintSize(u.toLong) + u


  /** Copy a view into fresh heap-backed storage, severing any tie to a decode buffer. */
  def ownedBytes(m: Mem[Byte]): Mem[Byte] =
    val a = new Array[Byte](m.length.toInt)
    m.inject(a) __ Unit
    Mem of a

  def ownedInts(m: Mem[Int]): Mem[Int] =
    val a = new Array[Int](m.length.toInt)
    m.inject(a) __ Unit
    Mem of a

  def ownedLongs(m: Mem[Long]): Mem[Long] =
    val a = new Array[Long](m.length.toInt)
    m.inject(a) __ Unit
    Mem of a

  def ownedFloats(m: Mem[Float]): Mem[Float] =
    val a = new Array[Float](m.length.toInt)
    m.inject(a) __ Unit
    Mem of a

  def ownedDoubles(m: Mem[Double]): Mem[Double] =
    val a = new Array[Double](m.length.toInt)
    m.inject(a) __ Unit
    Mem of a

  //////////////////////////////
  /// Boxless optional scalars //
  //////////////////////////////

  // `optional` scalars narrower than eight bytes ride in a wider primitive with absence as
  // an out-of-band bit pattern, so presence never allocates.  Unlike an in-band sentinel,
  // the pattern is unreachable from any present value: present `OptInt` is sign-extended,
  // present `OptUInt` and `OptFloat` are zero-extended (raw bits for float, so every NaN
  // spelling survives verbatim), present `OptBool` is 0 or 1, and the nowhere point is none
  // of these.  Eight-byte scalars have no spare bits and stay `T Or Unit`.

  private inline val AbsentL = 0x6B39A0D54C17E28BL
  private inline val AbsentI = 0x5AD093C6

  /** An `optional int32`/`sint32`/`sfixed32` value, present or absent, without a box. */
  opaque type OptInt = Long
  object OptInt {
    inline def apply(v: Int): OptInt = v.toLong
    val unit: OptInt = AbsentL
    extension (x: OptInt)
      inline def isIs: Boolean = x != AbsentL
      inline def isAlt: Boolean = x == AbsentL
      inline def get: Int = if x == AbsentL then throw new NoSuchElementException("get on absent OptInt") else x.toInt
      inline def getOrElse(inline v: Int): Int = if x == AbsentL then v else x.toInt
      inline def fold[Z](inline f: Int => Z)(inline g: Unit => Z): Z = if x == AbsentL then g(()) else f(x.toInt)
      inline def or: Int Or Unit = if x == AbsentL then Alt.unit else Is(x.toInt)
  }

  /** An `optional uint32`/`fixed32` value, present or absent, without a box. */
  opaque type OptUInt = Long
  object OptUInt {
    inline def apply(v: UInt): OptUInt = v.signed & 0xFFFFFFFFL
    val unit: OptUInt = AbsentL
    extension (x: OptUInt)
      inline def isIs: Boolean = x != AbsentL
      inline def isAlt: Boolean = x == AbsentL
      inline def get: UInt = if x == AbsentL then throw new NoSuchElementException("get on absent OptUInt") else UInt(x.toInt)
      inline def getOrElse(inline v: UInt): UInt = if x == AbsentL then v else UInt(x.toInt)
      inline def fold[Z](inline f: UInt => Z)(inline g: Unit => Z): Z = if x == AbsentL then g(()) else f(UInt(x.toInt))
      inline def or: UInt Or Unit = if x == AbsentL then Alt.unit else Is(UInt(x.toInt))
  }

  /** An `optional float` value, present or absent, without a box; bits are kept raw. */
  opaque type OptFloat = Long
  object OptFloat {
    inline def apply(v: Float): OptFloat = v.bitsI.toLong & 0xFFFFFFFFL
    val unit: OptFloat = AbsentL
    extension (x: OptFloat)
      inline def isIs: Boolean = x != AbsentL
      inline def isAlt: Boolean = x == AbsentL
      inline def get: Float = if x == AbsentL then throw new NoSuchElementException("get on absent OptFloat") else x.toInt.bitsF
      inline def getOrElse(inline v: Float): Float = if x == AbsentL then v else x.toInt.bitsF
      inline def fold[Z](inline f: Float => Z)(inline g: Unit => Z): Z = if x == AbsentL then g(()) else f(x.toInt.bitsF)
      inline def or: Float Or Unit = if x == AbsentL then Alt.unit else Is(x.toInt.bitsF)
  }

  /** An `optional bool` value, present or absent, without a box. */
  opaque type OptBool = Int
  object OptBool {
    inline def apply(v: Boolean): OptBool = if v then 1 else 0
    val unit: OptBool = AbsentI
    extension (x: OptBool)
      inline def isIs: Boolean = x != AbsentI
      inline def isAlt: Boolean = x == AbsentI
      inline def get: Boolean = if x == AbsentI then throw new NoSuchElementException("get on absent OptBool") else x == 1
      inline def getOrElse(inline v: Boolean): Boolean = if x == AbsentI then v else x == 1
      inline def fold[Z](inline f: Boolean => Z)(inline g: Unit => Z): Z = if x == AbsentI then g(()) else f(x == 1)
      inline def or: Boolean Or Unit = if x == AbsentI then Alt.unit else Is(x == 1)
  }


  /** Label a stretch of coding (typically one message reader) so any failure inside
    * carries the path to where it happened.  Free unless a `Halt` actually passes through.
    */
  inline def context[A](name: String)(inline f: => A): A =
    try f
    catch case h: Halt =>
      h.message = name + ": " + h.message
      throw h

  /** Run a whole-message decode, converting a `Halt` (or any other non-fatal exception)
    * into an `Err`.  This rim is the only place decoding failures become values.
    */
  inline def decode[A](bs: Array[Byte])(inline f: In => A): Ask[A] =
    try Is(f(In.of(bs)))
    catch
      case h: Halt => Alt(Err(h.message))
      case e if e.catchable => Alt(Err(e))

  /** Decode from a span of an array; offsets in error messages are absolute in `bs`. */
  inline def decode[A](bs: Array[Byte], i0: Int, iN: Int)(inline f: In => A): Ask[A] =
    try Is(f(In.of(bs, i0, iN)))
    catch
      case h: Halt => Alt(Err(h.message))
      case e if e.catchable => Alt(Err(e))

  /** Decode from the span of `bs` given by a range literal or an `Iv.X` interval. */
  inline def decode[A, R <: Iv.X | Rg](bs: Array[Byte], inline r: R)(inline f: In => A): Ask[A] = Iv.dispatch(r, bs)((i0, iN) => decode(bs, i0, iN)(f))

  /** Decode straight out of memory — no copy of the payload into the heap first. */
  inline def decode[A](m: Mem[Byte])(inline f: In => A): Ask[A] =
    try Is(f(In.of(m)))
    catch
      case h: Halt => Alt(Err(h.message))
      case e if e.catchable => Alt(Err(e))

  /** Decode from a span of memory; offsets in error messages are absolute in `m`. */
  inline def decode[A](m: Mem[Byte], i0: Long, iN: Long)(inline f: In => A): Ask[A] =
    try Is(f(In.of(m, i0, iN)))
    catch
      case h: Halt => Alt(Err(h.message))
      case e if e.catchable => Alt(Err(e))

  /** Encode straight into a span of memory (e.g. a shared-memory slot), answering the number
    * of bytes written.  Fails (as a value) if the encoding does not fit the span.
    */
  inline def encodeInto(m: Mem[Byte], i0: Long, iN: Long)(inline f: Out => Unit): Ask[Long] =
    try
      val o = Out.into(m, i0, iN)
      f(o)
      Is(o.written)
    catch
      case h: Halt => Alt(Err(h.message))
      case e if e.catchable => Alt(Err(e))

  inline def encodeInto(m: Mem[Byte])(inline f: Out => Unit): Ask[Long] = encodeInto(m, 0L, m.length)(f)


  //////////////////////////////
  /// Generated-code runtime ///
  //////////////////////////////

  /** What a generated message does on its own behalf: write itself to an `Out`, and know
    * how many bytes that takes.  `Out.msg` uses `sizeOf` for the length prefix and then
    * writes the body straight into the enclosing sink, so nesting never stages a buffer.
    */
  trait Writable {
    private var sizeMemo: Int = -1

    def writeTo(o: Out): Unit

    /** The serialized byte length of this message's body, memoized on first use (a benign
      * write-once race).  The count is taken by running `writeTo` against a counting sink,
      * so it cannot disagree with what is actually written -- provided the message is not
      * mutated through a held `Array` afterward, which the do-not-mutate-after-handoff
      * convention already forbids.
      */
    final def sizeOf: Int =
      var z = sizeMemo
      if z < 0 then
        val counter = new SizeOut
        writeTo(counter)
        if counter.total > Int.MaxValue then fail(s"message of ${counter.total} bytes exceeds the 2 GB protobuf limit")
        z = counter.total.toInt
        sizeMemo = z
      z

    /** Encode into an array of exactly the right size -- one allocation, no trailing copy. */
    final def toBytes: Array[Byte] =
      val k = sizeOf
      val o = new ArrOut(k)
      writeTo(o)
      if o.length == k && o.buffer.length == k then o.buffer else o.result
  }

  /** What a generated companion does on the message's behalf: fresh and merging reads, and
    * `Ask`-valued parses from either substrate.  Only `default` and the merging `readFrom`
    * are per-message; everything else is defined once, here.
    */
  trait Companion[A <: Writable] {
    def default: A
    def readFrom(in: In, prior: A): A
    final def readFrom(in: In): A = readFrom(in, default)
    final def parse(bs: Array[Byte]): Ask[A] = decode(bs)(readFrom)
    final def parse(bs: Array[Byte], i0: Int, iN: Int): Ask[A] = decode(bs, i0, iN)(readFrom)
    final inline def parse[R <: Iv.X | Rg](bs: Array[Byte], inline r: R): Ask[A] = Iv.dispatch(r, bs)((i0, iN) => parse(bs, i0, iN))
    final def parse(m: Mem[Byte]): Ask[A] = decode(m)(readFrom)
    final def parse(m: Mem[Byte], i0: Long, iN: Long): Ask[A] = decode(m, i0, iN)(readFrom)
    /** Parse a whole stream (read to its end; the stream is not closed).  This is the shape
      * stream-framed transports hand a message in -- e.g. a gRPC marshaller's `parse`.
      */
    final def parse(in: java.io.InputStream): Ask[A] =
      nice{ in.readAllBytes() }.flatMap(bs => decode(bs)(readFrom))
  }

  /** Spec merge for a singular message field: decode the next length-delimited value on top
    * of the prior occupant, or on the companion's default if the field was absent so far.
    */
  def merge[A <: Writable](in: In, prior: A Or Unit, c: Companion[A]): A Or Unit =
    Is(c.readFrom(in.sub(), prior.getOrElse(_ => c.default)))


  //////////////////////////////
  /// Encoding               ///
  //////////////////////////////

  /** Field emitters over some byte sink: one emitter per proto3 field type, with the sink
    * (growable array, or fixed span of `Mem`) supplied by the concrete subclass.
    *
    * Each scalar emitter comes in two forms: the plain one follows proto3 implicit-presence
    * rules (nothing is written when the value is the default), while the `Always` form writes
    * unconditionally, which is what explicit presence demands — `optional` fields, oneof
    * members, and elements of repeated fields are serialized even at their zero values.
    * Enum fields are `int32` at the wire level; write them with `int32`/`int32Always` and
    * the enum's number.
    *
    * Nested messages are always built in their own growable `ArrOut` (the length prefix
    * demands the size up front) and embedded with `msg`, whatever the destination substrate.
    */
  sealed abstract class Out {

    /** Append one varint (also the implementation's hot loop, so it lives with the sink). */
    def varint(v: Long): Unit

    protected def rawByte(v: Int): Unit
    protected def raw32(v: Int): Unit
    protected def raw64(v: Long): Unit
    protected def rawBytes(bs: Array[Byte], off: Int, len: Int): Unit
    protected def rawMem(src: MemorySegment, srcOff: Long, len: Long): Unit

    def tag(field: Int, wire: Int): Unit = varint((field.toLong << 3) | wire)

    // --- varint family ---

    def int32Always(field: Int, v: Int): Unit = { tag(field, WVarint); varint(v.toLong) }
    def int32(field: Int, v: Int): Unit = if v != 0 then int32Always(field, v)

    def int64Always(field: Int, v: Long): Unit = { tag(field, WVarint); varint(v) }
    def int64(field: Int, v: Long): Unit = if v != 0 then int64Always(field, v)

    def uint32Always(field: Int, v: UInt): Unit = { tag(field, WVarint); varint(v.signed & 0xFFFFFFFFL) }
    def uint32(field: Int, v: UInt): Unit = if v.signed != 0 then uint32Always(field, v)

    def uint64Always(field: Int, v: ULong): Unit = { tag(field, WVarint); varint(v.signed) }
    def uint64(field: Int, v: ULong): Unit = if v.signed != 0 then uint64Always(field, v)

    def sint32Always(field: Int, v: Int): Unit = { tag(field, WVarint); varint(((v << 1) ^ (v >> 31)).toLong & 0xFFFFFFFFL) }
    def sint32(field: Int, v: Int): Unit = if v != 0 then sint32Always(field, v)

    def sint64Always(field: Int, v: Long): Unit = { tag(field, WVarint); varint((v << 1) ^ (v >> 63)) }
    def sint64(field: Int, v: Long): Unit = if v != 0 then sint64Always(field, v)

    def boolAlways(field: Int, b: Boolean): Unit = { tag(field, WVarint); varint(if b then 1 else 0) }
    def bool(field: Int, b: Boolean): Unit = if b then boolAlways(field, b)

    // --- fixed-width family (little-endian by spec, whatever the host) ---

    def fixed32Always(field: Int, v: UInt): Unit = { tag(field, WFix32); raw32(v.signed) }
    def fixed32(field: Int, v: UInt): Unit = if v.signed != 0 then fixed32Always(field, v)

    def sfixed32Always(field: Int, v: Int): Unit = { tag(field, WFix32); raw32(v) }
    def sfixed32(field: Int, v: Int): Unit = if v != 0 then sfixed32Always(field, v)

    def floatAlways(field: Int, v: Float): Unit = { tag(field, WFix32); raw32(v.bitsI) }
    def float(field: Int, v: Float): Unit = if v.bitsI != 0 then floatAlways(field, v)

    def fixed64Always(field: Int, v: ULong): Unit = { tag(field, WFix64); raw64(v.signed) }
    def fixed64(field: Int, v: ULong): Unit = if v.signed != 0 then fixed64Always(field, v)

    def sfixed64Always(field: Int, v: Long): Unit = { tag(field, WFix64); raw64(v) }
    def sfixed64(field: Int, v: Long): Unit = if v != 0 then sfixed64Always(field, v)

    def doubleAlways(field: Int, v: Double): Unit = { tag(field, WFix64); raw64(v.bitsL) }
    def double(field: Int, v: Double): Unit = if v.bitsL != 0 then doubleAlways(field, v)

    // --- length-delimited family ---

    def bytesAlways(field: Int, bs: Array[Byte]): Unit =
      tag(field, WLen)
      varint(bs.length)
      rawBytes(bs, 0, bs.length)

    def bytes(field: Int, bs: Array[Byte]): Unit = if bs.length > 0 then bytesAlways(field, bs)

    def stringAlways(field: Int, s: String): Unit = bytesAlways(field, s.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    def string(field: Int, s: String): Unit = if s.nonEmpty then stringAlways(field, s)

    /** Emit a bytes payload straight out of memory — one bulk copy, no heap staging. */
    def bytesAlways(field: Int, m: Mem[Byte]): Unit =
      tag(field, WLen)
      varint(m.length)
      rawMem(m.segment, 0L, m.length)

    def bytes(field: Int, m: Mem[Byte]): Unit = if m.length > 0 then bytesAlways(field, m)

    /** Embed a nested message (or map entry, or oneof message arm).  Presence is the caller's
      * decision — call only when the field is actually set; an empty present message is a
      * legitimate two-byte emission.
      */
    def msg(field: Int, m: ArrOut): Unit =
      tag(field, WLen)
      varint(m.length)
      rawBytes(m.buffer, 0, m.length)

    /** Embed a nested message in one pass: its memoized `sizeOf` supplies the length
      * prefix and the body writes straight into this sink -- no staging buffer at any
      * nesting depth.
      */
    def msg(field: Int, w: Writable): Unit =
      tag(field, WLen)
      varint(w.sizeOf.toLong)
      w.writeTo(this)

    /** Embed an optional nested message; absent writes nothing. */
    def msg(field: Int, w: Writable Or Unit): Unit = w.fold(v => msg(field, v))(_ => ())

    /** The tag-and-length header for an embedded message whose body -- exactly `size`
      * bytes, typically summed with the `Pb.size*` family -- the caller writes next,
      * straight into this sink.  This is how generated map entries avoid staging.
      */
    def msgHeader(field: Int, size: Int): Unit =
      tag(field, WLen)
      varint(size.toLong)

    // Boxless optional-scalar emitters: absent writes nothing, present always writes,
    // zero included -- explicit presence is the whole point of `optional` in proto3.
    // (Inside Pb the Opt types are transparently primitive, so the tests are direct.)
    def int32(field: Int, v: OptInt): Unit = if v != AbsentL then int32Always(field, v.toInt)
    def sint32(field: Int, v: OptInt): Unit = if v != AbsentL then sint32Always(field, v.toInt)
    def sfixed32(field: Int, v: OptInt): Unit = if v != AbsentL then sfixed32Always(field, v.toInt)
    def uint32(field: Int, v: OptUInt): Unit = if v != AbsentL then uint32Always(field, UInt(v.toInt))
    def fixed32(field: Int, v: OptUInt): Unit = if v != AbsentL then fixed32Always(field, UInt(v.toInt))
    def float(field: Int, v: OptFloat): Unit = if v != AbsentL then floatAlways(field, v.toInt.bitsF)
    def bool(field: Int, v: OptBool): Unit = if v != AbsentI then boolAlways(field, v == 1)

    /** Write back one unknown field.  The data must be re-emittable as captured by
      * `In.keep`: a well-formed varint (over-long spellings allowed), exactly 8 or 4 bytes
      * for the fixed widths, anything for length-delimited, a well-formed field sequence
      * for a group interior.  Halts rather than corrupt.
      */
    def unknown(field: Int, wire: Int, data: Array[Byte]): Unit =
      if field <= 0 || field > 536870911 then fail(s"unknown-field number $field out of range")
      wire match
        case WVarint =>
          if data.length < 1 || data.length > 10 || (data(data.length - 1) & 0x80) != 0 then
            fail(s"unknown-field $field: ${data.length} bytes are not one varint")
          tag(field, wire)
          rawBytes(data, 0, data.length)
        case WFix64 =>
          if data.length != 8 then fail(s"unknown-field $field: fixed64 needs 8 bytes, not ${data.length}")
          tag(field, wire)
          rawBytes(data, 0, data.length)
        case WFix32 =>
          if data.length != 4 then fail(s"unknown-field $field: fixed32 needs 4 bytes, not ${data.length}")
          tag(field, wire)
          rawBytes(data, 0, data.length)
        case WLen =>
          tag(field, wire)
          varint(data.length)
          rawBytes(data, 0, data.length)
        case WSGroup =>
          val v = In.of(data)
          while v.next() do v.skip()
          tag(field, WSGroup)
          rawBytes(data, 0, data.length)
          tag(field, WEGroup)
        case w => fail(s"unknown-field $field: wire type $w cannot be written")

    def unknown(u: Unknown): Unit = unknown(u.field, u.wire, u.data)

    /** Write back a run of unknown fields in order — typically last, after the known fields. */
    def unknowns(us: List[Unknown]): Unit = us.foreach(u => unknown(u.field, u.wire, u.data))

    // --- packed repeated (proto3 default for numeric scalars; nothing emitted when empty) ---
    // Varint payloads sum their sizes in a first pass so the length prefix can lead --
    // two walks over the array, zero staging.

    def packedInt32(field: Int, vs: Array[Int]): Unit =
      if vs.length > 0 then
        var sz = 0L
        vs.use()(v => sz += varintSize(v.toLong))
        tag(field, WLen)
        varint(sz)
        vs.use()(v => varint(v.toLong))

    def packedInt64(field: Int, vs: Array[Long]): Unit =
      if vs.length > 0 then
        var sz = 0L
        vs.use()(v => sz += varintSize(v))
        tag(field, WLen)
        varint(sz)
        vs.use()(v => varint(v))

    /** Repeated uint32 travels as an `Array[Int]` of bit patterns (no `ClassTag[UInt]` exists
      * outside kse.maths); each element is zero-extended here.
      */
    def packedUInt32(field: Int, vs: Array[Int]): Unit =
      if vs.length > 0 then
        var sz = 0L
        vs.use()(v => sz += varintSize(v.toLong & 0xFFFFFFFFL))
        tag(field, WLen)
        varint(sz)
        vs.use()(v => varint(v.toLong & 0xFFFFFFFFL))

    /** Repeated uint64 as an `Array[Long]` of bit patterns; the encoding is number-identical. */
    def packedUInt64(field: Int, vs: Array[Long]): Unit = packedInt64(field, vs)

    def packedSInt32(field: Int, vs: Array[Int]): Unit =
      if vs.length > 0 then
        var sz = 0L
        vs.use()(v => sz += varintSize(((v << 1) ^ (v >> 31)).toLong & 0xFFFFFFFFL))
        tag(field, WLen)
        varint(sz)
        vs.use()(v => varint(((v << 1) ^ (v >> 31)).toLong & 0xFFFFFFFFL))

    def packedSInt64(field: Int, vs: Array[Long]): Unit =
      if vs.length > 0 then
        var sz = 0L
        vs.use()(v => sz += varintSize((v << 1) ^ (v >> 63)))
        tag(field, WLen)
        varint(sz)
        vs.use()(v => varint((v << 1) ^ (v >> 63)))

    def packedBool(field: Int, vs: Array[Boolean]): Unit =
      if vs.length > 0 then
        tag(field, WLen)
        varint(vs.length)
        vs.use()(b => rawByte(if b then 1 else 0))

    def packedFixed32(field: Int, vs: Array[Int]): Unit =
      if vs.length > 0 then
        tag(field, WLen)
        varint(4L * vs.length)
        vs.use()(v => raw32(v))

    def packedFixed64(field: Int, vs: Array[Long]): Unit =
      if vs.length > 0 then
        tag(field, WLen)
        varint(8L * vs.length)
        vs.use()(v => raw64(v))

    def packedFloat(field: Int, vs: Array[Float]): Unit =
      if vs.length > 0 then
        tag(field, WLen)
        varint(4L * vs.length)
        vs.use()(v => raw32(v.bitsI))

    def packedDouble(field: Int, vs: Array[Double]): Unit =
      if vs.length > 0 then
        tag(field, WLen)
        varint(8L * vs.length)
        vs.use()(v => raw64(v.bitsL))

    // Packed fixed-width runs straight out of memory: the payload is bulk-copied verbatim,
    // which is exactly right for views taken by the packed*View readers (wire is LE, and
    // kse3 assumes LE hosts throughout).

    def packedFixed32(field: Int, vs: Mem[Int]): Unit =
      if vs.length > 0 then
        tag(field, WLen)
        varint(4L * vs.length)
        rawMem(vs.segment, 0L, 4L * vs.length)

    def packedFixed64(field: Int, vs: Mem[Long]): Unit =
      if vs.length > 0 then
        tag(field, WLen)
        varint(8L * vs.length)
        rawMem(vs.segment, 0L, 8L * vs.length)

    def packedFloat(field: Int, vs: Mem[Float]): Unit =
      if vs.length > 0 then
        tag(field, WLen)
        varint(4L * vs.length)
        rawMem(vs.segment, 0L, 4L * vs.length)

    def packedDouble(field: Int, vs: Mem[Double]): Unit =
      if vs.length > 0 then
        tag(field, WLen)
        varint(8L * vs.length)
        rawMem(vs.segment, 0L, 8L * vs.length)
  }

  object Out {
    /** A growable array-backed sink — the general-traffic substrate. */
    def apply(): ArrOut = new ArrOut(64)

    /** An array-backed sink pre-sized for a known encoding, e.g. from `sizeOf`. */
    def apply(capacity: Int): ArrOut = new ArrOut(capacity)

    /** A sink over a span of memory, e.g. a shared-memory slot; halts if the encoding outgrows it. */
    def into(m: Mem[Byte], i0: Long, iN: Long): MemOut = new MemOut(m, i0, iN)
    def into(m: Mem[Byte]): MemOut = new MemOut(m, 0L, m.length)
  }

  /** Growable array-backed `Out`.  `result` copies, so the buffer may keep growing after;
    * `clear()` permits reuse.
    */
  final class ArrOut private[alien] (capacity: Int) extends Out {
    private var buf = new Array[Byte](jm.max(capacity, 1))
    private var n = 0

    private def ensure(k: Int): Unit =
      if n + k > buf.length then buf = java.util.Arrays.copyOf(buf, jm.max(buf.length * 2, n + k))

    def varint(v: Long): Unit =
      ensure(10)
      var x = v
      while (x & ~0x7FL) != 0 do
        buf(n) = ((x & 0x7F) | 0x80).toByte
        n += 1
        x = x >>> 7
      buf(n) = (x & 0x7F).toByte
      n += 1

    protected def rawByte(v: Int): Unit =
      ensure(1)
      buf(n) = v.toByte
      n += 1

    protected def raw32(v: Int): Unit =
      ensure(4)
      buf(n    ) = ( v         & 0xFF).toByte
      buf(n + 1) = ((v >>>  8) & 0xFF).toByte
      buf(n + 2) = ((v >>> 16) & 0xFF).toByte
      buf(n + 3) = ((v >>> 24) & 0xFF).toByte
      n += 4

    protected def raw64(v: Long): Unit =
      ensure(8)
      var j = 0
      while j < 8 do
        buf(n + j) = ((v >>> (8 * j)) & 0xFF).toByte
        j += 1
      n += 8

    protected def rawBytes(bs: Array[Byte], off: Int, len: Int): Unit =
      ensure(len)
      System.arraycopy(bs, off, buf, n, len)
      n += len

    protected def rawMem(src: MemorySegment, srcOff: Long, len: Long): Unit =
      ensure(len.toInt)
      MemorySegment.copy(src, srcOff, MemorySegment.ofArray(buf), n.toLong, len)
      n += len.toInt

    def length: Int = n

    private[alien] def buffer: Array[Byte] = buf

    def result: Array[Byte] = java.util.Arrays.copyOf(buf, n)

    def clear(): this.type = { n = 0; this }
  }

  /** Fixed-capacity `Out` over a span of `Mem` — encoding lands directly in (shared or
    * mapped) memory.  Outgrowing the span halts; `Pb.encodeInto` turns that into an `Err`.
    */
  final class MemOut private[alien] (m: Mem[Byte], i0: Long, iN: Long) extends Out {
    private var i = i0

    private def ensure(k: Int): Unit =
      if i + k > iN then fail(s"encoding overran the ${iN - i0} byte destination at offset ${i - i0}")

    def varint(v: Long): Unit =
      var x = v
      while (x & ~0x7FL) != 0 do
        ensure(1)
        m.setB(i, ((x & 0x7F) | 0x80).toByte)
        i += 1
        x = x >>> 7
      ensure(1)
      m.setB(i, (x & 0x7F).toByte)
      i += 1

    protected def rawByte(v: Int): Unit =
      ensure(1)
      m.setB(i, v.toByte)
      i += 1

    protected def raw32(v: Int): Unit =
      ensure(4)
      m.setI_le(i, v)
      i += 4

    protected def raw64(v: Long): Unit =
      ensure(8)
      m.setL_le(i, v)
      i += 8

    protected def rawBytes(bs: Array[Byte], off: Int, len: Int): Unit =
      ensure(len)
      MemorySegment.copy(MemorySegment.ofArray(bs), off.toLong, m.segment, i, len.toLong)
      i += len

    protected def rawMem(src: MemorySegment, srcOff: Long, len: Long): Unit =
      if i + len > iN then fail(s"encoding overran the ${iN - i0} byte destination at offset ${i - i0}")
      MemorySegment.copy(src, srcOff, m.segment, i, len)
      i += len

    /** Bytes written so far. */
    def written: Long = i - i0
  }

  /** An `Out` that only counts: run a `writeTo` against it and `total` is the byte length
    * the same code will produce for real.  Sizing and writing thus share one code path and
    * cannot disagree.  Nested messages contribute their memoized `sizeOf` instead of being
    * walked again, and strings are measured without materializing their bytes.
    */
  final class SizeOut private[alien] () extends Out {
    private var n = 0L

    def total: Long = n

    def varint(v: Long): Unit = n += varintSize(v)
    protected def rawByte(v: Int): Unit = n += 1
    protected def raw32(v: Int): Unit = n += 4
    protected def raw64(v: Long): Unit = n += 8
    protected def rawBytes(bs: Array[Byte], off: Int, len: Int): Unit = n += len
    protected def rawMem(src: MemorySegment, srcOff: Long, len: Long): Unit = n += len

    override def msg(field: Int, w: Writable): Unit =
      tag(field, WLen)
      val z = w.sizeOf
      varint(z.toLong)
      n += z

    override def stringAlways(field: Int, s: String): Unit =
      val u = utf8Size(s)
      tag(field, WLen)
      varint(u.toLong)
      n += u
  }


  //////////////////////////////
  /// Decoding               ///
  //////////////////////////////

  /** Field-by-field reader over one message's span of bytes (in an array or in `Mem`).
    * `next()` advances to the next (field, wire) pair; the typed getters then read the value,
    * halting with an informative message if the wire type cannot supply that field type.
    * `sub()` descends into a length-delimited payload without copying.  Repeated-scalar
    * readers (`int32s` and kin) accept both packed and unpacked spellings, accumulating so
    * interleaved chunks concatenate in order, as the spec requires.
    *
    * All failure is via `Halt`; run whole decodes through `Pb.decode` to get an `Ask`.
    * An `In` is mutable single-pass state — do not share or rewind one.
    */
  sealed abstract class In(protected var i: Long, val end: Long, protected val depth: Int) {
    var field: Int = 0
    var wire: Int = 0

    /** Read one varint (also the implementation's hot loop, so it lives with the source). */
    def readVarint(): Long

    protected def raw32Work(): Int
    protected def raw64Work(): Long
    protected def stringWork(i0: Long, iN: Long): String
    protected def bytesWork(i0: Long, iN: Long): Array[Byte]
    protected def subWork(i0: Long, iN: Long): In
    protected def viewWork(i0: Long, iN: Long): Mem[Byte]

    def hasMore: Boolean = i < end

    def next(): Boolean =
      if i >= end then false
      else
        val t = readVarint()
        field = (t >>> 3).toInt
        wire = (t & 7).toInt
        if field == 0 then fail(s"tag with field number 0 at byte ${i - 1}")
        if (t >>> 3) > 536870911L then fail(s"tag with field number ${t >>> 3} out of range at byte ${i - 1}")
        true

    private def wantWire(w: Int, what: String): Unit =
      if wire != w then fail(s"field $field at byte $i: expected $what, got wire type $wire")

    // --- singular getters, named for the proto3 type they realize ---

    def int32(): Int = { wantWire(WVarint, "varint"); readVarint().toInt }
    def int64(): Long = { wantWire(WVarint, "varint"); readVarint() }
    def uint32(): UInt = { wantWire(WVarint, "varint"); UInt(readVarint().toInt) }
    def uint64(): ULong = { wantWire(WVarint, "varint"); ULong(readVarint()) }
    def sint32(): Int = { wantWire(WVarint, "varint"); val u = readVarint(); ((u >>> 1) ^ -(u & 1)).toInt }
    def sint64(): Long = { wantWire(WVarint, "varint"); val u = readVarint(); (u >>> 1) ^ -(u & 1) }
    def bool(): Boolean = { wantWire(WVarint, "varint"); readVarint() != 0 }

    def fixed32(): UInt = { wantWire(WFix32, "fixed32"); UInt(raw32Work()) }
    def sfixed32(): Int = { wantWire(WFix32, "fixed32"); raw32Work() }
    def float(): Float = { wantWire(WFix32, "fixed32"); raw32Work().bitsF }
    def fixed64(): ULong = { wantWire(WFix64, "fixed64"); ULong(raw64Work()) }
    def sfixed64(): Long = { wantWire(WFix64, "fixed64"); raw64Work() }
    def double(): Double = { wantWire(WFix64, "fixed64"); raw64Work().bitsD }

    private def lenSpan(): Long =
      wantWire(WLen, "length-delimited")
      val len = readVarint()
      if len < 0 || len > end - i then fail(s"field $field at byte $i: length $len overruns the message")
      val a = i
      i += len
      a

    private def lenSpanInt(): Long =
      val a = lenSpan()
      if i - a > Int.MaxValue - 8 then fail(s"field $field at byte $a: ${i - a} byte payload is too big for the heap")
      a

    def string(): String = { val a = lenSpanInt(); stringWork(a, i) }

    def bytes(): Array[Byte] = { val a = lenSpanInt(); bytesWork(a, i) }

    // --- zero-copy views: these ALIAS the decode source and must not outlive it ---

    /** Zero-copy view of a bytes payload.  Aliases the decode source: if the buffer is
      * reused or unmapped, the view goes with it — `bytes()` (or `Pb.ownedBytes`) is the
      * independent form.
      */
    def bytesView(): Mem[Byte] = { val a = lenSpanInt(); viewWork(a, i) }

    // Packed fixed-width runs are the wire format's other statically-shaped span, so they
    // too can be viewed in place (as little-endian data, which kse3 assumes throughout).
    // Each reader accepts the unpacked spelling as well, and a second chunk of the same
    // field concatenates by degrading to a private copy — pass the field's current value
    // and the right thing happens, including spec merge behavior (chunks append).

    private def spliceWork(priorSeg: MemorySegment, priorBytes: Long, a: Long, b: Long): Mem[Byte] =
      if priorBytes + (b - a) > Int.MaxValue - 8 then fail(s"field $field at byte $a: packed payload too big for the heap")
      val arr = new Array[Byte]((priorBytes + (b - a)).toInt)
      MemorySegment.copy(priorSeg, 0L, MemorySegment.ofArray(arr), 0L, priorBytes)
      val chunk = bytesWork(a, b)
      System.arraycopy(chunk, 0, arr, priorBytes.toInt, chunk.length)
      Mem of arr

    private def fixedSpan(width: Int, natural: Int): Long =
      if wire == natural then
        if i + width > end then fail(s"field $field runs off the end at byte $i")
        val a = i
        i += width
        a
      else
        val a = lenSpanInt()
        if (i - a) % width != 0 then fail(s"field $field at byte $a: ${i - a} byte packed payload is ragged (width $width)")
        a

    def packedFixed32View(prior: Mem[Int]): Mem[Int] =
      val a = fixedSpan(4, WFix32)
      if prior.length == 0 then viewWork(a, i).as[Int]
      else spliceWork(prior.segment, 4L * prior.length, a, i).as[Int]

    def packedFixed64View(prior: Mem[Long]): Mem[Long] =
      val a = fixedSpan(8, WFix64)
      if prior.length == 0 then viewWork(a, i).as[Long]
      else spliceWork(prior.segment, 8L * prior.length, a, i).as[Long]

    def packedFloatView(prior: Mem[Float]): Mem[Float] =
      val a = fixedSpan(4, WFix32)
      if prior.length == 0 then viewWork(a, i).as[Float]
      else spliceWork(prior.segment, 4L * prior.length, a, i).as[Float]

    def packedDoubleView(prior: Mem[Double]): Mem[Double] =
      val a = fixedSpan(8, WFix64)
      if prior.length == 0 then viewWork(a, i).as[Double]
      else spliceWork(prior.segment, 8L * prior.length, a, i).as[Double]

    /** Descend into a length-delimited payload (nested message or packed run) without copying. */
    def sub(): In =
      if depth >= MaxDepth then fail(s"field $field at byte $i: nesting deeper than $MaxDepth")
      val a = lenSpan()
      subWork(a, i)

    // --- repeated scalars: packed and unpacked spellings both accepted, per spec ---

    def int32s(acc: IntAcc): Unit =
      if wire == WVarint then acc += readVarint().toInt
      else { val in = sub(); while in.hasMore do acc += in.readVarint().toInt }

    def int64s(acc: LongAcc): Unit =
      if wire == WVarint then acc += readVarint()
      else { val in = sub(); while in.hasMore do acc += in.readVarint() }

    def uint32s(acc: IntAcc): Unit = int32s(acc)
    def uint64s(acc: LongAcc): Unit = int64s(acc)

    def sint32s(acc: IntAcc): Unit =
      if wire == WVarint then acc += sint32()
      else { val in = sub(); while in.hasMore do { val u = in.readVarint(); acc += ((u >>> 1) ^ -(u & 1)).toInt } }

    def sint64s(acc: LongAcc): Unit =
      if wire == WVarint then acc += sint64()
      else { val in = sub(); while in.hasMore do { val u = in.readVarint(); acc += (u >>> 1) ^ -(u & 1) } }

    def bools(acc: BoolAcc): Unit =
      if wire == WVarint then acc += readVarint() != 0
      else { val in = sub(); while in.hasMore do acc += in.readVarint() != 0 }

    def fixed32s(acc: IntAcc): Unit =
      if wire == WFix32 then acc += raw32Work()
      else { val in = sub(); while in.hasMore do acc += in.raw32Work() }

    def fixed64s(acc: LongAcc): Unit =
      if wire == WFix64 then acc += raw64Work()
      else { val in = sub(); while in.hasMore do acc += in.raw64Work() }

    def floats(acc: FloatAcc): Unit =
      if wire == WFix32 then acc += raw32Work().bitsF
      else { val in = sub(); while in.hasMore do acc += in.raw32Work().bitsF }

    def doubles(acc: DoubleAcc): Unit =
      if wire == WFix64 then acc += raw64Work().bitsD
      else { val in = sub(); while in.hasMore do acc += in.raw64Work().bitsD }

    /** Skip a whole group, itself skipping anything inside (nested groups included), until
      * the matching end-group tag; answers the offset where that end tag began, so `keep`
      * can capture the interior verbatim.
      */
    private def skipGroup(open: Int, d: Int): Long =
      if d >= MaxDepth then fail(s"group $open at byte $i: nesting deeper than $MaxDepth")
      var endAt = -1L
      while endAt < 0 do
        if i >= end then fail(s"group $open never ends")
        val tagAt = i
        val t = readVarint()
        val f = (t >>> 3).toInt
        val w = (t & 7).toInt
        if (t >>> 3) == 0 || (t >>> 3) > 536870911L then fail(s"tag with field number ${t >>> 3} out of range at byte $tagAt")
        w match
          case WEGroup => if f == open then endAt = tagAt else fail(s"group $open at byte $tagAt: closed by end-group for field $f")
          case WSGroup => skipGroup(f, d + 1) __ Unit
          case WVarint => readVarint() __ Unit
          case WFix64  => if i + 8 > end then fail(s"group $open runs off the end at byte $i") else i += 8
          case WLen =>
            val len = readVarint()
            if len < 0 || len > end - i then fail(s"field $f at byte $i: length $len overruns the message")
            i += len
          case WFix32  => if i + 4 > end then fail(s"group $open runs off the end at byte $i") else i += 4
          case other   => fail(s"field $f at byte $tagAt: wire type $other does not exist")
      endAt

    def skip(): Unit = wire match
      case WVarint => readVarint() __ Unit
      case WFix64  => if i + 8 > end then fail(s"skip of field $field runs off the end at byte $i") else i += 8
      case WLen    => lenSpan() __ Unit
      case WFix32  => if i + 4 > end then fail(s"skip of field $field runs off the end at byte $i") else i += 4
      case WSGroup => skipGroup(field, depth + 1) __ Unit
      case WEGroup => fail(s"field $field at byte $i: end-group with no start-group")
      case w       => fail(s"field $field at byte $i: wire type $w does not exist")

    /** Capture the current field verbatim instead of dropping it — the modern retention
      * behavior, so a read-modify-write pass preserves what this schema does not know.
      * A group is captured as its interior; `Out.unknown` restores the bracketing tags.
      */
    def keep(): Unknown = wire match
      case WVarint =>
        val a = i
        readVarint() __ Unit
        Unknown(field, wire, bytesWork(a, i))
      case WFix64 =>
        if i + 8 > end then fail(s"keep of field $field runs off the end at byte $i")
        val u = Unknown(field, wire, bytesWork(i, i + 8))
        i += 8
        u
      case WLen =>
        val a = lenSpanInt()
        Unknown(field, wire, bytesWork(a, i))
      case WFix32 =>
        if i + 4 > end then fail(s"keep of field $field runs off the end at byte $i")
        val u = Unknown(field, wire, bytesWork(i, i + 4))
        i += 4
        u
      case WSGroup =>
        val a = i
        val b = skipGroup(field, depth + 1)
        if b - a > Int.MaxValue - 8 then fail(s"field $field at byte $a: group too big for the heap")
        Unknown(field, WSGroup, bytesWork(a, b))
      case WEGroup => fail(s"field $field at byte $i: end-group with no start-group")
      case w => fail(s"field $field at byte $i: wire type $w does not exist")
  }

  object In {
    def of(bs: Array[Byte]): In = new ArrIn(bs, 0L, bs.length.toLong, 0)
    def of(bs: Array[Byte], i0: Int, iN: Int): In = new ArrIn(bs, i0.toLong, iN.toLong, 0)
    inline def of[R <: Iv.X | Rg](bs: Array[Byte], inline r: R): In = Iv.dispatch(r, bs)((i0, iN) => of(bs, i0, iN))
    def of(m: Mem[Byte]): In = new MemIn(m, 0L, m.length, 0)
    def of(m: Mem[Byte], i0: Long, iN: Long): In = new MemIn(m, i0, iN, 0)
  }

  final class ArrIn private[alien] (bs: Array[Byte], i0: Long, iN: Long, d: Int) extends In(i0, iN, d) {
    def readVarint(): Long =
      var x = 0L
      var shift = 0
      var more = true
      while more do
        if i >= end then fail(s"varint runs off the end at byte $i")
        if shift >= 70 then fail(s"varint longer than 10 bytes at byte $i")
        val b = bs(i.toInt)
        i += 1
        x |= (b & 0x7FL) << shift
        shift += 7
        more = (b & 0x80) != 0
      x

    protected def raw32Work(): Int =
      if i + 4 > end then fail(s"fixed32 runs off the end at byte $i")
      val j = i.toInt
      val v = (bs(j) & 0xFF) | ((bs(j+1) & 0xFF) << 8) | ((bs(j+2) & 0xFF) << 16) | ((bs(j+3) & 0xFF) << 24)
      i += 4
      v

    protected def raw64Work(): Long =
      if i + 8 > end then fail(s"fixed64 runs off the end at byte $i")
      val j = i.toInt
      var v = 0L
      var k = 7
      while k >= 0 do
        v = (v << 8) | (bs(j + k) & 0xFFL)
        k -= 1
      i += 8
      v

    protected def stringWork(i0: Long, iN: Long): String =
      if !validUtf8(bs, i0.toInt, iN.toInt) then fail(s"field $field at byte $i0: string is not valid UTF-8")
      new String(bs, i0.toInt, (iN - i0).toInt, java.nio.charset.StandardCharsets.UTF_8)

    protected def bytesWork(i0: Long, iN: Long): Array[Byte] =
      java.util.Arrays.copyOfRange(bs, i0.toInt, iN.toInt)

    protected def subWork(i0: Long, iN: Long): In = new ArrIn(bs, i0, iN, depth + 1)

    protected def viewWork(i0: Long, iN: Long): Mem[Byte] = (Mem of bs).view(i0, iN)
  }

  final class MemIn private[alien] (m: Mem[Byte], i0: Long, iN: Long, d: Int) extends In(i0, iN, d) {
    def readVarint(): Long =
      var x = 0L
      var shift = 0
      var more = true
      while more do
        if i >= end then fail(s"varint runs off the end at byte $i")
        if shift >= 70 then fail(s"varint longer than 10 bytes at byte $i")
        val b = m(i)
        i += 1
        x |= (b & 0x7FL) << shift
        shift += 7
        more = (b & 0x80) != 0
      x

    protected def raw32Work(): Int =
      if i + 4 > end then fail(s"fixed32 runs off the end at byte $i")
      val v = m.getI_le(i)
      i += 4
      v

    protected def raw64Work(): Long =
      if i + 8 > end then fail(s"fixed64 runs off the end at byte $i")
      val v = m.getL_le(i)
      i += 8
      v

    protected def stringWork(i0: Long, iN: Long): String =
      val a = bytesWork(i0, iN)
      if !validUtf8(a, 0, a.length) then fail(s"field $field at byte $i0: string is not valid UTF-8")
      new String(a, java.nio.charset.StandardCharsets.UTF_8)

    protected def bytesWork(i0: Long, iN: Long): Array[Byte] =
      val out = new Array[Byte]((iN - i0).toInt)
      m.inject(out)(i0, iN) __ Unit
      out

    protected def subWork(i0: Long, iN: Long): In = new MemIn(m, i0, iN, depth + 1)

    protected def viewWork(i0: Long, iN: Long): Mem[Byte] = m.view(i0, iN)
  }


  //////////////////////////////
  /// Accumulators           ///
  //////////////////////////////

  // Tiny growable buffers for repeated fields: primitive-typed so decoding repeated numbers
  // never boxes, plus a ClassTag-generic one for strings, bytes, and messages.

  final class IntAcc {
    private var a = new Array[Int](8)
    private var n = 0
    def +=(x: Int): Unit =
      if n >= a.length then a = java.util.Arrays.copyOf(a, a.length * 2)
      a(n) = x
      n += 1
    def ++=(xs: Array[Int]): Unit =
      var k = 0
      while k < xs.length do
        this += xs(k)
        k += 1
    def result: Array[Int] = java.util.Arrays.copyOf(a, n)
  }
  object IntAcc {
    def apply(): IntAcc = new IntAcc
    /** An accumulator seeded with prior contents (the merge-append form). */
    def apply(prior: Array[Int]): IntAcc = { val acc = new IntAcc; acc ++= prior; acc }
  }

  final class LongAcc {
    private var a = new Array[Long](8)
    private var n = 0
    def +=(x: Long): Unit =
      if n >= a.length then a = java.util.Arrays.copyOf(a, a.length * 2)
      a(n) = x
      n += 1
    def ++=(xs: Array[Long]): Unit =
      var k = 0
      while k < xs.length do
        this += xs(k)
        k += 1
    def result: Array[Long] = java.util.Arrays.copyOf(a, n)
  }
  object LongAcc {
    def apply(): LongAcc = new LongAcc
    /** An accumulator seeded with prior contents (the merge-append form). */
    def apply(prior: Array[Long]): LongAcc = { val acc = new LongAcc; acc ++= prior; acc }
  }

  final class FloatAcc {
    private var a = new Array[Float](8)
    private var n = 0
    def +=(x: Float): Unit =
      if n >= a.length then a = java.util.Arrays.copyOf(a, a.length * 2)
      a(n) = x
      n += 1
    def ++=(xs: Array[Float]): Unit =
      var k = 0
      while k < xs.length do
        this += xs(k)
        k += 1
    def result: Array[Float] = java.util.Arrays.copyOf(a, n)
  }
  object FloatAcc {
    def apply(): FloatAcc = new FloatAcc
    /** An accumulator seeded with prior contents (the merge-append form). */
    def apply(prior: Array[Float]): FloatAcc = { val acc = new FloatAcc; acc ++= prior; acc }
  }

  final class DoubleAcc {
    private var a = new Array[Double](8)
    private var n = 0
    def +=(x: Double): Unit =
      if n >= a.length then a = java.util.Arrays.copyOf(a, a.length * 2)
      a(n) = x
      n += 1
    def ++=(xs: Array[Double]): Unit =
      var k = 0
      while k < xs.length do
        this += xs(k)
        k += 1
    def result: Array[Double] = java.util.Arrays.copyOf(a, n)
  }
  object DoubleAcc {
    def apply(): DoubleAcc = new DoubleAcc
    /** An accumulator seeded with prior contents (the merge-append form). */
    def apply(prior: Array[Double]): DoubleAcc = { val acc = new DoubleAcc; acc ++= prior; acc }
  }

  final class BoolAcc {
    private var a = new Array[Boolean](8)
    private var n = 0
    def +=(x: Boolean): Unit =
      if n >= a.length then a = java.util.Arrays.copyOf(a, a.length * 2)
      a(n) = x
      n += 1
    def ++=(xs: Array[Boolean]): Unit =
      var k = 0
      while k < xs.length do
        this += xs(k)
        k += 1
    def result: Array[Boolean] = java.util.Arrays.copyOf(a, n)
  }
  object BoolAcc {
    def apply(): BoolAcc = new BoolAcc
    /** An accumulator seeded with prior contents (the merge-append form). */
    def apply(prior: Array[Boolean]): BoolAcc = { val acc = new BoolAcc; acc ++= prior; acc }
  }

  final class RefAcc[A >: Null <: AnyRef : ClassTag] {
    private var a = new Array[A](8)
    private var n = 0
    def +=(x: A): Unit =
      if n >= a.length then a = java.util.Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], a.length * 2).asInstanceOf[Array[A]]
      a(n) = x
      n += 1
    def ++=(xs: Array[A]): Unit =
      var k = 0
      while k < xs.length do
        this += xs(k)
        k += 1
    def result: Array[A] = java.util.Arrays.copyOf(a.asInstanceOf[Array[AnyRef]], n).asInstanceOf[Array[A]]
  }
  object RefAcc {
    def apply[A >: Null <: AnyRef : ClassTag](): RefAcc[A] = new RefAcc[A]
    /** An accumulator seeded with prior contents (the merge-append form). */
    def apply[A >: Null <: AnyRef : ClassTag](prior: Array[A]): RefAcc[A] = { val acc = new RefAcc[A]; acc ++= prior; acc }
  }
}
