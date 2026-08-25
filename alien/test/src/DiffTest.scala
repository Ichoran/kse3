// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.test.alien


import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import com.google.protobuf.{ByteString, DynamicMessage}
import com.google.protobuf.DescriptorProtos._
import com.google.protobuf.Descriptors


/** Differential verification of the generated Track bindings against protobuf-java.
  * The descriptor below is track.proto transcribed by hand into protobuf-java's builder
  * API, so the oracle and the kse3 pipeline share nothing but the .proto contract.
  */
@RunWith(classOf[JUnit4])
class DiffTest {
  import kse.basics.testutilities.TestUtilities.{given, _}
  import kse.basics.{given, _}
  import kse.flow.{given, _}
  import kse.maths.{given, _}
  import kse.test.alien.track.{Mood, Pt, Track}

  given Asserter(
    (m, test, x) => assertEquals(m, x, test),
    (m, test, x) => assertNotEquals(m, x, test),
    assertTrue
  )

  import FieldDescriptorProto.{Type => FT, Label => FL}

  private def fld(name: String, number: Int, t: FT): FieldDescriptorProto.Builder =
    FieldDescriptorProto.newBuilder().setName(name).setNumber(number).setType(t).setLabel(FL.LABEL_OPTIONAL)

  val fileDesc: Descriptors.FileDescriptor =
    val mood = EnumDescriptorProto.newBuilder()
      .setName("Mood")
      .setOptions(EnumOptions.newBuilder().setAllowAlias(true))
      .addValue(EnumValueDescriptorProto.newBuilder().setName("MOOD_UNSPECIFIED").setNumber(0))
      .addValue(EnumValueDescriptorProto.newBuilder().setName("HAPPY").setNumber(1))
      .addValue(EnumValueDescriptorProto.newBuilder().setName("GLAD").setNumber(1))
      .addValue(EnumValueDescriptorProto.newBuilder().setName("GRUMPY").setNumber(16))
    val pt = DescriptorProto.newBuilder()
      .setName("Pt")
      .addField(fld("x", 1, FT.TYPE_DOUBLE))
      .addField(fld("y", 2, FT.TYPE_DOUBLE))
    val tagsEntry = DescriptorProto.newBuilder()
      .setName("TagsEntry")
      .setOptions(MessageOptions.newBuilder().setMapEntry(true))
      .addField(fld("key", 1, FT.TYPE_STRING))
      .addField(fld("value", 2, FT.TYPE_SINT64))
    val meta = DescriptorProto.newBuilder()
      .setName("Meta")
      .addField(fld("mood", 1, FT.TYPE_ENUM).setTypeName(".alien.test.Mood"))
      .addField(fld("blob", 2, FT.TYPE_BYTES))
    val track = DescriptorProto.newBuilder()
      .setName("Track")
      .addNestedType(tagsEntry)
      .addNestedType(meta)
      .addOneofDecl(OneofDescriptorProto.newBuilder().setName("extra"))
      .addOneofDecl(OneofDescriptorProto.newBuilder().setName("_score"))
      .addField(fld("id", 1, FT.TYPE_STRING))
      .addField(fld("pts", 2, FT.TYPE_MESSAGE).setTypeName(".alien.test.Pt").setLabel(FL.LABEL_REPEATED))
      .addField(fld("tags", 3, FT.TYPE_MESSAGE).setTypeName(".alien.test.Track.TagsEntry").setLabel(FL.LABEL_REPEATED))
      .addField(fld("score", 4, FT.TYPE_FLOAT).setProto3Optional(true).setOneofIndex(1))
      .addField(fld("hops", 5, FT.TYPE_INT32).setLabel(FL.LABEL_REPEATED).setOptions(FieldOptions.newBuilder().setPacked(false)))
      .addField(fld("note", 10, FT.TYPE_STRING).setOneofIndex(0))
      .addField(fld("anchor", 11, FT.TYPE_MESSAGE).setTypeName(".alien.test.Pt").setOneofIndex(0))
      .addField(fld("mood", 12, FT.TYPE_ENUM).setTypeName(".alien.test.Mood").setOneofIndex(0))
      .addField(fld("meta", 20, FT.TYPE_MESSAGE).setTypeName(".alien.test.Track.Meta"))
      .addField(fld("stamp", 21, FT.TYPE_UINT64))
    val f = FileDescriptorProto.newBuilder()
      .setName("track.proto")
      .setPackage("alien.test")
      .setSyntax("proto3")
      .addEnumType(mood)
      .addMessageType(pt)
      .addMessageType(track)
      .build()
    Descriptors.FileDescriptor.buildFrom(f, new Array[Descriptors.FileDescriptor](0))

  def trackD = fileDesc.findMessageTypeByName("Track")
  def ptD = fileDesc.findMessageTypeByName("Pt")
  def moodD = fileDesc.findEnumTypeByName("Mood")

  private def jPt(x: Double, y: Double): DynamicMessage =
    val b = DynamicMessage.newBuilder(ptD)
    if x != 0.0 then b.setField(ptD.findFieldByName("x"), x) __ Unit
    if y != 0.0 then b.setField(ptD.findFieldByName("y"), y) __ Unit
    b.build()

  /** The protobuf-java twin of AlienTest's fully-loaded Track, minus the map (map entry
    * order would make byte comparison flaky); one map entry is added in the semantic test.
    */
  private def jTrack(): DynamicMessage =
    val metaD = trackD.findNestedTypeByName("Meta")
    val mb = DynamicMessage.newBuilder(metaD)
      .setField(metaD.findFieldByName("mood"), moodD.findValueByNumber(16))
      .setField(metaD.findFieldByName("blob"), ByteString.copyFrom(Array[Byte](1, 2, 3)))
    val b = DynamicMessage.newBuilder(trackD)
      .setField(trackD.findFieldByName("id"), "t1")
      .setField(trackD.findFieldByName("score"), 0.5f)
      .setField(trackD.findFieldByName("anchor"), jPt(9.0, 9.0))
      .setField(trackD.findFieldByName("meta"), mb.build())
      .setField(trackD.findFieldByName("stamp"), java.lang.Long.valueOf(-1L))
    b.addRepeatedField(trackD.findFieldByName("pts"), jPt(1.5, -2.5)) __ Unit
    b.addRepeatedField(trackD.findFieldByName("pts"), jPt(0.0, 3.0)) __ Unit
    b.addRepeatedField(trackD.findFieldByName("hops"), java.lang.Integer.valueOf(3)) __ Unit
    b.addRepeatedField(trackD.findFieldByName("hops"), java.lang.Integer.valueOf(-4)) __ Unit
    b.addRepeatedField(trackD.findFieldByName("hops"), java.lang.Integer.valueOf(5)) __ Unit
    b.build()

  private def kTrack(): Track = Track(
    id = "t1",
    pts = Array(Pt(1.5, -2.5), Pt(0.0, 3.0)),
    score = Is(0.5f),
    hops = Array(3, -4, 5),
    extra = Track.Extra.Anchor(Pt(9.0, 9.0)),
    meta = Is(Track.Meta(Mood.GRUMPY, Array[Byte](1, 2, 3))),
    stamp = ULong(-1L)
  )

  @Test
  def diffBytesTest(): Unit =
    // both serializers emit in ascending field order, so map-free messages match byte for byte
    T ~ kTrack().toBytes =**= jTrack().toByteArray

  @Test
  def diffTheirsToOursTest(): Unit =
    val withTag = DynamicMessage.newBuilder(jTrack())
    val te = trackD.findNestedTypeByName("TagsEntry")
    val entry = DynamicMessage.newBuilder(te)
      .setField(te.findFieldByName("key"), "a")
      .setField(te.findFieldByName("value"), java.lang.Long.valueOf(Long.MinValue))
      .build()
    withTag.addRepeatedField(trackD.findFieldByName("tags"), entry) __ Unit
    val u = Track.parse(withTag.build().toByteArray).fold(t => t)(e => throw new AssertionError(e.toString))
    T ~ u.id ==== "t1"
    T ~ u.pts.map(p => (p.x, p.y)).toList ==== List((1.5, -2.5), (0.0, 3.0))
    T ~ u.tags ==== Map("a" -> Long.MinValue)
    T ~ u.score ==== Is(0.5f)
    T ~ u.hops.toList ==== List(3, -4, 5)
    T ~ u.extra ==== Track.Extra.Anchor(Pt(9.0, 9.0))
    T ~ u.meta.fold(m => (m.mood.number, m.blob.toList))(_ => (-1, Nil)) ==== (16, List[Byte](1, 2, 3))
    T ~ u.stamp.signed ==== -1L

  @Test
  def diffOursToTheirsTest(): Unit =
    val t = kTrack().copy(tags = Map("k" -> -3L))
    val m = DynamicMessage.parseFrom(trackD, t.toBytes)
    T ~ m.getField(trackD.findFieldByName("id")) ==== "t1"
    val pts = m.getField(trackD.findFieldByName("pts")).asInstanceOf[java.util.List[DynamicMessage]]
    T ~ pts.size ==== 2
    T ~ pts.get(0).getField(ptD.findFieldByName("x")) ==== 1.5
    T ~ pts.get(1).getField(ptD.findFieldByName("y")) ==== 3.0
    T ~ m.hasField(trackD.findFieldByName("score")) ==== true
    T ~ m.getField(trackD.findFieldByName("score")) ==== 0.5f
    T ~ m.getOneofFieldDescriptor(trackD.getOneofs.get(0)).getName ==== "anchor"
    T ~ m.getField(trackD.findFieldByName("stamp")) ==== java.lang.Long.valueOf(-1L)
    val tags = m.getField(trackD.findFieldByName("tags")).asInstanceOf[java.util.List[DynamicMessage]]
    val te = trackD.findNestedTypeByName("TagsEntry")
    T ~ tags.size ==== 1
    T ~ tags.get(0).getField(te.findFieldByName("key")) ==== "k"
    T ~ tags.get(0).getField(te.findFieldByName("value")) ==== java.lang.Long.valueOf(-3L)

  @Test
  def diffEdgeValuesTest(): Unit =
    // negative-zero float in an optional field: presence plus sign both survive the oracle
    val negZero = Track(score = Is(-0.0f)).toBytes
    val m = DynamicMessage.parseFrom(trackD, negZero)
    T ~ m.hasField(trackD.findFieldByName("score")) ==== true
    T ~ java.lang.Float.floatToRawIntBits(m.getField(trackD.findFieldByName("score")).asInstanceOf[Float]) ==== 0x80000000
    T ~ Track.parse(m.toByteArray).fold(_.score.fold(_.bitsI)(_ => 0))(_ => 1) ==== 0x80000000
    // an enum number no one declared, both directions
    val whoKnows = DynamicMessage.newBuilder(trackD)
      .setField(trackD.findFieldByName("mood"), moodD.findValueByNumberCreatingIfUnknown(42))
      .build()
    T ~ Track.parse(whoKnows.toByteArray).fold(_.extra)(e => throw new AssertionError(e.toString)) ==== Track.Extra.MoodArm(Mood(42))
    val ours = Track(extra = Track.Extra.MoodArm(Mood(42))).toBytes
    T ~ DynamicMessage.parseFrom(trackD, ours).getField(trackD.findFieldByName("mood")).asInstanceOf[Descriptors.EnumValueDescriptor].getNumber ==== 42
    // oneof arms at their zero values are presence, not absence, to both sides
    val eNote = Track(extra = Track.Extra.Note("")).toBytes
    T ~ DynamicMessage.parseFrom(trackD, eNote).getOneofFieldDescriptor(trackD.getOneofs.get(0)).getName ==== "note"
    val jNote = DynamicMessage.newBuilder(trackD).setField(trackD.findFieldByName("note"), "").build().toByteArray
    T ~ Track.parse(jNote).fold(_.extra)(e => throw new AssertionError(e.toString)) ==== Track.Extra.Note("")
    T ~ jNote =**= eNote

  @Test
  def diffUnknownRetentionTest(): Unit =
    // fields our schema does not name survive a kse3 read-modify-write and land where
    // protobuf-java expects them: in its UnknownFieldSet, values intact
    val junky = kse.alien.Pb.Out()
    junky.string(1, "keeper")
    junky.int32Always(99, 12345)
    junky.doubleAlways(98, 2.25)
    junky.stringAlways(97, "junk")
    val ours = Track.parse(junky.result).fold(t => t)(e => throw new AssertionError(e.toString))
    val m = DynamicMessage.parseFrom(trackD, ours.copy(id = "edited").toBytes)
    T ~ m.getField(trackD.findFieldByName("id")) ==== "edited"
    val uf = m.getUnknownFields
    T ~ uf.getField(99).getVarintList.get(0) ==== java.lang.Long.valueOf(12345L)
    T ~ uf.getField(98).getFixed64List.get(0).longValue.bitsD ==== 2.25
    T ~ uf.getField(97).getLengthDelimitedList.get(0).toStringUtf8 ==== "junk"
    // an unknown group also rides through and lands as a group in pb-java's UnknownFieldSet
    val g = Array[Byte](0x33, 0x08, 0x01, 0x34)   // group field 6 { f1: varint 1 }
    val withG = Track.parse(Track(id = "g").toBytes ++ g).fold(t => t)(e => throw new AssertionError(e.toString))
    val mg = DynamicMessage.parseFrom(trackD, withG.toBytes)
    T ~ mg.getUnknownFields.getField(6).getGroupList.size ==== 1
    T ~ mg.getUnknownFields.getField(6).getGroupList.get(0).getField(1).getVarintList.get(0) ==== java.lang.Long.valueOf(1L)

  @Test
  def diffMergeTest(): Unit =
    // concatenated encodings must merge; protobuf-java is the oracle for every clause
    val t1 = Track(id = "a", pts = Array(Pt(1.0, 2.0)), hops = Array(1),
                   extra = Track.Extra.Anchor(Pt(1.0, 0.0)), meta = Is(Track.Meta(Mood.GRUMPY, Array[Byte](1))), stamp = ULong(1L))
    val t2 = Track(id = "b", pts = Array(Pt(3.0, 4.0)),
                   extra = Track.Extra.Anchor(Pt(0.0, 2.0)), meta = Is(Track.Meta(blob = Array[Byte](9))))
    val cat = t1.toBytes ++ t2.toBytes
    val ours = Track.parse(cat).fold(t => t)(e => throw new AssertionError(e.toString))
    val jm = DynamicMessage.parseFrom(trackD, cat)
    T ~ jm.getField(trackD.findFieldByName("id")) ==== ours.id
    T ~ jm.getField(trackD.findFieldByName("pts")).asInstanceOf[java.util.List[?]].size ==== ours.pts.length
    T ~ jm.getField(trackD.findFieldByName("stamp")) ==== java.lang.Long.valueOf(ours.stamp.signed)
    val metaD = trackD.findNestedTypeByName("Meta")
    val jMeta = jm.getField(trackD.findFieldByName("meta")).asInstanceOf[DynamicMessage]
    T ~ jMeta.getField(metaD.findFieldByName("mood")).asInstanceOf[Descriptors.EnumValueDescriptor].getNumber ==== ours.meta.fold(_.mood.number)(_ => -1)
    T ~ jMeta.getField(metaD.findFieldByName("blob")).asInstanceOf[ByteString].toByteArray.toList ==== ours.meta.fold(_.blob.toList)(_ => Nil)
    val jAnchor = jm.getField(trackD.findFieldByName("anchor")).asInstanceOf[DynamicMessage]
    val ourAnchor = ours.extra match
      case Track.Extra.Anchor(p) => (p.x, p.y)
      case _ => (Double.NaN, Double.NaN)
    T ~ (jAnchor.getField(ptD.findFieldByName("x")), jAnchor.getField(ptD.findFieldByName("y"))) ==== ourAnchor
    T ~ ourAnchor ==== (1.0, 2.0)
}
