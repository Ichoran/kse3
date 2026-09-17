// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

package kse.test.alien

// Every example in GUIDE_EXTRA-alien.md lives here between a `// guide: name` and `// guide: end` pair,
// so that the guide's code is compiled and run.  `check-guides.py` verifies the two stay identical.

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit._
import org.junit.Assert._

import io.grpc.Status

import kse.basics.{given, *}
import kse.flow.{given, *}
import kse.alien.{Pb, Proto, PbGen, Grpc}
import kse.test.alien.track.{Pt, Track}   // generated from track.proto by PbGen and checked in as TrackProto.scala

object GuideExamples {

  // guide: wire.kse3
  def encodePoint(x: Double, y: Double, label: String): Array[Byte] =
    val o = Pb.Out()                                   // an Array[Byte] target; Pb.Out.into(mem) writes off-heap instead
    o.double(1, x)                                     // field number, then value; proto3 defaults (0, "", empty) are omitted
    o.double(2, y)
    o.string(3, label)
    o.result
  def decodePoint(bytes: Array[Byte]): Ask[(Double, Double, String)] =
    Pb.decode(bytes){ in =>                            // failures halt inside; Ask at the rim, with field and byte offset
      var x = 0.0
      var y = 0.0
      var label = ""
      while in.next() do in.field match
        case 1 => x = in.double()
        case 2 => y = in.double()
        case 3 => label = in.string()
        case _ => in.skip()                            // or in.keep() to carry an unknown field through
      (x, y, label)
    }
  // guide: end


  // guide: schema.kse3
  val proto = """
    syntax = "proto3";
    package fish;
    message Catch {
      string species = 1;
      repeated double lengths = 2;
      optional int32 depth = 3;
      map<string, string> notes = 4;
    }
  """
  def bindings(): Ask[List[(String, String)]] = Ask:
    val schema = Proto.read(proto, "catch.proto").?    // parse and link; proto2, editions, and groups are refused by name
    PbGen.generate(schema).?                           // one Scala source per .proto file, as (file name, text)
  // guide: end

  // guide: bindings.kse3
  def roundTrip(): Ask[Track] =
    val t = Track(id = "eel", pts = Array(Pt(1.5, 2.5), Pt(3.0, 4.0)), tags = Map("bait" -> 2L))
    val bytes = t.toBytes                              // the wire encoding; Pb.encodeInto(mem)(t.writeTo) for off-heap
    Track.parse(bytes)                                 // Ask[Track]; an error names the message, field, and byte offset
  // guide: end


  // guide: grpc.kse3
  val center = Grpc.unary("fish.Tracker", "Center", Track, Pt)   // a method descriptor from two generated companions
  val tracker = Grpc.Service().unary(center){ t =>                 // a server: plain functions, Ask results
    if t.pts.length == 0 then Grpc.or(Status.Code.INVALID_ARGUMENT, "no points")   // the peer sees this Status
    else Is(Pt(t.pts.map(_.x).sum / t.pts.length, t.pts.map(_.y).sum / t.pts.length))
  }
  def centerOf(t: Track): Ask[Pt] =
    Resource.Nice(Grpc.loopback(tracker.definition))(hl => { hl._2.close(); hl._1.close() }){ (host, link) =>
      Grpc.call(link.channel, center, t).?             // Ask[Pt]; serve(port) and connectLocal(target) for a real socket
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

  @Test
  def wireTest(): Unit =
    val bs = G.encodePoint(1.5, -2.0, "eel")
    T ~ G.decodePoint(bs) ==== (1.5, -2.0, "eel")
    T ~ G.decodePoint(G.encodePoint(0.0, 0.0, "")).map(_ => bs.length > 0) ==== true
    T ~ G.encodePoint(0.0, 0.0, "").length ==== 0
    T ~ G.decodePoint(Array[Byte](0x09, 1)).isAlt ==== true
    T ~ G.decodePoint(bs ++ G.encodePoint(0.0, 7.0, "")) ==== (1.5, 7.0, "eel")

  @Test
  def schemaTest(): Unit =
    val files = G.bindings()
    T ~ files.map(_.map(_._1)) ==== List("CatchProto.scala")
    T ~ files.map(_.head._2.contains("final case class Catch(")) ==== true
    T ~ files.map(_.head._2.contains("package fish")) ==== true
    T ~ G.roundTrip().map(t => (t.id, t.pts.toList, t.tags)) ==== ("eel", List(Pt(1.5, 2.5), Pt(3.0, 4.0)), Map("bait" -> 2L))

  @Test
  def grpcTest(): Unit =
    T ~ G.centerOf(Track(id = "tri", pts = Array(Pt(0, 0), Pt(3, 0), Pt(0, 3)))) ==== Pt(1, 1)
    val hollow = G.centerOf(Track(id = "hollow"))
    T ~ hollow.isAlt ==== true
    T ~ hollow.fold(_ => "")(e => Grpc.statusOf(e).getCode.toString) ==== "INVALID_ARGUMENT"
}
