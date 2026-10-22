// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

// The schemas the alien tests are generated from, and the runner that generates them; a module of its own so
// the build can generate before alien.test compiles.

package kse.test.alien


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
    |  bytes payload = 30 [(kse3.view) = true];
    |  repeated double wave = 31 [(kse3.view) = true];
    |  reserved 100 to max;
    |  reserved "old_name";
    |}
    |
    |service Tracker {
    |  rpc Get (Pt) returns (Track);
    |  rpc Watch (Pt) returns (stream Track) { option idempotency_level = NO_SIDE_EFFECTS; }
    |}
    |""".stripMargin

  /** The config TrackProto.scala is generated with. */
  val trackConfig = kse.alien.PbGen.Config(pkgOf = _ => "kse.test.alien.track", walker = true)

  /** Two files, the second reaching into the first, for `Config(known = true)`: optional, repeated, map and oneof
    * message fields, two oneofs in one message, an unsigned map key, and a map of scalars.
    */
  val knownBase = """
    |syntax = "proto3";
    |package alien.knownbase;
    |
    |message Value {
    |  oneof kind {
    |    double num = 1;
    |    string text = 2;
    |  }
    |  string unit = 3;
    |}
    |
    |message Shape {
    |  oneof x {
    |    int32 width = 1;
    |    Value scale = 2;
    |  }
    |}
    |
    |message Flat {
    |  int32 a = 1;
    |  repeated Flat more = 2;
    |}
    |""".stripMargin

  val knownStage = """
    |syntax = "proto3";
    |package alien.known;
    |
    |import "knownbase.proto";
    |
    |message Stage {
    |  string name = 1;
    |  optional alien.knownbase.Shape input = 2;
    |  map<string, alien.knownbase.Value> params = 3;
    |  repeated Row rows = 4;
    |  map<uint32, Row> by_id = 5;
    |  oneof body {
    |    Row single = 6;
    |    string note = 7;
    |  }
    |  oneof mode {
    |    int32 level = 8;
    |    bool off = 9;
    |  }
    |  alien.knownbase.Flat flat = 10;
    |  map<int32, string> tags = 11;
    |  message Row {
    |    int32 n = 1;
    |    alien.knownbase.Value v = 2;
    |  }
    |}
    |""".stripMargin

  val knownSources = List(("knownbase.proto", knownBase), ("stage.proto", knownStage))

  /** The config KnownbaseProto.scala and StageProto.scala are generated with. */
  val knownConfig = kse.alien.PbGen.Config(pkgOf = p => "kse.test.alien." + p.split('.').last, known = true)
}


/** Writes the generated sources the alien tests compile against: TrackProto.scala from `TestProtos.track`, and
  * KnownbaseProto.scala and StageProto.scala from `TestProtos.knownSources`.  The build runs it into
  * `alien.test`'s generated sources; `mill alien.testgen.run <dir>` writes them anywhere to read.
  */
object GenMain {
  def main(args: Array[String]): Unit =
    import kse.basics.{given, _}
    import kse.flow.{given, _}
    if args.length != 1 then
      System.err.println("Usage: GenMain <output-dir>")
      System.exit(2)
    val gen = Or.Ret:
      val track = kse.alien.PbGen.generate(kse.alien.Proto.read(List(("track.proto", TestProtos.track))).?, TestProtos.trackConfig).?
      val known = kse.alien.PbGen.generate(kse.alien.Proto.read(TestProtos.knownSources).?, TestProtos.knownConfig).?
      track ++ known
    gen.fold{ files =>
      files.foreach: (name, content) =>
        java.nio.file.Files.writeString(java.nio.file.Path.of(args(0), name), content) __ Unit
    }{ e =>
      System.err.println("FAILED:\n" + e.toString)
      System.exit(1)
    }
}
