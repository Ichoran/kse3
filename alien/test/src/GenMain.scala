// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab).

// Scratch dev runner: regenerate TrackProto.scala from TestProtos.track, and KnownbaseProto.scala and
// StageProto.scala from TestProtos.knownSources.
// Usage: mill alien.test.runMain kse.test.alien.GenMain <output-dir>

package kse.test.alien

object GenMain {
  def main(args: Array[String]): Unit =
    import kse.flow.{given, _}
    val gen = Or.Ret:
      val track = kse.alien.PbGen.generate(kse.alien.Proto.read(List(("track.proto", TestProtos.track))).?, TestProtos.trackConfig).?
      val known = kse.alien.PbGen.generate(kse.alien.Proto.read(TestProtos.knownSources).?, TestProtos.knownConfig).?
      track ++ known
    gen.fold{ files =>
      files.foreach: (name, content) =>
        val dir = if args.length > 0 then args(0) else "."
        val p = java.nio.file.Path.of(dir, name)
        java.nio.file.Files.writeString(p, content)
        println(s"wrote $p")
    }{ e => println("FAILED:\n" + e.toString) }
}
