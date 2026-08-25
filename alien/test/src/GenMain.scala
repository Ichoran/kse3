// Scratch dev runner: regenerate TrackProto.scala from TestProtos.track.
// Usage: mill alien.test.runMain kse.test.alien.GenMain <output-dir>

package kse.test.alien

object GenMain {
  def main(args: Array[String]): Unit =
    import kse.flow.{given, _}
    val gen = Or.Ret:
      kse.alien.PbGen.generate(kse.alien.Proto.read(List(("track.proto", TestProtos.track))).?, TestProtos.trackConfig).?
    gen.fold{ files =>
      files.foreach: (name, content) =>
        val dir = if args.length > 0 then args(0) else "."
        val p = java.nio.file.Path.of(dir, name)
        java.nio.file.Files.writeString(p, content)
        println(s"wrote $p")
    }{ e => println("FAILED:\n" + e.toString) }
}
