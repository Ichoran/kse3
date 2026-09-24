# kse3 alien

The alien module is the process boundary: talking to code that is not this JVM, in the format it speaks.  It is
protocol buffers written from scratch.  A proto3 wire codec reads and writes arrays or off-heap `Mem` with no copy
between them; a parser and linker for `.proto` files means no protoc anywhere; a code generator turns a schema into
kse3-style Scala, case classes whose parsing answers `Ask`, with `Or Unit` for an optional message, plain arrays for
repeated fields, and `Mem` views where zero-copy is asked for; and a thin rim over grpc-java makes a call an ordinary
function answering `Ask` and a stream something straight-line code pulls from and pushes to.  Alien is in `all`,
not `foundation`, and gRPC is a compile-only dependency: a program using only `Pb`, `Proto`, and `PbGen` adds no
jar, and one using `Grpc` adds grpc-api, grpc-stub, and a transport to its own build.

`import kse.alien.{Pb, Proto, PbGen, Grpc}` names the four objects; generated code imports `kse.alien.Pb` itself.
Everything leans on `kse.basics`, `kse.flow`, and `kse.maths` (`UInt` and `ULong` for the unsigned field types).

<!-- guide examples: alien/test/src/GuideExamples.scala -->
Every code example below is compiled and run from `alien/test/src/GuideExamples.scala`, the gRPC one over the
in-process transport; `mill guides.run` verifies that the two copies are identical.

## What's here

- **Wire codec**: `Pb.Out` to emit fields and `Pb.decode(bytes){ in => ... }` to read them, on arrays or `Mem`; unknown fields kept or skipped; zero-copy views.
- **Schemas and generated bindings**: `Proto.read` parses and links `.proto` text, `PbGen.generate` writes the Scala; the generated classes parse, print, and merge as the spec says.
- **Grpc**: method descriptors from generated companions, a `Service` of plain functions, `serve`, `loopback`, and `connectLocal`, and `call`, `stream`, `upload`, and `converse` answering `Ask`.

## Wire codec

`Pb.Out()` is an encoder over a growable array; `Pb.Out.into(mem)` writes into a span of off-heap memory and halts if
the encoding would outgrow it.  Its methods are the proto3 field types, each taking a field number and a value:
`int32`, `sint64`, `fixed32`, `double`, `bool`, `string`, `bytes`, `msg` for an embedded message, `packedDouble`
and kin for packed repeated fields, and an `Always` twin of each for when a default value must still go on the
wire.  `Pb.decode(bytes){ in => ... }` runs a reader against a `Pb.In`: `in.next()` advances to the next field,
`in.field` and `in.wire` say which one, the typed readers take its value, `in.sub()` descends into an embedded
message, `in.skip()` steps over a field, and `in.keep()` captures one verbatim as a `Pb.Unknown` for re-emission.
A failure anywhere inside halts with the field and byte offset, wrapped in the `Pb.context` names of the messages it
was in, and comes out of the rim as an `Err`.  Both sides come in two substrates, `Array[Byte]` for sockets and
gRPC and `Mem[Byte]` for shared memory and mapped files, with the per-byte loops monomorphic in each.
`bytesView()` and `packedDoubleView()` answer `Mem` slices that alias the buffer, the only zero-copy the format
permits; everything else is an owned copy.

**Reach for this when** you'd lay out bytes by hand for another process to read, or pull in the protobuf runtime for
one message.

<!-- guide: wire.kse3 -->
```scala
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
```

The proto3 rules are followed loudly: a `string` that is not UTF-8 is an error, a singular field with the wrong wire
type is an error rather than an unknown, groups are skipped and kept but never decoded, and nesting stops at 512
levels.  Concatenating two encodings of a message is a merge, as the spec says, which the reader above shows: a
later scalar wins.

Full API: `alien/src/Pb.scala`; its header comment lists every deliberate choice.

## Schemas and generated bindings

`Proto.read(text, "name.proto")` parses one file and links it, and `Proto.read(List((name, text), ...))` links
several together, resolving type references the way protoc does.  Proto2, editions, `required`, `group`, `extend`,
and `extensions` are refused by name.  `PbGen.generate(schema, config)` gives one Scala source per file.  A message
becomes a `final case class` with proto3 defaults whose companion is a `Pb.Companion`, so `Msg.parse(bytes)` is an
`Ask[Msg]`, `msg.toBytes` the encoding, and `Msg.readFrom(in, prior)` a merge on top of an existing value.  An
optional message is `T Or Unit`, an optional narrow scalar the boxless `Pb.OptInt` and kin (with `Option`'s
vocabulary as inline extensions, `isEmpty`, `getOrElse`, `map`, `toOption`, and `from(Option)` to build one), a repeated field a plain
array, a map an immutable `Map`, an enum an open opaque `Int` with names, and a `oneof` a Scala enum with an `Unset`
case.  Unknown fields are retained and re-emitted unless `Config(retainUnknown = false)`.  A field marked
`[(kse3.view) = true]`, or named in `Config.viewFields`, decodes as a `Mem` view aliasing the buffer, and a message
holding one gets an `owned` method that copies it off.  `Config(walker = true)` adds a generated walker over every
repeated and length-carrying field, for a size bound that must cover a whole schema.

**Reach for these when** you'd run protoc and ScalaPB, or keep a message class in step with a `.proto` file by hand.

<!-- guide: schema.kse3 -->
```scala
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
```

<!-- guide: bindings.kse3 -->
```scala
def roundTrip(): Ask[Track] =
  val t = Track(id = "eel", pts = Array(Pt(1.5, 2.5), Pt(3.0, 4.0)), tags = Map("bait" -> 2L))
  val bytes = t.toBytes                              // the wire encoding; Pb.encodeInto(mem)(t.writeTo) for off-heap
  Track.parse(bytes)                                 // Ask[Track]; an error names the message, field, and byte offset
```

`Track` and `Pt` there are generated output, checked in as `alien/test/src/TrackProto.scala`, which compiles under
the project's full warning set; `mill alien.test.runMain kse.test.alien.GenMain <dir>` regenerates it from the
schema in `AlienTest`.  A generated companion also parses from an `InputStream` and from a span of an array or
`Mem`.

Full API: `alien/src/Schema.scala` for `Proto` and its syntax tree; `alien/src/PbGen.scala`, whose header comment
gives the whole mapping.

## Grpc

`Grpc.unary(service, name, In, Out)` makes a method descriptor from two generated companions, and `serverStream`,
`clientStream`, and `bidi` likewise.  `Grpc.Service()` assembles a server from plain functions: `unary(m){ a => ... }`
answers `Ask[B]`, `serverStream(m){ (a, sender) => ... }` pushes through a `Sender`, `clientStream(m){ inbox => ... }`
pulls from an `Inbox`, and `bidi` has both; `definition` is what a server serves.  `Grpc.serve(port)(defs)` serves
on TCP with handlers on virtual threads, `Grpc.loopback(defs)` joins a server and a channel in-process for tests,
and `Grpc.connectLocal(target)` opens a plaintext channel; each comes back as a closeable `Host` or `Link`.
`connect` and `connectLocal` hand the `Link` back READY, waiting up to `readyWithin` (10 s by default) for a server
that is still starting, so a first call does not fail fast with `UNAVAILABLE`; `Duration.ZERO` gives grpc's lazy
channel, and `link.ready(within)` waits again later.  On the
client, `Grpc.call(channel, m, a)` is an `Ask[B]`, `stream` hands each response to a function, `upload` feeds a
`Sender`, and `converse` gives both ends.  A failed call is an `Err` that remembers its `Status`, read with
`Grpc.statusOf`, and a handler refuses with `Grpc.or(code, why)`.  Handlers may block, since every executor this
layer makes runs virtual threads, and nothing here keeps the JVM alive after a `Host` is closed.

**Reach for this when** you'd write a `StreamObserver`, or a `sys.exit` at the end of a gRPC main to get the process
to stop.

<!-- guide: grpc.kse3 -->
```scala
val center = Grpc.unary("fish.Tracker", "Center", Track, Pt)   // a method descriptor from two generated companions
val tracker = Grpc.Service().unary(center){ t =>                 // a server: plain functions, Ask results
  if t.pts.length == 0 then Grpc.or(Status.Code.INVALID_ARGUMENT, "no points")   // the peer sees this Status
  else Is(Pt(t.pts.map(_.x).sum / t.pts.length, t.pts.map(_.y).sum / t.pts.length))
}
def centerOf(t: Track): Ask[Pt] =
  Resource.Nice(Grpc.loopback(tracker.definition))(hl => { hl._2.close(); hl._1.close() }){ (host, link) =>
    Grpc.call(link.channel, center, t).?             // Ask[Pt]; serve(port) and connectLocal(target) for a real socket
  }
```

grpc-java stays in charge of HTTP/2, flow control, deadlines, and transports; `Grpc.serve(builder)` and
`Grpc.connect(builder)` take a builder you configured, TLS and all, for anything that crosses a real network.
Using `Grpc` means adding grpc-api, grpc-stub, and one of grpc-netty-shaded, grpc-okhttp, or grpc-inprocess to
your build.

Full API: `alien/src/Grpc.scala`; its header comment shows all four call shapes on both sides.
