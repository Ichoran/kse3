# kse3 eio

The eio module is input and output: files through extension methods on `Path`, delimited text through `Csv` and
`Xsv`, text through the `Grok` parser and the whole-string `parse` verbs, command lines through `Cleasy`, bytes
through `sendTo` and the stream and buffer views, the usual encodings, and at the bottom Unix-domain sockets that
carry file descriptors and shared memory that reads as a `Mem`.  Everything that can fail answers an `Ask`, so `.?`
chains a file operation the way it chains anything else, and the throwing JDK forms are still there under `raw` for
when an exception is what you want.

`import kse.eio.{given, *}` brings in everything except the command-line parser, which is
`import kse.eio.cleasy.*`.  It leans on `kse.basics`, `kse.flow` (`Ask`, `.?`, `Resource`), and `kse.maths`
(unsigned types, durations).

<!-- guide examples: eio/test/src/GuideExamples.scala -->
Every code example below is compiled from `eio/test/src/GuideExamples.scala` and, apart from the socket and
shared-memory ones, run there; `mill guides.run` verifies that the two copies are identical.

## What's here

- **Paths**: `slurp`, `gulp`, `write`, and the rest on `Path`, answering `Ask`; `atomically` for a replace that is never half done; `/`, `ext`, `base` for taking paths apart; `raw` for the throwing forms.
- **Xsv**: `Csv` and `Tsv` to and from `Array[Array[String]]`, other separators through `Xsv`, and a streaming visitor underneath.
- **Parsing**: `parseI`, `parseD`, and their kin read a whole string as one number, as an `Ask` or with a jump.
- **Grok**: a direct-mode parser; ask for an `I`, a `D`, a `tok`, or a `str` and get it or an error that says where; sub-parses, alternatives, and streaming sources.
- **Command lines**: `Cleasy` declares options with a parser, a short form, a default, and help, and gives back values typed by name.
- **Streams and buffers**: `sendTo` between streams, channels, and iterators; `input`, `output`, and `buffer` views of a byte array; `writeTo` a path from a collection.
- **Text and encodings**: `bytes` and `utf8`, hex, Base64, and Base85 in string and byte forms, decoding as an `Ask`.
- **Sockets and shared memory**: `FdSock` Unix-domain sockets that pass descriptors, and `SharedMemory` regions shared between processes as `Mem`; Linux and macOS, native access required.

## Paths

The file operations live on `java.nio.file.Path` as extension methods and answer `Ask` instead of throwing.
`"data/eel.csv".path` makes a path from a string and `dir / "name"` resolves one.  `name`, `ext`, `base`, and
`parent` take a path apart, and `nameTo`/`nameOp`, `extTo`/`extOp`, and `baseTo`/`baseOp` give a sibling with that
one part replaced or transformed.  `slurp` reads lines and `gulp` reads bytes.  `write`, `append`, `create` (fails
if the file exists), and `update` (writes only if the content differs, so timestamps stay quiet) take bytes, and
each has a `Lines` twin.  `atomically.write` and `writeLines` go through a same-directory temp file, an fsync, and a
rename, so a reader sees the old content or the new and never a partial file; `atomically.update` likewise.
`openRead`, `openWrite`, and `openAppend` give buffered streams meant for `Resource.nice`.  `exists`, `isDirectory`,
`size` (-1 if absent), `time`, `mkdir`, `mkdirs`, `touch`, `delete`, `copyTo`, `moveTo`, `copyInto`, `moveInto`,
`paths` for a directory's entries, and `recursively.delete()` do what they say.  `raw` is the same set throwing
exceptions.

**Reach for these when** you'd write `Files.readAllLines` in a `try`, build a sibling path by string surgery, or
write a file in place and hope nobody reads it half written.

Plain Scala:

<!-- guide: paths.plain -->
```scala
import java.nio.file.StandardCopyOption
import scala.jdk.CollectionConverters.*
def countLinesPlain(p: Path): Either[String, Int] =
  try Right(Files.readAllLines(p).size)
  catch case e: java.io.IOException => Left(s"$p: ${e.getMessage}")
def replacePlain(p: Path, lines: Seq[String]): Either[String, Unit] =
  try
    val tmp = p.resolveSibling(p.getFileName.toString + ".tmp")
    Files.write(tmp, lines.asJava): Unit
    Files.move(tmp, p, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING): Unit
    Right(())
  catch case e: java.io.IOException => Left(s"$p: ${e.getMessage}")
```

kse3:

<!-- guide: paths.kse3 -->
```scala
def countLines(p: Path): Ask[Int] = p.slurp.map(_.length)                        // slurp is Ask[Array[String]]
def replace(p: Path, lines: Seq[String]): Ask[Unit] = p.atomically.writeLines(lines)   // temp file, fsync, rename
```

The plain version's temp file is not forced to disk, so a crash can leave the rename done and the content not, and
`.tmp` collides with the next caller's.  Taking a path apart and listing a directory:

<!-- guide: pathnames.kse3 -->
```scala
val p = dir / "eel.csv"                       // resolve; "eel.csv".path makes one from a String
val q = p.extTo("tsv")                        // eel.tsv, beside it
val r = p.nameOp("old-" + _)                  // old-eel.csv
val stem = p.base                             // "eel"; p.ext is "csv", p.parent is a Path Or Unit
val csvs = dir.paths.select(_.ext == "csv")   // the entries of a directory, or empty if it isn't one
```

<!-- guide: pathopen.kse3 -->
```scala
Resource.nice(p.openRead())(_.close)(in => in.read())   // closed however the block ends; openWrite, openAppend likewise
```

<!-- guide: pathdirs.kse3 -->
```scala
val dir = Files.createTempDirectory("guide")
(dir / "work" / "data").mkdirs().?            // parents included
f(dir / "work")
dir.recursively.delete()                      // everything under dir, then dir; refuses to escape its root
!dir.exists
```

`recursively.atomicDelete()` renames the tree aside first, so nothing else sees it half gone.  `openIOMem[Long]()`
maps a file as a `Mem.Owned`, resizing it first if you pass a function of the count already there.  `inZip{ root => ... }` opens a
zip file as a filesystem and `zipTo` makes one.  `real` resolves symlinks even through a missing tail, and
`makeSymlink`, `symlinkTo`, `symlink`, and `followSymlink` are the link operations.

Full API: `eio/src/Paths.scala`, the `extension (the_path: Path)` block; `atomically` and `raw` are in
`PathsHelper` below it.

## Xsv

`Csv` and `Tsv` read and write delimited text as `Array[Array[String]]`: `Csv.decode` from a `String`,
`Array[Byte]`, `InputStream`, or a sequence of chunks, `Csv.read(path)`, and `Csv.write(table)(path)`.  Quoting is
RFC 4180 both ways: a quoted cell may hold the separator, a doubled quote, or a line break, and CR, LF, and CRLF all
end a row.  `Xsv.comma`, `tab`, `space`, and `semi` are the same parser for other separators, `Xsv.create(c)` for any
other ASCII separator (an `Ask`, since a newline is refused), and the `trim` forms (`Xsv.trimComma` and so on) strip whitespace around unquoted cells.
`bomless` on any of them skips a UTF-8 byte order mark.  Underneath is a streaming `Xsv.Visitor`:
`xsv.visit(content, visitor)` calls it per cell and per row without building anything, and `Xsv.Visitor.onString()`
and `onBytes()` are the table-building visitors, with a `strictRect` option that makes ragged rows an error.

**Reach for these when** you'd write `line.split(",")`.

Plain Scala:

<!-- guide: xsv.plain -->
```scala
def tablePlain(text: String): Array[Array[String]] =
  text.split("\n").map(_.split(",", -1))        // wrong the moment a cell is quoted
```

kse3:

<!-- guide: xsv.kse3 -->
```scala
def table(text: String): Ask[Array[Array[String]]] = Csv.decode(text)   // quotes, embedded commas and newlines, CRLF
```

<!-- guide: xsvfiles.kse3 -->
```scala
Csv.write(rows)(p).?                          // quotes only the cells that need it
val back = Csv.read(p).?                      // Tsv likewise; Xsv.semi, Xsv.space, Xsv.trimComma for other shapes
```

The parser is a byte-level state machine, so a large file is read as bytes and decoded per cell rather than through
a `String` of the whole thing.  Rows are as long as they are; nothing is padded.

Full API: `eio/src/Xsv.scala`; `Csv` and `Tsv` are at the bottom.

## Parsing

The `parse` verbs on `String` read the whole string as one number.  `parseI` gives an `Ask[Int]`; `parseI_?` gives
the `Int` or leaves the enclosing `Ask:` with the error, and never boxes on the way.  The grammar is `Grok`'s:
signed decimal `B`, `S`, `I`, `L`; unsigned `UB`, `US`, `UI`, `UL`; hex bit patterns `XB` to `XL` with no `0x`;
unsigned hex `UxB` to `UxL`; and `D` and `F`, correctly rounded.  Nothing is trimmed and nothing may follow the
number.  No `Grok` is built: these are the bare kernels in `kse.maths.Parse`.  `Int.from(s)` and the other
companions spell the `Ask` forms.

**Reach for these when** you'd write `s.toInt` in a `try`, `Try(s.toDouble).toOption`, or `s.toIntOption` and then
need to say why it failed.

Plain Scala:

<!-- guide: parse.plain -->
```scala
def ratioPlain(a: String, b: String): Either[String, Double] =
  try
    val x = a.toInt
    val y = b.toInt
    if y == 0 then Left("zero denominator") else Right(x.toDouble / y)
  catch case e: NumberFormatException => Left(e.getMessage)
```

kse3:

<!-- guide: parse.kse3 -->
```scala
def ratio(a: String, b: String): Ask[Double] = Ask:
  val x = a.parseI_?                            // the whole string must be the number; leaves with the Err if not
  val y = b.parseI_?
  if y == 0 then Err ?# "zero denominator"
  x.toDouble / y
```

<!-- guide: parseask.kse3 -->
```scala
val n = s.parseI                              // Ask[Int]; Int.from(s) is the same call
val bits = h.parseXI                          // a hex bit pattern, no 0x: "ff" is 255 and "ffffffff" is -1
val d = s.parseD                              // correctly rounded; parseL, parseUL, parseF, and so on likewise
```

Full API: `eio/src/Adaptors.scala`, the section headed "Covering numeric parses of whole strings".

## Grok

`Grok` is a direct-mode parser.  Inside `Grok(content){ g => ... }` you ask for what comes next and get it with
the cursor advanced, or the block leaves with an `Err` that says what was expected, what was found, and where (line,
column, and an excerpt).  The readers are one letter where a letter will do: `I`, `L`, `S`, `B` signed, `uI` and
kin unsigned, `xI` and kin hex, `D`, `F`, `Z` for a Boolean, `C` for one character; `tok` is the next delimited
token, `str` a quoted string (`Quote.json`, `Quote.csv`, `Quote.sql`), `digits(n)` and `chars(n)` fixed-width
fields; `< '-'` requires a character and `< "literal"` a literal; `skip(n)` and `end` do what they say.  The `_?`
forms take a predicate: `g.I_?(_ > 0, "count must be positive")`.  Tokens are delimited by line ends unless the
`delim` argument says otherwise: `Delim.white`, `Delim.of(",;")`, or a `Char` or `String`.  `g.grok(){ ... }` parses
within the next token with the delimiter narrowed one level (lines to words to nothing), and `g.select(a, b, c)`
tries alternatives from the same cursor and reports all of them if none fits.  `Grok.all` is the same and also
requires the input to be used up.  The content may be a `String`, `Array[Char]`, `Array[Byte]`, or `Mem[Byte]`
(structure is matched as ASCII and only the tokens are decoded, as UTF-8), and `Grok.buffered(inputStream)` or
`Grok.chunked(iterator)` parses through a sliding window when the input isn't in memory.

**Reach for this when** you'd write a regex with capture groups and `toInt` each one, split a line and index into
the pieces, or hand-write a recursive-descent parser for a small format.

Plain Scala:

<!-- guide: grok.plain -->
```scala
def recordsPlain(text: String): Either[String, List[(String, Double)]] =
  try
    val rs = text.linesIterator.filter(_.trim.nonEmpty).map{ line =>
      val ws = line.trim.split("\\s+")
      if ws.length != 2 then throw new IllegalArgumentException(s"bad line: $line")
      (ws(0), ws(1).toDouble)
    }
    Right(rs.toList)
  catch case e: IllegalArgumentException => Left(e.getMessage)   // NumberFormatException is one
```

kse3:

<!-- guide: grok.kse3 -->
```scala
def records(text: String): Ask[List[(String, Double)]] = Grok.all(text){ g =>   // tokens are lines; within one, words
  var rs = List.empty[(String, Double)]
  while g.sp.hasMore do rs = g.grok(){ (g.tok, g.D) } :: rs   // each line is its own sub-parse
  rs.reverse
}
```

<!-- guide: grokdate.kse3 -->
```scala
Grok(s, Delim.white, partial = true){ g => (g.I, (g < '-').I, (g < '-').I, g.tok) }   // "2026-09-16 eel"
```

<!-- guide: grokselect.kse3 -->
```scala
Grok(s, Delim.white){ g =>
  g.select(                                   // the first alternative to parse wins; each starts at the same cursor
    (g.I + g.I).toString,                     // "3 4" gives "7"
    { g.skip(1); g.D.toString },              // "x 2.5" gives "2.5"
    g.tok_?(_ == "none", "expected none")     // "none" gives "none"; anything else fails with all three attempts
  )
}
```

The failure is data: a `Grok.Failure` inside the `Err`, with `position`, `line`, `column`, `excerpt`, and the
`attempts` a `select` made.  `partial = true` lets a number stop at the first character that can't continue it (the
`-` in the date) instead of requiring a delimiter after it.  `exact = true` turns delimiters into separators, so
`a,,c` is three fields with an empty middle; that is the mode for record formats, where `Csv` is usually the better
tool anyway.  `sp` skips delimiters and never fails, `peekOr(c)` looks without moving, and `done(v)` finishes a
parse or sub-parse early.  A `Grok` is a mutable cursor for one parse on one thread; don't let it escape its block.

Full API: `eio/src/Grok.scala`; `Delim` and `Quote` at the top, the readers on `sealed abstract class Grok`, and
the sources in `object Grok`.

## Command lines

`Cleasy`, in `kse.eio.cleasy`, declares options once and parses `args` into values typed by name.  An option is a
long name, then optionally a short letter, a value parser, a default, and help text, joined by `~` and `%`:
`"count" ~ 'n' ~ (_int, () => 4) % "How many"`.  The parsers are `_int`, `_long`, `_double`, `_str`, `_tf` (a flag
that also accepts `=false`), `_path`, `_file`, `_dir`, `_uint`, and `_ulong`; `p.maybe` makes the value optional,
`the("fast") | "slow"` is a choice among literals, and `_int.desc("count")` names the value in the help.  `.x` on
the name (`"n".x ~ _int`) lets the option repeat and collects a `List`.  `parse(args)` answers an `Ask[Args]` whose
`apply("name")` has the type the declaration implies: the value when there is a default, an `Option` of it
otherwise, a `List` when it repeats.  `found("name")` says whether it was given, `args` is what was left, in order,
and `userString()` is the help text.  `--` ends option parsing and short flags combine as `-nv`.  Declaring two
options with the same name or letter is a compile error, as is asking for a name that wasn't declared.

**Reach for this when** you'd write a `while` over `args` with a `match` inside it.

Plain Scala:

<!-- guide: cleasy.plain -->
```scala
def optionsPlain(args: Array[String]): Either[String, (Int, Boolean, List[String])] =
  var n = 4
  var verbose = false
  var rest = List.empty[String]
  var i = 0
  try
    while i < args.length do
      args(i) match
        case "-n" => n = args(i + 1).toInt; i += 1
        case s if s.startsWith("--count=") => n = s.drop(8).toInt
        case "-v" | "--verbose" => verbose = true
        case s => rest = s :: rest
      i += 1
    Right((n, verbose, rest.reverse))
  catch case e: RuntimeException => Left(e.getMessage)   // NumberFormatException, ArrayIndexOutOfBoundsException
```

kse3:

<!-- guide: cleasy.kse3 -->
```scala
val spec = Cleasy("Count fish.")
  -- "count" ~ 'n' ~ (_int, () => 4) % "How many to count"   // --count=7 or -n 7; 4 when absent
  -- "verbose" ~ 'v' % "Say more"                              // a flag: --verbose or -v
def options(args: Array[String]): Ask[(Int, Boolean, List[String])] =
  spec.parse(args).map(a => (a("count"), a.found("verbose"), a.args.toList))   // typed by name; args is what's left
```

The plain version accepts `-nv`, `--count`, and `-n` with no value in ways it didn't mean to, and says nothing about
what it accepts; the kse3 one rejects an unknown option, a repeated single option, or a missing value with a message
that names the argument, and produces its own usage text.

Full API: `eio/src/Cleasy.scala`; the `Parse` values at the top, then `Opt`, `OptN`, `OptD`, `Args`, and `Cleasy`.

## Streams and buffers

`sendTo` moves bytes between anything that holds them: an `InputStream`, a `ReadableByteChannel`, a
`MultiArrayChannel`, or an iterator of `Array[Byte]` or `String` on one side, and an `OutputStream`, a
`WritableByteChannel`, or a `MultiArrayChannel` on the other, answering the count as an `Ask[Long]`.  The `Send`
typeclass behind it has `limited(n)` for a bounded move.  A collection of strings or byte arrays goes straight to a
file with `writeTo(path)`, `appendTo`, or `createAt`.  An `Array[Byte]` has views: `input()` is a
`ByteArrayInputStream`, `output()` an `OutputStream` that fills the array and refuses to overflow it, `buffer()` a
little-endian `ByteBuffer`, and `readChannel()` and `writeChannel()` channels; a `ByteBuffer`, `SeekableByteChannel`,
or `RandomAccessFile` has `input` and `output` stream views, and an `InputStream` has `channel`.
`MultiArrayChannel` is a growable in-memory `SeekableByteChannel` kept as chunks rather than one reallocated array:
write into it, then read it back.

**Reach for these when** you'd write a `read`/`write` loop with an 8K buffer, or `new ByteArrayInputStream(bytes)`.

Plain Scala:

<!-- guide: streams.plain -->
```scala
def copyPlain(in: java.io.InputStream, out: java.io.OutputStream): Long =
  val buf = new Array[Byte](8192)
  var total = 0L
  var n = in.read(buf)
  while n >= 0 do
    out.write(buf, 0, n)
    total += n
    n = in.read(buf)
  total
```

kse3:

<!-- guide: streams.kse3 -->
```scala
def copy(in: java.io.InputStream, out: java.io.OutputStream): Ask[Long] = in.sendTo(out)   // streams, channels, iterators
```

<!-- guide: streamviews.kse3 -->
```scala
val bytes = text.bytes                        // UTF-8; bytes.utf8 goes back
val sink = new Array[Byte](64)
val moved = bytes.input().sendTo(sink.output())   // a ByteArrayInputStream into a bounded OutputStream over sink
val first = bytes.buffer().getInt             // a little-endian ByteBuffer over the same array
```

<!-- guide: streamlines.kse3 -->
```scala
ls.writeTo(p).?                               // any collection of String or Array[Byte]; appendTo and createAt too
p.slurp.?
```

Full API: `eio/src/ReadWrite.scala` for `Send` and `sendTo`; `eio/src/Adaptors.scala` for the views and
`MultiArrayChannel`.

## Text and encodings

`s.bytes` is UTF-8 and `bytes.utf8` decodes it; `ascii`, `iso8859_1`, and `bomlessUtf8` (drops a byte order mark)
are the others.  Hex, Base64, and Base85 come in three forms on `Array[Byte]`: `encodeHex` to bytes,
`stringEncodeHex` to a `String`, and `decodeHex`, on bytes or a `String`, to an `Ask[Array[Byte]]`.  Hex is upper
case with a `Lo` variant; `encode64` is the URL-safe alphabet with `basic`, `mime`, and `lines` (76 columns)
variants; `encode85` is the ZeroMQ alphabet with an `ascii` variant.  The `EioHex`, `EioBase64`, and `EioBase85`
objects underneath encode and decode ranges into an existing array.

**Reach for these when** you'd write `f"$b%02x"` in a loop, or `new String(bytes, "UTF-8")` with the charset spelled
by hand.

Plain Scala:

<!-- guide: encode.plain -->
```scala
def hexPlain(bs: Array[Byte]): String = bs.map(b => f"$b%02X").mkString
def base64Plain(bs: Array[Byte]): String = java.util.Base64.getUrlEncoder.encodeToString(bs)
```

kse3:

<!-- guide: encode.kse3 -->
```scala
def hex(bs: Array[Byte]): String = bs.stringEncodeHex          // "DEADBEEF"; stringEncodeHexLo for lower case
def base64(bs: Array[Byte]): String = bs.stringEncode64        // the URL-safe alphabet; encode64basic is the standard one
def unhex(s: String): Ask[Array[Byte]] = s.decodeHex           // an Err, not an exception, for a bad digit or odd length
```

Full API: `eio/src/Adaptors.scala`; `EioBase64`, `EioBase85`, `EioHex`, and the `extension (underlying: Array[Byte])`
and `(underlying: String)` blocks after them.

## Sockets and shared memory

Two native facilities sit at the bottom of eio, for Linux and macOS, and the JVM needs
`--enable-native-access=ALL-UNNAMED` to use them.  `FdSock` is a Unix-domain stream socket that can carry a file
descriptor along with bytes (`SCM_RIGHTS`), which the JDK's own Unix-domain channels can't: `FdSock.listen(path)`
gives a `Server` and `connect(path)` a `Conn`, with `read`, `write`, `sendFd`, and `recvFd`, and every receive is
bounded by the timeout given at creation.  `SharedMemory` maps RAM-backed memory shared between processes as a
`Mem`: `createNamed` and `attach` by an OS name, which also works on Windows, or `createFd` and `attachFd`
anonymously, with `offerFd(path, n)` and `acceptFd(path)` doing the socket handoff in one call each.  The receiving
side holds a `Mem.Owned[A]` over the same physical pages.  Calls block in native code, so give a long wait a
platform thread, and a failed system call is an `Err` carrying the `errno` as data (`GetLastError` on Windows),
even from an entry point that throws: `PosixSocket.errnoOf(err)` reads the code back, and `SharedMemory.exhausted(err)`
says whether a failure was the system running out of memory or descriptors, worth retrying rather than reporting.
These two examples are
compiled but not run by the guide test, since they need a peer and native access.

**Reach for these when** another process on the same machine should see the same memory or receive an open
descriptor, and a TCP socket or a file would mean a copy.

<!-- guide: fdsock.kse3 -->
```scala
def ping(sock: Path): Ask[String] =
  Resource.Nice(FdSock.connect(sock, 5.s))(_.close()){ conn =>
    conn.write("ping".bytes).?
    val buf = new Array[Byte](64)
    val n = conn.read(buf).?                    // bounded by the timeout given at connect
    new String(buf, 0, n)
  }
```

<!-- guide: shm.kse3 -->
```scala
def offer(sock: Path): Ask[Unit] =
  Resource.Nice(SharedMemory.offerFd[Long](sock, 1024))(_.close()){ later =>
    later.use(_.use(_.set()(i => i * i)))       // fill the region; the Tidy.Later owns it until close
    later.op(_.serveOne()).?                    // one peer connects at sock and receives the descriptor
  }
def accept(sock: Path): Ask[Long] =
  Resource.nice(SharedMemory.acceptFd[Long](sock))(_.close()){ view =>
    view.op(m => m(3))                          // a Mem.Owned over the same physical memory: 9
  }
```

A named region is unlinked when its creator closes on POSIX and findable while any handle is open on Windows, and a
discovered size is exact on Linux but rounded to pages elsewhere, so a protocol that needs the logical length
carries it; the header comment of `SharedMemory.scala` has the full account.  `FdSock.Raw` is the non-blocking tier:
one system call per method, polled for readiness, for code that drives the descriptor itself.

Full API: `eio/src/FdSock.scala` and `eio/src/SharedMemory.scala`, whose header comments explain the platform
differences; `eio/src/PosixSocket.scala` is the layer of constants and downcalls beneath both.
