# The kse3 guides

kse3 (Kerr Scala Extensions 3) is a set of Scala 3 libraries that make fast code short: unboxed error handling, arrays
that do what data frames do, off-heap memory that reads like an array, structured concurrency on virtual threads, and
the numerics, IO, JSON, plotting, and wire formats to go with them.  Nearly everything is an extension method or an
opaque type, and nearly everything hot is `inline`, so the convenience is free at runtime.

This file is the map.  Read it once; then read a module's guide when the trigger below says so.

## How the guides work

- **GUIDE-all.md** (this file): what each module is for and when to read its guide, the vocabulary the guides use
  without re-explaining, and a table of plain-Scala habits with their kse3 replacements.
- **GUIDE-*module*.md**: one file per module.  Each opens with a "What's here" list whose bold names are exactly the
  section headers below it, so one section can be pulled out on its own:

  ```
  awk '/^## /{p = ($0 == "## Intervals")} p' GUIDE-basics.md
  ```

  Each section says what the facility is, opens with **Reach for these when** and the situation that calls for it,
  shows plain Scala next to kse3 where the contrast teaches something, and ends with where the full API lives.
- **GUIDE_EXTRA-*module*.md**: the same shape, for the modules outside `foundation` (twodee, alien), which are
  still moving and may take outside dependencies.  The underscore sorts them after the core guides.
- **Every code example is real.**  It is copied verbatim from `*module*/test/src/GuideExamples.scala`, where it is
  compiled with the project's warning flags and run by a JUnit test that checks the plain and kse3 versions agree.
  `mill guides.run` verifies that the guide text and the code are identical, and that the section lists
  match the headers.  When the two differ it reports, and never repairs, because which side is right is a judgement.

## The modules

Dependencies point downward; `foundation` bundles everything with no outside dependencies, and `all` adds the modules
that may take some.

```
basics                      arrays, intervals, Mu and Atom, strings, sorting, labels, tuples, Mem, test harness
  ├─ flow                   control flow: Or / Ask / Err, .? early return, exception capture, Resource and Tidy, caches, loops
  └─ maths                  numerics, unsigned and packed types, time, random, hashing, statistics, fitting, geometry, colour
       ├─ loom              Fu futures, Go/Chan channels, Munch actors, Percolate batches, SplitDeque  (basics, flow, maths)
       ├─ thyme             Thyme benchmarks, Parsley in-situ profiler                                (basics, flow, maths)
       └─ eio               paths and files, Xsv, Grok text parsing, command lines, streams, sockets  (flow, maths)
            ├─ jsaun        JSON                                                                       (basics, flow, maths, eio)
            ├─ twodee       static 2D plotting                                                         (basics, flow, maths, eio)
            └─ alien        protobuf and gRPC, wire codec plus schema parser and code generator        (basics, flow, maths, loom, eio)
foundation = basics flow maths loom thyme eio jsaun        all = foundation twodee alien
```

**basics** is the ground floor.  It has the array verbs (`select`, `where`, `alter`, and the rest) with ranges
measured from either end, `Iv` intervals, `Mu` for a mutable container and `Atom` for atomic access, and the `say`
interpolator with `MkStr`.  It also has stable inlined sorting, `NewType` opaque types, labels, tuple operations, `Mem`
off-heap arrays, and the `T ~ x ==== y` test harness.  Read GUIDE-basics.md before writing a loop over an array, a mutable counter, a string builder, a sort, a wrapper
type, or a test.

**flow** is control flow: the ways execution leaves a block early and the ways data gets computed along the way.
Much of that is about things that might not work, so it has `X Or Y` with a favored branch that has no wrapper,
`Ask[A]` for the `Err` case, `.?` to return the failure early, `nice{}` and `safe{}` to capture exceptions, and
`attempt`/`default` fallback chains.  The rest is `Resource`, `Tidy`, and `defer` for releasing what was acquired,
`Lazy`, `Worm`, `Soft`, and `Hold` for computing a value once or on demand, and a few loop shapes.
Read GUIDE-flow.md before writing any function that can fail, opens or closes anything, catches an exception, or
caches a value.

**maths** is the numeric toolbox: extension methods that replace `scala.math`, unsigned and packed integers, `Hex`,
`NanoDuration` and friends for time, `Prng` random numbers, hashing, statistics and histograms, sketches, robust
fits, root finding, vectors and matrices, colour spaces, number parsing and printing, and human-friendly string
ordering.  Read GUIDE-maths.md any time you'd reach for `scala.math`, `java.util.Random`, `System.nanoTime`,
`java.time`, or would hand-roll a hash, a histogram, or a fit.

**loom** is concurrency on virtual threads: `Fu` futures that carry `Ask` and support `.?`, `Sync` locks,
`Go`/`Chan` channels with a persistent select loop, `Munch` supervised actors, `Percolate` work-sharing batches, and
the `SplitDeque` behind them.  Read GUIDE-loom.md before starting a thread, waiting on anything, sharing mutable
state, or building a pipeline.

**eio** is input and output: `Path` extensions (`slurp`, `gulp`, `write`, atomic replace and delete), `Csv`/`Tsv`,
the `Grok` direct-mode text parser, `Cleasy` command-line parsing, stream and buffer adaptors, hex, number parsing
verbs on strings, and file-descriptor level sockets and shared memory.  Read GUIDE-eio.md when touching files, paths,
CSV, byte streams, sockets, command-line arguments, or parsing text.

**jsaun** is high-performance JSON: `Json` values (`Jobj`, `Jarr`, `Jnum`, `Jstr`, `Jbool`, `Jnull`), a format-preserving parser,
pretty and compact output, and `FromJson`/`Jsonize` typeclasses for your own types.  Read GUIDE-jsaun.md for any
JSON.

**twodee** is static 2D plotting in the Makie style: a figure is data plus a `Look`, rendered to SVG or a Java2D
window or PNG.  Read GUIDE_EXTRA-twodee.md when making a chart or figure.

**alien** is the process boundary: a protobuf wire codec over arrays or `Mem`, a proto3 schema parser and linker,
`PbGen` code generation, and a gRPC surface.  Read GUIDE_EXTRA-alien.md when talking to another process or language.

**thyme** is measurement: `Thyme` nanobenchmarks with warmup and statistics, and `Parsley` for A/B latency
comparisons inside a running program.  Read GUIDE-thyme.md when you need to know how fast something is.

## Vocabulary

The module guides use these words without stopping to define them.

- **Or, favored, disfavored.**  `X Or Y` is a two-branch value.  `Is(x)` is the favored branch, usually the success,
  and is normally not boxed; `Alt(y)` is the disfavored branch.  Everything is done through extension methods
  (`fold`, `map`, `getOrElse`, `exists`, `.?`, and so on), never by pattern matching or casting.
- **Ask, Err.**  `Ask[A]` is `A Or Err`.  `Err` is the one error type: it can hold a message, a throwable, or several
  errors, and it can add context.
- **`.?` and jumps.**  `.?` on an `Or`, `Option`, `Try`, `Either`, or NaN-able `Double` gives the favored value or
  leaves the enclosing `Or.Ret:`, `Ask:`, or `boundary` with the disfavored one.  It compiles to a jump when the
  boundary is in the same method, so it costs nothing; only across methods does it throw a stackless exception.
  `quit` and `skip` in `shortcut` are the same mechanism.
- **nice, safe, cope.**  Blocks that run code and turn a thrown exception into a disfavored branch: `Or Err`,
  `Or Throwable`, or `Or E` through a `Cope[E]`.  The "nice" family is the `Err`-typed one.
- **guard, release, Tidy.**  A `Tidy[A]` is how to clean up an `A`.  In `Resource.assemble`, a guard is a value that
  will be torn down unless it is released, and its fate is decided in the one expression that consumes it.
- **opaque type, NewType, Translucent.**  A compile-time-only wrapper with no runtime cost.  `NewType[A]` makes one in
  a line; `Translucent[O, I]` is the witness that `O` is really an `I`, which lets generic code such as `Mu`, `Atom`,
  `.copy`, and `Mem.As` see through it.
- **inline.**  kse3 marks hot methods `inline` so that lambdas vanish and primitives never box.  The price is that a
  type parameter must be concrete where such a method is used, and that a method with many such calls grows large.
- **`__`.**  The inline identity function.  `expr __ Unit` discards a value on purpose, which is how kse3 satisfies its
  non-Unit-statement warning.
- **interval.**  `Iv(i0, iN)` is half-open, `i0` included and `iN` not.  `End` is the last index and `Start` the first,
  so `1 to End-1` is a range measured from both ends; `Iv.X` is the family of such relative ranges.
- **Mem.**  An array of primitives that may live off-heap, with the array verbs and `Long` indices.  `Mem.As` holds an
  opaque type, `Mem.AoS` packed records.
- **durations.**  `NanoDuration` is a `Long` of nanoseconds that saturates instead of overflowing; `DoubleDuration` is
  seconds in a `Double`.  `.nano` converts a `java.time.Duration` safely; `toNanos` throws past 292 years.

## If you catch yourself writing...

Plain-Scala habits and what kse3 has instead.  A section reference is `GUIDE-*module*.md § Section`.

| If you catch yourself writing | Use instead | See |
| --- | --- | --- |
| `scala.math.sqrt(x)`, `math.abs(x)` | `java.lang.Math` imported as `jm`, or `x.sqrt`, `x.abs` from kse.maths | GUIDE-maths.md § Numbers |
| `Either[E, A]`, `Try[A]`, or `Option[A]` meaning "value or failure" | `A Or E`, `Ask[A]`; keep `Option` for things that are genuinely optional | GUIDE-flow.md § Or |
| `x match { case Is(v) => ... }`, `case Alt(e)`, or a cast on an `Or` | `x.fold(good)(bad)`, `map`, `getOrElse`, `.?` | GUIDE-flow.md § Or |
| `opt.get`, `isDefined` ladders, `flatMap` chains on `Either` | an `Or.Ret:` or `Ask:` block with `.?` | GUIDE-flow.md § Early return |
| `try { ... } catch { case e: Throwable => ... }` | `nice{ ... }` or `safe{ ... }`; if you must catch `Throwable`, test `catchable` | GUIDE-flow.md § Catching exceptions |
| `try ... finally x.close()` | `Resource`, `Tidy`, or `Resource.assemble` when several things must be torn down together | GUIDE-flow.md § Resources |
| a `null`-checked cache field, or a `lazy val` you wish you could pass around | `Lazy`, `Worm`, `Soft`, `Hold` | GUIDE-flow.md § Cached values |
| `identity` | `__` | GUIDE-basics.md § Inline glue |
| `val _ = expr` | `expr __ Unit` | GUIDE-basics.md § Inline glue |
| `IArray[A]` | `Array[A]` handed out by reference and not mutated afterwards | GUIDE-basics.md § Arrays with ranges |
| `A | Null` meaning "maybe absent" | an empty value (a zero-length array) or `A Or Unit` | GUIDE-flow.md § Or |
| `xs.slice(...)`, `xs.zipWithIndex.filter(...)`, `xs.map` on an array | `select`, `where`, `copyWith` | GUIDE-basics.md § Arrays with ranges |
| `var i = 0; while i < n do` just to count | `n.times`, `n.visit`, `n.unfold` | GUIDE-basics.md § Counting and traversing |
| `new StringBuilder`, or `s"$n item${if n == 1 then "" else "s"}"` | `MkStr`, `say"$n# item//s#"` | GUIDE-basics.md § Strings |
| `case class Meters(v: Double) extends AnyVal` | `object Meters extends NewType[Double]` | GUIDE-basics.md § Opaque types |
| `Array(0)` or a captured `var` as a mutable cell; `new AtomicLong` | `Mu`, `Atom`, `Atom.Count` | GUIDE-basics.md § Mu and Atom |
| `MemorySegment` or `ByteBuffer` by hand | `Mem` | GUIDE-basics.md § Mem |
| `duration.toNanos`, `duration.toMillis` | `duration.nano`, then the `NanoDuration` accessors | GUIDE-maths.md § Time |
| `System.nanoTime` arithmetic, `java.time` arithmetic | `NanoDuration`, `NanoInstant`, `Now`, `Tic` | GUIDE-maths.md § Time |
| `scala.util.Random`, `java.util.Random` | `Prng()` | GUIDE-maths.md § Random numbers |
| `latch.await()`, `future.get()`, `thread.join()` with no timeout | the same call with the bound stated, `NanoDuration.MaxValue` or `1e9.days` if it truly is forever | GUIDE-loom.md § Sync and waiting |
| `new Thread`, `ExecutorService`, `scala.concurrent.Future` | `Fu`, or `Go`/`Chan`, or `Munch` | GUIDE-loom.md § Fu |
| `synchronized`, `ReentrantLock` | `Sync` for a lock, `Atom` for a single value | GUIDE-loom.md § Sync and waiting |
| `Files.readAllLines(p)`, `Files.write(p, ...)` | `p.slurp`, `p.gulp`, `p.write`, `p.atomically.write` | GUIDE-eio.md § Paths |
| `line.split(",")` | `Csv.read`, `Xsv` | GUIDE-eio.md § Xsv |
| `s.toInt` inside a `try`, `Try(s.toDouble)` | the `parse` verbs on `String`, which give an `Ask` or use `.?` | GUIDE-eio.md § Parsing |
| a regex with capture groups, then `toInt` on each | `Grok` | GUIDE-eio.md § Grok |
| a `while` over `args` with a `match` inside | `Cleasy` | GUIDE-eio.md § Command lines |
| a `read`/`write` loop with a buffer, `new ByteArrayInputStream(bytes)` | `in.sendTo(out)`, `bytes.input()` | GUIDE-eio.md § Streams and buffers |
| `f"$b%02x"` in a loop, `new String(bytes, "UTF-8")` | `bytes.stringEncodeHex`, `bytes.utf8`, `s.bytes` | GUIDE-eio.md § Text and encodings |
| Jackson, uJson, circe | `Json.parse`, `Jobj`, `derives Jsonize, FromJson` | GUIDE-jsaun.md § Parsing and access |
| reading a JSON config, changing one value, and writing it back reformatted | `Json.M.parseFmt`, edit, `print` | GUIDE-jsaun.md § Format-preserving edits |
| `System.nanoTime` around a loop to see how fast it is | `Thyme().bench{ ... }`, `benchOff` to compare two | GUIDE-thyme.md § Thyme |
| `nanoTime` and log lines in production code to find the slow branch | `Parsley` in a companion, `time{}` and `timeOff` | GUIDE-thyme.md § Parsley |
| a CSV opened in a notebook, matplotlib, JFreeChart | `Fig: f => ...`, then `svg` or `png` | GUIDE_EXTRA-twodee.md § Figures |
| protoc and ScalaPB, or a message class kept in step with a `.proto` by hand | `Proto.read`, `PbGen.generate`, `Msg.parse`, `msg.toBytes` | GUIDE_EXTRA-alien.md § Schemas and generated bindings |
| a `StreamObserver`, or `sys.exit` at the end of a gRPC main | `Grpc.Service`, `Grpc.call`, `Grpc.loopback` | GUIDE_EXTRA-alien.md § Grpc |
