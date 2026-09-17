# kse3 flow

The flow module is about control flow: how execution gets from here to there, and how data gets computed along the
way.  Scala's own tools are `if`, `match`, `try`, `return`, and lazy vals.  kse3 adds the ones that turn out to be
missing once code has to leave early, carry a failure as a value, always release what it acquired, try one thing and
then another, or compute something only once and only when needed.  Much of that is about things that might not
work, so the result type `Or` and its early return `.?` come first; the rest is about resources, deferred work,
caches, and loops.

`import kse.flow.{given, *}` brings in everything below.  You'll nearly always want `import kse.basics.{given, *}`
alongside it.

<!-- guide examples: flow/test/src/GuideExamples.scala -->
Every code example below is compiled and run from `flow/test/src/GuideExamples.scala`; `mill guides.run` verifies that
the two copies are identical.

## What's here

- **Or**: `X Or Y`, a two-branch result whose favored side has no wrapper; read through `fold`, `map`, `getOrElse`, `||`, never by pattern matching.
- **Ask and Err**: `Ask[A]` is `A Or Err`; `Err` wraps a message, an exception, or several errors, and takes context.
- **Early return**: `.?` inside `Or.Ret:` or `Ask:` hands back the failure and keeps the success path flat.
- **Catching exceptions**: `nice{}`, `safe{}`, `ratchet`, and friends turn a throw into a value.
- **Fallback chains**: `attempt: ... .attempt: ... .default:` tries alternatives in order.
- **Resources**: `Resource(acquire)(tidy)(use)` and `defer` release what was acquired, whichever way the block ends.
- **Owning a resource**: `Tidy.Later` for one owner without a lexical scope, `Tidy.Lease` for concurrent borrowers.
- **Assembling resources**: `Resource.assemble` with `guard`, `temp`, and `release` builds a thing from several acquisitions, cleaning up on any failure.
- **Cached values**: `Lazy`, `Worm`, `Soft`, and the reactive `Hold` control when a value is computed and recomputed.
- **Loops**: `cFor`, `iFor`, `loop`, and `escape`, so that a loop's shape has a name instead of a hand-kept counter or flag.
- **Many results**: `valid`, `errors`, `validMap`, `collectThem` for a collection of `Or`s.

## Or

`X Or Y` holds either an `X`, the favored branch made with `Is(x)`, or a `Y`, the disfavored branch made with
`Alt(y)`.  It is like `Either` with three differences.  The favored value has no wrapper object around it, so a
success allocates nothing beyond the value.  The method set is richer and speaks of favored and disfavored rather
than left and right.  And you never pattern-match on one: the extension methods (`fold`, `map`, `mapAlt`, `flatMap`,
`getOrElse`, `exists`, `forall`, `foreach`, `isIs`, `isAlt`) are how the branches are read, and a `case Is(x)`,
`case Alt(y)`, or a cast would box or break the representation.

**Reach for this when** you'd write `Either[E, A]`, an `Option[A]` that means "value or failure", or return `null`
or `-1` to say something went wrong.

Plain Scala:

<!-- guide: or.plain -->
```scala
def parsePlain(s: String): Either[String, Int] =
  if s.nonEmpty && s.forall(_.isDigit) then Right(s.toInt) else Left(s"not a number: $s")

def scalePlain(s: String, record: Int => Unit, log: String => Unit): (Int, Boolean) =
  val x = parsePlain(s).map(_ * 100)
  val v = x.getOrElse(-1)
  val ok = x.exists(_ > 500)
  x match
    case Right(n) => record(n)
    case Left(e)  => log(e)
  (v, ok)
```

kse3:

<!-- guide: or.kse3 -->
```scala
def parse(s: String): Int Or String =
  if s.nonEmpty && s.forall(_.isDigit) then Is(s.toInt) else Alt(s"not a number: $s")

def scale(s: String, record: Int => Unit, log: String => Unit): (Int, Boolean) =
  val x = parse(s).map(_ * 100)
  val v = x.getOrElse(_ => -1)
  val ok = x.exists(_ > 500)
  x.fold(record)(log)
  (v, ok)
```

`getOrElse` takes a function of the disfavored value, not a bare default.  `x || y` keeps `x` if it is favored and
otherwise evaluates `y`.  `Or.from(option)` gives `A Or Unit`; `Or.from(either)` and `Or.from(try)` do what you'd
expect; `x.orAlt[E]` and `x.orIs[A]` wrap a plain value while naming the other branch's type.  `reject{ case ... }`
moves favored values that match into the disfavored branch and `rescue` does the reverse.  A collection of `Or`s
splits with `collectIs`, `collectAlt`, or `collectThem`.

Don't reach into `Is`, `IsBox`, or `IsJust`: an `Or` nested in an `Or` has representation rules that the extension
methods respect and a cast does not.

Full API: `flow/src/Or.scala`; the `extension [X, Y](or: Or[X, Y])` block is the main event, and `extension [A](a: A)`
near the end has the wrappers.

## Ask and Err

`Ask[A]` is `A Or Err`, and it is what most kse3 functions that can fail return.  `Err` is the one error type:
`Err("message")`, `Err(throwable)`, or `Err(e1, e2)("what they have in common")` to gather several.  It stays cheap
(a plain message is just the `String` underneath) and it prints well: an `Err` made from an exception carries a
compact trace, and context added with `+#` stacks up as indented lines.

**Reach for these when** you'd invent a per-project error type, thread `Either[Throwable, A]` around, or catch an
exception only to rethrow it with a better message.

Plain Scala:

<!-- guide: ask.plain -->
```scala
def portPlain(s: String): Either[String, Int] =
  try
    val n = s.toInt
    if n < 1 || n > 65535 then Left(s"port out of range: $n") else Right(n)
  catch
    case e: NumberFormatException => Left(s"bad port: ${e.getMessage}")

def configPortPlain(s: String): Either[String, Int] =
  portPlain(s).left.map(e => s"while reading the server config: $e")
```

kse3:

<!-- guide: ask.kse3 -->
```scala
def port(s: String): Ask[Int] = Ask:
  val n = s.toInt                                        // an exception becomes the Err
  if n < 1 || n > 65535 then Err ?# s"port out of range: $n"
  n

def configPort(s: String): Ask[Int] =
  port(s).mapAlt(_ +# "while reading the server config")
```

Inside an `Ask:` block a thrown exception becomes the `Err`, and `Err ?# "message"` leaves at once with a new one.
On the way out, `mapAlt(_ +# "context")` adds a line above the existing error, so a chain of them reads as a trace of
intentions.  `e.toThrowable` goes the other way when a Java API wants an exception, and `e.toss` throws it.
`x.altCase{ case bad => "why" }` turns particular values into failures.

Full API: `flow/src/Err.scala`.

## Early return

`.?` on an `Or` gives the favored value, or leaves the enclosing block with the disfavored one.  The blocks are
`Or.Ret:` (for any `X Or Y`; the body's value is wrapped in `Is`), `Or.FlatRet:` (the body's value is already an
`Or`), and for `Ask` the shorter `Ask:` and `Ask.flat:`, which also turn a thrown exception into the `Err`.  `.?`
works on `Option`, `Either`, `Try`, and iterators too, and on a `Double` inside `calculate:`, where a NaN leaves
early.  It is a jump when the block is in the same method, so it costs nothing.

**Reach for this when** you'd nest `flatMap`s, write a `for`/`yield` over `Either`s, put a `return` inside a loop, or
check `isDefined` before `.get`.

Plain Scala:

<!-- guide: early.plain -->
```scala
def sumPlain(a: String, b: String): Either[String, Int] =
  for
    x <- parsePlain(a)
    y <- parsePlain(b)
  yield x + y

def totalPlain(items: Array[String], bonus: Option[Int]): Either[String, Int] =
  var t = 0
  var i = 0
  while i < items.length do
    parsePlain(items(i)) match
      case Right(n) => t += n
      case Left(e)  => return Left(e)
    i += 1
  bonus match
    case Some(b) => Right(t + b)
    case None    => Left("bonus is required")
```

kse3:

<!-- guide: early.kse3 -->
```scala
def sum(a: String, b: String): Int Or String = Or.Ret:
  parse(a).? + parse(b).?

def total(items: Array[String], bonus: Option[Int]): Ask[Int] = Ask:
  var t = 0
  items.use(): s =>
    t += parse(s).?                                      // a String error leaves as an Err
  t + bonus.?#("bonus is required")
```

Inside `Ask:`, a plain `String` error (from an `Int Or String`, say) can leave with `.?` directly and arrives as an
`Err`.  `.?#("context")` adds context on the way out, `.?+(f)` remaps the error, and `Err ?# "message"` leaves
without a value to test.  For side-effecting code with no result there is `escape:` with `escape.when_?(cond)`, and
for loops `loop:` with `loop.stop_?(cond)`.

Full API: `flow/src/Flow.scala` for `.?` and the blocks; `flow/src/Err.scala` for `Ask:`.

## Catching exceptions

`nice{ ... }` runs a block and gives an `Ask[A]`, with the exception wrapped in the `Err`.  `safe{ ... }` gives
`A Or Throwable` with the exception itself.  `ratchet(default)(_ => ...)` gives the block's value or the default, and
nests to keep partial progress.  None of them catches what should never be caught (VM errors, interrupts, control-flow
jumps); `threadnice` and `threadsafe` catch the control-flow ones as well, for the top of a thread.

**Reach for these when** you'd write `try ... catch { case e: Throwable => ... }` or `Try(...).toOption`.

Plain Scala:

<!-- guide: catching.plain -->
```scala
val n = try s.toInt catch { case _: NumberFormatException => -1 }
val r = try Right(risky()) catch { case e: Exception => Left(e) }
```

kse3:

<!-- guide: catching.kse3 -->
```scala
val n = ratchet(-1)(_ => s.toInt)
val r = safe{ risky() }                                // Int Or Throwable
val a = nice{ risky() }                                // Ask[Int]: the exception wrapped in an Err
```

`cope{ ... }` maps the exception through a `Cope[E]` you supply to give `A Or E`.  `catchmatch(expr){ handler }{ f }`
handles the exception and the value with two partial functions in one expression.  `niceMap` on an `Ask` is `map`
with the same exception capture.

Full API: `flow/src/Flow.scala`.

## Fallback chains

`attempt:` runs a block; `.attempt:` after it runs the next block only if the first failed; `.default:` ends the
chain with a value.  Inside a block, `.!` on an `Or`, `Option`, `Either`, or `Try` gives the value or fails the
attempt.  `.safe:` in place of `.attempt:` (and `attempt.safe:` to open) also counts a thrown exception as a failed
attempt.

**Reach for this when** you'd nest `try`/`catch` to try a second parser, or chain `orElse` on `Option`s you first had
to build.

Plain Scala:

<!-- guide: fallback.plain -->
```scala
try s.toInt
catch case _: NumberFormatException =>
  try s.toDouble.toInt
  catch case _: NumberFormatException => 0
```

kse3:

<!-- guide: fallback.kse3 -->
```scala
attempt.safe:
  s.toInt
.safe:
  s.toDouble.toInt
.default:
  0
```

<!-- guide: bang.kse3 -->
```scala
attempt:
  parse(a).! * 2                                       // .! fails this attempt if parse did
.attempt:
  parse(b).!
.default:
  -1
```

Next to `||` on an `Or`, an attempt chain can fail from anywhere inside its block, and the error itself is dropped:
this is for when only the value matters.  When the errors matter, use `Ask` and `||`.

Full API: `flow/src/Flow.scala`, `object attempt` and `Attempt`.

## Resources

`Resource(acquire)(tidy)(use)` acquires a thing, runs `use` on it, and runs `tidy` on it afterwards no matter how
`use` ended: normally, by exception, or by an early return.  `Tidy.closes` is the `tidy` for anything
`AutoCloseable`.  `Resource.nice` takes the acquisition as an `Ask` and gives an `Ask` back, so a failure to acquire,
a failure in use, or a failure to close all come out as the disfavored branch; `Resource.Nice` is the same with `.?`
available inside `use`.

**Reach for these when** you'd write `try ... finally x.close()`, or a `try` whose `catch` exists only to close
something before rethrowing.

Plain Scala:

<!-- guide: resource.plain -->
```scala
val h = Handle.open(name)
try h.read()
finally h.close()
```

kse3:

<!-- guide: resource.kse3 -->
```scala
Resource(Handle.open(name))(_.close())(h => h.read())
```

<!-- guide: resourcenice.kse3 -->
```scala
Resource.nice(nice{ Handle.open(name) })(Tidy.closes)(h => h.read())
```

`Resource.clean` is `nice` plus a JVM shutdown hook for the duration of `use`, so an interrupt signal mid-use still
releases the resource.  An interruption is never turned into a value by any of these: the close still runs, and the
interruption leaves as the exception it is.

For cleanup that isn't tied to one value, `defer(later)(body)` runs `later` when `body` ends, like Go's `defer` but
for a block; a `procrastinator:` block collects several `defer`s and runs them in reverse order at its end, and
`procrastinator.nice:` also gathers whatever they throw into one `Err`.

<!-- guide: defer.kse3 -->
```scala
defer(log("done")):
  log("working")
  42
```

Full API: `flow/src/Resource.scala` for `Resource` and `Tidy`; `flow/src/Flow.scala` for `defer` and `procrastinator`.

## Owning a resource

Sometimes a resource outlives any block: a connection kept for the life of a server, a mapping handed to callers.
`Resource.closedLater(acquire)(tidy)` gives a `Tidy.Later`, the sole owner: `later.op(f)` and `later.use(f)` run
`f` on the resource, `later.close()` releases it, and if nobody ever calls `close`, JVM shutdown or garbage
collection does.  `Resource.leased(acquire)(tidy)` gives a `Tidy.Lease` for a resource that concurrent callers
borrow: each `op` is a counted borrow, `close()` refuses new ones and runs the cleanup once the last borrow returns,
so a live resource can be retired or replaced without yanking it from a caller mid-use.

**Reach for these when** you'd store a closeable in a field and hope someone remembers to close it, or write a
reference count by hand.

<!-- guide: later.kse3 -->
```scala
val later = Resource.closedLater(Handle.open("cfg"))(Tidy.closes)   // sole owner: closes at close(), shutdown, or GC
val n = later.op(_.read())
later.close()
val lease = Resource.leased(Handle.open("db"))(Tidy.closes)         // shared owner: borrowers use op, use, nice
val m = lease.op(_.read())
lease.close()                                                        // cleanup runs once the last borrow returns
```

Both have `nice(f)` and `flatNice(f)` for `f` with `.?` inside, and `Later` has `opAndClose` and friends for a last
use.  Using either after `close` throws (or yields an `Err` from the `nice` forms).

Full API: `flow/src/Resource.scala`, `Tidy.Later` and `Tidy.Lease`.

## Assembling resources

Building one thing out of several acquisitions is where `try`/`finally` fails you: if the third step throws, the
first two must be undone, but if everything succeeds, they must not be.  `Resource.assemble:` is a block in which
every acquisition is registered with one of two verbs.  `Resource.temp(x)(undo)` is for what the block needs only
while assembling: torn down when the block exits, whichever way.  `Resource.guard(x)(undo)` is for what may be
handed out: torn down if the block fails, and kept only if some later expression consumes it, with `release(g)` to
hand it out as is, `releaseOp(g)(f)` to hand out something built from it, or `delegate` to build a new owner that is
guarded in its turn.  Tear-downs run newest first.

**Reach for this when** a constructor opens two or more things, or you find yourself writing `catch { case e =>
x.close(); throw e }`.

Plain Scala:

<!-- guide: assemble.plain -->
```scala
val x = Handle.open(a)
val y =
  try Handle.open(b)
  catch
    case e: Throwable =>
      x.close()
      throw e
(x, y)
```

kse3:

<!-- guide: assemble.kse3 -->
```scala
Resource.assemble:
  val x = Resource.guard(Handle.open(a))(_.close())
  val y = Resource.guard(Handle.open(b))(_.close())
  Resource.release(x, y)
```

<!-- guide: assembleop.kse3 -->
```scala
Resource.assemble:
  val tmp = Resource.temp(Handle.open("scratch"))(_.close())     // gone when the block exits, whichever way
  val h = Resource.guard(Handle.open(a))(_.close())              // torn down only if we fail from here on
  val k = tmp.read()
  Resource.releaseOp(h)(new Server(_, k))                        // handed out in h's place, once built
```

A guard's fate is decided in exactly one expression: never `release(x)` as a statement whose value is dropped.  A
guard that is never released is torn down even on success, so nothing acquired can be lost; a released guard is
still torn down if the block then fails, so a failed assembly hands out nothing.  The block may return only released
values, singly or as a tuple, and the caller receives them bare.  `Resource.assembleNice:` is the same inside an
`Ask`, with `.?` allowed and failures coming back as an `Err`.

Full API: `flow/src/Resource.scala`, `object Resource` from `assemble` down; the docstring on `assemble` is the full
contract.

## Cached values

Four ways to control when a value gets computed.  `Lazy(expr)` computes once, on first `.value`, and is a value you
can pass around, unlike `lazy val`.  `Worm.of[V]` is a write-once cell: `set` fills it, from any thread, and `get`
reads it or throws.  `Soft(source)(compute)` keeps the result behind a `SoftReference`, so under memory pressure it is
dropped and later recomputed from `source`.  `Hold` is a small reactive graph: `Hold.mutable(x)` is an input,
`hold.map(f)` and `a.mapWith(b)(f)` derive from inputs and recompute only when something upstream changed, and
`trust`, `expireIn`, and `expireIf` bound how long a value is believed.

**Reach for these when** you'd write a `null` check around a cached field, a `@volatile var` with a "computed"
flag, or an invalidation method you have to remember to call.

<!-- guide: cached.kse3 -->
```scala
val once = Lazy(expensive())              // computed on the first .value, then kept
val w = Worm.of[String]                   // write once, from any thread
w.set("eel")
val cfg = Soft("fish.cfg")(load)          // dropped under memory pressure, recomputed from its source
```

<!-- guide: hold.kse3 -->
```scala
val capitals = Hold.mutable(false)
val name = Hold.mutable("Salmon")
val fish = name.mapWith(capitals)((s, c) => if c then s.toUpperCase else s.toLowerCase)
val a = fish.value                        // "salmon"
capitals.set(true)
val b = fish.value                        // "SALMON": an input changed, so it was recomputed
```

`Hold`s are thread-safe and lock along dependency chains only, so they cannot deadlock as long as the graph is
acyclic; a generator that reads another `Hold` makes that a dependency too.

Full API: `flow/src/Cached.scala`.

## Loops

kse3 is implemented with `while` loops, but written with something better wherever it can be: a loop with a
counter, a flag, or an index kept by hand is compact and hard to get wrong once the loop shape has a name, and it is
no slower, because every one of these inlines to the `while` you'd have written.  basics gives you `n.visit` and the
array verbs for the common shapes; flow adds the rest.  `cFor(start)(test)(step)(body)` is C's three-part `for`.
`iFor(iterator)` visits anything iterable with its index, as does `.visit((x, i) => ...)` on an `Iterator`,
`Stepper`, or Java iterator.  `loop:` runs until `loop.stop_?(cond)` (or `proceed_?(cond)`, the opposite test), and
`escape:` is the same for a block that should be able to leave early with `escape.when_?(cond)` or
`unless_?(cond)`.  A literal `true` in `stop_?` or `when_?` is the unconditional exit.

**Reach for these when** a `while` needs a flag variable, a counter you maintain by hand, or a `return` from the
middle.

Plain Scala:

<!-- guide: loops.plain -->
```scala
var x = 1
while x < 100 do
  seen(x)
  x *= 2
var i = 0
for s <- list do
  f(s, i)
  i += 1
```

kse3:

<!-- guide: loops.kse3 -->
```scala
cFor(1)(_ < 100)(_ * 2)(seen)
iFor(list.iterator)(f)
```

<!-- guide: loopbreak.kse3 -->
```scala
loop:
  val line = next()
  loop.stop_?(line == null)
  process(line)
```

`q.drainWith(f)` empties a `java.util.Queue` into `f`.

Full API: `flow/src/Repeat.scala` for `cFor`, `iFor`, `visit`, and `drainWith`; `flow/src/Flow.scala` for `loop` and
`escape`.

## Many results

A collection of `Or`s usually wants one of three things: all the favored values if there is no failure, the
failures, or both halves.  `results.valid` is an `Ask` of all the values or the first error; `results.errors` is the
errors; `collectIs`, `collectAlt`, and `collectThem` split without judging.  `inputs.validMap(f)` maps and stops at
the first error without building the intermediate collection, and `validOrErrors` gives all the values or all the
errors.

**Reach for these when** you'd `foldRight` a list of `Either`s into an `Either` of a list, or `partitionMap`.

Plain Scala:

<!-- guide: many.plain -->
```scala
val results = inputs.map(portPlain)
val all = results.foldRight(Right(Nil): Either[String, List[Int]]): (r, acc) =>
  for x <- r; xs <- acc yield x :: xs
val bad = results.collect{ case Left(e) => e }
```

kse3:

<!-- guide: many.kse3 -->
```scala
val results = inputs.map(port)
val all = results.valid                   // every value, or the first error
val bad = results.errors
val direct = inputs.validMap(port)        // the same as all, without the intermediate list
```

Full API: `flow/src/Data.scala`.
