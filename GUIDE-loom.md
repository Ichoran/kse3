# kse3 loom

The loom module is concurrency on virtual threads.  Threads are cheap now, so the module doesn't try to hide them;
it makes the ways of coordinating them small and hard to misuse.  A `Fu` is a future whose result is an `Ask`, so
`.?` works inside it.  `Go` and `Chan` are Go-style tasks and channels with a select loop and an automatic close
cascade, for pipelines.  `Percolate` runs work that generates more work on a few threads, for batch computation.
`SplitDeque` is the concurrent deque underneath.  `Munch` is a small actor runtime, for when things need names.

`import kse.loom.{given, *}` brings in everything below, and everything here leans on `kse.basics`, `kse.flow`
(`Ask`, `.?`), and `kse.maths` (durations).

<!-- guide examples: loom/test/src/GuideExamples.scala -->
Every code example below is compiled and run from `loom/test/src/GuideExamples.scala`; `mill guides.run` verifies
that the two copies are identical.

## What's here

- **Fu**: a future on a virtual thread whose body can `.?`, whose result is an `Ask`, and that other futures can `.?` on.
- **Sync and waiting**: a lock you use as a block, condition waits that are always bounded, and `Threaded` for a thread with a result.
- **Go and Chan**: tasks that register `put`, `get`, and `into` on channels and then select over them, with channels closing themselves when their writers finish.
- **Channels from any thread**: `send`, `recv`, `close`, and the `RunStatus` that says why not; `ChanN` for bulk; `Source` and `Sink` endpoints.
- **Percolate**: work that produces work, run on a few threads with the main thread always able to do any of it.
- **SplitDeque**: a concurrent deque that moves batches in logarithmic time.
- **Munch**: named, supervised, message-driven entities, when a pipeline is the wrong shape.

## Fu

`Fu{ body }` runs `body` on a virtual thread and gives a `Fu[A]`.  Inside the body you can `.?` on any `Ask`, and
on any other `Fu`, which waits for it; a thrown exception becomes the `Err`.  `await()` blocks for the result as an
`Ask[A]`, `await(timeout)` gives up after a while, and `cancel()` interrupts.  `map` and `flatMap` chain, but the
usual shape is to start everything you can and then `.?` the results in one place, which builds a tree of work
rather than a chain.  An array of futures has `.fu()` for all the values or the first error and `.allFu()` for every
`Ask`.  `Fu.group{ ... }` runs a body whose inner futures are cancelled the moment one fails.

**Reach for this when** you'd write `scala.concurrent.Future` with an `ExecutionContext` and `Await`, or
`CompletableFuture` chains, or start a thread just to run one thing.

Plain Scala:

<!-- guide: fu.plain -->
```scala
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
val a = Future(portPlain(x))
val b = Future(portPlain(y))
val sum = for ea <- a; eb <- b yield for va <- ea; vb <- eb yield va + vb
Await.result(sum, 10.seconds)
```

kse3:

<!-- guide: fu.kse3 -->
```scala
val a = Fu{ port(x).? }                   // a Fu[Int]: the body may leave with an Err via .?
val b = Fu{ port(y).? }
val sum = Fu{ a.? + b.? }                 // waits for both, or carries the first error
sum.await()                               // an Ask[Int]
```

<!-- guide: fuarray.kse3 -->
```scala
val fus = items.map(s => Fu{ port(s).? })
fus.fu().await()                          // every value, or the first error
```

The `Executor` is a given; the default is a virtual-thread-per-task executor, and `Fu.Executor.create()` makes
another if you want isolation.  `Fu.of(ask)` is an already-complete future and `Fu.wrap(cf)` adopts a
`CompletableFuture`.  Don't block inside a `Fu` before you've started everything else you can: `.?` waits.

Full API: `loom/src/Fu.scala`.

## Sync and waiting

`Sync()` is a lock used as a block: `lock { ... }` runs the body holding it, `lock.ifFree { ... }` gives `Alt` if
someone else has it, and `lock.uninterrupted { ... }` ignores interrupts for a short critical section.
`lock.waiter()` is a condition: `w(duration)` waits, `w.!` wakes one waiter and `w.!!!` all of them.  There is no
untimed wait on purpose: a wait takes a `NanoDuration` or `Duration` and saturates at the longest nanoseconds can
express, so "wait forever" is written as a very long time (`1e9.days`) and is visible as such.  `Threaded{ body }`
is a plain thread that has a result: `await()` is an `Ask` of what the body produced or the exception it died with.

**Reach for these when** you'd write `synchronized` with `wait` and `notifyAll`, a `ReentrantLock` with
`try`/`finally`, or a `Thread` plus a shared variable to get an answer out of it.

Plain Scala:

<!-- guide: sync.plain -->
```scala
val lock = new Object
var ready = false
def waitReady(): Unit = lock.synchronized { while !ready do lock.wait() }
def signal(): Unit = lock.synchronized { ready = true; lock.notifyAll() }
```

kse3:

<!-- guide: sync.kse3 -->
```scala
val lock = Sync()
val w = lock.waiter()
var ready = false
def waitReady(): Unit = lock { while !ready do w(1.s_nano) __ Unit }   // every wait is bounded and re-checked
def signal(): Unit = lock { ready = true; w.!!! }
```

<!-- guide: threaded.kse3 -->
```scala
val t = Threaded{ (1 to 100).sum }        // a Thread with a result
t.await()                                 // Ask[Int]: the value, or what the thread failed with
```

For a single shared value, `Atom` (basics) is lighter than a lock.

Full API: `loom/src/Fu.scala`, `Sync`, `Wait`, and `Threaded`.

## Go and Chan

`Go.session { ... }` opens a structured scope; inside it, `Go { ... }` spawns a task.  A task's body runs once to
register what the task does with channels: `ch.onRecv{ v => ... }` or `ch.get{ ... }` to consume, `ch.onSendWhile(cond){ value }`
or `ch.put{ ... }` to produce, `src.into(dst){ f }` to transform one channel into another.  Then the task loops,
servicing whatever is ready; a task with several registrations is a select over them.  `Chan[A](capacity)` is a
bounded channel that counts its writers and closes itself when the last one finishes, so a consumer ends when its
input is drained and closed, and the whole tree finishes without anyone sending a poison pill.  Every task's lambdas
run on its own thread, so a `var` captured in one task needs no synchronization.  `session.await()` is an
`Ask[Unit]`: success, or the first failure, which cancels the rest.

**Reach for these when** you'd wire threads together with `BlockingQueue`s and sentinel values, or when you have a
pipeline, a fan-in, or a fan-out.

Plain Scala:

<!-- guide: gochan.plain -->
```scala
val q = new java.util.concurrent.ArrayBlockingQueue[Int](4)
val total = new java.util.concurrent.atomic.AtomicLong(0)
val producer = new Thread(() => { var i = 0; while i < n do { i += 1; q.put(i) }; q.put(-1) })
val consumer = new Thread(() => { var v = q.take(); while v >= 0 do { total.addAndGet(v): Unit; v = q.take() } })
producer.start()
consumer.start()
producer.join()
consumer.join()
total.get
```

kse3:

<!-- guide: gochan.kse3 -->
```scala
val ch = Chan[Int](4)
val total = Atom(0L)
val h = Go.session: g ?=>
  Go:
    var i = 0
    ch.onSendWhile(i < n){ i += 1; i }     // producer; ch closes by itself when this task ends
  ch.onRecv{ v => total += v }             // consumer; ends once ch is drained and closed
h.await() __ Unit                          // Ask[Unit]: success, or the first failure in the tree
total()
```

<!-- guide: gointo.kse3 -->
```scala
val words = Chan[String](8)
val lengths = Chan[Int](8)
var longest = 0
var count = 0
val h = Go.session: g ?=>
  Go:
    var i = 0
    words.onSendWhile(i < names.length){ i += 1; names(i - 1) }
  Go:
    words.into(lengths)(_.length)          // reads words, writes lengths, closes lengths after
  Go:
    Defer{ count = -count }                // runs when this task ends, however it ends
    Stop.on(longest >= 6)                  // a graceful exit condition, checked each loop
    lengths.onRecv{ n => count += 1; if n > longest then longest = n }
```

The session body is itself a task, so registrations there run on the session's thread.  `Stop()` ends the current
task gracefully, `Stop.on(cond)` does so when a condition holds, and `Defer{ ... }` registers cleanup; both words are
the same in a `Munch` handler.  `h.stop()` asks the whole tree to finish and `h.cancel()` interrupts it.  Inside
`get`, `put`, and `into` handlers, `.?` on an `Ask` fails the task, and a failed task fails the session.  A task that
writes to a channel it doesn't otherwise register declares it with `ch.writing`, so the close cascade counts it.
`Go.x(n){ ... }` spawns `n` identical tasks.

Full API: `loom/src/Go.scala` for tasks and the channel verbs, `loom/src/Orchestrate.scala` for `Stop` and `Defer`.

## Channels from any thread

A `Chan` also works imperatively, from any thread.  `send(x)` blocks while the channel is full and answers a
`RunStatus`: `Okay`, or `Done` if the channel is closed, or `Fail` if it was failed.  `recv()` blocks while empty and
gives `A Or RunStatus`, so the value is the favored branch and the reason it stopped is the other.  `trySend` and
`tryRecv` don't block and can say `Wait`.  `close()` lets the remaining values drain; `fail(err)` poisons the
channel for everyone.  `ChanN` is the same with bulk moves: `sendN` and `recvN` move a slice of an array under one
lock acquisition, and `getFull` waits for a whole chunk.  `Chan.Source` wraps an iterator or array as something a
`Go` task can read, and `Chan.Sink` wraps a builder, collection, or function as something it can write.

**Reach for these when** a thread outside any `Go` session must feed or drain a pipeline, or when items are
small and many and per-item locking would dominate.

<!-- guide: chan.kse3 -->
```scala
val ch = Chan[Int](2)
val summer = Threaded:
  var s = 0
  var more = true
  while more do ch.recv().fold(v => s += v)(_ => more = false)   // the Alt is Wait, Done, or Fail
  s
ch.send(1) __ Unit                         // blocks while full; the RunStatus says Okay or why not
ch.send(2) __ Unit
ch.send(3) __ Unit
ch.close() __ Unit                         // the receiver drains 3 then sees Done
summer.await()
```

Full API: `loom/src/Chan.scala` and `loom/src/ChanN.scala`.

## Percolate

`Percolate` is for CPU-bound work whose shape is only known as it runs: each unit of `Work` does a chunk and
returns the follow-on work it produced, so loops and recursion live in the data.  You subclass it, say how many
worker threads to use, define `newWork()` to produce root work until it answers `Work.Empty`, and call `go()`.  The
engine schedules everything, bounds work in flight with a permit budget, and keeps one invariant above all: every
item can be run by the main thread, so with zero workers the same program still completes.  `Resource` subclasses
describe things only one thread may use at a time (a file, a single-threaded library), served from a per-resource
queue; `Producer` is a resource that makes work from a single-threaded source.

**Reach for this when** a parallel collection or a fixed thread pool doesn't fit because the work isn't known up
front, or because some of it must go through one thread.

<!-- guide: percolate.kse3 -->
```scala
class SumSquares(n: Int, parallelism: Int) extends Percolate(parallelism) {
  val total = Atom(0L)
  private var next = 1
  def setup(): Ask[Unit] = Is.unit
  def teardown(): Ask[Unit] = Is.unit
  def newWork(): Ask[Work] = Is:              // called until it answers Work.Empty
    if next > n then Work.Empty
    else
      val k = next
      next += 1
      new Work() { def work(): Ask[Array[Work]] = { total += k.toLong * k; Is(Work.none) } }
}
```

<!-- guide: percolaterun.kse3 -->
```scala
val p = SumSquares(100, 4)
val timing = p.go()                       // runs to completion on 4 workers plus this thread
```

`go()` gives the wall and busy time as `NanoDuration`s, or the error that stopped the run; `step()` runs one item
on the calling thread for when you want to drive it yourself.  `Percolate.Sort`, `Gather`, and `Aggregate` are
ready-made resources for the common reductions.

Full API: `loom/src/Percolate.scala`; its header comment is the design in full.

## SplitDeque

A concurrent double-ended queue where a batch of any size moves in or out in `O(blockSize * log n)` time, so many
fast producers can feed a consumer that takes chunks, or one producer can feed many consumers.  `pushLeft`,
`pushRight`, `popLeft()`, and `popRight()` are the single-element operations (the pops give `A Or Unit`);
`splitLeft(n)` and `splitRight(n)` move `n` elements out into a `Batch`, a plain non-concurrent deque, and
`spliceLeft` and `spliceRight` move a batch's whole contents back in.  `ChanN` is built on it.

**Reach for this when** a `ConcurrentLinkedDeque` is too slow because every element is touched under contention, or
when you're re-batching between producers and consumers with different natural chunk sizes.

<!-- guide: splitdeque.kse3 -->
```scala
val d = SplitDeque.empty[Int]
100.visit(i => d.pushRight(i))
val first = d.popLeft()                   // Int Or Unit
val batch = d.splitLeft(10)               // the next 10, moved out in O(blockSize * log n)
val moved = batch.length                  // a Batch is a plain deque of its own
d.spliceRight(batch)                      // and back on the other end; the batch is empty again
```

Full API: `loom/src/SplitDeque.scala`.

## Munch

`Munch` is an actor runtime for when the things you're coordinating have identities: a connection, a session, a
device.  A supervisor holds keyed registries of "munchers", each a behavior `M => Unit` running single-threaded over
its own mailbox with private state in closed-over `var`s.  `reg.spawn(key){ behavior }` makes one and gives a
`Ref`; `ref ! msg` sends, and `ref.feed(Msg(_))` sends a request and gives a `Fu` for the reply.  A muncher that
fails consults its own decider (stop, restart, escalate) and doesn't take its siblings down, which is the opposite
of a `Go` session's tree failure and the right thing for independent entities.  This is the least-used part of
loom; for a known pipeline use `Go` and `Chan`, and for work-generates-work use `Percolate`.

<!-- guide: munch.kse3 -->
```scala
val acc = Atom(0L)
val sup = Munch.supervisor()
val adders = sup.registry[String, Int]("adders")
val adder = adders.spawn("main"){ (n: Int) => acc += n }
adder ! 5
adder ! 7
sup.stop().map(_ => acc())                // drains mailboxes, then reports
```

Full API: `loom/src/Munch.scala`; its header comment explains what it adds over `Go` and why.
