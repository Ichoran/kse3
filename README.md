# Kse3

This repository contains the Kerr Standard Extensions to Scala 3.

These are intended to cover everything that is commonly used for general-purpose
programming and general-purpose data analysis, but isn't in the Scala
standard library (or is, but needs improvement).

Kse3 has no pretenses of being an idiomatic toolchain.  It is designed for
high-productivity programming for people who like direct style and good error
handling, and who care about performance.  There is no particular attempt to
take a monadic or other functional approach, except inasmuch as it helps
productivity.  When there is a tradeoff between enabling good user code and
writing "good" library code (DRY, etc.), Kse3 favors the user.  Kse is
supposed to take care of any necessary ugly stuff so you don't have to.

**Warning: kse3 only works on Scala 3.7 and later due to its use of
`scala.util.boundary` and named tuples.  It also assumes at least Java 21.**


## How do I get it?

Only kse3-basics, kse3-flow, kse3-maths and some of kse3-eio (and kse3-testing) are available presently.
You'll need to specify an appropriate version of Scala.  For example, in mill:

```scala
def scalaVersion = "3.7.0"
```

And add at least one of

```scala
ivy"com.github.ichoran::kse3-basics:0.4.0"
ivy"com.github.ichoran::kse3-flow:0.4.0"
ivy"com.github.ichoran::kse3-maths:0.4.0"
ivy"com.github.ichoran::kse3-eio:0.4.0"
```

to try it out.  Or, the scala-cli header equivalent:

```scala
//> using scala 3.7.0
//> using dep com.github.ichoran::kse3-basics:0.4.0
//> using dep com.github.ichoran::kse3-flow:0.4.0
//> using dep com.github.ichoran::kse3-maths:0.4.0
//> using dep com.github.ichoran::kse3-eio:0.4.0
```

Because scala-cli does not by default use the default JVM and does not use Java 21 by default,
you'll typically need to pass `--jvm=21`, or `--jvm=system` if you have 21 installed, when
running scala-cli.

kse3 expects that you will use compiler options to warn against discarding values.  This is
an important safety mechanism when handling errors as values: even if there is no meaningful
return value in case of success, failing to propagate an error in case of failure is rarely
a good idea!  In mill, use

```scala
  def scalacOptions = Seq(
    // Add other options as needed
    "-Wnonunit-statement",
    "-Wvalue-discard",
  )
```

and in scala-cli

```scala
//> using options -Wvalue-discard -Wnonunit-statement
// Add other options as needed
```

If you use some other build system, you can probably figure out from the above what you need.

Then in your code,

```scala
import kse.basics.{given, *}
import kse.basics.intervals.{given, *}
import kse.basics.labels.{given, *}
import kse.flow.{given, *}
import kse.maths.{given, *}
import kse.maths.packed.{given, *}
import kse.eio.{given, *}
```

and you have everything available.

(Note: I don't test without the `given` imports.  They may work, as Kse3 generally puts
givens in places where they'll be automatically found.  No givens are defined that
aren't essential for functionality, so if they're there, you want to import them.)

(Note 2: I find intervals indispensible but labels and packed maths only occasionally
useful, so you may not want to bother with lines 3 and 6.)

#### What about infix?

Kse3 is intended to be used with a liberally infix style.  Although methods will sometimes be explicitly
notated `infix`, the recommendation is to turn off infix warnings altogether.  In Mill, this would be
something like

```scala
  def scalacOptions = Seq(
    // Add other options as needed
    "-Wconf:msg=is not declared infix:s"
  )
```

If you use scala-cli headers, where spaces don't parse as part of the message, you can use instead

```scala
//> using options -Wconf:msg=is.not.declared.infix:s
```

(Note that this has to go above the MainClass declaration if you have one.)

#### Is Kse3 stable?  Binary compatible?

Kse3 is intended as something of an **incubator for good ideas**, so stability is not a primary goal for
any part of the library that is under active development.  Nonetheless, eventually the well-designed parts
will be stable.

Binary compatibility is not likely to ever be an explicit goal, because Kse3 makes very heavy use of inlining.
Because you have to recompile all the time anyway, there is no reason to try to maintain binary compatibility.



#### How do I pronounce it?

I don't hear words when I read them, so I don't have any strong opinion.  I think the most conventional way to pronounce something with that spelling would be Ks-ee, so let's go with that?


## Subprojects

The structure of packages is still being determined.  In general, they will
be `kse.` something--a name collision with the original Kse for Scala 2, but
you shouldn't use that with Scala 3 because Kse is actually still on Scala
2.12.  So it's all good.

### kse.basics

The basics module has no dependencies itself.  In mill, add the dependency

```scala
ivy"com.github.ichoran::kse3-basics:0.4.0"
```

and in your code,

```scala
import kse.basics.*
```

You have access to full-speed inlined pipe (to transform values) and tap (to act on them but pass forward the original):

```scala
val m = (1 + 2*3).pipe(x => x*x)
val n = (1 + 2*3).tap(println) + 1 // prints: 7
println(n)                         // prints: 8
println(m)                         // prints: 49
```

Unlike the equivalents in the Scala standard library, these are inlined and thus can be used without a loss in speed.
(You also get `x.effect(f)` if you just want to cause some side-effect not pass along any value.)


There are standard mutable containers for primitive and object types for counting, callbacks, and other such use:

```scala
def nextEven(m: Mu[Int]): Unit =
  m.op(i => if i % 2 == 0 then i + 2 else i + 1)

val count = Mu(1)
for (i <- 1 to 5) nextEven(count)
println(count())   // prints: 10


def tokenCount(s: String, tokens: Option[Mu[Array[String]]]): Int =
  if s.isEmpty then 0
  else
    val tok = s.split("\\s+")
    tokens.foreach(_ := tok)
    tok.length

val tok = Mu(Array.empty[String])
println(tokenCount("minnow salmon bass eel", Some(tok)))  // prints: 4
println(tok().mkString)                                   // prints: minnowsalmonbasseel
```

Plus there are handy methods provided on tuples, wrappers to suppress printing or use identity hash codes, and a bunch of methods that add basic ranged functionality to arrays with full hand-rolled speed by virtue of extensive use of inlines and custom interval and endpoint handling.

```scala
import kse.basics.intervals.*
val a = Array(1, 2, 3, 4, 5)
val b = a.select(_ % 2 == 1)  // Array(1, 3, 5)
b(1 to End) = a  // b is now Array(1, 1, 2)
b(End - 1) = 4   // b is now Array(1, 4, 2)
b.inject(a, 2)   // a is now Array(1, 2, 1, 4, 2)
var n = 0
a.use()(n += _)
println(n)      // Prints 10
```

There is also a universal ultra-lightweight type-tagging system using string constants to refine types like `String \ "name"`.  If
you have a tuple that is entirely labeled, you can convert it into a named tuple with `.labelsToNames`; if you have a named tuple, you can
convert it to a tuple of labeled values with `.namesToLabels`.  Even easier, if you have a named 1-tuple literal, `.lb` produces the
label-tagged version, and if you have a tagged type. `.nt` is the corresponding named 1-tuple.

There are a variety of helper functions for named and regular tuples; all work at least up to 22 entries, and some work for arbitrary sizes.
This includes a tiny lens facility that allows focus on named tuples by field name.

```scala
import kse.basics.labels.*

val nup = (a = "eel", b = true, c = 5.5)
var v = nup.lens["c"].pluck      // Double \ "c"
v = (c = 25.0).lb
val np2 = nup replace 'e' \ "a"  // Or replace (a = 'e').lb
```

Named tuples stress convenience.  However, the label-tagged version is meant for safety: they enforce identity by requiring you
to give the name (string) after `~` in order to recover the value.  However, if you tag types with unique names, you can
use `conjure("name")` instead of `summon[Type \ "name"]`!

Use it whenever identity is really important, but types aren't specific enough.  If you want your tagged type
to be a subtype or supertype of the original type--less secure, but there are cases where it is convenient--then
use `\>` or `\<` respectively (the mnemonic being `x \> "label"` means that `x` itself is bigger than with the label).
You can also convert between the three types by using `newtyped`, `subtyped`, and `supertyped`.

There's an extra-easy interface to atomic types, too (plus counters and toggles).  Use `val a = Atom(value)` to declare an atomic value,
then get it with `a()`, set it with `a := newValue`, and update it atomically (using CAS operations, so the
operation may be repeated when under contention) using `a.opAndGet(x => f(x))` and other variants.

```scala
val a = Atom("salmon eel")
val eel = a.opAndGet(_.split(' ').head)  // Atomic!
println(eel == a())   // prints true
```

The interface to `Atom` is nearly identical to the interface to `Mu`, in order to aid refactoring from single-threaded to multi-threaded computations.

See the test suite, or package Scaladoc, for more examples of what you could do with `kse.basics`!


### kse.flow

The flow module depends only on kse.basics.  In mill, add the dependency

```scala
ivy"com.github.ichoran::kse3-flow:0.4.0"
```

and in your code,

```scala
import kse.flow.*
```

Then you have access to the left-biased unboxed sum type `Or`:

```scala
def little(i: Int): Int Or String =
  if i > -100 && i < 100 then Is(i)
  else Alt(s"$i is not little")

val x = little(12)
val y = little(999)

println(x)             // prints: 12
println(y)             // prints: Alt(999 is not little)
y.foreachAlt(println)  // prints: 999 is not little
x.foreachAlt(println)  // does not print anything

val z = x.map(_ * -3)
val w = z.reject{
  case i if i < 0 => "No negatives please"
}

println(z)             // prints: -36
println(w)             // prints: Alt("No negatives please")
```

You have Rust-style `.?` early-exit error handling for `Or` (and `Option` and `Either` and `Try`, but once you have `Or`, why ever use those?), and `safe{ }` to intercept exceptions and pack the result into `A Or Throwable`.

Note that `.?` does not need to exit an entire method; it will instead exit an `Or.Ret:` block (there are other variants available, but this is the most general).  To exit a method, wrap the entire method body in an `Or.Ret:` block.

```scala
def reverseParse(s: String): Int Or String =
    safe{ s.reverse.toInt }.mapAlt(_ => s"$s is not a number in reverse")

def favorite(i: Int): Int Or String = Or.Ret:
  val x = little(i).?  // return bad case early, keep going with good value
  x * reverseParse(x.toString).?

val a = favorite(12)
val b = favorite(999)
val c = favorite(-3)
println(a)   // prints: 252
println(b)   // prints: Alt(999 is not little)
println(c)   // prints: Alt(-3 is not a number in reverse)
```

If you don't care about what the bad values are (usually you should!), you also have `.!` that can be used inside a single or chained attempt block, but otherwise works like `.?`--you can use `true_!` to declare that something must be true (or the attempt fails), or `false_!`:

```scala
val opt = Option(42)
val number =
  attempt:
    val a = favorite(999).!  // fails here
    (a < 30).true_!          // this would fail too
    2 * a
  .attempt:
    val a = favorite(12).!   // succeeds, giving 21
    val b = opt.!            // gets 42 from option
    (a >= b).true_!          // test passes
    a * b
  .default:
    -1                       // If everything failed, would give this
```

`.!` can be used on an `Or`, `Option`, `Either`, `Try`, an empty iterator (if non-empty, it will get the next item), or a `Boolean` test which is false.  It will return the success branch if available, or will discard the error value and proceed to the next attempt or default block if it's   When a `Boolean` test succeeds, you can chain `&&` afterwards to run the next computation.  If you want `attempt` to catch exceptions too, use `attempt.safe` and chain with `.safe` instead of `.attempt`.  If you want to do partial pattern matches. start with `x.attemptCase:` or as an alternative use `.orCase(x):` followed by your case statements.

There are inlined index-based loops, as fast as `while` but without risk of an indexing error:

```scala
val list = List("salmon", "herring", "perch")
iFor(list.iterator){ (s, i) => println(s*i) }  // prints newline, then "herring", then "perchperch"
```

Error handling is greatly simplified and streamlined by using an `Err` type that is either a simple string message, or is
a wrapped exception.  (Custom `Err` types can also be defined by extending `ErrType`.)  Any operation that may fail should
return `A Or Err`, where `A` is the success type.  Use `Err.break("message")` or `Err ?# "message"` to exit with a lightweight
error message.  Use `foo().?` to propagate errors from an error-prone method that you call.  Use `foo() ?# "message"` to
propagate the error with an explanation about the context.  To catch exceptions and pack them as an `Err`, use `nice{ baz() }`.
Because `A Or Err` is a very common pattern, the type `Ask[A]` is aliased to it.  If you use `Ask:` instead of `Or.Ret:` any
exceptions that occur within the block will also be caught and packaged.  

```scala
def positiveInt(s: String): Ask[Int] = Ask:
  var ndigits = 0
  s.use(){ c => if c.isDigit then ndigits += 1 }
  if ndigits < s.length then Err ?# s"${s.length - ndigits} characters are not digits"
  val n = nice{ s.toInt }.?
  if n == 0 then Err ?# "Zero not allowed"
  n

val pi1 = positiveInt("9815")  // Is(9815)
val pi2 = positiveInt("45wx")  // Alt(Err("2 characters are not digits"))
val pi3 = positiveInt("123456789013245")  // Exception packed in Err
```

If you have collected your results in an `Array`, you can use `??` to get the values, or return to a boundary with all the errors packed in an `ErrType.Many`.

Because `Or` is unboxed, this style of error-handling has particularly low overhead for the success case, and if
user-defined error strings or a custom `ErrType` are used rather than wrapping exceptions, the failure case also
has much higher performance than the alternative of throwing exceptions.

In addition to these features, kse.flow provides a variety of other nice things that you can find by
perusing the ScalaDoc, the unit tests, or the code.

### kse.maths

This exists but presently is undocumented.  Feel free to look through the unit tests to see what can be done, however!

A couple of highlights include unsigned primitives (`UInt`, `ULong`, and the like) which do math as they should (use `.unsigned` and `.signed` to
convert back and forth (`.u` and `.s` if you want less typing), and `.pr` to get a string for the unsigned verson).
So, for example, `0xE0000000.u / 2.u` is `0x40000000`.  And if you want math to be bounded rather than to wrap, use `+#`, `/#`, etc. operators to
clip the value inside the range, or `+!`, `/!` etc. to throw an exception.  For example `UByte(0xF0) +# UByte(0x80)` is `UByte(0xFF)`.

But there are also handy ways to manipulate time, some common special functions like erf, estimation and fitting routines, and more.

### kse.loom

This is a work in progress, and isn't well-documented.

Project Loom has delivered low-overhead virtual threads to Java 21, making concurrency especially approachable.  `kse.swarm` embraces this
with an ultra-lightweight futures system based around virtual threads, where blocking is an encouraged form of concurrency control.

Offload any computation to a virtual thread by using `Fu`:

```scala
import kse.flow.*
import kse.loom.*

object Concurrent:
  import java.nio.file.{Files, Path}

  def readFileUnsafe(p: Path): Array[String] =
    // Live dangerously, using Java methods!
    Files.readAllLines(p).toArray(new Array[String](0))

  val p = Path.of("README.md")
  val lines = Fu:
    readFileUnsafe(p)   // Immediately queues for execution on a virtual thread

  val done = lines.isComplete  // Probably false at this point!

  Thread.sleep(50)  // Concurrent with reading

  // Lazy so we won't block unless we want the result before the file's been read
  // Initializers don't always interact well with blocking on multithreaded computations
  lazy val answer = Ask:
    lines.ask().?.find(_ startsWith "import ") ?# "No imports"  // Blocks until reading is complete

println(Concurrent.done)    // Does not block
println(Concurrent.answer)  // Blocks 
```

In the above example, `lines` loads in the background while `Thread.sleep` is running; if there is any work left to do,
the current thread blocks on the call to `ask()`, and any errors during execution cause an early exit to the
`Ask:` block.

But what if you don't want to block the current thread?  You can `map` and `flatMap` `Fu`.  But, even better, you can
just keep `Fu:`-ing, because `Fu:` itself (and `Fu.flat:`, which takes an `A => (B Or Err)`) provides a boundary point
enabling `.?`.  **However, you must be careful not to have control flow jump out of a Fu**  The control flow will be
caught and the block will terminate with an `Err`, but the logic will still be wrong.

```scala
object Concurrent2:
  import java.nio.file.{Files, Path}

  def readFile(p: Path): Array[String] Or Err =
    nice{ Files.readAllLines(p).toArray(new Array[String](0)) }

  val lines = Fu.flat:
    readFile(Concurrent.p)

  val slow = Fu:
    Thread.sleep(50)

  val answer = Fu:
    slow.?  // Make sure it's done
    lines.?.find(_ startsWith "import ") ?# "No imports"

  // Now it's all in virtual-thread futures!  answer.ask() will get you the result when you need it
```

If you want subcomputations to be cancelled, if possible, by using Java's executor services, then use
`Fu.group:` and `Fu.flatGroup:` in place of `Fu:` and `Fu.flat:`.  This raises the overhead, however,
so it's only worth doing when you know that the subcomputations will take a while, and early ones might
fail, so the later ones should be cancelled.  The Java mechanism is to use `InterruptedException`, so
anything that catches and ignores the exception won't be killed, and long-running computations would
need explicit `Thread.yield` calls to mark points where interruption is acceptable.


### kse.eio

This also partly exists but is also mostly undocumented.  There are some tests, though!

A couple of highlights include the path tools--just `.path` a `String` and you have a `java.nio.file.Path` and you can
do a bunch of handy things with it like tell if it exists (`.exists`), change the extension `.extTo("jpg")`, and so on.

And there's a very easy and yet safe command-line option parser.  Create it like so:

```scala
val parser =
  import kse.eio.cleasy.*    // Don't import this everywhere!  Too many decorators!
  Cleasy("My Nifty Title")
    -- "axis" ~ ("x" | "y" | "z")      % "Pick which axis to rotate"
    -- "angle" ~ (_double, () => 0.0)  % "Angle of rotation (default 0)"
    -- "quiet" ~ 'q'                   % "Print as little as possible to stdout"
    -- 'n'.x ~ _uint                   % "Rotate this many times before display (may be repeated)"
```

If you don't like the nifty operators, you can use `+ Opt("axis", "x" | "y" | "z")` style instead, and `.comment("...")` instead of `%`
for the description.

Then you can parse like so:

```scala
val args = Array("foo", "-qn", "99", "--axis=y")
val oarg = parser.parse(args).?   // You're inside an `Ask:` block, right?
val axis = oarg("axis")           // Some("y")
val shhh = oarg.found("quiet")    // true
val rots = oarg("n")              // List(99)
val angl = oarg("angle")          // 0.0, the default
val oops = oarg("oops")           // Does not compile!  You can't mess this up!
```

There are pre-defined parsers for flexible Boolean `_tf`, numbers (e.g. `_int`, `_ulong`, `_double`), strings (`_str`) and so on.

The default error messages are sufficiently clear to pass back to a user who is familiar with command-lines.

To print all the options, get a description string using `parser.userString()`:

```text
My Nifty Title
Option         Short     Description                                           
-------------  --------  ------------------------------------------------------
--axis=x/y/z              Pick which axis to rotate
--angle=value             Angle of rotation (default 0)
--quiet        -q         Print as little as possible to stdout
--n=value      -n value   Rotate this many times before display (may be
                          repeated)
Use -- to end option parsing. Short options may be combined as -abc.

```

And if you just want the options collected into a named tuple, and the arguments in an array, use `oarg.options` and `oarg.args` respectively.

### kse.testing

This enables a very succinct but delightfully powerful testing framework, where your tests look like

```scala
T ~ myTestFunction ==== myExpectedValue --: typed[MyExpectedType]
```

For one-liner tests, it's incredibly effective.  All the tests for `kse` use `kse.testutilities` (the package name is
longer because it's nice to reserve `testing` for other use).

## Naming Conventions

Although types and interfaces provide a lot of information about a program,
names also can transmit important information about expectations for behavior
(or about types or interfaces).  In order to take advantage of this extra
channel to convey information, Kse3 attempts to maintain the following
naming conventions; violations without good reason can be considered bugs.

#### General principles.

1. Readability first.  If following another principle makes something hard
to read, don't follow it.

2. Convenience over documentation.  Method names should be short and to the
point if they are expected to be used heavily--in that case we can assume
the reader knows what they mean.  Only rarely-used or discouraged method names
should be long.  If you want to know what 'py' does (the first time),
read the documentation or the source code!  However, it is important to avoid
method names that are so terse as to be easily confused (see: readability first).

3. Assume infix.  Even if it's not marked, assume anything could be used
infix if it might possibly read more clearly.  That is: infix notation is
_not_ an exception to the first principle.

#### Construction of objects.

1. The default creation method is `apply` on a companion object, to have
uniform syntax with constructors ().  An `apply` may fail, or may correct
illegal bounds and so on, but if the usual creation method fails so often
that it should have an `Option` or `Or` type instead, the creation method
should not be called `apply`.

2. An opaque type or a class that ought usually to have its input checked
but could possibly be used directly (e.g. on the underlying type) is
created with the `wrap` method.  If there is a `wrap` method to create it,
the should be a corresponding `unwrap` method to get the same type back out.
A `wrap/unwrap/wrap` cycle should produce functionally identical objects.

3. An opaque type or a class that is just a label or adds functionality,
and works on every wrapped value, will reveal its contents with
`underlying`, if it is not container-like, or `value`, if it is
container-like, instead of `unwrap`.

4. TODO: figure out when to use `of`, `from`, `parse`, and so on, but these
are allowed to return Option/Or types when appropriate.

#### Access to contents of objects.

1. If an object contains a single unique value that is always available
save in an error state (e.g. being "closed"), that value is called `value`.
If the object is mutable, so the value is not necessarily stable, `apply()`
is preferred to reflect the unstable nature of the value.

2. If an object may contain a single unique value, or a value correspoding
to some parameters, you get that value _unsafely_ by calling `get` (a new
exception will be thrown on failure).  If there is a stored error to
propagate, `grab` instead of `get` will try to propagate the error as a
catchable exception, without a new stack trace, if possible.  Safe
alternatives are named `getQuux`, where `Quux` specifies an alterative
strategy.  For instance, `getOrElse(bippy)` should do `bippy` when the
value is not there (perhaps `bippy` is a default value).

3. If an object may be able to give values when supplied a parameter, the
method used is `apply`.  It may return an Option/Or type if errors are
routine.

4. If there is a way to return an `Or Err` type instead of failing on a
missing single value, then `ask` will give the value or an `Err` (i.e. it
will be an `Ask` type).

5. To access a value, if present, returning the original object, use `peek(f)`
where `f` acts on the contents and returns `Unit`.  `use(f)` will simply act on the
contents (like `foreach` but `use` will be inlined when practical).

6. To act on and update a modifiable value, if present, returning the original
object, use `poke(f)` where `f` maps between the same types.  To simply update without
returning the orignal object, use `zap(f)`.

7. If there is a unique modifiable value, `:=` will set it if the setting is foolproof
and the value is always there; otherwise, if more parameters are needed or error
values are needed, `set` will set it.

#### Accessors and Modification

1. Accessors to fields and field-like quantities should just be the name of
the thing, e.g. `foo`, even if error is routine so an `Or` type is returned.

2. Risky accessors, if not the normal alternative, should be named `fooGet`
or `fooGrab`.

3. An error-wrapping accessor as an alternative to a dangerous default should
be called `fooAsk`.

4. To re-create the same object with a different value of a field, use
`fooTo` and the new value. If it is possible to create an object of different
type, `fooTo` should allow it.

5. To re-create the same object with a different value computed from the
existing one, use `fooOp` and a function to compute the new value.

6. To set a mutable value where setting cannot go wrong, use `.foo =` (i.e.
the `foo_=` method).  If it can go wrong, `.fooSet` is allowed to return an
error value.

7. To set or alter a mutable field inline, use `.fooPoke(f)`; this will return
the object that you've modified, so it should only be provided if errors are
rare.

8. If there are both self-mutating and new-object-creating variants of the
same method, the former should have `Me` appended, e.g. `reverseMe()` and
the latter should be in past tense, e.g. `reversed`.
The `Me` notation is generally considered good form regardless for
self-modifying methods, unless the naming is clunky.

#### Control Flow, Exceptions and Error Handling

1. Early-exit control flow is either called `break()`, with parens, or some
modification thereof; or contains `?`.

1. Anything called `safe`, `safely`, etc., will catch and package exceptions.
Control flow will not be caught.

2. Anything called `nice`, `nicely`, etc., will catch exceptions and put them
and other errors into an `Or Err` preserving the `Or Err` status if it's
already there.  `nice{ foo } || bar.nicely(f)` is a sensible fallback pattern.

3. **Do not ever use `Try`.**  It catches control flow.  You need to
understand threading, lazy evaluation, and other situations where control flow
might escape.  The result will be wrong in those cases even if you catch
control flow with `Try`, and `Try` will prevent use of `.?` and other
efficient, low-syntactic-overhead control flow constructs that Kse provides.

4. If you must catch control flow-style errors, use `threadsafe{ ... }`.
`ControlThrowable` will be caught too.  For instance, this should be used
at thread boundaries (hence the name).

## More to come

There is more to come, obviously.
