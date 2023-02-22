# Kse3

This repository contains the Kerr Standard Extensions to Scala 3.

These are intended to cover everything that the standard library forgot that
and is commonly used for general-purpose programming and data analysis.

Kse3 has no pretenses of being an idiomatic toolchain.  It is designed for
high-productivity programming for people who like direct style, good error
handling, and care about performance.  There is no particular attempt to
take a monadic or other functional approach, except inasmuch as it helps
productivity.  When there is a tradeoff between enabling good user code and
writing "good" library code (DRY, etc.), Kse3 favors the user.  Kse is
supposed to take care of any necessary ugly stuff so you don't have to.

**Warning: kse3 only works on Scala 3.3 and later due to its use of
`scala.util.boundary`**

**Warning: when you use extensions with Scala 3 in multiple libraries,
they clobber each other because they all share the same namespace.
Do NOT use extensions in libraries!  Except...kse3 does it anyway,
in the hope that someday extensions will be usable, and because it's
meant to cover core functionality.**

## How do I get it?

Only kse3-flow and kse3-maths (and kse3-testing) are available presently.  In mill, make sure your module has

```scala
def scalaVersion = "3.3.0-RC2"
```

And add at least the first line out of

```scala
ivy"com.github.ichoran::kse3-flow:0.1.1"
ivy"com.github.ichoran::kse3-maths:0.1.1"
```

to try it out.  If you use some other build system, you can probably figure out from the above what you need.

Then in your code,

```scala
import kse.flow.{given, _}
```

and you're ready to go.

#### Is this stable?  Binary compatible?

Not yet and no.  Eventually it will be stable.  Binary compatibility is not likely to ever be an explicit goal,
because Kse3 makes very heavy use of inlining.  Because you have to recompile all the time anyway, there is
no reason to try to maintain binary compatibility.

#### How do I pronounce it?

I don't hear words when I read them, so I don't have any strong opinion.  I think the most conventional way to pronounce something with that spelling would be K-see, so let's go with that?


## Subprojects

The structure of packages is still being determined.  In general, they will
be `kse.` something--a name collision with the original Kse for Scala 2, but
you shouldn't use that with Scala 3 because Kse is actually still on Scala
2.12.  So it's all good.

### kse.flow

The flow module is available separately (but you probably don't want to use it
separately).  In mill, add the dependency

```scala
ivy"com.github.ichoran::kse3-flow:0.1.1"
```

and in your code,

```scala
import kse.flow.{given, _}
```

Then you have access to the left-biased unboxed sum type `Or`:

```scala
def little(i: Int): Int Or String =
  if i > -100 && i < 100 then Is(i)
  else Alt(s"$i is not little")

val x = little(12)
val y = little(999)

println(x)             // prints: 10
println(y)             // prints: Alt(999 is not little)
y.foreachAlt(println)  // prints: 999 is not little
x.foreachAlt(println)  // does not print anything

val z = x.map(_ * -3)
val w = z.discard{
  case i if i < 0 => "No negatives please"
}

println(z)             // prints: -30
println(w)             // prints: Alt("No negatives please")
```

You have Rust-style `.?` early-exit error handling into `Or` (and `Option` and `Either` and `Try`, but once you have `Or`, why ever use those?), and `safe{ }` to intercept exceptions and pack the result into `A Or Throwable`.

```scala
def reverseParse(s: String): Int Or String =
    safe{ s.reverse.toInt }.mapAlt(_ => s"$s is not a number in reverse")

def favorite(i: Int): Int Or String = Or.Ret{
  val x = little(i).?  // return bad case early, keep going with good value
  x * reverseParse(x.toString).?
}

val a = favorite(12)
val b = favorite(999)
val c = favorite(-3)
println(a)   // prints: 252
println(b)   // prints: Alt(999 is not little)
println(c)   // prints: Alt(-3 is not a number in reverse)
```

You have access to full-speed inlined pipe (to transform values) and tap (to act on them but pass forward the original):

```scala
val m = (1 + 2*3).pipe(x => x*x)
val n = (1 + 2*3).tap(println) + 1 // prints: 7
println(n)                         // prints: 8
println(m)                         // prints: 49
```

There are inlined index-based loops, as fast as `while` but without risk of an indexing error:

```scala
val list = List("salmon", "herring", "perch")
iFor(list.iterator){ (i, s) => println(s*i) }  // prints newline, then "herring", then "perchperch"
```

Standard mutable containers for primitive and object types for counting, callbacks, and other such use:

```scala
def nextEven(m: Mu[Int]): m.type =
  m.zap(i => if i % 2 == 0 then i + 2 else i + 1)

val count = Mu(1)
for (i <- 1 to 5) nextEven(count)
println(count.value)   // prints: 10


def tokenCount(s: String, tokens: Mu[Array[String]] Or Unit): Int =
  if s.isEmpty then 0
  else
    val tok = s.split("\\s+")
    tokens.foreach(_.set(tok))
    tok.length

val tok = Mu(Array.empty[String])
println(tokenCount("minnow salmon bass eel", tok))  // prints: 4
println(tok.value.mkString)                         // prints: minnowsalmonbasseel
```

And a variety of other nice things that you can find by perusing the ScalaDoc, the unit tests, or the code.

### kse.maths

This exists but presently is undocumented.  Feel free to look through the unit tests to see what can be done, however!

### kse.eio

This also partly exists but isn't published to maven, and is also undocumented.  There are some tests, though!


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
point if they are expected to be used heavily.  Only rarely-used or
discouraged method names should be long.  If you want to know what 'py' does,
read the documentation or the source code!

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

2. If an object may contain a single unique value, or a value correspoding
to some parameters, you get that value _unsafely_ by calling `get` (a new
exception will be thrown on failure).  If there is a stored error to
propagate, `grab` instead of `get` will try to propagate the error as a
catchable exception, without a new stack trace, if possible.  Safe
alternatives are named `getSping`, where `Sping` specifies an alterative
strategy.  For instance, `getOrElse(bippy)` should do `bippy` when the
value is not there (perhaps `bippy` is a default value).

3. If an object may be able to give values when supplied a parameter, the
method used is `apply`.  It may return an Option/Or type if errors are
routine.

4. If there is a way to return an `Or Err` type instead of failing on a
missing single value, then `ask` will give the value or an `Err`.

5. To access a value, if present, returning the original object, use `use(f)`
where `f` acts on the contents and returns `Unit`.  Generally a parameterized
`use` is not encouraged, e.g. `use(3)(println)`: use `tap` instead.

6. To act on and update a modifiable value, if present, returning the original
object, use `zap(f)` where `f` maps between the same types.  Again,
parameterized `zap` is not encouraged, e.g. no `zap(3)(_.toUpperCase)`.

7. If there is a unique modifiable value, `set` will set it.

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

7. To set or alter a mutable field inline, use `.fooZap(f)`; this will return
the object that you've modified, so it should only be provided if errors are
rare.

8. If there are both self-mutating and new-object-creating variants of the
same method, the last one should have `Me` appended, e.g. `reverseMe()`.
The `Me` notation is generally considered good form regardless for
self-modifying methods, unless the naming is clunky.

#### Exceptions and Error Handling

1. Anything called `safe`, `safely`, etc., will catch and package exceptions.
Control flow will not be caught.

2. Anything called `nice`, `nicely`, etc., will catch exceptions and put them
and other errors into an `Or Err` preserving the `Or Err` status if it's
already there.  `nice{ foo } || bar.nicely(f)` is a sensible fallback pattern.

3. Do not every use `Try`.  It catches control flow.  You are expected to
understand threading, lazy evaluation, and other situations where control flow
might escape.  The result will be wrong even if you catch control flow.

4. If you must catch control flow-style errors, use `threadsafe{ ... }`.
`ControlThrowable` will be caught too.

## More to come

There is more to come, obviously.
