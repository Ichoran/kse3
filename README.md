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

**Warning: kse3 only works on Scala 3.1 due to its particular use of macros**
(This is subject to change--in particular, kse3 only has a goal of working
with one scala x.y version at any time.  more is nice, not a guarantee.)

## How do I get it?

Only kse3-flow is available presently.  In mill, make sure your module has

```scala
def scalaVersion = "3.1.3"
```

**(warning: it must be 3.1...3.0 and 3.2 don't work!)**
and add the dependency

```scala
ivy"com.github.ichoran::kse3-flow:0.0.3"
```

to try it out.  If you use some other build system, you can probably figure out from the above what you need.

Then in your code,

```scala
import kse.flow.{given, _}
```

and you're ready to go.


## Subprojects

The structure of packages is still being determined.  In general, they will
be `kse.` something--a name collision with the original Kse for Scala 2, but
you shouldn't use that with Scala 3 because Kse is actually still on Scala
2.12.  So it's all good.

### kse.flow

The flow module is available separately (but you probably don't want to use it separately).  In mill, add the dependency

```scala
ivy"com.github.ichoran::kse3-flow:0.0.3"
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
iFor(list.iterator){ (i, s) => println(s*i) }  // prints nothing, then "herring", then "perchperch"
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

## More to come

There is more to come, obviously.
