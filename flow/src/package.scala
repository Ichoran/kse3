// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

package kse

/**
  * KSE Flow provides a variety of efficient and convenient flow-control methods and data structures.
  * 
  * Because KSE Flow makes very heavy use of extension methods and opaque types,
  * and ScalaDoc does not gather extensions in a particularly usable way, the documentation
  * is more awkward to traverse than in a more traditionally structured library.
  * 
  * If the examples below do not cover your use cases, please check the test suite in the
  * repository.  Practically everything that can be done is tested there, albeit briefly.
  * 
  * == Or ==
  * 
  * The workhorse data type of kse.flow is [[Or]].  It is an unboxed left-biased sum type; it functions
  * similarly to `Either`, but is intended for use where the successful branch is more common and therefore
  * unboxed.  It further has a somewhat richer and more useful set of methods than does `Either`.  Finally,
  * because the success and failure branches are parameterized only by their own type, handling the types
  * is typically more convenient than with `Either` where the alternate type is carried along even known
  * to be irrelevant.
  * 
  * `Or` is a union type of two possibilities: a success branch, [[Is]], and a failure branch [[Alt]].
  * `Is` in turn is a union of an opaque type that is simply the unboxed success value, `IsJust`, or
  * in case of a success that is itself an `Or` with a failure somewhere therein, `IsBox`.  The `IsJust` and `IsBox` types
  * should never be manipulated directly.  Instead, `Or` provides a set of high-efficiency inlined
  * methods for transforming and operating on the values, and also provides less-efficient unapply routines
  * for match statements.
  * 
  * Because `Or` is a sum type, most methods for `Or` are extensions on `X Or Y`.
  * 
  * Here are a few highlights:
  * 
  * {{{
  * import kse.flow.{given, _}
  * 
  * val x = Is(5)            // typed as Int Or Nothing
  * val y = 5.orAlt[String]  // typed as Int Or String
  * println(x == y)          // prints: true
  * val yy = "eel".orIs[Int] // Typed as Int Or String
  * println(x == yy)         // prints: false
  * 
  * val z = Some("salmon").toOr   // typed as String Or Unit
  * val w = safe{ "cod".toInt }   // typed as Int Or Throwable
  * val v = nice{ "cod".toInt }   // typed as Int Or String (with exception printed out)
  * println(z.isIs)               // prints: true
  * println(w.isIs)               // prints: false
  * println(v.isAlt)              // prints: true
  * 
  * val u = safe{ "-".toInt } || safe{ "9".toInt }
  * println(u) // prints: 9
  * 
  * def little(i: Int): Int Or String = i match
  *   case x if x >= -99 && x <= 99 => Is(x)
  *   case _                        => Alt(s"$i is not little")
  * 
  * val a = little(10).map(_ > 0)   // == Is(true)
  * val b = little(100).map(_ > 0)  // == Alt("100 is not little"), typed as Boolean Or String
  * 
  * // Due to overly aggressive exhaustiveness checking, use `fold` instead of `match` to destructure 
  * a.fold{ i => println(i) }{ a => println("Oops") }  // prints: true
  * b.fold{ i => println(i) }{ a => println("Oops") }  // prints: Oops
  *
  * val c = z.discard {
  *   case s if s.length > 5 => s.length
  * }
  * println(c)    // prints: Alt(6)
  * }}}
  * 
  * 
  * == Early return with Or ==
  * 
  * Error handling can both be simplified by early returns, but the early returns can also produce
  * confusing code.  Typically, when there is an error there is little to do but return some sort
  * of error condition, but when there is not an error, processing can continue.  Rust prioritizes
  * this condition by allowing early returns of the unfavored branch of their `Result` type; kse does
  * the same with `Or`.
  * 
  * The most common construct to use is `Or.Ret`, with covers an entire method or a `val` with annotated
  * type:
  * 
  * {{{
  * def addLittle(i: Int, j: Int): Int Or String = Or.Ret:
  *   little(i).? + little(j).?
  * 
  * def addLittle2(i: Int, j: Int): Int Or String = Or.Ret:
  *   val x = little(i).?
  *   val y = little(j).?
  *   x + y
  * }}}
  * 
  * Here, `Or.Ret:` indicates that when `.?` is applied to an `Or`, it should extract the disfavored value
  * and keep going, or immediately return with the disfavored value.  If written with `Either`, the above
  * code would be
  * 
  * {{{
  * def small(i: Int): Either[String, Int] = i match
  *   case x if x >= -99 && x <= 99 => Right(x)
  *   case _                        => Left(s"$i is not small")
  * 
  * def addSmall(i: Int, j: Int): Either[String, Int] =
  *   small(i).flatMap(x => small(j).map(y => x + y))
  * 
  * def addSmall2(i: Int, j: Int): Either[String, Int] =
  *   for
  *     x <- small(i)
  *     y <- small(j)
  *    yield
  *     x + y
  * }}}
  * 
  * In analogy to `flatMap`, `FlatRet` accepts an `Or` as the final value instead:
  * 
  * {{{
  * def parse(s: String): Int Or String =
  *   if s.forall(_.isDigit) then Is(s.toInt) else Alt(s)
  * 
  * def addStrings(s: String, t: String): Int Or String = Or.FlatRet:
  *   val i = parse(s).?
  *   val j = parse(t).?
  *   addLittle(i, j)
  * }}}
  * 
  * 
  * == Early discard to default with attempt/default and .! ==
  * 
  * In cases where errors don't matter, one can use `Int Or Unit` with early returns,
  * but even this tends to have more boilerplate than is ideal, and can result in
  * less clear logic.
  * 
  * For the specific case of trying to produce a value, and then trying backups,
  * we instead have `attempt` and `default` (where `default` ends the chain and
  * performs all the computations).  To indicate that an attempt has failed, one
  * can either use `ensure(...)` which will abort if `...` evalutes to false,
  * or one can discard the disfavored branch of an `Or`, `Either`, etc., with `.!`
  * 
  * {{{
  * val number =
  *   attempt:
  *     val x = parse("90123").!
  *     val y = addLittle(55, 555).!  // This will fail, so we proceed to the next attempt
  *     x + y
  *   .attempt:                       // Note: MUST have dot here!
  *     addStrings("14", "53").!      // This succeeds, giving us 67
  *   .default:
  *     0                             // Fortunately we never need this
  * }}}
  * 
  * 
  * == Flow-aware exception-handling ==
  * 
  * Due to a change in policy designed to reduce the number of control-flow
  * exceptions that escape and kill threads, the Scala library's exception-handling
  * constructs catch control-flow exceptions (after previously not doing so).
  * 
  * However, because one cannot write scalable code without allowing for
  * control-flow, kse.flow has its own set of control-flow-permissive exception
  * handling routines.
  * 
  * Specifically, an extension method `.catchable` is provided for `Throwable`
  * that indicates if an exception should be caught; this returns `false` for
  * control-flow exceptions (i.e. subclasses of `ControlThrowable`).  If,
  * instead, you are protecting an executing thread, `.threadCatchable`
  * should be used instead which will tell you to intercept `ControlThrowable`.
  * Note that it should always be a design error to let `ControlThrowable`
  * escape to the top level where it must either be caught as a generic
  * exception or left to terminate the thread.
  * 
  * Additionally, `.hasAnyStackTrace` is provided which checks whether the
  * exception itself or any of the causing exceptions have a stack trace, and
  * `.explain` and `.explainAsArray` and related methods convert an exception
  * to a somewhat-more-readable form (with control over when stack traces will
  * be abbreviated).
  * 
  * To allow better interoperability between exceptions and sum-type error
  * handling (`Or` or `Either`, for instance), a typeclass `Cope[E]` is
  * defined to allow automatic translation between a `Throwable` and an
  * instance of type `E` in compatible exception-handling routines.
  * 
  * The lower-level exception-handling routines are the following:
  * 
  * {{{
  * safe{ ... }                     // Packs into an Or of type ... Or Throwable
  * safe(f: Throwable => E){ ... }  // Packs into an Or of type ... Or E
  * nice{ ... }                     // Packs into an Or of type ... Or Err (Err can box `String` and `Throwable`)
  * cope{ ... }                     // If you have a given Cope[E], packs into ... Or E
  * threadsafe{ ... }               // Like safe but catches control flow to save your thread
  * threadnice{ ... }               // Like nice but catches control flow to save your thread
  * }}}
  * 
  * Four higher-level constructs are provided: `ratchet`; `niceMap/copeMap`;  `Err.Or/Err.FlatOr`; and `attempt.safe`
  * 
  * **Ratchet**
  * 
  * Nested calls to `ratchet` are intended to be used in a case where you have a default and want to successively refine
  * the value, stopping with your best attempt.  `ratchet(d)(f)` is equivalent to `safe{ f(d) }.getOrElse{ _ => d }`
  * It's a very easy way to try to parse a number with a default:
  * 
  * {{{
  * // This syntax makes for especially clean nesting
  * ratchet(0): _ =>
  *   "minnow".toInt
  * 
  * // It's equivalent to this
  * safe{ "minnow".toInt }.getOrElse(0)
  * }}}
  * 
  * But you can also use it to save progress:
  * {{{
  * val number = " 15 "
  * val polish = "angelfish"
  * ratchet(0.0): _ =>
  *   val n = number.trim.toInt
  *   ratchet(n.toDouble): x =>
  *     x * polish.toDouble
  * }}}
  * 
  * **Monadic Maps**
  * 
  * `niceMap` and `copeMap` give a more traditional monadic interface, where `A Or Err` or `A Or E`
  * can be mapped safely by either packing errors in `Err` or converting to `E` with a `Cope[E]`.
  * 
  * {{{
  * def add(s1: String, s2: String) =
  *   nice{ s1.toInt }.niceMap{ i => i + s2.toInt }
  * 
  * def sub(s1: String, s2: String) =
  *   given Cope[Unit] with
  *     def fromThrowable(t: Throwable): Unit = ()
  *   cope{ s1.toInt }.copeMap{ i => i + s2.toInt }
  * }}}
  * 
  * 
  * **Early Return (explicit and Err.Or/Err.FlatOr)**
  * 
  * The preferred way to handle these situations, however, should usually be to use `.?` or `.?+` error
  * handling, keeping the sites of error specific:
  * 
  * {{{
  * def add2(s1: String, s2: String): Int Or Err = Or.Ret:
  *   val i1 = nice{ s1.toInt }.?
  *   val i2 = nice{ s2.toInt }.?
  *   i1 + i2
  * 
  * def sub2(s1: String, s2: String): Int Or Unit = Or.Ret:
  *   given Cope[Unit] with
  *     def fromThrowable(t: Throwable): Unit = ()
  *   val i1 = cope{ s1.toInt }.?
  *   val i2 = cope{ s2.toInt }.?
  *   i1 + i2
  * }}}
  * 
  * Furthermore, with the `Err` type you can use `Err.Or:` instead of `Or.Ret:` and `Err.FlatOr:`
  * instead of `Or.FlatRet:` in order to catch errors during processing as well; type inference
  * usually works without an explicit return type, also:
  * 
  * {{{
  * def add3(s1: String, s2: String) = Err.Or:
  *   val i1 = nice{ s1.toInt }.?   // Early return
  *   val i2 = s2.toInt             // Might throw, but that will be caught
  *   i1 + i2
  * }}}
  * 
  * **attempt.safe**
  * 
  * If you are going to be discarding the error case and want to try several approaches before
  * you end up with a default (the opposite case of `ratchet` where we build up gradually to
  * success; here we try for the best success all at once, then keep falling back), the `attempt`
  * scheme works with `.safe:` also.  The attempt needs to be opened with `attempt.safe:`, if
  * that one needs to be safe; but you can just use `.safe:` instead of `.attempt:` on fallbacks:
  * 
  * {{{
  * attempt.safe:
  *   "five".toInt + parse("3").!
  * .safe:
  *   "-5".toInt
  * .default:
  *   0
  * }}}
  * 
  * 
  * **works with `.breakable`**
  * 
  * If you're using `kse.basics`, you can use `.orSkip` or `.orQuit` anywhere you could bail out of
  * an attempt using `.!`.
  * 
  * **escape and calculate**
  * 
  * If you just want to bail out of some side-effecting code if a condition fails, use `escape:` and decorate
  * the condition with `.?`
  * 
  * {{{
  * escape:
  *   for i <- 0 to 10 do
  *     (i < a.length).?
  *     a(i) = i
  * }}}
  * 
  * If it's a calculation and you want to bail out with `NaN` if you hit a `NaN`, use `calculate:` and `.?` on the values.
  * 
  * {{{
  * val answer = calculate:
  *   var sum = 0.0
  *   var i = 0
  *   while i < a.length do
  *     sum += a(i).?
  *     i += 1
  *   sum
  * }}}
  * 
  * (If `NaN`s are common, this could save a lot of computation.)
  * 
  * 
  * == Copying mutables ==
  * 
  * kse.flow defines a typeclass `Copies` that enables an extension method `.copy` on any class `A`
  * for which there is a given `Copies[A]`.  If you import givens, you get `Copies` for array types
  * (the primitives and AnyRef).  Support for arrays of opaques is experimental and trait-based
  * (see source).  **This has moved to kse.basics**
  * 
  * 
  */
package object flow {}
