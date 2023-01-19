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
  * val x = Is(5)         // typed as Int Or Nothing
  * val y = 5.or[String]  // typed as Int Or String
  * println(x == y)       // prints: true
  * 
  * val z = Some("salmon").toOr   // typed as String Or Unit
  * val w = safe{ "cod".toInt }   // typed as Int Or Throwable
  * val v = nice{ "cod".toInt }   // typed as Int Or String with throwable printed out
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
  * val c = z.discard{
  *   case s if s.length > 5 => s.length
  * }
  * println(c)    // prints: Alt(6)
  * 
  * 
  * }}}
  */
package object flow {}
