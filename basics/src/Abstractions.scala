// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr, UCSF, and Calico Life Sciences LLC.

package kse.basics

import scala.util.boundary



/** Typeclass that witnesses that O is actually opaquely implemented by I */
trait Translucent[O, I] {
  final def fromOpaque(o: O): I = o.asInstanceOf[I]
  final def uncheckedIntoOpaque(i: I): O = i.asInstanceOf[O]
  transparent inline final def inlineFromOpaque(inline o: O) = o.asInstanceOf[I]
  transparent inline final def inlineUncheckedIntoOpaque(inline i: I) = i.asInstanceOf[O]
  transparent inline final def inlineArrayFromOpaque(inline oa: Array[O]) = oa.asInstanceOf[Array[I]]
  transparent inline final def inlineUncheckedArrayIntoOpaque(inline ia: Array[I]) = ia.asInstanceOf[Array[O]]
}
object Translucent {
  trait Companion[O, I] {
    given translucency: Translucent[O, I] with {}
  }

  given [O, I](using Translucent[O, I]): Translucent[Array[O], Array[I]] with {}
}



/** Way to create new unboxed types.  Use
  * ```
  * object MyThing extends NewType[OldThing] {
  *   extension (t: Type) def myMethodOnMyThing: Bar = whateverComputation(t.unwrap)
  * }
  * ```
  *
  * To generate a new value, use `val m = MyThing.wrap(x)`.  To get the value, use `m.unwrap`.
  * In addition to that, you can use any extension methods you have defined.
  */
trait NewType[A] {
  opaque type Type = A

  /** Create the newtype */
  inline def wrap(a: A): Type = a

  /** Because this is only for boxing-style newtypes, have a direct method too */
  inline def apply(a: A): Type = a

  /** Get the value from the newtype--this is the ONLY way to interact with it */
  extension (t: Type)
    inline def unwrap: A = t
    inline def value: A = t

  /** Defer to the wrapped type's CanEqual */
  given (using CanEqual[A, A]): CanEqual[Type, Type] = CanEqual.derived

  /** Enable array copying via translucency */
  given translucency: Translucent[Type, A] with {}
}



/** Typeclass to enable generic copying of mutable things to a decent replica of themselves. */
trait Copies[A] {
  /** Creates a copy of a presumably mutable object. */
  def copy(a: A): A
}
object Copies {
  given copiesArrayBoolean: Copies[Array[Boolean]] with
    def copy(a: Array[Boolean]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayByte:    Copies[Array[Byte   ]] with
    def copy(a: Array[Byte   ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayShort:   Copies[Array[Short  ]] with
    def copy(a: Array[Short  ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayChar:    Copies[Array[Char   ]] with
    def copy(a: Array[Char   ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayInt:     Copies[Array[Int    ]] with
    def copy(a: Array[Int    ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayLong:    Copies[Array[Long   ]] with
    def copy(a: Array[Long   ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayFloat:   Copies[Array[Float  ]] with
    def copy(a: Array[Float  ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayDouble:  Copies[Array[Double ]] with
    def copy(a: Array[Double ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayGeneric[A <: AnyRef]: Copies[Array[A]] with
    def copy(a: Array[A]) = java.util.Arrays.copyOf[A & AnyRef](a, a.length)

  given copiesArrayOpaque[O, A](using translucency: Translucent[O, A], copier: Copies[Array[A]]): Copies[Array[O]] with
    def copy(a: Array[O]) = translucency.inlineUncheckedArrayIntoOpaque( copier.copy( translucency.inlineArrayFromOpaque(a) ) )
}

/** Use copier typeclasses, if available, to copy a (presumably mutable) object. */
extension [A](a: A)
  inline def copy(using copier: Copies[A]): A = copier copy a



object shortcut {
  sealed trait Type {}
  object Skips extends Type {}
  object Quits extends Type {}

  inline def quittable(inline f: boundary.Label[Quits.type] ?=> Unit): Unit =
    boundary[Quits.type]:
      f
      Quits

  inline def skippable(inline f: boundary.Label[Skips.type] ?=> Unit): Unit =
    boundary[Skips.type]:
      f
      Skips

  inline def outer(inline f: boundary.Label[Type] ?=> Unit): Unit =
    boundary[Type]:
      f
      Quits

  inline def inner(inline f: boundary.Label[Type] ?=> Unit)(using boundary.Label[Type]): Unit =
    val what = boundary[Type]:
      f
      Skips
    if what eq Quits then boundary.break(Quits)

  inline def skip[S >: Skips.type <: Type](using boundary.Label[S]) = boundary.break(Skips: S)

  inline def skipIf[S >: Skips.type <: Type](p: Boolean)(using boundary.Label[S]): Unit = if p then boundary.break(Skips: S)

  inline def quit[Q >: Quits.type <: Type](using boundary.Label[Q]) = boundary.break(Quits: Q)

  inline def quitIf[Q >: Quits.type <: Type](p: Boolean)(using boundary.Label[Q]): Unit = if p then boundary.break(Quits: Q)
}
