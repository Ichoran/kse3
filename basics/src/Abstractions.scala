// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023-24 Rex Kerr, UCSF, and Calico Life Sciences LLC.

package kse.basics


import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.util.{boundary, NotGiven}
import scala.compiletime.summonFrom


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

  /** Get the value from the newtype--this is the only way to interact with it
    * because the implementing object is in a different scope than the type.
    */
  extension (t: Type)
    inline def unwrap: A = t
    inline def value: A = t

  /** Defer to the wrapped type's CanEqual */
  given (using CanEqual[A, A]): CanEqual[Type, Type] = CanEqual.derived

  /** Enable array copying etc. via translucency */
  given translucency: Translucent[Type, A] with {}
}


/** A stable identifier to disambiguate types by label */
type LabelVal = String & Singleton

/** A labelled type unrelated to the thing it is labelling; create with `val x: Int \ "eel" = \(5)`; access with `x ~ "eel"` or `x.unlabel` */
opaque infix type \[+A, L <: LabelVal] = A
object \ {
  inline def apply[A, L <: LabelVal](a: A): (A \ L) = a
  extension [A, L <: LabelVal](la: A \ L)
    inline def ~(l: L): A = la
    inline def unlabel: A = la
    inline def valueTo[B](b: B): (B \ L) = b
    inline def valueOp[B](f: A => B): (B \ L) = f((la: A))
    inline def labelTo[M <: LabelVal](m: M): (A \ M) = (la: A)
    inline def subtyped: (A \< L) = (la: A)
    inline def supertyped: (A \> L) = (la: A)
    transparent inline def label: L = compiletime.constValue[L]
}

/** A labelled type that is a subtype of the thing it is labeling.  Create with `val x: Int \< "eel"; use it directly. */
opaque infix type \<[+A, L <: LabelVal] <: A = A
object \< {
  inline def apply[A, L <: LabelVal](a: A): (A \< L) = a
  extension [A, L <: LabelVal](la: A \< L)
    inline def unlabel: A = la
    inline def valueTo[B](b: B): (B \< L) = b
    inline def valueOp[B](f: A => B): (B \< L) = f((la: A))
    inline def labelTo[M <: LabelVal](m: M): (A \< M) = (la: A)
    inline def newtyped: (A \ L) = (la: A)
    inline def supertyped: (A \> L) = (la: A)
    transparent inline def label: L = compiletime.constValue[L]
}

/** A labelled type that is a supertype of the thing it is labeling.  Create with `val x: Int \> "eel"; access with `x ~ "eel"` or `x.unlabel` */
opaque infix type \>[+A, L <: LabelVal] >: A = A
object \> {
  inline def apply[A, L <: LabelVal](a: A): (A \> L) = a
  extension [A, L <: LabelVal](la: A \> L)
    inline def ~(l: L): A = la
    inline def unlabel: A = la
    inline def valueTo[B](b: B): (B \> L) = b
    inline def valueOp[B](f: A => B): (B \> L) = f((la: A))
    inline def labelTo[M <: LabelVal](m: M): (A \> M) = (la: A)
    inline def newtyped: (A \ L) = (la: A)
    inline def subtyped: (A \< L) = (la: A)
    transparent inline def label: L = compiletime.constValue[L]
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


/** Implements barriers to crossing boundaries; you can only jump within your own Corral. */
opaque type Corral[S <: Singleton] = Unit
object Corral {
  given zero: Corral[0] = ()
  given unwrappedCorral[S <: Singleton, C <: Singleton & Corral[S]](using Corral[C]): C = ().asInstanceOf[C]

  inline def apply[A, S <: Singleton](using c: Corral[S])(inline f: Corral[c.type] ?=> A): A =
    f(using ((): Corral[c.type]))
}

/** Witnesses that a boundary jump is possible but specifies the Corral within which one can jump */
opaque type Hop[-A, S <: Singleton] = Unit
object Hop {
  inline def apply[A, S <: Singleton](using c: Corral[S])(inline f: (boundary.Label[A], Hop[A, c.type]) ?=> A): A =
    boundary[A]: label ?=>
      f(using label, ((): Hop[A, c.type]))

  inline def jump[A, S <: Singleton](a: A)(using l: boundary.Label[A], h: Hop[A, S], s: S): Nothing = summonFrom{
    case _: Corral[S] => scala.compiletime.error("Hop cannot cross its containing Corral")
    case _            => boundary.break(a)
  }
}

/** Helper class to specify what the type of the boundary is without having to specify the Corral instance */
opaque type HopWith[A] = Unit
object HopWith{
  inline def apply[A](): kse.basics.HopWith[A] = ()

  extension [A](h: HopWith[A])
    inline def here[S <: Singleton](using c: Corral[S])(inline f: (boundary.Label[A], Hop[A, c.type]) ?=> A) = Hop.apply[A, S](f)
}

/** Witnesses that you want a boundary with a type; the actual boundary must be instantiated with `.here`, e.g. `hop[Int].here:` */
inline def hop[A]: HopWith[A] = HopWith.apply[A]()


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

  opaque type QuitTest = Boolean
  object QuitTest {
    extension (p: QuitTest)
      inline def ?[Q >: Quits.type <: Type](using boundary.Label[Q]): Unit = if p then boundary.break(Quits: Q)
  }
  opaque type SkipTest = Boolean
  object SkipTest {
    extension (p: SkipTest)
      inline def ?[S >: Skips.type <: Type](using boundary.Label[S]): Unit = if p then boundary.break(Skips: S)
  }

  inline def quit(p: Boolean): QuitTest = p
  inline def skip(p: Boolean): SkipTest = p

  inline def breakAndSkip[S >: Skips.type <: Type]()(using boundary.Label[S]) = boundary.break(Skips: S)

  inline def breakAndQuit[Q >: Quits.type <: Type]()(using boundary.Label[Q]) = boundary.break(Quits: Q)

  object hopped {
    inline def quittable[C <: Singleton](using c: Corral[C])(inline f: (boundary.Label[Quits.type], Hop[Quits.type, c.type]) ?=> Unit): Unit =
      Hop(using c):
        f
        Quits

    inline def skippable[C <: Singleton](using c: Corral[C])(inline f: (boundary.Label[Skips.type], Hop[Skips.type, c.type]) ?=> Unit): Unit =
      Hop(using c):
        f
        Skips

    inline def outer[C <: Singleton](using c: Corral[C])(inline f: (boundary.Label[Type], Hop[Type, c.type]) ?=> Unit): Unit =
      Hop(using c):
        f
        Quits

    inline def inner[C <: Singleton](using c: Corral[C])(inline f: (boundary.Label[Type], Hop[Type, c.type]) ?=> Unit)(using l: boundary.Label[Type], h: Hop[Type, c.type]): Unit =
      val what = Hop(using c):
        f
        Skips
      if what eq Quits then Hop.jump(Quits)

    inline def breakAndSkip[S >: Skips.type <: Type, C <: Singleton]()(using l: boundary.Label[S], h: Hop[S, C], c: C) = Hop.jump(Skips: S)

    inline def breakAndSkipIf[S >: Skips.type <: Type, C <: Singleton](p: Boolean)(using l: boundary.Label[S], h: Hop[S, C], c: C): Unit = if p then Hop.jump(Skips: S)

    inline def breakAndQuit[Q >: Quits.type <: Type, C <: Singleton]()(using l: boundary.Label[Q], h: Hop[Q, C], c: C) = Hop.jump(Quits: Q)

    inline def breakAndQuitIf[Q >: Quits.type <: Type, C <: Singleton](p: Boolean)(using l: boundary.Label[Q], h: Hop[Q, C], c: C): Unit = if p then Hop.jump(Quits: Q)
  }
}

