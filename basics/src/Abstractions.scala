// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr, UCSF, and Calico Life Sciences LLC.

package kse.flow


/** Typeclass that witnesses that A is actually opaquely implemented by B */
trait Translucent[A, B] {
  final def fromOpaque(a: A): B = a.asInstanceOf[B]
  final def uncheckedIntoOpaque(b: B): A = b.asInstanceOf[A]
  transparent inline final def inlineFromOpaque(inline a: A) = a.asInstanceOf[B]
  transparent inline final def inlineIntoOpaque(inline b: B) = b.asInstanceOf[A]
}
object Translucent {
  trait Companion[A, B] {
    given translucency: Translucent[Array[A], Array[B]] with {
    }

    given copiesOpaqueArray(using cp: Copies[Array[B]], tlc: Translucent[Array[A], Array[B]]): Copies[Array[A]] with
      def copy(a: Array[A]): Array[A] = tlc.uncheckedIntoOpaque( cp.copy( tlc.fromOpaque(a) ) )
  }
}



/** Typeclass to enable generic copying of mutable things to a decent replica of themselves. */
trait Copies[A] {
  /** Creates a copy of a presumably mutable object. */
  def copy(a: A): A
}
object Copies {
  given copiesArrayBoolean: Copies[Array[Boolean]] with
    def copy(a: Array[Boolean]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayByte:    Copies[Array[Byte  ]] with
    def copy(a: Array[Byte   ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayShort:   Copies[Array[Short ]] with
    def copy(a: Array[Short  ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayChar:    Copies[Array[Char  ]] with
    def copy(a: Array[Char   ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayInt:     Copies[Array[Int   ]] with
    def copy(a: Array[Int    ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayLong:    Copies[Array[Long  ]] with
    def copy(a: Array[Long   ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayFloat:   Copies[Array[Float ]] with
    def copy(a: Array[Float  ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayDouble:  Copies[Array[Double]] with
    def copy(a: Array[Double ]) = java.util.Arrays.copyOf(a, a.length)

  given copiesArrayGeneric[A <: AnyRef]: Copies[Array[A]] with
    def copy(a: Array[A]) = java.util.Arrays.copyOf[A & AnyRef](a, a.length)
}

/** Use copier typeclasses, if available, to copy a (presumably mutable) object. */
extension [A](a: A)
  inline def copy(using copier: Copies[A]): A = copier copy a
