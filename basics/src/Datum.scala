// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2021-25 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.basics


import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference, LongAdder}

import scala.annotation.targetName
import scala.compiletime.{erasedValue, summonFrom}
import scala.reflect.ClassTag
import scala.util.boundary

import scala.collection.immutable.{Range => Rg}

import kse.basics.intervals._



extension [A](a: Array[A]) {
  inline def apply(i: kse.basics.FromLengthIdx): A = a(i of a)
  inline def apply(e: End.type): A = a(a.length - 1)

  inline def use(i: Int)(inline f: A => Unit): a.type =
    if i >= 0 && i < a.length then f(a(i))
    a
  inline def zap(i: Int)(inline f: A => A): a.type =
    if i >= 0 && i < a.length then a(i) = f(a(i))
    a
}


extension (a: String) {
  inline def apply(i: Int) = a.charAt(i)

  @targetName("applyFromLengthIdx")
  inline def apply(i: kse.basics.FromLengthIdx): Char = a.charAt(i of a)

  inline def apply(e: End.type): Char = a.charAt(a.length - 1)

  inline def use(i: Int)(inline f: Char => Unit): a.type =
    if i >= 0 && i < a.length then f(a.charAt(i))
    a

  inline def arr: Array[Char] = a.toCharArray

  inline def builder(): java.lang.StringBuilder =
    if a.length == 0 then new java.lang.StringBuilder() else new java.lang.StringBuilder(a)

  inline def build(inline f: java.lang.StringBuilder => Unit): String =
    val b = new java.lang.StringBuilder(a)
    f(b)
    b.toString
}


/** Holds mutable data (would be better if standard library exposed this!) */
sealed abstract class Mu[A] {
  def getValue: A
  def setValue(a: A): Unit
  def copy: Mu[A]
  override def toString = s"~$getValue"
  override def hashCode = getValue.##
  override def equals(a: Any) = a match
    case m: Mu[?] => m.getValue.asInstanceOf[Any] == getValue
    case _ => false
}
object Mu {
  type Primitive = Unit | Boolean | Byte | Short | Char | Int | Long | Float | Double

  sealed abstract class T[A] extends Mu[A] {
    override def copy: T[A]
  }
  object T {
    inline def apply[X <: Primitive](x: X): T[X] = inline x match
      case u: Unit    => MuUnit.asInstanceOf[T[X]]
      case z: Boolean => (new MuBoolean(z)).asInstanceOf[T[X]]
      case b: Byte    => (new MuByte(b)).asInstanceOf[T[X]]
      case s: Short   => (new MuShort(s)).asInstanceOf[T[X]]
      case c: Char    => (new MuChar(c)).asInstanceOf[T[X]]
      case i: Int     => (new MuInt(i)).asInstanceOf[T[X]]
      case l: Long    => (new MuLong(l)).asInstanceOf[T[X]]
      case f: Float   => (new MuFloat(f)).asInstanceOf[T[X]]
      case d: Double  => (new MuDouble(d)).asInstanceOf[T[X]]

    inline def apply[A](a: A)(using scala.util.NotGiven[A <:< Primitive]): T[A] = summonFrom {
      case t: Translucent[A, Unit]    => MuUnit.asInstanceOf[T[A]]
      case t: Translucent[A, Boolean] => MuBoolean(t.inlineFromOpaque(a)).asInstanceOf[T[A]]
      case t: Translucent[A, Byte]    => MuByte(   t.inlineFromOpaque(a)).asInstanceOf[T[A]]
      case t: Translucent[A, Short]   => MuShort(  t.inlineFromOpaque(a)).asInstanceOf[T[A]]
      case t: Translucent[A, Char]    => MuChar(   t.inlineFromOpaque(a)).asInstanceOf[T[A]]
      case t: Translucent[A, Int]     => MuInt(    t.inlineFromOpaque(a)).asInstanceOf[T[A]]
      case t: Translucent[A, Long]    => MuLong(   t.inlineFromOpaque(a)).asInstanceOf[T[A]]
      case t: Translucent[A, Float]   => MuFloat(  t.inlineFromOpaque(a)).asInstanceOf[T[A]]
      case t: Translucent[A, Double]  => MuDouble( t.inlineFromOpaque(a)).asInstanceOf[T[A]]
    }

    given [A]: Copies[T[A]] with
      def copy(m: T[A]): T[A] = m.copy
  }

  type Specialized[X <: Primitive] = X match
    case Unit    => MuUnit.type
    case Boolean => MuBoolean
    case Byte    => MuByte
    case Short   => MuShort
    case Char    => MuChar
    case Int     => MuInt
    case Long    => MuLong
    case Float   => MuFloat
    case Double  => MuDouble

  object      MuUnit                   extends  T[Unit]    { def copy: MuUnit.type = this                 ;                      def getValue = ()      ; def setValue(a: Unit   ): Unit = () }
  final class MuBoolean(init: Boolean) extends  T[Boolean] { def copy: MuBoolean = new MuBoolean(myValue) ; var myValue = init ; def getValue = myValue ; def setValue(a: Boolean): Unit = { myValue = a} }
  final class MuByte   (init: Byte)    extends  T[Byte]    { def copy: MuByte    = new MuByte(myValue)    ; var myValue = init ; def getValue = myValue ; def setValue(a: Byte   ): Unit = { myValue = a} }
  final class MuShort  (init: Short)   extends  T[Short]   { def copy: MuShort   = new MuShort(myValue)   ; var myValue = init ; def getValue = myValue ; def setValue(a: Short  ): Unit = { myValue = a} }
  final class MuChar   (init: Char)    extends  T[Char]    { def copy: MuChar    = new MuChar(myValue)    ; var myValue = init ; def getValue = myValue ; def setValue(a: Char   ): Unit = { myValue = a} }
  final class MuInt    (init: Int)     extends  T[Int]     { def copy: MuInt     = new MuInt(myValue)     ; var myValue = init ; def getValue = myValue ; def setValue(a: Int    ): Unit = { myValue = a} }
  final class MuLong   (init: Long)    extends  T[Long]    { def copy: MuLong    = new MuLong(myValue)    ; var myValue = init ; def getValue = myValue ; def setValue(a: Long   ): Unit = { myValue = a} }
  final class MuFloat  (init: Float)   extends  T[Float]   { def copy: MuFloat   = new MuFloat(myValue)   ; var myValue = init ; def getValue = myValue ; def setValue(a: Float  ): Unit = { myValue = a} }
  final class MuDouble (init: Double)  extends  T[Double]  { def copy: MuDouble  = new MuDouble(myValue)  ; var myValue = init ; def getValue = myValue ; def setValue(a: Double ): Unit = { myValue = a} }
  final class MuAny[A] (init: A)       extends Mu[A]       { def copy: MuAny[A]  = new MuAny[A](myValue)  ; var myValue = init ; def getValue = myValue ; def setValue(a: A      ): Unit = { myValue = a} }

  transparent inline def apply[X <: Primitive](x: X): Specialized[X] = inline x match
    case u: Unit    => MuUnit
    case z: Boolean => new MuBoolean(z)
    case b: Byte    => new MuByte(b)
    case s: Short   => new MuShort(s)
    case c: Char    => new MuChar(c)
    case i: Int     => new MuInt(i)
    case l: Long    => new MuLong(l)
    case f: Float   => new MuFloat(f)
    case d: Double  => new MuDouble(d)

  transparent inline def apply[A](a: A)(using scala.util.NotGiven[A <:< Primitive]): Mu[A] = summonFrom {
    case t: Translucent[A, Unit]    => MuUnit.asInstanceOf[T[A]]
    case t: Translucent[A, Boolean] => MuBoolean(t.inlineFromOpaque(a)).asInstanceOf[T[A]]
    case t: Translucent[A, Byte]    => MuByte(   t.inlineFromOpaque(a)).asInstanceOf[T[A]]
    case t: Translucent[A, Short]   => MuShort(  t.inlineFromOpaque(a)).asInstanceOf[T[A]]
    case t: Translucent[A, Char]    => MuChar(   t.inlineFromOpaque(a)).asInstanceOf[T[A]]
    case t: Translucent[A, Int]     => MuInt(    t.inlineFromOpaque(a)).asInstanceOf[T[A]]
    case t: Translucent[A, Long]    => MuLong(   t.inlineFromOpaque(a)).asInstanceOf[T[A]]
    case t: Translucent[A, Float]   => MuFloat(  t.inlineFromOpaque(a)).asInstanceOf[T[A]]
    case t: Translucent[A, Double]  => MuDouble( t.inlineFromOpaque(a)).asInstanceOf[T[A]]
    case _                          => new MuAny(a)
  }

  given [A]: Copies[Mu[A]] with
    def copy(m: Mu[A]): Mu[A] = m.copy
}

extension [A, M <: Mu[A]](mu: M) {
  inline def apply(): A = inline mu match
    case mut: Mu.T[A] => inline mut match
      case u: Mu.MuUnit.type => ()
      case u: Mu.T[Unit]     => ()
      case z: Mu.MuBoolean   => z.myValue
      case z: Mu.T[Boolean]  => z.asInstanceOf[Mu.MuBoolean].myValue
      case b: Mu.MuByte      => b.myValue
      case b: Mu.T[Byte]     => b.asInstanceOf[Mu.MuByte].myValue
      case s: Mu.MuShort     => s.myValue
      case s: Mu.T[Short]    => s.asInstanceOf[Mu.MuShort].myValue
      case c: Mu.MuChar      => c.myValue
      case c: Mu.T[Char]     => c.asInstanceOf[Mu.MuChar].myValue
      case i: Mu.MuInt       => i.myValue
      case i: Mu.T[Int]      => i.asInstanceOf[Mu.MuInt].myValue
      case l: Mu.MuLong      => l.myValue
      case l: Mu.T[Long]     => l.asInstanceOf[Mu.MuLong].myValue
      case f: Mu.MuFloat     => f.myValue
      case f: Mu.T[Float]    => f.asInstanceOf[Mu.MuFloat].myValue
      case d: Mu.MuDouble    => d.myValue
      case d: Mu.T[Double]   => d.asInstanceOf[Mu.MuDouble].myValue
      case _                 => summonFrom {
        case t: Translucent[A, Unit]    => t.inlineUncheckedIntoOpaque(())
        case t: Translucent[A, Boolean] => t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuBoolean].myValue)
        case t: Translucent[A, Byte]    => t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuByte   ].myValue)
        case t: Translucent[A, Short]   => t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuShort  ].myValue)
        case t: Translucent[A, Char]    => t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuChar   ].myValue)
        case t: Translucent[A, Int]     => t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuInt    ].myValue)
        case t: Translucent[A, Long]    => t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuLong   ].myValue)
        case t: Translucent[A, Float]   => t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuFloat  ].myValue)
        case t: Translucent[A, Double]  => t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuDouble ].myValue)
      }
    case _ => inline erasedValue[A] match
      case _: Unit    => ().asInstanceOf[A]
      case _: Boolean => mu match
        case z: Mu.MuBoolean => z.myValue
        case _               => mu.getValue
      case _: Byte    => mu match
        case b: Mu.MuByte    => b.myValue
        case _               => mu.getValue
      case _: Short   => mu match
        case s: Mu.MuShort   => s.myValue
        case _               => mu.getValue
      case _: Char    => mu match
        case c: Mu.MuChar    => c.myValue
        case _               => mu.getValue
      case _: Int     => mu match
        case i: Mu.MuInt     => i.myValue
        case _               => mu.getValue
      case _: Long    => mu match
        case l: Mu.MuLong    => l.myValue
        case _               => mu.getValue
      case _: Float   => mu match
        case f: Mu.MuFloat   => f.myValue
        case _               => mu.getValue
      case _: Double  => mu match
        case d: Mu.MuDouble  => d.myValue
        case _               => mu.getValue
      case _ => summonFrom {
        case t: Translucent[A, Unit]    => t.inlineUncheckedIntoOpaque(())
        case t: Translucent[A, Boolean] => mu match
          case z: Mu.MuBoolean => t.inlineUncheckedIntoOpaque(z.myValue)
          case _               => mu.getValue
        case t: Translucent[A, Byte]    => mu match
          case b: Mu.MuByte    => t.inlineUncheckedIntoOpaque(b.myValue)
          case _               => mu.getValue
        case t: Translucent[A, Short]   => mu match
          case s: Mu.MuShort   => t.inlineUncheckedIntoOpaque(s.myValue)
          case _               => mu.getValue
        case t: Translucent[A, Char]    => mu match
          case c: Mu.MuChar    => t.inlineUncheckedIntoOpaque(c.myValue)
          case _               => mu.getValue
        case t: Translucent[A, Int]     => mu match
          case i: Mu.MuInt     => t.inlineUncheckedIntoOpaque(i.myValue)
          case _               => mu.getValue
        case t: Translucent[A, Long]    => mu match
          case l: Mu.MuLong    => t.inlineUncheckedIntoOpaque(l.myValue)
          case _               => mu.getValue
        case t: Translucent[A, Float]   => mu match
          case f: Mu.MuFloat   => t.inlineUncheckedIntoOpaque(f.myValue)
          case _               => mu.getValue
        case t: Translucent[A, Double]  => mu match
          case d: Mu.MuDouble  => t.inlineUncheckedIntoOpaque(d.myValue)
          case _               => mu.getValue
        case _ => mu.getValue
      }

  inline def :=(a: A): Unit = inline mu match
    case mut: Mu.T[A] => inline mut match
      case u: Mu.MuUnit.type => ()
      case u: Mu.T[Unit]     => ()
      case z: Mu.MuBoolean   => z.myValue = a
      case z: Mu.T[Boolean]  => z.asInstanceOf[Mu.MuBoolean].myValue = a
      case b: Mu.MuByte      => b.myValue = a
      case b: Mu.T[Byte]     => b.asInstanceOf[Mu.MuByte].myValue = a
      case s: Mu.MuShort     => s.myValue = a
      case s: Mu.T[Short]    => s.asInstanceOf[Mu.MuShort].myValue = a
      case c: Mu.MuChar      => c.myValue = a
      case c: Mu.T[Char]     => c.asInstanceOf[Mu.MuChar].myValue = a
      case i: Mu.MuInt       => i.myValue = a
      case i: Mu.T[Int]      => i.asInstanceOf[Mu.MuInt].myValue = a
      case l: Mu.MuLong      => l.myValue = a
      case l: Mu.T[Long]     => l.asInstanceOf[Mu.MuLong].myValue = a
      case f: Mu.MuFloat     => f.myValue = a
      case f: Mu.T[Float]    => f.asInstanceOf[Mu.MuFloat].myValue = a
      case d: Mu.MuDouble    => d.myValue = a
      case d: Mu.T[Double]   => d.asInstanceOf[Mu.MuDouble].myValue = a
      case _                 => summonFrom {
        case t: Translucent[A, Unit]    => ()
        case t: Translucent[A, Boolean] => mut.asInstanceOf[Mu.MuBoolean].myValue = t.inlineFromOpaque(a)
        case t: Translucent[A, Byte]    => mut.asInstanceOf[Mu.MuByte   ].myValue = t.inlineFromOpaque(a)
        case t: Translucent[A, Short]   => mut.asInstanceOf[Mu.MuShort  ].myValue = t.inlineFromOpaque(a)
        case t: Translucent[A, Char]    => mut.asInstanceOf[Mu.MuChar   ].myValue = t.inlineFromOpaque(a)
        case t: Translucent[A, Int]     => mut.asInstanceOf[Mu.MuInt    ].myValue = t.inlineFromOpaque(a)
        case t: Translucent[A, Long]    => mut.asInstanceOf[Mu.MuLong   ].myValue = t.inlineFromOpaque(a)
        case t: Translucent[A, Float]   => mut.asInstanceOf[Mu.MuFloat  ].myValue = t.inlineFromOpaque(a)
        case t: Translucent[A, Double]  => mut.asInstanceOf[Mu.MuDouble ].myValue = t.inlineFromOpaque(a)
      }
    case _ => inline erasedValue[A] match
      case _: Unit    => ()
      case _: Boolean => mu match
        case z: Mu.MuBoolean => z.myValue = a
        case _               => mu.setValue(a)
      case _: Byte    => mu match
        case b: Mu.MuByte    => b.myValue = a
        case _               => mu.setValue(a)
      case _: Short   => mu match
        case s: Mu.MuShort   => s.myValue = a
        case _               => mu.setValue(a)
      case _: Char    => mu match
        case c: Mu.MuChar    => c.myValue = a
        case _               => mu.setValue(a)
      case _: Int     => mu match
        case i: Mu.MuInt     => i.myValue = a
        case _               => mu.setValue(a)
      case _: Long    => mu match
        case l: Mu.MuLong    => l.myValue = a
        case _               => mu.setValue(a)
      case _: Float   => mu match
        case f: Mu.MuFloat   => f.myValue = a
        case _               => mu.setValue(a)
      case _: Double  => mu match
        case d: Mu.MuDouble  => d.myValue = a
        case _               => mu.setValue(a)
      case _ => summonFrom {
        case t: Translucent[A, Unit]    => ()
        case t: Translucent[A, Boolean] => mu match
          case z: Mu.MuBoolean => z.myValue = t.inlineFromOpaque(a)
          case _               => mu.setValue(a)
        case t: Translucent[A, Byte]    => mu match
          case b: Mu.MuByte    => b.myValue = t.inlineFromOpaque(a)
          case _               => mu.setValue(a)
        case t: Translucent[A, Short]   => mu match
          case s: Mu.MuShort   => s.myValue = t.inlineFromOpaque(a)
          case _               => mu.setValue(a)
        case t: Translucent[A, Char]    => mu match
          case c: Mu.MuChar    => c.myValue = t.inlineFromOpaque(a)
          case _               => mu.setValue(a)
        case t: Translucent[A, Int]     => mu match
          case i: Mu.MuInt     => i.myValue = t.inlineFromOpaque(a)
          case _               => mu.setValue(a)
        case t: Translucent[A, Long]    => mu match
          case l: Mu.MuLong    => l.myValue = t.inlineFromOpaque(a)
          case _               => mu.setValue(a)
        case t: Translucent[A, Float]   => mu match
          case f: Mu.MuFloat   => f.myValue = t.inlineFromOpaque(a)
          case _               => mu.setValue(a)
        case t: Translucent[A, Double]  => mu match
          case d: Mu.MuDouble  => d.myValue = t.inlineFromOpaque(a)
          case _               => mu.setValue(a)
        case _ => mu.setValue(a)
      }

  inline def use(inline f: A => Unit): mu.type =
    f(mu())
    mu

  inline def zap(inline f: A => A): mu.type = 
    inline mu match
      case mut: Mu.T[A] => inline mut match
        case u: Mu.MuUnit.type => ()
        case u: Mu.T[Unit]     => ()
        case z: Mu.MuBoolean   => z.myValue = f(z.myValue)
        case z: Mu.T[Boolean]  => z.asInstanceOf[Mu.MuBoolean].myValue = f(z.asInstanceOf[Mu.MuBoolean].myValue)
        case b: Mu.MuByte      => b.myValue = f(b.myValue)
        case b: Mu.T[Byte]     => b.asInstanceOf[Mu.MuByte].myValue = f(b.asInstanceOf[Mu.MuByte].myValue)
        case s: Mu.MuShort     => s.myValue = f(s.myValue)
        case s: Mu.T[Short]    => s.asInstanceOf[Mu.MuShort].myValue = f(s.asInstanceOf[Mu.MuShort].myValue)
        case c: Mu.MuChar      => c.myValue = f(c.myValue)
        case c: Mu.T[Char]     => c.asInstanceOf[Mu.MuChar].myValue = f(c.asInstanceOf[Mu.MuChar].myValue)
        case i: Mu.MuInt       => i.myValue = f(i.myValue)
        case i: Mu.T[Int]      => i.asInstanceOf[Mu.MuInt].myValue = f(i.asInstanceOf[Mu.MuInt].myValue)
        case l: Mu.MuLong      => l.myValue = f(l.myValue)
        case l: Mu.T[Long]     => l.asInstanceOf[Mu.MuLong].myValue = f(l.asInstanceOf[Mu.MuLong].myValue)
        case x: Mu.MuFloat     => x.myValue = f(x.myValue)
        case x: Mu.T[Float]    => x.asInstanceOf[Mu.MuFloat].myValue = f(x.asInstanceOf[Mu.MuFloat].myValue)
        case d: Mu.MuDouble    => d.myValue = f(d.myValue)
        case d: Mu.T[Double]   => d.asInstanceOf[Mu.MuDouble].myValue = f(d.asInstanceOf[Mu.MuDouble].myValue)
        case _                 => summonFrom {
          case t: Translucent[A, Unit]    => ()
          case t: Translucent[A, Boolean] => mut.asInstanceOf[Mu.MuBoolean].myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuBoolean].myValue)))
          case t: Translucent[A, Byte]    => mut.asInstanceOf[Mu.MuByte   ].myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuByte   ].myValue)))
          case t: Translucent[A, Short]   => mut.asInstanceOf[Mu.MuShort  ].myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuShort  ].myValue)))
          case t: Translucent[A, Char]    => mut.asInstanceOf[Mu.MuChar   ].myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuChar   ].myValue)))
          case t: Translucent[A, Int]     => mut.asInstanceOf[Mu.MuInt    ].myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuInt    ].myValue)))
          case t: Translucent[A, Long]    => mut.asInstanceOf[Mu.MuLong   ].myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuLong   ].myValue)))
          case t: Translucent[A, Float]   => mut.asInstanceOf[Mu.MuFloat  ].myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuFloat  ].myValue)))
          case t: Translucent[A, Double]  => mut.asInstanceOf[Mu.MuDouble ].myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(mut.asInstanceOf[Mu.MuDouble ].myValue)))
        }
      case _ => inline erasedValue[A] match
        case _: Unit    => ()
        case _: Boolean => mu match
          case z: Mu.MuBoolean => z.myValue = f(z.myValue)
          case _               => mu.setValue(f(mu.getValue))
        case _: Byte    => mu match
          case b: Mu.MuByte    => b.myValue = f(b.myValue)
          case _               => mu.setValue(f(mu.getValue))
        case _: Short   => mu match
          case s: Mu.MuShort   => s.myValue = f(s.myValue)
          case _               => mu.setValue(f(mu.getValue))
        case _: Char    => mu match
          case c: Mu.MuChar    => c.myValue = f(c.myValue)
          case _               => mu.setValue(f(mu.getValue))
        case _: Int     => mu match
          case i: Mu.MuInt     => i.myValue = f(i.myValue)
          case _               => mu.setValue(f(mu.getValue))
        case _: Long    => mu match
          case l: Mu.MuLong    => l.myValue = f(l.myValue)
          case _               => mu.setValue(f(mu.getValue))
        case _: Float   => mu match
          case x: Mu.MuFloat   => x.myValue = f(x.myValue)
          case _               => mu.setValue(f(mu.getValue))
        case _: Double  => mu match
          case d: Mu.MuDouble  => d.myValue = f(d.myValue)
          case _               => mu.setValue(f(mu.getValue))
        case _ => summonFrom {
          case t: Translucent[A, Unit]    => ()
          case t: Translucent[A, Boolean] => mu match
            case z: Mu.MuBoolean => z.myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(z.myValue)))
            case _               => mu.setValue(f(mu.getValue))
          case t: Translucent[A, Byte]    => mu match
            case b: Mu.MuByte    => b.myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(b.myValue)))
            case _               => mu.setValue(f(mu.getValue))
          case t: Translucent[A, Short]   => mu match
            case s: Mu.MuShort   => s.myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(s.myValue)))
            case _               => mu.setValue(f(mu.getValue))
          case t: Translucent[A, Char]    => mu match
            case c: Mu.MuChar    => c.myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(c.myValue)))
            case _               => mu.setValue(f(mu.getValue))
          case t: Translucent[A, Int]     => mu match
            case i: Mu.MuInt     => i.myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(i.myValue)))
            case _               => mu.setValue(f(mu.getValue))
          case t: Translucent[A, Long]    => mu match
            case l: Mu.MuLong    => l.myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(l.myValue)))
            case _               => mu.setValue(f(mu.getValue))
          case t: Translucent[A, Float]   => mu match
            case x: Mu.MuFloat   => x.myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(x.myValue)))
            case _               => mu.setValue(f(mu.getValue))
          case t: Translucent[A, Double]  => mu match
            case d: Mu.MuDouble  => d.myValue = t.inlineFromOpaque(f(t.inlineUncheckedIntoOpaque(d.myValue)))
            case _               => mu.setValue(f(mu.getValue))
          case _ => mu.setValue(f(mu.getValue))
        }
      mu
}

extension [A <: Mu.Primitive, M <: Mu[A]](mu: M) {
  inline def specific: Mu.Specialized[A] = inline erasedValue[A] match
    case _: Unit    => Mu.MuUnit
    case _: Boolean => mu match
      case z: Mu.MuBoolean => z
      case _               => new Mu.MuBoolean(mu.asInstanceOf[Mu[Boolean]].getValue)
    case _: Byte    => mu match
      case b: Mu.MuByte    => b
      case _               => new Mu.MuByte(   mu.asInstanceOf[Mu[Byte   ]].getValue)
    case _: Short   => mu match
      case s: Mu.MuShort   => s
      case _               => new Mu.MuShort(  mu.asInstanceOf[Mu[Short  ]].getValue)
    case _: Char    => mu match
      case c: Mu.MuChar    => c
      case _               => new Mu.MuChar(   mu.asInstanceOf[Mu[Char   ]].getValue)
    case _: Int     => mu match
      case i: Mu.MuInt     => i
      case _               => new Mu.MuInt(    mu.asInstanceOf[Mu[Int    ]].getValue)
    case _: Long    => mu match
      case l: Mu.MuLong    => l
      case _               => new Mu.MuLong(   mu.asInstanceOf[Mu[Long   ]].getValue)
    case _: Float   => mu match
      case x: Mu.MuFloat   => x
      case _               => new Mu.MuFloat(  mu.asInstanceOf[Mu[Float  ]].getValue)
    case _: Double  => mu match
      case d: Mu.MuDouble  => d
      case _               => new Mu.MuDouble( mu.asInstanceOf[Mu[Double ]].getValue)
}

extension [A <: Int | Long, M <: Mu[A]](mu: M) {
  inline def ++ : Unit = inline erasedValue[A] match
    case _: Int  => mu match
      case i: Mu.MuInt  => i.myValue += 1
      case _            => mu.setValue((mu.getValue.asInstanceOf[Int] + 1).asInstanceOf[A])
    case _: Long => mu match
      case l: Mu.MuLong => l.myValue += 1
      case _            => mu.setValue((mu.getValue.asInstanceOf[Long] + 1).asInstanceOf[A])
  
  inline def -- : Unit = inline erasedValue[A] match
    case _: Int  => mu match
      case i: Mu.MuInt  => i.myValue -= 1
      case _            => mu.setValue((mu.getValue.asInstanceOf[Int] - 1).asInstanceOf[A])
    case _: Long => mu match
      case l: Mu.MuLong => l.myValue -= 1
      case _            => mu.setValue((mu.getValue.asInstanceOf[Long] - 1).asInstanceOf[A])
}


/** Simplifies the interface to atomics
  *
  * You still need to be careful not to pull data out, modify it, and put it back in without realizing that you've missed an update.
  * Use op, zap, oldOp, or newOp depending on whether you just need to compute the new value, or whether you need the Atom, the value
  * you computed on, or the value you computed.
  */
opaque type Atom[A] <: AnyRef = AtomicInteger | AtomicLong | AtomicBoolean | AtomicReference[A]
object Atom {
  import java.lang.Double.{doubleToRawLongBits => d2l, longBitsToDouble => l2d}
  import java.lang.Float.{floatToRawIntBits => f2i, intBitsToFloat => i2f}

  inline def apply[A](a: A): kse.basics.Atom[A] = inline a match
    case i: Int     => new AtomicInteger(i)
    case l: Long    => new AtomicLong(l)
    case b: Byte    => new AtomicInteger(b)
    case s: Short   => new AtomicInteger(s)
    case c: Char    => new AtomicInteger(c)
    case f: Float   => new AtomicInteger(f2i(f))
    case d: Double  => new AtomicLong(d2l(d))
    case z: Boolean => new AtomicBoolean(z)
    case x: AnyRef  => new AtomicReference(x)
    case _ => summonFrom{
      case _: Translucent[A, Int]     => new AtomicInteger(a.asInstanceOf[Int])
      case _: Translucent[A, Long]    => new AtomicLong(a.asInstanceOf[Long])
      case _: Translucent[A, Byte]    => new AtomicInteger(a.asInstanceOf[Byte].toInt) 
      case _: Translucent[A, Short]   => new AtomicInteger(a.asInstanceOf[Short].toInt)
      case _: Translucent[A, Char]    => new AtomicInteger(a.asInstanceOf[Char].toInt)
      case _: Translucent[A, Float]   => new AtomicInteger(f2i(a.asInstanceOf[Float]))
      case _: Translucent[A, Double]  => new AtomicLong(d2l(a.asInstanceOf[Double]))
      case _: Translucent[A, Boolean] => new AtomicBoolean(a.asInstanceOf[Boolean])
      case _ => compiletime.error("No Atomic support for values of this type")
    }

  extension [A](a: Atom[A]) {
    transparent inline def underlying: AnyRef = inline erasedValue[A] match
      case _: Int => a.asInstanceOf[AtomicInteger]
      case _: Long => a.asInstanceOf[AtomicLong]
      case _: Boolean => a.asInstanceOf[AtomicBoolean]
      case _: AnyRef => a.asInstanceOf[AtomicReference[A]]
      case _ => compiletime.error("The underlying instance is virtually wrapped")

    inline def apply(): A = inline erasedValue[A] match
      case _: Int     => a.asInstanceOf[AtomicInteger].get().asInstanceOf[A]
      case _: Long    => a.asInstanceOf[AtomicLong   ].get().asInstanceOf[A]
      case _: Byte    => a.asInstanceOf[AtomicInteger].get().toByte .asInstanceOf[A]
      case _: Short   => a.asInstanceOf[AtomicInteger].get().toShort.asInstanceOf[A]
      case _: Char    => a.asInstanceOf[AtomicInteger].get().toChar .asInstanceOf[A]
      case _: Float   => i2f(a.asInstanceOf[AtomicInteger].get()).asInstanceOf[A]
      case _: Double  => l2d(a.asInstanceOf[AtomicLong   ].get()).asInstanceOf[A]
      case _: Boolean => a.asInstanceOf[AtomicBoolean  ].get().asInstanceOf[A]
      case _: AnyRef  => a.asInstanceOf[AtomicReference[A]].get()
      case _ => summonFrom{
        case _: Translucent[A, Int]     => a.asInstanceOf[AtomicInteger].get().asInstanceOf[A] 
        case _: Translucent[A, Long]    => a.asInstanceOf[AtomicLong   ].get().asInstanceOf[A]
        case _: Translucent[A, Byte]    => a.asInstanceOf[AtomicInteger].get().toByte .asInstanceOf[A]
        case _: Translucent[A, Short]   => a.asInstanceOf[AtomicInteger].get().toShort.asInstanceOf[A]
        case _: Translucent[A, Char]    => a.asInstanceOf[AtomicInteger].get().toChar .asInstanceOf[A]
        case _: Translucent[A, Float]   => i2f(a.asInstanceOf[AtomicInteger].get()).asInstanceOf[A]
        case _: Translucent[A, Double]  => l2d(a.asInstanceOf[AtomicLong   ].get()).asInstanceOf[A]
        case _: Translucent[A, Boolean] => a.asInstanceOf[AtomicBoolean  ].get().asInstanceOf[A]
        case _ => compiletime.error("Context missing to support atomic operations on values of this type")
      }

    inline def :=(x: A): Unit = inline x match
      case i: Int     => a.asInstanceOf[AtomicInteger].set(i)
      case l: Long    => a.asInstanceOf[AtomicLong   ].set(l)
      case b: Byte    => a.asInstanceOf[AtomicInteger].set(b)
      case s: Short   => a.asInstanceOf[AtomicInteger].set(s)
      case c: Char    => a.asInstanceOf[AtomicInteger].set(c)
      case f: Float   => a.asInstanceOf[AtomicInteger].set(f2i(f))
      case d: Double  => a.asInstanceOf[AtomicLong   ].set(d2l(d))
      case z: Boolean => a.asInstanceOf[AtomicBoolean ].set(z)
      case _: AnyRef  => a.asInstanceOf[AtomicReference[A]].set(x)
      case _ => summonFrom{
        case _: Translucent[A, Int]     => a.asInstanceOf[AtomicInteger].set(x.asInstanceOf[Int])
        case _: Translucent[A, Long]    => a.asInstanceOf[AtomicLong   ].set(x.asInstanceOf[Long])
        case _: Translucent[A, Byte]    => a.asInstanceOf[AtomicInteger].set(x.asInstanceOf[Byte].toInt)
        case _: Translucent[A, Short]   => a.asInstanceOf[AtomicInteger].set(x.asInstanceOf[Short].toInt)
        case _: Translucent[A, Char]    => a.asInstanceOf[AtomicInteger].set(x.asInstanceOf[Char].toInt)
        case _: Translucent[A, Float]   => a.asInstanceOf[AtomicInteger].set(f2i(x.asInstanceOf[Float]))
        case _: Translucent[A, Double]  => a.asInstanceOf[AtomicLong   ].set(d2l(x.asInstanceOf[Double]))
        case _: Translucent[A, Boolean] => a.asInstanceOf[AtomicBoolean].set(x.asInstanceOf[Boolean])
        case _ => compiletime.error("Context missing to support atomic operations on values of this type")
      }

    inline infix def swap(x: A): A = inline x match
      case i: Int     => a.asInstanceOf[AtomicInteger].getAndSet(i).asInstanceOf[A]
      case l: Long    => a.asInstanceOf[AtomicLong   ].getAndSet(l).asInstanceOf[A]
      case b: Byte    => a.asInstanceOf[AtomicInteger].getAndSet(b).toByte .asInstanceOf[A]
      case s: Short   => a.asInstanceOf[AtomicInteger].getAndSet(s).toShort.asInstanceOf[A]
      case c: Char    => a.asInstanceOf[AtomicInteger].getAndSet(c).toChar .asInstanceOf[A]
      case f: Float   => i2f(a.asInstanceOf[AtomicInteger].getAndSet(f2i(f))).asInstanceOf[A]
      case d: Double  => l2d(a.asInstanceOf[AtomicLong   ].getAndSet(d2l(d))).asInstanceOf[A]
      case z: Boolean => a.asInstanceOf[AtomicBoolean  ].getAndSet(z).asInstanceOf[A]
      case _: AnyRef  => a.asInstanceOf[AtomicReference[A]].getAndSet(x)
      case _ => summonFrom{
        case _: Translucent[A, Int]     => a.asInstanceOf[AtomicInteger].getAndSet(x.asInstanceOf[Int ]).asInstanceOf[A]
        case _: Translucent[A, Long]    => a.asInstanceOf[AtomicLong   ].getAndSet(x.asInstanceOf[Long]).asInstanceOf[A]
        case _: Translucent[A, Byte]    => a.asInstanceOf[AtomicInteger].getAndSet(x.asInstanceOf[Byte ].toInt).toByte .asInstanceOf[A]
        case _: Translucent[A, Short]   => a.asInstanceOf[AtomicInteger].getAndSet(x.asInstanceOf[Short].toInt).toShort.asInstanceOf[A]
        case _: Translucent[A, Char]    => a.asInstanceOf[AtomicInteger].getAndSet(x.asInstanceOf[Char ].toInt).toChar .asInstanceOf[A]
        case _: Translucent[A, Float]   => i2f(a.asInstanceOf[AtomicInteger].getAndSet(f2i(x.asInstanceOf[Float ]))).asInstanceOf[A]
        case _: Translucent[A, Double]  => l2d(a.asInstanceOf[AtomicLong   ].getAndSet(d2l(x.asInstanceOf[Double]))).asInstanceOf[A]
        case _: Translucent[A, Boolean] => a.asInstanceOf[AtomicBoolean  ].getAndSet(x.asInstanceOf[Boolean]).asInstanceOf[A]
        case _ => compiletime.error("Context missing to support atomic operations on values of this type")
      }

    inline def oldOp(inline f: A => A): A = inline erasedValue[A] match
      case _: Int =>
        var x = a.asInstanceOf[AtomicInteger].get().asInstanceOf[A]
        while !a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Int], f(x).asInstanceOf[Int]) do
          x = a.asInstanceOf[AtomicInteger].get().asInstanceOf[A]
        x
      case _: Long =>
        var x = a.asInstanceOf[AtomicLong].get().asInstanceOf[A]
        while !a.asInstanceOf[AtomicLong].compareAndSet(x.asInstanceOf[Long], f(x).asInstanceOf[Long]) do
          x = a.asInstanceOf[AtomicLong].get().asInstanceOf[A]
        x
      case _: Byte =>
        var x = a.asInstanceOf[AtomicInteger].get().toByte.asInstanceOf[A]
        while !a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Byte].toInt, f(x).asInstanceOf[Byte].toInt) do
          x = a.asInstanceOf[AtomicInteger].get().toByte.asInstanceOf[A]
        x
      case _: Short =>
        var x = a.asInstanceOf[AtomicInteger].get().toShort.asInstanceOf[A]
        while !a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Short].toInt, f(x).asInstanceOf[Short].toInt) do
          x = a.asInstanceOf[AtomicInteger].get().toShort.asInstanceOf[A]
        x
      case _: Char =>
        var x = a.asInstanceOf[AtomicInteger].get().toChar.asInstanceOf[A]
        while !a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Char].toInt, f(x).asInstanceOf[Char].toInt) do
          x = a.asInstanceOf[AtomicInteger].get().toChar.asInstanceOf[A]
        x
      case _: Float =>
        var x = i2f(a.asInstanceOf[AtomicInteger].get()).asInstanceOf[A]
        while !a.asInstanceOf[AtomicInteger].compareAndSet(f2i(x.asInstanceOf[Float]), f2i(f(x).asInstanceOf[Float])) do
          x = i2f(a.asInstanceOf[AtomicInteger].get()).asInstanceOf[A]
        x
      case _: Double =>
        var x = a.asInstanceOf[AtomicLong].get()
        while !a.asInstanceOf[AtomicLong].compareAndSet(x, d2l(f(l2d(x).asInstanceOf[A]).asInstanceOf[Double])) do
          x = a.asInstanceOf[AtomicLong].get()
        l2d(x).asInstanceOf[A]
      case _: Boolean =>
        var x = a.asInstanceOf[AtomicBoolean].get().asInstanceOf[A]
        while !a.asInstanceOf[AtomicBoolean].compareAndSet(x.asInstanceOf[Boolean], f(x).asInstanceOf[Boolean]) do
          x = a.asInstanceOf[AtomicBoolean].get().asInstanceOf[A]
        x
      case _: AnyRef =>
        var x = a.asInstanceOf[AtomicReference[A]].get()
        while !a.asInstanceOf[AtomicReference[A]].compareAndSet(x, f(x)) do
          x = a.asInstanceOf[AtomicReference[A]].get()
        x
      case _ => summonFrom{
        case _: Translucent[A, Int] =>
          var x = a.asInstanceOf[AtomicInteger].get()
          while !a.asInstanceOf[AtomicInteger].compareAndSet(x, f(x.asInstanceOf[A]).asInstanceOf[Int]) do
            x = x.asInstanceOf[AtomicInteger].get()
          x.asInstanceOf[A]
        case _: Translucent[A, Long] =>
          var x = a.asInstanceOf[AtomicLong].get()
          while !a.asInstanceOf[AtomicLong].compareAndSet(x, f(x.asInstanceOf[A]).asInstanceOf[Long]) do
            x = x.asInstanceOf[AtomicLong].get()
          x.asInstanceOf[A]
        case _: Translucent[A, Byte] =>
          var x = a.asInstanceOf[AtomicInteger].get()
          while !a.asInstanceOf[AtomicInteger].compareAndSet(x, f(x.toByte.asInstanceOf[A]).asInstanceOf[Byte].toInt) do
            x = x.asInstanceOf[AtomicInteger].get()
          x.toByte.asInstanceOf[A]
        case _: Translucent[A, Short] =>
          var x = a.asInstanceOf[AtomicInteger].get()
          while !a.asInstanceOf[AtomicInteger].compareAndSet(x, f(x.toShort.asInstanceOf[A]).asInstanceOf[Short].toInt) do
            x = x.asInstanceOf[AtomicInteger].get()
          x.toShort.asInstanceOf[A]
        case _: Translucent[A, Char] =>
          var x = a.asInstanceOf[AtomicInteger].get()
          while !a.asInstanceOf[AtomicInteger].compareAndSet(x, f(x.toChar.asInstanceOf[A]).asInstanceOf[Char].toInt) do
            x = x.asInstanceOf[AtomicInteger].get()
          x.toChar.asInstanceOf[A]
        case _: Translucent[A, Float] =>
          var x = a.asInstanceOf[AtomicInteger].get()
          while !a.asInstanceOf[AtomicInteger].compareAndSet(x, f2i(f(i2f(x).asInstanceOf[A]).asInstanceOf[Float])) do
            x = x.asInstanceOf[AtomicInteger].get()
          i2f(x).asInstanceOf[A]
        case _: Translucent[A, Double] =>
          var x = a.asInstanceOf[AtomicLong].get()
          while !a.asInstanceOf[AtomicLong].compareAndSet(x, d2l(f(l2d(x).asInstanceOf[A]).asInstanceOf[Double])) do
            x = x.asInstanceOf[AtomicLong].get()
          l2d(x).asInstanceOf[A]
        case _: Translucent[A, Boolean] =>
          var x = a.asInstanceOf[AtomicBoolean].get()
          while !a.asInstanceOf[AtomicBoolean].compareAndSet(x, f(x.asInstanceOf[A]).asInstanceOf[Boolean]) do
            x = a.asInstanceOf[AtomicBoolean].get()
          x.asInstanceOf[A]
        case _ => compiletime.error("Context missing to support atomic operations on values of this type")
      }

    inline def newOp(inline f: A => A): A = inline erasedValue[A] match
      case _: Int =>
        var x = a.asInstanceOf[AtomicInteger].get().asInstanceOf[A]
        while { val y = f(x); val done = a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Int], y.asInstanceOf[Int]); if done then x = y; !done } do
          x = a.asInstanceOf[AtomicInteger].get().asInstanceOf[A]
        x
      case _: Long =>
        var x = a.asInstanceOf[AtomicLong].get().asInstanceOf[A]
        while { val y = f(x); val done = a.asInstanceOf[AtomicLong].compareAndSet(x.asInstanceOf[Long], y.asInstanceOf[Long]); if done then x = y; !done } do
          x = a.asInstanceOf[AtomicLong].get().asInstanceOf[A]
        x
      case _: Byte =>
        var x = a.asInstanceOf[AtomicInteger].get().toByte.asInstanceOf[A]
        while { val y = f(x); val done = a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Byte].toInt, y.asInstanceOf[Byte].toInt); if done then x = y; !done } do
          x = a.asInstanceOf[AtomicInteger].get().toByte.asInstanceOf[A]
        x
      case _: Short =>
        var x = a.asInstanceOf[AtomicInteger].get().toShort.asInstanceOf[A]
        while { val y = f(x); val done = a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Short].toInt, y.asInstanceOf[Short].toInt); if done then x = y; !done } do
          x = a.asInstanceOf[AtomicInteger].get().toShort.asInstanceOf[A]
        x
      case _: Char =>
        var x = a.asInstanceOf[AtomicInteger].get().toChar.asInstanceOf[A]
        while { val y = f(x); val done = a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Char].toInt, y.asInstanceOf[Char].toInt); if done then x = y; !done } do
          x = a.asInstanceOf[AtomicInteger].get().toChar.asInstanceOf[A]
        x
      case _: Float =>
        var x = i2f(a.asInstanceOf[AtomicInteger].get()).asInstanceOf[A]
        while { val y = f(x); val done = a.asInstanceOf[AtomicInteger].compareAndSet(f2i(x.asInstanceOf[Float]), f2i(y.asInstanceOf[Float])); if done then x = y; !done } do
          x = i2f(a.asInstanceOf[AtomicInteger].get()).asInstanceOf[A]
        x
      case _: Double =>
        var x = l2d(a.asInstanceOf[AtomicLong].get()).asInstanceOf[A]
        while { val y = f(x); val done = a.asInstanceOf[AtomicLong].compareAndSet(d2l(x.asInstanceOf[Double]), d2l(y.asInstanceOf[Double])); if done then x = y; !done } do
          x = l2d(a.asInstanceOf[AtomicLong].get()).asInstanceOf[A]
        x
      case _: Boolean =>
        var x = a.asInstanceOf[AtomicBoolean].get().asInstanceOf[A]
        while { val y = f(x); val done = a.asInstanceOf[AtomicBoolean].compareAndSet(x.asInstanceOf[Boolean], y.asInstanceOf[Boolean]); if done then x = y; !done } do
          x = a.asInstanceOf[AtomicBoolean].get().asInstanceOf[A] 
        x
      case _: AnyRef => boundary[A]:
        var x = a.asInstanceOf[AtomicReference[A]].get()
        while { val y = f(x); val done = a.asInstanceOf[AtomicReference[A]].compareAndSet(x, y); if done then x = y; !done } do
          x = a.asInstanceOf[AtomicReference[A]].get()
        x
      case _ => summonFrom{
        case _: Translucent[A, Int] =>
          var x = a.asInstanceOf[AtomicInteger].get().asInstanceOf[A]
          while { val y = f(x); val done = a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Int], y.asInstanceOf[Int]); if done then x = y; !done } do
            x = a.asInstanceOf[AtomicInteger].get().asInstanceOf[A]
          x
        case _: Translucent[A, Long] =>
          var x = a.asInstanceOf[AtomicLong].get().asInstanceOf[A]
          while { val y = f(x); val done = a.asInstanceOf[AtomicLong].compareAndSet(x.asInstanceOf[Long], y.asInstanceOf[Long]); if done then x = y; !done } do
            x = a.asInstanceOf[AtomicLong].get().asInstanceOf[A]
          x
        case _: Translucent[A, Byte] =>
          var x = a.asInstanceOf[AtomicInteger].get().toByte
          while { val y = f(x.asInstanceOf[A]).asInstanceOf[Byte]; val done = a.asInstanceOf[AtomicInteger].compareAndSet(x.toInt, y.toInt); if done then x = y; !done } do
            x = a.asInstanceOf[AtomicInteger].get().toByte
          x.asInstanceOf[A]
        case _: Translucent[A, Short] =>
          var x = a.asInstanceOf[AtomicInteger].get().toShort
          while { val y = f(x.asInstanceOf[A]).asInstanceOf[Short]; val done = a.asInstanceOf[AtomicInteger].compareAndSet(x.toInt, y.toInt); if done then x = y; !done } do
            x = a.asInstanceOf[AtomicInteger].get().toShort
          x.asInstanceOf[A]
        case _: Translucent[A, Char] =>
          var x = a.asInstanceOf[AtomicInteger].get().toChar
          while { val y = f(x.asInstanceOf[A]).asInstanceOf[Char]; val done = a.asInstanceOf[AtomicInteger].compareAndSet(x.toInt, y.toInt); if done then x = y; !done } do
            x = a.asInstanceOf[AtomicInteger].get().toChar
          x.asInstanceOf[A]
        case _: Translucent[A, Float] =>
          var x = i2f(a.asInstanceOf[AtomicInteger].get())
          while { val y = f(x.asInstanceOf[A]).asInstanceOf[Float]; val done = a.asInstanceOf[AtomicInteger].compareAndSet(f2i(x), f2i(y)); if done then x = y; !done } do
            x = i2f(a.asInstanceOf[AtomicInteger].get())
          x.asInstanceOf[A]
        case _: Translucent[A, Double] =>
          var x = l2d(a.asInstanceOf[AtomicLong].get())
          while { val y = f(x.asInstanceOf[A]).asInstanceOf[Double]; val done = a.asInstanceOf[AtomicLong].compareAndSet(d2l(x), d2l(y)); if done then x = y; !done } do
            x = l2d(a.asInstanceOf[AtomicLong].get())
          x.asInstanceOf[A]
        case _: Translucent[A, Boolean] =>
          var x = a.asInstanceOf[AtomicBoolean].get()
          while { val y = f(x.asInstanceOf[A]).asInstanceOf[Boolean]; val done = a.asInstanceOf[AtomicBoolean].compareAndSet(x, y); if done then x = y; !done } do
            x = a.asInstanceOf[AtomicBoolean].get()
          x.asInstanceOf[A]
        case _ => compiletime.error("Context missing to support atomic operations on values of this type")
      }

    inline def op(inline f: A => A): Unit = inline erasedValue[A] match
      case _: Int =>
        var x = a.asInstanceOf[AtomicInteger].get().asInstanceOf[A]
        while !a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Int], f(x).asInstanceOf[Int]) do
          x = a.asInstanceOf[AtomicInteger].get().asInstanceOf[A]
      case _: Long =>
        var x = a.asInstanceOf[AtomicLong].get().asInstanceOf[A]
        while !a.asInstanceOf[AtomicLong].compareAndSet(x.asInstanceOf[Long], f(x).asInstanceOf[Long]) do
          x = a.asInstanceOf[AtomicLong].get().asInstanceOf[A]
      case _: Byte =>
        var x = a.asInstanceOf[AtomicInteger].get().toByte.asInstanceOf[A]
        while !a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Byte].toInt, f(x).asInstanceOf[Byte].toInt) do
          x = a.asInstanceOf[AtomicInteger].get().toByte.asInstanceOf[A]
      case _: Short =>
        var x = a.asInstanceOf[AtomicInteger].get().toShort.asInstanceOf[A]
        while !a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Short].toInt, f(x).asInstanceOf[Short].toInt) do
          x = a.asInstanceOf[AtomicInteger].get().toShort.asInstanceOf[A]
      case _: Char =>
        var x = a.asInstanceOf[AtomicInteger].get().toChar.asInstanceOf[A]
        while !a.asInstanceOf[AtomicInteger].compareAndSet(x.asInstanceOf[Char].toInt, f(x).asInstanceOf[Char].toInt) do
          x = a.asInstanceOf[AtomicInteger].get().toChar.asInstanceOf[A]
      case _: Float =>
        var x = i2f(a.asInstanceOf[AtomicInteger].get()).asInstanceOf[A]
        while !a.asInstanceOf[AtomicInteger].compareAndSet(f2i(x.asInstanceOf[Float]), f2i(f(x).asInstanceOf[Float])) do
          x = i2f(a.asInstanceOf[AtomicInteger].get()).asInstanceOf[A]
      case _: Double =>
        var x = a.asInstanceOf[AtomicLong].get()
        while !a.asInstanceOf[AtomicLong].compareAndSet(x, d2l(f(l2d(x).asInstanceOf[A]).asInstanceOf[Double])) do
          x = a.asInstanceOf[AtomicLong].get()
      case _: Boolean =>
        var x = a.asInstanceOf[AtomicBoolean].get().asInstanceOf[A]
        while !a.asInstanceOf[AtomicBoolean].compareAndSet(x.asInstanceOf[Boolean], f(x).asInstanceOf[Boolean]) do
          x = a.asInstanceOf[AtomicBoolean].get().asInstanceOf[A]
      case _: AnyRef =>
        var x = a.asInstanceOf[AtomicReference[A]].get()
        while !a.asInstanceOf[AtomicReference[A]].compareAndSet(x, f(x)) do
          x = a.asInstanceOf[AtomicReference[A]].get()
      case _ => summonFrom{
        case _: Translucent[A, Int] =>
          var x = a.asInstanceOf[AtomicInteger].get()
          while !a.asInstanceOf[AtomicInteger].compareAndSet(x, f(x.asInstanceOf[A]).asInstanceOf[Int]) do
            x = x.asInstanceOf[AtomicInteger].get()
        case _: Translucent[A, Long] =>
          var x = a.asInstanceOf[AtomicLong].get()
          while !a.asInstanceOf[AtomicLong].compareAndSet(x, f(x.asInstanceOf[A]).asInstanceOf[Long]) do
            x = x.asInstanceOf[AtomicLong].get()
        case _: Translucent[A, Byte] =>
          var x = a.asInstanceOf[AtomicInteger].get()
          while !a.asInstanceOf[AtomicInteger].compareAndSet(x, f(x.toByte.asInstanceOf[A]).asInstanceOf[Byte].toInt) do
            x = x.asInstanceOf[AtomicInteger].get()
        case _: Translucent[A, Short] =>
          var x = a.asInstanceOf[AtomicInteger].get()
          while !a.asInstanceOf[AtomicInteger].compareAndSet(x, f(x.toShort.asInstanceOf[A]).asInstanceOf[Short].toInt) do
            x = x.asInstanceOf[AtomicInteger].get()
        case _: Translucent[A, Char] =>
          var x = a.asInstanceOf[AtomicInteger].get()
          while !a.asInstanceOf[AtomicInteger].compareAndSet(x, f(x.toChar.asInstanceOf[A]).asInstanceOf[Char].toInt) do
            x = x.asInstanceOf[AtomicInteger].get()
        case _: Translucent[A, Float] =>
          var x = a.asInstanceOf[AtomicInteger].get()
          while !a.asInstanceOf[AtomicInteger].compareAndSet(x, f2i(f(i2f(x).asInstanceOf[A]).asInstanceOf[Float])) do
            x = x.asInstanceOf[AtomicInteger].get()
        case _: Translucent[A, Double] =>
          var x = a.asInstanceOf[AtomicLong].get()
          while !a.asInstanceOf[AtomicLong].compareAndSet(x, d2l(f(l2d(x).asInstanceOf[A]).asInstanceOf[Double])) do
            x = x.asInstanceOf[AtomicLong].get()
        case _: Translucent[A, Boolean] =>
          var x = a.asInstanceOf[AtomicBoolean].get()
          while !a.asInstanceOf[AtomicBoolean].compareAndSet(x, f(x.asInstanceOf[A]).asInstanceOf[Boolean]) do
            x = a.asInstanceOf[AtomicBoolean].get()
        case _ => compiletime.error("Context missing to support atomic operations on values of this type")
      }

    inline def zap(inline f: A => A): a.type =
      a.op(f)
      a
  }

  extension [A <: Int | Long](a: Atom[A]) {
    inline def ++ : Unit = inline erasedValue[A] match
      case _: Int  => a.asInstanceOf[AtomicInteger].incrementAndGet()
      case _: Long => a.asInstanceOf[AtomicLong].incrementAndGet()
    
    inline def -- : Unit = inline erasedValue[A] match
      case _: Int  => a.asInstanceOf[AtomicInteger].decrementAndGet()
      case _: Long => a.asInstanceOf[AtomicLong].decrementAndGet()
  }

  opaque type Count = LongAdder
  object Count {
    inline def apply(): kse.basics.Atom.Count = new LongAdder()
    def from(l: Long): kse.basics.Atom.Count =
      val a = new LongAdder()
      a.add(l)
      a

    extension (c: Count)
      inline def underlying: LongAdder = c
      inline def apply(): Long = c.sum()
      inline def :=(l: Long): Unit =
        c.reset()
        c.add(l)
      inline def ++ : Unit = c.increment()
      inline def -- : Unit = c.decrement()
      inline def swap(l: Long): Long =
        val ans = c.sumThenReset()
        c.add(l)
        ans
      inline def +=(l: Long): Unit = c.add(l)
      inline def -=(l: Long): Unit = c.add(-l)
  }

  opaque type Toggle = AtomicBoolean
  object Toggle {
    inline def apply(): kse.basics.Atom.Toggle = new AtomicBoolean()
    inline def apply(b: Boolean): kse.basics.Atom.Toggle = new AtomicBoolean(b)

    extension (t: Toggle)
      inline def underlying: AtomicBoolean = t
      inline def apply(): Boolean = (t: AtomicBoolean).get()
      inline def on(): Unit = (t: AtomicBoolean).set(true)
      inline def off(): Unit = (t: AtomicBoolean).set(false)
      inline def turnOn(): Boolean = (t: AtomicBoolean).compareAndSet(false, true)
      inline def turnOff(): Boolean = (t: AtomicBoolean).compareAndSet(true, false)
  }
}


/** Hides data from case classes, etc.
  *
  * Do NOT use in hash maps!  Every anonymous value looks like every other!
  */
final class Anon[A](val value: A) {
  def map[B](f: A => B) = new Anon(f(value))
  inline def use(f: A => Unit): this.type = { f(value); this }
  override def toString = "..."
  override def hashCode = 1239182
  override def equals(a: Any) = a match {
    case _: Anon[?] => true
    case _ => false
  }
}
object Anon {
  def apply[A](a: A) = new Anon(a)

  given [A](using Copies[A]): Copies[Anon[A]] with
    def copy(a: Anon[A]): Anon[A] = new Anon(summon[Copies[A]].copy(a.value))
}


/** Box that uses reference equality and identity hash code */
final class Identity[A](val value: A) {
  def map[B](f: A => B) = new Identity(f(value))
  inline def use(f: A => Unit): this.type = { f(value); this }
  override def toString = value.toString
  override def hashCode = java.lang.System.identityHashCode(value)
  override def equals(a: Any) = a match
    case i: Identity[?] => i.value.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef] 
    case _ => false
}
object Identity {
  given [A](using Copies[A]): Copies[Identity[A]] with
    def copy(i: Identity[A]): Identity[A] = new Identity(summon[Copies[A]].copy(i.value))
}



//////////////////////////////////////////////////////////////////////////////////
/// Generally helpful evaluation/execution utilities for singletons and tuples ///
//////////////////////////////////////////////////////////////////////////////////

extension[A <: AnyRef](a: A)
  /** Easier way to get the identity hash on an object. */
  inline def identityHash: Int = java.lang.System.identityHashCode(a)


/** Identity function, except inlined. */
inline def __[A](a: A): A = a


extension [A](a: A) {
  /** Apply a function to this value and return the result.  Same as `pipe`. */
  inline def fn[B](inline f: A => B): B = f(a)

  /** Apply a function to this value and return the result.  Same as `pipe`. */
  inline def pipe[B](inline f: A => B): B = f(a)

  /** Apply a side-effecting function to this value; return the original value */
  inline def tap(inline f: A => Unit): A = { f(a); a }

  /** Apply a test and alter the value if it passes */
  inline def fixIf(inline p: A => Boolean)(inline f: A => A): A = if p(a) then f(a) else a

  inline def |->[Z](inline op: A => Z): Z = op(a)
  inline def |->[B, Z](inline opb: ((A, B) => Z, B)) = basicsMacroImpl.applyWithoutBoxing2(a, opb)
  inline def |->[B, C, Z](inline opbc: ((A, B, C) => Z, B, C)) = basicsMacroImpl.applyWithoutBoxing3(a, opbc)
  inline def |->[B, C, D, Z](inline opbcd: ((A, B, C, D) => Z, B, C, D)) = basicsMacroImpl.applyWithoutBoxing4(a, opbcd) 
  inline def |->[B, C, D, E, Z](inline opbcde: ((A, B, C, D, E) => Z, B, C, D, E)) = basicsMacroImpl.applyWithoutBoxing5(a, opbcde) 
  inline def |->[B, C, D, E, F, Z](inline opbcdef: ((A, B, C, D, E, F) => Z, B, C, D, E, F)) = basicsMacroImpl.applyWithoutBoxing6(a, opbcdef) 
  inline def |->[B, C, D, E, F, G, Z](inline opbcdefg: ((A, B, C, D, E, F, G) => Z, B, C, D, E, F, G)) = basicsMacroImpl.applyWithoutBoxing7(a, opbcdefg) 
  inline def |->[B, C, D, E, F, G, H, Z](inline opbcdefgh: ((A, B, C, D, E, F, G, H) => Z, B, C, D, E, F, G, H)) = basicsMacroImpl.applyWithoutBoxing8(a, opbcdefgh) 
  inline def |->[B, C, D, E, F, G, H, I, Z](inline opbcdefghi: ((A, B, C, D, E, F, G, H, I) => Z, B, C, D, E, F, G, H, I)) = basicsMacroImpl.applyWithoutBoxing9(a, opbcdefghi) 
}
