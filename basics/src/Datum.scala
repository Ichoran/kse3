// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2011-15, 2021-25 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.

package kse.basics


//import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference, LongAdder}

import scala.annotation.targetName
import scala.compiletime.{erasedValue, summonFrom}
import scala.reflect.ClassTag
import scala.util.boundary

import scala.collection.immutable.{Range => Rg}

import kse.basics.intervals._



extension (ivx: Iv.X) {
  inline def of(inline target: Int | String | Array[?] | Iv): Iv = inline ivx match
    case iv:  Iv     => iv
    case raa: Iv.Raa => raa.iv
    case rae: Iv.Rae => rae.iv(target)
    case ras: Iv.Ras => ras.iv(target)
    case rea: Iv.Rea => rea.iv(target)
    case ree: Iv.Ree => ree.iv(target)
    case res: Iv.Res => res.iv(target)
    case rsa: Iv.Rsa => rsa.iv(target)
    case rse: Iv.Rse => rse.iv(target)
    case rss: Iv.Rss => rss.iv(target)
    case rxy: Iv.Rxy => rxy.iv(target)
  inline def index0(inline target: Int | String | Array[?] | Iv): Int = inline ivx match
    case iv:  Iv     => iv.i0
    case raa: Iv.Raa => raa.i0
    case rae: Iv.Rae => rae.i0
    case ras: Iv.Ras => ras.i0
    case rea: Iv.Rea => rea.i0(target)
    case ree: Iv.Ree => ree.i0(target)
    case res: Iv.Res => res.i0(target)
    case rsa: Iv.Rsa => rsa.i0(target)
    case rse: Iv.Rse => rse.i0(target)
    case rss: Iv.Rss => rss.i0(target)
    case rxy: Iv.Rxy => rxy.i0(target)
  inline def indexN(inline target: Int | String | Array[?] | Iv): Int = inline ivx match
    case iv:  Iv     => iv.iN
    case raa: Iv.Raa => Iv.up(raa.i1)
    case rae: Iv.Rae => Iv.up(rae.i1(target))
    case ras: Iv.Ras => Iv.up(ras.i1(target))
    case rea: Iv.Rea => Iv.up(rea.i1)
    case ree: Iv.Ree => Iv.up(ree.i1(target))
    case res: Iv.Res => Iv.up(res.i1(target))
    case rsa: Iv.Rsa => Iv.up(rsa.i1)
    case rse: Iv.Rse => Iv.up(rse.i1(target))
    case rss: Iv.Rss => Iv.up(rss.i1(target))
    case rxy: Iv.Rxy => Iv.up(rxy.i1(target))
}

extension(ivy: Iv.Y) {
  transparent inline def zero = inline ivy match
    case iv:  Iv     => iv.i0
    case raa: Iv.Raa => raa.i0
    case rae: Iv.Rae => rae.i0
    case ras: Iv.Ras => ras.i0
    case rea: Iv.Rea => rea.first
    case ree: Iv.Ree => ree.first
    case res: Iv.Res => res.first
    case rsa: Iv.Rsa => rsa.first
    case rse: Iv.Rse => rse.first
    case rss: Iv.Rss => rss.first
  transparent inline def one = inline ivy match
    case iv:  Iv     => Iv.dn(iv.iN)
    case raa: Iv.Raa => raa.i1
    case rae: Iv.Rae => rae.last
    case ras: Iv.Ras => ras.last
    case rea: Iv.Rea => rea.i1
    case ree: Iv.Ree => ree.last
    case res: Iv.Res => res.last
    case rsa: Iv.Rsa => rsa.i1
    case rse: Iv.Rse => rse.last
    case rss: Iv.Rss => rss.last
}


extension (i: Int) {
  @targetName("to_Range") inline infix def to(j: Int): collection.immutable.Range.Inclusive = scala.runtime.RichInt(i).to(j)

  @targetName("to_End_type") inline infix def to(end: End.type): Iv.Rae = Iv.Rae.fromValues(i, 0)

  @targetName("to_End_At") inline infix def to(end: End.At): Iv.Rae = Iv.Rae.fromValues(i, end.unwrap)

  @targetName("to_Start_type") inline infix def to(end: Start.type): Iv.Ras = Iv.Ras.fromValues(i, 0)

  @targetName("to_Start_At") inline infix def to(end: Start.At): Iv.Ras = Iv.Ras.fromValues(i, end.unwrap)

  inline def ꓺ    = Iv.Rxy.fromValues(i, -1)
  inline def `..` = Iv.Rxy.fromValues(i, -1)

  inline infix def ꓺ(   j: Int) = Iv.Rxy.fromValues(i, if (j & 0x7FFFFFFF) == 0 then j else j-1)
  inline infix def `..`(j: Int) = Iv.Rxy.fromValues(i, if (j & 0x7FFFFFFF) == 0 then j else j-1)

  inline infix def ꓺꘌ(   j: Int) = Iv.Rxy.fromValues(i, j)
  inline infix def `..=`(j: Int) = Iv.Rxy.fromValues(i, j)

  inline infix def ꓺ(   end: End.type) = Iv.Rxy.fromValues(i, -1)
  inline infix def `..`(end: End.type) = Iv.Rxy.fromValues(i, -1)

  inline def of[A](using ClassTag[A]): Array[A] =
    new Array[A](if i > 0 then i else 0)
}

inline def ꓺ:    Iv.Rxy = Iv.Rxy.wrap(0xFFFF_FFFF_0000_0000L)
inline def `..`: Iv.Rxy = Iv.Rxy.wrap(0xFFFF_FFFF_0000_0000L)



extension [A](a: Array[A]) {
  @targetName("apply_End_At") inline def apply(i: End.At): A = a(i of a)
  @targetName("apply_End_type") inline def apply(e: End.type): A = a(a.length - 1)
  @targetName("apply_Start_At") inline def apply(i: Start.At): A = a(i.unwrap)
  @targetName("apply_Start_type") inline def apply(i: Start.type): A = a(0)

  inline def use(i: Int)(inline f: A => Unit): a.type =
    if i >= 0 && i < a.length then f(a(i))
    a
  inline def zap(i: Int)(inline f: A => A): a.type =
    if i >= 0 && i < a.length then a(i) = f(a(i))
    a
}


extension (a: String) {
  @targetName("str_apply") inline def apply(i: Int): Char = a.charAt(i)
  @targetName("str_apply_End_At") inline def apply(i: End.At): Char = a.charAt(i of a)
  @targetName("str_apply_End_type") inline def apply(e: End.type): Char = a.charAt(a.length - 1)
  @targetName("str_apply_Start_At") inline def apply(i: Start.At): Char = a.charAt(i.unwrap)
  @targetName("str_apply_Start_type") inline def apply(i: Start.type): Char = a.charAt(0)

  inline def use(i: Int)(inline f: Char => Unit): a.type =
    if i >= 0 && i < a.length then f(a.charAt(i))
    a

  inline def arr: Array[Char] = a.toCharArray
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
      case t: Translucent.Chain[A, Unit]    => MuUnit.asInstanceOf[T[A]]
      case t: Translucent.Chain[A, Boolean] => MuBoolean(t.witness.reveal(a)).asInstanceOf[T[A]]
      case t: Translucent.Chain[A, Byte]    => MuByte(   t.witness.reveal(a)).asInstanceOf[T[A]]
      case t: Translucent.Chain[A, Short]   => MuShort(  t.witness.reveal(a)).asInstanceOf[T[A]]
      case t: Translucent.Chain[A, Char]    => MuChar(   t.witness.reveal(a)).asInstanceOf[T[A]]
      case t: Translucent.Chain[A, Int]     => MuInt(    t.witness.reveal(a)).asInstanceOf[T[A]]
      case t: Translucent.Chain[A, Long]    => MuLong(   t.witness.reveal(a)).asInstanceOf[T[A]]
      case t: Translucent.Chain[A, Float]   => MuFloat(  t.witness.reveal(a)).asInstanceOf[T[A]]
      case t: Translucent.Chain[A, Double]  => MuDouble( t.witness.reveal(a)).asInstanceOf[T[A]]
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
    case t: Translucent.Chain[A, Unit]    => MuUnit.asInstanceOf[T[A]]
    case t: Translucent.Chain[A, Boolean] => MuBoolean(t.witness.reveal(a)).asInstanceOf[T[A]]
    case t: Translucent.Chain[A, Byte]    => MuByte(   t.witness.reveal(a)).asInstanceOf[T[A]]
    case t: Translucent.Chain[A, Short]   => MuShort(  t.witness.reveal(a)).asInstanceOf[T[A]]
    case t: Translucent.Chain[A, Char]    => MuChar(   t.witness.reveal(a)).asInstanceOf[T[A]]
    case t: Translucent.Chain[A, Int]     => MuInt(    t.witness.reveal(a)).asInstanceOf[T[A]]
    case t: Translucent.Chain[A, Long]    => MuLong(   t.witness.reveal(a)).asInstanceOf[T[A]]
    case t: Translucent.Chain[A, Float]   => MuFloat(  t.witness.reveal(a)).asInstanceOf[T[A]]
    case t: Translucent.Chain[A, Double]  => MuDouble( t.witness.reveal(a)).asInstanceOf[T[A]]
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
        case t: Translucent.Chain[A, Unit]    => t.witness.conceal(())
        case t: Translucent.Chain[A, Boolean] => t.witness.conceal(mut.asInstanceOf[Mu.MuBoolean].myValue)
        case t: Translucent.Chain[A, Byte]    => t.witness.conceal(mut.asInstanceOf[Mu.MuByte   ].myValue)
        case t: Translucent.Chain[A, Short]   => t.witness.conceal(mut.asInstanceOf[Mu.MuShort  ].myValue)
        case t: Translucent.Chain[A, Char]    => t.witness.conceal(mut.asInstanceOf[Mu.MuChar   ].myValue)
        case t: Translucent.Chain[A, Int]     => t.witness.conceal(mut.asInstanceOf[Mu.MuInt    ].myValue)
        case t: Translucent.Chain[A, Long]    => t.witness.conceal(mut.asInstanceOf[Mu.MuLong   ].myValue)
        case t: Translucent.Chain[A, Float]   => t.witness.conceal(mut.asInstanceOf[Mu.MuFloat  ].myValue)
        case t: Translucent.Chain[A, Double]  => t.witness.conceal(mut.asInstanceOf[Mu.MuDouble ].myValue)
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
        case t: Translucent.Chain[A, Unit] => t.witness.conceal(())
        case t: Translucent.Chain[A, Boolean] => mu match
          case z: Mu.MuBoolean => t.witness.conceal(z.myValue)
          case _               => mu.getValue
        case t: Translucent.Chain[A, Byte] => mu match
          case b: Mu.MuByte => t.witness.conceal(b.myValue)
          case _            => mu.getValue
        case t: Translucent.Chain[A, Short] => mu match
          case s: Mu.MuShort => t.witness.conceal(s.myValue)
          case _             => mu.getValue
        case t: Translucent.Chain[A, Char] => mu match
          case c: Mu.MuChar => t.witness.conceal(c.myValue)
          case _            => mu.getValue
        case t: Translucent.Chain[A, Int] => mu match
          case i: Mu.MuInt => t.witness.conceal(i.myValue)
          case _           => mu.getValue
        case t: Translucent.Chain[A, Long] => mu match
          case l: Mu.MuLong => t.witness.conceal(l.myValue)
          case _            => mu.getValue
        case t: Translucent.Chain[A, Float] => mu match
          case f: Mu.MuFloat => t.witness.conceal(f.myValue)
          case _             => mu.getValue
        case t: Translucent.Chain[A, Double] => mu match
          case d: Mu.MuDouble => t.witness.conceal(d.myValue)
          case _              => mu.getValue
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
        case t: Translucent.Chain[A, Unit]    => ()
        case t: Translucent.Chain[A, Boolean] => mut.asInstanceOf[Mu.MuBoolean].myValue = t.witness.reveal(a)
        case t: Translucent.Chain[A, Byte]    => mut.asInstanceOf[Mu.MuByte   ].myValue = t.witness.reveal(a)
        case t: Translucent.Chain[A, Short]   => mut.asInstanceOf[Mu.MuShort  ].myValue = t.witness.reveal(a)
        case t: Translucent.Chain[A, Char]    => mut.asInstanceOf[Mu.MuChar   ].myValue = t.witness.reveal(a)
        case t: Translucent.Chain[A, Int]     => mut.asInstanceOf[Mu.MuInt    ].myValue = t.witness.reveal(a)
        case t: Translucent.Chain[A, Long]    => mut.asInstanceOf[Mu.MuLong   ].myValue = t.witness.reveal(a)
        case t: Translucent.Chain[A, Float]   => mut.asInstanceOf[Mu.MuFloat  ].myValue = t.witness.reveal(a)
        case t: Translucent.Chain[A, Double]  => mut.asInstanceOf[Mu.MuDouble ].myValue = t.witness.reveal(a)
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
        case t: Translucent.Chain[A, Unit] => ()
        case t: Translucent.Chain[A, Boolean] => mu match
          case z: Mu.MuBoolean => z.myValue = t.witness.reveal(a)
          case _               => mu.setValue(a)
        case t: Translucent.Chain[A, Byte] => mu match
          case b: Mu.MuByte => b.myValue = t.witness.reveal(a)
          case _            => mu.setValue(a)
        case t: Translucent.Chain[A, Short] => mu match
          case s: Mu.MuShort => s.myValue = t.witness.reveal(a)
          case _             => mu.setValue(a)
        case t: Translucent.Chain[A, Char] => mu match
          case c: Mu.MuChar => c.myValue = t.witness.reveal(a)
          case _            => mu.setValue(a)
        case t: Translucent.Chain[A, Int] => mu match
          case i: Mu.MuInt => i.myValue = t.witness.reveal(a)
          case _           => mu.setValue(a)
        case t: Translucent.Chain[A, Long] => mu match
          case l: Mu.MuLong => l.myValue = t.witness.reveal(a)
          case _            => mu.setValue(a)
        case t: Translucent.Chain[A, Float] => mu match
          case f: Mu.MuFloat => f.myValue = t.witness.reveal(a)
          case _             => mu.setValue(a)
        case t: Translucent.Chain[A, Double] => mu match
          case d: Mu.MuDouble => d.myValue = t.witness.reveal(a)
          case _              => mu.setValue(a)
        case _ => mu.setValue(a)
      }

  inline def use(inline f: A => Unit): mu.type =
    f(mu())
    mu

  inline def op(inline f: A => A): Unit = 
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
          case t: Translucent.Chain[A, Unit]    => ()
          case t: Translucent.Chain[A, Boolean] => mut.asInstanceOf[Mu.MuBoolean].myValue = t.witness.reveal(f(t.witness.conceal(mut.asInstanceOf[Mu.MuBoolean].myValue)))
          case t: Translucent.Chain[A, Byte]    => mut.asInstanceOf[Mu.MuByte   ].myValue = t.witness.reveal(f(t.witness.conceal(mut.asInstanceOf[Mu.MuByte   ].myValue)))
          case t: Translucent.Chain[A, Short]   => mut.asInstanceOf[Mu.MuShort  ].myValue = t.witness.reveal(f(t.witness.conceal(mut.asInstanceOf[Mu.MuShort  ].myValue)))
          case t: Translucent.Chain[A, Char]    => mut.asInstanceOf[Mu.MuChar   ].myValue = t.witness.reveal(f(t.witness.conceal(mut.asInstanceOf[Mu.MuChar   ].myValue)))
          case t: Translucent.Chain[A, Int]     => mut.asInstanceOf[Mu.MuInt    ].myValue = t.witness.reveal(f(t.witness.conceal(mut.asInstanceOf[Mu.MuInt    ].myValue)))
          case t: Translucent.Chain[A, Long]    => mut.asInstanceOf[Mu.MuLong   ].myValue = t.witness.reveal(f(t.witness.conceal(mut.asInstanceOf[Mu.MuLong   ].myValue)))
          case t: Translucent.Chain[A, Float]   => mut.asInstanceOf[Mu.MuFloat  ].myValue = t.witness.reveal(f(t.witness.conceal(mut.asInstanceOf[Mu.MuFloat  ].myValue)))
          case t: Translucent.Chain[A, Double]  => mut.asInstanceOf[Mu.MuDouble ].myValue = t.witness.reveal(f(t.witness.conceal(mut.asInstanceOf[Mu.MuDouble ].myValue)))
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
          case t: Translucent.Chain[A, Unit]    => ()
          case t: Translucent.Chain[A, Boolean] => mu match
            case z: Mu.MuBoolean => z.myValue = t.witness.reveal(f(t.witness.conceal(z.myValue)))
            case _               => mu.setValue(f(mu.getValue))
          case t: Translucent.Chain[A, Byte]    => mu match
            case b: Mu.MuByte => b.myValue = t.witness.reveal(f(t.witness.conceal(b.myValue)))
            case _            => mu.setValue(f(mu.getValue))
          case t: Translucent.Chain[A, Short]   => mu match
            case s: Mu.MuShort => s.myValue = t.witness.reveal(f(t.witness.conceal(s.myValue)))
            case _             => mu.setValue(f(mu.getValue))
          case t: Translucent.Chain[A, Char]    => mu match
            case c: Mu.MuChar => c.myValue = t.witness.reveal(f(t.witness.conceal(c.myValue)))
            case _            => mu.setValue(f(mu.getValue))
          case t: Translucent.Chain[A, Int]     => mu match
            case i: Mu.MuInt => i.myValue = t.witness.reveal(f(t.witness.conceal(i.myValue)))
            case _           => mu.setValue(f(mu.getValue))
          case t: Translucent.Chain[A, Long]    => mu match
            case l: Mu.MuLong => l.myValue = t.witness.reveal(f(t.witness.conceal(l.myValue)))
            case _            => mu.setValue(f(mu.getValue))
          case t: Translucent.Chain[A, Float]   => mu match
            case x: Mu.MuFloat => x.myValue = t.witness.reveal(f(t.witness.conceal(x.myValue)))
            case _             => mu.setValue(f(mu.getValue))
          case t: Translucent.Chain[A, Double]  => mu match
            case d: Mu.MuDouble => d.myValue = t.witness.reveal(f(t.witness.conceal(d.myValue)))
            case _              => mu.setValue(f(mu.getValue))
          case _ => mu.setValue(f(mu.getValue))
        }

  inline def zap(inline f: A => A): mu.type =
    op(f)
    mu

  inline def opAndGet(inline f: A => A): A =
    inline mu match
      case mut: Mu.T[A] => inline mut match
        case u: Mu.MuUnit.type => ()
        case u: Mu.T[Unit]     => ()
        case z: Mu.MuBoolean   => { val v = f(z.myValue); z.myValue = v; v }
        case z: Mu.T[Boolean]  => { val v = f(z.asInstanceOf[Mu.MuBoolean].myValue); z.asInstanceOf[Mu.MuBoolean].myValue = v; v }
        case b: Mu.MuByte      => { val v = f(b.myValue); b.myValue = v; v }
        case b: Mu.T[Byte]     => { val v = f(b.asInstanceOf[Mu.MuByte].myValue); b.asInstanceOf[Mu.MuByte].myValue = v; v }
        case s: Mu.MuShort     => { val v = f(s.myValue); s.myValue = v; v }
        case s: Mu.T[Short]    => { val v = f(s.asInstanceOf[Mu.MuShort].myValue); s.asInstanceOf[Mu.MuShort].myValue = v; v }
        case c: Mu.MuChar      => { val v = f(c.myValue); c.myValue = v; v }
        case c: Mu.T[Char]     => { val v = f(c.asInstanceOf[Mu.MuChar].myValue); c.asInstanceOf[Mu.MuChar].myValue = v; v }
        case i: Mu.MuInt       => { val v = f(i.myValue); i.myValue = v; v }
        case i: Mu.T[Int]      => { val v = f(i.asInstanceOf[Mu.MuInt].myValue); i.asInstanceOf[Mu.MuInt].myValue = v; v }
        case l: Mu.MuLong      => { val v = f(l.myValue); l.myValue = v; v }
        case l: Mu.T[Long]     => { val v = f(l.asInstanceOf[Mu.MuLong].myValue); l.asInstanceOf[Mu.MuLong].myValue = v; v }
        case x: Mu.MuFloat     => { val v = f(x.myValue); x.myValue = v; v }
        case x: Mu.T[Float]    => { val v = f(x.asInstanceOf[Mu.MuFloat].myValue); x.asInstanceOf[Mu.MuFloat].myValue = v; v }
        case d: Mu.MuDouble    => { val v = f(d.myValue); d.myValue = v; v }
        case d: Mu.T[Double]   => { val v = f(d.asInstanceOf[Mu.MuDouble].myValue); d.asInstanceOf[Mu.MuDouble].myValue = v; v }
        case _                 => summonFrom {
          case t: Translucent.Chain[A, Unit] => t.witness.conceal(())
          case t: Translucent.Chain[A, Boolean] => { val v = f(t.witness.conceal(mut.asInstanceOf[Mu.MuBoolean].myValue)); mut.asInstanceOf[Mu.MuBoolean].myValue = t.witness.reveal(v); v }
          case t: Translucent.Chain[A, Byte]    => { val v = f(t.witness.conceal(mut.asInstanceOf[Mu.MuByte   ].myValue)); mut.asInstanceOf[Mu.MuByte   ].myValue = t.witness.reveal(v); v }
          case t: Translucent.Chain[A, Short]   => { val v = f(t.witness.conceal(mut.asInstanceOf[Mu.MuShort  ].myValue)); mut.asInstanceOf[Mu.MuShort  ].myValue = t.witness.reveal(v); v }
          case t: Translucent.Chain[A, Char]    => { val v = f(t.witness.conceal(mut.asInstanceOf[Mu.MuChar   ].myValue)); mut.asInstanceOf[Mu.MuChar   ].myValue = t.witness.reveal(v); v }
          case t: Translucent.Chain[A, Int]     => { val v = f(t.witness.conceal(mut.asInstanceOf[Mu.MuInt    ].myValue)); mut.asInstanceOf[Mu.MuInt    ].myValue = t.witness.reveal(v); v }
          case t: Translucent.Chain[A, Long]    => { val v = f(t.witness.conceal(mut.asInstanceOf[Mu.MuLong   ].myValue)); mut.asInstanceOf[Mu.MuLong   ].myValue = t.witness.reveal(v); v }
          case t: Translucent.Chain[A, Float]   => { val v = f(t.witness.conceal(mut.asInstanceOf[Mu.MuFloat  ].myValue)); mut.asInstanceOf[Mu.MuFloat  ].myValue = t.witness.reveal(v); v }
          case t: Translucent.Chain[A, Double]  => { val v = f(t.witness.conceal(mut.asInstanceOf[Mu.MuDouble ].myValue)); mut.asInstanceOf[Mu.MuDouble ].myValue = t.witness.reveal(v); v }
        }
      case _ => inline erasedValue[A] match
        case _: Unit    => ().asInstanceOf[A]
        case _: Boolean => mu match
          case z: Mu.MuBoolean => { val v = f(z.myValue);   z.myValue = v;  v }
          case _               => { val v = f(mu.getValue); mu.setValue(v); v }
        case _: Byte    => mu match
          case b: Mu.MuByte    => { val v = f(b.myValue);   b.myValue = v;  v }
          case _               => { val v = f(mu.getValue); mu.setValue(v); v }
        case _: Short   => mu match
          case s: Mu.MuShort   => { val v = f(s.myValue);   s.myValue = v;  v }
          case _               => { val v = f(mu.getValue); mu.setValue(v); v }
        case _: Char    => mu match
          case c: Mu.MuChar    => { val v = f(c.myValue);   c.myValue = v;  v }
          case _               => { val v = f(mu.getValue); mu.setValue(v); v }
        case _: Int     => mu match
          case i: Mu.MuInt     => { val v = f(i.myValue);   i.myValue = v;  v }
          case _               => { val v = f(mu.getValue); mu.setValue(v); v }
        case _: Long    => mu match
          case l: Mu.MuLong    => { val v = f(l.myValue);   l.myValue = v;  v }
          case _               => { val v = f(mu.getValue); mu.setValue(v); v }
        case _: Float   => mu match
          case x: Mu.MuFloat   => { val v = f(x.myValue);   x.myValue = v;  v }
          case _               => { val v = f(mu.getValue); mu.setValue(v); v }
        case _: Double  => mu match
          case d: Mu.MuDouble  => { val v = f(d.myValue);   d.myValue = v;  v }
          case _               => { val v = f(mu.getValue); mu.setValue(v); v }
        case _ => summonFrom {
          case t: Translucent.Chain[A, Unit] => t.witness.conceal(())
          case t: Translucent.Chain[A, Boolean] => mu match
            case z: Mu.MuBoolean => { val v = f(t.witness.conceal(z.myValue)); z.myValue = t.witness.reveal(v); v }
            case _               => { val v = f(mu.getValue); mu.setValue(v); v }
          case t: Translucent.Chain[A, Byte] => mu match
            case b: Mu.MuByte    => { val v = f(t.witness.conceal(b.myValue)); b.myValue = t.witness.reveal(v); v }
            case _               => { val v = f(mu.getValue); mu.setValue(v); v }
          case t: Translucent.Chain[A, Short] => mu match
            case s: Mu.MuShort   => { val v = f(t.witness.conceal(s.myValue)); s.myValue = t.witness.reveal(v); v }
            case _               => { val v = f(mu.getValue); mu.setValue(v); v }
          case t: Translucent.Chain[A, Char] => mu match
            case c: Mu.MuChar    => { val v = f(t.witness.conceal(c.myValue)); c.myValue = t.witness.reveal(v); v }
            case _               => { val v = f(mu.getValue); mu.setValue(v); v }
          case t: Translucent.Chain[A, Int] => mu match
            case i: Mu.MuInt     => { val v = f(t.witness.conceal(i.myValue)); i.myValue = t.witness.reveal(v); v }
            case _               => { val v = f(mu.getValue); mu.setValue(v); v }
          case t: Translucent.Chain[A, Long] => mu match
            case l: Mu.MuLong    => { val v = f(t.witness.conceal(l.myValue)); l.myValue = t.witness.reveal(v); v }
            case _               => { val v = f(mu.getValue); mu.setValue(v); v }
          case t: Translucent.Chain[A, Float]   => mu match
            case x: Mu.MuFloat   => { val v = f(t.witness.conceal(x.myValue)); x.myValue = t.witness.reveal(v); v }
            case _               => { val v = f(mu.getValue); mu.setValue(v); v }
          case t: Translucent.Chain[A, Double]  => mu match
            case d: Mu.MuDouble  => { val v = f(t.witness.conceal(d.myValue)); d.myValue = t.witness.reveal(v); v }
            case _               => { val v = f(mu.getValue); mu.setValue(v); v }
          case _ => { val v = f(mu.getValue); mu.setValue(v); v }
        }

  inline def getAndOp(inline f: A => A): A =
    inline mu match
      case mut: Mu.T[A] => inline mut match
        case u: Mu.MuUnit.type => ()
        case u: Mu.T[Unit]     => ()
        case z: Mu.MuBoolean   => { val v = z.myValue; z.myValue = f(v); v }
        case z: Mu.T[Boolean]  => { val v = z.asInstanceOf[Mu.MuBoolean].myValue; z.asInstanceOf[Mu.MuBoolean].myValue = f(v); v }
        case b: Mu.MuByte      => { val v = b.myValue; b.myValue = f(v); v }
        case b: Mu.T[Byte]     => { val v = b.asInstanceOf[Mu.MuByte].myValue; b.asInstanceOf[Mu.MuByte].myValue = f(v); v }
        case s: Mu.MuShort     => { val v = s.myValue; s.myValue = f(v); v }
        case s: Mu.T[Short]    => { val v = s.asInstanceOf[Mu.MuShort].myValue; s.asInstanceOf[Mu.MuShort].myValue = f(v); v }
        case c: Mu.MuChar      => { val v = c.myValue; c.myValue = f(v); v }
        case c: Mu.T[Char]     => { val v = c.asInstanceOf[Mu.MuChar].myValue; c.asInstanceOf[Mu.MuChar].myValue = f(v); v }
        case i: Mu.MuInt       => { val v = i.myValue; i.myValue = f(v); v }
        case i: Mu.T[Int]      => { val v = i.asInstanceOf[Mu.MuInt].myValue; i.asInstanceOf[Mu.MuInt].myValue = f(v); v }
        case l: Mu.MuLong      => { val v = l.myValue; l.myValue = f(v); v }
        case l: Mu.T[Long]     => { val v = l.asInstanceOf[Mu.MuLong].myValue; l.asInstanceOf[Mu.MuLong].myValue = f(v); v }
        case x: Mu.MuFloat     => { val v = x.myValue; x.myValue = f(v); v }
        case x: Mu.T[Float]    => { val v = x.asInstanceOf[Mu.MuFloat].myValue; x.asInstanceOf[Mu.MuFloat].myValue = f(v); v }
        case d: Mu.MuDouble    => { val v = d.myValue; d.myValue = f(v); v }
        case d: Mu.T[Double]   => { val v = d.asInstanceOf[Mu.MuDouble].myValue; d.asInstanceOf[Mu.MuDouble].myValue = f(v); v }
        case _                 => summonFrom {
          case t: Translucent.Chain[A, Unit] => t.witness.conceal(())
          case t: Translucent.Chain[A, Boolean] => { val v = t.witness.conceal(mut.asInstanceOf[Mu.MuBoolean].myValue); mut.asInstanceOf[Mu.MuBoolean].myValue = t.witness.reveal(f(v)); v }
          case t: Translucent.Chain[A, Byte]    => { val v = t.witness.conceal(mut.asInstanceOf[Mu.MuByte   ].myValue); mut.asInstanceOf[Mu.MuByte   ].myValue = t.witness.reveal(f(v)); v }
          case t: Translucent.Chain[A, Short]   => { val v = t.witness.conceal(mut.asInstanceOf[Mu.MuShort  ].myValue); mut.asInstanceOf[Mu.MuShort  ].myValue = t.witness.reveal(f(v)); v }
          case t: Translucent.Chain[A, Char]    => { val v = t.witness.conceal(mut.asInstanceOf[Mu.MuChar   ].myValue); mut.asInstanceOf[Mu.MuChar   ].myValue = t.witness.reveal(f(v)); v }
          case t: Translucent.Chain[A, Int]     => { val v = t.witness.conceal(mut.asInstanceOf[Mu.MuInt    ].myValue); mut.asInstanceOf[Mu.MuInt    ].myValue = t.witness.reveal(f(v)); v }
          case t: Translucent.Chain[A, Long]    => { val v = t.witness.conceal(mut.asInstanceOf[Mu.MuLong   ].myValue); mut.asInstanceOf[Mu.MuLong   ].myValue = t.witness.reveal(f(v)); v }
          case t: Translucent.Chain[A, Float]   => { val v = t.witness.conceal(mut.asInstanceOf[Mu.MuFloat  ].myValue); mut.asInstanceOf[Mu.MuFloat  ].myValue = t.witness.reveal(f(v)); v }
          case t: Translucent.Chain[A, Double]  => { val v = t.witness.conceal(mut.asInstanceOf[Mu.MuDouble ].myValue); mut.asInstanceOf[Mu.MuDouble ].myValue = t.witness.reveal(f(v)); v }
        }
      case _ => inline erasedValue[A] match
        case _: Unit    => ().asInstanceOf[A]
        case _: Boolean => mu match
          case z: Mu.MuBoolean => { val v = z.myValue;   z.myValue = f(v);  v }
          case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
        case _: Byte    => mu match
          case b: Mu.MuByte    => { val v = b.myValue;   b.myValue = f(v);  v }
          case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
        case _: Short   => mu match
          case s: Mu.MuShort   => { val v = s.myValue;   s.myValue = f(v);  v }
          case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
        case _: Char    => mu match
          case c: Mu.MuChar    => { val v = c.myValue;   c.myValue = f(v);  v }
          case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
        case _: Int     => mu match
          case i: Mu.MuInt     => { val v = i.myValue;   i.myValue = f(v);  v }
          case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
        case _: Long    => mu match
          case l: Mu.MuLong    => { val v = l.myValue;   l.myValue = f(v);  v }
          case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
        case _: Float   => mu match
          case x: Mu.MuFloat   => { val v = x.myValue;   x.myValue = f(v);  v }
          case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
        case _: Double  => mu match
          case d: Mu.MuDouble  => { val v = d.myValue;   d.myValue = f(v);  v }
          case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
        case _ => summonFrom {
          case t: Translucent.Chain[A, Unit] => t.witness.conceal(())
          case t: Translucent.Chain[A, Boolean] => mu match
            case z: Mu.MuBoolean => { val v = t.witness.conceal(z.myValue); z.myValue = t.witness.reveal(f(v)); v }
            case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
          case t: Translucent.Chain[A, Byte] => mu match
            case b: Mu.MuByte    => { val v = t.witness.conceal(b.myValue); b.myValue = t.witness.reveal(f(v)); v }
            case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
          case t: Translucent.Chain[A, Short] => mu match
            case s: Mu.MuShort   => { val v = t.witness.conceal(s.myValue); s.myValue = t.witness.reveal(f(v)); v }
            case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
          case t: Translucent.Chain[A, Char] => mu match
            case c: Mu.MuChar    => { val v = t.witness.conceal(c.myValue); c.myValue = t.witness.reveal(f(v)); v }
            case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
          case t: Translucent.Chain[A, Int] => mu match
            case i: Mu.MuInt     => { val v = t.witness.conceal(i.myValue); i.myValue = t.witness.reveal(f(v)); v }
            case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
          case t: Translucent.Chain[A, Long] => mu match
            case l: Mu.MuLong    => { val v = t.witness.conceal(l.myValue); l.myValue = t.witness.reveal(f(v)); v }
            case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
          case t: Translucent.Chain[A, Float]   => mu match
            case x: Mu.MuFloat   => { val v = t.witness.conceal(x.myValue); x.myValue = t.witness.reveal(f(v)); v }
            case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
          case t: Translucent.Chain[A, Double]  => mu match
            case d: Mu.MuDouble  => { val v = t.witness.conceal(d.myValue); d.myValue = t.witness.reveal(f(v)); v }
            case _               => { val v = mu.getValue; mu.setValue(f(v)); v }
          case _ => { val v = mu.getValue; mu.setValue(f(v)); v }
        }
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
  * Use op, zap, getAndOp, or opAndGet depending on whether you just need to compute the new value, or whether you need the Atom, the value
  * you computed on, or the value you computed.
  */
opaque type Atom[A] <: AnyRef = AtomicInteger | AtomicLong | AtomicBoolean | AtomicReference[AnyRef]
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
      case _: Translucent.Chain[A, Int]     => new AtomicInteger(a.asInstanceOf[Int])
      case _: Translucent.Chain[A, Long]    => new AtomicLong(a.asInstanceOf[Long])
      case _: Translucent.Chain[A, Byte]    => new AtomicInteger(a.asInstanceOf[Byte].toInt) 
      case _: Translucent.Chain[A, Short]   => new AtomicInteger(a.asInstanceOf[Short].toInt)
      case _: Translucent.Chain[A, Char]    => new AtomicInteger(a.asInstanceOf[Char].toInt)
      case _: Translucent.Chain[A, Float]   => new AtomicInteger(f2i(a.asInstanceOf[Float]))
      case _: Translucent.Chain[A, Double]  => new AtomicLong(d2l(a.asInstanceOf[Double]))
      case _: Translucent.Chain[A, Boolean] => new AtomicBoolean(a.asInstanceOf[Boolean])
      case _ =>
        inline if Translucent.isEventually[A, AnyRef] then new AtomicReference(a.asInstanceOf[AnyRef])
        else compiletime.error("No Atomic support for values of this type")
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
      case _: AnyRef  => a.asInstanceOf[AtomicReference[AnyRef]].get().asInstanceOf[A]
      case _ => summonFrom{
        case _: Translucent.Chain[A, Int]     => a.asInstanceOf[AtomicInteger].get().asInstanceOf[A] 
        case _: Translucent.Chain[A, Long]    => a.asInstanceOf[AtomicLong   ].get().asInstanceOf[A]
        case _: Translucent.Chain[A, Byte]    => a.asInstanceOf[AtomicInteger].get().toByte .asInstanceOf[A]
        case _: Translucent.Chain[A, Short]   => a.asInstanceOf[AtomicInteger].get().toShort.asInstanceOf[A]
        case _: Translucent.Chain[A, Char]    => a.asInstanceOf[AtomicInteger].get().toChar .asInstanceOf[A]
        case _: Translucent.Chain[A, Float]   => i2f(a.asInstanceOf[AtomicInteger].get()).asInstanceOf[A]
        case _: Translucent.Chain[A, Double]  => l2d(a.asInstanceOf[AtomicLong   ].get()).asInstanceOf[A]
        case _: Translucent.Chain[A, Boolean] => a.asInstanceOf[AtomicBoolean  ].get().asInstanceOf[A]
        case _ =>
          inline if Translucent.isEventually[A, AnyRef] then a.asInstanceOf[AtomicReference[AnyRef]].get().asInstanceOf[A]
          else compiletime.error("Context missing to support atomic operations on values of this type")
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
        case _: Translucent.Chain[A, Int]     => a.asInstanceOf[AtomicInteger].set(x.asInstanceOf[Int])
        case _: Translucent.Chain[A, Long]    => a.asInstanceOf[AtomicLong   ].set(x.asInstanceOf[Long])
        case _: Translucent.Chain[A, Byte]    => a.asInstanceOf[AtomicInteger].set(x.asInstanceOf[Byte].toInt)
        case _: Translucent.Chain[A, Short]   => a.asInstanceOf[AtomicInteger].set(x.asInstanceOf[Short].toInt)
        case _: Translucent.Chain[A, Char]    => a.asInstanceOf[AtomicInteger].set(x.asInstanceOf[Char].toInt)
        case _: Translucent.Chain[A, Float]   => a.asInstanceOf[AtomicInteger].set(f2i(x.asInstanceOf[Float]))
        case _: Translucent.Chain[A, Double]  => a.asInstanceOf[AtomicLong   ].set(d2l(x.asInstanceOf[Double]))
        case _: Translucent.Chain[A, Boolean] => a.asInstanceOf[AtomicBoolean].set(x.asInstanceOf[Boolean])
        case _ =>
          inline if Translucent.isEventually[A, AnyRef] then a.asInstanceOf[AtomicReference[AnyRef]].set(a.asInstanceOf[AnyRef])
          else compiletime.error("Context missing to support atomic operations on values of this type")
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
      case _: AnyRef  => a.asInstanceOf[AtomicReference[AnyRef]].getAndSet(x.asInstanceOf[AnyRef]).asInstanceOf[A]
      case _ => summonFrom{
        case _: Translucent[A, Int]     => a.asInstanceOf[AtomicInteger].getAndSet(x.asInstanceOf[Int ]).asInstanceOf[A]
        case _: Translucent[A, Long]    => a.asInstanceOf[AtomicLong   ].getAndSet(x.asInstanceOf[Long]).asInstanceOf[A]
        case _: Translucent[A, Byte]    => a.asInstanceOf[AtomicInteger].getAndSet(x.asInstanceOf[Byte ].toInt).toByte .asInstanceOf[A]
        case _: Translucent[A, Short]   => a.asInstanceOf[AtomicInteger].getAndSet(x.asInstanceOf[Short].toInt).toShort.asInstanceOf[A]
        case _: Translucent[A, Char]    => a.asInstanceOf[AtomicInteger].getAndSet(x.asInstanceOf[Char ].toInt).toChar .asInstanceOf[A]
        case _: Translucent[A, Float]   => i2f(a.asInstanceOf[AtomicInteger].getAndSet(f2i(x.asInstanceOf[Float ]))).asInstanceOf[A]
        case _: Translucent[A, Double]  => l2d(a.asInstanceOf[AtomicLong   ].getAndSet(d2l(x.asInstanceOf[Double]))).asInstanceOf[A]
        case _: Translucent[A, Boolean] => a.asInstanceOf[AtomicBoolean].getAndSet(x.asInstanceOf[Boolean]).asInstanceOf[A]
        case _ =>
          inline if Translucent.isEventually[A, AnyRef] then a.asInstanceOf[AtomicReference[AnyRef]].getAndSet(x.asInstanceOf[AnyRef]).asInstanceOf[A]
          else compiletime.error("Context missing to support atomic operations on values of this type")
      }

    inline def getAndOp(inline f: A => A): A = inline erasedValue[A] match
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
        var x = a.asInstanceOf[AtomicReference[AnyRef]].get().asInstanceOf[A]
        while !a.asInstanceOf[AtomicReference[AnyRef]].compareAndSet(x.asInstanceOf[AnyRef], f(x).asInstanceOf[AnyRef]) do
          x = a.asInstanceOf[AtomicReference[AnyRef]].get().asInstanceOf[A]
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
        case _ =>
          inline if Translucent.isEventually[A, AnyRef] then
            var x = a.asInstanceOf[AtomicReference[AnyRef]].get().asInstanceOf[A]
            while !a.asInstanceOf[AtomicReference[AnyRef]].compareAndSet(x.asInstanceOf[AnyRef], f(x).asInstanceOf[AnyRef]) do
              x = a.asInstanceOf[AtomicReference[AnyRef]].get().asInstanceOf[A]
            x
          else compiletime.error("Context missing to support atomic operations on values of this type")
      }

    inline def opAndGet(inline f: A => A): A = inline erasedValue[A] match
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
        case _ =>
          inline if Translucent.isEventually[A, AnyRef] then
            var x = a.asInstanceOf[AtomicReference[AnyRef]].get().asInstanceOf[A]
            while { val y = f(x); val done = a.asInstanceOf[AtomicReference[AnyRef]].compareAndSet(x.asInstanceOf[AnyRef], y.asInstanceOf[AnyRef]); if done then x = y; !done } do
              x = a.asInstanceOf[AtomicReference[AnyRef]].get().asInstanceOf[A]
            x
          else compiletime.error("Context missing to support atomic operations on values of this type")
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
        case _ =>
          inline if Translucent.isEventually[A, AnyRef] then
            var x = a.asInstanceOf[AtomicReference[AnyRef]].get().asInstanceOf[A]
            while !a.asInstanceOf[AtomicReference[AnyRef]].compareAndSet(x.asInstanceOf[AnyRef], f(x).asInstanceOf[AnyRef]) do
              x = a.asInstanceOf[AtomicReference[AnyRef]].get().asInstanceOf[A]
          else compiletime.error("Context missing to support atomic operations on values of this type")
      }

    inline def zap(inline f: A => A): a.type =
      a.op(f)
      a
  }

  extension [A <: Int | Long](a: Atom[A]) {
    inline def ++ : Unit = inline erasedValue[A] match
      case _: Int  => a.asInstanceOf[AtomicInteger].incrementAndGet(): Unit
      case _: Long => a.asInstanceOf[AtomicLong].incrementAndGet(): Unit
    
    inline def -- : Unit = inline erasedValue[A] match
      case _: Int  => a.asInstanceOf[AtomicInteger].decrementAndGet(): Unit
      case _: Long => a.asInstanceOf[AtomicLong].decrementAndGet(): Unit
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
  /** Apply a function to this value and return the result.  Same as `pipe`, except it can be tupled. */
  inline infix def fn[B](inline op: A => B): B = op(a)

  /** Apply a function to this value and return the result.  Same as `fn`, except tuple inputs are always treated whole. */
  inline infix def pipe[B](inline f: A => B): B = f(a)

  /** Apply a side-effecting function to this value; return the original value */
  inline def tap(inline f: A => Unit): A = { f(a); a }

  /** Apply a side-effecting function to this value; discard the value. */
  inline def effect(inline f: A => Unit): Unit = f(a)

  /** Discard the value in an observable way. Use as __ Unit */
  inline infix def __(nothing: Unit.type): Unit = inline compiletime.erasedValue[A] match
    case _: Unit => compiletime.error("Cannot discard a value that is already Unit")
    case _       => ()

  /** Apply a test and alter the value if it passes */
  inline def fixIf(inline p: A => Boolean)(inline f: A => A): A = if p(a) then f(a) else a

  inline def |->[Z](inline op: A => Z): Z = op(a)
  inline def |->[B, Z](inline opb: ((A, B) => Z, B)) = basicsMacroImpl.applyWithoutBoxing2(a, opb)
  inline def |->[B, C, Z](inline op3: ((A, B, C) => Z, B, C)) = basicsMacroImpl.applyWithoutBoxing3(a, op3)
  inline def |->[B, C, D, Z](inline op4: ((A, B, C, D) => Z, B, C, D)) = basicsMacroImpl.applyWithoutBoxing4(a, op4)
  inline def |->[B, C, D, E, Z](inline op5: ((A, B, C, D, E) => Z, B, C, D, E)) = basicsMacroImpl.applyWithoutBoxing5(a, op5)
  inline def |->[B, C, D, E, F, Z](inline op6: ((A, B, C, D, E, F) => Z, B, C, D, E, F)) = basicsMacroImpl.applyWithoutBoxing6(a, op6)
  inline def |->[B, C, D, E, F, G, Z](inline op7: ((A, B, C, D, E, F, G) => Z, B, C, D, E, F, G)) = basicsMacroImpl.applyWithoutBoxing7(a, op7)
  inline def |->[B, C, D, E, F, G, H, Z](inline op8: ((A, B, C, D, E, F, G, H) => Z, B, C, D, E, F, G, H)) = basicsMacroImpl.applyWithoutBoxing8(a, op8)
  inline def |->[B, C, D, E, F, G, H, I, Z](inline op9: ((A, B, C, D, E, F, G, H, I) => Z, B, C, D, E, F, G, H, I)) = basicsMacroImpl.applyWithoutBoxing9(a, op9)
  inline def |->[B, C, D, E, F, G, H, I, J, Z](inline op10: ((A, B, C, D, E, F, G, H, I, J) => Z, B, C, D, E, F, G, H, I, J)) = basicsMacroImpl.applyWithoutBoxing10(a, op10)
  inline def |->[B, C, D, E, F, G, H, I, J, K, Z](inline op11: ((A, B, C, D, E, F, G, H, I, J, K) => Z, B, C, D, E, F, G, H, I, J, K)) = basicsMacroImpl.applyWithoutBoxing11(a, op11)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, Z](inline op12: ((A, B, C, D, E, F, G, H, I, J, K, L) => Z, B, C, D, E, F, G, H, I, J, K, L)) = basicsMacroImpl.applyWithoutBoxing12(a, op12)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, M, Z](inline op13: ((A, B, C, D, E, F, G, H, I, J, K, L, M) => Z, B, C, D, E, F, G, H, I, J, K, L, M)) = basicsMacroImpl.applyWithoutBoxing13(a, op13)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, M, N, Z](inline op14: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N)) = basicsMacroImpl.applyWithoutBoxing14(a, op14)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z](inline op15: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) = basicsMacroImpl.applyWithoutBoxing15(a, op15)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z](inline op16: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) = basicsMacroImpl.applyWithoutBoxing16(a, op16)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z](inline op17: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) = basicsMacroImpl.applyWithoutBoxing17(a, op17)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z](inline op18: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) = basicsMacroImpl.applyWithoutBoxing18(a, op18)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z](inline op19: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) = basicsMacroImpl.applyWithoutBoxing19(a, op19)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z](inline op20: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) = basicsMacroImpl.applyWithoutBoxing20(a, op20)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z](inline op21: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) = basicsMacroImpl.applyWithoutBoxing21(a, op21)
  inline def |->[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z](inline op22: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) = basicsMacroImpl.applyWithoutBoxing22(a, op22)
}


extension [A, B](inline tup2: (A, B))
  inline def fn[Z](inline op2: (A, B) => Z): Z = basicsMacroImpl.fnWithoutBoxing2(tup2, op2)

extension [A, B, C](inline tup3: (A, B, C))
  inline def fn[Z](inline op3: (A, B, C) => Z): Z = basicsMacroImpl.fnWithoutBoxing3(tup3, op3)

extension [A, B, C, D](inline tup4: (A, B, C, D))
  inline def fn[Z](inline op4: (A, B, C, D) => Z): Z = basicsMacroImpl.fnWithoutBoxing4(tup4, op4)

extension [A, B, C, D, E](inline tup5: (A, B, C, D, E))
  inline def fn[Z](inline op5: (A, B, C, D, E) => Z): Z = basicsMacroImpl.fnWithoutBoxing5(tup5, op5)

extension [A, B, C, D, E, F](inline tup6: (A, B, C, D, E, F))
  inline def fn[Z](inline op6: (A, B, C, D, E, F) => Z): Z = basicsMacroImpl.fnWithoutBoxing6(tup6, op6)

extension [A, B, C, D, E, F, G](inline tup7: (A, B, C, D, E, F, G))
  inline def fn[Z](inline op7: (A, B, C, D, E, F, G) => Z): Z = basicsMacroImpl.fnWithoutBoxing7(tup7, op7)

extension [A, B, C, D, E, F, G, H](inline tup8: (A, B, C, D, E, F, G, H))
  inline def fn[Z](inline op8: (A, B, C, D, E, F, G, H) => Z): Z = basicsMacroImpl.fnWithoutBoxing8(tup8, op8)

extension [A, B, C, D, E, F, G, H, I](inline tup9: (A, B, C, D, E, F, G, H, I))
  inline def fn[Z](inline op9: (A, B, C, D, E, F, G, H, I) => Z): Z = basicsMacroImpl.fnWithoutBoxing9(tup9, op9)

extension [A, B, C, D, E, F, G, H, I, J](inline tup10: (A, B, C, D, E, F, G, H, I, J))
  inline def fn[Z](inline op10: (A, B, C, D, E, F, G, H, I, J) => Z): Z = basicsMacroImpl.fnWithoutBoxing10(tup10, op10)

extension [A, B, C, D, E, F, G, H, I, J, K](inline tup11: (A, B, C, D, E, F, G, H, I, J, K))
  inline def fn[Z](inline op11: (A, B, C, D, E, F, G, H, I, J, K) => Z): Z = basicsMacroImpl.fnWithoutBoxing11(tup11, op11)

extension [A, B, C, D, E, F, G, H, I, J, K, L](inline tup12: (A, B, C, D, E, F, G, H, I, J, K, L))
  inline def fn[Z](inline op12: (A, B, C, D, E, F, G, H, I, J, K, L) => Z): Z = basicsMacroImpl.fnWithoutBoxing12(tup12, op12)

extension [A, B, C, D, E, F, G, H, I, J, K, L, M](inline tup13: (A, B, C, D, E, F, G, H, I, J, K, L, M))
  inline def fn[Z](inline op13: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Z): Z = basicsMacroImpl.fnWithoutBoxing13(tup13, op13)

extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N](inline tup14: (A, B, C, D, E, F, G, H, I, J, K, L, M, N))
  inline def fn[Z](inline op14: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z): Z = basicsMacroImpl.fnWithoutBoxing14(tup14, op14)

extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](inline tup15: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))
  inline def fn[Z](inline op15: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z): Z = basicsMacroImpl.fnWithoutBoxing15(tup15, op15)

extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](inline tup16: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))
  inline def fn[Z](inline op16: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z): Z = basicsMacroImpl.fnWithoutBoxing16(tup16, op16)

extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](inline tup17: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))
  inline def fn[Z](inline op17: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z): Z = basicsMacroImpl.fnWithoutBoxing17(tup17, op17)

extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](inline tup18: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))
  inline def fn[Z](inline op18: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z): Z = basicsMacroImpl.fnWithoutBoxing18(tup18, op18)

extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](inline tup19: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))
  inline def fn[Z](inline op19: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z): Z = basicsMacroImpl.fnWithoutBoxing19(tup19, op19)

extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](inline tup20: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))
  inline def fn[Z](inline op20: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z): Z = basicsMacroImpl.fnWithoutBoxing20(tup20, op20)

extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](inline tup21: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))
  inline def fn[Z](inline op21: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z): Z = basicsMacroImpl.fnWithoutBoxing21(tup21, op21)

extension [A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](inline tup22: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))
  inline def fn[Z](inline op22: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z): Z = basicsMacroImpl.fnWithoutBoxing22(tup22, op22)
/*

################
## GENERATORS ##
################

def mkArrowPipe(n: Int) =
  assert(n > 1 && n < 26)
  val args = "ABCDEFGHIJKLMNOPQRSTUVWXY".take(n).map(_.toString)
  val arga = args.mkString(", ")
  val argb = args.drop(1).mkString(", ")
  println(s"inline def |->[$argb, Z](inline op$n: (($arga) => Z, $argb)) = basicsMacroImpl.applyWithoutBoxing$n(a, op$n)")

for n <- 3 to 22 do
  mkArrowPipe(n)

def mkFnTup(n: Int) =
  assert(n > 1 && n < 26)
  val args = "ABCDEFGHIJKLMNOPQRSTUVWXY".take(n).map(_.toString)
  val arga = args.mkString(", ")
  println(s"extension [$arga](inline tup$n: ($arga))")
  println(s"  inline def fn[Z](inline op$n: ($arga) => Z): Z = basicsMacroImpl.fnWithoutBoxing$n(tup$n, op$n)")
  println()

for n <- 3 to 22 do
  mkFnTup(n)

*/
