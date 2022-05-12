// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2021-22 Rex Kerr, UCSF, and Calico Life Sciences LLC.


package kse.flow


sealed trait CanHop[A] {
  def hop(a: A): Nothing
}
object CanHop {
  sealed trait Unit extends CanHop[scala.Unit] {
    def hop(): Nothing; def hop(unit: scala.Unit): Nothing = hop()
  }
  sealed trait Int    extends CanHop[scala.Int]    { def hop(int: scala.Int): Nothing }
  sealed trait Long   extends CanHop[scala.Long]   { def hop(long: scala.Long): Nothing }
  sealed trait Float  extends CanHop[scala.Float]  { def hop(float: scala.Float): Nothing }
  sealed trait Double extends CanHop[scala.Double] { def hop(double: scala.Double): Nothing }
  sealed trait Any[A] extends CanHop[A]            { def hop(a: A): Nothing }

  final class Map[A, B](f: A => B)(using ch: CanHop[B]) extends CanHop[A] { def hop(a: A): Nothing = ch.hop(f(a)) }
}


sealed abstract class Hop[A] extends scala.util.control.ControlThrowable("") {
  def value: A
  override def getMessage = value.toString
  override def toString = s"Hop($value)"
}
object Hop {
  sealed abstract class Unit   extends Hop[scala.Unit]   { def u = ();          def value = u }
  sealed abstract class Int    extends Hop[scala.Int]    { def i: scala.Int;    def value = i }
  sealed abstract class Long   extends Hop[scala.Long]   { def l: scala.Long;   def value = l }
  sealed abstract class Float  extends Hop[scala.Float]  { def f: scala.Float;  def value = f }
  sealed abstract class Double extends Hop[scala.Double] { def d: scala.Double; def value = d }
  sealed abstract class Any[A] extends Hop[A]            { def a: A;            def value = a }

  final class UnitImpl   extends Unit   with CanHop.Unit   {                               def hop(): Nothing                     = throw this                           }
  final class IntImpl    extends Int    with CanHop.Int    { var i = 0;                    def hop(int: scala.Int): Nothing       = { i = int;    throw this } }
  final class LongImpl   extends Long   with CanHop.Long   { var l = 0;                    def hop(long: scala.Long): Nothing     = { l = long;   throw this } }
  final class FloatImpl  extends Float  with CanHop.Float  { var f = 0;                    def hop(float: scala.Float): Nothing   = { f = float;  throw this } }
  final class DoubleImpl extends Double with CanHop.Double { var d = 0;                    def hop(double: scala.Double): Nothing = { d = double; throw this } }
  final class AnyImpl[A] extends Any[A] with CanHop.Any[A] { var a = null.asInstanceOf[A]; def hop(any: A): Nothing               = { a = any;    throw this } }

  def unit  (f: CanHop.Unit   ?=> scala.Unit  ): scala.Unit   = { given ch: UnitImpl   = new UnitImpl;   try { f } catch { case h: Hop.Unit   if ch eq h => ()   } }
  def int   (f: CanHop.Int    ?=> scala.Int   ): scala.Int    = { given ch: IntImpl    = new IntImpl;    try { f } catch { case h: Hop.Int    if ch eq h => ch.i } }
  def long  (f: CanHop.Long   ?=> scala.Long  ): scala.Long   = { given ch: LongImpl   = new LongImpl;   try { f } catch { case h: Hop.Long   if ch eq h => ch.l } }
  def float (f: CanHop.Float  ?=> scala.Float ): scala.Float  = { given ch: FloatImpl  = new FloatImpl;  try { f } catch { case h: Hop.Float  if ch eq h => ch.f } }
  def double(f: CanHop.Double ?=> scala.Double): scala.Double = { given ch: DoubleImpl = new DoubleImpl; try { f } catch { case h: Hop.Double if ch eq h => ch.d } }
  def any[A](f: CanHop.Any[A] ?=> A           ): A            = { given ch: AnyImpl[A] = new AnyImpl[A]; try { f } catch { case h: Hop.Any[_] if ch eq h => ch.a } }
  
  def map[A, B](f: A => B)(using chb: CanHop[B])(g: CanHop[A] ?=> A): A = g(using new CanHop.Map[A, B](f)(using chb))

  inline def out(               )(using ch: CanHop[scala.Unit]): Nothing = ch.hop(())
  inline def out(x: scala.Int   )(using ch: CanHop.Int        ): Nothing = ch hop x
  inline def out(x: scala.Long  )(using ch: CanHop.Long       ): Nothing = ch hop x
  inline def out(x: scala.Double)(using ch: CanHop.Double     ): Nothing = ch hop x
  inline def out(x: scala.Float )(using ch: CanHop.Float      ): Nothing = ch hop x
  inline def out[A](x: A        )(using ch: CanHop[A]         ): Nothing = ch hop x
}
