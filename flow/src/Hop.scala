// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2021-22 Rex Kerr, UCSF, and Calico Life Sciences LLC.


package kse.flow

/** A typeclass witnessing that you can use stackless exceptions to hop out, with a value, to an enclosing context */
sealed trait CanHop[A] {
  /** Transports the value `a` of type `A` to the enclosing context */
  def hop(a: A): Nothing
}
object CanHop {
  sealed trait Unit extends CanHop[scala.Unit] {
    def hop(): Nothing

    def hop(unit: scala.Unit): Nothing = hop()
  }
  sealed trait Int    extends CanHop[scala.Int]    { def hop(int: scala.Int): Nothing }
  sealed trait Long   extends CanHop[scala.Long]   { def hop(long: scala.Long): Nothing }
  sealed trait Float  extends CanHop[scala.Float]  { def hop(float: scala.Float): Nothing }
  sealed trait Double extends CanHop[scala.Double] { def hop(double: scala.Double): Nothing }
  sealed trait Any[A] extends CanHop[A]            { def hop(a: A): Nothing }

  final class Map[A, B](f: A => B)(using ch: CanHop[B]) extends CanHop[A] { def hop(a: A): Nothing = ch.hop(f(a)) }
}

/** A stackless exception class that transports a value out to an enclosing context that provides a `CanHop`.
  *
  * Hops can be nested; to jump to the outer context, explicitly say `using` with a named outer context.
  * 
  * Basic usage:
  * {{{
  * val xs = 5 :: -5 :: 5 :: Nil
  * val sumOrError = Hop.int{
  *   xs.foldLeft(0)((acc, n) => if n < 0 then n.hop else acc + n)
  * }
  * // Returns -5 because the negative value is hopped out
  * }}}
  * 
  * Nested usage:
  * {{{
  * // Want to sum these lists, stopping immediately and returning a negative value if one is found
  * // but also adding 0 for a list and stop summing it if a 0 is reached
  * val xss = List(5 :: 3 :: 0 :: Nil, 2 :: -1 :: 3 :: Nil, 4 :: Nil)
  * val sumOrError = Hop.int{ outer ?=>
  *   xss.foldLeft(0){ (acc, xs) =>
  *     val sum = Hop.int{ xs.foldLeft(0){ (ac2, x) =>
  *       if x < 0 then x.hop(using outer)
  *       else if x == 0 then 0.hop
  *       else ac2 + x
  *     }}
  *   }
  * }
  * }}}
  */
sealed abstract class Hop[A] extends scala.util.control.ControlThrowable("") {
  /** The value */
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

  /** Creates a context from which you can hop with a Unit value.
    * 
    * Usage:
    * {{{
    * Hop.unit{ val s = getString(); if s.isEmpty then ().hop; println(s) }
    * }}} 
    */
  def unit  (f: CanHop.Unit   ?=> scala.Unit  ): scala.Unit   = { given ch: UnitImpl   = new UnitImpl;   try { f } catch { case h: Hop.Unit   if ch eq h => ()   } }

  /** Creates a context from which you can hop with an Int value.
    * 
    * Usage:
    * {{{
    * Hop.int{ val i = getInt(); if i < 0 then i.hop; println(s"$i is nonnegative"); i }
    * }}} 
    */
  def int   (f: CanHop.Int    ?=> scala.Int   ): scala.Int    = { given ch: IntImpl    = new IntImpl;    try { f } catch { case h: Hop.Int    if ch eq h => ch.i } }

  /** Creates a context from which you can hop with a Long value.
    * 
    * Usage:
    * {{{
    * Hop.long{ val l = getLong(); if l < 0 || l > Int.MaxValue then l.hop; println(s"Embiggening $l"); l*l }
    * }}} 
    */
  def long  (f: CanHop.Long   ?=> scala.Long  ): scala.Long   = { given ch: LongImpl   = new LongImpl;   try { f } catch { case h: Hop.Long   if ch eq h => ch.l } }
 
  /** Creates a context from which you can hop with a Float value.
    * 
    * Usage:
    * {{{
    * Hop.float{ val f = getFloat(); if f <= 0 then 0f.hop; 1.0f/f }
    * }}} 
    */
  def float (f: CanHop.Float  ?=> scala.Float ): scala.Float  = { given ch: FloatImpl  = new FloatImpl;  try { f } catch { case h: Hop.Float  if ch eq h => ch.f } }

  /** Creates a context from which you can hop with a Dloat value.
    * 
    * Usage:
    * {{{
    * Hop.double{ val d = getDouble(); if d <= 0 then 0.0.hop; 1.0/d }
    * }}} 
    */
  def double(f: CanHop.Double ?=> scala.Double): scala.Double = { given ch: DoubleImpl = new DoubleImpl; try { f } catch { case h: Hop.Double if ch eq h => ch.d } }

  /** Creates a context from which you can hop with a value of a particular type.
    * 
    * Usage:
    * {{{
    * Hop.any[String]{ val s = getString(); if s.length > 5 then s.hop; s + s.toUpperCase }
    * }}} 
    */
  def any[A](f: CanHop.Any[A] ?=> A           ): A            = { given ch: AnyImpl[A] = new AnyImpl[A]; try { f } catch { case h: Hop.Any[_] if ch eq h => ch.a } }
  
  /** Creates a new context that will map into some existing context for hopping.
    *
    * Usage:
    * {{{
    * Hop.any[String]{
    *   val s = getString()
    *   if isBad(s) then s"$s is bad".hop
    *   val times = Hop.map((d: Double) => s"Bad quantity: $d"){
    *     val d = getDouble()
    *     if d.rint != d || d < 1 then d.hop
    *     d
    *   }.toInt
    *   s * times
    * }
    * }}}
    */
  def map[A, B](f: A => B)(using chb: CanHop[B])(g: CanHop[A] ?=> A): A = g(using new CanHop.Map[A, B](f)(using chb))

  /** Creates a new context that hops disfavored Or values.
    * 
    * Usage:
    * {{{
    * def rooty(d: Double): Double Or String =
    *   Hop.alt[String]{
    *     val d = getDouble()
    *     if d < 0 then s"Root of negative number: $d".hop
    *     val root = math.sqrt(d)
    *     root + 1
    *   }
    * }}}
    */
  inline def alt[Y]: kse.flow.Hop.HopOrDispatcher[Y] = new AnyImpl[Y]()

  opaque type HopOrDispatcher[Y] = AnyImpl[Y]
  object HopOrDispatcher {
    extension [Y](ch: HopOrDispatcher[Y])
      inline def apply[X](inline f: CanHop[Y] ?=> X): X Or Y =
        try
          val x = f(using ch)
          Is(x)
        catch
          case h: Hop.Any[_] if h eq ch => Alt(ch.a)
  }
}

extension (u: scala.Unit)
  /** Hops a Unit value in a context where Unit can hop. */
  inline def hop(using ch: CanHop[scala.Unit]): Nothing = ch.hop(())

extension (z: scala.Boolean)
  /** Hops a Boolean value in a context where Boolean can hop. */
  inline def hop(using ch: CanHop[scala.Boolean]): Nothing = ch hop z

extension (b: scala.Byte)
  /** Hops a Byte value in a context where Byte can hop. */
  inline def hop(using ch: CanHop[scala.Byte]): Nothing = ch hop b

extension (c: scala.Char)
  /** Hops a Char value in a context where Char can hop. */
  inline def hop(using ch: CanHop[scala.Char]): Nothing = ch hop c

extension (s: scala.Short)
  /** Hops a Short value in a context where Short can hop. */
  inline def hop(using ch: CanHop[scala.Short]): Nothing = ch hop s

extension (i: scala.Int)
  /** Hops an Int value in a context where Int can hop. */
  inline def hop(using ch: CanHop[scala.Int]): Nothing = ch hop i

extension (l: scala.Long)
  /** Hops a Long value in a context where Long can hop. */
  inline def hop(using ch: CanHop[scala.Long]): Nothing = ch hop l

extension (f: scala.Float)
  /** Hops a Float value in a context where Float can hop. */
  inline def hop(using ch: CanHop[scala.Float]): Nothing = ch hop f

extension (d: scala.Double)
  /** Hops a Double value in a context where Double can hop. */
  inline def hop(using ch: CanHop[scala.Double]): Nothing = ch hop d

extension [A](a: A)
  /** Hops a value in a context where a value of that type can hop. */
  inline def hop(using ch: CanHop[A]): Nothing = ch hop a
