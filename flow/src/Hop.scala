package kse.flow

trait CanHop[A] {
  def hop(a: A): Nothing
  def owns(h: Hop[_]): Boolean = h.catcher eq this
}
object CanHop {
  final class Unit extends CanHop[scala.Unit] {
    def hop(): Nothing = throw new Hop.Unit(this)
    def hop(unit: scala.Unit): Nothing = throw new Hop.Unit(this)
  }
  final class Int extends CanHop[scala.Int] {
    def hop(double: scala.Int): Nothing = throw new Hop.Int(double, this)
  }
  final class Long extends CanHop[scala.Long] {
    def hop(double: scala.Long): Nothing = throw new Hop.Long(double, this)
  }
  final class Double extends CanHop[scala.Double] {
    def hop(double: scala.Double): Nothing = throw new Hop.Double(double, this)
  }
  final class Float extends CanHop[scala.Float] {
    def hop(double: scala.Float): Nothing = throw new Hop.Float(double, this)
  }
  sealed class Any[A] extends CanHop[A] {
    def hop(a: A): Nothing = throw new Hop.Any(a, this)
  }
  final class Map[A, B](f: A => B)(using ch: CanHop[B]) extends CanHop[A] {
    def hop(a: A): Nothing = throw new Hop.Any(f(a), ch)
    override def owns(h: Hop[_]): Boolean = ch owns h
  }
}

abstract class Hop[A] extends scala.util.control.ControlThrowable("") {
  def value: A
  def catcher: CanHop[A]
  override def getMessage = value.toString
  override def toString = s"Hop($value)"
}

object Hop {
  final class Unit(val catcher: CanHop.Unit) extends Hop[scala.Unit] {
    def value = ()
  }
  final class Int(val value: scala.Int,       val catcher: CanHop[scala.Int]   ) extends Hop[scala.Int] {}
  final class Long(val value: scala.Long,     val catcher: CanHop[scala.Long]  ) extends Hop[scala.Long] {}
  final class Double(val value: scala.Double, val catcher: CanHop[scala.Double]) extends Hop[scala.Double] {}
  final class Float(val value: scala.Float,   val catcher: CanHop[scala.Float] ) extends Hop[scala.Float] {}
  final class Any[A](val value: A,            val catcher: CanHop[A]           ) extends Hop[A] {}

  def unit(f: CanHop.Unit ?=> scala.Unit): scala.Unit = {
    given ch: CanHop.Unit = new CanHop.Unit
    try { f } catch { case h: Hop.Unit if ch owns h => () }
  }

  def int(f: CanHop.Int ?=> scala.Int): scala.Int = {
    given ch: CanHop.Int = new CanHop.Int
    try { f } catch { case h: Hop.Int if ch owns h => h.value }
  }

  def long(f: CanHop.Long ?=> scala.Long): scala.Long = {
    given ch: CanHop.Long = new CanHop.Long
    try { f } catch { case h: Hop.Long if ch owns h => h.value }
  }

  def double(f: CanHop.Double ?=> scala.Double): scala.Double = {
    given ch: CanHop.Double = new CanHop.Double
    try { f } catch { case h: Hop.Double if ch owns h => h.value }
  }

  def double(f: CanHop.Float ?=> scala.Float): scala.Float = {
    given ch: CanHop.Float = new CanHop.Float
    try { f } catch { case h: Hop.Float if ch owns h => h.value }
  }

  def any[A](f: CanHop[A] ?=> A): A = {
    given ch: CanHop[A] = new CanHop.Any[A]
    try { f } catch { case h: Hop[_] if ch owns h => h.value.asInstanceOf[A] }
  }

  inline def out(               )(using ch: CanHop[scala.Unit]): Nothing = ch.hop(())
  inline def out(x: scala.Int   )(using ch: CanHop.Int        ): Nothing = ch hop x
  inline def out(x: scala.Long  )(using ch: CanHop.Long       ): Nothing = ch hop x
  inline def out(x: scala.Double)(using ch: CanHop.Double     ): Nothing = ch hop x
  inline def out(x: scala.Float )(using ch: CanHop.Float      ): Nothing = ch hop x
  inline def out[A](x: A        )(using ch: CanHop[A]         ): Nothing = ch hop x

  inline def map[A, B, Z](f: A => B)(g: CanHop[A] ?=> Z)(using chb: CanHop[B]): Z = g(using new CanHop.Map(f))
}
