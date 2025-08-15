// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-25 Rex Kerr and Calico Life Sciences LLC.

package kse.basics.intervals


//import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.annotation.targetName
import scala.reflect.ClassTag
import scala.util.boundary

import scala.collection.immutable.{Range => Rg}
import scala.collection.IntStepper

import kse.basics._


object Start {
  opaque type At = Int
  object At extends Translucent.Companion[At, Int] {
    inline def wrap(i: Int): At = i
    extension (at: At) {
      inline def unwrap: Int = at
      inline infix def of(iv: Iv): Int = Iv.i0(iv) + (at: Int)

      @targetName("to_a") inline infix def to(last: Int       ): Iv.Rsa = Iv.Rsa.fromValues(at: Int, last)
      @targetName("to_e") inline infix def to(last: End.At    ): Iv.Rse = Iv.Rse.fromValues(at: Int, last.unwrap)
      @targetName("to_E") inline infix def to(last: End.type  ): Iv.Rse = Iv.Rse.fromValues(at: Int, 0)
      @targetName("to_s") inline infix def to(last: Start.At  ): Iv.Rss = Iv.Rss.fromValues(at: Int, last: Int)
      @targetName("to_S") inline infix def to(last: Start.type): Iv.Rss = Iv.Rss.fromValues(at: Int, 0)
    }
  }

  def +(i: Int): At = i
  def -(i: Int): At = -i

  inline infix def of(iv: Iv): Int = Iv.i0(iv)

  @targetName("to_a") inline infix def to(last: Int       ): Iv.Rsa = Iv.Rsa.fromValues(0, last)
  @targetName("to_e") inline infix def to(last: End.At    ): Iv.Rse = Iv.Rse.fromValues(0, last.unwrap)
  @targetName("to_E") inline infix def to(last: End.type  ): Iv.Rse = Iv.Rse.fromValues(0, 0)
  @targetName("to_s") inline infix def to(last: Start.At  ): Iv.Rss = Iv.Rss.fromValues(0, last: Int)
  @targetName("to_S") inline infix def to(last: Start.type): Iv.Rss = Iv.Rss.fromValues(0, 0)
}


object End {
  opaque type At = Int
  object At extends Translucent.Companion[At, Int] {
    inline def wrap(i: Int): At = i
    extension (at: At) {
      inline def unwrap: Int = at
      inline infix def of[A](a: Array[A]): Int = a.length - 1 + (at: Int)
      inline infix def of(s: String): Int = s.length - 1 + (at: Int)
      inline infix def of(size: Int): Int = size - 1 + (at: Int)
      inline infix def of(iv: Iv): Int = Iv.iN(iv) - 1 + (at: Int)

      @targetName("to_a") inline infix def to(last: Int       ): Iv.Rea = Iv.Rea.fromValues(at: Int, last)
      @targetName("to_e") inline infix def to(last: End.At    ): Iv.Ree = Iv.Ree.fromValues(at: Int, last: Int)
      @targetName("to_E") inline infix def to(last: End.type  ): Iv.Ree = Iv.Ree.fromValues(at: Int, 0)
      @targetName("to_s") inline infix def to(last: Start.At  ): Iv.Res = Iv.Res.fromValues(at: Int, last.unwrap)
      @targetName("to_S") inline infix def to(last: Start.type): Iv.Res = Iv.Res.fromValues(at: Int, 0)
    }
  }

  inline def +(i: Int): At = i
  inline def -(i: Int): At = -i

  inline infix def of[A](a: Array[A]): Int = a.length - 1
  inline infix def of(s: String): Int = s.length - 1
  inline infix def of(size: Int): Int = size - 1
  inline infix def of(iv: Iv): Int = Iv.iN(iv) - 1

  @targetName("to_a") inline infix def to(last: Int       ): Iv.Rea = Iv.Rea.fromValues(0, last)
  @targetName("to_e") inline infix def to(last: End.At    ): Iv.Ree = Iv.Ree.fromValues(0, last: Int)
  @targetName("to_E") inline infix def to(last: End.type  ): Iv.Ree = Iv.Ree.fromValues(0, 0)
  @targetName("to_s") inline infix def to(last: Start.At  ): Iv.Res = Iv.Res.fromValues(0, last.unwrap)
  @targetName("to_S") inline infix def to(last: Start.type): Iv.Res = Iv.Res.fromValues(0, 0)
}



opaque type Iv = Long
object Iv extends Translucent.Companion[Iv, Long] {
  def apply(i0: Int, iN: Int): Iv =
    (i0 & 0xFFFFFFFFL) | (iN.toLong << 32)

  inline def wrap(l: Long): Iv = l

  inline infix def of(inline r: scala.collection.immutable.Range): Iv = basicsMacroImpl.rangePackedInLong(r)

  inline infix def of(s: String): Iv = apply(0, s.length)

  inline infix def of[A](a: Array[A]): Iv = apply(0, a.length)

  extension (iv: Iv) {
    inline def unwrap: Long = iv
    inline def i0: Int = (iv & 0xFFFFFFFFL).toInt
    inline def i1: Int = { val x = (iv >> 32).toInt; if x == Int.MinValue then x else x-1 }
    inline def iN: Int = (iv >>> 32).toInt
    inline def i0To(i: Int): Iv =
      (i & 0xFFFFFFFFL) | ((iv: Long) & 0xFFFFFFFF00000000L)
    inline def iNTo(i: Int): Iv =
      ((iv: Long) & 0xFFFFFFFFL) | (i.toLong << 32)
    inline def i0Op(inline f: Int => Int): Iv =
      (f(((iv: Long) & 0xFFFFFFFFL).toInt) & 0xFFFFFFFFL) | ((iv: Long) & 0xFFFFFFFF00000000L)
    inline def iNOp(inline f: Int => Int): Iv =
      ((iv: Long) & 0xFFFFFFFFL) | (f(((iv: Long) >>> 32).toInt).toLong << 32)
    inline def ops(inline f: Int => Int, inline g: Int => Int): Iv =
      (f(((iv: Long) & 0xFFFFFFFFL).toInt) & 0xFFFFFFFFL) | (g(((iv: Long) >>> 32).toInt).toLong << 32)
    def +#(i: Int): Iv =
      val i0 = (iv & 0xFFFFFFFFL).toInt
      val iN = (iv >>> 32).toInt
      var j = i
      if i >= 0 then
        if i0 + j < i0 then j = Int.MaxValue - i0
        if iN + j < iN then j = Int.MaxValue - iN
      else
        if i0 + j > i0 then j = Int.MinValue - i0
        if iN + j > iN then j = Int.MinValue - iN
      ((i0+j) & 0xFFFFFFFFL) | ((iN+j).toLong << 32)
    def -#(i: Int): Iv =
      val i0 = (iv & 0xFFFFFFFFL).toInt
      val iN = (iv >>> 32).toInt
      var j = i
      if i >= 0 then
        if i0 - j > i0 then j = i0 - Int.MinValue
        if iN - j > iN then j = iN - Int.MinValue
      else
        if i0 - j < i0 then j = i0 - Int.MaxValue
        if iN - j < iN then j = iN - Int.MaxValue
      ((i0-j) & 0xFFFFFFFFL) | ((iN-j).toLong << 32)
    def &(that: Iv): Iv =
      val i0 = (iv & 0xFFFFFFFFL)
      var i = (that & 0xFFFFFFFFL)
      if i0 > i then i = i0
      val iN = (iv >>> 32).toInt
      var j = (that >>> 32).toInt
      if iN < j then j = iN
      (i & 0xFFFFFFFFL) | (j.toLong << 32)
    def |(that: Iv): Iv =
      val i0 = (iv & 0xFFFFFFFFL)
      var i = (that & 0xFFFFFFFFL)
      val iN = (iv >>> 32).toInt
      var j = (that >>> 32).toInt
      if iN <= i0 then
        if j <= i then
          if i0 > i then i = i0
          if iN < j then j = iN
      else if j <= i then
        i = i0
        j = iN
      else
        if i0 < i then i = i0
        if iN > j then j = iN
      (i & 0xFFFFFFFFL) | (j.toLong << 32)
    inline def length: Int =
      val i = i0
      val j = iN
      if i < j then
        val n = j - i
        if n < 0 then Int.MaxValue else n
      else 0
    inline def isEmpty: Boolean = (iv & 0xFFFFFFFFL).toInt >= (iv >>> 32).toInt
    inline def contains(i: Int): Boolean = (i >= Iv.i0(iv)) && (i < Iv.iN(iv))
    def pr: String = s"${Iv.i0(iv)}..${Iv.iN(iv)}"
    def clippedToSize(n: Int): Iv =
      val i = i0
      val j = iN
      if i < 0 then
        if j > n then n.toLong << 32
        else if j <= 0 then 0L
        else j.toLong << 32
      else if j > n then
        if i > n then (n & 0xFFFFFFFFL) | (n.toLong << 32)
        else (i & 0xFFFFFFFFL) | (n.toLong << 32)
      else iv
    inline def clippedTo[A](a: Array[A]): Iv = clippedToSize(a.length)
    inline def clippedTo(a: String): Iv = clippedToSize(a.length)

    inline def visit(inline f: Int => Unit): Unit =
      var i = (iv & 0xFFFFFFFFL).toInt
      val j = (iv >>> 32).toInt
      while i < j do
        f(i)
        i += 1

    def where(): Array[Int] =
      val i = (iv & 0xFFFFFFFFL).toInt
      val j = (iv >>> 32).toInt
      val a = new Array[Int](if j > i then j - i else 0)
      var k = i
      var h = 0
      while k < j do
        a(h) = k
        k += 1
        h += 1
      a

    def whereBy(step: Int): Array[Int] =
      val i = (iv & 0xFFFFFFFFL).toInt
      val j = (iv >>> 32).toInt
      if step == 0 || i >= j then Array.empty[Int]
      else
        var start = if step < 0 then j-1 else i
        val sabs = if step < 0 then -step else step
        var n = java.lang.Integer.divideUnsigned(j-i, sabs)
        if j != i + n*sabs then n += 1
        val a = new Array[Int](n)
        n = 0
        while n < a.length do
          a(n) = start
          n += 1
          start += step
        a

    def steps(): IntStepper =
      val i = iv.i0
      val j = iv.iN
      if i < j then new AffineIntStepper(j - i, i, 1) else AffineIntStepper.empty

    def stepsBy(step: Int): IntStepper =
      val i = iv.i0
      val j = iv.iN
      if step == 0 || i >= j then AffineIntStepper.empty
      else
        val start = if step < 0 then j-1 else i
        val sabs = if step < 0 then -step else step
        val n = java.lang.Integer.divideUnsigned(j-i, sabs)
        val extra = if j == i + n*sabs then 0 else 1
        new AffineIntStepper(n + extra, start, step)

    inline def unfold[A](inline f: Int => A)(using ClassTag[A]): Array[A] =
      var i = iv.i0
      val j = iv.iN
      val a = new Array[A](if i < j then j - i else 0)
      var k = 0
      while k < a.length do
        a(k) = f(i)
        i += 1
        k += 1
      a

    inline def unfoldFlex[A](inline f: boundary.Label[shortcut.Type] ?=> Int => A)(using ClassTag[A]): Array[A] =
      var i = iv.i0
      val j = iv.iN
      val a = new Array[A](if i < j then j - i else 0)
      var k = 0
      shortcut.outer:
        while i < j do
          shortcut.inner:
            a(k) = f(i)
            k += 1
          i += 1
      if a.length == k then a
      else
        val aa = new Array[A](k)
        System.arraycopy(a, 0, aa, 0, k)
        aa
  }


  final class AffineIntStepper(count: Int, offset: Int, scale: Int) extends IntStepper {
    private var remaining = count
    private var i = offset
    def characteristics =
      import java.util.Spliterator.*
      ORDERED | SIZED | SUBSIZED | NONNULL | IMMUTABLE
    def hasStep = remaining != 0
    def nextStep =
      if remaining == 0 then throw new NoSuchElementException("Empty Stepper")
      else
        val ans = i
        i += scale
        remaining -= 1
        ans
    def trySplit() =
      if remaining < 2 then null
      else
        val r = remaining >>> 2
        val j = i
        i += r * scale
        remaining -= r
        new AffineIntStepper(r, j, scale)
    def estimateSize = remaining & 0xFFFFFFFFL
  }
  object AffineIntStepper {
    val empty = new AffineIntStepper(0, 0, 1)
    def forever(value: Int): IntStepper = new IntStepper {
      def characteristics =
        import java.util.Spliterator.*
        NONNULL | IMMUTABLE
      def hasStep = true
      def nextStep = value
      def trySplit() = forever(value)
      def estimateSize = Long.MaxValue
    }
  }

  type Pt = Int | End.At | End.type | Start.At | Start.type

  type In = Int | String | Array[?] | Iv

  inline def zero(target: Int | String | Array[?] | Iv): Int = inline target match
    case iv: Iv => Iv.i0(iv)
    case _ => 0

  inline def one(target: Int | String | Array[?] | Iv): Int = inline target match
    case n: Int => n - 1
    case s: String => s.length - 1
    case a: Array[?] => a.length - 1
    case iv: Iv => Iv.iN(iv) - 1

  inline def point(where: Pt, in: In) = inline where match
    case x: Int => x
    case e: End.At => inline in match
      case i: Int => i - 1
      case s: String => e of s
      case a: Array[?] => e of a
      case iv: Iv => e of iv
    case e: End.type => one(in)
    case s: Start.At => inline in match
      case iv: Iv => s of iv
      case _      => s.unwrap
    case s: Start.type => inline in match
      case iv: Iv => iv.i0
      case _      => 0
        
  inline def up(i: Int) = if i == Int.MaxValue then Int.MaxValue else i+1
  inline def dn(i: Int) = if i == Int.MinValue then Int.MinValue else i-1

  final val empty: Iv = 0L

  opaque type Raa = Long
  object Raa extends Translucent.Companion[Raa, Long] {
    inline def fromValues(first: Int, last: Int): Raa = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Raa = value
    extension (raa: Raa)
      inline def unwrap: Long = raa
      inline def i0: Int = ((raa: Long) & 0xFFFFFFFFL).toInt
      inline def i1: Int = ((raa: Long) >>> 32).toInt
      inline def iv: Iv = if ((raa: Long) & 0xFFFFFFFF00000000L) == 0x7FFFFFFF00000000L then Iv.wrap(raa: Long) else Iv.wrap((raa: Long) + 0x100000000L)
      def len: Long = java.lang.Math.max(1 + ((raa: Long) >>> 32) - ((raa: Long) & 0xFFFFFFFFL), 0L)
  }

  opaque type Rae = Long
  object Rae extends Translucent.Companion[Rae, Long] {
    inline def fromValues(first: Int, last: Int): Rae = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Rae = value
    extension (rae: Rae)
      inline def unwrap: Long = rae
      inline def i0: Int = ((rae: Long) & 0xFFFFFFFFL).toInt
      inline def i1(in: In): Int = ((rae: Long) >>> 32).toInt + one(in)
      inline def iv(in: In): Iv = Iv(i0, Iv.up(i1(in)))
      inline def last: End.At = End.At.wrap(((rae: Long) >>> 32).toInt)
  }

  opaque type Ras = Long
  object Ras extends Translucent.Companion[Ras, Long] {
    inline def fromValues(first: Int, last: Int): Ras = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Ras = value
    extension (ras: Ras)
      inline def unwrap: Long = ras
      inline def i0: Int = ((ras: Long) & 0xFFFFFFFFL).toInt
      inline def i1(in: In): Int = ((ras: Long) >>> 32).toInt + zero(in)
      inline def iv(in: In): Iv = Iv(i0, Iv.up(i1(in)))
      inline def last: Start.At = Start.At.wrap(((ras: Long) >>> 32).toInt)
  }

  opaque type Rea = Long
  object Rea extends Translucent.Companion[Rea, Long] {
    inline def fromValues(first: Int, last: Int): Rea = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Rea = value
    extension (rea: Rea)
      inline def unwrap: Long = rea
      inline def i0(in: In): Int = ((rea: Long) & 0xFFFFFFFFL).toInt + one(in)
      inline def i1: Int = ((rea: Long) >>> 32).toInt
      inline def iv(in: In): Iv = Iv(i0(in), Iv.up(i1))
      inline def first: End.At = End.At.wrap(((rea: Long) & 0xFFFFFFFFL).toInt)
  }

  opaque type Ree = Long
  object Ree extends Translucent.Companion[Ree, Long] {
    inline def fromValues(first: Int, last: Int): Ree = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Ree = value
    extension (ree: Ree)
      inline def unwrap: Long = ree
      inline def i0(in: In): Int = ((ree: Long) & 0xFFFFFFFFL).toInt + one(in)
      inline def i1(in: In): Int = ((ree: Long) >>> 32).toInt + one(in)
      inline def iv(in: In): Iv = Iv(i0(in), Iv.up(i1(in)))
      inline def first: End.At = End.At.wrap(((ree: Long) & 0xFFFFFFFFL).toInt)
      inline def last: End.At = End.At.wrap(((ree: Long) >>> 32).toInt)
      def len: Long = java.lang.Math.max(1 + ((ree: Long) >>> 32) - ((ree: Long) & 0xFFFFFFFFL), 0L)
  }

  opaque type Res = Long
  object Res extends Translucent.Companion[Res, Long] {
    inline def fromValues(first: Int, last: Int): Res = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Res = value
    extension (res: Res)
      inline def unwrap: Long = res
      inline def i0(in: In): Int = ((res: Long) & 0xFFFFFFFFL).toInt + one(in)
      inline def i1(in: In): Int = ((res: Long) >>> 32).toInt + zero(in)
      inline def iv(in: In): Iv = Iv(i0(in), Iv.up(i1(in)))
      inline def first: End.At = End.At.wrap(((res: Long) & 0xFFFFFFFFL).toInt)
      inline def last: Start.At = Start.At.wrap(((res: Long) >>> 32).toInt)
  }

  opaque type Rsa = Long
  object Rsa extends Translucent.Companion[Rsa, Long] {
    inline def fromValues(first: Int, last: Int): Rsa = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Rsa = value
    extension (rsa: Rsa)
      inline def unwrap: Long = rsa
      inline def i0(in: In): Int = ((rsa: Long) & 0xFFFFFFFFL).toInt + zero(in)
      inline def i1: Int = ((rsa: Long) >>> 32).toInt
      inline def iv(in: In): Iv = Iv(i0(in), Iv.up(i1))
      inline def first: Start.At = Start.At.wrap(((rsa: Long) & 0xFFFFFFFFL).toInt)
  }

  opaque type Rse = Long
  object Rse extends Translucent.Companion[Rse, Long] {
    inline def fromValues(first: Int, last: Int): Rse = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Rse = value
    extension (rse: Rse)
      inline def unwrap: Long = rse
      inline def i0(in: In): Int = ((rse: Long) & 0xFFFFFFFFL).toInt + zero(in)
      inline def i1(in: In): Int = ((rse: Long) >>> 32).toInt + one(in)
      inline def iv(in: In): Iv = Iv(i0(in), Iv.up(i1(in)))
      inline def first: Start.At = Start.At.wrap(((rse: Long) & 0xFFFFFFFFL).toInt)
      inline def last: End.At = End.At.wrap(((rse: Long) >>> 32).toInt)
  }

  opaque type Rss = Long
  object Rss extends Translucent.Companion[Rss, Long] {
    inline def fromValues(first: Int, last: Int): Rss = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Rss = value
    extension (rss: Rss)
      inline def unwrap: Long = rss
      inline def i0(in: In): Int = ((rss: Long) & 0xFFFFFFFFL).toInt + zero(in)
      inline def i1(in: In): Int = ((rss: Long) >>> 32).toInt + zero(in)
      inline def iv(in: In): Iv = Iv(i0(in), Iv.up(i1(in)))
      inline def first: Start.At = Start.At.wrap(((rss: Long) & 0xFFFFFFFFL).toInt)
      inline def last: Start.At = Start.At.wrap(((rss: Long) >>> 32).toInt)
      def len: Long = java.lang.Math.max(1 + ((rss: Long) >>> 32) - ((rss: Long) & 0xFFFFFFFFL), 0L)
  }

  opaque type Rxy = Long
  object Rxy extends Translucent.Companion[Rxy, Long] {
    inline def fromValues(first: Int, last: Int): Rxy = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Rxy = value
    extension (rxy: Rxy)
      inline def unwrap: Long = rxy
      inline def i0(in: In): Int =
        val x = ((rxy: Long) & 0xFFFFFFFFL).toInt
        inline in match
          case n: Int      => if x < 0 then x + n        else x
          case s: String   => if x < 0 then x + s.length else x
          case a: Array[?] => if x < 0 then x + a.length else x
          case iv: Iv      => if x < 0 then x + ((iv: Long) >>> 32).toInt else ((iv: Long) & 0xFFFFFFFFL).toInt + x
      inline def i1(in: In): Int =
        val y = ((rxy: Long) >>> 32).toInt
        inline in match
          case n: Int      => if y < 0 then y + n        else y
          case s: String   => if y < 0 then y + s.length else y
          case a: Array[?] => if y < 0 then y + a.length else y
          case iv: Iv      => if y < 0 then y + ((iv: Long) >>> 32).toInt else ((iv: Long) & 0xFFFFFFFFL).toInt + y
      inline def iv(in: In) = Iv(Rxy.i0(rxy)(in), Iv.up(Rxy.i1(rxy)(in)))
  }

  type R = Raa | Rae | Ras | Rea | Ree | Res | Rsa | Rse | Rss | Rxy

  type R2 = Raa | Rae | Ras | Rea | Ree | Res | Rsa | Rse | Rss

  type X = Iv | R

  type Y = Iv | R2
}
