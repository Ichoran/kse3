// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2022-23 Rex Kerr and Calico Life Sciences LLC.

package kse.basics.intervals


// import scala.language.`3.6-migration` -- tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.annotation.targetName

import scala.collection.immutable.{Range => Rg}

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

  extension (iv: Iv)
    inline def unwrap: Long = iv
    inline def i0: Int = (iv & 0xFFFFFFFFL).toInt
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
    def shiftIntoSize(n: Int): Iv =
      if n <= 0 then 0L
      else
        var i = i0
        var j = iN
        if j <= i then
          if i < 0 then 0L
          else
            if i >= n then i = n
            (i & 0xFFFFFFFFL) | (i.toLong << 32)
        else if i < 0 then
          j -= i
          if j < 0 || j > n then j = n
          j.toLong << 32
        else if j > n then
          i -= j - n
          if i < 0 || i > j then i = 0
          (i & 0xFFFFFFFFL) | (n.toLong << 32)
        else iv
    inline def shiftInto[A](a: Array[A]): Iv = shiftIntoSize(a.length)
    inline def shiftInto(a: String): Iv = shiftIntoSize(a.length)

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

  inline def zero(target: Int | String | Array[?] | Iv): Int = inline target match
    case iv: Iv => Iv.i0(iv)
    case _ => 0

  inline def one(target: Int | String | Array[?] | Iv): Int = inline target match
    case n: Int => n - 1
    case s: String => s.length - 1
    case a: Array[?] => a.length - 1
    case iv: Iv => Iv.iN(iv) - 1

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
      inline def i1(target: Int | String | Array[?] | Iv): Int = ((rae: Long) >>> 32).toInt + one(target)
      inline def iv(target: Int | String | Array[?] | Iv): Iv = Iv(i0, Iv.up(i1(target)))
      inline def last: End.At = End.At.wrap(((rae: Long) >>> 32).toInt)
  }

  opaque type Ras = Long
  object Ras extends Translucent.Companion[Ras, Long] {
    inline def fromValues(first: Int, last: Int): Ras = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Ras = value
    extension (ras: Ras)
      inline def unwrap: Long = ras
      inline def i0: Int = ((ras: Long) & 0xFFFFFFFFL).toInt
      inline def i1(target: Int | String | Array[?] | Iv): Int = ((ras: Long) >>> 32).toInt + zero(target)
      inline def iv(target: Int | String | Array[?] | Iv): Iv = Iv(i0, Iv.up(i1(target)))
      inline def last: Start.At = Start.At.wrap(((ras: Long) >>> 32).toInt)
  }

  opaque type Rea = Long
  object Rea extends Translucent.Companion[Rea, Long] {
    inline def fromValues(first: Int, last: Int): Rea = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Rea = value
    extension (rea: Rea)
      inline def unwrap: Long = rea
      inline def i0(target: Int | String | Array[?] | Iv): Int = ((rea: Long) & 0xFFFFFFFFL).toInt + one(target)
      inline def i1: Int = ((rea: Long) >>> 32).toInt
      inline def iv(target: Int | String | Array[?] | Iv): Iv = Iv(i0(target), Iv.up(i1))
      inline def first: End.At = End.At.wrap(((rea: Long) & 0xFFFFFFFFL).toInt)
  }

  opaque type Ree = Long
  object Ree extends Translucent.Companion[Ree, Long] {
    inline def fromValues(first: Int, last: Int): Ree = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Ree = value
    extension (ree: Ree)
      inline def unwrap: Long = ree
      inline def i0(target: Int | String | Array[?] | Iv): Int = ((ree: Long) & 0xFFFFFFFFL).toInt + one(target)
      inline def i1(target: Int | String | Array[?] | Iv): Int = ((ree: Long) >>> 32).toInt + one(target)
      inline def iv(target: Int | String | Array[?] | Iv): Iv = Iv(i0(target), Iv.up(i1(target)))
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
      inline def i0(target: Int | String | Array[?] | Iv): Int = ((res: Long) & 0xFFFFFFFFL).toInt + one(target)
      inline def i1(target: Int | String | Array[?] | Iv): Int = ((res: Long) >>> 32).toInt + zero(target)
      inline def iv(target: Int | String | Array[?] | Iv): Iv = Iv(i0(target), Iv.up(i1(target)))
      inline def first: End.At = End.At.wrap(((res: Long) & 0xFFFFFFFFL).toInt)
      inline def last: Start.At = Start.At.wrap(((res: Long) >>> 32).toInt)
  }

  opaque type Rsa = Long
  object Rsa extends Translucent.Companion[Rsa, Long] {
    inline def fromValues(first: Int, last: Int): Rsa = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Rsa = value
    extension (rsa: Rsa)
      inline def unwrap: Long = rsa
      inline def i0(target: Int | String | Array[?] | Iv): Int = ((rsa: Long) & 0xFFFFFFFFL).toInt + zero(target)
      inline def i1: Int = ((rsa: Long) >>> 32).toInt
      inline def iv(target: Int | String | Array[?] | Iv): Iv = Iv(i0(target), Iv.up(i1))
      inline def first: Start.At = Start.At.wrap(((rsa: Long) & 0xFFFFFFFFL).toInt)
  }

  opaque type Rse = Long
  object Rse extends Translucent.Companion[Rse, Long] {
    inline def fromValues(first: Int, last: Int): Rse = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Rse = value
    extension (rse: Rse)
      inline def unwrap: Long = rse
      inline def i0(target: Int | String | Array[?] | Iv): Int = ((rse: Long) & 0xFFFFFFFFL).toInt + zero(target)
      inline def i1(target: Int | String | Array[?] | Iv): Int = ((rse: Long) >>> 32).toInt + one(target)
      inline def iv(target: Int | String | Array[?] | Iv): Iv = Iv(i0(target), Iv.up(i1(target)))
      inline def first: Start.At = Start.At.wrap(((rse: Long) & 0xFFFFFFFFL).toInt)
      inline def last: End.At = End.At.wrap(((rse: Long) >>> 32).toInt)
  }

  opaque type Rss = Long
  object Rss extends Translucent.Companion[Rss, Long] {
    inline def fromValues(first: Int, last: Int): Rss = ((first & 0xFFFFFFFFL) | (last.toLong << 32))
    inline def wrap(value: Long): Rss = value
    extension (rss: Rss)
      inline def unwrap: Long = rss
      inline def i0(target: Int | String | Array[?] | Iv): Int = ((rss: Long) & 0xFFFFFFFFL).toInt + zero(target)
      inline def i1(target: Int | String | Array[?] | Iv): Int = ((rss: Long) >>> 32).toInt + zero(target)
      inline def iv(target: Int | String | Array[?] | Iv): Iv = Iv(i0(target), Iv.up(i1(target)))
      inline def first: Start.At = Start.At.wrap(((rss: Long) & 0xFFFFFFFFL).toInt)
      inline def last: Start.At = Start.At.wrap(((rss: Long) >>> 32).toInt)
      def len: Long = java.lang.Math.max(1 + ((rss: Long) >>> 32) - ((rss: Long) & 0xFFFFFFFFL), 0L)
  }

  type R = Raa | Rae | Ras | Rea | Ree | Res | Rsa | Rse | Rss

  type X = Iv | R
}

