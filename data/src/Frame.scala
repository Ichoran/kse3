// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences, LLC.

package kse.data


import java.lang.{Math => jm}

import scala.compiletime.{erasedValue, summonFrom}
import scala.reflect.ClassTag

import kse.flow._
import kse.maths._

sealed trait Stripe[E <: Stripe.Datum] {
  val length: Int
  def genericGet(i: Int): E
  inline def apply(i: Int): E
  inline def py(i: Int): E = if i < 0 then apply(length - i) else apply(i)
  inline def R(i: Int): E = apply(i - 1)
  inline def useEveryone(inline f: E => Unit): Unit
  inline def useNonZero(inline f: (E, Int) => Unit): Unit
  inline def isZero(inline e: E): Boolean = inline e match
    case ei: Int        => ei == 0
    case ed: Double     => ed.nan
    case es: String     => (es eq null) || es.isEmpty
    case ev: Frame.Cell => ev.value eq null
}
object Stripe {
  import Frame.Cell
  type Datum = Int | Double | String | Cell
  type Data = Array[Int] | Array[Double] | Array[String] | Array[Cell]
  type Plural[E <: Datum] <: Data = E match
    case Int    => Array[Int]
    case Double => Array[Double]
    case String => Array[String]
    case Cell   => Array[Cell]
  type Element[D <: Data] <: Datum = D match
    case Array[Int]    => Int
    case Array[Double] => Double
    case Array[String] => String
    case Array[Cell]   => Cell

  inline def zero[E <: Datum] = summonFrom {
    case _: (E =:= Int)    => 0.asInstanceOf[E]
    case _: (E =:= Double) => Double.NaN.asInstanceOf[E]
    case _: (E =:= String) => "".asInstanceOf[E]
    case _: (E =:= Cell)   => Frame.NullCell.asInstanceOf[E]
  }
  sealed trait HasZero[E <: Datum] { def value: E }
  object HasZero {
    given HasZero[Int]    = new HasZero[Int]    { def value = zero[Int   ] }
    given HasZero[Double] = new HasZero[Double] { def value = zero[Double] }
    given HasZero[String] = new HasZero[String] { def value = zero[String] }
    given HasZero[Cell]   = new HasZero[Cell]   { def value = zero[Cell  ] }
  }


  final class Dense[E <: Datum](val data: Array[E]) extends Stripe[E] {
    val length = data.length
    def genericGet(i: Int) = data(i)
    inline def apply(i: Int): E = inline data match
      case ai: Array[Int]    => ai(i)
      case ad: Array[Double] => ad(i)
      case as: Array[String] => as(i)
      case ac: Array[Cell]   => ac(i)
    inline def useEveryone(inline f: E => Unit): Unit =
      var i = 0
      val n = length
      while i < n do
        f(apply(i))
        i += 1
    inline def useNonZero(inline f: (E, Int) => Unit): Unit =
      var i = 0
      val n = length
      while i < n do
        val x = apply(i)
        if !isZero(x) then f(x, i)
        i += 1
    inline def toSparse(using tag: ClassTag[E], hz: HasZero[E]): Sparse[E] =
      var n = 0
      useNonZero((_, _) => n += 1)
      val es = new Array[E](n)
      val is = new Array[Int](n)
      var j = 0
      useNonZero{ (e, i) => es(j) = e; is(j) = i; j += 1 }
      Sparse(es, is, length)
  }
  object Dense {
    transparent inline def build[E <: Datum] = summonFrom {
      case _: (E =:= Int)    => Build.DenseInt()
      case _: (E =:= Double) => Build.DenseDouble()
      case _: (E =:= String) => Build.DenseString()
      case _: (E =:= Cell)   => Build.DenseCell()
    }
  }

  final class Sparse[E <: Datum](val data: Array[E], val indices: Array[Int], val length: Int)(using HasZero[E]) extends Stripe[E] {
    private val genericZero: E = summon[HasZero[E]].value
    private def lookup(i: Int): Int =
      if length <= 0 || i < 0 then -1
      else if indices.length < 7 then
        var j = 0
        while j < indices.length do
          if indices(j) == i then return j
          j += 1 
        -1     
      else
        var j0 = 0
        val i0 = indices(j0)
        if i <= i0 then return (if i == i0 then j0 else -1)
        var j1 = indices.py.index(-1)
        val i1 = indices(j1)
        if i >= i1 then return (if i == i1 then j1 else -1)
        while j0 + 1 < j1 do
          val j = (j0 + j1) >>> 1
          val ix = indices(j)
          if i == ix then return j
          else if i > ix then j0 = j
          else j1 = j
        -1
      -1
    private inline def specific(j: Int): E = inline data match
      case ai: Array[Int]    => ai(j)
      case ad: Array[Double] => ad(j)
      case as: Array[String] => as(j)
      case ac: Array[Cell]   => ac(j)
    def genericGet(i: Int): E =
      val j = lookup(i)
      if j < 0 then genericZero
      else data(j)
    inline def apply(i: Int): E =
      val j = lookup(i)
      if j < 0 then zero[E]
      else specific(j)
    inline def useEveryone(inline f: E => Unit): Unit =
      var i = 0
      var j = 0
      val z = zero[E]
      val n = indices.length
      while j < n do
        val i0 = indices(j)
        while i < i0 do
          f(z)
          i += 1
        f(specific(j))
        j += 1
        i += 1
      while i < length do
        f(z)
        i += 1
    inline def useNonZero(inline f: (E, Int) => Unit): Unit =
      var j = 0
      val n = indices.length
      while j < n do
        val i = indices(j)
        f(specific(j), i)
        j += 1
    inline def toDense(using tag: ClassTag[E]): Dense[E] =
      val es = new Array[E](length)
      var j = 0
      useEveryone{ e => es(j) = e; j += 1 }
      Dense(es)
  }
  object Sparse {
    transparent inline def build[E <: Datum] = summonFrom {
      case _: (E =:= Int)    => Build.SparseInt()
      case _: (E =:= Double) => Build.SparseDouble()
      case _: (E =:= String) => Build.SparseString()
      case _: (E =:= Cell)   => Build.SparseCell()
    }
  }

  final class Stride[E <: Datum](val data: Array[E], val start: Int, val step: Int, val length: Int) extends Stripe[E] {
    def genericGet(i: Int): E = data(start + step*i)
    inline def apply(i: Int): E = inline data match
      case ai: Array[Int]    => ai(start + step*i)
      case ad: Array[Double] => ad(start + step*i)
      case as: Array[String] => as(start + step*i)
      case ac: Array[Cell]   => ac(start + step*i)
    inline def useEveryone(inline f: E => Unit): Unit =
      var i = 0
      while i < length do
        f(apply(i))
        i += 1
    inline def useNonZero(inline f: (E, Int) => Unit): Unit =
      var i = 0
      while i < length do
        val x = apply(i)
        if !isZero(x) then f(x, i)
        i += 1
    inline def toDense(using tag: ClassTag[E]): Dense[E] =
      val es = new Array[E](length)
      var j = 0
      useEveryone{ e => es(j) = e; j += 1 }
      Dense(es)
  }

  final class Splay[E <: Datum, S <: [F <: Datum] =>> Stripe[F]](val across: Array[S[E]], val index: Int) extends Stripe[E] {
    val length = across.length
    def genericGet(i: Int): E = across(i).genericGet(index)
    inline def apply(i: Int): E = across(i)(index)
    inline def useEveryone(inline f: E => Unit): Unit =
      var i = 0
      while i < across.length do
        f(across(i)(index))
        i += 1
    inline def useNonZero(inline f: (E, Int) => Unit): Unit =
      var i = 0
      while i < across.length do
        val x = across(i)(index)
        if !isZero(x) then f(x, i)
        i += 1
    inline def toDense(using tag: ClassTag[E]): Dense[E] =
      val es = new Array[E](across.length)
      var j = 0
      useEveryone{ e => es(j) = e; j += 1 }
      Dense(es)
  }
  
  final class Labels[E <: Datum, S <: [F <: Datum] =>> Stripe[F]](val underlying: S[E], val labels: S[String]) extends Stripe[E] {
    // Note: could also write S[F <: Data] <: Stripe[F] but the lambda form makes what's going on with F more obvious
    val length = underlying.length
    def genericGet(i: Int): E = underlying.genericGet(i)
    def genericLabel(i: Int): String = labels.genericGet(i)
    inline def apply(i: Int): E = underlying(i)
    inline def label(i: Int): String = labels(i).asInstanceOf[String]
    inline def useEveryone(inline f: E => Unit): Unit = underlying.useEveryone(f)
    inline def useNonZero(inline f: (E, Int) => Unit): Unit = underlying.useNonZero(f)
    inline def useEveryoneLabeled(inline f: (E, String) => Unit): Unit =
      var i = 0
      underlying.useEveryone{ e => f(e, labels(i)); i += 1 }
    inline def useNonZeroLabeled(inline f: (E, Int, String) => Unit): Unit =
      underlying.useNonZero{ (e, i) => f(e, i, labels(i)) }
  }

  object Build {
    sealed abstract class DenseBuild[E <: Datum]() {
      def append(e: E): Unit
      def result: Dense[E]
      def clear(): Unit
    }
    final class DenseInt() extends DenseBuild[Int]() {
      private var buffer: Array[Int] = new Array[Int](8)
      private var n = 0
      def append(x: Int): Unit =
        if n >= buffer.length then buffer = buffer.copyToSize(0x7FFFFFF8 & (buffer.length | (buffer.length << 1)))
        buffer(n) = x
        n += 1
      def result: Dense[Int] = Dense(buffer.shrinkCopy(n))
      def clear(): Unit = { buffer = new Array[Int](4); n = 0 }
    }
    final class DenseDouble() extends DenseBuild[Double]() {
      private var buffer: Array[Double] = new Array[Double](8)
      private var n = 0
      def append(x: Double): Unit =
        if n >= buffer.length then buffer = buffer.copyToSize(0x7FFFFFF8 & (buffer.length | (buffer.length << 1)))
        buffer(n) = x
        n += 1
      def result: Dense[Double] = Dense(buffer.shrinkCopy(n))
      def clear(): Unit = { buffer = new Array[Double](8); n = 0 }
    }
    final class DenseString() extends DenseBuild[String]() {
      private var buffer: Array[String] = new Array[String](8)
      private var n = 0
      def append(x: String): Unit =
        if n >= buffer.length then buffer = buffer.copyToSize(0x7FFFFFF8 & (buffer.length | (buffer.length << 1)))
        buffer(n) = x
        n += 1
      def result: Dense[String] = Dense(buffer.shrinkCopy(n))
      def clear(): Unit = { buffer = new Array[String](8); n = 0 }
    }
    final class DenseCell() extends DenseBuild[Cell]() {
      private var buffer: Array[Cell] = new Array[Cell](8)
      private var n = 0
      def append(x: Cell): Unit =
        if n >= buffer.length then buffer = buffer.copyToSize(0x7FFFFFF8 & (buffer.length | (buffer.length << 1)))
        buffer(n) = x
        n += 1
      def result: Dense[Cell] = Dense(buffer.shrinkCopy(n))
      def clear(): Unit = { buffer = new Array[Cell](8); n = 0 }
    }

    sealed abstract class SparseBuild[E <: Datum]() {
      protected val buffie = new scala.collection.mutable.TreeMap[Int, E]()
      final def update(i: Int, e: E): Unit = buffie(i) = e
      def result(max: Int): Sparse[E]
      final def clear(): Unit = buffie.clear
    }
    final class SparseInt() extends SparseBuild[Int]() {
      def result(max: Int): Sparse[Int] =
        val es = new Array[Int](buffie.size)
        val is = new Array[Int](buffie.size)
        iFor(buffie.iterator) { (kv, j) =>
          es(j) = kv._2
          val i = kv._1
          if i < 0 then throw new IllegalArgumentException(s"Negative index $i")
          is(j) = i
        }
        var n = if max > 0 then max else 0
        if is.length > 0 then
          val m = is.py(-1)
          if m == Int.MaxValue then throw new IllegalArgumentException(s"Over-large index $m")
          if m >= n then n = m + 1
        Sparse(es, is, n)
    }
    final class SparseDouble() extends SparseBuild[Double]() {
      def result(max: Int): Sparse[Double] =
        val es = new Array[Double](buffie.size)
        val is = new Array[Int](buffie.size)
        iFor(buffie.iterator) { (kv, j) =>
          es(j) = kv._2
          val i = kv._1
          if i < 0 then throw new IllegalArgumentException(s"Negative index $i")
          is(j) = i
        }
        var n = if max > 0 then max else 0
        if is.length > 0 then
          val m = is.py(-1)
          if m == Int.MaxValue then throw new IllegalArgumentException(s"Over-large index $m")
          if m >= n then n = m + 1
        Sparse(es, is, n)
    }
    final class SparseString() extends SparseBuild[String]() {
      def result(max: Int): Sparse[String] =
        val es = new Array[String](buffie.size)
        val is = new Array[Int](buffie.size)
        iFor(buffie.iterator) { (kv, j) =>
          es(j) = kv._2
          val i = kv._1
          if i < 0 then throw new IllegalArgumentException(s"Negative index $i")
          is(j) = i
        }
        var n = if max > 0 then max else 0
        if is.length > 0 then
          val m = is.py(-1)
          if m == Int.MaxValue then throw new IllegalArgumentException(s"Over-large index $m")
          if m >= n then n = m + 1
        Sparse(es, is, n)
    }
    final class SparseCell() extends SparseBuild[Cell]() {
      def result(max: Int): Sparse[Cell] =
        val es = new Array[Cell](buffie.size)
        val is = new Array[Int](buffie.size)
        iFor(buffie.iterator) { (kv, j) =>
          es(j) = kv._2
          val i = kv._1
          if i < 0 then throw new IllegalArgumentException(s"Negative index $i")
          is(j) = i
        }
        var n = if max > 0 then max else 0
        if is.length > 0 then
          val m = is.py(-1)
          if m == Int.MaxValue then throw new IllegalArgumentException(s"Over-large index $m")
          if m >= n then n = m + 1
        Sparse(es, is, n)
    }
  }

  inline def get[E <: Datum, S <: Stripe[E]](s: S, i: Int): E = inline s match
    case dense:  Dense[E]     => dense(i)
    case sparse: Sparse[E]    => sparse(i)
    case stride: Stride[E]    => stride(i)
    case splay:  Splay[E, _]  => splay(i)
    case label:  Labels[E, _] => label(i)
}

object Frame {
  abstract class Cell() {
    type Value >: Null <: AnyRef
    val value: Value
    final def get: Value Or Unit =
      if value eq null then Alt.unit else Is(value)
    def console(width: Int): (String, Int)
    override def toString = console(79)._1
    final override def equals(a: Any) = a match
      case c: Cell => value == c.value
      case _ => false
    final override def hashCode = value.##
  }
  object NullCell extends Cell() {
    type Value = Null
    val value = null
    def console(width: Int): (String, Int) =
      if width >= 6 then ("(null)", 0)
      else ("_", 0)
  }
}
