// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences, LLC.

package kse.data


import java.lang.{Math => jm}

import scala.compiletime.erasedValue
import scala.reflect.ClassTag

import kse.flow._
import kse.maths._

sealed trait Stripe[D <: Stripe.Data] {
  inline def length: Int
  inline def apply(i: Int): Stripe.Element[D]
  inline def py(i: Int): Stripe.Element[D] = if i < 0 then apply(length - i) else apply(i)
  inline def R(i: Int): Stripe.Element[D] = apply(i - 1)
  inline def useEveryone(inline f: Stripe.Element[D] => Unit): Unit
  inline def useNonZero(inline f: (Stripe.Element[D], Int) => Unit): Unit
  inline def isZero(inline e: Stripe.Element[D]): Boolean = inline e match
    case ei: Int        => ei == 0
    case ed: Double     => ed.nan
    case es: String     => (es eq null) || es.isEmpty
    case ev: Frame.Cell => ev.value eq null    
  inline def zero: Stripe.Element[D] = inline erasedValue[D] match
    case _: Array[Int]        => 0
    case _: Array[Double]     => Double.NaN
    case _: Array[String]     => ""
    case _: Array[Frame.Cell] => Frame.NullCell
}
object Stripe {
  import Frame.Cell
  type Datum = Int | Double | String | Cell
  type Data = Array[Int] | Array[Double] | Array[String] | Array[Cell]
  type Plural[D <: Datum] <: Data = D match
    case Int    => Array[Int]
    case Double => Array[Double]
    case String => Array[String]
    case Cell   => Array[Cell]
  type Element[D <: Data] <: Datum = D match
    case Array[Int]    => Int
    case Array[Double] => Double
    case Array[String] => String
    case Array[Cell]   => Cell

  final class Dense[D <: Data](val data: D) extends Stripe[D] {
    inline def length = data.length
    inline def apply(i: Int): Element[D] = inline data match
      case ai: Array[Int]    => ai(i)
      case ad: Array[Double] => ad(i)
      case as: Array[String] => as(i)
      case ac: Array[Cell]   => ac(i)
    inline def useEveryone(inline f: Element[D] => Unit): Unit =
      var i = 0
      while i < data.length do
        f(apply(i))
        i += 1
    inline def useNonZero(inline f: (Element[D], Int) => Unit): Unit =
      var i = 0
      while i < data.length do
        val x = apply(i)
        if !isZero(x) then f(x, i)
        i += 1
  }
  object Dense {
    transparent inline def build[E <: Datum] = inline erasedValue[E] match
      case _: Int    => Build.DenseInt()
      case _: Double => Build.DenseDouble()
      case _: String => Build.DenseString()
      case _: Cell   => Build.DenseCell()
  }

  final class Sparse[D <: Data](val data: D, val indices: Array[Int], val size: Int) extends Stripe[D] {
    inline def length = size
    private def lookup(i: Int): Int =
      if size <= 0 || i < 0 then -1
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
    private inline def specific(j: Int): Element[D] = inline data match
      case ai: Array[Int]    => ai(j)
      case ad: Array[Double] => ad(j)
      case as: Array[String] => as(j)
      case ac: Array[Cell]   => ac(j)

    inline def apply(i: Int): Element[D] =
      val j = lookup(i)
      if j < 0 then zero
      else specific(j)
    inline def useEveryone(inline f: Element[D] => Unit): Unit =
      var i = 0
      var j = 0
      val z = zero
      while j < data.length do
        val i0 = indices(j)
        while i < i0 do
          f(z)
          i += 1
        f(specific(j))
        j += 1
        i += 1
      while i < size do
        f(z)
        i += 1
    inline def useNonZero(inline f: (Element[D], Int) => Unit): Unit =
      var j = 0
      while j < data.length do
        val i = indices(j)
        f(specific(j), i)
        j += 1
  }

  final class Stride[D <: Data](val data: D, val start: Int, val step: Int, val size: Int) extends Stripe[D] {
    inline def length = size
    inline def apply(i: Int): Element[D] = inline data match
      case ai: Array[Int]    => ai(start + step*i)
      case ad: Array[Double] => ad(start + step*i)
      case as: Array[String] => as(start + step*i)
      case ac: Array[Cell]   => ac(start + step*i)
    inline def useEveryone(inline f: Element[D] => Unit): Unit =
      var i = 0
      while i < size do
        f(apply(i))
        i += 1
    inline def useNonZero(inline f: (Element[D], Int) => Unit): Unit =
      var i = 0
      while i < size do
        val x = apply(i)
        if !isZero(x) then f(x, i)
        i += 1
  }

  final class Labels[D <: Data, S <: [Q <: Data] =>> Stripe[Q]](val underlying: S[D], val labels: S[Array[String]]) extends Stripe[D] {
    // Note: could also write S[Q <: Data] <: Stripe[Q] but the lambda form makes what's going on with Q more obvious
    inline def length = underlying.length
    inline def apply(i: Int): Element[D] = underlying(i)
    inline def label(i: Int): String = labels(i).asInstanceOf[String]
    inline def useEveryone(inline f: Element[D] => Unit): Unit = underlying.useEveryone(f)
    inline def useNonZero(inline f: (Element[D], Int) => Unit): Unit = underlying.useNonZero(f)
    inline def useEveryoneLabeled(inline f: (Element[D], String) => Unit): Unit =
      var i = 0
      underlying.useEveryone{ e => f(e, labels(i)); i += 1 }
    inline def useNonZeroLabeled(inline f: (Element[D], Int, String) => Unit): Unit =
      underlying.useNonZero{ (e, i) => f(e, i, labels(i)) }
  }

  object Build {
    abstract class DenseBuild[D <: Data]() {
      def append(e: Element[D]): Unit
      def result: Dense[D]
      def clear(): Unit
    }
    final class DenseInt() extends DenseBuild[Array[Int]]() {
      private var buffer: Array[Int] = new Array[Int](8)
      private var n = 0
      def append(x: Int): Unit =
        if n >= buffer.length then buffer = buffer.copyToSize(0x7FFFFFF8 & (buffer.length | (buffer.length << 1)))
        buffer(n) = x
        n += 1
      def result: Dense[Array[Int]] = Dense(buffer.shrinkCopy(n))
      def clear(): Unit = { buffer = new Array[Int](4); n = 0 }
    }
    final class DenseDouble() extends DenseBuild[Array[Double]]() {
      private var buffer: Array[Double] = new Array[Double](8)
      private var n = 0
      def append(x: Double): Unit =
        if n >= buffer.length then buffer = buffer.copyToSize(0x7FFFFFF8 & (buffer.length | (buffer.length << 1)))
        buffer(n) = x
        n += 1
      def result: Dense[Array[Double]] = Dense(buffer.shrinkCopy(n))
      def clear(): Unit = { buffer = new Array[Double](8); n = 0 }
    }
    final class DenseString() extends DenseBuild[Array[String]]() {
      private var buffer: Array[String] = new Array[String](8)
      private var n = 0
      def append(x: String): Unit =
        if n >= buffer.length then buffer = buffer.copyToSize(0x7FFFFFF8 & (buffer.length | (buffer.length << 1)))
        buffer(n) = x
        n += 1
      def result: Dense[Array[String]] = Dense(buffer.shrinkCopy(n))
      def clear(): Unit = { buffer = new Array[String](8); n = 0 }
    }
    final class DenseCell() extends DenseBuild[Array[Cell]]() {
      private var buffer: Array[Cell] = new Array[Cell](8)
      private var n = 0
      def append(x: Cell): Unit =
        if n >= buffer.length then buffer = buffer.copyToSize(0x7FFFFFF8 & (buffer.length | (buffer.length << 1)))
        buffer(n) = x
        n += 1
      def result: Dense[Array[Cell]] = Dense(buffer.shrinkCopy(n))
      def clear(): Unit = { buffer = new Array[Cell](8); n = 0 }
    }
  }

  inline def get[D <: Data, S <: Stripe[D]](s: S, i: Int): Element[D] = inline s match
    case dense:  Dense[D]     => dense(i)
    case sparse: Sparse[D]    => sparse(i)
    case stride: Stride[D]    => stride(i)
    case label:  Labels[D, _] => label(i)
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
