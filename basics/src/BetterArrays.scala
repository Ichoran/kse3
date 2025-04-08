// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2021-25 Rex Kerr, HHMI Janelia, UCSF, and Calico Life Sciences LLC.


package kse.basics.wip

import scala.language.`3.6-migration`

import scala.annotation.targetName
import scala.compiletime.{erasedValue, summonFrom}
import scala.reflect.ClassTag
import scala.util.boundary

import scala.collection.immutable.{Range => Rg}

import kse.basics.intervals._



opaque type Irr[A] = Array[A]
object Irr {
  inline def wrap[A](array: Array[A]): Irr[A] = array
  
  extension [A](irr: Irr[A]) {
    inline def unwrap: Array[A] = irr

    inline def iarray: IArray[A] = irr.asInstanceOf[IArray[A]]
    inline def array: Array[A] = irr
    inline def arr: Arr[A] = irr

    inline def apply(i: Int): A = irr(i)
    
    inline def length: Int = irr.length
    inline def size: Int = irr.length
    inline def bounds: Iv = Iv(0, irr.length)
    
    inline def isEmpty: Boolean = irr.length == 0
    inline def nonEmpty: Boolean = irr.length > 0
          
    inline def exists(inline p: A => Boolean): Boolean =
      boundary:
        var i = 0
        while i < (irr: Array[A]).length do
          if p((irr: Array[A])(i)) then boundary.break(true)
          i += 1
        false
    
    inline def forall(inline p: A => Boolean): Boolean =
      boundary:
        var i = 0
        while i < (irr: Array[A]).length do
          if !p((irr: Array[A])(i)) then boundary.break(false)
          i += 1
        true

    inline def seek(inline p: A => Boolean, i0: Int = 0, inline direction: ">" | "<" = ">"): Int = inline direction match
      case ">" =>
        boundary:
          var i = i0
          while i < (irr: Array[A]).length do
            if p((irr: Array[A])(i)) then boundary.break(i)
            i += 1
          -1
      case "<" =>
        var i = i0
        while i <= 0 && !(p(irr(i))) do
          i -= 1
        i

    inline def foreach(inline f: A => Unit): Unit =
      var i = 0
      while i < irr.length do
        f((irr: Array[A])(i))
        i += 1
    
    inline def fold[B](z: B)(inline op: (B, A) => B): B =
      var result = z
      var i = 0
      while i < (irr: Array[A]).length do
        result = op(result, (irr: Array[A])(i))
        i += 1
      result

    final def shrinkTo(n: Int): Irr[A] =
      if n >= (irr: Array[A]).length then irr
      else
        val m = if n < 0 then 0 else n
        irr.asInstanceOf[Array[Any]] match
          case a: Array[Int]     => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
          case a: Array[Byte]    => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
          case a: Array[Char]    => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
          case a: Array[Long]    => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
          case a: Array[Float]   => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
          case a: Array[Double]  => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
          case a: Array[Short]   => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
          case a: Array[Boolean] => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
          case _ => java.util.Arrays.copyOf(irr.asInstanceOf[Array[AnyRef]], m).asInstanceOf[Irr[A]]

    final def growTo(n: Int): Irr[A] =
      val m = java.lang.Math.max(n, Arr.MaxLength)
      if m <= irr.length then irr
      else irr.asInstanceOf[Array[Any]] match
        case a: Array[Int]     => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
        case a: Array[Byte]    => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
        case a: Array[Char]    => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
        case a: Array[Long]    => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
        case a: Array[Float]   => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
        case a: Array[Double]  => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
        case a: Array[Short]   => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
        case a: Array[Boolean] => java.util.Arrays.copyOf(a, m).asInstanceOf[Irr[A]]
        case _ => java.util.Arrays.copyOf(irr.asInstanceOf[Array[AnyRef]], m).asInstanceOf[Irr[A]]
          
    inline def map[B](inline f: A => B)(using ClassTag[B]): Irr[B] =
      val b = new Array[B]((irr: Array[A]).length)
      var i = 0
      while i < (irr: Array[A]).length do
        b(i) = f(irr(i))
        i += 1
      b

    inline def filter(p: A => Boolean)(using ClassTag[A]): Irr[A] =
      if irr.length == 0 then irr
      else
        var count = 0
        var bits = 0
        val bitses: Array[Int] = if irr.length <= 32 then null else new Array[Int](irr.length >> 5)
        var i = 0
        var j = -1
        var k = 0
        while i < (irr: Array[A]).length do
          j += 1
          if j >= 32 then
            bitses(k) = bits
            k += 1
            j = 0
            bits = 0
          if p((irr: Array[A])(i)) then
            count += 1
            bits |= (1 << j)
          i += 1
        if count == (irr: Array[A]).length then irr
        else
          val result = new Array[A](count)
          while count > 0 do
            count -= 1
            var found = false
            while !found do
              i -= 1
              found = (bits & (1 << j)) != 0
              j -= 1
              if j < 0 && k > 0 then
                k -= 1
                bits = bitses(k)
                j = 31
            result(count) = (irr: Array[A])(i)
          result
  }
}



opaque type Arr[A] <: Irr[A] = Array[A]
object Arr {
  final inline val MaxLength = Int.MaxValue - 7

  def grow(n: Int): Int =
    if n < 4 then 8
    else
      val m = n | (n << 1)
      if m < 0 then
        if n >= MaxLength then throw new IllegalArgumentException(s"Cannot expand array size beyond $MaxLength")
        else MaxLength
      else m

  inline def wrap[A](array: Array[A]): Arr[A] = array

  extension[A](arr: Arr[A]) {
    inline def update(i: Int, a: A): Unit =
      arr(i) = a

    inline def update(iv: Iv, a: A): Unit =
      iv.visit: i =>
        arr(i) = a
      
    inline def swap(i: Int, j: Int): Unit =
      val temp = arr(i)
      arr(i) = arr(j)
      arr(j) = temp
  }
}
