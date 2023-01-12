// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2014-15, 2021-23 Rex Kerr, UCSF, and Calico Life Sciences LLC.

package kse.flow


/** C-style for loop: starting with an Int `zero`, until meeting a stopping condition `p`, advance with `next` and operate with `f`. */
inline def cFor(zero: Int)(inline p: Int => Boolean)(inline next: Int => Int)(inline f: Int => Unit): Unit =
  var x = zero
  while p(x) do
    f(x)
    x = next(x)

/** C-style for loop: starting with a Long `zero`, until meeting a stopping condition `p`, advance with `next` and operate with `f`. */
inline def cFor(zero: Long)(inline p: Long => Boolean)(inline next: Long => Long)(inline f: Long => Unit): Unit =
  var x = zero
  while p(x) do
    f(x)
    x = next(x)

/** C-style for loop: starting with an arbitrary `zero`, until meeting a stopping condition `p`, advance with `next` and operate with `f`. */
inline def cFor[A](zero: A)(inline p: A => Boolean)(inline next: A => A)(inline f: A => Unit): Unit =
  var x = zero
  while p(x) do
    f(x)
    x = next(x)

/** Repeat: run `f` on arguments up to the Int `n` */
inline def nFor(n: Int)(inline f: Int => Unit): Unit =
  var i = 0
  while i < n do
    f(i)
    i += 1

/** Repeat: run `f` on arguments up to the Long `n` */
inline def nFor(n: Long)(inline f: Long => Unit): Unit =
  var i = 0L
  while i < n do
    f(i)
    i += 1

/** For every item in array of bytes `a`, apply `f` to the item plus its index */
inline def aFor(a: Array[Byte])(inline f: (Byte, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

/** For every item in array of chars `a`, apply `f` to the item plus its index */
inline def aFor(a: Array[Char])(inline f: (Char, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

/** For every character in string `s`, apply `f` to the item plus its index */
inline def aFor(s: java.lang.CharSequence)(inline f: (Char, Int) => Unit): Unit =
  var i = 0
  while i < s.length do
    f(s charAt i, i)
    i += 1

/** For every item in array of shots `a`, apply `f` to the item plus its index */
inline def aFor(a: Array[Short])(inline f: (Short, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

/** For every item in array of ints `a`, apply `f` to the item plus its index */
inline def aFor(a: Array[Int])(inline f: (Int, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

/** For every item in array of longs `a`, apply `f` to the item plus its index */
inline def aFor(a: Array[Long])(inline f: (Long, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

/** For every item in array of floats `a`, apply `f` to the item plus its index */
inline def aFor(a: Array[Float])(inline f: (Float, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

/** For every item in array of doubles `a`, apply `f` to the item plus its index */
inline def aFor(a: Array[Double])(inline f: (Double, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

/** For every item in arbitrary array `a`, apply `f` to the item plus its index */
inline def aFor[A](a: Array[A])(inline f: (A, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

/** For everything in iterator `i`, run `f` on the item plus its index */
inline def iFor[A](i: Iterator[A])(inline f: (A, Int) => Unit): Unit =
  var n = 0
  while i.hasNext do
    f(i.next, n)
    n += 1
