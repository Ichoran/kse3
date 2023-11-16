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

/** For everything in iterator `i`, run `f` on the item plus its index */
inline def iFor[A](i: Iterator[A])(inline f: (A, Int) => Unit): Unit =
  var n = 0
  while i.hasNext do
    f(i.next, n)
    n += 1

/** For everything in stepper `s`, run `f` on the item plus its index */
inline def iFor[A](s: scala.collection.Stepper[A])(inline f: (A, Int) => Unit): Unit =
  var n = 0
  while s.hasStep do
    f(s.nextStep, n)
    n += 1

/** For everything in java Iterator `j`, run `f` on the item plus its index */
inline def iFor[A](j: java.util.Iterator[A])(inline f: (A, Int) => Unit): Unit =
  var n = 0
  while j.hasNext do
    f(j.next, n)
    n += 1

/** For everything in a java Enumeration `e`, run `f` on the item plus its index */
inline def iFor[A](e: java.util.Enumeration[A])(inline f: (A, Int) => Unit): Unit =
  var n = 0
  while e.hasMoreElements do
    f(e.nextElement, n)
    n += 1

/** For everything in a java Spliterator `s`, run `f` on the item plus its index */
inline def iFor[A](s: java.util.Spliterator[A])(inline f: (A, Int) => Unit): Unit =
  var n = 0
  while s.tryAdvance(a => f(a, n)) do n += 1
