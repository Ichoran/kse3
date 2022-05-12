package kse.flow

inline def cFor(zero: Int)(inline p: Int => Boolean)(inline next: Int => Int)(inline f: Int => Unit): Unit =
  var x = zero
  while p(x) do
    f(x)
    x = next(x)

inline def cFor(zero: Long)(inline p: Long => Boolean)(inline next: Long => Long)(inline f: Long => Unit): Unit =
  var x = zero
  while p(x) do
    f(x)
    x = next(x)

inline def cFor[A](zero: A)(inline p: A => Boolean)(inline next: A => A)(inline f: A => Unit): Unit =
  var x = zero
  while p(x) do
    f(x)
    x = next(x)

inline def nFor(n: Int)(inline f: Int => Unit): Unit =
  var i = 0
  while i < n do
    f(i)
    i += 1

inline def nFor(n: Long)(inline f: Long => Unit): Unit =
  var i = 0L
  while i < n do
    f(i)
    i += 1

inline def aFor(a: Array[Byte])(inline f: (Byte, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

inline def aFor(a: Array[Char])(inline f: (Char, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

inline def aFor(s: java.lang.CharSequence)(inline f: (Char, Int) => Unit): Unit =
  var i = 0
  while i < s.length do
    f(s charAt i, i)
    i += 1

inline def aFor(a: Array[Short])(inline f: (Short, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

inline def aFor(a: Array[Int])(inline f: (Int, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

inline def aFor(a: Array[Long])(inline f: (Long, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

inline def aFor(a: Array[Float])(inline f: (Float, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

inline def aFor(a: Array[Double])(inline f: (Double, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

inline def aFor[A](a: Array[A])(inline f: (A, Int) => Unit): Unit =
  var i = 0
  while i < a.length do
    f(a(i), i)
    i += 1

inline def iFor[A](i: Iterator[A])(inline f: A => Unit): Unit =
  while i.hasNext do f(i.next)

inline def ixFor[A](i: Iterator[A])(inline f: (A, Int) => Unit): Unit =
  var n = 0
  while i.hasNext do
    f(i.next, n)
    n += 1
