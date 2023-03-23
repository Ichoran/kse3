// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

/*
This file "shouldn't" exist.

Because Scala 3 doesn't have first-class method overloading, and because extension methods
are just bare method names, not parameterized by the type of the thing you're extending,
you have to define all "overloaded" extensions in the same package in the same file.

Annoying, but at least it's possible.
*/

package kse.flow

import scala.annotation.targetName

extension [X](is: Is[X])
  /** use is trivial--just apply function and return this Is */
  inline def use(inline f: X => Unit): is.type =
    f(Is unwrap is); is

extension [X, Y](or: Or[X, Y])
  /** Operate on the favored value if it exists, but pass on the original `Or`. */
  inline def use(inline f: X => Unit): or.type =
    (or: X Or Y) match
      case _: Alt[_] =>
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])
    or

extension [A](option: Option[A])
  /** Do something with a stored value, if present, and keep passing along the Option either way */
  inline def use(inline f: A => Unit): option.type =
    (option: Option[A]) match
      case Some(a) => f(a)
      case _       =>
    option

extension (ab: Array[Byte])
  inline def use(i: Int)(inline f: Byte => Unit): ab.type = { f(ab(i)); ab }
  inline def zap(i: Int)(inline f: Byte => Byte): ab.type = { ab(i) = f(ab(i)); ab }

extension (as: Array[Short])
  inline def use(i: Int)(inline f: Short => Unit): as.type = { f(as(i)); as }
  inline def zap(i: Int)(inline f: Short => Short): as.type = { as(i) = f(as(i)); as }

extension (ac: Array[Char])
  inline def use(i: Int)(inline f: Char => Unit): ac.type = { f(ac(i)); ac }
  inline def zap(i: Int)(inline f: Char => Char): ac.type = { ac(i) = f(ac(i)); ac }

extension (ai: Array[Int])
  inline def use(i: Int)(inline f: Int => Unit): ai.type = { f(ai(i)); ai }
  inline def zap(i: Int)(inline f: Int => Int): ai.type = { ai(i) = f(ai(i)); ai }

extension (al: Array[Long])
  inline def use(i: Int)(inline f: Long => Unit): al.type = { f(al(i)); al }
  inline def zap(i: Int)(inline f: Long => Long): al.type = { al(i) = f(al(i)); al }

extension (af: Array[Float])
  inline def use(i: Int)(inline f: Float => Unit): af.type = { f(af(i)); af }
  inline def zap(i: Int)(inline f: Float => Float): af.type = { af(i) = f(af(i)); af }

extension (ad: Array[Double])
  inline def use(i: Int)(inline f: Double => Unit): ad.type = { f(ad(i)); ad }
  inline def zap(i: Int)(inline f: Double => Double): ad.type = { ad(i) = f(ad(i)); ad }

extension [A >: Null <: AnyRef](aa: Array[A])
  inline def use(i: Int)(inline f: A => Unit): aa.type = { f(aa(i)); aa }
  inline def zap(i: Int)(inline f: A => A): aa.type = { aa(i) = f(aa(i)); aa }
