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


import scala.language.experimental.relaxedExtensionImports


extension [X](is: Is[X])
  /** use is trivial--just apply function and return this Is */
  inline def use(inline f: X => Unit): is.type =
    f(Is unwrap is); is

extension [X, Y](or: Or[X, Y])
  /** Operate on the favored value if it exists, but pass on the original `Or`. */
  inline def use(inline f: X => Unit): or.type =
    (or: X Or Y) match
      case _: Alt[?] =>
      case _ => f(Is unwrap or.asInstanceOf[Is[X]])
    or

extension [A](option: Option[A])
  /** Do something with a stored value, if present, and keep passing along the Option either way */
  inline def use(inline f: A => Unit): option.type =
    (option: Option[A]) match
      case Some(a) => f(a)
      case _       =>
    option
