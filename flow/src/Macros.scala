// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.flow.flowMacroImpl

// This file may not depend on anything else in kse.flow, so that it can be compiled first
// and its macros used elsewhere in the module.

import scala.quoted.*


/** True unless a value whose static type is `X` provably cannot be a runtime instance of the
  * class underlying `P`.  `P` should be a concrete class type; its type arguments are ignored.
  *
  * The test is by base classes: whatever `X`'s abstract parts are later instantiated to, a value
  * of type `X` conforms to every base class of `X`, and `baseClasses` resolves abstract types,
  * opaque types, and inliner-introduced proxies through their upper bounds rather than depending
  * on the shape of any tree.  So if `P`'s class fails to extend even one of them, no `P` instance
  * can inhabit `X`.  Unions are checked branchwise, since a value only need conform to one branch;
  * intersections must admit `P` on every branch.  Anything unrecognized answers `true`: a needless
  * `true` costs the caller a runtime test, while a wrong `false` would let representations lie.
  */
transparent inline def canBeInstanceOf[P, X]: Boolean = ${ canBeInstanceOfImpl[P, X] }

def canBeInstanceOfImpl[P: Type, X: Type](using Quotes): Expr[Boolean] =
  import quotes.reflect.*
  val p = TypeRepr.of[P]
  def can(t: TypeRepr, depth: Int): Boolean =
    depth > 64 || (t.dealias match
      case OrType(a, b)        => can(a, depth+1) || can(b, depth+1)
      case AndType(a, b)       => can(a, depth+1) && can(b, depth+1)
      case AnnotatedType(u, _) => can(u, depth+1)
      case tb: TypeBounds      => can(tb.hi, depth+1)
      case u                   => u.baseClasses.forall(bc => p.derivesFrom(bc)))
  val answer =
    try can(TypeRepr.of[X], 0)
    catch case e if scala.util.control.NonFatal(e) => true
  Expr(answer)
