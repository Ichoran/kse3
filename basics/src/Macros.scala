// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

package kse.basics.intervalMacroImpl

import scala.quoted.*

def deinliner(expr: Expr[Any])(using qt: Quotes): Expr[Any] =
  import qt.reflect._
  expr.asTerm match
    case Inlined(None, Nil, e)  => deinliner(e.asExprOf[Any])
    case Inlined(Some(e), _, _) => e.asExprOf[Any]
    case e                      => e.asExprOf[Any]

def packRangeInLongInclusive(i0: Int, i1: Int): Long =
  if i1 == Int.MaxValue then (i0 & 0xFFFFFFFFL) | 0x7FFFFFFF00000000L
  else (i0 & 0xFFFFFFFFL) | ((i1+1).toLong << 32)

def packRangeInLongExclusive(i0: Int, iN: Int): Long =
  (i0 & 0xFFFFFFFFL) | (iN.toLong << 32)

def rangePackedInLongExpr(range: Expr[Any])(using qt: Quotes): Expr[Long] =
  import qt.reflect._
  range match
    case '{ ($a: Int) to ($b: Int) }    => '{ packRangeInLongInclusive($a, $b) }
    case '{ ($a: Int) until ($b: Int) } => '{ packRangeInLongExclusive($a, $b) }
    case _ =>
      // The above won't work when there are multiple conflicting inlined extensions of to/until
      // Not sure if the below will work when it's not inlined, but this seems to do the trick when it is.
      deinliner(range).asTerm match
        case Apply(Apply(Ident("to"), ea :: Nil), eb :: Nil) =>
          (ea.asExprOf[Any], eb.asExprOf[Any]) match
            case ('{$a: Int}, '{$b: Int}) => '{ packRangeInLongInclusive($a, $b) }
            case _ => report.errorAndAbort("Iv-interval literal `x to y` must have Int bounds")
        case Apply(Apply(Ident("until"), ea :: Nil), eb :: Nil) =>
          (ea.asExprOf[Any], eb.asExprOf[Any]) match
            case ('{$a: Int}, '{$b: Int}) => '{ packRangeInLongExclusive($a, $b) }
            case _ => report.errorAndAbort("Iv-interval literal `x until y` must have Int bounds")
        case _ => report.errorAndAbort("Iv-interval literal must be `x to y` or `x until z`")

inline def rangePackedInLong(inline range: scala.collection.immutable.Range): Long =
  ${ rangePackedInLongExpr('range) }
