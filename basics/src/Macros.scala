// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

package kse.basics.intervalMacroImpl

import scala.quoted.*

def packRangeInLongInclusive(i0: Int, i1: Int): Long =
  if i0 < 0 then throw new IllegalArgumentException(s"Interval start index must be non-negative, not $i0")
  if i1 == Int.MaxValue then (i0 & 0xFFFFFFFFL) | 0x7FFFFFFF00000000L
  else (i0 & 0xFFFFFFFFL) | ((i1+1).toLong << 32)

def packRangeInLongExclusive(i0: Int, iN: Int): Long =
  if i0 < 0 then throw new IllegalArgumentException(s"Interval start index must be non-negative, not $i0")
  (i0 & 0xFFFFFFFFL) | (iN.toLong << 32)

def rangePackedInLongExpr(range: Expr[Any])(using qt: Quotes): Expr[Long] =
  import qt.reflect._
  range match
    case '{ ($a: Int) to ($b: Int) }    => '{ packRangeInLongInclusive($a, $b) }
    case '{ ($a: Int) until ($b: Int) } => '{ packRangeInLongExclusive($a, $b) }
    case _ => report.errorAndAbort("Iv-interval literal must be `x to y` or `x until z`")

inline def rangePackedInLong(inline range: scala.collection.immutable.Range): Long =
  ${ rangePackedInLongExpr('range) }
