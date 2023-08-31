// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

package kse.flow.flowMacroImpl

import scala.quoted.*

inline def packRangeInLongInclusive(i0: Int, i1: Int): Long =
  (i0 & 0xFFFFFFFFL) | (i1.toLong << 32)

inline def packRangeInLongExclusive(i0: Int, iN: Int): Long =
  if iN > Int.MinValue then (i0 & 0xFFFFFFFFL) | ((iN - 1).toLong << 32)
  else (i0 & 0xFFFFFFFFL) | 0x8000000000000000L

def rangePackedInLongExpr(range: Expr[Any])(using qt: Quotes): Expr[Long] =
  import qt.reflect._
  range match
    case '{ ($a: Int) to ($b: Int) }    => '{ packRangeInLongInclusive($a, $b) }
    case '{ ($a: Int) until ($b: Int) } => '{ packRangeInLongExclusive($a, $b) }
    case _ => report.errorAndAbort("Iv-interval literal must be `x to y` or `x until z`")

inline def rangePackedInLong(inline range: scala.collection.immutable.Range): Long =
  ${ rangePackedInLongExpr('range) }
