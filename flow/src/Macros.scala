// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

package kse.flow.flowMacroImpl

import scala.quoted.*

def inclusiveRangePackedInLongExpr(range: Expr[Any])(using Quotes): Expr[Long] =
  range match
    case '{ ($a: Int) to ($b: Int) } => '{ ($a & 0xFFFFFFFFL) | (($b).toLong << 32) }
    case _ => quotes.reflect.report.errorAndAbort("Can only accept literal range of the form x to y")

inline def inclusiveRangePackedInLong(inline range: scala.collection.immutable.Range): Long =
  ${ inclusiveRangePackedInLongExpr('range) }
