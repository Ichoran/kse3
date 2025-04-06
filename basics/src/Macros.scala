// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023 Rex Kerr and Calico Life Sciences LLC.

package kse.basics.basicsMacroImpl

import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.quoted.*

def deinliner(expr: Expr[Any])(using qt: Quotes): Expr[Any] =
  import qt.reflect.*
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
  import qt.reflect.*
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

def applyWithoutBoxingExpr2[A: Type, B: Type, Z: Type](a: Expr[A], e: Expr[((A, B) => Z, B)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B) => Z), $b: B) } => '{ $op.apply($a, $b) }

inline def applyWithoutBoxing2[A, B, Z](inline a: A, inline opb: ((A, B) => Z, B)): Z =
  ${ applyWithoutBoxingExpr2('a, 'opb) }

def applyWithoutBoxingExpr3[A: Type, B: Type, C: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C) => Z, B, C)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C) => Z), $b: B, $c: C) } => '{ $op.apply($a, $b, $c) }

inline def applyWithoutBoxing3[A, B, C, Z](inline a: A, inline opbc: ((A, B, C) => Z, B, C)): Z =
  ${ applyWithoutBoxingExpr3('a, 'opbc) }

def applyWithoutBoxingExpr4[A: Type, B: Type, C: Type, D: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D) => Z, B, C, D)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D) => Z), $b: B, $c: C, $d: D) } => '{ $op.apply($a, $b, $c, $d) }

inline def applyWithoutBoxing4[A, B, C, D, Z](inline a: A, inline opbcd: ((A, B, C, D) => Z, B, C, D)): Z =
  ${ applyWithoutBoxingExpr4('a, 'opbcd) }

def applyWithoutBoxingExpr5[A: Type, B: Type, C: Type, D: Type, E: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E) => Z, B, C, D, E)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E) => Z), $b: B, $c: C, $d: D, $e: E) } => '{ $op.apply($a, $b, $c, $d, $e) }

inline def applyWithoutBoxing5[A, B, C, D, E, Z](inline a: A, inline opbcde: ((A, B, C, D, E) => Z, B, C, D, E)): Z =
  ${ applyWithoutBoxingExpr5('a, 'opbcde) }

def applyWithoutBoxingExpr6[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F) => Z, B, C, D, E, F)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F) => Z), $b: B, $c: C, $d: D, $e: E, $f: F) } => '{ $op.apply($a, $b, $c, $d, $e, $f) }

inline def applyWithoutBoxing6[A, B, C, D, E, F, Z](inline a: A, inline opbcdef: ((A, B, C, D, E, F) => Z, B, C, D, E, F)): Z =
  ${ applyWithoutBoxingExpr6('a, 'opbcdef) }

def applyWithoutBoxingExpr7[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G) => Z, B, C, D, E, F, G)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g) }

inline def applyWithoutBoxing7[A, B, C, D, E, F, G, Z](inline a: A, inline opbcdefg: ((A, B, C, D, E, F, G) => Z, B, C, D, E, F, G)): Z =
  ${ applyWithoutBoxingExpr7('a, 'opbcdefg) }

def applyWithoutBoxingExpr8[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H) => Z, B, C, D, E, F, G, H)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h) }

inline def applyWithoutBoxing8[A, B, C, D, E, F, G, H, Z](inline a: A, inline opbcdefgh: ((A, B, C, D, E, F, G, H) => Z, B, C, D, E, F, G, H)): Z =
  ${ applyWithoutBoxingExpr8('a, 'opbcdefgh) }

def applyWithoutBoxingExpr9[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I) => Z, B, C, D, E, F, G, H, I)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i) }

inline def applyWithoutBoxing9[A, B, C, D, E, F, G, H, I, Z](inline a: A, inline opbcdefghi: ((A, B, C, D, E, F, G, H, I) => Z, B, C, D, E, F, G, H, I)): Z =
  ${ applyWithoutBoxingExpr9('a, 'opbcdefghi) }

