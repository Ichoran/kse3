// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2023-25 Rex Kerr and Calico Life Sciences LLC.

package kse.basics.basicsMacroImpl

//import scala.language.`3.6-migration` // tests whether opaque types use same-named methods on underlying type or the externally-visible extension

import scala.reflect.ClassTag
import scala.quoted.*


object TreePrettyPrinter {
  inline def report[A](inline expr: A): A =
    ${ reportImpl('{ expr }) }

  def reportImpl[A: Type](expr: Expr[A])(using Quotes): expr.type =
    import quotes.reflect.*
    println(formatTerm(expr.asTerm, 0))
    expr

  inline def prettyPrint[A](inline expr: A): String =
    ${ prettyPrintImpl('{ expr }) }

  def prettyPrintImpl[A: Type](expr: Expr[A])(using Quotes): Expr[String] =
    import quotes.reflect.*
    val formatted = formatTree(expr.asTerm, 0)
    Expr(formatted)
  
  def formatTree(using q: Quotes)(tree: q.reflect.Tree, level: Int): String =
    import q.reflect.*
    
    val indent = "  " * level
    
    tree match
      case t: Term => formatTerm(t, level)
      case t: TypeTree => s"$indent${t.show(using Printer.TreeStructure)}"
      case t => s"$indent${t.show(using Printer.TreeStructure)}"
  
  def formatTerm(using q: Quotes)(term: q.reflect.Term, level: Int): String =
    import q.reflect.*
    
    val indent = "  " * level
    val nextLevel = level + 1
    val nextIndent = "  " * nextLevel
    
    term match
      case Apply(fun, args) =>
        val funStr = formatTerm(fun, nextLevel) // No indent for function part
        val argsStr = args.map(arg => formatTree(arg, nextLevel)).mkString(s",\n")
        s"${indent}Apply(\n$funStr,\n$nextIndent[${if (args.isEmpty) "" else s"\n$argsStr\n$nextIndent"}]\n$indent)"
        
      case Select(qual, name) =>
        val qualStr = formatTerm(qual, nextLevel)
        s"${indent}Select(\n$qualStr,\n$nextIndent$name\n$indent)"
      
      case Literal(const) =>
        s"${indent}Literal(${const.show})"
        
      case Ident(name) =>
        s"${indent}Ident(\"$name\")"
      
      case New(tpt) =>
        val tptStr = formatTree(tpt, nextLevel)
        s"${indent}New(\n$tptStr\n$indent)"
        
      case Typed(expr, tpt) =>
        val exprStr = formatTerm(expr, nextLevel)
        val tptStr = formatTree(tpt, nextLevel)
        s"${indent}Typed(\n$exprStr,\n$tptStr\n$indent)"
        
      case If(cond, thenPart, elsePart) =>
        val condStr = formatTerm(cond, nextLevel)
        val thenStr = formatTerm(thenPart, nextLevel)
        val elseStr = formatTerm(elsePart, nextLevel)
        s"${indent}If(\n$condStr,\n$thenStr,\n$elseStr\n$indent)"
        
      case Block(stats, expr) =>
        val statsStr = stats.map(stat => formatTree(stat, nextLevel)).mkString(",\n")
        val exprStr = formatTerm(expr, nextLevel)
        val statsFormatted = if (stats.isEmpty) s"$nextIndent[]" else s"$nextIndent[\n$statsStr\n$nextIndent]"
        s"${indent}Block(\n$statsFormatted,\n$exprStr\n$indent)"
        
      case Inlined(call, bindings, expansion) =>
        val callStr = if (call.isEmpty) s"${nextIndent}None" else s"${nextIndent}Some(\n${formatTree(call.get, nextLevel + 1)}\n$nextIndent)"
        val bindingsStr = bindings.map(b => formatTree(b, nextLevel + 1)).mkString(",\n")
        val bindingsFormatted = if (bindings.isEmpty) s"$nextIndent[]" else s"$nextIndent[\n$bindingsStr\n$nextIndent]"
        val expansionStr = formatTree(expansion, nextLevel)
        
        s"${indent}Inlined(\n$callStr,\n$bindingsFormatted,\n$expansionStr\n$indent)"
      
      case other =>
        // For other term types, fallback to the standard printer but add indentation
        s"$indent${other.show(using Printer.TreeStructure)}"
}


def extractNamedTuple1LiteralExpr[L: Type, A: Type](a: Expr[NamedTuple.NamedTuple[Tuple1[L], Tuple1[A]]])(using qt: Quotes): Expr[A] =
  import qt.reflect.*
  deinliner(a).asTerm match
    case Apply(TypeApply(Apply(TypeApply(Select(Ident("NamedTuple"), "build"), _), _), _), ea :: Nil) => ea match
      case Apply(TypeApply(Select(Ident("Tuple1"), "apply"), _), lit :: Nil) => lit.asExprOf[A]
      case _ => qt.reflect.report.errorAndAbort("Only named Tuple1 literals are supported")
    case _ => qt.reflect.report.errorAndAbort("Only named Tuple1 literals are supported")

inline def extractNamedTuple1Literal[L <: String, A](inline nt1: NamedTuple.NamedTuple[Tuple1[L], Tuple1[A]]): A =
  ${ extractNamedTuple1LiteralExpr('nt1) }


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

inline def untypedRangePackedInLong(inline range: Any): Long =
  ${ rangePackedInLongExpr('range) }

def applyWithoutBoxingExpr2[A: Type, B: Type, Z: Type](a: Expr[A], e: Expr[((A, B) => Z, B)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B) => Z), $b: B) } => '{ $op.apply($a, $b) }

inline def applyWithoutBoxing2[A, B, Z](inline a: A, inline op2: ((A, B) => Z, B)): Z =
  ${ applyWithoutBoxingExpr2('a, 'op2) }

def applyWithoutBoxingExpr3[A: Type, B: Type, C: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C) => Z, B, C)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C) => Z), $b: B, $c: C) } => '{ $op.apply($a, $b, $c) }

inline def applyWithoutBoxing3[A, B, C, Z](inline a: A, inline op3: ((A, B, C) => Z, B, C)): Z = 
  ${ applyWithoutBoxingExpr3('a, 'op3) }

def applyWithoutBoxingExpr4[A: Type, B: Type, C: Type, D: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D) => Z, B, C, D)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D) => Z), $b: B, $c: C, $d: D) } => '{ $op.apply($a, $b, $c, $d) }

inline def applyWithoutBoxing4[A, B, C, D, Z](inline a: A, inline op4: ((A, B, C, D) => Z, B, C, D)): Z = 
  ${ applyWithoutBoxingExpr4('a, 'op4) }

def applyWithoutBoxingExpr5[A: Type, B: Type, C: Type, D: Type, E: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E) => Z, B, C, D, E)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E) => Z), $b: B, $c: C, $d: D, $e: E) } => '{ $op.apply($a, $b, $c, $d, $e) }

inline def applyWithoutBoxing5[A, B, C, D, E, Z](inline a: A, inline op5: ((A, B, C, D, E) => Z, B, C, D, E)): Z = 
  ${ applyWithoutBoxingExpr5('a, 'op5) }

def applyWithoutBoxingExpr6[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F) => Z, B, C, D, E, F)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F) => Z), $b: B, $c: C, $d: D, $e: E, $f: F) } => '{ $op.apply($a, $b, $c, $d, $e, $f) }

inline def applyWithoutBoxing6[A, B, C, D, E, F, Z](inline a: A, inline op6: ((A, B, C, D, E, F) => Z, B, C, D, E, F)): Z = 
  ${ applyWithoutBoxingExpr6('a, 'op6) }

def applyWithoutBoxingExpr7[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G) => Z, B, C, D, E, F, G)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g) }

inline def applyWithoutBoxing7[A, B, C, D, E, F, G, Z](inline a: A, inline op7: ((A, B, C, D, E, F, G) => Z, B, C, D, E, F, G)): Z = 
  ${ applyWithoutBoxingExpr7('a, 'op7) }

def applyWithoutBoxingExpr8[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H) => Z, B, C, D, E, F, G, H)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h) }

inline def applyWithoutBoxing8[A, B, C, D, E, F, G, H, Z](inline a: A, inline op8: ((A, B, C, D, E, F, G, H) => Z, B, C, D, E, F, G, H)): Z = 
  ${ applyWithoutBoxingExpr8('a, 'op8) }

def applyWithoutBoxingExpr9[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I) => Z, B, C, D, E, F, G, H, I)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i) }

inline def applyWithoutBoxing9[A, B, C, D, E, F, G, H, I, Z](inline a: A, inline op9: ((A, B, C, D, E, F, G, H, I) => Z, B, C, D, E, F, G, H, I)): Z = 
  ${ applyWithoutBoxingExpr9('a, 'op9) }

def applyWithoutBoxingExpr10[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J) => Z, B, C, D, E, F, G, H, I, J)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j) }

inline def applyWithoutBoxing10[A, B, C, D, E, F, G, H, I, J, Z](inline a: A, inline op10: ((A, B, C, D, E, F, G, H, I, J) => Z, B, C, D, E, F, G, H, I, J)): Z = 
  ${ applyWithoutBoxingExpr10('a, 'op10) }

def applyWithoutBoxingExpr11[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K) => Z, B, C, D, E, F, G, H, I, J, K)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k) }

inline def applyWithoutBoxing11[A, B, C, D, E, F, G, H, I, J, K, Z](inline a: A, inline op11: ((A, B, C, D, E, F, G, H, I, J, K) => Z, B, C, D, E, F, G, H, I, J, K)): Z = 
  ${ applyWithoutBoxingExpr11('a, 'op11) }

def applyWithoutBoxingExpr12[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L) => Z, B, C, D, E, F, G, H, I, J, K, L)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l) }

inline def applyWithoutBoxing12[A, B, C, D, E, F, G, H, I, J, K, L, Z](inline a: A, inline op12: ((A, B, C, D, E, F, G, H, I, J, K, L) => Z, B, C, D, E, F, G, H, I, J, K, L)): Z = 
  ${ applyWithoutBoxingExpr12('a, 'op12) }

def applyWithoutBoxingExpr13[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L, M) => Z, B, C, D, E, F, G, H, I, J, K, L, M)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L, M) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m) }

inline def applyWithoutBoxing13[A, B, C, D, E, F, G, H, I, J, K, L, M, Z](inline a: A, inline op13: ((A, B, C, D, E, F, G, H, I, J, K, L, M) => Z, B, C, D, E, F, G, H, I, J, K, L, M)): Z = 
  ${ applyWithoutBoxingExpr13('a, 'op13) }

def applyWithoutBoxingExpr14[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n) }

inline def applyWithoutBoxing14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z](inline a: A, inline op14: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N)): Z = 
  ${ applyWithoutBoxingExpr14('a, 'op14) }

def applyWithoutBoxingExpr15[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o) }

inline def applyWithoutBoxing15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z](inline a: A, inline op15: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O)): Z = 
  ${ applyWithoutBoxingExpr15('a, 'op15) }

def applyWithoutBoxingExpr16[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p) }

inline def applyWithoutBoxing16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z](inline a: A, inline op16: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)): Z = 
  ${ applyWithoutBoxingExpr16('a, 'op16) }

def applyWithoutBoxingExpr17[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q) }

inline def applyWithoutBoxing17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z](inline a: A, inline op17: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)): Z = 
  ${ applyWithoutBoxingExpr17('a, 'op17) }

def applyWithoutBoxingExpr18[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q, $r: R) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r) }

inline def applyWithoutBoxing18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z](inline a: A, inline op18: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)): Z = 
  ${ applyWithoutBoxingExpr18('a, 'op18) }

def applyWithoutBoxingExpr19[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q, $r: R, $s: S) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s) }

inline def applyWithoutBoxing19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z](inline a: A, inline op19: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)): Z = 
  ${ applyWithoutBoxingExpr19('a, 'op19) }

def applyWithoutBoxingExpr20[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q, $r: R, $s: S, $t: T) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t) }

inline def applyWithoutBoxing20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z](inline a: A, inline op20: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)): Z = 
  ${ applyWithoutBoxingExpr20('a, 'op20) }

def applyWithoutBoxingExpr21[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q, $r: R, $s: S, $t: T, $u: U) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u) }

inline def applyWithoutBoxing21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z](inline a: A, inline op21: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)): Z = 
  ${ applyWithoutBoxingExpr21('a, 'op21) }

def applyWithoutBoxingExpr22[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type, Z: Type](a: Expr[A], e: Expr[((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)])(using qt: Quotes): Expr[Z] =
  e match
    case '{ ($op: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z), $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q, $r: R, $s: S, $t: T, $u: U, $v: V) } => '{ $op.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u, $v) }

inline def applyWithoutBoxing22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z](inline a: A, inline op22: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)): Z = 
  ${ applyWithoutBoxingExpr22('a, 'op22) }


def fnWithoutBoxingExpr2[A: Type, B: Type, Z: Type](tup2: Expr[(A, B)], op2: Expr[(A, B) => Z])(using qt: Quotes): Expr[Z] =
  tup2 match
    case '{ ($a: A, $b: B) } => '{ $op2.apply($a, $b) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing2[A, B, Z](inline tup2: (A, B), inline op2: (A, B) => Z): Z =
  ${ fnWithoutBoxingExpr2('tup2, 'op2) }

def fnWithoutBoxingExpr3[A: Type, B: Type, C: Type, Z: Type](tup3: Expr[(A, B, C)], op3: Expr[(A, B, C) => Z])(using qt: Quotes): Expr[Z] =
  tup3 match
    case '{ ($a: A, $b: B, $c: C) } => '{ $op3.apply($a, $b, $c) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing3[A, B, C, Z](inline tup3: (A, B, C), inline op3: (A, B, C) => Z): Z =
  ${ fnWithoutBoxingExpr3('tup3, 'op3) }

def fnWithoutBoxingExpr4[A: Type, B: Type, C: Type, D: Type, Z: Type](tup4: Expr[(A, B, C, D)], op4: Expr[(A, B, C, D) => Z])(using qt: Quotes): Expr[Z] =
  tup4 match
    case '{ ($a: A, $b: B, $c: C, $d: D) } => '{ $op4.apply($a, $b, $c, $d) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing4[A, B, C, D, Z](inline tup4: (A, B, C, D), inline op4: (A, B, C, D) => Z): Z =
  ${ fnWithoutBoxingExpr4('tup4, 'op4) }

def fnWithoutBoxingExpr5[A: Type, B: Type, C: Type, D: Type, E: Type, Z: Type](tup5: Expr[(A, B, C, D, E)], op5: Expr[(A, B, C, D, E) => Z])(using qt: Quotes): Expr[Z] =
  tup5 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E) } => '{ $op5.apply($a, $b, $c, $d, $e) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing5[A, B, C, D, E, Z](inline tup5: (A, B, C, D, E), inline op5: (A, B, C, D, E) => Z): Z =
  ${ fnWithoutBoxingExpr5('tup5, 'op5) }

def fnWithoutBoxingExpr6[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, Z: Type](tup6: Expr[(A, B, C, D, E, F)], op6: Expr[(A, B, C, D, E, F) => Z])(using qt: Quotes): Expr[Z] =
  tup6 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F) } => '{ $op6.apply($a, $b, $c, $d, $e, $f) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing6[A, B, C, D, E, F, Z](inline tup6: (A, B, C, D, E, F), inline op6: (A, B, C, D, E, F) => Z): Z =
  ${ fnWithoutBoxingExpr6('tup6, 'op6) }

def fnWithoutBoxingExpr7[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, Z: Type](tup7: Expr[(A, B, C, D, E, F, G)], op7: Expr[(A, B, C, D, E, F, G) => Z])(using qt: Quotes): Expr[Z] =
  tup7 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G) } => '{ $op7.apply($a, $b, $c, $d, $e, $f, $g) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing7[A, B, C, D, E, F, G, Z](inline tup7: (A, B, C, D, E, F, G), inline op7: (A, B, C, D, E, F, G) => Z): Z =
  ${ fnWithoutBoxingExpr7('tup7, 'op7) }

def fnWithoutBoxingExpr8[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, Z: Type](tup8: Expr[(A, B, C, D, E, F, G, H)], op8: Expr[(A, B, C, D, E, F, G, H) => Z])(using qt: Quotes): Expr[Z] =
  tup8 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H) } => '{ $op8.apply($a, $b, $c, $d, $e, $f, $g, $h) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing8[A, B, C, D, E, F, G, H, Z](inline tup8: (A, B, C, D, E, F, G, H), inline op8: (A, B, C, D, E, F, G, H) => Z): Z =
  ${ fnWithoutBoxingExpr8('tup8, 'op8) }

def fnWithoutBoxingExpr9[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, Z: Type](tup9: Expr[(A, B, C, D, E, F, G, H, I)], op9: Expr[(A, B, C, D, E, F, G, H, I) => Z])(using qt: Quotes): Expr[Z] =
  tup9 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I) } => '{ $op9.apply($a, $b, $c, $d, $e, $f, $g, $h, $i) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing9[A, B, C, D, E, F, G, H, I, Z](inline tup9: (A, B, C, D, E, F, G, H, I), inline op9: (A, B, C, D, E, F, G, H, I) => Z): Z =
  ${ fnWithoutBoxingExpr9('tup9, 'op9) }

def fnWithoutBoxingExpr10[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, Z: Type](tup10: Expr[(A, B, C, D, E, F, G, H, I, J)], op10: Expr[(A, B, C, D, E, F, G, H, I, J) => Z])(using qt: Quotes): Expr[Z] =
  tup10 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J) } => '{ $op10.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing10[A, B, C, D, E, F, G, H, I, J, Z](inline tup10: (A, B, C, D, E, F, G, H, I, J), inline op10: (A, B, C, D, E, F, G, H, I, J) => Z): Z =
  ${ fnWithoutBoxingExpr10('tup10, 'op10) }

def fnWithoutBoxingExpr11[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, Z: Type](tup11: Expr[(A, B, C, D, E, F, G, H, I, J, K)], op11: Expr[(A, B, C, D, E, F, G, H, I, J, K) => Z])(using qt: Quotes): Expr[Z] =
  tup11 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K) } => '{ $op11.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing11[A, B, C, D, E, F, G, H, I, J, K, Z](inline tup11: (A, B, C, D, E, F, G, H, I, J, K), inline op11: (A, B, C, D, E, F, G, H, I, J, K) => Z): Z =
  ${ fnWithoutBoxingExpr11('tup11, 'op11) }

def fnWithoutBoxingExpr12[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, Z: Type](tup12: Expr[(A, B, C, D, E, F, G, H, I, J, K, L)], op12: Expr[(A, B, C, D, E, F, G, H, I, J, K, L) => Z])(using qt: Quotes): Expr[Z] =
  tup12 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L) } => '{ $op12.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing12[A, B, C, D, E, F, G, H, I, J, K, L, Z](inline tup12: (A, B, C, D, E, F, G, H, I, J, K, L), inline op12: (A, B, C, D, E, F, G, H, I, J, K, L) => Z): Z =
  ${ fnWithoutBoxingExpr12('tup12, 'op12) }

def fnWithoutBoxingExpr13[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, Z: Type](tup13: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M)], op13: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M) => Z])(using qt: Quotes): Expr[Z] =
  tup13 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M) } => '{ $op13.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing13[A, B, C, D, E, F, G, H, I, J, K, L, M, Z](inline tup13: (A, B, C, D, E, F, G, H, I, J, K, L, M), inline op13: (A, B, C, D, E, F, G, H, I, J, K, L, M) => Z): Z =
  ${ fnWithoutBoxingExpr13('tup13, 'op13) }

def fnWithoutBoxingExpr14[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, Z: Type](tup14: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)], op14: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z])(using qt: Quotes): Expr[Z] =
  tup14 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N) } => '{ $op14.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Z](inline tup14: (A, B, C, D, E, F, G, H, I, J, K, L, M, N), inline op14: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Z): Z =
  ${ fnWithoutBoxingExpr14('tup14, 'op14) }

def fnWithoutBoxingExpr15[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, Z: Type](tup15: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)], op15: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z])(using qt: Quotes): Expr[Z] =
  tup15 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O) } => '{ $op15.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Z](inline tup15: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O), inline op15: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Z): Z =
  ${ fnWithoutBoxingExpr15('tup15, 'op15) }

def fnWithoutBoxingExpr16[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Z: Type](tup16: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)], op16: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z])(using qt: Quotes): Expr[Z] =
  tup16 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P) } => '{ $op16.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Z](inline tup16: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P), inline op16: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Z): Z =
  ${ fnWithoutBoxingExpr16('tup16, 'op16) }

def fnWithoutBoxingExpr17[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, Z: Type](tup17: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)], op17: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z])(using qt: Quotes): Expr[Z] =
  tup17 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q) } => '{ $op17.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Z](inline tup17: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q), inline op17: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Z): Z =
  ${ fnWithoutBoxingExpr17('tup17, 'op17) }

def fnWithoutBoxingExpr18[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, Z: Type](tup18: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)], op18: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z])(using qt: Quotes): Expr[Z] =
  tup18 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q, $r: R) } => '{ $op18.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Z](inline tup18: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R), inline op18: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Z): Z =
  ${ fnWithoutBoxingExpr18('tup18, 'op18) }

def fnWithoutBoxingExpr19[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, Z: Type](tup19: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)], op19: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z])(using qt: Quotes): Expr[Z] =
  tup19 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q, $r: R, $s: S) } => '{ $op19.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Z](inline tup19: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S), inline op19: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Z): Z =
  ${ fnWithoutBoxingExpr19('tup19, 'op19) }

def fnWithoutBoxingExpr20[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, Z: Type](tup20: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)], op20: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z])(using qt: Quotes): Expr[Z] =
  tup20 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q, $r: R, $s: S, $t: T) } => '{ $op20.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Z](inline tup20: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T), inline op20: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Z): Z =
  ${ fnWithoutBoxingExpr20('tup20, 'op20) }

def fnWithoutBoxingExpr21[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, Z: Type](tup21: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)], op21: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z])(using qt: Quotes): Expr[Z] =
  tup21 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q, $r: R, $s: S, $t: T, $u: U) } => '{ $op21.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Z](inline tup21: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U), inline op21: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Z): Z =
  ${ fnWithoutBoxingExpr21('tup21, 'op21) }

def fnWithoutBoxingExpr22[A: Type, B: Type, C: Type, D: Type, E: Type, F: Type, G: Type, H: Type, I: Type, J: Type, K: Type, L: Type, M: Type, N: Type, O: Type, P: Type, Q: Type, R: Type, S: Type, T: Type, U: Type, V: Type, Z: Type](tup22: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)], op22: Expr[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z])(using qt: Quotes): Expr[Z] =
  tup22 match
    case '{ ($a: A, $b: B, $c: C, $d: D, $e: E, $f: F, $g: G, $h: H, $i: I, $j: J, $k: K, $l: L, $m: M, $n: N, $o: O, $p: P, $q: Q, $r: R, $s: S, $t: T, $u: U, $v: V) } => '{ $op22.apply($a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u, $v) }
    case _ => qt.reflect.report.errorAndAbort("fn only available for tuple literals; use .merge on tuple instances")

inline def fnWithoutBoxing22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Z](inline tup22: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V), inline op22: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Z): Z =
  ${ fnWithoutBoxingExpr22('tup22, 'op22) }
/*

################
## GENERATORS ##
################

def mkNoBoxAp(n: Int) =
  assert(n > 1 && n < 26)
  val args = "ABCDEFGHIJKLMNOPQRSTUVWXY".take(n).map(_.toString)
  val argtp = args.map(_ + ": Type").mkString(", ")
  val arga = args.mkString(", ")
  val argb = args.drop(1).mkString(", ")
  val termb = args.drop(1).map(t => s"$$${t.toLowerCase}: $t").mkString(", ")
  val dolla = args.map("$" + _.toLowerCase).mkString(", ")
  println(s"def applyWithoutBoxingExpr$n[$argtp, Z: Type](a: Expr[A], e: Expr[(($arga) => Z, $argb)])(using qt: Quotes): Expr[Z] =")
  println(s"  e match")
  println(s"    case '{ ($$op: (($arga) => Z), $termb) } => '{ $$op.apply($dolla) }")
  println()
  println(s"inline def applyWithoutBoxing$n[$arga, Z](inline a: A, inline op$n: (($arga) => Z, $argb)): Z = ")
  println(s"  $${ applyWithoutBoxingExpr$n('a, 'op$n) }")
  println()

def mkFnTup(n: Int) =
  assert(n > 1 && n < 26)
  val args = "ABCDEFGHIJKLMNOPQRSTUVWXY".take(n).map(_.toString)
  val argtp = args.map(_ + ": Type").mkString(", ")
  val arga = args.mkString(", ")
  val terma = args.map(t => s"$$${t.toLowerCase}: $t").mkString(", ")
  val dolla = args.map("$" + _.toLowerCase).mkString(", ")
  println(s"def fnWithoutBoxingExpr$n[$argtp, Z: Type](tup$n: Expr[($arga)], op$n: Expr[($arga) => Z])(using qt: Quotes): Expr[Z] =")
  println(s"  tup$n match")
  println(s"    case '{ ($terma) } => '{ $$op$n.apply($dolla) }")
  println(s"    case _ => qt.reflect.report.errorAndAbort(\"fn only available for tuple literals; use .merge on tuple instances\")")
  println()
  println(s"inline def fnWithoutBoxing$n[$arga, Z](inline tup$n: ($arga), inline op$n: ($arga) => Z): Z =")
  println(s"  $${ fnWithoutBoxingExpr$n('tup$n, 'op$n) }")
  println()

for n <- 3 to 22 do
  mkNoBoxAp(n)
println()
for n <- 3 to 22 do
  mkFnTup(n)

*/
