

package kse.flow

import scala.quoted._

/** An exception indicating that a flow element was not properly transformed in its transformation macro.
  */
final class UntransformedFlowException[A](val value: A)
extends Exception() {
  override def toString = s"${super.toString}($value)"
}


// Important references:
//   https://docs.scala-lang.org/scala3/guides/macros/macros.html
//   https://github.com/lampepfl/dotty/blob/3.0.2/library/src/scala/quoted/Quotes.scala#L25
//   https://github.com/lampepfl/dotty/blob/3.0.2/library/src/scala/quoted/Quotes.scala#L4370
//   https://softwaremill.com/scala-3-macros-tips-and-tricks/#shape-of-a-macro

/** Picks out throws of UntransformedFlowException and converts them into return statements.
  * The inline method calling this macro is required to have one argument which is immediately used as a match statement.
  * Only instances of the exception inlined by a `?` method (generally an extension method) are converted.
  * A throw must be the last statement in a match (it's okay to have a block, but the throw must be in the last expression).
  */
object EarlyReturnMacro {
  def transform[T: Type](x: Expr[T])(using Quotes): Expr[T] = (new ReturnTransformer(x)).transform()

  class ReturnTransformer[T: Type](x: Expr[T])(using Quotes) {
    import quotes.reflect.*

    def transform(): Expr[T] =
      println(x.asTerm.symbol.maybeOwner)
      val term = Impl.transformTerm(x.asTerm)(x.asTerm.symbol)
      Check(term)(term.symbol)
      term.asExprOf[T]

    private object Impl extends TreeMap {
      val tpr = TypeRepr.of[T]
      var defdefDepth = 0
      var lambdaEnabled = false
      private def reportPosition(p: Position): String = 
        val shown = p.sourceCode match {
          case Some(code) =>
            if (code.indexOf('\n') < 0) {
              if (code.length < 78) s"\nSource:\n  $code\n"
              else s"\nSource:\n  ${code.take(30)} ... ${code.takeRight(42)}\n"
            }
            else {
              val codeLines = code.linesIterator.toArray
              if (codeLines.length <= 5) codeLines.mkString("\nSource:\n", "\n", "\n")
              else codeLines.take(2).mkString("\nSource:\n", "\n", "\n") + codeLines.takeRight(3).mkString("  . . .\n", "\n", "\n")
            }
          case _ => ""
        }
        s"\nAt ${p.sourceFile}, line ${p.endLine+1}, column ${p.endColumn}$shown\n"
      private def returnThrownCaseDefs(casedefs: List[CaseDef], position: Position): List[CaseDef] = casedefs.map {
        case c @ CaseDef(
          v, p,
          b @ Block(xs,
            Apply(
              Ident("throw"),
              Apply(
                ta @ TypeApply(Select(New(TypeIdent("UntransformedFlowException")), _), tt :: Nil),
                param :: Nil
              ) :: Nil
            )
          )
        ) =>
          if (!(tt.tpe <:< tpr)) {
            report.throwError(s"Type mismatch in .? return.\n  Required: ${tpr.show}\n  Found:    ${tt.tpe.show}${reportPosition(position)}")
          }
          var s = param.symbol
          var n = 0
          while (s.exists && (n < defdefDepth || !s.isDefDef)) {
            if (s.isDefDef) n += 1
            s = s.maybeOwner
          }
          if (!s.isDefDef) {
            report.throwError(s"No method found enclosing .? return.${reportPosition(position)}")
          }
          CaseDef.copy(c)(v, p, Block.copy(b)(xs, Return(param, s)))
        case c @ CaseDef(
          v, p,
          Apply(
            Ident("throw"),
            Apply(
              ta @ TypeApply(Select(New(TypeIdent("UntransformedFlowException")), _), tt :: Nil),
              param :: Nil
            ) :: Nil
          )
        ) => 
          if (!(tt.tpe <:< tpr)) {
            report.throwError(s"Type mismatch in .? return.\n  Required: ${tpr.show}\n  Found:    ${tt.tpe.show}${reportPosition(position)}")
          }
          var s = param.symbol
          var n = 0
          while (s.exists && (n < defdefDepth || !s.isDefDef)) {
            if (s.isDefDef) n += 1
            s = s.maybeOwner
          }
          if (!s.isDefDef) {
            report.throwError(s"No method found enclosing .? return.${reportPosition(position)}")
          }
          CaseDef.copy(c)(v, p, Block(Nil, Return(param, s)))
        case x => x
      }
      override def transformStatement(statement: Statement)(owner: Symbol) = statement match
        case _: ValDef => super.transformStatement(statement)(owner)
        case _: Term => super.transformStatement(statement)(owner)
        case _: DefDef if lambdaEnabled => 
          defdefDepth += 1
          lambdaEnabled = false
          val answer = super.transformStatement(statement)(owner)
          defdefDepth -= 1
          answer
        case x => x
      override def transformTerm(term: Term)(owner: Symbol) = term match
        case z @ Inlined(a @ Some(Apply(Ident(i), _)), b, c) if i == "?" => c match
          case Typed(m @ Match(x, cds), y) =>
            val m2 = Match.copy(m)(transformTerm(x)(owner), returnThrownCaseDefs(cds, z.pos))
            Inlined.copy(term)(a, transformSubTrees(b)(owner), Typed.copy(c)(m2, y))
          case _ => super.transformTerm(term)(owner)
        case z @ Inlined(a @ Some(Apply(TypeApply(Ident(i), _), _)), b, c) if i == "?" => c match
          case Typed(m @ Match(x, cds), y) => 
            Inlined.copy(term)(
              a,
              transformSubTrees(b)(owner),
              Typed.copy(c)(Match.copy(m)(transformTerm(x)(owner), returnThrownCaseDefs(cds, z.pos)), y)
            )
          case _ => super.transformTerm(term)(owner)
        case Lambda(xs, y) => lambdaEnabled = true; super.transformTerm(term)(owner)
        /*
        case Inlined(a, b, c) => println(s"Inlined $a\nis $c\n"); super.transformTerm(term)(owner)  // Helpful case for debugging printlns
        case Block(xs, y) => 
          println("Block:")
          xs.zipWithIndex.foreach{ case (x, i) => println(s"  #$i = $x") }
          println("------")
          println(s"$y\n")
          super.transformTerm(term)(owner)  // Helpful case for debugging printlns
        */
        case _ => super.transformTerm(term)(owner)
    }

    private object Check extends TreeTraverser {
      def apply(tree: Tree)(owner: Symbol): Unit = traverseTree(tree)(owner)
      override def foldOverTree(x: Unit, tree: Tree)(owner: Symbol) = tree match
        case n @ New(TypeIdent("UntransformedFlowException")) => report.throwError(s"UntransformedFlowException escaped from ${n.pos}")
        case _ => super.foldOverTree(x, tree)(owner)
    }
  }
}

