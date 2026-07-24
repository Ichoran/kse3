// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2026 Rex Kerr and UCSF (Kato Lab)

package kse.testutilities

import scala.quoted.*


// Tiny call-site-position macros so the testing module needs no external dependencies.
// These deliberately duplicate a sliver of kse.basics (SourceLine and its macros, which are
// more capable); the testing module must stand alone, so it carries its own copies.

/** The line number at the summoning call site. */
final case class Line(value: Int)
object Line {
  /** Captures the call site's line number wherever a `(using Line)` is needed. */
  inline given Line = ${ TestUtilityMacros.lineImpl }
}

/** The file name (without directories) at the summoning call site. */
final case class FileName(value: String)
object FileName {
  /** Captures the call site's file name wherever a `(using FileName)` is needed. */
  inline given FileName = ${ TestUtilityMacros.fileNameImpl }
}

object TestUtilityMacros {
  def lineImpl(using Quotes): Expr[Line] =
    import quotes.reflect.*
    '{ Line(${ Expr(Position.ofMacroExpansion.startLine + 1) }) }   // reflect lines are 0-based

  def fileNameImpl(using Quotes): Expr[FileName] =
    import quotes.reflect.*
    val path = Position.ofMacroExpansion.sourceFile.path
    val a = path.lastIndexOf('/')
    val b = path.lastIndexOf('\\')
    val i = if a > b then a else b
    '{ FileName(${ Expr(if i >= 0 then path.substring(i + 1) else path) }) }
}
